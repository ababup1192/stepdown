// TurnTimeline — 1 手の出来事の列から、見せる時刻表（Timeline）と、そこで鳴らす音を組む（純粋）。
// タグは (出来事の番号, 見せ方の種類)。描く側（Motion）はタグの進みを読み、鳴らす側は同じ表の Cue を読む。
//
// ── 並べ方 ──
// 主人公の手（移動・殴る・拾う・矢・爆弾）は起きた順に後ろへ足す。当たった物の光は、原因の動きの途中に重ねる。
// 敵の移動は最初の 1 つが来た所に全部まとめて同時に置き、敵の攻撃（殴る・矢）だけを起きた順に並べる。
mod TurnTimeline {
    use fge::{Clock, Timeline}
    use fge::Timeline.Duration
    use Run.Event

    /// 時刻表の行の見せ方。
    pub enum Beat with Eq, Order, ToString {
        case HeroMove      // 主人公がマスの間を弧で跳ねる
        case HeroBump      // 進めない向きへ小さく寄って戻る
        case HeroStrike    // 主人公が相手へ踏み込んで戻る
        case Pick          // 拾ったアイコンが主人公へ吸い込まれる
        case Upgrade       // 上位を拾って持ち物の枠が光る
        case Arrow         // 主人公の矢の板が飛ぶ
        case Bomb          // 爆弾の光と煙が広がる
        case Thunder       // 画面が一瞬白くなり、敵ごとに稲妻が落ちる
        case Smoke         // 煙玉が主人公から敵の方へ弧を描いて飛ぶ
        case Puff          // 着いた所で灰色の煙が広がって弾ける
        case Flash         // 当たった物が白く光って震える
        case Die           // 敵が縮んで消える
        case EnemyMove     // 敵がマスの間を跳ねる・押される
        case EnemyStrike   // 敵が主人公へ踏み込んで戻る
        case EnemyArrow    // 弓兵の矢の板が飛ぶ
        case Windup        // 敵が身をかがめて技を溜める
        case Trap          // 罠の床が光り、毒矢が主人公へ跳ね上がる
        case Alarm         // 起きかけた敵の頭に「!」が跳ねて出る
        case Potion        // 頭の上で薬の瓶が傾いて空になる
        case Heal          // 回復の輪と緑の数
        case Eat           // 頭の上で食べ物が揺れて噛まれ、骨・かじった形になる（1 回目の「むしゃ」）
        case Bite2         // 2 回目の「むしゃ」
        case Bite3         // 3 回目の「むしゃ」
        case Full          // 食べ終えて満腹度の数が出る
        case Descend       // 階段を降りる
        case Fall          // 主人公が倒れる
    }

    /// 時刻表のタグ: (出来事の列の何番目か, 見せ方)。
    pub type alias Tag = (Int32, Beat)

    /// 1 手で鳴らす音。
    pub enum Sound with Eq, Order, ToString {
        case Step
        case Bump
        case Strike
        case Hit
        case Guard
        case Arrow
        case Bomb
        case Pick
        case Upgrade
        case Defeat
        case Potion
        case Munch
        case Puff
        case Clink
        case Stairs
        case Fall
        case Windup
        case Charge
    }

    /// 音の論理名（project.json の sounds の name）。
    def soundName(sound: Sound): String = match sound {
        case Sound.Step    => "sd_step"
        case Sound.Bump    => "sd_bump"
        case Sound.Strike  => "sd_strike"
        case Sound.Hit     => "sd_hit"
        case Sound.Guard   => "sd_guard"
        case Sound.Arrow   => "sd_arrow"
        case Sound.Bomb    => "sd_bomb"
        case Sound.Pick    => "sd_pick"
        case Sound.Upgrade => "sd_upgrade"
        case Sound.Defeat  => "sd_defeat"
        case Sound.Potion  => "sd_potion"
        case Sound.Munch   => "sd_munch"
        case Sound.Puff    => "sd_puff"
        case Sound.Clink   => "sd_clink"
        case Sound.Stairs  => "sd_stairs"
        case Sound.Fall    => "sd_fall"
        case Sound.Windup  => "sd_windup"
        case Sound.Charge  => "sd_charge"
    }

    /// 見せる秒（look の Doc から。dash = ダッシュの 1 歩の時は移動を短くする）。
    pub type alias Pace = { step = Float64, strike = Float64, arrow = Float64, potion = Float64, eat = Float64, smoke = Float64, pebble = Float64 }

    pub def paceOf(lk: LookDoc.Rec, dash: Bool): Pace =
        { step = if (dash) lk#dashStepSeconds else lk#stepSeconds, strike = lk#strikeSeconds, arrow = lk#arrowSeconds,
          potion = lk#potionSeconds, eat = lk#eatSeconds, smoke = lk#smokeSeconds, pebble = lk#pebbleSeconds }

    /// 並べている途中: 書いた行（逆順）・敵の移動をもう置いたか・爆弾の行・最後に放ったのが小石か・最後に走った突進の行。
    type alias Draft = { rows = List[Timeline.Step[Tag]], movesPlaced = Bool, bomb = Option[Tag], pebble = Bool, charge = Option[Tag] }

    def secs(s: Float64): Duration = Duration.Seconds(s)

    def cue(sound: Sound, row: Timeline.Step[Tag]): Timeline.Step[Tag] = Timeline.withSound(soundName(sound), row)

    /// 1 手の時刻表。組めない並び（同じタグ）はこの関数の書き誤りなので止める。
    pub def build(pace: Pace, events: List[Event]): Timeline.Timeline[Tag] =
        let indexed = List.zipWithIndex(events);
        let moves = indexed |> List.filter(ie -> isEnemyMove(snd(ie))) |> List.map(fst);
        let start = { rows = Nil, movesPlaced = false, bomb = None, pebble = false, charge = None };
        let draft = indexed |> List.foldLeft((d, ie) -> rowsOf(pace, moves, d, ie), start);
        Timeline.buildOrPanic("turn", Timeline.noTempo(), List.reverse(draft#rows))

    def add(rows: List[Timeline.Step[Tag]], d: Draft): Draft = { rows = List.reverse(rows) ::: d#rows | d }

    /// 当たった物の光（原因のクリップの途中から重ねる）。
    def flashOn(pace: Pace, anchor: Tag, delay: Float64, i: Int32, sound: Sound): Timeline.Step[Tag] =
        Timeline.atStartOf(anchor, (i, Beat.Flash), secs(pace#strike)) |> Timeline.withDelay(secs(delay)) |> cue(sound)

    def rowsOf(pace: Pace, moves: List[Int32], d: Draft, ie: (Int32, Event)): Draft =
        let (i, event) = ie;
        let after = (beat: Beat, length: Float64) -> Timeline.append((i, beat), secs(length));
        match event {
            case Event.HeroMoved(_, _)     => add(cue(Sound.Step, after(Beat.HeroMove, pace#step)) :: Nil, d)
            case Event.HeroBlocked(_)      => add(cue(Sound.Bump, after(Beat.HeroBump, pace#step * 0.6)) :: Nil, d)
            case Event.Descended           => add(cue(Sound.Stairs, after(Beat.Descend, pace#strike * 3.0)) :: Nil, d)
            case Event.Picked(_, Item.Pick.Upgraded) =>
                add(cue(Sound.Upgrade, after(Beat.Pick, pace#step)) :: Timeline.join((i, Beat.Upgrade), secs(pace#strike * 4.0)) :: Nil, d)
            case Event.Picked(_, _)        => add(cue(Sound.Pick, after(Beat.Pick, pace#step)) :: Nil, d)
            case Event.HeroStruck(_, _)    =>
                add(cue(Sound.Strike, after(Beat.HeroStrike, pace#strike)) :: flashOn(pace, (i, Beat.HeroStrike), pace#strike * 0.45, i, Sound.Hit) :: Nil, d)
            case Event.ArrowShot(_, kind, _) if Item.isStone(Item.familyOf(kind)) => { pebble = true | add(after(Beat.Arrow, pace#pebble) :: Nil, d) }
            case Event.ArrowShot(_, _, _)  => { pebble = false | add(cue(Sound.Arrow, after(Beat.Arrow, pace#arrow)) :: Nil, d) }
            case Event.ArrowHit(_, _)      => add(cue(if (d#pebble) Sound.Clink else Sound.Hit, after(Beat.Flash, pace#strike)) :: Nil, d)
            case Event.BombThrown(_, _)    => { bomb = Some((i, Beat.Bomb)) | add(cue(Sound.Bomb, after(Beat.Bomb, pace#arrow * 2.5)) :: Nil, d) }
            case Event.BombHit(_, _)       =>
                let row = d#bomb |> Option.map(bomb -> flashOn(pace, bomb, pace#arrow * 0.5, i, Sound.Hit))
                    |> Option.getWithDefault(cue(Sound.Hit, after(Beat.Flash, pace#strike)));
                add(row :: Nil, d)
            case Event.ThunderCast(_)      => { bomb = Some((i, Beat.Thunder)) | add(cue(Sound.Bomb, after(Beat.Thunder, pace#arrow * 2.0)) :: Nil, d) }
            case Event.ThunderHit(_, _)    =>
                let row = d#bomb |> Option.map(cast -> flashOn(pace, cast, pace#arrow * 0.3, i, Sound.Hit))
                    |> Option.getWithDefault(cue(Sound.Hit, after(Beat.Flash, pace#strike)));
                add(row :: Nil, d)
            case Event.SmokeThrown(_)      => add(after(Beat.Smoke, pace#smoke * 0.5) :: cue(Sound.Puff, after(Beat.Puff, pace#smoke * 0.5)) :: Nil, d)
            case Event.EnemyBurned(_, _)   => add(cue(Sound.Hit, after(Beat.Flash, pace#strike)) :: Nil, d)
            case Event.TrapSprung(Trap.Kind.Alarm, _) => add(cue(Sound.Clink, after(Beat.Trap, pace#arrow)) :: Nil, d)
            case Event.TrapSprung(_, _)    => add(cue(Sound.Arrow, after(Beat.Trap, pace#arrow)) :: Nil, d)
            case Event.Guarded(_)          => add(cue(Sound.Guard, after(Beat.Flash, pace#strike)) :: Nil, d)
            case Event.AlarmRang(_)        => add(after(Beat.Alarm, pace#strike * 2.0) :: Nil, d)
            case Event.EnemyTrapped(_, _)  => add(cue(Sound.Hit, after(Beat.Flash, pace#strike)) :: Nil, d)
            case Event.EnemyPushed(_, _, _) => add(after(Beat.EnemyMove, pace#step) :: Nil, d)
            case Event.EnemyDied(_, _, _)  => add(cue(Sound.Defeat, after(Beat.Die, pace#strike * 1.6)) :: Nil, d)
            case Event.EnemyMoved(_, _, _) if not d#movesPlaced =>
                let together = moves |> List.zipWithIndex |> List.map(match (k, j) ->
                    if (k == 0) Timeline.append((j, Beat.EnemyMove), secs(pace#step)) else Timeline.join((j, Beat.EnemyMove), secs(pace#step)));
                { movesPlaced = true | add(together, d) }
            case Event.EnemyShot(_, _, _)  => add(cue(Sound.Arrow, after(Beat.EnemyArrow, pace#arrow)) :: Nil, d)
            case Event.EnemyWindup(_, _)   => add(cue(Sound.Windup, after(Beat.Windup, pace#strike * 1.5)) :: Nil, d)
            case Event.EnemyCharged(_, _, _) => { charge = Some((i, Beat.EnemyMove)) | add(cue(Sound.Charge, after(Beat.EnemyMove, pace#step * 1.4)) :: Nil, d) }
            case Event.HeroHurt(Run.Source.Charge(_), _, guarded) =>
                let sound = if (guarded) Sound.Guard else Sound.Hit;
                let row = d#charge |> Option.map(rush -> flashOn(pace, rush, pace#step * 1.2, i, sound)) |> Option.getWithDefault(cue(sound, after(Beat.Flash, pace#strike)));
                add(row :: Nil, d)
            case Event.HeroHurt(Run.Source.Melee(_), _, guarded) =>
                let sound = if (guarded) Sound.Guard else Sound.Hit;
                add(cue(Sound.Strike, after(Beat.EnemyStrike, pace#strike)) :: flashOn(pace, (i, Beat.EnemyStrike), pace#strike * 0.45, i, sound) :: Nil, d)
            case Event.HeroHurt(_, _, guarded) => add(cue(if (guarded) Sound.Guard else Sound.Hit, after(Beat.Flash, pace#strike)) :: Nil, d)
            case Event.PotionUsed(_, _)    => add(cue(Sound.Potion, after(Beat.Potion, pace#potion)) :: after(Beat.Heal, pace#strike * 1.5) :: Nil, d)
            case Event.Ate(_, _)           =>
                let bite = (beat: Beat, at: Float64) -> Timeline.atStartOf((i, Beat.Eat), (i, beat), secs(pace#eat * 0.15)) |> Timeline.withDelay(secs(pace#eat * at)) |> cue(Sound.Munch);
                add(cue(Sound.Munch, after(Beat.Eat, pace#eat)) :: bite(Beat.Bite2, 0.25) :: bite(Beat.Bite3, 0.5) :: after(Beat.Full, pace#strike) :: Nil, d)
            case Event.HeroDied            => add(cue(Sound.Fall, after(Beat.Fall, pace#strike * 4.0)) :: Nil, d)
            case _                         => d
        }

    def isEnemyMove(event: Event): Bool = match event {
        case Event.EnemyMoved(_, _, _) => true
        case _                         => false
    }

    /// 起点 at から測って、表が終わっていないか。
    pub def isPlaying(clock: Clock.Clock[c], at: Clock.Time[c], tl: Timeline.Timeline[Tag]): Bool =
        Clock.since(at, Clock.nowOf(clock)) < Timeline.endOf(tl)
}
