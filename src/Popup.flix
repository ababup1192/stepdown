// Popup — 1 手の出来事から作る浮かぶ数と、HP の棒の減った分・増えた分（純粋）。
// 時刻は実時間。始まりは「その出来事の時刻表の始まりの秒」を手を当てた時の実時間に足した所で、
// 見せ終える前に飛ばされたら今へ早める（時刻表に乗せないのは、飛ばした手の数を消さないため）。
mod Popup {
    use fge::{Clock, Lifetime, Timeline}
    use Board.Cell
    use Run.Event
    use TurnTimeline.Beat

    /// 数の種類（色と大きさが分かれる）。
    ///   Dealt   … 主人公が敵に与えた
    ///   Taken   … 主人公が敵から受けた
    ///   Healed  … 薬で HP が戻った
    ///   Fed     … 食べ物で満腹度が戻った
    ///   Blocked … 盾で 0 になった・弾いた
    ///   Starved … 満腹度 0 で減った
    ///   Burned  … 燃えている敵が減った
    ///   Exp     … 倒した敵の経験値
    pub enum Kind with Eq, ToString {
        case Dealt
        case Taken
        case Healed
        case Fed
        case Blocked
        case Starved
        case Burned
        case Exp
    }

    /// 浮かぶ数 1 つ。item = 頭の上に一瞬出す物（薬・食べ物）・lift = 出る段（同じマスでまだ消えていない数の一番上の段の 1 つ上。無ければ 0）。
    pub type alias Popup = { kind = Kind, cell = Cell, amount = Int32, item = Option[Item.Kind], lift = Int32, startedAt = Clock.Time[Clock.Real] }

    /// HP の棒の動き: before（動く前に見えていた値）から low まで減り（hitAt から）、そこから to まで戻る（healAt から。戻らなければ None）。
    pub type alias Trail = { before = Float64, low = Int32, to = Int32, hitAt = Clock.Time[Clock.Real], healAt = Option[Clock.Time[Clock.Real]] }

    /// 見せる長さ（look の Doc から）。life = 数が消えるまで・hold = HP の減った分を残す長さ・shrink = HP の棒が縮む・伸びる長さ。
    pub type alias Timing = { life = Float64, hold = Float64, shrink = Float64 }

    /// 出来事の数を見せ始める時刻表のタグ（無ければ前の数と同じ時）。
    def beatOf(event: Event): Option[Beat] = match event {
        case Event.HeroStruck(_, _)  => Some(Beat.Flash)
        case Event.ArrowHit(_, _)    => Some(Beat.Flash)
        case Event.BombHit(_, _)     => Some(Beat.Flash)
        case Event.EnemyBurned(_, _) => Some(Beat.Flash)
        case Event.EnemyTrapped(_, _) => Some(Beat.Flash)
        case Event.Guarded(_)        => Some(Beat.Flash)
        case Event.ThunderHit(_, _)  => Some(Beat.Flash)
        case Event.HeroHurt(_, _, _) => Some(Beat.Flash)
        case Event.PotionUsed(_, _)  => Some(Beat.Heal)
        case Event.DollSaved(_)      => Some(Beat.Heal)
        case Event.Ate(_, _)         => Some(Beat.Full)
        case Event.EnemyDied(_, _, _) => Some(Beat.Die)
        case _                       => None
    }

    /// 出来事 1 つの数（種類・量・物）。数を出さない出来事は None。
    def numberOf(event: Event): Option[(Kind, Int32, Option[Item.Kind])] = match event {
        case Event.HeroStruck(_, n)  => dealt(n)
        case Event.ArrowHit(_, n)    => dealt(n)
        case Event.Guarded(_)        => Some((Kind.Blocked, 0, None))
        case Event.ThunderHit(_, n)  => dealt(n)
        case Event.BombHit(_, n)     => Some((Kind.Dealt, n, None))
        case Event.EnemyBurned(_, n) => Some((Kind.Burned, n, None))
        case Event.EnemyTrapped(_, n) => Some((Kind.Dealt, n, None))
        case Event.HeroHurt(Run.Source.Starve, n, _) => Some((Kind.Starved, n, None))
        case Event.HeroHurt(_, n, guarded) => if (guarded or n <= 0) Some((Kind.Blocked, 0, None)) else Some((Kind.Taken, n, None))
        case Event.PotionUsed(kind, n) => Some((Kind.Healed, n, Some(kind)))
        case Event.DollSaved(n)      => Some((Kind.Healed, n, Some(Item.Kind.Doll)))
        case Event.Ate(kind, n)      => Some((Kind.Fed, n, Some(kind)))
        case Event.ExpGained(n)      => Some((Kind.Exp, n, None))
        case _                       => None
    }

    /// 与えた数（盾で防がれた 0 は出さない。続く Guarded が灰色の 0 と盾を出す）。
    def dealt(n: Int32): Option[(Kind, Int32, Option[Item.Kind])] = if (n <= 0) None else Some((Kind.Dealt, n, None))

    /// 並べている途中: 足した数（逆順）・前の数の時刻表の秒・最後に倒れた敵のマス。
    type alias Draft = { made = List[Popup], offset = Float64, lastDied = Option[Cell] }

    /// 1 手の出来事から浮かぶ数を作る。before・after = 手の前と後・tl = その手の時刻表・now = 手を当てた時の実時間・alive = まだ出ている数。
    /// 出る時刻は出来事の時刻表の始まり。同じマスで、その時刻にまだ消えていない数があれば、その一番上の 1 段上から出す。
    /// WhyNot: 後の数を前の数が消えるまで遅らせる — 続けて受けた時に数が遅れて出て、どの手の数か分からなくなる。
    pub def fromTurn(timing: Timing, turn: { before = Run.Rec, after = Run.Rec, events = List[Event] }, tl: Timeline.Timeline[TurnTimeline.Tag],
                     now: Clock.Time[Clock.Real], alive: List[Popup]): List[Popup] =
        let foes = List.toMap((turn#after#enemies ::: turn#before#enemies) |> List.map(e -> (e#id, e#at)));
        let hero = turn#after#hero#at;
        let cellOf = (event: Event, d: Draft) -> match event {
            case Event.HeroStruck(id, _)  => Map.getWithDefault(id, hero, foes)
            case Event.ArrowHit(id, _)    => Map.getWithDefault(id, hero, foes)
            case Event.BombHit(id, _)     => Map.getWithDefault(id, hero, foes)
            case Event.EnemyBurned(id, _) => Map.getWithDefault(id, hero, foes)
            case Event.EnemyTrapped(id, _) => Map.getWithDefault(id, hero, foes)
            case Event.Guarded(id)        => Map.getWithDefault(id, hero, foes)
            case Event.ThunderHit(id, _)  => Map.getWithDefault(id, hero, foes)
            case Event.ExpGained(_)       => Option.getWithDefault(hero, d#lastDied)
            case _                        => hero
        };
        let step = (d: Draft, ie: (Int32, Event)) -> {
            let (i, event) = ie;
            let offset = beatOf(event) |> Option.flatMap(beat -> Timeline.clipOf((i, beat), tl)) |> Option.map(c -> c#start) |> Option.getWithDefault(d#offset);
            let died = match event {
                case Event.EnemyDied(_, _, cell) => Some(cell)
                case _                           => d#lastDied
            };
            let made = numberOf(event) |> Option.map(match (kind, amount, item) -> {
                let cell = cellOf(event, d);
                let startedAt = Clock.after(offset, now);
                let lift = stackOn(timing, cell, startedAt, alive ::: d#made);
                { kind = kind, cell = cell, amount = amount, item = item, lift = lift, startedAt = startedAt } :: d#made
            }) |> Option.getWithDefault(d#made);
            { made = made, offset = offset, lastDied = died }
        };
        List.reverse((List.zipWithIndex(turn#events) |> List.foldLeft(step, { made = Nil, offset = 0.0, lastDied = None }))#made)

    /// cell に at の時刻で出す数の段: その時刻に出ている同じマスの数の一番上の段の 1 つ上（無ければ 0）。
    def stackOn(timing: Timing, cell: Cell, at: Clock.Time[Clock.Real], shown: List[Popup]): Int32 =
        shown |> List.filter(p -> p#cell == cell and Lifetime.aliveOn(at, spanOf(timing, p)))
            |> List.map(p -> p#lift + 1) |> List.foldLeft(Int32.max, 0)

    /// 数 1 つの寿命（実時間）。
    def spanOf(timing: Timing, p: Popup): Lifetime.Span[Clock.Real] = { startedAt = p#startedAt, dur = timing#life }

    /// 消えるまでの長さを過ぎた数を外す（まだ始まっていない数は残す）。
    pub def prune(timing: Timing, now: Clock.Time[Clock.Real], popups: List[Popup]): List[Popup] =
        popups |> List.filter(p -> Lifetime.remainingOn(now, spanOf(timing, p)) > 0.0)

    /// まだ始まっていない数を今へ早める（手を見せ終えたことにした時）。
    pub def hurry(now: Clock.Time[Clock.Real], popups: List[Popup]): List[Popup] =
        popups |> List.map(p -> if (p#startedAt > now) { startedAt = now | p } else p)

    /// 数を seconds 秒だけ前に始めたことにする（描き出しの途中の場面）。
    pub def shift(seconds: Float64, popups: List[Popup]): List[Popup] =
        popups |> List.map(p -> { startedAt = Clock.after(0.0 - seconds, p#startedAt) | p })

    /// 動いていない HP の棒（今の値のまま）。
    pub def still(hp: Int32): Trail = { before = Int32.toFloat64(hp), low = hp, to = hp, hitAt = Clock.never(), healAt = None }

    /// HP の棒の動きを seconds 秒だけ前に始めたことにする（描き出しの途中の場面）。
    pub def shiftTrail(seconds: Float64, trail: Trail): Trail =
        { hitAt = Clock.after(0.0 - seconds, trail#hitAt), healAt = trail#healAt |> Option.map(Clock.after(0.0 - seconds)) | trail }

    /// 0..1 に収める。
    def unit(x: Float64): Float64 = Float64.max(0.0, Float64.min(1.0, x))

    /// 終わりへ向けてゆっくりになる進み。
    def easeOut(p: Float64): Float64 = 1.0 - (1.0 - p) * (1.0 - p)

    /// HP の棒の今の形: front = ふつうの色の端・extra = 色を付けた所の端（減った分は赤・増えた分は緑）・healing = 増えているか。
    pub def barOf(timing: Timing, now: Clock.Time[Clock.Real], trail: Trail): { front = Float64, extra = Float64, healing = Bool } =
        let low = Int32.toFloat64(trail#low);
        let to = Int32.toFloat64(trail#to);
        let hit = Clock.since(trail#hitAt, now);
        let heal = trail#healAt |> Option.map(at -> Clock.since(at, now)) |> Option.filter(e -> e >= 0.0);
        let shrinking = (low < trail#before);
        let left = trail#before + (low - trail#before) * easeOut(unit((hit - timing#hold) / Float64.max(0.01, timing#shrink)));
        match heal {
            case Some(elapsed) =>
                let grown = low + (to - low) * easeOut(unit(elapsed / Float64.max(0.01, timing#shrink)));
                let settled = elapsed > timing#shrink + timing#hold;
                { front = if (settled) to else low, extra = grown, healing = true }
            case None =>
                if (hit < 0.0 or not shrinking) { front = trail#before, extra = trail#before, healing = false }
                else { front = low, extra = left, healing = false }
        }

    /// 1 手の出来事の HP の動き: 一番下がった値・最初に減った出来事の番号・その後で最初に戻った出来事の番号。
    type alias Path = { hp = Int32, low = Int32, hit = Option[Int32], heal = Option[Int32] }

    def pathOf(start: Int32, events: List[Event]): Path =
        List.zipWithIndex(events) |> List.foldLeft((acc, ie) -> match ie {
            case (i, Event.HeroHurt(_, n, guarded)) if not guarded and n > 0 =>
                let hp = acc#hp - n;
                { hp = hp, low = Int32.min(acc#low, hp), hit = Option.withDefault(default = Some(i), acc#hit) | acc }
            case (i, Event.PotionUsed(_, n)) => { hp = acc#hp + n, heal = Option.withDefault(default = Some(i), acc#heal) | acc }
            case (_, Event.Regenerated(n))   => { hp = acc#hp + n | acc }
            case _                           => acc
        }, { hp = start, low = start, hit = None, heal = None })

    /// 1 手で HP が変わったら棒の動きを作り直す（変わらなければ前のまま）。減り始めは最初に減った出来事の時刻表の始まり、
    /// 戻り始めは回復の輪の始まり（薬が無ければ減り終えた後・減っていなければすぐ）。
    pub def trailOf(timing: Timing, turn: { before = Run.Rec, after = Run.Rec, events = List[Event] }, tl: Timeline.Timeline[TurnTimeline.Tag],
                    now: Clock.Time[Clock.Real], old: Trail): Trail =
        let start = turn#before#hero#hp;
        let hp = turn#after#hero#hp;
        let path = pathOf(start, turn#events);
        let low = Int32.min(path#low, hp);
        let clipAt = (i: Int32, beat: Beat) -> Timeline.clipOf((i, beat), tl) |> Option.map(c -> c#start);
        let hitAt = path#hit |> Option.flatMap(i -> clipAt(i, Beat.Flash)) |> Option.getWithDefault(0.0);
        let healAt = if (hp <= low) None
            else Some(path#heal |> Option.flatMap(i -> clipAt(i, Beat.Heal))
                |> Option.getWithDefault(if (low < start) hitAt + timing#hold + timing#shrink else 0.0));
        if (hp == start and low == start) old
        else {
            let shown = barOf(timing, now, old);
            let before = if (old#to != start) Int32.toFloat64(start)
                else if (shown#healing) shown#extra
                else Float64.max(shown#front, shown#extra);
            { before = before, low = low, to = hp, hitAt = Clock.after(hitAt, now), healAt = healAt |> Option.map(at -> Clock.after(at, now)) }
        }
}
