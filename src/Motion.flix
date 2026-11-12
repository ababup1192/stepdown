// Motion — 1 手の (手の前の Run, 手の後の Run, 出来事の列, 時刻表の進み) から、板と飛ぶ物と光の今の形を導く（純粋）。
// 時刻表の組み方は TurnTimeline。待機の揺れ・飛ぶ敵の上下・床のアイテムの浮き沈みは実時間（time）で、時刻表の外。
//
// ── 動き ──
//   移動はマスの間を弧で跳ね、殴る時は相手へ 0.3 マス踏み込んで戻る。当たると白く光って小さく震え、倒れた敵は縮んで消える。
//   矢は小さな板が飛び、爆弾は光と煙の円が広がる。拾ったアイテムは主人公へ吸い込まれ、上位を拾うと持ち物の枠が光る。
mod Motion {
    use fge::{Color, Curve, Num, Vec3}
    use Board.{Cell, Dir8}
    use Run.Event
    use TurnTimeline.Beat

    /// 1 手の見せ方の材料。progress = 時刻表のタグの今の進み（0..1。表に無いタグは 1）。
    pub type alias Turn = {
        before = Run.Rec,
        after = Run.Rec,
        events = List[(Int32, Event)],
        progress = TurnTimeline.Tag -> Float64
    }

    /// 今の形: 板（キャラ・アイテム・飛ぶ矢）・広がる円・持ち物の枠の光（系統 → 光。光っていない系統は入れない）。
    pub type alias Pose = { figures = List[SceneView.Figure], bursts = List[SceneView.Burst], slotGlow = Map[Item.Family, Float64] }

    /// 1 手の今の形。time = 実時間の秒・fallen = 倒れた画面か（時刻表が無くても倒れた姿にする）・light = 板の明るさ（look の figureLight）。
    /// WhyNot: 板に光を受けさせない — 手もとの灯りが板の正面へ当たり、トーンマップで絵の色が白へ褪せる。
    pub def poseOf(turn: Turn, time: Float64, fallen: Bool, light: Float64): Pose =
        let moves = movesOf(turn);
        let shaded = (f: SceneView.Figure) -> { let (r, g, b) = Color.channels(f#tint); { tint = Color.rgb(r * light, g * light, b * light), lit = false | f } };
        { figures = List.flatten(trapFigures(turn) :: heroFigure(turn, time, fallen) :: enemyFigures(turn, moves, time) :: itemFigures(turn, time) :: arrows(turn) :: smokeBalls(turn) :: Nil)
              |> List.map(shaded),
          bursts = bombBursts(turn) ::: smokeBursts(turn),
          slotGlow = turn#after#pack |> List.map(held -> (Run.familyOfHeld(held), glowOf(turn, Run.familyOfHeld(held)))) |> List.toMap }

    // ── 進みの読み方 ──

    def progressOf(turn: Turn, i: Int32, beat: Beat): Float64 = turn#progress((i, beat))

    def isActive(p: Float64): Bool = p > 0.0 and p < 1.0

    /// 進み p の間に 0 → 1 → 0 と山になる値（前と後は 0）。
    def pulse(p: Float64): Float64 = if (isActive(p)) Num.sin(p * Num.pi()) else 0.0

    def lerp(a: Vec3.Vec3, b: Vec3.Vec3, p: Float64): Vec3.Vec3 = Vec3.add(a, Vec3.mul(Vec3.sub(b, a), p))

    def point(cell: Cell): Vec3.Vec3 = FloorMesh.cellPoint(cell)

    def up(h: Float64): Vec3.Vec3 = Vec3.v3(0.0, h, 0.0)

    /// a から b への水平の向き（長さ 1。同じ点なら 0）。
    def toward(a: Vec3.Vec3, b: Vec3.Vec3): Vec3.Vec3 =
        let d = Vec3.sub(b, a);
        let len = Vec3.length(d);
        if (len < 0.0001) Vec3.zero() else Vec3.mul(d, 1.0 / len)

    def dirVector(dir: Dir8): Vec3.Vec3 = Vec3.sub(point(Board.step8(dir, (0, 0))), point((0, 0)))

    /// 当たった光の強さと震え（光の進み p から）。
    type alias Flash = { glow = Float64, shake = Float64 }

    def flashOf(p: Float64): Flash =
        if (not isActive(p)) { glow = 0.0, shake = 0.0 }
        else { glow = 1.0 - p, shake = 0.07 * Num.sin(p * Num.pi() * 6.0) * (1.0 - p) }

    /// 光を重ねる（強い方・震えは足す）。
    def mixFlash(a: Flash, b: Flash): Flash = { glow = Float64.max(a#glow, b#glow), shake = a#shake + b#shake }

    def noFlash(): Flash = { glow = 0.0, shake = 0.0 }

    /// 色を白へ寄せて明るくする（glow 1 で 2.4 倍の白）。
    def flashTint(base: Color, glow: Float64): Color =
        let (r, g, b) = Color.channels(Color.mix(base, Color.rgb(1.0, 1.0, 1.0), glow));
        let k = 1.0 + 1.4 * glow;
        Color.rgb(r * k, g * k, b * k)

    def white(): Color = Color.rgb(1.0, 1.0, 1.0)

    // ── 主人公 ──

    /// 出来事の列から、主人公への当たりの光（白 = 盾で 0・弾いた、赤 = HP が減った）。
    def heroFlash(turn: Turn): { white = Flash, red = Flash } =
        turn#events |> List.foldLeft((acc, ie) -> match ie {
            case (i, Event.HeroHurt(_, n, guarded)) if guarded or n <= 0 => { white = mixFlash(acc#white, flashOf(progressOf(turn, i, Beat.Flash))) | acc }
            case (i, Event.HeroHurt(_, _, _)) => { red = mixFlash(acc#red, flashOf(progressOf(turn, i, Beat.Flash))) | acc }
            case _                            => acc
        }, { white = noFlash(), red = noFlash() })

    /// 色を赤へ寄せて明るくする（glow 1 で赤く光る）。
    def redTint(base: Color, glow: Float64): Color =
        let (r, g, b) = Color.channels(Color.mix(base, Color.rgb(1.0, 0.18, 0.12), 0.8 * glow));
        let k = 1.0 + 0.9 * glow;
        Color.rgb(r * k, g * k, b * k)

    /// 主人公の足もとの今の点（移動の弧・進めない向きへの寄り・殴る踏み込み）。
    def heroPoint(turn: Turn): Vec3.Vec3 =
        let enemiesBefore = turn#before#enemies |> List.map(e -> (e#id, e#at)) |> List.toMap;
        let home = point(turn#after#hero#at);
        turn#events |> List.foldLeft((at, ie) -> match ie {
            case (i, Event.HeroMoved(fromCell, to)) =>
                let p = progressOf(turn, i, Beat.HeroMove);
                if (p >= 1.0) at else Vec3.add(lerp(point(fromCell), point(to), p), up(0.28 * pulse(p)))
            case (i, Event.HeroBlocked(dir)) => Vec3.add(at, Vec3.mul(dirVector(dir), 0.15 * pulse(progressOf(turn, i, Beat.HeroBump))))
            case (i, Event.HeroStruck(id, _)) =>
                let target = Map.get(id, enemiesBefore) |> Option.map(point) |> Option.getWithDefault(at);
                Vec3.add(at, Vec3.mul(toward(at, target), 0.3 * pulse(progressOf(turn, i, Beat.HeroStrike))))
            case _ => at
        }, home)

    /// 降りる・倒れるの進み（無ければ 0）と、薬の光。
    type alias HeroState = { descend = Float64, fall = Float64, potion = Float64 }

    def heroState(turn: Turn): HeroState =
        turn#events |> List.foldLeft((acc, ie) -> match ie {
            case (i, Event.Descended)        => { descend = progressOf(turn, i, Beat.Descend) | acc }
            case (i, Event.HeroDied)         => { fall = progressOf(turn, i, Beat.Fall) | acc }
            case (i, Event.PotionUsed(_, _)) => { potion = pulse(progressOf(turn, i, Beat.Heal)) | acc }
            case _                           => acc
        }, { descend = 0.0, fall = 0.0, potion = 0.0 })

    def heroFigure(turn: Turn, t: Float64, fallen: Bool): List[SceneView.Figure] =
        let state = heroState(turn);
        let fall = if (fallen) Float64.max(state#fall, if (List.exists(ie -> snd(ie) == Event.HeroDied, turn#events)) 0.0 else 1.0) else state#fall;
        let flashes = heroFlash(turn);
        let flash = mixFlash(flashes#white, flashes#red);
        let breathe = 1.0 - fall;
        let bob = up(0.025 * breathe * (0.5 + 0.5 * Num.sin(t * 2.6)));
        let stretch = 1.0 + 0.025 * breathe * Num.sin(t * 2.6 + 1.3);
        let height = heroHeight() * stretch * (1.0 - 0.4 * fall) * Float64.max(0.0, 1.0 - state#descend);
        let base = Color.mix(white(), Color.rgb(0.55, 0.5, 0.55), fall);
        let healed = Color.mix(base, Color.rgb(0.6, 1.0, 0.6), state#potion * 0.6);
        let at = Vec3.add(Vec3.add(heroPoint(turn), bob), Vec3.v3(flash#shake, 0.0 - 0.3 * state#descend, 0.0));
        if (height <= 0.01) Nil
        else { picture = Art.figure(Art.Figure.Hero), at = at, height = height,
               tint = redTint(flashTint(healed, Float64.max(flashes#white#glow, state#potion * 0.4)), flashes#red#glow), lit = flash#glow <= 0.0, shadow = 0.34 * (1.0 - state#descend) } :: Nil

    /// 主人公の板の高さ（m）。
    pub def heroHeight(): Float64 = 1.3

    // ── 敵 ──

    /// 敵の id → その手の移動（押されたのも含む。起きた順の (出来事の番号, 元のマス, 先のマス)）。
    def movesOf(turn: Turn): Map[Int32, List[(Int32, Cell, Cell)]] =
        turn#events |> List.foldRight((ie, acc) -> match ie {
            case (i, Event.EnemyMoved(id, fromCell, to))  => Map.insertWith((a, b) -> a ::: b, id, (i, fromCell, to) :: Nil, acc)
            case (i, Event.EnemyPushed(id, fromCell, to)) => Map.insertWith((a, b) -> a ::: b, id, (i, fromCell, to) :: Nil, acc)
            case (i, Event.EnemyCharged(id, fromCell, to)) if fromCell != to => Map.insertWith((a, b) -> a ::: b, id, (i, fromCell, to) :: Nil, acc)
            case _                                    => acc
        }, Map.empty())

    /// 敵 1 体への出来事の効き: 光・倒れる進み（倒れなければ None）・殴りかかる踏み込み。
    type alias Hit = { flash = Flash, dying = Option[Float64], lunge = Float64 }

    def hitsOf(turn: Turn): Map[Int32, Hit] =
        let none = { flash = noFlash(), dying = None, lunge = 0.0 };
        let flashed = (id: Int32, i: Int32, acc: Map[Int32, Hit]) -> {
            let h = Map.getWithDefault(id, none, acc);
            Map.insert(id, { flash = mixFlash(h#flash, flashOf(progressOf(turn, i, Beat.Flash))) | h }, acc)
        };
        let lunged = (id: Int32, i: Int32, acc: Map[Int32, Hit]) -> {
            let h = Map.getWithDefault(id, none, acc);
            Map.insert(id, { lunge = Float64.max(h#lunge, pulse(progressOf(turn, i, Beat.EnemyStrike))) | h }, acc)
        };
        let dies = (id: Int32, i: Int32, beat: Beat, acc: Map[Int32, Hit]) -> {
            let h = Map.getWithDefault(id, none, acc);
            Map.insert(id, { dying = Some(progressOf(turn, i, beat)) | h }, acc)
        };
        turn#events |> List.foldLeft((acc, ie) -> match ie {
            case (i, Event.HeroStruck(id, _))  => flashed(id, i, acc)
            case (i, Event.ArrowHit(id, _))    => flashed(id, i, acc)
            case (i, Event.BombHit(id, _))     => flashed(id, i, acc)
            case (i, Event.ThunderHit(id, _))  => flashed(id, i, acc)
            case (i, Event.EnemyBurned(id, _)) => flashed(id, i, acc)
            case (i, Event.EnemyDied(id, _, _)) => dies(id, i, Beat.Die, acc)
            case (i, Event.EnemyExploded(id, _)) => dies(id, i, Beat.Bomb, acc)
            case (i, Event.HeroHurt(Run.Source.Melee(id), _, _)) => lunged(id, i, acc)
            case _ => acc
        }, Map.empty())

    /// 溜めている敵の色: 赤く脈打つ（溜めた技が次の手に来ると読ませる）。
    def windupTint(e: Enemy.Enemy, t: Float64, base: Color): Color =
        if (e#intent == Enemy.Intent.Idle) base
        else Color.mix(base, Color.rgb(1.0, 0.25, 0.15), 0.35 + 0.25 * Num.sin(t * 9.0))

    def figureOf(kind: Enemy.Kind): Art.Figure = match kind {
        case Enemy.Kind.Jelly  => Art.Figure.Jelly
        case Enemy.Kind.Swarm  => Art.Figure.Swarm
        case Enemy.Kind.Bat    => Art.Figure.Bat
        case Enemy.Kind.Archer => Art.Figure.Archer
        case Enemy.Kind.Shield => Art.Figure.Shield
        case Enemy.Kind.Charger => Art.Figure.Charger
        case Enemy.Kind.Bomber => Art.Figure.Bomber
    }

    /// 板の高さ（m）。
    pub def heightOf(kind: Enemy.Kind): Float64 = match kind {
        case Enemy.Kind.Jelly  => 0.8
        case Enemy.Kind.Swarm  => 0.5
        case Enemy.Kind.Bat    => 0.95
        case Enemy.Kind.Archer => 1.0
        case Enemy.Kind.Shield => 1.05
        case Enemy.Kind.Charger => 0.95
        case Enemy.Kind.Bomber => 0.7
    }

    /// 床から浮かせる高さ（飛ぶ敵だけ）。
    pub def liftOf(kind: Enemy.Kind): Float64 = if (Enemy.isFlying(kind)) 0.5 else 0.0

    /// 手の後に生きている敵と、この手で倒れた敵（手の前の姿で、縮み終わるまで）。
    def enemyFigures(turn: Turn, moves: Map[Int32, List[(Int32, Cell, Cell)]], t: Float64): List[SceneView.Figure] =
        let hits = hitsOf(turn);
        let alive = turn#after#enemies |> List.map(e -> e#id) |> List.toSet;
        let fallen = turn#before#enemies |> List.filter(e -> not Set.memberOf(e#id, alive));
        let heroAt = point(turn#after#hero#at);
        List.append(turn#after#enemies, fallen) |> List.flatMap(e ->
            enemyFigure(turn, t, heroAt, Map.getWithDefault(e#id, Nil, moves), Map.get(e#id, hits), e) |> Option.toList |> List.flatMap(body -> body :: shieldOf(e, body)))

    /// 盾持ちの盾: 向き facing の側に盾の絵を体より手前へ寄せて立てる（正面がどこかを読ませる）。
    def shieldOf(e: Enemy.Enemy, body: SceneView.Figure): List[SceneView.Figure] =
        if (e#kind != Enemy.Kind.Shield) Nil
        else {
            let ahead = Vec3.sub(point(Board.step8(e#facing, e#at)), point(e#at));
            { picture = Art.icon(Art.Icon.Shield), at = Vec3.add(Vec3.add(body#at, Vec3.mul(ahead, 0.34)), up(0.12 * body#height)),
              height = 0.55 * body#height, tint = body#tint, lit = body#lit, shadow = 0.0 } :: Nil
        }

    /// 今の移動の途中の点（移動が全部済んでいれば None）。
    def walkingPoint(turn: Turn, moves: List[(Int32, Cell, Cell)]): Option[Vec3.Vec3] =
        moves |> List.find(m -> { let (i, _, _) = m; progressOf(turn, i, Beat.EnemyMove) < 1.0 })
            |> Option.map(match (i, fromCell, to) -> {
                let p = progressOf(turn, i, Beat.EnemyMove);
                Vec3.add(lerp(point(fromCell), point(to), p), up(0.2 * pulse(p)))
            })

    /// 移動を全部終えた点（移動が無ければ e の今のマス）。
    def settledPoint(moves: List[(Int32, Cell, Cell)], e: Enemy.Enemy): Vec3.Vec3 =
        List.last(moves) |> Option.map(m -> { let (_, _, to) = m; point(to) }) |> Option.getWithDefault(point(e#at))

    def enemyFigure(turn: Turn, t: Float64, heroAt: Vec3.Vec3, moves: List[(Int32, Cell, Cell)], hit: Option[Hit], e: Enemy.Enemy): Option[SceneView.Figure] =
        let h = hit |> Option.getWithDefault({ flash = noFlash(), dying = None, lunge = 0.0 });
        let dying = h#dying |> Option.getWithDefault(0.0);
        if (dying >= 1.0) None
        else {
            let base = walkingPoint(turn, moves) |> Option.getWithDefault(settledPoint(moves, e));
            let lunge = Vec3.mul(toward(base, heroAt), 0.3 * h#lunge);
            let kindTint = windupTint(e, t, white());
            let tint = if (Enemy.isAwake(e)) kindTint else Color.mix(kindTint, Color.rgb(0.2, 0.22, 0.35), 0.45);
            let bob = if (Enemy.isFlying(e#kind)) 0.08 * Num.sin(t * 3.3 + Int32.toFloat64(e#id)) else 0.0;
            let scale = 1.0 - dying;
            Some({ picture = Art.figure(figureOf(e#kind)),
                   at = Vec3.add(Vec3.add(base, lunge), Vec3.v3(h#flash#shake, (liftOf(e#kind) + bob) * scale, 0.0)),
                   height = heightOf(e#kind) * scale * (if (e#intent == Enemy.Intent.Idle) 1.0 else 0.88), tint = flashTint(tint, Float64.max(h#flash#glow, dying * 0.6)), lit = h#flash#glow <= 0.0 and dying <= 0.0,
                   shadow = (if (e#kind == Enemy.Kind.Swarm) 0.2 else 0.3) * scale })
        }

    // ── 罠 ──

    /// 見えている罠（踏まれた罠）の床の板: 罠の絵の低い板を床に置く（影は落とさない）。
    def trapFigures(turn: Turn): List[SceneView.Figure] =
        Run.seenTraps(turn#after) |> Map.toList |> List.map(match (cell, kind) ->
            { picture = Art.icon(Art.iconOfTrap(kind)), at = Vec3.add(point(cell), up(0.02)), height = 0.42, tint = Color.rgb(0.85, 0.85, 0.85), lit = false, shadow = 0.0 })

    // ── アイテム ──

    /// 床のアイテム 1 つ: 系統のアイコンの小さな板を少し浮かせる。
    def itemFigure(t: Float64, cell: Cell, kind: Item.Kind, at: Vec3.Vec3, scale: Float64): SceneView.Figure =
        let (x, _) = cell;
        { picture = Art.icon(Art.iconOfKind(kind)),
          at = Vec3.add(at, up((0.16 + 0.04 * Num.sin(t * 2.0 + Int32.toFloat64(x))) * scale)),
          height = 0.46 * scale, tint = white(), lit = false, shadow = 0.18 * scale }

    /// 床のアイテム（手の後の物）と、この手で拾った物（主人公へ吸い込まれ終わるまで）。
    def itemFigures(turn: Turn, t: Float64): List[SceneView.Figure] =
        let lying = Map.toList(turn#after#items) |> List.map(match (cell, kind) -> itemFigure(t, cell, kind, point(cell), 1.0));
        let pickedAt = turn#events |> List.findMap(ie -> match ie {
            case (i, Event.Picked(_, _)) => Some(progressOf(turn, i, Beat.Pick))
            case _                       => None
        });
        let hero = Vec3.add(point(turn#after#hero#at), up(0.6));
        let taken = Map.toList(turn#before#items) |> List.filter(e -> not Map.memberOf(fst(e), turn#after#items))
            |> List.filterMap(match (cell, kind) -> pickedAt |> Option.filter(p -> p < 1.0) |> Option.map(p ->
                itemFigure(t, cell, kind, lerp(point(cell), hero, p * p), 1.0 - 0.75 * p)));
        List.append(lying, taken)

    // ── 飛ぶ物と広がる円 ──

    def arrowFigure(fromCell: Vec3.Vec3, to: Vec3.Vec3, p: Float64): List[SceneView.Figure] =
        if (not isActive(p)) Nil
        else { picture = Art.icon(Art.Icon.Arrow), at = Vec3.add(lerp(fromCell, to, p), up(0.32)), height = 0.42,
               tint = Color.rgb(1.3, 1.25, 1.15), lit = false, shadow = 0.08 } :: Nil

    /// 小石: 石 1 つの板が低い山なりで飛び、着いた所で小さく 1 回跳ねる（進みの 7 割で着く）。持ち物の枠と床の板は 3 つの塊の絵のまま。
    def pebbleFigure(fromCell: Vec3.Vec3, to: Vec3.Vec3, p: Float64): List[SceneView.Figure] =
        if (not isActive(p)) Nil
        else {
            let land = 0.7;
            let travel = Curve.pieces((0.0, 0.0) :: (land, 1.0) :: (1.0, 1.25) :: Nil, p);
            let hop = if (p < land) 0.45 * Curve.arch01(p / land) else 0.15 * Curve.arch01((p - land) / (1.0 - land));
            let at = Vec3.add(lerp(fromCell, to, travel), up(0.3 + hop));
            { picture = Art.icon(Art.Icon.PebbleOne), at = at, height = 0.45, tint = white(), lit = false, shadow = 0.06 } :: Nil
        }

    /// 飛んでいる矢と小石（主人公の矢・小石と弓兵の矢）。
    def arrows(turn: Turn): List[SceneView.Figure] =
        let hero = point(turn#after#hero#at);
        turn#events |> List.flatMap(ie -> match ie {
            case (i, Event.ArrowShot(_, kind, end)) if Item.isStone(Item.familyOf(kind)) => pebbleFigure(hero, point(end), progressOf(turn, i, Beat.Arrow))
            case (i, Event.ArrowShot(_, _, end))  => arrowFigure(hero, point(end), progressOf(turn, i, Beat.Arrow))
            case (i, Event.EnemyShot(_, fromCell, to)) => arrowFigure(point(fromCell), point(to), progressOf(turn, i, Beat.EnemyArrow))
            case _                                 => Nil
        })

    /// 爆弾の光（橙の外側と明るい芯）と煙（遅れて昇って広がる）。
    def burstsAt(center: Vec3.Vec3, p: Float64): List[SceneView.Burst] =
        if (not isActive(p)) Nil
        else {
            let fade = 1.0 - p;
            { at = Vec3.add(center, up(0.5)), radius = 0.5 + 1.7 * p, color = Color.rgb(1.0, 0.5, 0.12), alpha = Float64.min(1.0, 1.3 * fade), glow = true }
                :: { at = Vec3.add(center, up(0.55)), radius = 0.3 + 0.7 * p, color = Color.rgb(1.0, 0.9, 0.6), alpha = fade * fade, glow = true }
                :: { at = Vec3.add(center, up(0.7 + 0.6 * p)), radius = 0.4 + 1.5 * p, color = Color.rgb(0.3, 0.27, 0.26), alpha = 0.8 * pulse(p), glow = false }
                :: Nil
        }

    def bombBursts(turn: Turn): List[SceneView.Burst] =
        turn#events |> List.flatMap(ie -> match ie {
            case (i, Event.BombThrown(_, center)) => burstsAt(point(center), progressOf(turn, i, Beat.Bomb))
            case (i, Event.EnemyExploded(_, center)) => burstsAt(point(center), progressOf(turn, i, Beat.Bomb))
            case _                                => Nil
        })

    /// 煙玉の着く所: この手で眠った敵の真ん中（眠った敵がいなければ主人公の前）。
    def smokeTarget(turn: Turn, hero: Cell): Vec3.Vec3 =
        let at = (turn#after#enemies ::: turn#before#enemies) |> List.map(e -> (e#id, e#at)) |> List.toMap;
        let slept = turn#events |> List.filterMap(ie -> match ie {
            case (_, Event.EnemySlept(id, _)) => Map.get(id, at)
            case _                            => None
        }) |> List.map(point);
        if (List.isEmpty(slept)) Vec3.add(point(hero), Vec3.v3(0.0, 0.0, 0.5))
        else Vec3.mul(List.foldLeft(Vec3.add, Vec3.zero(), slept), 1.0 / Int32.toFloat64(List.length(slept)))

    /// 飛んでいる煙玉（主人公から着く所へ山なり）。
    def smokeBalls(turn: Turn): List[SceneView.Figure] =
        turn#events |> List.flatMap(ie -> match ie {
            case (i, Event.SmokeThrown(cell)) =>
                let p = progressOf(turn, i, Beat.Smoke);
                if (not isActive(p)) Nil
                else { picture = Art.icon(Art.Icon.Smoke), at = Vec3.add(lerp(point(cell), smokeTarget(turn, cell), p), up(0.5 + 0.9 * Curve.arch01(p))),
                       height = 0.85, tint = white(), lit = false, shadow = 0.16 } :: Nil
            case _ => Nil
        })

    /// 煙玉が弾けた煙: 濃さの違う灰色の小さな円が 9 つ、もくもくと広がって昇りながら薄れる（光らない）。
    def smokeBursts(turn: Turn): List[SceneView.Burst] =
        turn#events |> List.flatMap(ie -> match ie {
            case (i, Event.SmokeThrown(cell)) =>
                let p = progressOf(turn, i, Beat.Puff);
                let at = smokeTarget(turn, cell);
                if (not isActive(p)) Nil
                else List.range(0, 9) |> List.map(k -> {
                    let a = Int32.toFloat64(k) * 2.4;
                    let ring = 0.25 + 0.12 * Int32.toFloat64(Int32.modulo(k, 3));
                    let spread = ring * 2.2 * Curve.pieces((0.0, 0.0) :: (0.3, 0.7) :: (1.0, 1.0) :: Nil, p);
                    let shade = 0.42 + 0.06 * Int32.toFloat64(Int32.modulo(k, 4));
                    { at = Vec3.add(at, Vec3.v3(spread * Num.cos(a), 0.35 + 0.55 * p * (0.6 + 0.15 * Int32.toFloat64(Int32.modulo(k, 3))), spread * Num.sin(a))),
                      radius = 0.16 + 0.32 * p, color = Color.rgb(shade, shade, shade + 0.03), alpha = 0.9 * (1.0 - p * p), glow = false }
                })
            case _ => Nil
        })

    /// 系統 family の持ち物の枠の光（上位を拾った・その系統を使った手の間だけ 1 → 0）。
    def glowOf(turn: Turn, family: Item.Family): Float64 =
        let fading = (acc: Float64, q: Float64) -> if (isActive(q)) Float64.max(acc, 1.0 - q) else acc;
        turn#events |> List.foldLeft((acc, ie) -> match ie {
            case (i, Event.Picked(kind, Item.Pick.Upgraded)) if Item.familyOf(kind) == family => fading(acc, progressOf(turn, i, Beat.Upgrade))
            case (i, Event.PotionUsed(kind, _)) if Item.familyOf(kind) == family => fading(acc, progressOf(turn, i, Beat.Potion))
            case (i, Event.ArrowShot(_, kind, _)) if Item.familyOf(kind) == family => fading(acc, progressOf(turn, i, Beat.Arrow))
            case (i, Event.BombThrown(kind, _)) if Item.familyOf(kind) == family => fading(acc, progressOf(turn, i, Beat.Bomb))
            case (i, Event.SmokeThrown(_)) if family == Item.Family.Smoke => fading(acc, progressOf(turn, i, Beat.Smoke))
            case _ => acc
        }, 0.0)
}
