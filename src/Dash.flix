// Dash — ダッシュ: 始めた時に最後まで計算する（1 歩ずつ同じ Step.step を通す）。見せる側は列を 1 歩ずつ流す。
// 止まる（その歩で止まる）: 敵が見えた・拾った・食べた・コインを拾った・階段に乗った・次が歩けない・分かれ道・部屋の番号が変わった・HP が減った・
// 殴った・矢か小石か爆弾か煙玉を使った・満腹度が 0 になった・選ぶ場面が立った・部屋の中で通路の口の横に来た・次のマスが見えている罠（見えていない罠は踏んで止まる）・
// 主人公のマスか次のマスが敵の予告（赤）に入った・どれかの敵が溜めている。
// 一本道の通路の角は自動で曲がる（斜めにも曲がる）。始めた時に敵が見えていれば 1 歩だけ。始める向きは上下左右。
mod Dash {

    use Board.{Cell, Dir, Dir8}
    use Run.{Ctx, Event, Input}

    /// 1 歩ずつの (その歩の後の Run, 出来事) の列。最初の歩が進めなければ、その 1 歩（HeroBlocked）だけ。
    pub def dash(ctx: Ctx, runState: Run.State, dir: Dir): List[(Run.State, List[Event])] =
        if (Run.seesAwakeEnemy(ctx, runState)) Step.step(ctx, runState, Input.Move(Board.toDir8(dir))) :: Nil
        else stride(ctx, runState, Board.toDir8(dir), ctx#board#cols * ctx#board#rows)

    def stride(ctx: Ctx, runState: Run.State, dir: Dir8, budget: Int32): List[(Run.State, List[Event])] =
        let (next, events) = Step.step(ctx, runState, Input.Move(dir));
        let turn = if (budget <= 1 or stopsAfter(ctx, runState, next, events)) None else nextDir(ctx, next, dir) |> Option.filter(d -> isSafeAhead(ctx, next, d));
        match turn {
            case Some(d) => (next, events) :: stride(ctx, next, d, budget - 1)
            case None    => (next, events) :: Nil
        }

    /// この歩で止まるか。
    def stopsAfter(ctx: Ctx, before: Run.State, after: Run.State, events: List[Event]): Bool =
        let (was, now) = (Run.heroOf(before), Run.heroOf(after));
        Run.statusOf(after) != Run.Status.Playing
            or now#hp < was#hp
            or not List.isEmpty(Run.pendingOf(after))
            or (Run.rec(after)#fullness <= 0 and Run.rec(before)#fullness > 0)
            or List.exists(isStop, events)
            or Board.tileAt(now#at, ctx#board) == Board.Tile.Stairs
            or Board.roomAt(now#at, ctx#board) != Board.roomAt(was#at, ctx#board)
            or Run.seesAwakeEnemy(ctx, after)
            or reachesOpening(ctx#board, was#at, now#at)

    /// 次の歩へ進んでよいか: どの敵も溜めておらず、主人公のマスも次のマスも敵の予告（赤）に入っていない。
    /// WhyNot: 赤に入る歩だけを止める — 主人公のマスが赤なら待つか戻るかを人が選ぶ所で、ダッシュが代わりに選んでしまう。
    def isSafeAhead(ctx: Ctx, runState: Run.State, dir: Dir8): Bool =
        let at = Run.heroOf(runState)#at;
        not List.exists(e -> e#intent != Enemy.Intent.Idle, Run.enemiesOf(runState))
            and not Preview.isDangerous(ctx, runState, at) and not Preview.isDangerous(ctx, runState, Board.step8(dir, at))

    /// 部屋の中で、上下左右の隣に前の歩には無かった通路の口が来たか（壁際を歩いていて横の通路を通り過ぎない）。
    def reachesOpening(b: Board.Board, was: Cell, now: Cell): Bool =
        Board.roomAt(now, b) != 0 and not Set.isEmpty(Set.difference(openingsBeside(b, now), Set.insert(now, openingsBeside(b, was))))

    def openingsBeside(b: Board.Board, at: Cell): Set[Cell] =
        Board.dirs() |> List.map(d -> Board.step8(Board.toDir8(d), at))
            |> List.filter(c -> Board.isWalkable(c, b) and Board.roomAt(c, b) == 0) |> List.toSet

    def isStop(e: Event): Bool = match e {
        case Event.Picked(_, _)       => true
        case Event.Ate(_, _)          => true
        case Event.CoinPicked(_)      => true
        case Event.ArrowShot(_, _, _) => true
        case Event.BombThrown(_, _)   => true
        case Event.SmokeThrown(_)     => true
        case Event.HeroBlocked(_)     => true
        case Event.HeroStruck(_, _)   => true
        case Event.HeroHurt(_, _, _)  => true
        case Event.TrapSprung(_, _)   => true
        case _                        => false
    }

    /// 次の歩の向き。部屋の中はまっすぐ進めるなら同じ向き。
    /// 通路は 8 近傍の行けるマスのうち、後ろ側（来たマスとその横）を除いた物を出口とし、角を抜けて移れる出口どうしは 1 本と数える。
    /// 1 本ならその中の向き（同じ向き → 上下左右 → 斜めの順。角を曲がる）、0 本か 2 本以上（行き止まり・分かれ道）なら止まる。
    def nextDir(ctx: Ctx, runState: Run.State, dir: Dir8): Option[Dir8] =
        let b = ctx#board;
        let at = Run.heroOf(runState)#at;
        let occupied = Run.occupancy(Run.enemiesOf(runState));
        let open = (c: Cell) -> Board.isWalkable(c, b) and Board.isCornerOpen(at, c, b);
        let free = (c: Cell) -> open(c) and not Map.memberOf(c, occupied) and not Map.memberOf(c, Run.seenTraps(Run.rec(runState)));
        if (Board.roomAt(at, b) != 0) Some(dir) |> Option.filter(d -> free(Board.step8(d, at)))
        else {
            let exits = Board.neighbors8(at) |> List.filter(c -> open(c) and not Board.isBehind(dir, at, c));
            if (List.length(groupsOf(b, exits)) != 1) None
            else preferred(dir, at, exits) |> Option.filter(d -> free(Board.step8(d, at)))
        }

    /// 出口を、角を抜けて隣どうし移れる物でつないだ塊に分ける。
    def groupsOf(b: Board.Board, exits: List[Cell]): List[Set[Cell]] =
        exits |> List.foldLeft((groups, c) -> {
            let (touching, apart) = List.partition(g -> Set.exists(o -> Board.isStepOpen(o, c, b), g), groups);
            Set.insert(c, List.foldLeft(Set.union, Set.empty(), touching)) :: apart
        }, Nil)

    /// 1 本の出口の中で進む向き: 同じ向き → 上下左右 → 斜め。
    def preferred(dir: Dir8, at: Cell, exits: List[Cell]): Option[Dir8] =
        let dirs = exits |> List.filterMap(c -> Board.dirTo(at, c));
        let orthogonal = Board.dirs() |> List.map(Board.toDir8);
        if (List.memberOf(dir, dirs)) Some(dir)
        else List.find(d -> List.memberOf(d, orthogonal), dirs) |> Option.withDefault(default = List.head(dirs))
}
