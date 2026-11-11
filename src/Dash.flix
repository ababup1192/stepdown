// Dash — ダッシュ: 始めた時に最後まで計算する（1 歩ずつ同じ Step.step を通す）。見せる側は列を 1 歩ずつ流す。
// 止まる（その歩で止まる）: 敵が見えた・拾った・階段に乗った・次が歩けない・分かれ道・部屋の番号が変わった・HP が減った・殴った・矢か爆弾を使った。
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
        let turn = if (budget <= 1 or stopsAfter(ctx, runState, next, events)) None else nextDir(ctx, next, dir);
        match turn {
            case Some(d) => (next, events) :: stride(ctx, next, d, budget - 1)
            case None    => (next, events) :: Nil
        }

    /// この歩で止まるか。
    def stopsAfter(ctx: Ctx, before: Run.State, after: Run.State, events: List[Event]): Bool =
        let (was, now) = (Run.heroOf(before), Run.heroOf(after));
        Run.statusOf(after) != Run.Status.Playing
            or now#hp < was#hp
            or List.exists(isStop, events)
            or Board.tileAt(now#at, ctx#board) == Board.Tile.Stairs
            or Board.roomAt(now#at, ctx#board) != Board.roomAt(was#at, ctx#board)
            or Run.seesAwakeEnemy(ctx, after)

    def isStop(e: Event): Bool = match e {
        case Event.Picked(_, _)       => true
        case Event.ArrowShot(_, _, _) => true
        case Event.BombThrown(_, _)   => true
        case Event.HeroBlocked(_)     => true
        case Event.HeroStruck(_, _)   => true
        case Event.HeroHurt(_, _, _)  => true
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
        let free = (c: Cell) -> open(c) and not Map.memberOf(c, occupied);
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
