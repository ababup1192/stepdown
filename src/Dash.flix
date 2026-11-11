// Dash — ダッシュ: 始めた時に最後まで計算する（1 歩ずつ同じ Step.step を通す）。見せる側は列を 1 歩ずつ流す。
// 止まる（その歩で止まる）: 敵が見えた・拾った・階段に乗った・次が歩けない・分かれ道・部屋の番号が変わった・HP が減った・殴った・矢か爆弾を使った。
// 一本道の通路の角は自動で曲がる。始めた時に敵が見えていれば 1 歩だけ。
mod Dash {

    use Board.Dir
    use Run.{Ctx, Event, Input}

    /// 1 歩ずつの (その歩の後の Run, 出来事) の列。最初の歩が進めなければ、その 1 歩（HeroBlocked）だけ。
    pub def dash(ctx: Ctx, runState: Run.State, dir: Dir): List[(Run.State, List[Event])] =
        if (Run.seesAwakeEnemy(ctx, runState)) Step.step(ctx, runState, Input.Move(Board.toDir8(dir))) :: Nil
        else stride(ctx, runState, dir, ctx#board#cols * ctx#board#rows)

    def stride(ctx: Ctx, runState: Run.State, dir: Dir, budget: Int32): List[(Run.State, List[Event])] =
        let (next, events) = Step.step(ctx, runState, Input.Move(Board.toDir8(dir)));
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

    /// 次の歩の向き。部屋の中はまっすぐ進めるなら同じ向き。通路は来た向きを除いて行ける道が 1 本ならその向き（角を曲がる）、
    /// 0 本か 2 本以上（行き止まり・分かれ道）なら止まる。
    def nextDir(ctx: Ctx, runState: Run.State, dir: Dir): Option[Dir] =
        let at = Run.heroOf(runState)#at;
        let occupied = Run.occupancy(Run.enemiesOf(runState));
        let open = d -> { let c = Board.step(d, at); Board.isWalkable(c, ctx#board) and not Map.memberOf(c, occupied) };
        if (Board.roomAt(at, ctx#board) != 0) (if (open(dir)) Some(dir) else None)
        else {
            let exits = Board.dirs() |> List.filter(d -> d != Board.opposite(dir) and Board.isWalkable(Board.step(d, at), ctx#board));
            match exits {
                case d :: Nil if open(d) => Some(d)
                case _                   => None
            }
        }
}
