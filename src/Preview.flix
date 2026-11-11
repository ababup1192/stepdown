// Preview — 1 手の出来事の要約（summarize）と、方向の予告（preview = 本物の step を回して要約するだけ）と、敵の予告（dangerCells）。
// 予告専用の判定は書かない。敵の予告は「主人公の手の後にここにいたら当たるマス」で、当たる側に倒す（敵を倒した場合だけ外れる）。
mod Preview {

    use Board.Cell
    use Run.{Ctx, Event, Input}

    /// 要約の印。並びは summarize が出す順（手 → 使った物 → 受けるダメージ → 倒れる）。
    pub enum Mark with Eq, Order, ToString {
        case Move
        case Strike
        case Wait
        case Blocked
        case Descend
        case Arrow
        case Bomb
        case Potion
        case Damage(Int32)
        case Falls
    }

    /// 1 手の要約。
    pub type alias Summary = List[Mark]

    /// 出来事の列を「手（移動・殴る・待つ・×・降りる）・矢・爆弾・薬・受けるダメージ・倒れる」に要約する。
    /// 弾いた攻撃も盾で減らした後のダメージとして数える。
    pub def summarize(events: List[Event]): Summary =
        let damage = events |> List.map(damageOf) |> List.sum;
        let used = (mark, f) -> if (List.exists(f, events)) mark :: Nil else Nil;
        (actOf(events) :: Nil) ::: used(Mark.Arrow, isArrow) ::: used(Mark.Bomb, isBomb) ::: used(Mark.Potion, isPotion)
            ::: (if (damage > 0) Mark.Damage(damage) :: Nil else Nil) ::: used(Mark.Falls, e -> e == Event.HeroDied)

    def actOf(events: List[Event]): Mark =
        if (List.exists(isBlocked, events)) Mark.Blocked
        else if (List.memberOf(Event.Descended, events)) Mark.Descend
        else if (List.exists(isStrike, events)) Mark.Strike
        else if (List.exists(isMove, events)) Mark.Move
        else Mark.Wait

    def damageOf(e: Event): Int32 = match e {
        case Event.HeroHurt(_, amount, _) => amount
        case _ => 0
    }

    def isBlocked(e: Event): Bool = match e {
        case Event.HeroBlocked(_) => true
        case _ => false
    }

    def isStrike(e: Event): Bool = match e {
        case Event.HeroStruck(_, _) => true
        case _ => false
    }

    def isMove(e: Event): Bool = match e {
        case Event.HeroMoved(_, _) => true
        case _ => false
    }

    def isArrow(e: Event): Bool = match e {
        case Event.ArrowShot(_, _, _) => true
        case _ => false
    }

    def isBomb(e: Event): Bool = match e {
        case Event.BombThrown(_, _) => true
        case _ => false
    }

    def isPotion(e: Event): Bool = match e {
        case Event.PotionUsed(_, _) => true
        case _ => false
    }

    /// 5 つの入力（4 方向＋待つ）それぞれの要約（遊んでいない Run は空）。
    pub def preview(ctx: Ctx, runState: Run.State): List[(Input, Summary)] =
        if (Run.statusOf(runState) != Run.Status.Playing) Nil
        else Run.inputs() |> List.map(input -> (input, summarize(snd(Step.step(ctx, runState, input)))))

    /// 敵の予告: 主人公の手の後にそこへ立っていたら、この手で敵の攻撃が届きうるマス。
    /// 起きた敵は上下左右の隣と、弓兵なら射線（距離 arrowMin..arrowMax・矢を止めるマスまで。敵では止めない）。
    /// 眠っている敵は、そのマスに立つと気付かれる（起きる）所だけ。眠り玉で眠っている敵はこの手では動かないので入れない。
    pub def dangerCells(ctx: Ctx, runState: Run.State): Set[Cell] =
        Run.enemiesOf(runState) |> List.foldLeft((acc, e) -> Set.union(acc, threatOf(ctx, e)), Set.empty())

    def threatOf(ctx: Ctx, e: Enemy.Enemy): Set[Cell] = match e#rest {
        case Enemy.Rest.Awake     => reachOf(ctx, e)
        case Enemy.Rest.Dormant   => reachOf(ctx, e) |> Set.filter(c -> Run.notices(ctx, Board.walkDistances(c, ctx#board), c, e))
        case Enemy.Rest.Dozing(_) => Set.empty()
    }

    /// e が今の位置から攻撃を届かせるマス。
    def reachOf(ctx: Ctx, e: Enemy.Enemy): Set[Cell] =
        let own = Enemy.cellsOf(e);
        let beside = own |> Set.toList |> List.flatMap(Board.neighbors4) |> List.toSet |> Set.filter(c -> not Set.memberOf(c, own));
        if (Enemy.behaviourOf(e#kind) == Enemy.Behaviour.Shoot) Set.union(beside, lineOfFire(ctx, e#at)) else beside

    def lineOfFire(ctx: Ctx, origin: Cell): Set[Cell] =
        Board.dirs() |> List.flatMap(dir -> laneOf(ctx, dir, origin)) |> List.toSet

    /// origin から dir へ、矢が届くマス（距離 arrowMin..arrowMax・矢を止めるマスまで）。
    def laneOf(ctx: Ctx, dir: Board.Dir, origin: Cell): List[Cell] =
        let rules = RulesDoc.rec(ctx#rules);
        List.range(1, rules#arrowMax + 1)
            |> List.map(k -> (k, Board.ahead(dir, k, origin)))
            |> List.takeWhile(e -> not Board.stopsArrow(snd(e), ctx#board))
            |> List.filter(e -> fst(e) >= rules#arrowMin)
            |> List.map(snd)

    /// 起きている弓兵の射線: (弓兵のマス, 射線の一番遠いマス) の列（見せる側が床に破線を引く）。
    pub def firingLanes(ctx: Ctx, runState: Run.State): List[(Cell, Cell)] =
        Run.enemiesOf(runState)
            |> List.filter(e -> Enemy.isAwake(e) and Enemy.behaviourOf(e#kind) == Enemy.Behaviour.Shoot)
            |> List.flatMap(e -> Board.dirs() |> List.filterMap(dir -> List.last(laneOf(ctx, dir, e#at)) |> Option.map(far -> (e#at, far))))
}
