// Preview — 1 手の出来事の要約（summarize）と、方向の予告（preview = 本物の step を回して要約するだけ）と、敵の予告（dangerCells）。
// 予告専用の判定は書かない（敵の攻撃が届くかは Run.attackOn）。敵の予告は「主人公の手の後にここにいたら当たるマス」で、当たる側に倒す（敵を倒した場合だけ外れる）。
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
        case Trap(Trap.Kind)
        case Arrow
        case Pebble
        case IronBall
        case Bomb
        case Smoke
        case Potion
        case Damage(Int32)
        case Falls
    }

    /// 1 手の要約。
    pub type alias Summary = List[Mark]

    /// 出来事の列を「手（移動・殴る・待つ・×・降りる）・踏んだ罠・矢・鉄球・小石・爆弾・煙玉・薬・受けるダメージ・倒れる」に要約する。
    /// 弾いた攻撃も盾で減らした後のダメージとして数える。
    pub def summarize(events: List[Event]): Summary =
        let damage = events |> List.map(damageOf) |> List.sum;
        let used = (mark, f) -> if (List.exists(f, events)) mark :: Nil else Nil;
        (actOf(events) :: Nil) ::: trapsOf(events) ::: used(Mark.Arrow, isThrown(Item.Family.Arrow)) ::: used(Mark.IronBall, isThrown(Item.Family.IronBall)) ::: used(Mark.Pebble, isThrown(Item.Family.Pebble))
            ::: used(Mark.Bomb, isBomb) ::: used(Mark.Smoke, isSmoke) ::: used(Mark.Potion, isPotion)
            ::: (if (damage > 0) Mark.Damage(damage) :: Nil else Nil) ::: used(Mark.Falls, e -> e == Event.HeroDied)

    def actOf(events: List[Event]): Mark =
        if (List.exists(isBlocked, events)) Mark.Blocked
        else if (List.memberOf(Event.Descended, events)) Mark.Descend
        else if (List.exists(isStrike, events)) Mark.Strike
        else if (List.exists(isMove, events)) Mark.Move
        else Mark.Wait

    /// 主人公が踏んだ罠（主人公が入ったマスの罠だけ。敵が踏んだ罠は入れない）。
    def trapsOf(events: List[Event]): List[Mark] =
        let dest = events |> List.findMap(e -> match e {
            case Event.HeroMoved(_, to) => Some(to)
            case _                      => None
        });
        events |> List.filterMap(e -> match e {
            case Event.TrapSprung(kind, cell) if Some(cell) == dest => Some(Mark.Trap(kind))
            case _                                                => None
        })

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

    def isThrown(family: Item.Family, e: Event): Bool = match e {
        case Event.ArrowShot(_, kind, _) => Item.familyOf(kind) == family
        case _ => false
    }

    def isSmoke(e: Event): Bool = match e {
        case Event.SmokeThrown(_) => true
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

    /// 9 つの入力（8 方向＋待つ）それぞれの要約（遊んでいない・選ぶ場面がある Run は空）。見えていない罠は無い物として回す。
    pub def preview(ctx: Ctx, runState: Run.State): List[(Input, Summary)] =
        let seen = Run.asSeen(runState);
        if (Run.statusOf(runState) != Run.Status.Playing or not List.isEmpty(Run.pendingOf(runState))) Nil
        else Run.inputs() |> List.map(input -> (input, summarize(snd(Step.step(ctx, seen, input)))))

    /// 敵の予告: 主人公の手の後にそこへ立っていたら、この手で敵の攻撃が届きうるマス。
    /// 起きた敵は 8 近傍の隣（角で遮られるマスは除く）と、弓兵なら射線（距離 arrowMin..arrowMax・矢を止めるマスまで。敵では止めない）。
    /// 眠っている敵は、そのマスに立つと気付かれる（起きる）所だけ。Dozing の敵（眠り玉・当たって起きる途中）はこの手では動かないので入れない。
    /// 選ぶ場面がある間は空（手が進まない）。
    pub def dangerCells(ctx: Ctx, runState: Run.State): Set[Cell] =
        if (not List.isEmpty(Run.pendingOf(runState))) Set.empty()
        else Run.enemiesOf(runState) |> List.foldLeft((acc, e) -> Set.union(acc, threatOf(ctx, e)), Set.empty())

    def threatOf(ctx: Ctx, e: Enemy.Enemy): Set[Cell] = match e#rest {
        case Enemy.Rest.Awake     => reachOf(ctx, e)
        case Enemy.Rest.Dormant   => reachOf(ctx, e) |> Set.filter(c -> Run.notices(ctx, Board.walkDistances(c, ctx#board), c, e))
        case Enemy.Rest.Dozing(_) => Set.empty()
    }

    /// e が今の位置から攻撃を届かせるマス（Run.attackOn が届くと言うマス。矢は敵では止めない）。
    def reachOf(ctx: Ctx, e: Enemy.Enemy): Set[Cell] =
        let far = Int32.max(1, RulesDoc.rec(ctx#rules)#arrowMax);
        let (x, y) = e#at;
        List.range(x - far, x + e#size + far) |> List.flatMap(cx -> List.range(y - far, y + e#size + far) |> List.map(cy -> (cx, cy)))
            |> List.filter(c -> not Set.memberOf(c, Enemy.cellsOf(e)) and Option.nonEmpty(Run.attackOn(ctx, Set.empty(), e, c)))
            |> List.toSet

    def laneOf(ctx: Ctx, dir: Board.Dir, origin: Cell): List[Cell] = Board.lane(RulesDoc.reachOf(ctx#rules), dir, origin, ctx#board)

    /// 起きている弓兵の射線: (弓兵のマス, 射線の一番遠いマス) の列（見せる側が床に破線を引く）。
    pub def firingLanes(ctx: Ctx, runState: Run.State): List[(Cell, Cell)] =
        Run.enemiesOf(runState)
            |> List.filter(e -> Enemy.isAwake(e) and Enemy.behaviourOf(e#kind) == Enemy.Behaviour.Shoot)
            |> List.flatMap(e -> Board.dirs() |> List.filterMap(dir -> List.last(laneOf(ctx, dir, e#at)) |> Option.map(far -> (e#at, far))))
}
