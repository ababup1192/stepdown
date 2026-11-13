// ボスが仲間を呼ぶ: 主人公が体の隣にいない手番が summonEvery 続くと 1 体呼ぶ・隣にいれば湧かない（隣に来たら数え直して予告も消える）・
// 呼んだ仲間が summonMax 体いれば呼ばない・呼ぶ 1 手前に予告があり、予告のマスと湧いたマスが同じ・空きマスが無ければ湧かない・
// 大技を溜める手番には呼ばず、予告は次の溜めていない手番へ持ち越す。
mod TestSummon {

    use Board.Cell
    use Run.{Event, Input}
    use Fixtures.{start, withEnemy}

    /// 左の広間にボス（(2, 2) を左上）、主人公は右へ延びる 1 マス幅の通路の奥 (9, 2)。2×2 の体は通路に入れず、隣には来られない。
    def corridorRows(): List[String] =
        "###########" ::
        "#.....#####" ::
        "#.....,,,@#" ::
        "#.....#####" ::
        "#>....#####" ::
        "###########" :: Nil

    /// ボスの体の周りが全部壁（主人公は別の部屋）。
    def walledRows(): List[String] =
        "#######" ::
        "##..###" ::
        "##..###" ::
        "#######" ::
        "#@...>#" ::
        "#######" :: Nil

    def every(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Boss, EnemiesDoc.defaults())#summonEvery

    /// 起きたボス（大技は溜めない）と、HP の多い主人公。
    def bossAt(ctx: Run.Ctx, at: Cell): Run.State =
        let r = Run.rec(withEnemy(0, e -> { cooldown = 999 | e }, Fixtures.awake(start(ctx, (Enemy.Kind.Boss, at) :: Nil, Nil))));
        Run.of({ hero = { hp = 999, maxHp = 999 | r#hero } | r })

    def corridor(): Run.Ctx = Fixtures.ctx(corridorRows())

    def waits(n: Int32): List[Input] = List.repeat(n, Input.Wait)

    def summonedAlive(runState: Run.State): Int32 = Run.summonedAlive(Run.enemiesOf(runState))

    def marksOf(runState: Run.State): List[Cell] = Run.summonMarks(Run.enemiesOf(runState))

    def boss(runState: Run.State): Option[Enemy.Enemy] = Run.enemiesOf(runState) |> List.head

    /// 群れを (6, 2) へ呼んだ出来事（ボスは id 0・呼んだ群れは次の id 1）。
    def summonedAtMouth(): Event = Event.EnemySummoned(0, 1, Enemy.Kind.Swarm, (6, 2))

    /// 隣にいない手番が summonEvery 続いた手に、通路の口（主人公に近い側）へ群れが 1 体湧く（ボスに呼ばれた敵として数える）。
    @Test
    def testAwayTurnsSummonOne(): Unit \ Assert =
        let (after, events) = Step.steps(corridor(), bossAt(corridor(), (2, 2)), waits(every()));
        Assert.assertEq(expected = (true, 1), (List.memberOf(summonedAtMouth(), events), summonedAlive(after)))

    /// 呼ばれた群れは起きている（次の手番から動く）。
    @Test
    def testSummonedFoeIsAwake(): Unit \ Assert =
        let (after, _) = Step.steps(corridor(), bossAt(corridor(), (2, 2)), waits(every()));
        Assert.assertEq(expected = Some(Enemy.Rest.Awake), Run.enemiesOf(after) |> List.last |> Option.map(e -> e#rest))

    /// summonEvery より 1 手少ない間は湧かない。
    @Test
    def testFewerAwayTurnsSummonNone(): Unit \ Assert =
        let (after, _) = Step.steps(corridor(), bossAt(corridor(), (2, 2)), waits(every() - 1));
        Assert.assertEq(expected = 0, summonedAlive(after))

    /// 呼ぶ 1 手前に予告のマス (6, 2) が出て、次の手番に湧くマスは予告と同じ。
    @Test
    def testMarkComesOneTurnBeforeAtTheSameCell(): Unit \ Assert =
        let (marked, markEvents) = Step.steps(corridor(), bossAt(corridor(), (2, 2)), waits(every() - 1));
        let (_, events) = Step.step(corridor(), marked, Input.Wait);
        Assert.assertEq(expected = ((6, 2) :: Nil, true, true),
            (marksOf(marked), List.memberOf(Event.SummonMarked(0, (6, 2)), markEvents), List.memberOf(summonedAtMouth(), events)))

    /// 体の隣で殴り合っている間は湧かない（数えは 0 のまま）。
    @Test
    def testBesideHeroSummonsNone(): Unit \ Assert =
        let c = corridor();
        let r = Run.rec(bossAt(c, (2, 2)));
        let (after, _) = Step.steps(c, Run.of({ hero = { at = (4, 2) | r#hero } | r }), waits(every() * 2));
        Assert.assertEq(expected = (0, Some(0)), (summonedAlive(after), boss(after) |> Option.map(e -> e#away)))

    /// 予告の出た次の手番に主人公が隣にいれば、呼ばずに予告を消して数え直す。
    @Test
    def testStepInCancelsTheMark(): Unit \ Assert =
        let c = corridor();
        let r = Run.rec(withEnemy(0, e -> { away = every() - 1, summonAt = Some((1, 1)) | e }, bossAt(c, (2, 2))));
        let (after, _) = Step.step(c, Run.of({ hero = { at = (4, 2) | r#hero } | r }), Input.Wait);
        Assert.assertEq(expected = (0, Some((0, None))), (summonedAlive(after), boss(after) |> Option.map(e -> (e#away, e#summonAt))))

    /// 呼んだ仲間が summonMax 体生きている間は呼ばない（summonMax 1 で、湧いた群れが生きている間に 3 回分待っても、作った敵は 1 体だけ）。
    @Test
    def testSummonMaxStops(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Boss\": {\"summonMax\": 1}}}") | corridor() };
        let (after, _) = Step.steps(c, bossAt(c, (2, 2)), waits(every() * 3));
        Assert.assertEq(expected = (1, 2), (summonedAlive(after), Run.rec(after)#nextId))

    /// 体の隣に空いた歩けるマスが無ければ、予告も出ず湧かない。
    @Test
    def testNoRoomSummonsNone(): Unit \ Assert =
        let c = Fixtures.ctx(walledRows());
        let (after, _) = Step.steps(c, bossAt(c, (2, 1)), waits(every() * 2));
        Assert.assertEq(expected = (0, Nil), (summonedAlive(after), marksOf(after)))

    /// 予告の印のマスに敵が入っていれば、そこには呼ばない（別のマスにも湧かせない）。
    @Test
    def testBlockedMarkSummonsNothingThere(): Unit \ Assert =
        let c = corridor();
        let r = Run.rec(withEnemy(0, e -> { away = every() - 1, summonAt = Some((1, 1)) | e }, bossAt(c, (2, 2))));
        let blocker = { rest = Enemy.Rest.Dozing(9) | Enemy.make(1, Enemy.Kind.Jelly, (1, 1), 4) };
        let (after, _) = Step.step(c, Run.of({ enemies = r#enemies ::: (blocker :: Nil), nextId = 2 | r }), Input.Wait);
        Assert.assertEq(expected = (0, 2), (summonedAlive(after), Run.rec(after)#nextId))

    /// 大技を溜める手番には呼ばず、予告はそのまま残る。
    @Test
    def testWindupTurnKeepsTheMark(): Unit \ Assert =
        let c = corridor();
        let first = withEnemy(0, e -> { away = every() - 1, summonAt = Some((1, 1)), cooldown = 0 | e }, bossAt(c, (2, 2)));
        let (after, _) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = (0, (1, 1) :: Nil), (summonedAlive(after), marksOf(after)))

    /// 予告の印は大技の赤と別: 呼ぶ予告が出ている局面の赤は、予告の無い同じ局面の赤と同じ。
    @Test
    def testMarkDoesNotChangeTheRed(): Unit \ Assert =
        let c = corridor();
        let plain = bossAt(c, (2, 2));
        let marked = withEnemy(0, e -> { summonAt = Some((1, 1)) | e }, plain);
        Assert.assertEq(expected = Preview.dangerCells(c, plain), Preview.dangerCells(c, marked))
}
