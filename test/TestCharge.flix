// 突進: 上下左右で列がそろい間が空くと溜めて（その手は動かず当てない）、次の手番にまっすぐ走って当たる・道の外なら 0・
// 壁で止まると眠る・押されると溜めが消える・主人公の盾は突進にも効く・赤は走る道と同じ。盤は既定の広間（主人公 (1, 3)）。
mod TestCharge {

    use Board.{Dir, Dir8}
    use Run.{Event, Input, Source}
    use Fixtures.{ctx, start, hall, withEnemy}

    def chargerAttack(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Charger, EnemiesDoc.defaults())#attack

    def maxHp(): Int32 = RulesDoc.rec(RulesDoc.defaults())#maxHp

    /// 右 4 マスの起きた突進（列がそろっている）。
    def lined(c: Run.Ctx): Run.State = Fixtures.awake(start(c, (Enemy.Kind.Charger, (5, 3)) :: Nil, Nil))

    def firstOf(runState: Run.State): Option[Enemy.Enemy] = Run.enemiesOf(runState) |> List.head

    /// 列がそろうと溜める: その手は動かず、主人公の HP は減らない。
    @Test
    def testWindsUpWithoutMoving(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, lined(c), Input.Wait);
        Assert.assertEq(expected = (true, Some((5, 3)), maxHp(), Some(Enemy.Intent.Charge(Dir.Left))),
            (List.memberOf(Event.EnemyWindup(0, Enemy.Intent.Charge(Dir.Left)), events), firstOf(after) |> Option.map(e -> e#at),
             Run.heroOf(after)#hp, firstOf(after) |> Option.map(e -> e#intent)))

    /// 溜めた次の手に道の上で待つと、主人公の手前まで走って当たる（溜めは消える）。
    @Test
    def testChargeHitsOnTheLane(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.steps(c, lined(c), Input.Wait :: Input.Wait :: Nil);
        Assert.assertEq(expected = (Some((2, 3)), maxHp() - chargerAttack(), true, Some(Enemy.Intent.Idle)),
            (firstOf(after) |> Option.map(e -> e#at), Run.heroOf(after)#hp,
             List.memberOf(Event.HeroHurt(Source.Charge(0), chargerAttack(), false), events), firstOf(after) |> Option.map(e -> e#intent)))

    /// 溜めた次の手に道から外れると当たらず、突進は壁まで走って眠る。
    @Test
    def testSidestepMissesAndTheWallStuns(): Unit \ Assert =
        let c = ctx(hall());
        let (after, _) = Step.steps(c, lined(c), Input.Wait :: Input.Move(Dir8.Up) :: Nil);
        Assert.assertEq(expected = (maxHp(), Some((1, 3)), Some(Enemy.Rest.Dozing(1))),
            (Run.heroOf(after)#hp, firstOf(after) |> Option.map(e -> e#at), firstOf(after) |> Option.map(e -> e#rest)))

    /// 溜めている手は、隣にいても殴らない（上へ溜めた突進の左隣で待っても 0）。
    @Test
    def testChargingDoesNotStrikeBeside(): Unit \ Assert =
        let c = ctx(hall());
        let first = withEnemy(0, e -> { intent = Enemy.Intent.Charge(Dir.Up) | e }, Fixtures.awake(start(c, (Enemy.Kind.Charger, (2, 3)) :: Nil, Nil)));
        let (after, events) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = (false, Some((2, 1))), (List.exists(Run.hurtsHero, events), firstOf(after) |> Option.map(e -> e#at)))

    /// 銀の剣で押すと溜めが消え、その手は走らない（押された先で列がそろえば溜め直す）。
    @Test
    def testPushCancelsTheCharge(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Charger\": {\"hp\": 20}}}") | ctx(hall()) };
        let r = Run.rec(withEnemy(0, e -> { intent = Enemy.Intent.Charge(Dir.Up) | e }, Fixtures.awake(start(c, (Enemy.Kind.Charger, (2, 3)) :: Nil, Nil))));
        let armed = Run.of({ hero = { sword = Some(Item.Kind.SilverSword) | r#hero } | r });
        let (after, events) = Step.step(c, armed, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (Some((3, 3)), true, maxHp()),
            (firstOf(after) |> Option.map(e -> e#at), List.memberOf(Event.EnemyPushed(0, (2, 3), (3, 3)), events), Run.heroOf(after)#hp))

    /// 主人公の盾は突進にも効く（鉄の盾の守りの分だけ減る）。
    @Test
    def testShieldReducesTheCharge(): Unit \ Assert =
        let c = ctx(hall());
        let r = Run.rec(lined(c));
        let shielded = Run.of({ hero = { shield = Some(Item.Kind.IronShield) | r#hero } | r });
        let (after, _) = Step.steps(c, shielded, Input.Wait :: Input.Wait :: Nil);
        let guard = ItemsDoc.statsOf(Item.Kind.IronShield, ItemsDoc.defaults())#guard;
        Assert.assertEq(expected = maxHp() - (chargerAttack() - guard), Run.heroOf(after)#hp)

    /// 赤は走る道と同じ: 左へ溜めた突進の赤は、左の 4 マス（壁の手前まで）。
    @Test
    def testRedIsTheLane(): Unit \ Assert =
        let c = ctx(hall());
        let (charging, _) = Step.step(c, lined(c), Input.Wait);
        Assert.assertEq(expected = Set#{(4, 3), (3, 3), (2, 3), (1, 3)}, Preview.dangerCells(c, charging))
}
