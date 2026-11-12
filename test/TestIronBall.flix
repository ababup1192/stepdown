// 鉄球: 小石とは別の系統で、直線の距離 arrowMin..range の起きた敵に投げる・当たると 1 マス押す（押した先の罠も動く）・
// 判定の順は 矢 → 鉄球 → 小石。盤は文字で書いた小さな盤（主人公は (1, 1)、^ は毒矢）。
mod TestIronBall {

    use Run.{Event, Input}
    use Preview.Mark
    use Fixtures.{awake, ctx, holding, start}

    def stats(kind: Item.Kind): ItemsDoc.Stats = ItemsDoc.statsOf(kind, ItemsDoc.defaults())

    def lane(): List[String] = "########" :: "#@.....#" :: "#.....>#" :: "########" :: Nil

    /// 2 マス先の起きたゼリー（HP 20）に鉄球が当たり、items の damage を与えて 1 マス奥へ押す。
    @Test
    def testIronBallHitsAndPushes(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 20}}}") | ctx(lane()) };
        let first = start(c, (Enemy.Kind.Jelly, (3, 1)) :: Nil, Nil) |> awake |> holding(Run.Held.Held(Item.Kind.IronBall, 2) :: Nil);
        let (_, events) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = (true, true),
            (List.memberOf(Event.ArrowHit(0, stats(Item.Kind.IronBall)#damage), events), List.memberOf(Event.EnemyPushed(0, (3, 1), (4, 1)), events)))

    /// 鉄球で押した先が毒矢なら、敵に trapDamage も入る。
    @Test
    def testIronBallPushesOntoADart(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 20}}}") | ctx("########" :: "#@..^..#" :: "#.....>#" :: "########" :: Nil) };
        let first = start(c, (Enemy.Kind.Jelly, (3, 1)) :: Nil, Nil) |> awake |> holding(Run.Held.Held(Item.Kind.IronBall, 2) :: Nil);
        let (_, events) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = true, List.memberOf(Event.EnemyTrapped(0, RulesDoc.rec(RulesDoc.defaults())#trapDamage), events))

    /// 判定の順: 矢と鉄球を持てば矢、鉄球と小石を持てば鉄球を投げる。
    @Test
    def testArrowBeforeIronBallBeforePebble(): Unit \ Assert =
        let c = ctx(lane());
        let foes = start(c, (Enemy.Kind.Jelly, (3, 1)) :: Nil, Nil) |> awake;
        let usedWith = (pack: Run.Pack) -> Preview.summarize(snd(Step.step(c, holding(pack, foes), Input.Wait))) |> List.drop(1) |> List.head;
        Assert.assertEq(expected = (Some(Mark.Arrow), Some(Mark.IronBall)),
            (usedWith(Run.Held.Held(Item.Kind.IronBall, 2) :: Run.Held.Held(Item.Kind.WoodArrow, 2) :: Nil),
             usedWith(Run.Held.Held(Item.Kind.Pebble, 2) :: Run.Held.Held(Item.Kind.IronBall, 2) :: Nil)))

    /// 鉄球は range より遠い敵には投げない（4 マス先のゼリーには何もしない）。
    @Test
    def testIronBallStopsAtItsRange(): Unit \ Assert =
        let c = ctx(lane());
        let first = start(c, (Enemy.Kind.Jelly, (5, 1)) :: Nil, Nil) |> awake |> holding(Run.Held.Held(Item.Kind.IronBall, 2) :: Nil);
        Assert.assertEq(expected = Mark.Wait :: Nil, Preview.summarize(snd(Step.step(c, first, Input.Wait))))
}
