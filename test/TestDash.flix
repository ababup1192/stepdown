// ダッシュ: 敵が見えたら止まる・分かれ道で止まる・一本道の角は曲がる・敵が見えている時は 1 歩・矢を使ったら止まる。
// どれも部屋から通路へ 1 歩出てから始める（部屋の番号が変わる歩では止まるため）。
mod TestDash {

    use Board.{Dir, Dir8}
    use Run.Input
    use Preview.Mark
    use Fixtures.{ctx, start}

    /// 1 歩右へ出てから右へダッシュした列。
    def dashFromCorridor(c: Run.Ctx, first: Run.State): List[(Run.State, List[Run.Event])] =
        Dash.dash(c, fst(Step.step(c, first, Input.Move(Dir8.Right))), Dir.Right)

    def lastCell(steps: List[(Run.State, List[Run.Event])]): Option[Board.Cell] =
        List.last(steps) |> Option.map(e -> Run.heroOf(fst(e))#at)

    /// 通路の先の眠った敵に、歩く距離 wakeSteps まで近づいた歩で止まる。
    @Test
    def testStopsWhenAnEnemyComesIntoSight(): Unit \ Assert =
        let c = ctx("#################" :: "#@,,,,,,,,,,,,,>#" :: "#################" :: Nil);
        let steps = dashFromCorridor(c, start(c, (Enemy.Kind.Jelly, (13, 1)) :: Nil, Nil));
        let stopAt = 13 - RulesDoc.rec(RulesDoc.defaults())#wakeSteps;
        Assert.assertEq(expected = (stopAt - 2, Some((stopAt, 1))), (List.length(steps), lastCell(steps)))

    /// 分かれ道で止まる。
    @Test
    def testStopsAtAFork(): Unit \ Assert =
        let c = ctx("#########" :: "#@,,,,,>#" :: "####,####" :: "####,####" :: "#########" :: Nil);
        Assert.assertEq(expected = Some((4, 1)), lastCell(dashFromCorridor(c, start(c, Nil, Nil))))

    /// 一本道の角は曲がり、階段に乗って止まる。
    @Test
    def testTurnsCornersAndStopsOnStairs(): Unit \ Assert =
        let c = ctx("#######" :: "#@,,,##" :: "####,##" :: "####,>#" :: "#######" :: Nil);
        Assert.assertEq(expected = Some((5, 3)), lastCell(dashFromCorridor(c, start(c, Nil, Nil))))

    /// 始めた時に起きた敵が見えていれば 1 歩だけ。
    @Test
    def testOneStepWhenAnEnemyIsInSight(): Unit \ Assert =
        let c = ctx(Fixtures.hall());
        let (woken, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (7, 1)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = 1, List.length(Dash.dash(c, woken, Dir.Up)))

    /// 矢を使った歩で止まる（見える距離 0 で、敵に気付かれずに射程へ入る）。
    @Test
    def testStopsAfterShooting(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"wakeSteps\": 0}"), "############" :: "#@,,,,,,,,>#" :: "############" :: Nil);
        let steps = dashFromCorridor(c, start(c, (Enemy.Kind.Jelly, (8, 1)) :: Nil, (Item.Kind.WoodArrow, (2, 1)) :: Nil));
        Assert.assertEq(expected = (Some((4, 1)), Some(Mark.Move :: Mark.Arrow :: Nil)),
            (lastCell(steps), List.last(steps) |> Option.map(e -> Preview.summarize(snd(e)))))
}
