// ダッシュ: 敵が見えたら止まる・分かれ道で止まる・一本道の角は曲がる・敵が見えている時は 1 歩・捨てるカードが立ったら止まる・赤の手前で止まる。
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

    /// 斜めにつながる出口どうしは 1 本と数える: (5, 1) の先の (6, 1)・(6, 2)・(5, 2) は 1 本で、同じ向きの (6, 1) へ進む。
    /// その先は角を曲がって (6, 2) へ下り、階段 (6, 3) と袋の (5, 2) は角の壁で隔たる 2 本なので止まる。
    @Test
    def testAdjacentExitsCountAsOnePath(): Unit \ Assert =
        let c = ctx("########" :: "#@,,,,,#" :: "#####,,#" :: "######>#" :: "########" :: Nil);
        Assert.assertEq(expected = Some((6, 2)), lastCell(dashFromCorridor(c, start(c, Nil, Nil))))

    /// 始めた時に起きた敵が見えていれば 1 歩だけ。
    @Test
    def testOneStepWhenAnEnemyIsInSight(): Unit \ Assert =
        let c = ctx(Fixtures.hall());
        let (woken, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (7, 1)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = 1, List.length(Dash.dash(c, woken, Dir.Up)))

    /// 枠が埋まった後に 4 種類目へ乗った歩で止まる（3 歩で 3 つ拾い、通路の (7, 1) の小石で止まる）。
    @Test
    def testStopsWhenACardIsPending(): Unit \ Assert =
        let c = ctx("############" :: "#@,,,,,,,,>#" :: "############" :: Nil);
        let placed = (Item.Kind.WoodArrow, (2, 1)) :: (Item.Kind.SmallBomb, (3, 1)) :: (Item.Kind.Herb, (4, 1)) :: (Item.Kind.Pebble, (7, 1)) :: Nil;
        let (ready, _) = Step.steps(c, start(c, Nil, placed), Input.Move(Dir8.Right) :: Input.Move(Dir8.Right) :: Input.Move(Dir8.Right) :: Nil);
        let steps = Dash.dash(c, ready, Dir.Right);
        Assert.assertEq(expected = (Some((7, 1)), Some(Run.Choice.Swap(Item.Kind.Pebble) :: Nil)),
            (lastCell(steps), List.last(steps) |> Option.map(e -> Run.pendingOf(fst(e)))))

    /// 部屋の壁際を歩くと、横の壁に開いた通路の口の隣で止まる（(1, 3) から上へ。口は (0, 2)。壁まで行かずに (1, 2) で止まる）。
    @Test
    def testStopsBesideACorridorOpeningAlongAWall(): Unit \ Assert =
        let c = ctx("#######" :: "#.....#" :: ",.....#" :: "#@....#" :: "#....>#" :: "#######" :: Nil);
        Assert.assertEq(expected = Some((1, 2)), lastCell(Dash.dash(c, start(c, Nil, Nil), Dir.Up)))

    /// 口の隣から歩き出した時は、その口では止まらない（同じ口の横に居続けないので、次の壁で止まる）。
    @Test
    def testDoesNotStopAtTheOpeningItStartsBeside(): Unit \ Assert =
        let c = ctx("#######" :: "#.....#" :: "#.....#" :: ",@....#" :: "#....>#" :: "#######" :: Nil);
        Assert.assertEq(expected = Some((1, 1)), lastCell(Dash.dash(c, start(c, Nil, Nil), Dir.Up)))

    // ══════════════════════════════════════════════════════
    // 赤で止まる
    // ══════════════════════════════════════════════════════

    /// 気付かれない遠さの起きた弓兵の射線が通路を横切っていれば、射線の手前のマスで止まり、撃たれない（見える距離 1 の rules）。
    @Test
    def testStopsBeforeTheRed(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"wakeSteps\": 1}"),
            "#########" :: "#@,,,,,>#" :: "####.####" :: "####.####" :: "####.####" :: "####.####" :: "#########" :: Nil);
        let steps = dashFromCorridor(c, Fixtures.awake(start(c, (Enemy.Kind.Archer, (4, 5)) :: Nil, Nil)));
        Assert.assertEq(expected = (Some((3, 1)), false), (lastCell(steps), List.exists(e -> List.exists(Run.hurtsHero, snd(e)), steps)))

    /// 乱数の局面 seed で 4 方向へダッシュし、2 歩目から後の歩で敵の攻撃を受けたのに、その歩の前の赤に立ったマスが入っていなかった歩。
    def hitsOutsideTheRedWhileDashing(seed: Int64): List[String] =
        let (c, first) = Fixtures.randomCase(seed);
        Board.dirs() |> List.flatMap(dir -> {
            let steps = Dash.dash(c, first, dir);
            List.zip(steps, List.drop(1, steps)) |> List.filterMap(match ((before, _), (after, events)) -> {
                let at = Run.heroOf(after)#at;
                if (not List.exists(Run.hurtsHero, events) or Set.memberOf(at, Preview.dangerCells(c, before))) None
                else Some("種 ${seed}・${dir} で ${at}: ${Run.fingerprint(before)}")
            })
        })

    /// 乱数の局面 300 個 × 4 方向のダッシュで、途中の歩（2 歩目から）の被ダメージは、その歩の前の赤の中だけ。
    @Test
    def testDashIsHurtOnlyInsideTheRed(): Unit \ Assert =
        Assert.assertEq(expected = Nil, List.range(1, 301) |> List.flatMap(seed -> hitsOutsideTheRedWhileDashing(Int32.toInt64(seed))))
}

