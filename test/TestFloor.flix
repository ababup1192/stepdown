// 階の作り: assets の地図ごとに、階段まで歩いて着ける・始まりの近くに敵がいない・アイテムが床にある・同じ種から同じ階。
// 地図は assets/stepdown.floors.json を読む（遊ぶ物と同じ地図を見る）。種は区切りの良い数を並べる。
mod TestFloor {

    def docs(): Floor.Docs \ Fs.FileRead =
        let d = Controls.loadDocs();
        { rules = d#rules, items = d#items, enemies = d#enemies, cards = d#cards, floors = d#floors }

    def layouts(): Floor.Layouts \ Fs.FileRead = Floor.layoutsOf(FloorsDoc.load(FloorsDoc.path()))#layouts

    /// 1F の地図の番号。
    def layoutIds(): List[Int32] \ Fs.FileRead = List.range(0, Floor.layoutCount(1, layouts()))

    def seeds(): List[Int64] = 1i64 :: 2i64 :: 3i64 :: 10i64 :: 20i64 :: 50i64 :: 100i64 :: 1000i64 :: Nil

    /// 地図 layout を種 seed で作った 1F。
    def floorOn(layout: Int32, seed: Int64): Run.State \ Fs.FileRead =
        Floor.createOn(docs(), layouts(), { seed = seed, floor = 1, layout = layout }, seed)

    /// 1F の地図は 2 枚とも組めて、どちらも始まりから階段まで歩いて着ける。
    @Test
    def testEveryMapReachesTheStairs(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = layoutIds() |> List.map(_ -> true), layoutIds() |> List.map(layout -> Floor.reachesStairs(Floor.boardOf(layouts(), 1, layout))))

    /// 1F の地図は 2 枚とも、歩けるマスは全部、始まりから角を抜けずに歩いて着ける（斜めの角で閉じた所が無い）。
    @Test
    def testEveryWalkableCellIsReachable(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = (Nil, Nil),
            (Floor.unreachableCells(Floor.boardOf(layouts(), 1, 0)), Floor.unreachableCells(Floor.boardOf(layouts(), 1, 1))))

    /// どの地図・どの種でも、敵は始まりから enemyMinSteps 歩以上離れている。
    @Test
    def testNoEnemyNearTheStart(): Unit \ Assert + Fs.FileRead =
        let near = RulesDoc.rec(docs()#rules)#enemyMinSteps;
        Assert.assertEq(expected = Nil,
            layoutIds() |> List.flatMap(layout -> seeds() |> List.filter(seed ->
                Floor.nearestEnemySteps(Floor.boardOf(layouts(), 1, layout), floorOn(layout, seed)) |> Option.exists(steps -> steps < near))))

    /// どの地図・どの種でも、アイテムは床（階段でも始まりでもない）にある。
    @Test
    def testItemsLieOnTheFloor(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = Nil,
            layoutIds() |> List.flatMap(layout -> seeds() |> List.filter(seed ->
                not Floor.itemsOnFloor(Floor.boardOf(layouts(), 1, layout), floorOn(layout, seed)))))

    /// 階には floors の Doc の数だけ敵を置く。
    @Test
    def testFloorPlacesTheEnemyCountsFromTheDoc(): Unit \ Assert + Fs.FileRead =
        let made = Floor.create(docs(), layouts(), 1i64, 1);
        Assert.assertEq(expected = FloorsDoc.floorOf(1, docs()#floors) |> Option.map(f -> f#jelly + f#bat + f#archer + f#swarm),
            Some(List.length(Run.enemiesOf(made))))

    /// 床には items に加えて食べ物を置く: パンは breadOdds の重みの番号の数（[0, 0, 1] なら 2 個）、肉は meatChance（1 なら必ず 1 つ）。
    @Test
    def testFoodIsAddedToTheFixedItems(): Unit \ Assert + Fs.FileRead =
        let floors = Fixtures.floorsWith("{\"floors\": [{\"floor\": 1, \"maps\": [\"1a\"], \"items\": [\"Herb\"], \"breadOdds\": [0.0, 0.0, 1.0], \"meatChance\": 1.0}]}");
        let made = Floor.create({ floors = floors | docs() }, Floor.layoutsOf(floors)#layouts, 1i64, 1);
        Assert.assertEq(expected = (4, Set#{Item.Kind.Herb, Item.Kind.Bread, Item.Kind.Meat}),
            (Map.size(Run.rec(made)#items), Run.rec(made)#items |> Map.valuesOf |> List.toSet))

    /// 同じ種からは同じ階（地図・敵・アイテム）。
    @Test
    def testSameSeedMakesTheSameFloor(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = Run.fingerprint(Floor.create(docs(), layouts(), 42i64, 1)),
            Run.fingerprint(Floor.create(docs(), layouts(), 42i64, 1)))
}
