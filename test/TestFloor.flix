// 階の作り: assets の地図ごとに、階段まで罠を踏まずに歩いて着ける・始まりの近くに敵がいない・アイテムが床（字の候補）にある・同じ種から同じ階。
// 降りる: 持ち越す物と捨てる物。地図は assets/stepdown.floors.json を読む（遊ぶ物と同じ地図を見る）。種は区切りの良い数を並べる。
mod TestFloor {

    def docs(): Floor.Docs \ Fs.FileRead =
        let d = Controls.loadDocs();
        { rules = d#rules, items = d#items, enemies = d#enemies, cards = d#cards, floors = d#floors }

    def layouts(): Floor.Layouts \ Fs.FileRead = Floor.layoutsOf(FloorsDoc.load(FloorsDoc.path()))#layouts

    /// 全部の地図の (階, 地図の番号)。
    def mapIds(): List[(Int32, Int32)] \ Fs.FileRead = Floor.mapIds(layouts())

    def seeds(): List[Int64] = 1i64 :: 2i64 :: 3i64 :: 10i64 :: 20i64 :: 50i64 :: 100i64 :: 1000i64 :: Nil

    def boardAt(id: (Int32, Int32)): Board.Board \ Fs.FileRead = Floor.boardOf(layouts(), fst(id), snd(id))

    /// 地図 id を種 seed で作った階。
    def floorOn(id: (Int32, Int32), seed: Int64): Run.State \ Fs.FileRead =
        Floor.createOn(docs(), layouts(), { seed = seed, floor = fst(id), layout = snd(id) }, seed)

    // ══════════════════════════════════════════════════════
    // 地図
    // ══════════════════════════════════════════════════════

    /// 1F〜4F の地図は 2 枚ずつ組める。
    @Test
    def testEveryFloorHasTwoMaps(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = (1, 0) :: (1, 1) :: (2, 0) :: (2, 1) :: (3, 0) :: (3, 1) :: (4, 0) :: (4, 1) :: Nil, mapIds())

    /// どの地図も、始まりから階段まで罠を踏まずに歩いて着ける。
    @Test
    def testEveryMapReachesTheStairs(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = Nil, mapIds() |> List.filter(id -> not Floor.reachesStairs(boardAt(id))))

    /// どの地図も、歩けるマスは全部、始まりから角を抜けずに歩いて着ける（斜めの角で閉じた所が無い）。
    @Test
    def testEveryWalkableCellIsReachable(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = Nil, mapIds() |> List.filter(id -> not List.isEmpty(Floor.unreachableCells(boardAt(id)))))

    /// 2F の地図は 2 枚とも、毒矢の罠を 2 つ字で置いている。
    @Test
    def testSecondFloorMapsHaveTwoDarts(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = (2, 2), (Map.size(boardAt((2, 0))#traps), Map.size(boardAt((2, 1))#traps)))

    /// 3F の地図は 2 枚とも、毒矢 2 つと警報 1 つを字で置いている。
    @Test
    def testThirdFloorMapsHaveDartsAndAnAlarm(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = (3, 3, true, true),
            (Map.size(boardAt((3, 0))#traps), Map.size(boardAt((3, 1))#traps),
             List.memberOf(Trap.Kind.Alarm, Map.valuesOf(boardAt((3, 0))#traps)), List.memberOf(Trap.Kind.Alarm, Map.valuesOf(boardAt((3, 1))#traps))))

    /// 4F の地図は 2 枚とも、毒矢 2 つと落とし穴 1 つを字で置き、銀の剣か銀の盾の候補（&）が 1 つある。
    @Test
    def testFourthFloorMapsHaveDartsAPitAndSilver(): Unit \ Assert + Fs.FileRead =
        let dart = Trap.Kind.PoisonDart;
        let shape = (b: Board.Board) -> (b#traps, Board.spotsOf(Board.Spot.Silver, b));
        Assert.assertEq(expected = ((Map#{(7, 14) => dart, (14, 16) => Trap.Kind.Pit, (2, 25) => dart}, (3, 18) :: Nil),
                                    (Map#{(4, 12) => dart, (12, 12) => Trap.Kind.Pit, (2, 29) => dart}, (12, 14) :: Nil)),
            (shape(boardAt((4, 0))), shape(boardAt((4, 1)))))

    // ══════════════════════════════════════════════════════
    // 置き場
    // ══════════════════════════════════════════════════════

    /// どの地図・どの種でも、敵は始まりから enemyMinSteps 歩以上離れている。
    @Test
    def testNoEnemyNearTheStart(): Unit \ Assert + Fs.FileRead =
        let near = RulesDoc.rec(docs()#rules)#enemyMinSteps;
        Assert.assertEq(expected = Nil,
            mapIds() |> List.flatMap(id -> seeds() |> List.filter(seed ->
                Floor.nearestEnemySteps(boardAt(id), floorOn(id, seed)) |> Option.exists(steps -> steps < near))))

    /// どの地図・どの種でも、アイテムは床（階段でも始まりでもない）にある。
    @Test
    def testItemsLieOnTheFloor(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = Nil,
            mapIds() |> List.flatMap(id -> seeds() |> List.filter(seed ->
                not Floor.itemsOnFloor(boardAt(id), floorOn(id, seed)))))

    /// 2F の地図はどの種でも、食べ物は % に、コインは * に、items は $ に置き、items は $ の数（6）まで重複なしで全部置く（2F の items は 6 種類）。
    @Test
    def testSecondFloorItemsSitOnTheirSpots(): Unit \ Assert + Fs.FileRead =
        let made = floorOn((2, 0), 3i64);
        let fixed = FloorsDoc.floorOf(2, docs()#floors) |> Option.map(f -> List.toSet(f#items));
        Assert.assertEq(expected = (true, true, fixed),
            (Floor.itemsOnSpots(boardAt((2, 0)), made), Floor.itemsOnSpots(boardAt((2, 1)), floorOn((2, 1), 50i64)),
             Some(Run.rec(made)#items |> Map.valuesOf |> List.filter(k -> Item.familyOf(k) != Item.Family.Food and Item.familyOf(k) != Item.Family.Coin) |> List.toSet)))

    /// 階には floors の Doc の数だけ敵を置く。
    @Test
    def testFloorPlacesTheEnemyCountsFromTheDoc(): Unit \ Assert + Fs.FileRead =
        let made = Floor.begin(docs(), layouts(), 1i64);
        Assert.assertEq(expected = FloorsDoc.floorOf(1, docs()#floors) |> Option.map(FloorsDoc.bodyCount),
            Some(List.length(Run.enemiesOf(made))))

    /// 群れは 1 組 3 体で、どの小ゼリーもほかの小ゼリーの隣にいる（広間に 2 組: 6 体）。
    @Test
    def testSwarmGroupsAreThreeSideBySide(): Unit \ Assert + Fs.FileRead =
        let floors = Fixtures.floorsWith("{\"maps\": {\"h\": {\"rows\": [\"###########\", \"#.........#\", \"#.........#\", \"#@........#\", \"#.........#\", \"#........>#\", \"###########\"]}}, \"floors\": [{\"floor\": 1, \"maps\": [\"h\"], \"enemies\": {\"Swarm\": 2}, \"breadOdds\": [1.0], \"meatChance\": 0.0}]}");
        let made = Floor.begin({ floors = floors | docs() }, Floor.layoutsOf(floors)#layouts, 3i64);
        Assert.assertEq(expected = (6, Nil), (List.length(Run.enemiesOf(made)), Floor.straySwarms(made)))

    /// 床には items に加えて食べ物を置く: パンは breadOdds の重みの番号の数（[0, 0, 1] なら 2 個）、肉は meatChance（1 なら必ず 1 つ）。
    @Test
    def testFoodIsAddedToTheFixedItems(): Unit \ Assert + Fs.FileRead =
        let floors = Fixtures.floorsWith("{\"floors\": [{\"floor\": 1, \"maps\": [\"1a\"], \"items\": [\"Herb\"], \"breadOdds\": [0.0, 0.0, 1.0], \"meatChance\": 1.0}]}");
        let made = Floor.begin({ floors = floors | docs() }, Floor.layoutsOf(floors)#layouts, 1i64);
        Assert.assertEq(expected = (4, Set#{Item.Kind.Herb, Item.Kind.Bread, Item.Kind.Meat}),
            (Map.size(Run.rec(made)#items), Run.rec(made)#items |> Map.valuesOf |> List.toSet))

    /// 同じ種からは同じ階（地図・敵・アイテム）。
    @Test
    def testSameSeedMakesTheSameFloor(): Unit \ Assert + Fs.FileRead =
        Assert.assertEq(expected = Run.fingerprint(Floor.begin(docs(), layouts(), 42i64)),
            Run.fingerprint(Floor.begin(docs(), layouts(), 42i64)))

    // ══════════════════════════════════════════════════════
    // 降りる
    // ══════════════════════════════════════════════════════

    /// 1F の主人公を作り替えて 2F へ入った所: 前の階で HP 7/25・鉄の剣・木の盾・木の矢 4・満腹度 40・Lv3 の 2・攻撃のカード・
    /// レベルアップと捨てるカードの選ぶ場面・移動 30・手 33・拾わなかった物のマスを持っていた。
    def entered(): Run.State =
        let c1 = Fixtures.ctx(Fixtures.hall());
        let r = Run.rec(Fixtures.start(c1, (Enemy.Kind.Jelly, (5, 3)) :: Nil, (Item.Kind.Herb, (2, 1)) :: Nil));
        let prev = Run.of({ hero = { hp = 7, maxHp = 25, sword = Some(Item.Kind.IronSword), shield = Some(Item.Kind.WoodShield) | r#hero },
                            pack = Run.Held.Held(Item.Kind.WoodArrow, 4) :: Nil, fullness = 40, level = 3, exp = 2, perks = Run.Perk.Attack :: Nil,
                            pending = Run.Choice.LevelUp(Run.Perk.MaxHp :: Nil) :: Run.Choice.Swap(Item.Kind.Pebble) :: Nil,
                            walked = 30, turns = 33, declined = Some((2, 1)), status = Run.Status.Descended | r });
        let c2 = Fixtures.ctx("#######" :: "#.^..>#" :: "#.@...#" :: "#######" :: Nil);
        Run.enter(prev, c2, { floor = 2, layout = 0, seed = 1i64, rng = 1i64 },
                  { enemies = (Enemy.Kind.Bat, (4, 2)) :: Nil, items = (Item.Kind.Bread, (4, 1)) :: Nil, traps = c2#board#traps })

    /// 降りると HP・最大 HP・剣と盾・枠・満腹度・経験値とレベル・カード・レベルアップの選ぶ場面・数えた手を持ち越し、遊んでいる状態に戻る。
    @Test
    def testEnterCarriesTheHero(): Unit \ Assert =
        let r = Run.rec(entered());
        Assert.assertEq(expected = ((7, 25, Some(Item.Kind.IronSword), Some(Item.Kind.WoodShield)), Run.Held.Held(Item.Kind.WoodArrow, 4) :: Nil,
                                    (40, 3, 2), Run.Perk.Attack :: Nil, Run.Choice.LevelUp(Run.Perk.MaxHp :: Nil) :: Nil, (30, 33), Run.Status.Playing),
            ((r#hero#hp, r#hero#maxHp, r#hero#sword, r#hero#shield), r#pack, (r#fullness, r#level, r#exp), r#perks, r#pending, (r#walked, r#turns), r#status))

    /// 降りると前の階の床の物・敵・罠・拾わなかった物のマスは捨て、主人公は次の階の始まりに立ち、敵と物と罠は次の階の物だけ。
    @Test
    def testEnterDropsTheOldFloor(): Unit \ Assert =
        let next = entered();
        let r = Run.rec(next);
        Assert.assertEq(expected = ((2, 2), 2, Map#{(4, 1) => Item.Kind.Bread}, (0, (4, 2)) :: Nil, (0, Enemy.Rest.Dormant) :: Nil, Map#{(2, 1) => Trap.Kind.PoisonDart}, None),
            (r#hero#at, r#floor, r#items, Fixtures.enemyCells(next), Fixtures.enemyRests(next), r#traps, r#declined))

    /// 降りた階は周回の種と階の番号だけで決まる（前の階で何手遊んだかに依らない）。
    @Test
    def testDescendedFloorDependsOnlyOnTheSeed(): Unit \ Assert + Fs.FileRead =
        let first = Floor.begin(docs(), layouts(), 42i64);
        let played = Run.of({ hero = { hp = 5 | Run.heroOf(first) }, turns = 77 | Run.rec(first) });
        let layoutOf = (s: Run.State) -> (Run.rec(s)#layout, Run.rec(s)#items, Fixtures.enemyCells(s));
        Assert.assertEq(expected = layoutOf(Floor.enter(docs(), layouts(), first, 2)), layoutOf(Floor.enter(docs(), layouts(), played, 2)))
}
