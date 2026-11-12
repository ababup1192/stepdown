// ちいさなコイン: 通ったマスで勝手に拾う（枠を使わず、捨てるカードも出ない）・ダッシュは拾った歩で止まる・階をまたいで持ち越す・
// 階には coinOdds の重みで引いた枚数をコインの候補（*）へ置き、ほかの物と敵の置き場は替えない。
mod TestCoin {

    use Board.Dir8
    use Run.{Event, Input}
    use Fixtures.{ctx, start, hall, holding}

    /// 右隣にコインがあり、枠が 3 つ埋まっている所。
    def fullPackBesideACoin(c: Run.Ctx): Run.State =
        let r = Run.rec(holding(Run.Held.Held(Item.Kind.WoodArrow, 3) :: Run.Held.Held(Item.Kind.SmallBomb, 1) :: Run.Held.Held(Item.Kind.Herb, 1) :: Nil, start(c, Nil, Nil)));
        Run.of({ items = Map#{(2, 3) => Item.Kind.Coin} | r })

    /// 枠が埋まっていても拾い、数が 1 増え、枠も選ぶ場面も変わらない。
    @Test
    def testPickedWithoutASlot(): Unit \ Assert =
        let c = ctx(hall());
        let first = fullPackBesideACoin(c);
        let (after, events) = Step.step(c, first, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (1, Map#{}, Run.packOf(first), 0, true),
            (Run.rec(after)#coins, Run.rec(after)#items, Run.packOf(after), List.length(Run.pendingOf(after)), List.memberOf(Event.CoinPicked(1), events)))

    /// ダッシュはコインを拾った歩で止まる。
    @Test
    def testDashStopsOnACoin(): Unit \ Assert =
        let c = ctx(hall());
        let steps = Dash.dash(c, fullPackBesideACoin(c), Board.Dir.Right);
        Assert.assertEq(expected = Some((2, 3)), steps |> List.last |> Option.map(e -> Run.heroOf(fst(e))#at))

    /// 次の階へ持ち越す。
    @Test
    def testCoinsCarryOver(): Unit \ Assert =
        let c = ctx(hall());
        let carrying = Run.of({ coins = 3 | Run.rec(start(c, Nil, Nil)) });
        let next = Run.enter(carrying, c, { floor = 2, layout = 0, seed = 1i64, rng = 1i64 }, { enemies = Nil, items = Nil, traps = Map.empty() });
        Assert.assertEq(expected = 3, Run.rec(next)#coins)

    def floorsJson(coinOdds: String): String =
        "{\"maps\": {\"c\": {\"rows\": [\"#########\", \"#@..*..$#\", \"#.*...*.#\", \"#..$...>#\", \"#########\"]}}, " +
        "\"floors\": [{\"floor\": 1, \"maps\": [\"c\"], \"enemies\": {}, \"items\": [\"Herb\", \"Pebble\"], \"breadOdds\": [1.0], \"meatChance\": 0.0${coinOdds}}]}"

    def placed(coinOdds: String, seed: Int64): Run.Rec =
        let floors = Fixtures.floorsWith(floorsJson(coinOdds));
        let docs = { rules = RulesDoc.defaults(), items = ItemsDoc.defaults(), enemies = EnemiesDoc.defaults(), cards = CardsDoc.defaults(), floors = floors };
        Run.rec(Floor.begin(docs, Floor.layoutsOf(floors)#layouts, seed))

    /// coinOdds で 2 枚を引くと、コインの候補（*）の 2 マスにコインが置かれ、ほかの物の置き場は coinOdds が無い時と同じ。
    @Test
    def testCoinsGoOnTheirSpots(): Unit \ Assert =
        let withCoins = placed(", \"coinOdds\": [0.0, 0.0, 1.0]", 7i64);
        let without = placed("", 7i64);
        let coins = withCoins#items |> Map.filter(kind -> kind == Item.Kind.Coin) |> Map.keysOf;
        Assert.assertEq(expected = (2, true, without#items),
            (Set.size(coins), Set.isSubsetOf(coins, Set#{(4, 1), (2, 2), (6, 2)}), withCoins#items |> Map.filter(kind -> kind != Item.Kind.Coin)))
}
