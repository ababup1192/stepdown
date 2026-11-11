// JSON 橋渡しのテスト（1 Doc につき最大 3 本まで。同型の重複は書かない）。
mod TestDocs {

    use fge::{GameEngine, Resources}

    // ══════════════════════════════════════════════════════
    // RulesDoc
    // ══════════════════════════════════════════════════════

    /// 読めない JSON は None（起動の読み込み RulesDoc.load はそこで既定へ落ちる）。
    @Test
    def testRulesBrokenJsonFallsBack(): Unit \ Assert =
        Assert.assertEq(expected = None, RulesDoc.fromJson("{ broken") |> Option.map(doc -> RulesDoc.rec(doc)#maxHp))

    /// 1 欄だけ書いた JSON はそこだけ効き、残りは既定のまま。
    @Test
    def testRulesOverridesOneField(): Unit \ Assert =
        Assert.assertEq(expected = Some((7, RulesDoc.rec(RulesDoc.defaults())#arrowMax)),
            RulesDoc.fromJson("{\"version\":1, \"maxHp\": 7}") |> Option.map(doc -> (RulesDoc.rec(doc)#maxHp, RulesDoc.rec(doc)#arrowMax)))

    /// 薬を使う HP は 最大 HP × potionAt の切り捨て（20 × 0.25 = 5・7 × 0.25 = 1.75 → 1）。
    @Test
    def testPotionThresholdRoundsDown(): Unit \ Assert =
        Assert.assertEq(expected = (5, 1), (RulesDoc.potionHp(20, RulesDoc.defaults()), RulesDoc.potionHp(7, RulesDoc.defaults())))

    // ══════════════════════════════════════════════════════
    // ItemsDoc・EnemiesDoc
    // ══════════════════════════════════════════════════════

    /// 1 段の 1 欄だけ書けばそこだけ効き、書かない段は既定のまま。知らない段の名前は読まない。
    @Test
    def testItemsOverrideOneTier(): Unit \ Assert =
        Assert.assertEq(expected = Some((9, ItemsDoc.nameOf(Item.Kind.IronSword, ItemsDoc.defaults()), ItemsDoc.statsOf(Item.Kind.WoodSword, ItemsDoc.defaults())#attack)),
            ItemsDoc.fromJson("{\"items\": {\"IronSword\": {\"attack\": 9}, \"GoldSword\": {\"attack\": 99}}}")
                |> Option.map(doc -> (ItemsDoc.statsOf(Item.Kind.IronSword, doc)#attack, ItemsDoc.nameOf(Item.Kind.IronSword, doc), ItemsDoc.statsOf(Item.Kind.WoodSword, doc)#attack)))

    /// 読めない敵の Doc は None。1 欄だけ書けばそこだけ効く。
    @Test
    def testEnemiesFallBackAndOverride(): Unit \ Assert =
        Assert.assertEq(expected = (None, Some(9), Some(EnemiesDoc.statsOf(Enemy.Kind.Jelly, EnemiesDoc.defaults())#attack)),
            (EnemiesDoc.fromJson("[") |> Option.map(doc -> EnemiesDoc.statsOf(Enemy.Kind.Jelly, doc)#hp),
             EnemiesDoc.fromJson("{\"enemies\": {\"Jelly\": {\"hp\": 9}}}") |> Option.map(doc -> EnemiesDoc.statsOf(Enemy.Kind.Jelly, doc)#hp),
             EnemiesDoc.fromJson("{\"enemies\": {\"Jelly\": {\"hp\": 9}}}") |> Option.map(doc -> EnemiesDoc.statsOf(Enemy.Kind.Jelly, doc)#attack)))

    // ══════════════════════════════════════════════════════
    // FloorsDoc
    // ══════════════════════════════════════════════════════

    /// 階の地図の名前とアイテムの列は JSON の長さに追随し、知らないアイテムの名前は飛ばす。
    @Test
    def testFloorsFollowTheLists(): Unit \ Assert =
        let text = "{\"maps\": {\"a\": {\"rows\": [\"#@>#\"]}}, \"floors\": [{\"floor\": 1, \"maps\": [\"a\", \"a\"], \"items\": [\"Herb\", \"Nope\", \"Tonic\"]}]}";
        Assert.assertEq(expected = Some((2, Item.Kind.Herb :: Item.Kind.Tonic :: Nil)),
            FloorsDoc.fromJson(text) |> Option.flatMap(doc -> FloorsDoc.floorOf(1, doc)) |> Option.map(f -> (List.length(f#maps), f#items)))

    /// 組めない地図は飛ばして理由を言い、残りの地図で階を組む。
    @Test
    def testBrokenMapIsSkipped(): Unit \ Assert =
        let text = "{\"maps\": {\"bad\": {\"rows\": [\"#@#\"]}, \"good\": {\"rows\": [\"#@>#\"]}}, \"floors\": [{\"floor\": 1, \"maps\": [\"bad\", \"good\"]}]}";
        let built = FloorsDoc.fromJson(text) |> Option.map(Floor.layoutsOf);
        Assert.assertEq(expected = Some((1, 1)),
            built |> Option.map(b -> (Floor.layoutCount(1, b#layouts), List.length(b#problems))))

    // ══════════════════════════════════════════════════════
    // TextDoc・LookDoc・KeysDoc
    // ══════════════════════════════════════════════════════

    /// 文言の {名前} を差し込む。書いていない文言は既定。
    @Test
    def testTextFillsPlaceholders(): Unit \ Assert =
        Assert.assertEq(expected = Some(("ゼリーを 3 たたいた", TextDoc.lineOf(TextDoc.Line.Title, TextDoc.defaults()))),
            TextDoc.fromJson("{\"struck\": \"{enemy}を {n} たたいた\"}")
                |> Option.map(doc -> (TextDoc.fill(("enemy", "ゼリー") :: ("n", "3") :: Nil, TextDoc.lineOf(TextDoc.Line.Struck, doc)), TextDoc.lineOf(TextDoc.Line.Title, doc))))

    /// 見せ方の秒は 1 欄だけ書けばそこだけ効く。
    @Test
    def testLookOverridesOneField(): Unit \ Assert =
        Assert.assertEq(expected = Some((0.5, LookDoc.rec(LookDoc.defaults())#cameraPitch)),
            LookDoc.fromJson("{\"stepSeconds\": 0.5}") |> Option.map(doc -> (LookDoc.rec(doc)#stepSeconds, LookDoc.rec(doc)#cameraPitch)))

    /// 書き間違い("Sinistra")はその 1 個だけ落ちる。全部知らない名前で空になった操作だけ既定へ落ちる。
    @Test
    def testKeysDropUnknownNamesAndEmptyActionFallsBack(): Unit \ Assert =
        Assert.assertEq(
            expected = Some((GameEngine.Key.Left :: Nil, KeysDoc.rec(KeysDoc.defaults())#dashUp, KeysDoc.rec(KeysDoc.defaults())#down)),
            KeysDoc.fromJson("{\"left\": [\"Left\", \"Sinistra\"], \"dashUp\": [\"Hop\"]}")
                |> Option.map(d -> (KeysDoc.rec(d)#left, KeysDoc.rec(d)#dashUp, KeysDoc.rec(d)#down)))

    // ══════════════════════════════════════════════════════
    // 保存即反映の一覧表（Controls.docTable）
    // ══════════════════════════════════════════════════════

    /// 1 歩進めた 1F に、docId の行の読み直しを当てた後の移動の数（行が無ければ None）。
    def walkedAfterReload(docId: String): Option[Int32] \ IO =
        let res = Fixtures.res();
        let played = World.begin(res, World.initial(res, World.fixedSeed())) |> World.play(res, Run.Input.Move(Board.Dir8.Right));
        Controls.docTable() |> List.find(row -> row#docId == Some(docId))
            |> Option.map(row -> run { row#reload(Resources.of(res, played)) } with Fs.FileRead.runWithIO)
            |> Option.map(h -> Run.rec(h#world#runState)#walked)

    /// 階の Doc を保存し直すと、今の階を同じ種で初めから作り直す。
    @Test
    def testFloorsReloadRebuildsTheFloor(): Unit \ {Assert, IO} =
        Assert.assertEq(expected = Some(0), walkedAfterReload("floors"))

    /// キーの Doc を保存し直しても、今の階はそのまま。
    @Test
    def testKeysReloadKeepsTheFloor(): Unit \ {Assert, IO} =
        Assert.assertEq(expected = Some(1), walkedAfterReload("keys"))
}
