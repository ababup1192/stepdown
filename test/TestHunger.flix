// 空腹と食べ物: 手が進むたびに数えて満腹度が減る・0 の間は HP が減り自然回復が止まる・空腹のダメージも薬と倒れるを通る・
// 食べ物は止まったマス（待つも）で食べ、最大なら床に残し、あふれる分は捨てる・ダッシュは食べた歩で止まる。
// 盤は Fixtures.hall（7×5 の 1 部屋。主人公は (1, 3)）か 1 列の通路。敵はいない。
mod TestHunger {

    use Board.{Dir, Dir8}
    use Run.{Event, Input, Source}
    use Fixtures.{hall, start}

    def rules(): RulesDoc.Rec = RulesDoc.rec(RulesDoc.defaults())

    def right(): Input = Input.Move(Dir8.Right)

    def ctxOf(json: String, rows: List[String]): Run.Ctx = Fixtures.ctxWith(Fixtures.rulesWith(json), rows)

    // ══════════════════════════════════════════════════════
    // 減り方
    // ══════════════════════════════════════════════════════

    /// 手が進んだ数だけ数え、hungerEvery（2）ごとに満腹度 -1。左の壁へのつかえた手は数えない（待つ・つかえる・待つで 2 手）。
    @Test
    def testFullnessDropsByTurnsTaken(): Unit \ Assert =
        let c = ctxOf("{\"hungerEvery\": 2}", hall());
        let (after, _) = Step.steps(c, start(c, Nil, Nil), Input.Wait :: Input.Move(Dir8.Left) :: Input.Wait :: Nil);
        Assert.assertEq(expected = (rules()#fullnessMax - 1, 2), (Run.rec(after)#fullness, Run.rec(after)#turns))

    /// 満腹度が 0 の間は 1 手ごとに starveDamage だけ HP が減り、自然回復（毎歩）は止まる（満腹度 1・毎手 -1 で 1 歩目から 0）。
    @Test
    def testStarvingHurtsAndStopsRegen(): Unit \ Assert =
        let c = ctxOf("{\"fullnessMax\": 1, \"hungerEvery\": 1, \"regenEvery\": 1}", hall());
        let (once, _) = Step.step(c, start(c, Nil, Nil), right());
        let (twice, events) = Step.step(c, once, right());
        Assert.assertEq(expected = (rules()#maxHp - rules()#starveDamage * 2, false, true),
            (Run.heroOf(twice)#hp, List.memberOf(Event.Regenerated(1), events), List.memberOf(Event.HeroHurt(Source.Starve, rules()#starveDamage, false), events)))

    /// 空腹のダメージでも薬の閾値を割れば薬を使う（最大 HP 4・閾値 1。1 歩目で薬草を拾い、3 歩目で HP 1 → 薬草）。
    @Test
    def testStarvingGoesThroughThePotion(): Unit \ Assert =
        let c = ctxOf("{\"maxHp\": 4, \"fullnessMax\": 1, \"hungerEvery\": 1, \"regenEvery\": 99}", hall());
        let (after, _) = Step.steps(c, start(c, Nil, (Item.Kind.Herb, (2, 3)) :: Nil), right() :: right() :: right() :: Nil);
        let heal = ItemsDoc.statsOf(Item.Kind.Herb, ItemsDoc.defaults())#heal;
        Assert.assertEq(expected = (Run.Status.Playing, Int32.min(4, 1 + heal), Nil), (Run.statusOf(after), Run.heroOf(after)#hp, Run.packOf(after)))

    /// 空腹のダメージで HP が 0 になれば倒れる。
    @Test
    def testStarvingToDeath(): Unit \ Assert =
        let c = ctxOf("{\"maxHp\": 1, \"fullnessMax\": 1, \"hungerEvery\": 1}", hall());
        let (after, events) = Step.step(c, start(c, Nil, Nil), right());
        Assert.assertEq(expected = (Run.Status.Dead, Some(Event.HeroDied)), (Run.statusOf(after), List.last(events)))

    // ══════════════════════════════════════════════════════
    // 食べ物
    // ══════════════════════════════════════════════════════

    /// 満腹度が最大の時はパンの上に乗っても食べずに残す。1 手で 1 減った後、その上で待つと食べ、あふれた分は捨てる（戻るのは 1）。
    @Test
    def testEatOnlyWhenNotFullAndOverflowIsLost(): Unit \ Assert =
        let c = ctxOf("{\"hungerEvery\": 1}", hall());
        let (stood, first) = Step.step(c, start(c, Nil, (Item.Kind.Bread, (2, 3)) :: Nil), right());
        let (fed, second) = Step.step(c, stood, Input.Wait);
        Assert.assertEq(expected = (false, true, Map.empty()),
            (List.memberOf(Event.Ate(Item.Kind.Bread, 1), first), List.memberOf(Event.Ate(Item.Kind.Bread, 1), second), Run.rec(fed)#items))

    /// ダッシュは食べた歩で止まる（通路のパン (5, 1) の上。1 歩目で満腹度が 1 減っている）。
    @Test
    def testDashStopsWhereItEats(): Unit \ Assert =
        let c = ctxOf("{\"hungerEvery\": 1}", "##########" :: "#@,,,,,,>#" :: "##########" :: Nil);
        let (first, _) = Step.step(c, start(c, Nil, (Item.Kind.Bread, (5, 1)) :: Nil), right());
        Assert.assertEq(expected = Some((5, 1)), List.last(Dash.dash(c, first, Dir.Right)) |> Option.map(e -> Run.heroOf(fst(e))#at))
}
