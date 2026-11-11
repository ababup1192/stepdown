// 経験値・レベル・カード・落とし物: 倒すと経験値が入り、表の数に届くとレベルが上がってカードを積む（余りは持ち越し・表の後は最後の数）・
// 1 手で 2 つ上がれば 2 つ積む・取ったカードが効く・一度だけのカードは次の候補から外れる・倒した敵はマスか近くの空いた床に物を落とす。
// 盤は Fixtures.hall（7×5 の 1 部屋。主人公は (1, 3)）か小さな盤。敵は HP 1 にして 1 回殴れば倒れる。
mod TestLevel {

    use Board.Dir8
    use Run.{Event, Input, Perk}
    use Fixtures.{hall, start}

    def right(): Input = Input.Move(Dir8.Right)

    def cards(): CardsDoc.Rec = CardsDoc.rec(CardsDoc.defaults())

    /// HP 1・経験値 exp・落とす確率 chance でパンだけを落とすゼリーと HP 50 のコウモリ、rules の JSON で組んだ束（カードは 7 枚とも出す）。
    def ctxOf(exp: Int32, chance: Float64, rules: String, rows: List[String]): Run.Ctx =
        { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 1, \"exp\": ${exp}, \"dropChance\": ${chance}, \"drops\": [\"Bread\"]}, \"Bat\": {\"hp\": 50}}}"),
          cards = CardsDoc.Doc.Doc({ choices = 7 | cards() })
          | Fixtures.ctxWith(Fixtures.rulesWith(rules), rows) }

    /// 隣 (2, 3) の眠ったゼリーを殴って倒した後。
    def killed(c: Run.Ctx): (Run.State, List[Event]) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil), right())

    // ══════════════════════════════════════════════════════
    // 経験値とレベル
    // ══════════════════════════════════════════════════════

    /// 次のレベルまでの数は表の順、表を超えたら最後の数で続ける（[6, 7] → Lv1 は 6・Lv2 は 7・Lv5 も 7）。
    @Test
    def testExpTableRepeatsTheLastStep(): Unit \ Assert =
        let d = Fixtures.rulesWith("{\"levelExp\": [6, 7]}");
        Assert.assertEq(expected = (6, 7, 7), (RulesDoc.expToNext(1, d), RulesDoc.expToNext(2, d), RulesDoc.expToNext(5, d)))

    /// 経験値 3 の敵を倒し、要る数 2 で Lv2 に上がって余り 1 を持ち越し、カードを 1 つ積む。
    @Test
    def testKillGivesExpAndLevelsUp(): Unit \ Assert =
        let (after, events) = killed(ctxOf(3, 0.0, "{\"levelExp\": [2, 5]}", hall()));
        Assert.assertEq(expected = (2, 1, true, true, 1),
            (Run.rec(after)#level, Run.rec(after)#exp, List.memberOf(Event.ExpGained(3), events), List.memberOf(Event.LevelUp(2), events),
             List.length(Run.pendingOf(after))))

    /// 1 手で 2 つ上がればカードを 2 つ積む（経験値 2・要る数 1）。
    @Test
    def testTwoLevelsInOneTurnStackTwoCards(): Unit \ Assert =
        let (after, _) = killed(ctxOf(2, 0.0, "{\"levelExp\": [1]}", hall()));
        Assert.assertEq(expected = (3, 2), (Run.rec(after)#level, List.length(Run.pendingOf(after))))

    // ══════════════════════════════════════════════════════
    // カード
    // ══════════════════════════════════════════════════════

    /// 最大 HP のカードは最大 HP と今の HP を同じだけ増やす（手は進まない）。
    @Test
    def testMaxHpCardRaisesBoth(): Unit \ Assert =
        let c = ctxOf(3, 0.0, "{\"levelExp\": [3]}", hall());
        let (after, events) = Fixtures.chooseCard(c, Perk.MaxHp, fst(killed(c)));
        let max = RulesDoc.rec(c#rules)#maxHp;
        Assert.assertEq(expected = ((max + cards()#maxHp, max + cards()#maxHp), Event.PerkTaken(Perk.MaxHp) :: Nil, Nil),
            ((Run.heroOf(after)#maxHp, Run.heroOf(after)#hp), events, Run.pendingOf(after)))

    /// 攻撃のカードは殴るダメージを増やす（ゼリーを倒した後、斜めの HP 50 のコウモリを殴る）。
    @Test
    def testAttackCardRaisesTheStrike(): Unit \ Assert =
        let c = ctxOf(3, 0.0, "{\"levelExp\": [3]}", hall());
        let first = start(c, (Enemy.Kind.Jelly, (2, 3)) :: (Enemy.Kind.Bat, (2, 2)) :: Nil, Nil);
        let (took, _) = Fixtures.chooseCard(c, Perk.Attack, fst(Step.step(c, first, right())));
        let (_, events) = Step.step(c, took, Input.Move(Dir8.UpRight));
        Assert.assertEq(expected = Some(Event.HeroStruck(1, RulesDoc.rec(c#rules)#baseAttack + cards()#attack)), List.head(events))

    /// 一度だけのカード（爆弾 1 体）は、取った後の候補に出ない（7 枚とも出す設定。2 体目を倒した次の候補は 6 枚）。
    @Test
    def testOnceCardLeavesTheNextChoices(): Unit \ Assert =
        let c = ctxOf(3, 0.0, "{\"levelExp\": [3]}", hall());
        let first = start(c, (Enemy.Kind.Jelly, (2, 3)) :: (Enemy.Kind.Jelly, (2, 2)) :: Nil, Nil);
        let (took, _) = Fixtures.chooseCard(c, Perk.LoneBomb, fst(Step.step(c, first, right())));
        let (again, _) = Step.step(c, took, Input.Move(Dir8.UpRight));
        Assert.assertEq(expected = (6, false), (Fixtures.cardCount(again), Fixtures.offers(Perk.LoneBomb, again)))

    /// 爆弾 1 体のカードがあれば、攻撃できる隣の起きた敵が 1 体でも爆弾を投げる
    /// （ゼリーの下の爆弾を、倒した次の手に拾う。斜めの隣にコウモリ）。
    @Test
    def testLoneBombCard(): Unit \ Assert =
        let c = ctxOf(3, 0.0, "{\"levelExp\": [3]}", hall());
        let first = start(c, (Enemy.Kind.Jelly, (2, 3)) :: (Enemy.Kind.Bat, (3, 2)) :: Nil, (Item.Kind.SmallBomb, (2, 3)) :: Nil);
        let (took, _) = Fixtures.chooseCard(c, Perk.LoneBomb, fst(Step.step(c, first, right())));
        let (_, events) = Step.step(c, took, right());
        Assert.assertEq(expected = true, List.memberOf(Event.BombThrown(Item.Kind.SmallBomb, (2, 3)), events))

    /// 押し出しのカードで殴ると押し出し、押し先が壁なら動かさずに wallDamage を足す（真下の HP 50 のコウモリの下は壁）。
    @Test
    def testPushCardIntoAWall(): Unit \ Assert =
        let c = ctxOf(3, 0.0, "{\"levelExp\": [3]}", "#######" :: "#.@..>#" :: "#.....#" :: "#######" :: Nil);
        let first = start(c, (Enemy.Kind.Jelly, (3, 1)) :: (Enemy.Kind.Bat, (2, 2)) :: Nil, Nil);
        let (took, _) = Fixtures.chooseCard(c, Perk.Push, fst(Step.step(c, first, right())));
        let (_, events) = Step.step(c, took, Input.Move(Dir8.Down));
        Assert.assertEq(expected = Event.HeroStruck(1, RulesDoc.rec(c#rules)#baseAttack) :: Event.PushedIntoWall(1, cards()#wallDamage) :: Nil,
            List.take(2, events))

    // ══════════════════════════════════════════════════════
    // 落とし物
    // ══════════════════════════════════════════════════════

    /// 倒した敵は、そのマスが空いた床ならそこへ落とす（落とす確率 1）。
    @Test
    def testDropOnTheEnemyCell(): Unit \ Assert =
        let (after, events) = killed(ctxOf(0, 1.0, "{}", hall()));
        Assert.assertEq(expected = (true, Map#{(2, 3) => Item.Kind.Bread}),
            (List.memberOf(Event.ItemDropped(Item.Kind.Bread, (2, 3)), events), Run.rec(after)#items))

    /// 敵のマスが階段なら、歩く距離の近い空いた床へ落とす（主人公のマスには落とさない）。
    @Test
    def testDropMovesOffTheStairs(): Unit \ Assert =
        let c = ctxOf(0, 1.0, "{}", "#####" :: "#@>.#" :: "#####" :: Nil);
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 1)) :: Nil, Nil), right());
        Assert.assertEq(expected = true, List.memberOf(Event.ItemDropped(Item.Kind.Bread, (3, 1)), events))
}
