// 身代わり人形: HP が 0 以下になったら倒れる代わりに砕けて HP が最大の半分になる・薬が先・2 つ目は持てない・罠でも効く・
// 方向の予告は「人形」で「倒れる」を出さない。盤は既定の広間（主人公 (1, 3)）。
mod TestDoll {

    use Board.Dir8
    use Run.{Event, Input}
    use Preview.Mark
    use Fixtures.{ctx, start, hall, holding}

    def maxHp(): Int32 = RulesDoc.rec(RulesDoc.defaults())#maxHp

    def doll(n: Int32): Run.Held = Run.Held.Held(Item.Kind.Doll, n)

    /// HP hp で pack を持ち、右隣に起きたゼリーがいる所。
    def cornered(c: Run.Ctx, hp: Int32, pack: Run.Pack): Run.State =
        let r = Run.rec(holding(pack, Fixtures.awake(start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil))));
        Run.of({ hero = { hp = hp | r#hero } | r })

    /// 殴られて HP が 0 になる手に人形が砕け、HP が最大の半分で立っている。予告は人形で、倒れるは出ない。
    @Test
    def testDollSavesFromFalling(): Unit \ Assert =
        let c = ctx(hall());
        let first = cornered(c, 2, doll(1) :: Nil);
        let (after, events) = Step.step(c, first, Input.Wait);
        let summary = Preview.summarize(events);
        Assert.assertEq(expected = (Run.Status.Playing, maxHp() / 2, Nil, true, false),
            (Run.statusOf(after), Run.heroOf(after)#hp, Run.packOf(after), List.memberOf(Mark.Doll, summary), List.memberOf(Mark.Falls, summary)))

    /// 薬が先: 薬で持ちこたえれば人形は残る。
    @Test
    def testPotionComesFirst(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, cornered(c, 2, Run.Held.Held(Item.Kind.Herb, 1) :: doll(1) :: Nil), Input.Wait);
        Assert.assertEq(expected = (doll(1) :: Nil, false), (Run.packOf(after), List.memberOf(Event.DollSaved(maxHp() / 2), events)))

    /// 2 つ目は持てない（拾っても 1 つのまま）。
    @Test
    def testOnlyOneDoll(): Unit \ Assert =
        let c = ctx(hall());
        let r = Run.rec(holding(doll(1) :: Nil, start(c, Nil, Nil)));
        let (after, _) = Step.step(c, Run.of({ items = Map#{(2, 3) => Item.Kind.Doll} | r }), Input.Move(Dir8.Right));
        Assert.assertEq(expected = doll(1) :: Nil, Run.packOf(after))

    /// 毒矢の罠で HP が 0 になっても人形が砕ける。
    @Test
    def testDollSavesFromATrap(): Unit \ Assert =
        let c = ctx("######" :: "#@^.>#" :: "######" :: Nil);
        let r = Run.rec(holding(doll(1) :: Nil, start(c, Nil, Nil)));
        let (after, _) = Step.step(c, Run.of({ hero = { hp = 1 | r#hero } | r }), Input.Move(Dir8.Right));
        Assert.assertEq(expected = (Run.Status.Playing, maxHp() / 2), (Run.statusOf(after), Run.heroOf(after)#hp))
}
