// 拾う（上位互換）: 上の段を拾うと手持ち全部がその段・下の段は今の段で足す・上限で切る・下の剣は消える。
// 枠は 3 つ: 4 種類目は床に残して捨てるカードを積み、Choose で答える（捨てる・拾わない）。答えるまでほかの手は受けない。
// 盤は 1 列の部屋。右へ歩いて並べたアイテムを順に拾う（敵はいない）。
mod TestPickup {

    use Board.{Dir, Dir8}
    use Run.{Event, Input}
    use Fixtures.{ctx, start}

    def row(): List[String] = "###########" :: "#@.......>#" :: "###########" :: Nil

    def count(kind: Item.Kind): Int32 = ItemsDoc.statsOf(kind, ItemsDoc.defaults())#count

    /// kinds を (2, 1) から右へ並べ、その数だけ右へ歩いた後。
    def pickAll(kinds: List[Item.Kind]): (Run.State, List[Event]) =
        let c = ctx(row());
        let placed = kinds |> List.zipWithIndex |> List.map(e -> (snd(e), (2 + fst(e), 1)));
        Step.steps(c, start(c, Nil, placed), List.repeat(List.length(kinds), Input.Move(Dir8.Right)))

    /// 木の矢の後に鉄の矢: 手持ち全部が鉄の矢になり、数は足す。
    @Test
    def testHigherTierConvertsAll(): Unit \ Assert =
        Assert.assertEq(expected = Some(Run.Held.Held(Item.Kind.IronArrow, count(Item.Kind.WoodArrow) + count(Item.Kind.IronArrow))),
            Run.heldOf(Item.Family.Arrow, Run.packOf(fst(pickAll(Item.Kind.WoodArrow :: Item.Kind.IronArrow :: Nil)))))

    /// 鉄の矢の後に木の矢: 鉄の矢のまま数を足す。
    @Test
    def testLowerTierAddsToCurrent(): Unit \ Assert =
        Assert.assertEq(expected = Some(Run.Held.Held(Item.Kind.IronArrow, count(Item.Kind.IronArrow) + count(Item.Kind.WoodArrow))),
            Run.heldOf(Item.Family.Arrow, Run.packOf(fst(pickAll(Item.Kind.IronArrow :: Item.Kind.WoodArrow :: Nil)))))

    /// 矢を 4 回拾うと上限（items の cap）で切る。
    @Test
    def testCountIsCappedAtTheLimit(): Unit \ Assert =
        Assert.assertEq(expected = Some(Run.Held.Held(Item.Kind.WoodArrow, Int32.min(ItemsDoc.statsOf(Item.Kind.WoodArrow, ItemsDoc.defaults())#cap, count(Item.Kind.WoodArrow) * 4))),
            Run.heldOf(Item.Family.Arrow, Run.packOf(fst(pickAll(List.repeat(4, Item.Kind.WoodArrow))))))

    /// 鉄の剣の後に木の剣: 木の剣は拾って消え、鉄の剣のまま。
    @Test
    def testLowerSwordIsDiscarded(): Unit \ Assert =
        let (after, events) = pickAll(Item.Kind.IronSword :: Item.Kind.WoodSword :: Nil);
        Assert.assertEq(expected = (Some(Item.Kind.IronSword), true),
            (Run.heroOf(after)#sword, List.memberOf(Event.Picked(Item.Kind.WoodSword, Item.Pick.Discarded), events)))

    /// 最後の 1 本を撃つと矢の枠は列から外れる（拾う数 1 の矢を拾い、待つ間に (4, 1) へ寄った起きた敵へ撃つ）。
    @Test
    def testEmptySlotLeavesThePack(): Unit \ Assert =
        let c = { items = Fixtures.itemsWith("{\"items\": {\"WoodArrow\": {\"count\": 1}}}") | ctx(row()) };
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (5, 1)) :: Nil, (Item.Kind.WoodArrow, (2, 1)) :: Nil), Input.Wait :: Input.Move(Dir8.Right) :: Nil);
        Assert.assertEq(expected = Nil, Run.packOf(after))

    // ══════════════════════════════════════════════════════
    // 3 つの枠と捨てるカード
    // ══════════════════════════════════════════════════════

    def full(): List[Item.Kind] = Item.Kind.WoodArrow :: Item.Kind.SmallBomb :: Item.Kind.Herb :: Item.Kind.Pebble :: Nil

    /// 枠が 3 つ埋まった後の 4 種類目は床に残り、手は進んだまま（4 歩動いた）捨てるカードが積まれる。
    @Test
    def testFourthKindIsLeftAndAsks(): Unit \ Assert =
        let (after, _) = pickAll(full());
        Assert.assertEq(expected = (Run.Choice.Swap(Item.Kind.Pebble) :: Nil, Map#{(5, 1) => Item.Kind.Pebble}, 4),
            (Run.pendingOf(after), Run.rec(after)#items, Run.rec(after)#walked))

    /// 捨てるカードの 2 枚目（爆弾）を選ぶと、爆弾が消えて同じ位置に小石が入る（手は進まない）。
    @Test
    def testChoosingASlotSwapsIt(): Unit \ Assert =
        let c = ctx(row());
        let (after, events) = Step.step(c, fst(pickAll(full())), Input.Choose(1));
        Assert.assertEq(
            expected = (Run.Held.Held(Item.Kind.WoodArrow, count(Item.Kind.WoodArrow)) :: Run.Held.Held(Item.Kind.Pebble, count(Item.Kind.Pebble))
                            :: Run.Held.Held(Item.Kind.Herb, count(Item.Kind.Herb)) :: Nil,
                        Event.Swapped(Item.Kind.SmallBomb, Item.Kind.Pebble) :: Nil, Map.empty(), Nil, 4),
            (Run.packOf(after), events, Run.rec(after)#items, Run.pendingOf(after), Run.rec(after)#walked))

    /// 4 枚目（足もとの物）を選ぶと拾わずに床に残す。その上で待つと、自分から選び直す手として聞き直す。
    @Test
    def testLeavingItThenWaitingAsksAgain(): Unit \ Assert =
        let c = ctx(row());
        let (left, _) = Step.step(c, fst(pickAll(full())), Input.Choose(3));
        let (waited, _) = Step.step(c, left, Input.Wait);
        Assert.assertEq(expected = (Nil, Some((5, 1)), Run.Choice.Swap(Item.Kind.Pebble) :: Nil),
            (Run.pendingOf(left), Run.rec(left)#declined, Run.pendingOf(waited)))

    /// 捨てるカードがある間は、動く・待つ・範囲外の Choose は何もしない（出来事は空・指紋も変わらない）。予告も敵の予告も空。
    @Test
    def testOnlyChooseIsAcceptedWhilePending(): Unit \ Assert =
        let c = ctx(row());
        let asking = fst(pickAll(full()));
        let (moved, events) = Step.steps(c, asking, Input.Move(Dir8.Right) :: Input.Wait :: Input.Choose(9) :: Nil);
        Assert.assertEq(expected = (Run.fingerprint(asking), Nil, Nil, Set.empty()),
            (Run.fingerprint(moved), events, Preview.preview(c, asking), Preview.dangerCells(c, asking)))

    /// 4 種類目に乗った手で倒れたら、捨てるカードは捨てる（最大 HP 1。ゼリーが 4 歩目で隣に来て殴る。矢は射程を 9 からにして撃たせない）。
    @Test
    def testFallingDropsThePending(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"maxHp\": 1, \"arrowMin\": 9, \"arrowMax\": 9}"),
            "##############" :: "#@..........>#" :: "##############" :: Nil);
        let placed = (Item.Kind.WoodArrow, (2, 1)) :: (Item.Kind.SmallBomb, (3, 1)) :: (Item.Kind.Smoke, (4, 1)) :: (Item.Kind.Pebble, (5, 1)) :: Nil;
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (9, 1)) :: Nil, placed), List.repeat(4, Input.Move(Dir8.Right)));
        Assert.assertEq(expected = (Run.Status.Dead, Nil), (Run.statusOf(after), Run.pendingOf(after)))
}
