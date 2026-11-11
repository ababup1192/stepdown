// 拾う（上位互換）: 上の段を拾うと手持ち全部がその段・下の段は今の段で足す・上限で切る・下の剣は消える。
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

    /// 矢を 4 回拾うと上限で切る。
    @Test
    def testCountIsCappedAtTheLimit(): Unit \ Assert =
        Assert.assertEq(expected = Some(Run.Held.Held(Item.Kind.WoodArrow, Int32.min(RulesDoc.rec(RulesDoc.defaults())#arrowCap, count(Item.Kind.WoodArrow) * 4))),
            Run.heldOf(Item.Family.Arrow, Run.packOf(fst(pickAll(List.repeat(4, Item.Kind.WoodArrow))))))

    /// 鉄の剣の後に木の剣: 木の剣は拾って消え、鉄の剣のまま。
    @Test
    def testLowerSwordIsDiscarded(): Unit \ Assert =
        let (after, events) = pickAll(Item.Kind.IronSword :: Item.Kind.WoodSword :: Nil);
        Assert.assertEq(expected = (Some(Item.Kind.IronSword), true),
            (Run.heroOf(after)#sword, List.memberOf(Event.Picked(Item.Kind.WoodSword, Item.Pick.Discarded), events)))

    /// 最後の 1 本を撃つと矢の枠は列から外れる（拾う数 1 の矢を拾い、距離 3 の起きた敵へ撃つ）。
    @Test
    def testEmptySlotLeavesThePack(): Unit \ Assert =
        let c = { items = Fixtures.itemsWith("{\"items\": {\"WoodArrow\": {\"count\": 1}}}") | ctx(row()) };
        let (after, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (5, 1)) :: Nil, (Item.Kind.WoodArrow, (2, 1)) :: Nil), Input.Move(Dir8.Right));
        Assert.assertEq(expected = Nil, Run.packOf(after))
}
