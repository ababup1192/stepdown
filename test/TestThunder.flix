// かみなりの書: 判定の段で、主人公に気付いている起きた敵が thunderNeeds（3）以上なら、その全員に damage（盾の正面でも当たる）。
// 爆弾の条件が立つ時は爆弾が先・眠っている敵は数えない。盤は既定の広間（主人公 (1, 3)・1 部屋なのでどの敵も気付く）。
mod TestThunder {

    use Run.{Event, Input}
    use Fixtures.{ctx, start, hall, holding}

    def thunderDamage(): Int32 = ItemsDoc.statsOf(Item.Kind.Thunder, ItemsDoc.defaults())#damage

    def jellyHp(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Jelly, EnemiesDoc.defaults())#hp

    def scroll(): Run.Held = Run.Held.Held(Item.Kind.Thunder, 1)

    /// 起きたゼリーを cells に置き、pack を持って待った手の出来事。
    def waitWith(pack: Run.Pack, cells: List[Board.Cell]): (Run.State, List[Event]) =
        let c = ctx(hall());
        Step.step(c, holding(pack, Fixtures.awake(start(c, cells |> List.map(cell -> (Enemy.Kind.Jelly, cell)), Nil))), Input.Wait)

    /// 起きた敵 3 体で、3 体ともに damage が当たり、書は 1 つ減る（持っていた 1 つが無くなる）。
    @Test
    def testThreeAwakeEnemiesCallThunder(): Unit \ Assert =
        let (after, events) = waitWith(scroll() :: Nil, (5, 1) :: (6, 3) :: (5, 5) :: Nil);
        Assert.assertEq(expected = (true, Some(jellyHp() - thunderDamage()), Nil),
            (List.memberOf(Event.ThunderCast(0 :: 1 :: 2 :: Nil), events), Run.enemiesOf(after) |> List.head |> Option.map(e -> e#hp), Run.packOf(after)))

    /// 起きた敵 2 体では出ず、3 体でも 1 体が眠っていれば出ない。
    @Test
    def testTwoAwakeOrADozingOneCallsNoThunder(): Unit \ Assert =
        let c = ctx(hall());
        let dozing = Fixtures.resting(2, Enemy.Rest.Dozing(2), holding(scroll() :: Nil,
            Fixtures.awake(start(c, (Enemy.Kind.Jelly, (5, 1)) :: (Enemy.Kind.Jelly, (6, 3)) :: (Enemy.Kind.Jelly, (5, 5)) :: Nil, Nil))));
        Assert.assertEq(expected = (false, false),
            (Preview.summarize(snd(waitWith(scroll() :: Nil, (5, 1) :: (6, 3) :: Nil))) |> List.memberOf(Preview.Mark.Thunder),
             Preview.summarize(snd(Step.step(c, dozing, Input.Wait))) |> List.memberOf(Preview.Mark.Thunder)))

    /// 隣に 2 体で爆弾の条件が立つ時は、3 体いても爆弾が先（書は使わない）。
    @Test
    def testBombComesBeforeThunder(): Unit \ Assert =
        let (_, events) = waitWith(Run.Held.Held(Item.Kind.SmallBomb, 1) :: scroll() :: Nil, (2, 3) :: (2, 2) :: (6, 3) :: Nil);
        Assert.assertEq(expected = (true, false), (List.memberOf(Preview.Mark.Bomb, Preview.summarize(events)), List.memberOf(Preview.Mark.Thunder, Preview.summarize(events))))

    /// 正面を向いた盾持ちにも当たる。
    @Test
    def testThunderGoesThroughTheShield(): Unit \ Assert =
        let c = ctx(hall());
        let first = holding(scroll() :: Nil, Fixtures.awake(Fixtures.facing(0, Board.Dir8.Left,
            start(c, (Enemy.Kind.Shield, (5, 3)) :: (Enemy.Kind.Jelly, (5, 1)) :: (Enemy.Kind.Jelly, (5, 5)) :: Nil, Nil))));
        Assert.assertEq(expected = true, List.memberOf(Event.ThunderHit(0, thunderDamage()), snd(Step.step(c, first, Input.Wait))))
}
