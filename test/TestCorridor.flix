// 通路の矢: 撃つ側と当てる相手の両方が部屋の中の時だけ矢が出る（主人公の矢も弓兵の矢も）。弓兵は隣（殴る隣と同じ 8 方向・角は抜けない）へは
// 通路でも矢を撃つ。主人公の矢は今までどおり隣には撃たない。小石は通路でも投げる。赤も同じ式。
// 盤は左上の部屋（(1, 1)〜(3, 3)）から右へ通路が延びる。
mod TestCorridor {

    use Run.{Event, Input}
    use Fixtures.{ctx, start, holding}

    def rows(): List[String] =
        "##########" ::
        "#@..,,,,,#" ::
        "#...######" ::
        "#..>######" ::
        "##########" :: Nil

    /// 主人公を at に立たせ、起きた foes を置いた所。
    def placed(c: Run.Ctx, at: Board.Cell, foes: List[(Enemy.Kind, Board.Cell)]): Run.State =
        let r = Run.rec(Fixtures.awake(start(c, foes, Nil)));
        Run.of({ hero = { at = at | r#hero } | r })

    def shotAt(events: List[Event]): Bool = List.exists(Run.hurtsHero, events)

    /// 通路の弓兵は、列がそろい距離が届いても部屋の主人公へ撃たない。部屋どうしなら撃つ。
    @Test
    def testArcherInTheCorridorHoldsFire(): Unit \ Assert =
        let c = ctx(rows());
        let (_, fromCorridor) = Step.step(c, placed(c, (3, 1), (Enemy.Kind.Archer, (7, 1)) :: Nil), Input.Wait);
        let (_, inRoom) = Step.step(c, placed(c, (1, 1), (Enemy.Kind.Archer, (1, 3)) :: Nil), Input.Wait);
        Assert.assertEq(expected = (false, true), (shotAt(fromCorridor), shotAt(inRoom)))

    /// 弓兵は隣の主人公へは通路でも矢を撃つ（殴るでなく矢）。
    @Test
    def testArcherShootsBesideInTheCorridor(): Unit \ Assert =
        let c = ctx(rows());
        let (_, events) = Step.step(c, placed(c, (5, 1), (Enemy.Kind.Archer, (6, 1)) :: Nil), Input.Wait);
        Assert.assertEq(expected = (true, true), (List.memberOf(Event.EnemyShot(0, (6, 1), (5, 1)), events), shotAt(events)))

    /// 主人公の矢は通路の敵へは出ず、部屋の敵へは出る。小石は通路の敵へも投げる。
    @Test
    def testHeroArrowNeedsBothInRooms(): Unit \ Assert =
        let c = ctx(rows());
        let arrows = (foe: Board.Cell) -> holding(Run.Held.Held(Item.Kind.WoodArrow, 3) :: Nil, placed(c, (3, 1), (Enemy.Kind.Jelly, foe) :: Nil));
        let pebbles = holding(Run.Held.Held(Item.Kind.Pebble, 3) :: Nil, placed(c, (3, 1), (Enemy.Kind.Jelly, (5, 1)) :: Nil));
        let thrown = (runState: Run.State, mark: Preview.Mark) -> List.memberOf(mark, Preview.summarize(snd(Step.step(c, runState, Input.Wait))));
        Assert.assertEq(expected = (false, true, true),
            (thrown(arrows((6, 1)), Preview.Mark.Arrow), thrown(arrows((3, 3)), Preview.Mark.Arrow), thrown(pebbles, Preview.Mark.Pebble)))

    /// 赤も同じ式: 通路の弓兵の赤に部屋の主人公のマスは入らず、隣のマスは入る。
    @Test
    def testRedFollowsTheRule(): Unit \ Assert =
        let c = ctx(rows());
        let danger = Preview.dangerCells(c, placed(c, (3, 1), (Enemy.Kind.Archer, (7, 1)) :: Nil));
        Assert.assertEq(expected = (false, true), (Set.memberOf((3, 1), danger), Set.memberOf((6, 1), danger)))
}
