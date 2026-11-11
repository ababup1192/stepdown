// 盤: マスの性質の表・文字の地図の読み・部屋の番号・見たことにするマス。
mod TestBoard {

    use Board.Tile

    // ══════════════════════════════════════════════════════
    // マスの性質の表
    // ══════════════════════════════════════════════════════

    /// 歩ける・飛べる・矢を止める・見通しを止める（床・壁・柱・岩・水・階段の順）。
    @Test
    def testTraitsTable(): Unit \ Assert =
        let row = tile -> { let t = Board.traitsOf(tile); (t#walkable, t#flyable, t#stopsArrow, t#blocksSight) };
        Assert.assertEq(
            expected = ((true, true, false, false), (false, false, true, true), (false, false, true, true),
                        (false, false, true, false), (false, true, false, false), (true, true, false, false)),
            (row(Tile.Floor), row(Tile.Wall), row(Tile.Pillar), row(Tile.Rock), row(Tile.Water), row(Tile.Stairs)))

    // ══════════════════════════════════════════════════════
    // 文字の地図
    // ══════════════════════════════════════════════════════

    /// 知らない字・始まりが 2 つ・階段が無い・行の長さがそろわない地図は組まない。
    @Test
    def testMalformedMapsAreRejected(): Unit \ Assert =
        let bad = rows -> Result.isErr(Board.fromRows(rows));
        Assert.assertEq(expected = (true, true, true, true),
            (bad("#@?>#" :: Nil), bad("#@@>#" :: Nil), bad("#@..#" :: Nil), bad("#@.>#" :: "##" :: Nil)))

    /// 部屋は読む順に 1, 2。通路と壁は 0。
    @Test
    def testRoomsAreNumberedAndCorridorsAreZero(): Unit \ Assert =
        let b = Fixtures.board("#########" :: "#@.,,,.>#" :: "#########" :: Nil);
        Assert.assertEq(expected = (1, 1, 0, 0, 2, 2, 0),
            (Board.roomAt((1, 1), b), Board.roomAt((2, 1), b), Board.roomAt((3, 1), b), Board.roomAt((5, 1), b),
             Board.roomAt((6, 1), b), Board.roomAt((7, 1), b), Board.roomAt((0, 1), b)))

    /// 部屋の中から見たことにするのは部屋と囲む壁、通路からは 8 近傍。
    @Test
    def testViewFromRoomCoversItsWallsAndCorridorSeesNeighbors(): Unit \ Assert =
        let b = Fixtures.board("#######" :: "#@.,,>#" :: "#######" :: Nil);
        Assert.assertEq(expected = (12, 9),
            (Set.size(Board.viewFrom((1, 1), b)), Set.size(Board.viewFrom((4, 1), b))))

    /// 歩ける道の幅優先: 柱を回り込んだ歩数。
    @Test
    def testWalkDistanceGoesAroundPillars(): Unit \ Assert =
        let b = Fixtures.board("#####" :: "#@P>#" :: "#...#" :: "#####" :: Nil);
        Assert.assertEq(expected = Some(4), Map.get((3, 1), Board.walkDistances((1, 1), b)))
}
