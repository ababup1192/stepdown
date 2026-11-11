// タップの読み方（押しっぱなし・動いた距離・同じコマのスワイプ・UI の帯・床との交点の向き）と、見せている間の手の扱い（溜める・ダッシュを止める・1 歩ずつ進む）。
mod TestTap {

    use fge::{App, Camera3d, Clock, Resources, Vec2}
    use Board.{Dir, Dir8}
    use Run.Input

    // ══════════════════════════════════════════════════════
    // 押してから離すまで
    // ══════════════════════════════════════════════════════

    def pointer(clicked: Bool, down: Bool, x: Float64, y: Float64, dashed: Bool): Tap.Pointer =
        { clicked = clicked, down = down, cursor = { x = x, y = y }, now = Clock.zero(), dashed = dashed }

    def xy(point: Option[Vec2.Vec2]): Option[(Float64, Float64)] = point |> Option.map(q -> (q#x, q#y))

    def slop(): Float64 = RulesDoc.rec(RulesDoc.defaults())#tapSlop

    /// 押して → 押したまま → 離す（押した所から slop の半分だけ動いた）の 3 コマ。確定は離したコマだけで、点は押した所。
    @Test
    def testTapIsConfirmedOnTheReleaseFrame(): Unit \ Assert =
        let (held, first) = Tap.track(slop(), pointer(true, true, 100.0, 200.0, false), None);
        let (kept, second) = Tap.track(slop(), pointer(false, true, 100.0, 200.0, false), held);
        let (_, third) = Tap.track(slop(), pointer(false, false, 100.0 + slop() / 2.0, 200.0, false), kept);
        Assert.assertEq(expected = (None, None, Some((100.0, 200.0))), (xy(first), xy(second), xy(third)))

    /// 押した・離したが同じコマでも確定する。
    @Test
    def testPressAndReleaseInOneFrame(): Unit \ Assert =
        Assert.assertEq(expected = Some((50.0, 60.0)), xy(snd(Tap.track(slop(), pointer(true, false, 50.0, 60.0, false), None))))

    /// 押したまま slop より遠くへ動いた（横の払い）後は、離しても確定しない。
    @Test
    def testDragBeyondSlopIsDiscarded(): Unit \ Assert =
        let (held, _) = Tap.track(slop(), pointer(true, true, 100.0, 200.0, false), None);
        let (dragged, _) = Tap.track(slop(), pointer(false, true, 100.0 + slop() * 2.0, 200.0, false), held);
        let (_, released) = Tap.track(slop(), pointer(false, false, 100.0, 200.0, false), dragged);
        Assert.assertEq(expected = None, xy(released))

    /// 離したコマにダッシュの操作（スワイプ）が来たら、タップは捨てる。
    @Test
    def testSwipeInTheSameFrameDiscardsTheTap(): Unit \ Assert =
        let (held, _) = Tap.track(slop(), pointer(true, true, 100.0, 200.0, false), None);
        Assert.assertEq(expected = None, xy(snd(Tap.track(slop(), pointer(false, false, 100.0, 200.0, true), held))))

    // ══════════════════════════════════════════════════════
    // 点から入力へ
    // ══════════════════════════════════════════════════════

    /// 主人公 (5, 5) と床の点: 半径の中は待つ、軸から 22.5° 以内は上下左右、それより斜めは斜め（北が上・行が増えると下）。
    @Test
    def testInputTowardSplitsIntoEightDirections(): Unit \ Assert =
        let r = RulesDoc.rec(RulesDoc.defaults())#tapWaitRadius;
        let toward = Tap.inputToward(r, (5, 5));
        Assert.assertEq(
            expected = (Input.Wait, Input.Move(Dir8.Right), Input.Move(Dir8.Up), Input.Move(Dir8.Left), Input.Move(Dir8.Down),
                        Input.Move(Dir8.DownRight), Input.Move(Dir8.UpLeft), Input.Move(Dir8.UpRight), Input.Move(Dir8.DownLeft)),
            (toward((5.0 + r / 2.0, 5.0)), toward((7.0, 5.5)), toward((5.5, 3.0)), toward((2.0, 4.5)), toward((5.0, 8.0)),
             toward((7.0, 7.0)), toward((3.0, 3.0)), toward((7.0, 4.0)), toward((4.0, 6.5))))

    /// 下の UI の帯の中は盤の入力にしない。
    @Test
    def testBottomBandIsNotTheBoard(): Unit \ Assert =
        Assert.assertEq(expected = (true, false),
            (Tap.isOnBoard(HudView.bandTop(), { x = 270.0, y = HudView.bandTop() - 10.0 }),
             Tap.isOnBoard(HudView.bandTop(), { x = 270.0, y = HudView.bandTop() + 10.0 })))

    def size(): Vec2.Vec2 = { x = World.width(), y = World.height() }

    /// 本物のカメラで、主人公のマス・上のマス・右のマスの真ん中を画面へ写してタップすると、待つ・上・右になる。
    @Test
    def testTapOnProjectedCellsGivesTheirDirection(): Unit \ Assert + Fs.FileRead =
        let res = Res.build(Controls.loadDocs());
        let w = World.begin(res, World.initial(res, World.fixedSeed()));
        let camera = World.cameraOf(res, w);
        let hero = Run.heroOf(w#runState)#at;
        let tap = (cell: Board.Cell) -> Camera3d.worldToScreen(camera, size(), FloorMesh.cellPoint(cell)) |> Option.flatMap(
            Tap.inputOf({ camera = camera, size = size(), bandTop = HudView.bandTop() }, RulesDoc.rec(res#rules)#tapWaitRadius, hero));
        Assert.assertEq(expected = (Some(Input.Wait), Some(Input.Move(Dir8.Up)), Some(Input.Move(Dir8.Right))),
            (tap(hero), tap(Board.step(Dir.Up, hero)), tap(Board.step(Dir.Right, hero))))

    // ══════════════════════════════════════════════════════
    // 見せている間の手
    // ══════════════════════════════════════════════════════

    def started(): (Res.Res, World.World) \ Fs.FileRead =
        let res = Res.build(Controls.loadDocs());
        (res, World.begin(res, World.initial(res, World.fixedSeed())))

    def heroAt(w: World.World): Board.Cell = Run.heroOf(w#runState)#at

    /// 1 歩見せている間の 2 歩目は溜まり、今の 1 歩を飛ばしてから当たる（始まり (3, 3) から右・上で (4, 2)）。
    @Test
    def testInputWhileShowingIsQueuedThenApplied(): Unit \ Assert + Fs.FileRead =
        let (res, w) = started();
        let walking = World.command(res, World.Act.Go(Input.Move(Dir8.Right)), w);
        let queued = World.command(res, World.Act.Go(Input.Move(Dir8.Up)), walking);
        Assert.assertEq(expected = ((4, 3), true, (4, 2)), (heroAt(queued), Option.nonEmpty(queued#queued), heroAt(World.advance(res, queued))))

    /// 部屋の中の右へのダッシュは壁まで 2 歩: 始めた時は 1 歩目の Run、見せ終えるごとに次の歩へ進む。
    @Test
    def testDashShowsOneStepAtATime(): Unit \ Assert + Fs.FileRead =
        let (res, w) = started();
        let dashing = World.command(res, World.Act.Dash(Dir.Right), w);
        Assert.assertEq(expected = ((4, 3), (5, 3)), (heroAt(dashing), heroAt(World.advance(res, World.skipShow(dashing)))))

    /// ダッシュ中の手は溜めずに、次の歩で止める。
    @Test
    def testInputDuringDashStopsAtTheNextStep(): Unit \ Assert + Fs.FileRead =
        let (res, w) = started();
        let stopped = World.command(res, World.Act.Go(Input.Move(Dir8.Up)), World.command(res, World.Act.Dash(Dir.Right), w));
        Assert.assertEq(expected = ((4, 3), false), (heroAt(World.advance(res, World.skipShow(stopped))), World.isBusy(res, World.skipShow(stopped))))

    // ══════════════════════════════════════════════════════
    // 音（時刻表の Cue）
    // ══════════════════════════════════════════════════════

    /// 1 歩を当てたコマに歩く音が 1 回鳴り、次のコマ（1/60 秒後）には鳴り直さない。
    @Test
    def testStepSoundRingsOnceAtTheStartOfTheTurn(): Unit \ Assert + Fs.FileRead =
        let app = Game.app(Controls.loadDocs(), World.fixedSeed());
        let (res, w) = started();
        let ticked = World.command(res, World.Act.Go(Input.Move(Dir8.Right)), App.tickClocks(app, 1.0 / 60.0, Resources.of(res, w))#world);
        let later = App.tickClocks(app, 1.0 / 60.0, Resources.of(res, ticked))#world;
        Assert.assertEq(expected = ("sd_step" :: Nil, Nil), (World.sounds(res, ticked), World.sounds(res, later)))
}
