// タップの読み方（押したコマで取る・同じコマのスワイプ・UI の帯・床との交点の向き）と、見せている間の手の扱い（溜める・ダッシュを止める・1 歩ずつ進む）。
mod TestTap {

    use fge::{App, Camera3d, Clock, Resources, Vec2}
    use Board.{Dir, Dir8}
    use Run.Input

    // ══════════════════════════════════════════════════════
    // 押したコマ
    // ══════════════════════════════════════════════════════

    def pointer(clicked: Bool, x: Float64, y: Float64, dashed: Bool): Tap.Pointer =
        { clicked = clicked, cursor = { x = x, y = y }, dashed = dashed }

    def xy(point: Option[Vec2.Vec2]): Option[(Float64, Float64)] = point |> Option.map(q -> (q#x, q#y))

    /// 押したコマで、押した所がそのままタップの点になる（離すのを待たない）。
    @Test
    def testTapIsTakenOnThePressFrame(): Unit \ Assert =
        Assert.assertEq(expected = Some((100.0, 200.0)), xy(Tap.pressedAt(pointer(true, 100.0, 200.0, false))))

    /// 押していないコマ（押したまま・離した）はタップにならない。
    @Test
    def testFramesWithoutAPressAreNotTaps(): Unit \ Assert =
        Assert.assertEq(expected = None, xy(Tap.pressedAt(pointer(false, 100.0, 200.0, false))))

    /// 同じコマにダッシュの操作（スワイプ）が来たら、タップは捨てる。
    @Test
    def testSwipeInTheSameFrameDiscardsTheTap(): Unit \ Assert =
        Assert.assertEq(expected = None, xy(Tap.pressedAt(pointer(true, 100.0, 200.0, true))))

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
            Tap.inputOf({ camera = camera, size = size(), bandTop = HudView.bandTop() }, waitOf(res, camera, hero), hero));
        Assert.assertEq(expected = (Some(Input.Wait), Some(Input.Move(Dir8.Up)), Some(Input.Move(Dir8.Right))),
            (tap(hero), tap(Board.step(Dir.Up, hero)), tap(Board.step(Dir.Right, hero))))

    // ══════════════════════════════════════════════════════
    // 見せている間の手
    // ══════════════════════════════════════════════════════

    def started(): (Res.Res, World.World) \ Fs.FileRead =
        let res = Res.build(Controls.loadDocs());
        (res, World.begin(res, World.initial(res, World.fixedSeed())))

    def heroAt(w: World.World): Board.Cell = Run.heroOf(w#runState)#at

    /// 1 歩見せている間の 2 歩目は溜まり、今の 1 歩を飛ばしてから当たる（始まりから右・上）。
    @Test
    def testInputWhileShowingIsQueuedThenApplied(): Unit \ Assert + Fs.FileRead =
        let (res, w) = started();
        let right = Board.step(Dir.Right, heroAt(w));
        let walking = World.command(res, World.Act.Go(Input.Move(Dir8.Right)), w);
        let queued = World.command(res, World.Act.Go(Input.Move(Dir8.Up)), walking);
        Assert.assertEq(expected = (right, true, Board.step(Dir.Up, right)), (heroAt(queued), Option.nonEmpty(queued#queued), heroAt(World.advance(res, queued))))

    /// 部屋の中の右へのダッシュ: 始めた時は 1 歩目の Run、見せ終えるごとに次の歩へ進む。
    @Test
    def testDashShowsOneStepAtATime(): Unit \ Assert + Fs.FileRead =
        let (res, w) = started();
        let right = Board.step(Dir.Right, heroAt(w));
        let dashing = World.command(res, World.Act.Dash(Dir.Right), w);
        Assert.assertEq(expected = (right, Board.step(Dir.Right, right)), (heroAt(dashing), heroAt(World.advance(res, World.skipShow(dashing)))))

    /// ダッシュ中の手は溜めずに、次の歩で止める。
    @Test
    def testInputDuringDashStopsAtTheNextStep(): Unit \ Assert + Fs.FileRead =
        let (res, w) = started();
        let stopped = World.command(res, World.Act.Go(Input.Move(Dir8.Up)), World.command(res, World.Act.Dash(Dir.Right), w));
        Assert.assertEq(expected = (Board.step(Dir.Right, heroAt(w)), false), (heroAt(World.advance(res, World.skipShow(stopped))), World.isBusy(res, World.skipShow(stopped))))

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

    def waitOf(res: Res.Res, camera: Camera3d.Camera3d, hero: Board.Cell): Tap.Wait =
        { radius = RulesDoc.rec(res#rules)#tapWaitRadius, px = RulesDoc.rec(res#rules)#tapWaitPx,
          spot = Tap.waitSpotOf(camera, size(), LookDoc.rec(res#look)#waitBadgeLift, hero) }

    /// 主人公の体の上（待つ・降りるのアイコン）をタップすると、床へ写すと上のマスに当たる点でも待つになる。
    @Test
    def testTapOnTheHerosBodyWaits(): Unit \ Assert + Fs.FileRead =
        let res = Res.build(Controls.loadDocs());
        let w = World.begin(res, World.initial(res, World.fixedSeed()));
        let camera = World.cameraOf(res, w);
        let hero = Run.heroOf(w#runState)#at;
        let wait = waitOf(res, camera, hero);
        let onBody = wait#spot |> Option.flatMap(Tap.inputOf({ camera = camera, size = size(), bandTop = HudView.bandTop() }, wait, hero));
        Assert.assertEq(expected = Some(Input.Wait), onBody)
}
