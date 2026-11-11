// 予告: 出来事の要約の表（summarize）と、敵の予告に無いマスに立った手では受けるダメージが 0 になる性質（App の道を種で流す）。
mod TestPreview {

    use fge::{App, Clock, PropertyTest, Resources}
    use fge::GameEngine.Key
    use Board.{Dir, Dir8}
    use Run.Event
    use Preview.Mark

    // ══════════════════════════════════════════════════════
    // 要約の表
    // ══════════════════════════════════════════════════════

    /// 進めない → ×・殴って殴られる → 殴る + ダメージ・弾いた攻撃も盾で減らした後の数で数える・動いて矢と薬 → 移動 + 矢 + 薬・倒れる。
    @Test
    def testSummarizeTable(): Unit \ Assert =
        Assert.assertEq(
            expected = (Mark.Blocked :: Nil,
                        Mark.Strike :: Mark.Damage(3) :: Nil,
                        Mark.Wait :: Mark.Damage(2) :: Nil,
                        Mark.Move :: Mark.Arrow :: Mark.Potion :: Mark.Damage(4) :: Nil,
                        Mark.Wait :: Mark.Bomb :: Mark.Damage(5) :: Mark.Falls :: Nil,
                        Mark.Descend :: Nil),
            (Preview.summarize(Event.HeroBlocked(Dir8.Up) :: Nil),
             Preview.summarize(Event.HeroStruck(0, 2) :: Event.HeroHurt(Run.Source.Melee(0), 3, false) :: Nil),
             Preview.summarize(Event.HeroWaited :: Event.HeroHurt(Run.Source.Melee(1), 2, true) :: Nil),
             Preview.summarize(Event.HeroMoved((1, 1), (2, 1)) :: Event.ArrowShot(Dir.Right, Item.Kind.WoodArrow, (4, 1))
                 :: Event.HeroHurt(Run.Source.Melee(0), 2, false) :: Event.PotionUsed(Item.Kind.Herb, 6) :: Event.HeroHurt(Run.Source.Melee(1), 2, false) :: Nil),
             Preview.summarize(Event.HeroWaited :: Event.BombThrown(Item.Kind.SmallBomb, (1, 1)) :: Event.HeroHurt(Run.Source.Melee(0), 5, false) :: Event.HeroDied :: Nil),
             Preview.summarize(Event.Descended :: Nil)))

    /// 方向の予告は本物の step の要約と同じ（9 つの入力）。
    @Test
    def testPreviewIsTheSummaryOfStep(): Unit \ Assert =
        let c = Fixtures.ctx(Fixtures.hall());
        let first = Fixtures.start(c, (Enemy.Kind.Jelly, (2, 3)) :: (Enemy.Kind.Archer, (1, 5)) :: Nil, Nil);
        Assert.assertEq(expected = Run.inputs() |> List.map(input -> (input, Preview.summarize(snd(Step.step(c, first, input))))),
            Preview.preview(c, first))

    // ══════════════════════════════════════════════════════
    // 敵の予告の性質（種で流す）
    // ══════════════════════════════════════════════════════

    type alias Game = Resources.With[Res.Res, World.World]

    def setup(): PropertyTest.Setup[Key, Game] =
        { keyOf = (key, _) -> key, viewport = { x = World.width(), y = World.height() }, dt = 1.0 / 60.0 }

    /// 種 12 個 × 400 入力。矢印・斜めの 4 つと待つ（倒れた・抜けた後は Enter で題名へ・題名からまた始める）。
    def cases(): PropertyTest.Cases[Key] =
        { seeds = 12, length = 400,
          spec = { taps = List#{Key.Left, Key.Right, Key.Up, Key.Down, Key.Q, Key.E, Key.Z, Key.C, Key.Space, Key.Enter},
                   idle = 1 | PropertyTest.emptySpec() } }

    def app(): App.App[Game, {}] \ Fs.FileRead = Game.app(Controls.loadDocs(), World.fixedSeed())

    /// 周回の種 seed から 1F を始めた所（地図は種で決まる）。
    def started(seed: Int64): Game \ Fs.FileRead =
        let res = Res.build(Controls.loadDocs());
        Resources.of(res, World.begin(res, World.initial(res, seed)))

    /// そのフレームに新しい手を見せ始め、その手で主人公が攻撃を受けたか。
    def hurtIn(step: App.TraceStep[Game]): Bool =
        let (before, after) = (step#before#world#shown, step#after#world#shown);
        Clock.since(before#startedAt, after#startedAt) != 0.0 and List.exists(Run.hurtsHero, after#events)

    /// そのフレームの手で主人公が攻撃を受けたなら、主人公の立ったマスは手の前の敵の予告に入っていた。
    def safeOutsideDanger(step: App.TraceStep[Game]): Option[String] =
        let (before, after) = (step#before, step#after);
        let hurt = hurtIn(step);
        let ctx = World.ctx(before#resources, before#world);
        let danger = Preview.dangerCells(ctx, before#world#runState);
        let at = Run.heroOf(after#world#runState)#at;
        if (before#world#screen != World.Screen.Play or not hurt or Set.memberOf(at, danger)) None
        else Some("予告に無い ${at} で攻撃を受けた: ${Run.fingerprint(before#world#runState)}")

    /// 予告に無いマスに立った手では、主人公は攻撃を受けない（1F の地図 2 枚とも）。
    @Test
    def testNoDamageOutsideTheDangerCells(): Unit \ Assert + Fs.FileRead =
        let a = app();
        Assert.assertEq(expected = (None, None),
            (PropertyTest.forAll(a, setup(), cases(), started(World.fixedSeed()), safeOutsideDanger),
             PropertyTest.forAll(a, setup(), cases(), started(8i64), safeOutsideDanger)))

    /// 上の性質が空振りでないこと: 2 つの始まりは地図が違い、どれかの種の列で主人公が攻撃を受けている。
    @Test
    def testPropertyRunsOnBothMapsAndSeesDamage(): Unit \ Assert + Fs.FileRead =
        let a = app();
        let starts = started(World.fixedSeed()) :: started(8i64) :: Nil;
        let seen = starts |> List.exists(begin -> List.range(1, cases()#seeds + 1) |> List.exists(seed ->
            PropertyTest.traceOf(a, setup(), PropertyTest.generate(Int32.toInt64(seed), cases()#length, cases()#spec), begin) |> List.exists(hurtIn)));
        Assert.assertEq(expected = (0 :: 1 :: Nil, true),
            (starts |> List.map(g -> Run.rec(g#world#runState)#layout), seen))
}
