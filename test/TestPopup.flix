// 浮かぶ数と HP の棒とカードの押し所: 出来事ごとの数の種類・同じマスの数は上へずれる・盾の 0 は灰色の 0・
// 見せ終えたことにした数は今へ早まる・HP の減った分は残してから縮む・カードはその位置のタップで選ぶ。
// 出来事の列は手で組む（Step を通さず、見せ方だけを確かめる）。盤は Fixtures.hall（主人公は (1, 3)）。
mod TestPopup {

    use fge::Clock
    use Run.{Event, Source}
    use Fixtures.{hall, start}

    def timing(): Popup.Timing = { life = 0.6, hold = 0.35, shrink = 0.3 }

    /// 主人公の右 (2, 3) にゼリーがいる手の前後と出来事から、今 (時計の頭) を始まりに作った数。
    def numbers(events: List[Event]): List[Popup.Popup] =
        let c = Fixtures.ctx(hall());
        let r = Run.rec(start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil));
        let tl = TurnTimeline.build(TurnTimeline.paceOf(LookDoc.rec(LookDoc.defaults()), false), events);
        Popup.fromTurn({ before = r, after = r, events = events }, tl, Clock.zero(), Nil)

    /// 殴って倒すと、与えた数と経験値の数がゼリーのマスに出て、経験値は 1 つ上へずれる。
    @Test
    def testExpSitsAboveTheDealtNumberOnTheEnemyCell(): Unit \ Assert =
        let made = numbers(Event.HeroStruck(0, 2) :: Event.EnemyDied(0, Enemy.Kind.Jelly, (2, 3)) :: Event.ExpGained(3) :: Nil);
        Assert.assertEq(expected = (Popup.Kind.Dealt, (2, 3), 0) :: (Popup.Kind.Exp, (2, 3), 1) :: Nil,
            made |> List.map(pop -> (pop#kind, pop#cell, pop#lift)))

    /// 盾で 0 になった攻撃も弾いた攻撃も、灰色の 0 になる。
    @Test
    def testGuardedAndDeflectedHitsShowZero(): Unit \ Assert =
        let made = numbers(Event.HeroHurt(Source.Melee(0), 0, false) :: Event.HeroHurt(Source.Melee(0), 3, true) :: Nil);
        Assert.assertEq(expected = (Popup.Kind.Blocked, 0) :: (Popup.Kind.Blocked, 0) :: Nil, made |> List.map(pop -> (pop#kind, pop#amount)))

    /// 空腹で減った数と、薬で戻った数は主人公のマスに出る。
    @Test
    def testStarveAndPotionShowOnTheHero(): Unit \ Assert =
        let made = numbers(Event.HeroHurt(Source.Starve, 1, false) :: Event.PotionUsed(Item.Kind.Herb, 6) :: Nil);
        Assert.assertEq(expected = (Popup.Kind.Starved, (1, 3)) :: (Popup.Kind.Healed, (1, 3)) :: Nil, made |> List.map(pop -> (pop#kind, pop#cell)))

    /// 見せ終えたことにした手の、まだ始まっていない数は今に早まる。
    @Test
    def testHurryBringsLaterNumbersToNow(): Unit \ Assert =
        let made = numbers(Event.HeroStruck(0, 2) :: Event.HeroHurt(Source.Melee(0), 2, false) :: Nil);
        Assert.assertEq(expected = true, Popup.hurry(Clock.zero(), made) |> List.forAll(pop -> Clock.since(pop#startedAt, Clock.zero()) == 0.0))

    /// HP 10 から 6 へ減ると、待つ間は減った分を 10 まで残し、縮み終えると 6 になる。
    @Test
    def testLostPartIsHeldThenShrinks(): Unit \ Assert =
        let trail = { before = 10.0, low = 6, to = 6, hitAt = Clock.zero(), healAt = None };
        let extraAt = (seconds: Float64) -> Popup.barOf(timing(), Clock.after(seconds, Clock.zero()), trail)#extra;
        Assert.assertEq(expected = (10.0, 6.0), (extraAt(0.1), extraAt(1.0)))

    /// カードはその位置のタップで選び、カードの外は選ばない。
    @Test
    def testTapOnACardPicksIt(): Unit \ Assert =
        let r = CardView.rectOf(4, 2);
        let middle = { x = r#at#x + r#size#x / 2.0, y = r#at#y + r#size#y / 2.0 };
        Assert.assertEq(expected = (Some(2), None), (CardView.cardAt(4, middle), CardView.cardAt(4, { x = 270.0, y = 100.0 })))
}
