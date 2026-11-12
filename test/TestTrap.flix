// 罠（毒矢・警報）: 歩いて踏むと主人公に trapDamage（盾は効かない）・倒れたらその手で終わる・押し出された敵にも歩いて乗った敵にも効く・
// 踏まれるまで見えない（見えない罠は方向の予告に出ず、踏むと見える）・警報は階の始めの眠りの敵だけを起こしかける・
// ダッシュは見えている罠の手前で止まる。盤は文字で書いた小さな盤で、^ が毒矢・! が警報。
mod TestTrap {

    use Board.Dir8
    use Run.{Event, Input, Source}
    use Preview.Mark
    use Fixtures.{ctx, start}

    def trapDamage(): Int32 = RulesDoc.rec(RulesDoc.defaults())#trapDamage

    def dart(): Trap.Kind = Trap.Kind.PoisonDart

    // ══════════════════════════════════════════════════════
    // 主人公が踏む
    // ══════════════════════════════════════════════════════

    /// 毒矢を踏むと trapDamage を受ける。木の盾を持っていても減らない。方向の予告は「移動・毒矢・ダメージ」。
    @Test
    def testSteppingOnADartHurtsThroughTheShield(): Unit \ Assert =
        let c = ctx("######" :: "#@^.>#" :: "######" :: Nil);
        let r = Run.rec(start(c, Nil, Nil));
        let shielded = Run.of({ hero = { shield = Some(Item.Kind.WoodShield) | r#hero } | r });
        let (after, events) = Step.step(c, shielded, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (RulesDoc.rec(RulesDoc.defaults())#maxHp - trapDamage(), Mark.Move :: Mark.Trap(dart()) :: Mark.Damage(trapDamage()) :: Nil),
            (Run.heroOf(after)#hp, Preview.summarize(events)))

    /// 毒矢で倒れた手は、そこで終わる（隣の眠った敵は起きず、殴りもしない）。
    @Test
    def testFallingOnADartEndsTheTurn(): Unit \ Assert =
        let c = ctx("######" :: "#@^..#" :: "#...>#" :: "######" :: Nil);
        let r = Run.rec(start(c, (Enemy.Kind.Jelly, (3, 1)) :: Nil, Nil));
        let weak = Run.of({ hero = { hp = trapDamage() | r#hero } | r });
        let (after, events) = Step.step(c, weak, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (Run.Status.Dead, Event.HeroMoved((1, 1), (2, 1)) :: Event.TrapSprung(dart(), (2, 1)) :: Event.HeroHurt(Source.Trap(dart()), trapDamage(), false) :: Event.HeroDied :: Nil),
            (Run.statusOf(after), events))

    /// 罠のマスで待っても、もう一度は動かない（歩いて入った手だけ）。
    @Test
    def testWaitingOnADartDoesNothing(): Unit \ Assert =
        let c = ctx("######" :: "#@^.>#" :: "######" :: Nil);
        let (after, _) = Step.steps(c, start(c, Nil, Nil), Input.Move(Dir8.Right) :: Input.Wait :: Nil);
        Assert.assertEq(expected = RulesDoc.rec(RulesDoc.defaults())#maxHp - trapDamage(), Run.heroOf(after)#hp)

    // ══════════════════════════════════════════════════════
    // 敵と罠
    // ══════════════════════════════════════════════════════

    /// 銀の剣で殴って押し出した敵が毒矢に乗ると trapDamage を受ける（HP 20 のゼリー: 20 - 銀の剣の攻撃 - trapDamage）。
    @Test
    def testPushedEnemyIsHurtByTheDart(): Unit \ Assert =
        let base = ctx("#######" :: "#@.^..#" :: "#....>#" :: "#######" :: Nil);
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 20}}}") | base };
        let r = Run.rec(start(c, (Enemy.Kind.Jelly, (2, 1)) :: Nil, Nil));
        let armed = Run.of({ hero = { sword = Some(Item.Kind.SilverSword) | r#hero } | r });
        let (after, events) = Step.step(c, armed, Input.Move(Dir8.Right));
        let silver = ItemsDoc.statsOf(Item.Kind.SilverSword, ItemsDoc.defaults())#attack;
        Assert.assertEq(expected = ((0, (3, 1)) :: Nil, Some(20 - silver - trapDamage()), true),
            (Fixtures.enemyCells(after), Run.enemiesOf(after) |> List.head |> Option.map(e -> e#hp), List.memberOf(Event.EnemyTrapped(0, trapDamage()), events)))

    /// 敵も歩いて毒矢に乗ると trapDamage を受け、その罠は見えるようになる（ゼリーが寄って 2 手目に罠のマスへ）。
    @Test
    def testWalkingEnemyStepsOnTheDart(): Unit \ Assert =
        let c = ctx("#######" :: "#@.^..#" :: "##>####" :: "#######" :: Nil);
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (5, 1)) :: Nil, Nil), Input.Wait :: Input.Wait :: Nil);
        Assert.assertEq(expected = ((0, (3, 1)) :: Nil, Some(EnemiesDoc.statsOf(Enemy.Kind.Jelly, EnemiesDoc.defaults())#hp - trapDamage()), Set#{(3, 1)}),
            (Fixtures.enemyCells(after), Run.enemiesOf(after) |> List.head |> Option.map(e -> e#hp), Run.rec(after)#revealed))

    // ══════════════════════════════════════════════════════
    // 見えない罠
    // ══════════════════════════════════════════════════════

    /// 踏まれていない罠は方向の予告に出ず（右へ動くは「移動」だけ）、踏むと見える。
    @Test
    def testHiddenDartIsNotForetoldAndShowsOnceStepped(): Unit \ Assert =
        let c = ctx("######" :: "#@^.>#" :: "######" :: Nil);
        let first = start(c, Nil, Nil);
        let (after, _) = Step.step(c, first, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (Some(Mark.Move :: Nil), Map#{}, Map#{(2, 1) => dart()}),
            (Preview.preview(c, first) |> List.find(e -> fst(e) == Input.Move(Dir8.Right)) |> Option.map(snd),
             Run.seenTraps(Run.rec(first)), Run.seenTraps(Run.rec(after))))

    /// 見えている罠は方向の予告に出る（移動・毒矢・ダメージ）。
    @Test
    def testSeenDartIsForetold(): Unit \ Assert =
        let c = ctx("######" :: "#@^.>#" :: "######" :: Nil);
        let first = Fixtures.revealing((2, 1) :: Nil, start(c, Nil, Nil));
        Assert.assertEq(expected = Some(Mark.Move :: Mark.Trap(dart()) :: Mark.Damage(trapDamage()) :: Nil),
            Preview.preview(c, first) |> List.find(e -> fst(e) == Input.Move(Dir8.Right)) |> Option.map(snd))

    // ══════════════════════════════════════════════════════
    // 警報
    // ══════════════════════════════════════════════════════

    /// 警報を踏むと、階の始めの眠りの敵だけが Dozing(1) になる（煙玉で眠った Dozing(3) の敵は縮めない）。その手は誰も動かず被ダメージは 0。
    @Test
    def testAlarmWakesOnlyTheDormant(): Unit \ Assert =
        let c = ctx("#########" :: "#@!.....#" :: "#.......#" :: "#......>#" :: "#########" :: Nil);
        let dozing = Fixtures.resting(1, Enemy.Rest.Dozing(3), start(c, (Enemy.Kind.Jelly, (7, 1)) :: (Enemy.Kind.Jelly, (7, 2)) :: Nil, Nil));
        let (after, events) = Step.step(c, dozing, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (List.memberOf(Event.AlarmRang(0 :: Nil), events), (0, Enemy.Rest.Awake) :: (1, Enemy.Rest.Dozing(2)) :: Nil, Fixtures.enemyCells(dozing), Mark.Move :: Mark.Trap(Trap.Kind.Alarm) :: Nil),
            (true, Fixtures.enemyRests(after), Fixtures.enemyCells(after), Preview.summarize(events)))

    /// 敵が警報を踏んでも鳴り、ほかの眠った敵が起きかける（ゼリーが寄って 2 手目に警報のマスへ。壁の向こうの弓兵は気付かない所にいる）。
    @Test
    def testEnemyRingsTheAlarm(): Unit \ Assert =
        let c = ctx("###############" :: "#@.!..#.......#" :: "##>############" :: "###############" :: Nil);
        let (after, events) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (5, 1)) :: (Enemy.Kind.Archer, (13, 1)) :: Nil, Nil), Input.Wait :: Input.Wait :: Nil);
        Assert.assertEq(expected = (true, Set#{(3, 1)}, (0, Enemy.Rest.Awake) :: (1, Enemy.Rest.Awake) :: Nil),
            (List.memberOf(Event.AlarmRang(1 :: Nil), events), Run.rec(after)#revealed, Fixtures.enemyRests(after)))

    // ══════════════════════════════════════════════════════
    // ダッシュ
    // ══════════════════════════════════════════════════════

    /// ダッシュは次のマスが見えている罠なら、その手前で止まる。見えていない罠は踏んで、そこで止まる。
    @Test
    def testDashStopsBeforeASeenDart(): Unit \ Assert =
        let c = ctx("#########" :: "#@....^>#" :: "#########" :: Nil);
        let lastAt = (runState: Run.State) -> Dash.dash(c, runState, Board.Dir.Right) |> List.last |> Option.map(e -> Run.heroOf(fst(e))#at);
        Assert.assertEq(expected = (Some((5, 1)), Some((6, 1))), (lastAt(Fixtures.revealing((6, 1) :: Nil, start(c, Nil, Nil))), lastAt(start(c, Nil, Nil))))
}
