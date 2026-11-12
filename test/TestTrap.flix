// 罠（毒矢）: 歩いて踏むと主人公に trapDamage（盾は効かない）・倒れたらその手で終わる・押し出された敵にも効く・歩いて乗った敵には効かない・
// 方向の予告に出る・ダッシュは罠の手前で止まる。盤は文字で書いた小さな盤で、^ が毒矢。
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

    /// 敵は自分で歩いて毒矢に乗っても何も起きない（ゼリーが寄って 2 手目に罠のマスへ）。
    @Test
    def testWalkingEnemyIgnoresTheDart(): Unit \ Assert =
        let c = ctx("#######" :: "#@.^..#" :: "##>####" :: "#######" :: Nil);
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (5, 1)) :: Nil, Nil), Input.Wait :: Input.Wait :: Nil);
        Assert.assertEq(expected = ((0, (3, 1)) :: Nil, Some(EnemiesDoc.statsOf(Enemy.Kind.Jelly, EnemiesDoc.defaults())#hp)),
            (Fixtures.enemyCells(after), Run.enemiesOf(after) |> List.head |> Option.map(e -> e#hp)))

    // ══════════════════════════════════════════════════════
    // ダッシュ
    // ══════════════════════════════════════════════════════

    /// ダッシュは次のマスが罠なら、その手前で止まる。
    @Test
    def testDashStopsBeforeADart(): Unit \ Assert =
        let c = ctx("#########" :: "#@....^>#" :: "#########" :: Nil);
        Assert.assertEq(expected = Some((5, 1)), Dash.dash(c, start(c, Nil, Nil), Board.Dir.Right) |> List.last |> Option.map(e -> Run.heroOf(fst(e))#at))
}
