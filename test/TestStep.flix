// 1 手の決まり（Step）: 殴る・進めない・拾う → 判定・矢・爆弾・階段・敵が起きる・敵の手番・盾・薬・倒れる・自然回復。
// 盤は Fixtures.hall（7×5 の 1 部屋。主人公は (1, 3)）か、文字で書いた小さな盤。敵は同じ部屋なら最初の手の後に起きる。
mod TestStep {

    use Board.Dir
    use Run.{Event, Input}
    use Preview.Mark
    use Fixtures.{ctx, hall, start}

    def rules(): RulesDoc.Rec = RulesDoc.rec(RulesDoc.defaults())

    def jellyAttack(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Jelly, EnemiesDoc.defaults())#attack

    def stats(kind: Item.Kind): ItemsDoc.Stats = ItemsDoc.statsOf(kind, ItemsDoc.defaults())

    def right(): Input = Input.Move(Dir.Right)

    // ══════════════════════════════════════════════════════
    // 主人公の手
    // ══════════════════════════════════════════════════════

    /// 敵のいるマスへ動くと殴る（移動しない）。起きた敵はそのまま隣から殴り返す。
    @Test
    def testMovingIntoAnEnemyStrikesWithoutMoving(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil), right());
        Assert.assertEq(expected = ((1, 3), Event.HeroStruck(0, rules()#baseAttack) :: Event.EnemyWoke(0) :: Event.HeroHurt(0, jellyAttack(), false) :: Nil),
            (Run.heroOf(after)#at, events))

    /// 歩けないマスへの手は何も起きない（出来事は進めないだけ・移動の数も増えない）。
    @Test
    def testBlockedMoveDoesNotAdvance(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, start(c, Nil, Nil), Input.Move(Dir.Left));
        Assert.assertEq(expected = (Event.HeroBlocked(Dir.Left) :: Nil, 0), (events, Run.rec(after)#walked))

    /// 動いた先のアイテムを拾ってから判定する: 拾った矢で、距離 3 の敵を撃つ。
    @Test
    def testPickUpComesBeforeTheArrow(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (5, 3)) :: Nil, (Item.Kind.WoodArrow, (2, 3)) :: Nil), right());
        Assert.assertEq(
            expected = Event.HeroMoved((1, 3), (2, 3)) :: Event.Picked(Item.Kind.WoodArrow, Item.Pick.Upgraded)
                :: Event.ArrowShot(Dir.Right, Item.Kind.WoodArrow, (5, 3)) :: Event.ArrowHit(0, stats(Item.Kind.WoodArrow)#damage) :: Nil,
            List.take(4, events))

    /// 階段の上で待つと降りる（敵は動かない）。
    @Test
    def testWaitingOnStairsDescends(): Unit \ Assert =
        let c = ctx("#####" :: "#@>.#" :: "#####" :: Nil);
        let (after, events) = Step.steps(c, start(c, Nil, Nil), right() :: Input.Wait :: Nil);
        Assert.assertEq(expected = (Run.Status.Descended, Some(Event.Descended)), (Run.statusOf(after), List.last(events)))

    // ══════════════════════════════════════════════════════
    // 立ち位置の判定
    // ══════════════════════════════════════════════════════

    /// 隣（距離 1）の敵には矢を撃たない。
    @Test
    def testNoArrowAtDistanceOne(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (3, 3)) :: Nil, (Item.Kind.WoodArrow, (2, 3)) :: Nil), right());
        Assert.assertEq(expected = Mark.Move :: Mark.Damage(jellyAttack()) :: Nil, Preview.summarize(events))

    /// 矢は岩で止まる（岩の向こうの敵は撃たない）。
    @Test
    def testRockStopsTheArrow(): Unit \ Assert =
        let c = ctx("#########" :: "#@..r..>#" :: "#########" :: Nil);
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (5, 1)) :: Nil, (Item.Kind.WoodArrow, (2, 1)) :: Nil), right());
        Assert.assertEq(expected = Mark.Move :: Nil, Preview.summarize(events))

    /// 鉄の矢は 1 体を貫いて後ろの敵にも当たる。
    @Test
    def testIronArrowPiercesOneEnemy(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (4, 3)) :: (Enemy.Kind.Jelly, (5, 3)) :: Nil, (Item.Kind.IronArrow, (2, 3)) :: Nil), right());
        let damage = stats(Item.Kind.IronArrow)#damage;
        Assert.assertEq(expected = Event.ArrowShot(Dir.Right, Item.Kind.IronArrow, (5, 3)) :: Event.ArrowHit(0, damage) :: Event.ArrowHit(1, damage) :: Nil,
            events |> List.drop(2) |> List.take(3))

    /// 8 近傍に起きた敵が bombNeeds 体以上で爆弾を投げる（小爆弾の範囲の敵は倒れる）。
    /// 1 手目で爆弾を拾い、2 体は寄ってきて 3 手目の待つで (3, 3) と (2, 4) に並ぶ。
    @Test
    def testBombWhenEnoughAwakeEnemiesAround(): Unit \ Assert =
        let c = ctx(hall());
        let first = start(c, (Enemy.Kind.Jelly, (4, 2)) :: (Enemy.Kind.Jelly, (4, 4)) :: Nil, (Item.Kind.SmallBomb, (2, 3)) :: Nil);
        let (ready, _) = Step.steps(c, first, right() :: Input.Wait :: Nil);
        let (after, events) = Step.step(c, ready, Input.Wait);
        Assert.assertEq(expected = (Mark.Wait :: Mark.Bomb :: Nil, Nil), (Preview.summarize(events), Fixtures.enemyCells(after)))

    // ══════════════════════════════════════════════════════
    // 敵が起きる・敵の手番
    // ══════════════════════════════════════════════════════

    /// 同じ部屋の敵は起き、別の部屋で歩く距離が wakeSteps を超える敵は眠ったまま。通路を歩いて近づくと起きる。
    @Test
    def testEnemiesWakeBySameRoomOrWalkDistance(): Unit \ Assert =
        let c = ctx("#############" :: "#@..,,,,,...#" :: "#...#####..>#" :: "#############" :: Nil);
        let first = start(c, (Enemy.Kind.Jelly, (1, 2)) :: (Enemy.Kind.Jelly, (10, 2)) :: Nil, Nil);
        let (waited, _) = Step.step(c, first, Input.Wait);
        let (near, _) = Step.steps(c, first, right() :: right() :: right() :: right() :: right() :: Nil);
        Assert.assertEq(expected = ((0, Enemy.Rest.Awake) :: (1, Enemy.Rest.Dormant) :: Nil, Some((1, Enemy.Rest.Awake))),
            (Fixtures.enemyRests(waited), Fixtures.enemyRests(near) |> List.find(e -> fst(e) == 1)))

    /// 敵の手番は歩く距離の近い順 → 作った順。同じ距離の 2 体が同じマスへ寄るなら先の id が取り、後の id は次の同点のマスへ。
    @Test
    def testEnemyOrderAndTieBreak(): Unit \ Assert =
        let c = ctx(hall());
        let (after, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (3, 4)) :: (Enemy.Kind.Jelly, (3, 2)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = (0, (3, 3)) :: (1, (2, 2)) :: Nil, Fixtures.enemyCells(after))

    /// 弓兵: 列がそろい距離 2〜4 で間が空いていれば撃つ。
    @Test
    def testArcherShootsAlongALine(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Archer, (4, 3)) :: Nil, Nil), Input.Wait);
        let attack = EnemiesDoc.statsOf(Enemy.Kind.Archer, EnemiesDoc.defaults())#attack;
        Assert.assertEq(expected = Event.HeroWaited :: Event.EnemyWoke(0) :: Event.EnemyShot(0, (4, 3), (1, 3)) :: Event.HeroHurt(0, attack, false) :: Nil, events)

    /// 弓兵: 列がそろっていなければ、縦横の差の小さい方をそろえる向きへ 1 マス。
    @Test
    def testArcherStepsToAlign(): Unit \ Assert =
        let c = ctx(hall());
        let (after, _) = Step.step(c, start(c, (Enemy.Kind.Archer, (4, 1)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = (0, (4, 2)) :: Nil, Fixtures.enemyCells(after))

    /// コウモリは 2 手に 1 回だけ動く（起きた手の数が奇数の手は動かない）。
    @Test
    def testBatMovesEveryOtherTurn(): Unit \ Assert =
        let c = ctx(hall());
        let first = start(c, (Enemy.Kind.Bat, (6, 2)) :: Nil, Nil);
        let (once, _) = Step.step(c, first, Input.Wait);
        let (twice, _) = Step.step(c, once, Input.Wait);
        Assert.assertEq(expected = (true, false), (Fixtures.enemyCells(once) == (0, (6, 2)) :: Nil, Fixtures.enemyCells(twice) == (0, (6, 2)) :: Nil))

    // ══════════════════════════════════════════════════════
    // 受ける・薬・倒れる・自然回復
    // ══════════════════════════════════════════════════════

    /// 盾は受ける攻撃を守りの分だけ減らす。
    @Test
    def testShieldReducesDamage(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (3, 3)) :: Nil, (Item.Kind.WoodShield, (2, 3)) :: Nil), right());
        Assert.assertEq(expected = Mark.Move :: Mark.Damage(Int32.max(0, jellyAttack() - stats(Item.Kind.WoodShield)#guard)) :: Nil, Preview.summarize(events))

    /// HP が 最大 × potionAt 以下になったら倒れる判定の前に薬を使う（maxHp 4 で閾値 1・2 回殴られて 0 → 薬草で回復）。
    @Test
    def testPotionBeforeFalling(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"maxHp\": 4}"), hall());
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (3, 3)) :: Nil, (Item.Kind.Herb, (2, 3)) :: Nil), right() :: Input.Wait :: Nil);
        Assert.assertEq(expected = (Run.Status.Playing, Int32.min(4, 4 - jellyAttack() * 2 + stats(Item.Kind.Herb)#heal), 0),
            (Run.statusOf(after), Run.heroOf(after)#hp, Run.countOf(Run.packOf(after)#potions)))

    /// HP が 0 以下で倒れ、残りの敵は動かない（隣の 2 体のうち 2 体目は殴らない）。
    @Test
    def testFallingStopsTheRestOfTheEnemies(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"maxHp\": 2}"), hall());
        let (after, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: (Enemy.Kind.Jelly, (1, 2)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = (Run.Status.Dead, Mark.Wait :: Mark.Damage(jellyAttack()) :: Mark.Falls :: Nil),
            (Run.statusOf(after), Preview.summarize(events)))

    /// 移動した手の数が regenEvery の倍数で HP +1（待つは数えない）。1 手目に殴られ、2 歩で 1 戻る。
    @Test
    def testRegenCountsOnlyMoves(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"regenEvery\": 2}"), hall());
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil), Input.Wait :: Input.Move(Dir.Up) :: Input.Move(Dir.Up) :: Nil);
        Assert.assertEq(expected = (rules()#maxHp - jellyAttack() + 1, 2), (Run.heroOf(after)#hp, Run.rec(after)#walked))

    /// 銀の剣で殴ると、生き残った敵を 1 マス押し出す。
    @Test
    def testSilverSwordPushes(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 50}}}") | ctx(hall()) };
        let (armed, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (4, 3)) :: Nil, (Item.Kind.SilverSword, (2, 3)) :: Nil), right());
        let (_, events) = Step.step(c, armed, right());
        Assert.assertEq(expected = Event.HeroStruck(0, stats(Item.Kind.SilverSword)#attack) :: Event.EnemyPushed(0, (3, 3), (4, 3)) :: Nil,
            List.take(2, events))
}
