// 1 手の決まり（Step）: 殴る・進めない・拾う → 判定・矢・爆弾・階段・敵が起きる・敵の手番・盾・薬・倒れる・自然回復。
// 盤は Fixtures.hall（7×5 の 1 部屋。主人公は (1, 3)）か、文字で書いた小さな盤。敵は同じ部屋なら最初の手の後に起きる。
mod TestStep {

    use Board.{Dir, Dir8}
    use Run.Source
    use Run.{Event, Input}
    use Preview.Mark
    use Fixtures.{ctx, hall, start}

    def rules(): RulesDoc.Rec = RulesDoc.rec(RulesDoc.defaults())

    def jellyAttack(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Jelly, EnemiesDoc.defaults())#attack

    def stats(kind: Item.Kind): ItemsDoc.Stats = ItemsDoc.statsOf(kind, ItemsDoc.defaults())

    def right(): Input = Input.Move(Dir8.Right)

    // ══════════════════════════════════════════════════════
    // 主人公の手
    // ══════════════════════════════════════════════════════

    /// 敵のいるマスへ動くと殴る（移動しない）。眠っていた敵はその手の敵の番で起きるだけで、殴り返さない。
    @Test
    def testMovingIntoAnEnemyStrikesWithoutMoving(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil), right());
        Assert.assertEq(expected = ((1, 3), Event.HeroStruck(0, rules()#baseAttack) :: Event.EnemyWoke(0) :: Nil),
            (Run.heroOf(after)#at, events))

    /// 斜めの 2 つの横のマスのどちらかが壁なら、その斜めには動けない（水は角を作らないので抜けられる）。
    @Test
    def testDiagonalCannotCutAWallCorner(): Unit \ Assert =
        let c = ctx("######" :: "##.~.#" :: "#@..>#" :: "######" :: Nil);
        let (_, walled) = Step.step(c, start(c, Nil, Nil), Input.Move(Dir8.UpRight));
        let (watered, _) = Step.steps(c, start(c, Nil, Nil), right() :: right() :: Input.Move(Dir8.UpRight) :: Nil);
        Assert.assertEq(expected = (Event.HeroBlocked(Dir8.UpRight) :: Nil, (4, 1)), (walled, Run.heroOf(watered)#at))

    /// 斜めの敵は殴れる（銀の剣でなくても動かずに殴る）。
    @Test
    def testDiagonalStrike(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 2)) :: Nil, Nil), Input.Move(Dir8.UpRight));
        Assert.assertEq(expected = ((1, 3), Some(Event.HeroStruck(0, rules()#baseAttack))), (Run.heroOf(after)#at, List.head(events)))

    /// 歩けないマスへの手は何も起きない（出来事は進めないだけ・移動の数も増えない）。
    @Test
    def testBlockedMoveDoesNotAdvance(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, start(c, Nil, Nil), Input.Move(Dir8.Left));
        Assert.assertEq(expected = (Event.HeroBlocked(Dir8.Left) :: Nil, 0), (events, Run.rec(after)#walked))

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
    /// 1 手目で爆弾を拾い、2 体は斜めに寄ってきて (3, 3) と (3, 4) に並ぶ。2 手目の待つで投げる。
    @Test
    def testBombWhenEnoughAwakeEnemiesAround(): Unit \ Assert =
        let c = ctx(hall());
        let first = start(c, (Enemy.Kind.Jelly, (4, 2)) :: (Enemy.Kind.Jelly, (4, 4)) :: Nil, (Item.Kind.SmallBomb, (2, 3)) :: Nil);
        let (ready, _) = Step.step(c, first, right());
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

    /// 敵の手番は歩く距離の近い順 → 作った順。同じ距離の 2 体が 1 つしかない近づくマス (3, 2) を取り合うなら先の id が取り、
    /// 後の id は待つ（(2, 1) と (2, 3) が壁なので、(3, 1)・(3, 3) から (2, 2) へは角を抜けられない）。
    @Test
    def testEnemyOrderAndTieBreak(): Unit \ Assert =
        let c = ctx("#######" :: "###..>#" :: "#@....#" :: "###...#" :: "#######" :: Nil);
        let (after, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (4, 1)) :: (Enemy.Kind.Jelly, (4, 3)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = (0, (3, 2)) :: (1, (4, 3)) :: Nil, Fixtures.enemyCells(after))

    /// 斜めの隣でも、間の角が壁なら敵は殴れず、主人公の予告にも入らない。
    @Test
    def testEnemyCannotStrikeAcrossACorner(): Unit \ Assert =
        let c = ctx("######" :: "#.#..#" :: "#@..>#" :: "######" :: Nil);
        let first = start(c, (Enemy.Kind.Jelly, (2, 2)) :: Nil, Nil);
        let (_, events) = Step.step(c, first, Input.Move(Dir8.Up));
        Assert.assertEq(expected = (false, false),
            (List.exists(Run.hurtsHero, events), Set.memberOf((1, 1), Preview.dangerCells(c, first))))

    /// 眠っている敵は殴られても、その手の敵の番では動かずに起き（Dozing(1) → Awake）、次の手から殴り返す（見える距離 0 の通路）。
    @Test
    def testHitSleeperWakesNextTurn(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 50}}}")
                  | Fixtures.ctxWith(Fixtures.rulesWith("{\"wakeSteps\": 0}"), "#######" :: "#@,,,>#" :: "#######" :: Nil) };
        let (struck, first) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 1)) :: Nil, Nil), right());
        let (_, second) = Step.step(c, struck, Input.Wait);
        Assert.assertEq(expected = ((0, Enemy.Rest.Awake) :: Nil, false, true),
            (Fixtures.enemyRests(struck), List.exists(Run.hurtsHero, first), List.exists(Run.hurtsHero, second)))

    /// 弓兵: 列がそろい距離 2〜4 で間が空いていれば撃つ。
    @Test
    def testArcherShootsAlongALine(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, start(c, (Enemy.Kind.Archer, (4, 3)) :: Nil, Nil), Input.Wait);
        let attack = EnemiesDoc.statsOf(Enemy.Kind.Archer, EnemiesDoc.defaults())#attack;
        Assert.assertEq(expected = Event.HeroWaited :: Event.EnemyWoke(0) :: Event.EnemyShot(0, (4, 3), (1, 3)) :: Event.HeroHurt(Source.Arrow(0), attack, false) :: Nil, events)

    /// 弓兵: 撃てなければ、主人公を射線に入れるマス（ここでは主人公の右の列の (5, 3)）へ 8 方向の道で寄る。
    @Test
    def testArcherStepsToAlign(): Unit \ Assert =
        let c = ctx(hall());
        let (after, _) = Step.step(c, start(c, (Enemy.Kind.Archer, (7, 5)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = (0, (6, 4)) :: Nil, Fixtures.enemyCells(after))

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
            (Run.statusOf(after), Run.heroOf(after)#hp, Run.countOf(Run.heldOf(Item.Family.Potion, Run.packOf(after)))))

    /// HP が 0 以下で倒れ、残りの敵は動かない（隣の 2 体のうち 2 体目は殴らない）。
    @Test
    def testFallingStopsTheRestOfTheEnemies(): Unit \ Assert =
        let c = Fixtures.ctxWith(Fixtures.rulesWith("{\"maxHp\": 2}"), hall());
        let (after, events) = Step.step(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: (Enemy.Kind.Jelly, (1, 2)) :: Nil, Nil), Input.Wait);
        Assert.assertEq(expected = (Run.Status.Dead, Mark.Wait :: Mark.Damage(jellyAttack()) :: Mark.Falls :: Nil),
            (Run.statusOf(after), Preview.summarize(events)))

    /// 移動した手の数が regenEvery の倍数で HP +1（待つ・殴るは数えない）。1 手目に殴られ、HP 1 のゼリーを殴って倒し、2 歩で 1 戻る。
    @Test
    def testRegenCountsOnlyMoves(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 1}}}") | Fixtures.ctxWith(Fixtures.rulesWith("{\"regenEvery\": 2}"), hall()) };
        let (after, _) = Step.steps(c, start(c, (Enemy.Kind.Jelly, (2, 3)) :: Nil, Nil), Input.Wait :: right() :: Input.Move(Dir8.Up) :: Input.Move(Dir8.Up) :: Nil);
        Assert.assertEq(expected = (rules()#maxHp - jellyAttack() + 1, 2), (Run.heroOf(after)#hp, Run.rec(after)#walked))

    /// 銀の剣で殴ると、生き残った敵を殴った向き（斜めも）へ 1 マス押し出す。ゼリーは (4, 5) から (3, 4) へ寄ってくる。
    @Test
    def testSilverSwordPushes(): Unit \ Assert =
        let c = { enemies = Fixtures.enemiesWith("{\"enemies\": {\"Jelly\": {\"hp\": 50}}}") | ctx(hall()) };
        let (armed, _) = Step.step(c, start(c, (Enemy.Kind.Jelly, (4, 5)) :: Nil, (Item.Kind.SilverSword, (2, 3)) :: Nil), right());
        let (_, events) = Step.step(c, armed, Input.Move(Dir8.DownRight));
        Assert.assertEq(expected = Event.HeroStruck(0, stats(Item.Kind.SilverSword)#attack) :: Event.EnemyPushed(0, (3, 4), (4, 5)) :: Nil,
            List.take(2, events))
}
