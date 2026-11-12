// ボス（2×2）: 体の 4 マスのどれに当たっても 1 体・押されない・眠らない・罠を踏まない・
// cooldown が尽きた手番に大技を溜めて床に赤を出す（遠ければ帯・体の周りなら叩きつけ・HP が半分を切ると両方）・
// 大技は盾が効かず、赤の外へ出れば 0・柱の陰は帯が届かない・倒すと Cleared（残ったレベルアップは後で選ぶ）。
// 盤は 13×7 の広間。ボスは (6, 3) を左上にした (6..7, 3..4)。
mod TestBoss {

    use Board.{Cell, Dir8}
    use Run.{Event, Input, Source}
    use Fixtures.{start, withEnemy}

    def hallRows(): List[String] =
        "###############" ::
        "#.............#" ::
        "#.............#" ::
        "#.............#" ::
        "#@............#" ::
        "#.............#" ::
        "#.............#" ::
        "#............>#" ::
        "###############" :: Nil

    /// 左上の帯の行（1・2）の (3, 2) に柱がある広間。
    def pillarRows(): List[String] =
        "###############" ::
        "#.............#" ::
        "#..P..........#" ::
        "#.............#" ::
        "#@............#" ::
        "#.............#" ::
        "#.............#" ::
        "#............>#" ::
        "###############" :: Nil

    def c(): Run.Ctx = Fixtures.ctx(hallRows())

    def stats(): EnemiesDoc.Stats = EnemiesDoc.statsOf(Enemy.Kind.Boss, EnemiesDoc.defaults())

    def maxHp(): Int32 = RulesDoc.rec(RulesDoc.defaults())#maxHp

    def baseAttack(): Int32 = RulesDoc.rec(RulesDoc.defaults())#baseAttack

    /// 起きたボスと、heroAt に立つ主人公（cooldown は rest）。
    def facing(ctx: Run.Ctx, heroAt: Cell, rest: Int32): Run.State =
        let r = Run.rec(withEnemy(0, e -> { cooldown = rest | e }, Fixtures.awake(start(ctx, (Enemy.Kind.Boss, (6, 3)) :: Nil, Nil))));
        Run.of({ hero = { at = heroAt | r#hero } | r })

    def boss(runState: Run.State): Option[Enemy.Enemy] = Run.enemiesOf(runState) |> List.head

    def withIntent(intent: Enemy.Intent, runState: Run.State): Run.State = withEnemy(0, e -> { intent = intent | e }, runState)

    def withHero(f: Run.Hero -> Run.Hero, runState: Run.State): Run.State = { let r = Run.rec(runState); Run.of({ hero = f(r#hero) | r }) }

    /// 体の周り 12 マス。
    def ring(): Set[Cell] =
        List.range(5, 9) |> List.flatMap(x -> List.range(2, 6) |> List.map(y -> (x, y))) |> List.toSet |> Set.filter(cell -> not Set.memberOf(cell, Enemy.cellsAt((6, 3), 2)))

    /// 広間の行 top と top + 1 の全部のマス（体を除く）。
    def rows(top: Int32): Set[Cell] =
        List.range(1, 14) |> List.flatMap(x -> (x, top) :: (x, top + 1) :: Nil) |> List.toSet |> Set.filter(cell -> not Set.memberOf(cell, Enemy.cellsAt((6, 3), 2)))

    // ── 体 ──

    /// 体の 4 マスのどれを殴っても、同じ 1 体（id 0）に当たる。
    @Test
    def testEveryBodyCellIsTheSameFoe(): Unit \ Assert =
        let hit = (heroAt: Cell, dir: Dir8) -> List.memberOf(Event.HeroStruck(0, baseAttack()), snd(Step.step(c(), facing(c(), heroAt, 5), Input.Move(dir))));
        Assert.assertEq(expected = (true, true, true, true), (hit((5, 3), Dir8.Right), hit((8, 4), Dir8.Left), hit((6, 2), Dir8.Down), hit((7, 5), Dir8.Up)))

    /// 体の 2 マスを貫く銀の矢も、ボスには 1 回だけ当たる。
    @Test
    def testPiercingArrowHitsOnce(): Unit \ Assert =
        let first = Fixtures.holding(Run.Held.Held(Item.Kind.SilverArrow, 3) :: Nil, facing(c(), (3, 3), 5));
        let (after, events) = Step.step(c(), first, Input.Wait);
        let damage = ItemsDoc.statsOf(Item.Kind.SilverArrow, ItemsDoc.defaults())#damage;
        Assert.assertEq(expected = (true, Some(stats()#hp - damage)), (List.memberOf(Event.ArrowHit(0, damage), events), boss(after) |> Option.map(e -> e#hp)))

    /// 爆弾の範囲に体の 2 マスが入っても、当たるのは 1 回。
    @Test
    def testBombHitsOnce(): Unit \ Assert =
        let armed = Run.of({ perks = Run.Perk.LoneBomb :: Nil | Run.rec(Fixtures.holding(Run.Held.Held(Item.Kind.SmallBomb, 1) :: Nil, facing(c(), (5, 3), 5))) });
        let (after, _) = Step.step(c(), armed, Input.Wait);
        Assert.assertEq(expected = Some(stats()#hp - ItemsDoc.statsOf(Item.Kind.SmallBomb, ItemsDoc.defaults())#damage), boss(after) |> Option.map(e -> e#hp))

    /// 銀の剣で殴っても押されず、壁に当てたダメージも足さない。
    @Test
    def testBossIsNotPushed(): Unit \ Assert =
        let armed = withHero(h -> { sword = Some(Item.Kind.SilverSword) | h }, facing(c(), (5, 3), 5));
        let (after, _) = Step.step(c(), armed, Input.Move(Dir8.Right));
        let sword = ItemsDoc.statsOf(Item.Kind.SilverSword, ItemsDoc.defaults())#attack;
        Assert.assertEq(expected = Some(((6, 3), stats()#hp - sword)), boss(after) |> Option.map(e -> (e#at, e#hp)))

    /// 煙玉では眠らない（起きたまま）。
    @Test
    def testSmokeDoesNotPutTheBossToSleep(): Unit \ Assert =
        let low = withHero(h -> { hp = 3 | h }, Fixtures.holding(Run.Held.Held(Item.Kind.Smoke, 1) :: Nil, facing(c(), (5, 3), 5)));
        let (after, events) = Step.step(c(), low, Input.Wait);
        Assert.assertEq(expected = (true, Some(Enemy.Rest.Awake)), (List.memberOf(Event.SmokeThrown((5, 3)), events), boss(after) |> Option.map(e -> e#rest)))

    /// 歩いて寄る時は 2×2 のまま 1 マス動き（左下へ）、入ったマスの罠は踏まない。
    @Test
    def testBossWalksAsTwoByTwoAndSkipsTraps(): Unit \ Assert =
        let ctx = Fixtures.ctx("###############" :: "#.............#" :: "#.............#" :: "#....^........#" :: "#@...^........#" :: "#....^........#" :: "#.............#" :: "#............>#" :: "###############" :: Nil);
        let (after, events) = Step.step(ctx, facing(ctx, (1, 4), 5), Input.Wait);
        Assert.assertEq(expected = (true, Set.empty(), Some(stats()#hp)), (List.memberOf(Event.EnemyMoved(0, (6, 3), (5, 4)), events), Run.rec(after)#revealed, boss(after) |> Option.map(e -> e#hp)))

    // ── 大技の溜め ──

    /// cooldown が尽きた手番、主人公が遠ければ帯を溜める（主人公の行 1 とボスに近い行 2）。赤は 2 行の部屋のマス全部。
    @Test
    def testFarHeroGetsTheSweep(): Unit \ Assert =
        let (after, events) = Step.step(c(), facing(c(), (1, 1), 0), Input.Wait);
        Assert.assertEq(expected = (true, rows(1)),
            (List.memberOf(Event.EnemyWindup(0, Enemy.Intent.Sweep(1)), events), Preview.dangerCells(c(), after)))

    /// 主人公が体の周りにいれば叩きつけを溜める（その手は殴らない）。赤は体の周り 12 マス。
    @Test
    def testBesideHeroGetsTheSlam(): Unit \ Assert =
        let (after, events) = Step.step(c(), facing(c(), (5, 4), 0), Input.Wait);
        Assert.assertEq(expected = (true, maxHp(), ring()),
            (List.memberOf(Event.EnemyWindup(0, Enemy.Intent.Slam), events), Run.heroOf(after)#hp, Preview.dangerCells(c(), after)))

    /// HP が半分を切ると、叩きつけと帯を重ねて溜める。赤は体の周りと 2 行（主人公はボスの行にいるのでボスの 2 行）。
    @Test
    def testHalfHpCombinesBoth(): Unit \ Assert =
        let hurt = withEnemy(0, e -> { hp = stats()#hp / 2 - 1 | e }, facing(c(), (5, 4), 0));
        let (after, events) = Step.step(c(), hurt, Input.Wait);
        Assert.assertEq(expected = (true, Set.union(ring(), rows(3))),
            (List.memberOf(Event.EnemyWindup(0, Enemy.Intent.SlamAndSweep(3)), events), Preview.dangerCells(c(), after)))

    /// 柱の陰（ボスから見て柱の向こう）は帯が届かない。
    @Test
    def testPillarShadesTheSweep(): Unit \ Assert =
        let ctx = Fixtures.ctx(pillarRows());
        let band = Run.bandOf(ctx#board, Enemy.make(0, Enemy.Kind.Boss, (6, 3), stats()#hp), 1);
        Assert.assertEq(expected = (false, true, false), (Set.memberOf((2, 2), band), Set.memberOf((2, 1), band), Set.memberOf((3, 2), band)))

    // ── 大技を当てる ──

    /// 叩きつけは体の周りの主人公に slamDamage（鉄の盾も効かない）。
    @Test
    def testSlamIgnoresTheShield(): Unit \ Assert =
        let shielded = withHero(h -> { shield = Some(Item.Kind.IronShield) | h }, withIntent(Enemy.Intent.Slam, facing(c(), (5, 4), 0)));
        let (after, events) = Step.step(c(), shielded, Input.Wait);
        Assert.assertEq(expected = (maxHp() - stats()#slamDamage, true, true),
            (Run.heroOf(after)#hp, List.memberOf(Event.EnemySlammed(0), events), List.memberOf(Event.HeroHurt(Source.Slam(0), stats()#slamDamage, false), events)))

    /// 体の周りの外へ下がれば、叩きつけは 0。
    @Test
    def testSteppingOutOfTheRingDodges(): Unit \ Assert =
        let (after, events) = Step.step(c(), withIntent(Enemy.Intent.Slam, facing(c(), (5, 4), 0)), Input.Move(Dir8.Left));
        Assert.assertEq(expected = (maxHp(), false), (Run.heroOf(after)#hp, List.exists(Run.hurtsHero, events)))

    /// 帯の中で待てば sweepDamage、柱の陰で待てば 0。
    @Test
    def testSweepHitsTheBandButNotTheShade(): Unit \ Assert =
        let ctx = Fixtures.ctx(pillarRows());
        let hpAt = (heroAt: Cell) -> Run.heroOf(fst(Step.step(ctx, withIntent(Enemy.Intent.Sweep(1), facing(ctx, heroAt, 0)), Input.Wait)))#hp;
        Assert.assertEq(expected = (maxHp() - stats()#sweepDamage, maxHp()), (hpAt((2, 1)), hpAt((2, 2))))

    /// 大技の後は溜めが消え、次を溜めるまでの cooldown は cooldownMin..cooldownMax。
    @Test
    def testCooldownAfterTheSmash(): Unit \ Assert =
        let (after, _) = Step.step(c(), withIntent(Enemy.Intent.Slam, facing(c(), (1, 1), 0)), Input.Wait);
        let rolled = boss(after) |> Option.map(e -> (e#intent, e#cooldown >= stats()#cooldownMin and e#cooldown <= stats()#cooldownMax));
        Assert.assertEq(expected = Some((Enemy.Intent.Idle, true)), rolled)

    /// cooldown の残る手番は、隣の主人公を殴り（盾の効く攻撃）、cooldown を 1 減らす。
    @Test
    def testRestingBossStrikesBeside(): Unit \ Assert =
        let (after, events) = Step.step(c(), facing(c(), (5, 4), 2), Input.Wait);
        Assert.assertEq(expected = (true, Some(1)),
            (List.memberOf(Event.HeroHurt(Source.Melee(0), stats()#attack, false), events), boss(after) |> Option.map(e -> e#cooldown)))

    // ── 倒す ──

    /// 倒すと Cleared（経験値でレベルが 4 まで上がる）。その手に上がったレベルのカードは、Cleared の後でも選べる。
    @Test
    def testDefeatClearsAndCardsStillOpen(): Unit \ Assert =
        let weak = withEnemy(0, e -> { hp = baseAttack() | e }, facing(c(), (5, 3), 5));
        let (beaten, events) = Step.step(c(), weak, Input.Move(Dir8.Right));
        let (chosen, _) = Step.step(c(), beaten, Input.Choose(0));
        Assert.assertEq(expected = (Run.Status.Cleared, true, 4, true),
            (Run.statusOf(beaten), List.memberOf(Event.EnemyDied(0, Enemy.Kind.Boss, (6, 3)), events), Run.rec(beaten)#level, Run.rec(chosen)#perks != Nil))

    /// 倒した後の Cleared では、Choose 以外の手は何もしない。
    @Test
    def testClearedIgnoresMoves(): Unit \ Assert =
        let weak = withEnemy(0, e -> { hp = baseAttack() | e }, facing(c(), (5, 3), 5));
        let (beaten, _) = Step.step(c(), weak, Input.Move(Dir8.Right));
        Assert.assertEq(expected = Nil, snd(Step.step(c(), beaten, Input.Move(Dir8.Left))))

    /// 画面: ボスを倒した手を見せ終えるとカードを選び、3 枚（Lv4 まで）選び終えたら抜けた画面へ（既定の 1F の左上の部屋に置いたボス）。
    @Test
    def testScreenGoesCardsThenClear(): Unit \ Assert =
        let res = Fixtures.res();
        let w0 = World.begin(res, World.initial(res, World.fixedSeed()));
        let ctx = World.ctx(res, w0);
        let r = Run.rec(withEnemy(0, e -> { hp = baseAttack(), cooldown = 5 | e }, Fixtures.awake(start(ctx, (Enemy.Kind.Boss, (4, 1)) :: Nil, Nil))));
        let w1 = World.withState(res, Run.of({ hero = { at = (3, 1) | r#hero } | r }), w0);
        let w2 = World.advance(res, World.skipShow(World.play(res, Input.Move(Dir8.Right), w1)));
        let answered = World.answerCard(res, 0, World.answerCard(res, 0, World.answerCard(res, 0, w2)));
        Assert.assertEq(expected = (World.Screen.Cards, World.Screen.Clear), (w2#screen, answered#screen))
}
