// 自爆虫: 隣に来た手に導火線を付け（殴らない）、次の手番に弾けて殴れる隣の主人公と敵に blastDamage・盾は効かない・
// 離れれば当たらない・倒せば弾けない・赤は自爆虫の殴れる隣。盤は既定の広間（主人公 (1, 3)）。
mod TestBomber {

    use Board.Dir8
    use Run.{Event, Input, Source}
    use Fixtures.{ctx, start, hall, withEnemy}

    def blast(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Bomber, EnemiesDoc.defaults())#blastDamage

    def maxHp(): Int32 = RulesDoc.rec(RulesDoc.defaults())#maxHp

    /// 右隣の起きた自爆虫。
    def beside(c: Run.Ctx): Run.State = Fixtures.awake(start(c, (Enemy.Kind.Bomber, (2, 3)) :: Nil, Nil))

    /// cell の起きた自爆虫に導火線が付いた所（ほかの敵 others も起きている）。
    def fused(c: Run.Ctx, cell: Board.Cell, others: List[(Enemy.Kind, Board.Cell)]): Run.State =
        withEnemy(0, e -> { intent = Enemy.Intent.Fuse | e }, Fixtures.awake(start(c, (Enemy.Kind.Bomber, cell) :: others, Nil)))

    /// 隣に来た手は導火線を付けるだけで、殴らない。
    @Test
    def testLightsTheFuseWithoutStriking(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, beside(c), Input.Wait);
        Assert.assertEq(expected = (true, maxHp()), (List.memberOf(Event.EnemyWindup(0, Enemy.Intent.Fuse), events), Run.heroOf(after)#hp))

    /// 次の手番に弾けて、隣の主人公に blastDamage。自爆虫は消える。
    @Test
    def testBlastHitsBeside(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.steps(c, beside(c), Input.Wait :: Input.Wait :: Nil);
        Assert.assertEq(expected = (maxHp() - blast(), Nil, true),
            (Run.heroOf(after)#hp, Fixtures.enemyCells(after), List.memberOf(Event.HeroHurt(Source.Blast(0), blast(), false), events)))

    /// 導火線の手に隣から離れれば当たらない（右下の自爆虫から、上へ 1 歩）。
    @Test
    def testSteppingAwayMisses(): Unit \ Assert =
        let c = ctx(hall());
        let (after, events) = Step.step(c, fused(c, (2, 4), Nil), Input.Move(Dir8.Up));
        Assert.assertEq(expected = (maxHp(), true), (Run.heroOf(after)#hp, List.memberOf(Event.EnemyExploded(0, (2, 4)), events)))

    /// 導火線の付いた自爆虫を倒せば弾けない。
    @Test
    def testDefeatedBomberDoesNotExplode(): Unit \ Assert =
        let c = ctx(hall());
        let weak = withEnemy(0, e -> { hp = 1 | e }, fused(c, (2, 3), Nil));
        let (after, events) = Step.step(c, weak, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (maxHp(), false), (Run.heroOf(after)#hp, List.memberOf(Event.EnemyExploded(0, (2, 3)), events)))

    /// 弾けは隣の敵も巻き込む（右隣のゼリーに blastDamage）。
    @Test
    def testBlastCatchesEnemies(): Unit \ Assert =
        let c = ctx(hall());
        let (_, events) = Step.step(c, fused(c, (2, 3), (Enemy.Kind.Jelly, (3, 3)) :: Nil), Input.Wait);
        Assert.assertEq(expected = true, List.memberOf(Event.BombHit(1, blast()), events))

    /// 主人公の盾は弾けには効かない。
    @Test
    def testShieldDoesNotHelp(): Unit \ Assert =
        let c = ctx(hall());
        let r = Run.rec(fused(c, (2, 3), Nil));
        let shielded = Run.of({ hero = { shield = Some(Item.Kind.IronShield) | r#hero } | r });
        let (after, _) = Step.step(c, shielded, Input.Wait);
        Assert.assertEq(expected = maxHp() - blast(), Run.heroOf(after)#hp)

    /// 赤は自爆虫の殴れる隣の 8 マス。導火線の前の自爆虫には赤が無い。
    @Test
    def testRedIsTheBlast(): Unit \ Assert =
        let c = ctx(hall());
        Assert.assertEq(expected = (Set#{(1, 2), (2, 2), (3, 2), (1, 3), (3, 3), (1, 4), (2, 4), (3, 4)}, Set#{}),
            (Preview.dangerCells(c, fused(c, (2, 3), Nil)), Preview.dangerCells(c, beside(c))))
}
