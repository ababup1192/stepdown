// 盾持ち: 向き（歩く・待つ手番ごとに主人公の方へ 45° ずつ回る）の前の 3 方向からの殴る・矢・石を盾で防ぐ（0 でも当たったので眠っていれば起きかける）・
// 横や後ろからは通る・爆弾は防がない・殴る時は向きを変えない・正面の盾持ちには矢を撃たない。盤は既定の広間（主人公 (1, 3)）。
mod TestShield {

    use Board.Dir8
    use Run.{Event, Input}
    use Fixtures.{ctx, start, hall, facing, holding}

    def shieldHp(): Int32 = EnemiesDoc.statsOf(Enemy.Kind.Shield, EnemiesDoc.defaults())#hp

    def baseAttack(): Int32 = RulesDoc.rec(RulesDoc.defaults())#baseAttack

    def hpOf(runState: Run.State): Option[Int32] = Run.enemiesOf(runState) |> List.head |> Option.map(e -> e#hp)

    // ══════════════════════════════════════════════════════
    // 殴る
    // ══════════════════════════════════════════════════════

    /// 正面（左を向いた盾持ちの左）から殴ると 0 で、盾で防ぐ。眠っていた盾持ちは起きかけ、その手は殴り返さない。
    @Test
    def testStrikeFromTheFrontIsGuarded(): Unit \ Assert =
        let c = ctx(hall());
        let first = facing(0, Dir8.Left, start(c, (Enemy.Kind.Shield, (2, 3)) :: Nil, Nil));
        let (after, events) = Step.step(c, first, Input.Move(Dir8.Right));
        Assert.assertEq(expected = (Some(shieldHp()), true, Run.heroOf(first)#hp, Preview.Mark.Strike :: Preview.Mark.Guarded :: Nil),
            (hpOf(after), List.memberOf(Event.Guarded(0), events), Run.heroOf(after)#hp, Preview.summarize(events)))

    /// 斜め前（上を向いた盾持ちの左上）も正面に入り、真横（左）からは通る。
    @Test
    def testStrikeFromTheSideGoesThrough(): Unit \ Assert =
        let c = ctx(hall());
        let up = (at: Board.Cell) -> facing(0, Dir8.Up, start(c, (Enemy.Kind.Shield, at) :: Nil, Nil));
        let (fromSide, _) = Step.step(c, up((2, 3)), Input.Move(Dir8.Right));
        let (fromAhead, _) = Step.step(c, up((2, 4)), Input.Move(Dir8.DownRight));
        Assert.assertEq(expected = (Some(shieldHp() - baseAttack()), Some(shieldHp())), (hpOf(fromSide), hpOf(fromAhead)))

    /// 盾持ちが殴っても向きは変わらない（上を向いたまま右の主人公を殴る）。
    @Test
    def testStrikingKeepsTheFacing(): Unit \ Assert =
        let c = ctx(hall());
        let first = Fixtures.awake(facing(0, Dir8.Up, start(c, (Enemy.Kind.Shield, (2, 3)) :: Nil, Nil)));
        let (after, events) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = (true, Some(Dir8.Up)),
            (List.exists(Run.hurtsHero, events), Run.enemiesOf(after) |> List.head |> Option.map(e -> e#facing)))

    /// 歩く手番は、主人公への大まかな向きへ 45° だけ回る（右を向いた盾持ちが左下へ歩き、左上の主人公の方へ右上まで）。
    @Test
    def testWalkingTurnsOnlyOneStep(): Unit \ Assert =
        let c = ctx(hall());
        let first = Fixtures.awake(facing(0, Dir8.Right, start(c, (Enemy.Kind.Shield, (5, 3)) :: Nil, Nil)));
        let (after, _) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = Some(((4, 4), Dir8.UpRight)), Run.enemiesOf(after) |> List.head |> Option.map(e -> (e#at, e#facing)))

    /// 真後ろへは時計回りに 45° だけ回る（右を向いて、左へは右下）。
    @Test
    def testTurnTowardBehindIsOneStepClockwise(): Unit \ Assert =
        Assert.assertEq(expected = Dir8.DownRight, Board.turnToward(Dir8.Right, Dir8.Left))

    /// 正面の隣から上・右と 2 歩で真上へ回り込むと（その間の 2 手番は殴られ、向きは左のまま）、上から殴って防がれずに当たる。
    @Test
    def testCirclingTwoStepsHitsFromTheSide(): Unit \ Assert =
        let c = ctx(hall());
        let first = Run.of({ hero = { at = (2, 3) | Run.heroOf(start(c, Nil, Nil)) } | Run.rec(Fixtures.awake(facing(0, Dir8.Left, start(c, (Enemy.Kind.Shield, (3, 3)) :: Nil, Nil)))) });
        let (after, events) = Step.steps(c, first, Input.Move(Dir8.Up) :: Input.Move(Dir8.Right) :: Input.Move(Dir8.Down) :: Nil);
        Assert.assertEq(expected = (Some(shieldHp() - baseAttack()), false), (hpOf(after), List.memberOf(Event.Guarded(0), events)))

    // ══════════════════════════════════════════════════════
    // 矢と爆弾
    // ══════════════════════════════════════════════════════

    /// 起きた盾持ちが正面にいる列には矢を撃たず、横を向いていれば撃つ。
    @Test
    def testNoArrowAtTheFront(): Unit \ Assert =
        let c = ctx(hall());
        let shot = (dir: Dir8) -> {
            let first = holding(Run.Held.Held(Item.Kind.WoodArrow, 3) :: Nil, Fixtures.awake(facing(0, dir, start(c, (Enemy.Kind.Shield, (5, 3)) :: Nil, Nil))));
            Preview.summarize(snd(Step.step(c, first, Input.Wait))) |> List.memberOf(Preview.Mark.Arrow)
        };
        Assert.assertEq(expected = (false, true), (shot(Dir8.Left), shot(Dir8.Up)))

    /// 貫く鉄の矢は前のゼリーを射抜いても、後ろで正面を向いた盾持ちには防がれる（盾持ちは HP のまま）。
    @Test
    def testPiercingArrowStopsAtTheShield(): Unit \ Assert =
        let c = ctx(hall());
        let first = holding(Run.Held.Held(Item.Kind.IronArrow, 3) :: Nil,
            Fixtures.awake(facing(1, Dir8.Left, start(c, (Enemy.Kind.Jelly, (3, 3)) :: (Enemy.Kind.Shield, (5, 3)) :: Nil, Nil))));
        let (_, events) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = (true, true, false),
            (List.memberOf(Event.Guarded(1), events), List.memberOf(Event.ArrowHit(1, 0), events),
             List.exists(e -> e == Event.ArrowHit(1, ItemsDoc.statsOf(Item.Kind.IronArrow, ItemsDoc.defaults())#damage), events)))

    /// 爆弾は正面でも防がない（正面の盾持ちと隣のゼリーで爆弾が出て、盾持ちに小爆弾のダメージ）。
    @Test
    def testBombIsNotGuarded(): Unit \ Assert =
        let c = ctx(hall());
        let first = holding(Run.Held.Held(Item.Kind.SmallBomb, 1) :: Nil,
            Fixtures.awake(facing(0, Dir8.Left, start(c, (Enemy.Kind.Shield, (2, 3)) :: (Enemy.Kind.Jelly, (2, 2)) :: Nil, Nil))));
        let (_, events) = Step.step(c, first, Input.Wait);
        Assert.assertEq(expected = true, List.memberOf(Event.BombHit(0, ItemsDoc.statsOf(Item.Kind.SmallBomb, ItemsDoc.defaults())#damage), events))
}
