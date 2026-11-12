// テストが共有する組み立てと取り出し（文字の地図から盤と判定の束を組む・置き場から階の始めの Run を組む・Doc の既定で描く元を組む）。
mod Fixtures {

    use fge::AudioMixer
    use Board.Cell

    /// 文字の地図から盤を組む。組めない地図はテストの書き誤りなので止める。
    pub def board(rows: List[String]): Board.Board =
        match Board.fromRows(rows) {
            case Ok(b)    => b
            case Err(why) => bug!("テストの地図が組めない: ${why}")
        }

    /// 既定の Doc（rules だけ差し替え）で組んだ判定の束。
    pub def ctxWith(rules: RulesDoc.Doc, rows: List[String]): Run.Ctx =
        { rules = rules, items = ItemsDoc.defaults(), enemies = EnemiesDoc.defaults(), cards = CardsDoc.defaults(), board = board(rows) }

    /// 既定の Doc で組んだ判定の束。
    pub def ctx(rows: List[String]): Run.Ctx = ctxWith(RulesDoc.defaults(), rows)

    /// rules の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def rulesWith(json: String): RulesDoc.Doc =
        match RulesDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの rules の JSON が読めない: ${json}")
        }

    /// enemies の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def enemiesWith(json: String): EnemiesDoc.Doc =
        match EnemiesDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの enemies の JSON が読めない: ${json}")
        }

    /// items の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def itemsWith(json: String): ItemsDoc.Doc =
        match ItemsDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの items の JSON が読めない: ${json}")
        }

    /// floors の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def floorsWith(json: String): FloorsDoc.Doc =
        match FloorsDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの floors の JSON が読めない: ${json}")
        }

    /// 置き場から階の始めの Run（敵は全員眠っている・罠は地図の字のまま）。
    pub def start(ctx: Run.Ctx, enemies: List[(Enemy.Kind, Cell)], items: List[(Item.Kind, Cell)]): Run.State =
        Run.start(ctx, { floor = 1, layout = 0, seed = 1i64, rng = 1i64 }, { enemies = enemies, items = items, traps = ctx#board#traps })

    /// 敵を全員起こした Run（その手の敵の番から動く）。
    pub def awake(runState: Run.State): Run.State =
        let r = Run.rec(runState);
        Run.of({ enemies = r#enemies |> List.map(e -> { rest = Enemy.Rest.Awake | e }) | r })

    /// id の敵の眠り方を rest にした Run。
    pub def resting(id: Int32, rest: Enemy.Rest, runState: Run.State): Run.State =
        let r = Run.rec(runState);
        Run.of({ enemies = r#enemies |> List.map(e -> if (e#id == id) { rest = rest | e } else e) | r })

    /// cells の罠を見えている罠にした Run。
    pub def revealing(cells: List[Cell], runState: Run.State): Run.State =
        let r = Run.rec(runState);
        Run.of({ revealed = Set.union(r#revealed, List.toSet(cells)) | r })

    /// 持ち物の枠を pack にした Run。
    pub def holding(pack: Run.Pack, runState: Run.State): Run.State = Run.of({ pack = pack | Run.rec(runState) })

    /// 1 部屋の広間（7×5 の床。主人公は左の真ん中 (1, 3)・階段は右下 (7, 5)）。
    pub def hall(): List[String] =
        "#########" ::
        "#.......#" ::
        "#.......#" ::
        "#@......#" ::
        "#.......#" ::
        "#......>#" ::
        "#########" :: Nil

    // ── 乱数の局面（性質テストの生成器） ──

    /// 生成器が使う盤: 柱・岩・水の角と通路がある部屋と、毒矢の罠を字で置いた部屋。
    def randomBoards(): List[List[String]] =
        ("###########" ::
         "#....P....#" ::
         "#.r.....~~#" ::
         "#..@..P.~~#" ::
         "#.....,...#" ::
         "##,####.P.#" ::
         "#..P....r.#" ::
         "#.~~...>..#" ::
         "###########" :: Nil) ::
        ("#############" ::
         "#.....^.....#" ::
         "#.P.......P.#" ::
         "#...^.@.^...#" ::
         "#,,,,#.#,,,,#" ::
         "#.P.^...r..>#" ::
         "#############" :: Nil) :: Nil

    /// seed から乱数で組んだ局面: 盤・主人公の位置と HP・持ち物・1〜5 体の敵（種類・位置・眠り方・HP・燃える手・向き・溜め）・0〜3 個の毒矢か警報の罠を足す。
    /// 地図の字の罠もそのまま残し、罠は乱数で半分ほどを見えている罠にする。
    pub def randomCase(seed: Int64): (Run.Ctx, Run.State) =
        let boards = randomBoards();
        let (which, r1) = Rng.below(List.length(boards), Rng.mix(seed, 7i64));
        let c = ctx(List.nth(which, boards) |> Option.getWithDefault(hall()));
        let b = c#board;
        let walkable = Board.cells(b) |> List.filter(cell -> Board.isWalkable(cell, b));
        let flyable = Board.cells(b) |> List.filter(cell -> Board.isFlyable(cell, b));
        let (heroCells, r2) = Rng.pick(1, walkable, r1);
        let heroAt = List.head(heroCells) |> Option.getWithDefault(b#start);
        let (count, r3) = Rng.below(5, r2);
        let (foes, r4) = List.range(0, count + 1) |> List.foldLeft((acc, _) -> {
            let (made, state) = acc;
            let taken = Set.insert(heroAt, made |> List.map(e -> e#at) |> List.toSet);
            let (kindAt, s1) = Rng.below(List.length(Enemy.all()), state);
            let kind = List.nth(kindAt, Enemy.all()) |> Option.getWithDefault(Enemy.Kind.Jelly);
            let pool = (if (Enemy.isFlying(kind)) flyable else walkable) |> List.filter(cell -> not Set.memberOf(cell, taken) and Board.chebyshev(cell, heroAt) <= 5);
            let (cells, s2) = Rng.pick(1, pool, s1);
            let (restAt, s3) = Rng.below(4, s2);
            let (hp, s4) = Rng.below(4, s3);
            let (burn, s5) = Rng.below(3, s4);
            let (facingAt, s6) = Rng.below(8, s5);
            let rest = match restAt {
                case 0 => Enemy.Rest.Dormant
                case 1 => Enemy.Rest.Dozing(1 + burn)
                case _ => Enemy.Rest.Awake
            };
            let facing = List.nth(facingAt, Board.dirs8()) |> Option.getWithDefault(Board.Dir8.Down);
            let made2 = List.head(cells) |> Option.map(cell ->
                { rest = rest, hp = 1 + hp, burn = burn, facing = facing, turns = facingAt | Enemy.make(List.length(made), kind, cell, 1) } :: made)
                |> Option.getWithDefault(made);
            (made2, s6)
        }, (Nil, r3));
        let foeCells = foes |> List.map(e -> e#at) |> List.toSet;
        let (trapCount, r5) = Rng.below(4, r4);
        let floors = walkable |> List.filter(cell -> Board.tileAt(cell, b) == Board.Tile.Floor and cell != heroAt and not Set.memberOf(cell, foeCells));
        let (trapCells, r6) = Rng.pick(trapCount, floors, r5);
        let (packAt, r7) = Rng.below(5, r6);
        let (hpLeft, r8) = Rng.below(20, r7);
        let packs = Nil :: (Run.Held.Held(Item.Kind.WoodArrow, 3) :: Nil) :: (Run.Held.Held(Item.Kind.SmallBomb, 1) :: Nil)
            :: (Run.Held.Held(Item.Kind.Smoke, 1) :: Nil) :: (Run.Held.Held(Item.Kind.Pebble, 3) :: Run.Held.Held(Item.Kind.Herb, 1) :: Nil) :: Nil;
        let traps = trapCells |> List.zipWithIndex |> List.foldLeft((m, e) -> Map.insert(snd(e), if (Int32.modulo(fst(e) + packAt, 2) == 0) Trap.Kind.PoisonDart else Trap.Kind.Alarm, m), b#traps);
        let (shown, r9) = Rng.pick(Map.size(traps) / 2, Map.keysOf(traps) |> Set.toList, r8);
        let r = Run.rec(Run.start(c, { floor = 1, layout = 0, seed = seed, rng = r9 }, { enemies = Nil, items = Nil, traps = traps }));
        let enemies = List.reverse(foes);
        (c, Run.of({ hero = { at = heroAt, hp = 1 + hpLeft | r#hero }, pack = List.nth(packAt, packs) |> Option.getWithDefault(Nil),
                     enemies = enemies, nextId = List.length(enemies), revealed = List.toSet(shown), seen = Board.viewFrom(heroAt, b) | r }))

    /// 先頭の選ぶ場面のカード（レベルアップでなければ空）。
    def levelCards(runState: Run.State): List[Run.Perk] = match Run.pendingOf(runState) {
        case Run.Choice.LevelUp(cards) :: _ => cards
        case _                              => Nil
    }

    /// 先頭のレベルアップのカードの枚数。
    pub def cardCount(runState: Run.State): Int32 = List.length(levelCards(runState))

    /// 先頭のレベルアップの候補に perk があるか。
    pub def offers(perk: Run.Perk, runState: Run.State): Bool = List.memberOf(perk, levelCards(runState))

    /// 先頭のレベルアップの候補から perk を選ぶ 1 手（候補に無ければテストの書き誤りなので止める）。
    pub def chooseCard(ctx: Run.Ctx, perk: Run.Perk, runState: Run.State): (Run.State, List[Run.Event]) =
        match List.indexOf(perk, levelCards(runState)) {
            case Some(i) => Step.step(ctx, runState, Run.Input.Choose(i))
            case None    => bug!("候補に ${perk} が無い")
        }

    /// 敵の id → 位置。
    pub def enemyCells(runState: Run.State): List[(Int32, Cell)] = Run.enemiesOf(runState) |> List.map(e -> (e#id, e#at))

    /// 敵の id → 起きているか。
    pub def enemyRests(runState: Run.State): List[(Int32, Enemy.Rest)] = Run.enemiesOf(runState) |> List.map(e -> (e#id, e#rest))

    /// 既定の Doc で組んだ描く元（floors は既定の 1 枚）。
    pub def res(): Res.Res =
        Res.build({ rules = RulesDoc.defaults(), items = ItemsDoc.defaults(), enemies = EnemiesDoc.defaults(), cards = CardsDoc.defaults(),
                    floors = FloorsDoc.defaults(), look = LookDoc.defaults(), text = TextDoc.defaults(),
                    keys = KeysDoc.defaults(), theme = ThemeDoc.defaults(), mixer = AudioMixer.defaults(), kit = Kit.empty() })
}
