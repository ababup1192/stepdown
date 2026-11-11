// Floor — 階を作る: floors の Doc の地図から盤を組み（地図ごとに 1 回）、種から地図と敵・アイテムの置き場を選んで Run にする（純粋）。
// 乱数は地図の選び・食べ物の数・置き場だけ。種は Rng.mix(周回の種, 階)。敵は始まりから歩いて enemyMinSteps 以上、アイテムは床だけ（階段と始まりには置かない）。
// 食べ物（パンの数は breadOdds の重み・肉は meatChance）は floors の Doc の items の後ろに足して、ほかのアイテムと同じく置く。
mod Floor {

    use Board.Cell

    /// 階 → その階で選べる盤（floors の Doc の maps の並び）。
    pub type alias Layouts = Map[Int32, Vector[Board.Board]]

    /// 階を作るのに読む Doc。
    pub type alias Docs = { rules = RulesDoc.Doc, items = ItemsDoc.Doc, enemies = EnemiesDoc.Doc, cards = CardsDoc.Doc, floors = FloorsDoc.Doc }

    /// 階ごとの盤を組む。組めない地図・無い地図の名前は飛ばして理由を返し、盤が 1 枚も無い階は入れない。
    pub def layoutsOf(floors: FloorsDoc.Doc): { layouts = Layouts, problems = List[String] } =
        let maps = FloorsDoc.rec(floors)#maps;
        let built = FloorsDoc.rec(floors)#floors |> List.map(f -> (f#floor, f#maps |> List.map(name ->
            Map.get(name, maps) |> Option.map(m -> Board.fromRows(m#rows)) |> Option.getWithDefault(Err("地図 '${name}' が maps に無い"))
                |> Result.mapErr(why -> "[floors] ${f#floor}F の地図 '${name}': ${why}（飛ばします）"))));
        let problems = built |> List.flatMap(e -> snd(e) |> List.filterMap(b -> match b { case Err(why) => Some(why) case Ok(_) => None }));
        let layouts = built |> List.filterMap(e -> {
            let boards = snd(e) |> List.filterMap(Result.toOption);
            if (List.isEmpty(boards)) None else Some((fst(e), List.toVector(boards)))
        }) |> List.toMap;
        { layouts = layouts, problems = problems }

    /// 盤が組めない時に使う盤（floors の Doc の既定の 1 枚目）。
    def fallbackBoard(): Board.Board =
        let rows = FloorsDoc.rec(FloorsDoc.defaults())#maps |> Map.get("1a") |> Option.map(m -> m#rows) |> Option.getWithDefault(Nil);
        match Board.fromRows(rows) {
            case Ok(b)    => b
            case Err(why) => bug!("floors の既定の地図が組めない: ${why}")
        }

    /// floor 階で選べる地図の数（盤が無い階は fallbackBoard の 1 枚）。
    pub def layoutCount(floor: Int32, layouts: Layouts): Int32 =
        Map.get(floor, layouts) |> Option.map(Vector.length) |> Option.getWithDefault(1)

    /// floor 階の layout 番目の盤（無ければ fallbackBoard）。
    pub def boardOf(layouts: Layouts, floor: Int32, layout: Int32): Board.Board =
        Map.get(floor, layouts) |> Option.flatMap(boards -> Vector.nth(layout, boards)) |> Option.getWithDefault(fallbackBoard())

    /// Run の今の階の判定に使う束。
    pub def ctxOf(docs: Docs, layouts: Layouts, runState: Run.State): Run.Ctx =
        let r = Run.rec(runState);
        { rules = docs#rules, items = docs#items, enemies = docs#enemies, cards = docs#cards, board = boardOf(layouts, r#floor, r#layout) }

    /// 周回の種 seed で floor 階を作る（同じ種と Doc からは同じ階）。
    pub def create(docs: Docs, layouts: Layouts, seed: Int64, floor: Int32): Run.State =
        let (layout, rng) = Rng.below(layoutCount(floor, layouts), Rng.mix(seed, Int32.toInt64(floor)));
        createOn(docs, layouts, { seed = seed, floor = floor, layout = layout }, rng)

    /// 地図の番号を決めて階を作る（rng はその後の置き場に使う乱数の状態）。
    pub def createOn(docs: Docs, layouts: Layouts, at: { seed = Int64, floor = Int32, layout = Int32 }, rng: Int64): Run.State =
        let (floor, layout, seed) = (at#floor, at#layout, at#seed);
        let board = boardOf(layouts, floor, layout);
        let ctx = { rules = docs#rules, items = docs#items, enemies = docs#enemies, cards = docs#cards, board = board };
        let (placement, rest) = placementOf(docs, floor, board, rng);
        Run.start(ctx, { floor = floor, layout = layout, seed = seed, rng = rest }, placement)

    /// 敵とアイテムの置き場を選ぶ。
    def placementOf(docs: Docs, floor: Int32, board: Board.Board, rng: Int64): (Run.Placement, Int64) =
        let plan = FloorsDoc.floorOf(floor, docs#floors);
        let kinds = Enemy.all()
            |> List.filter(kind -> EnemiesDoc.statsOf(kind, docs#enemies)#firstFloor <= floor)
            |> List.flatMap(kind -> List.repeat(plan |> Option.map(FloorsDoc.enemyCount(kind)) |> Option.getWithDefault(0), kind));
        let fixed = plan |> Option.map(f -> f#items) |> Option.getWithDefault(Nil);
        let dist = Board.walkDistances(board#start, board);
        let open = Board.cells(board) |> List.filter(c -> Board.tileAt(c, board) == Board.Tile.Floor and c != board#start and Map.memberOf(c, dist));
        let far = open |> List.filter(c -> Map.getWithDefault(c, 0, dist) >= RulesDoc.rec(docs#rules)#enemyMinSteps);
        let (enemyCells, afterEnemies) = Rng.pick(List.length(kinds), far, rng);
        let (food, afterFood) = plan |> Option.map(f -> foodOf(f, afterEnemies)) |> Option.getWithDefault((Nil, afterEnemies));
        let items = fixed ::: food;
        let taken = List.toSet(enemyCells);
        let (itemCells, afterItems) = Rng.pick(List.length(items), open |> List.filter(c -> not Set.memberOf(c, taken)), afterFood);
        ({ enemies = List.zip(kinds, enemyCells), items = List.zip(items, itemCells) }, afterItems)

    /// 階に置く食べ物: パン（breadOdds の重みで 0, 1, 2… 個）と、meatChance で肉 1 つ。
    def foodOf(plan: FloorsDoc.FloorDef, rng: Int64): (List[Item.Kind], Int64) =
        let (breads, afterBread) = Rng.weighted(plan#breadOdds, rng);
        let (meat, afterMeat) = Rng.chance(plan#meatChance, afterBread);
        (List.repeat(breads, Item.Kind.Bread) ::: (if (meat) Item.Kind.Meat :: Nil else Nil), afterMeat)

    /// 置いた敵のマスの始まりからの歩数のうち一番近い物（置いた敵がいなければ None）。
    pub def nearestEnemySteps(board: Board.Board, runState: Run.State): Option[Int32] =
        let dist = Board.walkDistances(board#start, board);
        Run.enemiesOf(runState) |> List.filterMap(e -> Map.get(e#at, dist)) |> List.minimum

    /// 始まりから階段まで歩いて着けるか。
    pub def reachesStairs(board: Board.Board): Bool =
        let dist = Board.walkDistances(board#start, board);
        Board.cells(board) |> List.exists(c -> Board.tileAt(c, board) == Board.Tile.Stairs and Map.memberOf(c, dist))

    /// 床のアイテムが全部、床（階段でも始まりでもない）のマスにあるか。
    pub def itemsOnFloor(board: Board.Board, runState: Run.State): Bool =
        Run.rec(runState)#items |> Map.keysOf |> Set.forAll((c: Cell) -> Board.tileAt(c, board) == Board.Tile.Floor and c != board#start)
}
