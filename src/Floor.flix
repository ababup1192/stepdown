// Floor — 階を作る: floors の Doc の地図から盤を組み（地図ごとに 1 回）、種から地図と敵・アイテムの置き場を選んで Run にする（純粋）。
// 乱数は地図の選び・食べ物の数・置き場だけ。種は Rng.mix(周回の種, 階)。敵は始まりから歩いて enemyMinSteps 以上（群れは 3 体を隣り合わせて置く）、アイテムは床だけ（階段と始まりには置かない）。
// 食べ物（パンの数は breadOdds の重み・肉は meatChance）は、字の候補が無い盤では floors の Doc の items の後ろに足してほかのアイテムと同じく置き、
// 字の候補がある盤では食べ物の候補（%）から選ぶ。罠は地図の字のまま。
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

    /// 盤のある全部の地図の (階, 地図の番号)（階の順 → 地図の順）。
    pub def mapIds(layouts: Layouts): List[(Int32, Int32)] =
        Map.toList(layouts) |> List.flatMap(match (floor, boards) -> List.range(0, Vector.length(boards)) |> List.map(layout -> (floor, layout)))

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

    /// 周回の種 seed で 1F から潜り始める（同じ種と Doc からは同じ階）。
    pub def begin(docs: Docs, layouts: Layouts, seed: Int64): Run.State = arrive(docs, layouts, seed, 1, None)

    /// prev を持ち越して floor 階に入る（地図と置き場は周回の種と階の番号だけで決まる。Doc を保存した時は今の階で呼ぶ）。
    pub def enter(docs: Docs, layouts: Layouts, prev: Run.State, floor: Int32): Run.State =
        arrive(docs, layouts, Run.rec(prev)#seed, floor, Some(prev))

    def arrive(docs: Docs, layouts: Layouts, seed: Int64, floor: Int32, prev: Option[Run.State]): Run.State =
        let (layout, rng) = Rng.below(layoutCount(floor, layouts), Rng.mix(seed, Int32.toInt64(floor)));
        arriveOn(docs, layouts, { seed = seed, floor = floor, layout = layout }, rng, prev)

    /// 地図の番号を決めて、潜りの始めとしてその階を作る（rng はその後の置き場に使う乱数の状態）。
    pub def createOn(docs: Docs, layouts: Layouts, at: { seed = Int64, floor = Int32, layout = Int32 }, rng: Int64): Run.State =
        arriveOn(docs, layouts, at, rng, None)

    def arriveOn(docs: Docs, layouts: Layouts, at: { seed = Int64, floor = Int32, layout = Int32 }, rng: Int64, prev: Option[Run.State]): Run.State =
        let (floor, layout, seed) = (at#floor, at#layout, at#seed);
        let board = boardOf(layouts, floor, layout);
        let ctx = { rules = docs#rules, items = docs#items, enemies = docs#enemies, cards = docs#cards, board = board };
        let (placement, rest) = placementOf(docs, floor, board, rng);
        let arrival = { floor = floor, layout = layout, seed = seed, rng = rest };
        match prev {
            case Some(before) => Run.enter(before, ctx, arrival, placement)
            case None         => Run.start(ctx, arrival, placement)
        }

    /// 敵とアイテムの置き場を選ぶ。罠と物の候補を字で置いた盤は字のとおりに、そうでない盤は物を乱数で床に置く。
    def placementOf(docs: Docs, floor: Int32, board: Board.Board, rng: Int64): (Run.Placement, Int64) =
        let plan = FloorsDoc.floorOf(floor, docs#floors);
        let countOf = (kind: Enemy.Kind) -> plan |> Option.map(FloorsDoc.enemyCount(kind)) |> Option.getWithDefault(0);
        let kinds = Enemy.all() |> List.filter(kind -> kind != Enemy.Kind.Swarm) |> List.flatMap(kind -> List.repeat(countOf(kind), kind));
        let dist = Board.walkDistances(board#start, board);
        let marked = Set.union(Map.keysOf(board#traps), Map.keysOf(board#spots));
        let open = Board.cells(board) |> List.filter(c -> Board.tileAt(c, board) == Board.Tile.Floor and c != board#start and Map.memberOf(c, dist) and not Set.memberOf(c, marked));
        let far = open |> List.filter(c -> Map.getWithDefault(c, 0, dist) >= RulesDoc.rec(docs#rules)#enemyMinSteps);
        let (singleCells, afterSingles) = Rng.pick(List.length(kinds), far, rng);
        let (swarmCells, afterEnemies) = swarmsOf(board, countOf(Enemy.Kind.Swarm), far, List.toSet(singleCells), afterSingles);
        let enemyCells = singleCells ::: swarmCells;
        let (food, afterFood) = plan |> Option.map(f -> foodOf(f, afterEnemies)) |> Option.getWithDefault((Nil, afterEnemies));
        let fixed = plan |> Option.map(f -> f#items) |> Option.getWithDefault(Nil);
        let (items, afterItems) =
            if (Board.hasSpots(board)) placeOnSpots(board, food, fixed, afterFood)
            else scatter(open |> List.filter(c -> not List.memberOf(c, enemyCells)), fixed ::: food, afterFood);
        ({ enemies = List.zip(kinds, singleCells) ::: (swarmCells |> List.map(c -> (Enemy.Kind.Swarm, c))), items = items, traps = board#traps }, afterItems)

    /// 群れの組を groups 組置く: 組ごとに空いたマスを 1 つ選び、その隣（8 方向・角を抜けない）の空いたマスから 2 つ足す（足りなければ置ける数だけ）。
    def swarmsOf(board: Board.Board, groups: Int32, far: List[Cell], taken: Set[Cell], rng: Int64): (List[Cell], Int64) =
        let farSet = List.toSet(far);
        let (placed, rest, _) = List.range(0, groups) |> List.foldLeft((acc, _) -> {
            let (cells, state, used) = acc;
            let (heads, afterHead) = Rng.pick(1, far |> List.filter(c -> not Set.memberOf(c, used)), state);
            match heads {
                case head :: _ =>
                    let beside = Board.neighbors8(head) |> List.filter(c -> Set.memberOf(c, farSet) and not Set.memberOf(c, used) and Board.isStepOpen(head, c, board));
                    let (mates, afterMates) = Rng.pick(Int32.min(2, List.length(beside)), beside, afterHead);
                    let group = head :: mates;
                    (cells ::: group, afterMates, Set.union(used, List.toSet(group)))
                case Nil => (cells, afterHead, used)
            }
        }, (Nil, rng, taken));
        (placed, rest)

    /// 物を床の空いたマスへ乱数で置く。
    def scatter(free: List[Cell], items: List[Item.Kind], rng: Int64): (List[(Item.Kind, Cell)], Int64) =
        let (cells, rest) = Rng.pick(List.length(items), free, rng);
        (List.zip(items, cells), rest)

    /// 物を字の候補へ置く: 食べ物は食べ物の候補から、items は物の候補の数まで重複なしで引き、銀の候補には銀の剣か銀の盾。
    def placeOnSpots(board: Board.Board, food: List[Item.Kind], fixed: List[Item.Kind], rng: Int64): (List[(Item.Kind, Cell)], Int64) =
        let (foodCells, afterFood) = Rng.pick(List.length(food), Board.spotsOf(Board.Spot.Food, board), rng);
        let itemSpots = Board.spotsOf(Board.Spot.Item, board);
        let count = Int32.min(List.length(itemSpots), List.length(fixed));
        let (itemCells, afterCells) = Rng.pick(count, itemSpots, afterFood);
        let (drawn, afterDrawn) = Rng.pick(count, fixed, afterCells);
        let (silvers, afterSilver) = Board.spotsOf(Board.Spot.Silver, board) |> List.foldLeft((acc, cell) -> {
            let (placed, state) = acc;
            let (i, next) = Rng.below(2, state);
            ((if (i == 0) Item.Kind.SilverSword else Item.Kind.SilverShield, cell) :: placed, next)
        }, (Nil, afterDrawn));
        (List.zip(food, foodCells) ::: List.zip(drawn, itemCells) ::: List.reverse(silvers), afterSilver)

    /// 階に置く食べ物: パン（breadOdds の重みで 0, 1, 2… 個）と、meatChance で肉 1 つ。
    def foodOf(plan: FloorsDoc.FloorDef, rng: Int64): (List[Item.Kind], Int64) =
        let (breads, afterBread) = Rng.weighted(plan#breadOdds, rng);
        let (meat, afterMeat) = Rng.chance(plan#meatChance, afterBread);
        (List.repeat(breads, Item.Kind.Bread) ::: (if (meat) Item.Kind.Meat :: Nil else Nil), afterMeat)

    /// 置いた敵のマスの始まりからの歩数のうち一番近い物（置いた敵がいなければ None）。
    pub def nearestEnemySteps(board: Board.Board, runState: Run.State): Option[Int32] =
        let dist = Board.walkDistances(board#start, board);
        Run.enemiesOf(runState) |> List.filterMap(e -> Map.get(e#at, dist)) |> List.minimum

    /// 隣（8 方向）にほかの小ゼリーがいない小ゼリーのマス（群れが組で置かれていれば空）。
    pub def straySwarms(runState: Run.State): List[Cell] =
        let swarms = Run.enemiesOf(runState) |> List.filter(e -> e#kind == Enemy.Kind.Swarm) |> List.map(e -> e#at);
        let occupied = List.toSet(swarms);
        swarms |> List.filter(c -> not List.exists(n -> Set.memberOf(n, occupied), Board.neighbors8(c)))

    /// 始まりから階段まで、罠を踏まずに歩いて着けるか。
    pub def reachesStairs(board: Board.Board): Bool =
        let dist = Board.walkDistancesAvoiding(Map.keysOf(board#traps), board#start :: Nil, board);
        Board.cells(board) |> List.exists(c -> Board.tileAt(c, board) == Board.Tile.Stairs and Map.memberOf(c, dist))

    /// 歩けるのに、始まりから角を抜けずに歩いて着けないマス（読む順）。
    pub def unreachableCells(board: Board.Board): List[Cell] =
        let dist = Board.walkDistances(board#start, board);
        Board.cells(board) |> List.filter(c -> Board.isWalkable(c, board) and not Map.memberOf(c, dist))

    /// 字の候補のある盤で、床のアイテムが全部、種類に合う候補のマスにあるか（食べ物は %・銀の剣と銀の盾は &・ほかは $）。
    pub def itemsOnSpots(board: Board.Board, runState: Run.State): Bool =
        Run.rec(runState)#items |> Map.forAll((cell, kind) -> Map.get(cell, board#spots) == Some(spotOf(kind)))

    def spotOf(kind: Item.Kind): Board.Spot =
        if (Item.familyOf(kind) == Item.Family.Food) Board.Spot.Food
        else if (kind == Item.Kind.SilverSword or kind == Item.Kind.SilverShield) Board.Spot.Silver
        else Board.Spot.Item

    /// 床のアイテムが全部、床（階段でも始まりでもない）のマスにあるか。
    pub def itemsOnFloor(board: Board.Board, runState: Run.State): Bool =
        Run.rec(runState)#items |> Map.keysOf |> Set.forAll((c: Cell) -> Board.tileAt(c, board) == Board.Tile.Floor and c != board#start)
}
