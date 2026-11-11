// Board — 1 階の盤（マスの種類・部屋の番号・始まり・松明の印）と、マスの性質の表・隣・歩く距離（すべて純粋）。
// 盤は floors の Doc の文字の地図から組む（字の割り当ては glyphOf）。動く物（主人公・敵・アイテム）は Run が持つ。
mod Board {

    /// 盤のマス（列, 行）。(0, 0) が左上。
    pub type alias Cell = (Int32, Int32)

    /// 上下左右。並びは同点の時に選ぶ順（上・右・下・左）。
    pub enum Dir with Eq, Order, ToString {
        case Up
        case Right
        case Down
        case Left
    }

    /// マスの種類。判定はどれも traitsOf の表を引く。
    pub enum Tile with Eq, Order, ToString {
        case Floor
        case Wall
        case Pillar
        case Rock
        case Water
        case Stairs
    }

    /// マスの性質。
    ///   walkable    … 主人公と歩く敵が入れる
    ///   flyable     … 飛ぶ敵（コウモリ）が入れる
    ///   stopsArrow  … 矢がここで止まる（このマスには届かない）
    ///   blocksSight … 見通しを止める
    pub type alias Traits = { walkable = Bool, flyable = Bool, stopsArrow = Bool, blocksSight = Bool }

    pub def traitsOf(tile: Tile): Traits = match tile {
        case Tile.Floor  => { walkable = true,  flyable = true,  stopsArrow = false, blocksSight = false }
        case Tile.Wall   => { walkable = false, flyable = false, stopsArrow = true,  blocksSight = true }
        case Tile.Pillar => { walkable = false, flyable = false, stopsArrow = true,  blocksSight = true }
        case Tile.Rock   => { walkable = false, flyable = false, stopsArrow = true,  blocksSight = false }
        case Tile.Water  => { walkable = false, flyable = true,  stopsArrow = false, blocksSight = false }
        case Tile.Stairs => { walkable = true,  flyable = true,  stopsArrow = false, blocksSight = false }
    }

    /// 1 階の盤。
    ///   tiles   … 盤の中の全マス（盤の外は壁として読む）
    ///   rooms   … 部屋のマス → 部屋の番号（1 から。通路と壁は入れない＝番号 0）
    ///   views   … 部屋の番号 → 入った時に見たことにするマス（部屋と、それを囲む壁）
    ///   start   … 主人公の始まり
    ///   torches … 松明の印（壁のマス）
    pub type alias Board = {
        cols = Int32,
        rows = Int32,
        tiles = Map[Cell, Tile],
        rooms = Map[Cell, Int32],
        views = Map[Int32, Set[Cell]],
        start = Cell,
        torches = Set[Cell]
    }

    /// 地図の 1 字の読み。corridor は通路の床（部屋の番号 0）。
    type alias Glyph = { tile = Tile, corridor = Bool, start = Bool, torch = Bool }

    /// 地図の字 → マス。字の一覧は floors の Doc の note にも書く。
    def glyphOf(ch: Char): Option[Glyph] =
        let plain = tile -> Some({ tile = tile, corridor = false, start = false, torch = false });
        match ch {
            case '#' => plain(Tile.Wall)
            case 't' => Some({ tile = Tile.Wall, corridor = false, start = false, torch = true })
            case '.' => plain(Tile.Floor)
            case ',' => Some({ tile = Tile.Floor, corridor = true, start = false, torch = false })
            case '@' => Some({ tile = Tile.Floor, corridor = false, start = true, torch = false })
            case 'P' => plain(Tile.Pillar)
            case 'r' => plain(Tile.Rock)
            case '~' => plain(Tile.Water)
            case '>' => plain(Tile.Stairs)
            case _   => None
        }

    /// 文字の地図から盤を組む。長方形でない・知らない字・始まりが 1 つでない・階段が無いときは理由を返す。
    pub def fromRows(rows: List[String]): Result[String, Board] =
        let height = List.length(rows);
        let width = List.head(rows) |> Option.map(String.length) |> Option.getWithDefault(0);
        let glyphs = rows |> List.zipWithIndex |> List.flatMap(match (y, line) ->
            String.toList(line) |> List.zipWithIndex |> List.map(match (x, ch) -> ((x, y), ch, glyphOf(ch))));
        let unknown = glyphs |> List.find(match (_, _, g) -> Option.isEmpty(g));
        let read = glyphs |> List.filterMap(match (cell, _, g) -> g |> Option.map(glyph -> (cell, glyph)));
        let starts = read |> List.filter(e -> snd(e)#start) |> List.map(fst);
        let hasStairs = read |> List.exists(e -> snd(e)#tile == Tile.Stairs);
        if (height == 0 or width == 0) Err("地図が空")
        else if (List.exists(line -> String.length(line) != width, rows)) Err("地図の行の長さがそろっていない（${width} 字にそろえる）")
        else match (unknown, starts) {
            case (Some((cell, ch, _)), _) => Err("知らない字 '${ch}'（${cell}）")
            case (None, start :: Nil) if hasStairs => Ok(assemble(width, height, start, read))
            case (None, _ :: Nil) => Err("階段 '>' が無い")
            case (None, _) => Err("始まり '@' はちょうど 1 つ（${List.length(starts)} 個）")
        }

    def assemble(width: Int32, height: Int32, start: Cell, read: List[(Cell, Glyph)]): Board =
        let tiles = read |> List.map(e -> (fst(e), snd(e)#tile)) |> List.toMap;
        let roomable = read |> List.filter(e -> snd(e)#tile != Tile.Wall and not snd(e)#corridor) |> List.map(fst) |> List.toSet;
        let rooms = roomsOf(read |> List.map(fst), roomable);
        let views = rooms |> Map.foldLeftWithKey((acc, cell, room) ->
            Map.insertWith(Set.union, room, List.toSet(cell :: neighbors8(cell)), acc), Map.empty());
        { cols = width, rows = height, tiles = tiles, rooms = rooms, views = views, start = start,
          torches = read |> List.filter(e -> snd(e)#torch) |> List.map(fst) |> List.toSet }

    /// 部屋のマス（壁でも通路でもないマス）を上下左右でつながる塊に分け、読む順に 1, 2, … と番号を振る。
    def roomsOf(order: List[Cell], roomable: Set[Cell]): Map[Cell, Int32] =
        let (numbered, _) = order |> List.foldLeft((state, cell) -> {
            let (acc, next) = state;
            if (not Set.memberOf(cell, roomable) or Map.memberOf(cell, acc)) state
            else {
                let blob = distancesWithin(c -> Set.memberOf(c, roomable), cell) |> Map.keysOf;
                (Set.foldLeft((m, c) -> Map.insert(c, next, m), acc, blob), next + 1)
            }
        }, (Map.empty(), 1));
        numbered

    // ── 読む口 ──

    /// マスの種類（盤の外は壁）。
    pub def tileAt(cell: Cell, b: Board): Tile = Map.getWithDefault(cell, Tile.Wall, b#tiles)

    def traitsAt(cell: Cell, b: Board): Traits = traitsOf(tileAt(cell, b))

    pub def isWalkable(cell: Cell, b: Board): Bool = traitsAt(cell, b)#walkable

    pub def isFlyable(cell: Cell, b: Board): Bool = traitsAt(cell, b)#flyable

    pub def stopsArrow(cell: Cell, b: Board): Bool = traitsAt(cell, b)#stopsArrow

    /// 部屋の番号（通路と壁は 0）。
    pub def roomAt(cell: Cell, b: Board): Int32 = Map.getWithDefault(cell, 0, b#rooms)

    /// cell に立った時に見たことにするマス: 部屋の中なら部屋全体とそれを囲む壁、通路なら 8 近傍。
    pub def viewFrom(cell: Cell, b: Board): Set[Cell] =
        Map.get(roomAt(cell, b), b#views) |> Option.getWithDefault(List.toSet(cell :: neighbors8(cell)))

    /// 盤の全マス（読む順）。
    pub def cells(b: Board): List[Cell] =
        List.range(0, b#rows) |> List.flatMap(y -> List.range(0, b#cols) |> List.map(x -> (x, y)))

    // ── 向きと隣 ──

    /// 4 方向（同点の時に選ぶ順）。
    pub def dirs(): List[Dir] = Dir.Up :: Dir.Right :: Dir.Down :: Dir.Left :: Nil

    def delta(dir: Dir): (Int32, Int32) = match dir {
        case Dir.Up    => (0, -1)
        case Dir.Right => (1, 0)
        case Dir.Down  => (0, 1)
        case Dir.Left  => (-1, 0)
    }

    /// cell から dir へ n マス先。
    pub def ahead(dir: Dir, n: Int32, cell: Cell): Cell =
        let ((dx, dy), (x, y)) = (delta(dir), cell);
        (x + dx * n, y + dy * n)

    pub def step(dir: Dir, cell: Cell): Cell = ahead(dir, 1, cell)

    pub def opposite(dir: Dir): Dir = match dir {
        case Dir.Up    => Dir.Down
        case Dir.Right => Dir.Left
        case Dir.Down  => Dir.Up
        case Dir.Left  => Dir.Right
    }

    /// 上下左右の隣（上・右・下・左の順）。
    pub def neighbors4(cell: Cell): List[Cell] = dirs() |> List.map(dir -> step(dir, cell))

    /// 斜めも含めた 8 近傍。
    pub def neighbors8(cell: Cell): List[Cell] =
        let (x, y) = cell;
        (x - 1, y - 1) :: (x, y - 1) :: (x + 1, y - 1) :: (x - 1, y) :: (x + 1, y) :: (x - 1, y + 1) :: (x, y + 1) :: (x + 1, y + 1) :: Nil

    /// 縦横の差の大きい方（8 近傍なら 1）。
    pub def chebyshev(a: Cell, b: Cell): Int32 =
        let ((ax, ay), (bx, by)) = (a, b);
        Int32.max(Int32.abs(ax - bx), Int32.abs(ay - by))

    /// a から見て b が上下左右の直線上にあれば、その向きと距離。
    pub def lineTo(a: Cell, b: Cell): Option[(Dir, Int32)] =
        let ((ax, ay), (bx, by)) = (a, b);
        if (ax == bx and by < ay) Some((Dir.Up, ay - by))
        else if (ax == bx and by > ay) Some((Dir.Down, by - ay))
        else if (ay == by and bx > ax) Some((Dir.Right, bx - ax))
        else if (ay == by and bx < ax) Some((Dir.Left, ax - bx))
        else None

    // ── 歩く距離（幅優先） ──

    /// from から passable なマスだけを上下左右に通って着ける各マスまでの歩数（from は 0）。
    def distancesWithin(passable: Cell -> Bool, origin: Cell): Map[Cell, Int32] =
        spread(passable, origin :: Nil, 0, Map#{origin => 0})

    def spread(passable: Cell -> Bool, frontier: List[Cell], depth: Int32, seen: Map[Cell, Int32]): Map[Cell, Int32] =
        let (next, grown) = frontier |> List.foldLeft((state, cell) ->
            neighbors4(cell) |> List.foldLeft((inner, c) -> {
                let (more, known) = inner;
                if (Map.memberOf(c, known) or not passable(c)) inner
                else (c :: more, Map.insert(c, depth + 1, known))
            }, state), (Nil, seen));
        if (List.isEmpty(next)) grown else spread(passable, next, depth + 1, grown)

    /// from から歩けるマスを通った歩数。
    pub def walkDistances(origin: Cell, b: Board): Map[Cell, Int32] =
        distancesWithin(c -> isWalkable(c, b), origin)
}
