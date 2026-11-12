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

    /// 8 方向。並びは同点の時に選ぶ順（上から時計回り）。
    pub enum Dir8 with Eq, Order, ToString {
        case Up
        case UpRight
        case Right
        case DownRight
        case Down
        case DownLeft
        case Left
        case UpLeft
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
    ///   cutsCorner  … 斜めの 2 つの横のマスのどちらかがこれなら、その斜めには動けないし殴れない
    pub type alias Traits = { walkable = Bool, flyable = Bool, stopsArrow = Bool, blocksSight = Bool, cutsCorner = Bool }

    pub def traitsOf(tile: Tile): Traits = match tile {
        case Tile.Floor  => { walkable = true,  flyable = true,  stopsArrow = false, blocksSight = false, cutsCorner = false }
        case Tile.Wall   => { walkable = false, flyable = false, stopsArrow = true,  blocksSight = true,  cutsCorner = true }
        case Tile.Pillar => { walkable = false, flyable = false, stopsArrow = true,  blocksSight = true,  cutsCorner = true }
        case Tile.Rock   => { walkable = false, flyable = false, stopsArrow = true,  blocksSight = false, cutsCorner = true }
        case Tile.Water  => { walkable = false, flyable = true,  stopsArrow = false, blocksSight = false, cutsCorner = false }
        case Tile.Stairs => { walkable = true,  flyable = true,  stopsArrow = false, blocksSight = false, cutsCorner = false }
    }

    /// 1 階の盤。
    ///   tiles   … 盤の中の全マス（盤の外は壁として読む）
    ///   rooms   … 部屋のマス → 部屋の番号（1 から。通路と壁は入れない＝番号 0）
    ///   views   … 部屋の番号 → 入った時に見たことにするマス（部屋と、それを囲む壁）
    ///   start   … 主人公の始まり
    ///   torches … 松明の印（壁のマス）
    ///   traps   … 罠のマス（部屋の床）
    ///   spots   … 物を置いてよいマスの候補（部屋の床）
    pub type alias Board = {
        cols = Int32,
        rows = Int32,
        tiles = Map[Cell, Tile],
        rooms = Map[Cell, Int32],
        views = Map[Int32, Set[Cell]],
        start = Cell,
        torches = Set[Cell],
        traps = Map[Cell, Trap.Kind],
        spots = Map[Cell, Spot]
    }

    /// 物を置いてよいマスの候補の種類（何を置くかは Floor が乱数で決める）。
    ///   Food   … パン・肉の候補（floors の breadOdds・meatChance で引いた数だけ選ぶ）
    ///   Item   … floors の items から重複なしで引いた物の候補
    ///   Silver … 銀の剣か銀の盾のどちらか
    pub enum Spot with Eq, Order, ToString {
        case Food
        case Item
        case Silver
    }

    /// 地図の 1 字の読み。corridor は通路の床（部屋の番号 0）。
    type alias Glyph = { tile = Tile, corridor = Bool, start = Bool, torch = Bool, trap = Option[Trap.Kind], spot = Option[Spot] }

    /// 地図の字 → マス。字の一覧は floors の Doc の note にも書く。
    def glyphOf(ch: Char): Option[Glyph] =
        let plain = tile -> { tile = tile, corridor = false, start = false, torch = false, trap = None, spot = None };
        let spotted = spot -> Some({ spot = Some(spot) | plain(Tile.Floor) });
        match ch {
            case '#' => Some(plain(Tile.Wall))
            case 't' => Some({ torch = true | plain(Tile.Wall) })
            case '.' => Some(plain(Tile.Floor))
            case ',' => Some({ corridor = true | plain(Tile.Floor) })
            case '@' => Some({ start = true | plain(Tile.Floor) })
            case 'P' => Some(plain(Tile.Pillar))
            case 'r' => Some(plain(Tile.Rock))
            case '~' => Some(plain(Tile.Water))
            case '>' => Some(plain(Tile.Stairs))
            case '%' => spotted(Spot.Food)
            case '$' => spotted(Spot.Item)
            case '&' => spotted(Spot.Silver)
            case _   => Trap.fromGlyph(ch) |> Option.map(kind -> { trap = Some(kind) | plain(Tile.Floor) })
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
          torches = read |> List.filter(e -> snd(e)#torch) |> List.map(fst) |> List.toSet,
          traps = read |> List.filterMap(e -> snd(e)#trap |> Option.map(kind -> (fst(e), kind))) |> List.toMap,
          spots = read |> List.filterMap(e -> snd(e)#spot |> Option.map(spot -> (fst(e), spot))) |> List.toMap }

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

    /// 壁・柱・岩（角を作る固いマス）か。
    pub def isSolid(cell: Cell, b: Board): Bool = traitsAt(cell, b)#cutsCorner

    /// from から隣の to へ角を抜けずに移れるか（上下左右の隣はいつも抜けられる）。
    /// 斜めなら、2 つの横のマス (to の列, from の行) と (from の列, to の行) のどちらも角を作らない時だけ。
    pub def isCornerOpen(origin: Cell, target: Cell, b: Board): Bool =
        let ((fx, fy), (tx, ty)) = (origin, target);
        fx == tx or fy == ty or (not traitsAt((tx, fy), b)#cutsCorner and not traitsAt((fx, ty), b)#cutsCorner)

    /// 隣のマスどうしで、角を抜けずに移れるか（隣でなければ false）。
    pub def isStepOpen(origin: Cell, target: Cell, b: Board): Bool =
        chebyshev(origin, target) == 1 and isCornerOpen(origin, target, b)

    /// 部屋の番号（通路と壁は 0）。
    pub def roomAt(cell: Cell, b: Board): Int32 = Map.getWithDefault(cell, 0, b#rooms)

    /// cell に立った時に見たことにするマス: 部屋の中なら部屋全体とそれを囲む壁、通路なら 8 近傍。
    pub def viewFrom(cell: Cell, b: Board): Set[Cell] =
        Map.get(roomAt(cell, b), b#views) |> Option.getWithDefault(List.toSet(cell :: neighbors8(cell)))

    /// spot の候補のマス（読む順）。
    pub def spotsOf(spot: Spot, b: Board): List[Cell] =
        cells(b) |> List.filter(c -> Map.get(c, b#spots) == Some(spot))

    /// 罠か物の候補を字で置いた盤か（置いていない盤は、物を乱数で床に置く）。
    pub def hasSpots(b: Board): Bool = not Map.isEmpty(b#traps) or not Map.isEmpty(b#spots)

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

    /// 8 方向（同点の時に選ぶ順）。
    pub def dirs8(): List[Dir8] =
        Dir8.Up :: Dir8.UpRight :: Dir8.Right :: Dir8.DownRight :: Dir8.Down :: Dir8.DownLeft :: Dir8.Left :: Dir8.UpLeft :: Nil

    /// 上下左右を 8 方向の同じ向きへ。
    pub def toDir8(dir: Dir): Dir8 = match dir {
        case Dir.Up    => Dir8.Up
        case Dir.Right => Dir8.Right
        case Dir.Down  => Dir8.Down
        case Dir.Left  => Dir8.Left
    }

    def delta8(dir: Dir8): (Int32, Int32) = match dir {
        case Dir8.Up        => (0, -1)
        case Dir8.UpRight   => (1, -1)
        case Dir8.Right     => (1, 0)
        case Dir8.DownRight => (1, 1)
        case Dir8.Down      => (0, 1)
        case Dir8.DownLeft  => (-1, 1)
        case Dir8.Left      => (-1, 0)
        case Dir8.UpLeft    => (-1, -1)
    }

    /// cell から dir へ 1 マス先。
    pub def step8(dir: Dir8, cell: Cell): Cell =
        let ((dx, dy), (x, y)) = (delta8(dir), cell);
        (x + dx, y + dy)

    /// dir へ進んだ時に後ろ側になる隣か（dir の向きと逆の成分を持つ: 来たマスとその横の 2 つ）。
    pub def isBehind(dir: Dir8, at: Cell, c: Cell): Bool =
        let ((dx, dy), (ax, ay), (cx, cy)) = (delta8(dir), at, c);
        (cx - ax) * dx + (cy - ay) * dy < 0

    /// at から隣の c への向き（隣でなければ None）。
    pub def dirTo(at: Cell, c: Cell): Option[Dir8] = dirs8() |> List.find(dir -> step8(dir, at) == c)

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

    /// from から passable なマスだけを上下左右に通って着ける各マスまでの歩数（from は 0）。部屋分けに使う。
    def distancesWithin(passable: Cell -> Bool, origin: Cell): Map[Cell, Int32] =
        spread(cell -> neighbors4(cell) |> List.filter(passable), origin :: Nil, 0, Map#{origin => 0})

    /// 幅優先: frontier の各マスから next が返す隣へ 1 歩ずつ広げる。
    def spread(next: Cell -> List[Cell], frontier: List[Cell], depth: Int32, seen: Map[Cell, Int32]): Map[Cell, Int32] =
        let (more, grown) = frontier |> List.foldLeft((state, cell) ->
            next(cell) |> List.foldLeft((inner, c) -> {
                let (found, known) = inner;
                if (Map.memberOf(c, known)) inner
                else (c :: found, Map.insert(c, depth + 1, known))
            }, state), (Nil, seen));
        if (List.isEmpty(more)) grown else spread(next, more, depth + 1, grown)

    /// 歩けるマスを 8 方向（角は抜けない）に通った、origins のどれかからの歩数（origins は 0）。
    pub def walkDistancesFrom(origins: List[Cell], b: Board): Map[Cell, Int32] =
        let walkNext = (cell: Cell) -> neighbors8(cell) |> List.filter(c -> isWalkable(c, b) and isCornerOpen(cell, c, b));
        spread(walkNext, origins, 0, origins |> List.map(c -> (c, 0)) |> List.toMap)

    /// from から歩けるマスを 8 方向（角は抜けない）に通った歩数。
    pub def walkDistances(origin: Cell, b: Board): Map[Cell, Int32] = walkDistancesFrom(origin :: Nil, b)

    /// 矢の届く範囲（距離 nearest..farthest）。
    pub type alias Reach = { nearest = Int32, farthest = Int32 }

    /// origin から dir へ、矢が届くマス（距離 reach の中・矢を止めるマスの手前まで。敵では止めない）。
    pub def lane(reach: Reach, dir: Dir, origin: Cell, b: Board): List[Cell] =
        List.range(1, reach#farthest + 1)
            |> List.map(k -> (k, ahead(dir, k, origin)))
            |> List.takeWhile(e -> not stopsArrow(snd(e), b))
            |> List.filter(e -> fst(e) >= reach#nearest)
            |> List.map(snd)
}
