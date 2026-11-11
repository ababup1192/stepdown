// FloorMesh — 1 枚の地図の動かない物（床・壁・壁の上面・VOID・柱・岩・水場の底・階段・苔・松明の棒）を、
// 部品の束（Kit）から並べて材質ごとに 1 つの形へつなぐ（純粋）。水面は地図ごとに 1 枚の形。
//
// 座標: 1 マス = 1 m。マス (列, 行) の真ん中・床の上面が world の (列, 0, −行)。北（行の小さい方）が +Z、東が +X。
// 床の形違い・苔の置き場は、地図の鍵とマスの座標から決まる擬似乱数で選ぶ（同じ地図からは毎回同じ形）。
mod FloorMesh {
    use fge::{Log, Mat4, Mesh3d, Num, Scene3d, Vec3}
    use Board.{Cell, Dir, Tile}

    /// つないだ形 1 つ（材質 1 つ）。key = 材質の名前。
    pub type alias Group = { key = String, mesh = Mesh3d.Mesh3d, material = Option[Scene3d.Material3d] }

    /// 松明 1 本。world = 部品を置く行列（棒と炎が同じ行列）・pivot = 炎の真ん中（部品の座標）・flame = 炎の真ん中（world）・
    /// facing = 壁から離れる向き（長さ 1）。
    pub type alias Torch = { cell = Cell, world = Mat4.Mat4, pivot = Vec3.Vec3, flame = Vec3.Vec3, facing = Vec3.Vec3 }

    /// 1 枚の地図から作った物。waterLights = 水面の上に置く青い光の位置。
    pub type alias Built = {
        solids = List[Group],
        water = Option[Mesh3d.Mesh3d],
        torches = List[Torch],
        waterLights = List[Vec3.Vec3]
    }

    pub def empty(): Built = { solids = Nil, water = None, torches = Nil, waterLights = Nil }

    /// マスの真ん中・床の上面の world の点。
    pub def cellPoint(cell: Cell): Vec3.Vec3 = let (x, y) = cell; Vec3.v3(Int32.toFloat64(x), 0.0, Int32.toFloat64(0 - y))

    /// 向き dir の world の向き（長さ 1・水平）。
    def dirVector(dir: Dir): Vec3.Vec3 = match dir {
        case Dir.Up    => Vec3.v3(0.0, 0.0, 1.0)
        case Dir.Right => Vec3.v3(1.0, 0.0, 0.0)
        case Dir.Down  => Vec3.v3(0.0, 0.0, -1.0)
        case Dir.Left  => Vec3.v3(-1.0, 0.0, 0.0)
    }

    /// 部品の +Z（Blender の −Y・壁の手前の面）を dir へ向ける縦の軸のまわりの角。
    def yawOf(dir: Dir): Float64 = match dir {
        case Dir.Up    => 0.0
        case Dir.Right => Num.pi() / 2.0
        case Dir.Down  => Num.pi()
        case Dir.Left  => 0.0 - Num.pi() / 2.0
    }

    /// 地図の鍵 salt とマスから決まる乱数の状態。
    def seedOf(salt: Int64, cell: Cell): Int64 =
        let (x, y) = cell;
        Rng.mix(Rng.mix(salt, Int32.toInt64(x) + 7919i64), Int32.toInt64(y) + 104729i64)

    /// マスの置き場の行列（部品の原点 origin がマスの真ん中に来る・縦の軸のまわりに yaw）。
    def placeAt(cell: Cell, yaw: Float64, origin: Vec3.Vec3): Mat4.Mat4 =
        Mat4.mul(Mat4.translate(cellPoint(cell)), Mat4.mul(Mat4.rotateY(yaw), Mat4.translate(Vec3.mul(origin, -1.0))))

    /// 置く部品（ノードの名前・マス・角）。
    type alias Placed = { node = String, cell = Cell, yaw = Float64 }

    /// 地図 b を部品の束 kit で組む。salt = 地図の鍵（階と地図の番号から）。
    pub def build(kit: Kit.Kit, salt: Int64, b: Board.Board): Built =
        let cells = Board.cells(b);
        let placed = cells |> List.flatMap(cell -> piecesOf(salt, b, cell));
        let water = cells |> List.filter(c -> Board.tileAt(c, b) == Tile.Water);
        { solids = groupsOf(kit, placed),
          water = waterOf(kit, water),
          torches = b#torches |> Set.toList |> List.filterMap(c -> torchOf(kit, b, c)),
          waterLights = waterLightOf(water) |> Option.toList }

    /// マス 1 つの部品。
    def piecesOf(salt: Int64, b: Board.Board, cell: Cell): List[Placed] =
        let at = (node, yaw) -> { node = node, cell = cell, yaw = yaw };
        let rng = seedOf(salt, cell);
        let (turn, r1) = Rng.below(4, rng);
        let quarter = Int32.toFloat64(turn) * Num.pi() / 2.0;
        match Board.tileAt(cell, b) {
            case Tile.Floor  => floorOf(r1, cell) :: mossOf(r1, b, cell)
            case Tile.Stairs => at("STAIRS", 0.0) :: Nil
            case Tile.Pillar => floorOf(r1, cell) :: at("PILLAR", quarter) :: Nil
            case Tile.Rock   => floorOf(r1, cell) :: at("ROCK", Int32.toFloat64(turn) * 1.3) :: Nil
            case Tile.Water  => at("PIT", 0.0) :: Nil
            case Tile.Wall   =>
                if (faces(b, cell)) at("WALL", 0.0) :: at("WALL_TOP", 0.0) :: torchPiece(b, cell)
                else at("VOID", 0.0) :: Nil
        }

    /// 床の敷石（形違い 3 つと割れた敷石・向きも乱数）。
    def floorOf(rng: Int64, cell: Cell): Placed =
        let (pick, r2) = Rng.below(9, rng);
        let (turn, _) = Rng.below(4, r2);
        let node = if (pick == 0) "FLOOR_CRACK" else "FLOOR_${Int32.remainder(pick, 3)}";
        { node = node, cell = cell, yaw = Int32.toFloat64(turn) * Num.pi() / 2.0 }

    /// 壁か水に接する床にだけ、乱数で苔を置く。
    def mossOf(rng: Int64, b: Board.Board, cell: Cell): List[Placed] =
        let edge = Board.neighbors4(cell) |> List.exists(c -> { let t = Board.tileAt(c, b); t == Tile.Wall or t == Tile.Water });
        let (grows, r2) = Rng.chance(0.5, Rng.mix(rng, 31i64));
        let (kind, r3) = Rng.below(3, r2);
        let (turn, _) = Rng.below(16, r3);
        if (edge and grows) { node = "MOSS_${kind}", cell = cell, yaw = Int32.toFloat64(turn) * Num.pi() / 8.0 } :: Nil else Nil

    /// 壁でない物に 8 近傍で接する壁か（接しない壁は奥の黒い蓋だけ）。
    def faces(b: Board.Board, cell: Cell): Bool =
        Board.neighbors8(cell) |> List.exists(c -> Board.tileAt(c, b) != Tile.Wall)

    /// 松明の壁が向く向き（手前・左右・奥の順に、壁でないマスのある方）。
    def facingOf(b: Board.Board, cell: Cell): Option[Dir] =
        (Dir.Down :: Dir.Left :: Dir.Right :: Dir.Up :: Nil) |> List.find(dir -> Board.tileAt(Board.step(dir, cell), b) != Tile.Wall)

    def torchPiece(b: Board.Board, cell: Cell): List[Placed] =
        if (not Set.memberOf(cell, b#torches)) Nil
        else facingOf(b, cell) |> Option.map(dir -> { node = "TORCH", cell = cell, yaw = yawOf(dir) }) |> Option.toList

    def torchOf(kit: Kit.Kit, b: Board.Board, cell: Cell): Option[Torch] =
        forM (dir <- facingOf(b, cell); flame <- Kit.itemOf("FLAME", kit)) yield {
            let world = placeAt(cell, yawOf(dir), flame#origin);
            let center = flame#parts |> List.map(p -> Mesh3d.bounds(p#mesh)) |> List.head
                |> Option.map(bb -> Vec3.mul(Vec3.add(bb#min, bb#max), 0.5)) |> Option.getWithDefault(Vec3.v3(0.0, 1.0, 0.56));
            { cell = cell, world = world, pivot = center, flame = Mat4.apply(world, center), facing = dirVector(dir) }
        }

    /// 水面の青い光: 水のマスの重心に一番近い水のマスの上（水が無ければ None）。
    def waterLightOf(water: List[Cell]): Option[Vec3.Vec3] =
        let n = Int32.toFloat64(List.length(water));
        let mid = water |> List.foldLeft((acc, c) -> Vec3.add(acc, cellPoint(c)), Vec3.zero()) |> v -> Vec3.mul(v, 1.0 / Float64.max(1.0, n));
        water |> List.map(c -> (Vec3.length(Vec3.sub(cellPoint(c), mid)), c)) |> List.minimumBy((a, b) -> fst(a) <=> fst(b))
            |> Option.map(e -> Vec3.add(cellPoint(snd(e)), Vec3.v3(0.0, 1.2, 0.0)))

    // ── つなぐ ──

    /// 置いた部品を材質の名前ごとに集め、1 つの形にする（並びは初めて出た順）。
    def groupsOf(kit: Kit.Kit, placed: List[Placed]): List[Group] =
        let parts = placed |> List.flatMap(p -> Kit.itemOf(p#node, kit) |> Option.map(item ->
            item#parts |> List.map(part -> (part, placeAt(p#cell, p#yaw, item#origin)))) |> Option.getWithDefault(Nil));
        let (order, groups) = parts |> List.foldLeft((acc, e) -> {
            let (keys, m) = acc;
            let (part, world) = e;
            let next = if (Map.memberOf(part#key, m)) keys else part#key :: keys;
            (next, Map.insertWith((fresh, old) -> List.append(fresh, old), part#key, (part, world) :: Nil, m))
        }, (Nil, Map.empty()));
        List.reverse(order) |> List.filterMap(key -> Map.get(key, groups) |> Option.flatMap(members -> {
            let ordered = List.reverse(members);
            let material = List.head(ordered) |> Option.flatMap(e -> fst(e)#material);
            merged(ordered |> List.map(e -> (fst(e)#mesh, snd(e))), None) |> Option.map(mesh -> { key = key, mesh = mesh, material = material })
        }))

    /// 水面の板をつなぐ（uv は world の x・z から作るので、マスの継ぎ目で模様が切れない）。
    def waterOf(kit: Kit.Kit, cells: List[Cell]): Option[Mesh3d.Mesh3d] =
        Kit.itemOf("WATER", kit) |> Option.flatMap(item -> {
            let parts = cells |> List.flatMap(c -> item#parts |> List.map(p -> (p#mesh, placeAt(c, 0.0, item#origin))));
            if (List.isEmpty(parts)) None else merged(parts, Some(0.25))
        })

    def concatOf(f: (Mesh3d.Mesh3d, Mat4.Mat4) -> Vector[a], parts: List[(Mesh3d.Mesh3d, Mat4.Mat4)]): Vector[a] =
        Vector.flatten(List.toVector(List.map(match (m, w) -> f(m, w), parts)))

    /// 3 つずつ並んだ値を、点か向きとして行列で写す。
    def mapTriples(f: Vec3.Vec3 -> Vec3.Vec3, v: Vector[Float64]): Vector[Float64] =
        Vector.range(0, Vector.length(v) / 3) |> Vector.flatMap(i -> {
            let q = f(Vec3.v3(Vector.get(3 * i, v), Vector.get(3 * i + 1, v), Vector.get(3 * i + 2, v)));
            Vector#{q#x, q#y, q#z}
        })

    /// 形の列を world で写して 1 つにつなぐ。worldUv = Some(k) なら uv を world の (x, −z) × k にする。
    /// uv はどの形も持っている時だけ付ける。
    def merged(parts: List[(Mesh3d.Mesh3d, Mat4.Mat4)], worldUv: Option[Float64]): Option[Mesh3d.Mesh3d] =
        let (_, shifted) = parts |> List.foldLeft((acc, e) -> {
            let (base, out) = acc;
            let mesh = fst(e);
            (base + Mesh3d.vertexCount(mesh), Vector.map(i -> i + base, Mesh3d.indices(mesh)) :: out)
        }, (0, Nil));
        let positions = concatOf((m, w) -> mapTriples(Mat4.apply(w), Mesh3d.positions(m)), parts);
        let whole = Mesh3d.fromArrays({ positions = positions,
                                        normals = concatOf((m, w) -> mapTriples(Mat4.applyDir(w), Mesh3d.normals(m)), parts),
                                        colors = concatOf((m, _) -> Mesh3d.colors(m), parts),
                                        indices = Vector.flatten(List.toVector(List.reverse(shifted))), skin = None });
        let uvs = match worldUv {
            case Some(k) => Some(Vector.range(0, Vector.length(positions) / 3) |> Vector.flatMap(i ->
                                Vector#{Vector.get(3 * i, positions) * k, 0.0 - Vector.get(3 * i + 2, positions) * k}))
            case None if List.forAll(e -> Option.nonEmpty(Mesh3d.uvs(fst(e))), parts) =>
                Some(concatOf((m, _) -> Mesh3d.uvs(m) |> Option.getWithDefault(Vector.empty()), parts))
            case None => None
        };
        let textured = Result.flatMap(mesh -> uvs |> Option.map(uv -> Mesh3d.withUvs(uv, mesh)) |> Option.getWithDefault(Ok(mesh)), whole);
        match textured {
            case Ok(mesh) => Some(mesh)
            case Err(why) => Log.warnThen("[floor] 形をつなげない: ${why}", None)
        }
}
