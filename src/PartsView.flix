// PartsView — 2D の部品（純粋）: 字・絵・角の丸い板・帯・小さな絵（ハート・マス目・足あと・階段・砂時計）と、持ち物が発動する条件の絵。
// 上の帯と下の帯（HudView）・場面の上の印（MarkView）・カード（CardView）が同じ部品を使う。
// 寸法は 540×960 の design の px。
mod PartsView {
    use fge::{Color, FontAtlas, RawDraw, Render, TextDraw, Vec2}

    /// 持ち物が発動する条件・カードの効き（小さな絵の形）。
    pub enum Cue with Eq {
        /// 主人公から上下左右へ伸びる列（腕のマスの数）に敵が入ると（絵は十字と、上の先の敵 1 体）。
        case Line(Int32)
        /// まわりの n 体に囲まれると。
        case Surround(Int32)
        /// HP が最大のこの割合以下になると。
        case LowHp(Float64)
        /// 殴った敵を 1 マス押し出す（主人公・敵・押し出す先の矢じり）。
        case Push
        /// 満腹度の減りが遅い（砂時計）。
        case Slow
    }

    pub def p(x: Float64, y: Float64): Vec2.Vec2 = { x = x, y = y }

    pub def text(atlas: FontAtlas, s: String, at: Vec2.Vec2, size: Float64, color: Color, z: Int32): Render.PlacedItem =
        { at = at, item = Render.textTinted(s, atlas, size, color, z) }

    pub def centered(atlas: FontAtlas, s: String, center: Vec2.Vec2, size: Float64, color: Color, z: Int32): Render.PlacedItem =
        TextDraw.centeredAt({ text = s, atlas = atlas, size = size, color = color, z = z, center = center })

    /// ふちの色 rim を 8 方向へずらして敷いた上に字を置く（場面の上でも読める数）。
    pub def rimmed(atlas: FontAtlas, s: String, center: Vec2.Vec2, size: Float64, color: { fill = Color, rim = Color }, z: Int32): List[Render.PlacedItem] =
        let d = Float64.max(1.5, size * 0.06);
        let around = (-1, -1) :: (0, -1) :: (1, -1) :: (-1, 0) :: (1, 0) :: (-1, 1) :: (0, 1) :: (1, 1) :: Nil;
        List.append(around |> List.map(match (dx, dy) -> centered(atlas, s, p(center#x + Int32.toFloat64(dx) * d, center#y + Int32.toFloat64(dy) * d), size, color#rim, z)),
                    centered(atlas, s, center, size, color#fill, z + 1) :: Nil)

    /// 絵を真ん中 center に、長い辺が side になる大きさで置く。
    pub def icon(pic: Art.Picture, center: Vec2.Vec2, side: Float64, z: Int32): Render.PlacedItem =
        { at = center, item = Render.sprite(pic#name, z) |> Render.scaled(side / Float64.max(pic#width, pic#height)) }

    /// 角の丸い板（ふちの色 edge・ふちの太さ line）。
    pub def plate(at: Vec2.Vec2, size: Vec2.Vec2, r: Float64, fill: Render.Item, edge: Color, line: Float64, z: Int32): List[Render.PlacedItem] =
        { at = Vec2.sub(at, p(line, line)), item = RawDraw.box(Vec2.add(size, p(line * 2.0, line * 2.0)), edge, z) |> Render.rounded(r + line) }
            :: { at = at, item = fill |> Render.rounded(r) } :: Nil

    /// 上から下へ透け具合の変わる帯（左上 at・大きさ size・色 color・上の不透明さ a0・下の不透明さ a1）。
    pub def band(at: Vec2.Vec2, size: Vec2.Vec2, color: Color, alpha: { top = Float64, bottom = Float64 }, z: Int32): Render.PlacedItem =
        let v = (x: Float64, y: Float64, a: Float64) -> { pos = p(x, y), color = color, alpha = a };
        let (x0, y0, x1, y1) = (at#x, at#y, at#x + size#x, at#y + size#y);
        { at = Vec2.zero(), item = Render.gradPolygon(v(x0, y0, alpha#top) :: v(x1, y0, alpha#top) :: v(x1, y1, alpha#bottom) :: v(x0, y1, alpha#bottom) :: Nil, z) }

    /// 左から右へ透け具合の変わる帯。
    pub def sideBand(at: Vec2.Vec2, size: Vec2.Vec2, color: Color, alpha: { left = Float64, right = Float64 }, z: Int32): Render.PlacedItem =
        let v = (x: Float64, y: Float64, a: Float64) -> { pos = p(x, y), color = color, alpha = a };
        let (x0, y0, x1, y1) = (at#x, at#y, at#x + size#x, at#y + size#y);
        { at = Vec2.zero(), item = Render.gradPolygon(v(x0, y0, alpha#left) :: v(x1, y0, alpha#right) :: v(x1, y1, alpha#right) :: v(x0, y1, alpha#left) :: Nil, z) }

    pub def faded(alpha: Float64, g: Render.PlacedItem): Render.PlacedItem = { item = g#item |> Render.fade(alpha) | g }

    /// 絶対座標の頂点で組んだ多角形。
    pub def shape(vertices: List[Vec2.Vec2], color: Color, z: Int32): Render.PlacedItem =
        { at = Vec2.zero(), item = RawDraw.polygon(vertices, color, z) }

    /// 横に伸びる棒（左上 at・長さ length・太さ h・中身の割合 ratio・色 top/bottom）。中身が無ければ地だけ。
    pub def bar(at: Vec2.Vec2, size: Vec2.Vec2, colors: { edge = Color, back = Color }, z: Int32): List[Render.PlacedItem] =
        { at = Vec2.sub(at, p(2.0, 2.0)), item = RawDraw.box(Vec2.add(size, p(4.0, 4.0)), colors#edge, z) |> Render.rounded(size#y / 2.0 + 2.0) }
            :: { at = at, item = RawDraw.box(size, colors#back, z + 1) |> Render.rounded(size#y / 2.0) } :: Nil

    /// 棒の中身（左上 at・長さ length・太さ h）。短すぎる時は丸めが潰れないよう太さまで伸ばす。
    pub def fill(at: Vec2.Vec2, length: Float64, h: Float64, colors: { top = Color, bottom = Color }, z: Int32): List[Render.PlacedItem] =
        if (length <= 0.5) Nil
        else { at = at, item = Render.vgrad(p(Float64.max(h, length), h), colors, z) |> Render.rounded(h / 2.0) } :: Nil

    // ── 小さな絵（ピクトグラム）──

    /// ハート（真ん中 c・半幅 r）: ふちの色で一回り大きく敷き、上に面、左上に照り。
    pub def heart(t: ThemeDoc.Rec, c: Vec2.Vec2, r: Float64, z: Int32): List[Render.PlacedItem] =
        let body = (k: Float64, color: Color, zz: Int32) ->
            RawDraw.circleAt(p(c#x - r * 0.5, c#y - r * 0.28), r * 0.56 * k, color, zz)
                :: RawDraw.circleAt(p(c#x + r * 0.5, c#y - r * 0.28), r * 0.56 * k, color, zz)
                :: shape(p(c#x - r * 1.02 * k, c#y - r * 0.1) :: p(c#x + r * 1.02 * k, c#y - r * 0.1) :: p(c#x, c#y + r * 0.95 * k) :: Nil, color, zz) :: Nil;
        List.flatten(body(1.22, t#hpEdge, z) :: body(1.0, t#hpBottom, z + 1)
            :: (RawDraw.circleAt(p(c#x - r * 0.55, c#y - r * 0.4), r * 0.26, t#hpTop, z + 2) :: Nil) :: Nil)

    /// 星（真ん中 c・半径 r）: ふちの色で一回り大きく敷いて面を重ねる。
    pub def star(c: Vec2.Vec2, r: Float64, colors: { face = Color, edge = Color }, z: Int32): List[Render.PlacedItem] =
        RawDraw.starAt(c, 5, { outer = r * 1.2, inner = r * 0.62 }, colors#edge, z) :: RawDraw.starAt(c, 5, { outer = r, inner = r * 0.48 }, colors#face, z + 1) :: Nil

    /// 盤のマス 1 つ（左上 at・一辺 side）。上から下へ少し暗くして面に見せる。
    def tile(t: ThemeDoc.Rec, at: Vec2.Vec2, side: Float64, color: Color, z: Int32): Render.PlacedItem =
        { at = at, item = Render.vgrad(p(side, side), { top = color, bottom = Color.mix(color, t#shade, 0.35) }, z) |> Render.rounded(side * 0.22) }

    /// マス目の絵: 真ん中 c から (dx, dy) マスずれた所に色 color のマス（マスの一辺 side・間 gap）。
    def tileAt(t: ThemeDoc.Rec, c: Vec2.Vec2, side: Float64, gap: Float64, z: Int32, cell: ((Int32, Int32), Color)): Render.PlacedItem =
        let ((dx, dy), color) = cell;
        let pitch = side + gap;
        tile(t, p(c#x - side / 2.0 + Int32.toFloat64(dx) * pitch, c#y - side / 2.0 + Int32.toFloat64(dy) * pitch), side, color, z)

    /// 発動の条件・カードの効きの絵（真ん中 c）。
    pub def cue(t: ThemeDoc.Rec, c: Vec2.Vec2, q: Cue, z: Int32): List[Render.PlacedItem] =
        let floor = Color.mix(t#mapFloor, t#shade, 0.55);
        match q {
            case Cue.Line(arms) =>
                let arm = (dx: Int32, dy: Int32) -> List.range(1, arms + 1) |> List.map(k -> ((dx * k, dy * k), t#markArrow));
                let cells = ((0, 0), t#mapHero) :: ((0, 0 - arms - 1), t#mapEnemy) :: List.flatten(arm(0, -1) :: arm(1, 0) :: arm(0, 1) :: arm(-1, 0) :: Nil);
                let side = if (arms <= 1) 10.0 else 7.5;
                cells |> List.map(tileAt(t, p(c#x, c#y + side * 0.6), side, 1.5, z))
            case Cue.Surround(n) =>
                let ring = (-1, -1) :: (1, -1) :: (1, 1) :: (-1, 1) :: (0, -1) :: (1, 0) :: (0, 1) :: (-1, 0) :: Nil;
                let foes = List.take(n, ring) |> List.toSet;
                let colorAt = (d: (Int32, Int32)) -> if (d == (0, 0)) t#mapHero else if (Set.memberOf(d, foes)) t#mapEnemy else floor;
                List.range(-1, 2) |> List.flatMap(dy -> List.range(-1, 2) |> List.map(dx -> ((dx, dy), colorAt((dx, dy)))))
                    |> List.map(tileAt(t, c, 14.0, 2.0, z))
            case Cue.LowHp(ratio) =>
                let barAt = p(c#x - 26.0, c#y + 8.0);
                List.flatten(heart(t, p(c#x, c#y - 10.0), 10.0, z)
                    :: bar(barAt, p(52.0, 10.0), { edge = t#hpEdge, back = t#hpBack }, z)
                    :: fill(barAt, Float64.max(8.0, 52.0 * ratio), 10.0, { top = t#hpTop, bottom = t#hpBottom }, z + 2) :: Nil)
            case Cue.Push =>
                let cells = ((-1, 0), t#mapHero) :: ((0, 0), t#mapEnemy) :: ((1, 0), floor) :: Nil;
                let tip = p(c#x + 16.0, c#y);
                List.append(cells |> List.map(tileAt(t, c, 14.0, 2.0, z)),
                            shape(p(tip#x - 5.0, tip#y - 6.0) :: p(tip#x + 4.0, tip#y) :: p(tip#x - 5.0, tip#y + 6.0) :: Nil, t#markStrike, z + 1) :: Nil)
            case Cue.Slow => hourglass(c, t#ink, z)
        }

    /// 足あとの印（左右 2 つ）。
    pub def steps(c: Vec2.Vec2, color: Color, z: Int32): List[Render.PlacedItem] =
        let foot = (dx: Float64, dy: Float64) ->
            RawDraw.ellipseAt(p(c#x + dx, c#y + dy - 3.0), p(3.6, 5.2), color, z) :: RawDraw.circleAt(p(c#x + dx, c#y + dy + 5.0), 2.6, color, z) :: Nil;
        List.append(foot(-5.0, -2.0), foot(5.0, 2.0))

    /// 階段の印（左上から右下へ 3 段で降りる形と、下向きの矢じり）。
    pub def stairs(c: Vec2.Vec2, color: Color, z: Int32): List[Render.PlacedItem] =
        let v = (x: Float64, y: Float64) -> p(c#x + x, c#y + y);
        shape(v(-11.0, -9.0) :: v(-4.0, -9.0) :: v(-4.0, -3.0) :: v(3.0, -3.0) :: v(3.0, 3.0) :: v(10.0, 3.0) :: v(10.0, 10.0) :: v(-11.0, 10.0) :: Nil, color, z)
            :: shape(v(3.0, -12.0) :: v(11.0, -12.0) :: v(7.0, -6.0) :: Nil, color, z) :: Nil

    /// 砂時計の印。
    pub def hourglass(c: Vec2.Vec2, color: Color, z: Int32): List[Render.PlacedItem] =
        let v = (x: Float64, y: Float64) -> p(c#x + x, c#y + y);
        { at = v(-8.0, -11.0), item = RawDraw.box(p(16.0, 3.0), color, z) |> Render.rounded(1.5) }
            :: { at = v(-8.0, 8.0), item = RawDraw.box(p(16.0, 3.0), color, z) |> Render.rounded(1.5) }
            :: shape(v(-6.0, -8.0) :: v(6.0, -8.0) :: v(0.0, 0.0) :: Nil, color, z)
            :: shape(v(0.0, 0.0) :: v(6.0, 8.0) :: v(-6.0, 8.0) :: Nil, color, z) :: Nil
}
