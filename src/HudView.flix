// HudView — 3D の場面の上に重ねる 2D（純粋）: 上の帯（階・HP・剣と盾の枠・小さい地図）・下の帯（記録 1 行・持ち物 3 枠）・
// 場面の上の印（方向の予告のアイコン・眠りの z）・題名と終わりの板・終了の確認の窓。
// 字は階・数・記録・板にだけ使い、ほかは小さな絵で見せる。
// 寸法と色は 540×960 の design の px（手本は UI の下書き）。
mod HudView {
    use fge::{Color, FontAtlas, RawDraw, Render, TextDraw, Vec2}

    /// 予告のアイコン: 絵・足あと（動く）・階段（降りる）・砂時計（待つ）。
    pub enum Glyph with Eq {
        case Picture(Art.Icon)
        case Steps
        case Stairs
        case Hourglass
    }

    /// 方向の予告 1 つ。at = アイコンの真ん中・hurt = 受けるダメージ（無ければ 0）・color = 枠の色。
    pub type alias Badge = { at = Vec2.Vec2, glyph = Glyph, hurt = Int32, color = Color }

    /// 持ち物が発動する条件（枠の中の小さな絵の形）。
    pub enum Cue with Eq {
        /// 主人公から上下左右へ伸びる列に敵が入ると（絵は十字の列と、上の先の敵 1 体）。
        case Line
        /// まわりの n 体に囲まれると。
        case Surround(Int32)
        /// HP が最大のこの割合以下になると。
        case LowHp(Float64)
    }

    /// 持ち物の枠 1 つ。icon = 絵・count = 数・cue = 発動の条件・tier = ふちの色・held = 持っているか・glow = 上位を拾った光（0..1）。
    pub type alias Slot = { icon = Art.Icon, count = Int32, cue = Cue, tier = Color, held = Bool, glow = Float64 }

    /// 剣か盾の枠 1 つ。icon = 絵・tier = ふちの色・held = 持っているか。
    pub type alias Gear = { icon = Art.Icon, tier = Color, held = Bool }

    /// 小さい地図。cells = 見たマスと色・cols と rows = 盤の大きさ。
    pub type alias Minimap = { cols = Int32, rows = Int32, cells = List[((Int32, Int32), Color)] }

    /// 前に重ねる板（題名・倒れた・抜けた）。
    pub type alias Card = { title = String, prompt = String }

    /// 描くのに要る値。log = 記録の 1 行（無ければ空）。
    pub type alias In = {
        theme = ThemeDoc.Rec,
        time = Float64,
        floorLabel = String,
        hp = Int32,
        maxHp = Int32,
        gear = List[Gear],
        minimap = Minimap,
        log = String,
        slots = List[Slot],
        badges = List[Badge],
        sleepers = List[Vec2.Vec2],
        card = Option[Card],
        quitPrompt = Option[String],
        showBars = Bool
    }

    def width(): Float64 = 540.0
    def height(): Float64 = 960.0

    /// 下の UI の帯（記録と持ち物）の上の端。ここより下のタップは盤の入力にしない。
    pub def bandTop(): Float64 = 760.0
    def p(x: Float64, y: Float64): Vec2.Vec2 = { x = x, y = y }

    def text(atlas: FontAtlas, s: String, at: Vec2.Vec2, size: Float64, color: Color, z: Int32): Render.PlacedItem =
        { at = at, item = Render.textTinted(s, atlas, size, color, z) }

    def centered(atlas: FontAtlas, s: String, center: Vec2.Vec2, size: Float64, color: Color, z: Int32): Render.PlacedItem =
        TextDraw.centeredAt({ text = s, atlas = atlas, size = size, color = color, z = z, center = center })

    def icon(pic: Art.Picture, center: Vec2.Vec2, side: Float64, z: Int32): Render.PlacedItem =
        { at = center, item = Render.sprite(pic#name, z) |> Render.scaled(side / Float64.max(pic#width, pic#height)) }

    /// 角の丸い板（ふちの色 edge・ふちの太さ line）。
    def plate(at: Vec2.Vec2, size: Vec2.Vec2, r: Float64, fill: Render.Item, edge: Color, line: Float64, z: Int32): List[Render.PlacedItem] =
        { at = Vec2.sub(at, p(line, line)), item = RawDraw.box(Vec2.add(size, p(line * 2.0, line * 2.0)), edge, z) |> Render.rounded(r + line) }
            :: { at = at, item = fill |> Render.rounded(r) } :: Nil

    /// 上から下へ透け具合の変わる帯（色 color・上の不透明さ a0・下の不透明さ a1）。
    def band(y: Float64, h: Float64, color: Color, a0: Float64, a1: Float64, z: Int32): Render.PlacedItem =
        let v = (x: Float64, yy: Float64, a: Float64) -> { pos = p(x, yy), color = color, alpha = a };
        { at = Vec2.zero(), item = Render.gradPolygon(v(0.0, y, a0) :: v(width(), y, a0) :: v(width(), y + h, a1) :: v(0.0, y + h, a1) :: Nil, z) }

    def faded(alpha: Float64, g: Render.PlacedItem): Render.PlacedItem = { item = g#item |> Render.fade(alpha) | g }

    /// 絶対座標の頂点で組んだ多角形。
    def shape(vertices: List[Vec2.Vec2], color: Color, z: Int32): Render.PlacedItem =
        { at = Vec2.zero(), item = RawDraw.polygon(vertices, color, z) }

    // ── 小さな絵（ピクトグラム）──

    /// ハート（真ん中 c・半幅 r）: ふちの色で一回り大きく敷き、上に面、左上に照り。
    def heart(t: ThemeDoc.Rec, c: Vec2.Vec2, r: Float64, z: Int32): List[Render.PlacedItem] =
        let body = (k: Float64, color: Color, zz: Int32) ->
            RawDraw.circleAt(p(c#x - r * 0.5, c#y - r * 0.28), r * 0.56 * k, color, zz)
                :: RawDraw.circleAt(p(c#x + r * 0.5, c#y - r * 0.28), r * 0.56 * k, color, zz)
                :: shape(p(c#x - r * 1.02 * k, c#y - r * 0.1) :: p(c#x + r * 1.02 * k, c#y - r * 0.1) :: p(c#x, c#y + r * 0.95 * k) :: Nil, color, zz) :: Nil;
        List.flatten(body(1.22, t#hpEdge, z) :: body(1.0, t#hpBottom, z + 1)
            :: (RawDraw.circleAt(p(c#x - r * 0.55, c#y - r * 0.4), r * 0.26, t#hpTop, z + 2) :: Nil) :: Nil)

    /// 盤のマス 1 つ（左上 at・一辺 side）。上から下へ少し暗くして面に見せる。
    def tile(t: ThemeDoc.Rec, at: Vec2.Vec2, side: Float64, color: Color, z: Int32): Render.PlacedItem =
        { at = at, item = Render.vgrad(p(side, side), { top = color, bottom = Color.mix(color, t#shade, 0.35) }, z) |> Render.rounded(side * 0.22) }

    /// マス目の絵: 真ん中 c から (dx, dy) マスずれた所に色 color のマス（マスの一辺 side・間 gap）。
    def tileAt(t: ThemeDoc.Rec, c: Vec2.Vec2, side: Float64, gap: Float64, z: Int32, cell: ((Int32, Int32), Color)): Render.PlacedItem =
        let ((dx, dy), color) = cell;
        let pitch = side + gap;
        tile(t, p(c#x - side / 2.0 + Int32.toFloat64(dx) * pitch, c#y - side / 2.0 + Int32.toFloat64(dy) * pitch), side, color, z)

    /// 発動の条件の絵（真ん中 c）。
    def cue(t: ThemeDoc.Rec, c: Vec2.Vec2, q: Cue, z: Int32): List[Render.PlacedItem] =
        let floor = Color.mix(t#mapFloor, t#shade, 0.55);
        match q {
            case Cue.Line =>
                let arm = (dx: Int32, dy: Int32) -> List.range(1, 3) |> List.map(k -> ((dx * k, dy * k), t#markArrow));
                let cells = ((0, 0), t#mapHero) :: ((0, -3), t#mapEnemy) :: List.flatten(arm(0, -1) :: arm(1, 0) :: arm(0, 1) :: arm(-1, 0) :: Nil);
                cells |> List.map(tileAt(t, p(c#x, c#y + 4.5), 7.5, 1.5, z))
            case Cue.Surround(n) =>
                let ring = (-1, -1) :: (1, -1) :: (1, 1) :: (-1, 1) :: (0, -1) :: (1, 0) :: (0, 1) :: (-1, 0) :: Nil;
                let foes = List.take(n, ring) |> List.toSet;
                let colorAt = (d: (Int32, Int32)) -> if (d == (0, 0)) t#mapHero else if (Set.memberOf(d, foes)) t#mapEnemy else floor;
                List.range(-1, 2) |> List.flatMap(dy -> List.range(-1, 2) |> List.map(dx -> ((dx, dy), colorAt((dx, dy)))))
                    |> List.map(tileAt(t, c, 14.0, 2.0, z))
            case Cue.LowHp(ratio) =>
                let barAt = p(c#x - 26.0, c#y + 8.0);
                List.flatten(heart(t, p(c#x, c#y - 10.0), 10.0, z)
                    :: ({ at = Vec2.sub(barAt, p(2.0, 2.0)), item = RawDraw.box(p(56.0, 14.0), t#hpEdge, z) |> Render.rounded(7.0) }
                        :: { at = barAt, item = RawDraw.box(p(52.0, 10.0), t#hpBack, z + 1) |> Render.rounded(5.0) }
                        :: { at = barAt, item = Render.vgrad(p(Float64.max(8.0, 52.0 * ratio), 10.0), { top = t#hpTop, bottom = t#hpBottom }, z + 2) |> Render.rounded(5.0) } :: Nil)
                    :: Nil)
        }

    // ── 場面の上の印 ──

    /// 方向の予告のアイコンと受けるダメージ・眠りの z。
    pub def marks(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        List.append(w#badges |> List.flatMap(badge(atlas, w)), w#sleepers |> List.flatMap(sleeper(atlas, w)))

    def badge(atlas: FontAtlas, w: In, b: Badge): List[Render.PlacedItem] =
        let t = w#theme;
        let face = RawDraw.circleAt(b#at, 19.0, t#badgeFace, 2) :: Nil;
        let ring = RawDraw.circleAt(b#at, 22.0, t#badgeEdge, 1) :: faded(0.35, RawDraw.circleAt(b#at, 26.0, b#color, 0)) :: Nil;
        let hurt = if (b#hurt <= 0) Nil else hurtTag(atlas, t, b);
        List.flatten(ring :: face :: glyph(b#glyph, b#at, t#badgeEdge) :: hurt :: Nil)

    def glyph(g: Glyph, c: Vec2.Vec2, color: Color): List[Render.PlacedItem] = match g {
        case Glyph.Picture(i) => icon(Art.icon(i), c, 30.0, 3) :: Nil
        case Glyph.Steps      => steps(c, color)
        case Glyph.Stairs     => stairs(c, color)
        case Glyph.Hourglass  => hourglass(c, color)
    }

    /// 受けるダメージ: アイコンの下の赤いラベルに、小さなハートと数。
    def hurtTag(atlas: FontAtlas, t: ThemeDoc.Rec, b: Badge): List[Render.PlacedItem] =
        let s = "-${b#hurt}";
        let wide = TextDraw.width(s, atlas, 13.0) + 30.0;
        let at = p(b#at#x - wide / 2.0, b#at#y + 24.0);
        List.flatten(plate(at, p(wide, 19.0), 9.0, RawDraw.box(p(wide, 19.0), t#labelBack, 5) |> Render.fade(0.92), t#markDanger, 1.0, 4)
            :: heart(t, p(at#x + 11.0, at#y + 10.0), 5.5, 6)
            :: (centered(atlas, s, p(at#x + 19.0 + (wide - 22.0) / 2.0, at#y + 9.5), 13.0, t#markDanger, 6) :: Nil) :: Nil)

    /// 足あとの印（左右 2 つ）。
    def steps(c: Vec2.Vec2, color: Color): List[Render.PlacedItem] =
        let foot = (dx: Float64, dy: Float64) ->
            RawDraw.ellipseAt(p(c#x + dx, c#y + dy - 3.0), p(3.6, 5.2), color, 3) :: RawDraw.circleAt(p(c#x + dx, c#y + dy + 5.0), 2.6, color, 3) :: Nil;
        List.append(foot(-5.0, -2.0), foot(5.0, 2.0))

    /// 階段の印（左上から右下へ 3 段で降りる形と、下向きの矢じり）。
    def stairs(c: Vec2.Vec2, color: Color): List[Render.PlacedItem] =
        let v = (x: Float64, y: Float64) -> p(c#x + x, c#y + y);
        shape(v(-11.0, -9.0) :: v(-4.0, -9.0) :: v(-4.0, -3.0) :: v(3.0, -3.0) :: v(3.0, 3.0) :: v(10.0, 3.0) :: v(10.0, 10.0) :: v(-11.0, 10.0) :: Nil, color, 3)
            :: shape(v(3.0, -12.0) :: v(11.0, -12.0) :: v(7.0, -6.0) :: Nil, color, 3) :: Nil

    /// 砂時計の印。
    def hourglass(c: Vec2.Vec2, color: Color): List[Render.PlacedItem] =
        let v = (x: Float64, y: Float64) -> p(c#x + x, c#y + y);
        { at = v(-8.0, -11.0), item = RawDraw.box(p(16.0, 3.0), color, 3) |> Render.rounded(1.5) }
            :: { at = v(-8.0, 8.0), item = RawDraw.box(p(16.0, 3.0), color, 3) |> Render.rounded(1.5) }
            :: shape(v(-6.0, -8.0) :: v(6.0, -8.0) :: v(0.0, 0.0) :: Nil, color, 3)
            :: shape(v(0.0, 0.0) :: v(6.0, 8.0) :: v(-6.0, 8.0) :: Nil, color, 3) :: Nil

    /// 眠っている敵の頭の上の z（実時間でゆっくり昇って消える）。
    def sleeper(atlas: FontAtlas, w: In, at: Vec2.Vec2): List[Render.PlacedItem] =
        let phase = w#time * 0.6 - Float64.floor(w#time * 0.6);
        let rise = (k: Float64, size: Float64, dx: Float64) -> {
            let f = phase + k - Float64.floor(phase + k);
            faded(1.0 - f, centered(atlas, "z", p(at#x + dx + f * 6.0, at#y - f * 16.0), size, w#theme#sleep, 7))
        };
        rise(0.0, 15.0, 0.0) :: rise(0.5, 11.0, 7.0) :: Nil

    // ── 上の帯 ──

    pub def bars(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        if (not w#showBars) Nil
        else List.flatten(top(atlas, w) :: minimap(w) :: bottom(atlas, w) :: Nil)

    def top(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let t = w#theme;
        let ratio = Int32.toFloat64(Int32.max(0, w#hp)) / Int32.toFloat64(Int32.max(1, w#maxHp));
        let barAt = p(48.0, 70.0);
        let gear = w#gear |> List.zipWithIndex |> List.flatMap(match (i, g) -> gearSlot(t, p(18.0 + Int32.toFloat64(i) * 52.0, 94.0), g));
        List.flatten(
            (band(0.0, 150.0, t#shade, 0.85, 0.0, 0)
                :: text(atlas, w#floorLabel, p(18.0, 14.0), 34.0, t#ink, 3)
                :: { at = Vec2.sub(barAt, p(2.0, 2.0)), item = RawDraw.box(p(174.0, 16.0), t#hpEdge, 1) |> Render.rounded(8.0) }
                :: { at = barAt, item = RawDraw.box(p(170.0, 12.0), t#hpBack, 2) |> Render.rounded(6.0) }
                :: text(atlas, "${w#hp} / ${w#maxHp}", p(230.0, 64.0), 17.0, t#ink, 3) :: Nil)
            :: heart(t, p(29.0, 77.0), 10.0, 3)
            :: (if (ratio <= 0.0) Nil else { at = barAt, item = Render.vgrad(p(Float64.max(12.0, 170.0 * ratio), 12.0), { top = t#hpTop, bottom = t#hpBottom }, 3) |> Render.rounded(6.0) } :: Nil)
            :: gear :: Nil)

    /// 剣か盾の四角い枠（左上 at）。持っていない時は色を抜いた薄い絵・ふちは段の無い色。
    def gearSlot(t: ThemeDoc.Rec, at: Vec2.Vec2, g: Gear): List[Render.PlacedItem] =
        let size = p(42.0, 42.0);
        let body = plate(at, size, 8.0, Render.vgrad(size, { top = t#slotTop, bottom = t#slotBottom }, 2), g#tier, if (g#held) 2.0 else 1.5, 1);
        let pic = icon(Art.icon(g#icon), Vec2.add(at, p(21.0, 21.0)), 34.0, 3);
        let shown = if (g#held) pic else { item = pic#item |> Render.tinted(Color.mix(t#ink, t#shade, 0.45)) |> Render.fade(0.25) | pic };
        List.append(body, shown :: Nil)

    /// 小さい地図（見たマスだけ。右上）。
    def minimap(w: In): List[Render.PlacedItem] =
        let m = w#minimap;
        let pitch = Float64.min(6.0, 150.0 / Int32.toFloat64(Int32.max(1, m#rows)));
        let left = width() - 16.0 - pitch * Int32.toFloat64(m#cols);
        m#cells |> List.map(match ((x, y), color) ->
            { at = p(left + Int32.toFloat64(x) * pitch, 16.0 + Int32.toFloat64(y) * pitch), item = RawDraw.box(p(pitch - 1.0, pitch - 1.0), color, 2) })

    // ── 下の帯 ──

    def bottom(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let t = w#theme;
        let fade = band(700.0, 260.0, t#shade, 0.0, 0.9, 0);
        let slots = w#slots |> List.zipWithIndex |> List.flatMap(match (i, s) -> slot(atlas, w, p(24.0 + Int32.toFloat64(i) * 168.0, 818.0), s));
        List.flatten((fade :: Nil) :: logLine(atlas, w) :: slots :: Nil)

    /// 記録の 1 行（無ければ板も出さない）。
    def logLine(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let t = w#theme;
        let at = p(16.0, bandTop() + 8.0);
        if (String.isEmpty(w#log)) Nil
        else List.append(plate(at, p(width() - 32.0, 36.0), 10.0, RawDraw.box(p(width() - 32.0, 36.0), t#logBack, 2) |> Render.fade(0.85), t#logEdge, 1.0, 1),
                         text(atlas, w#log, p(at#x + 12.0, at#y + 8.0), 16.0, t#ink, 3) :: Nil)

    def slot(atlas: FontAtlas, w: In, at: Vec2.Vec2, s: Slot): List[Render.PlacedItem] =
        let t = w#theme;
        let size = p(156.0, 116.0);
        let body = plate(at, size, 14.0, Render.vgrad(size, { top = t#slotTop, bottom = t#slotBottom }, 2), s#tier, 2.0, 1);
        let panelAt = p(at#x + 84.0, at#y + 10.0);
        let panel = plate(panelAt, p(62.0, 58.0), 8.0, RawDraw.box(p(62.0, 58.0), t#labelBack, 2) |> Render.fade(0.9), Color.mix(s#tier, t#shade, 0.3), 1.0, 2);
        let alpha = if (s#held) 1.0 else 0.4;
        let inside = icon(Art.icon(s#icon), p(at#x + 46.0, at#y + 56.0), 72.0, 3)
            :: text(atlas, "×${s#count}", p(at#x + 92.0, at#y + 72.0), 28.0, t#ink, 3) :: Nil;
        let halo = if (s#glow <= 0.0) Nil
            else plate(Vec2.sub(at, p(4.0, 4.0)), Vec2.add(size, p(8.0, 8.0)), 18.0, RawDraw.box(Vec2.add(size, p(8.0, 8.0)), s#tier, 0) |> Render.fade(0.0), s#tier, 3.0 + 3.0 * s#glow, 0)
                |> Render.fadeAll(s#glow);
        List.flatten(halo :: body :: Render.fadeAll(if (s#held) 1.0 else 0.65, List.append(panel, cue(t, p(panelAt#x + 31.0, panelAt#y + 29.0), s#cue, 4))) :: Render.fadeAll(alpha, inside) :: Nil)

    // ── 前に重ねる板 ──

    /// 題名・倒れた・抜けた（場面を少し沈め、真ん中に板）。
    pub def card(atlas: FontAtlas, w: In): List[Render.PlacedItem] = match w#card {
        case None => Nil
        case Some(c) =>
            let t = w#theme;
            // WhyNot: 真ん中に置かない — 主人公は画面の真ん中に立つので、板が主人公とまわりの部屋を隠す。
            let at = p(50.0, 560.0);
            let size = p(width() - 100.0, 180.0);
            List.flatten(
                ({ at = Vec2.zero(), item = RawDraw.box(p(width(), height()), t#bg, 0) |> Render.fade(0.55) } :: Nil)
                :: plate(at, size, 18.0, Render.vgrad(size, { top = t#slotTop, bottom = t#slotBottom }, 2) |> Render.fade(0.95), t#tierWood, 2.0, 1)
                :: (centered(atlas, c#title, p(width() / 2.0, at#y + 72.0), 40.0, t#ink, 4)
                    :: faded(0.8, centered(atlas, c#prompt, p(width() / 2.0, at#y + 134.0), 17.0, t#ink, 4)) :: Nil) :: Nil)
    }

    /// 終了の確認の窓。
    pub def quit(atlas: FontAtlas, w: In): List[Render.PlacedItem] = match w#quitPrompt {
        case None => Nil
        case Some(s) =>
            let t = w#theme;
            let at = p(30.0, 440.0);
            let size = p(width() - 60.0, 80.0);
            List.flatten(
                ({ at = Vec2.zero(), item = RawDraw.box(p(width(), height()), t#bg, 0) |> Render.fade(0.6) } :: Nil)
                :: plate(at, size, 12.0, RawDraw.box(size, t#logBack, 2), t#logEdge, 1.0, 1)
                :: (centered(atlas, s, p(width() / 2.0, 480.0), 18.0, t#ink, 3) :: Nil) :: Nil)
    }
}
