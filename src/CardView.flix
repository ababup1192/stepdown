// CardView — 選ぶ場面のカード（純粋）: 捨てるカード（持っている 3 つ＋拾う物）とレベルアップのカード（候補）、
// レベルアップの演出（「LEVEL UP!」の絵が跳ねて揺れて出る・主人公から光の柱と星）と、選んだカードが主人公へ吸い込まれる・捨てた物が落ちる動き。
// 時計は実時間（Cards の間は遊びの時計が止まる）。カードは演出の途中から滑り込み、演出を待たずに選べる（押し所は最後の位置 rectOf）。
mod CardView {
    use fge::{Color, Curve, FontAtlas, Num, RawDraw, Render, Vec2}
    use fge::DrawCmd.BlendMode
    use PartsView.{p, centered, icon, plate, heart, cue, rimmed}

    /// カードの顔: アイコンかハート。
    pub enum Face with Eq {
        case Picture(Art.Icon)
        case Heart
    }

    /// カード 1 枚。label = 短い数（「+5」「×3」・無ければ空）・cue = 効きの小さな絵・edge = ふちの色・fresh = 拾う物（ふちが光る）・drop = 選ぶと捨てる。
    pub type alias Card = { face = Face, label = String, cue = Option[PartsView.Cue], edge = Color, fresh = Bool, drop = Bool }

    /// 選んだカード。since = 選んでからの秒・absorb = 主人公へ吸い込まれる（偽なら捨てて落ちる）。
    pub type alias Chosen = { cards = List[Card], index = Int32, since = Float64, absorb = Bool }

    /// 見せる長さ（look の Doc から）。
    pub type alias Timing = { levelUp = Float64, slide = Float64, chosen = Float64 }

    /// 描くのに要る値。cards = 今のカード（選ぶ場面でなければ空）・since = 出し始めてからの秒・levelUp = レベルアップの演出を出すか・
    /// hero = 主人公の体の真ん中（画面）・chosen = 吸い込まれる途中のカード。
    pub type alias In = {
        theme = ThemeDoc.Rec,
        time = Float64,
        timing = Timing,
        cards = List[Card],
        since = Float64,
        levelUp = Bool,
        hero = Vec2.Vec2,
        chosen = Option[Chosen]
    }

    def width(): Float64 = 540.0
    def height(): Float64 = 960.0

    /// count 枚のうち index 枚目のカードの最後の位置（左上と大きさ）。タップの押し所もこれ。
    pub def rectOf(count: Int32, index: Int32): { at = Vec2.Vec2, size = Vec2.Vec2 } =
        let size = if (count >= 4) p(112.0, 168.0) else p(138.0, 180.0);
        let gap = if (count >= 4) 12.0 else 18.0;
        let total = Int32.toFloat64(count) * size#x + Int32.toFloat64(Int32.max(0, count - 1)) * gap;
        { at = p((width() - total) / 2.0 + Int32.toFloat64(index) * (size#x + gap), 566.0), size = size }

    /// 点 point の上のカードの番号（無ければ None）。
    pub def cardAt(count: Int32, point: Vec2.Vec2): Option[Int32] =
        List.range(0, count) |> List.find(i -> {
            let r = rectOf(count, i);
            point#x >= r#at#x and point#x <= r#at#x + r#size#x and point#y >= r#at#y and point#y <= r#at#y + r#size#y
        })

    def unit(x: Float64): Float64 = Float64.max(0.0, Float64.min(1.0, x))

    /// 選ぶ場面の全部（奥から: 沈める幕 → 光の柱と星 → 文字の絵 → カード → 吸い込まれるカード）。
    pub def cards(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let showing = not List.isEmpty(w#cards);
        List.flatten(
            (if (showing) veil(w) else Nil)
            :: (if (showing and w#levelUp) burst(w) else Nil)
            :: (if (showing and w#levelUp) banner(w) else Nil)
            :: (if (showing) row(atlas, w) else Nil)
            :: (w#chosen |> Option.map(chosen(atlas, w)) |> Option.getWithDefault(Nil)) :: Nil)

    /// 場面を少し沈める幕（出し始めに薄く入る）。
    def veil(w: In): List[Render.PlacedItem] =
        { at = Vec2.zero(), item = RawDraw.box(p(width(), height()), w#theme#bg, 0) |> Render.fade(0.5 * unit(w#since / 0.15)) } :: Nil

    /// 光の柱（主人公の足もとから上へ消える帯）と、主人公から飛び散る星。演出の秒で消える。
    def burst(w: In): List[Render.PlacedItem] =
        let q = unit(w#since / Float64.max(0.01, w#timing#levelUp));
        if (q >= 1.0) Nil
        else {
            let t = w#theme;
            let fade = 1.0 - q;
            let feet = p(w#hero#x, w#hero#y + 40.0);
            let half = 30.0 * (0.5 + 0.9 * unit(w#since / 0.12)) * (1.0 - 0.4 * q);
            let v = (x: Float64, y: Float64, a: Float64) -> { pos = p(x, y), color = t#levelGlow, alpha = a };
            let column = { at = Vec2.zero(), item = Render.gradPolygon(
                v(feet#x - half, feet#y, 0.85 * fade) :: v(feet#x + half, feet#y, 0.85 * fade)
                    :: v(feet#x + half * 0.6, 0.0, 0.0) :: v(feet#x - half * 0.6, 0.0, 0.0) :: Nil, 2) |> Render.blended(BlendMode.Add) };
            let glow = Render.glowAt(w#hero, 110.0, t#levelGlow, { alpha = 0.8 * fade, blend = BlendMode.Add }, 1);
            let spark = (i: Int32) -> {
                let a = 0.0 - Num.pi() / 2.0 + (Int32.toFloat64(i) - 4.0) * 0.36;
                let speed = 240.0 + 50.0 * Int32.toFloat64(Int32.modulo(i, 3));
                let d = speed * w#since * (1.0 - 0.45 * q);
                let c = p(w#hero#x + d * Num.cos(a), w#hero#y + d * Num.sin(a) + 120.0 * q * q);
                Render.turnedAll(w#since * 6.0 + Int32.toFloat64(i), c, PartsView.star(c, 9.0 + 3.0 * Int32.toFloat64(Int32.modulo(i, 2)), { face = t#levelGlow, edge = t#expBottom }, 3))
                    |> Render.fadeAll(fade)
            };
            List.flatten(glow :: (column :: Nil) :: (List.range(0, 9) |> List.flatMap(spark)) :: Nil)
        }

    /// 「LEVEL UP!」の絵: 大きさが行き過ぎて戻るように跳ねて出て、揺れが静まる。出ている間は小さく揺れ続ける。
    def banner(w: In): List[Render.PlacedItem] =
        let pic = Art.fx(Art.Fx.LevelUp);
        let s = w#since;
        let grow = 1.0 - Curve.dampedSpring(1.0, 7.0, 15.0, s);
        let tilt = Curve.dampedSpring(0.22, 6.0, 19.0, s) + Curve.sine(0.025, 2.2, 0.0, w#time);
        let lift = Curve.dampedSpring(40.0, 8.0, 12.0, s);
        let center = p(width() / 2.0, 250.0 - lift);
        let side = 420.0 * Float64.max(0.0, grow);
        if (side <= 1.0) Nil
        else Render.turnedAll(tilt, center, icon(pic, center, side, 4) :: Nil)

    /// カードの列: 少しずつ遅れて下から滑り込む。
    def row(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let count = List.length(w#cards);
        let lead = if (w#levelUp) 0.12 else 0.0;
        w#cards |> List.zipWithIndex |> List.flatMap(match (i, c) -> {
            let q = unit((w#since - lead - 0.06 * Int32.toFloat64(i)) / Float64.max(0.01, w#timing#slide));
            let rise = 1.0 - Curve.dampedSpring(1.0, 9.0, 11.0, q * w#timing#slide * 3.0);
            let r = rectOf(count, i);
            let at = p(r#at#x, r#at#y + (1.0 - rise) * 440.0);
            if (q <= 0.0) Nil else card(atlas, w, at, r#size, c)
        })

    /// カード 1 枚（左上 at）: 顔の絵・短い数・効きの絵。捨てるカードの右上に赤い ×。拾う物はふちが脈打って光る。
    def card(atlas: FontAtlas, w: In, at: Vec2.Vec2, size: Vec2.Vec2, c: Card): List[Render.PlacedItem] =
        let t = w#theme;
        let cx = at#x + size#x / 2.0;
        let pulse = 0.5 + 0.5 * Num.sin(w#time * 5.0);
        let halo = if (c#fresh) plate(Vec2.sub(at, p(5.0, 5.0)), Vec2.add(size, p(10.0, 10.0)), 20.0, RawDraw.box(Vec2.add(size, p(10.0, 10.0)), c#edge, 0) |> Render.fade(0.0), c#edge, 3.0 + 2.0 * pulse, 0) |> Render.fadeAll(0.5 + 0.4 * pulse) else Nil;
        let shadow = { at = p(at#x + 4.0, at#y + 8.0), item = RawDraw.box(size, t#shade, 0) |> Render.rounded(18.0) |> Render.fade(0.45) };
        let body = plate(at, size, 16.0, Render.vgrad(size, { top = Color.mix(t#slotTop, t#ink, 0.08), bottom = t#slotBottom }, 1), c#edge, 2.5, 1);
        let shine = { at = p(at#x + 6.0, at#y + 6.0), item = Render.vgrad(p(size#x - 12.0, 40.0), { top = t#ink, bottom = t#slotTop }, 2) |> Render.rounded(12.0) |> Render.fade(0.10) };
        let faceAt = p(cx, at#y + 52.0);
        let ground = if (c#fresh) RawDraw.ellipseAt(p(cx, at#y + 84.0), p(32.0, 7.0), t#shade, 2) |> PartsView.faded(0.45) :: Nil else Nil;
        let face = match c#face {
            case Face.Picture(i) => icon(Art.icon(i), faceAt, 72.0, 3) :: Nil
            case Face.Heart      => heart(t, faceAt, 28.0, 3)
        };
        let label = if (String.isEmpty(c#label)) Nil else rimmed(atlas, c#label, p(cx, at#y + 104.0), 30.0, { fill = t#ink, rim = t#popRim }, 5);
        let cueAt = p(cx, if (String.isEmpty(c#label)) at#y + 118.0 else at#y + size#y - 28.0);
        let effect = c#cue |> Option.map(q -> Render.scaledAllAround(0.85, cueAt, cue(t, cueAt, q, 4))) |> Option.getWithDefault(Nil);
        let cross = if (c#drop) dropMark(t, p(at#x + size#x - 14.0, at#y + 14.0)) else Nil;
        List.flatten(halo :: (shadow :: Nil) :: body :: (shine :: Nil) :: ground :: face :: label :: effect :: cross :: Nil)

    /// 捨てる印（赤い丸に白い ×）。
    def dropMark(t: ThemeDoc.Rec, c: Vec2.Vec2): List[Render.PlacedItem] =
        let arm = (a: Float64) -> RawDraw.lineSeg(p(c#x - 6.0 * Num.cos(a), c#y - 6.0 * Num.sin(a)), p(c#x + 6.0 * Num.cos(a), c#y + 6.0 * Num.sin(a)), 3.0, t#ink, 7);
        RawDraw.circleAt(c, 13.0, t#popRim, 5) :: RawDraw.circleAt(c, 11.0, t#markDanger, 6) :: arm(Num.pi() / 4.0) :: arm(0.0 - Num.pi() / 4.0) :: Nil

    /// 選んだカード: 光りながら縮んで主人公へ吸い込まれる。捨てた物は回りながら下へ落ちて消える。
    def chosen(atlas: FontAtlas, w: In, c: Chosen): List[Render.PlacedItem] =
        let q = unit(c#since / Float64.max(0.01, w#timing#chosen));
        let count = List.length(c#cards);
        let picked = List.drop(c#index, c#cards) |> List.head;
        match picked {
            case None => Nil
            case Some(card1) =>
                if (q >= 1.0) Nil
                else {
                    let r = rectOf(count, c#index);
                    let home = p(r#at#x + r#size#x / 2.0, r#at#y + r#size#y / 2.0);
                    let center = if (c#absorb) p(home#x + (w#hero#x - home#x) * q * q, home#y + (w#hero#y - home#y) * q * q) else p(home#x, home#y + 260.0 * q * q);
                    let drawn = card(atlas, w, Vec2.sub(center, Vec2.mul(r#size, 0.5)), r#size, card1);
                    let flash = { at = Vec2.sub(center, Vec2.mul(r#size, 0.5)), item = RawDraw.box(r#size, Color.rgb(1.0, 1.0, 1.0), 8) |> Render.rounded(16.0) |> Render.fade(0.7 * (1.0 - q)) |> Render.blended(BlendMode.Add) };
                    if (c#absorb) Render.scaledAllAround(1.0 - 0.85 * q, center, flash :: drawn)
                    else Render.fadeAll(1.0 - q, Render.turnedAll(0.6 * q, center, drawn))
                }
        }
}
