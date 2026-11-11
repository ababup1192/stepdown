// HudView — 3D の場面の上に重ねる上の帯と下の帯（純粋）: 上は階・経験値とレベル・HP・満腹度・剣と盾・小さい地図、
// 下は記録 1 行と持ち物 3 枠。ほかに題名と終わりの板・終了の確認の窓・受けた時に画面の周りを赤く滲ませる幕。
// 字は階・数・記録・板にだけ使い、ほかは小さな絵で見せる。場面の上の印は MarkView、カードは CardView。
// 寸法と色は 540×960 の design の px（手本は UI の下書き）。
mod HudView {
    use fge::{Color, FontAtlas, Num, RawDraw, Render, Vec2}
    use PartsView.{p, text, centered, icon, plate, band, sideBand, faded, heart, star, bar, fill, cue}

    /// 持ち物の枠 1 つ。icon = 絵・count = 数・cue = 発動の条件・tier = ふちの色・glow = 上位を拾った光（0..1）。
    pub type alias Slot = { icon = Art.Icon, count = Int32, cue = PartsView.Cue, tier = Color, glow = Float64 }

    /// 剣か盾の枠 1 つ。icon = 絵・tier = ふちの色・held = 持っているか。
    pub type alias Gear = { icon = Art.Icon, tier = Color, held = Bool }

    /// 小さい地図。cells = 見たマスと色・cols と rows = 盤の大きさ。
    pub type alias Minimap = { cols = Int32, rows = Int32, cells = List[((Int32, Int32), Color)] }

    /// 前に重ねる板（題名・倒れた・抜けた）。
    pub type alias Card = { title = String, prompt = String }

    /// HP の棒の今の形（Popup.barOf の値）と最大。
    pub type alias Hp = { now = Int32, max = Int32, front = Float64, extra = Float64, healing = Bool }

    /// 満腹度の棒: 今と最大・食べた光（0..1）・空腹で減った赤い点滅（0..1）。
    pub type alias Fullness = { now = Int32, max = Int32, glow = Float64, alarm = Float64 }

    /// 経験値の棒: レベル・今のレベルで貯めた数・次までに要る数。
    pub type alias Exp = { level = Int32, now = Int32, need = Int32 }

    /// 描くのに要る値。log = 記録の 1 行（無ければ空）・slots = 3 枠（None は空き枠）・edge = 画面の周りの赤（0..1）。
    pub type alias In = {
        theme = ThemeDoc.Rec,
        time = Float64,
        floorLabel = String,
        hp = Hp,
        fullness = Fullness,
        exp = Exp,
        gear = List[Gear],
        minimap = Minimap,
        log = String,
        slots = List[Option[Slot]],
        card = Option[Card],
        quitPrompt = Option[String],
        showBars = Bool,
        edge = Float64
    }

    def width(): Float64 = 540.0
    def height(): Float64 = 960.0

    /// 下の UI の帯（記録と持ち物）の上の端。ここより下のタップは盤の入力にしない。
    pub def bandTop(): Float64 = 760.0

    // ── 上の帯 ──

    pub def bars(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        if (not w#showBars) Nil
        else List.flatten(top(atlas, w) :: minimap(w) :: bottom(atlas, w) :: Nil)

    def top(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let t = w#theme;
        let gear = w#gear |> List.zipWithIndex |> List.flatMap(match (i, g) -> gearSlot(t, p(18.0 + Int32.toFloat64(i) * 52.0, 124.0), g));
        List.flatten(
            (band(Vec2.zero(), p(width(), 180.0), t#shade, { top = 0.85, bottom = 0.0 }, 0)
                :: text(atlas, w#floorLabel, p(18.0, 14.0), 34.0, t#ink, 3) :: Nil)
            :: expBar(atlas, t, w#exp) :: hpBar(atlas, t, w#hp) :: fullnessBar(t, w#time, w#fullness) :: gear :: Nil)

    /// 経験値: 星の中にレベルの数、右に細い棒。
    def expBar(atlas: FontAtlas, t: ThemeDoc.Rec, e: Exp): List[Render.PlacedItem] =
        let at = p(122.0, 29.0);
        let length = 150.0;
        let ratio = Int32.toFloat64(Int32.max(0, e#now)) / Int32.toFloat64(Int32.max(1, e#need));
        List.flatten(star(p(100.0, 34.0), 16.0, { face = t#expBottom, edge = t#hpEdge }, 2)
            :: (centered(atlas, "${e#level}", p(100.0, 35.0), 15.0, t#labelBack, 4) :: Nil)
            :: bar(at, p(length, 8.0), { edge = t#hpEdge, back = t#hpBack }, 1)
            :: fill(at, length * Float64.min(1.0, ratio), 8.0, { top = t#expTop, bottom = t#expBottom }, 3) :: Nil)

    /// HP: ハートと棒。減った分は赤く残してから縮み、増えた分は緑で伸びる。
    def hpBar(atlas: FontAtlas, t: ThemeDoc.Rec, hp: Hp): List[Render.PlacedItem] =
        let at = p(48.0, 66.0);
        let length = 170.0;
        let along = (v: Float64) -> length * Float64.max(0.0, Float64.min(1.0, v / Int32.toFloat64(Int32.max(1, hp#max))));
        let extra = if (hp#healing) { top = Color.mix(t#hpGain, t#ink, 0.3), bottom = t#hpGain } else { top = Color.mix(t#hpLost, t#ink, 0.25), bottom = t#hpLost };
        List.flatten(heart(t, p(29.0, 73.0), 10.0, 3)
            :: bar(at, p(length, 12.0), { edge = t#hpEdge, back = t#hpBack }, 1)
            :: fill(at, along(Float64.max(hp#front, hp#extra)), 12.0, extra, 3)
            :: fill(at, along(Float64.min(hp#front, hp#extra)), 12.0, { top = t#hpTop, bottom = t#hpBottom }, 4)
            :: (text(atlas, "${hp#now} / ${hp#max}", p(230.0, 60.0), 17.0, t#ink, 5) :: Nil) :: Nil)

    /// 満腹度: パンの絵と棒。食べた時は光り、空腹で減った時と 0 の間は赤く点滅する。
    def fullnessBar(t: ThemeDoc.Rec, time: Float64, f: Fullness): List[Render.PlacedItem] =
        let at = p(48.0, 95.0);
        let length = 170.0;
        let ratio = Int32.toFloat64(Int32.max(0, f#now)) / Int32.toFloat64(Int32.max(1, f#max));
        let blink = Float64.max(f#alarm, if (f#now <= 0) 0.5 + 0.5 * Num.sin(time * 9.0) else 0.0);
        let edge = Color.mix(Color.mix(t#hpEdge, t#popFed, f#glow), t#popTaken, blink);
        let halo = if (f#glow <= 0.0) Nil
            else { at = Vec2.sub(at, p(5.0, 5.0)), item = RawDraw.box(p(length + 10.0, 18.0), t#popFed, 0) |> Render.rounded(9.0) |> Render.fade(0.45 * f#glow) } :: Nil;
        List.flatten(halo
            :: (icon(Art.icon(Art.Icon.Bread), p(29.0, 99.0), 26.0, 3) :: Nil)
            :: bar(at, p(length, 10.0), { edge = edge, back = t#hpBack }, 1)
            :: fill(at, length * Float64.min(1.0, ratio), 10.0, { top = Color.mix(t#fullTop, t#ink, 0.4 * f#glow), bottom = t#fullBottom }, 3) :: Nil)

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
        let fade = band(p(0.0, 700.0), p(width(), 260.0), t#shade, { top = 0.0, bottom = 0.9 }, 0);
        let slots = w#slots |> List.zipWithIndex |> List.flatMap(match (i, s) -> slot(atlas, t, p(24.0 + Int32.toFloat64(i) * 168.0, 818.0), s));
        List.flatten((fade :: Nil) :: logLine(atlas, w) :: slots :: Nil)

    /// 記録の 1 行（無ければ板も出さない）。
    def logLine(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        let t = w#theme;
        let at = p(16.0, bandTop() + 8.0);
        if (String.isEmpty(w#log)) Nil
        else List.append(plate(at, p(width() - 32.0, 36.0), 10.0, RawDraw.box(p(width() - 32.0, 36.0), t#logBack, 2) |> Render.fade(0.85), t#logEdge, 1.0, 1),
                         text(atlas, w#log, p(at#x + 12.0, at#y + 8.0), 16.0, t#ink, 3) :: Nil)

    /// 持ち物の枠の大きさ。
    def slotSize(): Vec2.Vec2 = p(156.0, 116.0)

    /// 持ち物の枠 1 つ（空き枠は薄いふちだけ）。
    def slot(atlas: FontAtlas, t: ThemeDoc.Rec, at: Vec2.Vec2, held: Option[Slot]): List[Render.PlacedItem] = match held {
        case None    => Render.fadeAll(0.45, plate(at, slotSize(), 14.0, RawDraw.box(slotSize(), t#slotBottom, 2) |> Render.fade(0.35), t#tierNone, 1.0, 1))
        case Some(s) => heldSlot(atlas, t, at, s)
    }

    def heldSlot(atlas: FontAtlas, t: ThemeDoc.Rec, at: Vec2.Vec2, s: Slot): List[Render.PlacedItem] =
        let size = slotSize();
        let body = plate(at, size, 14.0, Render.vgrad(size, { top = t#slotTop, bottom = t#slotBottom }, 2), s#tier, 2.0, 1);
        let panelAt = p(at#x + 84.0, at#y + 10.0);
        let panel = plate(panelAt, p(62.0, 58.0), 8.0, RawDraw.box(p(62.0, 58.0), t#labelBack, 2) |> Render.fade(0.9), Color.mix(s#tier, t#shade, 0.3), 1.0, 2);
        let inside = icon(Art.icon(s#icon), p(at#x + 46.0, at#y + 56.0), 72.0, 3)
            :: text(atlas, "×${s#count}", p(at#x + 92.0, at#y + 72.0), 28.0, t#ink, 3) :: Nil;
        let halo = if (s#glow <= 0.0) Nil
            else plate(Vec2.sub(at, p(4.0, 4.0)), Vec2.add(size, p(8.0, 8.0)), 18.0, RawDraw.box(Vec2.add(size, p(8.0, 8.0)), s#tier, 0) |> Render.fade(0.0), s#tier, 3.0 + 3.0 * s#glow, 0)
                |> Render.fadeAll(s#glow);
        List.flatten(halo :: body :: panel :: cue(t, p(panelAt#x + 31.0, panelAt#y + 29.0), s#cue, 4) :: inside :: Nil)

    // ── 前に重ねる板 ──

    /// 受けた時に画面の周りが一瞬赤く滲む（4 辺の内へ向けて薄れる帯）。
    pub def edge(w: In): List[Render.PlacedItem] =
        if (w#edge <= 0.0) Nil
        else {
            let c = w#theme#popTaken;
            let a = 0.55 * w#edge;
            let d = 90.0;
            band(Vec2.zero(), p(width(), d), c, { top = a, bottom = 0.0 }, 0)
                :: band(p(0.0, height() - d), p(width(), d), c, { top = 0.0, bottom = a }, 0)
                :: sideBand(Vec2.zero(), p(d, height()), c, { left = a, right = 0.0 }, 0)
                :: sideBand(p(width() - d, 0.0), p(d, height()), c, { left = 0.0, right = a }, 0) :: Nil
        }

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
