// MarkView — 3D の場面の上に重ねる 2D の印（純粋）: 方向の予告のアイコン・眠りの z・浮かぶ数・斬り跡の弧・矢の軌跡・
// 回復の緑の輪と粒・頭の上に一瞬出る物の絵。どれも 3D の点を Camera3d.worldToScreen で画面へ写した所に置く（写すのは View）。
// 字は数だけ。与えたか受けたかは色と大きさ、何をしたかは形で分ける。
mod MarkView {
    use fge::{Color, FontAtlas, Num, RawDraw, Render, TextDraw, Vec2}
    use PartsView.{p, centered, icon, plate, faded, heart, shape, steps, stairs, hourglass, rimmed}

    /// 予告のアイコン: 絵・足あと（動く）・階段（降りる）・砂時計（待つ）。
    pub enum Glyph with Eq {
        case Picture(Art.Icon)
        case Steps
        case Stairs
        case Hourglass
    }

    /// 方向の予告 1 つ。at = アイコンの真ん中・hurt = 受けるダメージ（無ければ 0）・color = 枠の色・scale = 大きさの倍（斜めは小さい）。
    pub type alias Badge = { at = Vec2.Vec2, glyph = Glyph, hurt = Int32, color = Color, scale = Float64 }

    /// 浮かぶ数 1 つ。at = 出始めの真ん中（頭の上・積んだ段の分だけ上）・lift = 段（横へ流れる向きを左右交互に分ける）・age = 出てからの秒。
    pub type alias Number = { at = Vec2.Vec2, kind = Popup.Kind, amount = Int32, lift = Int32, age = Float64 }

    /// 頭の上に一瞬出る物の絵（薬・食べ物・盾）。at = 頭の上・age = 出てからの秒。
    pub type alias Overhead = { at = Vec2.Vec2, icon = Art.Icon, age = Float64 }

    /// 回復の緑の輪と粒。at = 足もと・age = 出てからの秒。
    pub type alias Ring = { at = Vec2.Vec2, age = Float64 }

    /// 斬り跡の弧。at = 当たった物の真ん中・toward = 打った側の向き（画面のラジアン）・color = 色・progress = 進み（0..1）。
    pub type alias Slash = { at = Vec2.Vec2, toward = Float64, color = Color, progress = Float64 }

    /// 矢の軌跡（tail から head へ細る線）。
    pub type alias Trail = { tail = Vec2.Vec2, head = Vec2.Vec2, color = Color }

    /// 浮かぶ数の動き（look の Doc から）。life = 消えるまでの秒・popScale と popSeconds = 出た瞬間の倍とそこから 1 倍へ戻る秒・
    /// rise と fall = ダメージの数が投げ上がる高さと一番上から落ちる高さ（px）・drift = 横へ流れる幅（px）・fadeSeconds = 終わりの消える秒・
    /// healRise と healPop = 回復の数がまっすぐ昇る高さ（px）と出た瞬間の倍。
    pub type alias Timing = { life = Float64, popScale = Float64, popSeconds = Float64, rise = Float64, fall = Float64, drift = Float64,
                              fadeSeconds = Float64, healRise = Float64, healPop = Float64 }

    /// 描くのに要る値。
    pub type alias In = {
        theme = ThemeDoc.Rec,
        time = Float64,
        timing = Timing,
        badges = List[Badge],
        sleepers = List[Vec2.Vec2],
        numbers = List[Number],
        overheads = List[Overhead],
        rings = List[Ring],
        slashes = List[Slash],
        trails = List[Trail]
    }

    /// 場面の上の印（奥から: 予告 → 眠りの z → 軌跡 → 弧 → 輪 → 頭の上 → 数）。
    pub def marks(atlas: FontAtlas, w: In): List[Render.PlacedItem] =
        List.flatten(
            (w#badges |> List.flatMap(badge(atlas, w#theme)))
            :: (w#sleepers |> List.flatMap(sleeper(atlas, w)))
            :: (w#trails |> List.flatMap(trail))
            :: (w#slashes |> List.flatMap(slash))
            :: (w#rings |> List.flatMap(ring(w)))
            :: (w#overheads |> List.flatMap(overhead(w)))
            :: (w#numbers |> List.flatMap(number(atlas, w))) :: Nil)

    // ── 方向の予告 ──

    def badge(atlas: FontAtlas, t: ThemeDoc.Rec, b: Badge): List[Render.PlacedItem] =
        let k = b#scale;
        let face = RawDraw.circleAt(b#at, 19.0 * k, t#badgeFace, 2) :: Nil;
        let rings = RawDraw.circleAt(b#at, 22.0 * k, t#badgeEdge, 1) :: faded(0.35, RawDraw.circleAt(b#at, 26.0 * k, b#color, 0)) :: Nil;
        let hurt = if (b#hurt <= 0) Nil else hurtTag(atlas, t, b);
        List.flatten(rings :: face :: glyph(b#glyph, b#at, k, t#badgeEdge) :: hurt :: Nil)

    def glyph(g: Glyph, c: Vec2.Vec2, k: Float64, color: Color): List[Render.PlacedItem] =
        let small = (items: List[Render.PlacedItem]) -> if (k >= 0.999) items else Render.scaledAllAround(k, c, items);
        match g {
            case Glyph.Picture(i) => icon(Art.icon(i), c, 30.0 * k, 3) :: Nil
            case Glyph.Steps      => small(steps(c, color, 3))
            case Glyph.Stairs     => small(stairs(c, color, 3))
            case Glyph.Hourglass  => small(hourglass(c, color, 3))
        }

    /// 受けるダメージ: アイコンの下の赤いラベルに、小さなハートと数。
    def hurtTag(atlas: FontAtlas, t: ThemeDoc.Rec, b: Badge): List[Render.PlacedItem] =
        let s = "-${b#hurt}";
        let size = 13.0 * Float64.max(0.85, b#scale);
        let wide = TextDraw.width(s, atlas, size) + 30.0;
        let at = p(b#at#x - wide / 2.0, b#at#y + 24.0 * b#scale);
        List.flatten(plate(at, p(wide, 19.0), 9.0, RawDraw.box(p(wide, 19.0), t#labelBack, 5) |> Render.fade(0.92), t#markDanger, 1.0, 4)
            :: heart(t, p(at#x + 11.0, at#y + 10.0), 5.5, 6)
            :: (centered(atlas, s, p(at#x + 19.0 + (wide - 22.0) / 2.0, at#y + 9.5), size, t#markDanger, 6) :: Nil) :: Nil)

    /// 眠っている敵の頭の上の z（実時間でゆっくり昇って消える）。
    def sleeper(atlas: FontAtlas, w: In, at: Vec2.Vec2): List[Render.PlacedItem] =
        let phase = w#time * 0.6 - Float64.floor(w#time * 0.6);
        let rise = (k: Float64, size: Float64, dx: Float64) -> {
            let f = phase + k - Float64.floor(phase + k);
            faded(1.0 - f, centered(atlas, "z", p(at#x + dx + f * 6.0, at#y - f * 16.0), size, w#theme#sleep, 7))
        };
        rise(0.0, 15.0, 0.0) :: rise(0.5, 11.0, 7.0) :: Nil

    // ── 浮かぶ数 ──

    def unit(x: Float64): Float64 = Float64.max(0.0, Float64.min(1.0, x))

    /// 種類ごとの色・大きさ・昇り方（slow = 弧を描かずまっすぐゆっくり昇る）・前に付ける字。
    /// WhyNot: 受けた数を赤く大きくする — 主人公が赤く光り画面の周りが赤く滲むので、受けたことは数の色に頼らなくても分かる。
    def styleOf(w: In, kind: Popup.Kind): { color = Color, size = Float64, slow = Bool, sign = String } =
        let t = w#theme;
        match kind {
            case Popup.Kind.Dealt   => { color = t#popDealt, size = 34.0, slow = false, sign = "" }
            case Popup.Kind.Taken   => { color = t#popDealt, size = 34.0, slow = false, sign = "" }
            case Popup.Kind.Healed  => { color = t#popHealed, size = 32.0, slow = true, sign = "+" }
            case Popup.Kind.Fed     => { color = t#popFed, size = 28.0, slow = true, sign = "+" }
            case Popup.Kind.Blocked => { color = t#popBlocked, size = 30.0, slow = false, sign = "" }
            case Popup.Kind.Starved => { color = t#popDealt, size = 28.0, slow = false, sign = "" }
            case Popup.Kind.Burned  => { color = t#popDealt, size = 28.0, slow = false, sign = "" }
            case Popup.Kind.Exp     => { color = t#popExp, size = 24.0, slow = true, sign = "+" }
        }

    /// 投げ上げた物の高さ（px・上が正）: t 秒で一番上 rise に着き、life 秒で fall だけ落ちた所に来る放物線。
    def arcHeight(rise: Float64, fall: Float64, life: Float64, t: Float64): Float64 =
        if (rise <= 0.0) 0.0
        else {
            let top = life / (1.0 + Num.sqrt(Float64.max(0.0, fall) / rise));
            let d = (t - top) / Float64.max(0.001, top);
            rise * (1.0 - d * d)
        }

    /// 数 1 つ: 出た瞬間に大きく出て 1 倍へ弾み、ダメージは横へ流れながら放物線で上がって少し落ち、回復はまっすぐゆっくり昇る。
    /// 終わりの fadeSeconds 秒で小さくなりながら消える。
    def number(atlas: FontAtlas, w: In, n: Number): List[Render.PlacedItem] =
        let style = styleOf(w, n#kind);
        let tm = w#timing;
        let life = Float64.max(0.01, tm#life);
        let t = Float64.min(n#age, life);
        let popped = unit(t / Float64.max(0.001, tm#popSeconds));
        let fading = unit((t - (life - tm#fadeSeconds)) / Float64.max(0.001, tm#fadeSeconds));
        let start = if (style#slow) tm#healPop else tm#popScale;
        let scale = (start + (1.0 - start) * popped) * (1.0 - 0.45 * fading);
        let side = if (Int32.modulo(n#lift, 2) == 0) 1.0 else -1.0;
        let rise = if (style#slow) tm#healRise * (1.0 - (1.0 - t / life) * (1.0 - t / life)) else arcHeight(tm#rise, tm#fall, life, t);
        let drift = if (style#slow) 0.0 else side * tm#drift * (t / life);
        let at = p(n#at#x + drift, n#at#y - rise);
        Render.fadeAll(1.0 - fading, heavy(atlas, "${style#sign}${n#amount}", at, style#size * scale, style#color, w#theme#popRim))

    /// 太めに見せる字: 濃い影を右下へ細くずらして敷き、字は横に少しずらして重ねる。
    /// WhyNot: 角ばった字形のフォントへ替える — 描き出しの Pass つきの PNG が追加のフォントを受けず、絵の確かめと実機で字が変わる。
    def heavy(atlas: FontAtlas, s: String, center: Vec2.Vec2, size: Float64, fill: Color, shadow: Color): List[Render.PlacedItem] =
        let d = Float64.max(1.0, size * 0.05);
        let thick = Float64.max(0.6, size * 0.025);
        let at = (dx: Float64, dy: Float64) -> p(center#x + dx, center#y + dy);
        centered(atlas, s, at(d, d), size, shadow, 10) :: centered(atlas, s, at(d + thick, d), size, shadow, 10)
            :: centered(atlas, s, at(0.0, 0.0), size, fill, 11) :: centered(atlas, s, at(thick, 0.0), size, fill, 11) :: Nil

    /// 頭の上の物の絵: 弾んで出て、上へ少し昇って消える。
    def overhead(w: In, o: Overhead): List[Render.PlacedItem] =
        let q = unit(o#age / Float64.max(0.01, w#timing#life));
        let size = 40.0 * (if (q < 0.2) 0.6 + 2.0 * q else 1.0);
        let alpha = if (q < 0.6) 1.0 else 1.0 - (q - 0.6) / 0.4;
        Render.fadeAll(alpha, icon(Art.icon(o#icon), p(o#at#x, o#at#y - 14.0 * q), size, 9) :: Nil)

    /// 回復の輪（足もとから広がる平たい輪）と、立ち昇る光の粒。
    def ring(w: In, r: Ring): List[Render.PlacedItem] =
        let q = unit(r#age / Float64.max(0.01, w#timing#life));
        let c = w#theme#popHealed;
        let radius = 16.0 + 30.0 * (1.0 - (1.0 - q) * (1.0 - q));
        let fade = 1.0 - q;
        let band = RawDraw.ellipseAt(r#at, p(radius, radius * 0.38), c, 2) |> faded(0.7 * fade);
        let hole = RawDraw.ellipseAt(r#at, p(radius * 0.8, radius * 0.3), c, 1) |> faded(0.15 * fade);
        let mote = (i: Int32) -> {
            let a = Int32.toFloat64(i) * 1.3;
            let x = r#at#x + 20.0 * Num.cos(a) * (0.6 + 0.4 * q);
            let y = r#at#y - 70.0 * q * (0.7 + 0.1 * Int32.toFloat64(Int32.modulo(i, 3))) + 6.0 * Num.sin(a);
            RawDraw.circleAt(p(x, y), 3.2 - 1.5 * q, c, 3) |> faded(fade)
        };
        band :: hole :: (List.range(0, 7) |> List.map(mote))

    // ── 斬り跡と軌跡 ──

    /// 弧の帯（真ん中 c・半径 r・角度 a0..a1・太さは真ん中で thick、両端で 0）。
    def arcBand(c: Vec2.Vec2, r: Float64, a0: Float64, a1: Float64, thick: Float64, color: Color, z: Int32): Render.PlacedItem =
        let n = 12;
        let at = (i: Int32) -> a0 + (a1 - a0) * Int32.toFloat64(i) / Int32.toFloat64(n);
        let width = (i: Int32) -> thick * Num.sin(Num.pi() * Int32.toFloat64(i) / Int32.toFloat64(n));
        let point = (a: Float64, rr: Float64) -> p(c#x + rr * Num.cos(a), c#y + rr * Num.sin(a));
        let outer = List.range(0, n + 1) |> List.map(i -> point(at(i), r + width(i) / 2.0));
        let inner = List.range(0, n + 1) |> List.reverse |> List.map(i -> point(at(i), r - width(i) / 2.0));
        shape(outer ::: inner, color, z)

    /// 斬り跡: 打った側から当たった物を横切る弧が、走りながら細って消える。
    def slash(s: Slash): List[Render.PlacedItem] =
        let q = unit(s#progress);
        let side = s#toward + Num.pi() / 2.0;
        let sweep = 2.1;
        let head = side - sweep / 2.0 + sweep * Float64.min(1.0, q * 1.8);
        let tail = side - sweep / 2.0 + sweep * Float64.max(0.0, q * 1.8 - 0.8);
        let c = p(s#at#x - 14.0 * Num.cos(s#toward), s#at#y - 14.0 * Num.sin(s#toward));
        let fade = 1.0 - q * q;
        if (head - tail < 0.05) Nil
        else arcBand(c, 30.0, tail, head, 12.0 * fade, Color.mix(s#color, Color.rgb(1.0, 1.0, 1.0), 0.5), 5) |> faded(0.35 * fade)
            :: arcBand(c, 30.0, tail, head, 6.0 * fade, s#color, 6) |> faded(fade) :: Nil

    /// 矢の軌跡: 尾から頭へ太くなる 2 本の線。
    def trail(t: Trail): List[Render.PlacedItem] =
        let mid = p((t#tail#x + t#head#x) / 2.0, (t#tail#y + t#head#y) / 2.0);
        RawDraw.lineSeg(t#tail, mid, 1.5, t#color, 4) |> faded(0.35)
            :: RawDraw.lineSeg(mid, t#head, 3.0, t#color, 4) |> faded(0.75) :: Nil
}
