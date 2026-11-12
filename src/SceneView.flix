// SceneView — 斜め上から見下ろすダンジョンの 3D の場面（純粋）。
//
// ── 組み方 ──
// 動かない物は地図ごとに材質ごとにつないだ形（Res.floorAt）をそのまま置き、水面は 1 枚の半透明の形。
// 光は暗い青の環境光・影を落とす月（平行光）・松明の点光源（全部入れて選ぶのは engine）・主人公に付いて動く手もとの灯り・水の上の青い光。
// 炎は光を受けない色だけの形で、実時間で大きさと明るさを少し揺らす。
// キャラとアイテムはカメラに正対する板（ビルボード）で、足もとに丸い影。床の印（予告の枠・敵の予告）は床から少し浮かせた半透明の形。
mod SceneView {
    use fge::{Camera3d, Color, Frustum, Mat4, Mesh3d, Num, Render, Scene3d, ShaderDoc, Texture3d, Vec2, Vec3}
    use fge::ShaderDoc.{Field, Out, Shade, TexChan}

    /// 板 1 枚: 絵・立てる点（足もと）・高さ（m）・色を掛けるか・光を受けるか・丸い影の半径（0 で無し）。
    pub type alias Figure = { picture = Art.Picture, at = Vec3.Vec3, height = Float64, tint = Color, lit = Bool, shadow = Float64 }

    /// 床の印の線の形。
    pub enum Stroke with Eq, ToString {
        case Frame     // マスのふち
        case Dashed    // マスのふちの破線
        case Fill      // マスを薄く塗る
        case Guard(Float64, Float64) // 床の向き (x, z) を真ん中に、前と左右斜め前へ広がる弧（盾持ちが防ぐ側）
    }

    /// 床の印 1 つ（マスの真ん中・色・形）。
    pub type alias Mark = { at = Vec3.Vec3, color = Color, stroke = Stroke }

    /// 床の破線（origin から tip へ）。
    pub type alias Lane = { origin = Vec3.Vec3, tip = Vec3.Vec3, color = Color }

    /// カメラに正対して広がる円 1 つ（爆弾の光と煙）。真ん中 at・半径 radius（m）・色・不透明さ・光るか（煙は光らない）。
    pub type alias Burst = { at = Vec3.Vec3, radius = Float64, color = Color, alpha = Float64, glow = Bool }

    /// 描くのに要る値。time = 実時間の秒（炎と水の揺らぎ）・hand = 手もとの灯りを付ける主人公の足もと。
    pub type alias In = {
        time = Float64,
        camera = Camera3d.Camera3d,
        look = LookDoc.Rec,
        floor = FloorMesh.Built,
        flame = Option[Kit.Item],
        textures = List[Texture3d.Texture3d],
        hand = Vec3.Vec3,
        figures = List[Figure],
        bursts = List[Burst],
        marks = List[Mark],
        lanes = List[Lane]
    }

    def k(v: Float64): Field = Field.Const(v)

    // ── 材質 ──

    /// 光を受けず、頂点の色で光る半透明の材質（床の印・丸い影）。
    def glowMaterial(name: String, alpha: Float64): Scene3d.Material3d =
        let ch = (c: TexChan) -> Field.VertexColor({ chan = c });
        Scene3d.material({ name = name, cycleRate = 0.0, bindings = Nil,
                           out = Out.Fill({ shade = Shade.Solid(Color.rgb(0.0, 0.0, 0.0)), alpha = k(alpha) }) })
            |> Scene3d.withEmission(Shade.Rgb({ r = ch(TexChan.R), g = ch(TexChan.G), b = ch(TexChan.B) }))
            |> Scene3d.withAlphaBlend(true) |> Scene3d.withDoubleSided(true)

    /// 円の材質: 真ん中ほど濃く縁で消える。色と不透明さは param（円ごとに替えても同じプログラム）。
    def burstMaterial(): Scene3d.Material3d =
        let param = (name: String) -> Field.Param({ name = name });
        let disk = Field.Smoothstep({ lo = 0.0, hi = 1.0, of = Field.VertexColor({ chan = TexChan.R }) });
        Scene3d.material({ name = "burst", cycleRate = 0.0, bindings = Nil,
                           out = Out.Fill({ shade = Shade.Solid(Color.rgb(0.0, 0.0, 0.0)),
                                            alpha = Field.Bin({ op = ShaderDoc.BinOp.Mul, a = param("a"), b = disk }) }) })
            |> Scene3d.withEmission(Shade.Rgb({ r = param("r"), g = param("g"), b = param("b") }))
            |> Scene3d.withAlphaBlend(true) |> Scene3d.withDoubleSided(true)

    /// 炎: 地の色は黒で光を受けず、色だけで光る。
    def flameMaterial(color: Color, glow: Float64): Scene3d.Material3d =
        Scene3d.material({ name = "flame", cycleRate = 0.0, bindings = Nil,
                           out = Out.Fill({ shade = Shade.Solid(Color.rgb(0.0, 0.0, 0.0)), alpha = k(1.0) }) })
            |> Scene3d.withEmission(Shade.Solid(color)) |> Scene3d.withEmissionIntensity(glow)

    /// 水面: 上から見た uv の上をゆっくり流れる明暗（半透明・少しの照り返し）。
    def waterMaterial(lk: LookDoc.Rec): Scene3d.Material3d =
        let flow = Field.Scaled({ factor = 1.0, scroll = { x = lk#waterFlow, y = lk#waterFlow * 0.4 }, of = Field.Fbm({ octaves = 3, scale = 6.0, seed = 5 }) });
        let glint = Field.Smoothstep({ lo = 0.6, hi = 0.78, of = Field.Scaled({ factor = 1.0, scroll = { x = 0.0 - lk#waterFlow * 0.7, y = lk#waterFlow * 0.5 },
                                                                                  of = Field.Fbm({ octaves = 2, scale = 14.0, seed = 9 }) }) });
        let field = Field.Bin({ op = ShaderDoc.BinOp.Add, a = Field.Smoothstep({ lo = 0.3, hi = 0.8, of = flow }), b = glint });
        Scene3d.material({ name = "water", cycleRate = 0.0, bindings = Nil,
                           out = Out.Fill({ shade = Shade.Ramp({ lo = lk#waterDeep, hi = lk#waterLight, field = field }), alpha = k(lk#waterAlpha) }) })
            |> Scene3d.withAlphaBlend(true) |> Scene3d.withMetallicRoughness(0.0, lk#waterRoughness)

    // ── 揺らぎ ──

    /// 松明 i の今の揺れ（−1..1 くらい）。
    def flicker(t: Float64, i: Int32): Float64 =
        let f = Int32.toFloat64(i);
        0.55 * Num.sin(t * 8.3 + f * 1.7) + 0.3 * Num.sin(t * 21.7 + f * 2.9) + 0.15 * Num.sin(t * 3.1 + f)

    // ── 光 ──

    def pointLights(w: In): List[Scene3d.PointLight] =
        let lk = w#look;
        let torches = w#floor#torches |> List.zipWithIndex |> List.map(match (i, torch) ->
            { position = Vec3.add(torch#flame, Vec3.mul(torch#facing, 0.25)), color = lk#torchColor,
              intensity = lk#torchIntensity * (1.0 + lk#flameFlicker * 0.6 * flicker(w#time, i)), range = lk#torchRange });
        // WhyNot: 灯りを主人公の手前に置かない — 板の正面へ真っ直ぐ当たり、主人公の絵が白く飛ぶ。
        let hand = { position = Vec3.add(w#hand, Vec3.v3(0.25, lk#handHeight, 0.35)), color = lk#handColor, intensity = lk#handIntensity, range = lk#handRange };
        let water = w#floor#waterLights |> List.map(at -> { position = at, color = lk#waterLightColor, intensity = lk#waterLightIntensity, range = lk#waterLightRange });
        // 爆弾の光は一番大きな円 1 つにだけ灯りを付ける（灯りの数の上限を、光の円の数で食わない）。
        let blast = w#bursts |> List.filter(b -> b#glow) |> List.head |> Option.map(b ->
            { position = b#at, color = b#color, intensity = 8.0 * b#alpha, range = 2.0 + 2.0 * b#radius }) |> Option.toList;
        hand :: List.flatten(blast :: torches :: water :: Nil)

    /// 影を落とす月の光（方角と高さから。光の進む向きは光の来る向きの逆）。
    def moonOf(lk: LookDoc.Rec): Scene3d.DirectionalLight =
        let (az, el) = (Cam.rad(lk#moonAzimuth), Cam.rad(lk#moonElevation));
        let toward = Vec3.v3(Num.sin(az) * Num.cos(el), Num.sin(el), Num.cos(az) * Num.cos(el));
        { color = lk#moonColor, intensity = lk#moonIntensity,
          shadow = Some({ distance = 40.0, casterReach = 20.0, strength = lk#shadowStrength, mapSize = 2048, pcfRadius = 2 | Scene3d.directionalShadow() })
          | Scene3d.directionalLight(Vec3.mul(toward, -1.0)) }

    // ── 置く物 ──

    def solids(w: In): List[Scene3d.Instance3d] =
        w#floor#solids |> List.map(g -> {
            let placed = Scene3d.meshInstance(g#mesh, Mat4.identity()) |> Scene3d.withTint(if (g#key == "wall_top") w#look#wallTopTint else Color.rgb(1.0, 1.0, 1.0));
            g#material |> Option.map(m -> Scene3d.withMaterial(m, placed)) |> Option.getWithDefault(placed)
        })

    def water(w: In): List[Scene3d.Instance3d] =
        w#floor#water |> Option.map(mesh -> Scene3d.meshInstance(mesh, Mat4.identity()) |> Scene3d.withMaterial(waterMaterial(w#look))) |> Option.toList

    /// 炎（松明ごとに、炎の真ん中を軸に大きさを揺らす）。
    def flames(w: In): List[Scene3d.Instance3d] =
        let lk = w#look;
        w#flame |> Option.map(item -> w#floor#torches |> List.zipWithIndex |> List.flatMap(match (i, torch) -> {
            let wave = flicker(w#time, i);
            let s = 1.0 + lk#flameFlicker * 0.5 * wave;
            let pivot = torch#pivot;
            let swell = Mat4.mul(Mat4.translate(pivot), Mat4.mul(Mat4.scale(Vec3.v3(1.0 - 0.3 * (s - 1.0), s, 1.0 - 0.3 * (s - 1.0))), Mat4.translate(Vec3.mul(pivot, -1.0))));
            let m = flameMaterial(lk#flameColor, lk#flameGlow * (1.0 + lk#flameFlicker * 0.4 * wave));
            item#parts |> List.map(p -> Scene3d.meshInstance(p#mesh, Mat4.mul(torch#world, swell)) |> Scene3d.withMaterial(m))
        })) |> Option.getWithDefault(Nil)

    /// 床の上の水平な四角（真ん中 c・x と z の半分の幅・高さ y）。
    def quad(c: Vec3.Vec3, hx: Float64, hz: Float64, y: Float64, color: Color): Mesh3d.Polygon =
        { points = Vec3.v3(c#x - hx, y, c#z + hz) :: Vec3.v3(c#x + hx, y, c#z + hz) :: Vec3.v3(c#x + hx, y, c#z - hz) :: Vec3.v3(c#x - hx, y, c#z - hz) :: Nil,
          color = color }

    /// 印 1 つの四角の列（枠は 4 辺・破線は 4 辺を 3 つずつに切る）。
    def markPolys(m: Mark): List[Mesh3d.Polygon] =
        let (h, t, y) = (0.46, 0.035, 0.04);
        let c = m#at;
        let side = (dx: Float64, dz: Float64, hx: Float64, hz: Float64) -> quad(Vec3.add(c, Vec3.v3(dx, 0.0, dz)), hx, hz, y, m#color);
        match m#stroke {
            case Stroke.Fill   => quad(c, 0.47, 0.47, y - 0.005, m#color) :: Nil
            case Stroke.Frame  => side(0.0, h, h, t) :: side(0.0, 0.0 - h, h, t) :: side(h, 0.0, t, h - t) :: side(0.0 - h, 0.0, t, h - t) :: Nil
            case Stroke.Dashed =>
                (-0.31 :: 0.0 :: 0.31 :: Nil) |> List.flatMap(o ->
                    side(o, h, 0.09, t) :: side(o, 0.0 - h, 0.09, t) :: side(h, o, t, 0.09) :: side(0.0 - h, o, t, 0.09) :: Nil)
            case Stroke.Guard(fx, fz) =>
                let facing = Num.atan2(fz, fx);
                let (inner, outer, spread, n) = (0.5, 0.68, 1.2, 10);
                let point = (r: Float64, a: Float64) -> Vec3.add(c, Vec3.v3(r * Num.cos(a), y + 0.002, r * Num.sin(a)));
                let arc = List.range(0, n) |> List.map(k -> {
                    let a0 = facing - spread + 2.0 * spread * Int32.toFloat64(k) / Int32.toFloat64(n);
                    let a1 = facing - spread + 2.0 * spread * Int32.toFloat64(k + 1) / Int32.toFloat64(n);
                    { points = point(inner, a1) :: point(outer, a1) :: point(outer, a0) :: point(inner, a0) :: Nil, color = m#color }
                });
                // 奥を向くと弧の真ん中は体の板に隠れるので、板の頭より先のマスに向きの山形を置く。
                let (tip, back, wing, thick) = (1.25, 0.95, 0.45, 0.09);
                let chevron = (hand: Float64) ->
                    { points = point(tip, facing) :: point(back + thick, facing + hand * wing) :: point(back, facing + hand * wing) :: point(tip - thick, facing) :: Nil,
                      color = m#color };
                arc ::: chevron(1.0) :: chevron(-1.0) :: Nil
        }

    /// 破線 1 本の四角の列（0.3 m おきに 0.16 m の線）。
    def lanePolys(l: Lane): List[Mesh3d.Polygon] =
        let d = Vec3.sub(l#tip, l#origin);
        let len = Vec3.length(d);
        let n = Float64.floor(len / 0.3) |> Float64.tryToInt32 |> Option.getWithDefault(0);
        let along = Vec3.mul(d, 1.0 / Float64.max(0.001, len));
        let wide = Float64.abs(along#x) > 0.5;
        List.range(0, n) |> List.map(i -> {
            let c = Vec3.add(l#origin, Vec3.mul(along, 0.15 + Int32.toFloat64(i) * 0.3));
            if (wide) quad(c, 0.08, 0.035, 0.045, l#color) else quad(c, 0.035, 0.08, 0.045, l#color)
        })

    /// 床の印を 2 つの形にまとめる（薄い塗りと、線）。
    def marks(w: In): List[Scene3d.Instance3d] =
        let fills = w#marks |> List.filter(m -> m#stroke == Stroke.Fill) |> List.flatMap(markPolys);
        let lines = List.append(w#marks |> List.filter(m -> m#stroke != Stroke.Fill) |> List.flatMap(markPolys), w#lanes |> List.flatMap(lanePolys));
        let key = marksKey(w);
        let one = (polys: List[Mesh3d.Polygon], name: String, alpha: Float64) ->
            if (List.isEmpty(polys)) Nil
            else Scene3d.meshInstance(Mesh3d.withContentKey("${name}:${key}", Mesh3d.fromPolygons(polys)), Mat4.identity())
                |> Scene3d.withMaterial(glowMaterial(name, alpha)) |> (i -> i :: Nil);
        List.append(one(fills, "mark_fill", 0.2), one(lines, "mark_line", 0.9))

    /// 床の印の中身の鍵（同じ印のコマは GL が同じ頂点の列を使い、中身の SHA-256 も取らない）。
    def marksKey(w: In): String =
        let mark = (m: Mark) -> "${m#at#x},${m#at#z},${m#stroke},${Color.toHex(m#color)}";
        let lane = (l: Lane) -> "${l#origin#x},${l#origin#z},${l#tip#x},${l#tip#z},${Color.toHex(l#color)}";
        "${List.join(";", List.map(mark, w#marks))}|${List.join(";", List.map(lane, w#lanes))}"

    /// 丸い影（板ごとに 3 枚の同心円を重ね、真ん中ほど濃くする）を 1 つの形にまとめる。
    /// WhyNot: 影の地図の影に任せない — 板は影を落とさない（engine の決まり）ので、足が床に着いて見えない。
    def contactShadows(w: In): List[Scene3d.Instance3d] =
        let disc = (c: Vec3.Vec3, r: Float64, y: Float64) -> {
            let ring = List.range(0, 18) |> List.map(i -> {
                let a = 0.0 - Int32.toFloat64(i) / 18.0 * Num.tau();
                Vec3.v3(c#x + Num.cos(a) * r, y, c#z + Num.sin(a) * r * 0.8)
            });
            { points = ring, color = Color.rgb(0.0, 0.0, 0.0) }
        };
        let polys = w#figures |> List.filter(f -> f#shadow > 0.0) |> List.flatMap(f ->
            disc(f#at, f#shadow, 0.012) :: disc(f#at, f#shadow * 0.72, 0.014) :: disc(f#at, f#shadow * 0.45, 0.016) :: Nil);
        let material = Scene3d.material({ name = "contact_shadow", cycleRate = 0.0, bindings = Nil,
                                          out = Out.Fill({ shade = Shade.Solid(Color.rgb(0.0, 0.0, 0.0)), alpha = k(0.2) }) })
            |> Scene3d.withAlphaBlend(true) |> Scene3d.withDoubleSided(true);
        let key = w#figures |> List.filter(f -> f#shadow > 0.0) |> List.map(f -> "${f#at#x},${f#at#z},${f#shadow}") |> List.join(";");
        if (List.isEmpty(polys)) Nil
        else Scene3d.meshInstance(Mesh3d.withContentKey("contact_shadow:${key}", Mesh3d.fromPolygons(polys)), Mat4.identity())
            |> Scene3d.withMaterial(material) |> (i -> i :: Nil)

    /// 円の板の形: カメラに正対する半径 1 m の円（真ん中の頂点の色は白・縁は黒。材質が色の赤で縁を薄くする）。カメラの向きごとに鍵を付ける。
    def burstDisc(camera: Camera3d.Camera3d): Option[Mesh3d.Mesh3d] =
        let forward = Vec3.v3(Num.sin(camera#yaw) * Num.cos(camera#pitch), 0.0 - Num.sin(camera#pitch), Num.cos(camera#yaw) * Num.cos(camera#pitch));
        let right = Vec3.normalized(Vec3.cross(forward, Vec3.v3(0.0, 1.0, 0.0)));
        let upward = Vec3.cross(right, forward);
        let n = 20;
        let rim = List.range(0, n) |> List.map(i -> {
            let a = Int32.toFloat64(i) / Int32.toFloat64(n) * Num.tau();
            Vec3.add(Vec3.mul(right, Num.cos(a)), Vec3.mul(upward, Num.sin(a)))
        });
        let points = Vec3.zero() :: rim;
        let toward = Vec3.mul(forward, -1.0);
        Mesh3d.fromArrays({ positions = points |> List.flatMap(q -> q#x :: q#y :: q#z :: Nil) |> List.toVector,
                            normals = points |> List.flatMap(_ -> toward#x :: toward#y :: toward#z :: Nil) |> List.toVector,
                            colors = List.toVector((1.0 :: 1.0 :: 1.0 :: Nil) ::: (rim |> List.flatMap(_ -> 0.0 :: 0.0 :: 0.0 :: Nil))),
                            indices = List.range(0, n) |> List.flatMap(i -> 0 :: (i + 1) :: (Int32.remainder(i + 1, n) + 1) :: Nil) |> List.toVector,
                            skin = None })
            |> Result.toOption |> Option.map(Mesh3d.withContentKey("burst_disc:${camera#yaw}:${camera#pitch}"))

    /// 広がる円（爆弾の光と煙）。
    def bursts(w: In): List[Scene3d.Instance3d] =
        if (List.isEmpty(w#bursts)) Nil
        else burstDisc(w#camera) |> Option.map(quad -> w#bursts |> List.map(b -> {
            let (r, g, bl) = Color.channels(b#color);
            let light = if (b#glow) 3.0 else 1.0;
            let params = Map#{"r" => r * light, "g" => g * light, "b" => bl * light, "a" => b#alpha};
            Scene3d.meshInstance(quad, Mat4.mul(Mat4.translate(b#at), Mat4.scale(Vec3.v3(b#radius, b#radius, b#radius))))
                |> Scene3d.withMaterial(Scene3d.withParams(params, burstMaterial()))
        })) |> Option.getWithDefault(Nil)

    /// 板の足もとを目の方へ寄せる距離（m）。奥の壁の厚み 1 マスより大きいと、手前の壁より前に出てしまう。
    def billboardPull(): Float64 = 0.9

    /// 板（カメラに正対・α 0.5 で切る）。
    def billboards(w: In): List[Scene3d.Billboard3d] =
        w#figures |> List.map(f -> {
            let pic = f#picture;
            // 板はカメラに正対して後ろへ倒れるので、足もとに立てると上半分が奥の壁へめり込む。
            // 足もとを目の方へ寄せ、寄せた分だけ縮めて、画面の上の位置と大きさは変えない。
            let toEye = Vec3.sub(w#camera#eye, f#at);
            let dist = Vec3.length(toEye);
            let pull = Float64.min(billboardPull(), dist * 0.5);
            let at = Vec3.add(f#at, Vec3.mul(toEye, pull / dist));
            let h = f#height * (dist - pull) / dist;
            let board = Scene3d.billboard(pic#name, { position = Vec2.zero(), size = { x = pic#width, y = pic#height } }, at,
                                          { x = h * pic#width / pic#height, y = h })
                |> Scene3d.withBillboardTint(f#tint);
            if (f#lit) board else Scene3d.unlit(board)
        })

    // ── 1 コマの場面 ──

    /// 光と置く物の数（描き出しが報告に使う）。
    /// drawn = 視錐台の中で描く置いた物と板の数（engine の間引きと同じ判定）。
    pub def countsOf(size: Vec2.Vec2, w: In): { instances = Int32, billboards = Int32, drawn = Int32, lights = Int32, lightsInView = Int32 } =
        let frustum = Camera3d.frustum(w#camera, size#x / size#y);
        let lights = pointLights(w);
        let inView = List.count(i -> not Scene3d.isOutsideFrustum(frustum, i), instancesOf(w))
            + List.count(b -> not Scene3d.isBillboardOutsideFrustum(frustum, b), billboards(w));
        { instances = List.length(instancesOf(w)), billboards = List.length(w#figures), drawn = inView, lights = List.length(lights),
          lightsInView = lights |> List.filter(l -> Frustum.intersectsSphere(frustum, l#position, l#range)) |> List.length }

    def instancesOf(w: In): List[Scene3d.Instance3d] =
        List.flatten(solids(w) :: flames(w) :: contactShadows(w) :: marks(w) :: water(w) :: bursts(w) :: Nil)

    /// ダンジョン全部を engine の Scene3d 1 つで描く。
    pub def scene(size: Vec2.Vec2, w: In): Render.PlacedItem =
        let lk = w#look;
        Render.scene3d(Vec2.zero(), { instances = instancesOf(w), billboards = billboards(w), textures = w#textures,
                                      pointLights = pointLights(w), directionalLight = Some(moonOf(lk)),
                                      environment = { ambient = lk#ambient, hemisphere = 0.0, toonSteps = 0, fog = None, background = None },
                                      time = w#time | Scene3d.scene(w#camera, size) })
}
