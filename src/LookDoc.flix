// LookDoc — stepdown.look.json（見せ方の数: 1 手の秒・カメラ・光・炎・水・後処理）の Doc 層。fail-open — 読めないときは defaults()。
mod LookDoc {
    use fge::{Color, DocJson, JsonCodec, UiDoc}
    use Util.Json.Json

    /// 見せ方の数（色は画面の値・光の強さは線形の量）。
    ///   stepSeconds … 1 歩を見せる秒
    ///   dashStepSeconds … ダッシュの 1 歩を見せる秒
    ///   strikeSeconds … 殴るを見せる秒
    ///   arrowSeconds … 矢が飛ぶ秒
    ///   cameraPitch … カメラの見下ろす角度（度。90 で真上）
    ///   cameraYaw … カメラの向き（度。0 で盤の下側から見る）
    ///   cameraDistance … カメラから注目点までの距離（マス）
    ///   cameraFov … 縦の画角（度。46.4 で 28mm のレンズ相当）
    ///   cameraLead … 注目点を主人公より奥へずらす量（マス。主人公が画面の少し下に来る）
    ///   cameraFollowSeconds … 主人公を追う遅れ（秒）
    ///   ambient … どこにでも当たる光
    ///   moonColor … 影を落とす平行光の色
    ///   moonIntensity … 平行光の強さ
    ///   moonElevation … 平行光が来る高さ（度）
    ///   moonAzimuth … 平行光が来る方角（度。0 で北から・90 で東から）
    ///   wallTopTint … 壁の上面に掛ける色（暗くして床と分ける）
    ///   shadowStrength … 平行光の影の濃さ（0..1）
    ///   torchColor … 松明の点光源の色（赤寄り）
    ///   torchIntensity … 松明の点光源の強さ
    ///   torchRange … 松明の点光源が届く距離（マス）
    ///   handColor … 主人公に付いて動く灯りの色
    ///   handIntensity … 手もとの灯りの強さ
    ///   handRange … 手もとの灯りが届く距離（マス）
    ///   handHeight … 手もとの灯りの高さ（マス）
    ///   waterLightColor … 水面の上に置く青い光の色
    ///   waterLightIntensity … 水面の上の青い光の強さ
    ///   waterLightRange … 水面の上の青い光が届く距離（マス）
    ///   flameColor … 炎の色（光を受けない・明るくしすぎると白に寄る）
    ///   flameGlow … 炎の色に掛ける明るさ
    ///   flameFlicker … 炎の大きさと明るさ・松明の光の揺れ幅（0..1）
    ///   waterDeep … 水面の模様の暗い色
    ///   waterLight … 水面の模様の明るい色
    ///   waterAlpha … 水面の不透明さ（0..1）
    ///   waterRoughness … 水面の粗さ（小さいほど照り返しが鋭い）
    ///   waterFlow … 水面の模様が流れる速さ
    ///   exposure … トーンマップの前に掛ける露出（段）
    ///   whitePoint … この明るさで白になる
    ///   bloomThreshold … ブルームが掛かり始める明るさ
    ///   bloomKnee … しきいから掛かり切るまでの幅
    ///   bloomStrength … ブルームを足す強さ（弱め）
    ///   bloomRadius … ブルームのぼかしの半径（画面の高さに対する割合）
    ///   bloomTaps … ブルームのぼかしの標本の数
    ///   vignette … 画面の周辺を暗くする量
    ///   saturation … トーンマップの後に足す彩度
    ///   msaaSamples … 場面の縁のならしの標本の数（窓だけ）
    ///   figureLight … キャラの板の明るさ（板は光を受けない。絵の色にこれを掛ける）
    pub type alias Rec = {
        version = Int32,
        stepSeconds = Float64,
        dashStepSeconds = Float64,
        strikeSeconds = Float64,
        arrowSeconds = Float64,
        cameraPitch = Float64,
        cameraYaw = Float64,
        cameraDistance = Float64,
        cameraFov = Float64,
        cameraLead = Float64,
        cameraFollowSeconds = Float64,
        ambient = Color,
        moonColor = Color,
        moonIntensity = Float64,
        moonElevation = Float64,
        moonAzimuth = Float64,
        wallTopTint = Color,
        shadowStrength = Float64,
        torchColor = Color,
        torchIntensity = Float64,
        torchRange = Float64,
        handColor = Color,
        handIntensity = Float64,
        handRange = Float64,
        handHeight = Float64,
        waterLightColor = Color,
        waterLightIntensity = Float64,
        waterLightRange = Float64,
        flameColor = Color,
        flameGlow = Float64,
        flameFlicker = Float64,
        waterDeep = Color,
        waterLight = Color,
        waterAlpha = Float64,
        waterRoughness = Float64,
        waterFlow = Float64,
        exposure = Float64,
        whitePoint = Float64,
        bloomThreshold = Float64,
        bloomKnee = Float64,
        bloomStrength = Float64,
        bloomRadius = Float64,
        bloomTaps = Int32,
        vignette = Float64,
        saturation = Float64,
        msaaSamples = Int32,
        figureLight = Float64
    }

    pub enum Doc(Rec)

    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.look.json"

    /// 既定の色を #rrggbb で書くための近道（書き損じはマゼンタで目に見せる）。
    def hx(text: String): Color = Color.hex(text) |> Option.getWithDefault(Color.rgb(1.0, 0.0, 1.0))

    pub def defaults(): Doc =
        Doc.Doc({ version = 1, stepSeconds = 0.14, dashStepSeconds = 0.07, strikeSeconds = 0.18, arrowSeconds = 0.22, cameraPitch = 62.0, cameraYaw = 0.0, cameraDistance = 17.5, cameraFov = 46.4, cameraLead = 0.9, cameraFollowSeconds = 0.18, ambient = hx("#2a3450"), moonColor = hx("#a6bfff"), moonIntensity = 0.4, moonElevation = 62.0, moonAzimuth = 200.0, wallTopTint = hx("#5a5560"), shadowStrength = 0.55, torchColor = hx("#ff4d14"), torchIntensity = 1.0, torchRange = 5.0, handColor = hx("#ffc27a"), handIntensity = 6.0, handRange = 7.0, handHeight = 2.0, waterLightColor = hx("#73ccff"), waterLightIntensity = 1.2, waterLightRange = 4.0, flameColor = hx("#ff1a03"), flameGlow = 1.0, flameFlicker = 0.18, waterDeep = hx("#0b3346"), waterLight = hx("#3d8fa8"), waterAlpha = 0.72, waterRoughness = 0.18, waterFlow = 0.02, exposure = 0.3, whitePoint = 4.0, bloomThreshold = 0.9, bloomKnee = 0.6, bloomStrength = 0.45, bloomRadius = 0.035, bloomTaps = 12, vignette = 0.45, saturation = 0.05, msaaSamples = 4, figureLight = 0.85 })

    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let d = rec(defaults());
        let float = (key, fallback) -> JsonCodec.floatOr(key, fallback, root);
        let int = (key, fallback) -> JsonCodec.intOr(key, fallback, root);
        let color = (key, fallback) -> colorOr(fallback, key, root);
        Doc.Doc({ version = JsonCodec.intOr("version", d#version, root),
                  stepSeconds = float("stepSeconds", d#stepSeconds),
                  dashStepSeconds = float("dashStepSeconds", d#dashStepSeconds),
                  strikeSeconds = float("strikeSeconds", d#strikeSeconds),
                  arrowSeconds = float("arrowSeconds", d#arrowSeconds),
                  cameraPitch = float("cameraPitch", d#cameraPitch),
                  cameraYaw = float("cameraYaw", d#cameraYaw),
                  cameraDistance = float("cameraDistance", d#cameraDistance),
                  cameraFov = float("cameraFov", d#cameraFov),
                  cameraLead = float("cameraLead", d#cameraLead),
                  cameraFollowSeconds = float("cameraFollowSeconds", d#cameraFollowSeconds),
                  ambient = color("ambient", d#ambient),
                  moonColor = color("moonColor", d#moonColor),
                  moonIntensity = float("moonIntensity", d#moonIntensity),
                  moonElevation = float("moonElevation", d#moonElevation),
                  moonAzimuth = float("moonAzimuth", d#moonAzimuth),
                  wallTopTint = color("wallTopTint", d#wallTopTint),
                  shadowStrength = float("shadowStrength", d#shadowStrength),
                  torchColor = color("torchColor", d#torchColor),
                  torchIntensity = float("torchIntensity", d#torchIntensity),
                  torchRange = float("torchRange", d#torchRange),
                  handColor = color("handColor", d#handColor),
                  handIntensity = float("handIntensity", d#handIntensity),
                  handRange = float("handRange", d#handRange),
                  handHeight = float("handHeight", d#handHeight),
                  waterLightColor = color("waterLightColor", d#waterLightColor),
                  waterLightIntensity = float("waterLightIntensity", d#waterLightIntensity),
                  waterLightRange = float("waterLightRange", d#waterLightRange),
                  flameColor = color("flameColor", d#flameColor),
                  flameGlow = float("flameGlow", d#flameGlow),
                  flameFlicker = float("flameFlicker", d#flameFlicker),
                  waterDeep = color("waterDeep", d#waterDeep),
                  waterLight = color("waterLight", d#waterLight),
                  waterAlpha = float("waterAlpha", d#waterAlpha),
                  waterRoughness = float("waterRoughness", d#waterRoughness),
                  waterFlow = float("waterFlow", d#waterFlow),
                  exposure = float("exposure", d#exposure),
                  whitePoint = float("whitePoint", d#whitePoint),
                  bloomThreshold = float("bloomThreshold", d#bloomThreshold),
                  bloomKnee = float("bloomKnee", d#bloomKnee),
                  bloomStrength = float("bloomStrength", d#bloomStrength),
                  bloomRadius = float("bloomRadius", d#bloomRadius),
                  bloomTaps = int("bloomTaps", d#bloomTaps),
                  vignette = float("vignette", d#vignette),
                  saturation = float("saturation", d#saturation),
                  msaaSamples = int("msaaSamples", d#msaaSamples),
                  figureLight = float("figureLight", d#figureLight) })

    def colorOr(fallback: Color, key: String, obj: Map[String, Json]): Color =
        Map.get(key, obj)
            |> Option.flatMap(json -> UiDoc.colorOf(Map.empty(), key, json) |> Result.toOption)
            |> Option.getWithDefault(fallback)

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
