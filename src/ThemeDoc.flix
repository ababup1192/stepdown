// ThemeDoc — stepdown.theme.json（UI と床の印の色）の宣言ドキュメント層。3D の場面の光と材質の色は LookDoc。
// fail-open: 読めない・壊れているときは defaults() へ落とす（書き損じた色はその色だけ既定）。
mod ThemeDoc {
    use fge::{Color, DocJson, JsonCodec, UiDoc}
    use Util.Json.Json

    /// 色。
    ///   bg … 画面の下地（題名の幕・描き出しの背景）
    ///   ink … 字の色
    ///   shade … 上下の帯のグラデーションの陰
    ///   hpEdge … HP の棒の枠
    ///   hpBack … HP の棒の減った所
    ///   hpTop … HP の棒の上の色
    ///   hpBottom … HP の棒の下の色
    ///   mapFloor … 見たマスの床
    ///   mapWater … 見たマスの水
    ///   mapStairs … 見た階段
    ///   mapHero … 主人公のマス
    ///   mapEnemy … 見えている敵
    ///   logBack … 記録の板
    ///   logEdge … 記録の板の枠
    ///   slotTop … 持ち物の枠の上の色
    ///   slotBottom … 持ち物の枠の下の色
    ///   tierNone … 持っていない枠のふち
    ///   tierWood … 木（薬草・小さい爆弾）の段のふち
    ///   tierIron … 鉄（回復薬・大きい爆弾）の段のふち
    ///   tierSilver … 銀（万能薬・眠り玉）の段のふち
    ///   tierFire … 火の矢の段のふち
    ///   badgeFace … 予告のアイコンの丸の地
    ///   badgeEdge … 予告のアイコンの丸の縁
    ///   labelBack … 予告の文字のラベルの地
    ///   markMove … 移動と待つの枠
    ///   markStrike … 殴るの枠
    ///   markArrow … 矢の枠
    ///   markBomb … 爆弾の枠
    ///   markBlocked … 進めないの枠
    ///   markDescend … 階段を降りるの枠
    ///   markDanger … 敵の攻撃が届くマス・弓兵の射線・受けるダメージ
    ///   sleep … 眠っている敵の頭の上の z
    pub type alias Rec = {
        version = Int32,
        bg = Color,
        ink = Color,
        shade = Color,
        hpEdge = Color,
        hpBack = Color,
        hpTop = Color,
        hpBottom = Color,
        mapFloor = Color,
        mapWater = Color,
        mapStairs = Color,
        mapHero = Color,
        mapEnemy = Color,
        logBack = Color,
        logEdge = Color,
        slotTop = Color,
        slotBottom = Color,
        tierNone = Color,
        tierWood = Color,
        tierIron = Color,
        tierSilver = Color,
        tierFire = Color,
        badgeFace = Color,
        badgeEdge = Color,
        labelBack = Color,
        markMove = Color,
        markStrike = Color,
        markArrow = Color,
        markBomb = Color,
        markBlocked = Color,
        markDescend = Color,
        markDanger = Color,
        sleep = Color
    }
    pub enum Doc(Rec)
    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.theme.json"

    /// コード側の既定値（assets/stepdown.theme.json と同じ値から出発する）。
    pub def defaults(): Doc =
        Doc.Doc({ version = 1, bg = hexOr("#0b0c10"), ink = hexOr("#fff4e0"), shade = hexOr("#000000"), hpEdge = hexOr("#f3d9b0"), hpBack = hexOr("#3a1d1a"), hpTop = hexOr("#ff8f7a"), hpBottom = hexOr("#e24b45"), mapFloor = hexOr("#d8c29a"), mapWater = hexOr("#3fb4d6"), mapStairs = hexOr("#ffffff"), mapHero = hexOr("#ffe08a"), mapEnemy = hexOr("#ff5a4f"), logBack = hexOr("#1c120c"), logEdge = hexOr("#6b4a30"), slotTop = hexOr("#3d2618"), slotBottom = hexOr("#24160e"), tierNone = hexOr("#4a3a2c"), tierWood = hexOr("#8a6040"), tierIron = hexOr("#a7b4c2"), tierSilver = hexOr("#eef2f6"), tierFire = hexOr("#ff7a3a"), badgeFace = hexOr("#fff8e8"), badgeEdge = hexOr("#5a3420"), labelBack = hexOr("#2b1a12"), markMove = hexOr("#e8e0d0"), markStrike = hexOr("#ffd166"), markArrow = hexOr("#7fd4ff"), markBomb = hexOr("#ff9a5c"), markBlocked = hexOr("#8a8478"), markDescend = hexOr("#c9f08a"), markDanger = hexOr("#ff5a4f"), sleep = hexOr("#bcd0ff") })

    /// 既定の色を #rrggbb で書くための近道（書き損じはマゼンタで目に見せる）。
    def hexOr(text: String): Color =
        Color.hex(text) |> Option.getWithDefault(Color.rgb(1.0, 0.0, 1.0))

    /// テキストから Doc へ（fail-open）。色の表記（#rrggbb）は UiDoc.colorOf に委ねる。
    def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let d = rec(defaults());
        let color = (key, fallback) -> colorOr(fallback, key, root);
        Doc.Doc({ version = JsonCodec.intOr("version", d#version, root),
                  bg = color("bg", d#bg),
                  ink = color("ink", d#ink),
                  shade = color("shade", d#shade),
                  hpEdge = color("hpEdge", d#hpEdge),
                  hpBack = color("hpBack", d#hpBack),
                  hpTop = color("hpTop", d#hpTop),
                  hpBottom = color("hpBottom", d#hpBottom),
                  mapFloor = color("mapFloor", d#mapFloor),
                  mapWater = color("mapWater", d#mapWater),
                  mapStairs = color("mapStairs", d#mapStairs),
                  mapHero = color("mapHero", d#mapHero),
                  mapEnemy = color("mapEnemy", d#mapEnemy),
                  logBack = color("logBack", d#logBack),
                  logEdge = color("logEdge", d#logEdge),
                  slotTop = color("slotTop", d#slotTop),
                  slotBottom = color("slotBottom", d#slotBottom),
                  tierNone = color("tierNone", d#tierNone),
                  tierWood = color("tierWood", d#tierWood),
                  tierIron = color("tierIron", d#tierIron),
                  tierSilver = color("tierSilver", d#tierSilver),
                  tierFire = color("tierFire", d#tierFire),
                  badgeFace = color("badgeFace", d#badgeFace),
                  badgeEdge = color("badgeEdge", d#badgeEdge),
                  labelBack = color("labelBack", d#labelBack),
                  markMove = color("markMove", d#markMove),
                  markStrike = color("markStrike", d#markStrike),
                  markArrow = color("markArrow", d#markArrow),
                  markBomb = color("markBomb", d#markBomb),
                  markBlocked = color("markBlocked", d#markBlocked),
                  markDescend = color("markDescend", d#markDescend),
                  markDanger = color("markDanger", d#markDanger),
                  sleep = color("sleep", d#sleep) })

    def colorOr(fallbackColor: Color, key: String, obj: Map[String, Json]): Color =
        Map.get(key, obj)
            |> Option.flatMap(json -> UiDoc.colorOf(Map.empty(), key, json) |> Result.toOption)
            |> Option.getWithDefault(fallbackColor)

    /// path の Doc を読む。読めない・壊れているときは defaults（fail-open）。
    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
