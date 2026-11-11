// RulesDoc — stepdown.rules.json（遊びの決まりの数: HP・攻撃・矢の射程・爆弾が出る数・薬の閾値・自然回復・見える距離・持ち物の上限・空腹・タップ）の Doc 層。
// 判定の式・敵の動き方はコード（Step）。秒は look の Doc（LookDoc）。
// fail-open — 読めない・壊れているときは defaults() へ落とし、ゲームは必ず起動する。
mod RulesDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 遊びの決まりの数。
    ///   maxHp          … 最大 HP（階の始め）
    ///   baseAttack     … 剣を持たない時の攻撃
    ///   arrowMin/Max   … 矢を撃つ距離（主人公も弓兵も同じ。1 には撃たない）
    ///   bombNeeds      … 8 近傍の起きた敵がこの数以上で爆弾を投げる
    ///   potionAt       … HP が 最大 HP × これ 以下になったら薬を使う
    ///   regenEvery     … 移動したこの手数ごとに HP +1
    ///   wakeSteps      … 見える距離（歩く距離）。眠った敵が起き、ダッシュが止まる
    ///   enemyMinSteps  … 階の始めに敵を置くのは、始まりからこの歩数以上
    ///   arrowCap・bombCap・potionCap … 持ち物の上限
    ///   burnDamage     … 燃える敵が 1 手ごとに受けるダメージ
    ///   fullnessMax    … 満腹度の最大（階の始め）
    ///   hungerEvery    … 手が進んだこの数ごとに満腹度 -1
    ///   starveDamage   … 満腹度 0 の間、1 手ごとに減る HP
    ///   tapWaitRadius  … 主人公のマスの中でタップしたら待つとみなす半径（マス）
    ///   tapSlop        … 押してから動いてもタップとみなす距離（px）
    pub type alias Rec = {
        version = Int32,
        maxHp = Int32,
        baseAttack = Int32,
        arrowMin = Int32,
        arrowMax = Int32,
        bombNeeds = Int32,
        potionAt = Float64,
        regenEvery = Int32,
        wakeSteps = Int32,
        enemyMinSteps = Int32,
        arrowCap = Int32,
        bombCap = Int32,
        potionCap = Int32,
        burnDamage = Int32,
        fullnessMax = Int32,
        hungerEvery = Int32,
        starveDamage = Int32,
        tapWaitRadius = Float64,
        tapSlop = Float64
    }

    /// 型検査が record を展開しないため、持ち回る形は名前付きの enum にする。
    pub enum Doc(Rec)

    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.rules.json"

    /// コード側の既定値（assets/stepdown.rules.json と同じ値から出発する）。
    pub def defaults(): Doc =
        Doc.Doc({ version = 1, maxHp = 20, baseAttack = 2, arrowMin = 2, arrowMax = 4, bombNeeds = 2,
                  potionAt = 0.25, regenEvery = 12, wakeSteps = 5, enemyMinSteps = 5,
                  arrowCap = 15, bombCap = 5, potionCap = 3, burnDamage = 1,
                  fullnessMax = 100, hungerEvery = 3, starveDamage = 1,
                  tapWaitRadius = 0.45, tapSlop = 12.0 })

    /// テキストから Doc へ（fail-open）。JSON でない・形が違うフィールドは既定へ落とす。
    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let d = rec(defaults());
        let int = (key, fallback) -> JsonCodec.intOr(key, fallback, root);
        let float = (key, fallback) -> JsonCodec.floatOr(key, fallback, root);
        Doc.Doc({ version = int("version", d#version), maxHp = int("maxHp", d#maxHp),
                  baseAttack = int("baseAttack", d#baseAttack), arrowMin = int("arrowMin", d#arrowMin),
                  arrowMax = int("arrowMax", d#arrowMax), bombNeeds = int("bombNeeds", d#bombNeeds),
                  potionAt = float("potionAt", d#potionAt), regenEvery = int("regenEvery", d#regenEvery),
                  wakeSteps = int("wakeSteps", d#wakeSteps), enemyMinSteps = int("enemyMinSteps", d#enemyMinSteps),
                  arrowCap = int("arrowCap", d#arrowCap), bombCap = int("bombCap", d#bombCap),
                  potionCap = int("potionCap", d#potionCap), burnDamage = int("burnDamage", d#burnDamage),
                  fullnessMax = int("fullnessMax", d#fullnessMax), hungerEvery = int("hungerEvery", d#hungerEvery),
                  starveDamage = int("starveDamage", d#starveDamage),
                  tapWaitRadius = float("tapWaitRadius", d#tapWaitRadius), tapSlop = float("tapSlop", d#tapSlop) })

    /// 薬を使う HP（最大 HP × potionAt を切り捨て）。
    pub def potionHp(maxHp: Int32, d: Doc): Int32 =
        Float64.floor(Int32.toFloat64(maxHp) * rec(d)#potionAt) |> Float64.tryToInt32 |> Option.getWithDefault(0)

    /// 矢を撃つ距離（主人公も弓兵も同じ）。
    pub def reachOf(d: Doc): Board.Reach = { nearest = rec(d)#arrowMin, farthest = rec(d)#arrowMax }

    /// 系統の持てる上限。
    pub def capOf(family: Item.Family, d: Doc): Int32 = match family {
        case Item.Family.Arrow  => rec(d)#arrowCap
        case Item.Family.Bomb   => rec(d)#bombCap
        case Item.Family.Potion => rec(d)#potionCap
        case _                  => 1
    }

    /// path の Doc を読む。読めない・壊れているときは defaults（fail-open）。
    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
