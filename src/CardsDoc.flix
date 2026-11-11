// CardsDoc — stepdown.cards.json（レベルアップのカードの量: 候補の枚数・最大 HP・攻撃・届く距離・空腹の遅さ・食べ物・押し出しの壁）の Doc 層。
// カードの種類と効き方はコード（Run.Perk・Step）。ここは量だけ。
// fail-open — 読めない・壊れているときは defaults() へ落とす。
mod CardsDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// カードの量。
    ///   choices     … レベルアップで出る候補の枚数
    ///   maxHp       … 最大 HP に足す（今の HP も同じだけ増える）
    ///   attack      … 攻撃に足す
    ///   reach       … 矢と小石の届く最長の距離に足す
    ///   hungerEvery … 満腹度の減りが遅いカードを持つ時の、満腹度 -1 までの手の数
    ///   feast       … 食べ物で戻る満腹度に足す
    ///   wallDamage  … 押し出しの先が壁・柱・岩の時に足すダメージ
    pub type alias Rec = {
        version = Int32,
        choices = Int32,
        maxHp = Int32,
        attack = Int32,
        reach = Int32,
        hungerEvery = Int32,
        feast = Int32,
        wallDamage = Int32
    }

    /// 型検査が record を展開しないため、持ち回る形は名前付きの enum にする。
    pub enum Doc(Rec)

    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.cards.json"

    /// コード側の既定値（assets/stepdown.cards.json と同じ値から出発する）。
    pub def defaults(): Doc =
        Doc.Doc({ version = 1, choices = 3, maxHp = 5, attack = 1, reach = 1, hungerEvery = 4, feast = 10, wallDamage = 1 })

    /// テキストから Doc へ（fail-open）。JSON でない・形が違うフィールドは既定へ落とす。
    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let d = rec(defaults());
        let int = (key, fallback) -> JsonCodec.intOr(key, fallback, root);
        Doc.Doc({ version = int("version", d#version), choices = int("choices", d#choices), maxHp = int("maxHp", d#maxHp),
                  attack = int("attack", d#attack), reach = int("reach", d#reach), hungerEvery = int("hungerEvery", d#hungerEvery),
                  feast = int("feast", d#feast), wallDamage = int("wallDamage", d#wallDamage) })

    /// path の Doc を読む。読めない・壊れているときは defaults（fail-open）。
    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
