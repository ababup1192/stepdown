// KeysDoc — stepdown.keys.json（キー割り当て）の宣言ドキュメント層。
// 「どのキーがどの操作か」だけを外に出す — 1 手の決まりはコード（Controls / World）のまま。
// ダッシュの 4 つはブラウザ版のスワイプ（project.json の web.touchGestures）が押すキーでもあるので、ほかの操作と重ならないキーにする。
// fail-open — 読めない・壊れているときは defaults() へ落とす。操作の欄が空・全部
// 知らない名前のときも、その操作だけ既定へ落とす — 書き間違いで操作不能を作らない。
mod KeysDoc {
    use fge::{DocJson, InputMap, JsonCodec}
    use fge::GameEngine.Key
    use Util.Json.Json

    /// 操作ごとのキーの列。同じ操作に複数キーを割り当ててよい。
    // World が持つ record は enum で包む（型検査が record を展開しないため）。
    pub type alias Rec = {
        version = Int32,
        left = List[Key],      // 左へ 1 歩
        right = List[Key],     // 右へ 1 歩
        up = List[Key],        // 上へ 1 歩
        down = List[Key],      // 下へ 1 歩
        wait = List[Key],      // その場で待つ（階段の上なら降りる）
        dashUp = List[Key],    // 上へダッシュ
        dashDown = List[Key],  // 下へダッシュ
        dashLeft = List[Key],  // 左へダッシュ
        dashRight = List[Key], // 右へダッシュ
        confirm = List[Key]    // 決定（はじめる・もう一度）
    }
    pub enum Doc(Rec)
    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.keys.json"

    /// コード側の既定（assets/stepdown.keys.json と同じ割り当てから出発する）。
    pub def defaults(): Doc =
        Doc.Doc({ version = 1,
                  left  = Key.Left :: Nil,
                  right = Key.Right :: Nil,
                  up    = Key.Up :: Nil,
                  down  = Key.Down :: Nil,
                  wait  = Key.Space :: Key.Period :: Nil,
                  dashUp = Key.W :: Nil,
                  dashDown = Key.S :: Nil,
                  dashLeft = Key.A :: Nil,
                  dashRight = Key.D :: Nil,
                  confirm = Key.Enter :: Nil })

    /// テキストから Doc へ（fail-open）。JSON でない・形が違う欄は既定へ落とす。
    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    /// path の Doc を読む。読めない・壊れているときは defaults（fail-open）。
    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)

    def fromRoot(root: Map[String, Json]): Doc =
        let d = rec(defaults());
        let keys = (field, fallback) -> keysOr(field, fallback, root);
        Doc.Doc({ version = JsonCodec.intOr("version", d#version, root),
                  left  = keys("left",  d#left),
                  right = keys("right", d#right),
                  up    = keys("up",    d#up),
                  down  = keys("down",  d#down),
                  wait  = keys("wait",  d#wait),
                  dashUp = keys("dashUp", d#dashUp),
                  dashDown = keys("dashDown", d#dashDown),
                  dashLeft = keys("dashLeft", d#dashLeft),
                  dashRight = keys("dashRight", d#dashRight),
                  confirm = keys("confirm", d#confirm) })

    /// 欄の文字列列 → キーの列。知らない名前はその 1 個だけ捨て（InputMap.keysOf）、
    /// 列ごと空になる形（欄が無い・型違い・全部知らない名前）だけ フォールバックする。
    def keysOr(field: String, fallback: List[Key], root: Map[String, Json]): List[Key] =
        Map.get(field, root)
            |> Option.flatMap(element -> JsonCodec.decodeList(JsonCodec.expectString, element))
            |> Option.map(InputMap.keysOf)
            |> Option.flatMap(keys -> if (List.isEmpty(keys)) None else Some(keys))
            |> Option.getWithDefault(fallback)
}
