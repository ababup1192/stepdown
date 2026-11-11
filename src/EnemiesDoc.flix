// EnemiesDoc — stepdown.enemies.json（敵ごとの名前・HP・攻撃・初出の階）の Doc 層。
// 動き方はコード（Enemy.behaviourOf・Step）、階ごとの数は floors の Doc。キーは Enemy.Kind の名前。
// fail-open — 読めない Doc・書いていない敵・書いていない欄は defaults() の値へ落とす。
mod EnemiesDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 敵 1 種の強さ。firstFloor より前の階には出ない。
    pub type alias Stats = { name = String, hp = Int32, attack = Int32, firstFloor = Int32 }

    pub type alias Rec = { version = Int32, stats = Map[Enemy.Kind, Stats] }

    pub enum Doc(Rec)

    def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.enemies.json"

    /// コード側の既定（assets/stepdown.enemies.json と同じ値から出発する）。
    def defaultStats(kind: Enemy.Kind): Stats = match kind {
        case Enemy.Kind.Jelly  => { name = "ゼリー", hp = 5, attack = 2, firstFloor = 1 }
        case Enemy.Kind.Bat    => { name = "コウモリ", hp = 3, attack = 1, firstFloor = 1 }
        case Enemy.Kind.Archer => { name = "弓兵", hp = 4, attack = 2, firstFloor = 1 }
        case Enemy.Kind.Swarm  => { name = "群れ", hp = 3, attack = 1, firstFloor = 2 }
    }

    pub def defaults(): Doc =
        Doc.Doc({ version = 1, stats = Enemy.all() |> List.map(kind -> (kind, defaultStats(kind))) |> List.toMap })

    /// 敵の強さ（Doc に無い敵は既定）。
    pub def statsOf(kind: Enemy.Kind, d: Doc): Stats =
        Map.get(kind, rec(d)#stats) |> Option.getWithDefault(defaultStats(kind))

    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let enemies = Map.get("enemies", root) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        let objOf = kind -> Map.get("${kind}", enemies) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        Doc.Doc({ version = JsonCodec.intOr("version", 1, root),
                  stats = Enemy.all() |> List.map(kind -> (kind, statsFrom(defaultStats(kind), objOf(kind)))) |> List.toMap })

    def statsFrom(d: Stats, obj: Map[String, Json]): Stats =
        { name = JsonCodec.stringOr("name", d#name, obj), hp = JsonCodec.intOr("hp", d#hp, obj),
          attack = JsonCodec.intOr("attack", d#attack, obj), firstFloor = JsonCodec.intOr("firstFloor", d#firstFloor, obj) }

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
