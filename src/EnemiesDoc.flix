// EnemiesDoc — stepdown.enemies.json（敵ごとの名前・HP・攻撃・経験値・落とし物）の Doc 層。
// 動き方はコード（Enemy.behaviourOf・Step）、階ごとの数は floors の Doc。キーは Enemy.Kind の名前。
// fail-open — 読めない Doc・書いていない敵・書いていない欄は defaults() の値へ落とす。
mod EnemiesDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 敵 1 種の強さ。exp は倒した時の経験値、dropChance の確率で drops から 1 つ落とす
    /// （drops は同じ種類を並べるほど出やすい）。chargeMin..chargeMax は突進が溜める距離（走るのは chargeMax + 1 マスまで。突進でない敵は 0）、
    /// blastDamage は自爆虫が弾けた時に周りへ与えるダメージ（盾は効かない。自爆虫でない敵は 0）。
    pub type alias Stats = { name = String, hp = Int32, attack = Int32, exp = Int32, dropChance = Float64, drops = List[Item.Kind], chargeMin = Int32, chargeMax = Int32, blastDamage = Int32 }

    pub type alias Rec = { version = Int32, stats = Map[Enemy.Kind, Stats] }

    pub enum Doc(Rec)

    def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.enemies.json"

    /// コード側の既定（assets/stepdown.enemies.json と同じ値から出発する）。
    def defaultStats(kind: Enemy.Kind): Stats = match kind {
        case Enemy.Kind.Jelly  => { name = "ゼリー", hp = 4, attack = 2, exp = 3, dropChance = 0.25,
                                    drops = Item.Kind.Bread :: Item.Kind.Bread :: Item.Kind.Pebble :: Item.Kind.Herb :: Nil | plain() }
        case Enemy.Kind.Bat    => { name = "コウモリ", hp = 3, attack = 1, exp = 3, dropChance = 0.2,
                                    drops = Item.Kind.Pebble :: Item.Kind.Bread :: Nil | plain() }
        case Enemy.Kind.Archer => { name = "弓兵", hp = 4, attack = 2, exp = 5, dropChance = 0.35,
                                    drops = Item.Kind.WoodArrow :: Item.Kind.WoodArrow :: Item.Kind.Smoke :: Nil | plain() }
        case Enemy.Kind.Swarm  => { name = "群れ", hp = 3, attack = 1, exp = 1, dropChance = 0.1,
                                    drops = Item.Kind.Bread :: Nil | plain() }
        case Enemy.Kind.Shield => { name = "盾持ち", hp = 8, attack = 3, exp = 7, dropChance = 0.3,
                                    drops = Item.Kind.Herb :: Item.Kind.Bread :: Nil | plain() }
        case Enemy.Kind.Charger => { name = "突進", hp = 7, attack = 4, exp = 6, dropChance = 0.3,
                                     drops = Item.Kind.Bread :: Item.Kind.Herb :: Nil, chargeMin = 2, chargeMax = 5 | plain() }
        case Enemy.Kind.Bomber => { name = "自爆虫", hp = 4, attack = 0, exp = 5, dropChance = 0.25,
                                    drops = Item.Kind.Smoke :: Item.Kind.Bread :: Nil, blastDamage = 6 | plain() }
    }

    /// 使わない欄を 0 にした強さ（種類ごとの既定はこれに欄を書き足す）。
    def plain(): Stats = { name = "", hp = 1, attack = 0, exp = 0, dropChance = 0.0, drops = Nil, chargeMin = 0, chargeMax = 0, blastDamage = 0 }

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
          attack = JsonCodec.intOr("attack", d#attack, obj),
          exp = JsonCodec.intOr("exp", d#exp, obj), dropChance = JsonCodec.floatOr("dropChance", d#dropChance, obj),
          drops = Map.get("drops", obj) |> Option.flatMap(JsonCodec.decodeList(JsonCodec.expectString))
              |> Option.map(List.filterMap(Item.fromName)) |> Option.getWithDefault(d#drops),
          chargeMin = JsonCodec.intOr("chargeMin", d#chargeMin, obj), chargeMax = JsonCodec.intOr("chargeMax", d#chargeMax, obj),
          blastDamage = JsonCodec.intOr("blastDamage", d#blastDamage, obj) }

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
