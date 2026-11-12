// ItemsDoc — stepdown.items.json（アイテムの段ごとの数値: 名前・攻撃・守り・弾く確率・ダメージ・貫通・燃える・範囲・眠り・回復・満腹度・拾う数）の Doc 層。
// 段の並びと効果の種類はコード（Item）。キーは Item.Kind の名前で、Doc で段は足せない。
// fail-open — 読めない Doc・書いていない段・書いていない欄は defaults() の値へ落とす。
mod ItemsDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 1 段の数値。その系統で使わない欄は 0。
    ///   attack  … 剣の攻撃（素の攻撃と置き換える）
    ///   guard   … 盾が受ける攻撃から引く数
    ///   deflect … 盾が攻撃を弾く確率（0..1）
    ///   damage  … 矢・小石・爆弾のダメージ
    ///   range   … 小石が届く最長の距離（矢は rules の arrowMax）
    ///   pierce  … 矢が貫いて当たる追加の敵の数
    ///   burn    … 矢が当たった敵の燃える手の数
    ///   radius  … 爆弾の当たる範囲（主人公から縦横の差がこの数以内）
    ///   sleep   … 眠り玉が当たった敵・煙玉で隣の敵が眠る手の数
    ///   heal    … 薬の回復量
    ///   food    … 食べ物で戻る満腹度
    ///   count   … 床から拾った時に増える数（矢・小石・爆弾・煙玉・薬）
    ///   cap     … 持てる上限（同じ系統の上の段を拾った時は上の段の上限）
    pub type alias Stats = {
        name = String,
        attack = Int32,
        guard = Int32,
        deflect = Float64,
        damage = Int32,
        range = Int32,
        pierce = Int32,
        burn = Int32,
        radius = Int32,
        sleep = Int32,
        heal = Int32,
        food = Int32,
        count = Int32,
        cap = Int32
    }

    pub type alias Rec = { version = Int32, stats = Map[Item.Kind, Stats] }

    pub enum Doc(Rec)

    def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.items.json"

    def zero(name: String): Stats =
        { name = name, attack = 0, guard = 0, deflect = 0.0, damage = 0, range = 0, pierce = 0, burn = 0, radius = 0, sleep = 0, heal = 0, food = 0, count = 0, cap = 0 }

    /// コード側の既定（assets/stepdown.items.json と同じ値から出発する）。
    def defaultStats(kind: Item.Kind): Stats = match kind {
        case Item.Kind.WoodSword    => { attack = 3 | zero("木の剣") }
        case Item.Kind.IronSword    => { attack = 5 | zero("鉄の剣") }
        case Item.Kind.SilverSword  => { attack = 7 | zero("銀の剣") }
        case Item.Kind.WoodShield   => { guard = 1 | zero("木の盾") }
        case Item.Kind.IronShield   => { guard = 2 | zero("鉄の盾") }
        case Item.Kind.SilverShield => { guard = 3, deflect = 0.25 | zero("銀の盾") }
        case Item.Kind.WoodArrow    => { damage = 3, count = 5, cap = 15 | zero("木の矢") }
        case Item.Kind.IronArrow    => { damage = 4, pierce = 1, count = 5, cap = 15 | zero("鉄の矢") }
        case Item.Kind.SilverArrow  => { damage = 5, pierce = 2, count = 5, cap = 15 | zero("銀の矢") }
        case Item.Kind.FireArrow    => { damage = 5, burn = 3, count = 3, cap = 15 | zero("火の矢") }
        case Item.Kind.SmallBomb    => { damage = 6, radius = 1, count = 2, cap = 5 | zero("小爆弾") }
        case Item.Kind.BigBomb      => { damage = 8, radius = 2, count = 2, cap = 5 | zero("大爆弾") }
        case Item.Kind.SleepBomb    => { radius = 2, sleep = 3, count = 2, cap = 5 | zero("眠り玉") }
        case Item.Kind.Herb         => { heal = 6, count = 1, cap = 3 | zero("薬草") }
        case Item.Kind.Tonic        => { heal = 12, count = 1, cap = 3 | zero("回復薬") }
        case Item.Kind.Elixir       => { heal = 99, count = 1, cap = 3 | zero("万能薬") }
        case Item.Kind.Bread        => { food = 30 | zero("パン") }
        case Item.Kind.Meat         => { food = 60 | zero("肉") }
        case Item.Kind.Pebble       => { damage = 1, range = 3, count = 6, cap = 12 | zero("小石") }
        case Item.Kind.Smoke        => { sleep = 2, count = 2, cap = 3 | zero("煙玉") }
        case Item.Kind.IronBall     => { damage = 3, range = 3, count = 2, cap = 4 | zero("鉄球") }
    }

    pub def defaults(): Doc =
        Doc.Doc({ version = 1, stats = Item.all() |> List.map(kind -> (kind, defaultStats(kind))) |> List.toMap })

    /// 段の数値（Doc に無い段は既定）。
    pub def statsOf(kind: Item.Kind, d: Doc): Stats =
        Map.get(kind, rec(d)#stats) |> Option.getWithDefault(defaultStats(kind))

    /// 段の名前。
    pub def nameOf(kind: Item.Kind, d: Doc): String = statsOf(kind, d)#name

    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let items = Map.get("items", root) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        let objOf = kind -> Map.get("${kind}", items) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        Doc.Doc({ version = JsonCodec.intOr("version", 1, root),
                  stats = Item.all() |> List.map(kind -> (kind, statsFrom(defaultStats(kind), objOf(kind)))) |> List.toMap })

    def statsFrom(d: Stats, obj: Map[String, Json]): Stats =
        let int = (key, fallback) -> JsonCodec.intOr(key, fallback, obj);
        { name = JsonCodec.stringOr("name", d#name, obj), attack = int("attack", d#attack), guard = int("guard", d#guard),
          deflect = JsonCodec.floatOr("deflect", d#deflect, obj), damage = int("damage", d#damage), range = int("range", d#range),
          pierce = int("pierce", d#pierce), burn = int("burn", d#burn), radius = int("radius", d#radius),
          sleep = int("sleep", d#sleep), heal = int("heal", d#heal), food = int("food", d#food), count = int("count", d#count), cap = int("cap", d#cap) }

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
