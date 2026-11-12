// FloorsDoc — stepdown.floors.json（地図の文字と、階ごとに使う地図・敵の種類ごとの数・アイテム）の Doc 層。
// 地図の字の意味は Board.glyphOf、置き場を選ぶ決まりは Floor。
// fail-open — 読めない Doc は defaults()（1F の地図 1 枚）。読めない階・知らないアイテムの名前はその分だけ飛ばす。
mod FloorsDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 地図 1 枚。rows は文字の行。
    pub type alias MapDef = { title = String, rows = List[String] }

    /// 階 1 つ。maps は使う地図の名前（乱数で 1 つ選ぶ）。enemies は種類 → 数（群れだけは 3 体の組の数）、items は床に置くアイテム。
    /// breadOdds はパンを 0 個・1 個・2 個…置く重み（足して 1 でなくてよい）、meatChance は肉を 1 つ置く確率。
    pub type alias FloorDef = {
        floor = Int32,
        maps = List[String],
        enemies = Map[Enemy.Kind, Int32],
        items = List[Item.Kind],
        breadOdds = List[Float64],
        meatChance = Float64
    }

    pub type alias Rec = { version = Int32, maps = Map[String, MapDef], floors = List[FloorDef] }

    pub enum Doc(Rec)

    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.floors.json"

    /// コード側の既定（assets の 1F の 1 枚目と同じ地図）。
    pub def defaults(): Doc =
        Doc.Doc({ version = 1,
                  maps = Map#{ "1a" => { title = "1F 四つの部屋", rows = defaultRows() } },
                  floors = { floor = 1, maps = "1a" :: Nil, enemies = Map#{Enemy.Kind.Jelly => 4, Enemy.Kind.Bat => 2, Enemy.Kind.Archer => 1},
                             items = Item.Kind.WoodArrow :: Item.Kind.SmallBomb :: Item.Kind.Herb :: Item.Kind.IronSword :: Nil,
                             breadOdds = defaultBreadOdds(), meatChance = 0.15 } :: Nil })

    def defaultBreadOdds(): List[Float64] = 0.15 :: 0.7 :: 0.15 :: Nil

    def defaultRows(): List[String] =
        "###t#######t###" :: "#.....###.....#" :: "#..r..,,,..P..#" :: "#..@..###.....#" :: "#.....###..P..#" ::
        "##,######.....#" :: "##,######.....#" :: "##,#######,####" :: "##,,,,,,,,,####" :: "######,########" ::
        "##t###,###t####" :: "##.....~~...###" :: "##...~~~~~..###" :: "#t..r.~~~...,,#" :: "##.....~....#,#" ::
        "##..........#,#" :: "####,########,#" :: "####,########,#" :: "####,,,,,,,,,,#" :: "########,######" ::
        "###t####,###t##" :: "#.............#" :: "#..P.......P..#" :: "#.............#" :: "t......r......#" ::
        "#..P.......P..#" :: "#.............#" :: "#.>...........#" :: "#######t#######" :: Nil

    /// n 階の決まり（無ければ None）。
    pub def floorOf(n: Int32, d: Doc): Option[FloorDef] = rec(d)#floors |> List.find(f -> f#floor == n)

    /// 階に置く敵の数（群れは組の数。書いていない種類は 0）。
    pub def enemyCount(kind: Enemy.Kind, f: FloorDef): Int32 = Map.getWithDefault(kind, 0, f#enemies)

    /// 階に置く敵の体の数（群れは 1 組 3 体）。
    pub def bodyCount(f: FloorDef): Int32 =
        Map.foldLeftWithKey((acc, kind, n) -> acc + (if (kind == Enemy.Kind.Swarm) n * 3 else n), 0, f#enemies)

    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        let d = rec(defaults());
        let maps = Map.get("maps", root) |> Option.flatMap(JsonCodec.expectObject)
            |> Option.map(Map.filterMap(json -> JsonCodec.expectObject(json) |> Option.flatMap(mapOf)))
            |> Option.filter(m -> not Map.isEmpty(m)) |> Option.getWithDefault(d#maps);
        let floors = Map.get("floors", root) |> Option.flatMap(JsonCodec.decodeList(j -> Some(j)))
            |> Option.map(List.filterMap(json -> JsonCodec.expectObject(json) |> Option.flatMap(floorDefOf)))
            |> Option.filter(fs -> not List.isEmpty(fs)) |> Option.getWithDefault(d#floors);
        Doc.Doc({ version = JsonCodec.intOr("version", d#version, root), maps = maps, floors = floors })

    def mapOf(obj: Map[String, Json]): Option[MapDef] =
        Map.get("rows", obj) |> Option.flatMap(JsonCodec.decodeList(JsonCodec.expectString))
            |> Option.map(rows -> { title = JsonCodec.stringOr("title", "", obj), rows = rows })

    def floorDefOf(obj: Map[String, Json]): Option[FloorDef] =
        let names = key -> Map.get(key, obj) |> Option.flatMap(JsonCodec.decodeList(JsonCodec.expectString)) |> Option.getWithDefault(Nil);
        let counts = Map.get("enemies", obj) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        let enemies = Enemy.all() |> List.map(kind -> (kind, JsonCodec.intOr("${kind}", 0, counts))) |> List.filter(e -> snd(e) > 0) |> List.toMap;
        let odds = Map.get("breadOdds", obj) |> Option.flatMap(JsonCodec.decodeList(JsonCodec.expectFloat)) |> Option.getWithDefault(defaultBreadOdds());
        Map.get("floor", obj) |> Option.flatMap(JsonCodec.expectInt) |> Option.map(n ->
            { floor = n, maps = names("maps"), enemies = enemies, items = names("items") |> List.filterMap(Item.fromName),
              breadOdds = odds, meatChance = JsonCodec.floatOr("meatChance", 0.15, obj) })

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
