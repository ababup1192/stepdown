// FloorsDoc — stepdown.floors.json（地図の文字と、階ごとに使う地図・敵の数・アイテム）の Doc 層。
// 地図の字の意味は Board.glyphOf、置き場を選ぶ決まりは Floor。
// fail-open — 読めない Doc は defaults()（1F の地図 1 枚）。読めない階・知らないアイテムの名前はその分だけ飛ばす。
mod FloorsDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 地図 1 枚。rows は文字の行。
    pub type alias MapDef = { title = String, rows = List[String] }

    /// 階 1 つ。maps は使う地図の名前（乱数で 1 つ選ぶ）。jelly…swarm は敵の数、items は床に置くアイテム。
    pub type alias FloorDef = {
        floor = Int32,
        maps = List[String],
        jelly = Int32,
        bat = Int32,
        archer = Int32,
        swarm = Int32,
        items = List[Item.Kind]
    }

    pub type alias Rec = { version = Int32, maps = Map[String, MapDef], floors = List[FloorDef] }

    pub enum Doc(Rec)

    pub def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.floors.json"

    /// コード側の既定（assets の 1F の 1 枚目と同じ地図）。
    pub def defaults(): Doc =
        Doc.Doc({ version = 1,
                  maps = Map#{ "1a" => { title = "1F 噴水の広間", rows = defaultRows() } },
                  floors = { floor = 1, maps = "1a" :: Nil, jelly = 2, bat = 1, archer = 1, swarm = 0,
                             items = Item.Kind.WoodArrow :: Item.Kind.SmallBomb :: Item.Kind.Herb :: Item.Kind.IronArrow :: Item.Kind.IronSword :: Nil } :: Nil })

    def defaultRows(): List[String] =
        "#t###t#########" :: "#.....#########" :: "#..r..,,,######" :: "#..@..##,######" :: "########,######" ::
        "#######..######" :: "#######..######" :: "#####......####" :: "####........###" :: "###..P....~~.##" ::
        "##.......~~~~t#" :: "##..r....~~~.##" :: "##....P...~..##" :: "#t...........##" :: "###..>....P..##" ::
        "##...........##" :: "####.......####" :: "#####.....#####" :: "######...######" :: "######...######" ::
        "#######t#######" :: Nil

    /// n 階の決まり（無ければ None）。
    pub def floorOf(n: Int32, d: Doc): Option[FloorDef] = rec(d)#floors |> List.find(f -> f#floor == n)

    /// 階に置く敵の数。
    pub def enemyCount(kind: Enemy.Kind, f: FloorDef): Int32 = match kind {
        case Enemy.Kind.Jelly  => f#jelly
        case Enemy.Kind.Bat    => f#bat
        case Enemy.Kind.Archer => f#archer
        case Enemy.Kind.Swarm  => f#swarm
    }

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
        let count = key -> JsonCodec.intOr(key, 0, obj);
        Map.get("floor", obj) |> Option.flatMap(JsonCodec.expectInt) |> Option.map(n ->
            { floor = n, maps = names("maps"), jelly = count("jelly"), bat = count("bat"), archer = count("archer"),
              swarm = count("swarm"), items = names("items") |> List.filterMap(Item.fromName) })

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
