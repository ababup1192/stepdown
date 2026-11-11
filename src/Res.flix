// Res — 描く元の束: Doc と、Doc から作る物（階ごとの盤と、地図ごとにつないだ 3D の形）。読むだけで、Doc の読み直しの時だけ替わる。
// 遊びの状態は World が持ち、App の w は Resources.With[Res, World] の組（Game.app）。
// Res は World を使わない（使う向きは World → Res だけ。lint/rules.flix の module-cycle）。
mod Res {

    use fge::{AudioMixer, Log}

    /// 起動時と描き出しが同じ形で読む Doc の束。
    pub type alias Docs = {
        rules = RulesDoc.Doc,
        items = ItemsDoc.Doc,
        enemies = EnemiesDoc.Doc,
        floors = FloorsDoc.Doc,
        look = LookDoc.Doc,
        text = TextDoc.Doc,
        keys = KeysDoc.Doc,
        theme = ThemeDoc.Doc,
        mixer = AudioMixer.Mixer,
        kit = Kit.Kit
    }

    /// 描く元の束。layouts は floors の Doc から組んだ階ごとの盤、meshes は (階, 地図の番号) → つないだ形。
    pub type alias Res = {
        rules = RulesDoc.Doc,
        items = ItemsDoc.Doc,
        enemies = EnemiesDoc.Doc,
        floors = FloorsDoc.Doc,
        look = LookDoc.Doc,
        text = TextDoc.Doc,
        keys = KeysDoc.Doc,
        theme = ThemeDoc.Doc,
        mixer = AudioMixer.Mixer,
        layouts = Floor.Layouts,
        kit = Kit.Kit,
        meshes = Map[(Int32, Int32), FloorMesh.Built]
    }

    /// Doc の束から描く元を組む（盤もここで組む。組めない地図は 1 行ずつ言って飛ばす）。
    pub def build(docs: Docs): Res =
        let layouts = layoutsOf(docs#floors);
        { rules = docs#rules, items = docs#items, enemies = docs#enemies, floors = docs#floors, look = docs#look,
          text = docs#text, keys = docs#keys, theme = docs#theme, mixer = docs#mixer, layouts = layouts,
          kit = docs#kit, meshes = meshesOf(docs#kit, layouts) }

    /// 地図ごとに、動かない物を材質ごとに 1 つの形へつなぐ。
    def meshesOf(kit: Kit.Kit, layouts: Floor.Layouts): Map[(Int32, Int32), FloorMesh.Built] =
        Map.toList(layouts) |> List.flatMap(match (floor, boards) ->
            Vector.toList(boards) |> List.zipWithIndex |> List.map(match (layout, b) ->
                ((floor, layout), FloorMesh.build(kit, Rng.mix(Int32.toInt64(floor), Int32.toInt64(layout) + 1i64), b))))
            |> List.toMap

    /// 階 floor の layout 番目の地図のつないだ形（無ければ空）。
    pub def floorAt(res: Res, floor: Int32, layout: Int32): FloorMesh.Built =
        Map.get((floor, layout), res#meshes) |> Option.getWithDefault(FloorMesh.empty())

    def layoutsOf(floors: FloorsDoc.Doc): Floor.Layouts =
        let built = Floor.layoutsOf(floors);
        built#layouts |> Log.warnAllThen(built#problems)

    /// 階を作るのに読む Doc。
    pub def floorDocs(res: Res): Floor.Docs =
        { rules = res#rules, items = res#items, enemies = res#enemies, floors = res#floors }

    /// run の今の階の判定に使う束。
    pub def ctxOf(res: Res, runState: Run.State): Run.Ctx = Floor.ctxOf(floorDocs(res), res#layouts, runState)

    // ── Doc の差し替え（保存即反映。Controls.docTable の reload） ──

    pub def withRules(rules: RulesDoc.Doc, res: Res): Res = { rules = rules | res }

    pub def withItems(items: ItemsDoc.Doc, res: Res): Res = { items = items | res }

    pub def withEnemies(enemies: EnemiesDoc.Doc, res: Res): Res = { enemies = enemies | res }

    pub def withFloors(floors: FloorsDoc.Doc, res: Res): Res =
        let layouts = layoutsOf(floors);
        { floors = floors, layouts = layouts, meshes = meshesOf(res#kit, layouts) | res }

    pub def withLook(look: LookDoc.Doc, res: Res): Res = { look = look | res }

    pub def withText(text: TextDoc.Doc, res: Res): Res = { text = text | res }

    pub def withKeys(keys: KeysDoc.Doc, res: Res): Res = { keys = keys | res }

    pub def withTheme(theme: ThemeDoc.Doc, res: Res): Res = { theme = theme | res }

    pub def withMixer(mixer: AudioMixer.Mixer, res: Res): Res = { mixer = mixer | res }
}
