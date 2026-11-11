// Kit — ダンジョンの部品の束（modeling/kit.py の書き出し assets/models/kit.glb）: ノードの名前で引く。
// 読み込みだけが IO。読めない時は理由を 1 行出して空の束で続ける（その部品は描かれない）。
mod Kit {
    use fge::{Gltf, ImageData, Log, Mesh3d, Model3d, Scene3d, Texture3d, Vec3}

    /// 部品の形 1 つ: 形・材質（None は頂点の色）・材質の名前（つなぐ時の組の鍵）。
    pub type alias Part = { mesh = Mesh3d.Mesh3d, material = Option[Scene3d.Material3d], key = String }

    /// 部品 1 つ: 書き出しの原点と形の列。
    pub type alias Item = { origin = Vec3.Vec3, parts = List[Part] }

    pub type alias Kit = { items = Map[String, Item], textures = List[Texture3d.Texture3d] }

    pub def empty(): Kit = { items = Map.empty(), textures = Nil }

    pub def path(): String = "assets/models/kit.glb"

    pub def load(dec: ImageData.ImageDecoder, p: String): Kit \ Fs.FileRead + IO =
        match Gltf.loadTexturedUsing(dec, Gltf.noTextureLimits(), p) {
            case Err(why)  => Log.warnThen("[kit] ${p}: ${why}", empty())
            case Ok(model) => ofModel(model)
        }

    def ofModel(model: Model3d.Model3d): Kit =
        let items = model#nodes |> List.filter(n -> not List.isEmpty(n#parts)) |> List.map(n ->
            (n#name, { origin = Model3d.nodeOrigin(n),
                       parts = Model3d.partsOfNode(n, model) |> List.map(p ->
                           { mesh = p#mesh, material = p#material,
                             key = p#gltfMaterial |> Option.map(g -> g#name) |> Option.getWithDefault("${n#name}") }) }));
        { items = List.toMap(items), textures = model#textures }

    /// 名前の部品（無い名前は None）。
    pub def itemOf(name: String, kit: Kit): Option[Item] = Map.get(name, kit#items)
}
