// SceneRender — ゲーム画面を画面なしで PNG に生成する（スナップショット比較用）。
// 3D の場面と後処理の Pass は SoftRaster が CPU で塗る（実機の GPU と同じ式）。
// 実行: make render-all → gallery/<場面>.png
//       make render SHOT=play → debug/play.png（1 枚だけ）
mod SceneRender {
    use fge::{App, FontAtlas, GameEngine, GpuHandle, HeadlessFont, HeadlessLoad, HeadlessRender, Render, Scene3d, Vec2}

    import java.io.File

    pub def design(): Vec2.Vec2 = { x = World.width(), y = World.height() }

    pub def ttf(): String = "assets/ShipporiMincho-Regular.subset.ttf"

    pub def cfgOf(outDir: String, atlas: FontAtlas, res: Res.Res): HeadlessRender.RenderConfig =
        { design = design(), scale = 1, background = ThemeDoc.rec(res#theme)#bg,
          fontTtf = ttf(), fontAtlas = atlas,
          outDir = outDir, frameW = 540, frameH = 960,
          texturePath = Art.all() |> List.map(pic -> (pic#name, pic#path)) |> List.toMap,
          shapeAntialias = true | HeadlessRender.defaults() }

    /// 絵の名前 → 寸法（Render.drawWith と Pass の計画が UV を解くのに読む）。
    def textureInfo(name: String): Option[GameEngine.TextureInfo] =
        Art.all() |> List.find(pic -> pic#name == name)
            |> Option.map(pic -> { id = GpuHandle.wrapTextureId(0), width = pic#width, height = pic#height })

    /// 描く元（実機の起動と同じ Controls.loadAll から組む）。
    def loadRes(): Res.Res \ IO =
        Res.build(run { HeadlessLoad.handle(Controls.loadAll) } with Fs.FileRead.runWithIO)

    // ── 決定的な場面（スナップショット） ──

    pub def all(): Unit \ IO = App.Headless.exitWith(allJob(App.Headless.requestFromProcess()))

    /// SceneRender.all の中身（常駐の /run からも呼ぶ）。戻り値は終了コード。
    def allJob(_req: App.Headless.Request): Int32 \ IO =
        discard allShots();
        0

    /// 常駐（App.Headless.serveJobs）が /run で流す入口の束。
    pub def jobs(): List[App.Headless.Job] =
        App.Headless.job("SceneRender", "one", req -> oneJob(req)) ::
        App.Headless.job("SceneRender", "all", req -> allJob(req)) :: Nil

    /// SceneRender.all の中身。生成した PNG のパス列を返す — 走行中のゲームへの POST /render
    /// （Main の App.onRenderRequest）もこれを呼ぶ。プレイ状態には触れず Docs から組み立てる。
    pub def allShots(): List[String] \ IO =
        shotsInto("gallery", _ -> true)

    /// SHOT で指した場面だけを debug/ へ生成する（スナップショットを汚さない）。
    pub def one(): Unit \ IO = App.Headless.exitWith(oneJob(App.Headless.requestFromProcess()))

    /// SceneRender.one の中身（常駐の /run からも呼ぶ）。env は要求の表（App.Headless.envOf）だけを読む。
    def oneJob(req: App.Headless.Request): Int32 \ IO =
        let wanted = Map.getWithDefault("SHOT", "", App.Headless.envOf(req)) |> String.splitOn(substr = " ")
            |> List.map(String.trim) |> List.filter(n -> not String.isEmpty(n));
        let known = List.toSet(Shots.names());
        let unknown = wanted |> List.filter(n -> not Set.memberOf(n, known));
        if (List.isEmpty(wanted) or not List.isEmpty(unknown)) {
            println("[render] 描けない名前です: ${List.join(" ", unknown)}");
            println("[render] SHOT=<場面> に使えるのは: ${List.join(" ", Shots.names())}");
            1
        } else {
            let picked = List.toSet(wanted);
            discard shotsInto("debug", n -> Set.memberOf(n, picked));
            0
        }

    /// keep が真を返す場面だけを outDir へ生成し、そのファイルパス列を返す。
    def shotsInto(outDir: String, keep: String -> Bool): List[String] \ IO =
        discard (new File(outDir)).mkdirs();
        let atlas = HeadlessFont.buildUiAtlas(ttf(), ttf());
        let res = loadRes();
        let cfg = cfgOf(outDir, atlas, res);
        let ctx = App.viewCtxOf(atlas, design());
        Shots.names() |> List.filter(keep) |> List.filterMap(name ->
            Shots.worldOf(name, res) |> Option.map(w -> shoot(cfg, ctx, res, w, name)))

    /// 1 場面を PNG に生成し、生成したファイルパスを返す。Pass の計画（3D の場面・ブルーム・後処理）も実機と同じ物を描く。
    def shoot(cfg: HeadlessRender.RenderConfig, ctx: App.ViewCtx, res: Res.Res, w: World.World, name: String): String \ IO =
        let input = View.inputOf(res, w);
        let counts = SceneView.countsOf(design(), input#scene);
        println("[render] ${name}: 置いた物 ${counts#instances}・板 ${counts#billboards}（視錐台の中で描くのは ${counts#drawn}）・点光源 ${counts#lights}（画面に掛かる ${counts#lightsInView}・計算は ${Scene3d.maxPointLights()} 本まで）");
        let prepared = Render.passSpecsOf(textureInfo, View.passPlanOf(input));
        prepared#issues |> List.forEach(i -> println("[render] Pass の計画: ${i}"));
        let items = View.frameOf(ctx, input);
        let surfaces = Render.shaderSurfaces(items);
        let (drawables, polygons) = Render.drawWith(textureInfo, Render.withoutShaders(items));
        HeadlessRender.renderPngWithPasses(cfg, prepared#specs, drawables, polygons, surfaces, name);
        App.Headless.writeShotRecord(cfg#outDir, name, Some(E2e.fingerprint(res, w)));
        let path = "${cfg#outDir}/${name}.png";
        println("rendered: ${path}");
        path
}
