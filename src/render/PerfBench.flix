// PerfBench — 遊びの場面 1 コマの CPU の仕事（系・Pass の計画・本編の面・engine へ渡す前の写し）を窓なしで何度も回し、1 コマの平均の ms を出す。
// 先に起動の Res.build（地図ごとの形をつなぐ）の ms も出す。GL の送り・GPU の塗りは入らない（ブラウザの fps=1 の数とは同じ物差しではない。前後の比べに使う）。
// 実行: $ENGINE/bin/headless-run PerfBench.main（SHOT=<場面> で場面・PART=step|input|plan|specs|frame で一部だけ。既定は play・all）
mod PerfBench {
    use fge::{App, GameEngine, GpuHandle, HeadlessFont, HeadlessLoad, Render, Vec2}

    import java.lang.System

    def design(): Vec2.Vec2 = { x = World.width(), y = World.height() }

    def textureInfo(name: String): Option[GameEngine.TextureInfo] =
        Art.all() |> List.find(pic -> pic#name == name)
            |> Option.map(pic -> { id = GpuHandle.wrapTextureId(0), width = pic#width, height = pic#height })

    def idleFrame(): App.Frame =
        App.frameOf({ prev = Set.empty() }, { current = Set.empty() }, 1.0 / 60.0, design(),
                    { prevCursor = None, cursor = Vec2.zero() }, { prevDown = false, down = false, wheel = 0.0 })

    /// 1 コマの仕事: 系を 1 回・Pass の計画と本編の面を組み、engine の描く前の写し（passSpecsOf・drawWith）まで通す。
    def oneFrame(part: String, ctx: App.ViewCtx, res: Res.Res, w: World.World): (World.World, Int32) =
        let next = Controls.step(idleFrame(), res, w);
        match part {
            case "step" => (next, 1)
            case "input" => (next, List.length(View.inputOf(res, next)#scene#figures))
            case "plan" => (next, List.length(View.passPlan(res, next)#passes))
            case "specs" => (next, List.length(Render.passSpecsOf(textureInfo, View.passPlan(res, next))#specs))
            case "frame" => (next, List.length(View.frame(ctx, res, next)))
            case _ =>
                let specs = Render.passSpecsOf(textureInfo, View.passPlan(res, next));
                let items = View.frame(ctx, res, next);
                let (drawables, polygons) = Render.drawWith(textureInfo, Render.withoutShaders(items));
                (next, List.length(specs#specs) + List.length(drawables) + List.length(polygons))
        }

    /// n コマ回した (かかった ns の合計, 描く物の数の合計)。WhyNot: 時計を外で 2 回読むだけにしない — 純粋な仕事は時計の読みの外へ動かされる。
    def loop(part: String, ctx: App.ViewCtx, res: Res.Res, n: Int32, w: World.World, acc: (Int64, Int32)): (Int64, Int32) \ IO =
        if (n <= 0) acc
        else {
            let t0 = System.nanoTime();
            let (next, k) = oneFrame(part, ctx, res, w);
            let t1 = System.nanoTime() + Int32.toInt64(k * 0);
            loop(part, ctx, res, n - 1, next, (fst(acc) + t1 - t0, snd(acc) + k))
        }

    pub def main(): Unit \ IO =
        let env = App.Headless.envOf(App.Headless.requestFromProcess());
        let name = Map.getWithDefault("SHOT", "play", env);
        let part = Map.getWithDefault("PART", "all", env);
        let docs = run { HeadlessLoad.handle(Controls.loadAll) } with Fs.FileRead.runWithIO;
        let t0 = System.nanoTime();
        let res = Res.build(docs);
        let t1 = System.nanoTime() + Int32.toInt64(Map.size(res#meshes) * 0);
        println("[bench] 起動の Res.build ${Int64.toFloat64(t1 - t0) / 1000000.0} ms（地図 ${Map.size(res#meshes)} 枚の形をつなぐ）");
        let atlas = HeadlessFont.buildUiAtlas("assets/ShipporiMincho-Regular.subset.ttf", "assets/ShipporiMincho-Regular.subset.ttf");
        let ctx = App.viewCtxOf(atlas, design());
        match Shots.worldOf(name, res) {
            case None => println("[bench] 場面が無い: ${name}")
            case Some(w) =>
                let (_, warm) = loop(part, ctx, res, 200, w, (0i64, 0));
                let rounds = 600;
                let (ns, k) = loop(part, ctx, res, rounds, w, (0i64, 0));
                let ms = Int64.toFloat64(ns) / 1000000.0 / Int32.toFloat64(rounds);
                println("[bench] ${name}: 1 コマ ${ms} ms（${rounds} コマの平均・描く物 ${k / rounds}・空回し ${warm / 200}）")
        }
}
