// VideoRender — 紹介の動画のコマを、画面なしで書く。
// 実行: make video FLOW=e2e/showcase.flow → debug/video/showcase.mp4（GPU で塗れた時は ffmpeg へ直に流して mp4 を書く。
//       SoftRaster に落ちた時はコマを debug/video/showcase/f0000.png … に書き、make が mp4 へ束ねる）
//
// 手順は e2e/tour.flow（e2e と同じ flow の実行器）で、キーを実機と同じ 1 フレームずつ World に流す。撮るのは mark in_<名前> から
// out_<名前> までの区間だけで、並んだ順につなぐ。手順は 60 fps で流し、VIDEO_FPS ごとに間引いて描く。
// 音は撮る区間のフレームごとの音の宣言を実機と同じミキサーで混ぜ、debug/video/<名前>.wav に書く（make が mp4 へ重ねる）。
// コマは 540×960（design のまま）で描く。周回の種は World.fixedSeed。
//
// 環境変数: VIDEO_FPS（既定 30。60 を割り切る数）・VIDEO_FLOW（手順。既定 e2e/tour.flow）・
//           VIDEO_OUT（コマを書く debug/video/<名前>。既定 showcase）・VIDEO_X264（GPU の道で mp4 を書く ffmpeg の旗）・
//           FGE_GPU / FGE_VIDEO_PAINT / FFMPEG（VideoOut.settingsOf）
mod VideoRender {
    use fge::{App, AudioMixdown, Flow, FontAtlas, HeadlessFont, HeadlessLoad, Render, Resources, SfxSynth, Sort, VideoOut}

    import java.io.File
    import java.nio.file.{Files, Paths}

    /// App の w（描く元と World の組）。
    type alias AppState = Resources.With[Res.Res, World.World]

    /// 撮る手順と、コマを書く所（debug/video/<名前>）。
    type alias Take = { flow = String, outDir = String }

    def takeOf(env: Map[String, String]): Take =
        let pick = (key, fallback) -> Map.get(key, env) |> Option.map(String.trim) |> Option.filter(v -> not String.isEmpty(v)) |> Option.getWithDefault(fallback);
        { flow = pick("VIDEO_FLOW", "e2e/showcase.flow"), outDir = "debug/video/${pick("VIDEO_OUT", "showcase")}" }

    /// 手順を流す速さ（flow の既定の fps と同じ）。
    def playFps(): Int32 = 60

    pub def frames(): Unit \ IO = App.Headless.exitWith(framesJob(App.Headless.requestFromProcess()))

    /// 常駐（App.Headless.serveJobs）が /run で流す入口の束。
    pub def jobs(): List[App.Headless.Job] =
        App.Headless.job("VideoRender", "frames", req -> framesJob(req)) :: Nil

    def framesJob(req: App.Headless.Request): Int32 \ IO =
        let env = App.Headless.envOf(req);
        // GL の文脈は AWT（字のアトラス）より先に繋ぐ（VideoOut.settingsOf の doc）。
        let settings = VideoOut.settingsOf(env);
        let fps = Map.get("VIDEO_FPS", env) |> Option.flatMap(Int32.fromString)
            |> Option.filter(n -> n > 0 and Int32.modulo(playFps(), n) == 0) |> Option.getWithDefault(30);
        let app = run { Game.app(HeadlessLoad.handle(Controls.loadAll), World.fixedSeed()) } with Fs.FileRead.runWithIO;
        let fresh = run { App.freshWorld(app, SceneRender.design()) } with Fs.FileRead.runWithIO;
        let take = takeOf(env);
        discard (new File(take#outDir)).mkdirs();
        match playFlow(app, fresh, take#flow, Files.readString(Paths.get(take#flow))) {
            case Err(why) =>
                println("[video] ${why}");
                1
            case Ok(outcome) => writeOutcome(settings, env, fps, take, outcome)
        }

    /// 手順（flow の文）を読んで、まっさらな World から流す。読めない・途中で止まった時は、その訳の 1 行。
    def playFlow(app: App.App[AppState, {}], fresh: AppState, flowPath: String, text: String): Result[String, Flow.Outcome[AppState]] \ IO =
        Flow.Parse.parseText(Flow.vocabOf(app, fresh), flowPath, text)
            |> Result.mapErr(problems -> "手順を読めない\n" + Flow.Parse.problemsText(problems))
            |> Result.flatMap(script -> {
                let played = run {
                    Flow.play(app, { design = SceneRender.design(), limit = 100000 }, script, { world = fresh, state = App.freshStepState() })
                } with Fs.FileRead.runWithIO;
                played |> Result.mapErr(failure -> "手順が ${failure#at#number} 行目で止まった: ${failure#why}（${failure#at#text}）/ ${failure#seen}")
            })

    def writeOutcome(settings: VideoOut.Settings, env: Map[String, String], fps: Int32, take: Take, outcome: Flow.Outcome[AppState]): Int32 \ IO =
        match segmentsOf(outcome#marks) {
            case Err(why) =>
                println("[video] ${take#flow} の mark: ${why}");
                1
            case Ok(segments) =>
                let shown = segments |> List.flatMap(seg -> outcome#frames |> List.drop(fst(seg)) |> List.take(snd(seg) - fst(seg)));
                let stride = playFps() / fps;
                let painted = shown |> List.zipWithIndex |> List.filter(p -> Int32.modulo(fst(p), stride) == 0)
                    |> List.map(p -> snd(p)#step#after);
                println("[video] ${List.length(outcome#frames)} フレームを流し、${List.length(segments)} 区間・${List.length(painted)} コマを撮る（${Int32.toString(List.length(painted) / fps)} 秒・${Int32.toString(fps)} fps）");
                writeVideo(settings, encoderOf(env, fps, take), fps, take#outDir, painted);
                writeAudio(take, shown |> List.map(p -> p#step));
                0
        }

    /// 音を混ぜる速さ（効果音の WAV と同じ）。
    def sampleRate(): Int32 = 44100

    /// 撮るフレームの音を混ぜて debug/video/<名前>.wav に書く。素材は assets/sfx の WAV（名前はファイル名から .wav を除いた物）。
    def writeAudio(take: Take, steps: List[App.TraceStep[AppState]]): Unit \ IO =
        let dir = new File("assets/sfx");
        let names: List[String] = Array.toList(dir.list());
        let sounds = names |> List.filter(n -> String.endsWith(suffix = ".wav", n))
            |> List.map(n -> (String.dropRight(4, n), AudioMixdown.loadWav(sampleRate(), "assets/sfx/${n}"))) |> List.toMap;
        let mixer = steps |> List.head |> Option.map(st -> Resources.read((r, _) -> r#mixer, st#after));
        match mixer {
            case None    => ()
            case Some(m) =>
                let frames = steps |> List.map(st -> { plays = st#sounds, sustained = st#sustained, mixer = m });
                let samples = AudioMixdown.mixdown(sampleRate(), Int32.toFloat64(playFps()), sounds, frames) |> Vector.toList;
                let path = "${take#outDir}.wav";
                SfxSynth.writeBytes(path, SfxSynth.wavBytes({ sampleRate = sampleRate() }, samples));
                println("[video] 音: ${path}（いちばん大きな振れ幅 ${AudioMixdown.peakOf(List.toVector(samples))}）")
        }

    /// mark の in_<名前> / out_<名前> の組を、始まりのフレームの順に (始まり, 終わり) で並べる。
    /// 対の無い mark・終わりが始まりより前の組は Err（黙って区間を落とすと動画が短くなったことに気付けない）。
    def segmentsOf(marks: Map[String, Int32]): Result[String, List[(Int32, Int32)]] =
        let names = Map.keysOf(marks) |> Set.toList;
        let ins = names |> List.filterMap(n -> String.stripPrefix(substr = "in_", n));
        let outs = names |> List.filterMap(n -> String.stripPrefix(substr = "out_", n));
        let inSet = List.toSet(ins);
        let outSet = List.toSet(outs);
        let lonely = List.append(ins |> List.filter(n -> not Set.memberOf(n, outSet)) |> List.map(n -> "in_${n}"),
                                 outs |> List.filter(n -> not Set.memberOf(n, inSet)) |> List.map(n -> "out_${n}"));
        let pairs = ins |> List.map(n -> (Map.getWithDefault("in_${n}", 0, marks), Map.getWithDefault("out_${n}", 0, marks)));
        let backwards = pairs |> List.filter(p -> snd(p) <= fst(p));
        if (not List.isEmpty(lonely)) Err("対の無い印 ${List.join(", ", lonely)}")
        else if (List.isEmpty(pairs)) Err("in_<名前> と out_<名前> の組が 1 つも無い")
        else if (not List.isEmpty(backwards)) Err("終わりが始まりより前の組がある")
        else Ok(Sort.sortStableBy(fst, pairs))

    /// GPU の道で debug/video/<名前>.mp4 を直に書く ffmpeg の旗（VIDEO_X264 は Makefile と同じ字を受ける）。
    def encoderOf(env: Map[String, String], fps: Int32, take: Take): VideoOut.Encoder =
        let x264 = Map.getWithDefault("VIDEO_X264", "-c:v libx264 -preset medium -crf 18 -pix_fmt yuv420p", env);
        let flags = String.splitOn({ substr = " " }, x264) |> List.filter(part -> not String.isEmpty(part));
        { rate = Int32.toString(fps), inputs = Nil,
          outputs = List.append(flags, "-movflags" :: "+faststart" :: "${take#outDir}.mp4" :: Nil) }

    def writeVideo(settings: VideoOut.Settings, encoder: VideoOut.Encoder, fps: Int32, outDir: String, worlds: List[AppState]): Unit \ IO =
        let first = match List.head(worlds) {
            case Some(w) => w
            case None    => bug!("[video] 撮るコマが無い")
        };
        let res = Resources.read((r, _) -> r, first);
        let atlas = HeadlessFont.buildUiAtlas(SceneRender.ttf(), SceneRender.ttf());
        let painter = VideoOut.writeFramesWithPasses(settings,
            { outDir = outDir, fps = fps, soft = SceneRender.cfgOf(outDir, atlas, res), softFonts = _ -> atlas, extraFonts = Map.empty(),
              images = Nil, scene = sceneOf, wanted = _ -> true, bakeSurfaces = false, encoders = encoder :: Nil },
            _ -> Resources.read((r, w) -> View.passPlan(r, w)), worlds);
        println("[video] コマ: ${outDir}（${painter}）")

    /// 1 コマの本編（GPU では GL の字のアトラスで組む）。
    def sceneOf(fonts: String -> FontAtlas): AppState -> List[Render.PlacedItem] =
        Resources.read((r, w) -> View.frame(App.viewCtxOf(fonts("default"), SceneRender.design()), r, w))
}
