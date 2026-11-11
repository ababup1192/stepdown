// E2e — e2e/<名前>.flow の語彙（App.withFlow の表と指紋）と、画面なしで流す入口。
// flow の書き方は engine の engine_world/src/Flow.flix 冒頭。名前の一覧は流すたびに e2e/vocab.json へ書き出す。
//   make flow FLOW=e2e/level1.flow   画面なしで最後まで流す
mod E2e {
    use fge::{App, Flow, InputMap, Resources}
    use fge::GameEngine.Key

    /// make flow（bin/fge flow run|check）が流す入口。
    pub def play(): Unit \ IO = App.Headless.exitWith(playJob(App.Headless.requestFromProcess()))

    def playJob(req: App.Headless.Request): Int32 \ IO =
        run { Flow.Cli.mainOf(Game.app(Controls.loadDocs(), World.fixedSeed()), App.Headless.envOf(req)) } with Fs.FileRead.runWithIO with Fs.FileWrite.runWithIO

    /// 語彙 e2e/vocab.json だけを書く入口（bin/headless-run が jar を作った時に流す・make flow-vocab）。
    pub def vocab(): Unit \ IO = App.Headless.exitWith(vocabJob(App.Headless.requestFromProcess()))

    def vocabJob(req: App.Headless.Request): Int32 \ IO =
        run { Flow.Cli.vocabMainOf(Game.app(Controls.loadDocs(), World.fixedSeed()), App.Headless.envOf(req)) } with Fs.FileRead.runWithIO with Fs.FileWrite.runWithIO

    /// 常駐（App.Headless.serveJobs）が /run で流す E2e の入口の束。
    pub def jobs(): List[App.Headless.Job] =
        App.Headless.job("E2e", "play", req -> playJob(req)) ::
        App.Headless.job("E2e", "vocab", req -> vocabJob(req)) :: Nil

    /// 役の名前 → キー（stepdown.keys.json の既定と同じ）。
    def roles(): InputMap.Table[String] =
        (Key.Left, "left") :: (Key.Right, "right") :: (Key.Up, "up") :: (Key.Down, "down") :: (Key.Space, "wait") ::
        (Key.Q, "upLeft") :: (Key.E, "upRight") :: (Key.Z, "downLeft") :: (Key.C, "downRight") ::
        (Key.W, "dashUp") :: (Key.S, "dashDown") :: (Key.A, "dashLeft") :: (Key.D, "dashRight") :: (Key.Enter, "confirm") :: Nil

    /// flow の表: 役と問い。
    pub def table(): Flow.Table[Resources.With[Res.Res, World.World]] =
        let hero = w -> Run.heroOf(w#runState);
        { roles = _ -> roles(),
          probes =
              { name = "screen", doc = "画面（Title・Play・Cards・Dead・Clear）", read = Flow.Probe.Text(Resources.read((_, w) -> "${w#screen}")) } ::
              { name = "floor", doc = "階（1 から）", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(Run.rec(w#runState)#floor))) } ::
              { name = "hp", doc = "主人公の HP", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(hero(w)#hp))) } ::
              { name = "col", doc = "主人公の列", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(fst(hero(w)#at)))) } ::
              { name = "row", doc = "主人公の行", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(snd(hero(w)#at)))) } ::
              { name = "walked", doc = "移動した手の数", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(Run.rec(w#runState)#walked))) } ::
              { name = "foes", doc = "生きている敵の数", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(List.length(Run.enemiesOf(w#runState))))) } ::
              { name = "arrows", doc = "持っている矢の数", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(Run.countOf(Run.heldOf(Item.Family.Arrow, Run.packOf(w#runState)))))) } ::
              { name = "bombs", doc = "持っている爆弾の数", read = Flow.Probe.Num(Resources.read((_, w) -> Int32.toFloat64(Run.countOf(Run.heldOf(Item.Family.Bomb, Run.packOf(w#runState)))))) } ::
              { name = "busy", doc = "1 手を見せている途中か（ダッシュの残りも含む）", read = Flow.Probe.Flag(Resources.read((res, w) -> World.isBusy(res, w))) } :: Nil,
          ready = Some(Resources.read((res, w) -> if (World.isBusy(res, w)) Flow.Ready.Busy("1 手を見せている") else Flow.Ready.Ready))
          | Flow.emptyTable() }

    /// 指紋の元（記録の再生が比べる）: 画面と Run だけ。
    pub def fingerprint(_res: Res.Res, w: World.World): String =
        "${w#screen}|${Run.fingerprint(w#runState)}"
}
