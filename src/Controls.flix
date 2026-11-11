// Controls — キー・タップを World の手（1 歩・待つ・ダッシュ・決定）へ写す（純粋）と、Doc の読み直し。
// どのキーがどの操作かは KeysDoc（stepdown.keys.json）が決める。1 フレームに当てる操作は 1 つだけ。
// タップは Tap（離したコマで確定・方向は床との交点）、スワイプは project.json の web.touchGestures がダッシュのキーへ読み替える。
mod Controls {

    use fge::{App, AudioMixer, AudioMixerDoc, DocTable, LoadProgress, Resources, Vec2}
    use fge::GameEngine.Key
    use Board.Dir

    /// 毎フレームのシステム: 入力を当て、見せ終えた手の続き（ダッシュの次の歩・溜めた手）を始め、カメラを寄せる。
    /// 終了の確認の窓が出ている間は Enter で終わる・Esc で戻る（OS の決まりに合わせた固定のキー。割り当ての Doc には載せない）。
    pub def step(frame: App.Frame, res: Res.Res, w: World.World): World.World =
        World.follow(res, frame#realNow, World.advance(res, act(frame, res, w)))

    def act(frame: App.Frame, res: Res.Res, w: World.World): World.World =
        let pressed = key -> App.justPressed(key, frame);
        if (World.isAskingQuit(w))
            (if (pressed(Key.Enter)) World.answerQuit(true, w)
             else if (pressed(Key.Escape)) World.answerQuit(false, w)
             else w)
        else if (pressed(Key.Escape)) World.askQuit(w)
        else {
            let command = commandOf(frame, KeysDoc.rec(res#keys));
            let pointer = { clicked = frame#mouseClicked, down = frame#mouseDown, cursor = frame#cursor, now = frame#realNow,
                            dashed = Option.exists(isDash, command) };
            let (press, tapped) = Tap.track(RulesDoc.rec(res#rules)#tapSlop, pointer, w#press);
            let held = { press = press | w };
            match (command, tapped) {
                case (Some(c), _)     => apply(res, c, held)
                case (None, Some(at)) => apply(res, tapCommand(res, at, held), held)
                case _                => held
            }
        }

    /// キーが表す操作。
    pub enum Command with Eq, ToString {
        case Go(Run.Input)
        case Dash(Dir)
        case Confirm
    }

    def isDash(command: Command): Bool = match command {
        case Command.Dash(_) => true
        case _               => false
    }

    /// タップの操作: 遊びの画面では盤の上の点の向き（UI の帯の中は何もしない）、ほかの画面では決定。
    def tapCommand(res: Res.Res, at: Vec2.Vec2, w: World.World): Command =
        if (w#screen != World.Screen.Play) Command.Confirm
        else {
            let screen = { camera = World.cameraOf(res, w), size = { x = World.width(), y = World.height() }, bandTop = HudView.bandTop() };
            Tap.inputOf(screen, RulesDoc.rec(res#rules)#tapWaitRadius, Run.heroOf(w#runState)#at, at)
                |> Option.map(Command.Go) |> Option.getWithDefault(Command.Confirm)
        }

    /// このフレームに押した操作（上の行ほど先に見る）。Shift を押しながらの矢印はダッシュ。
    def commandOf(frame: App.Frame, k: KeysDoc.Rec): Option[Command] =
        let tapped = (keys: List[Key]) -> List.exists(key -> App.justPressed(key, frame), keys);
        let shifted = App.isDown(Key.LeftShift, frame) or App.isDown(Key.RightShift, frame);
        let arrow = dir -> if (shifted) Command.Dash(dir) else Command.Go(Run.Input.Move(dir));
        ((k#confirm, Command.Confirm)
            :: (k#dashUp, Command.Dash(Dir.Up)) :: (k#dashDown, Command.Dash(Dir.Down))
            :: (k#dashLeft, Command.Dash(Dir.Left)) :: (k#dashRight, Command.Dash(Dir.Right))
            :: (k#up, arrow(Dir.Up)) :: (k#down, arrow(Dir.Down))
            :: (k#left, arrow(Dir.Left)) :: (k#right, arrow(Dir.Right))
            :: (k#wait, Command.Go(Run.Input.Wait)) :: Nil)
            |> List.find(e -> tapped(fst(e))) |> Option.map(snd)

    /// 操作を画面ごとに当てる。題名はどの操作でも始め、遊びは 1 歩・待つ・ダッシュ、倒れた・抜けた後は見せ終えてから決定で題名へ。
    def apply(res: Res.Res, command: Command, w: World.World): World.World =
        match (w#screen, command) {
            case (World.Screen.Title, _)                => World.begin(res, w)
            case (World.Screen.Play, Command.Go(input)) => World.command(res, World.Act.Go(input), w)
            case (World.Screen.Play, Command.Dash(dir)) => World.command(res, World.Act.Dash(dir), w)
            case (World.Screen.Dead, Command.Confirm) if not World.isBusy(res, w)  => World.toTitle(res, w)
            case (World.Screen.Clear, Command.Confirm) if not World.isBusy(res, w) => World.toTitle(res, w)
            case _ => w
        }

    /// 1 フレームの音: 今見せている 1 手の時刻表の Cue（App.withAudio に渡す形）。
    pub def sounds(res: Res.Res, w: World.World): List[String] = World.sounds(res, w)

    def mixerPath(): String = "assets/stepdown.mixer.json"

    // ── Doc の読み込み・読み直し（watchFile / F1） ──

    /// 起動時・描き出しが読む Doc の束と部品の束（kit.glb）。区切りごとに進み具合を知らせる
    /// （実機は起動画面が進む・描き出しは HeadlessLoad.handle で捨てる）。
    pub def loadAll(): Res.Docs \ Fs.FileRead + IO + LoadProgress =
        LoadProgress.report("reading docs...", 0, 2);
        let docs = loadDocs();
        LoadProgress.report("reading kit...", 1, 2);
        { kit = Kit.load(LoadProgress.imageDecoder(), Kit.path()) | docs }

    /// Doc の束（部品の束は空。3D の形を描かない flow とテストが読む）。
    pub def loadDocs(): Res.Docs \ Fs.FileRead =
        { rules = RulesDoc.load(RulesDoc.path()), items = ItemsDoc.load(ItemsDoc.path()),
          enemies = EnemiesDoc.load(EnemiesDoc.path()), floors = FloorsDoc.load(FloorsDoc.path()),
          look = LookDoc.load(LookDoc.path()), text = TextDoc.load(TextDoc.path()),
          keys = KeysDoc.load(KeysDoc.path()), theme = ThemeDoc.load(ThemeDoc.path()),
          mixer = AudioMixerDoc.load(AudioMixer.defaults(), mixerPath()), kit = Kit.empty() }

    /// Doc の一覧表(DocTable)。watchFile(保存即反映)・F1 の一括リロード・Studio へ名乗る「表示中」は、
    /// どれもこの 1 枚から導出する。docId は project.json の editor.resources[] の id と同じ物。
    /// 決まり・アイテム・敵・階は今の階を同じ種で作り直す（World.settle）。ほかは World をそのまま。
    pub def docTable(): List[DocTable.Entry[Resources.With[Res.Res, World.World]]] =
        let unchanged = (_, w) -> w;
        Resources.reloadRow("rules", RulesDoc.path(), res -> Res.withRules(RulesDoc.load(RulesDoc.path()), res), World.settle)
            :: Resources.reloadRow("items", ItemsDoc.path(), res -> Res.withItems(ItemsDoc.load(ItemsDoc.path()), res), World.settle)
            :: Resources.reloadRow("enemies", EnemiesDoc.path(), res -> Res.withEnemies(EnemiesDoc.load(EnemiesDoc.path()), res), World.settle)
            :: Resources.reloadRow("floors", FloorsDoc.path(), res -> Res.withFloors(FloorsDoc.load(FloorsDoc.path()), res), World.settle)
            :: Resources.reloadRow("look", LookDoc.path(), res -> Res.withLook(LookDoc.load(LookDoc.path()), res), unchanged)
            :: Resources.reloadRow("text", TextDoc.path(), res -> Res.withText(TextDoc.load(TextDoc.path()), res), unchanged)
            :: Resources.reloadRow("keys", KeysDoc.path(), res -> Res.withKeys(KeysDoc.load(KeysDoc.path()), res), unchanged)
            :: Resources.reloadRow("theme", ThemeDoc.path(), res -> Res.withTheme(ThemeDoc.load(ThemeDoc.path()), res), unchanged)
            :: Resources.reloadRow("mixer", mixerPath(), res -> Res.withMixer(AudioMixerDoc.load(AudioMixer.defaults(), mixerPath()), res), unchanged)
            :: Nil
}
