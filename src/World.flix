// World — 見せる状態（決まりの状態 Run・画面・今見せている 1 手・溜めた入力・カメラ・遊びの時計）と、入力を Run へ当てる口（すべて純粋）。
// 1 手の決まりは Step、予告は Preview、ダッシュは Dash、階を作るのは Floor。Doc と盤は Res が持ち、World は持たない。
//
// ── 画面の移り変わり ──
//   Title ──決定──▶ Play ──倒れた──▶ Dead ──決定──▶ Title
//                   Play ──階段で待つ──▶ Clear ──決定──▶ Title   （試作は 1F だけ）
//   Play ──選ぶ場面が立ち、その手を見せ終えた──▶ Cards ──選ぶ（pending が空になる）──▶ Play
//
// ── 1 手の見せ方 ──
// 手を当てた時に Run を進め、その手の出来事を時刻表（TurnTimeline）で遊びの時計の上に流す。
// 見せている間に来た入力は 1 つだけ溜め、今の時刻表を最後まで飛ばしてから当てる。
// ダッシュは列を始めに全部計算し、1 歩見せ始めるごとにその歩の Run を run にする。ダッシュ中の入力は次の歩で止めるだけ（溜めない）。
mod World {

    use fge::{Camera3d, Clock, Timeline}
    use Board.{Cell, Dir}
    use Run.Input

    /// 遊びの時計の名前（Cards と終了の確認で止まる）。
    pub enum Play

    /// どの画面か。
    pub enum Screen with Eq, ToString {
        case Title
        case Play
        case Cards
        case Dead
        case Clear
    }

    /// 終了の確認。Esc か窓の閉じるボタンで Asking、はいで Confirmed（Main の App.quitWhen が見る）、いいえで Playing へ戻る。
    pub enum Quit with Eq, ToString {
        case Playing
        case Asking
        case Confirmed
    }

    /// 盤への手: 1 手（4 方向か待つ）かダッシュ。
    pub enum Act with Eq, ToString {
        case Go(Input)
        case Dash(Dir)
    }

    /// 今見せている 1 手: 手の前の Run・その手の出来事・ダッシュの残りの手の列・見せ始めた時刻・ダッシュの 1 歩か。
    pub type alias Shown = {
        before = Run.State,
        events = List[Run.Event],
        rest = List[(Run.State, List[Run.Event])],
        startedAt = Clock.Time[Play],
        dash = Bool
    }

    /// run から作り置く予告: 5 つの入力の要約・敵の予告のマス・弓兵の射線。
    pub type alias Foresight = {
        moves = List[(Input, Preview.Summary)],
        danger = Set[Cell],
        lanes = List[(Cell, Cell)]
    }

    /// カメラが追う盤の上の点（マス単位。マスの真ん中が整数）と、その今の動く速さ（マス/秒）。
    pub type alias Cam = { x = Float64, y = Float64, vx = Float64, vy = Float64 }

    /// 見せる状態。
    ///   runState  … 決まりの状態（進行の正）
    ///   shown     … 今見せている 1 手
    ///   queued    … 見せている間に来た手（1 つまで）
    ///   foresight … 予告（run が変わった時だけ作り直す）
    ///   press     … 押している途中のタップ
    ///   seed      … 周回の種
    ///   realNow   … このコマの実時間（App が配る frame#realNow の写し。松明の揺らぎと待機の揺れに使う）
    pub type alias World = {
        runState = Run.State,
        screen = Screen,
        shown = Shown,
        queued = Option[Act],
        cam = Cam,
        clock = Clock.Clock[Play],
        press = Option[Tap.Press],
        foresight = Foresight,
        quit = Quit,
        seed = Int64,
        realNow = Clock.Time[Clock.Real]
    }

    pub def width(): Float64 = 540.0
    pub def height(): Float64 = 960.0

    /// flow とテストと描き出しの周回の種。
    pub def fixedSeed(): Int64 = 20261112i64

    /// ゲーム開始時の状態（題名の画面。後ろに 1F の始まりを持つ）。
    pub def initial(res: Res.Res, seed: Int64): World =
        let runState = Floor.create(Res.floorDocs(res), res#layouts, seed, 1);
        { runState = runState, screen = Screen.Title, shown = shownOf(runState), queued = None,
          cam = camOf(runState), clock = Clock.start(), press = None, foresight = foresightOf(res, runState),
          quit = Quit.Playing, seed = seed, realNow = Clock.zero() }

    /// 何も見せていない 1 手（見せ終えた過去に置く）。
    def shownOf(runState: Run.State): Shown =
        { before = runState, events = Nil, rest = Nil, startedAt = Clock.never(), dash = false }

    def foresightOf(res: Res.Res, runState: Run.State): Foresight =
        let ctx = Res.ctxOf(res, runState);
        { moves = Preview.preview(ctx, runState), danger = Preview.dangerCells(ctx, runState), lanes = Preview.firingLanes(ctx, runState) }

    def camOf(runState: Run.State): Cam =
        let (x, y) = Run.heroOf(runState)#at;
        { x = Int32.toFloat64(x), y = Int32.toFloat64(y), vx = 0.0, vy = 0.0 }

    /// 毎コマ: カメラの点を主人公へ遊びの時計の秒でなめらかに寄せ、実時間を写す。
    pub def follow(res: Res.Res, now: Clock.Time[Clock.Real], w: World): World =
        let dt = Clock.dtOf(w#clock);
        let seconds = LookDoc.rec(res#look)#cameraFollowSeconds;
        let target = camOf(w#runState);
        let (x, vx) = Cam.smoothDamp(seconds, dt, target#x, (w#cam#x, w#cam#vx));
        let (y, vy) = Cam.smoothDamp(seconds, dt, target#y, (w#cam#y, w#cam#vy));
        { cam = { x = x, y = y, vx = vx, vy = vy }, realNow = now | w }

    /// カメラの点を主人公に合わせる（描き出しの場面は追い終えた所を撮る）。
    pub def snapCam(w: World): World = { cam = camOf(w#runState) | w }

    /// 今のカメラ（タップの方向と描く側が同じ物を使う）。
    pub def cameraOf(res: Res.Res, w: World): Camera3d.Camera3d = Cam.ofLook(LookDoc.rec(res#look), w#cam#x, w#cam#y)

    /// 今の階の判定に使う束。
    pub def ctx(res: Res.Res, w: World): Run.Ctx = Res.ctxOf(res, w#runState)

    /// 周回の種で 1F を作り直して遊び始める。
    pub def begin(res: Res.Res, w: World): World =
        let runState = Floor.create(Res.floorDocs(res), res#layouts, w#seed, 1);
        { runState = runState, screen = Screen.Play, shown = shownOf(runState), queued = None,
          foresight = foresightOf(res, runState) | w }

    // ── 1 手の時刻表 ──

    /// 今見せている 1 手の時刻表。
    pub def timelineOf(res: Res.Res, w: World): Timeline.Timeline[TurnTimeline.Tag] =
        TurnTimeline.build(TurnTimeline.paceOf(LookDoc.rec(res#look), w#shown#dash), w#shown#events)

    /// 今の 1 手の時刻表がまだ流れているか。
    def isShowing(res: Res.Res, w: World): Bool = TurnTimeline.isPlaying(w#clock, w#shown#startedAt, timelineOf(res, w))

    /// 見せ終えていない（時刻表が流れている・ダッシュの残りがある）か。
    pub def isBusy(res: Res.Res, w: World): Bool = isShowing(res, w) or not List.isEmpty(w#shown#rest)

    /// 今の 1 手を見せ終えたことにする（描き出しの場面は見せ終えた所を撮る）。
    pub def skipShow(w: World): World = { shown = { startedAt = Clock.never() | w#shown } | w }

    /// 今の 1 手を、見せ始めてから seconds 秒たった所にする（描き出しの途中の場面）。
    pub def seekShow(seconds: Float64, w: World): World =
        { shown = { startedAt = Clock.after(0.0 - seconds, Clock.nowOf(w#clock)) | w#shown } | w }

    // ── 手を当てる ──

    /// 盤への手。見せている間は 1 つだけ溜めて今の時刻表を飛ばし、ダッシュ中は次の歩で止める（溜めない）。選ぶ場面がある間は受けない。
    pub def command(res: Res.Res, act: Act, w: World): World =
        if (w#screen != Screen.Play or isChoosing(w)) w
        else if (not List.isEmpty(w#shown#rest)) { shown = { rest = Nil | w#shown } | w }
        else if (isShowing(res, w)) skipShow({ queued = Some(act) | w })
        else apply(res, act, w)

    def apply(res: Res.Res, act: Act, w: World): World = match act {
        case Act.Go(input) => play(res, input, w)
        case Act.Dash(dir) => dash(res, dir, w)
    }

    /// 選ぶ場面が立っているか（Screen.Cards は見せ終えてからこれを写す）。
    def isChoosing(w: World): Bool = not List.isEmpty(Run.pendingOf(w#runState))

    /// 毎コマ: 見せ終えたら、選ぶ場面なら Cards へ、そうでなければダッシュの次の歩か溜めた手を始める。
    pub def advance(res: Res.Res, w: World): World =
        if (isShowing(res, w)) w
        else if (w#screen == Screen.Play and isChoosing(w)) { screen = Screen.Cards | w }
        else match (w#shown#rest, w#queued) {
            case ((next, events) :: tail, _) => settleScreen(withRun(res, next, events, { dash = true, rest = tail }, w))
            case (Nil, Some(act))            => apply(res, act, { queued = None | w })
            case _                           => w
        }

    /// run を差し替え、その手を今から見せ始める（手が進まなかった時は予告を作り直さない）。
    def withRun(res: Res.Res, runState: Run.State, events: List[Run.Event], pace: { dash = Bool, rest = List[(Run.State, List[Run.Event])] }, w: World): World =
        let foresight = if (isBump(events)) w#foresight else foresightOf(res, runState);
        { runState = runState, foresight = foresight,
          shown = { before = w#runState, events = events, rest = pace#rest, startedAt = Clock.nowOf(w#clock), dash = pace#dash } | w }

    def isBump(events: List[Run.Event]): Bool = match events {
        case Run.Event.HeroBlocked(_) :: Nil => true
        case _                               => false
    }

    /// 1 手を当てる（見せている途中でもすぐ当てる。入力から来る手は command を通す）。Play の外は何もしない。倒れたら Dead・降りたら Clear。
    pub def play(res: Res.Res, input: Input, w: World): World =
        if (w#screen != Screen.Play) w
        else {
            let (next, events) = Step.step(ctx(res, w), w#runState, input);
            settleScreen(withRun(res, next, events, { dash = false, rest = Nil }, w))
        }

    /// ダッシュ: 列を全部計算し、最初の歩を当てて見せ始める（残りは advance が 1 歩ずつ）。
    def dash(res: Res.Res, dir: Dir, w: World): World =
        if (w#screen != Screen.Play) w
        else match Dash.dash(ctx(res, w), w#runState, dir) {
            case (first, events) :: tail => settleScreen(withRun(res, first, events, { dash = true, rest = tail }, w))
            case Nil                     => w
        }

    /// 倒れたら Dead・降りたら Clear。選ぶ場面が立ったら溜めた手とダッシュの残りを捨てる（Cards へは見せ終えてから advance が移す）。
    def settleScreen(w: World): World = match Run.statusOf(w#runState) {
        case Run.Status.Dead      => { screen = Screen.Dead, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Descended => { screen = Screen.Clear, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Playing if isChoosing(w) => { queued = None, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Playing   => w
    }

    /// 選ぶ場面の index 枚目で答え、答え終えたら遊びへ戻る（範囲外は何もしない）。
    pub def answerCard(res: Res.Res, index: Int32, w: World): World =
        if (w#screen != Screen.Cards) w
        else {
            let (next, events) = Step.step(ctx(res, w), w#runState, Input.Choose(index));
            let answered = withRun(res, next, events, { dash = false, rest = Nil }, w);
            { screen = if (isChoosing(answered)) Screen.Cards else Screen.Play | answered }
        }

    /// 決定で答える時のカード: 捨てるカードは「拾わない」（足もとの物・最後の 1 枚）。
    pub def defaultChoice(w: World): Int32 = match Run.pendingOf(w#runState) {
        case Run.Choice.Swap(_) :: _ => List.length(Run.packOf(w#runState))
        case _                       => 0
    }

    /// 題名へ戻る（次の周回の種で、後ろの 1F を作り直す）。
    pub def toTitle(res: Res.Res, w: World): World =
        { clock = w#clock, quit = w#quit | initial(res, fst(Rng.next(w#seed))) }

    // ── 時計 ──

    pub def clockOf(w: World): Clock.Clock[Play] = w#clock

    pub def withClock(clock: Clock.Clock[Play], w: World): World = { clock = clock | w }

    /// Cards と終了の確認の間は遊びの時計を止める。
    pub def paceOf(w: World): Clock.Pace[Play, Screen] =
        if (w#screen == Screen.Cards) Clock.paceOf(Clock.Run.Paused(Screen.Cards))
        else if (w#quit == Quit.Asking) Clock.paceOf(Clock.Run.Paused(w#screen))
        else Clock.paceOf(Clock.Run.Running)

    /// このコマに鳴らす音（今見せている 1 手の時刻表の Cue）。
    pub def sounds(res: Res.Res, w: World): List[String] = Timeline.soundsOnClock(w#clock, w#shown#startedAt, timelineOf(res, w))

    // ── 終了の確認 ──

    /// Esc・閉じるボタン: 確認の窓を出す。
    pub def askQuit(w: World): World =
        if (w#quit == Quit.Playing) { quit = Quit.Asking | w } else w

    /// 確認の窓への答え（yes なら終わる・no なら遊びへ戻る）。窓が出ていないときは何もしない。
    pub def answerQuit(yes: Bool, w: World): World =
        if (w#quit != Quit.Asking) w
        else if (yes) { quit = Quit.Confirmed | w }
        else { quit = Quit.Playing | w }

    pub def isAskingQuit(w: World): Bool = w#quit == Quit.Asking

    pub def isQuitting(w: World): Bool = w#quit == Quit.Confirmed

    // ── Doc の差し替え（保存即反映） ──

    /// 決まり・アイテム・敵・階の Doc を保存した後: 今の階を同じ種で作り直す（予告も作り直す）。
    pub def settle(res: Res.Res, w: World): World =
        let floor = Run.rec(w#runState)#floor;
        let runState = Floor.create(Res.floorDocs(res), res#layouts, Run.rec(w#runState)#seed, floor);
        { runState = runState, shown = shownOf(runState), queued = None, cam = camOf(runState),
          foresight = foresightOf(res, runState) | w }
}
