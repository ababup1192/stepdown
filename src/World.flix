// World — 見せる状態（決まりの状態 Run・画面・今見せている 1 手・溜めた入力・カメラ・遊びの時計）と、入力を Run へ当てる口（すべて純粋）。
// 1 手の決まりは Step、予告は Preview、ダッシュは Dash、階を作るのは Floor。Doc と盤は Res が持ち、World は持たない。
//
// ── 画面の移り変わり ──
//   Title ──決定──▶ Play ──倒れた──▶ Dead ──決定──▶ Title
//                   Play ──階段で待つ──▶ 暗転して次の階の Play（最後の階なら Clear ──決定──▶ Title）
//   Play ──選ぶ場面が立ち、その手を見せ終えた──▶ Cards ──選ぶ（pending が空になる）──▶ Play
//
// ── 1 手の見せ方 ──
// 手を当てた時に Run を進め、その手の出来事を時刻表（TurnTimeline）で遊びの時計の上に流す。
// 見せている間に来た入力は 1 つだけ溜め、今の時刻表を最後まで飛ばしてから当てる。
// ダッシュは列を始めに全部計算し、1 歩見せ始めるごとにその歩の Run を run にする。ダッシュ中の入力は次の歩で止めるだけ（溜めない）。
mod World {

    use fge::{App, Camera3d, Clock, Timeline}
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

    /// 選んだカード（遊びへ戻った後も実時間で主人公へ吸い込まれる・捨てた物は落ちて消える）。
    /// choice = 答えた選ぶ場面・pack = 答える前の枠の列・index = 何枚目か・startedAt = 選んだ実時間。
    pub type alias Chosen = { choice = Run.Choice, pack = Run.Pack, index = Int32, startedAt = Clock.Time[Clock.Real] }

    /// カメラが追う盤の上の点（マス単位。マスの真ん中が整数）と、その今の動く速さ（マス/秒）。
    pub type alias Cam = { x = Float64, y = Float64, vx = Float64, vy = Float64 }

    /// 見せる状態。
    ///   runState  … 決まりの状態（進行の正）
    ///   shown     … 今見せている 1 手
    ///   queued    … 見せている間に来た手（1 つまで）
    ///   foresight … 予告（run が変わった時だけ作り直す）
    ///   seed      … 周回の種
    ///   realNow   … このコマの実時間（App が配る frame#realNow の写し。松明の揺らぎと待機の揺れに使う）
    ///   popups    … 浮かぶ数（実時間で動く。Cards の間も止まらない）
    ///   trail     … HP の棒の減った分・増えた分
    ///   cardsAt   … 今のカードを出し始めた実時間（レベルアップの演出もここから）
    ///   chosen    … 最後に選んだカード
    ///   veilAt    … 階を降りる暗転を始めた実時間（降りていなければ遠い過去）
    pub type alias World = {
        runState = Run.State,
        screen = Screen,
        shown = Shown,
        queued = Option[Act],
        cam = Cam,
        clock = Clock.Clock[Play],
        foresight = Foresight,
        quit = Quit,
        seed = Int64,
        realNow = Clock.Time[Clock.Real],
        popups = List[Popup.Popup],
        trail = Popup.Trail,
        cardsAt = Clock.Time[Clock.Real],
        chosen = Option[Chosen],
        veilAt = Clock.Time[Clock.Real]
    }

    pub def width(): Float64 = 540.0
    pub def height(): Float64 = 960.0

    /// flow とテストと描き出しの周回の種。
    pub def fixedSeed(): Int64 = 20261112i64

    /// ゲーム開始時の状態（題名の画面。後ろに 1F の始まりを持つ）。
    pub def initial(res: Res.Res, seed: Int64): World =
        let runState = Floor.begin(Res.floorDocs(res), res#layouts, seed);
        { runState = runState, screen = Screen.Title, shown = shownOf(runState), queued = None,
          cam = camOf(runState), clock = Clock.start(), foresight = foresightOf(res, runState),
          quit = Quit.Playing, seed = seed, realNow = Clock.zero(),
          popups = Nil, trail = Popup.still(Run.heroOf(runState)#hp), cardsAt = Clock.never(), chosen = None, veilAt = Clock.never() }

    /// 何も見せていない 1 手（見せ終えた過去に置く）。
    def shownOf(runState: Run.State): Shown =
        { before = runState, events = Nil, rest = Nil, startedAt = Clock.never(), dash = false }

    def foresightOf(res: Res.Res, runState: Run.State): Foresight =
        let ctx = Res.ctxOf(res, runState);
        { moves = Preview.preview(ctx, runState), danger = Preview.dangerCells(ctx, runState), lanes = Preview.firingLanes(ctx, runState) }

    def camOf(runState: Run.State): Cam =
        let (x, y) = Run.heroOf(runState)#at;
        { x = Int32.toFloat64(x), y = Int32.toFloat64(y), vx = 0.0, vy = 0.0 }

    /// 浮かぶ数と HP の棒の長さ（look の Doc から）。
    pub def timingOf(res: Res.Res): Popup.Timing =
        let lk = LookDoc.rec(res#look);
        { life = lk#popupSeconds, hold = lk#hpHoldSeconds, shrink = lk#hpShrinkSeconds }

    /// 毎コマの始め: 実時間を写し、消える長さを過ぎた浮かぶ数を外す。
    pub def tick(res: Res.Res, now: Clock.Time[Clock.Real], w: World): World =
        { realNow = now, popups = Popup.prune(timingOf(res), now, w#popups) | w }

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

    /// 決まりの状態を差し替え、何も見せていない所から始める（予告と HP の棒も作り直す。描き出しの場面が並べ替えた後に使う）。
    pub def withState(res: Res.Res, runState: Run.State, w: World): World =
        { runState = runState, shown = shownOf(runState), queued = None, foresight = foresightOf(res, runState),
          trail = Popup.still(Run.heroOf(runState)#hp) | w }

    /// 周回の種で 1F を作り直して遊び始める。
    pub def begin(res: Res.Res, w: World): World =
        let runState = Floor.begin(Res.floorDocs(res), res#layouts, w#seed);
        { runState = runState, screen = Screen.Play, shown = shownOf(runState), queued = None,
          foresight = foresightOf(res, runState), popups = Nil, trail = Popup.still(Run.heroOf(runState)#hp), chosen = None | w }

    // ── 1 手の時刻表 ──

    /// 今見せている 1 手の時刻表。
    pub def timelineOf(res: Res.Res, w: World): Timeline.Timeline[TurnTimeline.Tag] =
        TurnTimeline.build(TurnTimeline.paceOf(LookDoc.rec(res#look), w#shown#dash), w#shown#events)

    /// 今の 1 手の時刻表がまだ流れているか。
    def isShowing(res: Res.Res, w: World): Bool = TurnTimeline.isPlaying(w#clock, w#shown#startedAt, timelineOf(res, w))

    /// 見せ終えていない（時刻表が流れている・ダッシュの残りがある・階を降りている）か。
    pub def isBusy(res: Res.Res, w: World): Bool =
        isShowing(res, w) or not List.isEmpty(w#shown#rest) or Run.statusOf(w#runState) == Run.Status.Descended or veilOf(res, w) > 0.0

    /// 階を降りる暗転の濃さ（0..1。暗転の半分の秒で 1 になり、次の階に替わってから同じ秒で 0 へ戻る）。
    pub def veilOf(res: Res.Res, w: World): Float64 =
        let half = LookDoc.rec(res#look)#descendSeconds / 2.0;
        let since = Clock.since(w#veilAt, w#realNow);
        if (Run.statusOf(w#runState) == Run.Status.Descended) Float64.min(1.0, Float64.max(0.0, since / half))
        else Float64.max(0.0, 1.0 - (since - half) / half) |> Float64.min(1.0)

    /// 今の 1 手を見せ終えたことにする（描き出しの場面は見せ終えた所を撮る）。
    pub def skipShow(w: World): World = { shown = { startedAt = Clock.never() | w#shown }, popups = Popup.hurry(w#realNow, w#popups) | w }

    /// 今の 1 手を、見せ始めてから seconds 秒たった所にする（描き出しの途中の場面）。
    pub def seekShow(seconds: Float64, w: World): World =
        { shown = { startedAt = Clock.after(0.0 - seconds, Clock.nowOf(w#clock)) | w#shown },
          popups = Popup.shift(seconds, w#popups), trail = Popup.shiftTrail(seconds, w#trail) | w }

    /// 今のカードを、出し始めてから seconds 秒たった所にする（描き出しのレベルアップの途中の場面）。
    /// 浮かぶ数と HP の棒も同じだけ進める。
    pub def seekCards(seconds: Float64, w: World): World =
        { cardsAt = Clock.after(0.0 - seconds, w#realNow), popups = Popup.shift(seconds, w#popups), trail = Popup.shiftTrail(seconds, w#trail) | w }

    /// 選んだカードを、選んでから seconds 秒たった所にする（描き出しの吸い込まれる途中の場面）。
    pub def seekChosen(seconds: Float64, w: World): World =
        { chosen = w#chosen |> Option.map(c -> { startedAt = Clock.after(0.0 - seconds, w#realNow) | c }) | w }

    // ── 手を当てる ──

    /// 盤への手。見せている間は 1 つだけ溜めて今の時刻表を飛ばし、ダッシュ中は次の歩で止める（溜めない）。選ぶ場面がある間は受けない。
    pub def command(res: Res.Res, act: Act, w: World): World =
        if (w#screen != Screen.Play or isChoosing(w) or Run.statusOf(w#runState) != Run.Status.Playing) w
        else if (not List.isEmpty(w#shown#rest)) { shown = { rest = Nil | w#shown } | w }
        else if (isShowing(res, w)) skipShow({ queued = Some(act) | w })
        else apply(res, act, w)

    def apply(res: Res.Res, act: Act, w: World): World = match act {
        case Act.Go(input) => play(res, input, w)
        case Act.Dash(dir) => dash(res, dir, w)
    }

    /// 選ぶ場面が立っているか（Screen.Cards は見せ終えてからこれを写す）。
    def isChoosing(w: World): Bool = not List.isEmpty(Run.pendingOf(w#runState))

    /// 毎コマ: 見せ終えたら、選ぶ場面なら Cards へ、降りた手なら暗転して次の階へ、そうでなければダッシュの次の歩か溜めた手を始める。
    pub def advance(res: Res.Res, w: World): World =
        if (isShowing(res, w)) w
        else if (w#screen == Screen.Play and isChoosing(w)) { screen = Screen.Cards, cardsAt = w#realNow | w }
        else if (Run.statusOf(w#runState) == Run.Status.Descended) descend(res, w)
        else match (w#shown#rest, w#queued) {
            case ((next, events) :: tail, _) => settleScreen(withRun(res, next, events, { dash = true, rest = tail }, w))
            case (Nil, Some(act))            => apply(res, act, { queued = None | w })
            case _                           => w
        }

    /// run を差し替え、その手を今から見せ始める（手が進まなかった時は予告を作り直さない）。
    /// 浮かぶ数と HP の棒もこの手の出来事から足す（始まりは時刻表の秒を今の実時間に足した所）。
    def withRun(res: Res.Res, runState: Run.State, events: List[Run.Event], pace: { dash = Bool, rest = List[(Run.State, List[Run.Event])] }, w: World): World =
        let foresight = if (isBump(events)) w#foresight else foresightOf(res, runState);
        let shown = { before = w#runState, events = events, rest = pace#rest, startedAt = Clock.nowOf(w#clock), dash = pace#dash };
        let tl = TurnTimeline.build(TurnTimeline.paceOf(LookDoc.rec(res#look), pace#dash), events);
        let turn = { before = Run.rec(w#runState), after = Run.rec(runState), events = events };
        { runState = runState, foresight = foresight, shown = shown,
          popups = w#popups ::: Popup.fromTurn(timingOf(res), turn, tl, w#realNow, w#popups),
          trail = Popup.trailOf(timingOf(res), turn, tl, w#realNow, w#trail) | w }

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

    /// 見せ終えた降りた手: 暗転を始め、暗くなりきったら次の階を作って何も見せていない所から始める
    /// （溜めた手・浮かぶ数・HP の棒の動きは前の階の物なので捨て、カメラは主人公へ飛ばす）。
    def descend(res: Res.Res, w: World): World =
        let half = LookDoc.rec(res#look)#descendSeconds / 2.0;
        if (Clock.since(w#veilAt, w#realNow) > LookDoc.rec(res#look)#descendSeconds) { veilAt = w#realNow | w }
        else if (Clock.since(w#veilAt, w#realNow) < half) w
        else {
            let runState = Floor.enter(Res.floorDocs(res), res#layouts, w#runState, Run.rec(w#runState)#floor + 1);
            { runState = runState, shown = shownOf(runState), queued = None, cam = camOf(runState), foresight = foresightOf(res, runState),
              popups = Nil, trail = Popup.still(Run.heroOf(runState)#hp) | w }
        }

    /// 倒れたら Dead・最後の階を抜けたら Clear・降りたらダッシュの残りを捨てる。選ぶ場面が立ったら溜めた手とダッシュの残りを捨てる（Cards へは見せ終えてから advance が移す）。
    def settleScreen(w: World): World = match Run.statusOf(w#runState) {
        case Run.Status.Dead      => { screen = Screen.Dead, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Descended => { queued = None, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Cleared   => { screen = Screen.Clear, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Playing if isChoosing(w) => { queued = None, shown = { rest = Nil | w#shown } | w }
        case Run.Status.Playing   => w
    }

    /// 選ぶ場面の index 枚目で答え、答え終えたら遊びへ戻る（範囲外は何もしない）。
    pub def answerCard(res: Res.Res, index: Int32, w: World): World =
        if (w#screen != Screen.Cards) w
        else {
            let (next, events) = Step.step(ctx(res, w), w#runState, Input.Choose(index));
            let answered = withRun(res, next, events, { dash = false, rest = Nil }, w);
            let picked = Run.pendingOf(w#runState) |> List.head |> Option.filter(_ -> List.length(Run.pendingOf(next)) != List.length(Run.pendingOf(w#runState)))
                |> Option.map(choice -> { choice = choice, pack = Run.packOf(w#runState), index = index, startedAt = w#realNow });
            let chosen = if (Option.isEmpty(picked)) w#chosen else picked;
            if (isChoosing(answered)) { screen = Screen.Cards, cardsAt = w#realNow, chosen = chosen | answered }
            else { screen = Screen.Play, chosen = chosen | answered }
        }

    /// 今の選ぶ場面のカードの枚数（捨てるカードは持っている枠＋足もとの物・レベルアップは候補の数）。
    pub def cardCount(w: World): Int32 = match Run.pendingOf(w#runState) {
        case Run.Choice.Swap(_) :: _        => List.length(Run.packOf(w#runState)) + 1
        case Run.Choice.LevelUp(perks) :: _ => List.length(perks)
        case Nil                            => 0
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

    /// 鳴り続ける曲: 題名・遊ぶ画面・カードの間はハープの曲を流す。倒れた・抜けた画面では止める
    /// （倒れる音とファンファーレの後を静かにして、終わったことを音で分ける）。
    pub def music(w: World): List[App.Sustain] =
        musicOf(w#screen) |> Option.toList |> List.map(name -> { name = name, volume = 1.0, pitch = 1.0 })

    /// 画面で流す曲の名前（無ければ None）。
    pub def musicOf(screen: Screen): Option[String] = match screen {
        case Screen.Dead  => None
        case Screen.Clear => None
        case _            => Some("bgm_harp")
    }

    /// カードの音（遊びの時計が止まっている間に起きるので時刻表に乗せず、前後の World から導く）:
    /// レベルアップのカードが出たらファンファーレ、選んだらカードを選ぶ音・捨てる音。
    pub def cardSounds(before: World, after: World): List[String] =
        let opened = if (after#cardsAt != before#cardsAt and after#screen == Screen.Cards) Run.pendingOf(after#runState) |> List.head else None;
        let fanfare = opened |> Option.filter(Run.isLevelUp) |> Option.map(_ -> "sd_fanfare") |> Option.toList;
        let stamp = (w: World) -> w#chosen |> Option.map(c -> c#startedAt);
        let picked = if (stamp(after) != stamp(before)) after#chosen else None;
        let pick = picked |> Option.map(c -> if (Run.isLevelUp(c#choice)) "sd_card" else "sd_discard") |> Option.toList;
        pick ::: fanfare

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

    /// 決まり・アイテム・敵・階の Doc を保存した後: 今の階を同じ種で作り直す（持ち越した物はそのまま・予告も作り直す）。
    pub def settle(res: Res.Res, w: World): World =
        let runState = Floor.enter(Res.floorDocs(res), res#layouts, w#runState, Run.rec(w#runState)#floor);
        { runState = runState, shown = shownOf(runState), queued = None, cam = camOf(runState),
          foresight = foresightOf(res, runState), popups = Nil, trail = Popup.still(Run.heroOf(runState)#hp) | w }
}
