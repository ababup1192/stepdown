// View — World から「(置き場所, 見せたい物) のリスト」と、その前に描く Pass の計画への投影（純粋）。
//
// ── 画面の層（奥から手前へ）──
// 場面（Layer.Scene）: 斜め上から見下ろす 3D のダンジョン（SceneView）。Pass の中で 3D の場面とブルームを組み、
//   周辺を暗くしてトーンマップした面 1 枚を FXAA でならして画面へ置く（PostView）。
// 印（Layer.Marks）: 方向の予告・眠りの z・浮かぶ数・斬り跡・矢の軌跡・回復の輪（MarkView。3D の点を Camera3d.worldToScreen で画面へ写した所）。
// HUD（Layer.Hud）: 画面の周りの赤・上の帯（階・経験値・HP・満腹度・剣と盾・小さい地図）・下の帯（記録・持ち物）・題名と終わりの板（HudView）。
// カード（Layer.Cards）: 捨てるカード・レベルアップのカードと演出・選んだカードの動き（CardView）。
// 終了の確認の窓（Layer.Prompt）: どれよりも手前。
mod View {

    use fge::{App, Camera3d, Clock, Color, Curve, Num, Render, SortingLayer, Timeline, Vec2, Vec3, ZStack}
    use Board.{Cell, Dir8}
    use ZOrder.Layer

    /// 2D に重ねる物の値（上と下の帯・場面の上の印・カード）。
    pub type alias Overlay = { bars = HudView.In, marks = MarkView.In, cards = CardView.In }

    /// 描くのに要る値。部品は World でなくこれを読む（lint/rules.flix の view-no-world）。
    pub type alias In = { scene = SceneView.In, hud = Overlay, clear = Color }

    def size(): Vec2.Vec2 = { x = World.width(), y = World.height() }

    // ── 入口 ──

    /// 本編の面（2D の印と HUD。3D の場面は passPlan が先に描いた面を置くだけ）。
    // WhyNot: inputOf を通さない — 3D の場面の値は Pass の計画の側で組むので、ここでも組むとコマごとに 2 回払う。
    pub def frame(ctx: App.ViewCtx, res: Res.Res, w: World.World): List[Render.PlacedItem] = SortingLayer.compose(ctx, layers(ctx, hudOf(res, w)))

    pub def frameOf(ctx: App.ViewCtx, w: In): List[Render.PlacedItem] = SortingLayer.compose(ctx, layers(ctx, w#hud))

    /// 本編より先に描く Pass の計画（3D の場面 → ブルーム → 後処理）。
    pub def passPlan(res: Res.Res, w: World.World): Render.PassPlan = planOf(sceneOf(res, w))

    pub def passPlanOf(w: In): Render.PassPlan = planOf(w#scene)

    def planOf(scene: SceneView.In): Render.PassPlan =
        let stack = SortingLayer.stack(App.pickCtx(size()), Layer.Scene);
        PostView.chain(size(), scene#look, clearColor(), stack(ZStack.leaf(SceneView.scene(size(), scene) :: Nil)))

    def clearColor(): Color = Color.rgb(0.0, 0.0, 0.0)

    def layers(ctx: App.ViewCtx, w: Overlay): List[(Layer, ZStack.Tree[Layer, Unit])] =
        (Layer.Scene, ZStack.leaf(PostView.fxaaFill(size()) :: Nil))
            :: (Layer.Marks, ZStack.leaf(MarkView.marks(ctx#atlas, w#marks)))
            :: (Layer.Hud, ZStack.of(ZStack.leaf(HudView.edge(w#bars)) :: ZStack.leaf(HudView.bars(ctx#atlas, w#bars)) :: ZStack.leaf(HudView.card(ctx#atlas, w#bars)) :: Nil))
            :: (Layer.Cards, ZStack.leaf(CardView.cards(ctx#atlas, w#cards)))
            :: (Layer.Prompt, ZStack.of(ZStack.leaf(HudView.veil(ctx#atlas, w#bars)) :: ZStack.leaf(HudView.quit(ctx#atlas, w#bars)) :: Nil)) :: Nil

    // ── World から描く値へ ──

    /// 描く元と World から描くのに要る値だけを写す（View の中で World を読むのは入口と sceneOf・hudOf だけ）。
    pub def inputOf(res: Res.Res, w: World.World): In = { scene = sceneOf(res, w), hud = hudOf(res, w), clear = clearColor() }

    /// 1 手の見せ方の材料（時刻表 tl は遊びの時計 clock で読む）。
    def turnOf(board: Board.Board, tl: Timeline.Timeline[TurnTimeline.Tag], clock: Clock.Clock[World.Play], shown: World.Shown, now: Run.State): Motion.Turn =
        { board = board, before = Run.rec(shown#before), after = Run.rec(now), events = List.zipWithIndex(shown#events),
          progress = tag -> Timeline.progressOn(clock, shown#startedAt, tag, tl) }

    /// 3D の場面の値。
    def sceneOf(res: Res.Res, w: World.World): SceneView.In =
        let lk = LookDoc.rec(res#look);
        let time = Clock.secondsOf(w#realNow);
        let r = Run.rec(w#runState);
        let theme = ThemeDoc.rec(res#theme);
        let turn = turnOf(World.ctx(res, w)#board, World.timelineOf(res, w), w#clock, w#shown, w#runState);
        let pose = Motion.poseOf(turn, time, w#screen == World.Screen.Dead, lk#figureLight);
        let foresight = w#screen == World.Screen.Play and not World.isBusy(res, w);
        let preview = if (foresight) w#foresight#moves |> List.filterMap(e -> shownOf(r#hero#at, e)) else Nil;
        { time = time, camera = shaken(turn, World.cameraOf(res, w)), look = lk, floor = Res.floorAt(res, r#floor, r#layout),
          flame = Kit.itemOf("FLAME", res#kit), textures = res#kit#textures,
          hand = FloorMesh.cellPoint(r#hero#at), figures = pose#figures, bursts = pose#bursts,
          marks = trapMarks(theme, r) ::: (if (foresight) preview |> List.flatMap(markOf(theme)) ::: dangerMarks(theme, r#hero#at, w#foresight#danger) ::: shieldMarks(theme, r) ::: summonMarks(theme, r) else Nil),
          lanes = if (foresight) w#foresight#lanes |> List.map(laneOf(theme)) else Nil }

    /// 画面の揺れ（ボスの叩きつけ・倒れる）の分だけ目をずらしたカメラ（場面と、場面の上の印が同じ物を使う）。
    def shaken(turn: Motion.Turn, camera: Camera3d.Camera3d): Camera3d.Camera3d = { eye = Vec3.add(camera#eye, Motion.shakeOf(turn)) | camera }

    /// 2D に重ねる物の値（上と下の帯・場面の上の印・カード）。
    def hudOf(res: Res.Res, w: World.World): Overlay =
        let lk = LookDoc.rec(res#look);
        let turn = turnOf(World.ctx(res, w)#board, World.timelineOf(res, w), w#clock, w#shown, w#runState);
        let camera = shaken(turn, World.cameraOf(res, w));
        let time = Clock.secondsOf(w#realNow);
        let r = Run.rec(w#runState);
        let screen = (q: Vec3.Vec3) -> Camera3d.worldToScreen(camera, size(), q);
        let theme = ThemeDoc.rec(res#theme);
        let busy = World.isBusy(res, w);
        let foresight = w#screen == World.Screen.Play and not busy;
        let preview = if (foresight) w#foresight#moves |> List.filterMap(e -> shownOf(r#hero#at, e)) else Nil;
        let glow = if (busy) Motion.poseOf(turn, time, false, 1.0)#slotGlow else Map.empty();
        let aged = w#popups |> List.map(pop -> (pop, Clock.since(pop#startedAt, w#realNow))) |> List.filter(e -> snd(e) >= 0.0);
        let hpBar = Popup.barOf(World.timingOf(res), w#realNow, w#trail);
        let hero = screen(Vec3.add(FloorMesh.cellPoint(r#hero#at), Vec3.v3(0.0, 0.7, 0.0))) |> Option.getWithDefault({ x = World.width() / 2.0, y = World.height() / 2.0 });
        let rules = RulesDoc.rec(res#rules);
        let bars = { theme = theme, time = time,
          floorLabel = TextDoc.fill(("n", "${if (r#status == Run.Status.Descended) r#floor + 1 else r#floor}") :: Nil, TextDoc.lineOf(TextDoc.Line.FloorLabel, res#text)),
          hp = { now = r#hero#hp, max = r#hero#maxHp, front = hpBar#front, extra = hpBar#extra, healing = hpBar#healing },
          fullness = { now = r#fullness, max = rules#fullnessMax, glow = fade(lk#popupSeconds, Popup.Kind.Fed, aged), alarm = fade(lk#popupSeconds, Popup.Kind.Starved, aged) },
          exp = { level = r#level, now = r#exp, need = RulesDoc.expToNext(r#level, res#rules) },
          gear = gearOf(theme, r#hero),
          coins = r#coins,
          boss = r#enemies |> List.find(e -> Enemy.isBoss(e#kind) and Enemy.isAwake(e)) |> Option.map(e ->
              { name = EnemiesDoc.statsOf(e#kind, res#enemies)#name, now = e#hp, max = EnemiesDoc.statsOf(e#kind, res#enemies)#hp }),
          minimap = minimapOf(theme, World.ctx(res, w)#board, r),
          log = logLine(res, w#shown, w#runState), slots = slotsOf(res, theme, r#pack, glow),
          card = if (busy) None else cardOf(res, w#screen),
          quitPrompt = if (World.isAskingQuit(w)) Some(TextDoc.lineOf(TextDoc.Line.QuitPrompt, res#text)) else None,
          showBars = w#screen != World.Screen.Title,
          edge = fade(lk#edgeSeconds, Popup.Kind.Taken, aged),
          flash = thunderFlash(turn),
          veil = World.veilOf(res, w) };
        let marks = { theme = theme, time = time,
          timing = { life = lk#popupSeconds, popScale = lk#popupPopScale, popSeconds = lk#popupPopSeconds, rise = lk#popupRise, fall = lk#popupFall,
                     drift = lk#popupDrift, fadeSeconds = lk#popupFadeSeconds, healRise = lk#healRise, healPop = lk#healPopScale },
          badges = preview |> List.filterMap(badgeOf(theme, lk, screen)),
          sleepers = if (w#screen != World.Screen.Play) Nil else r#enemies |> List.filter(e -> not Enemy.isAwake(e))
              |> List.filterMap(e -> screen(Vec3.add(FloorMesh.cellPoint(e#at), Vec3.v3(0.0, Motion.heightOf(e#kind) + Motion.liftOf(e#kind) + 0.1, 0.0)))),
          numbers = aged |> List.filterMap(numberOf(screen, { stack = lk#popupStack, gap = lk#popupHeadGap }, headOf(r))),
          overheads = aged |> List.filterMap(overheadOf(screen)),
          uses = usesOf(screen, { side = lk#useIconSize, tilt = lk#useTilt, gap = lk#popupHeadGap }, turn),
          rings = aged |> List.filterMap(ringOf(screen)),
          slashes = slashesOf(theme, screen, turn),
          trails = trailsOf(theme, screen, turn),
          alerts = alertsOf(screen, turn),
          bolts = boltsOf(screen, turn) };
        let choosing = w#screen == World.Screen.Cards;
        let pending = if (choosing) Run.pendingOf(w#runState) |> List.head else None;
        let cards = { theme = theme, time = time,
          timing = { levelUp = lk#levelUpSeconds, slide = lk#cardSlideSeconds, chosen = lk#chosenSeconds },
          cards = pending |> Option.map(choice -> cardsOf(res, theme, r#pack, choice)) |> Option.getWithDefault(Nil),
          since = Clock.since(w#cardsAt, w#realNow),
          levelUp = pending |> Option.exists(isLevelUp),
          hero = hero,
          chosen = w#chosen |> Option.map(c -> { cards = cardsOf(res, theme, c#pack, c#choice), index = c#index,
                                                  since = Clock.since(c#startedAt, w#realNow), absorb = isLevelUp(c#choice) }) };
        { bars = bars, marks = marks, cards = cards }

    // ── 浮かぶ数・斬り跡・軌跡 ──

    /// 種類 kind の数のうち、出てから seconds 秒の間の強さ（出始め 1 → 0。無ければ 0）。
    def fade(seconds: Float64, kind: Popup.Kind, aged: List[(Popup.Popup, Float64)]): Float64 =
        aged |> List.filter(e -> fst(e)#kind == kind) |> List.map(e -> 1.0 - snd(e) / Float64.max(0.01, seconds))
            |> List.foldLeft(Float64.max, 0.0)

    /// マスの上の数の出始めの点: そのマスに立つ物の板の上端から gap だけ上・積んだ段の分だけ stack px ずつ上。
    def numberOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], space: { stack = Float64, gap = Float64 }, head: Board.Cell -> Float64,
                 e: (Popup.Popup, Float64)): Option[MarkView.Number] =
        let (pop, age) = e;
        screen(Vec3.add(FloorMesh.cellPoint(pop#cell), Vec3.v3(0.0, head(pop#cell) + space#gap, 0.0)))
            |> Option.map(at -> { at = { x = at#x, y = at#y - space#stack * Int32.toFloat64(pop#lift) }, kind = pop#kind, amount = pop#amount, lift = pop#lift, age = age })

    /// マスに立つ物の板の上端の高さ（m）: 主人公・生きている敵（飛ぶ敵は浮かせた分も）。倒れて消えた敵のマスはゼリーほどの高さ。
    def headOf(r: Run.Rec, cell: Board.Cell): Float64 =
        if (cell == r#hero#at) Motion.heroHeight()
        else r#enemies |> List.find(e -> e#at == cell) |> Option.map(e -> Motion.heightOf(e#kind) + Motion.liftOf(e#kind)) |> Option.getWithDefault(0.8)

    /// 頭の横に出す物: 盾の 0 は盾（数は頭の上へ昇るので、絵は左へ寄せる）。薬・食べ物の絵は usesOf が時刻表で動かす。
    def overheadOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], e: (Popup.Popup, Float64)): Option[MarkView.Overhead] =
        let (pop, age) = e;
        let pictured = if (pop#kind == Popup.Kind.Blocked) Some(Art.Icon.Shield) else None;
        pictured |> Option.flatMap(i -> screen(Vec3.add(FloorMesh.cellPoint(pop#cell), Vec3.v3(0.0, 1.3, 0.0)))
            |> Option.map(at -> { at = { x = at#x - 40.0, y = at#y }, icon = i, age = age }))

    // ── 使う物の演出（頭の上・遊びの時計の時刻表で動く） ──

    /// 0..1 に収める。
    def clamp01(x: Float64): Float64 = Float64.max(0.0, Float64.min(1.0, x))

    /// a..b の間の進み（前は 0・後は 1）。
    def span(a: Float64, b: Float64, p: Float64): Float64 = clamp01((p - a) / Float64.max(0.001, b - a))

    /// 頭の上の横で使っている物（薬・食べ物）。size = 絵の大きさと瓶の傾き（度）と頭の上の隙間（m）。
    def usesOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], size: { side = Float64, tilt = Float64, gap = Float64 }, turn: Motion.Turn): List[MarkView.Use] =
        // WhyNot: 頭の真上に置く — 受けた数が同じ所から積み上がり、瓶や肉に重なって読めない。
        let head = screen(Vec3.add(FloorMesh.cellPoint(turn#after#hero#at), Vec3.v3(0.0, Motion.heroHeight() + size#gap, 0.0)));
        let above = head |> Option.map(h -> { x = h#x - size#side * 0.95, y = h#y - size#side * 0.35 });
        turn#events |> List.filterMap(ie -> match ie {
            case (i, Run.Event.PotionUsed(_, _)) => above |> Option.flatMap(at -> potionUse(size, at, turn#progress((i, TurnTimeline.Beat.Potion))))
            case (i, Run.Event.Ate(kind, _))     => above |> Option.flatMap(at -> eatUse(size, at, kind, turn#progress((i, TurnTimeline.Beat.Eat))))
            case _                               => None
        })

    /// 薬の進み → (大きさの倍, 傾きの割合, 濃さ)。弾んで出て → 傾いて → 空の瓶に替わって → 小さくなって消える。
    def potionScaleKeys(): List[(Float64, Float64)] = (0.0, 0.5) :: (0.15, 1.0) :: (0.75, 1.0) :: (1.0, 0.4) :: Nil
    def potionTiltKeys(): List[(Float64, Float64)] = (0.0, 0.0) :: (0.15, 0.0) :: (0.45, 1.0) :: (1.0, 1.0) :: Nil
    def potionPourKeys(): List[(Float64, Float64)] = (0.0, 0.0) :: (0.4, 0.0) :: (0.68, 1.0) :: (1.0, 1.0) :: Nil
    def fadeKeys(holdUntil: Float64): List[(Float64, Float64)] = (0.0, 1.0) :: (holdUntil, 1.0) :: (1.0, 0.0) :: Nil
    def swapFlashKeys(at: Float64): List[(Float64, Float64)] = (0.0, 0.0) :: (at, 0.0) :: (at + 0.001, 1.0) :: (at + 0.12, 0.0) :: (1.0, 0.0) :: Nil

    /// 薬: 弾んで出て傾き、しずくを落として空の瓶に替わり、小さくなって消える。
    def potionUse(size: { side = Float64, tilt = Float64, gap = Float64 }, at: Vec2.Vec2, p: Float64): Option[MarkView.Use] =
        if (p <= 0.0 or p >= 1.0) None
        else {
            let swapAt = 0.6;
            let pour = Curve.pieces(potionPourKeys(), p);
            let mouth = { x = at#x + size#side * 0.35, y = at#y - size#side * 0.1 };
            let drop = (k: Int32) -> {
                let q = clamp01(pour * 1.6 - Int32.toFloat64(k) * 0.3);
                { at = { x = mouth#x + 4.0 * Int32.toFloat64(k), y = mouth#y + 50.0 * q }, radius = 4.5, color = Color.rgb(0.5, 1.0, 0.55),
                  alpha = if (q <= 0.0 or q >= 1.0) 0.0 else 1.0 - q }
            };
            Some({ at = at, icon = if (p < swapAt) Art.Icon.Herb else Art.Icon.BottleEmpty, side = size#side * Curve.pieces(potionScaleKeys(), p),
                   tilt = size#tilt / 360.0 * Curve.pieces(potionTiltKeys(), p), alpha = Curve.pieces(fadeKeys(0.75), p),
                   flash = Curve.pieces(swapFlashKeys(swapAt), p), crumbs = List.range(0, 3) |> List.map(drop) })
        }

    /// 食べ物の大きさの倍: 弾んで出て、骨・かじった形に替わる瞬間に少し大きく出て戻り、小さくなって消える。
    def eatScaleKeys(swapAt: Float64): List[(Float64, Float64)] =
        (0.0, 0.6) :: (0.1, 1.0) :: (swapAt, 1.0) :: (swapAt + 0.001, 1.25) :: (swapAt + 0.12, 1.0) :: (0.86, 1.0) :: (1.0, 0.5) :: Nil

    /// 食べ物: 小刻みに揺れながら 3 回噛まれて粒が飛び、骨（パンはかじった形）に替わって消える。
    /// 替わる瞬間は少し大きく出す（骨は肉より細く小さく、そのままだと消えたように見える）。
    /// WhyNot: 替わる瞬間を白く光らせる — 光の玉が骨より目立ち、食べ終えた絵が読めない。
    def eatUse(size: { side = Float64, tilt = Float64, gap = Float64 }, at: Vec2.Vec2, kind: Item.Kind, p: Float64): Option[MarkView.Use] =
        if (p <= 0.0 or p >= 1.0) None
        else {
            let meat = kind == Item.Kind.Meat;
            let swapAt = 0.72;
            let swapped = p >= swapAt;
            let crumbColor = if (meat) Color.rgb(0.55, 0.3, 0.17) else Color.rgb(0.78, 0.59, 0.24);
            let shake = if (swapped) 0.0 else Curve.sine(5.0, Num.pi() * 14.0, 0.0, p);
            let crumb = (bite: Float64, k: Int32) -> {
                let q = span(bite, bite + 0.25, p);
                let a = Num.pi() * (1.15 + 0.23 * Int32.toFloat64(k)) + bite;
                { at = { x = at#x + Num.cos(a) * 40.0 * q, y = at#y + Num.sin(a) * 34.0 * q + 46.0 * q * q }, radius = 4.0,
                  color = crumbColor, alpha = if (q <= 0.0 or q >= 1.0) 0.0 else 1.0 - q }
            };
            let bites = 0.03 :: 0.28 :: 0.53 :: Nil;
            Some({ at = { x = at#x + shake, y = at#y }, icon = if (swapped) (if (meat) Art.Icon.Bone else Art.Icon.BreadBitten) else Art.iconOfKind(kind),
                   side = size#side * Curve.pieces(eatScaleKeys(swapAt), p), tilt = 0.0, alpha = Curve.pieces(fadeKeys(0.86), p),
                   flash = 0.0,
                   crumbs = bites |> List.flatMap(b -> List.range(0, 4) |> List.map(k -> crumb(b, k))) })
        }

    /// 回復の輪（薬で HP が戻った時だけ）。
    def ringOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], e: (Popup.Popup, Float64)): Option[MarkView.Ring] =
        let (pop, age) = e;
        if (pop#kind != Popup.Kind.Healed) None
        else screen(FloorMesh.cellPoint(pop#cell)) |> Option.map(at -> { at = at, age = age })

    /// 体の真ん中の高さの点。
    def bodyOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], cell: Cell): Option[Vec2.Vec2] =
        screen(Vec3.add(FloorMesh.cellPoint(cell), Vec3.v3(0.0, 0.55, 0.0)))

    /// 斬り跡: 主人公が殴った敵の上に白い弧、殴られた主人公の上に赤い弧（向きは打った側から）。
    def slashesOf(t: ThemeDoc.Rec, screen: Vec3.Vec3 -> Option[Vec2.Vec2], turn: Motion.Turn): List[MarkView.Slash] =
        let foes = List.toMap(turn#before#enemies |> List.map(e -> (e#id, e#at)));
        let hero = turn#after#hero#at;
        let arc = (i: Int32, target: Cell, attacker: Cell, color: Color) -> {
            let progress = turn#progress((i, TurnTimeline.Beat.Flash));
            if (progress <= 0.0 or progress >= 1.0) None
            else bodyOf(screen, target) |> Option.flatMap(at -> bodyOf(screen, attacker) |> Option.map(from1 ->
                { at = at, toward = Num.atan2(from1#y - at#y, from1#x - at#x), color = color, progress = progress }))
        };
        turn#events |> List.filterMap(ie -> match ie {
            case (i, Run.Event.HeroStruck(id, _)) => Map.get(id, foes) |> Option.flatMap(foe -> arc(i, foe, hero, t#popDealt))
            case (i, Run.Event.HeroHurt(Run.Source.Melee(id), _, _)) => Map.get(id, foes) |> Option.flatMap(foe -> arc(i, hero, foe, t#popTaken))
            case (i, Run.Event.HeroHurt(Run.Source.Charge(id), _, _)) => Map.get(id, foes) |> Option.flatMap(foe -> arc(i, hero, foe, t#popTaken))
            case _ => None
        })

    /// 矢の軌跡: 主人公の矢は白、弓兵の矢は赤。小石は線を引かず、山なりに飛ぶ石の板だけで見せる。
    def trailsOf(t: ThemeDoc.Rec, screen: Vec3.Vec3 -> Option[Vec2.Vec2], turn: Motion.Turn): List[MarkView.Trail] =
        let hero = turn#after#hero#at;
        let line = (progress: Float64, start: Cell, end: Cell, color: Color) ->
            if (progress <= 0.0 or progress >= 1.0) None
            else bodyOf(screen, start) |> Option.flatMap(a -> bodyOf(screen, end) |> Option.map(b -> {
                let along = (k: Float64) -> { x = a#x + (b#x - a#x) * k, y = a#y + (b#y - a#y) * k };
                { tail = along(Float64.max(0.0, progress - 0.45)), head = along(progress), color = color }
            }));
        turn#events |> List.filterMap(ie -> match ie {
            case (_, Run.Event.ArrowShot(_, kind, _)) if Item.isStone(Item.familyOf(kind)) => None
            case (i, Run.Event.ArrowShot(_, _, end)) => line(turn#progress((i, TurnTimeline.Beat.Arrow)), hero, end, t#popDealt)
            case (i, Run.Event.EnemyShot(_, start, end)) => line(turn#progress((i, TurnTimeline.Beat.EnemyArrow)), start, end, t#popTaken)
            case _ => None
        })

    /// 警報で起きかけた敵の頭の「!」（手の後の位置）。
    def alertsOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], turn: Motion.Turn): List[MarkView.Alert] =
        let foes = List.toMap(turn#after#enemies |> List.map(e -> (e#id, e)));
        turn#events |> List.flatMap(ie -> match ie {
            case (i, Run.Event.AlarmRang(ids)) =>
                let progress = turn#progress((i, TurnTimeline.Beat.Alarm));
                if (progress <= 0.0 or progress >= 1.0) Nil
                else ids |> List.filterMap(id -> Map.get(id, foes)) |> List.filterMap(e ->
                    screen(Vec3.add(FloorMesh.cellPoint(e#at), Vec3.v3(0.0, Motion.heightOf(e#kind) + Motion.liftOf(e#kind) + 0.35, 0.0)))
                        |> Option.map(at -> { at = at, progress = progress }))
            case _ => Nil
        })

    /// かみなりの白い光（落ちた瞬間に 1 で、すぐ消える）。
    def thunderFlash(turn: Motion.Turn): Float64 =
        turn#events |> List.map(ie -> match ie {
            case (i, Run.Event.ThunderCast(_)) => Curve.pieces((0.0, 0.0) :: (0.05, 0.85) :: (0.35, 0.0) :: (1.0, 0.0) :: Nil, clamp01(turn#progress((i, TurnTimeline.Beat.Thunder))))
            case _                             => 0.0
        }) |> List.foldLeft(Float64.max, 0.0)

    /// かみなりの稲妻: 敵ごとに画面の上の方から体へ、折れ曲がった線（敵の id で折れ方を変える）。
    def boltsOf(screen: Vec3.Vec3 -> Option[Vec2.Vec2], turn: Motion.Turn): List[MarkView.Bolt] =
        let foes = List.toMap(turn#before#enemies |> List.map(e -> (e#id, e#at)));
        turn#events |> List.flatMap(ie -> match ie {
            case (i, Run.Event.ThunderCast(ids)) =>
                let progress = turn#progress((i, TurnTimeline.Beat.Thunder));
                if (progress <= 0.0 or progress >= 1.0) Nil
                else ids |> List.filterMap(id -> Map.get(id, foes) |> Option.flatMap(cell -> bodyOf(screen, cell)) |> Option.map(at -> {
                    let bend = (k: Int32) -> 14.0 * Num.sin(Int32.toFloat64(id * 7 + k * 3) * 1.7);
                    let points = List.range(0, 7) |> List.map(k -> {
                        let f = Int32.toFloat64(k) / 6.0;
                        { x = at#x + (if (k == 0 or k == 6) 0.0 else bend(k)), y = at#y - 260.0 * (1.0 - f) }
                    });
                    { points = points, alpha = Curve.pieces((0.0, 1.0) :: (0.5, 1.0) :: (1.0, 0.0) :: Nil, progress) }
                }))
            case _ => Nil
        })

    // ── カード ──

    def isLevelUp(choice: Run.Choice): Bool = match choice {
        case Run.Choice.LevelUp(_) => true
        case _                     => false
    }

    /// 選ぶ場面のカード: 捨てるカードは持っている枠（並び順）と足もとの物、レベルアップは候補。
    def cardsOf(res: Res.Res, t: ThemeDoc.Rec, pack: Run.Pack, choice: Run.Choice): List[CardView.Card] = match choice {
        case Run.Choice.Swap(kind) =>
            let held = pack |> List.map(h -> { face = CardView.Face.Picture(Art.iconOfKind(Run.kindOfHeld(h))), label = "×${Run.countOf(Some(h))}",
                                               cue = Some(cueOf(res, Run.familyOfHeld(h))), edge = tierColor(t, Some(Run.kindOfHeld(h))), fresh = false, drop = true });
            let found = { face = CardView.Face.Picture(Art.iconOfKind(kind)), label = "+${ItemsDoc.statsOf(kind, res#items)#count}",
                          cue = Some(cueOf(res, Item.familyOf(kind))), edge = t#cardNew, fresh = true, drop = true };
            held ::: (found :: Nil)
        case Run.Choice.LevelUp(perks) => perks |> List.map(perkCard(res, t))
    }

    /// レベルアップのカード 1 枚（絵と短い数・効きの絵）。
    def perkCard(res: Res.Res, t: ThemeDoc.Rec, perk: Run.Perk): CardView.Card =
        let amounts = CardsDoc.rec(res#cards);
        let make = (face: CardView.Face, label: String, cue: Option[PartsView.Cue]) -> { face = face, label = label, cue = cue, edge = t#cardEdge, fresh = false, drop = false };
        let pic = (i: Art.Icon) -> CardView.Face.Picture(i);
        match perk {
            case Run.Perk.MaxHp      => make(CardView.Face.Heart, "+${amounts#maxHp}", None)
            case Run.Perk.Attack     => make(pic(Art.Icon.Sword), "+${amounts#attack}", None)
            case Run.Perk.Reach      => make(pic(Art.Icon.Arrow), "+${amounts#reach}", Some(PartsView.Cue.Line(2)))
            case Run.Perk.LoneBomb   => make(pic(Art.Icon.Bomb), "", Some(PartsView.Cue.Surround(1)))
            case Run.Perk.Push       => make(pic(Art.Icon.Sword), "", Some(PartsView.Cue.Push))
            case Run.Perk.SlowHunger => make(pic(Art.Icon.Bread), "", Some(PartsView.Cue.Slow))
            case Run.Perk.Feast      => make(pic(Art.Icon.Meat), "+${amounts#feast}", None)
        }

    // ── 予告 ──

    /// 予告の主な種類（枠の色とアイコンを決める）。
    enum Act with Eq {
        case Move
        case Wait
        case Strike
        case Arrow
        case Pebble
        case Bomb
        case Smoke
        case Potion
        case Descend
        case Trap(Trap.Kind)
        case Guarded
        case Thunder
        case Doll
    }

    /// 見せる予告 1 つ: 入力・マス・主な種類・受けるダメージ。
    type alias Shown = { input = Run.Input, cell = Cell, act = Act, hurt = Int32 }

    def damageOf(m: Preview.Mark): Int32 = match m {
        case Preview.Mark.Damage(n) => n
        case _                      => 0
    }

    /// 要約の主な種類（進めない向きは None）。
    def actOf(summary: Preview.Summary): Option[Act] =
        let has = mark -> List.memberOf(mark, summary);
        if (has(Preview.Mark.Blocked)) None
        else if (has(Preview.Mark.Doll)) Some(Act.Doll)
        else if (Option.nonEmpty(trapIn(summary))) trapIn(summary) |> Option.map(Act.Trap)
        else if (has(Preview.Mark.Bomb)) Some(Act.Bomb)
        else if (has(Preview.Mark.Thunder)) Some(Act.Thunder)
        else if (has(Preview.Mark.Arrow)) Some(Act.Arrow)
        else if (has(Preview.Mark.IronBall) or has(Preview.Mark.Pebble)) Some(Act.Pebble)
        else if (has(Preview.Mark.Smoke)) Some(Act.Smoke)
        else if (has(Preview.Mark.Guarded)) Some(Act.Guarded)
        else if (has(Preview.Mark.Descend)) Some(Act.Descend)
        else if (has(Preview.Mark.Strike)) Some(Act.Strike)
        else if (has(Preview.Mark.Potion)) Some(Act.Potion)
        else if (has(Preview.Mark.Move)) Some(Act.Move)
        else Some(Act.Wait)

    /// 要約の中の踏む罠。
    def trapIn(summary: Preview.Summary): Option[Trap.Kind] = summary |> List.findMap(m -> match m {
        case Preview.Mark.Trap(kind) => Some(kind)
        case _                       => None
    })

    /// 予告を見せる形にする。進めない向き（枠を出す床が無い）と、何も起きない待つは出さない。
    def shownOf(hero: Cell, e: (Run.Input, Preview.Summary)): Option[Shown] =
        let (input, summary) = e;
        let hurt = summary |> List.map(damageOf) |> List.sum;
        let cell = match input {
            case Run.Input.Move(dir) => Board.step8(dir, hero)
            case _                   => hero
        };
        let plainWait = input == Run.Input.Wait and actOf(summary) == Some(Act.Wait) and hurt == 0;
        if (plainWait) None else actOf(summary) |> Option.map(a -> { input = input, cell = cell, act = a, hurt = hurt })

    def colorOf(t: ThemeDoc.Rec, act: Act): Color = match act {
        case Act.Move    => t#markMove
        case Act.Wait    => t#markMove
        case Act.Strike  => t#markStrike
        case Act.Arrow   => t#markArrow
        case Act.Pebble  => t#markArrow
        case Act.Bomb    => t#markBomb
        case Act.Smoke   => t#markBomb
        case Act.Potion  => t#markDescend
        case Act.Descend => t#markDescend
        case Act.Trap(_) => t#markTrap
        case Act.Guarded => t#markBlocked
        case Act.Thunder => t#markBomb
        case Act.Doll    => t#markDanger
    }

    def markOf(t: ThemeDoc.Rec, s: Shown): List[SceneView.Mark] =
        let at = FloorMesh.cellPoint(s#cell);
        if (s#input == Run.Input.Wait) Nil
        else { at = at, color = colorOf(t, s#act), stroke = SceneView.Stroke.Fill } :: { at = at, color = colorOf(t, s#act), stroke = SceneView.Stroke.Frame } :: Nil

    /// 敵の予告: 届くマスを赤く塗って破線で囲み、主人公のマスが入っていれば赤い枠。
    def dangerMarks(t: ThemeDoc.Rec, hero: Cell, danger: Set[Cell]): List[SceneView.Mark] =
        let fills = Set.toList(danger) |> List.flatMap(c ->
            { at = FloorMesh.cellPoint(c), color = t#markDanger, stroke = SceneView.Stroke.Fill } :: { at = FloorMesh.cellPoint(c), color = t#markDanger, stroke = SceneView.Stroke.Dashed } :: Nil);
        if (Set.memberOf(hero, danger)) { at = FloorMesh.cellPoint(hero), color = t#markDanger, stroke = SceneView.Stroke.Frame } :: fills else fills

    /// ボスが次の手番に仲間を呼ぶマス（Run.summonMarks）: 呼ぶ色で薄く塗ってふちを引く。
    /// WhyNot: 敵の予告と同じ破線にする — 体の隣のマスは赤の破線と重なり、どちらの印か読めない。
    def summonMarks(t: ThemeDoc.Rec, r: Run.Rec): List[SceneView.Mark] =
        Run.summonMarks(r#enemies) |> List.flatMap(c ->
            { at = FloorMesh.cellPoint(c), color = t#markSummon, stroke = SceneView.Stroke.Fill } :: { at = FloorMesh.cellPoint(c), color = t#markSummon, stroke = SceneView.Stroke.Frame } :: Nil)

    /// 盾持ちが防ぐ側: 足もとから向き facing の側へ、前と左右斜め前（Enemy.guards の 3 方向）にかかる弧を盾の色で描く。
    /// マスのふちを縁取るだけでは、奥を向いた時に体の板に隠れて前がどちらか読めなかった。
    def shieldMarks(t: ThemeDoc.Rec, r: Run.Rec): List[SceneView.Mark] =
        let origin = FloorMesh.cellPoint((0, 0));
        r#enemies |> List.filter(e -> e#kind == Enemy.Kind.Shield) |> List.map(e -> {
            let v = Vec3.sub(FloorMesh.cellPoint(Board.step8(e#facing, (0, 0))), origin);
            { at = FloorMesh.cellPoint(e#at), color = t#markShield, stroke = SceneView.Stroke.Guard(v#x, v#z) }
        })

    /// 見えている罠（踏まれた罠）の床: 罠の色の面（罠の絵の板は Motion が立てる）。
    def trapMarks(t: ThemeDoc.Rec, r: Run.Rec): List[SceneView.Mark] =
        Run.seenTraps(r) |> Map.keysOf |> Set.toList |> List.map(c -> { at = FloorMesh.cellPoint(c), color = t#markTrap, stroke = SceneView.Stroke.Fill })

    def laneOf(t: ThemeDoc.Rec, lane: (Cell, Cell)): SceneView.Lane =
        let (archer, far) = (FloorMesh.cellPoint(fst(lane)), FloorMesh.cellPoint(snd(lane)));
        let d = Vec3.sub(far, archer);
        let along = Vec3.mul(d, 1.0 / Float64.max(0.001, Vec3.length(d)));
        { origin = Vec3.add(archer, Vec3.mul(along, 0.4)), tip = Vec3.add(far, Vec3.mul(along, 0.3)), color = t#markDanger }

    /// アイコンの置き場のずれ（左右は板と重ならないよう横へ・待つは体の上に重ねる。そこのタップが待つになる）。
    def offsetOf(input: Run.Input): Vec2.Vec2 = match input {
        case Run.Input.Move(Dir8.Up)        => { x = 0.0, y = -16.0 }
        case Run.Input.Move(Dir8.UpRight)   => { x = 24.0, y = -14.0 }
        case Run.Input.Move(Dir8.Right)     => { x = 30.0, y = -10.0 }
        case Run.Input.Move(Dir8.DownRight) => { x = 24.0, y = 0.0 }
        case Run.Input.Move(Dir8.Down)      => { x = 0.0, y = 4.0 }
        case Run.Input.Move(Dir8.DownLeft)  => { x = -24.0, y = 0.0 }
        case Run.Input.Move(Dir8.Left)      => { x = -30.0, y = -10.0 }
        case Run.Input.Move(Dir8.UpLeft)    => { x = -24.0, y = -14.0 }
        case _                              => { x = 0.0, y = 0.0 }
    }

    def glyphOf(act: Act): MarkView.Glyph = match act {
        case Act.Move    => MarkView.Glyph.Steps
        case Act.Wait    => MarkView.Glyph.Hourglass
        case Act.Strike  => MarkView.Glyph.Picture(Art.Icon.Sword)
        case Act.Arrow   => MarkView.Glyph.Picture(Art.Icon.Arrow)
        case Act.Pebble  => MarkView.Glyph.Picture(Art.Icon.Pebble)
        case Act.Bomb    => MarkView.Glyph.Picture(Art.Icon.Bomb)
        case Act.Smoke   => MarkView.Glyph.Picture(Art.Icon.Smoke)
        case Act.Potion  => MarkView.Glyph.Picture(Art.Icon.Herb)
        case Act.Descend => MarkView.Glyph.Stairs
        case Act.Trap(kind) => MarkView.Glyph.Picture(Art.iconOfTrap(kind))
        case Act.Guarded => MarkView.Glyph.Picture(Art.Icon.Shield)
        case Act.Thunder => MarkView.Glyph.Bolt
        case Act.Doll    => MarkView.Glyph.Picture(Art.Icon.Doll)
    }

    /// 斜めの向きか（予告のアイコンを小さくする）。
    def isDiagonal(input: Run.Input): Bool = match input {
        case Run.Input.Move(Dir8.UpRight)   => true
        case Run.Input.Move(Dir8.DownRight) => true
        case Run.Input.Move(Dir8.DownLeft)  => true
        case Run.Input.Move(Dir8.UpLeft)    => true
        case _                              => false
    }

    def badgeOf(t: ThemeDoc.Rec, lk: LookDoc.Rec, screen: Vec3.Vec3 -> Option[Vec2.Vec2], s: Shown): Option[MarkView.Badge] =
        let k = lk#badgeScale * (if (isDiagonal(s#input)) lk#diagonalBadgeScale else 1.0);
        let lift = if (s#input == Run.Input.Wait) lk#waitBadgeLift else 0.25;
        screen(Vec3.add(FloorMesh.cellPoint(s#cell), Vec3.v3(0.0, lift, 0.0))) |> Option.map(at ->
            { at = Vec2.add(at, Vec2.mul(offsetOf(s#input), lk#badgeScale)), glyph = glyphOf(s#act), hurt = s#hurt, color = colorOf(t, s#act),
              scale = k, alpha = lk#badgeAlpha })

    // ── HUD ──

    /// 剣と盾の枠（持っている段のふちの色）。
    def gearOf(t: ThemeDoc.Rec, hero: Run.Hero): List[HudView.Gear] =
        let gear = (icon: Art.Icon, held: Option[Item.Kind]) -> { icon = icon, tier = tierColor(t, held), held = Option.nonEmpty(held) };
        gear(Art.Icon.Sword, hero#sword) :: gear(hero#shield |> Option.map(Art.iconOfKind) |> Option.getWithDefault(Art.Icon.WoodShield), hero#shield) :: Nil

    /// 段のふちの色（系統の中で何段目か）。
    def tierColor(t: ThemeDoc.Rec, kind: Option[Item.Kind]): Color = match kind {
        case None                        => t#tierNone
        case Some(Item.Kind.FireArrow)   => t#tierFire
        case Some(Item.Kind.IronSword)   => t#tierIron
        case Some(Item.Kind.IronShield)  => t#tierIron
        case Some(Item.Kind.IronArrow)   => t#tierIron
        case Some(Item.Kind.BigBomb)     => t#tierIron
        case Some(Item.Kind.Tonic)       => t#tierIron
        case Some(Item.Kind.SilverSword)  => t#tierSilver
        case Some(Item.Kind.SilverShield) => t#tierSilver
        case Some(Item.Kind.SilverArrow) => t#tierSilver
        case Some(Item.Kind.SleepBomb)   => t#tierSilver
        case Some(Item.Kind.Elixir)      => t#tierSilver
        case Some(_)                     => t#tierWood
    }

    /// 持ち物の系統の発動の条件の絵。
    def cueOf(res: Res.Res, family: Item.Family): PartsView.Cue =
        let rules = RulesDoc.rec(res#rules);
        match family {
            case Item.Family.Bomb   => PartsView.Cue.Surround(rules#bombNeeds)
            case Item.Family.Smoke  => PartsView.Cue.Surround(rules#bombNeeds)
            case Item.Family.Thunder => PartsView.Cue.Surround(rules#thunderNeeds)
            case Item.Family.Potion => PartsView.Cue.LowHp(rules#potionAt)
            case Item.Family.Doll   => PartsView.Cue.LowHp(0.0)
            case Item.Family.Pebble => PartsView.Cue.Line(1)
            case Item.Family.IronBall => PartsView.Cue.Line(1)
            case _                  => PartsView.Cue.Line(2)
        }

    /// 持ち物の 3 枠: 枠の列を並び順に描き、足りない所は空き枠。
    def slotsOf(res: Res.Res, t: ThemeDoc.Rec, pack: Run.Pack, glow: Map[Item.Family, Float64]): List[Option[HudView.Slot]] =
        let filled = pack |> List.map(held -> {
            let family = Run.familyOfHeld(held);
            Some({ icon = Art.iconOfKind(Run.kindOfHeld(held)), count = Run.countOf(Some(held)), cue = cueOf(res, family),
                   tier = tierColor(t, Some(Run.kindOfHeld(held))), glow = Map.getWithDefault(family, 0.0, glow) })
        });
        List.take(Run.slotCount(), filled ::: List.repeat(Run.slotCount(), None))

    /// 小さい地図のマスの色（壁は描かない）。
    def tileColor(t: ThemeDoc.Rec, tile: Board.Tile): Option[Color] = match tile {
        case Board.Tile.Wall   => None
        case Board.Tile.Water  => Some(Color.mix(t#mapWater, t#shade, 0.35))
        case Board.Tile.Stairs => Some(t#mapStairs)
        case Board.Tile.Floor  => Some(Color.mix(t#mapFloor, t#shade, 0.35))
        case _                 => Some(Color.mix(t#mapFloor, t#shade, 0.6))
    }

    /// 小さい地図: 見たマスだけ。主人公と、見たマスにいる敵は色を替える。
    def minimapOf(t: ThemeDoc.Rec, b: Board.Board, r: Run.Rec): HudView.Minimap =
        let foes = r#enemies |> List.map(e -> e#at) |> List.toSet;
        let colorAt = (c: Cell) ->
            if (c == r#hero#at) Some(t#mapHero)
            else if (Set.memberOf(c, foes)) Some(t#mapEnemy)
            else tileColor(t, Board.tileAt(c, b));
        { cols = b#cols, rows = b#rows,
          cells = Set.toList(r#seen) |> List.filterMap(c -> colorAt(c) |> Option.map(color -> (c, color))) }

    /// 記録: この手の出来事のうち、字にする物の最後の 1 行（無ければ空）。
    def logLine(res: Res.Res, shown: World.Shown, now: Run.State): String =
        let named = (runState: Run.State) -> Run.enemiesOf(runState) |> List.map(e -> (e#id, EnemiesDoc.statsOf(e#kind, res#enemies)#name));
        let names = List.toMap(named(shown#before) ::: named(now));
        shown#events |> List.filterMap(lineOf(res, names)) |> List.last |> Option.getWithDefault("")

    def lineOf(res: Res.Res, names: Map[Int32, String], event: Run.Event): Option[String] =
        let say = (line, args) -> Some(TextDoc.fill(args, TextDoc.lineOf(line, res#text)));
        let item = kind -> ("item", ItemsDoc.nameOf(kind, res#items));
        let enemy = id -> ("enemy", Map.getWithDefault(id, "?", names));
        match event {
            case Run.Event.Picked(kind, Item.Pick.Upgraded)  => say(TextDoc.Line.Upgraded, item(kind) :: Nil)
            case Run.Event.Picked(kind, Item.Pick.Stacked)   => say(TextDoc.Line.Stacked, item(kind) :: Nil)
            case Run.Event.Picked(kind, Item.Pick.Discarded) => say(TextDoc.Line.Discarded, item(kind) :: Nil)
            case Run.Event.HeroStruck(id, n)    => say(TextDoc.Line.Struck, enemy(id) :: ("n", "${n}") :: Nil)
            case Run.Event.ArrowShot(_, kind, _) => say(TextDoc.Line.Shot, item(kind) :: Nil)
            case Run.Event.BombThrown(kind, _)  => say(TextDoc.Line.Bombed, item(kind) :: Nil)
            case Run.Event.EnemyDied(id, _, _)  => say(TextDoc.Line.Defeated, enemy(id) :: Nil)
            case Run.Event.EnemyWoke(id)        => say(TextDoc.Line.Woke, enemy(id) :: Nil)
            case Run.Event.HeroHurt(_, _, true) => say(TextDoc.Line.Deflected, Nil)
            case Run.Event.HeroHurt(Run.Source.Starve, n, _) => say(TextDoc.Line.Starved, ("n", "${n}") :: Nil)
            case Run.Event.HeroHurt(Run.Source.Trap(Trap.Kind.Pit), n, _) => say(TextDoc.Line.Pitfall, ("n", "${n}") :: Nil)
            case Run.Event.HeroHurt(Run.Source.Trap(_), n, _) => say(TextDoc.Line.Darted, ("n", "${n}") :: Nil)
            case Run.Event.EnemyFell(id, _)     => say(TextDoc.Line.EnemyFell, enemy(id) :: Nil)
            case Run.Event.EnemyTrapped(id, n)  => say(TextDoc.Line.EnemyDarted, enemy(id) :: ("n", "${n}") :: Nil)
            case Run.Event.AlarmRang(_)         => say(TextDoc.Line.Alarmed, Nil)
            case Run.Event.ThunderCast(_)       => say(TextDoc.Line.Thundered, Nil)
            case Run.Event.Guarded(id)          => say(TextDoc.Line.ShieldBlocked, enemy(id) :: Nil)
            case Run.Event.EnemyWindup(id, Enemy.Intent.Fuse) => say(TextDoc.Line.Fused, enemy(id) :: Nil)
            case Run.Event.EnemyWindup(id, _)   => say(TextDoc.Line.Braced, enemy(id) :: Nil)
            case Run.Event.EnemyExploded(id, _) => say(TextDoc.Line.Exploded, enemy(id) :: Nil)
            case Run.Event.SummonMarked(id, _)  => say(TextDoc.Line.SummonReady, enemy(id) :: Nil)
            case Run.Event.EnemySummoned(id, _, _, _) => say(TextDoc.Line.Summoned, enemy(id) :: Nil)
            case Run.Event.HeroHurt(_, 0, _)    => say(TextDoc.Line.Guarded, Nil)
            case Run.Event.HeroHurt(source, n, _) => Run.attackerOf(source) |> Option.flatMap(id -> say(TextDoc.Line.Hurt, enemy(id) :: ("n", "${n}") :: Nil))
            case Run.Event.Ate(kind, n)         => say(TextDoc.Line.Ate, item(kind) :: ("n", "${n}") :: Nil)
            case Run.Event.Swapped(old, kind)   => say(TextDoc.Line.Swapped, ("old", ItemsDoc.nameOf(old, res#items)) :: item(kind) :: Nil)
            case Run.Event.LevelUp(level)       => say(TextDoc.Line.LevelUp, ("n", "${level}") :: Nil)
            case Run.Event.ItemDropped(kind, _) => say(TextDoc.Line.Dropped, item(kind) :: Nil)
            case Run.Event.Burned(id, _)        => say(TextDoc.Line.Burning, enemy(id) :: Nil)
            case Run.Event.PushedIntoWall(id, _) => say(TextDoc.Line.Slammed, enemy(id) :: Nil)
            case Run.Event.SmokeThrown(_)       => say(TextDoc.Line.Bombed, item(Item.Kind.Smoke) :: Nil)
            case Run.Event.PotionUsed(kind, n)  => say(TextDoc.Line.Drank, item(kind) :: ("n", "${n}") :: Nil)
            case Run.Event.DollSaved(_)         => say(TextDoc.Line.DollBroke, Nil)
            case Run.Event.CoinPicked(n)        => say(TextDoc.Line.CoinFound, ("n", "${n}") :: Nil)
            case Run.Event.Descended            => say(TextDoc.Line.Descended, Nil)
            case Run.Event.HeroBlocked(_)       => say(TextDoc.Line.Blocked, Nil)
            case _                              => None
        }

    /// 題名・倒れた・抜けたの板。
    def cardOf(res: Res.Res, screen: World.Screen): Option[HudView.Card] =
        let say = line -> TextDoc.lineOf(line, res#text);
        match screen {
            case World.Screen.Title => Some({ title = say(TextDoc.Line.Title), prompt = say(TextDoc.Line.StartPrompt) })
            case World.Screen.Dead  => Some({ title = say(TextDoc.Line.Dead), prompt = say(TextDoc.Line.Retry) })
            case World.Screen.Clear => Some({ title = say(TextDoc.Line.Cleared), prompt = say(TextDoc.Line.Retry) })
            case _                  => None
        }
}
