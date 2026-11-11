// View — World から「(置き場所, 見せたい物) のリスト」と、その前に描く Pass の計画への投影（純粋）。
//
// ── 画面の層（奥から手前へ）──
// 場面（Layer.Scene）: 斜め上から見下ろす 3D のダンジョン（SceneView）。Pass の中で 3D の場面とブルームを組み、
//   周辺を暗くしてトーンマップした面 1 枚を FXAA でならして画面へ置く（PostView）。
// 印（Layer.Marks）: 方向の予告のアイコン・眠っている敵の z（3D の点を Camera3d.worldToScreen で画面へ写した所）。
// HUD（Layer.Hud）: 上の帯（階・HP・剣と盾・小さい地図）・下の帯（記録・持ち物）・題名と終わりの板（HudView）。
// 終了の確認の窓（Layer.Prompt）: どれよりも手前。
mod View {

    use fge::{App, Camera3d, Clock, Color, Render, SortingLayer, Timeline, Vec2, Vec3, ZStack}
    use Board.{Cell, Dir8}
    use ZOrder.Layer

    /// 描くのに要る値。部品は World でなくこれを読む（lint/rules.flix の view-no-world）。
    pub type alias In = { scene = SceneView.In, hud = HudView.In, clear = Color }

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

    def layers(ctx: App.ViewCtx, w: HudView.In): List[(Layer, ZStack.Tree[Layer, Unit])] =
        (Layer.Scene, ZStack.leaf(PostView.fxaaFill(size()) :: Nil))
            :: (Layer.Marks, ZStack.leaf(HudView.marks(ctx#atlas, w)))
            :: (Layer.Hud, ZStack.leaf(List.append(HudView.bars(ctx#atlas, w), HudView.card(ctx#atlas, w))))
            :: (Layer.Prompt, ZStack.leaf(HudView.quit(ctx#atlas, w))) :: Nil

    // ── World から描く値へ ──

    /// 描く元と World から描くのに要る値だけを写す（View の中で World を読むのは入口と sceneOf・hudOf だけ）。
    pub def inputOf(res: Res.Res, w: World.World): In = { scene = sceneOf(res, w), hud = hudOf(res, w), clear = clearColor() }

    /// 1 手の見せ方の材料（時刻表 tl は遊びの時計 clock で読む）。
    def turnOf(tl: Timeline.Timeline[TurnTimeline.Tag], clock: Clock.Clock[World.Play], shown: World.Shown, now: Run.State): Motion.Turn =
        { before = Run.rec(shown#before), after = Run.rec(now), events = List.zipWithIndex(shown#events),
          progress = tag -> Timeline.progressOn(clock, shown#startedAt, tag, tl) }

    /// 3D の場面の値。
    def sceneOf(res: Res.Res, w: World.World): SceneView.In =
        let lk = LookDoc.rec(res#look);
        let time = Clock.secondsOf(w#realNow);
        let r = Run.rec(w#runState);
        let theme = ThemeDoc.rec(res#theme);
        let pose = Motion.poseOf(turnOf(World.timelineOf(res, w), w#clock, w#shown, w#runState), time, w#screen == World.Screen.Dead, lk#figureLight);
        let foresight = w#screen == World.Screen.Play and not World.isBusy(res, w);
        let preview = if (foresight) w#foresight#moves |> List.filterMap(e -> shownOf(r#hero#at, e)) else Nil;
        { time = time, camera = World.cameraOf(res, w), look = lk, floor = Res.floorAt(res, r#floor, r#layout),
          flame = Kit.itemOf("FLAME", res#kit), textures = res#kit#textures,
          hand = FloorMesh.cellPoint(r#hero#at), figures = pose#figures, bursts = pose#bursts,
          marks = if (foresight) List.append(preview |> List.flatMap(markOf(theme)), dangerMarks(theme, r#hero#at, w#foresight#danger)) else Nil,
          lanes = if (foresight) w#foresight#lanes |> List.map(laneOf(theme)) else Nil }

    /// 2D の印と HUD の値。
    def hudOf(res: Res.Res, w: World.World): HudView.In =
        let camera = World.cameraOf(res, w);
        let time = Clock.secondsOf(w#realNow);
        let r = Run.rec(w#runState);
        let screen = (q: Vec3.Vec3) -> Camera3d.worldToScreen(camera, size(), q);
        let theme = ThemeDoc.rec(res#theme);
        let busy = World.isBusy(res, w);
        let foresight = w#screen == World.Screen.Play and not busy;
        let preview = if (foresight) w#foresight#moves |> List.filterMap(e -> shownOf(r#hero#at, e)) else Nil;
        let glow = if (busy) Motion.poseOf(turnOf(World.timelineOf(res, w), w#clock, w#shown, w#runState), time, false, 1.0)#slotGlow else Map.empty();
        { theme = theme, time = time,
          floorLabel = TextDoc.fill(("n", "${r#floor}") :: Nil, TextDoc.lineOf(TextDoc.Line.FloorLabel, res#text)),
          hp = r#hero#hp, maxHp = r#hero#maxHp, gear = gearOf(theme, r#hero),
          minimap = minimapOf(theme, World.ctx(res, w)#board, r),
          log = logLine(res, w#shown, w#runState), slots = slotsOf(res, theme, r, glow),
          badges = preview |> List.filterMap(badgeOf(theme, screen)),
          sleepers = if (w#screen != World.Screen.Play) Nil else r#enemies |> List.filter(e -> not Enemy.isAwake(e))
              |> List.filterMap(e -> screen(Vec3.add(FloorMesh.cellPoint(e#at), Vec3.v3(0.0, Motion.heightOf(e#kind) + Motion.liftOf(e#kind) + 0.1, 0.0)))),
          card = if (busy) None else cardOf(res, w#screen),
          quitPrompt = if (World.isAskingQuit(w)) Some(TextDoc.lineOf(TextDoc.Line.QuitPrompt, res#text)) else None,
          showBars = w#screen != World.Screen.Title }

    // ── 予告 ──

    /// 予告の主な種類（枠の色とアイコンを決める）。
    enum Act with Eq {
        case Move
        case Wait
        case Strike
        case Arrow
        case Bomb
        case Potion
        case Descend
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
        else if (has(Preview.Mark.Arrow)) Some(Act.Arrow)
        else if (has(Preview.Mark.Bomb)) Some(Act.Bomb)
        else if (has(Preview.Mark.Descend)) Some(Act.Descend)
        else if (has(Preview.Mark.Strike)) Some(Act.Strike)
        else if (has(Preview.Mark.Potion)) Some(Act.Potion)
        else if (has(Preview.Mark.Move)) Some(Act.Move)
        else Some(Act.Wait)

    /// 予告を見せる形にする。進めない向き（枠を出す床が無い）と、何も起きない待つは出さない。
    def shownOf(hero: Cell, e: (Run.Input, Preview.Summary)): Option[Shown] =
        let (input, summary) = e;
        let hurt = summary |> List.map(damageOf) |> List.sum;
        let cell = match input {
            case Run.Input.Move(dir) => Board.step8(dir, hero)
            case Run.Input.Wait      => hero
        };
        let plainWait = input == Run.Input.Wait and actOf(summary) == Some(Act.Wait) and hurt == 0;
        if (plainWait) None else actOf(summary) |> Option.map(a -> { input = input, cell = cell, act = a, hurt = hurt })

    def colorOf(t: ThemeDoc.Rec, act: Act): Color = match act {
        case Act.Move    => t#markMove
        case Act.Wait    => t#markMove
        case Act.Strike  => t#markStrike
        case Act.Arrow   => t#markArrow
        case Act.Bomb    => t#markBomb
        case Act.Potion  => t#markDescend
        case Act.Descend => t#markDescend
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

    def laneOf(t: ThemeDoc.Rec, lane: (Cell, Cell)): SceneView.Lane =
        let (archer, far) = (FloorMesh.cellPoint(fst(lane)), FloorMesh.cellPoint(snd(lane)));
        let d = Vec3.sub(far, archer);
        let along = Vec3.mul(d, 1.0 / Float64.max(0.001, Vec3.length(d)));
        { origin = Vec3.add(archer, Vec3.mul(along, 0.4)), tip = Vec3.add(far, Vec3.mul(along, 0.3)), color = t#markDanger }

    /// アイコンの置き場のずれ（左右は板と重ならないよう横へ・待つは頭の上）。
    def offsetOf(input: Run.Input): Vec2.Vec2 = match input {
        case Run.Input.Move(Dir8.Up)        => { x = 0.0, y = -16.0 }
        case Run.Input.Move(Dir8.UpRight)   => { x = 24.0, y = -14.0 }
        case Run.Input.Move(Dir8.Right)     => { x = 30.0, y = -10.0 }
        case Run.Input.Move(Dir8.DownRight) => { x = 24.0, y = 0.0 }
        case Run.Input.Move(Dir8.Down)      => { x = 0.0, y = 4.0 }
        case Run.Input.Move(Dir8.DownLeft)  => { x = -24.0, y = 0.0 }
        case Run.Input.Move(Dir8.Left)      => { x = -30.0, y = -10.0 }
        case Run.Input.Move(Dir8.UpLeft)    => { x = -24.0, y = -14.0 }
        case Run.Input.Wait                 => { x = 0.0, y = -118.0 }
    }

    def glyphOf(act: Act): HudView.Glyph = match act {
        case Act.Move    => HudView.Glyph.Steps
        case Act.Wait    => HudView.Glyph.Hourglass
        case Act.Strike  => HudView.Glyph.Picture(Art.Icon.Sword)
        case Act.Arrow   => HudView.Glyph.Picture(Art.Icon.Arrow)
        case Act.Bomb    => HudView.Glyph.Picture(Art.Icon.Bomb)
        case Act.Potion  => HudView.Glyph.Picture(Art.Icon.Herb)
        case Act.Descend => HudView.Glyph.Stairs
    }

    def badgeOf(t: ThemeDoc.Rec, screen: Vec3.Vec3 -> Option[Vec2.Vec2], s: Shown): Option[HudView.Badge] =
        screen(Vec3.add(FloorMesh.cellPoint(s#cell), Vec3.v3(0.0, 0.25, 0.0))) |> Option.map(at ->
            { at = Vec2.add(at, offsetOf(s#input)), glyph = glyphOf(s#act), hurt = s#hurt, color = colorOf(t, s#act) })

    // ── HUD ──

    /// 剣と盾の枠（持っている段のふちの色）。
    def gearOf(t: ThemeDoc.Rec, hero: Run.Hero): List[HudView.Gear] =
        let gear = (icon: Art.Icon, held: Option[Item.Kind]) -> { icon = icon, tier = tierColor(t, held), held = Option.nonEmpty(held) };
        gear(Art.Icon.Sword, hero#sword) :: gear(Art.Icon.Shield, hero#shield) :: Nil

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

    /// 持ち物の枠 3 つ: 持っている枠を拾った順に並べ、空いた所は持っていない系統を薄く出す。
    def slotsOf(res: Res.Res, t: ThemeDoc.Rec, r: Run.Rec, glow: Map[Item.Family, Float64]): List[HudView.Slot] =
        let rules = RulesDoc.rec(res#rules);
        let cueOf = (family: Item.Family) -> match family {
            case Item.Family.Bomb   => HudView.Cue.Surround(rules#bombNeeds)
            case Item.Family.Potion => HudView.Cue.LowHp(rules#potionAt)
            case _                  => HudView.Cue.Line
        };
        let slot = (family: Item.Family, held: Option[Run.Held]) -> {
            let kind = Run.kindOf(held);
            { icon = Art.iconOfFamily(family), count = Run.countOf(held), cue = cueOf(family), tier = tierColor(t, kind),
              held = Option.nonEmpty(kind), glow = Map.getWithDefault(family, 0.0, glow) }
        };
        let filled = r#pack |> List.map(held -> slot(Run.familyOfHeld(held), Some(held)));
        let empty = (Item.Family.Arrow :: Item.Family.Bomb :: Item.Family.Potion :: Nil)
            |> List.filter(family -> Option.isEmpty(Run.heldOf(family, r#pack)))
            |> List.map(family -> slot(family, None));
        List.take(3, filled ::: empty)

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
            case Run.Event.HeroHurt(source, n, _) => Run.attackerOf(source) |> Option.flatMap(id -> say(TextDoc.Line.Hurt, enemy(id) :: ("n", "${n}") :: Nil))
            case Run.Event.PotionUsed(kind, n)  => say(TextDoc.Line.Drank, item(kind) :: ("n", "${n}") :: Nil)
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
