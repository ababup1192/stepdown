// Shots — ギャラリーの場面（make render-all の 1 枚）の World を組む。描き出し（SceneRender）と実機（Game の App.withShots。
// START=shot:<場面>）が同じ関数で組むので、実機で開いた場面はギャラリーの絵と同じ World から始まる。
// WhyNot: 描き出しの物（HeadlessRender・HeadlessFont・java.io）を引かない — Game.app から engine_tools へつながり、ブラウザ版で組めなくなる。
mod Shots {

    use Board.Cell

    /// 描き出せる場面の名前。make render の使い方も SHOT の当たり判定も、この並びが source of truth。
    /// 場面の題と一言（Studio のギャラリーに出るラベル）は assets/stepdown.scenes.json 側で、name をここの名前とそろえる。
    /// 場面を足したら、ここと sceneOf と Makefile の SHOTS を一緒に足す。
    pub def names(): List[String] = "title" :: "play" :: "dead" :: "clear" :: "quit" :: "arrow" :: "bomb" :: Nil

    /// 名前 → 周回の種を固定した初めの World から組んだ場面の World（1 手を見せ終えた所。途中の場面はその秒の所）。
    pub def worldOf(name: String, res: Res.Res): Option[World.World] =
        let lk = LookDoc.rec(res#look);
        let w0 = World.initial(res, World.fixedSeed());
        match name {
            // 主人公の上 3 マスの起きたゼリーへ、待った手で矢を放ち、矢が半ばまで飛んだ所。
            case "arrow" => Some(midTurn(res, lk#arrowSeconds * 0.5,
                staged(res, (3, 13), Run.Held.Held(Item.Kind.WoodArrow, 6) :: Nil,
                       (Enemy.Kind.Jelly, (3, 10)) :: Nil, World.begin(res, w0))))
            // 起きたゼリー 2 体に左右の斜めから囲まれ、待った手で爆弾が破裂して光と煙が広がっている所。
            case "bomb"  => Some(midTurn(res, lk#arrowSeconds * 0.9,
                staged(res, (7, 15), Run.Held.Held(Item.Kind.SmallBomb, 2) :: Nil,
                       (Enemy.Kind.Jelly, (6, 14)) :: (Enemy.Kind.Jelly, (8, 14)) :: Nil, World.begin(res, w0))))
            case _       => sceneOf(name, res, w0) |> Option.map(w -> World.snapCam(World.skipShow(w)))
        }

    /// 待った手を当て、見せ始めてから seconds 秒の所にする。
    def midTurn(res: Res.Res, seconds: Float64, w: World.World): World.World =
        World.play(res, Run.Input.Wait, w) |> World.seekShow(seconds) |> World.snapCam

    /// 遊んでいる 1F の主人公を hero に立たせ、持ち物を pack にし、敵を起きた foes だけにする（床のアイテムは片付ける）。
    def staged(res: Res.Res, hero: Cell, pack: Run.Pack, foes: List[(Enemy.Kind, Cell)], w: World.World): World.World =
        let r = Run.rec(w#runState);
        let enemies = foes |> List.zipWithIndex |> List.map(match (i, (kind, cell)) ->
            { id = i, kind = kind, at = cell, size = 1, hp = EnemiesDoc.statsOf(kind, res#enemies)#hp,
              rest = Enemy.Rest.Awake, turns = 0, burn = 0 });
        { runState = Run.of({ hero = { at = hero | r#hero }, pack = pack, enemies = enemies, items = Map.empty(), nextId = List.length(enemies) | r }) | w }

    /// 名前 → その場面の World。names と対で保つ。
    def sceneOf(name: String, res: Res.Res, w0: World.World): Option[World.World] =
        let playing = World.begin(res, w0);
        let stairs = stairsOf(World.ctx(res, playing)#board);
        match name {
            case "title" => Some(w0)
            // 1F を始めて階段へ向かい、初めて敵が起きた所（敵の予告と方向の予告が出る）。
            case "play"  => Some(walkTo(res, stairs, w -> Run.seesAwakeEnemy(World.ctx(res, w), w#runState), 200, playing))
            // 階段へ向かう途中、敵が起きた所から待ち続けて倒れた。
            case "dead"  => Some(waitOut(res, 400, walkTo(res, stairs, w -> Run.seesAwakeEnemy(World.ctx(res, w), w#runState), 200, playing)))
            // 隣の起きた敵を殴りながら階段まで歩いて待った（試作は 1F で抜ける）。
            case "clear" => Some(walkTo(res, stairs, _ -> false, 400, playing) |> World.play(res, Run.Input.Wait))
            case "quit"  => Some(World.askQuit(walkTo(res, stairs, w -> Run.seesAwakeEnemy(World.ctx(res, w), w#runState), 200, playing)))
            case _       => None
        }

    def stairsOf(b: Board.Board): Cell =
        Board.cells(b) |> List.find(c -> Board.tileAt(c, b) == Board.Tile.Stairs) |> Option.getWithDefault(b#start)

    /// target へ歩く。隣に起きた敵がいれば先に殴る。着いた・stop が真・遊びが終わった・budget を使い切ったら止まる。
    def walkTo(res: Res.Res, target: Cell, stop: World.World -> Bool, budget: Int32, w: World.World): World.World =
        let b = World.ctx(res, w)#board;
        let dist = Board.walkDistances(target, b);
        let at = Run.heroOf(w#runState)#at;
        let here = Map.getWithDefault(at, 0, dist);
        if (budget <= 0 or here == 0 or stop(w) or w#screen != World.Screen.Play) w
        else nextDir(b, dist, here, w) |> Option.map(dir -> walkTo(res, target, stop, budget - 1, World.play(res, Run.Input.Move(dir), w)))
            |> Option.getWithDefault(w)

    /// 隣（角を抜けられる）の起きた敵への向き、いなければ歩数が 1 少ない隣への向き。
    def nextDir(b: Board.Board, dist: Map[Cell, Int32], here: Int32, w: World.World): Option[Board.Dir8] =
        let at = Run.heroOf(w#runState)#at;
        let awake = Run.enemiesOf(w#runState) |> List.filter(Enemy.isAwake) |> List.map(e -> e#at) |> List.toSet;
        let reachable = Board.dirs8() |> List.filter(dir -> Board.isCornerOpen(at, Board.step8(dir, at), b));
        let foe = reachable |> List.find(dir -> Set.memberOf(Board.step8(dir, at), awake));
        foe |> Option.withDefault(default = reachable |> List.find(dir -> Map.get(Board.step8(dir, at), dist) == Some(here - 1)))

    /// 倒れるか count 手まで待つ。
    def waitOut(res: Res.Res, count: Int32, w: World.World): World.World =
        if (count <= 0 or w#screen != World.Screen.Play) w else waitOut(res, count - 1, World.play(res, Run.Input.Wait, w))
}
