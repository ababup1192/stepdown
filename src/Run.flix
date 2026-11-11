// Run — 決まりの状態（1 回の潜りの今）と、1 手の入力・出来事・判定に使う Doc の束（すべて純粋）。
// 1 手を進めるのは Step、要約と予告は Preview、ダッシュは Dash、階を作るのは Floor。
// World は見せる状態で、Run だけが進行の正（fingerprint と flow の比べも Run だけを見る）。
mod Run {

    use Board.{Cell, Dir}

    /// 1 手の入力。
    pub enum Input with Eq, Order, ToString {
        case Move(Dir)
        case Wait
    }

    /// 予告を作る 5 つの入力（4 方向＋待つ）。
    pub def inputs(): List[Input] = (Board.dirs() |> List.map(Input.Move)) ::: (Input.Wait :: Nil)

    /// 潜りの状態。Dead と Descended では手を受けない。
    pub enum Status with Eq, ToString {
        case Playing
        case Dead
        case Descended
    }

    /// 主人公。sword・shield は持っている段（無ければ None）。
    pub type alias Hero = {
        at = Cell,
        hp = Int32,
        maxHp = Int32,
        sword = Option[Item.Kind],
        shield = Option[Item.Kind]
    }

    /// 数を持つ持ち物の 1 枠（段と数）。
    pub enum Held with Eq, ToString {
        case Held(Item.Kind, Int32)
    }

    /// 持ち物（矢・爆弾・薬）。拾っていない枠は None。
    pub type alias Pack = { arrows = Option[Held], bombs = Option[Held], potions = Option[Held] }

    /// 補正（カードの効果）。攻撃と守りに足す。
    pub type alias Perks = { attack = Int32, guard = Int32 }

    /// 決まりの状態。
    ///   floor   … 階（1 から）
    ///   layout  … その階の地図の番号（floors の Doc の maps の何番目か）
    ///   enemies … 生きている敵（作った順）
    ///   items   … 床のアイテム
    ///   seen    … 見たマス（霧）
    ///   rng     … 乱数の状態
    ///   seed    … 周回の種（階の種はここと階の番号から作る）
    ///   walked  … 移動した手の数（自然回復の数え）
    pub type alias Rec = {
        floor = Int32,
        layout = Int32,
        hero = Hero,
        pack = Pack,
        enemies = List[Enemy.Enemy],
        items = Map[Cell, Item.Kind],
        seen = Set[Cell],
        rng = Int64,
        seed = Int64,
        perks = Perks,
        walked = Int32,
        nextId = Int32,
        status = Status
    }

    /// 型検査が record を展開しないため、World が持つ形は名前付きの enum にする。
    pub enum State(Rec)

    pub def rec(runState: State): Rec = match runState { case State.State(r) => r }

    pub def of(r: Rec): State = State.State(r)

    /// 1 手の判定に使う物の束（今の階の盤と Doc）。
    pub type alias Ctx = { rules = RulesDoc.Doc, items = ItemsDoc.Doc, enemies = EnemiesDoc.Doc, board = Board.Board }

    /// 1 手で起きたこと（起きた順）。敵は id で指す。
    pub enum Event with Eq, ToString {
        /// 主人公が from から to へ動いた。
        case HeroMoved(Cell, Cell)
        /// 主人公がその向きへ進めなかった（手は進まない）。
        case HeroBlocked(Dir)
        case HeroWaited
        case Descended
        case Picked(Item.Kind, Item.Pick)
        /// 主人公が敵を殴った（敵の id・ダメージ）。
        case HeroStruck(Int32, Int32)
        /// 主人公が矢を放った（向き・段・矢が止まったマス）。
        case ArrowShot(Dir, Item.Kind, Cell)
        case ArrowHit(Int32, Int32)
        /// 主人公が爆弾を投げた（段・中心のマス）。
        case BombThrown(Item.Kind, Cell)
        case BombHit(Int32, Int32)
        case EnemyBurned(Int32, Int32)
        /// 敵が眠った（id・眠る手の数）。
        case EnemySlept(Int32, Int32)
        case EnemyPushed(Int32, Cell, Cell)
        case EnemyDied(Int32, Enemy.Kind, Cell)
        case EnemyWoke(Int32)
        case EnemyMoved(Int32, Cell, Cell)
        /// 弓兵が矢を撃った（id・撃ったマス・届いたマス）。続く HeroHurt がその当たり。
        case EnemyShot(Int32, Cell, Cell)
        /// 主人公が攻撃を受けた（敵の id・盾で減らした後のダメージ・弾いたか）。弾いた時は HP は減らない。
        case HeroHurt(Int32, Int32, Bool)
        case PotionUsed(Item.Kind, Int32)
        case Regenerated(Int32)
        case HeroDied
    }

    /// 主人公が攻撃を受けた出来事か（弾いた攻撃も含む）。
    pub def hurtsHero(event: Event): Bool = match event {
        case Event.HeroHurt(_, _, _) => true
        case _                       => false
    }

    /// 階の始めの置き場。
    pub type alias Placement = { enemies = List[(Enemy.Kind, Cell)], items = List[(Item.Kind, Cell)] }

    /// 階の始めの決まりの状態（主人公は始まりの印に全快で立ち、敵は全員眠っている）。
    pub def start(ctx: Ctx, at: { floor = Int32, layout = Int32, seed = Int64, rng = Int64 }, placement: Placement): State =
        let maxHp = RulesDoc.rec(ctx#rules)#maxHp;
        let enemies = placement#enemies |> List.zipWithIndex |> List.map(match (i, (kind, cell)) ->
            { id = i, kind = kind, at = cell, size = 1, hp = EnemiesDoc.statsOf(kind, ctx#enemies)#hp,
              rest = Enemy.Rest.Dormant, turns = 0, burn = 0 });
        let start = ctx#board#start;
        of({ floor = at#floor, layout = at#layout,
                  hero = { at = start, hp = maxHp, maxHp = maxHp, sword = None, shield = None },
                  pack = { arrows = None, bombs = None, potions = None },
                  enemies = enemies, items = List.toMap(placement#items |> List.map(match (kind, cell) -> (cell, kind))),
                  seen = Board.viewFrom(start, ctx#board), rng = at#rng, seed = at#seed,
                  perks = { attack = 0, guard = 0 }, walked = 0, nextId = List.length(enemies), status = Status.Playing })

    // ── 読む口 ──

    pub def heroOf(runState: State): Hero = rec(runState)#hero

    pub def packOf(runState: State): Pack = rec(runState)#pack

    pub def enemiesOf(runState: State): List[Enemy.Enemy] = rec(runState)#enemies

    pub def statusOf(runState: State): Status = rec(runState)#status

    /// 枠の数（無ければ 0）。
    pub def countOf(held: Option[Held]): Int32 = match held {
        case Some(Held.Held(_, n)) => n
        case None                  => 0
    }

    /// 枠の段（無ければ None）。
    pub def kindOf(held: Option[Held]): Option[Item.Kind] = match held {
        case Some(Held.Held(kind, _)) => Some(kind)
        case None                     => None
    }

    /// 敵の占めるマス → 敵。
    pub def occupancy(enemies: List[Enemy.Enemy]): Map[Cell, Enemy.Enemy] =
        enemies |> List.foldLeft((acc, e) -> Set.foldLeft((m, c) -> Map.insert(c, e, m), acc, Enemy.cellsOf(e)), Map.empty())

    /// 主人公が e に気付かれる位置にいるか: 同じ部屋（番号が同じ・0 でない）か、歩く距離（dist は主人公からの歩数）が見える距離以内。
    pub def notices(ctx: Ctx, dist: Map[Cell, Int32], heroAt: Cell, e: Enemy.Enemy): Bool =
        let room = Board.roomAt(heroAt, ctx#board);
        let near = RulesDoc.rec(ctx#rules)#wakeSteps;
        Enemy.cellsOf(e) |> Set.exists(c ->
            (room != 0 and Board.roomAt(c, ctx#board) == room)
                or Map.get(c, dist) |> Option.exists(steps -> steps <= near))

    /// 起きていて主人公に気付ける敵がいるか（ダッシュが止まる・1 歩だけになる）。
    pub def seesAwakeEnemy(ctx: Ctx, runState: State): Bool =
        let r = rec(runState);
        let dist = Board.walkDistances(r#hero#at, ctx#board);
        r#enemies |> List.exists(e -> Enemy.isAwake(e) and notices(ctx, dist, r#hero#at, e))

    /// 進行に効く欄の指紋（記録の再生・flow が比べる）。
    pub def fingerprint(runState: State): String =
        let r = rec(runState);
        let enemies = r#enemies |> List.map(e -> "${e#id}:${e#kind}@${e#at}/${e#hp}/${e#rest}") |> List.join(",");
        "F${r#floor}.${r#layout}|${r#status}|hero=${r#hero#at}/${r#hero#hp}|sword=${r#hero#sword}|shield=${r#hero#shield}|pack=${r#pack#arrows},${r#pack#bombs},${r#pack#potions}|enemies=${enemies}|items=${r#items}|walked=${r#walked}"
}
