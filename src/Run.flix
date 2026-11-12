// Run — 決まりの状態（1 回の潜りの今）と、1 手の入力・出来事・判定に使う Doc の束（すべて純粋）。
// 1 手を進めるのは Step、要約と予告は Preview、ダッシュは Dash、階を作るのは Floor。
// World は見せる状態で、Run だけが進行の正（fingerprint と flow の比べも Run だけを見る）。
mod Run {

    use Board.{Cell, Dir, Dir8}

    /// 1 手の入力。Choose は選ぶ場面（pending の先頭）のカードの何枚目か（手は進めない）。
    pub enum Input with Eq, Order, ToString {
        case Move(Dir8)
        case Wait
        case Choose(Int32)
    }

    /// 選ぶ場面。
    ///   Swap(kind)     … 枠が埋まっている時に 4 種類目の kind の上に立った。カードは持っている枠（並び順）と、足もとの kind（拾わない）
    ///   LevelUp(cards) … レベルが上がった。カードは候補（積んだ時に乱数で引いた）
    pub enum Choice with Eq, ToString {
        case Swap(Item.Kind)
        case LevelUp(List[Perk])
    }

    /// レベルアップのカード（量は cards の Doc）。
    ///   MaxHp      … 最大 HP と今の HP が増える
    ///   Attack     … 攻撃が増える
    ///   Reach      … 矢と小石の届く距離が延びる
    ///   LoneBomb   … 隣の起きた敵が 1 体でも爆弾を投げる（一度だけ）
    ///   Push       … 殴ると 1 マス押し出す（一度だけ。銀の剣とは重ならない）
    ///   SlowHunger … 満腹度の減りが遅くなる
    ///   Feast      … 食べ物で戻る満腹度が増える
    pub enum Perk with Eq, Order, ToString {
        case MaxHp
        case Attack
        case Reach
        case LoneBomb
        case Push
        case SlowHunger
        case Feast
    }

    /// 全部のカード（候補を引く並び）。
    pub def perks(): List[Perk] = Perk.MaxHp :: Perk.Attack :: Perk.Reach :: Perk.LoneBomb :: Perk.Push :: Perk.SlowHunger :: Perk.Feast :: Nil

    /// 一度取ったら候補から外れるカードか（重ねても効きが変わらない）。
    pub def isOnce(perk: Perk): Bool = perk == Perk.LoneBomb or perk == Perk.Push or perk == Perk.SlowHunger

    /// 持ち物の枠の数。
    pub def slotCount(): Int32 = 3

    /// 予告を作る 9 つの入力（8 方向＋待つ）。
    pub def inputs(): List[Input] = (Board.dirs8() |> List.map(Input.Move)) ::: (Input.Wait :: Nil)

    /// 潜りの状態。Playing の外では手を受けない。
    ///   Descended … 階段で降りた（次の階は Floor.enter で作る）
    ///   Cleared   … 最後の階（rules の lastFloor）の階段で降りた
    pub enum Status with Eq, ToString {
        case Playing
        case Dead
        case Descended
        case Cleared
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

    /// 持ち物の枠の列（拾った順・系統ごとに 1 枠・slotCount まで）。0 個になった枠は列から外す。
    pub type alias Pack = List[Held]

    /// 決まりの状態。
    ///   floor   … 階（1 から）
    ///   layout  … その階の地図の番号（floors の Doc の maps の何番目か）
    ///   enemies … 生きている敵（作った順）
    ///   items   … 床のアイテム
    ///   traps   … 罠のマス（見えていない物も入る）
    ///   revealed … 見えている罠のマス（主人公か敵が一度踏んだ。階を出るまで見えたまま）
    ///   seen    … 見たマス（霧）
    ///   rng     … 乱数の状態
    ///   seed    … 周回の種（階の種はここと階の番号から作る）
    ///   walked  … 移動した手の数（自然回復の数え）
    ///   turns   … 手が進んだ数（空腹の数え。つかえた手・降りる手は数えない）
    ///   fullness … 満腹度（0 の間は 1 手ごとに HP が減り、自然回復は止まる）
    ///   level・exp … レベルと、今のレベルで貯めた経験値（次までは RulesDoc.expToNext）
    ///   perks    … 取ったカード（取った順）
    ///   pending  … 選ぶ場面の列（先頭から 1 つずつ。ある間は Choose 以外の入力は何もしない）
    ///   declined … 捨てるカードで拾わなかった物のマス（そのマスを離れたら消す）
    pub type alias Rec = {
        floor = Int32,
        layout = Int32,
        hero = Hero,
        pack = Pack,
        enemies = List[Enemy.Enemy],
        items = Map[Cell, Item.Kind],
        traps = Map[Cell, Trap.Kind],
        revealed = Set[Cell],
        seen = Set[Cell],
        rng = Int64,
        seed = Int64,
        perks = List[Perk],
        level = Int32,
        exp = Int32,
        walked = Int32,
        turns = Int32,
        fullness = Int32,
        pending = List[Choice],
        declined = Option[Cell],
        nextId = Int32,
        status = Status
    }

    /// 型検査が record を展開しないため、World が持つ形は名前付きの enum にする。
    pub enum State(Rec)

    pub def rec(runState: State): Rec = match runState { case State.State(r) => r }

    pub def of(r: Rec): State = State.State(r)

    /// 1 手の判定に使う物の束（今の階の盤と Doc）。
    pub type alias Ctx = { rules = RulesDoc.Doc, items = ItemsDoc.Doc, enemies = EnemiesDoc.Doc, cards = CardsDoc.Doc, board = Board.Board }

    /// 1 手で起きたこと（起きた順）。敵は id で指す。
    pub enum Event with Eq, ToString {
        /// 主人公が from から to へ動いた。
        case HeroMoved(Cell, Cell)
        /// 主人公がその向きへ進めなかった（手は進まない）。
        case HeroBlocked(Dir8)
        case HeroWaited
        case Descended
        case Picked(Item.Kind, Item.Pick)
        /// 主人公が敵を殴った（敵の id・ダメージ）。
        case HeroStruck(Int32, Int32)
        /// 主人公が矢か小石を放った（向き・種類・止まったマス）。
        case ArrowShot(Dir, Item.Kind, Cell)
        case ArrowHit(Int32, Int32)
        /// 主人公が爆弾を投げた（段・中心のマス）。
        case BombThrown(Item.Kind, Cell)
        case BombHit(Int32, Int32)
        /// 主人公がかみなりの書を使った（当てる敵の id）。続く ThunderHit がそれぞれの当たり。
        case ThunderCast(List[Int32])
        case ThunderHit(Int32, Int32)
        /// 主人公が煙玉を使った（主人公のマス）。続く EnemySlept が眠った隣の敵。
        case SmokeThrown(Cell)
        /// 燃えている敵が敵の手番の始めにダメージを受けた（id・ダメージ）。
        case EnemyBurned(Int32, Int32)
        /// 敵が燃え始めた（id・燃える手の数）。
        case Burned(Int32, Int32)
        /// 敵が眠った（id・眠る手の数）。
        case EnemySlept(Int32, Int32)
        case EnemyPushed(Int32, Cell, Cell)
        case EnemyDied(Int32, Enemy.Kind, Cell)
        case EnemyWoke(Int32)
        case EnemyMoved(Int32, Cell, Cell)
        /// 弓兵が矢を撃った（id・撃ったマス・届いたマス）。続く HeroHurt がその当たり。
        case EnemyShot(Int32, Cell, Cell)
        /// 主人公が攻撃を受けた（出所・盾で減らした後のダメージ・弾いたか）。弾いた時は HP は減らない。
        case HeroHurt(Source, Int32, Bool)
        case PotionUsed(Item.Kind, Int32)
        /// 主人公が足もとの食べ物を食べた（種類・戻った満腹度）。
        case Ate(Item.Kind, Int32)
        /// 捨てるカードで持っていた物を捨て、足もとの物を拾った（捨てた物・拾った物）。
        case Swapped(Item.Kind, Item.Kind)
        /// 倒した敵の経験値が入った（量）。
        case ExpGained(Int32)
        /// レベルが上がった（新しいレベル）。続けてカードの pending が積まれる。
        case LevelUp(Int32)
        /// 倒した敵が物を落とした（種類・マス）。
        case ItemDropped(Item.Kind, Cell)
        /// 主人公か敵が入ったマスの罠が動いた（種類・マス）。そのマスの罠はこの後ずっと見える。主人公へのダメージは続く HeroHurt、敵へは EnemyTrapped、警報は AlarmRang、落とし穴は FellThrough・EnemyFell。
        case TrapSprung(Trap.Kind, Cell)
        /// 敵が罠でダメージを受けた（敵の id・ダメージ）。
        case EnemyTrapped(Int32, Int32)
        /// 警報で、眠っていた敵（id）が起きかけた（その手の敵の番では動かない）。
        case AlarmRang(List[Int32])
        /// 盾持ちが正面からの殴る・矢・石を盾で防いだ（敵の id）。前の HeroStruck・ArrowHit のダメージは 0。
        case Guarded(Int32)
        /// 敵が次の手番の技を溜めた（敵の id・溜め）。その手は動かず、当てない。
        case EnemyWindup(Int32, Enemy.Intent)
        /// 突進がまっすぐ走った（敵の id・走る前のマス・止まったマス）。主人公に当たれば続く HeroHurt。
        case EnemyCharged(Int32, Cell, Cell)
        /// 自爆虫が弾けて消えた（敵の id・マス）。続く BombHit が巻き込まれた敵、HeroHurt が主人公への当たり。経験値は入らない。
        case EnemyExploded(Int32, Cell)
        /// HP が 0 以下になった主人公の身代わりに人形が砕け、HP がこの値になった（倒れない）。
        case DollSaved(Int32)
        /// 主人公が落とし穴から次の階へ落ちた（その手はここで終わる。最後の階なら Cleared）。
        case FellThrough
        /// 敵が落とし穴に落ちて階から消えた（敵の id・マス）。経験値は入り、物は落とさない。
        case EnemyFell(Int32, Cell)
        /// 押し出しの先が壁・柱・岩で、押せずにダメージが増えた（敵の id・足したダメージ）。続く出来事で HP が減る。
        case PushedIntoWall(Int32, Int32)
        /// カードを取った。
        case PerkTaken(Perk)
        case Regenerated(Int32)
        case HeroDied
    }

    /// 主人公が受けたダメージの出所。
    ///   Melee(id) … 隣の敵に殴られた
    ///   Arrow(id) … 弓兵の矢が当たった
    ///   Charge(id) … 突進に当たった
    ///   Blast(id) … 自爆虫の弾けに巻き込まれた
    ///   Starve    … 満腹度が 0 で減った
    ///   Trap(kind) … 罠を踏んだ
    pub enum Source with Eq, ToString {
        case Melee(Int32)
        case Arrow(Int32)
        case Charge(Int32)
        case Blast(Int32)
        case Starve
        case Trap(Trap.Kind)
    }

    /// 出所の敵の id（空腹は None）。
    pub def attackerOf(source: Source): Option[Int32] = match source {
        case Source.Melee(id) => Some(id)
        case Source.Arrow(id) => Some(id)
        case Source.Charge(id) => Some(id)
        case Source.Blast(id) => Some(id)
        case Source.Starve    => None
        case Source.Trap(_)   => None
    }

    /// 主人公が敵の攻撃を受けた出来事か（弾いた攻撃も含む・空腹は入れない）。
    pub def hurtsHero(event: Event): Bool = match event {
        case Event.HeroHurt(source, _, _) => Option.nonEmpty(attackerOf(source))
        case _                            => false
    }

    /// 階の始めの置き場。
    pub type alias Placement = { enemies = List[(Enemy.Kind, Cell)], items = List[(Item.Kind, Cell)], traps = Map[Cell, Trap.Kind] }

    /// 階の番号と、その階を作った乱数。
    pub type alias Arrival = { floor = Int32, layout = Int32, seed = Int64, rng = Int64 }

    /// 潜りの始め（1F）: 主人公は全快・満腹で何も持たない。
    pub def start(ctx: Ctx, at: Arrival, placement: Placement): State =
        let rules = RulesDoc.rec(ctx#rules);
        let fresh = { floor = at#floor, layout = at#layout,
                      hero = { at = ctx#board#start, hp = rules#maxHp, maxHp = rules#maxHp, sword = None, shield = None },
                      pack = Nil, enemies = Nil, items = Map.empty(), traps = Map.empty(), revealed = Set.empty(), seen = Set.empty(), rng = at#rng, seed = at#seed,
                      perks = Nil, level = 1, exp = 0, walked = 0, turns = 0, fullness = rules#fullnessMax,
                      pending = Nil, declined = None, nextId = 0, status = Status.Playing };
        enter(of(fresh), ctx, at, placement)

    /// 次の階へ（Doc を保存した時は同じ階を作り直す）: 主人公の HP・満腹度・経験値とレベル・カード・枠・剣と盾・レベルアップの選ぶ場面・数えた手は持ち越す。
    /// 主人公は始まりの印に立ち、敵は全員眠っている。床の物・罠・霧・捨てるカード・拾わなかった物のマスは前の階の物なので捨てる。
    pub def enter(prev: State, ctx: Ctx, at: Arrival, placement: Placement): State =
        let r = rec(prev);
        let enemies = placement#enemies |> List.zipWithIndex |> List.map(match (i, (kind, cell)) ->
            Enemy.make(i, kind, cell, EnemiesDoc.statsOf(kind, ctx#enemies)#hp));
        let start = ctx#board#start;
        of({ floor = at#floor, layout = at#layout, hero = { at = start | r#hero },
             enemies = enemies, items = List.toMap(placement#items |> List.map(match (kind, cell) -> (cell, kind))), traps = placement#traps, revealed = Set.empty(),
             seen = Board.viewFrom(start, ctx#board), rng = at#rng, seed = at#seed,
             pending = r#pending |> List.filter(isLevelUp), declined = None, nextId = List.length(enemies), status = Status.Playing | r })

    /// レベルアップの選ぶ場面か。
    pub def isLevelUp(choice: Choice): Bool = match choice {
        case Choice.LevelUp(_) => true
        case Choice.Swap(_)    => false
    }

    // ── 読む口 ──

    pub def heroOf(runState: State): Hero = rec(runState)#hero

    pub def packOf(runState: State): Pack = rec(runState)#pack

    pub def enemiesOf(runState: State): List[Enemy.Enemy] = rec(runState)#enemies

    pub def statusOf(runState: State): Status = rec(runState)#status

    /// 取ったカード perk の数。
    pub def perkCount(perk: Perk, r: Rec): Int32 = r#perks |> List.count(p -> p == perk)

    /// 選ぶ場面の列。
    pub def pendingOf(runState: State): List[Choice] = rec(runState)#pending

    /// 枠の系統。
    pub def familyOfHeld(held: Held): Item.Family = match held {
        case Held.Held(kind, _) => Item.familyOf(kind)
    }

    /// 系統 family の枠（無ければ None）。
    pub def heldOf(family: Item.Family, pack: Pack): Option[Held] = List.find(held -> familyOfHeld(held) == family, pack)

    /// 枠を差し替える: 同じ系統の枠があればその位置へ、無ければ列の後ろへ。数が 0 以下の枠は外す。
    pub def withHeld(held: Held, pack: Pack): Pack =
        let family = familyOfHeld(held);
        let kept = if (countOf(Some(held)) <= 0) Nil else held :: Nil;
        if (Option.isEmpty(heldOf(family, pack))) pack ::: kept
        else pack |> List.flatMap(h -> if (familyOfHeld(h) == family) kept else h :: Nil)

    /// 系統 family の枠を 1 つ減らす（0 になったら外す）。
    pub def spend(family: Item.Family, pack: Pack): Pack = match heldOf(family, pack) {
        case Some(Held.Held(kind, n)) => withHeld(Held.Held(kind, n - 1), pack)
        case None                     => pack
    }

    /// 枠の種類。
    pub def kindOfHeld(held: Held): Item.Kind = match held {
        case Held.Held(kind, _) => kind
    }

    /// 枠の数（無ければ 0）。
    pub def countOf(held: Option[Held]): Int32 = match held {
        case Some(Held.Held(_, n)) => n
        case None                  => 0
    }

    /// 見えている罠（見えていない罠は入れない）。
    pub def seenTraps(r: Rec): Map[Cell, Trap.Kind] = r#traps |> Map.filterWithKey((c, _) -> Set.memberOf(c, r#revealed))

    /// 主人公に見えている物だけの Run（見えていない罠を除く）。方向の予告と自動で遊ぶ人の読みはこれで step を回す。
    pub def asSeen(runState: State): State = { let r = rec(runState); of({ traps = seenTraps(r) | r }) }

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

    /// 敵の攻撃の種類。
    ///   Melee  … 隣を殴る
    ///   Shot   … 弓兵の矢
    ///   Charge … 溜めた突進が走って当たる
    ///   Blast  … 導火線の付いた自爆虫が弾ける
    pub enum Attack with Eq, ToString {
        case Melee
        case Shot
        case Charge
        case Blast
    }

    /// e が今の位置から cell へ当てる攻撃（届かなければ None）。敵の手番（Step）と敵の予告（Preview）はどちらもこれだけを見る。
    ///   殴る … e のどれかのマスの 8 近傍の隣で、間の角が抜けている（自爆虫は殴らない）
    ///   矢   … 弓兵で、上下左右の列がそろい距離 arrowMin..arrowMax、間に矢を止めるマスも blockers も無い
    ///   突進 … 溜めた向きの走る道（chargeLane）の上。溜めている手は隣でも殴らない
    ///   弾け … 導火線の付いた自爆虫の殴れる隣と同じマス（壁と、壁の角の向こうは入らない）
    /// blockers は矢と突進を止める敵のマス。予告は敵が動いた後も当たる側に倒すので空で渡す。
    pub def attackOn(ctx: Ctx, blockers: Set[Cell], e: Enemy.Enemy, cell: Cell): Option[Attack] = match e#intent {
        case Enemy.Intent.Idle =>
            if (Enemy.strikesBeside(e#kind) and isBeside(ctx, e, cell)) Some(Attack.Melee)
            else if (Enemy.behaviourOf(e#kind) == Enemy.Behaviour.Shoot and isInLine(ctx, blockers, e#at, cell)) Some(Attack.Shot)
            else None
        case Enemy.Intent.Charge(dir) => if (List.memberOf(cell, chargeLane(ctx, blockers, e, dir))) Some(Attack.Charge) else None
        case Enemy.Intent.Fuse        => if (isBeside(ctx, e, cell)) Some(Attack.Blast) else None
    }

    /// 突進が dir へ走る道（e の隣から近い順）。歩けないマスか blockers の手前で止まり、長さは chargeMax + 1 まで。
    /// 走る長さ（Step）と赤い線（Preview）はどちらもこれだけを見る。主人公は道を止めない（道の上にいれば手前で止まって当たる）。
    pub def chargeLane(ctx: Ctx, blockers: Set[Cell], e: Enemy.Enemy, dir: Dir): List[Cell] =
        let longest = EnemiesDoc.statsOf(e#kind, ctx#enemies)#chargeMax + 1;
        List.range(1, longest + 1) |> List.map(k -> Board.ahead(dir, k, e#at))
            |> List.takeWhile(c -> Board.isWalkable(c, ctx#board) and not Set.memberOf(c, blockers))

    /// 突進が溜める向き: 主人公と上下左右の列がそろい、距離 chargeMin..chargeMax で、間が歩けて blockers も無い（隣は殴るので溜めない）。
    pub def chargeAim(ctx: Ctx, blockers: Set[Cell], e: Enemy.Enemy, cell: Cell): Option[Dir] =
        let stats = EnemiesDoc.statsOf(e#kind, ctx#enemies);
        Board.lineTo(e#at, cell) |> Option.filter(match (dir, k) ->
            k >= Int32.max(2, stats#chargeMin) and k <= stats#chargeMax
                and List.range(1, k) |> List.forAll(i -> { let between = Board.ahead(dir, i, e#at); Board.isWalkable(between, ctx#board) and not Set.memberOf(between, blockers) }))
            |> Option.map(fst)

    /// origin から cell へ上下左右の列がそろい、距離 arrowMin..arrowMax で、間に矢を止めるマスも blockers も無いか。
    def isInLine(ctx: Ctx, blockers: Set[Cell], origin: Cell, cell: Cell): Bool =
        let rules = RulesDoc.rec(ctx#rules);
        match Board.lineTo(origin, cell) {
            case Some((dir, k)) if k >= rules#arrowMin and k <= rules#arrowMax =>
                List.range(1, k) |> List.forAll(i -> {
                    let between = Board.ahead(dir, i, origin);
                    not Board.stopsArrow(between, ctx#board) and not Set.memberOf(between, blockers)
                })
            case _ => false
        }

    /// cell が e のどれかのマスの 8 近傍の隣で、間の角が抜けているか（e が殴れる隣）。
    pub def isBeside(ctx: Ctx, e: Enemy.Enemy, cell: Cell): Bool =
        Enemy.cellsOf(e) |> Set.exists(c -> c != cell and Board.isStepOpen(c, cell, ctx#board))

    /// 起きていて主人公に気付ける敵がいるか（ダッシュが止まる・1 歩だけになる）。
    pub def seesAwakeEnemy(ctx: Ctx, runState: State): Bool =
        let r = rec(runState);
        let dist = Board.walkDistances(r#hero#at, ctx#board);
        r#enemies |> List.exists(e -> Enemy.isAwake(e) and notices(ctx, dist, r#hero#at, e))

    /// 進行に効く欄の指紋（記録の再生・flow が比べる）。
    pub def fingerprint(runState: State): String =
        let r = rec(runState);
        let enemies = r#enemies |> List.map(e -> "${e#id}:${e#kind}@${e#at}/${e#hp}/${e#rest}/t${e#turns}/b${e#burn}/${e#facing}/${e#intent}") |> List.join(",");
        "F${r#floor}.${r#layout}|${r#status}|hero=${r#hero#at}/${r#hero#hp}|sword=${r#hero#sword}|shield=${r#hero#shield}|pack=${r#pack}|enemies=${enemies}|items=${r#items}|traps=${r#traps}|revealed=${r#revealed}|walked=${r#walked}|turns=${r#turns}|fullness=${r#fullness}|pending=${r#pending}|declined=${r#declined}|level=${r#level}/${r#exp}|perks=${r#perks}"
}
