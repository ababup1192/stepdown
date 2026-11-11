// Step — 1 手を Run に当てる（純粋）。
//   1. 主人公の手: 敵がいれば殴る（移動しない）・歩けなければ何も起きない（手は進まない）・それ以外は動いて拾う。
//      待つは「その場に動いた」とみなし、階段の上なら降りる（その手はそこで終わる）。
//   2. 動いたか待った後、その位置で 1 つだけ判定する: 爆弾（8 近傍の起きた敵が bombNeeds 以上）→ 矢（直線の射程に敵）。
//   3. 気付いた敵が起きる → 敵の手番（主人公への歩く距離の近い順 → 作った順）。受けるたびに薬・倒れるを見る。
//   4. 移動した手だけ数え、regenEvery ごとに HP +1。
mod Step {

    use fge::Sort
    use Board.{Cell, Dir}
    use Run.{Ctx, Event, Input, Status}

    /// 1 手の途中の状態。log は起きた順の逆（finish で反転する）。
    type alias Turn = { runState = Run.Rec, log = List[Event] }

    /// 1 手。Playing でない Run には何もしない。
    pub def step(ctx: Ctx, runState: Run.State, input: Input): (Run.State, List[Event]) =
        let r = Run.rec(runState);
        if (r#status != Status.Playing) (runState, Nil)
        else match input {
            case Input.Wait      => waitHere(ctx, r)
            case Input.Move(dir) => moveTo(ctx, dir, r)
        }

    /// 入力の列を順に当てる（最後の Run と、出来事を全部つないだ列）。
    pub def steps(ctx: Ctx, runState: Run.State, inputs: List[Input]): (Run.State, List[Event]) =
        let (last, logs) = inputs |> List.foldLeft((acc, input) -> {
            let (current, log) = acc;
            let (next, events) = step(ctx, current, input);
            (next, events :: log)
        }, (runState, Nil));
        (last, logs |> List.reverse |> List.flatten)

    def begin(r: Run.Rec): Turn = { runState = r, log = Nil }

    def emit(event: Event, t: Turn): Turn = { log = event :: t#log | t }

    def withRun(r: Run.Rec, t: Turn): Turn = { runState = r | t }

    /// 手の終わり: 今の位置から見えるマスを見たことにする。
    def finish(ctx: Ctx, t: Turn): (Run.State, List[Event]) =
        let r = t#runState;
        (Run.of({ seen = Set.union(r#seen, Board.viewFrom(r#hero#at, ctx#board)) | r }), List.reverse(t#log))

    // ══════════════════════════════════════════════════════
    // 主人公の手
    // ══════════════════════════════════════════════════════

    def waitHere(ctx: Ctx, r: Run.Rec): (Run.State, List[Event]) =
        if (Board.tileAt(r#hero#at, ctx#board) == Board.Tile.Stairs)
            finish(ctx, begin({ status = Status.Descended | r }) |> emit(Event.Descended))
        else begin(r) |> emit(Event.HeroWaited) |> judge(ctx) |> enemyPhase(ctx) |> finish(ctx)

    def moveTo(ctx: Ctx, dir: Dir, r: Run.Rec): (Run.State, List[Event]) =
        let target = Board.step(dir, r#hero#at);
        match Map.get(target, Run.occupancy(r#enemies)) {
            case Some(e) => begin(r) |> strike(ctx, dir, e#id) |> enemyPhase(ctx) |> finish(ctx)
            case None if not Board.isWalkable(target, ctx#board) => (Run.of(r), Event.HeroBlocked(dir) :: Nil)
            case None => begin(r) |> walk(target) |> pickUp(ctx) |> judge(ctx) |> enemyPhase(ctx) |> regen(ctx) |> finish(ctx)
        }

    def walk(target: Cell, t: Turn): Turn =
        let r = t#runState;
        t |> withRun({ hero = { at = target | r#hero }, walked = r#walked + 1 | r }) |> emit(Event.HeroMoved(r#hero#at, target))

    /// 主人公の攻撃（剣があれば剣の攻撃、無ければ素の攻撃）に補正を足した物。
    def attackOf(ctx: Ctx, r: Run.Rec): Int32 =
        let base = r#hero#sword |> Option.map(kind -> ItemsDoc.statsOf(kind, ctx#items)#attack)
            |> Option.getWithDefault(RulesDoc.rec(ctx#rules)#baseAttack);
        base + r#perks#attack

    def strike(ctx: Ctx, dir: Dir, id: Int32, t: Turn): Turn =
        let r = t#runState;
        let damage = attackOf(ctx, r);
        let struck = t |> emit(Event.HeroStruck(id, damage)) |> hurt(id, damage);
        if (Option.flatMap(Item.effectOf, r#hero#sword) == Some(Item.Effect.Push)) push(ctx, dir, id, struck) else struck

    /// 殴った敵を dir へ 1 マス押す（押し先に入れなければ押さない）。
    def push(ctx: Ctx, dir: Dir, id: Int32, t: Turn): Turn =
        enemyOf(id, t) |> Option.map(e -> {
            let dest = Board.step(dir, e#at);
            if (canStand(ctx, e, dest, t#runState)) t |> setEnemy({ at = dest | e }) |> emit(Event.EnemyPushed(id, e#at, dest))
            else t
        }) |> Option.getWithDefault(t)

    // ══════════════════════════════════════════════════════
    // 拾う（上位互換）
    // ══════════════════════════════════════════════════════

    def pickUp(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        Map.get(r#hero#at, r#items) |> Option.map(kind -> {
            let (taken, how) = take(ctx, kind, r);
            t |> withRun({ items = Map.remove(r#hero#at, r#items) | taken }) |> emit(Event.Picked(kind, how))
        }) |> Option.getWithDefault(t)

    /// 拾ったアイテムを手持ちへ。
    def take(ctx: Ctx, kind: Item.Kind, r: Run.Rec): (Run.Rec, Item.Pick) =
        let pack = r#pack;
        match Item.familyOf(kind) {
            case Item.Family.Sword  => let (held, how) = equip(kind, r#hero#sword); ({ hero = { sword = held | r#hero } | r }, how)
            case Item.Family.Shield => let (held, how) = equip(kind, r#hero#shield); ({ hero = { shield = held | r#hero } | r }, how)
            case Item.Family.Arrow  => let (held, how) = stack(ctx, kind, pack#arrows); ({ pack = { arrows = held | pack } | r }, how)
            case Item.Family.Bomb   => let (held, how) = stack(ctx, kind, pack#bombs); ({ pack = { bombs = held | pack } | r }, how)
            case Item.Family.Potion => let (held, how) = stack(ctx, kind, pack#potions); ({ pack = { potions = held | pack } | r }, how)
        }

    /// 剣・盾: 上の段なら持ち替え、同じか下の段なら拾って消える。
    def equip(kind: Item.Kind, current: Option[Item.Kind]): (Option[Item.Kind], Item.Pick) = match current {
        case Some(held) if Item.tierOf(held) >= Item.tierOf(kind) => (current, Item.Pick.Discarded)
        case _ => (Some(kind), Item.Pick.Upgraded)
    }

    /// 数あり: 上の段なら手持ち全部がその段になって数を足す、同じか下の段なら今の段の数に足す（どちらも上限で切る）。
    def stack(ctx: Ctx, kind: Item.Kind, held: Option[Run.Held]): (Option[Run.Held], Item.Pick) =
        let cap = RulesDoc.capOf(Item.familyOf(kind), ctx#rules);
        let total = Int32.min(cap, Run.countOf(held) + ItemsDoc.statsOf(kind, ctx#items)#count);
        match held {
            case Some(Run.Held.Held(now, _)) if Item.tierOf(now) >= Item.tierOf(kind) => (Some(Run.Held.Held(now, total)), Item.Pick.Stacked)
            case _ => (Some(Run.Held.Held(kind, total)), Item.Pick.Upgraded)
        }

    // ══════════════════════════════════════════════════════
    // 立ち位置の判定（爆弾 → 矢）
    // ══════════════════════════════════════════════════════

    def judge(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let around = r#enemies |> List.count(e -> Enemy.isAwake(e) and Set.exists(c -> Board.chebyshev(c, r#hero#at) <= 1, Enemy.cellsOf(e)));
        let bombs = if (around >= RulesDoc.rec(ctx#rules)#bombNeeds) r#pack#bombs else None;
        match (bombs, arrowTarget(ctx, r)) {
            case (Some(Run.Held.Held(kind, n)), _) if n >= 1 => throwBomb(ctx, kind, t)
            case (_, Some(dir)) => shoot(ctx, dir, t)
            case _ => t
        }

    /// 矢を撃つ向き: 直線の距離 arrowMin..arrowMax に敵がいて、間に矢を止めるマスも敵も無い向きのうち、近い順 → 上・右・下・左。
    def arrowTarget(ctx: Ctx, r: Run.Rec): Option[Dir] =
        if (Run.countOf(r#pack#arrows) < 1) None
        else {
            let occupied = Run.occupancy(r#enemies);
            let nearest = RulesDoc.rec(ctx#rules)#arrowMin;
            Board.dirs()
                |> List.filterMap(dir -> firstEnemyAlong(ctx, occupied, dir, r#hero#at, 1)
                    |> Option.filter(k -> k >= nearest) |> Option.map(k -> (k, dir)))
                |> List.foldLeft((best, c) -> if (Option.exists(b -> fst(b) <= fst(c), best)) best else Some(c), None)
                |> Option.map(snd)
        }

    /// from から dir へ k マス目から見て、最初に敵がいる距離（矢を止めるマスか射程の外で None）。
    def firstEnemyAlong(ctx: Ctx, occupied: Map[Cell, Enemy.Enemy], dir: Dir, origin: Cell, k: Int32): Option[Int32] =
        let cell = Board.ahead(dir, k, origin);
        if (k > RulesDoc.rec(ctx#rules)#arrowMax or Board.stopsArrow(cell, ctx#board)) None
        else if (Map.memberOf(cell, occupied)) Some(k)
        else firstEnemyAlong(ctx, occupied, dir, origin, k + 1)

    def shoot(ctx: Ctx, dir: Dir, t: Turn): Turn =
        let r = t#runState;
        match r#pack#arrows {
            case Some(Run.Held.Held(kind, n)) =>
                let stats = ItemsDoc.statsOf(kind, ctx#items);
                let (hits, end) = arrowPath(ctx, Run.occupancy(r#enemies), dir, r#hero#at, 1 + stats#pierce);
                let shot = t |> withRun({ pack = { arrows = Some(Run.Held.Held(kind, n - 1)) | r#pack } | r })
                    |> emit(Event.ArrowShot(dir, kind, end));
                hits |> List.foldLeft((acc, id) ->
                    acc |> emit(Event.ArrowHit(id, stats#damage)) |> hurt(id, stats#damage) |> setBurn(id, stats#burn), shot)
            case None => t
        }

    /// 矢の通り道: 当たった敵の id（当たった順・limit 体まで）と、矢が止まったマス。
    def arrowPath(ctx: Ctx, occupied: Map[Cell, Enemy.Enemy], dir: Dir, origin: Cell, limit: Int32): (List[Int32], Cell) =
        flight(ctx, occupied, dir, origin, limit, 1, Nil)

    def flight(ctx: Ctx, occupied: Map[Cell, Enemy.Enemy], dir: Dir, origin: Cell, limit: Int32, k: Int32, hits: List[Int32]): (List[Int32], Cell) =
        let cell = Board.ahead(dir, k, origin);
        let found = Map.get(cell, occupied) |> Option.map(e -> e#id) |> Option.filter(id -> not List.memberOf(id, hits));
        let hitsNow = Option.toList(found) ::: hits;
        if (Board.stopsArrow(cell, ctx#board)) (List.reverse(hits), Board.ahead(dir, k - 1, origin))
        else if (List.length(hitsNow) >= limit or k >= RulesDoc.rec(ctx#rules)#arrowMax) (List.reverse(hitsNow), cell)
        else flight(ctx, occupied, dir, origin, limit, k + 1, hitsNow)

    def throwBomb(ctx: Ctx, kind: Item.Kind, t: Turn): Turn =
        let r = t#runState;
        let stats = ItemsDoc.statsOf(kind, ctx#items);
        let targets = r#enemies
            |> List.filter(e -> Set.exists(c -> Board.chebyshev(c, r#hero#at) <= stats#radius, Enemy.cellsOf(e)))
            |> List.map(e -> e#id);
        let thrown = t |> withRun({ pack = { bombs = Some(Run.Held.Held(kind, Run.countOf(r#pack#bombs) - 1)) | r#pack } | r })
            |> emit(Event.BombThrown(kind, r#hero#at));
        targets |> List.foldLeft((acc, id) -> acc |> bombHit(id, stats#damage) |> setSleep(id, stats#sleep), thrown)

    def bombHit(id: Int32, damage: Int32, t: Turn): Turn =
        if (damage <= 0) t else t |> emit(Event.BombHit(id, damage)) |> hurt(id, damage)

    // ══════════════════════════════════════════════════════
    // 敵の値を替える
    // ══════════════════════════════════════════════════════

    def enemyOf(id: Int32, t: Turn): Option[Enemy.Enemy] = List.find(e -> e#id == id, t#runState#enemies)

    def setEnemy(e: Enemy.Enemy, t: Turn): Turn =
        t |> withRun({ enemies = t#runState#enemies |> List.map(x -> if (x#id == e#id) e else x) | t#runState })

    /// 敵にダメージ。HP が 0 以下なら倒れて消える。
    /// WhyNot: 当たっても眠りから起こさない。起こすと、主人公の矢で起きた弓兵が撃ち返す形が敵の予告（気付く位置だけを見る）から漏れる。
    def hurt(id: Int32, damage: Int32, t: Turn): Turn =
        enemyOf(id, t) |> Option.map(e ->
            if (e#hp - damage <= 0)
                t |> withRun({ enemies = List.filter(x -> x#id != id, t#runState#enemies) | t#runState }) |> emit(Event.EnemyDied(id, e#kind, e#at))
            else t |> setEnemy({ hp = e#hp - damage | e })
        ) |> Option.getWithDefault(t)

    def setBurn(id: Int32, turns: Int32, t: Turn): Turn =
        if (turns <= 0) t else enemyOf(id, t) |> Option.map(e -> setEnemy({ burn = turns | e }, t)) |> Option.getWithDefault(t)

    def setSleep(id: Int32, turns: Int32, t: Turn): Turn =
        if (turns <= 0) t
        else enemyOf(id, t) |> Option.map(e -> t |> setEnemy({ rest = Enemy.Rest.Dozing(turns) | e }) |> emit(Event.EnemySlept(id, turns)))
            |> Option.getWithDefault(t)

    /// e が dest（左上）に立てるか: 全部のマスが入れる地形で、主人公とほかの敵がいない。
    def canStand(ctx: Ctx, e: Enemy.Enemy, dest: Cell, r: Run.Rec): Bool =
        let occupied = Run.occupancy(r#enemies);
        let passable = if (Enemy.isFlying(e#kind)) (c -> Board.isFlyable(c, ctx#board)) else (c -> Board.isWalkable(c, ctx#board));
        Enemy.cellsAt(dest, e#size) |> Set.forAll(c ->
            passable(c) and c != r#hero#at and Map.get(c, occupied) |> Option.forAll(other -> other#id == e#id))

    def moveEnemy(e: Enemy.Enemy, dest: Cell, t: Turn): Turn =
        t |> setEnemy({ at = dest | e }) |> emit(Event.EnemyMoved(e#id, e#at, dest))

    // ══════════════════════════════════════════════════════
    // 敵が起きる・敵の手番
    // ══════════════════════════════════════════════════════

    def enemyPhase(ctx: Ctx, t: Turn): Turn =
        let heroAt = t#runState#hero#at;
        let dist = Board.walkDistances(heroAt, ctx#board);
        let woken = t#runState#enemies
            |> List.filter(e -> e#rest == Enemy.Rest.Dormant and Run.notices(ctx, dist, heroAt, e))
            |> List.foldLeft((acc, e) -> acc |> setEnemy({ rest = Enemy.Rest.Awake | e }) |> emit(Event.EnemyWoke(e#id)), t);
        woken#runState#enemies
            |> Sort.sortStableBy(e -> (distanceOf(dist, e), e#id))
            |> List.map(e -> e#id)
            |> List.foldLeft((acc, id) -> enemyTurn(ctx, dist, id, acc), woken)

    /// 主人公への歩く距離（どのマスからも着けなければ Int32 の最大）。
    def distanceOf(dist: Map[Cell, Int32], e: Enemy.Enemy): Int32 =
        Enemy.cellsOf(e) |> Set.foldLeft((acc, c) -> Int32.min(acc, Map.getWithDefault(c, Int32.maxValue(), dist)), Int32.maxValue())

    def enemyTurn(ctx: Ctx, dist: Map[Cell, Int32], id: Int32, t: Turn): Turn =
        if (t#runState#status != Status.Playing) t
        else enemyOf(id, t) |> Option.map(e -> burnTick(ctx, e, t) |> restOrAct(ctx, dist, id)) |> Option.getWithDefault(t)

    def burnTick(ctx: Ctx, e: Enemy.Enemy, t: Turn): Turn =
        if (e#burn <= 0) t
        else {
            let damage = RulesDoc.rec(ctx#rules)#burnDamage;
            t |> setEnemy({ burn = e#burn - 1 | e }) |> emit(Event.EnemyBurned(e#id, damage)) |> hurt(e#id, damage)
        }

    def restOrAct(ctx: Ctx, dist: Map[Cell, Int32], id: Int32, t: Turn): Turn =
        enemyOf(id, t) |> Option.map(e -> byRest(ctx, dist, e, t)) |> Option.getWithDefault(t)

    def byRest(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn = match e#rest {
        case Enemy.Rest.Dormant   => t
        case Enemy.Rest.Dozing(n) => setEnemy({ rest = if (n <= 1) Enemy.Rest.Awake else Enemy.Rest.Dozing(n - 1) | e }, t)
        case Enemy.Rest.Awake     =>
            let counted = { turns = e#turns + 1 | e };
            act(ctx, dist, counted, setEnemy(counted, t))
    }

    def act(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn =
        if (isBeside(e, t#runState#hero#at)) attackHero(ctx, e, None, t)
        else match Enemy.behaviourOf(e#kind) {
            case Enemy.Behaviour.Chase   => chase(ctx, dist, e, t)
            case Enemy.Behaviour.Flutter => flutter(ctx, e, t)
            case Enemy.Behaviour.Shoot   => archer(ctx, dist, e, t)
        }

    /// 主人公が e の上下左右の隣にいる。
    def isBeside(e: Enemy.Enemy, heroAt: Cell): Bool =
        Enemy.cellsOf(e) |> Set.exists(c -> List.memberOf(heroAt, Board.neighbors4(c)))

    /// 歩ける道の幅優先で主人公へ 1 マス寄る。同点は上・右・下・左、塞がっていれば次の同点、無ければ待つ。
    def chase(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn =
        let here = distanceOf(dist, e);
        Board.dirs()
            |> List.map(dir -> Board.step(dir, e#at))
            |> List.find(next -> Map.get(next, dist) == Some(here - 1) and canStand(ctx, e, next, t#runState))
            |> Option.map(next -> moveEnemy(e, next, t))
            |> Option.getWithDefault(t)

    /// 2 手に 1 回（手の数が偶数の手）、入れる隣からでたらめに 1 マス。
    def flutter(ctx: Ctx, e: Enemy.Enemy, t: Turn): Turn =
        let options = Board.dirs() |> List.map(dir -> Board.step(dir, e#at)) |> List.filter(next -> canStand(ctx, e, next, t#runState));
        if (Int32.modulo(e#turns, 2) != 0 or List.isEmpty(options)) t
        else {
            let (i, rng) = Rng.below(List.length(options), t#runState#rng);
            let moved = t |> withRun({ rng = rng | t#runState });
            List.drop(i, options) |> List.head |> Option.map(next -> moveEnemy(e, next, moved)) |> Option.getWithDefault(moved)
        }

    /// 列がそろい、距離 arrowMin..arrowMax で間が空いていれば撃つ。そろっていても撃てなければ寄る、そろっていなければ列をそろえる方へ。
    def archer(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn =
        let rules = RulesDoc.rec(ctx#rules);
        let heroAt = t#runState#hero#at;
        match Board.lineTo(e#at, heroAt) {
            case Some((dir, k)) if k >= rules#arrowMin and k <= rules#arrowMax and isLineClear(ctx, t#runState, dir, e#at, k) =>
                attackHero(ctx, e, Some(e#at), t)
            case Some(_) => chase(ctx, dist, e, t)
            case None    => align(ctx, dist, e, t)
        }

    /// from から dir へ 1..k-1 マスに、矢を止めるマスも敵も無い。
    def isLineClear(ctx: Ctx, r: Run.Rec, dir: Dir, origin: Cell, k: Int32): Bool =
        let occupied = Run.occupancy(r#enemies);
        List.range(1, k) |> List.forAll(i -> {
            let cell = Board.ahead(dir, i, origin);
            not Board.stopsArrow(cell, ctx#board) and not Map.memberOf(cell, occupied)
        })

    /// 縦横の差の小さい方の軸を 0 にする向きを先に試し、だめならもう一方。どちらも塞がっていれば寄る。
    def align(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn =
        let ((hx, hy), (ex, ey)) = (t#runState#hero#at, e#at);
        let across = if (hx > ex) Dir.Right else Dir.Left;
        let along = if (hy > ey) Dir.Down else Dir.Up;
        let tries = if (Int32.abs(hx - ex) <= Int32.abs(hy - ey)) across :: along :: Nil else along :: across :: Nil;
        tries |> List.map(dir -> Board.step(dir, e#at))
            |> List.find(next -> canStand(ctx, e, next, t#runState))
            |> Option.map(next -> moveEnemy(e, next, t))
            |> Option.getWithDefault(chase(ctx, dist, e, t))

    // ══════════════════════════════════════════════════════
    // 主人公が受ける・薬・倒れる・自然回復
    // ══════════════════════════════════════════════════════

    /// e の攻撃を受ける（shotFrom は弓兵の矢の撃ったマス）。盾の守りと補正を引き、銀の盾はたまに弾く。
    def attackHero(ctx: Ctx, e: Enemy.Enemy, shotFrom: Option[Cell], t: Turn): Turn =
        let r = t#runState;
        let guard = r#hero#shield |> Option.map(kind -> ItemsDoc.statsOf(kind, ctx#items)#guard) |> Option.getWithDefault(0);
        let amount = Int32.max(0, EnemiesDoc.statsOf(e#kind, ctx#enemies)#attack - guard - r#perks#guard);
        let chance = r#hero#shield |> Option.filter(kind -> Item.effectOf(kind) == Some(Item.Effect.Deflect))
            |> Option.map(kind -> ItemsDoc.statsOf(kind, ctx#items)#deflect) |> Option.getWithDefault(0.0);
        let (deflected, rng) = if (chance > 0.0) Rng.chance(chance, r#rng) else (false, r#rng);
        let taken = if (deflected) 0 else amount;
        let aimed = shotFrom |> Option.map(origin -> emit(Event.EnemyShot(e#id, origin, r#hero#at), t)) |> Option.getWithDefault(t);
        aimed
            |> withRun({ hero = { hp = r#hero#hp - taken | r#hero }, rng = rng | r })
            |> emit(Event.HeroHurt(e#id, amount, deflected))
            |> drinkIfLow(ctx)
            |> fallIfDown

    /// HP が薬の閾値以下で薬があれば 1 つ使う（倒れる判定の前）。
    def drinkIfLow(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        match r#pack#potions {
            case Some(Run.Held.Held(kind, n)) if n >= 1 and r#hero#hp <= RulesDoc.potionHp(r#hero#maxHp, ctx#rules) =>
                let hp = Int32.min(r#hero#maxHp, r#hero#hp + ItemsDoc.statsOf(kind, ctx#items)#heal);
                t |> withRun({ hero = { hp = hp | r#hero }, pack = { potions = Some(Run.Held.Held(kind, n - 1)) | r#pack } | r })
                  |> emit(Event.PotionUsed(kind, hp - r#hero#hp))
            case _ => t
        }

    def fallIfDown(t: Turn): Turn =
        if (t#runState#hero#hp > 0) t else t |> withRun({ status = Status.Dead | t#runState }) |> emit(Event.HeroDied)

    /// 移動した手の数が regenEvery の倍数になったら HP +1（最大まで）。
    def regen(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let every = RulesDoc.rec(ctx#rules)#regenEvery;
        if (r#status != Status.Playing or every <= 0 or Int32.modulo(r#walked, every) != 0 or r#hero#hp >= r#hero#maxHp) t
        else t |> withRun({ hero = { hp = r#hero#hp + 1 | r#hero } | r }) |> emit(Event.Regenerated(1))
}
