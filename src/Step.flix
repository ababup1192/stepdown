// Step — 1 手を Run に当てる（純粋）。
//   1. 主人公の手（8 方向。角は抜けない）: 敵がいれば殴る（移動しない）・歩けないか角なら何も起きない（手は進まない）・それ以外は動いて罠を踏み（倒れたらそこで終わり）、拾う。
//      罠は見えていなくても効く（踏まれて見えるようになる）。歩く敵も罠を踏む。
//      待つは「その場に動いた」とみなし、階段の上なら降りる（その手はそこで終わる。最後の階なら Cleared）。
//   2. 動いたか待った後、その位置で 1 つだけ判定する: 爆弾（攻撃できる隣の起きた敵が bombNeeds 以上）→ かみなりの書（気付いている起きた敵が thunderNeeds 以上）
//      → 矢 → 鉄球（当たると 1 マス押す）→ 小石（直線の射程の起きた敵）
//      → 煙玉（隣の起きた敵が bombNeeds 以上か、1 体以上で HP が smokeAt 以下）。
//   拾う: 枠は slotCount まで。埋まっていて 4 種類目なら床に残して捨てるカード（pending）を積む。手は最後まで進め、Choose で答える。
//   3. 気付いた敵が起きる → 敵の手番（主人公への歩く距離の近い順 → 作った順）。受けるたびに薬・倒れるを見る。
//   4. 移動した手だけ数え、regenEvery ごとに HP +1（満腹度 0 の手は飛ばす）。
//   5. 手が進んだ数を数え、hungerEvery ごとに満腹度 -1。満腹度 0 なら HP が減る（薬 → 倒れるを見る）。
//   食べ物は持たず、止まったマス（待つも含む）で食べる。満腹度が最大の時は食べずに床に残す。
//   敵を倒すと（殴る・矢・爆弾・燃えるのどれでも）経験値が入り、たまに物を落とす。レベルが上がるとカードの pending を積む。
mod Step {

    use fge::Sort
    use Board.{Cell, Dir, Dir8}
    use Run.{Ctx, Event, Input, Source, Status}

    /// 1 手の途中の状態。log は起きた順の逆（finish で反転する）。
    type alias Turn = { runState = Run.Rec, log = List[Event] }

    /// 1 手。Playing でない Run には何もしない。選ぶ場面がある間は Choose だけを受け、Choose は手を進めない。
    pub def step(ctx: Ctx, runState: Run.State, input: Input): (Run.State, List[Event]) =
        let r = Run.rec(runState);
        let choosing = not List.isEmpty(r#pending);
        match input {
            case _ if r#status != Status.Playing => (runState, Nil)
            case Input.Choose(i) if choosing     => answer(ctx, i, r)
            case _ if choosing                   => (runState, Nil)
            case Input.Choose(_)                 => (runState, Nil)
            case Input.Wait                      => waitHere(ctx, r)
            case Input.Move(dir)                 => moveTo(ctx, dir, r)
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

    /// 手の終わり: 今の位置から見えるマスを見たことにし、拾わなかった物のマスを離れていたら忘れる。倒れたら選ぶ場面は捨てる。
    def finish(ctx: Ctx, t: Turn): (Run.State, List[Event]) =
        let r = t#runState;
        let declined = r#declined |> Option.filter(c -> c == r#hero#at);
        let pending = if (r#status == Status.Dead) Nil else r#pending;
        (Run.of({ seen = Set.union(r#seen, Board.viewFrom(r#hero#at, ctx#board)), declined = declined, pending = pending | r }), List.reverse(t#log))

    // ══════════════════════════════════════════════════════
    // 主人公の手
    // ══════════════════════════════════════════════════════

    def waitHere(ctx: Ctx, r: Run.Rec): (Run.State, List[Event]) =
        if (Board.tileAt(r#hero#at, ctx#board) == Board.Tile.Stairs)
            finish(ctx, begin({ status = if (r#floor >= RulesDoc.rec(ctx#rules)#lastFloor) Status.Cleared else Status.Descended | r }) |> emit(Event.Descended))
        else begin({ declined = None | r }) |> emit(Event.HeroWaited) |> pickUp(ctx) |> judge(ctx) |> enemyPhase(ctx) |> hunger(ctx) |> finish(ctx)

    def moveTo(ctx: Ctx, dir: Dir8, r: Run.Rec): (Run.State, List[Event]) =
        let target = Board.step8(dir, r#hero#at);
        match Map.get(target, Run.occupancy(r#enemies)) {
            case _ if not Board.isCornerOpen(r#hero#at, target, ctx#board) => (Run.of(r), Event.HeroBlocked(dir) :: Nil)
            case Some(e) => begin(r) |> strike(ctx, dir, e#id) |> enemyPhase(ctx) |> hunger(ctx) |> finish(ctx)
            case None if not Board.isWalkable(target, ctx#board) => (Run.of(r), Event.HeroBlocked(dir) :: Nil)
            case None => begin(r) |> walk(target) |> springTrap(ctx) |> whilePlaying(t -> t |> pickUp(ctx) |> judge(ctx) |> enemyPhase(ctx) |> regen(ctx) |> hunger(ctx)) |> finish(ctx)
        }

    /// 主人公が倒れていなければ残りを進める（罠で倒れた手は、拾う・判定・敵の番・空腹をしない）。
    def whilePlaying(rest: Turn -> Turn, t: Turn): Turn = if (t#runState#status == Status.Playing) rest(t) else t

    /// 歩いて入ったマスの罠が動き、見えるようになる。毒矢は trapDamage（盾は効かない）、警報は眠っている敵を起こしかける。
    def springTrap(ctx: Ctx, t: Turn): Turn =
        let at = t#runState#hero#at;
        Map.get(at, t#runState#traps) |> Option.map(kind -> t |> reveal(kind, at) |> heroTrap(ctx, kind)) |> Option.getWithDefault(t)

    def heroTrap(ctx: Ctx, kind: Trap.Kind, t: Turn): Turn =
        let r = t#runState;
        match kind {
            case Trap.Kind.PoisonDart =>
                let damage = RulesDoc.rec(ctx#rules)#trapDamage;
                t |> withRun({ hero = { hp = r#hero#hp - damage | r#hero } | r })
                  |> emit(Event.HeroHurt(Source.Trap(Trap.Kind.PoisonDart), damage, false))
                  |> drinkIfLow(ctx)
                  |> fallIfDown
            case Trap.Kind.Alarm => ringAlarm(t)
            case Trap.Kind.Pit   => t
        }

    /// 罠が動いた出来事を出し、そのマスの罠を見えるようにする。
    def reveal(kind: Trap.Kind, cell: Cell, t: Turn): Turn =
        t |> withRun({ revealed = Set.insert(cell, t#runState#revealed) | t#runState }) |> emit(Event.TrapSprung(kind, cell))

    /// 警報: 階の始めの眠り（Dormant）の敵だけを Dozing(1) にする（その手の敵の番では動かず、次の手から起きる）。
    /// WhyNot: 眠り玉・煙玉の Dozing も縮める — 主人公が眠らせた敵が、罠のせいで先に起きると読めない。
    def ringAlarm(t: Turn): Turn =
        let dormant = t#runState#enemies |> List.filter(e -> e#rest == Enemy.Rest.Dormant);
        dormant |> List.foldLeft((acc, e) -> setEnemy({ rest = Enemy.Rest.Dozing(1) | e }, acc), t)
            |> emit(Event.AlarmRang(dormant |> List.map(e -> e#id)))

    def walk(target: Cell, t: Turn): Turn =
        let r = t#runState;
        t |> withRun({ hero = { at = target | r#hero }, walked = r#walked + 1 | r }) |> emit(Event.HeroMoved(r#hero#at, target))

    /// 主人公の攻撃（剣があれば剣の攻撃、無ければ素の攻撃）にカードの分を足した物。
    def attackOf(ctx: Ctx, r: Run.Rec): Int32 =
        let base = r#hero#sword |> Option.map(kind -> ItemsDoc.statsOf(kind, ctx#items)#attack)
            |> Option.getWithDefault(RulesDoc.rec(ctx#rules)#baseAttack);
        base + Run.perkCount(Run.Perk.Attack, r) * CardsDoc.rec(ctx#cards)#attack

    /// 殴ると押し出すか（銀の剣か押し出しのカード。両方あっても 1 マス）。
    def pushes(r: Run.Rec): Bool =
        Option.flatMap(Item.effectOf, r#hero#sword) == Some(Item.Effect.Push) or Run.perkCount(Run.Perk.Push, r) > 0

    def strike(ctx: Ctx, dir: Dir8, id: Int32, t: Turn): Turn =
        let r = t#runState;
        let damage = attackOf(ctx, r);
        if (isGuarded(r#hero#at, id, t)) t |> emit(Event.HeroStruck(id, 0)) |> guarded(ctx, id)
        else {
            let struck = t |> emit(Event.HeroStruck(id, damage)) |> hurt(ctx, id, damage);
            if (pushes(r)) push(ctx, dir, id, struck) else struck
        }

    /// id の敵が attacker のマスからの攻撃を盾の正面で受けるか。
    def isGuarded(attacker: Cell, id: Int32, t: Turn): Bool = enemyOf(id, t) |> Option.exists(Enemy.guards(attacker))

    /// 盾で防いだ: ダメージは 0 だが当たったので、眠っていれば Dozing(1) にする（押さない・燃えない）。
    def guarded(ctx: Ctx, id: Int32, t: Turn): Turn = t |> emit(Event.Guarded(id)) |> hurt(ctx, id, 0)

    /// 殴った敵を dir へ 1 マス押す（押されて動いた敵の溜めは消える）。押し先が壁・柱・岩なら動かさずに wallDamage を足す。水・ほかの敵・角では押さない。
    def push(ctx: Ctx, dir: Dir8, id: Int32, t: Turn): Turn =
        enemyOf(id, t) |> Option.map(e -> {
            let dest = Board.step8(dir, e#at);
            let extra = CardsDoc.rec(ctx#cards)#wallDamage;
            if (canStand(ctx, e, dest, t#runState)) t |> setEnemy({ at = dest, intent = Enemy.Intent.Idle | e }) |> emit(Event.EnemyPushed(id, e#at, dest)) |> trapEnemy(ctx, id, dest)
            else if (Board.isSolid(dest, ctx#board) and extra > 0) t |> emit(Event.PushedIntoWall(id, extra)) |> hurt(ctx, id, extra)
            else t
        }) |> Option.getWithDefault(t)

    /// 敵が入ったマス（歩いた・押し出された）の罠が動き、見えるようになる。毒矢は trapDamage、警報は眠っている敵を起こしかける。
    def trapEnemy(ctx: Ctx, id: Int32, cell: Cell, t: Turn): Turn =
        Map.get(cell, t#runState#traps) |> Option.map(kind -> t |> reveal(kind, cell) |> enemyTrap(ctx, id, kind)) |> Option.getWithDefault(t)

    def enemyTrap(ctx: Ctx, id: Int32, kind: Trap.Kind, t: Turn): Turn = match kind {
        case Trap.Kind.PoisonDart =>
            let damage = RulesDoc.rec(ctx#rules)#trapDamage;
            t |> emit(Event.EnemyTrapped(id, damage)) |> hurt(ctx, id, damage)
        case Trap.Kind.Alarm => ringAlarm(t)
        case Trap.Kind.Pit   => t
    }

    // ══════════════════════════════════════════════════════
    // 拾う（上位互換・枠は slotCount まで）
    // ══════════════════════════════════════════════════════

    /// 足もとの物を拾う・食べる。枠が埋まっていて 4 種類目なら床に残したまま捨てるカードを積む
    /// （拾わなかった物のマスでは積まない。待つ手は declined を消してから来るので聞き直す）。
    def pickUp(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        Map.get(r#hero#at, r#items) |> Option.map(kind -> pickUpKind(ctx, kind, t)) |> Option.getWithDefault(t)

    def pickUpKind(ctx: Ctx, kind: Item.Kind, t: Turn): Turn =
        let r = t#runState;
        let family = Item.familyOf(kind);
        if (family == Item.Family.Food) eat(ctx, kind, t)
        else if (not hasRoom(family, r#pack) and r#declined == Some(r#hero#at)) t
        else if (not hasRoom(family, r#pack)) t |> withRun({ pending = r#pending ::: (Run.Choice.Swap(kind) :: Nil) | r })
        else {
            let (taken, how) = take(ctx, kind, r);
            t |> withRun({ items = Map.remove(r#hero#at, r#items) | taken }) |> emit(Event.Picked(kind, how))
        }

    /// family の物を拾う余地があるか: 枠に入らない系統・同じ系統の枠がある・枠が空いている。
    def hasRoom(family: Item.Family, pack: Run.Pack): Bool =
        not Item.isPacked(family) or Option.nonEmpty(Run.heldOf(family, pack)) or List.length(pack) < Run.slotCount()

    /// 足もとの食べ物を食べる（満腹度が最大なら床に残す。あふれる分は捨てる）。
    def eat(ctx: Ctx, kind: Item.Kind, t: Turn): Turn =
        let r = t#runState;
        let most = RulesDoc.rec(ctx#rules)#fullnessMax;
        let gain = ItemsDoc.statsOf(kind, ctx#items)#food + Run.perkCount(Run.Perk.Feast, r) * CardsDoc.rec(ctx#cards)#feast;
        let full = Int32.min(most, r#fullness + gain);
        if (r#fullness >= most) t
        else t |> withRun({ items = Map.remove(r#hero#at, r#items), fullness = full | r }) |> emit(Event.Ate(kind, full - r#fullness))

    /// 拾ったアイテムを手持ちへ。
    def take(ctx: Ctx, kind: Item.Kind, r: Run.Rec): (Run.Rec, Item.Pick) =
        match Item.familyOf(kind) {
            case Item.Family.Sword  => let (held, how) = equip(kind, r#hero#sword); ({ hero = { sword = held | r#hero } | r }, how)
            case Item.Family.Shield => let (held, how) = equip(kind, r#hero#shield); ({ hero = { shield = held | r#hero } | r }, how)
            case family             =>
                let (held, how) = stack(ctx, kind, Run.heldOf(family, r#pack));
                ({ pack = Run.withHeld(held, r#pack) | r }, how)
        }

    /// 剣・盾: 上の段なら持ち替え、同じか下の段なら拾って消える。
    def equip(kind: Item.Kind, current: Option[Item.Kind]): (Option[Item.Kind], Item.Pick) = match current {
        case Some(held) if Item.tierOf(held) >= Item.tierOf(kind) => (current, Item.Pick.Discarded)
        case _ => (Some(kind), Item.Pick.Upgraded)
    }

    /// 数あり: 上の段なら手持ち全部がその段になって数を足す、同じか下の段なら今の段の数に足す（どちらも残る段の上限で切る）。
    def stack(ctx: Ctx, kind: Item.Kind, held: Option[Run.Held]): (Run.Held, Item.Pick) =
        let total = (k: Item.Kind) -> Int32.min(ItemsDoc.statsOf(k, ctx#items)#cap, Run.countOf(held) + ItemsDoc.statsOf(kind, ctx#items)#count);
        match held {
            case Some(Run.Held.Held(now, _)) if Item.tierOf(now) >= Item.tierOf(kind) => (Run.Held.Held(now, total(now)), Item.Pick.Stacked)
            case _ => (Run.Held.Held(kind, total(kind)), Item.Pick.Upgraded)
        }

    // ══════════════════════════════════════════════════════
    // 選ぶ場面（手は進めない）
    // ══════════════════════════════════════════════════════

    /// pending の先頭に i 枚目で答える（範囲外は何もしない）。
    def answer(ctx: Ctx, i: Int32, r: Run.Rec): (Run.State, List[Event]) =
        let answered = match r#pending {
            case Run.Choice.Swap(kind) :: rest     => swap(ctx, kind, i, { pending = rest | r })
            case Run.Choice.LevelUp(cards) :: rest => List.nth(i, cards) |> Option.map(perk -> takePerk(ctx, perk, { pending = rest | r }))
            case Nil                               => None
        };
        answered |> Option.getWithDefault((Run.of(r), Nil))

    /// カードを取る（最大 HP は今の HP も同じだけ増やす）。
    def takePerk(ctx: Ctx, perk: Run.Perk, r: Run.Rec): (Run.State, List[Event]) =
        let more = if (perk == Run.Perk.MaxHp) CardsDoc.rec(ctx#cards)#maxHp else 0;
        (Run.of({ perks = r#perks ::: (perk :: Nil), hero = { maxHp = r#hero#maxHp + more, hp = r#hero#hp + more | r#hero } | r }),
         Event.PerkTaken(perk) :: Nil)

    /// 捨てるカード: 0..枠の数-1 はその枠を捨てて足もとの kind をその位置へ拾う、枠の数番目は拾わずに床に残す（このマスでは聞き直さない）。
    def swap(ctx: Ctx, kind: Item.Kind, i: Int32, r: Run.Rec): Option[(Run.State, List[Event])] =
        let leave = i == List.length(r#pack);
        let dropped = List.nth(i, r#pack);
        if (leave) Some((Run.of({ declined = Some(r#hero#at) | r }), Nil))
        else dropped |> Option.map(old -> {
            let (held, _) = stack(ctx, kind, None);
            let pack = r#pack |> List.zipWithIndex |> List.map(match (j, h) -> if (j == i) held else h);
            (Run.of({ pack = pack, items = Map.remove(r#hero#at, r#items) | r }), Event.Swapped(Run.kindOfHeld(old), kind) :: Nil)
        })

    // ══════════════════════════════════════════════════════
    // 立ち位置の判定（1 手に 1 つ: 爆弾 → かみなりの書 → 矢 → 鉄球 → 小石 → 煙玉）
    // ══════════════════════════════════════════════════════

    def judge(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let rules = RulesDoc.rec(ctx#rules);
        let around = r#enemies |> List.count(e -> Enemy.isAwake(e) and isBeside(ctx, e, r#hero#at));
        let usable = (family: Item.Family) -> Run.countOf(Run.heldOf(family, r#pack)) >= 1;
        let needs = if (Run.perkCount(Run.Perk.LoneBomb, r) > 0) 1 else rules#bombNeeds;
        let smoky = around >= rules#bombNeeds or (around >= 1 and r#hero#hp <= RulesDoc.smokeHp(r#hero#maxHp, ctx#rules));
        let shots = (family: Item.Family) -> if (usable(family)) lineTarget(ctx, family, r) else None;
        let noticing = noticingIds(ctx, r);
        if (around >= needs and usable(Item.Family.Bomb)) throwBomb(ctx, t)
        else if (List.length(noticing) >= rules#thunderNeeds and usable(Item.Family.Thunder)) castThunder(ctx, noticing, t)
        else shots(Item.Family.Arrow) |> Option.map(dir -> shoot(ctx, Item.Family.Arrow, dir, t))
            |> Option.withDefault(default = shots(Item.Family.IronBall) |> Option.map(dir -> shoot(ctx, Item.Family.IronBall, dir, t)))
            |> Option.withDefault(default = shots(Item.Family.Pebble) |> Option.map(dir -> shoot(ctx, Item.Family.Pebble, dir, t)))
            |> Option.getWithDefault(if (smoky and usable(Item.Family.Smoke)) throwSmoke(ctx, t) else t)

    /// 矢・石の届く最長の距離（矢は rules の arrowMax、小石と鉄球は items の range。どれも届く距離のカードの分だけ延びる）。
    def farthestOf(ctx: Ctx, family: Item.Family, r: Run.Rec): Int32 =
        let base = if (Item.isStone(family)) Run.heldOf(family, r#pack) |> Option.map(h -> ItemsDoc.statsOf(Run.kindOfHeld(h), ctx#items)#range) |> Option.getWithDefault(0)
            else RulesDoc.rec(ctx#rules)#arrowMax;
        base + Run.perkCount(Run.Perk.Reach, r) * CardsDoc.rec(ctx#cards)#reach

    /// 矢・小石を撃つ向き: 直線の距離 arrowMin..届く距離 の最初の敵が起きていて盾の正面でなく、間に矢を止めるマスも敵も無い向きのうち、近い順 → 上・右・下・左。
    def lineTarget(ctx: Ctx, family: Item.Family, r: Run.Rec): Option[Dir] =
        let occupied = Run.occupancy(r#enemies);
        let nearest = RulesDoc.rec(ctx#rules)#arrowMin;
        let farthest = farthestOf(ctx, family, r);
        Board.dirs()
            |> List.filterMap(dir -> firstEnemyAlong(ctx, occupied, farthest, dir, r#hero#at, 1)
                |> Option.filter(hit -> fst(hit) >= nearest and Enemy.isAwake(snd(hit)) and not Enemy.guards(r#hero#at, snd(hit))) |> Option.map(hit -> (fst(hit), dir)))
            |> List.foldLeft((best, c) -> if (Option.exists(b -> fst(b) <= fst(c), best)) best else Some(c), None)
            |> Option.map(snd)

    /// from から dir へ k マス目から見て、最初にいる敵とその距離（矢を止めるマスか farthest の外で None）。
    def firstEnemyAlong(ctx: Ctx, occupied: Map[Cell, Enemy.Enemy], farthest: Int32, dir: Dir, origin: Cell, k: Int32): Option[(Int32, Enemy.Enemy)] =
        let cell = Board.ahead(dir, k, origin);
        if (k > farthest or Board.stopsArrow(cell, ctx#board)) None
        else Map.get(cell, occupied) |> Option.map(e -> (k, e)) |> Option.withDefault(default = firstEnemyAlong(ctx, occupied, farthest, dir, origin, k + 1))

    /// family（矢か小石）を dir へ 1 つ放つ。
    def shoot(ctx: Ctx, family: Item.Family, dir: Dir, t: Turn): Turn =
        let r = t#runState;
        match Run.heldOf(family, r#pack) {
            case Some(Run.Held.Held(kind, _)) =>
                let stats = ItemsDoc.statsOf(kind, ctx#items);
                let flying = { limit = 1 + stats#pierce, farthest = farthestOf(ctx, family, r) };
                let (hits, end) = flight(ctx, Run.occupancy(r#enemies), flying, dir, r#hero#at, 1, Nil);
                let shot = t |> withRun({ pack = Run.spend(family, r#pack) | r })
                    |> emit(Event.ArrowShot(dir, kind, end));
                let knock = (id: Int32, acc: Turn) -> if (family == Item.Family.IronBall) push(ctx, Board.toDir8(dir), id, acc) else acc;
                let hit = (id: Int32, acc: Turn) -> acc |> emit(Event.ArrowHit(id, stats#damage)) |> hurt(ctx, id, stats#damage) |> setBurn(id, stats#burn) |> knock(id);
                let block = (id: Int32, acc: Turn) -> acc |> emit(Event.ArrowHit(id, 0)) |> guarded(ctx, id);
                // WhyNot: 防がれた後も貫いて後ろの敵に当てる — 盾持ちの前に並ぶ敵を盾ごと射抜けて、正面を外す意味が消える。
                let (struck, _) = hits |> List.foldLeft((acc, id) -> {
                    let (turn, stopped) = acc;
                    if (stopped) acc
                    else if (isGuarded(r#hero#at, id, turn)) (block(id, turn), true)
                    else (hit(id, turn), false)
                }, (shot, false));
                struck
            case None => t
        }

    /// 放った物の通り道（当たった敵の id は当たった順・limit 体まで、と止まったマス）。
    def flight(ctx: Ctx, occupied: Map[Cell, Enemy.Enemy], flying: { limit = Int32, farthest = Int32 }, dir: Dir, origin: Cell, k: Int32, hits: List[Int32]): (List[Int32], Cell) =
        let cell = Board.ahead(dir, k, origin);
        let found = Map.get(cell, occupied) |> Option.map(e -> e#id) |> Option.filter(id -> not List.memberOf(id, hits));
        let hitsNow = Option.toList(found) ::: hits;
        if (Board.stopsArrow(cell, ctx#board)) (List.reverse(hits), Board.ahead(dir, k - 1, origin))
        else if (List.length(hitsNow) >= flying#limit or k >= flying#farthest) (List.reverse(hitsNow), cell)
        else flight(ctx, occupied, flying, dir, origin, k + 1, hitsNow)

    def throwBomb(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        Run.heldOf(Item.Family.Bomb, r#pack) |> Option.map(held -> {
            let kind = Run.kindOfHeld(held);
            let stats = ItemsDoc.statsOf(kind, ctx#items);
            let targets = r#enemies
                |> List.filter(e -> Set.exists(c -> Board.chebyshev(c, r#hero#at) <= stats#radius, Enemy.cellsOf(e)))
                |> List.map(e -> e#id);
            let thrown = t |> withRun({ pack = Run.spend(Item.Family.Bomb, r#pack) | r })
                |> emit(Event.BombThrown(kind, r#hero#at));
            targets |> List.foldLeft((acc, id) -> acc |> bombHit(ctx, id, stats#damage) |> setSleep(id, stats#sleep), thrown)
        }) |> Option.getWithDefault(t)

    def bombHit(ctx: Ctx, id: Int32, damage: Int32, t: Turn): Turn =
        if (damage <= 0) t else t |> emit(Event.BombHit(id, damage)) |> hurt(ctx, id, damage)

    /// 主人公に気付いている起きた敵の id（作った順）。
    def noticingIds(ctx: Ctx, r: Run.Rec): List[Int32] =
        let dist = Board.walkDistances(r#hero#at, ctx#board);
        r#enemies |> List.filter(e -> Enemy.isAwake(e) and Run.notices(ctx, dist, r#hero#at, e)) |> List.map(e -> e#id)

    /// かみなりの書: 気付いている起きた敵の全員に damage（盾では防げない）。
    def castThunder(ctx: Ctx, targets: List[Int32], t: Turn): Turn =
        let r = t#runState;
        Run.heldOf(Item.Family.Thunder, r#pack) |> Option.map(held -> {
            let damage = ItemsDoc.statsOf(Run.kindOfHeld(held), ctx#items)#damage;
            let cast = t |> withRun({ pack = Run.spend(Item.Family.Thunder, r#pack) | r }) |> emit(Event.ThunderCast(targets));
            targets |> List.foldLeft((acc, id) -> acc |> emit(Event.ThunderHit(id, damage)) |> hurt(ctx, id, damage), cast)
        }) |> Option.getWithDefault(t)

    /// 煙玉: 主人公を攻撃できる隣の起きた敵を、煙玉の sleep 手だけ眠らせる。
    def throwSmoke(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let turns = Run.heldOf(Item.Family.Smoke, r#pack) |> Option.map(h -> ItemsDoc.statsOf(Run.kindOfHeld(h), ctx#items)#sleep) |> Option.getWithDefault(0);
        let targets = r#enemies |> List.filter(e -> Enemy.isAwake(e) and isBeside(ctx, e, r#hero#at)) |> List.map(e -> e#id);
        let thrown = t |> withRun({ pack = Run.spend(Item.Family.Smoke, r#pack) | r }) |> emit(Event.SmokeThrown(r#hero#at));
        targets |> List.foldLeft((acc, id) -> setSleep(id, turns, acc), thrown)

    // ══════════════════════════════════════════════════════
    // 敵の値を替える
    // ══════════════════════════════════════════════════════

    def enemyOf(id: Int32, t: Turn): Option[Enemy.Enemy] = List.find(e -> e#id == id, t#runState#enemies)

    def setEnemy(e: Enemy.Enemy, t: Turn): Turn =
        t |> withRun({ enemies = t#runState#enemies |> List.map(x -> if (x#id == e#id) e else x) | t#runState })

    /// 敵にダメージ。HP が 0 以下なら倒れて消える。眠っている敵は Dozing(1) になり、その手の敵の番では動かず次の手から起きる。
    /// WhyNot: すぐ起こさない。その手で撃ち返す・殴り返すと、敵の予告（その手の前に起きている敵と、気付く位置だけを見る）から漏れる。
    def hurt(ctx: Ctx, id: Int32, damage: Int32, t: Turn): Turn =
        enemyOf(id, t) |> Option.map(e ->
            if (e#hp - damage <= 0)
                t |> withRun({ enemies = List.filter(x -> x#id != id, t#runState#enemies) | t#runState }) |> emit(Event.EnemyDied(id, e#kind, e#at))
                  |> dropLoot(ctx, e) |> gainExp(ctx, EnemiesDoc.statsOf(e#kind, ctx#enemies)#exp)
            else t |> setEnemy({ hp = e#hp - damage, rest = if (Enemy.isAwake(e)) e#rest else Enemy.Rest.Dozing(1) | e })
        ) |> Option.getWithDefault(t)

    /// 生きている敵を turns 手のあいだ燃やす（燃え始めの出来事。燃えるダメージは敵の手番ごとの EnemyBurned）。
    def setBurn(id: Int32, turns: Int32, t: Turn): Turn =
        if (turns <= 0) t else enemyOf(id, t) |> Option.map(e -> t |> setEnemy({ burn = turns | e }) |> emit(Event.Burned(id, turns))) |> Option.getWithDefault(t)

    /// 生きている敵を turns 手のあいだ眠らせる（溜めは消える）。
    def setSleep(id: Int32, turns: Int32, t: Turn): Turn =
        if (turns <= 0) t
        else enemyOf(id, t) |> Option.map(e -> t |> setEnemy({ rest = Enemy.Rest.Dozing(turns), intent = Enemy.Intent.Idle | e }) |> emit(Event.EnemySlept(id, turns)))
            |> Option.getWithDefault(t)

    // ══════════════════════════════════════════════════════
    // 倒した後（落とし物・経験値）
    // ══════════════════════════════════════════════════════

    /// 倒した e は dropChance で drops から 1 つ落とす（乱数はいつも 2 回引く）。
    def dropLoot(ctx: Ctx, e: Enemy.Enemy, t: Turn): Turn =
        let r = t#runState;
        let stats = EnemiesDoc.statsOf(e#kind, ctx#enemies);
        let (dropped, afterChance) = Rng.chance(stats#dropChance, r#rng);
        let (pick, afterPick) = Rng.below(List.length(stats#drops), afterChance);
        let rolled = { rng = afterPick | r };
        List.nth(pick, stats#drops) |> Option.filter(_ -> dropped)
            |> Option.flatMap(kind -> dropCell(ctx, e#at, rolled) |> Option.map(cell -> (kind, cell)))
            |> Option.map(match (kind, cell) -> t |> withRun({ items = Map.insert(cell, kind, rolled#items) | rolled }) |> emit(Event.ItemDropped(kind, cell)))
            |> Option.getWithDefault(withRun(rolled, t))

    /// 落とし物の置き場: origin が空いた床ならそこ、違えば歩く距離の近い空いた床（同じ距離は読む順）。
    /// 空いた床 = 床（階段・水でない）で、主人公のマスでも始まりでもなく、ほかの物も罠も無い。無ければ None。
    def dropCell(ctx: Ctx, origin: Cell, r: Run.Rec): Option[Cell] =
        let b = ctx#board;
        let free = (c: Cell) -> Board.tileAt(c, b) == Board.Tile.Floor and c != r#hero#at and c != b#start and not Map.memberOf(c, r#items) and not Map.memberOf(c, r#traps);
        if (free(origin)) Some(origin)
        else Board.walkDistancesFrom(origin :: Nil, b) |> Map.toList |> List.filter(e -> free(fst(e)))
            |> List.map(match (c, steps) -> (steps, (snd(c), fst(c)))) |> List.minimum |> Option.map(e -> { let (_, (y, x)) = e; (x, y) })

    /// 経験値を足し、次のレベルまでに要る数に届いたら上げる（余りは持ち越す。1 度に 2 つ上がれば 2 つ積む）。
    def gainExp(ctx: Ctx, amount: Int32, t: Turn): Turn =
        if (amount <= 0) t
        else t |> withRun({ exp = t#runState#exp + amount | t#runState }) |> emit(Event.ExpGained(amount)) |> levelUps(ctx)

    def levelUps(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let need = RulesDoc.expToNext(r#level, ctx#rules);
        if (r#exp < need) t
        else {
            let (cards, rng) = drawCards(ctx, r);
            let raised = { level = r#level + 1, exp = r#exp - need, rng = rng, pending = r#pending ::: (Run.Choice.LevelUp(cards) :: Nil) | r };
            t |> withRun(raised) |> emit(Event.LevelUp(r#level + 1)) |> levelUps(ctx)
        }

    /// レベルアップの候補: 一度だけのカードで取った物を除き、乱数で choices 枚。
    def drawCards(ctx: Ctx, r: Run.Rec): (List[Run.Perk], Int64) =
        let open = Run.perks() |> List.filter(p -> not (Run.isOnce(p) and Run.perkCount(p, r) > 0));
        Rng.pick(CardsDoc.rec(ctx#cards)#choices, open, r#rng)

    /// e が隣の dest（左上）へ移れるか: 角を抜けず、全部のマスが入れる地形で、主人公とほかの敵がいない。
    def canStand(ctx: Ctx, e: Enemy.Enemy, dest: Cell, r: Run.Rec): Bool =
        let occupied = Run.occupancy(r#enemies);
        let passable = if (Enemy.isFlying(e#kind)) (c -> Board.isFlyable(c, ctx#board)) else (c -> Board.isWalkable(c, ctx#board));
        Board.isCornerOpen(e#at, dest, ctx#board) and Enemy.cellsAt(dest, e#size) |> Set.forAll(c ->
            passable(c) and c != r#hero#at and Map.get(c, occupied) |> Option.forAll(other -> other#id == e#id))

    /// 敵が自分で動く（向きを動いた向きにする）。歩く敵は入ったマスの罠を踏む（見えている罠もよけない）。
    /// WhyNot: 飛ぶ敵にも罠を効かせる — 床の板を踏まずに飛び越える絵と合わない。
    def moveEnemy(ctx: Ctx, e: Enemy.Enemy, dest: Cell, t: Turn): Turn =
        let facing = Board.dirTo(e#at, dest) |> Option.getWithDefault(e#facing);
        let moved = t |> setEnemy({ at = dest, facing = facing | e }) |> emit(Event.EnemyMoved(e#id, e#at, dest));
        if (Enemy.isFlying(e#kind)) moved else trapEnemy(ctx, e#id, dest, moved)

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
            t |> setEnemy({ burn = e#burn - 1 | e }) |> emit(Event.EnemyBurned(e#id, damage)) |> hurt(ctx, e#id, damage)
        }

    def restOrAct(ctx: Ctx, dist: Map[Cell, Int32], id: Int32, t: Turn): Turn =
        enemyOf(id, t) |> Option.map(e -> byRest(ctx, dist, e, t)) |> Option.getWithDefault(t)

    def byRest(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn = match e#rest {
        case Enemy.Rest.Dormant   => t
        case Enemy.Rest.Dozing(n) if n <= 1 => t |> setEnemy({ rest = Enemy.Rest.Awake | e }) |> emit(Event.EnemyWoke(e#id))
        case Enemy.Rest.Dozing(n) => setEnemy({ rest = Enemy.Rest.Dozing(n - 1) | e }, t)
        case Enemy.Rest.Awake     =>
            let counted = { turns = e#turns + 1 | e };
            act(ctx, dist, counted, setEnemy(counted, t))
    }

    /// 溜めていれば溜めた技だけを実行する。溜めていなければ、主人公へ攻撃が届けば当て（届くかは Run.attackOn だけで決める）、
    /// 届かなければ溜められる技を溜め、それも無ければ動く。
    def act(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn =
        let blockers = blockersFor(e, t);
        match e#intent {
            case Enemy.Intent.Charge(dir) => charge(ctx, blockers, e, dir, t)
            case Enemy.Intent.Fuse        => explode(ctx, e, t)
            case Enemy.Intent.Idle        =>
                Run.attackOn(ctx, blockers, e, t#runState#hero#at)
                    |> Option.map(attack -> attackHero(ctx, e, attack, t))
                    |> Option.withDefault(default = windupOf(ctx, blockers, e, t#runState#hero#at) |> Option.map(intent -> windup(e, intent, t)))
                    |> Option.getWithDefault(moveBy(ctx, dist, e, t))
        }

    /// e のほかの敵の占めるマス（矢と突進を止める）。
    def blockersFor(e: Enemy.Enemy, t: Turn): Set[Cell] =
        Run.occupancy(t#runState#enemies) |> Map.filter(other -> other#id != e#id) |> Map.keysOf

    /// 今溜められる技（突進は主人公と列がそろい間が空いた時・自爆虫は主人公が殴れる隣にいる時）。
    def windupOf(ctx: Ctx, blockers: Set[Cell], e: Enemy.Enemy, heroAt: Cell): Option[Enemy.Intent] = match Enemy.behaviourOf(e#kind) {
        case Enemy.Behaviour.Charge => Run.chargeAim(ctx, blockers, e, heroAt) |> Option.map(Enemy.Intent.Charge)
        case Enemy.Behaviour.Blast  => if (isBeside(ctx, e, heroAt)) Some(Enemy.Intent.Fuse) else None
        case _                      => None
    }

    /// 導火線の付いた自爆虫が弾けて消える: 殴れる隣（Run.attackOn が弾けと言うマス）の敵と主人公に blastDamage（盾は効かない）。
    /// 巻き込まれて倒れた敵の経験値は主人公に入り、弾けた自爆虫の分は入らない。
    def explode(ctx: Ctx, e: Enemy.Enemy, t: Turn): Turn =
        let damage = EnemiesDoc.statsOf(e#kind, ctx#enemies)#blastDamage;
        let caught = (x: Enemy.Enemy) -> x#id != e#id and Set.exists(c -> Run.attackOn(ctx, Set.empty(), e, c) == Some(Run.Attack.Blast), Enemy.cellsOf(x));
        let victims = t#runState#enemies |> List.filter(caught) |> List.map(x -> x#id);
        let heroCaught = Run.attackOn(ctx, Set.empty(), e, t#runState#hero#at) == Some(Run.Attack.Blast);
        let gone = t |> withRun({ enemies = List.filter(x -> x#id != e#id, t#runState#enemies) | t#runState }) |> emit(Event.EnemyExploded(e#id, e#at));
        let struck = victims |> List.foldLeft((acc, id) -> bombHit(ctx, id, damage, acc), gone);
        if (heroCaught and damage > 0) struck |> heroBlast(ctx, e#id, damage) else struck

    def heroBlast(ctx: Ctx, id: Int32, damage: Int32, t: Turn): Turn =
        let r = t#runState;
        t |> withRun({ hero = { hp = r#hero#hp - damage | r#hero } | r })
          |> emit(Event.HeroHurt(Source.Blast(id), damage, false))
          |> drinkIfLow(ctx)
          |> fallIfDown

    /// 技を溜める（動かず、当てない）。
    def windup(e: Enemy.Enemy, intent: Enemy.Intent, t: Turn): Turn =
        t |> setEnemy({ intent = intent | e }) |> emit(Event.EnemyWindup(e#id, intent))

    /// 溜めた突進: dir の走る道を、主人公の手前か道の終わりまで走る。主人公が道にいれば当たり、歩けないマスで止まったら Dozing(1)。
    /// 入ったマスの罠は止まったマスだけで踏む。
    /// WhyNot: 通り過ぎたマスの罠も踏ませる — 1 手で罠が何個も動き、どこで何が起きたか見せきれない。
    def charge(ctx: Ctx, blockers: Set[Cell], e: Enemy.Enemy, dir: Dir, t: Turn): Turn =
        let heroAt = t#runState#hero#at;
        let lane = Run.chargeLane(ctx, blockers, e, dir);
        let path = lane |> List.takeWhile(c -> c != heroAt);
        let hits = List.memberOf(heroAt, lane);
        let dest = List.last(path) |> Option.getWithDefault(e#at);
        let ran = { at = dest, facing = Board.toDir8(dir), intent = Enemy.Intent.Idle | e };
        let stopped = not hits and not Board.isWalkable(Board.ahead(dir, 1, dest), ctx#board);
        let landed = t |> setEnemy(ran) |> emit(Event.EnemyCharged(e#id, e#at, dest))
            |> (acc -> if (dest == e#at) acc else trapEnemy(ctx, e#id, dest, acc));
        landed |> whileAlive(e#id, acc ->
            if (hits) attackHero(ctx, ran, Run.Attack.Charge, acc)
            else if (stopped) enemyOf(e#id, acc) |> Option.map(now -> setEnemy({ rest = Enemy.Rest.Dozing(1) | now }, acc)) |> Option.getWithDefault(acc)
            else acc)

    /// id の敵がまだ生きていれば残りを進める。
    def whileAlive(id: Int32, rest: Turn -> Turn, t: Turn): Turn = if (Option.isEmpty(enemyOf(id, t))) t else rest(t)

    def moveBy(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn = match Enemy.behaviourOf(e#kind) {
        case Enemy.Behaviour.Chase   => chase(ctx, dist, e, t)
        case Enemy.Behaviour.Charge  => chase(ctx, dist, e, t)
        case Enemy.Behaviour.Blast   => chase(ctx, dist, e, t)
        case Enemy.Behaviour.Flutter => flutter(ctx, e, t)
        case Enemy.Behaviour.Shoot   => align(ctx, dist, e, t)
    }

    /// 主人公が e の殴れる隣にいるか。
    def isBeside(ctx: Ctx, e: Enemy.Enemy, heroAt: Cell): Bool = Run.isBeside(ctx, e, heroAt)

    /// 歩く距離 toward が 1 少ない隣へ 1 マス寄る。同点は 上・右上・右・右下・下・左下・左・左上、塞がっていれば次の同点、無ければ fallback。
    def stepDown(ctx: Ctx, toward: Map[Cell, Int32], e: Enemy.Enemy, fallback: Turn, t: Turn): Turn =
        let here = distanceOf(toward, e);
        Board.dirs8()
            |> List.map(dir -> Board.step8(dir, e#at))
            |> List.find(next -> Map.get(next, toward) == Some(here - 1) and canStand(ctx, e, next, t#runState))
            |> Option.map(next -> moveEnemy(ctx, e, next, t))
            |> Option.getWithDefault(fallback)

    /// 歩ける道（8 方向・角は抜けない）の幅優先で主人公へ 1 マス寄る。
    def chase(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn = stepDown(ctx, dist, e, t, t)

    /// 2 手に 1 回（手の数が偶数の手）、入れる隣（8 方向）からでたらめに 1 マス。
    def flutter(ctx: Ctx, e: Enemy.Enemy, t: Turn): Turn =
        let options = Board.dirs8() |> List.map(dir -> Board.step8(dir, e#at)) |> List.filter(next -> canStand(ctx, e, next, t#runState));
        if (Int32.modulo(e#turns, 2) != 0 or List.isEmpty(options)) t
        else {
            let (i, rng) = Rng.below(List.length(options), t#runState#rng);
            let moved = t |> withRun({ rng = rng | t#runState });
            List.drop(i, options) |> List.head |> Option.map(next -> moveEnemy(ctx, e, next, moved)) |> Option.getWithDefault(moved)
        }

    /// 主人公を射線に入れる歩けるマス（主人公から 4 方向・距離 arrowMin..arrowMax・矢を止めるマスの手前まで）への歩く距離で 1 マス寄る。
    /// もう射線のマスにいる（間を敵が塞いでいる）・寄れない時は主人公へ寄る。
    def align(ctx: Ctx, dist: Map[Cell, Int32], e: Enemy.Enemy, t: Turn): Turn =
        let heroAt = t#runState#hero#at;
        let lines = Board.dirs() |> List.flatMap(dir -> Board.lane(RulesDoc.reachOf(ctx#rules), dir, heroAt, ctx#board))
            |> List.filter(c -> Board.isWalkable(c, ctx#board));
        if (List.memberOf(e#at, lines)) chase(ctx, dist, e, t)
        else stepDown(ctx, Board.walkDistancesFrom(lines, ctx#board), e, chase(ctx, dist, e, t), t)

    // ══════════════════════════════════════════════════════
    // 主人公が受ける・薬・倒れる・自然回復
    // ══════════════════════════════════════════════════════

    /// e の攻撃を受ける。盾の守りと補正を引き、銀の盾はたまに弾く。
    def attackHero(ctx: Ctx, e: Enemy.Enemy, attack: Run.Attack, t: Turn): Turn =
        let r = t#runState;
        let guard = r#hero#shield |> Option.map(kind -> ItemsDoc.statsOf(kind, ctx#items)#guard) |> Option.getWithDefault(0);
        let amount = Int32.max(0, EnemiesDoc.statsOf(e#kind, ctx#enemies)#attack - guard);
        let chance = r#hero#shield |> Option.filter(kind -> Item.effectOf(kind) == Some(Item.Effect.Deflect))
            |> Option.map(kind -> ItemsDoc.statsOf(kind, ctx#items)#deflect) |> Option.getWithDefault(0.0);
        let (deflected, rng) = if (chance > 0.0) Rng.chance(chance, r#rng) else (false, r#rng);
        let taken = if (deflected) 0 else amount;
        let shot = attack == Run.Attack.Shot;
        let aimed = if (shot) emit(Event.EnemyShot(e#id, e#at, r#hero#at), t) else t;
        aimed
            |> withRun({ hero = { hp = r#hero#hp - taken | r#hero }, rng = rng | r })
            |> emit(Event.HeroHurt(sourceOf(e#id, attack), amount, deflected))
            |> drinkIfLow(ctx)
            |> fallIfDown

    /// 敵の攻撃の種類 → 主人公が受けたダメージの出所。
    def sourceOf(id: Int32, attack: Run.Attack): Source = match attack {
        case Run.Attack.Melee  => Source.Melee(id)
        case Run.Attack.Shot   => Source.Arrow(id)
        case Run.Attack.Charge => Source.Charge(id)
        case Run.Attack.Blast  => Source.Blast(id)
    }

    /// HP が薬の閾値以下で薬があれば 1 つ使う（倒れる判定の前）。
    def drinkIfLow(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        match Run.heldOf(Item.Family.Potion, r#pack) {
            case Some(Run.Held.Held(kind, n)) if n >= 1 and r#hero#hp <= RulesDoc.potionHp(r#hero#maxHp, ctx#rules) =>
                let hp = Int32.min(r#hero#maxHp, r#hero#hp + ItemsDoc.statsOf(kind, ctx#items)#heal);
                t |> withRun({ hero = { hp = hp | r#hero }, pack = Run.spend(Item.Family.Potion, r#pack) | r })
                  |> emit(Event.PotionUsed(kind, hp - r#hero#hp))
            case _ => t
        }

    /// HP が 0 以下なら倒れる。身代わり人形があれば、倒れる代わりに人形が砕けて HP が最大の半分になる（薬の後に見る）。
    def fallIfDown(t: Turn): Turn =
        let r = t#runState;
        let saved = Int32.max(1, r#hero#maxHp / 2);
        if (r#hero#hp > 0) t
        else if (Run.countOf(Run.heldOf(Item.Family.Doll, r#pack)) >= 1)
            t |> withRun({ hero = { hp = saved | r#hero }, pack = Run.spend(Item.Family.Doll, r#pack) | r }) |> emit(Event.DollSaved(saved))
        else t |> withRun({ status = Status.Dead | r }) |> emit(Event.HeroDied)

    /// 移動した手の数が regenEvery の倍数になったら HP +1（最大まで。満腹度 0 の手は飛ばす）。
    def regen(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let every = RulesDoc.rec(ctx#rules)#regenEvery;
        if (r#status != Status.Playing or r#fullness <= 0 or every <= 0 or Int32.modulo(r#walked, every) != 0 or r#hero#hp >= r#hero#maxHp) t
        else t |> withRun({ hero = { hp = r#hero#hp + 1 | r#hero } | r }) |> emit(Event.Regenerated(1))

    /// 手が進んだ数を数え、hungerEvery（空腹が遅いカードなら cards の hungerEvery）の倍数で満腹度 -1。満腹度が 0 なら starveDamage を受ける（薬 → 倒れるを見る）。
    def hunger(ctx: Ctx, t: Turn): Turn =
        let r = t#runState;
        let rules = RulesDoc.rec(ctx#rules);
        let every = if (Run.perkCount(Run.Perk.SlowHunger, r) > 0) CardsDoc.rec(ctx#cards)#hungerEvery else rules#hungerEvery;
        let turns = r#turns + 1;
        let fullness = if (every > 0 and Int32.modulo(turns, every) == 0) Int32.max(0, r#fullness - 1) else r#fullness;
        let fed = { turns = turns, fullness = fullness | r };
        if (r#status != Status.Playing) t
        else if (fullness > 0 or rules#starveDamage <= 0) withRun(fed, t)
        else t
            |> withRun({ hero = { hp = fed#hero#hp - rules#starveDamage | fed#hero } | fed })
            |> emit(Event.HeroHurt(Source.Starve, rules#starveDamage, false))
            |> drinkIfLow(ctx)
            |> fallIfDown
}
