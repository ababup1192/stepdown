// BotRun — 自動で遊ぶ 4 人（でたらめ・避ける人・戦う人・先を読む人）に 1F から最後の階までを種ごとに遊ばせ、クリア率・死亡率・着いた階などの数を出す（画面なし）。
// 回すのは決まりの関数（Step.step と、その要約の Preview.summarize）だけ。見せる側（World・時刻表）は通さない。
// 手を選ぶ読みは人と同じく見えている罠だけの Run（Run.asSeen）で回し、選んだ手は本物の Run に当てる。
// 実行: make bots（SEEDS=<種の数・既定 200、先を読む人だけは 50>・LIMIT=<1 回の手の上限・既定 2000>・ONLY=Random,Avoider,Fighter,Lookahead のどれかで絞る
//                  ・DEPTH=<先を読む手数・既定 3>・MODE=peek（Run の乱数のまま読む・既定）か sample（乱数を 3 通りに替えて平均））
mod BotRun {
    use fge::{App, Sort}
    use Board.Cell
    use Run.{Ctx, Event, Input}

    import java.lang.System

    /// 遊ぶ人。
    ///   Random  … 9 つの入力から乱数で選ぶ
    ///   Avoider … 被ダメージ 0 の入力のうち、階段への歩く距離が一番縮む物（同点は乱数）。階段の上なら待つ
    ///   Fighter … 被ダメージ 0 で殴れる時は殴る。それ以外は Avoider と同じ
    ///   Lookahead … Step.step で depth 手先まで全部の入力を試し、局面の点数が一番高くなる入力（同点は乱数）
    pub enum Bot with Eq, ToString {
        case Random
        case Avoider
        case Fighter
        case Lookahead
    }

    /// 先を読む人の読み方。samples は乱数を替えて試す数（1 なら Run の乱数のまま＝この先の敵の手が分かっている読み）。
    type alias Look = { depth = Int32, samples = Int32 }

    /// 1 つの入力を当てた先。damage は敵から受けるダメージ（空腹は入れない）。
    type alias Candidate = { input = Input, next = Run.State, damage = Int32, blocked = Bool, strike = Bool, at = Cell }

    /// 1 回の遊びの数え。
    ///   hands  … 選んだ手の数（選ぶ場面の答えは数えない）
    ///   stuck  … 動ける入力が全部被弾する手の数（赤に囲まれて動けなかった）
    ///   judged … 被弾する入力としない入力が両方ある手の数
    ///   strikes … 自分から殴った手の数
    ///   cause   … 倒れた手で最後に受けたダメージの出所（倒れていなければ空）
    type alias Tally = { hands = Int32, stuck = Int32, judged = Int32, strikes = Int32, cause = String, rng = Int64 }

    /// 1 回の遊びの終わり。
    type alias Outcome = { status = Run.Status, floor = Int32, hands = Int32, turns = Int32, fullness = Int32, level = Int32, stuck = Int32, judged = Int32, strikes = Int32, cause = String }

    /// 遊ぶ盤（判定の束と、階段からの歩く距離）。
    type alias Field = { ctx = Ctx, toStairs = Map[Cell, Int32], look = Look }

    /// 階を作る Doc と盤。
    type alias Setup = { docs = Floor.Docs, layouts = Floor.Layouts, look = Look }

    def enemyDamage(events: List[Event]): Int32 =
        events |> List.map(e -> match e {
            case Event.HeroHurt(source, amount, _) if Option.nonEmpty(Run.attackerOf(source)) => amount
            case _ => 0
        }) |> List.sum

    def candidateOf(ctx: Ctx, runState: Run.State, input: Input): Candidate =
        let (next, events) = Step.step(ctx, runState, input);
        let act = Preview.summarize(events) |> List.head;
        { input = input, next = next, damage = enemyDamage(events), blocked = act == Some(Preview.Mark.Blocked),
          strike = act == Some(Preview.Mark.Strike), at = Run.heroOf(next)#at }

    /// 選ぶ場面の答え: 捨てるカードは数の少ない物（同じなら左）、レベルアップは左。
    def answer(ctx: Ctx, runState: Run.State): Input = match Run.pendingOf(runState) {
        case Run.Choice.Swap(kind) :: _ => Input.Choose(fewestAt(ctx, kind, Run.packOf(runState)))
        case _                          => Input.Choose(0)
    }

    /// 捨てるカード（持っている枠の並び＋足もとの物）のうち、数が一番少ない物の番号。
    def fewestAt(ctx: Ctx, kind: Item.Kind, pack: Run.Pack): Int32 =
        let counts = (pack |> List.map(held -> Run.countOf(Some(held)))) ::: (ItemsDoc.statsOf(kind, ctx#items)#count :: Nil);
        counts |> List.zipWithIndex |> List.foldLeft((best, e) -> if (snd(e) < snd(best)) e else best, (0, Int32.maxValue())) |> fst

    def pickAny(options: List[Candidate], rng: Int64): (Candidate, Int64) =
        let (i, next) = Rng.below(List.length(options), rng);
        match List.drop(i, options) {
            case c :: _ => (c, next)
            case Nil    => bug!("入力の候補が空")
        }

    /// 階段へ向かう: 階段の上なら待つ。被弾しない入力（無ければ一番被弾の少ない入力）のうち、階段に一番近づく物。
    def headFor(field: Field, runState: Run.State, options: List[Candidate], rng: Int64): (Candidate, Int64) =
        let onStairs = Board.tileAt(Run.heroOf(runState)#at, field#ctx#board) == Board.Tile.Stairs;
        let moving = options |> List.filter(c -> not c#blocked);
        let safe = moving |> List.filter(c -> c#damage == 0);
        let least = moving |> List.map(c -> c#damage) |> List.minimum |> Option.getWithDefault(0);
        let pool = if (onStairs) options |> List.filter(c -> c#input == Input.Wait)
                   else if (List.isEmpty(safe)) moving |> List.filter(c -> c#damage == least)
                   else safe;
        let distOf = (c: Candidate) -> Map.getWithDefault(c#at, Int32.maxValue(), field#toStairs);
        let nearest = pool |> List.map(distOf) |> List.minimum |> Option.getWithDefault(0);
        pickAny(pool |> List.filter(c -> distOf(c) == nearest), rng)

    def strikeOr(field: Field, runState: Run.State, options: List[Candidate], rng: Int64): (Candidate, Int64) =
        match List.find(c -> c#strike and c#damage == 0, options) {
            case Some(c) => (c, rng)
            case None    => headFor(field, runState, options, rng)
        }

    def decide(bot: Bot, field: Field, runState: Run.State, options: List[Candidate], rng: Int64): (Candidate, Int64) =
        match bot {
            case Bot.Random    => pickAny(options, rng)
            case Bot.Avoider   => headFor(field, runState, options, rng)
            case Bot.Fighter   => strikeOr(field, runState, options, rng)
            case Bot.Lookahead => lookAhead(field, runState, options, rng)
        }

    /// 局面の点数の重み。HP は最大の半分より下の 1 点を重く、上の 1 点を軽く数える。
    /// WhyNot: HP の 1 点を一律に重くする — 満タンでも 1 発を嫌って、通り道で眠る敵の手前で待ち続け、空腹で倒れる。
    def lowHpWeight(): Int32 = 10
    def highHpWeight(): Int32 = 2
    def stairsWeight(): Int32 = 4
    def levelWeight(): Int32 = 40
    def expWeight(): Int32 = 4
    def fullnessWeight(): Int32 = 1
    def itemWeight(): Int32 = 6
    def deadScore(): Int32 = -1000000
    def clearScore(): Int32 = 1000000

    /// 局面の点数（HP・階段への歩く距離・経験値とレベル・満腹度・持ち物の残り数の足し算）。
    /// left は読み残した手数（早く降りる・遅く倒れる方を高くする）。
    def scoreOf(field: Field, left: Int32, runState: Run.State): Int32 =
        let r = Run.rec(runState);
        let dist = Map.getWithDefault(r#hero#at, 99, field#toStairs);
        let items = r#pack |> List.map(held -> Run.countOf(Some(held))) |> List.sum;
        match r#status {
            case Run.Status.Dead      => deadScore() - left
            case Run.Status.Descended => clearScore() + left
            case Run.Status.Cleared   => clearScore() + left
            case Run.Status.Playing   =>
                hpScore(r#hero) - dist * stairsWeight() + r#level * levelWeight() + r#exp * expWeight()
                + r#fullness * fullnessWeight() + items * itemWeight()
        }

    def hpScore(hero: Run.Hero): Int32 =
        let half = hero#maxHp / 2;
        Int32.min(hero#hp, half) * lowHpWeight() + Int32.max(0, hero#hp - half) * highHpWeight()

    /// left 手先まで読んだ一番良い点数。選ぶ場面は他のボットと同じ答えで進め、手数に数えない。
    /// WhyNot: つかえる入力を読まない — 何も起きない手で、同じ局面を 2 度読むだけになる。
    def bestAfter(field: Field, left: Int32, runState: Run.State): Int32 =
        let r = Run.rec(runState);
        if (r#status != Run.Status.Playing or left <= 0) scoreOf(field, left, runState)
        else if (not List.isEmpty(r#pending)) bestAfter(field, left, fst(Step.step(field#ctx, runState, answer(field#ctx, runState))))
        else Run.inputs()
            |> List.map(input -> Step.step(field#ctx, runState, input))
            |> List.filter(stepped -> not isBlocked(snd(stepped)))
            |> List.map(stepped -> bestAfter(field, left - 1, fst(stepped)))
            |> List.maximum
            |> Option.getWithDefault(scoreOf(field, left, runState))

    def isBlocked(events: List[Event]): Bool = Preview.summarize(events) |> List.head == Some(Preview.Mark.Blocked)

    /// 1 つの入力の点数。samples が 2 以上なら Run の乱数を替えた局面から同じ入力を試し、点数を平均する。
    def valueOf(field: Field, runState: Run.State, c: Candidate): Int32 =
        let samples = field#look#samples;
        if (samples <= 1) bestAfter(field, field#look#depth - 1, c#next)
        else {
            let r = Run.rec(runState);
            let total = List.range(1, samples + 1)
                |> List.map(k -> fst(Step.step(field#ctx, Run.of({ rng = Rng.mix(r#rng, Int32.toInt64(k)) | r }), c#input)))
                |> List.map(next -> bestAfter(field, field#look#depth - 1, next))
                |> List.sum;
            total / samples
        }

    def lookAhead(field: Field, runState: Run.State, options: List[Candidate], rng: Int64): (Candidate, Int64) =
        let scored = options |> List.filter(c -> not c#blocked) |> List.map(c -> (c, valueOf(field, runState, c)));
        let top = scored |> List.map(snd) |> List.maximum |> Option.getWithDefault(0);
        pickAny(scored |> List.filter(e -> snd(e) == top) |> List.map(fst), rng)

    def countIf(cond: Bool): Int32 = if (cond) 1 else 0

    /// 倒れるか降りるか limit 手まで遊ぶ（最後の Run と数え）。
    def play(bot: Bot, field: Field, limit: Int32, runState: Run.State, tally: Tally): (Run.State, Tally) =
        let r = Run.rec(runState);
        if (r#status != Run.Status.Playing or tally#hands >= limit) (runState, tally)
        else if (not List.isEmpty(r#pending)) play(bot, field, limit, fst(Step.step(field#ctx, runState, answer(field#ctx, runState))), tally)
        else {
            let seen = Run.asSeen(runState);
            let options = Run.inputs() |> List.map(input -> candidateOf(field#ctx, seen, input));
            let moving = List.length(options |> List.filter(c -> not c#blocked));
            let hurting = List.count(c -> not c#blocked and c#damage > 0, options);
            let (picked, rng) = decide(bot, field, seen, options, tally#rng);
            let (next, events) = Step.step(field#ctx, runState, picked#input);
            play(bot, field, limit, next,
                 { hands = tally#hands + 1, stuck = tally#stuck + countIf(hurting == moving),
                   judged = tally#judged + countIf(hurting > 0 and hurting < moving),
                   strikes = tally#strikes + countIf(picked#strike), cause = causeOf(events, tally#cause), rng = rng })
        }

    /// 倒れた手なら最後に受けたダメージの出所の名前（敵の id は落とす）、そうでなければ前の値。
    def causeOf(events: List[Event], before: String): String =
        if (not List.memberOf(Event.HeroDied, events)) before
        else events |> List.filterMap(e -> match e {
            case Event.HeroHurt(source, _, _) => Some(sourceName(source))
            case _                            => None
        }) |> List.last |> Option.getWithDefault("?")

    def sourceName(source: Run.Source): String = match source {
        case Run.Source.Melee(_) => "殴る"
        case Run.Source.Arrow(_) => "矢"
        case Run.Source.Charge(_) => "突進"
        case Run.Source.Starve   => "空腹"
        case Run.Source.Trap(_)  => "罠"
    }

    def salt(bot: Bot): Int64 = match bot {
        case Bot.Random  => 1i64
        case Bot.Avoider => 2i64
        case Bot.Fighter => 3i64
        case Bot.Lookahead => 4i64
    }

    def fieldOf(look: Look, ctx: Ctx): Field =
        let stairs = Board.cells(ctx#board) |> List.filter(c -> Board.tileAt(c, ctx#board) == Board.Tile.Stairs);
        { ctx = ctx, toStairs = Board.walkDistancesFrom(stairs, ctx#board), look = look }

    /// 種 seed で 1F から bot が limit 手まで遊んだ (最後の Run, 数え)。階段で降りたら次の階を作って続ける。
    def playSeed(setup: Setup, bot: Bot, limit: Int32, seed: Int64): (Run.State, Tally) =
        playFloors(setup, bot, limit, Floor.begin(setup#docs, setup#layouts, seed),
                   { hands = 0, stuck = 0, judged = 0, strikes = 0, cause = "", rng = Rng.mix(seed, salt(bot)) })

    def playFloors(setup: Setup, bot: Bot, limit: Int32, runState: Run.State, tally: Tally): (Run.State, Tally) =
        let (after, counted) = play(bot, fieldOf(setup#look, Floor.ctxOf(setup#docs, setup#layouts, runState)), limit, runState, tally);
        if (Run.statusOf(after) == Run.Status.Descended and counted#hands < limit)
            playFloors(setup, bot, limit, Floor.enter(setup#docs, setup#layouts, after, Run.rec(after)#floor + 1), counted)
        else (after, counted)

    def outcomeOf(result: (Run.State, Tally)): Outcome =
        let (runState, tally) = result;
        let r = Run.rec(runState);
        { status = r#status, floor = r#floor, hands = tally#hands, turns = r#turns, fullness = r#fullness, level = r#level, stuck = tally#stuck, judged = tally#judged, strikes = tally#strikes, cause = tally#cause }

    def median(xs: List[Int32]): String =
        let sorted = Sort.sortStable(xs);
        List.drop(List.length(sorted) / 2, sorted) |> List.head |> Option.map(Int32.toString) |> Option.getWithDefault("-")

    def percent(part: Int32, whole: Int32): String =
        if (whole <= 0) "-"
        else {
            let tenths = part * 1000 / whole;
            "${tenths / 10}.${Int32.remainder(tenths, 10)}%"
        }

    /// 階ごとの「着いた回数・その階で倒れた割合・その階を抜けた割合」（抜けた = 次の階に着いたかクリア）。
    def byFloor(deepest: Int32, outcomes: List[Outcome]): String =
        List.range(1, deepest + 1) |> List.map(f -> {
            let reached = List.count(o -> o#floor >= f, outcomes);
            let died = List.count(o -> o#floor == f and o#status == Run.Status.Dead, outcomes);
            let passed = List.count(o -> o#floor > f or (o#floor == f and o#status == Run.Status.Cleared), outcomes);
            "${f}F 着 ${reached}・倒 ${percent(died, reached)}・抜 ${percent(passed, reached)}"
        }) |> List.join(" / ")

    /// 倒れた手の出所ごとの回数（階ごと）。
    def causes(dead: List[Outcome]): String =
        dead |> List.map(o -> "${o#floor}F ${o#cause}") |> List.toSet |> Set.toList
            |> List.map(key -> "${key} ${List.count(o -> "${o#floor}F ${o#cause}" == key, dead)}") |> List.join(" / ")

    /// 1 人分の結果の表の行。
    def report(bot: Bot, fullnessMax: Int32, outcomes: List[Outcome]): String =
        let n = List.length(outcomes);
        let has = status -> outcomes |> List.filter(o -> o#status == status);
        let (cleared, dead) = (has(Run.Status.Cleared), has(Run.Status.Dead));
        let cut = n - List.length(cleared) - List.length(dead);
        let top = outcomes |> List.map(o -> o#level) |> List.maximum |> Option.getWithDefault(1);
        let levels = List.range(1, top + 1) |> List.map(lv -> "Lv${lv} ${List.count(o -> o#level == lv, outcomes)}") |> List.join(" / ");
        let deepest = outcomes |> List.map(o -> o#floor) |> List.maximum |> Option.getWithDefault(1);
        let floors = List.range(1, deepest + 1) |> List.map(f -> "${f}F ${List.count(o -> o#floor == f, outcomes)}（死亡 ${List.count(o -> o#floor == f and o#status == Run.Status.Dead, outcomes)}）") |> List.join(" / ");
        let hands = outcomes |> List.map(o -> o#hands) |> List.sum;
        "[bots] ${bot}（${n} 回）\n" +
        "  クリア ${percent(List.length(cleared), n)}・死亡 ${percent(List.length(dead), n)}・打ち切り ${cut}\n" +
        "  最後にいた階 ${floors}\n" +
        "  階ごと（着いた回数・そこで倒れた割合・抜けた割合）${byFloor(deepest, outcomes)}\n" +
        "  倒れた手の出所 ${causes(dead)}\n" +
        "  赤に囲まれて動けなかった手 ${outcomes |> List.map(o -> o#stuck) |> List.sum}（${List.count(o -> o#stuck > 0, outcomes)} 回の遊びで）\n" +
        "  手数（クリアの中央値）${median(cleared |> List.map(o -> o#turns))}・生き延びた手数（死亡の中央値）${median(dead |> List.map(o -> o#turns))}・全部の中央値 ${median(outcomes |> List.map(o -> o#turns))}\n" +
        "  満腹度の残り（クリアの中央値）${median(cleared |> List.map(o -> o#fullness * 100 / fullnessMax))}%\n" +
        "  到達レベル ${levels}（Lv2 以上 ${percent(List.count(o -> o#level >= 2, outcomes), n)}）\n" +
        "  自分から殴った手 ${outcomes |> List.map(o -> o#strikes) |> List.sum}（1 回の遊びの中央値 ${median(outcomes |> List.map(o -> o#strikes))}）\n" +
        "  判断の割合 ${percent(outcomes |> List.map(o -> o#judged) |> List.sum, hands)}（${hands} 手）"

    /// 種ごとに避ける人で 15 手進めた所の予告（9 入力 × step）を作り直す (1 回の平均の ms, 作った要約の数)。
    /// WhyNot: 時計の読みを要約の数につなぐ — つながない純粋な仕事は消されるか時計の外へ動かされ、間が空になる。
    def previewMs(setup: Setup, seeds: List[Int64]): (Float64, Int32) \ IO =
        let states = seeds |> List.map(seed -> {
            let (runState, _) = playSeed(setup, Bot.Avoider, 15, seed);
            (Floor.ctxOf(setup#docs, setup#layouts, runState), runState)
        });
        let rounds = 5;
        let (ns, k, made) = List.range(0, rounds) |> List.foldLeft((acc, _) ->
            states |> List.foldLeft((inner, e) -> {
                let (spent, count, summaries) = inner;
                let t0 = System.nanoTime();
                let n = if (t0 == 0i64) 0 else List.length(Preview.preview(fst(e), snd(e)));
                let t1 = if (n < 0) 0i64 else System.nanoTime();
                (spent + t1 - t0, count + 1, summaries + n)
            }, acc), (0i64, 0, 0));
        (Int64.toFloat64(ns) / 1000000.0 / Int32.toFloat64(Int32.max(1, k)), made)

    def envInt(name: String, default: Int32, env: Map[String, String]): Int32 =
        Map.get(name, env) |> Option.flatMap(Int32.fromString) |> Option.getWithDefault(default)

    pub def main(): Unit \ IO =
        let env = App.Headless.envOf(App.Headless.requestFromProcess());
        let seedsOf = (bot: Bot) -> List.range(1, envInt("SEEDS", if (bot == Bot.Lookahead) 50 else 200, env) + 1) |> List.map(Int32.toInt64);
        let seeds = seedsOf(Bot.Random);
        let d = run { Controls.loadDocs() } with Fs.FileRead.runWithIO;
        let setup = { docs = { rules = d#rules, items = d#items, enemies = d#enemies, cards = d#cards, floors = d#floors },
                      layouts = Floor.layoutsOf(d#floors)#layouts,
                      look = { depth = envInt("DEPTH", 3, env), samples = if (Map.get("MODE", env) == Some("sample")) 3 else 1 } };
        let limit = envInt("LIMIT", 2000, env);
        let fullnessMax = RulesDoc.rec(d#rules)#fullnessMax;
        let only = Map.getWithDefault("ONLY", "", env);
        (Bot.Random :: Bot.Avoider :: Bot.Fighter :: Bot.Lookahead :: Nil) |> List.filter(bot -> only == "" or String.contains(substr = "${bot}", only)) |> List.forEach(bot -> {
            let t0 = System.nanoTime();
            let outcomes = seedsOf(bot) |> List.map(seed -> outcomeOf(playSeed(setup, bot, limit, seed)));
            let t1 = if (List.length(outcomes) < 0) 0i64 else System.nanoTime();
            println(report(bot, fullnessMax, outcomes));
            println("  （${Int64.toFloat64(t1 - t0) / 1000000000.0} 秒）")
        });
        discard previewMs(setup, seeds);
        let (ms, made) = previewMs(setup, seeds);
        println("[bots] 予告の作り直し 1 回 ${ms} ms（9 入力 × step・種ごとに 15 手進めた所・5 周の平均・要約 ${made}）")
}
