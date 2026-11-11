// テストが共有する組み立てと取り出し（文字の地図から盤と判定の束を組む・置き場から階の始めの Run を組む・Doc の既定で描く元を組む）。
mod Fixtures {

    use fge::AudioMixer
    use Board.Cell

    /// 文字の地図から盤を組む。組めない地図はテストの書き誤りなので止める。
    pub def board(rows: List[String]): Board.Board =
        match Board.fromRows(rows) {
            case Ok(b)    => b
            case Err(why) => bug!("テストの地図が組めない: ${why}")
        }

    /// 既定の Doc（rules だけ差し替え）で組んだ判定の束。
    pub def ctxWith(rules: RulesDoc.Doc, rows: List[String]): Run.Ctx =
        { rules = rules, items = ItemsDoc.defaults(), enemies = EnemiesDoc.defaults(), cards = CardsDoc.defaults(), board = board(rows) }

    /// 既定の Doc で組んだ判定の束。
    pub def ctx(rows: List[String]): Run.Ctx = ctxWith(RulesDoc.defaults(), rows)

    /// rules の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def rulesWith(json: String): RulesDoc.Doc =
        match RulesDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの rules の JSON が読めない: ${json}")
        }

    /// enemies の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def enemiesWith(json: String): EnemiesDoc.Doc =
        match EnemiesDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの enemies の JSON が読めない: ${json}")
        }

    /// items の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def itemsWith(json: String): ItemsDoc.Doc =
        match ItemsDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの items の JSON が読めない: ${json}")
        }

    /// floors の JSON の欄を差し替えた Doc（書き誤りは止める）。
    pub def floorsWith(json: String): FloorsDoc.Doc =
        match FloorsDoc.fromJson(json) {
            case Some(d) => d
            case None    => bug!("テストの floors の JSON が読めない: ${json}")
        }

    /// 置き場から階の始めの Run（敵は全員眠っている）。
    pub def start(ctx: Run.Ctx, enemies: List[(Enemy.Kind, Cell)], items: List[(Item.Kind, Cell)]): Run.State =
        Run.start(ctx, { floor = 1, layout = 0, seed = 1i64, rng = 1i64 }, { enemies = enemies, items = items })

    /// 1 部屋の広間（7×5 の床。主人公は左の真ん中 (1, 3)・階段は右下 (7, 5)）。
    pub def hall(): List[String] =
        "#########" ::
        "#.......#" ::
        "#.......#" ::
        "#@......#" ::
        "#.......#" ::
        "#......>#" ::
        "#########" :: Nil

    /// 先頭の選ぶ場面のカード（レベルアップでなければ空）。
    def levelCards(runState: Run.State): List[Run.Perk] = match Run.pendingOf(runState) {
        case Run.Choice.LevelUp(cards) :: _ => cards
        case _                              => Nil
    }

    /// 先頭のレベルアップのカードの枚数。
    pub def cardCount(runState: Run.State): Int32 = List.length(levelCards(runState))

    /// 先頭のレベルアップの候補に perk があるか。
    pub def offers(perk: Run.Perk, runState: Run.State): Bool = List.memberOf(perk, levelCards(runState))

    /// 先頭のレベルアップの候補から perk を選ぶ 1 手（候補に無ければテストの書き誤りなので止める）。
    pub def chooseCard(ctx: Run.Ctx, perk: Run.Perk, runState: Run.State): (Run.State, List[Run.Event]) =
        match List.indexOf(perk, levelCards(runState)) {
            case Some(i) => Step.step(ctx, runState, Run.Input.Choose(i))
            case None    => bug!("候補に ${perk} が無い")
        }

    /// 敵の id → 位置。
    pub def enemyCells(runState: Run.State): List[(Int32, Cell)] = Run.enemiesOf(runState) |> List.map(e -> (e#id, e#at))

    /// 敵の id → 起きているか。
    pub def enemyRests(runState: Run.State): List[(Int32, Enemy.Rest)] = Run.enemiesOf(runState) |> List.map(e -> (e#id, e#rest))

    /// 既定の Doc で組んだ描く元（floors は既定の 1 枚）。
    pub def res(): Res.Res =
        Res.build({ rules = RulesDoc.defaults(), items = ItemsDoc.defaults(), enemies = EnemiesDoc.defaults(), cards = CardsDoc.defaults(),
                    floors = FloorsDoc.defaults(), look = LookDoc.defaults(), text = TextDoc.defaults(),
                    keys = KeysDoc.defaults(), theme = ThemeDoc.defaults(), mixer = AudioMixer.defaults(), kit = Kit.empty() })
}
