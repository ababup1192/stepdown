// TextDoc — stepdown.text.json（画面と記録の文言）の Doc 層。{enemy}・{item}・{n} は fill で差し込む。
// どの出来事にどの文言を当てるかはコード。fail-open — 読めない・書いていない文言は defaults() の物。
mod TextDoc {
    use fge::{DocJson, JsonCodec}
    use Util.Json.Json

    /// 文言の種類。Doc のキーは keyOf の名前。
    pub enum Line with Eq, Order, ToString {
        case Title
        case StartPrompt
        case Dead
        case Cleared
        case Retry
        case QuitPrompt
        case Upgraded
        case Stacked
        case Discarded
        case Struck
        case Shot
        case Bombed
        case Defeated
        case Woke
        case Approached
        case Hurt
        case Deflected
        case Drank
        case Descended
        case Blocked
        case Ate
        case Starved
        case Guarded
        case Swapped
        case LevelUp
        case Dropped
        case Burning
        case Slammed
        case Darted
        case EnemyDarted
        case Thundered
        case ShieldBlocked
        case Alarmed
        case EnemyFell
        case Pitfall
        case DollBroke
        case Exploded
        case Fused
        case Braced
        case FloorLabel
    }

    pub type alias Rec = { version = Int32, lines = Map[Line, String] }

    pub enum Doc(Rec)

    def rec(d: Doc): Rec = match d { case Doc.Doc(r) => r }

    pub def path(): String = "assets/stepdown.text.json"

    def all(): List[Line] =
        Line.Title :: Line.StartPrompt :: Line.Dead :: Line.Cleared :: Line.Retry :: Line.QuitPrompt ::
        Line.Upgraded :: Line.Stacked :: Line.Discarded :: Line.Struck :: Line.Shot :: Line.Bombed ::
        Line.Defeated :: Line.Woke :: Line.Approached :: Line.Hurt :: Line.Deflected :: Line.Drank ::
        Line.Descended :: Line.Blocked ::
        Line.Ate :: Line.Starved :: Line.Guarded :: Line.Swapped :: Line.LevelUp :: Line.Dropped :: Line.Burning :: Line.Slammed :: Line.Darted :: Line.EnemyDarted :: Line.Thundered :: Line.ShieldBlocked :: Line.Alarmed :: Line.EnemyFell :: Line.Pitfall :: Line.DollBroke :: Line.Exploded :: Line.Fused :: Line.Braced ::
        Line.FloorLabel :: Nil

    /// Doc のキー。
    def keyOf(line: Line): String = match line {
        case Line.Title       => "title"
        case Line.StartPrompt => "startPrompt"
        case Line.Dead        => "dead"
        case Line.Cleared     => "cleared"
        case Line.Retry       => "retry"
        case Line.QuitPrompt  => "quitPrompt"
        case Line.Upgraded    => "upgraded"
        case Line.Stacked     => "stacked"
        case Line.Discarded   => "discarded"
        case Line.Struck      => "struck"
        case Line.Shot        => "shot"
        case Line.Bombed      => "bombed"
        case Line.Defeated    => "defeated"
        case Line.Woke        => "woke"
        case Line.Approached  => "approached"
        case Line.Hurt        => "hurt"
        case Line.Deflected   => "deflected"
        case Line.Drank       => "drank"
        case Line.Descended   => "descended"
        case Line.Blocked     => "blocked"
        case Line.Ate => "ate"
        case Line.Starved => "starved"
        case Line.Guarded => "guarded"
        case Line.Swapped => "swapped"
        case Line.LevelUp => "levelUp"
        case Line.Dropped => "dropped"
        case Line.Burning => "burning"
        case Line.Slammed => "slammed"
        case Line.Darted => "darted"
        case Line.EnemyDarted => "enemyDarted"
        case Line.Thundered => "thundered"
        case Line.ShieldBlocked => "shieldBlocked"
        case Line.Alarmed => "alarmed"
        case Line.EnemyFell => "enemyFell"
        case Line.Pitfall => "pitfall"
        case Line.DollBroke => "dollBroke"
        case Line.Exploded => "exploded"
        case Line.Fused => "fused"
        case Line.Braced => "braced"
        case Line.FloorLabel => "floorLabel"
    }

    /// コード側の既定（assets/stepdown.text.json と同じ文言から出発する）。
    def defaultLine(line: Line): String = match line {
        case Line.Title       => "片手間迷宮"
        case Line.StartPrompt => "タップか Enter で はじめる"
        case Line.Dead        => "たおれてしまった…"
        case Line.Cleared     => "迷宮を ぬけた！"
        case Line.Retry       => "タップか Enter で もう一度"
        case Line.QuitPrompt  => "やめますか?  Enter: はい  /  Esc: いいえ"
        case Line.Upgraded    => "{item}を ひろった！ 手持ちが {item}に なった"
        case Line.Stacked     => "{item}を ひろった"
        case Line.Discarded   => "{item}は 今のより 弱い"
        case Line.Struck      => "{enemy}に {n} のダメージ"
        case Line.Shot        => "{item}を はなった"
        case Line.Bombed      => "{item}を なげた"
        case Line.Defeated    => "{enemy}を たおした"
        case Line.Woke        => "{enemy}が 目をさました"
        case Line.Approached  => "{enemy}が となりに にじりよってきた！"
        case Line.Hurt        => "{enemy}から {n} のダメージ"
        case Line.Deflected   => "盾で はじいた！"
        case Line.Drank       => "{item}を のんだ（+{n}）"
        case Line.Descended   => "階段を おりた"
        case Line.Blocked     => "そこへは 進めない"
        case Line.Ate => "{item}を たべた（+{n}）"
        case Line.Starved => "おなかが すいて {n} のダメージ"
        case Line.Guarded => "盾で ふせいだ"
        case Line.Swapped => "{old}を すてて {item}を ひろった"
        case Line.LevelUp => "レベル {n} に あがった！"
        case Line.Dropped => "{item}が おちた"
        case Line.Burning => "{enemy}が もえはじめた"
        case Line.Slammed => "{enemy}を 壁に たたきつけた"
        case Line.Darted => "毒矢を ふんだ！ {n} のダメージ"
        case Line.EnemyDarted => "{enemy}に 毒矢が ささった（{n}）"
        case Line.Thundered => "かみなりが おちた！"
        case Line.ShieldBlocked => "{enemy}が 盾で ふせいだ"
        case Line.Alarmed => "警報が ひびいた！"
        case Line.EnemyFell => "{enemy}が 穴に おちた"
        case Line.Pitfall => "落とし穴に おちた！ {n} のダメージ"
        case Line.DollBroke => "身代わり人形が くだけた！"
        case Line.Exploded => "{enemy}が はじけた！"
        case Line.Fused => "{enemy}の 導火線に 火が ついた！"
        case Line.Braced => "{enemy}が 身がまえた！"
        case Line.FloorLabel => "{n}F"
    }

    pub def defaults(): Doc =
        Doc.Doc({ version = 1, lines = all() |> List.map(line -> (line, defaultLine(line))) |> List.toMap })

    /// 文言（Doc に無ければ既定）。
    pub def lineOf(line: Line, d: Doc): String =
        Map.get(line, rec(d)#lines) |> Option.getWithDefault(defaultLine(line))

    /// 文言の {名前} を args の値で置き換える。
    pub def fill(args: List[(String, String)], text: String): String =
        args |> List.foldLeft((acc, arg) -> String.replace(src = "{${fst(arg)}}", dst = snd(arg), acc), text)

    pub def fromJson(text: String): Option[Doc] =
        DocJson.decodeObject(fromRoot, text)

    def fromRoot(root: Map[String, Json]): Doc =
        Doc.Doc({ version = JsonCodec.intOr("version", 1, root),
                  lines = all() |> List.map(line -> (line, JsonCodec.stringOr(keyOf(line), defaultLine(line), root))) |> List.toMap })

    pub def load(p: String): Doc \ Fs.FileRead =
        DocJson.loadOr(fromJson, defaults(), p)
}
