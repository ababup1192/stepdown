// Enemy — 敵の種類・動き方・眠りの状態と、1 体の値。強さ（HP・攻撃・初出の階）は enemies の Doc、動く決まりは Step。
mod Enemy {

    use Board.Cell

    /// 敵の種類。
    pub enum Kind with Eq, Order, ToString {
        case Jelly
        case Bat
        case Archer
        case Swarm
        case Shield
        case Charger
        case Bomber
    }

    /// 動き方。
    ///   Chase   … 歩ける道で主人公へ 1 マス寄る
    ///   Flutter … 2 手に 1 回、飛べるマスへでたらめに 1 マス
    ///   Shoot   … 列がそろえば矢を撃ち、そろわなければ列をそろえる方へ寄る
    ///   Charge  … 上下左右で列がそろい、間が空いていれば溜めて（動かない）、次の手番にその向きへまっすぐ走る。そろわなければ寄る
    ///   Blast   … 隣に来たら導火線に火を付け（殴らない）、次の手番に周りを吹き飛ばして消える。隣でなければ寄る
    pub enum Behaviour with Eq, ToString {
        case Chase
        case Flutter
        case Shoot
        case Charge
        case Blast
    }

    /// 起きているか。
    ///   Dormant   … 階の始めの眠り（主人公に気付くまで）
    ///   Awake     … 起きている（以後ずっと）
    ///   Dozing(n) … 眠り玉の眠り（残りの手の数）
    pub enum Rest with Eq, ToString {
        case Dormant
        case Awake
        case Dozing(Int32)
    }

    /// 次の手番に当てる技の溜め（溜めている間は、隣でも殴らずにこれだけを実行する）。押された・眠った・倒れたら消える。
    ///   Idle        … 溜めていない
    ///   Charge(dir) … 突進の溜め（次の手番に dir へまっすぐ走る）
    ///   Fuse        … 自爆虫の導火線（次の手番に弾ける）
    pub enum Intent with Eq, ToString {
        case Idle
        case Charge(Board.Dir)
        case Fuse
    }

    /// 敵 1 体。id は作った順の番号（同点の時に先に動く）。turns は起きて手番を迎えた数、burn は燃える残りの手の数。
    /// size はマスの幅（ボスの 2×2 に備える。at は左上）。facing は最後に歩いた向き（殴る時は変えない）。
    pub type alias Enemy = {
        id = Int32,
        kind = Kind,
        at = Cell,
        size = Int32,
        hp = Int32,
        rest = Rest,
        turns = Int32,
        burn = Int32,
        facing = Board.Dir8,
        intent = Intent
    }

    /// 階の始めの敵（眠っていて、下を向き、溜めていない）。hp は enemies の Doc の強さ。
    pub def make(id: Int32, kind: Kind, at: Cell, hp: Int32): Enemy =
        { id = id, kind = kind, at = at, size = 1, hp = hp, rest = Rest.Dormant, turns = 0, burn = 0, facing = Board.Dir8.Down, intent = Intent.Idle }

    /// 全部の種類。
    pub def all(): List[Kind] = Kind.Jelly :: Kind.Bat :: Kind.Archer :: Kind.Swarm :: Kind.Shield :: Kind.Charger :: Kind.Bomber :: Nil

    pub def behaviourOf(kind: Kind): Behaviour = match kind {
        case Kind.Jelly  => Behaviour.Chase
        case Kind.Swarm  => Behaviour.Chase
        case Kind.Shield => Behaviour.Chase
        case Kind.Bat    => Behaviour.Flutter
        case Kind.Archer => Behaviour.Shoot
        case Kind.Charger => Behaviour.Charge
        case Kind.Bomber => Behaviour.Blast
    }

    /// 隣を殴る敵か（自爆虫は殴らずに導火線を付ける）。
    pub def strikesBeside(kind: Kind): Bool = behaviourOf(kind) != Behaviour.Blast

    /// 飛ぶ敵か（飛べるマスを通る）。
    pub def isFlying(kind: Kind): Bool = behaviourOf(kind) == Behaviour.Flutter

    /// 占めるマス。
    pub def cellsOf(e: Enemy): Set[Cell] = cellsAt(e#at, e#size)

    /// at を左上にした size×size のマス。
    pub def cellsAt(at: Cell, size: Int32): Set[Cell] =
        let (x, y) = at;
        List.range(0, size) |> List.flatMap(dy -> List.range(0, size) |> List.map(dx -> (x + dx, y + dy))) |> List.toSet

    pub def isAwake(e: Enemy): Bool = e#rest == Rest.Awake

    /// 盾持ちが attacker のマスからの攻撃を正面で防ぐか: attacker への大まかな向きが、向き facing と 8 方向の輪で 1 歩以内（前の 3 方向）。
    pub def guards(attacker: Cell, e: Enemy): Bool =
        e#kind == Kind.Shield and Board.headingTo(e#at, attacker) |> Option.exists(d -> Board.turnsBetween(d, e#facing) <= 1)
}
