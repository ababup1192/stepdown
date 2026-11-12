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
        case Boss
    }

    /// 動き方。
    ///   Chase   … 歩ける道で主人公へ 1 マス寄る
    ///   Flutter … 2 手に 1 回、飛べるマスへでたらめに 1 マス
    ///   Shoot   … 列がそろえば矢を撃ち、そろわなければ列をそろえる方へ寄る
    ///   Charge  … 上下左右で列がそろい、間が空いていれば溜めて（動かない）、次の手番にその向きへまっすぐ走る。そろわなければ寄る
    ///   Blast   … 隣に来たら導火線に火を付け（殴らない）、次の手番に周りを吹き飛ばして消える。隣でなければ寄る
    ///   Boss    … 2×2 の体で歩いて寄って殴り、cooldown の手が尽きるたびに大技（叩きつけ・帯）を溜める
    pub enum Behaviour with Eq, ToString {
        case Chase
        case Flutter
        case Shoot
        case Charge
        case Blast
        case Boss
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
    ///   Slam        … ボスの叩きつけ（次の手番に体の周り 12 マスを叩く）
    ///   Sweep(top)  … ボスの帯（次の手番に、部屋の中の行 top と top + 1 をなぎ払う）
    ///   SlamAndSweep(top) … 叩きつけと帯を同じ手番に重ねる（HP が半分を切ったボス）
    pub enum Intent with Eq, ToString {
        case Idle
        case Charge(Board.Dir)
        case Fuse
        case Slam
        case Sweep(Int32)
        case SlamAndSweep(Int32)
    }

    /// 敵 1 体。id は作った順の番号（同点の時に先に動く）。turns は起きて手番を迎えた数、burn は燃える残りの手の数。
    /// size はマスの幅（ボスは 2×2。at は左上）。facing は最後に歩いた向き（盾持ちは歩く・待つ手番ごとに主人公の方へ 45° ずつ回る。殴る時は変えない）。
    /// cooldown はボスが次の大技を溜めるまでの手の数（0 の手番に溜める。ボスでない敵は使わない）。
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
        intent = Intent,
        cooldown = Int32
    }

    /// 階の始めの敵（眠っていて、下を向き、溜めていない）。hp は enemies の Doc の強さ。
    pub def make(id: Int32, kind: Kind, at: Cell, hp: Int32): Enemy =
        { id = id, kind = kind, at = at, size = sizeOf(kind), hp = hp, rest = Rest.Dormant, turns = 0, burn = 0, facing = Board.Dir8.Down, intent = Intent.Idle, cooldown = 0 }

    /// 全部の種類。
    pub def all(): List[Kind] = Kind.Jelly :: Kind.Bat :: Kind.Archer :: Kind.Swarm :: Kind.Shield :: Kind.Charger :: Kind.Bomber :: Kind.Boss :: Nil

    /// 体のマスの幅（ボスは 2×2）。
    pub def sizeOf(kind: Kind): Int32 = if (kind == Kind.Boss) 2 else 1

    /// ボスか（押されない・眠り玉と煙玉で眠らない・罠を踏まない・倒すと潜りが終わる）。
    pub def isBoss(kind: Kind): Bool = kind == Kind.Boss

    /// 体の真ん中（マスの座標。1 マスの敵は at、2×2 は 4 マスの間）。見せる側の位置はここから引く。
    pub def centerOf(e: Enemy): (Float64, Float64) =
        let (x, y) = e#at;
        let half = Int32.toFloat64(e#size - 1) / 2.0;
        (Int32.toFloat64(x) + half, Int32.toFloat64(y) + half)

    /// ボスの帯の上の行: 主人公の行と、ボスに近い側の 1 行（主人公がボスの行にいればボスの 2 行）。
    pub def bandTop(e: Enemy, heroAt: Cell): Int32 =
        let (_, y) = e#at;
        let (_, hy) = heroAt;
        if (hy < y) hy
        else if (hy >= y + e#size) hy - 1
        else y

    /// 溜めた技が叩きつけを含むか。
    pub def slams(intent: Intent): Bool = match intent {
        case Intent.Slam            => true
        case Intent.SlamAndSweep(_) => true
        case _                      => false
    }

    /// 溜めた技の帯の上の行（帯を含まなければ None）。
    pub def sweepTop(intent: Intent): Option[Int32] = match intent {
        case Intent.Sweep(top)        => Some(top)
        case Intent.SlamAndSweep(top) => Some(top)
        case _                        => None
    }

    pub def behaviourOf(kind: Kind): Behaviour = match kind {
        case Kind.Jelly  => Behaviour.Chase
        case Kind.Swarm  => Behaviour.Chase
        case Kind.Shield => Behaviour.Chase
        case Kind.Bat    => Behaviour.Flutter
        case Kind.Archer => Behaviour.Shoot
        case Kind.Charger => Behaviour.Charge
        case Kind.Bomber => Behaviour.Blast
        case Kind.Boss   => Behaviour.Boss
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
