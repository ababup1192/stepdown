// Item — アイテムの種類（段の並び）・系統・効果の種類。数値は items の Doc（ItemsDoc）、拾った時の決まりは Step。
// 段の並びはここの enum が決める（Doc で段は足せない）。
mod Item {

    /// アイテムの種類。系統ごとに下の段から並べる。
    pub enum Kind with Eq, Order, ToString {
        case WoodSword
        case IronSword
        case SilverSword
        case WoodShield
        case IronShield
        case SilverShield
        case WoodArrow
        case IronArrow
        case SilverArrow
        case FireArrow
        case SmallBomb
        case BigBomb
        case SleepBomb
        case Herb
        case Tonic
        case Elixir
        case Bread
        case Meat
        case Pebble
        case Smoke
        case IronBall
        case Thunder
    }

    /// 系統。剣と盾は数なし（枠の外の装備）。矢・小石・鉄球・爆弾・煙玉・かみなりの書・薬は数ありで持ち物の枠に入る。食べ物は持たずに止まったマスで食べる。
    pub enum Family with Eq, Order, ToString {
        case Sword
        case Shield
        case Arrow
        case Pebble
        case IronBall
        case Bomb
        case Smoke
        case Thunder
        case Potion
        case Food
    }

    /// 効果の種類（数値は items の Doc）。
    ///   Push    … 殴った敵を 1 マス押し出す
    ///   Deflect … 受ける攻撃をたまに弾く
    ///   Pierce  … 矢が敵を貫いて次の敵にも当たる
    ///   Burn    … 当たった敵が何手か燃える
    ///   Sleep   … 当たった敵を何手か眠らせる
    pub enum Effect with Eq, ToString {
        case Push
        case Deflect
        case Pierce
        case Burn
        case Sleep
    }

    /// 拾った時に起きたこと。
    ///   Upgraded  … 上の段（か初めて）を拾い、手持ちがその段になった
    ///   Stacked   … 同じか下の段を拾い、今の段の数に足した
    ///   Discarded … 下か同じ段の剣・盾で、拾って消えた
    pub enum Pick with Eq, ToString {
        case Upgraded
        case Stacked
        case Discarded
    }

    /// 全部の種類（並びは enum のまま）。
    pub def all(): List[Kind] =
        Kind.WoodSword :: Kind.IronSword :: Kind.SilverSword :: Kind.WoodShield :: Kind.IronShield :: Kind.SilverShield ::
        Kind.WoodArrow :: Kind.IronArrow :: Kind.SilverArrow :: Kind.FireArrow ::
        Kind.SmallBomb :: Kind.BigBomb :: Kind.SleepBomb :: Kind.Herb :: Kind.Tonic :: Kind.Elixir :: Kind.Bread :: Kind.Meat :: Kind.Pebble :: Kind.Smoke :: Kind.IronBall :: Kind.Thunder :: Nil

    /// Doc に書いた名前（enum の名前のまま）から種類へ。
    pub def fromName(name: String): Option[Kind] = all() |> List.find(kind -> "${kind}" == name)

    pub def familyOf(kind: Kind): Family = match kind {
        case Kind.WoodSword    => Family.Sword
        case Kind.IronSword    => Family.Sword
        case Kind.SilverSword  => Family.Sword
        case Kind.WoodShield   => Family.Shield
        case Kind.IronShield   => Family.Shield
        case Kind.SilverShield => Family.Shield
        case Kind.WoodArrow    => Family.Arrow
        case Kind.IronArrow    => Family.Arrow
        case Kind.SilverArrow  => Family.Arrow
        case Kind.FireArrow    => Family.Arrow
        case Kind.SmallBomb    => Family.Bomb
        case Kind.BigBomb      => Family.Bomb
        case Kind.SleepBomb    => Family.Bomb
        case Kind.Herb         => Family.Potion
        case Kind.Tonic        => Family.Potion
        case Kind.Elixir       => Family.Potion
        case Kind.Bread        => Family.Food
        case Kind.Meat         => Family.Food
        case Kind.Pebble       => Family.Pebble
        case Kind.Smoke        => Family.Smoke
        case Kind.IronBall     => Family.IronBall
        case Kind.Thunder      => Family.Thunder
    }

    /// 系統の中の段（下から 0, 1, 2, …）。
    pub def tierOf(kind: Kind): Int32 = match kind {
        case Kind.WoodSword    => 0
        case Kind.IronSword    => 1
        case Kind.SilverSword  => 2
        case Kind.WoodShield   => 0
        case Kind.IronShield   => 1
        case Kind.SilverShield => 2
        case Kind.WoodArrow    => 0
        case Kind.IronArrow    => 1
        case Kind.SilverArrow  => 2
        case Kind.FireArrow    => 3
        case Kind.SmallBomb    => 0
        case Kind.BigBomb      => 1
        case Kind.SleepBomb    => 2
        case Kind.Herb         => 0
        case Kind.Tonic        => 1
        case Kind.Elixir       => 2
        case Kind.Bread        => 0
        case Kind.Meat         => 1
        case Kind.Pebble       => 0
        case Kind.Smoke        => 0
        case Kind.IronBall     => 0
        case Kind.Thunder      => 0
    }

    /// 持ち物の枠に入る系統か（剣・盾・食べ物は入らない）。
    pub def isPacked(family: Family): Bool = match family {
        case Family.Sword  => false
        case Family.Shield => false
        case Family.Food   => false
        case _             => true
    }

    /// 山なりに投げる石の系統か（小石と鉄球。届く距離は items の range、飛ぶ絵は石）。
    pub def isStone(family: Family): Bool = family == Family.Pebble or family == Family.IronBall

    /// その段だけが持つ効果。
    pub def effectOf(kind: Kind): Option[Effect] = match kind {
        case Kind.SilverSword  => Some(Effect.Push)
        case Kind.SilverShield => Some(Effect.Deflect)
        case Kind.IronArrow    => Some(Effect.Pierce)
        case Kind.SilverArrow  => Some(Effect.Pierce)
        case Kind.FireArrow    => Some(Effect.Burn)
        case Kind.SleepBomb    => Some(Effect.Sleep)
        case _                 => None
    }
}
