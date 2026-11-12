// Art — キャラとアイコンと演出の絵（assets/chars/・assets/icons/・assets/fx/）の名前と寸法（純粋）。
// project.json の textures に同じ名前で登録してある（未登録の名前は起動で落ちる）。画面なしの描き出しも同じ表を読む。
mod Art {

    /// 絵 1 枚: 登録の名前・PNG の場所・寸法（px）。
    pub type alias Picture = { name = String, path = String, width = Float64, height = Float64 }

    /// キャラの絵。
    pub enum Figure with Eq, ToString {
        case Hero
        case Jelly
        case Bat
        case Archer
        case Swarm
        case Shield
        case Charger
        case Bomber
        case Boss
    }

    /// アイコンの絵。
    pub enum Icon with Eq, ToString {
        case Sword
        case Shield
        case Arrow
        case Bomb
        case Herb
        case Bread
        case Meat
        case Pebble
        case Smoke
        case BottleEmpty
        case Bone
        case BreadBitten
        case PebbleOne
        case Thunder
        case IronBall
        case Doll
        case WoodShield
        case IronShield
        case Coin
        case TrapDart
        case TrapAlarm
        case TrapPit
    }

    /// 画面に重ねる演出の絵（assets/fx/）。
    pub enum Fx with Eq, ToString {
        case LevelUp
    }

    pub def figure(f: Figure): Picture = match f {
        case Figure.Hero   => { name = "chr_hero", path = "assets/chars/hero.png", width = 203.0, height = 320.0 }
        case Figure.Jelly  => { name = "chr_blob", path = "assets/chars/blob.png", width = 284.0, height = 320.0 }
        case Figure.Bat    => { name = "chr_bat", path = "assets/chars/bat.png", width = 496.0, height = 320.0 }
        case Figure.Archer => { name = "chr_archer", path = "assets/chars/archer.png", width = 261.0, height = 320.0 }
        case Figure.Swarm  => { name = "chr_swarm", path = "assets/chars/swarm.png", width = 364.0, height = 320.0 }
        case Figure.Shield => { name = "chr_shieldbearer", path = "assets/chars/shieldbearer.png", width = 230.0, height = 320.0 }
        case Figure.Charger => { name = "chr_charger", path = "assets/chars/charger.png", width = 343.0, height = 320.0 }
        case Figure.Bomber => { name = "chr_bomber", path = "assets/chars/bomber.png", width = 242.0, height = 320.0 }
        case Figure.Boss   => { name = "chr_boss", path = "assets/chars/boss.png", width = 863.0, height = 480.0 }
    }

    pub def icon(i: Icon): Picture =
        let square = (name: String) -> { name = "icon_${name}", path = "assets/icons/${name}.png", width = 128.0, height = 128.0 };
        match i {
            case Icon.Sword  => square("sword")
            case Icon.Shield => square("shield")
            case Icon.Arrow => square("arrow")
            case Icon.Bomb  => square("bomb")
            case Icon.Herb  => square("herb")
            case Icon.Bread  => square("bread")
            case Icon.Meat   => square("meat")
            case Icon.Pebble => square("pebble")
            case Icon.Smoke  => square("smoke")
            case Icon.BottleEmpty => square("bottle_empty")
            case Icon.Bone        => square("bone")
            case Icon.BreadBitten => square("bread_bitten")
            case Icon.PebbleOne   => square("pebble_one")
            case Icon.Thunder     => square("scroll_thunder")
            case Icon.IronBall    => square("ironball")
            case Icon.Doll        => square("doll")
            case Icon.WoodShield  => square("shield_wood")
            case Icon.IronShield  => square("shield_iron")
            case Icon.Coin        => square("coin")
            case Icon.TrapDart    => square("trap_arrow")
            case Icon.TrapAlarm   => square("trap_alarm")
            case Icon.TrapPit     => square("trap_pit")
        }

    pub def fx(f: Fx): Picture = match f {
        case Fx.LevelUp => { name = "fx_levelup", path = "assets/fx/levelup.png", width = 512.0, height = 234.0 }
    }

    def icons(): List[Icon] =
        Icon.Sword :: Icon.Shield :: Icon.Arrow :: Icon.Bomb :: Icon.Herb :: Icon.Bread :: Icon.Meat :: Icon.Pebble :: Icon.Smoke :: Icon.BottleEmpty :: Icon.Bone :: Icon.BreadBitten :: Icon.PebbleOne ::
        Icon.Thunder :: Icon.IronBall :: Icon.Doll :: Icon.WoodShield :: Icon.IronShield :: Icon.Coin :: Icon.TrapDart :: Icon.TrapAlarm :: Icon.TrapPit :: Nil

    /// 全部の絵（描き出しが読む）。
    pub def all(): List[Picture] =
        List.map(figure, Figure.Hero :: Figure.Jelly :: Figure.Bat :: Figure.Archer :: Figure.Swarm :: Figure.Shield :: Figure.Charger :: Figure.Bomber :: Figure.Boss :: Nil)
            ::: List.map(icon, icons()) ::: (fx(Fx.LevelUp) :: Nil)

    /// アイテムの系統の絵（食べ物はパン）。
    def iconOfFamily(family: Item.Family): Icon = match family {
        case Item.Family.Sword  => Icon.Sword
        case Item.Family.Shield => Icon.Shield
        case Item.Family.Arrow  => Icon.Arrow
        case Item.Family.Bomb   => Icon.Bomb
        case Item.Family.Potion => Icon.Herb
        case Item.Family.Food   => Icon.Bread
        case Item.Family.Pebble => Icon.Pebble
        case Item.Family.IronBall => Icon.IronBall
        case Item.Family.Smoke  => Icon.Smoke
        case Item.Family.Thunder => Icon.Thunder
        case Item.Family.Doll   => Icon.Doll
    }

    /// アイテムの種類の絵（食べ物と、木と鉄の盾は種類で絵が分かれる）。
    pub def iconOfKind(kind: Item.Kind): Icon = match kind {
        case Item.Kind.Meat       => Icon.Meat
        case Item.Kind.WoodShield => Icon.WoodShield
        case Item.Kind.IronShield => Icon.IronShield
        case _                    => iconOfFamily(Item.familyOf(kind))
    }

    /// 罠の床の板の絵。
    pub def iconOfTrap(kind: Trap.Kind): Icon = match kind {
        case Trap.Kind.PoisonDart => Icon.TrapDart
        case Trap.Kind.Alarm      => Icon.TrapAlarm
        case Trap.Kind.Pit        => Icon.TrapPit
    }
}
