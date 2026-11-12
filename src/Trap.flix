// Trap — 罠の種類と、地図の字。罠は部屋の床に見えていて、主人公が歩いて入った手に効く（効き方は Step）。
// 敵は自分で歩いて乗っても効かず、押し出されて乗った時だけ hitsPushed の罠が効く。
mod Trap {

    /// 罠の種類。
    ///   PoisonDart … 毒矢（主人公に rules の trapDamage。盾は効かない）
    ///   Alarm      … 警報（階の眠っている敵が全部起きかける）
    ///   Pit        … 落とし穴（次の階へ落ちる）
    pub enum Kind with Eq, Order, ToString {
        case PoisonDart
        case Alarm
        case Pit
    }

    /// 地図の字 → 罠。
    pub def fromGlyph(ch: Char): Option[Kind] = match ch {
        case '^' => Some(Kind.PoisonDart)
        case '!' => Some(Kind.Alarm)
        case 'o' => Some(Kind.Pit)
        case _   => None
    }
}
