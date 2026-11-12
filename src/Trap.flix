// Trap — 罠の種類と、地図の字。罠は部屋の床にあり、誰か（主人公か歩く敵・押し出された敵）が入った手に効く（効き方は Step）。
// 一度踏まれるまでは見えない（見えている罠は Run.Rec の revealed）。方向の予告・ダッシュは見えている罠だけを見る。
mod Trap {

    /// 罠の種類。
    ///   PoisonDart … 毒矢（主人公に rules の trapDamage。盾は効かない）
    ///   Alarm      … 警報（階の始めの眠りの敵が全部起きかける）
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
