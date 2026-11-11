// Tap — 画面のタップを盤の入力へ読む（純粋）。
//   押し始め: mouseClicked のコマ。確定: mouseDown が切れた最初のコマ（同じコマでもよい）。
//   捨てる: 押してから動いた距離が tapSlop を超えた・同じコマにダッシュの操作（スワイプ）が来た。
//   方向: 押した点からのレイが床（y = 0）と交わる所を、主人公のマスとの差の大きい軸へ寄せる。主人公のマスの中（半径 tapWaitRadius）は待つ。
//   下の UI の帯の中は盤の入力にしない。
mod Tap {
    use fge::{Camera3d, Clock, Ray3d, Vec2}
    use Board.{Cell, Dir}
    use Run.Input

    /// 押し始めたタップ（押した所と時刻）。
    pub type alias Press = { at = Vec2.Vec2, startedAt = Clock.Time[Clock.Real] }

    /// 1 コマのポインタ: 押し始めたか・押しているか・今の所・実時間・同じコマにダッシュの操作が来たか。
    pub type alias Pointer = { clicked = Bool, down = Bool, cursor = Vec2.Vec2, now = Clock.Time[Clock.Real], dashed = Bool }

    /// 1 コマ進めた (押している途中のタップ, このコマで確定したタップの点)。slop = 動いてもタップとみなす距離（px）。
    pub def track(slop: Float64, pointer: Pointer, press: Option[Press]): (Option[Press], Option[Vec2.Vec2]) =
        let started = if (pointer#clicked) Some({ at = pointer#cursor, startedAt = pointer#now }) else press;
        let kept = started |> Option.filter(p -> not pointer#dashed and Vec2.distanceTo(p#at, pointer#cursor) <= slop);
        if (pointer#down) (kept, None) else (None, kept |> Option.map(p -> p#at))

    /// 盤の入力にする所か（下の UI の帯 bandTop より上）。
    pub def isOnBoard(bandTop: Float64, point: Vec2.Vec2): Bool = point#y < bandTop

    /// 画面の点の下にある床の点の (列, 行)（小数。床と交わらなければ None）。
    def floorAt(camera: Camera3d.Camera3d, size: Vec2.Vec2, point: Vec2.Vec2): Option[(Float64, Float64)] =
        let ray = Camera3d.screenPointToRay(camera, size, point);
        let (origin, toward) = (Ray3d.origin(ray), Ray3d.direction(ray));
        if (toward#y > -0.000001) None
        else {
            let t = 0.0 - origin#y / toward#y;
            Some((origin#x + toward#x * t, 0.0 - (origin#z + toward#z * t)))
        }

    /// 床の点 (列, 行) と主人公のマスから入力: 半径 waitRadius の中は待つ、それ以外は差の大きい軸の向き。
    pub def inputToward(waitRadius: Float64, hero: Cell, spot: (Float64, Float64)): Input =
        let (hx, hy) = hero;
        let (dx, dy) = (fst(spot) - Int32.toFloat64(hx), snd(spot) - Int32.toFloat64(hy));
        if (dx * dx + dy * dy <= waitRadius * waitRadius) Input.Wait
        else if (Float64.abs(dx) > Float64.abs(dy)) Input.Move(Board.toDir8(if (dx > 0.0) Dir.Right else Dir.Left))
        else Input.Move(Board.toDir8(if (dy > 0.0) Dir.Down else Dir.Up))

    /// 盤の上のタップの入力（UI の帯の中・床と交わらない点は None）。
    pub def inputOf(view: { camera = Camera3d.Camera3d, size = Vec2.Vec2, bandTop = Float64 }, waitRadius: Float64, hero: Cell, point: Vec2.Vec2): Option[Input] =
        if (not isOnBoard(view#bandTop, point)) None
        else floorAt(view#camera, view#size, point) |> Option.map(inputToward(waitRadius, hero))
}
