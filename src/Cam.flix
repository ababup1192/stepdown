// Cam — 斜め上から見下ろす固定のカメラ（純粋）。World が注目点をなめらかに追い、描く側がこの構えでカメラを組む。
mod Cam {
    use fge::{Camera3d, Num, Vec3}

    /// カメラの構え: 見下ろす角・向き（ラジアン。0 で北を見る）・注目点からの距離（m）・縦の画角（ラジアン）。
    pub type alias Rig = { pitch = Float64, yaw = Float64, distance = Float64, fovY = Float64 }

    pub def rad(d: Float64): Float64 = d * Num.pi() / 180.0

    /// 値 x（今の変わる速さ v）を target へ、およそ seconds 秒の遅れで寄せた次の (値, 速さ)。
    /// WhyNot: 毎コマ「残りの何割」を足す形にしない — 目標が急に変わると動き出しが角張る。速さを持ち越すと、動き出しも止まり際も丸くなる。
    pub def smoothDamp(seconds: Float64, dt: Float64, target: Float64, state: (Float64, Float64)): (Float64, Float64) =
        let (x, v) = state;
        let omega = 2.0 / Float64.max(0.0001, seconds);
        let k = omega * dt;
        let decay = 1.0 / (1.0 + k + 0.48 * k * k + 0.235 * k * k * k);
        let change = x - target;
        let temp = (v + omega * change) * dt;
        let next = target + (change + temp) * decay;
        if ((target > x) == (next > target)) (target, 0.0) else (next, (v - omega * temp) * decay)

    /// 構えの見ている向き。
    def forwardOf(rig: Rig): Vec3.Vec3 =
        Vec3.v3(Num.sin(rig#yaw) * Num.cos(rig#pitch), 0.0 - Num.sin(rig#pitch), Num.cos(rig#yaw) * Num.cos(rig#pitch))

    /// 注目点 focus を構え rig で見るカメラ。
    def cameraOf(focus: Vec3.Vec3, rig: Rig): Camera3d.Camera3d =
        { eye = Vec3.sub(focus, Vec3.mul(forwardOf(rig), rig#distance)), yaw = rig#yaw, pitch = rig#pitch, fovY = rig#fovY,
          near = 0.5, far = 80.0 }

    /// 見せ方の Doc の構えで、盤の点 (x 列, y 行。マス単位) を見るカメラ。注目点は盤の点を奥へ cameraLead だけずらした所。
    pub def ofLook(lk: LookDoc.Rec, x: Float64, y: Float64): Camera3d.Camera3d =
        let yaw = rad(lk#cameraYaw);
        let focus = Vec3.v3(x + Num.sin(yaw) * lk#cameraLead, 0.0, 0.0 - y + Num.cos(yaw) * lk#cameraLead);
        cameraOf(focus, { pitch = rad(lk#cameraPitch), yaw = yaw, distance = lk#cameraDistance, fovY = rad(lk#cameraFov) })
}
