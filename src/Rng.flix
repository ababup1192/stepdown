// Rng — Run の中に持つ乱数（splitmix64）。状態は Int64 1 つで、引くたびに次の状態を返す（effect は使わない）。
mod Rng {

    def golden(): Int64 = -7046029254386353131i64

    /// 1 回引く: (出た数, 次の状態)。
    pub def next(state: Int64): (Int64, Int64) =
        let s = state + golden();
        let z1 = Int64.bitwiseXor(s, Int64.logicalRightShift(dist = 30, s)) * -4658895280553007687i64;
        let z2 = Int64.bitwiseXor(z1, Int64.logicalRightShift(dist = 27, z1)) * -7723592293110705685i64;
        (Int64.bitwiseXor(z2, Int64.logicalRightShift(dist = 31, z2)), s)

    /// 0 以上 n 未満の整数（n が 1 未満なら 0）。
    pub def below(n: Int32, state: Int64): (Int32, Int64) =
        let (z, s) = next(state);
        if (n < 1) (0, s)
        else (Int64.remainder(Int64.logicalRightShift(dist = 1, z), Int32.toInt64(n)) |> Int64.truncateToInt32, s)

    /// 確率 p で true。
    pub def chance(p: Float64, state: Int64): (Bool, Int64) =
        let (z, s) = next(state);
        let unit = Int64.toFloat64(Int64.logicalRightShift(dist = 11, z)) / 9007199254740992.0;
        (unit < p, s)

    /// 重みの列から 1 つ選んだ番号（重みが全部 0 以下なら 0）。
    pub def weighted(weights: List[Float64], state: Int64): (Int32, Int64) =
        let (z, s) = next(state);
        let total = weights |> List.filter(w -> w > 0.0) |> List.foldLeft((acc, w) -> acc + w, 0.0);
        let point = Int64.toFloat64(Int64.logicalRightShift(dist = 11, z)) / 9007199254740992.0 * total;
        let (index, _) = weights |> List.zipWithIndex |> List.foldLeft((acc, iw) -> {
            let ((found, left), (i, w)) = (acc, iw);
            let width = Float64.max(0.0, w);
            if (found >= 0) acc else if (left < width) (i, left) else (-1, left - width)
        }, (-1, point));
        (Int32.max(0, index), s)

    /// 2 つの数から種を作る（周回の種と階の番号 → その階の種）。
    pub def mix(a: Int64, b: Int64): Int64 = fst(next(Int64.bitwiseXor(a, b * golden())))

    /// 列から 1 つずつ抜いて count 個選ぶ（選んだ順に返す）。
    pub def pick(count: Int32, items: List[a], state: Int64): (List[a], Int64) =
        pickInto(count, List.toVector(items), Nil, state)

    def pickInto(count: Int32, pool: Vector[a], chosen: List[a], state: Int64): (List[a], Int64) =
        if (count <= 0 or Vector.isEmpty(pool)) (List.reverse(chosen), state)
        else {
            let (i, s) = below(Vector.length(pool), state);
            let last = Vector.length(pool) - 1;
            let picked = Vector.get(i, pool);
            let rest = pool |> Vector.update(i, Vector.get(last, pool)) |> Vector.take(last);
            pickInto(count - 1, rest, picked :: chosen, s)
        }
}
