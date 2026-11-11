// Starts — 始まりの登録簿（App.withStarts。Game が Resources.liftStart で組の行へ持ち上げる）。名前と引数から、新しく作った初期の World を場面へ組む。
// 起動（START=play）・リモートの POST /start・flow の `start play`・テストの App.startOrBug が同じ表を引く。
mod Starts {
    use fge::Resources

    /// 登録簿。先頭が起動の既定。
    pub def all(): List[Resources.Start[Res.Res, World.World]] =
        { name = "title", doc = "題名の画面（起動の既定）", args = Map.empty(), build = (_, w, _) -> Ok(w) } ::
        { name = "play", doc = "1F の始まり（周回の種は起動の World のまま）", args = Map.empty(), build = (res, w, _) -> Ok(World.begin(res, w)) } :: Nil
}
