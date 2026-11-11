// Game — ゲーム 1 本の App の組み立て。実機（Main）・flow の入口（E2e）・テストが同じ App を使う
// （同じ物を 3 か所で組み立てると、系の配線を 1 か所だけ直した時に実機と flow がずれる）。
// main だけが知る物（デバッグの印・ウィンドウ・終了の確認）は Main で足す。
// App の w は描く元（Res）と遊びの状態（World）の組。系・絵・問いは Resources の口で両方を受ける。
mod Game {
    use fge::{App, Resources}

    /// seed は周回の種（flow とテストは World.fixedSeed・実機は起動時刻）。
    pub def app(docs: Res.Docs, seed: Int64): App.App[Resources.With[Res.Res, World.World], {}] =
        let res = Res.build(docs);
        App.game({
            init = Resources.of(res, World.initial(res, seed)),
            update = Resources.system(Controls.step),
            view = Resources.view(View.frame),
            reloads = Nil
        })
            |> App.withDocTable(Controls.docTable())
            // 遊びの時計（Cards と終了の確認で止まる）。
            |> Resources.addClock({ clockOf = World.clockOf, withClock = World.withClock, paceOf = (_, w) -> World.paceOf(w) })
            // 音: 一発の音は今見せている 1 手の時刻表の Cue（区間は遊びの時計が持つ）と、カードの前後の World から。大きさはミキサーのバス（stepdown.mixer.json）。
            |> App.withAudio(ws -> Resources.read((r, after) -> Controls.sounds(r, { before = Resources.read((_, prev) -> prev, ws#before), after = after }), ws#after))
            |> App.withMixer(Resources.read((r, _) -> r#mixer))
            // 好きな場面から起動する・テストと flow が場面を名前で組む（START=play など）。
            |> App.withStarts(Starts.all() |> List.map(Resources.liftStart))
            |> App.withShots(Resources.shotsOf(Shots.names(), Shots.worldOf))
            // e2e/<名前>.flow の語彙（役・問い）と、記録の再生が比べる指紋。
            |> App.withFlow(E2e.table())
            |> App.withFingerprint(Resources.read(E2e.fingerprint))
            // 1 行の要点（flow の失敗・リモートの /step が最後の World として見せる）。
            |> App.withStatusLine(Resources.read(E2e.fingerprint))
}
