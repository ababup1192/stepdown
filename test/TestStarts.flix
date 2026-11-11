// 始まりの登録簿（Starts.all を App.startOrBug / App.buildStart で引く）とギャラリーの場面。
mod TestStarts {
    use fge::{App, Resources, Vec2}

    def app(): App.App[Resources.With[Res.Res, World.World], {}] \ Fs.FileRead = Game.app(Controls.loadDocs(), World.fixedSeed())

    /// play は 1F の始まりで、遊びの画面・主人公は地図の始まりの印・移動は 0。
    @Test
    def testPlayStartsTheFirstFloor(): Unit \ Assert + Fs.FileRead =
        let started = App.startOrBug(app(), "play", Map.empty());
        Assert.assertEq(expected = (World.Screen.Play, 1, true, 0),
            Resources.read((res, w) -> (w#screen, Run.rec(w#runState)#floor,
                Run.heroOf(w#runState)#at == World.ctx(res, w)#board#start, Run.rec(w#runState)#walked), started))

    /// ギャラリーの場面は全部、実機と同じ App から shot:<名前> の始まりで組める。
    @Test
    def testEveryShotBuildsFromTheApp(): Unit \ Assert + Fs.FileRead =
        let a = app();
        Assert.assertEq(expected = Nil,
            Shots.names() |> List.filter(name -> Result.isErr(App.buildStart(a, Vec2.zero(), App.shotStartName(name), Map.empty()))))
}
