// ブラウザ版の入口（bin/fge export-web だけが組む）。Main と同じ App を、開発の道具なしでブラウザの handler 一式の上に走らせる。
mod WebMain {
    use fge::{App, Resources, WebFs, WebLayer}

    import java.lang.System

    pub def main(): Unit \ IO =
        run {
            let app = Game.app(Controls.loadAll(), System.currentTimeMillis())
                |> App.withPassPlan(Resources.read((res, w) -> View.passPlan(res, w)))
                |> App.withCloseRequest(Resources.mapWorld((_, w) -> World.askQuit(w)))
                |> App.quitWhen((_, h) -> Resources.read((_, w) -> World.isQuitting(w), h));
            App.launchWithDevTools(App.DevTools.none(), app)
        } with WebLayer.withProject3d(".")
          with WebFs.runFileRead
          with WebFs.runFileWrite
}
