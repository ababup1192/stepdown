// エントリーポイント。Doc は App を組む前に読み、その値から描く元（Res）を作る
// （壊れていても defaults で必ず起動する）。App の組み立ては Game.app（flow の入口 E2e とテストも同じ物を使う）。
// ここで足すのは main だけが知る物（周回の種・終了の確認・注釈・デバッグの印・開発の道具の /render）。
//
// 1 手の決まりは Step.flix、予告は Preview.flix。まず Run.flix と Step.flix の冒頭を読むとよい。
use fge::{App, LwjglLayer, Resources}

import java.lang.System

def main(): Unit \ IO =
    if (LwjglLayer.ensureMainThread()) ()
    else {
        let seed = System.currentTimeMillis();
        run {
            // Doc を読んで App を組む所ごと渡す（flow を記録する make debug の起動でも、ここで読んだ中身を使い Doc を読み直さない）。
            App.loadAndLaunch(() -> {
                Game.app(Controls.loadAll(), seed)
                    // 3D の場面・ブルーム・後処理を先に Pass へ描き、本編の面が読み返す。
                    |> App.withPassPlan(Resources.read((res, w) -> View.passPlan(res, w)))
                    // 窓の閉じるボタンも Esc と同じ確認へ回す（quitWhen を差し替えると既定の「Esc ですぐ終了」は外れる）。
                    |> App.withCloseRequest(Resources.mapWorld((_, w) -> World.askQuit(w)))
                    |> App.quitWhen((_, h) -> Resources.read((_, w) -> World.isQuitting(w), h))
                    // F8 で書き出す注釈チケットに、その瞬間の文脈と z の層の名前を添える（層の表は ZOrder）。
                    |> App.withAnnotationContext(Resources.read((res, w) ->
                        { facts = ("画面", "${w#screen}") :: ("状態", E2e.fingerprint(res, w)) :: ("終了の確認", "${w#quit}") :: Nil,
                          layers = ZOrder.layers(), hudLayers = (Nil: List[ZOrder.Layer]) }))
                    |> App.withDebug(App.debugFromEnv())
                    // WhyNot: Game.app で繋がない — Game.app はテスト・flow・ブラウザ版と共有するので、開発の道具（POST /render）を持たせない。
                    |> App.onRenderRequest(SceneRender.allShots)
            })
        } with LwjglLayer.withProject(".")
          with Fs.FileRead.runWithIO
          with Fs.FileWrite.runWithIO
    }
