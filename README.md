# ひとあし迷宮

縦 540×960 のスマホブラウザ向けターン制ローグライク。入力は方向だけで、行動は立ち位置の法則で決まる（計画は `plans/plan.md`）。
今の画面は決まりを確かめるための仮の 2D（マスを色の四角・駒を丸と字）で、斜め上から見下ろす 3D に描き直す前の段。

## 始め方

| コマンド | 何をするか |
|---|---|
| `make run`    | ゲームを起動する（矢印で 1 歩・Space で待つ・Shift＋矢印か W A S D でダッシュ・Enter で決定） |
| `make check`  | 型検査だけ走らせる（一番速い確認） |
| `make test`   | テストを実行する |
| `make render SHOT=play` | その場面だけ `debug/play.png` へ描き出す（場面は title play dead clear quit） |
| `make flow FLOW=e2e/level1.flow` | 題名から 1F を始めて歩くまでを画面なしで流す |

## 読む順

1. `src/Run.flix` … 決まりの状態（Run）・1 手の入力と出来事・判定に使う Doc の束（Ctx）。
2. `src/Step.flix` … 1 手（殴る・動いて拾う → 爆弾か矢 → 敵が起きる → 敵の手番 → 薬・倒れる → 自然回復）。
3. `src/Preview.flix`（要約と予告）・`src/Dash.flix`（ダッシュ）・`src/Floor.flix`（階を作る）・`src/Board.flix`（マスの性質の表と部屋）。
4. `src/World.flix` … 見せる状態（画面・今見せている 1 手・予告の作り置き・遊びの時計）。`src/Controls.flix` がキーを当てる。
5. `src/View.flix` … 仮の 2D の画面。

数値と文言は `assets/stepdown.{rules,items,enemies,floors,look,text}.json`（保存即反映。schema と `project.json` の `editor.resources` で Studio から触れる）。

## AI エージェント向け指針の配布（sync-agents）

エンジン側で `make sync-agents GAME=/path/to/このゲーム` を実行すると、エンジン共通の
エージェント指針（agents-pack/AGENTS.core.md）とこのゲームの `AGENTS.local.md` を連結した
`AGENTS.md` が生成され、共通スキルが `.claude/skills/` にコピーされます。`AGENTS.md` は
生成物なので直接編集せず、ゲーム固有の原則は `AGENTS.local.md` に書いて再 sync してください。

## 字

- しっぽり明朝（`assets/ShipporiMincho-Regular.subset.ttf`）— Copyright 2021 The Shippori Mincho Project Authors。SIL Open Font License 1.1。engine の templates/sound-novel-starter から写した subset。
