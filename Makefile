# ゲーム開発の入口。
#   make        … ゲームを起動する（ウィンドウが開く）
#   make help   … 共通の口の一覧（run / test / check / 描き出し / status …）
#
# エンジンや java の場所はマシンごとに違うので、git に入れない local.mk に書く
# （make new-game / Studio が生成する。別のマシンで clone したら 1 度だけ書き直す）:
#   ENGINE := /abs/path/to/flix_game_engine   （Studio 同梱なら …/resources/engine）
#   JAVA   := /abs/path/to/java.exe           （Windows のみ。Studio 同梱 JRE の java.exe）
# 既定の ../.. は「テンプレを engine リポの中でそのまま動かす」ための値。
-include local.mk
ENGINE ?= ../..

# 描き出せる場面の名前（source of truth は Shots.names。ここは使い方に出す写し）。
# 場面を足したら Shots.names と一緒にここも足す。
SHOTS := title play dead clear quit arrow bomb dirs popups swap levelup_mid levelup chosen

# ENGINE の場所が違うと include ごと落ち、make は「そんなファイルは無い」としか言わない。
# 先に自前で見て、直し方の 1 行を出してから止める（この検査が無いと make status も塞がる）。
ifeq ($(shell test -f "$(ENGINE)/mk/game.mk" && echo ok),)
$(error ENGINE が見当たりません ($(ENGINE)/mk/game.mk が無い) — local.mk に「ENGINE := /abs/path/to/flix_game_engine」を書くか、make <対象> ENGINE=... で指定してください。Studio 同梱なら …/resources/engine)
endif

# 共通の口（run / test / check / 描き出し / status …）はエンジンが持つ。
# 写しではないので、エンジンを上げると直しがそのまま届く。
# include は make の構文でクォートが効かず、空白でパスが分割される。
# Studio 同梱の engine は "/Applications/Flix GE Studio.app/…" と空白を含むので、
# 空白を退避してから渡す（レシピの中の $(ENGINE) は "" で括ってあるのでそのままでよい）。
space := $(subst ,, )
include $(subst $(space),\$(space),$(ENGINE))/mk/game.mk

# コミットで .flix か lint/ を触ったら、lint/rules.flix の決まり（依存の向き・View が World を受けない・テストの形）も見る。
# flixlint を取れない時（オフラインの初回など）は止めずに知らせる（make lint-flixlint で後から流せる）。
.PHONY: precommit-local
precommit-local:
	@if grep -qE '\.flix$$|^lint/' "$(STAGED_FILE)"; then \
	  fge=bin/fge; [ -x "$$fge" ] || fge="$(ENGINE)/bin/fge"; \
	  if "$$fge" flixlint-prepare > /dev/null; then $(MAKE) --no-print-directory lint-flixlint; \
	  else echo "[pre-commit] flixlint を用意できないので lint/rules.flix の検査は飛ばします（後で make lint-flixlint）"; fi; \
	fi

# 紹介の動画 = debug/video/<flow の名前>.mp4（540×960・30 fps）。手順は e2e/<名前>.flow（撮るのは mark in_ / out_ の間だけ）。
#   make video FLOW=e2e/showcase.flow   出力の名前は OUT=<名前> で替えられる
VIDEO_FLOW = $(or $(FLOW),e2e/showcase.flow)
VIDEO_OUT = $(or $(OUT),$(basename $(notdir $(VIDEO_FLOW))))
VIDEO_FPS ?= 30
VIDEO_X264 ?= -c:v libx264 -preset medium -crf 18 -pix_fmt yuv420p
.PHONY: video
video:
	rm -rf debug/video/$(VIDEO_OUT) debug/video/$(VIDEO_OUT).mp4
	VIDEO_FPS=$(VIDEO_FPS) VIDEO_FLOW=$(VIDEO_FLOW) VIDEO_OUT=$(VIDEO_OUT) VIDEO_X264="$(VIDEO_X264)" $(HEADLESS_RUN) VideoRender.frames
	@if [ -f debug/video/$(VIDEO_OUT)/f0000.png ]; then \
	  ffmpeg -y -loglevel error -framerate $(VIDEO_FPS) -i debug/video/$(VIDEO_OUT)/f%04d.png $(VIDEO_X264) -movflags +faststart debug/video/$(VIDEO_OUT).mp4; fi
	@echo "[video] debug/video/$(VIDEO_OUT).mp4"

# 自動で遊ぶ 3 人（でたらめ・避ける人・戦う人）の数: クリア率・死亡率・手数・満腹度・レベル・判断の割合・予告の時間。
#   make bots   種の数は SEEDS=<数>（既定 200）・1 回の手の上限は LIMIT=<数>（既定 2000）・ONLY=Fighter などで 1 人に絞る
.PHONY: bots
bots:
	SEEDS=$(SEEDS) LIMIT=$(LIMIT) ONLY=$(ONLY) $(HEADLESS_RUN) BotRun.main
