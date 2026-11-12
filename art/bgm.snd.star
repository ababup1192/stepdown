# 石のダンジョンの曲の候補 4 本（聞き比べ用。まだ project.json には載せない）。
# bin/fge snd build art/bgm.snd.star で譜面（メモリの中）から鳴らし、検査を通った物を atelier/bgm/<名前>.wav へ書く。
# 1 手 0.1 秒の効果音が上に乗るので、どれも打つ音の立ち上がりを丸め、中域（効果音の芯がいる 150〜1500Hz）は薄く、
# 旋律は高い所か低い所に逃がす。輪は整数小節で頭に戻る（intro 無しの body だけ）。

GAIN = 0.4

# 何十分も流すので kind=bgm の目安（-16）の幅の中で少し下げる。
LUFS = -18

REVERB = {"room": 0.75, "damp": 0.6, "wet": 0.28, "predelay_ms": 20}

def song(name, chip):
    snd.write_spec("//art/bgm/%s.sndsrc.json" % name, {
        "version": 1,
        "target": "//atelier/bgm/%s.wav" % name,
        "kind": "bgm",
        "style": "free",
        "voices": [{"ops": [op.song(chip = chip)]}],
        "post": [op.normalize(lufs = LUFS)],
    })

def musicbox():
    # オルゴール: ヘ長調・72 BPM・16 小節。高い所の FM のベルと、遅れて返す写しと、低いサインのベースだけ。
    p = mus.chords("F | C | Dm | Am | F | Bb | Gm C | F | Dm | Am | Bb | C | F | Bb | F C | F")
    mel = mus.mml("o6 c4 f8 a8 g4 f4 | e4. f8 g2 | a4 g8 f8 d4 f4 | c2. r4 | " +
                  "c4 f8 a8 o7 c4 o6 a4 | b-4. a8 g2 | a8 g8 f8 e8 d4 e4 | f2. r4 | " +
                  "d4 f8 a8 g4 f4 | e4. d8 c2 | d4 e8 f8 g4 a4 | g2. r4 | " +
                  "a4 o7 c8 o6 a8 g4 f4 | b-4 a8 g8 f4 d4 | c4 f4 e4. g8 | f2. r4", inst = "bell")
    snd.write_chip("//art/bgm/bgm_musicbox.chip.json", {
        "bpm": 72, "gain": GAIN, "fx": {"reverb": REVERB},
        "instruments": {
            "bell": {"vol": "11 10 9 8 7 6 5 4 3 2 1 0", "speed": 3},
            "bass": {"vol": "3 6 8 8 7 7 6 | 6 / 4 2 1 0", "speed": 2},
        },
        "channels": [
            {"name": "bell", "part": "sine", "vol": 0.55, "pan": -0.2, "fm": {"ratio": 3.5, "index": 1.5, "decay_ms": 350, "floor": 0.05}, "send": {"reverb": 0.6}},
            {"name": "echo", "part": "sine", "vol": 0.3, "pan": 0.35, "fm": {"ratio": 3.5, "index": 1.0, "decay_ms": 300, "floor": 0.05}, "send": {"reverb": 0.8}},
            {"name": "bass", "part": "sine", "vol": 0.7, "send": {"reverb": 0.2}},
        ],
        "body": {"bell": mel,
                 "echo": mus.echo(mel, steps = 3, vol = -3),
                 "bass": mus.bass(p, "x-------x-------", ["1", "5"], inst = "bass", octave = 2)},
    })
    song("bgm_musicbox", "//art/bgm/bgm_musicbox.chip.json")

def cave():
    # 洞窟のアンビエント: イ短調・56 BPM・12 小節。ローパスを掛けたノコギリの和音を 3 本でゆっくり膨らませ、
    # 小節の終わりで先に離して次の膨らみへ受け渡す。上で時々、下から上がる短い水滴の音が落ちる。
    p = mus.chords("Am | Fmaj7 | C | G | Am | Fmaj7 | Dm7 | Esus4 | Am | Fmaj7 | Dm7 | E")
    hold = "x-----------...."
    drips = mus.mml("r1 | r2. o7 e16 r16 r8 | r4 o6 a16 r16 r8 r2 | r1 | " +
                    "r2 o7 c16 r16 r8 r4 | r4. o6 g16 r16 r2 | r1 | r2. o7 d16 r16 o6 a16 r16 | " +
                    "r1 | r4 o7 e16 r16 r8 r2 | r2 r8 o7 e16 r16 r4 | r1", inst = "drip")
    pad = {"mode": "low", "hz": 750, "q": 0.7}
    uni = {"voices": 3, "cents": 9, "width": 0.7}
    snd.write_chip("//art/bgm/bgm_cave.chip.json", {
        "bpm": 56, "gain": GAIN, "fx": {"reverb": {"room": 0.9, "damp": 0.55, "wet": 0.35, "predelay_ms": 30},
                                        "delay": {"steps": 6, "feedback": 0.45, "wet": 0.3, "pingpong": True, "lowpass": 3000}},
        "instruments": {
            "pad": {"vol": "5 6 7 8 9 10 | 10 / 9 8 7 6 5 3 1 0", "speed": 6},
            "drip": {"vol": "9 6 4 2 1 0"},
            "low": {"vol": "4 5 6 7 | 7 / 6 5 5 4 3 1 0", "speed": 6},
        },
        "channels": [
            {"name": "pad1", "part": "saw", "vol": 0.32, "pan": -0.3, "filter": pad, "unison": uni, "send": {"reverb": 0.6}},
            {"name": "pad2", "part": "saw", "vol": 0.28, "pan": 0.3, "filter": pad, "unison": uni, "send": {"reverb": 0.6}},
            {"name": "pad3", "part": "saw", "vol": 0.22, "filter": pad, "unison": uni, "send": {"reverb": 0.7}},
            {"name": "drip", "part": "sine", "vol": 0.35, "pitchenv": {"semis": -9, "ms": 18}, "send": {"reverb": 0.9, "delay": 0.8}},
            {"name": "low", "part": "sine", "vol": 0.6},
        ],
        "body": {"pad1": mus.arp(p, [0], hold, inst = "pad", octave = 3),
                 "pad2": mus.arp(p, [2], hold, inst = "pad", octave = 3),
                 "pad3": mus.arp(p, [1], hold, inst = "pad", octave = 4),
                 "drip": drips,
                 "low": mus.bass(p, hold, ["1"], inst = "low", octave = 2)},
    })
    song("bgm_cave", "//art/bgm/bgm_cave.chip.json")

def camp():
    # やさしいチップチューン: ト長調・92 BPM・16 小節。細い矩形波（25% / 12.5%）をローパスで丸め、立ち上がりも数 tick かけて膨らませる。
    # 太鼓は入れない（効果音の打つ音とぶつかる）。
    p = mus.chords("G | Em | C | D | G | Em | Am D | G | C | D | Bm | Em | Am | D | G | D")
    mel = mus.mml("o5 b4 a8 g8 d4 g4 | e4. f+8 g4 b4 | o6 c4 o5 b8 a8 g4 e4 | f+2. r4 | " +
                  "b4 a8 g8 d4 g4 | b4. o6 c8 d4 o5 b4 | a4 g8 a8 f+4 d4 | g2. r4 | " +
                  "e4 g8 o6 c8 e4 d4 | o5 a4. b8 a4 f+4 | f+4 d8 f+8 b4 a4 | g4. f+8 e2 | " +
                  "c4 e8 a8 o6 c4 o5 a4 | a4 f+8 a8 d4 f+4 | g2. b8 a8 | f+2 r4 d4", inst = "lead")
    snd.write_chip("//art/bgm/bgm_camp.chip.json", {
        "bpm": 92, "gain": GAIN, "fx": {"reverb": {"room": 0.5, "damp": 0.6, "wet": 0.18}},
        "instruments": {
            "lead": {"duty": "25", "vol": "4 6 8 9 | 8 / 6 4 2 1 0", "vib": "5 10 14"},
            "pluck": {"duty": "12", "vol": "3 6 5 4 3 2 1 0", "speed": 2},
            "tri": {"vol": "6 10 | 10 / 6 3 0"},
        },
        "channels": [
            {"name": "lead", "part": "pulse", "vol": 0.42, "filter": {"mode": "low", "hz": 1800, "q": 0.7}, "send": {"reverb": 0.4}},
            {"name": "echo", "part": "pulse", "vol": 0.18, "pan": 0.4, "filter": {"mode": "low", "hz": 1400, "q": 0.7}, "send": {"reverb": 0.6}},
            {"name": "arp", "part": "pulse", "vol": 0.26, "pan": -0.35, "filter": {"mode": "low", "hz": 1300, "q": 0.7}, "send": {"reverb": 0.4}},
            {"name": "bass", "part": "triangle", "vol": 0.55},
        ],
        "body": {"lead": mel,
                 "echo": mus.echo(mel, steps = 3, vol = -4),
                 "arp": mus.arp(p, [0, 1, 2, 1], "x-x-x-x-x-x-x-x-", inst = "pluck", octave = 4),
                 "bass": mus.bass(p, "x-------x-------", ["1", "5"], inst = "tri", octave = 2)},
    })
    song("bgm_camp", "//art/bgm/bgm_camp.chip.json")

def harp():
    # ハープ: ニ短調・80 BPM・16 小節。低めの分散和音を 8 分で淡々と回し、5〜8 小節と 13〜16 小節だけ柔らかい笛の旋律が乗る。
    p = mus.chords("Dm | Bb | Gm | A | Dm | Bb | Gm | A7 | F | C | Bb | A | Dm | Gm | Bb A | Dm")
    phrase1 = mus.mml("o5 a2 f4 e4 | d2. f4 | g4. a8 b-4 a4 | c+2 e4 r4", inst = "flute")
    phrase2 = mus.mml("o5 a4. g8 f4 d4 | b-2 g4 d4 | f4 d4 e4 c+4 | d2. r4", inst = "flute")
    snd.write_chip("//art/bgm/bgm_harp.chip.json", {
        "bpm": 80, "gain": GAIN, "fx": {"reverb": REVERB},
        "instruments": {
            "harp": {"vol": "11 10 9 8 7 6 5 4 3 2 1 0", "speed": 3},
            "flute": {"vol": "2 4 6 7 8 | 8 / 6 4 2 1 0", "speed": 2, "vib": "4 12 20"},
            "bass": {"vol": "3 6 7 | 7 / 5 3 1 0", "speed": 3},
        },
        "channels": [
            {"name": "harp", "part": "sine", "vol": 0.5, "pan": -0.15, "fm": {"ratio": 2, "index": 1.4, "decay_ms": 220, "floor": 0.1}, "send": {"reverb": 0.5}},
            {"name": "flute", "part": "sine", "vol": 0.38, "pan": 0.2, "send": {"reverb": 0.6}},
            {"name": "bass", "part": "sine", "vol": 0.6},
        ],
        "body": {"harp": mus.arp(p, [0, 1, 2, 3, 4, 3, 2, 1], "x-x-x-x-x-x-x-x-", inst = "harp", octave = 3),
                 "flute": mus.rest(4) + phrase1 + mus.rest(4) + phrase2,
                 "bass": mus.bass(p, "x---------------", ["1"], inst = "bass", octave = 2)},
    })
    song("bgm_harp", "//art/bgm/bgm_harp.chip.json")

def main():
    musicbox()
    cave()
    camp()
    harp()
main()
