# 石のダンジョンの効果音 20 個（1 手の時刻表の Cue で鳴らす。名前は TurnTimeline.Sound と同じ sd_*）。
# bin/fge snd build art/stepdown.snd.star で art/sfx/*.sndsrc.json を書き、検査を通った物を atelier/sfx/<名前>.wav へ書く
# （採用した物を assets/sfx へ移し、project.json の sounds に載せる）。
# 石の部屋なので短い響きを少し足す。1 手は 0.1〜0.2 秒で回るので、どれも 0.25 秒以内に収め、続けて鳴っても濁らない長さにする。
# 立ち上がりは 2ms 以上（耳に刺さる「パチ」を出さない）。
# ファンファーレとカードの 3 つは遊びの時計が止まった間に鳴る（World.cardSounds）。ファンファーレだけ 0.6 秒前後まで伸ばす。

def tone(hz, ms, at = 0, amp = 0.35, decay = 60, to_hz = None, attack = 3, duty = 0.5, cut = 2.0):
    return {"at": at, "ops": [op.pulse(hz = hz, ms = ms, amp = amp, duty = duty, to_hz = to_hz, accumulate = to_hz != None),
                              op.lowpass(hz = hz * cut, stages = 2),
                              op.env(attack_ms = attack, decay_ms = decay)]}

def hiss(ms, at = 0, amp = 0.3, hz = 1800, decay = 30, attack = 3):
    return {"at": at, "ops": [op.noise(ms = ms, amp = amp), op.lowpass(hz = hz, stages = 2), op.env(attack_ms = attack, decay_ms = decay)]}

# 帯を動かす雑音（風切り・煙・吸い込み）。
def sweep(ms, at = 0, amp = 0.3, hz = 800, to_hz = 2400, attack = 10, decay = 60, mode = "band"):
    return {"at": at, "ops": [op.noise(ms = ms, amp = amp), op.svf(mode = mode, hz = hz, to_hz = to_hz, q = 0.7),
                              op.env(attack_ms = attack, decay_ms = decay), op.fade()]}

def sound(name, voices, room = 0.3, wet = 0.12, lufs = -21):
    snd.write_spec("//art/sfx/%s.sndsrc.json" % name, {
        "version": 1,
        "target": "//atelier/sfx/%s.wav" % name,
        "kind": "sfx",
        "style": "free",
        "channels": "mono",
        "voices": voices,
        "post": [op.reverb(room = room, wet = wet), op.finish(in_ms = 3, out_ms = 20), op.normalize(lufs = lufs)],
    })

def main():
    # 歩く: 石の床を靴で踏む低い「コッ」。
    sound("sd_step", [tone(180, 50, amp = 0.3, decay = 14, to_hz = 140), hiss(22, amp = 0.12, hz = 1500, decay = 7)], room = 0.25, wet = 0.08, lufs = -26)
    # 進めない: 壁に当たる鈍い「ゴツ」。
    sound("sd_bump", [tone(110, 80, amp = 0.35, decay = 24, to_hz = 80), hiss(30, amp = 0.15, hz = 700, decay = 10)], lufs = -25)
    # 殴る: 空を切る短い風切り。
    sound("sd_strike", [sweep(110, amp = 0.35, hz = 900, to_hz = 3200, attack = 8, decay = 40)], room = 0.2, wet = 0.06, lufs = -23)
    # 当たる: 柔らかい物を叩く「ボフッ」と高い粒。
    sound("sd_hit", [hiss(80, amp = 0.45, hz = 1200, decay = 22, attack = 2), tone(150, 70, amp = 0.35, decay = 20, to_hz = 90),
                     tone(900, 30, amp = 0.12, decay = 8, duty = 0.25)], lufs = -20)
    # 盾で弾く: 金属の「キン」（離れた 2 音）。
    sound("sd_guard", [tone(1320, 160, amp = 0.22, decay = 70, duty = 0.25, cut = 3.0), tone(1980, 120, amp = 0.12, decay = 50, duty = 0.25, cut = 3.0)], room = 0.35, wet = 0.15)
    # 矢: 弦の「ビン」と、飛ぶ風切り。
    sound("sd_arrow", [tone(330, 60, amp = 0.25, decay = 18, to_hz = 300, duty = 0.25), sweep(180, at = 20, amp = 0.28, hz = 2600, to_hz = 1200, decay = 90)], lufs = -22)
    # 爆弾: 低い破裂と、尾を引く煙の雑音。
    sound("sd_bomb", [tone(70, 240, amp = 0.6, decay = 90, to_hz = 38), hiss(260, amp = 0.55, hz = 1400, decay = 80, attack = 2),
                      sweep(420, at = 60, amp = 0.25, hz = 600, to_hz = 200, attack = 40, decay = 200, mode = "low")], room = 0.5, wet = 0.2, lufs = -18)
    # 拾う: 上がる 2 音。
    sound("sd_pick", [tone(660, 60, amp = 0.22, decay = 30, duty = 0.25), tone(990, 90, at = 55, amp = 0.22, decay = 45, duty = 0.25)], lufs = -23)
    # 上位を拾う: 上がる 3 音（長 3 和音）ときらめき。
    sound("sd_upgrade", [tone(523, 70, amp = 0.22, decay = 40, duty = 0.25), tone(659, 70, at = 60, amp = 0.22, decay = 40, duty = 0.25),
                         tone(784, 200, at = 120, amp = 0.24, decay = 110, duty = 0.25), hiss(160, at = 120, amp = 0.06, hz = 6000, decay = 60)],
          room = 0.4, wet = 0.18, lufs = -21)
    # 敵が倒れる: 下がる音と、しぼむ雑音。
    sound("sd_defeat", [tone(440, 220, amp = 0.25, decay = 110, to_hz = 110, duty = 0.25), sweep(220, amp = 0.18, hz = 1600, to_hz = 300, decay = 90, mode = "low")], lufs = -22)
    # 薬: 泡の上がる粒（短い音を 4 つ、少しずつ高く）。
    sound("sd_potion", [tone(500 + 120 * i, 40, at = 50 * i, amp = 0.18, decay = 18, duty = 0.25) for i in range(4)] +
                       [sweep(240, amp = 0.1, hz = 500, to_hz = 1800, attack = 30, decay = 120)], room = 0.35, wet = 0.15, lufs = -23)
    # 階段: 下がる足音 3 つと、低い響き。
    sound("sd_stairs", [tone(200 - 30 * i, 60, at = 110 * i, amp = 0.3, decay = 18, to_hz = 150 - 25 * i) for i in range(3)] +
                       [tone(98, 400, at = 300, amp = 0.2, decay = 200)], room = 0.6, wet = 0.25, lufs = -21)
    # 主人公が倒れる: 下がる重い音と、崩れる雑音。
    sound("sd_fall", [tone(220, 500, amp = 0.32, decay = 260, to_hz = 55), hiss(300, at = 120, amp = 0.3, hz = 900, decay = 120)], room = 0.5, wet = 0.2, lufs = -20)
    # 食べる: 噛む短い 2 つの「モグ」と、小さく上がる音。
    sound("sd_eat", [hiss(45, amp = 0.3, hz = 900, decay = 14, attack = 3), hiss(45, at = 110, amp = 0.28, hz = 800, decay = 14, attack = 3),
                     tone(392, 60, at = 200, amp = 0.16, decay = 30, duty = 0.25), tone(523, 80, at = 250, amp = 0.16, decay = 40, duty = 0.25)], room = 0.2, wet = 0.08, lufs = -23)
    # 捨てる: 物が離れる下がる風切りと、石に落ちる小さな「コト」。
    sound("sd_discard", [sweep(140, amp = 0.25, hz = 2200, to_hz = 600, attack = 6, decay = 60), tone(240, 50, at = 150, amp = 0.28, decay = 14, to_hz = 180)], lufs = -23)
    # カードを選ぶ: 明るい 2 音が上がり、光って吸い込まれる短いきらめき。
    sound("sd_card", [tone(784, 60, amp = 0.2, decay = 30, duty = 0.25), tone(1175, 110, at = 50, amp = 0.2, decay = 60, duty = 0.25),
                      sweep(180, at = 40, amp = 0.08, hz = 3000, to_hz = 7000, attack = 20, decay = 90)], room = 0.35, wet = 0.15, lufs = -22)
    # レベルアップのファンファーレ: ド・ミ・ソと跳ねて、高いドを伸ばす（長 3 和音を重ねて締める）。
    sound("sd_fanfare", [tone(523, 90, amp = 0.22, decay = 50, duty = 0.25), tone(659, 90, at = 100, amp = 0.22, decay = 50, duty = 0.25),
                         tone(784, 90, at = 200, amp = 0.22, decay = 50, duty = 0.25),
                         tone(1047, 330, at = 300, amp = 0.24, decay = 220, duty = 0.25), tone(659, 300, at = 300, amp = 0.12, decay = 200, duty = 0.5),
                         tone(784, 300, at = 300, amp = 0.12, decay = 200, duty = 0.5), hiss(260, at = 300, amp = 0.05, hz = 7000, decay = 140)],
          room = 0.45, wet = 0.2, lufs = -20)
    # 1 回噛む: 短い「むしゃ」（低い擦れと、湿った小さな粒）。食べる間に 2〜3 回続けて鳴らすので 0.1 秒で切る。
    sound("sd_munch", [hiss(55, amp = 0.32, hz = 1100, decay = 16, attack = 3), tone(170, 45, amp = 0.18, decay = 14, to_hz = 120),
                       hiss(25, at = 30, amp = 0.12, hz = 3200, decay = 8, attack = 2)], room = 0.15, wet = 0.05, lufs = -24)
    # 煙玉が弾ける: 柔らかい「ぽふっ」（低い膨らみと、こもった息の雑音。爆弾の破裂のような芯は入れない）。
    sound("sd_puff", [tone(130, 90, amp = 0.25, decay = 40, to_hz = 90, attack = 8), sweep(260, amp = 0.32, hz = 500, to_hz = 1400, attack = 12, decay = 110, mode = "low")],
          room = 0.35, wet = 0.15, lufs = -24)
    # 小石が当たる: 軽い「コツン」（高めの硬い短音と、小さな跳ね返り）。
    sound("sd_clink", [tone(1250, 40, amp = 0.22, decay = 12, duty = 0.25, cut = 3.0), hiss(18, amp = 0.15, hz = 4000, decay = 5, attack = 2),
                       tone(1500, 25, at = 70, amp = 0.1, decay = 8, duty = 0.25, cut = 3.0)], room = 0.25, wet = 0.08, lufs = -24)
main()
