// PostView — 画面の後処理（ブルーム・周辺の暗さ・トーンマップ・彩度・FXAA）の Pass の並びと、本編の面（純粋）。
// 場面の中身は呼ぶ側（View）が渡す。場面から bloomB までは 1 を超える明るさを持つ Pass。
mod PostView {

    use fge::{Color, PostProcess, Render, RenderTarget, ShaderDoc, Vec2}
    use fge::ShaderDoc.{BinOp, Field, Shade, Out, TexChan}

    def scenePass(): String = "scene"
    def brightPass(): String = "bright"
    def bloomPassA(): String = "bloomA"
    def bloomPassB(): String = "bloomB"

    /// トーンマップ済みの本編の Pass の名前（画面へ写す前の最後の段）。
    /// WhyNot: 縮めた Pass や design の大きさにしない — FXAA は 1 テクセル = 1 画素を前提に縁を探す。
    def postPass(): String = "post"

    /// 後処理の Pass の並び: 場面（窓の画素の HDR）→ 明るい所 → ぼかし 2 回 → 本編の面を窓の画素の Rgba8Unorm へ。
    pub def chain(size: Vec2.Vec2, look: LookDoc.Rec, clear: Color, scene: List[Render.PlacedItem]): Render.PassPlan =
        let black = RenderTarget.LoadOp.Clear(Color.rgb(0.0, 0.0, 0.0));
        let fill = spec -> fullFill(size, spec);
        let aspect = size#x / size#y;
        let passes =
            { name = scenePass(), clear = RenderTarget.LoadOp.Clear(clear), items = scene }
                :: { name = brightPass(), clear = black, items = fill(brightSpec(look)) :: Nil }
                :: { name = bloomPassA(), clear = black, items = fill(blurOf(aspect, bloomPassA(), brightPass(), look#bloomRadius, look#bloomTaps)) :: Nil }
                :: { name = bloomPassB(), clear = black, items = fill(blurOf(aspect, bloomPassB(), bloomPassA(), look#bloomRadius * 0.5, look#bloomTaps)) :: Nil }
                :: { name = postPass(), clear = black, items = fill(mainSpec(aspect, look)) :: Nil }
                :: Nil;
        let at = s -> RenderTarget.withHdr({ scale = s | RenderTarget.defaultDescriptor() });
        { targets = Map#{ scenePass() => RenderTarget.withMsaa(look#msaaSamples, RenderTarget.withHdr(RenderTarget.windowDescriptor())),
                          brightPass() => at(0.5),
                          bloomPassA() => at(0.25),
                          bloomPassB() => at(0.25),
                          postPass() => RenderTarget.windowDescriptor() }
          | Render.passPlan(passes) }

    /// 画面へ置く面: post の Pass を FXAA でならして写す。
    pub def fxaaFill(size: Vec2.Vec2): Render.PlacedItem = fullFill(size, PostProcess.fxaaSpec(postPass()))

    def fullFill(size: Vec2.Vec2, spec: ShaderDoc.Spec): Render.PlacedItem =
        Render.shaderFill(spec, { position = Vec2.zero(), size = size }, 0.0, 0)

    def k(v: Float64): Field = Field.Const(v)
    def add(a: Field, b: Field): Field = Field.Bin({ op = BinOp.Add, a = a, b = b })
    def sub(a: Field, b: Field): Field = Field.Bin({ op = BinOp.Sub, a = a, b = b })
    def mul(a: Field, b: Field): Field = Field.Bin({ op = BinOp.Mul, a = a, b = b })
    def linear(f: Field): Field = Field.Sampler({ filter = ShaderDoc.FilterMode.Linear, of = f })

    /// 場面とブルームを足し、周辺を暗くしてトーンマップし、彩度で仕上げる。
    def mainSpec(aspect: Float64, look: LookDoc.Rec): ShaderDoc.Spec =
        let sharp = chan -> Field.Tex({ name = scenePass(), chan = chan });
        let bloom = chan -> linear(Field.Tex({ name = bloomPassB(), chan = chan }));
        let vig = sub(k(1.0), mul(k(look#vignette),
            Field.Smoothstep({ lo = 0.35, hi = 1.05, of = Field.RadialAspect({ cx = 0.5, cy = 0.52, aspect = aspect }) })));
        let base = chan -> mul(add(sharp(chan), mul(k(look#bloomStrength), bloom(chan))), Field.Ref("vig"));
        let hdr = { name = "dungeon_post", cycleRate = 0.0,
                    bindings = ("vig", vig) :: ("r0", base(TexChan.R)) :: ("g0", base(TexChan.G)) :: ("b0", base(TexChan.B)) :: Nil,
                    out = Out.Fill({ shade = Shade.Rgb({ r = Field.Ref("r0"), g = Field.Ref("g0"), b = Field.Ref("b0") }), alpha = k(1.0) }) };
        PostProcess.tonemappedWith(PostProcess.Tonemap.AcesFitted, look#exposure, look#whitePoint, finish(look), hdr)

    /// トーンマップの後の 0..1 の色の彩度を足し引きする。
    def finish(look: LookDoc.Rec): (Field, Field, Field) -> Shade =
        (r, g, b) -> {
            let lum = PostProcess.luma709(r, g, b);
            let graded = (c: Field) -> add(lum, mul(sub(c, lum), k(1.0 + look#saturation)));
            Shade.Rgb({ r = graded(r), g = graded(g), b = graded(b) })
        }

    /// 場面の明るい所だけを残す（しきいは HDR の目盛り）。
    def brightSpec(look: LookDoc.Rec): ShaderDoc.Spec =
        let read = chan -> linear(Field.Tex({ name = scenePass(), chan = chan }));
        let luma = PostProcess.luma709(read(TexChan.R), read(TexChan.G), read(TexChan.B));
        let over = Field.Smoothstep({ lo = look#bloomThreshold, hi = look#bloomThreshold + Float64.max(0.01, look#bloomKnee), of = luma });
        let chan = c -> mul(read(c), Field.Ref("over"));
        { name = "dungeon_bright", cycleRate = 0.0, bindings = ("over", over) :: Nil,
          out = Out.Fill({ shade = Shade.Rgb({ r = chan(TexChan.R), g = chan(TexChan.G), b = chan(TexChan.B) }), alpha = k(1.0) }) }

    /// 前の Pass を Linear で読んでぼかし、そのまま写す面（ブルームの 2 段で共用）。
    def blurOf(aspect: Float64, name: String, src: String, radius: Float64, taps: Int32): ShaderDoc.Spec =
        let b = chan -> linear(Field.TexBlur({ name = src, chan = chan, radius = { x = radius / aspect, y = radius },
                                               taps = ShaderDoc.blurTaps(taps), amount = k(1.0) }));
        { name = "dungeon_blur_${name}", cycleRate = 0.0, bindings = Nil,
          out = Out.Fill({ shade = Shade.Rgb({ r = b(TexChan.R), g = b(TexChan.G), b = b(TexChan.B) }), alpha = k(1.0) }) }
}
