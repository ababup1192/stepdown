// ZOrder — 画面全体の重なりの層（奥から手前）。Unity の Sorting Layer と同じ考え方。
// 新しい層（ポーズ・確認の窓）は case を足し、zRange に範囲を書く（書き忘れは match の網羅で止まる）。
// 層の中の前後は ZStack に積んだ順で決まり、葉の中の z は 0..31。根で SortingLayer.compose に渡す（View.frame）。
// 使い方の全文は engine の engine_world/src/SortingLayer.flix 冒頭と docs/z-bands.md。
mod ZOrder {

    use fge::SortingLayer

    pub enum Layer with Eq, ToString {
        case Scene   // 3D の場面（後処理を済ませた面 1 枚）
        case Marks   // 場面の上の 2D の印（予告のアイコンとラベル・眠りの z）
        case Hud     // 上の階と HP・下の持ち物と記録・題名と終わりの画面の文字・受けた時の画面の周りの赤
        case Cards   // 捨てるカード・レベルアップのカードと演出（場面と帯を沈めて上に出す）
        case Prompt  // 終了の確認の窓（場面のどれよりも手前）
    }

    instance SortingLayer[Layer] {
        pub def zRange(layer: Layer): (Int32, Int32) =
            match layer {
                case Layer.Scene  => (0, 119)
                case Layer.Marks  => (120, 179)
                case Layer.Hud    => (180, 239)
                case Layer.Cards  => (240, 299)
                case Layer.Prompt => (300, 309)
            }
    }

    /// 奥から手前へ全部の層（F8 の注釈の文脈・表のテストが使う）。
    pub def layers(): List[Layer] = Layer.Scene :: Layer.Marks :: Layer.Hud :: Layer.Cards :: Layer.Prompt :: Nil
}
