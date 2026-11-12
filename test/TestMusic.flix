// 曲: 題名と遊ぶ画面とカードの間はハープの曲が鳴り続け、倒れた・抜けた画面では止まる。
mod TestMusic {

    use World.Screen

    /// 題名・遊ぶ画面・カードは bgm_harp、倒れた・抜けた画面は無し。
    @Test
    def testHarpPlaysExceptOnTheEndScreens(): Unit \ Assert =
        Assert.assertEq(expected = (Some("bgm_harp"), Some("bgm_harp"), Some("bgm_harp"), None, None),
            (World.musicOf(Screen.Title), World.musicOf(Screen.Play), World.musicOf(Screen.Cards), World.musicOf(Screen.Dead), World.musicOf(Screen.Clear)))
}
