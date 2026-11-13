// Starts — 始まりの登録簿（App.withStarts。Game が Resources.liftStart で組の行へ持ち上げる）。名前と引数から、新しく作った初期の World を場面へ組む。
// 起動（START=play）・リモートの POST /start・flow の `start play`・テストの App.startOrBug が同じ表を引く。
mod Starts {
    use fge::Resources

    /// 登録簿。先頭が起動の既定。
    pub def all(): List[Resources.Start[Res.Res, World.World]] =
        { name = "title", doc = "題名の画面（起動の既定）", args = Map.empty(), build = (_, w, _) -> Ok(w) } ::
        { name = "play", doc = "1F の始まり（周回の種は起動の World のまま）", args = Map.empty(), build = (res, w, _) -> Ok(World.begin(res, w)) } ::
        { name = "floor5", doc = "5F に入った所（Lv7・鉄の剣と鉄の盾・鉄の矢 8・小爆弾 4・回復薬 1・爆弾 1 体のカード。地図は周回の種で決まる）", args = Map.empty(), build = (res, w, _) -> Ok(fifthFloor(res, w)) } :: Nil

    /// 4F まで普通に潜ってきた人の持ち物とレベルで、5F へ降りた所。
    /// WhyNot: 1F から 4F を flow で歩かせる — 手順が長く、Doc を少し直すだけで 5F に着く前に崩れる。
    def fifthFloor(res: Res.Res, w: World.World): World.World =
        let playing = World.begin(res, w);
        World.withState(res, Floor.enter(Res.floorDocs(res), res#layouts, Run.of(seasoned(Run.rec(playing#runState))), 5), playing)

    /// 4F まで潜ってきた人の HP・剣と盾・持ち物・カード・レベル（5F の始まりと、ボットを 5F から遊ばせる時が同じ物を使う）。
    pub def seasoned(r: Run.Rec): Run.Rec =
        { hero = { hp = 30, maxHp = 30, sword = Some(Item.Kind.IronSword), shield = Some(Item.Kind.IronShield) | r#hero },
          pack = Run.Held.Held(Item.Kind.IronArrow, 8) :: Run.Held.Held(Item.Kind.SmallBomb, 4) :: Run.Held.Held(Item.Kind.Tonic, 1) :: Nil,
          perks = Run.Perk.MaxHp :: Run.Perk.Attack :: Run.Perk.LoneBomb :: Run.Perk.MaxHp :: Run.Perk.Attack :: Run.Perk.Feast :: Nil,
          level = 7, exp = 4, fullness = 70, coins = 9 | r }
}
