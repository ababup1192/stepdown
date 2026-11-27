// fge-switches.js — 頁の旗の読み手。URL の ?fge=<語>,<語>,… と、道具が頁より先に入れる window.fgeDev と、前の鍵（?fps=1 など）を 1 つの形にする。
// 書き出し（bin/fge export-web）が index.html の頭と sw.js に中身を埋める。字の決まりは docs/web-export.md の「頁の旗」。
// WhyNot: 頁の JS・頭の script・Service Worker のそれぞれで読まない — ?fge= の字の決まりが 3 つに分かれ、テーブルのテスト（testdata/web-switches.cases.json）を 1 本で流せない。
// WhyNot: 読み出しの関数の外で window を触らない — Service Worker（window が無い）にも同じ中身を埋める。
"use strict";

// 1 行 = 1 つの旗。field は window.fgeDev と読んだ結果の欄の名前・name は ?fge= の名前（小文字）・values は取れる値（無ければ真偽・"int" は正の整数）・
// short は値だけで書ける語・legacy は前の鍵（読んだら知らせる。sw だけは期限なしで知らせない。?fge= の後に足した旗は持たない）。
var FGE_SWITCHES = [
  { field: "fps", name: "fps", legacy: "fps" },
  { field: "backend", name: "backend", values: ["webgl2", "gl", "webgpu"], short: ["webgl2", "gl", "webgpu"], legacy: "backend" },
  { field: "net", name: "net", values: ["4g"], short: ["4g"], legacy: "net" },
  { field: "sw", name: "sw", values: ["0"], short: [], legacy: "sw", keep: true },
  { field: "input", name: "input", values: ["touch", "mouse"], short: ["touch", "mouse"], legacy: "input" },
  { field: "orientation", name: "orientation", values: ["portrait", "landscape"], short: ["portrait", "landscape"], legacy: "orientation" },
  { field: "size", name: "size", values: ["small", "medium", "large"], short: ["small", "medium", "large"], legacy: "size" },
  { field: "glcheck", name: "glcheck", legacy: "glcheck" },
  { field: "loadtime", name: "loadtime", legacy: "loadtime" },
  { field: "capture", name: "capture", values: "int", legacy: "capture" },
  { field: "deferred", name: "deferred", values: ["all"], short: [], legacy: "deferred" },
  { field: "usedAssets", name: "usedassets", legacy: "usedAssets" },
  { field: "webgpuFail", name: "webgpufail", values: ["prepare", "canvas"], short: [], legacy: "webgpuFail" },
  { field: "errors", name: "errors" },
  { field: "noworker", name: "noworker" }
];

// 何も頼まれていない時の値。backend の "" は既定（WebGPU を使えれば WebGPU）。
function fgeSwitchDefaults() {
  return { fps: false, backend: "", net: "", sw: true, input: "", orientation: "", size: "", glcheck: false, loadtime: false,
           capture: 0, deferred: "", usedAssets: false, webgpuFail: "", errors: false, noworker: false };
}

// 旗 s の字の値 raw を欄の値にする（読めなければ undefined）。前の鍵の真偽は "1" だけを真と読む。
function fgeSwitchValueOf(s, raw) {
  var v = String(raw).toLowerCase();
  if (s.field === "sw") return v === "0" ? false : undefined;
  if (s.field === "backend") return v === "webgl2" || v === "gl" ? "webgl2" : (v === "webgpu" ? "" : undefined);
  if (s.values === "int") { var n = /^[0-9]+$/.test(v) ? parseInt(v, 10) : 0; return n > 0 ? n : undefined; }
  if (!s.values) return v === "1" ? true : undefined;
  return s.values.indexOf(v) >= 0 ? v : undefined;
}

function fgeSwitchOf(field) {
  return FGE_SWITCHES.filter(function (s) { return s.field === field; })[0];
}

// window.fgeDev の欄の値 v を欄の値にする（読めなければ undefined）。真偽と数はそのまま・字は ?fge= の値と同じ読み方。
function fgeSwitchDevValueOf(s, v) {
  if (v === undefined || v === null) return undefined;
  if (typeof v === "boolean") return s.field === "sw" || !s.values ? v : undefined;
  if (typeof v === "number") return s.values === "int" && v > 0 ? Math.floor(v) : undefined;
  return fgeSwitchValueOf(s, v);
}

// ?fge= の 1 語を { s, value } にする（知らない語は null）。
function fgeSwitchWordOf(word) {
  var w = word.toLowerCase();
  if (w === "nosw") return { s: fgeSwitchOf("sw"), value: false };
  var colon = w.indexOf(":");
  var name = colon < 0 ? w : w.slice(0, colon);
  for (var i = 0; i < FGE_SWITCHES.length; i++) {
    var s = FGE_SWITCHES[i];
    if (colon < 0 && s.short && s.short.indexOf(w) >= 0) return { s: s, value: fgeSwitchValueOf(s, w) };
    if (s.name !== name) continue;
    if (colon < 0) return s.values ? null : { s: s, value: true };
    var value = s.values ? fgeSwitchValueOf(s, w.slice(colon + 1)) : undefined;
    return value === undefined ? null : { s: s, value: value };
  }
  return null;
}

// 頁の旗を読む。search は location.search の形（"?a=1&fge=fps"）・dev は window.fgeDev（無ければ null）。
// 返す物: switches（全部の欄）・legacy（読んだ前の鍵ごとに { from: "fps=1", to: "fps" }）・unknown（?fge= の知らない語）。
// 旗ごとに window.fgeDev → ?fge= → 前の鍵の順で、最初に値を持つ所を使う。同じ旗が ?fge= に 2 回あれば後の語。
// WhyNot: backend を順で上書きしない — WebGPU で落ちて開き直した頁は URL に webgl2 を持つ。fgeDev が別の値でも webgl2 を取らないと開き直しを繰り返す。
// WhyNot: 知らない語で止めない — 打ち間違いも、後の版の語を前の版の頁で開いた時も、旗が効かないだけで遊べる。
function fgeReadSwitches(search, dev) {
  var out = fgeSwitchDefaults();
  var params = new URLSearchParams(search || "");
  var fromWords = {}, legacy = [], unknown = [];
  (params.get("fge") || "").split(",").forEach(function (raw) {
    var word = raw.trim();
    if (word.length === 0) return;
    var got = fgeSwitchWordOf(word);
    if (got === null) unknown.push(word);
    else fromWords[got.s.field] = got.value;
  });
  var webgl2 = false;
  FGE_SWITCHES.forEach(function (s) {
    var old = s.legacy ? params.get(s.legacy) : null;
    var fromLegacy = old === null ? undefined : fgeSwitchValueOf(s, old);
    if (old !== null && !s.keep) legacy.push({ from: s.legacy + "=" + old, to: fgeSwitchWordFor(s, old) });
    var fromDev = dev ? fgeSwitchDevValueOf(s, dev[s.field]) : undefined;
    var picked = [fromDev, fromWords[s.field], fromLegacy].filter(function (v) { return v !== undefined; });
    if (s.field === "backend") webgl2 = picked.indexOf("webgl2") >= 0;
    if (picked.length > 0) out[s.field] = picked[0];
  });
  out.backend = webgl2 ? "webgl2" : "";
  return { switches: out, legacy: legacy, unknown: unknown };
}

// 前の鍵の旗 s の値 raw に当たる ?fge= の語（前の鍵の知らせに出す）。
function fgeSwitchWordFor(s, raw) {
  var v = String(raw).toLowerCase();
  if (!s.values) return s.name;
  if (s.short && s.short.indexOf(v) >= 0) return v;
  return fgeSwitchValueOf(s, v) === undefined ? s.name + ":…" : s.name + ":" + v;
}

// search の ?fge= に word を足した search（前の鍵 drop は外す）。WebGPU で落ちた頁を webgl2 で開き直す時に使う。
function fgeSearchWithWord(search, word, drop) {
  var params = new URLSearchParams(search || "");
  var words = (params.get("fge") || "").split(",").map(function (w) { return w.trim(); }).filter(function (w) { return w.length > 0; });
  if (words.indexOf(word) < 0) words.push(word);
  params.set("fge", words.join(","));
  if (drop) params.delete(drop);
  return "?" + params.toString().replace(/%2C/gi, ",").replace(/%3A/gi, ":");
}

// この頁の旗（頁で 1 回だけ読む。頭の script と fge-web.js が同じ答えを使う）。
function fgePageSwitches() {
  if (!window.fgeSwitchesRead) window.fgeSwitchesRead = fgeReadSwitches(location.search, window.fgeDev || null);
  return window.fgeSwitchesRead;
}

// fge-sw.js — 書き出しの Service Worker。bin/fge export-web が fge-switches.js（頁の旗の読み手）の後ろにつないで書き出しの根へ sw.js として置き、束の id の所を替える。
// 何をどう返すかは docs/web-export.md の「起動の始まりと 2 回目からの起動」。
"use strict";

var FGE_GAME_ID = "stepdown";
var FGE_SCOPE = self.registration.scope;
var FGE_SCOPE_PATH = new URL(FGE_SCOPE).pathname;
var FGE_PREFIX = "fge-sw:" + FGE_GAME_ID + ":" + FGE_SCOPE_PATH + ":";
var FGE_META = FGE_PREFIX + "meta";
var FGE_VERSION_URL = FGE_SCOPE + "version.txt";
var FGE_INDEX_URL = FGE_SCOPE + "index.html";
var FGE_VERSION_RE = /^[0-9a-f]{6,64}$/;
// WhyNot: 頁の version.txt を毎回もう一度ネットへ出さない — 頁を開く時に index.html の所で確かめたばかりで、同じ答えを往復 1 回分待つだけになる。
var FGE_CHECK_REUSE_MS = 10000;
var fgeLatest = null;
var fgeCheck = null;
var fgeRemoving = false;
var fgeSums = new Map();
// WhyNot: 頁が送ってきた URL を、この Service Worker が取っている最中にもう一度取らない — 同じ物を 2 回運ぶ。
var fgeInFlight = new Set();

function fgeCacheName(version) { return FGE_PREFIX + "v" + version; }

self.addEventListener("install", function (event) {
  event.waitUntil(fgeCheckVersion(false).catch(function () { return null; }).then(function () { return self.skipWaiting(); }));
});

// WhyNot: 次に開くまで待たずに今の頁を受け持つ — 初めての頁が後から取る後回しの素材もキャッシュへ入り、2 回目の起動に間に合う。
self.addEventListener("activate", function (event) {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("fetch", function (event) {
  var req = event.request;
  if (fgeRemoving || req.method !== "GET" || req.url.indexOf(FGE_SCOPE) !== 0) return;
  var url = new URL(req.url);
  var path = url.origin + url.pathname;
  if (req.mode === "navigate") {
    if (path !== FGE_SCOPE && path !== FGE_INDEX_URL) return;
    // WhyNot: ?sw=0（?fge=nosw）の頁の物をキャッシュから返さない — 壊れたキャッシュで起動の途中に落ちると、頁の側の外す処理まで届かない。
    // unregister は頁が全部閉じるまで効かず、その間もこの頁は受け持たれたままなので、fgeRemoving で fetch を全部素のネットへ回す。
    if (fgeNoServiceWorker(url.search)) {
      fgeRemoving = true;
      event.waitUntil(fgeRemoveSelf());
      return;
    }
    var later = fgeLater(event);
    event.respondWith(later.until(fgeOrNetwork(fgeServeIndex(later), req)));
    return;
  }
  var version = url.searchParams.get("v");
  // WhyNot: Range の付いた物を受けない — 版のキャッシュは丸ごとの 200 だけを持ち、206 を作り直すより素のネットへ回す方が確か。
  if (path !== FGE_VERSION_URL && (!version || req.headers.has("range"))) return;
  var work = fgeLater(event);
  event.respondWith(work.until(fgeOwnClient(event).then(function (own) {
    if (!own) return fetch(req);
    return fgeOrNetwork(path === FGE_VERSION_URL ? fgeServeVersion() : fgeServeVersioned(req, version, work), req);
  })));
});

self.addEventListener("message", function (event) {
  var d = event.data;
  if (fgeRemoving) return;
  if (d && d.type === "fge-sw-cache" && Array.isArray(d.urls)) {
    event.waitUntil(fgeCacheUrls(d.urls).catch(function (e) { console.warn("[fge-sw] 先に入れられない: " + (e && e.message || e)); }));
  } else if (d && d.type === "fge-sw-drop" && typeof d.keep === "string") {
    event.waitUntil(fgeDropOtherVersions(d.keep, event.source && event.source.id).catch(function () { /* 次の頁が消す */ }));
  }
});

// WhyNot: 応答を返した後の then の中で event.waitUntil を呼ばない — respondWith を寿命の延長に数えない版では、その時にはもう呼べず例外になり、同じ物を 2 回取る。
function fgeLater(event) {
  var jobs = [];
  var done;
  event.waitUntil(new Promise(function (r) { done = r; }));
  return {
    add: function (p) { jobs.push(p.catch(function () { /* 入らなくても次にネットから取る */ })); },
    until: function (main) {
      main.then(function () { return null; }, function () { return null; }).then(function () { return Promise.all(jobs); }).then(done, done);
      return main;
    }
  };
}

// WhyNot: 自分の scope の頁でない物を受けない — ドメインの根に置いたゲームの Service Worker は、まだ自分のを持たない
// サブフォルダのゲームの頁も受け持つ。その頁の ?v= を根のゲームの版で入れると、版の違う物を混ぜる。
// WhyNot: ?sw=0（?fge=nosw）の頁の物を fgeRemoving だけで見分けない — Service Worker は止まって起こし直されると旗を忘れる。
function fgeOwnClient(event) {
  if (!event.clientId) return Promise.resolve(true);
  return self.clients.get(event.clientId).then(function (c) {
    if (!c) return true;
    var u = new URL(c.url);
    if (fgeNoServiceWorker(u.search)) return false;
    // WhyNot: glb を解く Worker（頁が起こす fge-glb-worker.js）の取る物を素のネットへ回さない — iOS の Safari で 1.3 MB の .br を回線で取り直す。
    return u.pathname === FGE_SCOPE_PATH || u.pathname === FGE_SCOPE_PATH + "index.html" || u.pathname === FGE_SCOPE_PATH + "fge-glb-worker.js";
  }, function () { return true; });
}

// 頁の URL が Service Worker を外す口（?sw=0・?fge=nosw）か。
function fgeNoServiceWorker(search) {
  return fgeReadSwitches(search, null).switches.sw === false;
}

function fgeOrNetwork(promise, req) {
  return promise.catch(function (e) {
    console.warn("[fge-sw] " + req.url + " を素のネットへ回す: " + (e && e.message || e));
    return fetch(req);
  });
}

function fgeRemoveSelf() {
  return self.registration.unregister().then(function () { return fgeDeleteCaches(function () { return true; }); });
}

// WhyNot: 束の id で絞らない — id を替えて書き出し直した時の古い id のキャッシュが残る。書き出しのフォルダの道で絞る。
function fgeDeleteCaches(drop) {
  return caches.keys().then(function (names) {
    return Promise.all(names.filter(function (name) {
      return name.indexOf("fge-sw:") === 0 && name.indexOf(":" + FGE_SCOPE_PATH + ":") > 0 && drop(name);
    }).map(function (name) { return caches.delete(name); }));
  });
}

// ── 版 ──
function fgeStoredVersion() {
  if (fgeLatest) return Promise.resolve(fgeLatest);
  return caches.match(FGE_VERSION_URL, { cacheName: FGE_META }).then(function (res) {
    return res ? res.text() : "";
  }).then(function (t) {
    t = t.trim();
    if (FGE_VERSION_RE.test(t) && !fgeLatest) fgeLatest = t;
    return fgeLatest;
  });
}

// WhyNot: 頁を開くたびの確かめに前の答えを使わない — 続けて開いた間に版が上がると、古い版のまま読み直さない。
// 前の答えを使うのは、その頁を開いた所で始めた確かめ（fromNav）を頁の version.txt に返す時だけ。
function fgeReuseNavCheck() {
  if (fgeCheck && fgeCheck.nav && Date.now() - fgeCheck.at < FGE_CHECK_REUSE_MS) return fgeCheck.promise;
  return fgeCheckVersion(false);
}

function fgeCheckVersion(fromNav) {
  var check = { at: Date.now(), nav: fromNav, promise: null };
  check.promise = fetch(FGE_VERSION_URL, { cache: "no-store" }).then(function (res) {
    if (!res.ok) throw new Error("version.txt が " + res.status);
    return res.text();
  }).then(function (t) {
    t = t.trim();
    if (!FGE_VERSION_RE.test(t)) throw new Error("version.txt が版の形でない");
    return fgeSetLatest(t).then(function () { return t; });
  });
  check.promise.catch(function () { if (fgeCheck === check) fgeCheck = null; });
  fgeCheck = check;
  return check.promise;
}

function fgeSetLatest(version) {
  fgeLatest = version;
  return caches.open(FGE_META).then(function (cache) {
    return cache.put(FGE_VERSION_URL, new Response(version + "\n", { headers: { "Content-Type": "text/plain" } }));
  });
}

// WhyNot: 新しい版が見えた所で古い版を消さない — 押して遊び始めた頁は古い版のまま続け（読み直さない）、後から取る物を古い版のキャッシュから取る。
// 消すと素のネットへ行き、置き場の新しい中身が古い版の URL で届く。
// WhyNot: 今の版でない頁の知らせでは消さない — 古い版の頁が新しい版のキャッシュを消す。
// WhyNot: 知らせてきた頁の他に、この scope の頁が開いている間は消さない — 別のタブで古い版のまま遊んでいるかもしれない。次に開いた時に消す。
function fgeDropOtherVersions(keep, senderId) {
  return Promise.all([fgeStoredVersion(), self.clients.matchAll({ type: "window" })]).then(function (got) {
    if (got[0] !== keep) return;
    var others = got[1].filter(function (c) { return c.id !== senderId && c.url.indexOf(FGE_SCOPE) === 0; });
    if (others.length > 0) return;
    return fgeDeleteCaches(function (name) { return name !== FGE_META && name !== fgeCacheName(keep); });
  });
}

function fgeServeVersion() {
  return fgeReuseNavCheck().catch(function (e) {
    return fgeStoredVersion().then(function (stored) {
      if (!stored) throw e;
      return stored;
    });
  }).then(function (version) {
    return new Response(version + "\n", { headers: { "Content-Type": "text/plain", "Cache-Control": "no-store" } });
  });
}

// ── 中身の照らし（sums.json）──
// WhyNot: 届いた後の version.txt が同じ版かだけを見て入れない — 置き場の入れ替えは拠点とファイルごとに混ざり、
// 新しい版の URL に古い中身が届く（その逆も）。入れたら次の版まで残るので、1 本ずつの中身を sums.json のハッシュと照らす。
// sums.json そのものは、その sha256 の頭 12 字が版と同じことで照らす。
function fgeSumsOf(version) {
  if (!fgeSums.has(version)) {
    var p = fgeLoadSums(version).catch(function () { return null; });
    fgeSums.set(version, p);
    p.then(function (s) { if (!s) fgeSums.delete(version); });
  }
  return fgeSums.get(version);
}

function fgeLoadSums(version) {
  var url = FGE_SCOPE + "sums.json?v=" + version;
  var name = fgeCacheName(version);
  return caches.match(url, { cacheName: name }).then(function (hit) {
    if (hit) return hit.arrayBuffer().then(function (buf) { return { buf: buf, fresh: false }; });
    return fetch(url, { cache: "no-cache", credentials: "same-origin" }).then(function (res) {
      if (res.status !== 200) throw new Error("sums.json が " + res.status);
      return res.arrayBuffer().then(function (buf) { return { buf: buf, fresh: true }; });
    });
  }).then(function (got) {
    return fgeDigest(got.buf).then(function (hex) {
      if (hex.slice(0, 12) !== version) throw new Error("sums.json が版 " + version + " と合わない");
      var sums = JSON.parse(new TextDecoder().decode(got.buf));
      if (!got.fresh) return sums;
      return caches.open(name).then(function (cache) {
        return cache.put(url, new Response(got.buf, { headers: { "Content-Type": "application/json" } }));
      }).then(function () { return sums; });
    });
  });
}

function fgeDigest(buf) {
  return crypto.subtle.digest("SHA-256", buf).then(function (h) {
    return Array.prototype.map.call(new Uint8Array(h), function (b) { return (b < 16 ? "0" : "") + b.toString(16); }).join("");
  });
}

// WhyNot: 届いた応答の Content-Encoding を持ち越さない — 中身はもう解いてあり、作り直した応答に付けたままだと長さと合わない。
function fgePutVerified(url, version, res) {
  var rel = decodeURIComponent(new URL(url).pathname.slice(FGE_SCOPE_PATH.length));
  return Promise.all([fgeSumsOf(version), res.arrayBuffer()]).then(function (got) {
    var sums = got[0];
    if (!sums || typeof sums[rel] !== "string") return;
    return fgeDigest(got[1]).then(function (hex) {
      if (hex !== sums[rel]) return;
      var headers = new Headers(res.headers);
      headers.delete("Content-Encoding");
      headers.delete("Content-Length");
      return caches.open(fgeCacheName(version)).then(function (cache) {
        return cache.put(url, new Response(got[1], { status: 200, headers: headers }));
      });
    });
  });
}

// ── 頁 ──
// WhyNot: index.html を返す前に版の答えを待たない — 往復 1 回分が起動に乗る。返した後に頁が version.txt で確かめ、違えば読み直す。
function fgeServeIndex(later) {
  later.add(fgeCheckVersion(true));
  return fgeStoredVersion().then(function (version) {
    if (!version) return null;
    return caches.match(FGE_INDEX_URL, { cacheName: fgeCacheName(version) });
  }).then(function (hit) {
    return hit || fgeFetchIndex(FGE_INDEX_URL, { cache: "no-cache", credentials: "same-origin" }, later);
  });
}

// WhyNot: 返す頁をブラウザの HTTP キャッシュから取らない（no-cache）— 置き場が max-age を付けて配り、版の上がった後も古い頁が出る。
// WhyNot: 頁を今の版の所へ決め打ちで入れない — 置き場の入れ替えの途中や HTTP キャッシュからだと別の版の頁が届く。
// 頁の中の版（起動役の baked）の所へ入れるので、頁と素材の版は違わない。返すのは覚えている版の所からだけ。
function fgeFetchIndex(url, init, later) {
  return fetch(url, init).then(function (res) {
    if (res.redirected) throw new Error("index.html が転送された");
    if (res.status !== 200 || res.type !== "basic") return res;
    var keep = res.clone();
    later.add(res.clone().text().then(function (html) {
      var m = /var baked = "([0-9a-f]{6,64})";/.exec(html);
      if (!m) return;
      return caches.open(fgeCacheName(m[1])).then(function (cache) { return cache.put(FGE_INDEX_URL, keep); });
    }));
    return res;
  });
}

// ── 版の付いた物 ──
function fgeServeVersioned(req, version, later) {
  return caches.match(req.url, { cacheName: fgeCacheName(version), ignoreVary: true }).then(function (hit) {
    if (hit) return hit;
    fgeInFlight.add(req.url);
    return fetch(req).then(function (res) {
      if (res.status === 200 && res.type === "basic") {
        later.add(fgePutVerified(req.url, version, res.clone()).then(function () { fgeInFlight.delete(req.url); },
          function () { fgeInFlight.delete(req.url); }));
      } else {
        fgeInFlight.delete(req.url);
      }
      return res;
    }, function (e) {
      fgeInFlight.delete(req.url);
      throw e;
    });
  });
}

// WhyNot: 頁が送ってきた URL をネットから取り直さない（only-if-cached）— iOS の Safari は大きい物を HTTP キャッシュに残さないことがあり、
// 押した後に wasm と素材を回線で取り直して後回しの取り込みと回線を取り合う。HTTP キャッシュに無い物は、次に開いた時にこの Service Worker を通って入る。
function fgeCacheUrls(urls) {
  return fgeStoredVersion().then(function (stored) {
    return stored || fgeCheckVersion(false).catch(function () { return null; });
  }).then(function (version) {
    if (!version) return;
    var todo = urls.filter(function (u) { return typeof u === "string" && u.indexOf(FGE_SCOPE) === 0; });
    var next = function () {
      var u = todo.shift();
      if (u === undefined) return Promise.resolve();
      return fgeCacheOne(version, u).catch(function () { /* 1 本落ちても残りは入れる */ }).then(next);
    };
    return Promise.all([next(), next(), next(), next()]);
  });
}

function fgeCacheOne(version, u) {
  var url = new URL(u);
  var path = url.origin + url.pathname;
  var name = fgeCacheName(version);
  var init = { cache: "only-if-cached", mode: "same-origin", credentials: "same-origin" };
  if (path === FGE_SCOPE || path === FGE_INDEX_URL) {
    return caches.match(FGE_INDEX_URL, { cacheName: name }).then(function (hit) {
      if (hit) return null;
      var jobs = [];
      return fgeFetchIndex(u, init, { add: function (p) { jobs.push(p); } }).then(function () { return Promise.all(jobs); });
    });
  }
  var v = url.searchParams.get("v");
  if (!v || fgeInFlight.has(u)) return Promise.resolve();
  return caches.match(u, { cacheName: fgeCacheName(v), ignoreVary: true }).then(function (hit) {
    if (hit || fgeInFlight.has(u)) return null;
    return fetch(u, init).then(function (res) {
      if (res.status === 200 && res.type === "basic") return fgePutVerified(u, v, res);
    });
  });
}
