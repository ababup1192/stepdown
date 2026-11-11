# ダンジョンの部品を Blender で組み、assets/models/kit.glb に書き出す。ゲームはノードの名前で引いて格子に並べる。
#   B=/nix/store/2rqcqsp3ww61r0qw46izbppssz3bnqxg-blender-5.2.3/bin/blender
#   $B -b --factory-startup --python modeling/kit.py                … assets/models/kit.glb と assets/models/tex/*.png
#   $B -b --factory-startup --python modeling/kit.py -- --preview   … debug/models/kit.png（部品を並べた確かめ絵）
# 寸法は 1 マス = 1 m。どの部品もマスの真ん中・床の上面（高さ 0）が原点。Blender の Z が上。
#
# 書き出す物の名前:
#   FLOOR_0..2（敷石の形違い）・FLOOR_CRACK（4 つに割れた敷石）・WALL（側面）・WALL_TOP（壁の上面）・VOID（奥の黒い蓋）
#   PILLAR・ROCK・PIT（水場の底）・WATER（水面の板。材質はゲーム側で差し替える）・STAIRS・TORCH・FLAME・MOSS_0..2
import bpy, bmesh, os, sys, math, random
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..")
OUT = os.path.join(ROOT, "assets", "models")
TEX = os.path.join(OUT, "tex")
DEBUG = os.path.join(ROOT, "debug", "models")
for d in (OUT, TEX, DEBUG): os.makedirs(d, exist_ok=True)
ARGS = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
PREVIEW = "--preview" in ARGS
random.seed(3); np.random.seed(3)

bpy.ops.wm.read_factory_settings(use_empty=True)
# 低めの壁: 見下ろしで、手前の壁の上面が主人公を隠さない高さ
WALL_H = 0.6

# ── 模様の画像（値のノイズを数段重ねる。毎コマ作らないよう画像にして貼る）──
def noise(n, octaves, seed):
    rng = np.random.default_rng(seed); acc = np.zeros((n, n)); amp = 1.0; tot = 0
    for o in range(octaves):
        k = 4 * 2 ** o
        g = rng.random((k + 1, k + 1)); g[-1, :] = g[0, :]; g[:, -1] = g[:, 0]
        xs = np.linspace(0, k, n, endpoint=False); i = xs.astype(int); f = xs - i; f = f * f * (3 - 2 * f)
        a = g[np.ix_(i, i)] * (1 - f)[:, None] * (1 - f)[None, :] + g[np.ix_(i + 1, i)] * f[:, None] * (1 - f)[None, :] \
            + g[np.ix_(i, i + 1)] * (1 - f)[:, None] * f[None, :] + g[np.ix_(i + 1, i + 1)] * f[:, None] * f[None, :]
        acc += a * amp; tot += amp; amp *= 0.5
    return acc / tot

def texture(name, lo, hi, octaves=5, seed=0, edge=0.0, n=256):
    v = noise(n, octaves, seed)
    v = np.clip((v - 0.3) / 0.45, 0, 1)
    if edge > 0:
        # 縁を少し暗くして、敷石 1 枚ずつの形を読ませる
        t = np.linspace(0, 1, n); d = np.minimum(t, 1 - t); e = np.minimum(d[:, None], d[None, :])
        v = v * (1 - edge * np.clip(1 - e / 0.12, 0, 1))
    lo = np.array(lo); hi = np.array(hi)
    rgb = lo[None, None, :] * (1 - v[..., None]) + hi[None, None, :] * v[..., None]
    rgba = np.concatenate([rgb, np.ones((n, n, 1))], axis=2).astype(np.float32)
    im = bpy.data.images.new(name, n, n, alpha=False)
    im.pixels[:] = rgba[::-1].ravel()
    im.filepath_raw = os.path.join(TEX, name + ".png"); im.file_format = 'PNG'; im.save()
    return im

def mat(name, img=None, rgb=(0.5, 0.5, 0.5), rough=0.85, emit=None):
    m = bpy.data.materials.new(name); m.use_nodes = True
    p = m.node_tree.nodes["Principled BSDF"]
    p.inputs["Roughness"].default_value = rough
    if img is not None:
        t = m.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
        m.node_tree.links.new(t.outputs["Color"], p.inputs["Base Color"])
    else:
        p.inputs["Base Color"].default_value = (*rgb, 1)
    if emit:
        p.inputs["Emission Color"].default_value = (*emit[0], 1); p.inputs["Emission Strength"].default_value = emit[1]
    return m

M_FLOOR = mat("floor", texture("floor", (0.20, 0.16, 0.12), (0.42, 0.34, 0.25), seed=1, edge=0.35))
M_FLOOR2 = mat("floor_b", texture("floor_b", (0.17, 0.14, 0.11), (0.37, 0.31, 0.24), seed=7, edge=0.35))
M_WALL = mat("wall", texture("wall", (0.20, 0.17, 0.15), (0.38, 0.33, 0.28), octaves=4, seed=2, edge=0.25))
# 壁の上面は奥の黒い蓋より明るい石の色にする。蓋と同じ黒だと、部屋の間の 2 行の壁が床を覆う黒い板に見える
M_TOP = mat("wall_top", texture("wall_top", (0.14, 0.12, 0.12), (0.30, 0.26, 0.25), octaves=3, seed=3, edge=0.5))
M_PIT = mat("pit", texture("pit", (0.10, 0.20, 0.18), (0.28, 0.36, 0.30), seed=4))
M_MOSS = mat("moss", texture("moss", (0.10, 0.22, 0.07), (0.28, 0.42, 0.14), octaves=3, seed=5, n=64), rough=0.95)
M_WOOD = mat("wood", rgb=(0.30, 0.17, 0.08), rough=0.7)
# 地の色は黒にして光だけで色を出す。地の色があると、すぐ前の松明の光を照り返して白く飛ぶ
M_FLAME = mat("flame", rgb=(0.0, 0.0, 0.0), rough=1.0, emit=((1.0, 0.22, 0.02), 2.2))
M_WATER = mat("water", rgb=(0.04, 0.2, 0.25), rough=0.05)
M_VOID = mat("void", rgb=(0.01, 0.01, 0.015), rough=1.0)

made = []

def box(name, cx, cy, z0, z1, sx, sy, m, bevel=0.0, parent=None):
    bpy.ops.mesh.primitive_cube_add(location=(cx, cy, (z0 + z1) / 2))
    o = bpy.context.object; o.name = name
    o.scale = (sx / 2, sy / 2, (z1 - z0) / 2); bpy.ops.object.transform_apply(scale=True)
    if bevel > 0:
        b = o.modifiers.new("bv", 'BEVEL'); b.width = bevel; b.segments = 2
        bpy.ops.object.modifier_apply(modifier="bv")
    o.data.materials.append(m)
    return o

def join(name, objs):
    bpy.ops.object.select_all(action='DESELECT')
    for o in objs: o.select_set(True)
    bpy.context.view_layer.objects.active = objs[0]
    bpy.ops.object.join(); o = bpy.context.object; o.name = name
    made.append(o); return o

def jitter(o, amt):
    for v in o.data.vertices:
        v.co.x += random.uniform(-amt, amt); v.co.y += random.uniform(-amt, amt); v.co.z += random.uniform(-amt, amt) * 0.4

# 敷石: 形違いを 3 つ。高さを少しずつ変えて、平らな床に見せない
for i, (dz, s, m) in enumerate(((0.0, 0.95, M_FLOOR), (-0.012, 0.94, M_FLOOR2), (0.01, 0.95, M_FLOOR))):
    o = box("t", random.uniform(-.01, .01), random.uniform(-.01, .01), -0.5, dz, s, s, m, 0.045)
    jitter(o, 0.006); made.append(o); o.name = f"FLOOR_{i}"
parts = []
for qx in (-0.24, 0.24):
    for qy in (-0.24, 0.24):
        q = box("q", qx + random.uniform(-.02, .02), qy + random.uniform(-.02, .02), -0.5, random.uniform(-0.03, 0.01), 0.45, 0.45, M_FLOOR2, 0.03)
        parts.append(q)
join("FLOOR_CRACK", parts)

o = box("WALL", 0, 0, -0.5, WALL_H - 0.06, 1.0, 1.0, M_WALL, 0.03); made.append(o)
o = box("WALL_TOP", 0, 0, WALL_H - 0.08, WALL_H, 1.04, 1.04, M_TOP, 0.03); made.append(o)
o = box("VOID", 0, 0, WALL_H - 0.02, WALL_H, 1.0, 1.0, M_VOID); made.append(o)

join("PILLAR", [box("p", 0, 0, 0, 1.3, 0.62, 0.62, M_WALL, 0.06), box("pb", 0, 0, 0, 0.16, 0.8, 0.8, M_WALL, 0.04),
                box("pc", 0, 0, 1.12, 1.24, 0.78, 0.78, M_WALL, 0.04)])

bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=2, radius=0.38, location=(0, 0, 0.2))
o = bpy.context.object; o.scale = (1.1, 0.9, 0.7); bpy.ops.object.transform_apply(scale=True)
for v in o.data.vertices: v.co *= random.uniform(0.85, 1.12)
o.data.materials.append(M_WALL); o.name = "ROCK"; made.append(o)

o = box("PIT", 0, 0, -0.6, -0.5, 1.0, 1.0, M_PIT); made.append(o)
bpy.ops.mesh.primitive_plane_add(size=1.0, location=(0, 0, -0.16)); o = bpy.context.object; o.name = "WATER"
o.data.materials.append(M_WATER); made.append(o)

st = []
for dx, dy, sx, sy in ((0, 0.44, 1, 0.12), (0, -0.44, 1, 0.12), (0.44, 0, 0.12, 1), (-0.44, 0, 0.12, 1)):
    st.append(box("rim", dx, dy, -0.5, 0.02, sx, sy, M_WALL, 0.02))
st.append(box("hole", 0, 0, -1.4, -1.3, 0.9, 0.9, M_VOID))
for i in range(5):
    st.append(box("step", 0, 0.28 - i * 0.16, -0.95, -0.12 - i * 0.17, 0.76, 0.16, M_FLOOR, 0.015))
join("STAIRS", st)

# 松明は壁の手前側の面（-Y）に付く形。ゲームが向きを回して壁に合わせる
# 炎の上端を壁の上面（0.6）より下に収める。上に出ると、見下ろしでは壁の上に乗って見える
join("TORCH", [box("k", 0, -0.51, 0.16, 0.22, 0.10, 0.04, M_WOOD), box("s", 0, -0.56, 0.16, 0.34, 0.06, 0.06, M_WOOD),
               box("b", 0, -0.56, 0.32, 0.37, 0.11, 0.11, M_WOOD)])
bpy.ops.mesh.primitive_uv_sphere_add(segments=8, ring_count=5, radius=0.075, location=(0, -0.56, 0.45))
o = bpy.context.object; o.scale.z = 1.5; bpy.ops.object.transform_apply(scale=True)
o.data.materials.append(M_FLAME); o.name = "FLAME"; made.append(o)

for k in range(3):
    blobs = []
    for _ in range(3 + k):
        bpy.ops.mesh.primitive_uv_sphere_add(segments=8, ring_count=5, radius=random.uniform(.06, .12),
                                             location=(random.uniform(-.35, .35), random.uniform(-.35, .35), 0.0))
        b = bpy.context.object; b.scale.z = 0.45; bpy.ops.object.transform_apply(scale=True); b.data.materials.append(M_MOSS)
        blobs.append(b)
    join(f"MOSS_{k}", blobs)

# 原点はマスの真ん中・床の上面（どの部品も (0,0,0) を基準に作った）
for o in made:
    bpy.context.view_layer.objects.active = o
    o.select_set(True)
    if o.data.uv_layers.active is None:
        o.data.uv_layers.new()

if PREVIEW:
    for i, o in enumerate(made):
        o.location.x = (i % 6) * 1.6; o.location.y = (i // 6) * 1.8
    cam = bpy.data.cameras.new("c"); co = bpy.data.objects.new("c", cam); bpy.context.scene.collection.objects.link(co)
    co.location = (4.0, -7.5, 8.0); co.rotation_euler = (math.radians(50), 0, 0); bpy.context.scene.camera = co
    sun = bpy.data.lights.new("s", 'SUN'); sun.energy = 3; so = bpy.data.objects.new("s", sun)
    so.rotation_euler = (math.radians(40), math.radians(20), 0); bpy.context.scene.collection.objects.link(so)
    w = bpy.data.worlds.new("w"); bpy.context.scene.world = w; w.use_nodes = True
    w.node_tree.nodes["Background"].inputs["Color"].default_value = (0.2, 0.2, 0.25, 1)
    sc = bpy.context.scene; sc.render.engine = 'BLENDER_EEVEE'; sc.render.resolution_x = 1200; sc.render.resolution_y = 900
    sc.render.filepath = os.path.join(DEBUG, "kit.png"); bpy.ops.render.render(write_still=True)
    sys.exit(0)

bpy.ops.object.select_all(action='DESELECT')
for o in made: o.select_set(True)
path = os.path.join(OUT, "kit.glb")
bpy.ops.export_scene.gltf(filepath=path, export_format="GLB", use_selection=True, export_animations=False,
                          export_materials="EXPORT", export_texcoords=True, export_yup=True, export_apply=True)
print("exported:", path, [o.name for o in made])
