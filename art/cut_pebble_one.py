import bpy, numpy as np, sys
src, dst = sys.argv[sys.argv.index("--")+1:]
im = bpy.data.images.load(src); w,h = im.size
a = np.array(im.pixels[:], dtype=np.float32).reshape(h,w,4)
lum = 0.3*a[...,0]+0.59*a[...,1]+0.11*a[...,2]
mask = (a[...,3] > 0.5) & (lum > 0.28)
lab = np.zeros((h,w), np.int32); n = 0; sizes = {}
for y in range(h):
    for x in range(w):
        if mask[y,x] and lab[y,x]==0:
            n += 1; st=[(y,x)]; lab[y,x]=n; c=0
            while st:
                cy,cx = st.pop(); c+=1
                for dy,dx in ((1,0),(-1,0),(0,1),(0,-1)):
                    ny,nx=cy+dy,cx+dx
                    if 0<=ny<h and 0<=nx<w and mask[ny,nx] and lab[ny,nx]==0:
                        lab[ny,nx]=n; st.append((ny,nx))
            sizes[n]=c
big = 1  # 一番手前（輪郭が切れていない）右下の石; print("parts", sizes, "pick", big)
keep = lab==big
# 縁の半透明も拾う: 選んだ塊から 2px 以内の画素
others = (lab>0) & (lab!=big) & np.isin(lab, [k for k,v in sizes.items() if v>50])
grow = keep.copy()
for _ in range(4):
    g = grow.copy()
    g[1:]|=grow[:-1]; g[:-1]|=grow[1:]; g[:,1:]|=grow[:,:-1]; g[:,:-1]|=grow[:,1:]
    grow = g & ~others
grow &= a[...,3] > 0.0
b = a.copy(); b[~grow] = 0
ys,xs = np.where(grow & (a[...,3]>0.02))
y0,y1,x0,x1 = ys.min(),ys.max()+1,xs.min(),xs.max()+1
c = np.ascontiguousarray(b[y0:y1,x0:x1]); ch,cw = c.shape[:2]
c[...,:3]*=c[...,3:4]
s = 116/max(cw,ch); tw,th = max(1,round(cw*s)),max(1,round(ch*s))
t = bpy.data.images.new("t", cw, ch, alpha=True); t.pixels[:] = c.ravel(); t.scale(tw, th)
r = np.array(t.pixels[:], dtype=np.float32).reshape(th,tw,4)
out = np.zeros((128,128,4),np.float32); oy=(128-th)//2; ox=(128-tw)//2; out[oy:oy+th,ox:ox+tw]=r
al=out[...,3:4]; out[...,:3]=np.where(al>1e-4,out[...,:3]/np.maximum(al,1e-4),0)
o = bpy.data.images.new("o",128,128,alpha=True); o.pixels[:]=out.ravel(); o.filepath_raw=dst; o.file_format='PNG'; o.save()
print("crop", cw, ch, "->", tw, th)
