/* ==================================================================
   render.js   ——  3-D scene + tunnel geometry (no UI code)
   ------------------------------------------------------------------
   • walks top-level joint's leftExtent once (nodes + markers interleaved)
   • exports: scene, camera, renderer, controls, build(), clearDyn()
     plus dynamic arrays & selection handles the UI layer will use.
   ================================================================== */

import * as THREE            from 'three';
import { OrbitControls }     from 'https://cdn.jsdelivr.net/npm/three@0.160.0/examples/jsm/controls/OrbitControls.js';
import { site, cfg, gap } from './model.js';
import { DEFAULT_DISTANCE_METERS, DEFAULT_DELAY_NS } from './constants.js';
import { delayToMeters } from './units.js';
import { playTransitionSound } from './sound.js';

/* reference to the main extent is obtained via site.rootExtent() */

/* ──────────────────────────────────────────────────────────────
   • Public objects & state (controls.js imports these) */
export const scene     = new THREE.Scene();  scene.background = new THREE.Color(0);
export const camera    = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 3000);
export const renderer  = new THREE.WebGLRenderer({ antialias: true });

export const controls  = new OrbitControls(camera, renderer.domElement);
controls.enableDamping = true;
controls.minDistance = 2;
controls.maxDistance = 2000;
/* root group for all dynamic geometry */
let dynGroup = null;

/* dynamic holders (exported) */
export let nodeObjs   = [];   // [{grp,pyr,lab}]
export let spanObjs   = [];   // [{grp,cyl,lab,left,right}]
export let breakObjs  = [];   // [LineSegments]
export let markerObjs = [];   // [{ln,lLab,rLab}]
// Selection handles shared with the UI. `extent` is the owning extent array
// and `localIdx` is the element's index inside it (works for side extents too).
export const sel       = {
  node:null,      // index in nodeObjs array
  joint:null,     // index in nodeObjs when the node is a joint
  break:null,     // index in breakObjs
  marker:null,    // index in markerObjs
  extent:null,    // owning extent array
  localIdx:null   // element index within that extent
};

/* ──────────────────────────────────────────────────────────────
   • One-off renderer setup */
renderer.setSize(innerWidth, innerHeight);
document.body.appendChild(renderer.domElement);
try {
  const { registerPassThroughElement } = await import('./uiState.js');
  if (typeof registerPassThroughElement === 'function') {
    registerPassThroughElement(renderer.domElement);
  }
} catch {}

/* ──────────────────────────────────────────────────────────────
   • Shared geometry / materials */
const matTunnel = new THREE.LineBasicMaterial({ color: 0x00ff00 });
const matBreak  = new THREE.LineBasicMaterial({ color: 0x00ff00 });
const geoPyr    = new THREE.ConeGeometry(cfg.PYR_R, cfg.PYR_L, 4).rotateZ(-Math.PI / 2);
const geoSpan   = new THREE.CylinderGeometry(0.07, 0.07, 1, 8, 1, true);
const profile   = new THREE.Shape().moveTo(-cfg.R, 0).lineTo(cfg.R, 0)
                                   .absarc(0, 0, cfg.R, 0, Math.PI, false);
profile.isRect = false;
const rectProfile = new THREE.Shape().moveTo(-cfg.R, 0).lineTo(cfg.R, 0)
                                   .lineTo(cfg.R, cfg.R).lineTo(-cfg.R, cfg.R);
rectProfile.isRect = true;

function makeJFloor(segLen, reversed = false) {
  const halfW  = cfg.R;
  const baseZ  = reversed ? -segLen * 0.7 : -segLen * 0.3;
  const tipZ   = reversed ? -segLen * 0.3 : -segLen * 0.7;
  const baseX  = halfW * 0.6;

  const verts = [
    -halfW, 0, 0,  halfW, 0, 0,
     halfW, 0, 0,  halfW, 0, -segLen,
     halfW, 0, -segLen, -halfW, 0, -segLen,
    -halfW, 0, -segLen, -halfW, 0, 0,

    -baseX, 0.01, baseZ,  0, 0.01, tipZ,
     baseX, 0.01, baseZ,  0, 0.01, tipZ
  ];
  const geo = new THREE.BufferGeometry();
  geo.setAttribute('position', new THREE.Float32BufferAttribute(verts, 3));
  return new THREE.LineSegments(geo, matTunnel.clone());
}

/* ──────────────────────────────────────────────────────────────
   • Utilities: clear, break, marker, highlight */
export function clearDyn () {
  if (dynGroup) {
    scene.remove(dynGroup);
    dynGroup = null;
  }
  nodeObjs.length = spanObjs.length = breakObjs.length = markerObjs.length = 0;
}

/* ---------- sprite label helper ---------- */
function sprite (text, color = '#ffff00', font = 30, scale = 2.5) {
  const canvas = document.createElement('canvas');
  canvas.width = canvas.height = 256;
  const ctx = canvas.getContext('2d');

  ctx.font         = `${font}px monospace`;
  ctx.fillStyle    = color;
  ctx.textAlign    = 'center';
  ctx.textBaseline = 'middle';
  ctx.fillText(text, 128, 128);

  const tex = new THREE.CanvasTexture(canvas);
  tex.colorSpace       = THREE.SRGBColorSpace;  // r160+ compliant
  tex.minFilter        = THREE.LinearFilter;
  tex.magFilter        = THREE.LinearFilter;
  tex.generateMipmaps  = false;

  const spr = new THREE.Sprite(
    new THREE.SpriteMaterial({
      map: tex,
      transparent: true,
      depthTest: false,
      depthWrite: false
    })
  );
  spr.scale.set(scale, scale, 1);
  spr.userData = { text, font };
  return spr;
}

function outgoingDelay(el){
  if(!el || (el.type !== 'node' && el.type !== 'joint')) return DEFAULT_DELAY_NS;
  if(el.type === 'joint') return el.delayOut ?? DEFAULT_DELAY_NS;
  return el.delayRight ?? DEFAULT_DELAY_NS;
}

function outgoingMeters(el){
  return delayToMeters(outgoingDelay(el), DEFAULT_DISTANCE_METERS);
}

function offsetVector(offset, flipX = false) {
  const { x, y, z } = offset;
  return new THREE.Vector3(flipX ? -x : x, y, z);
}

function recolor (spr, newColor = '#ffff00') {
  const { text, font } = spr.userData;
  const ctx = spr.material.map.image.getContext('2d');
  ctx.clearRect(0, 0, 256, 256);
  ctx.font         = `${font}px monospace`;
  ctx.fillStyle    = newColor;
  ctx.textAlign    = 'center';
  ctx.textBaseline = 'middle';
  ctx.fillText(text, 128, 128);
  spr.material.map.needsUpdate = true;
}

/* make both helpers available to other render.js functions */
export { sprite, recolor };

export function addBreak (zCenter, gVis, leftNodeIdx, group = dynGroup, collect = true, extent = site.rootExtent(), options = {}) {
  const { style = 'zigzag', reversed = false, towardJoint = null } = options;

  const verts = [];
  if (style === 'chevron') {
    const half = gVis / 2;
    const count = 3;
    const spacing = gVis / (count + 1);
    const depth = spacing * 0.6;
    const width = cfg.R * 0.7;
    const arrowTowardsJoint = towardJoint ?? reversed ?? false;
    const dir = arrowTowardsJoint ? 1 : -1;
    const startOffset = dir === -1 ? half : -half;
    verts.push(-cfg.R, 0, zCenter + startOffset, cfg.R, 0, zCenter + startOffset);
    for (let i = 0; i < count; i++) {
      const baseOffset = half - (i + 0.5) * spacing;
      const baseZ = zCenter + (dir === -1 ? baseOffset : -baseOffset);
      const tipZ = baseZ + dir * depth;
      verts.push(-width, 0, baseZ, 0, 0, tipZ);
      verts.push(width, 0, baseZ, 0, 0, tipZ);
    }
  } else {
    const half = gVis / 2;
    const dz = gVis / 8;
    for (let k = 0; k < 8; k++) {
      const z1 = zCenter + half - k * dz;
      const z2 = z1 - dz;
      const x1 = k % 2 ? -cfg.R : cfg.R;
      const x2 = k % 2 ?  cfg.R : -cfg.R;
      verts.push(x1, 0, z1, x2, 0, z2);
    }
  }

  const geo = new THREE.BufferGeometry();
  geo.setAttribute('position', new THREE.Float32BufferAttribute(verts, 3));
  const ln  = new THREE.LineSegments(geo, matBreak.clone());
  ln.material.color.set(cfg.COL_I);
  ln.userData = { dyn:true, isBreak:true, leftIdx:leftNodeIdx, extent };
  if (collect) {
    ln.userData.index = breakObjs.length;
    breakObjs.push(ln);
  }
  if (group) group.add(ln);
}

export function makeMarker (midZ, gVis, leftNodeIdx, leftZone, rightZone,
                            group = dynGroup, collect = true, extent = site.rootExtent(),
                            localIdx = 0) {
  const halfW = 5.0;                 // 10 m across tunnel width
  const halfH = 3.5;                 // 7  m tall
  const halfD = gVis / 2 - 0.05;     // fill gap front-to-back
  const yBase = 0.02;
  const reversed = !!extent?.[0]?.reverse;

  /* rectangle outline (front & rear) */

    const verts = [
	-halfW, yBase,        midZ + halfD,   halfW, yBase,        midZ + halfD,
	halfW, yBase,        midZ + halfD,   halfW, yBase + 7.0,  midZ + halfD,
	halfW, yBase + 7.0,  midZ + halfD,  -halfW, yBase + 7.0,  midZ + halfD,
	-halfW, yBase + 7.0,  midZ + halfD,  -halfW, yBase,        midZ + halfD
    ];

  const geo = new THREE.BufferGeometry();
  geo.setAttribute('position', new THREE.Float32BufferAttribute(verts, 3));
  const ln  = new THREE.LineSegments(geo, new THREE.LineBasicMaterial({ color: cfg.COL_I }));
  ln.userData = { dyn:true, isMarker:true, leftIdx:leftNodeIdx, extent, localIdx };
  if (collect) ln.userData.index = markerObjs.length;

  /* zone labels */
  const lblL = sprite(leftZone  || '', '#ffff00', 32, 4),
        lblR = sprite(rightZone || '', '#ffff00', 32, 4);
  const planeZ = midZ + halfD;
  const lblSep = cfg.ZONE_LBL_SEP;
  const leftOffset  = reversed ? -lblSep : lblSep;
  const rightOffset = reversed ?  lblSep : -lblSep;
  lblL.position.set(0, halfH * 1.7, planeZ + leftOffset);
  lblR.position.set(0, halfH * 1.7, planeZ + rightOffset);
  // make labels and an invisible plane selectable
  Object.assign(lblL.userData, { isMarker:true, extent, localIdx });
  Object.assign(lblR.userData, { isMarker:true, extent, localIdx });
  if (collect) { lblL.userData.index = ln.userData.index; lblR.userData.index = ln.userData.index; }
  const pickPlane = new THREE.Mesh(
    new THREE.PlaneGeometry(halfW * 2, 7),
    new THREE.MeshBasicMaterial({ visible:false })
  );
  pickPlane.position.set(0, yBase + halfH, planeZ);
  pickPlane.userData = { ...ln.userData };
  ln.add(lblL, lblR, pickPlane);

  if (group) group.add(ln);
  if (collect) markerObjs.push({ ln, lLab:lblL, rLab:lblR });
}

/* highlight helpers (used by UI layer) */
export function highlightNode   (i,on){ const o=nodeObjs[i]; if(!o) return;
  o.pyr.material.color.set(on?cfg.COL_S:cfg.COL_I);
  recolor(o.lab,on?'#ff0000':'#ffff00');
  spanObjs.forEach(sp=>{
    if(sp.left===i||sp.right===i){
      sp.cyl.material.color.set(on?cfg.COL_S:cfg.COL_I);
      recolor(sp.lab,on?'#ff0000':'#ffff00');
    }
  });
}
export const highlightBreak  = (i,on)=> breakObjs[i]?.material.color.set(on?cfg.COL_S:cfg.COL_I);
export const highlightMarker = (i,on)=>{ const m=markerObjs[i];
  if(!m) return; m.ln.material.color.set(on?cfg.COL_S:cfg.COL_I);
  recolor(m.lLab,on?'#ff0000':'#ffff00'); recolor(m.rLab,on?'#ff0000':'#ffff00');
};
export const highlightJoint = highlightNode;

/* build helper for rendering an extent */
function buildConnexions(list, group, collect) {
  const reversed = !!list[0]?.reverse;
  const isRootExtent = list === site.rootExtent();
  const sgn = reversed ? -1 : 1;
  const offMac = offsetVector(cfg.OFF_MAC, reversed);
  const offDis = offsetVector(cfg.OFF_DIS, reversed);
  let zCursor = 0,
      pendingGap = 0,
      nPrev = null,
      nCurr = -1,
      lastNodeZ = null,
      firstSegLen = cfg.SEG,
      lastSegLen = cfg.SEG,
      lastIdx = null;             // global index of previous node

  for (let arrIdx = 1; arrIdx < list.length; arrIdx++) {
    const el = list[arrIdx];
    if (pendingGap) { zCursor -= pendingGap; pendingGap = 0; }

    if (el.type === 'node' || el.type === 'joint') {
      const segLen = el.type === 'joint' ? cfg.JSEG : cfg.SEG;
      if (nCurr === -1) firstSegLen = segLen;
      nCurr++;

      const currIdx = nodeObjs.length;           // unique global index
      const isJ = el.type === 'joint';
      const shape = el.evacpoint ? rectProfile : profile;
      const seg = isJ
        ? makeJFloor(segLen, reversed)
        : new THREE.LineSegments(
            new THREE.EdgesGeometry(
              new THREE.ExtrudeGeometry(shape,{ depth: segLen, bevelEnabled:false })
            ),
            matTunnel.clone()
          );
      seg.position.z = isJ ? zCursor : zCursor - segLen;
      seg.userData = { dyn:true, extent:list, localIdx:arrIdx };
      if (isJ && collect) Object.assign(seg.userData, {
        index: currIdx,
        joint: true
      });
      if (group) group.add(seg);

      const mac = sprite((el.id ?? '').split(':').slice(3).join(':'), '#ffff00');
      mac.position.copy(new THREE.Vector3(sgn*cfg.R,cfg.H,zCursor - segLen/2).add(offMac));
      const gNode = new THREE.Group(); gNode.add(mac); gNode.userData.dyn=true; if(group) group.add(gNode);

      let selObj = seg;
      if (!isJ) {
        const pyr = new THREE.LineSegments(
          new THREE.WireframeGeometry(geoPyr),
          new THREE.LineBasicMaterial({ color: cfg.COL_I })
        );
        pyr.position.set(sgn*(cfg.R - cfg.PYR_L/2), cfg.H, zCursor - segLen/2);
        if (collect) pyr.userData = {
          index: currIdx,
          joint: false,
          extent: list,
          localIdx: arrIdx
        };
        gNode.add(pyr); selObj = pyr;
      }
      if (collect) nodeObjs.push({ grp:gNode, pyr:selObj, lab:mac, seg });

      if (nPrev !== null) {
        const prevNode = nodeByIndexIn(list,nPrev);
        const prevSeg  = prevNode.type==='joint' ? cfg.JSEG : cfg.SEG;
        const dist  = outgoingMeters(prevNode);
        const gVis  = gap(dist);
        const prevPyrZ = lastNodeZ - prevSeg/2;
        const currPyrZ = zCursor - segLen/2;
        const spanZ = (prevPyrZ + currPyrZ)/2;
        const breakZ = lastNodeZ - prevSeg - gVis/2;
        const spanLen = prevSeg/2 + gVis + segLen/2;

        const cyl = new THREE.Mesh(geoSpan,new THREE.MeshBasicMaterial({color:cfg.COL_I}));
        cyl.scale.set(1, spanLen, 1);
        cyl.position.set(sgn*cfg.R,cfg.H,spanZ);
        cyl.rotation.x = Math.PI/2;
        const lbl = sprite(dist.toFixed(1)+'m','#ffff00',32,3);
        lbl.position.copy(new THREE.Vector3(sgn*cfg.R,cfg.H,spanZ).add(offDis));

        addBreak(breakZ, gVis, nPrev, group, collect, list);

        const gSpan = new THREE.Group();
        gSpan.add(cyl, lbl);
        gSpan.userData.dyn = true;
        if (group) group.add(gSpan);
        if (collect) spanObjs.push({
          grp: gSpan,
          cyl,
          lab: lbl,
          left: lastIdx,
          right: currIdx
        });
      }

      pendingGap = gap(outgoingMeters(el));
      lastNodeZ  = zCursor;
      lastSegLen = segLen;
      zCursor   -= segLen;
      nPrev      = nCurr;
      lastIdx    = currIdx;

      if (isJ) {
        if (el.leftExtent) {
          const fg = new THREE.Group();
          fg.position.set(-cfg.R - cfg.BASE_GAP, 0, zCursor + cfg.JSEG/2);
          fg.rotation.y = Math.PI / 2;
          if (group) group.add(fg);
          buildConnexions(el.leftExtent, fg, collect);
        }
        if (el.rightExtent) {
          const bg = new THREE.Group();
          bg.position.set(cfg.R + cfg.BASE_GAP, 0, zCursor + cfg.JSEG/2);
          bg.rotation.y = -Math.PI / 2;
          if (group) group.add(bg);
          buildConnexions(el.rightExtent, bg, collect);
        }
      }
      continue;
    }

    if (el.type === 'marker') {
      if (nPrev !== null) {
        const prevNode = nodeByIndexIn(list,nPrev);
        const prevSeg  = prevNode.type==='joint'?cfg.JSEG:cfg.SEG;
        const dist  = outgoingMeters(prevNode);
        const gVis  = gap(dist);
        const midZ  = lastNodeZ - prevSeg - gVis + 0.05;
        makeMarker(midZ, gVis, nPrev, el.leftZone, el.rightZone, group, collect, list, arrIdx);
      } else {
        const gVis  = cfg.BASE_GAP;
        const midZ  = firstSegLen/2 + 0.05;
        makeMarker(midZ, gVis, -1, el.leftZone, el.rightZone, group, collect, list, arrIdx);
      }
    }
  }

  const entryOpts = isRootExtent ? undefined : { style:'chevron', towardJoint: !!list[0]?.reverse };
  addBreak(cfg.BASE_GAP/2, cfg.BASE_GAP, -1, group, collect, list, entryOpts);
  if (nPrev !== null) {
    addBreak(lastNodeZ - lastSegLen - cfg.BASE_GAP/2, cfg.BASE_GAP, nPrev, group, collect, list);
  }
}

function introZoom () {
  const focusShift = cfg.SEG + cfg.BASE_GAP;
  const finalPos = { x: cfg.R * 4, y: cfg.R * 2, z: 6 - focusShift };
  const finalTgt = { x: 0, y: cfg.H, z: -focusShift };
  const len = Math.hypot(finalPos.x, finalPos.y, finalPos.z);
  const scale = controls.maxDistance / len;
  const startPos = {
    x: finalPos.x * scale,
    y: finalPos.y * scale,
    z: finalPos.z * scale
  };
  camera.position.set(startPos.x, startPos.y, startPos.z);
  controls.target.set(finalTgt.x, finalTgt.y, finalTgt.z);
  controls.update();
  const start = performance.now();
  const dur = 1000;
  function step () {
    const t = Math.min((performance.now() - start) / dur, 1);
    camera.position.set(
      startPos.x + (finalPos.x - startPos.x) * t,
      startPos.y + (finalPos.y - startPos.y) * t,
      startPos.z + (finalPos.z - startPos.z) * t
    );
    controls.update();
    if (t < 1) requestAnimationFrame(step);
  }
  requestAnimationFrame(step);
}

/* ──────────────────────────────────────────────────────────────
   • Main builder  */
export function build (preserveCam=false, heading=0) {

  /* save camera, clear dynamic layers */
  const camPos = preserveCam ? camera.position.clone()  : null;
  const camTgt = preserveCam ? controls.target.clone() : null;
  clearDyn();

  /* root group even if the extent is empty */
  dynGroup = new THREE.Group();
  dynGroup.userData.dyn = true;
  dynGroup.rotation.y = heading;
  scene.add(dynGroup);
  buildConnexions(site.rootExtent(), dynGroup, true);

  if (preserveCam && camPos && camTgt) {
    camera.position.copy(camPos);
    controls.target.copy(camTgt);
  } else {
    introZoom();
  }


}

/* helper: get node object by its node index (i.e. 0,1,2…) */
function nodeByIndex(idx){
  let n=-1;
  for(const el of site.rootExtent())
    if((el.type==='node' || el.type==='joint') && ++n===idx) return el;
  return null;
}

function nodeByIndexIn(arr, idx){
  let n=-1;
  for(const el of arr)
    if((el.type==='node' || el.type==='joint') && ++n===idx) return el;
  return null;
}

/* ──────────────────────────────────────────────────────────────
   ‣ Pointer-picking helper – exported                           */
const ray   = new THREE.Raycaster();
const mouse = new THREE.Vector2();
ray.params.Line.threshold = 0.2;

/**  pick(e)
 *    • e = PointerEvent from the canvas
 *    • returns one of:
 *        { hit:false }                                 – nothing
 *        { hit:true, kind:'node'  , nodeIdx, extent, localIdx }
 *        { hit:true, kind:'marker', markerIdx, extent, localIdx }
 *        { hit:true, kind:'break' , breakIdx }
 */
export function pick (e) {
  const rect = renderer.domElement.getBoundingClientRect();
  mouse.x = ((e.clientX - rect.left) / rect.width ) * 2 - 1;
  mouse.y = -((e.clientY - rect.top ) / rect.height) * 2 + 1;
  ray.setFromCamera(mouse, camera);

  /* search nodes → markers → breaks (highest → lowest priority) */
  const hits = ray.intersectObjects([
    ...nodeObjs.map(o => o.pyr),
    ...markerObjs.map(m => m.ln),
    ...breakObjs
  ], true);

  if (!hits.length) return { hit:false };

  let chosen = null;
  let bestPrio = Infinity; // lower is higher priority
  for (const { object } of hits) {
    let prio = 3, kind = 'break';
    if (object.userData?.isMarker) { prio = 1; kind = 'marker'; }
    else if (object.userData?.index !== undefined && object.userData.localIdx !== undefined) {
      prio = 2; kind = object.userData.joint ? 'joint' : 'node';
    }

    if (prio < bestPrio) {
      bestPrio = prio;
      chosen = { object, kind };
      if (prio === 1) break; // cannot get higher priority than marker
    }
  }

  if (!chosen) return { hit:false };
  const obj = chosen.object;
  if (chosen.kind === 'marker') {
    return {
      hit:true,
      kind:'marker',
      markerIdx: obj.userData.index,
      extent  : obj.userData.extent,
      localIdx: obj.userData.localIdx
    };
  }
  if (chosen.kind === 'node' || chosen.kind === 'joint') {
    return {
      hit:true,
      kind: chosen.kind,
      nodeIdx: obj.userData.index,
      extent : obj.userData.extent,
      localIdx: obj.userData.localIdx
    };
  }
  if (obj.userData?.isBreak) {
    return { hit:true, kind:'break', breakIdx: obj.userData.index };
  }
  return { hit:false };
}

/* ──────────────────────────────────────────────────────────────
   • initial build + animate */
if (!site.deferInitialBuild) {
  build();
  playTransitionSound();
}
window.addEventListener('resize',()=>{
  camera.aspect = innerWidth/innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(innerWidth,innerHeight);
});
(function anim(){ requestAnimationFrame(anim);
  controls.update(); renderer.render(scene,camera); })();
