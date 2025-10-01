/* ==================================================================
   controls.js   ——  UI layer (dialogs, buttons, selection, hot-keys)
   Works with render.js exports.  No Three-geometry here.
   ================================================================== */

import {
  scene, camera, renderer, controls, build, clearDyn, pick,
  nodeObjs, breakObjs, markerObjs,
  highlightNode, highlightJoint, highlightBreak, highlightMarker,
  sel
} from './render.js';  // `sel` holds selection info; see render.js for details

import {
  site, connexions, CAP_POOL,
  createNode, createJoint, createZoneMarker,
  deleteNodeIn, deleteMarkerIn,
  extentInserted, extentEmptied,
  findJointForExtent
} from './model.js';
import { delayToMeters, metersToDelay } from './units.js';
import { DEFAULT_DELAY_NS, DEFAULT_DISTANCE_METERS } from './constants.js';

/* ── undo/redo history ───────────────────────────── */
const history = [];
let histIdx = -1;

// camera flight constants
export const FLY_ACCEL = 0.05;
export const FLY_MAX_SPEED = 2.0;
let flyData = null;
function pushHistory(doFn, undoFn, label = ''){
  history.splice(histIdx+1);
  history.push({doFn, undoFn, label});
  histIdx = history.length-1;
  updateHistButtons();
}
function undoHist(){
  if(histIdx < 0) return;
  const entry = history[histIdx];
  entry.undoFn();
  histIdx--;
  updateHistButtons();
  if(entry.label){
    showPopup(`Undo ${entry.label}`);
  }
}
function redoHist(){
  if(histIdx+1 >= history.length) return;
  histIdx++;
  const entry = history[histIdx];
  entry.doFn();
  updateHistButtons();
  if(entry.label){
    showPopup(`Redo ${entry.label}`);
  }
}
function canUndo(){ return histIdx >= 0; }
function canRedo(){ return histIdx + 1 < history.length; }

function makeExtentGetter(arr){
  if(arr === connexions) return () => connexions;
  const j = findJointForExtent(arr);
  if(!j) return () => arr;
  const side = j.leftExtent === arr ? 'leftExtent' : 'rightExtent';
  return () => j[side];
}

function locateElement(target, extent = connexions){
  if(!target) return null;
  for(let i=0; i<extent.length; i++){
    const el = extent[i];
    if(el === target) return { extent, idx:i };
    if(el && el.type === 'joint'){
      if(Array.isArray(el.leftExtent)){
        const found = locateElement(target, el.leftExtent);
        if(found) return found;
      }
      if(Array.isArray(el.rightExtent)){
        const found = locateElement(target, el.rightExtent);
        if(found) return found;
      }
    }
  }
  return null;
}

function resolveElement(target){
  const found = locateElement(target);
  if(!found) return null;
  return {
    idx: found.idx,
    getArr: makeExtentGetter(found.extent)
  };
}

function getOutgoingDelay(el){
  if(!el || (el.type !== 'node' && el.type !== 'joint')) return DEFAULT_DELAY_NS;
  if(el.type === 'joint') return el.delayOut ?? DEFAULT_DELAY_NS;
  return el.delayRight ?? DEFAULT_DELAY_NS;
}

/* ──────────────────────────────────────────────────────────────
   ‣ Utility — make fixed green button                            */
function makeButton(label, top, onclick = null, left = 'center', parent = document.body) {
  const b = document.createElement('button');
  b.textContent = label;
  const pos = parent === document.body
    ? `position:fixed; top:${top}px; ${left === 'center'
        ? 'left:50%; transform:translateX(-50%);'
        : left === 'left'
          ? 'left:10px;'
          : 'right:10px;'} `
    : '';
  b.style.cssText = `
    ${pos}background:#0f0; color:#000; border:none; padding:6px 14px;
    font:0.9rem monospace; border-radius:4px; cursor:pointer; display:none;
  `;
  if (onclick) b.onclick = onclick;
  b.onmouseenter = () => (b.style.filter = 'brightness(1.2)');
  b.onmouseleave = () => (b.style.filter = '');
  parent.appendChild(b);
  return b;
}

/* ──────────────────────────────────────────────────────────────
   ‣ Transient pop-up dialog                                     */
const activePopups = [];

function updatePopupPositions(){
  let offset = 0;
  for (const popup of activePopups) {
    const height = popup.offsetHeight || 0;
    popup.style.top = `calc(50% + ${offset}px)`;
    offset += height + 12;
  }
}

function schedulePopupLayout(){
  if (typeof requestAnimationFrame === 'function') {
    requestAnimationFrame(updatePopupPositions);
  } else {
    setTimeout(updatePopupPositions, 0);
  }
}

export function showPopup(text){
  const dlg = document.createElement('div');
  dlg.textContent = text;
  dlg.style.cssText = `
    position:fixed; top:50%; left:50%; transform:translateX(-50%);
    background:rgba(0,0,0,0.7); color:#fff; padding:16px 28px;
    border-radius:6px; font:2rem monospace; pointer-events:none;
    opacity:0.85; transition:opacity 5s;
  `;
  document.body.appendChild(dlg);
  activePopups.unshift(dlg);
  schedulePopupLayout();
  requestAnimationFrame(()=>{ dlg.style.opacity = '0'; });

  const remove = () => {
    const idx = activePopups.indexOf(dlg);
    if (idx !== -1) activePopups.splice(idx, 1);
    dlg.remove();
    schedulePopupLayout();
  };
  setTimeout(remove, 5000);
}

/* ──────────────────────────────────────────────────────────────
   ‣ Global selection cleanup                                     */
function clearSelection () {
  if (sel.node   !== null) { highlightNode  (sel.node  , false); }
  if (sel.joint  !== null) { highlightJoint(sel.joint , false); }
  if (sel.break  !== null) { highlightBreak (sel.break , false); }
  if (sel.marker !== null) { highlightMarker(sel.marker, false); }
  sel.node = sel.joint = sel.break = sel.marker = null;
  // owning extent and index (works for main and side extents)
  sel.extent = sel.localIdx = null;

  addBtn.style.display    =
  jointBtn.style.display  =
  markerBtn.style.display =
  nodeDlg.style.display   =
  markDlg.style.display   =
  siteDlg.style.display   = 'none';
}

/* ──────────────────────────────────────────────────────────────
   ‣ Buttons                                                     */
const addBar = document.createElement('div');
addBar.style.cssText = `
  position:fixed; top:10px; left:50%; transform:translateX(-50%);
  display:flex; gap:6px;
`;
document.body.appendChild(addBar);

const addBtn    = makeButton('Add node'  , 0, addNodeAtBreak, 'center', addBar);
const markerBtn = makeButton('Add marker', 0, addMarkerAtBreak, 'center', addBar);
const jointBtn  = makeButton('Add joint' , 0, addJointAtBreak, 'center', addBar);

const viewCtrlBar = document.createElement('div');
viewCtrlBar.style.cssText = `
  position:fixed; top:40px; left:10px;
  display:flex; gap:6px; align-items:center;
  z-index:5;
`;
document.body.appendChild(viewCtrlBar);

const resetBtn  = makeButton('Reset view', 0, resetCam, 'left', viewCtrlBar);
const undoBtn   = makeButton('Undo', 0, () => { undoHist(); clearSelection(); build(true); updateHistButtons(); }, 'left', viewCtrlBar);
const redoBtn   = makeButton('Redo', 0, () => { redoHist(); clearSelection(); build(true); updateHistButtons(); }, 'left', viewCtrlBar);
[resetBtn, undoBtn, redoBtn].forEach(btn => { btn.style.display = ''; });

function setHistoryButtonState(btn, enabled) {
  btn.disabled = !enabled;
  btn.style.backgroundColor = enabled ? '#0f0' : '#555';
  btn.style.color = enabled ? '#000' : '#bbb';
  btn.style.cursor = enabled ? 'pointer' : 'default';
  btn.style.opacity = enabled ? '1' : '0.6';
}

function updateHistButtons(){
  setHistoryButtonState(undoBtn, canUndo());
  setHistoryButtonState(redoBtn, canRedo());
}
updateHistButtons();

/* ── search field ───────────────────────────────────────────── */
const searchInput = document.getElementById('findNode');
if(searchInput){
  searchInput.addEventListener('keydown',e=>{
    if(e.key==='Enter'){
      const term = searchInput.value.trim().toLowerCase();
      if(!term) return;
      const matches = [];
      nodeObjs.forEach((o,idx)=>{
        const data = o.pyr.userData.extent[o.pyr.userData.localIdx];
        if(data.id && data.id.toLowerCase().includes(term)) matches.push(idx);
      });
      if(matches.length===1){
        const idx = matches[0];
        const obj = nodeObjs[idx];
        if(obj){
          clearSelection();
          sel.node     = idx;
          sel.extent   = obj.pyr.userData.extent;
          sel.localIdx = obj.pyr.userData.localIdx;
          highlightNode(idx, true);
          openNodeDialog(sel.extent, sel.localIdx);
          flyToNode(idx);
        }
      }
    }
  });
}

/* ──────────────────────────────────────────────────────────────
   ‣ Camera flight helper                                        */
function startFly(destPos, destTgt){
  flyData = {
    destPos,
    destTgt,
    speed: 0
  };
  requestAnimationFrame(flyStep);
}

function flyStep(){
  if(!flyData) return;
  const posDiff = flyData.destPos.clone().sub(camera.position);
  const tgtDiff = flyData.destTgt.clone().sub(controls.target);
  const dist = Math.max(posDiff.length(), tgtDiff.length());
  if(dist < 0.01 && flyData.speed < 0.01){
    camera.position.copy(flyData.destPos);
    controls.target.copy(flyData.destTgt);
    controls.update();
    flyData = null;
    return;
  }

  const stop = (flyData.speed * flyData.speed) / (2 * FLY_ACCEL);
  if(dist < stop) flyData.speed = Math.max(0, flyData.speed - FLY_ACCEL);
  else            flyData.speed = Math.min(FLY_MAX_SPEED, flyData.speed + FLY_ACCEL);

  const step = Math.min(dist, flyData.speed);
  if(posDiff.length()>0) camera.position.add(posDiff.normalize().multiplyScalar(step));
  if(tgtDiff.length()>0) controls.target.add(tgtDiff.normalize().multiplyScalar(step));
  controls.update();
  requestAnimationFrame(flyStep);
}

function flyToNode(idx){
  const obj = nodeObjs[idx];
  if(!obj) return;
  const pos = obj.pyr.getWorldPosition(camera.position.clone());
  const offset = camera.position.clone().sub(controls.target);
  const destTgt = pos.clone();
  const destPos = pos.clone().add(offset);
  startFly(destPos, destTgt);
}

/* ──────────────────────────────────────────────────────────────
   ‣ Node dialog (bottom-right)                                  */
const nodeDlg = document.createElement('div');
nodeDlg.style.cssText = `
  position:fixed; bottom:10px; right:10px; width:250px; background:#111;
  border:1px solid #666; border-radius:6px; padding:8px; color:#fff;
  font:0.8rem monospace; display:none;
`;
nodeDlg.innerHTML = `
  <h3 style="margin:0 0 6px;font-size:0.9rem;color:#0f0">Node properties</h3>
  <label>MAC <input id="mac"  type="text"
         style="width:100%;background:#222;border:1px solid #555;color:#fff;
                padding:2px 4px;font:0.8rem monospace"></label>
  <label>Distance to right (m) <input id="dist" type="number" step="1"
         style="width:100%;background:#222;border:1px solid #555;color:#fff;
                padding:2px 4px;font:0.8rem monospace"></label>
  <div style="margin:6px 0 2px">Capabilities</div>
  <div id="caps"></div>
  <button id="applyNode"
          style="margin-top:6px;width:100%;background:#0f0;border:none;
                 color:#000;padding:4px 0;font:0.85rem monospace;border-radius:4px;
                 cursor:pointer">Apply</button>
`;
document.body.appendChild(nodeDlg);

/* ──────────────────────────────────────────────────────────────
   ‣ Joint dialog                                                */
const jointDlg = document.createElement('div');
jointDlg.style.cssText = `
  position:fixed; bottom:10px; right:10px; width:230px; background:#111;
  border:1px solid #666; border-radius:6px; padding:8px; color:#fff;
  font:0.8rem monospace; display:none;
`;
jointDlg.innerHTML = `
  <h3 style="margin:0 0 6px;font-size:0.9rem;color:#0f0">Joint</h3>
  <label>Distance in (m) <input id="jDistIn" type="number" step="1"
         style="width:100%;background:#222;border:1px solid #555;color:#fff;
                padding:2px 4px;font:0.8rem monospace"></label>
  <label style="margin-top:6px">Distance out (m) <input id="jDistOut" type="number" step="1"
         style="width:100%;background:#222;border:1px solid #555;color:#fff;
                padding:2px 4px;font:0.8rem monospace"></label>
  <label style="margin-top:6px;display:flex;align-items:center;gap:6px">
    <input id="jEgress" type="checkbox" style="margin:0"> Egress
  </label>
  <label style="margin-top:6px;display:flex;align-items:center;gap:6px">
    <input id="jReversed" type="checkbox" style="margin:0"> Reversed
  </label>
  <button id="applyJoint"
          style="margin-top:8px;width:100%;background:#0f0;border:none;
                 color:#000;padding:4px 0;font:0.85rem monospace;border-radius:4px;
                 cursor:pointer">Apply</button>
`;
document.body.appendChild(jointDlg);

function findClosestBranchNode(joint){
  if(!joint || joint.type !== 'joint') return null;
  const inspect = ext => {
    if(!Array.isArray(ext) || ext.length <= 1) return null;
    const firstConn = ext[1];
    if(firstConn && firstConn.type === 'node'){
      return { node: firstConn, extent: ext, idx: 1 };
    }
    return null;
  };
  return inspect(joint.leftExtent) || inspect(joint.rightExtent);
}

/* helper — fill capability check-boxes */
function fillCaps (node) {
  const wrap = document.getElementById('caps');
  wrap.innerHTML = '';
  if (!node) return;

  const ensureMask = () => {
    if (typeof node.capabilities !== 'number') {
      node.capabilities = 0;
    }
    return node.capabilities >>> 0;
  };

  const setAttr = (el, name, value) => {
    if (name === 'role') el.role = value;
    else if (name === 'aria-checked') el.ariaChecked = value;
    if (typeof el.setAttribute === 'function') {
      el.setAttribute(name, value);
    }
  };

  CAP_POOL.forEach(({ label, bit }) => {
    const item = document.createElement('div');
    item.className = 'cap-item';
    setAttr(item, 'role', 'checkbox');
    item.tabIndex = 0;
    item.textContent = label;

    const updateVisual = () => {
      const selected = ((ensureMask() & bit) !== 0);
      item.className = selected ? 'cap-item cap-item--selected' : 'cap-item';
      setAttr(item, 'aria-checked', selected ? 'true' : 'false');
    };

    const toggle = () => {
      const current = ensureMask();
      const next = (current & bit) !== 0
        ? (current & ~bit) >>> 0
        : (current | bit) >>> 0;
      if(next === current) return;
      node.capabilities = next;
      updateVisual();

      const resolved = resolveElement(node);
      if(!resolved){
        updateHistButtons();
        build(true);
        return;
      }
      const { getArr, idx } = resolved;

      pushHistory(
        () => {
          const a = getArr();
          const el = a[idx];
          if(el && el.type==='node'){ el.capabilities = next; }
          node.capabilities = next;
          updateVisual();
        },
        () => {
          const a = getArr();
          const el = a[idx];
          if(el && el.type==='node'){ el.capabilities = current; }
          node.capabilities = current;
          updateVisual();
        },
        'change node capability'
      );
      updateHistButtons();
      build(true);
    };

    item.addEventListener('click', e => {
      e.preventDefault();
      toggle();
    });
    item.addEventListener('keydown', e => {
      if (e.key === ' ' || e.key === 'Enter') {
        e.preventDefault();
        toggle();
      }
    });

    wrap.appendChild(item);
    updateVisual();
  });
}

/* open the node dialog */
function openNodeDialog (extent = connexions, idx = 0) {
  const arr = extent;
  const n = arr[idx];
  if (!n || n.type !== 'node') return;

  document.getElementById('mac').value = n.id ?? '';
  const metres = delayToMeters(n.delayRight, DEFAULT_DISTANCE_METERS);
  document.getElementById('dist').value = Math.round(metres * 100) / 100;
  fillCaps(n);
  nodeDlg.style.display = 'block';
  markDlg.style.display = jointDlg.style.display = siteDlg.style.display = 'none';
}

/* apply node changes */
function applyNode () {
  if (!sel.extent) return;
  const arr = sel.extent;
  const getArr = makeExtentGetter(arr);
  const idx = sel.localIdx;
  const n = arr[idx];
  if (!n || n.type !== 'node') return;

  const oldId   = n.id;
  const oldDelay = n.delayRight;
  const newId   = document.getElementById('mac').value.trim();
  const newDist = parseFloat(document.getElementById('dist').value) || DEFAULT_DISTANCE_METERS;
  const newDelay = metersToDelay(newDist, DEFAULT_DELAY_NS);

  n.id   = newId;
  n.delayRight = newDelay;
  pushHistory(
    () => {
      const a = getArr();
      const el = a[idx];
      if(el && el.type==='node'){ el.id=newId; el.delayRight=newDelay; }
    },
    () => {
      const a = getArr();
      const el = a[idx];
      if(el && el.type==='node'){ el.id=oldId; el.delayRight=oldDelay; }
    },
    'change node properties'
  );
  updateHistButtons();

  clearSelection(); build(true); nodeDlg.style.display = 'none';
}
document.getElementById('applyNode').onclick = applyNode;
nodeDlg.addEventListener('keydown', e=>{
  if (e.key === 'Enter'){ e.preventDefault(); applyNode(); }
});

/* open the joint dialog */
function openJointDialog(extent = connexions, idx = 0){
  const arr = extent;
  const j = arr[idx];
  if(!j || j.type!=='joint') return;

  const metresIn = delayToMeters(j.delayIn, DEFAULT_DISTANCE_METERS);
  const metresOut = delayToMeters(j.delayOut, DEFAULT_DISTANCE_METERS);
  document.getElementById('jDistIn').value  = Math.round(metresIn * 100) / 100;
  document.getElementById('jDistOut').value = Math.round(metresOut * 100) / 100;
  const egressCb = document.getElementById('jEgress');
  const branchInfo = findClosestBranchNode(j);
  if(egressCb){
    egressCb.disabled = !branchInfo;
    egressCb.checked = branchInfo ? !!j.egress : false;
  }
  const revCb = document.getElementById('jReversed');
  if(revCb){
    const sides = ['leftExtent', 'rightExtent'].filter(side => Array.isArray(j[side]));
    const primary = sides.find(side => Array.isArray(j[side]) && j[side].length > 0) || sides[0];
    const ext = primary ? j[primary] : null;
    revCb.disabled = !ext;
    revCb.checked = !!j.reversed;
    revCb._jointSides = sides;
    if ('dataset' in revCb && revCb.dataset) revCb.dataset.side = primary || '';
    revCb._jointSide = primary || '';
  }
  jointDlg.style.display = 'block';
  nodeDlg.style.display = markDlg.style.display = siteDlg.style.display = 'none';
}

function applyJoint(){
  if(!sel.extent) return;
  const arr = sel.extent;
  const getArr = makeExtentGetter(arr);
  const idx = sel.localIdx;
  const j = arr[idx];
  if(!j || j.type!=='joint') return;

  const oldIn  = j.delayIn;
  const oldOut = j.delayOut;
  const oldEgress = !!j.egress;
  const newInMeters  = parseFloat(document.getElementById('jDistIn').value) || DEFAULT_DISTANCE_METERS;
  const newOutMeters = parseFloat(document.getElementById('jDistOut').value) || DEFAULT_DISTANCE_METERS;
  const newIn  = metersToDelay(newInMeters, DEFAULT_DELAY_NS);
  const newOut = metersToDelay(newOutMeters, DEFAULT_DELAY_NS);
  const egressCb = document.getElementById('jEgress');
  const branchInfo = findClosestBranchNode(j);
  const desiredEgress = egressCb ? !!egressCb.checked : false;
  const newEgress = branchInfo ? desiredEgress : false;
  const getNodeExtent = branchInfo ? makeExtentGetter(branchInfo.extent) : null;
  const nodeIdx = branchInfo ? branchInfo.idx : null;
  const oldNodeEvac = branchInfo ? !!branchInfo.node.evacpoint : false;

  const revCb = document.getElementById('jReversed');
  const preferredSide = (j.directionRight && Array.isArray(j.rightExtent)) ? 'rightExtent' : 'leftExtent';
  const storedSides = revCb ? (revCb._jointSides ?? []) : [];
  let targetSides = storedSides.length ? storedSides : [preferredSide];
  targetSides = targetSides.filter(side => Array.isArray(j[side]));
  if (!targetSides.length) {
    targetSides = ['leftExtent','rightExtent'].filter(side => Array.isArray(j[side]));
  }
  const canToggleReverse = !!revCb && !revCb.disabled && targetSides.length;
  const oldReverseMap = new Map();
  const oldJointReversed = !!j.reversed;
  const newReverse = canToggleReverse ? !!revCb.checked : oldJointReversed;

  j.delayIn = newIn;
  j.delayOut = newOut;
  j.egress = newEgress;
  if(branchInfo && branchInfo.node.type === 'node'){
    branchInfo.node.evacpoint = newEgress;
  }
  if (canToggleReverse) {
    for (const side of targetSides) {
      const ext = j[side];
      if (!Array.isArray(ext)) continue;
      ext[0] = ext[0] || { reverse:false };
      oldReverseMap.set(side, !!ext[0].reverse);
      ext[0].reverse = newReverse;
    }
  }
  j.reversed = newReverse;
  pushHistory(
    () => {
      const a=getArr(); const el=a[idx];
      if(el && el.type==='joint'){
        el.delayIn=newIn; el.delayOut=newOut; el.egress=newEgress;
        if(getNodeExtent && nodeIdx!==null){
          const ext = getNodeExtent();
          const node = ext && ext[nodeIdx];
          if(node && node.type==='node'){ node.evacpoint = newEgress; }
        }
        if (canToggleReverse) {
          for (const side of targetSides) {
            const ext = el[side];
            if (Array.isArray(ext)) {
              ext[0] = ext[0] || { reverse:false };
              ext[0].reverse = newReverse;
            }
          }
        }
        el.reversed = newReverse;
      }
    },
    () => {
      const a=getArr(); const el=a[idx];
      if(el && el.type==='joint'){
        el.delayIn=oldIn; el.delayOut=oldOut; el.egress=oldEgress;
        if(getNodeExtent && nodeIdx!==null){
          const ext = getNodeExtent();
          const node = ext && ext[nodeIdx];
          if(node && node.type==='node'){ node.evacpoint = oldNodeEvac; }
        }
        if (canToggleReverse) {
          for (const side of targetSides) {
            const ext = el[side];
            if (Array.isArray(ext)) {
              ext[0] = ext[0] || { reverse:false };
              const prev = oldReverseMap.has(side) ? oldReverseMap.get(side) : oldJointReversed;
              ext[0].reverse = prev;
            }
          }
        }
        el.reversed = oldJointReversed;
      }
    },
    'change joint properties'
  );
  updateHistButtons();

  clearSelection(); build(true); jointDlg.style.display='none';
}
document.getElementById('applyJoint').onclick = applyJoint;
jointDlg.addEventListener('keydown',e=>{
  if(e.key==='Enter'){ e.preventDefault(); applyJoint(); }
});

/* close joint dialog on outside click */
document.addEventListener('pointerdown', e => {
  if (jointDlg.style.display === 'none') return;
  if (jointDlg.contains(e.target))
    return;
  else
    jointDlg.style.display='none';
  clearSelection();
}, { capture: true });

/* ──────────────────────────────────────────────────────────────
   ‣ Marker dialog                                               */
const markDlg = document.createElement('div');
markDlg.style.cssText = `
  position:fixed; bottom:10px; right:10px; width:260px; background:#111;
  border:1px solid #666; border-radius:6px; padding:8px; color:#fff;
  font:0.8rem monospace; display:none;
`;
markDlg.innerHTML = `
  <h3 style="margin:0 0 6px;font-size:0.9rem;color:#0f0">Zone marker</h3>
  <label>Left zone
    <select id="leftZoneSel" style="width:100%;background:#222;border:1px solid #555;color:#fff;
      padding:2px 4px;font:0.8rem monospace"></select>
  </label>
  <label style="margin-top:6px">Right zone
    <select id="rightZoneSel" style="width:100%;background:#222;border:1px solid #555;color:#fff;
      padding:2px 4px;font:0.8rem monospace"></select>
  </label>
  <button id="applyMarker" style="margin-top:8px;width:100%;background:#0f0;border:none;
      color:#000;padding:4px 0;font:0.85rem monospace;border-radius:4px;cursor:pointer">
    Apply
  </button>
`;
document.body.appendChild(markDlg);

function populateMarkerDialog (extent = connexions, idx = 0) {
  const arr = extent;
  const m = arr[idx];
  if (!m || m.type !== 'marker') return;

  const selL = document.getElementById('leftZoneSel');
  const selR = document.getElementById('rightZoneSel');
  const opts = site.zones.map(z => `<option>${z.id}</option>`).join('');
  selL.innerHTML = `<option value="">none</option>` + opts;
  selR.innerHTML = `<option value="">none</option>` + opts;
  selL.value = m.leftZone  || '';
  selR.value = m.rightZone || '';

  markDlg.style.display = 'block';
  nodeDlg.style.display = siteDlg.style.display = 'none';
}

function applyMarker () {
  if (!sel.extent) return;
  const arr = sel.extent;
  const getArr = makeExtentGetter(arr);
  const idx = sel.localIdx;
  const m = arr[idx];
  if (!m || m.type !== 'marker') return;

  const oldL = m.leftZone;
  const oldR = m.rightZone;
  const newL = document.getElementById('leftZoneSel').value;
  const newR = document.getElementById('rightZoneSel').value;

  m.leftZone  = newL;
  m.rightZone = newR;
  pushHistory(
    () => {
      const a=getArr(); const el=a[idx];
      if(el && el.type==='marker'){ el.leftZone=newL; el.rightZone=newR; }
    },
    () => {
      const a=getArr(); const el=a[idx];
      if(el && el.type==='marker'){ el.leftZone=oldL; el.rightZone=oldR; }
    },
    'change marker zones'
  );
  updateHistButtons();

  clearSelection(); build(true);
}
document.getElementById('applyMarker').onclick = applyMarker;
markDlg.addEventListener('keydown', e=>{
  if (e.key === 'Enter'){ e.preventDefault(); applyMarker(); }
});

/* ──────────────────────────────────────────────────────────────
   ‣ Site dialog (zones)                                         */
const siteControlsBar = document.createElement('div');
siteControlsBar.style.cssText = `
  position:fixed; bottom:10px; left:10px;
  display:flex; gap:8px; align-items:flex-end;
  z-index:5;
`;
document.body.appendChild(siteControlsBar);

const siteBtn = makeButton(site.name, 0, null, 'left', siteControlsBar);
siteBtn.style.display = '';

const siteDlg = document.createElement('div');
siteDlg.style.cssText = `
  position:fixed; bottom:10px; left:10px; width:300px; background:#111;
  border:1px solid #666; border-radius:6px; padding:8px; color:#fff;
  font:0.8rem monospace; display:none; z-index:10;
`;
siteDlg.innerHTML = `
  <h3 style="margin:0 0 6px;font-size:0.9rem;color:#0f0">Site properties</h3>
  <label>Site name
    <input id="siteName" type="text"
      style="width:100%;background:#222;border:1px solid #555;color:#fff;
             padding:2px 4px;font:0.8rem monospace">
  </label>
  <div style="margin:8px 0 4px">Zones</div>
  <div id="zoneList" style="max-height:120px;overflow:auto;border:1px solid #444;
       padding:4px 2px"></div>
  <button id="addZoneRow" style="
    background:#222;color:#0f0;border:1px dashed #444;padding:2px 6px;
    font:0.8rem monospace;border-radius:4px;margin-top:4px;cursor:pointer">
    + add zone
  </button>
  <button id="applySite" style="
    margin-top:8px;width:100%;background:#0f0;border:none;color:#000;
    padding:4px 0;font:0.85rem monospace;border-radius:4px;cursor:pointer">
    Apply
  </button>
`;
document.body.appendChild(siteDlg);

function makeZoneRow(name){
  const row = document.createElement('div');
  row.style.cssText = 'display:flex;align-items:center;gap:4px;margin-bottom:2px';
  const input = document.createElement('input');
  input.type = 'text';
  input.value = name;
  input.style.cssText = 'flex:1 1 60%;background:#222;border:1px solid #555;color:#fff;'
    + 'padding:2px 4px;font:0.8rem monospace';
  const btn = document.createElement('button');
  btn.textContent = '×';
  btn.style.cssText = 'background:#800;border:none;color:#fff;cursor:pointer;width:20px';
  row.appendChild(input);
  row.appendChild(btn);
  return row;
}

function rebuildZoneRows () {
  const zoneList = document.getElementById('zoneList');
  zoneList.innerHTML = '';
  site.zones.forEach(z => {
    const row = makeZoneRow(z.id);
    zoneList.appendChild(row);
  });
}

siteBtn.onclick = () => {
  clearSelection();
  document.getElementById('siteName').value = site.name;
  rebuildZoneRows();
  siteDlg.style.display = 'block';
};

document.getElementById('addZoneRow').onclick = () => {
  const zoneList = document.getElementById('zoneList');
  const idx = zoneList.children ? zoneList.children.length + 1 : 1;
  zoneList.appendChild(makeZoneRow('zone'+idx));
};

document.getElementById('zoneList').onclick = e=>{
  if(e.target.tagName === 'BUTTON'){
    e.target.parentElement.remove();
  }
};

function applySiteState(name, zoneIds){
  site.name = name;
  siteBtn.textContent = site.name;
  site.zones = zoneIds.map(id => ({ id }));
}

function describeSiteChange(nameChanged, oldZones, newZones){
  const zonesChanged = oldZones.length !== newZones.length || oldZones.some((z,i)=>z!==newZones[i]);
  if(!nameChanged && zonesChanged){
    const added = newZones.filter(z => !oldZones.includes(z));
    const removed = oldZones.filter(z => !newZones.includes(z));
    if(newZones.length > oldZones.length && removed.length === 0){
      return added.length === 1 ? 'add zone' : 'add zones';
    }
    if(newZones.length < oldZones.length && added.length === 0){
      return removed.length === 1 ? 'remove zone' : 'remove zones';
    }
    if(newZones.length === oldZones.length && added.length === 1 && removed.length === 1){
      return 'rename zone';
    }
    return 'update site zones';
  }
  if(nameChanged && !zonesChanged) return 'change site name';
  if(nameChanged || zonesChanged) return 'update site settings';
  return 'update site settings';
}

function applySite(){
  const oldName = site.name;
  const oldZones = site.zones.map(z=>z.id);

  const newName = document.getElementById('siteName').value.trim() || 'site';
  const zoneListEl = document.getElementById('zoneList');
  const zoneInputs = [];
  if(zoneListEl){
    if(typeof zoneListEl.querySelectorAll === 'function'){
      zoneInputs.push(...zoneListEl.querySelectorAll('input[type="text"]'));
    } else if(Array.isArray(zoneListEl.children)){
      for(const row of zoneListEl.children){
        if(row && Array.isArray(row.children)){
          const input = row.children.find(child => child && typeof child.type === 'string' && child.type === 'text')
            || row.children.find(child => child && typeof child.tagName === 'string' && child.tagName.toLowerCase() === 'input');
          if(input) zoneInputs.push(input);
        }
      }
    }
  }
  const seenZones = new Set();
  const newZones = [];
  for(const input of zoneInputs){
    const name = input.value.trim();
    if(!name || seenZones.has(name)) continue;
    seenZones.add(name);
    newZones.push(name);
  }

  const nameChanged = oldName !== newName;
  const zonesChanged = oldZones.length !== newZones.length || oldZones.some((z,i)=>z!==newZones[i]);
  if(!nameChanged && !zonesChanged){
    siteDlg.style.display='none';
    return;
  }

  const label = describeSiteChange(nameChanged, oldZones, newZones);

  const applyNew = () => {
    applySiteState(newName, newZones);
    build(true);
  };

  const applyOld = () => {
    applySiteState(oldName, oldZones);
    build(true);
  };

  applyNew();
  pushHistory(applyNew, applyOld, label);
  updateHistButtons();

  siteDlg.style.display='none';
}
document.getElementById('applySite').onclick = applySite;
siteDlg.addEventListener('keydown',e=>{
  if(e.key==='Enter'){e.preventDefault();applySite();}
});

/* close site dialog on outside click */
document.addEventListener('pointerdown',e=>{
  if(!siteDlg.contains(e.target) && e.target!==siteBtn) siteDlg.style.display='none';
},{capture:true});

/* ──────────────────────────────────────────────────────────────
   ‣ Pointer picking (node / marker / break)                     */
renderer.domElement.addEventListener('pointerdown', e => {
  const info = pick(e);
  if (!info.hit) { clearSelection(); return; }

  /* ---------- NODE ---------- */
  if (info.kind === 'node') {
    if (sel.node === info.nodeIdx && sel.extent === info.extent) { clearSelection(); return; }
    clearSelection();
    sel.node     = info.nodeIdx;
    sel.extent   = info.extent;
    sel.localIdx = info.localIdx;
    highlightNode(info.nodeIdx, true);
    openNodeDialog(info.extent, info.localIdx);
    return;
  }

  /* ---------- JOINT ---------- */
  if (info.kind === 'joint') {
    if (sel.joint === info.nodeIdx && sel.extent === info.extent) { clearSelection(); return; }
    clearSelection();
    sel.joint    = info.nodeIdx;
    sel.extent   = info.extent;
    sel.localIdx = info.localIdx;
    highlightJoint(info.nodeIdx, true);
    openJointDialog(info.extent, info.localIdx);
    return;
  }

    /* ----- MARKER ----- */
    if (info.kind === 'marker') {
        if (sel.marker === info.markerIdx && sel.extent === info.extent) { clearSelection(); return; }

        clearSelection();
        sel.marker = info.markerIdx;
        sel.extent  = info.extent;
        sel.localIdx = info.localIdx; // index inside extent for deletion
        highlightMarker(info.markerIdx, true);

        populateMarkerDialog(info.extent, info.localIdx);
        return;
    }

  /* ---------- BREAK ---------- */
  if (info.kind === 'break') {
    if (sel.break === info.breakIdx) { clearSelection(); return; }
    clearSelection();
    sel.break = info.breakIdx;
    highlightBreak(info.breakIdx, true);
    addBtn.style.display = jointBtn.style.display = markerBtn.style.display = '';
  }
});

/* ──────────────────────────────────────────────────────────────
   ‣ Add-node / Add-marker at selected break                     */
function nodeIdxToIndexIn(arr, nodeIdx){
  let n=-1;
  for(let i=0;i<arr.length;i++)
    if((arr[i].type==='node' || arr[i].type==='joint') && ++n===nodeIdx) return i;
  return -1;
}

/* ---------- Add node at the selected break ---------- */
function addNodeAtBreak () {
  if (sel.break === null) return;

  const br = breakObjs[sel.break];
  const arr = br.userData.extent || connexions;
  const getArr = makeExtentGetter(arr);
  const leftNodeIdx = br.userData.leftIdx;
  const leftConnIdx = leftNodeIdx === -1 ? -1 : nodeIdxToIndexIn(arr, leftNodeIdx);

  const firstNode = arr.find(c => c.type === 'node' || c.type === 'joint');
  const baseDelay = leftConnIdx === -1
      ? getOutgoingDelay(firstNode)
      : getOutgoingDelay(arr[leftConnIdx]);

  /* create new node that keeps the SAME distance to its right */
  const newNode  = createNode(baseDelay);

  /* insert immediately after the left node, or at front */
  const insertAt = leftConnIdx === -1 ? 1 : leftConnIdx + 1;
  const a0 = getArr();
  a0.splice(insertAt, 0, newNode);
  if (a0 !== connexions) extentInserted(a0);
  pushHistory(
    () => { const a = getArr(); a.splice(insertAt, 0, newNode); if(a!==connexions) extentInserted(a); },
    () => { const a = getArr(); a.splice(insertAt, 1); if(a!==connexions) extentEmptied(a); },
    'add node'
  );
  updateHistButtons();

  clearSelection();
  build(true);                      // redraw without resetting camera
}

function addJointAtBreak () {
  if (sel.break === null) return;

  const br = breakObjs[sel.break];
  const arr = br.userData.extent || connexions;
  const getArr = makeExtentGetter(arr);
  const leftNodeIdx = br.userData.leftIdx;
  const leftConnIdx = leftNodeIdx === -1 ? -1 : nodeIdxToIndexIn(arr, leftNodeIdx);

  const firstNode = arr.find(c => c.type === 'node' || c.type === 'joint');
  const baseDelay = leftConnIdx === -1
      ? getOutgoingDelay(firstNode)
      : getOutgoingDelay(arr[leftConnIdx]);

  const joint = createJoint(baseDelay);
  const insertAt = leftConnIdx === -1 ? 1 : leftConnIdx + 1;
  const a0 = getArr();
  a0.splice(insertAt, 0, joint);
  if (a0 !== connexions) extentInserted(a0);
  pushHistory(
    () => { const a = getArr(); a.splice(insertAt, 0, joint); if(a!==connexions) extentInserted(a); },
    () => { const a = getArr(); a.splice(insertAt, 1); if(a!==connexions) extentEmptied(a); },
    'add joint'
  );
  updateHistButtons();

  clearSelection();
  build(true);
}

function addMarkerAtBreak(){
  if(sel.break===null) return;
  if(site.zones.length===0){ alert('Need at least one zone'); return; }

  const br = breakObjs[sel.break];
  const arr = br.userData.extent || connexions;
  const getArr = makeExtentGetter(arr);
  const leftNodeIdx = br.userData.leftIdx;
  const leftConnIdx = nodeIdxToIndexIn(arr, leftNodeIdx);

  /* allow marker before the first node */
  if(leftConnIdx === -1){
    const m = createZoneMarker('', '');
    const a0 = getArr();
    a0.splice(1,0,m);
    pushHistory(
      () => { const a = getArr(); a.splice(1,0,m); if(a!==connexions) extentInserted(a); },
      () => { const a = getArr(); a.splice(1,1); if(a!==connexions) extentEmptied(a); },
      'add marker'
    );
    updateHistButtons();
    clearSelection(); build(true); return;
  }

  const m = createZoneMarker('', '');
  const a0 = getArr();
  a0.splice(leftConnIdx+1,0,m);
  if (a0 !== connexions) extentInserted(a0);
  pushHistory(
    () => { const a = getArr(); a.splice(leftConnIdx+1,0,m); if(a!==connexions) extentInserted(a); },
    () => { const a = getArr(); a.splice(leftConnIdx+1,1); if(a!==connexions) extentEmptied(a); },
    'add marker'
  );
  updateHistButtons();

  clearSelection(); build(true);
}

/* ──────────────────────────────────────────────────────────────
   ‣ Keyboard shortcuts                                          */
window.addEventListener('keydown',e=>{
  /* Ctrl+Z – Undo */
  if(e.ctrlKey && !e.shiftKey && e.key.toLowerCase()==='z'){
    undoHist();
    clearSelection();
    build(true);
    updateHistButtons();
    e.preventDefault();
    return;
  }

  /* Ctrl+Y – Redo */
  if(e.ctrlKey && !e.shiftKey && e.key.toLowerCase()==='y'){
    redoHist();
    clearSelection();
    build(true);
    updateHistButtons();
    e.preventDefault();
    return;
  }
  /* Enter   – Add node / marker when break selected */
  if(e.key==='Enter' && sel.break!==null && nodeDlg.style.display==='none' && markDlg.style.display==='none'){
    if(e.shiftKey) addMarkerAtBreak(); else addNodeAtBreak();
  }

  /* Backspace/Delete – delete node or marker */
  if((e.key==='Backspace' || e.key==='Delete') && document.activeElement.tagName!=='INPUT'){
    if(sel.node!==null || sel.joint!==null){
      if(sel.extent){
        const arr = sel.extent;
        const getArr = makeExtentGetter(arr);
        const idx = sel.localIdx;
        const el = arr[idx];
        const removeLabel = el && el.type === 'joint' ? 'remove joint' : 'remove node';
        let prevIdx=null, prevDelay=null;
        for(let i=idx-1;i>=0;i--){
          if(arr[i].type==='node'){ prevIdx=i; prevDelay=arr[i].delayRight; break; }
          if(arr[i].type==='joint'){ prevIdx=i; prevDelay=arr[i].delayOut; break; }
        }
  const wasEmpty = arr.length===2;
        const a0 = getArr();
        deleteNodeIn(a0, idx);
        pushHistory(
          () => { const a = getArr(); deleteNodeIn(a, idx); },
          () => {
            const a = getArr();
            a.splice(idx,0,el);
            if(prevIdx!==null){
              if(a[prevIdx].type==='node') a[prevIdx].delayRight = prevDelay;
              else if(a[prevIdx].type==='joint') a[prevIdx].delayOut = prevDelay;
            }
            if(a!==connexions) (wasEmpty ? extentInserted(a) : 0);
          },
          removeLabel
        );
        updateHistButtons();
      }
      clearSelection(); build(true); e.preventDefault();
    }
    else if(sel.marker!==null){
      if(sel.extent){
        const arr = sel.extent;
        const getArr = makeExtentGetter(arr);
        const idx = sel.localIdx;
        const el = arr[idx];
        const wasEmpty = arr.length===2;
        const a0 = getArr();
        deleteMarkerIn(a0, idx);
        pushHistory(
          () => { const a = getArr(); deleteMarkerIn(a, idx); },
          () => { const a = getArr(); a.splice(idx,0,el); if(a!==connexions && wasEmpty) extentInserted(a); },
          'remove marker'
        );
        updateHistButtons();
      }
      clearSelection(); build(true); e.preventDefault();
    }
  }

  /* Shift + ↑ / ↓ – walk camera */
  if(e.shiftKey && !e.ctrlKey && !e.altKey && !e.metaKey){
    const step = 4;
    if(e.code==='ArrowUp'  ){ camera.position.z-=step; controls.target.z-=step; e.preventDefault(); }
    if(e.code==='ArrowDown'){ camera.position.z+=step; controls.target.z+=step; e.preventDefault(); }
  }
});

/* ──────────────────────────────────────────────────────────────
   ‣ Reset view button                                           */
function resetCam(){
  clearSelection();
  camera.position.set(20,10,0);
  controls.target.set(0,0,0);
  controls.update();
}
