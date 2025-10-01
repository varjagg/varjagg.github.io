/* model.js  –  unified “connexions” data layer  (Option A)  */
import { delayToMeters } from './units.js';
import {
  DEFAULT_DELAY_NS,
  DEFAULT_DISTANCE_METERS,
  cfg,
  CAP_MICROPHONE,
  CAP_SPEAKER_LEFT,
  CAP_SPEAKER_RIGHT,
  CAP_DICHROMATIC_LIGHTS,
  CAP_TEMPERATURE_SENSOR_MAST_TOP,
  CAP_POOL
} from './constants.js';

/* ── visual / tunnel constants ─────────────────────────── */
export const gap = d => Math.max(
  cfg.GAP_MIN, Math.min(cfg.GAP_MAX, cfg.BASE_GAP + (d - 25) * cfg.GAP_FACTOR)
);

/* ── top-level site object ─────────────────────────────── */
export const site = {
  name  : 'site',
  deferInitialBuild: false,

  /* zones can be referenced by zone-markers */
  zones : [
    { id:'Zone-A' },
    { id:'Zone-B' }
  ],

  /* main extent lives in the leftExtent of this top-level joint */
  joint : {
    type:'joint',
    delayOut:0, delayIn:0,
    attached:true,
    egress:false,
    directionRight:false,
    reversed:false,
    leftExtent:[
      { reverse:false },
      /* node 0 */   { type:'node',
                      id:'10:45:be:00:01:00', capabilities:CAP_MICROPHONE, delayRight:DEFAULT_DELAY_NS },

      /* marker */   { type:'marker',
                      leftZone:'Zone-A', rightZone:'Zone-B' },

      /* node 1 */   { type:'node',
                      id:'10:45:be:00:01:01', capabilities:CAP_SPEAKER_LEFT, delayRight:DEFAULT_DELAY_NS },

      /* joint */   { type:'joint',
                      delayOut:DEFAULT_DELAY_NS, delayIn:DEFAULT_DELAY_NS, attached:false, egress:false,
                      directionRight:false,
                      reversed:false,
                      leftExtent:[{ reverse:false }], rightExtent:[{ reverse:false }] },

      /* node 3 */   { type:'node',
                      id:'10:45:be:00:01:03', capabilities:0, delayRight:null }   // last node
    ],
    rightExtent:null
  },

  rootExtent() { return this.joint.leftExtent; }
};

/* shorthand alias so legacy loops can still say `connexions` */
export const connexions = site.joint.leftExtent;

/* ── factories & validators ────────────────────────────── */
/* sequential MAC generator */
function collectIds (arr, out) {
  arr.forEach(el => {
    if (el.type === 'node') {
      out.push(el.id);
    }
    if (el.type === 'joint') {
      if (el.leftExtent) collectIds(el.leftExtent, out);
      if (el.rightExtent)  collectIds(el.rightExtent, out);
    }
  });
}

function nextMac () {
  const ids = [];
  collectIds(connexions, ids);
  const nums = ids
    .map(id => parseInt(id.split(':').slice(3).join(''), 16))
    .filter(n => !isNaN(n));
  const next = ((Math.max(...nums, 0) + 1) & 0xFFFFFF)
                .toString(16).padStart(6, '0');
  return next.match(/.{2}/g).join(':');
}

/* create but do NOT insert – caller decides where */
export function createNode(delayRight = DEFAULT_DELAY_NS) {
  return {
    type:'node',
    id  : '10:45:be:' + nextMac(),
    capabilities:0,
    delayRight,
    evacpoint:false
  };
}

export function createJoint(delayOut = DEFAULT_DELAY_NS, delayIn = DEFAULT_DELAY_NS) {
  return {
    type:'joint',
    delayOut,
    delayIn,
    attached:false,
    egress:false,
    directionRight:false,
    reversed:false,
    leftExtent:[{ reverse:false }],
    rightExtent:[{ reverse:false }]
  };
}

/* ── helper: find parent joint for a side extent ───────────── */
export function findJointForExtent(ext, list = connexions) {
  for (const el of list) {
    if (el.type === 'joint') {
      if (el.leftExtent === ext || el.rightExtent === ext) return el;
      if (el.leftExtent) {
        const j = findJointForExtent(ext, el.leftExtent);
        if (j) return j;
      }
      if (el.rightExtent) {
        const j = findJointForExtent(ext, el.rightExtent);
        if (j) return j;
      }
    }
  }
  return null;
}

/* ── side-extent management helpers ────────────────────────── */
export function removeOtherExtent(joint, ext) {
  if (!joint) return;
  if (joint.leftExtent === ext) {
    joint.rightExtent = null;
    joint.directionRight = false;
  } else if (joint.rightExtent === ext) {
    joint.leftExtent = null;
    joint.directionRight = true;
  }
}

export function extentInserted(ext) {
  if (ext === connexions) return;
  if (ext.length > 1) {
    const j = findJointForExtent(ext);
    removeOtherExtent(j, ext);
    if (j) j.directionRight = j.rightExtent === ext;
  }
}

export function extentEmptied(ext) {
  if (ext === connexions) return;

  const hasConnexions = ext.some(el => el && (el.type === 'node' || el.type === 'joint'));
  if (hasConnexions) return;

  const j = findJointForExtent(ext);
  if (!j) return;

  const side = j.leftExtent === ext ? 'leftExtent' : 'rightExtent';
  const otherSide = side === 'leftExtent' ? 'rightExtent' : 'leftExtent';

  // removing the final connexion from a side extent means the joint no longer
  // serves as an egress point
  j.egress = false;

  const ensureStub = arr => {
    const rev = arr && arr[0] ? !!arr[0].reverse : false;
    return [{ reverse: rev }];
  };

  const rev = ext[0]?.reverse ?? false;
  if (ext.length === 0) ext.push({ reverse: rev });
  else ext[0] = { reverse: rev };

  j[side] = ext;
  j[otherSide] = ensureStub(j[otherSide]);
  j.directionRight = side === 'rightExtent' && ext.length > 1;
}

export function createZoneMarker(leftZone = '', rightZone = '') {
  return { type:'marker', leftZone, rightZone };
}

export function createZone(id) {
  if (site.zones.some(z => z.id === id)) return null;
  const z = { id };
  site.zones.push(z);
  return z;
}

export {
  cfg,
  CAP_MICROPHONE,
  CAP_SPEAKER_LEFT,
  CAP_SPEAKER_RIGHT,
  CAP_DICHROMATIC_LIGHTS,
  CAP_TEMPERATURE_SENSOR_MAST_TOP,
  CAP_POOL
} from './constants.js';
export const isValidZone = id => site.zones.some(z => z.id === id);

/* ── generic deletion helpers (by index inside given extent) ─ */
export function deleteNodeIn(arr, idx) {
  const el = arr[idx];
  if (!el || (el.type !== 'node' && el.type !== 'joint')) return;

  const delay = el.type === 'joint' ? el.delayOut : el.delayRight;
  for (let i = idx - 1; i >= 0; i--) {
    if (arr[i].type === 'node') { arr[i].delayRight = delay; break; }
    if (arr[i].type === 'joint') { arr[i].delayOut = delay; break; }
  }
  arr.splice(idx, 1);
  extentEmptied(arr);
}

export function deleteMarkerIn(arr, idx) {
  const el = arr[idx];
  if (!el || el.type !== 'marker') return;
  arr.splice(idx, 1);
  extentEmptied(arr);
}

/* wrappers for main extent (backwards compatibility) */
export function deleteNode(connIdx) {
  deleteNodeIn(connexions, connIdx);
  selRef.current = null;
}

export function deleteMarker(connIdx) {
  deleteMarkerIn(connexions, connIdx);
  selRef.current = null;
}

/* selection pointer exported for renderer */
export const selRef = { current: null };

/* populate / update the floating dialog */
export function rebuildDialog() {
  const dlg = document.getElementById('dlg');
  if (selRef.current === null) { dlg.style.display = 'none'; return; }

  const n = connexions[selRef.current];
  dlg.style.display = 'block';
  document.getElementById('mac').value  = n.id;
  const metres = delayToMeters(n.delayRight, DEFAULT_DISTANCE_METERS);
  document.getElementById('dist').value = Math.round(metres * 100) / 100;

  const capsDiv = document.getElementById('caps');
  capsDiv.innerHTML = '';
  const capMask = typeof n.capabilities === 'number' ? n.capabilities >>> 0 : 0;
  if (typeof n.capabilities !== 'number') n.capabilities = capMask;
  CAP_POOL.forEach(({ label, bit }) => {
    const cb = document.createElement('input');
    cb.type = 'checkbox';
    cb.checked = (capMask & bit) !== 0;
    cb.onchange = e => {
      const current = typeof n.capabilities === 'number' ? n.capabilities >>> 0 : 0;
      if (e.target.checked) n.capabilities = (current | bit) >>> 0;
      else n.capabilities = (current & ~bit) >>> 0;
    };
    const lab = document.createElement('label');
    lab.append(cb, document.createTextNode(label));
    capsDiv.append(lab);
  });
}
