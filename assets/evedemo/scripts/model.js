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
    { id:'Main' },
    { id:'Evacuation' },
    { id:'Utility' }
  ],

  /* main extent lives in the leftExtent of this top-level joint */
  joint : {
    type:'joint',
    delayOut:0,
    delayIn:0,
    attached:true,
    egress:false,
    directionRight:false,
    reversed:false,
    leftExtent:[
      { reverse:false },
      { type:'marker', leftZone:'', rightZone:'Main' },
      { type:'node', id:'10:45:be:00:01:00', capabilities:CAP_MICROPHONE, delayRight:88235294, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:01', capabilities:CAP_SPEAKER_LEFT, delayRight:29411765, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:02', capabilities:CAP_SPEAKER_RIGHT, delayRight:79411765, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:03', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:76470588, evacpoint:false },
      {
        type:'joint',
        delayOut:79411765,
        delayIn:79411765,
        attached:false,
        egress:true,
        directionRight:false,
        reversed:false,
        leftExtent:[
          { reverse:false },
          { type:'node', id:'10:45:be:00:01:10', capabilities:CAP_SPEAKER_RIGHT, delayRight:58823529, evacpoint:true },
          { type:'node', id:'10:45:be:00:01:11', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:55882353, evacpoint:false },
          {
            type:'joint',
            delayOut:70588235,
            delayIn:64705882,
            attached:false,
            egress:false,
            directionRight:false,
            reversed:false,
            leftExtent:[
              { reverse:false },
              { type:'node', id:'10:45:be:00:01:07', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:52941176, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:08', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:58823529, evacpoint:false },
              {
                type:'joint',
                delayOut:55882353,
                delayIn:55882353,
                attached:false,
                egress:false,
                directionRight:false,
                reversed:false,
                leftExtent:[
                  { reverse:false },
                  { type:'node', id:'10:45:be:00:01:04', capabilities:CAP_MICROPHONE, delayRight:50000000, evacpoint:false },
                  { type:'marker', leftZone:'Main', rightZone:'' }
                ],
                rightExtent:[ { reverse:true } ]
              },
              { type:'node', id:'10:45:be:00:01:09', capabilities:CAP_MICROPHONE, delayRight:61764706, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:0a', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:54411765, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:0b', capabilities:0, delayRight:null, evacpoint:false },
              { type:'marker', leftZone:'Main', rightZone:'' }
            ],
            rightExtent:[ { reverse:false } ]
          },
          { type:'node', id:'10:45:be:00:01:12', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:67647059, evacpoint:false },
          {
            type:'joint',
            delayOut:61764706,
            delayIn:61764706,
            attached:false,
            egress:false,
            directionRight:true,
            reversed:false,
            leftExtent:[ { reverse:false } ],
            rightExtent:[
              { reverse:true },
              { type:'node', id:'10:45:be:00:01:0c', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:70588235, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:0d', capabilities:CAP_MICROPHONE, delayRight:67647059, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:0e', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:64705882, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:0f', capabilities:0, delayRight:null, evacpoint:false },
              { type:'marker', leftZone:'Main', rightZone:'' }
            ]
          },
          { type:'node', id:'10:45:be:00:01:13', capabilities:CAP_MICROPHONE, delayRight:61764706, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:14', capabilities:0, delayRight:null, evacpoint:false },
          { type:'marker', leftZone:'Main', rightZone:'' }
        ],
        rightExtent:[ { reverse:false } ]
      },
      { type:'node', id:'10:45:be:00:01:15', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:73529412, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:16', capabilities:0, delayRight:88235294, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:17', capabilities:CAP_MICROPHONE, delayRight:70588235, evacpoint:false },
      {
        type:'joint',
        delayOut:76470588,
        delayIn:70588235,
        attached:false,
        egress:false,
        directionRight:true,
        reversed:false,
        leftExtent:[ { reverse:false } ],
        rightExtent:[
          { reverse:false },
          { type:'node', id:'10:45:be:00:01:1b', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:64705882, evacpoint:false },
          {
            type:'joint',
            delayOut:58823529,
            delayIn:52941176,
            attached:false,
            egress:false,
            directionRight:false,
            reversed:false,
            leftExtent:[
              { reverse:false },
              { type:'node', id:'10:45:be:00:01:18', capabilities:0, delayRight:29411765, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:19', capabilities:CAP_MICROPHONE, delayRight:55882353, evacpoint:false },
              { type:'node', id:'10:45:be:00:01:1a', capabilities:0, delayRight:null, evacpoint:false },
              { type:'marker', leftZone:'Main', rightZone:'' }
            ],
            rightExtent:[ { reverse:false } ]
          },
          { type:'node', id:'10:45:be:00:01:1c', capabilities:CAP_MICROPHONE, delayRight:61764706, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:1d', capabilities:CAP_SPEAKER_LEFT, delayRight:70588235, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:1e', capabilities:CAP_SPEAKER_RIGHT, delayRight:67647059, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:1f', capabilities:0, delayRight:null, evacpoint:false },
          { type:'marker', leftZone:'Main', rightZone:'' }
        ]
      },
      { type:'node', id:'10:45:be:00:01:20', capabilities:0, delayRight:29411765, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:21', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:67647059, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:22', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:64705882, evacpoint:false },
      {
        type:'joint',
        delayOut:67647059,
        delayIn:67647059,
        attached:false,
        egress:false,
        directionRight:false,
        reversed:false,
        leftExtent:[
          { reverse:true },
          { type:'node', id:'10:45:be:00:01:23', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:67647059, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:24', capabilities:CAP_MICROPHONE, delayRight:61764706, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:25', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:58823529, evacpoint:false },
          { type:'node', id:'10:45:be:00:01:26', capabilities:0, delayRight:null, evacpoint:false },
          { type:'marker', leftZone:'Main', rightZone:'' }
        ],
        rightExtent:[ { reverse:false } ]
      },
      { type:'node', id:'10:45:be:00:01:27', capabilities:0, delayRight:88235294, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:28', capabilities:CAP_SPEAKER_LEFT, delayRight:61764706, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:29', capabilities:0, delayRight:29411765, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:2a', capabilities:CAP_TEMPERATURE_SENSOR_MAST_TOP, delayRight:70588235, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:2b', capabilities:CAP_MICROPHONE, delayRight:67647059, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:2c', capabilities:0, delayRight:88235294, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:2d', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:64705882, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:2e', capabilities:0, delayRight:29411765, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:2f', capabilities:CAP_DICHROMATIC_LIGHTS, delayRight:61764706, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:30', capabilities:CAP_MICROPHONE, delayRight:88235294, evacpoint:false },
      { type:'node', id:'10:45:be:00:01:31', capabilities:0, delayRight:null, evacpoint:false },
      { type:'marker', leftZone:'Main', rightZone:'' }
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
