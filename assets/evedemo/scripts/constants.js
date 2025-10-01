export const SOUND_SPEED = 340; // meters per second

export const DEFAULT_DISTANCE_METERS = 25;
export const DEFAULT_DELAY_NS = Math.round(DEFAULT_DISTANCE_METERS / SOUND_SPEED * 1e9);

export const CAP_MICROPHONE = 1 << 0;
export const CAP_SPEAKER_LEFT = 1 << 1;
export const CAP_SPEAKER_RIGHT = 1 << 2;
export const CAP_DICHROMATIC_LIGHTS = 1 << 3;
export const CAP_TEMPERATURE_SENSOR_MAST_TOP = 1 << 4;

export const CAP_POOL = [
  { label: 'microphone', bit: CAP_MICROPHONE },
  { label: 'speaker left', bit: CAP_SPEAKER_LEFT },
  { label: 'speaker right', bit: CAP_SPEAKER_RIGHT },
  { label: 'dichromatic lights', bit: CAP_DICHROMATIC_LIGHTS },
  { label: 'temperature sensor', bit: CAP_TEMPERATURE_SENSOR_MAST_TOP }
];

export const cfg = {
  SEG: 3,                     // tunnel-slice depth  (m)
  JSEG: 9.5,                  // joint slice depth (m)
  BASE_GAP: 2.5, GAP_FACTOR: 0.1, GAP_MIN: 0.5, GAP_MAX: 4,
  R: 9.5 / 2, H: 3,
  PYR_L: 0.6, PYR_R: 0.3,
  COL_I: 0xffff00, COL_S: 0xff0000,
  OFF_MAC: { x: -1.2, y: 0.9, z: 0 },
  OFF_DIS: { x: -1, y: 0.6, z: 0 },
  ZONE_LBL_SEP: 2               // distance of zone labels from marker plane
};
