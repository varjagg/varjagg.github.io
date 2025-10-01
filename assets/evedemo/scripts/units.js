import { SOUND_SPEED, DEFAULT_DISTANCE_METERS, DEFAULT_DELAY_NS } from './constants.js';

export { DEFAULT_DISTANCE_METERS, DEFAULT_DELAY_NS } from './constants.js';

export function metersToDelay(meters, fallback = DEFAULT_DELAY_NS) {
  if (meters === undefined || meters === null) return fallback;
  const value = Number(meters);
  if (!Number.isFinite(value)) return fallback;
  return Math.round(value / SOUND_SPEED * 1e9);
}

export function delayToMeters(delay, fallback = DEFAULT_DISTANCE_METERS) {
  if (delay === undefined || delay === null) return fallback;
  const value = Number(delay);
  if (!Number.isFinite(value)) return fallback;
  return value * SOUND_SPEED / 1e9;
}
