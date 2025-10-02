const AUDIO_PATH = 'transition.mp3';

let transitionAudio = null;
let transitionReady = null;
let transitionReadyResolved = false;

function ensureTransitionAudio(){
  if (typeof Audio === 'undefined') return null;
  if (!transitionAudio) {
    transitionAudio = new Audio(AUDIO_PATH);
    transitionAudio.preload = 'auto';
    transitionReady = new Promise(resolve => {
      const settle = () => {
        transitionAudio.removeEventListener('canplaythrough', settle);
        transitionAudio.removeEventListener('loadeddata', settle);
        transitionAudio.removeEventListener('error', settle);
        transitionReadyResolved = true;
        resolve();
      };
      transitionAudio.addEventListener('canplaythrough', settle, { once:true });
      transitionAudio.addEventListener('loadeddata', settle, { once:true });
      transitionAudio.addEventListener('error', settle, { once:true });
      try { transitionAudio.load(); }
      catch { settle(); }
    });
  }
  return transitionAudio;
}

export function warmTransitionSound(){
  ensureTransitionAudio();
  return transitionReady ?? Promise.resolve();
}

function startPlayback(audio){
  try { audio.currentTime = 0; }
  catch {}
  const playPromise = audio.play();
  if (playPromise && typeof playPromise.catch === 'function') {
    playPromise.catch(() => {
      const resume = () => {
        audio.play();
        if (typeof document?.removeEventListener === 'function') {
          document.removeEventListener('click', resume);
        }
      };
      if (typeof document?.addEventListener === 'function') {
        document.addEventListener('click', resume, { once:true });
      }
    });
  }
}

export function playTransitionSound() {
  const audio = ensureTransitionAudio();
  if (!audio) return;
  if (transitionReadyResolved || audio.readyState >= 2) {
    startPlayback(audio);
    return;
  }
  const ready = transitionReady ?? Promise.resolve();
  ready.then(() => startPlayback(audio));
}

warmTransitionSound().catch(() => {});
