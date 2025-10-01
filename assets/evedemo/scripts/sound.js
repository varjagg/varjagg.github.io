let transitionAudio = null;

export function playTransitionSound() {
  if (typeof Audio === 'undefined') return; // no audio support
  if (!transitionAudio) {
    transitionAudio = new Audio('transition.mp3');
    transitionAudio.preload = 'auto';
  }
  try {
    transitionAudio.currentTime = 0;
  } catch {}
  const p = transitionAudio.play();
  if (p && typeof p.catch === 'function') {
    p.catch(() => {
      const resume = () => {
        transitionAudio.play();
        document.removeEventListener('click', resume);
      };
      document.addEventListener('click', resume, { once: true });
    });
  }
}
