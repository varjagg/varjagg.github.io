let overlay = null;
let lockCount = 0;
const passThroughElements = new Set();
let ensurePending = false;

function ensureOverlay() {
  if (overlay || typeof document === 'undefined') return;
  if (!document.body) {
    if (!ensurePending && typeof document.addEventListener === 'function') {
      ensurePending = true;
      document.addEventListener('DOMContentLoaded', () => {
        ensurePending = false;
        ensureOverlay();
      }, { once: true });
    }
    return;
  }

  overlay = document.createElement('div');
  overlay.id = 'ws-overlay';
  overlay.style.cssText = `
    position:fixed;
    inset:0;
    width:100%;
    height:100%;
    background:rgba(0,0,0,0.45);
    display:none;
    pointer-events:none;
    z-index:1500;
    opacity:0;
    transition:opacity 0.2s ease;
  `;

  const message = document.createElement('div');
  message.textContent = 'Communicating with Evacsound central…';
  message.style.cssText = `
    position:absolute;
    top:50%;
    left:50%;
    transform:translate(-50%, -50%);
    color:#fff;
    font:1.1rem monospace;
    background:rgba(0,0,0,0.6);
    padding:12px 18px;
    border-radius:6px;
    pointer-events:none;
  `;
  overlay.appendChild(message);
  document.body.appendChild(overlay);
}

function isPassThrough(target) {
  if (!target) return false;
  if (passThroughElements.has(target)) return true;
  let node = target;
  while (node && node !== document.body) {
    if (passThroughElements.has(node)) return true;
    node = node.parentElement || node.parentNode;
  }
  return false;
}

function blockIfLocked(e) {
  if (!lockCount) return;
  if (isPassThrough(e.target)) return;
  if (typeof e.preventDefault === 'function') e.preventDefault();
  if (typeof e.stopPropagation === 'function') e.stopPropagation();
}

if (typeof document !== 'undefined') {
  document.addEventListener('pointerdown', blockIfLocked, true);
  document.addEventListener('pointerup', blockIfLocked, true);
  document.addEventListener('click', blockIfLocked, true);
  document.addEventListener('contextmenu', blockIfLocked, true);
  document.addEventListener('wheel', blockIfLocked, { capture: true, passive: false });
}

if (typeof window !== 'undefined') {
  window.addEventListener('keydown', e => {
    if (!lockCount) return;
    if (isPassThrough(e.target)) return;
    if (typeof e.preventDefault === 'function') e.preventDefault();
    if (typeof e.stopPropagation === 'function') e.stopPropagation();
  }, true);
}

function showOverlay() {
  ensureOverlay();
  if (!overlay) return;
  overlay.style.display = 'block';
  const activate = () => { if (overlay) overlay.style.opacity = '1'; };
  if (typeof requestAnimationFrame === 'function') requestAnimationFrame(activate);
  else activate();
}

function hideOverlay() {
  if (!overlay) return;
  const deactivate = () => {
    if (!overlay) return;
    if (lockCount === 0) {
      overlay.style.opacity = '0';
      const finalize = () => {
        if (!overlay || lockCount !== 0) return;
        overlay.style.display = 'none';
      };
      if (typeof requestAnimationFrame === 'function') requestAnimationFrame(finalize);
      else finalize();
    }
  };
  if (typeof requestAnimationFrame === 'function') requestAnimationFrame(deactivate);
  else deactivate();
}

export function registerPassThroughElement(el) {
  if (el) passThroughElements.add(el);
}

export function lockUi() {
  lockCount++;
  ensureOverlay();
  if (lockCount === 1) {
    const active = typeof document !== 'undefined' ? document.activeElement : null;
    if (active && typeof active.blur === 'function') active.blur();
    document.body?.classList?.add('ws-busy');
    showOverlay();
  }
}

export function unlockUi() {
  if (lockCount > 0) lockCount--;
  if (lockCount === 0) {
    document.body?.classList?.remove('ws-busy');
    hideOverlay();
  }
}

export function isUiLocked() {
  return lockCount > 0;
}

ensureOverlay();
