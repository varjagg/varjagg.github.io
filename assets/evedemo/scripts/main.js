const splash = document.getElementById('splash');

splash.addEventListener('click', async () => {
  splash.remove();

  await import('./model.js');
  await import('./render.js');
  await import('./controls.js');
});
