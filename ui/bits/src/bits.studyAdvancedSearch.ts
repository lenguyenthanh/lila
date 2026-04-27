site.load.then(() => {
  const form = document.querySelector<HTMLFormElement>('form.search');
  if (!form) return;

  const panel = form.querySelector<HTMLElement>('.search__advanced');
  const toggle = form.querySelector<HTMLButtonElement>('.search__advanced-toggle');
  const modeRadios = form.querySelectorAll<HTMLInputElement>('input[name="mode"]');

  // If the page arrived with any advanced field already populated (shared
  // link, page reload), keep the panel open. Otherwise collapse it.
  const advancedNames = [
    'owner',
    'member',
    'mode',
    'variant',
    'eco',
    'opening',
    'player1',
    'player2',
    'fideId1',
    'fideId2',
    'event',
  ];
  const hasAdvancedValue = advancedNames.some(n =>
    Array.from(form.querySelectorAll<HTMLInputElement | HTMLSelectElement>(`[name="${n}"]`)).some(el => {
      if (el instanceof HTMLInputElement && el.type === 'radio') return el.checked && !!el.value;
      return !!el.value;
    }),
  );

  if (panel && !hasAdvancedValue) panel.classList.add('is-collapsed');
  toggle?.setAttribute('aria-expanded', String(!panel?.classList.contains('is-collapsed')));

  toggle?.addEventListener('click', () => {
    if (!panel) return;
    const willOpen = panel.classList.contains('is-collapsed');
    panel.classList.toggle('is-collapsed', !willOpen);
    toggle.setAttribute('aria-expanded', String(willOpen));
  });

  // Keep the active-mode pill highlighted, the per-mode hint visible,
  // and the chapter-filters table shown only in the Filters mode.
  const syncMode = () => {
    if (!panel) return;
    const mode = Array.from(modeRadios).find(r => r.checked)?.value || '';
    panel.querySelectorAll<HTMLLabelElement>('.search__advanced-mode').forEach(lbl => {
      const v = lbl.querySelector<HTMLInputElement>('input')?.value || '';
      lbl.classList.toggle('is-active', v === mode);
    });
    panel.querySelectorAll<HTMLElement>('[data-mode]').forEach(el => {
      const want = el.getAttribute('data-mode') || '';
      el.classList.toggle('is-hidden', want !== mode);
    });
    panel.querySelector<HTMLTableElement>('.filters')?.classList.toggle('is-hidden', mode !== 'chapterFilters');
  };
  modeRadios.forEach(r => r.addEventListener('change', syncMode));
  syncMode();

  // Strip empty fields on submit so URLs stay short / shareable.
  form.addEventListener('submit', () => {
    form.querySelectorAll<HTMLInputElement | HTMLSelectElement>('input,select').forEach(el => {
      if (el instanceof HTMLInputElement && el.type === 'submit') return;
      if (el instanceof HTMLInputElement && el.type === 'radio' && !el.checked) {
        el.disabled = true;
        return;
      }
      if (!el.value) el.disabled = true;
    });
  });
});
