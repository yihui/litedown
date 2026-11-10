// link function calls like `fn(` or `pkg::fn(` in <code> to their help pages, using
// the map (function -> URL) at the URL in <meta name="function-index" content="...">
(d => {
  const url = d.querySelector('meta[name=function-index]')?.content;
  if (!url) return;
  const base = new URL(url, location.href), here = location.origin + location.pathname;
  // optional pkg:: prefix, then the name, which must be followed by `(`
  const re = /(?<![\w.$@])(?:[A-Za-z][\w.]*::)?([A-Za-z.][\w.]*)(?=\()/g;

  const link = (code, map) => {
    // in <pre>, only R code (prism.js marks it with language-r or span.language-r)
    if (code.closest('pre') && !code.classList.contains('language-r') &&
        !code.querySelector('span.language-r')) return;
    if (code.closest('a')) return;
    // collect non-empty text nodes with their offsets in the whole text; elements
    // (e.g., highlighting spans) are left intact, only text is wrapped
    const nodes = [], w = d.createTreeWalker(code, NodeFilter.SHOW_TEXT);
    let text = '', n;
    while (n = w.nextNode()) if (n.length) nodes.push([n, text.length]), text += n.data;
    const hits = [...text.matchAll(re)].filter(m => Object.hasOwn(map, m[1]));
    // go backwards so that splitting a node doesn't shift earlier offsets
    hits.reverse().forEach(m => {
      const u = new URL(map[m[1]], base), s = m.index, e = s + m[0].length;
      // no self-links to the page being read (but keep links to its sections)
      if (u.origin + u.pathname === here && !u.hash) return;
      // a name may span several text nodes (e.g., `pkg::` and `fn` highlighted
      // separately), so wrap the part in each node with a link to the same URL
      nodes.filter(([t, o]) => o < e && o + t.length > s).reverse().forEach(([t, o]) => {
        if (t.parentElement.closest('a')) return;
        const a = d.createElement('a'), r = d.createRange();
        a.href = u;
        r.setStart(t, Math.max(s, o) - o); r.setEnd(t, Math.min(e, o + t.length) - o);
        r.surroundContents(a);
      });
    });
  };

  fetch(base).then(r => r.json()).then(map => {
    d.querySelectorAll('code').forEach(code => link(code, map));
    // syntax highlighters may rewrite a code block after this point (and drop the
    // links), so link again when its content changes; this is idempotent, i.e., it
    // stops once all names are linked
    const todo = new Set();
    new MutationObserver(ms => {
      ms.forEach(m => {
        const el = m.target.nodeType === 1 ? m.target : m.target.parentElement;
        const code = el?.closest('code');
        if (code) todo.add(code);
      });
      if (!todo.size) return;
      // wait until the highlighter is done with the block
      setTimeout(() => { const s = [...todo]; todo.clear(); s.forEach(c => link(c, map)); }, 100);
    }).observe(d.body, {childList: true, subtree: true});
  }).catch(() => {});
})(document);
