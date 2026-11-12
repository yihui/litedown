// link function calls like `fn(` or `pkg::fn(` in <code> to their help pages, using
// the package name and the map (function -> URL) at the URL in
// <meta name="function-index" content="...">
(d => {
  const url = d.querySelector('meta[name=function-index]')?.content;
  if (!url) return;
  const base = new URL(url, location.href), here = location.origin + location.pathname;
  // optional pkg:: prefix (which must be the package itself), then the name, which
  // must be followed by `(`
  const re = /(?<![\w.$@])(?:([A-Za-z][\w.]*)::)?([A-Za-z.][\w.]*)(?=\()/g;

  const link = (code, {package: pkg, functions: map}) => {
    // in <pre>, only R code (prism.js marks it with language-r or span.language-r)
    if (code.closest('pre') && !code.classList.contains('language-r') &&
        !code.querySelector('span.language-r')) return;
    if (code.closest('a')) return;
    // collect non-empty text nodes with their offsets in the whole text; elements
    // (e.g., highlighting spans) are left intact, only text is wrapped
    const nodes = [], w = d.createTreeWalker(code, NodeFilter.SHOW_TEXT);
    let text = '', n;
    while (n = w.nextNode()) if (n.length) nodes.push([n, text.length]), text += n.data;
    const hits = [...text.matchAll(re)].filter(
      m => (!m[1] || m[1] === pkg) && Object.hasOwn(map, m[2])
    );
    // go backwards so that splitting a node doesn't shift earlier offsets
    hits.reverse().forEach(m => {
      // only the function name is linked, not the `pkg::` prefix
      const u = new URL(map[m[2]], base), e = m.index + m[0].length, s = e - m[2].length;
      // no self-links to the page being read (but keep links to its sections)
      if (u.origin + u.pathname === here && !u.hash) return;
      // the name may span several text nodes in highlighted code, so wrap the part
      // in each node with a link to the same URL
      nodes.filter(([t, o]) => o < e && o + t.length > s).reverse().forEach(([t, o]) => {
        if (t.parentElement.closest('a')) return;
        const a = d.createElement('a'), r = d.createRange();
        a.href = u; a.target = '_blank';
        r.setStart(t, Math.max(s, o) - o); r.setEnd(t, Math.min(e, o + t.length) - o);
        r.surroundContents(a);
      });
    });
  };

  fetch(base).then(r => r.json()).then(idx => {
    d.querySelectorAll('code').forEach(code => link(code, idx));
    // prism.js (loaded before this script) may rewrite a block later, e.g., after
    // the autoloader fetches its language, which drops the links, so link it again
    window.Prism?.hooks.add('after-highlight', env => link(env.element, idx));
  }).catch(() => {});
})(document);
