// link function calls like `fn(` or `pkg::fn(` in <code> to their help pages, using
// the map (function -> URL) at the URL in <meta name="function-index" content="...">
(d => {
  const url = d.querySelector('meta[name=function-index]')?.content;
  if (!url) return;
  const base = new URL(url, location.href), here = location.origin + location.pathname;
  // optional pkg:: prefix, then the name, which must be followed by `(`
  const re = /(?<![\w.$@])(?:[A-Za-z][\w.]*::)?([A-Za-z.][\w.]*)(?=\()/g;
  fetch(base).then(r => r.json()).then(map => {
    d.querySelectorAll('code').forEach(code => {
      // in <pre>, only R code (prism.js marks it with language-r or span.language-r)
      if (code.closest('pre') && !code.classList.contains('language-r') &&
          !code.querySelector('span.language-r')) return;
      if (code.closest('a')) return;
      // collect non-empty text nodes with their offsets in the whole text; elements
      // (e.g., highlighting spans) are left intact, only names in text nodes are wrapped
      const nodes = [], w = d.createTreeWalker(code, NodeFilter.SHOW_TEXT);
      let text = '', n;
      while (n = w.nextNode()) if (n.length) nodes.push([n, text.length]), text += n.data;
      const hits = [...text.matchAll(re)].filter(m => Object.hasOwn(map, m[1]));
      // go backwards so that wrapping a match doesn't shift earlier offsets
      hits.reverse().forEach(m => {
        const [node, off] = nodes.findLast(([, o]) => o <= m.index);
        const s = m.index - off, e = s + m[0].length;
        // skip names split across nodes, and those inside links
        if (e > node.length || node.parentElement.closest('a')) return;
        const u = new URL(map[m[1]], base);
        // no self-links to the page being read (but keep links to its sections)
        if (u.origin + u.pathname === here && !u.hash) return;
        const a = d.createElement('a'), r = d.createRange();
        a.href = u;
        r.setStart(node, s); r.setEnd(node, e);
        r.surroundContents(a);
      });
    });
  }).catch(() => {});
})(document);
