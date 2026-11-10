// link function calls like `foo(` in <code>/<pre><code> to their help pages, using
// the map (function -> URL) at the URL in <meta name="function-index" content="...">
(d => {
  const url = d.querySelector('meta[name=function-index]')?.content;
  if (!url) return;
  const base = new URL(url, location.href), here = location.origin + location.pathname;
  // a name followed by `(` (possibly in the next text node, e.g., when highlighted)
  const re = /(?<![\w.$@])[A-Za-z.][\w.]*(?=\()/g;
  fetch(base).then(r => r.json()).then(map => {
    d.querySelectorAll('code').forEach(code => {
      if (code.closest('a')) return;
      // collect text nodes (with their offsets in the whole text), and leave the
      // elements added by syntax highlighters intact: only wrap names in text nodes
      const nodes = [], w = d.createTreeWalker(code, NodeFilter.SHOW_TEXT);
      let text = '', n;
      while (n = w.nextNode()) n.length && (nodes.push([n, text.length]), text += n.data);
      const hits = [...text.matchAll(re)].filter(m => Object.hasOwn(map, m[0]));
      // go backwards so that splitting a node doesn't shift earlier offsets
      hits.reverse().forEach(m => {
        const [node, off] = nodes.findLast(([, o]) => o <= m.index);
        const s = m.index - off, e = s + m[0].length;
        // skip names split across nodes, and those inside links
        if (e > node.length || node.parentElement.closest('a')) return;
        const u = new URL(map[m[0]], base);
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
