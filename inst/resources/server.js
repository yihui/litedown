(d => {
  function new_req(url, data, callback) {
    const req = new XMLHttpRequest();
    req.open('POST', url);
    // let R know the type of the request
    req.send(data);
    req.onload = callback;
  }
  // remove empty frontmatter
  const fm = d.querySelector('.frontmatter');
  if (fm && !fm.innerText) fm.remove();
  // add edit buttons for book chapters
  d.querySelectorAll('div[data-source]').forEach(el => {
    const u = el.dataset.source;
    u && !el.querySelector('.pencil') && el.insertAdjacentHTML('afterbegin', `<a href="?path=${u}">✎</a>`);
  });
  // add classes and events to edit buttons
  d.querySelectorAll('a[href]').forEach(a => {
    if (a.innerText !== '✎' || a.classList.contains('pencil')) return;
    a.classList.add('pencil');
    a.onclick = e => {
      e.preventDefault();
      new_req(a.href, 'open');
    };
  });
  function check_one(q, a, s) {
    (q ? d.querySelectorAll(q) : [d.body]).forEach(el => {
      let u = a ? el[[a]] : location.href;
      // only check assets served by local server
      if (a && u && !(u.startsWith(location.origin) && u.includes('/custom/litedown/')))
        return;
      if (el.dataset.wait) return;
      el.dataset.wait = 1;  // don't send another request while waiting
      new_req(u, q ? (a ? 'asset' : `book:${el.dataset[[s]]}`) : 'page', e => {
        const res = e.target.responseText;
        if (e.target.status !== 200) {
          el.innerHTML = `<pre><code class="error">${res}</code></pre>`;
          return;
        }
        if (res !== '') {
          if (a) {
            el[a] = `${u.replace(/[?].*/, '')}?timestamp=${+new Date()}`;
            el.tagName ==='SCRIPT' && update_script(el);
          } else if (s) {
            // update a book chapter (response is an HTML fragment)
            update_chapter(el, res);
            // also reload js
            d.querySelectorAll('script[src]').forEach(update_script);
          } else {
            // the current page source file has changed; refresh page
            location.reload();
          }
        }
        el.dataset.wait = '';
      });
    });
  }
  function update_chapter(el, html) {
    const w = d.createElement('div'); w.innerHTML = html;
    // current chapter number
    const n = +el.querySelector('.section-number')?.innerText.replace(/^([0-9]+).*/, '$1');
    // change the leading 1 to n in section numbers
    n && w.querySelectorAll('.section-number').forEach(el => {
      const t = el.innerText.match(/^(\d+)(.*)/);
      if (t.length === 3) el.innerText = `${t[1] - 1 + n}${t[2]}`;
    });
    el.outerHTML = w.innerHTML;
    // update footnote numbers
    const q = 'a[id^="fnref-"]';
    w.querySelector(q) && d.querySelectorAll(q).forEach((el, i) => {
      el.innerText = i + 1;
    });
    w.remove();
  }
  // to reload <script src>, it has to be destroyed and re-created
  function update_script(el) {
    const s = d.createElement('script');
    el.after(s);
    for (const a of el.attributes) {
      s.setAttribute(a.name, a.value);
    }
    el.remove();
  }
  function check_all() {
    // don't refresh whole page when checking a book page of multiple source files
    d.querySelectorAll('div[data-source]').length <= 1 ?
      check_one() : check_one('div[data-source]', false, 'source');
    check_one('[src]', 'src');  // <img> and <script>
    check_one('link[href]', 'href');  // css
  }
  function new_interval() {
    d.body.dataset.timerId = setInterval(check_all, 2000);
  }
  window.addEventListener('load', new_interval);
  // when updating a book chapter, this script will be reloaded, and we need to
  // clear the old interval and create a new loop
  if (d.body.dataset.timerId) {
    clearInterval(d.body.dataset.timerId);
    new_interval();
  }
})(document);
