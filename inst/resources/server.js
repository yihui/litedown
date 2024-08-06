(d => {
  function new_req(url, data, callback) {
    const req = new XMLHttpRequest();
    req.open('POST', url);
    // let R know the type of the request
    req.setRequestHeader('litedown-data', data);
    req.send();
    req.onload = callback;
  }
  function show_dialog(resp) {
    let el = d.querySelector('dialog.litedown-dialog');
    if (!el) {
      el = d.createElement('dialog');
      el.className = 'litedown-dialog';
      el.innerHTML = '<p></p><button>OK</button>';
      el.querySelector('button').onclick = e => el.close();
      d.body.append(el);
    }
    const p = el.firstElementChild;
    p.innerText = resp.responseText; if (resp.status !== 200) p.className = 'error';
    el.showModal();
  }
  // remove empty frontmatter
  const fm = d.querySelector('.frontmatter');
  if (fm && !fm.innerText) fm.remove();
  const chapters = d.querySelectorAll('div[data-source]');
  // add edit buttons for book chapters
  chapters.forEach(el => {
    const u = el.dataset.source;
    u && !el.querySelector('.pencil') &&
      el.insertAdjacentHTML('afterbegin', `<a href="?path=${u}" title="Open ${u}">✎</a>`);
  });
  // add classes and events to edit buttons
  d.querySelectorAll('a[href]').forEach(a => {
    if (a.innerText !== '✎' || a.classList.contains('pencil')) return;
    a.classList.add('pencil'); if (!a.title) a.title = 'Open';
    a.onclick = e => {
      e.preventDefault();
      new_req(a.href, 'open');
    };
  });
  // add classes and events to save buttons
  d.querySelectorAll('a[href]').forEach(a => {
    if (a.innerText !== '↯' || a.classList.contains('save')) return;
    a.classList.add('save'); a.title = 'Render and Save';
    a.onclick = e => {
      e.preventDefault();
      const cls = d.body.classList;
      if (cls.contains('waiting')) return;
      cls.add('waiting');
      new_req(a.href, 'save', e => {
        show_dialog(e.target);
        cls.remove('waiting');
      });
    };
  });
  function check_one(q, a, s) {
    (q ? d.querySelectorAll(q) : [d.body]).forEach(el => {
      let u = a ? el[a] : location.href;
      // only check assets served by local server
      if (a && u && !(u.startsWith(location.origin) && u.includes('/custom/litedown/')))
        return;
      if (el.dataset.wait) return;
      el.dataset.wait = 1;  // don't send another request while waiting
      new_req(u, q ? (a ? 'asset' : `book:${el.dataset[s]}`) : 'page', e => {
        const res = e.target.responseText;
        if (e.target.status !== 200) {
          if (res.toLowerCase() != 'connection reset by peer') show_dialog(e.target);
        } else if (res !== '') {
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
            res === '1' && location.reload();
          }
        }
        el.dataset.wait = '';
      });
    });
  }
  function update_chapter(el, html) {
    const w = d.createElement('div'); w.innerHTML = html;
    // current chapter number
    const n = el.querySelector('.section-number')?.innerText.replace(/^([0-9A-Z]+).*/, '$1');
    // change the leading 1 to n in section numbers
    w.querySelectorAll('.section-number').forEach(el => {
      const t = el.innerText.match(/^(\d+)(.*)/);
      if (t.length === 3) el.innerText = `${n.match(/[A-Z]/) ? n : (t[1] - 1 + (+n || 1))}${t[2]}`;
    });
    w.firstElementChild.className = el.className;
    // TODO: update fig/tab numbers
    let q = '[class^="ref-number-"]';
    w.querySelector(q) && d.querySelectorAll(q).forEach(el => {
    });
    // update cross-references
    w.querySelectorAll('a[class^="cross-ref-"][href^="#"]').forEach(el => {
      const n = d.querySelector(el.getAttribute('href'))
        ?.querySelector('.section-number,[class^="ref-number-"]')
        ?.innerText;
      if (n) el.innerText = n;
    });
    el.outerHTML = w.innerHTML;
    // update footnote numbers
    q = 'a[id^="fnref-"]';
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
    chapters.length <= 1 ? check_one() : check_one('div[data-source]', false, 'source');
    check_one('[src]', 'src');  // <img> and <script>
    check_one('link[href]', 'href');  // css
  }
  // do live preview only when requested
  const live = d.querySelector('meta[name="live-previewer"]')?.content === 'litedown::roam';
  function new_interval() {
    if (live) d.body.dataset.timerId = setInterval(check_all, 2000);
    // also support opening files by clicking on line numbers in code blocks
    chapters.length <= 1 ? open_line(d) :
      chapters.forEach(el => open_line(el, el.dataset.source));
  }
  function open_line(container, path) {
    container.querySelectorAll('span[data-line-number]').forEach(el => {
      const n = +el.dataset.lineNumber;
      if (n) el.onclick = e => {
        const u = `${location.href.replace(/[?#].*/, '')}?line=${n}`;
        new_req(path ? `${u}&path=${path}` : u, 'open');
      };
    });
  }
  window.addEventListener('load', new_interval);
  // when updating a book chapter, this script will be reloaded, and we need to
  // clear the old interval and create a new loop
  if (d.body.dataset.timerId) {
    clearInterval(d.body.dataset.timerId);
    new_interval();
  }
})(document);
