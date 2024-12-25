(d => {
  function new_req(url, data, callback) {
    const req = new XMLHttpRequest();
    req.open('POST', url);
    // let R know the type of the request
    req.setRequestHeader('litedown-data', data);
    req.send();
    req.onload = callback;
  }
  function new_dialog(text, error, html, action = el => el.close(), callback = (el, b) => {}) {
    let el = d.querySelector('dialog.litedown-dialog');
    if (!el) {
      el = d.createElement('dialog');
      el.className = 'litedown-dialog';
      el.innerHTML = '<div></div><p><button>OK</button></p>';
      d.body.append(el);
    }
    const b = el.querySelector('button');
    b.onclick = e => action(el, b);
    const p = el.firstElementChild;
    p[(html && !error) ? 'innerHTML' : 'innerText'] = text.toLowerCase() === 'connection refused' ?
      (text + '; please re-run litedown::roam()') : text;
    if (error) p.className = 'error';
    callback(el, b);
    el.showModal();
  }
  function show_dialog(resp) {
    new_dialog(resp.responseText, resp.status !== 200);
  }
  // create a new file
  function new_file() {
    new_req(location.href, 'new', e => {
      new_dialog(e.target.responseText, e.target.status !== 200, true, (el, b) => {
        const file = el.querySelector('#filename-input').value;
        if (!file || b.innerText === 'Cancel') return el.close();
        const o = location.origin, p = location.pathname,
          u = o + p + (/\/$/.test(p) ? '' : '/../') + file, features = [];
        el.querySelectorAll('input[type="checkbox"]').forEach(input => {
          if (input.checked) features.push(input.name);
        });
        new_req(u, 'new:' + features.join(','), e => {
          const text = e.target.responseText;
          switch (text) {
            case 'view': location.href = u + '?preview=2'; break;
            case 'true': break;
            case 'false': new_dialog('Failed to create the file', true); break;
            default: new_dialog(text, true);
          }
        });
      }, (el, b) => {
        b.innerText = 'Cancel';
        const input = el.querySelector('#filename-input'),
          options = [...el.querySelector('#file-list').options].map(o => o.value);
        input.onchange = e => {
          const v = input.value.trim();
          if (!/.+\.[a-zA-Z0-9]+$/.test(v)) {
            input.value = v + '.Rmd'; input.oninput();
          }
        };
        input.oninput = e => {
          const v = input.value.trim(), X = '❌';
          b.innerText = (v === '' || v.startsWith(X) || options.includes(v) || options.includes(X + ' ' + v)) ? 'Cancel' : 'OK';
        };
      });
    });
  }
  // remove empty frontmatter
  const fm = d.querySelector('.frontmatter');
  fm && !fm.innerText.trim() && fm.remove();
  const chapters = d.querySelectorAll('div[data-source]');
  // add edit buttons for book chapters
  chapters.length > 1 && chapters.forEach((el, i) => {
    const u = el.dataset.source;
    if (!u || el.querySelector('.buttons')) return;
    const u2 = Array((u.match(/\/+/g) || []).length).fill('../').join(''),
      a2 = i === 0 ? '' : `<a href="${u2}${u}?preview=2" class="btn-lite run" title="Preview single chapter">⏵</a>`;
    el.insertAdjacentHTML('afterbegin', `<span class="buttons">${a2}<a href="?path=${u}" class="btn-lite open pencil" title="Open ${u}">✎</a></span>`);
  });
  const nav = d.querySelector('.nav-path, .title h1');
  const btn = nav.querySelector('.buttons') || d.createElement('span');
  btn.className = 'buttons';
  ['back', 'forward', 'new', 'refresh', 'print'].forEach((action, i) => {
    if (btn.querySelector(`.${action}`)) return;
    const a = d.createElement('a');
    a.href = '#'; a.title = action[0].toUpperCase() + action.slice(1);
    const k = ({
      back: 'Alt + LeftArrow', forward: 'Alt + RightArrow',
      refresh: 'Ctrl + R / Command + R'
    })[action];
    if (k) a.title += ` (${k})`;
    a.className = action + ' btn-lite';
    a.innerText = ['←', '→', '+', '⟳', '⎙'][i];
    a.onclick = e => btnAction(e, action);
    btn.append(a);
  });
  if (nav) {
    nav.querySelectorAll('a.btn-lite').forEach(a => btn.append(a));
    nav.append(btn);
  }
  function btnAction(e, action) {
    if (!action) return;
    e.preventDefault();
    switch (action) {
      case 'back': history.back(); break;
      case 'forward': history.forward(); break;
      case 'new': new_file(); break;
      case 'refresh': location.reload(); break;
      case 'print': window.print();
    }
  }
  // add classes and events to edit buttons
  d.querySelectorAll('a[href]').forEach(a => {
    if (a.innerText !== '✎' || a.classList.contains('pencil')) return;
    a.classList.add('pencil'); if (!a.title) a.title = 'Open';
    a.onclick = e => {
      e.preventDefault();
      new_req(a.href, 'open');
    };
  });
  function btnSave(e, a) {
    e.preventDefault();
    const cls = d.body.classList;
    if (cls.contains('waiting')) return;
    cls.add('waiting');
    new_req(a ? a.href : location.href, 'save', e => {
      show_dialog(e.target);
      cls.remove('waiting');
    });
  };
  // add classes and events to save buttons
  d.querySelectorAll('a.save[href]').forEach(a => {
    if (a.innerText !== '↯') return;
    a.title = 'Render to disk';
    a.getAttribute('href') === '#' && (a.title += '(Ctrl + K / Command + K)');
    a.onclick = e => btnSave(e, a);
  });
  function check_one(q, a, s) {
    (q ? d.querySelectorAll(q) : [d.body]).forEach(el => {
      let u = a ? el[a] : location.href;
      // only check assets served by local server
      if (a && u && !(u.startsWith(location.origin) && u.includes('/custom/litedown/')))
        return;
      // ignore plots under *__files/
      if (u && u.includes('__files/')) return;
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
            window.mermaid && mermaid?.init();
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
    w.querySelector('#TOC')?.remove();
    w.firstElementChild.className = el.className;
    // TODO: update fig/tab numbers
    let q = '[class^="ref-number-"]';
    w.querySelector(q) && d.querySelectorAll(q).forEach(el => {
    });
    // update cross-references
    w.querySelectorAll('a[class^="cross-ref-"][href^="#"]').forEach(el => {
      const n = d.getElementById(el.getAttribute('href').replace(/^#/, ''))
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
    container.querySelectorAll('.auto-numbers span[data-line-number]').forEach(el => {
      const n = +el.dataset.lineNumber;
      if (n) el.onclick = e => {
        const u = `${location.href.replace(/[?#].*/, '')}?line=${n}`;
        new_req(path ? `${u}&path=${path}` : u, 'open');
      };
    });
  }
  window.addEventListener('load', e => {
    d.onkeydown = e => {
      const k = e.key, ctrl = e.metaKey || e.ctrlKey; let a;
      k === 'r' && ctrl && (a = 'refresh');
      e.altKey && (a = ({'ArrowLeft': 'back', 'ArrowRight': 'forward'})[k]);
      btnAction(e, a);
      k == 'k' && ctrl && btnSave(e);
    };
    new_interval();
  });
  // send a request to clean up __files/ before page is unloaded
  window.addEventListener('beforeunload', e => new_req(location.href, 'cleanup'));
  // when updating a book chapter, this script will be reloaded, and we need to
  // clear the old interval and create a new loop
  if (d.body.dataset.timerId) {
    clearInterval(d.body.dataset.timerId);
    new_interval();
  }
})(document);
