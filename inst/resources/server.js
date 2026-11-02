(d => {
  const $ = (q, el = d) => el.querySelector(q),
    $$ = (q, el = d) => [...el.querySelectorAll(q)];
  // the root URL of the server, e.g., http://127.0.0.1:4321/custom/litedown/
  const root = location.href.replace(/(\/custom\/litedown\/).*/, '$1');
  function store(k, v) {
    try {
      if (v === undefined) {
        const x = sessionStorage.getItem(k); sessionStorage.removeItem(k); return x;
      }
      sessionStorage.setItem(k, v);
    } catch (e) {}
  }

  /* ---- requests ---- */
  let down = false, retry = 0;
  // send a POST request with the request type in the header 'litedown-data';
  // the callback always runs, with failed = true if the server is unreachable
  function new_req(url, data, callback = () => {}, opts = {}) {
    fetch(url, {
      method: 'POST', headers: {'litedown-data': data}, ...opts
    }).then(
      r => r.text().then(text => {
        const bad = r.status >= 502 && r.status <= 504 ||
          /^connection (refused|reset)/i.test(text);
        if (bad) server_down();
        callback({status: r.status, responseText: text, failed: bad});
      }),
      () => {
        if (!opts.keepalive) server_down();
        callback({status: 0, responseText: '', failed: true});
      }
    );
  }
  // keep pinging the server after it is disconnected, and reload the page once
  // it is back
  function server_down() {
    if (down) return;
    down = true; retry = 0;
    banner('Server disconnected. Reconnecting&hellip; (re-run <code>litedown::roam()</code> if you stopped it)');
    ping();
  }
  function ping() {
    const again = () => setTimeout(ping, Math.min(10000, 2000 * 1.5 ** retry++));
    fetch(location.href.replace(/[?#].*/, ''), {
      method: 'POST', headers: {'litedown-data': 'ping'}
    }).then(r => r.ok ? reload() : again(), again);
  }
  // reload the page and restore the scroll position afterwards
  function reload(updated) {
    store('litedown-scroll:' + location.href, scrollY);
    // keep toasts that are still showing (e.g., the page reloads because the
    // rendered output file was added to the folder being listed)
    const ts = $$('.litedown-toast').map(el => el.toast).filter(t => t && (!t.expire || t.expire > Date.now()));
    ts.length && store('litedown-toasts', JSON.stringify(ts));
    updated && store('litedown-updated', '1');
    location.reload();
  }

  /* ---- messages: banner, toasts, and dialogs ---- */
  function banner(html) {
    let el = $('.litedown-banner');
    if (!el) {
      el = d.createElement('div');
      el.className = 'litedown-banner'; el.setAttribute('role', 'alert');
      d.body.prepend(el);
    }
    el.innerHTML = html;
  }
  // a non-blocking message; errors stay until dismissed
  function toast(text, {error, html, link, timeout = error ? 0 : 10000} = {}) {
    let box = $('.litedown-toasts');
    if (!box) {
      box = d.createElement('div');
      box.className = 'litedown-toasts'; box.setAttribute('aria-live', 'polite');
      d.body.append(box);
    }
    // don't pile up identical messages (e.g., from repeated polling)
    if ($$('.litedown-toast', box).some(el => el.dataset.text === text)) return;
    const el = d.createElement('div');
    el.className = 'litedown-toast' + (error ? ' error' : '');
    el.toast = {text, error, html, link, expire: timeout && Date.now() + timeout};
    el.dataset.text = text; el.setAttribute('role', error ? 'alert' : 'status');
    const p = d.createElement('span');
    p[html ? 'innerHTML' : 'innerText'] = text;
    el.append(p);
    if (link) {
      const a = d.createElement('a');
      a.href = link; a.target = '_blank'; a.innerText = 'Open';
      el.append(a);
    }
    const x = d.createElement('button');
    x.type = 'button'; x.innerText = '×'; x.title = x.ariaLabel = 'Dismiss';
    x.onclick = () => el.remove();
    el.append(x);
    box.append(el);
    timeout && setTimeout(() => el.remove(), timeout);
  }
  // restore toasts saved before an automatic reload
  function restore_toasts() {
    try {
      JSON.parse(store('litedown-toasts') || '[]').forEach(t => {
        toast(t.text, {...t, timeout: t.expire ? t.expire - Date.now() : 0});
      });
    } catch (e) {}
  }
  // a modal dialog with a list of buttons: [{label, primary, action}]
  function new_dialog(content, {error, html, buttons, setup} = {}) {
    let el = $('dialog.litedown-dialog');
    if (!el) {
      el = d.createElement('dialog');
      el.className = 'litedown-dialog';
      el.innerHTML = '<div class="dialog-body"></div><p class="actions"></p>';
      d.body.append(el);
    }
    el.oncancel = null;
    const p = el.firstElementChild;
    p[(html && !error) ? 'innerHTML' : 'innerText'] = content;
    p.className = 'dialog-body' + (error ? ' error' : '');
    const bar = el.lastElementChild;
    bar.replaceChildren(...(buttons || [{label: 'OK', primary: true}]).map(b => {
      const x = d.createElement('button');
      x.type = 'button'; x.innerText = b.label; x.className = b.primary ? 'primary' : '';
      x.onclick = () => b.action ? b.action(el, x) : el.close();
      return x;
    }));
    setup && setup(el);
    el.showModal();
    return el;
  }

  /* ---- new files ---- */
  function new_file() {
    new_req(location.href, 'new', r => {
      if (r.status !== 200) return toast(r.responseText || 'Failed to open the form', {error: true});
      const o = location.origin, p = location.pathname,
        base = o + p + (/\/$/.test(p) ? '' : '/../');
      let create, input, names;
      const features = el => $$('input[type="checkbox"]:checked', el).map(i => i.name),
        final = v => /\.[a-zA-Z0-9]+$/.test(v) ? v : v + '.Rmd';
      function check(el) {
        const v = input.value.trim(), f = final(v), hint = $('#filename-hint', el);
        let msg = '', ok = false;
        if (v === '') msg = '';
        else if (/[\\:*?"<>|]|^\/|(^|\/)\.\.(\/|$)/.test(v)) msg = 'Invalid characters in the filename.';
        else if (names.includes(f)) msg = `${f} already exists.`;
        else { ok = true; msg = `Will create ${f}`; }
        hint.innerText = msg; hint.className = ok || !msg ? '' : 'error';
        create.disabled = !ok;
        // YAML and features only apply to files that litedown can render
        $('#feature-section', el).hidden = !/\.(rmd|md|qmd|r)$/i.test(f);
        input.setAttribute('aria-invalid', !ok && !!msg);
        return ok;
      }
      function preview(el) {
        const n = features(el);
        $('#features-count', el).innerText = n.length ? `${n.length} selected` : 'none';
        new_req(location.href, 'yaml:' + n.join(','), r => {
          $('#yaml-preview', el).innerText = r.status === 200 ? r.responseText : '';
        });
      }
      function submit(el) {
        if (!check(el)) return;
        const file = final(input.value.trim()), u = base + encodeURI(file);
        new_req(u, 'new:' + features(el).join(','), r => {
          const text = r.responseText;
          switch (text) {
            case 'view': location.href = u + '?preview=2'; break;
            case 'true': el.close(); toast(`Created ${file}`); break;
            case 'false': toast('Failed to create the file', {error: true}); break;
            default: toast(text, {error: true});
          }
        });
      }
      new_dialog(r.responseText, {
        html: true,
        buttons: [
          {label: 'Create', primary: true, action: submit},
          {label: 'Cancel'}
        ],
        setup: el => {
          input = $('#filename-input', el);
          create = $$('.actions button', el)[0];
          names = $$('#file-list option', el).map(o => o.value);
          input.oninput = () => check(el);
          input.onkeydown = e => e.key === 'Enter' && (e.preventDefault(), submit(el));
          // presets select a group of features
          const boxes = $$('input[type="checkbox"]', el);
          $$('.presets button', el).forEach(b => b.onclick = () => {
            const s = (b.dataset.features || '').split(',');
            boxes.forEach(i => i.checked = s.includes(i.name));
            $$('.presets button', el).forEach(x => x.setAttribute('aria-pressed', x === b));
            preview(el);
          });
          boxes.forEach(i => i.onchange = () => {
            $$('.presets button', el).forEach(x => x.setAttribute('aria-pressed', false));
            preview(el);
          });
          // confirm before discarding a typed filename with Esc
          el.oncancel = e => {
            if (input.value.trim() && !confirm('Discard the new file?')) e.preventDefault();
          };
          check(el); preview(el);
          input.focus();
        }
      });
    });
  }

  /* ---- toolbar ---- */
  // remove empty frontmatter
  const fm = $('.frontmatter');
  fm && !fm.innerText.trim() && fm.remove();
  const chapters = $$('[data-source]');
  // add edit buttons for book chapters
  chapters.length > 1 && chapters.forEach((el, i) => {
    const u = el.dataset.source;
    if (!u || $('.buttons', el)) return;
    const u2 = Array((u.match(/\/+/g) || []).length).fill('../').join(''),
      a2 = i === 0 ? '' : `<a href="${u2}${u}?preview=2" class="btn-lite run" title="Preview this chapter only (${u})" aria-label="Preview ${u}">►</a>`;
    el.insertAdjacentHTML('afterbegin', `<span class="buttons">${a2}<a href="?path=${u}" class="btn-lite open pencil" title="Open ${u} in editor" aria-label="Open ${u} in editor">✎</a></span>`);
  });
  const nav = $('.nav-path, .title h1');
  if (nav) {
    const btn = $('.buttons', nav) || d.createElement('span');
    btn.className = 'buttons';
    [
      ['back', '←', 'Back', 'Alt + LeftArrow'], ['forward', '→', 'Forward', 'Alt + RightArrow'],
      ['new', '+', 'New', 'create a new file'],
      ['refresh', '⟳', 'Refresh', 'Ctrl + R / Command + R'], ['print', '⎙', 'Print']
    ].forEach(([action, icon, label, key]) => {
      if ($(`.${action}`, btn)) return;
      const a = d.createElement('button');
      a.type = 'button'; a.className = action + ' btn-lite';
      a.title = label + (key ? ` (${key})` : '');
      a.ariaLabel = label; a.dataset.label = label;
      a.innerText = icon;
      a.onclick = e => btnAction(e, action);
      btn.append(a);
    });
    $$('a.btn-lite', nav).forEach(a => {
      const l = {save: 'Render', open: 'Edit', run: 'Run'};
      for (const k in l) a.classList.contains(k) && (a.dataset.label = l[k]);
      btn.append(a);
    });
    nav.append(btn);
  }
  // disable the back/forward buttons when there is no history (if supported)
  function nav_state() {
    if (!window.navigation) return;
    const b = $('.buttons .back'), f = $('.buttons .forward');
    b && (b.disabled = !navigation.canGoBack);
    f && (f.disabled = !navigation.canGoForward);
  }
  nav_state();
  function btnAction(e, action) {
    if (!action) return;
    e.preventDefault();
    switch (action) {
      case 'back': history.back(); break;
      case 'forward': history.forward(); break;
      case 'new': new_file(); break;
      case 'refresh': reload(); break;
      case 'print': window.print();
    }
  }
  // make links with icons behave like buttons
  function as_button(a) {
    a.setAttribute('role', 'button');
    if (!a.ariaLabel) a.ariaLabel = a.title;
  }
  // add classes and events to edit buttons
  function open_link(a) {
    a.onclick = e => {
      e.preventDefault();
      new_req(a.href, 'open');
    };
  }
  $$('a[href]').forEach(a => {
    if (a.innerText !== '✎' || a.onclick) return;
    a.classList.add('pencil'); if (!a.title) a.title = 'Open in editor';
    as_button(a);
    open_link(a);
  });
  // links to source lines in the traceback of an error page
  $$('.traceback a[href]').forEach(open_link);
  function btnSave(e, a) {
    e.preventDefault();
    const cls = d.body.classList, el = a || $('.buttons .save');
    if (cls.contains('waiting')) return;
    cls.add('waiting'); el && el.classList.add('waiting');
    new_req(a ? a.href : location.href, 'save', r => {
      cls.remove('waiting'); el && el.classList.remove('waiting');
      const ok = r.status === 200, m = r.responseText.match(/^Rendered and saved: (.+\.html?)$/);
      if (r.failed) return;
      toast(r.responseText, {
        error: !ok || /^Unable to render/.test(r.responseText),
        link: ok && m && !/^([/\\]|[a-zA-Z]:)/.test(m[1]) ? root + m[1] : null
      });
    });
  };
  // add classes and events to save buttons
  $$('a.save[href]').forEach(a => {
    if (a.innerText !== '↯') return;
    a.title = 'Render to disk in a fresh R session';
    a.getAttribute('href') === '#' && (a.title += ' (Ctrl + K / Command + K)');
    as_button(a);
    a.onclick = e => btnSave(e, a);
  });

  /* ---- file listing ---- */
  // show modification time as relative time
  function rel(ms) {
    const s = (Date.now() - ms) / 1000, u = [
      ['year', 31536000], ['month', 2592000], ['day', 86400], ['hour', 3600],
      ['minute', 60]
    ];
    for (const [n, k] of u) if (s >= k) {
      const v = Math.floor(s / k); return `${v} ${n}${v > 1 ? 's' : ''} ago`;
    }
    return 'just now';
  }
  function times() {
    $$('time[data-ms]').forEach(t => t.innerText = rel(+t.dataset.ms));
  }
  times(); setInterval(times, 30000);
  const tools = $('.listing-tools');
  if (tools) {
    const items = () => $$('.box, article > ul > li'),
      input = $('input', tools), sel = $('select', tools);
    const name = el => ($('a', el)?.innerText || '').toLowerCase(),
      ms = el => +($('time[data-ms]', el)?.dataset.ms || 0);
    items().forEach((el, i) => el.dataset.idx = i);
    function update() {
      const q = input.value.trim().toLowerCase();
      items().forEach(el => el.hidden = q !== '' && !name(el).includes(q));
      // sort items within their parents
      const by = sel.value;
      const cmp = {
        name: (a, b) => name(a).localeCompare(name(b)),
        time: (a, b) => ms(b) - ms(a),
        '': (a, b) => a.dataset.idx - b.dataset.idx
      }[by];
      const groups = new Map();
      items().forEach(el => {
        const g = groups.get(el.parentNode) || []; g.push(el); groups.set(el.parentNode, g);
      });
      groups.forEach((g, parent) => {
        const next = g[g.length - 1].nextSibling;
        g.sort(cmp).forEach(el => parent.insertBefore(el, next));
      });
      let none = $('.no-match');
      const n = items().filter(el => !el.hidden).length;
      if (!n && !none) {
        none = d.createElement('p'); none.className = 'no-match';
        none.innerText = 'No matching files.'; tools.after(none);
      }
      none && (none.hidden = n > 0);
    }
    input.oninput = update; sel.onchange = update;
    d.addEventListener('keydown', e => {
      if (e.key === '/' && !/^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) {
        e.preventDefault(); input.focus();
      }
      if (e.key === 'Escape' && e.target === input) { input.value = ''; update(); }
    });
  }

  /* ---- live preview ---- */
  function check_one(q, a, s) {
    (q ? $$(q) : [d.body]).forEach(el => {
      let u = a ? el[a] : location.href;
      // only check assets served by local server
      if (a && u && !(u.startsWith(location.origin) && u.includes('/custom/litedown/')))
        return;
      // ignore plots under *__files/
      if (u && u.includes('__files/')) return;
      if (el.dataset.wait) return;
      el.dataset.wait = 1;  // don't send another request while waiting
      new_req(u, q ? (a ? 'asset' : `book:${el.dataset[s]}`) : 'page', r => {
        const res = r.responseText;
        if (r.status !== 200) {
          r.failed || toast(res, {error: true});
        } else if (res !== '') {
          if (a) {
            el[a] = `${u.replace(/[?].*/, '')}?timestamp=${+new Date()}`;
            el.tagName ==='SCRIPT' && update_script(el);
          } else if (s) {
            // update a book chapter (response is an HTML fragment)
            update_chapter(el, res);
            window.mermaid && mermaid?.init();
            // also reload js
            $$('script[src]').forEach(update_script);
          } else {
            // the current page source file has changed; refresh page
            res === '1' && reload(true);
          }
        }
        el.dataset.wait = '';
      });
    });
  }
  function update_chapter(el, html) {
    const w = d.createElement('div'); w.innerHTML = html;
    $('#TOC', w)?.remove();
    w.firstElementChild.className = el.className;
    // TODO: update fig/tab numbers
    let q = '[class^="ref-number-"]';
    $(q, w) && $$(q).forEach(el => {
    });
    // update cross-references
    $$('a[class^="cross-ref-"][href^="#"]', w).forEach(el => {
      const n = d.getElementById(el.getAttribute('href').replace(/^#/, ''))
        ?.querySelector('.section-number,[class^="ref-number-"]')
        ?.innerText;
      if (n) el.innerText = n;
    });
    el.outerHTML = w.innerHTML;
    // update footnote numbers
    q = 'a[id^="fnref-"]';
    $(q, w) && $$(q).forEach((el, i) => {
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
    // no need to check when nobody is looking, or the server is gone
    if (d.hidden || down) return;
    // don't refresh whole page when checking a book page of multiple source files
    chapters.length <= 1 ? check_one() : check_one('[data-source]', false, 'source');
    check_one('[src]', 'src');  // <img> and <script>
    check_one('link[href]', 'href');  // css
  }
  // do live preview only when requested
  const live = $('meta[name="live-previewer"]')?.content === 'litedown::roam';
  function new_interval() {
    if (live) d.body.dataset.timerId = setInterval(check_all, 2000);
    // also support opening files by clicking on line numbers in code blocks
    chapters.length <= 1 ? open_line(d) :
      chapters.forEach(el => open_line(el, el.dataset.source));
  }
  function open_line(container, path) {
    $$('.auto-numbers span[data-line-number]', container).forEach(el => {
      const n = +el.dataset.lineNumber;
      if (n) el.onclick = e => {
        const u = `${location.href.replace(/[?#].*/, '')}?line=${n}`;
        new_req(path ? `${u}&path=${path}` : u, 'open');
      };
    });
  }
  // check immediately when the tab becomes visible again
  d.onvisibilitychange = () => live && check_all();
  window.onpageshow = nav_state;
  window.addEventListener('load', e => {
    d.onkeydown = e => {
      const k = e.key, ctrl = e.metaKey || e.ctrlKey; let a;
      k === 'r' && ctrl && !e.shiftKey && (a = 'refresh');
      e.altKey && (a = ({'ArrowLeft': 'back', 'ArrowRight': 'forward'})[k]);
      btnAction(e, a);
      k == 'k' && ctrl && btnSave(e);
    };
    new_interval();
    // restore the scroll position and notify the update after an auto reload
    const y = store('litedown-scroll:' + location.href);
    y && requestAnimationFrame(() => scrollTo(0, +y));
    restore_toasts();
    store('litedown-updated') && toast('Page updated', {timeout: 1500});
  });
  // send a request to clean up __files/ before page is unloaded
  window.onbeforeunload = e => new_req(location.href, 'cleanup', () => {}, {keepalive: true});
  // when updating a book chapter, this script will be reloaded, and we need to
  // clear the old interval and create a new loop
  if (d.body.dataset.timerId) {
    clearInterval(d.body.dataset.timerId);
    new_interval();
  }
})(document);
