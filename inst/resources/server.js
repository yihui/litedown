(d => {
  function new_req(url, data, callback) {
    const req = new XMLHttpRequest();
    req.open('POST', url);
    // let R know the type of the request
    req.send(data);
    req.onload = callback;
  }
  // add classes and events to edit buttons
  d.querySelectorAll('a[href]').forEach(a => {
    if (a.innerText !== '✎') return;
    a.classList.add('pencil');
    a.onclick = e => {
      e.preventDefault();
      new_req(a.href, 'open');
    };
  });
  function check_one(q, a, s) {
    (q ? d.querySelectorAll(q) : [d.body]).forEach(el => {
      let u = q ? (a ? el[[a]] : el.dataset[[s]]) : location.href;
      // only check assets served by local server
      if (a && u && !(u.startsWith(location.origin) && u.includes('/custom/litedown/')))
        return;
      if (el.dataset.wait) return;
      el.dataset.wait = 1;  // don't send another request while waiting
      new_req(u, q ? (a ? 'asset' : `book:${u}`) : 'page', e => {
        const res = e.target.responseText;
        if (res !== '') {
          if (a) {
            el[a] = `${u.replace(/[?].*/, '')}?timestamp=${+new Date()}`;
            el.tagName ==='SCRIPT' && update_script(el);
          } else if (s) {
            // update a book chapter (response is an HTML fragment)
            el.outerHTML = res;
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
    // don't refresh whole page when checking a book page
    d.querySelector('div[data-source]') || check_one();
    check_one('[src]', 'src');  // <img> and <script>
    check_one('link[href]', 'href');  // css
    check_one('div[data-source]', false, 'source');  // Rmd chapter
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
