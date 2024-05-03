(d => {
  const me = d.currentScript;
  window.addEventListener('load', () => {
    function check_one(q, a, s) {
      (q ? d.querySelectorAll(q) : [d.body]).forEach(el => {
        let u = q ? (a ? el[[a]] : el.dataset[[s]]) : location.href;
        // only check assets served by local server
        if (a && u && !u.startsWith(location.origin)) return;
        if (el.dataset.wait) return;
        el.dataset.wait = 1;  // don't send another request while waiting
        const req = new XMLHttpRequest();
        req.open('POST', u);
        // let R know the type of the request
        req.send(q ? (a ? 'asset' : 'book') : 'page');
        req.onload = (e) => {
          const res = e.target.responseText;
          if (res !== '') {
            if (a) {
              el[a] = `${u.replace(/[?].*/, '')}?timestamp=${+new Date()}`;
              el.tagName ==='SCRIPT' && update_script(el);
            } else if (s) {
              // update a book chapter
              // el.outerHTML = res;  // also reload js
            } else {
              // the current page source file has changed; refresh page
              location.reload();
            }
          }
          el.dataset.wait = '';
        };
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

    const id = setInterval(check_all, 2000);
    function check_all() {
      // if the current script has been removed, stop the check
      if (!me.parentNode) return clearInterval(id);
      // don't refresh whole page when checking a book page
      d.querySelector('div[data=source]') || check_one();
      check_one('[src]', 'src');  // <img> and <script>
      check_one('link[href]', 'href');  // css
      check_one('div[data=source]', false, 'source');  // Rmd chapter
    }
  });
})(document);
