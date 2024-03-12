(function(d) {
  let p = d.body;  // container of slides; assume <body> for now
  const s1 = ':scope > hr:not([class])', s2 = ':scope > h2';
  // find a container that has at least n "slides"
  function findContainer(s, n = 1) {
    if (p.querySelectorAll(s).length >= n) return true;
    // if body doesn't contain headings or <hr>s, look into children
    for (let i = 0; i < p.children.length; i++) {
      if (p.children[i].querySelectorAll(s).length >= n) {
        p = p.children[i]; break;
      }
    }
    return false;
  }
  function newEl(tag, cls) {
    const el = d.createElement(tag);
    if (cls) el.className = cls;
    return el;
  }
  if (!findContainer(s1, 3)) {
    // if not enough <hr>s found in children; look for <h2> instead
    if (p.tagName === 'BODY') {
      // not enough h2 found, this page is not appropriate for slides
      if (!findContainer(s2) && p.tagName === 'BODY') return;
      p.querySelectorAll(s2).forEach(h2 => h2.before(newEl('hr')));
    }
  }
  p.classList.add('slide-container');
  // add 'slide' class to the frontmatter div and toc
  ['.frontmatter', '#TOC'].forEach(s => {
    d.body.querySelector(s)?.classList.add('slide');
  });

  function newSlide(s) {
    return (s?.innerText === '') ? s : newEl('div', 'slide');
  }
  function isSep(el) {
    return el.tagName === 'HR' && el.attributes.length === 0;
  }
  let el = p.firstElementChild; if (isSep(el)) el.remove();
  el = p.firstElementChild; if (!el) return;
  let s = newSlide(); el.before(s);
  while (true) {
    let el = s.nextSibling;
    if (!el) break;
    // remove slide separators (<hr>) and create new slide
    if (isSep(el)) {
      s = newSlide(s);
      el.before(s); el.remove();
    } else if (el.classList?.contains('slide')) {
      s = newSlide(s);
      el.after(s);
    } else {
      s.append(el);
    }
  }
  function setAttr(el, attr) {
    const m = newEl('div');
    m.innerHTML = `<div ${attr}></div>`;
    const attrs = m.firstElementChild.attributes;
    for (let i = 0; i < attrs.length; i++) {
      let a = attrs[i];
      el.setAttribute(a.name, a.value);
    }
    m.remove();
  }
  function reveal(el) {
    setTimeout(() => el?.scrollIntoView(), 100);
  }
  const dE = d.documentElement, dC = d.body.classList;
  const slides = d.querySelectorAll('div.slide'), N = slides.length,
        tm = d.querySelector('span.timer'), fn = d.querySelector('.footnotes');
  slides.forEach((s, i) => {
    // append footnotes
    if (fn) s.querySelectorAll('.footnote-ref > a[href^="#fn"]').forEach(a => {
      const li = fn.querySelector('li' + a.getAttribute('href'));
      if (!li) return;
      let f = s.querySelector('section.footnotes');
      if (!f) {
        f = newEl('section', 'footnotes'); s.append(f);
      }
      f.append(li);
      li.firstElementChild?.insertAdjacentHTML('afterbegin', `[${a.innerHTML}] `);
      li.outerHTML = li.innerHTML;
    });
    // add a timer
    s.append(tm ? tm.cloneNode() : newEl('span', 'timer'));
    // add page numbers
    const n = newEl('span', 'page-number');
    n.innerText = i + 1 + '/' + N;
    n.onclick = e => location.hash = i + 1;
    s.append(n);
    // apply slide attributes in <!--# -->
    for (let k in s.childNodes) {
      const node = s.childNodes[k];
      if (node.nodeType !== Node.COMMENT_NODE) continue;
      let t = node.textContent;
      if (!/^#/.test(t)) continue;
      t = t.replace(/^#/, '');
      const r = /[\s\n]class="([^"]+)"/, m = t.match(r);
      if (m) {
        t = t.replace(r, '').trim();
        s.className += ' ' + m[1];
      }
      if (t) setAttr(s, t);
      break;
    }
    s.addEventListener('click', e => {
      e.altKey && (toggleView(e), reveal(e.target));
    });
  });
  [...d.querySelectorAll('a.footnote-backref'), fn, tm].forEach(el => el?.remove());
  const tms = d.querySelectorAll('span.timer'), t1 = 1000 * tms[0].dataset.total;
  let t0;
  function startTimers() {
    t0 = new Date();
    setInterval(setTimers, 1000);
  }
  function setTimers() {
    let t = (new Date() - t0);
    if (t1) t = t1 - t;
    const t2 = new Date(Math.abs(t)).toISOString().substr(11, 8).replace(/^00:/, '');
    tms.forEach(el => {
      el.innerText = t2;
      if (t < 0) el.style.display = el.style.display === 'none' ? '' : 'none';
    });
  }
  function toggleView(e) {
    (dC.toggle('overview') ? dC.remove('slide-mode') : setScale(e));
  }
  // press f for fullscreen mode
  d.addEventListener('keyup', e => {
    if (e.target !== d.body) return;
    e.key === 'f' && dE.requestFullscreen();
    e.key === 'o' && toggleView(e);
    e.key === 'm' && dC.toggle('mirrored');
    sessionStorage.setItem('body-class', d.body.className);
  });
  // start timer and set scale on fullscreen
  d.onfullscreenchange = e => {
    d.fullscreenElement && (!t0 && startTimers(), setScale(e));
  }
  tms.forEach(el => el.addEventListener('click', e => startTimers()));
  // measure the height of an empty slide
  let H = -1;
  function slideHeight() {
    if (H >= 0) return H;
    const s = newEl('div', 'slide');
    p.querySelector('.slide:last-of-type').after(s);
    H = s.offsetHeight;
    s.remove()
    return H;
  }
  // scale slides according to window height (baseline: 900px)
  const sty = newEl('style'); sty.setAttribute('type', 'text/css');
  d.head.append(sty);
  // default height/width ratio from screen size
  sty.innerHTML = `:root{--slide-ratio:${screen.height / screen.width}}`;
  // read --slide-ratio in case users have set it in their CSS
  const ratio = +getComputedStyle(dE).getPropertyValue('--slide-ratio');
  function setScale(e) {
    // navigate to a slide indicated by the hash if provided
    e.type === 'load' && reveal(slides[location.hash.replace(/^#/, '') - 1]);
    if (dC.contains('overview')) return;
    let h = window.innerHeight, p = h / 900, r2 = h / window.innerWidth, full = d.fullscreenElement;
    // add slide mode if there's enough window width, and remove it in case of scrollbar
    dC.toggle('slide-mode', full || r2 <= ratio) &&
      (!full && (dE.scrollWidth > dE.offsetWidth)) && dC.remove('slide-mode');
    sty.innerHTML = `:root{--slide-ratio:${ratio};--slide-scale:${p};--slide-top:${(p - 1)/2 * d.body.scrollHeight + 'px'};}`;
    // add spacers with enough height on load
    !d.querySelector('.spacer.fade') && dC.contains('slide-mode') && slides.forEach(s => {
      const sp = newEl('div', 'spacer fade'), h = s.offsetHeight, h2 = slideHeight();
      s.append(sp);
      if (h <= h2) return;
      sp.style.height = (h2 - h % h2) * p + 'px';
    });
  }
  ['load', 'resize'].forEach(evt => window.addEventListener(evt, setScale));
  // restore previsouly saved body class
  const bc = sessionStorage.getItem('body-class');
  if (bc) d.body.className += ' ' + bc;
})(document);
