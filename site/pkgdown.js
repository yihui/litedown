// pkgdown-style navbar: a brand (package name, from the GitHub link) with the
// version (from the footer) replaces the "Home" item of the site menu
(d => {
  const p = d.querySelector('.site-nav > p'), home = p?.querySelector('a[href]'),
    repo = p?.querySelector('a[href*="github.com/"]');
  if (!home) return;
  const brand = d.createElement('a');
  brand.href = home.getAttribute('href');
  brand.className = 'brand';
  brand.textContent = repo?.pathname.split('/').filter(Boolean).pop() || home.textContent;
  const v = d.querySelector('footer')?.textContent.match(/\bv(\d[\w.-]*)/);
  const ver = d.createElement('small');
  ver.className = 'version';
  ver.textContent = v?.[1] || '';
  home.replaceWith(brand, ver);
})(document);
