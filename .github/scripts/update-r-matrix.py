#!/usr/bin/env python3
"""Rewrite the R versions in the matrix of R-CMD-check.yaml.

Each R version goes to the newest GitHub-hosted Ubuntu runner that has a build
of it according to r-hub, because old R versions are dropped from new Ubuntu releases.
Versions that work on the newest runner (called ubuntu-latest) are listed in `r:`, and the rest are added
as `include:` entries between the BEGIN/END markers with an older runner.
"""
import json
import re
import urllib.error
import urllib.request

WORKFLOW = '.github/workflows/R-CMD-check.yaml'
API = 'https://api.r-hub.io/rversions'
OLDEST = (3, 2)
# the list of GitHub-hosted runner images is documented in this README
RUNNERS = 'https://raw.githubusercontent.com/actions/runner-images/main/README.md'


def get(path):
  with urllib.request.urlopen(f'{API}/{path}', timeout=60) as r:
    return json.load(r)


def minor(version):
  return tuple(int(x) for x in version.split('.')[:2])


def available(version, ubuntu):
  """Whether r-hub has a build of the R version for the Ubuntu version."""
  try:
    get(f'resolve/{version}/linux-ubuntu-{ubuntu}/x86_64')
    return True
  except urllib.error.HTTPError as e:
    if e.code == 404: return False
    raise


def ubuntu_runners():
  """Ubuntu versions of x64 GitHub-hosted runners, newest first."""
  with urllib.request.urlopen(RUNNERS, timeout=60) as r:
    readme = r.read().decode()
  vers = set(re.findall(r'`ubuntu-(\d+\.\d+)`', readme))
  return sorted(vers, key=lambda v: tuple(map(int, v.split('.'))),
                reverse=True)


candidates = ubuntu_runners()
latest = candidates[0]

release = minor(get('r-release')['version'])
versions = {
  minor(x['version']) for x in get('r-versions')
  if re.fullmatch(r'\d+\.\d+(\.\d+)?', x['version'])
}
versions = sorted((v for v in versions if v >= OLDEST and v != release),
                  reverse=True)
labels = ['.'.join(map(str, v)) for v in versions]
labels.append('.'.join(map(str, OLDEST)) + '.0')  # the minimal supported R

# map each version to the runner it works on
runner = {}
for v in labels:
  for u in candidates:
    if available(v, u):
      runner[v] = 'ubuntu-latest' if u == latest else f'ubuntu-{u}'
      break
  else:
    raise SystemExit(f'R {v} is not available on any of: {candidates}')

on_latest = ['release'] + [v for v in labels if runner[v] == 'ubuntu-latest']
r_line = '        r: [' + ', '.join(f"'{v}'" for v in on_latest + ['devel']) + ']'
include = ''.join(
  f"          - os: {runner[v]}\n            r: '{v}'\n"
  for v in labels if runner[v] != 'ubuntu-latest'
)

text = open(WORKFLOW).read()
text, n1 = re.subn(r'^        r: \[.*\]$', lambda m: r_line, text, flags=re.M)
text, n2 = re.subn(
  r'(^ *# BEGIN generated[^\n]*\n).*?(^ *# END generated)',
  lambda m: m.group(1) + include + m.group(2), text, flags=re.M | re.S
)
if n1 != 1 or n2 != 1: raise SystemExit('Failed to find the matrix in workflow')
open(WORKFLOW, 'w').write(text)
