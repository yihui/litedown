#!/bin/sh

# Sync the vendored cmark-gfm sources under src/cmark and src/extensions with
# upstream github/cmark-gfm, then re-apply litedown's patches.
#
# Usage:
#   src/patches/sync.sh [REF]
#
# REF is a git ref (tag/branch/commit) of github/cmark-gfm. If omitted, the
# latest release tag (highest 0.29.0.gfm.N) is used.
#
# Run from the package root (the directory containing DESCRIPTION). The script
# is also run monthly by .github/workflows/sync-cmark.yaml, which opens a pull
# request when the vendored sources change.
#
# What it does:
#   1. Clones upstream at REF into a temp dir.
#   2. Copies the C sources into src/cmark and src/extensions (dropping main.c,
#      which is the CLI entry point litedown does not use).
#   3. Generates config.h and cmark-gfm_version.h, which upstream builds with
#      CMake (litedown does not use CMake).
#   4. Re-applies every *.diff listed in apply.sh.
# Files that are litedown's own (glue C, COPYING, the generated export header)
# are left untouched; see NOT_UPSTREAM below.

set -e

REPO="https://github.com/github/cmark-gfm"

# Resolve the package root from this script's location (src/patches/sync.sh).
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$script_dir/../.." && pwd)
patches="$root/src/patches"
cmark="$root/src/cmark"
ext="$root/src/extensions"

# Files under src/cmark that are NOT copied from upstream src/ (they are either
# litedown's own or generated below) and must be preserved across a sync.
NOT_UPSTREAM="COPYING cmark-gfm_export.h cmark-gfm_version.h config.h"

# Emit a CMake-style *_export.h header for a static build. Upstream generates
# these with CMake's generate_export_header(); litedown does not use CMake, so
# we emit the static variant (all macros empty, matching the *_STATIC_DEFINE
# flags in src/Makevars). $1 = macro prefix, $2 = output path.
gen_export_header() {
  p=$1
  cat > "$2" <<EOF

#ifndef ${p}_EXPORT_H
#define ${p}_EXPORT_H

#ifdef ${p}_STATIC_DEFINE
#  define ${p}_EXPORT
#  define ${p}_NO_EXPORT
#else
#  ifndef ${p}_EXPORT
#    define ${p}_EXPORT __attribute__((visibility("default")))
#  endif
#  ifndef ${p}_NO_EXPORT
#    define ${p}_NO_EXPORT __attribute__((visibility("hidden")))
#  endif
#endif

#ifndef ${p}_DEPRECATED
#  define ${p}_DEPRECATED __attribute__ ((__deprecated__))
#endif

#ifndef ${p}_DEPRECATED_EXPORT
#  define ${p}_DEPRECATED_EXPORT ${p}_EXPORT ${p}_DEPRECATED
#endif

#ifndef ${p}_DEPRECATED_NO_EXPORT
#  define ${p}_DEPRECATED_NO_EXPORT ${p}_NO_EXPORT ${p}_DEPRECATED
#endif

#if 0 /* DEFINE_NO_DEPRECATED */
#  ifndef ${p}_NO_DEPRECATED
#    define ${p}_NO_DEPRECATED
#  endif
#endif

#endif /* ${p}_EXPORT_H */
EOF
}

ref="$1"
if [ -z "$ref" ]; then
  echo "Looking up the latest cmark-gfm release tag..."
  ref=$(git ls-remote --tags --refs "$REPO" 'refs/tags/0.*' \
    | awk -F/ '{print $NF}' \
    | sort -t. -k1,1n -k2,2n -k3,3n -k5,5n \
    | tail -1)
  [ -n "$ref" ] || { echo "Could not determine latest tag" >&2; exit 1; }
fi
echo "Syncing with cmark-gfm $ref"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
git clone -q --depth 1 --branch "$ref" "$REPO" "$tmp/cmark-gfm"
up="$tmp/cmark-gfm"

# 1. Copy core sources into src/cmark (keep litedown's own / generated files).
for f in "$up"/src/*.c "$up"/src/*.h "$up"/src/*.inc; do
  [ -e "$f" ] || continue
  name=$(basename "$f")
  [ "$name" = "main.c" ] && continue                 # CLI entry point, unused
  case " $NOT_UPSTREAM " in *" $name "*) continue;; esac
  cp "$f" "$cmark/$name"
done

# 2. Copy extension sources into src/extensions.
for f in "$up"/extensions/*.c "$up"/extensions/*.h; do
  [ -e "$f" ] || continue
  cp "$f" "$ext/$(basename "$f")"
done

# 3. Generate config.h from config.h.in. litedown targets gcc/clang, where all
#    of the probed features are available, so every #cmakedefine becomes a plain
#    #define (this matches how the commonmark R package ships config.h).
sed 's/#cmakedefine/#define/' "$up/src/config.h.in" > "$cmark/config.h"

# 4. Generate cmark-gfm_version.h. Prefer the numbers in the tag name (upstream
#    has shipped release tags without bumping CMakeLists.txt, e.g. 0.29.0.gfm.9
#    still says gfm.6 there), and fall back to CMakeLists.txt for non-tag refs.
ver_field() { grep -E "set\\(PROJECT_VERSION_$1 " "$up/CMakeLists.txt" | grep -oE '[0-9]+'; }
tag_ver=$(printf '%s\n' "$ref" | grep -oE '^[0-9]+\.[0-9]+\.[0-9]+\.gfm\.[0-9]+$' || true)
if [ -n "$tag_ver" ]; then
  V_MAJOR=$(echo "$tag_ver" | cut -d. -f1); V_MINOR=$(echo "$tag_ver" | cut -d. -f2)
  V_PATCH=$(echo "$tag_ver" | cut -d. -f3); V_GFM=$(echo "$tag_ver" | cut -d. -f5)
else
  V_MAJOR=$(ver_field MAJOR); V_MINOR=$(ver_field MINOR)
  V_PATCH=$(ver_field PATCH); V_GFM=$(ver_field GFM)
fi
cat > "$cmark/cmark-gfm_version.h" <<EOF
#ifndef CMARK_GFM_VERSION_H
#define CMARK_GFM_VERSION_H

#define CMARK_GFM_VERSION ((${V_MAJOR} << 24) | (${V_MINOR} << 16) | (${V_PATCH} << 8) | ${V_GFM})
#define CMARK_GFM_VERSION_STRING "${V_MAJOR}.${V_MINOR}.${V_PATCH}.gfm.${V_GFM}"

#endif
EOF

# 5. Generate the CMake export headers (static variants).
gen_export_header CMARK_GFM "$cmark/cmark-gfm_export.h"
gen_export_header CMARK_GFM_EXTENSIONS "$ext/cmark-gfm-extensions_export.h"

# 6. Re-apply litedown's patches.
echo "Applying patches..."
( cd "$patches" && sh ./apply.sh )

echo "Synced to cmark-gfm ${V_MAJOR}.${V_MINOR}.${V_PATCH}.gfm.${V_GFM} (ref $ref)."
echo "Remember to build and run the tests, and update src/patches/README.md if"
echo "the upstream version changed or a patch no longer applies."
