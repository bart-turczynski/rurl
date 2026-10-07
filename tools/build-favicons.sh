#!/usr/bin/env bash
#
# Build pkgdown/favicon/ from man/figures/logo.svg, offline (RURL-ujczpkqm).
#
# WHY THIS EXISTS. pkgdown's init_site() calls build_favicons() whenever the
# package has a logo, pkgdown/favicon/ is missing and the CI env var is unset.
# build_favicons() uploads the logo to realfavicongenerator.net. GitLab sets
# CI=true, so the `pages` job skipped the call and the live site shipped no
# favicons; `tools/local-ci.sh` sets no CI, so its `pages` replay called the
# API and went red whenever the API did. Committing the set fixes both. This
# script makes that set locally instead of from the API, so regenerating it
# after a logo change needs no third-party service either.
#
# OUTPUT. The files pkgdown's BS5 head.html links (favicon-96x96.png,
# favicon.svg, apple-touch-icon.png, favicon.ico, site.webmanifest), plus the
# two icons the manifest names. Same names as build_favicons() writes, so
# pkgdown needs no configuration.
#
# Run from the repository root after changing the logo, then commit the
# result. Needs rsvg-convert (librsvg) and ImageMagick 7 (`magick`); neither
# is in the CI image, and neither needs to be, because CI only copies the
# committed files.

set -euo pipefail

logo=man/figures/logo.svg
out=pkgdown/favicon

for tool in rsvg-convert magick; do
  command -v "$tool" >/dev/null || { echo "$tool is required" >&2; exit 2; }
done
[ -f "$logo" ] || { echo "run from the repository root: $logo not found" >&2; exit 2; }

# The logo is a 1732 x 2000 hexagon. A favicon is square, so center it on a
# 2000 x 2000 canvas: (2000 - 1732) / 2 = 134 units of transparent margin on
# each side. Keep only the drawing (the title and the <path> elements) and
# drop the ~10 KB of XMP/RDF metadata, which every page would otherwise load.
read -r w h < <(sed -n 's/.*viewBox="0 0 \([0-9]*\) \([0-9]*\)".*/\1 \2/p' "$logo" | head -1)
[ "${w:-}" = 1732 ] && [ "${h:-}" = 2000 ] || {
  echo "expected a 0 0 1732 2000 viewBox in $logo, got '${w:-} ${h:-}': recheck the square padding" >&2
  exit 1
}
paths=$(grep '^<path ' "$logo") || { echo "no <path> elements in $logo" >&2; exit 1; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

{
  echo '<svg xmlns="http://www.w3.org/2000/svg" viewBox="-134 0 2000 2000">'
  echo '<title>rurl</title>'
  echo "$paths"
  echo '</svg>'
} >"$tmp/favicon.svg"

png() { rsvg-convert --width "$1" --height "$1" -o "$2" "$tmp/favicon.svg"; }

png 96 "$tmp/favicon-96x96.png"
png 192 "$tmp/web-app-manifest-192x192.png"
png 512 "$tmp/web-app-manifest-512x512.png"
for s in 16 32 48; do png "$s" "$tmp/ico-$s.png"; done

# iOS paints a transparent apple-touch-icon's background black, which swallows
# a black hexagon, so flatten it onto white.
png 180 "$tmp/apple-touch.png"
magick "$tmp/apple-touch.png" -background white -flatten -strip \
  "$tmp/apple-touch-icon.png"

magick "$tmp/ico-16.png" "$tmp/ico-32.png" "$tmp/ico-48.png" "$tmp/favicon.ico"

# Icon paths are relative to the manifest, so they resolve under the site's
# /rurl/ prefix on GitLab Pages.
cat >"$tmp/site.webmanifest" <<'EOF'
{
  "name": "rurl",
  "short_name": "rurl",
  "icons": [
    {"src": "web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png", "purpose": "any"},
    {"src": "web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png", "purpose": "any"}
  ],
  "theme_color": "#ffffff",
  "background_color": "#ffffff",
  "display": "standalone"
}
EOF

rm -rf "$out"
mkdir -p "$out"
cp "$tmp"/favicon.svg "$tmp"/favicon.ico "$tmp"/favicon-96x96.png \
  "$tmp"/apple-touch-icon.png "$tmp"/web-app-manifest-*.png \
  "$tmp"/site.webmanifest "$out"/
ls -l "$out"
