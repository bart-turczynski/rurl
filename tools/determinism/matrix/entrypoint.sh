#!/bin/sh
# Container entrypoint: label the run with the libcurl version it actually got,
# then probe. RURL_DETERMINISM_LABEL_PREFIX names the image (e.g. ubuntu2204);
# the libcurl version is discovered at runtime, not assumed at build time.
set -e

prefix="${RURL_DETERMINISM_LABEL_PREFIX:-unknown}"
ver=$(Rscript -e 'cat(as.character(curl::curl_version()$version))')
RURL_DETERMINISM_LABEL="${prefix}-libcurl${ver}"
export RURL_DETERMINISM_LABEL

echo "== ${RURL_DETERMINISM_LABEL} =="
Rscript --version
Rscript -e 'cat("R curl package:", as.character(packageVersion("curl")), "\n")'
exec Rscript /probe/curl-probe.R
