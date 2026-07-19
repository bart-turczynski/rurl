# Parameterized libcurl-probe image (RURL-ttrcfneq).
#
# One Dockerfile, many base images: the ONLY thing that varies across the
# matrix is the distro, and with it the libcurl the distro links against.
#
# The R `curl` package is PINNED to one source version and built from CRAN in
# every image, deliberately NOT taken from the distro (`r-cran-curl`):
#   * every distro package predating curl 6.0.0 lacks curl_parse_url()
#     entirely -- the distro packages here range 4.3 to 6.2.1, and only the
#     newest one has the function at all;
#   * pinning it removes the R-curl-package version as a confound, leaving
#     libcurl as the only variable that moves across the matrix.
# 6.2.1 is chosen because it builds unmodified on R 3.6.3 (ubuntu:20.04) and
# on R 4.5 (debian:13). R itself still comes from the distro and still varies;
# it does not implement URL parsing, and it is recorded per label.
#
#   docker build -f Dockerfile --build-arg BASE_IMAGE=ubuntu:22.04 .
#
# Built by run-matrix.sh; see that script for the image list.

ARG BASE_IMAGE=ubuntu:24.04
FROM ${BASE_IMAGE}

ARG APT_PRE=true
ARG CURL_PKG_VERSION=6.2.1
ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# APT_PRE rewrites the archive host for EOL releases (see run-matrix.sh).
RUN sh -c "${APT_PRE}" \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
         r-base-core build-essential libcurl4-openssl-dev ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Pinned CRAN source tarball: no repo resolution, no snapshot service, no
# secrets -- one https GET that is reproducible for as long as the archive
# stands. It links against whatever libcurl the base image provides, which is
# exactly the variable under study.
RUN Rscript -e "install.packages(sprintf('https://cran.r-project.org/src/contrib/Archive/curl/curl_%s.tar.gz', '${CURL_PKG_VERSION}'), repos = NULL, type = 'source')" \
    && Rscript -e "stopifnot(is.function(curl::curl_parse_url))"

WORKDIR /probe
COPY corpus.csv /probe/corpus.csv
COPY curl-probe.R /probe/curl-probe.R
COPY entrypoint.sh /probe/entrypoint.sh
RUN chmod +x /probe/entrypoint.sh && mkdir -p /out

ENV RURL_DETERMINISM_OUT=/out
ENTRYPOINT ["/probe/entrypoint.sh"]
