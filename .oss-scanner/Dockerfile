# Build for Anthropic's OSS Scanner (https://github.com/anthropics/oss-scanner).
# The scanner builds this with the repository as the context, then audits the
# image with no network, so everything the tests need is installed here.
# rurl is pure R (no src/), so no compiler flags are set.
FROM rocker/r-ver:4.6.1

# scanner contract: the checkout lives inside the image, at /src
COPY . /src
WORKDIR /src

# Imports and Suggests, with the system libraries they need. DESCRIPTION has
# no Remotes: every dependency comes from the CRAN snapshot rocker pins.
RUN Rscript -e 'install.packages("pak")' \
 && Rscript -e 'pak::local_install_deps("/src", dependencies = TRUE, upgrade = FALSE)'

RUN R CMD INSTALL /src

# Smoke-test the image; failures are logged, not fatal. The two dependency
# audits (test-osv.R, test-security.R) skip without a network.
RUN Rscript -e 'testthat::test_local("/src", stop_on_failure = TRUE)' \
 || echo "WARNING: tests reported failures"
