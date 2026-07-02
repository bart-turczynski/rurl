#!/usr/bin/env Rscript
# Standalone parsing benchmark for rurl. NOT run on CRAN and NOT a test -- it is
# a manual harness for the vector-core refactor (RURL-gvlqokul) so throughput
# regressions are visible. Run from the package root with either an installed
# rurl or a dev tree:
#
#   Rscript inst/bench/parse-bench.R              # uses installed rurl
#   Rscript -e 'devtools::load_all(); source("inst/bench/parse-bench.R")'
#
# Scenario: 20,000 URLs drawn from ~5,000 unique spellings (so the cache hit
# path dominates on the warm pass), measured cold (caches cleared) then warm
# (caches primed), plus a scalar-accessor loop (get_domain() one URL at a time)
# that exercises the per-call wrapper overhead.
#
# ----------------------------------------------------------------------------
# Baseline measured 2026-07-02 (Apple silicon, R 4.6.0, rurl dev tree @ T1):
#   vector cold  (20k / ~5k unique): ~3,100 URLs/s
#   vector warm  (20k, all cached):  ~34,700 URLs/s
#   scalar accessor loop (5k get_domain): ~790 URLs/s
#
# After T4 (RURL-ohepgzyf: unique()+match() de-dup + vector-level memoization):
#   vector cold  (20k / ~5k unique): ~29,000 URLs/s (only 5k unique are parsed)
#   vector warm  (20k, all cached):  ~250,000 URLs/s (>1M/s once JIT-warmed)
#   20k all-duplicates-of-100:       ~6,000,000 URLs/s (duplicates cost match())
#   scalar accessor loop unchanged (per-call wrapper overhead is T5's target).
# Update these numbers when the refactor lands so drift is reviewable.
# ----------------------------------------------------------------------------

if (!requireNamespace("rurl", quietly = TRUE) &&
    !exists("safe_parse_urls", mode = "function")) {
  stop("rurl not available: install it or run after devtools::load_all().")
}
if (exists("safe_parse_urls", mode = "function")) {
  spu <- get("safe_parse_urls")
  gd <- get("get_domain")
  clear_caches <- get("rurl_clear_caches")
} else {
  spu <- rurl::safe_parse_urls
  gd <- rurl::get_domain
  clear_caches <- rurl::rurl_clear_caches
}

# Build ~5,000 unique URL spellings spanning a spread of parser branches.
build_unique_urls <- function(n_unique = 5000L) {
  schemes <- c("http://", "https://", "//", "ftp://")
  subs <- c("", "www.", "shop.", "a.b.", "www2.")
  tlds <- c("com", "org", "net", "io", "co.uk", "de")
  paths <- c(
    "/", "/index.html", "/a//b/./c/../d", "/products/item?id=%s",
    "/search?q=%s&p=2#top", "/path%%2Fseg/%s"
  )
  urls <- character(0)
  i <- 0L
  while (length(urls) < n_unique) {
    scheme <- schemes[[(i %% length(schemes)) + 1L]]
    sub <- subs[[(i %% length(subs)) + 1L]]
    tld <- tlds[[(i %% length(tlds)) + 1L]]
    path <- paths[[(i %% length(paths)) + 1L]]
    host <- paste0(sub, "site", i, ".", tld)
    urls <- c(urls, paste0(scheme, host, sprintf(path, i)))
    i <- i + 1L
  }
  urls[seq_len(n_unique)]
}

timed_rate <- function(label, n, expr) {
  elapsed <- system.time(force(expr))[["elapsed"]]
  rate <- n / elapsed
  cat(sprintf(
    "%-34s %8d urls  %7.3fs  %10.0f urls/s\n", label, n, elapsed, rate
  ))
  invisible(rate)
}

set.seed(1L)
unique_urls <- build_unique_urls(5000L)
big <- sample(rep_len(unique_urls, 20000L))

cat("rurl parse-bench\n")
cat(sprintf(
  "corpus: %d total, %d unique\n\n", length(big), length(unique(big))
))

# Unknown-TLD / malformed spellings in the synthetic corpus emit parse
# warnings; they are expected here and only clutter the throughput output.
clear_caches()
timed_rate(
  "vector cold (caches cleared)", length(big), suppressWarnings(spu(big))
)
timed_rate(
  "vector warm (caches primed)", length(big), suppressWarnings(spu(big))
)

clear_caches()
timed_rate("scalar accessor loop (get_domain)", length(unique_urls), {
  suppressWarnings(for (u in unique_urls) gd(u))
})

invisible(NULL)
