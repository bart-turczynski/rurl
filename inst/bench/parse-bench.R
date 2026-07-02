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
#
# After T7 (RURL-dkwrebdt: parse/present split -- full_parse caches Stage A):
#   Multi-profile scenario, 20k URLs / ~18k unique, cold caches, 5 accessors
#   each with a DIFFERENT presentation profile (Apple silicon, R 4.6.0):
#     before (main):  single parse 5.65s | 5 accessors 80.2s (14.2x the floor)
#     after  (T7):    single parse 7.09s | 5 accessors  6.3s (0.89x the floor)
#   -> ~12.7x faster on the multi-profile workload; the accessors now add no
#   core cost beyond a single parse (they share one Stage A cache entry per
#   URL and re-run only Stage B). The single-parse floor is ~25% slower because
#   Stage A derives the domain/TLD in both spellings so host_encoding stays a
#   Stage-B choice; this is paid once and amortized across every profile.
# Update these numbers when the refactor lands so drift is reviewable.
# ----------------------------------------------------------------------------

if (!requireNamespace("rurl", quietly = TRUE) &&
    !exists("safe_parse_urls", mode = "function")) {
  stop("rurl not available: install it or run after devtools::load_all().")
}
if (exists("safe_parse_urls", mode = "function")) {
  spu <- get("safe_parse_urls")
  gd <- get("get_domain")
  gh <- get("get_host")
  gt <- get("get_tld")
  gc <- get("get_clean_url")
  gs <- get("get_subdomain")
  clear_caches <- get("rurl_clear_caches")
} else {
  spu <- rurl::safe_parse_urls
  gd <- rurl::get_domain
  gh <- rurl::get_host
  gt <- rurl::get_tld
  gc <- rurl::get_clean_url
  gs <- rurl::get_subdomain
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

# Multi-profile accessor workload (RURL-dkwrebdt): the rurl-mcp pattern -- call
# several accessors, each with a DIFFERENT presentation profile, on the same
# URLs. Before the parse/present split each profile produced a different
# full_parse key, so every accessor re-ran the whole parse core (curl + PSL).
# After the split, all five share ONE Stage A cache entry per URL (they differ
# only in Stage-B presentation: case / host_encoding / trailing slash), so the
# core is computed once and the floor is a single safe_parse_urls() cold pass.
five_profiles_cold <- function(urls) {
  gh(urls, host_encoding = "unicode")
  gd(urls, host_encoding = "idna")
  gt(urls)
  gc(urls, case_handling = "upper", trailing_slash_handling = "strip")
  gs(urls)
  invisible(NULL)
}

cat("\nmulti-profile (5 accessors, distinct profiles, same URLs):\n")
clear_caches()
floor_rate <- timed_rate(
  "single safe_parse_urls cold (floor)", length(big),
  suppressWarnings(spu(big))
)
clear_caches()
five_rate <- timed_rate(
  "5 accessors cold (different profiles)", length(big),
  suppressWarnings(five_profiles_cold(big))
)
cat(sprintf(
  "  -> 5-accessor cold vs single-parse floor: %.2fx of floor time",
  floor_rate / five_rate
))
cat(" (1.0x == accessors add no core cost beyond one parse)\n")

invisible(NULL)
