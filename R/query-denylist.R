# Built-in tracking-parameter denylist and the pluggable denylist-source seam.
#
# The denylist is the product surface of query_handling = "filter": the param
# names (and utm_*-style globs) whose presence adds no page content and that
# filter mode drops from clean_url. v1 is intentionally SMALL and OBVIOUS -- the
# universally-recognized trackers (Google/Meta/Microsoft/... click IDs, utm_*,
# and household-name email-marketing tokens). The ambiguous / vendor-specific
# long tail is deliberately out; users extend per call with `params_drop`.
#
# License posture: this is an AUTHORED compilation. Individual parameter *names*
# are facts (Feist Publications v. Rural Telephone, 499 U.S. 340) and not
# copyrightable; the selection here is rurl's own, seeded from the MIT-licensed
# jparise/chrome-utm-stripper and general domain knowledge, gap-checked (read,
# not copied) against AdGuard's GPL general_url.txt. rurl is MIT + file LICENSE;
# no copyleft list is bundled.
#
# --- Pluggable backend seam --------------------------------------------------
# The filter engine NEVER hardcodes this list: ._filter_query_params() drops
# against a pattern set obtained by name from .resolve_query_denylist(). Sources
# live in .QUERY_DENYLIST_REGISTRY (populated in .onLoad, mirroring
# .CACHE_REGISTRY) and each maps a name to a *provider* -- either a character
# vector of glob patterns or a zero-argument function returning one (lazy, for a
# future externally-maintained list shipped as a sister data package: the pslr
# move for Brave / AdGuard / ClearURLs). Adding such a backend is: register a
# new source name in .onLoad; ._filter_query_params() is untouched.

# The v1 built-in "obvious trackers" source. A flat glob vector: an entry
# without '*' is an exact name (case-insensitive by default); utm_* is the sole
# glob (its family -- source/medium/campaign/term/content/... -- is unbounded
# and unambiguous). Matching is on the DECODED param name.
.QUERY_DENYLIST_BUILTIN <- c(
  # Google Analytics campaign params (utm_source/medium/campaign/content/...).
  "utm_*",
  # Ad / click identifiers.
  "gclid", "gclsrc", "dclid", "gbraid", "wbraid", "gad_source", "_gl", # Google
  "fbclid",  # Meta / Facebook
  "msclkid", # Microsoft / Bing
  "twclid",  # Twitter / X
  "ttclid",  # TikTok
  "yclid",   # Yandex
  "igshid",  # Instagram
  # Email / marketing automation (household-name vendors).
  "mc_cid", "mc_eid", # Mailchimp
  "_hsenc", "_hsmi",  # HubSpot
  "mkt_tok"           # Marketo
)

# Registry of denylist sources: name -> provider. Populated in .onLoad(); a
# future backend package registers additional named sources the same way.
.QUERY_DENYLIST_REGISTRY <- new.env(parent = emptyenv())

# Register a denylist source. `provider` is a character vector of glob patterns
# or a zero-argument function returning one. Called from .onLoad for "builtin";
# it is the seam a future backend uses to add "brave" / "adguard" / ... sources.
.register_query_denylist <- function(name, provider) {
  assign(name, provider, envir = .QUERY_DENYLIST_REGISTRY)
  invisible(NULL)
}

# Resolve a denylist source name to its character vector of glob patterns.
# A provider stored as a function is called once to materialize its patterns, so
# an external backend can defer loading its list until first use. NULL or an
# empty source resolves to no patterns; an unregistered name is an error (a
# typo'd source must fail loudly, not silently drop nothing).
.resolve_query_denylist <- function(source = "builtin") {
  if (is.null(source) || length(source) == 0L) {
    return(character(0))
  }
  if (!exists(source, envir = .QUERY_DENYLIST_REGISTRY, inherits = FALSE)) {
    stop(
      sprintf("Unknown query denylist source: '%s'.", source),
      call. = FALSE
    )
  }
  provider <- get(source, envir = .QUERY_DENYLIST_REGISTRY, inherits = FALSE)
  if (is.function(provider)) {
    provider <- provider()
  }
  as.character(provider)
}
