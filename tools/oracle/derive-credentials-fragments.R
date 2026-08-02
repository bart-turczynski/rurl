#!/usr/bin/env Rscript
# RURL-ozdejfzl -- the `wpt-credentials-fragments` oracle, as tracked code.
#
# Ported from `_scratch/build-credential-fragment-vectors.R` (RURL-yeikpnan),
# which produced the 43 rows but was never in the repository: `_scratch/` is
# gitignored, so `oracle-provenance.json` recorded a `generation_command` that no
# clean checkout could run.
#
# WHAT THIS FILE IS. The DERIVATION only -- the oracle. It answers "what does the
# WHATWG URL Standard say the serialization of this input is", and it answers it
# WITHOUT LOADING rurl. That is not a convenience; it is the independence
# property the evidence rests on. An oracle that calls the implementation it
# grades is a characterization test wearing an oracle's label, so the split is
# enforced structurally here: this file cannot reach rurl's answer even by
# accident, because rurl is never on the search path.
#
# The characterization rurl's own columns record (`rurl_*_status`,
# `rurl_*_clean`) is a SEPARATE concern and is not computed here. In the scratch
# builder the two were interleaved in one pass over the same rows, which is what
# made the file impossible to read as either one thing or the other.
#
# THE ORACLE. Inputs and their parsed components come from the committed WPT
# import (`inst/bench/wpt-url-cases.json` -- web-platform-tests
# `urltestdata.json`, BSD-3-Clause). The expected href is assembled from those
# recorded components by a direct transcription of the WHATWG URL serializer
# (URL Standard section 4.5), sharing no code with rurl:
#
#   output = scheme ":"
#   if host is non-null:
#       "//" , [ username [ ":" password ] "@" if credentials are included ] ,
#       host , [ ":" port if non-null ]
#   append path, then [ "?" query ], then [ "#" fragment ]
#
# "Includes credentials" is username non-empty OR password non-empty -- which is
# why `http://@h/` serializes as `http://h/` and `http://u:@h/` as `http://u@h/`.
# Those two are the contested cases (P2.5 open question 1); they are carried as
# evidence rather than argued from first principles.
#
# SCOPE. Restricted to cases whose host is unambiguously present, so the "//"
# decision is never guessed: a special scheme with a non-empty hostname. The
# empty-delimiter rows (`?`/`#` with nothing after) are admitted separately --
# they are the ADR 0012 D2 present-but-empty distinction, and the whole reason
# the FSSS recovers `query_kind`/`fragment_kind` lexically.
#
# Sourced by `tools/oracle/verify-credentials-fragments.R`. Running it directly
# prints the derived block and writes nothing.

ORACLE_SPECIAL_SCHEMES <- c("http:", "https:", "ws:", "wss:", "ftp:", "file:")

# Derive the credentials/fragments oracle from a committed WPT import.
#
# Returns a data.frame of `input`, `href` (the section 4.5 serialization) and
# `kind` (which shape the row was admitted for), plus the import's `_meta` as an
# attribute so the caller can build `source_reference` without re-reading.
# No de-duplication is applied -- that is the assembling driver's job, because it
# depends on what the rest of the corpus already covers.
derive_credentials_fragments <- function(
    cases_path = "inst/bench/wpt-url-cases.json") {
  if (!file.exists(cases_path)) {
    stop("FATAL: WPT import not found: ", cases_path,
         "\n  This file is committed; a missing copy means a broken checkout, ",
         "not a fetch to perform.", call. = FALSE)
  }
  j <- jsonlite::fromJSON(cases_path, simplifyVector = FALSE)
  meta <- j[["_meta"]]
  if (is.null(meta) || is.null(meta$upstream_revision)) {
    stop("FATAL: ", cases_path, " carries no _meta.upstream_revision; the ",
         "oracle cannot state which upstream revision it derives from.",
         call. = FALSE)
  }
  if (is.null(j$success) || !length(j$success)) {
    stop("FATAL: ", cases_path, " has no `success` array to derive from.",
         call. = FALSE)
  }

  g <- function(x, k) {
    v <- x[[k]]
    if (is.null(v)) "" else v
  }

  rows <- list()
  for (x in j$success) {
    proto <- g(x, "protocol")
    host <- g(x, "hostname")
    user <- g(x, "username")
    pass <- g(x, "password")
    hash <- g(x, "hash")
    srch <- g(x, "search")
    inp <- g(x, "input")

    has_cred <- nzchar(user) || nzchar(pass)
    has_frag <- nzchar(hash) || grepl("#$", inp)
    has_eq <- grepl("[?]$", inp) || identical(srch, "?")
    # An "@" the parser resolved as an EMPTY userinfo is the contested case
    # (P2.5 open question 1): WHATWG "includes credentials" is false when both
    # halves are empty, so `http://@h/` serializes as `http://h/` -- the "@" is
    # dropped, delimiter and all. Those rows carry no credentials in the OUTPUT,
    # so `has_cred` misses them; they are the single most load-bearing shape
    # here and must not be filtered out. An "@" appearing in the path, query or
    # fragment is not an authority delimiter and does not qualify.
    path0 <- g(x, "pathname")
    empty_cred <- grepl("@", inp, fixed = TRUE) && !has_cred &&
      !grepl("@", path0, fixed = TRUE) && !grepl("@", srch, fixed = TRUE) &&
      !grepl("@", hash, fixed = TRUE)
    if (!(has_cred || empty_cred || has_frag || has_eq)) next
    # Unambiguous authority only: a special scheme with a non-empty host.
    if (!(proto %in% ORACLE_SPECIAL_SCHEMES) || !nzchar(host)) next

    # WHATWG URL serializer, section 4.5.
    cred <- ""
    if (has_cred) {
      cred <- user
      if (nzchar(pass)) cred <- paste0(cred, ":", pass)
      cred <- paste0(cred, "@")
    }
    # The URL API's `search`/`hash` are "" for BOTH absent and present-but-empty,
    # the same null-vs-empty collapse `.blank_to_na()` performs inside rurl -- so
    # the delimiters cannot be read off them and are recovered lexically from the
    # input, exactly as the FSSS recovers query_kind / fragment_kind (ADR 0012
    # D2). Reconstructing from `search` alone got `https://x/?#<U+FFFF>y` wrong
    # by dropping a present-but-empty query; the cross-check caught it.
    hash_at <- regexpr("#", inp, fixed = TRUE)
    body <- if (hash_at > 0L) substring(inp, 1L, hash_at - 1L) else inp
    q_present <- regexpr("?", body, fixed = TRUE) > 0L
    f_present <- hash_at > 0L

    port <- g(x, "port")
    href <- paste0(
      proto, "//", cred, host, if (nzchar(port)) paste0(":", port) else "",
      g(x, "pathname"),
      if (q_present) paste0("?", sub("^[?]", "", srch)) else "",
      if (f_present) paste0("#", sub("^#", "", hash)) else ""
    )
    shape <- c(
      if (has_cred) "credentials",
      if (empty_cred) "empty-credentials",
      if (f_present) "fragment",
      if (q_present && !nzchar(srch)) "empty-query"
    )
    rows[[length(rows) + 1L]] <- list(
      input = inp, href = href, kind = paste(shape, collapse = "+")
    )
  }

  if (!length(rows)) {
    stop("FATAL: the derivation admitted zero rows. The selector cannot be ",
         "correct -- fail closed rather than record an empty oracle.",
         call. = FALSE)
  }

  out <- data.frame(
    input = vapply(rows, function(r) r$input, character(1)),
    href = vapply(rows, function(r) r$href, character(1)),
    kind = vapply(rows, function(r) r$kind, character(1)),
    stringsAsFactors = FALSE
  )
  attr(out, "wpt_meta") <- meta
  out
}

# `source_reference` for the derived rows, naming the two-hop provenance the
# record describes: the upstream project/revision, and the fact that the href is
# assembled rather than quoted.
credentials_fragments_source_reference <- function(meta) {
  sprintf(
    paste0("web-platform-tests %s url/resources/urltestdata.json @ %s ",
           "(BSD-3-Clause); expected href assembled from the recorded ",
           "components by the WHATWG URL serializer (URL Standard sec 4.5)"),
    meta$upstream_project, substr(meta$upstream_revision, 1, 12)
  )
}

# Runs only when this file is executed as a script: `sys.nframe()` is 0 at the
# top level of an Rscript invocation and non-zero inside the verifier's
# `source()` call. The earlier form also tested `!is.null(sys.frames())`, which
# is FALSE at top level -- so the block never fired and the header's "running it
# directly prints ..." was untrue of all three modules.
if (sys.nframe() == 0L) {
  d <- derive_credentials_fragments()
  cat("derived rows:", nrow(d), "\n")
  print(table(d$kind))
}
