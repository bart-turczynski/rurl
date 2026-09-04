# Package-wide utilities and global-variable declarations.

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# --- byte-indexed string slicing (RURL-kmpnbvdl) -----------------------------
#
# Authority seams must cut by POSITION on BYTES. `substring()`/`substr()` index
# NATIVE characters and THROW `invalid multibyte string` on a string that is
# DECLARED UTF-8 but holds invalid octets -- and that is not a hypothetical
# input here: `stringi::stri_match_first_regex()` marks every capture UTF-8
# unconditionally, so slicing an authority captured out of `http://<80>/p`
# aborted the whole vectorized call, losing every good row in the same batch.
#
# Byte indexing also removes the second half of the problem: `gregexpr()` and
# `stri_locate_*()` report positions in different units from `substring()` once
# a string is multi-byte and unmarked, so a position taken from one and applied
# to the other mis-slices under `LC_ALL=C`. Locating and cutting in the SAME
# unit -- bytes -- is invariant by construction.
#
# The encoding DECLARATION is carried across the cut rather than recomputed:
# these helpers move bytes, they do not reinterpret them.

# 1-based byte index of the last occurrence of a single-byte literal `ch` in
# `s`, or 0L when absent. `useBytes = TRUE` also suppresses the "input string
# is invalid UTF-8" warning that the same scan emits without it.
.last_byte_index <- function(s, ch) {
  pos <- gregexpr(ch, s, fixed = TRUE, useBytes = TRUE)[[1L]]
  if (pos[1L] == -1L) 0L else as.integer(pos[length(pos)])
}

# 1-based byte index of the FIRST occurrence of `ch` in `s`, or 0L when absent.
.first_byte_index <- function(s, ch) {
  pos <- regexpr(ch, s, fixed = TRUE, useBytes = TRUE)[1L]
  if (pos == -1L) 0L else as.integer(pos)
}

# `substring()` on BYTES: the bytes of `s` from byte `first` to byte `last`
# inclusive, keeping the input's encoding declaration. Out-of-range indices
# clamp to "" exactly as `substring()` does, and `first = NA` yields NA as it
# does there too.
#
# ONE deliberate difference: `last = NA` means "to the end of `s`" rather than
# `substring()`'s NA, because it is this function's default argument -- callers
# say `.byte_substring(s, i)` to take a suffix.
.byte_substring <- function(s, first, last = NA_integer_) {
  first <- as.integer(first)
  if (is.na(first)) {
    return(NA_character_)
  }
  b <- charToRaw(s)
  n <- length(b)
  if (is.na(last)) {
    last <- n
  }
  first <- max(first, 1L)
  last <- min(as.integer(last), n)
  out <- if (first > last) "" else rawToChar(b[first:last])
  Encoding(out) <- Encoding(s)
  out
}

# Vector form of `.byte_substring()`. `first`/`last` recycle to `length(s)`;
# NA input stays NA. Pure-ASCII elements keep the vectorized `substring()` C
# path -- byte and character indices coincide there, so it is exact -- and only
# the multi-byte or undecodable elements pay for the per-element cut.
.byte_substring_vec <- function(s, first, last = NA_integer_) {
  n <- length(s)
  first <- rep_len(as.integer(first), n)
  last <- rep_len(as.integer(last), n)
  out <- rep(NA_character_, n)
  present <- !is.na(s)
  ascii <- present & !grepl("[^\001-\177]", s, perl = TRUE, useBytes = TRUE)
  if (any(ascii)) {
    stop_at <- last[ascii]
    stop_at[is.na(stop_at)] <- .Machine$integer.max
    out[ascii] <- substring(s[ascii], first[ascii], stop_at)
  }
  for (i in which(present & !ascii)) {
    out[i] <- .byte_substring(s[i], first[i], last[i])
  }
  out
}

# Octet count of each element, NA where the element is NA. `nchar(type =
# "bytes")` counts without decoding, so unlike `stringi::stri_length()` -- which
# THROWS `invalid UTF-8 byte sequence detected` -- it is total over declared-
# UTF-8 elements holding invalid octets. Use it whenever a length feeds a
# `.byte_substring*()` offset, so the measuring and the cutting share one unit.
.byte_length <- function(s) {
  out <- rep(NA_integer_, length(s))
  ok <- !is.na(s)
  out[ok] <- nchar(s[ok], type = "bytes")
  out
}

# 1-based BYTE index of the first occurrence of `ch` in each element of `s`, or
# 0L where absent. Vector form of `.first_byte_index()`.
.first_byte_index_vec <- function(s, ch) {
  pos <- regexpr(ch, s, fixed = TRUE, useBytes = TRUE)
  pos[is.na(pos) | pos < 0L] <- 0L
  as.integer(pos)
}

# `grepl()` on an UNDECODABLE element: FALSE, quietly and in every locale.
#
# A host sliced out of a `stri_match_first_regex()` capture is DECLARED UTF-8
# whatever its octets. `grepl(perl = TRUE)` on such a string warns
# ("input string 1 is invalid UTF-8") and yields NA -- which every caller here
# already folds to FALSE. Skipping those rows keeps that answer and drops the
# warning, and it also removes a latent locale dependency: on an "unknown"-
# marked element the same call yields NA under a UTF-8 locale but MATCHES ON
# BYTES under `LC_ALL=C`, so the fold was only accidentally agreeing.
#
# Deliberately NOT `useBytes = TRUE`, which would be the obvious-looking fix
# and is the wrong one: it would let an ASCII pattern match INSIDE an
# undecodable host (`%41` in a host that also holds a stray `<80>`), routing
# that row into percent-decoding and past a rejection it currently gets. At a
# host seam that is an acceptance WIDENING, which is the one thing this seam
# must never do silently.
#
# "latin1" is exempt because every octet sequence is valid latin1: those
# elements decode, so they match normally.
.grepl_decodable <- function(pattern, x, ...) {
  out <- rep(FALSE, length(x))
  ok <- !is.na(x) & (validUTF8(x) | Encoding(x) == "latin1")
  if (any(ok)) {
    out[ok] <- grepl(pattern, x[ok], ...)
  }
  out
}

# `gsub()` on an UNDECODABLE element: returned BYTE-IDENTICAL, quietly and in
# every locale. The substitution counterpart of `.grepl_decodable()`, and needed
# for the same reason -- `gsub()` THROWS `input string 1 is invalid UTF-8` on a
# declared-UTF-8 element holding invalid octets, with `fixed = TRUE` no less
# than `perl = TRUE`, and one such element aborts the whole vectorized call.
#
# Leaving the bytes alone rather than substituting on them is the conservative
# half of the choice, and it is deliberate. `useBytes = TRUE` would also stop
# the throw, and for an ASCII-only pattern it is even byte-exact (UTF-8 is
# self-synchronizing, so an ASCII octet never occurs inside a multi-byte
# sequence) -- but it REWRITES an undecodable element, and every caller here
# feeds its output back into masks that later decide acceptance. An undecodable
# row is already bound for rejection; returning it unchanged keeps it there,
# whereas rewriting it can only change what a downstream predicate sees. Same
# rule as the host-charset greps: at these seams, do not touch what you cannot
# read.
.gsub_decodable <- function(pattern, replacement, x, ...) {
  out <- x
  ok <- !is.na(x) & (validUTF8(x) | Encoding(x) == "latin1")
  if (any(ok)) {
    out[ok] <- gsub(pattern, replacement, x[ok], ...)
  }
  out
}

# Single source of truth for the WHATWG standard-posture toggle (ADR 0012
# Layer 0, RURL-dztjlfkc). Every posture-dependent branch tests membership in
# the WHATWG profile through this predicate instead of an inline
# `identical(url_standard, "whatwg")`, so later layers add posture-dependent
# behavior in one place rather than scattering more identity checks. Returns a
# length-1 logical -- identical truthiness to the inline check it replaces, so
# it composes unchanged in negations and scalar-vs-vector `&`/`&&`/`||` uses.
.is_whatwg <- function(url_standard) {
  identical(url_standard, "whatwg")
}

# The URL schemes rurl supports. rurl's domain is authority-based (hierarchical)
# URLs. http/https/ftp/ftps carry "scheme://host[:port]/path"; file is the
# one supported hostless hierarchical scheme. In the WHATWG profile, `file:`
# has a small parser slice for drive-letter, host, and backslash state-machine
# forms; default/RFC behavior remains limited to the plain local forms.
# ftps is FTP-over-TLS (the https-analogue for ftp), not the unrelated
# SSH-based sftp.
# This is the single source of truth: a scheme-bearing input whose scheme is not
# here is rejected (opaque schemes like mailto:/tel:/data:, and unrecognized
# schemes like ws:/ssh:/typos).
.SUPPORTED_SCHEMES <- c("http", "https", "ftp", "ftps", "file")

# The WHATWG "special scheme" set (PRD v2 D7, RURL-jlvyjwog; file added by
# RURL-rutsdflg; ws/wss added by RURL-qluqkdwl / ADR 0012 Layer 1). WHATWG
# defines http/https/ftp/ws/wss/file as special, and all six are registered
# here as WHATWG-special metadata. ws/wss are DELIBERATELY kept OUT of
# .SUPPORTED_SCHEMES (the default-acceptance allowlist), so this metadata is
# inert: ws/wss inputs still hard-error at the prefix gate, exactly like any
# other unsupported scheme, until the Layer 2 acceptance axis exposes
# scheme_acceptance = "general". ftp**s** is rurl's own addition (FTP-over-TLS)
# and is NOT a WHATWG special scheme.
.WHATWG_SPECIAL_SCHEMES <- c("http", "https", "ftp", "ws", "wss", "file")

# Special schemes whose WHATWG no-slash authority recovery is shared by this
# parser's authority-based URL model. `file` has a separate state machine and is
# intentionally left to the existing file:// slice. ws/wss are registered here
# too (RURL-qluqkdwl / ADR 0012 Layer 1) but stay inert -- absent from
# .SUPPORTED_SCHEMES, no ws/wss input reaches this recovery path under the
# default acceptance allowlist.
.SPECIAL_AUTHORITY_SCHEMES <- c("http", "https", "ftp", "ws", "wss")

# WHATWG forbidden host/domain code points (RURL-jfuqpwvh) that can survive to a
# resolved reg-name host and must fail the host parse under url_standard =
# "whatwg". An ICU regex character class covering: C0 controls U+0001-U+001F and
# DEL U+007F, and the printable forbidden bytes space # % / : < > ? @ [ ] \ ^ |.
# NUL (U+0000) is excluded (an R string cannot hold it); tab/LF/CR are already
# stripped upstream (.strip_whatwg_control_chars_vec, RURL-tyetpjym). Hyphen and
# underscore are LEGAL and deliberately absent. Applied only to non-IP hosts
# (IPv6 literals legitimately contain "[" "]" ":"). See ADR 0004/0007.
.WHATWG_FORBIDDEN_HOST_CP <- "[\\u0001-\\u001f\\u007f #%/:<>?@\\[\\]\\\\^|]"

# WHATWG forbidden HOST code points (ADR 0012 L4b, RURL-yutinyhb). CORRECTNESS
# TRAP: this is the OPAQUE-HOST set, deliberately DISTINCT from the stricter
# forbidden-DOMAIN set above (`.WHATWG_FORBIDDEN_HOST_CP`). The domain set also
# forbids `%`, all C0 controls (U+0001-U+001F), and DEL (U+007F); an opaque host
# does NOT forbid those -- `%` is legal (and malformed `%` is a validation-error
# fact for L5, never a parse failure), C0/DEL are percent-encoded by the C0
# encoder rather than rejected, and NUL cannot occur in an R string. WHATWG's
# full 17-member forbidden-host set is NUL TAB LF CR SP # / : < > ? @ [ \ ] ^ |;
# TAB/LF/CR are already stripped upstream (.strip_whatwg_control_chars_vec) and
# NUL is unrepresentable, so the effective reject class is the 13 printable code
# points space # / : < > ? @ [ \ ] ^ | (backslash IS forbidden -- it is only
# rewritten to `/` for SPECIAL schemes, never for the non-special/opaque hosts
# this set gates). Applied ONLY to non-bracketed opaque hosts: a bracketed
# IPv6 literal legitimately carries `[` `]` `:` and is checked separately.
.WHATWG_FORBIDDEN_HOST_ONLY_CP <- "[\\u0020#/:<>?@\\[\\]\\\\^|]"

# The two host-charset code point classes that lived here -- the 15 WHATWG gap
# code points (RURL-dxwxeamq, ADR 0009) and the 11 RFC 3986 reg-name sub-delims
# (RURL-dnddogce) -- are gone with the pre-parse shim that consumed them
# (RURL-ezhzpkhg deletion 1, ADR 0013). They now exist as BYTE sets in the
# parser that judges them, `.WEB_HOST_GAP_BYTES` and
# `.WEB_HOST_SUBDELIM_BYTES` in R/parse-web.R. The change of unit is the point:
# an ICU class only matches a string stringi will accept, and a host token may
# be declared UTF-8 while holding invalid octets (RURL-kmpnbvdl).

# Default ports for rurl's WHATWG-special schemes (PRD v2 D1, RURL-qdlvldts;
# ws/wss added by RURL-qluqkdwl / ADR 0012 Layer 1). WHATWG defines defaults
# for http/https/ftp/ws/wss (80/443/21/80/443), all registered here. ws/wss are
# inert: absent from .SUPPORTED_SCHEMES, no ws/wss row survives to consult this
# table until the Layer 2 acceptance axis lands. ftps (rurl's own FTP-over-TLS
# addition, not a WHATWG special scheme per D2) has none, so a port on ftps
# never matches this table -- it is never elided under port_handling = "keep"
# and always registers as the non-default-port diagnostic fact, regardless of
# url_standard.
.SCHEME_DEFAULT_PORTS <- c(
  http = 80L, https = 443L, ftp = 21L, ws = 80L, wss = 443L
)

# Bare single-label hosts (no dot) accepted from scheme-less input. Only
# `localhost` is a genuine resolvable single-label host; other RFC 6761/2606
# reserved names are dotted suffixes handled by the normal path (.onion/.arpa
# are in the PSL; .local/.test/.invalid/.example -> warning-invalid-tld).
.SPECIAL_SINGLE_LABEL_HOSTS <- "localhost"

# Convert a glob to an anchored regex where ONLY '*' is special (matches any
# run of characters). Every other character -- including '.', '?', '[' -- is
# escaped to a literal, so a query param named "a.b" or "a[0]" matches only
# itself and a literal '*' in a pattern is unsupported (documented). Used to
# compile the built-in denylist and user params_keep/params_drop into matchers.
# Implementation: escape every non-word char (so '*' becomes the two-char '\*'),
# then turn that escaped star back into the regex '.*', then anchor.
.glob_to_regex <- function(glob) {
  escaped <- stringi::stri_replace_all_regex(glob, "([^A-Za-z0-9_])", "\\\\$1")
  escaped <- gsub("\\*", ".*", escaped, fixed = TRUE)
  paste0("^", escaped, "$")
}

# ASCII-only case mapping for URL SYNTAX (RURL-ugfpuotu). Scheme and host case
# normalization is defined over an ASCII grammar -- RFC 3986 section 6.2.2.1
# ("scheme and host are case-insensitive and are therefore normalized to
# lowercase", over the ASCII production) and the WHATWG URL Standard, which
# says "ASCII lowercase" explicitly. Neither asks for LINGUISTIC, locale-
# tailored folding.
#
# `stringi::stri_trans_tolower()` without `locale=` inherits the R session's
# locale via `stri_locale_get()`. Under a Turkish/Azeri session (`tr`, `az`;
# Lithuanian `lt` is a milder variant) ICU CORRECTLY maps "I" -> "ı" (dotless
# i) -- correct orthography, catastrophic for URL syntax: "WIKI.example.com"
# became "wıkı.example.com" (a different domain) and "FILE://" stopped matching
# `.SUPPORTED_SCHEMES`. This is the classic Turkish-I hazard (cf. Java's
# `toLowerCase()` without `Locale.ROOT`).
#
# Using `chartr()` over the two 26-letter ASCII alphabets removes ICU from the
# syntax path entirely: no locale, no ICU version, no Unicode version. Non-ASCII
# code points are passed through untouched, which is exactly what "ASCII
# lowercase" specifies. This is a deliberate base-R string exception in the
# sense of ADR 0005, not an unfinished stringi migration.
#
# NA and zero-length inputs propagate unchanged, matching `stri_trans_*`.
#
# `chartr()` is the fast path but it routes through `utf8towcs()`, which rejects
# a few code points some platforms refuse to widen (e.g. the U+FFFF
# noncharacter, which reaches this code from the hostile-input corpora). Those
# rows must survive the parse exactly as they did under stringi, so the fallback
# does the same 26 ASCII substitutions with `stri_replace_all_fixed()`
# (case-sensitive literal matching -- no case folding, hence still no locale and
# no ICU case tables). Sequential A->a ... Z->z cannot cascade because every
# replacement is lowercase and every pattern uppercase.
.ASCII_UPPER <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
.ASCII_LOWER <- "abcdefghijklmnopqrstuvwxyz"

.ascii_tolower <- function(x) {
  tryCatch(
    chartr(.ASCII_UPPER, .ASCII_LOWER, x),
    error = function(e) {
      stringi::stri_replace_all_fixed(x, LETTERS, letters,
        vectorize_all = FALSE
      )
    }
  )
}

.ascii_toupper <- function(x) {
  tryCatch(
    chartr(.ASCII_LOWER, .ASCII_UPPER, x),
    error = function(e) {
      stringi::stri_replace_all_fixed(x, letters, LETTERS,
        vectorize_all = FALSE
      )
    }
  )
}

# Explicit non-tailoring ICU locale for the sites where the user has asked for
# a PRESENTATION transform of text that may legitimately be non-ASCII, rather
# than for protocol-syntax normalization: `case_handling` applied to the PATH,
# and `case_handling = "upper"` applied to the HOST (both in
# `.apply_case_policy_vec()`; no standard ever uppercases a host, so that
# direction is presentation, not syntax). Unicode case mapping is wanted at
# those sites; locale tailoring is not.
#
# TRAP (verified empirically under `LC_ALL=tr_TR.UTF-8`): the two values a
# reviewer reaches for first, `locale = "root"` and `locale = "und"`, DO NOT
# override the ambient locale in stringi -- both still yield "wıkı". Only a
# concrete non-Turkish locale id does; "en_US_POSIX" is the POSIX/invariant
# locale and is verified to yield "wiki" under a Turkish session. Do not
# "simplify" this to "root"/"und": the obvious fix silently does nothing.
.ASCII_SAFE_ICU_LOCALE <- "en_US_POSIX"

# Coerce present-but-empty ("") raw components to NA, vectorized. This dates
# from the external parse engine, whose treatment of a present-but-empty
# component (e.g. the query of "https://example.com/?") varied by VERSION:
# older builds returned NULL (-> NA via %||%), newer ones "". Normalizing
# "" -> NA made rurl's raw query/fragment/userinfo output deterministic
# regardless. The engine is in-tree now and emits NULL for an empty component
# by contract, so this no longer papers over anything -- it is simply where
# the long-shipped "empty component == absent" behavior is enforced.
.blank_to_na <- function(x) {
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# Field spec for safe_parse_urls() result columns. Single source of truth that
# keeps the empty-frame template, the per-row error fallback, and the populated
# frame in sync with safe_parse_url()'s result fields. Each entry carries the
# column name, the default used when a field is absent, and a length-1 vapply
# type template (its zero-length form `template[0]` types the empty frame).
.spu_result_fields <- list(
  list(name = "original_url", default = NA_character_, template = character(1)),
  list(name = "scheme", default = NA_character_, template = character(1)),
  list(name = "host", default = NA_character_, template = character(1)),
  list(name = "port", default = NA_integer_, template = integer(1)),
  list(name = "path", default = NA_character_, template = character(1)),
  list(name = "query", default = NA_character_, template = character(1)),
  list(name = "fragment", default = NA_character_, template = character(1)),
  list(name = "user", default = NA_character_, template = character(1)),
  list(name = "password", default = NA_character_, template = character(1)),
  list(name = "domain", default = NA_character_, template = character(1)),
  list(name = "tld", default = NA_character_, template = character(1)),
  # Encoding-independent identity spellings (RURL-owrdsivt): the registrable
  # domain / public suffix in BOTH canonical spellings, regardless of
  # host_encoding. `domain`/`tld` above still follow host_encoding (a rendering
  # choice); these four are stable identity keys so a Unicode host and its
  # A-label share one domain_ascii (and one domain_unicode) without re-parsing.
  list(name = "domain_ascii", default = NA_character_, template = character(1)),
  list(
    name = "domain_unicode", default = NA_character_, template = character(1)
  ),
  list(name = "tld_ascii", default = NA_character_, template = character(1)),
  list(name = "tld_unicode", default = NA_character_, template = character(1)),
  list(name = "is_ip_host", default = NA, template = logical(1)),
  list(name = "clean_url", default = NA_character_, template = character(1)),
  list(name = "parse_status", default = "error", template = character(1))
)

# Field spec for the option-INDEPENDENT parse core (Stage A) that the full_parse
# cache stores (RURL-dkwrebdt). One entry per cached column, in the order the
# unnamed per-row cache value packs them, with the vapply type template used to
# gather cache hits (mirrors .spu_result_fields). Stage A holds the expensive,
# presentation-independent work -- raw components, IP detection, the post-www
# host, and the PSL decomposition in BOTH spellings (so host_encoding stays a
# Stage-B choice) -- keyed only by url x protocol x www x tld_source x
# scheme_relative. Stage B (._parse_stage_b_vec) derives every remaining column
# from these plus the presentation options and is never cached.
.spu_stage_a_fields <- list(
  list(name = "final_scheme", default = NA_character_, template = character(1)),
  # The scheme as the source SPELLED it (RURL-gkmwqpos, RUL-007): `final_scheme`
  # is the ASCII-lowercased classification token every route produces, and RFC
  # 3986 sec 6.2.2.1 makes that fold a normalization the `source` serializer
  # form must not apply. Equal to `final_scheme` wherever the prepared input's
  # scheme token is not the one the parse settled on. Cached: a parse fact.
  list(
    name = "source_scheme", default = NA_character_, template = character(1)
  ),
  # The scheme the PARSE settled on, before `protocol_handling` rewrites it
  # into `final_scheme` (RUL-016): a port's default-ness is a fact about the
  # parsed scheme (RFC 3986 sec 6.2.3; WHATWG port state), never about the
  # scheme the projection renders, so Stage B keys the default-port table off
  # this column. Cached: a parse fact.
  list(name = "raw_scheme", default = NA_character_, template = character(1)),
  list(name = "final_host", default = NA_character_, template = character(1)),
  list(name = "is_ip_host", default = NA, template = logical(1)),
  list(name = "raw_path", default = NA_character_, template = character(1)),
  list(name = "raw_query", default = NA_character_, template = character(1)),
  list(name = "raw_fragment", default = NA_character_, template = character(1)),
  list(name = "raw_user", default = NA_character_, template = character(1)),
  list(name = "raw_password", default = NA_character_, template = character(1)),
  list(name = "raw_port", default = NA_integer_, template = integer(1)),
  list(name = "domain_ascii", default = NA_character_, template = character(1)),
  list(
    name = "domain_unicode", default = NA_character_, template = character(1)
  ),
  list(name = "tld_ascii", default = NA_character_, template = character(1)),
  list(name = "tld_unicode", default = NA_character_, template = character(1)),
  list(name = "host_is_ace", default = FALSE, template = logical(1)),
  list(name = "looks_like_protocol", default = FALSE, template = logical(1)),
  list(
    name = "original_has_allowed_scheme", default = FALSE,
    template = logical(1)
  ),
  list(name = "is_scheme_relative", default = FALSE, template = logical(1)),
  # host:port input (RURL-aldwnots): matches the scheme regex but is a valid
  # host:port form, so it must be excluded from the unsupported-scheme demotion
  # in parse-status derivation. Cached with the rest of Stage A.
  list(name = "looks_like_host_port", default = FALSE, template = logical(1)),
  # Scheme-less input carrying userinfo (D5): drives the warning-userinfo status
  # and the NA clean_url in Stage B. Cached with the rest of Stage A.
  list(name = "scheme_less_userinfo", default = FALSE, template = logical(1)),
  # RFC 3986 scheme + path-rootless special-scheme rows (`http:example.com`):
  # parseable but hostless, so Stage B must not demote them to an error solely
  # because no authority exists.
  list(name = "rfc3986_path_rootless", default = FALSE, template = logical(1)),
  # Whether this row's userinfo was actually SPLIT into a username and a
  # password (RURL-ovpguvva). Stage B keys the WHATWG userinfo percent-encode
  # set off it, because encoding an UNDIVIDED userinfo would render its
  # structural ":" as "%3A". Cached with the rest of Stage A: it is a fact
  # about the parse, not a presentation choice.
  list(name = "general_userinfo_split", default = FALSE, template = logical(1)),
  # Whether the PARSED authority carried a userinfo delimiter at all
  # (RUL-001, ADR 0017 row 12). Distinct from `raw_user`/`raw_password`, which
  # `.blank_to_na()` empties for `http://@host/` and `http://:@host/`: the
  # delimiter is a parse fact the `credential_handling = "reject"` dial in
  # Stage B keys off, so it must survive a cache hit like the other facts.
  list(name = "authority_userinfo", default = FALSE, template = logical(1)),
  # Whether Stage A produced NO usable parse for this row (invalid input, a
  # Phase-1 rejection, or a parse failure). Cached WITH the other fields rather
  # than signalled by caching a NULL value, so a null row's classifier flags --
  # `looks_like_protocol` / `original_has_allowed_scheme` /
  # `looks_like_host_port`, which are what distinguish an admission REJECTION
  # from a syntax FAILURE -- survive a cache hit instead of reverting to their
  # defaults. Without this the layered verdicts (R/verdicts.R) would depend on
  # cache warmth, which P1.1 §2 forbids.
  list(name = "null_row", default = TRUE, template = logical(1))
)
