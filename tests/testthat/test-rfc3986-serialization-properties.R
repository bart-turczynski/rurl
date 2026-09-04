# The RFC 3986 SERIALIZATION quadrant (RURL-irhxumys, epic RURL-dorofzmb).
#
# WHY THIS FILE EXISTS. After P5.4 the conformance evidence had four quadrants
# and three of them were covered. WHATWG had both acceptance (202/202 WPT
# must-fail) and full-string serialization (336/336 WPT `href`, OR-022). RFC
# 3986 had acceptance only (OR-002). Nothing anywhere in the tree answered
# "what string does RFC 3986 require this URL to serialize to". That blind spot
# is not academic: RURL-savatsuc -- a host decoded past the unreserved set,
# producing output that does not re-parse -- passed every harness in the repo,
# because an acceptance-axis metric cannot see a bad output string on an input
# it accepts.
#
# WHY IT IS PROPERTY-BASED AND NOT A FIXTURE. WHATWG ships a machine-checkable
# suite with recorded `href` values; RFC 3986 is prose plus ABNF and has no
# equivalent. The available move -- hand-transcribing expected strings -- is the
# one RURL-nknytzxz caught red-handed: 75 fixture rows where the oracle and the
# implementation confirmed each other and their shared disagreement with the RFC
# stayed invisible. OR-006 already concedes that most RFC-side rows "certify a
# READING of the RFC, not the RFC".
#
# So this file transcribes NO expected strings. It states RFC 3986 sec 6.2.2 /
# 6.2.3 as PROPERTIES that must hold of every output, and evaluates them over a
# GENERATED population. A property needs no oracle, so it cannot co-confirm with
# the implementation; and the population is a cross product rather than a
# curated list, so it cannot be quietly trimmed to what already passes.
#
# THE ONE EXTERNAL JUDGE is `rfc3986_abnf_accepts()` (OR-003,
# helper-rfc3986-abnf.R) -- the RFC's own grammar, transcribed from the RFC and
# sharing no code with rurl. Property P-G runs every serialization back through
# it. That is the closest thing to an RFC serialization oracle that exists, and
# it is a genuine one: the grammar is normative text, not somebody's reading of
# it.
#
# WHAT THIS CANNOT DO. The grammar admits far more than the RFC requires a
# NORMALIZED serialization to be, so P-G alone is weak; N1-N4 supply the
# sec 6.2.2/6.2.3 obligations it cannot see. Neither can tell whether a
# component was sliced from the input correctly -- an output can satisfy every
# property here and still describe the wrong URL. This quadrant is a necessary
# condition on RFC serialization, not a sufficient one, and the register records
# it that way (OR-023).
#
# KNOWN DEVIATIONS are enumerated by input, never tolerated by a count. Each
# names the ticket that owns it, so a regression fails at the row that moved
# instead of quietly re-fitting a total.

# --- the population ----------------------------------------------------------

# Generated, not transcribed. Two axes -- an octet and a component position --
# crossed, plus a hand-built set of structural shapes that no octet sweep
# reaches (dot segments, default ports, empty delimiters, credential spellings,
# IP literals, opaque paths).
#
# Both scheme classes appear at every position on purpose. rurl's parse pipeline
# gates several phases on `.SUPPORTED_SCHEMES`, so a sweep over `http` alone
# measures the special-scheme path only and reports it as the whole story --
# which is exactly how RURL-xkhbhaje and RURL-dergzwku stayed invisible.
#
# RAW OCTETS ABOVE 0x7F ARE EXCLUDED, under protest: a single invalid-UTF-8
# input aborts the entire vectorized call from `.pct_hex_upper()` instead of
# failing that row (RURL-zexwmwxn), so the sweep cannot reach them until that is
# fixed. Their percent-encoded spellings ARE covered -- %00 through %FF, in both
# hex cases -- so the octet range itself is not the gap; only its literal
# spelling is.

rfc_prop_population <- local({
  triplets <- c(sprintf("%%%02X", 0:255), sprintf("%%%02x", 0:255))
  literals <- vapply(1:127, function(i) rawToChar(as.raw(i)), character(1))
  tokens <- unique(c(triplets, literals))

  # `http` = special/supported, `foo` = general. One position per template.
  positions <- c(
    "http://u%s@host/p", "http://ho%sst/p", "http://host/pa%sth",
    "http://host/p?q%sx", "http://host/p#f%sx", "http://host/%s/b",
    "foo://u%s@host/p", "foo://ho%sst/p", "foo://host/pa%sth",
    "foo://host/p?q%sx", "foo://host/p#f%sx", "foo:opa%sque"
  )

  shapes <- c(
    # sec 6.2.2.1 case
    "HTTP://EXAMPLE.COM/", "HtTp://ExAmPlE.CoM/A/B", "FOO://HoSt/p",
    "http://U@HoSt/p", "http://HO%2DST/p", "foo://HO%2DST/p",
    # sec 6.2.3 default ports
    "http://h:80/x", "https://h:443/x", "http://h:8080/x", "foo://h:80/x",
    "http://[::1]:80/p",
    # sec 6.2.2.3 dot segments
    "http://host/a/./b/../c", "http://host/../a", "http://host/./",
    "http://host/a/..", "http://host/a/.", "http://host//a//",
    "http://host/a/../../..", "foo:a/./b/../c", "foo:/a/./b",
    # sec 6.2.2.2 unreserved
    "http://host/%7Euser", "http://host/%7euser", "http://host/%2F",
    "http://%7Euser@host/", "http://host/?%7E#%7E", "http://host/?%7e#%7e",
    # structural: empty delimiters, hosts, authorities, credentials
    "http://h", "http://h/", "http://h?", "http://h#", "http://h?#",
    "foo://", "foo:///p", "foo:/p", "foo:", "foo:?q#f",
    "http://@h/", "http://u:@h/", "http://:p@h/", "http://u:p:q@h/",
    # non-hierarchical and mixed
    "urn:ietf:rfc:2648", "mailto:a@b.example", "http://127.0.0.1/x",
    "http://[2001:db8::1]/x", "http://h/p?a=1&b=2#f/g?h"
  )

  population <- unique(c(
    unlist(lapply(positions, sprintf, tokens), use.names = FALSE), shapes
  ))

  # Return the same immutable value to every property. R's copy-on-modify
  # semantics keep callers from mutating the cached population in place.
  function() population
})

# Serialize the population under one RFC posture and drop the rows the profile
# rejects. Acceptance is OR-002's axis and is deliberately not scored here.
rfc_prop_serialize <- local({
  pop <- rfc_prop_population()
  cache <- lapply(c("source", "normalized"), function(form) {
    out <- serialize_url(pop, standard = "rfc3986", form = form)
    keep <- !is.na(out)
    list(input = pop[keep], output = out[keep])
  })
  names(cache) <- c("source", "normalized")

  function(form) {
    stopifnot(length(form) == 1L, form %in% names(cache))
    cache[[form]]
  }
})

# Assert a property holds of every row except an enumerated deviation set.
#
# `deviations` is a character vector of INPUTS, not a tolerated count. Both
# directions are checked: an unlisted violation fails, and so does a listed
# input that no longer violates -- so a fix cannot land without deleting its
# entry here, and the deviation list cannot rot into a permanent allowance.
expect_property <- function(violates, input, deviations = character(0)) {
  expect_setequal(input[violates], deviations)
}

# --- shared component projections -------------------------------------------

# Deliberately re-derived from the OUTPUT STRING with plain regex rather than
# taken from rurl's own parser. A property checked through the parser under test
# is a property the parser can satisfy by being consistently wrong.
rfc_out_scheme <- function(x) sub(":.*$", "", x)

rfc_out_authority <- function(x) {
  ifelse(grepl("^[^:]+://", x), sub("^[^:]+://([^/?#]*).*$", "\\1", x), NA)
}

rfc_out_host <- function(x) {
  sub(":[0-9]*$", "", sub("^.*@", "", rfc_out_authority(x)))
}

rfc_out_port <- function(x) {
  a <- rfc_out_authority(x)
  ifelse(!is.na(a) & grepl(":[0-9]+$", a), sub("^.*:", "", a), NA)
}

rfc_out_path <- function(x) {
  sub("[?#].*$", "", sub("^[^:]+:(//[^/?#]*)?", "", x))
}

rfc_out_triplets <- function(x) regmatches(x, gregexpr("%[0-9A-Fa-f]{2}", x))

# unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"   (sec 2.3)
RFC_UNRESERVED_PCT <- local({
  o <- c(0x41:0x5A, 0x61:0x7A, 0x30:0x39, 0x2D, 0x2E, 0x5F, 0x7E)
  c(sprintf("%%%02X", o), sprintf("%%%02x", o))
})

# --- the deviation sets ------------------------------------------------------
#
# EMPTY. Every enumerated deviation this harness was born with has been fixed
# (the last, RURL-dergzwku, was a general-scheme userinfo admitting a raw
# LF/VT/FF/CR). A new one must be added here with its ticket, not tolerated by
# widening a predicate.

# Directly-written non-ASCII in a reg-name or path. NOT a deviation: ADR 0012
# and host-annotation-contracts.md sec RFC host form settle this as a
# "documented RFC-syntax posture, not IRI conformance" (SETTLED). The ABNF is
# ASCII-only by construction, so it must reject these; under the RURL-nknytzxz
# two-axis split that is `divergence_class`, not `rurl_deviation`, and it is
# excluded from P-G by an explicit predicate rather than by input list.
rfc_prop_non_ascii <- function(x) {
  grepl("[^\\x00-\\x7f]", x, perl = TRUE, useBytes = TRUE)
}

# Component positions whose percent-spelling the `source` posture DOES preserve,
# measured byte-for-byte over all 512 triplet spellings. The host is here under
# BOTH scheme classes since RURL-xkhbhaje, and the query and fragment since
# RURL-gkmwqpos (ruling RUL-007), which moved sec 6.2.2.1's hex-case fold off
# the `rfc3986` parse record and into the `normalized` serializer form. Every
# component position of `rfc_prop_population()` is now listed (the segment
# shape measured 0 deviations over its 494 accepted rows at the same time), so
# a position that starts folding again fails the test below by name.
SRC_PRESERVING_POSITIONS <- c(
  "http://ho%sst/p", "http://u%s@host/p", "http://host/pa%sth",
  "http://host/p?q%sx", "http://host/p#f%sx", "http://host/%s/b",
  "foo://ho%sst/p", "foo://u%s@host/p", "foo://host/pa%sth",
  "foo://host/p?q%sx", "foo://host/p#f%sx", "foo:opa%sque"
)

# --- P-G: every serialization is admitted by the RFC 3986 grammar ------------

test_that("every RFC serialization is admitted by the RFC 3986 ABNF", {
  # THE property this quadrant was missing, and the only one judged by an
  # authority outside rurl. `rfc3986_abnf_accepts()` is OR-003: the RFC's own
  # Section 3 + Appendix A grammar, transcribed from the RFC and sharing no code
  # with the implementation it judges.
  for (form in c("source", "normalized")) {
    r <- rfc_prop_serialize(form)
    ascii <- !rfc_prop_non_ascii(r$output)
    bad <- ascii & !rfc3986_abnf_accepts(r$output)
    expect_property(bad, r$input)
  }
})

test_that("the ABNF exclusion for non-ASCII output is narrow and enumerated", {
  # The one predicate-shaped carve-out above, bounded so it cannot widen
  # silently. If a future change starts emitting raw non-ASCII somewhere new,
  # this count moves and the exclusion is re-examined rather than inherited.
  r <- rfc_prop_serialize("normalized")
  expect_identical(sum(rfc_prop_non_ascii(r$output)), 0L)
})

# --- P-R / P-C: closure of the serialization ---------------------------------

test_that("an RFC serialization re-parses to itself", {
  # The FSSS-3 property, run over the generated population instead of the
  # 17-row hand list in test-serialize-fsss.R. This alone would have caught
  # RURL-savatsuc, with no oracle and no adjudication.
  for (form in c("source", "normalized")) {
    r <- rfc_prop_serialize(form)
    again <- serialize_url(r$output, standard = "rfc3986", form = form)
    bad <- is.na(again) | again != r$output
    expect_property(bad, r$input)
  }
})

test_that("normalization is confluent with the source posture", {
  # normalize(source(x)) == normalize(x). Normalization is defined on the URL,
  # so routing through the source rendering must not change where it lands --
  # otherwise `source` is losing state that `normalized` depends on.
  src <- rfc_prop_serialize("source")
  nrm <- rfc_prop_serialize("normalized")
  expect_identical(src$input, nrm$input)
  round <- serialize_url(src$output, standard = "rfc3986", form = "normalized")
  bad <- is.na(round) | round != nrm$output
  expect_property(bad, src$input)
})

test_that("`form` is a presentation axis and never changes acceptance", {
  # P2.5 OUT-O3, re-asserted on this population rather than on WPT's.
  src <- rfc_prop_serialize("source")
  nrm <- rfc_prop_serialize("normalized")
  expect_identical(
    src$input,
    nrm$input
  )
})

# --- N1: RFC 3986 sec 6.2.2.1, case normalization ----------------------------

test_that("normalization lower-cases the scheme and the host", {
  # sec 6.2.2.1: "the scheme and host are case-insensitive and therefore should
  # be normalized to lowercase". Percent triplets are excluded from the host
  # test -- their hex digits go the OTHER way (N1c), and folding them here would
  # assert the contradiction.
  r <- rfc_prop_serialize("normalized")
  sch <- rfc_out_scheme(r$output)
  expect_property(sch != tolower(sch), r$input)

  host <- gsub("%[0-9A-Fa-f]{2}", "", rfc_out_host(r$output))
  expect_property(!is.na(host) & host != tolower(host), r$input)
})

test_that("normalization upper-cases every surviving percent triplet", {
  # sec 6.2.2.1: "should be normalized to use uppercase letters for the digits
  # A-F". Applies to every component, so the property is stated over the whole
  # output string rather than per component.
  r <- rfc_prop_serialize("normalized")
  bad <- vapply(
    rfc_out_triplets(r$output), function(t) any(t != toupper(t)), logical(1)
  )
  expect_property(bad, r$input)
})

# --- N2: RFC 3986 sec 6.2.2.2, percent-encoding normalization ----------------

test_that("normalization decodes every triplet encoding an unreserved octet", {
  # sec 6.2.2.2: URIs that differ only by an unreserved octet's encoding are
  # equivalent, and "should be decoded". The obligation is stated over the URI,
  # with no scheme condition -- which is what the deviation below violates.
  r <- rfc_prop_serialize("normalized")
  bad <- vapply(
    rfc_out_triplets(r$output),
    function(t) any(t %in% RFC_UNRESERVED_PCT), logical(1)
  )
  expect_property(bad, r$input)
})

# --- N3 / N4: sec 6.2.2.3 path segments, sec 6.2.3 default port --------------

test_that("normalization removes dot segments from a rooted path", {
  # sec 6.2.2.3. A rootless path has no dot-segment meaning to remove, so the
  # property is conditioned on a leading "/" -- the same condition the
  # serializer applies, stated here independently from the output string.
  r <- rfc_prop_serialize("normalized")
  path <- rfc_out_path(r$output)
  bad <- startsWith(path, "/") & grepl("(^|/)\\.\\.?(/|$)", path)
  expect_property(bad, r$input)
})

test_that("normalization elides a port equal to the scheme default", {
  # sec 6.2.3 scheme-based normalization. `foo://h:80/x` must KEEP its port --
  # `foo` has no default -- which the population covers so that the property
  # cannot be satisfied by eliding `:80` unconditionally.
  r <- rfc_prop_serialize("normalized")
  sch <- rfc_out_scheme(r$output)
  port <- rfc_out_port(r$output)
  bad <- !is.na(port) &
    ((sch == "http" & port == "80") | (sch == "https" & port == "443"))
  expect_property(bad, r$input)

  expect_identical(
    serialize_url("foo://h:80/x", standard = "rfc3986", form = "normalized"),
    "foo://h:80/x"
  )
})

# --- the source posture is inert ---------------------------------------------

test_that("the source posture applies no sec 6.2.2/6.2.3 normalization", {
  # The contrapositive of N1-N4, and the property that keeps the two forms from
  # collapsing into one. Stated as the SURVIVAL of each thing `normalized`
  # removes, so it fails the moment a normalization migrates into `source`.
  src <- function(x) serialize_url(x, standard = "rfc3986", form = "source")
  expect_identical(src("http://host/a/./b/../c"), "http://host/a/./b/../c")
  expect_identical(src("http://h:80/x"), "http://h:80/x")
  expect_identical(src("https://h:443/x"), "https://h:443/x")
  expect_identical(src("http://host/%7Euser"), "http://host/%7Euser")
  expect_identical(src("http://host/%7euser"), "http://host/%7euser")
  expect_identical(src("http://EXAMPLE.com/A"), "http://EXAMPLE.com/A")
})

test_that("the source posture preserves percent-spelling outside the host", {
  # Byte-for-byte, over every octet in both hex cases: nothing is decoded and
  # no hex case is folded. Zero deviations -- the host is simply not in this
  # population, because its exception is stated as its own test below rather
  # than weakening this one.
  triplets <- c(sprintf("%%%02X", 0:255), sprintf("%%%02x", 0:255))
  pop <- unlist(
    lapply(SRC_PRESERVING_POSITIONS, sprintf, triplets), use.names = FALSE
  )
  out <- serialize_url(pop, standard = "rfc3986", form = "source")
  keep <- !is.na(out)
  expect_gt(sum(keep), 2000L)
  expect_property(keep & out != pop, pop)
})

test_that("the source posture is byte-preserving on the whole population", {
  # RURL-gkmwqpos, ruling RUL-007. `?serialize_url` says `source` preserves
  # source bytes, and until this ruling it did not on 316 of the 5967 accepted
  # population rows, in two families the previous version of this test pinned
  # as CHARACTERIZED FACTS: the query/fragment hex-case fold (313 rows) and the
  # scheme case fold (3 rows). Both are halves of RFC 3986 sec 6.2.2.1, which
  # sits under sec 6 "Normalization and Comparison" -- so both belong to
  # `form = "normalized"`, where the host's sec 6.2.2.2 fold already went in
  # RURL-xkhbhaje, and neither to the parse record nor to `form = "source"`.
  #
  # History of the deviating count, so the zero below is read as a measurement
  # and not as a vacuous bound: 324 -> 316 when RURL-epoinamh left
  # `path-abempty`'s empty match empty (8 rows retired), 316 -> 316 when
  # RURL-crrgaiel admitted 302 new `http://ho%XXst/p` rows that all preserved,
  # 316 -> 0 here. The accepted count rose 5664 -> 5966 (RURL-crrgaiel) ->
  # 5967 (RURL-uafjkaas, `urn:ietf:rfc:2648`) and does not move with this fix:
  # acceptance is `pqf_bytes`'s question and the parser still asks it.

  src <- function(x) serialize_url(x, standard = "rfc3986", form = "source")
  norm <- function(x) {
    serialize_url(x, standard = "rfc3986", form = "normalized")
  }

  # 1. Query and fragment keep their hex case. Total, at both positions: every
  # one of the 512 triplet spellings comes back as written. `normalized` is the
  # negative control: it still folds (sec 6.2.2.1) -- and, being a
  # normalization, is the same function of either spelling.
  expect_identical(src("http://host/p?q%0ax"), "http://host/p?q%0ax")
  expect_identical(src("http://host/p#f%0ax"), "http://host/p#f%0ax")
  expect_identical(norm("http://host/p?q%0ax"), "http://host/p?q%0Ax")
  expect_identical(norm("http://host/p#f%0ax"), "http://host/p#f%0Ax")
  expect_identical(
    src("http://x.test/p%7ca?q=%7ca#f%7ca"), "http://x.test/p%7ca?q=%7ca#f%7ca"
  )
  for (tmpl in c("http://host/p?q%sx", "http://host/p#f%sx")) {
    lower <- sprintf(tmpl, sprintf("%%%02x", 0:255))
    upper <- sprintf(tmpl, sprintf("%%%02X", 0:255))
    expect_identical(src(lower), lower)
    expect_identical(src(upper), upper)
    expect_identical(norm(lower), norm(upper))
    expect_false(any(norm(lower) == lower & norm(upper) != upper))
  }

  # 2. The scheme keeps its case, on the web route and on the general route
  # alike, while `normalized` folds it (sec 6.2.2.1) and the host stays as
  # written under `source` (sec 6.2.2.2 is `normalized`'s too).
  expect_identical(src("HTTP://EXAMPLE.COM/"), "HTTP://EXAMPLE.COM/")
  expect_identical(src("Http://EXAMPLE.COM/"), "Http://EXAMPLE.COM/")
  expect_identical(src("FOO://x/"), "FOO://x/")
  expect_identical(src("URN:ietf:rfc:2648"), "URN:ietf:rfc:2648")
  expect_identical(norm("HTTP://EXAMPLE.COM/"), "http://example.com/")
  expect_identical(norm("FOO://x/"), "foo://x/")

  # The record's scheme COLUMN is not the source spelling: it is the
  # classification token every consumer keys off, and the key contract keeps
  # scheme identity case-insensitive (key-join-contracts.md "scheme case": raw
  # spelling is retained for source reproduction only, never key equality).
  p <- safe_parse_urls(
    c("HTTP://EXAMPLE.COM/", "http://example.com/"),
    url_standard = "rfc3986", scheme_policy = "require",
    scheme_acceptance = "general"
  )
  expect_identical(p$scheme, c("http", "http"))
  # (Same host spelling on both rows: the `rfc3986` key's host is the source
  # spelling since RURL-xkhbhaje, and that is not what is under test here.)
  k <- get_url_key(
    c("HTTP://example.com/", "http://example.com/"),
    url_key_policy(standard = "rfc3986")
  )
  expect_identical(k[[1L]], k[[2L]])

  # And the retired family, asserted from the other side so it cannot come back:
  # an authority-only URI keeps its empty path in the `source` posture, and only
  # `normalized` applies sec 6.2.3.
  expect_identical(src(c("http://h", "http://h?")), c("http://h", "http://h?"))
  expect_identical(
    norm(c("http://h", "http://h?")), c("http://h/", "http://h/?")
  )

  # And the bound: zero, over the whole accepted population, so a family cannot
  # come back quietly. `urn:ietf:rfc:2648` is still named (RURL-uafjkaas): it
  # is the one row whose acceptance the count depends on.
  pop <- rfc_prop_serialize("source")
  expect_length(pop$input, 5967L)
  expect_identical(sum(pop$output != pop$input), 0L)
  expect_identical(src("urn:ietf:rfc:2648"), "urn:ietf:rfc:2648")
})

test_that("source reproduces raw bytes >= 0x80 in query and fragment", {
  # RURL-bpfumnbj, ruling RUL-015 -- the residual RUL-007 left in place. The
  # population above excludes raw octets above 0x7F (RURL-zexwmwxn), so the
  # zero it measures could not see that a raw non-ASCII byte in the QUERY or
  # FRAGMENT still came back percent-encoded while the same byte in the PATH
  # came back raw. RFC 3986 sec 2.1 makes a percent-encoding triplet a
  # REPRESENTATION of a data octet, and sec 5.3 recomposes components as they
  # are, so under RUL-007's own reading the source form reproduces the octet
  # it was handed. Valid UTF-8 only: an invalid sequence aborts the vectorized
  # call (RURL-zexwmwxn) and is not what this test is about.
  src <- function(x) serialize_url(x, standard = "rfc3986", form = "source")
  norm <- function(x) {
    serialize_url(x, standard = "rfc3986", form = "normalized")
  }
  raw <- c(
    "https://x/ü?ü#ü",             # web route, all three slots
    "https://localhost#\U0001F525",               # astral, fragment only
    "http://h/p?q=%7cü#f%7cü",          # beside a lowercase triplet
    "foo://h/p?q=ü#fü",                 # general route
    "https://x/�?�#�"              # wptcf-025's input
  )
  expect_identical(src(raw), raw)

  # `normalized` keeps encoding them (uppercase, sec 2.1's canonical spelling),
  # on both routes -- and is the same function of either spelling.
  enc <- c(
    "https://x/ü?%C3%BC#%C3%BC",
    "https://localhost/#%F0%9F%94%A5",
    "http://h/p?q=%7C%C3%BC#f%7C%C3%BC",
    "foo://h/p?q=%C3%BC#f%C3%BC",
    "https://x/�?%EF%BF%BD#%EF%BF%BD"
  )
  expect_identical(norm(raw), enc)
  expect_identical(norm(enc), enc)

  # Input that already spells the octet as a triplet does not move in either
  # form (wpt-fail-039's shape): `source` is not a decoder.
  already <- c(
    "https://x/%EF%BF%BD?%EF%BF%BD#%EF%BF%BD", "https://%EF%BF%BD",
    "http://h/p?q=%c3%bc#f%c3%bc"
  )
  expect_identical(src(already), already)
  expect_identical(
    norm(already),
    c("https://x/%EF%BF%BD?%EF%BF%BD#%EF%BF%BD", "https://%EF%BF%BD/",
      "http://h/p?q=%C3%BC#f%C3%BC")
  )

  # The record moves with the serializer under `rfc3986` only. The other two
  # arms are looped rather than assumed (design/posture-card.md, frame vs
  # payload): `whatwg` keeps the component pass's encoded spelling and the
  # frozen `NULL` profile is byte-identical to it.
  rfc <- safe_parse_urls(raw[1:4], url_standard = "rfc3986",
                         scheme_policy = "require",
                         scheme_acceptance = "general")
  expect_identical(rfc$query, c("ü", NA, "q=%7cü", "q=ü"))
  expect_identical(
    rfc$fragment, c("ü", "\U0001F525", "f%7cü", "fü")
  )
  w <- safe_parse_urls(raw[1:3], url_standard = "whatwg",
                       scheme_policy = "require", scheme_acceptance = "general")
  n <- safe_parse_urls(raw[1:3], url_standard = NULL,
                       scheme_policy = "infer", scheme_acceptance = "web")
  for (p in list(w, n)) {
    expect_identical(p$query, c("%C3%BC", NA, "q=%7C%C3%BC"))
    expect_identical(p$fragment, c("%C3%BC", "%F0%9F%94%A5", "f%7C%C3%BC"))
  }
  expect_identical(
    serialize_url(raw[1:3], standard = "whatwg"),
    c("https://x/%C3%BC?%C3%BC#%C3%BC", "https://localhost/#%F0%9F%94%A5",
      "http://h/p?q=%7C%C3%BC#f%7C%C3%BC")
  )
})

# --- the population itself ---------------------------------------------------

test_that("the property population covers both scheme classes at every axis", {
  # A property suite is only as good as what it is run over, and a population
  # that silently shrinks weakens every assertion above without failing any of
  # them. Pinned by shape, not by an exact total, so adding a structural shape
  # does not require re-deriving a magic number.
  pop <- rfc_prop_population()
  expect_gt(length(pop), 6000L)
  expect_identical(anyDuplicated(pop), 0L)

  # Every octet reachable as a triplet, in both hex cases, at every position.
  expect_true(all(sprintf("http://ho%%%02Xst/p", 0:255) %in% pop))
  expect_true(all(sprintf("foo://ho%%%02xst/p", 0:255) %in% pop))

  # Both scheme classes present at each of the six component positions.
  for (tmpl in c("%s://u%%41@host/p", "%s://ho%%41st/p", "%s://host/pa%%41th",
                 "%s://host/p?q%%41x", "%s://host/p#f%%41x")) {
    expect_true(all(sprintf(tmpl, c("http", "foo")) %in% pop))
  }

  # And the profile actually accepts a working majority of it -- a population
  # that is 95% rejected would make every property above vacuously true.
  acc <- !is.na(serialize_url(pop, standard = "rfc3986", form = "normalized"))
  expect_gt(sum(acc) / length(pop), 0.8)
})
