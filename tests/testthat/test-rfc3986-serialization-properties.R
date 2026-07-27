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

rfc_prop_population <- function() {
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

  unique(c(
    unlist(lapply(positions, sprintf, tokens), use.names = FALSE), shapes
  ))
}

# Serialize the population under one RFC posture and drop the rows the profile
# rejects. Acceptance is OR-002's axis and is deliberately not scored here.
rfc_prop_serialize <- function(form) {
  pop <- rfc_prop_population()
  out <- serialize_url(pop, standard = "rfc3986", form = form)
  keep <- !is.na(out)
  list(input = pop[keep], output = out[keep])
}

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

# `%7C` decodes to a literal `|`, which is in no RFC 3986 production, so the
# output is not a URI and does not re-parse. Its decoded spelling is pinned
# across the fixtures with a reasoned `oracle_ref`, which makes it a decision
# about which spelling is canonical rather than a patch.
DEV_PIPE_HOST <- c("http://ho%7Cst/p", "http://ho%7cst/p")

# A general-scheme userinfo admits raw LF/VT/FF/CR (0x0A-0x0D) and emits them
# verbatim; the special-scheme userinfo rejects all four.
DEV_USERINFO_C0 <- sprintf(
  "foo://u%s@host/p", vapply(0x0A:0x0D, function(i) rawToChar(as.raw(i)),
                             character(1))
)

# Directly-written non-ASCII in a reg-name or path. NOT a deviation: ADR 0012
# and host-annotation-contracts.md sec RFC host form settle this as a
# "documented RFC-syntax posture, not IRI conformance" (SETTLED). The ABNF is
# ASCII-only by construction, so it must reject these; under the RURL-nknytzxz
# two-axis split that is `divergence_class`, not `rurl_deviation`, and it is
# excluded from P-G by an explicit predicate rather than by input list.
rfc_prop_non_ascii <- function(x) {
  grepl("[^\\x00-\\x7f]", x, perl = TRUE, useBytes = TRUE)
}

# The general-scheme host is the only position that skips sec 6.2.2.2, so every
# unreserved triplet written there survives normalization.
DEV_GENERAL_HOST_PCT <- c(
  sprintf("foo://ho%sst/p", RFC_UNRESERVED_PCT), "foo://HO%2DST/p"
)

# Component positions whose percent-spelling the `source` posture DOES preserve,
# measured byte-for-byte over all 512 triplet spellings. Three of the six
# components are absent, each for its own reason and each pinned below: the host
# is normalized on the parse record (RURL-xkhbhaje symptom B), and the query and
# fragment fold hex case (RURL-gkmwqpos family 4).
SRC_PRESERVING_POSITIONS <- c(
  "http://u%s@host/p", "http://host/pa%sth",
  "foo://u%s@host/p", "foo://host/pa%sth", "foo:opa%sque"
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
    expect_property(
      bad, r$input,
      deviations = c(DEV_PIPE_HOST, DEV_USERINFO_C0)
    )
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
    expect_property(bad, r$input, deviations = DEV_PIPE_HOST)
  }
})

test_that("normalization is confluent with the source posture", {
  # normalize(source(x)) == normalize(x). Normalization is defined on the URL,
  # so routing through the source rendering must not change where it lands --
  # otherwise `source` is losing state that `normalized` depends on.
  pop <- rfc_prop_population()
  src <- serialize_url(pop, standard = "rfc3986", form = "source")
  nrm <- serialize_url(pop, standard = "rfc3986", form = "normalized")
  keep <- !is.na(src) & !is.na(nrm)
  round <- serialize_url(src[keep], standard = "rfc3986", form = "normalized")
  bad <- is.na(round) | round != nrm[keep]
  expect_property(bad, pop[keep], deviations = DEV_PIPE_HOST)
})

test_that("`form` is a presentation axis and never changes acceptance", {
  # P2.5 OUT-O3, re-asserted on this population rather than on WPT's.
  pop <- rfc_prop_population()
  expect_identical(
    is.na(serialize_url(pop, standard = "rfc3986", form = "source")),
    is.na(serialize_url(pop, standard = "rfc3986", form = "normalized"))
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
  expect_property(bad, r$input, deviations = DEV_GENERAL_HOST_PCT)
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

test_that("the source posture is not byte-preserving in four known places", {
  # RURL-gkmwqpos. `?serialize_url` says `source` preserves source bytes; it
  # does not, on 419 of 5668 accepted population rows in four families. These
  # are pinned as CHARACTERIZED FACTS, not as approved behavior: the ticket
  # records that the open question is whether the docs or the code is wrong.
  src <- function(x) serialize_url(x, standard = "rfc3986", form = "source")

  # 1. Host percent-spelling, normalized on the parse record (RURL-xkhbhaje).
  expect_identical(src("http://ho%2Dst/p"), "http://ho-st/p")
  expect_identical(src("http://ho%7fst/p"), "http://ho%7Fst/p")

  # 2. Query and fragment fold hex case -- half of sec 6.2.2.1, in the posture
  # defined as applying none of it. Total, not partial: every one of the 156
  # triplet spellings that HAS a lowercase hex letter folds, at both positions.
  expect_identical(src("http://host/p?q%0ax"), "http://host/p?q%0Ax")
  expect_identical(src("http://host/p#f%0ax"), "http://host/p#f%0Ax")
  for (tmpl in c("http://host/p?q%sx", "http://host/p#f%sx")) {
    lower <- sprintf(tmpl, sprintf("%%%02x", 0:255))
    out <- src(lower)
    expect_identical(sum(out != lower), 156L)
    # The 156 are exactly the spellings containing a foldable letter, and the
    # fold is the ONLY change: upper-casing the input reproduces the output.
    expect_identical(out, sprintf(tmpl, sprintf("%%%02X", 0:255)))
  }

  # 3. Scheme case -- the other half of sec 6.2.2.1, applied while the host
  # half is not, so the two forms agree on the scheme and differ on the host.
  expect_identical(src("HTTP://EXAMPLE.COM/"), "http://EXAMPLE.COM/")

  # 4. Empty path rendered as "/" -- a sec 6.2.3 scheme-based normalization.
  expect_identical(src("http://h"), "http://h/")
  expect_identical(src("http://h?"), "http://h/?")

  # And the bound, so four families cannot quietly become five.
  pop <- rfc_prop_population()
  out <- src(pop)
  keep <- !is.na(out)
  expect_identical(sum(keep & out != pop), 419L)
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
