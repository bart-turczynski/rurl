# Full-string standard serializers (FSSS, output surface (b)).
#
# These cover the deferral cells the surface-b build slice carries: SURF-b,
# INV-2, FSSS-1, FSSS-3, FSSS-7, CAP-2, and the fragment half of REC-4/FSSS-2.
#
# The suite is organized around what the contract actually promises, not around
# the functions: full-string completeness, standard-exactness, independence
# from presentation, and parse -> serialize -> parse idempotence -- the last
# being the oracle P5.3's conformance claims stand on, and the one the audit
# recorded as having ZERO instances in the suite before this file.

wf <- function(...) rurl:::.serialize_whatwg_full_vec(...)
rf <- function(...) rurl:::.serialize_rfc_full_vec(...)

# A host-present http row, so each test varies one axis against a fixed rest.
w_row <- function(userinfo = NA_character_, host = "h", port = NA_character_,
                  path = "/p", path_kind = "list",
                  query = NA_character_, query_kind = "absent",
                  fragment = NA_character_, fragment_kind = "absent",
                  scheme = "http", host_kind = "present",
                  authority_delimiter_present = TRUE) {
  wf(
    scheme = scheme, userinfo = userinfo, host = host, host_kind = host_kind,
    authority_delimiter_present = authority_delimiter_present, path = path,
    path_kind = path_kind, query = query, query_kind = query_kind,
    fragment = fragment, fragment_kind = fragment_kind, port = port
  )
}

# --- FSSS-1 / REC-4: full-string completeness -------------------------------

test_that("the WHATWG FSSS emits credentials and the fragment", {
  expect_identical(
    w_row(userinfo = "u:p", query = "a=1", query_kind = "present",
          fragment = "frag", fragment_kind = "present"),
    "http://u:p@h/p?a=1#frag"
  )
})

test_that("a present-but-empty query or fragment delimiter survives", {
  # ADR 0012 D2. This is the property that separates the FSSS from the clean
  # serializer most visibly: `#` carries information and must not collapse.
  expect_identical(w_row(fragment = "", fragment_kind = "empty"), "http://h/p#")
  expect_identical(w_row(query = "", query_kind = "empty"), "http://h/p?")
  expect_identical(
    w_row(query = "", query_kind = "empty",
          fragment = "", fragment_kind = "empty"),
    "http://h/p?#"
  )
})

test_that("an absent query or fragment emits no delimiter", {
  expect_identical(w_row(), "http://h/p")
})

test_that("the fragment is percent-encoded with the fragment set", {
  # Fragment set: SP " < > `  -- and NOT `?`, which stays literal.
  expect_identical(
    w_row(fragment = "a b\"<>`?x", fragment_kind = "present"),
    "http://h/p#a%20b%22%3C%3E%60?x"
  )
})

# --- FSSS-7 / CAP-2: WHATWG credential spec-exactness ------------------------

test_that("WHATWG drops a userinfo whose halves are both empty", {
  # "includes credentials" is false for `http://@h/`, so WPT pins the `@` as
  # dropped. The undivided slice is retained on the RECORD, not in this output.
  expect_identical(w_row(userinfo = ""), "http://h/p")
})

test_that("WHATWG drops an empty password but keeps its username", {
  # `http://u:@h/` -> `http://u@h/` (WPT).
  expect_identical(w_row(userinfo = "u:"), "http://u@h/p")
  expect_identical(w_row(userinfo = "u"), "http://u@h/p")
})

test_that("WHATWG keeps a password with an empty username", {
  # `:p` splits to username "" / password "p"; credentials are included, so the
  # empty username is emitted as empty and the pair still renders.
  expect_identical(w_row(userinfo = ":p"), "http://:p@h/p")
})

test_that("the userinfo split is at the FIRST colon only", {
  # A password may itself contain colons; they are not delimiters.
  expect_identical(w_row(userinfo = "u:p:q"), "http://u:p%3Aq@h/p")
})

test_that("credentials are percent-encoded with the userinfo set", {
  expect_identical(w_row(userinfo = "a b@c"), "http://a%20b%40c@h/p")
})

# --- INV-2: independence from presentation ----------------------------------

test_that("the FSSS takes no presentation dial", {
  # Not a style check: passing `port_handling` or `trailing_slash_handling`
  # must be impossible, because surface (b) is identity and those dials are
  # surface (c)/(d). If someone widens the signature, this fails.
  fsss_args <- names(formals(rurl:::.serialize_whatwg_full_vec))
  expect_false(any(c(
    "port_handling", "trailing_slash_handling", "path_encoding",
    "case_handling", "www_handling"
  ) %in% fsss_args))

  rfc_args <- names(formals(rurl:::.serialize_rfc_full_vec))
  expect_false(any(c(
    "port_handling", "trailing_slash_handling", "path_encoding",
    "case_handling", "www_handling"
  ) %in% rfc_args))
})

test_that("the identity port is emitted even when it is the scheme default", {
  # The public projection nulls a default port; that is a projection policy
  # downstream, not this surface's business.
  expect_identical(w_row(port = "80"), "http://h:80/p")
  expect_identical(w_row(port = "8080"), "http://h:8080/p")
})

test_that("a lone trailing slash is never stripped", {
  # The clean serializer strips it under `trailing_slash_handling = "strip"`.
  # Identity has no such dial and must keep the path verbatim.
  expect_identical(w_row(path = "/"), "http://h/")
})

# --- structural shapes ------------------------------------------------------

test_that("an empty authority with a present delimiter round-trips", {
  # P1.2 D-C: `//` comes from the recorded delimiter fact, never from host_kind.
  expect_identical(
    w_row(scheme = "foo", host = "", host_kind = "empty", path = "/bar"),
    "foo:///bar"
  )
})

test_that("a delimiter-absent row emits no //", {
  expect_identical(
    w_row(scheme = "foo", host = NA_character_, host_kind = "absent",
          authority_delimiter_present = FALSE, path = "/bar"),
    "foo:/bar"
  )
})

test_that("the /. guard fires for a host-null path that would read as //", {
  expect_identical(
    w_row(scheme = "foo", host = NA_character_, host_kind = "absent",
          authority_delimiter_present = FALSE, path = "//bar"),
    "foo:/.//bar"
  )
})

test_that("an opaque path takes the C0 set and no guard", {
  expect_identical(
    w_row(scheme = "mailto", host = NA_character_, host_kind = "absent",
          authority_delimiter_present = FALSE, path = "a@b.example",
          path_kind = "opaque"),
    "mailto:a@b.example"
  )
})

test_that("an opaque path's trailing space encodes when a fragment follows", {
  # The clean serializer only ever saw a query as the following delimiter,
  # since it drops fragments. The FSSS emits the fragment, so the fragment
  # counts too -- otherwise the space is lost on re-parse.
  expect_identical(
    w_row(scheme = "non-special", host = NA_character_, host_kind = "absent",
          authority_delimiter_present = FALSE, path = "opaque ",
          path_kind = "opaque", fragment = "f", fragment_kind = "present"),
    "non-special:opaque%20#f"
  )
  expect_identical(
    w_row(scheme = "non-special", host = NA_character_, host_kind = "absent",
          authority_delimiter_present = FALSE, path = "opaque ",
          path_kind = "opaque"),
    "non-special:opaque "
  )
})

# --- OUT-O3: both RFC postures ----------------------------------------------

r_row <- function(form, ...) {
  defaults <- list(
    scheme = "HTTP", userinfo = NA_character_, host = "EXAMPLE.com",
    host_kind = "present", authority_delimiter_present = TRUE,
    path = "/a/./b/../c", rfc_path_form = "abempty", query = NA_character_,
    query_kind = "absent", fragment = NA_character_, fragment_kind = "absent",
    port = NA_character_, form = form
  )
  do.call(rf, utils::modifyList(defaults, list(...)))
}

test_that("the source posture normalizes nothing", {
  expect_identical(
    r_row("source", path = "/a/./b/../c/%7euser%2F", port = "80",
          query = "x=%7e", query_kind = "present",
          fragment = "F%2f", fragment_kind = "present"),
    "HTTP://EXAMPLE.com:80/a/./b/../c/%7euser%2F?x=%7e#F%2f"
  )
})

test_that("the normalized posture applies section 6.2.2 and 6.2.3", {
  expect_identical(
    r_row("normalized", path = "/a/./b/../c/%7euser%2F", port = "80",
          query = "x=%7e", query_kind = "present",
          fragment = "F%2f", fragment_kind = "present"),
    # scheme + host case-folded; default port elided; dot segments removed;
    # %7e -> ~ (unreserved); %2f left encoded but upper-cased (reserved).
    "http://example.com/a/c/~user%2F?x=~#F%2F"
  )
})

test_that("normalization leaves a rootless path's dot segments alone", {
  # Section 6.2.2.3 has no rootless dot-segment meaning to remove.
  expect_identical(
    r_row("normalized", host = NA_character_, host_kind = "absent",
          authority_delimiter_present = FALSE, path = "a/./b",
          rfc_path_form = "rootless"),
    "http:a/./b"
  )
})

test_that("the RFC serializer keeps the undivided userinfo in both postures", {
  # RFC 3986 has no username/password concept, so every delimiter state the
  # WHATWG serializer is spec-required to drop survives here verbatim. This is
  # the half of the contract's credential-completeness that is renderable.
  expect_identical(
    r_row("source", userinfo = ""), "HTTP://@EXAMPLE.com/a/./b/../c"
  )
  expect_identical(
    r_row("source", userinfo = "u:"), "HTTP://u:@EXAMPLE.com/a/./b/../c"
  )
  expect_identical(
    r_row("source", userinfo = ":p"), "HTTP://:p@EXAMPLE.com/a/./b/../c"
  )
  expect_identical(
    r_row("normalized", userinfo = ""), "http://@example.com/a/c"
  )
  expect_identical(
    r_row("normalized", userinfo = "u:"), "http://u:@example.com/a/c"
  )
  expect_identical(
    r_row("normalized", userinfo = ":p"), "http://:p@example.com/a/c"
  )
})

test_that("the RFC serializer preserves an empty query or fragment delimiter", {
  expect_identical(
    r_row("source", query = "", query_kind = "empty",
          fragment = "", fragment_kind = "empty"),
    "HTTP://EXAMPLE.com/a/./b/../c?#"
  )
})

# --- FSSS-3: parse -> serialize -> parse idempotence -------------------------

# The oracle. The guarantee is NOT that serializing recovers the input bytes
# (that is surface (a), source reproduction); it is that serializing a parsed
# state and re-parsing it yields a state that serializes identically. A
# serializer that loses a component fails this on the second pass.
serialize_state <- function(url, standard) {
  st <- rurl:::.parse_opaque_urls_vec(url, standard)
  if (identical(standard, "whatwg")) {
    rurl:::.serialize_whatwg_full_vec(
      scheme = st$scheme, userinfo = st$userinfo, host = st$host,
      host_kind = st$host_kind,
      authority_delimiter_present = st$authority_delimiter_present,
      path = st$path, path_kind = st$path_kind, query = st$query,
      query_kind = st$query_kind, fragment = st$fragment,
      fragment_kind = st$fragment_kind, port = st$port
    )
  } else {
    rurl:::.serialize_rfc_full_vec(
      scheme = st$scheme, userinfo = st$userinfo, host = st$host,
      host_kind = st$host_kind,
      authority_delimiter_present = st$authority_delimiter_present,
      path = st$path, rfc_path_form = st$rfc_path_form, query = st$query,
      query_kind = st$query_kind, fragment = st$fragment,
      fragment_kind = st$fragment_kind, port = st$port, form = "source"
    )
  }
}

idempotence_corpus <- c(
  "foo://h/p",
  "foo://h/p?q=1",
  "foo://h/p?q=1#f",
  "foo://h/p#f",
  "foo://h/p#",
  "foo://h/p?",
  "foo://h/p?#",
  "foo://u:p@h/p",
  "foo://u@h/p",
  "foo://:p@h/p",
  "foo://h:8080/p",
  "foo:///p",
  "foo:/p",
  "foo:opaque",
  "foo:opaque?q#f",
  "foo://h/a/b/",
  "foo://h/"
)

test_that("WHATWG parse -> serialize -> parse is idempotent", {
  once <- vapply(
    idempotence_corpus, serialize_state, character(1),
    standard = "whatwg", USE.NAMES = FALSE
  )
  twice <- vapply(
    once, serialize_state, character(1),
    standard = "whatwg", USE.NAMES = FALSE
  )
  expect_identical(twice, once)
})

test_that("RFC parse -> serialize -> parse is idempotent", {
  once <- vapply(
    idempotence_corpus, serialize_state, character(1),
    standard = "rfc3986", USE.NAMES = FALSE
  )
  twice <- vapply(
    once, serialize_state, character(1),
    standard = "rfc3986", USE.NAMES = FALSE
  )
  expect_identical(twice, once)
})

test_that("the RFC source posture reproduces its input for this corpus", {
  # Stronger than idempotence and true only for rfc-syntax, which normalizes
  # nothing. Stated separately so a future normalization change fails HERE
  # rather than silently weakening the idempotence test above.
  once <- vapply(
    idempotence_corpus, serialize_state, character(1),
    standard = "rfc3986", USE.NAMES = FALSE
  )
  expect_identical(once, idempotence_corpus)
})

# --- vectorization ----------------------------------------------------------

test_that("both serializers are vectorized and recycle", {
  out <- wf(
    scheme = c("http", "https"), userinfo = c("u", NA_character_),
    host = c("a", "b"), host_kind = "present",
    authority_delimiter_present = TRUE, path = c("/1", "/2"),
    path_kind = "list", query = NA_character_, query_kind = "absent",
    fragment = c("f", NA_character_), fragment_kind = c("present", "absent"),
    port = NULL
  )
  expect_identical(out, c("http://u@a/1#f", "https://b/2"))
})

test_that("both serializers accept a zero-length record", {
  expect_identical(
    wf(scheme = character(0), userinfo = character(0), host = character(0),
       host_kind = character(0),
       authority_delimiter_present = logical(0), path = character(0),
       path_kind = character(0), query = character(0),
       query_kind = character(0), fragment = character(0),
       fragment_kind = character(0), port = NULL),
    character(0)
  )
})
