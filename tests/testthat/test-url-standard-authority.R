# Tests for selector-profile authority recovery slices.

test_that("whatwg uses last at-sign as authority host delimiter", {
  u <- "http://username@@@@example.com"
  res <- safe_parse_url(u, url_standard = "whatwg")

  expect_identical(res$host, "example.com")
  expect_identical(res$user, "username%40%40%40")
  expect_true(is.na(res$password))
  expect_identical(res$clean_url, "http://example.com/")
  expect_identical(res$parse_status, "ok")
})

test_that("rfc3986 rejects a repeated raw at-sign instead of recovering", {
  # RE-POINTED by RURL-qrfrvmkg. This used to assert that the rfc3986 selector
  # recovers host=example.com / user=username%40%40%40 -- the RURL-zqhgezuq
  # last-"@" repair, which encodes the excess "@" bytes so the parser can read
  # the authority. That repair is CORRECT under whatwg (the WHATWG parser
  # genuinely takes the last "@") and is asserted above; under rfc3986 it was
  # laundering an input the grammar has no production for. Both `userinfo` and
  # `reg-name` forbid a raw "@", so a valid authority carries AT MOST ONE, and
  # the audited RFC oracle records `failure` for this exact string (fixture row
  # ada-018: "the RFC oracle ALSO rejects here -- repeated '@' leaves a userinfo
  # that is not well-formed"). The uniform gate judges the input as accepted,
  # BEFORE the parser-compat repairs, so the repair can no longer rescue it.
  u <- "http://username@@@@example.com"
  expect_null(safe_parse_url(u, url_standard = "rfc3986"))
  expect_identical(get_parse_status(u, url_standard = "rfc3986"), "error")
  # the grammar, asserted directly and without any backend, agrees.
  expect_false(isTRUE(.rfc3986_generic_uri_ok(u)$ok))
})

test_that("no selector leaves repeated at-sign authority baseline unchanged", {
  expect_null(safe_parse_url("http://username@@@@example.com"))
})

test_that("whatwg recovers authority for special schemes without slashes", {
  res <- safe_parse_urls(
    c("http:example.com", "https:example.com/path"),
    url_standard = "whatwg"
  )

  expect_identical(res$scheme, c("http", "https"))
  expect_identical(res$host, c("example.com", "example.com"))
  expect_identical(res$path, c("/", "/path"))
  expect_identical(
    res$clean_url, c("http://example.com/", "https://example.com/path")
  )
  expect_identical(res$parse_status, c("ok", "ok"))
})

test_that("rfc3986 keeps missing-slash special schemes as path-rootless", {
  res <- safe_parse_urls(
    c("http:example.com", "https:example.com/path"),
    url_standard = "rfc3986"
  )

  expect_identical(res$scheme, c("http", "https"))
  expect_true(all(is.na(res$host)))
  expect_identical(res$path, c("example.com", "example.com/path"))
  expect_true(all(is.na(res$clean_url)))
  expect_identical(res$parse_status, c("ok", "ok"))
})

test_that("no selector keeps special schemes without slashes as errors", {
  urls <- c("http:example.com", "https:example.com/path")

  expect_true(all(is.na(get_clean_url(urls))))
  expect_identical(get_parse_status(urls), c("error", "error"))
})

test_that("rfc3986 empty authority does not duplicate host into path", {
  # This test's NAME was always the right invariant; its assertions used to
  # encode the violation (RURL-xfbzkico). It pinned `host = "evil.com"` with
  # `path = "/"` -- i.e. the WHATWG promotion -- under the grammar selector, and
  # so the duplication check below could only ever count 1 and pass vacuously.
  #
  # RFC 3986 sec 3: `hier-part = "//" authority path-abempty`, and `authority`
  # may be EMPTY, so the third slash begins `path-abempty`. The authority is
  # empty and the path is `/evil.com`. There is no promotion. Appendix B agrees,
  # and so do Go net/url, Python urlsplit, Ruby URI and Perl URI.
  urls <- c("https:///evil.com", "http:///evil.com")

  # Surface (b) and the identity record are the conformance substrates. NOT
  # `clean_url`, which is a policy-driven SEO product (P2.2 sec 1c/5.1) and is
  # barred as a claim substrate by P5.3 CLAIM-1.
  expect_identical(
    serialize_url(urls, standard = "rfc3986", form = "source"), urls
  )
  rec <- rurl:::.fsss_record_vec(urls, "rfc3986", NULL)
  expect_true(all(rec$ok))
  expect_true(all(rec$authority_delimiter_present))
  expect_identical(rec$host_kind, c("empty", "empty"))
  expect_identical(rec$path, c("/evil.com", "/evil.com"))

  # The duplication invariant, asserted so it can actually fail: the token must
  # appear in EXACTLY ONE component, and under the RFC that is the path.
  for (i in seq_along(urls)) {
    expect_true(is.na(rec$host[[i]]) || !nzchar(rec$host[[i]]))
    expect_identical(rec$path[[i]], "/evil.com")
  }

  # The accessors report the same GRAMMAR on every acceptance posture -- one
  # selector must not mean two parses. `scheme_acceptance` still decides whether
  # the row is ADMITTED, which is a different question and stays its own axis.
  for (posture in c("web", "general")) {
    res <- safe_parse_urls(urls, url_standard = "rfc3986",
                           scheme_acceptance = posture)
    expect_true(all(is.na(res$host)))
    expect_identical(res$path, c("/evil.com", "/evil.com"))
  }
})

test_that("whatwg empty-authority special schemes stay host/path coherent", {
  urls <- c("https:///evil.com", "https:////evil.com", "http:///evil.com")
  res <- safe_parse_urls(urls, url_standard = "whatwg")

  expect_identical(res$host, c("evil.com", "evil.com", "evil.com"))
  expect_identical(res$path, c("/", "/", "/"))
  expect_identical(
    res$clean_url,
    c("https://evil.com/", "https://evil.com/", "http://evil.com/")
  )
  expect_identical(res$parse_status, c("ok", "ok", "ok"))
})

test_that("rfc3986 parses an excess-slash empty authority per the grammar", {
  # Was "rfc3986 rejects unsupported excess-slash empty authority" -- a false
  # rejection pinned as if it were a rule (RURL-xfbzkico). `path-abempty =
  # *( "/" segment )` and `segment = *pchar` may be EMPTY, so `//evil.com` is a
  # well-formed `path-abempty` and a 4-slash run is a valid URI: empty
  # authority, path `//evil.com`. "Unsupported" described the libcurl
  # reproduction's reach, not RFC 3986.
  u <- "https:////evil.com"
  expect_true(rfc3986_abnf_accepts(u))
  expect_identical(serialize_url(u, standard = "rfc3986", form = "source"), u)

  rec <- rurl:::.fsss_record_vec(u, "rfc3986", NULL)
  expect_true(rec$ok)
  expect_true(rec$authority_delimiter_present)
  expect_identical(rec$host_kind, "empty")
  expect_identical(rec$path, "//evil.com")

  # WHATWG still collapses the run to `host = evil.com`; that divergence is the
  # point of the two profiles, and the security rationale in
  # fixtures/external-url-vectors.csv (Section VI.B) rests on the WHATWG side,
  # where the browser/fetcher model belongs.
  expect_identical(get_host(u, url_standard = "whatwg"), "evil.com")
})

# --- WHATWG non-special empty-host validation (RURL-kknambrz T2, then ---------
# --- RURL-jxvibxqq) ----------------------------------------------------------
# Under scheme_acceptance="general", url_standard="whatwg", the non-special
# authority parser must reject a host-missing authority. T2 implemented that as
# "empty host carrying a non-null PORT"; RURL-jxvibxqq corrected the trigger to
# the DELIMITER, since WHATWG fails on the `:` or `@` itself, before/without any
# port content. The single legal empty-host shape is a `//` authority holding
# nothing else at all (`foo:///bar`). RFC-profile authority/port rules are a
# separate path (sibling T4/T5) and must stay untouched.

test_that("whatwg general rejects empty host with a port", {
  bad <- c("data://:443", "sc://:12/")
  res <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, c("error", "error"))
})

test_that("whatwg general keeps empty host with no port accepted", {
  # foo:///bar -- empty host, and the authority holds NOTHING else (no `@`, no
  # `:`) -- is the one legal empty-host shape for a non-special scheme.
  res <- safe_parse_urls(
    "foo:///bar", scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, "ok")
  expect_true(is.na(res$host))
  expect_true(is.na(res$port))
})

test_that("whatwg general rejects host-missing authority shapes", {
  # RURL-jxvibxqq. WHATWG makes an empty host a failure as soon as the
  # authority carries a delimiter, and the trigger is the DELIMITER, not the
  # port having content:
  #   * host state -- "if c is U+003A (:) ... if buffer is the empty string,
  #     host-missing validation error, return failure" (fires on `:` alone,
  #     before any port is read).
  #   * authority state -- "if atSignSeen is true and buffer is the empty
  #     string, host-missing validation error, return failure".
  # `data://:` was previously pinned here as legal on the reading that an empty
  # port is a null port and therefore harmless. That reading was wrong: the
  # host-state rule fires on the `:` itself. adaR 0.3.5 rejects it, and
  # `data://:443` / `sc://:/` / `sc://@/` / `sc://te@s:t@/` are all in the WPT
  # must-fail set (inst/bench/wpt-url-cases.json).
  bad <- c("sc://@/", "sc://te@s:t@/", "sc://:/", "data://:")
  res <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, rep("error", length(bad)))
  # Rejected at `web` acceptance too -- general must not be the lenient one.
  web <- safe_parse_urls(
    bad, scheme_acceptance = "web", url_standard = "whatwg"
  )
  expect_identical(web$parse_status, rep("error", length(bad)))
})

test_that("whatwg general keeps legal authority shapes with delimiters", {
  # The mirror of the rejection test: a delimiter is only fatal when the host it
  # delimits is EMPTY. A non-empty host with an empty port, an IPv6 literal, or
  # userinfo all stay legal, so the host-missing rule cannot over-reject.
  ok <- c("sc://host:/", "sc://[::1]:/", "sc://user@host/", "sc://[::1]:80/")
  res <- safe_parse_urls(
    ok, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, rep("ok", length(ok)))
})

test_that("rfc3986 general still accepts host-missing authority shapes", {
  # WHATWG-only, exactly like T2: RFC 3986's `reg-name` and `port` are both
  # `*`-quantified, so an empty host (with or without an empty port) is
  # well-formed generic syntax. `sc://te@s:t@/` is excluded -- it is rejected
  # under RFC for an unrelated reason (`@` is not in the `userinfo` production).
  res <- safe_parse_urls(
    c("sc://@/", "sc://:/", "data://:"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$parse_status, c("ok", "ok", "ok"))
})

test_that("rfc3986 general empty-host-with-port behavior is unchanged", {
  # T2 is WHATWG-only: the RFC profile still accepts these (its reg-name/port
  # rules are owned by sibling tickets), so the fix must not perturb it.
  res <- safe_parse_urls(
    c("data://:443", "sc://:12/"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$parse_status, c("ok", "ok"))
  expect_identical(res$port, c(443L, 12L))
})

# ---------------------------------------------------------------------------
# WHATWG userinfo charset acceptance (RURL-micalqvh, half (a)).
#
# The web parser refuses an authority whose userinfo carries any of 30 ASCII
# code
# points -- SPACE (0x20), the C0 controls (0x00-0x1F) and DEL (0x7F) -- so rows
# WHATWG parses were rejected outright. Every one of the 30 is in the WHATWG
# userinfo percent-encode set, so the encoded spelling written before the parse
# the string IS what WHATWG stores; no restore step exists or is needed.
#
# These tests cover ACCEPTANCE ONLY. The wider userinfo percent-encode set
# ("^ < > | { } ; and ":" in a password) is deliberately NOT applied to the
# `user`/`password` columns yet -- that is a separate unit -- so a `^` still
# reports literally here.
# ---------------------------------------------------------------------------

test_that("whatwg accepts a space in userinfo and stores it percent-encoded", {
  res <- safe_parse_url("http://a b@host/", url_standard = "whatwg")

  expect_false(is.null(res))
  expect_identical(res$host, "host")
  expect_identical(res$user, "a%20b")
  expect_true(is.na(res$password))
})

test_that("whatwg accepts the WPT userinfo punctuation runs", {
  # WPT urltestdata rows: the full ASCII punctuation run in userinfo, with and
  # without a `joe:` password. WHATWG keeps host "host"; rurl errored on both.
  punct <- " !\"$%&'()*+,-.;<=>@[]^_`{|}~"
  punct_pw <- " !\"$%&'()*+,-.:;<=>@[]^_`{|}~"
  res <- safe_parse_urls(
    c(paste0("wss://", punct, "@host/"),
      paste0("wss://joe:", punct_pw, "@host/")),
    url_standard = "whatwg", scheme_acceptance = "general"
  )

  expect_identical(res$host, c("host", "host"))
  # The parsed credentials carry the userinfo percent-encode set (RURL-micalqvh
  # half b), so these are WPT's expected `username` / `password` byte-for-byte:
  # the space is %20, the non-delimiting "@" is %40, and the set members
  # `" ; < = > [ ] ^ ` { | }` are escaped, while `! $ % & ' ( ) * + , - . _ ~`
  # stay literal.
  expect_identical(
    res$user[1], "%20!%22$%&'()*+,-.%3B%3C%3D%3E%40%5B%5D%5E_%60%7B%7C%7D~"
  )
  expect_identical(res$user[2], "joe")
  expect_identical(
    res$password[2],
    "%20!%22$%&'()*+,-.%3A%3B%3C%3D%3E%40%5B%5D%5E_%60%7B%7C%7D~"
  )
})

test_that("whatwg accepts a C0 control and DEL in user and in password", {
  soh <- rawToChar(as.raw(1L))
  del <- rawToChar(as.raw(127L))
  vt <- rawToChar(as.raw(11L))
  res <- safe_parse_urls(
    c(paste0("http://a", soh, "b@host/"),
      paste0("http://a", del, "b@host/"),
      paste0("http://a", vt, "b@host/"),
      paste0("http://u:p", soh, "q@host/"),
      paste0("http://u:p", del, "q@host/")),
    url_standard = "whatwg"
  )

  expect_identical(res$host, rep("host", 5L))
  expect_identical(res$user, c("a%01b", "a%7Fb", "a%0Bb", "u", "u"))
  expect_identical(
    res$password, c(NA_character_, NA_character_, NA_character_, "p%01q",
                    "p%7Fq")
  )
})

test_that("userinfo charset acceptance never double-encodes an existing %", {
  # "%" is not one of the 30 code points, so an already-encoded userinfo passes
  # through byte-identically -- including rows the stage does not touch at all.
  res <- safe_parse_urls(
    c("http://%25DOMAIN:foobar@foodomain.com/",
      "http://u%40ser:p%40ss@ex.com/x",
      paste0("http://%25DOMAIN a:p%40ss@ex.com/x")),
    url_standard = "whatwg"
  )

  expect_identical(res$user, c("%25DOMAIN", "u%40ser", "%25DOMAIN%20a"))
  expect_identical(res$password, c("foobar", "p%40ss", "p%40ss"))
})

test_that("empty-user and empty-password userinfo forms still parse", {
  res <- safe_parse_urls(
    c("http://:p@h/", "http://u:@h/", "http://@h/"),
    url_standard = "whatwg"
  )

  expect_identical(res$host, rep("h", 3L))
  expect_identical(res$user, c(NA_character_, "u", NA_character_))
  expect_identical(res$password, c("p", NA_character_, NA_character_))
})

test_that("rfc3986 still rejects a space or control in userinfo", {
  # The acceptance set is explicitly gated on whatwg: RFC 3986 has no userinfo
  # production for SPACE, a C0 control or DEL, so these stay rejected and no
  # rfc3986 spelling changes.
  soh <- rawToChar(as.raw(1L))
  punct <- " !\"$%&'()*+,-.;<=>@[]^_`{|}~"
  bad <- c("http://a b@host/", paste0("http://a", soh, "b@host/"),
           paste0("wss://", punct, "@host/"))
  res <- safe_parse_urls(
    bad, url_standard = "rfc3986", scheme_acceptance = "general"
  )
  expect_identical(res$parse_status, rep("error", 3L))
  expect_true(all(is.na(res$host)))

  # ... and the legal rfc3986 userinfo rows keep their exact spelling.
  ok <- safe_parse_urls(
    c("http://u:pw@h/", "http://%25DOMAIN:foobar@foodomain.com/",
      "http://u%40ser:p%40ss@ex.com/x"),
    url_standard = "rfc3986"
  )
  expect_identical(ok$user, c("u", "%25DOMAIN", "u%40ser"))
  expect_identical(ok$password, c("pw", "foobar", "p%40ss"))
})

test_that("no-selector default is untouched by userinfo charset acceptance", {
  soh <- rawToChar(as.raw(1L))
  res <- safe_parse_urls(
    c("http://a b@host/", paste0("http://a", soh, "b@host/"),
      "http://u:pw@h/")
  )
  expect_identical(res$parse_status[1:2], c("error", "error"))
  expect_identical(res$user[3], "u")
})

test_that("newly accepted userinfo rows report the credential diagnostics", {
  # Acceptance does NOT launder the row's validation facts: the space is still a
  # non-URL-code-point, and `invalid-URL-unit` is judged on the ORIGINAL input,
  # so pre-encoding cannot suppress it.
  diag <- get_url_diagnostics("http://a b@host/", url_standard = "whatwg")
  expect_true("invalid-credentials" %in% diag)
  expect_true("invalid-URL-unit" %in% diag)
})

# ---------------------------------------------------------------------------
# General/opaque-route credentials (RURL-ovpguvva, epic RURL-rnobeauh)
#
# The opaque parser computed the authority's userinfo -- the WHATWG
# host-missing rule reads it -- and then dropped it from its return list, so
# every general-routed row reported NA credentials while the web route
# reported them exactly. It now splits at the first ":" per the WHATWG
# authority state, which also lets the userinfo percent-encode set apply here
# on the same terms as the web route.
# ---------------------------------------------------------------------------

test_that("the general route splits userinfo into user and password", {
  for (std in c("rfc3986", "whatwg")) {
    res <- safe_parse_urls(
      c("sc://u:p@h/x", "sc://u@h/x", "sc://h/x"),
      scheme_acceptance = "general", url_standard = std
    )
    expect_identical(res$user, c("u", "u", NA_character_), info = std)
    expect_identical(
      res$password, c("p", NA_character_, NA_character_), info = std
    )
    # The credentials do not disturb the rest of the parse.
    expect_identical(res$host, rep("h", 3L), info = std)
  }
})

test_that("the general route splits at the FIRST colon, not the last", {
  # WHATWG's authority state puts everything after the first ":" into the
  # password buffer, so a second ":" is password content, not a delimiter.
  res <- safe_parse_urls("sc://u:p:q@h/x",
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$user, "u")
  expect_identical(res$password, "p:q")
})

test_that("general-route credentials carry the WHATWG userinfo encode set", {
  res <- safe_parse_urls(c("sc://u:p:q@h/x", "sc://u^b:p@h/x"),
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  # The structural ":" became the delimiter; the one INSIDE the password is
  # content and is encoded. Encoding before splitting would have produced
  # "p%3Aq" out of the delimiter itself and misreported the split.
  expect_identical(res$user, c("u", "u%5Eb"))
  expect_identical(res$password, c("p%3Aq", "p"))
  # rfc3986 stays source-preserving on the same inputs.
  raw <- safe_parse_urls("sc://u:p:q@h/x",
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(raw$password, "p:q")
})

test_that("the RFC 8089 file: overlay keeps its userinfo UNDIVIDED", {
  # App. E.1's production is `[ userinfo "@" ]` with no credentials split, and
  # the appendix warns a password there is "a serious security exposure", so
  # rurl must not manufacture a split the RFC never draws. This is the one
  # general-routed producer that deliberately differs.
  res <- safe_parse_urls("file://u:p@host/p",
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$user, "u:p")
  expect_identical(res$password, NA_character_)
  # Undivided means the userinfo encode set must NOT be applied -- it would
  # render the structural ":" as "%3A".
  expect_false(grepl("%3A", res$user, fixed = TRUE))
})

test_that("a mailto: user stays a recipient local-part, not a userinfo", {
  # mailto is an opaque path: no authority, so the general parser leaves its
  # userinfo NA and the split mask is FALSE. safe_parse_url()'s user column is
  # NA for mailto (the D7 recipient extraction is an accessor-level
  # divergence), and no userinfo encode set may reach it.
  res <- safe_parse_urls("mailto:jane@example.com",
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$user, NA_character_)
  expect_identical(res$password, NA_character_)
})

test_that("the web route's credentials are unchanged", {
  # The special-scheme route always split, so nothing about it moves.
  res <- safe_parse_urls("http://u:p:q@ex.com/", url_standard = "whatwg")
  expect_identical(res$user, "u")
  expect_identical(res$password, "p%3Aq")
  expect_identical(
    safe_parse_urls("http://u:p:q@ex.com/",
      url_standard = "rfc3986")$password,
    "p:q"
  )
})

test_that("general-route credentials are reachable through the accessors", {
  u <- "sc://u:p:q@h/x"
  expect_identical(
    get_password(u, scheme_acceptance = "general", url_standard = "whatwg"),
    "p%3Aq"
  )
  expect_identical(
    get_password(u, scheme_acceptance = "general", url_standard = "rfc3986"),
    "p:q"
  )
  expect_identical(
    get_userinfo(u, scheme_acceptance = "general", url_standard = "whatwg"),
    "u:p%3Aq"
  )
})

test_that("general-route credentials raise invalid-credentials under whatwg", {
  # The diagnostic keys off raw_user/raw_password being present, so surfacing
  # credentials that were previously dropped must also surface the fact.
  diag <- get_url_diagnostics("sc://u:p@h/x",
    url_standard = "whatwg", scheme_acceptance = "general"
  )
  expect_true("invalid-credentials" %in% diag)
})
