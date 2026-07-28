# End-to-end tests for the public `scheme_acceptance = "general"` activation
# (ADR 0012 Layer 4b-2, RURL-qbnelzku). Layer 4b-1 (RURL-yutinyhb) unit-tested
# the pure building blocks; here `general` is publicly reachable, so these drive
# the PUBLIC API (safe_parse_urls + accessors) and assert the observable output.
#
# Byte-identity for the default `scheme_acceptance = "web"` posture is covered
# by test-characterization-snapshot.R + the parity harness; this file adds the
# general-posture behavior.

# --- composition rule (ADR 0012 D3) -----------------------------------------

test_that("general with url_standard = NULL is a validation error (D3)", {
  expect_error(
    safe_parse_urls("mailto:a@b.com", scheme_acceptance = "general"),
    "url_standard"
  )
  expect_error(
    safe_parse_url("mailto:a@b.com", scheme_acceptance = "general"),
    "url_standard"
  )
  # web needs no url_standard and never errors on composition.
  expect_silent(
    safe_parse_urls("http://example.com", scheme_acceptance = "web")
  )
})

# --- the 8 RURL-wncwfasl corpus false-rejects ------------------------------

test_that("corpus opaque/non-special inputs parse ok under whatwg general", {
  ok_inputs <- c(
    "mailto:a@b.com", "data:space?test#test", "fs:/hello.eth", "a:b#",
    "scheme:example.com", "scheme:example.com/path", "foo://///////bar.com/"
  )
  d <- safe_parse_urls(
    ok_inputs, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(any(d$parse_status == "error"))
  expect_true(all(d$parse_status %in% c("ok", "ok-ftp", "ok-scheme-relative")))
  # scheme is returned (today the whole row would error and scheme would be NA).
  expect_identical(
    d$scheme,
    c("mailto", "data", "fs", "a", "scheme", "scheme", "foo")
  )
  # parse_status is reachable through the accessor with scheme_acceptance too.
  st <- get_parse_status(
    ok_inputs, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(any(st == "error"))
})

test_that("repeated raw @ errors under rfc gate, escaped under whatwg", {
  bad <- "scheme://username@@@@example.com"
  # RFC 3986 generic-grammar gate (D1): a repeated raw @ in the authority fails.
  r <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(r$parse_status, "error")
  expect_true(is.na(r$clean_url))
  # WHATWG accepts and percent-escapes the excess credentials, recovering host.
  w <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(w$parse_status == "error")
  expect_identical(w$scheme, "scheme")
  expect_identical(w$host, "example.com")
})

# --- the four WHATWG non-special shapes (D2) --------------------------------

test_that("the four WHATWG non-special shapes parse and round-trip", {
  d <- safe_parse_urls(
    c("foo:bar", "foo:/bar", "foo:///bar", "foo://[::1]/bar"),
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_true(all(d$parse_status == "ok"))
  expect_identical(d$scheme, rep("foo", 4L))
  # opaque path (host absent), null-host list path, empty-host list path, IPv6.
  expect_identical(d$path, c("bar", "/bar", "/bar", "/bar"))
  expect_identical(d$host, c(NA, NA, NA, "[::1]"))
  # clean_url round-trips through the WHATWG serializer (null-vs-empty host).
  expect_identical(
    d$clean_url,
    c("foo:bar", "foo:/bar", "foo:///bar", "foo://[::1]/bar")
  )
})

test_that("opaque scheme payload is carried verbatim under whatwg general", {
  d <- safe_parse_urls(
    "mailto:a@b.com", scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(d$scheme, "mailto")
  # T1 (RURL-glphqenm): a non-special no-`//` scheme is a WHATWG OPAQUE PATH, so
  # the parse table presents NO authority -- the `@` never re-triggers authority
  # parsing -- while the opaque payload is carried verbatim in path and
  # clean_url. ADR 0012 D7's recipient decomposition still reaches users through
  # the get_*() accessors (pinned in test-email-diagnostics.R and in the
  # divergence test below); it is just no longer presented as a parse column.
  expect_identical(d$host, NA_character_)
  expect_identical(d$user, NA_character_)
  expect_identical(d$path, "a@b.com")
  expect_identical(d$clean_url, "mailto:a@b.com")
})

test_that("opaque tails carry @/:/?/# verbatim as path under whatwg general", {
  inputs <- c(
    "mailto:a@b.com",
    "mailto:a@b.com?subject=x",
    "data:text/plain,x",
    "tel:+1-234",
    "sc:a@b:1#frag"
  )
  d <- safe_parse_urls(
    inputs, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(any(d$parse_status == "error"))
  # Non-special no-`//` schemes: no authority is parsed, and none is presented.
  expect_identical(d$host, rep(NA_character_, length(inputs)))
  expect_identical(d$user, rep(NA_character_, length(inputs)))
  expect_identical(d$port, rep(NA_integer_, length(inputs)))
  # No host means no PSL decomposition of one, in either spelling.
  expect_identical(d$domain, rep(NA_character_, length(inputs)))
  expect_identical(d$tld, rep(NA_character_, length(inputs)))
  expect_identical(d$domain_ascii, rep(NA_character_, length(inputs)))
  expect_identical(d$tld_unicode, rep(NA_character_, length(inputs)))
  # Whole remainder is the opaque path; query/fragment still split per WHATWG.
  expect_identical(
    d$path,
    c("a@b.com", "a@b.com", "text/plain,x", "+1-234", "a@b:1")
  )
  expect_identical(d$query, c(NA, "subject=x", NA, NA, NA))
  expect_identical(d$fragment, c(NA, NA, NA, NA, "frag"))
})

test_that("mailto parse columns and the D7 accessors diverge by design", {
  # The deliberate split T1 introduced, pinned from both sides so neither can
  # drift silently: the parse TABLE is WHATWG-conformant (an opaque path has no
  # authority), while the ACCESSORS keep ADR 0012 D7's recipient decomposition,
  # resolved through the same PSL/presentation branches a web host takes.
  u <- "mailto:jane@sub.example.co.uk"
  args <- list(url_standard = "whatwg", scheme_acceptance = "general")

  d <- do.call(safe_parse_urls, c(list(u), args))
  expect_identical(d$host, NA_character_)
  expect_identical(d$user, NA_character_)
  expect_identical(d$domain, NA_character_)

  # The scalar surface masks identically to the vector one.
  s <- do.call(safe_parse_url, c(list(u), args))
  expect_identical(s$host, NA_character_)
  expect_identical(s$domain, NA_character_)

  expect_identical(do.call(get_host, c(list(u), args)), "sub.example.co.uk")
  expect_identical(do.call(get_domain, c(list(u), args)), "example.co.uk")
  expect_identical(do.call(get_tld, c(list(u), args)), "co.uk")
  expect_identical(do.call(get_subdomain, c(list(u), args)), "sub")
  expect_identical(do.call(get_user, c(list(u), args)), "jane")
})

test_that("a mailto: with a real // authority keeps the host it parsed", {
  # RURL-gmzipkyw. The opaque-path rule is "non-special scheme AND no `//`";
  # both halves matter. `mailto://host/p` carries a genuine authority, so D7's
  # recipient decomposition must NOT run over its path -- doing so overwrote the
  # parsed host with NA (there is no addr-spec in `/pathname`) and left a row
  # presenting a port with no host. WPT expects hostname `example.com` here.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")

  d <- do.call(safe_parse_urls, c(
    list(c("mailto://example.com:8080/pathname?search#hash",
           "mailto://test/a/../b",
           "MAILTO://ex.com/p",
           "mailto:jane@example.com")), args))

  # The three `//` forms keep their authority; scheme match is case-insensitive.
  expect_identical(d$host, c("example.com", "test", "ex.com", NA_character_))
  expect_identical(d$port, c(8080L, NA_integer_, NA_integer_, NA_integer_))
  # A port is never presented without the host it belongs to.
  expect_false(any(!is.na(d$port) & is.na(d$host)))
  # The opaque form is untouched: still no authority, payload still verbatim.
  expect_identical(d$path[4], "jane@example.com")

  # The scalar surface agrees with the vector one, as it must.
  s <- do.call(safe_parse_url,
               c(list("mailto://example.com:8080/pathname"), args))
  expect_identical(s$host, "example.com")
  expect_identical(s$port, 8080L)

  # D7 still reaches users for the opaque form -- this narrows the recipient
  # rule, it does not retire it.
  expect_identical(
    do.call(get_host, c(list("mailto:jane@sub.example.co.uk"), args)),
    "sub.example.co.uk"
  )
})

test_that("tab/LF/CR are stripped for non-special schemes too", {
  # RURL-lsgdeisl. WHATWG's step 1 removes every ASCII tab/LF/CR from the input
  # before anything is parsed, for ALL schemes. That step lived only in the
  # web-route preparation path, so rows routed to the general parser were
  # handed
  # the raw string: a tab was percent-encoded into the host and an LF was
  # rejected outright.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")
  u <- c("foo://ho\tst/", "foo://ho\nst/", "foo://ho\rst/", "foo://host/")
  d <- do.call(safe_parse_urls, c(list(u), args))

  expect_false(any(d$parse_status == "error"))
  # All four spellings converge on the same host as the clean input.
  expect_identical(d$host, rep("host", length(u)))

  # Stripping is scheme-independent, so an interior tab goes for a non-special
  # reg-name host as well.
  expect_identical(
    do.call(safe_parse_urls, c(list("sc://a\tb.com/p"), args))$host,
    "ab.com"
  )

  # RFC 3986 has NO strip step -- it requires such bytes to be percent-encoded
  # -- so the rfc3986 profile must still reject. This is the deliberate profile
  # split, not a gap.
  rfc <- suppressWarnings(safe_parse_urls(
    "foo://ho\tst/", url_standard = "rfc3986", scheme_policy = "require",
    scheme_acceptance = "general"))
  expect_identical(rfc$parse_status, "error")
})

test_that("an opaque path is percent-encoded with the C0-control set", {
  # RURL-qxpgcwie. WHATWG's opaque path state encodes each code point with the
  # C0-control percent-encode set as it is consumed, so the stored path -- what
  # the `pathname` getter returns -- is already encoded. rurl carried the
  # payload verbatim; only `clean_url` was encoded, so the `path` column and the
  # serialized URL disagreed.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")
  d <- do.call(safe_parse_urls, c(list(c(
    "wow:￿", "non-special:￿y", "non-special:x/￿y"
  )), args))
  expect_identical(d$path, c("%EF%BF%BF", "%EF%BF%BFy", "x/%EF%BF%BFy"))

  # The C0 set is NOT the path set: printable ASCII a LIST path escapes stays
  # literal in an opaque path. WPT pins this whole row verbatim.
  raw <- "non-special:cannot-be-a-base-url-!\"$%&'()*+,-.;<=>@[\\]^_`{|}~@/"
  expect_identical(
    do.call(safe_parse_urls, c(list(raw), args))$path,
    "cannot-be-a-base-url-!\"$%&'()*+,-.;<=>@[\\]^_`{|}~@/"
  )

  # Existing percent spellings survive, malformed `%` included (a validation
  # -error fact, never a re-encode).
  expect_identical(
    do.call(safe_parse_urls, c(list(c("sc:a%41b", "sc:a%zzb")), args))$path,
    c("a%41b", "a%zzb")
  )
})

test_that("a space before the ?/# ending an opaque path becomes %20", {
  # RURL-qxpgcwie. WHATWG encodes the space immediately preceding the delimiter
  # that ends an opaque path, and leaves every other space literal, so that a
  # trailing space survives a re-parse (which strips trailing spaces). Only the
  # LAST space of a run is affected.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")
  d <- do.call(safe_parse_urls, c(list(c(
    "non-special:opaque  ?hi", "non-special:opaque  #hi",
    "non-special:opaque  x?hi", "non-special:opaque  x#hi"
  )), args))
  expect_identical(
    d$path,
    c("opaque %20", "opaque %20", "opaque  x", "opaque  x")
  )

  # Tab/LF/CR are removed first (WHATWG step 1), so the rule sees the stripped
  # string: three spaces survive and only the third is encoded.
  expect_identical(
    do.call(safe_parse_urls,
            c(list("non-special:opaque \t\t  \t#hi"), args))$path,
    "opaque  %20"
  )
})

test_that("VT and FF in a general-routed input no longer break decomposition", {
  # RURL-qxpgcwie. ICU counts U+000B (VT) and U+000C (FF) as line terminators,
  # so the `(.*)` in the scheme/remainder split did not match them and the whole
  # row failed to decompose -- surfacing as a parse error rather than a host
  # with a percent-encoded control. WHATWG strips only tab/LF/CR; VT and FF
  # reach the C0 encoder.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")
  d <- do.call(safe_parse_urls, c(list(c(
    "sc://a\vb/", "sc://a\fb/", "sc:pa\vth"
  )), args))
  expect_false(any(d$parse_status == "error"))
  expect_identical(d$host, c("a%0Bb", "a%0Cb", NA_character_))
  expect_identical(d$path, c("/", "/", "pa%0Bth"))

  # The full WPT C0 row: an opaque host percent-encodes the C0 controls and DEL
  # rather than rejecting them, and keeps every non-forbidden printable.
  ctl <- rawToChar(as.raw(c(1:6, 7, 8, 11, 12, 14:31, 127)))
  got <- do.call(safe_parse_urls,
                 c(list(paste0("sc://", ctl, "!\"$%&'()*+,-.;=_`{}~/")), args))
  expect_identical(got$parse_status, "ok")
  expect_identical(
    got$host,
    paste0("%01%02%03%04%05%06%07%08%0B%0C%0E%0F%10%11%12%13%14%15%16%17",
           "%18%19%1A%1B%1C%1D%1E%1F%7F!\"$%&'()*+,-.;=_`{}~")
  )
})

test_that("IPv6 hosts are WHATWG-serialized for non-special schemes too", {
  # RURL-cyxegfjs. The WHATWG IPv6 serializer (longest zero run compressed,
  # lowercase hex, no dotted-quad tail) is scheme-independent -- the host parser
  # runs it for any scheme with an authority. rurl wired it on the Phase 5b
  # special-scheme branch only, so non-special hosts kept their input spelling.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")
  u <- c("non-special://[1:2:0:0:5:0:0:0]/", "non-special://[1:2:0:0:0:0:0:3]/",
         "non-special://[0:0:0:0:0:0:0:0]/", "non-special://[::127.0.0.1]/",
         "non-special://[ABCD::1]/", "non-special://[1:2:3:4:5:6:7:8]/")
  d <- do.call(safe_parse_urls, c(list(u), args))

  expect_false(any(d$parse_status == "error"))
  expect_identical(
    d$host,
    c("[1:2:0:0:5::]", "[1:2::3]", "[::]", "[::7f00:1]", "[abcd::1]",
      "[1:2:3:4:5:6:7:8]")
  )
  # The special-scheme spelling is the oracle: both branches must agree.
  expect_identical(
    do.call(safe_parse_urls, c(list("http://[1:2:0:0:5:0:0:0]/"), args))$host,
    d$host[1L]
  )
  # A port after the literal is unaffected, and the serialized host is what the
  # non-special serializer renders back out.
  p <- do.call(safe_parse_urls,
               c(list("non-special://[1:2:0:0:5:0:0:0]:8080/x"),
                 c(args, list(port_handling = "keep"))))
  expect_identical(p$port, 8080L)
  expect_identical(p$clean_url, "non-special://[1:2:0:0:5::]:8080/x")

  # A malformed literal is still a host parse failure, not a passthrough.
  bad <- suppressWarnings(
    do.call(safe_parse_urls, c(list("non-special://[1:2:3:4]/"), args))
  )
  expect_identical(bad$parse_status, "error")

  # rfc3986 stays source-preserving: the `rfc-syntax` posture disclaims host
  # normalization, so the input spelling survives. Deliberate profile split.
  rfc <- safe_parse_urls(
    "non-special://[1:2:0:0:5:0:0:0]/", url_standard = "rfc3986",
    scheme_policy = "require", scheme_acceptance = "general")
  expect_identical(rfc$host, "[1:2:0:0:5:0:0:0]")
})

test_that("an opaque payload ending in :<digits> still parses", {
  # RURL-jnvtttfm. The scheme-less `example.com:8080` carve-out matched with a
  # colon-greedy authority part, so `urn:ietf:rfc:2648` read as "authority
  # urn:ietf:rfc, port 2648", never reached the opaque parser, and fell through
  # to the web path that rejects `urn:`. Every opaque payload whose last
  # colon-separated segment was numeric was unparseable.
  args <- list(url_standard = "whatwg", scheme_policy = "require",
               scheme_acceptance = "general")
  u <- c("urn:ietf:rfc:2648", "urn:a:1", "sc:x:80", "sc:x:80/p",
         "urn:a:abc", "tel:+1-234")
  d <- do.call(safe_parse_urls, c(list(u), args))

  expect_false(any(d$parse_status == "error"))
  expect_identical(
    d$path,
    c("ietf:rfc:2648", "a:1", "x:80", "x:80/p", "a:abc", "+1-234")
  )
  # An opaque path has no authority, so a numeric tail is never read as a port.
  expect_identical(d$host, rep(NA_character_, length(u)))
  expect_identical(d$port, rep(NA_integer_, length(u)))

  # A trailing `?`/`#` used to be the only thing that saved these rows, by
  # breaking the carve-out's end-anchor. They must now agree with the bare form.
  q <- do.call(safe_parse_urls, c(list(c("urn:a:1?q", "urn:a:1#f")), args))
  expect_identical(q$path, c("a:1", "a:1"))
})

test_that("the scheme-less host:port form is still read as host:port", {
  # The guard narrowed by RURL-jnvtttfm must keep doing its actual job: a dot is
  # a legal scheme character, so `example.com:8080` also matches the scheme
  # regex and would otherwise be routed to the opaque parser.
  d <- suppressWarnings(safe_parse_urls(
    c("example.com:8080", "example.com:8080/p", "localhost:3000")))
  expect_identical(d$host, c("example.com", "example.com", "localhost"))
  expect_identical(d$port, c(8080L, 8080L, 3000L))
})

# --- no DNS/PSL derivation and no punycode for opaque/non-special hosts ------

test_that("opaque and non-special hosts get no domain/tld and no punycode", {
  # A genuinely opaque / non-special reg-name host is NOT asserted to be a DNS
  # name (ADR 0012 D2): no PSL derivation, no punycode. (mailto is the explicit
  # D7 carve-out and is covered in test-email-diagnostics.R.)
  d <- safe_parse_urls(
    "foo://host.example/x",
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_true(is.na(d$domain))
  expect_true(is.na(d$tld))
  expect_true(is.na(d$domain_ascii))
  expect_true(is.na(d$tld_ascii))
  # A non-special reg-name host is preserved verbatim (never IDNA/punycode).
  expect_identical(d$host, "host.example")
  # via accessors as well
  expect_true(is.na(get_domain(
    "foo://host.example/x",
    scheme_acceptance = "general", url_standard = "whatwg"
  )))
})

# --- get_scheme_class / get_host_type companion helpers ---------------------

test_that("get_host_type is reachable with scheme_acceptance = general", {
  # A non-special reg-name host is not asserted to be a DNS name; the helper
  # returns a token without erroring under general acceptance.
  ht <- get_host_type(
    "foo://host.example/x",
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_length(ht, 1L)
  expect_false(is.na(ht))
})

test_that("get_scheme carries scheme_acceptance (opaque schemes resolve)", {
  # Default web acceptance: an opaque scheme is outside the allowlist => NA.
  expect_identical(get_scheme("mailto:jane@example.com"), NA_character_)
  expect_identical(get_scheme("tel:+15551234567"), NA_character_)
  # general acceptance admits the literal scheme (both standards).
  expect_identical(
    get_scheme("mailto:jane@example.com",
      url_standard = "rfc3986", scheme_acceptance = "general"),
    "mailto"
  )
  expect_identical(
    get_scheme("ws://example.com/s",
      url_standard = "whatwg", scheme_acceptance = "general"),
    "ws"
  )
  # general still requires an explicit url_standard.
  expect_error(
    get_scheme("mailto:x", scheme_acceptance = "general"),
    "requires an explicit url_standard"
  )
})

test_that("get_scheme_class cascade returns non-special under general", {
  # D2 cascade completed: with general acceptance the opaque scheme resolves,
  # so get_scheme_class classifies it as non-special rather than
  # missing-or-error (the reachable-today web default).
  expect_identical(
    get_scheme_class("mailto:jane@example.com", url_standard = "rfc3986"),
    "missing-or-error"
  )
  expect_identical(
    get_scheme_class("mailto:jane@example.com",
      url_standard = "rfc3986", scheme_acceptance = "general"),
    "non-special"
  )
  # Special schemes stay special under general acceptance.
  expect_identical(
    get_scheme_class("http://example.com/",
      url_standard = "whatwg", scheme_acceptance = "general"),
    "special"
  )
  expect_identical(
    get_scheme_class("ws://example.com/s",
      url_standard = "whatwg", scheme_acceptance = "general"),
    "special"
  )
})

# --- ws/wss special-scheme metadata activates under general (D4 / L1) --------

test_that("ws/wss parse as special (ports 80/443) under whatwg general", {
  d <- safe_parse_urls(
    c("ws://example.com/s", "wss://example.com/s"),
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_true(all(d$parse_status == "ok"))
  expect_identical(d$scheme, c("ws", "wss"))
  expect_identical(d$host, c("example.com", "example.com"))
  # The L1 default-port metadata (ws->80, wss->443) is live: an explicit default
  # port is elided under strip_default, proving special-ness.
  dd <- safe_parse_urls(
    c("ws://example.com:80/s", "wss://example.com:443/s"),
    scheme_acceptance = "general", url_standard = "whatwg",
    port_handling = "strip_default"
  )
  expect_false(any(grepl(":80|:443", dd$clean_url)))
})

test_that("ws is still rejected under the default web acceptance", {
  d <- safe_parse_urls("ws://example.com/s")
  expect_identical(d$parse_status, "error")
})

# --- RFC-general branch: the gate is the acceptance contract ----------------

test_that("rfc3986 general parses generic URIs and gates bad authorities", {
  ok <- safe_parse_urls(
    c("mailto:a@b.com", "scheme:example.com", "fs:/hello.eth"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_true(all(ok$parse_status != "error"))
  expect_identical(ok$scheme, c("mailto", "scheme", "fs"))
  # file under rfc3986 uses the RFC 8089 overlay.
  f <- safe_parse_urls(
    "file:///etc/hosts", scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(f$parse_status, "ok")
  expect_identical(f$path, "/etc/hosts")
})
