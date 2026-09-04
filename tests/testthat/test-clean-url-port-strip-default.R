# RUL-016 (b), RURL-msybcxgq. `port_handling = "strip_default"` judges a
# port's default-ness on the scheme the input was PARSED with, never on the
# scheme `protocol_handling` renders. RFC 3986 sec 6.2.3 and the WHATWG URL
# Standard's port state make the default port equivalent to no port for the
# scheme the URL carries; nothing sanctions dropping a non-default port, which
# names a different origin (RFC 6454 sec 4).
#
# Measured before the fix, `.build_port_part_vec()` received the UPGRADED
# scheme, so with `strip_default` + `protocol_handling = "https"`:
#   http://example.com:80/a   -> https://example.com:80/a    (resurrected a
#                                port the WHATWG record holds as null)
#   http://example.com:443/a  -> https://example.com/a       (folded a real
#                                non-default origin)
#
# Posture: clean surface -- `clean_url` is never a conformance oracle; it is a
# lossy policy projection (ADR 0017). The selector is looped because the
# defect sat in the builder every arm shares.
#
# This fix MOVES `url_standard = NULL` rows, which ADR 0007 freezes, and ADR
# 0016 is what licenses it: the freeze binds selector-CAUSED drift, and this is
# a defect in the default path itself -- no standard, profile or documented
# contract sanctions folding `:443` on an `http` input. Per ADR 0016 the fix
# carries both evidence items:
#
#   1. WITNESS (the first test): the defect is in the NULL path -- the probe
#      runs under the default selector, and omitting `url_standard` and
#      passing `NULL` explicitly agree.
#   2. SIGNATURE (the second and third tests): inputs = a `http`/`https` URL
#      carrying a syntactic port; option combination = `port_handling =
#      "strip_default"` together with `protocol_handling = "https"` (or any
#      value that rewrites the scheme); the output field that moves is
#      `clean_url`, plus the parse-record `port` column under `whatwg` only.
#      Per arm: `NULL` and `rfc3986` moved only on `clean_url`
#      (their parse-record `port` column is raw and was already correct);
#      `whatwg` moved on `clean_url` AND on the `port` column, because the
#      same upgraded scheme also keyed `.apply_port_output_policy_vec()`, so
#      `http://example.com:80/a` reported `port = 80` where the WHATWG record
#      holds null. The surface default `port_handling = "exclude"` and the
#      literal `"keep"` are byte-identical on every arm, and `strip_default`
#      WITHOUT a scheme rewrite is byte-identical on every arm.

.port_inputs <- c(
  "http://example.com:80/a",
  "http://example.com:443/a",
  "http://example.com:8080/a",
  "https://example.com:443/a",
  "https://example.com:8080/a",
  "https://example.com:80/a"
)
.port_expected_after_https <- c(
  "https://example.com/a",
  "https://example.com:443/a",
  "https://example.com:8080/a",
  "https://example.com/a",
  "https://example.com:8080/a",
  "https://example.com:80/a"
)
.selectors <- list(whatwg = "whatwg", rfc3986 = "rfc3986", null = NULL)

test_that("NULL witness: strip_default + https judges the parsed scheme", {
  # ADR 0016 evidence item 1. Omitted and explicit NULL must agree.
  omitted <- get_clean_url(
    .port_inputs, port_handling = "strip_default", protocol_handling = "https"
  )
  explicit <- get_clean_url(
    .port_inputs, url_standard = NULL,
    port_handling = "strip_default", protocol_handling = "https"
  )
  expect_identical(omitted, explicit)
  expect_identical(omitted, .port_expected_after_https)
})

test_that("strip_default + https judges the parsed scheme on every arm", {
  for (nm in names(.selectors)) {
    s <- .selectors[[nm]]
    got <- get_clean_url(
      .port_inputs, url_standard = s,
      port_handling = "strip_default", protocol_handling = "https"
    )
    expect_identical(got, .port_expected_after_https, info = nm)
    # The same through the frame surface.
    expect_identical(
      safe_parse_urls(
        .port_inputs, url_standard = s,
        port_handling = "strip_default", protocol_handling = "https"
      )$clean_url,
      .port_expected_after_https,
      info = nm
    )
  }
})

test_that("signature: exclude, keep, and strip_default sans rewrite hold", {
  for (nm in names(.selectors)) {
    s <- .selectors[[nm]]
    # `exclude` (the surface default) never renders a port.
    expect_identical(
      get_clean_url(
        .port_inputs, url_standard = s, protocol_handling = "https"
      ),
      rep("https://example.com/a", length(.port_inputs)),
      info = nm
    )
    # `keep` is the literal override: every syntactic port survives verbatim.
    expect_identical(
      get_clean_url(
        .port_inputs, url_standard = s,
        port_handling = "keep", protocol_handling = "https"
      ),
      c(
        "https://example.com:80/a", "https://example.com:443/a",
        "https://example.com:8080/a", "https://example.com:443/a",
        "https://example.com:8080/a", "https://example.com:80/a"
      ),
      info = nm
    )
    # No scheme rewrite: the parsed and rendered schemes coincide, so the
    # parsed-scheme rule is observationally the old rule.
    expect_identical(
      get_clean_url(
        .port_inputs, url_standard = s, port_handling = "strip_default"
      ),
      c(
        "http://example.com/a", "http://example.com:443/a",
        "http://example.com:8080/a", "https://example.com/a",
        "https://example.com:8080/a", "https://example.com:80/a"
      ),
      info = nm
    )
  }
})

test_that("whatwg: the parse-record port column uses the parsed scheme", {
  # The WHATWG record nulls a port equal to the PARSED scheme's default. Before
  # the fix the upgraded scheme keyed the elision, so `http://example.com:80/a`
  # reported 80 and `http://example.com:443/a` reported NA under a https
  # rewrite. `get_port()` (no rewrite) already agreed with the record.
  got <- safe_parse_urls(
    .port_inputs, url_standard = "whatwg", protocol_handling = "https"
  )$port
  expect_identical(got, c(NA, 443L, 8080L, NA, 8080L, 80L))
  expect_identical(got, get_port(.port_inputs, url_standard = "whatwg"))
  # rfc3986 and NULL report the raw syntactic port: unchanged.
  for (s in list("rfc3986", NULL)) {
    expect_identical(
      safe_parse_urls(
        .port_inputs, url_standard = s, protocol_handling = "https"
      )$port,
      c(80L, 443L, 8080L, 443L, 8080L, 80L)
    )
  }
})

test_that("general: a non-special scheme upgraded to https keeps :443", {
  # `foo:` has no default port, so a rewrite to `https` must not make 443
  # look default. Pinned on both named standards (the general route has its
  # own serializers, which now take the parsed scheme for the port table).
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      get_clean_url(
        "foo://example.com:443/a", url_standard = s,
        scheme_acceptance = "general", scheme_policy = "require",
        port_handling = "strip_default", protocol_handling = "https"
      ),
      "https://example.com:443/a",
      info = s
    )
  }
})
