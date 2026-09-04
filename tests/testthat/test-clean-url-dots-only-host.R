# RUL-005 (ADR 0017 D1, RURL-otwfjvnf): on the cleaning surface the
# trailing-slash strip must not leave an output whose authority is only dots.
# `http://./` under `trailing_slash_handling = "strip"` cleans to `http://./`,
# never `http://.`. The parse is untouched.
#
# The strip lives in the shared builder (`.build_clean_url_vec`), so the fix is
# selector-independent: all three `url_standard` arms exhibited the defect and
# move identically. Because this moves `url_standard = NULL` output, the file
# carries the two ADR 0016 evidence items inline: the NULL witness (kept as the
# regression test) and the declared signature the fix stays inside.

dots_only <- c(
  "http://./", "http://.", "http://../", "http://.../", "http://./x/"
)
dots_only_stripped <- c(
  "http://./", "http://./", "http://../", "http://.../", "http://./x"
)
dots_only_kept <- c(
  "http://./", "http://./", "http://../", "http://.../", "http://./x/"
)
root_dot <- c("http://a./", "http://example.com./")
root_dot_stripped <- c("http://a.", "http://example.com.")

# The no-slash spelling "http://." parses to path "/" under WHATWG and under
# the frozen NULL arm, so it behaves like "http://./" there. Under `rfc3986`
# the parse records an EMPTY path (RFC 3986 §3.3 admits path-abempty = ""), so
# the arm renders "http://." under `keep` and `strip` alike: that string is the
# parse's, not the strip's, and RUL-005 leaves the parse untouched. The
# expectation below is therefore per arm for that one input.
no_slash_by_arm <- c(
  whatwg = "http://./", rfc3986 = "http://.", "NULL" = "http://./"
)
arm_expected <- function(std, expected) {
  label <- if (is.null(std)) "NULL" else std
  expected[dots_only == "http://."] <- no_slash_by_arm[[label]]
  expected
}

# --- ADR 0016 item 1: the NULL witness --------------------------------------
#
# Measured pre-fix (2026-09-04): every arm emitted "http://." for "http://./",
# "http://.." for "http://../" and "http://..." for "http://.../"; the NULL arm
# was not an exception. This block is the witness, kept as the regression test.

test_that("RUL-005 NULL witness: omitted and explicit NULL agree and keep /", {
  omitted <- get_clean_url(dots_only, trailing_slash_handling = "strip")
  explicit <- get_clean_url(
    dots_only, url_standard = NULL, trailing_slash_handling = "strip"
  )
  expect_identical(omitted, explicit)
  expect_identical(omitted, dots_only_stripped)
  expect_false(any(grepl("://\\.+$", omitted)))
})

# --- selector loop: the strip is selector-independent -----------------------

test_that("RUL-005: all three url_standard arms keep the dots-only separator", {
  for (std in list("whatwg", "rfc3986", NULL)) {
    label <- if (is.null(std)) "NULL" else std
    expect_identical(
      get_clean_url(
        dots_only, url_standard = std, trailing_slash_handling = "strip"
      ),
      arm_expected(std, dots_only_stripped),
      label = paste("arm", label)
    )
    # Root-dot FQDNs are unaffected: the strip still applies.
    expect_identical(
      get_clean_url(
        root_dot, url_standard = std, trailing_slash_handling = "strip"
      ),
      root_dot_stripped,
      label = paste("arm", label, "root-dot")
    )
    # `keep` is untouched by the ruling: it never stripped.
    expect_identical(
      get_clean_url(
        dots_only, url_standard = std, trailing_slash_handling = "keep"
      ),
      arm_expected(std, dots_only_kept),
      label = paste("arm", label, "keep")
    )
  }
})

# --- ADR 0016 item 2: the declared signature --------------------------------
#
# The fix moves exactly: `clean_url` (and therefore `get_clean_url()`), for a
# host matching `^\.+$`, under `trailing_slash_handling = "strip"`, when the
# path is exactly "/". Per arm: `whatwg`, `rfc3986` and `NULL` each exhibited
# the defect (measured above) because the strip runs in the shared builder
# after every arm's parse, and each moves by the same single byte -- the kept
# "/". Nothing else moves: host, parse_status, serialize_url() and
# get_url_key() are asserted below to be what they were before the fix.

test_that("RUL-005 signature: host, status, serialize_url, get_url_key hold", {
  for (std in c("whatwg", "rfc3986")) {
    df <- safe_parse_urls(dots_only, url_standard = std)
    expect_identical(
      df$host, c(".", ".", "..", "...", "."),
      label = paste("host", std)
    )
    expect_identical(
      df$parse_status, rep("warning-invalid-tld", length(dots_only)),
      label = paste("parse_status", std)
    )
  }
  df_null <- safe_parse_urls(dots_only)
  expect_identical(df_null$host, c(".", ".", "..", "...", "."))
  expect_identical(df_null$parse_status, rep("warning-invalid-tld", 5L))

  # The parse is untouched: the serializers keep the trailing slash exactly as
  # they did (the WHATWG parse of "http://." yields path "/").
  expect_identical(
    serialize_url(dots_only, standard = "whatwg"), dots_only_kept
  )
  expect_identical(
    serialize_url(dots_only, standard = "rfc3986", form = "normalized"),
    dots_only_kept
  )

  # Identity is unaffected, and the root-dot FQDN stays distinct from its
  # rootless spelling.
  keys <- as.character(
    get_url_key(c("http://example.com./", "http://example.com/"))
  )
  expect_false(anyNA(keys))
  expect_false(identical(keys[[1]], keys[[2]]))
  expect_identical(
    as.character(get_url_key("http://./")),
    as.character(get_url_key("http://."))
  )
})

# --- the non-special sibling builder mirrors the guard ----------------------
#
# Under `scheme_acceptance = "general"` a non-special scheme is rendered by the
# WHATWG posture serializer (`.serialize_whatwg_vec`), which carries its own
# lone-"/" strip. It can receive a dots-only opaque host ("foo://./"), so it
# mirrors the exception. The RFC generic serializer never strips.

test_that("RUL-005: the non-special WHATWG serializer keeps the separator", {
  y <- c("foo://./", "foo://../", "foo://a./")
  expect_identical(
    get_clean_url(
      y, url_standard = "whatwg", scheme_acceptance = "general",
      scheme_policy = "require", trailing_slash_handling = "strip"
    ),
    c("foo://./", "foo://../", "foo://a.")
  )
  expect_identical(
    get_clean_url(
      y, url_standard = "rfc3986", scheme_acceptance = "general",
      scheme_policy = "require", trailing_slash_handling = "strip"
    ),
    c("foo://./", "foo://../", "foo://a./")
  )
})
