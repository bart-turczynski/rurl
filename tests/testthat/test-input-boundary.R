# RURL-cwzickqy: one input boundary for every entry point.
#
# R hands rurl a string in one of four representations: UTF-8, latin1, bytes,
# or unmarked native bytes that may not be valid UTF-8. Each entry point used
# to prepare them its own way, and the copies drifted. This file pins what the
# boundary must not move, and (below the pins) what it fixes.
#
# The rows are built in helper-input-boundary.R from raw octets.

# --- Pins: what the boundary must not move ----------------------------------
#
# The parse seams (safe_parse_urls() and every accessor behind it) already
# turned a bytes-marked row into an error row and a latin1 row into its UTF-8
# twin. Their output for every input representation is pinned here under the
# frozen default and both named standards, and under LC_CTYPE C and a UTF-8
# LC_CTYPE. `status` and `clean` are readable; `md5` covers every other
# column (helper-input-boundary.R, ib_canon()). Measured on 6623cab.
#
# Row order: ascii, utf8, utf8_host, native, native_host, latin1, latin1_host,
# bytes, bytes_host, invalid, invalid_query, invalid_host, invalid_marked,
# mailto_latin1, na, empty.
ib_seam_pins <- list(
  null_web = list(
    status = c(
      "ok", "ok", "ok", "ok", "ok", "ok", "ok", "error", "error", "error",
      "ok", "error", "error", "error", "error", "error"
    ),
    clean = c(
      "http://h.example.com/p", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", NA, NA, NA, "http://h.example.com/p",
      NA, NA, NA, NA, NA
    ),
    md5 = "acf901d3ea73ee5cb67678255762d201"
  ),
  whatwg_web = list(
    status = c(
      "ok", "ok", "ok", "ok", "ok", "ok", "ok", "error", "error", "ok", "ok",
      "error", "ok", "error", "error", "error"
    ),
    clean = c(
      "http://h.example.com/p", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", NA, NA,
      "http://h.example.com/caf\ufffd", "http://h.example.com/p", NA,
      "http://h.example.com/p\ufffd", NA, NA, NA
    ),
    md5 = "8c54485243e4822496d04d879c4fa146"
  ),
  rfc3986_web = list(
    status = c(
      "ok", "ok", "ok", "ok", "ok", "ok", "ok", "error", "error", "error",
      "error", "error", "error", "error", "error", "error"
    ),
    clean = c(
      "http://h.example.com/p", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", NA, NA, NA, NA, NA, NA, NA, NA, NA
    ),
    md5 = "3d0397462a21b43621390ba6c94d502d"
  ),
  whatwg_general = list(
    status = c(
      "ok", "ok", "ok", "ok", "ok", "ok", "ok", "error", "error", "ok", "ok",
      "error", "ok", "ok", "error", "error"
    ),
    clean = c(
      "http://h.example.com/p", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", NA, NA,
      "http://h.example.com/caf\ufffd", "http://h.example.com/p", NA,
      "http://h.example.com/p\ufffd", "mailto:a%C3%BC@b.example.com", NA, NA
    ),
    md5 = "363345a7d40d8cbde02dffd362daab2f"
  ),
  rfc3986_general = list(
    status = c(
      "ok", "ok", "ok", "ok", "ok", "ok", "ok", "error", "error", "error",
      "error", "error", "error", "ok", "error", "error"
    ),
    clean = c(
      "http://h.example.com/p", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", "http://h.example.com/p\u00fc",
      "http://b\u00fccher.example.com/", NA, NA, NA, NA, NA, NA,
      "mailto:a\u00fc@b.example.com", NA, NA
    ),
    md5 = "d3b38abb0aab7f761016da54dfa925df"
  )
)

ib_utf8 <- hi_utf8_locale()

test_that("the parse seams keep their output for every input representation", {
  skip_if(is.na(ib_utf8), "no UTF-8 LC_CTYPE is available")
  rows <- ib_parse_rows()
  for (loc in c(ib_utf8, "C")) {
    for (p in names(ib_postures)) {
      out <- hi_in_ctype(
        loc, do.call(safe_parse_urls, c(list(rows), ib_postures[[p]]))
      )
      lab <- paste(p, loc)
      # The echo is the caller's own element, bytes and mark included.
      expect_identical(out$original_url, unname(rows), label = lab)
      expect_identical(out$parse_status, ib_seam_pins[[p]]$status, label = lab)
      expect_identical(out$clean_url, ib_seam_pins[[p]]$clean, label = lab)
      out$original_url <- NULL
      expect_identical(ib_md5(ib_canon(out)), ib_seam_pins[[p]]$md5,
                       label = paste(lab, "all columns"))
    }
  }
})

# Every manifest export over the plain template row and its UTF-8 twins, marked
# and unmarked: the md5 of its canonical output, under both LC_CTYPEs. These
# rows are what nearly every caller passes, and no change to the input
# boundary may move them. Keyed "<export> <standard> <option set> <operand>",
# the option set by its position in the manifest entry. Measured on 6623cab.
ib_export_pins <- c(
  "safe_parse_urls NULL 1 url" = "6fa9dfd2e2244270d25dfb29cb79e930",
  "safe_parse_urls NULL 2 url" = "4fef8dde29a59b3367d086e720665ae7",
  "safe_parse_urls whatwg 1 url" = "fa2e5fefaab3afdc95f71948583d6572",
  "safe_parse_urls whatwg 2 url" = "b59fa78bdb9c20f7bec9f30bb5b5e971",
  "safe_parse_urls rfc3986 1 url" = "d014bb659e2e45aef18dc24830e8e9e9",
  "safe_parse_urls rfc3986 2 url" = "0ec5802152d520275a4f059abfcdc9ea",
  "safe_parse_url NULL 1 url" = "f8c051ffbeccf3c0e23062f3a95728c3",
  "safe_parse_url whatwg 1 url" = "923c41a7c30d799192efbb9291c06d92",
  "safe_parse_url rfc3986 1 url" = "e1bd36ac7daea25b627228b476c13971",
  "get_parse_status NULL 1 url" = "615b3db3d471ae82a316696057e710f6",
  "get_parse_status whatwg 1 url" = "615b3db3d471ae82a316696057e710f6",
  "get_parse_status rfc3986 1 url" = "615b3db3d471ae82a316696057e710f6",
  "get_parse_verdicts NULL 1 url" = "7f907da308060c0378350cb75fe17f5a",
  "get_parse_verdicts whatwg 1 url" = "7f907da308060c0378350cb75fe17f5a",
  "get_parse_verdicts rfc3986 1 url" = "7f907da308060c0378350cb75fe17f5a",
  "get_clean_url NULL 1 url" = "78aca5f110e8f4d8a9e31c7abe230fdc",
  "get_clean_url NULL 2 url" = "fa9b768d6716af405b106024ba91a69b",
  "get_clean_url whatwg 1 url" = "78aca5f110e8f4d8a9e31c7abe230fdc",
  "get_clean_url whatwg 2 url" = "fa9b768d6716af405b106024ba91a69b",
  "get_clean_url rfc3986 1 url" = "78aca5f110e8f4d8a9e31c7abe230fdc",
  "get_clean_url rfc3986 2 url" = "fa9b768d6716af405b106024ba91a69b",
  "get_scheme NULL 1 url" = "6845fecfad8dcf680ffb01b92b5e8ae9",
  "get_scheme whatwg 1 url" = "6845fecfad8dcf680ffb01b92b5e8ae9",
  "get_scheme rfc3986 1 url" = "6845fecfad8dcf680ffb01b92b5e8ae9",
  "get_host NULL 1 url" = "9219457d84db42e35a3e6d6cc1b9cd8b",
  "get_host NULL 2 url" = "d3e00ff5ff4f210d340e09368b9e8196",
  "get_host whatwg 1 url" = "9219457d84db42e35a3e6d6cc1b9cd8b",
  "get_host whatwg 2 url" = "d3e00ff5ff4f210d340e09368b9e8196",
  "get_host rfc3986 1 url" = "9219457d84db42e35a3e6d6cc1b9cd8b",
  "get_host rfc3986 2 url" = "d3e00ff5ff4f210d340e09368b9e8196",
  "get_domain NULL 1 url" = "3b425b13a4ec0ca5b8eca6edbb91bc91",
  "get_domain whatwg 1 url" = "3b425b13a4ec0ca5b8eca6edbb91bc91",
  "get_domain rfc3986 1 url" = "3b425b13a4ec0ca5b8eca6edbb91bc91",
  "get_subdomain NULL 1 url" = "8cad7649483e1de0c80f2734fa5ef21a",
  "get_subdomain whatwg 1 url" = "8cad7649483e1de0c80f2734fa5ef21a",
  "get_subdomain rfc3986 1 url" = "8cad7649483e1de0c80f2734fa5ef21a",
  "get_tld NULL 1 url" = "996896643d31c94ccbae85bea8be80e7",
  "get_tld whatwg 1 url" = "996896643d31c94ccbae85bea8be80e7",
  "get_tld rfc3986 1 url" = "996896643d31c94ccbae85bea8be80e7",
  "get_port NULL 1 url" = "d4d66df5918166849f609197e6a0a3cb",
  "get_port whatwg 1 url" = "d4d66df5918166849f609197e6a0a3cb",
  "get_port rfc3986 1 url" = "d4d66df5918166849f609197e6a0a3cb",
  "get_path NULL 1 url" = "863cf9a94c352a2d8c43d933f328d1e4",
  "get_path NULL 2 url" = "68bbebc5f74161ddda44d280f3345cfd",
  "get_path whatwg 1 url" = "863cf9a94c352a2d8c43d933f328d1e4",
  "get_path whatwg 2 url" = "68bbebc5f74161ddda44d280f3345cfd",
  "get_path rfc3986 1 url" = "863cf9a94c352a2d8c43d933f328d1e4",
  "get_path rfc3986 2 url" = "68bbebc5f74161ddda44d280f3345cfd",
  "get_query NULL 1 url" = "02b2cf28890cde606083d783d3134491",
  "get_query NULL 2 url" = "570e93ebeb390cd5b4c15fc8624959c5",
  "get_query whatwg 1 url" = "02b2cf28890cde606083d783d3134491",
  "get_query whatwg 2 url" = "570e93ebeb390cd5b4c15fc8624959c5",
  "get_query rfc3986 1 url" = "02b2cf28890cde606083d783d3134491",
  "get_query rfc3986 2 url" = "570e93ebeb390cd5b4c15fc8624959c5",
  "get_fragment NULL 1 url" = "3ce84122e3ebc579e8e3e8d00d5b9cd7",
  "get_fragment whatwg 1 url" = "3ce84122e3ebc579e8e3e8d00d5b9cd7",
  "get_fragment rfc3986 1 url" = "c4522a9ab082d3d9cfb3405474992fbf",
  "get_user NULL 1 url" = "3dd72c5c8c631f1040cc3b9cdd6862de",
  "get_user whatwg 1 url" = "ebaccf4855fa77710b89ae9ded68534a",
  "get_user rfc3986 1 url" = "3dd72c5c8c631f1040cc3b9cdd6862de",
  "get_password NULL 1 url" = "2849f40a184fe9d3279aedd383dbc12b",
  "get_password whatwg 1 url" = "2849f40a184fe9d3279aedd383dbc12b",
  "get_password rfc3986 1 url" = "2849f40a184fe9d3279aedd383dbc12b",
  "get_userinfo NULL 1 url" = "3dd72c5c8c631f1040cc3b9cdd6862de",
  "get_userinfo whatwg 1 url" = "ebaccf4855fa77710b89ae9ded68534a",
  "get_userinfo rfc3986 1 url" = "3dd72c5c8c631f1040cc3b9cdd6862de",
  "get_host_type whatwg 1 url" = "c9fe530808487909696702f18b03d27b",
  "get_host_type rfc3986 1 url" = "c9fe530808487909696702f18b03d27b",
  "get_url_diagnostics whatwg 1 url" = "09523da4d1306b09d59f8ed8bb5adce7",
  "get_url_diagnostics rfc3986 1 url" = "05fe4816b01c9dae2aa967d493031351",
  "get_scheme_class whatwg 1 url" = "5ac05eb1f13a70f637c439692c74f854",
  "get_scheme_class rfc3986 1 url" = "5ac05eb1f13a70f637c439692c74f854",
  "check_hosts NULL 1 url" = "5a7d211969c40bc04c320d4928111191",
  "check_hosts whatwg 1 url" = "3737a77f6634499b1b4040449ae0e672",
  "check_hosts rfc3986 1 url" = "3737a77f6634499b1b4040449ae0e672",
  "is_valid_host NULL 1 url" = "f3158a74899479f7b1adda4005a69e6b",
  "is_valid_host whatwg 1 url" = "9afb18471bac9a899b04fae50954c705",
  "is_valid_host rfc3986 1 url" = "9afb18471bac9a899b04fae50954c705",
  "check_schemes whatwg 1 url" = "c7cdbdfe37f2ee743dcb6688136206c4",
  "check_schemes rfc3986 1 url" = "c7cdbdfe37f2ee743dcb6688136206c4",
  "get_mailto_recipients whatwg 1 url" = "d9b6731f96d2f346a7fbabaafdcae7e9",
  "get_mailto_recipients rfc3986 1 url" = "d9b6731f96d2f346a7fbabaafdcae7e9",
  "serialize_url whatwg 1 url" = "3a98bee0eb2552cef63a681eec4e238b",
  "serialize_url whatwg 2 url" = "3a98bee0eb2552cef63a681eec4e238b",
  "serialize_url rfc3986 1 url" = "ee4e4a63d3a3dd7ce08532e26c8ea9da",
  "serialize_url rfc3986 2 url" = "d72107d2b14aad158eba063cb380aeeb",
  "serialise_url whatwg 1 url" = "3a98bee0eb2552cef63a681eec4e238b",
  "serialise_url rfc3986 1 url" = "ee4e4a63d3a3dd7ce08532e26c8ea9da",
  "format_url none 1 url" = "6630961e2fa6af41a87d4928ec3b522a",
  "get_url_key whatwg 1 url" = "b084a4f758b61d251e5f950b01fbdb25",
  "get_url_key rfc3986 1 url" = "b3cec31b0d225b5cd4f5e1f5a1ab8d82",
  "query_param_summary none 1 urls" = "07833a06dd1855a5a5497ca218720135",
  "resolve_url NULL 1 relative_or_absolute" =
    "78aca5f110e8f4d8a9e31c7abe230fdc",
  "resolve_url NULL 1 base_url" = "bfe4b31f6c0ade011d498929d7a3b1b8",
  "resolve_url whatwg 1 relative_or_absolute" =
    "a933a97453ba4c17ee0425f8a06d68c4",
  "resolve_url whatwg 1 base_url" = "bfe4b31f6c0ade011d498929d7a3b1b8",
  "resolve_url whatwg 2 relative_or_absolute" =
    "4066e92c245f802e7bf7122aad4202e7",
  "resolve_url whatwg 2 base_url" = "7d3f7d59cea2e3e9916e8fe57ecaba06",
  "resolve_url rfc3986 1 relative_or_absolute" =
    "a933a97453ba4c17ee0425f8a06d68c4",
  "resolve_url rfc3986 1 base_url" = "bfe4b31f6c0ade011d498929d7a3b1b8",
  "resolve_url rfc3986 2 relative_or_absolute" =
    "ddc0cb9282821803d83a4cdc520df315",
  "resolve_url rfc3986 2 base_url" = "7129f1b886fb932809c4a283eb08ef7e",
  "url_inner_join whatwg 1 x" = "af20ab08aaafe0b15ab1a925a97527c4",
  "url_inner_join whatwg 1 y" = "0659ca240ef9d3345518fc18f7238bc5",
  "url_inner_join rfc3986 1 x" = "af20ab08aaafe0b15ab1a925a97527c4",
  "url_inner_join rfc3986 1 y" = "0659ca240ef9d3345518fc18f7238bc5",
  "url_left_join whatwg 1 x" = "77d7edbe8548752cdb066eb6738b9c09",
  "url_left_join whatwg 1 y" = "7afec6db64676658e0774e01693f18fc",
  "url_left_join rfc3986 1 x" = "77d7edbe8548752cdb066eb6738b9c09",
  "url_left_join rfc3986 1 y" = "7afec6db64676658e0774e01693f18fc",
  "url_right_join whatwg 1 x" = "02762375f916585f1d86fa2b8875b7bc",
  "url_right_join whatwg 1 y" = "fd287fd4de5b6428ed6603d9f671c8a0",
  "url_right_join rfc3986 1 x" = "02762375f916585f1d86fa2b8875b7bc",
  "url_right_join rfc3986 1 y" = "fd287fd4de5b6428ed6603d9f671c8a0",
  "url_full_join whatwg 1 x" = "1f4fd7e472e97f7cec04794378bf741b",
  "url_full_join whatwg 1 y" = "688a31461c26f62ee880409545e69208",
  "url_full_join rfc3986 1 x" = "1f4fd7e472e97f7cec04794378bf741b",
  "url_full_join rfc3986 1 y" = "688a31461c26f62ee880409545e69208",
  "url_semi_join whatwg 1 x" = "5239cd166959a8e34ad00cd4ee9dee36",
  "url_semi_join whatwg 1 y" = "e6b86910c27f3b7a35c19a56efc30548",
  "url_semi_join rfc3986 1 x" = "5239cd166959a8e34ad00cd4ee9dee36",
  "url_semi_join rfc3986 1 y" = "e6b86910c27f3b7a35c19a56efc30548",
  "url_anti_join whatwg 1 x" = "c9296c72c388029da13f288c75a6c3c0",
  "url_anti_join whatwg 1 y" = "f01da519a9c3d6cd75c6a2eae0ecd3c4",
  "url_anti_join rfc3986 1 x" = "c9296c72c388029da13f288c75a6c3c0",
  "url_anti_join rfc3986 1 y" = "f01da519a9c3d6cd75c6a2eae0ecd3c4",
  "canonical_join NULL 1 data_A" = "62a8cfeb38fb4dc472c1e7dd50f26b24",
  "canonical_join NULL 1 data_B" = "71214177ffe152c9f5c575e7c6e888c1",
  "canonical_join whatwg 1 data_A" = "62a8cfeb38fb4dc472c1e7dd50f26b24",
  "canonical_join whatwg 1 data_B" = "71214177ffe152c9f5c575e7c6e888c1",
  "canonical_join rfc3986 1 data_A" = "62a8cfeb38fb4dc472c1e7dd50f26b24",
  "canonical_join rfc3986 1 data_B" = "71214177ffe152c9f5c575e7c6e888c1"
)

test_that("every export keeps its output on ASCII and UTF-8 rows", {
  skip_on_cran()
  skip_if(is.na(ib_utf8), "no UTF-8 LC_CTYPE is available")
  for (loc in c(ib_utf8, "C")) {
    got <- hi_in_ctype(loc, ib_export_digests())
    expect_identical(sort(names(got)), sort(names(ib_export_pins)))
    moved <- names(ib_export_pins)[got[names(ib_export_pins)] != ib_export_pins]
    expect_identical(moved, character(0), label = paste("moved under", loc))
  }
})

# --- Fixed: entry points that prepared their input themselves --------------
#
# Each case below failed on 6623cab and passes once the entry point reads its
# operand through .prepare_url_input().

ib_bad_ref <- ib_str("h", 0xfc, ".com")
ib_bytes_base <- ib_str("http://h.com/", 0xfc, mark = "bytes")
ib_bad_bases <- c(ib_str("file:///caf", 0xe9),
                  ib_str("http://example.com/caf", 0xe9))
ib_selectors <- list(omitted = "omitted", `NULL` = NULL, whatwg = "whatwg",
                     rfc3986 = "rfc3986")
ib_resolve <- function(ref, base, sel, ...) {
  if (identical(sel, "omitted")) {
    resolve_url(ref, base, ...)
  } else {
    resolve_url(ref, base, url_standard = sel, ...)
  }
}

# ADR 0016: these move `url_standard = NULL` output, so the evidence the ADR
# asks for is here.
#
# Witness. On 6623cab, under url_standard = NULL (omitted or explicit):
#   resolve_url(c(<"h" 0xFC ".com", unmarked>, "g"), "http://b.com/a")
#     threw "invalid multibyte string" out of the whole call under a UTF-8
#     LC_CTYPE (both rows lost), and gave NA for the row under LC_CTYPE C;
#   resolve_url("g", <"http://h.com/" 0xFC, marked bytes>) gave
#     "http://h.com/g", although safe_parse_urls() calls that base an error;
#   resolve_url("x", <"file:///caf" 0xE9, unmarked>) gave "file:///x" under
#     LC_CTYPE C and NA under a UTF-8 one, and the same for
#     "http://example.com/caf" 0xE9; safe_parse_urls() calls both errors.
# No profile, standard or contract sanctions a result that throws for the
# whole vector, depends on LC_CTYPE, or resolves against a base the parser
# refuses: this is a default-path defect, not selector-caused drift.
#
# Signature. Inputs: a reference or base that is bytes-marked, or not valid
# UTF-8 once unmarked bytes are read as UTF-8. Options: resolve_url() under
# any url_standard and either output. Output: that row's element only, which
# under NULL and rfc3986 becomes NA (a bytes or undecodable base is no base,
# so an absolute reference still resolves); every other row is unchanged. Per
# arm: rfc3986 had all three defects, through the same string code as NULL.
# whatwg had the bytes defect, and its undecodable base resolved under
# LC_CTYPE C only; it now reads an undecodable reference or base with U+FFFD
# replacement in every locale, as its parse seam does, so its reference row
# does not move ("http://b.com/h<U+FFFD>.com") and its base rows resolve.
test_that("resolve_url(): an undecodable reference fails or reads alone", {
  skip_if(is.na(ib_utf8), "no UTF-8 LC_CTYPE is available")
  for (loc in c(ib_utf8, "C")) {
    for (s in names(ib_selectors)) {
      got <- hi_in_ctype(loc, hi_try(
        ib_resolve(c(ib_bad_ref, "g"), "http://b.com/a", ib_selectors[[s]])
      ))
      expect_true(got$ok, label = paste(s, loc))
      want <- if (identical(s, "whatwg")) {
        c("http://b.com/h\ufffd.com", "http://b.com/g")
      } else {
        c(NA, "http://b.com/g")
      }
      expect_identical(got$value, want, label = paste(s, loc))
    }
  }
})

test_that("resolve_url(): a bytes-marked base is no base", {
  skip_if(is.na(ib_utf8), "no UTF-8 LC_CTYPE is available")
  for (loc in c(ib_utf8, "C")) {
    for (s in names(ib_selectors)) {
      lab <- paste(s, loc)
      got <- hi_in_ctype(loc, ib_resolve(
        c("g", "g", "https://x.com/y"),
        c(ib_bytes_base, "http://h.com/a", ib_bytes_base), ib_selectors[[s]]
      ))
      expect_identical(got, c(NA, "http://h.com/g", "https://x.com/y"),
                       label = lab)
    }
    for (s in c("whatwg", "rfc3986")) {
      got <- hi_in_ctype(loc, resolve_url(
        "g", c(ib_bytes_base, "http://h.com/a"), url_standard = s,
        output = "serialized"
      ))
      expect_identical(got, c(NA, "http://h.com/g"), label = paste(s, loc))
    }
  }
})

test_that("resolve_url(): an undecodable base reads the same in every locale", {
  skip_if(is.na(ib_utf8), "no UTF-8 LC_CTYPE is available")
  for (loc in c(ib_utf8, "C")) {
    for (s in names(ib_selectors)) {
      lab <- paste(s, loc)
      got <- hi_in_ctype(loc, ib_resolve(
        "x", c(ib_bad_bases, "http://example.com/cafe"), ib_selectors[[s]]
      ))
      want <- if (identical(s, "whatwg")) {
        c("file:///x", "http://example.com/x", "http://example.com/x")
      } else {
        c(NA, NA, "http://example.com/x")
      }
      expect_identical(got, want, label = lab)
      # The parse seams call both bases errors under the frozen default and
      # rfc3986; whatwg reads them with U+FFFD replacement, and so does its
      # resolution.
      sel <- if (identical(s, "omitted")) NULL else ib_selectors[[s]]
      if (!identical(sel, "whatwg")) {
        st <- hi_in_ctype(loc, get_parse_status(ib_bad_bases,
                                                url_standard = sel))
        expect_identical(st, c("error", "error"), label = lab)
      }
    }
  }
})

test_that("a bytes-marked row is refused, not thrown, at every entry point", {
  good <- "http://u@h.example.com/p?q=v#f"
  b <- ib_str("http://h.example.com/p", 0xfc, mark = "bytes")
  for (s in c("whatwg", "rfc3986")) {
    ser <- serialize_url(c(good, b, good), standard = s)
    expect_identical(ser[2L], NA_character_, label = s)
    expect_identical(ser[c(1L, 3L)],
                     rep(serialize_url(good, standard = s), 2L), label = s)
    k <- get_url_key(c(good, b), url_key_policy(standard = s))
    expect_identical(attr(k, "keyability"), c("ok", "invalid-parse"),
                     label = s)
    expect_identical(get_host_type(c(good, b), url_standard = s),
                     c("domain", NA), label = s)
    expect_identical(get_url_diagnostics(c(good, b), url_standard = s)[[2L]],
                     character(0), label = s)
    expect_identical(is_valid_host(c(good, b), url_standard = s),
                     c(TRUE, NA), label = s)
    cs <- check_schemes(c(good, b), url_standard = s)
    expect_identical(cs$scheme, c("http", NA), label = s)
    m <- get_mailto_recipients(
      c("mailto:a@b.example.com",
        ib_str("mailto:a", 0xfc, "@b.example.com", mark = "bytes")),
      url_standard = s
    )
    expect_identical(m$url, "mailto:a@b.example.com", label = s)
  }
  expect_identical(format_url(c(good, b))[2L], NA_character_)
})

test_that("outputs that echo the input keep the caller's own element", {
  b <- ib_str("http://h.example.com/p", 0xfc, mark = "bytes")
  l <- ib_str("http://h.example.com/p", 0xfc, mark = "latin1")
  n <- ib_str("http://h.example.com/p", 0xe9)
  input <- c(b, l, n)
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(check_schemes(input, url_standard = s)$url, input,
                     label = s)
    expect_identical(check_hosts(input, url_standard = s)$url, input,
                     label = s)
  }
  expect_identical(safe_parse_urls(input)$original_url, input)
})

test_that("diagnostics re-parse the string Stage A's general route reads", {
  # Under rfc3986 with scheme_relative_handling = "keep" (the companion
  # helpers' setting), Stage A hands a `//` row the `http:` stand-in before
  # the general route reads it (RURL-ozpxupgq), which claims `///p`, `//` and
  # `//u@/p` and leaves `//h/p` on the web route. The diagnostics' second
  # general parse, which feeds the eligibility classifier, read the raw
  # string and claimed none of them. Every general parse one diagnostics call
  # runs must agree with Stage A's.
  real <- .general_parse_vec
  acc <- new.env()
  acc$seen <- list()
  local_mocked_bindings(.general_parse_vec = function(url, ...) {
    r <- real(url, ...)
    acc$seen[[length(acc$seen) + 1L]] <- r$general_parsed
    r
  })
  u <- c("///p", "//", "//u@/p", "//h/p")
  get_url_diagnostics(u, url_standard = "rfc3986",
                      scheme_acceptance = "general")
  routed <- Filter(function(g) length(g) == length(u), acc$seen)
  expect_gte(length(routed), 2L)
  for (g in routed) {
    expect_identical(g, c(TRUE, TRUE, TRUE, FALSE))
  }
})

# --- Ownership: no export reads a URL string around the boundary -----------
#
# .prepare_url_input() is swapped for a copy that also turns each SENTINEL
# into its TARGET: a URL that differs from it in every fact an export reads
# (scheme, authority, path, host). An export whose result depends on its URL
# operand only through the boundary then returns, for the sentinels, exactly
# what it returns for the targets, once any echo of the caller's own element
# is mapped to its target. A read that skips the boundary -- a string helper
# handed the raw operand, a second parse of it -- sees the sentinel and
# moves the result. The manifest decides which exports and operands are held
# to it, so a new export is held to it the day it gets its entry.
ib_sentinels <- list(
  web = c(
    "zz://sentinel-one.invalid/x" = "mailto:a@b.example.com",
    "sentinel-two" = "http://u:pw@h.example.com:8080/p/../q?x=1&utm_x=y#f",
    "https://sentinel-three.invalid/" = "file:///C:/dir/f.txt",
    "SENTINEL:four" = ib_str("https://b", 0xc3, 0xbc, "cher.example.com/",
                             0xc3, 0xa9, "?q=", 0xc3, 0xbc, mark = "UTF-8"),
    # A row of the other table in a join (hi_neighbors), so a join matches.
    "sentinel-five" = "https://good-two.example.com/b/"
  ),
  mailto = c(
    "mailto:sentinel@zz.invalid" = "mailto:a@b.example.com,c@d.example.com",
    "http://sentinel.invalid/" = "mailto:x@y.example.com?subject=s"
  ),
  reference = c(
    "sentinel-one" = "g/h?x=1#y",
    "/abs/sentinel" = "../up/",
    "zz:opaque-sentinel" = "//other.example.com/p",
    "#frag-sentinel" = "?q=2"
  ),
  base = c(
    "sentinel-base" = "http://base.example.com/dir/page?bq=1#bf",
    "zz://sentinel.invalid/" = "https://a.example.com/x/y",
    "http://sentinel.invalid/q/r" = "file:///C:/d/e",
    "ftp://sentinel.invalid/" = "http://h.example.com/"
  )
)

# Calls whose result does not depend on the URL at all, so no pair can show
# teeth there.
ib_no_teeth <- c(
  "is_valid_host NULL url" =
    "with no standard there is no host type, so every rule is NA"
)

ib_sentinel_set <- function(fn, entry, operand) {
  if (fn == "resolve_url") {
    return(ib_sentinels[[if (operand == "base_url") "base" else "reference"]])
  }
  ib_sentinels[[entry$template]]
}

test_that("every export reads its URL operands through the input boundary", {
  skip_on_cran()
  real <- .prepare_url_input
  pairs <- NULL
  local_mocked_bindings(.prepare_url_input = function(url) {
    p <- real(url)
    hit <- !is.na(p$url) & p$url %in% names(pairs)
    p$url[hit] <- unname(pairs[p$url[hit]])
    p
  })
  to_target <- function(s) {
    hit <- !is.na(s) & s %in% names(pairs)
    s[hit] <- unname(pairs[s[hit]])
    s
  }
  for (fn in names(capability_manifest)) {
    e <- capability_manifest[[fn]]
    for (std in e$standards) {
      for (op in e$operands) {
        pairs <- ib_sentinel_set(fn, e, op)
        lab <- paste(fn, std, op)
        via <- ib_run_export(fn, e, std, list(), op, names(pairs))
        want <- ib_run_export(fn, e, std, list(), op, unname(pairs))
        expect_identical(hi_map_strings(via, to_target), want, label = lab)
        # The pair has teeth: read raw, the sentinels give another result.
        pairs_off <- pairs
        pairs <- character(0)
        raw <- ib_run_export(fn, e, std, list(), op, names(pairs_off))
        pairs <- pairs_off
        if (!lab %in% names(ib_no_teeth)) {
          expect_false(identical(raw, want), label = paste(lab, "teeth"))
        }
      }
    }
  }
})

test_that("resolve_url(): whatwg reads an undecodable operand as U+FFFD", {
  skip_if(is.na(ib_utf8), "no UTF-8 LC_CTYPE is available")
  abs_ref <- ib_str("http://h.com/caf", 0xe9)
  for (loc in c(ib_utf8, "C")) {
    got <- hi_in_ctype(loc, expect_no_warning(resolve_url(
      abs_ref, "http://b.com/", url_standard = "whatwg", output = "serialized"
    )))
    expect_identical(got, "http://h.com/caf%EF%BF%BD", label = loc)
    # The parse seam reads the same octet the same way.
    expect_identical(
      hi_in_ctype(loc, serialize_url(abs_ref, "whatwg")), got, label = loc
    )
  }
})
