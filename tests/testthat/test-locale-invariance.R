# Locale invariance of rurl's output (RURL-izzuenjx).
#
# The contract these tests lock in, stated plainly:
#
#   1. rurl's output does not depend on `LC_CTYPE`. The same input yields the
#      same VALUES and the same `Encoding()` marks under `LC_ALL=C` and under a
#      UTF-8 locale. The cross-locale half of that is enforced by CI: the
#      `Tests (LC_ALL=C)` cell in .github/workflows/verify.yml runs this whole
#      suite under a genuinely non-UTF-8 LC_CTYPE. What THIS file asserts is
#      the in-process half -- the marks and the bytes -- which is what makes
#      the two runs agree.
#   2. Every returned character component carries `Encoding() == "UTF-8"` when
#      it holds non-ASCII. R never marks a pure-ASCII string, so an ASCII
#      component correctly reports "unknown"; that is asserted too, so a future
#      reader does not "fix" it into a bug.
#   3. `enc2utf8()` and `iconv()`-from-native must not reappear in the parse
#      path. Both transcode FROM THE SESSION LOCALE, which is precisely the
#      defect the marks replaced (`Encoding<-` declares, it never converts).
#
# Every assertion here would fail if one of the six fixes that established the
# contract were reverted (e9defab, c6a0457, b26a334, 62a24a9, d7181a1, 0a339ce).
#
# All non-ASCII literals below are written as \u escapes on purpose: an escape
# parses to a UTF-8-marked string in ANY locale, so these tests cannot
# themselves become the locale-dependent thing they are testing.

# http://<Cyrillic "primer">.<Cyrillic "rf">/<Cyrillic "p"> -- a wholly
# non-ASCII URL whose registrable domain AND public suffix are both IDN, so
# host / domain / tld / *_unicode / path all carry non-ASCII at once.
idn_url <- "http://\u043f\u0440\u0438\u043c\u0435\u0440.\u0440\u0444/\u043f"
ascii_url <- "http://example.com/p"

# Components that can carry non-ASCII, and the ones that are ASCII by
# construction (percent-encoded or a fixed vocabulary).
utf8_components <- c(
  "original_url", "host", "path", "domain", "tld", "domain_unicode",
  "tld_unicode", "clean_url"
)
ascii_components <- c("scheme", "domain_ascii", "tld_ascii", "parse_status")


# --- 1. Encoding() marks on the scalar parser --------------------------------

test_that("an IDN URL's non-ASCII components are declared UTF-8", {
  res <- safe_parse_url(idn_url)
  for (nm in utf8_components) {
    expect_identical(Encoding(res[[nm]]), "UTF-8", label = nm)
  }
})

test_that("ASCII-by-construction components are correctly unmarked", {
  # NOT a gap: R refuses to attach an encoding mark to a pure-ASCII string, so
  # "unknown" IS the correct answer for these and `.mark_result_utf8()` is a
  # deliberate no-op on them. Do not "fix" this into an expectation of "UTF-8"
  # -- it is unreachable.
  res <- safe_parse_url(idn_url)
  for (nm in ascii_components) {
    expect_identical(Encoding(res[[nm]]), "unknown", label = nm)
  }

  # A fully ASCII URL: every character component stays unmarked.
  ascii <- safe_parse_url(ascii_url)
  chr <- vapply(ascii, is.character, logical(1))
  expect_identical(
    unique(Encoding(unlist(ascii[chr], use.names = FALSE))), "unknown"
  )
})


# --- 2. Encoding() marks on the vector parser and the accessors --------------

test_that("safe_parse_urls() marks columns the same as safe_parse_url()", {
  # The Stage-B assembly chokepoint (c6a0457) is a different code path from the
  # scalar one; both must land on the same contract.
  df <- safe_parse_urls(c(idn_url, ascii_url))
  for (nm in utf8_components) {
    expect_identical(Encoding(df[[nm]][1]), "UTF-8", label = nm)
    expect_identical(Encoding(df[[nm]][2]), "unknown", label = nm)
  }
  for (nm in ascii_components) {
    expect_identical(Encoding(df[[nm]]), c("unknown", "unknown"), label = nm)
  }
})

test_that("accessors return UTF-8-marked values for an IDN URL", {
  expect_identical(Encoding(get_host(idn_url)), "UTF-8")
  expect_identical(Encoding(get_domain(idn_url)), "UTF-8")
  expect_identical(Encoding(get_tld(idn_url)), "UTF-8")
  expect_identical(Encoding(get_clean_url(idn_url)), "UTF-8")
  # ...and the unmarked-ASCII counterpart, for the same reason as above.
  expect_identical(Encoding(get_host(ascii_url)), "unknown")
  expect_identical(Encoding(get_domain(ascii_url)), "unknown")
})


# --- 3. Byte-level equality --------------------------------------------------

test_that("non-ASCII output is byte-identical to its UTF-8 encoding", {
  # A mark is not enough: a transcoding regression would change the BYTES.
  # These are the literal UTF-8 octets, spelled out rather than eyeballed from
  # a printed string.
  res <- safe_parse_url("http://m\u00fcnchen.de/\u00e9cole")

  expect_identical(
    charToRaw(res$host),
    as.raw(c(0x6d, 0xc3, 0xbc, 0x6e, 0x63, 0x68, 0x65, 0x6e, 0x2e, 0x64, 0x65))
  )
  expect_identical(
    charToRaw(res$path),
    as.raw(c(0x2f, 0xc3, 0xa9, 0x63, 0x6f, 0x6c, 0x65))
  )
  # domain / domain_unicode carry the same octets as the host they came from,
  # even though they are round-tripped through pslr and punycoder.
  expect_identical(charToRaw(res$domain), charToRaw(res$host))
  expect_identical(charToRaw(res$domain_unicode), charToRaw(res$host))

  # The IDN public suffix, likewise: Cyrillic "rf" is U+0440 U+0444.
  idn <- safe_parse_url(idn_url)
  expect_identical(charToRaw(idn$tld), as.raw(c(0xd1, 0x80, 0xd1, 0x84)))
  expect_identical(charToRaw(idn$tld_unicode), charToRaw(idn$tld))
  expect_identical(charToRaw(idn$path), as.raw(c(0x2f, 0xd0, 0xbf)))
})


# --- 4. The specific regressions the determinism epic fixed ------------------

test_that("domain is not NA for an IDN host", {
  # Pre-e9defab, the host reached pslr as unmarked native bytes; under a
  # non-UTF-8 LC_CTYPE pslr re-decoded them in the session locale and returned
  # NA, so domain / tld / *_ascii / *_unicode were ALL NA in a C locale and
  # populated in a UTF-8 one.
  for (u in c(idn_url, "http://xn--e1afmkfd.xn--p1ai/")) {
    res <- safe_parse_url(u)
    expect_false(is.na(res$domain))
    expect_false(is.na(res$tld))
    expect_identical(res$domain_ascii, "xn--e1afmkfd.xn--p1ai")
    expect_identical(res$tld_ascii, "xn--p1ai")
  }
})

test_that("a non-ASCII path percent-encodes to the UTF-8 octets", {
  # Pre-d7181a1 the path encoder read `charToRaw(enc2utf8(x))`. Under LC_ALL=C
  # `enc2utf8()` RE-DECODED the already-UTF-8 octets as native, so the encoder
  # saw the literal text "<c3><a9>" and emitted `/%3Cc3%3E%3Ca9%3Ecole`.
  encoded <- get_clean_url("http://ex.com/\u00e9cole", path_encoding = "encode")
  expect_identical(encoded, "http://ex.com/%C3%A9cole")
  expect_false(grepl("%3C", encoded, fixed = TRUE))
})

test_that("a U+FFFF mailto local part does not error", {
  # Base `toupper()` routes through `utf8towcs()`, which ERRORS on the U+FFFF
  # noncharacter; 62a24a9 pinned the site to `.ascii_toupper()`. Primary
  # coverage lives in test-email-diagnostics.R; repeated here because it is one
  # of the named cases this locale-invariance gate exists to hold.
  expect_no_error(
    out <- get_mailto_recipients(
      "mailto:a\uffffb@example.com", scheme_acceptance = "general"
    )
  )
  expect_identical(nrow(out), 1L)
  expect_identical(out$mailto_local_part_form, "invalid")
})

test_that("a non-ASCII quoted SMTPUTF8 local part classifies stably", {
  # `.email_valid_quoted_content()` splits the percent-decoded local part into
  # characters and reads each code point (RURL-iwtrwayz). Before it declared
  # its input UTF-8, that split ran byte-wise under LC_ALL=C, so a multi-byte
  # scalar leaked into the qtext check and the SMTPUTF8 verdict flipped by
  # locale. `mailto:"<U+00E9>"@example.com`, percent-encoded, must project to
  # `smtputf8` in every locale.
  out <- get_mailto_recipients(
    "mailto:%22%C3%A9%22@example.com",
    scheme_acceptance = "general", smtp_wire = TRUE
  )
  expect_identical(out$smtp_envelope_wire_mode, "smtputf8")
  expect_true(out$smtp_envelope_address_requires_smtputf8)
})


# --- 4b. The rfc3986 authority: no throw, no mark-dependent verdict ----------
#
# RURL-kmpnbvdl, two symptoms of one cause. Under `url_standard = "rfc3986"`
# an authority carrying a raw high byte used to (a) THROW `invalid multibyte
# string` out of the whole vectorized call, and (b) where it did not throw,
# decide accept-vs-reject differently depending on `LC_CTYPE`.
#
# Every input here is built from RAW OCTETS with `rawToChar()`, never from a
# source literal, and is therefore mark-"unknown" -- which is the shape that
# made the verdict locale-dependent, and the shape real user input has.

# "http://<80>/p": a lone continuation byte, so NOT valid UTF-8.
bad_authority <- rawToChar(as.raw(
  c(0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x80, 0x2f, 0x70)
))
# "ftp://<C3><A9>:<C3><A9>@example.com/p": VALID UTF-8 (U+00E9) in userinfo.
eacute_userinfo <- rawToChar(as.raw(
  c(0x66, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0xC3, 0xA9, 0x3a, 0xC3, 0xA9, 0x40,
    0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x63, 0x6f, 0x6d,
    0x2f, 0x70)
))

test_that("an undecodable rfc3986 authority rejects instead of throwing", {
  expect_no_error(h <- get_host(bad_authority, url_standard = "rfc3986"))
  expect_identical(h, NA_character_)
  expect_no_error(
    st <- get_parse_status(bad_authority, url_standard = "rfc3986")
  )
  expect_identical(st, "error")
  expect_no_error(
    s <- serialize_url(bad_authority, standard = "rfc3986")
  )
  expect_identical(s, NA_character_)
})

test_that("one undecodable row does not poison the rest of the batch", {
  # The headline severity: `substring()` aborted the CALL, not the row, so
  # every good URL in the same vector was lost. `safe_parse_urls()` is named
  # for the promise that broke.
  batch <- c("http://a.example.com/", bad_authority, "http://b.example.com/")
  expect_identical(
    get_host(batch, url_standard = "rfc3986"),
    c("a.example.com", NA, "b.example.com")
  )
  expect_identical(
    get_parse_status(batch, url_standard = "rfc3986"),
    c("ok", "error", "ok")
  )
})

# --- 4c. The same defect where the mask is a CONJUNCTION ---------------------
#
# The first pass at RURL-kmpnbvdl closed the authority `@` split and stopped
# there, because the corpus that found the defect varied ONE octet at a time.
# Every mask past that split is a conjunction: reaching it needs the bad octet
# AND a second character that makes the row eligible -- a sub-delim, a
# percent-triplet, a repeated "@". A one-octet-at-a-time corpus cannot build a
# pair, so it scored the partial fix as complete. These are the shapes that
# still threw afterwards, one per surviving site:
#
#   `.shim_whatwg_host_charset_vec()` reassembly  -- stri_length() on the
#     authority, reached only on a `restore` row (bad octet + charset mask).
#   `.pct_hex_upper()`                            -- gsub(perl = TRUE).
#   `.encode_excess_authority_at_vec()` repair    -- gsub(fixed = TRUE).
#
# Note `<C3><28>` needs no added sub-delim: "(" IS one, so the invalid pair is
# its own conjunction. That is why it threw where a lone `<80>` did not.

# "http://<80>!/p" -- undecodable octet AND a sub-delim in the same host.
bad_host_subdelim <- rawToChar(as.raw(
  c(0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x80, 0x21, 0x2f, 0x70)
))
# "http://<C3>(/p" -- a truncated 2-byte sequence whose trailing octet is "(".
bad_host_pair <- rawToChar(as.raw(
  c(0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0xC3, 0x28, 0x2f, 0x70)
))
# "http://<80>%7f/p" -- undecodable octet AND a lowercase percent-triplet, the
# pair that reaches `.pct_hex_upper()`.
bad_host_triplet <- rawToChar(as.raw(
  c(0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x80, 0x25, 0x37, 0x66,
    0x2f, 0x70)
))
# "http://<80>@!@e.com/p" -- undecodable octet AND a repeated "@", the pair
# that reaches the excess-"@" repair.
bad_userinfo_at2 <- rawToChar(as.raw(
  c(0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x80, 0x40, 0x21, 0x40,
    0x65, 0x2e, 0x63, 0x6f, 0x6d, 0x2f, 0x70)
))

test_that("an undecodable HOST rejects even when a mask also fires", {
  in_host <- c(bad_host_subdelim, bad_host_pair, bad_host_triplet)
  for (std in c("whatwg", "rfc3986")) {
    for (u in in_host) {
      expect_no_error(st <- get_parse_status(u, url_standard = std))
      expect_identical(st, "error")
      expect_no_error(h <- get_host(u, url_standard = std))
      expect_identical(h, NA_character_)
      expect_no_error(s <- serialize_url(u, standard = std))
      expect_identical(s, NA_character_)
    }
  }
})

test_that("an undecodable USERINFO follows each profile's own rule", {
  # Not a rejection under WHATWG, and that is correct rather than tolerated:
  # WHATWG parses a scalar-value string, so the undecodable octet becomes
  # U+FFFD and percent-encodes as %EF%BF%BD in the userinfo, leaving a valid
  # host. rfc3986 has no such substitution rule and rejects. Both answers are
  # byte-identical to what this input produced before RURL-kmpnbvdl -- the
  # totality fix repaired THROWS, it did not move an accepted row.
  expect_no_error(
    st <- get_parse_status(bad_userinfo_at2, url_standard = "whatwg")
  )
  expect_identical(st, "ok")
  expect_identical(
    get_host(bad_userinfo_at2, url_standard = "whatwg"), "e.com"
  )
  expect_identical(
    serialize_url(bad_userinfo_at2, standard = "whatwg"),
    "http://%EF%BF%BD%40!@e.com/p"
  )

  expect_no_error(
    st <- get_parse_status(bad_userinfo_at2, url_standard = "rfc3986")
  )
  expect_identical(st, "error")
  expect_identical(
    get_host(bad_userinfo_at2, url_standard = "rfc3986"), NA_character_
  )
})

test_that("a masked undecodable row does not poison the batch either", {
  for (u in c(bad_host_subdelim, bad_host_pair, bad_host_triplet,
              bad_userinfo_at2)) {
    batch <- c("http://a.example.com/", u, "http://b.example.com/")
    expect_identical(
      get_parse_status(batch, url_standard = "rfc3986"),
      c("ok", "error", "ok")
    )
    expect_identical(
      get_host(batch, url_standard = "rfc3986"),
      c("a.example.com", NA, "b.example.com")
    )
  }
})

test_that("the decodable-guarded helpers leave undecodable input alone", {
  # `.gsub_decodable()` returns an undecodable element BYTE-IDENTICAL rather
  # than substituting on it, so a rejected row is never rewritten on its way
  # out. Decodable elements substitute normally.
  undecodable <- rawToChar(as.raw(c(0x61, 0x80, 0x25, 0x37, 0x66)))
  x <- c("%7f", undecodable, NA)
  out <- .gsub_decodable("%([0-9a-f]{2})", "%\\U\\1", x, perl = TRUE)
  expect_identical(out[[1L]], "%7F")
  expect_identical(
    as.integer(charToRaw(out[[2L]])),
    as.integer(charToRaw(undecodable))
  )
  expect_true(is.na(out[[3L]]))
})

test_that("the undecodable short-circuit is exactly as severe as the walk", {
  # `.rfc3986_generic_uri_ok()` rejects an undecodable element WITHOUT walking
  # the grammar, justified by: the walk classifies every byte, every production
  # is ASCII apart from the `-\U0010FFFF` scalar-value tolerance, and an
  # invalid octet sequence denotes no scalar value. The way that argument could
  # fail is if the walk's decoder were MORE PERMISSIVE than `validUTF8()` --
  # repairing an overlong or surrogate form into a character the grammar likes,
  # so the walk would have accepted where the short-circuit rejects.
  #
  # It is not: ICU maps every one of these to U+FFFD, never back to the ASCII
  # they overlong-encode. `C0 AF` is the classic overlong "/" and must not
  # become a path separator.
  invalid <- list(
    c(0xC0, 0xAF), c(0xC1, 0xA1), c(0xE0, 0x81, 0xA1), c(0xC0, 0x80),
    c(0xED, 0xA0, 0x80), c(0xED, 0xB0, 0x80), c(0xF8, 0x88, 0x80, 0x80, 0x80),
    c(0xF5, 0x90, 0x80, 0x80), c(0xE2, 0x82), 0x80
  )
  for (seq in invalid) {
    u <- rawToChar(as.raw(c(
      0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x61, seq, 0x62, 0x2f, 0x70
    )))
    expect_false(validUTF8(u))
    expect_false(.rfc3986_generic_uri_ok(u)$ok)
    # the decisive half: ICU does not repair it into bare ASCII
    decoded <- suppressWarnings(
      stringi::stri_encode(rawToChar(as.raw(seq)), "UTF-8", "UTF-8")
    )
    expect_true(grepl("[^\001-\177]", decoded, useBytes = TRUE))
  }

  # The symmetric risk: a VALID sequence must never be short-circuited. These
  # are the tolerance's own endpoints and its awkward interior members.
  valid <- list(
    c(0xC2, 0x80), c(0xC3, 0xA9), c(0xE2, 0x82, 0xAC),
    c(0xF4, 0x8F, 0xBF, 0xBF), c(0xEF, 0xBF, 0xBF), c(0xEF, 0xBF, 0xBD)
  )
  for (seq in valid) {
    u <- rawToChar(as.raw(c(
      0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x61, seq, 0x62, 0x2f, 0x70
    )))
    expect_true(validUTF8(u))
    expect_true(.rfc3986_generic_uri_ok(u)$ok)
  }
})

test_that(".byte_length() counts octets and is total", {
  # It exists so a length feeding a `.byte_substring*()` offset is measured in
  # the SAME unit as the cut, and because `stringi::stri_length()` throws on
  # exactly the input this seam must survive.
  declared <- rawToChar(as.raw(c(0x61, 0x80)))
  Encoding(declared) <- "UTF-8"
  expect_identical(
    .byte_length(c("abc", declared, NA, "")),
    c(3L, 2L, NA_integer_, 0L)
  )
})

test_that(".byte_substring() matches substring() including NA indices", {
  # The helper documents itself as clamping "exactly as substring() does". On
  # pure ASCII the two MUST agree, byte and character indices being the same
  # there -- so this is a real oracle, not a restatement of the implementation.
  # `last = NA` is the ONE deliberate difference (it is the default, meaning
  # "to the end"), so it is excluded rather than asserted away.
  idx <- c(NA, -3L, -1L, 0L, 1L, 2L, 5L, 6L, 7L, 100L)
  for (s in c("abcdef", "a", "")) {
    for (first in idx) {
      for (last in idx[!is.na(idx)]) {
        expect_identical(
          .byte_substring(s, first, last), substring(s, first, last),
          info = paste("s=", s, "first=", first, "last=", last)
        )
      }
    }
  }
})

test_that("the rfc3986 verdict does not depend on the encoding mark", {
  # In-process half of locale invariance (see this file's header): under
  # `LC_ALL=C` an "unknown"-marked string is read as native, under a UTF-8
  # locale as UTF-8. Asserting that BOTH marks give the same answer is the
  # same statement, testable in one session.
  #
  # The mechanism, for whoever reads this next: the grammar walk mixes
  # `stri_locate_*()` (code-point indices) with `substring()` (native-character
  # indices). On an unmarked multi-byte string those units diverge under
  # `LC_ALL=C`, the authority is sliced at the wrong offset, and `ok-ftp`
  # became `error`.
  marked <- eacute_userinfo
  Encoding(marked) <- "UTF-8"
  for (fn in list(get_host, get_parse_status, get_user)) {
    expect_identical(
      fn(eacute_userinfo, url_standard = "rfc3986"),
      fn(marked, url_standard = "rfc3986")
    )
  }
  expect_identical(
    serialize_url(eacute_userinfo, standard = "rfc3986"),
    serialize_url(marked, standard = "rfc3986")
  )
  # ...and the answer both marks agree on is the accepting one: U+00E9 is a
  # Unicode scalar value, which the RFC 3986 gate admits wherever a data
  # character is admitted (ADR 0002 / D1 rule 5).
  expect_identical(
    get_parse_status(eacute_userinfo, url_standard = "rfc3986"), "ok-ftp"
  )
  expect_identical(
    get_host(eacute_userinfo, url_standard = "rfc3986"), "example.com"
  )
})

test_that("undecodable bytes reject on every rfc3986-gated surface", {
  # The trigger class, all shapes: lone continuation byte, truncated 2- and
  # 3-byte starts, an overlong encoding and a UTF-16 surrogate. None denotes a
  # scalar value, so none can match an RFC 3986 production.
  octets <- list(
    "lone-80" = 0x80, "lone-C3" = 0xC3, "trunc-E4BD" = c(0xE4, 0xBD),
    "overlong" = c(0xC0, 0xAF), "surrogate" = c(0xED, 0xA0, 0x80)
  )
  prefix <- c(0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f)  # "http://"
  for (nm in names(octets)) {
    u <- rawToChar(as.raw(c(prefix, octets[[nm]], 0x2f, 0x70)))
    expect_no_error(st <- get_parse_status(u, url_standard = "rfc3986"))
    expect_identical(st, "error", info = nm)
    # ...and in the userinfo, which is the other half of the authority.
    v <- rawToChar(as.raw(c(
      prefix, 0x75, octets[[nm]], 0x40, 0x68, 0x2e, 0x63, 0x6f, 0x6d, 0x2f
    )))
    expect_no_error(st2 <- get_parse_status(v, url_standard = "rfc3986"))
    expect_identical(st2, "error", info = nm)
  }
})

test_that("the byte-slice helpers cut positions, not characters", {
  # `.byte_substring()` / `.last_byte_index()` / `.first_byte_index()` are the
  # shared seam helpers. They must survive a DECLARED-UTF-8 string holding
  # invalid octets -- which is what every `stri_match_first_regex()` capture
  # is, since stringi marks its captures unconditionally.
  s <- rawToChar(as.raw(c(0x61, 0x80, 0x40, 0x68, 0x2e, 0x63)))  # "a<80>@h.c"
  Encoding(s) <- "UTF-8"
  expect_identical(rurl:::.last_byte_index(s, "@"), 3L)
  expect_identical(rurl:::.first_byte_index(s, "@"), 3L)
  expect_no_error(tail <- rurl:::.byte_substring(s, 4L))
  expect_identical(charToRaw(tail), as.raw(c(0x68, 0x2e, 0x63)))
  expect_no_error(head_part <- rurl:::.byte_substring(s, 1L, 2L))
  expect_identical(charToRaw(head_part), as.raw(c(0x61, 0x80)))
  # Absent needle, and out-of-range indices clamping to "" as substring() does.
  expect_identical(rurl:::.last_byte_index(s, "?"), 0L)
  expect_identical(rurl:::.byte_substring(s, 99L), "")
  # A multi-byte scalar is cut by BYTE, so the helper never re-indexes by
  # locale: "é" is 2 bytes whatever LC_CTYPE says.
  e <- rawToChar(as.raw(c(0xC3, 0xA9, 0x40, 0x78)))
  expect_identical(rurl:::.last_byte_index(e, "@"), 3L)
  expect_identical(
    charToRaw(rurl:::.byte_substring(e, 1L, 2L)), as.raw(c(0xC3, 0xA9))
  )
})


# --- 5. Meta-guard: no transcoding-from-native in the package ----------------

# Which namespace functions reference `fn`, and how many times. Reads the
# parsed BODY (`all.names()`), not the file text, so comments -- including the
# several that discuss `enc2utf8()` by name -- cannot produce a false hit, and
# the guard works whether or not package sources ship with the tests.
ns_callers <- function(fn) {
  ns <- asNamespace("rurl")
  nms <- ls(ns, all.names = TRUE)
  counts <- vapply(nms, function(nm) {
    obj <- get(nm, envir = ns)
    if (!is.function(obj)) {
      return(0L)
    }
    sum(all.names(body(obj)) == fn)
  }, integer(1))
  counts[counts > 0L]
}

test_that("enc2utf8() is gone from the whole namespace", {
  # `enc2utf8()` transcodes FROM the session locale. It must not appear
  # anywhere in the parse path.
  #
  # The last two calls lived in `.email_valid_quoted_content()`
  # (R/email-diagnostics.R) and were removed by RURL-iwtrwayz: the function now
  # declares its percent-decoded input UTF-8 with `Encoding<-` before the
  # per-character split, so `utf8ToInt()` reads code points directly. Any new
  # caller of `enc2utf8()` is a regression until proven otherwise.
  expect_identical(
    ns_callers("enc2utf8"), setNames(integer(0), character(0))
  )
})

test_that("iconv() is confined to the two UTF-8-to-UTF-8 sanitizers", {
  # The surviving `iconv()` calls are `from = "UTF-8", to = "UTF-8", sub = ""`
  # in R/domain.R -- an invalid-sequence scrubber, NOT a from-native
  # transcode, so they carry no locale dependency. Any new caller is a
  # regression until proven otherwise.
  expect_identical(
    ns_callers("iconv"),
    c(.punycode_to_unicode = 1L, .punycode_to_unicode_vec = 1L)
  )
})
