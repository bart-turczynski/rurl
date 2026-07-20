test_that("rurl_clear_caches resets memoization environments", {
  assign("k1", "v1", envir = rurl:::.rurl_cache$full_parse)
  assign("k2", "v2", envir = rurl:::.rurl_cache$puny_encode)
  assign("k3", "v3", envir = rurl:::.rurl_cache$puny_decode)

  old_envs <- list(
    full = rurl:::.rurl_cache$full_parse,
    puny_encode = rurl:::.rurl_cache$puny_encode,
    puny_decode = rurl:::.rurl_cache$puny_decode
  )

  rurl_clear_caches()

  expect_type(rurl:::.rurl_cache$full_parse, "environment")
  expect_type(rurl:::.rurl_cache$puny_encode, "environment")
  expect_type(rurl:::.rurl_cache$puny_decode, "environment")
  expect_false(identical(old_envs$full, rurl:::.rurl_cache$full_parse))
  expect_false(identical(old_envs$puny_encode, rurl:::.rurl_cache$puny_encode))
  expect_false(identical(old_envs$puny_decode, rurl:::.rurl_cache$puny_decode))
  expect_equal(ls(rurl:::.rurl_cache$full_parse), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_encode), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_decode), character(0))
})

test_that(".onLoad initializes the memoization caches and config", {
  assign("stale", TRUE, envir = rurl:::.rurl_cache$full_parse)
  assign("stale", TRUE, envir = rurl:::.rurl_cache$puny_encode)
  assign("stale", TRUE, envir = rurl:::.rurl_cache$puny_decode)

  ns <- asNamespace("rurl")
  was_locked <- bindingIsLocked(".rurl_cache", ns)
  if (was_locked) {
    unlockBinding(".rurl_cache", ns)
    withr::defer(lockBinding(".rurl_cache", ns), testthat::teardown_env())
  }

  rurl:::.onLoad(NULL, "rurl")

  # Caches are reinitialized and empty
  expect_equal(ls(rurl:::.rurl_cache$full_parse), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_encode), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_decode), character(0))

  # Config restored to historical defaults
  info <- rurl_cache_info()
  expect_equal(info$cache, c("full_parse", "puny_encode", "puny_decode"))
  expect_true(all(info$enabled))
})

# --- ASCII-safe cache keys (RURL-thwdukrr) -----------------------------------
#
# The caches are environments and the key is used as a variable name, so a
# non-ASCII key would force R to translate the name to the native encoding and
# emit "unable to translate ... to native encoding" under a non-UTF-8 LC_CTYPE.
# .cache_key_ascii() must therefore be total onto ASCII, injective, and derived
# from the declared encoding rather than the session locale.

test_that(".cache_key_ascii renders non-ASCII keys as pure ASCII", {
  hosts <- c("bücher.example", "中文.cn", "plain.example", "")
  keys <- rurl:::.cache_key_ascii(hosts)

  expect_type(keys, "character")
  expect_length(keys, length(hosts))
  expect_true(all(stringi::stri_enc_isascii(keys)))
  # ASCII-only inputs are unchanged by the escape.
  expect_identical(keys[3], "plain.example")
})

test_that(".cache_key_ascii is injective for distinct keys", {
  # Includes the adversarial pair: a literal backslash-u sequence must not
  # collide with the escape of the character it spells.
  inputs <- c(
    "bücher.example", "bucher.example", "bücher.example.",
    "a\\u00e9", "aé", "中文.cn", "中文.cn.",
    "x", "x\\", "x\\\\"
  )
  keys <- rurl:::.cache_key_ascii(inputs)
  # inputs 1 and 3 differ, 5 is the char, 4 is its literal spelling.
  expect_length(unique(keys), length(unique(inputs)))
  expect_false(keys[4] == keys[5])
  expect_true(all(stringi::stri_enc_isascii(keys)))
})

test_that(".cache_key_ascii depends on the declaration, not the locale", {
  utf8 <- "bücher.example"
  bytes <- rawToChar(charToRaw(utf8))
  expect_identical(Encoding(bytes), "unknown")
  # Undeclared UTF-8 bytes and the same string declared UTF-8 must agree: the
  # derivation reads the bytes as UTF-8 by declaration, never via the locale.
  expect_identical(
    rurl:::.cache_key_ascii(bytes),
    rurl:::.cache_key_ascii(utf8)
  )
  # A latin1-declared string is read through its declaration too.
  l1 <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(l1) <- "latin1"
  expect_true(stringi::stri_enc_isascii(rurl:::.cache_key_ascii(l1)))
  expect_identical(rurl:::.cache_key_ascii(l1), rurl:::.cache_key_ascii("café"))
})

test_that(".cache_key_ascii tolerates bytes that are not valid UTF-8", {
  bad1 <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  bad2 <- rawToChar(as.raw(c(0x61, 0xfe, 0x62)))
  keys <- rurl:::.cache_key_ascii(c(bad1, bad2, "ok"))
  expect_true(all(stringi::stri_enc_isascii(keys)))
  expect_length(unique(keys), 3L)
})

test_that("caches store non-ASCII keys under ASCII environment names", {
  rurl_clear_caches()
  host <- "bücher.example"
  Encoding(host) <- "UTF-8"

  expect_silent(rurl:::.cache_set("puny_encode", host, "xn--bcher-kva.example"))
  names_seen <- ls(rurl:::.rurl_cache$puny_encode)
  expect_true(all(stringi::stri_enc_isascii(names_seen)))
  expect_silent(hit <- rurl:::.cache_get("puny_encode", host))
  expect_identical(hit, "xn--bcher-kva.example")

  # Undeclared bytes for the same host resolve to the same entry.
  same <- rawToChar(charToRaw(host))
  expect_identical(
    rurl:::.cache_get("puny_encode", same), "xn--bcher-kva.example"
  )

  # A different host is a miss, not a collision.
  expect_identical(
    rurl:::.cache_get("puny_encode", "bücher.example."),
    rurl:::.rurl_cache_sentinel
  )
  rurl_clear_caches()
})

test_that("batch cache accessors agree with the scalar path (non-ASCII)", {
  rurl_clear_caches()
  hosts <- c("bücher.example", "中文.cn", "plain.example")
  expect_silent(rurl:::.cache_set_many("puny_encode", hosts, as.list(1:3)))
  names_seen <- ls(rurl:::.rurl_cache$puny_encode)
  expect_true(all(stringi::stri_enc_isascii(names_seen)))
  expect_identical(
    rurl:::.cache_get_many("puny_encode", hosts),
    list(1L, 2L, 3L),
    ignore_attr = TRUE
  )
  expect_identical(rurl:::.cache_get("puny_encode", hosts[2]), 2L)
  rurl_clear_caches()
})

test_that("parsing a non-ASCII host warms the cache without warnings", {
  rurl_clear_caches()
  url <- "https://bücher.example/pfad?q=é"
  Encoding(url) <- "UTF-8"

  expect_no_warning(first <- safe_parse_url(url))
  entries <- rurl_cache_info()$entries
  expect_true(all(stringi::stri_enc_isascii(ls(rurl:::.rurl_cache$full_parse))))
  expect_gt(entries[1], 0)

  # Second identical parse is a cache HIT: no new entries, identical result.
  expect_no_warning(second <- safe_parse_url(url))
  expect_identical(second, first)
  expect_identical(rurl_cache_info()$entries, entries)
  rurl_clear_caches()
})
