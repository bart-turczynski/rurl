# Golden tests for the internal query-filter engine (RURL-hcdygtph, T1).
# These lock the ordered-pair parser, the four modes, filter precedence, glob/
# case matching, the decode/re-encode + opaque contract, decode_plus, and stable
# sort BEFORE any public API wires into the memoized parse path (T2+). No public
# surface is exercised here; everything is reached via rurl::: internals.

# --- ._parse_query_pairs: ordered (key, value) substrate --------------------

test_that("._parse_query_pairs keeps order, dupes, and first-'=' split", {
  p <- rurl:::._parse_query_pairs("a=1&b=2&flag")
  expect_equal(p$key, c("a", "b", "flag"))
  expect_equal(p$value, c("1", "2", "")) # bare key -> empty value

  dup <- rurl:::._parse_query_pairs("a=1&b=2&a=3")
  expect_equal(dup$key, c("a", "b", "a")) # duplicate position kept
  expect_equal(dup$value, c("1", "2", "3"))

  first_eq <- rurl:::._parse_query_pairs("a=b=c")
  expect_equal(first_eq$key, "a") # split on FIRST '=' only
  expect_equal(first_eq$value, "b=c")
})

test_that("._parse_query_pairs skips empty segments and handles NA/empty", {
  skipped <- rurl:::._parse_query_pairs("a=1&&b=2")
  expect_equal(skipped$key, c("a", "b"))
  expect_equal(rurl:::._parse_query_pairs("")$key, character(0))
  expect_equal(rurl:::._parse_query_pairs(NA_character_)$key, character(0))
})

# --- Modes ------------------------------------------------------------------

test_that("query_handling modes: drop / keep / filter / allow", {
  f <- rurl:::._filter_query_params
  q <- "a=1&utm_source=x&gclid=9"

  expect_equal(f(q, "drop"), "") # drop == current clean_url default
  expect_equal(f(q, "keep"), "a=1&utm_source=x&gclid=9") # all, canonicalized
  expect_equal(f(q, "filter"), "a=1") # trackers gone, content kept

  # YouTube motivating case: ?v= survives filter.
  expect_equal(f("v=yzRJDl5GQVg&utm_source=x", "filter"), "v=yzRJDl5GQVg")

  # allow: only params_keep matches survive.
  expect_equal(f("a=1&v=2", "allow", params_keep = "v"), "v=2")
  # allow with empty params_keep degenerates to drop.
  expect_equal(f("a=1&v=2", "allow", params_keep = character(0)), "")
})

test_that("drop mode ignores everything (degenerate) and empty query -> ''", {
  f <- rurl:::._filter_query_params
  expect_equal(f("utm_source=x", "drop", params_keep = "utm_source"), "")
  expect_equal(f("", "filter"), "")
  expect_equal(f(NA_character_, "keep"), "")
})

# --- filter precedence ------------------------------------------------------

test_that("filter precedence: params_keep rescues empties AND beats denylist", {
  f <- rurl:::._filter_query_params
  # utm_source is denylisted AND empty AND empty-drop is on -- params_keep still
  # rescues it (highest precedence), and an empty value re-encodes to "key=".
  expect_equal(
    f("utm_source=&gclid=1", "filter",
      params_keep = "utm_source", empty_param_handling = "drop"),
    "utm_source="
  )
  # keep beats denylist for a non-empty value.
  expect_equal(
    f("utm_source=x&gclid=1", "filter", params_keep = "utm_source"),
    "utm_source=x"
  )
  # params_drop extends the denylist.
  expect_equal(f("a=1&foo=2", "filter", params_drop = "foo"), "a=1")
})

# --- glob + case matching ---------------------------------------------------

test_that("glob matching: only '*' special, on decoded names, case rules", {
  f <- rurl:::._filter_query_params
  # utm_* glob covers the whole family.
  expect_equal(f("utm_source=1&utm_medium=2&a=3", "filter"), "a=3")
  # matching is on the DECODED name (%75 == 'u').
  expect_equal(f("%75tm_source=x&a=1", "filter"), "a=1")
  # case-insensitive by default.
  expect_equal(f("UTM_Source=x&a=1", "filter"), "a=1")
  # params_case_sensitive = TRUE: differently-cased name is NOT the tracker.
  expect_equal(
    f("UTM_SOURCE=x&a=1", "filter", params_case_sensitive = TRUE),
    "UTM_SOURCE=x&a=1"
  )
})

test_that(".glob_to_regex treats only '*' as special", {
  expect_equal(rurl:::.glob_to_regex("utm_*"), "^utm_.*$")
  expect_equal(rurl:::.glob_to_regex("gclid"), "^gclid$")
  # '.' and '?' are literals, not regex metacharacters.
  expect_equal(rurl:::.glob_to_regex("a.b"), "^a\\.b$")
  expect_true(grepl(rurl:::.glob_to_regex("a.b"), "a.b"))
  expect_false(grepl(rurl:::.glob_to_regex("a.b"), "axb"))
})

# --- decode / re-encode + opaque contract -----------------------------------

test_that("re-encode: uppercase hex, embedded delimiters, empty -> key=", {
  f <- rurl:::._filter_query_params
  expect_equal(f("a=1%262", "keep"), "a=1%262") # decoded '&' re-encodes to %26
  expect_equal(f("a=%41", "keep"), "a=A")       # %41 normalizes to 'A'
  expect_equal(f("a=%2f", "keep"), "a=%2F")     # lowercase hex -> uppercase
  expect_equal(f("a=", "keep"), "a=")           # empty value -> "key="
  expect_equal(f("a", "keep"), "a=")            # ?a === ?a=
})

test_that("malformed percent tokens are opaque (byte-for-byte, no %25)", {
  f <- rurl:::._filter_query_params
  expect_equal(f("a=%zz", "keep"), "a=%zz") # non-hex after %
  expect_equal(f("a=%2", "keep"), "a=%2")   # only one hex digit
  expect_equal(f("a=%", "keep"), "a=%")     # lone %
  # opaque KEY also passes through and still matches by raw form for grouping.
  expect_equal(f("%zz=1", "keep"), "%zz=1")
})

# --- decode_plus (values only) ----------------------------------------------

test_that("decode_plus: '+' is space in values only, literal elsewhere", {
  f <- rurl:::._filter_query_params
  # FALSE: '+' is literal, canonicalizes to %2B.
  expect_equal(f("a=x+y", "keep", decode_plus = FALSE), "a=x%2By")
  # TRUE: '+' in the VALUE becomes space (%20); '+' in the KEY stays literal.
  expect_equal(f("a+b=x+y", "keep", decode_plus = TRUE), "a%2Bb=x%20y")
})

# --- sort + empty handling --------------------------------------------------

test_that("sort_params is a stable sort by decoded key", {
  f <- rurl:::._filter_query_params
  expect_equal(f("b=2&a=1", "keep", sort_params = TRUE), "a=1&b=2")
  # repeated key keeps its original relative order (2 before 1).
  expect_equal(f("b=2&a=1&b=1", "keep", sort_params = TRUE), "a=1&b=2&b=1")
})

test_that("empty_param_handling drops empty values unless rescued", {
  f <- rurl:::._filter_query_params
  expect_equal(f("a=&b=2", "keep", empty_param_handling = "keep"), "a=&b=2")
  expect_equal(f("a=&b=2", "keep", empty_param_handling = "drop"), "b=2")
  # ?a (bare) is empty too.
  expect_equal(f("a&b=2", "keep", empty_param_handling = "drop"), "b=2")
  # allow mode: empty allowed param drops under empty-drop (NOT rescued).
  expect_equal(
    f("v=&a=1", "allow", params_keep = "v", empty_param_handling = "drop"), ""
  )
})

# --- pluggable denylist source seam -----------------------------------------

test_that("denylist source registry resolves builtin and custom providers", {
  builtin <- rurl:::.resolve_query_denylist("builtin")
  expect_true("utm_*" %in% builtin)
  expect_true("gclid" %in% builtin)
  # parked/ambiguous tokens are deliberately OUT of v1.
  expect_false("cmp" %in% builtin)
  expect_false("icid" %in% builtin)

  # A custom vector source plugs in without touching the engine.
  rurl:::.register_query_denylist("test_vec", "foo*")
  on.exit(rm("test_vec", envir = rurl:::.QUERY_DENYLIST_REGISTRY), add = TRUE)
  expect_equal(
    rurl:::._filter_query_params(
      "foobar=1&a=2", "filter", denylist_source = "test_vec"
    ),
    "a=2"
  )

  # A function provider is materialized lazily.
  rurl:::.register_query_denylist("test_fn", function() "bar")
  on.exit(rm("test_fn", envir = rurl:::.QUERY_DENYLIST_REGISTRY), add = TRUE)
  expect_equal(rurl:::.resolve_query_denylist("test_fn"), "bar")

  # An unknown source is a loud error, not a silent no-op.
  expect_error(rurl:::.resolve_query_denylist("nope"), "Unknown query denylist")
})
