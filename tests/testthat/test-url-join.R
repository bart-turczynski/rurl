# Tests for the v3 identity-keyed join family (R/url-join.R), the engine half of
# RURL-mihbyjsr. Sections follow the contract they verify:
#
#   design/work/url-v3/contracts/key-join-contracts.md
#     "Eligibility and collision rows"  :122-130
#     "Six-join matrix"                 :138-145
#     "Cross-cutting join rows"         :149-165
#   design/work/url-v3/decisions/P3.2-key-join-closure.md  D-C..D-H
#   design/work/url-v3/decisions/P3.1-identity-canonical-join.md  D-C, D-D
#
# Nothing here is exported yet; the tests drive the internal engine, exactly as
# test-url-key.R does. Row-ORDER assertions deliberately use distinguishable
# payload values so a wrong stable order fails rather than passing by symmetry.

# Two frames whose duplicate keys and case-differing hosts make every order,
# multiplicity and retention rule observable.
jx <- function() {
  data.frame(
    URL = c(
      "https://Example.com/p",   # 1  matches y1, y3 (host case is not identity)
      "http://a.com/1",          # 2  keyable, no match
      "https://example.com/p",   # 3  same key as row 1
      "not a url"                # 4  non-keyable
    ),
    vx = 1:4, stringsAsFactors = FALSE
  )
}

jy <- function() {
  data.frame(
    URL = c(
      "https://example.com/p",   # 1
      "http://b.com/9",          # 2  keyable, no match
      "https://example.com/p"    # 3  duplicate key of y1
    ),
    vy = c("m", "n", "o"), stringsAsFactors = FALSE
  )
}

# --- eligibility vocabulary (:122-130, P3.1 D-C) -----------------------------

test_that("the eligibility vocabulary is projected, one label per row", {
  u <- c(
    "http://a.com/1", "http://example.invalidtld/", "not a url",
    "mailto:a@b.com", NA_character_, ""
  )
  e <- .url_join_eligibility_vec(u)

  expect_identical(e[[1]], "ok")
  expect_identical(e[[2]], "warning")
  expect_identical(e[[3]], "invalid-parse")
  expect_identical(e[[4]], "relative-or-opaque-reference")
  expect_identical(e[[5]], "missing-input")
  expect_identical(e[[6]], "empty-input")
  expect_true(all(e %in% .URL_JOIN_ELIGIBILITY))
})

test_that("missing input is never conflated with invalid parse (D-C)", {
  e <- .url_join_eligibility_vec(c(NA_character_, "not a url", ""))
  expect_identical(e, c("missing-input", "invalid-parse", "empty-input"))
})

test_that("eligibility is length- and zero-length-preserving", {
  expect_identical(.url_join_eligibility_vec(character(0)), character(0))
  expect_length(.url_join_eligibility_vec(rep("http://a.com/", 5L)), 5L)
})

# The vocabulary is SETTLED, so the term is enumerated even though the family's
# `scheme_acceptance = "general"` posture cannot produce it. Asserting the gap
# keeps it from being silently narrowed away -- the same discipline
# test-parse-verdicts.R applies to the L3 annotation-state enum.
test_that("unsupported-scheme is enumerated but unreachable here", {
  expect_true("unsupported-scheme" %in% .URL_JOIN_ELIGIBILITY)

  u <- c("mailto:a@b.com", "javascript:void(0)", "gopher://h/x", "data:,hi",
         "http://a.com/", "not a url")
  expect_false(any(.url_join_eligibility_vec(u) == "unsupported-scheme"))

  # The reason it is unreachable, stated as a fact rather than an assumption:
  # nothing is rejected at L2 under general acceptance.
  opts <- .url_key_parse_options(.url_key_policy_spec(), NULL)
  v <- attr(._parse_urls_cached(u, opts), "verdicts")
  expect_false(any(v$layer2_policy_verdict == "rejected-scheme"))
})

# The load-bearing agreement that lets `warned` ride the shipped verdict layers
# instead of a second notion of validity. Both classes must be populated, or the
# assertion would pass vacuously on an all-keyable corpus.
test_that("keyability agrees with the L1 syntax verdict, matched posture", {
  u <- c(
    "http://a.com/1", "https://Example.com/p", "mailto:a@b.com",
    "example.com", "not a url", "", NA_character_, "http://",
    "http://example.invalidtld/", "1.2.3.4", "user@example.com",
    "http://[::1]/x", "ftp://h.com:21/", "javascript:void(0)"
  )
  pol <- .url_key_policy_spec()
  keyable <- attr(.url_key_compute_vec(u, pol), "keyability") == "ok"
  opts <- .url_key_parse_options(pol, NULL)
  l1 <- attr(._parse_urls_cached(u, opts), "verdicts")$layer1_syntax_verdict

  expect_true(any(keyable))
  expect_false(all(keyable))
  expect_identical(keyable, l1 == "pass")
})

test_that("warned rows are keyable and carry an accept-with-note", {
  s <- .url_join_side_state(
    c("http://a.com/1", "http://example.invalidtld/", "user@example.com",
      "not a url"),
    .url_key_policy_spec()
  )
  expect_identical(s$warned, c(FALSE, TRUE, TRUE, FALSE))
  # `warned` is masked by `keyable`, so no row is both invalid and warning --
  # which is what keeps the two axes independent.
  expect_true(all(s$keyable[s$warned]))
})

# --- `by`: the named-vector idiom (KJ-O5 / P3.2 D-E) -------------------------

test_that("`by` accepts a bare string and a length-one named vector", {
  y2 <- setNames(jy(), c("link", "vy"))
  bare <- .url_join_impl(jx(), jy(), by = "URL", type = "inner")
  named <- .url_join_impl(jx(), y2, by = c(URL = "link"), type = "inner")

  expect_identical(nrow(bare), nrow(named))
  expect_identical(bare$vy, named$vy)
  # Differing names need no suffix, so both originals survive unsuffixed.
  expect_named(named, c("URL", "vx", "link", "vy"))
})

test_that("`by` rejects malformed selectors as an early input error", {
  x <- jx()
  y <- jy()
  expect_error(.url_join_impl(x, y, by = c("URL", "URL"), type = "inner"),
               class = "rurl_url_join_input_error")
  expect_error(.url_join_impl(x, y, by = NA_character_, type = "inner"),
               class = "rurl_url_join_input_error")
  expect_error(.url_join_impl(x, y, by = "", type = "inner"),
               class = "rurl_url_join_input_error")
  expect_error(.url_join_impl(x, y, by = 1, type = "inner"),
               class = "rurl_url_join_input_error")
  expect_error(.url_join_impl(x, y, by = "nope", type = "inner"),
               class = "rurl_url_join_input_error")
  expect_error(.url_join_impl(x, y, by = c(URL = "nope"), type = "inner"),
               class = "rurl_url_join_input_error")
})

test_that("the join column must be character or factor", {
  x <- data.frame(URL = 1:2, vx = 1:2)
  expect_error(.url_join_impl(x, jy(), by = "URL", type = "inner"),
               class = "rurl_url_join_input_error")
})

test_that("a factor join column is accepted and keys as its labels", {
  xf <- data.frame(URL = factor(c("https://Example.com/p", "http://a.com/1")),
                   vx = 1:2)
  res <- .url_join_impl(xf, jy(), by = "URL", type = "inner")
  expect_identical(nrow(res), 2L)
  expect_identical(res$vy, c("m", "o"))
})

# --- suffix and name repair (KJ-O6 / P3.2 D-F) -------------------------------

test_that("overlapping non-key names are suffixed deterministically", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "inner")
  expect_named(res, c("URL.x", "vx", "URL.y", "vy"))

  res2 <- .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                         suffix = c("_A", "_B"))
  expect_named(res2, c("URL_A", "vx", "URL_B", "vy"))
})

test_that("both original URL columns are preserved, never overwritten (:159)", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "inner")
  # The two sides' source strings differ and are BOTH kept: they merely compare
  # equal under the identity key, so neither is the other's canonical form.
  expect_identical(res$URL.x[[1]], "https://Example.com/p")
  expect_identical(res$URL.y[[1]], "https://example.com/p")
})

test_that("suffix ambiguity is an early error, never a silent repair", {
  # `vx` collides with the suffixed name `vx` would receive.
  x <- data.frame(URL = "https://example.com/p", v = 1L, v.x = 2L,
                  stringsAsFactors = FALSE)
  y <- data.frame(URL = "https://example.com/p", v = 3L,
                  stringsAsFactors = FALSE)
  expect_error(.url_join_impl(x, y, by = "URL", type = "inner"),
               class = "rurl_url_join_suffix_error")
})

test_that("duplicate input column names are rejected before any join work", {
  x <- jx()
  names(x) <- c("URL", "URL")
  expect_error(.url_join_impl(x, jy(), by = "URL", type = "inner"),
               class = "rurl_url_join_suffix_error")
})

test_that("`suffix` must be two non-NA strings", {
  x <- jx()
  y <- jy()
  expect_error(.url_join_impl(x, y, by = "URL", type = "inner", suffix = ".x"),
               class = "rurl_url_join_suffix_error")
  expect_error(
    .url_join_impl(x, y, by = "URL", type = "inner",
                   suffix = c(".x", NA_character_)),
    class = "rurl_url_join_suffix_error"
  )
})

test_that("semi and anti emit x columns only and never suffix", {
  for (t in c("semi", "anti")) {
    res <- .url_join_impl(jx(), jy(), by = "URL", type = t)
    expect_named(res, c("URL", "vx"))
  }
})

# --- key visibility (KJ-O7 / P3.2 D-G) ---------------------------------------

test_that("the comparison key is hidden by default", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "left")
  expect_false(any(vapply(res, inherits, logical(1), "rurl_url_key")))
})

test_that("key_name exposes the CLASSED key, never a URL-looking string", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "left", key_name = "k")
  expect_s3_class(res$k, "rurl_url_key")
  expect_identical(attr(res$k, "standard"), "whatwg")
  # Matched rows share one key; a non-keyable row is NA with a typed reason.
  expect_identical(unclass(res$k)[[1]], unclass(res$k)[[4]])
  expect_true(is.na(unclass(res$k)[[6]]))
  expect_identical(attr(res$k, "keyability")[[6]], "invalid-parse")
  # Never renderable as a URL.
  expect_false(any(grepl("^[a-z]+://", format(res$k))))
  expect_false(any(grepl("example", format(res$k), fixed = TRUE)))
})

test_that("an unmatched y row still reports its own key (coalesced)", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "full", key_name = "k")
  last <- nrow(res)
  expect_true(is.na(res$URL.x[[last]]))
  expect_false(is.na(unclass(res$k)[[last]]))
  expect_identical(attr(res$k, "keyability")[[last]], "ok")
})

test_that("a colliding key_name is an early error, never a silent rename", {
  expect_error(
    .url_join_impl(jx(), jy(), by = "URL", type = "left", key_name = "vx"),
    class = "rurl_url_join_key_name_error"
  )
  expect_error(
    .url_join_impl(jx(), jy(), by = "URL", type = "left", key_name = "URL.y"),
    class = "rurl_url_join_key_name_error"
  )
})

test_that("key_name must be NULL or one non-NA non-empty string", {
  x <- jx()
  y <- jy()
  expect_error(.url_join_impl(x, y, by = "URL", type = "left", key_name = ""),
               class = "rurl_url_join_key_name_error")
  expect_error(
    .url_join_impl(x, y, by = "URL", type = "left",
                   key_name = c("a", "b")),
    class = "rurl_url_join_key_name_error"
  )
  expect_error(
    .url_join_impl(x, y, by = "URL", type = "left", key_name = NA_character_),
    class = "rurl_url_join_key_name_error"
  )
})

# --- the six join matrix (:138-145, P3.2 D-C/D-D) ---------------------------

test_that("inner join keeps matching pairs in x order, y order within x", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "inner")
  expect_identical(res$vx, c(1L, 1L, 3L, 3L))
  expect_identical(res$vy, c("m", "o", "m", "o"))
})

test_that("left join keeps every x row and expands matches", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "left")
  expect_identical(res$vx, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$vy, c("m", "o", NA, "m", "o", NA))
  # A non-keyable x row is retained, unmatched -- never silently dropped.
  expect_identical(res$URL.x[[6]], "not a url")
})

test_that("right join is the y-primary mirror of left (P3.2 D-C)", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "right")
  # Every y row in y order; x matches in x order within each y row.
  expect_identical(res$vy, c("m", "m", "n", "o", "o"))
  expect_identical(res$vx, c(1L, 3L, NA, 1L, 3L))
  expect_true(is.na(res$URL.x[[3]]))
})

test_that("right join is the exact reflection of left with sides swapped", {
  # The two directional joins are ONE symmetry, so swapping sides and columns
  # must reproduce the same pairing set.
  l <- .url_join_impl(jx(), jy(), by = "URL", type = "left")
  r <- .url_join_impl(jy(), jx(), by = "URL", type = "right")
  pair <- function(a, b) sort(paste(a, b, sep = "|"))
  expect_identical(
    pair(l$vx[!is.na(l$vy)], l$vy[!is.na(l$vy)]),
    pair(r$vx[!is.na(r$vy)], r$vy[!is.na(r$vy)])
  )
})

test_that("full join is the left result then unmatched y rows in y order", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "full")
  expect_identical(res$vx, c(1L, 1L, 2L, 3L, 3L, 4L, NA))
  expect_identical(res$vy, c("m", "o", NA, "m", "o", NA, "n"))
})

test_that("semi join emits each matching x row once, in x order", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "semi")
  expect_identical(res$vx, c(1L, 3L))
})

test_that("anti join keeps unmatched x once, INCLUDING non-keyable (D-D)", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "anti")
  expect_identical(res$vx, c(2L, 4L))
  # A non-keyable row has by construction no eligible match, which is the
  # anti-join predicate itself -- dropping it would conflate "not keyable" with
  # "matched and therefore excluded".
  expect_true("not a url" %in% res$URL)
})

test_that("non-keyable y rows never match on any join type", {
  y <- data.frame(URL = c("not a url", NA_character_, ""), vy = 1:3,
                  stringsAsFactors = FALSE)
  inner <- .url_join_impl(jx(), y, by = "URL", type = "inner")
  expect_identical(nrow(inner), 0L)
  expect_identical(nrow(.url_join_impl(jx(), y, by = "URL", type = "semi")), 0L)
  expect_identical(nrow(.url_join_impl(jx(), y, by = "URL", type = "anti")), 4L)
})

# --- identity, not presentation (P3.1 D-A.3) --------------------------------

test_that("matching is by identity key, not by cleaned display string", {
  # Host case and an explicit default port are identity-equal; a differing query
  # and a differing path are not.
  x <- data.frame(URL = c("https://EXAMPLE.com:443/p", "http://h.com/a?b=1",
                          "http://h.com/a"),
                  vx = 1:3, stringsAsFactors = FALSE)
  y <- data.frame(URL = c("https://example.com/p", "http://h.com/a?b=2"),
                  vy = c("hit", "miss"), stringsAsFactors = FALSE)
  res <- .url_join_impl(x, y, by = "URL", type = "left")
  expect_identical(res$vy, c("hit", NA, NA))
})

test_that("fragment and userinfo are invisible to matching (Q5)", {
  x <- data.frame(URL = c("http://h.com/p#frag", "http://u:pw@h.com/p"),
                  vx = 1:2, stringsAsFactors = FALSE)
  y <- data.frame(URL = "http://h.com/p", vy = "hit", stringsAsFactors = FALSE)
  res <- .url_join_impl(x, y, by = "URL", type = "left")
  expect_identical(res$vy, c("hit", "hit"))
})

test_that("the family has no presentation dials to forward", {
  # Non-interference is STRUCTURAL here: unlike canonical_join()'s unrestricted
  # `...`, the engine has no `...` and no cleaning or display formal, so a
  # presentation dial cannot reach equality even by accident.
  fml <- names(formals(.url_join_impl))
  expect_false("..." %in% fml)

  # `engine` is the ONE name shared with the legacy-dial list, and it is there
  # for a different reason: it is the `pslr` annotation-layer dependency
  # (RURL-owrdsivt), not a display transform. It is admitted because
  # `.url_key_compute_vec()` accepts it, and it is safe because it cannot move
  # key bytes -- asserted below rather than assumed.
  expect_identical(
    intersect(fml, .CJ_LEGACY_PRESENTATION_DIALS),
    "engine"
  )
})

test_that("engine cannot change key bytes, so identity is engine-independent", {
  # The key frames no PSL-derived component -- domain/TLD decomposition is
  # excluded from identity by the "host editing" row -- so swapping the PSL
  # engine must leave every framed byte untouched.
  #
  # Scope of this assertion: it compares the default engine against an
  # explicitly constructed snapshot of the same list. It shows the plumbing
  # carries `engine` without disturbing the framing; it does NOT exercise two
  # DIFFERENT suffix lists, so it is not evidence about a divergent list.
  skip_if_not_installed("pslr")
  eng <- pslr::psl_engine()
  u <- c("https://Example.com/p", "http://sub.example.co.uk/x",
         "http://example.invalidtld/", "not a url", "mailto:a@b.com")
  pol <- .url_key_policy_spec()
  expect_identical(
    as.character(.url_key_compute_vec(u, pol, engine = NULL)),
    as.character(.url_key_compute_vec(u, pol, engine = eng))
  )

  # And it reaches the join without changing which rows match.
  x <- data.frame(URL = u, vx = seq_along(u), stringsAsFactors = FALSE)
  expect_identical(
    .url_join_impl(x, jy(), by = "URL", type = "left", engine = NULL),
    .url_join_impl(x, jy(), by = "URL", type = "left", engine = eng)
  )
})

test_that("`policy` must be the classed symmetric policy object", {
  expect_error(
    .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                   policy = list(standard = "whatwg")),
    class = "rurl_url_join_policy_error"
  )
})

test_that("the policy travels into the exposed key, so results are versioned", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "inner", key_name = "k")
  expect_identical(attr(res$k, "key_version"), .URL_KEY_POLICY_VERSION)
  expect_identical(attr(res$k, "schema_version"), .URL_KEY_SCHEMA_VERSION)
})

# --- multiple (P3.1 D-D) -----------------------------------------------------

test_that("multiple defaults to all and expands duplicates losslessly", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "inner")
  expect_identical(nrow(res), 4L)
  expect_identical(.URL_JOIN_MULTIPLE[[1]], "all")
})

test_that("multiple = first/last narrow to a defined stable order", {
  first <- .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                          multiple = "first")
  last <- .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                         multiple = "last")
  expect_identical(first$vy, c("m", "m"))
  expect_identical(last$vy, c("o", "o"))
})

test_that("multiple narrowing does not reuse the legacy collision dial", {
  expect_false("collision" %in% names(formals(.url_join_impl)))
  expect_true("multiple" %in% names(formals(.url_join_impl)))
})

test_that("a narrowed y row surfaces as unmatched in a full join", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "full",
                        multiple = "first")
  # y3 ("o") was narrowed out of the match set, so the full join reports it as
  # an unmatched y row rather than losing it.
  expect_true("o" %in% res$vy[is.na(res$vx)])
})

# --- relationship (P3.1 D-D) -------------------------------------------------

test_that("relationship defaults to none and does not constrain", {
  expect_identical(.URL_JOIN_RELATIONSHIPS[[1]], "none")
  expect_silent(.url_join_impl(jx(), jy(), by = "URL", type = "inner"))
})

test_that("one-to-one rejects duplicate keys on either side", {
  expect_error(
    .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                   relationship = "one-to-one"),
    class = "rurl_url_join_relationship_error"
  )
})

test_that("one-to-many constrains x, many-to-one constrains y", {
  # x has the duplicate key twice, y has it twice as well: both are violated.
  expect_error(
    .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                   relationship = "one-to-many"),
    class = "rurl_url_join_relationship_error"
  )
  expect_error(
    .url_join_impl(jx(), jy(), by = "URL", type = "inner",
                   relationship = "many-to-one"),
    class = "rurl_url_join_relationship_error"
  )

  # A unique-x frame satisfies one-to-many but a duplicate y still violates
  # many-to-one, which is what makes the two directions distinct.
  x1 <- data.frame(URL = "https://example.com/p", vx = 1L,
                   stringsAsFactors = FALSE)
  expect_silent(.url_join_impl(x1, jy(), by = "URL", type = "inner",
                               relationship = "one-to-many"))
  expect_error(
    .url_join_impl(x1, jy(), by = "URL", type = "inner",
                   relationship = "many-to-one"),
    class = "rurl_url_join_relationship_error"
  )
})

test_that("many-to-many allows expansion when declared explicitly", {
  expect_silent(.url_join_impl(jx(), jy(), by = "URL", type = "inner",
                               relationship = "many-to-many"))
})

test_that("relationship is checked only on keys present on BOTH sides", {
  # A duplicate key that can never match must not fail the check: the contract
  # constrains eligible MATCHING keys, not every duplicate in the input.
  x <- data.frame(URL = c("http://dup.com/", "http://dup.com/",
                          "https://example.com/p"),
                  vx = 1:3, stringsAsFactors = FALSE)
  y <- data.frame(URL = "https://example.com/p", vy = "m",
                  stringsAsFactors = FALSE)
  expect_silent(.url_join_impl(x, y, by = "URL", type = "inner",
                               relationship = "one-to-one"))
})

test_that("the relationship error reports counts and leaks no credentials", {
  cred <- "http://u:secretpw@h.com/p"
  x <- data.frame(URL = c(cred, cred), vx = 1:2, stringsAsFactors = FALSE)
  y <- data.frame(URL = "http://h.com/p", vy = "m", stringsAsFactors = FALSE)
  err <- tryCatch(
    .url_join_impl(x, y, by = "URL", type = "inner",
                   relationship = "one-to-one"),
    rurl_url_join_relationship_error = function(e) conditionMessage(e)
  )
  expect_match(err, "Projected result rows")
  expect_false(grepl("secretpw", err, fixed = TRUE))
  expect_false(grepl("://", err, fixed = TRUE))
})

test_that("the relationship preflight runs before materialization", {
  # A relationship violation must error even when the projected product is huge:
  # if the check ran after expansion this would materialize 250000 rows first.
  n <- 500L
  x <- data.frame(URL = rep("https://example.com/p", n), vx = seq_len(n),
                  stringsAsFactors = FALSE)
  y <- data.frame(URL = rep("https://example.com/p", n), vy = seq_len(n),
                  stringsAsFactors = FALSE)
  err <- tryCatch(
    .url_join_impl(x, y, by = "URL", type = "inner",
                   relationship = "one-to-one"),
    rurl_url_join_relationship_error = function(e) conditionMessage(e)
  )
  expect_match(err, "Projected result rows: 250000")
})

# --- invalid and warnings, the two split axes (P3.1 D-D) --------------------

test_that("invalid = keep retains non-keyable rows per the join's own rule", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "left")
  expect_identical(nrow(res), 6L)
  expect_true("not a url" %in% res$URL.x)
})

test_that("invalid = drop removes non-keyable rows before matching", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "left",
                        invalid = "drop")
  expect_false("not a url" %in% res$URL.x)
  expect_identical(nrow(res), 5L)

  # Dropping on the y side removes nothing from x.
  y <- rbind(jy(), data.frame(URL = "not a url", vy = "z",
                              stringsAsFactors = FALSE))
  res2 <- .url_join_impl(jx(), y, by = "URL", type = "left", invalid = "drop")
  expect_false("z" %in% res2$vy)
})

test_that("invalid = error stops with a typed condition naming the reasons", {
  err <- tryCatch(
    .url_join_impl(jx(), jy(), by = "URL", type = "left", invalid = "error"),
    rurl_url_join_invalid_error = function(e) conditionMessage(e)
  )
  expect_match(err, "non-keyable")
  expect_match(err, "invalid-parse")
  # Positions, never content.
  expect_false(grepl("not a url", err, fixed = TRUE))
})

test_that("warnings = allow lets warning rows match (the default)", {
  expect_identical(.URL_JOIN_WARNINGS[[1]], "allow")
  x <- data.frame(URL = "http://example.invalidtld/", vx = 1L,
                  stringsAsFactors = FALSE)
  y <- data.frame(URL = "http://example.invalidtld/", vy = "m",
                  stringsAsFactors = FALSE)
  res <- .url_join_impl(x, y, by = "URL", type = "inner")
  expect_identical(res$vy, "m")
})

test_that("warnings = reject makes a warning row ineligible but keeps it", {
  x <- data.frame(URL = c("http://example.invalidtld/", "http://a.com/1"),
                  vx = 1:2, stringsAsFactors = FALSE)
  y <- data.frame(URL = c("http://example.invalidtld/", "http://a.com/1"),
                  vy = c("m", "n"), stringsAsFactors = FALSE)

  inner <- .url_join_impl(x, y, by = "URL", type = "inner",
                          warnings = "reject")
  expect_identical(inner$vy, "n")

  # "rejected from the match set" is NOT "dropped from the result": the left
  # join still retains the row, unmatched.
  left <- .url_join_impl(x, y, by = "URL", type = "left", warnings = "reject")
  expect_identical(nrow(left), 2L)
  expect_identical(left$vy, c(NA, "n"))
})

test_that("warnings = error stops with its own typed condition", {
  x <- data.frame(URL = "http://example.invalidtld/", vx = 1L,
                  stringsAsFactors = FALSE)
  expect_error(
    .url_join_impl(x, jy(), by = "URL", type = "inner", warnings = "error"),
    class = "rurl_url_join_warning_error"
  )
})

test_that("the invalid and warnings axes are independent, not on_parse_error", {
  fml <- names(formals(.url_join_impl))
  expect_true(all(c("invalid", "warnings") %in% fml))
  expect_false("on_parse_error" %in% fml)
  expect_false("join_parse_status" %in% fml)

  # A frame with one non-keyable and one warning row exercises both axes at
  # once: dropping the invalid row must not drop the warning row.
  x <- data.frame(URL = c("not a url", "http://example.invalidtld/"),
                  vx = 1:2, stringsAsFactors = FALSE)
  res <- .url_join_impl(x, jy(), by = "URL", type = "left", invalid = "drop")
  expect_identical(res$vx, 2L)
})

# --- type restoration and zero-row prototypes (KJ-O8 / P3.2 D-H) ------------

test_that("a zero-row result carries the complete typed would-be schema", {
  y <- data.frame(URL = "http://nowhere.example/", vy = 1.5,
                  stringsAsFactors = FALSE)
  res <- .url_join_impl(jx(), y, by = "URL", type = "inner", key_name = "k")

  expect_identical(nrow(res), 0L)
  expect_named(res, c("URL.x", "vx", "URL.y", "vy", "k"))
  expect_type(res$URL.x, "character")
  expect_type(res$vx, "integer")
  expect_type(res$vy, "double")
  expect_s3_class(res$k, "rurl_url_key")
})

test_that("an empty input side yields the typed zero-row schema", {
  res <- .url_join_impl(jx()[0, ], jy(), by = "URL", type = "inner")
  expect_identical(nrow(res), 0L)
  expect_named(res, c("URL.x", "vx", "URL.y", "vy"))
  expect_type(res$vx, "integer")

  res2 <- .url_join_impl(jx(), jy()[0, ], by = "URL", type = "left")
  expect_identical(nrow(res2), 4L)
  expect_true(all(is.na(res2$vy)))
  expect_type(res2$vy, "character")
})

test_that("the missing payload uses x's own column types, not logical NA", {
  x <- data.frame(URL = "http://only-x.example/", n = 2.5, i = 7L,
                  f = factor("a"), stringsAsFactors = FALSE)
  y <- data.frame(URL = "http://only-y.example/", vy = "m",
                  stringsAsFactors = FALSE)
  res <- .url_join_impl(x, y, by = "URL", type = "full")

  expect_identical(nrow(res), 2L)
  expect_type(res$n, "double")
  expect_type(res$i, "integer")
  expect_s3_class(res$f, "factor")
  expect_true(is.na(res$n[[2]]))
})

test_that("the result is built by row-slicing x's prototype (D-H.1)", {
  # x's own subsetting contract governs: a data-frame subclass survives because
  # the result IS a row-slice of x, not a fresh data.frame().
  x <- jx()
  class(x) <- c("my_df", "data.frame")
  res <- .url_join_impl(x, jy(), by = "URL", type = "left")
  expect_s3_class(res, "my_df")

  # And the zero-row path takes the same route, so it cannot drift.
  res0 <- .url_join_impl(x, jy()[0, ], by = "URL", type = "inner")
  expect_s3_class(res0, "my_df")
  expect_identical(nrow(res0), 0L)
})

test_that("row names are reset rather than carrying NA slice artifacts", {
  res <- .url_join_impl(jx(), jy(), by = "URL", type = "full")
  expect_identical(rownames(res), as.character(seq_len(nrow(res))))
})

# --- vectorization and degenerate shapes ------------------------------------

test_that("both sides empty yields a typed zero-row result on every type", {
  x <- jx()[0, ]
  y <- jy()[0, ]
  for (t in .URL_JOIN_TYPES) {
    res <- .url_join_impl(x, y, by = "URL", type = t)
    expect_identical(nrow(res), 0L)
  }
})

test_that("a single-column frame joins without dropping to a vector", {
  x <- data.frame(URL = "https://example.com/p", stringsAsFactors = FALSE)
  y <- data.frame(URL = "https://example.com/p", stringsAsFactors = FALSE)
  res <- .url_join_impl(x, y, by = "URL", type = "inner")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("URL.x", "URL.y"))
  expect_identical(nrow(res), 1L)
})

test_that("non-data-frame inputs are a typed input error", {
  expect_error(.url_join_impl(list(URL = "http://a.com/"), jy(), by = "URL",
                              type = "inner"),
               class = "rurl_url_join_input_error")
  expect_error(.url_join_impl(jx(), "nope", by = "URL", type = "inner"),
               class = "rurl_url_join_input_error")
})

test_that("every join type is reachable and enumerated", {
  expect_identical(
    .URL_JOIN_TYPES,
    c("inner", "left", "right", "full", "semi", "anti")
  )
  for (t in .URL_JOIN_TYPES) {
    res <- .url_join_impl(jx(), jy(), by = "URL", type = t)
    expect_s3_class(res, "data.frame")
  }
})

test_that("all typed conditions subclass one family class", {
  err <- tryCatch(
    .url_join_impl(jx(), jy(), by = "nope", type = "inner"),
    rurl_url_join_error = function(e) class(e)
  )
  expect_true("rurl_url_join_input_error" %in% err)
  expect_true("rurl_url_join_error" %in% err)
})

# --- the engine stays unexported (VD-001 / deferral-gate D3) ----------------

test_that("no join-family export has shipped yet", {
  # VD-001's surface_probe names all eight; D3 fires on ANY of them appearing in
  # NAMESPACE, at which point all 51 cells must be verified in the same change.
  # This test is the local tripwire for that sequencing constraint.
  # The source tree keeps NAMESPACE two levels above tests/testthat, but under
  # `R CMD check` the tests run beside an INSTALLED copy, where it ships at the
  # package root instead. Resolve both rather than assume the source layout --
  # assuming it made this tripwire an error on every checked build.
  ns_path <- testthat::test_path("..", "..", "NAMESPACE")
  if (!file.exists(ns_path)) {
    ns_path <- system.file("NAMESPACE", package = "rurl")
  }
  expect_true(nzchar(ns_path) && file.exists(ns_path))
  ns <- readLines(ns_path, warn = FALSE)
  deferred <- c("get_url_key", "url_key_policy", "url_inner_join",
                "url_left_join", "url_right_join", "url_full_join",
                "url_semi_join", "url_anti_join")
  for (nm in deferred) {
    expect_false(any(grepl(sprintf("^export\\(%s\\)$", nm), ns)))
  }
})
