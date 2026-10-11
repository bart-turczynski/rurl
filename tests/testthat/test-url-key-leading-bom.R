# RURL-dquaxcaz: the URL key keeps a U+FEFF that starts a key field.
#
# Some stringi functions skip a U+FEFF (the byte-order mark) at the start of
# their input (ADR 0005, "a leading U+FEFF is a third equivalence gap"). A
# U+FEFF is ordinary data in a URL: RFC 3986 has no rule for it, and the WHATWG
# URL Standard strips only C0 controls and spaces. So a key field that starts
# with one must frame the mark, or the key collides with its unmarked twin.
# Each U+FEFF row has a U+200B twin: a zero-width code point stringi does not
# skip, so a twin that already gives the right answer shows the defect is the
# mark's alone.
#
# No `url_standard = NULL` output can move here: `url_key_policy()` rejects
# `standard = NULL` (pinned below), so the key and the joins built on it have
# only the `whatwg` and `rfc3986` arms. `.url_key_scheme_presence()` takes a
# `url_standard`, so its guard is also asserted with `NULL` passed directly.

bom <- "\ufeff"
zwsp <- "\u200b"

key <- function(u, std) {
  as.character(get_url_key(u, url_key_policy(std)))
}

key_fields <- function(u, std) {
  .url_key_state_vec(u, url_key_policy(std))$fields
}

# How a present field is framed: its UTF-8 byte count, `:`, its bytes.
framed <- function(x) {
  paste0(nchar(x, type = "bytes"), ":", x)
}

has_frame <- function(k, x) {
  grepl(framed(x), k, fixed = TRUE, useBytes = TRUE)
}

# Rows whose named key field starts with the mark under the named policy. Each
# entry: the standard, the field, the marked spelling with `%s` for the mark,
# and the field value the record holds for it.
marked_rows <- list(
  list("rfc3986", "host", "%sh.example.com/p", "%sh.example.com"),
  list("rfc3986", "host", "http://%sh.com/", "%sh.com"),
  list("rfc3986", "path", "foo:%sa", "%sa"),
  list("rfc3986", "query", "http://h/p?%sq", "%sq")
)

fill <- function(template, mark) {
  sprintf(template, mark)
}

arm_label <- function(std) {
  if (is.null(std)) "NULL" else std
}

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the record holds the mark in every field the fix reaches", {
  # The defect is in the framing, not the record: the record already holds
  # the mark, as it holds the U+200B twin's code point.
  for (row in marked_rows) {
    for (mark in c(bom, zwsp)) {
      f <- key_fields(fill(row[[3L]], mark), row[[1L]])[[row[[2L]]]]
      expect_identical(f, fill(row[[4L]], mark),
                       label = paste(row[[1L]], row[[3L]], utf8ToInt(mark)))
    }
  }
})

test_that("the U+200B twins already frame the code point", {
  for (row in marked_rows) {
    std <- row[[1L]]
    twin <- fill(row[[3L]], zwsp)
    plain <- fill(row[[3L]], "")
    k <- key(c(twin, plain), std)
    expect_true(has_frame(k[[1L]], fill(row[[4L]], zwsp)), label = twin)
    expect_false(k[[1L]] == k[[2L]], label = twin)
  }
  expect_identical(.url_key_frame(list(paste0(zwsp, "ab"))),
                   paste0("5:", zwsp, "ab"))
})

test_that("negative controls: no mark, a mark not first, NA and empty", {
  expect_identical(.url_key_frame(list(c("ab", NA, ""))),
                   c("2:ab", "-:", "0:"))
  expect_identical(.url_key_frame(list(paste0("a", bom, "b"))),
                   paste0("5:a", bom, "b"))
  expect_identical(
    .url_key_frame(list(c("x", "y"), c(paste0(bom, "z"), NA)))[[2L]], "1:y-:"
  )
  for (std in c("whatwg", "rfc3986")) {
    k <- key(c(paste0("foo://h/", bom), "foo://h/"), std)
    expect_true(has_frame(k[[1L]], paste0("/", bom)), label = std)
    expect_false(k[[1L]] == k[[2L]], label = std)
  }
  k <- key(c(paste0("http://h", bom, ".com/"), "http://h.com/"), "rfc3986")
  expect_true(has_frame(k[[1L]], paste0("h", bom, ".com")))
  expect_false(k[[1L]] == k[[2L]])
})

test_that("whatwg: a special host maps the mark to nothing, so twins agree", {
  # UTS #46 maps U+FEFF and U+200B to nothing, and the WHATWG host parser
  # runs domain to ASCII, so `\ufeffh.example.com/p` IS `h.example.com/p`
  # under this policy. That collision is the standard's, not the framing's.
  k <- key(c(paste0(bom, "h.example.com/p"), paste0(zwsp, "h.example.com/p"),
             "h.example.com/p"), "whatwg")
  expect_identical(k[[1L]], k[[3L]])
  expect_identical(k[[2L]], k[[3L]])
  # A special query percent-encodes the mark, and a non-special host or
  # opaque path does too, so no field starts with it.
  k <- key(c(paste0("http://h/p?", bom, "q"), paste0("foo://", bom, "h/p"),
             paste0("foo:", bom, "a")), "whatwg")
  expect_true(has_frame(k[[1L]], "%EF%BB%BFq"))
  expect_true(has_frame(k[[2L]], "%EF%BB%BFh"))
  expect_true(has_frame(k[[3L]], "%EF%BB%BFa"))
  # A non-special query is keyed as the standard serializes it, with the
  # query percent-encode set, so it frames the escape too (RURL-pjrqnqvb).
  # It still keys apart from its unmarked twin.
  for (tmpl in c("mailto:a@b?%sto=x", "foo:/p?%sq")) {
    k <- key(c(fill(tmpl, bom), fill(tmpl, "")), "whatwg")
    expect_true(has_frame(k[[1L]], fill(sub("^.*[?]", "", tmpl), "%EF%BB%BF")),
                label = tmpl)
    expect_false(k[[1L]] == k[[2L]], label = tmpl)
  }
})

test_that("the fragment and userinfo stay invisible with a mark in them", {
  # Ratification Q5: neither is web-resource identity, so a mark there must
  # not reach the key under either policy.
  for (std in c("whatwg", "rfc3986")) {
    k <- key(c(paste0("http://u:", bom, "pw@h/p#", bom, "f"), "http://h/p"),
             std)
    expect_identical(k[[1L]], k[[2L]], label = std)
  }
})

test_that("scheme presence: the U+200B twin and the unmarked rows", {
  # Passed every url_standard directly; the key itself reaches the helper
  # only under `whatwg` and `rfc3986`.
  u <- c("//h/p", paste0(zwsp, "//h/p"), "http://h/", "h.com/", "/p")
  for (std in list(NULL, "rfc3986", "whatwg")) {
    expect_identical(
      .url_key_scheme_presence(u, std, rep(FALSE, length(u))),
      c("scheme-relative", "inferred", "explicit", "inferred", "inferred"),
      label = arm_label(std)
    )
  }
})

test_that("a mark-led scheme-relative row is not keyable under either policy", {
  # The key is NA, typed as an invalid parse, before and after the guard, so
  # the presence it reads cannot reach a key byte.
  u <- c(paste0(bom, "//h/p"), paste0(zwsp, "//h/p"))
  for (std in c("whatwg", "rfc3986")) {
    k <- get_url_key(u, url_key_policy(std))
    expect_identical(as.character(k), c(NA_character_, NA_character_),
                     label = std)
    expect_identical(attr(k, "keyability"), c("invalid-parse", "invalid-parse"),
                     label = std)
  }
})

test_that("the key has no NULL arm, and the version is not bumped again", {
  expect_error(url_key_policy(standard = NULL), "NULL is not accepted")
  # Key version 2 is this unreleased cycle's bump (NEWS); it already
  # separates every key minted now from every key rurl 3.1.0 minted.
  expect_identical(url_key_policy()$key_version, 2L)
  expect_identical(url_key_policy("rfc3986")$key_version, 2L)
  expect_identical(url_key_policy()$schema_version, 1L)
})

# --- Defect: the framing drops a mark that starts a field ---------------------

test_that("the frame keeps a mark that starts the field", {
  # stri_enc_toutf8() read the mark as a byte-order mark and dropped it, so
  # `\ufeffab` framed as `2:ab`, the frame of `ab`. It is three more bytes.
  expect_identical(.url_key_frame(list(paste0(bom, "ab"))),
                   paste0("5:", bom, "ab"))
  expect_identical(.url_key_frame(list(bom)), paste0("3:", bom))
  withr::with_locale(c(LC_CTYPE = "C"), {
    expect_identical(.url_key_frame(list(paste0(bom, "ab"))),
                     paste0("5:", bom, "ab"))
  })
})

test_that("the frame reads a latin1 field by its mark, in any locale", {
  # The splice is only for mark-led rows: a paste0() splice on every row would
  # translate a latin1 field by the locale (`caf<e9>` under C).
  latin1_cafe <- "caf\xe9"
  Encoding(latin1_cafe) <- "latin1"
  for (ctype in c("C", "en_US.UTF-8")) {
    withr::with_locale(c(LC_CTYPE = ctype), {
      expect_identical(
        .url_key_frame(list(c(latin1_cafe, paste0(bom, "ab")))),
        c("5:caf\u00e9", paste0("5:", bom, "ab")),
        label = ctype
      )
    })
  }
})

test_that("a mark-led field no longer collides with its unmarked twin", {
  # Signature, per policy: `rfc3986` moves a host, path or query that starts
  # with the mark, since its record keeps each as written (RFC 3986 has no
  # rule for U+FEFF). Under `whatwg` the mark is percent-encoded in every key
  # field it can start, including a non-special query (RURL-pjrqnqvb), so no
  # `whatwg` row is left here (pinned above), and the fragment and userinfo
  # stay out of the key. Each marked row keyed
  # like its unmarked twin; it now frames the mark, as its U+200B twin does.
  for (row in marked_rows) {
    std <- row[[1L]]
    marked <- fill(row[[3L]], bom)
    k <- key(c(marked, fill(row[[3L]], "")), std)
    expect_false(k[[1L]] == k[[2L]], label = marked)
    expect_true(has_frame(k[[1L]], fill(row[[4L]], bom)), label = marked)
  }
  # The ticket's row, as a dedup would see it.
  u <- c(paste0(bom, "h.example.com/p"), "h.example.com/p")
  expect_identical(duplicated(get_url_key(u, url_key_policy("rfc3986"))),
                   c(FALSE, FALSE))
})

test_that("scheme presence reads a mark-led `//` as written", {
  # Latent: stri_startswith_fixed() read past the mark, so `\ufeff//h/p` read
  # as scheme-relative where its twin reads inferred. The row fails the parse
  # (pinned above), so no key moves.
  for (std in list(NULL, "rfc3986", "whatwg")) {
    expect_identical(
      .url_key_scheme_presence(paste0(bom, "//h/p"), std, FALSE),
      "inferred", label = arm_label(std)
    )
  }
})
