# RURL-dquaxcaz: the URL key keeps a U+FEFF that starts a key field.
#
# Some stringi functions skip a U+FEFF (the byte-order mark) at the start of
# their input (ADR 0005, "a leading U+FEFF is a third equivalence gap"). A
# U+FEFF is ordinary data to the WHATWG URL Standard, which strips only C0
# controls and spaces (under `rfc3986` it is a parse error, RUL-030). So a
# key field that starts with one must frame the mark, or the key collides
# with its unmarked twin.
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

# RUL-030 (owner ruling 2026-10-12, RURL-pntbujzc): under `rfc3986` a raw
# non-ASCII code point, U+FEFF and U+200B included, is a parse error (RFC 3986
# sections 2, 2.5 and 3.2.2: a URI is US-ASCII, non-ASCII data is written as
# percent-encoded UTF-8, and `reg-name` admits no raw non-ASCII). Every
# `marked_rows` row, its U+200B twin and any other rfc3986 row holding the
# mark is therefore not keyable: NA, typed `invalid-parse`. The framing fix
# itself stays covered by the `.url_key_frame()` assertions below.
expect_unkeyable <- function(k, label) {
  expect_identical(as.character(k), NA_character_, label = label)
  expect_identical(attr(k, "keyability"), "invalid-parse", label = label)
}

arm_label <- function(std) {
  if (is.null(std)) "NULL" else std
}

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the record holds the mark in every field the fix reaches", {
  # The defect is in the framing, not the record: the web-route state already
  # holds the mark, as it holds the U+200B twin's code point. The general
  # route's `foo:` row is now rejected by the RFC 3986 gate (RUL-030), so its
  # path field is NA; no row here is keyable (pinned below).
  for (row in marked_rows) {
    for (mark in c(bom, zwsp)) {
      f <- key_fields(fill(row[[3L]], mark), row[[1L]])[[row[[2L]]]]
      want <- if (startsWith(row[[3L]], "foo:")) NA_character_ else
        fill(row[[4L]], mark)
      expect_identical(f, want,
                       label = paste(row[[1L]], row[[3L]], utf8ToInt(mark)))
    }
  }
})

test_that("the U+200B twins already frame the code point", {
  for (row in marked_rows) {
    std <- row[[1L]]
    twin <- fill(row[[3L]], zwsp)
    plain <- fill(row[[3L]], "")
    # RUL-030: the twin is a parse error under rfc3986, so it has no key;
    # the plain row keeps its own.
    k <- get_url_key(c(twin, plain), url_key_policy(std))
    expect_unkeyable(k[1L], label = twin)
    expect_false(is.na(as.character(k)[[2L]]), label = twin)
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
  # A mark that does not start the path field: `whatwg` frames the path as
  # its URL record holds it, with the path percent-encode set applied
  # (RURL-ommuyaqi). Under `rfc3986` the raw mark is a parse error (RUL-030).
  k <- key(c(paste0("foo://h/", bom), "foo://h/"), "whatwg")
  expect_true(has_frame(k[[1L]], "/%EF%BB%BF"))
  expect_false(k[[1L]] == k[[2L]])
  expect_unkeyable(get_url_key(paste0("foo://h/", bom),
                               url_key_policy("rfc3986")), "rfc3986 path")
  expect_unkeyable(get_url_key(paste0("http://h", bom, ".com/"),
                               url_key_policy("rfc3986")), "rfc3986 host")
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
  k <- key(c(paste0("http://u:", bom, "pw@h/p#", bom, "f"), "http://h/p"),
           "whatwg")
  expect_identical(k[[1L]], k[[2L]])
  # Under rfc3986 the raw mark is a parse error wherever it stands (RUL-030),
  # so the marked row has no key at all rather than its twin's.
  expect_unkeyable(
    get_url_key(paste0("http://u:", bom, "pw@h/p#", bom, "f"),
                url_key_policy("rfc3986")),
    "rfc3986"
  )
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
  # Signature, per policy: `rfc3986` once moved a host, path or query that
  # started with the mark; under RUL-030 such a row is a parse error (RFC 3986
  # sections 2, 2.5). Under `whatwg` the mark is percent-encoded in every key
  # field it can start, a list path (RURL-ommuyaqi) and a non-special query
  # (RURL-pjrqnqvb) included, so no
  # `whatwg` row is left here (pinned above), and the fragment and userinfo
  # stay out of the key. Each marked row keyed
  # like its unmarked twin; it now frames the mark, as its U+200B twin does.
  # Under RUL-030 every such rfc3986 row is a parse error, so it cannot
  # collide with its unmarked twin: it has no key.
  for (row in marked_rows) {
    std <- row[[1L]]
    marked <- fill(row[[3L]], bom)
    k <- get_url_key(c(marked, fill(row[[3L]], "")), url_key_policy(std))
    expect_unkeyable(k[1L], label = marked)
    expect_false(is.na(as.character(k)[[2L]]), label = marked)
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
