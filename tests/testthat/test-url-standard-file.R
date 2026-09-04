# Tests for file:// support (RURL-rutsdflg, epic RURL-apxhgjhf). WHATWG treats
# file as a special scheme; RFC 3986 admits it as an ordinary registered
# hierarchical scheme. rurl supports the plain hostless forms `file:///...` and
# `file://localhost/...`. The localhost authority DECOMPOSES differently per
# selector -- collapsed to an empty host under `whatwg` and the byte-frozen NULL
# baseline, reported as an ordinary `reg-name` under `rfc3986` (RURL-zyytztdd,
# and see `.rfc_file_localhost_policy()`).

test_that("file URLs parse under both standard profiles", {
  urls <- c(
    "file://localhost/path/to/file.txt",
    "file:///path/to/file.txt"
  )

  # Everything EXCEPT the localhost host decomposition is selector-invariant.
  for (std in c("whatwg", "rfc3986")) {
    parsed <- safe_parse_urls(urls, url_standard = std)

    expect_identical(parsed$scheme, c("file", "file"), info = std)
    expect_identical(
      parsed$path, c("/path/to/file.txt", "/path/to/file.txt"), info = std
    )
    expect_identical(parsed$parse_status, c("ok", "ok"), info = std)
    expect_identical(parsed$port, c(NA_integer_, NA_integer_), info = std)
  }

  # This test previously asserted an empty host and a hostless `clean_url` for
  # BOTH selectors, i.e. it pinned RURL-zyytztdd's defect on the `rfc3986` half.
  # These are the presentation host and surface (c), not the identity record --
  # characterization of the accessor surface, not the conformance claim, which
  # the `.fsss_record_vec()` test below owns.
  w <- safe_parse_urls(urls, url_standard = "whatwg")
  expect_true(all(is.na(w$host)))
  expect_identical(
    w$clean_url, c("file:///path/to/file.txt", "file:///path/to/file.txt")
  )

  r <- safe_parse_urls(urls, url_standard = "rfc3986")
  expect_identical(r$host, c("localhost", NA_character_))
  expect_identical(
    r$clean_url,
    c("file://localhost/path/to/file.txt", "file:///path/to/file.txt")
  )
})

# RURL-zyytztdd. `localhost` is an ordinary `reg-name` under RFC 3986 S3.2.2, so
# the grammar-fidelity selector must REPORT it. Emptying it is WHATWG's
# file-host rule ("if host is localhost, set host to the empty string"), which
# ADR 0012 D5 scopes to WHATWG by explicit contrast with its `rfc-syntax`
# clause.
# RFC 8089 S2 lists the normative authority as empty, `localhost`, or `host` --
# three legal FORMS, none of them rewritten into another. Its App. B equivalence
# of `file://localhost/p` with `file:///p` is an equivalence, not an identical
# decomposition, and ADR 0007 puts `file` expansion out of the selector's scope.
test_that("rfc3986 reports localhost as an ordinary reg-name host", {
  u <- c("file://localhost/path/to/file.txt", "file:///path/to/file.txt")
  r <- rurl:::.fsss_record_vec(u, "rfc3986", NULL)

  expect_identical(r$ok, c(TRUE, TRUE))
  expect_identical(r$host, c("localhost", ""))
  expect_identical(r$host_kind, c("present", "empty"))
  expect_identical(r$authority_delimiter_present, c(TRUE, TRUE))
  expect_identical(r$path, c("/path/to/file.txt", "/path/to/file.txt"))

  # The host is now on the same seam as any other reg-name, so it preserves
  # source case exactly as `foo://Example.COM/p` does under this selector
  # (S6.2.2.1 lowercasing is a NORMALIZATION, not a parse result).
  expect_identical(
    rurl:::.fsss_record_vec("file://LocalHost/p", "rfc3986", NULL)$host,
    "LocalHost"
  )

  # Surface (b): the serializer round-trips the authority it was given.
  expect_identical(
    serialize_url(u, standard = "rfc3986", form = "source"),
    c("file://localhost/path/to/file.txt", "file:///path/to/file.txt")
  )
})

test_that("whatwg and NULL keep the localhost empty-host mapping", {
  u <- c("file://localhost/path/to/file.txt", "file:///path/to/file.txt")

  w <- rurl:::.fsss_record_vec(u, "whatwg", NULL)
  expect_identical(w$host, c("", ""))
  expect_identical(w$host_kind, c("empty", "empty"))
  expect_identical(
    serialize_url(u, standard = "whatwg", form = "source"),
    c("file:///path/to/file.txt", "file:///path/to/file.txt")
  )

  # The no-selector baseline is byte-frozen (ADR 0012 D4), so it keeps the
  # collapse even though `rfc3986` no longer applies it.
  expect_identical(
    serialize_url(u, form = "source"),
    c("file:///path/to/file.txt", "file:///path/to/file.txt")
  )
  expect_true(is.na(get_host("file://localhost/p")))
})

test_that("file URLs compose with accessors and scheme classification", {
  u <- "file://localhost/path/to/file.txt"

  expect_identical(get_clean_url(u, url_standard = "whatwg"),
    "file:///path/to/file.txt")
  expect_identical(get_path(u, url_standard = "whatwg"), "/path/to/file.txt")
  expect_true(is.na(get_host(u, url_standard = "whatwg")))
  expect_identical(get_scheme_class(u, url_standard = "whatwg"), "special")
})

test_that("legacy and RFC file forms are parsed by the in-tree overlay", {
  # These two inputs are the epic's known-divergent pair (RURL-gxqdmpcp): they
  # returned "error" on Linux/macOS and "ok" on Windows from the SAME external
  # call, because that engine's file: handling was a BUILD property. They are
  # now
  # decided by the in-tree RFC 8089 overlay, so the answer is the same on every
  # platform. This test previously asserted the macOS half of that divergence.
  urls <- c("file://example.com/path", "file:///c:/Windows/System32")

  expect_identical(get_parse_status(urls), c("ok", "ok"))
  expect_identical(
    get_parse_status(urls, url_standard = "rfc3986"), c("ok", "ok")
  )
  # The NULL selector and "rfc3986" agree by construction: both route file: to
  # the same overlay (RURL-obsweger decision (a)).
  expect_identical(
    get_clean_url(urls), get_clean_url(urls, url_standard = "rfc3986")
  )
  expect_identical(
    get_clean_url(urls),
    c("file://example.com/path", "file:///c:/Windows/System32")
  )
  # The drive-letter path keeps its leading slash. The Windows engine returned
  # path="c:/Windows/System32", which re-parses with host=c: -- one reason not
  # to adopt the Windows answer wholesale.
  expect_identical(get_path(urls)[2L], "/c:/Windows/System32")
})

test_that("file: Gate 1 rejects forms that are not valid RFC 3986", {
  # RFC 8089 App. F gives ABNF to nonstandard forms that ESCAPE RFC 3986:
  # `drive-letter = ALPHA ":" / ALPHA "|"`, but "|" is absent from `pchar`.
  # Gate 1 rejects those; the whatwg profile repairs them instead.
  bs <- paste0("file:///path", rawToChar(as.raw(92L)), "to")
  for (std in list(NULL, "rfc3986")) {
    status <- function(x) {
      if (is.null(std)) {
        get_parse_status(x)
      } else {
        get_parse_status(x, url_standard = std)
      }
    }
    expect_identical(status("file://C|/x"), "error")
    expect_identical(status(bs), "error")
    expect_identical(status("file://[example]/"), "error")
  }
  # Percent-encoding is the LEGAL way to carry "|" in a reg-name, so the
  # encoded form is accepted where the bare form is not. Confirmed against
  # Ruby's URI::RFC3986_Parser, which draws the same line.
  expect_identical(get_parse_status("file://C%7C"), "ok")
  # WHATWG repairs rather than rejects (matches adaR / Node `new URL()`).
  expect_identical(
    get_path("file://C|/x", url_standard = "whatwg"), "/C:/x"
  )
})

test_that("file: Gate 2 applies RFC 8089 S2's narrowing of the authority", {
  # No port: S2's `file-auth = "localhost" / host` has none, and no appendix
  # supplies a production for one -> parse failure. This is the NULL default,
  # byte-frozen by ADR 0012 D4. The `rfc3986` selector no longer agrees --
  # see the test below (RURL-uhkofhjf).
  expect_identical(get_parse_status("file://example.com:80/path"), "error")
  # userinfo IS admitted, by App. E.1/F's production, and is surfaced as a
  # fact rather than silently discarded.
  u <- "file://user@example.com/path"
  expect_identical(get_parse_status(u), "ok")
  expect_identical(get_user(u), "user")
  expect_identical(get_host(u), "example.com")
  # query/fragment survive: RFC 8089 never mentions either, and RFC 3986 S3.5
  # forbids scheme specs from restricting the fragment at all. RFC 8118 S3
  # depends on this working (application/pdf `#page=`).
  expect_identical(get_parse_status("file:///doc.pdf#page=2"), "ok")
  expect_identical(get_fragment("file:///doc.pdf#page=2"), "page=2")
  expect_identical(get_query("file:///data.csv?v=2"), "v=2")
})

test_that("under rfc3986 a file: port is a FACT, not a Gate 2 failure", {
  # RURL-uhkofhjf. ADR 0012 D5 lists four "scheme-specific facts (parseable !=
  # valid-for-the-scheme)" items for `file` under rfc-syntax: non-absolute path,
  # userinfo, port, query/fragment. Three were already facts; port alone gated
  # the parse. `rfc3986` is the scheme-AGNOSTIC generic syntax, and ADR 0012
  # rules that "scheme-specific restrictions are overlays, not generic parse
  # gates" -- so an RFC 8089 narrowing may not reject here.
  args <- list(url_standard = "rfc3986")
  u <- c("file://example:1/", "file://localhost:8098/path/to/file.txt",
         "file://example.com:80/path")
  expect_identical(
    do.call(get_parse_status, c(list(u), args)), rep("ok", 3L)
  )
  expect_identical(do.call(get_host, c(list(u), args)),
                   c("example", "localhost", "example.com"))
  expect_identical(do.call(get_port, c(list(u), args)), c(1L, 8098L, 80L))

  # The FACT is surfaced, grouped with query/fragment: RFC 8089 mentions a port
  # exactly as often as it mentions a query, which is never.
  d <- get_url_diagnostics(u, url_standard = "rfc3986",
                           scheme_acceptance = "general")
  for (i in seq_along(u)) {
    expect_true("file-component-outside-rfc8089" %in% d[[i]], info = u[[i]])
  }

  # Only the SCHEME-specific narrowing is lifted. RFC 3986 sec 3.2.3 is
  # `port = *DIGIT`, so a non-digit port is still a generic grammar failure,
  # and an EMPTY port is still no port rather than an error.
  expect_identical(
    do.call(get_parse_status,
            c(list(c("file://example:abc/", "file://example:1x/")), args)),
    c("error", "error")
  )
  expect_identical(
    do.call(get_parse_status, c(list("file://example:/"), args)), "ok"
  )
  expect_identical(do.call(get_port, c(list("file://example:/"), args)),
                   NA_integer_)

  # The other two profiles do not move. The NULL default is byte-frozen
  # (ADR 0012 D4) and WHATWG runs its own `file:` state machine.
  expect_identical(
    suppressWarnings(get_parse_status(u)), rep("error", 3L)
  )
  expect_identical(
    get_parse_status(u, url_standard = "whatwg"), rep("error", 3L)
  )
})

test_that("WHATWG file parser accepts drive-letter and bare path forms", {
  cases <- data.frame(
    input = c(
      "file:C|/m/",
      "file:C||/m/",
      "file:/C|/",
      "file://C|/",
      "file:///Y:",
      "file:///Y:/",
      "file:",
      "file:.",
      "file:/example.com/",
      "file:.//p",
      "file:/.//p",
      # WHATWG "file host state" (RURL-ufsltsit): a drive letter in the
      # authority position -- `C:` as much as `C|` -- is not a host, and a
      # slash-less one is the whole path (no `/` appended).
      "file://C:/",
      "file://C:",
      "file://d|",
      "file://d:\\x"
    ),
    path = c(
      "/C:/m/",
      "/C||/m/",
      "/C:/",
      "/C:/",
      "/Y:",
      "/Y:/",
      "/",
      "/",
      "/example.com/",
      "//p",
      "//p",
      "/C:/",
      "/C:",
      "/d:",
      "/d:/x"
    ),
    stringsAsFactors = FALSE
  )

  parsed <- safe_parse_urls(cases$input, url_standard = "whatwg")

  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_identical(parsed$scheme, rep("file", nrow(cases)))
  expect_true(all(is.na(parsed$host)))
  expect_identical(parsed$path, cases$path)
})

# RURL-msefniuz. WHATWG "path state" resolves `..` through "shorten a URL's
# path", which returns without removing anything when "url's scheme is 'file',
# path's size is 1, and path[0] is a normalized Windows drive letter". The
# absolute parser handed the path to the RFC 3986 section 5.2.4 remover, which
# knows no drive letter and shortened `file:///C:/../` to `file:///`.
test_that("WHATWG file parser never shortens past a lone drive letter", {
  cases <- data.frame(
    input = c(
      "file:///C:/../",
      "file:///C:/a/../..",
      "file:///C:/..",
      "file://host/C:/..",
      # `C|` is normalized to `C:` in the first segment before shortening.
      "file:///C|/..",
      # WHATWG dot segments are atoms: `%2e` counts, case-insensitively.
      "file:///C:/%2e%2e/",
      "file:///C:/a/%2E%2E/%2e%2e/",
      "file:///C:/.%2E/x/%2e",
      # An ordinary `..` below the drive letter still shortens.
      "file:///C:/a/../b",
      # Not a drive letter: two characters, not one, before the colon.
      "file:///Cx/../"
    ),
    serialized = c(
      "file:///C:/",
      "file:///C:/",
      "file:///C:/",
      "file://host/C:/",
      "file:///C:/",
      "file:///C:/",
      "file:///C:/",
      "file:///C:/x/",
      "file:///C:/b",
      "file:///"
    ),
    stringsAsFactors = FALSE
  )

  expect_identical(
    serialize_url(cases$input, standard = "whatwg"), cases$serialized
  )
  parsed <- safe_parse_urls(cases$input, profile = "whatwg")
  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_identical(
    parsed$path, sub("^file://[^/]*", "", cases$serialized, perl = TRUE)
  )

  # The clause is keyed on the scheme being `file`: a non-file path with the
  # same shape shortens to `/` exactly as before.
  expect_identical(
    serialize_url("http://h/C:/..", standard = "whatwg"), "http://h/"
  )
  expect_identical(
    safe_parse_urls("http://h/C:/..", profile = "whatwg")$path, "/"
  )

  # Negative controls: the rule is reached only from the WHATWG `file:` state
  # machine. `rfc3986` keeps section 5.2.4's shortening, and the frozen NULL
  # selector keeps the byte-frozen no-normalization record (ADR 0007). Both
  # values equal what `main` produced before this fix.
  ctrl <- c(
    "file:///C:/../", "file:///C:/a/../..", "file:///C:/..",
    "file://host/C:/..", "http://h/C:/.."
  )
  expect_identical(
    serialize_url(ctrl, standard = "rfc3986", form = "normalized"),
    c("file:///", "file:///", "file:///", "file://host/", "http://h/")
  )
  expect_identical(
    serialize_url(ctrl, standard = "rfc3986", form = "source"), ctrl
  )
  expect_identical(
    safe_parse_urls(
      ctrl, url_standard = "rfc3986", scheme_policy = "require",
      scheme_acceptance = "general"
    )$path,
    c("/", "/", "/", "/", "/")
  )
  frozen <- safe_parse_urls(ctrl)
  expect_identical(
    frozen$path, c("/C:/../", "/C:/a/../..", "/C:/..", "/C:/..", "/C:/..")
  )
  expect_identical(frozen$clean_url, ctrl)
})

test_that("WHATWG file parser preserves query and fragment on empty paths", {
  parsed <- safe_parse_urls(
    c("file:?q=v", "file:#frag"),
    url_standard = "whatwg",
    query_handling = "keep"
  )

  expect_identical(parsed$parse_status, c("ok", "ok"))
  expect_identical(parsed$path, c("/", "/"))
  expect_identical(parsed$query, c("q=v", NA_character_))
  expect_identical(parsed$fragment, c(NA_character_, "frag"))
  expect_identical(parsed$clean_url[1L], "file:///?q=v")
})

test_that("WHATWG file parser handles reverse solidus file states", {
  cases <- data.frame(
    input = c(
      r"(file:\\//)",
      r"(file:\\\\)",
      r"(file:\\\\?fox)",
      r"(file:\\\\#guppy)",
      r"(file:\\localhost//)",
      r"(file://\/localhost//cat)"
    ),
    path = c("//", "//", "//", "//", "//", "//localhost//cat"),
    query = c(NA_character_, NA_character_, "fox", NA_character_, NA_character_,
              NA_character_),
    fragment = c(NA_character_, NA_character_, NA_character_, "guppy",
                 NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  parsed <- safe_parse_urls(
    cases$input, url_standard = "whatwg", query_handling = "keep"
  )

  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_true(all(is.na(parsed$host)))
  expect_identical(parsed$path, cases$path)
  expect_identical(parsed$query, cases$query)
  expect_identical(parsed$fragment, cases$fragment)
})

test_that("WHATWG file parser accepts non-local and UTS-mapped hosts", {
  cases <- data.frame(
    input = c(
      "file://spider///",
      "file://example.net/C:/",
      "file://1.2.3.4/C:/",
      "file://[1::8]/C:/",
      "file://a\u00adb/p",
      "file://a%C2%ADb/p",
      paste0(
        "file://loC",
        "\U0001D400\U0001D40B\U0001D407\U0001D428\U0001D42C\U0001D42D",
        "/usr/bin"
      ),
      "file://xn--/p"
    ),
    host = c(
      "spider",
      "example.net",
      "1.2.3.4",
      "[1::8]",
      "ab",
      "ab",
      NA_character_,
      "xn--"
    ),
    path = c("///", "/C:/", "/C:/", "/C:/", "/p", "/p", "/usr/bin", "/p"),
    stringsAsFactors = FALSE
  )

  parsed <- safe_parse_urls(cases$input, url_standard = "whatwg")

  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_identical(parsed$host, cases$host)
  expect_identical(parsed$path, cases$path)
})

test_that("WHATWG file parser rejects forbidden decoded file hosts", {
  expect_identical(
    get_parse_status("file://%43%3A", url_standard = "whatwg"),
    "error"
  )
  expect_true(is.na(get_clean_url("file://%43%3A", url_standard = "whatwg")))
})

test_that("hostless invalid file: rows never emit a clean URL", {
  # RURL-hnddjptl. `clean_url` renders a hostless row as `scheme://` + path,
  # so a path not beginning with "/" lands in the authority position:
  # "file:C:/W" emitted "file://C:/W" (authority "C:" -- an SMB fetch on
  # Windows), "file:etc/passwd" -> "file://etc/passwd", "file:." -> "file://.".
  # All three are parse errors, so there is no canonical spelling to emit.
  offenders <- c("file:C:/W", "file:etc/passwd", "file:.")
  for (std in list(NULL, "rfc3986")) {
    parsed <- safe_parse_urls(offenders, url_standard = std)
    expect_identical(parsed$parse_status, rep("error", length(offenders)))
    # An error row has no canonical spelling to emit. This also covers the
    # RFC-normalized empty-path edge: "file:." must not become "file://".
    expect_true(all(is.na(parsed$clean_url)))
  }
  # The path column still reports what was read: the guard suppresses only the
  # reassembled key, it does not discard the parse facts.
  expect_identical(
    safe_parse_urls(offenders)$path, c("C:/W", "etc/passwd", ".")
  )
})

test_that("the authority guard leaves legitimate hostless file: rows intact", {
  # Companion to RURL-hnddjptl: an absolute path is safe to concatenate, so the
  # guard must not fire on it. Pins the shapes `is_file` exists for.
  expect_identical(
    get_clean_url("file:///etc/passwd", url_standard = "rfc3986"),
    "file:///etc/passwd"
  )
  expect_identical(
    get_clean_url("file:////server/share", url_standard = "rfc3986"),
    "file:////server/share"
  )
  expect_identical(
    get_clean_url("file://server/share/x", url_standard = "rfc3986"),
    "file://server/share/x"
  )
  expect_identical(
    get_clean_url("file:///", url_standard = "rfc3986"), "file:///"
  )
  # WHATWG makes these paths absolute during parsing, so they stay buildable.
  expect_identical(
    get_clean_url("file:C|/W", url_standard = "whatwg"), "file:///C:/W"
  )
  expect_identical(
    get_clean_url("file:etc/passwd", url_standard = "whatwg"),
    "file:///etc/passwd"
  )
})
