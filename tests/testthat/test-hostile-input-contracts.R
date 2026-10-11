# RURL-jmrpffgf: hostile-input contracts, per export.
#
# Edge-case tests were written per defect, so each hostile input class -- a
# leading U+FEFF, invalid UTF-8, a latin1 or bytes mark, a very long
# component -- was rediscovered one entry point at a time. This file states,
# for every export in the capability manifest (helper-capability-manifest.R),
# what it promises for a hostile row, and holds every export to it at once.
#
# Each hostile row (helper-hostile-inputs.R lists the classes) is cut into a
# URL template at one component position and sits between two good neighbors.
# For every supported standard and crossed option set:
#
#   error      the call does not throw;
#   isolation  each row's value equals its solo-call value, through a
#              projection that fits the output: one element, one data frame
#              row, the segment of an expanded output, the rows of a join that
#              carry the row's id; an aggregate's counts are the sums of its
#              rows' counts;
#   locale     LC_CTYPE C gives what a UTF-8 LC_CTYPE gives (first option set
#              only);
#   twin       a latin1 row gives what its UTF-8 twin gives, and a U+FEFF row
#              gives what its U+200B twin gives once each U+FEFF spelling in
#              the output is rewritten;
#   sentinel   a long component keeps its tail where the output echoes it.
#
# Every case that breaks a contract today is in `known_failures`, keyed by
# (export, standard, class, position) and owned by an open issue. The table
# is strict both ways: a broken contract that is not listed fails, and so
# does a listed case that no longer breaks, so a fix forces its entry out.
#
# The test never fixes a failure; the owning issue does.

kf <- function(export, standard, class, position, issue) {
  out <- expand.grid(export = export, standard = standard, class = class,
                     position = position, stringsAsFactors = FALSE)
  out$issue <- issue
  out
}
web_pos <- hi_positions("web")
mailto_pos <- hi_positions("mailto")
named <- c("whatwg", "rfc3986")
joins <- c("url_inner_join", "url_left_join", "url_right_join",
           "url_full_join", "url_semi_join", "url_anti_join")

known_failures <- rbind(
  # A bytes-marked element throws "bytes encoding is not supported by this
  # function" out of the whole call at every entry point that reads the
  # string with stringi before it reaches the parse (safe_parse_urls() and
  # its accessors fail the row instead). The general per-entry-point input
  # boundary.
  kf(c("get_host_type", "get_url_diagnostics", "check_hosts", "is_valid_host",
       "check_schemes", "serialize_url", "serialise_url", "get_url_key"),
     named, "bytes", web_pos, "RURL-cwzickqy"),
  kf("format_url", "none", "bytes", web_pos, "RURL-cwzickqy"),
  kf("get_mailto_recipients", named, "bytes", mailto_pos, "RURL-cwzickqy"),
  kf(joins, named, "bytes", c("x:host", "y:host"), "RURL-cwzickqy"),
  kf("resolve_url", named, "bytes",
     c(paste0("relative_or_absolute:", web_pos), "base_url:userinfo",
       "base_url:host"),
     "RURL-cwzickqy"),
  # resolve_url(): invalid UTF-8 in the reference throws "invalid multibyte
  # string"; in the base it resolves differently under LC_CTYPE C.
  kf("resolve_url", c("NULL", "rfc3986"), "invalid_utf8",
     c(paste0("relative_or_absolute:", web_pos),
       paste0("base_url:", c("path", "query", "fragment"))),
     "RURL-cwzickqy"),
  kf("resolve_url", "whatwg", "invalid_utf8",
     paste0("base_url:", c("userinfo", "path", "query", "fragment")),
     "RURL-cwzickqy"),
  # rfc3986 with query_handling other than "drop": an invalid-UTF-8 octet in
  # the query throws "missing value where TRUE/FALSE needed" out of the
  # whole call (strsplit() on the query).
  kf(c("safe_parse_urls", "get_clean_url"), "rfc3986", "invalid_utf8",
     "query", "UNFILED-1")
)

# Cases outside the matrix: inputs too costly to run for every export, each
# run alone by the probe test below. `class` names a probe in `probes`.
probes <- list(
  long_6e5 = list(class = "long", long_n = 600000L, template = "web"),
  file_long_6e5 = list(class = "long", long_n = 600000L, template = "file")
)
known_probe_failures <- rbind(
  # U_REGEX_STACK_OVERFLOW out of the whole call.
  kf("safe_parse_urls", "rfc3986", "long_6e5", c("host", "path"),
     "RURL-vewhvxnb"),
  kf("safe_parse_urls", "NULL", "file_long_6e5", "path", "RURL-vewhvxnb")
)

key_cols <- c("export", "standard", "class", "position")
case_key <- function(d) do.call(paste, c(d[key_cols], sep = " | "))

test_that("the known-failure tables are well formed", {
  all_known <- rbind(known_failures, known_probe_failures)
  expect_false(anyDuplicated(case_key(all_known)) > 0L)
  expect_true(all(all_known$export %in% names(capability_manifest)))
  expect_true(all(grepl("^(RURL-[a-z]{8}|UNFILED-[0-9]+)$", all_known$issue)))
  for (i in seq_len(nrow(known_failures))) {
    e <- capability_manifest[[known_failures$export[i]]]
    expect_true(known_failures$standard[i] %in% e$standards,
                label = case_key(known_failures[i, ]))
    expect_true(known_failures$class[i] %in% hi_classes,
                label = case_key(known_failures[i, ]))
  }
  expect_true(all(known_probe_failures$class %in% names(probes)))
})

utf8 <- hi_utf8_locale()

for (fn in names(capability_manifest)) {
  test_that(paste0(fn, "(): hostile rows keep their contracts"), {
    skip_if(is.na(utf8), "no UTF-8 LC_CTYPE is available")
    v <- hi_check_entry(fn, utf8 = utf8)
    observed <- unique(v[key_cols])
    known <- known_failures[known_failures$export == fn, ]

    new <- !case_key(observed) %in% case_key(known)
    detail <- vapply(which(new), function(i) {
      hit <- case_key(v) == case_key(observed[i, ])
      paste0(case_key(observed[i, ]), " [", v$check[hit][1L], ", ",
             v$options[hit][1L], "] ", v$detail[hit][1L])
    }, "")
    expect_identical(detail, character(0),
                     label = "broken contracts no known failure lists")

    fixed <- case_key(known)[!case_key(known) %in% case_key(observed)]
    expect_identical(fixed, character(0),
                     label = "known failures that now pass (remove them)")
  })
}

test_that("each probe case still fails as its issue describes", {
  # The overflow point is ICU's backtrack stack limit, which a platform or a
  # stringi build can move: hold it where the suite is run on purpose.
  skip_on_cran()
  skip_if(is.na(utf8), "no UTF-8 LC_CTYPE is available")
  for (i in seq_len(nrow(known_probe_failures))) {
    k <- known_probe_failures[i, ]
    p <- probes[[k$class]]
    e <- capability_manifest[[k$export]]
    e$template <- p$template
    v <- hi_check_export(k$export, e, k$standard, list(), e$operands[1L],
                         utf8, c_loc = NA_character_, classes = p$class,
                         positions = k$position, long_n = p$long_n)
    expect_gt(nrow(v), 0L,
              label = paste(case_key(k), "violations (remove it if fixed)"))
  }
})

test_that("unsupported selectors and malformed arguments keep their errors", {
  for (fn in names(capability_manifest)) {
    e <- capability_manifest[[fn]]
    plain <- hi_plain(e$template)
    for (std in e$unsupported) {
      expect_error(hi_runner(fn, e, std, list(), e$operands[1L])$run(plain),
                   label = paste(fn, std))
    }
    if (e$selector != "none") {
      expect_error(hi_runner(fn, e, "bogus", list(), e$operands[1L])$run(plain),
                   label = paste(fn, "bogus"))
    }
  }
  # The URL operand must be URL strings where the export says so.
  expect_error(get_host(1L), "character vector")
  expect_error(get_url_diagnostics(1L, url_standard = "whatwg"),
               "character vector")
  expect_error(get_mailto_recipients(1L), "character vector")
  expect_error(query_param_summary(1L), "character vector")
  # safe_parse_url() is scalar; the vector form is safe_parse_urls().
  expect_error(safe_parse_url(c("http://a.example/", "http://b.example/")),
               "single URL")
  # A join's policy is a policy object and its `by` a column of both tables.
  x <- data.frame(url = "http://a.example/", stringsAsFactors = FALSE)
  expect_error(url_left_join(x, x, by = "url", policy = "whatwg"), "policy")
  expect_error(url_left_join(x, x, by = "nope"), "nope")
  # resolve_url(): serialized output names a standard and takes no dials.
  expect_error(resolve_url("g", "http://a.example/", output = "serialized"),
               "url_standard")
  expect_error(resolve_url("g", "http://a.example/", url_standard = "whatwg",
                           output = "serialized", port_handling = "keep"),
               "port_handling")
})
