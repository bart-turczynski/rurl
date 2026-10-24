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
# Spellings for kf() entries.
web_pos <- hi_positions("web")
mailto_pos <- hi_positions("mailto")
named <- c("whatwg", "rfc3986")
joins <- c("url_inner_join", "url_left_join", "url_right_join",
           "url_full_join", "url_semi_join", "url_anti_join")

# Empty: every matrix case keeps its contract. The last entry, an rfc3986
# invalid-UTF-8 query that stopped safe_parse_urls() and get_clean_url()
# (RURL-kzbqvxpk), went when the query split moved to bytes. A new entry is a
# row of
#   kf(export, standard, class, position, issue)
known_failures <- kf(character(0), character(0), character(0), character(0),
                     character(0))

# Cases outside the matrix: inputs too costly to run for every export. The
# probe test runs every case of every probe -- each standard at each position
# -- alone. `class` is the input class the probe cuts in.
probes <- list(
  long_6e5 = list(class = "long", long_n = 600000L, template = "web",
                  export = "safe_parse_urls",
                  standards = c("NULL", "whatwg", "rfc3986"),
                  positions = c("host", "path")),
  file_long_6e5 = list(class = "long", long_n = 600000L, template = "file",
                       export = "safe_parse_urls",
                       standards = c("NULL", "whatwg", "rfc3986"),
                       positions = "path")
)
# The probe cases that fail today: (export, standard, probe, position), the
# owning issue, and `detail`, a regular expression the failure's detail must
# match, so a listed case that starts failing some other way is not taken for
# the failure its issue tracks. A probe case not listed must pass.
#
# Empty: every probe case passes. The 6e5-octet cases overflowed ICU's
# backtrack stack (U_REGEX_STACK_OVERFLOW) under rfc3986 and on the NULL
# `file:` route until RURL-vewhvxnb; this empty table is its regression
# guard. A new entry is a row of
#   kpf(export, standard, probe, position, issue, detail)
kpf <- function(export, standard, class, position, issue, detail) {
  out <- kf(export, standard, class, position, issue)
  out$detail <- rep_len(detail, nrow(out))
  out
}
known_probe_failures <- kpf(character(0), character(0), character(0),
                            character(0), character(0), character(0))

key_cols <- c("export", "standard", "class", "position")
case_key <- function(d) do.call(paste, c(d[key_cols], sep = " | "))
probe_keys <- unlist(lapply(names(probes), function(pn) {
  p <- probes[[pn]]
  case_key(kf(p$export, p$standards, pn, p$positions, ""))
}))

test_that("the known-failure tables are well formed", {
  all_known <- rbind(known_failures,
                     known_probe_failures[names(known_failures)])
  expect_false(anyDuplicated(case_key(all_known)) > 0L)
  expect_true(all(all_known$export %in% names(capability_manifest)))
  expect_true(all(grepl("^(RURL-[a-z]{8}|UNFILED-[0-9]+)$", all_known$issue)))
  for (i in seq_len(NROW(known_failures))) {
    e <- capability_manifest[[known_failures$export[i]]]
    expect_true(known_failures$standard[i] %in% e$standards,
                label = case_key(known_failures[i, ]))
    expect_true(known_failures$class[i] %in% hi_classes,
                label = case_key(known_failures[i, ]))
  }
  # A listed probe case the probe test does not run would never be checked.
  expect_identical(setdiff(case_key(known_probe_failures), probe_keys),
                   character(0), label = "listed probe cases no probe runs")
  expect_true(all(nzchar(known_probe_failures$detail)))
  for (p in probes) {
    e <- capability_manifest[[p$export]]
    expect_true(all(p$standards %in% e$standards), label = p$export)
    expect_true(all(p$positions %in% hi_positions(p$template)),
                label = p$export)
  }
})

utf8 <- hi_utf8_locale()

for (fn in names(capability_manifest)) {
  test_that(paste0(fn, "(): hostile rows keep their contracts"), {
    # The table is strict both ways, and some of its entries (the locale
    # contract under LC_CTYPE C above all) are what this platform's C library
    # and ICU build do: hold it where the suite is run on purpose, not on
    # every CRAN flavor.
    skip_on_cran()
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

test_that("a probe case passes unless listed, and a listed one still fails", {
  # The overflow point is ICU's backtrack stack limit, which a platform or a
  # stringi build can move: hold it where the suite is run on purpose.
  skip_on_cran()
  skip_if(is.na(utf8), "no UTF-8 LC_CTYPE is available")
  for (pn in names(probes)) {
    p <- probes[[pn]]
    e <- capability_manifest[[p$export]]
    e$template <- p$template
    for (std in p$standards) {
      for (pos in p$positions) {
        key <- case_key(kf(p$export, std, pn, pos, ""))
        v <- hi_check_export(p$export, e, std, list(), e$operands[1L], utf8,
                             c_loc = NA_character_, classes = p$class,
                             positions = pos, long_n = p$long_n)
        listed <- known_probe_failures[case_key(known_probe_failures) == key, ]
        if (NROW(listed) == 0L) {
          expect_identical(sprintf("[%s] %s", v$check, v$detail), character(0),
                           label = paste(key, "violations no entry lists"))
        } else {
          expect_true(any(grepl(listed$detail, v$detail)),
                      label = paste0(key, " fails with /", listed$detail,
                                     "/ (remove it if fixed)"))
        }
      }
    }
  }
})

# The error each rejected selector value raises. For the "policy" exports
# (get_url_key() and the url_*_join() family) the rejection is raised inside
# url_key_policy(), which hi_selector_args() calls to build the `policy`
# argument: the export itself never sees the bad value.
rejection_pattern <- function(e, std) {
  if (std == "bogus") {
    return(switch(e$selector,
      url_standard = ,
      dots = "url_standard must be NULL, \"rfc3986\", or \"whatwg\"",
      standard = ,
      policy = "should be one of"
    ))
  }
  switch(e$selector,
    # get_host_type(), get_url_diagnostics(), get_scheme_class() (ADR 0015);
    # check_schemes() and get_mailto_recipients() (scheme_acceptance =
    # "general" under NULL).
    url_standard = paste0("`url_standard` is required",
                          "|requires an explicit url_standard"),
    standard = ,
    policy = "must be named; NULL is not accepted"
  )
}

test_that("unsupported selectors and malformed arguments keep their errors", {
  for (fn in names(capability_manifest)) {
    e <- capability_manifest[[fn]]
    plain <- hi_plain(e$template)
    rejected <- c(e$unsupported, if (e$selector != "none") "bogus")
    for (std in rejected) {
      expect_error(hi_runner(fn, e, std, list(), e$operands[1L])$run(plain),
                   rejection_pattern(e, std), label = paste(fn, std))
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
