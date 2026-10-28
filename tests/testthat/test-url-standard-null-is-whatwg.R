# ADR 0020 D3/D4 (RURL-mbhuhwzk): the default `url_standard` is "whatwg", and
# `NULL` stays accepted and means "whatwg" wherever `url_standard` is a
# parameter. The frozen pre-selector profile is retired, so for every exported
# function that takes `url_standard`,
#
#   f(x) == f(x, url_standard = NULL) == f(x, url_standard = "whatwg")
#
# over the fixture corpora. The functions are enumerated from the namespace, so
# a new export with a `url_standard` formal is covered without editing this
# file. A function whose formal default already names another standard keeps
# it (ADR 0020 D4: `get_mailto_recipients()` defaults to "rfc3986"); there the
# default call is compared with that standard, and `NULL` still reads as
# "whatwg".

# Formal defaults that name a standard other than "whatwg" (ADR 0020 D4).
null_is_whatwg_named_defaults <- c(get_mailto_recipients = "rfc3986")

# The corpus: every input of the characterization corpus and every runnable
# input of the external vectors, de-duplicated. NA and "" stay in.
null_is_whatwg_corpus <- function() {
  ext_path <- testthat::test_path("fixtures", "external-url-vectors.csv")
  raw <- readBin(ext_path, "raw", file.size(ext_path))
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  fx <- utils::read.csv(
    text = txt, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = "NA", encoding = "UTF-8"
  )
  ext <- character(0)
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    runnable <- fx$runnable == "yes"
    ext <- vapply(
      fx$input_json[runnable], jsonlite::fromJSON, character(1),
      USE.NAMES = FALSE
    )
  }
  unique(c(parse_corpus_urls(), ext))
}

# Every exported function with a `url_standard` formal.
null_is_whatwg_functions <- function() {
  ns <- asNamespace("rurl")
  ex <- sort(getNamespaceExports("rurl"))
  keep <- vapply(ex, function(nm) {
    fn <- get(nm, envir = ns)
    is.function(fn) && "url_standard" %in% names(formals(fn))
  }, logical(1))
  ex[keep]
}

# The leading arguments each function is called with. `resolve_url()` takes
# the corpus as references against one fixed base; everything else takes the
# corpus as its first argument.
null_is_whatwg_args <- function(fn_name, x) {
  if (identical(fn_name, "resolve_url")) {
    return(list(relative_or_absolute = x, base_url = "http://example.com/a/b"))
  }
  stats::setNames(list(x), names(formals(get(fn_name, asNamespace("rurl"))))[1])
}

# Call `fn_name` and return its value, or the error message (so an error on
# one arm and a value on another, or two different errors, compare unequal).
null_is_whatwg_call <- function(fn_name, args) {
  tryCatch(
    do.call(get(fn_name, asNamespace("rurl")), args),
    error = function(e) structure(conditionMessage(e), class = "call_error")
  )
}

test_that("the url_standard enumeration finds the parse surface", {
  fns <- null_is_whatwg_functions()
  # A floor, not a list: these must be among them.
  expect_true(all(c(
    "safe_parse_url", "safe_parse_urls", "get_clean_url", "get_host",
    "get_parse_verdicts", "resolve_url", "get_host_type",
    "get_url_diagnostics", "get_scheme_class", "get_mailto_recipients"
  ) %in% fns))
})

test_that("url_standard defaults to \"whatwg\" unless it names another", {
  ns <- asNamespace("rurl")
  for (fn_name in null_is_whatwg_functions()) {
    formal <- formals(get(fn_name, envir = ns))[["url_standard"]]
    # A formal with no default (the empty symbol) reads as "<none>".
    default <- if (identical(formal, quote(expr = ))) "<none>" else eval(formal)
    expected <- if (fn_name %in% names(null_is_whatwg_named_defaults)) {
      null_is_whatwg_named_defaults[[fn_name]]
    } else {
      "whatwg"
    }
    expect_identical(default, expected, info = fn_name)
  }
})

test_that("f(x), f(x, NULL) and f(x, \"whatwg\") are identical", {
  x <- null_is_whatwg_corpus()
  # safe_parse_url() is scalar: run it over a sample of the corpus.
  x_scalar <- x[seq(1L, length(x), by = 7L)]
  failing <- character(0)
  for (fn_name in null_is_whatwg_functions()) {
    scalar <- identical(fn_name, "safe_parse_url")
    inputs <- if (scalar) as.list(x_scalar) else list(x)
    for (xi in inputs) {
      args <- null_is_whatwg_args(fn_name, xi)
      as_null <- null_is_whatwg_call(
        fn_name, c(args, list(url_standard = NULL))
      )
      as_whatwg <- null_is_whatwg_call(
        fn_name, c(args, list(url_standard = "whatwg"))
      )
      named <- null_is_whatwg_named_defaults[fn_name]
      as_default <- null_is_whatwg_call(fn_name, args)
      default_ref <- if (is.na(named)) {
        as_whatwg
      } else {
        null_is_whatwg_call(fn_name, c(args, list(url_standard = named[[1]])))
      }
      ok <- identical(as_null, as_whatwg) && identical(as_default, default_ref)
      if (!ok) {
        failing <- c(failing, fn_name)
      }
    }
  }
  failing <- unique(failing)
  expect_identical(failing, character(0))
})

test_that("resolve_url(output = \"serialized\") reads NULL as whatwg", {
  x <- null_is_whatwg_corpus()
  base <- "http://example.com/a/b"
  serial <- function(...) {
    null_is_whatwg_call("resolve_url", list(
      relative_or_absolute = x, base_url = base, output = "serialized", ...
    ))
  }
  as_whatwg <- serial(url_standard = "whatwg")
  expect_false(inherits(as_whatwg, "call_error"))
  expect_identical(serial(), as_whatwg)
  expect_identical(serial(url_standard = NULL), as_whatwg)
})
