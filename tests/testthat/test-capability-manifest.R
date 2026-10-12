# RURL-jmrpffgf: every export is in the capability manifest or exempt.
#
# helper-capability-manifest.R names, per export that reads URL strings, what
# it takes and what it returns; test-hostile-input-contracts.R holds each
# entry to its hostile-input contracts. An export missing from both the
# manifest and the exempt list would be an entry point no contract covers, so
# adding one without an entry fails here.

# The `export()` names NAMESPACE declares. Read from the file, not from
# getNamespaceExports(): under devtools::load_all() the namespace exports
# every object. system.file() finds the source NAMESPACE under load_all() and
# the installed one under R CMD check.
namespace_exports <- function() {
  path <- system.file("NAMESPACE", package = "rurl")
  if (!nzchar(path)) {
    testthat::skip("NAMESPACE file not found")
  }
  lines <- trimws(readLines(path, warn = FALSE))
  if (any(startsWith(lines, "exportPattern("))) {
    stop("NAMESPACE uses exportPattern(); list the exports explicitly",
         call. = FALSE)
  }
  ex <- lines[startsWith(lines, "export(")]
  ex <- sub("^export\\((.*)\\)$", "\\1", ex)
  ex <- unlist(strsplit(ex, ",", fixed = TRUE), use.names = FALSE)
  sort(gsub("^[\"`]|[\"`]$", "", trimws(ex)))
}

test_that("every NAMESPACE export is in the manifest or on the exempt list", {
  exports <- namespace_exports()
  expect_gt(length(exports), 0L)
  covered <- c(names(capability_manifest), names(capability_exempt))
  expect_identical(setdiff(exports, covered), character(0),
                   label = "exports with no manifest entry and no exemption")
  expect_identical(setdiff(covered, exports), character(0),
                   label = "manifest or exempt names NAMESPACE does not export")
  expect_identical(intersect(names(capability_manifest),
                             names(capability_exempt)),
                   character(0))
  expect_false(anyDuplicated(names(capability_manifest)) > 0L)
  expect_false(anyDuplicated(names(capability_exempt)) > 0L)
})

test_that("every exemption carries a reason", {
  expect_true(all(nzchar(trimws(capability_exempt))))
})

test_that("every manifest entry is well formed against the live formals", {
  shapes <- c("vector", "key", "list", "frame", "expanded", "aggregate",
              "scalar", "join")
  selectors <- c("url_standard", "standard", "policy", "dots", "none")
  ns <- asNamespace("rurl")
  for (fn in names(capability_manifest)) {
    e <- capability_manifest[[fn]]
    fm <- names(formals(get(fn, envir = ns)))
    expect_true(e$shape %in% shapes, label = fn)
    expect_true(e$selector %in% selectors, label = fn)
    expect_true(e$template %in% c("web", "mailto"), label = fn)
    expect_gt(length(e$standards), 0L, label = fn)
    if (e$selector == "none") {
      expect_identical(e$standards, "none", label = fn)
    } else {
      expect_true(all(e$standards %in% c("NULL", "whatwg", "rfc3986")),
                  label = fn)
    }
    expect_length(intersect(e$standards, e$unsupported), 0L)
    if (e$shape == "join") {
      # A join reads the URL column of each of its two table operands.
      expect_length(e$operands, 2L)
    }
    # Every URL operand and every crossed option is a live formal (or reaches
    # the export through its `...`).
    takes_dots <- "..." %in% fm
    opt_names <- as.character(unlist(lapply(e$options, names)))
    for (arg in c(e$operands, opt_names[!startsWith(opt_names, ".")])) {
      expect_true(arg %in% fm || takes_dots,
                  label = paste(fn, "formal", arg))
    }
    for (o in e$options) {
      expect_true(all(o$.standards %in% e$standards), label = fn)
    }
    expect_true(all(e$positions %in% hi_positions(e$template)), label = fn)
    expect_true(all(e$echoes %in% hi_positions(e$template)), label = fn)
    sel_formal <- switch(e$selector,
                         url_standard = "url_standard",
                         standard = "standard",
                         policy = "policy",
                         NULL)
    if (!is.null(sel_formal)) {
      expect_true(sel_formal %in% fm, label = paste(fn, "selector formal"))
    }
  }
})
