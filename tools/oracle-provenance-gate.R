#!/usr/bin/env Rscript

# Oracle-provenance gate (rurl 3.0 protocol hardening, G4).
#
# S8 finding 9 -- "Oracle integrity gate"
# (design/work/url-v3/evidence/S8-performance-curl-conformance-migration.md)
# -- requires every normative fixture to carry immutable provenance: upstream
# project/revision/path, retrieval date, license, raw-source hash, import and
# generation commands, transformed-fixture hash, standard/version/section,
# claim kind and applicability selector. P5.3 section 2.3
# (design/work/url-v3/decisions/P5.3-oracle-claim-policy.md) turns that into
# the field requirement, and tests/testthat/fixtures/oracle-provenance.json is
# the record that satisfies it.
#
# This gate makes that record EXECUTABLE. A provenance record nobody checks is
# prose: it rots the first time a fixture gains a row or a hash is regenerated,
# and it rots silently, which is the exact failure mode the G4 phase exists to
# prevent. Every quantity the record states about a tracked file is RECOMPUTED
# here from the file's bytes -- a recorded hash or tally is a claim, never
# evidence.
#
# This VERIFIES; it decides nothing. It is the sibling of the C-08 cache-doc
# and C-10 release-rule gates: same shape, same fail-closed posture. It reads
# tracked files only, so it needs no package build, no network, and is
# deterministic.
#
# THE RULES
#   PV1 shape       -- the record parses; the top-level sections, per-fixture
#                      and per-group keys exist; the section-2.3 field list is
#                      pinned at its stated size so a field cannot be quietly
#                      dropped from the contract the later rules read.
#   PV2 paths       -- every in-repo path the record names exists on disk.
#   PV3 hashes      -- each transformed_fixture_sha256 is recomputed from the
#                      file's bytes and must match.
#   PV4 counts      -- each fixture's declared count is recomputed from the
#                      file, its groups' counts must sum to it, a CSV group's
#                      count must match the real per-source tally, and a JSON
#                      oracle's own _meta.counts must match the real array
#                      lengths.
#   PV5 in scope    -- a group with section_2_3_applies = true carries EVERY
#                      section-2.3 field, each either a real value or the
#                      literal MISSING[<fp-issue-id>]. Blank, null, "TBD",
#                      "N/A" or an omitted key is a FAIL: a gap must be
#                      visible, not inferable.
#   PV6 out of scope-- a group with section_2_3_applies = false carries a
#                      non-empty out_of_scope_reason and uses NO missing
#                      sentinel. Being outside the requirement is not a gap.
#   PV7 no 2nd copy -- every value this record mirrors from a fixture's own
#                      in-band _meta block agrees with it byte-for-byte, and
#                      no _meta field exists that the record does not mirror.
#   PV8 bijection   -- for a fixture whose groups are keyed by a `source`
#                      column, the recorded group set is an EXACT bijection
#                      with that column's distinct values. A new upstream in
#                      the CSV with no provenance is an ORPHAN; a recorded
#                      group no longer in the CSV is a PHANTOM. Both FAIL.
#
# FIELD RESOLUTION (PV5). A section-2.3 field is looked up on the group first,
# then on the fixture. That is not leniency -- it is how the record states its
# own contract: transformed_fixture_sha256 is a property of the committed file,
# so required_fields_note records it once per fixture rather than once per
# group. No other section-2.3 field name occurs at fixture level, so nothing
# else can be masked by the fallback.
#
# INHERITED OUT-OF-SCOPE REASON (PV6). Likewise, a whole fixture may be out of
# scope (url-standard-conformance.csv is: no group in it vendors upstream
# bytes), in which case the reason is stated once on the fixture and covers its
# groups. A group with NO reason in effect, at either level, FAILS.
#
# SENTINEL SCANNING (PV6). Only the out-of-scope GROUP's own subtree is scanned
# for MISSING[...]. Fixture-level prose legitimately mentions the sentinel when
# explaining it for that fixture's IN-scope groups (external-url-vectors.csv's
# retrieval_date_status does exactly this), and the conventions block defines
# it. Scanning those would flag the explanation as the defect.
#
# Dependencies: jsonlite (Suggests, as tools/oracle-audit-rfc3986.R uses) and
# digest for sha256. Nothing else beyond base R.
#
# Usage:
#   Rscript tools/oracle-provenance-gate.R             # verify, exit 1 on drift
#   Rscript tools/oracle-provenance-gate.R --self-test # positive/negative

RECORD_PATH <- "tests/testthat/fixtures/oracle-provenance.json"

# The record states 14 keys for the 11 requirement items; PV1 pins the size so
# the contract PV5 reads cannot be narrowed without touching this gate.
EXPECTED_FIELD_COUNT <- 14L

REQUIRED_TOP <- c("record_kind", "record_version", "summary",
                  "governing_decision", "conventions", "fixtures")
REQUIRED_DECISION <- c("path", "section", "requirement", "required_fields")
REQUIRED_FIXTURE <- c("path", "fixture_kind", "transformed_fixture_sha256",
                      "in_fixture_provenance", "source_group_key",
                      "source_groups")
REQUIRED_GROUP <- c("group", "row_count", "section_2_3_applies")

# Values that LOOK recorded but state nothing. Section 2.3 requires a real
# value or the sentinel; these are neither.
PLACEHOLDERS <- c("tbd", "n/a", "na", "null", "none", "unknown", "?", "-")

SENTINEL_RE <- "^MISSING\\[RURL-[a-z0-9]+\\]$"

# PV7's mirror map: record group field (name) -> in-band _meta key (value).
# `counts` is structural and compared separately.
MIRROR_FIELDS <- c(
  upstream_project = "upstream_project",
  upstream_revision = "upstream_revision",
  upstream_path = "upstream_path",
  retrieval_date = "retrieved",
  license = "license",
  raw_source_sha256 = "raw_source_sha256",
  import_command = "import_command",
  generation_command = "generation_command",
  standard = "standard",
  standard_version = "standard_version",
  standard_section = "standard_section",
  claim_kind = "claim_kind",
  applicability_selector = "applicability_selector"
)
META_STRUCTURAL <- "counts"

# ---- readers ----------------------------------------------------------------

read_json_file <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

read_csv_file <- function(path) {
  utils::read.csv(path, colClasses = "character", comment.char = "",
                  check.names = FALSE)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

is_json_path <- function(path) endsWith(tolower(path), ".json")

as_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  as.character(x)[1L]
}

# ---- record navigation ------------------------------------------------------

fixtures_of <- function(rec) {
  if (is.null(rec$fixtures)) list() else rec$fixtures
}

groups_of <- function(fx) {
  if (is.null(fx$source_groups)) list() else fx$source_groups
}

fixture_label <- function(fx) as_chr(fx$path)

group_label <- function(fx, g) {
  sprintf("%s/%s", fixture_label(fx), as_chr(g$group))
}

# A field resolves on the group, then on the fixture (see FIELD RESOLUTION).
resolve_field <- function(g, fx, key) {
  if (!is.null(g[[key]])) return(g[[key]])
  fx[[key]]
}

# The count a fixture declares: CSV fixtures state row_count, the JSON oracle
# states case_count$total.
declared_count <- function(fx) {
  if (!is.null(fx$row_count)) return(as.numeric(fx$row_count)[1L])
  if (!is.null(fx$case_count)) return(as.numeric(fx$case_count$total)[1L])
  NA_real_
}

# The count the FILE actually has. JSON: every top-level array except _meta.
actual_count <- function(path) {
  if (is_json_path(path)) {
    j <- read_json_file(path)
    nm <- setdiff(names(j), "_meta")
    return(sum(vapply(j[nm], length, integer(1))))
  }
  nrow(read_csv_file(path))
}

# The `source` column of a CSV fixture, or NULL when it has none.
source_column <- function(root, fx) {
  p <- file.path(root, as_chr(fx$path))
  if (is_json_path(as_chr(fx$path)) || !file.exists(p)) return(NULL)
  d <- read_csv_file(p)
  if (!"source" %in% names(d)) return(NULL)
  as.character(d$source)
}

# ---- field classification ---------------------------------------------------

field_verdict <- function(v) {
  if (is.null(v)) return("absent")
  if (!is.character(v) || length(v) != 1L || is.na(v)) return("not-a-string")
  s <- trimws(v)
  if (!nzchar(s)) return("blank")
  if (grepl("MISSING[", s, fixed = TRUE)) {
    if (grepl(SENTINEL_RE, s)) return("sentinel")
    return("bad-sentinel")
  }
  if (tolower(s) %in% PLACEHOLDERS) return("placeholder")
  "value"
}

has_sentinel <- function(x) {
  flat <- as.character(unlist(x, use.names = FALSE))
  any(grepl("MISSING[", flat, fixed = TRUE))
}

# ---- rules ------------------------------------------------------------------

finding <- function(id, ok, detail) {
  list(list(id = id, ok = ok, detail = detail))
}

missing_keys <- function(x, keys) keys[!keys %in% names(x)]

required_fields <- function(rec) {
  as.character(unlist(rec$governing_decision$required_fields,
                      use.names = FALSE))
}

rule_pv1 <- function(rec, expected_fields) {
  bad <- character(0)
  gaps <- missing_keys(rec, REQUIRED_TOP)
  if (length(gaps)) {
    bad <- c(bad, sprintf("top-level lacks %s", toString(gaps)))
  }
  gd <- rec$governing_decision
  gaps <- missing_keys(gd, REQUIRED_DECISION)
  if (length(gaps)) {
    bad <- c(bad, sprintf("governing_decision lacks %s", toString(gaps)))
  }
  if (is.null(rec$conventions$missing_sentinel)) {
    bad <- c(bad, "conventions lacks missing_sentinel")
  }
  nfields <- length(required_fields(rec))
  if (nfields != expected_fields) {
    bad <- c(bad, sprintf(paste("required_fields has %d entries, gate pinned",
                                "at %d -- the section-2.3 contract changed;",
                                "update this gate deliberately"),
                          nfields, expected_fields))
  }
  fxs <- fixtures_of(rec)
  if (!length(fxs)) bad <- c(bad, "no fixtures recorded")
  for (fx in fxs) {
    lab <- fixture_label(fx)
    gaps <- missing_keys(fx, REQUIRED_FIXTURE)
    if (length(gaps)) {
      bad <- c(bad, sprintf("fixture %s lacks %s", lab, toString(gaps)))
    }
    ncount <- sum(c("row_count", "case_count") %in% names(fx))
    if (ncount != 1L) {
      bad <- c(bad, sprintf(
        "fixture %s must declare exactly one of row_count/case_count", lab))
    }
    gs <- groups_of(fx)
    if (!length(gs)) bad <- c(bad, sprintf("fixture %s has no groups", lab))
    for (g in gs) {
      gaps <- missing_keys(g, REQUIRED_GROUP)
      if (length(gaps)) {
        bad <- c(bad, sprintf("group %s lacks %s", group_label(fx, g),
                              toString(gaps)))
      }
      if (!is.logical(g$section_2_3_applies) ||
            length(g$section_2_3_applies) != 1L) {
        bad <- c(bad, sprintf("group %s: section_2_3_applies is not a flag",
                              group_label(fx, g)))
      }
    }
  }
  finding("PV1", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf(paste("record shape holds: %d fixtures, %d groups,",
                             "%d section-2.3 fields"),
                       length(fxs),
                       sum(vapply(fxs, function(f) length(groups_of(f)),
                                  integer(1))),
                       nfields))
}

rule_pv2 <- function(root, rec) {
  paths <- c(as_chr(rec$governing_decision$path),
             vapply(fixtures_of(rec), fixture_label, character(1)))
  paths <- paths[!is.na(paths)]
  gone <- paths[!file.exists(file.path(root, paths))]
  finding("PV2", length(gone) == 0L,
          if (length(gone)) sprintf("recorded path(s) not on disk: %s",
                                    toString(gone))
          else sprintf("all %d recorded in-repo paths exist", length(paths)))
}

rule_pv3 <- function(root, rec) {
  bad <- character(0)
  n <- 0L
  for (fx in fixtures_of(rec)) {
    p <- file.path(root, fixture_label(fx))
    if (!file.exists(p)) {
      bad <- c(bad, sprintf("%s: cannot hash, file absent", fixture_label(fx)))
      next
    }
    recorded <- as_chr(fx$transformed_fixture_sha256)
    got <- sha256_file(p)
    n <- n + 1L
    if (!identical(recorded, got)) {
      bad <- c(bad, sprintf("%s: recorded %s, recomputed %s",
                            fixture_label(fx), recorded, got))
    }
  }
  finding("PV3", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf("%d recomputed sha256 digest(s) match the record", n))
}

rule_pv4 <- function(root, rec) {
  bad <- character(0)
  checks <- 0L
  for (fx in fixtures_of(rec)) {
    lab <- fixture_label(fx)
    p <- file.path(root, lab)
    if (!file.exists(p)) {
      bad <- c(bad, sprintf("%s: cannot count, file absent", lab))
      next
    }
    declared <- declared_count(fx)
    actual <- actual_count(p)
    checks <- checks + 1L
    if (!isTRUE(declared == actual)) {
      bad <- c(bad, sprintf("%s: declares %s rows, file has %d",
                            lab, format(declared), actual))
    }
    gs <- groups_of(fx)
    total <- sum(vapply(gs, function(g) as.numeric(g$row_count)[1L],
                        numeric(1)))
    if (!isTRUE(total == declared)) {
      bad <- c(bad, sprintf("%s: group counts sum to %s, fixture declares %s",
                            lab, format(total), format(declared)))
    }
    src <- source_column(root, fx)
    if (!is.null(src)) {
      for (g in gs) {
        key <- as_chr(g$group)
        tally <- sum(src == key)
        checks <- checks + 1L
        if (!isTRUE(as.numeric(g$row_count)[1L] == tally)) {
          bad <- c(bad, sprintf("%s: records %s rows, CSV has %d",
                                group_label(fx, g),
                                format(as.numeric(g$row_count)[1L]), tally))
        }
      }
    }
    if (is_json_path(lab)) {
      j <- read_json_file(p)
      counts <- j[["_meta"]][[META_STRUCTURAL]]
      for (nm in names(counts)) {
        checks <- checks + 1L
        got <- length(j[[nm]])
        if (!isTRUE(as.numeric(counts[[nm]])[1L] == got)) {
          bad <- c(bad, sprintf("%s: _meta.counts.%s says %s, array has %d",
                                lab, nm, format(counts[[nm]]), got))
        }
      }
    }
  }
  finding("PV4", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf("%d recomputed count(s) match the record", checks))
}

rule_pv5 <- function(rec) {
  fields <- required_fields(rec)
  bad <- character(0)
  n <- 0L
  for (fx in fixtures_of(rec)) {
    for (g in groups_of(fx)) {
      if (!isTRUE(g$section_2_3_applies)) next
      n <- n + 1L
      for (key in fields) {
        verdict <- field_verdict(resolve_field(g, fx, key))
        if (!verdict %in% c("value", "sentinel")) {
          bad <- c(bad, sprintf("%s: %s is %s", group_label(fx, g), key,
                                verdict))
        }
      }
    }
  }
  finding("PV5", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf(paste("all %d in-scope group(s) carry every one of the",
                             "%d section-2.3 fields"), n, length(fields)))
}

rule_pv6 <- function(rec) {
  bad <- character(0)
  n <- 0L
  for (fx in fixtures_of(rec)) {
    for (g in groups_of(fx)) {
      if (!identical(g$section_2_3_applies, FALSE)) next
      n <- n + 1L
      lab <- group_label(fx, g)
      reason <- resolve_field(g, fx, "out_of_scope_reason")
      if (!field_verdict(reason) %in% c("value", "sentinel")) {
        bad <- c(bad, sprintf("%s: out_of_scope_reason is %s", lab,
                              field_verdict(reason)))
      }
      if (has_sentinel(g)) {
        bad <- c(bad, sprintf(
          "%s: out of scope yet uses a MISSING[...] sentinel", lab))
      }
    }
  }
  finding("PV6", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf(paste("all %d out-of-scope group(s) state a reason and",
                             "use no sentinel"), n))
}

rule_pv7 <- function(root, rec) {
  bad <- character(0)
  n <- 0L
  for (fx in fixtures_of(rec)) {
    lab <- fixture_label(fx)
    p <- file.path(root, lab)
    if (!is_json_path(lab) || !file.exists(p)) next
    meta <- read_json_file(p)[["_meta"]]
    if (is.null(meta)) next
    gs <- groups_of(fx)
    if (length(gs) != 1L) {
      bad <- c(bad, sprintf(
        "%s: carries an in-band _meta but %d groups -- the mirror is ambiguous",
        lab, length(gs)))
      next
    }
    g <- gs[[1L]]
    for (key in names(MIRROR_FIELDS)) {
      mk <- MIRROR_FIELDS[[key]]
      if (!mk %in% names(meta)) next
      n <- n + 1L
      if (!identical(as_chr(g[[key]]), as_chr(meta[[mk]]))) {
        bad <- c(bad, sprintf("%s: record %s = %s, in-band _meta.%s = %s",
                              lab, key, as_chr(g[[key]]), mk,
                              as_chr(meta[[mk]])))
      }
    }
    counts <- meta[[META_STRUCTURAL]]
    for (nm in names(counts)) {
      n <- n + 1L
      if (!identical(as_chr(fx$case_count[[nm]]), as_chr(counts[[nm]]))) {
        bad <- c(bad, sprintf("%s: record case_count.%s = %s, _meta.counts.%s",
                              lab, nm, as_chr(fx$case_count[[nm]]), nm))
      }
    }
    unmirrored <- setdiff(names(meta), c(unname(MIRROR_FIELDS),
                                         META_STRUCTURAL))
    if (length(unmirrored)) {
      bad <- c(bad, sprintf(paste("%s: _meta field(s) %s are mirrored nowhere",
                                  "-- extend the record and this gate's",
                                  "mirror map"), lab, toString(unmirrored)))
    }
  }
  finding("PV7", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf("%d mirrored value(s) agree with the in-band _meta", n))
}

rule_pv8 <- function(root, rec) {
  bad <- character(0)
  n <- 0L
  for (fx in fixtures_of(rec)) {
    src <- source_column(root, fx)
    if (is.null(src)) next
    n <- n + 1L
    lab <- fixture_label(fx)
    recorded <- vapply(groups_of(fx), function(g) as_chr(g[["group"]]),
                       character(1))
    dupes <- unique(recorded[duplicated(recorded)])
    if (length(dupes)) {
      bad <- c(bad, sprintf("%s: duplicate group key(s) %s", lab,
                            toString(dupes)))
    }
    present <- sort(unique(src))
    orphan <- setdiff(present, recorded)
    phantom <- setdiff(recorded, present)
    if (length(orphan)) {
      bad <- c(bad, sprintf("%s: ORPHAN source(s) with no provenance: %s",
                            lab, toString(orphan)))
    }
    if (length(phantom)) {
      bad <- c(bad, sprintf("%s: PHANTOM group(s) absent from the CSV: %s",
                            lab, toString(phantom)))
    }
  }
  finding("PV8", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf(paste("group keys are a bijection with the source",
                             "column in %d source-keyed fixture(s)"), n))
}

check_oracle_provenance <- function(root = ".",
                                    record = RECORD_PATH,
                                    expected_fields = EXPECTED_FIELD_COUNT) {
  p <- file.path(root, record)
  if (!file.exists(p)) {
    return(finding("PV0", FALSE, sprintf("provenance record not found: %s", p)))
  }
  rec <- tryCatch(read_json_file(p), error = function(e) e)
  if (inherits(rec, "error")) {
    return(finding("PV0", FALSE,
                   sprintf("record does not parse: %s", conditionMessage(rec))))
  }
  shape <- rule_pv1(rec, expected_fields)
  if (!shape[[1L]]$ok) {
    rest <- lapply(c("PV2", "PV3", "PV4", "PV5", "PV6", "PV7", "PV8"),
                   function(id) {
                     list(id = id, ok = FALSE,
                          detail = "not evaluated -- record shape is broken")
                   })
    return(c(shape, rest))
  }
  c(shape,
    rule_pv2(root, rec), rule_pv3(root, rec), rule_pv4(root, rec),
    rule_pv5(rec), rule_pv6(rec), rule_pv7(root, rec), rule_pv8(root, rec))
}

# ---- reporting --------------------------------------------------------------

print_findings <- function(findings) {
  ok <- TRUE
  for (f in findings) {
    cat(sprintf("%-4s %-5s %s\n", f$id, if (f$ok) "PASS" else "FAIL", f$detail))
    if (!f$ok) ok <- FALSE
  }
  cat(sprintf("VERDICT: %s\n", if (ok) "PASS" else "FAIL"))
  ok
}

# ---- self-test --------------------------------------------------------------

# A synthetic tree in tempdir(): a JSON oracle carrying an in-band _meta, a
# source-keyed CSV, and a record derived from BOTH files' real bytes so the
# positive case passes for the same reason the real tree does. File-side
# mutations are applied before the record is derived, so a negative isolates
# the rule under test instead of also tripping PV3/PV4.
self_test <- function() {
  st <- new.env(parent = emptyenv())
  st$pass <- 0L
  st$fail <- character(0)
  expect <- function(label, cond) {
    if (isTRUE(cond)) st$pass <- st$pass + 1L else st$fail <- c(st$fail, label)
  }

  sentinel <- "MISSING[RURL-vwurxmzm]"
  meta_base <- list(
    upstream_project = "acme/suite",
    upstream_revision = "0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c",
    upstream_path = "url/resources/cases.json",
    retrieved = "2026-01-02",
    license = "BSD-3-Clause (acme contributors)",
    raw_source_sha256 = strrep("ab", 32L),
    import_command = "curl -fsSL https://example.invalid/cases.json -o c.json",
    generation_command = "python3 make.py c.json",
    standard = "WHATWG URL Standard",
    standard_version = "Living Standard (unversioned)",
    standard_section = "URL parsing",
    claim_kind = "conformance",
    applicability_selector = "base null"
  )

  mk <- function(mutate = NULL, meta_extra = NULL, csv_sources = NULL) {
    root <- tempfile("oracleprov-")
    dir.create(file.path(root, "tests", "testthat", "fixtures"),
               recursive = TRUE)
    dir.create(file.path(root, "inst", "bench"), recursive = TRUE)
    dir.create(file.path(root, "design"), recursive = TRUE)
    writeLines("decision", file.path(root, "design", "policy.md"))

    meta <- c(meta_base, meta_extra,
              list(counts = list(success = 2L, failure = 1L)))
    cases <- list(`_meta` = meta, success = list("a", "b"),
                  failure = list("c"))
    json_rel <- "inst/bench/cases.json"
    jsonlite::write_json(cases, file.path(root, json_rel), auto_unbox = TRUE,
                         pretty = TRUE)

    src <- if (is.null(csv_sources)) {
      c("alpha", "alpha", "alpha", "beta", "beta")
    } else {
      csv_sources
    }
    csv_rel <- "tests/testthat/fixtures/vectors.csv"
    utils::write.csv(data.frame(id = seq_along(src), source = src),
                     file.path(root, csv_rel), row.names = FALSE)

    in_scope <- c(list(group = "alpha", row_count = sum(src == "alpha"),
                       section_2_3_applies = TRUE),
                  meta_base[setdiff(names(meta_base), "retrieved")],
                  list(retrieval_date = sentinel))
    out_scope <- list(group = "beta", row_count = sum(src == "beta"),
                      section_2_3_applies = FALSE,
                      out_of_scope_reason = "generated here; nothing imported")
    json_group <- c(list(group = "wpt-like", row_count = 3L,
                         section_2_3_applies = TRUE),
                    meta_base[setdiff(names(meta_base), "retrieved")],
                    list(retrieval_date = meta_base$retrieved))

    rec <- list(
      record_kind = "oracle-provenance",
      record_version = 1L,
      summary = "synthetic self-test record",
      governing_decision = list(
        path = "design/policy.md", section = "2.3",
        requirement = "every imported oracle pins provenance",
        required_fields = c(setdiff(names(meta_base), "retrieved"),
                            "retrieval_date",
                            "transformed_fixture_sha256")),
      conventions = list(missing_sentinel = sentinel),
      fixtures = list(
        list(path = json_rel, fixture_kind = "imported suite",
             transformed_fixture_sha256 = sha256_file(file.path(root,
                                                                json_rel)),
             case_count = list(total = 3L, success = 2L, failure = 1L),
             in_fixture_provenance = "_meta block",
             source_group_key = "not applicable",
             source_groups = list(json_group)),
        list(path = csv_rel, fixture_kind = "multi-source table",
             transformed_fixture_sha256 = sha256_file(file.path(root,
                                                                csv_rel)),
             row_count = length(src),
             in_fixture_provenance = "per-row source column",
             source_group_key = "source column",
             source_groups = list(in_scope, out_scope))))
    if (!is.null(mutate)) rec <- mutate(rec)
    jsonlite::write_json(rec, file.path(root, RECORD_PATH), auto_unbox = TRUE,
                         pretty = TRUE)
    root
  }

  # required_fields is the 13 _meta names minus `retrieved`, plus
  # transformed_fixture_sha256 and retrieval_date -- 14, matching the real pin.
  nfields <- length(meta_base) + 1L
  rule <- function(root, id) {
    for (f in check_oracle_provenance(root, expected_fields = nfields)) {
      if (identical(f$id, id)) return(isTRUE(f$ok))
    }
    NA
  }
  verdict <- function(root) {
    all(vapply(check_oracle_provenance(root, expected_fields = nfields),
               function(f) isTRUE(f$ok), logical(1)))
  }
  set_group <- function(fixture, group, key, value) {
    function(rec) {
      rec$fixtures[[fixture]]$source_groups[[group]][[key]] <- value
      rec
    }
  }

  # 1. Clean positive -- every rule passes on a consistent tree.
  expect("positive: consistent tree passes", verdict(mk()))

  # 2. PV0 -- a missing record fails closed rather than passing vacuously.
  expect("missing record fails closed",
         identical(verdict(tempfile("oracleprov-none-")), FALSE))

  # 3. PV1 -- a dropped top-level section.
  r <- mk(function(rec) {
    rec$conventions <- NULL
    rec
  })
  expect("PV1 fails on a dropped top-level section",
         identical(rule(r, "PV1"), FALSE))

  # 4. PV1 -- a group missing its scope flag.
  r <- mk(function(rec) {
    rec$fixtures[[2L]]$source_groups[[1L]]$section_2_3_applies <- NULL
    rec
  })
  expect("PV1 fails on a group with no scope flag",
         identical(rule(r, "PV1"), FALSE))

  # 5. PV1 -- narrowing the section-2.3 contract is a deliberate act.
  r <- mk(function(rec) {
    gd <- rec$governing_decision$required_fields
    rec$governing_decision$required_fields <- gd[-1L]
    rec
  })
  expect("PV1 fails when required_fields shrinks",
         identical(rule(r, "PV1"), FALSE))

  # 6. PV2 -- a recorded path that is not on disk.
  r <- mk(function(rec) {
    rec$governing_decision$path <- "design/not-there.md"
    rec
  })
  expect("PV2 fails on a recorded path that does not exist",
         identical(rule(r, "PV2"), FALSE))

  # 7. PV3 -- one flipped character in a recorded digest.
  r <- mk(function(rec) {
    h <- rec$fixtures[[2L]]$transformed_fixture_sha256
    substr(h, 1L, 1L) <- if (startsWith(h, "a")) "b" else "a"
    rec$fixtures[[2L]]$transformed_fixture_sha256 <- h
    rec
  })
  expect("PV3 fails on a one-character hash flip",
         identical(rule(r, "PV3"), FALSE))

  # 8. PV4 -- a fixture count that no longer matches the file.
  r <- mk(function(rec) {
    rec$fixtures[[2L]]$row_count <- 99L
    rec
  })
  expect("PV4 fails when a fixture count is wrong",
         identical(rule(r, "PV4"), FALSE))

  # 9. PV4 -- group counts that no longer sum to the fixture count.
  r <- mk(set_group(2L, 1L, "row_count", 1L))
  expect("PV4 fails when group counts do not sum",
         identical(rule(r, "PV4"), FALSE))

  # 10. PV4 -- the JSON oracle's own _meta.counts vs the real array lengths.
  root <- mk()
  p <- file.path(root, "inst", "bench", "cases.json")
  j <- read_json_file(p)
  j[["_meta"]]$counts$success <- 7L
  jsonlite::write_json(j, p, auto_unbox = TRUE, pretty = TRUE)
  expect("PV4 fails when _meta.counts disagrees with the arrays",
         identical(rule(root, "PV4"), FALSE))

  # 11. PV5 -- an omitted section-2.3 field.
  r <- mk(set_group(2L, 1L, "license", NULL))
  expect("PV5 fails on an omitted section-2.3 field",
         identical(rule(r, "PV5"), FALSE))

  # 12. PV5 -- a blank one.
  r <- mk(set_group(2L, 1L, "license", ""))
  expect("PV5 fails on a blank section-2.3 field",
         identical(rule(r, "PV5"), FALSE))

  # 13. PV5 -- the "TBD"/"N/A" placeholder class.
  r <- mk(set_group(2L, 1L, "license", "TBD"))
  expect("PV5 fails on a TBD placeholder", identical(rule(r, "PV5"), FALSE))
  r <- mk(set_group(2L, 1L, "import_command", "N/A"))
  expect("PV5 fails on an N/A placeholder", identical(rule(r, "PV5"), FALSE))

  # 14. PV5 -- a sentinel whose carrier is not an fp issue id.
  r <- mk(set_group(2L, 1L, "import_command", "MISSING[TODO]"))
  expect("PV5 fails on a sentinel with no fp carrier",
         identical(rule(r, "PV5"), FALSE))

  # 15. PV5 -- a well-formed sentinel is the accepted way to record a gap.
  r <- mk(set_group(2L, 1L, "import_command", "MISSING[RURL-abc123]"))
  expect("PV5 passes on a well-formed sentinel",
         identical(rule(r, "PV5"), TRUE))

  # 16. The fixture-level fallback is load-bearing, not decoration: no group
  #     carries transformed_fixture_sha256, so the clean positive above passes
  #     PV5 only because the fallback resolves. Delete the sole copy and the
  #     gate goes red (PV1 reaches the absent key first).
  r <- mk(function(rec) {
    rec$fixtures[[2L]]$transformed_fixture_sha256 <- NULL
    rec
  })
  expect("removing the only copy of a fixture hash turns the gate red",
         identical(verdict(r), FALSE))

  # 17. PV6 -- an out-of-scope group with no reason at either level.
  r <- mk(set_group(2L, 2L, "out_of_scope_reason", NULL))
  expect("PV6 fails on an out-of-scope group with no reason",
         identical(rule(r, "PV6"), FALSE))

  # 18. PV6 -- and it passes when the reason is inherited from the fixture.
  r <- mk(function(rec) {
    rec$fixtures[[2L]]$source_groups[[2L]]$out_of_scope_reason <- NULL
    rec$fixtures[[2L]]$out_of_scope_reason <- "whole fixture is out of scope"
    rec
  })
  expect("PV6 accepts a fixture-level reason",
         identical(rule(r, "PV6"), TRUE))

  # 19. PV6 -- out of scope is not a gap, so a sentinel there is a defect.
  r <- mk(set_group(2L, 2L, "upstream_path", sentinel))
  expect("PV6 fails when an out-of-scope group uses a sentinel",
         identical(rule(r, "PV6"), FALSE))

  # 20. PV7 -- the record disagrees with the fixture's own _meta.
  r <- mk(set_group(1L, 1L, "upstream_revision", "deadbeef"))
  expect("PV7 fails on a mirrored value that drifted",
         identical(rule(r, "PV7"), FALSE))

  # 21. PV7 -- a mirrored count that drifted.
  r <- mk(function(rec) {
    rec$fixtures[[1L]]$case_count$success <- 5L
    rec
  })
  expect("PV7 fails on a mirrored count that drifted",
         identical(rule(r, "PV7"), FALSE))

  # 22. PV7 -- a NEW _meta field the record mirrors nowhere. The file is
  #     mutated before the record is derived, so PV3/PV4 still pass and PV7 is
  #     demonstrably the rule that caught it.
  r <- mk(meta_extra = list(curation_note = "added upstream, unmirrored"))
  expect("PV7 fails on an unmirrored _meta field",
         identical(rule(r, "PV7"), FALSE))
  expect("PV7 is the only rule that catches an unmirrored _meta field",
         identical(rule(r, "PV3"), TRUE) && identical(rule(r, "PV4"), TRUE))

  # 23. PV8 -- a new upstream in the CSV with no provenance group (ORPHAN).
  r <- mk(csv_sources = c("alpha", "alpha", "alpha", "beta", "beta", "gamma"))
  expect("PV8 fails on an orphan source", identical(rule(r, "PV8"), FALSE))

  # 24. PV8 -- a recorded group no longer in the CSV (PHANTOM). Its row_count
  #     is 0, so the counts still reconcile and PV8 alone catches it.
  r <- mk(function(rec) {
    gs <- rec$fixtures[[2L]]$source_groups
    rec$fixtures[[2L]]$source_groups <- c(gs, list(list(
      group = "gamma", row_count = 0L, section_2_3_applies = FALSE,
      out_of_scope_reason = "retired upstream")))
    rec
  })
  expect("PV8 fails on a phantom group", identical(rule(r, "PV8"), FALSE))
  expect("PV8 is the only rule that catches a zero-row phantom",
         identical(rule(r, "PV4"), TRUE))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("oracle-provenance-gate self-test: FAIL", call. = FALSE)
  }
  cat("VERDICT: PASS\n")
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  for (pkg in c("jsonlite", "digest")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("oracle-provenance gate needs the '%s' package", pkg),
           call. = FALSE)
    }
  }
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }
  ok <- print_findings(check_oracle_provenance("."))
  if (!ok) {
    stop("oracle-provenance gate: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
