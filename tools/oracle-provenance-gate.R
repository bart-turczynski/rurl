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
#   PV9 source pins -- wherever a group carries normative_dependencies, it is
#                      a non-empty ARRAY of objects, each carrying the ten keys
#                      conventions.normative_dependency_scope names, a
#                      pin_status and a revision_scheme drawn from that
#                      convention's two enums, a tracking_issue whenever the
#                      status is `missing`, a not_applicable_reason drawn from
#                      the third enum whenever -- and ONLY whenever -- the
#                      status is `not-applicable`, an immutable_revision
#                      validated ACCORDING TO ITS SCHEME plus ISO dates
#                      whenever it is `verified`, at least one algorithm
#                      anchor, and never the MISSING[...] sentinel.
#  PV10 source answer-- EVERY source group in EVERY fixture CARRIES a non-empty
#                      normative_dependencies. There is no exemption: a group
#                      that derives nothing from a normative source records one
#                      entry with pin_status = "not-applicable" saying so.
#                      Absence is a FAIL, never a pass.
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
# SHAPE, NOT PRESENCE (PV9). normative_dependencies carries the record's SECOND
# pinning duty -- source pinning -- which conventions.normative_dependency_scope
# states as orthogonal to section 2.3: a group may owe both duties, either one,
# or neither. PV9 therefore judges only groups that CARRY the key, and judges
# only its shape. Deciding which groups OWE a pin is a different question with a
# different evidence base, and it is not this rule's business.
#
# PRESENCE, UNIVERSALLY (PV10). What PV9 leaves open is the hole the ticket that
# produced both rules came through, and it is worth naming exactly: the defect
# was not a wrong value, it was SILENCE. ip-obfuscation derived its 24 expected
# values by transcribing a Living Standard, and because NO FIELD IN THE RECORD
# NAMED THAT DEPENDENCY, nobody was ever asked to date it -- so six section
# citations that resolve to no revision of the standard survived review inside a
# group the gate reported as fully provenanced. PV9 only fires on groups that
# already volunteer the key, so it cannot see that: a source group added
# tomorrow could omit normative_dependencies entirely and every rule above would
# stay green. PV10 closes it from the other side by making the QUESTION
# mandatory rather than the pin. An absent key is indistinguishable from an
# unasked question, so absence is an error and never a pass.
#
# WHAT PV10 DOES AND DOES NOT INFER FROM THE CONVENTION. Read
# conventions.normative_dependency_scope precisely: it states when the source-
# pinning DUTY applies ("to any source group whose oracle is transcribed,
# computed, or hand-derived from a standard"), and it supplies the enum member
# that makes a universal ANSWER writable -- pin_status "not-applicable" (the
# duty does not apply to this source), whose whole purpose is to let a group
# that owes no pin still answer. It does not itself spell out "every
# group must carry the key"; PV10 is the rule that says so. The record already
# practises it -- inst/bench/wpt-url-cases.json's group carries exactly such a
# negative declaration and explains it as recording "that the source-pinning
# question was considered and does not apply, rather than leaving the key
# absent" -- and PV10 makes that practice non-optional for the next group.
# Judging WHETHER a not-applicable answer is the honest one stays out of scope:
# PV10 demands an answer, PV9 demands it be well-formed, and a human still owns
# whether it is true.
#
# WHY A REVISION HAS A SCHEME (PV9). The first cut of this rule required every
# `verified` pin to be a 40-hex git sha, which is true of the WHATWG entry and
# false in general: UTS-46 has no repository, so its pin can only ever be a
# named edition. A gate that rejected that would have obstructed the closure of
# the very ticket that produced it. That is no longer hypothetical -- UTS-46 was
# pinned as "UTS #46 revision 35 (2025-09-04), IDNA mapping table Unicode
# 16.0.0", which the sha rule would have refused outright, and which names TWO
# coordinates because the document's revisions and the mapping table's Unicode
# versions run on independent cadences. revision_scheme names the kind of
# revision, and the check follows the scheme instead of assuming one. What the
# gate CANNOT do is decide immutability; see NON_IMMUTABLE_REVISIONS for the
# honest limit -- and it cannot decide TRUTH either: nothing here proves the 29
# transcribed mappings hold at the pinned edition. That is a network check, run
# by hand, at tools/oracle/check-uts46-mapping-pin.R.
#
# WHY PV9 DOES NOT REUSE THE SENTINEL (and why PV6 needs no re-scoping). The
# convention makes pin_status an enum precisely so an unpinned normative source
# can be recorded inside a section_2_3_applies = false group without PV6 reading
# it as a section-2.3 gap. PV9 enforces that separation from the other side: a
# MISSING[...] anywhere in an entry FAILS, in every group, in scope or out.
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

# PV9's contract, read off conventions.normative_dependency_scope rather than
# invented here: "Each entry names its source, source_url, immutable_revision,
# revision_scheme, revision_date, retrieved_at, pin_kind, pin_status and
# algorithm_anchors, plus a note stating what is load-bearing about it".
# tracking_issue is NOT in this list because the convention makes it
# conditional on pin_status = "missing".
NORMATIVE_DEP_FIELDS <- c("source", "source_url", "immutable_revision",
                          "revision_scheme", "revision_date", "retrieved_at",
                          "pin_kind", "pin_status", "algorithm_anchors", "note")

# The same convention's first enum, verbatim and closed. A STATUS AXIS ONLY:
# "verified" = the derivation was checked against that revision on retrieved_at;
# "missing" = the source is read but no revision is pinned; "not-applicable" =
# the duty does not apply to this source. WHY the duty does not apply is a
# different question and lives in NORMATIVE_DEP_NA_REASON below.
NORMATIVE_DEP_STATUS <- c("verified", "missing", "not-applicable")

# The convention's THIRD enum (RURL-ynirvjxb), and the reason axis the status
# member used to swallow. "not-applicable" was glossed as "the source is cited
# but nothing is derived from it", which was FALSE for two of the eight entries
# carrying it: RFC 3986's 25 rows and PRD 6.1's rows ARE hand-derived from their
# source's text. What is inapplicable there is the DUTY, not the derivation.
#
#   no-derivation   the source is cited but no expected value is derived from
#                   its TEXT -- values read out of vendored hash-pinned bytes,
#                   or the authority is another document such as a paper.
#   frozen-source   derived, but the source is a published, numbered document
#                   that cannot be amended in place, so the amendment hazard
#                   the duty exists to detect cannot arise.
#   internal-source derived, but the source is IN this repository and versioned
#                   by its git history, so there is no external revision to name
#                   and a change to it is a diff reviewable beside the fixture.
#
# A SEPARATE KEY, NOT A FOURTH pin_status MEMBER, which is what the finding
# asked for. pin_status answers "what is the state of the pin?" and this answers
# "why is there no pinning duty?"; one field cannot carry both axes. The status
# axis is closed while this one is OPEN -- it went from one recognised reason to
# three inside a single 13-entry record -- so a member per reason would keep
# being added, whereas a closed second enum grows where it was built to.
NORMATIVE_DEP_NA_REASON <- c("no-derivation", "frozen-source",
                             "internal-source")

# Its second enum: what KIND of thing immutable_revision names. A normative
# source's immutable revision is NOT always a git commit -- UTS-46 has no
# repository to cite -- and a rule that demanded one would have made the still
# -open half of RURL-qhwktfcw unpinnable, obstructing the closure of the ticket
# that created the rule. The scheme is what lets the check be shape-directed
# instead of assuming git.
NORMATIVE_DEP_SCHEME <- c("git-commit", "document-version", "unpinned")

# "unpinned" claims nothing, so it is legal only where the status claims
# nothing either.
UNPINNABLE_STATUS <- c("missing", "not-applicable")

# HONEST LIMIT. Immutability is not machine-checkable: no predicate can tell
# "Unicode 15.1.0" (a frozen edition) from "Unicode 16" (one that may not exist
# yet). What IS checkable is the specific class of non-immutable strings that
# actually turns up in practice, which is the mistake worth catching. Matched
# case-insensitively on the trimmed value; this rejects the placeholders that
# get written, NOT every string a careless author could invent.
NON_IMMUTABLE_REVISIONS <- c("", "latest", "head", "living standard",
                             "unversioned", "not pinned", "not recorded",
                             "n/a", "current")

# Character classes written out rather than ranged. `[a-f]` is a collation
# range and this repo has been bitten by locale-dependent matching before; the
# enumerated set means the same thing under every LC_COLLATE.
COMMIT_SHA_RE <- "^[0123456789abcdef]{40}$"
ISO_DATE_RE <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

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

# A JSON scalar string, or NA when the value is absent or is something else.
# PV9 needs this rather than as_chr(): "the key is present but holds an object
# or an array" must FAIL, not be silently coerced to its first element.
one_string <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  x
}

is_commit_sha <- function(s) !is.na(s) && grepl(COMMIT_SHA_RE, s)

# A named edition, to the extent that is decidable -- see the honest limit on
# NON_IMMUTABLE_REVISIONS. This does NOT prove the edition is immutable; it
# proves the author did not write a moving target's usual name.
is_named_edition <- function(s) {
  if (is.na(s)) return(FALSE)
  !tolower(trimws(s)) %in% NON_IMMUTABLE_REVISIONS
}

# Shape AND validity: 2026-13-45 has the shape and is not a date.
is_iso_date <- function(s) {
  if (is.na(s) || !grepl(ISO_DATE_RE, s)) return(FALSE)
  !is.na(as.Date(s, format = "%Y-%m-%d"))
}

# A JSON array of non-blank strings. A JSON object fails on its names, a bare
# scalar string is accepted -- it is a character vector of length one.
is_anchor_vector <- function(x) {
  if (!is.null(names(x))) return(FALSE)
  flat <- as.character(unlist(x, use.names = FALSE))
  length(flat) > 0L && !anyNA(flat) && all(nzchar(trimws(flat)))
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

# One entry of a group's normative_dependencies array. Returns the defects it
# has, labelled; character(0) when it is well-formed. Every value check is
# guarded on the key being PRESENT, so an omitted key is reported once by the
# required-key check rather than twice.
normative_dep_defects <- function(e, lab) {
  bad <- character(0)
  if (!is.list(e) || is.null(names(e))) {
    return(sprintf("%s is not an object", lab))
  }
  gaps <- missing_keys(e, NORMATIVE_DEP_FIELDS)
  if (length(gaps)) {
    bad <- c(bad, sprintf("%s lacks %s", lab, toString(gaps)))
  }
  status <- NA_character_
  if ("pin_status" %in% names(e)) {
    status <- one_string(e$pin_status)
    if (is.na(status) || !status %in% NORMATIVE_DEP_STATUS) {
      shown <- if (is.na(status)) "not a scalar string" else {
        paste0("\"", status, "\"")
      }
      bad <- c(bad, sprintf("%s: pin_status is %s, not one of %s", lab, shown,
                            toString(NORMATIVE_DEP_STATUS)))
      status <- NA_character_
    }
  }
  scheme <- NA_character_
  if ("revision_scheme" %in% names(e)) {
    scheme <- one_string(e$revision_scheme)
    if (is.na(scheme) || !scheme %in% NORMATIVE_DEP_SCHEME) {
      shown <- if (is.na(scheme)) "not a scalar string" else {
        paste0("\"", scheme, "\"")
      }
      bad <- c(bad, sprintf("%s: revision_scheme is %s, not one of %s", lab,
                            shown, toString(NORMATIVE_DEP_SCHEME)))
      scheme <- NA_character_
    }
  }
  if (identical(status, "missing")) {
    if (!identical(field_verdict(e$tracking_issue), "value")) {
      bad <- c(bad, sprintf(paste("%s: pin_status = missing without a",
                                  "tracking_issue -- an unpinned source is a",
                                  "filed gap, not an exemption"), lab))
    }
    if (!is.na(scheme) && !identical(scheme, "unpinned")) {
      bad <- c(bad, sprintf(paste("%s: pin_status = missing but",
                                  "revision_scheme is \"%s\" -- a filed gap",
                                  "cannot also claim a pinning scheme"),
                            lab, scheme))
    }
  }
  # The symmetric conditional to tracking_issue-on-missing, and for the same
  # kind of reason: a status that excuses the duty must say WHICH excuse it is.
  # Six of the record's eight not-applicable entries derive nothing; two derive
  # 25 rows and a fixture's worth respectively. Without this key a consumer
  # reading pin_status cannot tell those apart -- the same undifferentiated
  # pass that normative_dependencies itself was added to close.
  if (identical(status, "not-applicable")) {
    reason <- one_string(e$not_applicable_reason)
    if (is.na(reason) || !reason %in% NORMATIVE_DEP_NA_REASON) {
      shown <- if (is.na(reason)) "absent or not a scalar string" else {
        paste0("\"", reason, "\"")
      }
      bad <- c(bad, sprintf(paste("%s: pin_status = not-applicable but",
                                  "not_applicable_reason is %s -- say which of",
                                  "%s applies; the status member alone cannot"),
                            lab, shown, toString(NORMATIVE_DEP_NA_REASON)))
    }
  } else if (!is.na(status) && "not_applicable_reason" %in% names(e)) {
    # Forbidden rather than merely unrequired. An entry that claims a pin state
    # cannot also excuse the duty, and the copy-paste that leaves the key behind
    # is exactly how a real pin would come to read as an exemption.
    bad <- c(bad, sprintf(paste("%s: pin_status = \"%s\" but a",
                                "not_applicable_reason is recorded -- the",
                                "reason axis applies only where the duty does",
                                "not"), lab, status))
  }
  if (identical(scheme, "unpinned") && !is.na(status) &&
        !status %in% UNPINNABLE_STATUS) {
    bad <- c(bad, sprintf(paste("%s: revision_scheme = unpinned is legal only",
                                "where pin_status is %s, not \"%s\""),
                          lab, toString(UNPINNABLE_STATUS), status))
  }
  if (identical(status, "verified")) {
    # Validated ACCORDING TO THE SCHEME. Assuming git here is what made the
    # first cut of this rule reject a legitimate document-version pin.
    if ("immutable_revision" %in% names(e) && !is.na(scheme)) {
      rev <- one_string(e$immutable_revision)
      if (identical(scheme, "git-commit") && !is_commit_sha(rev)) {
        bad <- c(bad, sprintf(paste("%s: revision_scheme = git-commit but",
                                    "immutable_revision is not a",
                                    "40-character lowercase hex commit sha"),
                              lab))
      }
      if (identical(scheme, "document-version") && !is_named_edition(rev)) {
        bad <- c(bad, sprintf(paste("%s: revision_scheme = document-version",
                                    "but immutable_revision is \"%s\", a",
                                    "moving target rather than a named",
                                    "edition"),
                              lab, if (is.na(rev)) "" else rev))
      }
    }
    for (key in c("revision_date", "retrieved_at")) {
      if (key %in% names(e) && !is_iso_date(one_string(e[[key]]))) {
        bad <- c(bad, sprintf(paste("%s: pin_status = verified but %s is not",
                                    "an ISO yyyy-mm-dd date"), lab, key))
      }
    }
  }
  if ("algorithm_anchors" %in% names(e) &&
        !is_anchor_vector(e$algorithm_anchors)) {
    bad <- c(bad, sprintf(paste("%s: algorithm_anchors is not a non-empty",
                                "character vector -- the anchors are the",
                                "durable key, not the section numbers"), lab))
  }
  if (has_sentinel(e)) {
    bad <- c(bad, sprintf(paste("%s: uses a MISSING[...] sentinel -- an",
                                "unpinned normative source is recorded",
                                "through the pin_status enum instead"), lab))
  }
  bad
}

rule_pv9 <- function(rec) {
  bad <- character(0)
  ngroups <- 0L
  nentries <- 0L
  for (fx in fixtures_of(rec)) {
    for (g in groups_of(fx)) {
      deps <- g[["normative_dependencies"]]
      if (is.null(deps)) next
      ngroups <- ngroups + 1L
      lab <- group_label(fx, g)
      if (!is.list(deps) || !is.null(names(deps)) || !length(deps)) {
        bad <- c(bad, sprintf(paste("%s: normative_dependencies is not a",
                                    "non-empty array -- one derivation may",
                                    "read several sources"), lab))
        next
      }
      for (i in seq_along(deps)) {
        nentries <- nentries + 1L
        bad <- c(bad, normative_dep_defects(
          deps[[i]], sprintf("%s: normative_dependencies[%d]", lab, i)))
      }
    }
  }
  finding("PV9", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf(paste("%d normative-dependency entr%s in %d group(s)",
                             "are well-formed"),
                       nentries, if (nentries == 1L) "y" else "ies", ngroups))
}

# PV10 -- PRESENCE. The rule that stops the silence recurring: see "PRESENCE,
# UNIVERSALLY" above. PV9 judges the shape of an answer that was volunteered;
# this judges that an answer exists at all, in every group, with no exemption.
# Deliberately additive -- it reuses nothing of PV9's logic and changes none of
# it, so the two can fail independently and say different things.
rule_pv10 <- function(rec) {
  bad <- character(0)
  ngroups <- 0L
  nanswered <- 0L
  for (fx in fixtures_of(rec)) {
    for (g in groups_of(fx)) {
      ngroups <- ngroups + 1L
      deps <- g[["normative_dependencies"]]
      # NULL covers both an absent key and an explicit JSON null; length 0
      # covers a present-but-empty array. All three are the same silence.
      if (is.null(deps) || !length(deps)) {
        bad <- c(bad, sprintf(paste(
          "%s: no normative_dependencies -- every source group must ANSWER the",
          "source-pinning question, and an absent key is indistinguishable",
          "from an unasked question. FIX: add an entry naming the normative",
          "source this group's expected values are derived from; if nothing",
          "here is derived from a normative source, record ONE entry with",
          "pin_status = \"not-applicable\" -- \"the source is cited but",
          "nothing is derived from it\" -- and a note saying why. See",
          "conventions.normative_dependency_scope, and",
          "inst/bench/wpt-url-cases.json's group for a worked negative",
          "declaration."), group_label(fx, g)))
      } else {
        nanswered <- nanswered + 1L
      }
    }
  }
  # THE FLOOR, IN THE RULE ITSELF. A presence rule that walked no group would
  # report PASS having proved nothing, which is the exact failure mode it
  # exists to close. Refuse to pass vacuously.
  if (!ngroups) {
    bad <- c(bad, paste("PV10 inspected NO source group -- a presence rule",
                        "that walks an empty iteration passes for the wrong",
                        "reason; the record has no groups to answer for"))
  }
  finding("PV10", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else sprintf(paste("all %d source group(s) answer the source-pinning",
                             "question (%d non-empty normative_dependencies)"),
                       ngroups, nanswered))
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
    rest <- lapply(c("PV2", "PV3", "PV4", "PV5", "PV6", "PV7", "PV8", "PV9",
                     "PV10"),
                   function(id) {
                     list(id = id, ok = FALSE,
                          detail = "not evaluated -- record shape is broken")
                   })
    return(c(shape, rest))
  }
  c(shape,
    rule_pv2(root, rec), rule_pv3(root, rec), rule_pv4(root, rec),
    rule_pv5(rec), rule_pv6(rec), rule_pv7(root, rec), rule_pv8(root, rec),
    rule_pv9(rec), rule_pv10(rec))
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

  # Two normative_dependencies entries modelled on the real ip-obfuscation
  # array: one pinned source and one read-but-unpinned source. The second is
  # not decoration -- pin_status = missing is the branch where tracking_issue
  # becomes mandatory and where the sha/date requirements must NOT apply.
  dep_base <- list(
    list(source = "WHATWG URL Standard",
         source_url = "https://url.spec.whatwg.org/",
         immutable_revision = "0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c",
         revision_scheme = "git-commit",
         revision_date = "2026-07-06",
         retrieved_at = "2026-08-01",
         pin_kind = "retrieved-at",
         pin_status = "verified",
         algorithm_anchors = list("concept-host-parser", "concept-ipv4-parser"),
         note = "the expected values are computed by transcribing these"),
    list(source = "UTS #46",
         source_url = "https://www.unicode.org/reports/tr46/",
         immutable_revision = "not pinned",
         revision_scheme = "unpinned",
         revision_date = "not recorded",
         retrieved_at = "not recorded",
         pin_kind = "none",
         pin_status = "missing",
         tracking_issue = "RURL-abc123",
         algorithm_anchors = list("IDNA_Mapping_Table"),
         note = "a second source this derivation reads, and it is not pinned")
  )

  # The shape the git-commit assumption would have made unwritable: a source
  # with no repository, verified against a named edition. Used below to prove
  # the widening is real rather than nominal.
  dep_document <- list(source = "UTS #46",
                       source_url = "https://www.unicode.org/reports/tr46/",
                       immutable_revision = "Unicode 15.1.0",
                       revision_scheme = "document-version",
                       revision_date = "2023-09-12",
                       retrieved_at = "2026-08-01",
                       pin_kind = "verified-at",
                       pin_status = "verified",
                       algorithm_anchors = list("IDNA_Mapping_Table"),
                       note = "the mapping table, pinned to a named edition")

  # The NEGATIVE declaration, modelled on inst/bench/wpt-url-cases.json's
  # group: a group that derives nothing from a standard's text still ANSWERS,
  # through the not-applicable enum member. PV10 has no exemption, so this is
  # the shape that keeps a no-derivation group writable -- if it failed, the
  # rule would be forcing authors to invent pins.
  dep_not_applicable <- list(
    source = "WHATWG URL Standard",
    source_url = "https://url.spec.whatwg.org/",
    immutable_revision = "not applicable",
    revision_scheme = "unpinned",
    revision_date = "not applicable",
    retrieved_at = "not applicable",
    pin_kind = "none",
    pin_status = "not-applicable",
    not_applicable_reason = "no-derivation",
    algorithm_anchors = list("concept-url-parser"),
    note = paste("read out of vendored bytes, not derived from spec text;",
                 "recorded explicitly rather than by omission"))

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
                  # PV10 admits no exemption, so alpha answers too -- and it
                  # answers "not-applicable", which is the boundary the rule
                  # must leave writable.
                  list(retrieval_date = sentinel,
                       normative_dependencies = list(dep_not_applicable)))
    # beta owes SOURCE pinning only -- nothing vendored, expectations derived
    # from two standards -- which is the ip-obfuscation shape. wpt-like owes
    # BOTH duties, which is the wpt-credentials-fragments shape and the case
    # that proves PV9 is not a restatement of PV6: PV6 never looks at an
    # in-scope group.
    out_scope <- list(group = "beta", row_count = sum(src == "beta"),
                      section_2_3_applies = FALSE,
                      out_of_scope_reason = "generated here; nothing imported",
                      normative_dependencies = dep_base)
    json_group <- c(list(group = "wpt-like", row_count = 3L,
                         section_2_3_applies = TRUE),
                    meta_base[setdiff(names(meta_base), "retrieved")],
                    list(retrieval_date = meta_base$retrieved,
                         normative_dependencies = dep_base[1L]))

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
  detail <- function(root, id) {
    for (f in check_oracle_provenance(root, expected_fields = nfields)) {
      if (identical(f$id, id)) return(f$detail)
    }
    NA_character_
  }
  set_group <- function(fixture, group, key, value) {
    function(rec) {
      rec$fixtures[[fixture]]$source_groups[[group]][[key]] <- value
      rec
    }
  }
  # Mutate one key of one normative_dependencies entry. A NULL value deletes
  # the key, which is how the required-key check is falsified.
  set_dep <- function(fixture, group, entry, key, value) {
    function(rec) {
      deps <- rec$fixtures[[fixture]]$source_groups[[group]]
      deps <- deps$normative_dependencies
      deps[[entry]][[key]] <- value
      rec$fixtures[[fixture]]$source_groups[[group]]$normative_dependencies <-
        deps
      rec
    }
  }
  # A rule is only falsified if the gate as a whole goes red for it, so every
  # PV9 negative below asserts the exit verdict too, not just the rule.
  pv9_fails <- function(root) {
    identical(rule(root, "PV9"), FALSE) && identical(verdict(root), FALSE)
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

  # ---- PV9 -------------------------------------------------------------
  #
  # THE FLOOR, FIRST. Every case below is worthless if the positive tree
  # exercises no entries, because a rule that inspects nothing passes for the
  # wrong reason. Assert the population before asserting anything about it.
  expect("PV9's positive case exercises a real population",
         startsWith(detail(mk(), "PV9"),
                    "4 normative-dependency entries in 3 group(s)"))

  # 26. ... and here is what that floor guards against. Strip every array and
  #     PV9 still says PASS: it judges shape, never presence (PV10 owns
  #     presence, which is why this asserts the RULE and not the verdict).
  #     Only the count in the detail distinguishes a vacuous pass from a real
  #     one.
  r <- mk(function(rec) {
    rec$fixtures[[1L]]$source_groups[[1L]]$normative_dependencies <- NULL
    rec$fixtures[[2L]]$source_groups[[1L]]$normative_dependencies <- NULL
    rec$fixtures[[2L]]$source_groups[[2L]]$normative_dependencies <- NULL
    rec
  })
  expect("PV9 passes vacuously when nothing carries the key",
         identical(rule(r, "PV9"), TRUE))
  expect("and the vacuous pass is visible in the count",
         startsWith(detail(r, "PV9"), "0 normative-dependency entries"))

  # 27. Check 1 -- an ARRAY. A single object is the tempting mis-shape,
  #     because one derivation usually does read one source.
  r <- mk(set_group(2L, 2L, "normative_dependencies", dep_base[[1L]]))
  expect("PV9 fails when normative_dependencies is a bare object",
         pv9_fails(r))
  r <- mk(set_group(2L, 2L, "normative_dependencies", list()))
  expect("PV9 fails on an empty normative_dependencies array", pv9_fails(r))
  #     BOUNDARY: an array of one is legal -- the rule is non-empty, not plural.
  r <- mk(set_group(2L, 2L, "normative_dependencies", dep_base[1L]))
  expect("PV9 passes on a single-entry array", identical(verdict(r), TRUE))

  # 28. Check 2 -- the nine required keys.
  r <- mk(set_dep(2L, 2L, 1L, "note", NULL))
  expect("PV9 fails on an entry with no note", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "source_url", NULL))
  expect("PV9 fails on an entry with no source_url", pv9_fails(r))
  #     BOUNDARY: the list is a floor, not a whitelist. The real record carries
  #     an anchor_scope_note on one entry; an extra key must not fail.
  r <- mk(set_dep(2L, 2L, 1L, "anchor_scope_note",
                  "every anchor listed is transcribed"))
  expect("PV9 passes on an entry carrying an extra key",
         identical(verdict(r), TRUE))

  # 29. Check 3 -- pin_status is a CLOSED enum.
  r <- mk(set_dep(2L, 2L, 1L, "pin_status", "pinned"))
  expect("PV9 fails on a pin_status outside the enum", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "pin_status", "Verified"))
  expect("PV9 fails on a mis-cased pin_status", pv9_fails(r))
  #     BOUNDARY: not-applicable is the third member. It now carries a
  #     mandatory reason, so the legal shape is status AND reason together.
  r <- mk(function(rec) {
    rec <- set_dep(2L, 2L, 1L, "pin_status", "not-applicable")(rec)
    set_dep(2L, 2L, 1L, "not_applicable_reason", "no-derivation")(rec)
  })
  expect("PV9 passes on pin_status = not-applicable",
         identical(verdict(r), TRUE))

  # 30. Check 4 -- missing is a FILED gap, so it owes a tracking_issue.
  r <- mk(set_dep(2L, 2L, 2L, "tracking_issue", NULL))
  expect("PV9 fails on pin_status = missing with no tracking_issue",
         pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 2L, "tracking_issue", ""))
  expect("PV9 fails on a blank tracking_issue", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 2L, "tracking_issue", "TBD"))
  expect("PV9 fails on a placeholder tracking_issue", pv9_fails(r))
  #     BOUNDARY: the duty is scoped to `missing`. Entry 1 is verified and
  #     carries no tracking_issue at all, and must stay legal.
  r <- mk(function(rec) {
    rec <- set_dep(2L, 2L, 2L, "pin_status", "not-applicable")(rec)
    rec <- set_dep(2L, 2L, 2L, "not_applicable_reason", "no-derivation")(rec)
    set_dep(2L, 2L, 2L, "tracking_issue", NULL)(rec)
  })
  expect("PV9 requires a tracking_issue only for pin_status = missing",
         identical(verdict(r), TRUE))

  # 31. Check 5 -- verified means a revision anyone can resolve, on a date.
  r <- mk(set_dep(2L, 2L, 1L, "immutable_revision", "0f1e2d3"))
  expect("PV9 fails when a verified pin abbreviates its sha", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "immutable_revision",
                  toupper("0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c")))
  expect("PV9 fails on an uppercase sha", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "revision_date", "2026-07"))
  expect("PV9 fails on a truncated revision_date", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "retrieved_at", "2026-13-45"))
  expect("PV9 fails on a date-shaped string that is not a date", pv9_fails(r))
  #     BOUNDARY: an UNPINNED source legitimately records prose in exactly
  #     those three fields. Entry 2 does, and the clean positive proves it, but
  #     assert it directly -- a sha rule applied to every status would make
  #     pin_status = missing unwritable.
  r <- mk(function(rec) {
    rec <- set_dep(2L, 2L, 1L, "pin_status", "missing")(rec)
    rec <- set_dep(2L, 2L, 1L, "tracking_issue", "RURL-abc123")(rec)
    rec <- set_dep(2L, 2L, 1L, "revision_scheme", "unpinned")(rec)
    rec <- set_dep(2L, 2L, 1L, "immutable_revision", "not pinned")(rec)
    rec <- set_dep(2L, 2L, 1L, "revision_date", "not recorded")(rec)
    set_dep(2L, 2L, 1L, "retrieved_at", "not recorded")(rec)
  })
  expect("PV9 demands a sha and ISO dates only of a verified pin",
         identical(verdict(r), TRUE))

  # 32. Check 6 -- anchors are the durable key, so there must be one.
  r <- mk(set_dep(2L, 2L, 1L, "algorithm_anchors", list()))
  expect("PV9 fails on an empty algorithm_anchors", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "algorithm_anchors", list("")))
  expect("PV9 fails on a blank algorithm anchor", pv9_fails(r))
  #     BOUNDARY: one anchor is enough; the rule is non-empty, not exhaustive.
  r <- mk(set_dep(2L, 2L, 1L, "algorithm_anchors", list("concept-host-parser")))
  expect("PV9 passes on a single algorithm anchor", identical(verdict(r), TRUE))

  # 33. Check 7 -- the sentinel is reserved for an absent section-2.3 SCALAR;
  #     an unpinned source uses the enum. Asserted on the IN-SCOPE group,
  #     because PV6 skips those entirely: this is PV9's own catch, not PV6's.
  r <- mk(set_dep(1L, 1L, 1L, "note", paste("the revision is", sentinel)))
  expect("PV9 fails on a sentinel inside an in-scope group's entry",
         pv9_fails(r))
  expect("PV6 never sees it -- an in-scope group is not its subject",
         identical(rule(r, "PV6"), TRUE))
  #     BOUNDARY: prose that merely NAMES the key is not the sentinel itself.
  r <- mk(set_dep(2L, 2L, 2L, "note",
                  paste("unpinned; recorded through pin_status rather than",
                        "the MISSING marker PV6 scans for")))
  expect("PV9 passes on a note that discusses pinning without a sentinel",
         identical(verdict(r), TRUE))

  # 34. Check 8 -- revision_scheme is the SECOND closed enum, and it is
  #     required of every entry, because every pin_status constrains it.
  r <- mk(set_dep(2L, 2L, 1L, "revision_scheme", "svn-rev"))
  expect("PV9 fails on a revision_scheme outside the enum", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 1L, "revision_scheme", NULL))
  expect("PV9 fails on an entry with no revision_scheme", pv9_fails(r))

  # 35. Check 8 -- the cross-field rules. "unpinned" claims nothing, so it
  #     cannot sit under a status that claims something; and a filed gap that
  #     names a scheme is describing a pin it does not have.
  r <- mk(set_dep(2L, 2L, 1L, "revision_scheme", "unpinned"))
  expect("PV9 fails on pin_status = verified with revision_scheme = unpinned",
         pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 2L, "revision_scheme", "git-commit"))
  expect("PV9 fails on pin_status = missing without revision_scheme = unpinned",
         pv9_fails(r))
  #     BOUNDARY: not-applicable is the other status that may be unpinned.
  r <- mk(function(rec) {
    rec <- set_dep(2L, 2L, 2L, "pin_status", "not-applicable")(rec)
    set_dep(2L, 2L, 2L, "not_applicable_reason", "no-derivation")(rec)
  })
  expect("PV9 passes on not-applicable with revision_scheme = unpinned",
         identical(verdict(r), TRUE))

  # 36. Check 8 -- document-version rejects the moving targets that actually
  #     get written. It cannot prove immutability and does not claim to.
  for (moving in c("latest", "HEAD", "Living Standard", "unversioned",
                   "current", "")) {
    r <- mk(function(rec) {
      rec <- set_dep(2L, 2L, 1L, "revision_scheme", "document-version")(rec)
      set_dep(2L, 2L, 1L, "immutable_revision", moving)(rec)
    })
    expect(sprintf("PV9 fails a document-version pinned to \"%s\"", moving),
           pv9_fails(r))
  }

  # 37. THE WIDENING, PROVED. A source with no repository, verified against a
  #     named edition, must go GREEN -- this is the shape the first cut of
  #     check 5 wrongly rejected, and it is how RURL-qhwktfcw's open UTS-46
  #     half will eventually be closed.
  r <- mk(set_group(2L, 2L, "normative_dependencies",
                    list(dep_base[[1L]], dep_document)))
  expect("PV9 passes a document-version pin to a named edition",
         identical(verdict(r), TRUE))

  #     ... and the widening is REAL, not laxity. The pre-change rule was
  #     is_commit_sha() applied to EVERY verified pin; that predicate still
  #     exists and still says no to this revision, so the same string under
  #     the git-commit scheme stays red. The check followed the scheme; it was
  #     not relaxed for everyone.
  r <- mk(set_group(2L, 2L, "normative_dependencies", list(
    dep_base[[1L]],
    utils::modifyList(dep_document, list(revision_scheme = "git-commit")))))
  expect("the same revision under git-commit is still rejected", pv9_fails(r))
  expect("which is what makes 'Unicode 15.1.0' a widening, not a loophole",
         identical(is_commit_sha("Unicode 15.1.0"), FALSE) &&
           identical(is_named_edition("Unicode 15.1.0"), TRUE))

  # ---- PV10 ------------------------------------------------------------
  #
  # Same discipline as PV9: a presence rule is the easiest of all to pass for
  # nothing, so the population comes first.
  pv10_fails <- function(root) {
    identical(rule(root, "PV10"), FALSE) && identical(verdict(root), FALSE)
  }

  # 38. THE FLOOR. Assert PV10 actually walked every group of the positive
  #     tree -- 3 of them -- before believing anything it says about them.
  expect("PV10's positive case walks every group",
         startsWith(detail(mk(), "PV10"),
                    "all 3 source group(s) answer the source-pinning question"))

  # 39. FALSIFIED, three ways -- once per class of group, because "no
  #     exemption" is the whole content of the rule. An in-scope CSV group, an
  #     out-of-scope CSV group and the JSON oracle's group each go red on
  #     their own when the key is simply absent.
  r <- mk(set_group(2L, 1L, "normative_dependencies", NULL))
  expect("PV10 fails when an IN-SCOPE group omits the key", pv10_fails(r))
  expect("PV9 never sees the omission -- it judges shape, not presence",
         identical(rule(r, "PV9"), TRUE))
  r <- mk(set_group(2L, 2L, "normative_dependencies", NULL))
  expect("PV10 fails when an OUT-OF-SCOPE group omits the key", pv10_fails(r))
  expect("PV6 passes it too -- being out of section 2.3 is not an exemption",
         identical(rule(r, "PV6"), TRUE))
  r <- mk(set_group(1L, 1L, "normative_dependencies", NULL))
  expect("PV10 fails when the JSON oracle's group omits the key",
         pv10_fails(r))

  # 40. FALSIFIED, fourth way -- present but EMPTY. An empty array answers
  #     nothing; it is silence with a key in front of it.
  r <- mk(set_group(2L, 1L, "normative_dependencies", list()))
  expect("PV10 fails on an empty normative_dependencies array", pv10_fails(r))

  # 41. FALSIFIED, fifth way -- present but NULL. The record's shape allows it
  #     (jsonlite serialises NA to JSON null and parses it back to a present
  #     key holding NULL), and PV9 SKIPS it exactly as it skips an absent key,
  #     so PV10 is the only rule standing between this and a green gate.
  r <- mk(set_group(2L, 1L, "normative_dependencies", NA))
  expect("PV10 fails on a null normative_dependencies", pv10_fails(r))
  expect("PV10 is the only rule that catches a null answer",
         identical(rule(r, "PV9"), TRUE) && identical(rule(r, "PV5"), TRUE))

  # 42. BOUNDARY. The unmodified positive already passes (case 1), and it
  #     passes with alpha answering "not-applicable" -- but assert the shape
  #     directly, on a group whose ONLY entry is a negative declaration. If
  #     this went red the rule would be demanding invented pins rather than
  #     answers, which is the failure mode that would get PV10 deleted.
  r <- mk(set_group(2L, 2L, "normative_dependencies",
                    list(dep_not_applicable)))
  expect("PV10 passes a group whose only entry is not-applicable",
         identical(verdict(r), TRUE))
  expect("and the not-applicable answer counts as an answer",
         startsWith(detail(r, "PV10"), "all 3 source group(s) answer"))

  # 43. THE FLOOR, FALSIFIED IN THE RULE ITSELF. A presence rule that inspects
  #     ZERO groups must not report PASS. Called directly, because a record
  #     with no groups trips PV1 first and would otherwise "fail" for a reason
  #     that says nothing about PV10.
  expect("PV10 refuses to pass over an empty iteration",
         identical(rule_pv10(list(fixtures = list()))[[1L]]$ok, FALSE))
  expect("PV10 refuses to pass over fixtures that hold no groups",
         identical(rule_pv10(list(fixtures = list(list(path = "x.csv"))))[[
           1L]]$ok, FALSE))

  # 44. Check 9 (RURL-ynirvjxb) -- not_applicable_reason, the THIRD enum and
  #     the reason axis. The status member was carrying two questions; these
  #     cases pin the split down. dep_not_applicable is the subject throughout:
  #     alpha's only entry, which is fixtures[2].source_groups[1][1].
  #
  #     FALSIFIED, five ways.
  r <- mk(set_dep(2L, 1L, 1L, "not_applicable_reason", NULL))
  expect("PV9 fails on not-applicable with no reason", pv9_fails(r))
  r <- mk(set_dep(2L, 1L, 1L, "not_applicable_reason", "no derivation"))
  expect("PV9 fails on a reason outside the enum", pv9_fails(r))
  r <- mk(set_dep(2L, 1L, 1L, "not_applicable_reason", "No-Derivation"))
  expect("PV9 fails on a mis-cased reason", pv9_fails(r))
  r <- mk(set_dep(2L, 1L, 1L, "not_applicable_reason", ""))
  expect("PV9 fails on a blank reason", pv9_fails(r))
  #     ... and a reason that is an ARRAY, not a scalar. one_string() is why
  #     this fails rather than being coerced to its first element.
  r <- mk(set_dep(2L, 1L, 1L, "not_applicable_reason",
                  list("no-derivation", "frozen-source")))
  expect("PV9 fails on a reason holding an array", pv9_fails(r))

  # 45. Check 9 -- FORBIDDEN where the duty is not excused, which is the half
  #     a "required on not-applicable" rule alone would miss. A verified pin
  #     that also carries an excuse is a copy-paste, and it is exactly how a
  #     real pin would come to read as an exemption.
  r <- mk(set_dep(2L, 2L, 1L, "not_applicable_reason", "no-derivation"))
  expect("PV9 fails on a verified entry carrying a reason", pv9_fails(r))
  r <- mk(set_dep(2L, 2L, 2L, "not_applicable_reason", "frozen-source"))
  expect("PV9 fails on a missing entry carrying a reason", pv9_fails(r))

  # 46. Check 9 -- BOUNDARY, once per member, because a rule that accepted only
  #     the member the record happens to use most would still pass every case
  #     above. All three are load-bearing in the real record: no-derivation on
  #     six entries, frozen-source on RFC 3986, internal-source on PRD 6.1.
  for (reason in c("no-derivation", "frozen-source", "internal-source")) {
    r <- mk(set_dep(2L, 1L, 1L, "not_applicable_reason", reason))
    expect(sprintf("PV9 passes on not_applicable_reason = \"%s\"", reason),
           identical(verdict(r), TRUE))
  }
  #     BOUNDARY: the two statuses that owe NO reason must stay writable with
  #     the key absent. The clean positive covers it, but assert it directly --
  #     a rule that required the key of every entry would make a pin unwritable.
  expect("PV9 leaves verified and missing entries free of the key",
         identical(verdict(mk()), TRUE) &&
           !"not_applicable_reason" %in% names(dep_base[[1L]]) &&
           !"not_applicable_reason" %in% names(dep_base[[2L]]))

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
