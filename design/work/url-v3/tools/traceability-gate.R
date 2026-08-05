#!/usr/bin/env Rscript
# Claim-traceability gate (§7 G4 criterion 1; §6 artifact 11 traceability map;
# RCON-10 / S9 H6).
#
# Criterion 1 reads: "Every normative claim maps to tests, fixtures,
# benchmarks, or a documented manual verification with exact commands."
# S9 H6 says a citation is not enough on its own and asks for "automated
# orphan/reverse-coverage checks". This gate is that check.
#
# THE POPULATION RULE. A normative claim is a data row of a pipe table in a
# §6 contract whose LAST cell begins with `SETTLED` or `OPEN`. That is the
# status column the contract family already uses, so the population is read
# off the contracts themselves rather than transcribed. Envelope, Inputs,
# legend, and prose tables carry no status column and are therefore not
# claims. Nothing is hand-listed, so a claim cannot be dropped by omission:
# add a contract row and it appears here on the next run.
#
# WHY THE INDEX IS GENERATED, NOT WRITTEN. A hand-copied index of 479 rows is
# a second copy of the contracts that starts drifting the moment either side
# is edited, and the drift is silent. The `## Claim index` and
# `## Coverage census` blocks are regenerated from the contracts and compared
# byte-for-byte (T3), so the map cannot disagree with its own sources. The
# hand-authored surface is deliberately small: which slice owns which contract
# SECTION (67 rows), the CLAIMS that dissent from their section (the override
# table), the slice registry, the discharge-record registry, and the
# excluded-source dispositions.
#
# OWNERSHIP GRANULARITY (P0.8 D-D, RURL-sbhpzwzk). Ownership is assigned per
# section, and some sections cannot be expressed that way: `SS s5` states
# special-ness, default port, host/PSL eligibility and semantic-transform
# eligibility in four columns of one table, and the map's own precedents send
# those to four different slices, so no single owner for the section is right
# for all nine of its claims. The index already carries a per-claim
# `owning_slice`, but it was a PROJECTION of the section row and therefore not
# assignable. `## Claim ownership overrides` is the dissent list -- one row per
# claim whose owner differs from its section's -- and the derivation resolves
# every claim as OVERRIDE IF LISTED, SECTION OWNER OTHERWISE. The override is
# applied here rather than transcribed into the generated block, which is the
# whole point: T3 compares the block against this derivation, so a table the
# derivation ignored would still pass byte-for-byte. That is why T9 exists and
# why the self-test asserts the moved value in the generated index and census,
# not merely that the table parses.
#
# WHAT THIS GATE DOES NOT CLAIM. `coverage = MAPPED` means the verification
# slice that owns the claim's section is on disk -- it does NOT mean that
# slice cites this individual claim. Per-claim citation needs the slices to
# quote `TR-*` ids, which the six owed slices can do from the start and the
# three shipped ones predate. T7 is the partial substitute available today:
# a shipped owning slice must at least cite the contract it owns sections of.
# The record says this in `## Open cells`; G4-acceptance must report
# criterion 1 as partially met, not met.
#
# Rules:
#   T0  structure   -- required sections exist; the generated blocks are
#                      present and their BEGIN/END markers pair up.
#   T1  totality    -- derived claim-bearing sections and `## Section
#                      ownership` rows are an exact bijection. A new contract
#                      section is an ORPHAN until it is assigned; a stale
#                      ownership row is a PHANTOM. This is S9 H6's
#                      orphan/reverse-coverage check, both directions.
#   T2  vocabulary  -- every owner is a registered slice id, the literal
#                      UNASSIGNED, or `UNASSIGNED[subtype]`; carrier named iff
#                      UNASSIGNED. Invented slice names FAIL, and so does any
#                      other qualifier spelling -- the accepted set is exactly
#                      what the coverage census can group, so a row cannot be
#                      admitted here and then vanish from the tally. Applies to
#                      the SECTION rows and the claim OVERRIDE rows alike: two
#                      vocabularies over one census is the drift T2 is for.
#   T3  fidelity    -- the committed generated blocks equal a fresh
#                      derivation, byte for byte (index AND census; the
#                      census is a tally, and an unrecomputed tally is just
#                      another unchecked claim -- oracle-label-gate O6).
#   T4  no phantoms -- every repo path the record cites in a code span exists
#                      on disk. Keeps forward references to unmerged work out.
#   T5  slices      -- the `## Verification slices` table matches the registry
#                      below AND disk: SHIPPED <=> the file exists, OWED <=>
#                      it does not. Authoring a slice without re-owning its
#                      sections here FAILS.
#   T6  exclusions  -- every contract contributing zero claims is listed in
#                      `## Excluded sources` with a reason, and every listed
#                      file really does contribute zero. A contract cannot
#                      leave the population silently.
#   T7  citation    -- a contract with at least one CLAIM owned by a SHIPPED
#                      slice must be cited by path in that slice. Read off the
#                      resolved per-claim owners, so an override cannot grant a
#                      shipped slice coverage of a contract it never names.
#   T8  discharges  -- `## Discharge records` accounts for every verification
#                      record that is not a registered slice, in BOTH
#                      directions, and each listed record really claims its
#                      deferral (`DISCHARGED[VD-nnn]`) and names the contract
#                      whose cells it maps. The verification directory
#                      therefore partitions into: this map, the registered
#                      slices, and the registered discharge records. Nothing
#                      else may sit there unaccounted.
#   T9  overrides    -- every `## Claim ownership overrides` row names a claim
#                      the population actually contains, exactly once, with a
#                      reason, and DISSENTS from its section's owner. A
#                      redundant override is a no-op today and silent drift
#                      tomorrow: change the section row and the claim stops
#                      moving with its family for a reason nobody recorded.
#
# WHY T8 EXISTS, AND WHY IT IS HALF OF A PAIR (P0.7 D-E, RURL-ogktvhgp).
# `tools/deferral-gate.R` rule D2 requires a DISCHARGED deferral to be claimed
# by a "verification slice", and it used to accept ANY `verification/*.md` as
# one -- so a discharge could be claimed by a file that claims nothing, and
# three of the four discharges on disk were in fact claimed by records this map
# had never heard of. Both gates passed while contradicting each other about
# the same file. D2 now reads the claimant registry out of THIS record (the
# `## Verification slices` and `## Discharge records` tables), so there is one
# definition of an admissible claimant and two consumers of it. T5 and T8 are
# what keep that definition honest against disk; without them the narrowing
# would just move the blind spot into this file.
#
# WHY THIS LIVES UNDER design/work/url-v3/tools/. Control-plane gate over a
# control-plane record, beside ci-gate.R and oracle-label-gate.R; `^design$`
# is already in .Rbuildignore, so it needs no build-ignore line of its own.
#
# Base R only. No network, no package deps. Usage:
#   Rscript design/work/url-v3/tools/traceability-gate.R
#   Rscript design/work/url-v3/tools/traceability-gate.R --self-test
#   Rscript design/work/url-v3/tools/traceability-gate.R --regenerate

# ---- constants --------------------------------------------------------------

CONTRACT_DIR <- file.path("design", "work", "url-v3", "contracts")

MAP_REL <- file.path(
  "design", "work", "url-v3", "verification", "traceability-map.md"
)

VERIFICATION_DIR <- file.path("design", "work", "url-v3", "verification")

# Stable short prefixes for claim ids. Keyed by contract file basename so a
# renamed contract fails loudly here instead of silently renumbering ids.
CONTRACT_ABBREV <- c(
  "canonical-state-contract.md"         = "CS",
  "cleaning-mutation-contracts.md"      = "CM",
  "cross-artifact-consistency.md"       = "CA",
  "host-annotation-contracts.md"        = "HA",
  "key-join-contracts.md"               = "KJ",
  "output-contracts.md"                 = "OUT",
  "public-surface-closure.md"           = "PS",
  # Artifact 4's roster half (P0.6). Its rows are normative claims exactly as
  # before the split — they merely live in a second file now, so they stay in the
  # claim population under their own prefix rather than silently leaving it.
  "public-surface-disposition.md"       = "PSD",
  "semantic-cache-contract.md"          = "SC",
  "standard-scheme-matrices.md"         = "SS",
  "validation-intervention-contract.md" = "VI"
)

# The verification slices that may own a contract section. The eight §7 G4
# criterion-3 property families, plus the release slice (C-10/P0.4) and the
# host slice that cache-slice.md already names as a boundary ("owned by G3.H
# and its future G4 verification slice"). Adding a name here is adding scope,
# which is why T2 refuses anything not on this list.
SLICE_REGISTRY <- c(
  "cache-slice", "determinism-slice", "release-slice",
  "state-slice", "full-string-slice", "vector-slice",
  "mutation-slice", "join-slice", "migration-slice", "host-slice"
)

UNASSIGNED <- "UNASSIGNED"

# The ONLY two owner spellings the census grouper can fold back into a row:
# the bare literal and the bracket-qualified form `UNASSIGNED[subtype]`. Any
# other qualifier -- a parenthesized one, most plausibly -- used to satisfy T2's
# vocabulary rule and resolve in the claim index while matching no census group,
# so its claims silently left the by-slice tally and the census stopped summing
# to its own total (RURL-fymdhizq). T3 structurally cannot catch that: the
# census is generated, so it agrees byte-for-byte with the generator and both
# are wrong together. T2, resolve_coverage() and generate_census() therefore
# share ONE predicate, and T2 fails closed on anything it cannot group.
UNASSIGNED_RE <- "^UNASSIGNED(\\[[^][]+\\])?$"

is_unassigned_owner <- function(owner) grepl(UNASSIGNED_RE, owner)

# The census row an owner is tallied under: the bracket qualifier is a subtype
# of its parent, so it folds back. Every other owner is its own group.
owner_group <- function(owner) sub("\\[[^][]*\\]$", "", owner)

OWNERSHIP_FIELDS <- c("contract", "sec", "section", "owning_slice", "carrier")

# The dissent list. A row here overrides the section-derived owner for ONE
# claim; `reason` is required because the row exists precisely where the
# section rule is wrong, and the next reader needs to know which property
# precedent moved it.
OVERRIDE_FIELDS <- c("claim_id", "owning_slice", "carrier", "reason")

SLICE_FIELDS <- c("slice_id", "tracked_path", "state")

# A discharge record is NOT a slice and must never be added to SLICE_REGISTRY:
# it maps one deferral's cells onto shipped evidence and says so in its own
# envelope, where a slice owns a property family's whole surface. Registering
# it here is what makes it an admissible D2 claimant without granting it
# section ownership, which stays with the registry above.
DISCHARGE_FIELDS <- c("record_id", "deferral_id", "tracked_path", "contract",
                      "scope")

DEFERRAL_ID_RE <- "^VD-[0-9]+$"

EXCLUSION_FIELDS <- c("path", "claims", "reason")

INDEX_FIELDS <- c(
  "claim_id", "status", "owning_slice", "coverage", "source"
)

REQUIRED_SECTIONS <- c(
  "Envelope", "Purpose", "Inputs", "Population rule", "Verification slices",
  "Discharge records", "Section ownership", "Claim ownership overrides",
  "Claim index", "Coverage census", "Excluded sources", "Scope boundaries",
  "Open cells"
)

GENERATED_BLOCKS <- c("claim-index", "coverage-census")

CARRIER_RE <- "^RURL-[a-z0-9]{8}$"

# ---- markdown helpers -------------------------------------------------------

split_row <- function(x) {
  x <- sub("^\\|", "", x)
  x <- sub("\\|\\s*$", "", x)
  trimws(strsplit(x, "|", fixed = TRUE)[[1]])
}

is_separator <- function(x) grepl("^\\|[\\s:|-]*$", x, perl = TRUE)

is_table_row <- function(x) grepl("^\\|", x) & grepl("\\|\\s*$", x)

# Data rows of the pipe table(s) under a `## <heading>` line, stopping at the
# next `##`. Header and separator dropped. A cell holding a literal `|` splits
# into extra fields; that is not worked around, it surfaces as a field-count
# mismatch and fails closed.
table_rows <- function(lines, heading) {
  start <- which(trimws(lines) == paste0("## ", heading))
  if (length(start) != 1L) return(NULL)
  rest <- lines[seq.int(start[1] + 1L, length(lines))]
  stop_at <- which(grepl("^## ", rest))
  if (length(stop_at)) rest <- rest[seq_len(stop_at[1] - 1L)]
  rest <- trimws(rest)
  rest <- rest[is_table_row(rest)]
  rest <- rest[!is_separator(rest)]
  if (length(rest) < 2L) return(list())
  lapply(rest[-1], split_row)
}

as_frame <- function(rows, fields) {
  if (!length(rows)) {
    out <- as.data.frame(
      matrix(character(0), ncol = length(fields)), stringsAsFactors = FALSE
    )
    names(out) <- fields
    out$.width_ok <- logical(0)
    return(out)
  }
  widths <- vapply(rows, length, integer(1))
  pad <- function(r) {
    if (length(r) >= length(fields)) return(r[seq_along(fields)])
    c(r, rep(NA_character_, length(fields) - length(r)))
  }
  out <- as.data.frame(
    do.call(rbind, lapply(rows, pad)), stringsAsFactors = FALSE
  )
  names(out) <- fields
  out$.width_ok <- widths == length(fields)
  out
}

blank <- function(x) is.na(x) | !nzchar(trimws(x))

md_table <- function(fields, rows) {
  c(
    paste0("| ", paste(fields, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(fields)), collapse = "|"), "|"),
    rows
  )
}

md_row <- function(...) paste0("| ", paste(c(...), collapse = " | "), " |")

# ---- claim derivation -------------------------------------------------------

slugify <- function(x) {
  x <- gsub("`", "", x, fixed = TRUE)
  x <- gsub("\\*\\*", "", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  substr(x, 1L, 32L)
}

# Every claim in one contract, in document order.
#
# `sec` is the ordinal of the CLAIM-BEARING section within the file, not of
# every `##` heading: prose sections come and go, and numbering off them would
# renumber ids for an edit that touched no claim.
claims_in_file <- function(path, abbrev) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  section <- NA_character_
  seen <- character(0)
  out <- list()
  for (i in seq_along(lines)) {
    ln <- trimws(lines[i])
    if (grepl("^## ", ln)) section <- sub("^## ", "", ln)
    if (!is_table_row(ln) || is_separator(ln)) next
    cells <- split_row(ln)
    if (!length(cells)) next
    status <- cells[length(cells)]
    if (!grepl("^(SETTLED|OPEN)\\b", status)) next
    if (!section %in% seen) seen <- c(seen, section)
    out[[length(out) + 1L]] <- data.frame(
      contract = abbrev,
      sec = paste0("s", match(section, seen)),
      section = section,
      key = cells[1],
      status = if (grepl("^SETTLED", status)) "SETTLED" else "OPEN",
      status_full = status,
      line = i,
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) {
    return(data.frame(
      contract = character(0), sec = character(0), section = character(0),
      key = character(0), status = character(0), status_full = character(0),
      line = integer(0), claim_id = character(0), stringsAsFactors = FALSE
    ))
  }
  d <- do.call(rbind, out)
  base <- paste0("TR-", d$contract, "-", d$sec, "-", slugify(d$key))
  # Deterministic disambiguation for repeated keys inside one section (the
  # port truth table lists `HTTP absent port` under two profiles).
  d$claim_id <- base
  dupe <- base %in% base[duplicated(base)]
  if (any(dupe)) {
    for (b in unique(base[dupe])) {
      at <- which(base == b)
      d$claim_id[at] <- paste0(b, c("", paste0("-", seq_along(at)[-1] + 0L)))
    }
  }
  d
}

derive_claims <- function(root) {
  files <- sort(list.files(
    file.path(root, CONTRACT_DIR), pattern = "[.]md$", full.names = TRUE
  ))
  parts <- lapply(files, function(f) {
    ab <- CONTRACT_ABBREV[basename(f)]
    if (is.na(ab)) {
      stop("contract not in CONTRACT_ABBREV: ", basename(f), call. = FALSE)
    }
    claims_in_file(f, unname(ab))
  })
  do.call(rbind, parts)
}

contract_paths <- function(root) {
  files <- sort(list.files(
    file.path(root, CONTRACT_DIR), pattern = "[.]md$", full.names = FALSE
  ))
  stats::setNames(file.path(CONTRACT_DIR, files), CONTRACT_ABBREV[files])
}

# ---- map reading ------------------------------------------------------------

read_map <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  list(
    lines = lines,
    ownership = as_frame(
      table_rows(lines, "Section ownership"), OWNERSHIP_FIELDS
    ),
    overrides = as_frame(
      table_rows(lines, "Claim ownership overrides"), OVERRIDE_FIELDS
    ),
    slices = as_frame(table_rows(lines, "Verification slices"), SLICE_FIELDS),
    discharges = as_frame(
      table_rows(lines, "Discharge records"), DISCHARGE_FIELDS
    ),
    exclusions = as_frame(
      table_rows(lines, "Excluded sources"), EXCLUSION_FIELDS
    )
  )
}

block_bounds <- function(lines, name) {
  b <- grep(paste0("<!-- BEGIN GENERATED: ", name), lines, fixed = FALSE)
  e <- grep(paste0("<!-- END GENERATED: ", name, " -->"), lines, fixed = TRUE)
  if (length(b) != 1L || length(e) != 1L || e <= b) return(NULL)
  c(b, e)
}

block_content <- function(lines, name) {
  at <- block_bounds(lines, name)
  if (is.null(at)) return(NULL)
  if (at[2] - at[1] <= 1L) return(character(0))
  lines[seq.int(at[1] + 1L, at[2] - 1L)]
}

# Repo paths cited in code spans. A path is recognised by a leading known
# top-level directory, so prose in backticks is not mistaken for a file.
paths_in_record <- function(lines) {
  spans <- unlist(
    regmatches(lines, gregexpr("`[^`]+`", lines)), use.names = FALSE
  )
  spans <- trimws(gsub("`", "", spans, fixed = TRUE))
  keep <- grepl("^(tests|inst|tools|analysis|design|R|man|\\.github)/[^ ]+$",
                spans)
  sub(":[0-9]+(-[0-9]+)?$", "", unique(spans[keep]))
}

# ---- generation -------------------------------------------------------------

# The section rule: a claim's default owner is the owner of the section it
# lives in. `ORPHAN` when no ownership row matches it, which T1 reports.
section_owner <- function(claims, ownership) {
  key <- paste(claims$contract, claims$section, sep = "")
  okey <- paste(ownership$contract, ownership$section, sep = "")
  owner <- ownership$owning_slice[match(key, okey)]
  owner[is.na(owner)] <- "ORPHAN"
  owner
}

# The dissent rule, applied ON TOP of the section rule: override if listed,
# section owner otherwise. A row naming a claim outside the population is
# ignored here and FAILED by T9 -- resolution must not depend on the map being
# well formed, or a malformed row would move a claim it does not name.
apply_overrides <- function(owner, claims, overrides) {
  if (!nrow(overrides)) return(owner)
  at <- match(trimws(overrides$claim_id), claims$claim_id)
  hit <- !is.na(at)
  owner[at[hit]] <- trimws(overrides$owning_slice[hit])
  owner
}

# Coverage is DERIVED, never asserted: a claim is MAPPED only when the slice
# owning it is actually on disk. That is what stops the map from claiming
# coverage that does not exist yet.
resolve_coverage <- function(claims, map, root) {
  owner <- apply_overrides(
    section_owner(claims, map$ownership), claims, map$overrides
  )
  shipped <- vapply(SLICE_REGISTRY, function(s) {
    file.exists(file.path(root, VERIFICATION_DIR, paste0(s, ".md")))
  }, logical(1))
  cov <- ifelse(
    owner == "ORPHAN", "ORPHAN",
    ifelse(is_unassigned_owner(owner), UNASSIGNED,
           ifelse(owner %in% SLICE_REGISTRY[shipped], "MAPPED", "PENDING"))
  )
  list(owner = owner, coverage = cov)
}

generate_index <- function(claims, map, root, paths) {
  r <- resolve_coverage(claims, map, root)
  src <- paste0(paths[claims$contract], ":", claims$line)
  md_table(INDEX_FIELDS, vapply(seq_len(nrow(claims)), function(i) {
    md_row(claims$claim_id[i], claims$status[i], r$owner[i], r$coverage[i],
           paste0("`", src[i], "`"))
  }, character(1)))
}

generate_census <- function(claims, map, root, paths) {
  r <- resolve_coverage(claims, map, root)
  tally <- function(sel) {
    c(sum(sel), sum(sel & claims$status == "SETTLED"),
      sum(sel & claims$status == "OPEN"))
  }
  group <- owner_group(r$owner)
  owners <- c(SLICE_REGISTRY, UNASSIGNED, "ORPHAN")
  owners <- owners[vapply(owners, function(o) any(group == o), logical(1))]
  by_owner <- vapply(owners, function(o) {
    sel <- group == o
    t <- tally(sel)
    md_row(o, unique(r$coverage[sel])[1], t[1], t[2], t[3])
  }, character(1), USE.NAMES = FALSE)
  tot <- tally(rep(TRUE, nrow(claims)))

  # A tally that does not sum to its own total is the RURL-fymdhizq failure:
  # some owner spelling matched no group and its claims left the census
  # silently. T3 cannot see it -- it compares the generated block against this
  # same generator -- so the generator refuses to emit an unsound census.
  ungrouped <- setdiff(unique(group), owners)
  if (length(ungrouped)) {
    stop("census would drop ", sum(!group %in% owners), " of ", tot[1],
         " claims: owner group(s) in no census row: ",
         paste(ungrouped, collapse = ", "), call. = FALSE)
  }
  by_owner <- c(by_owner, md_row("**total**", "—", tot[1], tot[2], tot[3]))

  abbrevs <- names(paths)
  abbrevs <- abbrevs[abbrevs %in% claims$contract]
  by_contract <- vapply(abbrevs, function(a) {
    sel <- claims$contract == a
    t <- tally(sel)
    md_row(a, paste0("`", paths[[a]], "`"), t[1], t[2], t[3],
           length(unique(claims$section[sel])))
  }, character(1), USE.NAMES = FALSE)

  c(
    "### By owning slice",
    "",
    md_table(c("owning_slice", "coverage", "claims", "SETTLED", "OPEN"),
             by_owner),
    "",
    "### By contract",
    "",
    md_table(c("contract", "path", "claims", "SETTLED", "OPEN", "sections"),
             by_contract)
  )
}

generate_blocks <- function(root, map) {
  claims <- derive_claims(root)
  paths <- contract_paths(root)
  list(
    "claim-index" = generate_index(claims, map, root, paths),
    "coverage-census" = generate_census(claims, map, root, paths)
  )
}

regenerate <- function(root) {
  path <- file.path(root, MAP_REL)
  map <- read_map(path)
  blocks <- generate_blocks(root, map)
  lines <- map$lines
  for (name in GENERATED_BLOCKS) {
    at <- block_bounds(lines, name)
    if (is.null(at)) {
      stop("missing generated block: ", name, call. = FALSE)
    }
    lines <- c(
      lines[seq_len(at[1])], blocks[[name]],
      lines[seq.int(at[2], length(lines))]
    )
  }
  writeLines(lines, path, useBytes = TRUE)
  invisible(TRUE)
}

# ---- rules ------------------------------------------------------------------

check <- function(id, ok, detail) list(id = id, ok = isTRUE(ok), detail = detail)

evaluate <- function(map, root) {
  res <- list()
  claims <- derive_claims(root)
  paths <- contract_paths(root)
  own <- map$ownership

  # T0 -- structure.
  missing_sec <- REQUIRED_SECTIONS[
    !vapply(REQUIRED_SECTIONS, function(s) {
      any(trimws(map$lines) == paste0("## ", s))
    }, logical(1))
  ]
  bad_block <- GENERATED_BLOCKS[
    vapply(GENERATED_BLOCKS, function(b) {
      is.null(block_bounds(map$lines, b))
    }, logical(1))
  ]
  width_bad <- sum(!own$.width_ok) + sum(!map$slices$.width_ok) +
    sum(!map$discharges$.width_ok) + sum(!map$exclusions$.width_ok) +
    sum(!map$overrides$.width_ok)
  res[[length(res) + 1L]] <- check(
    "T0",
    !length(missing_sec) && !length(bad_block) && width_bad == 0L,
    if (length(missing_sec)) {
      paste("missing section(s):", paste(missing_sec, collapse = ", "))
    } else if (length(bad_block)) {
      paste("malformed generated block(s):", paste(bad_block, collapse = ", "))
    } else if (width_bad) {
      paste(width_bad, "table row(s) with wrong field count")
    } else {
      sprintf("%d sections, %d generated blocks, tables well formed",
              length(REQUIRED_SECTIONS), length(GENERATED_BLOCKS))
    }
  )

  # T1 -- orphan / reverse coverage, both directions.
  derived <- unique(paste(claims$contract, claims$section, sep = " :: "))
  declared <- paste(own$contract, own$section, sep = " :: ")
  orphans <- setdiff(derived, declared)
  phantoms <- setdiff(declared, derived)
  dupes <- unique(declared[duplicated(declared)])
  res[[length(res) + 1L]] <- check(
    "T1",
    !length(orphans) && !length(phantoms) && !length(dupes),
    if (length(orphans)) {
      paste("ORPHAN section(s) with no owner:",
            paste(utils::head(orphans, 4), collapse = "; "))
    } else if (length(phantoms)) {
      paste("PHANTOM ownership row(s) matching no contract section:",
            paste(utils::head(phantoms, 4), collapse = "; "))
    } else if (length(dupes)) {
      paste("duplicate ownership row(s):", paste(dupes, collapse = "; "))
    } else {
      sprintf("%d claim-bearing sections, %d ownership rows, exact bijection",
              length(derived), nrow(own))
    }
  )

  # T2 -- owner vocabulary and carrier discipline, over the SECTION rows and
  # the claim OVERRIDE rows alike. Both tables feed one census, so a spelling
  # admitted in one and rejected by the other is exactly the disagreement this
  # rule exists to prevent (RURL-fymdhizq, one predicate and no second copy).
  ovr <- map$overrides
  owner <- c(own$owning_slice, ovr$owning_slice)
  label <- paste(c(rep("section", nrow(own)), rep("override", nrow(ovr))),
                 "row", c(seq_len(nrow(own)), seq_len(nrow(ovr))))
  is_unassigned <- is_unassigned_owner(owner)
  bad_owner <- owner[!is_unassigned & !(owner %in% SLICE_REGISTRY)]
  carrier <- c(own$carrier, ovr$carrier)
  carrier_clean <- gsub("^—$", "", trimws(carrier))
  bad_carrier <- label[which(
    (is_unassigned & !grepl(CARRIER_RE, carrier_clean)) |
      (!is_unassigned & nzchar(carrier_clean))
  )]
  res[[length(res) + 1L]] <- check(
    "T2",
    !length(bad_owner) && !length(bad_carrier),
    if (length(bad_owner)) {
      # An UNASSIGNED-prefixed spelling that failed the predicate is a
      # qualifier the census cannot group, not an invented slice name. Say so:
      # the two need different edits.
      near <- unique(bad_owner[startsWith(bad_owner, UNASSIGNED)])
      rest <- unique(bad_owner[!startsWith(bad_owner, UNASSIGNED)])
      paste(c(
        if (length(near)) {
          paste("UNASSIGNED qualifier the census cannot group (use",
                "UNASSIGNED[subtype]):", paste(near, collapse = ", "))
        },
        if (length(rest)) {
          paste("owner not in the slice registry:",
                paste(rest, collapse = ", "))
        }
      ), collapse = "; ")
    } else if (length(bad_carrier)) {
      paste("carrier required iff UNASSIGNED; violated at:",
            paste(bad_carrier, collapse = ", "))
    } else {
      sprintf(paste("%d owners in registry, %d UNASSIGNED with a carrier,",
                    "over %d section row(s) and %d claim override(s)"),
              sum(!is_unassigned), sum(is_unassigned), nrow(own), nrow(ovr))
    }
  )

  # T3 -- generated blocks equal a fresh derivation. A derivation that REFUSES
  # to produce a sound census is a T3 failure with its reason, not a traceback:
  # the gate has to report a verdict for every rule even when one cannot run.
  fresh <- tryCatch(generate_blocks(root, map), error = identity)
  stale <- if (inherits(fresh, "error")) {
    GENERATED_BLOCKS
  } else {
    GENERATED_BLOCKS[vapply(GENERATED_BLOCKS, function(b) {
      !identical(block_content(map$lines, b), fresh[[b]])
    }, logical(1))]
  }
  res[[length(res) + 1L]] <- check(
    "T3",
    !length(stale) && !inherits(fresh, "error"),
    if (inherits(fresh, "error")) {
      paste("cannot derive the generated blocks:", conditionMessage(fresh))
    } else if (length(stale)) {
      paste0("stale generated block(s): ", paste(stale, collapse = ", "),
             " -- rerun with --regenerate")
    } else {
      sprintf("%d claims across %d contracts regenerate identically",
              nrow(claims), length(unique(claims$contract)))
    }
  )

  # T4 -- no phantom paths.
  cited <- paths_in_record(map$lines)
  gone <- cited[!file.exists(file.path(root, cited))]
  res[[length(res) + 1L]] <- check(
    "T4",
    !length(gone),
    if (length(gone)) {
      paste("cited path(s) not on disk:", paste(gone, collapse = ", "))
    } else {
      sprintf("%d cited repo path(s) all exist", length(cited))
    }
  )

  # T5 -- slice registry against the record AND disk.
  sl <- map$slices
  reg_missing <- setdiff(SLICE_REGISTRY, sl$slice_id)
  reg_extra <- setdiff(sl$slice_id, SLICE_REGISTRY)
  exists_on_disk <- file.exists(file.path(root, sl$tracked_path))
  claimed_shipped <- trimws(sl$state) == "SHIPPED"
  mismatched <- sl$slice_id[exists_on_disk != claimed_shipped]
  bad_state <- sl$slice_id[!trimws(sl$state) %in% c("SHIPPED", "OWED")]
  res[[length(res) + 1L]] <- check(
    "T5",
    !length(reg_missing) && !length(reg_extra) && !length(mismatched) &&
      !length(bad_state),
    if (length(reg_missing) || length(reg_extra)) {
      paste("slice table disagrees with the registry; missing:",
            paste(reg_missing, collapse = ","), "| extra:",
            paste(reg_extra, collapse = ","))
    } else if (length(bad_state)) {
      paste("state must be SHIPPED or OWED:",
            paste(bad_state, collapse = ", "))
    } else if (length(mismatched)) {
      paste("state disagrees with disk:", paste(mismatched, collapse = ", "))
    } else {
      sprintf("%d slices: %d SHIPPED, %d OWED, all agree with disk",
              nrow(sl), sum(claimed_shipped), sum(!claimed_shipped))
    }
  )

  # T6 -- zero-claim contracts are dispositioned, not silently dropped.
  zero <- unname(paths[!names(paths) %in% unique(claims$contract)])
  listed <- trimws(gsub("`", "", map$exclusions$path, fixed = TRUE))
  undisclosed <- setdiff(zero, listed)
  wrongly <- listed[listed %in% unname(paths[unique(claims$contract)])]
  no_reason <- listed[blank(map$exclusions$reason)]
  res[[length(res) + 1L]] <- check(
    "T6",
    !length(undisclosed) && !length(wrongly) && !length(no_reason),
    if (length(undisclosed)) {
      paste("contract(s) contributing no claims and not excluded:",
            paste(undisclosed, collapse = ", "))
    } else if (length(wrongly)) {
      paste("excluded file(s) that DO contribute claims:",
            paste(wrongly, collapse = ", "))
    } else if (length(no_reason)) {
      paste("exclusion(s) with no reason:", paste(no_reason, collapse = ", "))
    } else {
      sprintf("%d zero-claim contract(s) dispositioned", length(zero))
    }
  )

  # T7 -- a shipped owning slice must cite the contract it owns. Read off the
  # RESOLVED per-claim owners, not the section table: an override can hand a
  # claim to a slice whose section-level rows never mention that contract, and
  # a citation rule blind to overrides would let it grant coverage silently.
  resolved <- apply_overrides(
    section_owner(claims, own), claims, map$overrides
  )
  owned <- data.frame(contract = claims$contract, owning_slice = resolved,
                      stringsAsFactors = FALSE)
  pairs <- unique(owned[resolved %in% SLICE_REGISTRY, , drop = FALSE])
  uncited <- character(0)
  for (i in seq_len(nrow(pairs))) {
    sp <- file.path(root, VERIFICATION_DIR,
                    paste0(pairs$owning_slice[i], ".md"))
    if (!file.exists(sp)) next
    want <- paths[[pairs$contract[i]]]
    body <- readLines(sp, warn = FALSE, encoding = "UTF-8")
    if (!any(grepl(want, body, fixed = TRUE))) {
      uncited <- c(uncited, paste0(pairs$owning_slice[i], " -> ", want))
    }
  }
  res[[length(res) + 1L]] <- check(
    "T7",
    !length(uncited),
    if (length(uncited)) {
      paste("shipped slice does not cite the contract it owns:",
            paste(uncited, collapse = "; "))
    } else {
      "every shipped owning slice cites its contract by path"
    }
  )

  # T8 -- discharge records, both directions.
  res[[length(res) + 1L]] <- check_discharges(map, root, paths)

  # T9 -- claim ownership overrides.
  res[[length(res) + 1L]] <- check_overrides(map, claims)

  res
}

# T8. The verification directory partitions into three kinds of record, and
# this rule is what makes that partition a fact rather than a convention:
# this map, a registered slice, or a registered discharge record. A file that
# is none of them FAILS -- which is the reverse direction, and the one that
# was missing: three discharge records sat on disk with the map unaware of
# them, and `deferral-gate.R` D2 was leaning on exactly those files.
#
# The forward direction checks that a listed record is what it says it is: the
# file exists, its id matches its path, its `deferral_id` is well formed, it
# actually carries the `DISCHARGED[VD-nnn]` claim D2 will credit it with, and
# it names the contract whose cells it maps.
#
# The contract check matches the contract's BASENAME, not its repo path. The
# family spells the same citation two ways -- `contracts/output-contracts.md`
# and the full `design/work/url-v3/...` form, both in use across the four
# records on disk -- and requiring one spelling would be a naming rule wearing
# a coverage rule's clothes. T7 can demand the full path of a slice because a
# slice's own contract citation is load-bearing for section ownership; here
# the question is only whether the record names its subject.
check_discharges <- function(map, root, paths) {
  d <- map$discharges
  if (!nrow(d)) {
    unaccounted <- unregistered_records(root, character(0))
    return(check(
      "T8", !length(unaccounted),
      if (length(unaccounted)) {
        paste("verification record(s) in no registry:",
              paste(unaccounted, collapse = ", "))
      } else {
        "no discharge records registered, and none on disk"
      }
    ))
  }

  bad <- character(0)
  for (i in seq_len(nrow(d))) {
    id <- d$record_id[i]
    if (any(blank(unlist(d[i, DISCHARGE_FIELDS], use.names = FALSE)))) {
      bad <- c(bad, paste0(id, ": empty field"))
      next
    }
    want_path <- file.path(VERIFICATION_DIR, paste0(id, ".md"))
    if (!identical(trimws(d$tracked_path[i]), want_path)) {
      bad <- c(bad, paste0(id, ": tracked_path is not ", want_path))
      next
    }
    if (id %in% SLICE_REGISTRY) {
      bad <- c(bad, paste0(id,
                           ": a registered slice is not a discharge record"))
      next
    }
    if (!grepl(DEFERRAL_ID_RE, d$deferral_id[i])) {
      bad <- c(bad, paste0(id, ": deferral_id '", d$deferral_id[i],
                           "' fails ", DEFERRAL_ID_RE))
      next
    }
    if (!file.exists(file.path(root, want_path))) {
      bad <- c(bad, paste0(id, ": listed but not on disk"))
      next
    }
    abbrev <- trimws(d$contract[i])
    if (!abbrev %in% names(paths)) {
      bad <- c(bad, paste0(id, ": contract '", abbrev,
                           "' is not a §6 contract"))
      next
    }
    body <- readLines(file.path(root, want_path), warn = FALSE,
                      encoding = "UTF-8")
    txt <- paste(body, collapse = "\n")
    if (!grepl(paste0("DISCHARGED[", d$deferral_id[i], "]"), txt,
               fixed = TRUE)) {
      bad <- c(bad, paste0(id, ": record does not claim DISCHARGED[",
                           d$deferral_id[i], "]"))
      next
    }
    if (!grepl(basename(paths[[abbrev]]), txt, fixed = TRUE)) {
      bad <- c(bad, paste0(id, ": record never names ",
                           basename(paths[[abbrev]])))
    }
  }
  dupes <- unique(d$record_id[duplicated(d$record_id)])
  if (length(dupes)) {
    bad <- c(bad, paste("duplicate record_id:", paste(dupes, collapse = ", ")))
  }
  unaccounted <- unregistered_records(root, d$record_id)

  check(
    "T8",
    !length(bad) && !length(unaccounted),
    if (length(bad)) {
      paste("discharge record(s) failing the registry:",
            paste(bad, collapse = "; "))
    } else if (length(unaccounted)) {
      paste("verification record(s) in no registry:",
            paste(unaccounted, collapse = ", "))
    } else {
      sprintf("%d discharge record(s), each claiming its deferral; %s",
              nrow(d), "the verification directory partitions")
    }
  )
}

# Files under verification/ that are neither this map, nor a registered slice,
# nor a registered discharge record. `known_discharges` is passed in rather
# than re-read so the empty-table case and the populated one ask disk the same
# question.
unregistered_records <- function(root, known_discharges) {
  dir <- file.path(root, VERIFICATION_DIR)
  if (!dir.exists(dir)) return(character(0))
  on_disk <- list.files(dir, pattern = "[.]md$")
  known <- c(basename(MAP_REL), paste0(SLICE_REGISTRY, ".md"),
             paste0(known_discharges, ".md"))
  setdiff(on_disk, known)
}

# T9. The override table is the ONE place ownership is hand-assigned below
# section granularity, so it gets the same treatment `## Section ownership`
# gets from T1: a row must name something that exists, exactly once. Three
# failures, each with its own edit:
#
#   PHANTOM     -- `claim_id` is in no contract. A renamed row key or a
#                  reordered claim-bearing section renumbers ids (the map says
#                  so in `## Open cells` 4), and this is where that shows up:
#                  an override pointing at a dead id silently stops moving its
#                  claim, and the census reports the section owner as if the
#                  dissent had never been recorded.
#   DUPLICATE   -- two rows for one claim. Last write would win, which is not a
#                  rule anybody chose.
#   REDUNDANT   -- the override restates the section owner. It moves nothing
#                  today, and tomorrow -- when the section row changes -- it
#                  pins the claim to the old owner for a reason nobody wrote
#                  down. A dissent list that contains agreements is not one.
#
# `reason` is required for the same reason `carrier` is on an UNASSIGNED row:
# the table exists where the general rule is wrong, and the next reader needs
# the precedent that moved it.
check_overrides <- function(map, claims) {
  ovr <- map$overrides
  if (!nrow(ovr)) {
    return(check("T9", TRUE, "no claim ownership overrides registered"))
  }
  ids <- trimws(ovr$claim_id)
  bad <- character(0)
  if (any(blank(ovr$reason))) {
    bad <- c(bad, paste("override(s) with no reason:",
                        paste(ids[blank(ovr$reason)], collapse = ", ")))
  }
  phantom <- ids[!ids %in% claims$claim_id]
  if (length(phantom)) {
    bad <- c(bad, paste("claim_id in no contract:",
                        paste(phantom, collapse = ", ")))
  }
  dupes <- unique(ids[duplicated(ids)])
  if (length(dupes)) {
    bad <- c(bad, paste("duplicate claim_id:", paste(dupes, collapse = ", ")))
  }
  at <- match(ids, claims$claim_id)
  sect <- section_owner(claims, map$ownership)
  same <- !is.na(at) & sect[at] == trimws(ovr$owning_slice)
  if (any(same)) {
    bad <- c(bad, paste("override restates its section's owner:",
                        paste(ids[same], collapse = ", ")))
  }
  check(
    "T9", !length(bad),
    if (length(bad)) {
      paste(bad, collapse = "; ")
    } else {
      sprintf("%d claim override(s), each dissenting from its section",
              nrow(ovr))
    }
  )
}

# ---- reporting --------------------------------------------------------------

report <- function(res) {
  for (r in res) {
    cat(sprintf("  %-3s %-4s %s\n", r$id, if (r$ok) "PASS" else "FAIL",
                r$detail))
  }
  all(vapply(res, function(r) r$ok, logical(1)))
}

repo_root <- function() {
  here <- normalizePath(".", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(here, MAP_REL))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  NULL
}

main <- function(args) {
  root <- repo_root()
  if (is.null(root)) {
    cat("TRACEABILITY GATE: FAIL -- cannot locate", MAP_REL, "\n")
    quit(status = 1L)
  }
  if ("--regenerate" %in% args) {
    regenerate(root)
    cat("regenerated", MAP_REL, "\n")
    quit(status = 0L)
  }
  cat("CLAIM-TRACEABILITY GATE (G4 criterion 1 / RCON-10 / S9 H6)\n")
  ok <- report(evaluate(read_map(file.path(root, MAP_REL)), root))
  cat(sprintf("VERDICT: %s\n", if (ok) "PASS" else "FAIL"))
  quit(status = if (ok) 0L else 1L)
}

# ---- self-test --------------------------------------------------------------

# A throwaway repo root: two small contracts, a slice tree, and a map whose
# generated blocks are filled in by the generator itself, so a fixture that is
# not exercising T3 does not trip it by accident.
mk <- function(contracts = NULL, ownership = NULL, slices = NULL,
               exclusions = NULL, extra_files = character(0),
               shipped = "cache-slice", regenerate_blocks = TRUE,
               sections = REQUIRED_SECTIONS, discharges = NULL,
               discharge_records = list(), overrides = NULL) {
  root <- file.path(tempdir(), paste0("trg-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, CONTRACT_DIR), recursive = TRUE,
             showWarnings = FALSE)
  dir.create(file.path(root, VERIFICATION_DIR), recursive = TRUE,
             showWarnings = FALSE)
  if (is.null(contracts)) contracts <- default_contracts()
  for (nm in names(contracts)) {
    writeLines(contracts[[nm]], file.path(root, CONTRACT_DIR, nm))
  }
  for (s in shipped) {
    writeLines(
      c("# slice", paste0("Verifies `", CONTRACT_DIR, "/semantic-cache.md`.")),
      file.path(root, VERIFICATION_DIR, paste0(s, ".md"))
    )
  }
  for (f in extra_files) {
    dir.create(file.path(root, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    writeLines("x", file.path(root, f))
  }
  for (nm in names(discharge_records)) {
    writeLines(discharge_records[[nm]],
               file.path(root, VERIFICATION_DIR, paste0(nm, ".md")))
  }
  if (is.null(ownership)) ownership <- default_ownership()
  if (is.null(slices)) slices <- default_slices()
  if (is.null(exclusions)) exclusions <- default_exclusions()
  if (is.null(discharges)) discharges <- character(0)
  if (is.null(overrides)) overrides <- character(0)
  body <- character(0)
  for (s in sections) {
    body <- c(body, paste0("## ", s), "")
    if (identical(s, "Section ownership")) {
      body <- c(body, md_table(OWNERSHIP_FIELDS, ownership), "")
    } else if (identical(s, "Verification slices")) {
      body <- c(body, md_table(SLICE_FIELDS, slices), "")
    } else if (identical(s, "Discharge records")) {
      body <- c(body, md_table(DISCHARGE_FIELDS, discharges), "")
    } else if (identical(s, "Claim ownership overrides")) {
      body <- c(body, md_table(OVERRIDE_FIELDS, overrides), "")
    } else if (identical(s, "Excluded sources")) {
      body <- c(body, md_table(EXCLUSION_FIELDS, exclusions), "")
    } else if (identical(s, "Claim index")) {
      body <- c(body, "<!-- BEGIN GENERATED: claim-index -->",
                "<!-- END GENERATED: claim-index -->", "")
    } else if (identical(s, "Coverage census")) {
      body <- c(body, "<!-- BEGIN GENERATED: coverage-census -->",
                "<!-- END GENERATED: coverage-census -->", "")
    } else {
      body <- c(body, "Prose.", "")
    }
  }
  writeLines(c("# Traceability map", "", body), file.path(root, MAP_REL))
  if (regenerate_blocks) regenerate(root)
  root
}

default_contracts <- function() {
  list(
    "semantic-cache.md" = c(
      "# c", "", "## Cache inventory rows", "",
      "| cache | owner_decision_ref | status |", "|---|---|---|",
      "| stage_a | P5.1 | SETTLED |", "| psl | P5.1 | SETTLED |", "",
      "## Bound rows", "",
      "| dimension | owner_decision_ref | status |", "|---|---|---|",
      "| bound | P5.1 | OPEN |"
    ),
    "state.md" = c(
      "# s", "", "## Rows", "",
      "| field | owner_decision_ref | status |", "|---|---|---|",
      "| host | P1.1 | SETTLED |"
    )
  )
}

default_ownership <- function() {
  c(
    md_row("SC", "s1", "Cache inventory rows", "cache-slice", "—"),
    md_row("SC", "s2", "Bound rows", "cache-slice", "—"),
    md_row("CS", "s1", "Rows", "state-slice", "—")
  )
}

default_slices <- function() {
  vapply(SLICE_REGISTRY, function(s) {
    md_row(s, file.path(VERIFICATION_DIR, paste0(s, ".md")),
           if (identical(s, "cache-slice")) "SHIPPED" else "OWED")
  }, character(1), USE.NAMES = FALSE)
}

default_exclusions <- function() character(0)

self_test <- function() {
  passed <- 0L
  failed <- 0L
  expect <- function(what, ok) {
    if (isTRUE(ok)) {
      passed <<- passed + 1L
    } else {
      failed <<- failed + 1L
      cat("  FAIL:", what, "\n")
    }
  }
  # Self-test contracts use two files; map them into the abbrev table.
  old <- CONTRACT_ABBREV
  CONTRACT_ABBREV <<- c(old, "semantic-cache.md" = "SC", "state.md" = "CS")
  on.exit(CONTRACT_ABBREV <<- old, add = TRUE)

  verdict <- function(root, rule) {
    res <- evaluate(read_map(file.path(root, MAP_REL)), root)
    hit <- Filter(function(r) identical(r$id, rule), res)
    hit[[1]]$ok
  }
  all_pass <- function(root) {
    all(vapply(evaluate(read_map(file.path(root, MAP_REL)), root),
               function(r) r$ok, logical(1)))
  }

  # --- baseline -------------------------------------------------------------
  root <- mk()
  expect("baseline passes every rule", all_pass(root))

  # --- T0 -------------------------------------------------------------------
  expect("T0 flags a missing required section",
         !verdict(mk(sections = setdiff(REQUIRED_SECTIONS, "Open cells")),
                  "T0"))
  bad <- mk()
  ln <- readLines(file.path(bad, MAP_REL))
  ln <- ln[!grepl("END GENERATED: coverage-census", ln)]
  writeLines(ln, file.path(bad, MAP_REL))
  expect("T0 flags an unpaired generated marker", !verdict(bad, "T0"))
  expect("T0 flags a short ownership row",
         !verdict(mk(ownership = c(default_ownership(),
                                   md_row("CS", "s9", "Nope")),
                     regenerate_blocks = FALSE), "T0"))

  # --- T1 -------------------------------------------------------------------
  expect("T1 flags an ORPHAN section (contract row with no owner)",
         !verdict(mk(ownership = default_ownership()[1:2]), "T1"))
  expect("T1 flags a PHANTOM ownership row",
         !verdict(mk(ownership = c(default_ownership(),
                                   md_row("SC", "s3", "Ghost", "cache-slice",
                                          "—"))), "T1"))
  expect("T1 flags a duplicate ownership row",
         !verdict(mk(ownership = c(default_ownership(),
                                   default_ownership()[1])), "T1"))

  # --- T2 -------------------------------------------------------------------
  # regenerate_blocks = FALSE: an owner outside the census vocabulary is one
  # the generator now refuses to tally at all, so the fixture cannot build its
  # blocks. Same opt-out the malformed-row fixtures above use.
  expect("T2 rejects an invented slice name",
         !verdict(mk(ownership = c(
           default_ownership()[1:2],
           md_row("CS", "s1", "Rows", "vibes-slice", "—")),
           regenerate_blocks = FALSE), "T2"))
  expect("T2 rejects UNASSIGNED with no carrier",
         !verdict(mk(ownership = c(
           default_ownership()[1:2],
           md_row("CS", "s1", "Rows", "UNASSIGNED", "—"))), "T2"))
  expect("T2 rejects a malformed carrier id",
         !verdict(mk(ownership = c(
           default_ownership()[1:2],
           md_row("CS", "s1", "Rows", "UNASSIGNED", "RURL-XX"))), "T2"))
  expect("T2 rejects a carrier on an assigned row",
         !verdict(mk(ownership = c(
           default_ownership()[1:2],
           md_row("CS", "s1", "Rows", "state-slice", "RURL-abcd1234"))), "T2"))
  expect("T2 accepts UNASSIGNED with a well-formed carrier",
         verdict(mk(ownership = c(
           default_ownership()[1:2],
           md_row("CS", "s1", "Rows", "UNASSIGNED", "RURL-abcd1234"))), "T2"))

  # RURL-fymdhizq. The vocabulary rule and the census grouper must accept the
  # SAME set. A spelling T2 admits but generate_census() cannot group takes its
  # claims out of the by-slice tally with all eight rules green.
  qualified <- function(owner) {
    mk(ownership = c(default_ownership()[1:2],
                     md_row("CS", "s1", "Rows", owner, "RURL-abcd1234")),
       regenerate_blocks = FALSE)
  }
  expect("T2 accepts the bracket-qualified UNASSIGNED the census can group",
         verdict(qualified("UNASSIGNED[criterion-1]"), "T2"))
  expect("T2 rejects a parenthesized UNASSIGNED qualifier",
         !verdict(qualified("UNASSIGNED (criterion-1 self-verified)"), "T2"))
  expect("T2 rejects a bare-suffix UNASSIGNED qualifier",
         !verdict(qualified("UNASSIGNEDish"), "T2"))
  expect("T2 rejects an empty bracket qualifier",
         !verdict(qualified("UNASSIGNED[]"), "T2"))

  # The by-slice rows must sum to the census total. Asserted on the generator
  # itself, because T3 compares the block against this same generator and so
  # agrees with it byte-for-byte when both are wrong.
  census_sums <- function(root) {
    ln <- readLines(file.path(root, MAP_REL))
    at <- block_bounds(ln, "coverage-census")
    rows <- ln[seq.int(at[1], at[2])]
    # The block carries TWO tables and both have `claims` in column 4, so a
    # whole-block sum silently double-counts. Score the by-slice table only.
    from <- grep("^### By owning slice", rows)
    to <- grep("^### By contract", rows)
    rows <- rows[seq.int(from + 1L, to - 1L)]
    rows <- rows[grepl("^\\| ", rows) & !grepl("^\\|---", rows)]
    cells <- lapply(strsplit(rows, "\\|"), trimws)
    n <- vapply(cells, function(x) suppressWarnings(as.integer(x[4])),
                integer(1))
    is_total <- vapply(cells, function(x) identical(x[2], "**total**"),
                       logical(1))
    !is.na(n[is_total]) && sum(n[!is_total & !is.na(n)]) == n[is_total]
  }
  expect("the by-slice census sums to its own total", census_sums(mk()))
  expect("the census generator refuses to drop an ungroupable owner",
         inherits(try(regenerate(qualified("UNASSIGNED (x)")), silent = TRUE),
                  "try-error"))

  # --- T3 -------------------------------------------------------------------
  stale <- mk()
  ln <- readLines(file.path(stale, MAP_REL))
  ln <- sub("^\\| TR-CS-s1-host \\| SETTLED", "| TR-CS-s1-host | OPEN", ln)
  writeLines(ln, file.path(stale, MAP_REL))
  expect("T3 flags a hand-edited claim-index row", !verdict(stale, "T3"))

  stale2 <- mk()
  ln <- readLines(file.path(stale2, MAP_REL))
  ln <- sub("^\\| \\*\\*total\\*\\* \\| — \\| 4 ", "| **total** | — | 99 ", ln)
  writeLines(ln, file.path(stale2, MAP_REL))
  expect("T3 flags a doctored census total", !verdict(stale2, "T3"))

  grown <- mk()
  cf <- file.path(grown, CONTRACT_DIR, "state.md")
  writeLines(c(readLines(cf), "| port | P1.1 | SETTLED |"), cf)
  expect("T3 flags a new contract row not yet regenerated",
         !verdict(grown, "T3"))

  # --- T4 -------------------------------------------------------------------
  ghost <- mk()
  ln <- readLines(file.path(ghost, MAP_REL))
  ln <- c(ln, "See `design/work/url-v3/tools/not-merged-yet.R`.")
  writeLines(ln, file.path(ghost, MAP_REL))
  expect("T4 flags a forward reference to an unmerged path",
         !verdict(ghost, "T4"))

  # --- T5 -------------------------------------------------------------------
  expect("T5 flags a slice missing from the table",
         !verdict(mk(slices = default_slices()[-3]), "T5"))
  expect("T5 flags a slice not in the registry",
         !verdict(mk(slices = c(default_slices(),
                                md_row("ghost-slice", "x.md", "OWED"))), "T5"))
  expect("T5 flags an unknown state token",
         !verdict(mk(slices = sub("OWED", "SOON", default_slices())), "T5"))
  drift <- mk(shipped = c("cache-slice", "state-slice"))
  expect("T5 flags a slice authored on disk but still marked OWED",
         !verdict(drift, "T5"))
  expect("T5 flags SHIPPED with no file on disk",
         !verdict(mk(shipped = character(0)), "T5"))

  # --- T6 -------------------------------------------------------------------
  cs <- default_contracts()
  cs[["capstone.md"]] <- c("# cap", "", "## Assertions", "",
                           "| # | verdict |", "|---|---|", "| (i) | PASS |")
  old2 <- CONTRACT_ABBREV
  CONTRACT_ABBREV <<- c(old2, "capstone.md" = "CA")
  expect("T6 flags a zero-claim contract with no disposition",
         !verdict(mk(contracts = cs), "T6"))
  expect("T6 accepts a zero-claim contract that is dispositioned",
         verdict(mk(contracts = cs, exclusions = md_row(
           paste0("`", CONTRACT_DIR, "/capstone.md`"), "0", "capstone")),
           "T6"))
  expect("T6 flags excluding a contract that DOES carry claims",
         !verdict(mk(contracts = cs, exclusions = c(
           md_row(paste0("`", CONTRACT_DIR, "/capstone.md`"), "0", "capstone"),
           md_row(paste0("`", CONTRACT_DIR, "/state.md`"), "0", "nope"))),
           "T6"))
  expect("T6 flags an exclusion with no reason",
         !verdict(mk(contracts = cs, exclusions = md_row(
           paste0("`", CONTRACT_DIR, "/capstone.md`"), "0", "")), "T6"))
  CONTRACT_ABBREV <<- old2

  # --- T7 -------------------------------------------------------------------
  silent <- mk()
  sp <- file.path(silent, VERIFICATION_DIR, "cache-slice.md")
  writeLines(c("# slice", "Says nothing about any contract."), sp)
  expect("T7 flags a shipped slice that never cites its contract",
         !verdict(silent, "T7"))

  # --- T8 -------------------------------------------------------------------
  # A well-formed discharge record: it claims one deferral and names the
  # contract whose cells it maps, in the relative spelling two of the four real
  # records use.
  drec <- function(id = "VD-001", contract = "semantic-cache.md") {
    c("# discharge", "", paste0("## DISCHARGED[", id, "]"), "",
      paste0("Cells of `contracts/", contract, "` now ship."))
  }
  drow <- function(record = "cache-discharge", id = "VD-001",
                   path = NULL, contract = "SC", scope = "one row") {
    md_row(record, id,
           if (is.null(path)) {
             file.path(VERIFICATION_DIR, paste0(record, ".md"))
           } else {
             path
           },
           contract, scope)
  }
  registered <- function(...) {
    mk(discharges = drow(...),
       discharge_records = list("cache-discharge" = drec()))
  }
  expect("baseline with no discharge records passes T8", verdict(mk(), "T8"))
  expect("T8 accepts a registered discharge record",
         verdict(registered(), "T8"))

  # The reverse direction, and the reason this rule exists: a record on disk
  # that no registry names. Three of these sat unaccounted while both gates
  # passed (P0.7 D-E).
  expect("T8 flags a verification record in no registry",
         !verdict(mk(discharge_records = list("ghost-discharge" = drec())),
                  "T8"))
  expect("T8 flags a listed record with no file on disk",
         !verdict(mk(discharges = drow()), "T8"))
  expect("T8 flags a record that claims no deferral",
         !verdict(mk(discharges = drow(),
                     discharge_records = list(
                       "cache-discharge" = c("# d", "cache things"))), "T8"))
  expect(paste("T8 flags a record claiming a DIFFERENT deferral than it is",
               "listed for"),
         !verdict(mk(discharges = drow(id = "VD-002"),
                     discharge_records = list(
                       "cache-discharge" = drec("VD-001"))), "T8"))
  expect("T8 flags a malformed deferral_id",
         !verdict(mk(discharges = drow(id = "VD1"),
                     discharge_records = list(
                       "cache-discharge" = drec("VD1"))), "T8"))
  expect("T8 flags an unknown contract abbreviation",
         !verdict(registered(contract = "ZZ"), "T8"))
  expect("T8 flags a record that never names its contract",
         !verdict(mk(discharges = drow(contract = "CS"),
                     discharge_records = list(
                       "cache-discharge" = drec())), "T8"))
  expect("T8 flags a tracked_path that disagrees with the record id",
         !verdict(mk(discharges = drow(path = "design/elsewhere.md"),
                     discharge_records = list(
                       "cache-discharge" = drec())), "T8"))
  expect("T8 flags a discharge record wearing a registered slice's name",
         !verdict(mk(discharges = drow(record = "cache-slice"),
                     shipped = "cache-slice"), "T8"))
  expect("T8 flags a duplicate record_id",
         !verdict(mk(discharges = c(drow(), drow()),
                     discharge_records = list("cache-discharge" = drec())),
                  "T8"))
  expect("T0 flags a short discharge row",
         !verdict(mk(discharges = md_row("cache-discharge", "VD-001"),
                     discharge_records = list("cache-discharge" = drec())),
                  "T0"))
  expect("T0 flags a missing Discharge records section",
         !verdict(mk(sections = setdiff(REQUIRED_SECTIONS,
                                        "Discharge records")), "T0"))

  # --- T9 and the override derivation ---------------------------------------
  # The gotcha this leaf was filed with (RURL-sbhpzwzk / P0.8 D-D): the claim
  # index is compared byte-for-byte against a fresh derivation, so a gate that
  # PARSED the override table and then ignored it would pass T0-T8 and T3 with
  # the claim still reading its section's owner. Every assertion below that
  # matters therefore reads the GENERATED block, not the rule verdict.
  index_row <- function(root, claim) {
    rows <- block_content(readLines(file.path(root, MAP_REL)), "claim-index")
    hit <- rows[startsWith(rows, paste0("| ", claim, " |"))]
    if (!length(hit)) return(NULL)
    split_row(hit[1])
  }
  census_claims <- function(root, owner) {
    rows <- block_content(readLines(file.path(root, MAP_REL)),
                          "coverage-census")
    rows <- rows[seq.int(grep("^### By owning slice", rows) + 1L,
                         grep("^### By contract", rows) - 1L)]
    hit <- rows[startsWith(rows, paste0("| ", owner, " |"))]
    if (!length(hit)) return(0L)
    as.integer(split_row(hit[1])[3])
  }
  ovr_row <- function(claim = "TR-CS-s1-host", owner = "cache-slice",
                      carrier = "—", reason = "KJ s3 precedent") {
    md_row(claim, owner, carrier, reason)
  }
  # `state.md` §Rows is owned by the OWED state-slice, so its one claim reads
  # PENDING at baseline. Moving it to the SHIPPED cache-slice changes owner,
  # coverage and tally at once -- three observables, one derivation.
  cite_both <- function(root) {
    writeLines(
      c("# slice", paste0("Verifies `", CONTRACT_DIR, "/semantic-cache.md`",
                          " and `", CONTRACT_DIR, "/state.md`.")),
      file.path(root, VERIFICATION_DIR, "cache-slice.md")
    )
    root
  }
  base <- mk()
  expect("baseline: the claim reads its SECTION owner",
         identical(index_row(base, "TR-CS-s1-host")[3:4],
                   c("state-slice", "PENDING")))
  expect("baseline: cache-slice tallies 3, state-slice 1",
         census_claims(base, "cache-slice") == 3L &&
           census_claims(base, "state-slice") == 1L)

  moved <- cite_both(mk(overrides = ovr_row()))
  expect("an override MOVES the owner in the generated claim index",
         identical(index_row(moved, "TR-CS-s1-host")[3], "cache-slice"))
  expect("an override re-derives coverage from the new owner (MAPPED)",
         identical(index_row(moved, "TR-CS-s1-host")[4], "MAPPED"))
  expect("an override moves the claim's census tally with it",
         census_claims(moved, "cache-slice") == 4L &&
           census_claims(moved, "state-slice") == 0L)
  expect("a well-formed override passes every rule", all_pass(moved))

  expect("T9 flags an override naming a claim in no contract",
         !verdict(mk(overrides = ovr_row(claim = "TR-CS-s1-ghost")), "T9"))
  expect("T9 flags a duplicate claim_id",
         !verdict(cite_both(mk(overrides = c(ovr_row(), ovr_row()))), "T9"))
  expect("T9 flags an override that restates its section's owner",
         !verdict(mk(overrides = ovr_row(owner = "state-slice")), "T9"))
  expect("T9 flags an override with no reason",
         !verdict(cite_both(mk(overrides = ovr_row(reason = ""))), "T9"))
  expect("T0 flags a short override row",
         !verdict(mk(overrides = md_row("TR-CS-s1-host", "cache-slice"),
                     regenerate_blocks = FALSE), "T0"))
  expect("T0 flags a missing Claim ownership overrides section",
         !verdict(mk(sections = setdiff(REQUIRED_SECTIONS,
                                        "Claim ownership overrides")), "T0"))

  # T2 is one vocabulary over both tables, not two that can drift apart.
  expect("T2 rejects an invented slice name in an override row",
         !verdict(mk(overrides = ovr_row(owner = "vibes-slice"),
                     regenerate_blocks = FALSE), "T2"))
  expect("T2 rejects a carrier on an assigned override row",
         !verdict(cite_both(mk(overrides = ovr_row(carrier = "RURL-abcd1234"))),
                  "T2"))
  expect("T2 rejects an UNASSIGNED override with no carrier",
         !verdict(mk(overrides = ovr_row(owner = "UNASSIGNED")), "T2"))
  expect("T2 accepts an UNASSIGNED[subtype] override with a carrier",
         verdict(mk(overrides = ovr_row(owner = "UNASSIGNED[capstone]",
                                        carrier = "RURL-abcd1234")), "T2"))

  # T7 reads the RESOLVED owners: an override is exactly how a shipped slice
  # can come to own a claim in a contract its section rows never mention.
  expect("T7 flags an override handing a claim to a slice that never cites it",
         !verdict(mk(overrides = ovr_row()), "T7"))

  cat(sprintf("\nself-test: %d passed, %d failed\n", passed, failed))
  quit(status = if (failed == 0L) 0L else 1L)
}

args <- commandArgs(trailingOnly = TRUE)
if ("--self-test" %in% args) self_test() else main(args)
