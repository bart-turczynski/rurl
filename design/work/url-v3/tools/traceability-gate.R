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
# SECTION (67 rows), the slice registry, and the excluded-source dispositions.
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
#                      admitted here and then vanish from the tally.
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
#   T7  citation    -- a contract with at least one section owned by a
#                      SHIPPED slice must be cited by path in that slice.
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

SLICE_FIELDS <- c("slice_id", "tracked_path", "state")

EXCLUSION_FIELDS <- c("path", "claims", "reason")

INDEX_FIELDS <- c(
  "claim_id", "status", "owning_slice", "coverage", "source"
)

REQUIRED_SECTIONS <- c(
  "Envelope", "Purpose", "Inputs", "Population rule", "Verification slices",
  "Section ownership", "Claim index", "Coverage census", "Excluded sources",
  "Scope boundaries", "Open cells"
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
    slices = as_frame(table_rows(lines, "Verification slices"), SLICE_FIELDS),
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

# Coverage is DERIVED, never asserted: a claim is MAPPED only when the slice
# owning its section is actually on disk. That is what stops the map from
# claiming coverage that does not exist yet.
resolve_coverage <- function(claims, ownership, root) {
  key <- paste(claims$contract, claims$section, sep = "")
  okey <- paste(ownership$contract, ownership$section, sep = "")
  owner <- ownership$owning_slice[match(key, okey)]
  owner[is.na(owner)] <- "ORPHAN"
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

generate_index <- function(claims, ownership, root, paths) {
  r <- resolve_coverage(claims, ownership, root)
  src <- paste0(paths[claims$contract], ":", claims$line)
  md_table(INDEX_FIELDS, vapply(seq_len(nrow(claims)), function(i) {
    md_row(claims$claim_id[i], claims$status[i], r$owner[i], r$coverage[i],
           paste0("`", src[i], "`"))
  }, character(1)))
}

generate_census <- function(claims, ownership, root, paths) {
  r <- resolve_coverage(claims, ownership, root)
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
    "claim-index" = generate_index(claims, map$ownership, root, paths),
    "coverage-census" = generate_census(claims, map$ownership, root, paths)
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
    sum(!map$exclusions$.width_ok)
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

  # T2 -- owner vocabulary and carrier discipline.
  owner <- own$owning_slice
  is_unassigned <- is_unassigned_owner(owner)
  bad_owner <- owner[!is_unassigned & !(owner %in% SLICE_REGISTRY)]
  carrier <- own$carrier
  carrier_clean <- gsub("^—$", "", trimws(carrier))
  bad_carrier <- which(
    (is_unassigned & !grepl(CARRIER_RE, carrier_clean)) |
      (!is_unassigned & nzchar(carrier_clean))
  )
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
      paste("carrier required iff UNASSIGNED; violated at row(s):",
            paste(bad_carrier, collapse = ", "))
    } else {
      sprintf("%d owners in registry, %d UNASSIGNED with a carrier",
              sum(!is_unassigned), sum(is_unassigned))
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

  # T7 -- a shipped owning slice must cite the contract it owns.
  shipped_owned <- own[own$owning_slice %in% SLICE_REGISTRY, , drop = FALSE]
  pairs <- unique(shipped_owned[, c("contract", "owning_slice")])
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

  res
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
               sections = REQUIRED_SECTIONS) {
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
  if (is.null(ownership)) ownership <- default_ownership()
  if (is.null(slices)) slices <- default_slices()
  if (is.null(exclusions)) exclusions <- default_exclusions()
  body <- character(0)
  for (s in sections) {
    body <- c(body, paste0("## ", s), "")
    if (identical(s, "Section ownership")) {
      body <- c(body, md_table(OWNERSHIP_FIELDS, ownership), "")
    } else if (identical(s, "Verification slices")) {
      body <- c(body, md_table(SLICE_FIELDS, slices), "")
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

  cat(sprintf("\nself-test: %d passed, %d failed\n", passed, failed))
  quit(status = if (failed == 0L) 0L else 1L)
}

args <- commandArgs(trailingOnly = TRUE)
if ("--self-test" %in% args) self_test() else main(args)
