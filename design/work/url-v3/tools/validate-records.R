#!/usr/bin/env Rscript
# validate-records.R — structural validator for the §6 records.
#
# SCOPE, after ADR 0014. This script used to enforce two different things at
# once: (1) that the records are STRUCTURALLY sound -- ids unique, references
# resolvable, registers in bijection with the sources they claim to cover, the
# public surface matching NAMESPACE -- and (2) that each record had been
# RATIFIED, via manifest hash-pinning, `## Inputs` hash cascades and an
# ACCEPTED/PROPOSED lifecycle.
#
# Only (1) survives. The ratification layer is retired: this project has one
# developer, so propose -> ratify -> seal separated a proposer from an approver
# who were the same person, and the hash cascade reopened accepted gates for
# edits that git already reports. What remains is the half that catches real
# defects -- a duplicated id, a dangling reference, an export that no inventory
# row covers -- none of which git tells you.
#
# Contract/claim drift is still caught, by a better instrument: traceability-
# gate.R regenerates the claim index and byte-compares it, so editing a contract
# still fails a machine check.
#
# Run from the repository root:
#   Rscript design/work/url-v3/tools/validate-records.R
#   Rscript design/work/url-v3/tools/validate-records.R --regenerate
#       # add a stub row for every NAMESPACE export and every
#       # `.spu_result_fields` field the two artifact-4 files lack -- the
#       # inventory register and the disposition roster -- and re-derive the
#       # roster's bijection counts; print what changed; exit 0. Free-text
#       # cells are written as `TODO`; nothing existing is rewritten.
#   Rscript design/work/url-v3/tools/validate-records.R --self-test
#       # (1) run the artifact-4 predicates (sections F, F2, G) on in-memory
#       # fixtures, one positive and one negative case per rule (RUL-009);
#       # (2) prove --regenerate on a temp copy of the real inputs: red
#       # before, green after, diff exactly the one row.

suppressWarnings(suppressMessages({
  ok <- requireNamespace("yaml", quietly = TRUE)
}))
if (!ok) stop("validate-records.R needs the 'yaml' package")
`%||%` <- function(a, b) if (is.null(a)) b else a

.vr_args <- commandArgs(trailingOnly = TRUE)
.vr_regenerate <- "--regenerate" %in% .vr_args

## --- table parsers and the artifact-4 predicates (pure) -----------------------
## Everything in this block is a function of its arguments: no `<<-`, no read of
## the working tree. It sits AHEAD of the --self-test dispatch because the
## self-test calls the same functions on in-memory fixtures that the live run
## calls on the tree (RUL-009, RURL-ajplxzik). The checks that do read the tree
## stay top-level code further down, in the order they always ran.
gv <- function(r, k) if (!is.null(names(r)) && k %in% names(r)) r[[k]] else NA_character_

# Split a table row into cells, honoring GitHub's backslash-escaped `\|` pipes
# (a `\|` inside a cell is literal, not a column separator) via a perl lookbehind.
.tcells  <- function(s) {
  s <- sub("^\\s*\\|", "", sub("\\|\\s*$", "", s))
  parts <- strsplit(s, "(?<!\\\\)\\|", perl = TRUE)[[1]]
  trimws(gsub("\\|", "|", parts, fixed = TRUE))
}
.is_trow <- function(s) grepl("^\\s*\\|", s)
.is_tsep <- function(s) grepl("^\\s*\\|[-:|[:space:]]*$", s) & grepl("-", s)
# Parse every GitHub-style pipe table in a line vector into list(header, rows).
parse_pipe_tables <- function(ln) {
  tabs <- list(); i <- 1L; N <- length(ln)
  while (i <= N) {
    if (.is_trow(ln[i]) && i < N && .is_tsep(ln[i + 1L])) {
      header <- .tcells(ln[i]); j <- i + 2L; rows <- list()
      while (j <= N && .is_trow(ln[j]) && !.is_tsep(ln[j])) {
        cs <- .tcells(ln[j])
        rows[[length(rows) + 1L]] <- stats::setNames(cs, header[seq_along(cs)])
        j <- j + 1L
      }
      tabs[[length(tabs) + 1L]] <- list(header = header, rows = rows); i <- j
    } else i <- i + 1L
  }
  tabs
}
# Lines of a "## <name>" section (exclusive of the next "## ").
section_lines <- function(ln, name) {
  h <- which(grepl(sprintf("^##\\s+%s\\s*$", name), ln)); if (length(h) != 1) return(character(0))
  nxt <- which(grepl("^##\\s", ln) & seq_along(ln) > h)
  end <- if (length(nxt)) min(nxt) - 1L else length(ln)
  ln[(h + 1):end]
}

# --- artifact-4 helpers (P0.6: invariant half + roster half) -----------------
# The three roster table shapes. Named once so section F can assert their ABSENCE
# from the gate-pinned invariant and section G their presence in the roster.
PSD_EXPORT_HDR <- c("export", "owning contract(s)", "v3 disposition", "status")
PSD_FIELD_HDR  <- c("field", "owning contract(s)", "v3 disposition", "status")
PSD_ITEM_HDR   <- c("item", "owning contract", "v3 disposition", "status")

# G3 leaf -> owning contract filename, read out of the invariant's legend table.
# Read rather than hardcoded: the legend is the invariant's to state, and a leaf
# that names a nonexistent contract must fail loudly instead of being ignored.
psd_legend <- function(ln) {
  out <- list()
  lg <- Filter(function(t) "G3 leaf" %in% t$header && "contract" %in% t$header,
               parse_pipe_tables(ln))
  if (length(lg) != 1) return(out)
  for (r in lg[[1]]$rows) {
    leaf <- trimws(gv(r, "G3 leaf") %||% "")
    cn <- gsub("`", "", trimws(gv(r, "contract") %||% ""), fixed = TRUE)
    if (grepl("^G3\\.[0-9A-Z]+$", leaf) && nzchar(cn)) out[[leaf]] <- paste0(cn, ".md")
  }
  out
}

# G3 leaf -> the accepted decisions the legend says that leaf governs. Same table
# as psd_legend(), fourth column. Used to check that a SETTLED roster row cites a
# decision its OWNER actually projects, not merely something Pn.n@sha-shaped:
# RURL-nravluqd was a roster row citing P3.3 while G3.K did not project it, which
# the shape-only check below could not see.
psd_legend_decisions <- function(ln) {
  out <- list()
  lg <- Filter(function(t) "G3 leaf" %in% t$header && "contract" %in% t$header,
               parse_pipe_tables(ln))
  if (length(lg) != 1) return(out)
  dcol <- "governing accepted decisions"
  if (!dcol %in% lg[[1]]$header) return(out)
  for (r in lg[[1]]$rows) {
    leaf <- trimws(gv(r, "G3 leaf") %||% "")
    if (!grepl("^G3\\.[0-9A-Z]+$", leaf)) next
    out[[leaf]] <- unlist(regmatches(gv(r, dcol) %||% "",
                                     gregexpr("P[0-9]+\\.[0-9]+@[0-9a-f]{7}", gv(r, dcol) %||% "")))
  }
  out
}

# G3 leaf -> the §6 artifact number the legend assigns it. Same table again, third
# column, digits only ("10 (cache)" and "10 (host)" both yield "10"). Used by the
# agreement check to verify that a row naming "artifact N (G3.X)" pairs an
# artifact with the leaf the legend actually puts there.
psd_legend_artifacts <- function(ln) {
  out <- list()
  lg <- Filter(function(t) "G3 leaf" %in% t$header && "contract" %in% t$header,
               parse_pipe_tables(ln))
  if (length(lg) != 1) return(out)
  acol <- "§6 artifact"
  if (!acol %in% lg[[1]]$header) return(out)
  for (r in lg[[1]]$rows) {
    leaf <- trimws(gv(r, "G3 leaf") %||% "")
    if (!grepl("^G3\\.[0-9A-Z]+$", leaf)) next
    out[[leaf]] <- gsub("[^0-9]", "", gv(r, acol) %||% "")
  }
  out
}

# Roster cell -> its raw "owning contract(s)" text, parsed out of the roster
# half's lines. The agreement section (invariant half) asserts things ABOUT
# roster rows, so it cannot be checked without them. psd_roster_owners(), below
# the live run's derived sets, memoizes this over the tree.
psd_roster_cells <- function(ln) {
  out <- list()
  for (tb in parse_pipe_tables(ln)) {
    key <- tb$header[[1]]
    if (!key %in% c("export", "field", "item")) next
    own <- if ("owning contract(s)" %in% tb$header) "owning contract(s)" else
      if ("owning contract" %in% tb$header) "owning contract" else next
    for (r in tb$rows) {
      cell <- gsub("`", "", trimws(gv(r, key) %||% ""), fixed = TRUE)
      if (nzchar(cell)) out[[cell]] <- gv(r, own) %||% ""
    }
  }
  out
}

# English number words the agreement column may use in place of a digit. Kept
# small on purpose: a count phrase the checker cannot resolve must not silently
# become an unchecked claim, so anything outside this set fails the lookup and
# the row falls back to needing a resolvable term for its anchor.
PSC_NUMWORDS <- c(one = 1L, two = 2L, three = 3L, four = 4L, five = 5L,
                  six = 6L, seven = 7L, eight = 8L, nine = 9L, ten = 10L)
psc_as_count <- function(x) {
  x <- tolower(trimws(x))
  if (grepl("^[0-9]+$", x)) return(as.integer(x))
  if (x %in% names(PSC_NUMWORDS)) return(PSC_NUMWORDS[[x]])
  NA_integer_
}

# A per-predicate accumulator with check()'s contract, minus the `<<-`: the
# predicate RETURNS what it found -- its failure messages, with the number of
# checks it evaluated as attr "checks" -- and the live run feeds that into the
# global accumulator through absorb(), one pass per check that raised nothing.
# That is what lets --self-test run sections F, F2 and G on in-memory fixtures
# and prove each rule goes red on a mutation (RUL-009).
psd_acc <- function() {
  a <- new.env(parent = emptyenv())
  a$n <- 0L
  a$msgs <- character(0)
  a$check <- function(cond, msg) {
    a$n <- a$n + 1L
    if (!isTRUE(cond)) a$msgs <- c(a$msgs, msg)
    invisible(NULL)
  }
  a$result <- function() structure(a$msgs, checks = a$n)
  a
}

## F. artifact 4, invariant half — the ownership rule itself ---------------------
## The G3-pinned file must state I1-I5 and the legend, and must NOT contain a
## roster table: the split is enforced structurally, not just described in a
## comment, so a well-meaning future edit cannot quietly re-merge the halves
## and restore the cascade.
##   ln              lines of public-surface-closure.md
##   cells           roster cell -> owner text (psd_roster_cells() of the roster)
##   fields          the derived public-output-field set (.spu_result_fields)
##   contract_exists function(<contract filename>) -> logical
##   adr_exists      function(<four-digit ADR number>) -> logical
psd_closure_failures <- function(ln, cells, fields, contract_exists, adr_exists) {
  a <- psd_acc(); check <- a$check
  tabs <- parse_pipe_tables(ln)
  for (h in list(PSD_EXPORT_HDR, PSD_FIELD_HDR, PSD_ITEM_HDR))
    check(!any(vapply(tabs, function(t) identical(t$header, h), TRUE)),
          sprintf("public-surface-closure: roster table [%s] belongs in public-surface-disposition.md (P0.6)",
                  paste(h, collapse = " | ")))
  for (i in sprintf("I%d", 1:5))
    check(any(grepl(sprintf("\\*\\*%s ", i), ln, fixed = FALSE)),
          sprintf("public-surface-closure: invariant clause %s is not stated", i))
  check(!any(grepl("^##\\s+Bijection", ln)),
        "public-surface-closure: a Bijection (count) table must not live in the gate-pinned invariant (P0.6 I1)")
  lg <- psd_legend(ln)
  check(length(lg) >= 8L,
        sprintf("public-surface-closure: owning-contract legend must map at least 8 G3 leaves (got %d)",
                length(lg)))
  for (leaf in names(lg))
    check(contract_exists(lg[[leaf]]),
          sprintf("public-surface-closure: legend leaf %s names no contract file (%s)", leaf, lg[[leaf]]))

  ## F2. the cross-artifact agreement rows (PS s1) --------------------------
  ## The section claims the roster's VOCABULARY agrees with the owning
  ## contracts; until RURL-jzgelfto nothing read it, so the eight TR-PS-s1-*
  ## claims were assertion-only. The trap the ticket names is real: every row
  ## is SETTLED today, so a status-shaped check certifies nothing. These four
  ## predicates are therefore all about RESOLUTION against derived facts —
  ## the legend, the roster half, NAMESPACE, .spu_result_fields — and each
  ## was proved to go red by mutation before being trusted green.
  agr <- Filter(function(t) identical(t$header,
                                      c("shared concept", "canonical owner", "agreement", "status")),
                tabs)
  check(length(agr) == 1,
        "public-surface-closure: cross-artifact agreement table (shared concept | canonical owner | agreement | status) not found")
  if (length(agr) == 1) {
    lga <- psd_legend_artifacts(ln)
    check(length(cells) >= 1,
          "public-surface-closure: could not read the roster half's cells to check the agreement rows")
    for (r in agr[[1]]$rows) {
      concept <- gv(r, "shared concept") %||% ""
      owner <- gv(r, "canonical owner") %||% ""
      agree <- gv(r, "agreement") %||% ""
      st <- trimws(gv(r, "status") %||% "")
      lab <- if (nzchar(concept)) concept else owner
      check(st %in% c("SETTLED", "OPEN"),
            sprintf("public-surface-closure agreement [%s]: status '%s' not in {SETTLED, OPEN}", lab, st))

      ## (a) the canonical owner resolves — leaf in the legend, and the
      ## artifact number is the one the legend puts on that leaf. A row
      ## naming a coherent-looking but wrong pair is the failure this catches.
      leaves <- unique(regmatches(owner, gregexpr("G3\\.[0-9A-Z]+", owner))[[1]])
      check(length(leaves) >= 1,
            sprintf("public-surface-closure agreement [%s]: canonical owner names no G3 leaf: '%s'", lab, owner))
      for (lf in leaves)
        check(lf %in% names(lg),
              sprintf("public-surface-closure agreement [%s]: owner leaf '%s' is not in the legend", lab, lf))
      anum <- regmatches(owner, regexpr("(?<=artifact )[0-9]+", owner, perl = TRUE))
      adrs <- unique(regmatches(owner, gregexpr("ADR [0-9]{4}", owner))[[1]])
      check(length(anum) == 1 || length(adrs) >= 1,
            sprintf("public-surface-closure agreement [%s]: canonical owner names neither a §6 artifact nor an ADR: '%s'",
                    lab, owner))
      if (length(anum) == 1 && length(leaves) >= 1) {
        expected <- unique(unlist(lga[leaves]))
        check(length(expected) >= 1 && anum %in% expected,
              sprintf("public-surface-closure agreement [%s]: owner says artifact %s but the legend puts %s on artifact %s",
                      lab, anum, paste(leaves, collapse = "/"),
                      if (length(expected)) paste(expected, collapse = "/") else "<none>"))
      }
      for (ad in adrs)
        check(adr_exists(sub("ADR ", "", ad)),
              sprintf("public-surface-closure agreement [%s]: cites %s, which is not a file in design/adr", lab, ad))

      ## (b) every term the row names is a real roster cell. A trailing `*`
      ## is a glob over cell names and must match at least one — the glob
      ## `rurl_cache_*` silently matched two of the three cache surfaces
      ## because the third is spelled `rurl_clear_caches` (RURL-jzgelfto).
      raw <- unlist(regmatches(c(concept, agree), gregexpr("`[^`]+`", c(concept, agree))))
      terms <- unique(gsub("`", "", raw, fixed = TRUE))
      terms <- terms[grepl("^[A-Za-z_][A-Za-z0-9_]*\\*?$", terms)]
      matched <- character(0)
      for (tm in terms) {
        hits <- if (grepl("\\*$", tm))
          grep(sprintf("^%s", sub("\\*$", "", tm)), names(cells), value = TRUE)
        else intersect(tm, names(cells))
        check(length(hits) >= 1,
              sprintf("public-surface-closure agreement [%s]: term '%s' names no roster cell", lab, tm))
        matched <- union(matched, hits)
      }

      ## (c) at least one cell the row names is owned by the leaf the row
      ## names. Rows legitimately mention a neighbouring contract's cell for
      ## contrast (`clean_url` in the key row), so this is "at least one",
      ## not "all" — but it still fails a row assigned to the wrong owner,
      ## which (a) and (b) both pass.
      if (length(matched)) {
        linked <- vapply(matched, function(cl)
          any(vapply(leaves, function(lf) grepl(lf, cells[[cl]], fixed = TRUE), TRUE)), TRUE)
        check(any(linked),
              sprintf("public-surface-closure agreement [%s]: none of the roster cells it names (%s) is owned by %s",
                      lab, paste(matched, collapse = ", "), paste(leaves, collapse = "/")))
      }

      ## (d) a transcribed count must equal the derived one. I1 bans counts
      ## from artifact 4 for exactly this reason; the two shapes this section
      ## actually uses are checked rather than trusted. A count phrase whose
      ## number is unreadable resolves to NA and fails here rather than
      ## passing silently.
      counted <- 0L
      pf <- regmatches(concept, regexpr("(?i)(?<=\\bthe )\\S+(?= public fields)", concept, perl = TRUE))
      if (length(pf) == 1) {
        counted <- counted + 1L
        check(identical(psc_as_count(pf), length(fields)),
              sprintf("public-surface-closure agreement [%s]: says '%s public fields' but .spu_result_fields has %d (I1)",
                      lab, pf, length(fields)))
      }
      nr <- regmatches(agree, regexpr("(?i)(?<=\\bthe )\\S+(?=\\s[^|]*\\brows\\b)", agree, perl = TRUE))
      if (length(nr) == 1 && !is.na(psc_as_count(nr))) {
        counted <- counted + 1L
        check(identical(psc_as_count(nr), length(matched)),
              sprintf("public-surface-closure agreement [%s]: says '%s ... rows' but its terms name %d roster cell(s)",
                      lab, nr, length(matched)))
      }

      ## (e) no row is inert. A row that neither names a resolvable cell nor
      ## carries a verified count has nothing this validator can falsify, so
      ## it would sit here SETTLED and unchecked — the nullity P0.9 §6 warns
      ## about, and the reason this whole block exists.
      check(length(matched) >= 1 || counted >= 1,
            sprintf("public-surface-closure agreement [%s]: row is inert — it names no roster cell and carries no checkable count",
                    lab))
    }
  }
  a$result()
}

## G. artifact 4, roster half — I1-I5 over the per-cell rows -------------------
## Not a gate input (P0.6). Everything here is DERIVED: the export set from
## NAMESPACE, the field set from .spu_result_fields, the legend from the
## invariant half. No count is transcribed, so growing the surface cannot make
## this section stale.
##   ln             lines of public-surface-disposition.md
##   inv_ln         lines of public-surface-closure.md (the legend's home)
##   exports        the derived export set (NAMESPACE)
##   fields         the derived public-output-field set (.spu_result_fields)
##   contract_lines function(<contract filename>) -> its lines, or character(0)
psd_roster_failures <- function(ln, inv_ln, exports, fields, contract_lines) {
  a <- psd_acc(); check <- a$check
  tabs <- parse_pipe_tables(ln)
  exp_tab <- Filter(function(t) identical(t$header, PSD_EXPORT_HDR), tabs)
  fld_tab <- Filter(function(t) identical(t$header, PSD_FIELD_HDR), tabs)
  itm_tab <- Filter(function(t) identical(t$header, PSD_ITEM_HDR), tabs)
  check(length(exp_tab) == 1, "public-surface-disposition: exactly one exported-function roster table")
  check(length(fld_tab) == 1, "public-surface-disposition: exactly one public-output-field roster table")
  check(length(itm_tab) == 1, "public-surface-disposition: exactly one curl/migration roster table")

  ## --- I1 + I2: population derived from source, compared BY NAME ----------
  unq <- function(x) gsub("`", "", trimws(x %||% ""), fixed = TRUE)
  if (length(exp_tab) == 1) {
    rows_e <- vapply(exp_tab[[1]]$rows, function(r) unq(gv(r, "export")), "")
    for (e in setdiff(exports, rows_e))
      check(FALSE, sprintf("public-surface-disposition: NAMESPACE exports '%s' with no roster row (I2)", e))
    for (e in setdiff(rows_e, exports))
      check(FALSE, sprintf("public-surface-disposition: roster row '%s' is not a NAMESPACE export (I2)", e))
    dup <- unique(rows_e[duplicated(rows_e)])
    check(length(dup) == 0,
          sprintf("public-surface-disposition: duplicate export row(s) %s (I2)", paste(dup, collapse = ", ")))
  }
  if (length(fld_tab) == 1) {
    rows_f <- vapply(fld_tab[[1]]$rows, function(r) unq(gv(r, "field")), "")
    src_f <- fields
    check(length(src_f) >= 1,
          "public-surface-disposition: could not derive the field set from .spu_result_fields (I1)")
    for (f in setdiff(src_f, rows_f))
      check(FALSE, sprintf("public-surface-disposition: .spu_result_fields has '%s' with no roster row (I2)", f))
    for (f in setdiff(rows_f, src_f))
      check(FALSE, sprintf("public-surface-disposition: roster row '%s' is not a .spu_result_fields entry (I2)", f))
  }
  if (length(itm_tab) == 1) {
    items <- vapply(itm_tab[[1]]$rows, function(r) unq(gv(r, "item")), "")
    check(sum(grepl("^curl-", items)) >= 1L,
          "public-surface-disposition: no curl-dependency row (§10 curl surface)")
    check(sum(items == "migration-surface") == 1L,
          "public-surface-disposition: exactly one migration-surface row required")
  }

  ## --- the Bijection table is VERIFIED against source, never pinned -------
  bij <- Filter(function(t) "surface class" %in% t$header && "count" %in% t$header, tabs)
  check(length(bij) == 1, "public-surface-disposition: bijection table (surface class | count) not found")
  if (length(bij) == 1 && length(exp_tab) == 1 && length(fld_tab) == 1 && length(itm_tab) == 1) {
    labels <- vapply(bij[[1]]$rows, function(r) tolower(gv(r, "surface class") %||% ""), "")
    nums <- vapply(bij[[1]]$rows, function(r) {
      d <- gsub("[^0-9]", "", gv(r, "count") %||% "")
      if (nzchar(d)) as.integer(d) else NA_integer_
    }, integer(1))
    tot_i <- grep("total", labels)
    comp_i <- setdiff(seq_along(labels), tot_i)
    derived <- length(exports) + length(fields) + length(itm_tab[[1]]$rows)
    check(sum(nums[comp_i], na.rm = TRUE) == derived,
          sprintf("public-surface-disposition: bijection components sum to %d but the derived surface is %d (NAMESPACE + .spu_result_fields + curl/migration rows)",
                  sum(nums[comp_i], na.rm = TRUE), derived))
    check(length(tot_i) == 1 && identical(nums[tot_i[1]], derived),
          sprintf("public-surface-disposition: bijection total is %s but the derived surface is %d",
                  if (length(tot_i) == 1) nums[tot_i[1]] else "<none>", derived))
  }

  ## --- I2/I3/I4: every row is owned, and every citation resolves ----------
  lg <- psd_legend(inv_ln)
  check(length(lg) >= 8L,
        "public-surface-disposition: could not read the owning-contract legend from the invariant half")
  psd_leg_dec <- psd_legend_decisions(inv_ln)
  check(length(psd_leg_dec) >= 8L,
        "public-surface-disposition: could not read the legend's governing-decision column")
  ctext <- list()
  for (f in unique(unlist(lg))) ctext[[f]] <- contract_lines(f)
  for (tb in c(exp_tab, fld_tab, itm_tab)) {
    key <- tb$header[[1]]
    own_col <- if ("owning contract(s)" %in% tb$header) "owning contract(s)" else "owning contract"
    for (r in tb$rows) {
      cell <- unq(gv(r, key))
      owner_raw <- gv(r, own_col) %||% ""
      st_raw <- gv(r, "status") %||% ""
      st <- sub("\\s.*$", "", trimws(st_raw))
      check(st %in% c("SETTLED", "OPEN"),
            sprintf("public-surface-disposition %s: status '%s' not in {SETTLED, OPEN}", cell, st_raw))
      leaves <- unique(regmatches(owner_raw, gregexpr("G3\\.[0-9A-Z]+", owner_raw))[[1]])
      downstream <- grepl("artifact 11|artifact 4", owner_raw)
      check(length(leaves) >= 1 || downstream,
            sprintf("public-surface-disposition %s: names no owning contract (I2): '%s'", cell, owner_raw))
      for (lf in leaves)
        check(lf %in% names(lg),
              sprintf("public-surface-disposition %s: owning contract '%s' is not in the legend (I2)", cell, lf))
      disp <- gv(r, "v3 disposition") %||% ""
      if (identical(st, "SETTLED")) {
        # I3: a SETTLED row must cite the accepted decision its owner projects
        # (P-tier ref, an ADR, or — for the migration row — its discharge).
        ok <- grepl("P[0-9]+\\.[0-9]+@[0-9a-f]{7}", disp) ||
          grepl("ADR [0-9]{4}", disp) || grepl("discharged", disp)
        check(ok, sprintf("public-surface-disposition %s: SETTLED cites no decision, ADR, or discharge (I3): '%s'",
                          cell, disp))
        # I3, second half: the citation must be one the OWNER projects. The
        # check above is shape-only, so a row could cite a decision its owning
        # contract has never heard of and still pass — which is exactly how
        # RURL-nravluqd survived (`url_key_policy` cited P3.3 while G3.K's
        # legend row listed only P3.1 and P3.2, and the contract never
        # mentioned it). Silent when a leaf lists no decisions, so the
        # artifact-11/artifact-4 rows are unaffected.
        cited <- regmatches(disp, gregexpr("P[0-9]+\\.[0-9]+@[0-9a-f]{7}", disp))[[1]]
        allowed <- unique(unlist(psd_leg_dec[leaves]))
        if (length(cited) && length(allowed)) {
          for (pd in setdiff(cited, allowed))
            check(FALSE, sprintf(
              "public-surface-disposition %s: SETTLED cites %s, which the legend does not list for %s (I3)",
              cell, pd, paste(leaves, collapse = "/")))
        }
      } else if (identical(st, "OPEN")) {
        # I3/I4: each -O id cited must EXIST in one of the owning contracts.
        # "HOST-O2/O4" is shorthand: a bare /O<n> inherits the last prefix.
        toks <- regmatches(st_raw, gregexpr("[A-Z]+-O[0-9]+|/O[0-9]+", st_raw))[[1]]
        ids <- character(0); last_pref <- NA_character_
        for (tk in toks) {
          if (grepl("^/O", tk)) {
            if (!is.na(last_pref)) ids <- c(ids, paste0(last_pref, "-", sub("^/", "", tk)))
          } else {
            ids <- c(ids, tk); last_pref <- sub("-O[0-9]+$", "", tk)
          }
        }
        named_downstream <- grepl("artifact 11|S1 s/v|RCON-08|P4 host", st_raw) || downstream
        check(length(ids) >= 1 || named_downstream,
              sprintf("public-surface-disposition %s: OPEN cites neither an open-cell id nor a named downstream artifact (I3): '%s'",
                      cell, st_raw))
        for (id in unique(ids)) {
          hay <- unlist(ctext[unique(unlist(lg[leaves]))], use.names = FALSE)
          if (length(hay) == 0) hay <- unlist(ctext, use.names = FALSE)
          check(any(grepl(id, hay, fixed = TRUE)),
                sprintf("public-surface-disposition %s: OPEN cites '%s', which appears in none of its owning contracts (%s) — dangling citation (I3)",
                        cell, id, paste(leaves, collapse = ", ")))
        }
      }
    }
  }

  ## --- I4: the roster opens nothing of its own ----------------------------
  obody2 <- section_lines(ln, "Open cells")
  check(!any(grepl("PSD-O[0-9]+", obody2)),
        "public-surface-disposition: the roster must not define open cells of its own (I4)")
  check(any(grepl("PSC-O", obody2)),
        "public-surface-disposition: ## Open cells must forward to the invariant's PSC-O groups (I4)")
  a$result()
}

## --- --self-test ---------------------------------------------------------------
## Two halves. (1) The F/F2/G predicates above, run on small in-memory fixtures:
## one positive case per rule (the unmutated fixture raises nothing under it)
## and one negative case per rule (a mutation that must raise it). A predicate
## that stays green on its mutated input is the vacuity this exists to catch
## (RUL-009). (2) --regenerate proven on a temp copy of the real inputs: run
## as a subprocess, because the checks below are top-level code keyed to the
## working directory; each scenario removes one row (or adds one export),
## asserts the gate is RED, regenerates, asserts it is GREEN, and asserts the
## file differs from its pre-regeneration bytes by exactly the one row.
if ("--self-test" %in% .vr_args) {
  ## (1) the predicates over synthetic fixtures ----------------------------
  ## The world: six exports, three public fields, an eight-leaf legend, and
  ## the two artifact-4 halves written against them. Every code path of F,
  ## F2 and G is exercised by the baseline -- the number-word count, the
  ## `*` glob, the ADR-only owner, the `/O<n>` shorthand, the named
  ## downstream OPEN row, the SETTLED-by-discharge item row.
  FX_EXPORTS <- c("alpha_fn", "beta_fn", "gamma_fn",
                  "rurl_cache_config", "rurl_cache_info", "rurl_clear_caches")
  FX_FIELDS <- c("host", "path", "query")
  FX_ADRS <- "0006"
  FX_CONTRACTS <- list(
    "c3.md" = "# c3", "c5.md" = "# c5", "c6.md" = "# c6",
    "c7.md" = c("# c7", "- OUT-O1 undivided userinfo"),
    "c8.md" = "# c8", "c9.md" = "# c9",
    "ch.md" = c("# ch", "- HOST-O2 de-overload", "- HOST-O4 reproducibility"),
    "ck.md" = "# ck"
  )
  FX_CLOSURE <- c(
    "# Public-surface closure (fixture)",
    "",
    "## The closure invariant",
    "",
    "**I1 — Population is derived, never transcribed.** text",
    "**I2 — Every cell is owned.** text",
    "**I3 — Every disposition is SETTLED or OPEN.** text",
    "**I4 — No open question is invented in artifact 4.** text",
    "**I5 — The Stage-A internals stay out.** text",
    "",
    "## Owning-contract legend",
    "",
    "| G3 leaf | contract | §6 artifact | governing accepted decisions |",
    "|---|---|---|---|",
    "| G3.3 | c3 | 3 | P1.1@aaaaaaa |",
    "| G3.5 | c5 | 5 | P2.4@aaaaaaa |",
    "| G3.6 | c6 | 6 | P2.1@aaaaaaa |",
    "| G3.7 | c7 | 7 | P2.2@aaaaaaa |",
    "| G3.8 | c8 | 8 | P2.2@aaaaaaa |",
    "| G3.9 | c9 | 10 (cache) | P5.1@aaaaaaa |",
    "| G3.H | ch | 10 (host) | P4.1@aaaaaaa |",
    "| G3.K | ck | 9 | P3.1@aaaaaaa |",
    "| — | verification contracts (NOT a G3 leaf) | 11 | downstream |",
    "",
    "## Cross-artifact agreement",
    "",
    "| shared concept | canonical owner | agreement | status |",
    "|---|---|---|---|",
    "| the three public fields | artifact 3 (G3.3) | every field row names an artifact-3 field | SETTLED |",
    "| `alpha_fn` companion | artifact 6 (G3.6) | the `alpha_fn` row is companion | SETTLED |",
    paste("| cache semantics | artifact 10 cache (G3.9) |",
          "the three `rurl_cache_*` / `rurl_clear_caches` rows defer to G3.9 | SETTLED |"),
    "| `beta_fn` stays companion | ADR 0006 (via G3.6) | `beta_fn` never widens the parse frame | SETTLED |",
    "",
    "## Open cells",
    "",
    "- **PSC-O1 — host dispositions.** `gamma_fn` forwards to G3.H HOST-O2/O4.",
    ""
  )
  FX_ROSTER <- c(
    "# Public-surface disposition (fixture)",
    "",
    "## Bijection",
    "",
    "| surface class | count | source of truth | status |",
    "|---|---|---|---|",
    "| exported functions | 6 | NAMESPACE | SETTLED |",
    "| public output fields | 3 | .spu_result_fields | SETTLED |",
    "| curl-dependency surfaces | 1 | DESCRIPTION | SETTLED |",
    "| migration-surface | 1 | this roster | SETTLED |",
    "| **total** | **11** | | |",
    "",
    "## Exported-function disposition roster",
    "",
    "| export | owning contract(s) | v3 disposition | status |",
    "|---|---|---|---|",
    "| `alpha_fn` | G3.6 | companion diagnostic (P2.1@aaaaaaa; ADR 0006) | SETTLED |",
    "| `beta_fn` | G3.6 (+ G3.7) | stays companion (ADR 0006) | SETTLED |",
    "| `gamma_fn` | G3.H | host surface; de-overload → HOST-O2, reproducibility → HOST-O4 | OPEN (HOST-O2/O4) |",
    "| `rurl_cache_config` | G3.9 | cache config (P5.1@aaaaaaa) | SETTLED |",
    "| `rurl_cache_info` | G3.9 | cache info (P5.1@aaaaaaa) | SETTLED |",
    "| `rurl_clear_caches` | G3.9 (+ artifact 11) | clearing budget → §6 artifact 11 | OPEN (artifact 11) |",
    "",
    "## Public-output-field disposition roster",
    "",
    "| field | owning contract(s) | v3 disposition | status |",
    "|---|---|---|---|",
    "| `host` | G3.3 (+ G3.H) | presentation host (P1.1@aaaaaaa) | SETTLED |",
    "| `path` | G3.3 | path projection (P1.1@aaaaaaa) | SETTLED |",
    "| `query` | G3.3 (+ G3.7) | three-valued presence; undivided form → OUT-O1 | OPEN (OUT-O1) |",
    "",
    "## curl-dependency + migration-surface disposition",
    "",
    "| item | owning contract | v3 disposition | status |",
    "|---|---|---|---|",
    "| `curl-import` | §6 artifact 11 | removal downstream (RCON-09) | OPEN (artifact 11 / G4) |",
    "| `migration-surface` | artifact 4 (this roster) | **discharged**: every cell maps to an owner | SETTLED |",
    "",
    "## Open cells",
    "",
    "No open cell is invented here; every OPEN row forwards to the invariant",
    "half's **PSC-O1** group (`public-surface-closure.md`, `## Open cells`).",
    ""
  )
  closure_run <- function(cl = FX_CLOSURE, ro = FX_ROSTER, cells = psd_roster_cells(ro),
                          fields = FX_FIELDS, contracts = FX_CONTRACTS, adrs = FX_ADRS) {
    psd_closure_failures(cl, cells = cells, fields = fields,
                         contract_exists = function(f) f %in% names(contracts),
                         adr_exists = function(n) n %in% adrs)
  }
  roster_run <- function(ro = FX_ROSTER, cl = FX_CLOSURE, exports = FX_EXPORTS,
                         fields = FX_FIELDS, contracts = FX_CONTRACTS) {
    psd_roster_failures(ro, inv_ln = cl, exports = exports, fields = fields,
                        contract_lines = function(f)
                          if (f %in% names(contracts)) contracts[[f]] else character(0))
  }
  # Fixture mutators. Fixed-string matching throughout: the fixtures are full
  # of `|`, `*` and `(`, and a mutation that silently matched nothing would be
  # a positive case wearing a negative case's label, so a miss is an error.
  sub_line <- function(x, pattern, replacement) {
    k <- grep(pattern, x, fixed = TRUE)
    if (length(k) < 1L) stop("self-test fixture: no line contains ", pattern, call. = FALSE)
    x[[k[[1L]]]] <- sub(pattern, replacement, x[[k[[1L]]]], fixed = TRUE)
    x
  }
  drop_line <- function(x, pattern) {
    k <- grep(pattern, x, fixed = TRUE)
    if (length(k) < 1L) stop("self-test fixture: no line contains ", pattern, call. = FALSE)
    x[-k[[1L]]]
  }
  add_after <- function(x, pattern, lines) {
    k <- grep(pattern, x, fixed = TRUE)
    if (length(k) < 1L) stop("self-test fixture: no line contains ", pattern, call. = FALSE)
    append(x, lines, after = k[[1L]])
  }

  st <- new.env(parent = emptyenv())
  st$pos <- 0L; st$neg <- 0L; st$failed <- character(0)
  st_check <- function(label, ok, kind) {
    if (isTRUE(ok)) {
      if (identical(kind, "positive")) st$pos <- st$pos + 1L else st$neg <- st$neg + 1L
    } else {
      st$failed <- c(st$failed, label)
      cat("  FAIL: ", label, "\n", sep = "")
    }
    invisible(NULL)
  }
  # One rule = one positive case (the baseline raises nothing matching
  # `pattern`) + one negative case (the mutated run raises it).
  base_closure <- closure_run()
  base_roster <- roster_run()
  rule <- function(label, pattern, mutated, base) {
    st_check(paste0(label, " -- baseline green"), !any(grepl(pattern, base, fixed = TRUE)), "positive")
    st_check(paste0(label, " -- mutation red"), any(grepl(pattern, mutated, fixed = TRUE)), "negative")
  }
  green <- function(label, got, pattern = NULL) {
    ok <- if (is.null(pattern)) length(got) == 0L else !any(grepl(pattern, got, fixed = TRUE))
    st_check(label, ok, "positive")
  }

  green("F/F2: closure baseline raises nothing", base_closure)
  green("G: roster baseline raises nothing", base_roster)

  ## -- F: the invariant half --------------------------------------------
  rule("F roster-table absent", "roster table [export | owning contract(s)", base = base_closure,
       closure_run(cl = add_after(FX_CLOSURE, "## Open cells",
                                  c("| export | owning contract(s) | v3 disposition | status |",
                                    "|---|---|---|---|", "| `x` | G3.3 | y | SETTLED |"))))
  rule("F I1-I5 stated", "invariant clause I3 is not stated", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "**I3 —", "**J3 —")))
  rule("F no Bijection heading", "Bijection (count) table must not live", base = base_closure,
       closure_run(cl = add_after(FX_CLOSURE, "## Open cells", "## Bijection")))
  rule("F legend size", "legend must map at least 8 G3 leaves (got 7)", base = base_closure,
       closure_run(cl = drop_line(FX_CLOSURE, "| G3.K | ck |")))
  rule("F legend leaf's contract exists", "legend leaf G3.K names no contract file (missing.md)",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| G3.K | ck |", "| G3.K | missing |")))
  green("F: the legend's non-leaf row (—) is not counted as a leaf",
        closure_run(cl = add_after(FX_CLOSURE, "| — | verification",
                                   "| — | more prose (NOT a G3 leaf) | 12 | none |")))

  ## -- F2: the agreement rows ---------------------------------------------
  rule("F2 agreement table present",
       "agreement table (shared concept | canonical owner | agreement | status) not found",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| shared concept | canonical owner |", "| shared concept | owner |")))
  rule("F2 roster cells readable", "could not read the roster half's cells", base = base_closure,
       closure_run(cells = list()))
  rule("F2 status vocabulary", "agreement [the three public fields]: status 'DONE' not in {SETTLED, OPEN}",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "artifact-3 field | SETTLED |", "artifact-3 field | DONE |")))
  rule("F2(a) owner names a G3 leaf", "agreement [`alpha_fn` companion]: canonical owner names no G3 leaf",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| artifact 6 (G3.6) |", "| artifact 6 |")))
  rule("F2(a) owner leaf is in the legend", "owner leaf 'G3.Z' is not in the legend", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| artifact 6 (G3.6) |", "| artifact 6 (G3.Z) |")))
  rule("F2(a) owner names an artifact or an ADR", "canonical owner names neither a §6 artifact nor an ADR",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| artifact 6 (G3.6) |", "| (G3.6) |")))
  rule("F2(a) artifact number matches the legend", "owner says artifact 7 but the legend puts G3.6 on artifact 6",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| artifact 6 (G3.6) |", "| artifact 7 (G3.6) |")))
  rule("F2(a) cited ADR is a file", "cites ADR 0099, which is not a file in design/adr", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| ADR 0006 (via G3.6) |", "| ADR 0099 (via G3.6) |")))
  rule("F2(b) term names a roster cell", "term 'omega_fn' names no roster cell", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| `alpha_fn` companion |", "| `omega_fn` companion |")))
  rule("F2(b) glob term matches at least one cell", "term 'rurl_nope_*' names no roster cell",
       base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "`rurl_cache_*`", "`rurl_nope_*`")))
  rule("F2(c) a named cell is owned by the named leaf",
       "none of the roster cells it names (alpha_fn) is owned by G3.3", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "| artifact 6 (G3.6) |", "| artifact 3 (G3.3) |")))
  rule("F2(d) 'the N public fields' equals the derived count",
       "says 'two public fields' but .spu_result_fields has 3 (I1)", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "the three public fields", "the two public fields")))
  rule("F2(d) an unreadable numeral fails rather than passes",
       "says 'several public fields' but .spu_result_fields has 3 (I1)", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "the three public fields", "the several public fields")))
  green("F2(d) a digit numeral resolves like a number word",
        closure_run(cl = sub_line(FX_CLOSURE, "the three public fields", "the 3 public fields")))
  rule("F2(d) 'the N ... rows' equals the cells its terms name",
       "says 'two ... rows' but its terms name 3 roster cell(s)", base = base_closure,
       closure_run(cl = sub_line(FX_CLOSURE, "the three `rurl_cache_*`", "the two `rurl_cache_*`")))
  rule("F2(e) no inert row", "agreement [prose only]: row is inert", base = base_closure,
       closure_run(cl = add_after(FX_CLOSURE, "| `beta_fn` stays companion |",
                                  "| prose only | artifact 3 (G3.3) | nothing this validator can falsify | SETTLED |")))

  ## -- G: the roster half ---------------------------------------------------
  rule("G exactly one export table", "exactly one exported-function roster table", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| export | owning contract(s) |", "| exports | owning contract(s) |")))
  rule("G exactly one field table", "exactly one public-output-field roster table", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| field | owning contract(s) |", "| fields | owning contract(s) |")))
  rule("G exactly one item table", "exactly one curl/migration roster table", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| item | owning contract |", "| items | owning contract |")))
  rule("G I2 every export has a roster row", "NAMESPACE exports 'beta_fn' with no roster row (I2)",
       base = base_roster, roster_run(ro = drop_line(FX_ROSTER, "| `beta_fn` |")))
  rule("G I2 every export row is a NAMESPACE export", "roster row 'zeta_fn' is not a NAMESPACE export (I2)",
       base = base_roster,
       roster_run(ro = add_after(FX_ROSTER, "| `rurl_clear_caches` |",
                                 "| `zeta_fn` | G3.6 | x (ADR 0006) | SETTLED |")))
  rule("G I2 no duplicate export row", "duplicate export row(s) alpha_fn (I2)", base = base_roster,
       roster_run(ro = add_after(FX_ROSTER, "| `alpha_fn` |", "| `alpha_fn` | G3.6 | again (ADR 0006) | SETTLED |")))
  rule("G I1 the field set is derivable", "could not derive the field set from .spu_result_fields (I1)",
       base = base_roster, roster_run(fields = character(0)))
  rule("G I2 every field has a roster row", ".spu_result_fields has 'path' with no roster row (I2)",
       base = base_roster, roster_run(ro = drop_line(FX_ROSTER, "| `path` |")))
  rule("G I2 every field row is a .spu_result_fields entry",
       "roster row 'extra' is not a .spu_result_fields entry (I2)", base = base_roster,
       roster_run(ro = add_after(FX_ROSTER, "| `query` |", "| `extra` | G3.3 | x (P1.1@aaaaaaa) | SETTLED |")))
  rule("G a curl-dependency row exists", "no curl-dependency row (§10 curl surface)", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| `curl-import` |", "| `kurl-import` |")))
  rule("G exactly one migration-surface row", "exactly one migration-surface row required", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| `migration-surface` |", "| `migration-surfaces` |")))
  rule("G bijection table present", "bijection table (surface class | count) not found", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| surface class | count |", "| surface klass | count |")))
  rule("G bijection components sum to the derived surface",
       "bijection components sum to 12 but the derived surface is 11", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| exported functions | 6 |", "| exported functions | 7 |")))
  rule("G bijection total equals the derived surface", "bijection total is 12 but the derived surface is 11",
       base = base_roster, roster_run(ro = sub_line(FX_ROSTER, "| **total** | **11** |", "| **total** | **12** |")))
  rule("G legend readable from the invariant half",
       "could not read the owning-contract legend from the invariant half", base = base_roster,
       roster_run(cl = character(0)))
  rule("G legend's decision column readable", "could not read the legend's governing-decision column",
       base = base_roster,
       roster_run(cl = sub_line(FX_CLOSURE, "governing accepted decisions", "decisions")))
  rule("G row status vocabulary", "public-surface-disposition alpha_fn: status 'DONE' not in {SETTLED, OPEN}",
       base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "ADR 0006) | SETTLED |", "ADR 0006) | DONE |")))
  rule("G I2 row names an owning contract", "alpha_fn: names no owning contract (I2)", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| `alpha_fn` | G3.6 |", "| `alpha_fn` | nobody |")))
  rule("G I2 owning contract is in the legend", "alpha_fn: owning contract 'G3.Z' is not in the legend (I2)",
       base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| `alpha_fn` | G3.6 |", "| `alpha_fn` | G3.Z |")))
  rule("G I3 SETTLED cites a decision, ADR or discharge",
       "alpha_fn: SETTLED cites no decision, ADR, or discharge (I3)", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "companion diagnostic (P2.1@aaaaaaa; ADR 0006)", "companion diagnostic")))
  rule("G I3 SETTLED cites a decision its owner projects",
       "alpha_fn: SETTLED cites P3.3@aaaaaaa, which the legend does not list for G3.6 (I3)", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "(P2.1@aaaaaaa; ADR 0006)", "(P3.3@aaaaaaa)")))
  green("G I3 is silent when the owner's legend row lists no decisions",
        roster_run(cl = sub_line(FX_CLOSURE, "| G3.6 | c6 | 6 | P2.1@aaaaaaa |", "| G3.6 | c6 | 6 | — |")),
        pattern = "which the legend does not list")
  rule("G I3 OPEN cites an open-cell id or a named downstream artifact",
       "gamma_fn: OPEN cites neither an open-cell id nor a named downstream artifact (I3)", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| OPEN (HOST-O2/O4) |", "| OPEN (later) |")))
  rule("G I3 an OPEN citation resolves in an owning contract",
       "gamma_fn: OPEN cites 'HOST-O9', which appears in none of its owning contracts (G3.H)", base = base_roster,
       roster_run(ro = sub_line(FX_ROSTER, "| OPEN (HOST-O2/O4) |", "| OPEN (HOST-O9) |")))
  green("G I3 the /O<n> shorthand inherits the prefix and resolves", base_roster, pattern = "HOST-O4")
  rule("G I3 the /O<n> shorthand is checked, not skipped",
       "gamma_fn: OPEN cites 'HOST-O4', which appears in none of its owning contracts (G3.H)", base = base_roster,
       roster_run(contracts = modifyList(FX_CONTRACTS, list("ch.md" = c("# ch", "- HOST-O2 only")))))
  rule("G I4 the roster opens no cell of its own", "must not define open cells of its own (I4)",
       base = base_roster, roster_run(ro = add_after(FX_ROSTER, "half's **PSC-O1** group", "- PSD-O1 invented here")))
  rule("G I4 the roster forwards to PSC-O groups", "## Open cells must forward to the invariant's PSC-O groups (I4)",
       base = base_roster, roster_run(ro = sub_line(FX_ROSTER, "**PSC-O1** group", "**PSX-O1** group")))

  cat(sprintf("validate-records.R --self-test: predicates F/F2/G: %d cases (%d positive, %d negative), %d failed\n",
              st$pos + st$neg + length(st$failed), st$pos, st$neg, length(st$failed)))
  if (length(st$failed)) quit(status = 1L)

  ## (2) --regenerate proven on a copy of the real inputs ------------------
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]]))
  run <- function(dir, flag = "") {
    out <- suppressWarnings(system2(
      "sh", c("-c", shQuote(sprintf("cd %s && exec Rscript %s %s",
                                    shQuote(dir), shQuote(script), flag))),
      stdout = TRUE, stderr = TRUE
    ))
    list(status = attr(out, "status") %||% 0L, out = out)
  }
  # Every line of `before` survives in order, and these are the extra lines.
  lines_added <- function(before, after) {
    i <- 1L; extra <- character(0)
    for (ln in after) {
      if (i <= length(before) && identical(ln, before[[i]])) i <- i + 1L
      else extra <- c(extra, ln)
    }
    if (i <= length(before)) NULL else extra
  }
  fixture <- function(tag) {
    d <- file.path(tempfile("validate-records-selftest-"), tag)
    dir.create(file.path(d, "R"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(d, "design", "work"), recursive = TRUE, showWarnings = FALSE)
    file.copy("NAMESPACE", d)
    file.copy(file.path("R", "utils.R"), file.path(d, "R"))
    file.copy(file.path("design", "adr"), file.path(d, "design"), recursive = TRUE)
    file.copy(file.path("design", "work", "url-v3"), file.path(d, "design", "work"),
              recursive = TRUE)
    d
  }
  inv <- function(d) file.path(d, "design/work/url-v3/registers/public-surface-inventory.md")
  dis <- function(d) file.path(d, "design/work/url-v3/contracts/public-surface-disposition.md")
  drop_row <- function(path, pattern) {
    ln <- readLines(path, warn = FALSE)
    hit <- grep(pattern, ln)
    if (length(hit) != 1L) fail(sprintf("fixture row `%s` not unique in %s", pattern, basename(path)))
    writeLines(ln[-hit], path, useBytes = TRUE)
  }
  cycle <- function(d, tag) {
    b_inv <- readLines(inv(d), warn = FALSE); b_dis <- readLines(dis(d), warn = FALSE)
    if (run(d)$status == 0L) fail(sprintf("[%s] the gate is not red before --regenerate", tag))
    r <- run(d, "--regenerate")
    if (r$status != 0L) fail(sprintf("[%s] --regenerate exited %d:\n%s", tag, r$status, paste(r$out, collapse = "\n")))
    g <- run(d)
    if (g$status != 0L) fail(sprintf("[%s] the gate is still red after --regenerate:\n%s", tag,
                                     paste(grep("^  - ", g$out, value = TRUE), collapse = "\n")))
    list(inv = lines_added(b_inv, readLines(inv(d), warn = FALSE)),
         dis = lines_added(b_dis, readLines(dis(d), warn = FALSE)),
         out = r$out)
  }
  one_row <- function(x, prefix, what) {
    if (is.null(x)) fail(sprintf("%s: an existing line was rewritten or reordered", what))
    if (length(x) != 1L || !startsWith(x, prefix))
      fail(sprintf("%s: diff is not exactly the one stub row (%s): %s", what, prefix,
                   paste(x, collapse = " / ")))
  }

  # A: an export row missing from both files (the row the register lost).
  d <- fixture("export")
  drop_row(inv(d), "^\\| url_semi_join \\|")
  drop_row(dis(d), "^\\| `url_semi_join` \\|")
  r <- cycle(d, "export")
  one_row(r$inv, "| url_semi_join | exported-function | TODO |", "inventory export")
  one_row(r$dis, "| `url_semi_join` | TODO", "disposition export")
  a <- readLines(inv(d), warn = FALSE)
  if (grep("^\\| url_semi_join \\|", a) != grep("^\\| url_right_join \\|", a) + 1L)
    fail("inventory export stub was not inserted in alphabetical position")
  if (!any(grepl("public-surface-inventory.md: + export row `url_semi_join`", r$out, fixed = TRUE)))
    fail("--regenerate did not print the inventory row it added")

  # B: a public-output-field row missing from both files.
  d <- fixture("field")
  drop_row(inv(d), "^\\| tld_unicode \\|")
  drop_row(dis(d), "^\\| `tld_unicode` \\|")
  r <- cycle(d, "field")
  one_row(r$inv, "| tld_unicode | public-output-field | TODO |", "inventory field")
  one_row(r$dis, "| `tld_unicode` | TODO", "disposition field")

  # C: the way the gap arrives in practice -- a NEW export, so both files lack
  # the row AND the roster's bijection counts are stale by one.
  #
  # The two expected counts below are LITERALS on purpose: deriving them from
  # the tree would make this case tautological -- it would pass even if
  # --regenerate wrote nothing. The cost is that they are the real roster's
  # counts plus one, so ADDING A REAL EXPORT MOVES THEM. When this case fails
  # with "roster diff is not {row, exports count, total}" right after a new
  # export landed, that is what happened: bump both, do not weaken the check.
  # Note this case does not run locally unless the diff touches a gate
  # implementation, so CI is usually where it first goes red (RUL-021).
  d <- fixture("new-export")
  cat("export(zz_probe_export)\n", file = file.path(d, "NAMESPACE"), append = TRUE)
  b_dis <- readLines(dis(d), warn = FALSE)
  r <- cycle(d, "new-export")
  one_row(r$inv, "| zz_probe_export | exported-function | TODO |", "inventory new export")
  a <- readLines(dis(d), warn = FALSE)
  changed <- setdiff(a, b_dis)
  if (length(changed) != 3L ||
        sum(startsWith(changed, "| `zz_probe_export` | TODO")) != 1L ||
        !any(grepl("^\\| exported functions \\| 42 \\|", changed)) ||
        !any(grepl("^\\| \\*\\*total\\*\\* \\| \\*\\*64\\*\\* \\|", changed)))
    fail(sprintf("new-export: roster diff is not {row, exports count, total}: %s",
                 paste(changed, collapse = " / ")))

  # D: a no-op on the real tree leaves both files byte-identical.
  d <- fixture("noop")
  b_inv <- readLines(inv(d), warn = FALSE); b_dis <- readLines(dis(d), warn = FALSE)
  r <- run(d, "--regenerate")
  if (r$status != 0L || !identical(readLines(inv(d), warn = FALSE), b_inv) ||
        !identical(readLines(dis(d), warn = FALSE), b_dis))
    fail("--regenerate touched a tree that had nothing to regenerate")

  cat(sprintf(paste0("validate-records.R --self-test: PASS (%d predicate cases: %d positive,",
                     " %d negative; 3 regenerate scenarios + 1 no-op)\n"),
              st$pos + st$neg, st$pos, st$neg))
  quit(status = 0L)
}

root <- "design/work/url-v3"
sdir <- file.path(root, "schema")
lifecycle <- yaml::read_yaml(file.path(sdir, "lifecycle.yaml"))
rschemas  <- yaml::read_yaml(file.path(sdir, "record-schemas.yaml"))

fail <- character(0); pass <- 0L
check <- function(cond, msg) if (isTRUE(cond)) pass <<- pass + 1L else fail <<- c(fail, msg)
# The artifact-4 predicates (psd_closure_failures, psd_roster_failures) RETURN
# their findings; this feeds them into the same accumulator, one pass per
# evaluated check that raised nothing, so the count and output are unchanged.
absorb <- function(res) {
  pass <<- pass + attr(res, "checks") - length(res)
  fail <<- c(fail, as.character(res))
}
valid_states <- names(lifecycle$states)

read_frontmatter <- function(path) {
  ln <- readLines(path, warn = FALSE)
  idx <- which(ln == "---")
  if (length(idx) < 2) return(NULL)
  yaml::yaml.load(paste(ln[(idx[1] + 1):(idx[2] - 1)], collapse = "\n"))
}

## --- owner-decision records ------------------------------------------------
dec <- rschemas$record_types$`owner-decision`
dec_files <- Sys.glob(dec$path_glob)
records <- list()
for (f in dec_files) {
  fm <- read_frontmatter(f)
  check(!is.null(fm), sprintf("owner-decision: no frontmatter: %s", f))
  if (is.null(fm)) next
  # The required-field sweep is gone with the seal (ADR 0014): it existed to
  # demand `accepted_at`/`accepted_evidence`/`approver`/`authority`, which no
  # longer mean anything now that "merged to main" IS acceptance. Frontmatter is
  # deliberately NOT rewritten across the 28 records -- the fields are harmless
  # history; nothing reads them.
  id <- as.character(fm$id %||% "")
  check(grepl(dec$id_pattern, id),
        sprintf("%s: id '%s' fails pattern %s", basename(f), id, dec$id_pattern))
  st <- as.character(fm$state %||% "")
  # unknown_states
  check(st %in% valid_states, sprintf("[unknown_states] %s: state '%s'", basename(f), st))
  check(st %in% dec$allowed_states,
        sprintf("%s: state '%s' not allowed for owner-decision", basename(f), st))
  records[[id]] <- list(state = st, path = f, fm = fm)
}

## --- duplicate_ids ---------------------------------------------------------
ids <- vapply(dec_files, function(f) as.character(read_frontmatter(f)$id %||% NA_character_), "")
ids <- ids[!is.na(ids)]
dups <- unique(ids[duplicated(ids)])
check(length(dups) == 0, sprintf("[duplicate_ids] %s", paste(dups, collapse = ", ")))

## --- broken_references (depends_on/supersedes/superseded_by) ---------------
known <- names(records)
for (id in known) {
  fm <- records[[id]]$fm
  for (rf in dec$reference_fields) {
    refs <- as.character(unlist(fm[[rf]] %||% character(0)))
    for (r in refs) {
      check(r %in% known,
            sprintf("[broken_references] %s: %s -> unknown id '%s'", id, rf, r))
    }
  }
}

## The owner/approver assertion for ACCEPTED records and the manifest
## hash-drift sweep both lived here. Both are retired (ADR 0014): the first
## asserted that a record the sole developer wrote was approved by the sole
## developer, and the second re-pinned every artifact's sha256 in manifest.yaml
## so that editing any of them reopened the acceptance. manifest.yaml is gone.

## --- register presence: snapshots.log columnar shape -----------------------
snap <- file.path(root, "registers", "snapshots.log")
if (file.exists(snap)) {
  rows <- readLines(snap, warn = FALSE)
  rows <- rows[nzchar(trimws(rows)) & !grepl("^#", rows)]
  check(length(rows) >= 1, "snapshots.log: no binding rows")
  for (rw in rows) {
    cols <- strsplit(trimws(rw), "[[:space:]]+")[[1]]
    check(length(cols) == 4,
          sprintf("snapshots.log: row is not 4 columns: '%s'", rw))
  }
}

## --- G1.1 registers: finding (RCON) + source-claim (S) -----------------------
## Parses the two Markdown row-registers and enforces the reconciliation §9
## bidirectional coverage: forward (§4 Sources, transferred into findings.md)
## must equal the inverse (source-claims target_ref + parsed §9). Every frozen
## finding is represented exactly once, multi-RCON links allowed. (RURL-jalwcgzk)
reg_dir   <- file.path(root, "registers")
recon     <- file.path(root, "protocol-review-reconciliation.md")
sc_path   <- file.path(reg_dir, "source-claims.md")
fd_path   <- file.path(reg_dir, "findings.md")
sc_pat    <- rschemas$record_types$register$variants$`source-claim`$id_pattern
fd_pat    <- rschemas$record_types$register$variants$finding$id_pattern
sc_fields <- rschemas$record_types$register$variants$`source-claim`$row_fields
fd_fields <- rschemas$record_types$register$variants$finding$row_fields
env_fields <- yaml::read_yaml(file.path(sdir, "envelope.yaml"))$required_envelope_fields

# Read the pipe-table under the "## Rows" heading of a register file.
read_rows <- function(path) {
  ln <- readLines(path, warn = FALSE)
  h  <- which(grepl("^##\\s+Rows\\s*$", ln))
  if (length(h) != 1) return(NULL)
  nxt <- which(grepl("^##\\s", ln) & seq_along(ln) > h)
  end <- if (length(nxt)) min(nxt) - 1L else length(ln)
  body <- ln[(h + 1):end]
  tbl  <- body[grepl("^\\|", body)]
  if (length(tbl) < 3) return(NULL)
  header <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", tbl[1])), "\\|")[[1]])
  data <- tbl[-(1:2)]                                   # drop header + separator
  data <- data[!grepl("^\\|[-:\\s|]*$", data)]
  rows <- lapply(data, function(r) {
    cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", r)), "\\|")[[1]])
    stats::setNames(cells, header[seq_along(cells)])
  })
  list(header = header, rows = rows)
}

# Expand one "S<n> ..." report-group into canonical claim_ids.
expand_group <- function(g) {
  g <- trimws(g)
  m <- regmatches(g, regexec("^S([0-9]+)[[:space:]]+(.*)$", g))[[1]]
  if (length(m) < 3) stop(sprintf("unparseable report group: '%s'", g))
  rn <- m[2]; rest <- m[3]
  is_finding <- grepl("finding", rest)
  rest <- gsub("findings?", "", rest)
  rest <- gsub("[[:space:]]+and[[:space:]]+", ",", rest)
  rest <- gsub("[[:space:]]+", "", rest)
  items <- strsplit(rest, ",")[[1]]; items <- items[nzchar(items)]
  out <- character(0)
  for (it in items) {
    if (rn == "2") {
      mm <- regmatches(it, regexec("^S2-([0-9]+)(-S2-([0-9]+))?$", it))[[1]]
      if (length(mm) < 2 || !nzchar(mm[2])) stop(sprintf("bad S2 item '%s'", it))
      a <- as.integer(mm[2]); b <- if (nzchar(mm[4])) as.integer(mm[4]) else a
      for (k in a:b) out <- c(out, sprintf("S2 S2-%02d", k))
    } else if (rn == "9") {
      mm <- regmatches(it, regexec("^([CHM])([0-9]+)(-([CHM])([0-9]+))?$", it))[[1]]
      if (length(mm) < 3 || !nzchar(mm[2])) stop(sprintf("bad S9 item '%s'", it))
      L <- mm[2]; a <- as.integer(mm[3]); b <- if (nzchar(mm[6])) as.integer(mm[6]) else a
      for (k in a:b) out <- c(out, sprintf("S9 %s%d", L, k))
    } else if (is_finding) {
      mm <- regmatches(it, regexec("^([0-9]+)(-([0-9]+))?$", it))[[1]]
      if (length(mm) < 2 || !nzchar(mm[2])) stop(sprintf("bad finding item '%s'", it))
      a <- as.integer(mm[2]); b <- if (nzchar(mm[4])) as.integer(mm[4]) else a
      for (k in a:b) out <- c(out, sprintf("S%s finding %d", rn, k))
    } else {
      mm <- regmatches(it, regexec("^F([0-9]+)(-F([0-9]+))?$", it))[[1]]
      if (length(mm) < 2 || !nzchar(mm[2])) stop(sprintf("bad F item '%s'", it))
      a <- as.integer(mm[2]); b <- if (nzchar(mm[4])) as.integer(mm[4]) else a
      for (k in a:b) out <- c(out, sprintf("S%s F%d", rn, k))
    }
  }
  out
}
expand_cell <- function(cell) {
  cell <- sub("\\(.*$", "", trimws(cell))            # drop trailing parenthetical
  parts <- strsplit(cell, ";")[[1]]; parts <- parts[nzchar(trimws(parts))]
  unique(unlist(lapply(parts, expand_group)))
}

sc_reg <- if (file.exists(sc_path)) read_rows(sc_path) else NULL
fd_reg <- if (file.exists(fd_path)) read_rows(fd_path) else NULL
check(!is.null(sc_reg), "source-claims.md: no parseable Rows table")
check(!is.null(fd_reg), "findings.md: no parseable Rows table")

if (!is.null(sc_reg) && !is.null(fd_reg)) {
  # envelope block present in each register
  for (rf in list(list("source-claims.md", sc_path), list("findings.md", fd_path))) {
    txt <- paste(readLines(rf[[2]], warn = FALSE), collapse = "\n")
    for (ef in env_fields) {
      check(grepl(sprintf("\\|\\s*%s\\s*\\|", ef), txt),
            sprintf("[missing_required_fields] %s: envelope field '%s'", rf[[1]], ef))
    }
  }

  # --- source-claim rows: schema, id pattern, states, uniqueness --------------
  sc_rows <- sc_reg$rows
  for (f in sc_fields) check(f %in% sc_reg$header,
    sprintf("[missing_required_fields] source-claims.md: column '%s'", f))
  sc_ids <- vapply(sc_rows, function(r) gv(r, "claim_id"), "")
  for (id in sc_ids) check(grepl(sc_pat, id),
    sprintf("source-claims.md: claim_id '%s' fails %s", id, sc_pat))
  dup_sc <- unique(sc_ids[duplicated(sc_ids)])
  check(length(dup_sc) == 0, sprintf("[duplicate_ids] source-claims: %s", paste(dup_sc, collapse = ", ")))
  for (r in sc_rows) check((gv(r, "state")) %in% valid_states,
    sprintf("[unknown_states] source-claims %s: '%s'", gv(r, "claim_id"), gv(r, "state")))
  # §10 path-correction discipline: S4/S6 rows carry a correction, others "—"
  for (r in sc_rows) {
    rep <- sub("[[:space:]].*$", "", gv(r, "claim_id"))
    pc  <- gv(r, "path_correction")
    if (rep %in% c("S4", "S6")) {
      check(grepl("§10", pc), sprintf("source-claims %s: missing §10 path_correction", gv(r, "claim_id")))
    } else {
      check(identical(pc, "—"), sprintf("source-claims %s: unexpected path_correction '%s'", gv(r, "claim_id"), pc))
    }
  }
  # target_ref per claim (RCON set)
  sc_target <- list()
  for (r in sc_rows) {
    refs <- regmatches(gv(r, "target_ref"), gregexpr("RCON-[0-9]+", gv(r, "target_ref")))[[1]]
    check(length(refs) >= 1, sprintf("source-claims %s: no target_ref RCON", gv(r, "claim_id")))
    sc_target[[gv(r, "claim_id")]] <- sort(unique(refs))
  }

  # --- finding rows: schema, id pattern, disposition --------------------------
  fd_rows <- fd_reg$rows
  for (f in fd_fields) check(f %in% fd_reg$header,
    sprintf("[missing_required_fields] findings.md: column '%s'", f))
  fd_ids <- vapply(fd_rows, function(r) gv(r, "finding_id"), "")
  for (id in fd_ids) check(grepl(fd_pat, id),
    sprintf("findings.md: finding_id '%s' fails %s", id, fd_pat))
  dup_fd <- unique(fd_ids[duplicated(fd_ids)])
  check(length(dup_fd) == 0, sprintf("[duplicate_ids] findings: %s", paste(dup_fd, collapse = ", ")))
  for (r in fd_rows) check((gv(r, "disposition")) %in% valid_states,
    sprintf("[unknown_states] findings %s: '%s'", gv(r, "finding_id"), gv(r, "disposition")))

  # --- parse reconciliation §9 (ground-truth inverse) + §4 (forward) ----------
  rl <- readLines(recon, warn = FALSE)
  # §9 table rows: between the "## 9." heading and the next "## " heading.
  s9 <- which(grepl("^## 9\\.", rl)); s9e <- which(grepl("^## 10\\.", rl))
  block9 <- rl[(s9 + 1):(s9e - 1)]
  trows <- block9[grepl("^\\|", block9) & grepl("RCON-", block9)]
  gt_inv <- list()                                   # claim_id -> sorted RCON set
  for (tr in trows) {
    cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", tr)), "\\|")[[1]])
    claims <- expand_cell(cells[1])
    rcons  <- sort(unique(regmatches(cells[2], gregexpr("RCON-[0-9]+", cells[2]))[[1]]))
    for (c in claims) gt_inv[[c]] <- sort(unique(c(gt_inv[[c]], rcons)))
  }
  # §4 Sources per RCON (forward), transferred verbatim into findings.md; also
  # self-check the reconciliation's own §4 against §9.
  gt_fwd <- list(); cur <- NA_character_
  for (line in rl) {
    hm <- regmatches(line, regexec("^### (RCON-[0-9]+)", line))[[1]]
    if (length(hm) >= 2) cur <- hm[2]
    if (grepl("^\\*\\*Sources:\\*\\*", line) && !is.na(cur)) {
      gt_fwd[[cur]] <- sort(expand_cell(sub(".*\\*\\*Sources:\\*\\*", "", line)))
      cur <- NA_character_
    }
  }

  # invert §9 -> forward, and assert reconciliation §4 == inverse of §9
  inv_of_9 <- list()
  for (c in names(gt_inv)) for (rc in gt_inv[[c]]) inv_of_9[[rc]] <- sort(unique(c(inv_of_9[[rc]], c)))
  for (rc in sort(union(names(gt_fwd), names(inv_of_9)))) {
    check(identical(gt_fwd[[rc]] %||% character(0), inv_of_9[[rc]] %||% character(0)),
          sprintf("reconciliation §4 Sources for %s disagree with §9 inverse", rc))
  }

  # --- completeness: register claim set == §9 claim set (bijection) -----------
  check(setequal(sc_ids, names(gt_inv)),
        sprintf("source-claims rows != §9 findings (rows=%d, §9=%d)", length(sc_ids), length(gt_inv)))
  check(length(sc_ids) == length(gt_inv),
        sprintf("source-claims exact count mismatch: %d vs §9 %d", length(sc_ids), length(gt_inv)))

  # --- inverse check: source-claims target_ref == §9 inverse (per claim) ------
  for (c in sc_ids) {
    check(identical(sc_target[[c]] %||% character(0), gt_inv[[c]] %||% character(0)),
          sprintf("source-claims %s target_ref != §9 inverse", c))
  }

  # --- forward check: findings.md sources expand == §9 forward (per RCON) -----
  fd_src <- list()
  for (r in fd_rows) fd_src[[gv(r, "finding_id")]] <- sort(expand_cell(gv(r, "sources")))
  for (rc in sort(union(names(fd_src), names(inv_of_9)))) {
    check(identical(fd_src[[rc]] %||% character(0), inv_of_9[[rc]] %||% character(0)),
          sprintf("findings.md %s sources expand != §9 forward map", rc))
    # every expanded source resolves to an actual source-claim row (no orphan)
    for (c in fd_src[[rc]] %||% character(0))
      check(c %in% sc_ids, sprintf("findings.md %s: source '%s' has no source-claim row", rc, c))
  }
  check(length(fd_ids) == 10L, sprintf("findings register expected 10 RCON rows, got %d", length(fd_ids)))
}

## --- the public surface, DERIVED from source ---------------------------------
## Every artifact-4 check compares against these rather than against a number
## written into a document (P0.6 I1; RURL-oygqsykd). Memoized: several sections
## use them. A transcribed count is a maintenance tax and a stale-fact risk —
## the "29 exports / 51 rows" prose that survived the 51->52 growth is the proof.
.psd_cache <- new.env(parent = emptyenv())
psd_ns_exports <- function() {
  if (is.null(.psd_cache$exports)) {
    ns <- readLines("NAMESPACE", warn = FALSE)
    e <- regmatches(ns, regexpr("(?<=^export\\()[^)]+", ns, perl = TRUE))
    .psd_cache$exports <- sort(e[nzchar(e)])
  }
  .psd_cache$exports
}
psd_result_fields <- function() {
  if (is.null(.psd_cache$fields)) {
    ut <- readLines(file.path("R", "utils.R"), warn = FALSE)
    i0 <- grep("^\\.spu_result_fields\\s*<-\\s*list\\(", ut)
    f <- character(0)
    if (length(i0) == 1) {
      ends <- grep("^\\)\\s*$", ut)
      ends <- ends[ends > i0]
      if (length(ends)) {
        blk <- ut[i0:ends[[1]]]
        m <- regmatches(blk, regexpr('(?<=name = ")[^"]+', blk, perl = TRUE))
        f <- sort(m[nzchar(m)])
      }
    }
    .psd_cache$fields <- f
  }
  .psd_cache$fields
}

## --- G1.2 register: public-surface-inventory (§6 artifact 4) -----------------
## Parses the artifact-4 inventory register and enforces: PROPOSED envelope,
## every row DISCOVERED with non-empty required columns, the NAMESPACE bijection
## over exported-function/exported-data rows (mirrors G1.1's row-count
## bijection), and the public-output-field rows. Both bijections are by NAME
## against derived sources — no count is pinned here (P0.6 I1). (RURL-dmsgpcak)
psi_path <- file.path(reg_dir, "public-surface-inventory.md")
psi_reg  <- if (file.exists(psi_path)) read_rows(psi_path) else NULL
check(!is.null(psi_reg), "public-surface-inventory.md: no parseable Rows table")
psi_n <- 0L
psi_checks_before <- pass + length(fail)
if (!is.null(psi_reg)) {
  ptxt <- paste(readLines(psi_path, warn = FALSE), collapse = "\n")
  # envelope: every required field present, lifecycle_state == PROPOSED
  for (ef in env_fields) {
    check(grepl(sprintf("\\|\\s*%s\\s*\\|", ef), ptxt),
          sprintf("[missing_required_fields] public-surface-inventory.md: envelope field '%s'", ef))
  }
  check(grepl("\\|\\s*lifecycle_state\\s*\\|\\s*PROPOSED\\s*\\|", ptxt),
        "public-surface-inventory.md: lifecycle_state must be PROPOSED")

  # required row columns present
  psi_cols <- c("item_id", "kind", "arguments", "behavior_dials", "interactions",
                "vector_error", "docs", "migration", "test_family", "state",
                "disposition", "missing_evidence")
  for (col in psi_cols) {
    check(col %in% psi_reg$header,
          sprintf("[missing_required_fields] public-surface-inventory.md: column '%s'", col))
  }

  psi_rows <- psi_reg$rows
  psi_n <- length(psi_rows)
  psi_kinds <- c("exported-function", "exported-data", "public-output-field",
                 "curl-dependency", "migration-surface")
  # every row: state == DISCOVERED, kind in the closed set, non-empty required cells
  for (r in psi_rows) {
    id <- gv(r, "item_id")
    check(identical(gv(r, "state"), "DISCOVERED"),
          sprintf("[unknown_states] public-surface-inventory %s: state '%s' != DISCOVERED", id, gv(r, "state")))
    check(gv(r, "kind") %in% psi_kinds,
          sprintf("public-surface-inventory %s: kind '%s' not in closed set", id, gv(r, "kind")))
    for (col in psi_cols) {
      check(!is.na(gv(r, col)) && nzchar(gv(r, col)),
            sprintf("public-surface-inventory %s: empty required column '%s'", id, col))
    }
  }
  # unique item_ids
  psi_ids <- vapply(psi_rows, function(r) gv(r, "item_id"), "")
  dup_psi <- unique(psi_ids[duplicated(psi_ids)])
  check(length(dup_psi) == 0,
        sprintf("[duplicate_ids] public-surface-inventory: %s", paste(dup_psi, collapse = ", ")))

  # --- NAMESPACE bijection over exported-function/exported-data rows, BY NAME --
  # No count is asserted: setequal + the two setdiff loops below are strictly
  # stronger than a cardinality check, and unlike a literal they never need
  # editing when the surface grows (P0.6 I1).
  ns_exports <- psd_ns_exports()
  is_export_row <- vapply(psi_rows,
                          function(r) gv(r, "kind") %in% c("exported-function", "exported-data"), TRUE)
  fn_ids <- sort(vapply(psi_rows[is_export_row], function(r) gv(r, "item_id"), ""))
  check(setequal(fn_ids, ns_exports),
        sprintf("public-surface-inventory export rows != NAMESPACE (rows=%d, NAMESPACE=%d)",
                length(fn_ids), length(ns_exports)))
  for (e in setdiff(ns_exports, fn_ids))
    check(FALSE, sprintf("public-surface-inventory: missing export row '%s'", e))
  for (e in setdiff(fn_ids, ns_exports))
    check(FALSE, sprintf("public-surface-inventory: extra export row '%s' not in NAMESPACE", e))

  # --- public-output-field rows, exact name set derived from .spu_result_fields -
  pub_fields <- psd_result_fields()
  check(length(pub_fields) >= 1,
        "public-surface-inventory: could not derive the field set from .spu_result_fields")
  is_field_row <- vapply(psi_rows, function(r) identical(gv(r, "kind"), "public-output-field"), TRUE)
  field_ids <- sort(vapply(psi_rows[is_field_row], function(r) gv(r, "item_id"), ""))
  check(setequal(field_ids, pub_fields),
        sprintf("public-surface-inventory public-output-field rows != .spu_result_fields (rows=%d, source=%d)",
                length(field_ids), length(pub_fields)))
  for (f in setdiff(pub_fields, field_ids))
    check(FALSE, sprintf("public-surface-inventory: missing public-output-field row '%s'", f))

  # --- curl dependency + migration surface (§10) explicitly present -----------
  n_curl <- sum(vapply(psi_rows, function(r) identical(gv(r, "kind"), "curl-dependency"), TRUE))
  n_migr <- sum(vapply(psi_rows, function(r) identical(gv(r, "kind"), "migration-surface"), TRUE))
  check(n_curl >= 1L, "public-surface-inventory: no curl-dependency row (§10 curl surface)")
  check(n_migr >= 1L, "public-surface-inventory: no migration-surface row (§10 migration surface)")
}
psi_checks <- (pass + length(fail)) - psi_checks_before

## --- G2 register: contradictions (§6 artifact 2) -----------------------------
## Parses the artifact-2 contradiction register and enforces: PROPOSED envelope
## (the register artifact itself is not sealed into the manifest until
## cp-snapshot-2), the required columns, the 12-row C-01..C-12 bijection (no
## missing/extra/dup), related_rcon values all within RCON-01..RCON-10, and a
## non-empty bound_owner_tier.
##
## G2 FINALIZATION (RURL-oknltrux): every contradiction now carries an owner
## disposition (§7 G2 exit), projected from the accepted P-tier. So each row is
## asserted DISPOSED, not scaffold: disposition_state == ACCEPTED, a typed
## disposition_type in {ACCEPTED, REJECTED, SUPERSEDED, COMPATIBILITY-ONLY}
## (§5 intro), a non-placeholder owner_decision_ref naming the accepted decision,
## and affected_claims / invalidated_artifacts filled (no "TBD at disposition").
con_path <- file.path(reg_dir, "contradictions.md")
con_pat  <- rschemas$record_types$register$variants$contradiction$id_pattern
con_reg  <- if (file.exists(con_path)) read_rows(con_path) else NULL
check(!is.null(con_reg), "contradictions.md: no parseable Rows table")
con_n <- 0L
con_checks_before <- pass + length(fail)
if (!is.null(con_reg)) {
  ctxt <- paste(readLines(con_path, warn = FALSE), collapse = "\n")
  # envelope: every required field present, lifecycle_state == PROPOSED
  for (ef in env_fields) {
    check(grepl(sprintf("\\|\\s*%s\\s*\\|", ef), ctxt),
          sprintf("[missing_required_fields] contradictions.md: envelope field '%s'", ef))
  }
  check(grepl("\\|\\s*lifecycle_state\\s*\\|\\s*PROPOSED\\s*\\|", ctxt),
        "contradictions.md: lifecycle_state must be PROPOSED")

  # required row columns present
  con_cols <- c("id", "contradiction", "required_disposition", "related_rcon",
                "bound_owner_tier", "disposition_type", "disposition_state",
                "authority", "owner_decision_ref", "affected_claims",
                "invalidated_artifacts", "verification_ref")
  for (col in con_cols) {
    check(col %in% con_reg$header,
          sprintf("[missing_required_fields] contradictions.md: column '%s'", col))
  }

  con_rows <- con_reg$rows
  con_n <- length(con_rows)
  disp_types <- c("ACCEPTED", "REJECTED", "SUPERSEDED", "COMPATIBILITY-ONLY")
  # per-row: id pattern, DISPOSED shape, related_rcon in range, tier present
  for (r in con_rows) {
    id <- gv(r, "id")
    check(grepl(con_pat, id),
          sprintf("contradictions.md: id '%s' fails %s", id, con_pat))
    # G2: every contradiction is owner-disposed (§7 G2). The row's
    # disposition_state is the lifecycle state of the disposition itself
    # (ACCEPTED = owner-accepted), independent of disposition_type (which may
    # itself be REJECTED — the claim is rejected, but that rejection is accepted).
    check(identical(gv(r, "disposition_state"), "ACCEPTED"),
          sprintf("[unknown_states] contradictions %s: disposition_state '%s' != ACCEPTED (G2 disposition)",
                  id, gv(r, "disposition_state")))
    check(gv(r, "disposition_type") %in% disp_types,
          sprintf("contradictions %s: disposition_type '%s' not in {%s}",
                  id, gv(r, "disposition_type"), paste(disp_types, collapse = ", ")))
    odr <- gv(r, "owner_decision_ref")
    check(!is.na(odr) && nzchar(odr) &&
            !grepl("TBD|pending", odr, ignore.case = TRUE),
          sprintf("contradictions %s: owner_decision_ref must name the accepted decision (got '%s')",
                  id, odr %||% "<NA>"))
    for (col in c("affected_claims", "invalidated_artifacts")) {
      v <- gv(r, col)
      check(!is.na(v) && nzchar(v) && !grepl("TBD at disposition", v, fixed = TRUE),
            sprintf("contradictions %s: %s still scaffold placeholder ('%s')",
                    id, col, v %||% "<NA>"))
    }
    check(!is.na(gv(r, "bound_owner_tier")) && nzchar(gv(r, "bound_owner_tier")),
          sprintf("contradictions %s: empty bound_owner_tier", id))
    rcons <- regmatches(gv(r, "related_rcon"),
                        gregexpr("RCON-[0-9]+", gv(r, "related_rcon")))[[1]]
    check(length(rcons) >= 1,
          sprintf("contradictions %s: no related_rcon", id))
    for (rc in rcons) {
      n <- suppressWarnings(as.integer(sub("RCON-", "", rc)))
      check(!is.na(n) && n >= 1L && n <= 10L,
            sprintf("contradictions %s: related_rcon '%s' outside RCON-01..RCON-10", id, rc))
    }
  }
  # 12-row C-01..C-12 bijection (no missing / extra / duplicate)
  con_ids <- vapply(con_rows, function(r) gv(r, "id"), "")
  dup_con <- unique(con_ids[duplicated(con_ids)])
  check(length(dup_con) == 0,
        sprintf("[duplicate_ids] contradictions: %s", paste(dup_con, collapse = ", ")))
  con_expect <- sprintf("C-%02d", 1:12)
  check(length(con_ids) == 12L,
        sprintf("contradictions register expected 12 rows, got %d", length(con_ids)))
  check(setequal(con_ids, con_expect),
        sprintf("contradictions rows != C-01..C-12 (got %s)", paste(sort(con_ids), collapse = ", ")))
  for (e in setdiff(con_expect, con_ids))
    check(FALSE, sprintf("contradictions: missing row '%s'", e))
  for (e in setdiff(con_ids, con_expect))
    check(FALSE, sprintf("contradictions: extra row '%s' not in C-01..C-12", e))
}
con_checks <- (pass + length(fail)) - con_checks_before

## --- Gate-acceptance input hashes: RETIRED (ADR 0014) ------------------------
## This section recomputed every sha256 pinned in a gates/*.md '## Inputs' table
## and failed with "gate acceptance reopened" on any drift. It was the costliest
## instrument in the control plane: editing a contract body reopened an ACCEPTED
## gate, which then stayed red until an owner seal-merge -- for a change git had
## already reported in full, to an approver who was the same person as the
## proposer.
##
## It is not replaced by nothing. traceability-gate.R T3 regenerates the claim
## index from the contracts and byte-compares it, so editing a contract still
## fails a machine check -- one clearable by regenerating rather than by a
## ratification ceremony.
##
## The gates/ records are deleted with it; both are recoverable from the tag
## v3-control-plane-final.

## --- G3 contract-family records (§6 artifacts 3–10 + 4 + the G3.X capstone) ----
## The ten design/work/url-v3/contracts/*.md are a record family parallel to the
## registers and gate-acceptance records. This section is the validator coverage
## each contract's envelope promises ("<name> validator section stages with the
## cp-snapshot-3 seal"). It asserts, WITHOUT inventing product semantics, only what
## each contract's own envelope + completion_rule already promise:
##   * common envelope: the 17 required fields non-empty; lifecycle_state PROPOSED
##     (the seal is a manifest present:true hash-pin, NOT an envelope flip — mirrors
##     the registers); tracked_location == the file's own path; unique id.
##   * tamper-evident ## Inputs: each projected source exists and its recorded
##     sha256 recomputes equal (same mechanism as gates/*.md) — a drifted projection
##     FAILS the record, exactly like the G2-acceptance reopening rule.
##   * cell discipline (the completion_rule's "each carry a non-placeholder
##     owner_decision_ref with status SETTLED or an explicit status OPEN"): every
##     matrix table carrying an `owner_decision_ref` + `status` column pair has each
##     row status in {SETTLED, OPEN}, and each SETTLED row cites a non-placeholder
##     ref; every `## Open cells` section that lists an -O cell states destinations.
## The canonical-state contract (artifact 3) additionally gets the deep staged
## section (_scratch/orchestrate/g3-seal-staging/canonical-state-validator-section.md):
## Rows columns/enums, 18-field completeness, verdict-layer sets, open-cell coverage.
## Objective per-contract counts (the capstone's five criterion verdicts; the three
## caches) are asserted only where the contract states an exact cardinality.
## (RURL-huneoffx; the cp-snapshot-3 seal.)
##
## ARTIFACT 4 IS TWO FILES (P0.6; RURL-oygqsykd). public-surface-closure.md is the
## INVARIANT (I1-I5 + the owning-contract legend) and is a G3 `## Inputs` hash pin;
## public-surface-disposition.md is the ROSTER (one row per surface cell) and is
## deliberately NOT a gate input, because G3 criterion 1 is a property over the
## surface, not a statement about its size. Pinning the roster in the gate made
## every added export reopen contract-matrix closure for a change that SATISFIED
## the property. So the roster's conformance is asserted here instead, and — this
## is the part that matters — the population is DERIVED from NAMESPACE and
## `.spu_result_fields` and compared BY NAME. No surface count is written in this
## file or in either contract; a transcribed number is a stale fact waiting to
## happen, and the 51->52 growth that left "29 exports" in three places of the
## closure prose is the proof.
contracts_dir <- file.path(root, "contracts")
contract_n <- 0L
contract_checks_before <- pass + length(fail)

# The 2-column ## Envelope table -> named character.
read_envelope <- function(ln) {
  h <- which(grepl("^##\\s+Envelope\\s*$", ln)); if (length(h) != 1) return(NULL)
  nxt <- which(grepl("^##\\s", ln) & seq_along(ln) > h)
  end <- if (length(nxt)) min(nxt) - 1L else length(ln)
  tbl <- ln[(h + 1):end]; tbl <- tbl[.is_trow(tbl) & !.is_tsep(tbl)]
  kv <- list()
  for (r in tbl[-1]) { cs <- .tcells(r); if (length(cs) >= 2 && nzchar(cs[[1]])) kv[[cs[[1]]]] <- cs[[2]] }
  kv
}
.placeholder <- function(x) is.na(x) || !nzchar(trimws(x %||% "")) ||
  grepl("^(—|-|tbd|pending|n/?a)$", trimws(x), ignore.case = TRUE)

## --- --regenerate: emit the rows the artifact-4 checks derive ---------------
## The population is the same one the checks compare against -- psd_ns_exports()
## and psd_result_fields() -- and the row shape is read off the table each row
## joins, so a regenerated row is one the checks accept by construction:
##   inventory   item_id | kind | 9 x TODO ... | state=DISCOVERED | TODO | TODO
##   roster      `name` | TODO (artifact 4: unassigned) | TODO | OPEN (TODO: ...)
## The roster owner/status stubs name "artifact 4" (this roster) because I2/I3
## accept an unowned cell only as a named downstream artifact; the TODO in the
## same cell says the assignment is still to be made. Export rows are inserted
## in alphabetical position (the order both tables keep); field rows are
## appended after the last field row (the tables follow `.spu_result_fields`
## order, which the derived set does not carry). The roster's Bijection counts
## are re-derived when they no longer sum to the surface, digits only.
.tline <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
# Line indices of the first pipe table whose header is `header`: list(header=,
# rows=<indices of data lines>), or NULL. The same tokenizer the checks use.
.table_at <- function(ln, header) {
  for (i in seq_len(length(ln) - 1L)) {
    if (.is_trow(ln[i]) && .is_tsep(ln[i + 1L]) && identical(.tcells(ln[i]), header)) {
      j <- i + 2L
      while (j <= length(ln) && .is_trow(ln[j]) && !.is_tsep(ln[j])) j <- j + 1L
      return(list(header = i, rows = if (j > i + 2L) seq.int(i + 2L, j - 1L) else integer(0)))
    }
  }
  NULL
}
# C-locale "k sorts after name" -- the order both tables keep, made independent
# of the session locale (the LC_ALL=C cell of verify.yml runs this too).
.c_after <- function(k, name) {
  k != name & vapply(k, function(x) identical(sort(c(x, name), method = "radix")[[1L]], name), TRUE)
}
# Insert `line` into a table whose data lines are `rows` (line indices) keyed
# by `keys`: before the first `group` row whose key sorts after `name` when
# `ordered`, else after the last `group` row, else after the last row.
.insert_row <- function(ln, rows, keys, group, name, line, ordered = TRUE) {
  at <- NA_integer_
  if (ordered) {
    later <- rows[group & .c_after(keys, name)]
    if (length(later)) at <- later[[1L]] - 1L
  }
  if (is.na(at)) {
    same <- rows[group]
    at <- if (length(same)) same[length(same)] else rows[length(rows)]
  }
  append(ln, line, after = at)
}

regenerate_public_surface <- function() {
  changed <- character(0)
  exports <- psd_ns_exports()
  fields <- psd_result_fields()
  export_kinds <- c("exported-function", "exported-data")

  ## inventory register ------------------------------------------------------
  ln <- readLines(psi_path, warn = FALSE)
  reg <- read_rows(psi_path)
  if (is.null(reg)) stop("public-surface-inventory.md: no parseable Rows table")
  h <- which(grepl("^##\\s+Rows\\s*$", ln))
  col <- function(name) match(name, reg$header)
  stub_row <- function(id, kind) {
    cells <- rep("TODO", length(reg$header))
    cells[col("item_id")] <- id
    cells[col("kind")] <- kind
    cells[col("state")] <- "DISCOVERED"
    .tline(cells)
  }
  # Re-read the table after every insertion: line indices move.
  inv_state <- function(ln) {
    tab <- .table_at(ln[h:length(ln)], reg$header)
    if (is.null(tab)) stop("public-surface-inventory.md: cannot locate the Rows table")
    rows <- tab$rows + h - 1L
    cells <- lapply(ln[rows], .tcells)
    list(rows = rows,
         keys = vapply(cells, function(cs) cs[[col("item_id")]], ""),
         kinds = vapply(cells, function(cs) cs[[col("kind")]], ""))
  }
  s <- inv_state(ln)
  for (e in setdiff(exports, s$keys[s$kinds %in% export_kinds])) {
    ln <- .insert_row(ln, s$rows, s$keys, s$kinds %in% export_kinds, e,
                      stub_row(e, "exported-function"))
    changed <- c(changed, sprintf("public-surface-inventory.md: + export row `%s`", e))
    s <- inv_state(ln)
  }
  for (f in setdiff(fields, s$keys[s$kinds == "public-output-field"])) {
    ln <- .insert_row(ln, s$rows, s$keys, s$kinds == "public-output-field", f,
                      stub_row(f, "public-output-field"), ordered = FALSE)
    changed <- c(changed, sprintf("public-surface-inventory.md: + public-output-field row `%s`", f))
    s <- inv_state(ln)
  }
  if (length(changed)) writeLines(ln, psi_path, useBytes = TRUE)

  ## disposition roster ------------------------------------------------------
  dp <- file.path(contracts_dir, "public-surface-disposition.md")
  ln <- readLines(dp, warn = FALSE)
  before_n <- length(changed)
  unq <- function(x) gsub("`", "", trimws(x), fixed = TRUE)
  roster_stub <- function(name) .tline(c(
    sprintf("`%s`", name), "TODO (artifact 4: unassigned)", "TODO",
    "OPEN (TODO: assign an owning G3 contract; artifact 4)"
  ))
  for (spec in list(list(PSD_EXPORT_HDR, exports, TRUE), list(PSD_FIELD_HDR, fields, FALSE))) {
    header <- spec[[1]]; want <- spec[[2]]; ordered <- spec[[3]]
    repeat {
      tab <- .table_at(ln, header)
      if (is.null(tab)) stop(sprintf("public-surface-disposition.md: no `%s` roster table", header[[1]]))
      keys <- vapply(ln[tab$rows], function(r) unq(.tcells(r)[[1]]), "")
      missing <- setdiff(want, keys)
      if (!length(missing)) break
      ln <- .insert_row(ln, tab$rows, keys, rep(TRUE, length(keys)), missing[[1]],
                        roster_stub(missing[[1]]), ordered = ordered)
      changed <- c(changed, sprintf("public-surface-disposition.md: + %s row `%s`",
                                    header[[1]], missing[[1]]))
    }
  }

  # Bijection counts: digits only, re-derived the way the check derives them
  # (NAMESPACE + .spu_result_fields + curl/migration rows).
  items <- .table_at(ln, PSD_ITEM_HDR)
  bij <- NULL
  for (i in seq_len(length(ln) - 1L)) {
    if (.is_trow(ln[i]) && .is_tsep(ln[i + 1L])) {
      hd <- .tcells(ln[i])
      if ("surface class" %in% hd && "count" %in% hd) { bij <- .table_at(ln, hd); break }
    }
  }
  if (!is.null(bij) && !is.null(items)) {
    derived <- length(exports) + length(fields) + length(items$rows)
    targets <- list(c("^exported functions$", length(exports)),
                    c("^public output fields$", length(fields)),
                    c("total", derived))
    for (t in targets) {
      n <- as.integer(t[[2]])
      for (r in bij$rows) {
        cs <- .tcells(ln[r])
        if (!grepl(t[[1]], tolower(cs[[1]]))) next
        have <- as.integer(gsub("[^0-9]", "", cs[[2]]))
        if (identical(have, n)) next
        # the digits of the second cell only; bold markers and every other
        # byte of the row are kept
        ln[r] <- sub("^(\\|[^|]*\\|[^|0-9]*)[0-9]+", sprintf("\\1%d", n), ln[r])
        changed <- c(changed, sprintf(
          "public-surface-disposition.md: bijection count `%s` %d -> %d", cs[[1]], have, n))
      }
    }
  }
  if (length(changed) > before_n) writeLines(ln, dp, useBytes = TRUE)

  if (!length(changed)) cat("  nothing to regenerate: the artifact-4 files cover the surface\n")
  for (c in changed) cat("  + ", c, "\n", sep = "")
  invisible(changed)
}

# Roster cell -> its raw "owning contract(s)" text, read out of the roster half
# and memoized like the other derived sets. The parsing is psd_roster_cells()
# (with the predicates, above), which --self-test calls on a fixture instead.
psd_roster_owners <- function(dir) {
  if (is.null(.psd_cache$roster)) {
    p <- file.path(dir, "public-surface-disposition.md")
    .psd_cache$roster <- psd_roster_cells(
      if (file.exists(p)) readLines(p, warn = FALSE) else character(0))
  }
  .psd_cache$roster
}

env_required <- c("id", "name", "artifact_number", "schema_version", "tracked_location",
                  "owner", "single_writer", "lifecycle_state", "dependencies", "bound_decision",
                  "bound_evidence", "closes_finding", "completion_rule", "content_hash",
                  "approval_evidence", "validation_command", "validator_note")
seen_cids <- character(0)
if (dir.exists(contracts_dir)) {
  for (cf in sort(list.files(contracts_dir, pattern = "\\.md$", full.names = TRUE))) {
    bn <- basename(cf); ln <- readLines(cf, warn = FALSE)
    contract_n <- contract_n + 1L

    ## A. common envelope --------------------------------------------------------
    env <- read_envelope(ln)
    check(!is.null(env), sprintf("contracts %s: no parseable ## Envelope", bn))
    if (!is.null(env)) {
      for (ef in env_required)
        check(!is.null(env[[ef]]) && nzchar(env[[ef]]),
              sprintf("[missing_required_fields] contracts %s: envelope field '%s'", bn, ef))
      check(identical(env[["lifecycle_state"]], "PROPOSED"),
            sprintf("[unknown_states] contracts %s: lifecycle_state must be PROPOSED until cp-snapshot-3 (got '%s')",
                    bn, env[["lifecycle_state"]] %||% "<NA>"))
      check(identical(env[["tracked_location"]] %||% "", cf),
            sprintf("contracts %s: tracked_location '%s' != own path '%s'",
                    bn, env[["tracked_location"]] %||% "<NA>", cf))
      check(!is.null(env[["single_writer"]]) && grepl("owner", env[["single_writer"]], ignore.case = TRUE),
            sprintf("contracts %s: single_writer must name the repository owner", bn))
      cid <- env[["id"]] %||% ""
      check(nzchar(cid) && !(cid %in% seen_cids),
            sprintf("[duplicate_ids] contracts %s: id '%s'", bn, cid))
      seen_cids <- c(seen_cids, cid)
    }
    for (sec in c("Inputs", "Scope boundaries", "Open cells"))
      check(any(grepl(sprintf("^##\\s+%s\\s*$", sec), ln)),
            sprintf("contracts %s: missing '## %s' section", bn, sec))

    ## B. ## Inputs paths resolve ------------------------------------------------
    ## The sha256 comparison here is retired (ADR 0014) for the same reason as
    ## the gate cascade: it turned any edit to a projected source into a
    ## "projection stale" failure clearable only by re-sealing. What is KEPT is
    ## the half that finds real defects and that git does not report -- a path
    ## listed twice, or an Inputs row pointing at a file that no longer exists.
    itbl <- section_lines(ln, "Inputs"); itbl <- itbl[.is_trow(itbl) & !.is_tsep(itbl)]
    irows <- if (length(itbl) >= 1) itbl[-1] else character(0)
    seen_paths <- character(0)
    for (r in irows) {
      cs <- .tcells(r); p <- cs[[1]]
      if (!grepl("^design/work/url-v3/", p)) next
      check(!(p %in% seen_paths), sprintf("contracts %s: duplicate ## Inputs path %s", bn, p))
      seen_paths <- c(seen_paths, p)
      check(file.exists(p), sprintf("contracts %s: ## Inputs path missing: %s", bn, p))
    }
    check(length(seen_paths) >= 1, sprintf("contracts %s: ## Inputs has no source rows", bn))

    ## C. cell discipline over owner_decision_ref+status matrix tables -----------
    disc_tables <- 0L
    for (tb in parse_pipe_tables(ln)) {
      if (!("status" %in% tb$header && "owner_decision_ref" %in% tb$header)) next
      disc_tables <- disc_tables + 1L
      for (r in tb$rows) {
        raw <- gv(r, "status")
        if (is.na(raw) || !nzchar(trimws(raw))) next        # spacer/continuation row
        st <- sub("\\s.*$", "", trimws(raw))                 # leading token
        check(st %in% c("SETTLED", "OPEN"),
              sprintf("contracts %s: status '%s' not in {SETTLED, OPEN}", bn, raw))
        if (identical(st, "SETTLED"))
          check(!.placeholder(gv(r, "owner_decision_ref")),
                sprintf("contracts %s: SETTLED row cites placeholder owner_decision_ref ('%s')",
                        bn, gv(r, "owner_decision_ref") %||% "<NA>"))
      }
    }

    ## D. Open cells name destinations ------------------------------------------
    obody <- section_lines(ln, "Open cells")
    if (any(grepl("[A-Z]+-O[0-9]+", obody)))
      check(any(grepl("Settles at|destination|CLOSED|closed", obody)),
            sprintf("contracts %s: ## Open cells lists -O cells but names no settlement destination", bn))

    ## E. deep section — canonical-state (artifact 3) ---------------------------
    if (identical(bn, "canonical-state-contract.md") && !is.null(env)) {
      check(identical(env[["artifact_number"]], "3"),
            sprintf("canonical-state: artifact_number must be 3 (got '%s')", env[["artifact_number"]] %||% "<NA>"))
      check(identical(env[["bound_decision"]], "P1.1"),
            "canonical-state: bound_decision must be P1.1")
      check(identical(env[["bound_evidence"]], "S1"),
            "canonical-state: bound_evidence must be S1")
      check(grepl("RCON-02", env[["closes_finding"]] %||% ""),
            "canonical-state: closes_finding must name RCON-02")
      cr <- read_rows(cf)
      check(!is.null(cr), "canonical-state: no parseable ## Rows table")
      if (!is.null(cr)) {
        want <- c("field", "type", "presence", "provenance", "invariants",
                  "public_projection", "lifecycle", "owner_decision_ref", "status")
        check(identical(cr$header, want),
              sprintf("canonical-state: Rows columns must be exactly [%s] (got [%s])",
                      paste(want, collapse = ", "), paste(cr$header, collapse = ", ")))
        fields <- vapply(cr$rows, function(r) gv(r, "field"), "")
        dupf <- unique(fields[duplicated(fields)])
        check(length(dupf) == 0, sprintf("canonical-state: duplicate field(s) %s", paste(dupf, collapse = ", ")))
        presence_ok    <- c("absent", "present-empty", "present-nonempty", "n/a")
        provenance_ok  <- c("source", "parsed", "derived", "classifier", "PSL", "projection")
        projection_ok  <- c("public", "internal", "companion")
        for (r in cr$rows) {
          fn <- gv(r, "field")
          for (col in c("type", "presence", "provenance", "invariants", "public_projection", "lifecycle")) {
            v <- gv(r, col)
            check(!is.na(v) && nzchar(trimws(v %||% "")),
                  sprintf("canonical-state %s: empty '%s' (no unowned cells)", fn, col))
          }
          # presence: strip a trailing "(...)" note, then each " / "-joined token
          # must be in the closed set (n/a is a single token with a bare slash).
          pbase <- sub("\\s*\\(.*\\)\\s*$", "", gv(r, "presence") %||% "")
          pv <- trimws(strsplit(pbase, "\\s+/\\s+")[[1]])
          check(length(pv) >= 1 && all(pv %in% presence_ok),
                sprintf("canonical-state %s: presence '%s' outside {%s}", fn, gv(r, "presence"),
                        paste(presence_ok, collapse = ", ")))
          check(sub("\\s.*$", "", gv(r, "provenance") %||% "") %in% provenance_ok,
                sprintf("canonical-state %s: provenance leading token '%s' outside set", fn, gv(r, "provenance")))
          check(sub("\\s.*$", "", gv(r, "public_projection") %||% "") %in% projection_ok,
                sprintf("canonical-state %s: public_projection leading token '%s' outside set", fn, gv(r, "public_projection")))
          st <- gv(r, "status")
          check(st %in% c("SETTLED", "OPEN"),
                sprintf("canonical-state %s: status '%s' not in {SETTLED, OPEN}", fn, st))
          if (identical(st, "SETTLED"))
            check(grepl("^P1\\.1@[0-9a-f]{7,40}$", gv(r, "owner_decision_ref") %||% ""),
                  sprintf("canonical-state %s: SETTLED owner_decision_ref must match P1.1@<sha> (got '%s')",
                          fn, gv(r, "owner_decision_ref") %||% "<NA>"))
        }
        # completeness: the 18 public .spu_result_fields present as public rows
        pub18 <- c("original_url", "scheme", "host", "port", "path", "query", "fragment",
                   "user", "password", "domain", "tld", "domain_ascii", "domain_unicode",
                   "tld_ascii", "tld_unicode", "is_ip_host", "clean_url", "parse_status")
        for (f in pub18) {
          idx <- which(fields == f)
          check(length(idx) == 1 &&
                  identical(sub("\\s.*$", "", gv(cr$rows[[idx[1]]], "public_projection") %||% ""), "public"),
                sprintf("canonical-state: public field '%s' missing or not public_projection=public", f))
        }
        # structural-kind fields present
        for (f in c("host_kind", "authority_kind", "query_kind", "fragment_kind"))
          check(f %in% fields, sprintf("canonical-state: structural field '%s' missing", f))
        # verdict-layer fields: companion projection + layer3 enum set
        for (f in c("layer1_syntax_verdict", "layer2_policy_verdict", "layer3_annotation_state")) {
          idx <- which(fields == f)
          check(length(idx) == 1 &&
                  grepl("^companion", gv(cr$rows[[idx[1]]], "public_projection") %||% ""),
                sprintf("canonical-state: verdict field '%s' missing or not companion", f))
        }
        idx3 <- which(fields == "layer3_annotation_state")
        if (length(idx3) == 1) {
          ty <- gv(cr$rows[[idx3[1]]], "type") %||% ""
          for (tok in c("not-requested", "not-applicable", "known", "unknown", "invalid-input", "dependency-error"))
            check(grepl(tok, ty, fixed = TRUE),
                  sprintf("canonical-state: layer3_annotation_state type must enumerate '%s'", tok))
        }
        # authority_kind stays OPEN (regression guard — P1.1 Q3 deferred it)
        idxa <- which(fields == "authority_kind")
        if (length(idxa) == 1)
          check(identical(gv(cr$rows[[idxa[1]]], "status"), "OPEN"),
                "canonical-state: authority_kind must remain status OPEN (P1.1 Q3 deferred)")
        # every OPEN row's field is named in ## Open cells
        for (r in cr$rows) if (identical(gv(r, "status"), "OPEN")) {
          fn <- gv(r, "field")
          check(any(grepl(fn, obody, fixed = TRUE)),
                sprintf("canonical-state: OPEN field '%s' absent from ## Open cells", fn))
        }
      }
    }

    ## F + F2. artifact 4, invariant half -- psd_closure_failures(), defined
    ## with the parsers at the top of the file so --self-test can reach it.
    ## The tree-bound inputs are resolved here; the rules live there.
    if (identical(bn, "public-surface-closure.md")) {
      absorb(psd_closure_failures(
        ln, cells = psd_roster_owners(contracts_dir), fields = psd_result_fields(),
        contract_exists = function(f) file.exists(file.path(contracts_dir, f)),
        adr_exists = function(n)
          length(list.files("design/adr", pattern = sprintf("^%s-", n))) >= 1
      ))
    }

    ## G. artifact 4, roster half -- psd_roster_failures(), likewise at the top.
    if (identical(bn, "public-surface-disposition.md")) {
      inv_path <- file.path(contracts_dir, "public-surface-closure.md")
      absorb(psd_roster_failures(
        ln,
        inv_ln = if (file.exists(inv_path)) readLines(inv_path, warn = FALSE) else character(0),
        exports = psd_ns_exports(), fields = psd_result_fields(),
        contract_lines = function(f) {
          p <- file.path(contracts_dir, f)
          if (file.exists(p)) readLines(p, warn = FALSE) else character(0)
        }
      ))
    }

    if (identical(bn, "cross-artifact-consistency.md")) {
      crit <- Filter(function(t) "verdict" %in% t$header && "#" %in% t$header, parse_pipe_tables(ln))
      crit <- Filter(function(t) length(t$rows) == 5L, crit)
      check(length(crit) >= 1,
            "cross-artifact-consistency: criterion table with five (i)-(v) rows not found")
      if (length(crit) >= 1)
        for (r in crit[[1]]$rows)
          check(identical(gv(r, "verdict"), "PASS"),
                sprintf("cross-artifact-consistency: criterion %s verdict must be PASS (got '%s')",
                        gv(r, "#"), gv(r, "verdict")))
    }
    if (identical(bn, "semantic-cache-contract.md")) {
      inv <- Filter(function(t) "cache" %in% t$header && "eviction" %in% t$header, parse_pipe_tables(ln))
      check(length(inv) >= 1 && length(inv[[1]]$rows) == 3L,
            "semantic-cache: cache-inventory table must enumerate exactly 3 caches (P5.1)")
    }
  }
}
contract_checks <- (pass + length(fail)) - contract_checks_before

## --- --regenerate: write the rows, say so, exit 0 ---------------------------
## Placed after every check so it runs against the same derived sets the checks
## just scored; without the flag nothing below this line is different.
if (.vr_regenerate) {
  cat("validate-records.R --regenerate\n")
  regenerate_public_surface()
  cat("regenerated the artifact-4 register and roster\n")
  quit(status = 0L)
}

cat("validate-records.R\n")
cat(sprintf("contracts: %d files, %d added checks\n", contract_n, contract_checks))
cat(sprintf("public-surface-inventory: %d rows, %d added checks\n", psi_n, psi_checks))
cat(sprintf("contradictions: %d rows, %d added checks\n", con_n, con_checks))
cat(sprintf("registers: source-claims=%d rows, findings=%d rows\n",
            if (!is.null(sc_reg)) length(sc_reg$rows) else 0L,
            if (!is.null(fd_reg)) length(fd_reg$rows) else 0L))
cat(sprintf("owner-decision records: %d (%s)\n", length(records), paste(known, collapse = ", ")))
cat(sprintf("checks passed: %d\n", pass))
if (length(fail) > 0) {
  cat(sprintf("checks FAILED: %d\n", length(fail)))
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("VALIDATION PASSED\n")
