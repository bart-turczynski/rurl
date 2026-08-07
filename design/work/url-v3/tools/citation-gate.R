#!/usr/bin/env Rscript
# Citation gate -- G3.K citations resolve to the rows they name.
#
# WHAT THIS GATE IS FOR. `verification/key-join-discharge.md` says its 51-cell
# arithmetic is "derived and checkable" against `contracts/key-join-contracts.md`
# own tables. Until this gate existed, nothing checked it. The record located
# each block by a LINE RANGE into the contract, and the ~25 per-row citations by
# a bare line number -- so the moment the contract grew a paragraph, every
# citation below the insertion point silently denoted a different row. That is
# exactly what happened: RURL-nravluqd added 25 lines and RURL-ojrtnnhy added
# more, and all six ranges plus all 24 per-row citations went stale without a
# single instrument noticing (RURL-ztakrggx).
#
# WHY THE CITATIONS ARE NAMES AND NOT LINE NUMBERS. Three reasons, in order of
# force:
#
#   1. The notation was AMBIGUOUS, not merely stale. Inside
#      key-join-discharge.md, `:80` meant "contract line 80" in block 2's cell
#      column and "port 80" in block 3's left/right column, 24 lines apart. A
#      citation form that collides with the domain's own vocabulary cannot be
#      checked without heuristics and cannot be read reliably either.
#   2. The line number carried NO information the row name did not already
#      carry. Every per-row citation sat immediately after the row's own key
#      ("scheme source (`:80`)"), and every range sat immediately after the
#      section heading it denoted. The number was redundant, and redundancy
#      that has to be hand-maintained is a defect generator.
#   3. The record's actual claim is ARITHMETIC OVER ROW COUNTS -- 7 + 16 + 14 +
#      6 + 8 = 51. A line range does not check that. A row count does. Citing
#      the section by heading and checking its row count makes the record's own
#      "derived and checkable" promise true for the first time.
#
# So the fix is not a re-pin. Re-pinning buys one correct revision and rots on
# the next contract edit; naming the section and the row cannot rot, because
# the gate resolves the name against the live contract on every run.
#
# WHY THIS IS A SEPARATE GATE AND NOT NEW RULES IN traceability-gate.R. That
# gate's "10/10 rules, 71/71 self-test" counts are pinned in several records and
# in RURL-ztakrggx's own acceptance criteria. Adding rules there would move two
# numbers other artifacts assert. This gate owns its own rule set.
#
# Rules:
#   C0  notation   -- no line-reference token (`:N`, `:N-M`, `(:N-M)`) survives
#                     in a citation position. This is the rule that was RED
#                     against the stale bytes, and it stays load-bearing: it is
#                     what stops the rotting notation being reintroduced.
#   C1  section    -- every contract section the discharge cites by heading
#                     exists exactly once in the contract, and the block table's
#                     citation agrees with the block header's own citation. The
#                     record names each section twice; the gate makes that
#                     redundancy a cross-check instead of a second place to rot.
#   C2  arithmetic -- each cited section's DATA-ROW COUNT equals the `rows` value
#                     the block table claims, and the `cells` column sums to the
#                     stated total. Add a row to a contract table and the 51-cell
#                     derivation goes red instead of quietly becoming false.
#   C3  row        -- every cell name the discharge cites resolves to exactly one
#                     data row of its block's contract section. Block 3 cites by
#                     row index, so its indices must be exactly 1..N.
#   C4  migration  -- the `canonical_join()` migration-row count, spelled as an
#                     English word in the discharge record AND in the six
#                     public-surface-inventory rows that point at it, equals the
#                     contract's actual row count. This rule was also RED: both
#                     documents said "eleven" where the contract has ten.
#
# Base R only. No network, no package deps. Usage:
#   Rscript design/work/url-v3/tools/citation-gate.R
#   Rscript design/work/url-v3/tools/citation-gate.R --self-test

# ---- constants --------------------------------------------------------------

CONTRACT <- "design/work/url-v3/contracts/key-join-contracts.md"
DISCHARGE <- "design/work/url-v3/verification/key-join-discharge.md"
INVENTORY <- "design/work/url-v3/registers/public-surface-inventory.md"

# A line-reference token as the stale notation spelled it: a backticked `:80` /
# `:80-95`, or a parenthesized (:175-184) as public-surface-inventory used. Only
# ever applied to CITATION POSITIONS -- block 3's left/right column is full of
# `:80` and `:443` port literals and must never be scanned with this.
LINEREF_RE <- "(`:[0-9]+(-[0-9]+)?`)|(\\(:[0-9]+(-[0-9]+)?\\))"

# C4 compares a spelled count against a derived integer, so it needs the map.
# Deliberately covers well past the current ten: the point is that growing the
# migration table trips the gate rather than outrunning its vocabulary.
NUMBER_WORDS <- c(
  one = 1L, two = 2L, three = 3L, four = 4L, five = 5L, six = 6L,
  seven = 7L, eight = 8L, nine = 9L, ten = 10L, eleven = 11L,
  twelve = 12L, thirteen = 13L, fourteen = 14L, fifteen = 15L,
  sixteen = 16L, seventeen = 17L, eighteen = 18L, nineteen = 19L,
  twenty = 20L
)

# What makes a sentence a pointer at the migration table: this phrase, plus
# either the function name or the contract's short id. Deliberately loose --
# a pointer that a reword can hide from the gate is a pointer the gate does not
# really check.
POINTER_PHRASE <- "migration rows"

# ---- small helpers ----------------------------------------------------------

read_lines_of <- function(path) {
  if (!file.exists(path)) {
    stop("citation-gate: missing file: ", path, call. = FALSE)
  }
  readLines(path, warn = FALSE)
}

# Strip the markdown that decorates a cell but does not identify it. Backticks,
# bold, and surrounding space are presentation; the identifier underneath is
# what a citation denotes.
norm_cell <- function(x) {
  x <- gsub("`", "", x, fixed = TRUE)
  x <- gsub("**", "", x, fixed = TRUE)
  trimws(x)
}

# A citation names the section the way a reader would write it, with the `## `
# that makes it greppable; `sections_of()` keys on the heading text alone. One
# normalizer for both sides, so the two can never drift apart.
norm_heading <- function(x) trimws(sub("^#+\\s*", "", norm_cell(x)))

# A cited row name may carry the open-cell annotation the discharge record adds
# for the reader ("domain spelling (KJ-O2)"); the contract row is named without
# it. Strip a trailing ` (KJ-On)` only -- never a parenthesis that is part of
# the identifier, which is why `url_key_policy()` survives this untouched.
strip_kj_tag <- function(x) trimws(sub("\\s*\\(KJ-O[0-9]+\\)$", "", x))

# The contract spells a join row as its full call, `url_inner_join(x, y, ...)`;
# the discharge cites the function. Reduce both to the leading identifier.
leading_ident <- function(x) sub("\\(.*$", "", x)

# The count word a pointer claims, read from the pointer's own neighbourhood
# rather than from the whole line. public-surface-inventory rows are ~2000
# characters wide and say things like "one named URL column" hundreds of
# characters away; a line-wide search reports those as the migration count. So
# the window is tight and the NEAREST number word wins, which admits both
# shapes in use: "the ten `canonical_join()` migration rows" (count before) and
# "migration rows of G3.K (..., ten rows)" (count after).
count_word_near <- function(text, phrase, before = 40L, after = 80L) {
  at <- regexpr(phrase, text, fixed = TRUE)
  if (at < 0L) {
    return(NA_character_)
  }
  from <- max(1L, at - before)
  to <- min(nchar(text), at + attr(at, "match.length") + after)
  window <- substr(text, from, to)
  re <- paste0("\\b(", paste(names(NUMBER_WORDS), collapse = "|"), ")\\b")
  m <- gregexpr(re, window, ignore.case = TRUE)[[1L]]
  if (m[[1L]] < 0L) {
    return(NA_character_)
  }
  words <- regmatches(window, gregexpr(re, window, ignore.case = TRUE))[[1L]]
  # Distance from each word to the phrase, both measured in the window.
  anchor <- at - from + 1L
  nearest <- which.min(abs(as.integer(m) - anchor))
  words[[nearest]]
}

# The prose paragraph containing line k, joined into one string. C4 searches for
# a count word near a phrase, and in wrapped markdown the two routinely land on
# different lines -- "The ten" ending one line and "migration rows" opening the
# next. Scanning the raw line would report "no count nearby" for a perfectly
# correct sentence. Table rows are their own paragraph: a `|` line is one record
# and must never be glued to its neighbours.
paragraph_text <- function(lines, k) {
  if (!nzchar(trimws(lines[[k]]))) {
    return("")
  }
  if (grepl("^\\s*[|#]", lines[[k]])) {
    return(lines[[k]])
  }
  boundary <- function(i) {
    !nzchar(trimws(lines[[i]])) || grepl("^\\s*[|#]", lines[[i]])
  }
  i <- k
  while (i > 1L && !boundary(i - 1L)) i <- i - 1L
  j <- k
  while (j < length(lines) && !boundary(j + 1L)) j <- j + 1L
  paste(trimws(lines[seq.int(i, j)]), collapse = " ")
}

# Which lines sit inside an HTML comment. These records carry a header comment
# that is deliberately FROZEN -- it narrates the artifact as authored and is not
# swept when the body moves. So it is commentary, not a claim, and C4 must not
# demand a live row count from it.
comment_mask <- function(lines) {
  inside <- FALSE
  out <- logical(length(lines))
  for (k in seq_along(lines)) {
    opens <- grepl("<!--", lines[[k]], fixed = TRUE)
    closes <- grepl("-->", lines[[k]], fixed = TRUE)
    out[[k]] <- inside || opens
    if (opens && !closes) inside <- TRUE
    if (closes) inside <- FALSE
  }
  out
}

split_row <- function(line) {
  cells <- strsplit(line, "|", fixed = TRUE)[[1]]
  # A leading `|` yields an empty first element; a trailing `|` a last one.
  if (length(cells) > 0L && trimws(cells[[1]]) == "") cells <- cells[-1L]
  n <- length(cells)
  if (n > 0L && trimws(cells[[n]]) == "") cells <- cells[-n]
  trimws(cells)
}

is_table_line <- function(line) grepl("^\\s*\\|", line)

is_separator_line <- function(line) grepl("^\\s*\\|[\\s:|-]+\\|\\s*$", line, perl = TRUE)

# Level-2 sections of a markdown file: heading text plus the half-open line
# range it owns. `### ` subheadings stay INSIDE their parent, which is what puts
# `### Note on row 6` inside the truth-table section where it belongs.
sections_of <- function(lines) {
  idx <- grep("^## ", lines)
  if (length(idx) == 0L) {
    return(list())
  }
  ends <- c(idx[-1L] - 1L, length(lines))
  lapply(seq_along(idx), function(i) {
    list(
      heading = norm_cell(sub("^##\\s+", "", lines[[idx[[i]]]])),
      start = idx[[i]],
      end = ends[[i]]
    )
  })
}

# The FIRST pipe table inside [start, end]: header line, separator, then data
# rows until the block breaks. Returns the data-row line numbers, which is the
# only thing every rule here needs.
first_table_rows <- function(lines, start, end) {
  i <- start
  while (i <= end && !is_table_line(lines[[i]])) i <- i + 1L
  if (i > end) {
    return(integer(0))
  }
  header <- i
  sep <- header + 1L
  if (sep > end || !is_separator_line(lines[[sep]])) {
    return(integer(0))
  }
  j <- sep + 1L
  while (j <= end && is_table_line(lines[[j]])) j <- j + 1L
  if (j - 1L < sep + 1L) integer(0) else seq.int(sep + 1L, j - 1L)
}

# ---- contract model ---------------------------------------------------------

# heading -> character vector of first-column row keys, in document order.
contract_model <- function(lines) {
  secs <- sections_of(lines)
  out <- list()
  for (s in secs) {
    rows <- first_table_rows(lines, s$start, s$end)
    keys <- vapply(rows, function(k) {
      cells <- split_row(lines[[k]])
      if (length(cells) == 0L) "" else norm_cell(cells[[1L]])
    }, character(1))
    out[[s$heading]] <- keys
  }
  out
}

# ---- discharge model --------------------------------------------------------

# The block table under `## Where the 51 cells come from`: one row per block
# plus a trailing total row whose `block` cell is empty.
discharge_block_table <- function(lines) {
  secs <- sections_of(lines)
  hit <- Filter(function(s) grepl("^Where the .* cells come from$", s$heading), secs)
  if (length(hit) != 1L) {
    return(NULL)
  }
  s <- hit[[1L]]
  rows <- first_table_rows(lines, s$start, s$end)
  blocks <- list()
  total <- NA_integer_
  total_line <- NA_integer_
  for (k in rows) {
    cells <- split_row(lines[[k]])
    if (length(cells) < 4L) next
    if (norm_cell(cells[[1L]]) == "") {
      total <- suppressWarnings(as.integer(norm_cell(cells[[4L]])))
      total_line <- k
      next
    }
    blocks[[length(blocks) + 1L]] <- list(
      line = k,
      name = norm_cell(cells[[1L]]),
      cited_raw = cells[[2L]],
      cited = norm_heading(cells[[2L]]),
      rows = suppressWarnings(as.integer(norm_cell(cells[[3L]]))),
      cells = suppressWarnings(as.integer(norm_cell(cells[[4L]])))
    )
  }
  list(blocks = blocks, total = total, total_line = total_line)
}

# `### Block 2 -- key-policy rows (16 cells, contract `## Key-policy rows`)`
# plus the block's own table. `cited` is the heading named in the header.
discharge_blocks <- function(lines) {
  idx <- grep("^### Block [0-9]+", lines)
  if (length(idx) == 0L) {
    return(list())
  }
  # A block ends at the next block header or the next level-2 heading.
  stops <- sort(c(idx[-1L], grep("^## ", lines)))
  lapply(idx, function(i) {
    later <- stops[stops > i]
    end <- if (length(later) == 0L) length(lines) else later[[1L]] - 1L
    header <- lines[[i]]
    num <- suppressWarnings(as.integer(sub("^### Block ([0-9]+).*$", "\\1", header)))
    cited <- sub("^.*contract\\s+`([^`]*)`.*$", "\\1", header)
    if (identical(cited, header)) cited <- NA_character_
    rows <- first_table_rows(lines, i, end)
    keys <- vapply(rows, function(k) {
      cells <- split_row(lines[[k]])
      if (length(cells) == 0L) "" else norm_cell(cells[[1L]])
    }, character(1))
    list(
      number = num, line = i, header = header,
      cited = norm_heading(cited), row_lines = rows, keys = keys
    )
  })
}

# ---- citation positions -----------------------------------------------------

# Every place a G3.K citation legitimately appears. C0 scans exactly these and
# nothing else -- which is the whole reason it does not trip over the port
# literals in block 3's left/right column.
citation_positions <- function(dis, inv, block_table, blocks) {
  pos <- list()
  add <- function(file, line, what, text) {
    pos[[length(pos) + 1L]] <<- list(
      file = file, line = line, what = what, text = text
    )
  }
  for (b in block_table$blocks) {
    add(DISCHARGE, b$line, paste0("block-table row '", b$name, "'"), b$cited_raw)
  }
  for (b in blocks) {
    add(DISCHARGE, b$line, paste0("Block ", b$number, " header"), b$header)
    # Block 3 cites by row index; its other columns carry port literals.
    if (!identical(b$number, 3L)) {
      for (k in b$row_lines) {
        cells <- split_row(dis[[k]])
        cell <- if (length(cells) == 0L) "" else cells[[1L]]
        add(DISCHARGE, k, paste0("Block ", b$number, " cell column"), cell)
      }
    }
  }
  # The two residual paragraphs cite a block and a row in prose.
  for (k in grep("[Bb]lock [0-9]", dis)) {
    if (grepl("^### Block", dis[[k]])) next
    add(DISCHARGE, k, "residual prose", dis[[k]])
  }
  # The boundary bullet that points at the migration table.
  for (k in grep("migration rows", dis)) {
    add(DISCHARGE, k, "migration-row pointer", dis[[k]])
  }
  for (k in grep(POINTER_PHRASE, inv, fixed = TRUE)) {
    add(INVENTORY, k, "inventory migration pointer", inv[[k]])
  }
  pos
}

# ---- rules ------------------------------------------------------------------

run_rules <- function(contract, dis, inv) {
  fail <- character(0)
  note <- function(rule, msg) fail <<- c(fail, paste0(rule, ": ", msg))

  model <- contract_model(contract)
  bt <- discharge_block_table(dis)
  blocks <- discharge_blocks(dis)

  if (is.null(bt)) {
    note("C2", "no '## Where the N cells come from' block table found")
    return(fail)
  }
  if (length(blocks) == 0L) {
    note("C3", "no '### Block n' sections found")
    return(fail)
  }

  # -- C0 -- notation ---------------------------------------------------------
  for (p in citation_positions(dis, inv, bt, blocks)) {
    hits <- regmatches(p$text, gregexpr(LINEREF_RE, p$text, perl = TRUE))[[1L]]
    if (length(hits) > 0L) {
      note("C0", sprintf(
        "%s:%d (%s) still carries line-reference %s -- cite the section heading or the row name instead",
        p$file, p$line, p$what, paste(unique(hits), collapse = ", ")
      ))
    }
  }

  # -- C1 -- section resolution ----------------------------------------------
  n_common <- min(length(bt$blocks), length(blocks))
  for (i in seq_len(n_common)) {
    cited <- bt$blocks[[i]]$cited
    if (!nzchar(cited)) {
      note("C1", sprintf(
        "block-table row '%s' cites no contract section", bt$blocks[[i]]$name
      ))
      next
    }
    if (is.null(model[[cited]])) {
      note("C1", sprintf(
        "block-table row '%s' cites section '%s', which the contract does not have",
        bt$blocks[[i]]$name, cited
      ))
    }
    hdr <- blocks[[i]]$cited
    if (is.na(hdr) || !nzchar(hdr)) {
      note("C1", sprintf("Block %d header cites no contract section", blocks[[i]]$number))
    } else if (!identical(hdr, cited)) {
      note("C1", sprintf(
        "Block %d cites '%s' in its header but '%s' in the block table",
        blocks[[i]]$number, hdr, cited
      ))
    }
  }
  if (length(bt$blocks) != length(blocks)) {
    note("C1", sprintf(
      "the block table lists %d blocks but the record has %d '### Block n' sections",
      length(bt$blocks), length(blocks)
    ))
  }

  # -- C2 -- arithmetic -------------------------------------------------------
  for (b in bt$blocks) {
    keys <- model[[b$cited]]
    if (is.null(keys)) next
    if (is.na(b$rows) || length(keys) != b$rows) {
      note("C2", sprintf(
        "block '%s' claims %s rows but contract section '%s' has %d",
        b$name, format(b$rows), b$cited, length(keys)
      ))
    }
  }
  derived <- sum(vapply(bt$blocks, function(b) {
    if (is.na(b$cells)) 0L else b$cells
  }, integer(1)))
  if (is.na(bt$total) || derived != bt$total) {
    note("C2", sprintf(
      "the cells column sums to %d but the table's total says %s",
      derived, format(bt$total)
    ))
  }

  # -- C3 -- row resolution ---------------------------------------------------
  for (b in blocks) {
    keys <- model[[b$cited]]
    if (is.null(keys)) next
    if (identical(b$number, 3L)) {
      got <- suppressWarnings(as.integer(b$keys))
      if (!identical(got, seq_along(keys))) {
        note("C3", sprintf(
          "Block 3 cites rows [%s] but contract section '%s' has rows 1..%d",
          paste(b$keys, collapse = ", "), b$cited, length(keys)
        ))
      }
      next
    }
    pool <- leading_ident(keys)
    for (j in seq_along(b$keys)) {
      want <- leading_ident(strip_kj_tag(b$keys[[j]]))
      n_hit <- sum(pool == want)
      if (n_hit != 1L) {
        note("C3", sprintf(
          "%s:%d Block %d cites row '%s', which resolves to %d rows of '%s'",
          DISCHARGE, b$row_lines[[j]], b$number, b$keys[[j]], n_hit, b$cited
        ))
      }
    }
  }

  # -- C4 -- migration count --------------------------------------------------
  mig <- model[["canonical_join() migration rows"]]
  if (is.null(mig)) {
    note("C4", "the contract has no '## `canonical_join()` migration rows' section")
  } else {
    want <- length(mig)
    # A pointer site is found by MEANING, not by one turn of phrase. Backticks
    # come out first: the same pointer is spelled "`canonical_join()` migration
    # rows" in prose and "`## canonical_join() migration rows`" as a section
    # citation, and a fixed match on either spelling misses the other. That is
    # not hypothetical -- it is how the first cut of this rule went silently
    # green against the very bytes it was written to fail.
    is_pointer <- function(txt) {
      flat <- gsub("`", "", txt, fixed = TRUE)
      grepl(POINTER_PHRASE, flat, fixed = TRUE) &&
        (grepl("canonical_join", flat, fixed = TRUE) ||
          grepl("G3.K", flat, fixed = TRUE))
    }
    # Where a pointer MUST carry a count: every inventory row that names the
    # table, and the discharge's own Boundary section. Elsewhere -- the header
    # comment, say -- naming the table without counting it is legitimate prose,
    # so those sites are checked only if they do state a number.
    bounded <- Filter(
      function(s) startsWith(s$heading, "Boundary"), sections_of(dis)
    )
    in_boundary <- function(k) {
      any(vapply(bounded, function(s) k >= s$start && k <= s$end, logical(1)))
    }
    claims <- list()
    seen <- character(0)
    collect <- function(file, lines, must_count) {
      masked <- comment_mask(lines)
      for (k in seq_along(lines)) {
        if (masked[[k]]) next
        txt <- paragraph_text(lines, k)
        if (!is_pointer(txt)) next
        tag <- paste(file, txt)
        if (tag %in% seen) next
        seen <<- c(seen, tag)
        claims[[length(claims) + 1L]] <<- list(
          file = file, line = k, text = txt, must_count = must_count(k)
        )
      }
    }
    collect(DISCHARGE, dis, in_boundary)
    collect(INVENTORY, inv, function(k) TRUE)

    for (cl in claims) {
      word <- count_word_near(cl$text, POINTER_PHRASE)
      if (is.na(word)) {
        if (cl$must_count) {
          note("C4", sprintf(
            "%s:%d points at the migration rows but states no count nearby -- the count is the checkable part",
            cl$file, cl$line
          ))
        }
        next
      }
      got <- NUMBER_WORDS[[tolower(word)]]
      if (!identical(got, want)) {
        note("C4", sprintf(
          "%s:%d says '%s' migration rows; the contract has %d",
          cl$file, cl$line, word, want
        ))
      }
    }
  }

  fail
}

# ---- self-test --------------------------------------------------------------

# Synthetic fixtures. Small on purpose: each rule gets a green baseline and at
# least one mutation that must turn it red. A gate that has only ever been run
# against passing bytes is an assertion about nothing.
FIXTURE_CONTRACT <- c(
  "# c",
  "",
  "## Alpha rows",
  "",
  "| surface | status |",
  "|---|---|",
  "| `alpha_one(x, ...)` | SETTLED |",
  "| alpha two | SETTLED |",
  "",
  "## Beta rows",
  "",
  "| dimension | status |",
  "|---|---|",
  "| beta one | SETTLED |",
  "| beta two | SETTLED |",
  "| beta three | SETTLED |",
  "",
  "## `canonical_join()` migration rows",
  "",
  "| phase | status |",
  "|---|---|",
  "| mig one | SETTLED |",
  "| mig two | SETTLED |",
  ""
)

FIXTURE_DISCHARGE <- c(
  "# d",
  "",
  "## Where the 5 cells come from",
  "",
  "| block | contract table | rows | cells |",
  "|---|---|---:|---:|",
  "| alpha | `## Alpha rows` | 2 | 2 |",
  "| beta | `## Beta rows` | 3 | 3 |",
  "| | | | **5** |",
  "",
  "## Cell map",
  "",
  "### Block 1 -- alpha (2 cells, contract `## Alpha rows`)",
  "",
  "| cell | evidence |",
  "|---|---|",
  "| `alpha_one` | e |",
  "| alpha two (KJ-O1) | e |",
  "",
  "### Block 2 -- beta (3 cells, contract `## Beta rows`)",
  "",
  "| cell | evidence |",
  "|---|---|",
  "| beta one | e |",
  "| beta two | e |",
  "| beta three | e |",
  "",
  "## Boundary",
  "",
  "- Its `## canonical_join() migration rows` table holds two rows, all",
  "  outside this record.",
  ""
)

FIXTURE_INVENTORY <- c(
  "# i",
  "",
  "| export | note |",
  "|---|---|",
  "| j | among G3.K's two `## canonical_join() migration rows`, outside VD-001 |",
  ""
)

self_test <- function() {
  pass <- 0L
  fail <- 0L
  check <- function(label, ok) {
    if (isTRUE(ok)) {
      pass <<- pass + 1L
    } else {
      fail <<- fail + 1L
      cat("  FAIL: ", label, "\n", sep = "")
    }
  }
  # A mutation is red when at least one failure carries the expected rule id.
  red_for <- function(label, rule, con = FIXTURE_CONTRACT,
                      dis = FIXTURE_DISCHARGE, inv = FIXTURE_INVENTORY) {
    got <- run_rules(con, dis, inv)
    check(label, any(startsWith(got, paste0(rule, ":"))))
  }
  sub_line <- function(x, pattern, replacement) {
    k <- grep(pattern, x)[[1L]]
    x[[k]] <- sub(pattern, replacement, x[[k]])
    x
  }

  base <- run_rules(FIXTURE_CONTRACT, FIXTURE_DISCHARGE, FIXTURE_INVENTORY)
  check("baseline fixture is green", length(base) == 0L)

  # -- C0 --
  red_for(
    "C0 fires on a backticked line ref in a block-table row", "C0",
    dis = sub_line(FIXTURE_DISCHARGE, "`## Alpha rows`", "`## Alpha rows` (`:5-6`)")
  )
  red_for(
    "C0 fires on a line ref in a block header", "C0",
    dis = sub_line(
      FIXTURE_DISCHARGE, "^### Block 1 -- alpha \\(2 cells",
      "### Block 1 -- alpha (2 cells, contract `:5-6`) ("
    )
  )
  red_for(
    "C0 fires on a line ref in a cell column", "C0",
    dis = sub_line(FIXTURE_DISCHARGE, "^\\| beta one \\|", "| beta one (`:13`) |")
  )
  red_for(
    "C0 fires on a parenthesized line ref in the inventory", "C0",
    inv = sub_line(FIXTURE_INVENTORY, "outside VD-001", "(:20-21), outside VD-001")
  )
  ports_dis <- append(
    FIXTURE_DISCHARGE,
    c(
      "### Block 3 -- ports (2 cells, contract `## Alpha rows`)",
      "",
      "| row | left / right |",
      "|---|---|",
      "| 1 | HTTP absent / HTTP `:80` |",
      "| 2 | HTTPS absent / HTTPS `:443` |",
      ""
    ),
    after = length(FIXTURE_DISCHARGE) - 4L
  )
  ports_out <- run_rules(FIXTURE_CONTRACT, ports_dis, FIXTURE_INVENTORY)
  check(
    "C0 does NOT fire on a port literal in block 3's own column",
    !any(startsWith(ports_out, "C0:"))
  )

  # -- C1 --
  red_for(
    "C1 fires when the block table cites a section the contract lacks", "C1",
    dis = sub_line(FIXTURE_DISCHARGE, "`## Alpha rows` \\| 2", "`## Missing rows` | 2")
  )
  red_for(
    "C1 fires when header and block table disagree", "C1",
    dis = sub_line(
      FIXTURE_DISCHARGE, "### Block 1 -- alpha \\(2 cells, contract `## Alpha rows`\\)",
      "### Block 1 -- alpha (2 cells, contract `## Beta rows`)"
    )
  )
  red_for(
    "C1 fires when a block has no cited section", "C1",
    dis = sub_line(
      FIXTURE_DISCHARGE, "### Block 2 -- beta \\(3 cells, contract `## Beta rows`\\)",
      "### Block 2 -- beta (3 cells)"
    )
  )

  # -- C2 --
  red_for(
    "C2 fires when a claimed row count is wrong", "C2",
    dis = sub_line(FIXTURE_DISCHARGE, "`## Beta rows` \\| 3", "`## Beta rows` | 4")
  )
  red_for(
    "C2 fires when the contract grows a row the block table has not seen", "C2",
    con = append(FIXTURE_CONTRACT, "| beta four | SETTLED |", after = 15L)
  )
  red_for(
    "C2 fires when the cells column does not sum to the total", "C2",
    dis = sub_line(FIXTURE_DISCHARGE, "\\*\\*5\\*\\*", "**6**")
  )

  # -- C3 --
  red_for(
    "C3 fires on a cited row the contract section does not have", "C3",
    dis = sub_line(FIXTURE_DISCHARGE, "^\\| beta two \\|", "| beta nine |")
  )
  red_for(
    "C3 fires when a contract row is renamed under the citation", "C3",
    con = sub_line(FIXTURE_CONTRACT, "^\\| beta three \\|", "| beta drei |")
  )
  check(
    "C3 accepts the `(KJ-On)` annotation the record adds for the reader",
    !any(startsWith(base, "C3:"))
  )
  red_for(
    "C3 fires when block 3's indices are not 1..N", "C3",
    dis = append(
      FIXTURE_DISCHARGE,
      c(
        "### Block 3 -- ports (2 cells, contract `## Alpha rows`)",
        "",
        "| row | left / right |",
        "|---|---|",
        "| 1 | a |",
        "| 3 | b |",
        ""
      ),
      after = length(FIXTURE_DISCHARGE) - 4L
    )
  )

  # -- C4 --
  red_for(
    "C4 fires when the discharge miscounts the migration rows", "C4",
    dis = sub_line(FIXTURE_DISCHARGE, "table holds two rows", "table holds three rows")
  )
  red_for(
    "C4 fires when the inventory miscounts the migration rows", "C4",
    inv = sub_line(FIXTURE_INVENTORY, "G3.K's two", "G3.K's eleven")
  )
  red_for(
    "C4 fires when the contract grows a migration row", "C4",
    con = append(FIXTURE_CONTRACT, "| mig three | SETTLED |", after = 21L)
  )
  red_for(
    "C4 fires when a pointer states no count at all", "C4",
    inv = sub_line(FIXTURE_INVENTORY, "G3.K's two", "G3.K's")
  )
  # The count and the phrase routinely wrap onto different lines. C4 reads the
  # paragraph, not the line, or a correct sentence would report as uncounted.
  wrapped <- FIXTURE_DISCHARGE
  wk <- grep("table holds two rows", wrapped)[[1L]]
  wrapped <- append(
    wrapped[-wk],
    c("- Its `## canonical_join() migration rows` table", "  holds two rows, all"),
    after = wk - 1L
  )
  check(
    "C4 reads across a wrapped paragraph rather than a single line",
    !any(startsWith(run_rules(FIXTURE_CONTRACT, wrapped, FIXTURE_INVENTORY), "C4:"))
  )
  # The frozen header comment names the table without counting it. That is
  # legitimate prose, not an uncounted pointer.
  commented <- append(
    FIXTURE_DISCHARGE,
    c(
      "<!-- narrates exactly one deferral, including the",
      "     `canonical_join()` migration rows, as authored. -->",
      ""
    ),
    after = 1L
  )
  check(
    "C4 ignores a frozen header comment that names the table",
    !any(startsWith(run_rules(FIXTURE_CONTRACT, commented, FIXTURE_INVENTORY), "C4:"))
  )

  cat(sprintf("citation-gate self-test: %d passed, %d failed\n", pass, fail))
  if (fail > 0L) quit(status = 1L)
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(NULL))
  }
  failures <- run_rules(
    read_lines_of(CONTRACT), read_lines_of(DISCHARGE), read_lines_of(INVENTORY)
  )
  if (length(failures) == 0L) {
    cat("citation-gate: PASS -- C0..C4, every G3.K citation resolves\n")
    return(invisible(NULL))
  }
  cat("citation-gate: FAIL\n")
  for (f in failures) cat("  ", f, "\n", sep = "")
  cat(sprintf("citation-gate: %d failure(s)\n", length(failures)))
  quit(status = 1L)
}

main()
