#!/usr/bin/env Rscript
# pin-inventory.R — the enforced-vs-documentary hash-pin inventory (RURL-kufiwlxc).
#
# THE PROBLEM THIS REMOVES. Before editing a control-plane artifact you need to
# know its blast radius: which recorded sha256 pins go stale, and which of those
# are actually RECOMPUTED by a validator. `grep -rl <filename>` cannot answer
# that. It counts every mention, including hash tables that are deliberately
# frozen history, so it overstates the radius (measured ~2.5x for the
# validation-intervention contract: 5 grep hits, 4 enforced sites) — and in the
# other direction it invites "helpfully" re-pinning a table that is MEANT to
# drift, which destroys the tamper-evidence the mechanism exists to produce.
#
# THE ENFORCEMENT MODEL, mirrored from the validators (not re-decided here):
#
#   manifest.yaml `decisions[]` / `artifacts[]` / `registers[]`
#       ENFORCED by validate-manifest.R. `artifacts[]` is enforced a SECOND
#       time by validate-records.R (`[manifest_hash_drift]`), so an artifact
#       hash drift surfaces as two failures, not one.
#
#   contracts/*.md  `## Inputs`
#       ENFORCED by validate-records.R (contract-family section; it globs
#       contracts/ and recomputes every row).
#
#   gates/*.md  `## Inputs`
#       ENFORCED by validate-records.R — UNLESS the envelope declares
#       `state: SUPERSEDED`, which is deliberately NOT hash-checked. Such a
#       record is retained history describing inputs as they stood at ITS OWN
#       acceptance commit; drift against today's tree is expected rather than a
#       defect (see the SUPERSESSION comment in validate-records.R, RURL-kpyapioi).
#       Its body prose is baseline-scoped: it may still say it is recomputed
#       "on every run", which was true when it was accepted. Do not refresh it.
#
#   verification/*.md  `## Inputs`
#       DOCUMENTARY: no validator globs design/work/url-v3/verification/ yet.
#       The verification-family validator section stages with the sealing G4
#       snapshot. Until then these tables are authored, not enforced — which is
#       what let traceability-map.md's table silently go stale before P0.6
#       re-derived it.
#
# So the rule is a DIRECTORY + LIFECYCLE rule, not a "looks like a hash table"
# rule, and that is exactly what a grep cannot see.
#
# Usage:
#   Rscript design/work/url-v3/tools/pin-inventory.R
#       full inventory, grouped by pinned path.
#   Rscript design/work/url-v3/tools/pin-inventory.R <path-or-substring>
#       blast radius for one artifact.
#   Rscript design/work/url-v3/tools/pin-inventory.R --drift
#       only paths whose recorded hash no longer matches disk.
#   Rscript design/work/url-v3/tools/pin-inventory.R --self-test
#       prove the model against the validators themselves: every ENFORCED and
#       drifted pin this tool reports must appear as a real validator failure,
#       and no DOCUMENTARY drift may. Exits non-zero if the model is wrong.
#
# This tool is READ-ONLY. It never rewrites a hash.

ok <- requireNamespace("digest", quietly = TRUE)
if (!ok) stop("pin-inventory.R needs the 'digest' package")

ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=",
  commandArgs(trailingOnly = FALSE), value = TRUE)[1])), ".."), mustWork = TRUE)
REPO <- normalizePath(file.path(ROOT, "..", "..", ".."), mustWork = TRUE)

sha256_of <- function(p) digest::digest(file = p, algo = "sha256")

## Records a single pin site: who pins what, and whether a validator recomputes it.
pins <- list()
add_pin <- function(target, site, enforced, validator, reason, recorded,
                    kind, sitefile) {
  pins[[length(pins) + 1L]] <<- list(
    target = target, site = site, enforced = enforced,
    validator = validator, reason = reason, recorded = recorded,
    kind = kind, sitefile = sitefile)
}

## The failure string each validator emits when THIS site's pin drifts. Site-
## attributed on purpose: asserting only that a target is mentioned somewhere
## cannot tell a correct model from one that mislabels which SITE enforces it.
expected_failures <- function(kind, sitefile, target) {
  switch(kind,
    manifest = if (identical(sitefile, "artifacts")) c(
        sprintf("artifacts: sha256 mismatch for %s", target),
        sprintf("[manifest_hash_drift] %s", target))
      else sprintf("%s: sha256 mismatch for %s", sitefile, target),
    gates = sprintf("gates: %s input hash drift for %s", sitefile, target),
    contracts = sprintf("contracts %s: ## Inputs hash drift for %s", sitefile, target),
    verification = character(0),
    character(0))
}

## --- 1. manifest.yaml -------------------------------------------------------
## Parsed line-wise rather than via yaml::, so the reported site keeps the
## top-level list name (decisions/artifacts/registers) the reader needs.
mpath <- file.path(ROOT, "manifest.yaml")
mln <- readLines(mpath, warn = FALSE)
cur_key <- NA_character_
cur_path <- NA_character_
for (l in mln) {
  if (grepl("^[a-z_]+:", l)) {
    cur_key <- sub(":.*$", "", l)
    cur_path <- NA_character_
    next
  }
  p <- regmatches(l, regexpr("(?<=path:\\s)\\S+", l, perl = TRUE))
  if (length(p)) cur_path <- p
  h <- regmatches(l, regexpr("(?<=sha256:\\s)[0-9a-f]{64}", l, perl = TRUE))
  if (length(h) && !is.na(cur_path)) {
    dbl <- identical(cur_key, "artifacts")
    add_pin(cur_path, sprintf("manifest.yaml `%s[]`", cur_key), TRUE,
            if (dbl) "validate-manifest.R + validate-records.R" else "validate-manifest.R",
            if (dbl) "hashed list; artifacts[] is checked twice" else "hashed list",
            h, "manifest", cur_key)
    cur_path <- NA_character_
  }
}

## --- 2. `## Inputs` tables --------------------------------------------------
inputs_rows <- function(f) {
  ln <- readLines(f, warn = FALSE)
  h <- which(grepl("^##\\s+Inputs\\s*$", ln))
  if (!length(h)) return(NULL)
  nxt <- which(grepl("^##\\s+", ln) & seq_along(ln) > h[1])
  blk <- ln[h[1]:(if (length(nxt)) nxt[1] - 1L else length(ln))]
  rows <- blk[grepl("^\\|", blk) & grepl("[0-9a-f]{64}", blk)]
  if (!length(rows)) return(NULL)
  do.call(rbind, lapply(rows, function(r) {
    cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", r)), "|", fixed = TRUE)[[1]])
    p <- gsub("`", "", cells[1])
    hh <- regmatches(r, regexpr("[0-9a-f]{64}", r))
    data.frame(path = p, sha = hh, stringsAsFactors = FALSE)
  }))
}

envelope_state <- function(f) {
  ln <- readLines(f, warn = FALSE)
  hit <- grep("^\\|\\s*state\\s*\\|", ln, value = TRUE)
  if (!length(hit)) return(NA_character_)
  trimws(strsplit(hit[1], "|", fixed = TRUE)[[1]][3])
}

scan_dir <- function(sub) {
  d <- file.path(ROOT, sub)
  if (!dir.exists(d)) return(invisible(NULL))
  for (f in sort(list.files(d, pattern = "\\.md$", full.names = TRUE))) {
    tb <- inputs_rows(f)
    if (is.null(tb)) next
    st <- envelope_state(f)
    superseded <- identical(st, "SUPERSEDED")
    enforced <- (sub == "contracts") || (sub == "gates" && !superseded)
    reason <- if (sub == "verification") {
      "no validator globs verification/ yet (stages with the G4 seal)"
    } else if (superseded) {
      "envelope state: SUPERSEDED — frozen history, deliberately not recomputed"
    } else {
      sprintf("%s/ is globbed and recomputed on every run", sub)
    }
    for (i in seq_len(nrow(tb))) {
      add_pin(tb$path[i], sprintf("%s/%s `## Inputs`", sub, basename(f)),
              enforced, if (enforced) "validate-records.R" else "(none)",
              reason, tb$sha[i], sub, basename(f))
    }
  }
}
for (s in c("contracts", "gates", "verification")) scan_dir(s)

## --- 3. resolve drift -------------------------------------------------------
df <- do.call(rbind, lapply(pins, function(p) as.data.frame(p, stringsAsFactors = FALSE)))
df$actual <- vapply(df$target, function(t) {
  fp <- file.path(REPO, t)
  if (file.exists(fp)) sha256_of(fp) else NA_character_
}, character(1))
df$drifted <- !is.na(df$actual) & df$recorded != df$actual

args <- commandArgs(trailingOnly = TRUE)

## --- 4. self-test: prove the model against the validators -------------------
if ("--self-test" %in% args) {
  run <- function(script, extra = character()) {
    out <- suppressWarnings(system2("Rscript", c(file.path(ROOT, "tools", script), extra),
                                    stdout = TRUE, stderr = TRUE))
    paste(out, collapse = "\n")
  }
  recs <- run("validate-records.R")
  man <- run("validate-manifest.R", mpath)
  both <- paste(recs, man, sep = "\n")

  fails <- 0L
  checked <- 0L
  say <- function(tag, msg) cat(sprintf("  %-5s %s\n", tag, msg))

  drifted <- df[df$drifted, , drop = FALSE]
  for (i in seq_len(nrow(drifted))) {
    r <- drifted[i, ]
    pats <- expected_failures(r$kind, r$sitefile, r$target)
    if (!length(pats)) {
      ## An "enforced" claim we cannot attribute to a validator failure string
      ## is unverifiable, so it fails rather than being skipped: that is the
      ## hole through which a mislabelled directory would otherwise pass.
      if (r$enforced) {
        fails <- fails + 1L
        checked <- checked + 1L
        say("FAIL", sprintf("enforced pin has no known failure string, model unverifiable: %s",
                            r$site))
      }
      next
    }
    checked <- checked + 1L
    seen <- vapply(pats, function(p) grepl(p, both, fixed = TRUE), logical(1))
    if (r$enforced) {
      ## Each site must produce ITS OWN attributed failure.
      if (all(seen)) {
        say("PASS", sprintf("%s -> %s", r$site, basename(r$target)))
      } else {
        fails <- fails + 1L
        say("FAIL", sprintf("enforced pin emits no attributed failure: %s -> %s",
                            r$site, r$target))
      }
    } else {
      ## A documentary site must stay silent even when its own pin is stale.
      if (any(seen)) {
        fails <- fails + 1L
        say("FAIL", sprintf("documentary pin wrongly enforced: %s -> %s",
                            r$site, r$target))
      } else {
        say("PASS", sprintf("silent, as designed: %s -> %s", r$site, basename(r$target)))
      }
    }
  }
  if (!checked) say("PASS", "no drifted pins to cross-check (tree is pinned clean)")

  cat(sprintf("\nSELF-TEST: %s\n", if (fails == 0L) "PASS" else sprintf("FAIL (%d)", fails)))
  quit(status = if (fails == 0L) 0L else 1L)
}

## --- 5. report --------------------------------------------------------------
sel <- df
if ("--drift" %in% args) sel <- sel[sel$target %in% unique(sel$target[sel$drifted]), ]
q <- args[!grepl("^--", args)]
if (length(q)) sel <- sel[grepl(q[1], sel$target, fixed = TRUE), ]

cat("ENFORCED-PIN INVENTORY (RURL-kufiwlxc)\n")
cat(sprintf("  %d pin sites over %d distinct targets; %d enforced, %d documentary\n\n",
            nrow(sel), length(unique(sel$target)), sum(sel$enforced), sum(!sel$enforced)))

if (!nrow(sel)) {
  cat("  (no pin sites match)\n")
} else {
  for (t in unique(sel$target)) {
    s <- sel[sel$target == t, ]
    cat(sprintf("%s\n", t))
    ## The badge distinguishes the two kinds of drift, because only one of them
    ## means anything: enforced drift turns a gate red and must be explained,
    ## documentary drift is expected and must NOT be "fixed" by re-pinning.
    badge <- if (any(s$drifted & s$enforced)) {
      "  [ENFORCED DRIFT — a gate is red for this]"
    } else if (any(s$drifted)) {
      "  [documentary drift only — no gate reacts; do not re-pin]"
    } else {
      ""
    }
    cat(sprintf("  blast radius: %d enforced site(s)%s\n", sum(s$enforced), badge))
    for (i in seq_len(nrow(s))) {
      cat(sprintf("    %-11s %-46s %s\n",
                  if (s$enforced[i]) "ENFORCED" else "documentary",
                  s$site[i],
                  if (s$drifted[i]) "drift" else "match"))
      cat(sprintf("      %s%s\n", if (s$enforced[i]) sprintf("via %s; ", s$validator[i]) else "",
                  s$reason[i]))
    }
    cat("\n")
  }
}
