# The v3 identity-keyed join family -- the engine, plus the six exported
# wrappers at the foot of the file.
#
# `registers/verification-deferrals.md` VD-001 probes this surface with
# `export:get_url_key;...;export:url_anti_join`, and deferral-gate D3 fires the
# moment ANY of those eight names appears in NAMESPACE -- at which point all 51
# of VD-001's cells must be covered in the same change (P0.5 failure condition
# 3). The probes are `export:` probes, matched as `^export\(<name>\)$` against
# NAMESPACE (`tools/deferral-gate.R:100-121`), so the engine was invisible to
# them while it landed first. All eight wrappers then landed in ONE change
# together with `verification/key-join-discharge.md`, which claims
# `DISCHARGED[VD-001]`. Output surface (b) did the same: VD-002 records the
# serializers shipping in `df00da8` while public `serialize_url()` waited for
# P2.5, with D3 green throughout.
#
# Contract discipline (design/work/url-v3/contracts/key-join-contracts.md,
# "Six-join matrix" and "Cross-cutting join rows"; P3.1 D-C/D-D; P3.2 D-C..D-H):
#
#   * Equality is the comparison KEY's, never a presentation string. This file
#     never touches `clean_url`, and it never re-derives identity -- it consumes
#     `.url_key_compute_vec()` and compares the framed bytes. That is what keeps
#     `canonical_join()`'s legacy defect (identity == cleaned display) from
#     being inherited.
#   * ONE immutable, symmetric policy is applied to both sides. Side-specific
#     rules are prohibited because equality must stay symmetric and transitive
#     (`:66`).
#   * Row order is PINNED per join, not inherited from `merge()` /
#     `data.frame()` incidental behavior. `canonical_join()` uses
#     `merge(sort = FALSE)`, whose order is documented as unspecified; this
#     family builds index vectors and never calls `merge()`.
#   * Rows are NEVER discarded to "resolve" duplicate keys. Duplicate keys are a
#     multiplicity fact owned by `relationship` / `multiple` -- the fix for the
#     shipped `collision = "first"` silent row-discard (P3.1 D-D).
#   * Results are built by ROW-SLICING `x`'s prototype, and every zero-row
#     result carries the complete typed would-be schema (P3.2 D-H).

.URL_JOIN_TYPES <- c("inner", "left", "right", "full", "semi", "anti")

# Independent rurl contract (P3.1 D-D: the family is pinned as an independent
# rurl contract, explicitly NOT a promise about an unspecified dplyr version).
# Constraints are stated over ELIGIBLE keys, which is the only set the contract
# makes claims about:
#
#   none          no constraint and no check (default)
#   one-to-one    every matching key appears at most once on each side
#   one-to-many   every matching key appears at most once in `x`
#   many-to-one   every matching key appears at most once in `y`
#   many-to-many  no constraint, declared explicitly rather than by omission
.URL_JOIN_RELATIONSHIPS <- c(
  "none", "one-to-one", "one-to-many", "many-to-one", "many-to-many"
)

# `all` is the default and is lossless. `first`/`last` are the separately named
# lossy narrowings P3.1 D-D requires, with order defined as `y`'s row order (and
# `x`'s row order for the y-primary right join). They deliberately do NOT reuse
# the shipped `collision` dial, whose `"first"` default silently discarded rows.
.URL_JOIN_MULTIPLE <- c("all", "first", "last")

# The two axes that replace the misleading `on_parse_error` (P3.1 D-D, C7):
# non-keyable rows and warning rows are separate concerns and get separate
# dials. Neither reuses `on_parse_error`, and neither is a cleaning dial.
.URL_JOIN_INVALID <- c("keep", "drop", "error")
.URL_JOIN_WARNINGS <- c("allow", "reject", "error")

# The eligibility vocabulary (`:122`, P3.1 D-C). All seven terms are enumerated
# because the vocabulary is SETTLED and must not be silently narrowed.
#
# MEASURED GAP: `unsupported-scheme` has NO instance under this family's parse
# posture. It projects L2 `rejected-scheme`, and the key's posture is
# `scheme_acceptance = "general"`, under which no scheme is rejected -- measured
# as 0 of 483 corpus rows. The term is enumerated and the gap is ASSERTED in
# `test-url-join.R` rather than left silent, exactly as the L3 annotation-state
# enum gap is asserted in `test-parse-verdicts.R`.
.URL_JOIN_ELIGIBILITY <- c(
  "ok", "warning", "invalid-parse", "unsupported-scheme",
  "relative-or-opaque-reference", "missing-input", "empty-input"
)

# --- typed conditions --------------------------------------------------------

# Stable typed condition classes (`:164`). Every class below has a reachable
# trigger; a class with no trigger would be a promise the code does not keep.
#
# The "unmatched" condition the same contract row lists is DROPPED, by owner
# ruling (P3.3 section 2, RURL-kcgsuzll), not merely unimplemented. The six
# joins already express unmatched-ness structurally, as `NA` on the non-primary
# side -- which is what a join is. A condition or an axis would re-report the
# result's own shape as a diagnostic, and a caller who wants the count already
# has it (`is.na()` over the key column, or `relationship`).
.url_join_abort <- function(msg, class) {
  stop(errorCondition(
    msg, class = c(class, "rurl_url_join_error"), call = NULL
  ))
}

# --- per-side state ----------------------------------------------------------

# One side's join state: the comparison key plus the two independent predicates
# the axes consult.
#
# `keyable` is the KEY's own verdict and is authoritative for "can this row
# match at all". `warned` is the refinement the `warnings` axis needs, and it
# rides the SHIPPED verdict layers (R/verdicts.R) read off the SAME parse pass
# under the SAME options object -- not a second, parallel notion of validity.
# That matters: `parse_status` is literally pi(L1, L2, L3), so consuming the
# layers is consuming the one derivation rather than re-deriving a 16-argument
# call site, which is precisely how a compensation gate silently narrows.
#
# The agreement `L1 == "pass"` <-> `keyable` is MEASURED (0 disagreements over a
# 483-row corpus, both classes populated) and asserted as a test. It is a
# measured agreement, not a proof, which is why `keyable` -- not L1 -- remains
# the authority for matching.
.url_join_side_state <- function(url, policy, engine = NULL) {
  url <- if (is.factor(url)) as.character(url) else url
  key <- .url_key_compute_vec(url, policy, engine)
  reason <- attr(key, "keyability")
  keyable <- reason == "ok"

  n <- length(url)
  if (n == 0L) {
    return(list(key = key, keyable = logical(0), warned = logical(0)))
  }

  opts <- .url_key_parse_options(policy, engine)
  v <- attr(._parse_urls_cached(unname(as.character(url)), opts), "verdicts")

  # A warning row is parseable but carries an accept-with-note: userinfo on a
  # scheme-less input (L2) or a host whose PSL annotation did not resolve (L3).
  # These are exactly the rows the legacy `join_parse_status = "ok_or_warning"`
  # dial governed, now a first-class axis instead of a parse-status string test.
  #
  # NOTE the one asymmetry `engine` introduces. It cannot move a key byte -- the
  # key frames no PSL-derived component, and that is asserted -- so identity
  # non-interference holds unconditionally. But L3 IS a PSL annotation, so a
  # divergent suffix list could in principle move `warned`, and therefore which
  # rows are eligible under `warnings = "reject"`. Identity stays engine-
  # independent; ELIGIBILITY does not, and that distinction is deliberate rather
  # than overlooked.
  warned <- keyable &
    (v$layer2_policy_verdict == "warn-userinfo" |
       v$layer3_annotation_state == "unknown")

  list(key = key, keyable = keyable, warned = warned)
}

# Project the seven-term eligibility vocabulary. Precedence-ordered so each row
# carries exactly one label, and DIAGNOSTIC ONLY: the join consults `keyable`
# and `warned`, never this label, so a precedence choice here can never decide
# which rows match. (Ordering it the other way would let a scheme-relative row
# hide a warning from the `warnings` axis.)
#
# Deliberately NOT called by the join path -- it is the companion diagnostic
# surface (`:164`), and computing it per join would buy nothing but a third
# parse of the same input.
.url_join_eligibility_vec <- function(url, policy = .url_key_policy_spec(),
                                      engine = NULL) {
  url <- if (is.factor(url)) as.character(url) else url
  if (length(url) == 0L) {
    return(character(0))
  }
  reason <- attr(.url_key_compute_vec(url, policy, engine), "keyability")
  out <- rep("ok", length(url))

  opts <- .url_key_parse_options(policy, engine)
  v <- attr(._parse_urls_cached(unname(as.character(url)), opts), "verdicts")
  rec <- .fsss_record_vec(url, policy$standard, engine, opts = opts)
  presence <- .url_key_scheme_presence(
    url, opts$url_standard,
    looks_like_host_port = ._parse_stage_a_vec(url, opts)$looks_like_host_port
  )

  # Keyable-but-not-a-resolved-web-reference, then the warning refinement, then
  # the fatal states -- fatal last so they always win.
  relative_opaque <- reason == "ok" &
    (presence != "explicit" |
       (!is.na(rec$path_kind) & rec$path_kind == "opaque"))
  out[relative_opaque] <- "relative-or-opaque-reference"
  out[reason == "ok" &
        (v$layer2_policy_verdict == "warn-userinfo" |
           v$layer3_annotation_state == "unknown")] <- "warning"
  out[v$layer2_policy_verdict == "rejected-scheme"] <- "unsupported-scheme"
  out[reason == "invalid-parse"] <- "invalid-parse"
  out[reason == "empty-input"] <- "empty-input"
  out[reason == "missing-input"] <- "missing-input"
  out
}

# Apply the `invalid` and `warnings` axes to one side, returning the surviving
# row indices plus the per-row match eligibility.
#
# The two axes are genuinely independent: `invalid` governs NON-KEYABLE rows and
# `warnings` governs KEYABLE-but-noted rows, and no row is in both sets by
# construction (`warned` is masked by `keyable`).
#
# `reject` makes a warning row INELIGIBLE TO MATCH without removing it, so the
# join type's own retention rule still applies (a left join keeps it,
# unmatched).
# That is the same shape as a non-keyable row under `invalid = "keep"`, and it
# is what keeps "rejected from the match set" from being conflated with
# "dropped from the result".
.url_join_apply_axes <- function(state, invalid, warnings, side) {
  n <- length(state$keyable)
  if (invalid == "error" && !all(state$keyable)) {
    bad <- which(!state$keyable)
    .url_join_abort(
      sprintf(
        paste0(
          "`%s` has %d non-keyable row(s) and `invalid = \"error\"`. ",
          "Reasons: %s. Row(s): %s. Use invalid = \"keep\" or \"drop\"."
        ),
        side, length(bad),
        toString(sort(unique(attr(state$key, "keyability")[bad]))),
        .url_join_row_digest(bad)
      ),
      "rurl_url_join_invalid_error"
    )
  }
  if (warnings == "error" && any(state$warned)) {
    bad <- which(state$warned)
    .url_join_abort(
      sprintf(
        paste0(
          "`%s` has %d warning row(s) and `warnings = \"error\"`. ",
          "Row(s): %s. Use warnings = \"allow\" or \"reject\"."
        ),
        side, length(bad), .url_join_row_digest(bad)
      ),
      "rurl_url_join_warning_error"
    )
  }

  keep <- rep(TRUE, n)
  if (invalid == "drop") {
    keep <- state$keyable
  }
  eligible <- state$keyable
  if (warnings == "reject") {
    eligible <- eligible & !state$warned
  }
  list(keep = which(keep), eligible = eligible)
}

# Report row POSITIONS, never row content. Conditions must not leak credentials
# (`:164`), and a source URL can carry userinfo -- which the key itself cannot,
# since ratification Q5 excludes userinfo from identity entirely.
.url_join_row_digest <- function(idx, max_n = 5L) {
  shown <- utils::head(idx, max_n)
  paste0(
    toString(shown),
    if (length(idx) > length(shown)) {
      sprintf(", ... (%d more)", length(idx) - length(shown))
    } else {
      ""
    }
  )
}

# --- argument validation -----------------------------------------------------

# KJ-O5 / P3.2 D-E: a single `by`, using the named-vector idiom. A bare string
# names one column present on both sides; a length-one NAMED vector maps left to
# right as `c(x_col = "y_col")`. There is deliberately no `col_x`/`col_y` pair:
# this idiom generalizes to multi-column keying later by lengthening the vector,
# without a signature break.
.url_join_resolve_by <- function(by, x, y) {
  if (!is.character(by) || length(by) != 1L || is.na(by) || !nzchar(by)) {
    .url_join_abort(
      paste0(
        "`by` must be a length-one, non-NA, non-empty character vector: ",
        "either \"col\" (the same column on both sides) or the named form ",
        "c(x_col = \"y_col\")."
      ),
      "rurl_url_join_input_error"
    )
  }
  nm <- names(by)
  x_col <- if (is.null(nm) || is.na(nm) || !nzchar(nm)) by[[1L]] else nm
  y_col <- by[[1L]]

  .url_join_check_column(x, x_col, "x")
  .url_join_check_column(y, y_col, "y")
  list(x = x_col, y = y_col)
}

.url_join_check_column <- function(data, col, side) {
  if (!col %in% names(data)) {
    .url_join_abort(
      sprintf("`by` names column \"%s\", which is not present in `%s`.",
              col, side),
      "rurl_url_join_input_error"
    )
  }
  v <- data[[col]]
  if (!is.character(v) && !is.factor(v)) {
    .url_join_abort(
      sprintf(
        "Join column \"%s\" in `%s` must be character or factor, not %s.",
        col, side, class(v)[[1L]]
      ),
      "rurl_url_join_input_error"
    )
  }
  invisible(TRUE)
}

# KJ-O6 / P3.2 D-F, verbatim: a single deterministic rurl repair algorithm,
# `suffix = c(".x", ".y")` by default, and ambiguity is an EARLY ERROR rather
# than a silent repair. The four numbered steps below are D-F's four steps.
#
# One reading D-F leaves to the implementation: whether `y`'s join column is a
# "key column" (excluded from suffixing) or a contributed `y` column. It is
# treated as CONTRIBUTED, because the key is the identity key and NOT the URL
# string -- `x`'s and `y`'s URL strings are distinct data that merely compare
# equal. Excluding it would drop `y`'s original URL whenever both sides use the
# same column name, contradicting the settled "preserve both originals" row
# (`:159`). So `by = "URL"` yields `URL.x` / `URL.y`, and differing names are
# both kept unsuffixed.
.url_join_name_plan <- function(x, y, suffix, type, key_name) {
  x_names <- names(x)
  y_names <- names(y)
  if (anyDuplicated(x_names) > 0L) {
    .url_join_abort(
      "`x` has duplicate column names; join input schemas must be unique.",
      "rurl_url_join_suffix_error"
    )
  }
  if (anyDuplicated(y_names) > 0L) {
    .url_join_abort(
      "`y` has duplicate column names; join input schemas must be unique.",
      "rurl_url_join_suffix_error"
    )
  }

  # semi/anti emit `x` columns only, so no suffixing can arise at all.
  if (type %in% c("semi", "anti")) {
    return(.url_join_finish_plan(x_names, x_names, character(0), character(0),
                                 key_name))
  }

  if (!is.character(suffix) || length(suffix) != 2L || anyNA(suffix)) {
    .url_join_abort(
      "`suffix` must be a length-2 character vector of non-NA strings.",
      "rurl_url_join_suffix_error"
    )
  }

  overlap <- intersect(x_names, y_names)
  x_out <- ifelse(x_names %in% overlap, paste0(x_names, suffix[[1L]]), x_names)
  y_out <- ifelse(y_names %in% overlap, paste0(y_names, suffix[[2L]]), y_names)

  .url_join_finish_plan(x_names, x_out, y_names, y_out, key_name)
}

# D-F step 4 and D-G's collision rule share one check: the assembled output
# schema must be unique, and if the requested names cannot produce one it is an
# early error. Nothing is renamed beyond the defined suffixing, and nothing is
# dropped.
.url_join_finish_plan <- function(x_in, x_out, y_in, y_out, key_name) {
  if (!is.null(key_name)) {
    if (!is.character(key_name) || length(key_name) != 1L ||
          is.na(key_name) || !nzchar(key_name)) {
      .url_join_abort(
        paste0(
          "`key_name` must be NULL (the key is hidden) or a length-one, ",
          "non-NA, non-empty character column name."
        ),
        "rurl_url_join_key_name_error"
      )
    }
    if (key_name %in% c(x_out, y_out)) {
      .url_join_abort(
        sprintf(
          paste0(
            "`key_name = \"%s\"` collides with an output column of the same ",
            "name. Choose a different name; the comparison key is never ",
            "silently renamed."
          ),
          key_name
        ),
        "rurl_url_join_key_name_error"
      )
    }
  }

  all_out <- c(x_out, y_out, key_name)
  if (anyDuplicated(all_out) > 0L) {
    dup <- unique(all_out[duplicated(all_out)])
    .url_join_abort(
      sprintf(
        paste0(
          "The requested `suffix` cannot produce a unique output schema: ",
          "%s would appear more than once. Columns are never silently ",
          "repaired; choose a different `suffix`."
        ),
        paste0("\"", dup, "\"", collapse = ", ")
      ),
      "rurl_url_join_suffix_error"
    )
  }
  list(x_in = x_in, x_out = x_out, y_in = y_in, y_out = y_out,
       key_name = key_name)
}

# --- matching ----------------------------------------------------------------

# Pair every row of side A against the eligible rows of side B, WITHOUT
# materializing the product: the return is a per-A-row match count plus the flat
# B indices in A-row order, each A row's group in B row order. Every join type
# below is an index computation over this one primitive, which is what makes the
# six stable orders one contract rather than six.
.url_join_pairs <- function(key_a, elig_a, key_b, elig_b, multiple) {
  n_a <- length(key_a)
  cnt <- integer(n_a)
  b_idx <- which(elig_b)
  if (length(b_idx) == 0L) {
    return(list(count = cnt, flat = integer(0)))
  }

  b_key <- .url_key_bytes(key_b)[b_idx]
  uk <- unique(b_key)
  groups <- split(b_idx, factor(match(b_key, uk), levels = seq_along(uk)))
  # `b_idx` is increasing, so each group is already in B row order; the lossy
  # narrowings take that order's first or last element.
  if (multiple == "first") {
    groups <- lapply(groups, function(v) v[[1L]])
  } else if (multiple == "last") {
    groups <- lapply(groups, function(v) v[[length(v)]])
  }

  a_grp <- rep(NA_integer_, n_a)
  hit <- elig_a & !is.na(.url_key_bytes(key_a))
  a_grp[hit] <- match(.url_key_bytes(key_a)[hit], uk)
  matched <- !is.na(a_grp)
  cnt[matched] <- lengths(groups)[a_grp[matched]]

  list(
    count = cnt,
    flat = unlist(groups[a_grp[cnt > 0L]], use.names = FALSE)
  )
}

# The framed bytes, stripped of class and attributes. Comparison always happens
# on these: `==`, `match`, `duplicated` and `%in%` then cannot disagree with
# each other, because there is only one representation.
.url_key_bytes <- function(k) {
  v <- unclass(k)
  attributes(v) <- NULL
  v
}

# Expand a per-A-row count into (A index, B index) pairs. `keep_unmatched`
# emits one NA-B row for an A row with no match, which is the missing-payload
# rule shared by left, right (mirrored) and full.
.url_join_expand <- function(count, flat, keep_unmatched) {
  reps <- if (keep_unmatched) pmax(count, 1L) else count
  a_i <- rep(seq_along(count), reps)
  b_i <- rep(NA_integer_, length(a_i))
  matched <- which(count > 0L)
  if (length(matched) > 0L) {
    ends <- cumsum(reps)
    starts <- ends - reps + 1L
    b_i[sequence(count[matched], from = starts[matched])] <- flat
  }
  list(a = a_i, b = b_i)
}

# --- relationship and resource preflight -------------------------------------

# P3.1 D-D: `relationship` is validated on ELIGIBLE keys BEFORE the result is
# materialized, and the resource guard preflights counts rather than expanding a
# Cartesian product first. Both are the same computation here -- multiplicity
# per matching key -- so both run once, on counts only.
#
# Constraints are checked over keys present on BOTH sides. Checking every
# eligible key instead would reject duplicates that can never match, which is a
# stricter contract than the one that is settled.
#
# No row limit is invented: the contract requires a preflight, not a cap, so the
# projected row count is computed and reported, never used to refuse.
.url_join_check_relationship <- function(relationship, kx, ky, ex, ey) {
  if (relationship %in% c("none", "many-to-many")) {
    return(invisible(NULL))
  }
  x_keys <- .url_key_bytes(kx)[ex]
  y_keys <- .url_key_bytes(ky)[ey]
  shared <- intersect(x_keys, y_keys)
  if (length(shared) == 0L) {
    return(invisible(NULL))
  }
  x_cnt <- tabulate(match(x_keys, shared), nbins = length(shared))
  y_cnt <- tabulate(match(y_keys, shared), nbins = length(shared))

  check_x <- relationship %in% c("one-to-one", "one-to-many")
  check_y <- relationship %in% c("one-to-one", "many-to-one")
  bad <- (check_x & x_cnt > 1L) | (check_y & y_cnt > 1L)
  if (!any(bad)) {
    return(invisible(NULL))
  }

  i <- which(bad)
  .url_join_abort(
    sprintf(
      paste0(
        "`relationship = \"%s\"` is violated by %d matching key(s). ",
        "Projected result rows: %d. Representative key(s): %s. ",
        "Declare relationship = \"many-to-many\" (or \"none\") to allow ",
        "duplicate expansion -- rows are never discarded to resolve it."
      ),
      relationship, length(i), sum(x_cnt * y_cnt),
      paste0(
        "<key ", substr(shared[utils::head(i, 3L)], 1L, 24L), "...> x",
        x_cnt[utils::head(i, 3L)], "/y", y_cnt[utils::head(i, 3L)],
        collapse = ", "
      )
    ),
    "rurl_url_join_relationship_error"
  )
}

# --- assembly ----------------------------------------------------------------

# P3.2 D-H(1): restoration BY CONSTRUCTION. The result is a row-slice of `x`, so
# `x`'s subclass and whatever its own subsetting contract preserves is what the
# result preserves; nothing is blind-copied. `x[NA_integer_, ]` supplies the
# typed missing payload for unmatched `y` rows, so the missing values are `x`'s
# own column types rather than logical `NA`.
#
# D-H(2): this same path builds the zero-row result, so an empty join carries
# the complete typed schema by construction -- there is no separate "empty
# template" that could drift from the populated one. That drift is exactly what
# `canonical_join()`'s parallel `.cj_empty_template()` risks.
.url_join_assemble <- function(x, y, x_i, y_i, plan, key_vals) {
  out <- x[x_i, , drop = FALSE]
  names(out) <- plan$x_out
  rownames(out) <- NULL

  if (length(plan$y_in) > 0L) {
    for (j in seq_along(plan$y_in)) {
      out[[plan$y_out[[j]]]] <- y[[plan$y_in[[j]]]][y_i]
    }
  }
  if (!is.null(plan$key_name)) {
    out[[plan$key_name]] <- key_vals
  }
  out
}

# The exposed comparison key (KJ-O7 / P3.2 D-G), coalesced across sides so an
# unmatched `y` row in a right or full join still reports its own key. Matched
# rows agree by construction, so either side would do. The value is the CLASSED
# key, never a URL-looking string.
.url_join_key_column <- function(kx, ky, x_i, y_i, policy) {
  vx <- .url_key_bytes(kx)
  rx <- attr(kx, "keyability")
  value <- vx[x_i]
  reason <- rx[x_i]
  if (!is.null(ky) && !is.null(y_i)) {
    fill <- is.na(x_i)
    if (any(fill)) {
      value[fill] <- .url_key_bytes(ky)[y_i[fill]]
      reason[fill] <- attr(ky, "keyability")[y_i[fill]]
    }
  }
  .new_url_key(value, policy, reason)
}

# --- the engine --------------------------------------------------------------

# All six operations. One function because they differ ONLY in which index pairs
# they emit -- sharing the key computation, the axes, the preflight, the name
# plan and the assembly is what makes "one symmetric policy applied to both
# sides" true by construction rather than by six-fold review.
#
# The six stable orders (`:138-145`, and KJ-O3 closed by P3.2 D-C as the
# y-primary mirror of left):
#
#   inner  matching pairs, x order, y match order within each x
#   left   every x row, matches expanded, unmatched x with missing y payload
#   right  every y row in y order, x matches in x order, unmatched y with
#          missing x payload  -- the exact reflection of left
#   full   the left result in x order, then unmatched y rows in y order
#   semi   each x row with >= 1 match, once, x columns only
#   anti   each x row with no match, once, x columns only; NON-KEYABLE x IS
#          RETAINED (P3.2 D-D), because a non-keyable row has by construction no
#          eligible match, which is the anti-join predicate itself
.url_join_impl <- function(x, y, by, type,
                           policy = .url_key_policy_spec(),
                           suffix = c(".x", ".y"),
                           key_name = NULL,
                           relationship = "none",
                           multiple = "all",
                           invalid = "keep",
                           warnings = "allow",
                           engine = NULL) {
  type <- match.arg(type, .URL_JOIN_TYPES)
  relationship <- match.arg(relationship, .URL_JOIN_RELATIONSHIPS)
  multiple <- match.arg(multiple, .URL_JOIN_MULTIPLE)
  invalid <- match.arg(invalid, .URL_JOIN_INVALID)
  warnings <- match.arg(warnings, .URL_JOIN_WARNINGS)

  if (!is.data.frame(x) || !is.data.frame(y)) {
    .url_join_abort("`x` and `y` must be data frames.",
                    "rurl_url_join_input_error")
  }
  # One immutable, symmetric policy object, or the family cannot promise that
  # equality is symmetric and transitive (`:66`).
  if (!inherits(policy, "rurl_url_key_policy")) {
    .url_join_abort(
      paste0(
        "`policy` must be a rurl_url_key_policy object. One immutable policy ",
        "is applied symmetrically to both sides; side-specific rules are ",
        "prohibited because equality must stay symmetric and transitive."
      ),
      "rurl_url_join_policy_error"
    )
  }

  cols <- .url_join_resolve_by(by, x, y)
  plan <- .url_join_name_plan(x, y, suffix = suffix, type = type,
                              key_name = key_name)

  sx <- .url_join_side_state(x[[cols$x]], policy, engine)
  sy <- .url_join_side_state(y[[cols$y]], policy, engine)
  ax <- .url_join_apply_axes(sx, invalid, warnings, "x")
  ay <- .url_join_apply_axes(sy, invalid, warnings, "y")

  # `invalid = "drop"` removes rows BEFORE any matching, so every index below is
  # an index into the surviving frame.
  x <- x[ax$keep, , drop = FALSE]
  y <- y[ay$keep, , drop = FALSE]
  kx <- sx$key[ax$keep]
  ky <- sy$key[ay$keep]
  ex <- ax$eligible[ax$keep]
  ey <- ay$eligible[ay$keep]

  .url_join_check_relationship(relationship, kx, ky, ex, ey)

  idx <- .url_join_indices(type, kx, ky, ex, ey, multiple)

  key_vals <- if (is.null(plan$key_name)) {
    NULL
  } else {
    .url_join_key_column(kx, ky, idx$x, idx$y, policy)
  }
  .url_join_assemble(x, y, idx$x, idx$y, plan, key_vals)
}

# The one place the six row orders are defined. Returns aligned `x`/`y` row
# indices, either of which may be NA to mean "missing payload from that side".
.url_join_indices <- function(type, kx, ky, ex, ey, multiple) {
  if (type %in% c("semi", "anti")) {
    p <- .url_join_pairs(kx, ex, ky, ey, "all")
    rows <- if (type == "semi") which(p$count > 0L) else which(p$count == 0L)
    return(list(x = rows, y = NULL))
  }

  if (type == "right") {
    # The y-primary mirror (P3.2 D-C): every y row in y order, x matches in x
    # order. Sides are swapped into the same primitive, so left and right are
    # one symmetry rather than two independent order rules.
    p <- .url_join_pairs(ky, ey, kx, ex, multiple)
    e <- .url_join_expand(p$count, p$flat, keep_unmatched = TRUE)
    return(list(x = e$b, y = e$a))
  }

  p <- .url_join_pairs(kx, ex, ky, ey, multiple)
  e <- .url_join_expand(p$count, p$flat, keep_unmatched = type != "inner")
  if (type != "full") {
    return(list(x = e$a, y = e$b))
  }

  # Full join: the left result, then the y rows it never consumed, in y order.
  # Defining "unmatched y" as "absent from the left result" rather than
  # recomputing a reverse match keeps the two halves consistent under the lossy
  # `multiple` narrowings, where a reverse match would disagree.
  used <- unique(e$b[!is.na(e$b)])
  rest <- setdiff(seq_along(ky), used)
  list(
    x = c(e$a, rep(NA_integer_, length(rest))),
    y = c(e$b, rest)
  )
}

# --- the exported surface ----------------------------------------------------
#
# Six thin wrappers over one engine. They differ only in `type`, which is the
# whole point: sharing the key computation, the axes, the preflight, the name
# plan and the assembly is what makes "one symmetric policy applied to both
# sides" true by construction rather than by six-fold review.
#
# `engine` IS public here, unlike on `get_url_key()`. The asymmetry is
# deliberate and measured: the key frames no PSL-derived component, so an engine
# cannot move a key byte, but `warnings = "reject"` reads the L3 PSL annotation,
# so a divergent suffix list CAN move which rows are eligible to match. Identity
# is engine-independent; eligibility is not, and the two signatures say so.

#' Identity-keyed URL joins
#'
#' Six joins that match rows on URL *identity* rather than on string equality.
#' Each side names one URL column; both sides are keyed with one immutable
#' [url_key_policy()], and rows pair up when their comparison keys are equal.
#'
#' @section Why not a plain join:
#'
#' Joining data frames on raw URL strings misses `http://example.com:80/a`
#' against `http://example.com/a`. Joining them on a *cleaned* string
#' overmatches instead, because cleaning is a display policy: it can strip a
#' trailing slash, a query parameter or a `www.` that genuinely distinguished
#' two resources. These joins use [get_url_key()], so what matches is what rurl
#' considers the same resource -- and no cleaning or display option can change
#' that.
#'
#' @section Row order:
#'
#' Order is part of the contract, not an artifact of the implementation:
#'
#' \describe{
#'   \item{`url_inner_join`}{matching pairs in `x` order, `y` matches in `y`
#'     order within each `x` row.}
#'   \item{`url_left_join`}{every `x` row in `x` order; unmatched `x` rows carry
#'     a missing `y` payload typed from `y`'s own columns.}
#'   \item{`url_right_join`}{the exact mirror: every `y` row in `y` order, `x`
#'     matches in `x` order.}
#'   \item{`url_full_join`}{the left-join result, then the `y` rows it never
#'     consumed, in `y` order.}
#'   \item{`url_semi_join`}{each `x` row with at least one match, once, `x`
#'     columns only.}
#'   \item{`url_anti_join`}{each `x` row with no match, once, `x` columns only.}
#' }
#'
#' Duplicate keys expand as a Cartesian product. Rows are never silently
#' discarded to "resolve" a duplicate -- multiplicity is a fact you declare with
#' `relationship` or narrow with `multiple`.
#'
#' @section Rows that cannot be keyed:
#'
#' A URL the standard cannot parse, or a missing or empty one, has no identity
#' and never matches -- not even another unparseable URL. `invalid` decides what
#' happens to those rows: `"keep"` (default) leaves them in, unmatched, so a
#' left join still returns them; `"drop"` removes them before matching;
#' `"error"` refuses the join and reports the offending row positions.
#'
#' `warnings` is a separate axis for rows that *did* parse but carry a note --
#' userinfo on a scheme-less input, or a host whose public-suffix annotation did
#' not resolve. `"allow"` (default) matches them normally, `"reject"` makes them
#' ineligible to match without removing them, and `"error"` refuses the join.
#'
#' `url_anti_join()` keeps non-keyable `x` rows, because a row that cannot match
#' anything is exactly what an anti join asks for.
#'
#' @section Conditions:
#'
#' Failures raise typed conditions -- `rurl_url_join_input_error`,
#' `rurl_url_join_policy_error`, `rurl_url_join_suffix_error`,
#' `rurl_url_join_key_name_error`, `rurl_url_join_relationship_error`,
#' `rurl_url_join_invalid_error` and `rurl_url_join_warning_error`, all
#' inheriting from `rurl_url_join_error` -- so they can be caught precisely.
#' Messages report row *positions* and truncated keys, never URL content, so a
#' credential in the input cannot leak into an error message.
#'
#' @param x,y Data frames to join.
#' @param by The URL columns to key on: either one column name present on both
#'   sides (`"URL"`), or the named form `c(x_col = "y_col")` when they differ.
#' @param policy A `rurl_url_key_policy` from [url_key_policy()], applied
#'   symmetrically to both sides. Side-specific rules are prohibited: equality
#'   has to stay symmetric and transitive.
#' @param suffix Length-2 character vector disambiguating column names present
#'   on both sides. Default `c(".x", ".y")`. If the result would still contain a
#'   duplicate name, the join errors rather than repairing it silently.
#' @param key_name Optional column name under which to expose the comparison
#'   key. `NULL` (default) hides it. The exposed value is the classed key from
#'   [get_url_key()], never a URL-looking string, and a name that collides with
#'   an output column is an error.
#' @param relationship Cardinality you assert about matching keys, checked
#'   *before* the result is materialized: `"none"` (default, no check),
#'   `"one-to-one"`, `"one-to-many"`, `"many-to-one"`, or `"many-to-many"`
#'   (no constraint, declared explicitly).
#' @param multiple How many `y` rows a matching `x` row may take: `"all"`
#'   (default, lossless) or the separately named lossy narrowings `"first"` /
#'   `"last"`, which take the first or last match in `y` row order.
#' @param invalid What to do with rows that cannot be keyed: `"keep"`
#'   (default), `"drop"` or `"error"`.
#' @param warnings What to do with rows that parsed with a warning: `"allow"`
#'   (default), `"reject"` (ineligible to match, but retained) or `"error"`.
#' @param engine Optional `psl_engine` object from `pslr::psl_engine()` for
#'   per-request Public Suffix List resolution. `NULL` (default) uses the
#'   session-global engine. It cannot affect the comparison key -- identity
#'   frames no public-suffix component -- but it can affect which rows count as
#'   warning rows under `warnings = "reject"`.
#'
#' @return A data frame built by row-slicing `x`, so `x`'s column types and
#'   subclass survive. `url_inner_join()`, `url_left_join()`,
#'   `url_right_join()` and `url_full_join()` return `x`'s columns followed by
#'   `y`'s, disambiguated by `suffix`; `url_semi_join()` and `url_anti_join()`
#'   return `x`'s columns only. A zero-row result is built by the same path, so
#'   it carries the complete typed schema.
#'
#' @seealso [get_url_key()] and [url_key_policy()] for the identity model, and
#'   [canonical_join()] for the legacy join that matches on cleaned strings.
#'
#' @examples
#' pages <- data.frame(
#'   URL = c("http://example.com:80/a", "https://example.com/b",
#'           "http://example.com/c?", "not a url"),
#'   clicks = c(10, 20, 30, 40),
#'   stringsAsFactors = FALSE
#' )
#' meta <- data.frame(
#'   URL = c("http://example.com/a", "http://example.com/b",
#'           "http://example.com/c"),
#'   title = c("A", "B", "C"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Only row 1 matches: `:80` is redundant under http, but http is not https,
#' # and a present-but-empty query is not the same resource as no query.
#' url_inner_join(pages, meta, by = "URL")
#'
#' # Every left row survives, unmatched ones with a typed missing payload.
#' url_left_join(pages, meta, by = "URL")
#'
#' # Rows that could not be parsed at all.
#' url_anti_join(pages, meta, by = "URL")
#'
#' # Expose the key you matched on.
#' url_inner_join(pages, meta, by = "URL", key_name = "key")
#'
#' # Relaxing scheme equality brings row 2 in.
#' url_inner_join(pages, meta, by = "URL",
#'                policy = url_key_policy(scheme_equality = "http_https"))
#'
#' @name url_join
NULL

#' @rdname url_join
#' @export
url_inner_join <- function(x, y, by, policy = url_key_policy(),
                           suffix = c(".x", ".y"), key_name = NULL,
                           relationship = "none", multiple = "all",
                           invalid = "keep", warnings = "allow",
                           engine = NULL) {
  .url_join_impl(x, y, by, "inner", policy = policy, suffix = suffix,
                 key_name = key_name, relationship = relationship,
                 multiple = multiple, invalid = invalid, warnings = warnings,
                 engine = engine)
}

#' @rdname url_join
#' @export
url_left_join <- function(x, y, by, policy = url_key_policy(),
                          suffix = c(".x", ".y"), key_name = NULL,
                          relationship = "none", multiple = "all",
                          invalid = "keep", warnings = "allow",
                          engine = NULL) {
  .url_join_impl(x, y, by, "left", policy = policy, suffix = suffix,
                 key_name = key_name, relationship = relationship,
                 multiple = multiple, invalid = invalid, warnings = warnings,
                 engine = engine)
}

#' @rdname url_join
#' @export
url_right_join <- function(x, y, by, policy = url_key_policy(),
                           suffix = c(".x", ".y"), key_name = NULL,
                           relationship = "none", multiple = "all",
                           invalid = "keep", warnings = "allow",
                           engine = NULL) {
  .url_join_impl(x, y, by, "right", policy = policy, suffix = suffix,
                 key_name = key_name, relationship = relationship,
                 multiple = multiple, invalid = invalid, warnings = warnings,
                 engine = engine)
}

#' @rdname url_join
#' @export
url_full_join <- function(x, y, by, policy = url_key_policy(),
                          suffix = c(".x", ".y"), key_name = NULL,
                          relationship = "none", multiple = "all",
                          invalid = "keep", warnings = "allow",
                          engine = NULL) {
  .url_join_impl(x, y, by, "full", policy = policy, suffix = suffix,
                 key_name = key_name, relationship = relationship,
                 multiple = multiple, invalid = invalid, warnings = warnings,
                 engine = engine)
}

#' @rdname url_join
#' @export
url_semi_join <- function(x, y, by, policy = url_key_policy(),
                          key_name = NULL, relationship = "none",
                          invalid = "keep", warnings = "allow",
                          engine = NULL) {
  .url_join_impl(x, y, by, "semi", policy = policy, key_name = key_name,
                 relationship = relationship, invalid = invalid,
                 warnings = warnings, engine = engine)
}

#' @rdname url_join
#' @export
url_anti_join <- function(x, y, by, policy = url_key_policy(),
                          key_name = NULL, relationship = "none",
                          invalid = "keep", warnings = "allow",
                          engine = NULL) {
  .url_join_impl(x, y, by, "anti", policy = policy, key_name = key_name,
                 relationship = relationship, invalid = invalid,
                 warnings = warnings, engine = engine)
}
