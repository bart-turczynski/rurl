# ADR 0015: `url_standard` is required on the three companion helpers

- **Status:** Accepted
- **Date:** 2026-08-23
- **Tracking:** RURL-kbpyivuk (this decision), RURL-bmxptxxz (the boundary of
  ADR 0007's `NULL` freeze, which this states). Rejects RURL-hikovisr's
  sentinel. Narrows **ADR 0007** §Consequences; relates to **ADR 0006**.

## Context

`get_host_type()`, `get_url_diagnostics()` and `get_scheme_class()` shipped in
rurl 2.2.0 with `url_standard = NULL` as their default. Measured on `848bd11`,
under that default **none of them can return information for any input**:

    get_host_type(u)         #> NA NA NA NA NA NA NA NA   (always)
    get_scheme_class(u)      #> NA NA NA NA NA NA NA NA   (always)
    get_url_diagnostics(u)   #> character(0) per row      (always)

There is no input for which the `NULL` arm answers anything. It is not a mode
that answers a different question — it is a mode that cannot answer. And it
exists for a traceable reason that is not design: ADR 0007 made the selector
purely additive, so the helpers the selector introduced shipped with a `NULL`
default rather than a chosen one.

Two of the three additionally overload a real return value:

- `get_host_type()` returns `NA` both for a selector-less call and for a row
  that is genuinely unclassifiable under the selector. The two are
  indistinguishable in the returned vector, so an all-`NA` result was never
  evidence about the input.
- `get_url_diagnostics()` returns `character(0)` both for a selector-less call
  and for a URL that raised no diagnostics, so an empty result was never
  evidence that a URL is clean.

`get_scheme_class()` does **not** overload: with a selector it never returns
`NA`, because unparseable, scheme-less, empty and `NA` input all classify as
`"missing-or-error"`. Its `NA` therefore meant exactly "you passed no
selector". This asymmetry is real and was measured, but it is not a reason to
spare it — see §Decision.

## Decision

Drop the default. On all three helpers `url_standard` is a required argument
with no default, and an explicitly-passed `NULL` is the same error as omitting
it — the mode is gone, not merely undefaulted:

    get_host_type <- function(url, url_standard, ...)   # was url_standard = NULL

`.require_url_standard()` (`R/parse.R`) raises the error and names both
profiles in the message, so the fix is in the error text. It is a distinct
helper from `.validate_url_standard()`, which still accepts `NULL` — the parse
functions keep the `NULL` profile and are untouched by this ADR.

**Boundary.** This covers exactly those three helpers. It does **not** touch:

- `safe_parse_url()`, `safe_parse_urls()`, `get_clean_url()`, `resolve_url()`
  or any other function that accepts `url_standard`. Their `NULL` profile is a
  real parse mode with defined output, and ADR 0007's freeze on it stands.
- `get_parse_verdicts()`, deliberately ungated: its layers describe the parse
  that actually ran, which is defined with or without a selector. That
  contract is unchanged and is the model for what a *useful* default on this
  axis looks like.

**Why all three, including `get_scheme_class()`.** Sparing it would make the
argument requirement depend on incidental return encoding rather than on
semantics: it would keep its useless default only because it happened to encode
"not requested" more cleanly than the other two. Unambiguous failure is still
failure — a caller who forgot the argument still receives no error and a column
that answers nothing, and has to already know the rule to decode it. The three
are one family of profile-derived metadata and take one rule.

**Why not a default of `"whatwg"`.** The obvious alternative was to change the
default rather than remove it, matching `is_valid_host()` and `check_hosts()`,
which both default to `"whatwg"`. Rejected: those are **policy** APIs with an
explicitly web-oriented posture, so a web-standard default is a defensible
policy choice there. These three are **descriptive metadata** APIs, and there
is no profile-neutral host type, diagnostic set or special-scheme fact for a
default to stand for. A silently-chosen profile would let a plausible WHATWG
classification flow through consumer code as authoritative while concealing
that no standard was ever named — worse as a contract than the silent `NA` it
replaced, because it fabricates a profile-dependent answer instead of
withholding one.

**Why not a sentinel token** (RURL-hikovisr, rejected and not to be
relitigated without new information). A non-`NA` sentinel silently breaks
*correct* consumer code: `x[!is.na(x)]`, `all(is.na(x))` and
`table(x, useNA = "ifany")` all change meaning. For `get_url_diagnostics()` it
is worse — every member of that vocabulary is an *observed problem*, so a
`"not-requested"` token would make consumers report a finding that does not
exist. Requiring the argument adds no member to any value domain: it makes the
ambiguous state unreachable rather than nameable.

## Consequences

- **Breaking, and deliberately so.** Every selector-less call errors. It rides
  rurl 3.0.0, which was already a breaking release.
- `NA` from `get_host_type()` now means one thing: unclassifiable under the
  standard you named. `character(0)` from `get_url_diagnostics()` now means one
  thing: no diagnostics under the standard you named. `get_scheme_class()` can
  no longer return `NA` at all.
- **ADR 0007's `NULL` freeze is narrowed, not broken.** The freeze promised
  that adding the selector would not perturb *pre-existing* output. These three
  helpers were introduced by the same release as the selector (rurl 2.2.0), so
  no output that predates the selector is affected by removing their `NULL`
  arm. The freeze continues to govern every parse function, and the
  Appendix-B/`NULL` carve-outs that depend on it are untouched. This is the
  boundary RURL-bmxptxxz asked for: the freeze covers the surface that existed
  before the selector, not the companions the selector added.
- Measured cost at the time of the change: **25** selector-less call sites,
  **all in `tests/testthat/`** and none in `R/`, plus zero downstream callers —
  `raddr`'s nine `rurl` mentions are prose and comments, not calls. The
  vignettes and README already passed a selector everywhere.
- One test claim moved rather than died: "the default parse fires no Layer-5
  facts" (ADR 0012 D4) is now pinned through `._url_metadata_vec()` in
  `test-url-diagnostics.R`, because the claim is about the `url_standard = NULL`
  *parse path*, which still exists, not about the helper arm that used to
  expose it.
- **Standing rule: do not reintroduce a default on these three.** A future
  argument for one has to first show a profile-neutral answer they could
  return. If such an answer is ever found for a given helper, that is the new
  information that would justify revisiting — for that helper alone.
