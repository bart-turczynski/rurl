# Zenodo DOIs for the package family

Promoted 2026-08-26 from Claude's auto-memory store. Set up 2026-06-27.

`rurl`, `pslr` and `punycoder` all carry Zenodo archive DOIs, minted through the GitHub↔Zenodo
integration: the per-repo toggle at `zenodo.org/account/settings/github`, after which publishing
a GitHub release auto-archives the repo and mints a version DOI.

## Concept DOIs — cite these

A concept DOI always resolves to the latest version, which is why it is the one that goes into
citation metadata.

| Package | Concept DOI | Version DOI at time of minting |
|---|---|---|
| rurl | `10.5281/zenodo.20972584` | 1.4.0 = `…585` |
| punycoder | `10.5281/zenodo.20973629` | 1.2.0 = `…630` |
| pslr | `10.5281/zenodo.20973660` | 1.0.2 = `…661` |

## The per-repo setup pattern

Proven on `rurl`, then applied to the other two:

1. Write a full-field `.zenodo.json`, and add `^\.zenodo\.json$` to `.Rbuildignore` — it is
   GitHub-only and must not ship to CRAN, exactly like `codemeta.json` and `CITATION.cff`.
2. Cut a GitHub release at the `DESCRIPTION` version to trigger the mint. All three repos'
   GitHub releases were behind CRAN at the time, and the release caught them up.
3. Wire the **concept** DOI into: the README badge and Citation section; `CITATION.cff` (`doi`
   plus an `identifiers` block, and the CRAN `repository` field); the `inst/CITATION` bibentry;
   and the `codemeta.json` identifier.

`related_identifiers` form a graph: `rurl` *requires* `pslr` and `punycoder`; each of those
*isRequiredBy* `rurl`.

Verify a live record through the public API — the fields to check are `doi` and `conceptdoi`:

```sh
curl -s "https://zenodo.org/api/records/<id>"
```

## The gated JOSS follow-up

JOSS mints its own **paper** DOI, separate from the Zenodo archive DOI, on acceptance — and it
requires a Zenodo archive DOI as an acceptance step, which is already satisfied. Bart has
submitted to rOpenSci and intends to submit to JOSS.

**When a JOSS submission is accepted, add the JOSS paper DOI to that package's `CITATION.cff`.
Do not pre-fill a placeholder.** The three fp DOI issues (RURL-biywcsrk, PSLR-auebjgpr,
PUNY-fqhoaoog, all closed) each carry this note.
