# rurl 3.0 protocol — §6 artifact 2: record schemas + lifecycle state machine

Produced by **G0.3** (`RURL-bufpkphu`) with children **G0.3a** (`RURL-iwahdugj`,
envelope + record validator) and **G0.3b** (`RURL-vytzhgzh`, transition +
propagation validator). This is the machine-readable authority-record contract
for the whole effort: every later record — owner decisions, accepted designs,
and the source-claim / decision / contradiction / finding registers — is
schema-validated here before it can drive G1–G5 leaf slicing.

## Files

| File | Role |
|---|---|
| `lifecycle.yaml` | The ten states, their entry authority / required evidence, and the legal transition table (reconciliation §6). Plus the propagation rules. |
| `envelope.yaml` | The universal §6 artifact envelope (fields every artifact 1–11 must expose) and the five rejection classes. Inventories artifacts 1–11 as `present` or `planned` with their gate. |
| `record-schemas.yaml` | Per-type schemas: `owner-decision`, `accepted-design`, and the four `register` variants, with id patterns, required fields, reference fields, and cross-type invariants. |
| `fixtures/*.yaml` | Five transition/propagation fixtures (source delta, owner supersession, invalidated review, replacement record, downstream-gate failure). |
| `../tools/validate-records.R` | **G0.3a** validator. |
| `../tools/validate-transitions.R` | **G0.3b** validator. |

## The ten-state lifecycle

```
DISCOVERED ─▶ PROPOSED ─▶ ACCEPTED ─▶ TRANSFERRED ─▶ VERIFIED
     │           │  ▲         │  │           │            │
     ├─▶ OPEN ───┘  │         │  ├─▶ SUPERSEDED (terminal) │
     └─▶ CONFLICT ──┘         │  └─▶ INVALIDATED (terminal) ◀┘
                 │            │
                 └─▶ REJECTED (terminal; a changed premise opens a NEW
                     DISCOVERED record linked via supersedes — never reopened
                     in place)
```

Every transition records **actor, timestamp, evidence, and affected dependency
IDs**. A source or owner-decision change marks dependents `INVALIDATED` and opens
replacement records at `DISCOVERED`; history is never silently rewound. Tracker
(`fp`) state is **non-authoritative** (P0.1 §5): a gate whose durable acceptance
record is stale is not "done" no matter what the tracker says.

## Import of the P0.1 bootstrap (no substance edit)

P0.1 §Consequences requires G0.3 to import and validate the bootstrap decision
records **without altering their substance**. `record-schemas.yaml:owner-decision`
is defined as the *minimum* required field set that P0.1–P0.3 already satisfy;
records may carry extra fields (e.g. P0.1's `owner_forge_identity`). The
validator confirms this: P0.1, P0.2, and P0.3 pass unchanged. Any schema that
could not represent these fields would be incomplete — not a licence to rewrite
the decision.

## Completion rule (G0.3)

G0.3 is done when **both child validators pass** and the schemas can drive later
leaf slicing. All three control-plane validators run clean from the repo root:

```sh
Rscript design/work/url-v3/tools/validate-manifest.R design/work/url-v3/manifest.yaml
Rscript design/work/url-v3/tools/validate-records.R
Rscript design/work/url-v3/tools/validate-transitions.R
```

Each is negative-tested: `validate-records.R` rejects duplicate IDs, broken
references, unknown states, missing required fields, and manifest hash drift;
`validate-transitions.R` rejects illegal transitions, missing transition
evidence, wrong terminal states, and any downstream gate left accepted while its
acceptance record is stale.

## Integrity coverage / next snapshot

These schema and tool files are tracked (durable in git history) but are **not**
pinned in the sealed `v3/cp-snapshot-1` manifest — that manifest's identity is
frozen at C1 and is not mutated post-acceptance (P0.2 §3.3). The G0.3 artifacts
are swept into the next control-plane snapshot (`v3/cp-snapshot-2`), taken at the
next durability checkpoint (G0 close / G1). Until then their integrity rests on
the passing validators plus git history, and `validate-records.R` continues to
enforce hash coverage for everything the snapshot-1 manifest lists.
