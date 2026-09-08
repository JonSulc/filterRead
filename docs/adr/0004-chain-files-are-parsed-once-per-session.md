---
status: accepted
---

# Chain files are parsed once per session

`.filterread_env$chain_cache` holds each parsed chain for the life of the R
session, keyed on the build pair and the chain directory it was read from.
Before it, every lift re-read the chain RDS from disk: 31 ms of a 92 ms
three-row lift, and a cost `foverlaps()` compounded by re-deriving its own
ordering over the 53,950-row b37-to-b38 chain on each call.

Two accessors sit over the cache. `get_shared_chain_dt()` returns the shared
object under a read-only contract and is what the lift paths call, so the hot
path pays no copy. `get_chain_dt()` returns `data.table::copy()` of it and
keeps the contract it always had, for callers that reach for a chain by name
and may edit it. All four cached build pairs resident is about 17 MB.

The overlap join does not consult a chain's key. Without a secondary index on
`(chr, start, end)` it re-sorts all 53,950 blocks on every call, about 6.2 ms
against 3.7 ms with one. `prepare_chain_dt()` puts that index on a chain
wherever one is written to RDS, where it survives the round trip, and again
where one enters the cache. The cache pass also runs `setDT()`, which a
self-reference cannot be stored through, and readies the chain files already
on disk, which carry no index.

Invalidation is explicit. `clear_chain_cache()` drops the in-memory entries
alongside the RDS files, `set_chain_path()` drops them because the path
selects which chain a build pair resolves to, and `setup_chain_files()` drops
them after writing chain RDS files, so a re-download is picked up.

A chain file replaced on disk out of band is therefore not noticed until one
of those runs. Checking `file.mtime()` on every hit was rejected: it costs
about 19 microseconds per lift to defend against an edit that, for a UCSC
chain, does not happen inside a session.

## Consequences

A test that injects a synthetic chain mocks `get_shared_chain_dt()`, the accessor
the lift paths call, so it is never served a real chain the cache happens to
be holding. Tests that exercise chain loading itself call `get_chain_dt()`
directly, and the three covering RDS behavior run under a temporary chain
directory, which invalidates the cache on the way in and on the way out.
