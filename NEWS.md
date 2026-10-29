## rurl (development version)

- The default `url_standard` is now `"whatwg"`, and `NULL` means `"whatwg"`, in every function that takes it; pass `url_standard = "rfc3986"` for the RFC 3986 reading (`RURL-mbhuhwzk`).
- Under the new default, a host ending in a number goes through the IPv4 parser (`http://127.1/` is `127.0.0.1`), and forbidden host code points fail (WHATWG URL, host parsing).
- Under the new default, hosts are UTS-46 mapped, `%2e` dot segments resolve, triplets keep their hex case, and a backslash separates path segments under a special scheme.
- Under the new default, WHATWG percent-encode sets apply to query, fragment and userinfo, a default port is `NA`, and C0 controls, spaces, tabs and newlines are stripped.
- Under the new default, `file:` URLs follow the WHATWG `file:` states and `resolve_url()` the WHATWG reference rules; `resolve_url(output = "serialized")` no longer needs `url_standard`.
- Under `"whatwg"`, explicit `case_handling` or `path_normalization = "both"` shapes only `clean_url`; `case_handling = "keep"`, `path_normalization = "none"` and `"collapse_slashes"` are errors (ADR 0020).
- `get_host()` and `get_path()` error on a `case_handling` value that would change the returned component, under both standards.
- `profile = "rfc-syntax"` paths keep a percent-triplet's hex case (`/%7e` stays `/%7e`), so `get_clean_url()` no longer equates `%2f` and `%2F` there.
- An explicit `url_standard = NULL` beside a `profile` overrides the bundle's standard as `"whatwg"` does.
- Under `"rfc3986"`, `path_encoding = "encode"` only adds escapes: `%2F`, empty segments and `pchar` sub-delimiters survive (RFC 3986 §3.3) (`RURL-dbyavcjt`).
- `path_encoding = "decode"` decodes only unreserved and non-ASCII UTF-8 triplets; reserved ones such as `%2F` and `%3F` stay encoded (RFC 3986 §2.2) (`RURL-eokizmdi`).
- rurl now requires pslr 1.2.2 or later, which fixes every parse failing under a Turkish or Azerbaijani locale (`RURL-tnsrcqwi`).
- `get_mailto_recipients()` gains a `mailto_local_part_raw` column before `mailto_local_part_form`, moving every later column one place right (`RURL-xwsyfkwh`).
- `get_mailto_recipients()` reports a quoted local part holding a backslash-escaped null octet as `"quoted-string"`, not `"invalid"` (RFC 5322 §4.1) (`RURL-mxsyzrho`).
- Under `"rfc3986"`, directly written non-ASCII is a parse error in every component; percent-encode it as UTF-8 or use `"whatwg"` (RFC 3986 §2.5) (`RURL-pntbujzc`).
- `get_url_diagnostics(url_standard = "rfc3986")` reports `unicode-outside-rfc3986-uri` for such a failed row, under either `scheme_acceptance` value.
- Under `"rfc3986"`, `resolve_url()` gives `NA` for a reference that is no URI-reference, such as `10.0.0.7:8080/foo.html` (RFC 3986 §4.1, §4.2) (`RURL-pntbujzc`).
- `get_mailto_recipients()` gains `parse_status` and `url_diagnostics` columns; under `"whatwg"` it splits recipients after the parser's control and whitespace stripping (`RURL-pntbujzc`).
- `resolve_url()` gives `NA` against a base the selected standard's parser refuses, an empty base under `"whatwg"` included; pass `NA` for a missing base (`RURL-vrvxxfvm`).
- `get_mailto_recipients()` no longer errors on a recipient whose decoded octets are not valid UTF-8; that field is `"invalid"` (RFC 6068 §2) (`RURL-lotjvpnt`).
- Under `"rfc3986"`, a row holding an octet that is not valid UTF-8 now fails alone instead of stopping the whole vector, in every `query_handling` (`RURL-otrpfhmq`).
- Under `"rfc3986"`, `path_encoding = "encode"` and `"decode"` keep a `%XX` that decodes to invalid UTF-8 as written (`/caf%E9`) (`RURL-yarkccco`).
- `resolve_url()` reads latin1 and invalid-UTF-8 references and bases as the parser does, with one answer in every locale (`RURL-ummmdiwu`).
- An element marked `"bytes"` is now a row that does not parse in every function, instead of an error that lost the whole call (`RURL-cwzickqy`).
- Very long components and hosts of very many labels now parse row by row in linear time, instead of failing the vector, mis-parsing or taking minutes (`RURL-vewhvxnb`).
- On R before 4.6.0, a URL component longer than 1,000,000 characters is no longer truncated (`RURL-hxeaefzp`).
- `profile = "browser"` repairs `scheme;` only for ASCII case variants of special schemes, so `httpſ;h.com` stays as written (WHATWG URL, ASCII case-insensitive) (`RURL-oahhlnvf`).
- A trailing U+0085, U+2028 or U+2029 no longer ends the URL early: `example.com:8080<U+2028>` parses as scheme `example.com` under `scheme_acceptance = "general"` (`RURL-oahhlnvf`).
- `*` in `params_keep`, `params_drop` and the built-in denylist now matches line terminators; `params_case_sensitive = TRUE` compares names octet for octet (`RURL-yyeohqah`).
- Under `"rfc3986"`, a `//` row with an empty host (`///p`) reads as an empty authority and path `/p`, not host `p` (RFC 3986 §3.2) (`RURL-ozpxupgq`).
- `serialize_url(standard = "rfc3986")` returns `NA` for a `//host` network-path reference instead of inventing an `http:` scheme (RFC 3986 §4.2) (`RURL-xjyrhqin`).
- Under `"whatwg"`, a percent-encoded U+3002, U+FF0E or U+FF61 in a host is a full stop, as its raw spelling is (UTS #46 §4) (`RURL-esztpdhx`).
- Under `"whatwg"`, `www_handling` and `subdomain_levels_to_keep` act on the UTS-46 mapped host, so a fullwidth or mark-led `www` counts as `www` (`RURL-fobwctml`).
- Under its `whatwg` policy, `get_url_key()` keys path and query as the standard serializes them, so `http://h/p{` and `http://h/p%7B` share a key (`RURL-ommuyaqi`).
- `path_normalization`, index-page, trailing-slash and case handling no longer alter the `path` of a general-routed row, which now matches its `clean_url` (`RURL-kbjzwrkt`).
- A leading U+FEFF in a host or path is now data in `case_handling`, `subdomain_levels_to_keep`, `www_handling` and `get_subdomain()`, as U+200B already was (`RURL-opwjlnla`).
- `get_url_key()`, `resolve_url()` under `"whatwg"` and `host_encoding = "unicode"` under `"rfc3986"` keep a leading U+FEFF instead of dropping it as a byte-order mark (`RURL-dquaxcaz`).
- Under `url_standard = "rfc3986"` with `scheme_acceptance = "general"`, `path_normalization` now governs `clean_url` too: `foo:/a/../b` cleans to `foo:/b` (RFC 3986 §6.2.2.3) (`RURL-rwlrcvnh`).
- `serialize_url(standard = "rfc3986", form = "normalized")` removes dot segments from rootless paths too (`foo:a/../b` gives `foo:/b`), but never leaves an authority-less path starting `//` (RFC 3986 §5.2.4, §3.3) (`RURL-yvaapvkg`).
- Under `url_standard = "rfc3986"`, `path_encoding`, `trailing_slash_handling` and `index_page_handling` keep a U+FEFF that starts the path instead of adding a leading `/` (RFC 3986 §3.3) (`RURL-zxnxczsw`).
- A latin1-marked input now parses as its UTF-8 twin under every `url_standard`, where it gave lone octets, parse errors or call-order-dependent results (RFC 3986 §2.5) (`RURL-buemdwcq`).
- Under `url_standard = "whatwg"` with `scheme_acceptance = "general"`, a non-special hierarchical URL's `clean_url` drops the `.` and `..` segments its `path` drops: `foo:/a/../b` cleans to `foo:/b` (`RURL-lzsjlxgu`).
- Under `url_standard = "whatwg"`, `resolve_url()` reads a reference with a scheme other than the base's special scheme alone: `foo:a/../b` stays `foo:a/../b`, `file:/C:/../x` gives `file:///C:/x` (`RURL-rdkewxlx`).
- Under `url_standard = "whatwg"` with `scheme_acceptance = "general"`, a non-special URL with text after a bracketed host's `]`, such as `foo://[::1]x/`, now fails to parse (WHATWG host parsing) (`RURL-rrqzdahp`).
- Under `url_standard = "whatwg"`, `get_url_diagnostics()` reports `leading-trailing-stripped` only when a leading or trailing C0 control or space was removed, not for non-ASCII URLs in a C locale (`RURL-bnorcgbv`).
- Under `profile = "browser"` with `url_standard = "rfc3986"`, a fixed-up URL (trim, `;` to `:`, `//` insertion) parses as its fixed spelling typed directly, `file:`, `ws:` and `wss:` included (`RURL-vmsmlflr`).
- Under that combination, `//` is completed on every `http:`, `https:`, `ftp:`, `ws:` and `wss:` URL with zero or one slash: `http:/a` and `http:a/b` now have host `a` (`RURL-vmsmlflr`).
- Under `url_standard = "rfc3986"`, an existing percent-triplet in `http`, `https`, `ftp` and `ftps` userinfo keeps its hex-digit case; `form = "normalized"` still uppercases it (RFC 3986 §6.2.2.1) (`RURL-bxrbpzet`).
- Under `url_standard = "whatwg"`, existing percent-triplets in the userinfo, query and fragment keep their hex-digit case, and a `%` starting no triplet is not uppercased; `get_url_key()` follows (`RURL-djvqopjk`).
- Under `url_standard = "whatwg"`, a `file:` URL with an invalid bracketed IPv6 host, such as `file://[1:]/`, now fails to parse like its `http:` twin (WHATWG IPv6 parser) (`RURL-ohtwkdgi`).
- `profile = "browser"` no longer inserts `//` after a slashless `file:`, so `file:p` parses as `file:///p`, not host `p` (WHATWG file state) (`RURL-anubcmpd`).
- `profile = "browser"` reads input starting with a single `/` as a local file path (`file:///Users/me/a.pdf`); `scheme_policy = "require"` or `url_standard = "rfc3986"` turns this off (`RURL-anubcmpd`).
- Under `url_standard = "whatwg"`, a `file:` URL with no authority and a colon before any slash now parses: `file:C:/x` is `file:///C:/x`, `file:ab:cd` is `file:///ab:cd` (WHATWG file state) (`RURL-otfaotzq`).
- Under `url_standard = "whatwg"`, a `file:` URL with a backslash-introduced empty host and no following separator gets path `/`: `file:\\` serializes as `file:///`, not `file:////` (`RURL-vhuozous`).
- Under `url_standard = "whatwg"`, a `file:` host holding a `%` not followed by two hex digits, such as `file://a%41%/p`, now fails to parse (WHATWG host parsing) (`RURL-zkirywwb`).
- Under `url_standard = "whatwg"`, a leading U+FEFF is kept in a `file:` path and in userinfo and maps away in a `file:` host; a host of the mark alone fails (WHATWG host parsing) (`RURL-azcvukyh`).
- `resolve_url()` keeps a U+FEFF that starts a path segment instead of dropping it or reading `\ufeff..` as a dot segment, under both standards (RFC 3986 §5.2.4) (`RURL-xrjtdnko`).
- A URL that starts with U+FEFF no longer has a scheme under either `url_standard`, so `\ufeffhttp://a.com/p` fails, and a host starting with it no longer shortens the path (RFC 3986 §3.1) (`RURL-vhionecz`).
- Under `url_standard = "whatwg"`, a host made only of U+FEFF fails its own row instead of erroring the whole call, and caches no longer confuse URLs differing by a leading U+FEFF (`RURL-iiehsbfg`).
- Under `url_standard = "rfc3986"`, an IPv6 literal with an IPv4 tail parses wherever `IPv6address` allows (`[fe80::5efe:1.2.3.4]`), and a leading-zero octet is rejected on every route (RFC 3986 §3.2.2) (`RURL-escneidz`).
- Under `url_standard = "whatwg"`, U+3002, U+FF0E and U+FF61 map to `.` in the host only: an IPv6 literal holding one fails, and userinfo percent-encodes them (WHATWG host parsing) (`RURL-mkbfseqs`).
- Under `url_standard = "whatwg"`, an IPv6 host ending in `::` is serialized canonically (`[1:2:3:4:5:6:7::]` becomes `[1:2:3:4:5:6:7:0]`); `serialize_url()`, `clean_url` and `get_url_key()` follow (WHATWG IPv6 serializer) (`RURL-sqmhtldq`).
- `url_key_policy()` and `get_url_key()` now carry key version 2: no key minted by rurl 3.1.0 matches a new one under either policy, so regenerate persisted keys from their URLs (`RURL-rntskojq`).
- `get_url_diagnostics()` reports `ipv6-non-canonical` and `ipv6-embedded-ipv4`, which also join `check_hosts()`'s `reasons`; rurl now imports raddr (>= 0.1.2) (`RURL-dxwsksor`).
- `get_url_diagnostics()` and `check_hosts()` results for a host with an empty label depend on the installed punycoder: its `-v3` profile also reports hyphen, STD3 and length defects (UTS #46 §4.2) (`RURL-tsmevksk`).
- `rurl` now requires R >= 4.1.0 (was 4.0.0), as its dependency `pslr` already did (`SEOR-nxycpzlk`).
- On R 4.1, `url_standard = "whatwg"` writes an IPv6 host with the shortest hexadecimal pieces (`[::7f00:1]`, not `[::7f00:0001]`) (WHATWG IPv6 serializer) (`SEOR-nxycpzlk`).
- `check_schemes()` lowercases `allowed_schemes` as ASCII, so `"FILE"` admits `file:` URLs under a Turkish or Azeri locale (`SEOR-rxxuzhmc`).
- `resolve_url()` now resolves under the standard its `profile` selects, as it already parsed; an explicit `url_standard` still overrides the profile's (`RURL-nwujwmft`).
- `resolve_url()` with a `profile` no longer errors on a governed knob such as `path_normalization` passed beside an explicit `url_standard`, matching `safe_parse_url()` (`RURL-frwyfdhp`).
- WHATWG percent-encoding in `serialize_url()`, `safe_parse_urls()` and `get_url_key()` runs once per call, about 20 to 40 times faster; long runs of bytes to escape parse faster too (`RURL-khcicsca`).
- Multi-megabyte rows no longer warn "PCRE error 'match limit exceeded'", and finding the last `@` or `:` is no longer quadratic in the number of matches (`RURL-rweowcfz`).
- `get_url_diagnostics()` no longer reports authority facts (`invalid-credentials`, `ipv4-*`, domain-shape tokens) on a URL whose parse has no authority, such as `mailto:a@x.de` (`RURL-cjxdaqym`).
- `get_user()` and `get_userinfo()` on a `mailto:` URL return `NA` when the decoded local part is not valid UTF-8, instead of an invalid string (RFC 6068 §2) (`RURL-gndbrpam`).
- Under `url_standard = "whatwg"`, `resolve_url()` merges against the parsed base: surrounding spaces, tabs and newlines are stripped and `http:x` becomes `http://x/`, as `new URL()` does (`RURL-vgmyrgxb`).
- Under `url_standard = "whatwg"`, `resolve_url()` returns `NA` for a relative reference against a base with an opaque path (`mailto:x`); a fragment-only reference still resolves (WHATWG no scheme state) (`RURL-mvjgpthk`).
- A `%00` in a `mailto:` recipient's local part or domain makes that half `"invalid"` in `get_mailto_recipients()` and `NA` from its accessors, instead of judging the truncated prefix (RFC 5322 §3.2.3) (`RURL-rujjbahq`).
- A `%00` in a path or query no longer truncates the percent-decoded value in `get_clean_url()` or `get_query()`: the triplet stays as written and the rest decodes (RFC 3986 §2.4) (`RURL-fpujlvoq`).

### Deprecated

- `canonical_join()` is deprecated and will be removed in rurl 4.0.0; it warns with class `rurl_canonical_join_deprecated`. Use the `url_*_join()` family instead (`RURL-atrvocqe`).

## rurl 3.1.0

### New features

- British spellings are accepted as aliases: `serialise_url()` for `serialize_url()`, `path_normalisation` for `path_normalization`; supplying both is an error, and `path_normal =` no longer partially matches (SEOR-qwomlgjd).
- `check_schemes()` reports a new reasons token, `no-authority`, for a URL with a scheme but no authority, such as `mailto:someone@example.com`, per RFC 3986 §3 or WHATWG (RURL-ktpjscne).
- `get_url_diagnostics()` and `check_hosts()` report `domain-invalid-ace-label` for an `xn--` label that is not a genuine A-label, per UTS #46 §4.1; parsing is unchanged (RURL-vicyvlvh).

### Bug fixes

- `host_encoding = "unicode"` again decodes an A-label with non-LDH code points, `xn--a_-wia.example` to `a_ä.example`, under punycoder 1.3.0, per RFC 3492 §6.2 (RURL-oizpyvdz).
- Under `url_standard = "whatwg"`, a host whose UTS-46 mapping yields a forbidden domain code point, such as fullwidth `＃`, now fails the parse, per WHATWG host parsing (RURL-crsrkcoh).
- `safe_parse_url()`, `safe_parse_urls()` and their accessors no longer fail on very long URLs with "variable names are limited to 10000 bytes" (RURL-tlmoybsl).

## rurl 3.0.1

No user-facing changes.

## rurl 3.0.0

### Breaking changes

- `profile = "seo"` (and `"canonical"`) now sets `url_standard = "whatwg"` and `host_encoding = "unicode"`, so cleaned URLs resolve `.`/`..` segments and render hosts in Unicode (ADR 0017).
- `trailing_slash_handling = "strip"` keeps the `/` when the host is only dots: `get_clean_url("http://./", profile = "seo")` now returns `"https://./"` (ADR 0017).
- `profile = "seo"` now keeps non-default ports, and `port_handling = "strip_default"` judges the default on the input scheme, not the rendered one, per RFC 3986 §6.2.3 (RURL-msybcxgq).
- Under `url_standard = "whatwg"`, the default and `"unicode"` host renderings apply UTS-46 mapping, so `BÜCHER.example` renders `bücher.example`, per WHATWG host parsing (RURL-nuqhbxvc).
- `profile = "seo"` now drops the whole query (`query_handling = "drop"`, was `"filter"`); pass `query_handling = "filter"` to keep non-tracker parameters (ADR 0017).
- rurl no longer depends on `curl` or the system `libcurl`; parsing is in-tree with unchanged output, so code relying on rurl to load `curl` must declare it.
- `canonical_join()` warns, with class `"rurl_legacy_join_dial_warning"`, when a presentation dial is passed through `...`; results are unchanged.
- `safe_parse_url()` no longer reports `host`, `user`, `domain` or `tld` for a `mailto:` URL under `scheme_acceptance = "general"`; `get_host()` and the other accessors still decompose it.
- Under `url_standard = "whatwg"`, `resolve_url()` applies WHATWG reference parsing before the merge, so `"http:g"` is relative and `\` reads as `/`; `NULL` is unchanged.
- Under a named `url_standard`, `resolve_url()` matches a scheme per RFC 3986 §3.1, so a colon in a relative first segment is no longer a scheme; such a base returns `NA`.
- `get_host_type()`, `get_url_diagnostics()` and `get_scheme_class()` now require `url_standard` (`"whatwg"` or `"rfc3986"`); omitting it or passing `NULL` is an error.

### Bug fixes

- `serialize_url(standard = "rfc3986", form = "source")` reproduces the input's scheme, query and fragment as written, including hex case and raw bytes ≥ 0x80 (RFC 3986 §5.3). (RURL-gkmwqpos)
- Under `url_standard = "rfc3986"`, the `query` and `fragment` columns, `get_query(decode = FALSE)`, `get_fragment()` and the `rfc3986` URL key keep source hex case and raw non-ASCII bytes. (RURL-bpfumnbj)
- `serialize_url(standard = "rfc3986", form = "normalized")` now percent-encodes raw non-ASCII query and fragment bytes on the general route too (`foo://h/p?q=ü` → `foo://h/p?q=%C3%BC`). (RURL-bpfumnbj)
- With `profile = "whatwg"` and `scheme_policy = "require"`, a dotted scheme that looks like `host:port` (`www.php.net:80/index.php`) parses as a scheme instead of failing (WHATWG URL scheme state). (RURL-lxdwuacn)
- `resolve_url()` keeps the `/.` guard when a resolved path starts with `//` and has no authority: `non-spec:/.//path`, not `non-spec://path` (RFC 3986 §3.3). (RURL-bedensww)
- `resolve_url(url_standard = "whatwg")` applies the WHATWG `file:` drive-letter rules against a `file:` base, and `file://C:/` now parses as `file:///C:/`. (RURL-ufsltsit)
- Under `url_standard = "whatwg"`, `..` no longer removes a lone drive letter from a `file:` path: `file:///C:/../` stays `file:///C:/` (WHATWG URL "shorten a URL's path"). (RURL-msefniuz)
- The WHATWG host "ends in a number" check runs after percent-decoding and UTS-46 mapping, so encoded or fullwidth IPv4 hosts parse as addresses (WHATWG URL host parser). (RURL-lxdwuacn)
- `serialize_url(standard = NULL)` is now an error instead of silently serializing as `"whatwg"`. (RURL-ouorolhb)
- `index_page_handling = "strip"` no longer leaves a path ending in `.` or `..`: `http://example.com/../index.html` now cleans to `http://example.com/`.
- `host_encoding = "unicode"`, and so `profile = "seo"`, keeps a trailing root dot: `example.com.` no longer becomes `example.com`.
- Error rows, including hostless `file:` errors such as `file:C:/W` and `file:.`, now return `NA` for `clean_url` instead of a string with a fabricated authority.
- Index-page and trailing-slash stripping no longer treat an encoded `%2F` as a path separator when combined with `path_encoding = "decode"` or `"encode"`.
- An input element marked `Encoding(x) <- "bytes"` becomes a `parse_status = "error"` row instead of aborting the whole vectorized call.
- Under `url_standard = "rfc3986"`, an authority-only URL has an empty path, not `"/"`; `form = "normalized"` adds the `/` for every scheme (RFC 3986 §6.2.3). (RURL-epoinamh)
- Under `url_standard = "rfc3986"`, a percent-encoded internationalized host gets `domain` and `tld` again, and `parse_status` is `ok` instead of `warning-invalid-tld`.
- Host acceptance of the 15 WHATWG code points (`whatwg`) and the 11 `sub-delims` (`rfc3986`) no longer depends on slash count, control characters elsewhere or the scheme. (ADR 0013)
- Under `url_standard = "rfc3986"`, a component ending in a raw LF, VT, FF or CR is rejected, and `foo://h:80\v/` no longer reads as port 80.
- Under `url_standard = "rfc3986"`, host percent-decoding happens only in `form = "normalized"`, for every scheme, and non-unreserved triplets like `%7C` stay encoded (RFC 3986 §6.2.2.2).
- Under `url_standard = "rfc3986"`, a `%7F` in the host stays encoded instead of decoding to a raw DEL, and surviving host triplets keep uppercase hex (RFC 3986 §6.2.2).
- A host-less `file:` URL has an empty host under `whatwg`, so `serialize_url()` emits `//`: `file:C|/m/` → `file:///C:/m/`. (RURL-uhwivndf)
- `serialize_url()` no longer invents userinfo or drops the host for backslash inputs in the CVE-2020-26291 hostname-confusion family. (RURL-yeikpnan)
- `url_standard = "whatwg"` percent-encodes the `user` and `password` columns with the userinfo percent-encode set: `a^b` becomes `a%5Eb`, `é` becomes `%C3%A9`.
- The general/opaque route now reports `user` and `password` instead of `NA`, splitting the userinfo at the first `:`.
- `url_standard = "whatwg"` accepts a userinfo containing a space, C0 control or DEL, percent-encoding it: `http://a b@host/` gives user `a%20b`.
- `url_standard = "whatwg"` strips leading and trailing C0 control or space from the input and reports `leading-trailing-stripped` in `get_url_diagnostics()` (WHATWG URL basic parser step 1). (RURL-yvxpanix)
- Under `scheme_acceptance = "general"`, opaque paths are C0-percent-encoded and VT or FF no longer fail the parse; under `whatwg`, `path_encoding = "encode"` encodes `^`. (RURL-qxpgcwie)
- Under `scheme_acceptance = "general"`, non-special IPv6 hosts are WHATWG-serialized: `[1:2:0:0:5:0:0:0]` → `[1:2:0:0:5::]`. (RURL-cyxegfjs)
- Under `scheme_acceptance = "general"`, tab, LF and CR are stripped for non-special schemes too, so `foo://ho<TAB>st/` gives host `host`. (RURL-lsgdeisl)
- Under `scheme_acceptance = "general"`, an opaque URL ending in `:<digits>`, such as `urn:ietf:rfc:2648`, now parses instead of failing. (RURL-jnvtttfm)
- `url_standard = "whatwg"` with `scheme_acceptance = "general"` rejects host-missing authorities such as `sc://@/`, `sc://:/` and `data://:` (WHATWG URL host state).
- `url_standard = "rfc3986"` applies RFC 3986's generic grammar to every scheme, rejecting raw `|`, `\`, `"` and a repeated `@` in an authority (RFC 3986 Appendix A).
- `safe_parse_urls()` turns a list element of the wrong length into an error row instead of failing the call.
- `safe_parse_urls()` no longer copies input names into the result's row names.
- `get_mailto_recipients()` no longer errors when `scheme_acceptance` is left at its default.

### New features

- New `check_schemes()` reports each URL's scheme, class and `allowed` status against an `allowed_schemes` allowlist; it never changes how a URL parses (ADR 0006). (RURL-zfycisur)
- New `credential_handling = c("strip", "reject")` on `get_clean_url()`, `safe_parse_url()`, `safe_parse_urls()` and profiles; `"reject"` returns `NA` for URLs carrying userinfo (RFC 3986 §3.2.1). (RURL-kjrjbhef)
- New `get_url_key()` and `url_key_policy()` return a versioned identity key that no cleaning option affects; unparseable input keys to `NA`, with the reason in `attr(, "keyability")`.
- New `url_inner_join()`, `url_left_join()`, `url_right_join()`, `url_full_join()`, `url_semi_join()` and `url_anti_join()` join data frames on `get_url_key()`, keeping duplicate matches; `canonical_join()` is unchanged.
- New `serialize_url()` returns the full standard serialization, credentials and fragment included; `standard = "rfc3986"` takes `form = "source"` or `"normalized"` (RFC 3986 §5.3, §6.2.2). Scheme-less input returns `NA`.
- New `format_url()` renders a URL for display: credentials become `<redacted>`, invisible and bidi code points become `<U+XXXX>`, and IDN hosts show both spellings.
- `resolve_url()` gains `output = "serialized"`, returning the resolved URL through `serialize_url()`; it requires an explicit `url_standard`. The default `output = "clean"` is unchanged.
- New `get_parse_verdicts()` splits `parse_status` into syntax, policy and Public Suffix List annotation verdicts; `parse_status` is unchanged and not deprecated.
- `get_password()`, `get_query()`, `get_fragment()` and `get_port()` gain `url_standard`, `scheme_policy` and `scheme_acceptance`; output at the defaults is unchanged.

### Documentation

- The documentation site moved to <https://bart-turczynski.gitlab.io/rurl/>. (RURL-vkltgopc)
- `?get_url_diagnostics` gains a canonical list of all 32 diagnostic tokens, their meanings and the postures they fire under.
- `safe_parse_url()` documents that its `query` and `fragment` columns are percent-encoded under `url_standard = "whatwg"`.
- `vignette("url-standard")` gains a *Conformance posture* section with the WHATWG and RFC 3986 conformance figures and their limits.
- `vignette("url-standard")` documents `scheme_acceptance` as its own axis: `"general"` parses `mailto:`, `data:` and `tel:`, and `url_standard` decides how the result reads.

## rurl 2.7.0

### Breaking changes

- `file:` URLs under `url_standard = "rfc3986"` and the default (`NULL`) are parsed by rurl, not libcurl, so results no longer differ by operating system (RURL-obsweger).
- `file:` URLs must be valid RFC 3986: `file://C|/x`, backslash paths and `file://[example]/` are errors; `file://C%7C` still parses (RFC 8089 Appendix E).
- A port in a `file:` URL is a parse error, and userinfo is now reported in the `user` column (RFC 8089 §2, Appendix E.1).
- Diagnostic `file-forbidden-component` is replaced by `file-userinfo-extension` and `file-component-outside-rfc8089`.
- Returned non-ASCII character components are marked `Encoding() == "UTF-8"` (was `"unknown"`); bytes are unchanged, but `identical()` against unmarked strings can differ.

### New features

- New `engine` argument on `safe_parse_url()`, `get_domain()`, `get_tld()` and related functions resolves against a given `pslr::psl_engine()`; requires `pslr` (>= 1.1.0) (RURL-mhibnqbd).
- New `scheme_acceptance` argument (`"web"` default, `"general"`): `"general"` parses opaque, non-special, generic and `file:` URLs, and recognizes `ws:`/`wss:` (ADR 0012).
- Under `scheme_acceptance = "general"`, `get_host()`, `get_domain()`, `get_tld()`, `get_subdomain()`, `get_user()` and `get_userinfo()` extract a `mailto:` first recipient's parts (ADR 0012).
- `get_scheme()` and `get_scheme_class()` gain `scheme_acceptance`, `scheme_policy` and `url_standard`; under `"general"` `mailto:` is `"mailto"` and `"non-special"`.
- New `get_mailto_recipients()` returns one row of structural facts per `mailto:` recipient; `smtp_wire = TRUE` adds SMTP wire facts (RFC 6068 §2, RFC 5321).
- New `profile` argument (`"browser"`, `"whatwg"`, `"rfc-syntax"`, `"seo"`/`"canonical"`) and `url_profile()` inspector; explicit arguments override the profile (ADR 0012).
- `get_url_diagnostics()` reports 11 new facts for general-mode parsing, only under `scheme_acceptance = "general"` (ADR 0012).
- New `is_valid_host()` and `check_hosts()` test whether a parsed host is a practical `web`, `dns`, `registrable`, `seo` or `url` host.

### Bug fixes

- Output no longer depends on the session character set: under `LC_ALL=C`, IDN `domain`/`tld` were `NA` and non-ASCII paths were mis-encoded (RURL-vzqmwthu).
- Scheme and host lowercasing is ASCII-only (RFC 3986 §6.2.2.1), so Turkish or Azeri sessions no longer yield `wıkı.example.com` or reject `FILE:` (RURL-ugfpuotu).

## rurl 2.5.0

### New features

- `path_encoding` now combines with any `url_standard` profile instead of erroring; `"encode"` gives the browser form, e.g. `/école` to `/%C3%A9cole` (ADR 0011).

### Bug fixes

- `url_standard = "rfc3986"` accepts RFC 3986 `reg-name` sub-delimiters (`! $ & ' ( ) * + , ; =`) in hosts (RFC 3986 §3.2.2).
- `url_standard = "whatwg"` accepts IPv4 hosts with empty hex parts, such as `https://0x.0x.0`, serialized as `0.0.0.0`.
- `url_standard = "whatwg"` with `host_encoding = "idna"` applies UTS-46 ignored-code-point mappings, so `https://a%C2%ADb/` becomes `https://ab/`.

## rurl 2.4.0

### New features

- New `scheme_policy` argument: `"infer"` (default) adds `http://` to scheme-less hosts, `"require"` makes them `parse_status = "error"` (ADR 0010).

## rurl 2.3.0

### Bug fixes

- `url_standard = "whatwg"` strips ASCII tab, LF and CR from input before parsing and reports `control-char-stripped` (WHATWG URL basic parser).
- `url_standard = "whatwg"` rejects forbidden host code points and failed domain-to-ASCII hosts as `parse_status = "error"` (WHATWG URL host parsing).
- `url_standard = "whatwg"` maps U+3002, U+FF0E and U+FF61 to `.` in the authority, so `http://127。0。0。1/` becomes `http://127.0.0.1/` (UTS-46).
- `url_standard = "whatwg"` accepts host code points `! " $ & ' ( ) * + , ; = ` { }` that libcurl rejects and reports `host-charset-shimmed` (ADR 0009).

### Diagnostics

- New `get_url_diagnostics()` tokens `control-char-stripped` and `host-charset-shimmed`, emitted under `"whatwg"`.

## rurl 2.2.2

### Bug fixes

- `url_standard = "whatwg"` rejects hosts ending in an invalid numeric label, such as `http://foo.09` or `http://1.2.3.08` (WHATWG URL IPv4 parser).

## rurl 2.2.1

### Packaging

- `Remotes:` now pin `pslr@v1.0.2` and `punycoder@v1.2.0`, so a fresh `pak` install resolves again.

## rurl 2.2.0

### New features

- New `url_standard` selector (`NULL` default, `"rfc3986"`, `"whatwg"`) on the parsers, accessors and `canonical_join()`; conflicting `path_encoding`, `path_normalization` or `case_handling` values error.
- New `port_handling` option for `clean_url`: `"exclude"` (default), `"keep"`, `"strip_default"`, `"strip_all"`.
- Under `url_standard = "whatwg"`, a literal backslash is a path separator for `http`, `https` and `ftp`; `%5C` never is.
- New `resolve_url()` resolves relative references against a base URL and returns the resolved `clean_url` (RFC 3986 §5).

### Diagnostics and classification helpers

- New `get_host_type()`, `get_scheme_class()` and `get_url_diagnostics()`, returning `NA` unless `url_standard` is set.

### Documentation

- New `url_standard` vignette comparing RFC 3986 and WHATWG on canonical cases.

## rurl 2.1.0

### New features

- `safe_parse_url()` and `safe_parse_urls()` gain `domain_ascii`, `domain_unicode`, `tld_ascii` and `tld_unicode` columns, independent of `host_encoding`.

## rurl 2.0.0

### New features

- New `query_handling` option (`"drop"` default, `"filter"`, `"allow"`, `"keep"`) with `params_keep`, `params_drop`, `sort_params` and related options keeps query parameters in `clean_url`.
- `get_clean_url()` and `get_query()` gain the query-filter arguments; `get_query()` defaults to `query_handling = "keep"`.
- New `query_param_summary()` tabulates query parameters across URLs, with a `would_drop` preview of `query_handling = "filter"`.
- The `clean_url` query is exempt from `case_handling`.

### Breaking changes

- `path_normalization = "none"` keeps `.`/`..` segments, and `%2e` is never resolved as traversal (RFC 3986 §5.2.4).
- Scheme-less input that is not host-shaped, such as `"example"` or `"/relative/path"`, returns `parse_status = "error"`; `localhost` is still accepted.
- Non-canonical IP literals (integer, hex, octal, short-form, out-of-range, leading-zero octets) return `"error"` instead of a coerced address.
- Only `http`, `https`, `ftp` and `ftps` are supported; any other scheme, including `mailto:` and `ws://`, returns `"error"`.
- New `parse_status` `"warning-userinfo"` for scheme-less userinfo input such as `"user@example.com"`, with `clean_url = NA`.

### Performance

- `safe_parse_urls()` parses each unique URL once, and accessors with different options share one cache entry per URL.

### Behavior changes

- `get_*()` accessors return unnamed vectors instead of vectors named by the input URLs.
- The `full_parse` cache is bounded at 100000 entries by default; `rurl_cache_config(max_full_parse = Inf)` restores the old behavior.
- An empty `query`, `fragment`, `user` or `password` is reported as `NA` on every libcurl version.
- `safe_parse_urls()` accepts factor input.

### Bug fixes

- Scheme-less `host:port` input such as `"example.com:8080/x"` now reports `"ok"` instead of `"error"`.
- Path, fragment and userinfo are no longer percent-decoded during parsing, so `clean_url` changes for percent-encoded paths.

## rurl 1.4.1

### Bug fixes

- IPv6 literals with an embedded IPv4 tail, such as `[::ffff:127.0.0.1]`, report `is_ip_host = TRUE` and `"ok"` (RFC 4291 §2.2).

### Infrastructure

- `oysteR` and `rosv` added to Suggests for dependency vulnerability audits.

## rurl 1.4.0

### Dependencies

- Requires `pslr` (>= 1.0.2) and `punycoder` (>= 1.2.0).

### Accessor improvements

- `get_path()` gains `path_normalization`, `index_page_handling`, `trailing_slash_handling` and `path_encoding`.
- `get_scheme()` gains `scheme_relative_handling`; `get_parse_status()`, `get_clean_url()` and `get_host()` gain `source`.
- `get_host()`, `get_domain()`, `get_tld()` and `get_subdomain()` gain `host_encoding`.

### Behavior change

- `get_domain()`, `get_tld()` and `get_subdomain()` follow `host_encoding` (default `"keep"`); pass `"unicode"` for the previous output.

### Documentation & metadata

- New package help page, `?rurl`.

## rurl 1.3.0

### Dependencies

- Public Suffix List matching is delegated to `pslr` (>= 1.0.1); `punycoder` (>= 1.1.0) is required.

### Behavior changes (PSL correctness)

- Wildcard and exception rules are honored, e.g. `get_tld("a.b.kobe.jp")` is `"b.kobe.jp"` and `get_tld("www.ck")` is `"ck"`.
- IDN hosts resolve a registered domain in every section, e.g. `get_domain("example.рф")`.
- `domain` follows `tld_source`, so a host with no suffix in that section gets `domain = NA`.

### Cache changes

- The `domain` and `tld` caches and their `rurl_cache_config()` arguments are removed.

## rurl 1.2.0

### Dependencies

- Requires `punycoder` (>= 1.0.0), now on CRAN.

### Behavior changes

- `case_handling` defaults to `"lower_host"` everywhere, so `get_path()` no longer lowercases paths (RFC 3986 §6.2.2.1).

## rurl 1.1.0

### New features

- `canonical_join()` gains `name_A` and `name_B` to set output column names.
- `canonical_join()` gains `join_parse_status`; `"ok_or_warning"` also joins `warning-*` rows.
- New `rurl_cache_info()` and `rurl_cache_config()` inspect and configure the memoization caches.

### Bug fixes

- `safe_parse_url()` returns `port` as an integer, and `safe_parse_urls()` no longer errors on URLs with a port.
- Bracketed IPv6 hosts are detected as IP hosts, with `parse_status = "ok"`.

### Behavior changes (potentially breaking)

- `subdomain_levels_to_keep = N` keeps the `N` rightmost subdomain labels instead of all of them.

---

## rurl v1 (GitHub Release) - 2026-02-16

- First stable release, tagged `v1` (package version `1.0.0`).

---

## rurl 0.3.0

### Highlights

- New `case_handling` and `trailing_slash_handling` parameters in `safe_parse_url()` and `get_clean_url()`.
- New `canonical_join()` joins datasets on normalized URL keys.
- Better handling of malformed schemes like `htp://`, scheme-less URLs with ports, and IPv6.

---

## rurl 0.2.0

### Highlights

- Accurate TLD extraction for ASCII and Unicode domains.
- Falls back gracefully when `urltools` is unavailable.
- NFC normalization with `stringi`.

## rurl 0.1.3

### Improvements

- Dropped the `psl` dependency; registered domains come from a built-in Public Suffix List.

### Documentation

- LICENSE clarifies MIT plus Mozilla Public Suffix List attribution.

## rurl 0.1.2

### Stabilization & Coverage

- Examples for all exported functions.
- Removed `hello()`.

## rurl 0.1.0

- All `get_*()` functions are vectorized over character vectors.
- Uses `curl` and `psl` for parsing.
