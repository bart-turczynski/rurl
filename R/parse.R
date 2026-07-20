# URL parsing: public parse functions, scalar wrapper, and impl orchestration.

#' Parse a URL comprehensively, extracting and deriving all relevant components.
#'
#' This function serves as the core URL processing engine. It parses a URL,
#' handles protocol and www prefix modifications, detects IP addresses,
#' and derives components like the registered domain and top-level domain (TLD).
#' Results are memoized for performance when processing large datasets.
#'
#' @param url A single URL string to be parsed. For vectors, use
#'   \code{\link{safe_parse_urls}}.
#' @param protocol_handling A character string specifying how to handle
#'   protocols. Defaults to "keep".
#'   Regardless of this option, rurl only processes authority-based URLs whose
#'   scheme is one of http, https, ftp, or ftps; a scheme-bearing input with any
#'   other scheme (e.g. \code{mailto:}, \code{tel:}, \code{ws:}) yields
#'   \code{parse_status = "error"}. Scheme inference (below) also requires the
#'   input to be host-shaped: a scheme-less string that is not a host (e.g.
#'   \code{"asdfghjkl"}, \code{"12345"}, \code{"/path"}) or is a non-canonical
#'   IP literal (integer/hex/octal/short forms, or leading-zero octets like
#'   \code{"192.168.010.1"}) is rejected as \code{"error"} rather than having a
#'   scheme fabricated for it.
#'   \itemize{
#'     \item{"keep": If a supported scheme exists (http, https, ftp, ftps), it's
#'     used. If no scheme and the input is host-shaped, "http://" is added;
#'     otherwise the input is not a URL and yields \code{"error"}.}
#'     \item{"none": If a supported scheme exists, it's used. If no scheme, then
#'     no scheme is used (scheme component will be NA).}
#'     \item{"strip": Any existing scheme is removed (scheme component will be
#'     NA).}
#'     \item{"http": The scheme is forced to be "http".}
#'     \item{"https": The scheme is forced to be "https".}
#'   }
#' @param www_handling A character string specifying how to handle "www"
#'   and `www[number]` prefixes in the host. Defaults to "none".
#'   \itemize{
#'     \item{"none": (Default) Leaves the host's www prefix (or lack thereof)
#'     untouched.}
#'     \item{"strip": Removes any "www." or `www[number].` prefix.}
#'     \item{"keep": Ensures the host starts with "www.". If it has
#'     `www[number].`, it's normalized to "www.". If no www prefix, "www." is
#'     added. An empty input host remains empty.}
#'     \item{"if_no_subdomain": If the host is a bare registered domain (e.g.,
#'     "example.com"), "www." is added. If the host already has a "www." or
#'     `www[number].` prefix, it is normalized to "www." (e.g.,
#'     "www1.example.com" becomes "www.example.com"; "www1.sub.example.com"
#'     becomes "www.sub.example.com"). If a non-www subdomain exists (e.g.,
#'     "sub.example.com" or the normalized "www.sub.example.com"), the host is
#'     not further altered. An empty input host remains empty.}
#'   }
#' @param tld_source Which TLD source to use for TLD extraction: "all", "icann",
#'   or "private". Defaults to "all".
#' @param case_handling A character string specifying how to handle the case of
#'                      the cleaned URL. Defaults to "lower_host", the
#'                      RFC 3986 §6.2.2.1 normalization (scheme and host are
#'                      case-insensitive and folded to lowercase; the path is
#'                      case-sensitive and preserved).
#'   \itemize{
#'     \item{"lower_host": (Default) Lowercases scheme and host only; the path
#'     keeps its original casing.}
#'     \item{"keep": Preserves casing of the reconstructed URL.}
#'     \item{"lower": Converts the cleaned URL to lowercase.}
#'     \item{"upper": Converts the cleaned URL to uppercase.}
#'   }
#' @param trailing_slash_handling A character string specifying how to handle
#'   trailing slashes in the path component of the cleaned URL. Defaults to
#'   "none".
#'   \itemize{
#'     \item{"none": (Default) No specific handling is applied. Path remains as
#'     is after initial parsing.}
#'     \item{"keep": Ensures a trailing slash. If a path exists and doesn't end
#'     with one, it's added. If path is just "/", it's kept.}
#'     \item{"strip": Removes a trailing slash if present, unless the path is
#'     solely "/".}
#'   }
#' @param index_page_handling A character string specifying how to handle
#' index/default pages. Defaults to "keep".
#'   \itemize{
#'     \item{"keep": (Default) Leave index/default page segments untouched.}
#'     \item{"strip": Remove a trailing index.* or default.* segment
#'     (case-insensitive).}
#'   }
#' @param path_normalization How to normalize path structure. Defaults to
#' "none". rurl owns dot-segment resolution: the path is read from the input
#' verbatim (not from libcurl's pre-normalized path), so \code{"none"} preserves
#' \code{.} / \code{..} segments (\code{/a/../b} stays \code{/a/../b}) and only
#' the settings below change them. Resolution follows RFC 3986 section 5.2.4 and
#' acts on \emph{literal} \code{.}/\code{..} segments only — a percent-encoded
#' \code{\%2e} is a normal path byte, never a dot segment, so it is never
#' treated as traversal.
#'   \itemize{
#'     \item{"none": (Default) No normalization; dot and slash structure is
#'     preserved exactly as written.}
#'     \item{"collapse_slashes": Collapse duplicate slashes in the path.}
#'     \item{"dot_segments": Resolve . and .. segments per RFC 3986.}
#'     \item{"both": Apply both collapse_slashes and dot_segments.}
#'   }
#' @param scheme_relative_handling How to handle URLs starting with "//".
#' Defaults to "keep".
#'   \itemize{
#'     \item{"keep": Parse using http but return scheme as NA and set status to
#'     "ok-scheme-relative".}
#'     \item{"http": Assume http for parsing and output.}
#'     \item{"https": Assume https for parsing and output.}
#'     \item{"error": Treat scheme-relative URLs as invalid.}
#'   }
#' @param host_encoding How to present the host in `clean_url`. Defaults to
#' "keep".
#'   \itemize{
#'     \item{"keep": Leave host as parsed by curl (may preserve original case).}
#'     \item{"idna": Convert Unicode host labels to Punycode (IDNA) for the
#'     cleaned URL.}
#'     \item{"unicode": Decode Punycode labels to Unicode for the cleaned URL.}
#'   }
#' @param path_encoding How to present the path percent-encoding in `clean_url`
#' — the readable-vs-browser rendering choice (the path analog of
#' `host_encoding`). Defaults to "keep". This is an orthogonal presentation
#' knob: it is independent of `url_standard` and layers on top of any profile
#' (e.g. `url_standard = "whatwg", path_encoding = "encode"` emits the
#' WHATWG-parsed path in browser form), exactly like `host_encoding`. Only
#' "keep" preserves a profile's canonical identity path verbatim; "encode" and
#' "decode" are presentation forms that may re-encode or decode reserved octets
#' (so `%2F` may fold to a path-separating `/`), independent of whether a
#' profile is set.
#'   \itemize{
#'     \item{"keep": Leave the path percent-encoding untouched (the path is
#'     preserved as written in the URL, so `%2F` stays `%2F` rather than
#'     decoding into a path-separating `/`). With no `url_standard`, rurl keeps
#'     its historical RFC-style percent-hex case canonicalization, so `%2f`
#'     becomes `%2F`. Under `url_standard = "whatwg"`, existing percent-triplet
#'     spelling is preserved byte-for-byte. Use "encode" to additionally
#'     normalize which bytes are encoded.}
#'     \item{"encode": The browser/percent-encoded rendering. Decodes the path
#'     first, then percent-encodes each segment (slashes preserved), so a
#'     readable non-ASCII path is emitted in its percent-encoded UTF-8 form.}
#'     \item{"decode": The readable rendering. Percent-decodes UTF-8 sequences
#'     in the path, so a percent-encoded segment is shown as readable text.}
#'   }
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many
#' levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed
#'     beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or
#'     added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com';
#'          'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from
#'     right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1,
#'          'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes
#'          'www.one.example.com'.}
#'   }
#' @param query_handling A character string controlling whether (and how) the
#' query string is included in `clean_url`. Defaults to "drop", which preserves
#' the historical query-free `clean_url`. The raw `query` result field is never
#' affected by this option — it always reports the faithful original query.
#'   \itemize{
#'     \item{"drop": (Default) `clean_url` carries no query, exactly as before.}
#'     \item{"filter": Keep contentful params, dropping known trackers via a
#'     built-in denylist (e.g. `utm_*`, `fbclid`, `gclid`). `params_drop`
#'     extends the denylist; `params_keep` rescues names (winning over both the
#'     denylist and empty-dropping).}
#'     \item{"allow": Keep only params whose names match `params_keep`; all
#'     others are dropped. Here `params_keep` is an inclusion criterion only,
#'     not an empty-rescue.}
#'     \item{"keep": Keep every param, re-encoded into canonical form (not the
#'     verbatim original — that stays on the `query` field).}
#'   }
#'   In every non-"drop" mode the surviving query is re-encoded canonically
#'   (uppercase percent-hex, spaces as `%20`) and appended after the path. The
#'   query is intentionally EXEMPT from `case_handling` (query values are
#'   case-sensitive — tokens, IDs, signatures), so under
#'   `case_handling = "lower"` or `"upper"` the `clean_url` is no longer
#'   uniformly cased: scheme/host/path fold but the query keeps its original
#'   case. Because `clean_url` is the \code{\link{canonical_join}} key, any
#'   non-"drop" mode also brings the query into that join key (so `?id=1` and
#'   `?id=2` stop collapsing, while `utm`-only differences still collapse under
#'   "filter").
#' @param params_keep Character vector of parameter-name globs (only `*` is
#'   special), or `NULL` (default). In "filter" mode this is the rescue list; in
#'   "allow" mode it is the allowlist. Ignored in "drop"/"keep".
#' @param params_drop Character vector of parameter-name globs to add to the
#'   built-in denylist in "filter" mode, or `NULL` (default). Ignored in
#'   "drop"/"allow"/"keep".
#' @param sort_params Logical (default `FALSE`). When `TRUE`, surviving params
#'   are stably sorted by decoded key. Active in "filter"/"allow"/"keep".
#' @param empty_param_handling One of "keep" (default) or "drop". "drop" removes
#'   empty-valued params (e.g. `?ref=`), except those rescued by `params_keep`
#'   in "filter" mode.
#' @param params_case_sensitive Logical (default `FALSE`). Controls whether the
#'   denylist and `params_keep`/`params_drop` matching is case-sensitive.
#' @param decode_plus Logical (default `FALSE`). When `TRUE`, `+` in query
#'   values is treated as a space (HTML-form decoding) before percent-decoding.
#'   `FALSE` keeps `+` literal (RFC 3986 generic behavior).
#' @param port_handling A character string controlling whether the port
#' appears in `clean_url`. Defaults to "exclude", today's only historical
#' behavior. This knob is standalone and standard-independent (editorial, like
#' `www_handling`) -- `url_standard` never governs whether it may be set.
#'   \itemize{
#'     \item{"exclude": (Default) The port never appears in `clean_url`.}
#'     \item{"strip_all": Explicit alias of "exclude".}
#'     \item{"keep": Include the syntactic port when present, including a
#'     default port under `url_standard = "whatwg"`. This is an explicit
#'     non-parity override for callers that need the input's port spelling.}
#'     \item{"strip_default": Keep only non-default ports (using the same
#'     scheme-default table), independent of `url_standard`.}
#'   }
#' @param scheme_policy Controls whether scheme-less, host-shaped input is
#' *accepted* (an input-acceptance axis, distinct from `protocol_handling`,
#' which only controls how the scheme is *presented*, and from `url_standard`,
#' which controls interpretation). Defaults to "infer".
#'   \itemize{
#'     \item{"infer": (Default) Fabricate `http://` for scheme-less host-shaped
#'     input (e.g. `example.com` parses as `http://example.com`), a
#'     browser-omnibox-style affordance. This is the historical behavior.}
#'     \item{"require": Reject scheme-less input — a scheme-less host-shaped
#'     value becomes `parse_status = "error"` rather than gaining a fabricated
#'     scheme. Use this for a strict, pure-parser posture. Note this governs
#'     only bare host input; scheme-relative `//host` input is governed
#'     separately by `scheme_relative_handling`.}
#'   }
#' @param scheme_acceptance Which scheme *tokens* may enter parsing (a
#' scheme-acceptance axis, distinct from `scheme_policy`, which governs
#' scheme-*less* input, and from `url_standard`, which governs interpretation).
#' Defaults to "web".
#'   \itemize{
#'     \item{"web": (Default) Only the curated web-scheme allowlist
#'     (`http`/`https`/`ftp`/`ftps`/`file`) is admitted; a scheme-bearing input
#'     outside it is `parse_status = "error"`. This is the historical,
#'     byte-for-byte compatible behavior.}
#'     \item{"general": Admit any syntactically valid scheme token and parse
#'     opaque (`mailto:x`), non-special (`foo://host`), and RFC-generic URLs.
#'     Requires an explicit `url_standard` (`"rfc3986"` or `"whatwg"`), which
#'     decides the interpretation; `general` with `url_standard = NULL` is an
#'     error. Non-special / opaque hosts receive no www-stripping, no domain/TLD
#'     derivation, and are never run through the IDNA/punycode helpers.}
#'   }
#' @param url_standard Optional top-level standard profile: `NULL` (default),
#'   `"rfc3986"`, or `"whatwg"`. With `NULL` the behavior is exactly what the
#'   individual low-level options select (fully backward compatible). When set,
#'   it selects a coherent set of standard-conformant behaviors for the axes it
#'   governs — path percent/dot handling, the host IPv4/reg-name model, and
#'   `case_handling` — so callers do not have to hand-assemble the low-level
#'   knobs. Passing a governed low-level knob (`path_normalization` or
#'   `case_handling`) with a value the selected
#'   profile would not choose is an error; passing the value the profile would
#'   pick is accepted (only `case_handling = "lower_host"` is accepted under a
#'   selector — `"keep"`, `"lower"`, and `"upper"` all conflict, since `"lower"`
#'   also lowercases the path, which neither standard sanctions). Added as the
#'   last argument so existing positional calls keep their meaning; always
#'   pass it by name. Under `"whatwg"` the selector additionally recognizes a
#'   literal backslash as a path separator for WHATWG-special schemes
#'   (`http`/`https`/`ftp`) and nulls default ports in parse output; use
#'   `port_handling = "strip_default"` for spec-style clean URL port rendering.
#'   See \code{\link{resolve_url}} for `url_standard`-governed
#'   reference resolution. The selector does **not** govern whether
#'   `port_handling` may be set (it is a standalone editorial knob), nor does it
#'   govern `path_encoding` (an orthogonal path-presentation knob that layers
#'   on any profile), IDNA rendering, or query handling.
#' @param engine Optional \pkg{pslr} engine controlling which Public Suffix List
#'   backs domain / TLD / subdomain extraction: `NULL` (default) resolves
#'   against \pkg{pslr}'s session-global default list — exactly the historical
#'   behavior — while a `pslr::psl_engine()` snapshot resolves against that
#'   specific list, per request, without mutating any global state (never call
#'   `pslr::psl_use()` for this). Use it to pin a particular list version or to
#'   load an alternate list via `pslr::psl_engine(source = "path", path = ...)`.
#'   **Process-local:** an engine holds a C++ external pointer that does **not**
#'   serialize across R sessions or parallel workers — build it in the process
#'   that uses it; never cache it to disk or send it to a worker (rebuild one
#'   per process instead). Only the domain-derived outputs (`domain`, `tld`,
#'   and the subdomain-trimmed host / `clean_url`) depend on it.
#' @param profile Optional named profile bundling several knobs at once: `NULL`
#'   (default; behaves exactly as the individual arguments select, fully
#'   backward compatible), `"browser"`, `"whatwg"`, `"rfc-syntax"`, `"seo"`, or
#'   the `"seo"` alias `"canonical"`. A profile is **separate** from
#'   `url_standard` (it bundles acceptance, interpretation, leniency, and
#'   canonicalization together) and expands only into arguments you did not
#'   supply explicitly — an explicit argument always overrides the profile.
#'   `"browser"` is a browser-*like* fix-up posture (http-prepending; not
#'   Chrome-faithful); `"whatwg"` is the absolute-URL no-base posture that
#'   *rejects* scheme-less input (unlike a bare `url_standard = "whatwg"`);
#'   `"rfc-syntax"` is RFC 3986 generic syntax as parsing, not normalization
#'   (case and dot-segments are preserved); `"seo"`/`"canonical"` is rurl's
#'   origin-cleaning intent (https, strip www / trailing slash / index page,
#'   filter tracking params). Inspect the resolved bundle with
#'   \code{\link{url_profile}}. Also accepted by \code{\link{canonical_join}}
#'   (forwarded through its \code{...}).
#' @return A named list with the following components:
#'   \itemize{
#'     \item `original_url`: The original URL string provided.
#'     \item `scheme`: The scheme (e.g., "http", "https").
#'     \item `host`: The host (e.g., "www.example.com"). NA if the host becomes
#'     empty after processing.
#'     \item `port`: The port number.
#'     \item `path`: The path component (e.g., "/path/to/resource").
#'     \item `query`: The raw query string as written in the URL, preserved
#'     byte-for-byte (e.g., "name=value"); not percent-decoded. A present-but-
#'     empty query (e.g. from a trailing "?") is reported as NA.
#'     \item `fragment`: The fragment identifier as written in the URL
#'     (e.g., "section"); not percent-decoded. Empty is reported as NA.
#'     \item `user`: The user name for authentication, as written in the URL;
#'     not percent-decoded. Empty is reported as NA.
#'     \item `password`: The password for authentication, as written in the
#'     URL; not percent-decoded. Empty is reported as NA.
#'     \item `domain`: The registered domain name (e.g., "example.com"). NA if
#'     host is an IP, empty, or derivation fails.
#'     \item `tld`: The top-level domain (e.g., "com"). NA if host is an IP,
#'     empty, or derivation fails.
#'     \item `domain_ascii`, `domain_unicode`: The registered domain in both
#'     canonical spellings, independent of `host_encoding`. For an
#'     internationalized domain, `domain_ascii` is the Punycode/A-label form
#'     (e.g., "xn--mnchen-3ya.de") and `domain_unicode` the decoded Unicode form
#'     (e.g., "münchen.de"); for an ASCII-only domain the two are equal.
#'     Unlike `domain` (which follows `host_encoding`, a rendering choice),
#'     these are stable identity keys — a Unicode host and its A-label share
#'     one `domain_ascii` — so consumers can build an encoding-independent key
#'     from a single parse. NA under the same conditions as `domain`.
#'     \item `tld_ascii`, `tld_unicode`: The public suffix (TLD) in both
#'     canonical spellings, the `tld` analogue of `domain_ascii`/
#'     `domain_unicode`. NA under the same conditions as `tld`.
#'     \item `is_ip_host`: Logical, TRUE if the host is an IP address.
#'     \item `clean_url`: A normalized canonical key reconstructed from
#'     scheme, host, and path, after processing and with case handling
#'     applied. The query is included only when `query_handling != "drop"`
#'     (the default is "drop", so by default the query is excluded); when
#'     included it is filtered/canonicalized per the query options and appended
#'     case-unfolded. The port is included only when
#'     `port_handling != "exclude"` (the default is "exclude", so by default
#'     the port is excluded, as before); fragment and userinfo are always
#'     excluded (use the dedicated components above to retrieve them). With
#'     `path_encoding = "decode"` the path is shown decoded, so `clean_url`
#'     is human-readable rather than guaranteed URL-safe. NA if host is
#'     empty/NA.
#'     \item `parse_status`: Character string indicating parsing outcome
#'       ("ok", "ok-ftp", "ok-scheme-relative", "error", "warning-no-tld",
#'       "warning-invalid-tld", "warning-public-suffix", "warning-userinfo").
#'       "warning-userinfo" marks a scheme-less input carrying userinfo (e.g.
#'       "user@example.com"): host/domain/tld/user still resolve, but
#'       \code{clean_url} is NA (rurl will not fabricate a canonical URL from an
#'       ambiguous, email-shaped, scheme-less string).
#'   }
#'   Returns `NULL` if the URL is fundamentally unparseable (e.g., NA, empty)
#'   or uses a disallowed scheme.
#' @seealso \code{\link{safe_parse_urls}}
#' @importFrom utils tail
#' @importFrom curl curl_parse_url curl_escape curl_unescape
#' @keywords internal
#' @export
#' @examples
#' safe_parse_url(
#'   "http://www.Example.com/Path?q=1#Frag",
#'   protocol_handling = "keep",
#'   case_handling = "lower"
#' )
#' safe_parse_url(
#'   "Example.com/Another",
#'   protocol_handling = "none",
#'   www_handling = "keep",
#'   case_handling = "upper",
#'   trailing_slash_handling = "keep"
#' )
#' safe_parse_url(
#'   "example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> www.example.com
#' safe_parse_url(
#'   "sub.example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> sub.example.com
#' safe_parse_url(
#'   "www1.example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> www.example.com
#' safe_parse_url(
#'   "www1.sub.example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> www.sub.example.com
#' safe_parse_url(
#'   "http://www.example.com/path/",
#'   trailing_slash_handling = "strip"
#' )
#' safe_parse_url("192.168.1.1/test")
#' safe_parse_url("ftp://user:pass@ftp.example.co.uk:21/file.txt")
#' safe_parse_url(
#'   "http://deep.sub.domain.example.com",
#'   subdomain_levels_to_keep = 0
#' )
#' safe_parse_url(
#'   "http://deep.sub.domain.example.com",
#'   subdomain_levels_to_keep = 1
#' )
#' safe_parse_url(
#'   "http://www.deep.sub.domain.example.com",
#'   www_handling = "keep",
#'   subdomain_levels_to_keep = 0
#' )
#' safe_parse_url(
#'   "http://www.deep.sub.domain.example.com",
#'   www_handling = "keep",
#'   subdomain_levels_to_keep = 1
#' )
#' # Query handling: keep contentful params, drop known trackers.
#' safe_parse_url(
#'   "http://example.com/watch?v=abc&utm_source=nl",
#'   query_handling = "filter"
#' )$clean_url
#' # -> "http://example.com/watch?v=abc"
#' # params_keep is a RESCUE in "filter" (wins over the denylist) ...
#' safe_parse_url(
#'   "http://example.com/?utm_source=nl&id=1",
#'   query_handling = "filter", params_keep = "utm_source"
#' )$clean_url
#' # -> "http://example.com/?utm_source=nl&id=1"
#' # ... but an ALLOWLIST in "allow" (only listed names survive).
#' safe_parse_url(
#'   "http://example.com/?a=1&id=2",
#'   query_handling = "allow", params_keep = "id"
#' )$clean_url
#' # -> "http://example.com/?id=2"
#' # "allow" empty-handling asymmetry: params_keep does NOT rescue empties, so
#' # an allowed empty param still drops under empty_param_handling = "drop".
#' safe_parse_url(
#'   "http://example.com/?id=&keep=1",
#'   query_handling = "allow", params_keep = c("id", "keep"),
#'   empty_param_handling = "drop"
#' )$clean_url
#' # -> "http://example.com/?keep=1"
safe_parse_url <- function(url,
                           protocol_handling = c(
                             "keep", "none", "strip", "http", "https"
                           ),
                           www_handling = c(
                             "none", "strip", "keep", "if_no_subdomain"
                           ),
                           tld_source = c("all", "private", "icann"),
                           case_handling = c(
                             "lower_host", "keep", "lower", "upper"
                           ),
                           trailing_slash_handling = c("none", "keep", "strip"),
                           index_page_handling = c("keep", "strip"),
                           path_normalization = c(
                             "none", "collapse_slashes", "dot_segments", "both"
                           ),
                           scheme_relative_handling = c(
                             "keep", "http", "https", "error"
                           ),
                           subdomain_levels_to_keep = NULL,
                           host_encoding = c("keep", "idna", "unicode"),
                           path_encoding = c("keep", "encode", "decode"),
                           query_handling = c(
                             "drop", "filter", "allow", "keep"
                           ),
                           params_keep = NULL,
                           params_drop = NULL,
                           sort_params = FALSE,
                           empty_param_handling = c("keep", "drop"),
                           params_case_sensitive = FALSE,
                           decode_plus = FALSE,
                           port_handling = c(
                             "exclude", "keep", "strip_default", "strip_all"
                           ),
                           scheme_policy = c("infer", "require"),
                           scheme_acceptance = c("web", "general"),
                           url_standard = NULL,
                           engine = NULL,
                           profile = NULL) {
  # Enforce scalar input to keep behavior explicit and predictable
  if (length(url) != 1) {
    stop(
      "safe_parse_url() expects a single URL. Use safe_parse_urls() for",
      " vectors.",
      call. = FALSE
    )
  }

  # url_standard (RURL-eqzkkohm): validate, then conflict-check the governed
  # low-level knobs the caller EXPLICITLY supplied (missing() must be evaluated
  # here, in the public frame). match.arg() resolves partial matches before the
  # comparison so an abbreviated-but-compatible value is accepted.
  url_standard <- .validate_url_standard(url_standard)
  profile <- .validate_profile(profile)
  # `path_encoding` is orthogonal (ADR 0011): not passed here, never conflicts.
  # On the profile path the conflict matrix is SKIPPED: a profile authorizes its
  # own combination (e.g. `rfc-syntax` = rfc3986 + no normalization), and the
  # iron rule lets explicit args override. Direct (profile = NULL) calls keep
  # ADR 0007's conflict behavior unchanged.
  if (is.null(profile)) {
    .check_url_standard_conflicts(url_standard, .governed_supplied(
      path_normalization = if (missing(path_normalization)) {
        NULL
      } else {
        match.arg(path_normalization)
      },
      case_handling =
        if (missing(case_handling)) NULL else match.arg(case_handling)
    ))
  }

  # Validate and normalize options once (match.arg + subdomain check)
  po_args <- list(
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = tld_source,
    case_handling = case_handling,
    trailing_slash_handling = trailing_slash_handling,
    index_page_handling = index_page_handling,
    path_normalization = path_normalization,
    scheme_relative_handling = scheme_relative_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding,
    path_encoding = path_encoding,
    query_handling = query_handling,
    params_keep = params_keep,
    params_drop = params_drop,
    sort_params = sort_params,
    empty_param_handling = empty_param_handling,
    params_case_sensitive = params_case_sensitive,
    decode_plus = decode_plus,
    port_handling = port_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard,
    engine = engine
  )
  if (!is.null(profile)) {
    po_args <- .merge_profile_args(po_args, .resolve_profile(profile, list(
      url_standard = url_standard,
      scheme_acceptance =
        if (missing(scheme_acceptance)) NULL else match.arg(scheme_acceptance),
      scheme_policy =
        if (missing(scheme_policy)) NULL else match.arg(scheme_policy),
      scheme_relative_handling = if (missing(scheme_relative_handling)) {
        NULL
      } else {
        match.arg(scheme_relative_handling)
      },
      path_normalization = if (missing(path_normalization)) {
        NULL
      } else {
        match.arg(path_normalization)
      },
      case_handling =
        if (missing(case_handling)) NULL else match.arg(case_handling),
      protocol_handling =
        if (missing(protocol_handling)) NULL else match.arg(protocol_handling),
      www_handling =
        if (missing(www_handling)) NULL else match.arg(www_handling),
      trailing_slash_handling = if (missing(trailing_slash_handling)) {
        NULL
      } else {
        match.arg(trailing_slash_handling)
      },
      index_page_handling = if (missing(index_page_handling)) {
        NULL
      } else {
        match.arg(index_page_handling)
      },
      query_handling =
        if (missing(query_handling)) NULL else match.arg(query_handling)
    )))
  }
  opts <- do.call(.parse_options, po_args)

  ._safe_parse_url_scalar(url, opts)
}

# Vectorized wrapper for safe_parse_url
# Returns a data.frame for easy downstream use
#' Parse multiple URLs and return a data.frame of components
#'
#' Vectorized wrapper around \code{\link{safe_parse_url}} that returns a
#' data.frame with one row per input URL.
#'
#' @param url A character vector of URLs to be parsed.
#' @inheritParams safe_parse_url
#' @return A data.frame with one row per URL and the same fields returned by
#'   \code{\link{safe_parse_url}}. Invalid inputs return NA fields with
#'   \code{parse_status = "error"}.
#' @export
#' @examples
#' safe_parse_urls(c("example.com", "https://www.example.com/path"))
safe_parse_urls <- function(url,
                            protocol_handling = c(
                              "keep", "none", "strip", "http", "https"
                            ),
                            www_handling = c(
                              "none", "strip", "keep", "if_no_subdomain"
                            ),
                            tld_source = c("all", "private", "icann"),
                            case_handling = c(
                              "lower_host", "keep", "lower", "upper"
                            ),
                            trailing_slash_handling = c(
                              "none", "keep", "strip"
                            ),
                            index_page_handling = c("keep", "strip"),
                            path_normalization = c(
                              "none", "collapse_slashes", "dot_segments", "both"
                            ),
                            scheme_relative_handling = c(
                              "keep", "http", "https", "error"
                            ),
                            subdomain_levels_to_keep = NULL,
                            host_encoding = c("keep", "idna", "unicode"),
                            path_encoding = c("keep", "encode", "decode"),
                            query_handling = c(
                              "drop", "filter", "allow", "keep"
                            ),
                            params_keep = NULL,
                            params_drop = NULL,
                            sort_params = FALSE,
                            empty_param_handling = c("keep", "drop"),
                            params_case_sensitive = FALSE,
                            decode_plus = FALSE,
                            port_handling = c(
                              "exclude", "keep", "strip_default", "strip_all"
                            ),
                            scheme_policy = c("infer", "require"),
                            scheme_acceptance = c("web", "general"),
                            url_standard = NULL,
                            engine = NULL,
                            profile = NULL) {
  # url_standard (RURL-eqzkkohm): validate + conflict-check the governed knobs
  # the caller explicitly supplied (see safe_parse_url() for the rationale).
  url_standard <- .validate_url_standard(url_standard)
  profile <- .validate_profile(profile)
  # `path_encoding` is orthogonal (ADR 0011): not passed here, never conflicts.
  # The conflict matrix is skipped on the profile path (see safe_parse_url()).
  if (is.null(profile)) {
    .check_url_standard_conflicts(url_standard, .governed_supplied(
      path_normalization = if (missing(path_normalization)) {
        NULL
      } else {
        match.arg(path_normalization)
      },
      case_handling =
        if (missing(case_handling)) NULL else match.arg(case_handling)
    ))
  }

  # Validate and normalize options once (match.arg + subdomain check)
  po_args <- list(
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = tld_source,
    case_handling = case_handling,
    trailing_slash_handling = trailing_slash_handling,
    index_page_handling = index_page_handling,
    path_normalization = path_normalization,
    scheme_relative_handling = scheme_relative_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding,
    path_encoding = path_encoding,
    query_handling = query_handling,
    params_keep = params_keep,
    params_drop = params_drop,
    sort_params = sort_params,
    empty_param_handling = empty_param_handling,
    params_case_sensitive = params_case_sensitive,
    decode_plus = decode_plus,
    port_handling = port_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard,
    engine = engine
  )
  if (!is.null(profile)) {
    po_args <- .merge_profile_args(po_args, .resolve_profile(profile, list(
      url_standard = url_standard,
      scheme_acceptance =
        if (missing(scheme_acceptance)) NULL else match.arg(scheme_acceptance),
      scheme_policy =
        if (missing(scheme_policy)) NULL else match.arg(scheme_policy),
      scheme_relative_handling = if (missing(scheme_relative_handling)) {
        NULL
      } else {
        match.arg(scheme_relative_handling)
      },
      path_normalization = if (missing(path_normalization)) {
        NULL
      } else {
        match.arg(path_normalization)
      },
      case_handling =
        if (missing(case_handling)) NULL else match.arg(case_handling),
      protocol_handling =
        if (missing(protocol_handling)) NULL else match.arg(protocol_handling),
      www_handling =
        if (missing(www_handling)) NULL else match.arg(www_handling),
      trailing_slash_handling = if (missing(trailing_slash_handling)) {
        NULL
      } else {
        match.arg(trailing_slash_handling)
      },
      index_page_handling = if (missing(index_page_handling)) {
        NULL
      } else {
        match.arg(index_page_handling)
      },
      query_handling =
        if (missing(query_handling)) NULL else match.arg(query_handling)
    )))
  }
  opts <- do.call(.parse_options, po_args)

  # Coerce factors to their labels up front so factor input parses as the
  # character strings it represents rather than falling through to the
  # non-character branch below and yielding an all-error row (canonical_join()
  # applies the same as.character() coercion to its key columns).
  if (is.factor(url)) {
    url <- as.character(url)
  }

  if (length(url) == 0) {
    return(.spu_empty_result())
  }

  # Coerce to a character vector to parse and, separately, the original_url
  # column. A non-character element (or a non-scalar list element) is not
  # parseable -- it becomes NA for the engine (an "error" row), exactly as the
  # scalar pipeline returned NULL for it -- but its original_url is still the
  # coerced input string. This preserves the historical column semantics without
  # a per-URL loop.
  if (is.character(url)) {
    # Fast path: a plain character vector needs no per-element coercion --
    # every element is already a length-1 character (NA included), so both the
    # original_url column and the parse input equal the input verbatim. This is
    # the overwhelmingly common shape and keeps the warm/duplicate path O(1) per
    # element instead of two vapply() closures over the whole vector.
    original_url_vec <- url
    parse_input <- url
  } else {
    urls_list <- as.list(url)
    original_url_vec <- vapply(urls_list, .spu_coerce_original, character(1))
    parse_input <- vapply(
      urls_list,
      function(u) if (is.character(u) && length(u) == 1L) u else NA_character_,
      character(1)
    )
  }

  # De-duplicate + memoize at the unique-URL level: unique parse inputs are
  # parsed once (with cross-call reuse via the full_parse cache), then expanded
  # back to every row. Duplicates cost only the match() inside
  # ._parse_urls_cached(). original_url is restored per row afterwards so it
  # reflects the input element even for duplicates / NA / non-character rows.
  field_names <- vapply(.spu_result_fields, function(f) f$name, character(1))
  cols <- ._parse_urls_cached(parse_input, opts)[field_names]
  cols$original_url <- original_url_vec
  cols$stringsAsFactors <- FALSE
  do.call(data.frame, cols)
}

# Empty (zero-row) result data.frame with the canonical column set/types.
.spu_empty_result <- function() {
  cols <- lapply(.spu_result_fields, function(f) f$template[0])
  names(cols) <- vapply(.spu_result_fields, function(f) f$name, character(1))
  cols$stringsAsFactors <- FALSE
  do.call(data.frame, cols)
}

# Declare (not convert) the parsed host as UTF-8. `curl::curl_parse_url()` hands
# back the host as raw bytes marked "unknown" (native), so under a non-UTF-8
# LC_CTYPE (e.g. LC_ALL=C, which win-builder uses) downstream consumers --
# `pslr` and `punycoder::host_normalize()` -- re-decode those bytes in the
# session locale and disagree with a UTF-8 session: `pslr` returns NA (domain /
# tld / *_ascii / *_unicode all NA) and `host_normalize()` flips the WHATWG
# UTS-46 fatal gate. Marking the bytes UTF-8 makes the whole pipeline
# locale-invariant. It must be `Encoding<-` and NOT `enc2utf8()`: `enc2utf8()`
# *transcodes* from the session locale, which is the locale-sensitive step this
# is fixing. Bytes are untouched; NA elements keep their NA-ness.
.mark_host_utf8 <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  ok <- !is.na(x)
  if (any(ok)) {
    Encoding(x[ok]) <- "UTF-8"
  }
  x
}

# Coerce one input element to the scalar `original_url` value: NA for missing
# or non-scalar inputs, the string itself for character, else as.character().
.spu_coerce_original <- function(u) {
  is_missing_url <- is.null(u) ||
    length(u) == 0 ||
    (is.atomic(u) && length(u) == 1 && is.na(u))
  if (is_missing_url) {
    return(NA_character_)
  }
  if (is.character(u)) {
    return(u)
  }
  if (is.atomic(u) && length(u) == 1) {
    return(as.character(u))
  }
  NA_character_
}

# Validate and normalize the parsing options shared by safe_parse_url() and
# safe_parse_urls(). Runs every match.arg() once and validates
# subdomain_levels_to_keep, returning a list keyed by option name. The formal
# defaults below are the single source of the allowed choices, so match.arg()
# resolves them here rather than in each public function.
# Allowed choices for the parsing options. Hoisted to constants so the
# .parse_options() signature does not need multi-line default vectors;
# match.arg() resolves each formal's default to the matching constant.
.opt_protocol_handling <- c("keep", "none", "strip", "http", "https")
.opt_www_handling <- c("none", "strip", "keep", "if_no_subdomain")
.opt_tld_source <- c("all", "private", "icann")
.opt_case_handling <- c("lower_host", "keep", "lower", "upper")
.opt_trailing_slash_handling <- c("none", "keep", "strip")
.opt_index_page_handling <- c("keep", "strip")
.opt_path_normalization <- c("none", "collapse_slashes", "dot_segments", "both")
.opt_scheme_relative_handling <- c("keep", "http", "https", "error")
# scheme_policy (RURL-vzgeurae): the input-*acceptance* axis for scheme-less
# host-shaped input, orthogonal to protocol_handling (presentation) and
# url_standard (interpretation). "infer" (default) fabricates "http://" for
# scheme-less host-shaped input (today's behavior, byte-for-byte). "require"
# rejects it (strict / pure-parser posture). See .prepare_urls_for_curl_vec().
.opt_scheme_policy <- c("infer", "require")
# scheme_acceptance (ADR 0012 D3): the scheme-*acceptance* axis, orthogonal to
# protocol_handling (presentation) and url_standard (interpretation). "web"
# (default) keeps the curated web-scheme allowlist -- scheme-bearing input
# outside http/https/ftp/ftps/file is an error, now decoupled from
# protocol_handling. "general" admits any syntactically valid scheme token and
# defers interpretation to url_standard (hence D3's composition rule: general
# requires a non-NULL url_standard). "general" is internal-only until the L3/L4
# shapers land (ADR 0012 Layer-2 build-order gate); it is NOT on any public
# signature yet.
.opt_scheme_acceptance <- c("web", "general")
# --- fixup_posture (RURL-jynceqrj, ADR 0012 Layer 6a) -----------------------
# The input-repair axis: "none" (default) never touches the string; "browser"
# runs the bounded, deterministic single-pass string fixer BEFORE the WHATWG
# parser (outer C0/space trim, `;`->`:` and `://` insertion for recognized
# special schemes). This is the fix-up posture that the future public `browser`
# profile will set (ADR 0012 standing rule 2); like `scheme_acceptance =
# "general"`, it is internal-only until the L6b profile lands -- it is NOT on
# any public signature yet. The step-4 fallback `http` prepend is NOT part of
# this fixer: it is the existing `scheme_policy = "infer"` seam, shared with the
# default posture (ADR 0012 D4 -- one prepend impl).
.opt_fixup_posture <- c("none", "browser")
.opt_host_encoding <- c("keep", "idna", "unicode")
.opt_path_encoding <- c("keep", "encode", "decode")
.opt_query_handling <- c("drop", "filter", "allow", "keep")
.opt_empty_param_handling <- c("keep", "drop")
# Standalone, standard-independent editorial knob (PRD v2 D1, RURL-qdlvldts):
# "exclude" (default) is today's only behavior -- port never appears in
# clean_url. "keep" includes the port, subject to url_standard's own
# syntactic default ports. "strip_default" keeps only non-default ports,
# independent of url_standard, and is the spec-style port renderer for clean
# URLs. "strip_all" is an explicit alias of "exclude". Lives alongside
# www_handling/trailing_slash_handling in the cleanup-knob tier -- NOT part of
# .URL_STANDARD_PROFILES.
.opt_port_handling <- c("exclude", "keep", "strip_default", "strip_all")

# --- url_standard selector (RURL-eqzkkohm) -----------------------------------
#
# `url_standard` is the top-level coherent-profile selector: NULL (today's
# behavior, exactly) or one of the two governed standards. It is threaded
# through every public parse/accessor function and stored on `opts` so later
# tickets can consume it in Stage A/B. In THIS ticket (T1, RURL-bbojhnhu) the
# profile->behavior mapping is a deliberate NO-OP: the argument exists,
# validates, and conflict-checks, but path/host output is byte-for-byte
# unchanged. T3/T4/T5 replace the placeholder behavior WITHOUT changing the
# conflict matrix defined below.
.url_standard_choices <- c("rfc3986", "whatwg")

# Final conflict matrix (PRD §5, D3; amended by ADR 0011 / RURL-sjnqhwtl). For
# each GOVERNED knob it records the value the selected profile "would choose".
# `path_identity` is the profile's path IDENTITY-normalization mode (a
# profile-internal sentinel with NO public enum equivalent) -- it is NOT a
# public argument, so it is never conflict-checked. The public presentation
# knob `path_encoding` (keep/encode/decode) is now ORTHOGONAL and un-governed:
# it layers on any profile like `host_encoding` (ADR 0011). `path_normalization`
# resolves dot segments under both profiles, so its value is "dot_segments"
# (v1 profiles do not collapse slashes, so "both" conflicts).
# `case_handling` (PRD v2 §5 D5, RURL-mevmyhxz): both profiles require
# "lower_host" -- it already matches RFC 3986 sec 6.2.2.1 and WHATWG
# scheme/host casing, and lowercases scheme+host only. "lower" also
# lowercases the path (R/parse-phases.R .apply_case_policy_vec), which
# neither spec sanctions, so it conflicts like "keep"/"upper" rather than
# being treated as a superset of "lower_host". No new casing logic -- this
# is a conflict-matrix entry only.
# Host IPv4 behavior is governed too, but it is not exposed as a public knob, so
# it contributes no conflict-checkable argument here.
.URL_STANDARD_PROFILES <- list(
  rfc3986 = list(
    path_identity = ".rfc3986_unreserved",
    path_normalization = "dot_segments",
    case_handling = "lower_host"
  ),
  whatwg = list(
    path_identity = ".whatwg_preserve",
    path_normalization = "dot_segments",
    case_handling = "lower_host"
  )
)

# --- public named profiles (ADR 0012 Layer 6 / D6, RURL-djmgzjmr) -------------
#
# A `profile` is the public sugar that bundles the parser knobs the earlier
# layers built (acceptance + interpretation + leniency + canonicalization) under
# one inspectable name. It is SEPARATE from `url_standard` (merging them is a
# category error): a profile RESOLVES to a bundle of knob->value pairs, expands
# only into slots the caller did not explicitly supply (explicit args override
# the profile -- iron rule), and is inspectable via `url_profile()`.
#
# `.URL_PROFILES` mirrors `.URL_STANDARD_PROFILES` in shape/placement: each
# entry is the resolved knob->value set for that profile. Only knobs the profile
# actually governs appear; everything else flows from the caller's arguments (or
# their defaults). The four bundles keep the "<= 4 profiles" claim true; `seo`
# is primary with `canonical` an ALIAS (one bundle, not a fifth).
#
# `fixup_posture` and `path_identity` are NOT public wrapper formals (ADR 0012
# standing rule 2 / ADR 0011); a profile is the ONLY seam that may set them.
.URL_PROFILES <- list(
  # honest home for the historical http-prepending: browser-*like*, not
  # Chrome-faithful. NO https_upgrade knob (navigation-layer, out of scope).
  browser = list(
    url_standard = "whatwg",
    scheme_acceptance = "general",
    scheme_policy = "infer",
    scheme_relative_handling = "http",
    fixup_posture = "browser"
  ),
  # absolute-URL, no-base spec posture. Deliberately sets scheme_policy =
  # "require", so it REJECTS scheme-less input a direct url_standard = "whatwg"
  # call (default scheme_policy = "infer") would accept (ADR 0012 foot-gun).
  whatwg = list(
    url_standard = "whatwg",
    scheme_acceptance = "general",
    scheme_policy = "require",
    scheme_relative_handling = "error"
  ),
  # Unicode-tolerant RFC-shaped generic syntax: PARSING, not normalization. The
  # path_normalization/case_handling/path_identity entries are AUTHORIZED
  # normalization exceptions (ADR 0012 lines 518-519), not conflicts with
  # ADR 0007 -- they bypass the direct-user conflict matrix on the profile path
  # only and win over the url_standard profile expansion in .parse_options().
  `rfc-syntax` = list(
    url_standard = "rfc3986",
    scheme_acceptance = "general",
    scheme_policy = "require",
    scheme_relative_handling = "keep",
    path_normalization = "none",
    case_handling = "keep",
    path_identity = "none"
  ),
  # rurl's ORIGIN cleaning intent, finally named. scheme_acceptance stays "web"
  # (the default) -- the built-in semantic transforms are HTTP(S)-only, which
  # the existing pipeline already enforces. Maps exactly onto get_clean_url()'s
  # existing knobs; no new cleaning machinery.
  seo = list(
    scheme_acceptance = "web",
    protocol_handling = "https",
    www_handling = "strip",
    trailing_slash_handling = "strip",
    index_page_handling = "strip",
    query_handling = "filter"
  )
)

# Profile aliases: `canonical` resolves identically to `seo` (one bundle, keeps
# the count at 4). This naming is a maintainer decision (RURL-djmgzjmr).
.URL_PROFILE_ALIASES <- list(canonical = "seo")

# Public choice set for the `profile` argument (bundle names + aliases).
.url_profile_choices <- c(names(.URL_PROFILES), names(.URL_PROFILE_ALIASES))

# Choice sets for the knobs a profile bundle may carry, keyed by knob name. Used
# by url_profile() to validate/resolve caller-supplied overrides (the parse
# wrappers validate via .parse_options()). `path_identity` and `fixup_posture`
# are profile-internal / profile-only and are not caller-supplyable here.
.profile_knob_choices <- list(
  url_standard = .url_standard_choices,
  scheme_acceptance = .opt_scheme_acceptance,
  scheme_policy = .opt_scheme_policy,
  scheme_relative_handling = .opt_scheme_relative_handling,
  path_normalization = .opt_path_normalization,
  case_handling = .opt_case_handling,
  protocol_handling = .opt_protocol_handling,
  www_handling = .opt_www_handling,
  trailing_slash_handling = .opt_trailing_slash_handling,
  index_page_handling = .opt_index_page_handling,
  query_handling = .opt_query_handling
)

# Validate `profile`: NULL (default) or one of the allowed profile/alias names.
# Returns the value unchanged (NULL passes through) or errors.
.validate_profile <- function(profile) {
  if (is.null(profile)) {
    return(NULL)
  }
  valid <- is.character(profile) &&
    length(profile) == 1L &&
    !is.na(profile) &&
    profile %in% .url_profile_choices
  if (!valid) {
    stop(
      "profile must be NULL or one of: ",
      toString(.url_profile_choices),
      ".",
      call. = FALSE
    )
  }
  profile
}

# Resolve a profile to its final knob->value bundle. `supplied` is a named list
# of knobs the caller EXPLICITLY supplied (already resolved to canonical
# spellings); for each such knob that the bundle also sets, the caller's value
# OVERRIDES the profile (iron rule) and the resolution is marked `customized`.
# Alias names (canonical) are resolved first. Single source of truth shared by
# the parse wrappers and the url_profile() inspector so they cannot diverge.
.resolve_profile <- function(profile, supplied = list()) {
  name <- profile
  if (!is.null(.URL_PROFILE_ALIASES[[name]])) {
    name <- .URL_PROFILE_ALIASES[[name]]
  }
  bundle <- .URL_PROFILES[[name]]
  supplied <- supplied[!vapply(supplied, is.null, logical(1))]
  overrides <- supplied[intersect(names(supplied), names(bundle))]
  for (knob in names(overrides)) {
    bundle[[knob]] <- overrides[[knob]]
  }
  list(
    profile = name,
    opts = bundle,
    customized = length(overrides) > 0L
  )
}

# Merge a resolved profile bundle into a parse-argument list (the arg list a
# public wrapper passes to .parse_options() or .extract_from_urls()). Bundle
# knobs that are formals of the target overwrite their entry; `fixup_posture`
# (browser) is threaded even though it is not a public wrapper formal; and the
# authorized path exceptions (path_normalization / path_identity) are handed
# over as `profile_authorized` so they SURVIVE the url_standard profile
# expansion inside .parse_options() (localized to the profile path).
.merge_profile_args <- function(args, res) {
  for (knob in intersect(names(res$opts), names(args))) {
    args[[knob]] <- res$opts[[knob]]
  }
  if (!is.null(res$opts$fixup_posture)) {
    args$fixup_posture <- res$opts$fixup_posture
  }
  auth <- res$opts[intersect(
    names(res$opts), c("path_normalization", "path_identity")
  )]
  if (length(auth) > 0L) {
    args$profile_authorized <- auth
  }
  args
}

# Public choice sets for the CONFLICT-CHECKABLE governed knobs, used to resolve
# partial matches (e.g. path_normalization = "dot" -> "dot_segments") before the
# conflict check so a legitimate abbreviated value is not mistaken for a
# conflicting one. `path_identity` is absent: it is profile-internal, not a
# public argument, so it is never supplied and never resolved here.
.url_standard_governed_choices <- list(
  path_normalization = .opt_path_normalization,
  case_handling = .opt_case_handling
)

# Validate `url_standard`: NULL (default) or one of the allowed profile names.
# Returns the value unchanged (NULL passes through) or errors.
.validate_url_standard <- function(url_standard) {
  if (is.null(url_standard)) {
    return(NULL)
  }
  valid <- is.character(url_standard) &&
    length(url_standard) == 1L &&
    !is.na(url_standard) &&
    url_standard %in% .url_standard_choices
  if (!valid) {
    stop(
      "url_standard must be NULL, \"rfc3986\", or \"whatwg\".",
      call. = FALSE
    )
  }
  url_standard
}

# Validate the optional per-request pslr engine (RURL-mhibnqbd). NULL means the
# session-global default (unchanged behavior); otherwise it must be a
# `psl_engine` object from `pslr::psl_engine()`. Returned unchanged so callers
# store it verbatim. NOTE (process-local hazard): a psl_engine holds a C++
# external pointer that does NOT serialize across R sessions or parallel
# workers; it must be built in the process that uses it, never cached to disk
# or sent to a worker.
.validate_engine <- function(engine) {
  if (is.null(engine)) {
    return(NULL)
  }
  if (!inherits(engine, "psl_engine")) {
    stop(
      "engine must be NULL or a `psl_engine` object from ",
      "`pslr::psl_engine()`.",
      call. = FALSE
    )
  }
  engine
}

# Collect the governed knobs a caller EXPLICITLY supplied: each argument is
# either the resolved value or NULL (not supplied). Drops the NULLs, leaving a
# named list keyed by knob name.
.governed_supplied <- function(...) {
  args <- list(...)
  args[!vapply(args, is.null, logical(1))]
}

# Resolve each supplied governed value against its public choice set so partial
# matches compare as their canonical spelling. A value outside the set (or an
# internal profile sentinel) is left untouched so it still registers as a
# conflict.
.resolve_governed_values <- function(supplied) {
  for (knob in names(supplied)) {
    choices <- .url_standard_governed_choices[[knob]]
    if (!is.null(choices)) {
      supplied[[knob]] <- tryCatch(
        match.arg(supplied[[knob]], choices),
        error = function(e) supplied[[knob]]
      )
    }
  }
  supplied
}

# Conflict check (PRD §5, D3): when `url_standard` is set, every governed knob
# the caller EXPLICITLY supplied must equal the value the selected profile would
# choose; a different value errors. `supplied` is a named list of resolved
# governed values (from .governed_supplied()). No-op when url_standard is NULL
# or nothing governed was supplied.
.check_url_standard_conflicts <- function(url_standard, supplied) {
  if (is.null(url_standard) || length(supplied) == 0L) {
    return(invisible(NULL))
  }
  profile <- .URL_STANDARD_PROFILES[[url_standard]]
  for (knob in names(supplied)) {
    required <- profile[[knob]]
    if (is.null(required)) {
      next
    }
    if (!identical(supplied[[knob]], required)) {
      stop(
        sprintf(
          paste0(
            "url_standard = \"%s\" governs `%s`; remove the explicit `%s` ",
            "argument (the profile sets it) or drop url_standard."
          ),
          url_standard, knob, knob
        ),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

# Conflict check across a `...` seam (canonical_join()), where missing() cannot
# tell which knobs the caller supplied. Reads the governed knob names straight
# from the captured dots list, validates url_standard, and applies the same
# conflict rule.
.check_url_standard_conflicts_dots <- function(dots) {
  url_standard <- .validate_url_standard(dots$url_standard)
  if (is.null(url_standard)) {
    return(invisible(NULL))
  }
  governed <- names(.URL_STANDARD_PROFILES[[url_standard]])
  supplied <- .resolve_governed_values(dots[intersect(names(dots), governed)])
  .check_url_standard_conflicts(url_standard, supplied)
}

# Validate a params_keep / params_drop argument: NULL or a character vector of
# glob patterns. Returns it unchanged (NULL passes through) or errors.
.validate_param_patterns <- function(x, arg_name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x)) {
    stop(arg_name, " must be NULL or a character vector.", call. = FALSE)
  }
  x
}

# Validate a scalar logical flag (sort_params / params_case_sensitive /
# decode_plus). Returns it unchanged or errors.
.validate_flag <- function(x, arg_name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(arg_name, " must be a single logical (TRUE or FALSE).", call. = FALSE)
  }
  x
}

.parse_options <- function(protocol_handling = .opt_protocol_handling,
                           www_handling = .opt_www_handling,
                           tld_source = .opt_tld_source,
                           case_handling = .opt_case_handling,
                           trailing_slash_handling =
                             .opt_trailing_slash_handling,
                           index_page_handling = .opt_index_page_handling,
                           path_normalization = .opt_path_normalization,
                           scheme_relative_handling =
                             .opt_scheme_relative_handling,
                           subdomain_levels_to_keep = NULL,
                           host_encoding = .opt_host_encoding,
                           path_encoding = .opt_path_encoding,
                           query_handling = .opt_query_handling,
                           params_keep = NULL,
                           params_drop = NULL,
                           sort_params = FALSE,
                           empty_param_handling = .opt_empty_param_handling,
                           params_case_sensitive = FALSE,
                           decode_plus = FALSE,
                           port_handling = .opt_port_handling,
                           scheme_policy = .opt_scheme_policy,
                           scheme_acceptance = .opt_scheme_acceptance,
                           fixup_posture = .opt_fixup_posture,
                           url_standard = NULL,
                           engine = NULL,
                           profile_authorized = NULL) {
  # match.arg first (matches the original error precedence), then validate
  # subdomain_levels_to_keep and the query options.
  opts <- list(
    protocol_handling = match.arg(protocol_handling),
    www_handling = match.arg(www_handling),
    tld_source = match.arg(tld_source),
    case_handling = match.arg(case_handling),
    trailing_slash_handling = match.arg(trailing_slash_handling),
    index_page_handling = match.arg(index_page_handling),
    path_normalization = match.arg(path_normalization),
    scheme_relative_handling = match.arg(scheme_relative_handling),
    host_encoding = match.arg(host_encoding),
    path_encoding = match.arg(path_encoding),
    # Internal path IDENTITY mode (ADR 0011). "none" unless a url_standard
    # profile sets it below; never a public argument. Kept distinct from the
    # public presentation knob `path_encoding`.
    path_identity = "none",
    query_handling = match.arg(query_handling),
    empty_param_handling = match.arg(empty_param_handling),
    port_handling = match.arg(port_handling),
    scheme_policy = match.arg(scheme_policy),
    scheme_acceptance = match.arg(scheme_acceptance),
    fixup_posture = match.arg(fixup_posture),
    params_keep = .validate_param_patterns(params_keep, "params_keep"),
    params_drop = .validate_param_patterns(params_drop, "params_drop"),
    sort_params = .validate_flag(sort_params, "sort_params"),
    params_case_sensitive = .validate_flag(
      params_case_sensitive, "params_case_sensitive"
    ),
    decode_plus = .validate_flag(decode_plus, "decode_plus")
  )

  valid_subdomain_value <- is.numeric(subdomain_levels_to_keep) &&
    length(subdomain_levels_to_keep) == 1L &&
    !is.na(subdomain_levels_to_keep) &&
    subdomain_levels_to_keep >= 0 &&
    subdomain_levels_to_keep %% 1 == 0
  invalid_subdomain_levels <-
    !is.null(subdomain_levels_to_keep) && !valid_subdomain_value
  if (invalid_subdomain_levels) {
    stop(
      "subdomain_levels_to_keep must be NULL or a non-negative integer.",
      call. = FALSE
    )
  }
  # Single-bracket list() assignment keeps the element when the value is NULL
  # ($<- NULL would drop it).
  opts["subdomain_levels_to_keep"] <- list(subdomain_levels_to_keep)
  # url_standard (RURL-eqzkkohm): validated + stored on opts so later tickets
  # can consume it in Stage A/B. Re-validated defensively; the public wrappers
  # already validate (and run the missing()-based conflict check they alone can
  # see). Single-bracket assignment keeps a NULL element.
  opts["url_standard"] <- list(.validate_url_standard(url_standard))
  # engine (RURL-mhibnqbd): the optional per-request pslr snapshot threaded into
  # the domain/PSL derivation. NULL (the default) means "use pslr's session-
  # global default engine" and keeps every call path byte-identical. Validated
  # + stored with single-bracket assignment so a NULL element is retained. Its
  # identity also enters the Stage-A cache key (.parse_cache_keys) so two calls
  # under different engines never share a memoized domain/TLD.
  opts["engine"] <- list(.validate_engine(engine))
  # Apply the selected profile's governed values (PRD S5, D3). The conflict
  # checkers above (run by the public wrappers, before this function is
  # called) already guarantee any EXPLICIT governed knob equals what the
  # profile requires, so this cannot silently override a caller's conflicting
  # choice -- it only fills in the profile value when the caller left the
  # knob at its default. The profile sets the internal path IDENTITY mode
  # (`path_identity`) -- ".rfc3986_unreserved" (RURL-gjltzwmp) or
  # ".whatwg_preserve" (RURL-bbmuehsx) -- which .normalize_path_vec() applies
  # BEFORE the orthogonal public `path_encoding` presentation step. Presentation
  # is un-governed (ADR 0011 / RURL-sjnqhwtl), so the profile never touches
  # `opts$path_encoding`.
  if (!is.null(opts$url_standard)) {
    profile <- .URL_STANDARD_PROFILES[[opts$url_standard]]
    opts$path_identity <- profile$path_identity
    opts$path_normalization <- profile$path_normalization
  }
  # Profile-authorized normalization exceptions (ADR 0012 D6, lines 518-519):
  # a named profile (e.g. `rfc-syntax`) may authorize keeping the path verbatim
  # even under a url_standard that would normally govern it. These win over the
  # expansion above. Only the profile path passes this argument, so the
  # direct-user conflict matrix (ADR 0007) is untouched for scheme-selector
  # calls -- the localization is that `profile_authorized` is NULL unless a
  # named profile set it.
  if (!is.null(profile_authorized)) {
    for (knob in names(profile_authorized)) {
      opts[[knob]] <- profile_authorized[[knob]]
    }
  }
  # ADR 0012 D3 composition rule: "general" acceptance admits any syntactically
  # valid scheme token but defers *interpretation* to url_standard, so it is
  # meaningless without one. Enforced AFTER profile expansion / explicit
  # override resolution so opts$url_standard is final. "web" composes freely
  # with any url_standard (incl. NULL).
  if (opts$scheme_acceptance == "general" && is.null(opts$url_standard)) {
    stop(
      "scheme_acceptance = \"general\" requires an explicit url_standard ",
      "(\"rfc3986\" or \"whatwg\"); it is NULL.",
      call. = FALSE
    )
  }
  opts
}

# Build Stage A memoization cache keys for a vector of URLs from validated
# options. Stage A is the option-INDEPENDENT parse core (RURL-dkwrebdt), so the
# key covers ONLY the options that actually change that core: the two scheme
# policies that decide what curl parses (protocol_handling,
# scheme_relative_handling), the www policy that shapes the post-www host fed to
# the PSL decomposition (www_handling), and the PSL section (tld_source). Every
# other option (case, trailing slash, index page, path normalization, path/host
# encoding, subdomain levels, and ALL seven query options) is a pure Stage-B
# presentation transform and is deliberately EXCLUDED, so profiles differing
# only in presentation share one cache entry. In particular the query engine
# (query_handling / params_keep / params_drop / sort_params /
# empty_param_handling / params_case_sensitive / decode_plus) reads only the
# cached raw query and shapes clean_url in Stage B, which is recomputed on every
# call -- so there is no memoized clean_url to go stale and the query options
# need not enter the key. (The PRD's self-delimiting cache-key serializer for
# the vector query options predates the parse/present split (RURL-dkwrebdt) that
# made clean_url a never-cached Stage-B output; it is obsolete here.) Single
# source of the field set, order, separator, and Unicode escaping so the key
# format cannot drift across call sites.
# Stable cache-key token for the per-request pslr engine (RURL-mhibnqbd). The
# Stage-A parse core caches the PSL decomposition (domain/TLD in both
# spellings), so an engine that resolves those against a DIFFERENT Public Suffix
# List MUST key a distinct cache entry -- otherwise a second call under another
# engine would reuse the first engine's memoized domain/TLD (a silent
# correctness bug). NULL (the session-global default) returns "" so the
# no-engine key stays byte-identical to the pre-engine format (mirroring how
# `opts$url_standard %||% ""` handles its NULL). A non-NULL engine is keyed by
# its snapshot identity (the sha256 of the PSL list content), which is stable
# across engine objects built from the same list -- so two engines over the
# same list share a cache entry (correct: identical PSL output) while different
# lists never collide. If a future pslr ever drops that field, fall back to the
# matcher's external-pointer address: per-object-unique and stable for the
# object's lifetime, so it can never produce a cross-engine stale hit (only
# less cross-object sharing).
.engine_cache_token <- function(engine) {
  if (is.null(engine)) {
    return("")
  }
  id <- tryCatch(engine[["snapshot"]][["identity"]], error = function(e) NULL)
  if (is.character(id) && length(id) == 1L && !is.na(id) && nzchar(id)) {
    return(id)
  }
  paste0(
    "engine-ptr:",
    paste(utils::capture.output(print(engine[["matcher"]])), collapse = "")
  )
}

.parse_cache_keys <- function(urls, opts) {
  # url_standard is Stage-A-affecting (RURL-luwvkwhd, PRD §5.1): it changes IP
  # detection / host rejection / final host in Stage A, so it MUST enter the key
  # or a second call under a different standard would return a stale cached host
  # (AC #9). Option (a) from the PRD: one extra cache entry per URL per standard
  # actually used. NULL maps to "" so the no-selector key is stable.
  # scheme_policy is Stage-A-affecting (RURL-vzgeurae): under "require" it folds
  # scheme-less host-shaped rows into the reject set, changing what curl parses
  # (and whether a row survives at all). So it MUST enter the key, or a second
  # call under a different policy would reuse a stale cached row.
  # scheme_acceptance is Stage-A-affecting (ADR 0012 D3): under "web" a
  # scheme-bearing input outside the curated allowlist is folded into the
  # reject set in Phase 1, whereas "general" admits it. So it changes which
  # rows survive Stage A and MUST enter the key, or a second call under a
  # different acceptance would reuse a stale cached row.
  # fixup_posture is Stage-A-affecting (RURL-jynceqrj, ADR 0012 Layer 6a):
  # under "browser" the bounded string fixer rewrites the input (outer trim,
  # `;`->`:`, `://` insertion) BEFORE curl sees it, so a fixed parse produces a
  # different curl input than the unfixed one. It MUST enter the key, or a
  # fixed parse would collide with an unfixed cached row (PRD Part 1).
  # engine is Stage-A-affecting (RURL-mhibnqbd): it resolves the memoized PSL
  # decomposition against a specific list, so it MUST enter the key or a second
  # call under a different engine would reuse a stale cached domain/TLD. NULL ->
  # "" keeps the no-engine key byte-identical (see .engine_cache_token()).
  cache_key <- paste(urls, opts$protocol_handling, opts$www_handling,
    opts$tld_source, opts$scheme_relative_handling,
    opts$url_standard %||% "", opts$scheme_policy, opts$scheme_acceptance,
    opts$fixup_posture, .engine_cache_token(opts$engine),
    sep = "\x1F"
  )
  stringi::stri_escape_unicode(enc2utf8(cache_key))
}

# Internal scalar helper that handles caching and calls the shared cached
# vector path (n = 1). Receives the validated `opts` list (from
# .parse_options()) and reuses it directly. Returns NULL for a NULL-equivalent
# row (invalid / rejected / curl failure) or the row as a named list
# (byte-identical to the historical .assemble_parse_result() output).
._safe_parse_url_scalar <- function(url, opts) {
  # Early return for invalid input (never cached, matching the historical
  # scalar pipeline that returned NULL for these before touching the cache).
  if (is.na(url) || !is.character(url) || url == "") {
    return(NULL)
  }

  # Scalar parse is the n = 1 case of the shared cached vector path, so the
  # per-URL cache behavior comes from the same code as safe_parse_urls().
  cols <- ._parse_urls_cached(url, opts)
  if (attr(cols, "null_row")[1L]) {
    return(NULL)
  }
  field_names <- vapply(.spu_result_fields, function(f) f$name, character(1))
  lapply(cols[field_names], function(column) column[[1L]])
}

# Shared cached vector parse path for safe_parse_url() (n = 1) and
# safe_parse_urls(). `parse_input` is a character vector (NA marks elements the
# caller could not coerce to a parseable string). Returns the list of 14 columns
# named by .spu_result_fields (canonical order) plus a `null_row` logical
# attribute -- the rows the scalar pipeline would have returned NULL for.
#
# Parse/present split (RURL-dkwrebdt): the full_parse cache stores STAGE A --
# the option-independent parse core (curl components, IP detection, post-www
# host, and the PSL decomposition in both spellings) -- keyed by
# url x protocol x www x tld_source x scheme_relative only. STAGE B
# (._parse_stage_b_vec) then derives the 14 presented columns from the cached
# Stage A plus the presentation options and is NEVER cached. So two accessor
# profiles that differ only in presentation (case, path handling, host_encoding
# spelling, subdomain levels, ...) share one cache entry and re-run only the
# cheap Stage B: the expensive curl + PSL work is done once per URL regardless
# of how many option profiles ask for it.
#
# Cache mechanics mirror the previous full-result cache: keyed/stored at the
# UNIQUE-URL level (one batch lookup, misses computed once, all rows expanded
# via match()); each value is either an unnamed Stage A field list
# (.spu_stage_a_fields order) or NULL for a null row, so NULL-caching semantics
# hold and hit reconstruction is a builtin `[[` gather. NA / "" inputs are null
# rows and are NEVER cached, so cache-entry counts still reflect real unique
# URLs only.
._parse_urls_cached <- function(parse_input, opts) {
  a_fields <- vapply(.spu_stage_a_fields, function(f) f$name, character(1))
  n_a_fields <- length(.spu_stage_a_fields)
  uniq <- unique(parse_input)
  m <- length(uniq)

  # Only a real URL string (non-NA, non-empty) participates in the cache.
  cacheable <- !is.na(uniq) & nzchar(uniq)
  keys <- rep(NA_character_, m)
  if (any(cacheable)) {
    keys[cacheable] <- .parse_cache_keys(uniq[cacheable], opts)
  }

  # Batch cache lookup over the cacheable keys.
  cached <- rep(list(.rurl_cache_sentinel), m)
  if (any(cacheable)) {
    cached[cacheable] <- .cache_get_many("full_parse", keys[cacheable])
  }
  hit <- !vapply(cached, identical, logical(1), .rurl_cache_sentinel)

  # Unique-level Stage A columns, initialized to the null-row defaults.
  a_uniq <- lapply(.spu_stage_a_fields, function(f) rep(f$default, m))
  names(a_uniq) <- a_fields
  null_uniq <- rep(TRUE, m)

  # Fill cache hits. A hit value is an unnamed Stage A field list (populated
  # row) or NULL (cached null row -- keep the defaults). Non-null rows are
  # gathered a field at a time with the builtin `[[`, so reconstruction avoids a
  # closure per hit.
  if (any(hit)) {
    hit_idx <- which(hit)
    is_null_hit <- vapply(cached[hit_idx], is.null, logical(1))
    null_uniq[hit_idx] <- is_null_hit
    nn_idx <- hit_idx[!is_null_hit]
    if (length(nn_idx) > 0L) {
      nn_vals <- cached[nn_idx]
      for (j in seq_len(n_a_fields)) {
        a_uniq[[j]][nn_idx] <- vapply(
          nn_vals, `[[`, .spu_stage_a_fields[[j]]$template, j
        )
      }
    }
  }

  # Compute Stage A for the misses (includes NA / "" inputs, which the engine
  # turns into null rows) and populate their columns.
  need_idx <- which(!hit)
  if (length(need_idx) > 0L) {
    a_cols <- ._parse_stage_a_vec(uniq[need_idx], opts)
    null_uniq[need_idx] <- attr(a_cols, "null_row")
    for (nm in a_fields) {
      a_uniq[[nm]][need_idx] <- a_cols[[nm]]
    }

    # Store the freshly computed, cacheable Stage A rows: the unnamed field
    # list, or NULL for a null row. NA / "" are never cached.
    store_idx <- need_idx[cacheable[need_idx]]
    if (length(store_idx) > 0L) {
      store_vals <- lapply(store_idx, function(i) {
        if (null_uniq[i]) {
          NULL
        } else {
          unname(lapply(a_uniq, `[[`, i))
        }
      })
      .cache_set_many("full_parse", keys[store_idx], store_vals)
    }
  }

  # Stage B: derive the 14 presented columns from the unique Stage A rows plus
  # the presentation options. Runs on every call (never cached); curl_ok is the
  # complement of the null rows.
  cols_uniq <- ._parse_stage_b_vec(a_uniq, uniq, !null_uniq, opts)

  # Null rows collapse to the all-default "error" row (original_url is left as
  # the unique input and restored per row by the caller). This matches the tail
  # of the previous single-stage engine.
  if (any(null_uniq)) {
    for (f in .spu_result_fields) {
      if (f$name == "original_url") {
        next
      }
      cols_uniq[[f$name]][null_uniq] <- f$default
    }
  }

  # Expand the unique rows back to every input position.
  pos <- match(parse_input, uniq)
  field_names <- vapply(.spu_result_fields, function(f) f$name, character(1))
  cols <- lapply(cols_uniq, function(col) col[pos])
  names(cols) <- field_names
  attr(cols, "null_row") <- null_uniq[pos]
  cols
}

# Stage A (vector): the option-INDEPENDENT parse core (RURL-dkwrebdt). Runs the
# expensive, presentation-independent phases -- scheme detection + curl input
# prep (1), the curl parse and raw-component extraction (2), final-scheme policy
# (4), IP detection (5), the www-prefix policy that shapes the host (6), and the
# registered-domain / TLD derivation (7) computed in BOTH the ascii and unicode
# spellings so host_encoding stays a Stage-B choice. Returns the Stage A columns
# named by .spu_stage_a_fields plus a `null_row` attribute (invalid input,
# phase-1 rejection, or curl failure). Its output for a given URL depends only
# on opts$protocol_handling / www_handling / tld_source /
# scheme_relative_handling (the full_parse cache key), so it is memoized once
# and reused across every presentation profile.
._parse_stage_a_vec <- function(urls, opts) {
  n <- length(urls)

  # Input-level validity: parseable only if a non-NA, non-empty character
  # scalar (callers map non-character input to NA before the engine).
  valid <- !is.na(urls) & nzchar(urls)

  # Phase 1: scheme detection and curl-input preparation. url_standard is passed
  # so the host-shape gate parses numeric IPv4 attempts faithfully instead of
  # rejecting them under a selector (RURL-luwvkwhd).
  prep <- .prepare_urls_for_curl_vec(
    urls, opts$protocol_handling, opts$scheme_relative_handling,
    opts$url_standard, opts$scheme_policy, opts$scheme_acceptance,
    opts$fixup_posture
  )

  # ADR 0012 Layer 4b-2 (RURL-qbnelzku): general-acceptance routing. Under
  # scheme_acceptance == "general", `.general_parse_vec` decomposes the
  # opaque / non-special-authority / RFC-generic / RFC-8089-file rows the
  # posture routes OUT of libcurl. A pure no-op under "web" (empty mask, NA
  # columns), so `general_route` is empty and every libcurl-path vector below
  # stays bit-identical -- byte-identity by construction. These rows are cut
  # from curl_parseable and their components installed directly (mirroring the
  # whatwg_file / rfc3986_path_rootless blocks). Rows the gate/parser rejects
  # (gen$ok = FALSE) simply never join parse_ok, so they present as errors.
  gen <- .general_parse_vec(urls, opts$url_standard, opts$scheme_acceptance)
  general_route <- valid & gen$general_parsed

  # Phase 2: parse with curl (the only per-URL loop) over the surviving rows.
  parseable <- valid & !prep$rejected
  rfc3986_path_rootless <- parseable & prep$rfc3986_path_rootless
  whatwg_file <- parseable & prep$whatwg_file
  curl_parseable <- parseable & !rfc3986_path_rootless & !whatwg_file &
    !general_route
  parsed_list <- vector("list", n)
  parse_idx <- which(curl_parseable)
  if (length(parse_idx) > 0L) {
    parsed_list[parse_idx] <- lapply(
      prep$url_to_parse[parse_idx], .parse_with_curl
    )
  }
  curl_ok <- curl_parseable & !vapply(parsed_list, is.null, logical(1))
  parsed_from_pqf_fallback <- rep(FALSE, n)
  fallback_idx <- which(
    curl_parseable & !curl_ok & prep$whatwg_pqf_url != prep$url_to_parse
  )
  if (length(fallback_idx) > 0L) {
    parsed_list[fallback_idx] <- lapply(
      prep$whatwg_pqf_url[fallback_idx], .parse_with_curl
    )
    fallback_ok <- !vapply(parsed_list[fallback_idx], is.null, logical(1))
    parsed_from_pqf_fallback[fallback_idx[fallback_ok]] <- TRUE
    curl_ok[fallback_idx[fallback_ok]] <- TRUE
  }
  file_parse <- .parse_whatwg_file_urls_vec(
    prep$whatwg_file_input[whatwg_file], prep$backslash_rewritten[whatwg_file]
  )
  file_ok <- rep(FALSE, n)
  file_ok[whatwg_file] <- file_parse$ok
  # General-acceptance rows that parsed successfully (incl. the RFC gate).
  general_ok <- general_route & gen$ok
  parse_ok <- curl_ok | rfc3986_path_rootless | file_ok | general_ok
  null_row <- !parse_ok

  # Pull raw components into columns (mirrors .extract_raw_components() and the
  # port/fragment/user/password fields of .assemble_parse_result(), using `$`
  # exactly as the scalar code does).
  raw_scheme <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$scheme %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_host <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$host %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  # Host shim restore (RURL-dxwxeamq / RURL-rgjpcbuk). For rows Phase 1
  # sanitized so curl could parse the structure, curl's `$host` is a
  # placeholder; overwrite it with the profile-correct host BEFORE IP detection
  # and the host model, so every downstream gate validates the real host.
  restore <- curl_ok & prep$restore_host_shimmed
  if (any(restore)) {
    raw_host[restore] <- prep$shimmed_true_host[restore]
  }
  # Path is re-derived from the prepared input (not curl's normalized $path) so
  # dot segments survive to path_normalization; see .extract_raw_path_vec().
  curl_path <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$path %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_path <- curl_path
  if (any(curl_ok)) {
    prepared_for_components <- prep$url_to_parse
    prepared_for_components[parsed_from_pqf_fallback] <-
      prep$whatwg_pqf_url[parsed_from_pqf_fallback]
    raw_path[curl_ok] <- .extract_raw_path_vec(
      prepared_for_components[curl_ok], curl_path[curl_ok]
    )
  }
  # query/fragment/userinfo are raw pass-throughs; .blank_to_na() maps a
  # present-but-empty "" component to NA so output is stable across libcurl
  # versions (see .blank_to_na in utils.R).
  raw_query <- .blank_to_na(vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$query %||% NA_character_
  }, character(1), USE.NAMES = FALSE))
  raw_fragment <- .blank_to_na(vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$fragment %||% NA_character_
  }, character(1), USE.NAMES = FALSE))
  raw_user <- .blank_to_na(vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$user %||% NA_character_
  }, character(1), USE.NAMES = FALSE))
  raw_password <- .blank_to_na(vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$password %||% NA_character_
  }, character(1), USE.NAMES = FALSE))
  raw_port <- vapply(parsed_list, function(p) {
    if (is.null(p)) {
      NA_integer_
    } else {
      suppressWarnings(as.integer(p$port %||% NA_integer_))
    }
  }, integer(1), USE.NAMES = FALSE)

  if (any(whatwg_file)) {
    raw_scheme[whatwg_file] <- "file"
    raw_host[whatwg_file] <- file_parse$host
    raw_path[whatwg_file] <- file_parse$path
    raw_query[whatwg_file] <- file_parse$query
    raw_fragment[whatwg_file] <- file_parse$fragment
    raw_user[whatwg_file] <- NA_character_
    raw_password[whatwg_file] <- NA_character_
    raw_port[whatwg_file] <- NA_integer_
  }

  if (any(rfc3986_path_rootless)) {
    raw_scheme[rfc3986_path_rootless] <-
      prep$rfc3986_path_rootless_scheme[rfc3986_path_rootless]
    raw_host[rfc3986_path_rootless] <- NA_character_
    raw_path[rfc3986_path_rootless] <-
      prep$rfc3986_path_rootless_path[rfc3986_path_rootless]
    raw_query[rfc3986_path_rootless] <-
      prep$rfc3986_path_rootless_query[rfc3986_path_rootless]
    raw_fragment[rfc3986_path_rootless] <-
      prep$rfc3986_path_rootless_fragment[rfc3986_path_rootless]
  }

  # ADR 0012 Layer 4b-2: install the general-routed components. host is the
  # posture host verbatim (opaque UTF-8 %-encoded / RFC source-preserving,
  # never punycode -- ADR 0002); query/fragment go through .blank_to_na so the
  # public columns keep the empty->NA contract (the empty-vs-absent distinction
  # is recovered in Stage B via `.general_parse_vec` for serialization).
  if (any(general_ok)) {
    raw_scheme[general_ok] <- gen$scheme[general_ok]
    raw_host[general_ok] <- gen$host[general_ok]
    raw_path[general_ok] <- gen$path[general_ok]
    raw_query[general_ok] <- .blank_to_na(gen$query[general_ok])
    raw_fragment[general_ok] <- .blank_to_na(gen$fragment[general_ok])
    # userinfo is surfaced only by the RFC 8089 `file:` overlay, which has a
    # production for it (App. E.1/F); every other general-routed row leaves
    # `gen$userinfo` NA, so their output is unchanged (RURL-obsweger). Password
    # stays NA: RFC 8089's production is `[ userinfo "@" ]` undivided, and
    # App. E.1 warns that a password there is "a serious security exposure",
    # so rurl does not manufacture a credentials split the RFC never draws.
    raw_user[general_ok] <- .blank_to_na(gen$userinfo[general_ok])
    raw_password[general_ok] <- NA_character_
    raw_port[general_ok] <- suppressWarnings(as.integer(gen$port[general_ok]))
  }

  # ADR 0012 D7: a mailto: URL carries recipient addr-specs, not a URL
  # authority. Expose the FIRST recipient's domain as `host` (so get_host /
  # get_domain / get_tld / get_subdomain decompose it exactly like a web host)
  # and its local-part as `user`. Extraction metadata ONLY: Stage B serializes
  # the general rows from its own gen_b re-parse (host NA for mailto), so
  # clean_url / round-trip is untouched. Domain-form RHS only; address-literal /
  # invalid -> NA host.
  is_mailto_gen <- general_ok & !is.na(raw_scheme) &
    .ascii_tolower(raw_scheme) == "mailto"
  if (any(is_mailto_gen)) {
    rp <- .mailto_first_recipient_parts(raw_path[is_mailto_gen])
    raw_host[is_mailto_gen] <- rp$host
    raw_user[is_mailto_gen] <- rp$user
  }

  # Every host source (curl, shim restore, `file:`, the general/opaque parser,
  # mailto) has merged into `raw_host` by here, so this is the single vector-
  # path chokepoint at which the host's encoding can be declared once for all
  # downstream consumers (`pslr`, `punycoder::host_normalize()`).
  raw_host <- .mark_host_utf8(raw_host)

  # Phase 4: final scheme per protocol policy.
  final_scheme <- .derive_final_scheme_vec(
    opts$protocol_handling, prep$looks_like_protocol, raw_scheme
  )

  # Phase 5: IP host detection (on curl's host, which may be a coerced IPv4).
  is_ip_host <- .detect_ip_host_vec(raw_host)
  is_ip_pre_model <- is_ip_host

  # Phase 5b: url_standard host IPv4/reg-name model (RURL-luwvkwhd). No-op when
  # url_standard is NULL. Under a selector it restores the RFC reg-name spelling
  # (or keeps curl's WHATWG coercion) and marks WHATWG-fatal numeric hosts,
  # which are folded into the null-row set so they present as parse errors.
  model <- .apply_host_standard_model_vec(
    prep$input_host, raw_host, is_ip_host, opts$url_standard,
    prep$is_ipv4_attempt
  )
  model_host <- model$host
  is_ip_host <- model$is_ip
  # The libcurl host model must never touch a general-routed host (parsed by
  # the posture opaque/RFC parser, not libcurl): restore its host + IP verdict
  # and exempt it from the model's WHATWG-fatal reject.
  general_acceptance <- identical(opts$scheme_acceptance, "general")
  if (general_acceptance && any(general_route)) {
    model_host[general_route] <- raw_host[general_route]
    is_ip_host[general_route] <- is_ip_pre_model[general_route]
    null_row <- null_row | (model$fatal & !general_route)
  } else {
    null_row <- null_row | model$fatal
  }

  # Phase 6: www-prefix policy shapes the host fed to the PSL decomposition.
  final_host <- .apply_www_policy_vec(
    model_host, opts$www_handling, is_ip_host, opts$engine
  )
  # ADR 0012 D2 (the one edit inside the cached Stage-A path): a general
  # non-special / opaque / reg-name / empty host does NOT get www-stripping.
  # Empty mask under "web", so a strict no-op for every web/special row.
  if (general_acceptance && any(general_route)) {
    final_host[general_route] <- model_host[general_route]
  }

  # Phase 7: registered-domain and TLD derivation (batched pslr calls), computed
  # in BOTH spellings. "idna" forces ascii A-labels and "unicode" forces
  # decoded Unicode for every eligible host, so Stage B can reproduce every
  # host_encoding spelling (keep / idna / unicode) by SELECTING between the two
  # columns per row -- no further pslr call. host_is_ace drives the per-host
  # choice for the "keep" spelling, mirroring .derive_domain_tld_vec()'s own
  # logic exactly.
  # ADR 0012 D2: a general non-special / opaque / reg-name / empty host is NOT
  # asserted to be a DNS name -- force its PSL input to NA so domain/tld resolve
  # to NA and the host never enters the pslr / punycode helpers (ADR 0002).
  # `psl_host` masks ONLY the general-routed rows; `final_host` (the public host
  # column) keeps the opaque host verbatim. Empty mask under "web".
  psl_host <- final_host
  if (general_acceptance && any(general_route)) {
    psl_host[general_route] <- NA_character_
    # ADR 0012 D7 carve-out of D2: a mailto recipient domain IS a domain, so it
    # DOES flow through the PSL decomposition (unlike an opaque general host).
    if (any(is_mailto_gen)) {
      keep <- is_mailto_gen & !is.na(final_host)
      psl_host[keep] <- final_host[keep]
    }
  }
  dt_ascii <- .derive_domain_tld_vec(
    psl_host, is_ip_host, opts$tld_source, "idna", opts$engine
  )
  dt_unicode <- .derive_domain_tld_vec(
    psl_host, is_ip_host, opts$tld_source, "unicode", opts$engine
  )
  host_is_ace <- .host_is_ace_vec(psl_host)

  cols <- list(
    final_scheme = final_scheme,
    final_host = final_host,
    is_ip_host = is_ip_host,
    raw_path = raw_path,
    raw_query = raw_query,
    raw_fragment = raw_fragment,
    raw_user = raw_user,
    raw_password = raw_password,
    raw_port = raw_port,
    domain_ascii = dt_ascii$domain,
    domain_unicode = dt_unicode$domain,
    tld_ascii = dt_ascii$tld,
    tld_unicode = dt_unicode$tld,
    host_is_ace = host_is_ace,
    looks_like_protocol = prep$looks_like_protocol,
    original_has_allowed_scheme = prep$original_has_allowed_scheme,
    is_scheme_relative = prep$is_scheme_relative,
    looks_like_host_port = prep$looks_like_host_port,
    scheme_less_userinfo = prep$scheme_less_userinfo,
    rfc3986_path_rootless = rfc3986_path_rootless,
    # Original (pre-curl) host token: NOT a cached Stage-A field (absent from
    # .spu_stage_a_fields) and never surfaced in the 14-column result, so it
    # widens no public output. The url_standard metadata seam
    # (.derive_url_metadata_vec) reads it off the direct ._parse_stage_a_vec()
    # call to compute shape-keyed host diagnostics (RURL-luwvkwhd / T2 seam).
    input_host = prep$input_host,
    # WHATWG backslash recognition (RURL-ledntyab): NOT a cached Stage-A field
    # either, read the same way by the metadata seam to emit
    # `invalid-reverse-solidus` only where a rewrite actually happened.
    backslash_rewritten = prep$backslash_rewritten,
    # WHATWG control-char strip (RURL-tyetpjym): same seam, emits
    # `control-char-stripped` only where a tab/LF/CR was actually removed.
    control_char_stripped = prep$control_char_stripped,
    # WHATWG host-charset shim (RURL-dxwxeamq, ADR 0009): same seam, emits
    # `host-charset-shimmed` where a curl-rejected-but-WHATWG-valid host code
    # point was accepted via the shim + true-host restore above.
    host_charset_shimmed = prep$host_charset_shimmed
  )
  attr(cols, "null_row") <- null_row
  cols
}

# Stage B (vector): the presentation transform (RURL-dkwrebdt). A pure,
# never-cached function of the cached Stage A columns (`a`), the per-row
# original URL, `curl_ok` (the complement of Stage A's null rows), and the
# validated presentation options. Runs the option-dependent phases -- path
# normalization (3), subdomain-level trimming (8), host encoding (9), case
# policy (10), clean-URL assembly (11), parse-status (12), and result assembly
# (13) -- selecting the domain/TLD spelling from Stage A's two spellings.
# Returns the 14 columns named by .spu_result_fields (null-row error defaults
# are applied by the caller). Every option EXCLUDED from the cache key is
# consumed here, so switching presentation profiles re-runs only this cheap
# stage.
._parse_stage_b_vec <- function(a, original_url, curl_ok, opts) {
  is_ip_host <- a$is_ip_host

  # Select the domain/TLD spelling exactly as .derive_domain_tld_vec() would for
  # this host_encoding: idna -> ascii, unicode -> unicode, keep -> ascii for
  # ACE (xn--) hosts else unicode. domain/tld emptiness (used by parse-status)
  # is spelling-independent, so status is unaffected by the choice.
  use_ascii <- if (opts$host_encoding == "idna") {
    rep(TRUE, length(a$host_is_ace))
  } else if (opts$host_encoding == "unicode") {
    rep(FALSE, length(a$host_is_ace))
  } else {
    a$host_is_ace
  }
  domain <- ifelse(use_ascii, a$domain_ascii, a$domain_unicode)
  tld <- ifelse(use_ascii, a$tld_ascii, a$tld_unicode)

  # ADR 0012 D2 / Layer 3c (RURL-jqlnnaiw): per-row Stage-B eligibility masks.
  # The ENTIRE restriction is gated on `scheme_acceptance == "general"`; under
  # "web" (the default and only publicly reachable value) `.stage_b_eligibility`
  # returns all-TRUE masks, so every revert mask below is EMPTY and every
  # transform runs exactly as today -- byte-identity is BY CONSTRUCTION. We only
  # compute the masks (and pay the classifier cost) under "general" so the web
  # hot path is untouched; the predicate itself still returns all-TRUE for any
  # non-"general" value (unit-tested) as the source of truth.
  #
  # DEFERRAL: www_handling (Stage A, Phase 6) and PSL domain/TLD derivation
  # (Stage A) are NOT gated here -- Stage-A host handling for `general` is L4b's
  # slice. This block gates only Stage-B path/host/query transforms.
  general_acceptance <- identical(opts$scheme_acceptance, "general")
  if (general_acceptance) {
    scheme_lc <- .ascii_tolower(a$final_scheme)
    is_special_row <- !is.na(scheme_lc) & scheme_lc %in% .WHATWG_SPECIAL_SCHEMES
    # ADR 0012 Layer 4b-2 (RURL-qbnelzku): re-run the pure general parser on the
    # original URL to recover the state kinds Stage A did not thread through the
    # cache (the cache stores only .spu_stage_a_fields). `gp` = the rows Stage A
    # routed to the posture parser AND that parsed ok (curl_ok is the complement
    # of Stage A's null rows). Cheap and deterministic; only the general posture
    # pays for it.
    gen_b <- .general_parse_vec(
      original_url, opts$url_standard, opts$scheme_acceptance
    )
    gp <- gen_b$general_parsed & curl_ok
    # path_kind / host_kind for eligibility: the L3a classifier proxy for the
    # libcurl (special) rows, the TRUE parser kinds for the general-routed rows.
    pk <- .whatwg_path_kind(is_special_row, a$raw_path)
    hk <- .host_kind(a$final_host)
    if (any(gp)) {
      pk[gp] <- gen_b$path_kind[gp]
      pk[gp & is.na(pk)] <- "list" # rfc posture: NA path_kind -> non-opaque
      hk[gp] <- gen_b$host_kind[gp]
    }
    elig <- .stage_b_eligibility(
      "general", a$final_scheme, opts$url_standard,
      path_kind = pk, host_kind = hk, is_ip_host = is_ip_host
    )
  }

  # Phase 3: path normalization.
  path_final <- .normalize_path_vec(
    a$raw_path, opts$path_encoding, opts$path_normalization,
    opts$index_page_handling, opts$trailing_slash_handling,
    path_identity = opts$path_identity
  )
  # D2 "Hierarchical path transforms": revert ineligible (opaque-path) rows to
  # the pre-normalization baseline (a$raw_path). Presentation via path_encoding
  # for opaque paths is ADR 0011's separate escape hatch (deferred to L3b/L4b);
  # L3c only skips the hierarchical transforms here.
  if (general_acceptance) {
    revert <- !elig$path_eligible
    path_final[revert] <- a$raw_path[revert]
  }

  # Phase 8: subdomain-level policy (may trim labels from the host).
  final_host <- .apply_subdomain_policy_vec(
    a$final_host, domain, opts$subdomain_levels_to_keep, is_ip_host, opts$engine
  )
  # D2 "Host presentation" / "DNS/PSL derivation": revert host-transform for
  # ineligible (opaque/IP/empty/non-domain) hosts to the pre-trim host.
  if (general_acceptance) {
    revert <- !elig$host_transform_eligible
    final_host[revert] <- a$final_host[revert]
  }

  # Phase 9: host encoding (idna / unicode).
  host_for_clean <- .apply_host_encoding_vec(
    final_host, opts$host_encoding, is_ip_host, opts$url_standard
  )
  # D2 "Host presentation (host_encoding)": revert ineligible hosts to the
  # pre-encoding host (post-subdomain `final_host`).
  if (general_acceptance) {
    revert <- !elig$host_transform_eligible
    host_for_clean[revert] <- final_host[revert]
  }

  # Phase 10: case policy applied to host, path, and scheme.
  cased <- .apply_case_policy_vec(
    host_for_clean, path_final, a$final_scheme, opts$case_handling
  )
  # D2 case split: SCHEME lowercasing is standards-required and ALWAYS applied
  # (cased$scheme kept). The host case fold is gated on host_transform_eligible
  # (a non-special opaque host is not lowercased); the path case fold is gated
  # on path_eligible (an opaque path is not folded). Revert the ineligible rows
  # to their pre-case baselines.
  if (general_acceptance) {
    cased$host[!elig$host_transform_eligible] <-
      host_for_clean[!elig$host_transform_eligible]
    cased$path[!elig$path_eligible] <- path_final[!elig$path_eligible]
  }

  # Query filtering: derive the canonical query to append to clean_url per
  # query_handling (default "drop" => "" for every row, i.e. the historical
  # query-free clean_url). Fed from the raw query (Stage A) and the query
  # options only; deliberately EXEMPT from case_handling (query values are
  # case-sensitive -- see .apply_case_policy_vec, which folds scheme/host/path
  # only), so it is computed here and appended AFTER the cased components.
  clean_query <- .filter_query_vec(a$raw_query, opts)
  # D2 "Automatic semantic transforms" (query filter/sort): HTTP(S) only under
  # `general`. Skip the transform for ineligible rows -- pass the raw query
  # through unchanged (NA -> "" to match .filter_query_vec's absent-query shape)
  # rather than applying the filter/sort/drop policy. This is the
  # `transform-skipped-ineligible-scheme` case L5 will surface.
  if (general_acceptance) {
    skip <- !elig$semantic_transform_eligible
    if (any(skip)) {
      rq <- a$raw_query[skip]
      rq[is.na(rq)] <- ""
      clean_query[skip] <- rq
    }
  }

  query_output <- a$raw_query
  fragment_output <- a$raw_fragment
  port_output <- .apply_port_output_policy_vec(
    a$final_scheme, a$raw_port, opts$port_handling, opts$url_standard
  )
  if (.is_whatwg(opts$url_standard)) {
    q_idx <- which(!is.na(query_output))
    if (length(q_idx) > 0L) {
      query_output[q_idx] <- mapply(
        .whatwg_query_percent_encode,
        query_output[q_idx],
        a$final_scheme[q_idx],
        USE.NAMES = FALSE
      )
    }
    f_idx <- which(!is.na(fragment_output))
    if (length(f_idx) > 0L) {
      fragment_output[f_idx] <- vapply(
        fragment_output[f_idx], .whatwg_fragment_percent_encode, character(1),
        USE.NAMES = FALSE
      )
    }
  }

  # Phase 11: clean URL reconstruction (with the filtered query appended).
  clean_url <- .build_clean_url_vec(
    cased$scheme, cased$host, cased$path, opts$trailing_slash_handling,
    query = clean_query, port = a$raw_port, port_handling = opts$port_handling,
    url_standard = opts$url_standard
  )
  # ADR 0012 Layer 4b-2 (RURL-qbnelzku): serializer dispatch. Under general the
  # posture-serialized clean_url REPLACES the special/host-present builder for
  # the general-routed rows -- WHATWG (opaque/list, null-vs-empty host, `/.`
  # guard) or RFC generic (source-preserving). web/special rows keep
  # `.build_clean_url_vec` verbatim; under "web" `gp` is empty (passthrough).
  if (general_acceptance && any(gp)) {
    if (.is_whatwg(opts$url_standard)) {
      clean_url[gp] <- .serialize_whatwg_vec(
        scheme = cased$scheme[gp], host = gen_b$host[gp],
        host_kind = gen_b$host_kind[gp], path = gen_b$path[gp],
        path_kind = gen_b$path_kind[gp], query = gen_b$query[gp],
        query_kind = gen_b$query_kind[gp], port = gen_b$port[gp],
        port_handling = opts$port_handling,
        trailing_slash_handling = opts$trailing_slash_handling
      )
    } else {
      clean_url[gp] <- .serialize_rfc_generic_vec(
        scheme = cased$scheme[gp], host = gen_b$host[gp],
        host_kind = gen_b$host_kind[gp], path = gen_b$path[gp],
        rfc_path_form = gen_b$rfc_path_form[gp], query = gen_b$query[gp],
        query_kind = gen_b$query_kind[gp], port = gen_b$port[gp],
        port_handling = opts$port_handling
      )
    }
  }

  # Phase 12: parse-status assignment (uses the post-subdomain-trim host,
  # exactly as the previous single-stage engine did).
  parse_status <- .derive_parse_status_vec(
    curl_ok = curl_ok,
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = opts$protocol_handling,
    final_scheme = a$final_scheme,
    looks_like_protocol = a$looks_like_protocol,
    original_has_allowed_scheme = a$original_has_allowed_scheme,
    looks_like_host_port = a$looks_like_host_port,
    is_scheme_relative = a$is_scheme_relative,
    scheme_relative_handling = opts$scheme_relative_handling,
    rfc3986_path_rootless = a$rfc3986_path_rootless,
    scheme_acceptance = opts$scheme_acceptance,
    is_general = if (general_acceptance) gp else NULL
  )

  # Phase 13: assemble the 14 typed columns.
  result <- .assemble_parse_result_vec(
    original_url = original_url,
    scheme_output = cased$scheme,
    host_output = cased$host,
    port = port_output,
    path_output = cased$path,
    raw_query = query_output,
    fragment = fragment_output,
    user = a$raw_user,
    password = a$raw_password,
    domain = domain,
    tld = tld,
    # Encoding-independent identity spellings (RURL-owrdsivt): surfaced straight
    # from Stage A's cached both-spelling decomposition, unaffected by
    # host_encoding (which only selects `domain`/`tld` above).
    domain_ascii = a$domain_ascii,
    domain_unicode = a$domain_unicode,
    tld_ascii = a$tld_ascii,
    tld_unicode = a$tld_unicode,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = a$is_scheme_relative,
    scheme_relative_handling = opts$scheme_relative_handling
  )

  # D5: scheme-less userinfo (user@example.com). host/domain/tld/user still
  # resolve, but rurl refuses to fabricate a canonical clean_url from an
  # ambiguous, email-shaped, scheme-less string: NA clean_url + the warning.
  slu <- a$scheme_less_userinfo & curl_ok
  if (any(slu)) {
    result$clean_url[slu] <- NA_character_
    result$parse_status[slu] <- .STATUS_WARN_USERINFO
  }
  result
}

# Internal implementation of safe_parse_url (not memoized)
# Arguments are already validated by the memoizing wrapper
._safe_parse_url_impl <- function(url,
                                  protocol_handling,
                                  www_handling,
                                  tld_source,
                                  case_handling,
                                  trailing_slash_handling,
                                  index_page_handling,
                                  path_normalization,
                                  scheme_relative_handling,
                                  subdomain_levels_to_keep,
                                  host_encoding = "keep",
                                  path_encoding = "keep",
                                  url_standard = NULL,
                                  engine = NULL) {
  original_input_url <- url

  # host_encoding/path_encoding are already validated upstream by
  # .parse_options(); no re-validation here.

  # Phase 1: scheme detection and input preparation for curl
  prep <- .prepare_url_for_curl(
    url, protocol_handling, scheme_relative_handling
  )
  if (is.null(prep)) {
    return(NULL)
  }

  # Phase 2: parse with curl, then pull out raw components
  parsed_curl <- .parse_with_curl(prep$url_to_parse)
  if (is.null(parsed_curl)) {
    return(NULL)
  }
  raw <- .extract_raw_components(parsed_curl, prep$url_to_parse)
  raw_host <- .mark_host_utf8(raw$host)
  raw_query <- raw$query

  # Phase 3: path normalization (decode, slashes/dots, index, trailing, encode)
  path_final <- .normalize_path(
    raw$path,
    path_encoding = path_encoding,
    path_normalization = path_normalization,
    index_page_handling = index_page_handling,
    trailing_slash_handling = trailing_slash_handling
  )

  # Phase 4: final scheme per protocol policy
  final_scheme <- .derive_final_scheme(
    protocol_handling, prep$looks_like_protocol, raw$scheme
  )

  # Phase 5: IP host detection
  is_ip_host <- .detect_ip_host(raw_host)

  # Phase 6: www prefix policy
  final_host <- .apply_www_policy(raw_host, www_handling, is_ip_host, engine)

  # Phase 7: registered-domain and TLD derivation. `domain`/`tld` follow
  # host_encoding (a rendering choice); the two fixed spellings (idna/unicode)
  # are also derived so the encoding-independent identity columns
  # (domain_ascii/domain_unicode/tld_ascii/tld_unicode, RURL-owrdsivt) can be
  # emitted -- mirroring Stage A's both-spelling derivation.
  domain_tld <- .derive_domain_tld(
    final_host, is_ip_host, tld_source, host_encoding, engine
  )
  domain <- domain_tld$domain
  tld <- domain_tld$tld
  dt_ascii <- .derive_domain_tld(
    final_host, is_ip_host, tld_source, "idna", engine
  )
  dt_unicode <- .derive_domain_tld(
    final_host, is_ip_host, tld_source, "unicode", engine
  )

  # Phase 8: subdomain-level policy
  final_host <- .apply_subdomain_policy(
    final_host, domain, subdomain_levels_to_keep, is_ip_host, engine
  )

  # Phase 9: host encoding (idna / unicode)
  host_for_clean <- .apply_host_encoding(
    final_host, host_encoding, is_ip_host, url_standard
  )

  # Phase 10: case policy applied to host, path, and scheme
  cased <- .apply_case_policy(
    host_for_clean, path_final, final_scheme, case_handling
  )
  host_output <- cased$host
  path_output <- cased$path
  scheme_output <- cased$scheme

  # Phase 11: clean URL reconstruction
  clean_url <- .build_clean_url(
    scheme_output, host_output, path_output, trailing_slash_handling
  )

  # Phase 12: parse-status assignment
  parse_status <- .derive_parse_status(
    parsed_curl = parsed_curl,
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = protocol_handling,
    final_scheme = final_scheme,
    looks_like_protocol = prep$looks_like_protocol,
    original_has_allowed_scheme = prep$original_has_allowed_scheme,
    looks_like_host_port = prep$looks_like_host_port,
    is_scheme_relative = prep$is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )

  # D5: scheme-less userinfo (user@example.com) -- suppress the fabricated
  # clean_url and flag warning-userinfo (host/domain/tld/user still resolve).
  if (isTRUE(prep$scheme_less_userinfo)) {
    clean_url <- NA_character_
    parse_status <- .STATUS_WARN_USERINFO
  }

  # Phase 13: assemble the typed result list
  .assemble_parse_result(
    original_input_url = original_input_url,
    scheme_output = scheme_output,
    host_output = host_output,
    parsed_curl = parsed_curl,
    path_output = path_output,
    raw_query = raw_query,
    domain = domain,
    tld = tld,
    domain_ascii = dt_ascii$domain,
    domain_unicode = dt_unicode$domain,
    tld_ascii = dt_ascii$tld,
    tld_unicode = dt_unicode$tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = prep$is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )
}
