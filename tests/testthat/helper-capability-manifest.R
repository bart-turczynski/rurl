# RURL-jmrpffgf: the capability manifest -- what each export takes.
#
# One entry per export that reads URL strings: the `url_standard` values it
# supports (and the ones it rejects), the URL operands it reads, a few options
# worth crossing, and the shape of what it returns. test-capability-manifest.R
# holds this list to NAMESPACE, so an export added without an entry here, or
# without a reasoned line on the exempt list below, fails the suite.
# test-hostile-input-contracts.R drives its hostile-input contracts from it.
#
# Fields:
#   selector    How the standard reaches the export:
#                 "url_standard"  the `url_standard` formal;
#                 "standard"      the `standard` formal (serialize_url());
#                 "policy"        `policy = url_key_policy(standard = )`;
#                 "dots"          `url_standard` passed through `...`
#                                 (canonical_join());
#                 "none"          the export takes no standard.
#   standards   The supported values, spelled as strings: "NULL" is the
#               byte-frozen pre-selector profile (ADR 0007), "none" means the
#               export takes no selector.
#   unsupported Selector values the export rejects with an error (ADR 0015
#               and the scheme_acceptance = "general" conflict check).
#   operands    The formals that carry URL strings. For a join they are the
#               two tables' URL columns.
#   template    "web" or "mailto": the URL shape the hostile rows are cut
#               into (see test-hostile-input-contracts.R).
#   options     Option sets crossed with every standard; `list()` is the
#               default call. Not a Cartesian product of every knob. A name
#               starting with "." is not an argument: `.standards` limits the
#               set to the standards it names.
#   positions   The template positions to cut hostile rows into; NULL is all
#               of them. Narrowed only where a call is expensive.
#   long_n      Length of the `long` payload; NULL is the harness default.
#   shape       What one input row maps to:
#                 "vector"     one element of a vector (a list under an
#                              option such as get_query(format = "list"));
#                 "key"        one element of a rurl_url_key plus its
#                              keyability attribute;
#                 "list"       one element of a list; a length-1 input
#                              returns that element bare;
#                 "frame"      one row of a data frame;
#                 "expanded"   zero or more rows of a data frame, in input
#                              order (get_mailto_recipients());
#                 "aggregate"  a summary over all rows (query_param_summary());
#                 "scalar"     one call per URL (safe_parse_url());
#                 "join"       a join of two tables on their URL columns.
#   echoes      Components the output carries verbatim enough to show a tail
#               sentinel, for the long-component contract.

cap_entry <- function(standards, shape, selector = "url_standard",
                      unsupported = character(0), operands = "url",
                      template = "web", options = list(list()),
                      echoes = character(0), positions = NULL,
                      long_n = NULL) {
  list(
    selector = selector, standards = standards, unsupported = unsupported,
    operands = operands, template = template, options = options,
    shape = shape, echoes = echoes, positions = positions, long_n = long_n
  )
}

cap_all3 <- c("NULL", "whatwg", "rfc3986")
cap_named <- c("whatwg", "rfc3986")
cap_join_positions <- "host"

capability_manifest <- list(
  # --- Parse surface -------------------------------------------------------
  safe_parse_urls = cap_entry(
    cap_all3, "frame",
    options = list(list(), list(path_encoding = "encode",
                                trailing_slash_handling = "strip",
                                query_handling = "keep")),
    echoes = c("path", "query", "fragment", "userinfo", "host")
  ),
  safe_parse_url = cap_entry(cap_all3, "scalar"),
  get_parse_status = cap_entry(cap_all3, "vector"),
  get_parse_verdicts = cap_entry(cap_all3, "frame"),
  get_clean_url = cap_entry(
    cap_all3, "vector",
    options = list(list(), list(query_handling = "keep",
                                index_page_handling = "strip",
                                path_encoding = "encode")),
    echoes = "path"
  ),
  # --- Component accessors -------------------------------------------------
  get_scheme = cap_entry(cap_all3, "vector"),
  get_host = cap_entry(
    cap_all3, "vector",
    options = list(list(), list(host_encoding = "idna")), echoes = "host"
  ),
  get_domain = cap_entry(cap_all3, "vector"),
  get_subdomain = cap_entry(cap_all3, "vector"),
  get_tld = cap_entry(cap_all3, "vector"),
  get_port = cap_entry(cap_all3, "vector"),
  get_path = cap_entry(
    cap_all3, "vector",
    options = list(list(), list(path_normalization = "dot_segments",
                                path_encoding = "encode")),
    echoes = "path"
  ),
  get_query = cap_entry(
    cap_all3, "vector",
    options = list(list(), list(format = "list")), echoes = "query"
  ),
  get_fragment = cap_entry(cap_all3, "vector", echoes = "fragment"),
  get_user = cap_entry(cap_all3, "vector", echoes = "userinfo"),
  get_password = cap_entry(cap_all3, "vector"),
  get_userinfo = cap_entry(cap_all3, "vector", echoes = "userinfo"),
  # --- Companion helpers: the standard is required (ADR 0015) --------------
  get_host_type = cap_entry(cap_named, "vector", unsupported = "NULL"),
  get_url_diagnostics = cap_entry(cap_named, "list", unsupported = "NULL"),
  get_scheme_class = cap_entry(cap_named, "vector", unsupported = "NULL"),
  # --- Host and scheme policy ----------------------------------------------
  # check_hosts() returns every rule's verdict, so is_valid_host() crosses
  # none.
  check_hosts = cap_entry(cap_all3, "frame"),
  is_valid_host = cap_entry(cap_all3, "vector"),
  # Default scheme_acceptance = "general", which NULL rejects.
  check_schemes = cap_entry(cap_named, "frame", unsupported = "NULL"),
  # The recipient splitter's time grows with the square of the local part
  # and domain length, so its long payload is shorter.
  get_mailto_recipients = cap_entry(
    cap_named, "expanded", unsupported = "NULL", template = "mailto",
    long_n = 2000L
  ),
  # --- Serializers and display ---------------------------------------------
  serialize_url = cap_entry(
    cap_named, "vector", selector = "standard", unsupported = "NULL",
    options = list(list(), list(form = "normalized")),
    echoes = c("path", "query", "fragment", "userinfo", "host")
  ),
  serialise_url = cap_entry(
    cap_named, "vector", selector = "standard", unsupported = "NULL",
    echoes = c("path", "query", "fragment", "userinfo", "host")
  ),
  format_url = cap_entry("none", "vector", selector = "none",
                         echoes = c("path", "query", "fragment")),
  get_url_key = cap_entry(
    cap_named, "key", selector = "policy", unsupported = "NULL"
  ),
  query_param_summary = cap_entry("none", "aggregate", selector = "none",
                                  operands = "urls"),
  # --- Reference resolution: both operands are URLs ------------------------
  resolve_url = cap_entry(
    cap_all3, "vector", operands = c("relative_or_absolute", "base_url"),
    # output = "serialized" under NULL is an error (no standard to
    # serialize as), pinned with the other rejected selectors.
    options = list(list(), list(output = "serialized",
                                .standards = cap_named))
  ),
  # --- Joins ---------------------------------------------------------------
  # Every join reads its keys through get_url_key() or safe_parse_urls(),
  # whose entries cross every position, and a join call is costly; a join is
  # held to hostile hosts, on both of its tables.
  url_inner_join = cap_entry(cap_named, "join", selector = "policy",
                             unsupported = "NULL", operands = c("x", "y"),
                             positions = cap_join_positions),
  url_left_join = cap_entry(cap_named, "join", selector = "policy",
                            unsupported = "NULL", operands = c("x", "y"),
                            positions = cap_join_positions),
  url_right_join = cap_entry(cap_named, "join", selector = "policy",
                             unsupported = "NULL", operands = c("x", "y"),
                             positions = cap_join_positions),
  url_full_join = cap_entry(cap_named, "join", selector = "policy",
                            unsupported = "NULL", operands = c("x", "y"),
                            positions = cap_join_positions),
  url_semi_join = cap_entry(cap_named, "join", selector = "policy",
                            unsupported = "NULL", operands = c("x", "y"),
                            positions = cap_join_positions),
  url_anti_join = cap_entry(cap_named, "join", selector = "policy",
                            unsupported = "NULL", operands = c("x", "y"),
                            positions = cap_join_positions),
  canonical_join = cap_entry(
    cap_all3, "join", selector = "dots", operands = c("data_A", "data_B"),
    options = list(list(join = "full")), positions = cap_join_positions
  )
)

# Exports that take no URL string, each with the reason it is exempt.
capability_exempt <- c(
  rurl_cache_config = "configures the memoization caches; takes no URL",
  rurl_cache_info = "reports the memoization caches; takes no argument",
  rurl_clear_caches = "empties the memoization caches; takes no argument",
  url_key_policy = "builds a join-key policy object; takes no URL",
  url_profile = "prints what a named profile bundles; takes no URL"
)
