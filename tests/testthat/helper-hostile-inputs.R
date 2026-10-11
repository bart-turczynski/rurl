# RURL-jmrpffgf: the machinery behind test-hostile-input-contracts.R.
#
# Builds short hostile rows cut into a URL template at one component position
# each, calls an export from the capability manifest
# (helper-capability-manifest.R) over them between good neighbors, and returns
# every broken contract as one row of a violations table keyed by (export,
# standard, class, position). Every name here starts with `hi_`: helpers share
# one environment with every test file.

# --- Input classes -----------------------------------------------------------
#
#   bom           U+FEFF leads the component. Some stringi functions skip a
#                 leading U+FEFF (.starts_with_bom() in R/parse-phases.R).
#   zwsp          U+200B in the same place: a zero-width code point stringi
#                 does not skip, the control for `bom`.
#   invalid_utf8  an unmarked octet 0xE9 that starts no UTF-8 sequence here.
#   bytes         an element marked "bytes" holding octet 0xFC.
#   latin1        an element marked "latin1" holding octet 0xFC (U+00FC).
#   utf8_twin     the same row spelled in UTF-8: the control for `latin1`.
#   na            NA_character_ (it has no position).
#   long          hi_long_n octets of "a" and then the sentinel "ztailz" lead
#                 the component.
#
# Every non-ASCII row is built from raw octets with rawToChar(), never from a
# source literal, and then given the mark its class names.

hi_long_n <- 5000L
hi_sentinel <- "ztailz"

hi_payload <- function(class, long_n = hi_long_n) {
  payloads <- list(
    bom = list(octets = as.raw(c(0xEF, 0xBB, 0xBF)), mark = "UTF-8"),
    zwsp = list(octets = as.raw(c(0xE2, 0x80, 0x8B)), mark = "UTF-8"),
    invalid_utf8 = list(octets = as.raw(0xE9), mark = "unknown"),
    bytes = list(octets = as.raw(0xFC), mark = "bytes"),
    latin1 = list(octets = as.raw(0xFC), mark = "latin1"),
    utf8_twin = list(octets = as.raw(c(0xC3, 0xBC)), mark = "UTF-8"),
    long = list(octets = charToRaw(paste0(strrep("a", long_n), hi_sentinel)),
                mark = "unknown")
  )
  if (!class %in% names(payloads)) {
    stop("unknown input class: ", class, call. = FALSE)
  }
  payloads[[class]]
}
hi_classes <- c("bom", "zwsp", "invalid_utf8", "bytes", "latin1",
                "utf8_twin", "na", "long")

# URL templates, as named parts. A position is the name of the part the
# payload is put in front of; `lead` puts it before the whole URL.
hi_templates <- list(
  web = c(lead = "", "http://", userinfo = "u", "@", host = "h.example", "/",
          path = "p", "?", query = "q=v", "#", fragment = "f"),
  mailto = c(lead = "", "mailto:", local = "l", "@", domain = "h.example",
             "?", query = "subject=s"),
  file = c(lead = "", "file:///", path = "p")
)
hi_positions <- function(template) {
  nm <- names(hi_templates[[template]])
  nm[nzchar(nm)]
}
hi_plain <- function(template) {
  paste(hi_templates[[template]], collapse = "")
}

# Good neighbors that sit between the hostile rows of a batch.
hi_neighbors <- list(
  web = c("http://good-one.example/a?x=1#y", "https://good-two.example/b/"),
  mailto = c("mailto:one@good.example", "mailto:two@good.example?subject=hi"),
  file = c("file:///good/one", "https://good-two.example/b/")
)

# One hostile row: `class`'s payload in front of `position`.
hi_row <- function(template, class, position, long_n = hi_long_n) {
  if (class == "na") {
    return(NA_character_)
  }
  pl <- hi_payload(class, long_n)
  parts <- hi_templates[[template]]
  octets <- unlist(lapply(seq_along(parts), function(i) {
    p <- charToRaw(parts[[i]])
    if (identical(names(parts)[i], position)) c(pl$octets, p) else p
  }))
  out <- rawToChar(octets)
  if (pl$mark != "unknown") {
    Encoding(out) <- pl$mark
  }
  out
}

# The cases of one class: a named character vector, position -> row.
hi_class_rows <- function(template, class, positions = NULL,
                          long_n = hi_long_n) {
  if (class == "na") {
    return(c(none = NA_character_))
  }
  pos <- if (is.null(positions)) hi_positions(template) else positions
  out <- vapply(pos, function(p) hi_row(template, class, p, long_n), "")
  names(out) <- pos
  out
}

# --- Calling an export ------------------------------------------------------

hi_selector_args <- function(entry, std) {
  value <- if (identical(std, "NULL")) list(NULL) else list(std)
  switch(entry$selector,
    url_standard = ,
    dots = stats::setNames(value, "url_standard"),
    standard = stats::setNames(value, "standard"),
    policy = list(policy = url_key_policy(standard = value[[1L]])),
    none = list()
  )
}

# An option set as the manifest spells it: a name starting with "." is
# metadata for the harness (`.standards` limits the set to those standards),
# never an argument.
hi_call_options <- function(opts) {
  nm <- names(opts)
  if (is.null(nm)) {
    return(opts)
  }
  opts[!startsWith(nm, ".")]
}
hi_options_apply <- function(opts, std) {
  is.null(opts$.standards) || std %in% opts$.standards
}
hi_option_id <- function(opts) {
  o <- hi_call_options(opts)
  if (length(o) == 0L) {
    return("default")
  }
  paste(names(o), unlist(o), sep = "=", collapse = ",")
}

hi_base_url <- "http://base.example/dir/page?bq=1#bf"
hi_relative <- "g/h?x=1#y"

# A runner for one (export, standard, option set, URL operand): `run(urls)`
# calls the export with `urls` in the operand, and `pieces(out, solos)` cuts
# its output into one value per input element (`solos` are the solo outputs
# of the same elements, which an expanded output needs to find its segments).
# `pieces` is NULL when the output has no per-element value (an aggregate, or
# a semi or anti join read from its `y` side): those compare whole.
hi_runner <- function(fn, entry, std, opts, operand) {
  f <- get(fn, envir = asNamespace("rurl"))
  args <- c(hi_selector_args(entry, std), hi_call_options(opts))
  if (entry$shape == "join") {
    return(hi_join_runner(fn, f, entry, args, operand))
  }
  run <- function(urls) {
    lead <- if (fn == "resolve_url" && operand == "base_url") {
      list(relative_or_absolute = hi_relative, base_url = urls)
    } else if (fn == "resolve_url") {
      list(relative_or_absolute = urls, base_url = hi_base_url)
    } else {
      list(urls)
    }
    do.call(f, c(lead, args))
  }
  pieces <- switch(entry$shape,
    aggregate = NULL,
    scalar = function(out, solos) list(out),
    expanded = function(out, solos) {
      n <- vapply(solos, nrow, 1L)
      if (sum(n) != nrow(out)) {
        return(NULL)
      }
      ends <- cumsum(n)
      lapply(seq_along(n), function(k) {
        r <- out[seq_len(n[k]) + ends[k] - n[k], , drop = FALSE]
        rownames(r) <- NULL
        r
      })
    },
    function(out, solos) {
      # get_url_diagnostics() returns a length-1 input's tokens bare
      # (documented); every other list output keeps the list.
      if (entry$shape == "list" && !is.list(out)) {
        out <- list(out)
      }
      lapply(seq_along(solos), function(i) hi_project(out, i))
    }
  )
  list(run = run, pieces = pieces, scalar = entry$shape == "scalar")
}

# A join reads `urls` as the URL column of its `operand` table; the other
# table holds the good neighbors and the plain template row, so a hostile row
# that normalizes to the template has a partner. Each element's value is the
# set of output rows carrying its id, without the id.
hi_join_runner <- function(fn, f, entry, args, operand) {
  canonical <- fn == "canonical_join"
  col <- if (canonical) "URL" else "url"
  other <- c(hi_neighbors$web, hi_plain("web"))
  first <- operand == entry$operands[1L]
  run <- function(urls) {
    side <- data.frame(sid = seq_along(urls), stringsAsFactors = FALSE)
    side[[col]] <- urls
    oth <- data.frame(oid = seq_along(other), stringsAsFactors = FALSE)
    oth[[col]] <- other
    tabs <- if (first) list(side, oth) else list(oth, side)
    if (canonical) {
      # Explicit output names: the default deparse(substitute()) of a table
      # passed through do.call() is not a name.
      suppressWarnings(
        do.call(f, c(list(data_A = tabs[[1L]], data_B = tabs[[2L]],
                          name_A = "A", name_B = "B"), args)),
        classes = "rurl_canonical_join_deprecated"
      )
    } else {
      do.call(f, c(list(x = tabs[[1L]], y = tabs[[2L]], by = col), args))
    }
  }
  semi_anti <- fn %in% c("url_semi_join", "url_anti_join")
  pieces <- if (semi_anti && !first) {
    NULL
  } else {
    function(out, solos) {
      id <- grep("^sid", names(out), value = TRUE)[1L]
      lapply(seq_along(solos), function(i) {
        r <- out[!is.na(out[[id]]) & out[[id]] == i, setdiff(names(out), id),
                 drop = FALSE]
        rownames(r) <- NULL
        r
      })
    }
  }
  list(run = run, pieces = pieces, scalar = FALSE)
}

# One row's value in a per-row output.
hi_project <- function(out, i) {
  if (is.data.frame(out)) {
    r <- out[i, , drop = FALSE]
    rownames(r) <- NULL
    r
  } else if (inherits(out, "rurl_url_key")) {
    list(key = unclass(out)[i], keyability = attr(out, "keyability")[i])
  } else {
    out[i]
  }
}

# --- Comparing outputs ------------------------------------------------------

# Apply `f` to every string in an output, names included.
hi_map_strings <- function(x, f) {
  if (!is.null(names(x))) {
    names(x) <- f(names(x))
  }
  if (is.list(x)) {
    a <- attributes(x)
    x <- lapply(x, hi_map_strings, f = f)
    attributes(x) <- a
    return(x)
  }
  if (!is.character(x)) {
    return(x)
  }
  a <- attributes(x)
  x <- f(x)
  attributes(x) <- a
  x
}

# Decode every ACE label ("xn--...") to Unicode, so a host that keeps the
# mark (under rfc3986 and NULL, host_encoding = "idna" encodes the host as
# written) compares as the code points it encodes.
hi_decode_ace <- function(s) {
  hit <- !is.na(s) & grepl("xn--", s, fixed = TRUE)
  if (!any(hit)) {
    return(s)
  }
  m <- gregexpr("xn--[A-Za-z0-9-]+", s[hit])
  regmatches(s[hit], m) <- lapply(regmatches(s[hit], m), function(lab) {
    dec <- suppressWarnings(punycoder::puny_decode(lab))
    ifelse(is.na(dec), lab, dec)
  })
  s
}

# Rewrite every U+FEFF spelling in an output to its U+200B spelling, so the
# output for a `bom` row can be compared with its `zwsp` twin.
hi_bom_to_zwsp <- function(s) {
  from <- c("﻿", "%EF%BB%BF", "%ef%bb%bf", "<U+FEFF>")
  to <- c("​", "%E2%80%8B", "%e2%80%8b", "<U+200B>")
  for (k in seq_along(from)) {
    s <- gsub(from[k], to[k], s, fixed = TRUE)
  }
  s
}

hi_short <- function(x) {
  s <- paste(utils::capture.output(utils::str(x, nchar.max = 60,
                                              vec.len = 2)),
             collapse = " ")
  substr(gsub("\\s+", " ", s), 1L, 160L)
}

# --- Locales ----------------------------------------------------------------

hi_utf8_locale <- function() {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "en_GB.UTF-8")) {
    got <- suppressWarnings(
      withr::with_locale(c(LC_CTYPE = loc), Sys.getlocale("LC_CTYPE"))
    )
    if (identical(got, loc)) {
      return(loc)
    }
  }
  NA_character_
}

# Evaluate `code` with LC_CTYPE switched in-process, on a cold cache: the
# parse cache key does not carry the locale.
hi_in_ctype <- function(locale, code) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  withr::with_locale(c(LC_CTYPE = locale), force(code))
}

hi_try <- function(expr) {
  tryCatch(
    suppressWarnings(list(ok = TRUE, value = expr)),
    error = function(e) list(ok = FALSE, msg = conditionMessage(e))
  )
}

# --- The contracts ----------------------------------------------------------

hi_violation <- function(fn, std, opt_id, class, position, check, detail) {
  data.frame(export = fn, standard = std, options = opt_id, class = class,
             position = position, check = check,
             detail = substr(detail, 1L, 200L), stringsAsFactors = FALSE)
}
hi_no_violations <- function() {
  hi_violation(character(0), character(0), character(0), character(0),
               character(0), character(0), character(0))
}

# The value of component `position` in one value, for the sentinel check;
# NULL when the output does not carry it as one string.
hi_echo_value <- function(value, position) {
  if (is.data.frame(value)) {
    col <- c(path = "path", query = "query", fragment = "fragment",
             userinfo = "user", host = "host")[[position]]
    value <- value[[col]]
  }
  if (is.character(value) && length(value) == 1L) value else NULL
}

# query_param_summary(): the per-parameter counts of a batch are the sums of
# its rows' counts, and each parameter's drop verdict is its rows' verdict.
hi_aggregate_isolated <- function(batch, solos) {
  all <- do.call(rbind, solos)
  key <- intersect(c("param", "value"), names(batch))
  k_all <- do.call(paste, c(all[key], sep = "\r"))
  k_b <- do.call(paste, c(batch[key], sep = "\r"))
  if (!setequal(k_all, k_b)) {
    return(FALSE)
  }
  n <- tapply(all$n, k_all, sum)
  n_urls <- tapply(all$n_urls, k_all, sum)
  drop <- tapply(all$would_drop, k_all, function(v) v[1L])
  identical(as.vector(n[k_b]), as.vector(batch$n)) &&
    identical(as.vector(n_urls[k_b]), as.vector(batch$n_urls)) &&
    identical(as.vector(drop[k_b]), as.vector(batch$would_drop))
}

# Run every contract for one (export, standard, option set, URL operand) over
# `classes` at the entry's positions of its template (all of them unless the
# entry names some), under LC_CTYPE `utf8` with `c_loc` for the invariance
# check (NA skips it). Returns the violations table.
hi_check_export <- function(fn, entry, std, opts, operand, utf8,
                            c_loc = "C", classes = hi_classes,
                            positions = entry$positions,
                            long_n = if (is.null(entry$long_n)) {
                              hi_long_n
                            } else {
                              entry$long_n
                            }) {
  tpl <- entry$template
  nb <- hi_neighbors[[tpl]]
  opt_id <- hi_option_id(opts)
  rn <- hi_runner(fn, entry, std, opts, operand)
  acc <- new.env()
  acc$out <- list()
  add <- function(class, position, check, detail) {
    if (length(entry$operands) > 1L) {
      position <- paste0(operand, ":", position)
    }
    acc$out[[length(acc$out) + 1L]] <- hi_violation(fn, std, opt_id, class,
                                                    position, check, detail)
  }
  solo_of <- function(u, loc = utf8) hi_in_ctype(loc, hi_try(rn$run(u)))
  piece_of <- function(s) {
    if (is.null(rn$pieces)) s else rn$pieces(s, list(s))[[1L]]
  }
  nb_solo <- lapply(nb, solo_of)
  values <- list()

  for (class in classes) {
    rows <- hi_class_rows(tpl, class, positions, long_n)
    solos <- lapply(rows, solo_of)
    for (k in seq_along(rows)) {
      if (solos[[k]]$ok) {
        values[[class]][[names(rows)[k]]] <- piece_of(solos[[k]]$value)
      } else {
        add(class, names(rows)[k], "error", solos[[k]]$msg)
      }
    }
    ok <- vapply(solos, `[[`, TRUE, "ok")

    if (rn$scalar) {
      # One call per URL: the locale contract is all that applies.
      if (!is.na(c_loc) && !class %in% c("na", "long")) {
        for (k in which(ok)) {
          sc <- solo_of(rows[[k]], c_loc)
          if (!sc$ok || !identical(sc$value, solos[[k]]$value)) {
            add(class, names(rows)[k], "locale",
                if (sc$ok) hi_short(sc$value) else sc$msg)
          }
        }
      }
      next
    }

    # The batch: each hostile row between two good neighbors. Where the
    # locale contract applies (the first option set, a class with non-ASCII
    # octets) the batch runs under LC_CTYPE C and is compared with the UTF-8
    # solo values: when both contracts hold the two agree, and only a
    # mismatch pays for the UTF-8 batch that tells a locale break from an
    # isolation break.
    batch <- c(as.vector(rbind(rep_len(nb, length(rows)), rows)), nb[1L])
    hostile_at <- seq(2L, by = 2L, length.out = length(rows))
    nb_at <- setdiff(seq_along(batch), hostile_at)
    pos_of <- function(j) {
      if (j %in% hostile_at) names(rows)[match(j, hostile_at)] else "neighbor"
    }
    locale_on <- !is.na(c_loc) && !class %in% c("na", "long")
    b <- hi_in_ctype(if (locale_on) c_loc else utf8, hi_try(rn$run(batch)))
    st <- new.env()
    st$bu <- if (locale_on) NULL else b
    batch_u <- function() {
      if (is.null(st$bu)) {
        st$bu <- hi_in_ctype(utf8, hi_try(rn$run(batch)))
      }
      st$bu
    }
    if (!b$ok && locale_on && batch_u()$ok) {
      add(class, "combined", "locale", paste("C locale threw:", b$msg))
      b <- st$bu
      locale_on <- FALSE
    }
    if (!b$ok) {
      # Attribute the throw: each row alone between its two neighbors.
      alone <- FALSE
      for (k in seq_along(rows)) {
        r <- hi_in_ctype(utf8, hi_try(rn$run(c(nb[1L], rows[[k]], nb[2L]))))
        if (!r$ok && ok[k]) {
          alone <- TRUE
          add(class, names(rows)[k], "error", paste("in a batch:", r$msg))
        }
        alone <- alone || !ok[k]
      }
      if (!alone) {
        add(class, "combined", "error", b$msg)
      }
      next
    }
    if (!all(ok) || !all(vapply(nb_solo, `[[`, TRUE, "ok"))) {
      next
    }
    batch_solos <- vector("list", length(batch))
    batch_solos[hostile_at] <- lapply(solos, `[[`, "value")
    batch_solos[nb_at] <- lapply(batch[nb_at], function(u) {
      nb_solo[[match(u, nb)]]$value
    })

    if (is.null(rn$pieces)) {
      # A whole output: the C batch equals the UTF-8 batch, and an
      # aggregate's counts are the sums of its rows' counts.
      ref <- b$value
      if (locale_on && batch_u()$ok) {
        ref <- st$bu$value
        if (!identical(b$value, ref)) {
          add(class, "combined", "locale", "outputs differ")
        }
      }
      if (entry$shape == "aggregate" &&
          !hi_aggregate_isolated(ref, batch_solos)) {
        add(class, "combined", "isolation", hi_short(ref))
      }
      next
    }

    # Row isolation (and locale): each element's value equals its solo-call
    # value.
    pb <- rn$pieces(b$value, batch_solos)
    if (is.null(pb)) {
      add(class, "combined", "isolation", "row counts do not add up")
      next
    }
    pu <- NULL
    for (j in seq_along(batch)) {
      want <- piece_of(batch_solos[[j]])
      if (identical(pb[[j]], want)) {
        next
      }
      if (!locale_on) {
        add(class, pos_of(j), "isolation",
            paste("batch", hi_short(pb[[j]]), "solo", hi_short(want)))
        next
      }
      if (is.null(pu)) {
        u <- batch_u()
        pu <- if (u$ok) rn$pieces(u$value, batch_solos) else list()
        if (is.null(pu)) {
          pu <- list()
        }
      }
      pu_j <- if (length(pu) >= j) pu[[j]] else NULL
      if (!identical(pb[[j]], pu_j)) {
        add(class, pos_of(j), "locale", paste("C", hi_short(pb[[j]])))
      }
      if (!identical(pu_j, want)) {
        add(class, pos_of(j), "isolation",
            paste("batch", hi_short(pu_j), "solo", hi_short(want)))
      }
    }
  }

  # Twins: latin1 equals its UTF-8 twin; U+FEFF equals its U+200B twin once
  # every U+FEFF spelling in the output is rewritten (the output may echo it).
  twin <- function(a, b, rewrite = NULL) {
    for (p in intersect(names(values[[a]]), names(values[[b]]))) {
      ga <- values[[a]][[p]]
      gb <- values[[b]][[p]]
      if (!is.null(rewrite)) {
        ga <- hi_map_strings(hi_map_strings(ga, hi_decode_ace), rewrite)
        gb <- hi_map_strings(gb, hi_decode_ace)
      }
      if (!identical(ga, gb)) {
        add(a, p, "twin", paste(hi_short(ga), "| twin", hi_short(gb)))
      }
    }
  }
  twin("latin1", "utf8_twin")
  twin("bom", "zwsp", hi_bom_to_zwsp)

  # Long components keep their tail where the output echoes the component
  # and the plain template row has that component.
  if (!is.null(values$long) && length(entry$echoes) > 0L) {
    plain <- solo_of(hi_plain(tpl))
    if (plain$ok) {
      for (p in intersect(names(values$long), entry$echoes)) {
        base <- hi_echo_value(piece_of(plain$value), p)
        got <- hi_echo_value(values$long[[p]], p)
        if (!is.null(base) && !is.na(base) &&
            (is.null(got) || is.na(got) ||
               !grepl(hi_sentinel, got, fixed = TRUE))) {
          add("long", p, "sentinel",
              paste("lost the tail:", hi_short(got)))
        }
      }
    }
  }

  if (length(acc$out) == 0L) {
    return(hi_no_violations())
  }
  do.call(rbind, acc$out)
}

# Every (standard, option set, operand) of one manifest entry. The locale
# contract runs on the entry's first option set only: it is a property of the
# input boundary, and doubling every call for every option set would double
# the file's runtime.
hi_check_entry <- function(fn, entry = capability_manifest[[fn]],
                           utf8 = hi_utf8_locale(), c_loc = "C",
                           classes = hi_classes) {
  out <- list(hi_no_violations())
  for (std in entry$standards) {
    for (oi in seq_along(entry$options)) {
      opts <- entry$options[[oi]]
      if (!hi_options_apply(opts, std)) {
        next
      }
      for (operand in entry$operands) {
        out[[length(out) + 1L]] <- hi_check_export(
          fn, entry, std, opts, operand, utf8,
          c_loc = if (oi == 1L) c_loc else NA_character_, classes = classes
        )
      }
    }
  }
  do.call(rbind, out)
}
