# Internal parse-status constants
.STATUS_OK               <- "ok"
.STATUS_OK_FTP           <- "ok-ftp"
.STATUS_OK_SCHEME_REL    <- "ok-scheme-relative"
.STATUS_ERROR            <- "error"
.STATUS_WARN_NO_TLD      <- "warning-no-tld"
.STATUS_WARN_INVALID_TLD <- "warning-invalid-tld"
.STATUS_WARN_PUBLIC_SUFFIX <- "warning-public-suffix"
# Scheme-less input carrying userinfo (e.g. "user@example.com"): host/domain/tld
# and user still resolve, but rurl refuses to fabricate a canonical clean_url
# from an ambiguous, email-shaped, scheme-less string (clean_url is NA). Its NA
# key makes it non-joinable in canonical_join() regardless of join mode.
.STATUS_WARN_USERINFO    <- "warning-userinfo"

.is_ok_status      <- function(s) {
  s %in% c(.STATUS_OK, .STATUS_OK_FTP, .STATUS_OK_SCHEME_REL)
}
.is_warning_status <- function(s) startsWith(s, "warning-")
.is_joinable_status <- function(s, mode = "ok_or_warning") {
  if (mode == "ok_or_warning") .is_ok_status(s) | .is_warning_status(s)
  else .is_ok_status(s)
}
