# Internal parse-status constants
.STATUS_OK               <- "ok"
.STATUS_OK_FTP           <- "ok-ftp"
.STATUS_OK_SCHEME_REL    <- "ok-scheme-relative"
.STATUS_ERROR            <- "error"
.STATUS_WARN_NO_TLD      <- "warning-no-tld"
.STATUS_WARN_INVALID_TLD <- "warning-invalid-tld"
.STATUS_WARN_PUBLIC_SUFFIX <- "warning-public-suffix"

.is_ok_status      <- function(s) {
  s %in% c(.STATUS_OK, .STATUS_OK_FTP, .STATUS_OK_SCHEME_REL)
}
.is_warning_status <- function(s) startsWith(s, "warning-")
.is_joinable_status <- function(s, mode = "ok_or_warning") {
  if (mode == "ok_or_warning") .is_ok_status(s) | .is_warning_status(s)
  else .is_ok_status(s)
}
