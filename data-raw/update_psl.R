# Script to update the Public Suffix List used in this package
# Source: https://publicsuffix.org/list/

psl_url <- "https://publicsuffix.org/list/public_suffix_list.dat"
psl_path <- tempfile(fileext = ".dat")

message("Downloading Public Suffix List...")
download.file(psl_url, psl_path, quiet = TRUE, mode = "wb")

# Read full list
psl_raw <- readLines(psl_path, warn = FALSE, encoding = "UTF-8")

# Basic cleaned version (non-comment, non-empty)
psl_clean <- grep("^[^/\\s]", psl_raw, value = TRUE)

# Helpers
clean_lines <- function(lines) {
  lines <- trimws(lines)
  lines <- lines[!grepl("^//", lines) & nzchar(lines)]
  lines <- sub("^[*!]", "", lines)
  unique(lines)
}
sort_by_depth <- function(tlds) {
  tlds[order(-vapply(strsplit(tlds, "\\."), length, integer(1)))]
}

# Extract ranges
icann_start <- grep("// ===BEGIN ICANN DOMAINS===", psl_raw)
icann_end   <- grep("// ===END ICANN DOMAINS===", psl_raw)
private_start <- grep("// ===BEGIN PRIVATE DOMAINS===", psl_raw)
private_end   <- grep("// ===END PRIVATE DOMAINS===", psl_raw)

# Extract and process
tld_icann   <- sort_by_depth(clean_lines(psl_raw[(icann_start + 1):(icann_end - 1)]))
tld_private <- sort_by_depth(clean_lines(psl_raw[(private_start + 1):(private_end - 1)]))
tld_all     <- sort_by_depth(unique(c(tld_icann, tld_private)))

# Add punycode-encoded versions of each TLD
add_punycode <- function(x) {
  puny <- urltools::puny_encode(x)
  unique(c(x, puny))
}


# Extract and process
tld_icann   <- sort_by_depth(unique(add_punycode(tld_icann)))
tld_private <- sort_by_depth(unique(add_punycode(tld_private)))
tld_all     <- sort_by_depth(unique(c(tld_icann, tld_private)))
clean_tlds <- tld_all
# Save internal data
usethis::use_data(
  psl_clean, tld_icann, tld_private, clean_tlds,
  internal = TRUE, overwrite = TRUE
)

message("PSL and TLD data updated and saved as internal package data.")

# Extract punycode ↔ Unicode mappings from comment lines
puny_lines <- grep("^//\\s+xn--", psl_raw, value = TRUE)

# Extract punycode TLD (xn--...) and associated Unicode version (assumed to be the next non-comment line)
punycode_map <- lapply(seq_along(puny_lines), function(i) {
  puny <- sub("^//\\s+(xn\\-\\-[a-z0-9\\-]+).*", "\\1", puny_lines[i])

  # Look ahead for the actual Unicode TLD in the following lines
  unicode_line <- NA
  next_lines <- psl_raw[(which(psl_raw == puny_lines[i]) + 1):(which(psl_raw == puny_lines[i]) + 5)]
  next_lines <- trimws(next_lines[!grepl("^//", next_lines) & nzchar(next_lines)])
  if (length(next_lines) > 0) {
    unicode_line <- next_lines[1]
  }

  c(puny = puny, unicode = unicode_line)
})

# Convert to data.frame and clean
punycode_df <- do.call(rbind, punycode_map)
punycode_df <- as.data.frame(punycode_df, stringsAsFactors = FALSE)
punycode_df <- punycode_df[!is.na(punycode_df$unicode) & nzchar(punycode_df$unicode), ]
punycode_df <- unique(punycode_df)

# Save it as internal data
usethis::use_data(punycode_df, internal = TRUE, overwrite = TRUE)
