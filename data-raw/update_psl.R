# Script to update the Public Suffix List used in this package
# Source: https://publicsuffix.org/list/

psl_url <- "https://publicsuffix.org/list/public_suffix_list.dat"
psl_path <- tempfile(fileext = ".dat")

message("Downloading Public Suffix List...")
download.file(psl_url, psl_path, quiet = TRUE, mode = "wb")

# Read and clean the file (remove comments and empty lines)
psl_raw <- readLines(psl_path, warn = FALSE, encoding = "UTF-8")
psl_clean <- grep("^[^/\\s]", psl_raw, value = TRUE)

# Store as internal data
usethis::use_data(psl_clean, internal = TRUE, overwrite = TRUE)
message("PSL updated and saved as internal data.")
