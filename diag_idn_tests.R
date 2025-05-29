library(rurl)
library(urltools)

# Define test cases
# Each element is a list with: unicode_domain, label1_uni, tld_uni
test_cases <- list(
  list(unicode_domain = "παράδειγμα.ελ", label1_uni = "παράδειγμα", tld_uni = "ελ"),
  list(unicode_domain = "δοκιμή.ελ",      label1_uni = "δοκιμή",      tld_uni = "ελ"),
  list(unicode_domain = "ελλάδα.ελ",       label1_uni = "ελλάδα",      tld_uni = "ελ"),
  list(unicode_domain = "τεστ.ελ",        label1_uni = "τεστ",        tld_uni = "ελ"), # Shorter Greek label
  list(unicode_domain = "α.ελ",           label1_uni = "α",            tld_uni = "ελ"), # Single char Greek label
  
  list(unicode_domain = "тест.рф",        label1_uni = "тест",        tld_uni = "рф"),
  list(unicode_domain = "пример.рф",      label1_uni = "пример",      tld_uni = "рф"),
  
  list(unicode_domain = "例子.中国",      label1_uni = "例子",        tld_uni = "中国"),
  list(unicode_domain = "公司.中国",      label1_uni = "公司",        tld_uni = "中国"),
  
  list(unicode_domain = "회사.한국",      label1_uni = "회사",        tld_uni = "한국"),
  list(unicode_domain = "정보.한국",      label1_uni = "정보",        tld_uni = "한국")
)

# Function to perform and print tests for a single case
run_diag_tests <- function(case) {
  cat(paste0("\n--- Testing: ", case$unicode_domain, " ---\n"))
  
  # 1. Encode to Punycode
  label1_puny <- NA
  tld_puny <- NA
  full_domain_puny <- NA
  
  cat("1. urltools::puny_encode results:\n")
  tryCatch({
    label1_puny <- urltools::puny_encode(case$label1_uni)
    cat(paste0("  Label 1 ('", case$label1_uni, "') -> Punycode: ", label1_puny, "\n"))
  }, error = function(e) cat(paste0("  Error encoding label 1 ", case$label1_uni, ": ", e$message, "\n")))
  
  tryCatch({
    tld_puny <- urltools::puny_encode(case$tld_uni)
    cat(paste0("  TLD     ('", case$tld_uni, "') -> Punycode: ", tld_puny, "\n"))
  }, error = function(e) cat(paste0("  Error encoding TLD ", case$tld_uni, ": ", e$message, "\n")))
  
  if (!is.na(label1_puny) && !is.na(tld_puny)) {
    full_domain_puny <- paste0(label1_puny, ".", tld_puny)
    cat(paste0("  Full Punycode Domain Constructed: ", full_domain_puny, "\n"))
  } else {
    cat("  Could not construct full Punycode domain due to encoding errors.\n")
    # Attempt to encode full unicode domain as a fallback if parts failed
    tryCatch({
        full_domain_puny_alt <- urltools::puny_encode(case$unicode_domain)
        cat(paste0("  Encoding full unicode domain ('", case$unicode_domain, "') directly -> Punycode: ", full_domain_puny_alt, "\n"))
        if (is.na(full_domain_puny)) full_domain_puny <- full_domain_puny_alt
    }, error = function(e) cat(paste0("  Error encoding full unicode domain ", case$unicode_domain, ": ", e$message, "\n")))
  }
  
  # 2. Test urltools::puny_decode on individual labels
  cat("\n2. urltools::puny_decode on individual Punycode labels:\n")
  if (!is.na(label1_puny)) {
    tryCatch({
      decoded_l1 <- urltools::puny_decode(label1_puny)
      cat(paste0("  Label 1 ('", label1_puny, "') -> Unicode: ", decoded_l1, 
                 " (Matches original: ", identical(decoded_l1, case$label1_uni), ")\n"))
    }, error = function(e) cat(paste0("  Error decoding label 1 ", label1_puny, ": ", e$message, "\n")))
  }
  if (!is.na(tld_puny)) {
    tryCatch({
      decoded_tld <- urltools::puny_decode(tld_puny)
      cat(paste0("  TLD     ('", tld_puny, "') -> Unicode: ", decoded_tld,
                 " (Matches original: ", identical(decoded_tld, case$tld_uni), ")\n"))
    }, error = function(e) cat(paste0("  Error decoding TLD ", tld_puny, ": ", e$message, "\n")))
  }

  # 3. Test urltools::puny_decode on full Punycode domain
  cat("\n3. urltools::puny_decode on full Punycode domain string:\n")
  if (!is.na(full_domain_puny)) {
    tryCatch({
      decoded_full_direct <- urltools::puny_decode(full_domain_puny)
      cat(paste0("  Full Punycode ('", full_domain_puny, "') -> Unicode: ", decoded_full_direct,
                 " (Matches original: ", identical(decoded_full_direct, case$unicode_domain), ")\n"))
      if (!identical(decoded_full_direct, case$unicode_domain)) {
          cat(paste0("    WARNING: Direct decode of full Punycode domain does not match original Unicode domain!\n"))
          cat(paste0("    Expected: ", case$unicode_domain, " vs Got: ", decoded_full_direct, "\n"))
      }
    }, error = function(e) cat(paste0("  Error decoding full Punycode ", full_domain_puny, ": ", e$message, "\n")))
  }
  
  # 4. Test rurl functions
  cat("\n4. rurl function tests:\n")
  # Test with Unicode domain string
  cat("  Using Unicode domain string ('", case$unicode_domain, "'):\n")
  tryCatch({
    rurl_domain_uni <- rurl::get_domain(case$unicode_domain)
    rurl_tld_uni <- rurl::get_tld(case$unicode_domain)
    cat(paste0("    get_domain: ", rurl_domain_uni, " (Expected: ", case$unicode_domain, ")\n"))
    cat(paste0("    get_tld   : ", rurl_tld_uni,    " (Expected: ", case$tld_uni, ")\n"))
     if (!identical(rurl_domain_uni, case$unicode_domain)) {
          cat(paste0("      WARNING: rurl::get_domain on Unicode string MISMATCH! Expected: ", case$unicode_domain, " vs Got: ", rurl_domain_uni, "\n"))
      }
  }, error = function(e) cat(paste0("    Error with rurl functions on Unicode: ", e$message, "\n")))
  
  # Test with full Punycode domain string (if available)
  if (!is.na(full_domain_puny)) {
    cat("  Using full Punycode domain string ('", full_domain_puny, "'):\n")
    tryCatch({
      rurl_domain_puny <- rurl::get_domain(full_domain_puny)
      rurl_tld_puny <- rurl::get_tld(full_domain_puny)
      cat(paste0("    get_domain: ", rurl_domain_puny, " (Expected: ", case$unicode_domain, ")\n")) # Expected is still the Unicode version
      cat(paste0("    get_tld   : ", rurl_tld_puny,    " (Expected: ", case$tld_uni, ")\n"))    # Expected is still the Unicode version
      if (!identical(rurl_domain_puny, case$unicode_domain)) {
          cat(paste0("      WARNING: rurl::get_domain on Punycode string MISMATCH! Expected: ", case$unicode_domain, " vs Got: ", rurl_domain_puny, "\n"))
      }
    }, error = function(e) cat(paste0("    Error with rurl functions on Punycode: ", e$message, "\n")))
  }
  cat("---------------------------------------\n")
}

# Run tests for all cases
cat("Starting IDN Diagnostic Tests...\n")
cat("Ensure 'rurl' package is loaded (e.g., via devtools::load_all() or library(rurl))\n")

if (!("package:rurl" %in% search() || "rurl" %in% loadedNamespaces())) {
  warning("'rurl' package does not seem to be loaded. Results may be inaccurate. Please load it first.")
}

for (case in test_cases) {
  run_diag_tests(case)
}

cat("IDN Diagnostic Tests Finished.\n")

# Note: 
# The `expected` domain for rurl::get_domain in these simple two-label cases should be the full unicode_domain itself,
# assuming the TLD is in the PSL and there are no other rules making the TLD itself a registered domain.
# For example, if '.ελ' is a TLD, then 'παράδειγμα.ελ' is the registered domain.
# This script does not use safe_parse_url directly but tests the underlying accessors.
# The behavior of safe_parse_url might differ slightly due to its more complex parsing logic for full URLs. 