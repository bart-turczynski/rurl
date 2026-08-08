# Security Policy

## Supported versions

`rurl` is distributed through CRAN. Security fixes are made against the latest
released version; please upgrade to the most recent release before reporting.

| Version              | Supported          |
| -------------------- | ------------------ |
| Latest CRAN release  | :white_check_mark: |
| Older releases       | :x:                |

## Reporting a vulnerability

**Please do not report security vulnerabilities through public issues.**

Preferred channel — **email the maintainer at bartek@turczynski.pl.**

Alternatively, open a **confidential issue** on the GitLab project:

1. Go to [Issues](https://gitlab.com/bart-turczynski/rurl/-/issues) and click
   **New issue**.
2. Tick **This issue is confidential** before submitting.

A confidential issue is visible only to project members.

Email is listed first deliberately: it works whether or not you have a GitLab
account, and it is the channel the maintainer monitors.

## What to expect

- We aim to acknowledge a report within **7 days**.
- We will investigate, work on a fix, and coordinate disclosure with you.
- We are happy to credit reporters in the release notes unless you prefer to
  remain anonymous.

## Scope

`rurl` is a pure-R library for parsing, normalizing, and cleaning URLs. It makes
no network connections of its own and resolves no hostnames, so its security
surface is the handling of untrusted URL *strings*.

It does parse credentials that untrusted input carries. A `user:password@host`
authority is decomposed and exposed through `get_user()`, `get_password()` and
`get_userinfo()`, and preserved by `serialize_url()`. `clean_url` always strips
credentials, and `rurl_url_key` excludes them from the key tuple, but a caller
that logs or prints a parse result can print a password. Treat parse output from
untrusted input as sensitive.

### What is in scope

- A component accessor returning a value that misrepresents the input — most
  importantly a host that is not the host a fetching client would connect to.
- Output that cannot be re-parsed, or that re-reads as a *different* URL. This
  includes a serialized URL whose authority was fabricated rather than parsed.
- Crashes, hangs, or unbounded memory on any input string.
- Credentials or other input content appearing in a condition message.

### What is out of scope

`rurl` reports facts about a URL; it returns no allow/deny verdict, and a
diagnostic is evidence rather than a decision. In particular:

- **It is not an SSRF guard.** `get_host_type()` and `get_url_diagnostics()` tell
  you a host is an IP literal and how it was spelled (`ipv4-octal`,
  `ipv4-number-form`, …); they do not tell you it is loopback, private, or
  link-local, and rurl deliberately holds no address-range classification. Answer
  that at the address layer, then apply policy above it.
- **It does not decide which schemes you should accept.** The default
  `scheme_acceptance = "web"` admits five schemes, but
  `scheme_acceptance = "general"` accepts any syntactically valid scheme,
  including `file:`, `smb:`, `scp:` and `javascript:`. Restricting schemes is the
  caller's job.
- **A `file:` URL is local-filesystem access, and it is not always local.** If
  you hand a parsed `file:` URL to anything that dereferences it, `file:///etc/passwd`
  reads a local file. A `file:` URL with a *non-empty* host — `file://server/share/x`
  — is a UNC path on Windows, so dereferencing it reaches a **remote SMB share**
  and can leak credentials to a host the URL author chose. rurl parses these;
  it does not open them, and it does not warn you. If your inputs are
  user-controlled, reject `file:` before dereferencing rather than after, and
  check `get_host()` — an empty host is the local-only form.
- Percent-*decoding* surfaces return the decoded bytes, which is what decoding
  means. `get_query()` (which decodes by default) and `path_encoding = "decode"`
  can therefore return control characters, including CR and LF. Do not
  interpolate a decoded value into a protocol context; use the encoded spelling.
- Dereferencing anything — fetching, following redirects, reading a `file:` path.
