# Security Policy

## Supported versions

`rurl` is distributed through CRAN. Security fixes are made against the
latest released version; please upgrade to the most recent release
before reporting.

| Version                     | Supported |
|-----------------------------|-----------|
| Latest CRAN release (1.4.x) | ✅        |
| Older releases              | ❌        |

## Reporting a vulnerability

**Please do not report security vulnerabilities through public GitHub
issues.**

Preferred channel — **GitHub private vulnerability reporting**:

1.  Go to the repository’s **Security** tab.
2.  Click **Report a vulnerability**.

This opens a private security advisory visible only to the maintainers.

If you cannot use that channel, email the maintainer at
**<bartek@turczynski.pl>** instead.

## What to expect

- We aim to acknowledge a report within **7 days**.
- We will investigate, work on a fix, and coordinate disclosure with
  you.
- We are happy to credit reporters in the release notes unless you
  prefer to remain anonymous.

## Scope

`rurl` is a pure-R library for parsing, normalizing, and cleaning URLs.
It makes no network connections of its own and handles no credentials,
so its security surface is limited to the safe handling of untrusted URL
input.
