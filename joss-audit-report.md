# JOSS Audit Report: hypothesize v0.10.0

**Audit date**: 2026-02-26 (re-audit after fixes)
**Auditor**: Automated JOSS readiness pipeline (Claude Opus 4.6)
**Repository**: <https://github.com/queelius/hypothesize>
**Zenodo DOI**: 10.5281/zenodo.18765626

## Summary

- **Status**: READY
- **Paper exists**: Yes, at `paper/paper.md` (1093 words)
- **Bibliography exists**: Yes, at `paper/paper.bib` (10 entries, all matched)
- **Tests**: 83 tests, all passing, 93.75% line coverage
- **R CMD check**: 0 errors, 0 warnings, 1 NOTE (benign timestamp check)
- **CI**: GitHub Actions R-CMD-check passing on 5 OS/R-version combinations
- **Reviewer checklist**: 27/28 items pass

All 7 major issues from the previous audit (2026-02-25) have been resolved.
One minor issue remains: the CODE_OF_CONDUCT.md contains an uncustomized
enforcement contact placeholder. This is the only recommended fix before
submission.

---

## Previous Issues: Resolution Status

| # | Previous Issue | Status | Evidence |
|---|---------------|--------|----------|
| 1 | Missing CONTRIBUTING.md | RESOLVED | File created with bug reports, PR, and dev setup sections |
| 2 | Missing CODE_OF_CONDUCT.md | RESOLVED | Contributor Covenant v2.1 added (minor placeholder remains) |
| 3 | State of the Field missing poolr and rstatix | RESOLVED | Both added with citations (`@cinar2021poolr`, `@kassambara2023rstatix`) and differentiation |
| 4 | No CI workflow | RESOLVED | `.github/workflows/R-CMD-check.yaml` with 5-config matrix, latest run passing |
| 5 | Missing LICENSE.md with full MIT text | RESOLVED | Full MIT text in LICENSE.md, excluded from R build via .Rbuildignore |
| 6 | Paper falsely claimed "available on CRAN" | RESOLVED | Changed to "submitted to CRAN" / r-universe |
| 7 | No GitHub issues for roadmap | RESOLVED | 3 open issues (#1 score test, #2 t-test, #3 Stouffer's method) |
| -- | print.hypothesis_test() returned NULL | RESOLVED | Now returns `invisible(x)` with correct `@return` tag |
| -- | .Rbuildignore incomplete | RESOLVED | Excludes paper/, .claude, community files, joss-audit-report.md |

---

## Critical Gaps (Blockers)

None.

---

## Major Issues (Should Fix)

None.

---

## Minor Issues (Nice to Fix)

### 1. CODE_OF_CONDUCT.md enforcement contact is a placeholder

- **Severity**: NOTE
- **What**: Line 40 of `CODE_OF_CONDUCT.md` reads:
  `...reported to the community leaders responsible for enforcement at [INSERT CONTACT METHOD].`
- **Why it matters**: JOSS reviewers inspect community files. A template placeholder
  signals the document was copied but not customized. Some reviewers will flag this.
- **Fix**: Replace `[INSERT CONTACT METHOD]` with the maintainer email
  (`lex@metafunctor.com`) or a dedicated reporting address.
- **File**: `/home/spinoza/github/rlang/hypothesize/CODE_OF_CONDUCT.md`, line 40

### 2. GitHub does not auto-detect the license (cosmetic)

- **Severity**: NOTE
- **What**: The GitHub API reports `spdx_id: "NOASSERTION"` because the `LICENSE`
  file contains only `YEAR: 2024 / COPYRIGHT HOLDER: Alexander Towell` (standard
  R convention for `MIT + file LICENSE`). The full MIT text is in `LICENSE.md`.
- **Why it matters**: The repository page may not display a license badge. This is
  cosmetic -- the DESCRIPTION correctly declares `License: MIT + file LICENSE` and
  the full text is in LICENSE.md.
- **Fix**: No action required. This is a known GitHub limitation with R packages.

### 3. print.hypothesis_test() has 0% test coverage

- **Severity**: NOTE
- **What**: Lines 88-94 of `R/hypothesize.R` (the `print.hypothesis_test` method)
  are never exercised by tests. Overall coverage is 93.75%.
- **Why it matters**: Minor coverage gap. Print methods are I/O and hard to break,
  but a reviewer may note the gap.
- **Fix**: Add a test like `expect_output(print(wald_test(1, 0.5)), "Hypothesis test")`
  to exercise the print path. This would bring coverage above 95%.

### 4. Unused imports in NAMESPACE

- **Severity**: NOTE
- **What**: `stats::pf` and `stats::qt` are imported (via `@importFrom` in
  roxygen2 comments) but never called in the source code.
- **Fix**: Remove `@importFrom stats pf` and `@importFrom stats qt` from the
  roxygen2 block in `hypothesis_test()`, then run `devtools::document()`.

### 5. Development history is compressed

- **Severity**: NOTE
- **What**: The repository has 11 commits total across 24 months. Several early
  commits have uninformative messages ("updates", "udpates"). There are 0 pull
  requests.
- **Why it matters**: JOSS reviewers check for evidence of sustained development.
  The 24-month span satisfies the 6-month rule, the tagged v0.10.0 release and
  3 open issues provide evidence of active planning, and CI is now passing.
- **Fix**: No action required for submission. Going forward, use descriptive
  commit messages and consider feature branches with PRs.

---

## JOSS Reviewer Checklist

### General Checks
- [x] Source code at repository URL (https://github.com/queelius/hypothesize)
- [x] OSI-approved LICENSE file (MIT declared in DESCRIPTION; full text in LICENSE.md)
- [x] Submitting author is major contributor (all 11 commits by Alexander Towell)
- [x] Demonstrates research impact (used in maskedcauses reliability ecosystem, teaching at SIUE)

### Development History
- [x] 6+ months public development (first commit 2024-02-19, latest 2026-02-26 = 24 months)
- [x] Evidence of releases and issues (v0.10.0 tagged release; 3 open issues; Zenodo DOI)
- [x] Active maintenance (most recent commit 2026-02-26; CI green)

### Functionality
- [x] Installation works as documented (`devtools::install_github("queelius/hypothesize")`)
- [x] Core functional claims confirmed (z_test, wald_test, lrt, fisher_combine, adjust_pval, confint)
- [x] Automated test suite (83 tests, testthat edition 3, all passing)
- [x] Test coverage adequate (93.75% line coverage)

### Documentation
- [x] Installation instructions (README.md)
- [x] Usage examples (README.md with 7 runnable examples; introductory vignette)
- [x] API documentation (16 roxygen2 man pages with examples and math; pkgdown site live)
- [x] Contribution guidelines (CONTRIBUTING.md)
- [x] Code of conduct (CODE_OF_CONDUCT.md, Contributor Covenant v2.1)

### Paper Quality
- [x] Summary section (clear, non-specialist accessible, describes 3 SICP principles)
- [x] Statement of Need (two audiences: applied researchers and package developers; gap identified)
- [x] State of the Field (6 packages named: infer, coin, broom, statsExpressions, poolr, rstatix)
- [x] Software Design (S3 class hierarchy, dual-interface fisher_combine, adjust_pval class preservation)
- [x] Research Impact Statement (maskedcauses ecosystem, SIUE pedagogy, Zenodo DOI, CRAN submission)
- [x] AI Usage Disclosure (present; specifies Claude used for docs, test scaffolding, manuscript)
- [x] References complete (10 citation keys in paper, all 10 in paper.bib, 0 orphaned entries)
- [x] Word count 750-1750 (1093 words)
- [x] YAML frontmatter complete (see detail below)

### YAML Frontmatter Detail
- [x] `title` -- includes package name ("hypothesize: A Consistent, Composable API...")
- [x] `tags` -- 5 entries (R, hypothesis testing, statistical inference, composable software, multiple testing)
- [x] `authors` -- name, orcid (0000-0001-6443-9897, 16 digits, validates at orcid.org), affiliation
- [x] `affiliations` -- index 1, "Southern Illinois University Edwardsville"
- [x] `date` -- "24 February 2026" (DD Month YYYY format)
- [x] `bibliography` -- points to paper.bib (exists, 10 entries)

### Post-Acceptance Requirements
- [x] Tagged release exists (v0.10.0)
- [x] Zenodo DOI minted (10.5281/zenodo.18765626 -- resolves HTTP 200)
- [x] CITATION.cff present (CFF 1.2.0, DOI, ORCID, version)
- [ ] Archive DOI in paper (concept DOI present; version-specific DOI assigned at acceptance)

---

## Specialist Reports

### Software Auditor

**Verdict**: PASS

| Aspect | Finding |
|--------|---------|
| Language | R (single-file package, ~765 lines including roxygen2) |
| Dependencies | Minimal: `stats` only (Imports). Suggests: testthat, knitr, rmarkdown |
| Test suite | 83 tests, 0 failures, 0 warnings, 0 skips |
| Test coverage | 93.75% (uncovered: `print.hypothesis_test`, 7 lines) |
| R CMD check | 0 errors, 0 warnings, 1 NOTE (unable to verify current time -- benign) |
| CI | GitHub Actions R-CMD-check on 5 OS/R-version configs, latest run: success |
| API design | Clean S3 generics, proper `UseMethod()`, extensible via `superclasses` |
| Documentation | 16 man pages with examples; 1 vignette; pkgdown site live at queelius.github.io |
| NAMESPACE | Generated by roxygen2; exports 10 functions; 2 unused imports (pf, qt) |
| Code quality | Consistent style, no global assignments, `invisible(x)` from print method |

### Community Auditor

**Verdict**: PASS

| File | Status | Notes |
|------|--------|-------|
| LICENSE + LICENSE.md | Present | MIT (R convention + full text) |
| CONTRIBUTING.md | Present | Bug reports, PRs, dev setup, links to CoC |
| CODE_OF_CONDUCT.md | Present | Contributor Covenant v2.1 (placeholder contact on line 40) |
| README.md | Present | Installation, 7 usage examples, extension guide, vignette link |
| NEWS.md | Present | Changelog for v0.10.0 and v0.9 |
| CITATION.cff | Present | CFF 1.2.0, DOI, ORCID, repo URL |
| .Rbuildignore | Present | Excludes paper/, .claude, community files from R package build |
| BugReports URL | Set | Points to github.com/queelius/hypothesize/issues |
| Issue tracker | Active | 3 open enhancement issues |
| GitHub Pages | Live | pkgdown site at https://queelius.github.io/hypothesize/ |
| GitHub community profile | Complete | CoC, Contributing, License, README all detected |

### Field Scout

**Verdict**: PASS

The State of the Field section names 6 packages with proper citations and
clear differentiation:

| Package | Cited | Differentiation |
|---------|-------|----------------|
| `infer` (Couch et al. 2021) | Yes | Simulation-based; no reusable return class for downstream packages |
| `coin` (Hothorn et al. 2008) | Yes | S4 permutation framework; tightly coupled to permutation testing |
| `broom` (Robinson 2014) | Yes | Post-hoc tidying of outputs; no composition at object level |
| `statsExpressions` (Patil 2021) | Yes | Tidy data frames for plotting; not a composable object class |
| `poolr` (Cinar & Viechtbauer 2022) | Yes | Comprehensive p-value combination; no composable test-object class |
| `rstatix` (Kassambara 2023) | Yes | Pipe-friendly wrappers; not a constructor API for new test types |

**Niche positioning is clear**: hypothesize is unique in providing (1) a minimal
S3 class any package can construct and return, (2) algebraic composition via
`fisher_combine()` and `adjust_pval()`, and (3) zero non-base dependencies.

**No significant competitor is missing**. The `multtest` (Bioconductor), `metap`,
and `parameters` (easystats) packages are tangentially related but focus on
different problems. Their omission is defensible.

---

## Recommended Next Steps

1. **Replace CODE_OF_CONDUCT.md placeholder** (quick fix):
   Change `[INSERT CONTACT METHOD]` on line 40 to `lex@metafunctor.com`.

2. **Submit to JOSS** via <https://joss.theoj.org/papers/new>.
   The package meets all substantive JOSS requirements.

3. *Optional improvements* (not blocking submission):
   - Add a `print.hypothesis_test` test to push coverage above 95%
   - Remove unused `@importFrom stats pf` and `@importFrom stats qt`
   - Consider adding issue/PR templates for community engagement

---

## Appendix: Verification Commands

```r
# Reproduce all checks locally
devtools::test()                    # 83 tests, 0 failures
covr::package_coverage()           # 93.75%
devtools::check()                  # 0 errors, 0 warnings, 1 NOTE (benign)
```

```bash
# Verify external resources (all return HTTP 200)
curl -sL -o /dev/null -w "%{http_code}" "https://doi.org/10.5281/zenodo.18765626"
curl -sL -o /dev/null -w "%{http_code}" "https://orcid.org/0000-0001-6443-9897"
curl -sL -o /dev/null -w "%{http_code}" "https://queelius.github.io/hypothesize/"

# Verify CI status
gh run list --repo queelius/hypothesize --limit 1
# completed  success  R-CMD-check  2026-02-26
```
