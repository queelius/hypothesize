# Contributing to hypothesize

Thank you for your interest in contributing to hypothesize! This document
provides guidelines for contributing to this project.

## Reporting Bugs

Open an issue at <https://github.com/queelius/hypothesize/issues> with:

- A minimal reproducible example
- Your R version (`sessionInfo()`)
- The expected vs. actual behavior

## Suggesting Features

Open an issue describing the feature, its motivation, and how it fits with the
package's SICP-inspired design (data abstraction, closure property, higher-order
functions).

## Pull Requests

1. Fork the repository and create a feature branch
2. Follow the existing code style (S3 classes, roxygen2 documentation)
3. Add tests for new functionality in `tests/testthat/`
4. Run `devtools::check()` and ensure no errors or warnings
5. Update documentation with `devtools::document()` if needed
6. Submit a PR with a clear description of the changes

## Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "knitr", "rmarkdown"))

# Load for development
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Code of Conduct

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).
