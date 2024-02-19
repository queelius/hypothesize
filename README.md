`hypothesize`: Statistical Tests in R
=====================================

`hypothesize` is a simple hypothesis testing API in R. It is mostly
designed to be used by other libraries so that they can wrap their own
hypothesis tests in a consistent way.

In this library, we define the API as a set of generics, and then
provide a simple implementation of the API for the likelihood ratio
test.

Installation
------------

You can install the development version of `hypothesize` from GitHub
with:

    # install.packages("devtools")
    #devtools::install_github("queelius/hypothesize")

Load the Package
----------------

    #library(hypothesize)
    source("R/hypothesize.R")

Core Features
-------------

`hypothesize` includes the following core functions:

-   `hypothesis_test()`: Creates a hypothesis test object. You can
    create your own, since the API is defined as a set of generics. This
    one implements the API, and we also provide a simple implementation
    for the likelihood ratio test that returns this kind of object.
-   `pval()`: Extracts the p-value from a hypothesis test.
-   `dof()`: Retrieves the degrees of freedom associated with a
    hypothesis test.
-   `test_stat()`: Obtains the test statistic from the hypothesis test.
-   `is_significant_at()`: Determines if the hypothesis test is
    significant at a specified significance level.
-   `lrt_from_loglik()`: Performs a Likelihood Ratio Test based on
    log-likelihood values from nested models.

Example: Using `lrt_from_loglik`
--------------------------------

The `lrt_from_loglik` function is particularly useful for comparing
nested models — where one model (the null model) is a special case of
another (the alternative model).

### Scenario

Suppose we have two models that aim to explain the same dataset. Model 1
(the null model) is simpler, with fewer parameters, while Model 2 (the
alternative model) includes additional parameters. We wish to test if
the complexity of Model 2 is justified by a significantly better fit to
the data.

### Step-by-Step Example

1.  **Define Log-Likelihoods**: Assume we have calculated the
    log-likelihoods for both models on the same dataset. For the null
    model, the log-likelihood is -100, and for the alternative model, it
    is -99. Assume that the difference in degrees of freedom between the
    two models is 2.

2.  **Perform LRT**: We use `lrt_from_loglik` to perform the Likelihood
    Ratio Test.

<!-- -->

    # Perform LRT
    lrt <- lrt_from_loglik(null_loglik = -100, alt_loglik = -96.105, dof = 3)

1.  **Evaluate Significance**: Determine if the difference in
    log-likelihoods is significant at the 5% level.

<!-- -->

    # Check significance
    is_significant_at(lrt, 0.05)
    #> [1] FALSE

A negative test result indicates that the alternative model is not
compatible with the data at the 5% significance level. However, we might
want to extract the test statistic, p-value, and degrees of freedom to
arrive at a more nuanced interpretation.

1.  **Examine the Test Result**: Extract and examine the test statistic,
    p-value, and degrees of freedom to evaluate the significance.

<!-- -->

    # Extract test statistic
    test_stat(lrt)
    #> [1] 7.8

    # Extract p-value
    pval(lrt)
    #> [1] 0.051

    # Extract degrees of freedom
    dof(lrt)
    #> [1] 3

We see that the *p*-value is only slightly above our (arbitrarily)
specified *α* = 0.05. This suggests that the alternative model may be
reasonable to consider, but it is not a clear-cut decision. In practice,
we would likely want to consider other factors, such as the practical
significance of the additional complexity, or collecting more data to
reduce uncertainty, before making a final decision.
