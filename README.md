`hypothesize`: Statistical Tests in R
=====================================

`hypothesize` is a simple hypothesis testing API in R.
It is mostly designed to be used by other libraries so that they can wrap
their own hypothesis tests in a consistent way.

We define the API as a set of generic methods. We also
provide implementations for the likelihood ration test (LRT) and the Wald test.

Installation
------------

You can install the development version of `hypothesize` from GitHub
with:

    # install.packages("devtools")
    devtools::install_github("queelius/hypothesize")

Load the Package
----------------

    library(hypothesize)

The `hypothesize` API
---------------------

`hypothesize` defines an API for retrieving hypothesis test results. An object
satisfies the concept of a hypothesis test if it implements the following generic
methods:

-   `pval()`: Extracts the p-value from an object that models a hypothesis test.

-   `dof()`: Retrieves the degrees of freedom associated with a
    hypothesis test.

-   `test_stat()`: Obtains the test statistic from the hypothesis test.

-   `is_significant_at()`: Determines if the hypothesis test is
    significant at a specified significance level.

Implementation: `hypothesis_test`
---------------------------------

We provide an implementations for `hypothesize`. It it has a
constructor that takes a statistical test (stat), p-value (p.value),
a degree-of-freedom (dof), and optionally a list of superclasses and
any additional arguments that will be passed into the object. Here
is its type signature:

    `hypothesis_test <- function(stat, p.value, dof, superclasses = NULL, ...) `

It creates a `hypothesis_test` object that implements all of the generic
methods required by `hypothesize`. The `hypothesis_test` object also
implements `print` for summary outputs.

We use this constructor for two tests we implement, the LRT and Wald tests:

-   `lrt()`: Performs a Likelihood Ratio Test based on log-likelihood
    values from nested models.

-   `wald_test()`: Performs a Wald test to compare a parameter estimate
    to a specified value.

Example: Using `lrt`
--------------------

The `lrt` function is particularly useful for comparing nested models —
where one model (the null model) is a special case of another (the
alternative model).

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

2.  **Perform LRT**: We use `lrt` to perform the Likelihood Ratio Test.

<!-- -->

    # Perform LRT
    stat <- lrt(null_loglik = -100, alt_loglik = -96.105, dof = 3)
    print(stat)
    #> Hypothesis test ( likelihood_ratio_test )
    #> -----------------------------
    #> Test statistic:  7.79 
    #> P-value:  0.0506 
    #> Degrees of freedom:  3 
    #> Significant at 5% level:  FALSE

We show the output of the `stat` object, which includes all the relevant
information about the test. However, we might want to look at its parts
independently, particularly if we need programmatic accees to relevant
parts of the test.

1.  **Evaluate Significance**: Determine if the difference in
    log-likelihoods is significant at the 5% level.

<!-- -->

    # Check significance
    is_significant_at(stat, 0.05)
    #> [1] FALSE

A negative test result indicates that the alternative model is not
compatible with the data at the 5% significance level. However, we might
want to extract the test statistic, p-value, and degrees of freedom to
arrive at a more nuanced interpretation.

1.  **Examine the Test Result**: Extract and examine the test statistic,
    p-value, and degrees of freedom to evaluate the significance.

<!-- -->

    # Extract test statistic
    test_stat(stat)
    #> [1] 7.79

    # Extract p-value
    pval(stat)
    #> [1] 0.0506

    # Extract degrees of freedom
    dof(stat)
    #> [1] 3

We see that the *p*-value is only slightly above our (arbitrarily)
specified *α* = 0.05. This suggests that the alternative model may be
reasonable to consider, but it is not a clear-cut decision. In practice,
we would likely want to consider other factors, such as the practical
significance of the additional complexity, or collecting more data to
reduce uncertainty, before making a final decision.

Example: Using Wald Test
------------------------

The Wald test is also implemented in `hypothesize`. Tis test is used to
compare the value of a parameter to a specified value, and is often used
in the context of regression models.

    # Example: Wald Test
    print(wald_test(estimate = 1.5, se = 0.5, null_value = 1))
    #> Hypothesis test ( wald_test )
    #> -----------------------------
    #> Test statistic:  1 
    #> P-value:  0.317 
    #> Degrees of freedom:  1 
    #> Significant at 5% level:  FALSE
