gasper News

gasper 1.0.1.9000 dev version
=============================

-   fix bug in `download_graph` function for downloading graph matrix
    without coordinates
-   add citation info issue\#1
-   bibliography of SURE paper updated in the vignettes
-   add boolean argument allowing to export the coefficients of the
    frame after thresholding (TRUE by default) in SURE\*thresh functions
    (FALSE save memory allocation for large graphs)

gasper 1.0.1 release (07/27/2020)
=================================

-   fix warnings bug SUREthresh functions
-   adapt DESCRIPTION to meet CRAN’s expectations (i.e., omit the
    redundant “in R” from the title, use the Authors@R field and declare
    Maintainer, Authors and Contributors with their appropriate roles
    with person() calls, do not start the description with “This
    package”, package name, title or similar. Just start with “Provides
    …” or similar, do not start the description with “This package”,
    package name, title or similar).
-   optionally specify a RNG seed (for reproducible experiments) in
    swissroll.R
-   remove adjacency\_mat N arg
-   Cran resubmission

gasper 1.0.0 release (07/23/2020)
=================================

-   First release
-   Cran submission
