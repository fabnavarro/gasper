gasper News

# gasper 1.1.6 (02/28/2024)

-   add `SuiteSparseData` data frame mirrors the structure presented on
    the SuiteSparse Matrix Collection website. Enables now easily query
    and download specific matrices directly from the Collection.
-   add GSP intro in vignettes

# gasper 1.1.5 (10/27/2023)

-   change `b` by `beta` in `betathresh` and `SUREThresh` to avoid
    confusion with the parameter that control the number of scales in
    other functions.
-   add Level dependent SURE thresholding `LD_SUREthresh`.
-   add graph Fourier transform functions `forward_gft`/`inverse_gft`.
-   add `spectral_coords` function to compute spectral coordinates of a
    graph using the two smallest non-zero eigenvalues of the graph
    Laplacian.
-   add random walk and normalized Laplacian matrix to `laplacian_mat`
    (keep unnormalized as default).
-   add more info in doc.
-   add `localize_gft`/`localize_sgwt` fun.
-   add custom frame filters support (in `forward/inverse_sgwt`,
    `HPFVN`, `plot_filter` and `tight_frame`) by passing additional
    arguments (`filter_params`/`filter_func`).
-   add `get_graph_info` to retrieve from SuiteSparse the two/three
    tables with “Matrix Information,” “Matrix Properties” and, if
    available, “SVD Statistics”.
-   change `info` output in `dowload_graph` to `temp` to access temp dir
    instead.
-   add optional logical argument `svd` to fetch singular values is
    needed and provided.

# gasper 1.1.4 relase (08/26/2023)

-   satisfy CRAN policy (by removing external download).
-   CRAN resubmission.

# gasper 1.1.3 relase (02/11/2023)

-   fix CITATION file to work properly when the package is not
    installed.
-   fix error in `HPFVN`.

# gasper 1.1.2 relase (01/06/2023)

-   satisfy CRAN policy: ‘Packages which use Internet resources should
    fail gracefully with an informative message if the resource is not
    available or has changed (and not give a check warning nor error).’
-   CRAN resubmission.

# gasper 1.1.1 release (02/16/2021)

-   correction of an error in `laplacian_mat` function (in the case of
    full matrix input).
-   replace `graphname` by `matrixname` (matrices in the SuiteSparse are
    not necessarily adjacency matrices).
-   error correction in `SNR` fun (wrong output for matrices)
-   add Peak Signal to Noise Ratio (`PNSR`) function.

# gasper 1.1.0 release (02/09/2021)

-   fix bug in `download_graph` function for downloading graph matrix
    without coordinates.
-   add citation info issue#1.
-   bibliography of SURE paper updated in the vignettes.
-   add boolean argument allowing to export the coefficients of the
    frame after thresholding (TRUE by default) in `SURE*thresh`
    functions (FALSE save memory allocation for large graphs).
-   add a new output `dim` to `download_graph` providing a url to a
    temporary file containing the information about the considered
    matrix.
-   add new argument to `randsignal` to avoid full spectrum calculation.
-   add new import (methods, Matrix, RSpectra) to consider large sparse
    matrices.
-   modify `randsignal` to benefit from sparse computation (much
    faster).
-   add `SNR` function (to remove rwavelet import used only in
    vignettes)
-   add a new output `dim` to `download_graph` providing the numbers of
    rows, columns and numerically nonzero elements of the sparse matrix.
-   add Matrix Market files import (remove data.frame storage to save a
    lot of memory and speed-up computation) to `download_graph`.
-   add fast forward/inverse sgwt functions (`foward_sgwt` and
    `inverse_sgwt`). Faster than calculation via
    `tight_frame`/`analysis` and potentially useful when frame elements
    do not need to be calculated.
-   add graph extension of the Von Neummann variance estimator using
    finest scale coefficients (`HPFVN`).
-   add kernel tight-frame evaluation function `zetav` (to avoid
    redundancy).
-   add new `plot_filter` function (plot wavelet tight-frame functions).

# gasper 1.0.1 release (07/27/2020)

-   fix warnings bug `SUREthresh` functions.
-   adapt DESCRIPTION to meet CRAN’s expectations (i.e., omit the
    redundant “in R” from the title, use the Authors@R field and declare
    Maintainer, Authors and Contributors with their appropriate roles
    with person() calls, do not start the description with “This
    package”, package name, title or similar. Just start with “Provides
    …” or similar, do not start the description with “This package”,
    package name, title or similar).
-   optionally specify a RNG seed (for reproducible experiments) in
    swissroll.R.
-   remove `adjacency_mat` N arg.
-   CRAN resubmission.

# gasper 1.0.0 release (07/23/2020)

-   First release.
-   CRAN submission.
