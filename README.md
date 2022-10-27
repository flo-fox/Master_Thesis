# Master Thesis: A Causal Test of the "Law of 1/n" and its Mechanisms

## Root directory

This GitHub repository contains the `.Rmd` files used to analyze the data set as well as the output files.

In the root directory, you will find the `renv` as well as `.rproj` files. Importantly, the Thesis itself is available here, along with the preparatory project study.

You also find the analysis `.Rmd` files here:

- The descriptive analysis, `Descriptives.Rmd`, used in section 5.1 of the Thesis.
- The validity analysis, `Analysis_RDD_Normalized_Cutoff_Validity.Rmd`, used in chapter 6.
- The "conventional" study, `Analysis_Conventional_Regressions.Rmd`, used in section 7.1.
- Various plot, `Plots.Rmd`, used in section 7.2.
- The main analysis of the overall "Law of 1/n", `Analysis_RDD_Normalized_Cutoff.Rmd`, used in section 7.3 and 7.4.
- The heterogeneity-evaluating analysis, `Analysis_RDD_Normalized_Cutoff_Heterogeneity.Rmd`, used in chapter 8.
- Unsuccessful attempts at exploiting the multi-cutoff setting directy, `Analysis_RDD_Multicutoff.Rmd`.

## Repo folders

The repo contains the following folders:

- `data` (not visible): Contains the data sets.
- `functions`: Contains functions and settings loaded into many `.Rmd` files.
- `plots`: Contains the plots generated. 16 of the 22 `pdf` plots are used in the Thesis.
- `results`: The outputs corresponding to the `.Rmd` files listed above.
- `renv`: Folder for `renv` package files.
- `tables`: Find the `.tex` tables output by the analysis. Those ending with `_mod.tex` are modified from their similar-named counterparts. These counterparts (obviously not ending with `_mod.tex`) are directly exported from R.
- `tables_robustness_checks`: Since the code for the nonparametric regression takes quite some time to run, its results are stored in tables to speed up code execution. Despite its name, it does not only include robustness-check tables.

## Data

Data sources:

- State-statistical offices
- [Regionaldatenbank Deutschland](https://www.regionalstatistik.de/genesis/online), Federal Statistical Office of Germany
- Federal Employment Agency

Data available upon request.

## Packages

Main packages used (excluding dependencies):

- `fixest`
- `ggcorrplot`
- `ggpubr`
- `haven`
- `htmltools`
- `janitor`
- `latex2exp`
- `plotly`
- `rdd`
- `rddensity`
- `RDHonest`
- `rdrobust`
- `rdmulti`
- `reshape2`
- `scales`
- `tidyverse`
- `xtable`

For details, refer to the `renv` files.
