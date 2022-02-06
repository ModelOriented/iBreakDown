# Model Agnostic Local Attributions  <img src="man/figures/logo.png" align="right" width="150"/>

[![R build status](https://github.com/ModelOriented/iBreakDown/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/iBreakDown/actions?query=workflow%3AR-CMD-check)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/iBreakDown/master.svg)](https://codecov.io/github/ModelOriented/iBreakDown?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/iBreakDown)](https://cran.r-project.org/package=iBreakDown)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/iBreakDown?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/iBreakDown)

## Overview

The `iBreakDown` package is a model agnostic tool for explanation of predictions from black boxes ML models.
Break Down Table shows contributions of every variable to a final prediction. 
Break Down Plot presents variable contributions in a concise graphical way. 
SHAP (Shapley Additive Attributions) values are calculated as average from random Break Down profiles.
This package works for binary classifiers as well as regression models. 

`iBreakDown` is a successor of the [breakDown](https://github.com/pbiecek/breakDown) package. It is faster (complexity `O(p)` instead of `O(p^2)`). It supports variable interactions and interactive explanations with D3.js visualizations. It is imported and used to compute model explanations in multiple packages e.g. [`DALEX`](https://github.com/ModelOriented/DALEX), [`modelStudio`](https://github.com/ModelOriented/modelStudio), [`arenar`](https://github.com/ModelOriented/ArenaR).

Methodology behind the **iBreakDown** package is described in the [arXiv paper](https://arxiv.org/abs/1903.11420) and [Explanatory Model Analysis](https://ema.drwhy.ai/breakDown.html) book. It is a part of [DrWhy.AI](http://DrWhy.AI) universe. 

## Installation

```{r}
# the easiest way to get iBreakDown is to install it from CRAN:
install.packages("iBreakDown")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("ModelOriented/iBreakDown")
```

## Learn more

Find more examples in the EMA book: https://ema.drwhy.ai/.

This version also works with **D3**:
[see an example](https://modeloriented.github.io/iBreakDown/prototypeDemo.html) and [demo](https://modeloriented.github.io/iBreakDown/articles/vignette_iBreakDown_titanic.html#plot-attributions-with-d3).

![plotD3](man/figures/plotD3.png)

## Acknowledgments

Work on this package was financially supported by the `NCN Opus grant 2016/21/B/ST6/02176`.
