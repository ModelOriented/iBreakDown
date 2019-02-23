[![Build Status](https://api.travis-ci.org/ModelOriented/iBreakDown.png)](https://travis-ci.org/ModelOriented/iBreakDown)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/iBreakDown/master.svg)](https://codecov.io/github/ModelOriented/iBreakDown?branch=master)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/ModelOriented/iBreakDown/master?filepath=jupyter-notebooks/)

# Break Down: Model Agnostic Explainers for Individual Predictions

**iBreakDown** is an experimental version of the [breakDown](https://github.com/pbiecek/breakDown) package. 
Expect rapid changes.

The `iBreakDown` package is a model agnostic tool for decomposition of predictions from black boxes.
Break Down Table shows contributions of every variable to a final prediction. 
Break Down Plot presents variable contributions in a concise graphical way. 
This package works for binary classifiers and general regression models. 

It's a part of [DrWhy](https://github.com/ModelOriented/DrWhy) collection of tools.

Find lots of R examples at `iBreakDown` website: https://ModelOriented.github.io/iBreakDown/

This version works also with D3! 
[see an example](https://modeloriented.github.io/iBreakDown/articles/vignette_iBreakDown_titanic.html#plot-attributions-with-d3).
![plotD3](images/plotD3.png)

## Installation

Install from GitHub

```
devtools::install_github("ModelOriented/iBreakDown")
```
