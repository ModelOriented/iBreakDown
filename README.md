[![Build Status](https://api.travis-ci.org/ModelOriented/breakDown2.png)](https://travis-ci.org/ModelOriented/breakDown2)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/breakDown2/master.svg)](https://codecov.io/github/ModelOriented/breakDown2?branch=master)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/ModelOriented/breakDown2/master?filepath=jupyter-notebook)

# Break Down: Model Agnostic Explainers for Individual Predictions

**breakDown2** is an experimental version of the [breakDown](https://github.com/pbiecek/breakDown) package. 
Expect rapid changes.

The `breakDown2` package is a model agnostic tool for decomposition of predictions from black boxes.
Break Down Table shows contributions of every variable to a final prediction. 
Break Down Plot presents variable contributions in a concise graphical way. 
This package works for binary classifiers and general regression models. 

It's a part of [DrWhy](https://github.com/ModelOriented/DrWhy) collection of tools.

Find lots of R examples at `breakDown2` website: https://ModelOriented.github.io/breakDown2/

This version works also with D3! 
[see an example](https://modeloriented.github.io/breakDown2/articles/vignette_breakDown2_titanic.html#plot-attributions-with-d3).
![plotD3](images/plotD3.png)

## Installation

Install from GitHub

```
devtools::install_github("ModelOriented/breakDown2")
```
