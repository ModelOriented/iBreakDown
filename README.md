[![Build Status](https://api.travis-ci.org/ModelOriented/iBreakDown.png)](https://travis-ci.org/ModelOriented/iBreakDown)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/iBreakDown/master.svg)](https://codecov.io/github/ModelOriented/iBreakDown?branch=master)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/ModelOriented/iBreakDown/master?filepath=jupyter-notebooks/)

# iBreakDown: Model Agnostic Explainers for Individual Predictions

The **iBreakDown** package is a model agnostic tool for explanation of predictions from black boxes ML models.
Break Down Table shows contributions of every variable to a final prediction. 
Break Down Plot presents variable contributions in a concise graphical way. 
This package works for binary classifiers as well as regression models. 

**iBreakDown** is a successor of the [breakDown](https://github.com/pbiecek/breakDown) package. It is faster (complexity O(p) instead of O(p^2)). It supports interactions and interactive explainers with D3.js plots.

It is a part of [DrWhy.AI](http://DrWhy.AI) collection of tools fo XAI.

Find lots of R examples at `iBreakDown` website: https://ModelOriented.github.io/iBreakDown/

Methodology behind the **iBreakDown** package is described in the [arxiv paper](add link) and [VEEDD book](https://pbiecek.github.io/PM_VEE/breakDown.html).

This version works also with D3! 
[see an example](https://modeloriented.github.io/iBreakDown/articles/vignette_iBreakDown_titanic.html#plot-attributions-with-d3).
![plotD3](images/plotD3.png)

## Installation

Install from GitHub

```
devtools::install_github("ModelOriented/iBreakDown")
```
