iBreakDown 0.9.6
----------------------------------------------------------------
* Function `local_attributions_uncertainty()` now supports `path = "average"` argument and plots shapley values  ([#29](https://github.com/ModelOriented/iBreakDown/issues/29)). 
* Function `local_attributions_uncertainty()` is renamed to  `break_down_uncertainty()`.
* Function `shap()` is an alias for  `break_down_uncertainty()`.


iBreakDown 0.9.5
----------------------------------------------------------------
* methodology behind `iBreakDown` is described on arXiv and linked in the CITATION

iBreakDown 0.9.4
----------------------------------------------------------------
* code refactoring
* added `local_attributions_uncertainty()` function that measures uncertanity behind additive attributions

iBreakDown 0.9.3
----------------------------------------------------------------
* `breakDown2` has changed name to `iBreakDown`

iBreakDown 0.9.2
----------------------------------------------------------------
* in the `local_attributions()` function user can force order in which variables shall be presented

iBreakDown 0.9.1
----------------------------------------------------------------
* added `plotD3()` function, that uses `r2d3` package to generate interactive D3 plots

iBreakDown 0.9.0
----------------------------------------------------------------
* Greedy strategies with time complexity O(p^2) are removed.
* `iBreakDown` is forked from `breakDown` version 0.2.0
