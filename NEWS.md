iBreakDown 1.3.1
---------------------------------------------------------------
* fix issue with text coliding with rectangle [#85](https://github.com/ModelOriented/iBreakDown/issues/85)

iBreakDown 1.2.1
---------------------------------------------------------------
* fix the intercept bar color when `baseline` is used (now it's blue)
* add `max_vars` alias to the plots (unification)

iBreakDown 1.2.0
---------------------------------------------------------------
* fix `shap` sign column [#82](https://github.com/ModelOriented/iBreakDown/issues/82)
* `DALEX` is moved to Suggests as in  [#84](https://github.com/ModelOriented/iBreakDown/issues/84)

iBreakDown 1.1.1
---------------------------------------------------------------
* doc fix for `CRAN`

iBreakDown 1.1.0
----------------------------------------------------------------
* `plot` and `plotD3` work now   [#77](https://github.com/ModelOriented/iBreakDown/issues/77)
* fix `xgboost` variable values in `break_down_uncertainty()` [#76](https://github.com/ModelOriented/iBreakDown/issues/76)
* depend on `R v3.5` to comply with `DALEX`
* `plot` has now `title` and `subtitle` arguments   [#67](https://github.com/ModelOriented/iBreakDown/issues/67)

iBreakDown 1.0
----------------------------------------------------------------
* Argument `vnames` added to `plot.break_down` to change freely labels 
* change `cummulative` to `cumulative` [#72](https://github.com/ModelOriented/iBreakDown/issues/72)
* add `max_features` to `plot.break_down_uncertainty()`

iBreakDown 0.9.9
----------------------------------------------------------------
* Description of break_down explainer added
* Description of break_down_uncertainty explainer added
* plotD3 for shap added
* chose first row when passing more than one `new_observation`

iBreakDown 0.9.8
----------------------------------------------------------------
* New version of D3 plots in `plotD3()`.
* Updated color palettes consistent with changes in DALEX 0.4.4

iBreakDown 0.9.6
----------------------------------------------------------------
* Function `local_attributions_uncertainty()` now supports `path = "average"` argument and plots shapley values  ([#29](https://github.com/ModelOriented/iBreakDown/issues/29)). 
* Function `local_attributions_uncertainty()` is renamed to  `break_down_uncertainty()`.
* Function `shap()` is an alias for  `break_down_uncertainty()`.
* binder is removed as it was not used.


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
