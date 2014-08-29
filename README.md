GDopp
============
[![Build status](https://ci.appveyor.com/api/projects/status/33liqfsmas7dee49)](https://ci.appveyor.com/project/jread-usgs/gdopp)  
`GDopp` is short for 'gas from doppler'. A set of tools to process Acoustic Doppler Velocimeter data into estimates of gas exchange.


###Installing `GDopp`
install this package using
```
  install.packages("GDopp", 
        repos = c("http://usgs-r.github.com", "http://cran.us.r-project.org"),
        dependencies = TRUE, type = "both")
```
###`GDopp` overview
The `GDopp` package was created support the autmated analysis Acoustic Doppler Velocimeter data in order to estimate turbulence metrics that can be used to parameterize gas exchange. This package is in active development, so features are expected to change and be added in the near future. At present, the package is designed to interact with data from the Nortek family of ADVs, so the file importing features (see `load_adv` and `load_sen`) are limited to those file types. 

`GDopp` Functions (as of v0.1.0)
=====

| Function       | Title           |
| ------------- |:-------------|
| `check_adv` | performs user-specified checks on adv data |
| `coord_transform`	| coordinate transformation |
| `epsilon_to_k`	| convert epsilon to k600 |
| `fit_epsilon`	| fit epsilon from block of ADV data |
| `get_adv_checks`	| returns a list of possible check functions for the adv |
| `get_kin_viscosity`	| get kinematic viscosity |
| `get_metadata`  | get measurement details |
| `load_adv`	| load ADV data |
| `load_sen`	| load sensor diagnostic data |
| `temp_calc`	| block-averaged temperature |
| `time_calc`	| block-averaged time |
| `velocity_calc`	| gets along-stream mean velocity |
| `window_adv`	| window ADV data |
| `window_coord`	| block-averaged coordinate shift |

##What libraries does `GDopp` need?
This version requires `oce`. This package is available on CRAN, and will be installed automatically when using the `install.packages()` instructions above.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

