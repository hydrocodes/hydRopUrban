---
output: github_document
---

# *hydRopUrban*: An R package for preliminary urban hydrological design <img src="https://github.com/hydrocodes/hydRopUrban/blob/main/tutorial/hydropurban.PNG" alt="logo" style="float:right;" width="200"/>

<!-- badges: start -->

[![Project Status: Active -- The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![CRAN](http://www.r-pkg.org/badges/version/hydRopUrban)](https://CRAN.R-project.org/package=hydRopUrban) [![License](https://img.shields.io/badge/license-GPL%20(%3E=%202)-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

<!-- badges: end -->

## 1. What is hydRopUrban?

`hydRopUrban` is an R package for preliminary hydrological design of urban drainage infraestructure. `hydRopUrban` seeks to help non-expert R users in automating calculations in urban hydrology.

## 2. What is hydRopUrban for?

Six main functions are implemented in `hydRopUrban`. Their applications cover the topics of empirical and deterministic urban hydrology based on wide-used theory.

The main functions are: - `rational()`: Estimation of discharge hydrograph by the standard rational method with a triangular hydrograph with a recession of 1xTc, where Tc is the time of concentration (Mulvany, 1851; Kuichling, 1889), by the modified rational method considering a storm event duration (Poertner, 1974; Smith and Lee, 1984), and by the universal rational method with a synthetic hydrograph for urban catchments (ASCE, 1996). - `caquot()`: Maximal discharge by Caquot's method for serial and parallel drainage units and equivalent values for unit drainage resulting (Caquot, 1941). - `routing()`: Outflow discharge through a channel routing by the method of Muskingum-Cunge (USACE, 1991), and the Convex method (USDA-SCS, 1965). - `pollutant()`: Estimation of pollutegraphs, discharge and concentration by wash-off from impervious surfaces (Akan and Houghtalen, 2003), and by wash-off from pervious surfaces (Huber and Dickinson, 1988). - `idf()`: Estimation of Intensity-Duration-Frequency adjustment from SCS storms (USDA, 1986). - `lagtime()`: Estimation of lag times and time of concentration by 18 empirical methods (Gericke et al, 2014).

## 3. How to install hydRopUrban?

The `hydRopUrban` package must be installed from Github hydrocodes repository, following the next 2 steps.

**Step 1**: In Rstudio, install `devtools` package from CRAN

**Step 2**: In Rstudio console or on your script, please write

``` r
devtools::install_github("hydrocodes/hydRopUrban")
```

During the installation, please check in R console and skip other updates with an empty line or selecting option "None".

That's all! Finally, do not forget call the package in your script.

Example: Using `rational()` function

``` r
library(hydRopUrban)
rational(crunoff = 0.8, intensity = 1.7, area = 0.512,
                  time.con = 0.91, delta.time = 0.05, duration =  2, method = 'modified', path = 'C:/')
```

Please, check tutorial folder for codelines examples and more details: <https://github.com/hydrocodes/hydRopUrban/tree/main/tutorial>

## 4. Credits

`hydRopUrban` was developed by Pedro Rau and Leonardo Gutierrez at Water Research and Technology Center of Universidad de Ingenieria y Tecnologia (UTEC-CITA, Lima) . For any issue or suggestion please write to: [pedro.rau.ing\@gmail.com](mailto:pedro.rau.ing@gmail.com){.email}

## 5. Versions

v 1.0 - November 18, 2022

v 0.4 - June 1, 2022

v 0.3 - January 25, 2022

v 0.2 - October 6, 2021

v 0.1 - August 11, 2021

## 6. How to cite?

Rau, P., Gutierrez, L., Callan, N. A tool in R for preliminary urban hydrological design. (submitted)

## 7. References

Akan, A.O., Houghtalen, R.J. (2003). Urban hydrology, hydraulics and stormwater quality. John Wiley & Sons.

ASCE (1996). Hydrology Handbook. ASCE Manual of Practice No. 28. New York, NY.

Caquot, A. (1941). Sur la quantite des eaux pluviales à écouler dans les agglomerations urbaines modernes. Comptes Rendus hebdomadaires des Séances de l'Académie des Sciences, 213(16), 509--515.

Gericke, O., Smithers J. (2014). Review of methods used to estimate catchment response time for the purpose of peak discharge estimation. 59(11):1935-1971.

Huber, W.C., Dickinson, R.E. (1988). Stormwater Management Model. User's Manual, Environmental Research Laboratory, EPA, USA.

Kuichling, E. (1889). The relation between the rainfall and the discharge of sewers in populous districts, Trans., ASCE, Vol. 20, pp. 1-60.

Mulvany, T.J. (1851). On the use of self-registering rain and flood gauges in making observations of the relations of rain fall and of flood discharges in a given catchment, Proceedings of Institution of Civil Engineers of Ireland, 4, 18-33.

Poertner, H.G. (1974). Practices in Detention of Urban Stormwater Runoff. APWA Special Report No. 43. Washington, D. C.: American Public Works Association.

Smith, A.A., Lee, K. (1984). The rational method revisited. Canadian Journal of Civil Engineering11(4): 854--862.

USACE (1991). A Muskingum-Cunge channel flow routing method for drainage networks. TP-135.

USDA-SCS (1986). Urban hydrology for small watersheds, tech release No 55.

USDA-SCS (1965). TR-20, Computer program for project formulation-Hydrology, Washington DC.
