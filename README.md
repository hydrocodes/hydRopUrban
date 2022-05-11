# hydRopUrban: An R package for preliminary urban hydrological design
<img src="https://github.com/hydrocodes/hydRopUrban/blob/main/tutorial/hydropurban.PNG" width="200">

## 1. What is hydRopUrban?
`hydRopUrban` is an R package for preliminary hydrological design of urban drainage infraestructure. `hydRopUrban` seeks to help non-expert R users in automating calculations in urban hydrology. 

## 2. What is hydRopUrban for?
Eight main functions are implemented in `hydRopUrban`. Their applications cover the topics of empirical and deterministic urban hydrology based on wide-used theory.

The main functions are:
- `rational()`: Estimation of discharge hydrograph by the standard rational method with a triangular hydrograph with a recession of 1xTc, where Tc is the time of concentration (Mulvany, 1851; Kuichling, 1889).
- `rationalm()`: Estimation of discharge hydrograph by the modified rational method with a triangular hydrograph with a recession of 1.5xTc, and considering a storm event duration (Poertner, 1974; Smith and Lee, 1984).
- `rationalu()`: Estimation of discharge hydrograph by the universal rational method with a synthetic hydrograph for urban catchments (ASCE, 1996).
- `caquots()`: Maximal discharge by Caquot's method for serial drainage units and equivalent values for unit drainage resulting (Caquot, 1941).
- `caquotp()`: Maximal discharge by Caquot's method for parallel drainage units and equivalent values for unit drainage resulting (Caquot, 1941).
- `mcunge()`: Outflow discharge through a channel routing by the method of Muskingum-Cunge (USACE, 1991).
- `pollutant()`: Estimation of pollutegraphs, discharge and concentration by wash-off from impervious surfaces (Akan and Houghtalen, 2003).
- `pollutantp()`: Estimation of a pollutegraph by wash-off from pervious surfaces (Huber and Dickinson, 1988).

## 3. How to install hydRopUrban?
The `hydRopUrban` package must be installed from Github hydrocodes repository, following the next 2 steps.

**Step 1**: In Rstudio, install `devtools` package from CRAN

**Step 2**: In Rstudio console or on your script, please write 

```r
devtools::install_github("hydrocodes/hydRopUrban")
```
During the installation, please check in R console and skip other updates with an empty line or selecting option "None".

That’s all! Finally, do not forget call the package in your script.

Example: Using `rationalu()` function
```r
library(hydRopUrban)
rationalu(data=database, dt=0.05)
```
Please, check tutorial folder for codelines examples and more details:
https://github.com/hydrocodes/hydRopUrban/tree/main/tutorial

## 4. Credits
`hydRopUrban` was developed by Pedro Rau at Water Research and Technology Center of Universidad de Ingenieria y Tecnologia (UTEC-CITA, Lima) . For any issue or suggestion please write to: pedro.rau.ing@gmail.com

## 5. Versions

v 1.2 - January 25, 2022

v 1.1 - October 06, 2021

v 1.0 - August 11, 2021

## 6. How to cite?

Rau, P. 2021. hydRopUrban: An R package for preliminary urban hydrological design. Github repository: https://github.com/hydrocodes/hydRopUrban

## 7. References

Akan, A.O., Houghtalen, R.J. (2003). Urban hydrology, hydraulics and stormwater quality. John Wiley & Sons.

ASCE (1996). Hydrology Handbook. ASCE Manual of Practice No. 28. New York, NY.

Caquot, A. (1941). Sur la quantite des eaux pluviales à écouler dans les agglomerations urbaines modernes. Comptes Rendus hebdomadaires des Séances de l’Académie des Sciences, 213(16), 509–515.

Huber, W.C., Dickinson, R.E. (1988). Stormwater Management Model. User’s Manual, Environmental Research Laboratory, EPA, USA.

Kuichling, E. (1889). The relation between the rainfall and the discharge of sewers in populous districts, Trans., ASCE, Vol. 20, pp. 1-60.

Mulvany, T.J. (1851). On the use of self-registering rain and flood gauges in making observations of the relations of rain fall and of flood discharges in a given catchment, Proceedings of Institution of Civil Engineers of Ireland, 4, 18-33.

Poertner, H.G. (1974). Practices in Detention of Urban Stormwater Runoff. APWA Special Report No. 43. Washington, D. C.: American Public Works Association.

Smith, A.A., Lee, K. (1984). The rational method revisited. Canadian Journal of Civil Engineering11(4): 854–862.

USACE (1991). A Muskingum-Cunge channel flow routing method for drainage networks. TP-135.
