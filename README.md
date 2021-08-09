# hydRopUrban: An R package for easy calculations in urban hydrology
<img src="https://github.com/hydrocodes/hydRopUrban/blob/main/tutorial/hydropurban.PNG" width="200">

## 1. What is hydRopUrban?
`hydRopUrban` is an R package for automating calculations in urban hydrology. `hydRopUrban` seeks to helping non-expert R users in the preliminar hydrological design of urban drainage infraestructure. 

## 2. What is hydRopUrban for?
Six main functions are implemented in `hydRopUrban`. Their applications cover the topics of empirical and deterministic urban hydrology based on wide-used theory.

The main functions are:
- `rational()`: Estimation of discharges time series by the standard rational method with a triangular hydrograph with a recession of 1xTc, without considering a storm duration.
- `rationalm()`: Estimation of discharges time series by the modified rational method with a triangular hydrograph with a recession of 1.5xTc, and considering a storm event duration.
- `rationalu()`: Estimation of discharges time series by the universal rational method with a synthetic hydrograph for urban catchments.
- `caquots()`: Maximal discharge by Caquot's method for serial drainage units and equivalent values for unit drainage resulting.
- `caquotp()`: Maximal discharge by Caquot's method for parallel drainage units and equivalent values for unit drainage resulting.
- `mcunge()`: Outflow discharge through a channel routing by the method of Muskingum-Cunge.

## 3. Credits
`hydRopUrban` was developed by Pedro Rau. For any issue or suggestion please write to: pedro.rau.ing@gmail.com

## 4. Versions

v 0.1.0 - August 9, 2021 (test version)

## 5. How to cite?

Rau, P. 2021. hydRopUrban: An R package for easy calculations in urban hydrology. Github repository: https://github.com/hydrocodes/hydRopUrban
