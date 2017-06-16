### Modeling Evapotranspiration in ECHSE

Using the Eco-Hydrological Simulation Environment (ECHSE) for modeling evapotranspiration fluxes in semi-arid regions.
**Code**, **data**, and **miscellanea**.

Original repositories of ECHSE [here](https://github.com/echse).
Repository of ECHSE with added evapotranspiration methods: [code](https://github.com/tpilz/echse_engines), [documentation](https://github.com/tpilz/echse_doc).

#### Shortened project tree:

* [data/](./data/)
  * [morocco/](./data/morocco/)
    * ...
  * [portugal/](./data/portugal/)
    * [climatedata.dat](./data/portugal/climatedata.dat) &mdash; _climate stats for study site 2011, 2012_
    * COR_ZM.RDA &mdash; _eddy tower data_
    * [forest_inventory.xls](./data/portugal/forest_inventory.xls)
    * Kdown &mdash; _SW downward radiation at stations HS & NSA_
    * Kup &mdash; _SW upward radiation ..._
    * Ldown &mdash; _LW downward radiation ..._
    * Lup &mdash; _LW upward radiation ..._
    * [metadata.dat](./data/portugal/metadata.dat) &mdash; _some info about units in data files_
    * meteo_HS.Rdata &mdash; _meteo data at field station HS_
    * meteo_NSA.Rdata &mdash; _meteo data at field station NSA_
    * [readme_COR1416_ZM.txt](./data/portugal/readme_COR1416_ZM.txt) &mdash; _meta data about eddy tower data_
    * [soildata.dat](./data/portugal/soildata.dat)
    * [soil_moisture_HS.txt](./data/portugal/soil_moisture_HS.txt) &mdash; _created from_ [tms.R](./R/tms.R)
    * [soil_moisture_NSA.txt](./data/portugal/soil_moisture_NSA.txt) &mdash; _ditto_
    * [table1.png](./data/portugal/table1.png) &mdash; _original table containing climate, soil, and veg data_
    * [vegdata.dat](./data/portugal/vegdata.dat)
* [R/](./R/) &mdash; _R scripts_
  * [albedoSin.R](./R/albedoSin.R) &mdash; _function writing sinusoidal albedo input file_
  * [echse_calibration.R](./R/echse_calibration.R) &mdash; _script for estimating parameters through calibration_
  * [echse_check_functions.R](./R/echse_check_functions.R) &mdash; _script for testing some functions in ECHSE engine_
  * [echse_morocco.R](./R/echse_morocco.R) &mdash; _main script for executing ECHSE model runs, Morocco_
  * [echse_portugal.R](./R/echse_portugal.R) &mdash; _main script for pre-processing & executing ECHSE model runs, Portugal_
  * [echseCtrl.R](./R/echseCtrl.R) &mdash; _functions writing ECHSE control files_
  * [echseDepend.R](./R/echseDepend.R) &mdash; _function printing dependencies of variables & parameters_
  * [echseInput.R](./R/echseInput.R) &mdash; _function writing data input files_
  * [echseParEst.R](./R/echseParEst.R) &mdash; _function estimating model parameters from observations_
  * [echsePost.R](./R/echsePost.R) &mdash; _function for running models & post-processing of results_
  * [echsePre.R](./R/echsePre.R) &mdash; _functions needed before data pre-processing_
  * [tms.R](./R/tms.R) &mdash; _script for soil moisture calculation_
* [results/](./results/) &mdash; _plots of simulation results_
  * ...
* [sensitivity/](./sensitivity/) &mdash; _plots from simple sensitivity analysis_
  * ...
* [tms_portugal/](./tms_portugal) &mdash; _data for and results from soil moisture calculation_
  * ...
* [decision_tree_maidment.pdf](./decision_tree_maidment.pdf) &mdash; _Selection & Computation Sequence for Estimating Energy, from Maidment (1993)._
