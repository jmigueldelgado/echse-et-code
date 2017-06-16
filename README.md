### Modeling Evapotranspiration in ECHSE

Using the Eco-Hydrological Simulation Environment (ECHSE) for modeling evapotranspiration fluxes in semi-arid regions.
**Code**, **data**, and **miscellanea**.

Original repositories of ECHSE [here](https://github.com/echse).
Repository of ECHSE with added evapotranspiration methods: [code](https://github.com/tpilz/echse_engines), [documentation](https://github.com/tpilz/echse_doc).

#### Shortened project tree:

* data/
  * morocco/
  * portugal/
    * climatedata.dat    _climate stats for study site 2011, 2012_
    * COR_ZM.RDA    _eddy tower data_
    * forest_inventory.xls
    * Kdown    _SW downward radiation at stations HS & NSA_
    * Kup    _SW upward radiation ..._
    * Ldown    _LW downward radiation ..._
    * Lup    _LW upward radiation ..._
    * metadata.dat    _some info about units in data files_
    * meteo_HS.Rdata    _meteo data at field station HS_
    * meteo_NSA.Rdata    _meteo data at field station NSA_
    * readme_COR1416_ZM.txt    _meta data about eddy tower data_
    * soildata.dat
    * soil_moisture_HS.txt    _created from_ [tms.R](./R/tms.R)
    * soil_moisture_NSA.txt    _ditto_
    * table1.png    _original table containing climate, soil, and veg data_
    * vegdata.dat
* R/    _R scripts_
  * albedoSin.R    _function writing sinusoidal albedo input file_
  * echse_calibration.R    _script for estimating parameters through calibration_
  * echse_check_functions.R    _script for testing some functions in ECHSE engine_
  * echse_morocco.R    _main script for executing ECHSE model runs, Morocco_
  * echse_portugal.R    _main script for pre-processing & executing ECHSE model runs, Portugal_
  * echseCtrl.R    _functions writing ECHSE control files_
  * echseDepend.R    _function printing dependencies of variables & parameters_
  * echseInput.R    _function writing data input files_
  * echseParEst.R    _function estimating model parameters from observations_
  * echsePost.R    _function for running models & post-processing of results_
  * echsePre.R    _functions needed before data pre-processing_
  * tms.R    _script for soil moisture calculation_
* results/    _plots of simulation results_
  * ...
* sensitivity/    _plots from simple sensitivity analysis_
  * ...
* tms_portugal/    _data for and results from soil moisture calculation_
  * ...
* decision_tree_maidment.pdf    _Selection & Computation Sequence for Estimating Energy, from Maidment (1993)._
