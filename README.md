# dimeoeryilmaz24
Codes for The Impacts of Health Shocks on Household Labor Supply and Domestic Production (2025)

****************
# DATA
- We use data from the German Socio-Economic Panel (SOEP) version SOEP-Core V37 (DOI: 10.5684/soep.core.v37eu). Data can be requested at https://www.diw.de/en/diw_01.c.601584.en/data_access.html
- The repository contains and XLSX spread sheet with conversions rate for monetary values, normalized to 2020 euros. The metadata for these conversion rates are also in the XLSX file.
- Source data for appendix A3 are in corresponding subfolder

***************
# CODES
- Boradly speaking, data preparation is performed in STATA, while analyses are carried on via R.
- Codes for main paper are to be run in the given order.
- Codes for the appendix can be run independently once the main datasets are generated.
- You need to define three directories: one for the raw SOEP files, one for the saved datasets and the directory for the results.
- The file conversion_rates.xlsx needs to be in the same folder as the raw files.

***************
# R PACKAGE VERSIONS
- Some results may be sensitive to the package version of the method. In particular, the did package for the Callaway and Sant'Anna (2021) estimator receives recurrent updates.

R version 4.4.1 (2024-06-14 ucrt)
Running under: Windows 11 x64 (build 22631)

attached base packages:

stats     
graphics  
grDevices 
utils     
datasets  
methods   
base     

other attached packages:

HonestDiD_0.2.6  
data.table_1.16.0  
tictoc_1.2.1  
furrr_0.3.1  
future_1.34.0  
readxl_1.4.3  
lfe_3.0-0  
Matrix_1.7-0  
plm_2.6-4  
here_1.0.1  
cowplot_1.1.3  
Cairo_1.6-2  
extrafont_0.19  
foreign_0.8-87  
xtable_1.8-4  
lubridate_1.9.3  
forcats_1.0.0  
stringr_1.5.1  
dplyr_1.1.4  
purrr_1.0.2  
readr_2.1.5  
tidyr_1.3.1  
tibble_3.2.1  
ggplot2_3.5.1  
tidyverse_2.0.0  
did_2.2.0.903  
haven_2.5.4  

loaded via a namespace (and not attached):

gtable_0.3.5  
rstatix_0.7.2  
collapse_2.0.16  
lattice_0.22-6  
numDeriv_2016.8-1.1  
tzdb_0.4.0  
vctrs_0.6.5  
tools_4.4.1  
Rdpack_2.6.1  
generics_0.1.3  
sandwich_3.1-1  
parallel_4.4.1  
fansi_1.0.6  
pkgconfig_2.0.3  
stringmagic_1.1.2  
lifecycle_1.0.4  
compiler_4.4.1  
maxLik_1.5-2.1  
munsell_0.5.1  
codetools_0.2-20  
carData_3.0-5  
Rttf2pt1_1.3.12  
Formula_1.2-5  
pillar_1.9.0  
car_3.1-2  
ggpubr_0.6.0  
extrafontdb_1.0  
MASS_7.3-61  
abind_1.4-8  
parallelly_1.38.0  
nlme_3.1-166  
digest_0.6.37  
tidyselect_1.2.1  
bdsmatrix_1.3-7  
stringi_1.8.4  
listenv_0.9.1  
miscTools_0.6-28  
rprojroot_2.0.4  
grid_4.4.1  
colorspace_2.1-1  
cli_3.6.3  
magrittr_2.0.3  
utf8_1.2.4  
broom_1.0.6  
dreamerr_1.4.0  
withr_3.0.1  
scales_1.3.0  
backports_1.5.0  
timechange_0.3.0  
globals_0.16.3  
cellranger_1.1.0  
ggsignif_0.6.4  
zoo_1.8-12  
hms_1.1.3  
rbibutils_2.2.16  
fixest_0.12.1  
lmtest_0.9-40  
rlang_1.1.4  
Rcpp_1.0.13  
glue_1.7.0  
BMisc_1.4.6  
rstudioapi_0.16.0  
R6_2.5.1     
