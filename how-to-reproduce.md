This document describes the functions of the active scripts in this project



## Environment
1.      Download and install R 
2.      Download and install  RStudio
3.      Register an account on GitHub.com and email @andkov your github username if you would like to have access to the workshop notebooks for the workshop, which we will use to share scripts and organize analyses.


## Scripts

### MAP

* 0 - [`./manipulation/map/0-ellis-island-map.r`](https://github.com/IALSA/ialsa-2016-amsterdam/blob/master/manipulation/map/0-ellis-island-map.R) produces the data transfer object (dto), used as point of departure for each subsequent report. 
* 1 - [`./manipulation/map/1-encode-multistate-mmse.R`]("https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/1-encode-multistate-mmse.R",) transformes data to create the variable reflecting the cognitive state. Encode missingness and corrects for temporal structure.
* 2 - [`./manipulation/map/2-estimate-model-A.R`]("https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/2-estimate-model-A.R",) - estimates model `A` as depicted in the [`./libs/images/support/model-a-specification.jpg`][model-a-spec]. The submodels is defined 


[model-a-spec]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/libs/images/support/model-a-specification.jpg