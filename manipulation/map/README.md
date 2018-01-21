This folder contains manipulation and estimation scripts of the study, after which it was named. 

## Chain of custody

![chain-of-custody][chain-of-custody]  

The data set for a  longitudinal study prepared by the respective [ialsa-study-curator][ialsa-study-curator] enters the analytic environment of the    
[ialsa-2018-amsterdam][ialsa-2018-amsterdam] and meets the      
-  [greeter][greeter] script, which adds custom meta-data and shapes the _data transfer object_ (dto), which tracks all states of the data on its way to the estimated model (hence, "chain of custody" metaphor).    
-  [tuner][tuner] script, which implements project-specific data transformation is then applied to create variables that could by used by the   
-  [encoder][encoder] script to encode the multistate variable, pivotal for the `msm()` estimation call. Then    
-  [validator][validator] script makes sure only valid cases (respondents) are selected and passes the data to the    
-  [modeler][modeler] script, which estimates the statistical model according the  [model-specification][model-a-spec] and saves generated objects to local drive.   



[chain-of-custody]:../../libs/images/support/chain-of-custody.jpg  

[greeter]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/0-greeter-map.R
[tuner]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/1-tuner-map.R
[encoder]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/2-encoder-map.R
[validator]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/3-validator-map.R
[modeler]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/4-modeler-map.R

[model-a-spec]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/libs/images/support/model-a-specification.jpg
[ialsa-study-curator]:https://github.com/IALSA/ialsa-study-curator/blob/master/README.md
[ialsa-2018-amsterdam]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/README.md