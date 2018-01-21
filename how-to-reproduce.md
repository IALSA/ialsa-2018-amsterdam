This document describes the analytic workflow of the project and narrates the sequence of implementation scripts.



## Environment  

- RStuido as IDE for R
- Git as version control + client or prompt
- a clone of [ialsa-study-curator][ialsa-study-curator] for the selected longitudinal study
- a clone of the current project repository: [ialsa-2018-amsterdam][ialsa-2018-amsterdam]

## Scripts

We devided the workflow into two stages. During the first stage (Chain of Custody), the data set for a  longitudinal study is transformed to fit requirments for model estimation. During the second stage (Model Evaluation), we transform the estimated object(s) into data sets, tables, and graphs designed to interpret the model results in the context of the research question.


## Chain of custody

![chain-of-custody][chain-of-custody]  

The data set for a  longitudinal study prepared by the respective [ialsa-study-curator][ialsa-study-curator] enters the analytic environment of the    
[ialsa-2018-amsterdam][ialsa-2018-amsterdam] and meets the      
- (0) [greeter][greeter] script, which adds custom meta-data and shapes the _data transfer object_ (dto), which tracks all states of the data on its way to the estimated model (hence, "chain of custody" metaphor).    
- (1) [tuner][tuner] script, which implements project-specific data transformation is then applied to create variables that could by used by the   
- (2) [encoder][encoder] script to encode the multistate variable, pivotal for the `msm()` estimation call. Then    
- (3) [validator][validator] script makes sure only valid cases (respondents) are selected and passes the data to the    
- (4) [modeler][modeler] script, which estimates the statistical model according the  [model-specification][model-a-spec] and saves generated objects to local drive.   


## Model evaluation

These scripts start with a set of estimated models and organize the information they contain into data sets, tables, and graphs designed to interpret model solutions, compare and contrast them, and draw insight about the nature of the underlying phenomena. 

To Be Developed

[chain-of-custody]:libs/images/support/chain-of-custody.jpg  

[greeter]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/0-greeter-map.R
[tuner]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/1-tuner-map.R
[encoder]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/2-encoder-map.R
[validator]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/3-validator-map.R
[modeler]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/manipulation/map/4-modeler-map.R

[model-a-spec]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/libs/images/support/model-a-specification.jpg
[ialsa-study-curator]:https://github.com/IALSA/ialsa-study-curator/blob/master/README.md
[ialsa-2018-amsterdam]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/README.md