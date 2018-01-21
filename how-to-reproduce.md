This document describes the functions of the active scripts in this project



## Environment  

- RStuido as IDE for R
- Git as version control + client or prompt
- a clone of [ialsa-study-curator][ialsa-study-curator] for the selected longitudinal study
- a clone of the current project repository: [ialsa-2018-amsterdam][ialsa-2018-amsterdam]

## Scripts

We devided the workflow into two stages. During the first one we called "Chain of Custody", the data set for a  longitudinal study prepared by the respective [ialsa-study-curator][ialsa-study-curator] enters the analytic environment of the [ialsa-2018-amsterdam][ialsa-2018-amsterdam] and meets the  
[greeter][greeter] script, which adds custom meta-data and shapes the  
_data transfer object_ (dto), which tracks all states of the data on its way to the estimated model (hence, "chain of custody" metaphor).
[tuner][tuner] script, which implements project-specific data transformation is then applied to create variables that could by used by the
[encoder][encoder] script to encode the multistate variable, pivotal for the `msm()` estimation call
[validator][validator] script makes sure only valid cases (respondents) are selected into the analized set and passes the data to the
[modeler][modeler] script, which specifies and estimates the statistical model and saves the produced objects to local drive. 


## Chain of custody

![chain-of-custody][chain-of-custody]  

The data set for a  longitudinal study prepared by the respective [ialsa-study-curator][ialsa-study-curator] enters the analytic environment of the [ialsa-2018-amsterdam][ialsa-2018-amsterdam] and meets the  
[greeter][greeter] script, which adds custom meta-data and shapes the  
_data transfer object_ (dto), which tracks all states of the data on its way to the estimated model (hence, "chain of custody" metaphor).
[tuner][tuner] script, which implements project-specific data transformation is then applied to create variables that could by used by the
[encoder][encoder] script to encode the multistate variable, pivotal for the `msm()` estimation call. Then
[validator][validator] script makes sure only valid cases (respondents) are selected  and passes to the
[modeler][modeler] script, which specifies and estimates the statistical model and saves the produced objects to local drive. 


## Model evaluation

These scripts start with a set of estimated models and organize the information they contain into data sets, tables, and graphs designed to interpret model solutions, compare and contrast them, and draw insight about the nature of the underlying phenomena. 

To Be Developed

[chain-of-custody]:libs/images/support/chain-of-custody.jpg  
[model-a-spec]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/libs/images/support/model-a-specification.jpg
[ialsa-study-curator]:https://github.com/IALSA/ialsa-study-curator/blob/master/README.md
[ialsa-2018-amsterdam]:https://github.com/IALSA/ialsa-2018-amsterdam/blob/master/README.md