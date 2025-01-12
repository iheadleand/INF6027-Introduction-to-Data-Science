# INF6027-Introduction-to-Data-Science
The following repository presents the datasets used and code produced for the INF6027 Introduction to Data Science Coursework. 

## Introduction to Work and Research Questions
The project aimed to explore how a songs audio and metadata features can be used to predict a songs popularity using information in the publically available MusicOSet dataset with research questions:

RQ1: What song audio and metadata features show significant association with song popularity 

RQ2: To what extent can multivariable linear regression be used to accurately predict song popularity from song audio and metadata features

The work was undertaken in R studio intially exploring how the different song features singificantly associated with song popularity. A multivariable linear regression model was then developed using selected features to evaluate how the song features could be used to predict song popularity. 


## Key Findings
Songs that are longer, less acoustic, more danceable, more energetic, less instrumental, non-live, more positive, have been released more recently, have greater speechiness, are minor, contain explicit lyrics and that are produced collaboratively are associated with greater popularity. 

Using ten selected metadata and audio features a mutlivariable linear regression model predicting song popularity was able to expalin 29% of the variation in song popularity. It was therefore concluded that multivariable lienar regression has low capacity for predicting song popualrity based on the data and song features presented in the MusicOSet dataset. 


## Details of the Code and Instructions for Running it 
Coding undertaken was conducted in R with the R script contained and available for download from the Script folder of the repository. The packages used for the coding are presented at the top of the script and should be installed not allready prior to running the script. The script is chronologically ordered and therefore can be run in its entirity. Headings are alternativley used to mark the sections of analysis: Packages, Import Data, Dataset Cleaning and Preparation, Binding and Merginging of Datasets, Preparation of Total Data, Creation of Test and Training Data, Exploratory Data Analysis sections, Multivariable Linear Regression, Multivariable Linear Regression Assumptions and finally Model Evaluation. 

The datasets used for the analysis are presented in the dataset file of the repository and should be downloaded within the file to enable their reading into the script. 
