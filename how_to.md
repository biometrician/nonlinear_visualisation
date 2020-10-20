---
title: "How to"
author: "Daniela Dunkler"
date: '2020-08-11'
---

**How to get the shiny app running?**
* Prepare the explanation.html following these steps:
  + knit to HTML
  
  + delete "!DOCTYPE html"
  
  + delete the complete head (from <head> to </head>) and add the following text instead:
      <head>
      
      <meta charset="utf-8" />
      <meta name="generator" content="pandoc" />
      <meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
      
      <meta name="author" content="Daniela Dunkler" />
      
      
      <title>Visualisation of non-linear modeling</title>
      <style>
      img {
        max-width:100%;
      }
      </style>
      
      </head>
  
  + then also delete these lines below the head:
      <div class="fluid-row" id="header">
      <h1 class="title toc-ignore">Visualisation of non-linear modeling</h1>
      <h4 class="author">Daniela Dunkler</h4>
      </div>


* run the app


* upload to shinyapps.io via clicking on republish
  + upload the following files
    - app.R
    - about.md
    - explanation.html
    - data/data.rds
    - www/stratos_logo.png
 
 
 
**How to reproduce the data used in the app?**

* "data.RDS" includes the 11 variables used in the application. The orginal file to reproduce the data can be found on cloudius at "...\CLOUDIUS\DD_2020_nonlinear_modeling\_shiny\_materials\20200708_nhanes_data.R". The "nhanes_BP.RData" from Georg can be found on cloudius at "...\CLOUDIUS\DD_2020_nonlinear_modeling\_shiny\_materials\nhanes_BP.Rdata", as well. 


* "explanation_data.RDS"", the data used for the explanation panel (explanation.Rmd)

Here is the R code:
 
load(file="./_materials/nhanes_BP.Rdata")
 
nhanes_m <- subset(nhanes_BP, gender == "male")
nhanes_f <- subset(nhanes_BP, gender == "female")

set.seed(436731)
nhanes_m2 <- nhanes_m[sample(1:nrow(nhanes_m), 1000),]
nhanes_f2 <- nhanes_f[sample(1:nrow(nhanes_f), 1000),]
nhanes_BP2 <- rbind(nhanes_m2, nhanes_f2)

data <- data.frame(cbind(weight_kg_m = nhanes_m2$weight_kg, 
                         bmi_m       = nhanes_m2$bmi, 
                         triglyc_m   = nhanes_m2$triglyc,
                         HDLchol_m   = nhanes_m2$HDLchol,
                         BPsys_m     = nhanes_m2$BPsys,
                         BPdia_m     = nhanes_m2$BPdia,
                         waist_f     = nhanes_f2$waist,
                         triglyc_f   = nhanes_f2$triglyc,
                         HDLchol_f   = nhanes_f2$HDLchol,
                         BPsys_f     = nhanes_f2$BPsys,
                         age_f       = nhanes_f2$age))
saveRDS(data, "./data/data.rds")

explanation_data <- nhanes_BP[,c("ID", "age", "bmi")]
saveRDS(explanation_data, "./data/explanation_data.rds")
