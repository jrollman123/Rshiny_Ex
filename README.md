# Packages Used (Install before trying to run app)
tidyverse  
ggplot2  
caret  
gbm  
DT  
psych  
knitr  
plotly  (Plotly graphs must be displayed in a web browser not Rstudio's app viewer)  
shiny  
shinydashboard  

# Install and Load Packages
```{r}
#Install Packages
Rpacks <- c("tidyverse","ggplot2","caret","gbm","DT","psych","knitr","plotly","shiny","shinydashboard")
install.packages(Rpacks)

#Load PAckages
library(tidyverse)
library(ggplot2)
library(caret)
library(gbm)
library(DT)
library(psych)
library(knitr)
library(plotly)
library(shiny)
library(shinydashboard)
```
# Run this code in a r script to render shiny app
```{r}
shiny::runGitHub("Rshiny_Ex", "jrollman123")
```
