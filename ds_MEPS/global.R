# Detect and install packages
packages <- c(
'shiny', 
'shinydashboard',
'DT', 
'tidyverse',
'survey',
'DBI',
'data.table',
'ggplot2',
'RSQLite'
)


#missing.packages <- setdiff(packages, rownames(installed.packages()))

#if (length(missing.packages) > 0) {
#cat(c('The following packages will be installed:\n\t', missing.packages, '\n'))
#install.packages(missing.packages, repos="http://cran.r-project.org", dependencies = TRUE)
#}

sapply(packages, require, character.only = TRUE)



