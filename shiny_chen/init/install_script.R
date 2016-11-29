# For Installation

packageList <- list("readxl", "EpiModel", "ggplot2", "reshape2", "plotly", "shinythemes", "shinyBS", "dplyr")

for(item in packageList){
  install.packages(item)
}


require(devtools)
install_github('rCharts', 'ramnathv')
