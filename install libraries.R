#
# Thanks to Shane https://stackoverflow.com/users/163053/shane
# See https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
#

list.of.packages <- c("readxl", "stringr", "dplyr", "purrr", "colorspace", "ggplot2", "rmarkdown", "parallel", "tinytex")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

tinytex::install_tinytex()