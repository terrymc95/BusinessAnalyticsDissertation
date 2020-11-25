## These packages are used throughout the project

install.packages(c("qdapRegex","tidyverse","tm","tidytext","caret","tictoc",
               	    "e1071","gbm","xgboost", "MLeval"))
packs <- c("qdapRegex","tidyverse","tm","tidytext","caret","tictoc",
        	   "e1071","gbm","xgboost", "MLeval")
for (i in 1:10){
  library(packs[i], character.only = T)
}
