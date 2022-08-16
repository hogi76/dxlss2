prework0 = function(){
    RequiredPackages = c('tidyverse', 'tree', 'randomForest', 'rpart', 'psych', 'caret', 'rpart.plot', 'DMwR2')
    for (i in RequiredPackages) {
      if (!i %in% row.names(installed.packages())) install.packages(i)
    }


    library(tidyverse)
    library(tree)
    library(randomForest)
    library(rpart)
    library(psych)
    library(caret)
    library(rpart.plot)
    library(DMwR2)

}


