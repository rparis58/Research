# http://www.r-bloggers.com/a-demonstration-of-vtreat-data-preparation/
# The point is: we got competitive results on real world data, in a single try (using vtreat to prepare data and caret to tune parameters). 
# The job of the data scientist is to actually work longer on a problem and do better. But having a good start helps.

library('vtreat')
library('caret')
library('gbm')
library('doMC')
library('WVPlots') # see https://github.com/WinVector/WVPlots

# parallel for vtreat
ncores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(ncores)
# parallel for caret
registerDoMC(cores=ncores)

# load data
# data from: http://archive.ics.uci.edu/ml/machine-learning-databases/adult/
colnames <-
  c(
    'age',
    'workclass',
    'fnlwgt',
    'education',
    'education-num',
    'marital-status',
    'occupation',
    'relationship',
    'race',
    'sex',
    'capital-gain',
    'capital-loss',
    'hours-per-week',
    'native-country',
    'class'
  )
dTrain <- read.table(
  'adult.data',
  header = FALSE,
  sep = ',',
  strip.white = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c('NA', '?', '')
)
colnames(dTrain) <- colnames
dTest <- read.table(
  'adult.test',
  skip = 1,
  header = FALSE,
  sep = ',',
  strip.white = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c('NA', '?', '')
)
colnames(dTest) <- colnames



  library('vtreat')
  library('caret')
  library('gbm')
  library('doMC')
  library('WVPlots') # see https://github.com/WinVector/WVPlots
  
  # parallel for vtreat
  ncores <- parallel::detectCores()
  parallelCluster <- parallel::makeCluster(ncores)
  # parallel for caret
  registerDoMC(cores=ncores)
  The we load our data for analysis. We are going to build a model predicting an income level from other demographic features. The data is taken from here and you can perform all of the demonstrated steps if you download the contents of the example git directory. Obviously this has a lot of moving parts (R, R Markdown, Github, R packages, devtools)- but is very easy to do a second time (first time can be a bit of learning and preparation).
  
  # load data
  # data from: http://archive.ics.uci.edu/ml/machine-learning-databases/adult/
  colnames <-
    c(
      'age',
      'workclass',
      'fnlwgt',
      'education',
      'education-num',
      'marital-status',
      'occupation',
      'relationship',
      'race',
      'sex',
      'capital-gain',
      'capital-loss',
      'hours-per-week',
      'native-country',
      'class'
    )
  dTrain <- read.table(
    'adult.data.txt',
    header = FALSE,
    sep = ',',
    strip.white = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c('NA', '?', '')
  )
  colnames(dTrain) <- colnames
  dTest <- read.table(
    'adult.test.txt',
    skip = 1,
    header = FALSE,
    sep = ',',
    strip.white = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c('NA', '?', '')
  )
  colnames(dTest) <- colnames

# Now we use vtreat to prepare the data for analysis. The goal of vtreat is to ensure a ready-to-dance 
# data frame in a statistically valid manner. We are respecting the test/train split and building our data preparation plan only on the training data (though we do apply it to the test data). This step helps with a huge number of potential problems through automated repairs:
    
# re-encoding missing values
# dealing with large cardinality categorical variables
# dealing with novel levels
# fixing variable/column names to be "R safe"
#looking for strange column types

# define problem
  yName <- 'class'
  yTarget <- '>50K'
  varNames <- setdiff(colnames,yName)
  
  # build variable encoding plan and prepare simulated out of sample
  # training fame (cross-frame) 
  # http://www.win-vector.com/blog/2016/05/vtreat-cross-frames/
  system.time({
    cd <- vtreat::mkCrossFrameCExperiment(dTrain,varNames,yName,yTarget,
                                          parallelCluster=parallelCluster)
    scoreFrame <- cd$treatments$scoreFrame
    dTrainTreated <- cd$crossFrame
    # pick our variables
    newVars <- scoreFrame$varName[scoreFrame$sig<1/nrow(scoreFrame)]
    dTestTreated <- vtreat::prepare(cd$treatments,dTest,
                                    pruneSig=NULL,varRestriction=newVars)
  })
  ##    user  system elapsed 
  ##  11.340   2.760  30.872
  #print(newVars)
  
  # train our model using caret
  system.time({
    yForm <- as.formula(paste(yName,paste(newVars,collapse=' + '),sep=' ~ '))
    # from: http://topepo.github.io/caret/training.html
    fitControl <- trainControl(## 10-fold CV
      method = "cv",
      number = 3)
    model <- train(yForm,
                   data = dTrainTreated,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE)
    print(model)
    dTest$pred <- predict(model,newdata=dTestTreated,type='prob')[,yTarget]
  })
  # Tuning parameter 'shrinkage' was held constant at a value of 0.1
  # Tuning parameter 'n.minobsinnode' was held constant at a value of 10
  # Accuracy was used to select the optimal model using  the largest value.
  # The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10. 
  # user  system elapsed 
  # 63.621   2.990  44.817 
  
  WVPlots::ROCPlot(dTest,'pred',yName,'predictions on test')
  WVPlots::DoubleDensityPlot(dTest,'pred',yName,'predictions on test')
  
  confusionMatrix <- table(truth=dTest[[yName]],pred=dTest$pred>=0.5)
  print(confusionMatrix)
  
  testAccuarcy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
  testAccuarcy
  
  ## test on complete.cases
  dTestComplete <- dTest[complete.cases(dTest[,varNames]),]
  confusionMatrixComplete <- table(truth=dTestComplete[[yName]], pred=dTestComplete$pred>=0.5)
  print(confusionMatrixComplete)
  
  testAccuarcyComplete <- (confusionMatrixComplete[1,1]+confusionMatrixComplete[2,2]) / sum(confusionMatrixComplete)
  testAccuarcyComplete
  
  #### clean up
  parallel::stopCluster(parallelCluster)
  
  