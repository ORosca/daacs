# Bayesian T-test: Comparison among the total DAACS scores of students from 
# online and traditional colleges.
# AUS= Adult Undergraduate Students
# TCAUS = Traditional College Age Undergraduate Students
# Result: from 22 t-tests (3 assessments, 5 scales, and 14 subscales) only the 
# math  assessment revealed a difference between TCAUS and AUS students. 
# TCAUS students have  higher math skills than AUS students.	
# There is no difference between TCAUS and AUS students on other DAACS assnts.

#------------------------------------------------------------------------------- 
# Load The data file
ua2umgc1SRLDaacs <- readRDS("ua2umgc1SRLDaacs.rds")
ua2umgc1MathReadDaacs <- readRDS("ua2umgc1MathReadDaacs.rds")

#Prepare the data:
#tibles do not work for Kruschke's codes:
ua2umgc1SRLDaacs <- as.data.frame(ua2umgc1SRLDaacs)
ua2umgc1MathReadDaacs <- as.data.frame(ua2umgc1MathReadDaacs)
#ua2umgc1SRLDaacs$college <- ifelse(ua2umgc1SRLDaacs$college =="TCAUS","AUS")
#ua2umgc1MathReadDaacs$college=ifelse(ua2umgc1MathReadDaacs$college=="TCAUS","AUS")
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Stan-Ymet-Xnom2grp-MrobustHet.R")
#------------------------------------------------------------------------------- 
# Specify filename root and graphical format for saving output.
fileNameRoot = "TwoGroupDAACS" 
graphFileType = "eps" 
#Subset the datasets (only complete cases and finite values work)
mathTotal<- ua2umgc1MathReadDaacs[,c(2,33)]
mathTotal <- subset(mathTotal[complete.cases(mathTotal),])
round(mathTotal$mathTotal, digits = 6)
readTotal<- ua2umgc1MathReadDaacs[,c(2,45)]
readTotal <- subset(readTotal[complete.cases(readTotal),])
round(readTotal$readTotal, digits = 6)
writeTotal<- ua2umgc1MathReadDaacs[,c(2,65)]
writeTotal <- subset(writeTotal[complete.cases(writeTotal),])
round(writeTotal$writeTotal, digits = 6)
srlTotal <- ua2umgc1SRLDaacs[,c(21,129)]
srlTotal <- subset(srlTotal[complete.cases(srlTotal),])
round(srlTotal$srlTotal, digits = 6)
srl_motivation <- ua2umgc1SRLDaacs[,c(4,129)]
srl_motivation <- subset(srl_motivation[complete.cases(srl_motivation),])
round(srl_motivation$srl_motivation, digits = 6)
srl_mastery_orientation <- ua2umgc1SRLDaacs[,c(12,129)]
srl_mastery_orientation <- 
  subset(srl_mastery_orientation[complete.cases(srl_mastery_orientation),])
srl_mindset <- ua2umgc1SRLDaacs[,c(13,129)]
srl_mindset <- subset(srl_mindset[complete.cases(srl_mindset),])
round(srl_mindset$srl_mindset, digits = 6)
srl_anxiety <- ua2umgc1SRLDaacs[,c(11,129)]
srl_anxiety <- subset(srl_anxiety[complete.cases(srl_anxiety),])
round(srl_anxiety$srl_anxiety, digits = 6)
srl_metacognition <- ua2umgc1SRLDaacs[,c(5,129)]
srl_metacognition<-subset(srl_metacognition[complete.cases(srl_metacognition),])
round(srl_metacognition$srl_metacognition, digits = 6)
srl_planning <- ua2umgc1SRLDaacs[,c(16,129)]
srl_planning <- subset(srl_planning[complete.cases(srl_planning),])
round(srl_planning$srl_planning, digits = 6)
srl_monitoring <- ua2umgc1SRLDaacs[,c(15,129)]
srl_monitoring <- subset(srl_monitoring[complete.cases(srl_monitoring),])
round(srl_monitoring$srl_monitoring, digits = 6)
srl_evaluation <- ua2umgc1SRLDaacs[,c(14,129)]
srl_evaluation <- subset(srl_evaluation[complete.cases(srl_evaluation),])
round(srl_evaluation$srl_evaluation, digits = 6)
srl_strategies <- ua2umgc1SRLDaacs[,c(3,129)]
srl_strategies <- subset(srl_strategies[complete.cases(srl_strategies),])
round(srl_strategies$srl_strategies, digits = 6)
srl_managing_environment <- ua2umgc1SRLDaacs[,c(9,129)]
srl_managing_environment <- 
  subset(srl_managing_environment[complete.cases(srl_managing_environment),])
round(srl_managing_environment$srl_managing_environment, digits = 6)
srl_managing_time <- ua2umgc1SRLDaacs[,c(7,129)]
srl_managing_time<-subset(srl_managing_time[complete.cases(srl_managing_time),])
round(srl_managing_time$srl_managing_time, digits = 6)
srl_understanding <- ua2umgc1SRLDaacs[,c(10,129)]
srl_understanding<-subset(srl_understanding[complete.cases(srl_understanding),])
round(srl_understanding$srl_understanding, digits = 6)
srl_help_seeking <- ua2umgc1SRLDaacs[,c(8,129)]
srl_help_seeking <- subset(srl_help_seeking[complete.cases(srl_help_seeking),])
round(srl_help_seeking$srl_help_seeking, digits = 6)
srl_selfefficacy <- ua2umgc1SRLDaacs[,c(6,129)]
srl_selfefficacy <- subset(srl_selfefficacy[complete.cases(srl_selfefficacy),])
srl_selfefficacy_for_mathematics <- ua2umgc1SRLDaacs[,c(17,129)]
srl_selfefficacy_for_mathematics <- 
  subset(srl_selfefficacy_for_mathematics[complete.cases(srl_selfefficacy_for_mathematics),])
srl_selfefficacy_for_writing <- ua2umgc1SRLDaacs[,c(20,129)]
srl_selfefficacy_for_writing <- 
subset(srl_selfefficacy_for_writing[complete.cases(srl_selfefficacy_for_writing),])
round(srl_selfefficacy_for_writing$srl_selfefficacy_for_writing, digits = 6)
srl_selfefficacy_for_reading <- ua2umgc1SRLDaacs[,c(19,129)]
srl_selfefficacy_for_reading <- 
  subset(srl_selfefficacy_for_reading[complete.cases(srl_selfefficacy_for_reading),])
round(srl_selfefficacy_for_reading$srl_selfefficacy_for_reading, digits = 6)
srl_selfefficacy_for_online_learning <- ua2umgc1SRLDaacs[,c(18,129)]
srl_selfefficacy_for_online_learning <- 
  subset(srl_selfefficacy_for_online_learning[complete.cases(srl_selfefficacy_for_online_learning),])
round(srl_selfefficacy_for_online_learning$srl_selfefficacy_for_online_learning, 
      digits = 6)
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=mathTotal, yName="mathTotal" , xName="college" ,
                    numSavedSteps=20000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.1,0.1) , 
                        RopeSdDiff=c(-0.1,0.1) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=mathTotal , yName="mathTotal" , xName="college" , 
          RopeMuDiff=c(-0.1,0.1) , RopeSdDiff=c(-0.1,0.1), RopeEff=c(-0.1,0.1) , 
          pairsPlot=TRUE , saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
mcmcCoda = genMCMC( datFrm=readTotal, yName="readTotal" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.1,0.1) , 
                        RopeSdDiff=c(-0.1,0.1) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=readTotal , yName="readTotal", 
          xName="college", RopeMuDiff=c(-0.1,0.1), RopeSdDiff=c(-0.1,0.1), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=writeTotal, yName="writeTotal" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.1,0.1) , 
                        RopeSdDiff=c(-0.1,0.1) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=writeTotal , yName="writeTotal", 
                xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
                 RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
                  saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srlTotal, yName="srlTotal" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srlTotal , yName="srlTotal", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_motivation, yName="srl_motivation" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_motivation , yName="srl_motivation", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_mastery_orientation, 
                    yName="srl_mastery_orientation" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_mastery_orientation , 
          yName="srl_mastery_orientation", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_mindset, yName="srl_mindset" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_mindset , yName="srl_mindset", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_anxiety, yName="srl_anxiety" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_anxiety , yName="srl_anxiety", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_metacognition, yName="srl_metacognition" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_metacognition , yName="srl_metacognition", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_planning, yName="srl_planning" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_planning , yName="srl_planning", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_monitoring, yName="srl_monitoring" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_monitoring , yName="srl_monitoring", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_evaluation, yName="srl_evaluation" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_evaluation , yName="srl_evaluation", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_strategies, yName="srl_strategies" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_strategies , yName="srl_strategies", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_managing_environment, 
                    yName="srl_managing_environment" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_managing_environment , 
          yName="srl_managing_environment", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_managing_time, yName="srl_managing_time" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_managing_time , yName="srl_managing_time", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_understanding, yName="srl_understanding" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_understanding , yName="srl_understanding", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_help_seeking, yName="srl_help_seeking" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_help_seeking , yName="srl_help_seeking", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_selfefficacy, yName="srl_selfefficacy" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_selfefficacy , yName="srl_selfefficacy", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_selfefficacy_for_mathematics, 
                    yName="srl_selfefficacy_for_mathematics" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_selfefficacy_for_mathematics , 
          yName="srl_selfefficacy_for_mathematics", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_selfefficacy_for_writing, 
                    yName="srl_selfefficacy_for_writing" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_selfefficacy_for_writing , 
          yName="srl_selfefficacy_for_writing", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_selfefficacy_for_reading, 
                    yName="srl_selfefficacy_for_reading" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_selfefficacy_for_reading , 
          yName="srl_selfefficacy_for_reading",
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------
mcmcCoda = genMCMC( datFrm=srl_selfefficacy_for_online_learning, 
                    yName="srl_selfefficacy_for_online_learning" , 
                    xName="college",numSavedSteps=20000 , saveName=fileNameRoot)
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=c(-0.5,0.5) , 
                        RopeSdDiff=c(-0.5,0.5) , RopeEff=c(-0.1,0.1) , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=srl_selfefficacy_for_online_learning , 
          yName="srl_selfefficacy_for_online_learning", 
          xName="college", RopeMuDiff=c(-0.5,0.5), RopeSdDiff=c(-0.5,0.5), 
          RopeEff=c(-0.1,0.1) ,pairsPlot=TRUE, saveName=fileNameRoot, 
          saveType=graphFileType )
#-------------------------------------------------------------------------------

