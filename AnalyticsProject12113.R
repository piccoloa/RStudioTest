
library(dplyr)
library(datasets)
library(rpart)
#TEst the revision in git
library(rattle)
library(RColorBrewer)
library(plyr)
library(car)
#install.packages('pscl')
library(pscl) #Needed for the McFadden R2 test
library(caret)
#install.packages("SDMTools")
library(SDMTools)
#install.packages("clusterGeneration")
library(clusterGeneration)
library(MASS)
library(e1071)
library(ggplot2)

DataNNew <- readRDS("data1.rds")
attach(DataNNew)

#removing variables with too few instances or missing data
filtered_rev0 <- filter(DataNNew, Rev > 0, EmCnt >0)  # exlclude na
filtered_coopp <- filter(filtered_rev0, CoOppDays >= 0, 
                         CoOppyMo >= 0, CoOppyLMo >= 0, 
                         CoOppyWMo >= 0)
summary(filtered_coopp)

tbl<-tbl_df(filtered_coopp)


DataProspAndCust<-select(filtered_coopp, interest,
                         IsCust,
                         Cntry,
                         ActSz,
                         Ind,
                         Rev,
                         EmCnt,
                         CoOppy,
                         CoOppDays,
                         CoMQLCt,
                         CoOppWCt,
                         CoOppLCt,
                         CoOppOpCt,
                         CoOppyMo,
                         CoOppyWMo,
                         CoOppyLMo,
                         CoOppyOMo)
detach(DataNNew)

rm(DataNNew, filtered_coopp, filtered_rev0, tbl)
#DataCust<-filter(DataProspAndCust, IsCust == 1) 
#DataProsp<-filter(DataProspAndCust, IsCust == 0)
DataProspAndCust<-tbl_df(DataProspAndCust)


####save datasets to files  #  
#saveRDS(DataCust, file="DataCust.rds")
#saveRDS(DataProsp, file="DataProsp.rds")
saveRDS(DataProspAndCust, file="DataProspAndCust.rds")

####remove datasets to avoid masking and using wrong datasets variables below
#rm(DataNNew,tbl,filtered_rev0, filtered_coopp, DataProspAndCust,DataProspAndCust,DataProsp,DataCust)

##***DataCust***#####create subgroup for factor varibles using ctree

########REading factor dataset
DataProspAndCust<- readRDS("DataProspAndCust.rds")


DataCntry <- subset(DataProspAndCust, select = c(1,3))
rm(DataProspAndCust, DataNNew, filtered_coopp, filtered_rev0, tbl)  #avoids maskinging issue when working with new vector with same name
attach(DataCntry)

cfit<-rpart(interest ~ Cntry,
            data = DataCntry, method = 'class')
fancyRpartPlot(cfit)
cfit
summary(cfit)

#DataProspAndCust<- readRDS("DataProspAndCust.rds")

DataCntry$Cntry1<-recode(Cntry, "'AUS' = '1'; 'AUT' = '1';' CAN' = '1';	'CHE' = '1';	'CRI' = '1';	
                         'DNK' = '1';	'FRA' = '1';	'GRL' = '1';	'IRL' = '1';	'ISR' = '1' ; 'ITA' = '1';	'LIE' = '1';	'NOR' = '1';	'NZL' = '1';	'PAN' = '1';	'SAU' = '1';	'SVK' = '1';	'TUN' = '1';	'TWN' = '1';	'UKR' = '1';	'USA' = '1';	'VEN' = '1';	'VNM' = '1';
                         ;else = '0'")	


DataCntry$Cntry2<-recode(Cntry, "'AGO' = '1';	'ARE' = '1';	'BEL' = '1';	'BGD' = '1';	'BGR' = '1';	'BHR' = '1';	'BLR' = '1';	'BRA' = '1';	'CHL' = '1';	'CHN' = '1';	'COL' = '1';	'CYP' = '1';	'CZE' = '1';	'DEU' = '1';	'EGY' = '1';	'ESP' = '1';	'EST' = '1';	'FIN' = '1';	'GBR' = '1';	'GRC' = '1';	'HRV' = '1';	'HUN' = '1';	'IDN' = '1';	'IND' = '1';	'ISL' = '1';	'JOR' = '1';	'JPN' = '1';	'KAZ' = '1';	'KEN' = '1';	'KOR' = '1';	'LKA' = '1';	'LTU' = '1';	'LUX' = '1';	'LVA' = '1';	'MDA' = '1';	'MEX' = '1';	'MLT' = '1';	'MYS' = '1';	'NGA' = '1';	'NLD' = '1';	'OMN' = '1';	'PAK' = '1';	'PHL' = '1';	'POL' = '1';	'PRT' = '1';	'QAT' = '1';	'ROU' = '1';	'RUS' = '1';	'SGP' = '1';	'SRB' = '1';	'SVN' = '1';	'SWE' = '1';	'THA' = '1';	'TUR' = '1';	'ZAF' = '1';
                         else = '0'")	



DataCntry<-as.data.frame(DataCntry)
#open original file to add new variables
DataProspAndCust<- readRDS("DataProspAndCust.rds")
DataProspAndCust$Cntry1<-DataCntry$Cntry1
DataProspAndCust$Cntry2<-DataCntry$Cntry2

x<-select(DataCntry, interest, Cntry) 
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
ggplot(x1, aes(Cntry) ) +
  geom_bar()
#veritical title
ggplot(x1, aes(Cntry) ) +
  geom_bar() + coord_flip()

x<-select(DataCntry, interest, Cntry1) 
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(Cntry1) ) +
  geom_bar() + coord_flip()

x<-select(DataCntry, interest, Cntry2) 
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(Cntry2) ) +
  geom_bar() + coord_flip()

x1<-x1 %>%
  filter(!is.na(Cntry), interest != '0') %>%
  group_by(Cntry) %>%
  summarise(c_Ind = count(interest)) 

rm(DataCntry)
detach(DataCntry)

#charts per plot
#par(mfrow=c(1,1))

#barplot(table(DataProspAndCust$interest), col="black", xlab="Species", ylab="Count", main="Bar plot of Sepal Length")
#barplot(table(DataProspAndCust$interest), col="black", xlab="Species", ylab="Count", main="Bar plot of Sepal Length")



#density <- ggplot(DataProspAndCust=Interest, aes(x=Rev))
#density + geom_histogram(binwidth=0.2, color="black", fill="steelblue", aes(y=..density..)) +
#  geom_density(stat="density", alpha=I(0.2), fill="blue") +
#  xlab("Sepal Width") +  ylab("Density") + ggtitle("Histogram & Density Curve")

#rm(DataCntry)

#create booleen values for ActSz
DataProspAndCust$ActSz1<-recode(DataProspAndCust$ActSz, "'Large Enterprise' = '1'; else = '0'")
DataProspAndCust$ActSz2<-recode(DataProspAndCust$ActSz, "'Mid-Enterprise' = '1'; else = '0'")
DataProspAndCust$ActSz3<-recode(DataProspAndCust$ActSz, "'SMB' = '1'; else = '0'")
DataProspAndCust$ActSz4<-recode(DataProspAndCust$ActSz, "'Blank' = '1'; else = '0'")

levels(DataProspAndCust$ActSz)
saveRDS(DataProspAndCust, file="DataProspAndCust.rds")
######plots for acct sze##################
x<-select(DataProspAndCust, interest, ActSz)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(ActSz) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind) 
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(Cntry2) ) +
  geom_bar() + coord_flip()


##***DataInd***#####create subgroup for factor varibles using ctree

DataInd <- subset(DataProspAndCust, select = c(1,5))
rm(DataProspAndCust)  #avoids maskinging issue when working with new vector with same name

attach(DataInd)
#https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/rpart.control.html
#rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
#              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
#             surrogatestyle = 0, maxdepth = 30, ...)
###rpart will not split greater than 1 node; classified by frequency rank
#cfit3<-rpart.control(
#                     data = DataInd, method = 'class')

#cfit3<-rpart(interest ~ Ind,
#            data = DataInd, method = 'class', control=rpart.control(minsplit=1000, minbucket=1, cp = 0.1))


########created frequence table############
DataInd %>%
 filter(!is.na(Ind)) %>%
group_by(Ind) %>%
summarise(c_Ind = count(Ind)) %>%
#mutate(rank = rank(c_Ind)) %>%
#arrange(rank)
#total industry count
x<-DataInd %>%
  filter(!is.na(Ind), Ind != 'Retail') %>%
  group_by(Ind) %>%
  summarise(c_Ind = count(Ind)) 

x<-DataInd %>%
  filter(!is.na(Ind)) %>%
  group_by(Ind) %>%
  summarise(c_Ind = count(Ind)) 

y<-DataInd %>%
  filter(!is.na(Ind), interest == '1') %>%
  group_by(Ind) %>%
  summarise(c_Ind1 = count(Ind)) 


z<-c(x$c_Ind, y$c_Ind1)
z = as.data.frame(z)
z<-mutate(z, rankInt = percent_rank(freq.1/freq))
clasInd<-z %>%
  group_by(x) %>%
  arrange(desc(rankInt))

rm(DataInd)
DataProspAndCust<- readRDS("DataProspAndCust.rds")
detach(DataInd)
#attach(DataCust)
#rm(clasInd, DataCntry, DataInd, x, y, z)

#mutate(DataProspAndCust, Ind1 = ifelse(grepl("Machinery", Ind), "1",
#                                      ifelse(grepl("Education", Ind), "1", "0")))

#c(7,8,9)
attach(DataProspAndCust)
DataProspAndCust$Ind1<-recode(Ind, "c('Machinery', 'Education') = 1	;	else = '0'")
DataProspAndCust$Ind2<-recode(Ind, "c('Government' ,'Legal') = 1;	else = '0'")
DataProspAndCust$Ind3<-recode(Ind, "c('Healthcare' , 'Utilities') = 1;	else = '0'")
DataProspAndCust$Ind4<-recode(Ind, "c('Consulting', 'Banking & Insurance') = 1;	else = '0'")
DataProspAndCust$Ind5<-recode(Ind, "c('Media','Communications') = 1;	else = '0'")
DataProspAndCust$Ind6<-recode(Ind, "c('Technology','Manufacturing') = 1	;else = '0'")
DataProspAndCust$Ind7<-recode(Ind, "c('Hospitality', 'Wholesale & Retail') = 1;	else = '0'")
DataProspAndCust$Ind8<-recode(Ind, "c('Recreation' , 'Construction') = 1;	else = '0'")
DataProspAndCust$Ind9<-recode(Ind, "c('Transportation','Chemicals') = 1;	else = '0'")
DataProspAndCust$Ind10<-recode(Ind, "c('Agriculture' , 'Other') = 1;	else = '0'")

saveRDS(DataProspAndCust, file="DataProspAndCust.rds")
summary(tbl_df(DataProspAndCust))


x <- filter(DataProspAndCust, interest == "1")
summary(x)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(Ind) ) +
  geom_bar() + coord_flip()

#charts per plot
par(mfrow=c(1,2))
x<-select(DataProspAndCust, interest, Ind1)

# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(Ind1) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind2)

# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind2) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind3)

# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind3) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind4)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind4) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind5)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind5) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind6)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind6) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind7)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind7) ) +
  geom_bar() + coord_flip()


x<-select(DataProspAndCust, interest, Ind8)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind8) ) +
  geom_bar() + coord_flip()

x<-select(DataProspAndCust, interest, Ind9)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind9) ) +
  geom_bar() + coord_flip()


x<-select(DataProspAndCust, interest, Ind10)
# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
ggplot(x1, aes(Ind10) ) +
  geom_bar() + coord_flip()

# Filter by to keep all intersted
x1 <- filter(x, interest == "1")
#frequency charts
#horizontal titles
#ggplot(x1, aes(Cntry) ) +
#  geom_bar()
#veritical title
ggplot(x1, aes(Ind2) ) +
  geom_bar() + coord_flip()




#Split dataframe into training and test set on Interest
# another approach to splitting dataL
smp_size <- floor(0.75 * nrow(DataProspAndCust))
set.seed(1234)
trainIndex <- sample(seq_len(nrow(DataProspAndCust)), size = smp_size)
#Split data based on even representation of Interest in both test and train

#set.seed(1234)
#trainIndex <- createDataPartition(DataProspAndCust$interest, p = 0.7, list = FALSE)

#Create Training dataframe
DataPCTrain <- DataProspAndCust[trainIndex, ]

#Lets calculate mean and SD of train vals
DataTrainMeans <- colMeans(DataPCTrain[, c('Rev','EmCnt','CoOppy','CoOppDays','CoMQLCt','CoOppWCt','CoOppLCt',
                                           'CoOppOpCt','CoOppyMo','CoOppyWMo','CoOppyLMo','CoOppyOMo')])

DataTrainSD <- apply(DataPCTrain[, c('Rev','EmCnt','CoOppy','CoOppDays','CoMQLCt','CoOppWCt','CoOppLCt',
                                  'CoOppOpCt','CoOppyMo','CoOppyWMo','CoOppyLMo','CoOppyOMo')], MARGIN = 2, FUN = sd)
#Create Test dataframe
DataPCTest <- DataProspAndCust[-trainIndex, ]

#Repeated 10 fold cross validation on training 
#http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

  #Normalize training set by calculating the mean and standard deviation
  #Then subtracting the mean from individual numeric feature values and dividing
  #by the standard deviation.
  #Normalizing only columns 6:17. DataPCTrain will have the new normalized columns
DataPCNormalTrain <- cbind(scale(DataPCTrain[,6:17]), DataPCTrain[, -c(6:17)])

#save
#saveRDS(DataPCNormalTrain, file="DataPCNormalTrain.rds")
#saveRDS(DataPCTest, file="DataPCTest.rds")
#saveRDS(DataPCTrain, file="DataPCTrain.rds")



#Normalize Test using values calculated from the training set
#First transformation of test will be subtracting mean of train from test vals

DataPCNormalTestTrans1 <- sweep(DataPCTest[,c('Rev','EmCnt','CoOppy','CoOppDays','CoMQLCt','CoOppWCt','CoOppLCt',
                                            'CoOppOpCt','CoOppyMo','CoOppyWMo','CoOppyLMo','CoOppyOMo')], 2, DataTrainMeans, "-")

#Second take the first transformation and divide by training SD
DataPCNormalTestTrans2 <- sweep(DataPCNormalTestTrans1, 2, DataTrainSD, "/")


#Create the normalized test set
DataPCNormalTest <- cbind(DataPCNormalTestTrans2, DataPCTest[,c('interest', 'IsCust','Cntry1','Cntry2','ActSz1',
                                                                 'ActSz2','ActSz3','Ind1','Ind2','Ind3','Ind4',
                                                                 'Ind5','Ind6','Ind7','Ind8','Ind9','Ind10')] )

#saveRDS(DataPCNormalTest, file = "DataPCNormalTest.rds")


#Run the logistic regression on DataPCNormalTrain
Allmod_fitGLM <- glm(interest ~ Rev + EmCnt + CoOppy + CoOppDays +
                   CoMQLCt + CoOppWCt + CoOppLCt + CoOppOpCt + CoOppyMo + CoOppyWMo +
                   CoOppyLMo + CoOppyOMo + Cntry1 + Cntry2 + ActSz1 + ActSz2 +
                   ActSz3 + Ind1 + Ind2 + Ind3 + Ind4 + Ind5 + Ind6 + Ind7 + Ind8 +
                   Ind9 + Ind10,  data=DataPCNormalTrain, family="binomial")
saveRDS(Allmod_fitGLM, file ="Allmod_fitGLM.rdata")

#check for multicolinearity on statisticall significant variables
#########test of colinearity
colintest<-DataProspAndCust
rm(DataProspAndCust)
colintest$testTarget<-DataProspAndCust$Rev
colin<-lm(testTarget~ CoOppy + CoOppDays +
            CoMQLCt + CoOppWCt + CoOppLCt + CoOppyMo +
            CoOppyLMo + CoOppyOMo, data=colintest)

# Evaluate Collinearity
vif(colin) # variance inflation factors 
colintestall<-vif(colin)
sqrt(vif(colin)) > 2 # problem?

#remove CoOppyMo
colin2<-lm(testTarget~ CoOppy + CoOppDays +
            CoMQLCt + CoOppWCt + CoOppLCt +
            CoOppyLMo + CoOppyOMo, data=colintest)

# Evaluate Collinearity
vif(colin2) # variance inflation factors 
sqrt(vif(colin2)) > 2 # problem?

#remove CoOppyMo
colin2<-lm(testTarget~ CoOppy + CoOppDays +
             CoMQLCt + CoOppWCt + CoOppLCt +
             CoOppyLMo + CoOppyOMo, data=colintest)

#remove CoOppy
colin3<-lm(testTarget~ CoOppDays +
             CoMQLCt + CoOppWCt + CoOppLCt +
             CoOppyLMo + CoOppyOMo, data=colintest)


# Evaluate Collinearity
vif(colin3) # variance inflation factors 
sqrt(vif(colin3)) > 2 # problem?
rm(colin, colin2, colin3)

#Run a Step wise logit regression removing variable with mulitcolinearity tested

mod_fit2 <- step(glm(interest ~ Rev + EmCnt + CoOppDays +
                 CoMQLCt + CoOppWCt + CoOppLCt + CoOppOpCt + CoOppyWMo +
                 CoOppyLMo + CoOppyOMo + Cntry1 + Cntry2 + ActSz1 + ActSz2 +
                 ActSz3 + Ind1 + Ind2 + Ind3 + Ind4 + Ind5 + Ind6 + Ind7 + Ind8 +
                 Ind9 + Ind10,  data=DataPCNormalTrain, family="binomial"))

#Run a logistic regression with only the without multicolin variables
Allmod_fitGLM2 <- glm(interest ~ Rev + EmCnt + CoOppDays +
                       CoMQLCt + CoOppWCt + CoOppLCt + CoOppOpCt + CoOppyWMo +
                       CoOppyLMo + CoOppyOMo + Cntry1 + Cntry2 + ActSz1 + ActSz2 +
                       ActSz3 + Ind1 + Ind2 + Ind3 + Ind4 + Ind5 + Ind6 + Ind7 + Ind8 +
                       Ind9 + Ind10,  data=DataPCNormalTrain, family="binomial")
summary(Allmod_fitGLM2)


#Run a logistic regression with only the without multicolin variables and statist significant variables
mod_fitStatSig <- glm(interest ~ CoOppDays +
                        CoMQLCt + CoOppWCt + CoOppLCt + CoOppyWMo +
                        CoOppyLMo + CoOppyOMo + Cntry2,  data=DataPCNormalTrain, family="binomial")
summary(mod_fitStatSig)

#Multicollinearity check using plots
NumVarableTest<-cbind(DataPCTrain$Rev, DataPCTrain$EmCnt, DataPCTrain$CoOppy,
                      DataPCTrain$CoOppDays, DataPCTrain$CoMQLCt,
                      DataPCTrain$CoOppWCt, DataPCTrain$CoOppLCt, DataPCTrain$CoOppOpCt,
                      DataPCTrain$CoOppyMo, DataPCTrain$CoOppyWMo, DataPCTrain$CoOppyLMo,
                      DataPCTrain$CoOppyOMo)

x<-cor(NumVarableTest)
library(corrplot)
corrplot(x, method = "circle")
corrplot(x, method = "square")
corrplot(x, method = "number")

#Get probabilities on the training data of 0 or 1 class: using stat sig model without multicol variables best AIC Score
probTrainData = predict(mod_fitStatSig, type = c("response"))
#Adding probtraindata probab column to DataPCNormalTrain dataset
DataPCNormalTrain$interest2 = probTrainData
#confustion matrix on training data
confusion.matrix(DataPCNormalTrain$interest, probTrainData, threshold = 0.5)

#Apply our model to test to predict
probTestData = predict(mod_fitStatSig, type = c("response"))
predTst <- predict(mod_fitStatSig, DataPCNormalTest, type="response")

confusion.matrix(DataPCNormalTest$interest, predTst, threshold = 0.5)


#________________stepwise train and test__________________________________
#Get probabilities for the regression using reduced stepwise regression reduced attribute set and apply to training
probTrainData2 = predict(mod_fit2, type = c("response"))
#Adding probtraindata probab column to DataPCNormalTrain dataset
DataPCNormalTrainStep<-DataPCNormalTrain  # keeping stepwise seperate
DataPCNormalTrainStep$interest2 = probTrainData2
#confustion matrix on training data
confusion.matrix(DataPCNormalTrain$interest, probTrainData2, threshold = 0.5)

  #Apply our model to test to predict
probTestData = predict(mod_fitStatSig, type = c("response"))
predTst <- predict(mod_fitStatSig, DataPCNormalTest, type="response")

confusion.matrix(DataPCNormalTest$interest, predTst, threshold = 0.5)


#_____________












probTrainData3 = predict(mod_fitStepRed, type = c("response"))

probTrainData2 = predict(mod_fit, type = "response")
sum(probTrainData2 == DataPCNormalTrain$interest)
length(probTrainData2)
table(probTrainData, DataPCNormalTrain$interest)
probTrainData2 = predict(mod_fit, type = "prob")


#confusion matrix for regression results using stat sig attributes
confusion.matrix(DataPCNormalTrain$interest, probTrainData3, threshold = 0.5)



#Likelihood based confidence interval for all the independent variables
confint(mod_fit)

#Run an anova model to analyze table of deviance
anova(mod_fit, test = "Chisq")

#The difference between the null deviance and the residual deviance shows how 
#our model is doing against the null model (a model with only the intercept). 
#The wider this gap, the better. Analyzing the table we can see the drop in deviance
#when adding each variable one at a time. Again, adding attributes 
#significantly reduces the residual deviance. The other variables seem to 
#improve the model less even with a low p-value. A large p-value here 
#indicates that the model without the variable explains more or less the same amount
#of variation. Ultimately what you would like to see is a significant drop in deviance and the AIC.

#While no exact equivalent to the R2 of linear regression exists, 
#the McFadden R2 index can be used to assess the model fit.Need library pscl
pR2(mod_fit2)

#Get the odds by exponentiating the estimates
cbind(exp(coef(mod_fit)), exp(confint(mod_fit)))

#The statistic to determine the overall significance of a logistic model is the 
#likelihood ratio test. It compares the likelihood of the full model (with all the predictors included) 
#with the likelihood of the null model (which contains only the intercept). 
#It is analogous to the overall F-test in linear regression. The likelihood ratio 
#test statistic is: G_0^2 = 2ln\frac{L}{L_0} where L is the likelihood of the full 
#model and L_0 is the likelihood of the null model. The likelihood ratio 
#test statistic (G_0^2 = 41.73) can be compared to a \chi^2 distribution with 3 degrees of freedom.


mod_fit.null <- glm(interest ~ 1, family = binomial, data = DataPCNormalTrain)
summary(mod_fit.null)
lr.mod_fit <- -(deviance(mod_fit) / 2)
lr.mod_fit.null <- -(deviance(mod_fit.null) / 2)
(lr <- 2 * (lr.mod_fit - lr.mod_fit.null))
1 - pchisq(lr, 2)

pchisq(deviance(mod_fit), df.residual(mod_fit), lower = FALSE)


#We will now use a Support Vector machine. Default kernel is 
DataSVMTrain <- svm(interest ~ Rev + EmCnt + CoOppy + CoOppDays +
                   CoMQLCt + CoOppWCt + CoOppLCt + CoOppOpCt + CoOppyMo + CoOppyWMo +
                   CoOppyLMo + CoOppyOMo + Cntry1 + Cntry2 + ActSz1 + ActSz2 +
                   ActSz3 + Ind1 + Ind2 + Ind3 + Ind4 + Ind5 + Ind6 + Ind7 + Ind8 +
                   Ind9 + Ind10,  data=DataPCNormalTrain)
predictSVMTrain <- predict(DataSVMTrain, DataPCNormalTrain)
summary(DataSVMTrain)
confusion.matrix(DataPCNormalTrain$interest, predictSVMTrain, threshold = 0.5)

#Apply SVM to test data
predSVMTest <- predict(DataSVMTrain, DataPCNormalTest, type="response")
confusion.matrix(DataPCNormalTest$interest, predSVMTest)
