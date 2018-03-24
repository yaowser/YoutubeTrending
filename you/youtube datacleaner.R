cat("\014")
options(warn=1)

require(survey)
require(dplyr)
require(lattice)

#read in, remove columns, from https://www.kaggle.com/datasnaek/youtube-new/data
setwd("C:/Users/Yao/Desktop/you")
youtubeRawUS <- read.csv(file="USvideos.csv", header=TRUE, sep=",")
youtubeRawUS$country <- rep("US",nrow(youtubeRawUS))
youtubeRawUS <- youtubeRawUS[-c(3:4,7,12:16)]
youtubeRawCA <- read.csv(file="CAvideos.csv", header=TRUE, sep=",")
youtubeRawCA$country <- rep("CA",nrow(youtubeRawCA))
youtubeRawCA <- youtubeRawCA[-c(3:4,7,12:16)]
youtubeRawDE <- read.csv(file="DEvideos.csv", header=TRUE, sep=",")
youtubeRawDE$country <- rep("DE",nrow(youtubeRawDE))
youtubeRawDE <- youtubeRawDE[-c(3:4,7,12:16)]
youtubeRawFR <- read.csv(file="FRvideos.csv", header=TRUE, sep=",")
youtubeRawFR$country <- rep("FR",nrow(youtubeRawFR))
youtubeRawFR <- youtubeRawFR[-c(3:4,7,12:16)]
youtubeRawGB <- read.csv(file="GBvideos.csv", header=TRUE, sep=",")
youtubeRawGB$country <- rep("GB",nrow(youtubeRawGB))
youtubeRawGB <- youtubeRawGB[-c(3:4,7,12:16)]

youtubeRaw<- rbind(youtubeRawUS, youtubeRawCA)
youtubeRaw<- rbind(youtubeRaw, youtubeRawDE)
youtubeRaw<- rbind(youtubeRaw, youtubeRawFR)
youtubeRaw<- rbind(youtubeRaw, youtubeRawGB)

head(youtubeRaw)
youtubeRaw2 <- youtubeRaw
#remove duplicates because they can be trending in multiple months, keep the least views to get on trending
youtubeRaw2 = youtubeRaw2[order(youtubeRaw2[,'video_id'],youtubeRaw2[,'views']),]
youtubeRaw2 = youtubeRaw2[!duplicated(youtubeRaw2$video_id),]
head(youtubeRaw2)
write.csv(youtubeRaw2, file = "youtubeRaw2.csv")

#replace strata categories into real names
youtubeRaw2$category_id2[youtubeRaw2$category_id == '1'] <- 'Film & Animation'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '2'] <- 'Autos & Vehicles'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '10'] <- 'Music'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '15'] <- 'Pets & Animals'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '17'] <- 'Sports'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '19'] <- 'Travel & Events'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '20'] <- 'Gaming'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '22'] <- 'People & Blogs'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '23'] <- 'Comedy'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '24'] <- 'Entertainment'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '25'] <- 'News & Politics'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '26'] <- 'Howto & Style'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '27'] <- 'Education'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '28'] <- 'Science & Technology'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '29'] <- 'Nonprofits & Activism'
youtubeRaw2$category_id2[youtubeRaw2$category_id == '43'] <- 'Science & Technology'
youtubeRaw2$category_id[youtubeRaw2$category_id == '43'] <- '28'

#sanity check, remove column names
head(youtubeRaw2)
rownames(youtubeRaw2) <- c()
youtubeRaw3 <- youtubeRaw2[order(youtubeRaw2$category_id2),]
head(youtubeRaw3)
write.csv(youtubeRaw3, file = "youtubeRaw3.csv", row.names=FALSE)

#use sas for more youtubeRaw3.csv dataset stats

#check initial distribution and number of rows
boxplot(youtubeRaw3$views, main="Uncleaned Boxplot Distribution of Videos Views", xlab="Trending Videos", ylab="Number of Views")
nrow(youtubeRaw3)
bwplot(views ~ category_id2 , data = youtubeRaw3, scales=list(x=list(rot=45)), main="Uncleaned Boxplot Distribution of Videos Views", xlab="Trending Videos Categories", ylab="Number of Views")

#solve for view count without cleaning data
sum(as.numeric(youtubeRaw3$views))
max(youtubeRaw3$views)
min(youtubeRaw3$views)
mean(youtubeRaw3$views)

#remove outliers more than 1.5 quant, save into new clean dataset
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
youtubeClean <- youtubeRaw3
youtubeClean$views <- remove_outliers(youtubeRaw3$views)

#check new distribution, only keep data within distribution
boxplot(youtubeClean$views, main="Cleaned Boxplot Distribution of Videos Views", xlab="Trending Videos", ylab="Number of Views")
bwplot(views ~ category_id2 , data = youtubeClean, scales=list(x=list(rot=45)), main="Cleaned Boxplot Distribution of Videos Views", xlab="Trending Videos Categories", ylab="Number of Views")
youtubeClean2 <- youtubeClean[complete.cases(youtubeClean), ]
write.csv(youtubeClean2, file = "youtubeClean2.csv", row.names=FALSE)

#solve for actual target average view count and number of rows for strata
sum(as.numeric(youtubeClean2$views))
nrow(youtubeClean2)
max(youtubeClean2$views)
min(youtubeClean2$views)
mean(youtubeClean2$views)
sd(youtubeClean2$views)

#average amount of views for the outliers removed
(sum(as.numeric(youtubeRaw3$views)) - sum(as.numeric(youtubeClean2$views))) / (nrow(youtubeRaw3) - nrow(youtubeClean2))
max(youtubeRaw3$views)-max(youtubeClean2$views)
min(youtubeRaw3$views)-min(youtubeClean2$views)
mean(youtubeRaw3$views)-mean(youtubeClean2$views)
nrow(youtubeClean2)/nrow(youtubeRaw3)


#use sas for more youtubeClean2.csv dataset stats

#how do we choose MOE? currently, MOE = 5000 views
#do we remove outliers per strata or for the whole dataset? currently, we remove for whole dataset
#after removing outliers, 88% of the dataset is kept...do we ignore fpc adjustment? currently we ignore b/c more than 10%

n0srs <- ceiling((1.96^2*sd(youtubeClean2$views)^2)/(5000^2))
n0srs

#SRS function

SrsMeanEstimate<-function(Seed, SampSize, printOutput= TRUE){
  set.seed(Seed)
  
  youtubeClean2.SRSSampled = sample_n(youtubeClean2,SampSize)
  
  if(printOutput == TRUE){
    print(nrow(youtubeClean2.SRSSampled))
    print(bwplot(views ~ category_id2, data = youtubeClean2.SRSSampled, scales=list(x=list(rot=45)), main="SRS Boxplot Distribution of Videos Views", xlab="Trending Videos Categories", ylab="Number of Views"))
  }
  
  mydesign <- svydesign(id = ~1, data = youtubeClean2.SRSSampled)
  
  srsMean = svymean(~views, design = mydesign)
  srsSE = SE(srsMean)
  srsCI = confint(srsMean)
  
  rm(youtubeClean2.SRSSampled)
  rm(mydesign)
  
  return(list(as.numeric(srsMean[1]),
              as.numeric(srsSE),
              as.numeric(srsCI[1]),
              as.numeric(srsCI[2])
  )
  )
}

srsMean <- SrsMeanEstimate(n0srs, n0srs)
print(paste('The Mean Estimate =', srsMean[[1]]))
print(paste('The Standard Error =', srsMean[[2]]))
mean(youtubeClean2$views)

#Proportional Strata 

PropMeanEstimate<-function(Seed, SampSize, printOutput= TRUE){
  
  set.seed(Seed)
  
  # Identify Frequency of category_id2 Stratum
  PropFreq <- as.data.frame(table(youtubeClean2[,c("category_id2")]))
  names(PropFreq)[1] = 'category_id2'
  PropFreq
  
  PropFreq$N = nrow(youtubeClean2)
  PropFreq$p = PropFreq$Freq/PropFreq$N
  PropFreq$SampSizeh = (PropFreq$p * SampSize)
  PropFreq$SampSizehRounded = round(PropFreq$SampSizeh)
  
  youtubeClean2.PropSampled <- NULL
  
  for (i in 1:nrow(PropFreq)){
    youtubeClean2.PropSampled<-rbind(youtubeClean2.PropSampled,
                                sample_n(youtubeClean2[(youtubeClean2$category_id2 == PropFreq[i,"category_id2"]),]
                                         ,PropFreq[i,"SampSizehRounded"]))
  }
  
  if(printOutput == TRUE){
    print(PropFreq)
    print(nrow(youtubeClean2.PropSampled))
    print(bwplot(views ~ category_id2, data = youtubeClean2.PropSampled, scales=list(x=list(rot=45)), main="Prop Boxplot Distribution of Videos Views", xlab="Trending Videos Categories", ylab="Number of Views"))
  }
  
  mydesign <- svydesign(id = ~1, strata = ~category_id2, data = youtubeClean2.PropSampled)
  
  propMean = svymean(~views, design = mydesign)
  propSE = SE(propMean)
  propCI = confint(propMean)
  
  rm(youtubeClean2.PropSampled)
  rm(mydesign)
  propCI = confint(propMean)
  return(list(as.numeric(propMean[1]),
              as.numeric(propSE),
              as.numeric(propCI[1]),
              as.numeric(propCI[2])
  )
  )
}

#adjusting the sample size calculation?

propMean <- PropMeanEstimate(n0srs, n0srs)
print(paste('The Mean Estimate =', propMean[[1]]))
print(paste('The Standard Error =', propMean[[2]]))
mean(youtubeClean2$views)

#deff = se_complex/se_srs
deffProp = as.numeric(propMean[[2]]/srsMean[[2]])
deffProp

n0prop = ceiling(n0srs*deffProp)
n0prop

#prop adjusted for deff

propMean <- PropMeanEstimate(n0srs, n0prop)
print(paste('The Mean Estimate =', propMean[[1]]))
print(paste('The Standard Error =', propMean[[2]]))

#task 2


SeedList <- c(10000, 20000, 30000, 40000, 50000)

df<- NULL

#SRS Seed Executions
for (seed in SeedList){
  srsEstimate <- SrsMeanEstimate(seed, n0srs, FALSE)
  srsEstimate <- data.frame('SRS', seed, srsEstimate)
  names(srsEstimate) <- c("EstimateType","SeedValue", "MeanEstimate", "SE", "LowerCI", "UpperCI")
  df<- rbind(df,srsEstimate)
}

#Prop Seed Executions
for (seed in SeedList){
  PropEstimate <- PropMeanEstimate(seed, n0srs, FALSE)
  PropEstimate <- data.frame('Prop', seed, PropEstimate)
  names(PropEstimate) <- c("EstimateType","SeedValue", "MeanEstimate", "SE", "LowerCI", "UpperCI")
  df<- rbind(df,PropEstimate)
}

#Prop Seed Executions
for (seed in SeedList){
  PropEstimate <- PropMeanEstimate(seed, n0prop, FALSE)
  PropEstimate <- data.frame('Prop DE', seed, PropEstimate)
  names(PropEstimate) <- c("EstimateType","SeedValue", "MeanEstimate", "SE", "LowerCI", "UpperCI")
  df<- rbind(df,PropEstimate)
}

#Add True Mean Value, in-line with estimates
df$TrueMeanValue <- mean(youtubeClean2$views)

#Add Bool Value for whether the Conf Limit contains the True Mean Value
df$WithinConfLimit <- df$LowerCI <= df$TrueMeanValue & df$UpperCI >= df$TrueMeanValue

#Print Results
print(df)

winner = aggregate(df[, 3:7], list(df$EstimateType), mean)
winner
#Prop wins slightly
#What is the percentage that the actual value is in the 95% confidence intervals for each design?
#abs(94384.50-93891.7)/2565.645 = 19.20734%
#how do you phrase it? true value is within 19.20734% of standard error for SRS estimation?

winner$PercentFromTrueMean <- abs(winner$TrueMeanValue - winner$MeanEstimate)/winner$SE*100
print(winner)