setwd("d:/project/data")
getwd()

source("./mergeData.R")
source("./cleaning.R")
source("./relationshipsAnalysis.R")
source("./outlierAnalysis.R")
source("./ISODATA.R")

factorpath = "./factors/Years_in_School.csv"
causepath = "./per1000/Child-All_causes.csv"
destpath = "./test.csv"

pcapath = "./factor&single_cause/Child-All_causes_All_Factors.csv"
allCausespath = "./per1000/Merged.csv"

#data = read.csv(allCausespath)
#commonCauseRankOfCountries(data[, -c(1, 2)])
#mergeFactorAndCause(factorpath, causepath, destpath)
#mergeBatchFactorsAndCauses(factorDir = "./factors", causeDir = "./per1000", destDir = "./single_factor&single_cause")
#mergeFactorsAndSingleCause(factorDir = "./factors", causepath = causepath, destDir = "./factor&single_cause")
#mergeAllFactors2AllCauses(factorDir = "./factors", causeDir = "./per1000", destDir = "./factor&single_cause")
#correlationAnalysis(dataDir = "./")
data = read.csv(pcapath)[, 2:10]
ans = PCA(data)

#data = read.csv(causespath)
#causeRankList = causeRank(data)

# ISODATA clustering analysis
if(FALSE){
  data = read.csv("./factor&single_cause/Child-All_causes_All_Factors.csv")
  list = ISODATA(data)
  clustered.data = list$data
  cluster.means = list$cluster.means
  outliers = findOutLiers(clustered.data, cluster.means)
  calculateAllDistances(clustered.data, cluster.means)
}

