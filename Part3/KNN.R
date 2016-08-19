library("nnet")

#This is the code for KNN algorithm
kNNclassify <- function (trainingdata, testdata, k, dist = "euclidean", p = 2, idx){
  #trainingdata                           trainingdata
  #testdata                               testdata
  #k                                      number of the nearest neighbors
  #dist                                   distance method to choose 
  #p                                      The power of the Minkowski distance
  #idx                                    The observation index of testdata, used to find misclassified index 
  #apply majority voting method for each observation 
  
  #Separate True values from training and test data
  isSpamtrain <- trainingdata$isSpam
  trainingdata <- trainingdata[,-which(names(trainingdata) == "isSpam")]
  isSpamtest <- testdata$isSpam
  testdata <- testdata[,-which(names(testdata) == "isSpam")]
  
  #Handle missing values
  trainingdata <- as.data.frame(sapply(trainingdata, nasub))
  testdata <- as.data.frame(sapply(testdata, nasub))
  
  #Rescale all variables
  trainingdata <- scale(trainingdata)
  testdata <- scale(testdata)
  
  trainrow <- nrow(trainingdata)
  testrow <- nrow(testdata)
  
  distance <- dist(rbind(trainingdata, testdata), method = dist, p = p)
  distmat <- as.matrix(distance)[(trainrow + 1):(trainrow + testrow), 1:trainrow]
  orderk <- apply(distmat, 1, order)[1:k,]
  
  if(k == 1){
  
  #Only the nearest 1 points
  predspam <- sapply(as.data.frame(orderk), function(x) isSpamtrain[x]) 
  }
  
  else{
    
  #K nearest points
  Spamlist <- sapply(as.data.frame(orderk), function(x) table(isSpamtrain[x]))
  predspam <- sapply(Spamlist, function(x) names(x)[which.is.max(x)])
  predspam <- as.logical(predspam)
  }
  
  correctrate <- mean(predspam == isSpamtest)
  confusionmat <- table(data.frame(predicted=predspam,trueValue=isSpamtest))
  
  print(idx[which(predspam != isSpamtest)])
  print(confusionmat)
  correctrate
  
}

#Cross-Validation code
CVclassify<-function(trainVariables, grp, k, dist = "euclidean", p = 2){
  #trainVariables                             training dataset
  #grp                                        number of groups in the cross validation
  #k                                          number of the nearest neighbors
  #dist                                       distance method to choose 
  #p                                          The power of the Minkowski distance
  
  #Permute observations
  sampleind <- sample(nrow(trainVariables), nrow(trainVariables), replace = FALSE)
  trainVariables <- trainVariables[sampleind,]
  
  #divide training dataset into given groups, remainder obervations will be allocated to former groups randomly.
  grpnum <- nrow(trainVariables)%/%grp
  leftnum <- nrow(trainVariables)%%grp
  grpind <- sample(grp, leftnum, replace = FALSE)
  factor <- as.factor(c(gl(grp, grpnum), grpind))
  CVlist <- split(trainVariables, factor)
  idx <- split(sampleind, factor)
  
  #For each group, apply KNN algorithm
  correctvec <- vector(length = grp)
  for(i in 1:grp){
    testdata <- CVlist[[i]]
    trainingdata <- do.call(rbind, CVlist[-i])
    correctvec[i] <- kNNclassify(trainingdata, testdata, k = k, dist = dist, p = p, idx = idx[[i]])
    
  }
  
  #Get the mean correctrate as final correctrate
  meancorrect <- mean(correctvec)
  meancorrect
}


#Test for different k and distance method to get the best cv correcrrate
testkdist <- function(trainVariables, grp, k, dist){
  
  #trainVariables                             training dataset
  #grp                                        number of groups in the cross validation
  #k                                          number of the nearest neighbors
  #dist                                       A MATRIX! With first column as chosen distance, senond column as minkowski power
  
  nk <-length(k) 
  ndist <- nrow(dist)
  parmat <- matrix(ncol = nk, nrow = ndist)
  for(i in 1:ndist){
    for(j in 1:nk){
      parmat[i,j] <- CVclassify(trainVariables, grp = grp, k = k[j], dist = dist[i,][1], p = as.numeric(dist[i,][2]))
    }
  }
  rownames(parmat) <- dist[, 1]
  colnames(parmat) <- as.character(k)
  
  parmat
}

#A function to replace missing values.
nasub<- function (x){
  if(is.logical(x[1])){
    
    #Missing values will be replaced by which occurs the most.
    x[which(is.na(x))] <- as.logical(names(which.max(table(x))))  
  }
  else{
    
    #Missing values will be replaced by mean of the whole variable.
    x[which(is.na(x))] <- mean(x, na.rm = TRUE)
  }
  x
}

############################################################################

#Decide which distance k combination shall we choose.
 k <- 1:5
 dist <- matrix(c("manhattan", "euclidean", "maximum", "minkowski", "minkowski", 2, 2, 2, 3, 4), ncol = 2)
 testkdist(trainVariables, 5, k, dist)

#By comparison, I decide to use k=1 dist="manhattan" to fit the model.
#Training data correctrate and confusion matrix
 traincorrectrate<-CVclassify(trainVariables,5,1,dist="manhattan")

#Test data with actual results correctrate and confusion matrix
 predcorrectrate<-kNNclassify(trainVariables,testVariables,1,dist="manhattan")

#Explore missclassified observations in training data
#Misclassification index
KNNmisind <- c(3567, 2176, 5528, 4915, 2360, 3444, 4312, 4996, 1723, 3886, 6289, 4046, 1873, 2214, 2480,
          4885, 737, 3086, 2438, 4524, 1236, 1011, 3284, 2838, 3435, 1130,  200, 6193, 5181, 6318,
          616, 1413, 2031,  827, 1837,  163, 1372, 1727, 3270, 4198, 1314, 5090, 5739, 3511, 5659,
          3502, 1303,  738, 2194, 3644, 1542, 2050, 3992,  959,   26, 1926,   83, 4056, 6097, 2221,
          1008, 2389, 2509, 5533, 4973,  983, 4725, 4445, 5843, 5686, 1129, 5027,  986, 3136, 6133, 3714,
          3306, 2956, 1943, 5444, 5064, 1094, 4787, 2972, 1140, 5448, 5060, 1792, 2350, 5375,  720,
          3070, 1905, 4270,  932, 6272, 3667, 5408, 2901, 5588, 1430, 6207, 2810,   66, 5042, 2904,
          5031, 5637, 1309, 5441, 5506, 2569, 3897, 2532, 2044, 6347, 4786,  414, 5148, 5392, 4402,
          2475, 4486, 2372, 6118, 1306,  902, 2776,  404,  231, 6379, 3253, 1831,
          665, 1966, 3659, 1618, 6464, 5081, 1956, 4713, 6524, 3251, 2469, 5709, 3321, 5047, 6386,
          2474, 1492, 2925, 3790, 1212, 3137, 5220,  106, 6008,  210, 2716,  842, 4768, 3750, 2900,
          6066, 1646, 6143, 2243, 4290, 6425, 5109, 5722, 5281, 5085,  642, 3530, 3856, 4453, 4412,
          3676, 6346, 4328, 4592, 2674, 5957, 3479, 2002, 1185,  613, 3207, 6279, 5775, 1279, 1299,
          2835,  307, 2882, 4119, 3074, 1343, 4176, 1329, 1338, 3689, 1003,
          6235, 6121, 4325, 191, 1118, 2105, 2368, 5398,   41, 1321,  646, 2268, 4677, 5848, 6093,
          203, 5515, 1404, 5942, 1436, 5513, 4055, 3454, 1606, 4059, 3451, 1049, 4529, 4246, 4896,
          39, 4122,  601, 5255, 6405, 2910, 3803,   15, 3943, 5016,   94, 1716, 1269, 4420, 2769,
          5498, 1736, 4638, 5658, 4415, 3412, 2709, 4489, 6044, 3692,  666, 2807, 2729, 3019, 3665,
          4165, 3645, 6499,  153, 4413, 460, 5984,  299, 1050, 4339, 4657, 4395, 1065, 2633,   13,
          3513, 2921, 3693, 4829, 5923, 1335,  943, 3962, 2340, 2843, 2879, 6507,  271, 1567, 2916, 3155,  
          887, 3740,  488, 2232, 2036, 5785,4063, 3653, 2935, 3015, 2110, 2370, 3048, 5820,  786, 2677,  
          540, 6276, 1371, 2005, 3621,3983, 1191,  886, 4649, 2451, 4084, 1787,  922, 5333, 5303, 2545, 3420, 
          5118, 3254, 3840,4675, 4247, 4442, 4479, 4266
)

#Plot density plot of each numeric variable for right classified and misclassified obs.
facind <- ifelse(1:nrow(trainVariables) %in% KNNmisind, FALSE, TRUE)
library("lattice")
islogic <- sapply(trainVariables[1,], is.logical)
trainVarnum <- trainVariables[,!islogic]
trainVarlog <- trainVariables[,islogic]

for(i in names(trainVarnum)){
  trainVarnum$facind <- facind
  ind <- which(names(trainVarnum) == i)
  jpeg(file = paste(i, ".jpg", sep = ""))
  plot <- densityplot( ~trainVarnum[,ind], trainVarnum, group = facind, auto.key = TRUE, xlab = i)
  print(plot)
  dev.off()
  
}

#get table of each logical variable for right classified and misclassified obs.
tblist <- by(as.data.frame(sapply(trainVarlog, nasub)), facind, function(x) sapply(x, mean))

#Missing value influence
missingind <- which(is.na(apply(as.matrix(trainVariables), 1, mean)))
ratio <- mean(KNNmisind %in% missingind)
