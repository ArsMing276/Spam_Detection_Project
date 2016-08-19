#This is the code for classification tree algorithm

library("lattice")

#get numeric variables
islogic <- sapply(trainVariables[1,], is.logical)
trainVarnum <- trainVariables[, !islogic]

#emploration of variable transformation by plotting
for(i in names(trainVarnum)){
  trainVarnum$isSpam <- trainVariables$isSpam
  ind <- which(names(trainVarnum) == i)
  jpeg(file = paste(i, ".jpg", sep = ""))
  
  plot1 <- densityplot( ~trainVarnum[,ind], trainVarnum, group = isSpam, auto.key = TRUE, xlab = i)
  plot2 <- densityplot( ~log(trainVarnum[,ind]), trainVarnum, group = isSpam, auto.key = TRUE, xlab = paste("log", i, sep = ""))
  plot3 <- densityplot( ~(trainVarnum[,ind])^2, trainVarnum, group = isSpam, auto.key = TRUE, xlab = paste("sqr", i, sep = ""))
  plot4 <- densityplot( ~sin(trainVarnum[,ind]), trainVarnum, group = isSpam, auto.key = TRUE, xlab = paste("sin", i, sep = ""))
  print(plot1, position = c(0, 0, 1/2, 1/2), more = TRUE)
  print(plot2, position = c(0, 1/2, 1/2, 1), more = TRUE)
  print(plot3, position = c(1/2, 1/2, 1, 1), more = TRUE)
  print(plot4, position = c(1/2, 0, 1, 1/2))
  dev.off()
  
}

##By comparision, apply transformed variables to fit the classification tree

library("rpart")
library("rpart.plot")
trainVariables$bodyCharacterCount <- log(trainVariables$bodyCharacterCount)
trainVariables$numAttachments <- log(trainVariables$numAttachments)
trainVariables$numLinesInBody <- log(trainVariables$numLinesInBody)
trainVariables$percentCapitals <- log(trainVariables$percentCapitals)
trainVariables$subjectExclamationCount <- sin(trainVariables$subjectExclamationCount)
trainVariables$percentForwards <- log(trainVariables$percentForwards)
trainVariables$subjectQuestCount <- log(trainVariables$subjectQuestCount)
trainVariables$percentHTMLTags <- log(trainVariables$percentHTMLTags)

#Fitted the model
classtree <- rpart(isSpam~., data = trainVariables, method = "class")
rpart.plot(classtree)

#Training data correctrate and confusion matrix
predicted <- predict(classtree, newdata = subset(trainVariables, select = -isSpam), type = "class")
traincorrectrate <- mean(predicted == trainVariables$isSpam)
trainconfusionmat <- table(data.frame(predicted = predicted, truevalue = trainVariables$isSpam))

#Test data with actual results correctrate and confusion matrix
testVariables$bodyCharacterCount <- log(testVariables$bodyCharacterCount)
testVariables$numAttachments <- log(testVariables$numAttachments)
testVariables$numLinesInBody <- log(testVariables$numLinesInBody)
testVariables$percentCapitals <- log(testVariables$percentCapitals)
testVariables$subjectExclamationCount <- sin(testVariables$subjectExclamationCount)
testVariables$percentForwards <- log(testVariables$percentForwards)
testVariables$subjectQuestCount <- log(testVariables$subjectQuestCount)
testVariables$percentHTMLTags <- log(testVariables$percentHTMLTags)
predcorrectrate <- mean(predict(classtree, newdata = subset(testVariables, select = -isSpam), type = "class") == testVariables$isSpam)
testconfusionmat <- table(data.frame(predicted = predict(classtree, newdata = subset(testVariables, select = -isSpam), type = "class"), truevalue = testVariables$isSpam))

#Explore missclassified observations in training data
#misclassification index
treemisind <- which(predicted != trainVariables$isSpam)

#Plot density plot of each numeric variable for right classified and misclassified obs.
#Be aware to use original not transformed trainVariables. 
facind <- ifelse(1:nrow(trainVariables) %in% treemisind, FALSE, TRUE)
library("lattice")
islogic <- sapply(trainVariables[1,], is.logical)
trainVarnum <- trainVariables[, !islogic]
trainVarlog <- trainVariables[, islogic]

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
ratio <- mean(treemisind %in% missingind)


#Explore the residuals
knnmis<-c(244, 365, 426, 469, 593,  692,  717,  743,  791,  831,  961, 1130, 1187, 1213, 1252,1255, 1298, 1318, 1319, 1328, 1702, 1717, 1725, 1762, 1764, 1818, 1835, 1845, 1847, 1871,1886, 1910, 1920, 1923, 1929)
treemis<-c(24,   43,   53,   58,  194,  197,  250,  300,  364,  437,  462,  534,  574,  616,  708,709, 
           756,  796,  812,  861,  885,  913, 1086, 1108, 1116, 1170, 1248, 1249, 1309, 1322,1404, 
           1417, 1446, 1457, 1460, 1466, 1476, 1482, 1483, 1488, 1497 ,1507, 1510 ,1513 ,1518,
           1524, 1525 ,1526 ,1527, 1528, 1535, 1537, 1538, 1540, 1542 ,1550, 1551, 1554, 1556, 1557,
           1558, 1560, 1562, 1563, 1565, 1566, 1569, 1571, 1575, 1577, 1578, 1580, 1583, 1584, 1585,
           1587, 1592, 1595, 1596, 1598, 1600, 1602, 1604, 1608, 1610, 1612 ,1618, 1620, 1621, 1627,
           1629, 1631, 1632 ,1633 ,1634, 1639 ,1643 ,1645 ,1648, 1658, 1659, 1661, 1664, 1666, 1667,
           1668, 1671, 1672, 1673 ,1686, 1689, 1690, 1698, 1700, 1701, 1708 ,1711, 1713, 1716, 1730,
           1735, 1744, 1745, 1747, 1749 ,1750, 1752 ,1756, 1758, 1761, 1767 ,1774, 1776, 1777 ,1778,
           1785 ,1790, 1791, 1792 ,1794, 1796 ,1797 ,1799 ,1806, 1813, 1817, 1821, 1823, 1824, 1826,
           1838, 1840, 1841, 1844, 1848, 1852, 1857, 1862, 1869, 1874, 1883, 1884, 1887, 1890, 1901,
           1905, 1914, 1916, 1919, 1924, 1928, 1938 ,1944, 1945, 1946, 1949, 1953, 1960, 1963, 1967,
           1969, 1970 ,1974, 1976 ,1996)
test<-testVariables[c(knnmis,treemis),]
factest<-c(rep("knn",length(knnmis)),rep("tree",length(treemis)))

#Plot density plot of each numeric variable for two types of misclassification.
library("lattice")
islogic <- sapply(test[1,], is.logical)
testVarnum <- test[, !islogic]
testVarlog <- test[, islogic]

for(i in names(testVarnum)){
  testVarnum$factest <- factest
  ind <- which(names(testVarnum) == i)
  jpeg(file = paste(i, ".jpg", sep = ""))
  plot <- densityplot( ~testVarnum[,ind], testVarnum, group = factest, auto.key = TRUE, xlab = i)
  print(plot)
  dev.off()
  
}

#get table of each logical variable for two types of misclassification.
tblist <- by(as.data.frame(sapply(testVarlog, nasub)), factest, function(x) sapply(x, mean))

