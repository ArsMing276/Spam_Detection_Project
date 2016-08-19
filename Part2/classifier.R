##This for classify email messages into HAM or SPAM

##First we list the code of 20 methods we use
##use stringr library
library(stringr)

##tr                trainMessages list

is.Spam <- function(tr){
  name <- names(tr)
  spam <- grepl("spam", name)
  
  spam
}

is.Re <- function(tr){
  isre <- vector(length = length(tr))
  #Get our the emails including subject
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  
  #If Re: at the beginning of subject
  isre[subjind] <- ifelse(regexpr("Re:", subject) == 1, TRUE, FALSE)  
  isre[!subjind] <- FALSE
  isre
  
}

numLinesInBody <- function(tr){
  numbody <- unlist(sapply(tr, function(x) length(x$body)))
  names(numbody) <- NULL
  numbody
}

bodyCharacterCount <- function(tr){
  numchar<-unlist(sapply(tr, function(x) sum(nchar(x$body, type = "bytes"))))
  names(numchar) <- NULL
  numchar
}

replyUnderline <- function(tr){
  replyind <- unlist(sapply(tr, function(x) any("Reply-To" == names(x$header))))
  names(replyind) <- NULL
  underline <- unlist(sapply(tr[replyind], function(x) grepl("[_[:alnum:]]", x$header["Reply-To"])))
  names(underline) <- NULL
  replyind[replyind] <- underline
  
  replyind
  
  
}

subjectExclamationCount <- function(tr){
  countexclaim <- vector(length = length(tr))
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  countexclaim[subjind] <- str_count(subject, "!")
  countexclaim[!subjind] <- 0
  
  countexclaim
}

subjectQuestCount <- function(tr){
  countques <- vector(length = length(tr))
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  countques[subjind] <- str_count(subject, "\\?")
  countques[!subjind] <- 0
  
  countques
}

numAttachments <- function(tr){
  countattach <- unlist(sapply(tr, function(x) length(x$attachment)))
  names(countattach) <- NULL
  
  countattach
}

percentCapitals <- function(tr){
  percent <- vector(length = length(tr))
  bodyind <- sapply(tr, function(x) length(x$body) != 0)
  body <- sapply(tr, function(x) x$body)
  uplower <- unlist(sapply(body[bodyind], function(x) sum(str_count(x, "[a-zA-Z]"))))
  onlyupper <- unlist(sapply(body[bodyind], function(x) sum(str_count(x, "[A-Z]"))))
  percent[bodyind] <- onlyupper/uplower
 
  #Replace with NAN 
  percent[!bodyind] <- 0/0
  
  percent
}

isInReplyTo <- function(tr){
  isReto <- sapply(tr, function(x) "In-Reply-To" %in% names(x$header))
  
  isReto
}

hourSent <- function(tr){
  time <- vector(length = length(tr))
  Dateind <- sapply(tr, function(x) "Date" %in% names(x$header))
  Date <- unlist(sapply(tr[Dateind], function(x) x$header["Date"]))
  matched <- regexpr("[0-9]{1,2}:", Date)
  
  #Extract the matched string and substitute ":" with " "
  hoursent <- gsub(":", "", regmatches(Date, matched))
  time[Dateind] <- hoursent
  time[!Dateind] <- 00
  
  as.numeric(time)
}

multipartText <- function(tr){
  countmutext <- vector(length = length(tr))
  contypeind <- unlist(sapply(tr, function(x) "Content-Type" %in% names(x$header)))
  contenttype <- unlist(sapply(tr[contypeind], function(x) x$header["Content-Type"]))
  
  #There is no match for multipart/text, I match with only multipart
  countmutext[contypeind] <- grepl("[mM]ultipart", contenttype)
  countmutext[!contypeind] <- FALSE
  
  countmutext
}

subjectPunctuationCheck <- function(tr){
  puncheck <- vector(length = length(tr))
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  puncheck[subjind] <- grepl("[A-Za-z][0-9[:punct:]]+[A-Za-z]", subject)
  puncheck[!subjind] <- FALSE
  
  puncheck
}

subjectSpamWords <- function(tr){
  Spamword <- vector(length = length(tr))
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  Spamword[subjind] <- grepl("([Vv]iagra|[Pp]ounds|[Ff]ree|[Ww]eight|[Gg]uarantee|[Mm]illions|[Dd]ollars|[Cc]redit|[Rr]isk|[Pp]rescription|[Gg]eneric|[Dd]rug|[Mm]oney [Bb]ack|[Cc]redit [Cc]ard)", subject)
  Spamword[!subjind] <- FALSE
  
  Spamword
  
}

percentSubjectBlanks <- function(tr){
  percent <- vector(length = length(tr))
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  countblank <- str_count(subject, " ")
  countall <- str_count(subject, ".")
  percent[subjind] <- countblank/countall
  percent[!subjind] <- 0/0
  
  percent
}

isOriginalMessage <- function(tr){
  isorigin <- vector(length = length(tr))
  bodyind <- sapply(tr, function(x) length(x$body) != 0)
  body <- sapply(tr, function(x) x$body)
  isorigin[bodyind] <- unlist(sapply(body[bodyind], function(x) any(grepl("[Oo]riginal [Mm]essage", x))))
  isorigin[!bodyind] <- FALSE
  
  isorigin
}

numDollarSigns <- function(tr){
  numsign <- vector(length = length(tr))
  bodyind <- sapply(tr,function(x) length(x$body) != 0)
  body <- sapply(tr, function(x) x$body)
  numsign[bodyind] <- sapply(body[bodyind], function(x) sum(str_count(x, "\\$")))
  numsign[!bodyind] <- 0
  
  numsign
}

isDear <- function(tr){
  startdear <- vector(length = length(tr))
  bodyind <- sapply(tr, function(x) length(x$body) != 0)
  body <- sapply(tr, function(x) x$body)
  startdear[bodyind] <- unlist(sapply(body[bodyind], function(x) any(regexpr("[Dd](EAR|ear)", x) == 1)))
  startdear[!bodyind] <- FALSE
  
  startdear
}

isYelling <- function(tr){
  capsub <- vector(length = length(tr))
  subjind <- unlist(sapply(tr, function(x) "Subject" %in% names(x$header)))
  subject <- unlist(sapply(tr[subjind], function(x) x$header["Subject"]))
  capsub[subjind] <- ifelse(str_count(subject, "[A-Z]") == str_count(subject, "[a-zA-Z]"), TRUE, FALSE)
  capsub[!subjind] <- FALSE
  
  capsub
}

priority <- function(tr){
  highprior <- vector(length = length(tr))
  highprior1 <- vector(length = length(tr))
  highprior2 <- vector(length = length(tr))
  
  #Process X-Priority cases and put into a vector
  xpriorind <- unlist(sapply(tr, function(x) "X-Priority" %in% names(x$header)))
  xpriority <- unlist(sapply(tr[xpriorind], function(x) x$header["X-Priority"]))
  
  #both 1 and 2 are high priority
  highprior1[xpriorind] <- grepl("[12]", xpriority)
  
  #Process X-Msmail-Priority cases and put into a vector
  xmspriorind <- unlist(sapply(tr, function(x) "X-Msmail-Priority" %in% names(x$header)))
  xmspriority <- unlist(sapply(tr[xmspriorind], function(x) x$header["X-Msmail-Priority"]))
  highprior2[xmspriorind] <- grepl("[Hh](IGH|igh)", xmspriority)
  
  #priority is high for either of the them is high 
  highprior <- highprior1|highprior2
  
  highprior
}

numRecipients <- function(tr){
  countnum <- vector(length = length(tr))
  
  #Get out "To" numbers
  Toind <- unlist(sapply(tr, function(x) "To" %in% names(x$header)))
  Tomail <- unlist(sapply(tr[Toind], function(x) x$header["To"]))
  countnum[Toind] <- str_count(Tomail, ",") + 1
  countnum[!Toind] <- 0
  
  #Get out "Cc" numbers
  ccind <- unlist(sapply(tr, function(x) any(grepl("^[Cc]{2}$", names(x$header)))))
  ccmail <- sapply(tr[ccind], function(x) x$header[(names(x$header) == "Cc" | names(x$header) == "CC")])
  num <- unlist(sapply(ccmail, function(x) sum(str_count(x, ",")) + 1))
  countnum[ccind] <- countnum[ccind] + num
  
  countnum
              
}

#Next, we process all the data into a data frame with 20 columns of our classifiers
classdf <- function(tr){
  df <- data.frame(is.Re = is.Re(tr), numLinesInBody = numLinesInBody(tr), 
                   bodyCharacterCount = bodyCharacterCount(tr), replyUnderline = replyUnderline(tr), 
                   subjectExclamationCount = subjectExclamationCount(tr), subjectQuestCount = subjectQuestCount(tr), 
                   numAttachments = numAttachments(tr), percentCapitals = percentCapitals(tr), 
                   isInReplyTo = isInReplyTo(tr), hourSent = hourSent(tr), multipartText = multipartText(tr),
                   subjectPunctuationCheck = subjectPunctuationCheck(tr), subjectSpamWords = subjectSpamWords(tr), 
                   percentSubjectBlanks = percentSubjectBlanks(tr), isOriginalMessage = isOriginalMessage(tr), 
                   numDollarSigns = numDollarSigns(tr), isDear = isDear(tr), isYelling = isYelling(tr), priority = priority(tr),
                   numRecipients = numRecipients(tr), row.names = NULL)
  df
  
}

#########################################################################################################################
##Test whether variables work well

df<-classdf(trainMessages)
## for logical type
  TRUEclass <- is.Spam(trainMessages)
  nspam <- sum(TRUEclass)
  nham <- sum(!TRUEclass)

  islogic <- sapply(df[1,], is.logical)
  dflogic <- df[,islogic]
  dfnum <- df[,!islogic]
  
  logictable <- sapply(dflogic, function(x) c(table(x[1 : nham]), table(x[(nham + 1) : (nham + nspam)])))
  correctrate <- sapply(dflogic, function(x) mean(x == TRUEclass))
  

## for numeric type
 
  
  for(i in names(dfnum)){
    jpeg(file = paste(i, ".jpg", sep = ""))
    plot(dfnum[[i]], pch = c(rep(2, nham), rep(3, nspam)),col = c(rep("blue", nham), rep("green", nspam)), 
    main = paste("plot of", i, sep = " "), ylab = i, xlab = "email index")
    abline(h = median(dfnum[[i]][1 : nham]), v = nham, col = c("red", "red"))
    legend("topleft", legend = c("HAM", "SPAM"), pch = c(2,3),col = c("blue", "green"), box.lwd=2)
    dev.off()
  }
  
