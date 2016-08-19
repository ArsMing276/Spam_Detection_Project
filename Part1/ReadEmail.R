#A function to read all emails in a directory
Rdallemail <- function(dir){
  #dir           Specific directory of the folder to read
  
  setwd(dir)
  
  #List all files in the path
  paths = list.files(dir, recursive = TRUE)
  lenpath <- length(paths)
  
  #Get detailed paths for all files
  dirs <- paste(dir, paths, sep="/")
  trainMessages <- vector("list", length = lenpath)
  
  for (i in 1 : lenpath) {
    #Apply function to each file and store them into a big list
    con <- file(dirs[i],open = "rt")
    infile <- readLines(con)
    close(con)
    sublist <- emailist(infile)
    trainMessages[[i]] <- sublist
    
  }
  
  #names the list with path name
  names(trainMessages) <- paths
  trainMessages
}

#Reading One Email Message into the standard form

emailist <- function(file){
  #File:      file of each email after read in by readLine function
  
  #One special case in spam 
  if(substring(file[1], 1, 3) == "mv "){
    list(object = file)
  }
  
  else{
    
    #Substitute uppercase BOUNDARY with boundary
    boundaline<-grep("BOUNDARY",file)
    file[boundaline]<-gsub("BOUNDARY","boundary",file[boundaline])
   
    #Substitute type with Type
    contentline<-grep("Content-",file)
    file[contentline]<-gsub("type","Type",file[contentline])
    
    #Make EmailHead
    blank <- which(file == "")
    
    if(grepl("From ", file[1]))
      heady <- file[2 : (blank[1] - 1)]
    else
      heady <- file[1 : (blank[1] - 1)]
    
    #Substitute "\t" for " " in order to use read.dcf 
    subheady <- gsub("\t", " ", heady)
    header.con <- textConnection(subheady)
    header <- read.dcf(header.con, all=TRUE)
    
    #List all variables in head and corresponding classes
    list <- sapply(names(header), function(x) class(header[[x]]))
    
    if(all(list != "list")){
      
      #No list class
      head <- as.vector(header)
      head <- paste(colnames(header), head, sep=" = ")
      
    }
    
    else{
      #One or more list class in 
      
      #Part one with list class
      names <- names(list[list == "list"])
      charRev <- unlist(header[names])
      Revpart <- paste(names(charRev), charRev, sep=" = ")
      
      #Part two without list class 
      indrev <- sapply(1 : length(names), function(x) which(names(header) == names[x]))
      Elsevec <- as.vector(do.call(rbind, header[, -indrev])[, 1])
      Elspart <- paste(names(header)[-indrev], Elsevec, sep=" = ")
      
      #Combine two parts into the head
      head<-c(Revpart, Elspart) 
      
    }
    
    #Make EmailBody and EmailAttachment
    if(any(grepl("boundary", head))){
      
      #With Attachment
      #Make EmailBody
      Elsepart <- file[blank[1]:length(file)]
      
      #Find boundary
      #Filter with ";" , "boundary=" and "\""
      boundproto <- strsplit(head[grep("boundary", head)], ";")[[1]]
      bound <- strsplit(boundproto[grep("boundary=", boundproto)], "boundary=")[[1]]
      bound <- gsub("[\"]", "", bound[2])
      
      #In some cases the boundary in head is not exactly equals to that in attachment
      #Filter boundary in attchament with part of bound we found in head  
      bound <- substring(bound, 3, nchar(bound))
      boundary <- Elsepart[grep(bound, Elsepart, fixed = TRUE, useBytes = TRUE)[1]]
      endboundary <- paste(boundary, "--", sep = "")
      
      #Find positions of boundary
      grepbound <- grep(boundary, file, fixed = TRUE, useBytes = TRUE)
      
      if(!any(grepl(endboundary, Elsepart)) && any(grepl("Content-Type", file[(grepbound[length(grepbound)] + 1) : length(file)]))){
        
        #Without a end boundary and there is a Head after the last boundary
        #Get body part and get rid of all ""
        body <- file[blank[1] : (grepbound[1] - 1)]
        body <- body[body != ""]
        
        #Get attachment part 
        Attpart <- file[grepbound[1] : length(file)]
        Attpart <- Attpart[Attpart != ""]
        
        #Split attachments, make each attachment a list with head and body and put all attachments into a big list
        fac <- cumsum(Attpart==boundary)
        Attsplit <- split(Attpart, fac)
        attachlist <- lapply(Attsplit, function(x) list(AttachHead = sub(":", " = ", x[grep("Content-Type",x)[1]]), AttachBody = x[-c(1,grep("Content-Type",x)[1])]) )
         
        list(HEAD = head, BODY = body, ATTACHMENT = attachlist)  
        
      }
      
      else{
        #There is a end boundary or there is no head after the last boundary or both of them
        
        #get boundary lines and endline of attachment
        boundline <- grepbound[-length(grepbound)]
        endline <- grepbound[length(grepbound)]
        
        #Get the bodies before and after attachments and combine them 
        bodybfatt <- file[blank[1] : (boundline[1] - 1)]
        bodyafatt <- file[(endline + 1) : length(file)]
        body <- c(bodybfatt, bodyafatt)
        
        #get rid of all ""
        body <- body[body != ""]
        
        #Get attachment part of a email and get rid of all ""
        Attpart <- file[boundline[1] : (endline - 1)]
        Attpart <- Attpart[Attpart != ""]
        
        #Split attachments, make each attachment a list with head and body and put all attachments into a big list
        fac <- cumsum(Attpart==boundary)
        Attsplit <- split(Attpart, fac)
        attachlist <- vector("list", length(Attsplit))
        
        for(i in 1:length(Attsplit)){
          
          #Attachment with a head
          if(any(grepl("Content-Type", Attsplit[[i]]))){
            attachlist[[i]] <- list(AttachHead = sub(":", " = ", Attsplit[[i]][grep("Content-Type", Attsplit[[i]])[1]]), AttachBody = Attsplit[[i]][-c(1, grep("Content-Type", Attsplit[[i]])[1])])
          }
          else{
            #Attachment without a head
            attachlist[[i]] <- list(AttachBody = Attsplit[[i]][-1])
          }
        }
        
        list(HEAD = head, BODY = body, ATTACHMENT = attachlist)  
        
      }
    }
    
    else{
      
      #Without Attachment
      body <- file[(blank[1] + 1) : length(file)]
      
      #Get rid of all "" 
      body <- body[body != ""]
      
      list(HEAD = head, BODY = body)
    }
  }
}
