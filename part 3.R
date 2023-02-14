specdir<-"C:/Users/Kevin/Documents/R/Projects/specdata2/specdata"

corr<-function(directory,threshold=0){
  on.exit(setwd("C:/Users/Kevin/Documents/R/Projects/specdata3"))
  
  setwd(paste("C:/Users/Kevin/Documents/R/Projects/specdata2/",directory,sep=""))
  sulfate<-numeric()
  nitrate<-numeric()

  idcol<-numeric()
  nobscol<-numeric()

  for(i in 1:332){
    
    if(i<10){
      file1<-paste("00",i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      idcol<-c(idcol,as.numeric(file1))
      nobscol<-c(nobscol,as.numeric(filesum))
    }
    if(i<100 && i>9){
      file1<-paste("0",i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      idcol<-c(idcol,as.numeric(file1))
      nobscol<-c(nobscol,as.numeric(filesum))
    }
    if(i>99){
      file1<-paste(i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      idcol<-c(idcol,as.numeric(file1))
      nobscol<-c(nobscol,as.numeric(filesum))
    }
  }

  matr<-cbind(idcol,nobscol)
 
  corvect<-numeric()
  for(j in 1:332){
    g<-matr[j,2]
    tb<-as.integer(g)

    if(tb > threshold){
      if(j<10){
        file5<-paste("00",j,sep="")
        file6<-read.csv(paste(file5,".csv",sep=""))
        
        sulfate<-file6[,2]
        nitrate<-file6[,3]

      }
      if(j<100 && j>9){
        file5<-paste("0",j,sep="")
        file6<-read.csv(paste(file5,".csv",sep=""))
        
        sulfate<-file6[,2]
        nitrate<-file6[,3]

      }
      if(j>99){
        file5<-paste(j,sep="")
        file6<-read.csv(paste(file5,".csv",sep=""))
        
        sulfate<-file6[,2]
        nitrate<-file6[,3]

      }

      cv<-cor(sulfate,nitrate,use="complete.obs")
      corvect<-c(corvect,cv)
    }
  }
  return(corvect)

}


#The above function completely re-uses the function from part 2 complete(), just copy and pasted with slight adjustments to 
#get the same data frame/matrix. I was having issues calling the function within corr() simply because I used print() as the
#output for complete() and not return. This didn't really give any valid data to work with. Once I updated to return() in
#complete(), I was able to simply create df<-complete("specdata") as that data frame inside this function corr(). 

#once the matrix has been filled out with all 332 files, I created a numeric vector corvect which was used to house all
#of the outputs of cor(nitrate,sulfate) based on successes given by the following for loop. for (j in 1:332) calls to the
#matrix from complete(1:332) row by row and pulls the value of nobs to compare to the threshold value. If it is greater
#than the threshold, it will add the entire sulfate & nitrate columns to their respective vector (including NAs) and then
#immediately call the cor() function on those vectors and save the output systematically to the correlation vector.
#if the nobs value for the given id=i is under the threshold, it simply skipped that number and moved to the next. 
#

#I felt as though I had the most success with the argument in cor(use='complete.obs') as opposed to pairwise.complete.obs.
#I'm not entirely 100% sure what the difference was as I did a lot of troubleshooting and variable changes to get this to
#work but complete.obs definitely produced the desired output. 

head(corr("specdata",400))

summary(corr("specdata",400))
cr<-corr("specdata")
length(cr)

summary(complete(specdir))

summary(corr2("specdata",400))


##testing using another function input inside this one with correct return() output which seems to be much better than print

corr2<-function(directory,threshold=0){
  on.exit(setwd("C:/Users/Kevin/Documents/R/Projects/specdata3"))
  
  
  sulfate<-numeric()
  nitrate<-numeric()
  
  df<-complete("specdata")
  setwd(paste("C:/Users/Kevin/Documents/R/Projects/specdata2/",directory,sep=""))
  
  corvect<-numeric()
  for(j in 1:332){
    g<-df[j,2]
    tb<-as.integer(g)
    
    if(tb > threshold){
      if(j<10){
        file5<-paste("00",j,sep="")
        file6<-read.csv(paste(file5,".csv",sep=""))
        
        sulfate<-file6[,2]
        nitrate<-file6[,3]
        
      }
      if(j<100 && j>9){
        file5<-paste("0",j,sep="")
        file6<-read.csv(paste(file5,".csv",sep=""))
        
        sulfate<-file6[,2]
        nitrate<-file6[,3]
        
      }
      if(j>99){
        file5<-paste(j,sep="")
        file6<-read.csv(paste(file5,".csv",sep=""))
        
        sulfate<-file6[,2]
        nitrate<-file6[,3]
        
      }
      
      cv<-cor(sulfate,nitrate,use="complete.obs")
      corvect<-c(corvect,cv)
    }
  }
  return(corvect)
  
}


