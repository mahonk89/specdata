specdir<-"C:/Users/Kevin/Documents/R/Projects/specdata2/specdata"

completeok<-function(directory,id = 1:332){
  setwd(directory)
  initialdir<-"C:/Users/Kevin/Documents/R/Projects/specdata3"
  matrix<-matrix(ncol = 2,nrow = length(id))
  r<-1
  for(i in id){
    
    if(i<10){
      file1<-paste("00",i,sep="")
      file2<-paste(file1,".csv",sep="")
      
      nitvect<-c(!is.na(read.csv(file2)[,"nitrate"]))
      sulvect<-c(!is.na(read.csv(file2)[,"sulfate"]))
      vectsum<-nitvect+sulvect
      filesum<-sum(vectsum[vectsum==2])/2
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    if(i<100 && i>9){
      file1<-paste("0",i,sep="")
      file2<-paste(file1,".csv",sep="")
      
      nitvect<-c(!is.na(read.csv(file2)[,"nitrate"]))
      sulvect<-c(!is.na(read.csv(file2)[,"sulfate"]))
      vectsum<-nitvect+sulvect
      filesum<-sum(vectsum[vectsum==2])/2
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    if(i>99){
      file1<-paste(i,sep="")
      file2<-paste(file1,".csv",sep="")
      
      nitvect<-c(!is.na(read.csv(file2)[,"nitrate"]))
      sulvect<-c(!is.na(read.csv(file2)[,"sulfate"]))
      vectsum<-nitvect+sulvect
      filesum<-sum(vectsum[vectsum==2])/2
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    
    r<-r+1
  }
  df<-as.data.frame(matrix)
  colnames(df)<-c("id","nobs")
  print(df)
  setwd(initialdir)
}



complete(specdir,c(2,4,8,10,12))
complete(specdir,300:332)

#This function creates a matrix with 2 columns and rows = the length of the id argument, values are empty. The for loop
#again is split to compensate for the leading 0s of the source files using 3 if statements. It makes file1 the name of the 
#csv file without .csv which is used in the matrix. file2 is the variable for the full text file to use in read.csv. 
#filesum reads the csv file, subsets to columns titled sulfate and nitrate and counts the total number of non-NA figures.
#That is exactly what is needed for the function. 
#The second part sets those values to specific places in the matrix using r as a counting variable, set as 1 at the start. 
#after the for loop is done, it changes the matrix to a data frame and renames the columns to the requested values and prints
#the data frame. 

#the coursera site seems to be off. The question is asking for all complete observations (including nitrate and sulfate)
#but the example output seems to only cuont the complete observations of 1 of the pollutants. 
#it only counts sulfate figures when it should be both. If I remove nitrate from the subset, the values match the
#example exactly. Since this isn't what the question is asking for, I'd rather leave it as is instead of trying to
#match their results - either way, I know how. 

#if the pollutants being measured are different for each file, it would be a good idea to subset as [,c(2,3)] as opposed
#to using the actual column names. 

#a fun experiment would be to add the total line at the bottom after the for loop is done. Another time.

#Pause: at this time I think what I have written should produce the sum of the number of complete observations (all non NA)
#however, nothing is being done with file sum, it will just keep overwriting that figure without doing it. 
#I think I could either create a list of 2 value vectors containing the file name and number of completed observations
#and combine to DF, or just populate the columns systematically. 

setwd(specdir)
sum(subset(read.csv("030.csv"),!is.na(nitrate) & !is.na(sulfate)))

bivect2<-rbinom(10,1,0.5)
bivect2
bivect+bivect2
d30<-read.csv("030.csv")
d30nan<-c(!is.na(d30[,"nitrate"]))
d30nas<-c(!is.na(d30[,"sulfate"]))
d30sum<-d30nan+d30nas
sum(d30sum[d30sum==2])/2

#updated using an archaic albeit effective manner. It appears that it was asking for complete values where complete values
#is defined as both !is.na("nitrate" & "sulfate")= T. Apparently, wherever there is a value for sulfate, there is always
#a non-NA value for nitrate, however the converse is not true. So the minimum value (sulfate) was matching the complete value
#number.
#I updated the filesum value to take the sum of !is.na(nitrate) and !is.na(sulfate) vectors where values are 2 (if the value
#is 2, then both columns had a non-NA value in that row and is a complete case). The simplest way for me to then find number
#of complete cases was take that sum of (TRUE+TRUE) and divide by 2. 

#Again, I'm sure there's a better way to subset the data to only include values if not NA in both column 2 and col 3
#but after a decent amount of testing (less googling) I wasn't able to come up with a way which was significantly more
#simple than the vector addition

#COMPLETE CASES!!!!!!

complete<-function(directory,id = 1:332){
  setwd(paste("C:/Users/Kevin/Documents/R/Projects/specdata2/",directory,sep=""))
  on.exit(setwd("C:/Users/Kevin/Documents/R/Projects/specdata3"))

  matrix<-matrix(ncol = 2,nrow = length(id))
  r<-1
  for(i in id){
    
    if(i<10){
      file1<-paste("00",i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))

      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    if(i<100 && i>9){
      file1<-paste("0",i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    if(i>99){
      file1<-paste(i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    
    r<-r+1
  }
  df<-as.data.frame(matrix)
  colnames(df)<-c("id","nobs")
  return(df)

}

complete(specdir)


completenoprint<-function(directory,id = 1:332){
  setwd(directory)
  initialdir<-"C:/Users/Kevin/Documents/R/Projects/specdata3"
  matrix<-matrix(ncol = 2,nrow = length(id))
  r<-1
  for(i in id){
    
    if(i<10){
      file1<-paste("00",i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    if(i<100 && i>9){
      file1<-paste("0",i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    if(i>99){
      file1<-paste(i,sep="")
      file2<-read.csv(paste(file1,".csv",sep=""))
      
      filesum<-sum(complete.cases(file2[,c(2,3)]))
      
      matrix[r,1]<-file1
      matrix[r,2]<-filesum
    }
    
    r<-r+1
  }
  df<-as.data.frame(matrix)
  colnames(df)<-c("id","nobs")
  assign("comp332",df,envir = globalenv())  ###
  setwd(initialdir)
}


