specdir<-"C:/Users/Kevin/Documents/R/Projects/specdata2/specdata"

complete<-function(directory,id = 1:332){
  setwd(directory)
  initialdir<-"C:/Users/Kevin/Documents/R/Projects/specdata3"
  matrix<-matrix(ncol = 2,nrow = length(id))
  r<-1
  for(i in id){
    
    if(i<10){
      file1<-paste("00",i,sep="")
      file2<-paste(file1,".csv",sep="")
      filesum<-sum(!is.na(read.csv(file2)[,c("sulfate","nitrate")]))
      
      matrix[r,]<-file1
      matrix[r,2]<-filesum
    }
    if(i<100 && i>9){
      file1<-paste("0",i,sep="")
      file2<-paste(file1,".csv",sep="")
      filesum<-sum(!is.na(read.csv(file2)[,c("sulfate","nitrate")]))
      
      matrix[r,]<-file1
      matrix[r,2]<-filesum
    }
    if(i>99){
      file1<-paste(i,sep="")
      file2<-paste(file1,".csv",sep="")
      filesum<-sum(!is.na(read.csv(file2)[,c("sulfate","nitrate")]))
      
      matrix[r,]<-file1
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
complete(specdir,46:71)

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