specdir<-"C:/Users/Kevin/Documents/R/Projects/specdata2/specdata"

pollutantmean<-function(directory,pollutant,id = 1:332){
  setwd(directory)
  on.exit(setwd("C:/Users/Kevin/Documents/R/Projects/specdata3"))

  sulfatesum<-0
  sulfateobs<-0
  nitratesum<-0
  nitrateobs<-0
  
  if(pollutant=="sulfate"){
    for(i in id){
      if(i<10){
        file1<-paste("00",i,".csv",sep="")
        sulfatesum<-sulfatesum+sum(read.csv(file1)[,"sulfate"],na.rm = T)
        sulfateobs<-sulfateobs+sum(!is.na(read.csv(file1)[,"sulfate"]))
      }
      if(i>9 && i<100){
        file1<-paste("0",i,".csv",sep="")
        sulfatesum<-sulfatesum+sum(read.csv(file1)[,"sulfate"],na.rm = T)
        sulfateobs<-sulfateobs+sum(!is.na(read.csv(file1)[,"sulfate"]))
      }
      if(i>99){
        file1<-paste(i,".csv",sep="")
        sulfatesum<-sulfatesum+sum(read.csv(file1)[,"sulfate"],na.rm = T)
        sulfateobs<-sulfateobs+sum(!is.na(read.csv(file1)[,"sulfate"]))
      }
    }
    print(sulfatesum/sulfateobs)
  }
  if(pollutant=="nitrate"){
    for(i in id){
      if(i<10){
        file1<-paste("00",i,".csv",sep="")
        nitratesum<-nitratesum+sum(read.csv(file1)[,"nitrate"],na.rm = T)
        nitrateobs<-nitrateobs+sum(!is.na(read.csv(file1)[,"nitrate"]))
      }
      if(i>9 && i<100){
        file1<-paste("0",i,".csv",sep="")
        nitratesum<-nitratesum+sum(read.csv(file1)[,"nitrate"],na.rm = T)
        nitrateobs<-nitrateobs+sum(!is.na(read.csv(file1)[,"nitrate"]))
      }
      if(i>99){
        file1<-paste(i,".csv",sep="")
        nitratesum<-nitratesum+sum(read.csv(file1)[,"nitrate"],na.rm = T)
        nitrateobs<-nitrateobs+sum(!is.na(read.csv(file1)[,"nitrate"]))
      }
    }
    return(nitratesum/nitrateobs)
  }

}

pollutantmean(specdir,"nitrate",70:72)

#This function was created for part 1 of week 2 assignment. I was having problems in Rstudio and github as I went about 
#creating the repo & project improperly which ultimately led to a lot of Rstudio crashes, inability to open files and perform
#calculations of any kind. I re-made a new project with properly setting up a Repo and made this from scratch instead of 
#even attempting to open the previous R script I kept with the function. I think this is fairly neat and to the point. 
#I'm sure there must be a better way to access the named files without having to split i into 3 categories to handle the
#leading zeroes, but this is clean enough. 
#mean calculated manually to avoid an inconsistencies. 