dataset1<-read.csv("001.csv") #testing wd and creating df using read.csv

#SCRATCH THE REST BELOW. IF NEEDED, use the first for loop. 

for(i in 1:332){
    
    filename<-paste(i,".csv",sep="")
  
  dataset <- paste("monitor",i,sep="") 
  assign(dataset,read.csv(filename))
}

#IGNORE BELOW
#The above for loop creates a dataframe titled monitor_001 through monitor_009 using the titles of excel files of the same names
#I'm splitting it up to keep the df name consistent with the name of the csv. I could be using a lot of nested if's/loops to
#keep track of the additional 00s that must be added for the first 99 values but I'm going to do separate fors

for(i in 10:99){
  
  filename<-paste("0",i,".csv",sep="")
  
  dataset <- paste("monitor","0",i,sep="") 
  assign(dataset,read.csv(filename))
}

#for the values from 10-99

for(i in 100:332){
  
  filename<-paste(i,".csv",sep="")
  
  dataset <- paste("monitor",i,sep="") 
  assign(dataset,read.csv(filename))
}

#Although I'm doing it to be consistent with the file names, I feel like it may bite me in the butt later for purpose of ease
#Whereas if I just labeled them monitor1, monitor4, monitor26, etc I would be able to just add a loop i<-i+1 and paste with
#"monitor" instead of having leading zeroes. We'll have to see if there is a setting to maintain leading 0s.

#IGNORE ABOVE
#IGNORE ABOVE

#Free space



if(i<10){
  file1<-paste("00",i,".csv",sep="")
  totalcolsum<-totalcolsum+sum(read.csv(file1)[,3],na.rm=T)
  
  } else if(10<=i<99) {
     file1<-paste("0",i,".csv",sep="")
     totalcolsum<-totalcolsum+sum(read.csv(file1)[,3],na.rm=T)
  
    } else {
        file1<-paste(i,".csv",sep="")
        totalcolsum<-totalcolsum+sum(read.csv(file1)[,3],na.rm=T)
}


###adding new section for sulfate. Copied directly from nitrate section, must change out all nit, nitrate to sul, sulfate
#also since subsetting in the read.csv, MUST change the column from 3 to 2, otherwise the figures are exactly the same
#and there will be no error for "sulfate" input instead of "nitrate"

if(pol=="sulfate"){
  for(i in min(id):max(id)){
    if(i<10){ #portion which sums sulfate values in files 001 thru 009
      file1<-paste("00",i,".csv",sep="")
      totalsulsum<-totalsulsum+sum(read.csv(file1)[,3],na.rm=T)
      totalsulobs<-totalsulobs+sum(!is.na(read.csv(file1)[,3]))
      
    } else if(10<=i && i<100) { #portion which sums sulfate values in files 010 through 099
      file1<-paste("0",i,".csv",sep="")
      totalsulsum<-totalsulsum+sum(read.csv(file1)[,3],na.rm=T)
      totalsulobs<-totalsulobs+sum(!is.na(read.csv(file1)[,3]))
      
    } else { #portion which sums sulfate values in files 100 through 332 (no leading zeroes)
      file1<-paste(i,".csv",sep="")
      totalsulsum<-totalsulsum+sum(read.csv(file1)[,3],na.rm=T)
      totalsulobs<-totalsulobs+sum(!is.na(read.csv(file1)[,3]))
    }
    
    
  }
  print(totalsulsum/totalsulobs)
}











