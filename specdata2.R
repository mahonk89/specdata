pollutantmean<-function(directory,pol,id=1:332){
  setwd(paste("C:/Users/Kevin/Documents/R/Projects/specdata2/",directory,sep=""))
  i<-min(id)
  totalnitsum<-0
  totalnitobs<-0
  totalsulsum<-0
  totalsulobs<-0
  if(pol=="nitrate"){
    for(i in min(id):max(id)){
      if(i<10){ #portion which sums nitrate values in files 001 thru 009
        file1<-paste("00",i,".csv",sep="")
        totalnitsum<-totalnitsum+sum(read.csv(file1)[,3],na.rm=T)
        totalnitobs<-totalnitobs+sum(!is.na(read.csv(file1)[,3]))
        
      } else if(10<=i && i<100) { #portion which sums nitrate values in files 010 through 099
        file1<-paste("0",i,".csv",sep="")
        totalnitsum<-totalnitsum+sum(read.csv(file1)[,3],na.rm=T)
        totalnitobs<-totalnitobs+sum(!is.na(read.csv(file1)[,3]))
        
      } else { #portion which sums nitrate values in files 100 through 332 (no leading zeroes)
        file1<-paste(i,".csv",sep="")
        totalnitsum<-totalnitsum+sum(read.csv(file1)[,3],na.rm=T)
        totalnitobs<-totalnitobs+sum(!is.na(read.csv(file1)[,3]))
      }
    }
    print(totalnitsum)
    print(totalnitobs)
    print(totalnitsum/totalnitobs)
  }
  if(pol=="sulfate"){
    for(i in min(id):max(id)){
      if(i<10){ #portion which sums sulfate values in files 001 thru 009
        file1<-paste("00",i,".csv",sep="")
        totalsulsum<-totalsulsum+sum(read.csv(file1)[,2],na.rm=T)
        totalsulobs<-totalsulobs+sum(!is.na(read.csv(file1)[,2]))
        
      } else if(10<=i && i<100) { #portion which sums sulfate values in files 010 through 099
        file1<-paste("0",i,".csv",sep="")
        totalsulsum<-totalsulsum+sum(read.csv(file1)[,2],na.rm=T)
        totalsulobs<-totalsulobs+sum(!is.na(read.csv(file1)[,2]))
        
      } else { #portion which sums sulfate values in files 100 through 332 (no leading zeroes)
        file1<-paste(i,".csv",sep="")
        totalsulsum<-totalsulsum+sum(read.csv(file1)[,2],na.rm=T)
        totalsulobs<-totalsulobs+sum(!is.na(read.csv(file1)[,2]))
      }
      
      
    }
    print(totalsulsum) #line not required
    print(totalsulobs) #line not required
    print(totalsulsum/totalsulobs)
  }
  
}
#I chose to take the sum of each column and the count of !NA values from each data set and calculate the mean that way. I know 
#colMeans exists, but when running this loop function I'm not sure how the colmeans function would act. (0.5+0.6)/2=0.55
#however 100/200=0.5 and 600/1000=0.6 but 700/1200=0.5833. I forget the principle, but this way of calculating should be much 
#more reliant. 

#test function run
pollutantmean("specdata","nitrate",23)
pollutantmean("specdata","sulfate",1:10)

#testing summing of NA values
sum(!is.na(read.csv("001.csv")[,3]))

#date=column 1
#sulfate=column 2
#nitrate=column 3
#ID=column 4 // same as filename without leading 0s. 

pollutantmean("specdata","nitrate",1:15)
pollutantmean("specdata","nitrate",10:15)
pollutantmean("specdata","nitrate",c(1:5,10:15))
#The above selection where ids are 1:5, 10:15 does NOT work. It will only calculate the average from 1:15, not excluding
#the figures 11:14
