#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform retrospective analysis SS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 15/01/2025 #
#~~~~~~~~~~~~~~~~~~~~~~


## Press Ctrl + Shift + O to see the document outline

## Run model retros and make plots

# Edit here! -------------------------------------------------------------------

rm(list=ls()) ## Clean environment
library(r4ss) 
library(icesAdvice)

mod_path <- paste0(getwd(), "./Model/M0 base model/", sep="") ## CHANGE name

## Dir for retros
plotdir_retro<- paste(mod_path, "/retros", sep="")
dir.create(plotdir_retro)

# Do retros --------------------------------------------------------------------

## Retros analysis by creating new directories, copying model files, and 
## changing the starter file to set the number of years of data to exclude

## SS.exe must be in the model folder

yper=0:-5 ## years period for retros

SS_doRetro(masterdir=mod_path, oldsubdir="", newsubdir = "retros", extras="-nox",
           subdirstart = "retro",years = yper, overwrite = TRUE, exefile = "ss")

retroModels <- SSgetoutput(dirvec=file.path(mod_path, "retros",
                                             paste("retro",yper,sep="")))

# save(retroModels, file=paste0(mod_path, 
#                               "/retros/retroModels.RData", sep=""))

retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1

endyrvec <- retroSummary$endyrs + yper

# R4ss plots -------------------------------------------------------------------

## Comparison plot pdf
SSplotComparisons(retroSummary, endyrvec=endyrvec, xlim=c(1994,2020), 
                  legendlabels=paste("Data",yper,"years"), print=FALSE, pdf=TRUE, 
                  plotdir = plotdir_retro)
## Retro recruits
SSplotRetroRecruits(retroSummary, endyrvec=endyrvec, cohorts=1983:2019,
                    relative=TRUE, legend=FALSE)
dev.off()

## Mohn's rho ss 

SSmohnsrho(retroSummary, endyrvec = endyrvec, startyr = 1960, verbose = TRUE)

# Summary plot CF --------------------------------------------------------------

## Our own retro plot

## SSB -------------------------------------------------------------------------

len_re=length(yper)-1

## Delete forecast years
library(reshape2)
library(ggplot2)
library(stringr)
startyr<-unique(retroSummary$startyrs)
endyr<-unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears<-length(years)
nforecastyears<-3 # Set to 3 in starter file
yearsfore <- c(years, years[nyears]+(1:nforecastyears))
nyearsfore=length(yearsfore)-length(years)
SSB <- as.data.frame(retroSummary[16])
nrSSB=nrow(SSB)
seq_aux=((nrSSB-nyearsfore)+1):nrSSB
SSB <- SSB[-c(1,2,seq_aux),] 
SSB <- SSB[,-(length(yper)+1)]
names(SSB) <- c(paste0("Retro",yper), "Year")

## Correct last years for each retro
endyr=max(endyrvec)
ind2=which(SSB$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(SSB$Year==endyrvec[i])
  SSB[(ind1+1):ind2,i]=NA
}
SSBm=SSB
SSB <- melt(SSB, id="Year")
names(SSB) <- c("Year", "Retro", "SSB")
ssb <- ggplot(SSB, aes(Year, SSB, col=Retro))+geom_line() +theme_classic()+xlim(1994,2020)
  
ssb_c <- ggplot(SSB, aes(Year, SSB, col=Retro))+geom_line() +theme_classic() +
 ggtitle (paste('rho', round(mohn(SSBm, peels = len_re),3), sep = " = "))+ theme(plot.title = element_text(size=10))+
        theme(plot.title = element_text(hjust = 0.5))+xlim(1980,2020) +ylim (0,70000)

ssb_c

## F (SS) ----------------------------------------------------------------------

## Delete forecast years
Fvalue <- as.data.frame(retroSummary[29])
nrF=nrow(Fvalue)
seq_aux=((nrF-nyearsfore)+1):nrF
Fvalue <- Fvalue[-c(seq_aux),] # Note that F starts directly in 1953
Fvalue <- Fvalue[,-(length(yper)+1)]
names(Fvalue) <- c(paste0("Retro",yper), "Year")

## Correct last years for each retro
endyr=max(endyrvec)
ind2=which(Fvalue$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(Fvalue$Year==endyrvec[i])
  Fvalue[(ind1+1):ind2,i]=NA
}

Fvaluem <- melt(Fvalue, id="Year")
names(Fvaluem) <- c("Year", "Retro", "F")
F.plot <- ggplot(Fvaluem, aes(Year, F, col=Retro))+geom_line() +theme_classic()+xlim(1994,2020)
F_c.plot <- ggplot(Fvaluem, aes(Year, F, col=Retro))+geom_line() +theme_classic()+
ggtitle(paste('rho', round(mohn(Fvalue, peels = len_re),3), sep = " = "))+ theme(plot.title = element_text(size=10))+
        theme(plot.title = element_text(hjust = 0.5))+xlim(1980,2020) +ylim (0,0.7)

## Rec -------------------------------------------------------------------------

## Delete forecast years
Recr <- as.data.frame(retroSummary[37])
nrRecr=nrow(Recr)
seq_aux=((nrRecr-nyearsfore)+1):nrRecr
Recr <- Recr[-c(1,2,seq_aux),] 
Recr <- Recr[,-(length(yper)+1)]
names(Recr) <- c(paste0("Retro",yper), "Year")
## Correct last years for each retro
endyr=max(endyrvec)
ind2=which(Recr$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(Recr$Year==endyrvec[i])
  Recr[(ind1+1):ind2,i]=NA
}
Recrm=Recr
Recr <- melt(Recr, id="Year")
names(Recr) <- c("Year", "Retro", "Recruitment")
Recruit.plot <- ggplot(Recr, aes(Year, Recruitment, col=Retro))+geom_line() +
  theme_classic()+ ggtitle (paste('rho', round(mohn(Recrm, peels = len_re),3), sep = " = "))+ theme(plot.title = element_text(size=10))+
        theme(plot.title = element_text(hjust = 0.5))

## Fbar ------------------------------------------------------------------------

## Note this is a different F than the one before (from SS)
nretros <- length(yper)
fileretr <- paste0("/retro",yper) 
fbar = list()

for (i in 1:nretros) {
  dir <- paste (mod_path, '/retros', fileretr[i], sep="")
  output <- SS_output(dir = dir,repfile = "Report.sso", 
                      compfile = "CompReport.sso",covarfile = "covar.sso", 
                      ncols = 200, forecast = T, warn = TRUE,covar = FALSE, 
                      checkcor = TRUE, cormax = 0.95, cormin = 0.01,
                      printhighcor = 10, printlowcor = 10, verbose = TRUE,
                      printstats = TRUE, hidewarn = TRUE, NoCompOK = FALSE,
                      aalmaxbinrange=0)
  lmin <- 20
  lmax <- 120
  years <- (output$startyr:output$endyr)
  nyears <- length(years)
  yearsfore <- c(years, years[nyears]+(1:output$nforecastyears))
  nyearsfore <- length(yearsfore)
  increments <- output$lbinspop[-1]-output$lbinspop[-output$nlbinspop]; increments <- c(increments, increments[length(increments)])
  len <- output$lbinspop + increments/2
  lenfatlen <- c(1:length(len))[(len>=lmin)&(len<=(lmax))]
  nfishfleets <- output$nfishfleets
  nfleets <- output$nfleets
  namesfleets <- output$FleetNames
  ages <- (0:output$accuage)
  nages <- length(ages)
  nseasons <- output$nseasons
  nmorphs <- length(output$morph_indexing$Index) 
  timeseries <- output$timeseries
  
  # Apical F~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  apicalf <- array( as.matrix(timeseries[timeseries$Yr >= years[1],substr(names(timeseries),1,3)=="F:_"]), dim=c(nseasons,nyearsfore, nfishfleets) ); dim(apicalf) 
  
  auxi <- output$sizeselex
  selex <- auxi[auxi$Yr >= years[1],]; selex[,1:5]
  unique(selex$Factor) # "Lsel" "Ret"  "Mort" "Keep" "Dead"
  # The factors of interest are total catch selectivity ("Lsel") and retention ogive ("Ret"):
  # selatlen
  selatlen <- array(1, dim=c(nyearsfore,nfleets,length(len)))
  
  # SELECTION-AT-LENGTH FOR EACH FLEET (INCLUDING ABUNDANCE INDICES) AND YEAR:
  for (fl in 1:nfleets){
    selc <- selex[(selex$Fleet==fl)&(selex$Factor=="Lsel"),]
    selc=subset(selc,selc$Sex==1) #! OJO  
    for(j in 1:nrow(selc)){ # years 
      if(j==1){
        indexyear <- 1
        selatlen[indexyear,fl,] <- as.numeric(selc[1,-(1:5)])
      }else{
        diff <- selc$Yr[j] - selc$Yr[j-1]
        
        if(diff>1){
          for(y in indexyear+c(1:(diff-1)))
          {selatlen[y,fl,]<-selatlen[indexyear,fl,]} # para todos los años que no aparecen datos antes del segundo año en data frame input data from 1
        }
        indexyear <- indexyear + diff
        
        selatlen[indexyear,fl,] <- as.numeric(selc[j,-(1:5)]) # inout of the corresponding year info
      }
    } 
    # if the last year is not the same as the last year for our model, we need to fill the remaining row using the info
    # of the last observed year info
    if(indexyear < dim(selatlen)[1]){ for(y in (indexyear+1):(dim(selatlen)[1])){selatlen[y,fl,]<-selatlen[indexyear,fl,]} }   
  }   
  
  # F-AT-LENGTH~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  (product of selection-at-length and apical f):
  fatlen <- array(dim=c(nseasons,nyearsfore,nfishfleets,length(len))); dim(fatlen)
  for(j in 1:nseasons){ fatlen[j,,,] <- as.vector(apicalf[j,,])*as.vector(selatlen[,1:nfishfleets,]) }
  ## Annual F-at-length, averaging over the 4 quarters, and adding up all the fishing fleets:
  ## first, averaging the 4 quarters, per fleet:
  fatlenannual <- apply(fatlen,c(2,3,4),mean)
  ## adding over fleets:
  Fatlenannual <- apply(fatlenannual,c(1,3),sum)
  ## CHECKING "fatlen" IN THE PROJECTION YEARS AS OBTAINED FROM THE SS3 INTERNAL FORECAST:
  # Checking that for each fleet and length class, "fatlen" is the same in all forecast years (i.e. that the following values are all 1):
  
  # Delete fleet 1 due to zero values in recent years (1953-1993)
  for(j in 1:(nyearsfore-nyears-1)){ print( range(fatlen[,nyearsfore,-1,]/fatlen[,nyearsfore-j,-1,] ) ) }
  fatlenave <- apply(fatlen[,,,lenfatlen],c(1,2,3),function(x){sum(x*increments[lenfatlen])/sum(increments[lenfatlen])})
  ## Average over the 4 Q in each year:
  fbarlenbyfleet <- apply(fatlenave,c(2,3),mean)
  ## Sum over fleets:
  fbarlen <- apply( fbarlenbyfleet, 1, sum)
  fbar[[i]] <- fbarlen # F for each year
}


# Fbar: F for each year
aux=fbar
fbar = as.data.frame(do.call(cbind, fbar))
fbar=cbind(fbar,yearsfore)
names(fbar) <- c(paste0("Retro",yper), "Year")
nrF=nrow(fbar)
## Delete forecast years
years <- (output$startyr:output$endyr)
yearsfore <- c(years, years[nyears]+(1:output$nforecastyears))
nyearsfore=length(yearsfore)-length(years)
endyrvec <- retroSummary$endyrs + yper
seq_aux=((nrF-nyearsfore)+1):nrF
fbar <- fbar[-c(seq_aux),]
## Correct last years for each retro
ind2=which(fbar$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(fbar$Year==endyrvec[i])
  fbar[(ind1+1):ind2,i]=NA
}
fbarm=fbar
fbar <- melt(fbar, id="Year")
names(fbar) <- c("Year", "Retro", "Fbar")
Fbar.plot <- ggplot(fbar, aes(Year, Fbar, col=Retro))+geom_line() +
             theme_classic() + xlim(1994,2020) + ggtitle ("Average yearly Fbar")+
  ggtitle(paste('rho', round(mohn(fbarm, peels = len_re),3), sep = " = "))+ theme(plot.title = element_text(size=10))+
        theme(plot.title = element_text(hjust = 0.5))

library(ggpubr)
combined_plot <- ggarrange(ssb_c, F_c.plot, nrow = 2, ncol = 1 ,
                           common.legend = TRUE, 
                           legend = "bottom")

# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave(paste(plotdir_retro, "/retros summary CF.jpeg"), plot = combined_plot, width = 6, height = 5, dpi = 300)
