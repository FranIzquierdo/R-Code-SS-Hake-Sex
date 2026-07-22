#~~~~~~~~~~~~~~~~~~~~~~
# Perform SS forecast #
#~~~~~~~~~~~~~~~~~~~~~~
# Modified 12/06/2026 #
#~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
# Marta Cousido       #
#~~~~~~~~~~~~~~~~~~~~~#                                                                                                        

## Press Ctrl + Shift + O to see the document outline

# 1) Create scenarios ----------------------------------------------------------

library(r4ss)
run<-"M0 base model"
mod_path <- paste0(getwd(), "./Model/M0 base model/", sep="") ## CHANGE name

dir.create(path= paste0(mod_path,"/forecast"), showWarnings = T, recursive = T)
dir.create(path= paste0(mod_path,"/forecast/forecast files"), showWarnings = T, recursive = T)

dir.forecastTAC <-  paste0(mod_path,"/forecast/forecast files")

file.copy(paste(mod_path, "forecast.ss", sep="/"),
          paste(dir.forecastTAC, "forecast.ss", sep="/"))

## read forecast from forecast folder
fore <- r4ss::SS_readforecast(file = file.path(dir.forecastTAC, "forecast.ss"),
                              verbose = FALSE)

## Look for values of apical F for intermediate year 2021 (report:14)
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE) ## read

## option for avoid errors in max print lines for ss files 
options(max.print=10000) # set max print lines
getOption("max.print") # get max print lines

# int year ---------------------------------------------------------------------

## prepare intermediate year data (2021)
dat=replist$exploitation
dat=dat[-c(3,4,5,6)];head(dat)
Naver=2 # number of average years-1
Nfor=fore$Nforecastyrs
startyear=max(dat$Yr)-Nfor-Naver
endyear=max(dat$Yr)-Nfor

## average of the last 3 years across seasons and fleets
data<-subset(dat,dat$Yr>=startyear & dat$Yr<=endyear)
data<-data[,-1] # remove year column
data_int<-aggregate(.~Seas,data=data,mean) 

## input intermediate year data
dimen=dim(data_int)
Year=rep(endyear+1,dimen[1]*(dimen[2]-1))
fore_dat_int=data.frame(Year)
fore_dat_int$Seas=rep(1:4)
fore_dat_int$Fleet=sort(rep(1:4,4))
fore_dat_int$F=as.vector(as.matrix(data_int[,-1]))

# define Fmult ---------------------------------------------------------------- 

# Flim	0.429	F with 50% probability of SSB>Blim (segreg without Btrigger)
datmul=replist$exploitation
head(subset(datmul, datmul$Yr>=2018),9)

# Last 3 years Fbar (automatic)
fbar_years <- subset(datmul, datmul$Yr >= (endyear - 2) & datmul$Yr <= endyear & datmul$Seas == 1)
(fbar_years$F_std) # F bar values for the last 3 years (2018-2020)
(denom <- mean(fbar_years$F_std, na.rm = TRUE))
flim<-0.9 # sacado de plot del yield de ss		
mulFlim<-flim/denom # este es el multiplier para llegar a Flim.

fmult=seq(0,mulFlim+0.05,by=0.05)# 
Fmult_names=paste0("Fmult",fmult)
l_fmult=length(fmult)
aux=fore_dat_int

## create data for following forecast years using int year and Fmult
for (i in 1:l_fmult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:Nfor){
    aux_fore$Year=endyear+j
    aux_fore$F=fmult[i]*aux$F
    fore_dat=rbind(fore_dat,aux_fore)
  }
  
  # input ------------------------------------------------------------------------
  
  fore$InputBasis<-99 # 99 for F, 2 for Catch
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  
  ## write all forecast files/scenarios
  r4ss::SS_writeforecast(fore, dir = dir.forecastTAC, file = paste0("forecast",Fmult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}

# 2) Do forecast ------------------------------------------------------------------

## FIRST generate forecast scenarios/files with script "prepare forecast.R"

# load packages
library(r4ss)
library(ss3diags)
library(readr)
library(plyr)
library(reshape)
library(tidyverse)
library(parallel)
library(doParallel)

sessionInfo() # check for ss3diags_2.0.1, r4ss_1.43.0, kobe_2.2.0

## pre-register parallel function 
registerDoParallel(6)

## set seed for consistency
set.seed(1234)


dir <- paste0(getwd(), "/Model", sep="") 
# create subfolder arrays for naming
#run = paste0("R",1:3) # number of models
#run = "R0 base hessian" #select desired run or run
tacs = paste0("Fmult",fmult) # TAC levels for forecast


## If we fix catch we would make each fleet to fish with a given catch percentaje
## Each fleet fishes depending on their selex, so it is better to do the
## forecast scenarios for different levels of F (instead of catch)

# create forecast folder and subfolders (if the first time)
for(i in 1:length(run)){
  dir.runN <- paste0(dir,"/",run[i])
  dir.runN.new <- paste0(mod_path,"/forecast")
  dir.create(path=dir.runN.new, showWarnings = T, recursive = T)
  for(j in 1:length(tacs)){
    dir.tacN <- paste0(dir.runN.new,"/",tacs[j])
    dir.create(path=dir.tacN, showWarnings = T, recursive = T)
    # copy the SS base files in every TAC subfolder 
    file.copy(paste(dir.runN, "starter.ss", sep="/"),
              paste(dir.tacN, "starter.ss", sep="/"))
    file.copy(paste(dir.runN, "control_fixed.ss", sep="/"),
              paste(dir.tacN, "control_fixed.ss", sep="/"))
    file.copy(paste(dir.runN, "shake_data.ss", sep="/"),
              paste(dir.tacN, "shake_data.ss", sep="/"))	
    #file.copy(paste(dir.runN, "wtatage.ss", sep="/"),
    #          paste(dir.tacN, "wtatage.ss", sep="/"))
    file.copy(paste(dir.runN, "ss.par", sep="/"),
              paste(dir.tacN, "ss.par", sep="/"))
    
    # copy the right forecast file from the "forecast_TAC" folder
    file.copy(paste(dir.forecastTAC,  paste0("forecast",tacs[j], ".ss") , sep="/"),
              paste(dir.tacN, "forecast.ss", sep="/"))
    # Edit "starter.ss"
    # Robust: use the r4ss parser (named fields), independent of the comment
    # text / SS version. The old approach matched the line by its comment with
    # grep() and broke when r4ss rewrote starter.ss in short format
    # (#_last_estimation_phase), leaving estimation turned on.
    starter <- r4ss::SS_readstarter(file = file.path(dir.tacN, "starter.ss"), verbose = FALSE)
    starter$init_values_src       <- 1  # 1 = use ss.par (estimated values from base model)
    starter$last_estimation_phase <- 0  # 0 = turn off estimation (project only, no re-estimation)
    r4ss::SS_writestarter(starter, dir = dir.tacN, file = "starter.ss",
                          overwrite = TRUE, verbose = FALSE)
    
  }
}

# run forecasts for each model
mc.cores = 1 

for(i in 1:length(run)){
  dir.runN.new <- file.path(mod_path, "forecast") 
  rutas_escenarios <- file.path(dir.runN.new, tacs)
  
  mclapply(rutas_escenarios, function(carpeta) {
    r4ss::run(
      dir = carpeta,
      exe = "ss.exe", 
      extras = "-nohess", 
      skipfinished = FALSE,
      verbose = FALSE
    )
  }, mc.cores = mc.cores)
}

r4ss::SS_readforecast(file = file.path(paste(mod_path,"/forecast/Fmult0.1", sep=""), "forecast.ss"),
                      verbose = FALSE)

# 3) Output table --------------------------------------------------------------

# Table (forecast)

#rm(list=ls()) ## Clean environment
library(r4ss) 
library(icesAdvice)

# Years

year_inter=2021

#run="R0 base hessian"
# Catches, recruitment, F, SSB
mod_path=paste0(paste0(getwd(), "/Model/", sep="") , run, sep="") # mod_path
fore_path <-  paste0(mod_path,"/forecast")

## Retros for directories
tabledir_fore<- paste(fore_path, "/table", sep="")
dir.create(tabledir_fore)

# Safety check: re-run any scenario whose Report.sso was truncated -------------
# An SS run can leave an incomplete Report.sso (file cut mid-write at extreme F).
# A truncated report is clearly smaller than the rest; detect it by size and
# re-run only those folders so SSgetoutput does not crash.
fore_dirs <- file.path(fore_path, Fmult_names)
reps      <- file.path(fore_dirs, "Report.sso")
sizes     <- file.size(reps)
typical   <- median(sizes, na.rm = TRUE)
bad       <- fore_dirs[is.na(sizes) | sizes < 0.95 * typical]  # >5% smaller = suspect

if (length(bad) > 0) {
  message("Incomplete Report.sso detected -> re-running: ",
          paste(basename(bad), collapse = ", "))
  for (d in bad) {
    file.copy("C:/SS exe/ss.exe", file.path(d, "ss.exe"), overwrite = TRUE)
    r4ss::run(dir = d, exe = "ss.exe", extras = "-nohess",
                        skipfinished = FALSE, verbose = FALSE)
  }
}

retroModels <- SSgetoutput(dirvec=file.path(fore_path,
                                            Fmult_names))

retroSummary <- SSsummarize(retroModels)

# SSB -------------------------------------------------------------------------

SSB <- as.data.frame(retroSummary["SpawnBio"])

Table_Inter=data.frame(matrix(0,ncol=6,nrow=1))
colnames(Table_Inter)=c(paste("SSB",year_inter+1,sep=""),"F","Rec","Catches", "Landings","Discards")
ind=which((year_inter+1)==SSB$SpawnBio.Yr)
Table_Inter[,1]=SSB[ind,1]

lastyear=max(SSB$SpawnBio.Yr)

Table_fmult=data.frame(matrix(0,ncol=6,nrow=length(Fmult_names)))
rownames(Table_fmult)=round(fmult,3)
colnames(Table_fmult)=c(paste("SSB",lastyear,sep=""),"F","Rec","Catches", "Landings","Discards")

ind=which(lastyear==SSB$SpawnBio.Yr)
ncol<-dim(SSB)[2]
aux=SSB[ind,-c(ncol-1,ncol)]# remove year columns
colnames(aux)=NULL
Table_fmult[,1]=unlist(aux)


# F ----------------------------------------------------------------------------
# Note that F is from intermediate year

Fvalue <- as.data.frame(retroSummary["Fvalue"])

ind=which((year_inter)==Fvalue$Fvalue.Yr)
Table_Inter[,2]=Fvalue[ind,1]

ind=which((lastyear-1)==Fvalue$Fvalue.Yr)
aux=Fvalue[ind,-c(ncol-1,ncol)]
colnames(aux)=NULL
Table_fmult[,2]=unlist(aux)

# Flim es distinto a Fcrash, mirar defs.
# Flim es F que da en equilibrio un 50% prob de que SSB este por encima de Blim

# Rec -------------------------------------------------------------------------
# Note constant recruitment!

Recr <- as.data.frame(retroSummary["recruits"])

ind=which((year_inter)==Recr$recruits.Yr)
Table_Inter[,3]=Recr[ind,1]

ind=which((lastyear-1)==Recr$recruits.Yr)
aux=Recr[ind,-c(ncol-1,ncol)]
colnames(aux)=NULL
Table_fmult[,3]=unlist(aux)

# Catches ----------------------------------------------------------------------

lcat=length(Fmult_names)

Fmsy_vector=Table_fmult[,4]

for (i in 1:lcat){
  
  output=retroModels[[i]]
  
  fltnms <- setNames(output$definitions$Fleet_name,1:9) # Ojo sobre PtCPUE
  
  ## Catch
  
  catch <- as_tibble(output$timeseries) %>% filter(Era == "FORE" ) %>% 
    select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) 
  names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:4], sep = "_"), paste('LanEst', fltnms[1:4], sep = "_"),
                    paste('CatEst', fltnms[1:4], sep = "_"))
  aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
  names(aux1) <- paste('DisEst', fltnms[1:4], sep = "_")
  catch <- catch %>% bind_cols(aux1) 
  catch <- catch %>% pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
    mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
    select('year', 'season', 'fleet', 'indicator', 'value')  
  
  Landings=subset(catch, catch$indicator=="LanEst")
  Landings$year=as.factor(Landings$year)
  library(plyr)
  Landings=ddply(Landings, .(year), summarize,  number=sum(value))
  
  
  Discards=subset(catch, catch$indicator=="DisEst")
  Discards$year=as.factor(Discards$year)
  Discards=ddply(Discards, .(year), summarize,  number=sum(value))
  
  
  Cat=subset(catch, catch$indicator=="CatEst")
  Cat$year=as.factor(Cat$year)
  Cat=ddply(Cat, .(year), summarize,  number=sum(value))
  
  
  
  if(i==1){
    Table_Inter$Catches=Cat[1,2]
    Table_Inter$Discards=Discards[1,2]
    Table_Inter$Landings=Landings[1,2]
  }
  
  ll=dim(Cat)[1]-1
  Table_fmult[i,]$Catches=Cat[ll,2]
  Table_fmult[i,]$Landings=Landings[ll,2]
  Table_fmult[i,]$Discards=Discards[ll,2]
  
  Fmsy_vector[i]=Cat[dim(Cat)[1],2]
}

# Save -------------------------------------------------------------------------

Table_Inter
Table_fmult
Fmsy_vector=as.data.frame(Fmsy_vector)
colnames(Fmsy_vector)<-paste("Catch",lastyear,sep="")
Table_fmult<-cbind(Table_fmult, Fmsy_vector)

write.csv(Table_Inter, paste(tabledir_fore, "/table intermediate year.csv", sep=""))
write.csv(Table_fmult, paste(tabledir_fore, "/table Fmult.csv", sep=""))

