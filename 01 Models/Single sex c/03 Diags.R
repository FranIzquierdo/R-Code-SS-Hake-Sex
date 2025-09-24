#~~~~~~~~~~~~~~~~~~~~~~~
# Apply ss3diags to SS #
#~~~~~~~~~~~~~~~~~~~~~~~
# Modified 16/02/2025 #
#~~~~~~~~~~~~~~~~~~~~~~~~~


## Press Ctrl + Shift + O to see the document outline

## Read retro files and make ss3diags plots

#library(devtools)
#install_github("jabbamodel/ss3diags")

# Edit here! -------------------------------------------------------------------

rm(list=ls()) ## Clean environment
library(r4ss)
library(ss3diags)

## Set model folder
mod_path <- paste0(getwd(), "./Model/M0 base model/", sep="") ## CHANGE name

## Create subfolder for plots
dirplot<-paste0(mod_path,"/ss3diags", sep="")
dir.create(dirplot)

yper=0:-5 ## years period for retros

retroModels <- SSgetoutput(dirvec=file.path(mod_path, "retros",
                                            paste("retro",yper,sep="")))

## Reference run
ss3rep = retroModels[[1]]

# ss3diags ---------------------------------------------------------------------

## Check Data
sspar()
SSplotData(ss3rep,subplot = 2)
dev.print(jpeg,paste0(dirplot,"/DataSetup_",".jpg"), width = 8, 
          height = 6, res = 300, units = "in")

## For cpue
pdf(paste0(dirplot,"/RunsTestResiduals.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(2,1),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="cpue",add=T)
dev.off()

## For length
pdf(paste0(dirplot,"/RunsTestResiduals.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,3),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="len",add=T)
dev.off()

## Check conflict between mean lengths
pdf(paste0(dirplot,"/JointResiduals_.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotJABBAres(ss3rep,subplots="cpue",add=T)
SSplotJABBAres(ss3rep,subplots="len",add=T)
dev.off()

## Check starter file
starter = SSsettingsBratioF(ss3rep)

## Get uncertainty from MVLN for F/F_Btrg with original F setting F_abs
sspar(mfrow=c(1,1),plot.cex = 0.9)
mvn = SSdeltaMVLN(ss3rep,plot = T,years=1972:2020)
mvn$labels # the out put is SB/SBtrg
dev.print(jpeg,paste0(dirplot,"/Kobe_.jpg"), width = 6.5, 
          height = 6.5, res = 300, units = "in")

pdf(paste0(dirplot,"/MVLN_TRJ_.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,3),plot.cex = 0.9)
SSplotEnsemble(mvn$kb,ylabs = mvn$labels,add=T)
dev.off()

## Summarize the list of retroModels
retroSummary <- r4ss::SSsummarize(retroModels)

## Now Check retros Analysis with one-step ahead Forecasts
sspar(mfrow=c(2,1),plot.cex = 0.9)
SSplotRetro(retroSummary,forecast = F,add=T)
SSplotRetro(retroSummary,forecast = F,add=T, xmin=2000)
dev.print(jpeg,paste0(dirplot,"/Retroforecast_.jpg"), width = 8, 
          height = 9, res = 300, units = "in")

## Do Hindcast with Cross-Validation of CPUE observations
pdf(paste0(dirplot,"/HCxvalIndex.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,2),plot.cex = 0.9)
SSplotHCxval(retroSummary,xmin=1953,add=T)
dev.off()

## Also test new feature of Hindcast with Cross-Validation for mean length
## Use new converter fuction SSretroComps()
hccomps = SSretroComps(retroModels)
pdf(paste0(dirplot,"/HCxvalLen_.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,3),plot.cex = 0.7)
SSplotHCxval(hccomps,add=T,subplots = "len",legendloc="topleft")
dev.off()
