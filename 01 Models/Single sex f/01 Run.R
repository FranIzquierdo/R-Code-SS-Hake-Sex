#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run SS shake model and do r4ss plots #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 12/06/2026 #
#~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
#~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

## M0 base model is built below automatically as a fresh copy of "base model"
## (best jitter): input files + ss.par + ss.exe are copied, then run with hessian.

## r4ss version to install:
# install.packages("pak")
# pak::pkg_install("r4ss/r4ss")

# Edit here! ------------------------------------------------------------------

rm(list=ls())
library(r4ss)

mod_path <- paste0(getwd(), "./Model/base model/", sep="") ## CHANGE name
dir.create(mod_path) ## check that exists

# Build M0 base model from "base model" and run it -----------------------------
# M0 base model is a fresh copy of the original "base model" (best jitter).
# If the model is already run (Report.sso exists) the copy and run are skipped.
# To re-run the model, delete the folder manually.

base_path <- paste0(getwd(), "./Model/base model/")  ## original (best jitter)

if (file.exists(paste0(mod_path, "Report.sso"))) {
  message("M0 base model already run -> skipping copy and run.")
} else {
  file.copy(paste0(base_path, "starter.ss"),       paste0(mod_path, "starter.ss"),       overwrite = TRUE)
  file.copy(paste0(base_path, "control_fixed.ss"), paste0(mod_path, "control_fixed.ss"), overwrite = TRUE)
  file.copy(paste0(base_path, "shake_data.ss"),    paste0(mod_path, "shake_data.ss"),    overwrite = TRUE)
  file.copy(paste0(base_path, "forecast.ss"),      paste0(mod_path, "forecast.ss"),      overwrite = TRUE)
  file.copy(paste0(base_path, "ss.par"),           paste0(mod_path, "ss.par"),           overwrite = TRUE)
  file.copy(paste0(base_path, "ss.exe"),           paste0(mod_path, "ss.exe"),           overwrite = TRUE)

  r4ss::run(dir = mod_path, verbose = TRUE, extras = "-read", exe = "ss.exe", show_in_console = TRUE) ## "-read", "-nox" or "-nohess"
} 

# r4ss plot --------------------------------------------------------------------

## Read output
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)

## Plot
SS_plots(replist, pdf=F, png=T, html=T, printfolder = "r4ss plots") ## html

## Summary plot
png(file=paste(mod_path,"/a_summaryplot.png",sep=""), width = 900, height = 800,pointsize=18)
par(mfrow=c(3,2))
SSplotCatch(replist, subplots=10); title("Landings")
SSplotSelex(replist, subplot = 1)
SSplotBiology(replist,subplots = 1)
SSplotBiology(replist,subplots = 6); title("Mat")
SSplotSummaryF(replist); title("F")
SSplotTimeseries(replist, subplot = 7, maxyr = 2021); title("Biomass")
dev.off()

## Tables
replist$likelihoods_used 
replist$likelihoods_by_fleet

#write.csv(replist$likelihoods_used, file=paste0(mod_path, "/likelihood-", run, ".csv", sep=""))
#write.csv(replist$likelihoods_by_fleet, file=paste0(mod_path, "/likelihood by fleet-", run, ".csv", sep=""))

