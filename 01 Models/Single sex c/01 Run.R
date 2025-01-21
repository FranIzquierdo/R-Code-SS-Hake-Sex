#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run SS shake model and do r4ss plots #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 16/01/2025 #
#~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
#~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

## SS files were setup manually and located in the "base model" folder

# library(devtools)
# remotes::install_github("r4ss/r4ss")

# * M0 base model --------------------------------------------------------------

# New dir
base_dir <- paste0(getwd(), "/Model/base model/", sep="") 
new_dir <- paste0(getwd(), "/Model/M0 base model/", sep="") 

# Files to copy
files_to_copy <- c("control_fixed.ss", "shake_data.ss", "starter.ss", "forecast.ss")

# Create dir if it does not exist
if (!dir.exists(new_dir)) {
   dir.create(new_dir)
   message("Carpeta '", new_dir, "' creada.")
}

# Copy files
for (file in files_to_copy) {
   source_path <- file.path(base_dir, file)  # Ruta completa del archivo origen
   target_path <- file.path(new_dir, file)  # Ruta completa del archivo destino
   
   if (file.exists(source_path)) {
      file.copy(source_path, target_path)
      message("Archivo '", file, "' copiado a '", new_dir, "'.")
   } else {
      message("Archivo '", file, "' no encontrado en '", base_dir, "'.")
   }
}

# Run --------------------------------------------------------------------------

rm(list=ls())
library(r4ss)

mod_path <- paste0(getwd(), "./Model/M0 base model/", sep="") ## CHANGE name
dir.create(mod_path) ## check that exists

r4ss::run_SS_models(dirvec = mod_path, model = "ss", exe_in_path = TRUE,
                    verbose=TRUE, extras = "-nox") ## "-nox" or "-nohess" 

# r4ss plot --------------------------------------------------------------------

## Read output
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)

## Plots r4ss
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

