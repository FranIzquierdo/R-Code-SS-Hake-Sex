#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform retrospective analysis SS #
# SSB, F and Recruitment — ggplot2  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 12/06/2026 #
#~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
# Marta Cousido       #
# Santiago Cerviño    #
#~~~~~~~~~~~~~~~~~~~~~~

# Edit here --------------------------------------------------------------------

rm(list = ls())
library(r4ss)
library(icesAdvice)
library(tidyverse)
library(ggpubr)

mod_path      <- paste0(getwd(), "./Model/M0 base model/")  ## CHANGE name
plotdir_retro <- file.path(mod_path, "retros")
dir.create(plotdir_retro, showWarnings = FALSE)

yper      <- 0:-5    # 0 = reference (all data), -5 = 5 years removed
nforecast <- 3       # forecast rows removed for the rho 
year_min  <- 1980    # first year shown
year_max  <- 2020    # last year shown (last data year)

# Do retros --------------------------------------------------------------------
retro_dirs  <- file.path(mod_path, "retros", paste0("retro", yper))
already_run <- all(file.exists(file.path(retro_dirs, "Report.sso")))
if (already_run) {
  message("Retros already run -> skipping SS_doRetro, reading existing output.")
} else {
    retro(dir = mod_path, oldsubdir = "", newsubdir = "retros",
        extras = "-nox", subdirstart = "retro", years = yper,
        overwrite = TRUE, exe = "ss")
}

retroModels  <- SSgetoutput(dirvec = retro_dirs)
retroSummary <- SSsummarize(retroModels)
endyrvec     <- retroSummary$endyrs + yper
len_re       <- length(yper) - 1
year_max     <- max(endyrvec)            # last data year (2020)

# Rho --------------------------------------------------------------------------
build_rho_df <- function(comp, drop_init) {
  df <- as.data.frame(retroSummary[[comp]])
  yr <- df[["Yr"]]
  df <- df[, seq_along(yper)]
  names(df) <- paste0("Retro", yper)
  df$Year <- yr
  n <- nrow(df)
  drop_rows <- ((n - nforecast) + 1):n
  if (drop_init) drop_rows <- c(1, 2, drop_rows)
  df <- df[-drop_rows, ]
  ind2 <- which(df$Year == max(endyrvec))
  for (i in 2:length(yper)) {
    ind1 <- which(df$Year == endyrvec[i])
    df[(ind1 + 1):ind2, i] <- NA
  }
  df
}

ssb_rho <- build_rho_df("SpawnBio", drop_init = TRUE)
rec_rho <- build_rho_df("recruits", drop_init = TRUE)
f_rho   <- build_rho_df("Fvalue",   drop_init = FALSE)

rho_ssb <- mohn(ssb_rho, peels = len_re)
rho_rec <- mohn(rec_rho, peels = len_re)
rho_f   <- mohn(f_rho,   peels = len_re)

# Plot -------------------------------------------------------------------------
to_long <- function(w) {
  long <- pivot_longer(w[w$Year >= year_min & w$Year <= year_max, ], -Year,
                       names_to = "Retro", values_to = "value")
  long$Retro <- factor(long$Retro, levels = paste0("Retro", yper))
  long
}

plot_retro <- function(w, rho, ylab) {
  ggplot(to_long(w), aes(Year, value, colour = Retro)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    scale_colour_viridis_d(option = "G", begin = 0.1, end = 0.9,
                           labels = paste("Data", yper, "yr")) +
    expand_limits(y = 0) +
    labs(x = NULL, y = ylab, colour = NULL,
         title = bquote("Mohn's" ~ rho == .(round(rho, 3)))) +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
}

ssb_c        <- plot_retro(ssb_rho, rho_ssb, "SSB")
F_c.plot     <- plot_retro(f_rho,   rho_f,   "F")
Recruit.plot <- plot_retro(rec_rho, rho_rec, "Recruitment")

combined_plot <- ggarrange(ssb_c, F_c.plot, Recruit.plot, nrow = 3, ncol = 1,
                           common.legend = TRUE, legend = "bottom")
print(combined_plot)

# Save 
ggsave(file.path(plotdir_retro, "retros_summary.jpeg"),
       combined_plot, width = 6, height = 9, dpi = 300)

# R4ss plots -------------------------------------------------------------------

## Comparison plot pdf
SSplotComparisons(retroSummary, endyrvec=endyrvec, xlim=c(1980,2020), 
                  legendlabels=paste("Data",yper,"years"), print=FALSE, pdf=TRUE, 
                  plotdir = plotdir_retro)

dev.off()

## Recruits
png(file.path(plotdir_retro, "retro_recruits.png"),
    width = 7, height = 5, units = "in", res = 300)
SSplotRetroRecruits(retroSummary, endyrvec = endyrvec, cohorts = 1983:2019,
                    relative = TRUE, legend = FALSE)
dev.off()
