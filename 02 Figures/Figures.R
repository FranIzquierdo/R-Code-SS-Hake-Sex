#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Make Hake Sex paper main Figures #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 03/04/2024 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo        #
# francisco.izqtar@gmail.com #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

# 01 MAP ICES ------------------------------------------------------------------

# 01 Map study area ICES

## Clean workspace
rm(list=ls())

## Load packages
library(rgdal)
library(ggplot2)
library(mapdata)
library("ggspatial")
library(ggpattern)
library("rnaturalearth")
library("rnaturalearthdata")

## Create plot directory
dir_plot <- paste0(getwd(), "/Output/")
dir.create(dir_plot)

## Create folder for downloaded data
dir_dat <- paste0(getwd(), "/Input/Shapefile ICES")
dir.create(dir_dat)

## Download ICES areas shapefile
url <- "https://gis.ices.dk/shapefiles/ICES_areas.zip"
file <- basename(url)
download.file(url, destfile = paste0(dir_dat, "/", file))

## Unzip
unzip(paste0(dir_dat, "/", file), exdir = dir_dat)

## Load ICES areas shapefile (in UTM)
areasUTM <- readOGR(dsn = dir_dat ,
                    # set layer name:
                    layer = "ICES_Areas_20160601_cut_dense_3857",
                    verbose = FALSE)

## Change CRS to WGS84
areasWGS84 <- spTransform(areasUTM, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Check
# plot(areasUTM, axes = TRUE)
# plot(areasWGS84, axes = TRUE)

## Get world map data
world <- ne_countries(returnclass = "sf")

## Fortify shapefiles for ggplot
areasWGS84 <- fortify(areasWGS84) 
world <- fortify(world) 

## Set map limits 
lons <- c(-28, 19)# c(-28, 19)
lats <- c(28, 53) # c(28, 59) 

## Create breaks and label vectors
ewbrks <- seq(lons[1], lons[2], 10)
nsbrks <- seq(lats[1], lats[2], 5)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x, "ºW"), ifelse(x > 0, paste(x, "ºE"), x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "ºS"), ifelse(x > 0, paste(x, "ºN"), x))))

## Select ICES subareas 8c9a for polygons
s_9a <- subset(areasWGS84, areasWGS84$id == c(35))
s_8c <- subset(areasWGS84, areasWGS84$id == c(36))

## Select world 
world <- map_data("world2Hires") # table(world$region)

## Select regions not well displayed by world (for overlapping them)
reg <- subset(world, region %in% c('Poland', 'Germany', 'Netherlands', 'Ireland', 'Uk', 'Algeria', 'Tunisia'))

## Define sea and land colors
fill_land <- "#FFEBCD"
fill_sea <- "#F1FBF7"

## Create map
ggplot() +
   
   # world (land) poly
   geom_polygon(data = world, aes(x = long, y = lat, group = group), 
                fill = fill_land, color = "darkgrey") + 
   
   geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
                fill = fill_land, color = "darkgrey") + 
   
   
   # ICES subareas poly 
   geom_polygon(data = areasWGS84, aes(long, lat, group = group, fill = hole),
                color = "darkgrey", size = 0.1)+
   scale_fill_manual(values = c(fill_sea, fill_land))+
   
   # polygon 8c 
   geom_polygon(data = s_8c, aes(long, lat, group = group),
                color = "black", fill = fill_sea, size = 0.3)+
   
   # polygon 9a
   geom_polygon(data = s_9a, aes(long, lat, group = group),
                color = "black", fill = fill_sea,  size = 0.3)+
   
   # add names (text label)
   annotate("text", x = -6.2, y = 45.1, label = "8c", size = 3, fontface = "bold") + 
   annotate("text", x = -12, y = 39.8, label = "9a", size = 3, fontface = "bold") + 
   
   # formatting
   scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
   scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
   ylab(" ") + xlab(" ") + 
   
   # configure projection and plot domain
   coord_map(xlim = lons, ylim = lats) +
   
   # theme custom
   theme_light()+
   theme(legend.position = "none",
         panel.background = element_rect(fill = fill_sea, colour = "#2C3E4F"),
         plot.background = element_rect(fill = "white", colour = "#2C3E4F")) 

## Save
ggsave(paste0(dir_plot,"/01 Map ICES 8c9a.jpeg"), dpi=800, width = 7.5, height = 6)

# 02 SS DATA -------------------------------------------------------------------

# SS input data plot

## Clean workspace
rm(list = ls())
library(r4ss)

## Crear directorio para las gráficas
dir_plot <- file.path(getwd(), "Output")

# Construir la ruta de manera correcta
new_mod_path <- file.path(getwd(), "../01 Models/Two sex/Model/M0 base model")

start <- r4ss::SS_readstarter(file = file.path(new_mod_path, "starter.ss"), 
                              verbose = FALSE)
dat <- r4ss::SS_readdat(file = file.path(new_mod_path, start$datfile),
                        verbose = FALSE)

## Filter by datatype, select year and fleet

## Catch
catch <- dat$catch[, c(1, 3)]
catch <- catch[-c(which(catch$year == -999)),] # remove -999 eq values
catch$type <- rep("Catches")
colnames(catch) <- c("year", "fleet",  "type")

## Discards
disc<-dat$discard_data[ ,c(1,3)]
disc$type<-rep("Catches") # named as catch to plot in the same place
disc$Flt<-rep(10)
colnames(disc)<-c("year", "fleet",  "type")

## LFD
LFD<-dat$lencomp[ ,c(1,3)]
LFD$FltSvy<-abs(LFD$FltSvy) # ignore negative numbers (Super Periods)
LFD$type<-rep("Length compositions")
colnames(LFD)<-c("year", "fleet",  "type")

## Size composition
sizecomp<-dat$sizefreq_data_list[[1]][c(2,4)] 
sizecomp$FltSvy<-abs(sizecomp$FltSvy) # ignore negative numbers (Super Periods)
sizecomp$type<-rep("Length compositions")
colnames(sizecomp)<-c("year", "fleet",  "type")

## Indices and CPUE
indices<-dat$CPUE[ ,c(1,3)]
indices$type<-rep("Abundance indices")
colnames(indices)<-c("year", "fleet",  "type")

## Join into dataframe
input<-rbind(catch, disc, LFD, sizecomp, indices)
input$year<-as.factor(input$year)
input$fleet<-as.factor(input$fleet)

## Filter unique values per year (we are interested only in 1 value per year)
library(dplyr)
input<-distinct(data.frame(input))
dim(input)

## Create fleetname column
input$fleetnam<-input$fleet

## Recode factor levels with names
input$fleetnam <- recode_factor(input$fleetnam,
                                "1" = "medium", "2" = "large", "3"="mixed",
                                "4"="small","5"="surv-sp-north","6"="surv-pt","7"="surv-sp-south",
                                "8"="CPUE medium","9"="CPUE large","10"="discards")
## Reorder factor levels: fleetname
input$fleetnam = factor(input$fleetnam, levels = c( "CPUE large","CPUE medium", "surv-pt", "surv-sp-south",
                                                    "surv-sp-north","discards","small","mixed",
                                                    "large","medium"))


## Reorder factor levels: fleet
input$fleet <- factor(input$fleet,      # Reordering group factor levels
                      levels = c("1", "2", "3",
                                 "4","10","5","6",
                                 "7","8", "9"))

## Reorder factor levels: type to group
input$group <- input$type # Replicate data
input$group <- factor(input$group, # Reordering group factor levels
                      levels = c("Catches", "Abundance indices", "Length compositions"))


## Plot
library(ggplot2)
ggplot(input, aes(x = factor(year), fill = factor(fleetnam), 
                  colour = factor(fleetnam), y=fleetnam)) +
   geom_point(size=2.7, shape=15)+
   scale_fill_manual(values = c("#ecaecd","#e697ac", "#edb9a1","#f7d297",  "#ffad70","darkgrey","#b38dca","#705cb5","#43428e","#2c2142"))+
   scale_colour_manual(values = c("#ecaecd","#e697ac", "#edb9a1","#f7d297",  "#ffad70", "darkgrey","#b38dca","#705cb5","#43428e","#2c2142"))+
   scale_x_discrete(name="Year", breaks=seq(from=1960, to=2020, by=5), labels=seq(from=1960, to=2020, by=5))+
   ylab("Fleets")+
   theme_light()+
   theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1))+
   theme( axis.text = element_text( size = 10 ),
          axis.text.x = element_text( size = 9 ),
          axis.text.y = element_text( size = 9 ),
          axis.title = element_text( size = 10 ),
          strip.text = element_text(colour = 'black',size = 8))+
   facet_wrap(~ group, ncol = 1,scales = "free_y") +
   theme(legend.position="none",
         strip.background=element_rect(colour="black",
                                       fill="white"))

## Save plot in jpeg
ggsave(paste0(dir_plot,"/02 SS input data.jpeg"), dpi= 800, width=6.4, height=6)

# 03 M & GROWTH-----------------------------------------------------------------

## Clean workspace
rm(list = ls())

## Create plot directory
dir_plot <- paste0(getwd(), "/Output/")
dir.create(dir_plot)

library(r4ss)
library(ggplot2)
library(dplyr)

dir<- file.path(getwd(), "../01 Models/Two sex/Model/M0 base model")

# Load the SS output
ss <- SSgetoutput(dirvec = dir, getcovar = F, verbose = FALSE)[[1]]

#SSplotSexRatio(ss, subplots=1, kind = "LEN", maxcols=5, maxrows = 3) # Export them manually

## M ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filter data by sex
female_data <- ss$endgrowth %>% 
   filter(Sex == 1) %>% 
   select(Real_Age, M)


female_data<- female_data %>%
   filter(Real_Age != 0 | (Real_Age == 0 & M == 1.19))

male_data <- ss$endgrowth %>% 
   filter(Sex == 2) %>% 
   select(Real_Age, M)


male_data<- male_data %>%
   filter(Real_Age != 0 | (Real_Age == 0 & M == 1.19))

# Combine datasets into one with a new factor for sex
female_data$Sex <- "female"
male_data$Sex <- "male"
data_comb <- rbind(female_data, male_data)

# Plot M at age data by sex
p_m <- ggplot(data = data_comb) +
   geom_line(aes(x = Real_Age, y = M, color = Sex, linetype = Sex), size = 1) +
   scale_color_manual(values = c("female" = "#43428e", "male" = "#ffad70"), name = "Sex") +
   scale_linetype_manual(values = c("female" = "solid", "male" = "dashed")) +
   labs(x = "Age (years)", y = "Natural mortality (M)", color = "Sex", linetype = "Sex") +
   theme_light() +
   theme(legend.position = "bottom") +
   guides(color = guide_legend(keywidth = unit(0.9, "cm")))

p_m

## Growth ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(r4ss)
library(ggplot2)
library(dplyr)

dir<-paste0(getwd(), "/Runs/M0 two-sex/")

# Load the SS output
ss <- SSgetoutput(dirvec = dir, getcovar = F, verbose = FALSE)[[1]]

#SSplotBiology(ss, subplots=1)

ss$endgrowth

library(ggplot2)
library(dplyr)

# Filter data by sex
female_data <- ss$endgrowth %>% 
   filter(Sex == 1) %>% 
   select(Real_Age, Len_Mid, SD_Mid)

female_data$Sex<-rep("female")

male_data <- ss$endgrowth %>% 
   filter(Sex == 2) %>% 
   select(Real_Age, Len_Mid, SD_Mid)

male_data$Sex <- rep("male")

comb_data<-rbind(female_data, male_data)

# Plot length-at-age data by sex with 95% confidence intervals
p_g <-  ggplot() +
   geom_line(data = comb_data, aes(x = Real_Age, y = Len_Mid, color = Sex, linetype = Sex), size = 1) +
   geom_ribbon(data = comb_data, aes(x = Real_Age, ymin = Len_Mid - 1.96 * SD_Mid, ymax = Len_Mid + 1.96 * SD_Mid, fill = Sex), alpha = 0.3) +
   scale_color_manual(values = c("female" = "#43428e", "male" = "#ffad70"), name = "Sex") +
   scale_fill_manual(values = c("female" = "#43428e", "male" = "#ffad70"), name = "Sex") +
   scale_linetype_manual(values = c("female" = "solid", "male" = "dashed")) +
   labs(x = "Age (years)", y = "Length (cm)", color = NULL, fill = "Sex") +  # Specify color = NULL to remove color legend
   theme_light() +
   theme(legend.position = "bottom") +
   guides(color = guide_legend(keywidth = unit(0.9, "cm")))  # Adjust keyheight here

p_g

## Arrange ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save
library(ggpubr)
g1<-ggarrange(p_g,p_m, ncol=2, nrow=1, common.legend = TRUE, legend="bottom", align = "hv",
              labels = c("A", "B"),font.label=list(color="black",size=9))
dev.off()
g1+  bgcolor("white") 

ggsave(paste0(dir_plot,"/03 M and growth.jpeg"), dpi=800, width=6.5, height=2.8)

# 04 S.RATIO  ------------------------------------------------------------------

# Observed sex ratio from surveys

## Clean workspace
rm(list=ls())

## Create plot directory
dir_plot <- paste0(getwd(), "/Output/")
dir.create(dir_plot)

library(readxl)
library(tidyverse)
library(ggpubr)

# Read the Excel file for surv-sp-north
file_path_sp_north <- paste0(getwd(), "/Input/Sex ratio/", "HKEbysex.xlsx")
indeterminate_sp_north <- read_excel(file_path_sp_north, sheet = "Indet")
males_sp_north <- read_excel(file_path_sp_north, sheet = "Males")
females_sp_north <- read_excel(file_path_sp_north, sheet = "Females")

# Combine the data into a single data frame for surv-sp-north
data_sp_north <- bind_rows(
   mutate(indeterminate_sp_north, sex = "Indet"),
   mutate(males_sp_north, sex = "Males"),
   mutate(females_sp_north, sex = "Females")
)

# Reshape the data to long format for surv-sp-north
data_long_sp_north <- pivot_longer(data_sp_north, -c(talla, sex), names_to = "year", values_to = "count")

# Transform the year column to real years for surv-sp-north
data_long_sp_north <- data_long_sp_north %>%
   mutate(year = as.numeric(gsub("N", "", year)),
          year = case_when(
             year >= 83 & year <= 99 ~ year + 1900,
             TRUE ~ year + 2000
          ))

data_long_sp_north<-data_long_sp_north%>%
   filter(year >= 1990)

table(data_long_sp_north$year)

# Calculate the total count for each talla for surv-sp-north
total_count_sp_north <- data_long_sp_north %>%
   group_by(talla) %>%
   summarise(total_count = sum(count))

# Calculate the total count for females at each talla for surv-sp-north
females_count_sp_north <- data_long_sp_north %>%
   filter(sex == "Females") %>%
   group_by(talla) %>%
   summarise(females_count = sum(count))

# Merge total count and females count data frames for surv-sp-north
sex_ratio_data_sp_north <- left_join(total_count_sp_north, females_count_sp_north, by = "talla") %>%
   mutate(sex_ratio = females_count / total_count) %>%
   replace_na(list(sex_ratio = 0)) # Replace NAs with 0 for divisions with zero total count

# Plot the sex ratio using ggplot2 for surv-sp-north
plot_surv_sp_north <- ggplot(sex_ratio_data_sp_north, aes(x = talla, y = sex_ratio, color = "surv-sp-north")) +
   geom_point() +  # Represent observed data as dots
   geom_smooth(method = "loess", se = FALSE, color = "#43428e") +  # Fit smoothed line
   labs(title = "Sex Ratio of Females by Talla - surv-sp-north",
        x = "Talla",
        y = "Sex Ratio (Females / Total)") +
   theme_minimal() +
   xlim(25, 80) +  # Limit x-axis from talla 25 to 80
   ylim(0.25, 1)  # Set y-axis limits

# Read the Excel file for surv-pt
file_path_pt <- paste0(getwd(), "/Input/Sex ratio/", "HKE_PT-DemSurvey_bysex_new2.1.xlsx")
indeterminate_pt <- read_excel(file_path_pt, sheet = "indet")
males_pt <- read_excel(file_path_pt, sheet = "males")
females_pt <- read_excel(file_path_pt, sheet = "females")

# Combine the data into a single data frame for surv-pt
data_pt <- bind_rows(
   mutate(indeterminate_pt, sex = "Indet"),
   mutate(males_pt, sex = "Males"),
   mutate(females_pt, sex = "Females")
)

# Reshape the data to long format for surv-pt
data_long_pt <- pivot_longer(data_pt, -c(lt, sex), names_to = "year", values_to = "count")

table(data_long_pt$year)

# Calculate the total count for each talla for surv-pt
total_count_pt <- data_long_pt %>%
   group_by(lt) %>%
   summarise(total_count = sum(count))

# Calculate the total count for females at each talla for surv-pt
females_count_pt <- data_long_pt %>%
   filter(sex == "Females") %>%
   group_by(lt) %>%
   summarise(females_count = sum(count))

# Merge total count and females count data frames for surv-pt
sex_ratio_data_pt <- left_join(total_count_pt, females_count_pt, by = "lt") %>%
   mutate(sex_ratio = females_count / total_count) %>%
   replace_na(list(sex_ratio = 0)) # Replace NAs with 0 for divisions with zero total count

# Plot the sex ratio using ggplot2 for surv-pt
plot_surv_pt <- ggplot(sex_ratio_data_pt, aes(x = lt, y = sex_ratio, color = "surv-pt")) +
   geom_point() +  # Represent observed data as dots
   geom_smooth(method = "loess", se = FALSE, color = "#ffad70") +  # Fit smoothed line
   labs(title = "Sex Ratio of Females by Talla - surv-pt",
        x = "Talla",
        y = "Sex Ratio (Females / Total)") +
   theme_minimal() +
   xlim(25, 80) +  # Limit x-axis from talla 25 to 80
   ylim(0.25, 1)  # Set y-axis limits


# Plot both datasets in the same plot
plot_combined <- ggplot() +
   geom_hline(yintercept = 0.5, linetype = "dashed", color = "#ffad70", size = 0.7)  +
   geom_point(data = sex_ratio_data_sp_north, aes(x = talla, y = sex_ratio), color = "#43428e") +  # Color for surv-sp-north
   geom_point(data = sex_ratio_data_pt, aes(x = lt, y = sex_ratio), color = "black") +  # Color for surv-pt
   geom_smooth(data = sex_ratio_data_sp_north, aes(x = talla, y = sex_ratio, linetype = "surv-sp-north"), method = "loess", se = T, fill = alpha("#43428e", 0.3), color = "#43428e", size=0.8, span=0.6) +
   geom_smooth(data = sex_ratio_data_pt, aes(x = lt, y = sex_ratio, linetype = "surv-pt"), method = "loess", se = T, fill = alpha("darkgrey", 0.3), color = "black", size=0.8, span=0.6) +
   labs(title = "",
        x = "Length (cm)",
        y = "Sex ratio") +
   theme_light() +
   scale_x_continuous(limits = c(20, 80), breaks = seq(20, 80, by = 10)) +
   scale_y_continuous(limits = c(0.2, 1.1), breaks = seq(0.2, 1.1, by = 0.1)) +
   scale_linetype_manual(name = "Survey", values = c("surv-sp-north" = "longdash", "surv-pt" = "solid"),
                         guide = guide_legend(override.aes = list(color = c( "black", "#43428e"), fill = alpha(c( "darkgrey", "#43428e"), 0.4)),
                                              title.position = "top",  # Position of the legend title
                                              title.theme = element_text(size = 11),  # Adjust legend title size
                                              keywidth = 1.7,  # Width of the legend keys
                                              keyheight = 0.4)) +  # Height of the legend keys
   theme(legend.position = "right",  # Set legend position to bottom
         legend.background = element_rect(fill = "white"))  # Set legend background color to white

plot_combined

# Save the combined plot
ggsave(paste0(dir_plot,"/04 Sex ratio observed.jpeg"), plot_combined, dpi=800, width=6, height=3)

# 05 STOCK STATUS ----------------------------------------------------------------

# Estimated quantities single sex and separate sex

## Clean environment
rm(list=ls())
library(r4ss)
library(dplyr)

## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-file.path(getwd(), "../01 Models/")
dir.create(dir_dat)

## Select models to compare
models <- c("/Single sex c/Model/M0 base model", 
            "/Single sex f/Model/M0 base model",
            "/Two sex/Model/M0 base model"
) 

nmodels <- length(models)
filemod <- paste0(dir_dat, models) 

## Read and summarize output
retroModels <- SSgetoutput(dirvec = filemod)
retroSummary <- SSsummarize(retroModels) 

tail(retroModels$replist1$derived_quants)

#rm1<-retroModels$replist1$derived_quants
#rm2<-retroModels$replist2$derived_quants
#rm3<-retroModels$replist3$derived_quants

#retromodels$replist1 single sex c
#retromodels$replist2 single sex f
#retromodels$replist3 Two sex

nforecastyears <- 3 

tablecomp <- SStableComparisons(retroSummary)
retroSummary$likelihoods_by_fleet
retroSummary$likelihoods
write.csv(tablecomp, file.path(dir_plot, "table models comparison.csv"))

library(reshape2)
library(ggplot2)
library(stringr)
startyr <- unique(retroSummary$startyrs) 
endyr <- unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears <- length(years)
yearsfore <- c(years, years[nyears] + (1:nforecastyears))
nyearsfore <- length(yearsfore) - length(years)

## SSBMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SSB
SSB <- as.data.frame(retroSummary[16])
SSBl <- as.data.frame(retroSummary[18]) # lower
SSBh <- as.data.frame(retroSummary[19]) # upper

## MSY
ssbMSY <- rep(0, nmodels)
ssbMSY[1] <- retroModels$replist1$derived_quants$Value[retroModels$replist1$derived_quants$Label == "SSB_MSY"]
ssbMSY[2] <- retroModels$replist2$derived_quants$Value[retroModels$replist2$derived_quants$Label == "SSB_MSY"]
ssbMSY[3] <- retroModels$replist3$derived_quants$Value[retroModels$replist3$derived_quants$Label == "SSB_MSY"]

df_single <- data.frame(
   ssb = SSB$SpawnBio.replist1/ssbMSY[1],
   lower = SSBl$SpawnBioLower.replist1/ssbMSY[1],
   upper = SSBh$SpawnBioUpper.replist1/ssbMSY[1],
   year=SSB$SpawnBio.Yr,
   model = "Single-sex_f"
)

df_comb <- data.frame(
   ssb = SSB$SpawnBio.replist2/ssbMSY[2],
   lower = SSBl$SpawnBioLower.replist2/ssbMSY[2],
   upper = SSBh$SpawnBioUpper.replist2/ssbMSY[2],
   year=SSB$SpawnBio.Yr,
   model = "Single-sex_c"
)

df_two <- data.frame(
   ssb = SSB$SpawnBio.replist3/ssbMSY[3],
   lower = SSBl$SpawnBioLower.replist3/ssbMSY[3],
   upper = SSBh$SpawnBioUpper.replist3/ssbMSY[3],
   year=SSB$SpawnBio.Yr,
   model= "Two-sex"
)

# Filter for year 2021 in each dataframe
ssb_single_2021 <- df_single %>% filter(year == 2021)
ssb_comb_2021 <- df_comb %>% filter(year == 2021)
ssb_two_2021 <- df_two %>% filter(year == 2021)

# Display SSB for 2021
ssb_single_2021
ssb_comb_2021
ssb_two_2021

df <- rbind(df_single, df_comb, df_two)
df$model <- factor(df$model, levels = c('Single-sex_f', 'Single-sex_c', 'Two-sex'))


library(ggplot2)

# Define colors for the models
css1 <- "#2c2142"  # Two sex
css2 <- "#ffad70"  # Single sex comb
css3 <- "#43428e"  # Single sex fem


# Create the updated plot with custom line types
ssb_msy <- ggplot(df, aes(x = year)) +
   geom_hline(yintercept = 1, col = "black", size = 0.5, linetype = "dotted") +
   
   # Use geom_ribbon for uncertainty intervals
   geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.4, color = "transparent") +
   
   # Plot the main SSB lines with specific line types for each model
   geom_line(aes(y = ssb, color = model, linetype = model), size = 0.8) +
   
   # Set limits
    #ylim(0, 3.1) + xlim(1980, 2021) +
   ylim(0, 4) + xlim(1950, 2021) +
   
   # Customize colors and fills for each model
   scale_fill_manual(name = "Model",
                     values = c("Single-sex_f" = css1, "Single-sex_c" = css2, "Two-sex" = css3),
                     labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   
   # Customize line colors for each model
   scale_color_manual(name = "Model",
                      values = c("Single-sex_f" = css1, "Single-sex_c" = css2, "Two-sex" = css3),
                      labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   
   # Customize line types for each model
   scale_linetype_manual(name = "Model",
                         values = c("Single-sex_f" = "dashed", "Single-sex_c" = "longdash", "Two-sex" = "solid"),
                         labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   
   # Customize the theme
   theme_light() +
   ylab("SSB/SSBMSY") +
   
   theme(legend.position = "bottom",
         legend.key.width = unit(c(0.9), "cm"),
         legend.key.height = unit(c(0.4), "cm"),
         legend.box.spacing = unit(0, "pt"),
         legend.margin = margin(0, 0, 0, 0),
         legend.text = element_text(size = 7),
         legend.title = element_text(size = 7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(10, 10, 0, 10), "pt")) +
   
   # Guides for legends
   guides(
      fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
      color = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
      linetype = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"))
   ) + xlim(1980,2020) + ylim(0,2) + xlab(" ")

# Plot
ssb_msy

## FMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# F
Fv <- as.data.frame(retroSummary[29])
Fl <- as.data.frame(retroSummary[31]) 
Fh <- as.data.frame(retroSummary[32]) 

## FMSY
fMSY <- rep(0, nmodels)
fMSY[1] <- retroModels$replist1$derived_quants$Value[retroModels$replist1$derived_quants$Label == "annF_MSY"]
fMSY[2] <- retroModels$replist2$derived_quants$Value[retroModels$replist2$derived_quants$Label == "annF_MSY"]
fMSY[3] <- retroModels$replist3$derived_quants$Value[retroModels$replist3$derived_quants$Label == "annF_MSY"]

# Restructure the Ft data for each model
df_single <- data.frame(
   F = Fv$Fvalue.replist1 / fMSY[1],
   lower = Fl$FvalueLower.replist1 / fMSY[1],
   upper = Fh$FvalueUpper.replist1 / fMSY[1],
   year = Fv$Fvalue.Yr,
   model = "Single-sex_f"
)

df_comb <- data.frame(
   F = Fv$Fvalue.replist2 / fMSY[2],
   lower = Fl$FvalueLower.replist2 / fMSY[2],
   upper = Fh$FvalueUpper.replist2 / fMSY[2],
   year = Fv$Fvalue.Yr,
   model = "Single-sex_c"
)

df_two <- data.frame(
   F = Fv$Fvalue.replist3 / fMSY[3],
   lower = Fl$FvalueLower.replist3 / fMSY[3],
   upper = Fh$FvalueUpper.replist3 / fMSY[3],
   year = Fv$Fvalue.Yr,
   model = "Two-sex"
)

# Filter for year 2021 in each dataframe
F_single_2021 <- df_single %>% filter(year == 2021)
F_comb_2021 <- df_comb %>% filter(year == 2021)
F_two_2021 <- df_two %>% filter(year == 2021)

# Display SSB for 2021
F_single_2021
F_comb_2021
F_two_2021

# Combine the data frames into one unified data frame
df_F <- rbind(df_single, df_comb, df_two)
df_F$model <- factor(df_F$model, levels = c('Single-sex_f', 'Single-sex_c', 'Two-sex'))

# Define colors for the models (same as the previous plot)
css1 <- "#2c2142"  # Two sex
css2 <- "#ffad70"  # Single sex comb
css3 <- "#43428e"  # Single sex fem

# Create the F/FMSY plot
f_msy <- ggplot(data = df_F, aes(x = year)) +
   geom_hline(yintercept = 1, col = "black", size = 0.5, linetype = "dotted") +
   
   # Use geom_ribbon for uncertainty intervals
   geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.4, color = "transparent") +
   
   # Plot the main F lines with specific line types for each model
   geom_line(aes(y = F, color = model, linetype = model), size = 0.8) +
   
   # Set axis limits
   xlim(1980, 2021) + ylim(0, 5) +
   
   scale_fill_manual(name = "Model",
                     values = c("Single-sex_f" = css1, "Single-sex_c" = css2, "Two-sex" = css3),
                     labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   
   scale_color_manual(name = "Model",
                      values = c("Single-sex_f" = css1, "Single-sex_c" = css2, "Two-sex" = css3),
                      labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   
   scale_linetype_manual(name = "Model",
                         values = c("Single-sex_f" = "dashed", "Single-sex_c" = "longdash", "Two-sex" = "solid"),
                         labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex'))+
   # Customize the theme
   theme_light() +
   ylab("F/FMSY") +
   xlab("Year")+
   
   theme(legend.position = "bottom",
         legend.key.width = unit(c(0.9), "cm"),
         legend.key.height = unit(c(0.4), "cm"),
         legend.box.spacing = unit(0, "pt"),
         legend.margin = margin(0, 0, 0, 0),
         legend.text = element_text(size = 7),
         legend.title = element_text(size = 7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(10, 10, 0, 10), "pt")) +
   
   # Guides for legends
   guides(
      fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
      color = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
      linetype = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"))
   )

# Plot the updated F/FMSY plot
f_msy

## Yield  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Single sex vs sex sep, equilibrium yield catch, SSB and depletion 

## single sex c ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Directory
ass.wd <- paste(dir_dat, "/Single sex c/Model/M0 base model", sep = "")

## SS3 output
replist <- SSgetoutput(dirvec = ass.wd, getcovar = F, verbose = FALSE)[[1]]

## Yield-per-recruit analysis
equil_yield <- replist[["equil_yield"]]

## MSY values
ssBmsy <- replist[["derived_quants"]]["SSB_MSY","Value"]
ssMSY1 <- replist[["derived_quants"]]["Dead_Catch_MSY","Value"]
ssFmsy <- replist[["derived_quants"]]["annF_MSY","Value"]

library(dplyr)

msy_vals <- equil_yield %>% select(SPRloop:Tot_Catch) %>% 
   filter(Tot_Catch == ssMSY1) %>% select(F_report, SSB, Tot_Catch) %>% .[1,]

msy_vals

## Ranges (95% MSY)

msy_ranges <- function(obj, msy) {
   
   msy_lowupp <- obj %>% select(SPRloop:Tot_Catch) %>% 
      mutate(dif = Tot_Catch - msy*.95)
   
   fs <- sort(obj[["F_report"]])
   
   # lower bound
   xlow <- msy_lowupp %>% filter(F_report < ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
   posl <- which(fs == xlow$F_report)
   posl <- c(posl, ifelse( xlow$dif < 0, posl+1, posl-1))
   flow <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posl])), 
                    data.frame(Tot_Catch = msy*.95))[[1]]
   
   # upper bound
   xupp <- msy_lowupp %>% filter(F_report > ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
   posu <- which(fs == xupp$F_report)
   posu <- c(posu, ifelse( xupp$dif < 0, posu+1, posu-1))
   fupp <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posu])), 
                    data.frame(Tot_Catch = msy*.95))[[1]]
   
   return(c(flow=flow, fupp=fupp)) 
   
}

msy_rg <- msy_ranges(equil_yield, msy = ssMSY1)

## MSY Franges
Fmsy_tmp1 <- msy_vals$F_report #replist[["derived_quants"]]["annF_MSY","Value"]
Fmsy_low1 <- msy_rg[['flow']]
Fmsy_upp1 <- msy_rg[['fupp']]

df <- cbind(equil_yield[["Tot_Catch"]], equil_yield[["F_report"]], equil_yield[["SSB"]], equil_yield[["Depletion"]])
class(df)
df <- as.data.frame(df)
df2 <- df
colnames(df2) <- c("Tot. Catch", "F report", "SSB", "Depletion")

## single sex f ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Directory
ass.wd <- paste(dir_dat, "/Single sex f/Model/M0 base model", sep = "")

## SS3 output
replist <- SSgetoutput(dirvec = ass.wd, getcovar = F, verbose = FALSE)[[1]]

## Yield-per-recruit analysis
equil_yield <- replist[["equil_yield"]]

## MSY values
ssBmsy <- replist[["derived_quants"]]["SSB_MSY","Value"]
ssMSY3 <- replist[["derived_quants"]]["Dead_Catch_MSY","Value"]
ssFmsy <- replist[["derived_quants"]]["annF_MSY","Value"]

library(dplyr)

msy_vals <- equil_yield %>% select(SPRloop:Tot_Catch) %>% 
   filter(Tot_Catch == ssMSY3) %>% select(F_report, SSB, Tot_Catch) %>% .[1,]

msy_vals

## Ranges (95% MSY)

msy_ranges <- function(obj, msy) {
   
   msy_lowupp <- obj %>% select(SPRloop:Tot_Catch) %>% 
      mutate(dif = Tot_Catch - msy*.95)
   
   fs <- sort(obj[["F_report"]])
   
   # lower bound
   xlow <- msy_lowupp %>% filter(F_report < ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
   posl <- which(fs == xlow$F_report)
   posl <- c(posl, ifelse( xlow$dif < 0, posl+1, posl-1))
   flow <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posl])), 
                    data.frame(Tot_Catch = msy*.95))[[1]]
   
   # upper bound
   xupp <- msy_lowupp %>% filter(F_report > ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
   posu <- which(fs == xupp$F_report)
   posu <- c(posu, ifelse( xupp$dif < 0, posu+1, posu-1))
   fupp <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posu])), 
                    data.frame(Tot_Catch = msy*.95))[[1]]
   
   return(c(flow=flow, fupp=fupp)) 
   
}

msy_rg <- msy_ranges(equil_yield, msy = ssMSY3)

## MSY Franges
Fmsy_tmp3 <- msy_vals$F_report #replist[["derived_quants"]]["annF_MSY","Value"]
Fmsy_low1 <- msy_rg[['flow']]
Fmsy_upp1 <- msy_rg[['fupp']]

df <- cbind(equil_yield[["Tot_Catch"]], equil_yield[["F_report"]], equil_yield[["SSB"]], equil_yield[["Depletion"]])
class(df)
df <- as.data.frame(df)
df3 <- df
colnames(df3) <- c("Tot. Catch", "F report", "SSB", "Depletion")

## two sex ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Directory
ass.wd <- paste(dir_dat, "/Two sex/Model/M0 base model", sep = "")

## SS3 output
replist <- SSgetoutput(dirvec = ass.wd, getcovar = F, verbose = FALSE)[[1]]

## Yield-per-recruit analysis
equil_yield <- replist[["equil_yield"]]

## MSY values
ssBmsy <- replist[["derived_quants"]]["SSB_MSY","Value"]
ssMSY <- replist[["derived_quants"]]["Dead_Catch_MSY","Value"]
ssFmsy <- replist[["derived_quants"]]["annF_MSY","Value"]

msy_vals <- equil_yield %>% select(SPRloop:Tot_Catch) %>% 
   filter(Tot_Catch == ssMSY) %>% select(F_report, SSB, Tot_Catch) %>% .[1,]
msy_vals

## Ranges (95% MSY)
msy_ranges <- function(obj, msy) {
   
   msy_lowupp <- obj %>% select(SPRloop:Tot_Catch) %>% 
      mutate(dif = Tot_Catch - msy*.95)
   
   fs <- sort(obj[["F_report"]])
   
   # lower bound
   xlow <- msy_lowupp %>% filter(F_report < ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
   posl <- which(fs == xlow$F_report)
   posl <- c(posl, ifelse( xlow$dif < 0, posl+1, posl-1))
   flow <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posl])), 
                    data.frame(Tot_Catch = msy*.95))[[1]]
   
   # upper bound
   xupp <- msy_lowupp %>% filter(F_report > ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
   posu <- which(fs == xupp$F_report)
   posu <- c(posu, ifelse( xupp$dif < 0, posu+1, posu-1))
   fupp <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posu])), 
                    data.frame(Tot_Catch = msy*.95))[[1]]
   
   return(c(flow=flow, fupp=fupp)) 
   
}

msy_rg <- msy_ranges(equil_yield, msy = ssMSY)

## MSY Franges
Fmsy_tmp <- msy_vals$F_report #replist[["derived_quants"]]["annF_MSY","Value"]
Fmsy_low <- msy_rg[['flow']]
Fmsy_upp <- msy_rg[['fupp']]

df <- cbind(equil_yield[["Tot_Catch"]], equil_yield[["F_report"]], equil_yield[["SSB"]], equil_yield[["Depletion"]])
class(df)
df <- as.data.frame(df)
df1 <- df
colnames(df1) <- c("Tot. Catch", "F report", "SSB", "Depletion")

# Plots
df1$Model <- "Two-sex"
df2$Model <- "Single-sex_f"
df3$Model <- "Single-sex_c"

## Join datasets
dftot <- rbind(df2, df3, df1)
dftot$Model <- factor(dftot$Model, levels = c("Single-sex_f", "Single-sex_c", "Two-sex"))

## Colors
css1 <- "#2c2142" # single
css2 <- "#ffad70" # single comb
css3 <- "#43428e" # two sex

## Plot
ycatch <- ggplot(data = dftot, aes(y = `Tot. Catch`, x = `F report`, colour = Model, linetype = Model)) +
   geom_line(size = 0.8) + 
   ## Single-sex
   geom_segment(x = Fmsy_tmp1, xend = Fmsy_tmp1, y = 0, yend = ssMSY1, col = css1, size = .6, linetype = "solid")+
   geom_vline(xintercept = 0.6, col = css1, size = .6, linetype = "solid")+
   
   ## Single II
   geom_segment(x = Fmsy_tmp3, xend = Fmsy_tmp3, y = 0, yend = ssMSY3, col = css2, size = .6, linetype = "dashed")+
   geom_vline(xintercept = 0.56, col = css2, size = .6, linetype = "dashed")+
   
   ## Two-sex
   geom_segment(x = Fmsy_tmp, xend = Fmsy_tmp, y = 0, yend = ssMSY, col = css3, size = .6, linetype = "longdash")+
   geom_vline(xintercept = 0.63, col = css3, size = .6, linetype = "longdash")+
   
   # Text annotations
   geom_text(aes(label = "FMSY", x = Fmsy_tmp + 0.02, y = 8000), angle = 0, hjust = 0, col = "black", size = 2)+
   geom_text(aes(label = "Fcrash", x = 0.65, y = 8000), angle = 0, hjust = 0, col = "black", size = 2)+ 
   ylim(0, 28000) + xlim(0, 0.8)+
   scale_linetype_manual(name = "Model", values = c( "longdash", "dashed","solid"), 
                         labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   scale_colour_manual(name = "Model", values = c(css1, css2, css3), 
                       labels = c('Single-sex_f', 'Single-sex_c', 'Two-sex')) +
   ylab("Total catch (tons)") + xlab("Fishing mortality (F)")+
   theme_light() +
   theme(legend.position = "none",
         legend.box.spacing = unit(0, "pt"),
         legend.margin = margin(0, 0, 0, 0),
         legend.text = element_text(size = 7),
         legend.title = element_text(size = 7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(10, 10, 0, 10), "pt"))+
         scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 30000, by = 5000)) 

ycatch

## Arrange ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggpubr)

# Get the legend from ssb_msy plot
legend_ssb <- get_legend(ssb_msy + theme(legend.text = element_text(size = 5.5)))

# Arrange the plots
g1 <- ggarrange(ycatch, ssb_msy + theme(legend.position = "none"), f_msy + theme(legend.position = "none"), 
                ncol = 1, nrow = 3, align = "hv", labels = c("A", "B", "C"), font.label = list(color = "black", size = 9))

# Add legend from ssb_msy plot
g1 <- ggarrange(g1, legend_ssb, ncol = 1, heights = c(3, 0.1))



dev.off()
g1+  bgcolor("white") 
ggsave(paste0(dir_plot,"/05 Stock status final.jpeg"), height=4, width=3, dpi=800, g1)


# 06 PROJECTIONS ---------------------------------------------------------------

## Single sex c ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# F0 scenarios for volpal and trawl fleets
# We will compare SSB for the models fishing at FMsy (correct way to compare)

# Debemos ir al modelo base y escenarios, y buscar en la tabla de forecast
# dónde esta Fmsy (F a la cual la captura es máxima)
# Esos son los escenarios que podemos y queremos comparar

## Clean environment
rm(list=ls())
library(r4ss)

## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-file.path(getwd(), "../01 Models/Single sex c/Model/")
dir.create(dir_dat)

## Select models to compare
models<-c("M1 M0 F0 volpal/forecast/Fmult2", # FMsy = 0.209, Fmult = 2
          "M2 M0 F0 trawl/forecast/Fmult4.2" # FMsy = 0.205, Fmult = 4.2
) # R0 must be always placed here 

nmodels <- length(models)
filemod <- paste0(dir_dat,models) 

## Read and summarize output
retroModels <- SSgetoutput(dirvec=filemod)
retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1

nforecastyears<-40 # Set to 3 in starter file

tablecomp<-SStableComparisons(retroSummary)
#write.csv(tablecomp, paste(dir_plot,"/table models comparison single sex scenarios.csv",sep=""))

## SSBMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(reshape2)
library(ggplot2)
library(stringr)
startyr<-unique(retroSummary$startyrs)
endyr<-unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears<-length(years)
yearsfore <- c(years, years[nyears]+(1:nforecastyears))
nyearsfore=length(yearsfore)-length(years)
SSB <- as.data.frame(retroSummary[16])
nrSSB=nrow(SSB)
SSB <- SSB[-c(1,2),] 
SSB <- SSB[,-(nmodels+1)]
names(SSB) <- c(models, "Year")

## Colors
css1<- "#2c2142" # single
css2<-"#43428e" # sex sep

## Plot
SSB <- melt(SSB, id="Year")

colnames(SSB)<- c("Year", "Model", "SSB")
ssb <- ggplot(SSB, aes(Year, SSB, col=Model, linetype=Model)) +
   geom_line(size=0.8) + xlab(" ") + ylab("SSB at FMSY") +
   theme_light()+xlim(2000,max(yearsfore)) + ylim(0,130000)+
   geom_vline(xintercept = 2021, col=css1, size=.6, linetype="dotted")+
   scale_colour_manual(name="Scenarios",values=c("#ffad70",css2), labels=c('target small','target large'))+
   scale_linetype_manual(name="Scenarios",values = c("solid","longdash"), labels=c('target small','target large'))+
   geom_text(aes(label = "projection",x=2022, y = 7000), angle = 0, hjust = 0, col="black", size=2, fontface="italic")+
   geom_text(aes(label = "Single-sex_c",x=2000, y = 110000), angle = 0, hjust = 0, col="black", size=2.5)+
   # geom_text(aes(label = "single sex",x=2001, y = 63000), angle = 0, hjust = 0, col="black", size=2.5)+
   theme(legend.position = "none",
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7.5),
         legend.title = element_text(size=7.5),
         axis.title.x = element_text(size = 8),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title.y = element_text(size = 8),
         plot.margin = unit(c(10,10,0,10), "pt")  )  +
         scale_y_continuous(limits = c(0, 130000), breaks = seq(0, 130000, by = 20000)) 

ssb

## Yield ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tab_M0base<-read.csv(file=paste0(dir_dat,"M0 base model/forecast/table/table Fmult.csv"))
tab_M0base$Scenario<-rep("Base")
tab_M1vol<-read.csv(file=paste0(dir_dat,"M1 M0 F0 volpal/forecast/table/table Fmult.csv"))
tab_M1vol$Scenario<-rep("F0 vol")
tab_M2tra<-read.csv(file=paste0(dir_dat,"M2 M0 F0 trawl/forecast/table/table Fmult.csv"))
tab_M2tra$Scenario<-rep("F0 trawl")

df1<-rbind(tab_M1vol,tab_M2tra) # quitar base tab_M0base,


yield_sin<-ggplot(data=df1, aes(y=`Catches`, x=`F`, colour=Scenario, linetype=Scenario))+
   geom_line(size=0.8) + 
   theme_light()+ ylab("Total catch (tons)")+ xlab(" ")+
   scale_colour_manual(name="Scenarios",values=c(css2,"#ffad70"), labels=c('target large','target small'))+
   scale_linetype_manual(name="Scenarios",values = c("longdash","solid"), labels=c('target large','target small'))+
   geom_text(aes(label = "FMSY",x=0.25, y = 9000), angle = 0, hjust = 0, col="black", size=2)+
   #geom_text(aes(label = "Single sex",x=0.005, y = 23000), angle = 0, hjust = 0, col="black", size=2.5, fontface="italic")+
   #geom_segment(x = 0.17859600,xend=0.17859600 ,y=0, yend=19338.627 , size=0.6, linetype="solid", col=css1)+
   geom_segment(x = 0.209984,xend=0.209984 ,y=0, yend=16160.438 , size=0.6, linetype="solid", col="#ffad70")+
   geom_segment(x = 0.205961,xend=0.205961 ,y=0, yend=26053.49 , size=0.6, linetype="longdash", col=css2)+
   theme(legend.position = "none",
         legend.key.width = unit(c(0.9), "cm"),
         legend.key.height = unit(c(0.4), "cm"),
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7.5),
         legend.title = element_text(size=7.5),
         axis.title.x = element_text(size = 8),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title.y = element_text(size = 8),
         plot.margin = unit(c(10,10,0,10), "pt") )+
   ylim(0,36200) + xlim(0,1.5)+
   scale_y_continuous(limits = c(0, 36200), breaks = seq(0, 36200, by = 5000))   # Customize breaks


yield_sin


## Single sex f ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# single-sex_F

# F0 scenarios for volpal and trawl fleets
# We will compare SSB for the models fishing at FMsy (correct way to compare)

# Debemos ir al modelo base y escenarios, y buscar en la tabla de forecast
# dónde esta Fmsy (F a la cual la captura es máxima)
# Esos son los escenarios que podemos y queremos comparar


## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-file.path(getwd(), "../01 Models/Single sex f/Model/")
dir.create(dir_dat)

## Select models to compare
models<-c("M1 M0 F0 volpal/forecast/Fmult1.2", # FMsy = 0.192311, Fmult = 1.2
          "M2 M0 F0 trawl/forecast/Fmult2.2" # FMsy = 0.187771, Fmult = 2.2
) # R0 must be always placed here 

nmodels <- length(models)
filemod <- paste0(dir_dat,models) 

## Read and summarize output
retroModels <- SSgetoutput(dirvec=filemod)
retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1

nforecastyears<-40 # Set to 3 in starter file

tablecomp<-SStableComparisons(retroSummary)
#write.csv(tablecomp, paste(dir_plot,"/table models comparison single sex scenarios.csv",sep=""))

## SSBMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(reshape2)
library(ggplot2)
library(stringr)
startyr<-unique(retroSummary$startyrs)
endyr<-unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears<-length(years)
yearsfore <- c(years, years[nyears]+(1:nforecastyears))
nyearsfore=length(yearsfore)-length(years)
SSB <- as.data.frame(retroSummary[16])
nrSSB=nrow(SSB)
SSB <- SSB[-c(1,2),] 
SSB <- SSB[,-(nmodels+1)]
names(SSB) <- c(models, "Year")

## Colors
css1<- "#2c2142" # single
css2<-"#43428e" # sex sep

## Plot
SSB <- melt(SSB, id="Year")

colnames(SSB)<- c("Year", "Model", "SSB")
ssbII <- ggplot(SSB, aes(Year, SSB, col=Model, linetype=Model)) +
   geom_line(size=0.8) + xlab("Year") + ylab("SSB at FMSY") +
   theme_light()+xlim(2000,max(yearsfore)) + ylim(0,130000)+
   geom_vline(xintercept = 2021, col=css1, size=.6, linetype="dotted")+
   scale_colour_manual(name="Scenarios",values=c("#ffad70",css2), labels=c('target small','target large'))+
   scale_linetype_manual(name="Scenarios",values = c("solid","longdash"), labels=c('target small','target large'))+
   geom_text(aes(label = "projection",x=2022, y = 7000), angle = 0, hjust = 0, col="black", size=2, fontface="italic")+
   geom_text(aes(label = "Single-sex_f",x=2000, y = 110000), angle = 0, hjust = 0, col="black", size=2.5)+
   # geom_text(aes(label = "single sex",x=2001, y = 63000), angle = 0, hjust = 0, col="black", size=2.5)+
   theme(legend.position = "none",
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7.5),
         legend.title = element_text(size=7.5),
         axis.title.x = element_text(size = 8),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title.y = element_text(size = 8),
         plot.margin = unit(c(10,10,0,10), "pt")  )+
   scale_y_continuous(limits = c(0, 130000), breaks = seq(0, 130000, by = 20000)) 

ssbII

## Yield ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tab_M0base<-read.csv(file=paste0(dir_dat,"M0 base model/forecast/table/table Fmult.csv"))
tab_M0base$Scenario<-rep("Base")
tab_M1vol<-read.csv(file=paste0(dir_dat,"M1 M0 F0 volpal/forecast/table/table Fmult.csv"))
tab_M1vol$Scenario<-rep("F0 vol")
tab_M2tra<-read.csv(file=paste0(dir_dat,"M2 M0 F0 trawl/forecast/table/table Fmult.csv"))
tab_M2tra$Scenario<-rep("F0 trawl")

df1<-rbind(tab_M1vol,tab_M2tra) # quitar base tab_M0base,


yield_sinII<-ggplot(data=df1, aes(y=`Catches`, x=`F`, colour=Scenario, linetype=Scenario))+
   geom_line(size=0.8) + 
   theme_light()+ ylab("Total catch (tons)")+ xlab("Fishing Mortality (F)")+
   scale_colour_manual(name="Scenarios",values=c(css2,"#ffad70"), labels=c('target large','target small'))+
   scale_linetype_manual(name="Scenarios",values = c("longdash","solid"), labels=c('target large','target small'))+
   geom_text(aes(label = "FMSY",x=0.25, y = 9000), angle = 0, hjust = 0, col="black", size=2)+
   #geom_text(aes(label = "Single sex",x=0.005, y = 23000), angle = 0, hjust = 0, col="black", size=2.5, fontface="italic")+
   #geom_segment(x = 0.17859600,xend=0.17859600 ,y=0, yend=19338.627 , size=0.6, linetype="solid", col=css1)+
   geom_segment(x = 0.224363,xend=0.224363 ,y=0, yend=19275.412 , size=0.6, linetype="solid", col="#ffad70")+
   geom_segment(x = 0.187771,xend=0.187771 ,y=0, yend=36177.88 , size=0.6, linetype="longdash", col=css2)+
   #geom_text(aes(label = "Single-sex_f",x=0.7, y = 32000), angle = 0, hjust = 0, col="black", size=2.5)+
   theme(legend.position = "none",
         legend.key.width = unit(c(0.9), "cm"),
         legend.key.height = unit(c(0.4), "cm"),
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7.5),
         legend.title = element_text(size=7.5),
         axis.title.x = element_text(size = 8),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title.y = element_text(size = 8),
         plot.margin = unit(c(10,10,0,10), "pt") )+
   ylim(0,36200) + xlim(0,1.5)+
   scale_y_continuous(limits = c(0, 36200), breaks = seq(0, 36200, by = 5000))   # Customize breaks


yield_sinII


## Two sex ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# OJO, usar SSBMsy y FMSY, ya que al cambiar Fs en proyecciones cambias selectividad
# Sacar plot de SSB, F y Catch a poder ser
# Es necesario igualar las F's, que se respete la proporción 
# pero que la F final sea la misma en todos los escenarios

## Clean environment
#rm(list=ls())
library(r4ss)

## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-file.path(getwd(), "../01 Models/Two sex/Model/")
dir.create(dir_dat)

## Select models to compare
models<-c("M1 M0 F0 volpal/forecast/Fmult1.4",# FMSY = 0.221564, mult 1.4
          "M2 M0 F0 trawl/forecast/Fmult2.25" # FMSY = 0.2062618, mult 2.3
) # R0 must be always placed here 

nmodels <- length(models)
filemod <- paste0(dir_dat,models) 

## Read and summarize output
retroModels <- SSgetoutput(dirvec=filemod)
retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1

nforecastyears<-40 # Set to 3 in starter file

tablecomp<-SStableComparisons(retroSummary)
#write.csv(tablecomp, paste(dir_plot,"/table models comparison single sex scenarios.csv",sep=""))


## SSBMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(reshape2)
library(ggplot2)
library(stringr)
startyr<-unique(retroSummary$startyrs)
endyr<-unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears<-length(years)
#nforecastyears<-6 # Set to 3 in starter file
yearsfore <- c(years, years[nyears]+(1:nforecastyears))
nyearsfore=length(yearsfore)-length(years)
SSB <- as.data.frame(retroSummary[16])
nrSSB=nrow(SSB)
#seq_aux=((nrSSB-nyearsfore)+1):nrSSB
SSB <- SSB[-c(1,2),] 
SSB <- SSB[,-(nmodels+1)]
names(SSB) <- c(models, "Year")


## Plot
SSB2 <- melt(SSB, id="Year")
colnames(SSB2)<- c("Year", "Model", "SSB")
ssb2 <- ggplot(SSB2, aes(Year, SSB, col=Model, linetype=Model))+geom_line(size=0.8) + 
   theme_light()+xlim(2000,max(yearsfore))+ ylim(0,130000)+  ylab("SSB at FMSY")+xlab(" ")+
   geom_vline(xintercept = 2021, col=css1, size=.6, linetype="dotted")+
   scale_colour_manual(name="Scenarios",values=c("#ffad70",css2), labels=c('target small','target large'))+
   scale_linetype_manual(name="Scenarios",values = c("solid","longdash"), labels=c('target small','target large'))+  
   geom_text(aes(label = "projection",x=2022, y = 7000), angle = 0, hjust = 0, col="black", size=2, fontface="italic")+
   geom_text(aes(label = "Two-sex",x=2000, y = 110000), angle = 0, hjust = 0, col="black", size=2.5)+
   #geom_text(aes(label = "separate sex",x=2001, y = 63500), angle = 0, hjust = 0, col="black", size=2.5)+
   theme(legend.position = "none",
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7.5),
         legend.title = element_text(size=7.5),
         axis.title.x = element_text(size = 8),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title.y = element_text(size = 8),
         plot.margin = unit(c(10,10,-5,10), "pt") )+
   scale_y_continuous(limits = c(0, 130000), breaks = seq(0, 130000, by = 20000)) 


ssb2

## Yield  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tab_M0base<-read.csv(file=paste0(dir_dat,"M0 base model/forecast/table/table Fmult.csv"))
tab_M0base$Scenario<-rep("Base")
tab_M1vol<-read.csv(file=paste0(dir_dat,"M1 M0 F0 volpal/forecast/table/table Fmult.csv"))
tab_M1vol$Scenario<-rep("F0 vol")
tab_M2tra<-read.csv(file=paste0(dir_dat,"M2 M0 F0 trawl/forecast/table/table Fmult.csv"))
tab_M2tra$Scenario<-rep("F0 trawl")

df<-rbind(tab_M1vol,tab_M2tra)#base

yield_sex<-ggplot(data=df, aes(y=`Catches`, x=`F`, colour=Scenario, linetype=Scenario))+
   geom_line(size=0.8) + 
   theme_light()+ ylab("Total catch (tons)")+
   scale_colour_manual(name="Scenarios",values=c(css2,"#ffad70"), labels=c('target large','target small'))+
   scale_linetype_manual(name="Scenarios",values = c("longdash","solid"), labels=c('target large','target small'))+
   geom_text(aes(label = "FMSY",x=0.25, y = 9000), angle = 0, hjust = 0, col="black", size=2)+
   #geom_text(aes(label = "Two-sex",x=0.7, y = 32000), angle = 0, hjust = 0, col="black", size=2.5)+
   #geom_segment(x = 0.1865950,xend=0.1865950 ,y=0, yend=23331.248 , size=0.6, linetype="solid", col=css1)+
   geom_segment(x = 0.2215640,xend=0.2215640 ,y=0, yend=1.886465e+04, size=0.6, linetype="solid", col="#ffad70")+
   geom_segment(x = 0.2022250,xend=0.2022250 ,y=0, yend=30192.060 , size=0.6, linetype="longdash", col=css2)+
   theme(legend.position = "bottom",
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7.5),
         legend.title = element_text(size=7.5),
         axis.title.x = element_text(size = 8),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title.y = element_text(size = 8),
         plot.margin = unit(c(10,10,0,10), "pt") )+
   guides(color = guide_legend(keywidth = unit(1.4, "cm"))) +
   ylim(0,36200) + xlim(0,1.5)+
   scale_y_continuous(limits = c(0, 36200), breaks = seq(0, 36200, by = 5000))   # Customize breaks
   

yield_sex #+ labs(title = " ", tag = "B")

## Compare plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggpubr)

# Get the legend from yield_sex plot
legend_yield_sex <- get_legend(yield_sex)

# Arrange the plots without the original legend
g2 <- ggarrange(ssb2, yield_sex,ssb,yield_sin , ssbII,yield_sinII + theme(legend.position = "bottom"), 
                ncol = 2, nrow = 3, common.legend = TRUE, align = "hv",
                labels = c("A", "B", "C", "D","E","F"), font.label = list(color = "black", size = 9))
#labels = c("A", "D", "B", "E","C","F")
# Add legend from yield_sex plot at the bottom 
#g2 <- ggarrange(g2, legend_yield_sex, ncol = 1, heights = c(3, 0.1), legend = "bottom")

g2
# Save the plot
ggsave(paste0(dir_plot, "/06 Projections final scaled.jpeg"), plot = g2, height = 5.5, width = 6, dpi=800)


# 07 EXPLOITATION  -------------------------------------------------------------

# sex ratio estimated 

# sex ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Clean environment
rm(list=ls())
library(r4ss)

## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-file.path(getwd(), "../01 Models/Two sex/Model")
dir.create(dir_dat)

## Read
run <- 'M0 base model'
mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)

df<-replist$natage

## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)
df<-df%>%filter(Era=="TIME",`Beg/Mid`=="B") %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Yr,Sex, variable), summarize,  Number=mean(value))

df_fem<-subset(df, df$Sex=="female")
df_mal<-subset(df, df$Sex=="male")

# Join datasets by year and variable
df_join<-full_join(df_fem, df_mal, by = c("Yr","variable"))  
df_join$ratio=df_join$Number.x/(df_join$Number.x+df_join$Number.y) 

# Plot biomasa at length
library(ggplot2)
ggplot(df_join, aes(x =as.numeric(variable), 
                    colour=factor(Yr), y=ratio))+
   geom_line()  + #facet_wrap(~Yr)+
   labs(colour='Year')+
   theme_light() + scale_color_viridis_d(option="A", alpha=0.8)+ xlim(20,70)+
   theme(plot.margin = margin(1,1,1.5,1.2, "cm"))+guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +   
   theme(legend.key.size = unit(0.5, 'cm'))   + theme(legend.text = element_text(size=9.5)) +  theme(legend.title = element_text(size=10))+
   labs(col='Years') +  theme(legend.position = "bottom")+
   xlab("Length") + ylab("Population numbers sex ratio")

## Save
#ggsave(paste0(dir_plot,"/Population numbers at length sex ratio.jpeg"), width=5.5, height=6)

# Plot by year groups:

df_join$year_groups <- cut(df_join$Yr, breaks = c(1960,1994,2010,2020), include.lowest = TRUE)
levels(df_join$year_groups)<-c("1960-1994","1995-2010","2011-2020")

library(ggplot2)
sr<-ggplot(df_join, aes(x = as.numeric(variable), group_by(year_groups),
                        colour = (year_groups), linetype = year_groups, y = ratio)) +
   geom_hline(yintercept = 0.5, linetype = "dotted", size = 0.7, color = "black") +  # Add horizontal dashed line
   geom_smooth(se = FALSE)  + #facet_wrap(~Yr)+
   scale_colour_manual(name = "Years", values = c("#2c2142", "#705cb5", "#edb9a1")) +
   scale_linetype_manual(name = "Years", values = c("solid", "F1", "longdash")) +
   scale_y_continuous(breaks = seq(0.4, 1, by = 0.1)) +  # Set y-axis breaks from 0.4 to 1 with a step of 0.1
   theme_light() +
   xlim(20, 70) +
   theme(plot.margin = margin(1, 1, 1.5, 1.2, "cm")) +
   guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +   
   theme(legend.key.width = unit(1.2, 'cm'),
         legend.position = "bottom",
         legend.box.spacing = unit(0, "pt"),  # The spacing between the plotting area and the legend box (unit)
         legend.margin = margin(0, 0, 0, 0),
         legend.text = element_text(size = 7),
         legend.title = element_text(size = 7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(4, 10, -3, 10), "pt")) +
   xlab("Length (cm)") + ylab("Sex ratio")

sr
## Save
#ggsave(paste0(dir_plot,"/Population numbers at length sex ratio by groups.jpeg"), width=5.5, height=6)

# scenarios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(r4ss)

## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-file.path(getwd(), "../01 Models/Two sex/Model/M0 base model/forecast/")
dir.create(dir_dat)

## Read
run <- 'Fmult0' # F0

mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)

## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)
df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B", Yr == 2060) %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Yr,Sex, variable), summarize,  Number=mean(value))

df_fem<-subset(df, df$Sex=="female")
df_mal<-subset(df, df$Sex=="male")

# Join datasets by year and variable
df_join<-full_join(df_fem, df_mal, by = c("Yr","variable"))  
df_join$ratio=df_join$Number.x/(df_join$Number.x+df_join$Number.y) 

df_join_F0<-df_join
df_join_F0$scen<-rep("F0")


# --- ## Read

run <- 'Fmult0.9'# FMsy = 0.186595, Fmult = 0.9

mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)


## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)
df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B", Yr == 2060) %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Yr,Sex, variable), summarize,  Number=mean(value))

df_fem<-subset(df, df$Sex=="female")
df_mal<-subset(df, df$Sex=="male")

# Join datasets by year and variable
df_join<-full_join(df_fem, df_mal, by = c("Yr","variable"))  
df_join$ratio=df_join$Number.x/(df_join$Number.x+df_join$Number.y) 

df_join_FMSY<-df_join
df_join_FMSY$scen<-rep("FMSY")

# --- ## Read

run <- 'Fmult1.8'# 2 FMsy= 0.37319

mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)


## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)
df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B", Yr == 2060) %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Yr,Sex, variable), summarize,  Number=mean(value))

df_fem<-subset(df, df$Sex=="female")
df_mal<-subset(df, df$Sex=="male")

# Join datasets by year and variable
df_join<-full_join(df_fem, df_mal, by = c("Yr","variable"))  
df_join$ratio=df_join$Number.x/(df_join$Number.x+df_join$Number.y) 

df_join_2FMSY<-df_join
df_join_2FMSY$scen<-rep("2FMSY")


df_join_full<-rbind(df_join_F0, df_join_FMSY, df_join_2FMSY)

# Plot biomasa at length
library(ggplot2)
sreq<-ggplot(df_join_full, aes(x =as.numeric(variable), 
                               colour=factor(scen),linetype=factor(scen), y=ratio))+
   geom_line(size=0.8) +
   theme_light() + 
   #   scale_colour_manual(name="Year",values=c("#2c2142","#705cb5","#edb9a1"))+
   scale_colour_manual(name="Scenarios",values = c("2FMSY" ="#705cb5",
                                                   "F0"="#2c2142",
                                                   "FMSY"="#edb9a1"))+
   scale_linetype_manual(name="Scenarios",values = c("F1","longdash","solid"), labels=c('2FMSY','F0','FMSY'))+
   theme(plot.margin = margin(1,1,1.5,1.2, "cm"))+guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +   
   theme(legend.position = "bottom",
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7),
         legend.title = element_text(size=7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(3,10,0,10), "pt") )+
   xlab("Length") + ylab("Female ratio in eq.")+ xlim(20,70)

sreq

## Save
#ggsave(paste0(dir_plot,"/Population numbers at length sex ratio.jpeg"), width=5.5, height=6)

r1<-gridExtra::grid.arrange(sr, sreq)
#ggsave(paste0(dir_plot,"/SR and SR eq.jpeg"), width=5.5, height=6, r1)

# OSR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(r4ss)

# Base run ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Run
run <- 'M0 base model'
mod_path <- paste0(getwd(), "/../01 Models/Two sex/Model/", run, sep="") 
dir.create(mod_path) ## check 

## Read output
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)

# MatLen ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Length increments in population length bins (distance between bins)
increments <- replist$lbinspop[-1]-replist$lbinspop[-replist$nlbinspop]
increments <- c(increments, increments[length(increments)])

## Length at mid-point of population length bins 
len <- replist$lbinspop + increments/2
matslope_f <- as.numeric(replist$MGparmAdj$"Mat_slope_Fem")[1]
matl50_f <- as.numeric(replist$MGparmAdj$"Mat50%_Fem")[1]
matlen_f <- 1/( 1 + exp(matslope_f*(len - matl50_f )) ) # Ogiva: % matures by length

## males (from male ojive html)
matslope_m <- -0.334
matl50_m <- 28.25
matlen_m <- 1/( 1 + exp(matslope_m*(len - matl50_m )) ) # Ogiva: % matures by length

# define Fmult ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Flim:	F with 50% probability of SSB>Blim (segreg without Btrigger)
datmul=replist$exploitation
head(subset(datmul, datmul$Yr>=2018),9)

# Hacer media de ultimos 3 años (18,19 y 20)
(denom<-(0.228603       + 0.233716       + 0.159678       )/3) # 3 last years Fbar
flim<-0.63 # Sacado del plot del yield
mulFlim<-flim/denom # este es el multiplier para llegar a Flim.
fmult=seq(0,mulFlim+0.05,by=0.05)# try 0.01

# F scenarios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

path <- paste0(mod_path, "/forecast")  # Example directory
scen<-list.dirs(path)[2:63] # Get full directory paths
scen # scenarios F

# OSR scenarios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create data.frame to save values
osr=list()
Fval=list()
Nrec_f=list()
Nrec_m=list()

for(j in seq_along(scen)) { 
   
   output = SS_output(dir = scen[j], covar = FALSE) 
   
   # Numbers at length
   df<-output$natlen
   
   ## Filter and select
   library(dplyr)
   df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B") %>% # eq year 2060?
      select (-c(1,2,4,5,6,7,10,11,12))
   
   ## Columns to rows
   library(reshape2)
   df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))
   
   ## Recode factor levels with names
   library(dplyr)
   df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 
   
   ## Summarise across season and year
   library(plyr)
   df<-ddply(df, .(Yr, Sex, variable), summarize,  numbers=sum(value))
   
   ## Mean across years
   library(plyr)
   #dfm<-ddply(df, .(Sex, variable), summarize,  numbers=mean(numbers))
   dfm=subset(df, df$Yr==2060)
   
   ## Subset males and females
   df_fem<-subset(dfm, Sex=="female")
   df_male<-subset(dfm, Sex=="male")
   
   ## OSR
   numerador=sum(df_fem$numbers * matlen_f)
   denominador= (numerador + sum(df_male$numbers * matlen_m))
   sro=numerador/denominador
   osr[j]=sro
   
   ## F value 
   Fv <- output$derived_quants
   Fvalu <- Fv[substr(Fv$Label,1,2)=="F_",]; 
   Fvalue <- Fvalu[length(Fvalu$Value),2] # F value at last year (eq)
   Fval[j]=Fvalue
   
   ## Save
   Nrec_f[j] = df_fem
   Nrec_m[j] = df_male
   
   
}

osr # OSR
Fval # F value
fmult # F multiplier scenario

osr_tab<-cbind(osr,fmult,Fval)
osr_tab<-as.data.frame(osr_tab)
osr_tab$osr<-as.numeric(osr_tab$osr)
osr_tab$Fval<-as.numeric(osr_tab$Fval)
osr_tab$fmult<-as.numeric(osr_tab$fmult)
names(osr_tab)<-c("osr","Fmult","Fvalue")
head(osr_tab)

## Save
#write.csv(osr_tab, "sex-sep OSR table.csv")

# Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Colors
css1<- "#2c2142" # single
css2<-"#43428e" # sex sep

library(ggplot2)
posr<-ggplot(data=osr_tab, aes(x=Fvalue, y=osr)) +
   geom_line(size=0.8, col=css1) + theme_light() +
   xlab("F") + ylab("OSR") + ylim(0,1) +
   theme(legend.position = "none",
         legend.box.spacing = unit(0, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.margin=margin(0,0,0,0),
         legend.text=element_text(size=7),
         legend.title = element_text(size=7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(3,10,0,10), "pt") )

posr
#ggsave("OSR.jpeg", dpi=300, height=6, width=6)

# OSR historic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create data.frame to save values
osr=list()
df1<-replist$natlen
uyear=unique(df1[,8])
uyear=uyear[-c(62:101)] # remove forecast years
l_m=length(uyear)

for(j in 1:l_m) { 
   
   # Numbers at length
   df<- df1 %>%  filter(Yr==uyear[j])
   
   ## Filter and select
   df<-df%>%filter(Era=="TIME",`Beg/Mid`=="B") %>% # eq year 2060
      select (-c(1,2,4,5,6,7,10,11,12))
   
   ## Columns to rows
   library(reshape2)
   df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))
   
   ## Recode factor levels with names
   library(dplyr)
   df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 
   
   ## Summarise across season and year
   library(plyr)
   df<-ddply(df, .(Yr, Sex, variable), summarize,  numbers=sum(value))
   
   ## Subset males and females
   df_fem<-subset(df, Sex=="female")
   df_male<-subset(df, Sex=="male")
   
   ## OSR
   numerador=sum(df_fem$numbers * matlen_f)
   denominador= (numerador + sum(df_male$numbers * matlen_m))
   sro=numerador/denominador
   osr[j]=sro
   
}

osr # OSR
osr_year<-cbind(osr,uyear)
osr_year<-as.data.frame(osr_year)
osr_year$uyear<-as.numeric(osr_year$uyear)
osr_year$osr<-as.numeric(osr_year$osr)
names(osr_year)<-c("osr","Year")
head(osr_year)

## Save
#write.csv(osr, "sex-sep osr by year table.csv")

# Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
p2<-ggplot(data = osr_year, aes(x = Year, y = osr)) +
   geom_line(size = 0.8, col = "black") +
   scale_y_continuous(breaks = seq(0.2, 0.45, by = 0.05)) +
   theme_light() +
   xlab("Year") +
   ylab("OSR") +
   coord_cartesian(ylim = c(0.2, 0.45)) +  # Set y-axis limits to ensure 0.2 is visible
   theme(legend.position = "none",
         legend.box.spacing = unit(0, "pt"),  # The spacing between the plotting area and the legend box (unit)
         legend.margin = margin(0, 0, 0, 0),
         legend.text = element_text(size = 7),
         legend.title = element_text(size = 7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         plot.margin = unit(c(3, 10, 0, 10), "pt"))


pl<-gridExtra::grid.arrange(posr,p2)



# arrange plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save
library(ggpubr)
g3<-ggarrange(sr,posr,p2, ncol=1, nrow=3, common.legend = FALSE, align = "hv",
              labels = c("A","B","C"),font.label=list(color="black",size=9))
g3+  bgcolor("white") 

# Save the plot
ggsave(paste0(dir_plot, "/07 Exploitation SR and OSR.jpeg"), plot = g3, height = 6, width = 4, dpi=800)

# 08 F SCENARIOS ---------------------------------------------------------------

# pop str ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create plot dir
dir_plot<-paste0(getwd(),"/Output")
dir.create(dir_plot)

## Data dir
dir_dat<-paste0(getwd(),"/../01 Models/Two sex/Model/M0 base model/forecast/")
dir.create(dir_dat)

## Read
run <- 'Fmult0' # F0

mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)


## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)

df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B", Yr == 2060) %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Sex, variable), summarize,  Number=mean(value))

df_F0<-df
df_F0$scen<-rep("F0")


# --- ## Read

run <- 'Fmult0.9'# FMsy = 0.186595, Fmult = 0.9

mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)

## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)
df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B", Yr == 2060) %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Sex, variable), summarize,  Number=mean(value))

df_FMSY<-df
df_FMSY$scen<-rep("FMSY")

# --- ## Read

run <- 'Fmult1.8'# 2 FMsy= 0.37319

mod_path <- paste0(dir_dat, "/", run, sep="") 
dir.create(mod_path) # check
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE)


## Numbers at length
df<-replist$natlen

## Filter and select
library(dplyr)
df<-df%>%filter(Era=="FORE",`Beg/Mid`=="B", Yr == 2060) %>%
   select (-c(1,2,4,5,6,7,10,11,12,13))

## Columns to rows
library(reshape2)
df<-melt(df, id.vars = c("Sex", "Yr", "Seas"))

## Recode factor levels with names
library(dplyr)
df$Sex = factor(df$Sex, levels=c("1","2"), labels=c("female","male")) 

## Summarise by fleet
library(plyr)
df<-ddply(df, .(Sex, variable), summarize,  Number=mean(value))

df_2FMSY<-df
df_2FMSY$scen<-rep("2FMSY")


df_full<-rbind(df_F0, df_FMSY, df_2FMSY)

# Plot biomasa at length
library(ggplot2)
ggplot(df_full, aes(x =as.numeric(variable), fill = Sex, 
                    y=Number))+
   geom_col(color="#2f4550", alpha=0.9) +
   scale_fill_manual(values = c("male" ="#ffad70",
                                "female"="#43428e"))+
   theme_light() +
   xlab("Length") + ylab("Population numbers (eq.)")+ facet_wrap(~scen)


# bins 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
df_full$variable<-as.numeric(as.character(df_full$variable))

prueba<-df_full %>% mutate(bins = cut(variable, breaks=seq(from=0, to=135, 5)))
head(prueba)

library(plyr)

prr<-ddply(prueba, .(Sex,scen,bins), numcolwise(sum))

prr$new = factor(prr$scen, levels=c("F0","FMSY","2FMSY"), labels=c("F0","FMSY","2FMSY")) 


prr$Length<-prr$bins   
levels(prr$Length) <- seq(5, 135,5)

prr$Lengthcut<-as.numeric(as.character(prr$Length))
prr<-prr%>%filter(Lengthcut<105)

library(ggplot2)
p11<-ggplot(prr, aes(x =(Lengthcut), fill = Sex, 
                     y=Number))+
   geom_vline(xintercept=60, linetype="dotted", size=0.6, col="black")+
   geom_col(color="#2f4550", alpha=0.9) +
   scale_fill_manual(values = c("male" ="#705cb5",
                                "female"="#edb9a1"))+
   theme_light() +
   theme(strip.background = element_rect(colour="black",
                                         fill="white"), 
         strip.text=element_text(color="black"))+
   xlab("Length bins (cm)") + ylab("Population numbers")+ facet_wrap(~new, dir="h")+
   theme(legend.position = "bottom",
         legend.box.spacing = unit(1, "pt"),# The spacing between the plotting area and the legend box (unit)
         legend.text=element_text(size=7),
         legend.title = element_text(size=7),
         axis.title.x = element_text(size = 7.5),
         axis.text.x = element_text(size = 6.5),
         axis.text.y = element_text(size = 6.5),
         axis.title.y = element_text(size = 7.5),
         strip.text.x = element_text(size = 8),
         legend.key.size = unit(0.3, 'cm'))+
   scale_x_continuous(breaks=seq(0,100,10),
                      labels=seq(0,100,10))
p11
ggsave(paste0(dir_plot,"/08 Exploitation scenarios.jpeg"), width=6, height=2.6, dpi=800)
