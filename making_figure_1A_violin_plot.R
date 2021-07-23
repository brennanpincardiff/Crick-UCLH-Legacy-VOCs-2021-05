# making just Figure 1B - a violin plot

library(sp)
library(tidyverse)
library(khroma)

# download the data from my fork on Github
github_link <- "https://github.com/brennanpincardiff/Crick-UCLH-Legacy-VOCs-2021-05/blob/main/Crick_Legacy_2021-24-05_B-1-617-2_PUBLIC.Rda?raw=true"
load(url(github_link))

### Lines 21 to 261 of original script

### Subset data for further analysis
studyData <- dtHashed %>% 
    filter(COVID_vaccStatus %in% c(1,2)) # Ignore individuals who are in seroconversion window following dose 1 and dose 2 of vaccine

### Set constants for various functions / plots
strainOrder <- c("Wuhan1", "D614G", "Kent", "SAfrica", "India2")
referenceIC50 <- 2^9.802672

dose2cohort <- studyData %>% filter(COVID_vaccStatus == 2,  sampleOrderInVaccStatus == 1)

########################################################################
#   Panel 1. Vaccine responses per strain following 2nd dose Pfizer    #
########################################################################

relevantData <- dose2cohort %>%
    pivot_longer(cols = ends_with("ic50"), names_to = "strain", values_to = "ic50")
relevantData$strain <- str_replace_all(relevantData$strain, pattern = "_ic50", replacement = "")
relevantData$strain <- fct_relevel(relevantData$strain, strainOrder)


outplot <- ggplot(relevantData, aes(x=strain, y=ic50, color = strain, label = sample_barcode)) + 
    scale_colour_muted() +
    geom_hline(yintercept = referenceIC50,linetype = 3 ) +
    geom_violin(trim=TRUE) + 
    scale_y_continuous(trans='log2', 
                       breaks=c(5, 10, 64, 256, 1024, 5120), 
                       labels=c("[0]", "[<40]", "64", "256", "1024", "[>2560]"),
                       minor_breaks=c(32, 128, 512, 2048)) +
    ylab(bquote('Virus Neutralisation, '~IC[50]~ '')) +
    geom_jitter(shape=20, position=position_jitter(0.2), alpha=0.3) + 
    stat_summary(fun=median, geom = "point", color="black",  shape=5, size=1, stroke=1) + 
    theme_bw(base_family = "Helvetica Neue Thin") +
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12)) + 
    theme(axis.text.y = element_text(size=12))  + 
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(size=15),
        axis.title.x=element_blank()
    )

outplot

# Panel1 <- outplot

# ggsave("FIGURE-Panel1.svg", outplot, width=50, height=15, units="cm")
