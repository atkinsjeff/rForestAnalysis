library(ggplot2)
library(dlstats)
require(tidyverse)
require(ggrepel)
# displays as you require
require(scales)
# import cran stats
x <- cran_stats(c("forestinventory",
                  "ForestGapR",
                  "rLiDAR",
                  "leafR",
                  "lidR",
                  "rGEDI",
                  "rFIA",
                  "lmfor",
                  "forestr",
                  "ForIT",
                  "BIEN",
                  "forestChange",
                  "rForest",
                  "HMB",
                  "forestmangr",
                  "ForestTools",
                  "FI",
                  "forestSAS",
                  "ForestFit",
                  "measuRing",
                  "detrendeR",
                  "burnr",
                  "TRADER",
                  "treeclim",
                  "pointRes",
                  "dendroTools",
                  "BIOdry",
                  "dendrometeR",
                  "tracheideR",
                  "RAPTOR",
                  "DendroSync",
                  "fgeo",
                  "BIOMASS",
                  "Fgmutils",
                  "DGVM3D",
                  "FAwR",
                  "fdq",
                  "forestHES",
                  "gfcanalysis",
                  "medfate",
                  "r3PG",
                  "ShapeSelectForest",
                  "TreeLS",
                  "Sky",
                  "chillR",
                  "phenesse",
                  "phenex",
                  "phenmod",
                  "pheno",
                  "phenocamapi",
                  "phenocamr",
                  "phenoCDM",
                  "phenofit",
                  "phenopix",
                  "phenomap",
                  "hsdar",
                  "hyperspec"
))



x$days_on_CRAN <- x$end - x$start
x$days_on_CRAN <- as.integer(x$days_on_CRAN)
x %>%
  group_by(package) %>%
  summarize(total.downloads = sum(downloads, na.rm = TRUE),
            days.on.cran = sum(days_on_CRAN, na.rm = TRUE)) -> x.sums

x11(width = 8, height = 5)
ggplot(x.sums, aes(x = days.on.cran, y = total.downloads, label = package))+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_minimal()+
  geom_label_repel(min.segment.length = unit(0, 'lines'),
                   nudge_y = 6, max.overlaps = 30)+
  scale_y_continuous(labels = comma)+
  ylab("Total Downloads")+
  xlab("Days on CRAN")

# remove phenopix
x.sums %>%
  filter(package != "phenopix" & package != "lidR") -> x.sums2

x11(width = 12, height = 8)
ggplot(x.sums2, aes(x = days.on.cran, y = total.downloads, label = package))+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_minimal()+
  geom_label_repel(min.segment.length = unit(0, 'lines'),
                   nudge_y = 6, max.overlaps = 30)+
  scale_y_continuous(labels = comma)+
  ylab("Total Downloads")+
  xlab("Days on CRAN")

#write.csv(x.sums, "./data/package_downloads.csv")


if (!is.null(x)) {
  head(x)
  ggplot(x, aes(end, downloads, group=package, color=package)) +
    geom_line() + geom_point(aes(shape=package))
}