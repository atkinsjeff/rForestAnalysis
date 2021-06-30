# 
require(ggplot2)
require(tidyverse)
require(data.table)



# importing journal title
filenames <- list.files(path = "./data", pattern = "_journal_", full.names = TRUE) 
l <- lapply(filenames, fread, select = c("Source Titles", "records"), sep = "\t", nrows = 40)

names(l) <- basename(filenames)
#bind the rows from the list togetgher, putting the filenames into the colum "id"
df.journals <- rbindlist( l, idcol = "id" )

df.journals$year <- as.numeric(substr(df.journals$id, 0, 4))

# importing research areas
filenames <- list.files(path = "./data", pattern = "research", full.names = TRUE) 
l <- lapply(filenames, fread, select = c("Research Areas", "records"), sep = "\t")

names(l) <- basename(filenames)
#bind the rows from the list togetgher, putting the filenames into the colum "id"
df.research <- rbindlist( l, idcol = "id" )

df.research$year <-  as.integer(substr(df.research$id, 0, 4))


df.research  %>%
  filter(`Research Areas` %in% c("ENVIRONMENTAL SCIENCES ECOLOGY", "FORESTRY", "REMOTE SENSING", "EVOLUTIONARY BIOLOGY",
                                 "WATER RESOURCES", "BIODIVERSITY CONSERVATION", "PLANT SCIENCES")) %>%
  data.frame() -> x

x11()
ggplot(x, aes(x = year, y = records, color = Research.Areas)) +
  geom_line()



#### bring in all area data
# importing research areas
filenames <- list.files(path = "./data", pattern = "all", full.names = TRUE) 
l <- lapply(filenames, fread, select = c("Research Areas", "records"), sep = "\t", nrows = 300)

names(l) <- basename(filenames)
#bind the rows from the list togetgher, putting the filenames into the colum "id"
df.all <- rbindlist( l, idcol = "id" )

df.all$year <-  as.integer(substr(df.all$id, 0, 4))


df.all  %>%
  filter(`Research Areas` %in% c("ENVIRONMENTAL SCIENCES ECOLOGY", "FORESTRY", "REMOTE SENSING", "EVOLUTIONARY BIOLOGY",
                                 "WATER RESOURCES", "BIODIVERSITY CONSERVATION", "PLANT SCIENCES")) %>%
  data.frame() -> y

x$id <- NULL
y$id <- NULL
x$Research.Areas <- as.factor(x$Research.Areas)
y$Research.Areas <- as.factor(y$Research.Areas)

# now to change names
setnames(y, "records", "all.records")

z <- merge(x, y)

z$ratio <- z$records / z$all.records

scale_color_manual(   
  values = c(
    "ENVIRONMENTAL SCIENCES ECOLOGY" = "#e41a1c",
   "FORESTRY" ="#377eb8",
    "REMOTE SENSING" = "#4daf4a",
   "EVOLUTIONARY BIOLOGY" = "#984ea3",
    "WATER RESOURCES" ="#ff7f00",
   "BIODIVERSITY CONSERVATION" = "#ffff33", 
   "PLANT SCIENCES" = "#a65628"))

z %>%
  filter(year != 2020) -> zz


#
x11(width = 8, height = 3)
ggplot(zz, aes(x = as.integer(year), y = (ratio * 100), color = Research.Areas)) +
  geom_line(size = 2, alpha = 0.6)+
  geom_point(size = 3, shape = 21, fill = "white")+
  scale_color_manual(   
    values = c(
      "ENVIRONMENTAL SCIENCES ECOLOGY" = "#e41a1c",
      "FORESTRY" ="#377eb8",
      "REMOTE SENSING" = "#4daf4a",
      "EVOLUTIONARY BIOLOGY" = "#984ea3",
      "WATER RESOURCES" ="#ff7f00",
      "BIODIVERSITY CONSERVATION" = "dark grey", 
      "PLANT SCIENCES" = "#a65628"))+
  theme_minimal()+
  xlab("")+
  ylab(expression(atop("Literature Citing", paste("the R Programming Language (%)"))))+
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1, big.mark = ""))
  

z %>%
  filter(Research.Areas == "FORESTRY") -> df.for

df.for$ratio.all <- (1 - df.for$ratio)

df.for %>%
  select(year, ratio, ratio.all) -> df.for2



df.for3 <- gather(data = df.for2, key = component, value = percent, -year)

#df.for3$component <- rev(levels(df.for3$component))
df.for3$component <- sort(df.for3$component, decreasing = TRUE)

x11() 
ggplot(df.for3, aes(x = as.numeric(year), y = percent, fill = component))+
         geom_area(alpha = 0.6, size = 1, color = "black")
  
  
  
z %>%
  filter(Research.Areas == "ENVIRONMENTAL SCIENCES ECOLOGY") -> df.eco

df.eco$ratio.all <- (1 - df.eco$ratio)

df.eco %>%
  select(year, ratio, ratio.all) -> df.eco2



df.eco3 <- gather(data = df.eco2, key = component, value = percent, -year)

#df.eco3$component <- rev(levels(df.eco3$component))
df.eco3$component <- sort(df.eco3$component, decreasing = TRUE)

x11() 
ggplot(df.eco3, aes(x = as.numeric(year), y = percent, fill = component))+
  geom_area(alpha = 0.6, size = 1, color = "black")
  
  
  
  
  



















