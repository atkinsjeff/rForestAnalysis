# importing journal title
filenames <- list.files(path = "./data", pattern = "_journal_", full.names = TRUE) 
l <- lapply(filenames, fread, select = c("Source Titles", "records"), sep = "\t", nrows = 50)

names(l) <- basename(filenames)
#bind the rows from the list togetgher, putting the filenames into the colum "id"
df.journals <- rbindlist( l, idcol = "id" )

journal.list <- data.frame(unique(df.journals$`Source Titles`))
df.journals$year <- as.numeric(substr(df.journals$id, 0, 4))

#write.csv(journal.list, "./data/journal_list.csv")

# brings in the top 20 journals per Google Metrics
classes <- read.csv("./data/journal_classes.csv")

# moves to upper case
classes$journal_title <- as.factor(toupper(classes$journal_title))

names(df.journals)[2] <- "journal_title"

# reformat for merging
df.journals$id <- NULL
df.journals$journal_title <- as.factor(df.journals$journal_title)

df <- merge(df.journals, classes)

df %>%
  group_by(class, year) %>%
  summarize(sum.recs = sum(records)) -> df.sums
x11()
ggplot(df.sums, aes(x = year, y = sum.recs, color = class))+
  geom_line(size = 2)


# [37] "total_ecology_pubs_by_year.txt"         
# [38] "total_ecology_pubs_for2010_to_2019.txt" 
# [39] "total_forestry_pubs_by_year.txt"        
# [40] "total_forestry_pubs_for2010_to_2019.txt"

# total ecology pubs
total.eco <- read_delim("./data/total_ecology_pubs_by_year.txt", delim = "\t")
names(total.eco)[1] <- "year"
total.eco <- total.eco[,c(1,2)]
total.eco <- total.eco[-12,]
total.eco$class = as.factor("ecology")

# total forestry pubs
total.for <- read_delim("./data/total_forestry_pubs_by_year.txt", delim = "\t")
names(total.for)[1] <- "year"
total.for <- total.for[,c(1,2)]
total.for <- total.for[-12,]
total.for$class = as.factor("forestry")

total.for

totals <- rbind(total.eco, total.for)

big.boi <- merge(df.sums, totals)

big.boi$propR <- big.boi$sum.recs / big.boi$records


x11(width = 6, height = 6)
ggplot(big.boi, aes(x = year, y = (propR * 100), color = class))+
  geom_line(size = 2, alpha = 0.6)+
  geom_point(size = 3, shape = 21, fill = "white")+
  theme_minimal()+
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1, big.mark = ""))+
  scale_color_manual( 
    name="Journal Class",
    values = c(
      "ecology" = "#e41a1c",
      "forestry" ="#377eb8"))+
  theme(legend.position=c(0.02,1), legend.justification=c(0.02,1))+
  xlab("")+
  ylab(expression(atop("Percentage of papers citing R ", paste("in the top 20 journals of each field"))))
  


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


