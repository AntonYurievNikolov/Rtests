# Definition of split_low
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)
names <- lapply(split_low, function(x) {x[1]})
years <- lapply(split_low, function(x){x[2]})

#regexs
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org",
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")
grepl("@.*\\.edu$",emails)
hits<-grep("@.*\\.edu$",emails)
emails[hits]

#TYDIVERSE intro
# Filter for observations in the Oceania continent in 1952
oceania_1952<-gapminder%>%filter(year==1952,continent =='Oceania')
# Create a bar plot showing medianGdp by continent
ggplot(oceania_1952,aes(x=country,y=gdpPercap))+geom_col()


# Create a histogram of population (pop)

##CHANGE WITH THIS
#useful read packages

library(readr)

# Import potatoes.csv with read_csv(): potatoes
potatoes<-read_csv("potatoes.csv")

# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")

# Import 5 observations from potatoes.txt: potatoes_fragment
potatoes_fragment <- read_tsv("potatoes.txt", skip = 6, n_max = 5, col_names = properties)

# collectors

# The collectors you will need to import the data
fac <- col_factor(levels = c("Beef", "Meat", "Poultry"))
int <- col_integer()
hotdogs_factor <- read_tsv("hotdogs.txt",
                           col_names = c("type", "calories", "sodium"),
                           col_types = list(fac,int,int))

#readxl if you want to read excel
pop_list <- lapply(excel_sheets("urbanpop.xlsx"),
                   read_excel,
                   path = "urbanpop.xlsx")
cols <- c("country", paste0("year_", 1960:1966))
pop_b<-read_excel("urbanpop_nonames.xlsx",sheet=1,col_names=cols)

##!!!! CLEAN ALL
urban_clean<-na.omit(urban)


#lubrydate  
month_seq %m-%1:12*months(1)
#checkintervals
halleys <- halleys %>% 
  mutate(visible = start_date %--% end_date)
halleys_1066 <- halleys[14, ] 

monarchs %>% 
  filter(halleys_1066$perihelion_date %within% reign) %>%
  select(name, from, to, dominion)
monarchs %>% 
  filter(int_overlaps(halleys_1066$visible,reign) ) %>%
  select(name, from, to, dominion)

