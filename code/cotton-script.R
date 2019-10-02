########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cot <- read_csv("data/cotton-usda-nass.csv")
str(cot)
head(cot) 
tail(cot)
dim(cot)
summary(cot)


# 3.1. Create a NC data subset ----
nc_cot <- cot %>%
  dplyr::select(year, state, ag_district, county, data_item, value)


# 3.2. Divide the data_item column ----
nc_cot %>%
  separate(data_item,
           into = c("cotton_type", "measurement"),
           sep = "-") -> nc_cot

# 3.3. Convert the value column to numeric type ----

nc_cot <- nc_cot %>%
  filter(value != "(D)")

as.numeric(nc_cot$value) 
nc_cot$value <- as.numeric(nc_cot$value)

head(nc_cot)
# 4. Visualizing trends ----

nc_cot %>% 
  ggplot(mapping = aes(x=year, y=value)) +
  geom_point() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(rows = vars(measurement),
             cols = vars(ag_district),
             scales = "free_y") +
  labs(title = "Cotton Production in NC", 
       x = "Year", y ="", 
       caption = "Source: USDA NASS") 

# 5. Summarize data from 2018 ----
nc_cot5 <- nc_cot %>%
  filter(year == "2018") %>% 
  spread(measurement, value)

names(nc_cot5)[6] <- "acres"
names(nc_cot5)[7] <- "yield"

nc_cot5 %>%
  mutate(total = acres * yield) %>%
  top_n(3)%>%
  dplyr::select(county, total)


