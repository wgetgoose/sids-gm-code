library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(readxl)

# import wb dataset
sids_sectors <- read_excel("sids_sectors.xlsx")

#computing change in vars
sids_var_names <- c("y1990", "y1991", "y1992", "y1993", "y1994", "y1995", "y1996", 
                    "y1997", "y1998", "y1999", "y2000", "y2001", "y2002", 
                    "y2003", "y2004", "y2005", "y2006", "y2007", "y2008", "y2009",       
                    "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016", "y2017",
                    "y2018", "y2019", "y2020", "y2021", "y2022", "y2023")
indices <- c(1:length(sids_var_names))
output <- sids_sectors
for (i in 2:length(sids_var_names)) {  
  output <- output %>%
    mutate(
      !!paste0("change_", sids_var_names[i]) := !!sym(sids_var_names[i]) - !!sym(sids_var_names[i - 1])
    )
}
#tidying output
output <- output %>% 
  select(-c("y1990", "y1991", "y1992", "y1993", "y1994", "y1995", "y1996", 
            "y1997", "y1998", "y1999", "y2000", "y2001", "y2002", 
            "y2003", "y2004", "y2005", "y2006", "y2007", "y2008", "y2009",       
            "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016", "y2017",
            "y2018", "y2019", "y2020", "y2021", "y2022", "y2023"))
#create year col
net_long <- output %>%
  pivot_longer(
    cols = starts_with("change_y"),
    names_to = "year",
    names_prefix = "change_y",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year))
#breakout series into discrete columns
net_wide <- net_long %>%
  pivot_wider(
    id_cols = c(`Country Name`, `Country Code`, year),
    names_from = `Series Name`,
    values_from = value
  )
#saving
write_csv(net_wide, "net_sids_tidy.csv")
#percent sector contribution to growth
error <- net_wide %>% 
  select(-carbon_intensity) %>% 
  mutate(net_exports = exports-imports) %>% 
  mutate(sum = consumption + government + investment + net_exports) %>% 
  mutate(gdp_error = gdp-sum)
ggplot(data = error, mapping = aes(x = gdp_error))+
  geom_histogram()

final <- net_wide %>% 
  select(-carbon_intensity) %>% 
  mutate(exports = exports/gdp) %>% 
  mutate(imports = imports/gdp) %>% 
  mutate(consumption = consumption/gdp) %>% 
  mutate(government = government/gdp) %>% 
  mutate(investment = investment/gdp) %>% 
  mutate(gdp = gdp/gdp) %>% 
  mutate(net_exports = exports-imports) %>%
  select(-c("exports", "imports"))

ggplot(data = final, mapping = aes(x = net_exports))+
  geom_histogram()
write_csv(final, "sids_gm_attempt1.csv")
  