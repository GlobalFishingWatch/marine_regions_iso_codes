#Check the comparison table follow UN convention guidelines

#############
library(dplyr)
# to know who is landlocked: tidycountries
library(tidycountries)
tc <- restcountries_tidy_data %>%
  select(-suffixes, -calling_code) %>% distinct()
count(tc, landlocked)
land <- restcountries_tidy_data %>%
  select(common_name, official_name, cca3, landlocked) %>%
  distinct() %>%
  rename(iso = cca3)

# the table I sent to the team
iso <- readr::read_csv("comparison_table.csv")
iso <- iso %>%
  select(MRGID, name, iso, POL_TYPE, SOVEREIGN1) %>%
  left_join(land) %>%
  arrange(iso)
count(iso, landlocked)
#un code
un <- readr::read_csv("Standard Naming and ISO - EN Naming.csv", n_max = 248)

### all in tidycountries?
# how many names in the iso table are in the un table
iso %>% mutate(in_un = name %in% un$Name) %>%
  mutate(special_case = name != SOVEREIGN1) %>%
  filter(in_un == FALSE)
#ok not useful

#let's check the actual name by ISO
un_comparison <- un %>% select(Name_un = Name, `ISO 3166`) %>%
  rename(iso = `ISO 3166`) %>%
  left_join(land) %>%
  full_join(iso) %>% arrange(iso)

# let's check the major differences:
un_comparison %>%
filter(Name_un != name) %>%
  filter(Name_un != SOVEREIGN1) %>%
  select(Name_un, iso, MRGID, name_marine_regions = name) %>%
  View()
  readr::write_csv("UN_major_differences.csv")

#todo bien con los landlocked
  un_comparison %>% filter(landlocked == TRUE) %>% View()

table(un$Name %in% tc$common_name)
table(un$Name %in% tc$official_name)
un %>% filter(!Name %in% tc$common_name) %>% View()
### #####

all <- un %>% select(Name, `ISO 3166`, Type, `Administering Power`, `Disputed by`) %>%
  full_join(api, by = join_by(`ISO 3166` == iso3))
View(all)
count(all,`ISO 3166` == iso3)
filter(all, is.na(`ISO 3166`)) %>% View()
all %>% arrange(iso3) %>%  View()



un %>% full_join(eeza, by = join_by(Name == TERRITORY1)) %>% View()
