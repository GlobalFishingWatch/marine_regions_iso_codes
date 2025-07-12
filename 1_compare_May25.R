library(dplyr)
library(sf)
library(readxl)
devtools::load_all("../../R/gfwr/")

#api return
api <- get_regions()


# raw mr data
eeza <- sf::read_sf("~/Downloads/World_EEZ_v12_20231025_gpkg/eez_v12.gpkg")
names(eeza)
mr <- eeza %>% sf::st_drop_geometry()

###islas y territorios indiscutiblemente de alguien 18
mr %>% filter(is.na(ISO_TER1) & POL_TYPE == "200NM")

###islas y territorios en Joint regime #21
mr %>% filter(POL_TYPE == "Joint regime")

###islas y territorios en Joint regime #35
mr %>% filter(POL_TYPE == "Overlapping claim")

mr_name <- mr %>% mutate(name = case_when(
  # Joint regimes are named joint regimes
  POL_TYPE == "Joint regime" ~ GEONAME,
  # Overlapping claims that are named keep their name
  POL_TYPE %in% c("Overlapping claim") & stringr::str_detect(string = mr$GEONAME, "claim ") ~ TERRITORY1,
  # Overlapping claims that are not named are named overlapping claims
  POL_TYPE %in% c("Overlapping claim") & stringr::str_detect(string = mr$GEONAME, "claim:") ~ GEONAME,
  # Areas belonging to the EEZ of a main country keep their name
  POL_TYPE == "200NM" ~ TERRITORY1)) %>% relocate(name)
#any(is.na(mr_name$name))
library(dplyr)
mr %>% filter(ISO_TER1 == "AIA")
#Anguilla AIA - GBR - "Anguilla" #---> la version vieja

mr_name_iso <- mr_name %>% mutate(iso = case_when(
  # Joint regimes are named joint regimes, they have no ISO
  POL_TYPE == "Joint regime" ~ NA,
  # Overlapping claims that are named keep their name and their ISO when available
  POL_TYPE %in% c("Overlapping claim") & stringr::str_detect(string = mr_name$GEONAME, "claim ") ~ ISO_TER1,
  # Overlapping claims that are not named are named overlapping claims and have NO ISO
  POL_TYPE %in% c("Overlapping claim") & stringr::str_detect(string = mr_name$GEONAME, "claim:") ~ NA,
  # Areas belonging to the EEZ of a main country keep their name and the country's ISO
  POL_TYPE == "200NM" & !is.na(ISO_TER1) ~ ISO_TER1,
  # Areas belonging to the EEZ of a main country but with no ISO get their mainland country's ISO
  POL_TYPE == "200NM" & is.na(ISO_TER1) ~ ISO_SOV1)) %>%
  relocate(iso)
  #filter(is.na(iso)) %>%
mr_name_iso %>% filter(POL_TYPE == "Overlapping claim")


### Join with the API return

comparison_table <- mr_name_iso %>%
  left_join(api, by = join_by(MRGID == id)) %>%
  relocate(MRGID, name, iso, iso3) %>%
  arrange(POL_TYPE, iso)

readr::write_csv(comparison_table, "comparison_table.csv")


