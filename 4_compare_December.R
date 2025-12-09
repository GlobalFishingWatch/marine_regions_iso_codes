library(dplyr)
library(sf)
library(readxl)
devtools::load_all("../../R/gfwr/")

#api return
gfwr <- gfw_regions() #changes were implemented
names(gfwr)
nrow(gfwr)
# read current return
apis <- jsonify::from_json("eez_response.json")
names(apis)
nrow(apis)
dplyr::left_join(gfwr, apis) |> write.csv("comparison_Dec.csv")
