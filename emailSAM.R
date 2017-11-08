library(gmailr)
library(purrr)
library(dplyr)
library(magrittr)
library(stringr)
library(ggmap)
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
register_google(key = Sys.getenv("googlemap_api_key"))
#https://stackoverflow.com/questions/33068063/encoding-and-raw-in-r
#https://stackoverflow.com/questions/27690036/losing-the-%C4%8C-%C4%8D-characters-utf-encoding-within-r
#https://stackoverflow.com/questions/6254744/override-a-function-that-is-imported-in-a-namespace
  my_base64url_decode_to_char <- function(x) { iconv(rawToChar(base64enc::base64decode(gsub("_", "/", gsub("-", "+", x)))), from="UTF-8", to="latin1") }
# # 
 getAnywhere("base64url_decode_to_char")
 unlockBinding("base64url_decode_to_char", getNamespace("gmailr"))
 assign("base64url_decode_to_char", my_base64url_decode_to_char, getNamespace("gmailr"))

  

#this works
# send_message(mime(from="simoncoulombe@gmail.com", to="simoncoulombe@gmail.com",
#                   subject="hello", "how are you doing?"))


my_messages = messages("from=noreply@samweb.com", num_results=1)

my_message = message(id = my_messages[[1]]$messages[[1]]$id, format="full")
body(my_message)

z <- body(my_message)  %>%  str_extract_all("UNI.+\\(vente\\)")
urls <- body(my_message)  %>%  str_extract_all('https://ng.samweb.com.+?"')
urls <- urls[[1]] %>% str_replace('"', "") %>% str_replace("=+$", "")
adresse <- str_replace(z[[1]], ".+?-\ ", "")  %>% #? pour rendre le + lazy au lieu de greedy
  str_replace("-[^-]*$", "")   %>% ## un -, suivi du plus de caractère possibles sauf un autre -  , bref, le dernier - et tout 
  str_replace("Sainte-Foy/Sillery/Cap-Rouge", "") %>% 
  str_replace("Saint-Louis", "") %>% 
  str_replace("Sillery", "") %>% 
  str_replace("Les Chutes-de-la-Chaudière-Est", "")  %>%
  str_replace("Saint-Romuald", "")  
  
  
  
adresse2 <- adresse %>% str_replace(",[^,]*$", "")
maisons <- as.tibble(adresse) %>% rename(adresse= value)

prix <-str_extract(z[[1]], "-[^-]*\\$") %>% str_replace_all("[-]", "") # enlever les -, les espace "\\ " et les signes de dollars\\$
prix



maisons$prix <- prix
maisons$adresse2 <- adresse2
maisons$urls <- urls
geo <- ggmap::geocode(maisons$adresse2)


geo
maisons$lat <- geo$lat
maisons$lon <- geo$lon


map_data <-  st_as_sf(maisons %>% arrange(prix) %>% 
                        filter(!is.na(lat), lon>-71.5, lon < 71), 
                      coords = c("lon", "lat"), 
                      crs = 4326, agr = "constant") 

map_data <- map_data %>% mutate(label = paste0(prix, " - " ,adresse))

my_map <- map_data %>% leaflet() %>% 
  addCircles(label= ~ label,
             #popup= '<a href = "https://rstudio.github.io/leaflet/"> R </a>') %>%
              #popup= ~paste0('<a href = "https://rstudio.github.io/leaflet/"> R </a>')) %>%
             #popup= ~paste0('<a href = ',"https://rstudio.github.io/leaflet/",'> R </a>')) %>%
             popup= ~paste0('<a href = ',urls,'> Inscription </a>')) %>%
  addProviderTiles(providers$Esri.WorldStreetMap)

my_map
saveWidget(my_map, file="emailSAM.html")

