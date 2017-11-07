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
#this works
# send_message(mime(from="simoncoulombe@gmail.com", to="simoncoulombe@gmail.com",
#                   subject="hello", "how are you doing?"))


my_messages = messages("from=noreply@samweb.com", num_results=1)
my_message = message(id = my_messages[[1]]$messages[[1]]$id, format="full")

maisons$adresse

z <- body(my_message)  %>%  str_extract_all("UNI.+\\(vente\\)")
urls <- body(my_message)  %>%  str_extract_all('https://ng.samweb.com.+?"')
urls <- urls[[1]] %>% str_replace('"', "") %>% str_replace("=+$", "")
adresse <- str_replace(z[[1]], ".+?-\ ", "")  %>% #? pour rendre le + lazy au lieu de greedy
  str_replace("-[^-]*$", "")   %>% ## un -, suivi du plus de caractère possibles sauf un autre -  , bref, le dernier - et tout 
  str_replace("Sainte-Foy/Sillery/Cap-Rouge", "") %>% 
  str_replace("Saint-Louis", "") %>% 
  str_replace("Sillery", "") %>% 
  str_replace("Ã©", "é") %>% ## ça ça ne marche pas trop.. j'ai lancé une issue pour l'encodage
  str_replace("Ã", "à") %>%
  str_replace("Ã´", "ô")  %>%
  str_replace("Les Chutes-de-la-Chaudià¨re-Est", "")  %>%
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

my_map <- map_data %>% leaflet() %>% 
  addCircles(label= ~ paste0(prix, " - " ,adresse),
             #popup= '<a href = "https://rstudio.github.io/leaflet/"> R </a>') %>%
              #popup= ~paste0('<a href = "https://rstudio.github.io/leaflet/"> R </a>')) %>%
             #popup= ~paste0('<a href = ',"https://rstudio.github.io/leaflet/",'> R </a>')) %>%
             popup= ~paste0('<a href = ',urls,'> Inscription </a>')) %>%
  addProviderTiles(providers$Esri.WorldStreetMap)

my_map
saveWidget(my_map, file="emailSAM.html")
