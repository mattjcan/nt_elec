# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(knitr)
library(xaringan)
library(rmarkdown)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(scales)
library(easynls)
library(gridExtra)
library(readxl)
library(survey)
library(vcd)
library(XML)
library(xml2)
library(sp)
library(rgdal)
library(leaflet)
library(mapview)
library(widgetframe)
library(rgeos)
library(kableExtra)
# library(raster)
library(sf)
library(janitor)
library(rvest)
library(kableExtra)
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)



# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

stroke_size <- 0.75

line_color <- "#2166ac"

ggmap::register_google(key = "AIzaSyBxhPE89Dhp4dxBHcShF7H3HSetJZHrD3M")

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Documents/git/nt_elec/" # parent directory for the data

nt_elec <- readOGR(paste0(d,"data/SED_2016_AUST.shx"))

pp16 <- read_csv(paste0(d,"data/pp16.csv"), skip = 0)

pp16 <- pp16 %>% 
  select(pp, lat, lon)

# TPP by division 

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/nt-summary-of-two-candidate-preferred-votes-by-division"

tpp <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

tpp <- as_tibble(tpp[[1]])

tpp <- tpp[1:25,1:4]

tpp <- tpp %>% 
  gather(key = "party", value = "v", -Electorate) %>% 
  rename(div = Electorate) %>% 
  filter(!is.na(v)) %>% 
  arrange(div)

tpp  <- tpp %>% 
  group_by(div) %>% 
  mutate(p = v / sum(v) * 100)

tpp_w <- tpp %>% 
  filter(p > 50)

tpp_w$m <- ifelse(tpp_w$party == "CLP", tpp_w$p - 50, -1 *(tpp_w$p - 50))

tpp_w <- tpp_w %>% 
  arrange(-m)

tpp_w_t <- tpp_w %>% 
  ungroup() %>% 
  select(div, party, m) %>% 
  mutate(m = round(m,2)) 

names(tpp_w_t) <- c("Electorate", "Party", "Margin (%)")

t_pend <- tpp_w_t %>% 
  kable("html", escape = F, booktabs = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(which(tpp_w_t$`Margin (%)` < 0 & tpp_w_t$`Margin (%)` > -8.05), bold = T, color = "white", background = "#ff5e5e") 



# polling booths

# arafura

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/arafura"

pb_arafura <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_arafura <- as_tibble(pb_arafura[[3]])

pb_arafura <- pb_arafura[1:8, ] 

names(pb_arafura)[1] <- "pp"

pb_arafura <- pb_arafura %>% 
  gather(key = "cand", value = "v", -pp)

pb_arafura$v <- as.numeric(pb_arafura$v)

pb_arafura <- pb_arafura %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_arafura$div <- "Arafura"

# barkly

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/barkly"

pb_barkly <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_barkly <- as_tibble(pb_barkly[[3]])

pb_barkly <- pb_barkly[1:10, ] 

names(pb_barkly)[1] <- "pp"

pb_barkly <- pb_barkly %>% 
  gather(key = "cand", value = "v", -pp)

pb_barkly$v <- as.numeric(pb_barkly$v)

pb_barkly <- pb_barkly %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_barkly$div <- "Barkly"

# braitling

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/braitling"

pb_braitling <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_braitling <- as_tibble(pb_braitling[[3]])

pb_braitling <- pb_braitling[1:9, ] 

names(pb_braitling)[1] <- "pp"

pb_braitling <- pb_braitling %>% 
  gather(key = "cand", value = "v", -pp)

pb_braitling$v <- as.numeric(pb_braitling$v)

pb_braitling <- pb_braitling %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_braitling$div <- "Braitling"

# brennan

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/brennan"

pb_brennan <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_brennan <- as_tibble(pb_brennan[[3]])

pb_brennan <- pb_brennan[1:8, ] 

names(pb_brennan)[1] <- "pp"

pb_brennan <- pb_brennan %>% 
  gather(key = "cand", value = "v", -pp)

pb_brennan$v <- as.numeric(pb_brennan$v)

pb_brennan <- pb_brennan %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_brennan$div <- "Brennan"

# drysdale

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/drysdale"

pb_drysdale <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_drysdale <- as_tibble(pb_drysdale[[3]])

pb_drysdale <- pb_drysdale[1:10, ] 

names(pb_drysdale)[1] <- "pp"

pb_drysdale <- pb_drysdale %>% 
  gather(key = "cand", value = "v", -pp)

pb_drysdale$v <- as.numeric(pb_drysdale$v)

pb_drysdale <- pb_drysdale %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_drysdale$div <- "Drysdale"

# fong lim

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/fong-lim"

pb_fonglim <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_fonglim <- as_tibble(pb_fonglim[[3]])

pb_fonglim <- pb_fonglim[1:9, ] 

names(pb_fonglim)[1] <- "pp"

pb_fonglim <- pb_fonglim %>% 
  gather(key = "cand", value = "v", -pp)

pb_fonglim$v <- as.numeric(pb_fonglim$v)

pb_fonglim <- pb_fonglim %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_fonglim$div <- "Fong Lim"

# katherine

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/katherine"

pb_katherine <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_katherine <- as_tibble(pb_katherine[[3]])

pb_katherine <- pb_katherine[1:9, ] 

names(pb_katherine)[1] <- "pp"

pb_katherine <- pb_katherine %>% 
  gather(key = "cand", value = "v", -pp)

pb_katherine$v <- as.numeric(pb_katherine$v)

pb_katherine <- pb_katherine %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_katherine$div <- "Katherine"

# karama

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/karama"

pb_karama <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_karama <- as_tibble(pb_karama[[3]])

pb_karama <- pb_karama[1:9, ] 

names(pb_karama)[1] <- "pp"

pb_karama <- pb_karama %>% 
  gather(key = "cand", value = "v", -pp)

pb_karama$v <- as.numeric(pb_karama$v)

pb_karama <- pb_karama %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_karama$div <- "Karama"

# port darwin

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/port-darwin"

pb_portdarwin <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

pb_portdarwin <- as_tibble(pb_portdarwin[[3]])

pb_portdarwin <- pb_portdarwin[1:9, ] 

names(pb_portdarwin)[1] <- "pp"

pb_portdarwin <- pb_portdarwin %>% 
  gather(key = "cand", value = "v", -pp)

pb_portdarwin$v <- as.numeric(pb_portdarwin$v)

pb_portdarwin <- pb_portdarwin %>% 
  group_by(pp) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_portdarwin$div <- "Port Darwin"

# combine all pbs 

pb_exp <- bind_rows(pb_arafura, pb_barkly, pb_brennan, pb_braitling, pb_drysdale, pb_fonglim, pb_katherine, pb_karama, pb_portdarwin) 

pb_exp <- pb_exp %>% 
  filter(!(pp %in% c("Absent", "Declaration", "Postal", "Early (other regions)", "Formal", "Informal", "Totals"))) %>% 
  filter(!(cand %in% c("Absent", "Declaration", "Postal", "Early (other regions)", "Formal", "Informal", "Total")))

re <- "\\(([^()]+)\\)"

pb_exp$party <- gsub(re, "\\1", str_extract_all(pb_exp$cand, re))

pb <- left_join(pb_exp, pp16, by = "pp")

pb_clp <- pb %>% 
  filter(party == "CLP")

pb_party <- pb %>% 
  group_by(pp, div, party, lat, lon) %>% 
  summarise(v = sum(v, na.rm = T)) %>% 
  group_by(pp, div, lat, lon) %>% 
  mutate(p = v / sum(v, na.rm = T) * 100)

pb_party <- pb_party %>% 
  spread(key = "party", value = "p") %>% 
  group_by(pp, div, lat, lon) %>% 
  summarise(`1TP` = sum(`1TP`, na.rm = T),
            ALP = sum(ALP, na.rm = T),
            CLP = sum(CLP, na.rm = T),
            GRN = sum(GRN, na.rm = T),
            Ind = sum(Ind, na.rm = T), 
            SFP = sum(SFP, na.rm = T))

pb_party <- pb_party %>% 
  mutate(Other = `1TP` + Ind + SFP)

pb_party$popup_label <- paste0("<b>", pb_party$pp, "</b>", "<br/>", "CLP: ", round(pb_party$CLP, 1), "%", "<br/>", "ALP: ", round(pb_party$ALP, 1), "%", "<br/>", "GRN: ", round(pb_party$GRN, 1), "%", "<br/>", "Other: ", round(pb_party$Other, 1), "%", "<br/>")

pal_clp <- colorBin(c("#ffebcc", "#ffc266", "#ff9900", "#995c00"), domain = pb_party$CLP, bins = c(0, 30, 40, 50, 75))

centers <- data.frame(gCentroid(nt_elec_map, byid = TRUE))

centers$div <- toupper(nt_elec_map@data$SED_NAME16)

m_clp <- leaflet(data = pb_party) %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addPolygons(data = nt_elec_map, fillColor = ~pal(col), fillOpacity = 0.5, color = "black", weight = 0.5, opacity = 0.5) %>% 
  addLegend(title = "CLP primary (%)", pal = pal_clp, values = c(0, 25), position = "bottomright") %>% 
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = ~div,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(
                        "color" = "white",
                        "font-family" = "serif",
                        "font-style" = "bold",
                        "font-size" = "8px"))) %>% 
  addCircleMarkers(data = pb_party %>% filter(!is.na(lat)), fillOpacity = 1, color = ~pal_clp(CLP), radius = 5, stroke = FALSE, popup = ~popup_label)



# pb_exp_gg <- geocode(location = pb_exp$pp, output = "more", source = "google")

# write_csv(pb_braitling, paste0(d, "data/pb_braitling.csv"))

# MAPS ----

list_nt <- nt_elec@data$SED_NAME16[400:424]

nt_elec <- nt_elec[nt_elec$SED_NAME16 %in% list_nt, ]

tpp_map <- tpp_w %>% 
  rename(SED_NAME16 = div)

tpp_map$clp <- ifelse(tpp_map$m > 0, tpp_map$p, tpp_map$p + (tpp_map$m * 2))

tpp_map[tpp_map$SED_NAME16 == "Nhulunbuy", ]$clp <- NA

tpp_map[tpp_map$SED_NAME16 == "Barkly", ]$clp <- NA

tpp_map[tpp_map$SED_NAME16 == "Blain", ]$clp <- NA

tpp_map[tpp_map$SED_NAME16 == "Karama", ]$clp <- NA

tpp_map$col <- ifelse(tpp_map$party == "ALP" & tpp_map$p < 58.1, "alp_marg", 
       ifelse(tpp_map$party == "ALP" & tpp_map$p >= 58.1, "alp_safe", ifelse(tpp_map$party == "CLP" & tpp_map$p < 53, "clp_marg", ifelse(tpp_map$party == "CLP" & tpp_map$p > 53, "clp_safe", "ind"))))

list_t <- nt_elec_map[nt_elec_map@data$col == "alp_marg", ]$SED_NAME16

nt_map_t <- nt_elec[nt_elec$SED_NAME16 %in% list_t, ]

nt_elec@data[nt_elec@data$SED_NAME16 == "Goyder (NT)", ]$SED_NAME16 <- "Goyder"

nt_elec@data[nt_elec@data$SED_NAME16 == "Stuart (NT)", ]$SED_NAME16 <- "Stuart"

nt_elec_map <- sp::merge(nt_elec, tpp_map, by = "SED_NAME16", all=F)

pal <- colorFactor(
  palette = c('#ff8c8c', "red", 'blue', "#0000FF", 'yellow'),
  domain = nt_elec_map$col
)

labels <- sprintf(
  "<strong>%s</strong><br/>%s <br/>TPP: %g %%",
  nt_elec_map$SED_NAME16, nt_elec_map$party, round(nt_elec_map$p,1)
) %>% lapply(htmltools::HTML)

m_nt_elec <- leaflet(nt_elec_map) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal(col), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = F),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 
  
# saveWidget(m_nt_elec, file=paste0(d,"maps/m_nt_elec.html"))
