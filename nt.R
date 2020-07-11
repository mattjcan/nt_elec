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

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Documents/git/nt_elec/" # parent directory for the data

nt_elec <- readOGR(paste0(d,"data/SED_2016_AUST.shx"))

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

url <- "https://ntec.nt.gov.au/elections/NT-Legislative-Assembly-elections/Past-elections/results-general-elections/2016-territory-election/results/arafura"

pb_arafura <- url %>%
  read_html() %>%
  html_nodes(xpath='//table') %>%
  html_table(fill = TRUE)

as_tibble(pb_arafura[[3]])

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
  addPolygons(fillColor = ~pal(col), fillOpacity = 0.75, weight = 0.2, color = "white", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

