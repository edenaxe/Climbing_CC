# Installs any required packages that aren't already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, jsonlite,  
               highcharter, formattable, plotly, 
               htmlwidgets, ggridges, viridis, hrbrthemes)


# Create WKT type polygons for each climbing location
locations <- rbind.data.frame(
  c('Yosemite', 'POLYGON((-119.63565916882614 37.75863977004669,-119.53884215222457 37.75863977004669,-119.53884215222457 37.71384026083025,-119.63565916882614 37.71384026083025,-119.63565916882614 37.75863977004669))'),
  c('The Buttermilks', 'POLYGON((-118.58086547056519 37.32981394701595,-118.57009371916138 37.32981394701595,-118.57009371916138 37.325650644954806,-118.58086547056519 37.325650644954806,-118.58086547056519 37.32981394701595))'),
  c('Joshua Tree', 'POLYGON((-116.21409968300509 34.03727503745559,-116.04758814736056 34.03727503745559,-116.04758814736056 33.947894230321516,-116.21409968300509 33.947894230321516,-116.21409968300509 34.03727503745559))'),
  c('Black Mountain', 'POLYGON((-116.72889014637542 33.845255418457,-116.62074347889495 33.845255418457,-116.62074347889495 33.759383702288346,-116.72889014637542 33.759383702288346,-116.72889014637542 33.845255418457))'),
  c('Red Rocks', 'POLYGON((-115.42494754426592 36.15476373113308,-115.42812327973955 36.15465977812404,-115.4354618036043 36.15803818037594,-115.4342387162935 36.159891908478336,-115.42880992524736 36.159891908478336,-115.42406777970903 36.157691684280074,-115.42494754426592 36.15476373113308))'))
colnames(locations) <- c("Place", "geometry")


# List of all possible slugs using the 4 priority models for CA
slugs <- c("HadGEM2-ES_rcp45", "CNRM-CM5_rcp45", "CanESM2_rcp45", "MIROC5_rcp45", 
           "HadGEM2-ES_rcp85", "CNRM-CM5_rcp85", "CanESM2_rcp85", "MIROC5_rcp85")


# This function takes in a location and slug (model + scenario) and outputs a data frame 
# The data frame includes Place, Year, Month, Date, Model, Scenario, Indicator, Value
daily_max <- function(location, slug) { 
  
  getPolygon <- function(x){  
    Locations[match(x, locations[[1]]),2]
  } 
  
  # Making the GET requet from the Cal-Adapt API and converting in to a usable format
  content_json <- GET(paste0("http://api.cal-adapt.org/api/series/tasmax_day_", slug, "/events"),
                      query = list(g = getPolygon(location),
                                   stat = "mean",
                                   pagesize = "100",
                                   imperial = TRUE)) %>%
    content(as = "text") %>%
    fromJSON()
  
  daily_temps <- cbind.data.frame(Value = content_json[["data"]], Date = as.Date(content_json[["index"]])) %>%
    mutate(Place = location,
           Model = unlist(strsplit(slug, "_"))[1],
           Scenario = unlist(strsplit(slug, "_"))[2],
           Month = substr(Date, start = 6, stop = 7),
           Year = substr(content_json[["index"]], start = 1, stop = 4),
           Indicator = "Maximum Daily Temperature") %>%
    filter(Year >= 2020) %>%
    select(Place, Year, Month, Date, Model, Scenario, Indicator, Value)
  return(daily_temps)
  
}  


# This function takes in a location and outputs a data frame with all slugs
# Builds off the above function
get_all <- function(place) {
  
  lapply(slugs, 
         daily_max, 
         location = place) %>% 
    bind_rows() %>%
    group_by(Place, Year, Month, Date, Scenario, Indicator) %>%
    summarize(Value = mean(Value)) %>%
    mutate(Model = "Ensemble Mean") %>%
    select(Place, Year, Month, Date, Scenario, Model, Indicator, Value)
  
}


# Loop through all the locations and retrieve daily data using the 'get_all' function
# Rename scenarios and round up to two digits for temp values
all_locations <- lapply(locations$Place, 
                        get_all) %>% 
  bind_rows() %>%
  mutate(Month = lubridate::month(as.numeric(Month), label = TRUE, abbr = TRUE),
         Scenario = ifelse(Scenario == "rcp45", "Low Emissions", "High Emissions"),
         Value = round(Value, digits = 2))


# Create a ridgeline graph for month and highlight ideal temp range
ridge_graph <- function(data, scenario, maxT, minT) { 
  
  # Find number of days at or below ideal temps cutoff point
  number_days <- data %>%
    filter(Scenario == scenario) %>%
    filter(Value <= maxT) %>%
    filter(Value >= minT) %>%
    nrow()
  number_days <- round(number_days/length(unique(data$Year)), digits = 1)
  
  # Generate the ridge plot
  data %>%
    filter(Scenario == scenario) %>%
    group_by(Date, Month) %>%
    summarize(Value = mean(Value), Month) %>%
    ggplot(aes(x = Value, y = Month, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Temp. [F]", option = "A") +
    labs(title = paste0('Maximum Daily Temperatures in ', data$Place[1]),
         subtitle = paste0('Estimated ', number_days, 
                           ' days of ideal climbing temperature between ', 
                           min(data$Year), ' and ', max(data$Year),
                           ' under a ', tolower(scenario), ' scenario'),
         x = "Maximum Daily Temperature [F]") +
    geom_vline(xintercept = maxT, linetype = 2, color = "darkblue") +
    geom_vline(xintercept = minT, linetype = 2, color = "darkblue") +
    annotate("rect", xmin = minT, xmax = maxT, ymin = 0, 
             fill = "blue", ymax = 15, alpha = .1) +
    theme_ipsum() +
    theme(legend.position="none",
          panel.spacing = unit(0.1, "lines"),
          strip.text.x = element_text(size = 8))     
  
}


save(all_locations, locations, ridge_graph, file = "all_locations.rda")




# Load and test
load("all_locations.rda")

ridge_graph(all_locations %>%
              filter(Place == "The Buttermilks",
                     Year %in% (2030:2060)), 
            "Low Emissions", 55, 35)








