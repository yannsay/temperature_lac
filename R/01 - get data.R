# get data

# start_day <- floor_date(today(tzone = "CET"), "weeks")
# end_day <- ceiling_date(today(tzone = "CET"), "weeks") -1
inital_update <- "2024-06-14"
update_day <- today(tzone = "CET")
start_day <- "2024-06-10"
end_day <- "2024-06-16"

start_day_call <- str_replace_all(start_day, "-", "") %>% paste0("0000")
end_day_call <- str_replace_all(end_day, "-", "") %>% paste0("2200")

latitude_call <-"46.303696"
longitude_call <- "6.239853"

url_call <- paste0("https://alplakes-api.eawag.ch/simulations/depthtime/delft3d-flow/geneva/",
start_day_call,"/",end_day_call,"/",latitude_call,"/",longitude_call)

# Define the URL for the GET request
# url <- "https://alplakes-api.eawag.ch/simulations/depthtime/delft3d-flow/geneva/202406090000/202406152300/46.303696/6.239853"


# Perform the GET request and store the response
response <- try(GET(url_call, accept("application/json")),silent = T)

if(class(response) == "try-error") {
  json_file <- "data/response_14062024.json"
  json_data <- fromJSON(json_file)
  update_day <- inital_update
} else if(status_code(response) == 200){
  json_data <-  content(response, "text") |> jsonlite::fromJSON()
} else if(status_code(response) != 200) {
  json_file <- "data/response_14062024.json"
  json_data <- fromJSON(json_file)
  update_day <- inital_update
}

# clean
json_data$depth$data

json_data$temperature$data

json_data$time

hourmin <- json_data$time %>% str_extract("[0-9]{4}$")
yearmonthday <- json_data$time %>% str_extract("[0-9]{8}")

day_hour <- json_data$time %>% as_datetime(format = "%Y%m%d%H%M", tz = "CET")

dates_names <- day_hour %>% as.character() %>%
   str_replace_all(":00$", "")

dates_names <- ifelse(nchar(dates_names) < max(nchar(dates_names)), paste(dates_names, "00:00"), dates_names)

initialdata <- json_data$temperature$data %>%
  as_tibble() %>%
  `names<-`(dates_names) %>%
  mutate(profondeur = json_data$depth$data) %>%
  filter(rowSums(is.na(across(-profondeur))) != ncol(.)-1)


# create table
long_show_table <- initialdata %>%
  mutate(across(everything(), \(x) round(x,1))) %>%
  select(profondeur, everything()) %>%
  arrange(profondeur) %>%
  mutate(profondeur = -profondeur)

small_show_table <- long_show_table %>%
  mutate(profondeur = round(profondeur)) %>%
  filter(!duplicated(profondeur)) %>%
  filter(profondeur %in% seq(0, -50, by = -5) )

# per day

unique_days <- dates_names %>% str_remove_all(" .*") %>% unique()
week_days <- dates_names %>% ymd_hm(tz = "CET") %>% weekdays() %>% unique()
if("lundi" %in% week_days) {
  week_days <- week_days %>%
    str_replace("lundi", "Monday") %>%
    str_replace("mardi", "Tuesday") %>%
    str_replace("mercredi", "Wednesday") %>%
    str_replace("jeudi", "Thursday") %>%
    str_replace("vendredi", "Friday") %>%
    str_replace("samedi", "Saturday") %>%
    str_replace("dimanche", "Sunday")
}

long_show_table_day <- map(unique_days,
                           ~select(long_show_table, profondeur, contains(.x))) %>%
  set_names(week_days)

small_show_table_day <- map(unique_days,
                           ~select(small_show_table, profondeur, contains(.x))) %>%
  set_names(week_days)
