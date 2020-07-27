library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(oce)
library(progress)



arctic_data <- read_csv("data/whole_df_1.csv")

arctic_data$year <- year(arctic_data$time)
arctic_data$month <- month(arctic_data$time)

arctic_data <- arctic_data %>% select(-coords) %>% 
  group_by(Station, year, month) %>% 
  mutate(groups = cur_group_id()) %>% 
  select(Station,
         year,
         month,
         groups,
         Lon,
         Lat,
         lon_arc,
         lat_arc,
         depth,
         thetao,
         so,
         temp) %>% 
  arrange(Station, year, month, depth)


df_with_zero <- as_tibble(matrix(vector(), 0, length(names(arctic_data)), 
                                   dimnames = list(c(),names(arctic_data))))

ugroup = unique(arctic_data$groups)

pb <- progress_bar$new(
  total = length(ugroup),
  format = "procesed [:bar] :percent eta: :eta time elapsed: :elapsed")

for(group in ugroup){
  pb$tick()
  group_arr <- arctic_data %>% filter(groups == group)
  len_hor <- length(group_arr$depth)
  for(hor in 2:len_hor){
    if((group_arr$temp[hor - 1] < 0 & group_arr$temp[hor] >= 0) | 
       (group_arr$temp[hor - 1] > 0 & group_arr$temp[hor] <= 0)){
          node_string <- tibble(Station = group_arr$Station[hor],
                                year = group_arr$year[hor],
                                month = group_arr$month[hor],
                                groups = group_arr$groups[hor], 
                                Lon = group_arr$Lon[hor],
                                Lat = group_arr$Lat[hor],
                                lon_arc = group_arr$lon_arc[hor],
                                lat_arc = group_arr$lat_arc[hor],
                                depth = oce.approx(x = c(group_arr$temp[hor - 1], group_arr$temp[hor]),
                                                   y = c(group_arr$depth[hor - 1], group_arr$depth[hor]),
                                                   0),
                                thetao = oce.approx(x = c(group_arr$temp[hor - 1], group_arr$temp[hor]),
                                                    y = c(group_arr$thetao[hor - 1], group_arr$thetao[hor]),
                                                    0),
                                so = oce.approx(x = c(group_arr$temp[hor - 1], group_arr$temp[hor]),
                                                y = c(group_arr$so[hor - 1], group_arr$so[hor]),
                                                0),
                                temp = 0)
          group_arr <- rbind(group_arr, node_string)
    }
  }
  group_arr <- group_arr %>% arrange(depth)
  df_with_zero <- bind_rows(df_with_zero, group_arr)
}

df_with_zero$sigma <- swSigma(df_with_zero$so, df_with_zero$temp, pressure = df_with_zero$depth)
df_with_zero <- df_with_zero %>% 
  filter(temp > -5) %>% 
  write_csv("data/data_with_0_by_month.csv")
  # group_by(year, Station) %>% 
  # mutate(Layer = ifelse(temp >= 0, 'Atl',
  #                       ifelse(temp < 0 & sigma < 29.7, 'Upper', 'Bottom')))
  # mutate(Th = ifelse(Layer == 'Upper', ifelse(length(depth[Layer == 'Atl']) != 0, depth[T == 0][1],
  #                                             ifelse(length(depth[Layer == 'Bottom']) != 0, depth[Layer == 'Bottom'][1], max(depth[Layer == 'Upper']))),
  #                    ifelse(Layer == 'Atl', ifelse(length(depth[T == 0]) == 2, depth[T == 0][2] - depth[T == 0][1],
  #                                                  ifelse(length(depth[Layer == 'Upper']) != 0, max(depth[Layer == 'Atl']) - depth[T == 0], max(depth[Layer == 'Atl']))),
  #                           ifelse(Layer == 'Bottom', ifelse(length(depth[Layer == 'Upper']) != 0 & length(depth[Layer == 'Atl']) != 0, max(depth) - depth[T == 0][2],
  #                                                            ifelse(length(depth) == 0, max(depth) - depth[temp == 0][1], max(depth) - max(depth[Layer == 'Upper']))), 1))))




