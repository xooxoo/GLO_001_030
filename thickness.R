library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(oce)
library(ggdark)
library(RColorBrewer)



arc_df <- read_csv("data/df_with_layers.csv") %>% 
  arrange(year, Station)

nodes138 <- read_csv("data/nodes138.csv")

data_50_13 <- read_csv("data/data_50_13_subs.csv") %>% 
  mutate(SUBBAS = ifelse(SUBBAS == "WEST", "Amer", "Euro"))

nodes138 <- left_join(nodes138, data_50_13[c("Node", "SUBBAS")] %>% 
                        group_by(Node) %>% 
                        summarize_all(first), by='Node') %>% 
  rename(Station = Node,
         Subbas = SUBBAS)

arc_df <- left_join(nodes138[c("Station", "Subbas")], arc_df, by='Station')
# считаем толщину слоев:
#     Если слой верхний, и в узле присутсвует атл. вода, то толщина слоя будет равнятся границе атл. слоя.
#   Если атл. вод нет, то проверяется на наличие донных вод, если они есть, 
#   то берется граница донного слоя за толщину.
#     Далее проверяется условие Атл. вод. Если в узле присутсвуеют две нулевые изотермы, 
#   то это означает, что это границы между слоями (спорное решение, но рабочее). Тогда считаем тощину атл.
#   вод как разницу в глубине между двумя 0-изотермами. 
#   Если 0-изотерма одна: проверяется наличие верхнего слоя в узле, если он есть, то толщина атл. вод -
#   это разница в глубине между последним горизонтом, где есть Атл. вода и первой 0-изотермой.
#   Если верхнего слоя нет и 0-изотерм меньше 2, то просто берется максимальная глубина залегания атл.вод.
#     Если слой является донными водами, и над ним имеются 2 слоя, то тощиной является разница между 
#   максимальной глубиной и второй 0-изотермой. Если в узле нет верхнего слоя, то 0-изотерма будет только одна,
#   толщина считается также, как и в прошлом случае, только используется глубина залегания первой 0-изотермы.
#   Если в узле отсутствуют атл. воды, то толщина считается как разница между нижней границей верхнего слоя
#   и макс. глубиной.


arc_df <- arc_df %>% 
  group_by(groups) %>% 
  mutate(Th = ifelse(Layer == 'Upper', ifelse(length(depth[Layer == 'Atl']) != 0, 
                                              depth[temp == 0][1], 
                                       ifelse(length(depth[Layer == 'Bottom']) != 0, 
                                              depth[Layer == 'Bottom'][1], 
                                              max(depth[Layer == 'Upper']))), 
                                       ifelse(Layer == 'Atl', 
                                              ifelse(length(depth[temp == 0]) == 2, 
                                                     depth[temp == 0][2] - depth[temp == 0][1], 
                                       ifelse(length(depth[Layer == 'Upper']) != 0, 
                                              max(depth[Layer == 'Atl']) - depth[temp == 0], 
                                              max(depth[Layer == 'Atl']))), 
                                       ifelse(Layer == 'Bottom', 
                                              ifelse(length(depth[Layer == 'Upper']) != 0 & length(depth[Layer == 'Atl']) != 0, 
                                              max(depth) - depth[temp == 0][2], 
                                       ifelse(length(depth[Layer == 'Upper']) == 0, 
                                              max(depth) - depth[temp == 0][1], 
                                              max(depth) - max(depth[Layer == 'Upper']))),1))))


# arc_df_test <- left_join(nodes138[c("Station", "Subbas")], arc_df, by='Station')
# arc_df_test$Station[which(is.na(arc_df_test$groups))]

arc_df <- arc_df %>% na.omit(arc_df)

layer_mean <- arc_df %>% 
  filter(month < 5) %>% 
  group_by(year, Layer,Subbas) %>% 
  mutate(Layer = ordered(factor(Layer), c("Upper", "Atl", "Bottom"))) %>% 
  summarize_all(mean)

layer_mean %>%
  ggplot()+
  geom_point(aes(year, temp, col = Subbas))+
  geom_path(aes(year, temp, col = Subbas))+
  facet_grid(Layer~., scales = "free_y")+
  ggsave("by_sub_4mo.png",
         width = 22,
         height = 15,
         units = "cm")

layer_mean %>% 
  select(-c(Station,groups, month)) %>% 
  write_csv("data/glo_layer_subbas_mean_3m.csv")



old_data <- read_csv("data/old_data.csv") %>% 
  filter(Year < 1993) %>% 
  rename(year = Year, Subbas = SUBBAS, so = S, temp = Temp) %>% 
  mutate(Subbas = ifelse(Subbas == "AMER", "Amer", "Euro"))

new_and_old <- layer_mean %>% 
  select(names(old_data)) %>% 
  bind_rows(old_data) %>% 
  mutate(Layer = factor(Layer, levels = c("Upper", "Atl", "Bottom"),
                           ordered = TRUE)) %>% 
  arrange(year, Layer)

new_and_old %>% 
  ggplot()+
  geom_point(aes(year, temp, col = Subbas))+
  geom_path(aes(year, temp, col = Subbas))+
  facet_grid(Layer~., scales = "free_y")+
  ggsave("glo_1950_2018.png",
         width = 22,
         height = 15,
         units = "cm")

write_csv(new_and_old, "glo_data_1950-2018.csv")
