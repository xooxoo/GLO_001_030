library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(oce)
library(ggdark)
library(RColorBrewer)


# загружаем исходный фрэйм с нулевой изотермой
arc_df <- read_csv("data/data_with_0_by_month.csv")
ar_df <- arc_df %>%
  group_by(groups) %>% 
  mutate(Layer = ifelse(temp >= 0, "Atl", "none"))

ar_df_test <- left_join(nodes138[c("Station", "Subbas")], arc_df, by='Station')
a <- arc_df %>% filter(Station %in% na_nodes)

# фильтруем узлы с атлантическими водами
nodes <- ar_df %>% 
  select(Station, year, groups, Layer) %>% 
  group_by(groups) %>% 
  filter(any(Layer == "Atl")) %>% 
  summarise_all(first)

atl_nodes <- left_join(nodes[1], ar_df, by = 'groups')

# костыли для определения верхнего и нижнего слоя
atl_nodes<- atl_nodes %>% 
  mutate(depth = as.numeric(depth),
         Layer = as.factor(Layer)) %>% 
  group_by(groups) %>%
  mutate(Layer1 = ifelse(first(depth[Layer == "Atl"]) > depth, "Upper", "0")) %>% 
  mutate(Layer1 = ifelse(temp >= 0, "Atl", Layer1)) %>% 
  mutate(Layer1 = ifelse(Layer1 == "0", "Bottom", Layer1))

# проверяем, чтобы атлантические воды не лежали ниже нижнего слоя
atl_nodes <- atl_nodes %>% 
  mutate(isbot = ifelse(length(Layer1[Layer1 == "Bottom"] != 0), 
                        ifelse(max(depth[Layer1 == "Atl"]) > min(depth[Layer1 == "Bottom"]), "0", "1"), "none"))

# проверяем наличие верхнего слоя в узле и даляем те узлы,
# в которых алтантические воды лежат выше верхнего слоя
# лишнее условие в данном случае
# atl_nodes_beta <- atl_nodes %>%
#   mutate(isup = ifelse(length(Layer1[Layer1 == "Upper"] != 0),
#                         ifelse(min(depth[Layer1 == "Atl"]) > max(depth[Layer1 == "Upper"]), "0", "1"), "none"))


# удаляем нулевые
atl_nodes <- atl_nodes %>%
  group_by(groups) %>% 
  filter(any(isbot!=0),
         last(depth) > 300) %>% 
  mutate(Layer = Layer1) %>% 
  select(-c(Layer1, isbot))
  
# теперь выделяем узлы без атлантических вод

nodes_up <-  ar_df %>% 
  group_by(groups) %>% 
  filter(all(Layer == "none")) %>% 
  summarise(groups = first(groups))

up_bt_gr <- left_join(nodes_up, ar_df, by = 'groups')


up_bt_gr <- up_bt_gr %>% 
  group_by(groups) %>% 
  filter(last(depth) > 300) %>% 
  mutate(Layer = ifelse(depth > 300, "Bottom", "Upper"))

fin_df <- bind_rows(atl_nodes, up_bt_gr)


fin_df %>% 
  filter(month == 4) %>% 
  ggplot()+
  geom_path(aes(temp, depth, group = groups), size = 0.3, alpha = 0.3)+
  geom_point(aes(temp, depth, col=Layer), size = 0.1)+
  scale_y_reverse()+
  scale_color_brewer(palette ='Set2')+
  dark_theme_light()

write_csv(fin_df, "data/df_with_layers.csv")

