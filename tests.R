library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(oce)


ar_df <- read_csv("data/data_with_0.csv") %>% select(-Th)
ar_df <- ar_df %>%
  group_by(Station, year) %>% 
  mutate(Layer = ifelse(temp >= 0, "Atl", "non"),
         groups = cur_group_id())

nodes <- ar_df %>% 
  filter(Layer == "Atl") %>% 
  summarise(groups = first(groups))

atl_nodes <- left_join(nodes[-c(1,2)], ar_df, by= 'groups')


atl_nodes <- atl_nodes %>% 
  mutate(depth = as.numeric(depth),
         Layer = as.factor(Layer)) %>% 
  group_by(groups) %>%
  mutate(Layer1 = ifelse(first(depth[Layer == "Atl"]) > depth, "Upper", "0")) %>% 
  mutate(Layer1 = ifelse(temp >= 0, "Atl", Layer1)) %>% 
  mutate(Layer1 = ifelse(Layer1 == "0", "Bottom", Layer1))


atl_nodes <- atl_nodes %>% 
  group_by(groups) %>% 
  mutate(isbot = ifelse(Layer1 == "Atl" & (depth > max(depth[Layer1 == "Bottom"])) & (length(Layer1[Layer1 == "Bottom"]) != 0),
                        "1", "0"))

atl_nodes <-  atl_nodes %>% 
  group_by(groups) %>% 
  mutate(isbot = ifelse(Layer1 == "Bottom" & (depth < max(depth[Layer1 == "Atl"])),"1", "0"))

a <- atl_nodes %>%
  group_by(groups) %>% 
  filter(any(isbot == 1))

atl_nodes %>% 
  group_by(groups) %>% 
  filter(depth < 1000) %>% 
  ggplot()+
  geom_point(aes(temp, depth, group = groups, col = Layer1), size = 0.07, alpha=0.5)+
#  geom_point(aes(temp, depth, col = Layer1), size = 0.1)+
  scale_y_reverse()

atl_nodes <- atl_nodes %>% 
  filter(!groups %in% unique(a$groups))





atl_nodes_for_merge <- atl_nodes %>% 
  select(groups, Layer1, depth) %>% 
  rename(Layer = Layer1)


ar_df_test <- ar_df %>% select(-Layer)
fin_df <- right_join(atl_nodes_for_merge,
                     ar_df_test,by = c("groups", "depth"))

up_bt_gr <-  ar_df %>% 
  group_by(groups) %>% 
  filter(all(Layer == "non")) %>% 
  summarise(groups = first(groups))


up_bt_df <- ar_df %>% 
  filter(groups %in% up_bt_gr$groups)
up_bt_df <- up_bt_df %>% 
  group_by(groups) %>% 
  mutate(grad = (lead(sigma) - sigma)/(lead(depth) - depth))

test_gr <- up_bt_df %>% 
  group_by(groups) %>% 
  filter(any(depth < 100 & sigma > 29))
test_gr %>% 
  ggplot(aes(grad, depth))+
  geom_point(aes())+
  geom_path(aes(group=groups, col = factor(groups)))+
  scale_y_reverse()

# 
# bot %>%
#   filter(groups == 947) %>% 
#   ggplot()+
#   geom_point(aes(so, depth, group = groups, col = Layer1))+
#   scale_y_reverse()
# 
# bot %>% 
#   filter(depth < 100) %>% 
#   ggplot()+
#   geom_point(aes(temp, so, shape = Layer1, col = depth))+
#   scale_y_reverse()
