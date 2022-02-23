library(readxl)
library(tidyverse)

responses <- read_excel("responses-2.xlsx")

to_keep <- responses[,c(1:6)]
to_remove <- responses[,c(1, 7:11)]
order <- responses[,c(1, 12)]

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

keep <- to_keep %>% 
  pivot_longer(cols = c(2:6), values_to = "option") %>% 
  select(-name) %>% 
  na.omit() %>% 
  mutate(respondents = length(unique(`#`))) %>% 
  ungroup() %>% 
  group_by(option) %>% 
  mutate(count = n(),
         popularity = round(count / respondents, 4) * 100) %>% 
  select(-`#`) %>% 
  distinct()

remove <- to_remove %>% 
  pivot_longer(cols = c(2:6), values_to = "option") %>% 
  select(-name) %>% 
  na.omit() %>% 
  mutate(respondents = length(unique(`#`))) %>% 
  ungroup() %>% 
  group_by(option) %>% 
  mutate(count = n(),
         popularity = round(count / respondents, 4) * 100) %>% 
  select(-`#`) %>% 
  distinct()

merging <- bind_rows(keep, remove, .id = "id") %>% 
  select(-count) #%>% 
  # pivot_wider(id_cols = c(respondents, option), names_from = id, values_from = popularity)

devtools::install_github('bart6114/artyfarty')
library(artyfarty)

merging <- merging %>%  mutate(sppInv= ifelse(id =="2",popularity*-1,popularity))
merging <- merging %>%  mutate(id= ifelse(id =="2","remove","keep"))

# plot for only the North slope
merging %>% 
  ggplot(aes(x=option, y = sppInv,  fill=id))+
  geom_bar(stat="identity",position="identity")+
  scale_fill_manual(name="Response type",values = c("#FFA373","#50486D"))+
  coord_flip()+
  ggtitle("Which elements would you like to remove | keep ?")+
  geom_hline(yintercept=0)+
  ylab("% of respondents selecting the option")+
  xlab("")+
  scale_y_continuous(limits = c(-75,75), breaks = pretty(merging$sppInv), labels = abs(pretty(merging$sppInv))) +
  theme_scientific()

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# ORDER
devtools::install_github("davidsjoberg/ggbump")
library(ggbump)
library(cowplot)
library(wesanderson)

ordering <- order %>% 
  separate(col = order, into = c("1", "2", "3", "4", "5"), sep = ",") %>% 
  pivot_longer(cols = c(2:6), names_to = "rank") %>% 
  rename(respondents = `#`)

ggplot(ordering, aes(respondents, as.numeric(rank), color = value)) +
  geom_point(size = 7) + 
  geom_bump(size = 2, smooth = 8) +
  scale_y_reverse() +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(
        panel.grid.major = element_blank()) +
  labs(y = "rank",
       x = "respondents") +
  scale_x_discrete(labels = NULL) +
  scale_color_manual(values = wes_palette(n = 5, name = "Royal2"))

ordering_scores <- ordering %>% 
  mutate(total_resp = length(unique(respondents))) %>% 
  ungroup() %>% 
  group_by(value) %>% 
  mutate(mean_rank = round(mean(as.numeric(rank)), 2)) %>% 
  select(-c(respondents, rank)) %>% 
  distinct() %>% 
  arrange(mean_rank)

