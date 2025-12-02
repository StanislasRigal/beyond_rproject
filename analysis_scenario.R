#setwd("/home/rigal/Téléchargements/5-Workshop1")


analysis_grid <- read.csv("analysis_beyond_ws1.csv")
analysis_grid[analysis_grid == ""] <- NA
analysis_grid$spatial <- factor(analysis_grid$spatial, levels = c("city","subnational","national","europe","world"))
#analysis_grid <- analysis_grid[c(11:83),]


library(ggplot2)
library(reshape2)
library(viridis)

ggplot(analysis_grid) +
  geom_bar(aes(x=spatial)) +
  coord_flip() +
  xlab("") +
  theme_bw()

ggplot(analysis_grid) +
  geom_histogram(aes(x=temporal)) +
  xlab("") +
  theme_bw()

ggplot(analysis_grid) +
  geom_histogram(aes(x=n_scenario)) +
  xlab("") +
  theme_bw()

ggplot(analysis_grid) +
  geom_histogram(aes(x=n_scenario_postgrowth)) +
  xlab("") +
  theme_bw()

analysis_grid_long <- reshape2::melt(analysis_grid[,c("ID","n_scenario","n_scenario_postgrowth","n_scenario_greengrowth","n_scenario_BAU")], id.vars = "ID")

ggplot(analysis_grid_long[which(analysis_grid_long$variable != "n_scenario"),], aes(x=value, fill=variable)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity',binwidth = 1) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) + xlab("Number of scenarios") + ylab("Number of articles") +
  scale_fill_manual(values=c("darkgreen", "lightgreen","black")) + facet_wrap(~variable) + theme_minimal() +
  theme(legend.position="none")

ggsave("number_scenario.png",
       width = 6,
       height = 4,
       dpi = 300)

analysis_grid$environmental <- "none"
analysis_grid$environmental[which((analysis_grid$biodiversity_explicit == 1 | analysis_grid$biodiversity_implicit == 1) &
                                    (analysis_grid$climate_scenario == 1 | analysis_grid$climate_output == 1))] <- "biodiversity and climate"
analysis_grid$environmental[which((analysis_grid$biodiversity_explicit == 1 | analysis_grid$biodiversity_implicit == 1) &
                                    (analysis_grid$climate_scenario != 1 & analysis_grid$climate_output != 1))] <- "biodiversity"
analysis_grid$environmental[which((analysis_grid$biodiversity_explicit != 1 & analysis_grid$biodiversity_implicit != 1) &
                                    (analysis_grid$climate_scenario == 1 | analysis_grid$climate_output == 1))] <- "climate"

ggplot(analysis_grid) +
  geom_bar(aes(x=environmental, fill=environmental)) +
  coord_flip() +
  scale_fill_manual(values=c("green", "blue","red","black"))+
  xlab("") + ylab("Number of articles") + 
  theme_bw() + theme(legend.position = "none")

ggsave("environment.png",
       width = 6,
       height = 4,
       dpi = 300)

ggplot(analysis_grid) +
  geom_histogram(aes(x=year, fill=environmental)) +
  scale_fill_manual(values=c("green", "blue","red","black"))+
  xlab("") + ylab("Number of articles") + facet_wrap(~environmental) +
  scale_x_continuous(breaks = c(2010:2024)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggsave("environment2.png",
       width = 6,
       height = 6,
       dpi = 300)



analysis_grid$biodiversity <- "none"
analysis_grid$biodiversity[which(analysis_grid$biodiversity_explicit == 1)] <- "explicit"
analysis_grid$biodiversity[which(analysis_grid$biodiversity_implicit == 1)] <- "implicit"
analysis_grid$biodiversity[which(analysis_grid$crop == 1 | analysis_grid$livestock == 1 | analysis_grid$wood == 1 | analysis_grid$housing == 1)] <- "available information"

ggplot(analysis_grid) +
  geom_bar(aes(x=biodiversity, fill=biodiversity)) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green","lightgreen","black"))+
  xlab("") + ylab("Number of articles") + 
  theme_bw() + theme(legend.position = "none")

ggsave("biodiversity.png",
       width = 6,
       height = 4,
       dpi = 300)

ggplot(analysis_grid) +
  geom_histogram(aes(x=year, fill=biodiversity)) +
  scale_fill_manual(values=c("green", "blue","red","black"))+
  xlab("") + ylab("Number of articles") + facet_wrap(~biodiversity) +
  scale_x_continuous(breaks = c(2011:2024)) +
  scale_y_continuous(breaks = c(0:13)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggsave("biodiversity2.png",
       width = 6,
       height = 6,
       dpi = 300)

analysis_grid_socio <- reshape2::melt(analysis_grid[,c("ID","socioeco1","socioeco2","socioeco3","socioeco4")], id.vars = "ID")
analysis_grid_socio$value_new <- as.character(analysis_grid_socio$value)
analysis_grid_socio$value_new[which(analysis_grid_socio$value %in% c("basic income","income","private debt","private wealth","living standard","pensions"))] <- "private wealth"
analysis_grid_socio$value_new[which(analysis_grid_socio$value %in% c("inequality","unequality","poverty"))] <- "equality"
analysis_grid_socio$value_new[which(analysis_grid_socio$value %in% c("unemployment","work time reduction","Labour-capital"))] <- "employment"
analysis_grid_socio$value_new[which(analysis_grid_socio$value %in% c("consumption","financial crash","deflation","inflation","price","public debt", "public wealth", "stability","production","climate disturbance"))] <- "macroeconomic stability"
analysis_grid_socio$value_new[which(analysis_grid_socio$value %in% c("health","education","welfare","Well-being","sustainable prosperity","food","life expectancy","development"))] <- "well-being"
analysis_grid_socio$value_new[which(analysis_grid_socio$value %in% c("energy consumption","energy demand","energy scarcity","transport","carbone emission"))] <- "energy"

ggplot(analysis_grid_socio[which(!is.na(analysis_grid_socio$value_new)),], aes(x=value_new, fill=value_new)) +
  geom_bar(alpha=0.6) + 
  xlab("Socioeconomic effects") + ylab("Number of observation") +
  scale_fill_viridis_d() + theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("socioeco.png",
       width = 6,
       height = 4,
       dpi = 300)
