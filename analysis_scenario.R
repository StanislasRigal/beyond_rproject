analysis_grid <- read.csv("raw_data/analysis_beyond_ws1.csv")
analysis_grid[analysis_grid == ""] <- NA
analysis_grid$spatial <- factor(analysis_grid$spatial, levels = c("city","subnational","national","europe","world"))
#analysis_grid <- analysis_grid[c(11:83),]


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

ggsave("output/number_scenario.png",
       width = 6,
       height = 4,
       dpi = 300)

analysis_grid <- analysis_grid[which(is.na(analysis_grid$remark)),]
analysis_grid[is.na(analysis_grid)] <- 0

analysis_grid$environmental <- "No environmental aspect"
analysis_grid$environmental[which((analysis_grid$crop == 1 | analysis_grid$livestock == 1 | analysis_grid$wood == 1 | analysis_grid$housing == 1 | analysis_grid$biodiversity_explicit == 1 | analysis_grid$biodiversity_implicit == 1) &
                                    (analysis_grid$climate_scenario == 1 | analysis_grid$climate_output == 1))] <- "Biodiversity and climate"
analysis_grid$environmental[which((analysis_grid$crop == 1 | analysis_grid$livestock == 1 | analysis_grid$wood == 1 | analysis_grid$housing == 1 | analysis_grid$biodiversity_explicit == 1 | analysis_grid$biodiversity_implicit == 1) &
                                    (analysis_grid$climate_scenario != 1 & analysis_grid$climate_output != 1))] <- "Biodiversity only"
analysis_grid$environmental[which((analysis_grid$crop != 1 & analysis_grid$livestock != 1 & analysis_grid$wood != 1 & analysis_grid$housing != 1 & analysis_grid$biodiversity_explicit != 1 & analysis_grid$biodiversity_implicit != 1) &
                                    (analysis_grid$climate_scenario == 1 | analysis_grid$climate_output == 1))] <- "Climate only"

ggplot(analysis_grid) +
  geom_bar(aes(x=environmental, fill=environmental)) +
  coord_flip() +
  scale_fill_manual(values=c("Biodiversity only"="#C3E57E", "Biodiversity and climate"="#7EC3E5","Climate only"="#8F7EE5","No environmental aspect"="#999999"))+
  xlab("") + ylab("Number of articles") + 
  theme_bw() + theme(legend.position = "none")

ggsave("output/environment.png",
       width = 5,
       height = 5,
       dpi = 300)

ggplot(analysis_grid) +
  geom_histogram(aes(x=year, fill=environmental)) +
  scale_fill_manual(values=c("Biodiversity only"="#C3E57E", "Biodiversity and climate"="#7EC3E5","Climate only"="#8F7EE5","No environmental aspect"="#999999"))+
  xlab("") + ylab("Number of articles") + facet_wrap(~environmental) +
  scale_x_continuous(breaks = c(2010:2024)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggsave("output/environment2.png",
       width = 6,
       height = 6,
       dpi = 300)



analysis_grid$biodiversity <- "No biodiversity"
analysis_grid$biodiversity[which(analysis_grid$crop == 1 | analysis_grid$livestock == 1 | analysis_grid$wood == 1 | analysis_grid$housing == 1)] <- "Direct drivers"
analysis_grid$biodiversity[which(analysis_grid$biodiversity_implicit == 1)] <- "Ecological proxy"
analysis_grid$biodiversity[which(analysis_grid$biodiversity_explicit == 1)] <- "Explicit biodiversity"

ggplot(analysis_grid) +
  geom_bar(aes(x=biodiversity, fill=biodiversity)) +
  coord_flip() +
  scale_fill_manual(values=c("Explicit biodiversity" = "#6B990F", "Ecological proxy" = "#A3CC51","Direct drivers" = "#E5FFB2" ,"No biodiversity" = "#999999"))+
  xlab("") + ylab("Number of articles") + 
  theme_bw() + theme(legend.position = "none")

ggsave("output/biodiversity.png",
       width = 5,
       height = 5,
       dpi = 300)

ggplot(analysis_grid) +
  geom_histogram(aes(x=year, fill=biodiversity)) +
  scale_fill_manual(values=c("Explicit biodiversity" = "#6B990F", "Ecological proxy" = "#A3CC51","Direct drivers" = "#E5FFB2" ,"No biodiversity" = "#999999"))+
  xlab("") + ylab("Number of articles") + facet_wrap(~biodiversity) +
  scale_x_continuous(breaks = c(2011:2024)) +
  scale_y_continuous(breaks = c(0:13)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggsave("output/biodiversity2.png",
       width = 6,
       height = 6,
       dpi = 300)

analysis_grid$biodiversity2 <- ifelse(analysis_grid$biodiversity == "No biodiversity", "No biodiversity", "Biodiversity proxy")

ggplot(analysis_grid) +
  geom_histogram(aes(x=year, fill=biodiversity2),binwidth=1,col="white") +
  scale_fill_manual(values=c("Biodiversity proxy" = "#A3CC51" ,"No biodiversity" = "#999999"))+
  xlab("") + ylab("Number of articles") +
  scale_x_continuous(breaks = c(2011:2024)) +
  scale_y_continuous(breaks = c(0:13)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggplot(analysis_grid) +
  geom_histogram(aes(x=year, fill=biodiversity),binwidth=1,col="white") +
  scale_fill_manual(values=c("Explicit biodiversity" = "#6B990F", "Ecological proxy" = "#A3CC51","Direct drivers" = "#E5FFB2" ,"No biodiversity" = "#999999"))+
  xlab("") + ylab("Number of articles") +
  scale_x_continuous(breaks = c(2011:2024)) +
  scale_y_continuous(breaks = c(0:13)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggplot(analysis_grid) +
  geom_bar(aes(x=year, fill=biodiversity2),position="fill") +
  scale_fill_manual(values=c("Biodiversity proxy" = "#A3CC51" ,"No biodiversity" = "#999999"))+
  xlab("") + ylab("Number of articles") +
  scale_x_continuous(breaks = c(2011:2024)) +
  scale_y_continuous(breaks = c(0:13)) +
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

ggsave("output/biodiversity3.png",
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

ggsave("output/socioeco.png",
       width = 6,
       height = 4,
       dpi = 300)

# driver biodiversity

driver_grid <- read.csv("raw_data/biodiv_drivers.csv")
driver_grid[driver_grid == ""] <- NA

names(driver_grid) <- c("paperID","Biodiversity proxy name","Biodiversity indirect proxy","Biodiversity direct proxy","Climate feedback","GHG emissions","Population","Agriculture area or production","Forest area or production","Urban area",
                        "Biofuel","Environmental regulation","Climate","Trade","Transport","Other")

driver_grid_plot <- driver_grid[,c("paperID","Biodiversity indirect proxy","Biodiversity direct proxy","Climate feedback","GHG emissions","Population","Agriculture area or production","Forest area or production","Urban area",
                                   "Biofuel","Environmental regulation","Trade","Transport")]

driver_grid_plot[driver_grid_plot == 2] <- 1


driver_grid_long <- pivot_longer(driver_grid_plot[,c("paperID","Climate feedback","GHG emissions","Population","Agriculture area or production","Forest area or production","Urban area",
                                                     "Biofuel","Environmental regulation","Trade","Transport")], cols=c("Climate feedback","GHG emissions","Population","Agriculture area or production","Forest area or production","Urban area",
                                                          "Biofuel","Environmental regulation","Trade","Transport"),
                                 names_to = "drivers")

names(driver_grid_long)[3] <- "values"


driver_grid_long$drivers <- factor(driver_grid_long$drivers, levels = c("Climate feedback","GHG emissions","Population","Agriculture area or production","Forest area or production","Urban area",
                                                                        "Biofuel","Environmental regulation","Trade","Transport"))


driver_grid_longer <- pivot_stages_longer(na.omit(driver_grid_long), stages_from = c("paperID","drivers"), values_from = "values", additional_aes_from = "drivers")



driver_grid_long2 <- pivot_longer(driver_grid_plot[,c("paperID","Biodiversity indirect proxy","Biodiversity direct proxy")], cols=c("Biodiversity indirect proxy","Biodiversity direct proxy"),
                                 names_to = "drivers")

names(driver_grid_long2)[c(2,3)] <- c("proxy","values")

driver_grid_longer2 <- pivot_stages_longer(na.omit(driver_grid_long2), stages_from = c("proxy","paperID"), values_from = "values", additional_aes_from = "proxy")

driver_grid_longer2$edge_id <- sort(rep((c(1:9)+max(driver_grid_longer$edge_id)),2))

driver_grid_longer2$stage <- as.character(driver_grid_longer2$stage)
driver_grid_longer$stage <- as.character(driver_grid_longer$stage)
names(driver_grid_longer2)[1] <- "drivers"

driver_grid_longer_all <- rbind(driver_grid_longer,driver_grid_longer2)
driver_grid_longer_all$stage <- factor(driver_grid_longer_all$stage, levels = c("proxy","paperID","drivers"))

ggplot(
  data = driver_grid_longer_all,
  mapping = aes(x = stage, y = values, group = node,
                edge_id = edge_id, connector = connector, colour = stage, label = node)) +
  geom_sankeyedge(aes(fill=drivers), col=NA, position = position_sankey(v_space = "auto", order = "as_is", align = "justify")) +
  geom_sankeynode(col = NA, aes(fill=node),position = position_sankey(v_space = "auto", order = "as_is", align = "justify")) +
  scale_fill_manual(values = c("A05"="#C0C0C0","A36" ="#C0C0C0","A13" ="#C0C0C0","A29" ="#C0C0C0","A58" ="#C0C0C0","A69" ="#C0C0C0","A24" ="#C0C0C0","A44" ="#C0C0C0","A40" ="#C0C0C0","A62" ="#C0C0C0","A82" ="#C0C0C0","A18" ="#C0C0C0","A11" ="#C0C0C0","A75" ="#C0C0C0","A47" ="#C0C0C0","A55" ="#C0C0C0",
                               "Climate feedback"="#680000","GHG emissions"="#9b54f3","Population"="#bf8cfc","Agriculture area or production"="#f98517","Forest area or production"="#008c5c","Urban area"="#ac0000","Biofuel"="#33b983","Environmental regulation"="#0050ae","Trade"="#c85b00","Transport"="#002f64",
                               "Indirect proxy"="#33b983","Direct proxy"="#008c5c")) +
  #geom_text(aes(label = node, alpha=as.factor(stage)), size= 3, col="black", stat = "sankeynode", position = position_sankey(v_space = "auto", order = "as_is", align = "justify", nudge_x = 0.3)) +
  #scale_alpha_manual(values = c("paperID"=0,"drivers"=1)) +
  #geom_text(aes(label = node, alpha=as.factor(stage)), size= 3, col="black", stat = "sankeynode", position = position_sankey(v_space = "auto", order = "as_is", align = "justify")) +
  #scale_alpha_manual(values = c("paperID"=1,"drivers"=0)) +
  guides(fill   = guide_legend(ncol = 1), alpha  = guide_legend(ncol = 1), colour = guide_legend(ncol = 1)) +
  theme_void() +
  theme(legend.position = "none")



ggsave("output/sankey1.png",
       width = 7,
       height = 4,
       dpi = 500)


# paper values

value_grid <- read.csv("raw_data/paper_hnr.csv")

value_grid_hnr <- value_grid %>% group_by(Human.Nature.Relationship) %>% summarise(count=n())
value_grid_svn <- value_grid %>% group_by(Specific.values.of.nature) %>% summarise(count=n())

value_grid_plot <- data.frame(scale = c(rep("Human Nature Relationship",5), rep("Specific values of nature",4)),
                              variable = c(value_grid_hnr$Human.Nature.Relationship, value_grid_svn$Specific.values.of.nature),
                              value = c(value_grid_hnr$count,value_grid_svn$count))

ggplot(value_grid_plot, aes(x=scale, y=value, fill = variable)) + 
  geom_bar(stat = "identity") +  scale_fill_viridis_d() + 
  theme_minimal() + theme(axis.title = element_blank())


ggsave("output/value_barplot.png",
       width = 6,
       height = 4,
       dpi = 300)







