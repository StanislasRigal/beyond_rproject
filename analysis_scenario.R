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


driver_grid_long$drivers <- factor(driver_grid_long$drivers, levels = c("Climate feedback","GHG emissions","Population","Environmental regulation","Trade","Agriculture area or production","Forest area or production","Urban area",
                                                                        "Biofuel","Transport"))


driver_grid_longer <- pivot_stages_longer(na.omit(driver_grid_long), stages_from = c("paperID","drivers"), values_from = "values", additional_aes_from = "drivers")



driver_grid_long2 <- pivot_longer(driver_grid_plot[,c("paperID","Biodiversity indirect proxy","Biodiversity direct proxy")], cols=c("Biodiversity indirect proxy","Biodiversity direct proxy"),
                                 names_to = "drivers")

names(driver_grid_long2)[c(2,3)] <- c("proxy","values")

driver_grid_longer2 <- pivot_stages_longer(na.omit(driver_grid_long2), stages_from = c("proxy","paperID"), values_from = "values", additional_aes_from = "proxy")

driver_grid_longer2$values <- 3

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
                               "Biodiversity indirect proxy"="#33b983","Biodiversity direct proxy"="#008c5c")) +
  #geom_text(aes(label = node, alpha=as.factor(stage)), size= 3, col="black", stat = "sankeynode", position = position_sankey(v_space = "auto", order = "as_is", align = "justify", nudge_x = 0.3)) +
  #scale_alpha_manual(values = c("paperID"=0,"drivers"=1)) +
  #geom_text(aes(label = node, alpha=as.factor(stage)), size= 3, col="black", stat = "sankeynode", position = position_sankey(v_space = "auto", order = "as_is", align = "justify")) +
  #scale_alpha_manual(values = c("paperID"=1,"drivers"=0)) +
  guides(fill   = guide_legend(ncol = 1), alpha  = guide_legend(ncol = 1), colour = guide_legend(ncol = 1)) +
  theme_void() +
  theme(legend.position = "none")



ggsave("output/sankey_all.png",
       width = 7,
       height = 4,
       dpi = 500)


# paper values

value_grid <- read.csv("raw_data/paper_hnr.csv")

value_grid_hnr <- value_grid |> group_by(Human.Nature.Relationship) |> summarise(count=n())
value_grid_svn <- value_grid |> group_by(Specific.values.of.nature) |> summarise(count=n())

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

#value_grid <- rbind(value_grid, data.frame(Paper.title.ID = rep("summit",3), Human.Nature.Relationship = NA, Specific.values.of.nature = c("Relational","Intrinsic","Instrumental")))

value_grid$snv_x <- value_grid$snv_y <- NA
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Relational")] <- 0
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Relational")] <- 0
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Intrinsic")] <- 2
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Intrinsic")] <- 2*sqrt(3)
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Instrumental")] <- 4
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Instrumental")] <- 0
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Instrumental, Intrinsic")] <- 3
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Instrumental, Intrinsic")] <- sqrt(3)
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Instrumental, Relational")] <- 2
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Instrumental, Relational")] <- 0
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Intrinsic, Relational")] <- 1
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Intrinsic, Relational")] <- sqrt(3)
value_grid$snv_x[which(value_grid$Specific.values.of.nature == "Instrumental, Intrinsic, Relational")] <- 2
value_grid$snv_y[which(value_grid$Specific.values.of.nature == "Instrumental, Intrinsic, Relational")] <- 2/3*sqrt(3)


ggplot(value_grid, aes(x=snv_x, y=snv_y) ) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1, seed = 0))

ggplot(value_grid, aes(x=snv_x, y=snv_y) ) +
  geom_hex() +
  scale_fill_continuous(type = "viridis")

ggplot(value_grid, aes(x=snv_x, y=snv_y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_continuous(palette = "Grey") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,4)) +
  scale_y_continuous(expand = c(0, 0)) + theme_void() +
  theme(legend.position='none') 

ggsave("output/value_NNF.png",
       width = 4,
       height = 2*sqrt(3),
       dpi = 300)

### analyse scenario PGMC
PGMC_narrative <- read.csv("raw_data/PGMC_narrative.csv")

names(PGMC_narrative) <- c("time","name","global_coop","internat_gov","internat_prio_sov","internat_prio_env","internat_prio_soc","internat_prio_str",
                           "scale_gov","democracy","societal_prio_env","societal_prio_need","societal_prio_comf","societal_prio_equ",
                           "provisioning_str","provisioning_coord","livelihood_mech_marketwage","livelihood_mech_pubemp","livelihood_mech_incdec","livelihood_mech_state","livelihood_mech_coment",
                           "intervention_mod_market","intervention_mod_regu","intervention_mod_pubinv","intervention_mod_institreconf","intervention_mod_behav","intervention_mod_volunsimpl",
                           "intervention_point_demred","intervention_point_demshift","intervention_point_suppred","intervention_point_suppimpr","intervention_point_suppshift","intervention_point_demimpr",
                           "mat_provisioning_techdep","mat_provisioning_automationlev","enduse_tech","tech_complexity","precautionary_principle","transition_type",
                           "transition_trigger","transition_description",
                           "change_actor_comnet","change_actor_socmov","change_actor_union","change_actor_party","change_actor_regbloc","change_actor_north","change_actor_south","change_actor_supranat",
                           "HNR_NS","HNR_NN","HNR_NC","cosmovision","political_orientation","title","summary")

PGMC_narrative[PGMC_narrative==""] <- NA

PGMC_narrative$internat_prio_sov[which(PGMC_narrative$internat_prio_sov == "Sovereignty centred")] <- 1

PGMC_narrative[which(PGMC_narrative$internat_prio_sov == "Balanced"),c("internat_prio_sov","internat_prio_env","internat_prio_soc","internat_prio_str")] <- 1


PGMC_narrative_data <- PGMC_narrative[,c("global_coop","internat_gov","internat_prio_sov","internat_prio_env","internat_prio_soc","internat_prio_str",
                                         "scale_gov","democracy","societal_prio_env","societal_prio_need","societal_prio_comf","societal_prio_equ",
                                         "provisioning_str","provisioning_coord","livelihood_mech_marketwage","livelihood_mech_pubemp","livelihood_mech_incdec","livelihood_mech_state","livelihood_mech_coment",
                                         "intervention_mod_market","intervention_mod_regu","intervention_mod_pubinv","intervention_mod_institreconf","intervention_mod_behav","intervention_mod_volunsimpl",
                                         "intervention_point_demred","intervention_point_demshift","intervention_point_suppred","intervention_point_suppimpr","intervention_point_suppshift","intervention_point_demimpr",
                                         "mat_provisioning_techdep","mat_provisioning_automationlev","enduse_tech","tech_complexity","precautionary_principle","transition_type",
                                         "transition_trigger",
                                         "change_actor_comnet","change_actor_socmov","change_actor_union","change_actor_party","change_actor_regbloc","change_actor_north","change_actor_south","change_actor_supranat",
                                         "HNR_NS","HNR_NN","HNR_NC")]

PGMC_narrative_data$global_coop <- as.factor(PGMC_narrative_data$global_coop)
PGMC_narrative_data$internat_gov <- as.factor(PGMC_narrative_data$internat_gov)
PGMC_narrative_data$internat_prio_sov <- as.numeric(PGMC_narrative_data$internat_prio_sov)
PGMC_narrative_data$scale_gov <- as.factor(PGMC_narrative_data$scale_gov)
PGMC_narrative_data$democracy <- as.factor(PGMC_narrative_data$democracy)
PGMC_narrative_data$provisioning_str <- as.factor(PGMC_narrative_data$provisioning_str)
PGMC_narrative_data$provisioning_coord <- as.factor(PGMC_narrative_data$provisioning_coord)
PGMC_narrative_data$mat_provisioning_techdep <- as.factor(PGMC_narrative_data$mat_provisioning_techdep)
PGMC_narrative_data$mat_provisioning_automationlev <- as.factor(PGMC_narrative_data$mat_provisioning_automationlev)
PGMC_narrative_data$enduse_tech <- as.factor(PGMC_narrative_data$enduse_tech)
PGMC_narrative_data$tech_complexity <- as.factor(PGMC_narrative_data$tech_complexity)
PGMC_narrative_data$precautionary_principle <- as.factor(PGMC_narrative_data$precautionary_principle)
PGMC_narrative_data$transition_type <- as.factor(PGMC_narrative_data$transition_type)
PGMC_narrative_data$transition_trigger <- as.factor(PGMC_narrative_data$transition_trigger)

PGMC_narrative_data$internat_prio_sov <- factor(PGMC_narrative_data$internat_prio_sov,levels=c("4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$internat_prio_env <- factor(PGMC_narrative_data$internat_prio_env,levels=c("4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$internat_prio_soc <- factor(PGMC_narrative_data$internat_prio_soc,levels=c("4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$internat_prio_str <- factor(PGMC_narrative_data$internat_prio_str,levels=c("4","3","2","1"),ordered=TRUE)

PGMC_narrative_data$societal_prio_env <- factor(PGMC_narrative_data$societal_prio_env,levels=c("4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$societal_prio_need <- factor(PGMC_narrative_data$societal_prio_need,levels=c("4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$societal_prio_comf <- factor(PGMC_narrative_data$societal_prio_comf,levels=c("4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$societal_prio_equ <- factor(PGMC_narrative_data$societal_prio_equ,levels=c("4","3","2","1"),ordered=TRUE)

PGMC_narrative_data$livelihood_mech_marketwage <- factor(PGMC_narrative_data$livelihood_mech_marketwage,levels=c("5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$livelihood_mech_pubemp <- factor(PGMC_narrative_data$livelihood_mech_pubemp,levels=c("5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$livelihood_mech_incdec <- factor(PGMC_narrative_data$livelihood_mech_incdec,levels=c("5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$livelihood_mech_state <- factor(PGMC_narrative_data$livelihood_mech_state,levels=c("5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$livelihood_mech_coment <- factor(PGMC_narrative_data$livelihood_mech_coment,levels=c("5","4","3","2","1"),ordered=TRUE)

PGMC_narrative_data$intervention_mod_market <- factor(PGMC_narrative_data$intervention_mod_market,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_mod_regu <- factor(PGMC_narrative_data$intervention_mod_regu,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_mod_pubinv <- factor(PGMC_narrative_data$intervention_mod_pubinv,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_mod_institreconf <- factor(PGMC_narrative_data$intervention_mod_institreconf,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_mod_behav <- factor(PGMC_narrative_data$intervention_mod_behav,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_mod_volunsimpl <- factor(PGMC_narrative_data$intervention_mod_volunsimpl,levels=c("6","5","4","3","2","1"),ordered=TRUE)

PGMC_narrative_data$intervention_point_demred <- factor(PGMC_narrative_data$intervention_point_demred,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_point_demshift <- factor(PGMC_narrative_data$intervention_point_demshift,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_point_suppred <- factor(PGMC_narrative_data$intervention_point_suppred,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_point_suppimpr <- factor(PGMC_narrative_data$intervention_point_suppimpr,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_point_suppshift <- factor(PGMC_narrative_data$intervention_point_suppshift,levels=c("6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$intervention_point_demimpr <- factor(PGMC_narrative_data$intervention_point_demimpr,levels=c("6","5","4","3","2","1"),ordered=TRUE)

PGMC_narrative_data$change_actor_comnet <- factor(PGMC_narrative_data$change_actor_comnet,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_socmov <- factor(PGMC_narrative_data$change_actor_socmov,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_union <- factor(PGMC_narrative_data$change_actor_union,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_party <- factor(PGMC_narrative_data$change_actor_party,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_regbloc <- factor(PGMC_narrative_data$change_actor_regbloc,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_north <- factor(PGMC_narrative_data$change_actor_north,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_south <- factor(PGMC_narrative_data$change_actor_south,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)
PGMC_narrative_data$change_actor_supranat <- factor(PGMC_narrative_data$change_actor_supranat,levels=c("8","7","6","5","4","3","2","1"),ordered=TRUE)

PGMC_narrative_data$HNR_NS <- factor(PGMC_narrative_data$HNR_NS,levels=c("3","2","1"),ordered=TRUE)
PGMC_narrative_data$HNR_NN <- factor(PGMC_narrative_data$HNR_NN,levels=c("3","2","1"),ordered=TRUE)
PGMC_narrative_data$HNR_NC <- factor(PGMC_narrative_data$HNR_NC,levels=c("3","2","1"),ordered=TRUE)


write.csv(PGMC_narrative_data,"output/PGMC_narrative_data.csv", row.names = FALSE)

#### is structured (graphical)

gower_mat_dist <- cluster::daisy(PGMC_narrative_data)
seriation::dissplot(gower_mat_dist)

#### group number

nc <- c(NbClust::NbClust(diss=gower_mat_dist,distance=NULL,method="ward.D2",index="frey")$Best.nc[1],NbClust::NbClust(diss=gower_mat_dist,distance=NULL,method="ward.D2",index="mcclain")$Best.nc[1],
NbClust::NbClust(diss=gower_mat_dist,distance=NULL,method="ward.D2",index="cindex")$Best.nc[1],NbClust::NbClust(diss=gower_mat_dist,distance=NULL,method="ward.D2",index="silhouette")$Best.nc[1],
NbClust::NbClust(diss=gower_mat_dist,distance=NULL,method="ward.D2",index="dunn")$Best.nc[1])

#### hierachical classification (ascendent and descendant)

dendro_ac <- cluster::agnes(gower_mat_dist,method="ward")
factoextra::fviz_dend(dendro_ac)
classif_ac <- cutree(dendro_ac,k=4)

dendro_ac2 <- cluster::agnes(gower_mat_dist,method="average")
factoextra::fviz_dend(dendro_ac2)
classif_ac2 <- cutree(dendro_ac2,k=4)
classif_ac2_3 <- cutree(dendro_ac2,k=3)

dendro_dec <- cluster::diana(gower_mat_dist)
factoextra::fviz_dend(dendro_dec)
classif_dec <- cutree(dendro_dec,k=4)

fpc::cluster.stats(gower_mat_dist,clustering=classif_ac)$avg.silwidth # max best
fpc::cluster.stats(gower_mat_dist,clustering=classif_ac2)$avg.silwidth
fpc::cluster.stats(gower_mat_dist,clustering=classif_dec)$avg.silwidth
fpc::cluster.stats(gower_mat_dist,clustering=classif_ac)$dunn # max best
fpc::cluster.stats(gower_mat_dist,clustering=classif_ac2)$dunn
fpc::cluster.stats(gower_mat_dist,clustering=classif_dec)$dunn
clValid::connectivity(gower_mat_dist,clusters=classif_ac) # min best
clValid::connectivity(gower_mat_dist,clusters=classif_ac2)
clValid::connectivity(gower_mat_dist,clusters=classif_dec)

#### check classification

sil <- cluster::silhouette(classif_ac2,gower_mat_dist)
plot(sil)
classif_ac2[10] <- 3
classif_ac2[17] <- 4
classif_ac2[19] <- 4
sil <- cluster::silhouette(classif_ac2,gower_mat_dist)
plot(sil)

sil3 <- cluster::silhouette(classif_ac2_3,gower_mat_dist)
plot(sil3)
classif_ac2_3[10] <- 3
sil3 <- cluster::silhouette(classif_ac2_3,gower_mat_dist)
plot(sil3)

PGMC_narrative$class <- classif_ac2

### Mix analysis

#### regroup unfreq modalities

PGMC_narrative_data2 <- PGMC_narrative_data

PGMC_narrative_data2$global_coop[which(PGMC_narrative_data2$global_coop == "Full cooperation")] <- "High cooperation"
PGMC_narrative_data2$internat_gov[which(PGMC_narrative_data2$internat_gov == "Fragmented")] <- "Networked localism"
PGMC_narrative_data2$internat_gov[which(PGMC_narrative_data2$internat_gov == "Supranational governance")] <- "Regional bloc governance"
PGMC_narrative_data2$internat_prio <- apply(PGMC_narrative_data2[,c("internat_prio_sov","internat_prio_env","internat_prio_soc","internat_prio_str")],1,
                                           FUN = function(x){
                                             x <- as.numeric(x)
                                             x[which(is.na(x))] <- 0
                                             if(length(which(x==max(x))) > 1){
                                               y <- "Balanced"
                                             }else{
                                               if(which.max(x) == 1){
                                                 y <- "Sovereign"
                                               }
                                               if(which.max(x) == 2){
                                                 y <- "Environment"
                                               }
                                               if(which.max(x) == 3){
                                                 y <- "Social_justice"
                                               }
                                               if(which.max(x) == 4){
                                                 y <- "Structural_justice"
                                               }
                                             }
                                           return(y)
                                           })
PGMC_narrative_data2$internat_prio[which(PGMC_narrative_data2$internat_prio == "Environment")] <- "Balanced"
PGMC_narrative_data2[,c("internat_prio_sov","internat_prio_env","internat_prio_soc","internat_prio_str")] <- NULL
PGMC_narrative_data2$societal_prio <- apply(PGMC_narrative_data2[,c("societal_prio_env","societal_prio_need","societal_prio_comf","societal_prio_equ")],1,
                                            FUN = function(x){
                                              x <- as.numeric(x)
                                              x[which(is.na(x))] <- 0
                                              if(length(which(x==max(x))) > 1){
                                                y <- "Balanced"
                                              }else{
                                                if(which.max(x) == 1){
                                                  y <- "Environment"
                                                }
                                                if(which.max(x) == 2){
                                                  y <- "Need"
                                                }
                                                if(which.max(x) == 3){
                                                  y <- "Comfort"
                                                }
                                                if(which.max(x) == 4){
                                                  y <- "Equity"
                                                }
                                              }
                                              return(y)
                                            })
PGMC_narrative_data2$societal_prio[which(PGMC_narrative_data2$societal_prio == "Environment")] <- "Balanced"
PGMC_narrative_data2[,c("societal_prio_env","societal_prio_need","societal_prio_comf","societal_prio_equ")] <- NULL
PGMC_narrative_data2$livelihood_mech <- apply(PGMC_narrative_data2[,c("livelihood_mech_marketwage","livelihood_mech_pubemp","livelihood_mech_incdec","livelihood_mech_state","livelihood_mech_coment")],1,
                                            FUN = function(x){
                                              x <- as.numeric(x)
                                              x[which(is.na(x))] <- 0
                                              if(length(which(x==max(x))) > 1){
                                                y <- "Balanced"
                                              }else{
                                                if(which.max(x) == 1){
                                                  y <- "Market_wage"
                                                }
                                                if(which.max(x) == 2){
                                                  y <- "Public_employment"
                                                }
                                                if(which.max(x) == 3){
                                                  y <- "Income_decoupled"
                                                }
                                                if(which.max(x) == 4){
                                                  y <- "State"
                                                }
                                                if(which.max(x) == 5){
                                                  y <- "Community_entitlement"
                                                }
                                              }
                                              return(y)
                                            })
PGMC_narrative_data2$livelihood_mech[which(PGMC_narrative_data2$livelihood_mech %in% c("Balanced","Public_employment","State"))] <- "Other"
PGMC_narrative_data2[,c("livelihood_mech_marketwage","livelihood_mech_pubemp","livelihood_mech_incdec","livelihood_mech_state","livelihood_mech_coment")] <- NULL
PGMC_narrative_data2$intervention_mod <- apply(PGMC_narrative_data2[,c("intervention_mod_market","intervention_mod_regu","intervention_mod_pubinv","intervention_mod_institreconf","intervention_mod_behav","intervention_mod_volunsimpl")],1,
                                              FUN = function(x){
                                                x <- as.numeric(x)
                                                x[which(is.na(x))] <- 0
                                                if(length(which(x==max(x))) > 1){
                                                  y <- "Balanced"
                                                }else{
                                                  if(which.max(x) == 1){
                                                    y <- "Market"
                                                  }
                                                  if(which.max(x) == 2){
                                                    y <- "Regulation"
                                                  }
                                                  if(which.max(x) == 3){
                                                    y <- "Public_investment"
                                                  }
                                                  if(which.max(x) == 4){
                                                    y <- "INstitutional_reconfiguration"
                                                  }
                                                  if(which.max(x) == 5){
                                                    y <- "Social_behavioural"
                                                  }
                                                  if(which.max(x) == 6){
                                                    y <- "Voluntary_simplicity"
                                                  }
                                                }
                                                return(y)
                                              })
PGMC_narrative_data2$intervention_mod[which(PGMC_narrative_data2$intervention_mod %in% c("Public_investment"))] <- "Balanced"
PGMC_narrative_data2[,c("intervention_mod_market","intervention_mod_regu","intervention_mod_pubinv","intervention_mod_institreconf","intervention_mod_behav","intervention_mod_volunsimpl")] <- NULL
PGMC_narrative_data2$intervention_point <- apply(PGMC_narrative_data2[,c("intervention_point_demred","intervention_point_demshift","intervention_point_suppred","intervention_point_suppimpr","intervention_point_suppshift","intervention_point_demimpr")],1,
                                               FUN = function(x){
                                                 x <- as.numeric(x)
                                                 x[which(is.na(x))] <- 0
                                                 if(length(which(x==max(x))) > 1){
                                                   y <- "Balanced"
                                                 }else{
                                                   if(which.max(x) == 1){
                                                     y <- "Demand_reduction"
                                                   }
                                                   if(which.max(x) == 2){
                                                     y <- "Demand_shift"
                                                   }
                                                   if(which.max(x) == 3){
                                                     y <- "Supply_reduction"
                                                   }
                                                   if(which.max(x) == 4){
                                                     y <- "Supply_improve"
                                                   }
                                                   if(which.max(x) == 5){
                                                     y <- "Supply_shift"
                                                   }
                                                   if(which.max(x) == 6){
                                                     y <- "Demand_improve"
                                                   }
                                                 }
                                                 return(y)
                                               })
PGMC_narrative_data2$intervention_point[which(PGMC_narrative_data2$intervention_point %in% c("Supply_improve","Supply_reduction"))] <- "Supply"
PGMC_narrative_data2$intervention_point[which(PGMC_narrative_data2$intervention_point %in% c("Demand_reduction","Demand_shift"))] <- "Demand"
PGMC_narrative_data2[,c("intervention_point_demred","intervention_point_demshift","intervention_point_suppred","intervention_point_suppimpr","intervention_point_suppshift","intervention_point_demimpr")] <- NULL
PGMC_narrative_data2$transition_trigger <- as.character(PGMC_narrative_data2$transition_trigger)
PGMC_narrative_data2$transition_trigger[which(PGMC_narrative_data2$transition_trigger %in% c("Ecological trigger","Geopolitical trigger","Technological trigger"))] <- "Other"
PGMC_narrative_data2$transition_trigger[which(PGMC_narrative_data2$transition_trigger %in% c("Escalating social, political, geopolitical, techno-economic, and environmental crises, in particular the spread of fascism, lead to a rise of populist, progressive green or moderately ecosocial parties/politicians like Zac Polanksi. Their agenda focuses on addressing national social and global environmental issues."))] <- "Cumulative"
PGMC_narrative_data2$change_actor <- apply(PGMC_narrative_data2[,c("change_actor_comnet","change_actor_socmov","change_actor_union","change_actor_party","change_actor_regbloc","change_actor_north","change_actor_south","change_actor_supranat")],1,
                                                 FUN = function(x){
                                                   x <- as.numeric(x)
                                                   x[which(is.na(x))] <- 0
                                                   if(length(which(x==max(x))) > 1){
                                                     y <- "Balanced"
                                                   }else{
                                                     if(which.max(x) == 1){
                                                       y <- "Community_network"
                                                     }
                                                     if(which.max(x) == 2){
                                                       y <- "Social_movment"
                                                     }
                                                     if(which.max(x) == 3){
                                                       y <- "Trade_union"
                                                     }
                                                     if(which.max(x) == 4){
                                                       y <- "Political_party"
                                                     }
                                                     if(which.max(x) == 5){
                                                       y <- "Regional_bloc"
                                                     }
                                                     if(which.max(x) == 6){
                                                       y <- "Global_north"
                                                     }
                                                     if(which.max(x) == 7){
                                                       y <- "Global_south"
                                                     }
                                                     if(which.max(x) == 8){
                                                       y <- "Supranational"
                                                     }
                                                   }
                                                   return(y)
                                                 })
PGMC_narrative_data2$change_actor[which(PGMC_narrative_data2$change_actor %in% c("Regional_bloc","Global_north","Global_south"))] <- "Supranational"
PGMC_narrative_data2$change_actor[which(PGMC_narrative_data2$change_actor %in% c("Social_movment","Trade_union"))] <- "Community_network"
PGMC_narrative_data2[,c("change_actor_comnet","change_actor_socmov","change_actor_union","change_actor_party","change_actor_regbloc","change_actor_north","change_actor_south","change_actor_supranat")] <- NULL
PGMC_narrative_data2$HNR <- apply(PGMC_narrative_data2[,c("HNR_NS","HNR_NN","HNR_NC")],1,
                                           FUN = function(x){
                                             x <- as.numeric(x)
                                             x[which(is.na(x))] <- 0
                                             if(length(which(x==max(x))) > 1){
                                               y <- "Balanced"
                                             }else{
                                               if(which.max(x) == 1){
                                                 y <- "NS"
                                               }
                                               if(which.max(x) == 2){
                                                 y <- "NN"
                                               }
                                               if(which.max(x) == 3){
                                                 y <- "NC"
                                               }
                                             }
                                             return(y)
                                           })
PGMC_narrative_data2[,c("HNR_NS","HNR_NN","HNR_NC")] <- NULL
PGMC_narrative_data2$mat_provisioning_automationlev <- as.character(PGMC_narrative_data2$mat_provisioning_automationlev)
PGMC_narrative_data2$mat_provisioning_automationlev[which(is.na(PGMC_narrative_data2$mat_provisioning_automationlev))] <- "Other"
PGMC_narrative_data2$provisioning_str <- as.character(PGMC_narrative_data2$provisioning_str)
PGMC_narrative_data2$provisioning_str[which(PGMC_narrative_data2$provisioning_str == "Limited accumulation")] <- "Accumulation permitting"

PGMC_narrative_data2$global_coop <- as.factor(as.character(PGMC_narrative_data2$global_coop))
PGMC_narrative_data2$internat_gov <- as.factor(as.character(PGMC_narrative_data2$internat_gov))
PGMC_narrative_data2$provisioning_str <- as.factor(PGMC_narrative_data2$provisioning_str)
PGMC_narrative_data2$mat_provisioning_automationlev <- as.factor(PGMC_narrative_data2$mat_provisioning_automationlev)
PGMC_narrative_data2$transition_trigger <- as.factor(PGMC_narrative_data2$transition_trigger)
PGMC_narrative_data2$internat_prio <- as.factor(PGMC_narrative_data2$internat_prio)
PGMC_narrative_data2$societal_prio <- as.factor(PGMC_narrative_data2$societal_prio)
PGMC_narrative_data2$livelihood_mech <- as.factor(PGMC_narrative_data2$livelihood_mech)
PGMC_narrative_data2$intervention_mod <- as.factor(PGMC_narrative_data2$intervention_mod)
PGMC_narrative_data2$intervention_point <- as.factor(PGMC_narrative_data2$intervention_point)
PGMC_narrative_data2$change_actor <- as.factor(PGMC_narrative_data2$change_actor)
PGMC_narrative_data2$HNR <- as.factor(PGMC_narrative_data2$HNR)


AMix <- ade4::dudi.mix(PGMC_narrative_data2,scannf=FALSE,nf=10)
RVAideMemoire::MVA.synt(AMix)
RVAideMemoire::MVA.plot(AMix)
RVAideMemoire::scat.cr(AMix,axis=1)

gower_mat_dist2 <- cluster::daisy(PGMC_narrative_data2)
PGMC_narrative_data2$cluster <- classif_ac2

dbRDA <- vegan::dbrda(gower_mat_dist2 ~ global_coop+internat_gov+scale_gov+democracy+provisioning_str+provisioning_coord+mat_provisioning_techdep+
                        mat_provisioning_automationlev+enduse_tech+tech_complexity+precautionary_principle+transition_type+transition_trigger+internat_prio+societal_prio+
                        livelihood_mech+intervention_mod+intervention_point+change_actor+HNR,data=PGMC_narrative_data2, add=TRUE)
dbRDA <- vegan::dbrda(gower_mat_dist2 ~ internat_gov+democracy+provisioning_str+mat_provisioning_techdep+
                        transition_type+internat_prio+societal_prio,data=PGMC_narrative_data2, add=TRUE)
RVAideMemoire::MVA.synt(dbRDA)
RVAideMemoire::MVA.anova(dbRDA)
RVAideMemoire::MVA.plot(dbRDA, fac=classif_ac2, labels=PGMC_narrative$title, points=FALSE)
RVAideMemoire::MVA.plot(dbRDA, fac=classif_ac2_3, labels=PGMC_narrative$title, points=FALSE)
RVAideMemoire::MVA.plot(dbRDA, fac=cutree(dendro_ac2,k=15), labels=PGMC_narrative$title, points=FALSE)
plot(dbRDA)
plot(dbRDA, display='wa')
plot(dbRDA, display='bp')
