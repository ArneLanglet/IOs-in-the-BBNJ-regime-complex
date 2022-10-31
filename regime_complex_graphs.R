#### Arne Langlet, 31.10.2022
### R code using complex.xlsx dataset to extract data for marine policy "the emerging marine biodiversity regime complex" paper
### prepare and label data for visualization in gephi. 



# read packages
library(igraph)
library(tidyverse)
library(readxl)
library(dplyr)
library(data.table)
library(zoo)
library(lubridate)
library(openxlsx)
library(stringr)
library(igraph)
library(ggplot2)
library(rtf)
library(officer)
library(countrycode)
library(roperators)
library(kableExtra)
library(shiny)
library(visNetwork)
library(tidygraph)


# clean environment
rm(list = ls())

## read data
setwd("//share.univie.ac.at/maripoldata/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data")
complex <- read.csv("complex.csv")

organizations <- subset(complex, host_side_event >= 1 | 
                          total_draft_mentions >= 1 |
                          total_reference >= 1 |
                          total_plenary >= 1 |
                          total_participants >= 1)





## graph 1: connect IOs to provisions in which they were mentioned by a state/alliance 
## for each IGC

### if we want to take UNDOALOS out of sample:
#complex <- filter(complex, actor != "UNDOALOS")

con <- select(complex, actor, alliance_reference_igc1_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$alliance_reference_igc1_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, alliance_reference_igc1_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
crs1 <- cr

con <- select(complex, actor, alliance_reference_igc2_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$alliance_reference_igc2_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, alliance_reference_igc2_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
crs2 <- cr


con <- select(complex, actor, alliance_reference_igc3_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$alliance_reference_igc3_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, alliance_reference_igc3_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
crs3 <- cr

con <- select(complex, actor, alliance_reference_igc4_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$alliance_reference_igc4_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, alliance_reference_igc4_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
crs4 <- cr

con <- select(complex, actor, alliance_reference_igc5_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$alliance_reference_igc5_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, alliance_reference_igc5_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
crs5 <- cr

## aggregate data
crs <- rbind(crs1, crs2, crs3, crs4, crs5)


unique(crs$provision)

## filter out where references were general or could not be attributed to a specific provision/package
crs <- filter(crs, provision != "general")
crs <- filter(crs, provision != "na")

cr <- table(crs)

# save edgelist
colnames(crs) <- c("Source", "Target")

write.csv(crs, "X:/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data/1_edges.csv", row.names = FALSE)


# create network
cr <- cr %>% as.data.frame() %>% select(actor, provision, Freq) 
cr <- filter(cr, Freq > 0)


comp_net1 <- graph.data.frame(cr, directed = TRUE)

V(comp_net1)[V(comp_net1)$name %in% cr[,1]]$type <- 0
V(comp_net1)[V(comp_net1)$name %in% cr[,2]]$type <- 1
V(comp_net1)[V(comp_net1)$type == 1]$shape <- "square"
V(comp_net1)[V(comp_net1)$type == 0]$shape <- "circle"
V(comp_net1)$size <- degree(comp_net1)


V(comp_net1)$package <- ifelse(V(comp_net1)$name %in% c("sharing of benefits", "intellectual property rights", "collection of mgrs", "access to tk",
"application of mgr provision", "mgrs general", "activities with respect to mgrs", "objectives of mgr provision", "access to traditional knowledge", "monitoring and transparency" ), "MGR",
                             ifelse(V(comp_net1)$name %in% c("decision-making for abmts/mpas", "identification of areas", "consultation and assessment", "abmts/mpas general",
                                                             "proposals", "international cooperation and coordination", "objectives of abmt/mpa provision", "monitoring and review", "abmts/mpas generail"), "ABMT/MPA",
                                    ifelse(V(comp_net1)$name %in% c("eia process", "eias general", "thresholds and criteria for eia" , "seas", "public notification and consultation", "relationship to other processes",
                                                                    "monitoring", "evaluation", "screening", "cumulative impacts", "process", "obligation to conduct eia", "preparation and content of eia", "eia"), "EIA",
                                           ifelse(V(comp_net1)$name %in% c("objectives of cbtmt provision", "modalities for cbtmt", "modalities for cbtmt", "cbtmt general", "thresholds and criteria",
                                                                           "types of cbtmt" , "capacity building modalities", "additional modalities", "cbtmt", "cooperation in cbtmt" ), "cbtmt",
                                                  ifelse(V(comp_net1)$name %in% c("funding", "general objectives", "cop" , "scientific and technical body" ,
                                                                                  "clearing-house mechanism" , "decision-making body",  "international cooperation",
                                                                                  "secretariat", "implementation and compliance", "use of terms", "application", 
                                                                                  "relationship to relevant legal instruments", "review", "crosscutting general", "provisional application",
                                                                                  "relationship to other instruments","settlement of disputes", "general principles and approaches"), "Crosscutting",
                                                         "IO")))))
tibble1 <- comp_net1 %>% as_tbl_graph() %>%
  as_tibble()


# check amount of IOs / provisions
table(V(comp_net1)$type)

# calculate degree
d <- degree(comp_net1, mode = "out")
s <- strength(comp_net1, mode = "out", weights = E(comp_net1)$Freq)


c <- data.frame(cbind(d, s))

# save nodelist
colnames(tibble1) <- c("Id", "type", "shape", "size", "package")

write.csv(tibble1, "X:/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data/1_nodes.csv", row.names = FALSE)





## graph 2: connect IOs to provisions in which they made a statement
## for each IGC

######## IO involvement

## IGC 1
con <- select(complex, actor, igo_activity_igc1_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$igo_activity_igc1_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, igo_activity_igc1_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
cr1 <- cr


### IGC 2
con <- select(complex, actor, igo_activity_igc2_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$igo_activity_igc2_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, igo_activity_igc2_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
cr2 <- cr

### IGC 3
con <- select(complex, actor, igo_activity_igc3_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$igo_activity_igc3_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, igo_activity_igc3_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
cr3 <- cr

### IGC 4
con <- select(complex, actor, igo_activity_igc4_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$igo_activity_igc4_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, igo_activity_igc4_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
cr4 <- cr

### IGC 5
con <- select(complex, actor, igo_activity_igc5_provision)
con <- as.data.frame(con)
con$actor <- str_to_upper(con$actor)
con <- filter(con, (con$igo_activity_igc5_provision) != "")
cr <- tidytext::unnest_tokens(con, provision, igo_activity_igc5_provision, token = 'regex', pattern=",")
cr$provision <- trimws(cr$provision, "l")
cr5 <- cr


## aggregate data
cr <- rbind(cr1, cr2, cr3, cr4, cr5)



## if we want to filter out general statements or statements by UNDOALOS?
#cr <- filter(cr, provision != "general")
#cr <- filter(cr, actor != "UNDOALOS")

crs <- cr
cr <- table(cr)
cr <- cr %>% as.data.frame() %>% select(actor, provision, Freq) 



# save edgelist
colnames(crs) <- c("Source", "Target")
write.csv(crs, "X:/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data/2_edges.csv", row.names = FALSE)


# create network

cr <- filter(cr, Freq != 0)
comp_net1 <- graph.data.frame(cr, directed = TRUE)

V(comp_net1)[V(comp_net1)$name %in% cr[,1]]$type <- 0
V(comp_net1)[V(comp_net1)$name %in% cr[,2]]$type <- 1
V(comp_net1)[V(comp_net1)$type == 1]$shape <- "square"
V(comp_net1)[V(comp_net1)$type == 0]$shape <- "circle"
V(comp_net1)$size <- degree(comp_net1)
V(comp_net1)$package <- ifelse(V(comp_net1)$name %in% c("sharing of benefits", "intellectual property rights", "collection of mgrs", "access to tk",
                                                        "application of mgr provision", "mgrs general", "activities with respect to mgrs", "objectives of mgr provision", "access to traditional knowledge", "monitoring and transparency" ), "MGR",
                               ifelse(V(comp_net1)$name %in% c("decision-making for abmts/mpas", "identification of areas", "consultation and assessment", "abmts/mpas general",
                                                               "proposals", "international cooperation and coordination", "objectives of abmt/mpa provision", "monitoring and review", "abmts/mpas generail"), "ABMT/MPA",
                                      ifelse(V(comp_net1)$name %in% c("eia process", "eias general", "thresholds and criteria",  "seas", "public notification and consultation", "relationship to other processes",
                                                                      "monitoring", "evaluation", "screening", "cumulative impacts", "process", "obligation to conduct eia", "preparation and content of eia", "eia"), "EIA",
                                             ifelse(V(comp_net1)$name %in% c("objectives of cbtmt provision", "modalities for cbtmt", "modalities for cbtmt", "cbtmt general", 
                                                                             "types of cbtmt" , "capacity building modalities", "additional modalities", "cbtmt", "cooperation in cbtmt" ), "cbtmt",
                                                    ifelse(V(comp_net1)$name %in% c("funding", "general objectives", "cop" , "scientific and technical body" ,
                                                                                    "clearing-house mechanism" , "decision-making body",  "international cooperation",
                                                                                    "secretariat", "implementation and compliance", "use of terms", "application", 
                                                                                    "relationship to relevant legal instruments", "review", "crosscutting general", "provisional application",
                                                                                    "relationship to other instruments","settlement of disputes", "general principles and approaches"), "Crosscutting",
                                                           "IO")))))
tibble1 <- comp_net1 %>% as_tbl_graph() %>%
  as_tibble()
table(V(comp_net1)$type)


## save nodelist
colnames(tibble1) <- c("Id", "type", "shape", "size", "package")

write.csv(tibble1, "X:/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data/2_nodes.csv", row.names = FALSE)



## graph 3: connect IOs to provisions in which they were mentioned in a draft text
## for each IGC/draft


### IGC 1 
com <- select(complex, actor, topic_draft1_1)

names(com)[2] <- "provision"

mm1 <- com



################################################## IGC 2 draft 
com <- select(complex, actor, topic_draft2_1)
com2 <- select(complex, actor, topic_draft2_2)
com3 <- select(complex, actor, topic_draft2_3)
com4 <- select(complex, actor, topic_draft2_4)
com5 <- select(complex, actor, topic_draft2_5)
com6 <- select(complex, actor, topic_draft2_6)
com7 <- select(complex, actor, topic_draft2_7)
com8 <- select(complex, actor, topic_draft2_8)

names(com)[2] <- "provision"
names(com2)[2] <- "provision"
names(com3)[2] <- "provision"
names(com4)[2] <- "provision"
names(com5)[2] <- "provision"
names(com6)[2] <- "provision"
names(com7)[2] <- "provision"
names(com8)[2] <- "provision"



mm <- rbind(com, com2, com3, com4, com5, com6, com7, com8)
mm$provision[mm$provision=="conduct of environmental impact assessment"] <- "process" 
mm$provision[mm$provision=="environmental impact assessment consultation"] <- "public notification and consultation" 
mm$provision[mm$provision=="clearing house mechanism"] <- "clearing-house mechanism" 
mm$provision[mm$provision=="objectives of CBTT for capacity building"] <- "objectives of CBTT" 
mm$provision[mm$provision=="capacity building modalities"] <- "modalities" 
mm$provision[mm$provision=="funding of capacity building"] <- "funding" 
mm$provision[mm$provision=="modalities"] <- "modalities for CBTT" 
mm$provision[mm$provision=="benefit sharing"] <- "sharing of benefits" 
mm$provision[mm$provision=="categories of states for capacity building"] <- 'objectives of CBTT'
mm$provision[mm$provision=="process"] <- 'EIA process'

mm$provision <- str_to_lower(mm$provision)
mm$actor[mm$actor=="TRIBUNAL FOR THE LAW OF THE SEA"] <- "ITLOS"

mm2 <- mm






############################################## IGC 3 draft
com <- select(complex, actor, topic_draft3_1)
com2 <- select(complex, actor, topic_draft3_2)
com3 <- select(complex, actor, topic_draft3_3)
com4 <- select(complex, actor, topic_draft3_4)
com5 <- select(complex, actor, topic_draft3_5)
com6 <- select(complex, actor, topic_draft3_6)
com7 <- select(complex, actor, topic_draft3_7)

names(com)[2] <- "provision"
names(com2)[2] <- "provision"
names(com3)[2] <- "provision"
names(com4)[2] <- "provision"
names(com5)[2] <- "provision"
names(com6)[2] <- "provision"
names(com7)[2] <- "provision"



mm <- rbind(com, com2, com3, com4, com5, com6, com7)
mm$provision[mm$provision=="conduct of environmental impact assessment"] <- "process"
mm$provision[mm$provision=="environmental impact assessment consultation"] <- "public notification and consultation"
mm$provision[mm$provision=="clearing house mechanism"] <- "clearing-house mechanism"
mm$provision[mm$provision=="objectives of CBTT for capacity building"] <- "objectives of CBTT"
mm$provision[mm$provision=="capacity building modalities"] <- "modalities"
mm$provision[mm$provision=="funding of capacity building"] <- "funding"
mm$provision[mm$provision=="modalities"] <- "modalities for CBTT"
mm$provision[mm$provision=="benefit sharing"] <- "sharing of benefits"
mm$provision[mm$provision=="categories of states for capacity building"] <- 'objectives of CBTT'
mm$provision[mm$provision=="process"] <- 'EIA process'
mm$provision <- str_to_lower(mm$provision)

mm$actor[mm$actor=="TRIBUNAL FOR THE LAW OF THE SEA"] <- "ITLOS"

mm3 <- mm




######################################################################### IGC 4 draft 
com <- select(complex, actor, topic_draft4_1)

names(com)[2] <- "provision"
mm <- com
mm$provision[mm$provision=="conduct of environmental impact assessment"] <- "process"
mm$provision[mm$provision=="environmental impact assessment consultation"] <- "public notification and consultation"
mm$provision[mm$provision=="clearing house mechanism"] <- "clearing-house mechanism"
mm$provision[mm$provision=="objectives of CBTT for capacity building"] <- "objectives of CBTT"
mm$provision[mm$provision=="capacity building modalities"] <- "modalities"
mm$provision[mm$provision=="funding of capacity building"] <- "funding"
mm$provision[mm$provision=="modalities"] <- "modalities for CBTT"
mm$provision[mm$provision=="benefit sharing"] <- "sharing of benefits"
mm$provision[mm$provision=="categories of states for capacity building"] <- 'objectives of CBTT'
mm$provision[mm$provision=="process"] <- 'EIA process'

mm$actor[mm$actor=="TRIBUNAL FOR THE LAW OF THE SEA"] <- "ITLOS"


mm$actor
unique(mm$provision)

mm4 <- mm

mm4

mm <- filter(mm4, ((provision != "general") | (!is.na(provision)) | (provision != "-")))


mm <- rbind(mm1, mm2, mm3, mm4)

mmv <- as.data.frame(mm)
mmv$actor <- str_to_upper(mmv$actor)
mmv <- filter(mmv, ((provision != "general") | (!is.na(provision)) | (provision != "-")))





######################################################################### IGC5 draft 
com <- select(complex, actor, topic_draft5_1)

names(com)[2] <- "provision"

mm <- rbind(com)
mm$provision[mm$provision=="conduct of environmental impact assessment"] <- "process"
mm$provision[mm$provision=="environmental impact assessment consultation"] <- "public notification and consultation"
mm$provision[mm$provision=="clearing house mechanism"] <- "clearing-house mechanism"
mm$provision[mm$provision=="objectives of CBTT for capacity building"] <- "objectives of CBTT"
mm$provision[mm$provision=="capacity building modalities"] <- "modalities"
mm$provision[mm$provision=="funding of capacity building"] <- "funding"
mm$provision[mm$provision=="modalities"] <- "modalities for CBTT"
mm$provision[mm$provision=="benefit sharing"] <- "sharing of benefits"
mm$provision[mm$provision=="categories of states for capacity building"] <- 'objectives of CBTT'
mm$provision[mm$provision=="process"] <- 'EIA process'
mm$provision[mm$provision=="1"] <- 'clearing-house mechanism'

mm$actor[mm$actor=="TRIBUNAL FOR THE LAW OF THE SEA"] <- "ITLOS"
mm$actor



##### here we do not aggregate the data but take the latest draft text.
mm5 <- mm

mm <- filter(mm5, ((provision != "general") | (!is.na(provision)) | (provision != "-")))



mmv <- as.data.frame(mm)
mmv$actor <- str_to_upper(mmv$actor)
mmv <- filter(mmv, ((provision != "general") | (!is.na(provision)) | (provision != "-")))

crs <- as.data.frame.matrix(mmv)



# save edgelist
colnames(crs) <- c("Source", "Target")
write.csv(crs, "X:/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data/3_edges.csv", row.names = FALSE)



# create network
mmv <- table(mmv$actor, mmv$provision)
mmv <- as.data.frame.matrix(mmv)

comp_net1 <- graph.incidence(mmv)


V(comp_net1)[V(comp_net1)$name %in% crs[,1]]$type <- 0
V(comp_net1)[V(comp_net1)$name %in% crs[,2]]$type <- 1
V(comp_net1)[V(comp_net1)$type == 1]$shape <- "square"
V(comp_net1)[V(comp_net1)$type == 0]$shape <- "circle"
V(comp_net1)$size <- degree(comp_net1)
V(comp_net1)$package <- ifelse(V(comp_net1)$name %in% c("sharing of benefits", "intellectual property rights", "collection of mgrs", "access to tk",
                                                        "application of mgr provision", "mgrs general", "activities with respect to mgrs", "objectives of mgr provision", "access to traditional knowledge", "monitoring and transparency" ), "MGR",
                               ifelse(V(comp_net1)$name %in% c("decision-making for abmts/mpas", "identification of areas", "consultation and assessment", "abmts/mpas general",
                                                               "proposals", "international cooperation and coordination", "objectives of abmt/mpa provision", "monitoring and review"), "ABMT/MPA",
                                      ifelse(V(comp_net1)$name %in% c("eia process", "eias general", "thresholds and criteria for eia" , "seas", "public notification and consultation", "relationship to other processes",
                                                                      "monitoring", "evaluation", "screening", "cumulative impacts", "process", "obligation to conduct eia", "preparation and content of eia", "eia"), "EIA",
                                             ifelse(V(comp_net1)$name %in% c("objectives of cbtmt provision", "modalities for cbtmt", "modalities for cbtmt", "cbtmt general", "thresholds and criteria",
                                                                             "types of cbtmt" , "capacity building modalities", "additional modalities", "cbtmt", "cooperation in cbtmt" ), "cbtmt",
                                                    ifelse(V(comp_net1)$name %in% c("funding", "general objectives", "cop" , "scientific and technical body" ,
                                                                                    "clearing-house mechanism" , "decision-making body",  "international cooperation",
                                                                                    "secretariat", "implementation and compliance", "use of terms", "application", 
                                                                                    "relationship to relevant legal instruments", "review", "crosscutting general", "provisional application",
                                                                                    "relationship to other instruments","settlement of disputes", "general principles and approaches"), "Crosscutting",
                                                           "IO")))))


tibble1 <- comp_net1 %>% as_tbl_graph() %>%
  as_tibble()


table(V(comp_net1)$type)





# save nodelist
colnames(tibble1) <- c("type", "Id", "shape", "size", "package")

write.csv(tibble1, "X:/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data/3_nodes.csv", row.names = FALSE)
