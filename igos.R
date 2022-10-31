##### Arne Langlet, 31.10.2022
## R code to analyse BBNJ data for references to IOs and IO activity

# load packages
library(tidyverse)
library(readxl)
library(dplyr)
library(data.table)
library(zoo)
library(lubridate)
library(openxlsx)
library(stringr)
library(igraph)
#library(dils)
library(ggplot2)
library(rtf)
library(officer)
library(countrycode)
library(roperators)
library(kableExtra)
library(tidyr)
library(flextable)
library(writexl)



# clean working environment
rm(list = ls())

# set working directory
setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/4_final data")
bbnj_full <- read_excel("bbnj_full.xlsx")


# clean data
temp <- bbnj_full
source("//share.univie.ac.at/maripoldata/5_Research/WP1/collected data/3_working data/groups2.r")
bbnj_full <- temp


bbnj_full$observation <- str_to_lower(bbnj_full$observation)
bbnj_full$comment_obs <- str_to_lower(bbnj_full$comment_obs)
bbnj_full$issue_detail <- str_to_lower(bbnj_full$issue_detail)
bbnj_full$option_detail <- str_to_lower(bbnj_full$option_detail)

bbnj_full <- mutate(bbnj_full, alliance =ifelse (actor %in% afr, "African Group",
                                                ifelse (actor %in% clam, "CLAM",
                                                        ifelse (actor %in% cari, "CARICOM",
                                                                 ifelse (actor %in% aos, "AOSIS",
                                                                ifelse (actor %in% piss, "PSIDS",
                                                                        ifelse (actor %in% ldc, "Least Developed",
                                                                                ifelse (actor %in% nun, "Non UNCLOS Party",
                                                                                        ifelse (actor %in% g77, "G77/China",
                                                                                                paste(actor))))))))))










## integrate data collection Excel
setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data")
par <- read_excel("participants_igo-ngo.xlsx")

## replace empy abbr with the full name
par$actor <- ifelse(is.na(par$actor), par$name, par$actor)
## all to lower to allow merge
par$actor <- str_to_lower(par$actor)
par$name <- str_to_lower(par$name)

par$name <- ifelse(is.na(par$name), par$actor, par$name)

          


### check detailed topic 

#bbnj_full <- filter(bbnj_full, author != "SR", !is.na(author))
bbnj_full <- filter(bbnj_full, #negotiation_format != "informal" &
                     side_main != "side")
bbnj_full <- filter(bbnj_full, type_obs <= 4)
bbnj_full <- filter(bbnj_full, IGC %in% 1:5)

igos <- c(IGO, unag, unbodies, unepcon)
igos <- igos[!igos %in% c("secretariat", "chair", "president", "sg", "facilitator")]


#df_total <- as.data.frame(df_total)
#igos <- as.data.frame(igos)


#bbnj_full <- rbind(df_total, igos)


bbnj_full$claim <- bbnj_full$art_title


bbnj_full_states <- filter(bbnj_full, actor_type == "State")

bbnj_full_igos <- filter(bbnj_full, actor_type == "IGO" | actor_type == "UN Body"
                         # | actor_type == "NGO"
                         | actor_type == "UN Agency" | actor_type == "UN Env. Treaty")
bbnj_full_igos <- filter(bbnj_full_igos, actor != "secretariat" &
                           actor != "facilitator" & actor != "president")

bbnj_full_igos <- filter(bbnj_full_igos)



write_xlsx(bbnj_full_igos, "bbnj_full_igos.xlsx")


temp <- bbnj_full_igos
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
bbnj_full_igos <- temp



## reference check section
DT <- as.data.table(bbnj_full_states)

#### create html for qualitative analysis
igos <- c(IGO, unag, unbodies, unepcon)
igos
igos <- igos[!igos %in% c("secretariat", "chair", "president", "sg", "facilitator")]
df_total <- data.frame()
for (i in igos) {df = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
df$io <- c()
df <- df %>% mutate(io = paste0(i))
df_total <- rbind(df_total, df)}
table(df_total$claim, df_total$package_label)

df_total <- filter(df_total, IGC %in% c(1,2,3,4,5))
df_total <- df_total %>% arrange(IGC, date, time) 

temp <- df_total

temp
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
df_total2 <- temp


df_total3 <- df_total[!duplicated(df_total[, c("comment_obs")]),]

setwd("//share.univie.ac.at/maripoldata/5_Research/Publication/In Progress/WP1_Authority in Regime Complex Theory Paper/data")

openxlsx::write.xlsx(df_total, "references1.xlsx")

openxlsx::write.xlsx(df_total2, "references2.xlsx")


openxlsx::write.xlsx(df_total3, "references3.xlsx")




### look for references with the full name
###
n <- par$name
x = c()
DT1 = filter(DT, IGC == 1)
for (i in n){x = c(x, n = count(DT1[like(observation, i)|
                                         like(comment_obs, i) |
                                         like(issue_detail, i) |
                                         like(option_detail, i)]))}
df <- data.frame(reference_IGC1 = matrix(unlist(x), nrow=length(x), byrow=T))
n <- as.list(n)
n <- data.frame(name = matrix((unlist(n))))
n$name <- as.character(n$name)
nr_1 <- cbind(n,df)



x = c()
DT2 = filter(DT, IGC == 2)
for (i in n){x = c(x, n = count(DT2[like(observation, i)|
                                      like(comment_obs, i) |
                                      like(issue_detail, i) |
                                      like(option_detail, i)]))}
df <- data.frame(reference_IGC2 = matrix(unlist(x), nrow=length(x), byrow=T))
n <- as.list(n)
n <- data.frame(name = matrix((unlist(n))))
n$name <- as.character(n$name)
nr_2 <- cbind(n,df)

x = c()
DT3 = filter(DT, IGC == 3)
for (i in n){x = c(x, n = count(DT3[like(observation, i)|
                                      like(comment_obs, i) |
                                      like(issue_detail, i) |
                                      like(option_detail, i)]))}
df <- data.frame(reference_IGC3 = matrix(unlist(x), nrow=length(x), byrow=T))
n <- as.list(n)
n <- data.frame(name = matrix((unlist(n))))
n$name <- as.character(n$name)
nr_3 <- cbind(n,df)
nr_3

x = c()
DT4 = filter(DT, IGC == 4)
for (i in n){x = c(x, n = count(DT4[like(observation, i)|
                                      like(comment_obs, i) |
                                      like(issue_detail, i) |
                                      like(option_detail, i)]))}
df <- data.frame(reference_IGC4 = matrix(unlist(x), nrow=length(x), byrow=T))
n <- as.list(n)
n <- data.frame(name = matrix((unlist(n))))
n$name <- as.character(n$name)
nr_4 <- cbind(n,df)
nr_4


x = c()
DT5 = filter(DT, IGC == 5)
for (i in n){x = c(x, n = count(DT5[like(observation, i)|
                                      like(comment_obs, i) |
                                      like(issue_detail, i) |
                                      like(option_detail, i)]))}
df <- data.frame(reference_IGC5 = matrix(unlist(x), nrow=length(x), byrow=T))
n <- as.list(n)
n <- data.frame(name = matrix((unlist(n))))
n$name <- as.character(n$name)
nr_5 <- cbind(n,df)
nr_5



nr <- cbind(nr_1, nr_2, nr_3, nr_4, nr_5)
nr <- nr[,-5]
nr <- nr[,-3]
nr <- nr[,-5]
nr <- nr[,-6]




######### add the country that made the reference for each reference per IGC 
al <- c(IGO, unbodies, unepcon, unag, NGO, sc)
## IGC 1
df_total = data.frame()
bbnj_full_states1 <- filter(bbnj_full_states, IGC == 1)
for (i in al){ temp <- subset(bbnj_full_states1, grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$option_detail))
country_reference_igc1 = paste0((temp$actor), collapse = ", ")
df <- data.frame(country_reference_igc1)
df_total <- rbind(df_total, df)}

r1 <- cbind(al,df_total)
r1

### IGC 2 
df_total = data.frame()
bbnj_full_states2 <- filter(bbnj_full_states, IGC == 2)
for (i in al){ temp <- subset(bbnj_full_states2, grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$option_detail))
country_reference_igc2 = paste0((temp$actor), collapse = ", ")
df <- data.frame(country_reference_igc2)
df_total <- rbind(df_total, df)}

r2 <- cbind(al,df_total)
r2

### IGC 3 
df_total = data.frame()
bbnj_full_states3 <- filter(bbnj_full_states, IGC == 3)
for (i in al){ temp <- subset(bbnj_full_states3, grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$option_detail))
country_reference_igc3 = paste0((temp$actor), collapse = ", ")
df <- data.frame(country_reference_igc3)
df_total <- rbind(df_total, df)}

r3 <- cbind(al,df_total)
r3

### IGC 4 
df_total = data.frame()
bbnj_full_states4 <- filter(bbnj_full_states, IGC == 4)
for (i in al){ temp <- subset(bbnj_full_states4, grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$option_detail))
country_reference_igc4 = paste0((temp$actor), collapse = ", ")
df <- data.frame(country_reference_igc4)
df_total <- rbind(df_total, df)}

r4 <- cbind(al,df_total)
r4

### IGC 5 
df_total = data.frame()
bbnj_full_states5 <- filter(bbnj_full_states, IGC == 5)

for (i in al){ temp <- subset(bbnj_full_states5, grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$option_detail))
country_reference_igc5 = paste0((temp$actor), collapse = ", ")
df <- data.frame(country_reference_igc5)
df_total <- rbind(df_total, df)}


r5 <- cbind(al,df_total)
r5


r <- cbind(r1, r2, r3, r4, r5)
r <- r[,-5] 
r <- r[,-3]
r <- r[,-5] 
r <- r[,-6] 
r


###################################### add the  ALLIANCES and provisions to each reference for each IGC 
al <- c(IGO, unbodies, NGO, unepcon, unag, sc)

sort(unique(bbnj_full_states$claim))

### IGC 1
df_total = data.frame()
bbnj_full_states1 <- filter(bbnj_full_states, IGC == 1)
for (i in al){ temp <- subset(bbnj_full_states1, grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states1$option_detail))
alliance_reference_igc1 = paste0((temp$alliance), collapse = ", ")
alliance_reference_igc1_package = paste0((temp$package), collapse = ", ")
alliance_reference_igc1_provision = paste0((temp$claim), collapse = ", ")

df <- data.frame(alliance_reference_igc1)
df <- cbind(df, alliance_reference_igc1_package)
df <- cbind(df, alliance_reference_igc1_provision)

df_total <- rbind(df_total, df)}
ali1 <- cbind(al,df_total)
ali1



### IGC 2
df_total = data.frame()
bbnj_full_states2 <- filter(bbnj_full_states, IGC == 2)
for (i in al){ temp <- subset(bbnj_full_states2, grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states2$option_detail))
alliance_reference_igc2 = paste0((temp$alliance), collapse = ", ")
alliance_reference_igc2_package = paste0((temp$package), collapse = ", ")
alliance_reference_igc2_provision = paste0((temp$claim), collapse = ", ")

df <- data.frame(alliance_reference_igc2)
df <- cbind(df, alliance_reference_igc2_package)
df <- cbind(df, alliance_reference_igc2_provision)

df_total <- rbind(df_total, df)}
ali2 <- cbind(al,df_total)
ali2

### IGC 3
df_total = data.frame()
bbnj_full_states3 <- filter(bbnj_full_states, IGC == 3)
for (i in al){ temp <- subset(bbnj_full_states3, grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states3$option_detail))
alliance_reference_igc3 = paste0((temp$alliance), collapse = ", ")
alliance_reference_igc3_package = paste0((temp$package), collapse = ", ")
alliance_reference_igc3_provision = paste0((temp$claim), collapse = ", ")

df <- data.frame(alliance_reference_igc3)
df <- cbind(df, alliance_reference_igc3_package)
df <- cbind(df, alliance_reference_igc3_provision)

df_total <- rbind(df_total, df)}
ali3 <- cbind(al,df_total)
ali3

### IGC 4
df_total = data.frame()
bbnj_full_states4 <- filter(bbnj_full_states, IGC == 4)
for (i in al){ temp <- subset(bbnj_full_states4, grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states4$option_detail))
alliance_reference_igc4 = paste0((temp$alliance), collapse = ", ")
alliance_reference_igc4_package = paste0((temp$package), collapse = ", ")
alliance_reference_igc4_provision = paste0((temp$claim), collapse = ", ")

df <- data.frame(alliance_reference_igc4)
df <- cbind(df, alliance_reference_igc4_package)
df <- cbind(df, alliance_reference_igc4_provision)

df_total <- rbind(df_total, df)}
ali4 <- cbind(al,df_total)
ali4

### IGC 5
df_total = data.frame()
bbnj_full_states5 <- filter(bbnj_full_states, IGC == 5)
for (i in al){ temp <- subset(bbnj_full_states5, grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states5$option_detail))
alliance_reference_igc5 = paste0((temp$alliance), collapse = ", ")
alliance_reference_igc5_package = paste0((temp$package), collapse = ", ")
alliance_reference_igc5_provision = paste0((temp$claim), collapse = ", ")

df <- data.frame(alliance_reference_igc5)
df <- cbind(df, alliance_reference_igc5_package)
df <- cbind(df, alliance_reference_igc5_provision)

df_total <- rbind(df_total, df)}
ali5 <- cbind(al,df_total)
ali5




ali <- cbind(ali1, ali2, ali3, ali4, ali5)
ali <- ali[,-9]
ali <- ali[,-5]
ali <- ali[,-11]
ali <- ali[,-14]





### search for references 
### with abbreviations
### for each type of IO
IGO <- str_to_lower(IGO)
MGR = c()
ABMT= c()
EIA = c()
CBTT = c()
crosscutting = c()
other = c()


for (i in IGO){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
MGR = c(MGR, nrow(temp[temp$package == 1,]))
ABMT = c(ABMT, nrow(temp[temp$package == 2,]))
EIA = c(EIA, nrow(temp[temp$package == 3,]))
CBTT = c(CBTT, nrow(temp[temp$package == 4,]))
crosscutting = c(crosscutting, nrow(temp[temp$package == 5,]))
other = c(other, nrow(temp[temp$package == 9,]))}

df <- cbind(MGR, ABMT, EIA, CBTT, crosscutting, other)
i <- as.list(IGO)
i <- data.frame(matrix(unlist(i), nrow=length(i), byrow=T)) 
s <- cbind(IGO,df)
s

### by IGC
ref_igc1 = c()
ref_igc2= c()
ref_igc3 = c()
ref_igc4 = c()
ref_igc5 = c()

for (i in IGO){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
ref_igc1 = c(ref_igc1, nrow(temp[temp$IGC == 1,]))
ref_igc2 = c(ref_igc2, nrow(temp[temp$IGC == 2,]))
ref_igc3 = c(ref_igc3, nrow(temp[temp$IGC == 3,]))
ref_igc4 = c(ref_igc4, nrow(temp[temp$IGC == 4,]))
ref_igc5 = c(ref_igc5, nrow(temp[temp$IGC == 5,]))
}
dfi <- cbind(ref_igc1, ref_igc2, ref_igc3, ref_igc4, ref_igc5)
si <- cbind(IGO,dfi)



###
unbodies <- str_to_lower(unbodies)
MGR = c()
ABMT= c()
EIA = c()
CBTT = c()
crosscutting = c()
other = c()
for (i in unbodies){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                                    grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                                    grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                                    grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
                                         source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
MGR = c(MGR, nrow(temp[temp$package == 1,]))
ABMT = c(ABMT, nrow(temp[temp$package == 2,]))
EIA = c(EIA, nrow(temp[temp$package == 3,]))
CBTT = c(CBTT, nrow(temp[temp$package == 4,]))
crosscutting = c(crosscutting, nrow(temp[temp$package == 5,]))
other = c(other, nrow(temp[temp$package == 9,]))}

df <- cbind(MGR, ABMT, EIA, CBTT, crosscutting, other)
i <- as.list(unbodies)
i <- data.frame(matrix(unlist(i), nrow=length(i), byrow=T)) 
t <- cbind(unbodies,df)
t


### by IGC
ref_igc1 = c()
ref_igc2= c()
ref_igc3 = c()
ref_igc4 = c()
ref_igc5 = c()

for (i in unbodies){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
ref_igc1 = c(ref_igc1, nrow(temp[temp$IGC == 1,]))
ref_igc2 = c(ref_igc2, nrow(temp[temp$IGC == 2,]))
ref_igc3 = c(ref_igc3, nrow(temp[temp$IGC == 3,]))
ref_igc4 = c(ref_igc4, nrow(temp[temp$IGC == 4,]))
ref_igc5 = c(ref_igc5, nrow(temp[temp$IGC == 5,]))
}
dfi <- cbind(ref_igc1, ref_igc2, ref_igc3, ref_igc4, ref_igc5)
ti <- cbind(unbodies,dfi)
ti


###
NGO <- str_to_lower(NGO)
MGR = c()
ABMT= c()
EIA = c()
CBTT = c()
crosscutting = c()
other = c()
for (i in NGO){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
MGR = c(MGR, nrow(temp[temp$package == 1,]))
ABMT = c(ABMT, nrow(temp[temp$package == 2,]))
EIA = c(EIA, nrow(temp[temp$package == 3,]))
CBTT = c(CBTT, nrow(temp[temp$package == 4,]))
crosscutting = c(crosscutting, nrow(temp[temp$package == 5,]))
other = c(other, nrow(temp[temp$package == 9,]))}

df <- cbind(MGR, ABMT, EIA, CBTT, crosscutting, other)
i <- as.list(NGO)
i <- data.frame(matrix(unlist(i), nrow=length(i), byrow=T)) 
p <- cbind(NGO,df)

### by IGC
ref_igc1 = c()
ref_igc2= c()
ref_igc3 = c()
ref_igc4 = c()
ref_igc5 = c()


for (i in NGO){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                                    grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                                    grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                                    grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
ref_igc1 = c(ref_igc1, nrow(temp[temp$IGC == 1,]))
ref_igc2 = c(ref_igc2, nrow(temp[temp$IGC == 2,]))
ref_igc3 = c(ref_igc3, nrow(temp[temp$IGC == 3,]))
ref_igc4 = c(ref_igc4, nrow(temp[temp$IGC == 4,]))
ref_igc5 = c(ref_igc5, nrow(temp[temp$IGC == 5,]))
}
dfi <- cbind(ref_igc1, ref_igc2, ref_igc3, ref_igc4, ref_igc5)
pi <- cbind(NGO,dfi)
pi

###
unag <- str_to_lower(unag)
MGR = c()
ABMT= c()
EIA = c()
CBTT = c()
crosscutting = c()
other = c()
for (i in unag){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
MGR = c(MGR, nrow(temp[temp$package == 1,]))
ABMT = c(ABMT, nrow(temp[temp$package == 2,]))
EIA = c(EIA, nrow(temp[temp$package == 3,]))
CBTT = c(CBTT, nrow(temp[temp$package == 4,]))
crosscutting = c(crosscutting, nrow(temp[temp$package == 5,]))
other = c(other, nrow(temp[temp$package == 9,]))}

df <- cbind(MGR, ABMT, EIA, CBTT, crosscutting, other)
i <- as.list(unag)
i <- data.frame(matrix(unlist(i), nrow=length(i), byrow=T)) 
u <- cbind(unag,df)
u

### by IGC
ref_igc1 = c()
ref_igc2= c()
ref_igc3 = c()
ref_igc4 = c()
ref_igc5 = c()


for (i in unag){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                               grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
ref_igc1 = c(ref_igc1, nrow(temp[temp$IGC == 1,]))
ref_igc2 = c(ref_igc2, nrow(temp[temp$IGC == 2,]))
ref_igc3 = c(ref_igc3, nrow(temp[temp$IGC == 3,]))
ref_igc4 = c(ref_igc4, nrow(temp[temp$IGC == 4,]))
ref_igc5 = c(ref_igc5, nrow(temp[temp$IGC == 5,]))
}
dfi <- cbind(ref_igc1, ref_igc2, ref_igc3, ref_igc4, ref_igc5)
ui <- cbind(unag,dfi)
ui



###
unepcon <- str_to_lower(unepcon)
MGR = c()
ABMT= c()
EIA = c()
CBTT = c()
crosscutting = c()
other = c()
for (i in unepcon){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
MGR = c(MGR, nrow(temp[temp$package == 1,]))
ABMT = c(ABMT, nrow(temp[temp$package == 2,]))
EIA = c(EIA, nrow(temp[temp$package == 3,]))
CBTT = c(CBTT, nrow(temp[temp$package == 4,]))
crosscutting = c(crosscutting, nrow(temp[temp$package == 5,]))
other = c(other, nrow(temp[temp$package == 9,]))}

df <- cbind(MGR, ABMT, EIA, CBTT, crosscutting, other) 
i <- as.list(unepcon)
i <- data.frame(matrix(unlist(i), nrow=length(i), byrow=T)) 
o <- cbind(unepcon,df)
o


### by IGC
ref_igc1 = c()
ref_igc2= c()
ref_igc3 = c()
ref_igc4 = c()
ref_igc5 = c()

for (i in unepcon){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                                grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
ref_igc1 = c(ref_igc1, nrow(temp[temp$IGC == 1,]))
ref_igc2 = c(ref_igc2, nrow(temp[temp$IGC == 2,]))
ref_igc3 = c(ref_igc3, nrow(temp[temp$IGC == 3,]))
ref_igc4 = c(ref_igc4, nrow(temp[temp$IGC == 4,]))
ref_igc5 = c(ref_igc5, nrow(temp[temp$IGC == 5,]))

}
dfi <- cbind(ref_igc1, ref_igc2, ref_igc3, ref_igc4, ref_igc5)
oi <- cbind(unepcon,dfi)
oi

###
sc <- str_to_lower(sc)
MGR = c()
ABMT= c()
EIA = c()
CBTT = c()
crosscutting = c()
other = c()
for (i in sc){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                              grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
MGR = c(MGR, nrow(temp[temp$package == 1,]))
ABMT = c(ABMT, nrow(temp[temp$package == 2,]))
EIA = c(EIA, nrow(temp[temp$package == 3,]))
CBTT = c(CBTT, nrow(temp[temp$package == 4,]))
crosscutting = c(crosscutting, nrow(temp[temp$package == 5,]))
other = c(other, nrow(temp[temp$package == 9,]))}

df <- cbind(MGR, ABMT, EIA, CBTT, crosscutting, other) 
i <- as.list(sc)
i <- data.frame(matrix(unlist(i), nrow=length(i), byrow=T)) 
c <- cbind(sc,df)
c


### by IGC
ref_igc1 = c()
ref_igc2= c()
ref_igc3 = c()
ref_igc4 = c()
ref_igc5 = c()
for (i in sc){temp = subset(bbnj_full_states, grepl(paste0("\\<",i,"\\>"), bbnj_full_states$comment_obs)|
                                   grepl(paste0("\\<",i,"\\>"), bbnj_full_states$observation)|
                                   grepl(paste0("\\<",i,"\\>"), bbnj_full_states$issue_detail)|
                                   grepl(paste0("\\<",i,"\\>"), bbnj_full_states$option_detail))
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
ref_igc1 = c(ref_igc1, nrow(temp[temp$IGC == 1,]))
ref_igc2 = c(ref_igc2, nrow(temp[temp$IGC == 2,]))
ref_igc3 = c(ref_igc3, nrow(temp[temp$IGC == 3,]))
ref_igc4 = c(ref_igc4, nrow(temp[temp$IGC == 5,]))
ref_igc5 = c(ref_igc5, nrow(temp[temp$IGC == 5,]))

}
df <- cbind(ref_igc1, ref_igc2, ref_igc3, ref_igc4, ref_igc5)
ci <- cbind(sc,df)
ci



colnames(s)[1] <- "IGO"
colnames(t)[1] <- "IGO"
colnames(p)[1] <- "IGO"
colnames(c)[1] <- "IGO"
colnames(u)[1] <- "IGO"
colnames(o)[1] <- "IGO"
colnames(si)[1] <- "IGO"
si <- si[,-1]
colnames(ti)[1] <- "IGO"
ti <- ti[,-1]
colnames(pi)[1] <- "IGO"
pi <- pi[,-1]
colnames(ci)[1] <- "IGO"
ci <- ci[,-1]
colnames(ui)[1] <- "IGO"
ui <- ui[,-1]
colnames(oi)[1] <- "IGO"
oi <- oi[,-1]

ssi <- cbind(s, si)
tti <- cbind(t, ti)
ppi <- cbind(p, pi)
cci <- cbind(c, ci)
uui <- cbind(u, ui)
ooi <- cbind(o, oi)


ref <- rbind(ssi,tti,ppi,cci,uui,ooi)
ref[is.na(ref)] <- 0

ref <- merge(ref, r, by.x = "IGO", by.y="al")

ref <- merge(ref, ali, by.x = "IGO", by.y = "al")
ref$IGO <- as.character(ref$IGO)
ref$IGO[ref$IGO == "unclos"] <- "undoalos" 

### gather statements for qual. analyis
df_total <- data.frame()
n <- n[!n %in% c("high seas alliance")]

for (i in n) {df = DT[like(observation, i) |
                          like(comment_obs, i)|
                          like(issue_detail, i)|
                          like(option_detail, i)]
df_total <- rbind(df_total, df)
temp <- df_total
source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
#assign(paste("name", i, sep = "_"), temp)
df_totall <- temp
}



###### next section: activity check of IOs 


####### check activity per provision
df <- data.frame()
df_total <- data.frame()
bbnj_full1 <- filter(bbnj_full_igos, IGC == 1 & side_main == "main")
for (i in igos) {temp = subset(bbnj_full1, bbnj_full1$actor == i)
#source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
igo_activity_igc1_package = paste0((temp$package), collapse = ", ")
igo_activity_igc1_provision = paste0((temp$claim), collapse = ", ")
df <- data.frame(igo_activity_igc1_package)
df <- cbind(df, igo_activity_igc1_provision)
df_total <- rbind(df_total, df)}
act1 <- cbind(igos,df_total)




df <- data.frame()
df_total <- data.frame()
bbnj_full2 <- filter(bbnj_full_igos, IGC == 2)
for (i in igos) {temp = subset(bbnj_full2, bbnj_full2$actor == i)
#source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
igo_activity_igc2_package = paste0((temp$package), collapse = ", ")
igo_activity_igc2_provision = paste0((temp$claim), collapse = ", ")
df <- data.frame(igo_activity_igc2_package)
df <- cbind(df, igo_activity_igc2_provision)
df_total <- rbind(df_total, df)}
act2 <- cbind(igos ,df_total)

df <- data.frame()
df_total <- data.frame()
bbnj_full3 <- filter(bbnj_full_igos, IGC == 3)
for (i in igos) {temp = subset(bbnj_full3, bbnj_full3$actor == i)
#source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/double_clean.r")
igo_activity_igc3_package = paste0((temp$package), collapse = ", ")
igo_activity_igc3_provision = paste0((temp$claim), collapse = ", ")
df <- data.frame(igo_activity_igc3_package)
df <- cbind(df, igo_activity_igc3_provision)
df_total <- rbind(df_total, df)}
act3 <- cbind(igos ,df_total)

df <- data.frame()
df_total <- data.frame()
bbnj_full4 <- filter(bbnj_full_igos, IGC == 4)
for (i in igos) {temp = subset(bbnj_full4, bbnj_full4$actor == i)
#source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/4_working data/double_clean.r")
igo_activity_igc4_package = paste0((temp$package), collapse = ", ")
igo_activity_igc4_provision = paste0((temp$claim), collapse = ", ")
df <- data.frame(igo_activity_igc4_package)
df <- cbind(df, igo_activity_igc4_provision)
df_total <- rbind(df_total, df)}
act4 <- cbind(igos ,df_total)

df <- data.frame()
df_total <- data.frame()
bbnj_full5 <- filter(bbnj_full_igos, IGC == 5)
for (i in igos) {temp = subset(bbnj_full5, bbnj_full5$actor == i)
#source("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/5_working data/double_clean.r")
igo_activity_igc5_package = paste0((temp$package), collapse = ", ")
igo_activity_igc5_provision = paste0((temp$claim), collapse = ", ")
df <- data.frame(igo_activity_igc5_package)
df <- cbind(df, igo_activity_igc5_provision)
df_total <- rbind(df_total, df)}
act5 <- cbind(igos ,df_total)

act <- cbind(act1, act2, act3, act4, act5)
act <- act[,-13]
act <- act[,-10]

act <- act[,-7]
act <- act[,-4]

act
                 


###### activity check
### for qualitative analysis 



igoss <- ungroup(bbnj_full_igos)
igoss <- select(igoss, IGC, actor, actor_type,
                            time, date, package, package_label, claim,
                            observation, comment_obs, section_title,
                            subsection, issue_detail, option_detail, pro_contra, type_obs, side_main)

#igos <- igos[order(igos$unique_time),]
igoss <- unique(igoss)
head(igoss)
table(igoss$claim, igoss$package_label) 

dub <- filter(igoss, claim == "no dublication") %>% select(comment_obs)
dub

igoclaim <- igoss
a <- select(igoss, actor, claim, package, package_label)

## df_total is empy
#b <- select(df_total, actor = io, claim, package, package_label)

#comp <- rbind(a,b)
comp <- a
sort(unique(comp$claim))

setwd("//share.univie.ac.at/maripoldata/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data")

write.xlsx(comp, file ="comp.xlsx")


# 
# table(df_total$claim, df_total$package_label) %>%
#   kable() %>% 
#   kable_styling(bootstrap_options = c("bordered", "hover"), full_width = F) %>%
#   save_kable(file = paste0("overview_claims_packages.html"))
###

# table(comp$claim, comp$package_label) %>%
#      kable() %>% 
#      kable_styling(bootstrap_options = c("bordered", "hover"), full_width = F) %>%
#      save_kable(file = paste0("overview_claims_packages.html"))





bbnj_full_igos <- filter(bbnj_full_igos, side_main != "side")
bbnj_full_igos <- filter(bbnj_full_igos, type_obs != 5 | type_obs != 9)

bbnj_full_igos %>% filter(actor == "fao") %>% select(IGC)

org <- bbnj_full_igos %>% group_by(actor) %>% summarise(total_plenary = sum(side_main == "main"), 
                                              activity_IGC1=sum(IGC==1),      
                                       activity_IGC2=sum(IGC==2),
                                       activity_IGC3=sum(IGC==3),
                                       activity_IGC4=sum(IGC==4),
                                       activity_IGC5=sum(IGC==5),
                                       
                                       activity_IGC1_MGR = sum(package ==1 & IGC == 1),
                                       activity_IGC2_MGR = sum(package ==1 & IGC == 2),
                                       activity_IGC3_MGR = sum(package ==1 & IGC == 3),
                                       activity_IGC4_MGR = sum(package ==1 & IGC == 4),
                                       activity_IGC5_MGR = sum(package ==1 & IGC == 5),
                                       
                                       activity_IGC1_ABMT = sum (package == 2 & IGC == 1),
                                       activity_IGC2_ABMT = sum (package == 2 & IGC == 2),
                                       activity_IGC3_ABMT = sum (package == 2 & IGC == 3),
                                       activity_IGC4_ABMT = sum (package == 2 & IGC == 4),
                                       activity_IGC5_ABMT = sum (package == 2 & IGC == 5),
                                       
                                       activity_IGC1_EIA = sum (package == 3 & IGC == 1),
                                       activity_IGC2_EIA = sum (package == 3 & IGC == 2), 
                                       activity_IGC3_EIA = sum (package == 3 & IGC == 3),
                                       activity_IGC4_EIA = sum (package == 3 & IGC == 4,
                                       activity_IGC5_EIA = sum (package == 3 & IGC == 5),
                                       
                                       activity_IGC1_CBTT = sum (package == 4 & IGC == 1),
                                       activity_IGC2_CBTT = sum (package == 4 & IGC == 2),
                                       activity_IGC3_CBTT = sum (package == 4 & IGC == 3),
                                       activity_IGC4_CBTT = sum (package == 4 & IGC == 4),
                                       activity_IGC5_CBTT = sum (package == 4 & IGC == 5),
                                       
                                       activity_IGC1_crosscutting = sum (package == 5 & IGC == 1),
                                       activity_IGC2_crosscutting = sum (package == 5 & IGC == 2),
                                       activity_IGC3_crosscutting = sum (package == 5 & IGC == 3),
                                       activity_IGC4_crosscutting = sum (package == 5 & IGC == 4),
                                       activity_IGC5_crosscutting = sum (package == 5 & IGC == 5),
                                       
                                       activtiy_other = sum (package == 9)))




org[is.na(org)] <- 0

org$actor <- str_to_lower(org$actor)
org$actor[org$actor == "ioc-unesco"] <- "ioc"                    

par$participants_IGC1[is.na(par$participants_IGC1)] <- "0"
par$participants_IGC2[is.na(par$participants_IGC2)] <- "0"
par$participants_IGC3[is.na(par$participants_IGC3)] <- "0"
par$participants_IGC4[is.na(par$participants_IGC4)] <- "0"
par$participants_IGC4[is.na(par$participants_IGC5)] <- "0"

org <- merge(org, act, by.x = "actor", by.y = "igos", all.x = TRUE, all.y = TRUE)

x <- merge(org, ref, by.x="actor", by.y = "IGO", all.x = TRUE, all.y = TRUE)


x$MGR <- as.numeric(paste(x$MGR))
x$ABMT <- as.numeric(paste(x$ABMT))
x$EIA <- as.numeric(paste(x$EIA))
x$CBTT <- as.numeric(paste(x$CBTT))
x$crosscutting <- as.numeric(paste(x$crosscutting))
x$other <- as.numeric(paste(x$other))

x <- mutate(x, reference = MGR + ABMT + EIA + CBTT + crosscutting + other)


organizations <- merge(x, par, by = "actor", all.x = TRUE, all.y = TRUE)


nr

organizations <- merge(organizations, nr, by = "name", all.x = TRUE, all.y = TRUE)



#### references by Abbreviation and full name are being added up 
organizations$reference_IGC1[is.na(organizations$reference_IGC1)] <- 0
organizations$reference_IGC2[is.na(organizations$reference_IGC2)] <- 0
organizations$reference_IGC3[is.na(organizations$reference_IGC3)] <- 0
organizations$reference_IGC4[is.na(organizations$reference_IGC4)] <- 0
organizations$reference_IGC5[is.na(organizations$reference_IGC4)] <- 0

organizations$ref_igc1[is.na(organizations$ref_igc1)] <- 0
organizations$ref_igc2[is.na(organizations$ref_igc2)] <- 0
organizations$ref_igc3[is.na(organizations$ref_igc3)] <- 0
organizations$ref_igc4[is.na(organizations$ref_igc4)] <- 0
organizations$ref_igc5[is.na(organizations$ref_igc5)] <- 0


organizations$ref_igc1 <- as.numeric(organizations$ref_igc1)
organizations$ref_igc2 <- as.numeric(organizations$ref_igc2)
organizations$ref_igc3 <- as.numeric(organizations$ref_igc3)
organizations$ref_igc4 <- as.numeric(organizations$ref_igc4)
organizations$ref_igc5 <- as.numeric(organizations$ref_igc5)


#organizations$reference.x <- as.integer(organizations$reference.x)
#organizations$reference.y <- as.integer(organizations$reference.y)

organizations <- mutate(organizations, ref_igc1 = reference_IGC1 + ref_igc1)
organizations <- mutate(organizations, ref_igc2 = reference_IGC2 + ref_igc2)
organizations <- mutate(organizations, ref_igc3 = reference_IGC3 + ref_igc3)
organizations <- mutate(organizations, ref_igc4 = reference_IGC4 + ref_igc4)
organizations <- mutate(organizations, ref_igc5 = reference_IGC5 + ref_igc5)


organizations <- subset(organizations, select = -c(reference_IGC1, reference_IGC2, reference_IGC3, reference_IGC4, reference_IGC5))


organizations <- mutate(organizations, type =ifelse (actor %in% Country, "State",
                                         ifelse (actor %in% NGO, "NGO",
                                                 ifelse (actor %in% IGO, "IGO",
                                                         ifelse (actor %in% unbodies, "UN Body",
                                                                 ifelse (actor %in% sc, "Scientific Institute",
                                                                         ifelse (actor %in% unalliance, "UN Alliance",
                                                                                 "other")))))))

organizations$activity_IGC2[is.na(organizations$activity_IGC2)] <- 0
organizations$activity_IGC2[is.na(organizations$activity_IGC2)] <- 0
organizations$activity_IGC3[is.na(organizations$activity_IGC3)] <- 0
organizations$activity_IGC4[is.na(organizations$activity_IGC4)] <- 0



organizations$total_plenary[is.na(organizations$total_plenary)] <- 0
organizations$participants_IGC1[is.na(organizations$participants_IGC1)] <- 0
organizations$participants_IGC2[is.na(organizations$participants_IGC2)] <- 0
organizations$participants_IGC3[is.na(organizations$participants_IGC3)] <- 0
organizations$participants_IGC4[is.na(organizations$participants_IGC4)] <- 0

#organizations$total_reference[is.na(organizations$total_reference)] <- 0

organizations$participants_IGC1 <- as.numeric(organizations$participants_IGC1) 
organizations$participants_IGC2 <- as.numeric(organizations$participants_IGC2) 
organizations$participants_IGC3 <- as.numeric(organizations$participants_IGC3) 
organizations$participants_IGC4 <- as.numeric(organizations$participants_IGC4) 

organizations <- mutate(organizations, total_participants = (participants_IGC2 + participants_IGC3 + participants_IGC1 + participants_IGC4))

typeof(organizations$participants_IGC3)


### next section: side events organized by IOs 

##### 
setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/Side Events/IGC1")
igc1 <- read_excel("IGC1_events.xlsx", sheet = 2)
setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/Side Events/IGC2")
igc2 <- read_excel("side_events.xlsx")
setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/Side Events/IGC3")
igc3 <- read_excel("igc3_events.xlsx", sheet = 2)

side_events <- rbind(igc1, igc2, igc3)
side_events$Host1 <- str_to_lower(side_events$Host1)
side_events$Host2 <- str_to_lower(side_events$Host2)
side_events$Host3 <- str_to_lower(side_events$Host3)
side_events$Host3 <- str_to_lower(side_events$Host3)
side_events$Host4 <- str_to_lower(side_events$Host4)


hosts <- select(side_events, Host1, Host2, Host3, Host4, Host5)
hosts$Host1[hosts$Host1 == "ioc-unesco"] <- "ioc"
hosts$Host2[hosts$Host2 == "ioc-unesco"] <- "ioc"
hosts$Host3[hosts$Host3 == "ioc-unesco"] <- "ioc"
hosts$Host4[hosts$Host4 == "ioc-unesco"] <- "ioc"
hosts$Host5[hosts$Host5 == "ioc-unesco"] <- "ioc"

hosts$Host1[hosts$Host1 == "unep-wcmc"] <- "unep" 
hosts$Host2[hosts$Host2 == "unep-wcmc"] <- "unep" 
hosts$Host3[hosts$Host3 == "unep-wcmc"] <- "unep" 
hosts$Host4[hosts$Host4 == "unep-wcmc"] <- "unep" 
hosts$Host5[hosts$Host5 == "unep-wcmc"] <- "unep" 

side <- as.data.frame((table(unlist(hosts))))
side
names(side)[names(side) == "Var1"] <- "actor"
names(side)[names(side) == "Freq"] <- "host_side_event"
side$actor <- str_to_lower(side$actor)
                   


###### next section: IOs mentions in draft text
#### draft mentions
organizations$mentions_draft2[is.na(organizations$mentions_draft1)] <- 0
organizations$mentions_draft2[is.na(organizations$mentions_draft2)] <- 0
organizations$mentions_draft3[is.na(organizations$mentions_draft3)] <- 0
organizations$mentions_draft4[is.na(organizations$mentions_draft4)] <- 0
organizations$mentions_draft5[is.na(organizations$mentions_draft5)] <- 0

organizations$mentions_draft2 <- as.numeric(organizations$mentions_draft2)
organizations$mentions_draft3 <- as.numeric(organizations$mentions_draft3)
organizations$mentions_draft4 <- as.numeric(organizations$mentions_draft4)
organizations$mentions_draft5 <- as.numeric(organizations$mentions_draft5)

organizations <- mutate(organizations, total_draft_mentions = (mentions_draft2 + mentions_draft3 + mentions_draft4 + mentions_draft5))
organizations$total_draft_mentions <- na_if(organizations$total_draft_mentions, 0)

### merge

side
#organizations_full <- merge(organizations, side, by.x = "name", by.y = "actor", all.x = TRUE, all.y = TRUE)
organizations$name <- ifelse(is.na(organizations$name), organizations$actor, organizations$name)
organizations_full <- merge(organizations, side, by = "actor", all.x = TRUE)
organizations_full$host_side_event
organizations_full$host_side_event[is.na(organizations_full$host_side_event)] <- 0

organizations_full <- mutate(organizations_full, type =ifelse (actor %in% Country, "State",
                                         ifelse (actor %in% NGO, "NGO",
                                                 ifelse (actor %in% IGO, "IGO",
                                                         ifelse (actor %in% unbodies, "UN Body",
                                                                 ifelse (actor %in% unag, "UN Agency",
                                                                 ifelse (actor %in% sc, "Scientific Institute",
                                                                         ifelse (actor %in% unalliance, "UN Alliance",
                                                                                 ifelse (actor %in% business, "business",
                                                                                         ifelse(actor %in% unepcon, "UNEP Convention",
                                                                                         registered_as))))))))))


organizations_full$actor[organizations_full$actor == "barcelona"] <- "barcelona convention" 
organizations_full$actor[organizations_full$actor=="TRIBUNAL FOR THE LAW OF THE SEA"] <- "ITLOS"

table(organizations_full$type)  

igos_ngos <- filter(organizations_full, type =="NGO" | type == "IGO" | type == "UN Agency" | type == "UN Body" | type == "UNEP Convention")

setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data")

write.xlsx(organizations_full, file = "organizations_full.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)

write.xlsx(igos_ngos, file = "igos_ngos.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)


setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/4_final data")

igos_ngos <- subset(igos_ngos, select = -c(focus, focus2, focus3, focus4))

write.xlsx(igos_ngos, file = "igos_ngos.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)

names(igos_ngos)

types <- select(organizations_full, actor, name, type, registered_as)

## select the organizations you want
organizations <- filter(organizations_full, type == "IGO" | type== "UN Body" 
                        | type == "UN Agency" | registered_as == "IGO"
                        | registered_as == "UN body" | type == "UNEP Convention" 
                        | (is.na(type)))

#organizations <- filter(organizations_full, type != "NGO" & type != "Scientific Institute" & type != "business")
organizations$actor
organizations$actor <- str_to_upper(organizations$actor)
organizations <- filter(organizations, actor != "CHAIR",
                  actor != "PRESIDENT",  actor != "SECRETARIAT",
                  actor != "SG", actor != "FACILITATOR")

### fill NAs with 0
organizations$host_side_event[is.na(organizations$host_side_event)] <- 0
organizations$MGR <- as.integer(paste(organizations$MGR))
organizations$ABMT<- as.integer(paste(organizations$ABMT))
organizations$EIA<- as.integer(paste(organizations$EIA))
organizations$CBTT <- as.integer(paste(organizations$CBTT))
organizations$crosscutting<- as.integer(paste(organizations$crosscutting))
organizations$other<- as.integer(paste(organizations$other))

organizations <- mutate(organizations, total_reference = MGR + ABMT + EIA + CBTT + crosscutting + other)
organizations$total_reference
organizations$actor
#organizations$reference[is.na(organizations$reference)] <- 0
class(organizations$MGR)



### keep organizations  for regime complex
organizations <- subset(organizations, host_side_event >= 1 | 
                               total_draft_mentions >= 1 |
                               total_reference >= 1 |
                               total_plenary >= 1 |
                               total_participants >= 1)



### fill in RMFO data 
# 
# rfmos <- c("iccat", "iattc","ccamlr", "NEAFC", "nafo", "NPFC", "WCPFC", "SEAFO",
#            "APFIC", "GLFC")
# 
# rfmo <- organizations %>% filter(actor =="RFMO")



# x <- organizations$r
# 
# x <- as.character(x)
# y = unname(sapply(x, function(x) {
#   paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse=',')} ))
# organizations$reference_states <- y

x <- organizations$ali
x <- as.character(x)
y = unname(sapply(x, function(x) {
  paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse=',')} ))

#organizations$reference_alliance <- y

na <- filter(organizations, is.na(type))




organizations <- mutate(organizations, topic = ifelse(focus == "law of the sea" | focus == "international law" | focus == "regional" | focus == "security", "legal and security",
                                                ifelse(focus == "sustainable use" | focus == "environmental protection" | focus == "climate" | focus == "conservation" | focus == "pollution", "ecological",
                                                       ifelse(focus == "scientific cooperation" | focus == "development"| focus == "sustainable development" | focus == "capacity building" | focus == "education", "capacity and development",
                                                              ifelse(focus == "health" | focus == "safety" | focus == "food security" | focus == "fisheries" | focus == "human well-being", "human dimension",
                                                                     ifelse(focus == "trade" | focus == "intellectual property" | focus == "sharing of benefits" | focus == "equity" | focus == "economic development" , "economic",
                                                                            0))))))
organizations$country_reference_igc1

z <- select(organizations, actor, alliance_reference_igc1, alliance_reference_igc2, alliance_reference_igc3, topic)
x <- select(organizations, actor, country_reference_igc1,country_reference_igc2,country_reference_igc3, topic)




organizations$actor <- str_to_upper(organizations$actor)



setwd("//share.univie.ac.at/maripoldata/5_Research/Publication/In Progress/WP1_Marine Biodiversity Regime Complex Paper/Data")

read_docx() %>%  # a new, empty document
  body_add_table(x, style = "table_template") %>%
  print(target="regime_complex_ref.docx")

read_docx() %>%  # a new, empty document
  body_add_table(z, style = "table_template") %>%
  print(target="regime_complex_ref_alliance.docx")


### name - abbr 
n <- select(organizations, actor, name)
read_docx() %>%  # a new, empty document
  body_add_table(n, style = "table_template") %>%
  print(target="name.docx")



t <- select(organizations, actor, focus, focus2, focus3, focus4)
t$focus2[is.na(t$focus2)] <- ""
t$focus3[is.na(t$focus3)] <- ""
t$focus4[is.na(t$focus4)] <- ""

read_docx() %>%  # a new, empty document
  body_add_table(t, style = "table_template") %>%
  print(target="topics.docx")


#### network part 
complex <- filter(organizations, actor != "DESA", actor != "CHAIR",
                  actor != "PRESIDENT", actor != "SECRETARIAT",
                  actor != "UNSD", actor != "SG", actor != "FACILITATOR",
                  actor != "RFMO")

y <- select(complex, actor, name, total_participants, total_draft_mentions, 
            total_plenary, total_reference, host_side_event)
y[is.na(y)] <- ""
y$total_participants[y$total_participants == 0] <- ""
y$total_draft_mentions[y$total_draft_mentions == 0] <- ""
y$total_plenary[y$total_plenary == 0] <- ""
y$total_reference[y$total_reference == 0] <- ""
y$host_side_event[y$host_side_event == 0] <- ""
read_docx() %>%  # a new, empty document
  body_add_table(y, style = "table_template") %>%
  print(target="overview_regime_complex_abc.docx")

write.xlsx(y, file = "y.xlsx")

#complex[is.na(complex)] <- 0
complex <- mutate(complex, focus_group = ifelse(focus == "international law" | focus == "regional"| focus == "intellectual property"  | focus == "integration"| focus == "security", "legal and security",
                                                ifelse(focus == "sustainability" | focus == "environmental protection" | focus == "climate" | focus == "conservation" | focus == "pollution", "ecological",
                                                       ifelse(focus == "scientific cooperation" | focus == "sustainable development" | focus == "development" | focus == "capacity building" | focus == "education", "capacity and development",
                                                              ifelse(focus == "health" | focus == "safety" | focus == "food security" | focus == "fisheries" | focus == "human well-being", "human dimension",
                                                                     ifelse( focus == "sustainable use" | focus == "trade" | focus == "sharing of benefits" | focus == "equity" | focus == "economic development", "economic",
                                                                             0))))))



## mainstream some focusses to make graph look better
complex$focus[complex$focus=="science"] <- 'scientific cooperation'
#complex$focus[complex$focus=="sustainability"] <- 'sustainable use'
#complex$focus[complex$focus=="economic development"] <- 'development'
#complex$focus[complex$focus=="sustainable development"] <- 'development'
complex$focus[complex$focus=="environment protection"] <- 'environmental protection'
complex$focus[complex$focus=="law of the sea"] <- 'international law'
#complex$focus[complex$focus=="food security"] <- 'fisheries'

write.xlsx(complex, file = "complex.xlsx")
write.csv(complex, file = "complex.csv")


read_docx() %>%  # a new, empty document
  body_add_table(complex, style = "table_template") %>%
  print(target="complex.docx")



# 
