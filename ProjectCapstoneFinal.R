
#  Title: 'Predict Probability of Sales on Inbound Call Center Traffic'

library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
set.seed(1234)

# Set locale
Sys.setlocale(locale="C")

# Read in data for May 2016
calls.may <- read.csv("CallsDataMay2016.csv", header = TRUE, stringsAsFactors = FALSE)

# Select only connected calls
calls.may <- calls.may[calls.may$Outcome=="CONNECT",]


## Data Cleaning

# Cleaning BTN field
calls.may <- calls.may[which(calls.may$BTN != "Unavailable"),]
calls.may <- calls.may[which(calls.may$BTN != "navailable"),]
calls.may$BTN <- ifelse(nchar(calls.may$BTN) > 10, 
substr(calls.may$BTN, 
nchar(calls.may$BTN) - 9, 
nchar(calls.may$BTN)), 
calls.may$BTN)

calls.may <- calls.may[-which(grepl("^855|^0|^1",calls.may$BTN)),]

# create a new column to extract area code from BTN
calls.may$AreaCode <- substr(calls.may$BTN, 1,3)

# CableCompany field cleaning
# Converting all cable company names to lower case to eliminate case issues

calls.may$CableCompany <- str_to_lower(calls.may$CableCompany) 

# Remove all non graphical characters
calls.may$CableCompany <- str_replace_all(calls.may$CableCompany,"[^[:graph:]]", " ") 

# Trim white spaces

calls.may$CableCompany <- trimws(calls.may$CableCompany, which = "both")

# Fix odd-ball name entries
calls.may$CableCompany <- ifelse(calls.may$CableCompany=="atlanticbroadband","atlantic broadband",calls.may$CableCompany)
calls.may$CableCompany[which(calls.may$CableCompany == "att - touchtone technologies")] <- "att-touchtone technologies"  
calls.may$CableCompany[which(calls.may$CableCompany=="cableone")] <- "cable one"
calls.may$CableCompany[which(calls.may$CableCompany=="cyh - charter")] <- "cyh-charter"
calls.may$CableCompany[which(calls.may$CableCompany=="cyh - comcast")] <- "cyh-comcast"
calls.may$CableCompany[which(calls.may$CableCompany=="cyh-twc")] <- "cyh-time warner"
calls.may$CableCompany[which(calls.may$CableCompany=="cyh - twc")] <- "cyh-time warner"
calls.may$CableCompany[which(calls.may$CableCompany=="cyh - time warner")] <- "cyh-time warner"
calls.may$CableCompany[which(calls.may$CableCompany=="godish.com, ltd.llp")] <-"godish"
calls.may$CableCompany[which(calls.may$CableCompany=="satcntry")]<-"satellite country"
calls.may$CableCompany <- ifelse(calls.may$CableCompany=="twc","time warner",calls.may$CableCompany)
calls.may$CableCompany[which(calls.may$CableCompany=="uni-sat")] <- "uni-sat communications"
calls.may$CableCompany[which(calls.may$CableCompany=="Wind Stream")] <- "Windstream"


# create the sale/no sale variable to capture just the sale/non-sale outcome
calls.may$Sale <- ifelse(grepl("Play -", calls.may$Disposition, ignore.case = T),
 "Sale","No Sale")

## ----Exploratory Data Analysis-----

# Calculating conversion ratio by agent ID.
AgentID_df <- calls.may %>%
group_by(AgentID) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 agents by volume since the number of agents is huge
n <- 12
top_Agents <- AgentID_df %>% 
arrange(desc(ConvRate)) %>% 
slice(1:n)

# The factor() call sets the levels but creates NAs; the second line
# replaces NAs with the "other" factor level.

AgentID_df$top_Agents <- factor(AgentID_df$AgentID, levels = c(top_Agents$AgentID, "other"))
AgentID_df$top_Agents[which(is.na(AgentID_df$top_Agents))] <- "other"
rm(top_Agents)
ggplot(AgentID_df, aes(x=ConvRate, fill = top_Agents)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "Agent IDs")) +
theme_bw()

# Calculating conversion ratio by cable company.
CableCompany_df <- calls.may %>%
group_by(CableCompany) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 cable companies by volume since the number of cable companies is huge
n <- 12
top_CableCompanies <- CableCompany_df %>% 
arrange(desc(ConvRate)) %>% 
slice(1:n)

# The factor() call sets the levels but creates NAs; the second line
# replaces NAs with the "other" factor level.

CableCompany_df$top_companies <- factor(CableCompany_df$CableCompany, levels = c(top_CableCompanies$CableCompany, "other"))
CableCompany_df$top_companies[which(is.na(CableCompany_df$top_companies))] <- "other"
rm(top_CableCompanies)
ggplot(CableCompany_df, aes(x=ConvRate, fill = top_companies)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "Cable Company")) +
theme_bw()

# Plot conversion ratio for top Affiliates

# Calculating conversion ratio by Affiliates.
Affiliate_df <- calls.may %>%
group_by(Affiliate) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 affiliates by volume since the number of cable companies is huge
n <- 12
top_Affiliates <- Affiliate_df %>% 
arrange(desc(ConvRate)) %>% slice(1:n)

Affiliate_df$top_Affiliates <- factor(Affiliate_df$Affiliate, levels = c(top_Affiliates$Affiliate, "other"))
Affiliate_df$top_Affiliates[which(is.na(Affiliate_df$top_Affiliates))] <- "other"
rm(top_Affiliates)
#plot of Affiliate 
ggplot(Affiliate_df, aes(x=ConvRate, fill = top_Affiliates)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "Affiliates")) +
theme_bw()


# Calculating conversion ratio by TFN
TFN_df <- calls.may %>%
group_by(TFN) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 TFNs by volume since the number of TFNs is huge
n <- 12
top_TFNs <- TFN_df %>% 
arrange(desc(ConvRate)) %>% 
slice(1:n)

# The factor() call sets the levels but creates NAs; the second line
# replaces NAs with the "other" factor level.

TFN_df$top_TFNs <- factor(TFN_df$TFN, levels = c(top_TFNs$TFN, "other"))
TFN_df$top_TFNs[which(is.na(TFN_df$top_TFNs))] <- "other"
rm(top_TFNs)
ggplot(TFN_df, aes(x=ConvRate, fill = top_TFNs)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "TFNs")) +
theme_bw()

# Calculating conversion ratio by Call Skill
CallSkill_df <- calls.may %>%
group_by(CallSkill) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 call skills by volume 
n <- 12
top_CallSkills <- CallSkill_df %>% 
arrange(desc(ConvRate)) %>% 
slice(1:n)

# The factor() call sets the levels but creates NAs; the second line
# replaces NAs with the "other" factor level.

CallSkill_df$top_callskills <- factor(CallSkill_df$CallSkill, levels = c(top_CallSkills$CallSkill, "other"))
CallSkill_df$top_callskills[which(is.na(CallSkill_df$top_callskills))] <- "other"
rm(top_CallSkills)
ggplot(CallSkill_df, aes(x=ConvRate, fill = top_callskills)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "Call Skills")) +
theme_bw()

# Calculating conversion ratio by Call Skill
OrigSkill_df <- calls.may %>%
group_by(OriginalSkill) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 call skills by volume 
n <- 12
top_OrigSkills <- OrigSkill_df %>% 
arrange(desc(ConvRate)) %>% 
slice(1:n)

# The factor() call sets the levels but creates NAs; the second line
# replaces NAs with the "other" factor level.

OrigSkill_df$top_origskills <- factor(OrigSkill_df$OriginalSkill, levels = c(top_OrigSkills$OriginalSkill, "other"))
OrigSkill_df$top_origskills[which(is.na(OrigSkill_df$top_origskills))] <- "other"
rm(top_OrigSkills)
ggplot(OrigSkill_df, aes(x=ConvRate, fill = top_origskills)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "Original Skills")) +
theme_bw()

# Calculating conversion ratio by Call Skill
AreaCode_df <- calls.may %>%
group_by(AreaCode) %>%
summarise(CallsAns=n(), ConvRate = length(Sale[Sale == "Sale"])/length(Sale))

# Plot top 12 call skills by volume 
n <- 12
top_AreaCode <- AreaCode_df %>% 
arrange(desc(ConvRate)) %>% 
slice(1:n)

# The factor() call sets the levels but creates NAs; the second line
# replaces NAs with the "other" factor level.

AreaCode_df$top_areacodes <- factor(AreaCode_df$AreaCode, levels = c(top_AreaCode$AreaCode, "other"))
AreaCode_df$top_areacodes[which(is.na(AreaCode_df$top_areacodes))] <- "other"
rm(top_AreaCode)
ggplot(AreaCode_df, aes(x=ConvRate, fill = top_areacodes)) + 
geom_histogram(bins = 15) +
scale_fill_manual(values = c(brewer.pal(n = n, "Paired"), "grey50"), guide = guide_legend(title = "Area Codes")) +
theme_bw()

sales.ratio.overall <- prop.table(table(calls.may$Sale))
print(sales.ratio.overall, digits = 1)

# drop call start,end time and Outcome since they are not required for the analysis
calls.may<-calls.may[,-c(2,3,4,7,12)]

## Perform ANOVA
#Sample 1000 rows from the data to run anova
n<-sample(nrow(calls.may),1000)
calls.maysample <- calls.may[n,]

# convert sale-nosale to binary

calls.maysample$SaleCode<- ifelse(calls.maysample$Sale=="Sale",1,0)


#run anova
anova.calls.maysample<- anova(lm(SaleCode ~ Affiliate+AreaCode , calls.maysample))
anova.calls.maysample

# Percent of each level for factor, Pr(x)

percent.of.CableCompany <- calls.may%>%group_by(CableCompany)%>%summarise(CabCompCount=n())%>%
mutate(PercentOfTot = CabCompCount/sum(CabCompCount))%>%select(CableCompany, PercentOfTot)
percent.of.CableCompany

paste("Percent of Affiliate, Pr(Affiliate)")
percent.of.Affiliate <- calls.may%>%group_by(Affiliate)%>%summarise(AffiliateCount=n())%>%
mutate(PercentOfTot = AffiliateCount/sum(AffiliateCount))%>%select(Affiliate, PercentOfTot)
percent.of.Affiliate

paste("Percent of AreaCode, Pr(AreaCode)")
percent.of.AreaCode <- calls.may%>%group_by(AreaCode)%>%summarise(AreaCodeCount=n())%>%
mutate(PercentOfTot = AreaCodeCount/sum(AreaCodeCount))%>%select(AreaCode, PercentOfTot)
percent.of.AreaCode

paste("Percent of Sale , Pr(C)")
percent.of.Sale <- calls.may%>%group_by(Sale)%>%summarise(SaleCount=n())%>%
mutate(PercentOfTot = SaleCount/sum(SaleCount))%>%select(Sale, PercentOfTot)
percent.of.Sale


# Since we know from the ANOVA that only Affiliate and Area Code
# are significant, we'll only calculate those.

paste("Pr (Affiliate | C)")
PrAffiliate.Conv <- calls.may%>%filter(Sale=="Sale")%>%group_by(Affiliate)%>%summarise(CallsAns = n())%>%mutate(Prob=CallsAns/sum(CallsAns))%>%select(Affiliate,Prob)
PrAffiliate.Conv

paste("Pr (AreaCode | C)")
PrAreaCode.Conv <- calls.may%>%filter(Sale=="Sale")%>%group_by(AreaCode)%>%summarise(CallsAns = n())%>%mutate(Prob=CallsAns/sum(CallsAns))%>%select(AreaCode,Prob)
PrAreaCode.Conv


# Calculate probability for a specific call

#Probability of overall conversion Pr(C)
salerate = percent.of.Sale$PercentOfTot[2]

#Probability of Affiliate conversion Pr(Conv | Affiliate)
PrConv.given.Affiliate <- merge(PrAffiliate.Conv, percent.of.Affiliate)
PrConv.given.Affiliate <- PrConv.given.Affiliate%>%mutate(ProbOverall=Prob*salerate/PercentOfTot)

#Probability of AreaCode conversion Pr(Conv | AreaCode)
PrConv.given.AreaCode <- merge(PrAreaCode.Conv, percent.of.AreaCode)
PrConv.given.AreaCode <- PrConv.given.AreaCode%>%mutate(ProbOverall=Prob*salerate/PercentOfTot)

# Function to predict probability of sale in real time 


prob.of.sale<-function(AreaCode, Affiliate){
  prob <- PrConv.given.AreaCode$ProbOverall[PrConv.given.AreaCode$AreaCode==AreaCode]*100+PrConv.given.Affiliate$ProbOverall[PrConv.given.Affiliate$Affiliate==Affiliate]*100
  paste(round(prob/2,2),"%")
}
