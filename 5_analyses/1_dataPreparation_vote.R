# Data Preparation Script - Vote ##############################################
### Please cite ###
# Voting for insider trading regulation. An experimental study of informed and uninformed traders' preferences.

## Preparing workspace and necessary packages #################################

#install.packages("zTree")
pacman::p_load("zTree")
library("zTree")
#source("http://www.kirchkamp.de/lab/zTree.R") # Loads R import script by Oliver Kirchkamp (https://www.kirchkamp.de//lab/zTree.html#zTreeR)
library("stringr")
library("ggplot2")
library("mgcv")
library("effects")
library("margins")
library("pscl")
library("lme4")
library("gplots")
library("tidyr")
library("dplyr")
setwd(getwd())
 
SourceFiles<-list.files("./DataExcel.",pattern="^G(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32)(Fixed|Fluct)(NR|RN).xls", full.names=T, recursive=F)

RawData<-zTreeTables(SourceFiles,tables=NULL, sep="\t", ignore.errors = T)
# Changes made to excel files:
## changed Euro-Sign to Euro in text answers
## specified group in Date

save.image("RawDataI.RData")
Data <- RawData
globals <-Data$globals
contracts <- Data$contracts
subjects <- Data$subjects
infodata <- Data$infodata
predictions <- Data$predictions
EET <- Data$EET
session <- Data$session
summary <- Data$summary
timelog <- Data$timelog
tagnums <- Data$tagnums
predictionhistory <- Data$predictionhistory
history <- Data$history
## drop stage variables in subjects
subjects <- cbind(subjects[,which(colnames(subjects)=="Date"):which(colnames(subjects)=="CancelledVol")], subjects[,which(colnames(subjects)=="SumDividende"):ncol(subjects)])
subjects <- subset(subjects, select = -c(DoneRead1, DoneRead2, ShowOKRead2, DoneFinalVoting, DonePreQuestionnaire, DoneQuestionnaire, DoneRoleInfo, DoneSelection, DoneHistory, DoneVoting, DoneStageSelection, ScreenEET, DoneEET, ScreenPreQuestionnaire, ScreenQuestionnaire, CQ, CQ31, CQ32, CQ33, CQ34, CQ61, CQ62, CQ63, LeaveStage))

## Collect and distribute information about treatment and participants ########
## distribute treatment characteristics
clist <- c("globals", "contracts", "subjects", "infodata", "predictions", "EET", "session", "summary", "timelog", "tagnums", "predictionhistory", "history")
for(i in clist) {
  Object <- get(paste0(i))
  Object$Group <- as.numeric(substr(Object$Date, 13,14))
  Object$subTreatment<-"FLUCT"
  Object$subTreatment[Object$Group==3|Object$Group==4|Object$Group==5|Object$Group==6|Object$Group==11|Object$Group==12|Object$Group==15|Object$Group==16|Object$Group==23|Object$Group==25|Object$Group==26|Object$Group==27|Object$Group==29|Object$Group==30|Object$Group==31|Object$Group==32]<-"FIXED"
  Object$subTreatment <- factor(Object$subTreatment, levels = c("FIXED", "FLUCT"))
  Object$regime_order<-"NR"
  Object$regime_order[Object$Group==1|Object$Group==2|Object$Group==3|Object$Group==4|Object$Group==9|Object$Group==10|Object$Group==17|Object$Group==18|Object$Group==19|Object$Group==20|Object$Group==23|Object$Group==25|Object$Group==26|Object$Group==27|Object$Group==31|Object$Group==32]<-"RN"
  Object$Treatment<-paste0(Object$subTreatment, Object$regime_order)
  assign(paste0(i), Object)
}

## assign subjectID
clist <- c("infodata", "subjects" , "predictions", "EET", "session", "timelog", "predictionhistory", "history")
for(i in clist) {
  Object <- get(paste0(i))
  Object$subjectID[as.numeric(Object$Subject)!=11] <- 10*(as.numeric(Object$Group[as.numeric(Object$Subject)!=11])-1) + as.numeric(Object$Subject[as.numeric(Object$Subject)!=11])
  assign(paste0(i), Object)
}

## define trader prospects and roles
clist <- c("subjects" , "session", "history")
for(i in clist) {
  Object <- get(paste0(i))
  Object$ExpectedRole[as.numeric(Object$Subject)!=11 & (Object$Role==1 | Object$Role==2) & Object$subTreatment=="FLUCT"] <- "FLUCT"
  Object$ExpectedRole[as.numeric(Object$Subject)!=11 & Object$Role==1 & Object$subTreatment=="FIXED"] <- "Uninformed trader"
  Object$ExpectedRole[as.numeric(Object$Subject)!=11 & Object$Role==2 & Object$subTreatment=="FIXED"] <- "Informed trader"
  Object$ExpectedRole[as.numeric(Object$Subject)!=11 & Object$Role==3] <- "Observer"
  Object$ExpectedRole[as.numeric(Object$Subject)==11] <- "Experimenter"
  Object$ExpectedRole <- factor(Object$ExpectedRole, levels = c("FLUCT", "Informed trader", "Uninformed trader", "Observer", "Experimenter"))
  Object$Role[as.numeric(Object$Subject)!=11 & Object$Role==1] <- "Uninformed trader"
  Object$Role[as.numeric(Object$Subject)!=11 & Object$Role==2] <- "Informed trader"
  Object$Role[as.numeric(Object$Subject)!=11 & Object$Role==3] <- "Observer"
  Object$Role[as.numeric(Object$Subject)==11] <- "Experimenter"
  Object$Role <- factor(Object$Role, levels = c("Informed trader", "Uninformed trader", "Observer", "Experimenter"))
  assign(paste0(i), Object)
}

## collect sociodemographics
subjects$gender[as.numeric(subjects$Subject)!=11 & subjects$gender==1 & subjects$Period==12] <- "male"
subjects$gender[as.numeric(subjects$Subject)!=11 & subjects$gender==2 & subjects$Period==12] <- "female"
subjects$gender[as.numeric(subjects$Subject)!=11 & subjects$gender==3 & subjects$Period==12] <- "diverse"
subjects$gender <- factor(subjects$gender, levels = c("female", "male", "diverse"))

subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==1 & subjects$Period==12] <- "Medizin"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==2 & subjects$Period==12] <- "Jus"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==3 & subjects$Period==12] <- "SOWI"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==4 & subjects$Period==12] <- "GEIWI"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==5 & subjects$Period==12] <- "NATWI"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==6 & subjects$Period==12] <- "Theologie"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==7 & subjects$Period==12] <- "Baufakult?t"
subjects$Faculty[as.numeric(subjects$Subject)!=11 & subjects$Faculty==8 & subjects$Period==12] <- "andere"

## Market characteristics #####################################################
## assign market characteristics
globals$IsREG[globals$`IsPun[1]`==0] <- "NOREG"
globals$IsREG[globals$`IsPun[1]`==1] <- "REG"
globals$IsREG[globals$Period<1] <- "Training"
globals$BBV <- globals$`BBV[1]`
globals$BBVCent <- globals$BBV - 57.5
globals$VREG1 <- globals$`VREG1[1]`
globals$VREG2 <- globals$`VREG2[1]`
globals$Referendum1 <- globals$`Election1[1]`
globals$Referendum2 <- globals$`Election2[1]`

## distribute market characteristics
clist <- c("infodata", "subjects" , "predictions", "predictionhistory", "history", "contracts")
for(i in clist) {
  Object <- get(paste0(i))
  for (g in 1:32){
    for (P in (-1):12){
      Object$IsREG[Object$Group==g & Object$Period==P] <- globals$IsREG[globals$Group==g & globals$Period==P]
      Object$BBV[Object$Group==g & Object$Period==P] <- globals$BBV[globals$Group==g & globals$Period==P]
      Object$BBVCent <- Object$BBV - 57.5
      assign(paste0(i), Object)
    }
  }
}

## EET results ################################################################
## assign EET scores
subjects$xScore[subjects$xScore==(-77778)] <- NaN
subjects$yScore[subjects$yScore==(-77778)] <- NaN
subjects$yScore[subjects$`left[10]` == 1 & subjects$yScore==2.5] <- 1.5
subjects$xScore[subjects$`left[5]` == 1 & subjects$xScore==-2.5] <- -1.5
session <- merge(session, subset(subjects, Period == -1 & Subject!=11, select = c(subjectID, `left[1]`, `left[2]`, `left[3]`, `left[4]`, `left[5]`, `left[6]`, `left[7]`, `left[8]`, `left[9]`, `left[10]`)), by = "subjectID", all.x = T)
session$yScore[session$`left[10]` == 1 & session$yScore==2.5] <- 1.5
session$xScore[session$`left[5]` == 1 & session$xScore==-2.5] <- -1.5
session$xScore[session$xScore==(-77778)] <- NaN
session$yScore[session$yScore==(-77778)] <- NaN
session$xScore[session$xScore==(-77777)] <- NA
session$yScore[session$yScore==(-77777)] <- NA
session$EET <- "Selfish"
session$EET[session$xScore < -1] <- "Envious"
session$EET[session$xScore > 1] <- "Kiss up"
session$EET[session$yScore < -1] <- "Kick down"
session$EET[session$yScore > 1] <- "Maximin"
session$EET[session$xScore > 1 & session$yScore > 1] <- "Altruistic"
session$EET[session$xScore < -1 & session$yScore < -1] <- "Spiteful"
session$EET[session$xScore < -1 & session$yScore > 1] <- "Inequality averse"
session$EET[session$xScore > 1 & session$yScore < -1] <- "Equality averse"
session$EET[is.na(session$xScore) & is.na(session$yScore)] <- "Non-monotonic"
session$EET <- factor(session$EET, levels = c("Selfish", "Altruistic", "Maximin", "Inequality averse",  "Kiss up","Envious","Spiteful","Kick down", "Non-monotonic"))

session$xScoreAll <- session$xScore
session$yScoreAll <- session$yScore
session$xScoreAll[is.na(session$xScore)] <- 0
session$yScoreAll[is.na(session$yScore)] <- 0

## Trader activity ############################################################
subjects$active <- "inactive"
subjects$active[subjects$Role=="Observer"] <- NA
subjects$active[subjects$Volume>0 | subjects$LimitVol>0] <- "active"

subjects$Unmasked[subjects$Punished==1] <- "Unmasked"
subjects$Unmasked[subjects$Punished==1 & subjects$IsREG=="REG"] <- "Punished"
subjects$Unmasked[subjects$Punished==0 & subjects$Role=="Informed trader"] <- "Not unmasked"
subjects$Unmasked[subjects$Punished==0 & subjects$Role!="Informed trader"] <- "Uninformed trader"

## contracts/transactions/offers tables #######################################
## specify subjectID, roles, and action characteristics
contracts$SellerID <- NA
contracts$BuyerID <- NA
contracts$MakerID <- NA
contracts$TakerID <- NA
contracts$action[contracts$BidDel>0 | contracts$AskDel>0] <- "cancelled"
contracts$action[(contracts$tradeID)!=0] <- "transaction"
contracts$action[(contracts$q==contracts$qOfferB | contracts$q==contracts$qOfferS) & contracts$q!=0] <- "sold out"
contracts$action[contracts$Bid!=(-1) | contracts$Ask!=1001] <- "on market"
contracts$SellerID[contracts$Seller>0] <- 10*(contracts$Group[contracts$Seller>0]-1) + contracts$Seller[contracts$Seller>0]
contracts$BuyerID[contracts$Buyer>0] <- 10*(contracts$Group[contracts$Buyer>0]-1) + contracts$Buyer[contracts$Buyer>0]
contracts$MakerID[contracts$Maker>0] <- 10*(contracts$Group[contracts$Maker>0]-1) + contracts$Maker[contracts$Maker>0]
contracts$TakerID[contracts$Buyer>0 & contracts$Seller>0 & contracts$Buyer == contracts$Maker] <- contracts$SellerID[contracts$Buyer>0 & contracts$Seller>0 & contracts$Buyer == contracts$Maker]
contracts$TakerID[contracts$Buyer>0 & contracts$Seller>0 & contracts$Seller == contracts$Maker] <- contracts$BuyerID[contracts$Buyer>0 & contracts$Seller>0 & contracts$Seller == contracts$Maker]
contracts$Price <- contracts$p
contracts$Price[contracts$Price==0 | is.na(contracts$Price)] <- contracts$BidDel[contracts$Price==0| is.na(contracts$Price)]
contracts$Price[contracts$Price==0 | is.na(contracts$Price)] <- contracts$AskDel[contracts$Price==0| is.na(contracts$Price)]
contracts$Price[contracts$Price==0 | is.na(contracts$Price)] <- contracts$Bid[contracts$Price==0| is.na(contracts$Price)]
contracts$Price[contracts$Price==(-1) | is.na(contracts$Price)] <- contracts$Ask[contracts$Price==(-1)| is.na(contracts$Price)]
contracts$Price[contracts$Price==(1001) | is.na(contracts$Price)] <- NA
contracts$type[contracts$BuyerID==contracts$MakerID] <- "buying offer"
contracts$type[contracts$SellerID==contracts$MakerID] <- "selling offer"

## if noone deleted an offer, NA is written in file
contracts$BidDel[is.na(contracts$BidDel)] <- 0
contracts$AskDel[is.na(contracts$AskDel)] <- 0
contracts$TimeLSCHENKaufgebotAuktionBidsSelect[is.na(contracts$TimeLSCHENKaufgebotAuktionBidsSelect)] <- 0
contracts$TimeLSCHENVerkaufsgebotAuktionAsksSelect[is.na(contracts$TimeLSCHENVerkaufsgebotAuktionAsksSelect)] <- 0

## specify contract start and end time
contracts$contractStart <- 180-contracts$TimeKAUFGEBOTAuktionMakeBidMake
contracts$contractStart[contracts$TimeKAUFGEBOTAuktionMakeBidMake==0] <- 180-contracts$TimeVERKAUFSGEBOTAuktionMakeAskMake[contracts$TimeKAUFGEBOTAuktionMakeBidMake==0]
contracts$contractEnd[is.na(contracts$action)==F] <- 180
contracts$contractEnd[contracts$action=="cancelled" & contracts$type=="buying offer" & is.na(contracts$type)==F] <- 180 - contracts$TimeLSCHENKaufgebotAuktionBidsSelect[contracts$action=="cancelled" & contracts$type=="buying offer" & is.na(contracts$type)==F]
contracts$contractEnd[contracts$action=="cancelled" & contracts$type=="selling offer" & is.na(contracts$type)==F] <- 180 - contracts$TimeLSCHENVerkaufsgebotAuktionAsksSelect[contracts$action=="cancelled" & contracts$type=="selling offer" & is.na(contracts$type)==F]
contracts$contractEnd[(contracts$action=="sold out" | contracts$action=="transaction") & contracts$type=="buying offer" & is.na(contracts$type)==F] <- 180 - contracts$TimeVERKAUFAuktionSellSelect[(contracts$action=="sold out" | contracts$action=="transaction") & contracts$type=="buying offer" & is.na(contracts$type)==F]
contracts$contractEnd[(contracts$action=="sold out" | contracts$action=="transaction") & contracts$type=="selling offer" & is.na(contracts$type)==F] <- 180 - contracts$TimeKAUFAuktionBuySelect[(contracts$action=="sold out" | contracts$action=="transaction") & contracts$type=="selling offer" & is.na(contracts$type)==F]
contracts$timecontract <- contracts$contractEnd - contracts$contractStart

## specify contract volume
contracts$remainOfferVol <- contracts$qOfferB + contracts$qOfferS
contracts$transactionVol <- contracts$q

## Occurences per second ######################################################
contracts$time <- contracts$`TGraph[1]`
contsum <- aggregate(q ~ Group + Period + time, data = contracts, function(x) sum(x, na.rm = T), na.action = na.pass)
sl <- c(1:32)
pl <- c((-1):12)
tl <- c(0:180)
contsum2 <- expand.grid(sl, pl, tl)
names(contsum2) <- c("Group", "Period", "time")
names(contsum) <- c("Group", "Period", "time", "sumq")
seconds <- merge(contsum, contsum2, by = c("Group", "Period", "time"), all = T)
seconds$sumq[is.na(seconds$sumq)] <- 0
seconds <- merge(seconds, subset(globals, select = c("Group", "Period", "subTreatment", "regime_order", "BBVCent", "IsREG")), by = c("Group", "Period"))

## MA process
for(s in 1:32){
  for(P in (-1):12){
    seconds$MA[seconds$Group==s & seconds$Period==P] <- stats::filter(x = subset(seconds, Group==s & Period==P)$sumq  , filter =  c(1/2^c(1:9),1/2^9), method = "convolution", sides =1)  
  }
}

for(s in 1:32){
  for(P in (-1):12){
    tg <- gam(sumq ~ s(time, k=180/10), data=subset(seconds, Group==s & Period==P))
    seconds$volfit[seconds$Group==s & seconds$Period==P] <- fitted.values(tg)
  }}

seconds$BestBid<-NA
seconds$BestAsk<-NA
for(s in 1:32){
  for(P in (-1):12){
    for(t in 0:180){
      seconds$BestBid[seconds$Group==s & seconds$Period==P & seconds$time==t] <- max(subset(contracts, Period==P & Group==s & 
                                                                                              contractStart<=t & contractEnd>=t & type=="buying offer")$Price)
      seconds$BestAsk[seconds$Group==s & seconds$Period==P & seconds$time==t] <- min(subset(contracts, Period==P & Group==s & 
                                                                                              contractStart<=t & contractEnd>=t & type=="selling offer")$Price)
      seconds$lastPrice[seconds$Group==s & seconds$Period==P & seconds$time==t] <- min(1001, subset(contracts, Period==P & Group==s & 
                                                                                                Buyer>0 & Seller>0 & contractEnd == max(subset(contracts, Period==P & Group==s & 
                                                                                                                                                 Buyer>0 & Seller>0 & contractEnd <=t)$contractEnd))$Price)
      seconds$maxPaccBid[seconds$Group==s & seconds$Period==P & seconds$time==t] <- max(-1, subset(contracts, Period==P & Group==s & 180-TimeVERKAUFAuktionSellSelect==t)$p)
      seconds$minPaccBid[seconds$Group==s & seconds$Period==P & seconds$time==t] <- min(1001, subset(contracts, Period==P & Group==s & 180-TimeVERKAUFAuktionSellSelect==t)$p)
      seconds$maxPaccAsk[seconds$Group==s & seconds$Period==P & seconds$time==t] <- min(1001, subset(contracts, Period==P & Group==s & 180-TimeKAUFAuktionBuySelect==t)$p)
      seconds$minPaccAsk[seconds$Group==s & seconds$Period==P & seconds$time==t] <- max(-1, subset(contracts, Period==P & Group==s & 180-TimeKAUFAuktionBuySelect==t)$p)
      
    }
  }
}
seconds$BestBid[seconds$BestBid==(-Inf)] <- NA
seconds$BestAsk[seconds$BestAsk==(Inf)] <- NA
seconds$BestBid[seconds$BestBid==(-1)] <- NA
seconds$BestAsk[seconds$BestAsk==(1001)] <- NA
seconds$maxPaccBid[seconds$maxPaccBid==(-1)] <- NA
seconds$minPaccBid[seconds$minPaccBid==(1001)] <- NA
seconds$minPaccAsk[seconds$minPaccAsk==(-1)] <- NA
seconds$maxPaccAsk[seconds$maxPaccAsk==(1001)] <- NA
seconds$BAspread <- seconds$BestAsk-seconds$BestBid
seconds$midpointBA <- (seconds$BestAsk+seconds$BestBid)/2
seconds$lastPrice[seconds$lastPrice==(Inf) | seconds$lastPrice==(-Inf)] <- NA
seconds$lnlastPrice <- log(seconds$lastPrice)
secy <- seconds
secy$time <- secy$time+1
seconds <- merge(seconds, subset(secy, select = c("time", "Period", "Group", "lnlastPrice")), by = c("time", "Period", "Group"), all.x = T)
names(seconds)[names(seconds)=="lnlastPrice.x"] <- "lnlastPrice"
names(seconds)[names(seconds)=="lnlastPrice.y"] <- "L.lnlastPrice"
seconds$return <- seconds$lnlastPrice - seconds$L.lnlastPrice

winsa <- quantile(seconds$BestAsk, .995, na.rm = T)
winsa1 <- quantile(seconds$BestAsk, .005, na.rm = T)
winsb <- quantile(seconds$BestBid, .995, na.rm = T)
winsb1 <- quantile(seconds$BestBid, .005, na.rm = T)
seconds$BestAskwins <- seconds$BestAsk
seconds$BestAskwins[seconds$BestAsk>winsa & is.na(seconds$BestAsk)==F] <- winsa
seconds$BestAskwins[seconds$BestAsk<winsa1 & is.na(seconds$BestAsk)==F] <- winsa1
seconds$BestBidwins <- seconds$BestBid
seconds$BestBidwins[seconds$BestBid>winsb & is.na(seconds$BestBid)==F] <- winsb
seconds$BestBidwins[seconds$BestBid<winsb1 & is.na(seconds$BestBid)==F] <- winsb1
seconds$BAspreadwins <- seconds$BestAskwins-seconds$BestBidwins
winsc <- quantile(seconds$BAspread, .995, na.rm = T)
winsc1 <- quantile(seconds$BAspread, .005, na.rm = T)
seconds$BAspreadwins2 <- seconds$BAspread
seconds$BAspreadwins2[seconds$BAspread>winsc & is.na(seconds$BAspread)==F] <- winsc
seconds$BAspreadwins2[seconds$BAspread<winsc1 & is.na(seconds$BAspread)==F] <- winsc1

save.image("RawDataII.RData")

## contracts/transactions/offers tables #######################################
## distribute traders characteristics
contracts <- merge(contracts, subset(subjects, select = c(Period, Group, subjectID, Role)), by.x = c("Period", "Group", "MakerID"), by.y = c("Period", "Group", "subjectID"))
colnames(contracts)[colnames(contracts) == "Role"] <- "MakerRole" 
contracts <- merge(contracts, subset(subjects, select = c(Period, Group, subjectID, Role)), by.x = c("Period", "Group", "TakerID"), by.y = c("Period", "Group", "subjectID"))
colnames(contracts)[colnames(contracts) == "Role"] <- "TakerRole"
contracts$Rolematching <- "ii"
contracts$Rolematching[contracts$MakerRole == "Informed trader" & contracts$TakerRole == "Uninformed trader"] <- "ui"
contracts$Rolematching[contracts$MakerRole == "Uninformed trader" & contracts$TakerRole == "Informed trader"] <- "iu"
contracts$Rolematching[contracts$MakerRole == "Uninformed trader" & contracts$TakerRole == "Uninformed trader"] <- "uu"
contracts$RoleType <- paste0(contracts$Rolematching, contracts$type)
contracts$profitableOffer <- "profitable"
contracts$profitableOffer[(contracts$Price - contracts$BBV < 0 & contracts$type == "selling offer") | (contracts$Price - contracts$BBV > 0 & contracts$type != "selling offer")] <- "notprofitable"
contracts$profitableOffer[(contracts$Price - contracts$BBV == 0)] <- "neutral"
contracts$profitableAcc <- "profitable"
contracts$profitableAcc[(contracts$Price - contracts$BBV > 0 & contracts$type == "selling offer") | (contracts$Price - contracts$BBV < 0 & contracts$type != "selling offer")] <- "notprofitable"
contracts$profitableAcc[(contracts$Price - contracts$BBV == 0)] <- "neutral"

#contracts <- merge(contracts, seconds, by = c("Group", "Period", "time"), all.x = T)
transactions <- subset(contracts, Buyer>0 & Seller>0)
transactions$SellersProfit <- transactions$Price-transactions$BBV
transactions$transactionID <- row(transactions)[,1]
transactions$transactionIDm <- row(transactions)[,1]
## prepare for RD,RAD,GD,GAD
transactions$RDi <- transactions$Price-transactions$BBV
transactions$GDi <- log(transactions$Price/transactions$BBV)

## create offers table
offers <- aggregate(cbind(remainOfferVol, contractEnd) ~ Group + contractID + Period + Treatment + subTreatment + MakerID + MakerRole + Price + type + profitableOffer + BBV + BBVCent + IsREG, data = contracts, function(x) max(x, na.rm = T), na.action = na.pass)
colnames(offers) <- c("Group", "contractID" ,"Period", "Treatment" , "subTreatment", "MakerID", "MakerRole", "Price", "type", "profitableOffer", "BBV", "BBVCent","IsREG", "limitVolume", "offerEnd")
offers$offerID <- row(offers)[,1]
o1 <- aggregate(transactionVol ~ Group + contractID + Period, data = contracts, function(x) sum(x, na.rm = T), na.action = na.pass)
colnames(o1) <- c("Group", "contractID", "Period", "TotTransacted")
offers <- merge(offers, o1, by = c("Group", "contractID" , "Period"), all = T)
o2 <- aggregate( contractStart ~ Group + contractID + Period, data = contracts, function(x) min(x), na.action = na.pass)
colnames(o2) <- c("Group", "contractID", "Period", "offerStart")
offers <- merge(offers, o2, by = c("Group", "contractID", "Period"), all = T)
offers$status <- "cancelled"
offers$status[offers$TotTransacted==offers$limitVolume] <- "sold out"
offers$status[offers$TotTransacted!=offers$limitVolume & offers$offerEnd==180] <- "on market"
offers$TotTransacted[is.na(offers$TotTransacted)] <- 0

## Trader activity/profit #####################################################
## calculate trader volume
tranoffers <- aggregate(cbind(limitVolume, TotTransacted, as.numeric(TotTransacted>0), 1) ~ MakerID + Period + Group, data = offers, function(x) sum(x, na.rm = T), na.action = na.pass)
tranoffers2 <- aggregate(cbind(transactionVol, 1) ~ TakerID + Period + Group, data = transactions, function(x) sum(x, na.rm = T), na.action = na.pass)
colnames(tranoffers) <- c("subjectID", "Period", "Group", "LimitVolume", "transactedLimitVol", "LimitTrades",  "LimitOrders")
colnames(tranoffers2) <- c("subjectID", "Period", "Group", "MarketVolume", "MarketOrders.y")
subjects <- merge(subjects, tranoffers, by = c("Period", "subjectID", "Group"), all = T)
subjects <- merge(subjects, tranoffers2, by = c("Period", "subjectID", "Group"), all = T)
subjects$LimitVolume[subjects$Subject != 11 & is.na(subjects$LimitVolume) & subjects$Role != "Observer"] <- 0
subjects$transactedLimitVol[subjects$Subject != 11 & is.na(subjects$transactedLimitVol) & subjects$Role != "Observer"] <- 0
subjects$LimitOrders[subjects$Subject != 11 & is.na(subjects$LimitOrders) & subjects$Role != "Observer"] <- 0
subjects$LimitTrades[subjects$Subject != 11 & is.na(subjects$LimitTrades) & subjects$Role != "Observer"] <- 0
subjects$transactedLimitVol[subjects$Subject != 11 & is.na(subjects$transactedLimitVol) & subjects$Role != "Observer"] <- 0
subjects$MarketVolume[subjects$Subject != 11 & is.na(subjects$MarketVolume) & subjects$Role != "Observer"] <- 0
subjects$MarketOrders[subjects$Subject != 11 & is.na(subjects$MarketOrders.y) & subjects$Role != "Observer"] <- 0
subjects$LMdiff <- (subjects$LimitVolume-subjects$MarketVolume)
subjects$L2Mdiff <- (subjects$transactedLimitVol-subjects$MarketVolume)

## calculate traders trading profits
subjects$Trades <- subjects$Sales + subjects$Purchases
subjects$TradingProfit <- subjects$DiffAvEndVermoegen
subjects$lnVolume <- log(subjects$Volume)
subjects$InitialEndowment <- subjects$PersonalMoneyEndowment+subjects$PersonalStockEndowment*subjects$BBV
subjects$relInitialEndowment <- subjects$InitialEndowment/60/subjects$BBV
subjects$TPPun <- subjects$TradingProfit+subjects$CompensationReceived
subjects$TPRedist <- subjects$TradingProfit+subjects$CompensationReceived
subjects$TPRedist[subjects$Role == "Informed trader"] <- subjects$TradingProfit[subjects$Role == "Informed trader"] + subjects$CompensationReceived[subjects$Role == "Informed trader"]/2
subjects$PD <- subjects$TPPun/subjects$InitialEndowment
subjects$logPD <- log(1+subjects$TPPun/subjects$InitialEndowment)

## calculate traders relative endowment
subjects$InitialEndowmentUnits <- subjects$InitialEndowment/subjects$BBV
subjects$EndEndowmentUnits <- subjects$EndVermoegen/subjects$BBV
subjects$EndEndowmentUnitsPun <- (subjects$EndVermoegen + subjects$CompensationReceived)/subjects$BBV
subjects$relEndEndowment <- subjects$EndVermoegen/60/subjects$BBV
subjects$TPUnits <- subjects$TradingProfit/subjects$BBV
subjects$TPUnitsPun <- subjects$TPPun/subjects$BBV
subjects$TPUnitsRedist <- subjects$TPRedist/subjects$BBV
subjects$PDbefore <- subjects$TradingProfit/subjects$InitialEndowment
subjects$PDPun <- subjects$TPPun/subjects$InitialEndowment
subjects$PDRedist <- subjects$TPRedist/subjects$InitialEndowment
subjects$PDbeforeVol <- 1000 * subjects$TradingProfit/subjects$InitialEndowment/subjects$Volume
subjects$PDbeforeVol[is.na(subjects$PDbeforeVol)] <- 0
subjects$PDbeforeVol[is.infinite(subjects$PDbeforeVol)] <- 0
subjects$PDPunVol <- 1000 * subjects$TPPun/subjects$InitialEndowment/subjects$Volume
subjects$PDPunVol[is.na(subjects$PDPunVol)] <- 0
subjects$PDPunVol[is.infinite(subjects$PDPunVol)] <- 0
subjects$PDRedistVol <- 1000 * subjects$TPRedist/subjects$InitialEndowment/subjects$Volume
subjects$PDRedistVol[is.na(subjects$PDRedistVol)] <- 0
subjects$PDRedistVol[is.infinite(subjects$PDRedistVol)] <- 0
subjects$logTPPun <- log(subjects$TPUnitsPun+2*subjects$InitialEndowmentUnits)-log(2*subjects$InitialEndowmentUnits)

## distribute information to session table
cc <- subset(subjects, Subject !=11 & Period>0, select = c("subjectID", "Period", "Role", "ProfitPeriod","DiffAvEndVermoegen","CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "Unmasked", "IsREG", "IsInsider"))
colnames(cc)[colnames(cc)=="DiffAvEndVermoegen"] <- "TradingProfit"
colnames(cc)[colnames(cc)=="IsREG"] <- "IsREG"
ccc <- reshape(cc, idvar="subjectID",
               v.names = c("Role", "ProfitPeriod","TradingProfit","CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "Unmasked", "IsREG", "IsInsider"),
               timevar = "Period",
               direction="wide")
session <- merge(session, ccc, by = "subjectID")
session <- merge(session, subset(subjects, Period==12 & Subject!=11, select = c("gender", "age", "PunPerceived", "ReasonVote", "StrategyObserver", "ChanceIdentification", "StrategyTrader", "RiskGeneral", "RiskFinancial", "LossAversion", "Faculty", "notes", "subjectID", "Vote3", "orderfinal")), by = c("subjectID"))
colnames(session)[colnames(session)=="orderfinal"] <- "order3"
session <- merge(session, subset(subjects, Period==10 & Subject!=11, select = c("subjectID", "order")), by = c("subjectID"))
colnames(session)[colnames(session)=="order"] <- "order2"
session <- merge(session, subset(subjects, Period==7 & Subject!=11, select = c("subjectID", "order")), by = c("subjectID"))
colnames(session)[colnames(session)=="order"] <- "order1"
session$Referendum1 <- factor(session$IsREG.7, levels = c("NOREG", "REG"))
session$Referendum2 <- factor(session$IsREG.10, levels = c("NOREG", "REG"))

## Calculate aggregated data before each referendum ###########################
## calc means on Subject - REG level
SubjectREGagg <- aggregate(cbind(ProfitPeriod, (Volume>0 | LimitVol>0), DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV) ~ subjectID + IsREG, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period<7 & IsREG!="Training"))
colnames(SubjectREGagg)[colnames(SubjectREGagg) %in% c("V2", "V4")] <- c("PR", "CompensationReceived")
SubjectREGagg$Referendum <- "Referendum 1"
SubjectREGaggP3 <- aggregate(cbind(ProfitPeriod, (Volume>0 | LimitVol>0),  DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV) ~ subjectID + IsREG, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period<10 & IsREG!="Training"))
colnames(SubjectREGaggP3)[colnames(SubjectREGaggP3) %in% c("V2", "V4")] <- c("PR", "CompensationReceived")
SubjectREGaggP3$Referendum <- "Referendum 2"
SubjectREGaggP4 <- aggregate(cbind(ProfitPeriod, (Volume>0 | LimitVol>0),  DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV) ~ subjectID + IsREG, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, IsREG!="Training"))
colnames(SubjectREGaggP4)[colnames(SubjectREGaggP4) %in% c("V2", "V4")] <- c("PR", "CompensationReceived")
SubjectREGaggP4$Referendum <- "Referendum 3"

## calc means on Subject level
SubjectREGagga <- aggregate(cbind(ProfitPeriod, (Volume>0 | LimitVol>0),  DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV) ~ subjectID, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period<7 & IsREG!="Training"))
colnames(SubjectREGagga)[colnames(SubjectREGagga) %in% c("V2", "V4")] <- c("PR", "CompensationReceived")
SubjectREGaggP3a <- aggregate(cbind(ProfitPeriod, (Volume>0 | LimitVol>0),  DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV) ~ subjectID, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period>6 & Period<10 & IsREG!="Training"))
colnames(SubjectREGaggP3a)[colnames(SubjectREGaggP3a) %in% c("V2", "V4")] <- c("PR", "CompensationReceived")
SubjectREGaggP4a <- aggregate(cbind(ProfitPeriod, (Volume>0 | LimitVol>0),  DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV) ~ subjectID, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period > 9 & IsREG!="Training"))
colnames(SubjectREGaggP4a)[colnames(SubjectREGaggP4a) %in% c("V2", "V4")] <- c("PR", "CompensationReceived")

## calc means on Expected Role level
Expectedroleagg <- aggregate(cbind(ProfitPeriod, DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff) ~ ExpectedRole, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period<7 & IsREG!="Training"))
colnames(Expectedroleagg)[colnames(Expectedroleagg) %in% c("ProfitPeriod","DiffAvEndVermoegen","V3", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders", "LMdiff", "L2Mdiff")] <- c("RoleProfit1","RoleTP1","RoleCompensationReceived1", "RolePDbefore1", "RolePDRedist1", "RolePDPun1", "RolePDbeforeVol1", "RolePDRedistVol1", "RolePDPunVol1", "RoleLimit1", "RoleLimitTrades1", "RoleLimitOrders1", "RoletransactedLimitVol1", "RoleVol1", "RoleMarketVolume1", "RoleMarketOrders1", "RoleLMdiff1", "RoleL2Mdiff1")
ExpectedroleagglogL <- aggregate((log(LimitVol)) ~ ExpectedRole, FUN= mean, data = subset(subjects, Period<7 & IsREG!="Training" & LimitVol>0))
ExpectedroleagglogV <- aggregate((log(Volume)) ~ ExpectedRole, FUN= mean, data = subset(subjects, Period<7 & IsREG!="Training" & Volume>0))
ExpectedroleagglogM <- aggregate((log(VolMarketOrders)) ~ ExpectedRole, FUN= mean, data = subset(subjects, Period<7 & IsREG!="Training" & VolMarketOrders>0))
ExpectedroleaggP3 <- aggregate(cbind(ProfitPeriod, DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff) ~ ExpectedRole, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, Period<10 & IsREG!="Training"))
colnames(ExpectedroleaggP3)[colnames(ExpectedroleaggP3) %in% c("ProfitPeriod","DiffAvEndVermoegen","V3", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff")] <- c("RoleProfit2","RoleTP2","RoleCompensationReceived2", "RolePDbefore2", "RolePDRedist2", "RolePDPun2", "RolePDbeforeVol2", "RolePDRedistVol2", "RolePDPunVol2", "RoleLimit2", "RoleLimitTrades2", "RoleLimitOrders2", "RoletransactedLimitVol2", "RoleVol2", "RoleMarketVolume2", "RoleMarketOrders2", "RoleLMdiff2", "RoleL2Mdiff2")
ExpectedroleaggP4 <- aggregate(cbind(ProfitPeriod, DiffAvEndVermoegen, CompensationReceived/BBV,  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  LimitVol, LimitTrades, LimitOrders, transactedLimitVol, Volume, MarketVolume, MarketOrders, LMdiff, L2Mdiff) ~ ExpectedRole, function(x) mean(x, na.rm = T), na.action = na.pass, data = subset(subjects, IsREG!="Training"))
colnames(ExpectedroleaggP4)[colnames(ExpectedroleaggP4) %in% c("ProfitPeriod","DiffAvEndVermoegen","V3", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff")] <- c("RoleProfit3","RoleTP3","RoleCompensationReceived3", "RolePDbefore3", "RolePDRedist3", "RolePDPun3", "RolePDbeforeVol3", "RolePDRedistVol3", "RolePDPunVol3", "RoleLimit3", "RoleLimitTrades3", "RoleLimitOrders3", "RoletransactedLimitVol3", "RoleVol3", "RoleMarketVolume3", "RoleMarketOrders3", "RoleLMdiff3", "RoleL2Mdiff3")


SubjectREGaggL <- subset(SubjectREGagg, IsREG=="REG", select = -c(IsREG, Referendum))
colnames(SubjectREGaggL)[colnames(SubjectREGaggL) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfitREG1", "AvgPRREG1", "AvgTPREG1", "AvgCompensationReceivedREG1", "AvgPDbeforeREG1", "AvgPDRedistREG1", "AvgPDPunREG1",  "AvgPDbeforeVolREG1", "AvgPDRedistVolREG1", "AvgPDPunVolREG1","AvgLimitREG1", "AvgLimitTradesREG1", "AvgLimitOrdersREG1", "AvgtransactedLimitVolumeREG1", "AvgVolREG1", "AvgMarketVolumeREG1", "AvgMarketOrdersREG1", "AvgLMdiffREG1", "AvgL2MdiffREG1", "AvgInitialAssetsREG1", "AvgEndAssetsREG1", "AvgInitialEndowmentREG1", "AvgEndEndowmentREG1")
SubjectREGaggN <- subset(SubjectREGagg, IsREG=="NOREG", select = -c(IsREG, Referendum))
colnames(SubjectREGaggN)[colnames(SubjectREGaggN) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfitNOREG1", "AvgPRNOREG1", "AvgTPNOREG1", "AvgCompensationReceivedNOREG1", "AvgPDbeforeNOREG1", "AvgPDRedistNOREG1", "AvgPDPunNOREG1", "AvgPDbeforeVolNOREG1", "AvgPDRedistVolNOREG1", "AvgPDPunVolNOREG1", "AvgLimitNOREG1", "AvgLimitTradesNOREG1", "AvgLimitOrdersNOREG1", "AvgtransactedLimitVolumeNOREG1", "AvgVolNOREG1", "AvgMarketVolumeNOREG1", "AvgMarketOrdersNOREG1", "AvgLMdiffNOREG1", "AvgL2MdiffNOREG1", "AvgInitialAssetsNOREG1", "AvgEndAssetsNOREG1", "AvgInitialEndowmentNOREG1", "AvgEndEndowmentNOREG1")
session <- merge(session, SubjectREGaggL, by = c("subjectID"), all = T)
session <- merge(session, SubjectREGaggN, by = c("subjectID"), all = T)
SubjectREGaggP3L <- subset(SubjectREGaggP3, IsREG=="REG", select = -c(IsREG, Referendum))
colnames(SubjectREGaggP3L)[colnames(SubjectREGaggP3L) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfitREG2", "AvgPRREG2", "AvgTPREG2","AvgCompensationReceivedREG2", "AvgPDbeforeREG2", "AvgPDRedistREG2", "AvgPDPunREG2", "AvgPDbeforeVolREG2", "AvgPDRedistVolREG2", "AvgPDPunVolREG2", "AvgLimitREG2", "AvgLimitTradesREG2", "AvgLimitOrdersREG2", "AvgtransactedLimitVolumeREG2", "AvgVolREG2", "AvgMarketVolumeREG2", "AvgMarketOrdersREG2", "AvgLMdiffREG2", "AvgL2MdiffREG2", "AvgInitialAssetsREG2", "AvgEndAssetsREG2", "AvgInitialEndowmentREG2", "AvgEndEndowmentREG2")
SubjectREGaggP3N <- subset(SubjectREGaggP3, IsREG=="NOREG", select = -c(IsREG, Referendum))
colnames(SubjectREGaggP3N)[colnames(SubjectREGaggP3N) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfitNOREG2", "AvgPRNOREG2", "AvgTPNOREG2","AvgCompensationReceivedNOREG2", "AvgPDbeforeNOREG2", "AvgPDRedistNOREG2", "AvgPDPunNOREG2", "AvgPDbeforeVolNOREG2", "AvgPDRedistVolNOREG2", "AvgPDPunVolNOREG2", "AvgLimitNOREG2", "AvgLimitTradesNOREG2", "AvgLimitOrdersNOREG2", "AvgtransactedLimitVolumeNOREG2", "AvgVolNOREG2", "AvgMarketVolumeNOREG2", "AvgMarketOrdersNOREG2", "AvgLMdiffNOREG2", "AvgL2MdiffNOREG2", "AvgInitialAssetsNOREG2", "AvgEndAssetsNOREG2", "AvgInitialEndowmentNOREG2", "AvgEndEndowmentNOREG2")

## distribute aggregated values to session table
session <- merge(session, SubjectREGaggP3L, by = c("subjectID"), all = T)
session <- merge(session, SubjectREGaggP3N, by = c("subjectID"), all = T)
SubjectREGaggP4L <- subset(SubjectREGaggP4, IsREG=="REG", select = -c(IsREG, Referendum))
colnames(SubjectREGaggP4L)[colnames(SubjectREGaggP4L) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfitREG3", "AvgPRREG3", "AvgTPREG3","AvgCompensationReceivedREG3", "AvgPDbeforeREG3", "AvgPDRedistREG3", "AvgPDPunREG3", "AvgPDbeforeVolREG3", "AvgPDRedistVolREG3", "AvgPDPunVolREG3", "AvgLimitREG3", "AvgLimitTradesREG3", "AvgLimitOrdersREG3", "AvgtransactedLimitVolumeREG3", "AvgVolREG3", "AvgMarketVolumeREG3", "AvgMarketOrdersREG3", "AvgLMdiffREG3", "AvgL2MdiffREG3", "AvgInitialAssetsREG3", "AvgEndAssetsREG3", "AvgInitialEndowmentREG3", "AvgEndEndowmentREG3")
SubjectREGaggP4N <- subset(SubjectREGaggP4, IsREG=="NOREG", select = -c(IsREG, Referendum))
colnames(SubjectREGaggP4N)[colnames(SubjectREGaggP4N) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen","CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfitNOREG3", "AvgPRNOREG3", "AvgTPNOREG3","AvgCompensationReceivedNOREG3", "AvgPDbeforeNOREG3", "AvgPDRedistNOREG3", "AvgPDPunNOREG3", "AvgPDbeforeVolNOREG3", "AvgPDRedistVolNOREG3", "AvgPDPunVolNOREG3", "AvgLimitNOREG3", "AvgLimitTradesNOREG3", "AvgLimitOrdersNOREG3", "AvgtransactedLimitVolumeNOREG3", "AvgVolNOREG3", "AvgMarketVolumeNOREG3", "AvgMarketOrdersNOREG3", "AvgLMdiffNOREG3", "AvgL2MdiffNOREG3", "AvgInitialAssetsNOREG3", "AvgEndAssetsNOREG3", "AvgInitialEndowmentNOREG3", "AvgEndEndowmentNOREG3")
session <- merge(session, SubjectREGaggP4L, by = c("subjectID"), all = T)
session <- merge(session, SubjectREGaggP4N, by = c("subjectID"), all = T)
colnames(SubjectREGagga)[colnames(SubjectREGagga) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfit1", "AvgPR1", "AvgTP1","AvgCompensationReceived1", "AvgPDbefore1", "AvgPDRedist1", "AvgPDPun1", "AvgPDbeforeVol1", "AvgPDRedistVol1", "AvgPDPunVol1", "AvgLimit1", "AvgLimitTrades1", "AvgLimitOrders1", "AvgtransactedLimitVolume1", "AvgVol1", "AvgMarketVolume1", "AvgMarketOrders1", "AvgLMdiff1", "AvgL2Mdiff1", "AvgInitialAssets1", "AvgEndAssets1", "AvgInitialEndowment1", "AvgEndEndowment1")
session <- merge(session, SubjectREGagga, by = c("subjectID"), all = T)
colnames(SubjectREGaggP3a)[colnames(SubjectREGaggP3a) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfit2", "AvgPR2", "AvgTP2","AvgCompensationReceived2", "AvgPDbefore2", "AvgPDRedist2", "AvgPDPun2", "AvgPDbeforeVol2", "AvgPDRedistVol2", "AvgPDPunVol2", "AvgLimit2", "AvgLimitTrades2", "AvgLimitOrders2", "AvgtransactedLimitVolume2", "AvgVol2", "AvgMarketVolume2", "AvgMarketOrders2", "AvgLMdiff2", "AvgL2Mdiff2", "AvgInitialAssets2", "AvgEndAssets2", "AvgInitialEndowment2", "AvgEndEndowment2")
session <- merge(session, SubjectREGaggP3a, by = c("subjectID"), all = T)
colnames(SubjectREGaggP4a)[colnames(SubjectREGaggP4a) %in% c("ProfitPeriod","PR","DiffAvEndVermoegen", "CompensationReceived", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", "LimitVol", "LimitTrades",  "LimitOrders", "transactedLimitVol", "Volume", "MarketVolume", "MarketOrders","LMdiff", "L2Mdiff", "PersonalStockEndowment", "Stock", "V22", "V23")] <- 
  c("AvgProfit3", "AvgPR3", "AvgTP3","AvgCompensationReceived3", "AvgPDbefore3", "AvgPDRedist3", "AvgPDPun3", "AvgPDbeforeVol3", "AvgPDRedistVol3", "AvgPDPunVol3", "AvgLimit3", "AvgLimitTrades3", "AvgLimitOrders3", "AvgtransactedLimitVolume3", "AvgVol3", "AvgMarketVolume3", "AvgMarketOrders3", "AvgLMdiff3", "AvgL2Mdiff3", "AvgInitialAssets3", "AvgEndAssets3", "AvgInitialEndowment3", "AvgEndEndowment3")
session <- merge(session, SubjectREGaggP4a, by = c("subjectID"), all = T)

## count amount of periods in which participant took a trader type
session$IsInsiderCount1 <- (session$Role.1=="Informed trader") + (session$Role.2=="Informed trader") + (session$Role.3=="Informed trader") + (session$Role.4=="Informed trader") + (session$Role.5=="Informed trader") + (session$Role.6=="Informed trader")
session$IsInsiderCount2 <- (session$Role.7=="Informed trader") + (session$Role.8=="Informed trader") + (session$Role.9=="Informed trader")
session$IsInsiderCount3 <- session$IsInsiderCount2 + (session$Role.10=="Informed trader") + (session$Role.11=="Informed trader") + (session$Role.12=="Informed trader")

session$PunishedCount1 <- (session$Unmasked.1=="Punished") + (session$Unmasked.2=="Punished") + (session$Unmasked.3=="Punished") + (session$Unmasked.4=="Punished") + (session$Unmasked.5=="Punished") + (session$Unmasked.6=="Punished")
session$PunishedCount2 <- session$PunishedCount1 + (session$Unmasked.7=="Punished") + (session$Unmasked.8=="Punished") + (session$Unmasked.9=="Punished")
session$PunishedCount3 <- session$IsInsiderCount2 + (session$Unmasked.10=="Punished") + (session$Unmasked.11=="Punished") + (session$Unmasked.12=="Punished")

session$UnmaskedCount1 <- session$PunishedCount1 + (session$Unmasked.1=="Unmasked") + (session$Unmasked.2=="Unmasked") + (session$Unmasked.3=="Unmasked") + (session$Unmasked.4=="Unmasked") + (session$Unmasked.5=="Unmasked") + (session$Unmasked.6=="Unmasked")
session$UnmaskedCount2 <- session$UnmaskedCount1 + session$PunishedCount2 - session$PunishedCount1 + (session$Unmasked.7=="Unmasked") + (session$Unmasked.8=="Unmasked") + (session$Unmasked.9=="Unmasked")
session$UnmaskedCount3 <- session$UnmaskedCount2 + session$PunishedCount3 - session$PunishedCount2 + (session$Unmasked.10=="Unmasked") + (session$Unmasked.11=="Unmasked") + (session$Unmasked.12=="Unmasked")

## aggregate st.dev of entries
tradProfitVar <- aggregate(cbind(ProfitPeriod, log(ProfitPeriod),  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  DiffAvEndVermoegen, CompensationReceived) ~ subjectID, data = subset(subjects, Period > 0 & Period < 7), function(x) sd(x, na.rm = T), na.action = na.pass)
colnames(tradProfitVar) <- c("subjectID", "sdProfit", "sdlnProfit", "sdPDbefore", "sdPDRedist", "sdPDPun", "sdPDbeforeVol", "sdPDRedistVol", "sdPDPunVol", "sdTP", "sdComp")
session <- merge(session, tradProfitVar, by = "subjectID")
#subjects <- merge(subjects, tradProfitVar, by = "subjectID", all = T) # will be merged later
tradProfitVar1 <- aggregate(cbind(ProfitPeriod, log(ProfitPeriod),  PDbefore, PDRedist, PDPun, PDbeforeVol, PDRedistVol, PDPunVol,  DiffAvEndVermoegen, CompensationReceived) ~ subjectID + IsREG, data = subset(subjects, Period > 0 & Period < 7), function(x) sd(x, na.rm = T), na.action = na.pass)
tradProfitVar2 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdProfitPeriod") + IsREG, value.var = "ProfitPeriod", drop = FALSE)
tradProfitVar3 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdlnProfitPeriod") + IsREG, value.var = "V2", drop = FALSE)
tradProfitVar4 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdPDPun") + IsREG, value.var = "PDPun", drop = FALSE)
tradProfitVar6 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdPDbefore") + IsREG, value.var = "PDbefore", drop = FALSE)
tradProfitVar7 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdPDPunVol") + IsREG, value.var = "PDPunVol", drop = FALSE)
tradProfitVar8 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdPDbeforeVol") + IsREG, value.var = "PDbeforeVol", drop = FALSE)
tradProfitVar5 <- reshape2::dcast(subset(tradProfitVar1), subjectID ~ paste0("sdTP") + IsREG, value.var = "DiffAvEndVermoegen", drop = FALSE)
tradProfitVar2 <- merge(tradProfitVar2, tradProfitVar3, by = "subjectID")
tradProfitVar2 <- merge(tradProfitVar2, tradProfitVar4, by = "subjectID")
tradProfitVar2 <- merge(tradProfitVar2, tradProfitVar5, by = "subjectID")
tradProfitVar2 <- merge(tradProfitVar2, tradProfitVar6, by = "subjectID")
tradProfitVar2 <- merge(tradProfitVar2, tradProfitVar7, by = "subjectID")
tradProfitVar2 <- merge(tradProfitVar2, tradProfitVar8, by = "subjectID")
session <- merge(session, tradProfitVar2, by = "subjectID", all = T)

## count periods participants experienced to be disclosed/punished
for(P in 1:12){
  for(g in 1:32){
    subjects$SomeoneUnmasked[subjects$Group==g & !is.na(subjects$Group) & subjects$Period==P] <-  if(0==length(subjects$Unmasked[subjects$Group==g & !is.na(subjects$Group) & subjects$Subject!=11 & subjects$Period==P & (subjects$Unmasked=="Punished" | subjects$Unmasked=="Unmasked")])){
     "no"} else {"yes"}
    subjects$SomeonePunished[subjects$Group==g & !is.na(subjects$Group) & subjects$Period==P] <-  if(0==length(subjects$Unmasked[subjects$Group==g & !is.na(subjects$Group) & subjects$Subject!=11 & subjects$Period==P & subjects$Unmasked=="Punished" ])){
     "no"} else {"yes"}
  }}

    for(g in 1:32){
      session$SomeoneUnmaskedCount1[session$Group==g & !is.na(session$Group)] <-  length(subjects$SomeoneUnmasked[subjects$Group==g & !is.na(subjects$Group) & subjects$Period>0 & subjects$Period<7 & subjects$SomeoneUnmasked=="yes"])/11
      session$SomeoneUnmaskedCount2[session$Group==g & !is.na(session$Group)] <-  length(subjects$SomeoneUnmasked[subjects$Group==g & !is.na(subjects$Group) & subjects$Period>0 & subjects$Period<10 & subjects$SomeoneUnmasked=="yes"])/11
      session$SomeoneUnmaskedCount3[session$Group==g & !is.na(session$Group)] <-  length(subjects$SomeoneUnmasked[subjects$Group==g & !is.na(subjects$Group) & subjects$Period>0 & subjects$SomeoneUnmasked=="yes"])/11
      session$SomeonePunishedCount1[session$Group==g & !is.na(session$Group)] <-  length(subjects$SomeonePunished[subjects$Group==g & !is.na(subjects$Group) & subjects$Period>0 & subjects$Period<7 & subjects$SomeonePunished=="yes"])/11
      session$SomeonePunishedCount2[session$Group==g & !is.na(session$Group)] <-  length(subjects$SomeonePunished[subjects$Group==g & !is.na(subjects$Group) & subjects$Period>0 & subjects$Period<10 & subjects$SomeonePunished=="yes"])/11
      session$SomeonePunishedCount3[session$Group==g & !is.na(session$Group)] <-  length(subjects$SomeonePunished[subjects$Group==g & !is.na(subjects$Group) & subjects$Period>0 & subjects$SomeonePunished=="yes"])/11
    }

session$SomeonePunished1 <- "no"
session$SomeonePunished1[session$SomeonePunishedCount1>0] <- "yes"
session$SomeonePunished2 <- "no"
session$SomeonePunished2[session$SomeonePunishedCount2>0] <- "yes"
session$SomeonePunished3 <- "no"
session$SomeonePunished3[session$SomeonePunishedCount3>0] <- "yes"
session$SomeoneUnmasked1 <- "no"
session$SomeoneUnmasked1[session$SomeoneUnmaskedCount1>0] <- "yes"
session$SomeoneUnmasked2 <- "no"
session$SomeoneUnmasked2[session$SomeoneUnmaskedCount2>0] <- "yes"
session$SomeoneUnmasked3 <- "no"
session$SomeoneUnmasked3[session$SomeoneUnmaskedCount3>0] <- "yes"

session$PunishedCount1F <- session$PunishedCount1
session$PunishedCount1F[session$PunishedCount1==0] <- "no"
session$PunishedCount1F[session$PunishedCount1>1] <- "more than 1"
session$PunishedCount2F <- session$PunishedCount2
session$PunishedCount2F[session$PunishedCount2==0] <- "no"
session$PunishedCount2F[session$PunishedCount2>1] <- "more than 1"
session$PunishedCount3F <- session$PunishedCount3
session$PunishedCount3F[session$PunishedCount3==0] <- "no"
session$PunishedCount3F[session$PunishedCount3>1] <- "more than 1"
session$UnmaskedCount1F <- session$UnmaskedCount1
session$UnmaskedCount1F[session$UnmaskedCount1==0] <- "no"
session$UnmaskedCount1F[session$UnmaskedCount1>1] <- "more than 1"
session$UnmaskedCount2F <- session$UnmaskedCount2
session$UnmaskedCount2F[session$UnmaskedCount2==0] <- "no"
session$UnmaskedCount2F[session$UnmaskedCount2>1] <- "more than 1"
session$UnmaskedCount3F <- session$UnmaskedCount3
session$UnmaskedCount3F[session$UnmaskedCount3==0] <- "no"
session$UnmaskedCount3F[session$UnmaskedCount3>1] <- "more than 1"
session$SomeonePunished1F <- session$SomeonePunishedCount1
session$SomeonePunished1F[session$SomeonePunishedCount1==0] <- "no"
session$SomeonePunished1F[session$SomeonePunishedCount1>1] <- "more than 1"
session$SomeonePunished2F <- session$SomeonePunishedCount2
session$SomeonePunished2F[session$SomeonePunishedCount2==0] <- "no"
session$SomeonePunished2F[session$SomeonePunishedCount2>1] <- "more than 1"
session$SomeonePunished3F <- session$SomeonePunishedCount3
session$SomeonePunished3F[session$SomeonePunishedCount3==0] <- "no"
session$SomeonePunished3F[session$SomeonePunishedCount3>1] <- "more than 1"
session$SomeoneUnmasked1F <- session$SomeoneUnmaskedCount1
session$SomeoneUnmasked1F[session$SomeoneUnmaskedCount1==0] <- "no"
session$SomeoneUnmasked1F[session$SomeoneUnmaskedCount1>1] <- "more than 1"
session$SomeoneUnmasked2F <- session$SomeoneUnmaskedCount2
session$SomeoneUnmasked2F[session$SomeoneUnmaskedCount2==0] <- "no"
session$SomeoneUnmasked2F[session$SomeoneUnmaskedCount2>1] <- "more than 1"
session$SomeoneUnmasked3F <- session$SomeoneUnmaskedCount3
session$SomeoneUnmasked3F[session$SomeoneUnmaskedCount3==0] <- "no"
session$SomeoneUnmasked3F[session$SomeoneUnmaskedCount3>1] <- "more than 1"

session$Vote1<-as.factor(session$Vote1)
session$Vote2<-as.factor(session$Vote2)
session$Vote3<-as.factor(session$Vote3)
session$order1<-as.factor(session$order1)
session$order2<-as.factor(session$order2)
session$order3<-as.factor(session$order3)
session$regime_order <-as.factor(session$regime_order)
#session$xScore <- as.numeric(levels(session$xScore))[session$xScore]
#session$yScore <- as.numeric(levels(session$yScore))[session$yScore]
session$ExpectedRole <- as.factor(session$ExpectedRole)
session$Treatment <- as.factor(session$Treatment)
session$subTreatment <- as.factor(session$subTreatment)
session$gender <- as.factor(session$gender)
#session$age <- as.numeric(levels(session$age))[session$age]
session$Group <- as.factor(session$Group)

session$vote1 <- factor(interaction(session$Vote1, as.numeric(session$ExpectedRole == "Observer")), levels = c('0.0','1.0', '0.1','1.1'), labels = c("NOREG", "REG", NA, NA))
session$vote2 <- factor(interaction(session$Vote2, as.numeric(session$ExpectedRole == "Observer")), levels = c('0.0','1.0', '0.1','1.1'), labels = c("NOREG", "REG", NA, NA))
session$vote3 <- factor(interaction(session$Vote3, as.numeric(session$ExpectedRole == "Observer")), levels = c('0.0','1.0', '0.1','1.1'), labels = c("NOREG", "REG", NA, NA))
session$votescomb <- interaction(session$vote1, session$vote2, session$vote3)
session$votesallReferenda <- (factor(session$votescomb, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG")))
session$votes2Referenda <- (factor(session$votesallReferenda, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG"), labels = c("NOREG|NOREG", "NOREG|NOREG", "NOREG|REG", "NOREG|REG", "REG|NOREG", "REG|NOREG", "REG|REG", "REG|REG")))

## distribute to subjects table
subjects <- merge(subset(subjects, select = -c(Profit, Vote1, Vote2, Vote3, order, xScore, yScore, gender, age, PunPerceived, ReasonVote, StrategyObserver, ChanceIdentification, StrategyTrader, RiskGeneral, RiskFinancial, LossAversion, Faculty, notes, Vote3, orderfinal, `left[1]` , `left[2]` , `left[3]` , `left[4]` , `left[5]` , `left[6]` , `left[7]` , `left[8]` , `left[9]` , `left[10]`)), 
                  subset(session, select = -c(Date, Treatment, Subject, ShowUpFee, ShowUpFeeInvested, MoneyAdded, `PPayoffs[1]`, `PPayoffs[2]`, `PPayoffs[3]`, `PPayoffs[4]`, `PPayoffs[5]`, `PPayoffs[6]`, `PPayoffs[7]`, `PPayoffs[8]`, `PPayoffs[9]`, `PPayoffs[10]`, `PPayoffs[11]`, `PPayoffs[12]`, Role, Group, subTreatment,regime_order,ExpectedRole, IsAuthority, IsInsider,Role,QuestionnaireParticipate)),
          by = c("subjectID"), all=T)
subjects$Vote1[subjects$Role == "Observer" | subjects$Role == "Experimenter"] <- NA
subjects$Vote2[subjects$Role == "Observer" | subjects$Role == "Experimenter"] <- NA
subjects$vote1 <- factor(subjects$Vote1, levels = c(0,1, 'NA'), labels = c("NOREG", "REG", NA))
subjects$vote2 <- factor(subjects$Vote2, levels = c(0,1, 'NA'), labels = c("NOREG", "REG", NA))

## Observer proficiency #######################################################
correctobs <- aggregate(cbind(Selected, (IsInsider*Selected), (IsInsider*Selected*(IsREG=="REG")), ((1-IsInsider)*Selected)) ~ Group + Period, data= subset(predictions), function(x) sum(x))
colnames(correctobs) <- c("Group", "Period", "NumSelected", "NumDetections", "NumPunished", "NumMissuspected")
observers <- merge(subset(subjects, Role == "Observer"), correctobs, by = c("Group", "Period"))
obsprof <- aggregate(cbind(Punished, Punished*(IsREG=="REG")) ~ Group, data = subset(subjects, Role != "Experimenter" & Role != "Observer" & IsREG != "Training"), function(x) sum(x))
inactiveObsPeriod <- aggregate(cbind(NumSelected, (NumSelected>0), NumDetections, (NumDetections>0), NumPunished, (NumPunished>0), NumMissuspected) ~ Group, data = subset(observers, IsREG != "Training"), function(x) sum(x))

subjects$IsTrader <- factor(subjects$Role, levels = c("Observer", "Experimenter", "Informed trader", "Uninformed trader"), labels = c("Observer", "Experimenter", "Trader", "Trader"))

subjects$Vote1 <- as.numeric(levels(subjects$Vote1))[subjects$Vote1]
subjects$Vote2 <- as.numeric(levels(subjects$Vote2))[subjects$Vote2]
subjects$Vote3 <- as.numeric(levels(subjects$Vote3))[subjects$Vote3]

## marketsummary table #######################################################
subjects <- subjects %>%
  dplyr::group_by(Group, Period, IsTrader) %>%
  dplyr::mutate(rankPDbefore = rank(PDbefore, ties.method = "first")) %>%
  dplyr::mutate(rankPDPun = rank(PDPun, ties.method = "first")) %>%
  dplyr::mutate(rankProfit = rank(ProfitPeriod, ties.method = "first")) %>%
  # ProfitPeriod provides the accumulated profit over both markets
  dplyr::mutate(relPDbefore = ((1+PDbefore)/sum(1+PDbefore))) %>%
  dplyr::mutate(cumPDbefore = order_by(rankPDbefore, cumsum(relPDbefore))) %>%
  dplyr::mutate(sumPDbefore = sum(1+PDbefore)) %>%
  dplyr::mutate(relPDPun = ((1+PDPun)/sum(1+PDPun))) %>%
  dplyr::mutate(cumPDPun = order_by(rankPDPun, cumsum(relPDPun))) %>%
  dplyr::mutate(sumPDPun = sum(1+PDPun)) %>%
  dplyr::mutate(relProfit = (ProfitPeriod/sum(ProfitPeriod))) %>%
  dplyr::mutate(cumProfit = order_by(rankProfit, cumsum(relProfit))) %>%
  dplyr::mutate(rankinitialAssets = rank(PersonalStockEndowment, ties.method = "first")) %>%
  dplyr::mutate(cumAssets = order_by(rankinitialAssets, cumsum(PersonalStockEndowment/30/9))) %>%
  dplyr::mutate(rankinitialEndowment = rank(InitialEndowment, ties.method = "first")) %>%
  dplyr::mutate(cumEndowment = order_by(rankinitialEndowment, cumsum(InitialEndowment/BBV/60/9)))
HHalpha <- 2

marketsummary <- aggregate(cbind(LimitVol, CompensationReceived, ProfitPeriod, (active=="active"), cumPDbefore, cumPDPun, cumProfit, cumAssets, cumEndowment, (PersonalStockEndowment/(9*30))^HHalpha, (Stock/(9*30))^HHalpha, (InitialEndowment/BBV/(9*60))^HHalpha, (EndVermoegen/BBV/(9*60))^HHalpha, ((EndVermoegen + CompensationReceived)/BBV/(9*60))^HHalpha, (Volume)^HHalpha, ((1+PDbefore)/sumPDbefore)^HHalpha, ((1+PDPun)/sumPDPun)^HHalpha) ~ Group + Period + Treatment + subTreatment + regime_order + BBV + BBVCent + IsREG, data=subset(subjects, IsTrader == "Trader"), FUN = sum, na.action = NULL, na.rm=T)
colnames(marketsummary)[colnames(marketsummary) %in% c("V4", paste0("V", 10:17))] <- c("NumActiveTrader", "HHInitialAssets", "HHEndAssets", "HHInitialEndowment", "HHEndEndowment", "HHEndEndowmentPun", "HHVolume", "HHPDbefore", "HHPDPun")
marketsummary$Phase[marketsummary$Period > 0] <- "Phases 1 & 2"
marketsummary$Phase[marketsummary$Period > 6] <- "Phase 3"
marketsummary$Phase[marketsummary$Period > 9] <- "Phase 4"

marketsummary <- merge(marketsummary, aggregate(cbind(PersonalStockEndowment^HHalpha, Stock^HHalpha, (InitialEndowment/BBV)^HHalpha, (EndVermoegen/BBV)^HHalpha, ((EndVermoegen + CompensationReceived)/BBV)^HHalpha, Volume^HHalpha, (1+PDbefore)^HHalpha, (1+PDPun)^HHalpha,  1 + PDbefore, 1 + PDPun, PersonalStockEndowment, Stock, InitialEndowment/BBV, EndVermoegen/BBV, (EndVermoegen + CompensationReceived)/BBV) ~ Group + Period, data=subset(subjects, IsTrader == "Trader" & (Volume > 0 | LimitVolume > 0)), FUN = sum, na.action = NULL, na.rm=T), by = c("Group", "Period"))
colnames(marketsummary)[colnames(marketsummary) %in% c(paste0("V",1:10), "PersonalStockEndowment", "Stock", "V13", "V14", "V15")] <- c("HHI_InitialAssets", "HHI_EndAssets", "HHI_InitialEndowment", "HHI_EndEndowment", "HHI_EndEndowmentPun", "HHI_Volume", "HHI_PDbefore", "HHI_PDPun", "sumPDbeforeActive", "sumPDPunActive", "InitialAssetsActive", "EndAssetsActive", "InitialEndowmentActive", "EndEndowmentActive", "EndEndowmentPunActive")

marketsummary <- merge(marketsummary, correctobs, by = c("Group", "Period"), all = T)

## Distribution ###############################################################
## Gini coefficient
marketsummary$GiniPDbefore <- (1-marketsummary$cumPDbefore/5) # 2 / (9 + 1) = 1 / 5
marketsummary$GiniPDPun <- (1-marketsummary$cumPDPun/5)
marketsummary$GiniProfit <- (1-marketsummary$cumProfit/5)
marketsummary$GiniAssets <- (1-marketsummary$cumAssets/5) 
marketsummary$GiniEndowment <- (1-marketsummary$cumEndowment/5) 

## Herfindahl–Hirschman index
marketsummary$HHI_InitialAssets <-  (marketsummary$HHI_InitialAssets / (marketsummary$InitialAssetsActive)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)
marketsummary$HHI_EndAssets <- (marketsummary$HHI_EndAssets / (marketsummary$EndAssetsActive)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)
marketsummary$HHI_InitialEndowment <- (marketsummary$HHI_InitialEndowment / (marketsummary$InitialEndowmentActive)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)
marketsummary$HHI_EndEndowment <- (marketsummary$HHI_EndEndowment / (marketsummary$EndEndowmentActive)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)
marketsummary$HHI_EndEndowmentPun <- (marketsummary$HHI_EndEndowmentPun / (marketsummary$EndEndowmentPunActive)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)
marketsummary$HHI_PDbefore <- (marketsummary$HHI_PDbefore / (marketsummary$sumPDbefore)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)
marketsummary$HHI_PDPun <- (marketsummary$HHI_PDPun / (marketsummary$sumPDPun)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader - 1)

## Volume by trader type ######################################################
## calculate volume of trader type correspondings in transactions
transactions <- merge(transactions, subset(subjects, select = c("Role", "Period", "subjectID")), by.y= c("Period", "subjectID"), by.x = c("Period", "MakerID"))
colnames(transactions)[colnames(transactions)=="Role"] <- "makerRole"
transactions <- merge( transactions, subset(subjects, select = c("Role", "Period", "subjectID")), by.y= c("Period", "subjectID"), by.x = c("Period", "TakerID"))
colnames(transactions)[colnames(transactions)=="Role"] <- "takerRole"
oo <- aggregate(cbind(transactionVol) ~ Group + Period + TakerRole + MakerRole, data = transactions, function(x) sum(x))
oo1 <- reshape2::dcast(subset(oo), Group + Period ~ paste0("Volume") + TakerRole + MakerRole, value.var = "transactionVol", drop = FALSE)
oo1 <- subset(oo1, select = c("Group", "Period", "Volume_Informed trader_Informed trader","Volume_Uninformed trader_Informed trader", "Volume_Informed trader_Uninformed trader", "Volume_Uninformed trader_Uninformed trader"))
colnames(oo1) <- c("Group", "Period", "Volume_Informed_Informed","Volume_Uninformed_Informed", "Volume_Informed_Uninformed","Volume_Uninformed_Uninformed")
oo1[is.na(oo1)] <- 0
marketsummary <- merge(marketsummary, oo1, by = c("Group", "Period"))

marketsummary$VolumeUni <- marketsummary$Volume_Informed_Uninformed + marketsummary$Volume_Uninformed_Informed + marketsummary$Volume_Uninformed_Uninformed
marketsummary$VolumeInf <- marketsummary$Volume_Informed_Informed + marketsummary$Volume_Informed_Uninformed + marketsummary$Volume_Uninformed_Informed

## calculate limit volume by trader type
ol <- aggregate(cbind(limitVolume) ~ Group + Period + MakerRole, data = offers, function(x) sum(x))
ol1 <- reshape2::dcast(subset(ol), Group + Period ~ paste0("limitVolume") + MakerRole, value.var = "limitVolume", drop = FALSE)
ol1 <- subset(ol1, select = c("Group", "Period", "limitVolume_Informed trader", "limitVolume_Uninformed trader"))
colnames(ol1) <- c("Group", "Period", "limitVolumeInf","limitVolumeUni")
ol1[is.na(ol1)] <- 0
marketsummary <- merge(marketsummary, ol1, by = c("Group", "Period"))

## Return volatility ##########################################################
Pwinsa <- quantile(transactions$Price, .995, na.rm = T)
Pwinsa1 <- quantile(transactions$Price, .005, na.rm = T)
transactions$Pricewins <- transactions$Price
transactions$Pricewins[transactions$Price>Pwinsa & is.na(transactions$Price)==F] <- Pwinsa
transactions$Pricewins[transactions$Price<Pwinsa1 & is.na(transactions$Price)==F] <- Pwinsa1
transactionssd <- subset(transactions, select = c(Period, Group, transactionIDm, Pricewins, Price))
transactionssd$transactionIDm <- transactionssd$transactionIDm+1
names(transactionssd)[names(transactionssd)=="Pricewins"] <- "L.Pricewins"
names(transactionssd)[names(transactionssd)=="Price"] <- "L.Price"
transactions <- merge(transactions, transactionssd, by = c("Period", "Group", "transactionIDm"), all.x = T)
transactions$returnwins <- log(transactions$Pricewins) - log(transactions$L.Pricewins)
transactions$return <- log(transactions$Price) - log(transactions$L.Price)
Rwind <- quantile(transactions$return, .995, na.rm = T)
Rwind1 <- quantile(transactions$return, .005, na.rm = T)
transactions$returnwins2 <- transactions$return
transactions$returnwins2[transactions$return>Rwind & is.na(transactions$return)==F] <- Rwind
transactions$returnwins2[transactions$return<Rwind1 & is.na(transactions$return)==F] <- Rwind1
mean <- aggregate(cbind(return, returnwins,returnwins2, 1) ~ Group + Period, data = transactions, function(x) sum(x))
colnames(mean) <- c("Group", "Period", "meanreturn", "meanreturnwins", "meanreturnwins2", "obsreturn")
sd <- aggregate(cbind(return, returnwins, returnwins2) ~ Group + Period, data = transactions, function(x) sd(x))
colnames(sd) <- c("Group", "Period", "volatility", "volatilitywins", "volatilitywins2")
marketsummary <- merge(marketsummary, mean, by = c("Group", "Period"), all = T)
marketsummary <- merge(marketsummary, sd, by = c("Group", "Period"), all = T)

# within 99.5% quantile
h <- aggregate(cbind(BAspreadwins, BAspreadwins2) ~ Period + Group, seconds, function(x) mean(x))
colnames(h) <- c("Period","Group", "meanBAspreadwins", "meanBAspreadwins2")
marketsummary <- merge(marketsummary, h, by = c("Group", "Period"))

## one row per trader and period
tradersummary <- subset(subjects, Subject!=11 & Role!="Observer" & Period>0) 
tradersummary$Phase[tradersummary$Period > 0] <- "Phases 1 & 2"
tradersummary$Phase[tradersummary$Period > 6] <- "Phase 3"
tradersummary$Phase[tradersummary$Period > 9] <- "Phase 4"

##profitable
transactions2 <- subset(transactions, Period>0)
transactions2$unprofit <- abs(transactions2$SellersProfit)
transactions2$subjectID <- transactions2$SellerID
transactions2$subjectID[transactions2$SellersProfit>0] <- transactions2$BuyerID[transactions2$SellersProfit>0]
unprofit2 <- aggregate(cbind(unprofit, transactionVol, 1) ~ subjectID + Period, data=transactions2, FUN = sum, na.action = NULL, na.rm=T)
colnames(unprofit2) <- c("subjectID", "Period", "TPUnProfitTransaction", "VolUnprofitTransaction", "NumUnprofitTransactions")
tradersummary <- merge(tradersummary, unprofit2, by = c("subjectID", "Period"), all = T)
tradersummary$TPUnProfitTransaction[is.na(tradersummary$TPUnProfitTransaction)==T] <- 0
tradersummary$VolUnprofitTransaction[is.na(tradersummary$VolUnprofitTransaction)==T] <- 0
tradersummary$NumUnprofitTransactions[is.na(tradersummary$NumUnprofitTransactions)==T] <- 0

## time of unexecuted profitable orders
contracts$Type <- 1
contracts$Type[contracts$type=="selling offer"] <- (-1)
contracts$unprofittime <- contracts$Type*(contracts$Price-contracts$BBV)*contracts$remainOfferVol*contracts$timecontract
contracts$unprofittime[contracts$unprofittime<=0] <- NA
contracts$unprofitvol <- contracts$Type*sign(contracts$Price-contracts$BBV)*contracts$remainOfferVol*contracts$timecontract
contracts$unprofitvol[contracts$unprofitvol<=0] <- NA
unprofittime <- aggregate(cbind(unprofittime, remainOfferVol*timecontract, unprofitvol) ~ Group + Period, data=contracts, FUN = sum, na.action = NULL, na.rm=T)
colnames(unprofittime) <- c("Group", "Period", "unprofittime", "voltime", "unprofitvol")
marketsummary <- merge(marketsummary, unprofittime, by = c("Group", "Period"), all = T)
marketsummary$unprofittime[is.na(marketsummary$unprofittime)] <- 0
marketsummary$RUPT <- marketsummary$unprofittime / marketsummary$BBV / marketsummary$voltime

## bid-ask spread
a <- aggregate(cbind(BestBid, BestAsk, BAspread, midpointBA) ~ Period + Group, subset(seconds,  time==180), FUN = function(x) mean(x, na.rm = T), na.action = NULL)
colnames(a) <- c( "Period","Group", "BestBid180", "BestAsk180", "BAspread180", "midpointBA180")
b <- aggregate(cbind(BestBid, BestAsk, BAspread, midpointBA) ~ Period + Group, subset(seconds,  time>=150), FUN = function(x) mean(x, na.rm = T), na.action = NULL)
colnames(b) <- c("Period","Group", "BestBid150", "BestAsk150", "BAspread150", "midpointBA150")
c <- aggregate(cbind(BestBid, BestAsk, BAspread, midpointBA, return, lastPrice) ~ Period + Group, seconds, FUN = function(x) mean(x, na.rm = T), na.action = NULL)
colnames(c) <- c("Period","Group", "meanBestBid", "meanBestAsk", "meanBAspread", "meanmidpointBA", "meanReturnPerSecond", "meanPrice")
marketsummary <- merge(marketsummary, a, by = c("Group", "Period"))
marketsummary <- merge(marketsummary, b, by = c("Group", "Period"))
marketsummary <- merge(marketsummary, c, by = c("Group", "Period"))
d <- aggregate(cbind( return, lnlastPrice) ~ Period + Group, seconds, FUN = function(x) sd(x, na.rm = T), na.action = NULL)
colnames(d) <- c("Period","Group", "sdreturnPerSecond", "sdPrice")
marketsummary <- merge(marketsummary, d, by = c("Group", "Period"))

marketsummary$BA_BBV <- marketsummary$meanmidpointBA - marketsummary$BBV
marketsummary$BA_BBV150 <- marketsummary$midpointBA150 - marketsummary$BBV
marketsummary$BA_BBV180 <- marketsummary$midpointBA180 - marketsummary$BBV
marketsummary$lnBA_BBV <- log(marketsummary$meanmidpointBA / marketsummary$BBV)
marketsummary$lnBA_BBV150 <- log(marketsummary$midpointBA150 / marketsummary$BBV)
marketsummary$lnBA_BBV180 <- log(marketsummary$midpointBA180 / marketsummary$BBV)

# profitpotential
marketprofitAbility <- aggregate(cbind(transactionVol, (transactionVol*abs(Price-BBV)), transactionVol*RDi, transactionVol*abs(RDi), transactionVol*GDi, transactionVol*abs(GDi), abs(log(57.5/BBV))*transactionVol, transactionVol*BBV ,  1) ~ Group + Period, data=transactions, FUN = sum, na.action = NULL, na.rm=T)
colnames(marketprofitAbility) <- c("Group", "Period", "transactionVol", "ProfitPotential", "SRDi", "SRADi", "SGDi", "SGADi", "SGADhypi", "VBBV" , "NumTransactions")
marketprofitAbility$GD <- exp(1/marketprofitAbility$transactionVol*marketprofitAbility$SGDi)-1
marketprofitAbility$GAD <- exp(1/marketprofitAbility$transactionVol*marketprofitAbility$SGADi)-1
marketprofitAbility$GADhyp <- exp(1/marketprofitAbility$transactionVol * marketprofitAbility$SGADhypi) - 1
marketprofitAbility$RD <- marketprofitAbility$SRDi/marketprofitAbility$VBBV
marketprofitAbility$RAD <- marketprofitAbility$SRADi/marketprofitAbility$VBBV
marketprofitAbility <- rbind(marketprofitAbility, c(8,10,0,0,NA,NA,NA, NA,NA,NA,0,0,0,0,0,0))
marketsummary <- merge(marketsummary, marketprofitAbility, by = c("Group", "Period"), all = T)
marketsummary$ProfitPotential[is.na(marketsummary$ProfitPotential)] <- 0
marketsummary$transactionVol[is.na(marketsummary$transactionVol)] <- 0
marketsummary$rGAD <- 1-marketsummary$GAD/marketsummary$GADhyp

marketprofitAbility120 <- aggregate(cbind(transactionVol, (transactionVol*abs(Price-BBV)), transactionVol*RDi, transactionVol*abs(RDi), transactionVol*GDi, transactionVol*abs(GDi), abs(log(57.5/BBV))*transactionVol, transactionVol*BBV ,  1) ~ Group + Period, data=subset(transactions, time >= 120), function(x) sum(x, na.rm=T), na.action = NULL)
colnames(marketprofitAbility120) <- c("Group", "Period", "transactionVol", "ProfitPotential", "SRDi", "SRADi", "SGDi", "SGADi", "SGADhypi", "VBBV" , "NumTransactions")
marketprofitAbility120$GD120 <- exp(1/marketprofitAbility120$transactionVol*marketprofitAbility120$SGDi)-1
marketprofitAbility120$GAD120 <- exp(1/marketprofitAbility120$transactionVol*marketprofitAbility120$SGADi)-1
marketprofitAbility120$GADhyp120 <- exp(1/marketprofitAbility120$transactionVol * marketprofitAbility120$SGADhypi) - 1
marketprofitAbility120$RD120 <- marketprofitAbility120$SRDi/marketprofitAbility120$VBBV
marketprofitAbility120$RAD120 <- marketprofitAbility120$SRADi/marketprofitAbility120$VBBV
marketsummary <- merge(marketsummary, subset(marketprofitAbility120, select = c(GD120, GAD120, GADhyp120, RD120, RAD120, Group, Period)), by = c("Group", "Period"), all = T)
marketsummary$rGAD120 <- 1-marketsummary$GAD120/marketsummary$GADhyp120

marketsummary <- merge(marketsummary, subset(globals, select = c(Group, Period, `BestAsk[1]`, `BestBid[1]`, `VLeg1[1]`, `VLeg2[1]`, Referendum1 ,Referendum2, `RateLeg1[1]`, `RateLeg2[1]`  )), by=c("Group", "Period"))

marketsummary$BASpreads <- marketsummary$`BestAsk[1]`-marketsummary$`BestBid[1]`
marketsummary$BAmidpoint <- (marketsummary$`BestAsk[1]`+ marketsummary$`BestBid[1]`)/2
marketsummary$logdiffBAmidFundamental <- log(marketsummary$BAmidpoint/marketsummary$BBV)
marketsummary$lnVolume <- log(marketsummary$transactionVol)

marketsummary$Referenum <- "Training"
marketsummary$Referendum[(marketsummary$Period>=1)] <- "Referendum 1"
marketsummary$Referendum[(marketsummary$Period>=7)] <- "Referendum 2"
marketsummary$Referendum[(marketsummary$Period>=10)] <- "Referendum 3"
marketsummary$Referendum <- as.factor(marketsummary$Referendum)



v3 <- (aggregate(Vote3 ~ Group, data = subset(subjects, Period==12), FUN = sum))
v3$Period <- 12
colnames(v3)[colnames(v3)=="Vote3"] <-"VLeg3[1]"
globals <- merge(globals, v3, by = c("Group", "Period"), all = T)
globals$`Election3[1]` <- 0
globals$`Election3[1]`[globals$`VLeg3[1]`>4] <- 1

## TraderVote table ###########################################################
TraderVote <- subset(session, Subject!=11 & Role!="Observer") 
TraderVote$vote1 <- factor(TraderVote$Vote1, levels = c(0,1), labels = c("NOREG", "REG"))
TraderVote$vote2 <- factor(TraderVote$Vote2, levels = c(0,1), labels = c("NOREG", "REG"))
TraderVote$vote3 <- factor(TraderVote$Vote3, levels = c(0,1), labels = c("NOREG", "REG"))
TraderVote$Sumvotes <- as.numeric(TraderVote$Vote1) + as.numeric(TraderVote$Vote2) + as.numeric(TraderVote$Vote3) - 3
TraderVote$ExpectedRole <- factor(TraderVote$ExpectedRole)

## Votes table ################################################################
Votes <- subset(tradersummary, Period==7 | Period==10 | Period==12, select = -c(Role, Profit, ProfitPeriod ,ProfitPeriodCalculator,ProfitPeriodPredictor, ProfitPeriodTrader, ProfitPeriodAuthority, Vermoegen, Vermoegen1, EndVermoegen,VermoegenStock,AvEndVermoegen, DiffAvEndVermoegen,PD,Stock, Money, PersonalMoneyEndowment,PersonalStockEndowment, InitialEndowment, InitialEndowmentUnits,
                                                                                      Volume, lnVolume, 
                                                                                      ShortStock,ShortMoney,Claimbase,CompensationReceived,OutgoingMoney, OutgoingStock, SortOrder, Punished,Purchases,PurchasedVol,Sales, SoldVol,AvgPrice,LimitVol,CancelledVol,
                                                                                      SumDividende,Inventory,Divges,ABTransaktionen, ABContracts,ABActions, Volume, MarktVolume,Transaktionen, MarktTransaktionen,Contracts, VolContracts,rf,re,Rendite, Profit1, Profit2, ProfitEnd, AMProfit,AMProfitEnd,AMPreis, OldAMPreis,OldmaxAMPreis,EroeffPreis, SchluPreis,MarketOrders,MarketOrders.y, AMAuszahlung,LastP,LastPEnd,LastPup, LastPdown,LastPfactor, LastPHelp, TaxRate,
                                                                                      pChart,LastTime,NumInsider,NumTrader, PaymentSubjectsTable, PaymentAuthoritySubjectsTable,PDtimesMP, SelectedPeriod,beenInsider, GuessInsiders, GuessTraders,answer,RightAnswers, num1,num2,num3, SumTransaktionen,SumContracts,SumActions,IsInsider, TagNum,FreeMoney,FreeStock, ScreenDisplay, GroupEET,ownPayoffEET, TimeForceExitFromThisStagePreExperimentOK, TimeForceExitFromThePeriodPreExperimentOK, TimeForceExitFromTheExperimentPreExperimentOK, TimeOKPreExperimentOK, 
                                                                                      TimeForceExitFromThisStageEETOK, TimeForceExitFromThePeriodEETOK, TimeForceExitFromTheExperimentEETOK, TimeBestaetigenEETOK, TimeForceExitFromThisStagePreControlQuestionsOK, TimeForceExitFromThePeriodPreControlQuestionsOK, TimeForceExitFromTheExperimentPreControlQuestionsOK, TimeOKPreControlQuestionsOK, TimeForceExitFromThisStagePreExperimentQuestionnairesOK,
                                                                                      TimeForceExitFromThePeriodPreExperimentQuestionnairesOK, TimeForceExitFromTheExperimentPreExperimentQuestionnairesOK, TimeBestaetigenPreExperimentQuestionnairesOK, TimeForceExitFromThisStagePreMarketOK, TimeForceExitFromThePeriodPreMarketOK, TimeForceExitFromTheExperimentPreMarketOK, TimeOKPreMarketOK, TimeOKVotingOK, TimeForceExitFromThisStageVotingOK, 
                                                                                      TimeForceExitFromThePeriodVotingOK, TimeForceExitFromTheExperimentVotingOK,TimeOKRoleInfoOK, TimeForceExitFromThisStageRoleInfoOK, TimeForceExitFromThePeriodRoleInfoOK, TimeForceExitFromTheExperimentRoleInfoOK, SumTax, OldVolume, TimeOKAuktionOK, TimeAuktionOK, TimeEinschaltenAuktionOK, TimeAusschaltenAuktionOK, TimeHaendlerCodeAuktionOK, TimeMengeAuktionOK, TimeMarktansichtAuktionOK,
                                                                                      TimeForceExitFromThisStageAuktionOK, TimeForceExitFromThePeriodAuktionOK, TimeForceExitFromTheExperimentAuktionOK, TimeTraderCodeAuktionOK, TimeQuantityAuktionOK, TimePriceAuktionOK, TimeOKCalculationTaskOK, TimeOKSelectionOK, TimeSelectionOK, TimeEinschaltenSelectionOK, TimeAusschaltenSelectionOK, TimeForceExitFromThisStageSelectionOK, TimeForceExitFromThePeriodSelectionOK, TimeForceExitFromTheExperimentSelectionOK, TimeForceExitFromThisStageHistoryOK, 
                                                                                      TimeForceExitFromThePeriodHistoryOK, TimeForceExitFromTheExperimentHistoryOK, TimeOKHistoryOK, TimeOKVotingFinalOK,TimeForceExitFromThisStageVotingFinalOK, TimeForceExitFromThePeriodVotingFinalOK, TimeForceExitFromTheExperimentVotingFinalOK, TimeForceExitFromThisStagePreSelectPeriodOK,
                                                                                      TimeForceExitFromThePeriodPreSelectPeriodOK, TimeForceExitFromTheExperimentPreSelectPeriodOK, TimeOKPreSelectPeriodOK, SelectedPeriodEET, TimeOKSelectPeriodOK, TimeForceExitFromThisStageSelectPeriodOK, TimeForceExitFromThePeriodSelectPeriodOK,TimeForceExitFromTheExperimentSelectPeriodOK, TimeOKFinalResultsOK, TimeForceExitFromThisStageFinalResultsOK, TimeForceExitFromThePeriodFinalResultsOK,TimeForceExitFromTheExperimentFinalResultsOK,
                                                                                      FacultyOther, TimeForceExitFromThisStageQuestionnairesOK, TimeForceExitFromThePeriodQuestionnairesOK, TimeForceExitFromTheExperimentQuestionnairesOK, TimeBestaetigenQuestionnairesOK, TimeShowOKPreControlQuestionsOK))
Votes$Referendum <- factor(Votes$Period, level = c(7,10,12), label = c("Referendum 1", "Referendum 2", "Referendum 3"))
Votes$vote1 <- factor(Votes$Vote1, levels = c(0,1), labels = c("NOREG", "REG"))
Votes$vote2 <- factor(Votes$Vote2, levels = c(0,1), labels = c("NOREG", "REG"))
Votes$vote3 <- factor(Votes$Vote3, levels = c(0,1), labels = c("NOREG", "REG"))
Votes$vote <- Votes$vote1
Votes$vote[Votes$Period==10] <- Votes$vote2[Votes$Period==10]
Votes$vote[Votes$Period==12] <- Votes$vote3[Votes$Period==12]
Votes$Vote <- 0
Votes$Vote[Votes$vote=="REG"] <- 1
Votes <- merge(Votes, Expectedroleagg, by = c("ExpectedRole"))
Votes <- merge(Votes, ExpectedroleaggP3, by = c("ExpectedRole"))
Votes <- merge(Votes, ExpectedroleaggP4, by = c("ExpectedRole"))

Votes$en1 <- (as.numeric(Votes$Referendum=="Referendum 2"|Votes$Referendum=="Referendum 3"))
Votes$Referendum1a <- relevel(Votes$Referendum1, ref = "REG")
Votes$Referendum2a <- relevel(Votes$Referendum2, ref = "REG")

## Distribution ##############################################################
marketsummary$HHI_Volume <- (marketsummary$HHI_Volume / (marketsummary$transactionVol*2)^HHalpha * marketsummary$NumActiveTrader - 1) / (marketsummary$NumActiveTrader/2 - 1)
marketsummary$HHVolume <- marketsummary$HHVolume / (marketsummary$transactionVol*2)^HHalpha

Votes <- Votes %>%
  dplyr::group_by(Group, Referendum) %>%
  dplyr::mutate(rankPDbefore = rank(AvgPDbefore1, ties.method = "first")) %>%
  dplyr::mutate(rankPDPun = rank(AvgPDPun1, ties.method = "first")) %>%
  dplyr::mutate(rankProfit = rank(AvgProfit1, ties.method = "first")) %>%
  # ProfitPeriod provides the accumulated profit over both markets
  dplyr::mutate(relPDbefore = ((1+AvgPDbefore1)/sum(1+AvgPDbefore1))) %>%
  dplyr::mutate(cumPDbefore = order_by(rankPDbefore, cumsum(relPDbefore))) %>%
  dplyr::mutate(sumPDbefore = sum(1+AvgPDbefore1)) %>%
  dplyr::mutate(relPDPun = ((1+AvgPDPun1)/sum(1+AvgPDPun1))) %>%
  dplyr::mutate(cumPDPun = order_by(rankPDPun, cumsum(relPDPun))) %>%
  dplyr::mutate(sumPDPun = sum(1+AvgPDPun1)) %>%
  dplyr::mutate(relProfit = ((AvgProfit1+30)/sum(AvgProfit1+30))) %>%
  dplyr::mutate(cumProfit = order_by(rankProfit, cumsum(relProfit))) %>%
  dplyr::mutate(rankinitialAssets = rank(AvgInitialAssets1, ties.method = "first")) %>%
  dplyr::mutate(cumAssets = order_by(rankinitialAssets, cumsum(AvgInitialAssets1/30/9))) %>%
  dplyr::mutate(rankinitialEndowment = rank(AvgInitialEndowment1, ties.method = "first")) %>%
  dplyr::mutate(cumEndowment = order_by(rankinitialEndowment, cumsum(AvgInitialEndowment1/60/9)))

## one row per Ballot
Votes <- merge(Votes, aggregate(cbind(cumPDbefore, cumPDPun, cumProfit, cumAssets, cumEndowment, (AvgInitialAssets1/(9*30))^HHalpha, (AvgEndAssets1/(9*30))^HHalpha, (AvgInitialEndowment1/(9*60))^HHalpha, (AvgEndEndowment1/(9*60))^HHalpha, ((AvgEndEndowment1 + AvgCompensationReceived1)/(9*60))^HHalpha, ((1+AvgPDbefore1)/sumPDbefore)^HHalpha, ((1+AvgPDPun1)/sumPDPun)^HHalpha) ~ Group + Period, data=subset(Votes), FUN = sum, na.action = NULL, na.rm=T), by = c("Group", "Period"))
colnames(Votes)[colnames(Votes) %in% c("cumPDbefore.x", "cumPDPun.x", "cumProfit.x", "cumAssets.x", "cumEndowment.x", "cumPDbefore.y", "cumPDPun.y", "cumProfit.y", "cumAssets.y", "cumEndowment.y", paste0("V", 6:12))] <- c("cumPDbefore", "cumPDPun", "cumProfit", "cumAssets", "cumEndowment", "sumcumPDbefore", "sumcumPDPun", "sumcumProfit", "sumcumAssets", "sumcumEndowment", "HHInitialAssets", "HHEndAssets", "HHInitialEndowment", "HHEndEndowment", "HHEndEndowmentPun", "HHPDbefore", "HHPDPun")

# Gini coefficient
Votes$GiniPDbefore <- (1-Votes$sumcumPDbefore/5) # 2 / (9 + 1) = 1 / 5
Votes$GiniPDPun <- (1-Votes$sumcumPDPun/5)
Votes$GiniProfit <- (1-Votes$sumcumProfit/5)
Votes$GiniAssets <- (1-Votes$sumcumAssets/5) 
Votes$GiniEndowment <- (1-Votes$sumcumEndowment/5) 

## Short sells ################################################################
transactions <- merge(transactions, subset(tradersummary, select = c(subjectID, Period, PersonalStockEndowment, Stock)),by.x=c("SellerID", "Period"), by.y=c("subjectID", "Period"))
colnames(transactions)[colnames(transactions)=="PersonalStockEndowment"] <- "SellersInitialAssets"         
colnames(transactions)[colnames(transactions)=="Stock"] <- "SellersFinalAssets" 
transactions <- transactions %>%
  group_by(Group, Period, SellerID) %>%
  arrange(time) %>%
  mutate(SellerSales = cumsum(transactionVol))
transactions$SellerBuyes <- 0
for(i in 1:max(transactions$transactionID)){
  k <- transactions$SellerID[transactions$transactionID == i]
  t <- transactions$time[transactions$transactionID == i]
  p <- transactions$Period[transactions$transactionID == i]
  transactions$SellerBuyes[transactions$transactionID == i] <- sum(transactions$transactionVol[transactions$BuyerID == k & transactions$time<t & transactions$Period == p])
}
transactions$SellerAssetsBefore <- with(SellersInitialAssets + SellerBuyes - SellerSales + transactionVol, data = transactions)
transactions$SellerAssetsAfter <- with(SellersInitialAssets + SellerBuyes - SellerSales, data = transactions)
transactions$shortsells <- 0
transactions$shortsells[transactions$SellerAssetsAfter < 0] <- transactions$transactionVol[transactions$SellerAssetsAfter < 0]
transactions$shortsells[transactions$SellerAssetsAfter < 0 & transactions$SellerAssetsBefore > 0] <- -transactions$SellerAssetsAfter[transactions$SellerAssetsAfter < 0 & transactions$SellerAssetsBefore > 0]
shortTable1 <- aggregate(shortsells ~ Period + SellerID, data = transactions, function(x) sum(x))
colnames(shortTable1)[colnames(shortTable1)=="SellerID"] <- "subjectID"
tradersummary <- merge(tradersummary, shortTable1, by = c("subjectID", "Period"), all = T)
tradersummary$shortsells[is.na(tradersummary$shortsells)] <- 0

shortTable <- aggregate(cbind(shortsells) ~ Group + Period, function(x) sum(x), data=subset(tradersummary))
marketsummary <- merge(marketsummary, shortTable, by = c("Group", "Period"), all = T)

## Margin buys ################################################################
transactions <- merge(transactions, subset(tradersummary, select = c(subjectID, Period, PersonalMoneyEndowment, Money)),by.x=c("BuyerID", "Period"), by.y=c("subjectID", "Period"))
colnames(transactions)[colnames(transactions)=="PersonalMoneyEndowment"] <- "BuyerInitialCash"         
colnames(transactions)[colnames(transactions)=="Money"] <- "BuyerFinalCash" 
transactions <- transactions %>%
  group_by(Group, Period, BuyerID) %>%
  arrange(time) %>%
  mutate(BuyerbuysTaler = cumsum(transactionVol*Price))
transactions$BuyerSellsTaler <- 0
for(i in 1:max(transactions$transactionID)){
  k <- transactions$BuyerID[transactions$transactionID == i]
  t <- transactions$time[transactions$transactionID == i]
  p <- transactions$Period[transactions$transactionID == i]
  transactions$BuyerSellsTaler[transactions$transactionID == i] <- sum(transactions$Price[transactions$SellerID == k & transactions$time<t & transactions$Period == p]*transactions$transactionVol[transactions$SellerID == k & transactions$time<t & transactions$Period == p])
}
transactions$BuyerCashBefore <- with(BuyerInitialCash - BuyerbuysTaler + BuyerSellsTaler + transactionVol*Price, data = transactions)
transactions$BuyerCashAfter <- with(BuyerInitialCash - BuyerbuysTaler + BuyerSellsTaler, data = transactions)
transactions$marginbuysTaler <- 0
transactions$marginbuysTaler[transactions$BuyerCashAfter < 0] <- transactions$transactionVol[transactions$BuyerCashAfter < 0] * transactions$Price[transactions$BuyerCashAfter < 0]
transactions$marginbuysTaler[transactions$BuyerCashAfter < 0 & transactions$BuyerCashBefore > 0] <- -transactions$BuyerCashAfter[transactions$BuyerCashAfter < 0 & transactions$BuyerCashBefore > 0]
transactions$marginbuysAsset <- 0
transactions$marginbuysAsset[transactions$BuyerCashAfter < 0] <- transactions$transactionVol[transactions$BuyerCashAfter < 0]
transactions$marginbuysAsset[transactions$BuyerCashAfter < 0 & transactions$BuyerCashBefore > 0] <- -transactions$BuyerCashAfter[transactions$BuyerCashAfter < 0 & transactions$BuyerCashBefore > 0] / transactions$Price[transactions$BuyerCashAfter < 0 & transactions$BuyerCashBefore > 0]
marginTable1 <- aggregate(cbind(marginbuysTaler, marginbuysAsset) ~ Period + BuyerID, data = transactions, function(x) sum(x))
colnames(marginTable1)[colnames(marginTable1)=="BuyerID"] <- "subjectID"
tradersummary <- merge(tradersummary, marginTable1, by = c("subjectID", "Period"), all = T)
tradersummary$marginbuysTaler[is.na(tradersummary$marginbuysTaler)] <- 0
tradersummary$marginbuysAsset[is.na(tradersummary$marginbuysAsset)] <- 0

marginTable <- aggregate(cbind(marginbuysTaler, marginbuysAsset) ~ Group + Period, function(x) sum(x), data=subset(tradersummary))
marketsummary <- merge(marketsummary, marginTable, by = c("Group", "Period"), all = T)
marketsummary$marginbuys <- marketsummary$marginbuysTaler / marketsummary$BBV
tradersummary$marginbuys <- tradersummary$marginbuysTaler / tradersummary$BBV

shortTableR <- aggregate(cbind(shortsells, marginbuys, marginbuysAsset) ~ Group + Period + Role, data = tradersummary, function(x) sum(x))
shortTableR1 <- reshape2::dcast(subset(shortTableR), Group + Period ~ paste0("shortsells") + Role, value.var = "shortsells", drop = FALSE)
shortTableR2 <- reshape2::dcast(subset(shortTableR), Group + Period ~ paste0("marginbuys") + Role, value.var = "marginbuys", drop = FALSE)
shortTableR3 <- reshape2::dcast(subset(shortTableR), Group + Period ~ paste0("marginbuysAsset") + Role, value.var = "marginbuysAsset", drop = FALSE)
shortTableR1 <- merge(shortTableR1, shortTableR2, by = c("Group", "Period"))
shortTableR1 <- merge(shortTableR1, shortTableR3, by = c("Group", "Period"))
shortTableR1 <- subset(shortTableR1, select = c("Group", "Period", "shortsells_Informed trader","shortsells_Uninformed trader", "marginbuys_Informed trader","marginbuys_Uninformed trader", "marginbuysAsset_Informed trader","marginbuysAsset_Uninformed trader"))
colnames(shortTableR1) <- c("Group", "Period", "shortsells_Informed","shortsells_Uninformed", "marginbuys_Informed","marginbuys_Uninformed", "marginbuysAsset_Informed","marginbuysAsset_Uninformed")
shortTableR1[is.na(shortTableR1)] <- 0
marketsummary <- merge(marketsummary, shortTableR1, by = c("Group", "Period"))

avgtraderprofit2 <- aggregate(cbind(Volume, LimitVolume, TradingProfit, TPUnits, TPUnitsPun, ProfitPeriod, (Volume > 0 | LimitVolume > 0), PDbefore, PDPun, PDRedist, PDbeforeVol, PDPunVol, PDRedistVol, PDbefore*as.numeric(PDbefore>0), PDPun*as.numeric(PDPun>0), PDRedist*as.numeric(PDRedist>0), PDbefore*as.numeric(PDbefore<0), PDPun*as.numeric(PDPun<0), PDRedist*as.numeric(PDRedist<0), shortsells, marginbuys, marginbuysAsset) ~ Role + subjectID + Group, data=tradersummary, function(x) mean(x))
avgtraderprofit <- reshape2::dcast(subset(avgtraderprofit2), subjectID + Group ~ paste0("PDbefore") + Role, value.var = "PDbefore", drop = T)
colnames(avgtraderprofit) <- c("subjectID", "Group", "AvgPDbeforeInf", "AvgPDbeforeUni")
avgtraderprofit <- avgtraderprofit %>%
  dplyr::group_by(Group) %>%
  dplyr::mutate(rankavgPDbeforeUni = rank(AvgPDbeforeUni, ties.method = "first")) %>%
  dplyr::mutate(rankavgPDbeforeInf = rank(AvgPDbeforeInf, ties.method = "first"))

tradersummary <- merge(tradersummary, avgtraderprofit, by = c("subjectID", "Group"))
tradersummary$rankavgPDbeforeRole <- tradersummary$rankavgPDbeforeInf
tradersummary$rankavgPDbeforeRole[tradersummary$Role == "Uninformed trader"] <- tradersummary$rankavgPDbeforeUni[tradersummary$Role == "Uninformed trader"]

## voteGroup table ############################################################
voteGroup1 <- aggregate(cbind(Vote1,Vote2,Vote3, UnmaskedCount1, UnmaskedCount2, UnmaskedCount3) ~ Group + Treatment + subTreatment + regime_order + SomeoneUnmasked1 + SomeoneUnmasked2 + SomeoneUnmasked3 + SomeonePunished1 + SomeonePunished2 + SomeonePunished3 + SomeoneUnmaskedCount1 + SomeoneUnmaskedCount2 + SomeoneUnmaskedCount3 + SomeonePunishedCount1 + SomeonePunishedCount2 + SomeonePunishedCount3 + Referendum1 + Referendum2, data=TraderVote, FUN=sum, na.rm=T)
voteGroup <- aggregate(cbind((cbind(Vote1,Vote2,Vote3, UnmaskedCount1, UnmaskedCount2, UnmaskedCount3)), (Vote1+Vote2+Vote3), vote=="REG", vote=="REG" & ExpectedRole == "Uninformed trader") ~ Referendum + Group + Treatment + subTreatment + regime_order + SomeoneUnmasked1 + SomeoneUnmasked2 + SomeoneUnmasked3 + SomeonePunished1 + SomeonePunished2 + SomeonePunished3 + SomeoneUnmaskedCount1 + SomeoneUnmaskedCount2 + SomeoneUnmaskedCount3 + SomeonePunishedCount1 + SomeonePunishedCount2 + SomeonePunishedCount3 + Referendum1 + Referendum2 + HHInitialAssets + HHEndAssets + HHInitialEndowment + HHEndEndowment + HHEndEndowmentPun + HHPDbefore + HHPDPun, data=Votes, FUN=sum, na.rm=T)
colnames(voteGroup)[colnames(voteGroup) %in% c("V7", "V8", "V9")] <- c("sumVote", "Vote", "VoteUni")
voteGroupScoreMean <- aggregate(cbind(xScore, yScore) ~ Group, data=Votes, function(x) mean(x))
colnames(voteGroupScoreMean) <- c("Group", "MeanxScore", "MeanyScore")
voteGroupScoresd <- aggregate(cbind(xScore, yScore) ~ Group, data=Votes, function(x) sd(x))
colnames(voteGroupScoresd) <- c("Group", "sdxScore", "sdyScore")
voteGroup <- merge(voteGroup, voteGroupScoreMean, by = c("Group"))
voteGroup <- merge(voteGroup, voteGroupScoresd, by = c("Group"))
Groupstat <- aggregate((cbind(GAD, GD, RAD, RD, BBVCent, abs(BBVCent), RUPT, transactionVol, NumActiveTrader, NumSelected, NumDetections, NumPunished, NumMissuspected)) ~ Group, data=subset(marketsummary, Period > 0 & Period < 7), FUN=sum, na.rm=T)
voteGroup <- merge(voteGroup, Groupstat, by = c("Group"))
voteGroup$subTreatment <- factor(voteGroup$subTreatment, levels = c("FLUCT", "FIXED"))

## rename/set variables #######################################################
marketsummary$Period0 <- (as.numeric(marketsummary$Period)-1) %% 6
marketsummary$Period0[marketsummary$Phase == "Phase 4"] <- marketsummary$Period0[marketsummary$Phase == "Phase 4"] - 3
tradersummary$Period0 <- (as.numeric(tradersummary$Period)-1) %% 6
tradersummary$Period0[tradersummary$Phase == "Phase 4"] <- tradersummary$Period0[tradersummary$Phase == "Phase 4"] - 3
seconds$Period0 <- (as.numeric(seconds$Period)-1) %% 6
seconds$Period0[seconds$Phase == "Phase 4"] <- seconds$Period0[seconds$Phase == "Phase 4"] - 3
tradersummary$Active <- 0
tradersummary$Active[tradersummary$active=="active"] <- 1
tradersummary$CAratio <- tradersummary$PersonalMoneyEndowment/tradersummary$BBV/tradersummary$PersonalStockEndowment
tradersummary$vote3 <- factor(tradersummary$Vote3, levels = c(0,1), labels = c("NOREG", "REG"))
tradersummary$votescomb <- interaction(tradersummary$vote1, tradersummary$vote2, tradersummary$vote3)
tradersummary$votesallReferenda <- (factor(tradersummary$votescomb, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG")))
tradersummary$votes2Referenda <- (factor(tradersummary$votesallReferenda, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG"), labels = c("NOREG|NOREG", "NOREG|NOREG", "NOREG|REG", "NOREG|REG", "REG|NOREG", "REG|NOREG", "REG|REG", "REG|REG")))
tradersummary$votes <- (factor(tradersummary$votes2Referenda, levels = c("NOREG|NOREG", "NOREG|REG", "REG|NOREG", "REG|REG"), labels = c("NOREG", "switching", "switching", "REG")))
tradersummary$votesall <- (factor(tradersummary$votescomb, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG"), labels = c("NOREG", "switching", "switching", "switching", "switching", "switching", "switching", "REG")))

voteGroup$en2 <- (as.numeric(voteGroup$Referendum=="Referendum 2"|voteGroup$Referendum=="Referendum 3"))
voteGroup$en3 <- (as.numeric(voteGroup$Referendum=="Referendum 3"))
voteGroup$Referendum1a <- relevel(voteGroup$Referendum1, ref = "REG")
voteGroup$Referendum2a <- relevel(voteGroup$Referendum2, ref = "REG")

Votes$votescomb <- interaction(Votes$vote1, Votes$vote2, Votes$vote3)
#Votes$votes <- (factor(Votes$votes, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG"), labels = c(1:8)))
Votes$votesallReferenda <- (factor(Votes$votescomb, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG")))
Votes$votes2Referenda <- (factor(Votes$votesallReferenda, levels = c("NOREG.NOREG.NOREG", "NOREG.NOREG.REG", "NOREG.REG.NOREG", "NOREG.REG.REG", "REG.NOREG.NOREG", "REG.NOREG.REG", "REG.REG.NOREG", "REG.REG.REG"), labels = c("NOREG|NOREG", "NOREG|NOREG", "NOREG|REG", "NOREG|REG", "REG|NOREG", "REG|NOREG", "REG|REG", "REG|REG")))

Votes$ExpectedRole <- factor(Votes$ExpectedRole, levels = c("FLUCT", "Informed trader", "Uninformed trader"))
Votes$female <- factor(Votes$gender, levels = c("male", "diverse", "female"), labels = c("other", "other", "female"))
Votes$male <- factor(Votes$gender, levels = c("diverse", "female", "male"), labels = c("other", "other", "male"))
Votes$aget <- Votes$age
Votes$aget[Votes$aget>quantile(Votes$age, probs = .995)] <- quantile(Votes$age, probs = .995)
marketsummary$subTreatment <- factor(marketsummary$subTreatment, levels = c("FLUCT", "FIXED"))

Votes <- merge(Votes, avgtraderprofit, by = c("subjectID", "Group"))
Votes$rankavgPDbeforeRole <- Votes$rankavgPDbeforeInf
Votes$rankavgPDbeforeRole[Votes$Role == "Uninformed trader"] <- Votes$rankavgPDbeforeUni[Votes$Role == "Uninformed trader"]

Votes$xScoreAll <- Votes$xScore
Votes$yScoreAll <- Votes$yScore
Votes$xScoreAll[is.na(Votes$xScore)] <- 0
Votes$yScoreAll[is.na(Votes$yScore)] <- 0
Votes$EETMono <- factor((Votes$EET != "Non-monotonic"), levels = c(T, F), labels = c("Monotonic", "Non-monotonic"))

marketAgg <- aggregate(cbind(GAD, GAD120, meanBAspreadwins2, volatility, volatilitywins2) ~ IsREG + Group, subset(marketsummary, Period < 7), function(x) mean(x))
Votes <- merge(Votes, reshape2::dcast(subset(marketAgg), Group ~ paste0("GAD1") + IsREG, value.var = "GAD", drop = FALSE), by = "Group")
Votes <- merge(Votes, reshape2::dcast(subset(marketAgg), Group ~ paste0("GAD120") + IsREG, value.var = "GAD120", drop = FALSE), by = "Group")
Votes <- merge(Votes, reshape2::dcast(subset(marketAgg), Group ~ paste0("meanBAspreadwins2") + IsREG, value.var = "meanBAspreadwins2", drop = FALSE), by = "Group")
Votes <- merge(Votes, reshape2::dcast(subset(marketAgg), Group ~ paste0("volatility") + IsREG, value.var = "volatility", drop = FALSE), by = "Group")

save.image("RawData.RData")

marketsummary <- subset(marketsummary, select = c("Group", "Period", "Period0", "Phase", "Treatment", "subTreatment", "regime_order", "BBV", "BBVCent", "IsREG", "Referendum1", "Referendum2",
                                                  "transactionVol", "VolumeUni", "VolumeInf", "Volume_Informed_Informed", "Volume_Uninformed_Informed", "Volume_Informed_Uninformed", "Volume_Uninformed_Uninformed", "NumTransactions",
                                                  "LimitVol", "limitVolumeInf", "limitVolumeUni", "NumActiveTrader", 
                                                  "HHInitialAssets", "HHEndAssets", "HHInitialEndowment", "HHEndEndowment", "HHEndEndowmentPun", "HHVolume", "HHPDbefore", "HHPDPun","HHI_InitialAssets", "HHI_EndAssets", "HHI_InitialEndowment", "HHI_EndEndowment", "HHI_EndEndowmentPun", "HHI_Volume", "HHI_PDbefore", "HHI_PDPun",
                                                  "GiniPDbefore", "GiniPDPun", "GiniProfit", "GiniAssets", "GiniEndowment",
                                                  "BestBid180", "BestAsk180", "BAspread180", "midpointBA180", "BestBid150", "BestAsk150", "BAspread150", "midpointBA150", "BA_BBV", "BA_BBV150", "BA_BBV180", "lnBA_BBV",
                                                  "meanBestBid", "meanBestAsk", "meanBAspread", "meanmidpointBA", "meanBAspreadwins", "meanBAspreadwins2", "meanreturn", "meanreturnwins", "meanreturnwins2", 
                                                  "volatility", "volatilitywins", "volatilitywins2", "meanReturnPerSecond", "meanPrice", "sdreturnPerSecond", "sdPrice",
                                                  "ProfitPotential", "unprofittime", "RUPT", "GD", "GAD", "GADhyp","RD", "RAD", "rGAD", "GD120", "GAD120", "GADhyp120", "RD120", "RAD120", "rGAD120",
                                                  "NumSelected", "NumDetections", "NumPunished", "NumMissuspected",
                                                  "shortsells", "marginbuysTaler", "marginbuysAsset", "marginbuys", "shortsells_Informed", "shortsells_Uninformed", "marginbuys_Informed", "marginbuys_Uninformed", "marginbuysAsset_Informed", "marginbuysAsset_Uninformed"
))

tradersummary$Assets <- tradersummary$Stock
tradersummary$InitialMoney <- tradersummary$PersonalMoneyEndowment
tradersummary$InitialAssets <- tradersummary$PersonalStockEndowment
tradersummary$EndEndowment <- tradersummary$EndVermoegen
tradersummary$EndEndowmentPun <- tradersummary$EndVermoegen + tradersummary$CompensationReceived
tradersummary$trades <- tradersummary$Transaktionen


tradersummary <- subset(tradersummary, select = c("subjectID", "Group", "Date", "Subject", "Period", "Period0", "Phase",  "Treatment", "subTreatment", "regime_order", "BBV", "BBVCent", "IsREG", "Role", "ExpectedRole",
                                                  "vote1", "vote2", "vote3", "votesallReferenda", "votes2Referenda", "order1", "order2", "order3", "Referendum1", "Referendum2",
                                                  "InitialAssets", "Assets", "InitialMoney", "Money", "InitialEndowment", "EndEndowment",  "EndEndowmentPun", "InitialEndowmentUnits", "EndEndowmentUnits", "EndEndowmentUnitsPun", 
                                                  "TradingProfit", "CompensationReceived", "TPRedist", "TPPun", "TPUnits", "TPUnitsRedist", "TPUnitsPun", "ProfitPeriod", "PDbefore", "PDRedist", "PDPun", "PDbeforeVol", "PDRedistVol", "PDPunVol", 
                                                  "AvgPDbeforeInf", "AvgPDbeforeUni", "rankavgPDbeforeUni", "rankavgPDbeforeInf", "rankavgPDbeforeRole", 
                                                  "Volume", "LimitVolume", "CancelledVol", "MarketVolume", "transactedLimitVol", "PurchasedVol", "SoldVol",
                                                  "Trades", "MarketOrders", "LimitTrades", "LimitOrders", "Purchases", "Sales", "active", 
                                                  "TPUnProfitTransaction", "VolUnprofitTransaction", "NumUnprofitTransactions", 
                                                  "shortsells", "marginbuysTaler", "marginbuysAsset", "marginbuys",
                                                  "xScore", "yScore", "xScoreAll", "yScoreAll", "EET",
                                                  "Unmasked", "Punished",
                                                  "Role.1", "ProfitPeriod.1", "TradingProfit.1", "CompensationReceived.1", "PDbefore.1", "PDRedist.1", "PDPun.1", "PDbeforeVol.1", "PDRedistVol.1", "PDPunVol.1", "Unmasked.1", "IsREG.1", "IsInsider.1",
                                                  "Role.2", "ProfitPeriod.2", "TradingProfit.2", "CompensationReceived.2", "PDbefore.2", "PDRedist.2", "PDPun.2", "PDbeforeVol.2", "PDRedistVol.2", "PDPunVol.2", "Unmasked.2", "IsREG.2", "IsInsider.2",
                                                  "Role.3", "ProfitPeriod.3", "TradingProfit.3", "CompensationReceived.3", "PDbefore.3", "PDRedist.3", "PDPun.3", "PDbeforeVol.3", "PDRedistVol.3", "PDPunVol.3", "Unmasked.3", "IsREG.3", "IsInsider.3",
                                                  "Role.4", "ProfitPeriod.4", "TradingProfit.4", "CompensationReceived.4", "PDbefore.4", "PDRedist.4", "PDPun.4", "PDbeforeVol.4", "PDRedistVol.4", "PDPunVol.4", "Unmasked.4", "IsREG.4", "IsInsider.4",
                                                  "Role.5", "ProfitPeriod.5", "TradingProfit.5", "CompensationReceived.5", "PDbefore.5", "PDRedist.5", "PDPun.5", "PDbeforeVol.5", "PDRedistVol.5", "PDPunVol.5", "Unmasked.5", "IsREG.5", "IsInsider.5",
                                                  "Role.6", "ProfitPeriod.6", "TradingProfit.6", "CompensationReceived.6", "PDbefore.6", "PDRedist.6", "PDPun.6", "PDbeforeVol.6", "PDRedistVol.6", "PDPunVol.6", "Unmasked.6", "IsREG.6", "IsInsider.6",
                                                  "Role.7", "ProfitPeriod.7", "TradingProfit.7", "CompensationReceived.7", "PDbefore.7", "PDRedist.7", "PDPun.7", "PDbeforeVol.7", "PDRedistVol.7", "PDPunVol.7", "Unmasked.7", "IsREG.7", "IsInsider.7",
                                                  "Role.8", "ProfitPeriod.8", "TradingProfit.8", "CompensationReceived.8", "PDbefore.8", "PDRedist.8", "PDPun.8", "PDbeforeVol.8", "PDRedistVol.8", "PDPunVol.8", "Unmasked.8", "IsREG.8", "IsInsider.8",
                                                  "Role.9", "ProfitPeriod.9", "TradingProfit.9", "CompensationReceived.9", "PDbefore.9", "PDRedist.9", "PDPun.9", "PDbeforeVol.9", "PDRedistVol.9", "PDPunVol.9", "Unmasked.9", "IsREG.9", "IsInsider.9",
                                                  "Role.10", "ProfitPeriod.10", "TradingProfit.10", "CompensationReceived.10", "PDbefore.10", "PDRedist.10", "PDPun.10", "PDbeforeVol.10", "PDRedistVol.10", "PDPunVol.10", "Unmasked.10", "IsREG.10", "IsInsider.10",
                                                  "Role.11", "ProfitPeriod.11", "TradingProfit.11", "CompensationReceived.11", "PDbefore.11", "PDRedist.11", "PDPun.11", "PDbeforeVol.11", "PDRedistVol.11", "PDPunVol.11", "Unmasked.11", "IsREG.11", "IsInsider.11",
                                                  "Role.12", "ProfitPeriod.12", "TradingProfit.12", "CompensationReceived.12", "PDbefore.12", "PDRedist.12", "PDPun.12", "PDbeforeVol.12", "PDRedistVol.12", "PDPunVol.12", "Unmasked.12", "IsREG.12", "IsInsider.12",
                                                  "PunPerceived", "ReasonVote", "ChanceIdentification", "StrategyTrader", "RiskGeneral", "RiskFinancial", "LossAversion", "Faculty", "FacultyOther", "gender", "age", "notes",                                                      
                                                  "AvgPRREG1", "AvgPDbeforeREG1", "AvgPDRedistREG1", "AvgPDPunREG1", "AvgPDbeforeVolREG1", "AvgPDRedistVolREG1", "AvgPDPunVolREG1",
                                                  "AvgPRNOREG1", "AvgPDbeforeNOREG1", "AvgPDRedistNOREG1", "AvgPDPunNOREG1", "AvgPDbeforeVolNOREG1", "AvgPDRedistVolNOREG1", "AvgPDPunVolNOREG1",
                                                  "AvgPR1", "AvgPDbefore1", "AvgPDRedist1", "AvgPDPun1", "AvgPDbeforeVol1", "AvgPDRedistVol1", "AvgPDPunVol1",
                                                  "IsInsiderCount1", "IsInsiderCount2", "IsInsiderCount3", "PunishedCount1", "PunishedCount2", "PunishedCount3", "UnmaskedCount1", "UnmaskedCount2", "UnmaskedCount3", 
                                                  "sdProfit", "sdPDbefore", "sdPDRedist", "sdPDPun", "sdPDbeforeVol", "sdPDRedistVol", "sdPDPunVol"
))

subjects <- subset(subjects, select = c("subjectID", "Group", "Date", "Subject", "Period", "Treatment", "subTreatment", "regime_order", "BBV", "BBVCent", "IsREG", "Role", "ExpectedRole", "IsExperimenter", "IsInsider",
                                        "vote1", "vote2", "vote3", "votesallReferenda", "votes2Referenda", "order1", "order2", "order3", "Referendum1", "Referendum2",
                                        "TotalProfit", "ProfitPeriod",
                                        "xScore", "yScore", "xScoreAll", "yScoreAll", "EET", 
                                        "PunPerceived", "ReasonVote", "StrategyObserver", "ChanceIdentification", "StrategyTrader", "RiskGeneral", "RiskFinancial", "LossAversion", "Faculty", "FacultyOther", "gender", "age", "notes"
))

Votes <- subset(Votes, select = c("subjectID", "Group", "Date", "Phase", "Referendum", "Treatment", "subTreatment", "regime_order", "ExpectedRole",
                                  "vote", "vote1", "vote2", "vote3", "votesallReferenda", "votes2Referenda", "order1", "order2", "order3", "Referendum1", "Referendum2", "en1", "Referendum1a", "Referendum2a", 
                                  "AvgPRREG1", "AvgPDbeforeREG1", "AvgPDRedistREG1", "AvgPDPunREG1", "AvgPDbeforeVolREG1", "AvgPDRedistVolREG1", "AvgPDPunVolREG1",
                                  "AvgPRNOREG1", "AvgPDbeforeNOREG1", "AvgPDRedistNOREG1", "AvgPDPunNOREG1", "AvgPDbeforeVolNOREG1", "AvgPDRedistVolNOREG1", "AvgPDPunVolNOREG1",
                                  "AvgPR1", "AvgPDbefore1", "AvgPDRedist1", "AvgPDPun1", "AvgPDbeforeVol1", "AvgPDRedistVol1", "AvgPDPunVol1",
                                  "IsInsiderCount1", "IsInsiderCount2", "IsInsiderCount3", "PunishedCount1", "PunishedCount2", "PunishedCount3", "UnmaskedCount1", "UnmaskedCount2", "UnmaskedCount3", 
                                  "sdProfit", "sdPDbefore", "sdPDRedist", "sdPDPun", "sdPDbeforeVol", "sdPDRedistVol", "sdPDPunVol",
                                  "xScore", "yScore", "xScoreAll", "yScoreAll", "EET", "EETMono",
                                  "Role.1", "ProfitPeriod.1", "TradingProfit.1", "CompensationReceived.1", "PDbefore.1", "PDRedist.1", "PDPun.1", "PDbeforeVol.1", "PDRedistVol.1", "PDPunVol.1", "Unmasked.1", "IsREG.1", "IsInsider.1",
                                  "Role.2", "ProfitPeriod.2", "TradingProfit.2", "CompensationReceived.2", "PDbefore.2", "PDRedist.2", "PDPun.2", "PDbeforeVol.2", "PDRedistVol.2", "PDPunVol.2", "Unmasked.2", "IsREG.2", "IsInsider.2",
                                  "Role.3", "ProfitPeriod.3", "TradingProfit.3", "CompensationReceived.3", "PDbefore.3", "PDRedist.3", "PDPun.3", "PDbeforeVol.3", "PDRedistVol.3", "PDPunVol.3", "Unmasked.3", "IsREG.3", "IsInsider.3",
                                  "Role.4", "ProfitPeriod.4", "TradingProfit.4", "CompensationReceived.4", "PDbefore.4", "PDRedist.4", "PDPun.4", "PDbeforeVol.4", "PDRedistVol.4", "PDPunVol.4", "Unmasked.4", "IsREG.4", "IsInsider.4",
                                  "Role.5", "ProfitPeriod.5", "TradingProfit.5", "CompensationReceived.5", "PDbefore.5", "PDRedist.5", "PDPun.5", "PDbeforeVol.5", "PDRedistVol.5", "PDPunVol.5", "Unmasked.5", "IsREG.5", "IsInsider.5",
                                  "Role.6", "ProfitPeriod.6", "TradingProfit.6", "CompensationReceived.6", "PDbefore.6", "PDRedist.6", "PDPun.6", "PDbeforeVol.6", "PDRedistVol.6", "PDPunVol.6", "Unmasked.6", "IsREG.6", "IsInsider.6",
                                  "Role.7", "ProfitPeriod.7", "TradingProfit.7", "CompensationReceived.7", "PDbefore.7", "PDRedist.7", "PDPun.7", "PDbeforeVol.7", "PDRedistVol.7", "PDPunVol.7", "Unmasked.7", "IsREG.7", "IsInsider.7",
                                  "Role.8", "ProfitPeriod.8", "TradingProfit.8", "CompensationReceived.8", "PDbefore.8", "PDRedist.8", "PDPun.8", "PDbeforeVol.8", "PDRedistVol.8", "PDPunVol.8", "Unmasked.8", "IsREG.8", "IsInsider.8",
                                  "Role.9", "ProfitPeriod.9", "TradingProfit.9", "CompensationReceived.9", "PDbefore.9", "PDRedist.9", "PDPun.9", "PDbeforeVol.9", "PDRedistVol.9", "PDPunVol.9", "Unmasked.9", "IsREG.9", "IsInsider.9",
                                  "Role.10", "ProfitPeriod.10", "TradingProfit.10", "CompensationReceived.10", "PDbefore.10", "PDRedist.10", "PDPun.10", "PDbeforeVol.10", "PDRedistVol.10", "PDPunVol.10", "Unmasked.10", "IsREG.10", "IsInsider.10",
                                  "Role.11", "ProfitPeriod.11", "TradingProfit.11", "CompensationReceived.11", "PDbefore.11", "PDRedist.11", "PDPun.11", "PDbeforeVol.11", "PDRedistVol.11", "PDPunVol.11", "Unmasked.11", "IsREG.11", "IsInsider.11",
                                  "Role.12", "ProfitPeriod.12", "TradingProfit.12", "CompensationReceived.12", "PDbefore.12", "PDRedist.12", "PDPun.12", "PDbeforeVol.12", "PDRedistVol.12", "PDPunVol.12", "Unmasked.12", "IsREG.12", "IsInsider.12",
                                  "PunPerceived", "ReasonVote", "ChanceIdentification", "StrategyTrader", "RiskGeneral", "RiskFinancial", "LossAversion", "Faculty", "gender", "age", "notes"

))

voteGroup <- subset(voteGroup, select = c("Group", "Treatment", "subTreatment", "regime_order", "Referendum", "Referendum1", "Referendum2", 
                                          "HHInitialAssets", "HHEndAssets", "HHInitialEndowment", "HHEndEndowment", "HHEndEndowmentPun", "HHPDbefore", "HHPDPun",
                                          "Vote1", "Vote2", "Vote3", "sumVote", "Vote", "VoteUni", "MeanxScore", "MeanyScore", "sdxScore", "sdyScore", "GAD", "GD", "RAD", "RD", "RUPT", "en2", "en3", "Referendum1a", "Referendum2a"
  
))

observers <- subset(observers, select = c("subjectID", "Group", "Date", "Subject", "Period",  "Treatment", "subTreatment", "regime_order", "BBV", "BBVCent", "IsREG", "Role", "ExpectedRole",
                                          "Referendum1", "Referendum2", "ProfitPeriod", "TotalProfit",
                                          "NumSelected", "NumDetections", "NumPunished", "NumMissuspected",
                                          "xScore", "yScore", "xScoreAll", "yScoreAll", "EET",
                                          "PunPerceived", "StrategyObserver", "RiskGeneral", "RiskFinancial", "LossAversion", "Faculty", "FacultyOther", "gender", "age", "notes"
))

transactions <- subset(transactions, select = c("transactionID", "Group", "Date", "Period", "Treatment", "subTreatment", "regime_order", "BBV", "BBVCent", "IsREG",
                                                "type", "MakerID", "TakerID", "MakerRole", "TakerRole", "Rolematching",
                                                "Price", "Pricewins", "L.Pricewins", "L.Price", "returnwins", "return", "returnwins2", 
                                                "transactionVol", "remainOfferVol", "SellersProfit",
                                                "shortsells", "marginbuysTaler", "marginbuysAsset", "time"
))  


offers <- subset(offers, select = c("offerID", "Group", "Period", "Treatment", "subTreatment", "BBV", "BBVCent", "IsREG",
                                                "type", "status", "MakerID", "MakerRole",
                                                "Price", "limitVolume", "TotTransacted",
                                                "offerStart", "offerEnd"
)) 

seconds <- subset(seconds, select = c("time", "Group", "Period", "Period0", "subTreatment", "regime_order", "BBVCent", "IsREG",
                                      "MA", "BestBid", "BestAsk", "BAspread", "midpointBA", "BestAskwins", "BestBidwins", "BAspreadwins", "BAspreadwins2",
                                      "lastPrice", "lnlastPrice", "L.lnlastPrice", "return"
))

save(file = "Data.RData", list = c("marketsummary", "tradersummary", "subjects", "Votes", "voteGroup", "observers", "transactions", "offers", "seconds", "avgtraderprofit"))

