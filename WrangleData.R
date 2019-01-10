
library(dplyr)
library(ggplot2)
library(stringr)
library(XLConnect)

dataDir <- "RawData"

minListSize <- 10

###LÃ¤hde: http://tilastokeskus.fi/til/evaa/tau.html

partyCode <- c("KE",
               "KO", 
               "SD",
               "VA",
               "VI",
               "KD",
               "RP",
               "PS")




#Assumes that data formatted correctly 
#Sorted correctly by rank
cleanData <- function(indata){
  data <- indata %>% 
    group_by(YEAR,PROVINCE,PARTY) %>%
    mutate(
      N.REL = N/max(N),
      RANK=rank(-N,ties.method="random"),
      LISTSUM=sum(N),
      LISTPASSED = sum(PASSED),
      LISTSIZE=max(rank(N))) %>%
    filter(LISTSIZE>minListSize) %>%
    ungroup()
  
  #Estimate threshold
  data <- data %>%
    group_by(YEAR,PROVINCE) %>%
    mutate(THRESHOLD = min(DHONDT[PASSED==TRUE])) %>%
    ungroup()
  
  data <- data %>% 
    mutate(RANK.REL=(RANK-1)/LISTSIZE,
           PASSED.REL = LISTPASSED/LISTSIZE)
  
  cleanData <- as_tibble(data)
  
}







#----------------------------
#2007 data

wrangleData2007 <- function(){
  
  partyName2007 <- c("KESK - CENT",
                     "KOK - SAML", 
                     "SDP",
                     "VAS - VÃÄNST - LEFT",
                     "VIHR - GRÖNA - GREENS",
                     "KD",
                     "RKP - SFP",
                     "PS - SAF")
  
  
  province2007 <- c("HELSINKI - HELSINGFORS",
                    "UUSIMAA  - NYLAND",
                    "VARSINAIS-SUOMI - EGENTLIGA FINLAND",
                    "SATAKUNTA",
                    "HÄME - TAVASTLAND",
                    "PIRKANMAA - BIRKALAND",
                    "KYMI - KYMMENE",
                    "ETELÄ-SAVO - SÖDRA SAVOLAX - SOUTH SAVO",
                    "POHJOIS-SAVO - NORRA SAVOLAX - NORTH SAVO",
                    "POHJOIS-KARJALA - NORRA KARELEN - NORTH KARELIA",
                    "VAASA - VASA",
                    "KESKI-SUOMI - MELLERSTA FINLAND - CENTRAL FINLAND",
                    "OULU - ULEÅBORG",
                    "LAPPI - LAPPLAND - LAPLAND")
  
  provinceCode2007 <- c("HEL","UUS","VAR","SAT","HAM","PIR",
                        "KYM","ESA","PSA","PKA","VAA","KES",
                        "OUL","LAP")
  
  #Luetaan vain kerran
  
  wb <- loadWorkbook(paste0(dataDir,"/evaa_2007_2007-03-28_tau_015.xls"))
  rawdata <- as_tibble(readWorksheet(wb,1,startRow=10))
  
  
  colnames(rawdata) <- c("NAME","N","DHONDT")
  rawdata <- rawdata %>% 
    mutate(NAME=str_trim(NAME))
  rawdata$YEAR <- 2007
  
  #2007 data has weird mixture of chr(32) and chr(160) for spaces. Careful with provinces!
  rawdata$NAME <- rawdata$NAME %>% 
    str_replace_all(intToUtf8(160),intToUtf8(32)) 
  
  
  #Strip the data from the provinces
  #provinceStripped <- province2007 %>% 
  #  str_replace_all(intToUtf8(32),"")
  
  
  
  rawdata$PROVINCENUM <- NA
  maxInd <- nrow(rawdata)
  provinceInd <- which(rawdata$NAME %in% province2007)
  for (ind in 1:length(provinceInd)){
    rawdata$PROVINCENUM[provinceInd[ind]:maxInd] <- ind
  }
  
  rawdata <- rawdata %>% 
    mutate(PROVINCE = provinceCode2007[PROVINCENUM])
  
  
  #Find where group changes
  rawdata$GROUPNUM <- NA
  rawdata$GROUPNAME <- NA
  maxInd <- nrow(rawdata)
  switchInd <- which(is.na(rawdata$NAME))
  for (ind in 1:length(switchInd)){
    rawdata$GROUPNUM[switchInd[ind]:maxInd] <- ind
    rawdata$GROUPNAME[switchInd[ind]:maxInd] <- rawdata$NAME[switchInd[ind]+1]  
  }
  
  rawdata <- rawdata %>% 
    group_by(GROUPNUM) %>% 
    mutate(
      PARTY = substr(GROUPNAME,1,2)) %>%
    filter(PARTY %in% partyCode) %>%
    ungroup()
  
  rawdata <- rawdata %>% 
    filter(N != "") %>%
    mutate(N = str_replace(N,fixed(" "),"")) %>%
    mutate(N=as.numeric(N),PASSED=str_detect(NAME,fixed("*"))) %>%
    mutate(NAME=str_remove_all(NAME,fixed("*")))
  
  #Clean unnecessary columns
  rawdata <- rawdata %>%
    mutate(PROVINCENUM=NULL, GROUPNUM=NULL, GROUPNAME=NULL)
  
  rawdata <- rawdata %>%
    arrange(YEAR,PROVINCE,PARTY,desc(N)) 
  
  as_tibble(rawdata)
}


wrangleData2015 <- function(){
  
  
  partyName2015 <- c("KESK", 
                     "KOK", 
                     "SDP",
                     "VAS", 
                     "VIHR", 
                     "KD", 
                     "RKP", 
                     "PS") 
  
  partyRecode2015 <- c(KESK="KE",
                       KOK="KO", 
                       SDP="SD",
                       VAS="VA",
                       VIHR="VI",
                       KD="KD",
                       RKP="RK",
                       PS="PS")
  
  provinceRecode2015 <- c("Helsingin vaalipiiri"="HEL",
                          "Uudenmaan vaalipiiri"="UUS",
                          "Varsinais-Suomen vaalipiiri"="VAR",
                          "Satakunnan vaalipiiri"="SAT",
                          "Hämeen vaalipiiri"="HAM",
                          "Pirkanmaan vaalipiiri"="PIR",
                          "Kaakkois-Suomen vaalipiiri"="KAA",
                          "Savo-Karjalan vaalipiiri"="SAV",
                          "Vaasan vaalipiiri"="VAA",
                          "Keski-Suomen vaalipiiri"="KES",
                          "Oulun vaalipiiri"="OUL",
                          "Lapin vaalipiiri"="LAP")
  
  provinceCode2015 <- c("HEL","UUS","VAR","SAT","HAM","PIR",
                        "KAA", "SAV", "VAA","KES", "OUL","LAP")
  
  data <- as_tibble(read.csv2(paste0(dataDir,"/2015_150_evaa_105.csv"),skip=1))
  
  
  slashLoc <- unlist(str_locate_all(data$Ehdokas,"/"))
  nameEnd <- slashLoc[seq(1,length(slashLoc),4)]-2
  partyStart <- slashLoc[seq(1,length(slashLoc),4)]+2
  partyEnd <- slashLoc[seq(2,length(slashLoc),4)]-2
  provinceStart <- slashLoc[seq(2,length(slashLoc),4)]+2
  
  data <- data %>%
    mutate(YEAR=2015) %>%
    rename(N=Äänimäärä,
           DHONDT=Vertausluku) %>%
    mutate(NAME=substr(Ehdokas,1,nameEnd),
           PartyName = substr(Ehdokas,partyStart,partyEnd),
           ProvinceName = substr(Ehdokas,provinceStart,length(Ehdokas))) %>%
    filter(PartyName %in% partyName2015) %>%
    mutate(PARTY=recode(PartyName,!!!partyRecode2015),
           PROVINCE=recode(ProvinceName,!!!provinceRecode2015),
           PASSED=(Valintatieto==1)) %>%
    mutate(Ehdokas=NULL,PartyName=NULL,ProvinceName=NULL,
           Valintatieto=NULL) %>%
    arrange(YEAR,PROVINCE,PARTY,desc(N))  
}




if (T){
  data2007 <- as_tibble(wrangleData2007())
  data2015 <- as_tibble(wrangleData2015())
  
  
  alldata <- as_tibble(rbind(data2007,data2015))
  cleandata <- cleanData(alldata)
  
  write.csv(cleandata,file="CleanData.csv")
}


