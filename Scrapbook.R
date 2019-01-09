


#2003 ei toimi kunnolla
# #----------------------------
# #2003 data
# 
# wrangleData2003 <- function(){
#   
#   partyName2003 <- c("Suomen Keskusta",
#                      "Kansallinen Kokoomus", 
#                      "Suomen Sosialidemokraattinen Puolue",
#                      "Vasemmistoliitto",
#                      "Vihreä Liitto",
#                      "Suomen Kristillisdemokraatit",
#                      "Ruotsalainen Kansanpuolue",
#                      "Perussuomalaiset")
#   
#   
#   province2003 <- c("HELSINKI - HELSINGFORS",
#                     "UUSIMAA - NYLAND",
#                     "VARSINAIS-SUOMI - EGENTLIGA FINLAND",
#                     "SATAKUNTA",
#                     "HÄME - TAVASTLAND",
#                     "PIRKANMAA - BIRKALAND",
#                     "KYMI - KYMMENE",
#                     "ETELÄ-SAVO - SÖDRA SAVOLAX - SOUTH SAVO",
#                     "POHJOIS-SAVO - NORRA SAVOLAX - NORTH SAVO",
#                     "POHJOIS-KARJALA - NORRA KARELEN - NORTH KARELIA",
#                     "VAASA - VASA",
#                     "KESKI-SUOMI - MELLERSTA FINLAND - CENTRAL FINLAND ",
#                     "OULU - ULEÅBORG",
#                     "LAPPI - LAPPLAND - LAPLAND")
#   
#   provinceCode2003 <- c("HEL","UUS","VAR","SAT","HÄM","PIR",
#                         "KYM","ESA","PSA","PKA","VAA","KES",
#                         "OUL","LAP")
#   
#   #Luetaan vain kerran
#   
#   wb <- loadWorkbook("evaa_2003_2004-05-31_tau_026.xls")
#   rawdata <- as_tibble(readWorksheet(wb,1,startRow=10))
#   
#   colnames(rawdata) <- c("NAME","N","DHONDT")
#   rawdata <- rawdata %>% 
#     mutate(NAME=str_trim(NAME))
#   rawdata$YEAR <- 2003
#   
#   #2007 data has weird mixture of chr(32) and chr(160) for spaces. This solution screws up names, but makes provinces OK
#   rawdata$NAME <- rawdata$NAME %>% 
#     str_replace_all(intToUtf8(160),"") %>%
#     str_replace_all(intToUtf8(32),"")
#   
#   #Strip the data from the provinces
#   provinceStripped <- province2003 %>% 
#     str_replace_all(intToUtf8(32),"")
#   
#   
#   
#   rawdata$PROVINCENUM <- NA
#   maxInd <- nrow(rawdata)
#   provinceInd <- which(rawdata$NAME %in% provinceStripped)
#   for (ind in 1:length(provinceInd)){
#     rawdata$PROVINCENUM[provinceInd[ind]:maxInd] <- ind
#   }
#   rawdata <- rawdata %>% 
#     mutate(PROVINCE = provinceCode2003[PROVINCENUM])
#   
#   
#   
#   #Find where group changes
#   rawdata$GROUPNUM <- NA
#   rawdata$GROUPNAME <- NA
#   maxInd <- nrow(rawdata)
#   switchInd <- which(is.na(rawdata$NAME))
#   for (ind in 1:length(switchInd)){
#     rawdata$GROUPNUM[switchInd[ind]:maxInd] <- ind
#     rawdata$GROUPNAME[switchInd[ind]:maxInd] <- rawdata$NAME[switchInd[ind]+1]  
#   }
#   
#   rawdata <- rawdata %>% 
#     group_by(GROUPNUM) %>% 
#     mutate(
#       PARTY = substr(GROUPNAME,1,2)) %>%
#     filter(PARTY %in% partyCode) %>%
#     ungroup()
#   
#   rawdata <- rawdata %>% 
#     filter(N != "") %>%
#     mutate(N = str_replace(N,fixed(" "),"")) %>%
#     mutate(N = str_replace(N,intToUtf8(160),"")) %>%
#     mutate(N=as.numeric(N),PASSED=str_detect(NAME,fixed("*")))
#   
#   #Clean unnecessary columns
#   rawdata <- rawdata %>%
#     mutate(PROVINCENUM=NULL, GROUPNUM=NULL, GROUPNAME=NULL)
#   
#   as_tibble(rawdata)
# }



