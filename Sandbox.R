library(tidyr)


uus <- rawOrig[288,]

rawOrig$NAME <- rawOrig$NAME %>% str_replace_all(intToUtf8(160),"")