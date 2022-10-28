library(readr)
library(stringr)
library(tidyr)
library(tibble)

#bpa <- read_csv("bases/BPA_TESTE.txt")

bpa <- read_csv("bases/R0507 - FINAL.txt")

unidade_index <- list()
for(i in 1:nrow(bpa)){
  unidade_index[[i]] <- ifelse(substr(bpa[i,],1,7) == "UNIDADE", bpa[i,], NA)
}
unidade_index <- do.call(rbind, unidade_index) %>% as.data.frame()
unidade_index$V1 <- ifelse(!is.na(unidade_index$V1), substr(unidade_index$V1, 12,18),NA) 
names(unidade_index) <- "cnes"
unidade_index <- unidade_index %>% tidyr::fill(cnes,.direction = "down")
unidade_index$index <- 1:nrow(unidade_index) %>% as.character()



bpa_index <- list()
for(i in 1:nrow(bpa)){
  bpa_index[[i]] <- paste0(substr(bpa[i,], 1,2), substr(bpa[i,], 4,7), substr(bpa[i,], 9,11)) %>% as.numeric()
}
bpa_index <- do.call(rbind, bpa_index) %>% as.data.frame()
bpa_index$index <- 1:nrow(bpa_index) %>% as.character()
bpa_index <- na.omit(bpa_index)
bpa$index <- 1:nrow(bpa) %>% as.character()
bpa <- subset(bpa, index %in% bpa_index$index)
bpa <- merge(bpa, unidade_index, by = "index", all.x = T)

bpa$index <- NULL
names(bpa) <- c("raw", "cnes")
bpa$cmp <- substr(bpa$raw, 1,7)
bpa$cmp <- str_trim(bpa$cmp) 

bpa$flh <- substr(bpa$raw, 9,11)
bpa$flh <- str_trim(bpa$flh)

bpa$sq <- substr(bpa$raw, 13,14)
bpa$sq <- str_trim(bpa$sq)

bpa$proc <- substr(bpa$raw, 16,26)
bpa$proc <- str_trim(bpa$proc)

bpa$cbo <- substr(bpa$raw, 28,33)
bpa$cbo <- str_trim(bpa$cbo)

bpa$qt_prz <- substr(bpa$raw, 34,41) 
bpa$qt_prz <- sub("[//.]", "", bpa$qt_prz)
bpa$qt_prz <- str_trim(bpa$qt_prz) %>% as.numeric()

bpa$vl_prz <- substr(bpa$raw, 42,52) 
bpa$vl_prz <- sub("[//.]", "", bpa$vl_prz)
bpa$vl_prz <- sub("[//,]", ".", bpa$vl_prz)
bpa$vl_prz <- str_trim(bpa$vl_prz) %>% as.numeric()

bpa$qt_apvd <- substr(bpa$raw, 53,60) 
bpa$qt_apvd <- sub("[//.]", "", bpa$qt_apvd)
bpa$qt_apvd <- str_trim(bpa$qt_apvd) %>% as.numeric()

bpa$vl_apvd <- substr(bpa$raw, 61,71) 
bpa$vl_apvd <- sub("[//.]", "", bpa$vl_apvd)
bpa$vl_apvd <- sub("[//,]", ".", bpa$vl_apvd)
bpa$vl_apvd <- str_trim(bpa$vl_apvd) %>% as.numeric()

bpa$situacao <- substr(bpa$raw, 73,200)
bpa$situacao <- str_trim(bpa$situacao)
bpa$raw <- NULL

write.csv(bpa, "bases/bpa.csv", row.names = F)
