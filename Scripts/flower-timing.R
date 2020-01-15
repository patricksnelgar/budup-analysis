require(tidyverse)
require(magrittr)
require(lubridate)

validColumns <- function(colNames){
	asDates <- dmy(paste(colNames, "2019", ""))
	return(which(!is.na(asDates)))
}

mergeLogAndTranspose <- function(data, log, IDcolumn = 1){
	longFormat <- data %>%
		gather(key = "Date", value = "FlowerCount", -IDcolumn) %>%
		group_by_at(colnames(data)[IDcolumn]) %>%
		mutate(FlowerPercentage = FlowerCount / max(FlowerCount)) %>%
		arrange_at(colnames(data)[1])
	
	longFormat$information <- sapply(1:nrow(longFormat), function(x) log$information[log$CaneID == longFormat$CaneID[x] & log$Date == longFormat$Date[x]][1])
	
	longFormat %<>% mutate(Date = dmy(paste(Date, "2019")))
	
	return(longFormat)
}

updateFlowerNumbers <- function(originalData, IDcolumn = 1){
	
	data <-  select(originalData, 
					c(IDcolumn, validColumns(colnames(originalData))))
	
	log <- data.frame(CaneID = double(), Date = character(), information = character(), stringsAsFactors = FALSE)
	
	
	for(i in seq_along(data[[1]])) {
		
		maxValue <- 0
		
		for(j in 2:(length(data[i,]))) {
			
			if(any(c("x", "X", "xx", NA) %in% data[[i,j]]))
				data[[i,j]] <- maxValue
			
			if(data[[i,j]] > maxValue)
				maxValue <- as.numeric(data[[i,j]])
			
			if(data[[i,j]] < maxValue){
				
				log %<>% rbind(., data.frame(CaneID = data[i,1],
											 Date = names(data)[j],
											 information = paste("Value smaller than max: previous =", data[i,j], ": new =", maxValue)))
				data[[i, j]] <- maxValue
			}
			
		}
	}
	
	return(mergeLogAndTranspose(data, log))
}


rawG11 <- read_csv("./data/G11_flowering.csv")

processedG11_flowering <- updateFlowerNumbers(rawG11)

write_csv(processedG11_flowering, "./output/G11_flowering_longFormat.csv")

###################################  R19  ##########################################################

rawR19 <- read_csv("./data/R19_flowering.csv")

rawR19 %>% 
	filter(!is.na(X5)) %>%
	count()

rawR19 %>% 
	filter(!is.na(X7)) %>%
	count()

rawR19 %>%
	filter(!is.na(`29-Oct`)) %>%
	count()

hasX <- apply(rawR19, 1, function(x) any(x %in% c("x", "X", "xx")))

rawR19 %>%
	filter(hasX) %>%
	count()


# filter 'usable' rows == has an x or a total count - use remainder columns to establish if total count exists
filteredR19 <- 
	rawR19 %>% filter(hasX | !is.na(`29-Oct`) | !is.na(X5) | !is.na(X7))

#write_csv(processedR19, "workspace/usable_canes.csv")

processedR19_flowering <- updateFlowerNumbers(filteredR19)

write_csv(processedR19_flowering, "./output/R19_flowering_longFormat.csv")