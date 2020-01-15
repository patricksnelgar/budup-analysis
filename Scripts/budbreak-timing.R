require(tidyverse)
require(magrittr)
require(lubridate)

getValidColumns <- function(data){
	names <- colnames(data)
	asDates <- dmy(paste(names, "2019"))
	return(which(!is.na(asDates)))
}

mergeLogAndTranspose <- function(data, log, IDcolumn = 1){
	longFormat <- data %>%
		gather(key = "Date", value = "OpenBuds", -IDcolumn) %>%
		group_by_at(colnames(data)[IDcolumn]) %>%
		mutate(BudbreakPercentage = OpenBuds / max(OpenBuds)) %>%
		arrange_at(colnames(data)[1])
	
	longFormat$information <- sapply(1:nrow(longFormat), function(x) log$information[log$CaneID == longFormat$CaneID[x] & log$Date == longFormat$Date[x]][1])
	
	longFormat %<>% mutate(Date = dmy(paste(Date, "2019")))
	
	return(longFormat)
}

fillDataFrame <- function(data, IDcolumn = 1){
	processed <- select(data,
						c(IDcolumn, getValidColumns(data)))
	
	log <-  data.frame(CaneID = double(), Date = character(), information = character(), stringsAsFactors = FALSE)
	
	for(i in seq_along(processed[[1]])){
		maxValue <- 0
		
		for(j in seq_along(processed)[-1]){
			if(any(c("x", "X", "xx", NA) %in% processed[[i,j]]))
				processed[[i,j]] <- maxValue
			
			if(processed[[i,j]] > maxValue)
				maxValue <- processed[[i,j]]
			
			if(processed[[i,j]] < maxValue){
				log %<>% bind_rows(., data.frame(CaneID = processed[i,1], 
												 Date = names(processed)[j],
												 information = paste("Value smaller than max: previous =", processed[[i,j]], " new =", maxValue),
												 stringsAsFactors = FALSE))
				processed[[i,j]] <-  maxValue
			}
		}
	}
	
	return(mergeLogAndTranspose(processed, log))
}

treatmentsG11 <- read_csv("./data/treatmentsG11.csv")
treatmentsR19 <- read_csv("./data/treatmentsR19.csv")

#replace 'x' with NA so columns are read as numeric
rawBB_G11 <- read_csv("./data/G11_budbreak.csv", na = c("x", NA))

processedG11_budbreak <- fillDataFrame(rawBB_G11)

processedG11_budbreak$Treatment <- sapply(seq_along(processedG11_budbreak$CaneID), 
										  function(x) treatmentsG11$Treatment[processedG11_budbreak$CaneID[x] == treatmentsG11$CaneID])

rawBB_R19 <- read_csv("./data/R19_budbreak.csv", na = c("x", NA)) %>%
	rename(CaneID = 1)

processedR19_budbreak <- fillDataFrame(rawBB_R19)

processedR19_budbreak$Treatment <- sapply(seq_along(processedR19_budbreak$CaneID), 
										  function(x) treatmentsR19$Treatment[processedR19_budbreak$CaneID[x] == treatmentsR19$CaneID])


write_csv(processedG11_budbreak, "./output/G11_budbreak_longformat.csv")
write_csv(processedR19_budbreak, "./output/R19_budbreak_longformat.csv")
