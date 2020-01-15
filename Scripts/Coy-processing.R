require(coyparser)
require(tidyverse)

dataR19 <- read_csv("data/R19 Coy 2019.csv") %>%
			rename(CaneID = 1 , coy = 2) %>%
			filter(batchValidateCoy(coy))

processedR19 <- CoyProcessor(dataR19)

write_csv(processedR19,"./output/R19_processed_COY.csv")


dataG11 <- read_csv("data/G11 COY.csv") %>%
			rename(CaneID = 1, coy = 2) %>%
			filter(batchValidateCoy(coy))

processedG11 <- CoyProcessor(dataG11)

write_csv(processedG11, "output/G11_processed_COY.csv")
