#
# Written by Bartjan de Vries
#

# install.packages(c('tinytex', 'rmarkdown'))
# tinytex::install_tinytex()

# If you get error messages that pandoc is required either:
# - install pandoc on your system
# - if you have Rstudio installed run Sys.getenv("RSTUDIO_PANDOC") inside Rstudio, enter that directory in the line below and uncomment both lines

# PANDOC_DIR <- "/usr/lib/rstudio/bin/pandoc"
# Sys.setenv(RSTUDIO_PANDOC=PANDOC_DIR)

PANDOC_DIR <- Sys.getenv("RSTUDIO_PANDOC")

library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(ggplot2)
library(rmarkdown)
library(parallel)

fileDir <- "input"
outputDir <- "output"
rowToGrabUnitsFrom <- 2
noOfGatesToDisplay <- 2
authors <- c("Tetje C. van der Sluis")

TukeyTestAlpha <- .05

# make graphs look fancy
generalTheme <- theme(plot.title = element_text(hjust = 0.5))
perDayTheme <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

cleanColNames <- function(colNames) {
  colNames <- sapply(str_split(colNames, "\\|"), function(x) {
    name <- paste(x[1])
    if(str_detect(x[2], "Mean")){ name <- paste0(name, "|", x[2])}
    return(name)
  })
  colNames <- (sapply(str_split(colNames, "/"), function(x){paste(x[(length(x)-noOfGatesToDisplay + 1):length(x)], collapse = "/")}))
  substr(colNames, 1, 1) <- toupper(substr(colNames, 1, 1))
  colNames <- str_squish(str_trim(colNames))
  return(colNames)
}

extractUnits <- function(dataRow){
  dataUnits <- str_split(dataRow, " ")
  dataUnits <- sapply(dataUnits, function(x){str_replace(x[length(x)], "[0-9]+", "")})
  dataUnits[dataUnits == ""] <- NA
  dataUnits <- t(as.data.frame(dataUnits))
  row.names(dataUnits) <- "Unit"
  colnames(dataUnits) <- cleanColNames(colnames(dataRow))

  return(dataUnits)
}

stdErrorMean <- function(x){
  y = mean(x)
  stdError <- (sd(x) / sqrt(length(x)))
  data.frame(y = y, ymin = y - stdError, ymax = y + stdError)
}

stdErrorMeanOnlyUp <- function(x){
  x <- stdErrorMean(x)
  x[2] <-x[1]
  return(x)
}

createPlotDay <- function(theData, cellPop, dataUnit){
  cellPop <- as.name(cellPop)

  if(dataUnit == "%" && !is.na(dataUnit)){
    yAxisLabel <- ylab(paste0(cellPop, " (", dataUnit, ")"))
    yAxisRange <- ylim(0, NA)
  } else {
    yAxisLabel <- ylab(cellPop)
    
    #nothing to do here but we can not add NA to a ggplot so repeat something else
    yAxisRange <- yAxisLabel
  }
  
  ggplot(theData, aes(Group, !!cellPop, fill = Group)) +
    stat_summary(fun = "mean", geom="bar") +
    stat_summary(fun.data = "stdErrorMean", geom="errorbar", width = 0.3) +
    geom_jitter(width = 0.25, height = 0, shape = 1, color = "grey30") +
    yAxisRange +
    ggtitle(cellPop) + yAxisLabel +
    generalTheme + perDayTheme
}

createPlotTimeseries <- function(theData, cellPop, dataUnit){
  cellPop <- as.name(cellPop)
  
  if(dataUnit == "%" && !is.na(dataUnit)){
    yAxisLabel <- ylab(paste0(cellPop, " (", dataUnit, ")"))
    yAxisRange <- ylim(0, NA)
  } else {
    yAxisLabel <- ylab(cellPop)
    
    #nothing to do here but we can not add NA to a ggplot so repeat something else
    yAxisRange <- yAxisLabel
  }
  
  ggplot(theData, aes(Day, !!cellPop, group = Group)) +
    stat_summary(fun = "mean", geom="line", aes(color = Group), size = 0.8) +
    stat_summary(fun.data = "stdErrorMeanOnlyUp", geom="errorbar", aes(color = Group), width = 0.3) +
    yAxisRange +
    ggtitle(cellPop) + yAxisLabel +
    generalTheme
}

createStatsAndTests <- function(data, summaryData, predictor, response, alpha){
  confidence_level = 1- alpha
  
  ANOVAmodel <- aov(as.formula(paste0("`",response,"`", "~ ", predictor)), data)
  
  TukeyTest <- TukeyHSD(ANOVAmodel, conf.level = confidence_level)
  TukeyTest <- as.data.frame(TukeyTest[["Group"]]) 
  TukeyTest <- cbind(`Group pair` = row.names(TukeyTest), TukeyTest)
  TukeyTest <- TukeyTest %>%
                  mutate(Significant = ifelse(`p adj` < alpha, "YES", "no"))
  TukeyTest[,2:5] <- signif(TukeyTest[,2:5], 4)
  
  return(list(
            statsFor = response,
            general = data.frame( Group = summaryData$Group,
                                  Samples = summaryData$noOfSamples,
                                  Mean = signif(pull(summaryData, paste0(response, "_SEM"))$y, 4),
                                  SD = signif(pull(summaryData, paste0(response, "_SD")), 4),
                                  SER = paste(signif(pull(summaryData, paste0(response, "_SEM"))$ymin, 4),"-", signif(pull(summaryData, paste0(response, "_SEM"))$ymax, 4))
                      ),
            oneWayANOVA = ANOVAmodel,
            TukeyTest = TukeyTest
            )
  )
}

cleanUpData <- function(data){
  data <- data[1:(dim(data)[1] -2), ]
  
  measurementColumns <- 3:dim(data)[2]
  
  colnames(data)[measurementColumns] <- cleanColNames(colnames(data)[measurementColumns])
  
  if("TUBENAME" %in% colnames(data)){
    colnames(data)[match("TUBENAME", colnames(data))] <- "TUBE NAME"
  }
  data$`TUBE NAME` <- as.factor(data$`TUBE NAME`)
  
  # remove the " %" and convert to numeric
  data[,measurementColumns] <- sapply(measurementColumns, function(x) {as.numeric(str_replace(pull(data, x), " %", ""))})
  
  # extract the group names and sample no
  if(any(str_detect(data$`TUBE NAME`, "_"))){
    splitGroups <- str_split(data$`TUBE NAME`, "_")
    allGroups <- sapply(splitGroups, function(groupName){paste(groupName[1:length(groupName) -1], collapse = " ") })
    allSampleNos <- sapply(splitGroups, function(groupName){groupName[length(groupName)]})
  } else {
    allGroups <- data$`TUBE NAME`
    allSampleNos <- rep(NA, length(data$`TUBE NAME`))
  }
  
  allGroups <- str_squish(str_trim(allGroups))
  allGroups <- factor(allGroups, unique(allGroups), ordered = TRUE)
  allSampleNos <- as.factor(str_squish(str_trim(allSampleNos)))
  
  data <- cbind(data[1:2], Group = allGroups, sampleNo = allSampleNos, data[measurementColumns])
  
  return(data)
}

extractDayNumbers <- function(fileName){
  amountOfWrongFileNames <- 0
  splitFilenames <- str_split(fileName, " ")
  
  dayNumbers <- lapply(splitFilenames, function(x){
    dayNumber = str_split(x[length(x)], "\\.")[[1]][1]
    if(!grepl("^\\d+$", dayNumber)){
      amountOfWrongFileNames <<- amountOfWrongFileNames - 1
      warning("Use filenames ending on filname{space}{daynumber}")
      return(amountOfWrongFileNames);
    }
    return(dayNumber)
  })
  
  return(as.numeric(dayNumbers))
}

removeFileExtention <- function(fileName){
  fileName <- str_split(basename(fileName), "\\.")[[1]]
  fileName <- paste0(fileName[1:length(fileName) -1], collapse = ".")
  return(fileName)
}

processDataSingleDay <- function(theData) {
  dataUnits <- theData[["extractedUnits"]]
  title <- theData[["title"]]
  theData <- theData[["data"]]
  measurementColumns <- 6:(dim(theData)[[2]])

  plots <- lapply(measurementColumns, function(x){
    createPlotDay( theData[, c(colnames(theData)[x], "Group", "sampleNo")],
                cellPop = colnames(theData)[x],
                dataUnit = dataUnits[,colnames(theData)[x]])
  })
  
  summaryData <- theData %>% group_by(Day, Group) %>% summarise(noOfSamples = n(),
                                                        across(all_of(colnames(theData)[measurementColumns]), list(SD = sd, SEM = stdErrorMean)))
  
  statsAndTests <- sapply(colnames(theData)[measurementColumns], createStatsAndTests,
                          data = theData,
                          summaryData = summaryData,
                          predictor = "Group",
                          alpha = TukeyTestAlpha,
                          simplify = FALSE,
                          USE.NAMES = TRUE)
  
  return(list(title = title,plots = plots, summaryData = summaryData, statsAndTests = statsAndTests))
}

processDataTimeseries <- function(theData) {
  dataList <- lapply(theData, function(sheetAndInfo){
    return(sheetAndInfo[["data"]])
  })
  unitsList <- lapply(theData, function(sheetAndInfo){
    return(as.data.frame(sheetAndInfo[["extractedUnits"]]))
  })
  allData <- dataList %>% reduce(full_join)
  allUnits <- unitsList %>% reduce(full_join)
  rm(dataList)
  rm(unitsList)
  
  levels(allData$Day) <- sort(as.numeric(levels(allData$Day)))
  
  #allDataGrouped <- allData %>% group_by(Group, Day) %>% summarise(across(all_of(colnames(allData)[6:length(allData)]), list(mean = mean)))
  #allData <- allData[order(allData$Group, allData$Day),]
  
  measurementColumns <- 6:(dim(allData)[[2]])
  results <- lapply(measurementColumns, function(x){
    theData <- na.omit(allData[, c(colnames(allData)[x], "Group", "Day", "sampleNo")])
    
    plot <- createPlotTimeseries( theData,
                          cellPop = colnames(allData)[x],
                          dataUnit <- allUnits[,colnames(allData)[x]])
    
    list(name = c(colnames(allData)[x]), plot = plot)
  })
  
  names(results) <- colnames(allData)[measurementColumns]
  
  return(results)
}

filePaths <- list.files(fileDir, pattern="*.xls*", full.names=TRUE)
excelSheets <-lapply(filePaths, function(filePath){
  title <- removeFileExtention(basename(filePath))
  data <- read_excel(filePath, col_types = "text")
  day <- extractDayNumbers(basename(filePath))[[1]]
  return(list(title = title, day = day, data = data))
})
names(excelSheets) <- extractDayNumbers(basename(filePaths))

excelSheets <- lapply(excelSheets, function(sheetAndInfo){
  measurementColumns <- 3:dim(sheetAndInfo[["data"]])[2]
  extractedUnits <- extractUnits(sheetAndInfo[["data"]][rowToGrabUnitsFrom -1, measurementColumns])

  theData <- cleanUpData(sheetAndInfo[["data"]])
  theData <- cbind(theData[1:4],
                   Day = as.numeric(sheetAndInfo[["day"]]),
                   theData[5:dim(theData)[2]])
  
  return(list(title = sheetAndInfo[["title"]],
              day = sheetAndInfo[["day"]],
              data = theData,
              extractedUnits = extractedUnits
              )
  )
})

noOfChildren <- detectCores(logical = TRUE)
if(noOfChildren > length(excelSheets)){
  noOfChildren <- length(excelSheets)
}

cl <- makeCluster(noOfChildren)
clusterEvalQ(cl, library(ggplot2))
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c("createPlotDay", "stdErrorMean", "createStatsAndTests", "generalTheme", "perDayTheme", "TukeyTestAlpha"))

processedDataPerDay <- parLapply(cl, excelSheets, processDataSingleDay)
processedDataPerDay <- processedDataPerDay[as.character(sort(as.numeric(names(processedDataPerDay))))]

dir.create(outputDir, showWarnings = FALSE)

clusterEvalQ(cl, library(rmarkdown))
clusterExport(cl, c("outputDir", "authors", "PANDOC_DIR"))
clusterEvalQ(cl, Sys.setenv(RSTUDIO_PANDOC=PANDOC_DIR))

parLapply(cl, processedDataPerDay, function(processedDay){
  render("graph.Rmd", output_file = paste0(outputDir, "/", processedDay[["title"]]))
})
stopCluster(cl)

if(length(excelSheets) > 1){
  timeseries <- processDataTimeseries(excelSheets)

  render("timeseries.Rmd", output_file = paste0(outputDir, "/Timeseries"))
} else {
  print("Not enough files for time series.")
}
print("Script completed.")
