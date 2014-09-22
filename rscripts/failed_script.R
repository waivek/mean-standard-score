asd <- function(vector) {
    sum(abs(vector - median(vector))) / length(vector)
}
mss <- function(score, m, asd) {
    (score - m) / asd
}
get.distances <- function(point, nvector) {
    m <- median(nvector)
    asd <- get.absolute.standard.deviation(nvector)
    # mss_list <- sapply(nvector, get.modified.standard.score, m, asd)
    # abs(mss_list - get.modified.standard.score(point, m, asd))
    
    mss_of_sample <- sapply(nvector, get.modified.standard.score, m, asd)
    mss_of_subject <- get.modified.standard.score(point, m, asd)
    mss_of_subject<- rep(mss_of_subject, length(nvector))
    difference <- (mss_of_sample - mss_of_subject)
    difference_modded <- abs(difference)
    # print(length(mss_of_sample))
    # print(length(mss_of_subject))
    # print(length(difference))
    # print(length(difference_modded))
    organized_data <- data.frame(mss_of_sample, 
                                mss_of_subject, 
                            difference, difference_modded)
    organized_data
}
get.sport <- function(athlete_data, h, w) {
    neighbors <- get.distances(h, athlete_data$height) +
        get.distances(w, athlete_data$weight)    
    athlete_data$neighbors <- neighbors
    # return(athlete_data)
    athlete_data[with(athlete_data, order(neighbors)),]$sport[1]
}
main <- function() {
    txtFilePath <- "/home/waivek/Desktop/R/guidetodatamining.com/chapter-4/athletesTrainingSet.txt"
    colnames = c("name", "sport", "height", "weight")
    athlete_data <- read.table(txtFilePath, 
                               sep = "\t", 
                               header = TRUE,
                               col.names = colnames,
                               stringsAsFactors = FALSE)
    # column_is_numeric <- sapply(athlete_data, is.numeric)
    # median <- sapply(table[column_is_numeric], median)
    # absolute_deviation <- sapply(table[column_is_numeric], 
    #                               get.absolute.standard.deviation)
    # mads <- data.frame(median, absolute_deviation)
    
    # return(ttable)
    # return(get.sport(athlete_data, 70, 140))
    tstFilePath <- "/home/waivek/Desktop/R/guidetodatamining.com/chapter-4/athletesTestSet.txt"
    ttable <- read.table(tstFilePath, 
                         sep = "\t", 
                         header = TRUE, 
                         col.names = colnames,
                         stringsAsFactors = FALSE)
    # neighbors <- get.distances(h, athlete_data$height) +
    #              get.distances(w, athlete_data$weight)    
    # athlete_data$neighbors <- neighbors
    # athlete_data[with(athlete_data, order(neighbors)),]$sport[1]
    calc_sport <- character(0)
    for(row in rownames(ttable)) {
        # s <- get.sport(athlete_data, ttable$height, ttable$weight)
        s <- get.sport(ttable, ttable[row, "height"], ttable[row, "weight"])
        calc_sport <- c(calc_sport, s)
    }
    ttable$calc_sport <- calc_sport
    ttable
}