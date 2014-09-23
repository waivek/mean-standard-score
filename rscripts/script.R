mss <- function(score, median, asd) {
    (score - median) / asd
}
asd <- function(numeric_vector) {
        sum(abs(numeric_vector - median(numeric_vector))) /
             length(numeric_vector)
}
normalize.vector <- function(nvector, median, asd) {
    sapply(nvector, mss, median, asd)
}
normalize <- function(df) {
    column_is_numeric <- sapply(df, is.numeric)
    
    df[,column_is_numeric] <- sapply(df[,column_is_numeric], normalize.vector,
                                     global.median, global.asd)
    # loops through each numeric column
    # normalizes that column
    # replaces original numeric columns with normalized numeric columns
    # median and asd are calculated on the fly
    df
    
}
get.table <- function(table_location) {
    read.table(table_location, 
               stringsAsFactors = FALSE, 
               sep = "\t",
               header = TRUE
               )
}
## GLOBAL DEFINITION ##
global.median <- numeric(0)
global.asd <- numeric(0)
set.globals <- function(df) {
    column_is_numeric <- sapply(df, is.numeric)
    global.median <<- sapply(df[, column_is_numeric] , median)
    global.asd <<- sapply(df[, column_is_numeric] , asd)
    # The <<- operator changes the global variables
    # The <- operator would have simply created local variables of the
    # same name
}
read.normalized.table <- function(table_location, set.globals = TRUE) {
    normalized_table <- get.table(table_location)
    if(set.globals == TRUE) {
        set.globals(normalized_table)
    }
    normalized_table <- normalize(normalized_table)
    normalized_table
}
main <- function(test_data_location, classifier_data_location) {
    classifier <- read.normalized.table(classifier_data_location)
    return(classifier)
    test_data <- read.normalized.table(test_data_location, set.globals = FALSE)
    distances <- get.distances(test_data, classifier)
    nearest_neighbors <- get.nearest.neighbors(classifier$class, distances)
    nearest_neighbors
    
}
test.main <- function() {
    txtFilePath <- paste("/home/waivek/Desktop/R/guidetodatamining.com/",
                            "chapter-4/datasets/athletesTrainingSet.txt", sep="")
    tstFilePath <- paste("/home/waivek/Desktop/R/guidetodatamining.com/",
                            "chapter-4/datasets/athletesTestSet.txt", sep="")
    main(tstFilePath, txtFilePath)
}
bar <- function(point, points) {
    column_is_numeric <- sapply(points, is.numeric)
    for(rowname in rownames(points)) {
        print(points[, column_is_numeric][row, ])
        points[, column_is_numeric][row, ] <- abs(points[, column_is_numeric][row, ] - point)
    }
    points
    
}
