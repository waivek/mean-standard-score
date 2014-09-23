mss <- function(score, median, asd) {
    (score - median) / asd
}
asd <- function(numeric_vector) {
        sum(abs(numeric_vector - median(numeric_vector))) /
             length(numeric_vector)
}
normalize.vector <- function(nvector, m, a) {
    
    sapply(nvector, mss, m, a)
}
normalize <- function(df) {
    column_is_numeric <- sapply(df, is.numeric)
    for(name in names(df[, column_is_numeric])) {
        df[, name] <- normalize.vector(df[, name], global.median[name], global.asd[name])
    }
    # # Redundant Code df[,column_is_numeric] <- sapply(df[,column_is_numeric], normalize.vector)
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

get.distances <- function(vec, df) {
    class <- df[, "class"]
    column_is_numeric <- sapply(df, is.numeric)
    df <- df[, column_is_numeric]
    df <- t(t(df) - vec)
    df <- abs(df)
    distance <- rowSums(df)
    df <- data.frame(class, distance)
    df <- df[with(df, order(distance)),]
    df$class[1]
    
} 
classify <- function(df, classifier) {
    name <- df$comment
    actual <- df$class
    column_is_numeric <- sapply(df, is.numeric)
    df <- df[, column_is_numeric]
    factor <- apply(df, 1, get.distances, classifier)
    predicted <- as.character(factor)
    data.frame(name, predicted, actual)
}
main <- function(test_data_location, classifier_data_location) {
    classifier <- read.normalized.table(classifier_data_location)
    test_data <- read.normalized.table(test_data_location, set.globals = FALSE)
    classify(test_data, classifier)
    
}
test.main <- function() {
    txtFilePath <- paste("/home/waivek/Desktop/R/guidetodatamining.com/",
                            "chapter-4/datasets/athletesTrainingSet.txt", sep="")
    tstFilePath <- paste("/home/waivek/Desktop/R/guidetodatamining.com/",
                            "chapter-4/datasets/athletesTestSet.txt", sep="")
    main(tstFilePath, txtFilePath)
}

