# this function tabulates the frequency per row.
# setting MARGIN to 2 gives frequency per column.
table_per_row <- function(data, MARGIN=1) {
    flat_data <- as.vector(as.matrix(data)) # "flatten" the data into just a list of heads and tails
    unique_values <- unique(flat_data) # get all of the unique values in the data 
    factored <- apply(data, MARGIN, function(row) factor(row, levels=unique_values, simplify=FALSE)) # maps the "factor" function over the rows... this is just a precursor to make "table" work right
    tabulated <- sapply(factored, table) # map "table" over the previous line
    return(data.frame(t(tabulated))) # return as a dataframe in familiar orientation.
}

data <- read.csv("flippin_coins.csv")
coins <- data[,2:length(data)]
coins

heads_v_tails_per_student <- table_per_row(coins)

proportions <- heads_v_tails_per_student$heads/nrow(coins)

hist(proportions, breaks=seq(min(proportions),max(proportions),length.out=11))

# To do percents instead
lattice::histogram(heads_v_tails_per_student$heads)
