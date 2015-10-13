##The function should use the following template.
## The function takes three arguments: the 2-character abbreviated name of a state, an outcome 
## and the ranking of a hospital in that state for that outcome (num)


rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    outcome_f <- read.csv("outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)[, c(2,7,11,17,23)]
    
    ## Check that state and outcome are valid
    outcome_u_s <- unique(outcome_f$State)
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    
    if((state %in% outcome_u_s) == FALSE) {
        stop("invalid state")
    } else if((outcome %in% valid_outcome) == FALSE) {
        stop ("invalid outcome")
    } else {
        
        ## Return hospital name in that state with the given rank 30-day death rate
        
        #set new headers for dataset
        colnames(outcome_f) <- c("Hospital.Name", "State", valid_outcome)
        #filter the dataset for selected state.
        outcome_f_s <- outcome_f[outcome_f$State==state,]
        
        # get the column number (3,4 or 5) and further filter the dataset for selected outcome 
        column <- which(colnames(outcome_f_s) == outcome)
        outcome_f_s <- outcome_f_s[,c(1,2,column)]
        
        #rename the selected outcome as "Rate"
        names(outcome_f_s)[3] <- c("Rate")
        # get complete cases but before make the Not Available as NA 
        suppressWarnings(outcome_f_s[, 3] <- as.numeric(outcome_f_s[, 3]))
        outcome_f_s <- outcome_f_s[complete.cases(outcome_f_s$Rate), ]
    
        #dataset is now as required, sort with the Rate first and then with hospital name
        outcome_f_s <- outcome_f_s[order(outcome_f_s$Rate, outcome_f_s$Hospital.Name), ]
        
        # set the rank by comparing the num with "best", "worst"
        if (num == "best") 
            rank <- 1
        else if (num == "worst") 
            rank <- nrow(outcome_f_s)
        else
            rank <- num

        result <- outcome_f_s[rank, ]$Hospital.Name
        return(result)
 
    }    
}

