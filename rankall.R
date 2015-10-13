## The function should use the following template.
## Function takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.
## The first column in the data frame is named hospital, which contains the hospital name, 
## and the second column is named state, which contains the 2-character abbreviation for the state name.

rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    outcome_f <- read.csv("outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)[, c(2,7,11,17,23)]
    
    ## Check that the outcome is valid
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    
    ## Get the unique state
    state_uniq <- unique(outcome_f$State)
    
    if((outcome %in% valid_outcome) == FALSE) {
        stop ("invalid outcome")
    } else {
        
        #set new headers for dataset
        colnames(outcome_f) <- c("Hospital.Name", "State", valid_outcome)
        
        # get the column number (3,4 or 5) and further filter the dataset for selected outcome 
        column <- which(colnames(outcome_f) == outcome)
        outcome_f_o <- outcome_f[,c(1,2,column)]
        
        #rename the selected outcome as "Rate"
        names(outcome_f_o)[3] <- c("Rate")
        
        # get complete cases but before make the Not Available as NA 
        suppressWarnings(outcome_f_o[, 3] <- as.numeric(outcome_f_o[, 3]))
        outcome_f_o <- outcome_f_o[complete.cases(outcome_f_o$Rate), ]
        
        result <- character(0)
        ## For each state, find the hospital of the given rank and save it in result
        for(state in state_uniq) {
            outcome_f_s <- outcome_f_o[outcome_f_o$State == state, ]
            
            # sort by Rate and Hospital Name in that order.
            outcome_f_s <- outcome_f_s[order(outcome_f_s$Rate, outcome_f_s$Hospital.Name), ]
            
            # set the rank by comparing the num with "best", "worst"
            if (num == "best") 
                rank <- 1
            else if (num == "worst") 
                rank <- nrow(outcome_f_s)
            else 
                rank <- num
            
            #save the ranked hospital names for each state
            result <-c(result, outcome_f_s[rank, ]$Hospital.Name)

        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        result <- data.frame(hospital = result, state = state_uniq)
        result <- result[order(result$state), ]
        return(result)
    }    
}
