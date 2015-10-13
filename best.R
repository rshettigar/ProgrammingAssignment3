##The function should use the following template.
## Function take two arguments: the 2-character abbreviated name of a state and an outcome name.
## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
## The function reads the "outcome-of-care-measures.csv" file and returns a character vector
## with  the  name  of  the  hospital  that  has  the  best  (i.e.   lowest)  
## 30-day  mortality  for  the  specified  outcome in that state.

best <- function(state, outcome) {

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
    
        ## Return hospital name in that state with lowest 30-day death rate
        
        #set new headers for dataset
        colnames(outcome_f) <- c("Hospital.Name", "State", valid_outcome)
        #reduce to selected state dataset
        outcome_f_s <- outcome_f[outcome_f$State==state,]
        #further reduce to selected outcome dataset
        column <- which(colnames(outcome_f_s) == outcome)
        rowNum <- suppressWarnings(which.min(outcome_f_s[, column]))
        result <- sort(outcome_f_s[rowNum,]$Hospital.Name)
        return(result) 
    }    
}
