best <- function(state, outcome) {
    # check if input state is valid
    state <- toupper(state)
    state.abb <- c(state.abb, c("DC", "GU", "MP", "PR", "VI"))
    if(!any(state.abb == state)) {
        stop("invalid state")
    }
    
    # check if input outcome is valid
    outcome <- toupper(outcome)
    valid_outcome <- c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA")
    if(!any( valid_outcome == outcome)) {
        stop("invalid outcome")
    }
    
    # relevant indexes
    hospital_idx     <- 2  # Hospital Name
    state_idx        <- 7  # State
    mortality_ha_idx <- 11 # Hospital 30-Day Death (Mortality) Rates from Heart Attack
    mortality_hf_idx <- 17 # Hospital 30-Day Death (Mortality) Rates from Heart Failure
    mortality_p_idx  <- 23 # Hospital 30-Day Death (Mortality) Rates from Pneumonia
    
    outcome_idx <- switch(outcome,
                          "HEART ATTACK"  = mortality_ha_idx,
                          "HEART FAILURE" = mortality_hf_idx,
                          "PNEUMONIA"     = mortality_p_idx)
    
    column_classes <- c(rep("NULL", 46))
    column_classes[c(hospital_idx, state_idx, outcome_idx)] <- "character"
    
    data_file <- "outcome-of-care-measures.csv"
    data <- read.table(data_file, header = TRUE, 
                       sep = ",", quote = "\"", dec = ".",
                       comment.char = "", colClasses = column_classes,
                       na.strings = "Not Available")
    
    filtered_data <- data[which(data[[2]] == state), c(1, 3)]
    ordered_data <- filtered_data[order(as.numeric(filtered_data[[2]]), filtered_data[[1]]),]
    ret <- ordered_data[1,1]
    return(ret)
}