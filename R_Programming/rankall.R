rankall <- function(outcome, num = "best") {
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
    
    # get list of data frames by state
    covered_states <- c(state.abb, c("DC", "GU", "MP", "PR", "VI"))
    ordered_states <- covered_states[order(covered_states)]
    data_by_state <- lapply(ordered_states,
                            function(x) {
                                filter <- data[which(data[[2]] == x),]
                                order <- filter[order(as.numeric(filter[[3]]), filter[[1]]),]
                                na_remove <- order[!is.na(order[[3]]), c(1, 2)]
                            } )
    names(data_by_state) <- ordered_states

    if(toupper(num) == "BEST") { num <- 1 }
    ranking <- lapply(data_by_state,
                      function(x) {
                          if(toupper(num) == "WORST") { num <- nrow(x) }
                          x[as.integer(num),]
                      })
    
    res <- setNames(do.call(rbind.data.frame, ranking), c("hospital", "state"))
    res[,2] <- row.names(res)
    return(res)
    
}