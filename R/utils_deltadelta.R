# utils_deltadelta.R

# select controlUI
select_controlUI <- function(wrangled_data){
  renderUI({
    req(wrangled_data())  # Ensure data is available
    selectInput("select_control", "Select the control/untreated sample", choices = unique(wrangled_data()$cell))
  })
}

#select samplesUI
select_samplesUI <- function(wrangled_data){
  renderUI({
    req(wrangled_data())  # Ensure data is available
    selectInput("select_samples", "Select the diseased/treated sample(s)", choices = unique(wrangled_data()$cell), multiple = T)
  })
}

#select geneUI
select_geneUI <- function(wrangled_data){
  renderUI({
    req(wrangled_data())
    
    # Filter column names to include only those starting with "dcq"
    dcq_columns <- grep("^dcq_", colnames(wrangled_data()), value = TRUE)
    
    # Generate selectInput for choosing the column dynamically
    selectInput("select_gene", "Select Gene to calculate ΔΔCq", choices = dcq_columns)
  })
}

# create ddcq data
filtered_samples_for_ddcq_data <- function(wrangled_data, input){
  reactive({
    req(wrangled_data())
    req(input$select_gene)
    
    control <- input$select_control
    samples <- input$select_samples
    selected_gene <- input$select_gene
    
    ddcq_data <- wrangled_data() %>% 
      filter((cell == control) | (cell %in% samples)) %>% 
      dplyr::select(cell, all_of(selected_gene))
    
    # Resetting levels of factors to only include selected options
    ddcq_data$cell <- factor(ddcq_data$cell, levels = unique(c(as.character(control), as.character(samples))))

    return(ddcq_data)
  })
}

# Calculate the average delta cq for the selected gene in the control samples
calc_ddcq_data <- function(wrangled_data, ddcq_filtered_data, input, mean_value){
  reactive({
    req(wrangled_data())
    req(input$select_gene)
    req(input$select_control)
    
    selected_gene2 <- input$select_gene
    control2 <- input$select_control
    samples2 <- input$select_samples
    
    
    # Calculate the average delta cq for the selected gene in the control samples
    avg_dcq_ctrl <- ddcq_filtered_data() %>%
      filter(cell == control2) %>%
      group_by(cell) %>%
      summarise(dcq_ctrl_avg = mean(!!sym(selected_gene2), na.rm = TRUE), .groups = "drop")
    
    # Left join the original dataframe with the summarised dataframe
    avg_dcq_ctrl <- left_join(ddcq_filtered_data(), avg_dcq_ctrl, by = "cell")
    
    # Calculate the mean value
    mean_val <- mean(avg_dcq_ctrl$dcq_ctrl_avg, na.rm = TRUE)
    
    # Store the mean value in the reactive value
    mean_value(mean_val)
    
    # Assign the mean value to the entire dcq_ctrl_avg column
    avg_dcq_ctrl$dcq_ctrl_avg <- mean_val
    
    # Create a new column ddcq by subtracting selected_gene2 from dcq_ctrl_avg
    avg_dcq_ctrl$ddcq <-  avg_dcq_ctrl[[selected_gene2]] - avg_dcq_ctrl$dcq_ctrl_avg
    
    # Create a new column fc_ddcq containing 2^(-ddcq)
    avg_dcq_ctrl$fc_ddcq <- 2^(-avg_dcq_ctrl$ddcq)
    
    # Resetting levels of factors to only include selected options
    avg_dcq_ctrl$cell <- factor(avg_dcq_ctrl$cell, levels = unique(c(as.character(control2), as.character(samples2))))

    return(avg_dcq_ctrl)
  })
}
  
#display ddcq data in UI
render_ddcqUI <- function(average_dcq){
  renderDataTable({
    req(average_dcq())
    average_dcq()
  }, options = list(pageLength = 5))
}

# Calculate replicate averages when data is loaded
calc_rep_avg_ddcq <- function(average_dcq){
  reactive({
    req(average_dcq())
    
    rep_avg_ddcq <- average_dcq() %>%
      group_by(cell) %>%
      #summarize using geomteric mean
      summarize(mean_fc_ddcq = exp(mean(log(fc_ddcq), na.rm = TRUE)), .groups = "drop")
    return(rep_avg_ddcq)
  })
}


# Display the replicate averages table in "Calculations" tab
render_ddcq_avgUI <- function(rep_avg_data_ddcq){
  renderDataTable({
    rep_avg_data_ddcq()
  }, options = list(pageLength = 5))
}
