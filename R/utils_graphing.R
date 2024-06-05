# utils_graphing.R
#display message if ddcq dataset hasn't been created yet
ddcq_not_calculated_msg_graphs <- function(input, values){
  renderUI({
    # Check if 'ddcq_stats' is selected and data is not saved yet
    if (input$select_dcq_or_ddcq == "ddcq" && !values$ddcqDataSaved) {
      # Return a UI element with the message
      tagList(
        HTML('<h5>Please go to the 2<sup>-(∆∆Cq)</sup> Calculations tab to create your ∆∆Cq dataset.</h5>'),
        tags$p("You need to save your ∆∆Cq dataset before proceeding.")
      )
    } else {
      # Return NULL or an empty UI element if conditions are not met
      return()
    }
  })
}

select_dcq_ddcq_data <- function(input, wrangled_data, average_dcq){
  # Graphing dcq or ddcq  
  # Reactive values to store user selections
  selected_stats <- reactive({
    sub("_stats", "", input$select_dcq_or_ddcq_stats)
  })
  selected_graphs <- reactive({
    input$select_dcq_or_ddcq
  })
  # Reactive expression to check for consistency
  data_consistency <- reactive({
    selected_stats() == selected_graphs()
  })
  
  col_discrepancy <- reactive({
    selected_stat_column <- input$columnInput
    selected_plot_column <- if(input$select_dcq_or_ddcq == "dcq") input$fc_dcq_column else input$fc_ddcq
    !is.null(selected_stat_column) && !is.null(selected_plot_column) && selected_stat_column != selected_plot_column
    #selected_stat_column != selected_plot_column
  })
  
  list(
    data = reactive({
      if (!data_consistency()) {
        showModal(modalDialog(
          title = "Data Selection Mismatch",
          "Please ensure that the selected data type for stats and graphs are the same. i.e. ∆Cq vs ∆ΔCq.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      } else {
        if (input$select_dcq_or_ddcq == "dcq") {
          # If 'dcq' is selected, return wrangled_data()
          return(wrangled_data())
        } else {
          # If 'ddcq' is selected, return average_dcq()
          return(average_dcq())
        }
      }
    }),
    selected_stats = selected_stats,
    selected_graphs = selected_graphs,
    data_consistency = data_consistency,
    col_discrepancy = col_discrepancy,
    discrepancy_detected  = reactive({
      if(col_discrepancy()) {
        showModal(modalDialog(
          title = "Discrepancy Detected",
          "The selected gene for statistics does not match the gene for graphing. Please adjust your selections to ensure that the correct p-values are added to your graph.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })
  )
}
  

dcq_ddcq_UI <- function(input, output, wrangled_data){
  observeEvent(input$select_dcq_or_ddcq,{
    if (input$select_dcq_or_ddcq == "dcq") {
      # Render the dynamic selectInput for choosing condition
      output$condition_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available
        
        # Generate selectInput for choosing the condition dynamically
        selectInput("selected_condition", "Select Samples", choices = unique(wrangled_data()$cell),
                    multiple = TRUE)
      })
      
      # Render the dynamic selectInput for choosing the column
      output$column_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available
        
        # Filter column names to include only those starting with "fc_dcq"
        fc_dcq_columns <- grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
        
        # Generate selectInput for choosing the column dynamically
        selectInput("fc_dcq_column", "Select Gene", choices = fc_dcq_columns)
      })
    } else {
      # Hide the UI elements if ddcq is selected
      output$condition_selector <- renderUI(NULL)
      output$column_selector <- renderUI(NULL)
    }
  })
  
}

ddcq_UI <- function(input, output, wrangled_data){
  observeEvent(input$select_dcq_or_ddcq, {
    # Check if 'ddcq' is selected
    if (input$select_dcq_or_ddcq == "ddcq") {
      # Display the gene selection message
      output$selected_gene_ui <- renderUI({
        textOutput("selected_gene_message")
      })
    } else {
      # Hide the message when 'dcq' is selected or for any other condition
      output$selected_gene_ui <- renderUI({})
    }
  })
}

displayGeneUI <- function(input){
  renderText({
    req(input$select_gene)  # Ensure there is a selection
    # Use gsub to remove "dcq_" from the selected gene's name
    selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)
    paste("You are currently graphing gene:", selected_gene_cleaned)
  })
}

dynamic_YLabel <- function(input){
  renderUI({
    # Initialize variable to store cleaned gene name
    selected_gene_cleaned <- ""
    
    # Determine which input to use based on the condition selected
    if (input$select_dcq_or_ddcq == "dcq") {
      # Ensure the fc_dcq_column input is used for dcq condition
      req(input$fc_dcq_column)  # Ensure there is a selection for the dcq condition
      selected_gene_cleaned <- gsub("^fc_dcq_", "", input$fc_dcq_column)  # Clean the gene name for dcq
    } else if (input$select_dcq_or_ddcq == "ddcq") {
      # Assume there's another input mechanism for ddcq or use a default value
      req(input$select_gene)  # Placeholder, adjust as necessary for ddcq
      selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)  # Clean the gene name for ddcq
    }
    
    # Determine the placeholder text based on the selection
    placeholder_text <- if (input$select_dcq_or_ddcq == "dcq") {
      paste0("Relative *", selected_gene_cleaned, "* mRNA (2<sup>-ΔCq</sup>)")
    } else if (input$select_dcq_or_ddcq == "ddcq") {
      paste0("Fold change *", selected_gene_cleaned, "* mRNA (2<sup>-ΔΔCq</sup>)")
    } else {
      "Enter Y-axis Label"  # Default text if neither is selected
    }
    
    # Generate the text input with the dynamic placeholder
    textInput("y_label", "Enter Y-axis Label. Markup is accepted.", value = placeholder_text)
  })
}



# Dynamic generation of text inputs based on positions
xAxis_label <- function(input){
  renderUI({
    positions <- if (!is.null(input$x_axis_positions)) {
      unlist(strsplit(input$x_axis_positions, ","))
    } else {
      NULL
    }
    
    
    if (!is.null(positions) && length(positions) > 0) {
      lapply(positions, function(pos) {
        textInput(inputId = paste0("label_", pos),
                  label = paste("Label for", pos),
                  value = pos)  # Initial value can be set to the position itself or an empty string
      })
    }
  })
}


# Reactive function to get user-entered labels
userLabels_reactive <- function(input){
  reactive({
    positions <- if (!is.null(input$x_axis_positions)) {
      unlist(strsplit(input$x_axis_positions, ","))
    } else {
      NULL
    }
    
    if (!is.null(positions) && length(positions) > 0) {
      sapply(positions, function(pos) {
        input[[paste0("label_", pos)]]
      })
    }
  })
}

# Dynamic generation of text inputs based on positions
xAxis_positions <- function(input, output){
  observeEvent(input$labels_positions, {
    # Parse labels if user entered them
    filtered_labels <- if (!is.null(input$labels_positions)) {
      unlist(strsplit(input$labels_positions, ","))
    } else {
      NULL
    }
    
    # Update the UI with text boxes for custom labels
    label_textboxes <- lapply(filtered_labels, function(label) {
      textInput(paste0("label_", label), label, label)
    })
    # Render UI for text boxes
    output$labels_textboxes <- renderUI({
      label_textboxes
    })
  })
}

# Reactive function to get col names (i.e. gene)
gene_col_reactive <- function(wrangled_data){
  reactive({
    req(wrangled_data())  # Ensure data is available
    grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
  })
}

calc_num_groups <- function(shapiro_data_reactive){
  reactive({
    length(unique(shapiro_data_reactive()$cell))
  })
}
add_sigUI <- function(input, num_groups){
  renderUI({
    if(input$add_significance == "asterix" || input$add_significance == "cld" || input$add_significance == "pval"){
      tagList(
        fluidRow(
          column(4, numericInput("sigSize", "Label Size", min = 0, max = 20, value = 5)),
          if(input$add_significance != "cld") {
            tagList(
              column(4, numericInput("bracketSize", "Bracket Thickness", min = 0, max = 10, value = 0.8, step = 0.1)),
              column(4, numericInput("stepIncrease", "Step Increase", max = 20, value = 0.1, step = 0.1))
            )
          }
        ),
        if(input$add_significance != "cld" && num_groups() > 2) {
          fluidRow(
            column(6, checkboxInput("hideNS", "Hide ns", value = FALSE))
          )
        },
        if(input$add_significance == "pval"){
          tagList(
            fluidRow(
              column(6, selectInput("pValuePrefix", "P-value Prefix", choices = c("None", "P = "))),
              column(6, numericInput("pValueDecimals", "Decimal Places for P-value", value = 2, min = 0, max = 10)),
            ),
            fluidRow(
              #column(6, numericInput("stepIncrease", "Step Increase", min = 0, max = 20, value = 0.1, step = 0.1)),
              column(6, checkboxInput("remove0", "Remove leading zero from P-value", value = FALSE))
            )
          )
        },
        if(input$add_significance == "cld"){
          fluidRow(
            column(6, numericInput("stepIncrease", "Step Increase", max = 20, value = 0.1, step = 0.1))
          )
        },
        # Tip Length is applicable for all scenarios except "cld"
        if(input$add_significance != "cld") {
          fluidRow(
            column(12, numericInput("tipLength", "Tip Length", max = 20, value = 0.02, step = 0.01))
          )
        }
      )
    }
  })
}


# PLOT CODE
## EXTENISVE code for generating customisable graphs:
create_graph <- function(input, graph_generated, selected_stats, selected_graphs, dcq_or_ddcq, col_discrepancy, rep_avg_data, rep_avg_data_ddcq, error_fun, color_schemes, colours, theme_Marnie, user_labels, shapiro_data_reactive, comparisonResults, ci){
  renderPlot({
    req(input$select_dcq_or_ddcq, input$y_label, input$x_label)
    # Check if the graph has been generated and the data selection has changed
    if (graph_generated() && selected_stats() != selected_graphs()) {
      showNotification("Data selection has changed: Please re-generate the graph after selecting the correct data type.", type = "error")
      return(NULL)
    }
    # Call col_discrepancy to check for discrepancies
    if (col_discrepancy()) {
      showModal(modalDialog(
        title = "Discrepancy Detected",
        "The selected gene for statistics does not match the gene for graphing. Please adjust your selections to ensure that the correct p-values are added to your graph.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    }
    
    set.seed(input$seed_input)
    
    if(input$select_dcq_or_ddcq == "dcq"){
      filtered_data2 <- dcq_or_ddcq() %>%
        filter(cell %in% input$selected_condition)
      filtered_rep_avg_data2 <- rep_avg_data() %>%
        filter(cell %in% input$selected_condition)
      #If filtered_rep_avg_data2 has column fc_avg, rename to input$selected_condition
      #This is needed if only one gene is present in the dataset apart from the housekeepers
      if("fc_avg" %in% colnames(filtered_rep_avg_data2)){
        filtered_rep_avg_data2 <- filtered_rep_avg_data2 %>%
          rename(!!input$fc_dcq_column := fc_avg)
      }
      # Determine the x aesthetic based on the number of selected conditions
      x_aes <- if (length(input$selected_condition) >= 2) {
        sym("cell")
      } else {
        sym("group")
      }
      
      # Specify the y_aes based on user input
      y_aes <- sym(input$fc_dcq_column)
      y_aes_avg <- sym(input$fc_dcq_column)
      
      positions <- if (length(input$selected_condition) >= 2) {
        # Parse positions if user entered them, or use unique values from "cell" column
        if (!is.null(input$x_axis_positions)) {
          unlist(strsplit(input$x_axis_positions, ","))
        } else {
          unique(filtered_data2$cell)
        }
      } else {
        unlist(strsplit(input$x_axis_positions, ","))
      }
      
    }else if(input$select_dcq_or_ddcq == "ddcq"){
      filtered_data2 <- dcq_or_ddcq()
      filtered_rep_avg_data2 <- rep_avg_data_ddcq()
      # Determine the x aesthetic based on the number of selected conditions
      x_aes <- sym("cell")
      # Specify the y_aes based on user input
      y_aes <- sym("fc_ddcq")
      y_aes_avg <- sym("mean_fc_ddcq")
      
      positions <- if (length(input$select_samples) >= 2) {
        # Parse positions if user entered them, or use unique values from "cell" column
        if (!is.null(input$x_axis_positions)) {
          unlist(strsplit(input$x_axis_positions, ","))
        } else {
          unique(filtered_data2$group)
        }
      } else {
        unlist(strsplit(input$x_axis_positions, ","))
      }
    }
    
    
    # Check if x-axis categories are available
    if (is.null(input$x_axis_positions) || input$x_axis_positions == "") {
      validate(
        need(FALSE, "Please enter x-axis categories to build the graph.")
      )
    }
    
    # Get the color scheme based on user input
    color_scheme <- input$color_scheme_select
    colors <- if (color_scheme == "custom") {
      # If the user selects "Custom," use the custom colors defined earlier
      color_schemes$custom
    } else if (color_scheme %in% names(color_schemes)) {
      # If the user selects one of the predefined schemes, use the corresponding colors
      color_schemes[[color_scheme]]
    } else {
      # If none of the above, use a default set of colors
      c("#ffb000", "#648fff", "#dc267f", "#785ef0", "#00359c", "#fe6100")
    }
    
    
    # Check if the colour palette is smaller than the number of positions entered by the user
    if (length(colors) < length(positions)) {
      validate(
        need(FALSE, "The selected colour palette is smaller than the number of x-axis groups.")
      )
    }
    # Define x-axis theme based on checkbox
    x_axis_theme <- if (input$rotate_labels) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    }
    
    shapes_reactive <- reactive({
      if (input$change_shapes) {
        # New order when checkbox is selected
        setNames(c(16, 21, 17, 24, 15, 22, 18, 23, 4, 3, 8, 10, 9, 11, 12, 13, 14), positions)
      } else {
        # Default order
        setNames(c(16, 17, 15, 18, 4, 3, 8, 10, 9, 11, 12, 13, 14, 21, 22, 23, 24, 25), positions)
      }
    })
    
    
    
    # Determine which function to use based on user input
    error_fun <- if(input$error_type == "sd") {
      mean_sd
    } else if(input$error_type == "se") {
      mean_se 
    } else if(input$error_type == "ci") {
      ci
    } else{
      mean_se
    }
    
    # Calculate the y-limits based on the error function
    summary_stats <- error_fun(filtered_data2[[y_aes]])
    min_ymin <- min(summary_stats$ymin, na.rm = TRUE)
    max_ymax <- max(summary_stats$ymax, na.rm = TRUE)
    
    
    # Check if ymin is below 0 and adjust y_limits accordingly
    y_limits <- if (min_ymin < 0) {
      lower_limit <- min_ymin * 1.1 # Give a 10% buffer below the minimum ymin
      c(lower_limit, NA)
    } else {
      c(0, NA) # If ymin is not below 0, start the y-axis at 0
    }
    
    
    # Reactive for color assignments based on checkbox input for error bars
    dotplot_data_reactive <- reactive({
      dotplot_data <- filtered_data2
      if (input$match_colour_error) {
        dotplot_data$error_bar_color <- dotplot_data[[as.character(x_aes)]]  # Use the same column as x_aes for colors
      } else {
        dotplot_data$error_bar_color <- "black"  # Use black when checkbox is unchecked
      }
      dotplot_data
    })
    
    # Reactive for color assignments based on checkbox input for average bars
    dotplot_data_reactive_avg <- reactive({
      dotplot_data <- filtered_rep_avg_data2
      if (input$match_colour_avg) {
        dotplot_data$avg_bar_color <- dotplot_data$cell  # Use the same column as x_aes for colors
      } else {
        dotplot_data$avg_bar_color <- "black"  # Use black when checkbox is unchecked
      }
      dotplot_data
    })
    
    
    #Create your plot using ggplot2 with the selected dataset
    if (input$plot_type == "column"){
      if (input$fill_color_toggle == "color"){
        plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
          geom_bar(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes_avg, color = !!x_aes), stat = "identity", inherit.aes = FALSE, fill = "white", size = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
          stat_summary(fun.data = error_fun, geom = "errorbar", width = input$errorbar_width, aes(color = !!x_aes), linewidth = input$errorbar_thickness, na.rm = TRUE,  show.legend = FALSE) +
          geom_beeswarm(size = input$dot_size, method = "hex", cex = input$dot_spacing, na.rm = TRUE, aes(color = !!x_aes),  show.legend = FALSE) +
          labs(y = input$y_label, x = input$x_label) +
          scale_color_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
          theme_Marnie +
          scale_x_discrete(limits = positions, labels = user_labels()) +
          x_axis_theme 
      }else if (input$fill_color_toggle == "fill"){
        plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
          geom_bar(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes_avg, fill = !!x_aes), stat = "identity", inherit.aes = FALSE, color = "black", size = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
          stat_summary(fun.data = error_fun, geom = "errorbar", width = input$errorbar_width, color = "black", linewidth = input$errorbar_thickness, na.rm = TRUE,  show.legend = FALSE) +
          geom_beeswarm(size = input$dot_size, method = "hex", cex = input$dot_spacing, na.rm = TRUE, aes(fill = !!x_aes),  show.legend = FALSE) +
          labs(y = input$y_label, x = input$x_label) +
          scale_fill_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
          theme_Marnie +
          scale_x_discrete(limits = positions, labels = user_labels()) +
          x_axis_theme
      }
    }else if (input$plot_type == "dot") {
      # Dot plot
      
      plot <- ggplot(dotplot_data_reactive(), aes(x = !!x_aes, y = !!y_aes)) +
        geom_point(size = input$point_size, na.rm = TRUE, aes(color = !!x_aes, shape = !!x_aes),
                   show.legend = FALSE, stroke = input$stroke_thickness, position = position_jitter(width = input$jitter_amount)) +
        stat_summary(fun.data = error_fun, geom = "errorbar", width = input$error_bar_width,  aes(colour = error_bar_color), linewidth = input$error_bar_thickness, na.rm = TRUE,  show.legend = FALSE) +
        stat_summary(data = dotplot_data_reactive_avg(), aes(x = !!x_aes, y = !!y_aes_avg, color = avg_bar_color), inherit.aes = FALSE,
                     fun = mean, geom = "crossbar", width = input$average_line_width, linewidth = input$average_line_thickness, show.legend = FALSE) +  # Add average line for each column x
        labs(y = input$y_label, x = input$x_label) +
        scale_color_manual(values = c(setNames(colors, positions), "black" = "black"))+
        scale_shape_manual(values = shapes_reactive()) +
        theme_Marnie +
        scale_x_discrete(limits = positions, labels = user_labels()) +
        x_axis_theme
    }
    
    if (input$start_at_zero) {
      plot <- plot +
        scale_y_continuous(expand=expansion(mult=c(0,0.1)), limits = c(0,NA))
    }
    
    # Customize x-axis and y-axis label font size using numeric input
    axis_label_theme <- theme()
    
    if (input$x_axis_title_font_size != 14) {
      axis_label_theme <- axis_label_theme + theme(axis.title.x = element_text(size = input$x_axis_title_font_size))
    }
    
    if (input$y_axis_title_font_size != 14) {
      axis_label_theme <- axis_label_theme + theme(axis.title.y = element_text(size = input$y_axis_title_font_size))
    }
    
    # Apply axis label theme to the plot
    plot <- plot + axis_label_theme
    
    
    
    # Customize x-axis and y-axis label font size using numeric input
    axis_label_theme2 <- theme()
    
    if (input$x_axis_label_font_size != 12) {
      axis_label_theme2 <- axis_label_theme2 + theme(axis.text.x = element_text(size = input$x_axis_label_font_size))
    }
    
    if (input$y_axis_label_font_size != 12) {
      axis_label_theme2 <- axis_label_theme2 + theme(axis.text.y = element_text(size = input$y_axis_label_font_size))
    }
    
    
    # Apply axis label theme to the plot
    plot <- plot + axis_label_theme2
    
    # Set font based on user selection
    font_family <- input$font_selector
    plot <- plot + theme(text = element_text(family = font_family))
    #allow markdown on y axis
    plot <- plot + theme(axis.title.y = element_markdown())
    
    observe({
      req(input$add_significance != "none")
      if (is.null(input$group_comparison) || input$group_comparison == "" && input$add_significance != "none") {
        showModal(modalDialog(
          title = "Statistics Error",
          "Statistics have not been performed: Please select a group comparison test in the statistics tab before adding significance to the graph.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
    })
    
    max_y <- max(filtered_data2[[y_aes]], na.rm = TRUE)
    y_position_auto <- max_y + (0.1 * max_y)
    
    if(input$add_significance == "asterix"){
      num_groups <- length(unique(shapiro_data_reactive()$cell))
      if(num_groups > 2){
        comparisonResults_posthoc_renamed <- comparisonResults()$posthoc %>%
          rename(p.signif = `P Value Summary`)
        plot <- plot + stat_pvalue_manual(comparisonResults_posthoc_renamed, label = "p.signif", y.position = y_position_auto, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
      }else{
        comparisonResults_renamed <- comparisonResults()$test %>%
          rename(p.signif = `P Value Summary`)
        plot <- plot + stat_pvalue_manual(comparisonResults_renamed, label = "p.signif", y.position = y_position_auto, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
      }
      
    }else if(input$add_significance == "cld"){
      # Assuming comparisonResults()$cld_df has columns 'group' and 'Letters'
      cld_data <- comparisonResults()$cld
      # Transform cld_data to have group1 and group2 columns, both containing the same group values
      cld_data <- cld_data %>%
        mutate(group1 = Group, 
               group2 = Group)
      
      plot <- plot + stat_pvalue_manual(cld_data, label = "Letters", y.position = y_position_auto, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                        step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
    }else if(input$add_significance == "pval"){
      num_groups <- length(unique(shapiro_data_reactive()$cell))
      if(num_groups > 2){
        plot_data <- comparisonResults()$posthoc %>%
          rename(p.signif = `Adjusted P Value`)
        
        # Further filter if hideNS is TRUE
        if(input$hideNS){
          plot_data <- plot_data %>%
            # Ensure p.signif is numeric for comparison, handling potential character data
            mutate(p.signif = as.numeric(as.character(p.signif))) %>%
            filter(p.signif <= 0.05)
        }
        # Check if plot_data is empty after filtering
        if(nrow(plot_data) == 0){
          plot <- plot
        }else{
          
          # Then, format p-values according to user preferences
          formatted_pvalues <- lapply(plot_data$`p.signif`, function(p) {
            # Format the p-value with the specified number of decimal places
            formatted_p <- sprintf(paste0("%.", input$pValueDecimals, "f"), p)
            
            # Check and remove the leading zero if required
            if(input$remove0) {
              formatted_p <- sub("^0\\.", ".", formatted_p)
            }
            
            # Determine the prefix based on user selection
            prefix <- ifelse(input$pValuePrefix == "P = ", "P = ", "")
            
            # Construct the final string with prefix and potentially modified p-value
            final_string <- paste0(prefix, formatted_p)
            
            return(final_string)
          })
          
          
          
          # Update plot_data with formatted p-values
          plot_data$`p.signif` <- unlist(formatted_pvalues)
          # Finally, add the formatted p-values to the plot
          plot <- plot + stat_pvalue_manual(plot_data, label = "p.signif", 
                                            y.position = y_position_auto, label.size = input$sigSize, 
                                            bracket.size = input$bracketSize, 
                                            step.increase = input$stepIncrease, hide.ns = input$hideNS, 
                                            tip.length = input$tipLength, na.rm = TRUE, 
                                            inherit.aes= FALSE)
        }
      }else{
        plot_data <- comparisonResults()$test
        if("P Value" %in% names(plot_data)) {
          plot_data <- plot_data %>% 
            rename(p.signif = `P Value`)
        } else if("P Value (Two-Tailed)" %in% names(plot_data)) {
          plot_data <- plot_data %>%
            rename(p.signif = `P Value (Two-Tailed)`)
        }
        # Ensure the column is numeric for further operations
        plot_data <- plot_data %>%
          mutate(p.signif = as.numeric(as.character(p.signif)))
        
        # Then, format p-values according to user preferences
        formatted_pvalues <- lapply(plot_data$`p.signif`, function(p) {
          # Format the p-value with the specified number of decimal places
          formatted_p <- sprintf(paste0("%.", input$pValueDecimals, "f"), p)
          
          # Check and remove the leading zero if required
          if(input$remove0) {
            formatted_p <- sub("^0\\.", ".", formatted_p)
          }
          
          # Determine the prefix based on user selection
          prefix <- ifelse(input$pValuePrefix == "P = ", "P = ", "")
          
          # Construct the final string with prefix and potentially modified p-value
          final_string <- paste0(prefix, formatted_p)
          
          return(final_string)
        })
        
        # Update plot_data with formatted p-values
        plot_data$`p.signif` <- unlist(formatted_pvalues)
        # Finally, add the formatted p-values to the plot
        plot <- plot + stat_pvalue_manual(plot_data, label = "p.signif", 
                                          y.position = y_position_auto, label.size = input$sigSize, 
                                          bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, 
                                          tip.length = input$tipLength, na.rm = TRUE, 
                                          inherit.aes= FALSE)
      }
    }else if (input$add_significance == "none"){
      plot <- plot
    }
    
    # Set the plot size based on the height of the plot
    shinyjs::runjs(paste0('$("#download-container").height($("#plot").height());'))
    shinyjs::runjs(paste0('$("#download-container").width($("#plot").width());'))

    # Print the plot
    print(plot)
    graph_generated(TRUE)
  },
  width = function() {
    input$width * 100  # Adjust the multiplier as needed
  },
  height = function() {
    input$height * 100  # Adjust the multiplier as needed
    
  })
}
  
  
  
  
  
  
  
  
  
  
  







