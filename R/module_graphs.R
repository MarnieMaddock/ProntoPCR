#module_graphs.R
#define function to dynamically change file path of rmd file
get_rmd_template_path <- function(file) {
  if (file.exists(paste0("inst/rmd_templates/", file))) {
    return(paste0("inst/rmd_templates/", file))
  } else {
    return(system.file("rmd_templates", file, package = "ProntoPCR"))
  }
}

graphsSidebar <- function(id) {
  ns <- NS(id)
  tagList(
      #graph creation options
      h4(HTML("<b>Create Graph</b>")),
      radioButtons(ns("select_dcq_or_ddcq"), HTML("Select whether to graph 2<sup>-(ΔCq)</sup> or 2<sup>-(ΔΔCq)</sup> data:"),
                   choices = c("ΔCq" = "dcq", "∆ΔCq" = "ddcq"),
                   selected = "dcq"),
      uiOutput(ns("ddcqMessage_graphs")),
      uiOutput(ns("selected_samples_list")),
      uiOutput(ns("selected_gene_ui")), # Display the gene selection message if ddcq is being graphed
      tags$br(),
      fluidRow(
        column(width = 6, uiOutput(ns("condition_selector"))), # For DCQ: Render the dynamic selectInput for choosing conditions and columns
        column(width = 6, uiOutput(ns("column_selector"))) # For DCQ: Render the genes that have had fold change performed i.e. fc_ columns
      ),
      textInput(ns("x_axis_positions"), "Enter the order to display x-axis groups (comma-separated):", placeholder = "e.g., F_treated,F_untreated"),
      helpText("Ensure spelling is exactly as it is entered in the Group column. Do NOT use spaces. e.g., F_treated,F_untreated"),
      # Graph labels customisation.
      h4(HTML("<b>Customise Graph</b>")),
      h5(HTML("<b>Labels</b>")),
      uiOutput(ns("x_axis_labels")), # change names of x axis groups
      checkboxInput(ns("rotate_labels"), "Rotate x-axis labels", value = FALSE),  # Checkbox for label rotation
      uiOutput(ns("dynamic_y_label_input")), # automatically changes the y axis gene name depending on what gene is selected.
      textInput(ns("x_label"), "Enter X-axis Label. Markup is accepted.", value = "Group"), #renders x axis label input
      helpText("To have no x or y-axis label enter a space."),
      tags$br(),
      selectInput(ns("font_selector"), "Select Font", choices = c("Arial", "Arial Bold", "Calibri", "Times New Roman", "Georgia", "Comic Sans MS", "Century Gothic", "Tahoma")),
      fluidRow(
        column(
          width = 6,
          numericInput(ns("x_axis_title_font_size"), "X-axis Title Font Size:", value = 14, min = 1, max = 50),
          numericInput(ns("x_axis_label_font_size"), "X-axis Text Font Size:", value = 12, min = 1, max = 50)
        ),
        column(
          width = 6,
          numericInput(ns("y_axis_title_font_size"), "Y-axis Title Font Size:", value = 14, min = 1, max = 50),
          numericInput(ns("y_axis_label_font_size"), "Y-axis Text Font Size:", value = 12, min = 1, max = 50)
        )
      ),
      uiOutput(ns("legendCustomization")), # Display the legend customization options
      # graph design options
      h5(HTML("<b>Graph Design</b>")),
      # colour schemes
      selectInput(ns("color_scheme_select"), 
                  label = tags$span("Choose Colour Scheme:",
                                    tags$i(class =  "glyphicon glyphicon-info-sign", 
                                           style = "color:#00359bff;",
                                           title = "Custom colour selection is currently unavailable. Saving graphs as an SVG allow the user to alter graph aesthetics in illustrator, inkscape etc. Alternatively, colour scheme requests can be sent to: mlm715@uowmail.edu.au. Please include a list of colours (hex codes e.g. #000000, #63b8ff) in order.")
                                           ),
                  choices = c("Default" = "default", "Colourblind friendly 1" = "colourblind1",
                              "Colourblind friendly 2" = "colourblind2", "Colourblind friendly 3" = "colourblind3",
                              "Colourblind friendly 4" = "colourblind4", "Colourblind friendly 5" = "colourblind5",
                              "Grays 1" = "grays", "Grays 2" = "grays2",
                              "Grays 3" = "grays3", "ElectraGray" = "electraGray", "Bones" = "bones",
                              "Oranges 1" = "oranges", "Oranges 2" = "oranges2", "Pinks 1" = "pinks",
                              "Pinks 2" = "pinks2", "Blues 1" = "blues", "Blues 2" = "blues2", "Greens 1" = "greens",
                              "Greens 2" = "greens2", "Greens 3" = "greens3", "Green to Purple" = "green2purple",
                              "Purples 1" = "purples", "Purples 2" = "purples3", "Purple to Orange" = "purple2orange",
                              "Blaze" = "blaze", "Blaze 2" = "blaze2", "Peace 1" = "peace", "Peace 2" = "peace2", "Ireland" = "ireland",
                              "Two-tone 1" = "twotone1", "Two-tone 2" = "twotone2", "Two-tone 3" = "twotone3",
                              "Pastels 1" = "pastels", "Pastels 2" = "pastels2", "Pastels 3" = "pastels3", "Pastels 4" = "pastels4",
                              "Pastels 5" = "pastels5", "Pastels 6" = "pastels6", "Pastels 7" = "pastels7", "Vibrant 1" = "vibrant",
                              "Vibrant 2" = "vibrant2", "Vibrant 3" = "vibrant3", "Marnie's theme" = "marnie", "Amy's theme" = "amy"),
                  selected = "custom"),
      # Types of plots
      selectInput(ns("plot_type"), "Choose Plot Type:",
                  choices = c("Scatter Plot (Recommended)" = "dot", "Grouped Scatter Plot (Recommended)" = "dot_group", "Column Graph" = "column"),
                  selected = "dot"),
      # add significance to graph based of stats in previous tabe
      uiOutput(ns("significanceOptions")),
      uiOutput(ns("sigUI")), # Display significance display options depending on what type of significance is added to the graph.
      # Error bar options
      h5(HTML("<b>Error Bars</b>")),
      radioButtons(ns("error_type"), "Choose Error Bar Type:",
                   choices = list("Standard Deviation" = "sd", 
                                  "Standard Error" = "se",
                                  "95% Confidence Interval" = "ci"),
                   selected = "se"),
      checkboxInput(ns("start_at_zero"), "Start Y-axis at 0: Note this may cut off data points/error bars close to zero.", value = TRUE),
      uiOutput(ns("extraOptionsUI")), # Display the extra options for scatter plot
  )
}

    
graphsMain <- function(id) {
  ns <- NS(id)
  tagList(
    # Add a plot
    plotOutput(ns("plot")),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    h4(HTML("Download Graph")),
    tags$br(),
    h6("Due to differences in screen displays and resolution, the displayed graph may be slightly different to the downloaded graph. Check the downloaded graph to see if the settings are suitable. SVG graphs are editable in illustrator, inkscape, powerpoint etc and are the recommended option."),
    fluidRow(
      column(4,
             selectInput(ns("file_format"), "Choose File Format:",
                         choices = c("svg", "png", "jpeg", "tiff"),
                         selected = "svg")
      ),
      column(4,
             numericInput(ns("dpi"), "DPI:", 300)
      ),
      column(2,
             numericInput(ns("width"), "Width (inches):", 7)
      ),
      column(2,
             numericInput(ns("height"), "Height (inches):", 5)
      ),
      column(4,
             # Add a download button to save the graph according to above selections
             downloadButton(ns("downloadGraph"), "Download Graph")
      ),
      column(4,
             # Add a download button to save all the selected graph options as a html (RMD created) file
             downloadButton(ns("downloadGraphOptions"), "Download Selected Graph Options"))
    ),
    tags$br(),
    tags$br(),
    tags$br()
  )
}

graphsServer <- function(id, tabselected, values, ddcq_repAvg, descriptivesTable, theme_Marnie, wrangled_data, ddcq_selected_gene, ddcq_data, select_dcq_or_ddcq_stats, stats_gene, shapiro_data_reactive, graph_generated, rep_avg_data, rep_avg_data_ddcq, comparisonResults, group_comparison) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observe changes in data selection to reset the graph_generated flag
    observeEvent(input$select_dcq_or_ddcq, {
      graph_generated(FALSE)
    })
    
    observeEvent(select_dcq_or_ddcq_stats(), {
      graph_generated(FALSE)
    })
    
    #UI options for graphs selected
    output$extraOptionsUI <- renderUI({
      req(input$plot_type, tabselected())
      if (tabselected() == 5){
        if (input$plot_type == "column"){
          tagList(
            fluidRow(
              column(width = 6, numericInput(ns("errorbar_width"), "Error Bar Width:", value = 0.2, min = 0.05, max = 5, step = 0.1)),
              column(width = 6, numericInput(ns("errorbar_thickness"), "Error Bar Thickness:", value = 1, min = 0.05, step = 0.2))
            ),
            h5(HTML("<b>Point Design</b>")),
            selectInput(ns("fill_color_toggle"), "Choose Fill or Border:",
                        choices = c("Fill" = "fill", "Border" = "color"),
                        selected = "fill"),
            numericInput(ns("dot_size"), "Point Size:", value = 1.5, min = 1, max = 10, step = 0.5),
            numericInput(ns("dot_spacing"), "Point Spacing:", value = 2.7, min = 0.1, max = 5, step = 0.1)
          )
        } else if (input$plot_type == "dot" || input$plot_type == "dot_group") {
          # UI for scatter plot options
          tagList(
            fluidRow(
              column(width = 3.5, 
                     numericInput(ns("error_bar_width"), "Error Bar Width:", value = 0.2, min = 0.05, max = 5, step = 0.1),
                     numericInput(ns("average_line_width"), "Average Line Width:", value = 0.15, min = 0.01, step = 0.05)),
              column(width = 3.5, 
                     numericInput(ns("error_bar_thickness"), "Error Bar Thickness:", value = 1.5, min = 0.05, step = 0.2),
                     numericInput(ns("average_line_thickness"), "Average Line Thickness:", value = 1, min = 0.05, step = 0.1)),
              uiOutput(ns("error_bar_colUI")),
              #column(width = 3.5,
                     # checkboxInput(ns("use_geo_mean"), 
                     #               label = tags$span(
                     #                 "Use Geometric Mean for Average Line",
                     #                 tags$i(
                     #                   class =  "glyphicon glyphicon-info-sign", 
                     #                   style = "color:#00359bff;",
                     #                   title = "If selected, it is highly recommended to select 95% confidence interval as your error bar type."
                     #                 )),
                     #                FALSE),
                     #checkboxInput(ns("match_colour_error"), "Match error bar colour with dot points", FALSE),
                     #checkboxInput(ns("match_colour_avg"), "Match average line colour with dot points", FALSE))
            ),
            h5(HTML("<b>Point Design</b>")),
            numericInput(ns("point_size"), "Point Size:", value = 4.5, min = 0.5, max = 20, step = 0.5),
            checkboxInput(ns("change_shapes"), "Change to paired shapes.", FALSE),
            fluidRow(
              column(width = 6, numericInput(ns("stroke_thickness"), "Shape Outline Thickness", value = 1.5, min = 0.1, step = 0.2)),
              column(width = 6, numericInput(ns("jitter_amount"), "Point Spread:", value = 0.2, min = 0, max = 1.5, step = 0.1))
            ),
            numericInput(ns("seed_input"), 
                         label = tags$span("Set Seed:", 
                                           tags$i(
                                             class =  "glyphicon glyphicon-info-sign", 
                                             style = "color:#00359bff;",
                                             title = "This is a random value that allows you to change the order of the points on your graph. Change the number if points overlap for example."
                                           )),
                         value = 123),
            tags$br(),
            tags$br()
          )
        } else {
          # Default or other options if needed
          tagList(
            h5("Please select a plot type to see options.")
          )
        }
      }
    })
    
    #display message if ddcq dataset hasn't been created yet
    output$ddcqMessage_graphs <- renderUI({
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

    #display ddcq samples
    output$selected_samples_list <- renderUI({
        if(input$select_dcq_or_ddcq == "ddcq"){
          samples <- ddcq_repAvg()$cell
          samples_text <- paste("Samples selected:", paste(samples, collapse = ", "))
          tags$p(samples_text)
        } else {
          NULL
        }
      })

   
    
    
    # Reactive values to store user selections
    selected_stats <- reactive({
      sub("_stats", "", select_dcq_or_ddcq_stats())
    })
    
    selected_graphs <- reactive({
      input$select_dcq_or_ddcq
    })
  
    # Reactive expression to check for consistency of dcq or ddcq selection in the stats and graphs tab
    data_consistency <- reactive({
      selected_stats() == selected_graphs()
    })

    
    col_discrepancy <- reactive({
      selected_stat_column <- stats_gene()
      # If input$select_dcq_or_ddcq equals "dcq", it uses input$fc_dcq_column (the column selected for DCq plotting).
      # Otherwise, it uses input$fc_ddcq (the column selected for DDCq plotting).
      #selected_plot_column <- if(input$select_dcq_or_ddcq == "dcq") input$fc_dcq_column else "fc_ddcq"
      # Determine selected plot column based on the input
      selected_plot_column <- if (input$select_dcq_or_ddcq == "dcq") {
        input$fc_dcq_column
      } else {
        # Directly reference the column name; you might want to fetch specific values for comparison
        "fc_ddcq"
      }
      #will return FALSE if the selected gene for statistics does not match the gene for graphing
      !is.null(selected_stat_column) && !is.null(selected_plot_column) && selected_stat_column != selected_plot_column
    })
    
    # Observe the data consistency check and show modal if mismatch
    observeEvent(data_consistency(), {
      if (!data_consistency()) {
        showModal(modalDialog(
          title = "Data Selection Mismatch",
          "Please ensure that the selected data type for statistics and graphs are the same. i.e. ∆Cq vs ∆ΔCq.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (col_discrepancy()) {
        # Check column discrepancy only if data is consistent
        showModal(modalDialog(
          title = "Discrepancy Detected",
          "The selected gene for statistics does not match the gene for graphing. Please adjust your selections to ensure that the correct p-values are added to your graph.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })
    
    observeEvent(col_discrepancy(), {
      if (col_discrepancy() && data_consistency()) {
        showModal(modalDialog(
          title = "Discrepancy Detected",
          "The selected gene for statistics does not match the gene for graphing. Please adjust your selections to ensure that the correct data is being compared.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })
    
    
    observeEvent(input$select_dcq_or_ddcq,{
        if (input$select_dcq_or_ddcq == "dcq") {
          # Render the dynamic selectInput for choosing condition
          output$condition_selector <- renderUI({
            req(wrangled_data())  # Ensure data is available
            
            # Generate selectInput for choosing the condition dynamically
            selectInput(ns("selected_condition"), "Select Samples", choices = unique(wrangled_data()$cell),
                        multiple = TRUE)
          })
          
          # Render the dynamic selectInput for choosing the column
          output$column_selector <- renderUI({
            req(wrangled_data())  # Ensure data is available
            
            # Filter column names to include only those starting with "fc_dcq"
            fc_dcq_columns <- grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
            
            # Generate selectInput for choosing the column dynamically
            selectInput(ns("fc_dcq_column"), "Select Gene", choices = fc_dcq_columns)
          })
        } else {
          # Hide the UI elements if ddcq is selected
          output$condition_selector <- renderUI(NULL)
          output$column_selector <- renderUI(NULL)
        }
      })
    
    #display what geen was selected when creating ddcq dataset
    observeEvent(input$select_dcq_or_ddcq, {
        # Check if 'ddcq' is selected
        if (input$select_dcq_or_ddcq == "ddcq") {
          # Display the gene selection message
          output$selected_gene_ui <- renderUI({
            textOutput(ns("selected_gene_message"))
          })
        } else {
          # Hide the message when 'dcq' is selected
          output$selected_gene_ui <- renderUI({
            NULL
          })
        }
      })
    
    #Display the gene selection message if ddcq is being graphed
    output$selected_gene_message <- renderText({
        req(ddcq_selected_gene())  # Ensure there is a selection
        # Use gsub to remove "dcq_" from the selected gene's name
        selected_gene_cleaned <- gsub("^dcq_", "", ddcq_selected_gene())
        paste("You are currently graphing gene:", selected_gene_cleaned)
      })

    
    # Observe changes in the use_geo_mean checkbox
    # observeEvent(input$use_geo_mean, {
    #   if (input$use_geo_mean) {
    #     # Update radio buttons to select CI when geometric mean is used
    #     updateRadioButtons(session, "error_type",
    #                        choices = list("95% Confidence Interval" = "ci"),
    #                        selected = "ci")
    #   } else {
    #     # Enable all options if the geometric mean is not selected
    #     updateRadioButtons(session, "error_type",
    #                        choices = list("Standard Deviation" = "sd", 
    #                                       "Standard Error" = "se",
    #                                       "95% Confidence Interval" = "ci"),
    #                        selected = input$error_type)
    #   }
    # })
    
    output$dynamic_y_label_input <- renderUI({
        # Initialize variable to store cleaned gene name
        selected_gene_cleaned <- ""
        
        # Determine which input to use based on the condition selected
        if (input$select_dcq_or_ddcq == "dcq") {
          # Ensure the fc_dcq_column input is used for dcq condition
          req(input$fc_dcq_column)  # Ensure there is a selection for the dcq condition
          selected_gene_cleaned <- gsub("^fc_dcq_", "", input$fc_dcq_column)  # Clean the gene name for dcq
        } else if (input$select_dcq_or_ddcq == "ddcq") {
          # Assume there's another input mechanism for ddcq or use a default value
          req(ddcq_selected_gene())  # Placeholder, adjust as necessary for ddcq
          selected_gene_cleaned <- gsub("^dcq_", "", ddcq_selected_gene())  # Clean the gene name for ddcq
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
        textInput(ns("y_label"), "Enter Y-axis Label. Markup is accepted.", value = placeholder_text)
      })
    
    
    # Dynamic generation of text inputs based on positions
    #check this works
    xAxis_positions <- observeEvent(input$labels_positions, {
        # Parse labels if user entered them
        filtered_labels <- if (!is.null(input$labels_positions)) {
          unlist(strsplit(input$labels_positions, ","))
        } else {
          NULL
        }
        
        # Update the UI with text boxes for custom labels
        label_textboxes <- lapply(filtered_labels, function(label) {
          #NAMESPACE THIS?
          textInput(paste0("label_", label), label, label)
        })
        # Render UI for text boxes
        output$labels_textboxes <- renderUI({
          label_textboxes
        })
      })

    # Reactive value to store processed x-axis positions
    processed_positions <- reactive({
      req(input$x_axis_positions, input$plot_type)  # Ensure there are positions to process
      positions <- if (!is.null(input$x_axis_positions)) {
        unlist(strsplit(input$x_axis_positions, ","))
      } else {
        NULL
      }
      
      if (input$plot_type == "dot_group" && !is.null(positions)) {
        # Remove anything before the underscore and keep the part after the underscore
        unique(sapply(positions, function(pos) {
          strsplit(pos, "_")[[1]][2]
        }))
      } else {
        positions
      }
    })
    
    # Dynamic generation of text inputs based on positions
    output$x_axis_labels <- renderUI({
      positions <- processed_positions()
      
      if (!is.null(positions) && length(positions) > 0) {
        lapply(positions, function(pos) {
          textInput(inputId = ns(paste0("label_", pos)),
                    label = paste("Label for", pos),
                    value = pos)  # Initial value can be set to the position itself or an empty string
        })
      }
    })
    
    # Reactive function to get user-entered labels
    user_labels <- reactive({
      positions <- processed_positions()
      
      if (!is.null(positions) && length(positions) > 0) {
        sapply(positions, function(pos) {
          input[[paste0("label_", pos)]]
        })
      }
    })
    
          
    mean_sd <- function(x) {
      n <- sum(!is.na(x)) # Number of non-NA values
      if (n > 1) { # Can only calculate sd if n > 1
        sd_x <- sd(x, na.rm = TRUE)
        mean_x <- mean(x, na.rm = TRUE)
        return(data.frame(y = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x))
      } else {
        return(data.frame(y = NA, ymin = NA, ymax = NA)) # Return NA if not enough data
      }
    }
    
    se <- function(x) {
      n <- sum(!is.na(x))
      if (n > 1) {
        sd_x <- sd(x, na.rm = TRUE) / sqrt(n)
        mean_x <- mean(x, na.rm = TRUE)
        return(data.frame(y = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x))
      } else {
        return(data.frame(y = NA, ymin = NA, ymax = NA))
      }
    }
    
    ci <- function(x, confidence_level = 0.95) {
      n <- sum(!is.na(x))
      if (n > 1) {
        sd_x <- sd(x, na.rm = TRUE)
        mean_x <- mean(x, na.rm = TRUE)
        z <- qnorm((1 + confidence_level) / 2)  # Calculates the z-value for the specified confidence level
        error_margin <- z * sd_x / sqrt(n)
        return(data.frame(y = mean_x, ymin = mean_x - error_margin, ymax = mean_x + error_margin))
      } else {
        return(data.frame(y = NA, ymin = NA, ymax = NA))  # Return NA if not enough data
      }
    }
    
    # Reactive function to get col names (i.e. gene)
    # access `fc_dcq_columns_reactive()` as a reactive value.
    #check works
    fc_dcq_columns_reactive <- reactive({
        req(wrangled_data())  # Ensure data is available
        grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
      })

    # calculate number of groups
    num_groups <- reactive({
        length(unique(shapiro_data_reactive()$cell))
      })
    
    output$error_bar_colUI <- renderUI({
      req(input$plot_type =="dot")# Only show if plot_type is "dot"
      
      tagList(
        column(width =3.5,
               checkboxInput(ns("match_colour_error"),"Match error bar colour with dot points",FALSE),
               checkboxInput(ns("match_colour_avg"),"Match average line colour with dot points",FALSE))
        )
      })
    
    
    output$significanceOptions <- renderUI({
      req(input$plot_type !="dot_group")# Only show if plot_type is not "dot_group"
      tagList(
      h5(HTML("<b>Significance</b>")),
      radioButtons(ns("add_significance"), "If statistics have been performed, add significance to graph.", choices = c("None" = "none", "Asterisk Notation" = "asterix", "P Values" = "pval","Compact Letter Display" = "cld"),
                 selected = "none")
      )
    })
    
    output$sigUI <- renderUI({
      req(input$add_significance)
        if(input$add_significance == "asterix" || input$add_significance == "cld" || input$add_significance == "pval"){
          tagList(
            fluidRow(
              column(4, numericInput(ns("sigSize"), "Label Size", min = 0, max = 20, value = 5)),
              if(input$add_significance != "cld") {
                tagList(
                  column(4, numericInput(ns("bracketSize"), "Bracket Thickness", min = 0, max = 10, value = 0.8, step = 0.1)),
                  column(4, numericInput(ns("stepIncrease"), "Step Increase", max = 20, value = 0.1, step = 0.1))
                )
              }
            ),
            if(input$add_significance != "cld" && num_groups() > 2) {
              fluidRow(
                column(6, checkboxInput(ns("hideNS"), "Hide ns", value = FALSE))
              )
            },
            if(input$add_significance == "pval"){
              tagList(
                fluidRow(
                  column(6, selectInput(ns("pValuePrefix"), "P-value Prefix", choices = c("None", "P = "))),
                  column(6, numericInput(ns("pValueDecimals"), "Decimal Places for P-value", value = 2, min = 0, max = 10)),
                ),
                fluidRow(
                  #column(6, numericInput("stepIncrease", "Step Increase", min = 0, max = 20, value = 0.1, step = 0.1)),
                  column(6, checkboxInput(ns("remove0"), "Remove leading zero from P-value", value = FALSE))
                )
              )
            },
            if(input$add_significance == "cld"){
              fluidRow(
                column(6, numericInput(ns("stepIncrease"), "Step Increase", max = 20, value = 0.1, step = 0.1))
              )
            },
            # Tip Length is applicable for all scenarios except "cld"
            if(input$add_significance != "cld") {
              fluidRow(
                column(12, numericInput(ns("tipLength"), "Tip Length", max = 20, value = 0.02, step = 0.01))
              )
            }
          )
        }
      })
    
    
    # Reactive to return data based on the selected type
    dcq_or_ddcq <- reactive({
      if (input$select_dcq_or_ddcq == "dcq") {
        # If 'dcq' is selected, return wrangled_data()
        wrangled_data()
      } else {
        # If 'ddcq' is selected, return ddcq_data()
        ddcq_data()
      }
    })
    
    
    # render plot according to user input options
    # PLOT CODE
    ## EXTENISVE code for generating customisable graphs:
    output$plot <- renderPlot({
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
        
       #set seed for the graph
        set.seed(input$seed_input)
        # select only the samples selected for the grpah by the user 
        #add for geo mean graphing
        # if(input$select_dcq_or_ddcq == "dcq"){
        #   if(input$use_geo_mean == FALSE){
        #     filtered_data <- dcq_or_ddcq() %>%
        #       filter(cell %in% input$selected_condition)
        #     filtered_rep_avg_data <- rep_avg_data() %>%
        #       filter(cell %in% input$selected_condition)
        #   } else if (input$use_geo_mean == TRUE){
        #     print(head(descriptivesTable()))
        #     gene_name <- input$fc_dcq_column
        #     filtered_data <- dcq_or_ddcq() %>%
        #       filter(cell %in% input$selected_condition)
        #     
        #     filtered_rep_avg_data  <- descriptivesTable() %>% 
        #       dplyr::filter(Sample %in% input$selected_condition) %>% 
        #       dplyr::select(Sample, "Geometric Mean") %>% 
        #       dplyr::rename(cell = Sample) %>% 
        #       mutate(group = str_extract(cell, "(?<=_).*")) %>% 
        #       dplyr::rename(!!input$fc_dcq_column := `Geometric Mean`)
        #   }
        
        
        if(input$select_dcq_or_ddcq == "dcq"){
          filtered_data <- dcq_or_ddcq() %>%
            filter(cell %in% input$selected_condition)
          filtered_rep_avg_data <- rep_avg_data() %>%
            filter(cell %in% input$selected_condition)
          
          
          #If filtered_rep_avg_data has column fc_avg, rename to input$selected_condition
          #This is needed if only one gene is present in the dataset apart from the housekeepers
          if("fc_avg" %in% colnames(filtered_rep_avg_data)){
            filtered_rep_avg_data <- filtered_rep_avg_data %>%
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
              unique(filtered_data$cell)
            }
          } else {
            unlist(strsplit(input$x_axis_positions, ","))
          }
          
        }else if(input$select_dcq_or_ddcq == "ddcq"){
          filtered_data <- dcq_or_ddcq()
          filtered_rep_avg_data <- rep_avg_data_ddcq()
          
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
              unique(filtered_data$group)
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

        # colours to use for graphing
        color_schemes <- reactiveValues(colourblind1 = c("#00359c", "#648fff", "#785ef0", "#dc267f", "#fe6100", "#ffb000"),
                                                 colourblind2 = c("#ffbd00", "#ff5400", "#ff0054", "#9e0059", "#390099"),
                                                 colourblind3 = c("#70d6ff", "#ff70a6", "#ff9770", "#ffd670", "#e9ff70"),
                                                 colourblind4 = c("gray20", "#ff0167ff", "#117f90ff", "#40008fff", "#785ef1","#67ccfeff" ),
                                                 colourblind5 = c("#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff","#00359c" ),
                                                 grays = c("gray10", "gray30", "gray50", "gray70", "gray80", "gray100"),
                                                 grays2 = c("#f8f9fa", "#e9ecef", "#dee2e6", "#ced4da", "#adb5bd", "#6c757d", "#495057", "#343a40", "#212529"),
                                                 grays3 = c("#2b2d42", "#8d99ae", "#edf2f4"),
                                                 electraGray = c("#e00154", "#222337", "#e6e1dd", "#b4a8b4", "#ddd2cf"),
                                                 bones = c("#edede9", "#d6ccc2", "#f5ebe0", "#e3d5ca", "#d5bdaf"),
                                                 oranges = c("#ffc971", "#ffb627", "#ff9505", "#e2711d" ,"#cc5803"),
                                                 oranges2 = c("#ff4800", "#ff6000", "#ff6d00", "#ff7900", "#ff8500", "#ff9100", "#ff9e00", "#ffaa00", "#ffb600"),
                                                 pinks = c("#ffe5ec", "#ffc2d1", "#ffb3c6", "#ff8fab", "#fb6f92"),
                                                 pinks2 = c("#590d22", "#800f2f", "#a4133c", "#c9184a", "#ff4d6d", "#ff758f", "#ff8fa3", "#ffb3c1", "#ffccd5", "#fff0f3"),
                                                 blues = c("#03045e", "#0077b6", "#00b4d8", "#90e0ef", "#caf0f8"),
                                                 blues2 = c("#03045e", "#023e8a", "#0077b6", "#0096c7", "#00b4d8", "#48cae4", "#90e0ef", "#ade8f4", "#caf0f8"),
                                                 greens = c("#dad7cd", "#a3b18a", "#588157", "#3a5a40", "#344e41"),
                                                 greens2 = c("#d8f3dc", "#b7e4c7", "#95d5b2", "#74c69d", "#52b788", "#40916c", "#2d6a4f", "#1b4332", "#081c15"),
                                                 greens3 = c("#073b3a", "#0b6e4f", "#08a045", "#6bbf59"),
                                                 green2purple = c("#35e95f", "#35d475", "#35ac7a", "#347f83", "#2e518a", "#40288f", "#5702a1", "#6500a3", "#8127b9"),
                                                 purples = c("#c8b1e4", "#9b72cf", "#532b88", "#2f184b", "#f4effa"),
                                                 purples3 = c("#7b2cbf", "#9d4edd", "#e0aaff" ),
                                                 purple2orange = c("#9d53ff", "#b29ef8", "#f8d9c6", "#ffb57d", "#fb9649"),
                                                 blaze = c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500"),
                                                 blaze2 = c("#14213d", "#fca311", "#C49792", "#e5e5e5"),
                                                 peace = c("#2e58a4ff", "#b69e71ff", "#e3ded4ff", "#71aec7ff","#4f5357ff"),
                                                 peace2 = c("#797d62", "#9b9b7a", "#d9ae94", "#f1dca7", "#ffcb69","#d08c60", "#997b66") ,
                                                 ireland = c("#2ec4b6", "#cbf3f0", "#e5e5e5", "#ffbf69", "#ff9f1c"),
                                                 twotone1 = c("#023e8a", "#0077b6", "#ff7900","#ff9e00"),
                                                 twotone2 = c("#90b5daff", "#91b5daff", "#e47076ff", "#e46f74ff"),
                                                 twotone3 = c("#447db3ff", "#e7953fff"),
                                                 pastels = c("#ddfff7", "#93e1d8", "#ffa69e"),
                                                 pastels2 = c("#90f1ef", "#ffd6e0", "#ffef9f"),
                                                 pastels3 = c("#bdb2ff", "#ffcad4", "#b0d0d3" ),
                                                 pastels4 = c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff"),
                                                 pastels5 = c("#ccd5ae", "#e9edc9", "#fefae0", "#faedcd", "#d4a373"),
                                                 pastels6 = c("#ffadad", "#ffd6a5", "#fdffb6", "#caffbf", "#9bf6ff", "#a0c4ff", "#bdb2ff", "#ffc6ff", "#fffffc"),
                                                 pastels7 = c("#809bce", "#95b8d1","#b8e0d2", "#d6eadf", "#eac4d5"),
                                                 vibrant = c("#ff0f7b", "#f89b29" ),
                                                 vibrant2 = c("#10e0ff", "#0086eb", "#006ee9", "#ffcd00", "#ffef00"),
                                                 vibrant3 = c("#ff00c1", "#9600ff", "#4900ff", "#00b8ff", "#00fff9"),
                                                 amy = c("#25b9e8ff", "#f67836ff", "#7486b8ff",  "#f9ac1dff"),
                                                 marnie = c("#648fff", "#648fff", "#dc267f", "#dc267f", "#ffb000",  "#ffb000","#785ef0","#785ef0", "#fe6100", "#fe6100","#00359c","#00359c","#2ec4b6", "#2ec4b6"),
                                                 default = c("#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de", "#56cfe1","#64dfdf", "#72efdd", "#80ffdb", "#B7D7B9", "#D2C3A8", "#E0B9A0","#EDAF97","#C49792", "#AD91A3", "#9D91A3")
        )
        
        # Get the color scheme based on user input
        color_scheme <- input$color_scheme_select
        colors <- if (color_scheme == "default") {
          # If the user selects "Custom," use the custom colors defined earlier
          color_schemes$default
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
            need(FALSE, "The selected colour palette is smaller than the number of x-axis groups. Please select another option.")
          )
        }
        # Define x-axis theme based on checkbox
        x_axis_theme <- if (input$rotate_labels) {
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
        }
        
        #define the shapes to be used in the graph
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
        summary_stats <- error_fun(filtered_data[[y_aes]])
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
          dotplot_data <- filtered_data
          if (input$match_colour_error) {
            dotplot_data$error_bar_color <- dotplot_data[[as.character(x_aes)]]  # Use the same column as x_aes for colors
          } else {
            dotplot_data$error_bar_color <- "black"  # Use black when checkbox is unchecked
          }
          dotplot_data
        })
        
        # Reactive for color assignments based on checkbox input for average bars
        dotplot_data_reactive_avg <- reactive({
          dotplot_data <- filtered_rep_avg_data
          if (input$match_colour_avg) {
            dotplot_data$avg_bar_color <- dotplot_data$cell  # Use the same column as x_aes for colors
          } else {
            dotplot_data$avg_bar_color <- "black"  # Use black when checkbox is unchecked
          }
          dotplot_data
        })
        
        grouped_dotplot_data_reactive_avg <- reactive({
          dotplot_data <- filtered_data
          
          # Determine the column to use for mean calculation based on the dataset
          y_aes_avg_dot <- if (input$select_dcq_or_ddcq == "dcq") {
            y_aes_avg
          } else {
            sym("fc_ddcq")
          }
          
          # Group data by 'group' and calculate mean, ymin, ymax
          dotplot_data <- dotplot_data %>%
            group_by(group) %>%
            summarise(
              avg = mean(!!y_aes_avg_dot, na.rm = TRUE),
              sd = sd(!!y_aes, na.rm = TRUE),     # Calculate sd on raw data
              n = n()
            ) %>%
            mutate(
              se = sd / sqrt(n),
              ci = se * qnorm(0.975),  # For 95% confidence interval
              ymin = case_when(
                input$error_type == "sd" ~ avg - sd,
                input$error_type == "se" ~ avg - se,
                input$error_type == "ci" ~ avg - ci
              ),
              ymax = case_when(
                input$error_type == "sd" ~ avg + sd,
                input$error_type == "se" ~ avg + se,
                input$error_type == "ci" ~ avg + ci
              )
            )
        })
        
        
        # Dynamic generation of text inputs for legend labels
        output$legend_labels <- renderUI({
          conditions <- unique(filtered_data$condition)
          
          if (!is.null(conditions) && length(conditions) > 0) {
            lapply(conditions, function(cond) {
              textInput(inputId = ns(paste0("legend_label_", cond)),
                        label = paste("Label for", cond),
                        value = input[[paste0("legend_label_", cond)]] %||% cond)# Preserve user-entered value if available. The %||% operator checks if the left-hand side is NULL and, if so, returns the right-hand side. This can help preserve user-entered values:
            })
          }
        })
        
        # Reactive function to get user-entered legend labels
        user_legend_labels <- reactive({
          req(!is.null(filtered_data$condition))  # Ensure the condition column is not null
          conditions <- unique(filtered_data$condition)
          
          if (!is.null(conditions) && length(conditions) > 0) {
            labels <- sapply(conditions, function(cond) {
              input[[paste0("legend_label_", cond)]]
            })
            names(labels)<- conditions
            labels
          }
        })

        legendSizes <- reactiveValues(
          title =14,
          groups =12)
        
          observe({
            updateNumericInput(session, ns("legend_text_size_title"), value = legendSizes$title)
            updateNumericInput(session, ns("legend_text_size"), value = legendSizes$groups)})
          
          observeEvent(input$legend_text_size_title,{
            legendSizes$title <- input$legend_text_size_title
          })
          
          observeEvent(input$legend_text_size,{
            legendSizes$groups <- input$legend_text_size
          })
        
        output$legendCustomization <- renderUI({
          req(input$plot_type == "dot_group")  # Only show if dot_group is selected
          tagList(
            textInput(ns("legend_title"), "Legend Title", value = input$legend_title %||%"Condition"),
            uiOutput(ns("legend_labels")),  # Add this line to include dynamic legend labels
            fluidRow(
            column(4,
              selectInput(ns("legend_position"), "Legend Position",
                        choices = list("Right" = "right", "Bottom" = "bottom", "Left" = "left", "Top" = "top", "None" = "none"),
                        selected = input$legend_position %||%"right")),
            column(4,
            numericInput(ns("legend_text_size_title"), "Legend Title Size:", value = legendSizes$title,min=1,max=50)),
            column(4,
            numericInput(ns("legend_text_size"), "Legend Groups Size:", value = legendSizes$groups,min=1,max=50)),
            )
          )
        })
        
        #Create your plot using ggplot2 with the selected dataset
        if (input$plot_type == "column"){
          req(input$fill_color_toggle, input$errorbar_width, input$errorbar_thickness, input$dot_size, input$dot_spacing)
          req(!is.null(filtered_data$condition))  # Ensure the condition column is not null
          if (input$fill_color_toggle == "color"){
            plot <- ggplot(filtered_data, aes(x = !!x_aes, y = !!y_aes)) +
              geom_bar(data = filtered_rep_avg_data, aes(x = !!x_aes, y = !!y_aes_avg, color = !!x_aes), stat = "identity", inherit.aes = FALSE, fill = "white", linewidth = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
              stat_summary(fun.data = error_fun, geom = "errorbar", width = input$errorbar_width, aes(color = !!x_aes), linewidth = input$errorbar_thickness, na.rm = TRUE,  show.legend = FALSE) +
              geom_beeswarm(size = input$dot_size, method = "hex", cex = input$dot_spacing, na.rm = TRUE, aes(color = !!x_aes),  show.legend = FALSE) +
              labs(y = input$y_label, x = input$x_label) +
              scale_color_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
              theme_Marnie +
              scale_x_discrete(limits = positions, labels = user_labels()) +
              x_axis_theme 
          }else if (input$fill_color_toggle == "fill"){
            plot <- ggplot(filtered_data, aes(x = !!x_aes, y = !!y_aes)) +
              geom_bar(data = filtered_rep_avg_data, aes(x = !!x_aes, y = !!y_aes_avg, fill = !!x_aes), stat = "identity", inherit.aes = FALSE, color = "black", linewidth = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
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
          req(!is.null(filtered_data$condition))  # Ensure the condition column is not null
          req(input$point_size, input$stroke_thickness, input$jitter_amount, input$error_bar_width, input$error_bar_thickness, input$average_line_width, input$average_line_thickness)
          plot <- ggplot(dotplot_data_reactive(), aes(x = !!x_aes, y = !!y_aes)) +
            geom_point(size = input$point_size, na.rm = TRUE, aes(color = !!x_aes, shape = !!x_aes),
                       show.legend = FALSE, stroke = input$stroke_thickness, position = position_jitter(width = input$jitter_amount)) +
            stat_summary(fun.data = error_fun, geom = "errorbar", width = input$error_bar_width,  aes(colour = error_bar_color), linewidth = input$error_bar_thickness, na.rm = TRUE,  show.legend = FALSE) +
            stat_summary(data = dotplot_data_reactive_avg(), aes(x = !!x_aes, y = !!y_aes_avg, color = avg_bar_color), inherit.aes = FALSE,
                         fun = mean, geom = "crossbar", width = input$average_line_width, linewidth = input$average_line_thickness, show.legend = FALSE, na.rm = TRUE) +  # Add average line for each column x
            labs(y = input$y_label, x = input$x_label) +
            scale_color_manual(values = c(setNames(colors, positions), "black" = "black"))+
            scale_shape_manual(values = shapes_reactive()) +
            theme_Marnie +
            scale_x_discrete(limits = positions, labels = user_labels()) +
            x_axis_theme
        } else if (input$plot_type == "dot_group") {
          # Grouped Dot Plot
          req(input$point_size, input$stroke_thickness, input$jitter_amount, input$error_bar_width, input$error_bar_thickness, input$average_line_width, input$average_line_thickness, input$legend_title, input$legend_position, input$legend_text_size_title, input$legend_text_size)
          x_aes <- sym("group")  # x-axis is based on the 'group' column
          y_aes_avg_group <- sym("avg")
          unique_conditions <- unique(filtered_data$condition)
          
          shapes_reactive <- reactive({
            unique_conditions <- length(unique(filtered_data$condition))
            
            if (input$change_shapes) {
              # Ensure the number of shapes matches the number of unique conditions
              shapes <- c(16, 21, 17, 24, 15, 22, 18, 23, 4, 3, 8, 10, 9, 11, 12, 13, 14)
              setNames(shapes[1:unique_conditions], unique(filtered_data$condition))
            } else {
              # Default shape order
              shapes <- c(16, 17, 15, 18, 4, 3, 8, 10, 9, 11, 12, 13, 14, 21, 22, 23, 24, 25)
              setNames(shapes[1:unique_conditions], unique(filtered_data$condition))
            }
          })
          
          
          # Dynamically assign colors to conditions
          condition_colors <- setNames(colors[1:length(unique_conditions)], unique_conditions)
          
          plot <- ggplot(filtered_data, aes(x = !!x_aes, y = !!y_aes, color = !!sym("condition"))) +
            geom_point(size = input$point_size, na.rm = TRUE, aes(shape = !!sym("condition")),
                       show.legend = TRUE, stroke = input$stroke_thickness, position = position_jitter(width = input$jitter_amount)) +
            geom_errorbar(data = grouped_dotplot_data_reactive_avg(),
                          aes(x = group, ymin = ymin, ymax = ymax),
                          width = input$error_bar_width, linewidth = input$error_bar_thickness, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE) +
            stat_summary(data = grouped_dotplot_data_reactive_avg(), aes(x = !!x_aes, y = !!y_aes_avg_group, fill = "black"), inherit.aes = FALSE,
                         fun = mean, geom = "crossbar", width = input$average_line_width, linewidth = input$average_line_thickness, show.legend = FALSE, na.rm = TRUE) +  # Add average line for each column x
            labs(y = input$y_label, x = input$x_label, shape = input$legend_title, color = input$legend_title) +
            scale_color_manual(values = condition_colors, labels = user_legend_labels()) +
            scale_fill_manual(values = "black", guide = "none") +  # Explicitly remove the fill legend
            scale_shape_manual(values = shapes_reactive(), labels = user_legend_labels()) +
            theme_Marnie +
            scale_x_discrete(limits = processed_positions(), labels = user_labels()) +
            x_axis_theme +
              theme(
                legend.position = if(input$legend_position == "none") "none" else input$legend_position,
                legend.title = element_text(size = input$legend_text_size_title),
                legend.text = element_text(size = input$legend_text_size)
              )
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
        # Enable automatic font rendering via showtext
        showtext::showtext_auto()
        
        plot <- plot + theme(text = element_text(family = input$font_selector))
        
        #allow markdown on y and x axis
        plot <- plot + theme(axis.title.y = element_markdown())
        plot <- plot + theme(axis.title.x = element_markdown())
        
        observe({
          req(input$add_significance != "none")
          if (is.null(group_comparison()) || group_comparison() == "" && input$add_significance != "none") {
            showModal(modalDialog(
              title = "Statistics Error",
              "Statistics have not been performed: Please select a group comparison test in the statistics tab before adding significance to the graph.",
              easyClose = TRUE,
              footer = NULL
            ))
            return(NULL)
          }
        })
        
        max_y <- max(filtered_data[[y_aes]], na.rm = TRUE)
        y_position_auto <- max_y + (0.1 * max_y)
        
        if(input$add_significance == "asterix"){
          num_groups <- length(unique(shapiro_data_reactive()$cell))
          if(num_groups > 2){
            req(comparisonResults()$posthoc)
            comparisonResults_posthoc_renamed <- comparisonResults()$posthoc %>%
              rename(p.signif = `P Value Summary`)
            plot <- plot + stat_pvalue_manual(comparisonResults_posthoc_renamed, label = "p.signif", y.position = y_position_auto, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                              step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
          }else{
            req(comparisonResults()$test)
            comparisonResults_renamed <- comparisonResults()$test %>%
              rename(p.signif = `P Value Summary`)
            plot <- plot + stat_pvalue_manual(comparisonResults_renamed, label = "p.signif", y.position = y_position_auto, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                              step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
          }
          
        }else if(input$add_significance == "cld"){
          req(comparisonResults()$cld)
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
            req(comparisonResults()$posthoc)
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
                if (isTRUE(input$remove0)) {
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
            req(comparisonResults()$test)
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
              if (isTRUE(input$remove0)) {
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
      }, res = 96,  
      width = function() {
        input$width * 96  # Adjust the multiplier as needed
      },
      height = function() {
        input$height * 96  # Adjust the multiplier as needed
        
      })


    #save gene name for naming of saved graph file
    selected_gene_name <- reactive({
      # Check if 'ddcq' is selected
      if (input$select_dcq_or_ddcq == "ddcq") {
        # Clean the gene name for ddcq
        clean_gene_name <- gsub("^dcq_", "", ddcq_selected_gene())
      } else {
        # For 'dcq' and other conditions, return the selected column from `fc_dcq_column`
        clean_gene_name <- gsub("^fc_dcq_", "", input$fc_dcq_column)
      }
      return(clean_gene_name)
    })
    
    #download graph with dynamic names, formats etc
    output$downloadGraph <- downloadHandler(
      filename = function() {
        # Call the reactive expression to get the current gene name
        current_gene <- selected_gene_name()
        
        # Ensure there's a default value for the gene name
        gene_name <- ifelse(is.null(current_gene) || current_gene == "", "Gene", current_gene)
        
        # Generate the filename
        paste("Graph_", gene_name, "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".", input$file_format, sep = "")
      },
      content = function(file) {
        # Save the plot 
        set.seed(input$seed_input)
        # Get the last plot and apply the selected font family
        last_plot <- last_plot() + 
          theme(text = element_text(family = input$font_selector))  # Apply selected font family
        
        # Ensure font rendering is handled for downloads
        showtext::showtext_auto()
        
        ggsave(file, plot = last_plot, device = input$file_format, dpi = input$dpi, width = input$width, height = input$height)
      }
    )
    
    #download graph options with dynamic names, formats etc
    output$downloadGraphOptions <- downloadHandler(
      filename = function() {
        # Generate the filename
        paste("GraphOptions-", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".html", sep = "")
      },
      content = function(file) {
        # Specify the path to Graphs Report R Markdown template
        rmdTemplate <- get_rmd_template_path("GraphOptions.Rmd")
        #Render the Rmd file, passing the sample size table data as a parameter
        rmarkdown::render(input = rmdTemplate,
                          output_file = file,
                          params = list(
                            rotateXLabels = input$rotate_labels,
                            yAxisLabel = input$y_label,
                            xAxisLabel = input$x_label,
                            font = input$font_selector,
                            xAxisTitleFontSize = input$x_axis_title_font_size,
                            yAxisTitleFontSize = input$y_axis_title_font_size,
                            xAxisTextFontSize = input$x_axis_label_font_size,
                            yAxisTextFontSize = input$y_axis_label_font_size,
                            colourScheme = input$color_scheme_select,
                            plotType = input$plot_type,
                            significance = input$add_significance,
                            labelSize = input$sigSize,
                            bracketThickness = input$bracketSize,
                            tipLength = input$tipLength,
                            stepIncrease = input$stepIncrease,
                            hideNS = input$hideNS,
                            pValuePrefix = input$pValuePrefix,
                            pValueDecimals = input$pValueDecimals,
                            removeLeadingZero = input$remove0,
                            errorBarType = input$error_type,
                            errorBarColour = input$match_colour_error,
                            startAtZero = input$start_at_zero,
                            errorBarWidth = input$errorbar_width,
                            errorBarThickness = input$errorbar_thickness,
                            errorBarWidthdot = input$error_bar_width,
                            errorBarThicknessdot = input$error_bar_thickness,
                            averageLineWidth = input$average_line_width,
                            averageLineThickness = input$average_line_thickness,
                            averageLineColour = input$match_colour_avg,
                            fillOrBorder = input$fill_color_toggle,
                            pointSize = input$dot_size,
                            pointSizedot = input$point_size,
                            pointSpacing = input$dot_spacing,
                            changeToPairedShapes = input$change_shapes,
                            shapeOutlineThickness = input$stroke_thickness,
                            pointSpread = input$jitter_amount,
                            seedNumber = input$seed_input,
                            fileFormat = input$file_format,
                            dpi = input$dpi,
                            width = input$width,
                            height = input$height),
                          envir = new.env(parent = globalenv())
                          )
      }
    )
    
  })
}