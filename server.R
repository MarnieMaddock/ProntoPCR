
# Define server logic required to draw a histogram
server <- function(input, output) {
  theme_Marnie <- theme(axis.line.y = element_line(colour = "black", linewidth = 0.9),
                        axis.line.x = element_line(colour = "black", linewidth = 0.9),
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "white"),
                        panel.border = element_blank(),
                        axis.title.x = element_text(size = 16, margin = margin(5,0,0,0)),
                        axis.title.y = element_text(size =16, margin = margin(0,10,0,0)),
                        axis.text = element_text(size = 16, colour = "black"),
                        axis.text.x = element_text(margin = margin(t=5), size=14),
                        axis.text.y = element_text(size=14),
                        plot.title = element_text(size = 32, hjust = 0), # legend.position = c(0.8, 0.8) 
                        legend.position  = "right",
                        legend.key.size = unit(0.4, "cm"),
                        legend.text = element_text(size = 12),
                        legend.text.align = 0,
                        legend.title = element_text(face = "bold", size = 14),
                        legend.title.align = 0.5,
                        legend.key.width = unit(0.4,"cm"),
                        #legend.title = element_blank(),
                        legend.key = element_rect(fill = NA, colour = NA),
                        strip.text = element_text(size = 16, face = "bold"),
                        strip.background = element_rect(colour = "black"),
                        panel.spacing = unit(0, "lines")
  )
  # Reactive values to store housekeeper names and numeric input value
  housekeepers_names <- reactiveValues()
  
  # Reactive function to read and display the uploaded CSV file
  data <- reactive({
    req(input$file)
    
    # Read the CSV file
    df <- read.csv(input$file$datapath)
    
    return(df)
  })
  
  # Display the table using DataTable
  output$table <- renderDataTable({
    data()
  })
  
  # Generate dynamic text input fields based on the number of groups
  output$groups <- renderUI({
    housekeepers <- as.integer(input$housekeepers)
    lapply(
      1:housekeepers, 
      function(i){
        textInput(
          paste0("group", i), 
          paste0("Enter the name of housekeeper ", i)
        )
      }
    )
  })
  
  # Save the text inputs as variables when the button is clicked
  saved_variables <- reactiveValues()
  observeEvent(input$save_btn, {
    housekeepers <- as.integer(input$housekeepers)
    saved_variables$names <- sapply(1:housekeepers, function(i) input[[paste0("group", i)]])
  })
  
  # Generate the output text
  output$text1 <- renderText({
    if (is.null(input$housekeepers)) {
      return()
    }
    housekeepers <- as.integer(input$housekeepers)
    variables <- saved_variables$names
    paste0(
      "You have ", housekeepers, " housekeeper genes. ",
      "The housekeepers are called ", paste(variables, collapse = ", "), "."
    )
  })
  
  
  output$text2 <- renderText({
    paste("Once you have saved the housekeeper gene names, please move to the calculations tab.")
  })
  
  
  # New reactive expression for data wrangling
  wrangled_data <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    
    # Remove Cq.SD and Quality issues columns
    df <- data()[, c(1, 2, 4)]
    
    # Remove NTC
    df <- df[!grepl('NTC', df$Sample), ]
    
    #make the dataframe longer (with targets as columns)
    df <- df[,1:3] %>% pivot_wider(names_from = Target, values_from = Cq.Mean)
    
    # Retrieve the saved variables
    variables <- saved_variables$names
    
    #Average the housekeepers
    df <- df %>% 
      rowwise() %>% 
      mutate(
        mean_hk = mean(c_across(all_of(variables)), na.rm = TRUE),
      )
    #move columns
    df <- data_relocate(df, select = "mean_hk", after = "Sample")
    df <- data_relocate(df, select = saved_variables$names, after = "Sample")
    
    #Calculate delta Cq for each target
    #which(names(data) == "mean_hk") finds the column index of "mean_hk" in the dataframe.
    #+ 1 increments the index to select the columns directly after "mean_hk".
    #ncol(data) provides the last column index of the dataframe.
    df <- df %>% 
      mutate(across((which(names(df) == "mean_hk") + 1):ncol(df), 
                    list(dct = ~ ifelse(.x != 0, .x - mean_hk, 0)), 
                    .names = "{.fn}_{.col}"))
    
    #calulcate fold change (relative mRNA)
    # Calculate fc, considering the case where the data point is 0
    df <- df %>% 
      mutate(across(
        (which(startsWith(names(df), "dct_"))):ncol(df),
        list(fc = ~ ifelse(.x != 0, 2^(-.x), 0)),
        .names = "{.fn}_{.col}"
      ))
    
    
    
    # Make a new column that places each sample as the specified cell type
    df$cell_type <- gsub(".*_(\\w+)$", "\\1", df$Sample)
    df$cell_type <- as.factor(df$cell_type)
    #Add cell_line data
    df$cell_line <- sub("^([A-Za-z0-9]+)_.*", "\\1", df$Sample)
    df$cell_line <- as.factor(df$cell_line)
    #add combined
    df$cell <- paste(df$cell_type, df$cell_line, sep = "_")
    df$cell <- as.factor(df$cell)
    #Move column
    df <- data_relocate(df, select = "cell_line", after = "Sample")
    df <- data_relocate(df, select = "cell_type", after = "Sample")
    df <- data_relocate(df, select = "cell", after = "cell_line")
    
    return(df)
  })
  
  # Display the table using DataTable in "Calculations" tab
  output$calculations_table <- renderDataTable({
    req(wrangled_data())
    wrangled_data()
  })
  
  # Add this inside your server function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_PCR_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(wrangled_data(), file, row.names = FALSE)
    }
  )
  
  output$cell_type_filter <- renderUI({
    req(wrangled_data())  # Ensure data is available
    cell_types <- unique(wrangled_data()$cell_type)
    
    selectInput("cell_type", "Select Cell Type", choices = cell_types)
  })
  
  
  
  filtered_data <- reactive({
    req(wrangled_data())
    cell_types_to_filter <- input$cell_type
    
    if (!is.null(cell_types_to_filter)) {
      filtered_data <- wrangled_data() %>%
        filter(cell_type %in% cell_types_to_filter)
      return(filtered_data)
    } else {
      return(NULL)
    }
  })
  
  output$filtered_table <- renderDataTable({
    req(filtered_data())
    filtered_data()
  })
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      cell_type <- input$cell_type
      if (!is.null(cell_type)) {
        paste("processed_PCR_data_", cell_type, "_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("processed_PCR_data_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Calculate replicate averages when data is loaded
  
  rep_avg_data <- reactive({
    req(wrangled_data())
    
    vars <- colnames(wrangled_data()) %>%
      grep("^fc_dct", ., value = TRUE)
    
    # Now, the 'vars' object contains the desired column names
    rep_avg <- wrangled_data() %>%
      group_by(cell_type, cell_line) %>%
      summarise_at(vars, list(fc_avg = ~mean(., na.rm = TRUE))) %>%
      gather(key = "Variable", value = "fc_avg", -cell_type, -cell_line)
    
    rep_avg <- rep_avg %>% 
      pivot_wider(names_from = Variable, values_from = fc_avg)
    
    # Remove "_fc_avg" from column names
    colnames(rep_avg) <- sub("_fc_avg$", "", colnames(rep_avg))
    
    #add column cell
    rep_avg$cell <- paste(rep_avg$cell_type, rep_avg$cell_line, sep = "_")
    rep_avg$cell <- as.factor(rep_avg$cell)
    #Move column
    rep_avg <- data_relocate(rep_avg, select = "cell", after = "cell_line")
    
    return(rep_avg)
  })
  
  
  # Display the replicate averages table in "Calculations" tab
  output$rep_avg_table <- renderDataTable({
    rep_avg_data()
  })
  
  
  output$rep_avg_download <- downloadHandler(
    filename = function() {
      paste("Replicate_avg_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rep_avg_data(), file, row.names = FALSE)
    }
  )
  
  output$rep_avg_filtered_table <- renderDataTable({
    req(filtered_rep_avg_data())
    filtered_rep_avg_data()
  })
  
  filtered_rep_avg_data <- reactive({
    req(rep_avg_data())
    cell_types_to_filter <- input$cell_type
    
    if (!is.null(cell_types_to_filter)) {
      filtered_data <- rep_avg_data() %>%
        filter(cell_type %in% cell_types_to_filter)
      return(filtered_data)
    } else {
      return(NULL)
    }
  })
  
  output$rep_avg_filtered_download <- downloadHandler(
    filename = function() {
      cell_type <- input$cell_type
      if (!is.null(cell_type)) {
        paste("Replicate_avg_data_", cell_type, "_filtered_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("Replicate_avg_data_filtered_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_rep_avg_data(), file, row.names = FALSE)
    }
  )
  
  
  #GRAPHING
  # Render the dynamic selectInput for choosing cell type
  output$cell_type_selector <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    # Generate selectInput for choosing the cell type dynamically
    selectInput("selected_cell_type", "Select Cell Type to Graph", choices = unique(wrangled_data()$cell_type),
                multiple = TRUE)
  })
  
  output$column_selector <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    # Filter column names to include only those starting with "fc_dct"
    fc_dct_columns <- grep("^fc_dct", colnames(wrangled_data()), value = TRUE)
    
    # Generate selectInput for choosing the column dynamically
    selectInput("column", "Select Gene to Graph", choices = fc_dct_columns)
  })
  
  
  # Get the color scheme based on user input
  color_schemes <- reactiveValues(
    colourblind1 = c("#00359c", "#648fff", "#785ef0", "#dc267f", "#fe6100", "#ffb000"),
    colourblind2 = c("#ffbd00", "#ff5400", "#ff0054", "#9e0059", "#390099"),
    colourblind3 = c("#70d6ff", "#ff70a6", "#ff9770", "#ffd670", "#e9ff70"),
    colourblind4 = c("gray20", "#ff0166ff", "#117f80ff", "#40007fff", "#785ef0","#66ccfeff" ),
    grays = c("gray10", "gray30", "gray50", "gray70", "gray80", "gray100"),
    grays2 = c("#f8f9fa", "#e9ecef", "#dee2e6", "#ced4da", "#adb5bd", "#6c757d", "#495057", "#343a40", "#212529"),
    grays3 = c("#2b2d42", "#8d99ae", "#edf2f4"),
    electraGray = c("#e00154", "#222337", "#e6e1dd", "#b4a8b4", "#ddd2cf"),
    bones = c("#edede9", "#d6ccc2", "#f5ebe0", "#e3d5ca", "#d5bdaf"),
    oranges = c("#ffc971", "#ffb627", "#ff9505", "#e2711d" ,"#cc5803"),
    oranges2 = c("#ff4800", "#ff5400", "#ff6000", "#ff6d00", "#ff7900", "#ff8500", "#ff9100", "#ff9e00", "#ffaa00", "#ffb600"),
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
    blaze2 = c("#14213d", "#fca311", "#e5e5e5", "#ffffff"), 
    peace = c("#2e58a4ff", "#b69e71ff", "#e3ded4ff", "#71aec7ff","#4f5357ff"),
    peace2 = c("#797d62", "#9b9b7a", "#d9ae94", "#f1dca7", "#ffcb69","#d08c60", "#997b66") ,
    ireland = c("#ff9f1c", "#ffbf69", "#ffffff", "#cbf3f0", "#2ec4b6"),
    pastels = c("#ddfff7", "#93e1d8", "#ffa69e"),
    pastels2 = c("#90f1ef", "#ffd6e0", "#ffef9f"),
    pastels3 = c("#ffffff", "#ffcad4", "#b0d0d3" ),
    pastels4 = c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff"),
    pastels5 = c("#ccd5ae", "#e9edc9", "#fefae0", "#faedcd", "#d4a373"),
    pastels6 = c("#ffadad", "#ffd6a5", "#fdffb6", "#caffbf", "#9bf6ff", "#a0c4ff", "#bdb2ff", "#ffc6ff", "#fffffc"),
    pastels7 = c("#809bce", "#95b8d1","#b8e0d2", "#d6eadf", "#eac4d5"),
    vibrant = c("#ff0f7b", "#f89b29" ),
    vibrant2 = c("#10e0ff", "#0086eb", "#006ee9", "#ffcd00", "#ffef00"),
    vibrant3 = c("#ff00c1", "#9600ff", "#4900ff", "#00b8ff", "#00fff9"),
    custom = c("#330051","#4F007D","#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de", "#56cfe1","#64dfdf", "#72efdd", "#80ffdb", "#B7D7B9", "#D2C3A8", "#E0B9A0","#EDAF97","#C49792", "#AD91A3", "#9D91A3")
  )
  
  # Dynamic generation of text inputs based on positions
  output$x_axis_labels <- renderUI({
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
  
  # Reactive function to get user-entered labels
  user_labels <- reactive({
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
  
  # Reactive function for ggplot
  output$plot <- renderPlot({
    req(wrangled_data(), input$column, input$selected_cell_type, input$y_label, input$x_label)
    set.seed(input$seed_input)
    
    # Specify the y_aes based on user input
    y_aes <- sym(input$column)
    
    # Filter data based on the selected cell type
    filtered_data2 <- wrangled_data() %>%
      filter(cell_type %in% input$selected_cell_type)
    
    # Filter rep_avg_data based on the selected cell type
    filtered_rep_avg_data2 <- rep_avg_data() %>%
      filter(cell_type %in% input$selected_cell_type)
    
    # Determine the x aesthetic based on the number of selected cell types
    x_aes <- if (length(input$selected_cell_type) >= 2) {
      sym("cell")
    } else {
      sym("cell_line")
    }
    
    
    
    positions <- if (length(input$selected_cell_type) >= 2) {
      # Parse positions if user entered them, or use unique values from "cell" column
      if (!is.null(input$x_axis_positions)) {
        unlist(strsplit(input$x_axis_positions, ","))
      } else {
        unique(filtered_data2$cell)
      }
    } else {
      unlist(strsplit(input$x_axis_positions, ","))
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
    
    
    
    # Define x-axis theme based on checkbox
    x_axis_theme <- if (input$rotate_labels) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    }
    
    
    
    # Create ggplot with customizations
    # Render the plot
    if (input$plot_type == "column"){
      if (input$fill_color_toggle == "color"){
        plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
          geom_bar(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes, color = !!x_aes), stat = "identity", inherit.aes = FALSE, fill = "white", size = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(color = !!x_aes), linewidth = 0.7, na.rm = TRUE,  show.legend = FALSE) +
          geom_beeswarm(size = input$dot_size, method = "hex", cex = 2.7, na.rm = TRUE, aes(color = !!x_aes),  show.legend = FALSE) +
          #geom_hline(yintercept = 0, size = 1) +
          labs(y = input$y_label, x = input$x_label) +
          scale_color_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
          theme_Marnie +
          scale_y_continuous(expand=expand_scale(mult=c(0,0.1)), limits = c(0,NA)) +
          scale_x_discrete(limits = positions, labels = user_labels()) +
          x_axis_theme 
        
        
      }else if (input$fill_color_toggle == "fill"){
        plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
          geom_bar(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes, fill = !!x_aes), stat = "identity", inherit.aes = FALSE, color = "black", size = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black", linewidth = 0.7, na.rm = TRUE,  show.legend = FALSE) +
          geom_beeswarm(size = input$dot_size, method = "hex", cex = 2.7, na.rm = TRUE, aes(fill = !!x_aes),  show.legend = FALSE) +
          #geom_hline(yintercept = 0, size = 1) +
          labs(y = input$y_label, x = input$x_label) +
          scale_fill_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
          theme_Marnie +
          scale_y_continuous(expand=expand_scale(mult=c(0,0.1)), limits = c(0,NA)) +
          scale_x_discrete(limits = positions, labels = user_labels()) +
          x_axis_theme
      }
    }else if (input$plot_type == "dot") {
      # Dot plot
      plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
        geom_point(size = input$dot_size, na.rm = TRUE, aes(color = !!x_aes, shape = !!x_aes),
                   show.legend = FALSE, position = position_jitter(width = input$jitter_amount)) +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, colour = "black", linewidth = 1.5, na.rm = TRUE,  show.legend = FALSE) +
        stat_summary(data = filtered_rep_avg_data2, 
                     fun = mean, geom = "crossbar", width = 0.15, linewidth = 1, show.legend = FALSE, colour = "black") +  # Add average line for each column x
        labs(y = input$y_label, x = input$x_label) +
        scale_color_manual(values = setNames(colors, positions)) +
        scale_shape_manual(values = setNames(c(16, 17, 15, 18, 4, 3, 8, 10, 9, 11, 12, 13, 14, 21, 22, 23), positions)) +
        theme_Marnie +
        scale_y_continuous(expand=expand_scale(mult=c(0,0.1)), limits = c(0,NA)) +
        scale_x_discrete(limits = positions, labels = user_labels()) +
        x_axis_theme
    }     
    
    
    # Set font based on user selection
    font_family <- input$font_selector
    plot <- plot + theme(text = element_text(family = font_family))
    
    
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
    shinyjs::runjs(paste0('$("#download-container").height($("#plot").height());'))
    # Print the plot
    print(plot)
  }, 
  width = function() {
    input$width * 100  # Adjust the multiplier as needed
  }, 
  height = function() {
    input$height * 100  # Adjust the multiplier as needed
    
  })
  
  # Add a download handler for the graph
  output$downloadGraph <- downloadHandler(
    filename = function() {
      paste("your_graph_filename", ".", input$file_format, sep = "")
    },
    content = function(file) {
      # Use the `ggsave` function to save the plot as an SVG file
      ggsave(file, plot = last_plot(), device = input$file_format, dpi = input$dpi, width = input$width, height = input$height)
    }
  )
  
  #DELTADELTA 
  #
  #
  #
  output$select_cell_type <- renderUI({
    req(wrangled_data())
    selectInput("select_cell_type", "Select Cell Type", choices = unique(wrangled_data()$cell_type))
  })
  
  output$select_control <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    selectInput("select_control", "Select the control/untreated sample", choices = unique(wrangled_data()$cell_line))
  })
  
  output$select_samples <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    selectInput("select_samples", "Select the diseased/treated sample(s)", choices = unique(wrangled_data()$cell_line), multiple = T)
  })
  
  output$column_selector2 <- renderUI({
    req(wrangled_data())
    
    # Filter column names to include only those starting with "dct"
    dct_columns <- grep("^dct_", colnames(wrangled_data()), value = TRUE)
    
    # Generate selectInput for choosing the column dynamically
    selectInput("select_gene", "Select Gene to calculate DDCT", choices = dct_columns)
  })
  
  ddct_filtered_data <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    
    cell_type2 <- input$select_cell_type
    control <- input$select_control
    samples <- input$select_samples
    selected_gene <- input$select_gene
    
    ddct_data <- wrangled_data() %>% 
      filter((cell_line == control) | (cell_line %in% samples)) %>% 
      filter(cell_type == cell_type2) %>%
      select(cell_line, cell_type, all_of(selected_gene))
    
    
    return(ddct_data)
  })
  
  output$ddct_data <- renderDataTable({
    req(ddct_filtered_data())
    ddct_filtered_data()
  })
  
  #DELTADELTA 
  #
  #
  #
  output$select_cell_type <- renderUI({
    req(wrangled_data())
    selectInput("select_cell_type", "Select Cell Type", choices = unique(wrangled_data()$cell_type))
  })
  
  output$select_control <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    selectInput("select_control", "Select the control/untreated sample", choices = unique(wrangled_data()$cell_line))
  })
  
  output$select_samples <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    selectInput("select_samples", "Select the diseased/treated sample(s)", choices = unique(wrangled_data()$cell_line), multiple = T)
  })
  
  output$column_selector2 <- renderUI({
    req(wrangled_data())
    
    # Filter column names to include only those starting with "dct"
    dct_columns <- grep("^dct_", colnames(wrangled_data()), value = TRUE)
    
    # Generate selectInput for choosing the column dynamically
    selectInput("select_gene", "Select Gene to calculate DDCT", choices = dct_columns)
  })
  
  ddct_filtered_data <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    
    cell_type2 <- input$select_cell_type
    control <- input$select_control
    samples <- input$select_samples
    selected_gene <- input$select_gene
    
    ddct_data <- wrangled_data() %>% 
      filter((cell_line == control) | (cell_line %in% samples)) %>% 
      filter(cell_type == cell_type2) %>%
      select(cell_line, cell_type, all_of(selected_gene))
    
    
    return(ddct_data)
  })
  
  
  
  
  mean_value <- reactiveVal(NULL)
  # Calculate the average delta ct for the selected gene in the control samples
  average_dct <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    req(input$select_control)
    
    cell_type3 <- input$select_cell_type
    selected_gene2 <- input$select_gene
    control2 <- input$select_control
    
    
    # Calculate the average delta ct for the selected gene in the control samples
    avg_dct_ctrl <- ddct_filtered_data() %>%
      filter(cell_line == control2) %>%
      group_by(cell_line, cell_type) %>%
      summarise(dct_ctrl_avg = mean(!!sym(selected_gene2), na.rm = TRUE), .groups = "drop")
    
    # Left join the original dataframe with the summarised dataframe
    avg_dct_ctrl <- left_join(ddct_filtered_data(), avg_dct_ctrl, by = c("cell_line", "cell_type"))
    
    # Calculate the mean value
    mean_val <- mean(avg_dct_ctrl$dct_ctrl_avg, na.rm = TRUE)
    
    # Store the mean value in the reactive value
    mean_value(mean_val)
    
    # Assign the mean value to the entire dct_ctrl_avg column
    avg_dct_ctrl$dct_ctrl_avg <- mean_val
    
    # Create a new column ddct by subtracting selected_gene2 from dct_ctrl_avg
    avg_dct_ctrl$ddct <- avg_dct_ctrl$dct_ctrl_avg - avg_dct_ctrl[[selected_gene2]]
    
    # Create a new column fc_ddct containing 2^(-ddct)
    avg_dct_ctrl$fc_ddct <- 2^(-avg_dct_ctrl$ddct)
    
    return(avg_dct_ctrl)
    
    
  })
  
  
  output$ddct_data <- renderDataTable({
    req(average_dct())
    average_dct()
  })
  
  
  #GRAPH DDCT,
  
  
}

# Run the application 