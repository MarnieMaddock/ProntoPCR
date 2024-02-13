#utils_graphPanel.R
graphPanel <- function() {
  tagList(
    h5(HTML("<b>Create Graph</b>")),
    fluidRow(
      column(width = 6, uiOutput("condition_selector")),
      column(width = 6, uiOutput("column_selector"))
    ),
    textInput("x_axis_positions", 
              tagList(
                tags$label("Enter the order to display x-axis groups (comma-separated):"),
                helpText("Ensure spelling is exactly as it is entered in the Group column. Do NOT use spaces. e.g., untreated,treated")
              )
    ),
    h5(HTML("<b>Customise Graph</b>")),
    # Add textInputs for custom Y-axis and X-axis labels
    textInput("y_label", "Enter Y-axis Label", value = "Relative *GENE NAME* mRNA (2^-ΔCq)"),
    textInput("x_label", "Enter X-axis Label", value = "Group"),
    selectInput("font_selector", "Select Font", choices = c("Arial", "Times New Roman", "Helvetica", "Georgia", "Comic Sans MS", "Century Gothic",  "Courier New")),
    fluidRow(
      column(
        width = 6,
        numericInput("x_axis_title_font_size", "X-axis Title Font Size:", value = 14, min = 1, max = 50),
        numericInput("x_axis_label_font_size", "X-axis Text Font Size:", value = 12, min = 1, max = 50)
      ),
      column(
        width = 6,
        numericInput("y_axis_title_font_size", "Y-axis Title Font Size:", value = 14, min = 1, max = 50),
        numericInput("y_axis_label_font_size", "Y-axis Text Font Size:", value = 12, min = 1, max = 50)
      )
    ),
    tags$br(),
    uiOutput("x_axis_labels"),
    # Checkbox for label rotation
    checkboxInput("rotate_labels", "Rotate x-axis labels", value = FALSE),
    selectInput("plot_type", "Choose Plot Type:",
                choices = c("Column Graph" = "column", "Dot Plot" = "dot"),
                selected = "column"),
    selectInput("color_scheme_select", "Choose Colour Scheme:",
                choices = c("Custom" = "custom", "Colourblind friendly 1" = "colourblind1", 
                            "Colourblind friendly 2" = "colourblind2", "Colourblind friendly 3" = "colourblind3",
                            "Colourblind friendly 4" = "colourblind4", "Grays 1" = "grays", "Grays 2" = "grays2",
                            "Grays 3" = "grays3", "ElectraGray" = "electraGray", "Bones" = "bones", 
                            "Oranges 1" = "oranges", "Oranges 2" = "oranges2", "Pinks 1" = "pinks", 
                            "Pinks 2" = "pinks2", "Blues 1" = "blues", "Blues 2" = "blues2", "Greens 1" = "greens",
                            "Greens 2" = "greens2", "Greens 3" = "greens3", "Green to Purple" = "green2purple",
                            "Purples 1" = "purples", "Purples 2" = "purples3", "Purple to Orange" = "purple2orange",
                            "Blaze" = "blaze", "Blaze 2" = "blaze2", "Peace 1" = "peace", "Peace 2" = "peace2", "Ireland" = "ireland",
                            "Two-tone 1" = "twotone1", "Two-tone 2" = "twotone2", "Two-tone 3" = "twotone3",
                            "Pastels 1" = "pastels", "Pastels 2" = "pastels2", "Pastels 3" = "pastels3", "Pastels 4" = "pastels4",
                            "Pastels 5" = "pastels5", "Pastels 6" = "pastels6", "Pastels 7" = "pastels7", "Vibrant 1" = "vibrant", 
                            "Vibrant 2" = "vibrant2", "Vibrant 3" = "vibrant3"),
                selected = "custom"),
    numericInput("dot_size", "Point Size:", value = 1.5, min = 1, max = 10, step = 0.5),
    # Add a dropdown menu for font selection
    conditionalPanel(
      condition = "input.plot_type == 'column'",
      # Choose fill or colour
      selectInput("fill_color_toggle", "Choose Fill or Border:",
                  choices = c("Fill" = "fill", "Border" = "color"),
                  selected = "fill"))
  )
}
