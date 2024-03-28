library(shinytest2)

test_that("{shinytest2} recording: R", {
  app <- AppDriver$new(name = "R", height = 559, width = 979)
  app$expect_values()
  app$set_inputs(tabselected = "2")
  rlang::warn(paste0("`file` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(file = "exampledata.csv")
  app$set_inputs(table_rows_current = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(table_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
      15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 
      34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 
      53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 
      72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 
      91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 
      108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 
      123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 
      138, 139, 140, 141, 142, 143, 144), allow_no_input_binding_ = TRUE)
  app$set_inputs(table_state = c(1711601699871, 0, 5, "", TRUE, FALSE, TRUE, c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(group1 = "PPIA")
  app$set_inputs(group2 = "B2M")
  app$set_inputs(group3 = "GAPDH")
  app$click("save_btn")
  app$set_inputs(tabselected = "3")
  app$set_inputs(calculations_table_rows_current = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(calculations_table_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
      12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25), allow_no_input_binding_ = TRUE)
  app$set_inputs(calculations_table_state = c(1711601718411, 0, 5, "", TRUE, FALSE, 
      TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
          "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, 
          FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(filtered_table_rows_current = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(filtered_table_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(filtered_table_state = c(1711601718462, 0, 5, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_download("download_processed_data-download")
  app$expect_values()
  app$set_inputs(tabselected = "4")
  app$set_inputs(tabselected = "3")
  app$set_inputs(subCalc = "2")
  app$set_inputs(rep_avg_filtered_table_rows_current = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(rep_avg_filtered_table_rows_all = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(rep_avg_filtered_table_state = c(1711601738196, 0, 5, "", TRUE, 
      FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(rep_avg_table_rows_current = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(rep_avg_table_rows_all = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(rep_avg_table_state = c(1711601738218, 0, 5, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), 
      allow_no_input_binding_ = TRUE)
  app$expect_download("download_rep_avg_data-download")
  app$expect_values()
  app$set_inputs(tabselected = "4")
  app$set_inputs(subStats = "2")
  app$set_inputs(sampleInput = "N_sample1")
  app$set_inputs(sampleInput = c("N_sample1", "SC_sample1"))
  app$set_inputs(sample_size = TRUE)
  app$set_inputs(nTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(nTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(nTable_state = c(1711601785947, 0, 5, "", TRUE, FALSE, TRUE, c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
      TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(normality_test = "shapiro")
  app$set_inputs(normalityTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_state = c(1711601786815, 0, 10, "", TRUE, FALSE, 
      TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
          "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, 
          FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(normality_test = c("shapiro", "qqplot"))
  app$set_inputs(normalityTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_state = c(1711601787430, 0, 10, "", TRUE, FALSE, 
      TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
          "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, 
          FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(normality_test = c("shapiro", "qqplot", "density"))
  app$set_inputs(normalityTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_state = c(1711601788215, 0, 10, "", TRUE, FALSE, 
      TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
          "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, 
          FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$set_inputs(variance = TRUE)
  app$set_inputs(levene_rows_current = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(levene_rows_all = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(levene_state = c(1711601792427, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(log_transform = TRUE)
  app$set_inputs(normalityTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(normalityTable_state = c(1711601794240, 0, 10, "", TRUE, FALSE, 
      TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
          "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, 
          FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(levene_rows_current = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(levene_rows_all = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(levene_state = c(1711601794259, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(group_comparison = "parametric")
  app$set_inputs(cld_table_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(cld_table_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(cld_table_state = c(1711601796074, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(dataTable_rows_current = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(dataTable_rows_all = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(dataTable_state = c(1711601796080, 0, 5, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_download("downloadStats")
  app$expect_values()
  app$set_inputs(tabselected = "5")
  app$set_inputs(selected_condition = "N_sample1")
  app$set_inputs(selected_condition = c("N_sample1", "SC_sample1"))
  app$set_inputs(x_axis_positions = "N")
  app$set_inputs(x_axis_positions = "N_sampl")
  app$set_inputs(x_axis_positions = "N_sample")
  app$set_inputs(x_axis_positions = "N_sample1")
  app$set_inputs(x_axis_positions = "N_sample1,")
  app$set_inputs(x_axis_positions = "N_sample1,N")
  app$set_inputs(x_axis_positions = "N_sample1,N_")
  app$set_inputs(x_axis_positions = "N_sample1,")
  app$set_inputs(x_axis_positions = "N_sample1,SC_sample")
  app$set_inputs(x_axis_positions = "N_sample1,SC_sample1")
  app$set_inputs(rotate_labels = TRUE)
  app$set_inputs(add_significance = "asterix")
  app$set_inputs(fill_color_toggle = "color")
  app$set_inputs(width = 7)
  app$set_inputs(width = 6)
  app$set_inputs(height = 4)
  app$set_inputs(height = 3)
  app$set_inputs(height = 2)
  app$set_inputs(height = 3)
  app$set_inputs(height = 4)
  app$set_inputs(width = 5)
  app$set_inputs(width = 4)
  app$expect_values()
  app$expect_download("downloadGraph")
  app$expect_values()
})
