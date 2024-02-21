# utils_graphTheme.R
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