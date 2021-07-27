#New Theme
library(lemon)

#theme_classic
theme_new <- function(base_size = 11, base_family = "",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) 
  {
    # Starts with theme_bw and remove most parts
    theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
     
    theme(
        # no background and no grid
        panel.background  = element_blank(),
        panel.border      = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        strip.background  = element_blank(),
        strip.text.x      = element_text(size = rel(1), colour = "black"),
        plot.background   = element_blank(),
        
        # no axes
        axis.line         = element_line(colour = "black"),
        axis.ticks        = element_line(colour = "black"),
        axis.text         = element_text(size = rel(0.9), colour = "black"),
        
        # match legend key to panel.background
        legend.background = element_blank(),
        legend.key        = element_blank(),
        
        complete = TRUE
      )
  }
