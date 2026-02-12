# Custom theming to use across all my plots

library(ggplot2)
library(showtext)
library(knitr)
library(kableExtra)

theme_pub <- function(base_size = 10,
                      base_family = "Times New Roman",
                      line_size = 0.3,
                      grid = c("major", "minor")) {
  
  grid <- match.arg(grid, several.ok = TRUE)
  
  show_major <- "major" %in% grid
  show_minor <- "minor" %in% grid
  
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      
      # Text
      text = element_text(
        size = base_size,
        family = base_family,
        color = "black"
      ),
      
      # Titles
      plot.title = element_text(
        size = base_size * 1.2,
        face = "bold",
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = base_size,
        margin = margin(b = 6)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        color = "grey30",
        hjust = 1,
        margin = margin(t = 6)
      ),
      
      # Axes
      axis.title = element_text(
        size = base_size,
        face = "plain"
      ),
      axis.text = element_text(
        size = base_size * 0.9
      ),
      axis.title.y = element_text(
        margin = margin(r = 8)
      ),
      axis.title.x = element_text(
        margin = margin(t = 8)
      ),
      axis.ticks = element_line(color = "grey40", linewidth = line_size),
      
      # Remove minor ticks
      axis.ticks.length = unit(3, "pt"),

      # Panel and grid
      panel.grid.major = if (show_major)
        element_line(color = "grey85", linewidth = line_size)
      else element_blank(),
      
      panel.grid.minor = if (show_minor)
        element_line(color = "grey90", linewidth = line_size)
      else element_blank(),
      
      panel.background = element_rect(fill = "white", color = NA),
      
      # Legend
      legend.title = element_text(
        size = base_size * 0.95,
        face = "bold"
      ),
      legend.text = element_text(size = base_size * 0.9),
      legend.key = element_rect(fill = NA, color = NA),
      legend.position = "right",
      
      # Strips
      strip.text = element_text(
        size = base_size * 0.95,
        face = "bold",
        margin = margin(4, 4, 4, 4)
      ),
      strip.background = element_rect(fill = "grey90", color = "grey70", linewidth = line_size),
      
      # Margins
      plot.margin = margin(10, 10, 10, 10)
    )
}
# theme_set(theme_pub())



# Custom theming for tables
theme_table_pub <- function(kable_input,
                            font_size = 10,
                            font_family = "Times New Roman",
                            stripe_color = "#f5f5f5",
                            full_width = FALSE,
                            position = "center") {
  
  kable_input %>%
    kable_styling(
      font_size = font_size,
      latex_options = c("hold_position"),
      full_width = full_width,
      position = position
    ) %>%
    row_spec(0, bold = TRUE, font_size = font_size + 1) %>%
    kable_classic(full_width = full_width, font_size = font_size) %>%
    kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped"),
      stripe_color = stripe_color
    ) %>%
    kableExtra::kable_styling(
      html_font = font_family
    )
}
