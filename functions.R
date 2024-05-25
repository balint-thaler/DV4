
# ggplot function: 
plot_ggplot <- function(df) {
  p <- ggplot(df, aes(y = reorder(country, CL), x = CL)) +
    geom_col() + theme_bw() +
    ylab("") +
    xlab("Civil liberties score")
    
  return(p)
}


# plotly function:
plot_plotly <- function(df) {
  p <- plot_ly(df, x = ~CL, y = ~reorder(country, CL), type = 'bar') %>%
    layout(yaxis = list(title = ""),
           xaxis = list(title = "Civil liberties score"),
           barmode = 'group')
  
  return(p)
}

# dataset function
get_data <- function() {
  full_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
  full_data <- data.frame(full_data)
  return(full_data)

}

# filtering data function
get_filtered_data <- function(region_name, year, country, min_score, max_score) {
  full_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
  full_data <- data.frame(full_data)
  filtered_data <- full_data[full_data$Region_Name == toString(region_name) & full_data$year == year & full_data$country %in% country & full_data$CL <= max_score & full_data$CL >= min_score, ]
  return(filtered_data)
}

