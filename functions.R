
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
  p <- plot_ly(df, x = ~PR, y = ~reorder(country, PR), type = 'bar') %>%
    layout(yaxis = list(title = ""),
           xaxis = list(title = "Political rights score"),
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
get_filtered_data <- function(region_name, year) {
  full_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
  full_data <- data.frame(full_data)
  filtered_data <- full_data[full_data$Region_Name == toString(region_name) & full_data$year == year, ]
  return(filtered_data)
}

