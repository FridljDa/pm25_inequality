#plot
get_group_colors <- function(){
  group.colors <- c(
    RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6, 8:10, 12)],
    RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2],
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:3]  # Adding three new colors from "Dark2"
  )

  group.colors[c(12, 2)] <- group.colors[c(2, 12)]
  names(group.colors) <- c(
    "NH White",
    "Hispanic or Latino White",
    "Black American",
    "White",
    "Asian or Pacific Islander",
    "American Indian or Alaska Native",
    "High school graduate or lower",
    "Some college education but no 4-year college degree",
    "4-year college graduate or higher",
    "Non metro",
    "Large metro",
    "Small-medium metro",
    "high SVI",
    "middle SVI",
    "low SVI"#TODO svi
  )

  return(group.colors)
}
