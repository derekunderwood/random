library(reactable)

x <- data.frame(
  group = sample(c("a","b","c"), 200, replace = T),
  group_sub = sample(1:3, 200, replace = T),
  group_sub_sub = sample(c("x","y","z"), 200, replace = T),
  weight_to_hide = runif(200, 1000, 20000),
  return = runif(200, 0, 0.25)
  )

weighted_mean <-
  function(weight) {
    JS(
      paste0(
        "function(values, rows) {
          var numerator = 0
          var denominator = 0
          rows.forEach(function(row, index) {
            numerator += row['", weight, "'] * values[index]
            denominator += row['", weight, "']
          })
    
          return (numerator / denominator || 0)
  
        }"
      )
    )
  }

reactable(
  x,
  groupBy = c("group",
              "group_sub",
              "group_sub_sub"),
  columns = list(
    return = colDef(
      aggregate = weighted_mean(weight = "weight_to_hide"),
      format = colFormat(percent = TRUE, digits = 2)
    ),
    weight_to_hide = colDef(
      aggregate = "sum",
      show = FALSE,
      format = colFormat(currency = "USD", separators = T)
    )
  )
)
