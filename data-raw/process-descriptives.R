library(readxl)
library(data.table)


descriptives <- (read_excel(path = "data-raw/descriptives.xlsx") |> 
  as.data.table())[1:272,-10L]

descriptives |> names() |> dput()

names(descriptives) <- 
  c("Sex", "Age", "SBP (mm hg)", "DBP (mm hg)", 
    "MAP (mm hg)", "PP (mm hg)", "BMI", "Weight (kg)", 
    "Height (cm)")

descriptives$id <- 1:nrow(descriptives)
descriptives[, Sex := factor(Sex, levels = 1:2, labels = c("Female", "Male"))]

desc_long <- descriptives |> 
  melt(id.vars = "id")

mean_sd <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) {
    return("—")
  }
  paste0(mean(x) |> round(2), " ± ",
         sd(x) |> round(2))
}

tbl_overall <- desc_long[
  j = list(Overall = mean_sd(value)), 
  by = variable]

desc_long_sex <- descriptives |> 
  melt(id.vars = c("id", "Sex"))

tbl_sex <- desc_long_sex[
  j = tapply(value, Sex, mean_sd, simplify = F), 
  by = variable]

tbl_full <- merge.data.table(tbl_overall, tbl_sex, by = "variable", all = T)

n_perc <- function(x, N) {
  n <- length(x)
  perc <- paste0("(",round(n * 100 / N, 1), "%)")
  paste(n, perc)
}

sex_var <- descriptives$Sex

tbl_full[
  i = variable == "Sex", 
  j = `:=`(
    Female = n_perc(x = sex_var[sex_var == "Female"], N = 272),
    Male = n_perc(x = sex_var[sex_var == "Male"], N = 272)
  )
]

tbl_full |> 
  knitr::kable(col.names = c("Characteristic", "Overall", "Female", "Male"))
#> |Characteristic |Overall        |Female         |Male           |
#> |:--------------|:--------------|:--------------|:--------------|
#> |Sex            |—              |217 (79.8%)    |55 (20.2%)     |
#> |Age            |71.14 ± 6.03   |70.73 ± 6.27   |72.73 ± 4.7    |
#> |SBP (mm hg)    |130.23 ± 17.07 |129.58 ± 17.37 |132.8 ± 15.69  |
#> |DBP (mm hg)    |77.1 ± 9.58    |76.68 ± 9.83   |78.75 ± 8.4    |
#> |MAP (mm hg)    |94.81 ± 10.69  |94.31 ± 10.95  |96.76 ± 9.45   |
#> |PP (mm hg)     |53.14 ± 14.07  |52.9 ± 14.26   |54.05 ± 13.38  |
#> |BMI            |30.66 ± 5.43   |30.7 ± 5.64    |30.53 ± 4.53   |
#> |Weight (kg)    |75.06 ± 14.23  |73.88 ± 14.09  |79.69 ± 13.95  |
#> |Height (cm)    |156.56 ± 9.18  |155.29 ± 8.46  |161.55 ± 10.24 |
