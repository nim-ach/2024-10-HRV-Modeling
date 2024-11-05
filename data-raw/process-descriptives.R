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

tbl_full
