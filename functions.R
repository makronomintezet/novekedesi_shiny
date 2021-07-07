library(tidyverse)
options(scipen = 999)

NiceName <- function(x) {
  case_when(
    x == "adjusted_net_saving" ~ "Kiigazított nettó megtakarítás",
    x == "adjTFR" ~ "Tempó-igazított TTA",
    x == "tfr" ~ "Teljes termékenységi arányszám",
    x == "co2" ~ "Szén-dioxid kibocsátás",
    x == "dependency_ratio" ~ "Függőségi ráta", 
    x == "employment_ratio" ~ "Foglalkoztatási ráta",
    x == "gdp_per_capita" ~ "Egy főre eső GDP", 
    x == "gdp_ppp" ~ "GDP (PPP)",
    x == "gini" ~ "Gini koefficiens", 
    x == "life_expectancy" ~ "Várható élettartam", 
    x == "median_income" ~ "Medián jövedelem", 
    x == "n_employee" ~ "Foglalkoztatottak száma",
    x == "poverty_rate" ~ "Szegénységi arány",
    x == "public_debt" ~ "Államadósság",
    x == "unemployment_ratio" ~ "Munkanélküliségi ráta",
    x == "unemployment_program" ~ "Munkanélkülieket segítő programok",
    x == "labor_productivity" ~ "Munkatermelékenység",
    x == "labor_tax" ~ "Munkát terhelő adók",
    x == "p90p10" ~ "P90P10",
    x == "gni_herfindahl" ~ "GNI területi koncentrációja",
    x == "gni_diff" ~ "GNI legnagyobb területi eltérése (%)",
    x == "internet_usage" ~ "Internetet használók aránya",
    x == "real_wage" ~ "Reálbér",
    x == "upper_isced3" ~ "Középszintű oktatásban részesülők aránya",
    x == "tax_wedge" ~ "Adóék",
    x == "country" ~ "Ország",
    x == "development" ~ "Fejlettség",
    x == "advanced_economies" ~ "Fejlett országok",
    x == "emerging_economies" ~ "Fejlődő országok",
    x == "emerging_economies_cont_d" ~ "Fejlődő országok (alsóbb)",
    x == "time" ~ "Év",
    x == "population" ~ "Népesség",
    x == "renewables" ~ "Megújúló energiaforrás",
    x == "nuclear" ~ "Nukleáris energiaforrás",
    x == "infrastructure" ~ "Infrastruktúra",
    x == "fossil_fuels" ~ "Fosszilis tüzelőanyagok",
    x == "environmental_tax" ~ "Zöld adók",
    TRUE ~ x
  )
}

names_df <- tribble(
  ~ code, ~ hun,
  "adjusted_net_saving",  "Kiigazított nettó megtakarítás",
  "adjTFR",  "Tempó-igazított TTA",
  "tfr",  "Teljes termékenységi arányszám",
  "co2",  "Szén-dioxid kibocsátás",
  "dependency_ratio",  "Függőségi ráta", 
  "employment_ratio",  "Foglalkoztatási ráta",
  "gdp_per_capita",  "Egy főre eső GDP", 
  "gdp_ppp",  "GDP (PPP)",
  "gini",  "Gini koefficiens", 
  "life_expectancy",  "Várható élettartam", 
  "median_income",  "Medián jövedelem", 
  "n_employee",  "Foglalkoztatottak száma",
  "poverty_rate",  "Szegénységi arány",
  "public_debt",  "Államadósság",
  "unemployment_ratio", "Munkanélküliségi ráta",
  "unemployment_program", "Munkanélkülieket segítő programok",
  "labor_productivity", "Munkaerő-hatékonyság",
  "labor_tax", "Munkát terhelő adók",
  "p90p10", "P90P10",
  "gni_herfindahl", "GNI területi koncentrációja",
  "gni_diff", "GNI legnagyobb területi eltérése (%)",
  "internet_usage", "Internetet használók aránya",
  "real_wage", "Reálbér",
  "upper_isced3", "Középszintű oktatásban részesülők aránya",
  "tax_wedge", "Adóék",
  "renewables", "Megújúló energiaforrás",
  "nuclear", "Nukleáris energiaforrás",
  "infrastructure", "Infrastruktúra",
  "fossil_fuels", "Fosszilis tüzelőanyagok",
  "environmental_tax", "Zöld adók",
  "country",  "Ország",
  "development",  "Fejlettség",
  "advanced_economies",  "Fejlett országok",
  "emerging_economies",  "Fejlődő országok",
  "emerging_economies_cont_d",  "Fejlődő országok (alsóbb)",
  "population", "Népesség",
  "time",  "Év"
)

recode_name <- function(name, to_nice = TRUE) {
  if (to_nice) {
    df <- set_names(names_df, "old", "new")
  } else {
    df <- set_names(names_df, "new", "old")
  }
  index <- which(df$old == name)
  if (!is.null(index)) {
    name <- df$new[index]
  }
  name
}


FormatMoney <- function(x, digits = 0) {
  case_when(
    abs(x) >= 1e9 ~ str_c(format(x / 1e9, digits = digits), " mrd $"),
    abs(x) >= 1e6 ~ str_c(format(x / 1e6, digits = digits), " m $"),
    abs(x) >= 1e3 ~ str_c(format(x / 1e3, digits = digits), " ezer $"),
    T ~ str_c(format(x, digits = digits), " Ft")
  )
}

percent_hu <- function(x, accuracy = 0.01) {
  scales::percent(x, decimal.mark = ",", accuracy = accuracy)
}

add_dimension <- function(x, hun = FALSE) {
  dimension <- case_when(
    x == "adjusted_net_saving" ~ "sustainability",
    x == "adjTFR" ~ "sustainability",
    x == "tfr" ~ "sustainability",
    x == "co2" ~ "sustainability",
    x == "dependency_ratio" ~ "sustainability", 
    x == "employment_ratio" ~ "work",
    x == "gdp_per_capita" ~ "economic", 
    x == "gdp_ppp" ~ "economic",
    x == "gini" ~ "equality", 
    x == "life_expectancy" ~ "economic", 
    x == "median_income" ~ "economic", 
    x == "n_employee" ~ "work",
    x == "poverty_rate" ~ "equality",
    x == "public_debt" ~ "sustainability",
    x == "unemployment_ratio" ~ "work",
    x == "unemployment_program" ~ "work",
    x == "labor_productivity" ~ "work",
    x == "labor_tax" ~ "work",
    x == "p90p10" ~ "equality",
    x == "gni_herfindahl" ~ "equality",
    x == "gni_diff" ~ "equality",
    x == "internet_usage" ~ "economic",
    x == "real_wage" ~ "work",
    x == "upper_isced3" ~ "sustainability",
    x == "tax_wedge" ~ "work",
    x == "renewables" ~ "sustainability",
    x == "nuclear" ~ "sustainability",
    x == "infrastructure" ~ "economic",
    x == "fossil_fuels" ~ "sustainability",
    x == "environmental_tax" ~ "sustainability",
    TRUE ~ "unknown" # default value for uncategorized variables
  )
  if (hun) {
    dimension <- case_when(
      dimension == "sustainability" ~ "Fenntarthatóság",
      dimension == "work" ~ "Munkaalapú társadalom",
      dimension == "economic" ~ "Gazdasági fejlettség és növekedés",
      dimension == "equality" ~ "Társadalmi egyenlőtlenség",
      TRUE ~ "Egyéb"
    )
  }
  dimension
}

add_direction <- function(x) {
  case_when(
    x == "adjusted_net_saving" ~ "good",
    x == "adjTFR" ~ "good",
    x == "tfr" ~ "good",
    x == "co2" ~ "bad",
    x == "dependency_ratio" ~ "bad", 
    x == "employment_ratio" ~ "good",
    x == "gdp_per_capita" ~ "good", 
    x == "gdp_ppp" ~ "good",
    x == "gini" ~ "bad", 
    x == "life_expectancy" ~ "good", 
    x == "median_income" ~ "good", 
    x == "n_employee" ~ "good",
    x == "poverty_rate" ~ "bad",
    x == "public_debt" ~ "bad",
    x == "unemployment_ratio" ~ "bad",
    x == "unemployment_program" ~ "good",
    x == "labor_productivity" ~ "good",
    x == "labor_tax" ~ "bad",
    x == "p90p10" ~ "bad",
    x == "gni_herfindahl" ~ "bad",
    x == "gni_diff" ~ "bad",
    x == "internet_usage" ~ "good",
    x == "real_wage" ~ "good",
    x == "upper_isced3" ~ "good",
    x == "tax_wedge" ~ "bad",
    x == "renewables" ~ "good",
    x == "nuclear" ~ "bad",
    x == "infrastructure" ~ "good",
    x == "fossil_fuels" ~ "good",
    x == "environmental_tax" ~ "good",
    TRUE ~ "good" # default value for uncategorized variables
  )
}

NiceFormat <- function(x, y) {
  case_when(
    x == "adjusted_net_saving" ~ scales::percent(y, accuracy = .01, decimal.mark = ","),
    x == "adjTFR" ~ format(y, digits = 2, decimal.mark = ","),
    x == "tfr" ~ format(y, digits = 2, decimal.mark = ","),
    x == "co2" ~ format(y, digits = 2, decimal.mark = ","),
    x == "dependency_ratio" ~ scales::percent(y, accuracy = .01, decimal.mark = ","), 
    x == "employment_ratio" ~ scales::percent(y, accuracy = .01, decimal.mark = ","),
    x == "gdp_per_capita" ~ str_c(format(y / 1e3, digits = 0), " ezer $"), 
    x == "gdp_ppp" ~ str_c(format(y / 1e9, digits = 2, decimal.mark = ","), " milliárd $"),
    x == "gini" ~ format(y, digits = 2, decimal.mark = ","), 
    x == "life_expectancy" ~ format(y, digits = 1, decimal.mark = ","), 
    x == "median_income" ~ str_c(format(y), " $"), 
    x == "n_employee" ~ "Foglalkoztatottak száma",
    x == "poverty_rate" ~ scales::percent(y, decimal.mark = ",", accuracy = .01),
    x == "public_debt" ~ scales::percent(y / 100, accuracy = .01, decimal.mark = ","),
    x == "unemployment_ratio" ~ scales::percent(y / 100, accuracy = .01, decimal.mark = ","),
    x == "unemployment_program" ~ scales::percent(y / 100, accuracy = .01, decimal.mark = ","),
    x == "labor_productivity" ~ "Munkatermelékenység",
    x == "labor_tax" ~ "Munkát terhelő adók",
    x == "p90p10" ~ "P90P10",
    x == "gni_herfindahl" ~ "GNI területi koncentrációja",
    x == "gni_diff" ~ "GNI legnagyobb területi eltérése (%)",
    x == "internet_usage" ~ "Internetet használók aránya",
    x == "real_wage" ~ "Reálbér",
    x == "upper_isced3" ~ "Középszintű oktatásban részesülők aránya",
    x == "tax_wedge" ~ "Adóék",
    x == "country" ~ "Ország",
    x == "development" ~ "Fejlettség",
    x == "advanced_economies" ~ "Fejlett országok",
    x == "emerging_economies" ~ "Fejlődő országok",
    x == "emerging_economies_cont_d" ~ "Fejlődő országok (alsóbb)",
    x == "time" ~ "Év",
    x == "population" ~ "Népesség",
    x == "renewables" ~ "Megújúló energiaforrás",
    x == "nuclear" ~ "Nukleáris energiaforrás",
    x == "infrastructure" ~ "Infrastruktúra",
    x == "fossil_fuels" ~ "Fosszilis tüzelőanyagok",
    x == "environmental_tax" ~ "Zöld adók",
    TRUE ~ x
  )
}

GetRank <- function(x, n_group = 5, variable = NULL) {
  cut(x, quantile(x, 0:n_group/n_group, na.rm = T), F, include.lowest = T)
}

GetRank <- possibly(GetRank, 3, F)

herfindahl <- function(x) {
  sum((x / sum(x))^2)
}

# theme ------------------------------------------------------------------------

update_geom_defaults("point", list(fill = "#C42E35", 
                                   shape = 21, 
                                   color = "black", 
                                   size = 1.4))
update_geom_defaults("line", 
                     list(color = "#C42E35", size = 1.4))
update_geom_defaults("smooth", list(color = "#C42E35", size = 1.4))
update_geom_defaults("col", list(fill = "#2DA2BF"))
update_geom_defaults("bar", list(fill = "#2DA2BF"))
update_geom_defaults("hline", list(yintercept = 0, color = "grey50", size = 1))
update_geom_defaults("vline", list(xintercept = 0, color = "grey50", size = 1))
update_geom_defaults("density", 
                     list(color = "#C42E35", fill =  "#C42E35", alpha = .3, 
                          size = 1.4))
theme_set(
  theme_minimal() + 
    theme(
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      axis.title.y = element_text(size = 13)
    )
)

myplotly <- function(p, height = NULL, width = NULL) {
  font = list(
    family = "Alright Sans Regular", 
    size = 15,
    color = "black"
  )
  
  label = list(
    bgcolor = "#2DA2BF",
    bordercolor = "transparent",
    font = font
  )
  
  tooltip = "text"
  plotly::ggplotly(p, tooltip = tooltip, height = height, width = width) %>% 
    plotly::style(hoverlabel = label) %>% 
    plotly::layout(font = font) %>% 
    plotly::config(displayModeBar = FALSE)
}



