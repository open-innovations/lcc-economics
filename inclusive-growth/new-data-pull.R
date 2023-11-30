# new data pull

geographies <- list(
  target = "1811939401",
  core_cities = c("1811939460", "1811939630", "1811939683","1811939712","1811939378","1811939357","1811939339","1811939405","1811939397"
  ),
  west_yorkshire = "1853882371",
  yorkshire_humber = "2013265923",
  united_kingdom = "2092957697"
)

# Older stuff -------------------------------------------------------------

# nomis_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1811939401,1811939460,1811939630,1811939683,1811939712,1811939378,1811939357,1811939339,1811939405,1811939397,2092957697,1853882371,2013265923&variable=74,1213,1219,1220,117,118,1329...1338,18,45,84,111,290,720,344,1493...1499,1487,1488&measures=20599,21001&uid=0xce1485b8af597e9021c978a63225cd5b792607df"
#
# nomis <- readr::read_csv(nomis_url)
#
# nomis_data <- setNames(nomis, tolower(names(nomis))) |>
#   dplyr::select(date, date_name,
#                 geography_code, geography_name, geography_type,
#                 variable_code, variable_name,
#                 measures_code = measures, measures_name,
#                 value = obs_value) |>
#   # dplyr::filter(measures_name == "Variable" | measures_name == "Numerator") |>
#   dplyr::mutate(date = as.Date(paste0(date, "-01"))) |>
#   dplyr::mutate(category = dplyr::case_when(
#     grepl("% all in employment|Employment", variable_name) ~ "Employment",
#     grepl("Economic activity", variable_name) ~ "Economic activity",
#     grepl("Unemployment", variable_name) ~ "Unemployment",
#     grepl("self employed", variable_name) ~ "Self employment",
#     grepl("economically inactive", variable_name) ~ "Economic inactivity",
#     grepl("NVQ", variable_name) ~ "Qualifications",
#   )) |>
#   dplyr::mutate(is_summary = dplyr::case_when(
#     variable_name %in% c("% in employment who are self employed - aged 16-64",
#                          "Unemployment rate - aged 16-24",
#                          "Economic activity rate - aged 16-64",
#                          "Employment rate - aged 16-64",
#                          "Unemployment rate - aged 16-64",
#                          "% who are economically inactive - aged 16-64",
#                          "% with NVQ4+ - aged 16-64") ~ TRUE,
#     TRUE ~ FALSE
#   )) |>
#   dplyr::filter(!grepl("% all in employment who work in", variable_name))

geographies <- list(
  target = "1811939401",
  core_cities = c("1811939460", "1811939630", "1811939683", "1811939712",
                  "1811939378", "1811939357", "1811939339", "1811939405",
                  "1811939397"
  ),
  west_yorkshire = "1853882371",
  yorkshire_humber = "2013265923",
  united_kingdom = "2092957697"
)

geogs <- unlist(geographies)

variables <- list(
  employment = c(employment = 45 #,
                 # employment_by_industry = 1329:1338
  ),
  unemployment = 84,
  economic_activity = 18,
  economic_inactivity = c(inactivity = 111,
                          inactivity_by_age = c(1219, 1220, 117, 118),
                          inactivity_reasons = 1493:1499,
                          inactivity_wants_job = 1487:1488),
  self_employment = 74,
  youth_unemployment = 1213,
  qualifications = c(344, 720, 290)
)

vars <- unlist(variables)

measures <- list(
  variable = 20599,
  numerator = 21001
)

meas <- unlist(measures)

build_nomis_url <- function(geography, variable, measures) {
  endpoint <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?"
  geography <- paste0("geography=", paste(geography, collapse = ","))
  variable <- paste0("variable=", paste(variable, collapse = ","))
  measure <- paste0("measures=", paste(measures, collapse = ","))
  selectors <- paste(geography, variable, measure, sep = "&")
  url <- paste0(endpoint, selectors, "&uid=0xce1485b8af597e9021c978a63225cd5b792607df")
  return(url)
}

build_nomis_url(geogs, vars, meas)

# readr::read_csv(build_nomis_url(geogs, vars, meas))

retrieve_nomis_data <- function(url) {
  readr::read_csv(url, name_repair = tolower) |>
    dplyr::select(date, date_name,
                  geography_code, geography_name, geography_type,
                  variable_code, variable_name,
                  measures_code = measures, measures_name,
                  value = obs_value) |>
    dplyr::mutate(date = as.Date(paste0(date, "-01")))
}

data <- list()
data$employment <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$employment),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Employment") |>
  dplyr::mutate(is_summary = TRUE)

data$unemployment <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$unemployment),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Unemployment") |>
  dplyr::mutate(is_summary = TRUE)

data$economic_activity <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$economic_activity),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Economic activity") |>
  dplyr::mutate(is_summary = TRUE)

data$economic_inactivity <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$economic_inactivity),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Economic inactivity") |>
  dplyr::mutate(is_summary = ifelse(variable_code == 111, TRUE, FALSE))

data$self_employment <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$self_employment),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Self employment") |>
  dplyr::mutate(is_summary = TRUE)

data$youth_unemployment <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$youth_unemployment),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Youth unemployment") |>
  dplyr::mutate(is_summary = TRUE)

data$qualifications <- retrieve_nomis_data(
  build_nomis_url(unlist(geographies),
                  unlist(variables$qualifications),
                  unlist(measures))
) |>
  dplyr::mutate(category = "Qualifications") |>
  dplyr::mutate(is_summary = ifelse(variable_code == 290, TRUE, FALSE))

nomis_data <- dplyr::bind_rows(data) |>
  dplyr::mutate(geography_core_city = ifelse(
    !geography_type %in% c("countries",
                           "combined authorities",
                           "regions"),
    TRUE,
    FALSE)) |>
  # temporary fixes to align to old style formatting
  dplyr::mutate(variable_name_full = variable_name) |>
  dplyr::mutate(variable_name = dplyr::case_when(
    variable_name == "% in employment who are self employed - aged 16-64" ~
      "Self employed",
    variable_name == "Unemployment rate - aged 16-24" ~
      "Youth unemployment",
    variable_name == "Economic activity rate - aged 16-64" ~
      "Economic activity",
    variable_name == "Employment rate - aged 16-64" ~
      "Employment",
    variable_name == "Unemployment rate - aged 16-64" ~
      "Unemployment",
    variable_name == "% who are economically inactive - aged 16-64" ~
      "Economic inactivity",
    variable_name == "% with NVQ4+ - aged 16-64" ~
      "NVQ4+",
    variable_name == "% with NVQ3+ - aged 16-64" ~
      "NVQ3+",
    variable_name == "% with no qualifications (NVQ) - aged 16-64" ~
      "No NVQs",
    TRUE ~ variable_name
  ))

nomis_cc_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1811939401,1811939460,1811939630,1811939683,1811939712,1811939378,1811939357,1811939339,1811939405,1811939397,2092957697,1853882371,2013265923&gender=0&age=0&measure=1,2&measures=20100"

nomis_cc_temp <- readr::read_csv(nomis_cc_url, name_repair = tolower)

nomis_cc <- readr::read_csv("data-raw/inclusive-growth-dashboard/nomis-cc.csv")

nomis_cc_data <- nomis_cc_temp |>
  # dplyr::filter(gender_name == "Total") |>
  dplyr::select(date, date_name,
                geography_code, geography_name, geography_type,
                variable_code = measure_code, variable_name = measure_name,
                measures_code = measures, variable_name_full = measures_name, age_name,
                value = obs_value) |>
  dplyr::mutate(date = as.Date(paste0(date, "-01"))) |>
  dplyr::filter(#variable_name != "Claimant count",
                !is.na(value)) |>
  dplyr::mutate(measures_name = ifelse(variable_code == 1, "Numerator", "Variable")) |>
  dplyr::mutate(category = "Claimant count") |>
  # dplyr::mutate(is_summary = dplyr::case_when(
  #   age_name == "All categories: Age 16+" ~ TRUE,
  #   TRUE ~ FALSE
  # )) |>
  dplyr::mutate(is_summary = TRUE) |>
  dplyr::mutate(variable_name = "Claimant count")

all_data <- dplyr::bind_rows(nomis_data, nomis_cc_data) |>
  dplyr::mutate(geography_core_city = ifelse(
                  !geography_type %in% c("countries",
                                         "combined authorities",
                                         "regions"),
                  TRUE,
                  FALSE)) |>
  dplyr::mutate(variable_name_full = variable_name) |>
  dplyr::mutate(variable_name = dplyr::case_when(
    variable_name == "% in employment who are self employed - aged 16-64" ~
      "Self employed",
    variable_name == "Unemployment rate - aged 16-24" ~
      "Youth unemployment",
    variable_name == "Economic activity rate - aged 16-64" ~
      "Economic activity",
    variable_name == "Employment rate - aged 16-64" ~
      "Employment",
    variable_name == "Unemployment rate - aged 16-64" ~
      "Unemployment",
    variable_name == "% who are economically inactive - aged 16-64" ~
      "Economic inactivity",
    variable_name == "% with NVQ4+ - aged 16-64" ~
      "NVQ4+",
    variable_name == "% with NVQ3+ - aged 16-64" ~
      "NVQ3+",
    variable_name == "% with no qualifications (NVQ) - aged 16-64" ~
      "No NVQs",
    TRUE ~ variable_name
  ))

# GVA

rgva_lad <- readr::read_csv("~/Data/rgva_lad.csv") |>
  dplyr::select(date,
                geography_code = `LAD code`,
                geography_name = `LA name`,
                industry_code = SIC07,
                industry_name = `SIC07 description`,
                variable_name = variable,
                value) |>
  dplyr::filter(geography_code %in% all_data$geography_code) |>
  dplyr::filter(industry_code == "Total",
                variable_name == "CVM Pounds") |>
  dplyr::mutate(geography_type = "local authorities",
                category = "GVA",
                is_summary = TRUE,
                geography_core_city = TRUE,
                variable_name = "GVA",
                variable_name_full = "Gross Value Added")

rgva <- readr::read_csv("~/Data/ONS/Regional Accounts/GVA/rgva.csv") |>
  dplyr::select(date = dates.date,
                geography_code, geography_name, geography_type,
                industry_code, industry_name,
                variable_name = variable,
                value) |>
  dplyr::filter(geography_name %in% c(# "United Kingdom",
                                      "Yorkshire and The Humber",
                                      "West Yorkshire"),
                industry_code == "Total",
                variable_name == "constant") |>
  dplyr::mutate(category = "GVA",
                is_summary = FALSE,
                geography_core_city = FALSE,
                variable_name = "GVA",
                variable_name_full = "Gross Value Added")

rgva_data <- dplyr::bind_rows(rgva_lad, rgva) |>
  dplyr::mutate(date_name = as.character(substr(date, 1, 4)))

# CLIF

clif <- readxl::read_excel("inclusive-growth/table_2023-08-30_09-42-41.xlsx", skip = 8)
names(clif)[2] <- "geography_name"
clif2 <- clif |>
  dplyr::select(-Year) |>
  tidyr::pivot_longer(-geography_name, names_to = "date", values_drop_na = TRUE) |>
  dplyr::mutate(date = as.Date(paste0(substr(date, 1, 4), "-01-01")))

wy <- clif2 |>
  dplyr::filter(geography_name %in% c("Bradford",
                                      "Calderdale",
                                      "Kirklees",
                                      "Leeds",
                                      "Wakefield")) |>
  dplyr::group_by(date) |>
  dplyr::summarise(geography_name = "West Yorkshire", value = sum(value))

clif2 <- clif2 |>
  dplyr::filter(!geography_name %in% c("Bradford",
                                      "Calderdale",
                                      "Kirklees",
                                      "Wakefield",
                                      "Great Britain",
                                      "Northern Ireland")) |>
  dplyr::mutate(geography_name = ifelse(geography_name == "Total",
                                        "UK",
                                        geography_name)) |>
  dplyr::bind_rows(wy) |>
  dplyr::mutate(geography_type = ifelse(!geography_name %in% c("Yorkshire and The Humber", "West Yorkshire", "UK"), "local authorities", "other"),
                category = "Children in Low Income Families",
                is_summary = TRUE,
                geography_core_city = ifelse(!geography_name %in% c("Yorkshire and The Humber", "West Yorkshire", "UK"), TRUE, FALSE),
                variable_name = "Children in Low Income Families")

mype_api <- paste0(
  "https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",
  paste(unlist(geographies), collapse = ","),
  "&gender=0&c_age=MAKE|0_16|201;117&measures=20100"
)

mype_data <- readr::read_csv(mype_api)
names(mype_data) <- tolower(names(mype_data))

mype_data_final <- mype_data |>
  dplyr::mutate(date = as.Date(paste0(date, "-01-01")),
                date_name = as.character(date_name)) |>
  dplyr::select(date, date_name,
                geography_code, geography_name, geography_type,
                variable_name = c_age_name,
                value = obs_value)

clif_rate <- dplyr::left_join(clif2, mype_data_final,
                              by = c("date", "geography_name")) |>
  dplyr::mutate(rate = round((value.x / value.y * 100), 1)) |>
  dplyr::select(date, date_name,
                geography_code, geography_name, geography_type = geography_type.x,
                variable_name = variable_name.x,
                rate,
                count = value.x,
                category, is_summary, geography_core_city) |>
  tidyr::pivot_longer(cols = c(rate, count), names_to = "measures_name") |>
  dplyr::mutate(measures_name = ifelse(measures_name == "rate", "Variable", "Numerator")) |>
  dplyr::mutate(variable_name_full = "Children living in relative low income")

# final grouping

all_data <- dplyr::bind_rows(all_data, rgva_data, clif_rate)

# readr::write_csv(all_data, "inclusive-growth/all_data.csv")

### Specialised groupings for LCC industry definitions
### Should be pulled from BRES at SIC levels

# all industry definitions
# https://www.nomisweb.co.uk/api/v01/dataset/NM_189_1/industry/def.sdmx.htm

bres <- jsonlite::fromJSON("https://www.nomisweb.co.uk/api/v01/dataset/NM_189_1/industry/def.sdmx.json")

bres <- bres$structure$codelists$codelist$code[[1]] |>
  dplyr::select(description, value) |>
  jsonlite::flatten() |>
  dplyr::select(industry_code = description.value,
                bres_industry_code = value) |>
  tidyr::separate(industry_code, c("industry_code", "industry_name"), " : ")


lcc_sectors <- readr::read_csv("inclusive-growth/lcc-sectors.csv") |>
  tidyr::separate(industry, c("industry_code", "industry_name"), " : ")

joined <- dplyr::left_join(lcc_sectors, bres, by = c("industry_code"))

# build lists of each set of bres codes according to LCC sector

out <- list()
lcc_sectors <- joined$lcc_sector |> unique()

for (sector in lcc_sectors) {
  out[[sector]] <- dplyr::filter(joined, lcc_sector == sector)
}

industry_codes <- lapply(out, \(x) {
  x$bres_industry_code
})

# build NOMIS API call

lcc_industry_api <- lapply(lcc_sectors, function(sector) {
  paste0(
    "industry=MAKE|",
    gsub(" ", "%20", sector),
    "|",
    paste(industry_codes[[sector]], collapse = ";")
  )
}) |>
  setNames(lcc_sectors)

lcc_sector_data <- lapply(lcc_sectors, function(sector) {
  api <- paste0(
    "https://www.nomisweb.co.uk/api/v01/dataset/NM_189_1.data.csv?geography=",
    paste(unlist(geographies), collapse = ","),
    "&", # "&date=latest&",
    lcc_industry_api[[sector]],
    "&employment_status=4&measure=1&measures=20100"
  )
  readr::read_csv(api)
}) |>
  dplyr::bind_rows()

names(lcc_sector_data) <- tolower(names(lcc_sector_data))
lcc_sector_data_final <- lcc_sector_data |>
  dplyr::mutate(date = as.Date(paste0(date, "-01-01")),
                date_name = as.character(date_name),
                variable_name = industry_name) |>
  dplyr::select(date, date_name,
                geography_code, geography_name, geography_type,
                industry_code, industry_name,
                variable_name,
                value = obs_value) |>
  dplyr::mutate(category = "Employment",
                is_summary = FALSE,
                geography_core_city = ifelse(grepl("local authorities", geography_type), TRUE, FALSE),
                variable_name_full = paste("Number employed in", variable_name))

# backup
# readr::write_csv(all_data, "inclusive-growth/all_data_backup.csv")

all_data <- dplyr::bind_rows(all_data, lcc_sector_data_final)

# Demographics ------------------------------------------------------------

population_api <- paste0(
  "https://www.nomisweb.co.uk/api/v01/dataset/NM_31_1.data.csv?geography=",
  paste(unlist(geographies), collapse = ","),
  "&sex=7&age=0,24,22,25&measures=20100"
)

population_data <- readr::read_csv(population_api, name_repair = tolower)
names(population_data) <- tolower(names(population_data))

population_data_final <- population_data |>
  dplyr::mutate(date = as.Date(paste0(date, "-01-01")),
                date_name = as.character(date_name)) |>
  dplyr::select(date, date_name,
                geography_code, geography_name, geography_type,
                variable_name = age_name,
                value = obs_value) |>
  dplyr::mutate(variable_name = ifelse(variable_name == "All ages",
                                       "Population",
                                       variable_name)) |>
  dplyr::mutate(geography_core_city = ifelse(grepl("local authorities", geography_type), TRUE, FALSE),
                is_summary = ifelse(grepl("Population", variable_name),
                                    TRUE, FALSE),
                category = "Population",
                variable_name_full = "Mid-year population estimates")

all_data <- dplyr::bind_rows(all_data, population_data_final)

# Productivity ------------------------------------------------------------

prod <- readr::read_csv("~/Data/ONS/Regional Accounts/Productivity/itlproductivity.csv") |>
  dplyr::filter(geography_name %in% c("West Yorkshire",
                                      "Yorkshire and The Humber",
                                      "United Kingdom less Extra-Regio"))

prod_lad <- readr::read_csv("~/Data/ONS/Regional Accounts/Productivity/ladproductivity.csv") |>
  dplyr::filter(geography_name %in% c("Belfast",
                                      "Birmingham",
                                      "Bristol, City of",
                                      "Cardiff",
                                      "Glasgow City",
                                      "Leeds",
                                      "Liverpool",
                                      "Manchester",
                                      "Newcastle upon Tyne",
                                      "Nottingham",
                                      "Sheffield"))

prod_data <- dplyr::bind_rows(prod, prod_lad)

prod_data_final <- prod_data |>
  dplyr::mutate(geography_name = ifelse(geography_name == "United Kingdom less Extra-Regio", "United Kingdom", geography_name)) |>
  dplyr::mutate(date_name = as.character(date_name),
                category = "Productivity",
                is_summary = TRUE,
                geography_core_city = ifelse(!geography_name %in% c("West Yorkshire", "Yorkshire and The Humber", "United Kingdom"), TRUE, FALSE),
                variable_name_full = "-")

all_data <- dplyr::bind_rows(all_data, prod_data_final)
readr::write_csv(all_data, "inclusive-growth/all_data.csv")


# update code (temp deprecated) -------------------------------------------


# nomis_aps_path <- "../data-raw/inclusive-growth-dashboard/nomis-aps.csv"
# nomis_cc_path <- "../data-raw/inclusive-growth-dashboard/nomis-cc.csv"
#
# nomis_aps_info <- file.info(nomis_aps_path)

# if ((Sys.time() - nomis_aps_info$mtime) != 30) {
#   nomis_aps_url <- 'https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1811939401,1811939460,1811939630,1811939683,1811939712,1811939378,1811939357,1811939339,1811939405,1811939397,2092957697,1853882371,2013265923&variable=74,1213,18,45,84,111,290,344,720&measures=20599,21001,21002,21003&uid=0xce1485b8af597e9021c978a63225cd5b792607df'
#   #aps_data <- readr::read_csv(nomis_aps_url)
#   #readr::write_csv(aps_data, nomis_aps_path)
#
#   nomis_cc_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1811939460,1811939630,1811939683,1811939712,1811939378,1811939357,1811939339,1811939405,1811939401,1811939397,1853882371,2092957697,2013265923&date=latestMINUS221-latest&gender=0...2&age=0,2...4&measure=1,2&measures=20100&uid=0xce1485b8af597e9021c978a63225cd5b792607df"
#   #cc_data <- readr::read_csv(nomis_cc_url)
#   #readr::write_csv(cc_data, nomis_cc_path)
#
# } else {
# aps_data <- readr::read_csv(nomis_aps_path)
# cc_data <- readr::read_csv(nomis_cc_path)
#}

# names(aps_data) <- tolower(names(aps_data))

# data1 <- aps_data |>
#   dplyr::filter(measures_name == "Variable",
#                 !is.na(obs_value)) |>
#   dplyr::mutate(date = as.Date(paste0(date, '-01')),
#                 geography_core_city = ifelse(
#                   !geography_type %in% c("countries",
#                                          "combined authorities",
#                                          "regions"),
#                   TRUE,
#                   FALSE)) |>
#   dplyr::select(date, date_name,
#                 geography_code, geography_name,
#                 geography_type, geography_core_city,
#                 variable_code, variable_name,
#                 value = obs_value)
#
# cc <- cc_data |>
#   setNames(tolower(names(cc_data))) |>
#   dplyr::filter(!is.na(obs_value)) |>
#   dplyr::filter(gender_name == "Total",
#                 age_name == "All categories: Age 16+",
#                 measure_name != "Claimant count") |>
#   dplyr::mutate(date = as.Date(paste0(date, "-01"))) |>
#   dplyr::mutate(geography_core_city = ifelse(
#     !geography_type %in% c("countries",
#                            "combined authorities",
#                            "regions"),
#     TRUE,
#     FALSE)) |>
#   dplyr::select(date, date_name,
#                 geography_code, geography_name,
#                 geography_type, geography_core_city,
#                 gender_code, gender_name,
#                 age_code, age_name,
#                 variable_code = measure_code, variable_name = measure_name,
#                 value = obs_value)
#
# data1 <- dplyr::bind_rows(data1, cc)
