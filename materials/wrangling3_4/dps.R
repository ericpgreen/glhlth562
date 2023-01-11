library(tidyverse)
library(googlesheets4)
gs4_deauth()

raw <- read_sheet("https://docs.google.com/spreadsheets/d/1kQ35JKpk3vNaM7ZfFK2DFb1B9kQIQ4IODr7lOM394nQ/edit#gid=769268401") %>%
  select(-c(3:5))

# google "r vector from row"
# https://stackoverflow.com/a/55262815/841405
  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE)

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = stringr::str_remove(value,
                                       "Positive COVID-19 Cases\n"))

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = stringr::str_remove(value,
                                       "Positive COVID-19 Cases\n")) %>%
    mutate(value = stringr::str_trim(value, "both"))

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = stringr::str_remove(value,
                                       "Positive COVID-19 Cases\n")) %>%
    mutate(value = stringr::str_trim(value, "both")) %>%
    slice(4:n())

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = stringr::str_remove(value,
                                       "Positive COVID-19 Cases\n")) %>%
    mutate(value = stringr::str_trim(value, "both")) %>%
    slice(4:n()) %>%
    mutate(value = str_replace(value,
                               # pattern:
                               #  find first comma (\\,)
                               #  and any character after it (.)
                               #  all times (*)
                               "\\,.*",

                               # replace with:
                               #  nothing ("")
                               ""
                               )
           )

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = stringr::str_remove(value,
                                       "Positive COVID-19 Cases\n")) %>%
    mutate(value = stringr::str_trim(value, "both")) %>%
    slice(4:n()) %>%
    mutate(value = str_replace(value,
                               # pattern:
                               #  find first comma (\\,)
                               #  and any character after it (.)
                               #  all times (*)
                               "\\,.*",

                               # replace with:
                               #  nothing ("")
                               ""
    )
    ) %>%
    mutate(value = str_replace(value,
                               "\\-.*",
                               ""))

  start_date <- raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = stringr::str_remove(value,
                                       "Positive COVID-19 Cases\n")) %>%
    mutate(value = stringr::str_trim(value, "both")) %>%
    slice(3:n()) %>%
    mutate(value = str_replace(value, "\\,.*", "")) %>%
    mutate(value = str_replace(value, "\\-.*", ""))

# google: "r repeat vector in place"
# https://stackoverflow.com/a/15141795/841405
  dates <- rep(start_date$value, each = 3)

  labels <- rep(c("students", "staff", "clusers"), nrow(start_date))

  dates_labels <- paste(dates, labels, sep = "_")

  raw %>%
    setNames(c("code", "site_name", dates_labels))

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n())

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL")))

  elem <- c("320304", "320308", "320374", "320318",
            "320319", "320363", "320313", "320310",
            "320315", "320344", "320332", "320347",
            "320320", "320324", "320328", "320327",
            "320339", "320340", "320348", "320352",
            "320354", "320360", "320362", "320364",
            "320367", "320369", "320372", "320376",
            "320388", "320400", "320289")

  middle <- c("320306", "320316", "320338", "320342",
              "320346", "320343", "320355",  "320370",
              "320366")

  high <- c("320312", "320317", "320323", "320322",
            "320309", "320325", "320701", "320341",
            "320353", "320356", "320365", "320368",
            "320314", "320401")

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL"))) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    ))


  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL"))) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .)))

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL"))) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.)))

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL"))) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_")

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL"))) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_") %>%
    mutate(year = case_when(
            grepl("August", start_date) ~ 2021,
            grepl("September", start_date) ~ 2021,
            grepl("October", start_date) ~ 2021,
            grepl("November", start_date) ~ 2021,
            grepl("December", start_date) ~ 2021,
            TRUE ~ 2022),
           start_date = paste(start_date, year),
           start_date = lubridate::parse_date_time(start_date,
                                                   "%b d y"))

  dps <- raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% c("Central Services Total",
                              "Elementary Total",
                              "Middle Total",
                              "High/Secondary Total",
                              "GRAND TOTAL"))) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_") %>%
    mutate(year = case_when(
      grepl("August", start_date) ~ 2021,
      grepl("September", start_date) ~ 2021,
      grepl("October", start_date) ~ 2021,
      grepl("November", start_date) ~ 2021,
      grepl("December", start_date) ~ 2021,
      TRUE ~ 2022),
      start_date = paste(start_date, year),
      start_date = lubridate::parse_date_time(start_date,
                                              "%b d y")) %>%
    select(-year)

  dps %>%
    group_by(level, start_date, type) %>%
    summarize(totals = sum(value))
