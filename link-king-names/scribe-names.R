#This next line is run when the whole file is executed, but not when knitr calls individual chunks.
rm(list=ls(all=TRUE)) #Clear the memory for any variables set from any previous runs.


# ---- load-sources ------------------------------------------------------------


# ---- load-packages -----------------------------------------------------------
library(magrittr                , quietly=TRUE)
library(stringr                               )
library(wakefield                             )
requireNamespace("dplyr"                      )
requireNamespace("lubridate"                  )
requireNamespace("forcats"                    )


# ---- declare-globals ---------------------------------------------------------
# readr::spec_csv(path_in)
path_in      <- "./link-king-names/names-celebrities/names_raw.csv"


col_types = readr::cols_only(
    birth_year     = readr::col_integer(),
    primary_name   = readr::col_character()
    # X3             = readr::col_character(),
    # X4             = readr::col_character()
)

fake_ssn_with_repeats_function <- function(d){
  # Specifically avoiding formats invalid as of 2011-06-25
  d <-
    d %>% 
    dplyr::mutate(
      area     = sample(c(1:665, 667:699, 729:899), dplyr::n(), replace = TRUE),
      group    = sample(c(1:98)                   , dplyr::n(), replace = TRUE), # stopping short of 99, so I can safely increment by 1 if need be
      serial   = sample(c(1:9999)                 , dplyr::n(), replace = TRUE),
      
      area_3   = stringr::str_pad(area  , 3, pad = "0"),
      group_2  = stringr::str_pad(group , 2, pad = "0"),
      serial_4 = stringr::str_pad(serial, 4, pad = "0"),
      
      ssn_fake  = paste(area_3, group_2, serial_4, sep = "-")
    ) %>% 
    dplyr::select(
      -area,
      -group,
      -serial,
      -area_3,
      -group_2,
      -serial_4
    )
  
  
  return(d)
  
}


# ---- load-data ---------------------------------------------------------------
# ds_with_duplicates <- readr::read_csv(path_in, col_types=col_types)
# ds_with_duplicates <- readr::read_csv(path_in, col_types=col_types, local = readr::locale(encoding = "ISO-8859-1"))
# ds_with_duplicates <- readr::read_csv(path_in, col_types=col_types, local = readr::locale(encoding = "UTF-8"))
ds_names_raw <- readr::read_csv(path_in, col_types=col_types)

rm(path_in, col_types)

# dsq <-
#   ds_names_raw %>%
#   tibble::as_tibble() %>% 
#   dplyr::mutate(
#     rown = 1:dplyr::n()
#   ) %>% 
#   dplyr::filter(!is.na(X3) | !is.na(X4))

set.seed(1321)

floor(runif(19, min = 0, max = 2))

dsq2 <-
  ds_names_raw %>% 
  dplyr::mutate(
    rown_o = 1:dplyr::n(),
  ) %>% 
  dplyr::filter(rown_o <= 100 ) %>%
  dplyr::filter(!is.na(birth_year)) %>% 
  dplyr::group_by(rown_o) %>% 
  dplyr::mutate(
    count_name_segments = length(strsplit(primary_name, " ")[[1]]),
    name_first          =        strsplit(primary_name, " ")[[1]][1],
    name_last           =        strsplit(primary_name, " ")[[1]][2],
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(count_name_segments <= 2) %>% 
  dplyr::mutate(
   name_middle = dplyr::lag(name_first, 1),
   name_middle = dplyr::if_else(!is.na(name_middle), name_middle, dplyr::lead(name_first, 1)),
   
   name_maiden = dplyr::lead(name_last, 1),
   name_maiden = dplyr::if_else(!is.na(name_maiden), name_maiden, dplyr::lag(name_last, 1)),
   
   gender      = sample(0:1, dplyr::n(), replace = TRUE),
   gender      = dplyr::if_else(gender == 1, "Male", "Female"),
   
   ethnicity   = sample(1:5, dplyr::n(), replace = TRUE),
   ethnicity   = dplyr::case_when(
     ethnicity == 1 ~ "Asian",
     ethnicity == 2 ~ "African American",
     ethnicity == 3 ~ "Caucasian",
     ethnicity == 4 ~ "Hispanic",
     ethnicity == 5 ~ "Native American",
   ),
   
   birth_day   = sample(1:28, dplyr::n(), replace = TRUE),
   birth_month = sample(1:12, dplyr::n(), replace = TRUE),
   
   dob         = as.Date(paste(birth_year, birth_month, birth_day, sep = "-")),
   
   zip_code    = zip_code(n = dplyr::n(), k = dplyr::n())
   
  ) %>% 
  fake_ssn_with_repeats_function() %>% 
  dplyr::distinct(ssn_fake, .keep_all = TRUE) %>% 
  dplyr::mutate(
    rown_o          = as.character(rown_o),
    rown_o_w        = paste0("w",rown_o),
    
    name_first_w    = name_first,
    name_last_w     = name_last,
    name_middle_w   = name_middle,
    name_maiden_w   = name_maiden,
    gender_w        = gender,
    ethnicity_w     = ethnicity,
    dob_w           = dob,
    ssn_fake_w      = ssn_fake,
    
    name_middle_missing = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.40)),
    name_maiden_missing = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.40)),
    gender_missing      = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.40)),
    
    gender_swapped      = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.20)),
    
    name_middle_w = dplyr::if_else(name_middle_missing, NA_character_, name_middle_w),
    name_maiden_w = dplyr::if_else(name_maiden_missing, NA_character_, name_maiden_w),
    
    gender_w      = dplyr::case_when(
      !gender_swapped ~ gender_w,
      gender_swapped & gender_w == "Male"   ~ "Female",
      gender_swapped & gender_w == "Female" ~ "Male",
    ),
    
    gender_w      = dplyr::if_else(gender_missing     , NA_character_, gender_w     ),
  )

