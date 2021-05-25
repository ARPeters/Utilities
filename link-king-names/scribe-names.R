# This next line is run when the whole file is executed, but not when knitr calls individual chunks.
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
path_in           <- "./data-unshared/names-celebrities/names_raw.csv"
path_out_matching <- "./data-unshared/names-for-link-king/ds_matching.csv"
path_out_linking  <- "./data-unshared/names-for-link-king/ds_sample.csv"

col_types = readr::cols_only(
    birth_year     = readr::col_integer(),
    primary_name   = readr::col_character(),
    X3             = readr::col_character(),
    X4             = readr::col_character()
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
      # -group,
      -serial,
      # -area_3,
      -group_2,
      # -serial_4,
    )
  
  return(d)
  
}

generate_nickname <- function(d){
  d <-
    d %>% 
    dplyr::mutate(
      name_first_nickname = name_first_w
    ) %>% 
    dplyr::mutate(
      name_first_nickname = dplyr::case_when(
        name_first_nickname %in% c("Jon", "Johnny", "Jonathan")                              ~ "Jon",
        name_first_nickname %in% c("Richard","Dick", "Richie")                               ~ "Rich",
        name_first_nickname %in% c("James", "Jimmy", "Jameson")                              ~ "Jim",
        name_first_nickname %in% c("Georges")                                                ~ "George",
        name_first_nickname %in% c("Frederico", "Freddy", "Frederick", "Alfred", "Federico") ~ "Fred",
        name_first_nickname %in% c("Gerold", "Gérard", "Gerard")                             ~ "Jerry",
        name_first_nickname %in% c("Alex", "Alexander", "Alec", "Alan", "Allen", "Aleksey")  ~ "Al",
        name_first_nickname %in% c("Margaux", "Margarite", "Margaret", "Maggie")             ~ "Marge",
        name_first_nickname %in% c("Katharine", "Catharine", "Katharyn", "Katy")             ~ "Kate",
        name_first_nickname %in% c("Charles", "Charlton")                                    ~ "Charlie",
        name_first_nickname %in% c("William", "Liam", "Bill", "Billy")                       ~ "Will",
        name_first_nickname %in% c("Peter")                                                  ~ "Pete",
        name_first_nickname %in% c("Marilyn")                                                ~ "Mary",
        name_first_nickname %in% c("David")                                                  ~ "Dave",
        name_first_nickname %in% c("Laurence")                                               ~ "Larry",
        name_first_nickname %in% c("Anthony", "Antonio")                                     ~ "Tony",
        name_first_nickname %in% c("Elizabeth")                                              ~ "Liz",
        name_first_nickname %in% c("François", "Franz")                                      ~ "Frank",
        name_first_nickname %in% c("Thomas", "Tom")                                          ~ "Tommy",
        name_first_nickname %in% c("Jacob", "Jakob")                                         ~ "Jake",
        name_first_nickname %in% c("Robert", "Robbie")                                       ~ "Rob",
        name_first_nickname %in% c("Robert", "Robbie")                                       ~ "Rob",
        name_first_nickname %in% c("Walter", "Wallace")                                      ~ "Wally",
        name_first_nickname %in% c("Stephen", "Steven")                                      ~ "Steve",
        
        TRUE                                                                                 ~ name_first_w
        
      ),
      
      nicknamed = dplyr::if_else(name_first_nickname != name_first, TRUE, FALSE)
    )
}


# ---- load-data ---------------------------------------------------------------
ds_names_raw <- readr::read_csv(path_in, col_types=col_types)

# rm(path_in, col_types)


# ---- tweak-data ---------------------------------------------------------------
set.seed(1321)

floor(runif(19, min = 0, max = 2))

ds_middleman <-
  ds_names_raw %>% 
  dplyr::mutate(
    rown_o = 1:dplyr::n(),
  ) %>% 
  dplyr::filter(rown_o <= 50000 ) %>% 
  dplyr::group_by(rown_o) %>% 
  dplyr::mutate(
    count_name_segments = length(strsplit(primary_name, " ")[[1]]),
    name_first          =        strsplit(primary_name, " ")[[1]][1],
    name_last           =        strsplit(primary_name, " ")[[1]][2],
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(count_name_segments <= 2) %>% 
  dplyr::filter(!is.na(name_last)) %>% 
  dplyr::filter(2 <= nchar(name_first)) %>% 
  dplyr::filter(2 <= nchar(name_last)) %>% 
  dplyr::mutate(
    random_birth_year_int = sample(c(1927:2008), size = dplyr::n(), replace = TRUE),
    birth_year            = dplyr::if_else(!is.na(birth_year), birth_year, random_birth_year_int),
    
    
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
    
    zip_code    = as.integer(zip_code(n = dplyr::n(), k = dplyr::n()))
   
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
    zip_code_w      = zip_code,
    ssn_fake_w      = ssn_fake,
    
    name_middle_missing = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.40)),
    name_maiden_missing = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.40)),
    
    gender_swapped      = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.20)),
    gender_missing      = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.40)),

    ssn_typo            = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.20)),
    ssn_missing         = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.50)),
    
    ethnicity_swapped   = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.20)),
    ethnicity_missing   = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.20)),
    
    dob_increment       = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.30)),
    dob_missing         = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.30)),
    
    zip_typo            = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.30)),
    zip_missing         = as.logical(rbinom(n = dplyr::n(), size = 1, prob = 0.30)),
    
    name_middle_w       = dplyr::if_else(name_middle_missing, NA_character_, name_middle_w),
    name_maiden_w       = dplyr::if_else(name_maiden_missing, NA_character_, name_maiden_w),
    
    gender_w      = dplyr::case_when(
      !gender_swapped ~ gender_w,
      gender_swapped & gender_w == "Male"   ~ "Female",
      gender_swapped & gender_w == "Female" ~ "Male",
    ),
    
    gender_w      = dplyr::if_else(gender_missing , NA_character_ , gender_w  ),
    
    ssn_fake_w    = dplyr::if_else(ssn_missing    , NA_character_ , ssn_fake_w),
    
    ssn_fake_w    = dplyr::if_else(ssn_typo, paste(area_3, stringr::str_pad((group+1) , 2, pad = "0"), serial_4, sep = "-"), ssn_fake_w),
    # ssn_fake_w    = dplyr::if_else(ssn_typo, paste(dsq2$area_3, stringr::str_pad((dsq2$group+1) , 2, pad = "0"), dsq2$serial_4, sep = "-"), ssn_fake_w),
    
    ethnicity_w   = dplyr::case_when(
      !ethnicity_swapped                ~ ethnicity_w,
      ethnicity_w == "African American" ~ "Asian",
      ethnicity_w == "Asian"            ~ "Hispanic",
      ethnicity_w == "Hispanic"         ~ "Caucasian",
      ethnicity_w == "Caucasian"        ~ "Native American",
      ethnicity_w == "Native American"  ~ "African American"
    ),
    
    ethnicity_w = dplyr::if_else(ethnicity_missing, NA_character_, ethnicity_w),
    
    dob_w       = dplyr::if_else(!dob_increment, dob_w, as.Date(paste(birth_year+1, birth_month, birth_day, sep = "-"))),
    dob_w       = dplyr::if_else(!dob_missing, dob_w, as.Date(NA)),
    
    zip_code_w  = dplyr::if_else(!zip_typo   , zip_code_w, zip_code_w+1L),
    zip_code_w  = dplyr::if_else(!zip_missing, zip_code_w, NA_integer_)
    
  ) %>% 
  generate_nickname() %>% 
  dplyr::mutate(
    name_first_w       = name_first_nickname,

    gender_swapped     = dplyr::if_else(gender_missing,    FALSE, gender_swapped    ),
    ssn_typo           = dplyr::if_else(ssn_missing,       FALSE, ssn_typo          ),
    ethnicity_swapped  = dplyr::if_else(ethnicity_missing, FALSE, ethnicity_swapped ),
    dob_increment      = dplyr::if_else(dob_missing,       FALSE, dob_increment     ),
    zip_typo           = dplyr::if_else(zip_missing,       FALSE, zip_typo          ),
    
    wrinkle_count      = name_middle_missing + name_maiden_missing + gender_swapped + gender_missing + ssn_typo + ssn_missing + ethnicity_swapped  + ethnicity_missing + dob_increment + dob_missing + zip_typo + zip_missing + nicknamed
  ) %>% 
  dplyr::select(
    
    rown_o,
    rown_o_w, 
    
    name_first,
    name_first_w,
    name_last,
    name_last_w,
    name_middle,
    name_middle_w,
    name_maiden,
    name_maiden_w,
    gender, 
    gender_w,
    ethnicity,
    ethnicity_w,
    dob,
    dob_w,
    zip_code,
    zip_code_w,
    ssn_fake,
    ssn_fake_w,
    
    
    name_middle_missing, 
    name_maiden_missing,
    gender_swapped,
    gender_missing,
    ssn_typo, 
    ssn_missing,
    ethnicity_swapped,
    ethnicity_missing,
    dob_increment, 
    dob_missing,
    zip_typo,
    zip_missing,
    name_first_nickname, 
    nicknamed,
    wrinkle_count,
    
  ) %>% 
  dplyr::mutate(
    # random_sample_int           = sample(c(1:nrow(ds_middleman)), size = nrow(ds_middleman), replace = FALSE),
    random_sample_int           = sample(c(1:dplyr::n()), size = dplyr::n(), replace = FALSE),
    grab_for_matching           = random_sample_int <= 40000,
    grab_for_sample_possible    = random_sample_int <= 3000,
    grab_for_sample_impossible  = 41000 < random_sample_int & random_sample_int <= 42000,
    
    dob   = format(dob  , "%Y%m%d"),
    dob_w = format(dob_w, "%Y%m%d"),
  )

ds_middleman %>%
  dplyr::count(
    grab_for_matching,
    grab_for_sample_possible,
    grab_for_sample_impossible,
  ) %>%
  View()

ds_matching <-
  ds_middleman %>% 
  dplyr::filter(grab_for_matching)

ds_sample <-
  ds_middleman %>% 
  dplyr::filter(grab_for_sample_possible | grab_for_sample_impossible)
 

# ---- select-columns-to-write ---------------------------------------------------------------
ds_matching_slim <-
  ds_matching %>% 
  dplyr::select(
    rown_o,
    name_first,
    name_last, 
    name_middle,
    name_maiden,
    gender,
    ethnicity,
    dob,
    zip_code,
    ssn_fake,
    
    wrinkle_count,
    name_middle_missing,
    name_maiden_missing,
    gender_swapped,
    gender_missing,
    ssn_typo,
    ssn_missing,
    ethnicity_swapped,
    ethnicity_missing,
    dob_increment,
    dob_missing,
    zip_typo,
    zip_missing,
    nicknamed,
    
  )

ds_sample_slim <-
  ds_sample %>% 
  dplyr::select(
    rown_o_w, 
    name_first_w, 
    name_last_w,
    name_middle_w,
    name_maiden_w,
    gender_w, 
    ethnicity_w,
    dob_w,
    zip_code_w,
    ssn_fake_w, 
    
    wrinkle_count,
    name_middle_missing,
    name_maiden_missing,
    gender_swapped,
    gender_missing,
    ssn_typo,
    ssn_missing,
    ethnicity_swapped,
    ethnicity_missing,
    dob_increment,
    dob_missing,
    zip_typo,
    zip_missing,
    nicknamed,
    
    grab_for_sample_possible,
    grab_for_sample_impossible,
  )


# ---- write-data ---------------------------------------------------------------
readr::write_csv(ds_matching_slim, path = path_out_matching, na = "")
readr::write_csv(ds_sample_slim  , path = path_out_linking , na = "")

