### PREPROCESSING

# Load libraries.
library(httr)       
library(jsonlite)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(opendatascot)
library(tidyr)
library(lubridate)
library(purrr)
library(PostcodesioR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(forcats)
library(scales)
library(tibble)
library(brms)
library(INLA)
library(loo)
library(bayesplot)
library(performance)
library(lme4)
library(rlang)

## DATA IDENTIFICATION

# Initialize path to historical population CSV. 
# Note: This is the only dataset I couldn't get to work using the API, so the URL
# will need to be changed for any future users.
datazone_path <- "/Users/cyrusseyrafi/Desktop/Public Health Scotland/c505f490-c201-44bd-abd1-1bd7a64285ee.csv"

# Initialize 'General Practitioner Contact Details' API IDs by quarter.
gp_contacts_ids <- c(
  "2018-10" = "e37c14fe-51f7-4935-87d1-c79b30fe8824",
  "2019-01" = "afd3653b-103d-4a64-b42e-3ce727eb4e6c",
  "2019-04" = "d03319a8-d058-406f-bc92-b8faab4ed8a5",
  "2019-07" = "9147d685-578e-495b-a642-60a1afb0f5bc",
  "2019-10" = "b9c31baf-74ce-4331-af72-8d7b473dfc8c",
  "2020-01" = "b092b69f-0838-408e-bb89-082562f0e1cd",
  "2020-04" = "be6aea98-20d8-4112-9b56-c437b3c651e5",
  "2020-07" = "398ed9d8-790d-4469-b7e1-2e6d5d12f882",
  "2020-10" = "1347fe0c-919d-4573-bee8-6edf54e0896b",
  "2021-01" = "4d1deea9-b096-49a3-934a-7d4828cc9188",
  "2021-04" = "5b348d90-e76e-4660-b49e-1026a3c2f57f",
  "2021-07" = "b34ecefc-f383-4968-aaf7-06f71ecb0cca",
  "2021-10" = "3bc1c732-96c8-42f2-81c8-3ba7defe6ef0",
  "2022-01" = "614b821b-402d-466a-a1c9-2e791d4482c8",
  "2022-04" = "692397e6-53de-41e0-b735-a46803defd59",
  "2022-07" = "36c54711-b8f4-4f67-851a-0bfc3769da5f",
  "2022-10" = "fb48a65c-25fa-4001-9afb-6110d761daf2",
  "2023-01" = "4630768d-72d0-4d09-ab51-31b93b8a4423",
  "2023-04" = "2d122ac0-a632-4329-95a3-a4b4f0834c30",
  "2023-07" = "87e9355c-f357-4198-b9af-26d9f362228a",
  "2023-10" = "d5796895-754f-410f-bb08-ba0fe25e38fe",
  "2024-01" = "ac9178e7-f067-4c6f-b336-3121b4f4354f",
  "2024-04" = "647b256e-4a03-4963-8402-bf559c9e2fff",
  "2024-07" = "bb4e0731-868f-4999-95e4-af4a7d2f9073",
  "2024-10" = "37c3741d-04dd-4e24-9358-d4c7a3c479bc",
  "2025-01" = "f2e3a351-1aaf-421b-8f47-6e9765c7cc0c",
  "2025-04" = "bdd87ee5-268b-49b6-8f8a-7b376aae78b3",
  "2025-07" = "6b551359-93fb-4491-9860-e371c23f5b9f"
)

# Initialize 'GP Practice Contact Details and List Sizes' IDs by quarter.
gp_practices_ids <- c(
  "2015-01" = "ea7cb2c6-3e64-4ca9-8023-4c6ac1e9757d",
  "2015-04" = "58f0fadc-6595-43bd-bfd4-30c5cc2ca387",
  "2015-07" = "b2cb043e-4f34-4eeb-a27e-33a350ae7a80",
  "2015-10" = "9e050d9a-440a-491a-b78a-b9c7795512ef",
  "2016-01" = "fca02075-963b-4556-8927-e4215e229ecc",
  "2016-04" = "a3241be3-6cac-4bfe-adf7-b7974de0b806",
  "2016-07" = "50293f13-f5ee-409c-9e35-16a81ff63a27",
  "2016-10" = "02d608dd-82b5-4f73-8bd0-f603f0ad6deb",
  "2017-01" = "11367b8e-a3da-4dfb-98e5-3e8acb878257",
  "2017-04" = "7b7f8361-eb7b-4e18-974c-a863205132e1",
  "2017-07" = "1bea9689-132a-4d69-97bc-1ca2abcfd0bc",
  "2017-10" = "f772f188-a1cb-4d07-9335-3c02dd55cd33",
  "2018-01" = "94834d4d-5527-4a8d-84fb-edabbb3b48f7",
  "2018-04" = "9dd8bbf8-a774-4726-8541-f37c8e6f2331",
  "2018-07" = "57c17aad-1454-4348-87f8-8a98e990339e",
  "2018-10" = "34f02dbe-2827-47ae-821f-d529e26075cd",
  "2019-01" = "aa9bb31e-781d-44c9-95c9-f0476526d9f2",
  "2019-04" = "204bf88e-a2e7-4e57-8515-66c2f4ee4c28",
  "2019-07" = "204bf88e-a2e7-4e57-8515-66c2f4ee4c28",
  "2019-10" = "c01dc5f3-86ea-4a3d-8e0c-1d29f04a85d7",
  "2020-01" = "a444ae58-9f8c-4447-90d8-5c8641171bb7",
  "2020-04" = "779053ce-75ce-4aa1-9f40-51b7f33efa2b",
  "2020-07" = "3a3bc00b-9780-4b97-8120-2d5da5c8a619",
  "2020-10" = "42391720-7dcb-48a2-8070-b9d63b246ac6",
  "2021-01" = "42fd7367-b61f-448c-a268-2a6192c7df8d",
  "2021-04" = "a794d603-95ab-4309-8c92-b48970478c14",
  "2021-07" = "ce260981-c217-4291-978e-9e6ba2171747",
  "2021-10" = "14ef935c-a00d-4d39-9f04-228911ca5d0c",
  "2022-01" = "1f76c338-7890-4ee7-b1bd-4d837cc1d50a",
  "2022-04" = "8175c9ac-6953-4636-b151-f3946ef0fb80",
  "2022-07" = "5273d444-5a79-4fad-a518-119a368e2161",
  "2022-10" = "1a15cb34-fcf9-4d3f-ad63-1ba3e675fbe2",
  "2023-01" = "993422a6-c64f-4c57-ba41-9279ad5a7c89",
  "2023-04" = "9c1dccc7-7632-4b13-b451-092bd57973a4",
  "2023-07" = "a160fa97-8a3c-429a-9683-3835cafe1701",
  "2023-10" = "6e2b279f-5b9e-41ff-971c-1819c3df8ba9",
  "2024-01" = "54a6e1e3-98a3-4e78-be0d-1e6d6ebdde1d",
  "2024-04" = "b3b126d3-3b0c-4856-b348-0b37f8286367",
  "2024-07" = "215e9157-ccf2-47ab-aba5-4fb79eb32e41",
  "2024-10" = "64b42f68-3353-449b-bc1c-0f733b21803b",
  "2025-01" = "0d2e258a-1451-4af1-a7e5-e8327994fa55",
  "2025-04" = "f3633113-9768-4676-8ed1-6695fe385bbe",
  "2025-07" = "30b06220-17ad-44e8-b6c5-658d41ec1ea5"
)

# Initialize 'GP Practice Population Demographics' IDs by quarter.
gp_populations_ids <- c(
  "2014-01" = "e692e29a-e428-495e-a123-575c2a17c937",
  "2014-04" = "eff91350-89a5-4135-bb15-1d83122d5baa",
  "2014-07" = "54601e83-f93f-4661-9acd-39ff0a1bb19c",
  "2014-10" = "8a9e2fac-2bc6-4b3e-9db4-e96790eea3da",
  "2015-01" = "f8663319-e169-4f10-bada-e095e9a20b67",
  "2015-04" = "2f8af19a-cecd-4abf-92c0-968807d5be6d",
  "2015-07" = "128ab5b8-60eb-4392-a322-974747592dce",
  "2015-10" = "05001225-4437-47bb-8754-8c83d240ca47",
  "2016-01" = "1e09ff5c-2b62-4010-adbf-e74436fb0a9a",
  "2016-04" = "b0819bd6-b11a-4e55-a3e8-c4fb1432ebc0",
  "2016-07" = "4e288e0c-90c4-4f9d-9ce9-efa5741f08ae",
  "2016-10" = "4f3d240a-49f0-42f8-9639-4ac70a076c48",
  "2017-01" = "beb945aa-6be5-4c02-b168-4389523ad8ce",
  "2017-04" = "049eae64-92dd-47fd-bcc8-dafabd9dc864",
  "2017-07" = "e7340098-23df-4732-90eb-ce29b9f32cd8",
  "2017-10" = "751a40c9-8e90-433b-bed3-2eae238bd248",
  "2018-01" = "356eb0c9-3f73-4eab-80bf-7c816118f7b2",
  "2018-04" = "42db71e6-c7a6-4454-8bb2-d110da6c93e2",
  "2018-07" = "0a306292-cf96-4d89-96b8-33804174db3c",
  "2018-10" = "32a2ab8f-c02d-4657-abae-3900bdced1ef",
  "2019-01" = "a843237b-83a0-442a-8777-46743998cd9c",
  "2019-04" = "558f1c91-33a2-42fd-af0f-0066d6b0e4c1",
  "2019-07" = "538d2526-1fc9-431a-9efc-fb1008e76442",
  "2019-10" = "8f439ba4-fe7e-425b-a82c-44c5e7a04632",
  "2020-01" = "8e33187b-5284-4eb9-afd9-b229bb4109e2",
  "2020-04" = "34539f10-62fc-4b3c-8691-1f8f023da601",
  "2020-07" = "2167f804-5d7d-49ac-8724-24ada0fbafe8",
  "2020-10" = "571f5278-d6a5-4051-84c8-c01d688aa3ea",
  "2021-01" = "01534271-609c-4ca9-81d1-9f81dbc32de0",
  "2021-04" = "14584b26-6a63-4baf-be37-e13826981e3c",
  "2021-07" = "0779e100-1aaf-4e43-8536-57c8b99ca710",
  "2021-10" = "4a3c438b-2885-49e8-b7ed-c45cb44e1253",
  "2022-01" = "d07debcf-7832-4dc4-afb2-41101d5cc7ff",
  "2022-04" = "2c701f90-c26d-4963-8062-95b8611e5fd1",
  "2022-07" = "64918d4f-f1d9-4e99-8e9f-130ddc890748",
  "2022-10" = "2c7dcb55-c83a-44bd-9128-47d1d6f339ba",
  "2023-01" = "8921c5c1-ec7e-4c62-b55d-b73c3d086e3f",
  "2023-04" = "4b9aa55f-b425-4a4f-8610-4c1c87b46c2e",
  "2023-07" = "d7f423dd-9611-4ae9-a9c8-4dcc532ece22",
  "2023-10" = "ab677c28-f495-4191-83e1-aaa0c3e6a9b4",
  "2024-01" = "488685e9-95ff-4a48-b085-af50e1dc1863",
  "2024-04" = "3306ab5a-cd22-494a-be76-ee6753cef92d",
  "2024-07" = "81c6fd6a-6837-4fe2-8f4b-bd42293804df",
  "2024-10" = "cec9341e-ccba-4c71-afe4-a614f5e97b9f",
  "2025-01" = "ce967fa9-0f7f-4389-9f19-4958c45e43c2",
  "2025-04" = "571f5b52-7cae-4ca9-9867-72dc7e39dbad",
  "2025-07" = "9cb891c1-e886-4470-b06c-9a8cf6bf4b22"
)

# Initialize 2025-edition 'General practice - disease prevalence data visualisation' IDs by year.
gp_diseases_new_ids <- list(
  Male = list(
    url = "https://www.publichealthscotland.scot/media/33610/diseaseprevalenceingeneralpractice_practice_m.xlsx",
    sheet = "DP_Practice_M"
  ),
  Female = list(
    url = "https://www.publichealthscotland.scot/media/33609/diseaseprevalenceingeneralpractice_practice_f.xlsx",
    sheet = "DP_Practice_F"
  ),
  All = list(
    url = "https://www.publichealthscotland.scot/media/33611/diseaseprevalenceingeneralpractice_practice_total.xlsx",
    sheet = "DP_Practice_total"
  )
)

# Initialize 2024-edition 'General practice - disease prevalence data visualisation' IDs by year.
gp_diseases_old_ids <- list(
  Male = list(
    url = "https://www.publichealthscotland.scot/media/30622/diseaseprevalence_practice_m.xlsx",
    sheet = "DP_Practice_M"
  ),
  Female = list(
    url = "https://www.publichealthscotland.scot/media/30621/diseaseprevalence_practice_f.xlsx",
    sheet = "DP_Practice_F"
  ),
  All = list(
    url = "https://www.publichealthscotland.scot/media/30623/diseaseprevalence_practice_total.xlsx",
    sheet = "DP_Practice_total"
  )
)

# Initialize 'Data Zone 2011' ID from Public Health Scotland.
geography_id <- "395476ab-0720-4740-be07-ff4467141352"


# Initialize 'SIMD 2020v2' ID from Public Health Scotland.
simd_id <- "acade396-8430-4b34-895a-b3e757fa346e"

# Initialize 'Data Zone Centroids' ID from National Records Scotland.
datazone_coordinates_id <- "https://maps.gov.scot/ATOM/shapefiles/SG_DataZoneCent_2011.zip"


## DATA LOADING AND CLEANING

# download is a general function for loading datasets from Public Health Scotland's
# public dataset API.

# Parameters:
# - dataset: string, the CKAN resource ID for the dataset.
# - url: string, the CKAN API endpoint.
# - batch_size: number of rows per API call.
# - verbose: if TRUE, prints progress messages.
# - remove_id: if TRUE, removes the '_id' column automatically.

download <- function(dataset,
                     url = "https://www.opendata.nhs.scot/api/3/action/datastore_search",
                     batch_size = 1000,
                     verbose = TRUE,
                     remove_id = TRUE) {

  # Initialize variables for offset, dataset acquired, and rows surveyed.
  offset <- 0
  running_data <- list()
  running_row_count <- 0
  
  # Loop through batch_size rows of the dataset, adjusted by offset:
  repeat {
    # Construct dataset URL, adjusted by batch size and offset.
    dataset_url <- paste0(url,
                          "?resource_id=", dataset,
                          "&limit=", batch_size,
                          "&offset=", offset)
    
    if (verbose) {
      message("Fetching rows ", offset + 1, " to ", offset + batch_size, "...")
    }
    
    # Call data from API.
    response <- GET(dataset_url)
    if (status_code(response) != 200) {
      stop("API request failed at offset ", offset, 
           " (HTTP ", status_code(response), ")")
    }
    
    # Parse and extract JSON records.
    json_data <- fromJSON(content(response, "text"), flatten = TRUE)
    records <- json_data$result$records
    
    # Stop if complete.
    if (length(records) == 0) {
      if (verbose) message("No more records to fetch. Download complete.")
      break
    }
    
    # Convert data to dataframe.
    data <- as.data.frame(records)
    
    # Remove the '_id' index column.
    if (remove_id && "_id" %in% names(data)) {
      data[["_id"]] <- NULL
    }
    
    # Add to running dataset.
    running_data[[length(running_data) + 1]] <- data
    running_row_count <- running_row_count + nrow(data)
    
    # Adjust offset fro next loop.
    offset <- offset + batch_size
  }
  
  # Combine data chunks collected.
  full_data <- do.call(rbind, running_data)
  
  if (verbose) {
    message("Final dataset contains ", nrow(full_data), " rows.")
  }
  
  # Return full dataset.
  return(full_data)
}


# get_gp_contacts downloads 'General Practitioner Contact Details' from the
# PHS API for each quarter.

# Parameters:
# - gp_contacts_ids: vector, API keys for download.

get_gp_contacts <- function(gp_contacts_ids) {
  message("Downloading 'General Practitioner Contact Details' from Public Health Scotland")
  bind_rows(
    # For each quarter:
    lapply(names(gp_contacts_ids), function(quarter) {
      message("Quarter: ", quarter)
      # Load the dataset.
      df <- download(dataset = gp_contacts_ids[[quarter]], verbose = FALSE) %>%
        # Add a quarter column.
        mutate(quarter = quarter) %>%
        # Standardize telephone column.
        rename(Telephone = any_of("TelephoneNumber")) %>%
        # Standardize practice name.
        rename(GPPracticeName = any_of("PracticeName")) %>%
        # Remove SexQF column.
        select(-any_of("SexQF")) %>%
        # Add NA values if GP designation is unavailable.
        mutate(GPDesignation = if (!"GPDesignation" %in% names(.)) NA_character_ else GPDesignation)
      # Return the dataset.
      return(df)
    })
  ) %>%
    mutate(
      # Put quarter in data format.
      quarter = as.Date(paste0(quarter, "-01")),
    )
}

gp_contacts <- get_gp_contacts(gp_contacts_ids)

# get_gp_practices downloads 'GP Practice Contact Details and List Sizes' from 
# the PHS API for each quarter.

# Parameters:
# - gp_practices_ids: vector, API keys for download.

get_gp_practices <- function(gp_practices_ids) {
  message("Downloading 'GP Practice Contact Details and List Sizes' from Public Health Scotland")
  bind_rows(
    # For each quarter:
    lapply(names(gp_practices_ids), function(quarter) {
      message("Quarter: ", quarter)
      # Load the dataset.
      df <- download(dataset = gp_practices_ids[[quarter]], verbose = FALSE) %>%
        # Add a quarter column.
        mutate(quarter = quarter) %>%
        # Rename Listsize → PracticeListSize if needed.
        rename(PracticeListSize = any_of("Listsize")) %>%
        # Convert to numeric (silently handle character cases).
        mutate(PracticeListSize = suppressWarnings(as.numeric(PracticeListSize))) %>%
        # Standardize telephone column.
        rename(TelephoneNumber = any_of("Telephone")) %>%
        # Standardize practice name.
        rename(GPPracticeName = any_of("PracticeName")) %>%
        # Remove unused column variants.
        select(-any_of(c("Telephone", "PracticeName")))
      
      # Return the dataset.
      return(df)
    })
  ) %>%
  # Put quarter in date format.
  mutate(quarter = as.Date(paste0(quarter, "-01")))
}

gp_practices <- get_gp_practices(gp_practices_ids)

# get_gp_populations downloads the 'GP Practice Contact Details and List Sizes' from 
# the PHS API for each quarter.

# Parameters:
# - gp_practices_ids: vector, API keys for download.

get_gp_populations <- function(gp_populations_ids) {
  message("Downloading 'GP Practice Population Demographics' from Public Health Scotland")
  bind_rows(
    # For each quarter:
    lapply(names(gp_populations_ids), function(quarter) {
      message("Downloading: ", quarter)
      # Load the dataset.
      df <- download(dataset = gp_populations_ids[[quarter]], verbose = FALSE) %>%
        # Add a quarter column.
        mutate(quarter = quarter)
      # Return the dataset.
      return(df)
    })
  ) %>%
    # Put quarter in date format.
    mutate(quarter = as.Date(paste0(quarter, "-01")))
}

gp_populations <- get_gp_populations(gp_populations_ids)

# get_gp_diseases_new downloads the 2025 edition of 
# 'General practice - disease prevalence data visualisation' 
# from the PHS Publications page for each year from 2022 to 2025.

# Parameters:
# - gp_diseases_new_ids: list, URLs and sheets for download.

get_gp_diseases_new <- function(gp_diseases_new_ids) {
  message("Downloading 08 July 2025-version of 'General practice - disease prevalence data visualisation' from Public Health Scotland")
  
  # Download and clean a single Excel file.
  read_and_clean <- function(url, sheet) {
    # Download the file.
    tmp <- tempfile(fileext = ".xlsx")
    GET(url, write_disk(tmp, overwrite = TRUE))
    
    # Read and clean the data.
    read_excel(tmp, sheet = sheet) %>%
      # Split GPPractice/Area into GPPractice and Area.
      mutate(
        GPPractice = sub(" - .*", "", `GPPractice/Area`),
        Area = sub(".* - ", "", `GPPractice/Area`)
      ) %>%
      # Drop the original combined column.
      select(GPPractice, Area, everything(), -`GPPractice/Area`) %>%
      # Drop unused AreaType column.
      select(-any_of("AreaType"))
  }
  
  # Load and bind all datasets.
  wide <- bind_rows(lapply(gp_diseases_new_ids, function(src) {
    read_and_clean(src$url, src$sheet)
  }))
  
  # Identify ID columns.
  id_cols <- c("PracticeCode", "Area", "Year", "Age", "Sex")
  
  # Identify disease-related columns.
  value_cols <- names(wide) %>%
    str_subset("^(PatientCount_|Rate_)")
  
  # Extract unique disease names.
  diseases <- value_cols %>%
    str_remove("^(PatientCount_|Rate_)") %>%
    unique()
  
  # Reshape to long format: one row per disease per practice.
  reshaped <- bind_rows(compact(lapply(diseases, function(disease) {
    # Construct expected disease-specific column names.
    pc_col <- paste0("PatientCount_", disease)
    rate_col <- paste0("Rate_", disease)
    
    # Skip if none of the columns exist.
    if (!any(c(pc_col, rate_col) %in% names(wide))) return(NULL)
    
    # Select and rename relevant columns.
    wide %>%
      select(any_of(c(id_cols, pc_col, rate_col))) %>%
      rename(
        patient_count = !!sym(pc_col),
        rate = !!sym(rate_col)
      ) %>%
      # Add disease name as a column.
      mutate(disease = disease)
  })))
  
  # Reorder columns for output.
  reshaped %>%
    select(
      PracticeCode,
      Area,
      Year,
      Age,
      Sex,
      disease,
      patient_count,
      rate
    )
}

gp_diseases_new <- get_gp_diseases_new(gp_diseases_new_ids)
  
# get_gp_diseases_new downloads the 2024 edition of 
# 'General practice - disease prevalence data visualisation' 
# from the PHS Publications page for each year from 2018 to 2022

# Parameters:
# - gp_diseases_old_ids: list, URLs and sheets for download.

get_gp_diseases_old <- function(gp_diseases_old_ids) {
  # Define dataset URLs and sheet names for each sex category.
  message("Downloading 03 December 2024-version of 'General practice - disease prevalence data visualisation' from Public Health Scotland")
  
  # Download and clean a single Excel file.
  read_and_clean <- function(url, sheet) {
    # Download the file.
    tmp <- tempfile(fileext = ".xlsx")
    GET(url, write_disk(tmp, overwrite = TRUE))
    
    # Read and clean the data.
    read_excel(tmp, sheet = sheet) %>%
      # Split GPPractice/Area into GPPractice and Area.
      mutate(
        GPPractice = sub(" - .*", "", `GPPractice/Area`),
        Area = sub(".* - ", "", `GPPractice/Area`)
      ) %>%
      # Drop the original combined column.
      select(GPPractice, Area, everything(), -`GPPractice/Area`) %>%
      # Drop unused AreaType column.
      select(-any_of("AreaType"))
  }
  
  # Load and bind all datasets.
  wide <- bind_rows(lapply(gp_diseases_old_ids, function(src) {
    read_and_clean(src$url, src$sheet)
  }))
  
  # Identify ID columns.
  id_cols <- c("PracticeCode", "Area", "Year", "Age", "Sex")
  
  # Identify disease-related columns.
  value_cols <- names(wide) %>%
    str_subset("^(PatientCount_|Rate_|Change_)")
  
  # Extract unique disease names.
  diseases <- value_cols %>%
    str_remove("^(PatientCount_|Rate_|Change_)") %>%
    unique()
  
  # Reshape to long format: one row per disease per practice.
  reshaped <- bind_rows(compact(lapply(diseases, function(disease) {
    # Construct expected disease-specific column names.
    pc_col <- paste0("PatientCount_", disease)
    rate_col <- paste0("Rate_", disease)
    change_col <- paste0("Change_", disease)
    
    # Skip if none of the columns exist.
    if (!any(c(pc_col, rate_col, change_col) %in% names(wide))) return(NULL)
    
    # Select and rename relevant columns.
    wide %>%
      select(any_of(c(id_cols, pc_col, rate_col, change_col))) %>%
      rename(
        patient_count = !!sym(pc_col),
        rate = !!sym(rate_col),
        change = !!sym(change_col)
      ) %>%
      # Add disease name as a column.
      mutate(disease = disease)
  })))
  
  # Reorder columns for output.
  reshaped %>%
    select(
      PracticeCode,
      Area,
      Year,
      Age,
      Sex,
      disease,
      patient_count,
      rate,
      change
    )
}

gp_diseases_old <- get_gp_diseases_old(gp_diseases_old_ids)

# get_geography downloads 'Data Zone 2011' from the PHS API for each datazone in
# Scotland.

# Parameters:
# - geography_id: string, API key for download.

get_geography <- function(geography_id) {
  message("Downloading 'Data Zone 2011' from Public Health Scotland")
  geography <- download(geography_id) %>%
    distinct()
  return(geography)
}

geography <- get_geography(geography_id)

# get_geography downloads 'Data Zone 2011' from the PHS API for each datazone in
# Scotland.

# Parameters:
# - geography_id: string, API key for download.

get_simd <- function(simd_id) {
  message("Downloading 'SIMD 2020v2' from Public Health Scotland")
  simd <- download(simd_id)
  return(simd)
}

simd <- get_simd(simd_id)

# Population Index.

# 'get_population' downloads population data from the following sources:
# 1. historical data from 2014 to 2022 at the data zone, health and social care
# partnership, health board, and council area levels;
# 2. 2018-based projections up to 2043 at the country and council area levels; and
# 3. 2022-based projections up to 2043 at the country level;
# to engineer an approximate data zone-level (as well as HSCP, HB, and CA-level) 
# population dataset from 2014 to 2023, by age bracket and sex.

# The methodology is as fellows:
# - The historical data is used from 2014 to 2022.
# - The 2018-based projections are used to determine the proportion of the Scottish
#   population in each council area and health board from 2023 to 2043.
# - These proportions are applied to the 2022-based projections to disaggregate
#   them into council areas and health boards.
# - The disaggregated projections are re-weighted to match the national totals from the
#   2022-based projections, preserving age-sex distributions.
# - Relative weights are computed against 2022 values for each area-age-sex-variant combination.
# - These weights are then applied to 2022 historical population at the data zone level to 
#   project population forward to 2043 at the data zone level.
# - Data zone projections are aggregated to the HSCP level, and historical data zone
#   population is similarly aggregated to create a consistent HSCP time series from 2014–2043.
# - All projections (council, health board, HSCP, and data zone) are then assembled into a
#   single dataset including low, high, and principal variant estimates, by age bracket and sex.
# - Low and high variants are NA for historical values.

get_population <- function(datazone_path = datazone_path) {
  message("Beginning population pipeline...")
  
  # Extract distinct key-name pairs from a lookup table.
  safe_lookup <- function(df, key, name) {
    df %>%
      select(all_of(c(key, name))) %>%
      distinct() %>%
      filter(!is.na(.data[[key]]))
  }
  
  # Join area names and types to a dataset using geography codes.
  add_area_labels <- function(df, geography_df) {
    df %>%
      left_join(safe_lookup(geography_df, "CA", "CAName"), by = c("refArea" = "CA")) %>%
      mutate(
        Area_Name = ifelse(!is.na(CAName), CAName, NA_character_),
        Area_Type = ifelse(!is.na(CAName), "Council", NA_character_)
      ) %>%
      left_join(safe_lookup(geography_df, "HB", "HBName"), by = c("refArea" = "HB")) %>%
      mutate(
        Area_Name = ifelse(is.na(Area_Name) & !is.na(HBName), HBName, Area_Name),
        Area_Type = ifelse(is.na(Area_Type) & !is.na(HBName), "Health Board", Area_Type),
        Area_Name = ifelse(refArea == "S92000003", "Scotland", Area_Name),
        Area_Type = ifelse(refArea == "S92000003", "Country", Area_Type)
      ) %>%
      select(-CAName, -HBName)
  }
  
  # Initialize 2018-based projections by year and sex.
  message("Downloading 2018-based projections by year and sex...")
  
  # Define all sex-year combinations for 2018-based projections.
  ref_years <- 2018:2043
  sexes <- c("all", "male", "female")
  combo_grid <- expand_grid(sex = sexes, year = ref_years)
  
  # Downloads one sex-year combination of the 2018-based projections. 
  get_projection_2018 <- possibly(function(sex, year) {
    message("2018-based projection: ", year, " / ", sex)
    ods_dataset(
      "population-projections-2018-based",
      measureType = "count",
      refPeriod = as.character(year),
      populationProjectionVariant = "principal-projection",
      sex = sex
    ) %>%
      mutate(refPeriod = year, sex = sex)
  }, otherwise = NULL)
  
  # Loan data into the dataset.
  pop_2018 <- pmap_dfr(combo_grid, get_projection_2018)
  
  # Add area labels and standardize age brackets.
  pop_2018 <- pop_2018 %>%
    add_area_labels(geography) %>%
    mutate(age_standardized = case_when(
      age == "working-age-16-64" ~ "working-age",
      age == "pensionable-age-65-and-over" ~ "pension-age",
      age == "90-years-and-over" ~ "90-years-and-over",
      TRUE ~ age
    ))
  
  # Initialize 2022-basd projections.
  message("Downloading 2022-based projections...")
  
  # Extract three projection variants: principal, low, and high (for error).
  pop_2022 <- ods_dataset(
    "population-projections-2022-based",
    populationProjectionVariant = c("principal-projection", "population-low", "population-high")
  )


  message("Reweighting national totals...")
  
  # Extract national totals by age x sex x year x variant.
  shared_years <- 2022:2043
  variants <- unique(pop_2022$populationProjectionVariant)
  
  nat_totals <- pop_2022 %>%
    filter(refArea == "S92000003", refPeriod %in% shared_years) %>%
    mutate(age_standardized = case_when(
      age %in% c("90-94-years", "95-99-years", "100-104-years", "105-years-and-over") ~ "90-years-and-over",
      age == "working-age" ~ "working-age",
      age == "pension-age" ~ "pension-age",
      TRUE ~ age
    )) %>%
    group_by(refPeriod, age_standardized, sex, measureType, populationProjectionVariant) %>%
    summarise(value = sum(value), .groups = "drop")
  
  # Initialize all area type x variant combinations.
  combinations <- expand_grid(
    variant = variants,
    area_type = c("Council", "Health Board")
  )
  
  # Initialize function to disaggregate national projections to council areas
  # and health boards using the 2018-based proportions.
  disaggregate_variant <- function(variant, area_type) {
    message("Disaggregating ", variant, " to ", area_type, ".")
    
    nat_sub <- nat_totals %>%
      filter(populationProjectionVariant == variant)
    
    prop_df <- pop_2018 %>%
      filter(refPeriod %in% shared_years,
             Area_Type == area_type,
             refArea != "S92000003") %>%
      group_by(refPeriod, age_standardized, sex, refArea) %>%
      summarise(region_value = sum(value), .groups = "drop") %>%
      group_by(refPeriod, age_standardized, sex) %>%
      mutate(proportion = region_value / sum(region_value)) %>%
      ungroup()
    
    result <- prop_df %>%
      left_join(nat_sub %>% select(refPeriod, age_standardized, sex, value, measureType),
                by = c("refPeriod", "age_standardized", "sex")) %>%
      mutate(
        value = value * proportion,
        populationProjectionVariant = variant,
        age = age_standardized
      ) %>%
      select(refArea, refPeriod, measureType, populationProjectionVariant,
             sex, age, value)
    
    return(result)
  }
  
  # Disaggregate and combine all projections into one dataset.
  result_rows <- pmap(combinations, disaggregate_variant)
  result_all <- bind_rows(result_rows)
  
  # Add area names and types to disaggregated projections.
  named <- result_all %>%
    add_area_labels(geography)
  
  message("Computing relative weights to 2022...")
  
  # Compute ratio to 2022 value for each area x age x sex x variant.
  weights <- named %>%
    filter(refPeriod == 2022) %>%
    select(refArea, sex, age, populationProjectionVariant, base_value = value)
  
  # Initialize weight_2022 column.
  named <- named %>%
    left_join(weights, by = c("refArea", "sex", "age", "populationProjectionVariant")) %>%
    mutate(
      weight_2022 = ifelse(!is.na(base_value) & base_value > 0,
                           value / base_value, NA_real_)
    ) %>%
    select(-base_value) %>%
    relocate(weight_2022, .after = value)
  
  message("Downloading historical populations...")

  # Download and standardize historical population data.  
  load_historical_population <- function(datazone_path) {
    pop_dz   <- read_csv(datazone_path) %>% select(-"_id")
    pop_ca   <- download("09ebfefb-33f4-4f6a-8312-2d14e2b02ace")
    pop_hscp <- download("c3a393ce-253b-4c75-82dc-06b1bb5638a3")
    pop_hb   <- download("27a72cc8-d6d8-430c-8b4f-3109a9ceadb1")
    
    # Eliminate 'QF' columns.
    standardize <- function(df, key) {
      df %>%
        rename(Region = !!key, RegionQF = paste0(key, "QF")) %>%
        mutate(RegionType = key)
    }
    
    # Standardize columns by type.
    combined <- bind_rows(
      standardize(pop_dz, "DataZone"),
      standardize(pop_ca, "CA"),
      standardize(pop_hscp, "HSCP"),
      standardize(pop_hb, "HB")
    )
    
    # Extract age columns.
    age_cols <- grep("^Age\\d+$|^Age90plus$", names(combined), value = TRUE)
    
    # Aggregate into 5-year bands to match other preliminary datasets.
    collapse_ages <- function(data) {
      age_bands <- lapply(seq(0, 85, by = 5), function(start_age) {
        end_age <- start_age + 4
        cols <- paste0("Age", start_age:end_age)
        rowSums(data[cols])
      })
      
      age_90plus <- data$Age90plus
      age_data <- as.data.frame(
        setNames(c(age_bands, list(age_90plus)),
                 c(paste0("Age", seq(0, 85, 5), "-", seq(4, 89, 5)), "Age90plus")),
        check.names = FALSE
      )
      
      bind_cols(data %>% select(-all_of(age_cols)), age_data)
    }
    
    collapsed <- collapse_ages(combined)
    
    # Remove 'Scotland' as a whole country from the dataset.
    collapsed %>%
      mutate(row_id = row_number()) %>%
      group_by(Region, Year, Sex) %>%
      mutate(keep_flag = if_else(Region == "S92000003", row_number() == 1, NA)) %>%
      ungroup() %>%
      filter(!(Region == "S92000003" & is.na(keep_flag))) %>%
      mutate(RegionType = if_else(Region == "S92000003", "country", RegionType)) %>%
      select(-row_id, -keep_flag)
  }
  
  historical_population <- load_historical_population(datazone_path)
  dz_lookup <- geography %>% select(DataZone, DataZoneName, CA)
  
  # Map age columns to labels.
  age_map <- c(
    "Age0-4" = "0-4-years", "Age5-9" = "5-9-years", "Age10-14" = "10-14-years",
    "Age15-19" = "15-19-years", "Age20-24" = "20-24-years", "Age25-29" = "25-29-years",
    "Age30-34" = "30-34-years", "Age35-39" = "35-39-years", "Age40-44" = "40-44-years",
    "Age45-49" = "45-49-years", "Age50-54" = "50-54-years", "Age55-59" = "55-59-years",
    "Age60-64" = "60-64-years", "Age65-69" = "65-69-years", "Age70-74" = "70-74-years",
    "Age75-79" = "75-79-years", "Age80-84" = "80-84-years", "Age85-89" = "85-89-years",
    "Age90plus" = "90-years-and-over"
  )
  
  # Format historical age band data for all geographies.
  hist_bands <- historical_population %>%
    filter(Year >= 2014, Year <= 2022, RegionType %in% c("DataZone", "CA", "HB")) %>%
    select(Region, Year, Sex, RegionType, starts_with("Age")) %>%
    pivot_longer(cols = starts_with("Age"),
                 names_to = "age_hist", values_to = "value") %>%
    mutate(
      age = age_map[age_hist],
      refArea = Region,
      sex = tolower(Sex),
      refPeriod = Year,
      measureType = "count",
      populationProjectionVariant = "historical",
      weight_2022 = 1,
      Area_Type = case_when(
        RegionType == "CA" ~ "Council",
        RegionType == "HB" ~ "Health Board",
        RegionType == "DataZone" ~ "DataZone",
        Region == "S92000003" ~ "Country",
        TRUE ~ "Other"
      )
    ) %>%
    mutate(
      Area_Name = case_when(
        Area_Type == "Council" ~ geography$CAName[match(refArea, geography$CA)],
        Area_Type == "Health Board" ~ geography$HBName[match(refArea, geography$HB)],
        Area_Type == "DataZone" ~ geography$DataZoneName[match(refArea, geography$DataZone)],
        Area_Type == "Country" ~ "Scotland",
        TRUE ~ NA_character_
      )
    ) %>%
    select(refArea, Area_Name, Area_Type, refPeriod, measureType,
           populationProjectionVariant, sex, age, value, weight_2022)
  
  # Format historical totals (age == "all") for all geographies.
  hist_all <- historical_population %>%
    filter(Year >= 2014, Year <= 2022, RegionType %in% c("DataZone", "CA", "HB")) %>%
    transmute(
      refArea = Region,
      sex = tolower(Sex),
      age = "all",
      value = AllAges,
      refPeriod = Year,
      measureType = "count",
      populationProjectionVariant = "historical",
      weight_2022 = 1,
      Area_Type = case_when(
        RegionType == "CA" ~ "Council",
        RegionType == "HB" ~ "Health Board",
        RegionType == "DataZone" ~ "DataZone",
        Region == "S92000003" ~ "Country",
        TRUE ~ "Other"
      ),
      Area_Name = case_when(
        Area_Type == "Council" ~ geography$CAName[match(refArea, geography$CA)],
        Area_Type == "Health Board" ~ geography$HBName[match(refArea, geography$HB)],
        Area_Type == "DataZone" ~ geography$DataZoneName[match(refArea, geography$DataZone)],
        Area_Type == "Country" ~ "Scotland",
        TRUE ~ NA_character_
      )
    ) %>%
    select(refArea, Area_Name, Area_Type, refPeriod, measureType,
           populationProjectionVariant, sex, age, value, weight_2022)
  
  # Combine historical datasets.
  hist_combined <- bind_rows(hist_bands, hist_all) %>% distinct()

  message("Projecting DataZone values forward...")
  
  # Extract weights for projecting datazone populations from 2023 to 2043.
  council_weights <- named %>%
    filter(Area_Type == "Council", measureType == "count", refPeriod %in% 2023:2043) %>%
    select(council = refArea, refPeriod, sex, age, populationProjectionVariant, weight_2022)
  
  # Distribute council-level projection weights to datazones using 2022 shares.
  dz_future <- council_weights %>%
    left_join(dz_lookup %>% select(council = CA, refArea = DataZone, Area_Name = DataZoneName),
              by = "council", relationship = "many-to-many") %>%
    left_join(
      hist_combined %>%
        filter(refPeriod == 2022) %>%
        select(refArea, sex, age, base_2022 = value) %>%
        distinct(),
      by = c("refArea", "sex", "age")
    ) %>%
    mutate(
      Area_Type = "DataZone",
      measureType = "count",
      value = base_2022 * weight_2022
    ) %>%
    select(refArea, Area_Name, Area_Type, refPeriod, measureType,
           populationProjectionVariant, sex, age, value, weight_2022) %>%
    distinct()
  
  
  message("Aggregating population projections to HSCP level...")
  
  # Find the HSCP for each datazone.
  dz_to_hscp <- geography %>% select(DataZone, HSCP, HSCPName)
  
  # Aggregate forward projections from datazones to HSCPs.
  hscp_future <- dz_future %>%
    left_join(dz_to_hscp, by = c("refArea" = "DataZone")) %>%
    filter(!is.na(HSCP)) %>%
    group_by(HSCP, HSCPName, refPeriod, populationProjectionVariant, sex, age) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      region = HSCP,
      Area_Name = HSCPName,
      Area_Type = "Health and Social Care Partnership",
      measureType = "count",
      weight_2022 = 1
    ) %>%
    select(
      refArea = region, Area_Name, Area_Type, refPeriod, measureType,
      populationProjectionVariant, sex, age, value, weight_2022
    )
  
  message("Aggregating historical population to HSCP level...")
  
  # Aggregate historical populations from datazones to HSCPs.
  hscp_hist <- hist_combined %>%
    filter(Area_Type == "DataZone") %>%  # Start only from DataZone-level data
    left_join(dz_to_hscp, by = c("refArea" = "DataZone")) %>%
    filter(!is.na(HSCP)) %>%
    group_by(HSCP, HSCPName, refPeriod, populationProjectionVariant, sex, age) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      refArea = HSCP,
      Area_Name = HSCPName,
      Area_Type = "Health and Social Care Partnership",
      measureType = "count",
      weight_2022 = 1
    ) %>%
    select(refArea, Area_Name, Area_Type, refPeriod, measureType,
           populationProjectionVariant, sex, age, value, weight_2022)
  
  
  message("Combining projections...")
  
  # Combine all levels: CAs, HBs, HSCPs, DZs; historical and projected.
  base <- named
  
  pre_final_all <- bind_rows(base, hist_combined, dz_future, hscp_future, hscp_hist) %>%
    arrange(Area_Type, Area_Name, populationProjectionVariant, sex, age, refPeriod) %>%
    rename(region = refArea, year = refPeriod) %>%
    select(-measureType) %>%
    pivot_wider(
      names_from = populationProjectionVariant,
      values_from = c(value, weight_2022),
      names_glue = "{.value}_{populationProjectionVariant}"
    ) %>%
    # Exclude 2022 historicals since that's already known
      filter(!(year == 2022 & is.na(value_historical))) %>%
      select(-starts_with("weight_2022_")) %>%
    mutate(
      area_code = factor(region),
      sex = factor(sex),
      age = factor(age),
      area_name = factor(Area_Name),
      area_type = factor(Area_Type),
      population_low = `value_population-low`,
      population = coalesce(`value_principal-projection`, `value_historical`),
      population_high = `value_population-high`
    ) %>%
    select(area_code, area_name, area_type, year, age, sex, population_low, population, population_high) %>%
    mutate(
      population_low = if_else(year == 2022, NA_real_, population_low),
      population_high = if_else(year == 2022, NA_real_, population_high)
    ) %>%
    filter(!age %in% c("children-under-16-years", "working-age", "pension-age"))
  
  # Rename age brackets for consistency with other datasets.
  final_all <- pre_final_all %>% 
    mutate(
      age_bracket = case_when(
        age == "0-4-years" ~ "0 to 4",
        age == "5-9-years" ~ "5 to 9",
        age == "10-14-years" ~ "10 to 14",
        age == "15-19-years" ~ "15 to 19",
        age == "20-24-years" ~ "20 to 24",
        age == "25-29-years" ~ "25 to 29",
        age == "30-34-years" ~ "30 to 34",
        age == "35-39-years" ~ "35 to 39",
        age == "40-44-years" ~ "40 to 44",
        age == "45-49-years" ~ "45 to 49",
        age == "50-54-years" ~ "50 to 54",
        age == "55-59-years" ~ "55 to 59",
        age == "60-64-years" ~ "60 to 64",
        age == "65-69-years" ~ "65 to 69",
        age == "70-74-years" ~ "70 to 74",
        age == "75-79-years" ~ "75 to 79",
        age == "80-84-years" ~ "80 to 84",
        age == "85-89-years" ~ "85 to 89",
        age == "90-years-and-over" ~ "90 and over",
        age == "all" ~ "all",
        TRUE ~ NA_character_
      )
    ) %>%
    select(area_code, area_name, area_type, year, age_bracket, sex, population_low, population, population_high)
  
  return(final_all)

}

population <- get_population(datazone_path)


# 'get_practices' constructs a quarterly general practice dataset at the 
# GP practice level, combining:
# 
# 1. gp_contacts — number of GPs and gender breakdown;
# 2. gp_populations — patient counts by sex and age bracket;
# 3. gp_practices — geolocation, dispensing status, practice type;
# 4. simd — deprivation rankings at the data zone level.
#
# Each row corresponds to one sex-age combination for a practice in a given quarter.
#
# Additional fields include SIMD deciles, data zone, GP count, proportion male, 
# and flags for missing subcomponents (contacts/patient population).
#
# Coordinates are projected into the British National Grid (EPSG:27700).

get_practices <- function() {
  
  # Summarize gp_contacts by practice and quarter.
  gp_contacts_clean <- gp_contacts %>%
    mutate(
      # Convert reporting quarter to date.
      quarter = as.Date(quarter),
      # Standardize sex coding.
      Sex = case_when(
        tolower(Sex) %in% c("m", "male") ~ "M",
        tolower(Sex) %in% c("f", "female") ~ "F",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(PracticeCode, quarter) %>%
    summarise(
      gp_count = n_distinct(GeneralMedicalCouncilNumber),
      gp_male = sum(Sex == "M", na.rm = TRUE),
      gp_proportion_men = ifelse(gp_count > 0, gp_male / gp_count, NA_real_),
      .groups = "drop"
    )

  # Pivot gp_populations to long format by age, sex, and practice.
  gp_populations_long <- gp_populations %>%
    filter(Sex %in% c("Male", "Female", "All")) %>%
    pivot_longer(
      cols = c(
        Ages0to4, Ages5to14, Ages15to24, Ages25to44,
        Ages45to64, Ages65to74, Ages75to84, Ages85plus, AllAges
      ),
      names_to = "age_bracket",
      values_to = "patient_count"
    ) %>%
    mutate(
      # Standardize age bracket labels.
      age_bracket = case_when(
        age_bracket == "Ages0to4" ~ "0 to 4",
        age_bracket == "Ages5to14" ~ "5 to 14",
        age_bracket == "Ages15to24" ~ "15 to 24",
        age_bracket == "Ages25to44" ~ "25 to 44",
        age_bracket == "Ages45to64" ~ "45 to 64",
        age_bracket == "Ages65to74" ~ "65 to 74",
        age_bracket == "Ages75to84" ~ "75 to 84",
        age_bracket == "Ages85plus" ~ "85 and over",
        age_bracket == "AllAges" ~ "all",
        TRUE ~ NA_character_
      ),
      sex = tolower(Sex),
      quarter = as.Date(quarter)
    ) %>%
    select(PracticeCode, quarter, sex, age_bracket, patient_count)
  
  # Clean gp_practices and derive spatial and SIMD features.
  structure_df <- gp_practices %>%
    mutate(
      quarter = as.Date(quarter),
      practice_name = str_to_upper(GPPracticeName),
      datazone_code = DataZone,
      dispenses_medicine = case_when(
        Dispensing == "Y" ~ 1,
        Dispensing == "N" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      postcode_clean = str_remove_all(Postcode, "[:space:]")
    ) %>%
    rowwise() %>%
    mutate(
      # Try live postcodes; if that fails, try terminated postcodes.
      latitude = {
        tryCatch(
          postcode_lookup(postcode_clean)$latitude,
          error = function(e) {
            tryCatch(terminated_postcode(postcode_clean)$latitude, error = function(e) NA_real_)
          }
        )
      },
      longitude = {
        tryCatch(
          postcode_lookup(postcode_clean)$longitude,
          error = function(e) {
            tryCatch(terminated_postcode(postcode_clean)$longitude, error = function(e) NA_real_)
          }
        )
      }
    ) %>%
    ungroup()
  
  # After computing latitude/longitude:
  structure_df <- gp_practices %>%
    mutate(
      quarter = as.Date(quarter),
      practice_name = str_to_upper(GPPracticeName),
      datazone_code = DataZone,
      dispenses_medicine = case_when(
        Dispensing == "Y" ~ 1,
        Dispensing == "N" ~ 0,
        TRUE ~ NA_real_
      ),
      postcode_clean = str_remove_all(Postcode, "[:space:]")
    ) %>%
    rowwise() %>%
    mutate(
      latitude = {
        suppressMessages( tryCatch(
          postcode_lookup(postcode_clean)$latitude,
          error = function(e) tryCatch(terminated_postcode(postcode_clean)$latitude,
                                       error = function(e) NA_real_)
        ))
      },
      longitude = {
        suppressMessages( tryCatch(
          postcode_lookup(postcode_clean)$longitude,
          error = function(e) tryCatch(terminated_postcode(postcode_clean)$longitude,
                                       error = function(e) NA_real_)
        ))
      }
    ) %>%
    ungroup()
  
  # Split into rows with/without coordinates.
  with_xy  <- structure_df %>% filter(is.finite(longitude) & is.finite(latitude))
  no_xy    <- structure_df %>% filter(!is.finite(longitude) | !is.finite(latitude))
  
  # Convert only the valid ones to sf and back, then recombine.
  with_xy_en <- with_xy %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    st_transform(27700) %>%
    mutate(
      easting  = st_coordinates(.)[,1],
      northing = st_coordinates(.)[,2]
    ) %>%
    st_drop_geometry()
  
  no_xy_en <- no_xy %>%
    mutate(easting = NA_real_, northing = NA_real_)
  
  # Add in SIMD data.
  structure_df <- bind_rows(with_xy_en, no_xy_en) %>%
    select(
      practice_code = PracticeCode,
      quarter,
      practice_name,
      practice_type = PracticeType,
      datazone_code,
      dispenses_medicine,
      easting, northing
    ) %>%
    left_join(
      simd %>% select(DataZone, SIMD2020V2Rank, SIMD2020V2CountryDecile, SIMD2020V2CADecile),
      by = c("datazone_code" = "DataZone")
    ) %>%
    rename(
      simd_rank = SIMD2020V2Rank,
      simd_decile = SIMD2020V2CountryDecile,
      simd_council_decile = SIMD2020V2CADecile
    )
  
  
  
  # Merge practice structure with contacts and populations.
  structure_df %>%
    left_join(gp_contacts_clean, by = c("practice_code" = "PracticeCode", "quarter")) %>%
    left_join(gp_populations_long, by = c("practice_code" = "PracticeCode", "quarter")) %>%
    mutate(
      missing_data = case_when(
        is.na(patient_count) & is.na(gp_count) ~ "gp_contacts,gp_populations",
        is.na(patient_count) ~ "gp_populations",
        is.na(gp_count) ~ "gp_contacts",
        TRUE ~ NA_character_
      ),
      practice_code = as.character(practice_code)
    ) %>%
    select(
      quarter,
      practice_code,
      practice_name,
      practice_type,
      easting,
      northing,
      datazone_code,
      simd_rank,
      simd_decile,
      simd_council_decile,
      dispenses_medicine,
      gp_count,
      gp_proportion_men,
      sex,
      age_bracket,
      patient_count,
      missing_data
    ) %>%
    arrange(practice_code, quarter, sex, age_bracket)
  
}

practices <- get_practices()



# get_diseases standardizes disease prevalence records, estimates practice-year
# age-band populations, and joins practice + SIMD + GP staffing metadata.
#
# Parameters:
# - df_diseases: data.frame, long-format disease prevalence (per sex × age × year × practice).
# - precise: logical, if TRUE reconstruct patient_count from gp_populations using disease-derived
#            weights (fine-grained). If FALSE, use practices’ patient_count at coarse age bands.

get_diseases <- function(df_diseases, precise = FALSE) {
  message("Initializing disease dataset...")
  # standardize and reshape disease data.
  diseases_base <- df_diseases %>%
    mutate(
      # Harmonize age bands to practice/population schema.
      age_bracket = case_when(
        Age == "00-4"   ~ "0 to 4",
        Age == "05-9"   ~ "5 to 9",
        Age == "10-14"  ~ "10 to 14",
        Age == "15-19"  ~ "15 to 19",
        Age == "20-24"  ~ "20 to 24",
        Age == "25-29"  ~ "25 to 29",
        Age == "30-34"  ~ "30 to 34",
        Age == "35-39"  ~ "35 to 39",
        Age == "40-44"  ~ "40 to 44",
        Age == "45-49"  ~ "45 to 49",
        Age == "50-54"  ~ "50 to 54",
        Age == "55-59"  ~ "55 to 59",
        Age == "60-64"  ~ "60 to 64",
        Age == "65-69"  ~ "65 to 69",
        Age == "70-74"  ~ "70 to 74",
        Age == "75-79"  ~ "75 to 79",
        Age == "80-84"  ~ "80 to 84",
        Age == "85plus" ~ "85 and over",
        Age == "All"    ~ "all",
        TRUE ~ NA_character_
      ),
      # Map fine bands to practices’ coarse population brackets.
      pop_bracket = case_when(
        age_bracket %in% c("0 to 4") ~ "0 to 4",
        age_bracket %in% c("5 to 9", "10 to 14") ~ "5 to 14",
        age_bracket %in% c("15 to 19", "20 to 24") ~ "15 to 24",
        age_bracket %in% c("25 to 29", "30 to 34", "35 to 39", "40 to 44") ~ "25 to 44",
        age_bracket %in% c("45 to 49", "50 to 54", "55 to 59", "60 to 64") ~ "45 to 64",
        age_bracket %in% c("65 to 69", "70 to 74") ~ "65 to 74",
        age_bracket %in% c("75 to 79", "80 to 84") ~ "75 to 84",
        age_bracket == "85 and over" ~ "85 and over",
        TRUE ~ NA_character_
      ),
      # standardize sex labels.
      sex = case_when(
        tolower(Sex) %in% c("m", "male") ~ "male",
        tolower(Sex) %in% c("f", "female") ~ "female",
        tolower(Sex) == "all" ~ "all",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_bracket)) %>%
    rename(
      year = Year,
      practice_code = PracticeCode,
      disease_name = disease,
      disease_count = patient_count,
      disease_rate_per_100 = rate
    ) %>%
    mutate(
      practice_code = as.character(practice_code),
      # Crude population estimate from count and rate (per 100).
      raw_est = ifelse(disease_rate_per_100 > 0,
                       disease_count / (disease_rate_per_100 / 100),
                       NA_real_)
    )
  
  # Branch precise vs. coarse.
  if (isTRUE(precise)) {
    # Compute within-pop_bracket weights across diseases.
    bracket_weights <- diseases_base %>%
      filter(age_bracket != "all") %>%
      group_by(practice_code, year, sex, pop_bracket, age_bracket) %>%
      summarise(
        n_valid = sum(!is.na(raw_est)),
        weight_raw = mean(raw_est, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(practice_code, year, sex, pop_bracket) %>%
      mutate(
        n_total = n(),
        weight = case_when(
          n_valid == 0 & n_total == 1 ~ 1,
          n_valid == 0 ~ NA_real_,
          TRUE ~ weight_raw / sum(weight_raw, na.rm = TRUE)
        )
      ) %>%
      ungroup() %>%
      select(practice_code, year, sex, pop_bracket, age_bracket, weight)
    
    # Reshape gp_populations to yearly totals per pop_bracket × sex.
    gp_pop_long <- gp_populations %>%
      filter(Sex %in% c("Male", "Female", "All")) %>%
      mutate(
        sex = case_when(
          tolower(Sex) == "male" ~ "male",
          tolower(Sex) == "female" ~ "female",
          tolower(Sex) == "all" ~ "all",
          TRUE ~ NA_character_
        ),
        year = lubridate::year(as.Date(as.character(Date), format = "%Y%m%d")),
        practice_code = as.character(PracticeCode)
      ) %>%
      select(
        practice_code, year, sex,
        quarter = Date,
        `0 to 4` = Ages0to4,
        `5 to 14` = Ages5to14,
        `15 to 24` = Ages15to24,
        `25 to 44` = Ages25to44,
        `45 to 64` = Ages45to64,
        `65 to 74` = Ages65to74,
        `75 to 84` = Ages75to84,
        `85 and over` = Ages85plus
      ) %>%
      pivot_longer(
        cols = `0 to 4`:`85 and over`,
        names_to = "pop_bracket",
        values_to = "pop_count"
      ) %>%
      group_by(practice_code, year, sex, pop_bracket) %>%
      summarise(pop_year_total = mean(pop_count, na.rm = TRUE), .groups = "drop")
    
    # Apply weights to distribute population to age_bracket.
    gp_pop_est <- gp_pop_long %>%
      left_join(bracket_weights, by = c("practice_code", "year", "sex", "pop_bracket")) %>%
      mutate(patient_count = weight * pop_year_total) %>%
      select(practice_code, year, sex, age_bracket, patient_count)
    
    # Get totals ("all") directly from gp_populations.
    gp_pop_all <- gp_populations %>%
      filter(Sex %in% c("Male", "Female", "All")) %>%
      mutate(
        sex = case_when(
          tolower(Sex) == "male" ~ "male",
          tolower(Sex) == "female" ~ "female",
          tolower(Sex) == "all" ~ "all",
          TRUE ~ NA_character_
        ),
        year = lubridate::year(as.Date(as.character(Date), format = "%Y%m%d")),
        practice_code = as.character(PracticeCode)
      ) %>%
      group_by(practice_code, year, sex) %>%
      summarise(patient_count = mean(AllAges, na.rm = TRUE), .groups = "drop") %>%
      mutate(age_bracket = "all")
    
    gp_pop_all <- bind_rows(gp_pop_est, gp_pop_all)
    
    # Attach estimated populations to disease rows.
    diseases_joined <- diseases_base %>%
      left_join(gp_pop_all, by = c("practice_code", "year", "sex", "age_bracket")) %>%
      # Keep original disease_rate_per_100 (from source).
      identity()
    
  } else {
    # Aggregate practices to yearly patient_count by coarse age_bracket.
    practices_year_pop <- practices %>%
      filter(!is.na(age_bracket)) %>%
      mutate(
        year = lubridate::year(quarter),
        practice_code = as.character(practice_code)
      ) %>%
      group_by(practice_code, year, sex, age_bracket) %>%
      summarise(patient_count = mean(patient_count, na.rm = TRUE), .groups = "drop")
    
    # Collapse diseases’ fine bands into practices’ coarse age_brackets.
    diseases_coarse <- diseases_base %>%
      mutate(
        # For totals, keep "all"; otherwise use pop_bracket as the new age_bracket.
        age_bracket_coarse = if_else(age_bracket == "all", "all", pop_bracket)
      ) %>%
      group_by(practice_code, year, sex, disease_name, age_bracket_coarse) %>%
      summarise(
        disease_count = sum(disease_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(age_bracket = age_bracket_coarse)
    
    # Join practices’ patient_count and recompute disease_rate_per_100.
    diseases_joined <- diseases_coarse %>%
      left_join(practices_year_pop, by = c("practice_code", "year", "sex", "age_bracket")) %>%
      mutate(
        disease_rate_per_100 = if_else(
          is.finite(patient_count) & patient_count > 0,
          (disease_count / patient_count) * 100,
          NA_real_
        )
      ) %>%
      # Restore columns expected downstream; fill unavailable source fields with NA.
      mutate(
        disease_count = disease_count,
        disease_rate_per_100 = disease_rate_per_100
      )
  }
  
  # Get practice metadata.
  practice_info <- gp_practices %>%
    group_by(PracticeCode) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(
      practice_code = PracticeCode,
      practice_name = GPPracticeName,
      practice_type = PracticeType,
      datazone_code = DataZone
    ) %>%
    mutate(
      practice_name = toupper(practice_name),
      practice_code = as.character(practice_code)
    )
  
  # Get SIMD metadata (by datazone).
  simd_info <- simd %>%
    select(
      datazone_code = DataZone,
      simd_rank = SIMD2020V2Rank,
      simd_decile = SIMD2020V2CountryDecile,
      simd_council_decile = SIMD2020V2CADecile
    )
  
  # Get GP staffing summary by practice-year.
  gp_summary <- practices %>%
    mutate(practice_code = as.character(practice_code)) %>%
    group_by(practice_code, year = lubridate::year(quarter)) %>%
    summarise(
      gp_count = mean(gp_count, na.rm = TRUE),
      gp_proportion_men = mean(gp_proportion_men, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join metadata and construct completeness flags.
  diseases_joined %>%
    left_join(practice_info, by = "practice_code") %>%
    left_join(simd_info, by = "datazone_code") %>%
    left_join(gp_summary, by = c("practice_code", "year")) %>%
    mutate(
      # Flag missing upstream components per record.
      missing_data = pmap_chr(
        list(patient_count, practice_name, simd_rank, gp_count),
        ~ {
          missing <- c()
          if (is.na(..1)) missing <- c(missing, "gp_populations/practices")
          if (is.na(..2)) missing <- c(missing, "gp_practices")
          if (is.na(..3)) missing <- c(missing, "simd")
          if (is.na(..4)) missing <- c(missing, "gp_contacts")
          if (length(missing) == 0) NA_character_ else paste(missing, collapse = ", ")
        }
      )
    ) %>%
    select(
      year,
      practice_code,
      practice_name,
      practice_type,
      datazone_code,
      simd_rank,
      simd_decile,
      simd_council_decile,
      gp_count,
      gp_proportion_men,
      sex,
      age_bracket,
      patient_count,
      disease_name,
      disease_count,
      disease_rate_per_100,
      missing_data
    )
}

diseases_new <- get_diseases(gp_diseases_new)
diseases_old <- get_diseases(gp_diseases_old)



# 'get_areas' builds an areas dataset at the Data Zone, Council, HSCP, and Health Board levels
# for years 2015–2025, by sex and (coarse) age bracket. It combines:
# 1) population (historical + projections) — aggregated to coarse brackets for comparability;
# 2) practices — GP counts, % men, patient counts, and number of practices per DZ/year/sex/age;
# 3) SIMD — national rank and decile;
# 4) coordinates — Data Zone 2011 centroids (E/N) to provide locations and population-weighted
#    centroids for higher geographies;
# 5) diseases — patient counts, names, and rates per 100 by year, area, sex, and age bracket.

get_areas <- function() {
  
  # Initialize helper function to download + read Data Zone 2011 centroids and 
  # return (area_code, easting, northing). Kept local for clean namespace.
  get_coordinates <- function(datazone_coordinates_id) {
    
    # Download the zipped shapefile to a predictable name so subsequent calls overwrite safely.
    download.file(datazone_coordinates_id, destfile = "DataZoneCentroids2011.zip", mode = "wb")
    
    # Unzip into a dedicated folder to avoid clutter and make listing the .shp deterministic.
    unzip("DataZoneCentroids2011.zip", exdir = "datazone_centroids_2011")
    
    # Find the (only) .shp in the extracted folder; read as sf. This is the centroid layer.
    shp_path <- list.files("datazone_centroids_2011", pattern = "\\.shp$", full.names = TRUE)
    datazone_coordinates <- st_read(shp_path)
    
    # standardize column names to downstream schema and keep only what is needed for joins.
    datazone_coordinates %>% rename(area_code = DataZone, easting = Easting, northing = Northing) %>%
      select(area_code, easting, northing)
  }
  
  # Load coordinates.
  coordinates <- get_coordinates(datazone_coordinates_id)
  
  # Filter population to the target window (2015–2025) to match practices’ coverage.
  areas_base <- population %>% filter(year >= 2015, year <= 2025)
  
  # Map “fine” population age bands to the “coarse” practice bands for alignment in joins/ratios.
  age_bracket_map <- tribble(
    ~age_bracket_fine,     ~age_bracket_coarse,
    "0 to 4",              "0 to 4",
    "5 to 9",              "5 to 14",
    "10 to 14",            "5 to 14",
    "15 to 19",            "15 to 24",
    "20 to 24",            "15 to 24",
    "25 to 29",            "25 to 44",
    "30 to 34",            "25 to 44",
    "35 to 39",            "25 to 44",
    "40 to 44",            "25 to 44",
    "45 to 49",            "45 to 64",
    "50 to 54",            "45 to 64",
    "55 to 59",            "45 to 64",
    "60 to 64",            "45 to 64",
    "65 to 69",            "65 to 74",
    "70 to 74",            "65 to 74",
    "75 to 79",            "75 to 84",
    "80 to 84",            "75 to 84",
    "85 to 89",            "85 and over",
    "90 and over",         "85 and over",
    "all",                 "all"
  )
  
  # Aggregate population from fine to coarse brackets to match practices’ age schema.
  areas_base_binned <- areas_base %>%
    left_join(age_bracket_map, by = c("age_bracket" = "age_bracket_fine")) %>%
    group_by(area_code, area_type, area_name, year, sex, age_bracket = age_bracket_coarse) %>%
    summarise(
      population_low = sum(population_low, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      population_high = sum(population_high, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarize practices to annual figures at dz × sex × coarse age bracket.
  # I use the mean over quarters for gp_count/patient_count since most are likely
  # the same quarter to quarter.
  practices_summary <- practices %>%
    mutate(year = year(quarter)) %>%
    group_by(year, datazone_code, sex, age_bracket) %>%
    summarise(
      gp_count = mean(gp_count, na.rm = TRUE),
      gp_proportion_men = mean(gp_proportion_men, na.rm = TRUE),
      patient_count = mean(patient_count, na.rm = TRUE),
      practices_count = n_distinct(practice_code),
      missing_data = paste(unique(na.omit(missing_data)), collapse = ","),
      .groups = "drop"
    )
  
  # Join annual practice summaries onto DZ population (coarse age bands only).
  areas_with_practices <- areas_base_binned %>%
    filter(area_type == "DataZone") %>%
    left_join(
      practices_summary,
      by = c(
        "area_code" = "datazone_code",
        "year", "sex", "age_bracket"
      )
    ) %>%
    mutate(area_type = "DataZone")
  
  # Compute per-1,000 population densities for GPs and practice footprint.
  areas_with_density <- areas_with_practices %>%
    mutate(
      gp_per_1000 = if_else(
        !is.na(population) & !is.na(gp_count) & population > 0,
        1000 * gp_count / population,
        NA_real_
      ),
      practices_per_1000 = if_else(
        !is.na(population) & !is.na(practices_count) & population > 0,
        1000 * practices_count / population,
        NA_real_
      )
    )
  
  # Prepare SIMD (DZ-level) for join; keep rank + national decile.
  simd_clean <- simd %>%
    select(
      area_code = DataZone,
      simd_rank = SIMD2020V2Rank,
      simd_decile = SIMD2020V2CountryDecile
    )
  
  # Attach SIMD to DZ rows and initialize SD placeholders (used later for rollups).
  areas_with_simd <- areas_with_density %>%
    left_join(simd_clean, by = "area_code") %>%
    mutate(
      simd_rank_sd = 0,
      simd_decile_sd = 0
    )
  
  # Join DZ centroid coordinates; these feed DZ rows and weighted rollups.
  areas_with_coords <- areas_with_simd %>%
    left_join(coordinates, by = "area_code")  
  
  # Finalize the Data Zone slice with the agreed columns/order.
  areas_datazone <- areas_with_coords %>%
    filter(area_type == "DataZone") %>%
    select(
      area_code, area_type, area_name, easting, northing, year, sex, age_bracket,
      population_low, population, population_high,
      gp_count, gp_proportion_men, patient_count, practices_count,
      gp_per_1000, practices_per_1000,
      simd_rank, simd_rank_sd, simd_decile, simd_decile_sd,
      missing_data
    )
  
  # Build a lookup to relate each DZ to its higher geographies (CA/HSCP/HB).
  dz_lookup <- geography %>%
    select(
      datazone_code = DataZone,
      council_code = CA, council_name = CAName,
      hscp_code = HSCP, hscp_name = HSCPName,
      hb_code = HB, hb_name = HBName
    )
  
  # Enrich the DZ rows with their higher-area codes/names for later aggregation.
  dz_enriched <- areas_datazone %>%
    left_join(dz_lookup, by = c("area_code" = "datazone_code"))
  
  # Initialize function to roll Data Zones up to a chosen higher geography.
  aggregate_up <- function(df, area_code_col, area_name_col, area_type_label) {
    df %>%
      group_by(
        year, sex, age_bracket,
        area_code = .data[[area_code_col]]
      ) %>%
      summarise(
        area_type = area_type_label,
        area_name = first(na.omit(.data[[area_name_col]])),
        gp_count = sum(gp_count, na.rm = TRUE),
        gp_proportion_men = mean(gp_proportion_men, na.rm = TRUE),
        patient_count = sum(patient_count, na.rm = TRUE),
        practices_count = sum(practices_count, na.rm = TRUE),
        simd_rank_mean = mean(simd_rank, na.rm = TRUE),
        simd_rank_sd = sd(simd_rank, na.rm = TRUE),
        simd_decile_mean = mean(simd_decile, na.rm = TRUE),
        simd_decile_sd = sd(simd_decile, na.rm = TRUE),
        missing_data = paste(unique(na.omit(missing_data)), collapse = ","),
        .groups = "drop"
      ) %>%
      # Rename SIMD summaries to match final schema.
      rename(
        simd_rank = simd_rank_mean,
        simd_decile = simd_decile_mean
      ) %>%
      # Join population-weighted centroids computed from the DZ inputs.
      left_join(
        df %>%
          filter(!is.na(population), population > 0) %>%
          group_by(
            year, sex, age_bracket,
            area_code = .data[[area_code_col]]
          ) %>%
          summarise(
            easting = weighted.mean(easting, population, na.rm = TRUE),
            northing = weighted.mean(northing, population, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c("year", "sex", "age_bracket", "area_code")
      )
  }
  
  # Apply aggregation to each higher geography tier.
  agg_ca <- aggregate_up(dz_enriched, "council_code", "council_name", "Council")
  agg_hscp <- aggregate_up(dz_enriched, "hscp_code", "hscp_name", "Health and Social Care Partnership")
  agg_hb <- aggregate_up(dz_enriched, "hb_code", "hb_name", "Health Board")
  
  # Pull the non-DZ population as the base for rollups.
  areas_rollup_base <- areas_base_binned %>%
    filter(area_type != "DataZone")
  
  # Merge CA rollups.
  areas_ca <- areas_rollup_base %>%
    filter(area_type == "Council") %>%
    left_join(agg_ca, by = c("area_code", "area_type", "area_name", "year", "sex", "age_bracket")) %>%
    mutate(
      gp_per_1000 = if_else(!is.na(population) & !is.na(gp_count) & population > 0,
                            1000 * gp_count / population, NA_real_),
      practices_per_1000 = if_else(!is.na(population) & !is.na(practices_count) & population > 0,
                                   1000 * practices_count / population, NA_real_)
    )
  
  # Merge HSCP rollups.
  areas_hscp <- areas_rollup_base %>%
    filter(area_type == "Health and Social Care Partnership") %>%
    left_join(agg_hscp, by = c("area_code", "area_type", "area_name", "year", "sex", "age_bracket")) %>%
    mutate(
      gp_per_1000 = if_else(!is.na(population) & !is.na(gp_count) & population > 0,
                            1000 * gp_count / population, NA_real_),
      practices_per_1000 = if_else(!is.na(population) & !is.na(practices_count) & population > 0,
                                   1000 * practices_count / population, NA_real_)
    )
  
  # Merge HB rollups.
  areas_hb <- areas_rollup_base %>%
    filter(area_type == "Health Board") %>%
    left_join(agg_hb, by = c("area_code", "area_type", "area_name", "year", "sex", "age_bracket")) %>%
    mutate(
      gp_per_1000 = if_else(!is.na(population) & !is.na(gp_count) & population > 0,
                            1000 * gp_count / population, NA_real_),
      practices_per_1000 = if_else(!is.na(population) & !is.na(practices_count) & population > 0,
                                   1000 * practices_count / population, NA_real_)
    )
  
  # Merge all datasets into one.
  areas <- bind_rows(
    areas_datazone,
    areas_ca,
    areas_hscp,
    areas_hb
  ) %>%
    mutate(
      population_low = if_else(year <= 2022, NA_real_, population_low),
      population_high = if_else(year <= 2022, NA_real_, population_high),
      missing_data = case_when(
        is.na(gp_count) & (is.na(missing_data) | missing_data == "") ~ "practices",
        is.na(gp_count) & !str_detect(missing_data, "\\bpractices\\b") ~ paste(missing_data, "practices", sep = ","),
        TRUE ~ missing_data
      )
    ) %>%
    select(
      area_code, area_type, area_name, easting, northing, year, sex, age_bracket,
      population_low, population, population_high,
      gp_count, gp_proportion_men, patient_count, practices_count,
      gp_per_1000, practices_per_1000,
      simd_rank, simd_rank_sd, simd_decile, simd_decile_sd,
      missing_data
    )
  
  # Aggregate to DataZone level across practices.
  disease_dz <- diseases %>%
    group_by(year, datazone_code, sex, age_bracket, disease_name) %>%
    summarise(
      disease_count   = sum(disease_count, na.rm = TRUE),
      denom_patients  = sum(patient_count,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      disease_rate_per_100 = if_else(denom_patients > 0,
                                     100 * disease_count / denom_patients,
                                     NA_real_),
      area_code = datazone_code,
      area_type = "DataZone"
    ) %>%
    select(area_code, area_type, year, sex, age_bracket, disease_name,
           disease_count, disease_rate_per_100)
  
  # Lookup table for higher geographies.
  geo_lookup <- geography %>%
    transmute(
      datazone_code = DataZone,
      council_code  = CA,
      hscp_code     = HSCP,
      hb_code       = HB
    )
  
  # Helper to roll up disease data.
  rollup_disease <- function(code_col, area_type_label) {
    diseases %>%
      left_join(geo_lookup, by = "datazone_code") %>%
      group_by(
        year,
        area_code = .data[[code_col]],
        sex, age_bracket, disease_name
      ) %>%
      summarise(
        disease_count  = sum(disease_count, na.rm = TRUE),
        denom_patients = sum(patient_count,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        disease_rate_per_100 = if_else(denom_patients > 0,
                                       100 * disease_count / denom_patients,
                                       NA_real_),
        area_type = area_type_label
      ) %>%
      select(area_code, area_type, year, sex, age_bracket, disease_name,
             disease_count, disease_rate_per_100)
  }
  
  # Apply rollups.
  disease_ca   <- rollup_disease("council_code", "Council")
  disease_hscp <- rollup_disease("hscp_code",    "Health and Social Care Partnership")
  disease_hb   <- rollup_disease("hb_code",      "Health Board")
  
  # Combine disease data for all areas.
  disease_all_areas <- bind_rows(disease_dz, disease_ca, disease_hscp, disease_hb)
  
  # Join disease data onto main areas table.
  areas_with_diseases <- areas %>%
    left_join(
      disease_all_areas,
      by = c("area_code", "area_type", "year", "sex", "age_bracket")
    )
  
  # Return the expanded dataset.
  areas_with_diseases
}

areas <- get_areas()

# Restore preliminary datasets.
depreliminarize <- function() {
  list2env(preliminary, envir = .GlobalEnv)
  rm(preliminary, envir = .GlobalEnv)
}

# Clean environment by putting preliminary datasets into preliminary.
if (exists("preliminary", envir = .GlobalEnv)) {
  depreliminarize()
}

# Tuck away preliminary datasets.
preliminarize <- function() {
  preliminary <<- list(
    gp_contacts          = gp_contacts,
    gp_populations       = gp_populations,
    gp_practices         = gp_practices,
    gp_diseases_new      = gp_diseases_new,
    gp_diseases_new_ids  = gp_diseases_new_ids,
    gp_diseases_old_ids  = gp_diseases_old_ids,
    gp_diseases_old      = gp_diseases_old,
    simd                 = simd,
    geography_id         = geography_id,
    gp_contacts_ids      = gp_contacts_ids,
    gp_populations_ids   = gp_populations_ids,
    gp_practices_ids     = gp_practices_ids,
    simd_id              = simd_id,
    datazone_coordinates_id = datazone_coordinates_id,
    datazone_path        = datazone_path
  )
  
  if (exists("diseases_new", envir = .GlobalEnv)) {
    diseases <<- diseases_new
    rm(diseases_new, envir = .GlobalEnv)
  }

  
  rm(
    gp_contacts,
    gp_populations,
    gp_practices,
    gp_diseases_new,
    gp_diseases_new_ids,
    gp_diseases_old_ids,
    gp_diseases_old,
    simd,
    geography_id,
    gp_contacts_ids,
    gp_populations_ids,
    gp_practices_ids,
    simd_id,
    datazone_coordinates_id,
    datazone_path,
    envir = .GlobalEnv
  )
}
  
preliminarize()



### EXPLORATORY DATA ANALYSIS

## Practices

# run_practices_eda runs EDA and tables on the `practices` dataset and returns
# various summaries and plots for exploratory data analysis.

run_practices_eda <- function(practices, geography, print_tables = TRUE) {
  
  ## SETUP
  
  # Validate input type for practices.
  stopifnot(is.data.frame(practices))
  
  # Initialize a theme for all plots.
  thm <- theme_minimal()
  
  # Identify available health board name column.
  hb_col <- character(0)
  if (!is.null(geography)) {
    hb_col <- intersect(
      names(geography),
      c("HBName","HB2019Name","HB2020Name","HBTName","HBRName")
    )
  }
  
  # Extract single-row slice per (practice, quarter) for sex == 'all' & age == 'all'.
  patient_all <- practices %>%
    filter(sex == "all", age_bracket == "all") %>%
    distinct(quarter, practice_code, .keep_all = TRUE)
  
  # Container for return value.
  out <- list()
  
  ## STRUCTURE
  
  # Get snapshot of dataset size, coverage, and uniqueness of keys.
  snap <- practices %>%
    summarise(
      n_rows       = n(),
      n_cols       = ncol(.),
      q_first      = min(quarter, na.rm = TRUE),
      q_last       = max(quarter, na.rm = TRUE),
      n_practices  = n_distinct(practice_code),
      n_datazones  = n_distinct(datazone_code)
    )
  if (print_tables) print(snap)
  out$structure$snapshot <- snap
  
  # Quarterly coverage: distinct practices per quarter.
  coverage_q <- practices %>%
    filter(!is.na(practice_code), !is.na(quarter)) %>%
    distinct(quarter, practice_code) %>%
    count(quarter, name = "n_practices")
  print(
    ggplot(coverage_q, aes(quarter, n_practices)) +
      geom_line() +
      labs(title = "Quarterly Practice Converage",
           x = "Quarter", y = "Distinct practices") +
      thm
  )
  out$structure$coverage_q <- coverage_q
  
  # Variable-wise missingness fraction (descending).
  na_frac <- tibble(
    var = names(practices),
    na_frac = vapply(practices, function(x) mean(is.na(x)), numeric(1))
  ) %>% arrange(desc(na_frac))
  if (print_tables) print(na_frac)
  print(
    ggplot(na_frac, aes(fct_reorder(var, na_frac), na_frac)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(title = "Proportion NA per Feature", x = NULL, y = "Missing (%)") +
      thm
  )
  out$structure$na_frac <- na_frac
  
  # Upstream missing-data tags (if present).
  if ("missing_data" %in% names(practices)) {
    missing_tag_counts <- practices %>%
      mutate(
        missing_tag = if_else(is.na(missing_data) | missing_data == "", "(none)", missing_data)
      ) %>%
      count(missing_tag, sort = TRUE)
    if (print_tables) print(utils::head(missing_tag_counts, 20))
    out$structure$missing_tag_counts <- missing_tag_counts
  }
  
  # Duplication check at key grain: (practice, quarter, sex, age_bracket).
  dup_check <- practices %>%
    count(practice_code, quarter, sex, age_bracket, name = "n_rows") %>%
    filter(n_rows > 1)
  if (nrow(dup_check) > 0) {
    message("WARNING: duplicated rows at (practice,quarter,sex,age_bracket). Showing first 20:")
    print(utils::head(dup_check, 20))
  } else {
    message("OK: no duplicates at (practice, quarter, sex, age_bracket).")
  }
  out$structure$dup_check <- dup_check
  
  # GP staffing reporting share by quarter.
  gp_cov_q <- practices %>%
    group_by(quarter) %>%
    summarise(
      n_practices     = n_distinct(practice_code),
      n_with_gp_count = n_distinct(practice_code[!is.na(gp_count)]),
      share_with_gp   = n_with_gp_count / n_practices,
      .groups = "drop"
    )
  print(
    ggplot(gp_cov_q, aes(quarter, share_with_gp)) +
      geom_line() +
      scale_y_continuous(labels = percent) +
      labs(title = "Proportin of Practices with GP Count Data",
           x = "Quarter", y = "Share with gp_count") +
      thm
  )
  out$structure$gp_cov_q <- gp_cov_q
  
  # GP count distribution (sex/age == 'all').
  gp_dist <- patient_all
  print(
    ggplot(gp_dist, aes(gp_count)) +
      geom_histogram(bins = 40) +
      labs(title = "GP Count Distribution across Practices",
           x = "GP count", y = "Practices") +
      thm
  )
  if ("gp_proportion_men" %in% names(gp_dist)) {
    print(
      ggplot(gp_dist, aes(gp_proportion_men)) +
        geom_histogram(bins = 40) +
        labs(title = "Proportion of Male GPs",
             x = "Proportion male", y = "Practices") +
        thm
    )
  }
  
  # Patient totals check: ALL ≈ Male + Female at (practice, quarter, age_bracket).
  pt_check <- practices %>%
    filter(age_bracket != "all") %>%
    select(practice_code, quarter, age_bracket, sex, patient_count) %>%
    tidyr::pivot_wider(names_from = sex, values_from = patient_count) %>%
    mutate(
      sum_mf = coalesce(male, 0) + coalesce(female, 0),
      diff   = all - sum_mf
    )
  pt_summary <- pt_check %>%
    summarise(
      n_total        = n(),
      n_with_all_mf  = sum(!is.na(all) & !is.na(male) & !is.na(female)),
      n_mismatch     = sum(!is.na(all) & !is.na(sum_mf) & abs(diff) > 1e-6),
      share_mismatch = n_mismatch / n_with_all_mf
    )
  if (print_tables) print(pt_summary)
  out$structure$pt_summary <- pt_summary
  
  # SIMD coverage + distribution (sex/age == 'all').
  simd_cov <- patient_all %>%
    summarise(
      n_practices     = n_distinct(practice_code),
      n_with_simd     = n_distinct(practice_code[!is.na(simd_rank)]),
      share_with_simd = n_with_simd / n_practices
    )
  if (print_tables) print(simd_cov)
  print(
    patient_all %>%
      distinct(practice_code, .keep_all = TRUE) %>%
      ggplot(aes(simd_rank)) +
      geom_histogram(bins = 40) +
      labs(title = "SIMD Rank Distribution (1 = most deprived, 9 = least deprived",
           x = "SIMD 2020v2 rank", y = "Practices") +
      thm
  )
  out$structure$simd_cov <- simd_cov
  
  # Coordinate coverage + scatter of locations (British National Grid).
  xy_cov <- patient_all %>%
    summarise(
      n_practices   = n_distinct(practice_code),
      n_with_xy     = n_distinct(practice_code[is.finite(easting) & is.finite(northing)]),
      share_with_xy = n_with_xy / n_practices
    )
  if (print_tables) print(xy_cov)
  print(
    patient_all %>%
      distinct(practice_code, .keep_all = TRUE) %>%
      ggplot(aes(easting, northing)) +
      geom_point(alpha = 0.4, size = 0.8) +
      labs(title = "Practice Locations (British National Grid)",
           x = "Easting", y = "Northing") +
      thm
  )
  out$structure$xy_cov <- xy_cov
  
  # Longitudinal: median GP count and practice count over time.
  gp_time <- patient_all %>%
    group_by(quarter) %>%
    summarise(
      n_practices     = n_distinct(practice_code),
      median_gp_count = median(gp_count, na.rm = TRUE),
      .groups = "drop"
    )
  print(
    ggplot(gp_time, aes(quarter, median_gp_count)) +
      geom_line() +
      labs(title = "Median GP Count per Practice by Quarter",
           x = "Quarter", y = "Median GP count") +
      thm
  )
  out$structure$gp_time <- gp_time
  
  # Total registered patients over time (aggregate).
  pt_time_total <- patient_all %>%
    group_by(quarter) %>%
    summarise(total_patients = sum(patient_count, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(pt_time_total, aes(quarter, total_patients)) +
      geom_line() +
      labs(title = "Total Patients across practices",
           x = "Quarter", y = "Patients") +
      thm
  )
  out$structure$pt_time_total <- pt_time_total
  
  # GP count boxplots.
  print(
    patient_all %>%
      ggplot(aes(y = gp_count, x = 1)) +
      geom_boxplot(outlier.alpha = 0.3) +
      coord_flip() +
      labs(title = "GP counts (sex = all, age = all)",
           x = NULL, y = "GP count") +
      thm
  )
  
  ## PATIENT DEMOGRAPHICS AND FOOTPRINT
  
  # Distribution of practice list size (sex/age == 'all').
  print(
    ggplot(patient_all, aes(patient_count)) +
      geom_histogram(bins = 50) +
      labs(title = "Total Patients per Practice Distribution",
           x = "Total patients", y = "Practices") +
      thm
  )
  
  # Median practice list size over time.
  pt_time_med <- patient_all %>%
    group_by(quarter) %>%
    summarise(median_patients = median(patient_count, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(pt_time_med, aes(quarter, median_patients)) +
      geom_line() +
      labs(title = "Median Practice List by Quarter",
           x = "Quarter", y = "Median patients") +
      thm
  )
  out$demographics$pt_time_med <- pt_time_med
  
  # Age structure (overall shares across practices; exclude 'all').
  age_struct <- practices %>%
    filter(sex == "all", age_bracket != "all") %>%
    group_by(age_bracket) %>%
    summarise(patients = sum(patient_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      share = patients / sum(patients),
      age_bracket = fct_reorder(age_bracket, patients)
    )
  if (print_tables) print(age_struct)
  print(
    ggplot(age_struct, aes(age_bracket, share)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(title = "Total patients by age bracket across Scotland",
           x = NULL, y = "Share of patients") +
      thm
  )
  
  # Age structure over time (share by quarter).
  age_time <- practices %>%
    filter(sex == "all", age_bracket != "all") %>%
    group_by(quarter, age_bracket) %>%
    summarise(patients = sum(patient_count, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(share = patients / sum(patients))
  print(
    ggplot(age_time, aes(quarter, share, colour = age_bracket)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = percent) +
      labs(title = "Age bracket Share of Total Patients by Quarter",
           x = "Quarter", y = "Share of total patients", colour = "Age bracket") +
      thm
  )
  out$demographics$age_struct <- age_struct
  out$demographics$age_time <- age_time
  
  # Sex structure (overall and over time; restrict to male/female).
  sex_struct <- practices %>%
    filter(age_bracket == "all", sex %in% c("male","female")) %>%
    group_by(sex) %>%
    summarise(patients = sum(patient_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(share = patients / sum(patients))
  if (print_tables) print(sex_struct)
  print(
    ggplot(sex_struct, aes(sex, share, fill = sex)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = percent) +
      labs(title = "Overall Sex Distribution of Registered Patients",
           x = NULL, y = "Share of patients") +
      thm
  )
  sex_time <- practices %>%
    filter(age_bracket == "all", sex %in% c("male","female")) %>%
    group_by(quarter, sex) %>%
    summarise(patients = sum(patient_count, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(share = patients / sum(patients))
  print(
    ggplot(sex_time, aes(quarter, share, colour = sex)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = percent) +
      labs(title = "Overall Sex Distribution of Registered Patients over Time",
           x = "Quarter", y = "Share of total patients", colour = "Sex") +
      thm
  )
  out$demographics$sex_struct <- sex_struct
  out$demographics$sex_time <- sex_time
  
  # Footprint by SIMD decile (practice counts).
  simd_footprint <- patient_all %>%
    group_by(simd_decile) %>%
    summarise(practices = n_distinct(practice_code), .groups = "drop")
  print(
    ggplot(simd_footprint, aes(factor(simd_decile), practices)) +
      geom_col() +
      labs(title = "Practice Count by SIMD decile",
           x = "SIMD decile (1 = most deprived)", y = "Number of practices") +
      thm
  )
  out$demographics$simd_footprint <- simd_footprint
  
  # Top councils by number of practices (if geography provided).
  if (!is.null(geography)) {
    practices_per_council <- practices %>%
      filter(sex == "all", age_bracket == "all") %>%
      left_join(geography %>% select(datazone_code = DataZone, CAName), by = "datazone_code") %>%
      group_by(CAName) %>%
      summarise(practices = n_distinct(practice_code), .groups = "drop") %>%
      arrange(desc(practices)) %>%
      slice_head(n = 15)
    print(
      ggplot(practices_per_council, aes(fct_reorder(CAName, practices), practices)) +
        geom_col() + coord_flip() +
        labs(title = "Top 15 council areas by number of practices",
             x = NULL, y = "Practices") +
        thm
    )
    out$demographics$practices_per_council <- practices_per_council
  }
  
  ### GP WORKLOAD & DEPRIVATION
  
  # Patients per GP at practice-quarter level; drop zero/NA gp_count.
  gp_patient_ratio <- patient_all %>%
    filter(!is.na(gp_count), gp_count > 0) %>%
    mutate(patients_per_gp = patient_count / gp_count)
  
  # Distribution of patients/GP (linear + log scales).
  print(
    ggplot(gp_patient_ratio, aes(patients_per_gp)) +
      geom_histogram(bins = 50) +
      labs(title = "Distribution of Patients per GP (practice-quarter)",
           x = "Patients per GP", y = "Practice-quarters") +
      thm
  )
  print(
    ggplot(gp_patient_ratio, aes(patients_per_gp)) +
      geom_histogram(bins = 50) +
      scale_x_log10() +
      labs(title = "Patients per GP (log scale)",
           x = "Patients per GP", y = "Practice-quarters") +
      thm
  )
  
  # Outliers: top 1% threshold and exemplars.
  outlier_threshold <- stats::quantile(gp_patient_ratio$patients_per_gp, 0.99, na.rm = TRUE)
  outlier_practices <- gp_patient_ratio %>%
    filter(patients_per_gp > outlier_threshold) %>%
    arrange(desc(patients_per_gp)) %>%
    select(practice_code, quarter, patient_count, gp_count, patients_per_gp)
  if (print_tables) print(utils::head(outlier_practices, 20))
  out$workload$outlier_threshold <- outlier_threshold
  out$workload$outlier_practices <- outlier_practices
  
  # SIMD interactions: patient-weighted and practice-level summaries.
  simd_patient_weighted <- patient_all %>%
    group_by(simd_decile) %>%
    summarise(total_patients = sum(patient_count, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(simd_patient_weighted, aes(factor(simd_decile), total_patients)) +
      geom_col() +
      labs(title = "Patient-Weighted SIMD decile Distribution",
           x = "SIMD decile (1 = most deprived)", y = "Total patients") +
      thm
  )
  list_by_simd <- patient_all %>%
    group_by(simd_decile) %>%
    summarise(median_list = median(patient_count, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(list_by_simd, aes(factor(simd_decile), median_list)) +
      geom_col() +
      labs(title = "Median Practice List Size by SIMD decile",
           x = "SIMD decile (1 = most deprived)", y = "Median patients") +
      thm
  )
  gp_by_simd <- gp_patient_ratio %>%
    group_by(simd_decile) %>%
    summarise(median_patients_per_gp = median(patients_per_gp, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(gp_by_simd, aes(factor(simd_decile), median_patients_per_gp)) +
      geom_col() +
      labs(title = "Median Patients per GP by SIMD decile",
           x = "SIMD decile (1 = most deprived)", y = "Median patients per GP") +
      thm
  )
  gp_time_by_simd <- gp_patient_ratio %>%
    group_by(quarter, simd_decile) %>%
    summarise(median_patients_per_gp = median(patients_per_gp, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(gp_time_by_simd, aes(quarter, median_patients_per_gp, colour = factor(simd_decile))) +
      geom_line() +
      labs(title = "Median Patients per GP over time by SIMD decile",
           x = "Quarter", y = "Median patients per GP", colour = "SIMD decile") +
      thm
  )
  out$workload$simd_patient_weighted <- simd_patient_weighted
  out$workload$list_by_simd <- list_by_simd
  out$workload$gp_by_simd <- gp_by_simd
  out$workload$gp_time_by_simd <- gp_time_by_simd
  
  ### GEOGRAPHIC SUMMARIES
  
  if (!is.null(geography)) {
    # Attach council and, if present, health board names to the panel.
    panel_geo <- patient_all %>%
      left_join(
        geography %>% select(datazone_code = DataZone, CAName, dplyr::any_of(hb_col)),
        by = "datazone_code"
      )
    
    # Council-level GP density vs deprivation.
    council_gp_supply <- panel_geo %>%
      filter(!is.na(gp_count), gp_count > 0) %>%
      group_by(CAName) %>%
      summarise(
        total_patients       = sum(patient_count, na.rm = TRUE),
        total_gps            = sum(gp_count, na.rm = TRUE),
        gp_per_1000_patients = (total_gps / total_patients) * 1000,
        median_simd_rank     = median(simd_rank, na.rm = TRUE),
        practices            = n_distinct(practice_code),
        .groups = "drop"
      )
    print(
      ggplot(council_gp_supply, aes(median_simd_rank, gp_per_1000_patients)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Council-level GP density vs Deprivation",
             x = "Median SIMD rank (low = more deprived)",
             y = "GPs per 1,000 registered patients") +
        thm
    )
    out$geography$council_gp_supply <- council_gp_supply
    
    # Highest-workload councils (median patients/GP).
    council_workload <- panel_geo %>%
      filter(!is.na(gp_count), gp_count > 0) %>%
      mutate(ppgp = patient_count / gp_count) %>%
      group_by(CAName) %>%
      summarise(
        median_patients_per_gp = median(ppgp, na.rm = TRUE),
        iqr_patients_per_gp    = IQR(ppgp, na.rm = TRUE),
        practices              = n_distinct(practice_code),
        .groups = "drop"
      )
    top12_councils <- council_workload %>%
      arrange(desc(median_patients_per_gp)) %>%
      slice_head(n = 12)
    print(
      ggplot(top12_councils,
             aes(reorder(CAName, median_patients_per_gp), median_patients_per_gp)) +
        geom_col() + coord_flip() +
        labs(title = "Highest Median Patients per GP — Councils (top 12)",
             x = NULL, y = "Median patients per GP") +
        thm
    )
    out$geography$council_workload <- council_workload
    out$geography$top12_councils <- top12_councils
    
    # Health board summaries (if a HB column exists in geography).
    if (length(hb_col) > 0) {
      hb_name <- hb_col[1]
      
      hb_gp_supply <- panel_geo %>%
        filter(!is.na(.data[[hb_name]]), !is.na(gp_count), gp_count > 0) %>%
        group_by(.data[[hb_name]]) %>%
        summarise(
          total_patients       = sum(patient_count, na.rm = TRUE),
          total_gps            = sum(gp_count, na.rm = TRUE),
          gp_per_1000_patients = (total_gps / total_patients) * 1000,
          median_simd_rank     = median(simd_rank, na.rm = TRUE),
          practices            = n_distinct(practice_code),
          .groups = "drop"
        ) %>%
        rename(HBName = 1)
      print(
        ggplot(hb_gp_supply, aes(median_simd_rank, gp_per_1000_patients, label = HBName)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          labs(title = "Health Board GP Density vs Deprivation",
               x = "Median SIMD rank (low = more deprived)",
               y = "GPs per 1,000 registered patients") +
          thm
      )
      
      hb_workload <- panel_geo %>%
        filter(!is.na(.data[[hb_name]]), !is.na(gp_count), gp_count > 0) %>%
        mutate(ppgp = patient_count / gp_count) %>%
        group_by(.data[[hb_name]]) %>%
        summarise(
          median_patients_per_gp = median(ppgp, na.rm = TRUE),
          iqr_patients_per_gp    = IQR(ppgp, na.rm = TRUE),
          practices              = n_distinct(practice_code),
          .groups = "drop"
        ) %>%
        rename(HBName = 1)
      print(
        ggplot(hb_workload, aes(reorder(HBName, median_patients_per_gp),
                                median_patients_per_gp)) +
          geom_col() + coord_flip() +
          labs(title = "Median Patients per GP by Health Board",
               x = NULL, y = "Median Patients per GP") +
          thm
      )
      out$geography$hb_gp_supply <- hb_gp_supply
      out$geography$hb_workload <- hb_workload
    }
    
    # Time trend: patients/GP for the largest councils by practice count.
    council_time <- panel_geo %>%
      filter(!is.na(gp_count), gp_count > 0) %>%
      mutate(ppgp = patient_count / gp_count) %>%
      group_by(quarter, CAName) %>%
      summarise(median_ppgp = median(ppgp, na.rm = TRUE), .groups = "drop")
    focus_councils <- council_gp_supply %>%
      arrange(desc(practices)) %>%
      slice_head(n = 9) %>%
      pull(CAName)
    print(
      council_time %>%
        filter(CAName %in% focus_councils) %>%
        ggplot(aes(quarter, median_ppgp)) +
        geom_line() +
        facet_wrap(~ CAName, ncol = 3, scales = "free_y") +
        labs(title = "Patients per GP over time",
             x = "Quarter", y = "Median patients per GP") +
        thm
    )
    out$geography$council_time <- council_time
  }
  
  # Return silently with all captured summaries.
  invisible(out)
}

## Diseases

# run_practices_eda runs EDA and tables on the `practices` dataset and returns
# various summaries and plots for exploratory data analysis.

run_diseases_eda <- function(diseases, geography, print_tables = TRUE, top_n_diseases = 8) {
  
  # Plot theme used across figures.
  thm <- theme_minimal()
  
  # Container for return value.
  out <- list()
  
  ## PREPROCESSING
  
  # Standard prevalence calculation once for all rows.
  diseases <- diseases %>%
    mutate(prev = coalesce(disease_rate_per_100, 100 * disease_count / patient_count))
  
  # Core subset: sex == 'all' & age == 'all'.
  all_all <- diseases %>% filter(sex == "all", age_bracket == "all")
  
  ## COMPLETENESS AND STRUCTURE
  
  # Snapshot of size, coverage, distinct keys.
  snap <- diseases %>%
    summarise(
      n_rows      = n(),
      n_cols      = ncol(.),
      year_min    = min(year, na.rm = TRUE),
      year_max    = max(year, na.rm = TRUE),
      n_practices = n_distinct(practice_code),
      n_diseases  = n_distinct(disease_name)
    )
  if (print_tables) print(snap)
  out$structure$snapshot <- snap
  
  # Missingness profile by variable.
  na_frac <- tibble(
    var = names(diseases),
    na_frac = vapply(diseases, function(x) mean(is.na(x)), numeric(1))
  ) %>% arrange(desc(na_frac))
  if (print_tables) print(na_frac)
  print(
    ggplot(na_frac, aes(fct_reorder(var, na_frac), na_frac)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(title = "NA fraction by variable", x = NULL, y = "Missing (%)") +
      thm
  )
  out$structure$na_frac <- na_frac
  
  # Duplicate check at (practice, year, sex, age_bracket, disease_name).
  dup_check <- diseases %>%
    count(practice_code, year, sex, age_bracket, disease_name, name = "n_rows") %>%
    filter(n_rows > 1)
  if (nrow(dup_check) > 0) {
    message("WARNING: duplicated rows — showing first 20")
    print(utils::head(dup_check, 20))
  } else {
    message("OK: no duplicates at (practice, year, sex, age_bracket, disease_name).")
  }
  out$structure$dup_check <- dup_check
  
  # Coverage by year (distinct practices).
  cov_y <- all_all %>%
    distinct(year, practice_code) %>%
    count(year, name = "n_practices")
  print(
    ggplot(cov_y, aes(year, n_practices)) +
      geom_line() +
      labs(title = "Practice Coverage by Year (sex = all, age = all)",
           x = "Year", y = "Distinct practices") +
      thm
  )
  out$structure$coverage_y <- cov_y
  
  # SIMD coverage (sex/age == 'all').
  simd_cov <- all_all %>%
    summarise(
      n_practices     = n_distinct(practice_code),
      n_with_simd     = n_distinct(practice_code[!is.na(simd_rank)]),
      share_with_simd = n_with_simd / n_practices
    )
  if (print_tables) print(simd_cov)
  out$structure$simd_cov <- simd_cov
  
  ## OVERALL BURDEN
  
  # Total cases and median prevalence by disease.
  agg <- all_all %>%
    group_by(disease_name) %>%
    summarise(
      total_cases = sum(disease_count, na.rm = TRUE),
      median_prev = median(prev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_cases))
  if (print_tables) print(agg)
  out$burden$totals <- agg
  
  # Cases bar chart.
  print(
    ggplot(agg, aes(reorder(disease_name, total_cases), total_cases)) +
      geom_col() + coord_flip() +
      labs(title = "Total Cases by Disease (all years, sex = all, age = all)",
           x = NULL, y = "Cases") +
      thm
  )
  
  # Practice-level prevalence distributions..
  print(
    ggplot(all_all, aes(prev)) +
      geom_histogram(bins = 50) +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Practice-level Prevalence Distribution",
           x = "Prevalence (%)", y = "Practice-years") +
      thm
  )
  
  # Select top N diseases for focused plots.
  top_diseases <- head(agg$disease_name, min(top_n_diseases, nrow(agg)))
  
  ## BASIC DISEASE PLOTS
  
  # By age (sex = all).
  age_grid <- diseases %>%
    filter(disease_name %in% top_diseases, sex == "all", age_bracket != "all")
  print(
    ggplot(age_grid, aes(age_bracket, prev)) +
      geom_boxplot(outlier.alpha = 0.2) +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Age Bracket (sex = all)",
           x = "Age bracket", y = "Prevalence (%)") +
      thm
  )
  
  # By sex (age = all).
  sex_grid <- diseases %>%
    filter(disease_name %in% top_diseases, age_bracket == "all", sex %in% c("male","female"))
  print(
    ggplot(sex_grid, aes(sex, prev)) +
      geom_boxplot(outlier.alpha = 0.2) +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Sex (age = all)", x = "Sex", y = "Prevalence (%)") +
      thm
  )
  
  # By year (sex = all, age = all).
  year_grid <- all_all %>% filter(disease_name %in% top_diseases)
  print(
    ggplot(year_grid, aes(factor(year), prev)) +
      geom_boxplot(outlier.alpha = 0.2) +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Year (sex = all, age = all)",
           x = "Year", y = "Prevalence (%)") +
      thm
  )
  
  ## TIME TRENDS
  
  # National age weights from patient counts (sex = all).
  nat_w <- diseases %>%
    filter(sex == "all", age_bracket != "all") %>%
    group_by(age_bracket) %>%
    summarise(w = sum(patient_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(w = w / sum(w))
  
  # Crude patient-weighted prevalence over time.
  nat_time <- all_all %>%
    filter(disease_name %in% top_diseases) %>%
    group_by(year, disease_name) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      .groups = "drop"
    )
  print(
    ggplot(nat_time, aes(year, prev_pw, colour = disease_name)) +
      geom_point() + geom_smooth(method = "lm", se = TRUE) +
      labs(title = "Crude Patient-Weighted Prevalence over Time",
           x = "Year", y = "Prevalence (%)") +
      thm
  )
  
  # Age-standardized trends.
  std_time <- diseases %>%
    filter(disease_name %in% top_diseases, sex == "all", age_bracket != "all") %>%
    group_by(disease_name, year, age_bracket) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(nat_w, by = "age_bracket") %>%
    group_by(disease_name, year) %>%
    summarise(prev_age_std = sum(prev_pw * w), .groups = "drop")
  print(
    ggplot(std_time, aes(year, prev_age_std, colour = disease_name)) +
      geom_point() + geom_smooth(method = "lm", se = TRUE) +
      labs(title = "Age-Standardized National Trends", x = "Year", y = "Prevalence (%)") +
      thm
  )
  
  ## DEPRIVATION
  
  # SIMD gradient (patient-weighted prevalence).
  simd_df <- all_all %>%
    filter(disease_name %in% top_diseases) %>%
    group_by(disease_name, simd_decile) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      .groups = "drop"
    )
  print(
    ggplot(simd_df, aes(simd_decile, prev_pw, colour = disease_name)) +
      geom_point() + geom_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = 1:10) +
      labs(title = "SIMD Gradient (Patient-Weighted Prevalence)",
           x = "SIMD Decile (1 = Most Deprived)", y = "Prevalence (%)") +
      thm
  )
  
  ## CROSS-DISEASE CORRELATIONS
  
  # Spearman correlation matrix of prevalence across selected diseases.
  wide <- all_all %>%
    filter(disease_name %in% top_diseases) %>%
    select(practice_code, year, disease_name, prev) %>%
    pivot_wider(names_from = disease_name, values_from = prev)
  mat  <- as.matrix(wide[, !(names(wide) %in% c("practice_code", "year"))])
  cmat <- suppressWarnings(cor(mat, use = "pairwise.complete.obs", method = "spearman"))
  cdf  <- as.data.frame(as.table(cmat)) %>% rename(x = Var1, y = Var2, rho = Freq)
  print(
    ggplot(cdf, aes(x, y, fill = rho)) +
      geom_tile() + scale_fill_gradient2(limits = c(-1, 1)) +
      coord_equal() +
      labs(title = "Cross-Disease Spearman Correlations", x = NULL, y = NULL, fill = "ρ") +
      thm
  )
  
  invisible(out)
  
  ## SIMD GAP TABLES
  
  # Age-standardized prevalence by SIMD decile (patient-weighted).
  std <- diseases %>%
    filter(sex == "all", age_bracket != "all", disease_name %in% top_diseases) %>%
    group_by(disease_name, simd_decile, year, age_bracket) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(nat_w, by = "age_bracket") %>%
    group_by(disease_name, simd_decile, year) %>%
    summarise(prev_age_std = sum(prev_pw * w, na.rm = TRUE), .groups = "drop")
  
  # SIMD1 − SIMD10 gap per year and disease.
  gap_tbl <- std %>%
    filter(simd_decile %in% c(1, 10)) %>%
    tidyr::pivot_wider(
      names_from = simd_decile, values_from = prev_age_std, names_prefix = "dec_"
    ) %>%
    mutate(gap_pp = dec_1 - dec_10) %>%
    arrange(disease_name, year)
  if (print_tables) print(gap_tbl)
  out$deprivation$gap_table <- gap_tbl
  
  # Gap over time.
  print(
    ggplot(gap_tbl, aes(year, gap_pp, colour = disease_name)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_line() + geom_point() +
      labs(title = "Age-Standardized Deprivation Gap",
           x = "Year", y = "Gap in prevalence (pp)") +
      thm
  )
  
  ## GEOGRAPHIC SUMMARIES
  
  if (!is.null(geography)) {
    # Join council/HB/HSCP where available.
    geo_joined <- all_all %>%
      filter(disease_name %in% top_diseases) %>%
      left_join(
        geography %>% select(datazone_code = DataZone, CAName, HBName, HSCPName),
        by = "datazone_code"
      )
    
    # Helper: prevalence for top-N geographies by patient count.
    plot_top_geo <- function(df, geo_col, geo_label, N = 12) {
      top_geo <- df %>%
        group_by(.data[[geo_col]]) %>%
        summarise(total_patients = sum(patient_count, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_patients)) %>%
        slice_head(n = N) %>%
        pull(!!sym(geo_col))
      
      df %>%
        filter(.data[[geo_col]] %in% top_geo) %>%
        group_by(.data[[geo_col]], disease_name) %>%
        summarise(
          prev_pw = sum(prev * patient_count, na.rm = TRUE) /
            sum(patient_count[!is.na(prev)], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        ggplot(aes(reorder(.data[[geo_col]], prev_pw), prev_pw)) +
        geom_col() + coord_flip() +
        facet_wrap(~ disease_name, scales = "free_x") +
        labs(title = paste0(geo_label, "-level Prevalence (Top ", N, " by patient count)"),
             x = geo_label, y = "Prevalence (%)") +
        thm
    }
    
    # Top-N summaries by Council / Health Board / HSCP.
    print(plot_top_geo(geo_joined, "CAName", "Council"))
    print(plot_top_geo(geo_joined, "HBName", "Health Board"))
    if ("HSCPName" %in% names(geo_joined)) {
      print(plot_top_geo(geo_joined, "HSCPName", "HSCP"))
    }
    
    # HSCP distribution boxplots.
    if ("HSCPName" %in% names(geo_joined)) {
      hscp_prev <- geo_joined %>% filter(!is.na(HSCPName))
      hscp_order <- hscp_prev %>%
        group_by(HSCPName, disease_name) %>%
        summarise(med_prev = median(prev, na.rm = TRUE), .groups = "drop") %>%
        arrange(med_prev) %>% pull(HSCPName) %>% unique()
      hscp_prev$HSCPName <- factor(hscp_prev$HSCPName, levels = hscp_order)
      print(
        ggplot(hscp_prev, aes(x = HSCPName, y = prev)) +
          geom_boxplot(outlier.alpha = 0.15) +
          coord_flip() +
          facet_wrap(~ disease_name, scales = "free_y") +
          labs(title = "Distribution of Practice-level Prevalence by HSCP",
               x = "HSCP", y = "Prevalence (%)") +
          thm
      )
    }
    
    # Health Board distribution boxplots.
    if ("HBName" %in% names(geo_joined)) {
      hb_prev <- geo_joined %>% filter(!is.na(HBName))
      hb_order <- hb_prev %>%
        group_by(HBName, disease_name) %>%
        summarise(med_prev = median(prev, na.rm = TRUE), .groups = "drop") %>%
        arrange(med_prev) %>% pull(HBName) %>% unique()
      hb_prev$HBName <- factor(hb_prev$HBName, levels = hb_order)
      print(
        ggplot(hb_prev, aes(x = HBName, y = prev)) +
          geom_boxplot(outlier.alpha = 0.15) +
          coord_flip() +
          facet_wrap(~ disease_name, scales = "free_y") +
          labs(title = "Distribution of Practice-level Prevalence by Health Board",
               x = "Health Board", y = "Prevalence (%)") +
          thm
      )
    }
    
    # Store joined data for optional downstream use.
    out$geography$geo_joined <- geo_joined
  }
  return(out)
}


# Population
#
# run_population_eda keeps variables local, prints plots/tables, and returns
# a nested list of key summaries for optional downstream use.
# 
# Parameters:
#   - population: data.frame; population data.
#   - print_tables: logical; if TRUE, prints snapshot tables in addition to plots.
#   - national_level: character; fallback aggregation level for national totals if Country is absent (e.g., "Council").
#   - hist_year: integer; historical/reference year for comparisons (e.g., 2022).
#   - proj_year: integer; projection/target year for comparisons (e.g., 2042).
#   - projection_mode: one of ("variants","principal","all"); controls projection plotting.
#   - age_cutoffs: numeric vector of three cutoffs for ageing groups (e.g., (65, 75, 85)).


run_population_eda <- function(population,
                               print_tables = TRUE,
                               national_level = "Council",
                               hist_year = 2022,
                               proj_year = 2042,
                               projection_mode = c("variants", "principal", "all"),
                               age_cutoffs = c(65, 75, 85)) {
  
  ## SETUP
  
  # Validate input.
  stopifnot(is.data.frame(population))
  
  # Plot theme used across figures.
  thm <- theme_minimal()
  
  # Container for return value.
  out <- list()
  
  # Normalize projection mode.
  projection_mode <- match.arg(projection_mode)
  
  # Helper: parse numeric start age from age_bracket.
  parse_start_age <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("years", "", x)
    x <- gsub("and over", "", x)
    x <- gsub("[^0-9].*$", "", x) # keep only leading digits
    suppressWarnings(as.numeric(x))
  }
  
  ## COMPLETENESS AND STRUCTURE
  
  # Snapshot of size, coverage, distinct keys.
  snap <- population %>%
    summarise(
      n_rows       = n(),
      n_cols       = ncol(.),
      year_min     = min(year, na.rm = TRUE),
      year_max     = max(year, na.rm = TRUE),
      n_areas      = n_distinct(area_code),
      n_area_types = n_distinct(area_type),
      n_age_groups = n_distinct(age_bracket),
      n_sexes      = n_distinct(sex)
    )
  if (print_tables) print(snap)
  out$structure$snapshot <- snap
  
  # Variable-wise missingness fraction (descending).
  na_frac <- tibble(
    var = names(population),
    na_frac = vapply(population, function(x) mean(is.na(x)), numeric(1))
  ) %>% arrange(desc(na_frac))
  if (print_tables) print(na_frac)
  out$structure$na_frac <- na_frac
  
  ## NATIONAL TOTAL POPULATION
  
  # Build national totals if Country rows are absent; otherwise use provided.
  if (!"Country" %in% unique(population$area_type)) {
    national <- population %>%
      filter(area_type == national_level, sex == "all", age_bracket == "all") %>%
      group_by(year) %>%
      summarise(
        population      = sum(population, na.rm = TRUE),
        population_low  = sum(population_low, na.rm = TRUE),
        population_high = sum(population_high, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(area_type = "Country")
  } else {
    national <- population %>%
      filter(area_type == "Country", sex == "all", age_bracket == "all")
  }
  
  ## NATIONAL TRENDS
  
  # Total population over time.
  print(
    ggplot(national, aes(year, population)) +
      geom_line() +
      labs(title = "Total Scottish population (historical + projected)",
           x = "Year", y = "Population") +
      thm
  )
  
  # Ageing trends (shares for 65–74, 75–84, 85+ derived from age_cutoffs).
  ageing <- population %>%
    filter(area_type == national_level, sex == "all", age_bracket != "all") %>%
    mutate(
      age_numeric = parse_start_age(age_bracket),
      age_group = case_when(
        age_numeric >= age_cutoffs[1] & age_numeric < age_cutoffs[2] ~ paste0(age_cutoffs[1], "-", age_cutoffs[2]-1),
        age_numeric >= age_cutoffs[2] & age_numeric < age_cutoffs[3] ~ paste0(age_cutoffs[2], "-", age_cutoffs[3]-1),
        age_numeric >= age_cutoffs[3] ~ paste0(age_cutoffs[3], "+"),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_group)) %>%
    group_by(year, age_group) %>%
    summarise(pop = sum(population, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    mutate(share = pop / sum(pop))
  
  print(
    ggplot(ageing, aes(year, share, colour = age_group)) +
      geom_line(size = 1) +
      scale_y_continuous(labels = percent) +
      labs(title = paste0("Share of population aged ", age_cutoffs[1], "+ over time"),
           x = "Year", y = "Share of total population", colour = "Age group") +
      thm
  )
  out$national$ageing <- ageing
  
  ## PROJECTION SCENARIOS
  
  # Show variants with ribbon or all variants by color, as requested.
  if (projection_mode == "variants" && any(!is.na(national$population_low))) {
    print(
      ggplot(national, aes(year, population)) +
        geom_line(size = 1, colour = "black") +
        geom_ribbon(aes(ymin = population_low, ymax = population_high), alpha = 0.2) +
        labs(title = "Population projections with high/low variants",
             x = "Year", y = "Population") +
        thm
    )
  } else if (projection_mode == "all") {
    all_proj <- population %>%
      filter(sex == "all", age_bracket == "all", area_type == "Country") %>%
      pivot_longer(cols = starts_with("population"), names_to = "variant", values_to = "pop") %>%
      mutate(variant = sub("population_?", "", variant))
    print(
      ggplot(all_proj, aes(year, pop, colour = variant)) +
        geom_line(size = 1) +
        labs(title = "Population projections (all variants)",
             x = "Year", y = "Population", colour = "Variant") +
        thm
    )
  }
  
  ## MOST POPULOUS AREAS
  
  # Top areas at historical year.
  top_now <- population %>%
    filter(area_type == national_level, year == hist_year, sex == "all", age_bracket == "all") %>%
    arrange(desc(population)) %>%
    slice_head(n = 12)
  if (print_tables) print(top_now)
  out$geography$top_now <- top_now
  
  # Top areas at projection year.
  top_future <- population %>%
    filter(area_type == national_level, year == proj_year, sex == "all", age_bracket == "all") %>%
    arrange(desc(population)) %>%
    slice_head(n = 12)
  if (print_tables) print(top_future)
  out$geography$top_future <- top_future
  
  print(
    ggplot(top_now, aes(fct_reorder(area_name, population), population)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Most populous", national_level, "areas in", hist_year),
           x = NULL, y = "Population") +
      thm
  )
  print(
    ggplot(top_future, aes(fct_reorder(area_name, population), population)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Most populous", national_level, "areas in", proj_year),
           x = NULL, y = "Population") +
      thm
  )
  
  ## POPULATION GROWTH
  
  # Growth rate (total population) between hist_year and proj_year.
  growth_total <- population %>%
    filter(area_type == national_level, sex == "all", age_bracket == "all") %>%
    group_by(area_code, area_name) %>%
    summarise(
      growth_rate_total = (population[year == proj_year] - population[year == hist_year]) /
        population[year == hist_year],
      .groups = "drop"
    ) %>%
    arrange(desc(growth_rate_total))
  if (print_tables) print(head(growth_total, 12))
  out$geography$growth_total <- growth_total
  
  # Growth rate for 65+ population.
  growth_65 <- population %>%
    filter(area_type == national_level, sex == "all", age_bracket != "all") %>%
    mutate(age_numeric = parse_start_age(age_bracket)) %>%
    filter(!is.na(age_numeric), age_numeric >= age_cutoffs[1]) %>%
    group_by(area_code, area_name, year) %>%
    summarise(pop_65plus = sum(population, na.rm = TRUE), .groups = "drop") %>%
    group_by(area_code, area_name) %>%
    summarise(
      growth_rate = (pop_65plus[year == proj_year] - pop_65plus[year == hist_year]) /
        pop_65plus[year == hist_year],
      .groups = "drop"
    ) %>%
    arrange(desc(growth_rate))
  if (print_tables) print(head(growth_65, 12))
  out$geography$growth_65 <- growth_65
  
  ## POPULATION PYRAMIDS
  
  # Helper: pyramid for a given year (male negative, female positive).
  pyramid_plot <- function(df, year_val, title) {
    df %>%
      filter(year == year_val, age_bracket != "all", sex %in% c("male", "female")) %>%
      mutate(
        age_numeric = parse_start_age(age_bracket),
        age_bracket = factor(age_bracket, levels = unique(age_bracket[order(age_numeric)])),
        pop = ifelse(sex == "male", -population, population)
      ) %>%
      ggplot(aes(x = fct_rev(age_bracket), y = pop, fill = sex)) +
      geom_col(width = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = abs) +
      labs(title = title, x = "Age bracket", y = "Population") +
      thm
  }
  
  # Draw pyramids for historical and projection years.
  print(pyramid_plot(population, hist_year, paste("Population pyramid —", hist_year)))
  print(pyramid_plot(population, proj_year, paste("Population pyramid —", proj_year)))
  
  # Return silently with captured summaries.
  invisible(out)
}


# plot_scotland draws either a filled council map or a point map over a cropped
# Scotland basemap, using British National Grid coordinates.

plot_scotland <- function(data, area_type = "Council", ref_year, value_col = NULL,
                          size_col = NULL, title = NULL) {
  
  ## BOUNDING BOX
  
  # Compute bbox from points if available; otherwise use a broad Scotland fallback.
  if ("easting" %in% names(data) && "northing" %in% names(data)) {
    pts_sf <- st_as_sf(
      data %>% filter(year == ref_year) %>% distinct(easting, northing),
      coords = c("easting", "northing"), crs = 27700
    )
    bb <- st_bbox(pts_sf)
  } else {
    bb <- c(xmin = 0, ymin = 0, xmax = 700000, ymax = 1300000) # fallback
  }
  pad_x <- (bb["xmax"] - bb["xmin"]) * 0.2
  pad_y <- (bb["ymax"] - bb["ymin"]) * 0.2
  
  ## BASEMAP
  
  # Get UK outline, project to EPSG:27700, crop to padded Scotland extent.
  uk <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin == "United Kingdom") %>%
    st_transform(27700)
  uk_crop <- st_crop(
    uk,
    c(bb["xmin"] - pad_x, bb["ymin"] - pad_y, bb["xmax"] + pad_x, bb["ymax"] + pad_y)
  )
  
  ## MODE: COUNCIL POLYGONS
  
  if (tolower(area_type) == "council") {
    
    # Take UK admin-1 and keep only those intersecting Scotland crop.
    get_scotland_admin1 <- function(scot_outline_27700) {
      uk_adm1 <- ne_states(country = "United Kingdom", returnclass = "sf")
      uk_adm1_27700 <- st_transform(uk_adm1, 27700)
      sel <- st_intersects(uk_adm1_27700, st_geometry(scot_outline_27700), sparse = FALSE)
      st_make_valid(uk_adm1_27700[apply(sel, 1, any), ])
    }
    # Normalize names for fuzzy joins.
    nm_norm <- function(x) {
      x %>%
        tolower() %>%
        gsub("&", " and ", ., fixed = TRUE) %>%
        gsub("[^a-z0-9 ]", " ", .) %>%
        gsub("\\s+", " ", .) %>%
        trimws()
    }
    adm1_scot <- get_scotland_admin1(uk_crop)
    adm1_scot_names <- adm1_scot %>%
      st_drop_geometry() %>%
      transmute(ne_name = name, name_norm = nm_norm(name)) %>%
      distinct()
    
    # Prepare council metric for the reference year; normalize names and patch special names..
    council_ref <- data %>%
      filter(year == ref_year) %>%
      select(area_name, !!sym(value_col)) %>%
      mutate(name_norm = nm_norm(area_name)) %>%
      mutate(name_norm = case_when(
        name_norm == "city of edinburgh" ~ "edinburgh",
        name_norm == "dundee city"       ~ "dundee",
        name_norm == "glasgow city"      ~ "glasgow",
        name_norm == "perth and kinross" ~ "perthshire and kinross",
        name_norm == "na h eileanan siar" ~ "eilean siar",
        name_norm == "orkney islands"    ~ "orkney",
        name_norm == "north ayrshire"    ~ "north ayshire",
        name_norm == "aberdeen city"     ~ "aberdeen",
        TRUE ~ name_norm
      ))
    
    # Join to natural Earth shapes.
    council_join <- council_ref %>%
      left_join(adm1_scot_names, by = "name_norm") %>%
      left_join(adm1_scot, by = c("ne_name" = "name")) %>%
      filter(!is.na(geometry)) %>%
      st_as_sf() %>%
      st_cast("MULTIPOLYGON")
    
    # Draw filled councils.
    p <- ggplot() +
      geom_sf(data = uk_crop, fill = NA, linewidth = 0.4, colour = "grey30") +
      geom_sf(data = council_join, aes(fill = .data[[value_col]]), colour = "white", linewidth = 0.2) +
      scale_fill_viridis_c(option = "C", labels = scales::label_number(big.mark = ","), na.value = "grey80") +
      coord_sf(
        xlim = c(bb["xmin"] - pad_x, bb["xmax"] + pad_x),
        ylim = c(bb["ymin"] - pad_y, bb["ymax"] + pad_y),
        expand = FALSE
      ) +
      labs(title = title, fill = value_col) +
      theme_minimal() +
      theme(axis.title = element_blank())
    print(p)
    
    ## POINTS
  } else if (tolower(area_type) == "points") {
    # Convert ref-year rows to sf points in EPSG:27700.
    pts_sf <- st_as_sf(data %>% filter(year == ref_year),
                       coords = c("easting", "northing"), crs = 27700)
    # Draw points coloured by value_col and sized by size_col.
    p <- ggplot() +
      geom_sf(data = uk_crop, fill = NA, linewidth = 0.4, colour = "grey30") +
      geom_sf(data = pts_sf, aes_string(size = size_col, colour = value_col), alpha = 0.7) +
      scale_colour_viridis_c(option = "C", na.value = "grey80") +
      coord_sf(
        xlim = c(bb["xmin"] - pad_x, bb["xmax"] + pad_x),
        ylim = c(bb["ymin"] - pad_y, bb["ymax"] + pad_y),
        expand = FALSE
      ) +
      labs(title = title, colour = value_col, size = size_col) +
      theme_minimal()
    print(p)
  }
}



# run_areas_eda prints plots/tables for area-level metrics and returns an
# invisible list of summaries (structure, focus slices, deprivation, trends, etc.).


run_areas_eda <- function(areas,
                          print_tables = TRUE,
                          focus_level = "Health and Social Care Partnership",
                          window_years = c(2018, 2025),
                          ref_year = 2022) {
  
  ## SETUP

  thm <- theme_minimal()
  out <- list()
  
  ## STRUCTURE AND MISSINGNESS
  
  # Snapshot of size, span, and distinct categories.
  snap <- areas %>%
    summarise(
      n_rows = n(), n_cols = ncol(.),
      year_min = min(year, na.rm = TRUE),
      year_max = max(year, na.rm = TRUE),
      n_areas = n_distinct(area_code),
      area_types = paste(sort(unique(area_type)), collapse = ", "),
      n_age_groups = n_distinct(age_bracket),
      n_sexes = n_distinct(sex)
    )
  if (print_tables) print(snap)
  out$structure$snapshot <- snap
  
  # Variable-wise missingness.
  na_frac <- tibble(
    var = names(areas),
    na_frac = vapply(areas, function(x) mean(is.na(x)), numeric(1))
  ) %>% arrange(desc(na_frac))
  if (print_tables) print(na_frac)
  print(
    ggplot(na_frac, aes(fct_reorder(var, na_frac), na_frac)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(title = "Proportion NA by Feature", x = NULL, y = "Missing (%)") +
      thm
  )
  
  ## FOCUS SLICE (sex = all, age = all)
  
  focus <- areas %>% filter(area_type == focus_level)
  panel <- focus %>% filter(sex == "all", age_bracket == "all")
  
  ## GP COVERAGE AND WORKLOAD (ref_year)
  
  if ("gp_per_1000" %in% names(panel)) {
    panel_ref <- panel %>% filter(year == ref_year)
    print(
      ggplot(panel_ref, aes(gp_per_1000)) +
        geom_histogram(bins = 40) +
        labs(title = paste("GP per 1,000: ", focus_level, ref_year),
             x = "GP per 1,000", y = "Areas") + thm
    )
    out$focus$gp_per_1000_ref <- panel_ref
  }
  
  if (all(c("patient_count", "gp_count") %in% names(panel))) {
    ppg_ref <- panel %>%
      filter(year == ref_year, !is.na(gp_count), gp_count > 0) %>%
      mutate(patients_per_gp = patient_count / gp_count)
    print(
      ggplot(ppg_ref, aes(patients_per_gp)) +
        geom_histogram(bins = 40) +
        labs(title = paste("Patients per GP:", focus_level, ref_year),
             x = "Patients per GP", y = "Areas") + thm
    )
    out$focus$ppg_ref <- ppg_ref
  }
  
  ## DEPRIVATION (ref_year)
  
  if ("simd_decile" %in% names(panel)) {
    # GP per 1,000 vs SIMD decile.
    if ("gp_per_1000" %in% names(panel)) {
      simd_gp <- panel %>%
        filter(year == ref_year) %>%
        group_by(simd_decile) %>%
        summarise(med_gp_per_1000 = median(gp_per_1000, na.rm = TRUE), .groups = "drop")
      print(
        ggplot(simd_gp, aes(simd_decile, med_gp_per_1000)) +
          geom_point() + geom_smooth(method = "lm", se = FALSE) +
          scale_x_continuous(breaks = 1:10) +
          labs(title = paste("Median GP per 1,000 vs SIMD Decile: ", ref_year),
               x = "SIMD Decile (1=most deprived)", y = "GP per 1,000") + thm
      )
      out$deprivation$gp_per_1000 <- simd_gp
    }
    # Patients per GP vs SIMD decile.
    if ("patients_per_gp" %in% names(out$focus)) {
      simd_ppg <- ppg_ref %>%
        group_by(simd_decile) %>%
        summarise(med_ppg = median(patients_per_gp, na.rm = TRUE), .groups = "drop")
      print(
        ggplot(simd_ppg, aes(simd_decile, med_ppg)) +
          geom_point() + geom_smooth(method = "lm", se = FALSE) +
          scale_x_continuous(breaks = 1:10) +
          labs(title = paste("Median patients per GP vs SIMD Decile: ", ref_year),
               x = "SIMD decile (1=most deprived)", y = "Patients per GP") + thm
      )
      out$deprivation$patients_per_gp <- simd_ppg
    }
  }
  
  ## TIME TRENDS (focus_level)
  
  if ("gp_per_1000" %in% names(panel)) {
    gp_time <- panel %>%
      group_by(year) %>%
      summarise(median_gp_per_1000 = median(gp_per_1000, na.rm = TRUE), .groups = "drop")
    print(
      ggplot(gp_time, aes(year, median_gp_per_1000)) +
        geom_line() +
        labs(title = paste("Median GP per 1,000 over time: ", focus_level),
             x = "Year", y = "GP per 1,000") + thm
    )
    out$trends$gp_per_1000 <- gp_time
  }
  
  if ("gp_count" %in% names(panel) && "patient_count" %in% names(panel)) {
    ppg_time <- panel %>%
      filter(!is.na(gp_count), gp_count > 0) %>%
      mutate(ppg = patient_count / gp_count) %>%
      group_by(year) %>%
      summarise(median_ppg = median(ppg, na.rm = TRUE), .groups = "drop")
    print(
      ggplot(ppg_time, aes(year, median_ppg)) +
        geom_line() +
        labs(title = paste("Median patients per GP over time: ", focus_level),
             x = "Year", y = "Patients per GP") + thm
    )
    out$trends$patients_per_gp <- ppg_time
  }
  
  # Total population over time.
  pop_time <- panel %>%
    group_by(year) %>%
    summarise(total_pop = sum(population, na.rm = TRUE), .groups = "drop")
  print(
    ggplot(pop_time, aes(year, total_pop)) +
      geom_line() +
      labs(title = paste("Total population over time: ", focus_level),
           x = "Year", y = "Population") + thm
  )
  out$trends$population <- pop_time
  
  ## TOP/BOTTOM PERFORMANCE (window_years)
  
  if (length(window_years) == 2 && all(window_years %in% panel$year)) {
    change_tbl <- panel %>%
      filter(year %in% window_years) %>%
      group_by(area_code, area_name) %>%
      summarise(
        gp_per_1000_change = diff(gp_per_1000[order(year)]),
        ppg_change = diff((patient_count/gp_count)[order(year)]),
        .groups = "drop"
      )
    top_gp <- change_tbl %>% arrange(desc(gp_per_1000_change)) %>% slice_head(n = 10)
    bot_gp <- change_tbl %>% arrange(gp_per_1000_change) %>% slice_head(n = 10)
    if (print_tables) {
      cat("\nTop 10 improvements in GP per 1,000:\n"); print(top_gp)
      cat("\nBottom 10 changes in GP per 1,000:\n"); print(bot_gp)
    }
    out$performance$top_gp_per_1000 <- top_gp
    out$performance$bottom_gp_per_1000 <- bot_gp
  }
  
  ## 65+ SHARE OVER TIME
  
  if ("age_bracket" %in% names(focus)) {
    share65 <- focus %>%
      filter(sex == "all", age_bracket != "all") %>%
      mutate(is_65plus = age_bracket %in% c("65 to 74", "75 to 84", "85 and over")) %>%
      group_by(area_code, area_name, year) %>%
      summarise(
        pop_all = sum(population, na.rm = TRUE),
        pop_65  = sum(population[is_65plus], na.rm = TRUE),
        share_65 = ifelse(pop_all > 0, pop_65 / pop_all, NA_real_),
        .groups = "drop"
      )
    share65_time <- share65 %>%
      group_by(year) %>%
      summarise(median_share65 = median(share_65, na.rm = TRUE), .groups = "drop")
    print(
      ggplot(share65_time, aes(year, median_share65)) +
        geom_line() + scale_y_continuous(labels = percent) +
        labs(title = paste("Median 65+ share over time: ", focus_level),
             x = "Year", y = "Median share 65+") + thm
    )
    out$trends$share65 <- share65_time
  }
  
  ## MAP
  
  if ("Council" %in% areas$area_type) {
    plot_scotland(
      data = areas %>% filter(area_type == "Council", sex == "all", age_bracket == "all"),
      area_type = "Council",
      ref_year = ref_year,
      value_col = "population",
      title = paste("Population by Council: ", ref_year)
    )
  }
  
  # Return silently with captured outputs.
  invisible(out)
}


## Top Diseases Spatial EDA

# run_top_diseases_eda produces point maps, choropleths, grids, and summary
# tables for selected diseases over a specified year range.
#
# Parameters:
# - practices: data.frame; practice-level dataset from pipeline.
# - diseases: data.frame; includes disease_name, counts, patient counts, SIMD, GP counts.
# - areas: data.frame; geographic areas with coordinates, types, and practice counts.
# - geography: data.frame; lookup with DataZone, council, HB, and HSCP names.
# - top_diseases: character vector; diseases to analyse (default: 8 key conditions).
# - years: numeric; years to average over (default: 2022:2025).
# - simd_hold_decile: integer; 1–10 decile to hold constant for council SIMD grid (default: 5).
# - output_dir: character; directory to write CSV outputs (default: "outputs").

run_top_diseases_eda <- function(
    practices,
    diseases,
    areas,
    geography,
    top_diseases = c(
      "Depression","Hypertension","Asthma","Diabetes",
      "Cancer","Coronary Heart Disease (CHD)",
      "Chronic Kidney Disease (CKD)","Stroke and TIA"
    ),
    years = 2022:2025,
    simd_hold_decile = 5,
    output_dir = "outputs"
) {
  
  ## SETUP

  # Set a minimal ggplot theme for consistency.
  thm <- theme_minimal()
  
  # Define reference year as the maximum of the selected years.
  ref_year <- max(years)
  
  # Create the output directory if it does not exist.
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Warn if the mapping helper function is missing.
  if (!exists("plot_scotland")) {
    warning("plot_scotland() not found — maps will be skipped.")
  }
  
  ## CORE PREPROCESSING
  
  # Compute prevalence if not already present.
  diseases <- diseases %>%
    mutate(prev = coalesce(disease_rate_per_100, 100 * disease_count / patient_count))
  
  # Filter to top diseases and selected years.
  dis_allall <- diseases %>%
    filter(disease_name %in% top_diseases, year %in% years)
  
  ## DATAZONE POINT MAPS
  
  # Build centroids for each datazone, averaging over the period.
  dz_centroids <- areas %>%
    filter(area_type == "DataZone", sex == "all", age_bracket == "all", year %in% years) %>%
    group_by(area_code) %>%
    summarise(
      easting = mean(easting, na.rm = TRUE),
      northing = mean(northing, na.rm = TRUE),
      practices_count = mean(practices_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Aggregate disease counts and prevalence to datazone level.
  dis_dz_mean <- dis_allall %>%
    filter(sex == "all", age_bracket == "all") %>%
    group_by(datazone_code, disease_name) %>%
    summarise(
      disease_count = mean(disease_count, na.rm = TRUE),
      prev = mean(prev, na.rm = TRUE),
      patient_count = mean(patient_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(dz_centroids, by = c("datazone_code" = "area_code")) %>%
    filter(is.finite(easting), is.finite(northing),
           is.finite(prev), is.finite(disease_count),
           is.finite(practices_count), practices_count > 0) %>%
    mutate(year = ref_year)
  
  # Plot point maps for each disease if mapping is available.
  if (exists("plot_scotland")) {
    unique(dis_dz_mean$disease_name) %>% walk(function(dz) {
      df <- dis_dz_mean %>% filter(disease_name == dz)
      if (nrow(df) > 0) {
        plot_scotland(
          data = df,
          area_type = "points",
          ref_year = ref_year,
          value_col = "disease_count",
          size_col  = "disease_count",
          title = paste0(dz, " — datazone-level disease counts (avg ",
                         min(years), "–", max(years), ")")
        )
      }
    })
  }
  
  ## COUNCIL CHOROPLETHS
  
  # Aggregate disease counts and prevalence to council level.
  council_mean <- dis_allall %>%
    filter(sex == "all", age_bracket == "all") %>%
    left_join(geography %>% select(datazone_code = DataZone, CAName), by = "datazone_code") %>%
    filter(!is.na(CAName)) %>%
    group_by(CAName, disease_name) %>%
    summarise(
      disease_count = mean(disease_count, na.rm = TRUE),
      prev = mean(prev, na.rm = TRUE),
      patients = mean(patient_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(area_type = "Council", area_name = CAName, year = ref_year)
  
  # Build council centroids for cropping Scotland maps.
  council_xy <- areas %>%
    filter(area_type == "Council", sex == "all", age_bracket == "all") %>%
    group_by(area_name) %>%
    summarise(
      easting = mean(easting, na.rm = TRUE),
      northing = mean(northing, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot council-level choropleths per disease if mapping is available.
  if (exists("plot_scotland")) {
    unique(council_mean$disease_name) %>% walk(function(dz) {
      df <- council_mean %>%
        filter(disease_name == dz) %>%
        left_join(council_xy, by = "area_name")
      if (nrow(df) > 0) {
        plot_scotland(
          data = df %>% select(area_name, year, disease_count, easting, northing),
          area_type = "Council",
          ref_year = ref_year,
          value_col = "disease_count",
          title = paste0(dz, ": Council-level Disease Counts (avg ",
                         min(years), "–", max(years), ")")
        )
      }
    })
  }
  
  ## GRID PLOTS
  
  # Prepare working dataset with factor levels for sex and SIMD.
  dis_work <- dis_allall %>%
    mutate(
      sex = factor(sex, levels = c("male","female","all")),
      simd_decile = as.integer(simd_decile),
      simd_council_decile = as.integer(simd_council_decile)
    )
  
  # Define ordered age brackets for plotting.
  coarse_levels <- c("0 to 4","5 to 14","15 to 24","25 to 44",
                     "45 to 64","65 to 74","75 to 84","85 and over")
  
  ### Prevalence by Sex
  print(
    dis_work %>%
      filter(age_bracket == "all", sex %in% c("male","female")) %>%
      ggplot(aes(sex, prev)) +
      geom_boxplot(outlier.alpha = 0.2) +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Sex (Age = All)", x = "Sex", y = "Prevalence (%)") +
      thm
  )
  
  ### Prevalence by Age Bracket
  print(
    dis_work %>%
      filter(sex == "all", age_bracket != "all") %>%
      mutate(age_bracket = factor(age_bracket, levels = coarse_levels)) %>%
      ggplot(aes(age_bracket, prev)) +
      geom_boxplot(outlier.alpha = 0.2) +
      coord_flip() +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Age Bracket (Sex = All)",
           x = "Age Bracket", y = "Prevalence (%)") +
      thm
  )
  
  ### Prevalence by Year
  print(
    dis_work %>%
      filter(sex == "all", age_bracket == "all") %>%
      ggplot(aes(factor(year), prev)) +
      geom_boxplot(outlier.alpha = 0.2) +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Year (Sex = All, Age = All)",
           x = "Year", y = "Prevalence (%)") +
      thm
  )
  
  ### Prevalence by Health Board
  print(
    dis_work %>%
      left_join(geography %>% select(datazone_code = DataZone, HBName), by = "datazone_code") %>%
      filter(!is.na(HBName), sex == "all", age_bracket == "all") %>%
      ggplot(aes(fct_reorder(HBName, prev, .fun = median, na.rm = TRUE), prev)) +
      geom_boxplot(outlier.alpha = 0.15) +
      coord_flip() +
      facet_wrap(~ disease_name, scales = "free_y") +
      labs(title = "Prevalence by Health Board (All-All)",
           x = "Health Board", y = "Prevalence (%)") +
      thm
  )
  
  ## SUMMARY TABLES
  
  # Council patient-weighted prevalence.
  council_prev_pw <- dis_allall %>%
    filter(sex == "all", age_bracket == "all") %>%
    left_join(geography %>% select(datazone_code = DataZone, CAName), by = "datazone_code") %>%
    filter(!is.na(CAName)) %>%
    group_by(disease_name, CAName) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      cases = sum(disease_count, na.rm = TRUE),
      patients = sum(patient_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(disease_name, desc(prev_pw))
  #write.csv(council_prev_pw, file.path(output_dir, "council_prevalence_patient_weighted.csv"), row.names = FALSE)
  
  # Health Board patient-weighted prevalence.
  hb_prev_pw <- dis_allall %>%
    filter(sex == "all", age_bracket == "all") %>%
    left_join(geography %>% select(datazone_code = DataZone, HBName), by = "datazone_code") %>%
    filter(!is.na(HBName)) %>%
    group_by(disease_name, HBName) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      cases = sum(disease_count, na.rm = TRUE),
      patients = sum(patient_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(disease_name, desc(prev_pw))
  #write.csv(hb_prev_pw, file.path(output_dir, "hb_prevalence_patient_weighted.csv"), row.names = FALSE)
  
  # Deprivation slopes by disease.
  dep_slopes <- dis_work %>%
    filter(sex == "all", age_bracket == "all", !is.na(simd_decile)) %>%
    group_by(disease_name) %>%
    summarise(
      n = n(),
      slope = coef(lm(prev ~ simd_decile))[["simd_decile"]],
      intercept = coef(lm(prev ~ simd_decile))[["(Intercept)"]],
      .groups = "drop"
    ) %>%
    arrange(slope)
  #write.csv(dep_slopes, file.path(output_dir, "deprivation_slopes_by_disease.csv"), row.names = FALSE)
  
  # Spearman correlations with GP count.
  gp_corr <- dis_work %>%
    filter(sex == "all", age_bracket == "all", is.finite(gp_count), gp_count > 0) %>%
    group_by(disease_name) %>%
    summarise(
      spearman_r = suppressWarnings(cor(prev, gp_count, method = "spearman", use = "pairwise")),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(spearman_r)
  #write.csv(gp_corr, file.path(output_dir, "gp_count_spearman_by_disease.csv"), row.names = FALSE)
  
  # SIMD1 vs SIMD10 prevalence gap.
  simd_gap <- dis_work %>%
    filter(sex == "all", age_bracket == "all", simd_decile %in% c(1, 10)) %>%
    group_by(disease_name, simd_decile) %>%
    summarise(
      prev_pw = sum(prev * patient_count, na.rm = TRUE) /
        sum(patient_count[!is.na(prev)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = simd_decile, values_from = prev_pw, names_prefix = "dec_") %>%
    mutate(gap_pp = dec_1 - dec_10) %>%
    arrange(desc(gap_pp))
  #write.csv(simd_gap, file.path(output_dir, "simd_gap_dec1_minus_dec10.csv"), row.names = FALSE)
  
  # Return key outputs invisibly.
  invisible(list(
    datazone_points = dis_dz_mean,
    council_choropleth = council_mean,
    council_prev_pw = council_prev_pw,
    hb_prev_pw = hb_prev_pw,
    deprivation_slopes = dep_slopes,
    gp_corr = gp_corr,
    simd_gap = simd_gap
  ))
}

# practices_eda <- run_practices_eda(practices, geography)
# diseases_eda <- run_diseases_eda(diseases, geography)
# population_eda <- run_population_eda(population)
# areas_eda <- run_areas_eda(areas)
# top_diseases_eda <- run_top_diseases_eda(practices, diseases, areas, geography)

# Define chosen diseases.
focus_diseases <- c(
  "Diabetes",
  "Depression",
  "Hypertension",
  "Asthma",
  "Cancer",
  "Coronary Heart Disease (CHD)",
  "Chronic Kidney Disease (CKD)",
  "Stroke and TIA"
)

get_final <- function(practices, geography, areas, diseases, diseases_list) {
  
  ## AGE MIDPOINT LOOKUP
  
  # Define midpoints for each age bracket for use in modelling.
  age_mid_lookup <- tibble::tibble(
    age_bracket = c("0 to 4","5 to 14","15 to 24","25 to 44",
                    "45 to 64","65 to 74","75 to 84","85 and over"),
    age_mid = c(2.5, 10, 20, 35, 55, 70, 80, 90)
  )
  
  ## PRACTICE COORDINATES
  
  # Get latest coordinates for each practice (sex = all, age = all, latest quarter only).
  practice_coords <- practices %>%
    filter(sex == "all", age_bracket == "all") %>%
    group_by(practice_code) %>%
    slice_max(quarter, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(practice_code, easting, northing) %>%
    filter(!is.na(easting), !is.na(northing))
  
  ## NEAREST PRACTICE DISTANCE
  
  # Compute Euclidean distance to the nearest other practice.
  coord_mat <- as.matrix(practice_coords[, c("easting", "northing")])
  dist_mat <- as.matrix(dist(coord_mat))
  nearest_dist <- apply(dist_mat, 1, function(x) {
    y <- x[x > 0 & !is.infinite(x)]
    if (length(y) == 0) NA_real_ else min(y, na.rm = TRUE)
  })
  
  # Add nearest distance to practice coordinates table.
  practice_coords <- practice_coords %>%
    mutate(nearest_practice_dist = nearest_dist)
  
  ## GEOGRAPHY IDS
  
  # Create lookup of CA, HSCP, and HB IDs by DataZone.
  geo_lookup <- geography %>%
    distinct(DataZone, CA, HSCP, HB) %>%
    rename(
      datazone_code = DataZone,
      CA_id = CA,
      HSCP_id = HSCP,
      HB_id = HB
    )
  
  ## HSCP POPULATION LOOKUP
  
  # Get latest HSCP population for joining to practices.
  latest_year <- max(areas$year, na.rm = TRUE)
  pop_lookup <- areas %>%
    filter(
      year == latest_year,
      age_bracket == "all",
      sex == "all",
      area_type == "Health and Social Care Partnership"
    ) %>%
    distinct(area_code, population) %>%
    rename(
      HSCP_id = area_code,
      hscp_population = population
    )
  
  ## ASSEMBLE MODELLING TABLE
  
  # Build combined dataset for all diseases in diseases_list.
  final <- purrr::map_dfr(diseases_list, function(disease_filter) {
    diseases %>%
      filter(
        disease_name == disease_filter,
        year %in% 2022:2025,
        sex %in% c("male", "female"),
        age_bracket %in% age_mid_lookup$age_bracket
      ) %>%
      left_join(age_mid_lookup, by = "age_bracket") %>%
      left_join(geo_lookup, by = "datazone_code") %>%
      left_join(practice_coords, by = "practice_code") %>%
      left_join(pop_lookup, by = "HSCP_id") %>%
      mutate(
        # Ensure disease counts do not exceed patient counts.
        disease_count = ifelse(disease_count > patient_count, patient_count, disease_count),
        # Calculate patients per GP where GP count > 0.
        patients_per_gp = ifelse(!is.na(gp_count) & gp_count > 0,
                                 patient_count / gp_count, NA_real_),
        # Log-transform patients per GP.
        log_ppgp = log1p(patients_per_gp),
        # Keep age bracket as an ordered factor.
        age_bracket = factor(age_bracket, levels = age_mid_lookup$age_bracket),
        # Add HSCP population columns.
        pop_hscp = hscp_population,
        population = hscp_population
      ) %>%
      # Remove rows with missing or zero patient counts.
      filter(!is.na(patient_count), patient_count > 0) %>%
      # Select modelling variables.
      select(
        practice_code, year, sex, age_bracket, age_mid,
        disease_name, disease_count, patient_count, disease_rate_per_100,
        simd_decile,
        gp_count, patients_per_gp, log_ppgp,
        CA_id, HSCP_id, HB_id,
        easting, northing, nearest_practice_dist,
        population, pop_hscp
      )
  })
  
  # Return final modelling dataset.
  return(final)
}

# Build final dataset for selected focus diseases.
final <- get_final(practices, geography, areas, diseases, focus_diseases)

# 
# ## Random Effects Variance Trace
# 
# # test_geographic_random_effect sequentially fits mixed models for each disease and
# # grouping factor, adding fixed effects stepwise, and extracts random-effect
# # variances. It offers a method of testing the significance of geographic random effects
# # since random sampling would not yield enough practices per area to properly examine it.
# 
# test_geographic_random_effect <- function(data) {
#   
#   ## SETUP
#   
#   # Extract unique disease names.
#   disease_list <- unique(data$disease_name)
#   
#   # Define grouping factors to test.
#   group_vars <- c("HSCP_id", "HB_id", "CA_id")
#   
#   # Define fixed-effect terms added stepwise.
#   fixed_steps <- list(
#     NULL,
#     "age_bracket",
#     "age_bracket + sex",
#     "age_bracket + sex + year",
#     "age_bracket + sex + year + log_ppgp_sc",
#     "age_bracket + sex + year + log_ppgp_sc + pop_hscp_sc",
#     "age_bracket + sex + year + log_ppgp_sc + pop_hscp_sc + simd_decile",
#     "age_bracket + sex + year + log_ppgp_sc + pop_hscp_sc + simd_decile + dist_sc"
#   )
#   
#   # Helper to standardize proportions to (0,1).
#   sv01 <- function(y) {
#     y <- as.numeric(y)
#     y[!is.finite(y)] <- NA_real_
#     if (max(y, na.rm = TRUE) > 1.0001) y <- y / 100
#     n <- sum(!is.na(y))
#     y <- pmin(pmax(y, 0), 1)
#     (y * (n - 1) + 0.5) / n
#   }
#   
#   # Prepare container for results.
#   results <- vector("list", length(disease_list) * length(group_vars) * length(fixed_steps))
#   k <- 0
#   
#   ## MAIN LOOP: iterate over diseases, groups, and steps.
#   
#   for (disease in disease_list) {
#     message("Processing disease: ", disease)
#     
#     # Filter dataset to the current disease and prepare variables.
#     dat0 <- data %>%
#       filter(disease_name == disease) %>%
#       mutate(
#         rate_prop   = sv01(disease_rate_per_100),
#         age_bracket = factor(age_bracket),
#         sex         = factor(sex),
#         year        = factor(year),
#         log_ppgp_sc = as.numeric(scale(as.numeric(log_ppgp))),
#         pop_hscp_sc = as.numeric(scale(as.numeric(pop_hscp))),
#         simd_decile = suppressWarnings(as.numeric(simd_decile)),
#         dist_sc     = as.numeric(scale(as.numeric(nearest_practice_dist))),
#         row_id      = dplyr::row_number()
#       )
#     
#     for (group_var in group_vars) {
#       if (!group_var %in% names(dat0)) next
#       
#       # Drop rows with missing group_var.
#       dat_g <- dat0 %>% filter(!is.na(.data[[group_var]]))
#       
#       # Require at least two levels to estimate variance.
#       n_levels <- dplyr::n_distinct(dat_g[[group_var]])
#       if (n_levels < 2) next
#       
#       # Specify additional random effects to control for before testing.
#       extra_res <- character(0)
#       if (group_var == "HB_id" && "HSCP_id" %in% names(dat_g)) {
#         extra_res <- c("(1|HSCP_id)")
#       } else if (group_var == "CA_id") {
#         if ("HSCP_id" %in% names(dat_g)) extra_res <- c(extra_res, "(1|HSCP_id)")
#         if ("HB_id" %in% names(dat_g))   extra_res <- c(extra_res, "(1|HB_id)")
#       }
#       
#       for (i in seq_along(fixed_steps)) {
#         fe_terms <- fixed_steps[[i]]
#         re_terms <- paste(c(paste0("(1|", group_var, ")"), extra_res), collapse = " + ")
#         
#         # Build model formula.
#         if (is.null(fe_terms)) {
#           form <- as.formula(paste0("rate_prop ~ ", re_terms))
#         } else {
#           form <- as.formula(paste0("rate_prop ~ ", fe_terms, " + ", re_terms))
#         }
#         
#         # Fit model.
#         fit <- try(
#           lmer(form, data = dat_g, REML = TRUE, na.action = na.exclude),
#           silent = TRUE
#         )
#         
#         if (inherits(fit, "try-error")) {
#           k <- k + 1
#           results[[k]] <- data.frame(
#             disease = disease, group = group_var, n_levels = n_levels,
#             step = i - 1,
#             added_predictors = ifelse(is.null(fe_terms), "(Intercept only)", fe_terms),
#             var_group = NA_real_, pearson_r_resid = NA_real_
#           )
#           next
#         }
#         
#         # Extract variance for the current group_var.
#         vc <- as.data.frame(VarCorr(fit))
#         var_group <- try(
#           vc$vcov[vc$grp == group_var & vc$var1 == "(Intercept)"][1],
#           silent = TRUE
#         )
#         if (inherits(var_group, "try-error") || length(var_group) == 0) var_group <- NA_real_
#         
#         # Extract residuals, align with original data.
#         res <- resid(fit)
#         if (length(res) != nrow(dat_g)) {
#           used <- data.frame(row_id = as.integer(names(res)), resid = as.numeric(res))
#           dat_g <- dat_g %>% left_join(used, by = "row_id")
#         } else {
#           dat_g$resid <- as.numeric(res)
#         }
#         
#         # Compute Pearson correlation between mean residuals and outcomes by group.
#         means <- dat_g %>%
#           group_by(.grp = .data[[group_var]]) %>%
#           summarise(
#             mean_resid   = mean(resid, na.rm = TRUE),
#             mean_outcome = mean(rate_prop, na.rm = TRUE),
#             .groups = "drop"
#           )
#         
#         pearson_r <- suppressWarnings(cor(means$mean_resid, means$mean_outcome))
#         if (!is.finite(pearson_r)) pearson_r <- NA_real_
#         
#         # Store results.
#         k <- k + 1
#         results[[k]] <- data.frame(
#           disease = disease,
#           group = group_var,
#           n_levels = n_levels,
#           step = i - 1,
#           added_predictors = ifelse(is.null(fe_terms), "(Intercept only)", fe_terms),
#           var_group = var_group,
#           pearson_r_resid = pearson_r
#         )
#       }
#     }
#   }
#   
#   ## COMBINE RESULTS
#   
#   rez <- bind_rows(results)
#   
#   ## PLOTS
#   
#   step_seq <- c(
#     "Intercept only",
#     "age_bracket",
#     "sex",
#     "year",
#     "log_ppgp_sc",
#     "pop_hscp_sc",
#     "simd_decile",
#     "dist_sc"
#   )
#   
#   rez_clean <- rez %>%
#     mutate(
#       step = as.integer(step),
#       step_added = factor(step_seq[pmax(1, step + 1)], levels = step_seq)
#     ) %>%
#     group_by(disease, group) %>%
#     arrange(step, .by_group = TRUE) %>%
#     mutate(
#       var0 = first(var_group),
#       frac_remaining = var_group / var0,
#       drop = lag(var_group) - var_group
#     ) %>%
#     ungroup()
#   
#   # Fraction of variance remaining plot.
#   p1 <- ggplot(rez_clean, aes(x = step, y = frac_remaining)) +
#     geom_line() +
#     geom_point(size = 0.9) +
#     facet_grid(disease ~ group, scales = "free_y") +
#     scale_x_continuous(breaks = 0:7, labels = step_seq) +
#     labs(
#       x = "Predictor added at step",
#       y = "Fraction of RE variance remaining (vs step 0)",
#       title = "Random-effect variance consumed by predictors (stepwise)"
#     ) +
#     theme_minimal(base_size = 9) +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       panel.spacing = unit(0.7, "lines")
#     )
#   print(p1)
#   
#   # Annotated largest drops.
#   big_drops <- rez_clean %>%
#     group_by(disease, group) %>%
#     filter(!is.na(drop)) %>%
#     slice_max(drop, n = 1, with_ties = FALSE) %>%
#     ungroup()
#   
#   p1_annot <- p1 +
#     geom_point(data = big_drops, aes(x = step, y = frac_remaining),
#                shape = 21, stroke = 0.5) +
#     geom_text(data = big_drops, aes(x = step, y = frac_remaining, label = as.character(step_added)),
#               vjust = -0.6, size = 2.7)
#   print(p1_annot)
#   
#   # Heatmap of variance drops by predictor.
#   drops_tbl <- rez_clean %>%
#     filter(step > 0) %>%
#     mutate(predictor = step_added) %>%
#     select(disease, group, predictor, drop)
#   
#   p2 <- ggplot(drops_tbl, aes(x = predictor, y = group, fill = pmax(drop, 0))) +
#     geom_tile() +
#     facet_wrap(~ disease, ncol = 2, scales = "free_y") +
#     scale_fill_continuous(name = "Var. drop\n(at step)", guide = "colorbar") +
#     labs(
#       x = "Predictor added at this step",
#       y = "Grouping factor",
#       title = "Stepwise reduction in random-effect variance by predictor"
#     ) +
#     theme_minimal(base_size = 9) +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       panel.spacing = unit(0.8, "lines")
#     )
#   print(p2)
#   
#   # Focus plot for one disease.
#   focus_disease <- "Diabetes"  # Change as needed.
#   p_focus <- rez_clean %>%
#     filter(disease == focus_disease) %>%
#     ggplot(aes(step, frac_remaining)) +
#     geom_line() +
#     geom_point() +
#     facet_wrap(~ group, scales = "free_y") +
#     scale_x_continuous(breaks = 0:7, labels = step_seq) +
#     labs(
#       x = "Predictor added at step",
#       y = "Fraction remaining",
#       title = paste0("Variance remaining by step — ", focus_disease)
#     ) +
#     theme_minimal(base_size = 10) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   print(p_focus)
#   
#   # Return results.
#   return(rez)
# }

## MODELING

# Run brms models.
run_brms_models <- function(
    final, current_disease,
    n_practices = 40,
    chains = 4,
    iter = 4000,
    warmup = 1000,
    cores = 2,
    save_path = NULL,
    models_to_run = c("all","beta","binomial")
) {
  # Set CmdStan backend if need be.
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr not installed. Run cmdstanr::install_cmdstan().")
  }
  options(brms.backend = "cmdstanr")
  set.seed(1234)
  
  models_to_run <- match.arg(models_to_run)
  
  # Prepare disease-specific dataset.
  dat <- final %>% filter(disease_name == current_disease)
  if (nrow(dat) == 0) { message("No rows for ", current_disease); return(invisible(NULL)) }
  
  # Ensure practice_code exists.
  if (!"practice_code" %in% names(dat)) {
    key_cols <- intersect(c("practice_code","CA_id","HSCP_id","HB_id"), names(dat))
    if (length(key_cols) >= 1) {
      dat <- dat %>% mutate(practice_code = interaction(!!!rlang::syms(key_cols), drop = TRUE))
    } else {
      dat <- dat %>% mutate(practice_code = factor(paste0("p", ceiling(row_number()/1000))))
    }
  }
  
  # Ensure required columns exist.
  need_cols <- c("disease_rate_per_100","simd_decile","log_ppgp","age_mid","sex",
                 "nearest_practice_dist","practice_code","HSCP_id","HB_id")
  for (cc in setdiff(need_cols, names(dat))) dat[[cc]] <- NA
  
  # Coerce types and clean basic fields.
  dat <- dat %>%
    mutate(
      sex = as.factor(as.character(sex)),
      simd_decile = suppressWarnings(as.numeric(simd_decile)),
      log_ppgp    = suppressWarnings(as.numeric(log_ppgp)),
      age_mid     = suppressWarnings(as.numeric(age_mid)),
      nearest_practice_dist = suppressWarnings(as.numeric(nearest_practice_dist)),
      HSCP_id = as.factor(HSCP_id),
      HB_id   = as.factor(HB_id)
    )
  
  # Helper to scale proportions into (0,1) with simple smoothing.
  sv01 <- function(y) {
    y <- as.numeric(y); y[!is.finite(y)] <- NA_real_
    if (max(y, na.rm=TRUE) > 1.0001) y <- y/100
    n <- sum(!is.na(y))
    y <- pmin(pmax(y,0),1)
    (y*(n-1)+0.5)/n
  }
  
  # Build full-data frame for beta-family models.
  dat_clean_beta <- dat %>%
    mutate(
      rate_prop   = sv01(disease_rate_per_100),
      dist_sc     = scale(nearest_practice_dist)[,1],
      age_sc      = scale(age_mid)[,1],
      log_ppgp_sc = scale(log_ppgp)[,1]
    ) %>%
    filter(!is.na(rate_prop), !is.na(log_ppgp_sc), !is.na(age_sc),
           !is.na(sex), !is.na(practice_code), !is.na(HSCP_id), !is.na(HB_id))
  
  if (nrow(dat_clean_beta) == 0) { message("All rows dropped after cleaning (beta)"); return(invisible(NULL)) }
  
  # Build full-data frame for binomial models, synthesising counts if missing.
  dat_full_binom <- dat_clean_beta
  if (!all(c("disease_count","patient_count") %in% names(dat_full_binom))) {
    dat_full_binom <- dat_full_binom %>%
      mutate(
        patient_count = 1000L,
        disease_count = pmax(0L, pmin(patient_count, round(rate_prop * patient_count)))
      )
  }
  dat_full_binom <- dat_full_binom %>%
    mutate(disease_count = round(disease_count), patient_count = round(patient_count)) %>%
    filter(disease_count <= patient_count, disease_count >= 0, patient_count > 0)
  
  if (nrow(dat_full_binom) == 0 && models_to_run %in% c("all","binomial")) {
    message("All rows dropped after cleaning (binomial) for ", current_disease)
  }
  
  # Sample a candidate subset of practices for fast model comparison.
  keep_pract <- sample(unique(dat_clean_beta$practice_code),
                       min(n_practices, dplyr::n_distinct(dat_clean_beta$practice_code)))
  sdat_beta  <- filter(dat_clean_beta, practice_code %in% keep_pract)
  sdat_binom <- filter(dat_full_binom, practice_code %in% keep_pract)
  
  # Specify candidate formulas.
  bf_beta_extended <- bf(rate_prop ~ simd_decile + log_ppgp_sc + age_sc*sex + dist_sc +
                           (1|practice_code) + (1|HSCP_id) + (1|HB_id))
  bf_beta_reduced  <- bf(rate_prop ~ log_ppgp_sc + age_sc*sex +
                           (1|practice_code) + (1|HSCP_id) + (1|HB_id))
  bf_zoib <- bf(
    rate_prop ~ log_ppgp_sc + age_sc*sex +
      (1|practice_code) + (1|HSCP_id) + (1|HB_id),
    zoi ~ log_ppgp_sc + age_sc*sex,
    coi ~ 1
  )
  
  bf_binom_reduced <- bf(disease_count | trials(patient_count) ~ log_ppgp_sc + age_sc*sex +
                           (1|practice_code) + (1|HSCP_id) + (1|HB_id))
  bf_binom_extended <- bf(disease_count | trials(patient_count) ~ simd_decile + log_ppgp_sc +
                            age_sc*sex + dist_sc + (1|practice_code) + (1|HSCP_id) + (1|HB_id))
  
  # Set priors.
  pri_beta <- c(
    set_prior("normal(0,1)", class="b"),
    set_prior("normal(0,1)", class="Intercept"),
    set_prior("gamma(2,0.1)", class="phi")
  )
  pri_zoib <- c(
    pri_beta,
    set_prior("normal(0,1)", class="b", dpar="zoi"),
    set_prior("normal(0,1)", class="Intercept", dpar="zoi"),
    set_prior("normal(0,1)", class="Intercept", dpar="coi")
  )
  pri_binom <- c(
    set_prior("normal(0,1)", class="b"),
    set_prior("normal(0,1)", class="Intercept")
  )
  
  # Configure sampler controls.
  ctrl_fast <- list(adapt_delta = 0.90, max_treedepth = 10)
  ctrl_zoib <- list(adapt_delta = 0.95, max_treedepth = 12)
  
  # Create save directory.
  if (is.null(save_path)) save_path <- paste0("models_", gsub("[^A-Za-z0-9]+","_", current_disease))
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define helpers for writing and fitting models.
  write_txt <- function(fname, txt) cat(paste0(txt, collapse = "\n"), file = file.path(save_path, fname))
  fit_and_save <- function(name, formula, data, family, prior, control) {
    rds_file <- file.path(save_path, paste0(name, ".rds"))
    warn_file <- file.path(save_path, paste0(name, "_warnings.txt"))
    err_file  <- file.path(save_path, paste0(name, "_error.txt"))
    if (file.exists(rds_file)) { message("Skipping ", name, " (exists)."); return(invisible(TRUE)) }
    message("Fitting ", name, " for ", current_disease, "...")
    warns <- character()
    fit <- tryCatch(
      withCallingHandlers(
        brm(formula, data = data, family = family, prior = prior,
            chains = chains, iter = iter, warmup = warmup, cores = cores,
            control = control, seed = 1234),
        warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
      ),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      write_txt(basename(err_file), c("MODEL FIT ERROR.", conditionMessage(fit)))
      message("Failed: ", name)
      return(invisible(FALSE))
    } else {
      saveRDS(fit, rds_file)
      if (length(warns) > 0) write_txt(basename(warn_file), unique(warns))
      rm(fit); gc()
      return(invisible(TRUE))
    }
  }
  
  # Fit candidate models per family on the subset.
  if (models_to_run %in% c("all","beta")) {
    fit_and_save("beta_ext", bf_beta_extended, sdat_beta, Beta(), pri_beta, ctrl_fast)
    fit_and_save("beta_red", bf_beta_reduced,  sdat_beta, Beta(), pri_beta, ctrl_fast)
    fit_and_save("zoib_red", bf_zoib,          sdat_beta, zero_one_inflated_beta(), pri_zoib, ctrl_zoib)
  }
  if (models_to_run %in% c("all","binomial") && nrow(sdat_binom) > 0) {
    fit_and_save("binomial_ext", bf_binom_extended, sdat_binom, binomial(link="logit"), pri_binom, ctrl_fast)
    fit_and_save("binomial_red", bf_binom_reduced,  sdat_binom, binomial(link="logit"), pri_binom, ctrl_fast)
  }
  
  # Compare within-family via LOO, then refit the best on full data.
  do_family_loo <- function(cand_names, family_label, full_data, family_mapper, prior_mapper, ctrl_mapper) {
    # Keep only existing fitted objects.
    existing <- file.exists(file.path(save_path, paste0(cand_names, ".rds")))
    if (!any(existing)) return(NULL)
    
    # Load candidate fits.
    fits <- lapply(cand_names[existing], function(nm) readRDS(file.path(save_path, paste0(nm, ".rds"))))
    names(fits) <- cand_names[existing]
    
    # Compute LOO objects.
    loo_list <- lapply(fits, function(f) tryCatch(loo(f), error = function(e) NULL))
    keep <- !vapply(loo_list, is.null, logical(1))
    if (!any(keep)) return(NULL)
    fits <- fits[keep]; loo_list <- loo_list[keep]
    
    # Rank by expected log predictive density.
    elpds <- vapply(loo_list, function(x) x$estimates["elpd_loo","Estimate"], numeric(1))
    rank_df <- data.frame(model = names(elpds), elpd_loo = elpds, row.names = NULL) %>% arrange(desc(elpds))
    write.csv(rank_df, file.path(save_path, paste0("loo_compare_simple_", family_label, ".csv")), row.names = FALSE)
    
    best <- rank_df$model[1]
    write_txt(paste0("best_model_", family_label, ".txt"), best)
    
    # Identify formula, family, priors, and control for the best model.
    best_formula <- switch(best,
                           beta_ext      = bf_beta_extended,
                           beta_red      = bf_beta_reduced,
                           zoib_red      = bf_zoib,
                           binomial_ext  = bf_binom_extended,
                           binomial_red  = bf_binom_reduced)
    
    best_family <- family_mapper[[best]]
    best_prior  <- prior_mapper[[best]]
    best_ctrl   <- ctrl_mapper[[best]]
    
    # Refit the best model on full data if not already present.
    full_file <- file.path(save_path, paste0("fit_full_", best, ".rds"))
    if (!file.exists(full_file)) {
      message("Refitting full data: ", best, " (", family_label, ") for ", current_disease, "...")
      warns <- character()
      refit <- tryCatch(
        withCallingHandlers(
          brm(best_formula, data = full_data, family = best_family, prior = best_prior,
              chains = chains, iter = iter*2, warmup = warmup*2, cores = cores,
              control = best_ctrl, seed = 1234),
          warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
        ),
        error = function(e) e
      )
      if (inherits(refit, "error")) {
        write_txt(paste0("fit_full_", best, "_error.txt"),
                  c("FULL REFIT ERROR.", conditionMessage(refit)))
        message("Full-data refit failed: ", best)
      } else {
        saveRDS(refit, full_file)
        if (length(warns) > 0) write_txt(paste0("fit_full_", best, "_warnings.txt"), unique(warns))
        rm(refit); gc()
      }
    } else {
      message("Skipping full-data refit (exists): ", basename(full_file))
    }
  }
  
  # Define mappers for families, priors, and controls.
  family_mapper <- list(
    beta_ext      = Beta(), beta_red = Beta(), zoib_red = zero_one_inflated_beta(),
    binomial_ext  = binomial(link="logit"), binomial_red = binomial(link="logit")
  )
  prior_mapper <- list(
    beta_ext      = pri_beta, beta_red = pri_beta, zoib_red = pri_zoib,
    binomial_ext  = pri_binom, binomial_red = pri_binom
  )
  ctrl_mapper <- list(
    beta_ext      = ctrl_fast, beta_red = ctrl_fast, zoib_red = ctrl_zoib,
    binomial_ext  = ctrl_fast, binomial_red = ctrl_fast
  )
  
  # Run per-family LOO comparison and full refits.
  cand_beta   <- c("beta_ext","beta_red","zoib_red")
  cand_binom  <- c("binomial_ext","binomial_red")
  
  if (models_to_run %in% c("all","beta")) {
    do_family_loo(cand_beta,   "beta",     dat_clean_beta, family_mapper, prior_mapper, ctrl_mapper)
  }
  if (models_to_run %in% c("all","binomial") && nrow(dat_full_binom) > 0) {
    do_family_loo(cand_binom,  "binomial", dat_full_binom, family_mapper, prior_mapper, ctrl_mapper)
  }
  
  # Return invisibly.
  invisible(NULL)
}

## MODEL SUMMARIES AND POSTERIOR CHECKS

# Summarize all saved models and run posterior predictive checks per disease.
check_models <- function(diseases, base_dir = ".") {

  # Helper to append text output to file.
  log_to_file <- function(path, ...) {
    cat(..., file = path, append = TRUE, sep = "")
  }
  
  # Iterate over diseases and inspect saved fits.
  for (dis in diseases) {
    save_dir <- file.path(base_dir, paste0("models_", gsub("[^a-zA-Z0-9]+", "_", dis)))
    if (!dir.exists(save_dir)) {
      message("Skipping ", dis, " — directory not found: ", save_dir, ".")
      next
    }
    
    files_rds <- list.files(save_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files_rds) == 0) {
      message("Skipping ", dis, " — no .rds files in ", save_dir, ".")
      next
    }
    
    log_file <- file.path(save_dir, paste0("summary_", dis, ".txt"))
    if (file.exists(log_file)) file.remove(log_file)
    
    log_to_file(log_file, "\n========== DISEASE: ", dis, " ==========\n\n")
    
    # Process full-data refits first, then candidate subset fits.
    ord <- order(!grepl("^fit_full_", basename(files_rds)), basename(files_rds))
    files_rds <- files_rds[ord]
    
    for (rf in files_rds) {
      model_name <- sub("\\.rds$", "", basename(rf))
      log_to_file(log_file, "\n--- Model: ", model_name, " ---\n")
      
      fit <- tryCatch(readRDS(rf), error = function(e) e)
      if (inherits(fit, "error")) {
        log_to_file(log_file, "Failed to read: ", rf, " (", conditionMessage(fit), ").\n")
        next
      }
      
      # Capture and write summary.
      log_to_file(log_file, "\n# Summary.\n")
      capture.output(print(fit), file = log_file, append = TRUE)
      
      # Compute and write LOO.
      log_to_file(log_file, "\n# LOO summary.\n")
      loo_obj <- tryCatch(loo(fit), error = function(e) NULL)
      if (is.null(loo_obj)) {
        log_to_file(log_file, "LOO not available.\n")
      } else {
        capture.output(print(loo_obj), file = log_file, append = TRUE)
      }
      
      # Posterior predictive checks — save plots.
      log_to_file(log_file, "\n# Posterior predictive checks saved to disk.\n")
      
      # Density overlay.
      g1 <- pp_check(fit, ndraws = 100) +
        ggtitle("Posterior Predictive Check: Density Overlay",
                subtitle = paste("Disease:", dis, "| Model:", model_name))
      ggsave(file.path(save_dir, paste0(model_name, "_ppc_density.png")), g1, width = 6, height = 4)
      
      # Means.
      g2 <- pp_check(fit, type = "stat", stat = "mean", ndraws = 200) +
        ggtitle("Posterior Predictive Check: Means",
                subtitle = paste("Disease:", dis, "| Model:", model_name))
      ggsave(file.path(save_dir, paste0(model_name, "_ppc_means.png")), g2, width = 6, height = 4)
      
      # Standard deviations.
      g3 <- pp_check(fit, type = "stat", stat = "sd", ndraws = 200) +
        ggtitle("Posterior Predictive Check: Standard Deviations",
                subtitle = paste("Disease:", dis, "| Model:", model_name))
      ggsave(file.path(save_dir, paste0(model_name, "_ppc_sd.png")), g3, width = 6, height = 4)
    }
    
    message("Finished summaries and posterior checks for ", dis, ".")
  }
  
  # Return invisibly.
  invisible(NULL)
}

# Example call using focus_diseases vector.
check_models(focus_diseases)


## PROJECTIONS

# Project disease counts for a single disease, family, and scenario.
project_disease_counts <- function(
    disease,
    family = c("beta","binomial"),
    population,
    final,
    base_dir = ".",
    area_types = c("Council","Health and Social Care Partnership","Health Board"),
    include_datazones = FALSE,
    years = 2023:2043,
    scenario = c("population","population_low","population_high"),
    chunk_rows = 5000            
){
  # Check that inputs are data frames.
  stopifnot(is.data.frame(population), is.data.frame(final))
  family   <- match.arg(family)
  scenario <- match.arg(scenario)
  
  message(sprintf("Start project_disease_counts. Disease: %s. Family: %s. Scenario: %s.",
                  disease, family, scenario))
  
  # Define model folder path.
  prj_folder <- file.path(base_dir, paste0("models_", gsub("[^A-Za-z0-9]+","_", disease)))
  if (!dir.exists(prj_folder)) stop("Model folder not found: ", prj_folder)
  
  # Helper function to pick the best available model file.
  pick_full_model_path <- function(fam) {
    best_file <- file.path(prj_folder, paste0("best_model_", fam, ".txt"))
    if (file.exists(best_file)) {
      best <- trimws(readLines(best_file, warn = FALSE)[1])
      cand <- file.path(prj_folder, paste0("fit_full_", best, ".rds"))
      if (file.exists(cand)) return(cand)
    }
    fam_pat <- if (fam == "beta") "^fit_full_(beta|zoib)_" else "^fit_full_binomial_"
    ff <- list.files(prj_folder, pattern = "\\.rds$", full.names = TRUE)
    ff <- ff[grepl(fam_pat, basename(ff))]
    if (!length(ff)) stop("No full model found for family '", fam, "' in ", prj_folder)
    ff[1]
  }
  
  # Define 8 disease age bands and their midpoints.
  disease_age_levels <- c("0 to 4","5 to 14","15 to 24","25 to 44","45 to 64","65 to 74","75 to 84","85 and over")
  age_mid_lookup <- tibble(
    age_bracket = disease_age_levels,
    age_mid = c(2.5, 10, 20, 35, 55, 70, 80, 90)
  )
  
  # Helper to collapse detailed population ages into 8 disease age bands.
  collapse_pop_age <- function(df) {
    df %>%
      mutate(age_bucket = case_when(
        age_bracket %in% c("0 to 4")                           ~ "0 to 4",
        age_bracket %in% c("5 to 9","10 to 14")                ~ "5 to 14",
        age_bracket %in% c("15 to 19","20 to 24")              ~ "15 to 24",
        age_bracket %in% c("25 to 29","30 to 34","35 to 39","40 to 44") ~ "25 to 44",
        age_bracket %in% c("45 to 49","50 to 54","55 to 59","60 to 64") ~ "45 to 64",
        age_bracket %in% c("65 to 69","70 to 74")              ~ "65 to 74",
        age_bracket %in% c("75 to 79","80 to 84")              ~ "75 to 84",
        age_bracket %in% c("85 to 89","90 and over")           ~ "85 and over",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(age_bucket)) %>%
      group_by(area_code, area_name, area_type, year, sex, age_bucket) %>%
      summarise(
        across(c(population_low, population, population_high), \(x) sum(x, na.rm = TRUE)),
        .groups="drop"
      ) %>%
      rename(age_bracket = age_bucket)
  }
  
  # Restrict population to requested area types and drop sex = all.
  wanted_types <- unique(c(area_types, if (isTRUE(include_datazones)) "DataZone"))
  if (!isTRUE(include_datazones)) wanted_types <- setdiff(wanted_types, "DataZone")
  pop0 <- population %>%
    filter(area_type %in% wanted_types, year %in% years, sex %in% c("female","male"))
  
  # Collapse population into disease bands and pick scenario-specific population.
  pop_collapsed <- collapse_pop_age(pop0)
  pop_long <- pop_collapsed %>%
    mutate(N = case_when(
      scenario == "population_low"  ~ population_low,
      scenario == "population_high" ~ population_high,
      TRUE                          ~ population
    )) %>%
    select(area_code, area_name, area_type, year, sex, age_bracket, N)
  
  # Filter training data for scaling and check that rows exist.
  train <- final %>%
    filter(disease_name == disease, sex %in% c("female","male"), age_bracket %in% disease_age_levels) %>%
    mutate(
      log_ppgp = as.numeric(log_ppgp),
      nearest_practice_dist = as.numeric(nearest_practice_dist),
      age_mid = as.numeric(age_mid)
    )
  if (!nrow(train)) stop("No rows in 'final' for disease = ", disease)
  
  # Compute scaling means and standard deviations for covariates.
  m_age  <- mean(train$age_mid, na.rm = TRUE); s_age  <- sd(train$age_mid, na.rm = TRUE); if (!is.finite(s_age)||s_age==0) s_age <- 1
  m_ppgp <- mean(train$log_ppgp, na.rm = TRUE); s_ppgp<- sd(train$log_ppgp, na.rm = TRUE); if (!is.finite(s_ppgp)||s_ppgp==0) s_ppgp <- 1
  m_dist <- mean(train$nearest_practice_dist, na.rm = TRUE); s_dist<- sd(train$nearest_practice_dist, na.rm = TRUE); if (!is.finite(s_dist)||s_dist==0) s_dist <- 1
  latest_fit_year <- max(train$year, na.rm = TRUE)
  
  # Helper to pick ID column for area type.
  area_id_col <- function(tp) case_when(
    tp == "Council"                            ~ "CA_id",
    tp == "Health and Social Care Partnership" ~ "HSCP_id",
    tp == "Health Board"                       ~ "HB_id",
    tp == "DataZone"                           ~ NA_character_,
    TRUE ~ NA_character_
  )
  
  # Helper to build area-level covariates.
  build_area_cov <- function(tp) {
    idcol <- area_id_col(tp)
    if (is.na(idcol)) return(NULL)
    train %>%
      filter(year == latest_fit_year, !is.na(.data[[idcol]])) %>%
      group_by(area_code = .data[[idcol]], area_type = tp) %>%
      summarise(
        simd_decile = suppressWarnings(mean(as.numeric(simd_decile), na.rm = TRUE)),
        log_ppgp    = mean(log_ppgp, na.rm = TRUE),
        dist        = mean(nearest_practice_dist, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        log_ppgp_sc = (log_ppgp - m_ppgp)/s_ppgp,
        dist_sc     = (dist - m_dist)/s_dist
      ) %>%
      select(area_type, area_code, simd_decile, log_ppgp_sc, dist_sc)
  }
  area_cov <- map_dfr(wanted_types, build_area_cov)
  if (!nrow(area_cov)) stop("No area covariates found for requested area_types.")
  
  # Join population with area covariates and compute scaled values.
  pop_ready <- pop_long %>%
    left_join(area_cov, by = c("area_type","area_code")) %>%
    left_join(age_mid_lookup, by = "age_bracket") %>%
    mutate(
      age_sc = (age_mid - m_age)/s_age,
      log_ppgp_sc = ifelse(is.na(log_ppgp_sc), 0, log_ppgp_sc),
      dist_sc     = ifelse(is.na(dist_sc), 0, dist_sc),
      simd_decile = ifelse(is.na(simd_decile), mean(train$simd_decile, na.rm = TRUE), simd_decile)
    )
  
  # Load fitted model object.
  fam_label <- if (family == "beta") "beta" else "binomial"
  fit_path  <- pick_full_model_path(fam_label)
  fit <- readRDS(fit_path)
  message("Loaded full model: ", basename(fit_path), "  (family = ", fit$family$family, ")")
  
  # Determine model structure flags.
  base <- basename(fit_path)
  is_beta     <- grepl("beta", base) || fit$family$family %in% c("beta","zero_one_inflated_beta")
  is_binomial <- grepl("binomial", base) || fit$family$family == "binomial"
  is_ext      <- grepl("_ext", base)
  is_red      <- grepl("_red", base)
  
  # Choose required covariates for new data.
  need_cols <- c("age_sc","sex")
  if (is_ext) need_cols <- c(need_cols, "simd_decile","log_ppgp_sc","dist_sc")
  if (is_red) need_cols <- c(need_cols, "log_ppgp_sc")
  need_cols <- unique(need_cols)
  
  # Build prediction dataset.
  newdata <- pop_ready %>%
    select(area_type, area_code, area_name, year, sex, age_bracket, N, age_mid, all_of(intersect(need_cols, names(pop_ready))))
  if (is_binomial) {
    newdata <- newdata %>% mutate(patient_count = as.numeric(round(pmax(N,0))))
  }
  newdata$sex <- factor(as.character(newdata$sex), levels = c("female","male"))
  
  message(sprintf("Prepare newdata. Rows: %d. Chunk size: %d.", nrow(newdata), chunk_rows))
  
  n <- nrow(newdata)
  if (n == 0) return(newdata %>% mutate(pred_rate_mean=NA_real_, pred_rate_l95=NA_real_, pred_rate_u95=NA_real_,
                                        pred_count_mean=NA_real_, pred_count_l95=NA_real_, pred_count_u95=NA_real_) )
  if (is.null(chunk_rows) || !is.finite(chunk_rows) || chunk_rows < 1) chunk_rows <- n
  
  # Split indices into chunks.
  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_rows))
  
  force(fit) 
  
  # Summarize one chunk.
  predict_chunk <- function(ii) {
    nd <- newdata[ii, , drop = FALSE]
    
    # Context about this chunk.
    yrs <- if (nrow(nd)) sprintf("%s to %s", suppressWarnings(min(nd$year, na.rm = TRUE)),
                                 suppressWarnings(max(nd$year, na.rm = TRUE))) else "NA"
    sexes <- if (nrow(nd)) paste(unique(as.character(nd$sex)), collapse = ", ") else "NA"
    
    message(sprintf("Predict chunk. Rows: %d. Years: %s. Sexes: %s. Indices: %d to %d.",
                    nrow(nd), yrs, sexes, min(ii), max(ii)))
    
    # Defensive checks.
    if (nrow(nd) == 0) {
      stop("Encounter empty chunk. Stop.")
    }
    if (!inherits(fit, "brmsfit")) {
      stop(sprintf("Fit is not a brmsfit. Class: %s.", paste(class(fit), collapse = ", ")))
    }
    if (is.null(fit$fit) || is.null(fit$fit@sim)) {
      stop("Model fit has no posterior draws (fit@sim is NULL).")
    }
    
    re_form <- NA
    
    if (is_beta) {
      mu_draws <- tryCatch(
        posterior_epred(fit, newdata = nd, re_formula = re_form, allow_new_levels = TRUE),
        error = function(e) {
          message(sprintf("Error in posterior_epred for beta. Rows: %d. Years: %s. %s",
                          nrow(nd), yrs, conditionMessage(e)))
          stop(e)
        }
      )
      
      # Summaries.
      rate_mean <- colMeans(mu_draws, na.rm = TRUE)
      rate_q    <- apply(mu_draws, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
      
      count_draws <- sweep(mu_draws, 2, nd$N, `*`)
      cnt_mean <- colMeans(count_draws, na.rm = TRUE)
      cnt_q    <- apply(count_draws, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
      
      # Free memory promptly.
      rm(mu_draws, count_draws); gc()
      
      res <- nd %>% mutate(
        pred_rate_mean = rate_mean, pred_rate_l95 = rate_q[1,], pred_rate_u95 = rate_q[2,],
        pred_count_mean = cnt_mean, pred_count_l95 = cnt_q[1,], pred_count_u95 = cnt_q[2,]
      )
      message("Finish chunk.")
      return(res)
      
    } else {  
      mu_draws <- tryCatch(
        posterior_epred(fit, newdata = nd, re_formula = re_form, allow_new_levels = TRUE),
        error = function(e) {
          message(sprintf("Error in posterior_epred for binomial. Rows: %d. Years: %s. %s",
                          nrow(nd), yrs, conditionMessage(e)))
          stop(e)
        }
      )
      
      cnt_mean <- colMeans(mu_draws, na.rm = TRUE)
      cnt_q    <- apply(mu_draws, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
      
      # Guard against zero/NA patient_count.
      denom <- pmax(nd$patient_count %||% NA_real_, 1)
      if (any(!is.finite(denom))) {
        stop("Patient_count contains non-finite values in binomial branch.")
      }
      
      rate_draws <- sweep(mu_draws, 2, denom, `/`)
      rate_mean <- colMeans(rate_draws, na.rm = TRUE)
      rate_q    <- apply(rate_draws, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
      
      rm(mu_draws, rate_draws); gc()
      
      res <- nd %>% mutate(
        pred_rate_mean = rate_mean, pred_rate_l95 = rate_q[1,], pred_rate_u95 = rate_q[2,],
        pred_count_mean = cnt_mean, pred_count_l95 = cnt_q[1,], pred_count_u95 = cnt_q[2,]
      )
      message("Finish chunk.")
      return(res)
    }
  }
  
  # Evaluate chunks sequentially and bind immediately.
  out_base <- map_dfr(idx, predict_chunk)
  
  # Aggregate to sex = all within each age/area/year.
  out_mf <- out_base %>%
    filter(sex %in% c("female","male")) %>%
    group_by(area_type, area_code, area_name, year, age_bracket) %>%
    summarise(
      sex = "all",
      N = sum(N, na.rm = TRUE),
      pred_count_mean = sum(pred_count_mean, na.rm = TRUE),
      pred_count_l95  = sum(pred_count_l95,  na.rm = TRUE),
      pred_count_u95  = sum(pred_count_u95,  na.rm = TRUE),
      .groups="drop"
    ) %>%
    mutate(
      pred_rate_mean = ifelse(N > 0, pred_count_mean/N, NA_real_),
      pred_rate_l95  = ifelse(N > 0, pred_count_l95 /N, NA_real_),
      pred_rate_u95  = ifelse(N > 0, pred_count_u95 /N, NA_real_)
    )
  
  # Combine female, male, and all together and add family/model metadata.
  out_all <- bind_rows(
    out_base %>% mutate(sex = as.character(sex)),
    out_mf
  ) %>%
    mutate(
      sex = factor(sex, levels = c("female","male","all")),
      family = ifelse(is_beta,"beta","binomial"),
      model_file = basename(fit_path)
    ) %>%
    arrange(area_type, area_code, year, factor(age_bracket, levels = disease_age_levels), sex)
  
  # Re-aggregate all sexes at the very end to ensure consistency.
  out_all_sex <- out_all %>%
    filter(sex %in% c("female","male")) %>%
    group_by(family, model_file,
             area_type, area_code, area_name, year, age_bracket) %>%
    summarise(
      sex = "all",
      N = sum(N, na.rm = TRUE),
      pred_count_mean = sum(pred_count_mean, na.rm = TRUE),
      pred_count_l95  = sum(pred_count_l95,  na.rm = TRUE),
      pred_count_u95  = sum(pred_count_u95,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pred_rate_mean = ifelse(N > 0, pred_count_mean / N, NA_real_),
      pred_rate_l95  = ifelse(N > 0, pred_count_l95  / N, NA_real_),
      pred_rate_u95  = ifelse(N > 0, pred_count_u95  / N, NA_real_)
    )
  
  # Replace any prior sex = all rows with newly aggregated ones.
  out_all <- out_all %>%
    filter(sex %in% c("female","male")) %>%
    bind_rows(out_all_sex) %>%
    arrange(area_type, area_code, year,
            factor(age_bracket, levels = disease_age_levels),
            sex)
  
  return(out_all)
}

# Project disease counts for all scenarios for one disease-family combination.
project_disease_counts_all <- function(
    disease,
    family = c("beta","binomial"),
    population,
    final,
    base_dir = ".",
    area_types = c("Council","Health and Social Care Partnership","Health Board"),
    include_datazones = FALSE,
    years = 2023:2043,
    chunk_rows = 5000 
){
  family <- match.arg(family)
  scenarios <- c("population_low","population","population_high")
  
  message(sprintf("Run all scenarios for %s. Family: %s.", disease, family))
  
  # Run for each scenario and tag variant.
  results <- map_dfr(scenarios, function(scen) {
    message(sprintf("Start scenario. Scenario: %s.", scen))
    project_disease_counts(
      disease = disease,
      family = family,
      population = population,
      final = final,
      base_dir = base_dir,
      area_types = area_types,
      include_datazones = include_datazones,
      years = years,
      scenario = scen,
      chunk_rows = chunk_rows
    ) %>%
      mutate(
        variant = case_when(
          scen == "population_low"  ~ "low",
          scen == "population"      ~ "principal",
          scen == "population_high" ~ "high"
        )
      )
  })
  
  # Drop unused columns and reshape to wide format by variant.
  results <- results %>%
    select(-any_of(c("N", "age_mid", "age_sc", "log_ppgp_sc", "patient_count"))) %>%
    pivot_wider(
      id_cols = c(area_type, area_code, area_name, year, sex, age_bracket, family, model_file),
      names_from = variant,
      values_from = c(pred_rate_mean, pred_rate_l95, pred_rate_u95,
                      pred_count_mean, pred_count_l95, pred_count_u95),
      names_glue = "{.value}_{variant}"
    )
  
  message("Finish scenarios and reshape results.")
  
  # Rename columns to match required schema.
  results <- results %>%
    rename(
      low_rate       = pred_rate_mean_low,
      low_rate_l95   = pred_rate_l95_low,
      low_rate_u95   = pred_rate_u95_low,
      rate           = pred_rate_mean_principal,
      rate_l95       = pred_rate_l95_principal,
      rate_u95       = pred_rate_u95_principal,
      high_rate      = pred_rate_mean_high,
      high_rate_l95  = pred_rate_l95_high,
      high_rate_u95  = pred_rate_u95_high,
      low_count      = pred_count_mean_low,
      low_count_l95  = pred_count_l95_low,
      low_count_u95  = pred_count_u95_low,
      count          = pred_count_mean_principal,
      count_l95      = pred_count_l95_principal,
      count_u95      = pred_count_u95_principal,
      high_count     = pred_count_mean_high,
      high_count_l95 = pred_count_l95_high,
      high_count_u95 = pred_count_u95_high
    ) %>%
    mutate(across(
      c(low_rate, low_rate_l95, low_rate_u95,
        rate, rate_l95, rate_u95,
        high_rate, high_rate_l95, high_rate_u95),
      ~ .x * 100
    ))
  
  return(results)
}

# Project disease counts for all diseases and both families.
project_all_diseases <- function(
    focus_diseases,
    population,
    final,
    base_dir = ".",
    area_types = c("Council","Health and Social Care Partnership","Health Board", "DataZone"),
    include_datazones = FALSE,
    years = 2023:2043,
    chunk_rows = 5000   # <-- pass through to inner function
){
  families <- c("beta", "binomial")
  
  message(sprintf("Start projecting diseases. Diseases: %s.", paste(focus_diseases, collapse = ", ")))
  
  # Run for each disease and family.
  results <- map_dfr(focus_diseases, function(disease) {
    map_dfr(families, function(fam) {
      message("Running: ", disease, " (family = ", fam, ")")
      out <- project_disease_counts_all(
        disease = disease,
        family = fam,
        population = population,
        final = final,
        base_dir = base_dir,
        area_types = area_types,
        include_datazones = include_datazones,
        years = years,
        chunk_rows = chunk_rows
      ) %>%
        mutate(disease = disease)
      message(sprintf("Finish family. Disease: %s. Family: %s.", disease, fam))
      return(out)
    })
  })
  
  # Reorder so that disease column is after area_name.
  results <- results %>%
    relocate(disease, .after = area_name)
  
  return(results)
}

# Run brms models.
for (dis in focus_diseases) {
  save_dir <- paste0("models_", gsub("[^a-zA-Z0-9]+", "_", dis))
  if (dir.exists(save_dir) && length(list.files(save_dir, pattern = "\\.rds$", full.names = TRUE)) > 0) {
    message("Skipping ", dis, " because models already exist in ", save_dir)
  } else {
    message("Running models for ", dis, " and saving to ", save_dir)
    run_brms_models(final, dis, save_path = save_dir)
  }
}

projections <- project_all_diseases(focus_diseases, population, final)


get_growth <- function(projections, family = "beta") {
  # Slice projections data.
  projections_beta <- projections %>% filter(family == family)
  
  # Aggregate across all Health Boards to create Scotland-level totals.
  scotland_proj <- projections_beta %>%
    filter(area_type == "Health Board", sex == "all") %>%
    group_by(disease, year) %>%
    summarise(
      count = sum(count, na.rm = TRUE),
      N     = sum(count / (rate/100), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(rate = 100 * count / N)  # recompute prevalence (%)
  
  # Extract 2023 baseline and 2043 projections.
  baseline <- scotland_proj %>%
    filter(year == 2023) %>%
    select(disease, base_rate = rate, base_count = count)
  
  projection <- scotland_proj %>%
    filter(year == 2043) %>%
    select(disease, proj_rate = rate, proj_count = count)
  
  # Join and compute average annual increase.
  growth <- baseline %>%
    inner_join(projection, by = "disease") %>%
    mutate(
      annual_increase_pp = (proj_rate - base_rate) / (2043 - 2023),
      annual_increase_pct = (proj_rate / base_rate)^(1/(2043-2023)) - 1
    )
  
  return(growth)
}
