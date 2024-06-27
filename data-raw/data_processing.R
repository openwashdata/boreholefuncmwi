# Description ------------------------------------------------------------------
# R script to process uploaded raw data into a tidy, analysis-ready data frame
# Load packages ----------------------------------------------------------------
## Run the following code in console if you don't have the packages
## install.packages(c("usethis", "fs", "here", "readr", "openxlsx", "dplyr", "janitor", "tidyr"))
library(usethis)
library(fs)
library(here)
library(readr)
library(openxlsx)
library(dplyr)
library(janitor)
library(tidyr)
# Read data --------------------------------------------------------------------
borehole_malawi <- read_csv2("data-raw/dataset.csv")

# Tidy data --------------------------------------------------------------------

## Add ID numbers --------------------------------------------------------------
borehole_malawi <- borehole_malawi |>
  mutate(id = seq(1:n())) |>
  relocate(id)


# Rename columns from survey question to short variable name -------------------
borehole_malawi<-borehole_malawi |>
  rename(date = today,
         committee_members = `How many people are on the Hand pump borehole Committee?`,
         last_meeting_participants = `About how many community members attended the last meeting? (99 is don’t know)`,
         annual_budget = `How much money do u need to manage this borehole without financial problems annually?`,
         village = `Village name`,
         number_households = `How many households use (d) this hand pump borehole?`,

         tariff_costs_consider = `What costs were considered when setting the tariff or user fee?`,
         tariff_frequency = `How often is the tariff/user fee collected?`,
         tariff_amount = `How much is the tariff/user fee (in Kwacha)?`,
         total_money = `How much do you source to support operations and maintenance annually?`,
         tariff_hh_number=`How many households in the community paid a water fee the last time it was collected?`,

         distance_materials = `How far away are (were) the materials you use for hand pump borehole repairs (ex. spare parts, tools, etc.)?`,
         service_provider = `Is there a service provider or someone responsible for operating and/or maintaining this hand pump borehole or water system?...142`,
         preventive_maintenance =`Do you conduct preventive maintenance?`,
         functional_status =`Functional status of the borehole`,
         role="Main role of the respondent")


## Filter data with 17 important variables --------------------------------------
borehole_malawi_small <- borehole_malawi |>
  select(id,
         date,
         role,

         committee_members,
         last_meeting_participants,
         annual_budget,

         village,
         number_households,

         tariff_costs_consider,
         tariff_frequency,
         tariff_amount,
         total_money,

         tariff_hh_number,
         distance_materials,
         service_provider,
         preventive_maintenance,
         functional_status)


## Remove the entries where the borehole no longer exists or abandoned ---------
borehole_malawi_small<-borehole_malawi_small |>
  clean_names() |>
  filter(functional_status != 'No longer exists or abandoned')


# Recode functionality column --------------------------------------------------
borehole_malawi_small<-borehole_malawi_small |>
  mutate(
      functional_status = case_when(
      functional_status == "Functional" ~ "Yes",
      functional_status == "Not functional" ~ "No",
      functional_status == 'Partially functional but in need of repair'~"No"
    )
  )


## Simplify entry of distance_materials variable -------------------------------
borehole_malawi_small <- borehole_malawi_small %>%
  mutate(
    distance_materials = case_when(
      distance_materials == "Accessible within the community" ~ "Within community",
      TRUE ~ as.character(distance_materials)
    )
  )


## Modify village names --------------------------------------------------------
borehole_malawi_small <- borehole_malawi_small |>
  # Remove the records where village name is missing or Nb
  filter(!is.na(village) & village != "Nb") |>
  # Recode village names
  mutate(
    village = case_when(
      village == "Mlirasaambo" ~ "Mlirasambo",
      village == "Mlirasaambo" ~ "Mlirasaambo",
      village == "Mulirasambo" ~ "Mlirasambo",
      village == "Mugabi" ~ "Mugabe",
      TRUE ~ as.character(village)
    )
  )


# Recode the variable tariff_costs_consider ------------------------------------
borehole_malawi_small <- borehole_malawi_small %>%
  mutate(
    tariff_costs_consider = case_when(
      tariff_costs_consider == "Affordability Maintenance costs" ~ "Maintenance costs",
      tariff_costs_consider == "Total replacement cost for the system" ~ "Maintenance costs",
      TRUE ~ as.character(tariff_costs_consider)
    )
  )


# Modify tariff_amount variable

# Define the modification function
modify <- function(dataset) {
  dataset <- as.numeric(dataset)  # Convert the column to numeric to handle NAs

  # 1. Bring down values that are greater than 2000 to 2000
  dataset[dataset > 2000] <- 2000

  # 2. When it finds values that have five digits, the last digit should be deleted
  dataset <- ifelse(nchar(as.character(dataset)) == 5, as.numeric(substring(as.character(dataset), 1, 4)), dataset)

  # 3. When it finds a single number, it should add 2 zeros to it
  dataset <- ifelse(dataset < 10, dataset * 100, dataset)

  return(dataset)
}

# Apply the modify function to the variable tariff_amount
borehole_malawi_small$tariff_amount <- modify(borehole_malawi_small$tariff_amount)


# Replace NAs in columns tariff_hh_number, tariff_amount, preventive_maintenance, tariff_frequency and tariff_costs_consider
borehole_malawi_small <- borehole_malawi_small |>
  mutate(tariff_hh_number = replace_na(tariff_hh_number, 0)) |>
  # same for tarrif amount
  mutate(tariff_amount = replace_na(tariff_amount, 0)) |>
  # preventive mantainance
  mutate(preventive_maintenance = replace_na(preventive_maintenance,'No')) |>
  # if tarrif frequency is missing it means those people do not pay tariff
  # because that question as not applicable to them. Therefore>None
  mutate(tariff_frequency = replace_na(tariff_frequency,'None')) |>
  #those who have NA on consideration for setting the tarrif are the ones that drink from boreholes that do not have tarrif system
  mutate(tariff_costs_consider = replace_na(tariff_costs_consider,'No tarrif system set'))


# Replace Yes/No with 1/0 in multiple columns
borehole_malawi_small <- borehole_malawi_small %>%
  mutate_at(vars(service_provider:functional_status), ~ifelse(. == "Yes", 1, 0))

# Convert categorical variables to factors
borehole_malawi_small |>
  mutate(across(c(role, village, tariff_costs_consider, tariff_frequency,
                  distance_materials, service_provider, preventive_maintenance,
                  functional_status), as.factor))

# Save final dataset
boreholefuncmwi <- borehole_malawi_small

# Export Data ------------------------------------------------------------------
usethis::use_data(boreholefuncmwi, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
readr::write_csv(boreholefuncmwi,
                 here::here("inst", "extdata", paste0("boreholefuncmwi", ".csv")))
openxlsx::write.xlsx(boreholefuncmwi,
                     here::here("inst", "extdata", paste0("boreholefuncmwi", ".xlsx")))
