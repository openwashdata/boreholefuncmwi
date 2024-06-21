# Description ------------------------------------------------------------------
# R script to process uploaded raw data into a tidy, analysis-ready data frame
# Load packages ----------------------------------------------------------------
## Run the following code in console if you don't have the packages
## install.packages(c("usethis", "fs", "here", "readr", "openxlsx"))
library(usethis)
library(fs)
library(here)
library(readr)
library(openxlsx)


#libraries used for cleaning the data

library(tidyverse)

#clean column names
library (janitor)

#for EDA
library(explore)

#here here package
library(here)

#making simple tables
library(knitr)

library(dplyr)

#for making dummy variables
library(fastDummies)


# Read data --------------------------------------------------------------------
borehole_malawi <- read_csv2("data-raw/dataset.csv")


# Tidy data --------------------------------------------------------------------

#add id numbers
borehole_malawi <- borehole_malawi |>
  mutate(id = seq(1:n())) |>
  relocate(id)


#renaming column names

#rename some relevant questions
borehole_malawi<-borehole_malawi |>
  rename(date=today,
         committee_members=`How many people are on the Hand pump borehole Committee?`,
         last_meeting_participants=`About how many community members attended the last meeting? (99 is don’t know)`,
         annual_budget=`How much money do u need to manage this borehole without financial problems annually?`)



#objective 1: Knowing the number of households using boreholes in each village village.
borehole_malawi<-borehole_malawi |>
  rename(village=`Village name`,
        number_households= `How many households use (d) this hand pump borehole?`)



#objective 2: Checking if there is a relationship between the costs that were considered before setting the tariff and the amount that is agreed to be paid as a tariff per month now
borehole_malawi<-borehole_malawi |>
  rename( tariff_costs_consider=`What costs were considered when setting the tariff or user fee?`,
          tariff_frequency=`How often is the tariff/user fee collected?`,
          tariff_amount=`How much is the tariff/user fee (in Kwacha)?`,
          total_money=`How much do you source to support operations and maintenance annually?`)



#objective 3: Checking if the actual tariff collected per month, distance to where borehole spare parts are, presence of a service provider and conducting  preventive maintenance lead to the functionality of boreholes
borehole_malawi<-borehole_malawi |>
  rename(tariff_hh_number=`How many households in the community paid a water fee the last time it was collected?`,
         distance_materials= `How far away are (were) the materials you use for hand pump borehole repairs (ex. spare parts, tools, etc.)?`,
         service_provider= `Is there a service provider or someone responsible for operating and/or maintaining this hand pump borehole or water system?...142`,
         preventive_maintenance=`Do you conduct preventive maintenance?`,
         functional_status=`Functional status of the borehole`,
         role="Main role of the respondent")


#create a smaller dataset with 17 important variables
borehole_malawi_small <- borehole_malawi |>
  select(id,
         date,
         role,

         committee_members,
         last_meeting_participants,
         annual_budget,

         #objective 1
         village,
         number_households,

         #objective2
         tariff_costs_consider,
         tariff_frequency,
         tariff_amount,
         total_money,

         #objective 3
         tariff_hh_number,
         distance_materials,
         service_provider,
         preventive_maintenance,
         functional_status)


#remove the entries where the borehole no longer exists or abandoned
borehole_malawi_small<-borehole_malawi_small |>
  clean_names() |>
  filter(functional_status != 'No longer exists or abandoned')


#recode functionality column
borehole_malawi_small<-borehole_malawi_small |>
  mutate(
      functional_status = case_when(
      functional_status == "Functional" ~ "Yes",
      functional_status == "Not functional" ~ "No",
      functional_status == 'Partially functional but in need of repair'~"No"

    )
  )


#Simplify entry of distance_materials variable
borehole_malawi_small <- borehole_malawi_small %>%
  mutate(
    distance_materials = case_when(
      distance_materials == "Accessible within the community" ~ "Within community",
      TRUE ~ as.character(distance_materials)
    )
  )


#remove the records where village name is missing or Nb
borehole_malawi_small <- borehole_malawi_small %>%
  filter(!is.na(village) & village != "Nb")



#recode village names
borehole_malawi_small <- borehole_malawi_small %>%
  mutate(
    village = case_when(
      village == "Mlirasaambo" ~ "Mlirasambo",
      village == "Mlirasaambo" ~ "Mlirasaambo",
      village == "Mulirasambo" ~ "Mlirasambo",
      village == "Mugabi" ~ "Mugabe",
      TRUE ~ as.character(village)
    )
  )


#recode the variable tariff_costs_consider
borehole_malawi_small <- borehole_malawi_small %>%
  mutate(
    tariff_costs_consider = case_when(
      tariff_costs_consider == "Affordability Maintenance costs" ~ "Maintenance costs",
      tariff_costs_consider == "Total replacement cost for the system" ~ "Maintenance costs",
      TRUE ~ as.character(tariff_costs_consider)
    )
  )


#Modify tariff_amount variable

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


#replace NAs in columns tariff_hh_number, tariff_amount, preventive_maintenance, tariff_frequency and tariff_costs_consider
borehole_malawi_small <- borehole_malawi_small |>
  mutate(tariff_hh_number = replace_na(tariff_hh_number, 0))

#same for tarrif amount
borehole_malawi_small <- borehole_malawi_small |>
  mutate(tariff_amount = replace_na(tariff_amount, 0))

#preventive mantainance
borehole_malawi_small <- borehole_malawi_small |>
  mutate(preventive_maintenance = replace_na(preventive_maintenance,'No'))

#if tarrif frequency is missing it means those people do not pay tarrif beause that question as not applicable to them. Therefore>None
borehole_malawi_small <- borehole_malawi_small |>
  mutate(tariff_frequency = replace_na(tariff_frequency,'None'))

#those who have NA on consideration for setting the tarrif are the ones that drink from boreholes that do not have tarrif system
borehole_malawi_small <- borehole_malawi_small |>
  mutate(tariff_costs_consider = replace_na(tariff_costs_consider,'No tarrif system set'))


# Replace Yes/No with 1/0 in multiple columns
borehole_malawi_small <- borehole_malawi_small %>%
  mutate_at(vars(service_provider:functional_status), ~ifelse(. == "Yes", 1, 0))

#convert categorical variables to factors
borehole_malawi_small$village <- as.factor(borehole_malawi_small$village)
borehole_malawi_small$tariff_costs_consider <- as.factor(borehole_malawi_small$tariff_costs_consider)
borehole_malawi_small$tariff_frequency <- as.factor(borehole_malawi_small$tariff_frequency)
borehole_malawi_small$distance_materials <- as.factor(borehole_malawi_small$distance_materials)
borehole_malawi_small$service_provider <- as.factor(borehole_malawi_small$service_provider)
borehole_malawi_small$preventive_maintenance <- as.factor(borehole_malawi_small$preventive_maintenance)
borehole_malawi_small$functional_status <- as.factor(borehole_malawi_small$functional_status)


#save final dataset
boreholefuncmwi <- borehole_malawi_small



# Export Data ------------------------------------------------------------------
usethis::use_data(boreholefuncmwi, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
readr::write_csv(boreholefuncmwi,
                 here::here("inst", "extdata", paste0("boreholefuncmwi", ".csv")))
openxlsx::write.xlsx(boreholefuncmwi,
                     here::here("inst", "extdata", paste0("boreholefuncmwi", ".xlsx")))
