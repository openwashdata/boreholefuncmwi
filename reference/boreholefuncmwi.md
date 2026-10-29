# boreholefuncmwi: Analysis of boreholes' functionality in Northern Malawi

The data is obtained from a survey conducted in Wasambo in Karonga
district (Northen Malawi). This data is about boreholes and handpumps
that people mostly rely on for drinking water, secondary to wells and
rivers. The data were collected in June 2013.

## Usage

``` r
boreholefuncmwi
```

## Format

A tibble with 108 rows and 17 variables

- id:

  Identification number of the observation

- date:

  Date the survey was filled out

- role:

  Main role of the respondent. Options include 'Area Mechanic',
  'Chief/Village Headman', 'Community member', 'Councilor', 'Head
  teacher/Teacher', 'HSA/Health officer', 'Hand pump borehole committee
  chair', 'Hand pump borehole committee member', 'Hand pump borehole
  owner', 'Water Seller', and 'Water User Association/Water Board
  member'

- committee_members:

  How many people are on the Hand pump borehole Committee as of 2023?

- last_meeting_participants:

  How many community members attended the last meeting? (Label 99 for
  unknown number of attendants)

- annual_budget:

  How much money in Malawi Kwacha do you need to manage this borehole
  without financial problems annually?

- village:

  Name of the village

- number_households:

  How many households use this hand pump borehole?

- tariff_costs_consider:

  What costs were considered when setting the tariff or user fee?
  Options including Maintenance costs, Operation costs - e.g. salary of
  the water seller, Total replacement cost for the system, Set by local
  government, and Bill payments.

- tariff_frequency:

  How often is the tariff/user fee collected? Including per month, per 2
  months, per quarter, per year and when required for repairs.

- tariff_amount:

  How much is the tariff/user fee in Kwacha?

- total_money:

  How much money in Malawi Kwacha do you source to support operations
  and maintenance annually?

- tariff_hh_number:

  How many households in the community paid a water fee the last time it
  was collected?

- distance_materials:

  How far away are/were the materials you use for hand pump borehole
  repairs? Options include '\>20km', '0-20km', 'within community', and
  'don't know'.

- service_provider:

  Is there a service provider or someone responsible for operating
  and/or maintaining this hand pump borehole or water system? Yes for 1
  and No for 0.

- preventive_maintenance:

  Do you conduct preventive maintenance? Yes for 1 and No for 0.

- functional_status:

  Functional status of the borehole. Functional for 1, Not functional
  for 0 (including 'Partially functional but in need of repair', 'Not
  functional')
