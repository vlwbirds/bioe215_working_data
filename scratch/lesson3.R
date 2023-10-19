library("tidyverse")

# read and inspect data
surveys <- read_csv("data/portal_data_joined.csv")
head(surveys)
surveys
summary(surveys)


# Q1 What's the type of column species_id? Of hindfoot_length?
  #column species_id is character
  # hidfoot_length is integer

# Q2 Hoe many rows and columns are in surveys?
  # I looked at the Environment tab and could read 34786 obs. of 13 variables

#columns use select(), rows use filter()

#no quotes needed in the tidyverse, first object must be the DF assignment I want
select(surveys, plot_id, species_id, weight_g = weight)

#remove columns by adding - before the column name
select(surveys, -record_id, -species_id)

# filters work on conditions
# must use == to determine filter
# | symbol is the OR condition
filter(surveys, year == 1995)
filter(surveys, year == 1995, plot_id == 7)
filter(surveys, month == 2 | day == 20)


# Q3 filter() surveys to records collected in November where hindfoot_length is greater than 36.0
filter(surveys, month == 11, hindfoot_length > 36)

#Q4 Fix these errors
#missing second =
filter(surveys, year = 1995)
#column field misspelled
filter(surveys, polt_id == 2)


# pipes
select(filter(surveys, year == 1995), plot_id, weight)

surveys_psw <- surveys %>% 
  filter(year == 1995) %>% 
  select(plot_id, weight)
surveys_psw

# Q5 Use pipes to subset surveys to animals collected before 1995 retaining just the columns year, sex, and weight
surveys_ysw <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)
surveys_ysw


# add columns is mutate()
surveys %>% 
  mutate(wieght_kg = weight / 1000) %>% 
  view()

surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) %>% 
  view()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) %>% 
  view()

# Q6 Create a new dataframe from the surveys data that meets the following criteria: contains onle the species_id column and a new column called hindfoot_cm containing the hindfoot_length values (currently in mm) converted ti centimeters. In this hindfoot_cm comlumn there are no NAs and all values are less than 3
surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  select(species_id, hindfoot_cm) %>% 
  filter(hindfoot_cm < 3)
  view()

  
# Split, apply, combine
  
surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))
# results in warning about grouping and produces NaN in output

surveys %>% 
  drop_na(weight) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop") %>% 
  arrange(desc(min_weight))


#DROP YOUR GROUPS

# Q7: How many animals were caught in each plot_type surveyed?
surveys %>% 
  group_by(plot_type) %>%
  summarize(n(),
            .groups = "drop")
            
?n  
    

# Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). Also add the number of observations (hint: see ?n).
surveys %>% 
  filter(!is.na(hindfoot_length))
  group_by(species_id) %>% 
  summarize(mean_hind = mean(hindfoot_length),
            min_hind = min(hindfoot_length),
            max_hind = max(hindfoot_length),
            n(),
            .groups = "drop")
# Q9: What was the heaviest animal measured in each year? Return the columns year, genus, species_id, and weight.