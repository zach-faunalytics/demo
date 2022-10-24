## ---- Set Up ---- ##

library(tidyverse) # Load tidyverse package
setwd('/Users/Faunalytics/Code/') # Set working directory (aka folder you're working out of)


## ---- Getting Data ---- ##

# Source: https://www.fao.org/faostat/en/#data/QCL
data <- read.csv("https://github.com/zach-faunalytics/demo/raw/main/fao-data.csv") # Load data
head(data) # Look at first 6 rows of data
View(data) # Look at all of the data


## ---- Data Wrangling ---- ##

# Select the columns of interest
data <- data |>
  select(Country, Item, Unit,
         Y2010, Y2011, Y2012, Y2013, Y2014, Y2015, Y2016, Y2017, Y2018, Y2019, Y2020)
head(data) # Look at first 6 rows of data

# Select the rows of interest
data <- data |>
  filter(Item == "Chickens") |>  # Only keep rows where Item is "Chickens"
  # Only keep rows where the country is in the given options
  filter(Country %in% c("Canada", "United States of America", "Mexico", "China"))
head(data) # Look at first 6 rows of data

# Make data long instead of wide
data <- data |>
  # Make the columns from Y2010 to Y2020 into two columns:
  # Where the names of the columns (e.g. Y2010) go in a new column called Year
  # and the values of those columns go in a new column called Chickens
  pivot_longer(cols = Y2010:Y2020, names_to = "Year", values_to = "Chickens")
head(data) # Look at first 6 rows of data

# Remove Y from Year value and change Year from text to number
data <- data |>
  mutate(Year = str_remove(Year, "Y"), # Remove the character "Y" from values in the Year column
         Year = as.numeric(Year)) # Tell R to treat the Year column as a number instead of as text


## ---- Graph Data ---- ##

ggplot(data) +  # Create a plot
  geom_line( # Make it a line graph
    # With the year on the x-axis, number of chickens on
    # the y-axis, and color the lines based on the country
    aes(x = Year, y = Chickens, color = Country)
    ) +
  scale_x_continuous(breaks = 2010:2020) + # Label each year on x-axis
  # Change the default labels
  labs(title = "Number of Chickens Slaughtered, 2010-2020", # Set title
       caption = "Source: Food and Agriculture Organization of the United Nations (FAO)", # Set caption
       x = "Year", # Set x-axis label
       y = "Number of Chickens (thousands)") + # Set y-axis label
  theme_classic() # Apply ggplot's classic theme


## ---- Data Summarizing ---- ##

# Make new object with total amount of chickens by country
data_summary <- data |>
  group_by(Country) |> # Group data by country...
  summarize(Chickens = sum(Chickens) * 1000) |> # Total the number of chickens
  # and multiply by 1000 to give the actual number (by default, in 1000s of chickens)
  ungroup() # No longer group data by country
data_summary # Show summarized data


## ---- Graph Summarized Data ---- ##

ggplot(data_summary) + # Create a plot
  geom_col(   # Make it a column plot (aka a vertical bar plot)
    # With the country on the x-axis, number of chickens on
    # the y-axis, and fill the bars in based on the country
    aes(x = Country, y = Chickens, fill = Country)
    ) +
  # Change the default labels
  labs(title = "Number of Chickens Slaughtered, 2010-2020", # Set title
       caption = "Source: Food and Agriculture Organization of the United Nations (FAO)", # Set caption
       x = "Country", # Set x-axis label
       y = "Number of Chickens") + # Set y-axis label
  theme_classic() # Apply ggplot's classic theme
