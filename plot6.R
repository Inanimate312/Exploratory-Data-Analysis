################################################################################
#
#       NEI EPA Air Pollution - 1999, 2002, 2005, 2008 Emissions 
#  from Motor Vehicle-Related Sources in Baltimore City and Los Angeles
#
#     This program downloads, cleans, and examines 
#     NEI EPA data and plots motor vehicle-related PM2.5 emissions 
#     by type for 1999, 2002, 2005, and 2008 in Baltimore City, MD 
#     and Los Angeles County, CA to compare trends in vehicle-related
#     PM2.5 emissions between these locations.
#
################################################################################
# 1. Initialize and download EPA data set
################################################################################
# Set URL and filepath
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipfile <- file.path(getwd(), "epa-data.zip")

# Download and unzip file if not already downloaded
if(!file.exists(zipfile)) {
  download.file(url,
                destfile = zipfile)
  unzip(zipfile)
}

################################################################################
# 2. Read in data 
################################################################################
library(dplyr)
library(ggplot2)

# Set file path
epa_file <- file.path(getwd(), "summarySCC_PM25.rds")
classification_file <- file.path(getwd(), "Source_Classification_Code.rds")

epa_data <- readRDS(epa_file)
classification_data <- readRDS(classification_file)

# Join emissions and classifications data
epa_data <- inner_join(epa_data, classification_data, by = "SCC")

# Filter to motor vehicle-related sources
epa_data <- epa_data %>%
  filter(grepl("veh", Short.Name, ignore.case = TRUE) |
           grepl("veh", EI.Sector, ignore.case = TRUE) | 
           grepl("veh", SCC.Level.One, ignore.case = TRUE) |
           grepl("veh", SCC.Level.Two, ignore.case = TRUE) | 
           grepl("veh", SCC.Level.Three, ignore.case = TRUE) | 
           grepl("veh", SCC.Level.Four, ignore.case = TRUE))

################################################################################
# 3. Plot total emissions for 1999, 2002, 2005, and 2008
################################################################################

# Define function to calculate total emissions in the given FIPS code and year
annual_total <- function(data, fips, year) {
  sum(data$Emissions[data$fips == fips & data$year == year], na.rm = TRUE)
}

# Calculate annual totals
# Baltimore City
em99_bal <- annual_total(epa_data, "24510", 1999)
em02_bal <- annual_total(epa_data, "24510", 2002)
em05_bal <- annual_total(epa_data, "24510", 2005)
em08_bal <- annual_total(epa_data, "24510", 2008)

# Los Angeles County
em99_la <- annual_total(epa_data, "06037", 1999)
em02_la <- annual_total(epa_data, "06037", 2002)
em05_la <- annual_total(epa_data, "06037", 2005)
em08_la <- annual_total(epa_data, "06037", 2008)

# Plot annual totals exported to PNG file
annual_emissions_bal <- c(em99_bal, em02_bal, em05_bal, em08_bal)
annual_emissions_la <- c(em99_la, em02_la, em05_la, em08_la)
years <- c(1999, 2002, 2005, 2008)

# We will set y axis limit to 10% above the highest annual emissions
ylimit_bal <- max(annual_emissions_bal) * 1.1
ylimit_la <- max(annual_emissions_la) * 1.1

# Construct plot
png("plot6.png", width = 480, height = 480)

par(mfrow = c(1,2))

barplot(
  annual_emissions_bal, 
  names.arg = years,
  ylim = c(0, ylimit_bal),
  yaxt = "n",
  xlab = "Year",
  ylab = "Vehicle-Related PM25 Emissions (tons)",
  main = "Baltimore City, MD"
)

ticks <- axTicks(2)

axis(side = 2, at = ticks, labels = format(ticks, scientific = FALSE, big.mark = ","))

barplot(
  annual_emissions_la, 
  names.arg = years,
  ylim = c(0, ylimit_la),
  yaxt = "n",
  xlab = "Year",
  ylab = "Vehicle-Related PM25 Emissions (tons)",
  main = "Los Angeles, CA"
)

ticks <- axTicks(2)

axis(side = 2, at = ticks, labels = format(ticks, scientific = FALSE, big.mark = ","))

dev.off()