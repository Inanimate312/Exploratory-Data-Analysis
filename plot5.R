################################################################################
#
#     NEI EPA Air Pollution - 1999, 2002, 2005, 2008 Emissions 
#     from Motor Vehicle-Related Sources in Baltimore City, MD
#
#     This program downloads, cleans, and examines 
#     NEI EPA data and plots motor vehicle-related PM2.5 emissions 
#     by type for 1999, 2002, 2005, and 2008 in Baltimore City, MD
#     to identify trends in vehicle-related PM2.5 emissions in this city.
#
################################################################################
# 1. Initialize and download UCI data set
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
em99 <- annual_total(epa_data, "24510", 1999)
em02 <- annual_total(epa_data, "24510", 2002)
em05 <- annual_total(epa_data, "24510", 2005)
em08 <- annual_total(epa_data, "24510", 2008)

# Plot annual totals exported to PNG file
annual_emissions <- c(em99, em02, em05, em08)
years <- c(1999, 2002, 2005, 2008)

# We will set y axis limit to 10% above the highest annual emissions
ylimit <- max(annual_emissions) * 1.1

# Construct plot
png("plot5.png", width = 480, height = 480)

barplot(
  annual_emissions, 
  names.arg = years,
  ylim = c(0, ylimit),
  yaxt = "n",
  xlab = "Year",
  ylab = "Vehicle-Related PM25 Emissions (tons)",
  main = "Baltimore City, MD - Vehicle PM25 Emissions"
)

ticks <- axTicks(2)

axis(side = 2, at = ticks, labels = format(ticks, scientific = FALSE, big.mark = ","))

dev.off()