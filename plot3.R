################################################################################
#
#     NEI EPA Air Pollution - 1999, 2002, 2005, 2008 in Baltimore City, MD
#                               By Type of Emission
#
#       This program downloads, cleans, and examines 
#       NEI EPA data and plots PM2.5 emissions by type in Baltimore City, MD
#       for 1999, 2002, 2005, and 2008 to identify which if any type of PM2.5 
#       emissions have decreased from 1999 to 2008 in this city.
#
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

# Set file path
epa_file <- file.path(getwd(), "summarySCC_PM25.rds")
classification_file <- file.path(getwd(), "Source_Classification_Code.rds")

epa_data <- readRDS(epa_file)
classification_data <- readRDS(classification_file)

################################################################################
# 3. Plot total emissions for 1999, 2002, 2005, and 2008
################################################################################
library(ggplot2)

# Define function to calculate total emissions in the given 
# FIPS code, emissions type, and year
annual_total <- function(data, fips, type, year) {
  sum(data$Emissions[data$fips == fips & 
                     data$type == type & 
                     data$year == year], na.rm = TRUE)
}

# Calculate annual totals by emissions type
# Point
em99point <- annual_total(epa_data, "24510", "POINT", 1999)
em02point <- annual_total(epa_data, "24510", "POINT", 2002)
em05point <- annual_total(epa_data, "24510", "POINT", 2005)
em08point <- annual_total(epa_data, "24510", "POINT", 2008)

# Non-Point
em99nonpoint <- annual_total(epa_data, "24510", "NONPOINT", 1999)
em02nonpoint <- annual_total(epa_data, "24510", "NONPOINT", 2002)
em05nonpoint <- annual_total(epa_data, "24510", "NONPOINT", 2005)
em08nonpoint <- annual_total(epa_data, "24510", "NONPOINT", 2008)

# On-Road
em99onroad <- annual_total(epa_data, "24510", "ON-ROAD", 1999)
em02onroad <- annual_total(epa_data, "24510", "ON-ROAD", 2002)
em05onroad <- annual_total(epa_data, "24510", "ON-ROAD", 2005)
em08onroad <- annual_total(epa_data, "24510", "ON-ROAD", 2008)

# Non-Road
em99nonroad <- annual_total(epa_data, "24510", "NON-ROAD", 1999)
em02nonroad <- annual_total(epa_data, "24510", "NON-ROAD", 2002)
em05nonroad <- annual_total(epa_data, "24510", "NON-ROAD", 2005)
em08nonroad <- annual_total(epa_data, "24510", "NON-ROAD", 2008)

# Plot annual totals exported to PNG file
types <- c("point", "nonpoint", "onroad", "nonroad")

emissions_by_type <- data.frame (
  year = rep(c(1999, 2002, 2005, 2008), each = 4),
  type = rep(types, times = 4),
  emissions = c(
    em99point, em99nonpoint, em99onroad, em99nonroad,
    em02point, em02nonpoint, em02onroad, em02nonroad,
    em05point, em05nonpoint, em05onroad, em05nonroad,
    em08point, em08nonpoint, em08onroad, em08nonroad
  )
)

# We will set y axis limit to 10% above the highest annual emissions
ylimit <- ceiling(max(emissions_by_type$emissions) * 1.1)
emissions_by_type$year <- factor(emissions_by_type$year)


# Construct plot
png("plot3.png", width = 480, height = 480)

ggplot(emissions_by_type, aes(x = year, y = emissions, fill = type)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(emissions, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  )
  labs(
    x = "Year",
    y = "Total PM25 Emissions (tons)",
    fill = "Type"
  )

dev.off()