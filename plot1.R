################################################################################
#
#               NEI EPA Air Pollution - 1999, 2002, 2005, 2008
#
#       This program downloads, cleans, and examines 
#       NEI EPA data and plots total PM2.5 emissions for 1999, 
#       2002, 2005, and 2008 to identify whether PM2.5 emissions
#       have decreased from 1999 to 2008.
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

# Define function to calculate total emissions in the given year
annual_total <- function(data, year) {
  sum(data$Emissions[data$year == year], na.rm = TRUE)
}

# Calculate annual totals
em99 <- annual_total(epa_data, 1999)
em02 <- annual_total(epa_data, 2002)
em05 <- annual_total(epa_data, 2005)
em08 <- annual_total(epa_data, 2008)

# Plot annual totals exported to PNG file
annual_emissions <- c(em99, em02, em05, em08)
years <- c(1999, 2002, 2005, 2008)

# We will set y axis limit to 10% above the highest annual emissions
ylimit <- max(annual_emissions) * 1.1

# Construct plot
png("plot1.png", width = 480, height = 480)

barplot(
      annual_emissions, 
      names.arg = years,
      ylim = c(0, ylimit),
      yaxt = "n",
      xlab = "Year",
      ylab = "Total PM25 Emissions (tons)",
      main = "Annual PM25 Emissions"
)

ticks <- axTicks(2)

axis(side = 2, at = ticks, labels = format(ticks, scientific = FALSE, big.mark = ","))

dev.off()