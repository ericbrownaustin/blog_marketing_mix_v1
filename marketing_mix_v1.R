library(RGA)
library(ChannelAttribution)
library(data.table)
library(ggplot2)
library(scales)

profile <- {{replace this with your google analytics view id}}

start.date <- {{replace with your start date in YYYY-MM-DD}}
end.date <- {{replace with your end date in YYYY-MM-DD}}

#pull path data from Google Analytics
mcf_data <- get_mcf(profile, start.date, end.date,
                     metrics = "mcf:totalConversions,mcf:totalConversionValue",
                     dimensions = "mcf:basicChannelGroupingPath",
                     fetch.by = "day")

#limit to ecommerce transactions only
setDT(mcf_data)
mcf_data <- mcf_data[conversionType=="Transaction"]

#create path length metric
mcf_data$pathlength <- stringr::str_count(mcf_data$basicChannelGroupingPath, ">") + 1

#remove multi word channel names (problematic for ChannelAttribution script)
mcf_data$basicChannelGroupingPath <- gsub("(?<=\\w)\\s(?=\\w|\\$)", "", mcf_data$basicChannelGroupingPath, perl = TRUE)

#create heuristic multi-channel models
H <- heuristic_models(mcf_data, 'basicChannelGroupingPath', 'totalConversions', var_value = 'totalConversionValue')

#round to convert from scientific notation (if needed)
H$linear_touch_conversions <- round(H$linear_touch_conversions,0)
H$linear_touch_value <- round(H$linear_touch_value,0)

#create markov model
M <- markov_model(mcf_data, 'basicChannelGroupingPath', 'totalConversions', var_value = 'totalConversionValue', order = 1) 
M$total_conversion <- round(M$total_conversion,0)
M$total_conversion_value <- round(M$total_conversion_value,0)

#merge data frames and order by total conversion value
R <- merge(H, M, by = 'channel_name') 
R <- R[order(-R$total_conversion_value),]

#select and rename columns
R1 <- R[, (colnames(R) %in% c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'total_conversion'))]
colnames(R1) <- c('channel_name', 'first_touch', 'last_touch', 'linear_touch', 'markov_model') 

#transform data from wide to long version
R1 <- melt(R1, id = 'channel_name')

# Plot the total conversions
ggplot(R1, aes(channel_name, value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('TOTAL CONVERSIONS') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title = element_text(size = 20)) +
  ylab("") + 
  scale_y_continuous(labels = comma)
