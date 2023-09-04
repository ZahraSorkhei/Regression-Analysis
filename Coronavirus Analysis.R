##### ================== Initial Setup ==================
Sys.setlocale(locale = 'persian')
library(data.table)
library(ggplot2)
library(plyr)
##### ================== Reading Data ==================

d = fread('iranprovs_mortality_monthly.csv', encoding = 'UTF-8')
d$ym_num = d$y + d$m / 12 - 1/24
# Group the data by age group and calculate the total number of deaths
dy = aggregate(n ~ age_group, data = d, FUN = sum)

# Create a bar chart of the number of deaths by age group
ggplot(dy, aes(x = age_group, y = n)) +
  geom_bar(stat = "identity", fill = "#90EE90", colour = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


##### ================== Preprocessing ==================
val_repl <- c("0", "01-04", "05-09", "10-14")
d$age_group <- sapply(d$age_group, function(x) replace(x, x %in% val_repl, "0-14"))

val_repl <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
d$age_group <- sapply(d$age_group, function(x) replace(x, x %in% val_repl, "15-49"))

val_repl <- c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
d$age_group <- sapply(d$age_group, function(x) replace(x, x %in% val_repl, "50-79"))

val_repl <- c("80-84", "85-89", "90-94", "95+")
d$age_group <- sapply(d$age_group, function(x) replace(x, x %in% val_repl, "80+"))
# Define the age groups for each dataset
age_groups1 <- c("15-49")
age_groups2 <- c("50-79")
age_groups3 <- c("80+")
age_groups4 <- c("0-14")

# Subset the data based on the age groups
d1 <- d[age_group %in% age_groups1]
d2 <- d[age_group %in% age_groups2]
d3 <- d[age_group %in% age_groups3]
d4 <- d[age_group %in% age_groups4]

ds1 = d1[, .(n = sum(n)), .(y, m, ym_num, prov)]
ds2 = d2[, .(n = sum(n)), .(y, m, ym_num, prov)]
ds3 = d3[, .(n = sum(n)), .(y, m, ym_num, prov)]
ds4 = d4[, .(n = sum(n)), .(y, m, ym_num, prov)]


ym_num_covid = 1398 + 10/12 - 1/24
ym_num_start = ym_num_covid - 5
##### ================== Fit function ==================
# Define a function to perform the calculations for each province and month
calc_data <- function(province, month,ds) {
  temp_data = ds[prov == province & m == month]
  ym_num_start = ym_num_covid - 5
  ym_num_covid = 1398 + 10/12 - 1/24
  dsm = temp_data[ym_num > ym_num_start]
  dsm2fit = dsm[ym_num < ym_num_covid]
  fit = lm(n ~ ym_num, dsm2fit)
  # Check the p-value of the fit
  #print(summary(fit)$coefficients[2,4])
  if (summary(fit)$coefficients[2,4] > 0.1) {
    # Use the average of dsm2fit for prediction
    predicted = rep(mean(dsm2fit$n), times = nrow(temp_data))
    temp_data$n_predicted = predicted
    predicted_lower = predicted - sd(dsm2fit$n)
    predicted_upper = predicted + sd(dsm2fit$n)
  } else {
    # Use the fitted model for prediction
    predicted = predict(fit ,temp_data, interval = 'confidence')
    temp_data$n_predicted = predicted[,1]
    predicted_lower = predicted[,1]-sigma(fit)
    predicted_upper = predicted[,1]+sigma(fit)
  }
  
  temp_data$n_lower = predicted_lower
  temp_data$n_upper = predicted_upper
  temp_data$n_diff = temp_data$n - temp_data$n_upper
  temp_data$excess = rep(0, times=nrow(temp_data))
  temp_data$normalized_excess = rep(0, times=nrow(temp_data))
  temp_data[n_diff > 0]$excess = temp_data[n_diff > 0]$n - temp_data[n_diff > 0]$n_predicted
  return(temp_data)
}
##### ================== Age Group1 ==================
# Create a list of all combinations of province and month
prov_month_list1 <- expand.grid(province = unique(ds1$prov), month = seq(1:12))

# Use lapply to apply the calc_data function over the list
data_list1 <- lapply(seq_len(nrow(prov_month_list1)), function(i) {
  calc_data(prov_month_list1$province[i], prov_month_list1$month[i],ds1)
})

# Combine the list into a data.table
data1 <- rbindlist(data_list1)
data1 = data1[ym_num >= ym_num_covid]
# Normalize the excess by province
data1[, normalized_excess := excess / max(excess), by = prov]
##### ================== Age Group2 ==================

# Create a list of all combinations of province and month
prov_month_list2 <- expand.grid(province = unique(ds2$prov), month = seq(1:12))

# Use lapply to apply the calc_data function over the list
data_list2 <- lapply(seq_len(nrow(prov_month_list2)), function(i) {
  calc_data(prov_month_list2$province[i], prov_month_list2$month[i],ds2)
})

# Combine the list into a data.table
data2 <- rbindlist(data_list2)
data2 = data2[ym_num >= ym_num_covid]
# Normalize the excess by province
data2[, normalized_excess := excess / max(excess), by = prov]
##### ================== Age Group3 ==================

# Create a list of all combinations of province and month
prov_month_list3 <- expand.grid(province = unique(ds3$prov), month = seq(1:12))

# Use lapply to apply the calc_data function over the list
data_list3 <- lapply(seq_len(nrow(prov_month_list3)), function(i) {
  calc_data(prov_month_list3$province[i], prov_month_list3$month[i],ds3)
})

# Combine the list into a data.table
data3 <- rbindlist(data_list3)
data3 = data3[ym_num >= ym_num_covid]
# Normalize the excess by province
data3[, normalized_excess := excess / max(excess), by = prov]
##### ================== Age Group4 ==================

# Create a list of all combinations of province and month
prov_month_list4 <- expand.grid(province = unique(ds4$prov), month = seq(1:12))

# Use lapply to apply the calc_data function over the list
data_list4 <- lapply(seq_len(nrow(prov_month_list4)), function(i) {
  calc_data(prov_month_list4$province[i], prov_month_list4$month[i],ds4)
})

# Combine the list into a data.table
data4 <- rbindlist(data_list4)
data4 = data4[ym_num >= ym_num_covid]
# Normalize the excess by province
data4[, normalized_excess := excess / max(excess), by = prov]



##### ================== Plotting Heatmap ==================
# Define a custom color palette
data11 = subset(data1, select = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"))
data22 = subset(data2, select = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"))
data33 = subset(data3, select = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"))
data44 = subset(data3, select = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"))
data <- merge(data11, data22, by = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"), all = TRUE)
data <- merge(data, data33, by = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"), all = TRUE) 
data <- merge(data, data44, by = c("y", "m", "ym_num", "prov", "n", "excess","n_predicted"), all = TRUE)
data = data[ym_num >= ym_num_covid]
max_excess <- max(data $excess, na.rm = TRUE)
data[, normalized_excess := excess / max(excess), by = prov]
d <- aggregate(n_predicted ~ prov, data = data, FUN = sum)

setDT(data) #convert data to data.table
setDT(d) #convert d to data.table

#join data and d by prov and calculate percent_excess
data[d, on = "prov", percent_excess := excess / i.n_predicted]


my_palette <- colorRampPalette(c("blue", "green", "yellow", "red"))

# Create a plot with geom_raster
p <- ggplot(data, aes(x = ym_num, y = prov)) +
  geom_raster(aes(fill = percent_excess)) +
  xlab("time") +
  ylab("province") +
  scale_fill_gradientn(colours = my_palette(100))

# Print the plot
print(p)

# Define a custom color palette
my_palette <- c("white", "red")

# Plot the table as a heatmap with labels
ggplot(data, aes(x = prov, y = ym_num, fill = excess)) +
  geom_tile() +
  geom_text(aes(label = round(excess, 0)), size = 1) +
  scale_fill_gradientn(colors = my_palette) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Province", y = "Year-Month", fill = "Excess")
##### ================== Find Total Mortality ==================
# Define a function to calculate and plot the total excess by time
plot_excess_bytime <- function(data) {
  # Calculate the mean excess by year and month
  country_data <- data[, .(excess_bytime = excess), by = .(y, m)]
  
  # Plot the mean excess as a bar chart
  ggplot(country_data, aes(x = factor(y), y = excess_bytime, fill = factor(m))) +
    geom_col() +
    xlab(label = "time") +
    ylab(label = "total excess") +
    scale_fill_brewer(palette = "Set3")+
    ggtitle("The totla mortality is ",sum(data$excess))
  
}

# Apply the plot_excess_bytime function to the data
plot_excess_bytime(data)
##### ================== Find Total Mortality By Province ==================
# Define a function to calculate and plot the total excess by province
plot_excess_byprov <- function(data) {
  # Calculate the mean excess by province
  province_data <- data[, .(excess_byprov = excess), by = .(prov,y)]
  
  # Plot the mean excess as a bar chart
  ggplot(province_data, aes(x = reorder(prov, -excess_byprov), y = excess_byprov, fill = factor(y))) +
    geom_col() +
    xlab(label = "province") +
    ylab(label = "total excess") +
    scale_fill_manual(values = rainbow(15)) + # Use rainbow to generate 31 colors
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # Remove the legend
}

# Apply the plot_excess_byprov function to the data
plot_excess_byprov(data)
##### ================== Find Best Province Base on Slope ==================
# Calculate the sum of excess by province for data
data1_sum <- aggregate(excess ~ prov, data1, sum)
data2_sum <- aggregate(excess ~ prov, data2, sum)
data3_sum <- aggregate(excess ~ prov, data3, sum)
data4_sum <- aggregate(excess ~ prov, data3, sum)
# Merge the three data frames by province
data_sum <- merge(data1_sum, data2_sum, by = "prov")
data_sum <- merge(data_sum, data3_sum, by = "prov")
data_sum <- merge(data_sum, data4_sum, by = "prov")

# Rename the columns to indicate the source of excess
names(data_sum) <- c("prov", "excess1", "excess2", "excess3","excess4")
# Create a new column with the rate
data_rate1 <- mean(mutate(data_sum, rate = excess1 / (excess1+excess2 + excess3+excess4))$rate)
data_rate2 <- mean(mutate(data_sum, rate = excess2 / (excess1+excess2 + excess3+excess4))$rate)
data_rate3 <- mean(mutate(data_sum, rate = excess3 / (excess1+excess2 + excess3+excess4))$rate)
data_rate4 <- mean(mutate(data_sum, rate = excess4 / (excess1+excess2 + excess3+excess4))$rate)

# Join the two data frames by province
#data_join <- merge(data2_sum, data3_sum, by = "prov")

data11 = data1[ym_num >= ym_num_covid]
data22 = data2[ym_num >= ym_num_covid]
data33 = data3[ym_num >= ym_num_covid]
data44 = data3[ym_num >= ym_num_covid]

# Split the data by province
data_split1 <- split(data11 , data11$prov)
data_split2 <- split(data22 , data22$prov)
data_split3 <- split(data33 , data33$prov)
data_split4 <- split(data44 , data44$prov)

# Fit a linear model to each subset and get the slope
slopes1 <- sapply(data_split1, function(x) lm(normalized_excess ~ ym_num, x)$coeff[2])
slopes2 <- sapply(data_split2, function(x) lm(normalized_excess ~ ym_num, x)$coeff[2])
slopes3 <- sapply(data_split3, function(x) lm(normalized_excess ~ ym_num, x)$coeff[2])
slopes4 <- sapply(data_split4, function(x) lm(normalized_excess ~ ym_num, x)$coeff[2])

# Convert the slopes vector to a data frame
slopes_df1 <- data.frame(province = sub(".ym_num", "", names(slopes1)),slope = slopes1)
slopes_df2 <- data.frame(province = sub(".ym_num", "", names(slopes2)),slope = slopes2)
slopes_df3 <- data.frame(province = sub(".ym_num", "", names(slopes3)),slope = slopes3)
slopes_df4 <- data.frame(province = sub(".ym_num", "", names(slopes4)),slope = slopes4)
# Put all data frames into a list
df_list <- list(slopes_df1, slopes_df2, slopes_df3, slopes_df4)

# Merge the data frames by province
merged_df <- full_join(slopes_df1, slopes_df2, by = "province") %>%
  full_join(slopes_df3, by = "province") %>%
  full_join(slopes_df4, by = "province")

# Rename the columns
colnames(merged_df) <- c("province", "slope1", "slope2", "slope3", "slope4")
merged_df <- mutate(merged_df, slope_total = slope1*data_rate1 + slope2*data_rate2 + slope3*data_rate3 +slope4*data_rate4)


# Create a barplot with ggplot2
ggplot(merged_df, aes(x = province, y = slope_total)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Province") +
  ylab("Slope of excess")


##### ================== Find Best Province Base on N ==================


my_palette <- colorRampPalette(c("blue", "green", "yellow", "red"))

# Create a plot with geom_raster
p <- ggplot(data4, aes(x = ym_num, y = prov)) +
  geom_raster(aes(fill = normalized_excess)) +
  xlab("time") +
  ylab("province") +
  scale_fill_gradientn(colours = my_palette(100))

# Print the plot
print(p)




data_sum <- merge(data1_sum, data2_sum, by = "prov")
data_sum <- merge(data_sum, data3_sum, by = "prov")



# Create a new column with the rate
data_rate1 <- mean(mutate(data_sum, rate = excess.x / (excess.x+excess.y + excess))$rate)
data_rate2 <- mean(mutate(data_sum, rate = excess.y / (excess.x+excess.y + excess))$rate)
data_rate3 <- mean(mutate(data_sum, rate = excess / (excess.x+excess.y + excess))$rate)


# Calculate the sum of n by province for ds
ds1_sum <- aggregate(n ~ prov, ds1, sum)
ds2_sum <- aggregate(n ~ prov, ds2, sum)
ds3_sum <- aggregate(n ~ prov, ds3, sum)


# Join the two data frames by province
data_join1 <- merge(data1_sum, ds1_sum, by = "prov")
data_join2 <- merge(data2_sum, ds2_sum, by = "prov")
data_join3 <- merge(data3_sum, ds3_sum, by = "prov")


# Create a new column with the ratio
data_ratio1 <- mutate(data_join1, ratio = 1-excess / n)
data_ratio2 <- mutate(data_join2, ratio = 1-excess / n)
data_ratio3 <- mutate(data_join3, ratio = 1-excess / n)


# Put all data frames into a list
data_ratio <- list(data_ratio1, data_ratio2, data_ratio3,data_ratio4)

data_join <- merge(data_ratio1,data_ratio2, by = "prov")
data_join <- merge(data_join,data_ratio3, by = "prov")


data_join <- mutate(data_join, rate = ratio.x*data_rate1+ratio.y*data_rate2+ratio*data_rate3)
# Plot the ratio as a boxplot

# Plot the ratio as a boxplot
ggplot(data_join  , aes(x = prov, y = rate)) +
  geom_boxplot(color = "blue") +
  xlab("Province") +
  ylab("Ratio of excess to n") +
  theme(axis.text.x = element_text(size = 10, angle = 90)) # make the x-axis text smaller and rotate it by 90 degrees