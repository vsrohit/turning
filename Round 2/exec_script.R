# Global details for the analysis ----------------------------------------------
# !!!!Execute these before you execute the functions
nExp <- 10    # Number of experimens
sf   <- 10000 # Sampling Frequency
f    <- 5000  # Frame size

# Get the data and select the required component -------------------------------
#Read the raw data from csv
exp_data <- get_exp_data()

#Select only Fz data (column number 4)
Fz_data <- get_component(exp_data, 4) 

# Start and end points of continuous cutting zone

start_c <- c(40000, 120000, 40000, 40000, 100000, 40000, 60000, 25000, 40000, 50000)
end_c   <- c(150000, 170000, 145000, 110000, 130000, 110000, 150000, 130000, 150000, 150000)


# Step 2 - Get continuous data with added frame number--------------------------
cont_Fz <-  Fz_data %>%
  get_continuous_data() %>%
  adjust_time() %>%
  add_frame_numbers()


# Step 3 - Violin plot ---------------------------------------------------------
violin_data <- make_frames_as_factors(cont_Fz)
pvio <- plot_violin(violin_data)
save_plot("Violinplot.pdf",pvio,
          base_height = 11.69, base_width = 8.27)

# Step 4 - Caclulate stats and plot them-------
#Calculate mean and plot data
Fz_mean_1 <- frame_stat(cont_Fz, mean)
p_mean <- stat_plot(Fz_mean_1, "Frame number", "Mean")
save_plot("Continuous mean plot.pdf",p_mean,
          base_height = 11.69, base_width = 8.27)

#Calculate sd and plot data
Fz_sd_1 <- frame_stat(cont_Fz, sd)
p_sd <- stat_plot(Fz_sd_1, "Frame number", "Standard deviation")
save_plot("Continuous sd plot.pdf",p_sd,
          base_height = 11.69, base_width = 8.27)

#Calculate skewness and plot data
Fz_skew_1 <- frame_stat(cont_Fz, skewness)
p_skew <- stat_plot(Fz_skew_1, "Frame number", "Skewness")
save_plot("Continuous skew plot.pdf",p_skew,
          base_height = 11.69, base_width = 8.27)

#Calculate Kurtosis and plot data
Fz_kurt_1 <- frame_stat(cont_Fz, kurtosis)
p_kurt <- stat_plot(Fz_kurt_1, "Frame number", "Kurtosis")
save_plot("Continuous kurt plot.pdf",p_kurt,
          base_height = 11.69, base_width = 8.27)

#Calculate single stats for each experiment and store them in final_data
full_mean <- full(cont_Fz, mean)
full_sd <- full(cont_Fz, sd)
full_skew <- full(cont_Fz, skewness)
full_kurt <- full(cont_Fz, kurtosis)

final_data <- data.frame("Mean" = full_mean,
                         "Std.dev" = full_sd,
                         "Skewness" = full_skew,
                         "Kurtosis" = full_kurt)

write.csv(final_data, "final_data.csv")

#Calculate trends and store them in a data frame called final_data_trend
mean_trend <- trend(Fz_mean_1)
sd_trend <- trend(Fz_sd_1)
skew_trend <- trend(Fz_skew_1)
kurt_trend <- trend(Fz_kurt_1)
final_data_trend <- data.frame("Mean_trend" = mean_trend,
                               "Std.dev_trend" = sd_trend,
                               "Skewness_trend" = skew_trend,
                               "Kurtosis_trend" = kurt_trend)

write.csv(final_data_trend, "final_data_trend.csv")

