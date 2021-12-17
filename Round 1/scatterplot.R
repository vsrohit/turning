# Import data
summary_data <- read_csv("values.csv")
View(summary_data)

#Convert s,f,d into factors
summary_data <- summary_data %>%
  mutate(s = as.factor(s)) %>%
  mutate(f = as.factor(f)) %>%
  mutate(d = as.factor(d))

# Mean
#mean_plot1
mp1 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = f, y = Mean, shape = s, size = d))
save_plot("Mean vs feed.pdf",mp1,
          base_height = 4, base_width = 5.5)


#mean_plot2
mp2 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = s, y = Mean, shape = f, size = d))
save_plot("Mean vs Speed.pdf",mp2,
          base_height = 4, base_width = 5.5)


# Standard deviation
#sd_plot1
sd1<- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = f, y = Std.dev, shape = s, size = d))
save_plot("sd vs Feed.pdf",sd1,
          base_height = 4, base_width = 5.5)

#sd_plot2
sd2 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = s, y = Std.dev, shape = f, size = d))
save_plot("sd vs speed.pdf",sd2,
          base_height = 4, base_width = 5.5)


# Skewness
#skew_plot1
sk1 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = f, y = Skewness, shape = s, size = d))
save_plot("skewness vs feed.pdf",sk1,
          base_height = 4, base_width = 5.5)

#skew_plot2
sk2 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = s, y = Skewness, shape = f, size = d))
save_plot("skewness vs speed.pdf",sk2,
          base_height = 4, base_width = 5.5)

#Kurtosis
#ks_plot1
k1 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = f, y = Kurtosis, shape = s, size = d))
save_plot("Kurtosis vs feed.pdf",k1,
          base_height = 4, base_width = 5.5)
#ks_plot2
k2 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = s, y = Kurtosis, shape = f, size = d))
save_plot("Kurtosis vs speed.pdf",k2,
          base_height = 4, base_width = 5.5)
# Mean trend
#mean_trend1
mt1 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = f, y = Mean_trend, shape = s, size = d))
save_plot("Mean trend vs feed.pdf",mt1,
          base_height = 4, base_width = 5.5)

#mean_trend2
mt2 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = s, y = Mean_trend, shape = f, size = d))
save_plot("Mean trend vs speed.pdf",mt2,
          base_height = 4, base_width = 5.5)

# Standard deviation trend
#sd_trend1
sd1 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = f, y = Std.dev_trend, shape = s, size = d))
save_plot("sd trend vs feed.pdf",sd1,
          base_height = 4, base_width = 5.5)

#sd_trend2
sd2 <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = s, y = Std.dev_trend, shape = f, size = d))
save_plot("sd trend vs speed.pdf",sd2,
          base_height = 4, base_width = 5.5)

# Scatter plot Mean vs sd
mvs <- ggplot(data = summary_data) +
  geom_point(mapping = aes(x = Mean, y = Std.dev, shape = f, color = s, size = d))
save_plot("mean vs sd.pdf",mvs,
          base_height = 4, base_width = 5.5)

