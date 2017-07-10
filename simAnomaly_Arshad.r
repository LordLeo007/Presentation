
# Anomaly detection

# install.packages("pacman")

# Install packages for Anomaly detection
pacman::p_load(ggplot2, grid, gridExtra, robustbase)

# Load Data ############################
# Read data wine
data = read.csv("geoMap.csv")   # Load data

# Check the stracture
str(data)

# Transform variables to factors
data$Region = as.factor(data$Region)

# Univariate outliers

# Boxplot for each variable separately
# Lion
u1 <- qplot(data = data, y = lion, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "lion") +
geom_text(aes(label = ifelse(lion %in%
              boxplot.stats(lion)$out, 
              as.character(state.name), "")), hjust = 1.5)
u1

# Tiger
u2 <- qplot(data = data, y = Tiger, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Tiger") +
  geom_text(aes(label = ifelse(Tiger %in%
                                 boxplot.stats(Tiger)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u2

# Gray.wolf
u3 <- qplot(data = data, y = Gray.wolf, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Gray.wolf") +
  geom_text(aes(label = ifelse(Gray.wolf %in%
                                 boxplot.stats(Gray.wolf)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u3

# Hyena
u4 <- qplot(data = data, y = Hyena, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "bretix") +
  geom_text(aes(label = ifelse(Hyena %in%
                                 boxplot.stats(Hyena)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u4

# Bear
u5 <- qplot(data = data, y = Bear, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Bear") +
  geom_text(aes(label = ifelse(Bear %in%
                                 boxplot.stats(Bear)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u5


# Plot boxplots together
grid.arrange(u1, u2, u3, u4, u5, nrow = 2,
             top = "Boxplots: Univariate outliers")


# Bivariate outliers
b1 <- qplot(data = data, x = lion, y = Tiger,
            main = "lion vs. Tiger") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((lion < 2.5 | lion > 7.5 | Tiger > 3.0),
                as.character(state.name), "")), hjust = 1.5)
b1

b2 <- qplot(data = data, x = Hyena, y = Bear,
            main = "Hyena vs. Bear") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Hyena > 8.9 | Bear > 40),
                               as.character(state.name), "")), hjust = 1.5)
b2

b3 <- qplot(data = data, x = Gray.wolf, y = Bear,
            main = "Gray.wolf vs. Bear") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Gray.wolf > 7.5 | Gray.wolf < 1 | Bear > 40),
                               as.character(state.name), "")), hjust = 1.5)
b3

# Plot together
grid.arrange(b1, b2, b3, nrow = 1, top = "Bivariate outliers")



# Clean up #####################
rm(list = ls())


