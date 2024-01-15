#Load Library:
library (lattice)
library (psych)
library (gmodels)
library (ggplot2)
library (dplyr)

#View Dataset
carsDB

#wt vs mpg
ggplot(data=carsDB, aes (x=wt, y=mpg)) +geom_point ()+ stat_smooth(method = 'lm') + 
  labs(title = "Scatter Plot of WT vs MPG with Smoothing Line") + 
  theme(plot.title = element_text(hjust =0.5)) + xlab('Weight(1,000)') + ylab('Fuel Effeciency (MPG)')

# Create the side-by-side boxplot
my_colors = c("red", "blue", "green")
ggplot(data=carsDB, aes(x = factor(cyl), y = disp, fill = factor(cyl))) +
  geom_boxplot() +
  labs(x = "Number of Cylinders (cyl)", y = "Displacement (disp)") +
  ggtitle("Side-by-Side Boxplot for Displacement vs. Number of Cylinders") +
scale_fill_manual(values = my_colors) +  # Apply custom colors
theme(plot.title = element_text(hjust =0.5))

#Calculate the mean and median for engine displacement (disp) by number of cylinders (syl)
mean_disp <- aggregate (carsDB$disp, by=list(carsDB$cyl), FUN=mean)
median_disp <- aggregate(carsDB$disp, by=list (carsDB$cyl), FUN=median)
# Rename the columns for clarity
colnames(mean_disp) <- c("Cylinders", "Mean Displacement")
colnames(median_disp) <- c("Cylinders", "Median Displacemeat")
# View the calculated means and medians
print (mean_disp)
print (median_disp)
