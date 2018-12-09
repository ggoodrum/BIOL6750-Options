# Author: Greg Goodrum
# Course: BIOL 6750 - Programming for Biologists
# Section: Session 13 - A Brief Tour of Plotting in R
# ------------------------------------------------------------

# 1.  Replicate 'Homework 1' in Advanced Base R using ggplot2.
#     Build slowly one piece at a time.

library(ggplot2)
library(gridExtra)

# Organize data inside a data frame.
a <- rnorm(100)
b <- rnorm(100)
b.cut <- cut(b, breaks=5)t
bin <-
c <- rnorm(100)
response <- a * b + c + rnorm(100)
data <- data.frame(a=a, b=b, c=c, response=response, b.cut=b.cut)

# Create better color ramp
library(RColorBrewer)
better.cols <- colorRampPalette(brewer.pal(9, name='RdBu'))(1000)
cut.resp <- as.numeric(cut(response, 1000))

# Create scaled version of c variable so it ranges from 0 to 2.
scale.c <- c + abs(min(data$c))
scale.c <- (scale.c/(max(scale.c))) * 2

# Create plots, consider doing one with ggplot
# Difference in scale is the result of differences of scale between base R and ggplot
# I used a range of 0-4 instead of 0-2 so that the difference of scale would be more apparent
plt1 <- ggplot(data=data, aes(x=b,y=a)) + geom_point(aes(colour=response, size=scale.c)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), plot.title = element_text(face = 'bold', hjust = 0.5), axis.text.y= element_text(angle=90, face='bold')) +
  scale_size(range=c(0,4), guide = FALSE) +
  scale_colour_gradient2(low='red', high='blue', mid='grey40', guide = FALSE) +
  xlab('b') + ylab('a') + ggtitle("OK colors")

plt2 <- ggplot(data=data, aes(x=b,y=a)) + geom_point(aes(colour=response, size=scale.c)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), plot.title = element_text(face = 'bold', hjust = 0.5), axis.text.y = element_text(angle=90, face='bold')) +
  scale_colour_gradientn(colours = better.cols, guide=FALSE) +
  scale_size(range=c(0,4), guide = FALSE) +
  xlab('b') + ylab('a') + ggtitle("Better colors")

# Use grid.arrange() to orient and display plots next to one another
grid.arrange(plt1, plt2, nrow=1)

# --------------------------------------------------------
# 2. (a) Add a legend where indicated in "Advanced Base R"
#        Make sure to do in Base R.
# First, simulate the data
a <- rnorm(100)
b <- rnorm(100)
c <- rnorm(100)
response <- a * b + c + rnorm(100)
# Make a color palette to display our response variable
ok.cols <- colorRampPalette(c("red","grey40","blue"))(1000)
library(RColorBrewer)
better.cols <- colorRampPalette(brewer.pal(9,name="RdBu"))(1000)
# "Cut" our response variable into 1000 sections (to match the colors)
#   - we're coercing the data from a factor to make the subsetting
#   - easier - check what would happen otherwise if you like
cut.resp <- as.numeric(cut(response, 1000))
# Rescale our 'c' variable so it ranges sensibly enough to plot
#   - as a size dimension (from 0 to 2)
scale.c <- c + abs(min(c))
scale.c <- (scale.c / (max(scale.c))) * 2

# Make two plotting frames, with a 2:1 ratio of their heights
# Route to PDF because RStudio has problems with display window
pdf("Option13-1.pdf")
# Make two plotting frames, with a 2:1 ratio of their heights
layout(matrix(1:3, nrow=3))
plot(a ~ b, col=better.cols[cut.resp], cex=scale.c)
legend("bottomright", legend=c, col = better.cols[cut.resp], cex = scale.c)
# Make a blank plot at the bottom
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", axes=FALSE)
title((main='Legend'))
# Add a gradient scale bar
library(plotrix)
gradient.rect(0, 0, 1, 1, col=better.cols, border=NA)
# Give it some labels
text(seq(0,1,length.out=5), y=.5,
     labels=round(seq(min(response),max(response),length.out=5),2)
)
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="")
#... adding a legend with different sizes is an exercise at the end
legend("bottom", legend=c(min(c), median(c), max(c)), pch=1, pt.cex = c(min(scale.c), median(scale.c), max(scale.c)), title = "C Value")
#legend(x=.5, y=.5, legend=c, col = better.cols[cut.resp], cex = scale.c)
dev.off()


#pdf("demo.pdf")
#plot(1:10)
#dev.off()

# 2. (b) Make that legend appear in a separate plotting frame
#        in the bottom row of your plot.  You should have the
#        main plot as the entire top row, and the bottom row
#        should contain two separate plotting frames.

# Make two plotting frames, with a 2:1 ratio of their heights
pdf('Option13-2.pdf')
layout(matrix(c(1,2,1,3), nrow=2), heights=c(2,1))
# FILL THE FIRST PLOTTING WINDOW
plot(a ~ b, col=better.cols[cut.resp], cex=scale.c)
# Make a blank plot at the bottom, this re-routes where the activity is happening
# CHANGE TO THE NEXT PLOT
# Axes allows you to remove the guides specified in the xlim and ylim fields
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", axes = FALSE)
title(main = 'Response Value')
# Add a gradient scale bar
library(plotrix)
gradient.rect(0, 0, 1, 1, col=better.cols, border=NA)
# Give it some labels
text(seq(0,1,length.out=5), y=.5,
     labels=round(seq(min(response),max(response),length.out=5),2)
)
#... adding a legend with different sizes is an exercise at the end
# CHANGE TO THE NEXT PLOT
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="")
#... adding a legend with different sizes is an exercise at the end
title(main = 'C Value')
legend("center", legend=c(round(min(c), digits = 2), round(median(c), digits = 2), round(max(c), digits = 2)), pch=1, pt.cex = c(min(scale.c), median(scale.c), max(scale.c)), bty = 'n')
dev.off()

# ------------------------------------------------------------------------
# 3. (a) Go through ggplot2 options and make notes to yourself
#        on at least 5 options you want to use.
#        ALL EXAMPLES SHOWN HERE REQUIRE A DATAFRAME TO EXECUTE

value <- sample(1:30, 31, replace=TRUE)
value2 <- sample(90:100, 31, replace=TRUE)
day <- rep(c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), length.out=31)
weather <- rep(c('Rainy', 'Snowy', 'Sunny', 'Cloudy'), length.out=31)
test.data <- data.frame(value = value, DyofWk = day, value2=value2, weather=weather)

# 1. ggsave()
# - Function that saves a ggplot output to file.  This is not part of a ggplot() + call, but instead is a separate line call
# - ggsave(filename = "filename.filetype", width = number, height = number, unit = unit, plot = plot.name)
# - Matches the file type to the extension specified in the file name
# - Units are the measurements for the output figure dimensions (ex. in, mm, cm),
#   Units defaults to the units of the plotting window
# - plot defaults to last_plot() if not specified
ggsave(filename = "test.pdf", width = 5, height = 5)

# 2. geom_bar()
# - geom_bar at base form is meant to be used like a histogram (1 variable and counts)
# - The way to circumvent this is in ggplot() set x = the variable you want to plot, and
# - y = another column of the dataframe that groups the values specified in x.
# - Numerous examples of different fills and orientations can be found here:
# - http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
ggplot(data=data, aes(x=b.cut, y=a)) + geom_bar(stat='identity')
ggplot(data=test.data, aes(x=DyofWk, y=value)) + geom_bar(stat='identity')

# 3. geom_ribbon(aes(ymin=, ymax=), fill=) + geom_line(aes(y=))
# - geom_line plots a line using y = some.field in the input dataframe
# - geom_ribbon can be used in conjunciton to create an wider poly-line
# - Ribbon must come before line, otherwise the ribbon will over the line
# - Super useful for plotting values that have an associated range such
#   as standard deviation or a published accuracy range.
ggplot(data = test.data, aes(x=value, y=value2)) + geom_ribbon(aes(ymin=value2-1, ymax=value2+1), fill='grey40') + geom_line(aes(y=value2))

# 4. facet_grid()
# - facet_grid() is a way to break graphed data into different combinations of plots
#   based on categorical classifications shared among all data values
# - facet_grid(.~something) divides into columns
#   facet_grid(something.~) divides into rows
#   faced_grid(something ~ somethingelse) breaks into rows and columns
ggplot(test.data, aes(x=value, y=value2)) + facet_grid(.~DyofWk) + geom_point()
ggplot(test.data, aes(x=value, y=value2)) + facet_grid(DyofWk~.) + geom_point()
ggplot(test.data, aes(x=value, y=value2)) + facet_grid(DyofWk~weather) + geom_point()

# 5. geom_smooth()/stat_smooth()
# - Fits a smooth line to a set of data.
# - geom_smooth() is a simple fitting, stat_smooth() allows users to define the formula
# - Default is to use the 'loess' method adn the formula y ~ x.
ggplot(test.data, aes (x=value, y=value2)) + geom_point() + geom_smooth()
ggplot(data, aes (x=x, y=y)) + geom_point() + stat_smooth(method='lm', formula = y ~ poly(x,2))







