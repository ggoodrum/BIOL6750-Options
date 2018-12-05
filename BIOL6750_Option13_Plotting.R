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
c <- rnorm(100)
response <- a * b + c + rnorm(100)
data <- data.frame(a=a, b=b, c=c, response=response)

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
pdf("Option1-.pdf")
# Make two plotting frames, with a 2:1 ratio of their heights
layout(matrix(1:2, nrow=2))
plot(a ~ b, col=better.cols[cut.resp], cex=scale.c)
legend("bottomright", legend=c, col = better.cols[cut.resp], cex = scale.c)
# Make a blank plot at the bottom
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", axes=FALSE)
# Add a gradient scale bar
library(plotrix)
gradient.rect(0, 0, 1, 1, col=better.cols, border=NA)
# Give it some labels
text(seq(0,1,length.out=5), y=.5,
     labels=round(seq(min(response),max(response),length.out=5),2)
)
#... adding a legend with different sizes is an exercise at the end
legend("topleft", legend="hey you", pch=20)
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
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", axes=FALSE)
# Add a gradient scale bar
library(plotrix)
gradient.rect(0, 0, 1, 1, col=better.cols, border=NA)
# Give it some labels
text(seq(0,1,length.out=5), y=.5,
     labels=round(seq(min(response),max(response),length.out=5),2)
)
#... adding a legend with different sizes is an exercise at the end
# CHANGE TO THE NEXT PLOT
plot(NA, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", axes=FALSE)
#... adding a legend with different sizes is an exercise at the end
legend("center", legend='C Value', pch='O', title = 'Legend')
dev.off()

# ------------------------------------------------------------------------
# 3. (a) Go through ggplot2 options and make notes to yourself
#        on at least 5 options you want to use.




