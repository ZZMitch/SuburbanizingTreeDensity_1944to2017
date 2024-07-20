### UTM Accuracy Check Boxplots ###

# Set Working Directory #
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Aerial Photography Time-Series/Mississauga Tree Change Paper/Field Accuracy")
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Aerial Photography Time-Series/Mississauga Tree Change Paper/Field Accuracy")

# Add Data #
Found <- read.csv("FoundTrees.csv")
NotFound <- read.csv("NotFoundTrees.csv")

# Create Boxplots #
#boxplot(Found$DBH)
#boxplot(NotFound$DBH)
#boxplot(DBH ~ Type, Found)
#boxplot(DBH ~ Type, NotFound)

FoundDec <- subset(Found, Type == "Dec")
FoundCon <- subset(Found, Type == "Con")
  
NotFoundDec <- subset(NotFound, Type == "Dec")
NotFoundCon <- subset(NotFound, Type == "Con")

# Combine Boxplots #
#boxplot(Found$DBH, NotFound$DBH, ylim = c(0,120))
#boxplot(DBH ~ Type, Found, ylim = c(0,120))
#boxplot(DBH ~ Type, NotFound, ylim = c(0,120))

labels = c("All","Deciduous","Coniferous","All","Deciduous","Coniferous")

par(mar = c(2.5,4,0.5,0.5))

boxplot(Found$DBH,FoundDec$DBH,FoundCon$DBH,NotFound$DBH,NotFoundDec$DBH,NotFoundCon$DBH,
        ylab = "DBH (cm)")
axis(1, at = c(1:6), labels = labels)

text(1,1,"n = 204")
text(2,1,"n = 118")
text(3,1,"n = 86")
text(4,1,"n = 173")
text(5,1,"n = 163")
text(6,1,"n = 10")

abline(a = NULL, b = NULL, h = NULL, v = 3.5)

text(0.45, 118, "a.", cex = 2)
text(3.7, 118, "b.", cex = 2)

text(2.8,119.5,"Recorded Trees", cex = 1.4)
text(6.125,119.5,"Missed Trees", cex = 1.4)


