#install.packages("ggplot2")
library("ggplot2")
CAWI_data <- read.csv("/home/shravanthi/igeek/RTinySteps/crdCAW_1_2013.csv")
cw <- CAWI_data
#column names
names(cw)
view(CAWI_data)
str(cw)
summary(cw)
summary(cw$State.UTs)
summary(cw$Crime.Head)
head(cw)
levels(cw$State.UTs)
ggplot(data=cw, aes(x=Cases.reported.during.the.year, y=Cases.sent.for.trial.during.the.year, color=Crime.Head)) + geom_point()
ggplot(data=cw, aes(x=log(Cases.reported.during.the.year), y=log(Cases.sent.for.trial.during.the.year), color=Crime.Head)) + geom_point()

gp1 = ggplot(data=cw, aes(x=log(Cases.reported.during.the.year), y=Cases.sent.for.trial.during.the.year, color=Crime.Head)) + geom_point()
gp1 + facet_wrap(~State.UTs, ncol=2)

gp2 = ggplot(data=cw, aes(x=State.UTs, y=Cases.sent.for.trial.during.the.year)) + geom_bar(stat="identity")
gp2 = ggplot(data=cw, aes(x=Crime.Head, y=Cases.sent.for.trial.during.the.year)) + geom_bar(stat="identity")
gp2 + facet_wrap(~State.UTs, ncol=2)

crimes <- as.character(levels(cw$Crime.Head))
states <- as.character(levels(cw$State.UTs))

cw.5states <- subset(cw, cw$State.UTs %in% head(states))
gp3 = ggplot(data=cw.5states, aes(x=Crime.Head, y=Cases.sent.for.trial.during.the.year, fill=Crime.Head)) + geom_bar(stat="identity")
gp3 + facet_wrap(~State.UTs, ncol=2)

cw.5states.5crimes <- subset(cw, cw$State.UTs %in% head(states) && cw$Crime.Head %in% head(crimes))
cw.5states.5crimes
gp4 = ggplot(data=cw.5states.5crimes, aes(x=Crime.Head, y=Cases.sent.for.trial.during.the.year, fill=Crime.Head)) + geom_bar(stat="identity")
gp4 + facet_wrap(~State.UTs, ncol=2)

