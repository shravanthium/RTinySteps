WBCD_data <- read.csv("/home/shravanthi/igeek/RTinySteps/WBCD_data.csv")
WBCD_data
bc  <- WBCD_data
str(bc)
names(bc)
names(bc) <- c("id", "label", names(bc)[3:32])
summary(bc)

plot(bc$X1001, bc$X0.1184)
plot(bc$X1001, bc$X0.1184, col=bc$label, pch=20, main ="X1001 vs X0.1184", xlab="V3", ylab="V4" )

legend("topright", legend=unique(bc$label), col=unique(bc$label),pch=20)

pairs(bc,[3:32])
pairs(bc,[3:32])

cor(bc[,3:32])
cor(bc[,3:32]).test

boxplot(bc[, 3:32])
hist(bc$X1.095)

groups <- sample(bc$id, size=dim(bc)[1], replace=F)

bc.test <- subset(bc, bc$id %in% groups[530:569])
bc.cval <- subset(bc, bc$id %in% groups[456:529])
bc.train <- subset(bc, bc$id %in% groups[1:455])

lrmodel1 <- glm(label ~ X1001+X0.1184+X0.2776, data=bc.train, family="binomial")
summary(lrmodel1)

lrmodel2 <- glm(label ~ X0.3001+X0.1471+X0.2419+X0.07871+X1.095+X0.9053+X8.589+X153.4+X0.006399+X0.04904+X0.05373+X0.01587+X0.03003+X0.006193+X25.38+X17.33+X184.6+X2019+X0.1622+X0.6656+X0.7119+X0.2654+X0.4601+X0.1189, data=bc.train, family="binomial")
summary(lrmodel2)
lrmodel2.p.cval <- predict(lrmodel2, bc.cval, type="response")

lrmodel2.p.cval.label <-rep("B", dim(bc.cval)[1])
lrmodel2.p.cval.label[lrmodel2.p.cval > 0.5] <- "M"

predicted <- lrmodel2.p.cval.label
actual <- as.character(bc.cval$label)

confmat <- as.matrix(table(actual, predicted))

tp <- confmat[2,2]
fp <- confmat[1,2]
tn <- confmat[1,1]
fn <- confmat[2,1]

precision <- tp/(tp+fp)

recall <- tp/(tf+fn)
recall <- tp/(tp+fn)

fdr = 1-precision

fdr <- fp/(tp+fp)

confmat
bc.pca <- prcomp(bc, scale=T)
bc.pca <- prcomp(bc[,3:32], scale=T)

bc.pca$center
bc.pca$scale
bc.pca$rotation

biplot(bc.pca, scale=0)
bc.pca$sdev^2
bc.pc.var <- bc.pca$sdev^2
bc.pca.pve <- bc.pca.var/sum(bc.pca.var)
bc.pca.var <- bc.pca$sdev^2
bc.pca.pve <- bc.pca.var/sum(bc.pca.var)
bc.pca.var
bc.pca.pve
