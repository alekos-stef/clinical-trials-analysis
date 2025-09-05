dat1 <- read.table("C:/Users/aleks/Desktop/exercise_ct.dat",header=T)
attach(dat1)

# Ερώτηση 1

dat1$mmse3 <- dat1$mmse2 - dat1$mmse1
 

# Μετατροπή σε κατηγορικές μεταβλητές 
dat1$sex <- factor(dat1$sex, levels = c('1', '2'), labels = c("male", "female"))

dat1$dose1 <- factor(dat1$dose1, levels = c("8", "16", "24"), labels = c("8", "16", "24"))
 
dat1$ae2 <- factor (dat1$ae2, levels = c('0', '1'), labels = c("No Adverse Reaction", "Adverse Reaction"))
 
# Τα στοιχεία του dataset
attach(dat1)
str(dat1)

#Περιγραφικά Χαρακτηριστικά των μεταβλητών
summary(age); sd(age)

summary(ian1);sd(ian1)
summary(ian2);sd(ian2)
summary(ian3);sd(ian3)


summary(mmse1);sd(mmse1)
summary(mmse2);sd(mmse2)
summary(mmse3);sd(mmse3)

summary(dat1$sex)
summary(dat1$dose1)
summary(dat1$ae2)

# Protevon erotima:
hist(mmse3, col = 'blue')
qqnorm(mmse3)
ks.test(dat1$mmse3, pnorm)
?ks.test

?kruskal.test
kruskal.test(dat1$mmse3 ~ dat1$dose1)
hist(mmse3); ks.test(mmse3, pnorm)
# Δευτερευοντα Ερωτήματα
# Ερώτημα 1
plot(dat1$dose1, mmse3, xlab = "dose1", ylab="mmse3")
pairwise.wilcox.test(dat1$mmse3, dat1$dose1, p.adj = "bonferroni", exact =F)

# Ερώτημα 2

plot(sex, mmse1, xlab="gender/sex", ylab="mmse1")

kruskal.test(dat1$mmse1 ~ dat1$sex)
wilcox.test(dat1$mmse1 ~ dat1$sex)

cor.test(dat1$mmse1, age)
cor.test(dat1$mmse1, ian1)
cor.test(dat1$mmse1, ian2)
cor.test(dat1$mmse1, ian3)



# Ερώτημα 3
hist(mmse1)
kruskal.test(dat1$mmse1 ~ dat1$dose1)

# Ερώτημα 4
plot(age, mmse3)

plot(ian1, mmse3)
plot(ian2, mmse3)
plot(ian3, mmse3)

cor.test(dat1$mmse3, age)
cor.test(dat1$mmse3, ian1)
cor.test(dat1$mmse3, ian2)
cor.test(dat1$mmse3, ian3)


hist(mmse3)
ks.test(mmse3, pnorm) 

wilcox.test(mmse3 ~ sex, data =dat1)


# Ερώτημα 5
?chisq.test
chisq.test(dat1$ae2, dat1$sex)
plot(ae2, ian1, ylab= "ian1", xlab = "ae2")
plot(ae2, ian2, ylab = "ian2", xlab = "ae2")
plot(ae2, ian3, ylab = "ian3", xlab = "ae2")

model51 <- glm (ae2 ~ ian1, binomial, data=dat1);summary(model51)
model52 <- glm (ae2 ~ ian2, binomial, data= dat1); summary(model52)
model53 <- glm (ae2 ~ ian3, binomial, data = dat1); summary(model53)

# Ερώτημα 6
model6 <- glm(ae2 ~ mmse1, binomial, data = dat1) ; summary(model6)


# Ερώτημα 7
?chisq.test
chisq.test(dat1$ae2, dat1$dose1)


# ΑΣΚΗΣΗ 2 
groupmeans = c(1.5, 3, 3)
power.anova.test(groups = 3, between.var = var(groupmeans), 
                 within.var = 5, sig.level = 0.05, power=0.8)
