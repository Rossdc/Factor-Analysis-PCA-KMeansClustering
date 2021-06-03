library(dplyr)
library(tidyverse)
library(ggfortify)
library(tidyr)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(broom)
library(reshape2)
library(ggpubr)
library(zoo)
library(nortest)
library(stringr)

#PCA ANALYSIS - INSIGHT 1

df <- read.csv("murica.csv", header = T, sep = ",")

#Several NA's
df[!complete.cases(df),]

df$Car.Type[df$Car.Type=="MiniVan"] <- "1"
df$Car.Type[df$Car.Type=="Pickup"] <- "2"
df$Car.Type[df$Car.Type=="regular"] <- "3"
df$Car.Type[df$Car.Type=="Sports"] <- "4"
df$Car.Type[df$Car.Type=="SUV"] <- "5"
df$Car.Type[df$Car.Type=="Wagon"] <- "6"

#Removing columns we don't need

df <- select(df,-c("Drive.Train.Type", "Vehicle.Name"))

df$Car.Type <- as.numeric(df$Car.Type)

#Putting in means for columns being selected
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df <- replace(df, TRUE, lapply(df, NA2mean))

#Scree Plot

library(FactoMineR)

res.pca <- PCA(df, graph = FALSE)

library(factoextra)


#Eigenvalues can be used to determine the number of principal components to retain after PCA (Kaiser 1961)
eig.val <- get_eigenvalue(res.pca)
eig.val

#Choose two as it explains 73.8% of the variation

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60)) +
  theme_bw()

######Correlation####

correalation_matrix <- data.frame(round(cor(df[sapply(df, is.numeric)], use="complete.obs"), digits = 3))

#Correalation Plot
corrplot::corrplot(cor(correalation_matrix, method = "kendall"), method = "number", type = "upper", tl.col="black", tl.cex=0.8, tl.srt=70, bg = "lightgrey")

#Manual Line of Best Fit
lineofbestfit <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

#Correlogram
p1 <- ggpairs(df, lower = list(continuous = wrap(lineofbestfit, method = "lm")),
              diag = list(continuous = wrap("barDiag", colour = "blue", bins = 5)),
              upper = list(continuous = wrap("cor", size = 5)), title = "Correlogram")
p1

#Graphing variables
var <- get_pca_var(res.pca)
head(var$coord)
head(var$contrib)

#Factor Map - quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates)
fviz_cos2(res.pca, choice = "var", axes = 1:2) +
  theme_bw()

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
) +
  theme_bw()

######Clustering####
set.seed(123)

#Determining optional numbers of clusters
fviz_nbclust(df, kmeans, method = "silhouette")
kmeans2 <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(kmeans2, df)

aggregate(df, by=list(cluster=kmeans2$cluster), mean)

res.km <- kmeans(var$coord, centers = 2, nstart = 25)
grp <- as.factor(res.km$cluster)

#####PCA#####

# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF"),
             legend.title = "PC") +
  theme_bw()

fviz_pca_biplot(res.pca, 
                col.ind = factor(df$Car.Type), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Car Type") +
  theme_bw()

fviz_pca_biplot(res.pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = factor(df$Car.Type),
                col.ind = "black",
                # Color variable by groups
                col.var = factor(grp),
                legend.title = list(fill = "Car Type", color = "PC"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")+
  theme_bw()# Variable colors

fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = factor(df$Car.Type), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Car Type", color = "Contrib",
                                    alpha = "Contrib")
) + theme_bw()

res.ind <- get_pca_ind(res.pca)
# 2. Coordinate of groups
coord.groups <- res.ind$coord %>%
  as_data_frame() %>%
  select(Dim.1, Dim.2) %>%
  mutate(CarType = groups) %>%
  group_by(CarType) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2)
  )
coord.groups

#Getting results of PC's
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
res.desc$Dim.2

res.pca

#PCA

library(caTools)
set.seed(123)

#Creating test and training set
split = sample.split(df$Car.Type, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

training_set[-1] = scale(training_set[-1])
test_set[-1] = scale(test_set[-1])

library(lattice)
library(caret)
library(e1071)

#Selecting PCA's
pca = preProcess(x = training_set[-1], method = 'pca', pcaComp = 2)

training_set = predict(pca, training_set)
#moving dependent variable
training_set = training_set[c(2, 3, 1)]

#same for test set
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

library(e1071)

#SVM MODEL BEING USED
classifier = svm(formula = Car.Type ~ .,#dependent variable
                 data = training_set, # this will be the new training set
                 type = 'C-classification',
                 kernel = 'linear')

y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(actual = test_set[, 3], predicted = y_pred)
sum(cm)

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
#Creating grid
grid_set = expand.grid(X1, X2)
#PC1 and PC2 added as grid axes
colnames(grid_set) = c('PC1', 'PC2')

#Adding to grid
y_grid = predict(classifier, newdata = grid_set)

#Plot

plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'red', ifelse(y_grid == 1, 'purple', 'pink')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'orange', ifelse(set[, 3] == 1, 'pink', 'red3')))


#Almost all data points fall within the regular category hence most data relates to regular cars

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) +
  theme_bw()
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) +
  theme_bw()

#Total contribution
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10) +
  theme_bw()

#####ANOVA - Insight 2#####

#Using run if to generate normally distributed data - Sales
Sales <- round(runif(1000, min=5, max=100),0)

#Creating Products
Product <- c("Nebuliser", "Inhaler", "CPAP")
Product<- sample(rep(Product, c(333,333,334)))

#Creating Distribution Channel
Channel <- c("Pharmacy", "Direct")
Channel <- sample(rep(Channel,c(500,500)))

#Creating Data Frame

df1 <- data.frame(Sales,Product,Channel)

#Boxplot1

ggplot(df1, aes(Product, Sales)) + geom_boxplot(aes(fill = Channel)) + theme_bw()+
  stat_boxplot(geom ='errorbar') +
  ggtitle("Distribution of Sales by Channel") +
  theme(plot.title = element_text(hjust = 0.5))

#qq plot
ggqqplot(df1, "Sales", ggtheme = theme_bw()) +
  facet_grid(Product ~ Channel, labeller = "label_both") + theme_bw()

#Testing for homogeniety
library(DescTools)
LeveneTest(Sales ~ Product*Channel, df1)

#ANOVA
res.aov2 <- aov(Sales ~ Product*Channel, data = df1)
summary(res.aov2)

#Creating Graphs
model  <- lm(Sales ~ Product*Channel,
             data = df1)

step<- step(model, direction="backward", trace = FALSE)
summary(step)$coeff
b<- ggplot2::fortify(step)

#QQ Plot
normal <- ggplot(b, aes(qqnorm(.stdresid)[[1]], .stdresid)) + geom_point(colour="blue")+ stat_smooth(method="lm")
normal <- normal + ggtitle("Q-Q Normal")

#Turkey Test
tukeyresults <- TukeyHSD(res.aov2)
plot(tukeyresults)

#Boxplot
ggboxplot(b, x = "Product", y = "Sales", color = "Channel", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(b$Sales), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 120)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")
