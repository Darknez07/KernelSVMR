#Importing  the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

test_set[-3] = scale(test_set[-3])
training_set[-3] = scale(training_set[-3])

head(training_set)

library(e1071)

clf = svm(formula = Purchased~.,
          data = training_set,
          kernel = "radial")

y_pred = predict(clf, newdata = test_set)

head(data.frame(y_pred))
head(data.frame(test_set[,3]))

cm = table(test_set[,3],y_pred)

library('ggplot2')

ggplot(data =  data.frame(cm), mapping = aes(x = y_pred, y = Var1)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "blue") 
theme_bw() + theme(legend.position = "none")

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(clf, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, 
        matrix(as.numeric(y_grid), 
               length(X1), length(X2)), 
        add = TRUE)
points(grid_set, pch = '.', 
       col = ifelse(y_grid == 1, 
                    'springgreen3', 'tomato'))
points(set, pch = 21, 
       bg = ifelse(set[, 3] == 1, 
                   'green4', 'red3'))

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(clf, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), 
                       length(X1), length(X2)), 
        add = TRUE)
points(grid_set, pch = '.', 
       col = ifelse(y_grid == 1, 
                    'springgreen3', 'tomato'))
points(set, pch = 21, 
       bg = ifelse(set[, 3] == 1, 
                   'green4', 'red3'))


