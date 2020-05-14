options(java.parameters = "-Xmx4g")
pacman::p_load(tidyverse, magrittr, data.table, R.utils, skimr, nycflights13, YARF, MASS)

###Problem 1
bills = fread("https://github.com/kapelner/QC_Math_390.4_Spring_2020/raw/master/labs/bills_dataset/bills.csv.bz2")
discounts = fread("https://github.com/kapelner/QC_Math_390.4_Spring_2020/raw/master/labs/bills_dataset/discounts.csv.bz2")
bills = bills[discount_id != 5e6 & discount_id != 5693147]

set.seed(3)
discounts = discounts[sample(1 : .N)]
discounts_sub = head(discounts, 15)
bills = bills[sample(1 : .N)]
bills_sub = head(bills)[, customer_id := NULL]

bills_sub[order(discount_id)]
discounts_sub[order(id)]

###Problem 2
data(weather)
weather %<>% select(-origin, -time_hour)
skim(weather)


###### prob 7
pacman::p_load_gh("coatless/ucidata")
data(adult)
adult %<>% 
  na.omit #kill any observations with missingness
adult$income = as.factor(as.numeric(adult$income == ">50K"))
train_size = 2000
train_indices = sample(1 : nrow(adult), train_size)
adult_train = adult[train_indices, ]
y_train = adult_train$income
X_train = adult_train
X_train$income = NULL
test_indices = sample(setdiff(1 : nrow(adult), train_indices), train_size)
adult_test = adult[test_indices, ]
y_test = adult_test$income
X_test = adult_test
X_test$income = NULL
rf_mod = YARF(X_train, y_train)
rf_mod


#problem 9
data(Boston)
test_prop = 0.1
train_indices = sample(1 : nrow(Boston), round((1 - test_prop) * nrow(Boston)))
Boston_train = Boston[train_indices, ]
y_train = Boston_train$medv
X_train = Boston_train
X_train$medv = NULL
n_train = nrow(X_train)

tree_mod = YARFCART(X_train, y_train, 
                    bootstrap_indices = 1 : n_train, calculate_oob_error = FALSE, nodesize = 200)

illustrate_trees(tree_mod, max_depth = 4, open_file = TRUE)
get_tree_num_nodes_leaves_max_depths(tree_mod)
