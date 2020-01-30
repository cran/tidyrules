## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
library("tidyrules")
library("dplyr")
library("C50")
library("pander")

# build model
c5_model <- C5.0(Species ~ ., data = iris, rules = TRUE)

# extract rules in a tidy tibble
tidy_rules <- tidyRules(c5_model)

# View tidy_rules
tidy_rules %>% 
  select(-c(rule_number,trial_number)) %>% 
  pandoc.table()

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
# Example 1, filter rules based on support
tidy_rules %>% 
  filter(support >= 48) %>% 
  select(LHS, RHS)


# Example 2, filter rules based on RHS
tidy_rules %>% 
  filter(RHS == "virginica") %>% 
  select(LHS, support, confidence, lift)

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
iris %>% 
  filter(eval(parse(text = tidy_rules[3,"LHS"]))) %>%  # filter using a C5 rule
  count(Species)

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
# loading packages
library("tidyrules")
library("C50")
library("dplyr")

# attrition data load
data("attrition", package = "rsample")
attrition <- as_tibble(attrition)

glimpse(attrition)

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
# our C5 model
c5_att <- C5.0(Attrition ~ ., data = attrition, rules = TRUE)

# sample rules from C5
c5_att$output %>% 
  stringr::str_sub(start = 194L
                   , end = 578L) %>% 
  writeLines()

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
# Extract rules to a tidy tibble
tr_att <- tidyRules(c5_att)

tr_att

## -----------------------------------------------------------------------------
tr_att %>% 
  head(5) %>% 
  select(LHS,RHS) %>% 
  pandoc.table(split.cells = 60)

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
rules_example_1 <- tr_att %>% 
  filter(RHS == "No") %>% 
  arrange(desc(support))

rules_example_1

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
# filter a rule with conditions
large_support_rule <- tr_att %>% 
  filter(RHS == "Yes") %>% 
  top_n(1, wt = support) %>% 
  pull(LHS)

# parseable rule 
parseable_rule <- parse(text = large_support_rule)

# apply filter on data frame using parseable rule
attrition %>% 
  filter(eval(parseable_rule))

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
library("tidyrules")
library("dplyr")
library("rpart")
# BreastCancer
data(BreastCancer, package = "mlbench")
bc_train <- BreastCancer %>%
  select(-Id) %>%
  mutate_if(is.ordered, function(x) x <- factor(x,ordered = F))

rpart_bc <- rpart(Class ~ ., data = bc_train)

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
library("rpart.plot")
prp(rpart_bc)

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
# tidyrule extract
rules_bc <- tidyRules(rpart_bc)

rules_bc

# filter the data using a rule 
bc_train %>% 
  filter(eval(parse(text = rules_bc[5,"LHS"]))) %>% 
  as_tibble()

## ----warning=FALSE,echo=TRUE,message=FALSE------------------------------------
library("tidyrules")
library("dplyr")
library("Cubist")
# ames housing data set
ames   <- AmesHousing::make_ames()
cubist_ames <- cubist(x = ames[, setdiff(colnames(ames), c("Sale_Price"))],
                          y = log10(ames[["Sale_Price"]]),
                          committees = 3
                          )

# rule extract 
rules_ames <- tidyRules(cubist_ames)

rules_ames

