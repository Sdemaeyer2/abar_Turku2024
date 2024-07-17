
### CREATING HYPOTHETICAL OUTCOME PLOTS FOR BRMS MODEL ###

# make sure your model is loaded in your R session
# here we will apply it to the model M3

## Step 1: sample n number of parameter values from the posterior

# here we sample n = 20
library(posterior)
S <- as_draws_df(M3) %>% 
  select(
    # select the necessary parameters to calculate the predicted scores
    b_Intercept, 
    b_FirstVersion_GM,
    ends_with(",Intercept]"),       # select class specific intercept residuals
    ends_with(",FirstVersion_GM]")  # select class specific slope residuals
  ) %>%
  slice_sample(
    n = 20, # define the number of lines (draws) you want per class
    replace = T
  ) %>%
  mutate(
    draw = seq(1:20)
  )

## Create long dataframe

S_Long <- S %>%

    ## Pivot longer

    pivot_longer(
    cols = c(
      ends_with(",Intercept]"),       # select class specific intercept residuals
      ends_with(",FirstVersion_GM]")  # select class specific slope residuals
    ),
    names_sep = ",",
    names_to = c("Class", "Parameter"),
    values_to = "residual"
  ) %>%
  
  ## remove parts of text variables to get good identifiers for Class and Parameter

  mutate(
    Class = str_remove(
      Class,
      pattern = ".*\\["
      ),
    Parameter = str_remove(
      Parameter,
      pattern = "\\]"
      )
  ) %>% 
  
  ## Pivot wider again to have column for each random parameter
  
  pivot_wider(
    names_from = Parameter,
    values_from = residual
  )
  

  
## Step 2: Create a vector of possible scores for your X variable

# here I make a vector of potential values for km4week_z (set a sensible range!)
# our km4week_z is a z score so I choose numbers between -3 and 3
X <- seq(-15, 15, by = .1)

## Step 3: Create an empty tibble that will be filled with predictions

Predictions <- tibble(
  draw = NULL,
  X = NULL,
  Pred1 = NULL,
  Class = NULL
)

## Step 4: For each of our n (here 20) samples of parameter values calculate a prediction of Y

for(i in 1:400){

  Si <- S_Long[i,]
  
  # Calculate a predicted score based on the fixed and random estimates of that draw
  
  Pred1 <- Si$b_Intercept + Si$Intercept + (Si$b_FirstVersion_GM + Si$FirstVersion_GM )*X  
  
  draw <- Si$draw
  
  Class <- as.factor(Si$Class)
  
  Pred <- tibble(
    draw,
    X,  
    Pred1, 
    Class
  )
  
  Predictions <- rbind(Predictions, Pred)
}


## Step 5: Make a plot!

P1 <- Predictions %>%
  select(draw, X, Pred1, Class) %>%
  ggplot(aes(x = X,
             y = Pred1,
             group = draw)) +
  geom_line(color = "gray60", alpha = .6) +
  facet_wrap(~Class, labeller = label_both) +
  scale_y_continuous("predicted values") +
  scale_x_continuous("first version (centred around the mean)")

P1
