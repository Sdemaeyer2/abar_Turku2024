
### CREATING HYPOTHETICAL OUTCOME PLOTS FOR BRMS MODEL ###

# make sure your model is loaded in your R session
# here we will apply it to the model M3

## Step 1: sample n number of parameter values from the posterior

# here we sample n = 20

S <- as_draws_df(M3) %>% 
  select(
    # select the necessary parameters to calculate the predicted scores
    b_Intercept, 
    b_FirstVersion_GM,
  ) %>%
  slice_sample(
    n = 20, # define the number of lines (draws) you want
    replace = T
  )

## Step 2: Create a vector of possible scores for your X variable

# here I make a vector of potential values for km4week_z (set a sensible range!)
# our km4week_z is a z score so I choose numbers between -3 and 3
X <- seq(-15, 15, by = .1)

## Step 3: Create an empty tibble that will be filled with predictions

Predictions <- tibble(
  draw = NULL,
  X = NULL,
  Pred1 = NULL  
)

## Step 4: For each of our n (here 20) samples of parameter values calculate a prediction of Y

for(i in 1:20){

  Si <- S[i,]
  
  Pred1 <- Si$b_Intercept + Si$b_FirstVersion_GM*X
  
  draw <- rep(i,length(X))
  
  Pred <- tibble(
    draw,
    X,  
    Pred1, 
  )
  
  Predictions <- rbind(Predictions, Pred)
}

# Check the result of our predictions

head(Predictions)

## Step 5: Make a plot!

P1 <- Predictions %>%
  select(draw, X, Pred1) %>%
  ggplot(aes(x = X,
             y = Pred1,
             group = draw)) +
  geom_line(color = "gray60", alpha = .6) +
  scale_y_continuous("predicted values") +
  scale_x_continuous("first version (centred around the mean)")

P1
