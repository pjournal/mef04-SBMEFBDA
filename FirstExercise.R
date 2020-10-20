#Temperature Conversion

convert_temperature <- function(temperature,F_to_C){
  if (F_to_C == FALSE) {
    new_temperature = (temperature * 9 / 5) + 32
  }
  else{
    new_temperature = (temperature - 32) * 5 / 9
  }
  return(new_temperature)
}

x <- convert_temperature(temperature = 86,F_to_C = TRUE)
x

#Future Value of an Investment

calculate_future_value <- function(investment, interest, duration_in_years){
  future_val = investment * (1 + interest)^duration_in_years
  return(future_val)
}

calculate_future_value(investment = 100, interest = 0.07, duration_in_years = 5)

#Calculate Probability of Dice

get_prob_dice <- function(six_count, throw_count){
  probability_rate <- dbinom(six_count, size=throw_count, prob=1/6) 
  return(probability_rate)
}

get_prob_dice(3,5)

#Generating Color Hex Codes

generate_hex_code <- function(n){
  num_vals <- c(0:9)
  letter_vals <- c("a","b","c","d","e","f")
  all_vals <- append(num_vals, letter_vals)
  hex_code <- ""
  hex_array <- c()
  for (y in 1:n) {
    for (x in 1:6) {
      hex_code <- paste0(hex_code, sample(all_vals,1))
    }
    hex_code <- paste0("#",hex_code)
    hex_array[y] <- hex_code
    hex_code <- ""
  }
  return(hex_array)
}

generate_hex_code(n=3)

#Rock, Scissors, Paper

rsp_game <- function(choice){
  possible_choices <- c("rock","scissors","paper")
  func_choice <- sample(1:3, 1)
  my_choice <- possible_choices[func_choice]
  if (my_choice == choice) {
    return("I choose the same. Tie!")
  }
  else if(my_choice == "rock" & choice == "scissors"){
    return("You lost, I choosed rock.")
  } 
  else if(my_choice == "rock" & choice == "paper"){
    return("You won, I choosed rock.")
  } 
  else if(my_choice == "scissors" & choice == "rock"){
    return("You won, I choosed scissors")
  } 
  else if(my_choice == "scissors" & choice == "paper"){
    return("You lost, I choosed scissors")
  } 
  else if(my_choice == "paper" & choice == "rock"){
    return("You lost, I choosed paper")
  } 
  else if(my_choice == "paper" & choice == "scissors"){
    return("You won, I choosed paper")
  } 
}

rsp_game("rock")

