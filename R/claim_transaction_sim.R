library(dplyr)
library(ids) ##for making random claim numbers
library(purrr)
library(lubridate)
library(tidyr)
library(ggplot2)


gen_ult_claim <- function(n) {
  ##simulate n lognormally distributed ultimate claim values
  rlnorm(n, 10, 2)
}

gen_num_tran <- function(n) {
  ##simulate n many transaction counts
  ## currently a fixed poisson lambda = 10 
  ## transaction count distribution
  ##
  ## TODO: investigate reasonableness of this distribution
  pmax(rpois(n, lambda = 10), 1)
}

gen_acc_date <- function(acc_year) {
  ## Return a vector of all calendar dates in
  ##  acc_year.
  
    days_non_leap <- sample(0:364, size = length(acc_year), replace = TRUE)
    days_leap <- sample(0:365, size = length(acc_year), replace = TRUE)
    
    days <- if_else(leap_year(acc_year), days_leap, days_non_leap)
    
    dates <- lubridate::ymd(paste0(acc_year, "-01-01")) + days(days)
    
    return(dates)
}

gen_dur <- function(num_tran) {
  ##return claim_duration in days
  
  ## floor at number of transactions so that transactions
  ## can be evenly split along days
  #pmax(rpois(length(num_tran), lambda = 270), num_tran)
  pmax(sample(1:3600, size = length(num_tran), replace = TRUE), num_tran)
}

gen_tran_dates <- function(acc_date, dur, num_tran) {
  ## Generate num_tran many dates between (inclusive)
  ## acc_date and acc_date + dur.
  
  
  close_date <- acc_date + days(dur)
  
  dates_to_sample <- purrr::map2(acc_date,
                                 close_date,
                                 seq,
                                 by = "days")

  
  helper_fun <- function(dates, n) {
    sample(dates, size = n, replace = FALSE)
  }
  
  tran_dates <- purrr::map2(dates_to_sample,
                            num_tran,
                            helper_fun)
  
  return(tran_dates)
}

gen_payments <- function(ult_loss, book_dates) {
  ## ult_loss should be a single number
  ## book_dates should be a vector of dates to
  ##  generate a payment amount for
  
  n <- length(book_dates)
  
  payments <- vector(mode = "numeric",
                     length = n)
  
  
  for(i in 1:n) {
    
    if(i == n) {
      payments[i] <- ult_loss
    } else {
      ## sample 1 observation from a uniform (0,ult_loss)
      ##  distrubtion then decrement ult_loss.
      payments[i] <- runif(1, 0, ult_loss)
      ult_loss <- ult_loss - payments[i]
    }
  }
  
  return(payments)
}

gen_ay_activity <- function(acc_year, claim_count) {
  ## Simulates transaction_level claim activity
  ##  for each acc_year and the correspond 
  ## claim_count.
  ##
  ## acc_year and claim_count are equal length vectors
  ## such that acc_year[i] has claim_count[i] 
  ## claims.
  
  
  out <- tibble::tibble(claim_num = ids::sentence(n = claim_count, style = "kebab"),
                        ult_loss  = gen_ult_claim(claim_count),
                        num_tran  = gen_num_tran(claim_count),
                        acc_year  = acc_year)
  
  out <- out %>%
    mutate(acc_date = gen_acc_date(acc_year),
           duration = gen_dur(num_tran = num_tran),
           book_date = purrr::pmap(.l = list(acc_date,
                                             duration,
                                             num_tran),
                                   .f = gen_tran_dates)) %>%
    ## weird unnesting step - the call to purrr::pmap above
    ##  returns a single-element list for each row, and in that list
    ##  is a vector of book_dates with length equal to the number of
    ##  transactions in that row.
    ## This unnest() call is removing the single-element list, but leaving
    ##  the vectors.
    unnest(cols = 'book_date') %>%
    ## about to exploit the fact that the book dates are still in 
    ## a random order
    mutate(payment = purrr::pmap(.l = list(ult_loss,
                                           book_date),
                                 .f = gen_payments)) %>%
    ## unnest the book_date vector-column and the payment vector-column
    ##  Note that this will fail unless each row's vectors have the same lenght
    ##  in each column.  
    unnest(cols = c("book_date", "payment")) %>%
    arrange(book_date) %>%
    group_by(claim_num) %>%
    mutate(paid_to_date = cumsum(payment),
           unpaid       = ult_loss - paid_to_date) %>%
    ungroup() %>%
    select(-acc_year)
  
  return(out)
}


## Create a dataframe with accident year and claim count
## information.  
## Currently using simple poisson distribution to
## vary claim counts independently by acc_year.
test_df <- tibble::tibble(acc_year = 2010:2020,
                          claim_count = rpois(11, lambda = 100))

## Simulate transaction-level claim payments for
## 2010 - 2020.
test_df <- test_df %>%
  mutate(claim_activity = purrr::map2(acc_year, claim_count,
                                      gen_ay_activity)) %>%
  unnest(claim_activity)


## Plot cumulative paid amounts for each claim
##  faceting on accident year.
test_df %>%
  ggplot(aes(x = book_date,
             y = paid_to_date,
             color = claim_num)) +
  geom_line() +
  facet_wrap(~acc_year) +
  theme(legend.position = "none")


## Plot ultimate unpaid (typically not known) for each
##  accident year at common ages.
## For instance, 2010-01-31 and 2011-01-31 are both 
##  age 30 days for accident years 2010 and 2011 respectively.
test_df %>%
  mutate(book_age = as.numeric(book_date - ymd(paste0(acc_year, "-01-01")))) %>%
  group_by(acc_year, book_age) %>%
  arrange(book_age) %>%
  summarize(unpaid = sum(unpaid),
            .groups = "drop") %>%
  group_by(acc_year) %>%
  mutate(total_ay_unpaid = sum(unpaid) - cumsum(unpaid)) %>%
  ungroup() %>%
  ggplot(aes(x = book_age,
             y = total_ay_unpaid,
             color = factor(acc_year))) +
  geom_line()

## Plot cumulative paid loss at each book_date
## for each accident year.
test_df %>%
  mutate(book_age = as.numeric(book_date - ymd(paste0(acc_year, "-01-01")))) %>%
  arrange(book_age) %>%
  group_by(acc_year, book_age) %>%
  summarize(payment = sum(payment),
            .groups = "drop") %>%
  group_by(acc_year) %>%
  mutate(cumulative_paid = cumsum(payment)) %>%
  ungroup() %>%
  ggplot(aes(x = book_age, y = cumulative_paid,
             color = factor(acc_year))) +
  geom_line()
  

