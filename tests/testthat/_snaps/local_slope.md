# local_slope() returns expected values

    Code
      slopes
    Output
      # A tibble: 996,000 x 4
              x2 .draw slope smooth
           <dbl> <dbl> <dbl>  <dbl>
       1 0.00234     1 -32.5  -4.02
       2 0.00234     2 -60.3   5.92
       3 0.00234     3 -16.9  -2.37
       4 0.00234     4 -35.7  -2.85
       5 0.00234     5 -36.2  -1.59
       6 0.00234     6 -49.9   2.01
       7 0.00234     7 -19.8  -4.85
       8 0.00234     8 -35.7   2.80
       9 0.00234     9 -15.1  -4.74
      10 0.00234    10 -24.6  -3.06
      # ... with 995,990 more rows

# local_slope() returns expected values for factor-smooth interaction

    Code
      slopes2
    Output
      # A tibble: 2,754,000 x 5
         g         x2 .draw slope  smooth
         <chr>  <dbl> <dbl> <dbl>   <dbl>
       1 a     0.0774     1 15.4  -1.63  
       2 a     0.0774     2 50.6  -6.53  
       3 a     0.0774     3 20.1   0.0629
       4 a     0.0774     4 24.4  -3.21  
       5 a     0.0774     5  6.83  8.14  
       6 a     0.0774     6  4.33  0.710 
       7 a     0.0774     7 79.3  -5.42  
       8 a     0.0774     8 28.9  -5.26  
       9 a     0.0774     9 75.6  -6.38  
      10 a     0.0774    10  9.15  1.07  
      # ... with 2,753,990 more rows

