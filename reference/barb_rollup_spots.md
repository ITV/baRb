# Roll up raw spot data by broadcast time

Roll up raw spot data by broadcast time

## Usage

``` r
barb_rollup_spots(spots, plus_one = TRUE, hd = TRUE, granularity = "day")
```

## Arguments

- spots:

  A raw spot file

- plus_one:

  Roll up +1 channels? (T/F)

- hd:

  Roll up HD channels? (T/F)

## Value

A tibble of rolled up spots
