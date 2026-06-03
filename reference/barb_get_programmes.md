# Get a tibble of spots from the BARB API

Get a tibble of spots from the BARB API

## Usage

``` r
barb_get_programmes(
  min_transmission_date = NULL,
  max_transmission_date = NULL,
  station_code = "",
  macro_regions = FALSE,
  consolidated = TRUE,
  use_reporting_days = FALSE,
  async = TRUE
)
```

## Arguments

- min_transmission_date:

  Start date of the spot query

- max_transmission_date:

  End date of the spot query

- station_code:

  A station code obtained with barb_get_stations()

- macro_regions:

  whether to return macro or standard region areas

- consolidated:

  whether to return consolidated or only live viewing. Defaults to TRUE
  (consolidated).

- use_reporting_days:

  whether to use a standard 24 hour clock or the BARB reporting clock.
  Defaults to FALSE (standard 24 hour clock).

- async:

  should the async API be used?

## Value

A tibble of TV spots

## Examples

``` r
# barb_get_programmes(min_transmission_date = '2024-01-01', max_transmission_date = '2024-01-01', station_code = 10)
```
