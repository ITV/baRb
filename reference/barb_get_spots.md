# Get a tibble of spots from the BARB API

Get a tibble of spots from the BARB API

## Usage

``` r
barb_get_spots(
  min_transmission_date = NULL,
  max_transmission_date = NULL,
  advertiser_name = NULL,
  consolidated = TRUE,
  use_reporting_days = FALSE,
  standardise_audiences = NULL,
  metric = "audience_size_hundreds",
  retry_on_initial_no_response = FALSE,
  fail_on_unsuccessful_pagination = FALSE,
  retries = 5,
  pause_before_retry = 90,
  remove_duplicates = TRUE,
  async = TRUE,
  last_updated_greater_than = NULL
)
```

## Arguments

- min_transmission_date:

  Start date of the spot query

- max_transmission_date:

  End date of the spot query

- advertiser_name:

  Advertiser name. Get names from barb_get_advertisers()

- consolidated:

  whether to return consolidated or only live viewing. Defaults to TRUE
  (consolidated).

- use_reporting_days:

  whether to use a standard 24 hour clock or the BARB reporting clock.
  Defaults to FALSE (standard 24 hour clock).

- standardise_audiences:

  whether to standardise impacts by spot time length. Options are the
  default of no standardisation (""), "using_duration" or
  "using_rate_factors".

- metric:

  Either "audience_size_hundreds" to return impacts, or "tvrs" to return
  TVR's

- retry_on_initial_no_response:

  If the API responds with no data for the first page of results, should
  baRb retry? Pagination will always automatically retry to avoid
  incomplete datasets.

- fail_on_unsuccessful_pagination:

  If the API has still not responded with data for a results page after
  all retries, should the function fail? FALSE will generate a warning
  but return results anyway.

- retries:

  Number of times to retry a page request that has responded with no
  data

- pause_before_retry:

  Time in seconds to pause before retrying. Helps to avoid a quick
  succession of consecutive failed queries that can trigger rate
  limiting.

- remove_duplicates:

  BARB's API reports against multiple panel_region definitions, some of
  which create duplicate impacts (e.g. spots are reported against both
  macro and micro regions). Should duplicate impacts be removed?

- async:

  should the async API be used?

- last_updated_greater_than:

  return only spots with a last amended date after "yyyy-mm-dd"

## Value

A tibble of TV spots

## Examples

``` r
#barb_get_spots(min_transmission_date = "2023-01-01", max_transmission_date = "2023-01-31", advertiser_name = "HAYS TRAVEL")
```
