# Manages sync and async queries with retries for timeout problems on sync queries. Called by barb_get_spots() and barb_get_programmes(). Not called directly by users.

Manages sync and async queries with retries for timeout problems on sync
queries. Called by barb_get_spots() and barb_get_programmes(). Not
called directly by users.

## Usage

``` r
barb_manage_query(
  query_url = NULL,
  query_params = list(),
  metric = "audience_size_hundreds",
  retry_on_initial_no_response = FALSE,
  fail_on_unsuccessful_pagination = FALSE,
  retries = 5,
  pause_before_retry = 90,
  async = FALSE,
  json_processor = NULL
)
```

## Arguments

- query_url:

  An API url e.g. barb_url_programmes()

- query_params:

  A list of query parameters

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

- async:

  should the async API be used?

- json_processor:

  A json processing function e.g. process_spot_json. Pass this without
  brackets().
