# Submit a query to the BARB API. Not called directly by users.

Submit a query to the BARB API. Not called directly by users.

## Usage

``` r
barb_query_api(url, query = list(), async = FALSE, async_check_interval = 20)
```

## Arguments

- url:

  Endpoint URL

- query:

  List of query parameters

## Value

API results json if async = FALSE or download URLs if async = TRUE
