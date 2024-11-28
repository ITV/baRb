
# baRb

<!-- badges: start -->
<!-- badges: end -->

Functions for working with BARB's TV spot API

### Installation

```
remotes::install_github("ITV/baRb")
```

### Getting started

BaRb requires a username and password to be set using environment variables.
Use these commands or set the values in your .Renviron file.

```
Sys.setenv(BARB_API_USERNAME = "username")
Sys.setenv(BARB_API_PASSWORD = "password")
```


Get a list of available advertisers with:

```
barb_get_advertisers()
```

And a spot list for a specific advertiser with:

```
barb_get_spots(
  min_transmission_date = "2024-01-01",
  max_transmission_date = "2024-01-01",
  advertiser_name = "HAYS TRAVEL",
  async = FALSE)
```

Or programme data with:

```
barb_get_programmes(
  min_transmission_date = "2024-01-01",
  max_transmission_date = "2024-01-01",
  station_code = 10,
  async = FALSE
)
```

Official documentation for BARB's API can be found [here](https://barb-api.co.uk/api-docs).

### Sync and async queries

BARB's API has two sets of endpoints. 'Sync' endpoints return data immediately as json while 'async' endpoints initiate a query that runs on the server and results are downloaded as fragmented parquet files by a second API query once they are ready.

baRb wraps these two types of endpoints with a convenient switching option so that you can query either the sync or async endpoints and obtain a tibble of results in exactly the same format.

The following code will use the async endpoint to run a query:

```
barb_get_spots(
  min_transmission_date = "2024-01-01",
  max_transmission_date = "2024-01-01",
  advertiser_name = "HAYS TRAVEL",
  async = TRUE)
```

Sync endpoints return results faster for small queries but will easily time out.

Functionality from previous versions of baRb that would retry timed-out sync queries has been removed. If a query times out you will now receive an error and a suggestion to switch to async=TRUE.

Unless you're building an interactive application that needs to run small queries very quickly, you're probably better off using `async = TRUE` and the baRb package now defaults to `async = TRUE`.

### Why do I see the message "Removing duplicated spots"?

when using `barb_get_spots()` you will see the message 'Removing duplicated spots' and the total number of rows returned by the function will be fewer than is reported by sync API status messages during the query.
This difference is because BARB's API returns data for many panel regions, which causes spot data to be duplicated. The baRb package filters panel regions and removes these duplicates for you.
