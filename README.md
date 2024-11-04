
# baRb

<!-- badges: start -->
<!-- badges: end -->

Functions for working with BARB's TV spot API

### Installation

```
remotes::install_github("neilcharles/baRb")
```

### Getting started

BaRb requires a username and password to be set using environment variables.
Use these commands or set in your .Renviron file.

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
  min_transmission_date = "2023-01-01",
  max_transmission_date = "2023-01-31",
  advertiser_name = "hays_travel")
```

Official documentation for BARB's API can be found [here](https://barb-api.co.uk/api-docs).
