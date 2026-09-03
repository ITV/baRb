# baRb

Functions for working with BARB’s TV spot API

**HOTFIX 2026-09-03: This package has been patched to work with Barb’s
API v3.0. Only the following functions are currently available.**

    barb_get_advertisers()
    barb_get_spots()

### Installation

    remotes::install_github("ITV/baRb")

### Getting started

BaRb requires a refresh token to be set using an environment variable.

See Barb’s [API
documentation](https://documenter.getpostman.com/view/52530320/2sBYAswBkZ#4008ee2c-d497-4166-bcce-c03b0dd1dff4)
to understand how to get a refresh token.

Use this commands or set the value in your .Renviron file.

    Sys.setenv(BARB_API_REFRESH_TOKEN = "TOKEN")

Get a list of available advertisers with:

    barb_get_advertisers()

And a spot list for a specific advertiser with:

    barb_get_spots(
      min_transmission_date = "2024-01-01",
      max_transmission_date = "2024-01-01",
      advertiser_name = "HAYS TRAVEL")

### Why do I see the message “Removing duplicated spots”?

when using
[`barb_get_spots()`](http://io.itv.com/baRb/reference/barb_get_spots.md)
you will see the message ‘Removing duplicated spots’ and the total
number of rows returned by the function will be fewer than is reported
by sync API status messages during the query. This difference is because
BARB’s API returns data for many panel regions, which causes spot data
to be duplicated. The baRb package filters panel regions and removes
these duplicates for you.
