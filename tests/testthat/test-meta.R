# from API docs
panel_example = list(
  list(
    panel_code = 1,
    panel_region = "north west",
    is_macro_region = TRUE
  )
)

describe("barb_get_panels", {
  it("successfully adds the class to the returned object", {
    mockery::stub(barb_get_panels, "barb", panel_example)
    res = barb_get_panels(as_tibble = FALSE)
    expect_s3_class(res, "barb_panels")
  })
  it("successfully returns a tibble if asked", {
    mockery::stub(barb_get_panels, "barb", panel_example)
    res = barb_get_panels(as_tibble = TRUE)
    expect_s3_class(res, "tbl")
  })
})