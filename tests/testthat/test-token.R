withr::local_options(list(
  cli.default_handler = function(...) {} # turn off cli messages for testing
))

describe("get token", {
  it("returns an error if NA token is found", {
    withr::with_envvar(
      c(BARB_TOKEN = NA),
      expect_error(get_token())
    )
  })
  it("returns an error if NULL token is found", {
    withr::with_envvar(
      c(BARB_TOKEN = NA),
      expect_error(get_token(NULL))
    )
  })
  it("returns the environment variable by default", {
    res = withr::with_envvar(
      c(BARB_TOKEN = "abc"), {
        get_token()
      }
    )
    expect_equal(res, "abc")
  })
  it("prioritises the argument even if env var exists", {
    res = withr::with_envvar(
      c(BARB_TOKEN = "abc"),
      get_token("def")
    )
    expect_equal(res, "def")
  })
})

describe("set token", {
  it("correctly sets the environment variable", {
    withr::with_envvar(
      c(BARB_TOKEN = NA), {
        set_token("def")
        expect_equal(Sys.getenv("BARB_TOKEN"), "def")
      }
    )
  })
})

describe("check response", {
  it("returns TRUE with status code 200", {
    expect_true(check_response(list(status_code = 200)))
  })
  it("returns FALSE with any other status code", {
    expect_false(check_response(list(status_code = 401), verbose = FALSE))
  })
})