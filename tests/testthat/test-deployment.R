# Where the app thinks it is running, and what that changes. Nothing here
# depends on the actual environment: every case sets the signal it is testing.

with_env <- function(vars, code) {
  old <- Sys.getenv(names(vars), names = TRUE, unset = NA)
  do.call(Sys.setenv, as.list(vars))
  on.exit({
    set <- old[!is.na(old)]
    if (length(set)) do.call(Sys.setenv, as.list(set))
    unset <- names(old)[is.na(old)]
    if (length(unset)) Sys.unsetenv(unset)
  }, add = TRUE)
  force(code)
}

clear_signals <- function(code) {
  with_env(
    c(
      SHINY_PORT = "", SHINY_SERVER_VERSION = "", R_CONFIG_ACTIVE = "",
      CONNECT_SERVER = "", RSTUDIO_CONNECT_HASTE = "", ANALYSISKIT_SERVER = ""
    ),
    withr::with_options(list(analysiskit.server = NULL), code)
  )
}


test_that("a plain local session is not mistaken for a server", {
  skip_if(dir.exists("/opt/shiny-server"), "this machine really is a Shiny Server")
  clear_signals(expect_false(ak_is_server()))
})

test_that("each hosting signal is recognised", {
  for (signal in c(
    "SHINY_PORT", "SHINY_SERVER_VERSION", "CONNECT_SERVER", "RSTUDIO_CONNECT_HASTE"
  )) {
    clear_signals({
      vars <- stats::setNames(list("set"), signal)
      with_env(vars, expect_true(ak_is_server(), info = signal))
    })
  }

  # shinyapps.io identifies itself through the value, not the presence.
  clear_signals(
    with_env(c(R_CONFIG_ACTIVE = "rsconnect"), expect_true(ak_is_server()))
  )
  clear_signals(
    with_env(c(R_CONFIG_ACTIVE = "default"), expect_false(ak_is_server()))
  )
})

test_that("an unrecognised deployment can be told what it is", {
  # The detection cannot know every host, so it must be overridable rather than
  # leaving someone stuck with a folder picker that writes into a container.
  clear_signals({
    withr::with_options(list(analysiskit.server = TRUE), expect_true(ak_is_server()))
    withr::with_options(list(analysiskit.server = FALSE), expect_false(ak_is_server()))
  })

  clear_signals(with_env(c(ANALYSISKIT_SERVER = "true"), expect_true(ak_is_server())))
  clear_signals(with_env(c(ANALYSISKIT_SERVER = "no"), expect_false(ak_is_server())))
})

test_that("the option beats the environment, and the environment beats detection", {
  clear_signals(
    with_env(c(SHINY_PORT = "3838", ANALYSISKIT_SERVER = "false"), {
      expect_false(ak_is_server())
      withr::with_options(list(analysiskit.server = TRUE), expect_true(ak_is_server()))
    })
  )
})

test_that("served, the destination settles itself and offers no folder picker", {
  mode <- ak_destination_mode(server = TRUE)

  expect_false(mode$pick_folder)
  expect_true(mode$settled)
  expect_match(mode$explanation, "download")
})

test_that("locally, the user picks the folder and nothing is settled for them", {
  mode <- ak_destination_mode(server = FALSE)

  expect_true(mode$pick_folder)
  expect_false(mode$settled)
  expect_match(mode$explanation, "[Cc]hoose the folder")
})


# A step with no control under it is read as a control that failed to appear.
# The served wording exists to prevent exactly that reading, so it is worth
# pinning: no heading that promises a folder, a name for what does happen, and
# a pointer to where the button will be.
test_that("served, nothing in the destination step promises a folder", {
  mode <- ak_destination_mode(server = TRUE)

  expect_false(grepl("folder", mode$label, ignore.case = TRUE))
  expect_equal(mode$step_label, "Delivery")
  expect_match(mode$explanation, "Results tab")
  # Presented as complete rather than as a neutral note about something absent.
  expect_equal(mode$status_type, "success")
})


test_that("locally, the destination step is still named after the choice", {
  mode <- ak_destination_mode(server = FALSE)

  expect_equal(mode$label, "Output folder")
  expect_equal(mode$step_label, "Destination")
  expect_equal(mode$status_type, "neutral")
})

test_that("the session output folder exists and is writable", {
  path <- ak_session_output_dir()

  expect_true(dir.exists(path))
  expect_null(ak_check_folder(path))
  # Inside the session temp directory, so it goes when the session does rather
  # than accumulating somewhere a user might mistake for storage.
  expect_true(startsWith(normalizePath(path), normalizePath(tempdir())))
})
