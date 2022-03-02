
test_that("winver_ver", {
  cases <- list(
    list(c("", "Microsoft Windows [Version 6.3.9600]"), "6.3.9600"),
    list("Microsoft Windows [version 6.1.7601]", "6.1.7601"),
    list("Microsoft Windows [vers\u00e3o 10.0.18362.207]", "10.0.18362.207"))

  source(system.file("tools", "winver.R", package = "ps"), local = TRUE)
  
  for (x in cases) expect_identical(winver_ver(x[[1]]), x[[2]])
})

test_that("winver_wmic", {
  cases <- list(
    list(c("\r", "\r", "Version=6.3.9600\r", "\r", "\r", "\r"),
         "6.3.9600"),
    list(c("\r", "\r", "version=6.3.9600\r", "\r", "\r", "\r"),
         "6.3.9600"),
    list(c("\r", "\r", "vers\u00e3o=6.3.9600\r", "\r", "\r", "\r"),
         "6.3.9600"))    

  source(system.file("tools", "winver.R", package = "ps"), local = TRUE)
  
  for (x in cases) expect_identical(winver_wmic(x[[1]]), x[[2]])
})
