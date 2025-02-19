.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Data: (c) OpenStreetMap contributors, ODbL 1.0 - http://www.openstreetmap.org/copyright")
  packageStartupMessage("Routing: OSRM - http://project-osrm.org/")
  packageStartupMessage(paste0("sp support will be dropped in the next major release, ",
                               "please use sf objects instead."))
  
  # options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")
}

.onLoad <- function(libname, pkgname) {
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
  # options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")
}