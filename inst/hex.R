library(hexSticker)
library(tidyverse)
library(sf)
library(showtext)

font_add_google("Open Sans", "opensans")

devtools::load_all()

bnd <- PATHtoolsZambia::retrieve("province-shp") %>%
  rmapshaper::ms_dissolve() %>%
  st_geometry()

(p <- ggplot() +
    geom_sf(data = bnd, fill = "#464F60", color = "black", size = 0.1) +
    theme_void() +
    theme_transparent())

s <- sticker(p, package = "catchment",
             p_size = 14, p_fontface = "bold",
             h_fill = "#F65050",
             h_color = "#A8001E",
             p_family = "opensans",
             s_x=1, s_y=.75, s_width=1.3, s_height=1,
             filename = "inst/images/hexlogo.png")
plot(s)

library(qrcode)
code <- qr_code("https://github.com/PATH-Global-Health/catchment")
print(code)
plot(code)
generate_svg(code, filename = "inst/images/qr.svg")

imgurl <- system.file("inst/images/qr.svg")


s <- sticker("inst/images/qr.svg", package = "catchment",
             p_size = 14, p_fontface = "bold", p_y = 1.5,
             h_fill = "#F65050",
             h_color = "#A8001E",
             p_family = "opensans",
             s_x=1, s_y=0.85, s_width=0.5, s_height=0.5,
             filename = "inst/images/hexlogo.png")
plot(s)
