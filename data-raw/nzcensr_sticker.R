# Generates the hex sticker!
library(nzcensr)
library(hexSticker)

# Create plot of New Zealand
nz <- st_geometry(dwelling_regions) %>% st_union %>% st_simplify(dTolerance = 100)

# Generate and save sticker.
sticker(expression(plot(nz, col = "white", border = "white", lwd = 0.0000000001)),
        package = "nzcensr",
        p_size=8,
        s_x=0.8, s_y=0.6, s_width=2, s_height=1.5,
        h_fill="#000000", h_color="#000000",
        filename="inst/figures/test.png")
