res <- brandthis::create_brand(
  "Use the name 'Iris Dataset'",
  img = "https://upload.wikimedia.org/wikipedia/commons/d/db/Iris_versicolor_4.jpg",
  type = "personal",
  chat_fn = ellmer::chat_github,
  model = "gpt-4.1",
  echo = TRUE
)

brandthis::suggest_color_scales(res)
# scale_color_paletteer_d("nord::frost", dynamic = FALSE)
# scale_color_paletteer_d("ggthemes_solarized::green", dynamic =
#                           TRUE)

# scale_color_paletteer_c("scico::berlin")
# scale_color_paletteer_c("grDevices::Purples 2")
# scale_color_paletteer_c("Redmonder::dPBIPuOr")

# scale_color_paletteer_c("ggthemes::Gold-Purple Diverging")
# scale_color_paletteer_c("RColorBrewer::PRGn")

color_palette <- brandthis::create_color_palette(res)
# $discrete1
# [1] "#6E2DAB" "#49752E" "#FFB300" "#D7263D" "#008B8B"
#
# $discrete2
# [1] "#9B79CD" "#2F4F4F" "#B0C4DE" "#DAA520" "#A52A2A"
#
# $sequential1
# [1] "#EFEFF8" "#C7BDE0" "#9B79CD" "#6E2DAB" "#522180"
#
# $sequential2
# [1] "#EBF5E7" "#B8D3A8" "#82B471" "#49752E" "#30511F"
#
# $sequential3
# [1] "#FFF8E1" "#FFDDA1" "#FFB300" "#CC8C00" "#996600"
#
# $diverging1
# [1] "#A50F15" "#D7263D" "#F49C8F" "#FDF7EC" "#A5D6A7" "#49752E"
# [7] "#284A1A"
#
# $diverging2
# [1] "#522180" "#6E2DAB" "#B6A0D9" "#FDF7EC" "#FFDDA1" "#FFB300"
# [7] "#CC8C00"
