require(tmap) # install tmap before ggplot2 for plotting fns to work
require(ggplot2)

tmap_options(unit = "m")

makeMap <- function(shapefile, fill.vars, fill.palette = "Reds", ncol = 1,
                    show.text = F, text = "Name", save.map = F,
                    map.width = 14.75, map.height = 8.47,
                    save.name = "output/plots/garden_map.pdf") {
  m <- qtm(
    shp = shapefile, fill = fill.vars,
    fill.palette = fill.palette, ncol = ncol
  ) +
    tm_compass() +
    tm_layout(
      legend.title.size = 1.8,
      legend.text.size = 1,
      legend.position = c("left", "top")
    ) +
    tm_scale_bar()

  if (show.text == T) {
    m <- m + tm_text(text)
  }

  if (save.map == T) {
    tmap_save(
      tm = m, filename = save.name, width = map.width,
      height = map.height, units = "in"
    )
  }
}

makeMapLoop <- function(shapefile, var.list, var.group, fill.palette = "Reds",
                        show.text = F, text = "Name", save.map = F,
                        map.width = 14.75, map.height = 8.47) {
  for (v in var.list) {
    plot.title <- paste0("output/plots/", var.group, "_", v, ".pdf")
    makeMap(
      shapefile = shapefile, fill.vars = c(v),
      fill.palette = fill.palette, ncol = 1, show.text = show.text,
      text = text, save.map = save.map, map.width = map.width,
      map.height = map.height, save.name = plot.title
    )
  }
}

makeStackedBarplot <- function(plot.data,
                               scale_values = c(
                                 "red4", "tomato2",
                                 "lightcoral", "pink"
                               ),
                               fill.var, x.var, y.var,
                               plot.title, x.axis, y.axis,
                               text.angle = 0, save.plot = F,
                               plot.width = 14.75, plot.height = 8.47,
                               save.name = "output/plots/garden_barplot.pdf") {
  ggplot(plot.data, aes_string(fill = fill.var, y = y.var, x = x.var)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = scale_values) +
    labs(title = plot.title) +
    xlab(x.axis) +
    ylab(y.axis) +
    theme(axis.text.x = element_text(angle = text.angle, hjust = 1))

  if (save.plot == T) {
    ggsave(save.name,
      width = plot.width, height = plot.height,
      units = "in"
    )
  }
}
