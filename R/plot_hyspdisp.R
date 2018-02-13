#' Count particles in zip codes
#'
#' \code{plot_hyspdisp}  takes as input particle-zip code links plots on a map.
#'
#' @param hyspdisp_out data table of Zip codes and particle numbers:
#' \enumerate{
#'   \item ZIP (zip code numbers)
#'   \item N (number of particles)
#' }
#' @param zc zip code shapefile location
#' @param cw ZIP - ZCTA crosswalk file
#' @return This function returns a data table of zip codes with associated number of particles.

plot_hyspdisp <- function(hyspdisp_out,
                      zc,
                      cw,
                      outpath = getwd(),
                      plot.title = NULL,
                      metric = 'N',
                      legend_lims = c(0, 1e5),
                      show.legend = NULL,
                      filename = NULL,
                      facility.loc = NULL,
                      legend.text.angle = 0){

  zips <- st_read(zc)
  setnames( zips, 'ZCTA5CE10', 'ZCTA')
  cw$ZCTA <- formatC( cw$ZCTA,
                      width = 5,
                      format = "d",
                      flag = "0") # to merge on zcta ID
  zips <- merge( zips, cw, by = "ZCTA", all = F, allow.cartesian = TRUE) # all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  zips$ZIP <- formatC( zips$ZIP,
                       width = 5,
                       format = "d",
                       flag = "0") # to merge on zcta ID

  zip_dataset_sf <- data.table( merge( zips, hyspdisp_out, by = c('ZIP'), all.y = T))


  # color.option <- 'viridis'
  if (is.null( plot.title))
    plot.title <- paste('HYSPdisp')
  if (is.null( filename))
    filename <- paste('map_hyspdisp.png', sep = '')

  plot_sf_merg <- zip_dataset_sf
  setnames(plot_sf_merg, metric, 'metric')

  colorscale <-
    scale_color_viridis(
      discrete = F,
      option = 'magma',
      limits = legend_lims,
      oob = squish,
      direction = 1,
      na.value = "white"
    )
  fillscale <-
    scale_fill_viridis(
      discrete = F,
      option = 'magma',
      limits = legend_lims,
      oob = squish,
      direction = 1,
      na.value="white"
    )


  gg <- ggplot(data = plot_sf_merg,
               aes(fill  = metric,
                   color = metric)) +
    theme_bw() +
    labs(title = plot.title) +
    geom_sf(size = 0.01) +
    geom_polygon(
      data = map_data("state"),
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      size = .25
    ) +
    geom_point(
      x = facility.loc[1],
      y = facility.loc[2],
      shape = 1,
      colour = "forestgreen",
      # fill = "darkred",
      size = .75
    ) +
    scale_shape_discrete(solid = T) +
    coord_sf(
      xlim = c(-123, -69),
      ylim = c(24, 50),
      datum = NA
    ) +
    colorscale +
    fillscale +
    theme(
      plot.title = element_text(size = 14), #element_text(size = 16, hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = if( !is.null(show.legend)) {
        show.legend
      } else
        c(.20, .15),
      legend.text = element_text(size = 8, angle = legend.text.angle),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.05, 'npc'),
      legend.direction = 'horizontal'
    )

  invisible(ggsave(
    file.path(outpath, filename),
    gg,
    width = 13.5,
    height = 7.79,
    unit = 'cm'
  ))
}
