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

plot_hyspdisp <- function(hyspdisp_out.sf,
                          metric,
                          plot.title = NULL,
                          legend.lims = NULL,
                          legend.pos = c(.20, .15),
                          legend.title = NULL,
                          longitude.lims = c(-123, -69),
                          latitude.lims = c(24, 50),
                          facility.loc = data.table( x = as.numeric(NA),
                                                     y = as.numeric(NA)),
                          legend.text.angle = 0){


  # color.option <- 'viridis'
  if (is.null( filename))
    filename <- paste('map_hyspdisp.png', sep = '')

  plot_sf_merg <- copy( hyspdisp_out.sf)
  setnames(plot_sf_merg, metric, 'metric')

  if( is.null( legend.lims))
    legend.lims <- c( 0, quantile(plot_sf_merg$metric, .95))

  colorscale <-
    scale_color_viridis(
      name = legend.title,
      discrete = F,
      option = 'magma',
      limits = legend.lims,
      oob = squish,
      direction = 1,
      na.value = NA,
      guide = guide_colorbar( title.position = 'top',
                              title.hjust = 0.5,
                              title.vjust = 0 ,
                              label.vjust = 1)
    )
  fillscale <-
    scale_fill_viridis(
      name = legend.title,
      discrete = F,
      option = 'magma',
      limits = legend.lims,
      oob = squish,
      direction = 1,
      na.value = NA,
      guide = guide_colorbar( title.position = 'top',
                              title.hjust = 0.5,
                              title.vjust = 0 ,
                              label.vjust = 1)

    )

  gg <- ggplot(data = plot_sf_merg,
               aes(fill  = metric,
                   color = metric)) +
    theme_bw() +
    labs(title = plot.title) +
    geom_sf( aes(geometry = geometry),
             size = 0.01) +
    geom_polygon(
      data = map_data("state"),
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      size = .25
    ) +
    geom_point( data = facility.loc,
                aes( x = x,
                     y = y),
                shape = 1,
                colour = "forestgreen",
                inherit.aes = F,
                size = 2
    ) +
    scale_shape_discrete(solid = T) +
    coord_sf(
      xlim = longitude.lims,
      ylim = latitude.lims,
      datum = NA
    ) +
    colorscale +
    fillscale +
    theme(
      plot.title = if( !is.null(plot.title)) {
        element_text(size = 16, hjust = 0.5)
      } else
        element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = legend.pos,
      legend.text = element_text(size = 8, angle = legend.text.angle),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.05, 'npc'),
      legend.direction = 'horizontal'
    )

  return( gg)
}
