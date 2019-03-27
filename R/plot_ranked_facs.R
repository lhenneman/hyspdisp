plot_ranked_facs <- function( ranks.dt,
                              size.var,
                              size.name,
                              size.legend.range = NULL,
                              plot.title = NULL,
                              xlims = NULL,
                              ylims = NULL,
                              dist.scalebar = 400){

  # -- limit data table to units under the rank -- #
  ranks.dt.trim <- copy( ranks.dt)

  # -- set name of variable size variable -- #
  setnames( ranks.dt.trim, size.var, 'size.var')

  # -- link with PP data if not already  -- #
  if( !( 'Longitude' %in% names( ranks.dt.trim) & 'Latitude' %in% names( ranks.dt.trim)))
    stop( "Latitude and Longitude must be included in ranks.dt")

  # -- find lat/lon range  -- #
  if( is.null( xlims) & is.null( ylims)){
    latlonrange <- data.table( xlim = c( min( ranks.dt.trim$Longitude) - .1,
                                         max( ranks.dt.trim$Longitude) + .1),
                               ylim = c( min( ranks.dt.trim$Latitude - .5),
                                         max( ranks.dt.trim$Latitude + .1)))
  } else
    latlonrange <- data.table( xlim = xlims,
                               ylim = ylims)

  # -- find size legend range  -- #
  if( is.null( size.legend.range) ){
    size.legend.range <- c( 0, signif( max( ranks.dt.trim$size.var), 2))
  }


  # -- download states  -- #
  states <- data.table( map_data("state"))

  # -- make the plot  -- #
  gg_coal <- ggplot() +
    theme_bw() +
    labs(title = plot.title) +
    geom_polygon(data = states,
                 aes(x = long, y = lat, group = group),
                 fill = 'white',
                 color = "black",
                 size = .25) +
    coord_sf(
      xlim = latlonrange$xlim,
      ylim = latlonrange$ylim,
      datum = NA
    ) +
    geom_point(data = ranks.dt.trim,
               aes(x = Longitude,
                   y = Latitude,
                   size = size.var),
               color = '#479ddd') +
    scale_size_area(guide = guide_legend(title.position = "top"),
                          name = size.name,
                          max_size = 5,
                          limits = size.legend.range,
                          oob = squish
    ) +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5), #element_blank(), #
      axis.title = element_text(size = 24),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_text(size = 10),
      legend.title.align = 0.5,
      legend.position = "bottom", #c(.22, .15),
      legend.text = element_text(size = 8, angle = 0),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.05, 'npc'),
      legend.direction = 'horizontal',
      # rect = element_blank(), #( fill = 'transparent'),
      strip.text = element_text( size = 14),
      strip.background = element_rect( fill = 'white')
    )  +
    geom_rect( data = latlonrange,
               aes(xmin = xlim[1] - 5,
                   xmax = xlim[1] + (xlim[2] - xlim[1]) / 2,
                   ymin = ylim[1] - 5,
                   ymax = ylim[1] + .5),
               fill = 'white',
               color = NA) +
    ggsn::scalebar( location = 'bottomleft',
                    anchor = c( x = latlonrange$xlim[1] + .2, y = latlonrange$ylim[1] + .2),
                    x.min = latlonrange$xlim[1],
                    y.min = latlonrange$ylim[1],
                    x.max = latlonrange$xlim[2],
                    y.max = latlonrange$ylim[2],
                    dist = dist.scalebar / 2,
                    height = 0.02,
                    st.dist = 0.04,
                    st.size = 3,
                    dd2km = TRUE,
                    model = 'WGS84')

  print( gg_coal)
  return( list( plot = gg_coal,
                latlonrange = copy( latlonrange)))

}
