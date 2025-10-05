#' Plot Segmentation Indicators
#'
#' Visualizes segmentation quality indicators (e.g., MI, v, ms, GS) across different values of `n`
#' from an object produced by `compute.optmetrics()`.
#'
#' @param optimal_metrics A data frame returned by `compute.optmetrics()`, containing
#' normalized indicators with one column named `n` and additional columns for each metric
#' (e.g., `MI`, `v`, `ms`, `GS`).
#'
#' @return A `ggplot2` object displaying the trend of segmentation indicators over values of `n`.
#'
#' @details
#' This plot is helpful to visually identify the optimal number of segments or units (`n`) by comparing
#' multiple performance indicators. The goodness-of-fit metric (`GS`) is highlighted with a thicker red line,
#' while other indicators are shown in gray with different line types.
#'
#' @examples
#' \dontrun{
#' optimal_metrics <- compute.optmetrics(...)  # assuming output contains 'n', 'MI', 'v', 'ms', 'GS'
#' plot_segmentation_indicators(optimal_metrics)
#' }
#'
#' @export

plot_segmentation_indicators <- function(optimal_metrics) {

  p <- optimal_metrics %>%
    tidyr::pivot_longer(cols=c('MI','v','ms','GS'),values_to='value',names_to='variable') %>%
    mutate(variable = factor(variable,levels = c('MI','v','ms','GS'))) %>%
    ggplot2::ggplot(aes(x=n,y=value,color=variable,linetype=variable)) +
    geom_line(linewidth=1) +
    scale_color_manual(values=c('grey','grey','grey','red')) +
    scale_linetype_manual(values=c('dotted','dashed','twodash','solid')) +
    scale_size_manual(values=c(1,1,1,1.5)) +
    theme_bw(base_size=9) +
    labs(x='number of units - n',
         y='normalized indicator value',
         color='index:',
         linetype='index:') +
    theme(legend.key.width=unit(1.5,"cm"),
          legend.position='bottom')

  return(p)
}
