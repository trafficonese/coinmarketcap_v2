
#' Plot The Price of the Largest Market Cap Cryptocurrencies
#'
#' @param currency currency code (default is 'USD')
#' @param k the number of top cryptocurrencies to plot (default is 5)
#' @param bar_color a valid color name or hexadecimal color code (default is 'grey')
#' @return A ggplot of top Cryptocurrencies based on their rank (Market Cap)
#' @examples
#' plot_top_currencies('EUR')
#' plot_top_currencies('GBP')
#' @export
plot_top_currencies <- function(currency = 'USD', k = 5, bar_color = 'grey') {

        stopifnot(currency %in% get_valid_currencies())

        k <- as.integer(k)
        if (k < 1) {
                stop("Parameter k must be a integer value greater than zero.")
        }

        temp <- data.frame(fromJSON(rawToChar(curl_fetch_memory(
                paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency))$content)))

        if (k > nrow(temp)) {
                warning(paste0("The argument provided to k is greater than the number ",
                               "of cryptocurrencies. Only ", nrow(temp),
                               " will be plotted."))
        }

        temp <- temp[seq_len(min(k, nrow(temp))), ]

        temp[,tolower(paste0('price_',currency))] <- round(as.numeric(temp[,tolower(paste0('price_',currency))]),2)
        ggplot(temp, aes_string('name',tolower(paste0('price_',currency)))) +
                geom_bar(stat = 'identity', fill = bar_color) +
                ylab(paste0('Price in ',currency)) +
                xlab('Cryptocurrencies') +
                ggtitle(paste0('Top ',k,' Cryptocurrencies with Largest Marketcaps')) +
                coord_flip()
}
