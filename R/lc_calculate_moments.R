#def _calculate_moments(self, x, ws, quick=False):
#    if self.discourage_overlap:
#        return self._calculate_moments_ns(x, ws, quick=quick)
#else:
#        return self._calculate_moments_syn(x, ws, quick=quick)
#'@keywords internal

lc_calculate_moments <- function(data, weights, quick = FALSE, discourage_overlap, eps, yscale){
    if(discourage_overlap == TRUE){
        moments <- lc_calculate_moments_ns(data, weights, quick, eps, yscale)
    } else {
        moments <- lc_calculate_moments_syn(data, weights, eps, yscale)
    }
    return(moments)
}
