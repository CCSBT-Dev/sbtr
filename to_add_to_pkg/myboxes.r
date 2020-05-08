#' @title myboxes
#' @export
#' 
myboxes <- function(x, width, stats, out = NULL, group, labels = rep("", length = length(x)), horizontal = F,xcol=16)
{
 Segments <- if(horizontal) function(x1, y1, x2, 
   y2)
  segments(y1, x1, y2, x2) else function(
   x1, y1, x2, y2)
  segments(x1, y1, x2, y2)
 Points <- if(horizontal) function(x, y)
  points(y, x) else function(x, y)
  points(x, y)
 Mtext <- if(horizontal) function(name, x)
  mtext(name, 2, 2, at = x, srt = 90)
   else function(name, x)
  mtext(name, 1, 2, at = x, srt = 0)
 n <- length(x)
 if(length(width) == 1) {
  del <- rep(width, 5)
  del[c(1, 5)] <- width * 0.8
  del <- c( - del, del)
  xx <- outer(del, x, "+")
 }
 else {
  del <- matrix(width, 10, n, byrow = T)
  small <- c(1, 5, 6, 10)
  del[small,  ] <- 0.8 * del[small,  ]
  del[1:5,  ] <-  - del[1:5,  ]
  xx <- rep(x, rep(10, n)) + del
 }
 if(length(out) > 0) {
  Points(x[group], out)
 }
#whiskers, ticks at max & min, UQ and LQ
 for (inx in 1:length(xx[1,]))
 polygon(xx[c(2, 7, 7, 2),inx ], stats[c(2, 2, 4, 
  4),inx ], col = xcol)
 Segments(xx[1:5,  ], stats, xx[6:10,  ], stats)
 savlwd <- par("lwd")
 par(lwd = 2)
 Segments(xx[3,  ], stats[3,  ], xx[8,  ], stats[
  3,  ])
 par(lwd = savlwd) #sides of boxes
 Segments(xx[c(2, 7),  ], stats[c(2, 2),  ], xx[
  c(2, 7),  ], stats[c(4, 4),  ])
 savlty <- par("lty")
 par(lty = 2)
 xx <- rep(x, rep(2, n))
 Segments(xx, stats[c(5, 2),  ], xx, stats[c(4, 
  1),  ])
 par(lty = savlty)
 Mtext(labels, x)
 invisible(NULL)
}
