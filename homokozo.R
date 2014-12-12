my_if1 <- function() {
	x <- rnorm(1)
	if(x > 0) {
		y <- "nagyobb"
		print(y)
	} else {
	y <- "nem nagyobb"
	print(y)
	}
	print(x)
}

my_if2 <- function() {
	x <- rnorm(1)
	y <- if(x > 0) {
		"nagyobb"
	} else {
		"nem nagyobb"
	}
	print(y)
	print(x)
}

my_loop1 <- function(){
	x <- c("a", "b")
	for(i in 1:2) print(x[i])
	y <- c("A", "B")
	for(i in 1:length(y)) {
		print(y[i])
	}
	z <- c("aA", "bB")
	for(i in seq_along(z)) print(z[i])
	v <- c("aB", "cD")
	for(letter in v) print(letter)
}

my_loop2 <- function() {
	m <- matrix(1:6, 2, 3)	
	for(i in seq_len(nrow(m))) for(j in seq_len(ncol(m))) print(m[i, j])
}

my_loop3 <- function(){
	m <- matrix(NA, 2, 3)
	x <- c("a", "b")
	for(i in 1:2) m[i, 1] <- x[i]
	y <- c("A", "B")
	for(i in 1:length(y)) m[i, 2] <- y[i]
	z <- c("aA", "bB")
	for(i in seq_along(z)) m[i, 3] <- z[i]
	print(m)
}

my_loop4_next <- function(x) {
  c <- NA
  for(i in 1:x) {
    if(i < x / 2) {
      next
    } else {
      c[i] <- i
    }
  }
  print(c)
}

my_while <- function(x) {
	m <- NA
	count <- 1
	while(count <= x) {
		m[count] <- count
		count <- count + 1
	}
	print(m)
	class(m)
}

my_coin <- function() {
	z <- 5
	while(z >= 4 && z <= 7) {
		print(z)
		coin <- rbinom(1, 1, 0.5)
		if(coin == 1) { ## random walk
			z <- z + 1
		} else {
			z <- z - 1
		}
	}
}

my_repeat <- function(x) {
  y <- 0
  z <- NA
  repeat {
    if(x > y){
      y <- y + 1
      z[y] <- rnorm(1)
      print(z)
    } else {
      break
    }
  }
}