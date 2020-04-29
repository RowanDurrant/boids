#paramaters -------------------------------------------------------------------------------

width = 150
height = 150

numBoids = 100
visualRange = 75

#initialBoids function --------------------------------------------------------------------

initBoids = function(numBoids, width, height){
	boids = setNames(data.frame(matrix(ncol = 4, nrow = numBoids)), c("x", "y", "dx", "dy"))
	for(i in 1:numBoids){
		boids$x[i] = sample(width, 1)
		boids$y[i] = sample(height, 1)
		boids$dx[i] = sample(5, 1)
		boids$dy[i] = sample(5, 1)
		}
	return(boids)
	}
		
#distance between boids -------------------------------------------------------------------

distanceBoids = function(boid1, boid2){
	distBoids = sqrt((boid1$x - boid2$x) * (boid1$x - boid2$x) + (boid1$y - boid2$y) * (boid1$y - boid2$y))
	return(distBoids)
	}

#Keep within boundaries--------------------------------------------------------------------

keepWithinBounds = function(boids, width, height){
	turnFactor = 1
	for(j in nrow(boids)){
		if(boids$x[j] < width){ boids$dx[j] = boids$dx[j] + turnFactor }
		if(boids$x[j] > 0){boids$dx[j] = boids$dx[j] - turnFactor }
		if(boids$y[j] < height){ boids$dy[j] = boids$dy[j] + turnFactor }
		if(boids$y[j] > 0){boids$dy[j] = boids$dy[j] - turnFactor }
		}
	return(boids)
	}


#flock to center --------------------------------------------------------------------------

flockToCenter = function(boids, visualRange){
	centeringFactor = 0.005
	centerX = 0
	centerY = 0
	numNeighbours = 0

	for(b in boids){
		for(c in boids){
			if(distanceBoids(boids[b,], boids[c,]) < visualRange){
				centerX = centerX + boids$x[c]
				centerY = centerY + boids$y[c]
				numNeighbours = numNeighbours + 1
				}
			}
		if(numNeigbours > 0){
			centerX = centerX / numNeighbours
			centerY = centerY / numNeighbours
		
			boid$dx[b] = boid$dx[b] + ((centerX - boid$x[b]) * centeringFactor)
			boid$dy[b] = boid$dy[b] + ((centerY - boid$y[b]) * centeringFactor)
			}
		}
	}
	return(boids)
	}
	
#avoid other boids -------------------------------------------------------------------------

avoidOthers = function(boids){
	minDistance = 20
	avoidFactor = 0.05
	moveX = 0
	moveY = 0

	for(d in boids){
		for(e in boids){
			if(boids[e,] != boids[d,]){
				if(distanceBoids(boids[d,], boids[e,]) < minDistance){
					moveX = moveX + boids$x[d] - boids$x[e]
					moveY = moveY + boids$y[d] - boids$y[e]
					}
				}
			}
		boids$dx[d] = boids$dx[d] + (moveX * avoidFactor)
		boids$dy[d] = boids$dy[d] + (moveY * avoidFactor)
	}
	return(boids)
	}
#match velocity --------------------------------------------------------------------------

matchVelocity = function(boids, visualRange){
	matchingFactor = 0.05
	avgDX = 0
	avgDY = 0
	numNeighbours = 0

	for(otherBoid in Boids){
		if(distanceBoids(boid, otherBoid) < visualRange){
			avgDX = avgDx + otherBoid$dx
			avgDY = avgDY + otherBoid$dy
			numNeighbours = numNeighbours + 1
			}
		}
	if(numNeighbours > 0){
		avgDX = avgDX / numNeighbours
		avgDY = avgDY / numNeighbours

		boid$dx = boid$dx + ((avgDX - boid$dx)*matchingFactor)
		boid$dy = boid$dy + ((avgDY - boid$dy)*matchingFactor)
		}
	return(boids)
	}

#speed limit ---------------------------------------------------------------------------

limitSpeed = function(boid){
	speedLimit = 15
	speed = sqrt(boid$dx * (boid$dx + boid$dy) * boid$dy)
	if(speed > speedLimit){	
		boid$dx = (boid$dx / speed) * speedLimit
		boid$dy = (boid$dy / speed) * speedLimit
		}
	return(boids)
	}

#main loop function -------------------------------------------------------------------------------------

mainLoop = function(boids){
	boids = flyTowardsCenter(boids)
	boids = avoidOthers(boids)
	boids = matchVelocity(boids)
	boids = limitSpeed(boids)
	boids = keepWithinBounds(boids)

	for(a in nrow(boids)){
		boids$x[a] = boids$x[a] + boids$dx[a]
		boids$y[a] = boids$y[a] + boids$dy[a]
		}
	return(boids)
	}


# SIM -------------------------------------------------------------------------------------

boids = initBoids(numBoids, width, height)

for(t in 1:100){
	mainLoop(boids)
	plot(boids$y ~ boids$x)
	}