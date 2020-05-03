library(animation)

#paramaters -------------------------------------------------------------------------------

width = 400
height = 400

numBoids = 30
visualRange = 50
falcVisualRange = 150

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


#initialFalcs function --------------------------------------------------------------------

initFalcs = function(numBoids, width, height){
	boids = setNames(data.frame(matrix(ncol = 4, nrow = numBoids)), c("x", "y", "dx", "dy"))
	for(i in 1:numBoids){
		boids$x[i] = -150
		boids$y[i] = 200
		boids$dx[i] = 20
		boids$dy[i] = -10
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
	for(j in 1:nrow(boids)){
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

	for(b in 1:nrow(boids)){
		for(c in 1:nrow(boids)){
			if(distanceBoids(boids[b,], boids[c,]) < visualRange){
				centerX = centerX + boids$x[c]
				centerY = centerY + boids$y[c]
				numNeighbours = numNeighbours + 1
				}
			}
		if(numNeighbours > 0){
			centerX = centerX / numNeighbours
			centerY = centerY / numNeighbours
		
			boids$dx[b] = boids$dx[b] + ((centerX - boids$x[b]) * centeringFactor)
			boids$dy[b] = boids$dy[b] + ((centerY - boids$y[b]) * centeringFactor)
			}
		}
	
	return(boids)
	}
	
#avoid other boids -------------------------------------------------------------------------

avoidOthers = function(boids){
	minDistance = 15
	avoidFactor = 0.05
	moveX = 0
	moveY = 0

	for(d in 1:nrow(boids)){
		for(e in 1:nrow(boids)){
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
	
for(f in 1:nrow(boids)){
	for(g in 1:nrow(boids)){
		if(distanceBoids(boids[f,], boids[g,]) < visualRange){
			avgDX = avgDX + boids$dx[g]
			avgDY = avgDY + boids$dy[g]
			numNeighbours = numNeighbours + 1
			}
		}
	if(numNeighbours > 0){
		avgDX = avgDX / numNeighbours
		avgDY = avgDY / numNeighbours

		boids$dx[f] = boids$dx[f] + ((avgDX - boids$dx[f])*matchingFactor)
		boids$dy[f] = boids$dy[f] + ((avgDY - boids$dy[f])*matchingFactor)
		}
	}
	return(boids)
	}

#speed limit ---------------------------------------------------------------------------

limitSpeed = function(boids, speedLimit){
	for(h in 1:nrow(boids)){
		speed = sqrt(abs(boids$dx[h] * (boids$dx[h] + boids$dy[h]) * boids$dy[h]))
		if(speedLimit <= speed){	
			boids$dx[h] = (boids$dx[h] / speed) * speedLimit
			boids$dy[h] = (boids$dy[h] / speed) * speedLimit
			}
		else{}
		}
	return(boids)
	}

#avoid predator -------------------------------------------------------------------------

avoidPredator = function(boids, falcs){
	minDistance = 100
	avoidFactor = 0.1
	moveX = 0
	moveY = 0

	for(d in 1:nrow(boids)){
		for(e in 1:nrow(falcs)){
			if(distanceBoids(boids[d,], falcs[e,]) < minDistance){
				moveX = moveX + boids$x[d] - falcs$x[e]
				moveY = moveY + boids$y[d] - falcs$y[e]
				}
			}
		boids$dx[d] = boids$dx[d] + (moveX * avoidFactor)
		boids$dy[d] = boids$dy[d] + (moveY * avoidFactor)
	}
	return(boids)
	}

#falc chase boids --------------------------------------------------------------------------

falcChase = function(falcs, boids, falcVisualRange){
	centeringFactor = 0.1
	centerX = 0
	centerY = 0
	numPrey = 0

	for(b in 1:nrow(falcs)){
		for(c in 1:nrow(boids)){
			if(distanceBoids(falcs[b,], boids[c,]) < falcVisualRange){
				centerX = centerX + boids$x[c]
				centerY = centerY + boids$y[c]
				numPrey = numPrey + 1
				}
			}
		if(numPrey > 0){
			centerX = centerX / numPrey
			centerY = centerY / numPrey
		
			falcs$dx[b] = falcs$dx[b] + ((centerX - falcs$x[b]) * centeringFactor)
			falcs$dy[b] = falcs$dy[b] + ((centerY - falcs$y[b]) * centeringFactor)
			}
		}
	
	return(falcs)
	}
	

#main loop function -------------------------------------------------------------------------------------

mainLoop = function(boids, falcs, visualRange, width, height){

	boids = matchVelocity(boids, visualRange)
	boids = flockToCenter(boids, visualRange)
	boids = keepWithinBounds(boids, width, height)
	boids = avoidOthers(boids)
	boids = avoidPredator(boids, falcs)
	boids = limitSpeed(boids, 15)

	for(a in 1:nrow(boids)){
		boids$x[a] = boids$x[a] + boids$dx[a]
		boids$y[a] = boids$y[a] + boids$dy[a]
		}

	return(boids)
	}

#falc loop --------------------------------------------------------------------------------

falcLoop = function(falcs, boids, falcVisualRange){
	falcs = falcChase(falcs, boids, falcVisualRange)
	falcs = limitSpeed(falcs, 15)
	for(n in 1:nrow(falcs)){
		falcs$x[n] = falcs$x[n] + falcs$dx[n]
		falcs$y[n] = falcs$y[n] + falcs$dy[n]
		}

	return(falcs)
	}

# SIM -------------------------------------------------------------------------------------
falcs = initFalcs(1, width, height)
boids = initBoids(numBoids, width, height)
ani.options(interval=0.05)

saveGIF({
    for (t in 1:200){ 
	falcs = falcLoop(falcs, boids, falcVisualRange)
	boids = mainLoop(boids, falcs, visualRange, width, height)
	print(paste("Time step", t, "done"))

	plot(boids$y ~ boids$x, xlim = c(-100,width+100), ylim = c(-100, height+100))
	points(falcs$y ~ falcs$x, col = "red", pch = 16)
	}
})
