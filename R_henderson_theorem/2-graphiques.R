library(plot3D)
scatter_3D <- function(x, titre = NULL, phi = 40,
                       theta = 40){
    add <- nrow(x) >0
    if(add){
        with(x, 
             scatter3D(x = fidelity.weight,
                       y = smoothness.weight,
                       z = timeliness.weight,
                       colvar = NULL, 
                       phi = phi, theta = theta,
                       # bty = "g",
                       pch = 1,
                       # cex = 0.1, alpha = 0.4,
                       ticktype = "detailed",
                       xlim = c(0,1),
                       ylim = c(0,1),
                       zlim = c(0,1), 
                       xlab = "\n\nFidelity",
                       ylab ="\n\nSmoothness",
                       zlab = "\n\nTimeliness",
                       main = titre))
        polygon3D(x = c(0,0,1), y = c(0,1,0), z = c(1,0,0),
                  add = add, alpha = 0.2,
                  ticktype = "detailed",
                  phi = phi, theta = theta,
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }else{
        scatter3D(x = -2,
                  y = 2,
                  z = 2,
                  colvar = NULL, 
                  phi = phi, theta = theta,
                  # bty = "g",
                  pch = 1,
                  # cex = 0.1, alpha = 0.4,
                  ticktype = "detailed",
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }
}


p=0
poids_non_equiv <- readRDS(sprintf("R_henderson_theorem/data/poids_non_equiv_h%i.RDS",6))
exist_non_equiv <- sapply(poids_non_equiv, function(x){
    sapply(x, nrow)>0
}) # 3 éléments
sum(exist_non_equiv)
exist_non_equiv
par(mfrow = c(1,3), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for (p in 0:2){
    for(q in names(poids_non_equiv[[sprintf("p=%i",p)]])){
        titre <- sprintf("p = %i - %s", p, gsub("="," = ",q))
        data <- poids_non_equiv[[sprintf("p=%i",p)]][[q]]
        if(nrow(data)>0){
            scatter_3D(poids_non_equiv[[sprintf("p=%i",p)]][[q]],
                       theta = 150,
                       titre = titre)
        }
        
    }
}
poids_non_equiv <- readRDS(sprintf("R_henderson_theorem/data/poids_non_equiv_h%i.RDS",11))
exist_non_equiv <- sapply(poids_non_equiv, function(x){
    sapply(x, nrow)>0
}) 
sum(exist_non_equiv) # 15 éléments
exist_non_equiv

par(mfrow = c(3,3), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for (p in 0:1){
    for(q in names(poids_non_equiv[[sprintf("p=%i",p)]])){
        titre <- sprintf("p = %i - %s", p, q)
        data <- poids_non_equiv[[sprintf("p=%i",p)]][[q]]
        if(nrow(data)>0){
            scatter_3D(poids_non_equiv[[sprintf("p=%i",p)]][[q]],
                       theta = 150,
                       titre = titre)
        }
        
    }
}

par(mfrow = c(2,3), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for (p in 2){
    for(q in names(poids_non_equiv[[sprintf("p=%i",p)]])){
        titre <- sprintf("p = %i - %s", p, q)
        data <- poids_non_equiv[[sprintf("p=%i",p)]][[q]]
        if(nrow(data)>0){
            scatter_3D(poids_non_equiv[[sprintf("p=%i",p)]][[q]],
                       theta = 150,
                       titre = titre)
        }
        
    }
}

