M=matrix(c(0,2,-8,5,-8,0,-10,5,10,-1,2,4,1,8,0,-3,6,0,1,1) , nrow = 2) # Un exemple de 10 points dans R^2
dessin_ordre(M)                  # Affiche l'ordre de l'angle que font les différents points et le pivot avec l'axe des abscises. 
dev.off()
p=parcours_graham(M)             # Affiche l'enveloppe convexe et stocke p.
p                                # p est la matrice de points de l'enveloppe convexe.
