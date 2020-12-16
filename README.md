# Parcours de Graham
Algorithmes sous R du parcours de Graham pour le calcul de l'enveloppe convexe de points dans R^2  
Ce dépot contient plusieurs Fichier R et quelques exemples.  
**Tous les algorithmes présent prennent en entrée une matrice de taille 2*n ou n correspond au nombres de points choisit dans R^2.**

Le parcours de Graham commence par trouver un point pivot ( point de plus petite ordonnée ) et trie ensuite les autres points de manière croissante selon l'angle que fait l'axe des abscisses avec le segment joignant le pivot et le point courant.   
Il intègre alors dans une nouvelle liste A le pivot et le point suivant et va à chaque étape considérer un nouveau point courant qui sera le suivant dans la liste triée B. Pour tous les points p dans la liste B tant que le segment formé par le dernier point dans la liste A et le point courant p est à droite du segment formé par les deux dernier points de la liste A on suprime de la liste A le dernier point. En sortant de la boucle on ajoute à la liste A le point courant p et on refait le même travail avec le point suivant.   
On obtient ainsi les points de l'enveloppe convexe.

#### Les Algorithmes

L'algorithme angle_2dim permet de trouver le pivot et de calculer l'angle entre ce dernier et tous les autres points. Il est de complexité O(n).  
Les algorithmes fusion et tri_fusion sont des algorithmes de type diviser pour mieux régner et ont une complexité O(nlog(n)).  
L'algorithme labeltri produit à partir d'une matrice de points dans R^2 la matrice de points triée par le procéder expliquée plus haut. Il est de complexité O(n).  
L'algorithme dessin_ordre permet d'afficher les points choisit labélisé selon l'ordre. Il est de complexité O(n).  
L'algorithme principal parcours_graham est un algorithme dynamique qui permet de ne garder que les points appartenant au bord de l'enveloppe convexe. Il est de complexité O(nlog(n)).  
Enfin l'algorithme dessin_env permet d'afficher l'enveloppe convexe des points de R^2 sélectioner. Il est de complexité O(n).
