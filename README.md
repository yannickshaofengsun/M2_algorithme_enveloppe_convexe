
# Description des Algorithmes"


## L'algorithme Naïf

## Marche de Jarvis

**La marche de Jarvis** est un algorithme qui permets de déterminer l’enveloppe convexe d’un nombre de points donnés. On appelle **enveloppe convexe** d’un ensemble fini de points, le plus petit ensemble convexe qui contient tous les points considérés. L’enveloppe convexe revient donc à un polyèdre qui a pour sommet certains ou tous les points considérés.

Nous considérons par exemple l'ensemble des points en 2D générés ci-dessous à l'aide de la loi normale et nous déterminerons donc l'ensemble convexe de ces points.

```
mu=matrix(c(1,2))
sigma=matrix(c(1,0,0,1),nrow=2,ncol=2,byrow=TRUE)
X<-rmvnorm(100,mu,sigma)
plot(X[,1],X[,2])
```

La marche de Jarvis est un algorithme itératif et récursif. Il consiste en trois étapes :

**Etape 1** : 

Premièrement on détermine le premier point X~0~ H~3~PO~4~ de l’enveloppe ou point pivot. Nous prenons le point d’abscisse minimal ou point le plus à gauche. Si plusieurs points sont d’abscisse minimal alors on prend parmi eux le point d’ordonné minimal. Cette fonction est appelée `min_X` dans notre package.

```{r}
min_X<-function(X){
  first=which.min(X[,1])
  test=X[first,]
  if (length(first)==1){first}
  else {first=which.min(test[,2])}
  return (first)
}
```
Cette fonction retourne donc l'indice du premier point de l'ensemble convexe. Pour recupérer ce point il suffit donc d'indexer l'ensemble `X` par rapport à l'indice retourné par la fonction `min_X`. 

```{r}
a=min_X(X)
a
X[a,]
```

$\texttt{Etape 2}$ : 

Ensuite, on cherche le nouveau point X~i~ avec i entre 1 et h, qui suit le précédent point de l’enveloppe convexe trouvé (h étant le nombre de points de l’enveloppe convexe). Ce point doit minimiser l’angle formé par X~i~X~i-1~ avec X~i-2~ X~i-1~. 
Pour vérifier cette condition, nul besoin de calculer l’angle formé par ces segments. Pour ce faire, il suffit juste de calculer le produit vectoriel des vecteurs définis par les points X~i~(a~i~,b~i~), X~i-1~(a~i-1~,b~i-1~) et  X~i-2~ (a~i-2~,b~i-2~). Le produit vectoriel est donné par :

$$\begin{array}
pv = (a_{i-2} - a_{i-1})( b_{i} - b_{i-1}) - (b_{i-2} - b_{i-1})( a_{i} - a_{i-1})
\end{array}$$

```{r}
produit_vectoriel<-function(A, B, C)
  {
       pv=(B[1] - A[1]) * (C[2] - A[2]) - (C[1] - A[1]) * (B[2] - A[2])
       if (pv >0) {q=1}
       else if (pv<0){q=-1}
       else {q=0}
       return (q)
  }
```

Si le produit vectoriel est supérieur à 0 alors les trois points $X_{i-2}$, $X_{i-1}$ et $X{i}$ sont en sens direct (ils forment un tournant à gauche). Si le produit vectoriel est égal à 0 alors les trois points sont alignés et si le produit vectoriel est négatif alors les trois points forment un sens indirect (ils forment un tournant à droite).
Nous retiendrons donc le point $X_i$ pour lequel le produit vectoriel des vecteurs définis par les trois points $X_i (a_i,b_i)$, $X_j (a_j,b_j)$ et $X_k (a_k,b_k)$ est strictement supérieur à 0 (avec $X_j$ l’ensemble de tous les points et $X_k$ un point autre que le point $X_i$ . Ceci permet d’éviter les points qui sont alignés. La fonction qui permets d’obtenir le point suivant est $\texttt{next_point}$.

```{r}
next_point<-function(X,i){
  if (i==1) {k=2}#on initialise par un vecteur autre que le vecteur d'indice i
  else {k=1}
  for (j in 1:length(X[,1]))
    if (produit_vectoriel(X[i,],X[j,],X[k,])>0)
    {k=j}
  return (k)
}
```

```{r}
b=next_point(X,65)
b
X[b,]
```

$\texttt{Etape 3}$ :

Enfin, il suffit de rechercher itérativement les points $X_{i}$ qui forment l’enveloppe convexe en appelant récursivement la fonction $\texttt{next_point}$ précédente de recherche du point suivant. On s’arrête lorsqu’on retombe sur le premier point $X_{0}$. Cette fonction finale est appelée $\texttt{env_conv}$.


```{r}
env_conv <- function(X){
  first=min_X(X)
  nextpt=next_point(X,first)
  env=c(first,nextpt)
  while (nextpt!=first)
    {nextpt=next_point(X,nextpt)
    env=c(env,nextpt)}
  res=matrix(nrow=length(env),ncol=2)
  for (i in 1:length(env))
  res[i,]=X[env[i],]
  return (res)}

```

On obtient donc l'ensemble convexe des points $\texttt{X}$ représentés ci-dessous.

```{r include=FALSE}
env_conv(X)
```

```{r echo=FALSE}
plot(X,pch=19,main = "Enveloppe convexe des points affichés")
lines(env_conv(X),col = "red")
```

$\texttt{Etude de la complexité}$ :

Soit n le nombre total de points considérés et h le nombre de points formant l’enveloppe convexe.
La première étape de l’algorithme recherche le point pivot ou le premier point qu’on sait appartenir à l’enveloppe convexe puisqu’il est le plus à gauche par exemple (fonction min_X). Cette étape est de complexité $O(1)$.
Après avoir trouvé le premier point dont on sait appartenir à l’enveloppe convexe de l’ensemble X nous avons conçu la fonction $\texttt{next_point}$ qui connaissant un point de l’enveloppe (A par exemple) permets de chercher parmi tous les points de X le point suivant devant appartenir à l’enveloppe. Le point suivant obtenu par cette fonction (B par exemple) doit être tel que tous les autres points soient à droite de la droite (AB). Cette fonction est de complexité $O(n)$ puisque la recherche du prochain point de l’enveloppe parcourt chacun des n points de l’ensemble X. Autrement dit, pour chaque point devant être sur l’enveloppe convexe on teste chacun des n points de l’ensemble des points pour voir lequel sera le prochain sur l’enveloppe convexe. Ce qui se justifie par la boucle for qui parcourt j entre 1 et n
La fonction finale qui détermine l’enveloppe convexe est une fonction récursive fait appel aux fonctions $\texttt{min_X}$ et $\texttt{next_point}$. Cette fonction est de complexité $O(nh)$. En effet, comme vu précédent la fonction $\textbb{next_point}$ s’exécute en $O(n)$. De plus, la boucle while tourne h fois pour chacun des points trouvés par la fonction $\texttt{next_point}$ jusqu’à retomber sur le premier point de départ. Autrement dit, la boucle while s’exécute pour chacun des points h et pour chaque itération, calcule le prochain point avec une complexité en $O(n)$. 
On obtient donc au finale une complexité de l’ordre de $O(nh)$ pour l’algorithme de Jarvis avec n le nombre total de points considérés et h le nombre de points formant l’enveloppe convexe.
Cet algorithme marchera très bien pour les ensembles dont l’enveloppe convexe contient très peu de point. Cependant, si tous les points de l’ensemble font partie de l’enveloppe convexe on aura une complexité théorique de $O(n^2)$.

##  Parcours de Graham
