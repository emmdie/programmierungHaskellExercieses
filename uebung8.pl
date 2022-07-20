edge(1,1).
edge(1,2).
edge(1,4).
edge(4,3).
edge(3,2).

path(W,W). 
path(U,W) :- edge(U,W).
path(U,W) :- edge(U,V), path(V,W).
