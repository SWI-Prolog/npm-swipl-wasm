myMax(X, Y, X) :- X >= Y, !.
myMax(X, Y, Y) :- X < Y.
