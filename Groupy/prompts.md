## gms1
W1 = test:first(1, gms1, 1000).
W2 = test:add(2, gms1, W1, 1000).
W3 = test:add(3, gms1, W1, 1000).
W4 = test:add(4, gms1, W1, 1000).

## gms2
G1 = test:first(1, gms2, 1000).
G2 = test:add(2, gms2, G1, 1000).
G3 = test:add(3, gms2, G1, 1000).

G4 = test:add(4, gms2, G, 1000).
G5 = test:add(5, gms2, G, 1000).
G6 = test:add(6, gms2, G, 1000).

## gms3
H1 = test:first(1, gms3, 1000).
H2 = test:add(2, gms3, H1, 1000).
H3 = test:add(3, gms3, H1, 1000).

H4 = test:add(4, gms3, H, 1000).
H5 = test:add(5, gms3, H, 1000).
H6 = test:add(6, gms3, H, 1000).
test:freeze(H).
test:go(H)

## gms4
K1 = test:first(1, gms4, 1000).
K2 = test:add(2, gms4, K1, 1000).
K3 = test:add(3, gms4, K1, 1000).
K4 = test:add(3, gms4, K1, 1000).



## Once for many nodes.
M1 = test:more(4,gms1,1000).
M2 = test:more(4,gms2,1000).
M3 = test:more(6,gms3,1000).
M4 = test:more(5,gms4,1000).