This is the Mathematica implementation I used for my Master's thesis. More info as well as usage examples will follow.

# Conventions
We use the convention $|\lambda|<1$ for the multipliers and the Schottky generators obey $\frac{\gamma z - P^\prime}{\gamma z - P}=\frac{1}{\lambda}\frac{z-P^\prime}{z-P}$. The generator $\gamma_i$ maps the exterior of the circle $C_i$ (with centre $c_i$, radius $r_i$) to the interior of $C_i^\prime$. The fixed point $P$ lies in $C^\prime$ and is the attractive fixed point, $P^\prime$ lies in $C$ and is the repelling one. The generators are normalized to have determinant 1.
In addition to the centres $c,c^\prime$ and radii $r,r^\prime$, the geometric data $\{{c,c'},{r,r'},\alpha,b\}$ (or just "geometric" for short) also includes an angle $\alpha$ and complex number $b$ which specify an automorphism of the unit disc. The geometric data (9 real d.o.f.) can be compressed into the three complex numbers $w_1,w_2,w_3$ (6 real d.o.f.) which uniquely determine the generator (just like $P,P^\prime,\lambda$).

# ClassicalSchottkyPackage
Construct Schottky generators, groups and cosets from geometric data, evaluate Abel's map and the period matrix in the Schottky language, and plot the Schottky cover and the Schottky theta function.

# ModularPackage
Construct random Riemann matrices and symplectic matrices, evaluate the group actions of the symplectic group $\mathrm{Sp}(2g,\mathbb{Z})$ on the Siegel upper half-space $\mathcal{H}_g$ and $\mathbb{C}^g\times\mathcal{H}_g$.