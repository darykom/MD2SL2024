# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 17:50:51 2024

@author: dario
"""

import numpy as np
from numba import jit

import matplotlib.pyplot as plt
from matplotlib import colormaps
from matplotlib import cm
from matplotlib.colors import ListedColormap
import matplotlib.ticker as mticker

# \definecolor{FewLightGray}{RGB}{140, 140, 140}
# \definecolor{FewLightBlue}{RGB}{136, 189, 230}
# \definecolor{FewLightOrange}{RGB}{251, 178, 88}
# \definecolor{FewLightGreen}{RGB}{144, 205, 151}
# \definecolor{FewLightPink}{RGB}{246, 170, 201}
# \definecolor{FewLightBrown}{RGB}{191, 165, 84}
# \definecolor{FewLightPurple}{RGB}{188, 153, 199}
# \definecolor{FewLightYellow}{RGB}{237, 221, 70}
# \definecolor{FewLightRed}{RGB}{240, 126, 110}
# %\definecolor{FewLightGray}{RGB}{140, 140, 140}
# %
# \definecolor{FewMediumGray}{RGB}{77, 77, 77}
# \definecolor{FewMediumBlue}{RGB}{93, 165, 218}
# \definecolor{FewMediumOrange}{RGB}{250, 164, 58}
# \definecolor{FewMediumGreen}{RGB}{96, 189, 104}
# \definecolor{FewMediumPink}{RGB}{241, 124, 176}
# \definecolor{FewMediumBrown}{RGB}{178, 145, 47}
# \definecolor{FewMediumPurple}{RGB}{178, 118, 178}
# \definecolor{FewMediumYellow}{RGB}{222, 207, 63}
# \definecolor{FewMediumRed}{RGB}{241, 88, 84}
# %
# \definecolor{FewDarkGray}{RGB}{38, 38, 38}
# \definecolor{FewDarkBlue}{RGB}{38, 93, 171}
# \definecolor{FewDarkOrange}{RGB}{223, 92, 36}
# \definecolor{FewDarkGreen}{RGB}{5, 151, 72}
# \definecolor{FewDarkPink}{RGB}{229, 18, 111}
# \definecolor{FewDarkBrown}{RGB}{157, 114, 42}
# \definecolor{FewDarkPurple}{RGB}{123, 58, 150}
# \definecolor{FewDarkYellow}{RGB}{199, 180, 46}	
# \definecolor{FewDarkRed}{RGB}{203, 32, 39}




@jit
def Newton(z0,zroots,eps,res,N):
    z = z0;
    #maxn = 0
    for n in range(N):
        for kz in range(len(zroots)):
            zr = zroots[kz]
            if n==0 and abs(z-zr) <= 5*res:
                return (N-1,kz)
            if abs(z-zr) < eps:
                return (n, kz)
            # if np.abs(np.abs(z)-1) < eps:
            #     d = np.abs(z-zroots)
            #     km = np.argmin(d)
            #     return (n, km)
        z = (2*z*z*z-1)/(3*z*z) #z-z**3/(3*z**2)
        #z = (2*z**3 -z**2 +1)/(3*z**2 -2*z +1)
        #z = (4*z**3-1)/(6*z**2+1)
        #z = 1/(2*z**2+1)
    return (N,-1)
    
@jit
def Basins():
    #maxn = 0
    res = np.sqrt(dx**2+dy**2)
    split = N//3
    img = np.empty((a,b))
    for kx in range(a):
        for ky in range(b):
            z0 = x[kx] + 1j*y[ky]
            n, kz = Newton(z0,zr,eps,res,N)
            
            s = kz*split
            ofs = split*n/N
            
            # if kz==1:
            #     if maxn<n and abs(z0-zr[kz]) > 2*res:
            #         maxn=n
            #         print((n,kz, z0, ofs,split, s + ofs))
            # if n==N and abs(z0-zr[kz]) < 2*res:
            #     print((n,kz, z0, ofs,split, s + ofs))
            img[(b-1)-ky,kx] = ofs+s
            #img[(b-1)-ky,kx] = n
    return img


#p, q, fname = (-0.481762, -0.531657, 'fig006.pdf')
xmin, xmax = (-2.0, 2.0)
ymin, ymax = (-2.0, 2.0)
#xmin, xmax = ( 0.43,  1.43)
#ymin, ymax = (-1.7, -0.7)


eps = 1e-13
N = 36#36
K = 128


pal_zr3 = colormaps['Blues'] 
pal_zr2 = colormaps['Purples'] #colormaps['Greens'] #
pal_zr1 = colormaps['Oranges'] 
newcolors = np.vstack((pal_zr1(np.linspace(0, 1, K)),
                        pal_zr2(np.linspace(0, 1, K)),
                        pal_zr3(np.linspace(0, 1, K))))
palette = ListedColormap(newcolors, name='trimap')
#palette = colormaps['Blues']
#palette = colormaps['jet']

a, b = (2000, 2000)

zr = np.roots([1,0,0,+1])
#zr = np.roots([1,-1,1,-1])
#zr = np.roots([2,0,1,1])
dx, dy = ((xmax-xmin)/(a-1), (ymax-ymin)/(b-1))

x = np.linspace(xmin, xmax, a)
y = np.linspace(ymin, ymax, b)

img = Basins()


fig, ax = plt.subplots(figsize=(12, 12))

im = ax.matshow(img, cmap=palette)
#cbar = plt.colorbar(im, ax=ax)

ax.set_xticks(np.arange(0,a+1,a/4))
#ax.xaxis.set_major_formatter(mticker.FormatStrFormatter('%.2f'))
#ax.set_xticklabels(np.arange(xmin,xmax+dx,(xmax-xmin)/4))
#ax.set_xticklabels([round(label, 2) for label in ax.get_xticks()])
ax.set_xticklabels(round(label, 2) for label in np.arange(xmin,xmax+dx,(xmax-xmin)/4))


ax.set_yticks(np.arange(0,b+1,b/4))
ax.set_yticklabels(np.arange(ymax,ymin-dx,(ymin-ymax)/4))
#plt.savefig('../LaTex/Figure/NewtonBasinsDetail.pdf', bbox_inches='tight')
plt.savefig('../LaTex/Figure/NewtonBasinsCayley.pdf', bbox_inches='tight')
plt.show()

