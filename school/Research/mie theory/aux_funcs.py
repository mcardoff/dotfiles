import numpy as np
import cmath as cm
import scipy as sp
from scipy.interpolate import interp1d
from scipy.special import spherical_jn, spherical_yn, h1vp, h2vp

#Hankel Functions and respective derivatives
def h1(n,r):
    return spherical_jn(n,r) + 1j*spherical_yn(n,r)

def h2(n,r):
    return spherical_jn(n,r)-1j*spherical_yn(n,r)

def h1p(n,r):
    return spherical_jn(n,r,1) + 1j*spherical_yn(n,r,1)

def h2p(n,r):
    return spherical_jn(n,r,1)-1j*spherical_yn(n,r,1)

#Riccati-Bessel Functions psi and xi and respective derivatives
def psi(n,r):
    return r*spherical_jn(n,r)

def xi(n,r):
    return r*h1(n,r)

def psip(n,r):
    return spherical_jn(n,r) + r * spherical_jn(n,r,1)

def xip(n,r):
    return h1(n,r) + r * h1p(n, r)

#Logarithmic Derivative
def D(n,r):
    return psi(n-1,r)/psi(n,r)-n/r


redata = np.genfromtxt('/home/mcard/school/Research/mie theory/e1.csv', delimiter=',')
imdata = np.genfromtxt('/home/mcard/school/Research/mie theory/e2.csv', delimiter=',')
# e1 = sp.interpolate.interp1d(redata[:,0],redata[:,1],kind='cubic',fill_value='extrapolate')
# e2 = sp.interpolate.interp1d(imdata[:,0],imdata[:,1],kind='cubic',fill_value='extrapolate')
# Dielectric constant of Niobium
def DielecNb(wl):
    e = 1239.841984 / wl
    e1 = -0.000469 + 0.0315*(e) - 0.89199 * (e**2) + 13.8147 * (e**3) - 126.3859 * (e**4) + 683.1397 * (e**5) - 2019.5053 * (e**6) + 2513.7193 * (e**7)
    # e2 = -0.0024531 * (e) + 0.13912 * (e**1) + -3.328 * (e**2) + 43.4499 * (e**3) + -333.5553 * (e**4) + 1501.064 * (e**5) + -3653.4889 * (e**6) + 3700.3889 * (e**7)
    e2 = 1
    return cm.sqrt(e1+e2*1j)

# Dielectric constant of Indium
def Dielec(wl):
    e = 1239.841984 / wl
    einf = 1.2
    wp = 11.6
    im = 0.2
    return cm.sqrt(einf*(1-((wp)**2)/(e**2)) + im*1j)

#Index of refraction (exp fit to expt data)
def Min(wl):
    e = 1239.841984 / wl
    return (0.169918+0.748822*np.exp(-0.7*(e-2))) + (0.729676+5.04016*np.exp(-0.4*(e-2)))*1j
    # r = -0.885065 + 0.00433653*wl - 1.3854e-6 * wl**2
    # i = 1.01808 + 0.00713627*wl - 3.50259e-7 * wl**2
    # return r + i*1j

def IndexAl(wl):
    # e = 1239.841984 / wl
    n = 1.360 + -0.0235 * (wl**1) + 1.64E-04 * (wl**2) + -5.49E-07 * (wl**3) + 9.50E-10 * (wl**4) + -7.82E-13 * (wl**5) + 2.39E-16 * (wl**6)
    k = 0.215 + -0.0171 * (wl**1) + 2.94E-04 * (wl**2) + -1.15E-06 * (wl**3) + 2.16E-09 * (wl**4) + -1.93E-12 * (wl**5) + 6.57E-16 * (wl**6)
    return n + k*1j

def Min2(wl):
    # Not used
    n = (2.425365849e-6)*(wl**2)+(-9.035088405e-5)*wl+1.4334428037e-1
    k = (-3/8022379097e-6)*(wl**2)+(1.34531048509e-2)*wl-8.3953304203e-1
    return n + 1j*k

#Scattering expansion coefficients an, bn
def acoeff(n,m,x):
    num = (D(n,m*x)/m + n/x)*psi(n,x)-psi(n-1,x)
    den = (D(n,m*x)/m + n/x)*xi(n,x)-xi(n-1,x)
    return num / den

def bcoeff(n,m,x):
    num = (m*D(n,m*x)+n/x)*psi(n,x)-psi(n-1,x)
    den = (m*D(n,m*x)+n/x)*xi(n,x)-xi(n-1,x)
    return num / den

