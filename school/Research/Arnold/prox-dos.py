import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import quad
import csv

#Formatting
matplotlib.rcParams['axes.linewidth']=1.5
plt.tick_params(which='both', direction='in', length=12, width=1.5, right=True, top=True, labelsize=22)
plt.tick_params(which='minor',length=5)
#plt.xticks([0.5,1.0,1.5,2.0,2.5])
#plt.minorticks_on()

#Import CSVs
datahigh = [[],[]]
with open("./high.csv") as high:
    rdr = csv.reader(high, delimiter=',')
    for row in rdr:
        datahigh[0].append(float(row[0]))
        datahigh[1].append(float(row[1]))
datalow = [[],[]]
with open("./low.csv") as low:
    rdr = csv.reader(low, delimiter=',')
    for row in rdr:
        datalow[0].append(float(row[0]))
        datalow[1].append(float(row[1]))

#Fit parameters
DeltaS = 1.38
DeltaN = 0.001#DeltaS / 1.2
dl=0.001
Rhigh=0.45#0.45#0.085
Rmid=0.09
Rlow=0.0045#0.025
k = 0.08617
T = 1.5

#Define Functions used in Proximity Model DOS
def Omega(e,Delta):
    return np.sqrt(e**2-Delta**2+0j)

def F(e):
    return (e**2 - DeltaS*DeltaN)/(Omega(e,DeltaN)*Omega(e,DeltaS))

def G(e):
    return e*(DeltaS-DeltaN)/(Omega(e,DeltaN)*Omega(e,DeltaS))

def DKd(e,dl,R):
    return R*Omega(e,DeltaN)+dl*1j

def sigma(e,dl,R):
    return np.imag( ((e/Omega(e,DeltaN))*( F(e)*np.cos(DKd(e,dl,R))*1j + np.sin(DKd(e,dl,R)) )+1j*G(e)*DeltaN/Omega(e,DeltaN))/(np.cos(DKd(e,dl,R)) - F(e)*np.sin(DKd(e,dl,R))*1j) )

#BCS DOS
def N(e,Delta):
    return e/np.sqrt(e**2-Delta**2+0j)

#Fermi-Dirac Distribution
def f(e):
    return 1/(np.exp(-e/(k*T))+1)

def fp(e):
    return -1*(1/(k*T))*np.exp(-e/(k*T))/((1+np.exp(-e/(k*T)))**2)

def intgnd(e,v,dl,R):
    return np.abs(sigma(e,dl,R))*(-fp(e-v))

def bcsintgnd(e,v,Delta):
    return np.abs(N(e,Delta))*(-fp(e-v))

xmin = 0.1
xmax = 3.5
vr = np.linspace(xmin,xmax,400)#1200

ixmin=1.1
ixmax=1.5
iymin=0
iymax=6.0

ix=.53
iy=.46
iw=.34
ih=.34

plt.figure(1)
plt.plot(vr,[N(v,DeltaS) for v in vr],label="BCS",color="blue",linewidth=2)
plt.plot(vr,[sigma(v,dl,Rhigh) for v in vr],label="Arnold, R=0.09 meV$^{-1}$",color="black",linewidth=2)
plt.ylim([0,16])
plt.xlim([0,3])
plt.xlabel("E (meV)",fontsize=20)
plt.ylabel("Density of States",fontsize=20)
plt.tick_params(axis='y',pad=10)
plt.tick_params(axis='x',pad=10)
#plt.plot([ixmin,ixmax,ixmax,ixmin,ixmin],[iymin,iymin,iymax,iymax,iymin],color='red')
#plt.plot([ixmin,1.565],[iymax,62.5],color='red')
#plt.plot([ixmax,2.884],[iymin,31.6],color='red')
plt.legend(loc='upper left',fontsize=18)
#a = plt.axes([ix,iy,iw,ih],facecolor='#ffffff')
#a.spines['bottom'].set_color('red')
#a.spines['top'].set_color('red')
#a.spines['left'].set_color('red')
#a.spines['right'].set_color('red')
#a.set_xlim([ixmin,ixmax])
#a.set_ylim([iymin,iymax])
#a.tick_params(which='both', top=False, bottom=False, labelbottom=False, labelleft=False, left=False)#, direction='in', length=6, width=1.5, right=True, top=True, labelsize=20,pad=8)
#a.plot(vr,[N(v,DeltaS) for v in vr],label="BCS",color="blue",linewidth=2)
#a.plot(vr,[sigma(v,dl,Rhigh) for v in vr],label="Arnold Model",color="black",linewidth=2)
plt.show()

didvr = [[],[],[],[]]
cutoff=5
for v in vr:
    didvr[0].append(quad(intgnd,v-cutoff,v+cutoff,args=(v,dl,Rhigh),limit=70)[0])
    didvr[1].append(quad(intgnd,v-cutoff,v+cutoff,args=(v,dl,Rlow),limit=70)[0])
    didvr[3].append(quad(intgnd,v-cutoff,v+cutoff,args=(v,dl,Rmid),limit=70)[0])
    didvr[2].append(quad(bcsintgnd,DeltaS,v+cutoff,args=(v,DeltaS),limit=70)[0])


plt.figure(2)
plt.axis([xmin,xmax,0,2.3])
plt.xlabel("Voltage (mV)",fontsize=28)
plt.ylabel("Normalized Conductance (A.U.)",fontsize=28)
plt.plot(vr,didvr[0],color="black",label="Arnold Model, R=0.450 meV$^{-1}$",linewidth=2)
plt.plot(vr,didvr[3],color="green",label="Arnold Model, R=0.090 mev$^{-1}$",linewidth=2)
plt.plot(vr,didvr[1],color="red",label="Arnold Model, R=0.0045 mev$^{-1}$",linewidth=2)
plt.plot(vr,didvr[2],color="blue",label="BCS",linestyle="--",linewidth=2)
plt.legend(fontsize=18)
#plt.scatter(datahigh[0],datahigh[1],color='black',s=70,facecolors='red')
#plt.scatter(datalow[0],datalow[1],color='black',s=70,facecolors='blue')
#plt.text(0.62,1.42,"$\Delta_{S}$="+str(DeltaS)+" meV\n$\Delta_{N}$="+str(DeltaN)+" meV\n$\\frac{d}{l}$="+str(dl)+"\n$R_{high}$="+str(Rhigh)+"\n$R_{low}$="+str(Rlow),fontsize=26,linespacing=1.6)
plt.tick_params(axis='y',pad=10)
plt.tick_params(axis='x',pad=10)

#a = plt.axes([.53,.16,.34,.34],facecolor='#ededed')
#a.set_xticks([0.5,1.5,2.5])
#a.set_yticks([0.0,1.0,2.0])
#a.set_xlim([0.5,2.5])
#a.set_ylim([0,2.3])
plt.tick_params(which='both', direction='in', length=12, width=1.5, right=True, top=True, labelsize=20,pad=8)
#plt.scatter(datahigh[0][22:100],datahigh[1][22:100],color='black',s=50,facecolors='red')
#plt.plot(vr,didvr[2],color="black",linewidth=3)

fig = plt.gcf()
fig.set_size_inches((10,10))
#fig.savefig('./BCS-comparison.pdf',dpi=300)
plt.show()
