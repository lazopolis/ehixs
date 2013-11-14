C
C     Here we collect squared amplitudes for higgs decays
C

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C     Some Helper functions

c     Squared breit-wigner:
      doubleprecision function BWsq(S,m,Gamma)
      implicit none
      doubleprecision S,m,Gamma      
      BWsq=1d0/((S-m**2)**2+(m*Gamma)**2)      
      end function

c     interference of two products of breitwigners:
      doubleprecision function BWprodint
     -  (s1,s2,s3,s4,M1,M2,M3,M4,G1,G2,G3,G4)
      implicit none
      doubleprecision s1,s2,s3,s4,M1,M2,M3,M4,G1,G2,G3,G4 
      doubleprecision A1,A2,A3,A4,B1,B2,B3,B4
      A1=S1-M1**2
      A2=S2-M2**2
      A3=S3-M3**2
      A4=S4-M4**2      
      B1=M1*G1
      B2=M2*G2
      B3=M3*G3
      B4=M4*G4
      BWprodint=
     - (A1*A2*A3*A4-A1*A2*B3*B4+A1*B2*A3*B4+A1*B2*B3*A4
     - +B1*A2*A3*B4+B1*A2*B3*A4-B1*B2*A3*A4+B1*B2*B3*B4)      
     - /((B4**2+A4**2)*(B3**2+A3**2)*(B2**2+A2**2)*(B1**2+A1**2))
      end function



C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> W W  -> l n l n 

      doubleprecision function HWWlnln(GF,mW,GammaW,mZ,GammaZ,
     -     s12,s13,s14,s23,s24,s34)

      implicit none
C     Input:
      doubleprecision GF,mW,GammaW,mZ,GammaZ,s12,s13,s14,s23,s24,s34
C     Internal variables:
      doubleprecision pref,form,gw
C     Functions
      doubleprecision BWsq
c     the weak coupling constant:
      gw=dsqrt(8d0*mw**2*GF/sqrt(2d0))
c     coupling (W to l n)^4 :
      pref=gw**4/(2d0**(6))
c     coupling (HWW)^2
      pref=pref*(gw*Mw)**2
c     form factor:
      form=64d0*s13*s24*BWsq(s12,MW,GammaW)*BWsq(s34,Mw,GammaW)
c     squared amplitude
      HWWlnln=pref*form
      end function

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> Z Z  -> l n l n 

      doubleprecision function HZZlnln(GF,mW,GammaW,mZ,GammaZ,
     -     s12,s13,s14,s23,s24,s34)

      implicit none
C     Input:
      doubleprecision GF,mW,GammaW,mZ,GammaZ,s12,s13,s14,s23,s24,s34
C     Internal variables:
      doubleprecision pref,form,gw,V1,V2,A1,A2,cosw
C     Functions
      doubleprecision BWsq

c     the weak coupling constant:
      gw=sqrt(8d0*mw**2*GF/sqrt(2d0))
      cosw=Mw/Mz

c     Vector and Axial Couplings:
c     electron-Z
      V1=gw/cosw*(3d0/4d0-cosw**2)
      A1=gw/cosw*(-1d0/4d0)
c     neutrino-Z
      V2=gw/cosw*(1d0/4d0)
      A2=gw/cosw*(1d0/4d0)

c     coupling (HZZ)^2
      pref=(gw*Mz/cosw)**2
c     form factor:
      form=8*BWsq(s14,Mz,Gammaz)*BWsq(s23,Mz,Gammaz)*(
     -s24*s13*
     -((A1*A2)**2+(A1*V2)**2+(V1*A2)**2+(V1*V2)**2+4*A1*A2*V1*V2)
     -+s12*s34*
     -((A1*A2)**2+(A1*V2)**2+(V1*A2)**2+(V1*V2)**2-4*A1*A2*V1*V2)
     -)

c     squared amplitude
      HZZlnln=pref*form
      end function



C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> W W / Z Z  -> l n l n 

      doubleprecision function HWWZZlnln(GF,mW,GammaW,mZ,GammaZ,
     -     s12,s13,s14,s23,s24,s34,topology)

      implicit none
C     Input:
      doubleprecision GF,mW,GammaW,mZ,GammaZ,s12,s13,s14,s23,s24,s34
      character*20 topology 
C     Internal variables:
      doubleprecision pref,form,gw,V1,V2,A1,A2,cosw,D1234,D2314
C     Functions
      doubleprecision BWsq,BWprodint,HWWlnln,HZZlnln

c     the weak coupling constant:
      gw=sqrt(8d0*mw**2*GF/sqrt(2d0))
      cosw=Mw/Mz

c     Vector and Axial Couplings:
c     electron-Z
      V1=gw/cosw*(3d0/4d0-cosw**2)
      A1=gw/cosw*(-1d0/4d0)
c     neutrino-Z
      V2=gw/cosw*(1d0/4d0)
      A2=gw/cosw*(1d0/4d0)

c     Breit-Wigners:
      D1234=BWsq(s12,MW,GammaW)*BWsq(s34,Mw,GammaW)
      D2314=BWsq(s14,Mz,Gammaz)*BWsq(s23,Mz,Gammaz)

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
c    H-WW/ZZ Interference->lnln

c     coupling (HZZ)*(HWW)
      pref=(gw*Mz/cosw)*(gw*Mw)
      pref=pref*gw**2/2**(3)
c     form factor:
      form=32d0*BWprodint(s23,s14,s12,s34,Mz,Mz,Mw,Mw,
     - Gammaz,Gammaz,Gammaw,Gammaw)
      form=form*(A1+V1)*(A2+V2)*s24*s13
c     squared amplitude
      HWWZZlnln=pref*form

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
c    H-ZZ->lnln

c     coupling (HZZ)^2
      pref=(gw*Mz/cosw)**2
c     form factor:
      form=8*D2314*(
     -s24*s13*
     -((A1*A2)**2+(A1*V2)**2+(V1*A2)**2+(V1*V2)**2+4*A1*A2*V1*V2)
     -+s12*s34*
     -((A1*A2)**2+(A1*V2)**2+(V1*A2)**2+(V1*V2)**2-4*A1*A2*V1*V2)
     -)
c     squared amplitude (factor 3 because can't experimentally see neutrinos!)
      HWWZZlnln=HWWZZlnln+3d0*pref*form

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
c    H->WW->lnln

c     coupling (W to l n)^4 :
      pref=gw**4/(2d0**(6))
c     coupling (HWW)^2
      pref=pref*(gw*Mw)**2
c     form factor:
      form=64d0*s13*s24*D1234
c     squared amplitude
      HWWZZlnln=HWWZZlnln+pref*form

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

c     peak brancher:
      if(topology.eq.'D1234')then 
         HWWZZlnln=HWWZZlnln*D1234/(D1234+D2314)          
      else if(topology.eq.'D2314')then
         HWWZZlnln=HWWZZlnln*D2314/(D1234+D2314)          
      else if(topology.eq.'all') then         
c     do nothing 
      else
         write(6,*)"not defined!!!!!!"
      end if
         
      end function





C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> Z Z  -> l l l l 

      doubleprecision function HZZllll(GF,mW,GammaW,mZ,GammaZ,
     -     s12,s13,s14,s23,s24,s34,topology)

      implicit none
C     Input:
      doubleprecision GF,mW,GammaW,mZ,GammaZ,s12,s13,s14,s23,s24,s34
      character*20 topology
C     Internal variables:
      doubleprecision pref,form,gw,V,A,cosw,D1234,D2314
C     Functions
      doubleprecision BWsq,BWprodint

c     the weak coupling constant:
      gw=sqrt(8d0*mw**2*GF/sqrt(2d0))
      cosw=Mw/Mz

c     Vector and Axial Couplings:
c     electron-Z
      V=gw/cosw*(3d0/4d0-cosw**2)
      A=gw/cosw*(-1d0/4d0)

c     coupling (HZZ)^2
      pref=(gw*Mz/cosw)**2

C     branch into topologies:
      D1234=BWsq(s12,Mz,Gammaz)*BWsq(s34,Mz,Gammaz)
      D2314=BWsq(s23,Mz,Gammaz)*BWsq(s14,Mz,Gammaz)

      form=(8*S13*(V**4+A**4+6*V**2*A**2)*S24
     -     +8*S14*S23*(A-V)**2*(A+V)**2)
     -     *D1234    
      form=form+(8*S13*(V**4+A**4+6*V**2*A**2)*S24
     -     +8*S12*S34*(A-V)**2*(A+V)**2)
     -     *D2314            
      form=form+16*S13*(V**4+A**4+6*V**2*A**2)*S24
     -     *BWprodint(s23,s14,s12,s34,Mz,Mz,Mz,Mz,
     -     Gammaz,Gammaz,Gammaz,Gammaz)

      if(topology.eq.'all')then  !nothing to be done
      else if(topology.eq.'D1234')then
         form=form*D1234/(D1234+D2314) 
      else if(topology.eq.'D2314')then
         form=form*D2314/(D1234+D2314) 
      else 
         write(6,*)" HZZllll topology not defined",topology
      end if

c     squared amplitude
      HZZllll=pref*form

      end function





C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> Z Z  -> e e mu mu  

      doubleprecision function HZZeemm(GF,mW,GammaW,mZ,GammaZ,
     -     s12,s13,s14,s23,s24,s34)

      implicit none
C     Input:
      doubleprecision GF,mW,GammaW,mZ,GammaZ,s12,s13,s14,s23,s24,s34
      character*10 topology
C     Internal variables:
      doubleprecision pref,form,gw,V,A,cosw
C     Functions
      doubleprecision BWsq,BWprodint

c     the weak coupling constant:
      gw=sqrt(8d0*mw**2*GF/sqrt(2d0))
      cosw=Mw/Mz

c     Vector and Axial Couplings:
c     electron/muon-Z
      V=gw/cosw*(3d0/4d0-cosw**2)
      A=gw/cosw*(-1d0/4d0)

c     coupling (HZZ)^2
      pref=(gw*Mz/cosw)**2

      form=(8*S13*(V**4+A**4+6*V**2*A**2)*S24
     -     +8*S14*S23*(A-V)**2*(A+V)**2)
     -     *BWsq(s12,Mz,Gammaz)*BWsq(s34,Mz,Gammaz)    

c     squared amplitude
      HZZeemm=pref*form

      end function






C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> photon(1) Z(2)  

      doubleprecision function HphotonZ(GF,mW,GammaW,mZ,GammaZ,alpha,
     -     MH,MT,QZ)                                               !Input (QZ is the offshell mass of the Z!!)
      implicit none
      doubleprecision GF,mW,GammaW,mZ,GammaZ,MH,MT,QZ        
      doubleprecision gw,cw,sw,Qt,It,Nc,alpha             !internal
      doublecomplex FW,FT,c0_aux,c2_aux
      doubleprecision prefW,prefT,pi
      parameter(pi=3.141592653589793d0)

c     couplings.. 
      gw=sqrt(8d0*mw**2*GF/sqrt(2d0))
      cw=Mw/Mz
      sw=sqrt(1d0-cw**2)
      Nc=3d0

C     top charge and isospin:
      Qt=2d0/3d0
      It=1d0/2d0

c     prefactors:
      prefW=cw/sw/4d0
      prefT=Qt*Nc*(It-2d0*sw**2*Qt)/(sw*cw)/2d0

c     Compute form factors:
      FT=C0_aux(MH,QZ,MT)+4d0*C2_aux(MH,QZ,MT)
      FW=2d0*C2_aux(MH,QZ,MW)
      FW=FW*((MH/MW)**2*(1d0-2d0*(MW/QZ)**2)+2d0*(1d0-6d0*(MW/QZ)**2))
      FW=FW+4d0*(1d0-4d0*(MW/QZ)**2)*C0_aux(MH,QZ,MW)
C     multiply with this to "un-mix" djouadis mixing of on 
C     and off-shell z-mass:
      FW=FW*(QZ/MW)**2     

c     put all together and square:
      HphotonZ=abs(prefW*FW+prefT*FT)**2  

c     average over spin corrections:
      HphotonZ=HphotonZ*(MH**2-QZ**2)**2/2d0

c     prefactor of amplitude squared:
      HphotonZ=HphotonZ/2d0*alpha*GF**2*MW**2*sw**2/pi**3

      end function





C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> photon(1) Z(2) -> photon(1) l+(3) l-(4)   


      doubleprecision function Hphotonll(GF,mW,GammaW,mZ,GammaZ,alpha,
     -     MH,MT,s13,s14,s34)   !Input (QZ is the offshell mass of the Z!!)
      implicit none
      doubleprecision GF,mW,GammaW,mZ,GammaZ,MH,MT,QZ,s13,s14,s34        
      doubleprecision gw,cw,alpha,A,V !internal
      doubleprecision HphotonZ,pi,BWsq
      parameter(pi=3.141592653589793d0)

c     weak couplings:
      gw=dsqrt(8d0*mw**2*GF/sqrt(2d0))
      cw=Mw/MZ
c     ee(or mumu)Z couplings:
      V=gw/cw*(3d0/4d0-cw**2)
      A=gw/cw*(-1d0/4d0)

c     use the result for the H->gamma Z form factor:
      Hphotonll=HphotonZ(GF,mW,GammaW,mZ,GammaZ,alpha,MH,MT,sqrt(s34))
c     divide by Z polarisation sum: 
      Hphotonll=Hphotonll/((MH**2-s34)**2/2d0)

c     multiply by Z lepton coupling, polarisation sum, and Breit-Wigner:
      Hphotonll=(V**2+A**2)*Hphotonll*BWsq(s34,Mz,Gammaz)
      Hphotonll=Hphotonll*s34*(s13**2+s14**2)       

      end function



c
c     auxiliary functions for H->Z gamma
c

      doublecomplex function C2_aux(MH,MZ,MT)
      implicit none
      doubleprecision MH,MZ,MT
      doubleprecision th,tz
      doublecomplex f_aux,g_aux
      th=(2d0*MT/MH)**2
      tz=(2d0*MT/MZ)**2
      C2_aux=tz*th/(tz-th)/2d0+tz*(th/(tz-th))**2/2d0
     &     *(tz*(f_aux(tz)-f_aux(th))+2d0*(g_aux(tz)-g_aux(th)))
      C2_aux=C2_aux
      end function

ccc

      doublecomplex function C0_aux(MH,MZ,MT)
      implicit none
      doubleprecision MH,MZ,MT
      doubleprecision th,tz
      doublecomplex f_aux,g_aux
      th=(2d0*MT/MH)**2
      tz=(2d0*MT/MZ)**2
      C0_aux=-2d0*tz*th/(tz-th)*(f_aux(tz)-f_aux(th))
      C0_aux=C0_aux 
      end function

ccc

      doublecomplex function g_aux(tau)
      implicit none
      doubleprecision tau,tmp,pi
      doublecomplex one,I       
      parameter(pi=3.141592653589793d0)       
      one=(1d0,0d0)
      I=(0d0,1d0)
      if(tau.ge.1d0)then
         g_aux=dsqrt(tau-1d0)*dasin(dsqrt(1d0/tau))*one
      else
         tmp=dlog((1d0+dsqrt(1d0-tau))/(1d0-dsqrt(1d0-tau)))
         g_aux=0.5d0*dsqrt(1-tau)*(tmp*one-I*pi)
      end if      
      end function
ccc

      doublecomplex function f_aux(tau)
      implicit none
      doubleprecision tau,tmp,pi
      doublecomplex one,I
      parameter(pi=3.141592653589793d0)       
      one=(1d0,0d0)
      I=(0d0,1d0)
      if(tau.ge.1d0)then
         f_aux=(dasin(dsqrt(1d0/tau)))**2*one
      else
         tmp=dlog((1d0+dsqrt(1d0-tau))/(1d0-dsqrt(1d0-tau)))
         f_aux=-0.25d0*(tmp*one-I*pi)**2
      end if
      end function






C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> photon(1) photon(1)  

      doubleprecision function Hdiphoton(GF,mW,alpha,MH,MT)                           
      implicit none
      doubleprecision GF,mW,MH,MT      
      doubleprecision Qt,Nc,alpha !internal
      doubleprecision tW,tT
      doubleprecision prefT,pi
      doublecomplex FT,FW,f_aux
      doublecomplex one,I       
      parameter(pi=3.141592653589793d0)       
c     complex:
      one=(1d0,0d0)
      I=(0d0,1d0)      

c     couplings.. 
      Nc=3d0
      Qt=2d0/3d0

c     Compute form factors:
      tT=Mh**2/(2d0*Mt)**2
      tW=Mh**2/(2d0*Mw)**2
      prefT=Qt**2*Nc
      FT=prefT*2d0*(tT*one+(tT-1d0)*f_aux(1d0/tT))/(tT**2) 
      FW=-( (2d0*tW**2+3d0*tW)*one + 3d0*(2d0*tW-1d0)*f_aux(1d0/tW))
     &/(tW**2)

c     put all together and square:
      Hdiphoton=abs(FW+FT)**2  

c     prefactor of amplitude squared:
      Hdiphoton=Hdiphoton*GF*alpha**2*MH**3/128d0/dsqrt(2d0)/pi**3
      
c     now take out phase-space:
      Hdiphoton=Hdiphoton*8d0*pi*(2d0*mh)

c     include NLO:)
      Hdiphoton=Hdiphoton*1.018d0

      end function



















      
