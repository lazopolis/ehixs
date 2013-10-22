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
     -     s12,s13,s14,s23,s24,s34)

      implicit none
C     Input:
      doubleprecision GF,mW,GammaW,mZ,GammaZ,s12,s13,s14,s23,s24,s34
C     Internal variables:
      doubleprecision pref,form,gw,V1,V2,A1,A2,cosw
C     Functions
      doubleprecision BWsq,BWprodint

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
c     coupling (HZZ)*(HWW)
      pref=(gw*Mz/cosw)*(gw*Mw)
      pref=pref*gw**2/2**(3)
c     form factor:
      form=32d0*BWprodint(s23,s14,s12,s34,Mz,Mz,Mw,Mw,
     - Gammaz,Gammaz,Gammaw,Gammaw)
      form=form*(A1+V1)*(A2+V2)*s24*s13

c     squared amplitude
      HWWZZlnln=pref*form
      end function





C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCC   The Amplitude for H -> Z Z  -> l l l l 

      doubleprecision function HZZllll(GF,mW,GammaW,mZ,GammaZ,
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
c     electron-Z
      V=gw/cosw*(3d0/4d0-cosw**2)
      A=gw/cosw*(-1d0/4d0)

c     coupling (HZZ)^2
      pref=(gw*Mz/cosw)**2

C     branch into topologies:

c      if(topology.eq.'s12s34')then
         form=(8*S13*(V**4+A**4+6*V**2*A**2)*S24
     -        +8*S14*S23*(A-V)**2*(A+V)**2)
     -        *BWsq(s12,Mz,Gammaz)*BWsq(s34,Mz,Gammaz)    
         
c      else if(topology.eq.'s23s14')then
         form=form+(8*S13*(V**4+A**4+6*V**2*A**2)*S24
     -        +8*S12*S34*(A-V)**2*(A+V)**2)
     -        *BWsq(s23,Mz,Gammaz)*BWsq(s14,Mz,Gammaz)            

c     else if(topology.eq.'s12s34s23s14')then
         form=form+16*S13*(V**4+A**4+6*V**2*A**2)*S24
     -        *BWprodint(s23,s14,s12,s34,Mz,Mz,Mz,Mz,
     -        Gammaz,Gammaz,Gammaz,Gammaz)
         
c      end if

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













      
