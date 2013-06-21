C=============================================================================
C---  basis functions
C=============================================================================
c---  Li2

      double complex  function cli2(z)
      implicit none
      double complex ris, z, bsli2_inside,bsli2_outside, wcli2
      double precision zabs, pi, zeta2, border

      pi=3.1415926535897932385D0
      zeta2=pi**2/6d0

      border = 0.3d0 
      zabs = abs(z)
      if (z.eq.(dcmplx(1d0,0d0))) then 
         ris = dcmplx(zeta2,0d0)
      else
         if (zabs.le.1d0) then
            if (zabs.le.border) then 
               ris=bsli2_inside(z)
            else
               ris=bsli2_outside(z)
            endif
         else
            ris=-wcli2(1d0/z)-zeta2-0.5d0*log(-z)**2
         endif
      endif
         cli2=ris
         return
      end
      
c---  recursion
      
      double complex  function wcli2(z)
      implicit none
      double complex z, cli2
      wcli2 =  cli2(z)
      return
      end

c--- Li3

      double complex  function cli3(z)
      implicit none
      double complex ris, z, bsli3_inside,bsli3_outside, wcli3
      double precision zabs,border, pi, zeta2, zeta3
      
      pi=3.1415926535897932385D0
      zeta2=pi**2/6d0
      zeta3=1.20205690315959428539973816151d0
     
      border = 0.3d0
      zabs = abs(z)

      if (z.eq.(dcmplx(1d0,0d0))) then 
         ris = dcmplx(zeta3,0d0)
      else
         
         if (zabs.le.1d0) then
            if (zabs.le.border) then 
               ris=bsli3_inside(z)
            else
               ris=bsli3_outside(z)
            endif
         else
            ris=wcli3(1d0/z)-log(-z)**3/6d0-zeta2*log(-z)
         endif
      endif
      cli3=ris
      return
      end
      
c---  recursion

      double complex  function wcli3(z)
      implicit none
      double complex z, cli3
      wcli3 =  cli3(z)
      return
      end

c--- Li4

      double complex  function cli4(z)
      implicit none
      double complex ris, z, bsli4_outside, bsli4_inside, wcli4
      double precision zabs, pi, zeta2, zeta3, zeta4, border
     
      pi=3.1415926535897932385D0
      zeta2=pi**2/6d0
      zeta3=1.20205690315959428539973816151d0
      zeta4=pi**4/90d0
      
      border = 0.3d0
      zabs = abs(z)
      
      if (z.eq.dcmplx(1d0,0d0)) then 
         ris = dcmplx(zeta4,0d0)
      else 
         if (zabs.le.1d0) then
            if (zabs.le.border) then ! on the mini-disc (remember those?) of |z| <= 0.3, we use the log(1-x) expansion
               ris=bsli4_inside(z)
            else ! on the annulus 0.3 < |z| <= 1, we use the log(x) expansion
               ris=bsli4_outside(z)
            endif
         else                   ! outside the unit circle, the inversion mapping is needed. NOTE: this is "our" mapping, derived from the integral rep. of Li4 and absorbing
c                                 all imaginary parts into log(-z).
            ris=-wcli4(1d0/z) -log(-z)**4/24d0 - 7d0*zeta4/4d0 
     &           - zeta2*log(-z)**2/2d0
         endif
      endif
      
      cli4=ris
      return
      end
      
c     --- recursion for li4
      
      double complex  function wcli4(z)
      implicit none
      double complex z, cli4
      wcli4 = cli4(z)
      return
      end
c --- the case Li4(1-z^2) needs some special treatment because of its branch cut structure
c --- (that's what 'sbc' stands for: special branch cut)

      double complex function cli4_sbc(z)
      implicit none
      double complex ris, z, cli4, myi,basis14 !,cli4_with_signim
      double complex ll1,ll2,ll3
      double precision pi,zabs,zreal
      integer signim
      
      pi=3.1415926535897932385D0
      zabs = abs(z)
      zreal = dreal(z)
      myi = dcmplx(0d0,1d0)
      if (dimag(z).ne.0d0) then 
         signim = dimag(z)/dabs(dimag(z)) ! the sign of the imaginary part of z (+- 1)
      else
         signim = 1
      endif
           
      if (zabs.le.1d0) then !normal li4
         if (zreal.gt.0d0) then
            ris = cli4(1d0 - z**2)
         else if (zreal.eq.0d0 .and. signim.eq.1) then !also normal li4
            ris = cli4(1d0 - z**2 - dcmplx(0d0,1d-60))
         else                   ! special branch cut configuration
            ris = cli4(1d0 - z**2- dcmplx(0d0,1d-60))
     &           - myi*pi*signim/3d0*(log(1d0 - z)+log(1d0+z))**3 
         endif
      else 
         ll1=log(1d0/z)
         ll2=log(1d0 - 1d0/z)
         ll3=log(1d0 + 1d0/z)
         ris = -2d0/3d0*ll1**4 + 4d0/3d0*ll2
     &        *ll1**3 + 4d0/3d0*ll3*ll1**3 
     &        - ll2**2*ll1**2 - ll3**2
     &        *ll1**2 - 2d0*ll2*ll3
     &        *ll1**2 - pi**2/3d0*ll1**2 + 1d0/3d0
     &        *ll2**3*ll1 + 1d0/3d0
     &        *ll3**3*ll1 + ll2
     &        *ll3**2*ll1 + pi**2/3d0*ll1
     &        *ll2 + ll3*ll2**2
     &        *ll1 + pi**2/3d0*ll1*ll3 
     &        - 1d0/24d0*ll2**4 - 1d0/24d0
     &        *ll3**4 - 1d0/6d0*ll2
     &        *ll3**3 - pi**2/12d0*ll2**2 
     &        - pi**2/12d0*ll3**2 
     &        - 1d0/4d0*ll2**2*ll3**2 
     &        - 1d0/6d0*ll2**3*ll3 
     &        - pi**2/6d0*ll2*ll3 
     &        - 7*pi**4/360d0 
     &        -basis14(1d0/z)
      endif
      
      cli4_sbc = ris
      return
      end

c --- the case Li4(4z/(1+z)^2) also needs some special treatment because of its branch cut structure

      double complex function cli4_sbc_2(z)
      implicit none
      double complex ris, z, cli4, myi, wcli4_sbc_2
      double complex arg
      double precision pi,zabs,zreal,ll2
      integer signim
      
      pi=3.1415926535897932385D0
      ll2 = dlog(2d0)
      zabs = abs(z)
      zreal = dreal(z)
      myi = dcmplx(0d0,1d0)
      if (dimag(z).ne.0d0) then 
         signim = dimag(z)/dabs(dimag(z))
      else
         signim = 1
      endif
      
      
      ris = dcmplx(0d0,0d0)
      if (zabs.eq.1d0) then 
         arg = dcmplx(dreal(4d0*z/(1d0+z)**2),signim*1d-60)
         ris = cli4(arg)
      else
         if (zabs.lt.1d0) then
            ris = cli4(4d0*z/(1d0+z)**2)
         else                   
            ris = wcli4_sbc_2(1d0/z) + myi*pi*signim*
     &(4d0*ll2**2*log(1d0/z) - 8d0*ll2**2*log(1d0+1d0/z) 
     &+ 2d0*ll2*log(1d0/z)**2 - 8d0*ll2*log(1d0/z)
     &*log(1d0+1d0/z) + 8d0*ll2*log(1d0+1d0/z)**2 
     &+ 1d0/3d0*log(1d0/z)**3 - 2d0*log(1d0+1d0/z)*log(1d0/z)**2 
     &+ 4d0*log(1d0+1d0/z)**2*log(1d0/z) - 8d0/3d0*log(1d0+1d0/z)**3 
     &+ 8d0/3d0*ll2**3)
         endif
      endif
      
      cli4_sbc_2 = ris
      return
      end

c     --- recursion for cli4_sbc_2
      
      double complex  function wcli4_sbc_2(z)
      implicit none
      double complex z, cli4_sbc_2
      wcli4_sbc_2 =  cli4_sbc_2(z)
      return
      end

C-----------------------------------------------------------------------
C     mapping of H_2-2(z) into convergent region
      
      double complex  function ch2m2(z)
      implicit none
      double complex ris,z,bsh2m2_inside,bsh2m2_outside,cli4,cli2
      double complex HPL4,wch2m2,myi !,cli4_sbc,cli3
      double precision pi,zeta2,zeta3,zeta4,zabs,zreal,border
      integer signim

      pi=3.1415926535897932385D0
      zeta2=pi**2/6d0
      zeta3=1.20205690315959428539973816151d0
      zeta4=pi**4/90d0
      myi = dcmplx(0d0,1d0)
      
      border = 0.3d0
      zabs = abs(z)
      zreal = dreal(z)
      if (dimag(z).ne.0d0) then 
         signim = dimag(z)/dabs(dimag(z)) ! the sign of the imaginary part of z (+- 1)
      else
         signim = 1
      endif
      
      
      if (zabs.le.1d0) then
         if (zabs.lt.border) then ! inside circle of |z| = 0.3, we employ the log(1+z) expansion
            ris = bsh2m2_inside(z)
         else
            if (zreal.ge.0d0) then ! on the half annulus 0.3 < |z| < 1 ; Re(z) >= 0, we have the log(x) exp.
               ris = bsh2m2_outside(z)
            else                ! for Re(z) < 0, we map back to Re(z) > 0 by using the fact that HPL4(n1,n2,n3,n4,z) = (+-) HPL4(-n1,-n2,-n3,-n4,-z) (if n4 =/= 0):
               ris = HPL4(0,-1,0,1,-z) 
            endif
         endif
      else                      ! For |z| > 1, we use the inversion formula to map into the unit circle. 
         ris = wch2m2(1d0/z) + 37d0*pi**4/720d0 
     &        - HPL4(0,1,0,0,1d0/z) 
     &        - log(1d0/z)**4/24d0 - pi**2/12d0*log(1d0/z)**2 
     &        - pi**2/6d0*cli2(1d0/z) 
     &        - cli4(-1d0/z) 
     &        + 3d0*zeta3*log(1d0/z)/2d0 
     &        - pi**3*myi*signim*log(1d0/z)/12d0
      endif
      ch2m2=ris
      return
      end
      
      
c     --- recursion for H_2-2(z)
      
      double complex  function wch2m2(z)
      implicit none
      double complex z, ch2m2
           
      wch2m2 = ch2m2(z)
      return
      end

C------------------------------------------------------------------------------
C     mapping of H21-1(z) into convergent region 
      
      double complex  function ch21m1(z)
      implicit none
      double complex ris,z,bsh21m1_inside,bsh21m1_outside_1
      double complex bsh21m1_outside_2,cli4,cli2,HPL4,wch21m1,myi,ch2m2
      double precision pi,zeta2,zeta3,zeta4,border,zreal,zabs,ll2
      integer signim

      pi=3.1415926535897932385D0
      zeta2=pi**2/6d0
      zeta3=1.20205690315959428539973816151d0
      zeta4=pi**4/90d0
      ll2 = dlog(2d0)
      border = 0.3d0
      myi = dcmplx(0d0,1d0)

      if (dimag(z).ne.0d0) then 
         signim = dimag(z)/dabs(dimag(z))
      else
         signim = 1
      endif

      zabs = abs(z)
      zreal = dreal(z)
      
      
      if (zabs.le.1d0) then
         if (zabs.lt.border) then ! inside circle of |z| = 0.3, we employ the log(1-z) expansion
            ris = bsh21m1_inside(z)
         else
            if (zreal.ge.0d0) then ! on the half annulus 0.3 < |z| < 1 ; Re(z) >= 0, we have a log(x) exp.
               ris = bsh21m1_outside_1(z)
            else                ! for Re(z) < 0, there is a different log(x) expansion
               ris = bsh21m1_outside_2(z)
            endif
         endif
      else                      ! For |z| > 1, we use the inversion formula to map into the unit circle. 
         ris = -wch21m1(1d0/z)-pi**4/144d0 -ch2m2(1d0/z) 
     &        - HPL4(0,0,1,-1,1d0/z) + HPL4(0,0,1,0,1d0/z) 
     &        + HPL4(0,1,0,0,1d0/z) 
     &        + HPL4(0,1,1,0,1d0/z) + log(1d0/z)**4/24d0 
     &        + pi**2*ll2**2/3d0 - ll2**4/12d0 
     &        + 3d0*pi**2*ll2*log(1d0/z)/4d0 
     &        + pi**2*log(1d0/z)**2/8d0 
     &        + pi**2*cli2(1d0/z)/4d0 
     &        - 2*cli4(dcmplx(0.5d0,0d0)) 
     &        + cli4(-1d0/z) 
     &        - 7d0*zeta3*log(1d0/z)/8d0 
     &        + myi*signim*(pi**3*ll2/6d0 
     &        + pi**3*log(1d0/z)/12d0 
     &        - 0.5d0*pi*ll2**2*log(1d0/z) 
     &        - 0.5d0*pi*ll2*log(1d0/z)**2 
     &        - pi*ll2*cli2(1d0/z))
         
      endif
      ch21m1=ris
      return
      end
      

c     --- recursion for H21-1(z)
      
      double complex  function wch21m1(z)
      implicit none
      double complex z, ch21m1
            
      wch21m1 =  ch21m1(z)
      return
      end
