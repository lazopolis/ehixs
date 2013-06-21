C=============================================================================
C---  basis expansions
C=============================================================================

C---- expansion of dilogarithm in y = - log(1-z) with Bernoulli numbers  
C------ requires  routine fbern in bernoulli.F for the  coefficients 
C------- of the series  expansion 

      double  complex function bsli2_inside(z)
      implicit none
      integer i, Nmax
      double complex elem, ris, z, zb 
      double precision fbern
      Nmax=20
      zb = dcmplx(1d0,0d0)-z
      zb = -log(zb)
      ris = dcmplx(0d0, 0d0)
      do i=0,Nmax
      elem = zb**(i+1)*fbern(i)/(i+1)
      ris = ris+elem
      enddo
      bsli2_inside=ris 
      return 
      end

C---- expansion of the dilogarithm in log(z) with Zeta values  
C------requires  routine zetaval2 in coefficients.F for the  coefficients 
C-------of the series  expansion 
C-------- used for border < |z| < 1
      
      double  complex function bsli2_outside(z)
      implicit none
      integer i, Nmax
      double complex elem, ris, z, zb, coeffi
      double precision zetaval2
      Nmax=60
      zb = log(z)
      ris = dcmplx(0d0, 0d0)
      do i=0,Nmax 
         if (i.eq.1) then
            coeffi = 1d0 -log(-zb)
         else
            coeffi = dcmplx(zetaval2(i),0d0)
         endif
         elem = zb**i*coeffi
         ris = ris+elem
      enddo
      
      bsli2_outside=ris 
      return 
      end

C---- expansion of trilogarithm in y = - log(1-z) with Bernoulli numbers  
C------requires  routine fbern3 in coefficients.F for the  coefficients 
C-------of the series  expansion 
      
      double  complex function bsli3_inside(z)
      implicit none
      integer i, Nmax
      double complex elem, ris, z, zb 
      double precision fbern3, coeffi
      Nmax=20
      zb = dcmplx(1d0,0d0)-z
      zb = -log(zb)
      ris = dcmplx(0d0, 0d0)
      do i=0,Nmax 
         coeffi=fbern3(i)
         elem = zb**(i+1)*coeffi/(i+1)
         ris = ris+elem
      enddo
      
      bsli3_inside=ris 
      return 
      end

C---- expansion of the trilogarithm in log(z) with Zeta values  
C------requires  routine zetaval3 in coefficients.F for the  coefficients 
C-------of the series  expansion 
C-------- used for border < |z| < 1
      
      double  complex function bsli3_outside(z)
      implicit none
      integer i, Nmax
      double complex elem, ris, z, zb, coeffi
      double precision zetaval3
      Nmax=60
      zb = log(z)
      ris = dcmplx(0d0, 0d0)
      do i=0,Nmax 
         if (i.eq.2) then
            coeffi = (1d0 + 0.5d0 -log(-zb))/2d0
         else
            coeffi = dcmplx(zetaval3(i),0d0)
         endif
         elem = zb**i*coeffi
         ris = ris+elem
      enddo
      
      bsli3_outside=ris 
      return 
      end

C---- expansion of tetralogarithm in y = - log(1-z) with Bernoulli numbers  
C------requires  routine fbern4 in coefficients.F for the  coefficients 
C-------of the series  expansion 
      
      double  complex function bsli4_inside(z)
      implicit none
      integer i, Nmax
      double complex elem, ris, z, zb 
      double precision coeffi, fbern4
      Nmax=20
      zb = dcmplx(1d0,0d0)-z
      zb = -log(zb)
      ris = dcmplx(0d0, 0d0)
      do i=0,Nmax 
         coeffi=fbern4(i)
         elem = zb**(i+1)*coeffi/(i+1)
         ris = ris+elem
      enddo
      
      bsli4_inside=ris 
      return 
      end

C---- expansion of tetralogarithm in y = log(z) with Zeta values  
C------requires  routine zetaval in coefficients.F for the  coefficients 
C-------of the series  expansion 
C-------- used for 0.3 < |z| < 1
      
      double  complex function bsli4_outside(z)
      implicit none
      integer i, Nmax
      double complex elem, ris, z, zb, coeffi
      double precision zetaval4
      Nmax=60
      zb = log(z)
      ris = dcmplx(0d0, 0d0)
      do i=0,Nmax 
         if (i.eq.3) then
            coeffi = 1d0 + 0.5d0 + 1d0/3d0 -log(-zb)
            coeffi = coeffi/6d0 !factorial, for zeta values absorbed in zetaval
         else
            coeffi = dcmplx(zetaval4(i),0d0)
         endif
         elem = zb**i*coeffi
         ris = ris+elem
      enddo
      
      bsli4_outside=ris 
      return 
      end

C---- expansion of H2m2(z) = -Li22(-1,z) in y=-log(1+z)
C---- requires the routine bsh2m2_inside_coeff in li22coeff.F for the coefficients
C---- Nmax is the highest order of the taylor expansion

      double complex function bsh2m2_inside(z)
      implicit none
      double complex z,y,elem,ris,coeff
      double precision bsh2m2_inside_coeff
      integer n, Nmax

      Nmax = 60
      
      y = dcmplx(1d0,0d0)+z
      y = -log(y)
      ris = dcmplx(0d0,0d0)
      do n=0,Nmax
         coeff = bsh2m2_inside_coeff(n)
         elem = y**(n+1)*coeff
         ris = ris + elem
      enddo
      
      bsh2m2_inside = ris
      return 
      end

C---- expansion of H2m2(z) = -Li22(-1,z) in y = log(z) (and Re(z) >= 0)
C---- requires the routine bsh2m2_outside_coeff in li22coeff.F for the coefficients
C---- Nmax is the highest order of the taylor expansion

      double complex function bsh2m2_outside(z)
      implicit none
      double complex z,y,elem,ris,coeff,cli2,cli4
      double precision bsh2m2_outside_coeff,pi,zeta3,ll2
      integer n, Nmax

      pi=3.1415926535897932385D0
      zeta3=1.20205690315959428539973816151d0
      ll2 = dlog(2d0)
      Nmax = 60
   
      y = log(z)
      ris = dcmplx(0d0,0d0)
      do n=0,Nmax
         coeff = bsh2m2_outside_coeff(n)
         elem = y**(n+2)*coeff
         ris = ris + elem
      enddo
      ! additional pieces (not part of the sum)
      ris = ris + 71d0/1440d0*pi**4 + 1d0/6d0*pi**2*ll2**2 
     &     - ll2**4/6d0 - 4d0*cli4(dcmplx(0.5d0,0d0)) 
     &     - 7d0/2d0*ll2*zeta3 
     &     - 5d0/8d0*zeta3*log(z) + pi**2/12d0*cli2(z) - pi**4/72d0
      
      bsh2m2_outside = ris
      return 
      end


C---- expansion of H_2,1,-1(z) in y = log(1-z)
C---- requires the routine bsh21m1_inside_coeff in li22coeff.F for the coefficients
C---- Nmax is the highest order of the taylor expansion

      double complex function bsh21m1_inside(z)
      implicit none
      double complex z,y,elem,ris,coeff
      double precision bsh21m1_inside_coeff
      integer n, Nmax
      
      Nmax = 60
      
      y = dcmplx(1d0,0d0)-z
      y = -log(y)
      ris = dcmplx(0d0,0d0)
      do n=0,Nmax
         coeff = bsh21m1_inside_coeff(n)
         elem = y**(n+1)*coeff
         ris = ris + elem
      enddo

      ris = ris 

      bsh21m1_inside = ris
      return
      end

C---- expansion of H_2,1,-1(z) in y = log(z) for Re(z) >= 0
C---- requires the routine bsh21m1_outside_1_coeff in li22coeff.F for the coefficients
C---- Nmax is the highest order of the taylor expansion

      double complex function bsh21m1_outside_1(z)
      implicit none
      double complex z,y,elem,ris,coeff,cli2,cli4,cli3
      double precision bsh21m1_outside_1_coeff,pi,zeta3,ll2
      integer n, Nmax

      pi=3.1415926535897932385D0
      zeta3=1.20205690315959428539973816151d0
      ll2 = dlog(2d0)
      Nmax = 60

      y = log(z)
      ris = dcmplx(0d0,0d0)
      do n=0,Nmax
         coeff = bsh21m1_outside_1_coeff(n)
         elem = y**(n+2)*coeff
         ris = ris + elem
      enddo
      
      ! additional pieces (not part of the sum)
      ris = ris - pi**4/80d0 + pi**2/12d0*ll2**2+ll2**4/24d0 
     &     + cli4(dcmplx(0.5d0,0d0)) + 7d0/8d0*ll2*zeta3 
     &     + y*(7d0*zeta3/8d0 + ll2**3/6d0 
     &     - pi**2/12d0*ll2) - (0.5d0*ll2**2 - pi**2/12d0)
     &     *(pi**2/6d0 - cli2(z)) + ll2*(-cli3(1d0 - z) 
     &     + cli2(1d0 - z)*log(1d0 - z) + 0.5d0*y*log(1d0-z)**2)
   
      bsh21m1_outside_1 = ris
      return
      end

C---- expansion of H_2,1,-1(z) in y = log(-z) for Re(z) <= 0
C---- requires the routine bsh21m1_outside_2_coeff in li22coeff.F for the coefficients
C---- Nmax is the highest order of the taylor expansion

      double complex function bsh21m1_outside_2(z)
      implicit none
      double complex z,y,elem,ris,coeff1,coeff2,coeff3,cli2,cli4,m1
      double precision bsh21m1_outside_2_coeff,pi,zeta3,ll2
      integer n, Nmax

      pi=3.1415926535897932385D0
      zeta3=1.20205690315959428539973816151d0
      ll2 = dlog(2d0)
      Nmax = 60
      m1 = dcmplx(-1d0,0d0)
      
      y = log(-z)
      ris = dcmplx(0d0,0d0)
      
      do n=0,Nmax
         coeff1 = bsh21m1_outside_2_coeff(n,1)
     &        *(log(-y) - 1d0/(n+1) - 1d0/(n+2))/4d0
         coeff2 = -bsh21m1_outside_2_coeff(n,2)/4d0
         coeff3 = -bsh21m1_outside_2_coeff(n,3)/4d0
         elem = y**(n+2)*(coeff1 + coeff2 + coeff3)
         ris = ris + elem
      enddo
      
!     additional pieces (not part of the sum)
      ris = ris + pi**4/80d0 - pi**2*ll2**2/24d0 
     &     - ll2**4/12d0 - (pi**2/12d0 
     &     - ll2**2/2d0)*(-pi**2/12d0 -ll2*y - cli2(z)) 
     &     - 2d0*cli4(dcmplx(0.5d0,0d0)) 
     &     - y*(-ll2**3/6d0 + zeta3/8d0)
      
      bsh21m1_outside_2 = ris 
      return
      end
