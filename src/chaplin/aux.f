C=============================================================================
C---  Auxiliary functions
C=============================================================================

c ------------------------------------------------
c --- some auxiliary functions:
      double complex function s(x)
      implicit none
      double complex x,ris
      ris = dcmplx(1d0,0d0)
      if (dimag(x).lt.0d0) then
         ris = dcmplx(-1d0,0d0)
      endif
      s=ris
      return
      end function
c ------------------------------------------------
c s1(x) = s(x^2)...if the imaginary part of x^2 is zero, it depends
c on the value of the real part of x if we return -1 or 1 because:
c (x+I*eps) = x^2 + 2*x*I*eps - eps^2
      double complex function s1(x)
      implicit none
      double complex x,ris
      ris = dcmplx(1d0,0d0)
      if (dimag(x**2).lt.0d0) then
         ris = dcmplx(-1d0,0d0)
      else if (dimag(x**2).eq.0d0.and.dreal(x).lt.0d0) then
         ris = dcmplx(-1d0,0d0)
      endif
      s1=ris
      return
      end function
c ------------------------------------------------
c s2(x) = s(4x/(1+x)^2)...if the imaginary part of the arg is zero, it depends
c on the value of the real part of x if we return -1 or 1 because:
c 4*(x+I*eps)/(1+x+I*eps)^2 = 4x/(1+x)^2 + 4*(1-x)*I*eps/(1+x)^3
      double complex function s2(x)
      implicit none
      double complex x,ris,arg
      arg = 4d0*x/(1d0+x)**2
      ris = dcmplx(1d0,0d0)
      if (dimag(arg).lt.0d0) then
         ris = dcmplx(-1d0,0d0)
      else if (dimag(arg).eq.0d0.and.dreal(1d0-x).lt.0d0) then
         ris = dcmplx(-1d0,0d0)
      endif
      s2=ris
      return
      end function
c ------------------------------------------------
