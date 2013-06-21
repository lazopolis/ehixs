  
      subroutine rqqbar2ght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision rqqbar2ght1s1e1  
      doubleprecision rqqbar2ght1s1e0  
      doubleprecision rqqbar2ght1s1em1  
      doubleprecision rqqbar2ght1s1em2  
      doubleprecision rqqbar2ght1s1em3  
      doubleprecision rqqbar2ght1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rqqbar2ght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rqqbar2ght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rqqbar2ght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rqqbar2ght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rqqbar2ght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rqqbar2ght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rqqbar2ght1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x1
      t6 = 0.1D1 / z
      t7 = t1 ** 2
      t8 = t7 * t1
      t14 = log(-t6 * t7 * x1 * t4)
      t19 = x1 ** 2
      t24 = 0.12D2 * t6 * t8 + (0.12D2 * lh + 0.12D2 * t14) * t6 * t8 * 
     #(0.1D1 - 0.2D1 * x1 + 0.2D1 * t19)
      t27 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, 0.
     #4D1 / 0.27D2 * t24 * wd)
      rqqbar2ght1s1e1 = 0.4D1 / 0.27D2 * t27 * t24 * wd

      end function



      doubleprecision function rqqbar2ght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = 0.1D1 / z
      t7 = t1 ** 2
      t8 = t7 * t1
      t11 = x1 ** 2
      t13 = 0.1D1 - 0.2D1 * x1 + 0.2D1 * t11
      t17 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, -0.16D2 / 0.9D1 * t6 * t8 * t13 * wd)
      rqqbar2ght1s1e0 = -0.16D2 / 0.9D1 * t17 * t6 * t8 * t13 * wd

      end function



      doubleprecision function rqqbar2ght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqqbar2ght1s1em1 = 0.0D0

      end function



      doubleprecision function rqqbar2ght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqqbar2ght1s1em2 = 0.0D0

      end function



      doubleprecision function rqqbar2ght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqqbar2ght1s1em3 = 0.0D0

      end function



      doubleprecision function rqqbar2ght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqqbar2ght1s1em4 = 0.0D0

      end function
