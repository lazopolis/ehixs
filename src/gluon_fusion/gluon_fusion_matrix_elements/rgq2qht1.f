  
      subroutine rgq2qht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision rgq2qht1s1e1  
      doubleprecision rgq2qht1s1e0  
      doubleprecision rgq2qht1s1em1  
      doubleprecision rgq2qht1s1em2  
      doubleprecision rgq2qht1s1em3  
      doubleprecision rgq2qht1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rgq2qht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rgq2qht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rgq2qht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rgq2qht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rgq2qht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rgq2qht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rgq2qht1s1e1
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
      t3 = 0.1D1 / z
      t4 = t3 * wd
      t5 = z ** 2
      t6 = x1 * t3
      t7 = t1 ** 2
      t9 = log(t6 * t7)
      t10 = 0.2D1 * z
      t11 = 0.2D1 + t5 - t10
      t18 = (0.12D2 * lh - 0.12D2) * t3
      t22 = 0.1D1 / x1
      t25 = log(t3 * t7)
      t29 = lh ** 2
      t31 = 0.3141592653589793D1 ** 2
      t34 = t25 ** 2
      t44 = (-0.12D2 * t4 * (-t5 - t9 * t11) + t18 * wd * t11) * t22 / 0
     #.18D2 + (-(0.12D2 * lh + 0.12D2 * t25 - 0.12D2) * z + (-0.6D1 * t2
     #9 + t31 - 0.12D2 * t25 * lh - 0.6D1 * t34 + 0.12D2 * lh + 0.12D2 *
     # t25 - 0.12D2) * t3 * t11) * wd / 0.18D2
      t45 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t44)
      t47 = -0.1D1 + x1
      t50 = x1 * z
      t52 = x1 ** 2
      t54 = 0.2D1 * t52 * z
      t56 = 0.2D1 * t5 * x1
      t57 = t52 * t5
      t60 = log(-t6 * t7 * t47)
      t63 = 0.2D1 - 0.2D1 * x1 + t52 - t10 + 0.4D1 * t50 - t54 + t5 - t5
     #6 + t57
      t70 = 0.12D2 * t4 * (-0.2D1 * t50 - t52 + t54 - t5 + t56 - t57 - t
     #60 * t63) - t18 * wd * t63
      t73 = FJET(XB1, XB2, s, -t2 * t47, t2 * x1, 0.0D0, 0.0D0, 0.0D0, t
     #70 * t22 / 0.18D2)
      rgq2qht1s1e1 = t45 * t44 + t73 * t70 * t22 / 0.18D2

      end function



      doubleprecision function rgq2qht1s1e0
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
      t3 = 0.1D1 / z
      t4 = t3 * wd
      t5 = z ** 2
      t6 = 0.2D1 * z
      t7 = 0.2D1 + t5 - t6
      t8 = 0.1D1 / x1
      t13 = t1 ** 2
      t15 = log(t3 * t13)
      t23 = -0.2D1 / 0.3D1 * t4 * t7 * t8 + (0.12D2 * z + (0.12D2 * lh +
     # 0.12D2 * t15 - 0.12D2) * t3 * t7) * wd / 0.18D2
      t24 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t23)
      t30 = x1 ** 2
      t38 = 0.2D1 - 0.2D1 * x1 + t30 - t6 + 0.4D1 * x1 * z - 0.2D1 * t30
     # * z + t5 - 0.2D1 * t5 * x1 + t30 * t5
      t42 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, 0.2D1 / 0.3D1 * t4 * t38 * t8)
      rgq2qht1s1e0 = t24 * t23 + 0.2D1 / 0.3D1 * t42 * t3 * wd * t38 * t
     #8

      end function



      doubleprecision function rgq2qht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = 0.1D1 / z
      t5 = z ** 2
      t7 = 0.2D1 + t5 - 0.2D1 * z
      t10 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, -0.2D1 / 0.3D1 * t3 * wd * t7)
      rgq2qht1s1em1 = -0.2D1 / 0.3D1 * t10 * t3 * wd * t7

      end function



      doubleprecision function rgq2qht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgq2qht1s1em2 = 0.0D0

      end function



      doubleprecision function rgq2qht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgq2qht1s1em3 = 0.0D0

      end function



      doubleprecision function rgq2qht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgq2qht1s1em4 = 0.0D0

      end function
