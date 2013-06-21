  
      subroutine rqg2qht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision rqg2qht1s1e1  
      doubleprecision rqg2qht1s1e0  
      doubleprecision rqg2qht1s1em1  
      doubleprecision rqg2qht1s1em2  
      doubleprecision rqg2qht1s1em3  
      doubleprecision rqg2qht1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rqg2qht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rqg2qht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rqg2qht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rqg2qht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rqg2qht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rqg2qht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rqg2qht1s1e1
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
      t11 = 0.2D1 - t10 + t5
      t18 = (0.12D2 * lh - 0.12D2) * t3
      t23 = 0.1D1 / x1
      t26 = log(t3 * t7)
      t30 = lh ** 2
      t32 = 0.3141592653589793D1 ** 2
      t35 = t26 ** 2
      t45 = -(-0.12D2 * t4 * (t5 + t9 * t11) - t18 * wd * t11) * t23 / 0
     #.18D2 + (-(0.12D2 * lh + 0.12D2 * t26 - 0.12D2) * z + (-0.6D1 * t3
     #0 + t32 - 0.12D2 * t26 * lh - 0.6D1 * t35 + 0.12D2 * lh + 0.12D2 *
     # t26 - 0.12D2) * t3 * t11) * wd / 0.18D2
      t46 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t45)
      t49 = -0.1D1 + x1
      t52 = 0.2D1 * t5 * x1
      t53 = x1 ** 2
      t54 = t5 * t53
      t55 = z * x1
      t58 = 0.2D1 * z * t53
      t61 = log(-t6 * t7 * t49)
      t64 = 0.2D1 - t10 - 0.2D1 * x1 + t5 + t53 + 0.4D1 * t55 - t58 - t5
     #2 + t54
      t71 = -0.12D2 * t4 * (t52 - t54 - 0.2D1 * t55 + t58 - t5 - t53 - t
     #61 * t64) + t18 * wd * t64
      t74 = FJET(XB1, XB2, s, t2 * x1, -t2 * t49, 0.0D0, 0.0D0, 0.0D0, -
     #t71 * t23 / 0.18D2)
      rqg2qht1s1e1 = t46 * t45 - t74 * t71 * t23 / 0.18D2

      end function



      doubleprecision function rqg2qht1s1e0
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
      t5 = 0.2D1 * z
      t6 = z ** 2
      t7 = -0.2D1 + t5 - t6
      t8 = 0.1D1 / x1
      t13 = t1 ** 2
      t15 = log(t3 * t13)
      t24 = 0.2D1 / 0.3D1 * t4 * t7 * t8 + (0.12D2 * z - (0.12D2 * lh + 
     #0.12D2 * t15 - 0.12D2) * t3 * t7) * wd / 0.18D2
      t25 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t24)
      t31 = x1 ** 2
      t39 = 0.2D1 - t5 - 0.2D1 * x1 + t6 + t31 + 0.4D1 * z * x1 - 0.2D1 
     #* z * t31 - 0.2D1 * t6 * x1 + t6 * t31
      t43 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, 0.2D1 / 0.3D1 * t4 * t39 * t8)
      rqg2qht1s1e0 = t25 * t24 + 0.2D1 / 0.3D1 * t43 * t3 * wd * t39 * t
     #8

      end function



      doubleprecision function rqg2qht1s1em1
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
      t6 = 0.2D1 - 0.2D1 * z + t5
      t10 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, -0.2D1 / 0.3D1 * t3 * t6 * wd)
      rqg2qht1s1em1 = -0.2D1 / 0.3D1 * t10 * t3 * t6 * wd

      end function



      doubleprecision function rqg2qht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqg2qht1s1em2 = 0.0D0

      end function



      doubleprecision function rqg2qht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqg2qht1s1em3 = 0.0D0

      end function



      doubleprecision function rqg2qht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rqg2qht1s1em4 = 0.0D0

      end function
