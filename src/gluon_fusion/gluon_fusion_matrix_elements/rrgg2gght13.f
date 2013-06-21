  
      subroutine rrgg2gght13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght13s1e1  
      doubleprecision rrgg2gght13s1e0  
      doubleprecision rrgg2gght13s1em1  
      doubleprecision rrgg2gght13s1em2  
      doubleprecision rrgg2gght13s1em3  
      doubleprecision rrgg2gght13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght13s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * x1
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = x3 * s * t7
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t13 = t10 * t7
      t14 = t1 ** 2
      t15 = t14 * wd
      t16 = x1 * z
      t17 = -z - x1 + t16
      t18 = 0.1D1 / t17
      t20 = x4 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t25 = Sqrt(x3 * t17 * t9)
      t26 = t25 ** 2
      t28 = 0.1D1 / x2
      t39 = x1 ** 2
      t40 = x3 * t39
      t41 = Sin(t20)
      t42 = t41 ** 2
      t44 = z ** 2
      t47 = t6 ** 2
      t52 = log(0.4D1 * t40 * t42 / t44 * t18 * t47 * t9)
      t61 = -0.18D2 * t15 * t18 * t22 * t26 * t28 - 0.36D2 * t15 * t18 *
     # t22 * t26 + (0.180D3 * t18 * lh + 0.90D2 * t18 + 0.90D2 * t52 * t
     #18) * t22 * t26 * t14 * wd / 0.5D1
      t62 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t61)
      t64 = x3 * z
      t65 = t3 * z
      t66 = x2 * x3
      t67 = t66 * z
      t68 = t3 * x2
      t69 = x2 * z
      t70 = t3 * t69
      t71 = sqrt(x2)
      t79 = Sqrt(-x3 * (-0.1D1 + t71) * (t71 + 0.1D1) * t17 * t9)
      t81 = 0.2D1 * t21 * t71 * t79
      t86 = x1 * x2
      t87 = t86 * z
      t88 = z + x1 - t16 - t69 - t86 + t87 - t64 - t3 + t65 + t67 + t68 
     #- t70 + t66 + t81
      t97 = t17 ** 2
      t98 = 0.1D1 / t97
      t101 = 0.1D1 / (z + x1 - t16 - t86 + t87)
      t102 = t71 * x2
      t103 = t102 * t39
      t106 = t71 * t44
      t109 = t71 * z
      t112 = t39 * t44
      t113 = x3 * t71
      t118 = t21 * t79
      t123 = t44 * x3
      t126 = t102 * x1
      t129 = x1 * t71
      t130 = t39 * t71
      t132 = 0.4D1 * t103 * t64 - 0.6D1 * t3 * t106 - 0.8D1 * t40 * t109
     # + 0.4D1 * t112 * t113 + 0.4D1 * t3 * t109 - 0.4D1 * t86 * t118 - 
     #0.4D1 * t16 * t118 - 0.2D1 * t103 * t123 + 0.2D1 * t126 * t123 + t
     #126 - t106 + t103 - t109 - t129 - 0.2D1 * t130
      t134 = z * t21 * t79
      t165 = 0.4D1 * t86 * t134 - 0.2D1 * t103 * z - t126 * t44 + t103 *
     # t44 + 0.4D1 * t134 + 0.4D1 * t40 * t71 + 0.2D1 * t3 * t71 - 0.2D1
     # * t129 * z + 0.4D1 * t130 * z - 0.2D1 * t112 * t71 + 0.3D1 * t129
     # * t44 + 0.4D1 * x1 * t21 * t79 + 0.2D1 * t106 * x3 - 0.2D1 * t103
     # * x3 - 0.2D1 * t126 * x3 + 0.2D1 * t113 * z
      t167 = (t132 + t165) ** 2
      t172 = FJET(XB1, XB2, s, t2 * x1 * (-t64 - t3 + t65 + t67 + t68 - 
     #t70 - x2 + t66 + t81) * t18, -t8, -t2 * x1 * t88 * t18, t13, s * t
     #14 * x2 * x1 * t6 * t18, -0.9D1 / 0.8D1 * t15 * t98 * t101 * t167 
     #* t28)
      rrgg2gght13s1e1 = t62 * t61 - 0.9D1 / 0.8D1 * t172 * t14 * wd * t9
     #8 * t101 * t167 * t28

      end function



      doubleprecision function rrgg2gght13s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t7 = t1 * (-0.1D1 + x1)
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t14 = t1 ** 2
      t17 = -z - x1 + x1 * z
      t20 = cos(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t25 = Sqrt(x3 * t17 * t9)
      t26 = t25 ** 2
      t27 = 0.1D1 / t17 * t21 * t26
      t30 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, -0.18D2 * t14 * wd * t27)
      rrgg2gght13s1e0 = -0.18D2 * t30 * t14 * wd * t27

      end function



      doubleprecision function rrgg2gght13s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gght13s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght13s1em4 = 0.0D0

      end function
