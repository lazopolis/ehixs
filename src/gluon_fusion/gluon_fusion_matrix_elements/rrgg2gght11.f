  
      subroutine rrgg2gght11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght11s1e1  
      doubleprecision rrgg2gght11s1e0  
      doubleprecision rrgg2gght11s1em1  
      doubleprecision rrgg2gght11s1em2  
      doubleprecision rrgg2gght11s1em3  
      doubleprecision rrgg2gght11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght11s1e1
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
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t16 * x1
      t18 = t1 ** 2
      t19 = t18 ** 2
      t21 = t17 * t6 * t19
      t22 = 0.1D1 / z
      t23 = wd * t22
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t27 = t16 * t26
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t6 ** 2
      t33 = x1 ** 2
      t34 = t33 * x3
      t39 = log(0.4D1 * t27 * t29 * t30 * t34 * x2 * t9)
      t40 = cos(t24)
      t41 = t40 ** 2
      t45 = Sqrt(x3 * t15 * t9)
      t46 = t45 ** 2
      t52 = t17 * t6 * lh
      t53 = t19 * wd
      t60 = 0.1D1 / x2
      t69 = log(0.4D1 * t34 * t26 * t29 * t16 * t30 * t9)
      t71 = x1 * t6
      t75 = t69 ** 2
      t79 = lh ** 2
      t81 = 0.3141592653589793D1 ** 2
      t93 = (0.360D3 * t21 * t23 * t39 * t41 * t46 + 0.720D3 * t52 * t53
     # * t22 * t41 * t46) * t60 / 0.40D2 + (-0.180D3 * t69 * t16 * t71 *
     # lh - 0.45D2 * t75 * t16 * t71 + t17 * t6 * (-0.180D3 * t79 + 0.30
     #D2 * t81)) * t19 * t41 * t46 * wd * t22 / 0.10D2
      t94 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t93)
      t96 = x3 * z
      t97 = t3 * z
      t98 = x2 * x3
      t99 = t98 * z
      t100 = t3 * x2
      t101 = x2 * z
      t102 = t3 * t101
      t103 = sqrt(x2)
      t105 = -0.1D1 + t103
      t107 = t103 + 0.1D1
      t111 = Sqrt(-x3 * t105 * t107 * t15 * t9)
      t113 = 0.2D1 * t40 * t103 * t111
      t118 = x1 * x2
      t120 = z + x1 - t14 - t101 - t118 + t118 * z - t96 - t3 + t97 + t9
     #9 + t100 - t102 + t98 + t113
      t136 = log(-0.4D1 * t27 * t33 * t30 * t29 * t105 * t107 * t98 * t9
     #)
      t142 = (-t103 + 0.2D1 * t40 * t111 + 0.2D1 * t103 * x3) ** 2
      t151 = -0.90D2 * t21 * t23 * t136 * t142 - 0.180D3 * t52 * t53 * t
     #22 * t142
      t154 = FJET(XB1, XB2, s, t2 * x1 * (-t96 - t3 + t97 + t99 + t100 -
     # t102 - x2 + t98 + t113) * t16, -t8, -t2 * x1 * t120 * t16, t13, s
     # * t18 * x2 * t71 * t16, t151 * t60 / 0.40D2)
      rrgg2gght11s1e1 = t94 * t93 + t154 * t151 * t60 / 0.40D2

      end function



      doubleprecision function rrgg2gght11s1e0
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
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t16 * x1
      t18 = t1 ** 2
      t19 = t18 ** 2
      t21 = t17 * t6 * t19
      t22 = 0.1D1 / z
      t23 = wd * t22
      t24 = x4 * 0.3141592653589793D1
      t25 = cos(t24)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t15 * t9)
      t30 = t29 ** 2
      t32 = 0.1D1 / x2
      t40 = x1 ** 2
      t42 = Sin(t24)
      t43 = t42 ** 2
      t45 = z ** 2
      t48 = t6 ** 2
      t53 = log(0.4D1 * x3 * t40 * t43 / t45 * t16 * t48 * t9)
      t55 = x1 * t6
      t65 = -0.9D1 * t21 * t23 * t26 * t30 * t32 + (0.180D3 * t17 * t6 *
     # lh + 0.90D2 * t53 * t16 * t55) * t19 * t26 * t30 * wd * t22 / 0.1
     #0D2
      t66 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t65)
      t68 = x3 * z
      t69 = t3 * z
      t70 = x2 * x3
      t71 = t70 * z
      t72 = t3 * x2
      t73 = x2 * z
      t74 = t3 * t73
      t75 = sqrt(x2)
      t83 = Sqrt(-x3 * (-0.1D1 + t75) * (t75 + 0.1D1) * t15 * t9)
      t85 = 0.2D1 * t25 * t75 * t83
      t90 = x1 * x2
      t92 = z + x1 - t14 - t73 - t90 + t90 * z - t68 - t3 + t69 + t71 + 
     #t72 - t74 + t70 + t85
      t105 = (-t75 + 0.2D1 * t25 * t83 + 0.2D1 * t75 * x3) ** 2
      t110 = FJET(XB1, XB2, s, t2 * x1 * (-t68 - t3 + t69 + t71 + t72 - 
     #t74 - x2 + t70 + t85) * t16, -t8, -t2 * x1 * t92 * t16, t13, s * t
     #18 * x2 * t55 * t16, 0.9D1 / 0.4D1 * t21 * t23 * t105 * t32)
      rrgg2gght11s1e0 = t66 * t65 + 0.9D1 / 0.4D1 * t110 * t16 * t55 * t
     #19 * wd * t22 * t105 * t32

      end function



      doubleprecision function rrgg2gght11s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t15 = -z - x1 + x1 * z
      t16 = 0.1D1 / t15
      t18 = t1 ** 2
      t19 = t18 ** 2
      t22 = 0.1D1 / z
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t15 * t9)
      t30 = t29 ** 2
      t35 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, -0.9D1 * t16 * x1 * t6 * t19 * wd * t22 * t
     #26 * t30)
      rrgg2gght11s1em1 = -0.9D1 * t35 * t16 * x1 * t6 * t19 * wd * t22 *
     # t26 * t30

      end function



      doubleprecision function rrgg2gght11s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght11s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght11s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght11s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght11s1em4 = 0.0D0

      end function
