  
      subroutine rrqqbar2gght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2gght6s1e1  
      doubleprecision rrqqbar2gght6s1e0  
      doubleprecision rrqqbar2gght6s1em1  
      doubleprecision rrqqbar2gght6s1em2  
      doubleprecision rrqqbar2gght6s1em3  
      doubleprecision rrqqbar2gght6s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x3 * x1
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t16 * wd
      t18 = t1 ** 2
      t19 = t18 * t1
      t21 = t17 * t19 * x1
      t23 = t5 / z
      t25 = x4 * 0.3141592653589793D1
      t26 = Sin(t25)
      t27 = t26 ** 2
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t27 * t29
      t32 = t18 ** 2
      t33 = x1 ** 2
      t35 = t5 ** 2
      t41 = log(0.4D1 * x2 * t16 * t30 * t32 * t33 * t35 * x3 * t8)
      t42 = cos(t25)
      t43 = t42 ** 2
      t47 = Sqrt(x3 * t15 * t8)
      t48 = t47 ** 2
      t54 = t16 * lh
      t59 = (0.90D2 * t17 + 0.180D3 * t54 * wd) * t19 * x1
      t61 = t23 * t43 * t48
      t65 = 0.1D1 / x2
      t72 = t33 * t35
      t77 = log(0.4D1 * t16 * t27 * t29 * t32 * t72 * x3 * t8)
      t78 = t77 * t16
      t84 = t77 ** 2
      t87 = lh ** 2
      t89 = 0.3141592653589793D1 ** 2
      t100 = 0.4D1 / 0.135D3 * (0.360D3 * t21 * t23 * t41 * t43 * t48 + 
     #0.4D1 * t59 * t61) * t65 + 0.16D2 / 0.135D3 * (-(0.180D3 * t54 + 0
     #.90D2 * t78) * wd + (-0.180D3 * t78 * lh - 0.45D2 * t84 * t16 + t1
     #6 * (-0.180D3 * t87 + 0.30D2 * t89)) * wd) * t19 * x1 * t61
      t101 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, 
     #t100)
      t103 = x3 * z
      t104 = t3 * z
      t105 = x2 * x3
      t106 = t105 * z
      t107 = t105 * x1
      t108 = t105 * t14
      t109 = sqrt(x2)
      t111 = -0.1D1 + t109
      t113 = t109 + 0.1D1
      t117 = Sqrt(-x3 * t111 * t113 * t15 * t8)
      t119 = 0.2D1 * t42 * t109 * t117
      t125 = x2 * x1
      t127 = z + x1 - t14 - x2 * z - t125 + t125 * z - t103 - t3 + t104 
     #+ t106 + t107 - t108 + t105 + t119
      t145 = log(-0.4D1 * t16 * t32 * t72 * t111 * t113 * x3 * t8 * t30 
     #* x2)
      t151 = (0.2D1 * t109 * x3 - t109 + 0.2D1 * t42 * t117) ** 2
      t158 = -0.90D2 * t21 * t23 * t145 * t151 - t59 * t23 * t151
      t161 = FJET(XB1, XB2, s, t2 * x1 * (-t103 - t3 + t104 + t106 + t10
     #7 - t108 - x2 + t105 + t119) * t16, -t7, -t2 * x1 * t127 * t16, t1
     #3, s * t18 * x2 * x1 * t5 * t16, 0.4D1 / 0.135D3 * t158 * t65)
      rrqqbar2gght6s1e1 = t101 * t100 + 0.4D1 / 0.135D3 * t161 * t158 * 
     #t65

      end function



      doubleprecision function rrqqbar2gght6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x3 * x1
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t16 * wd
      t18 = t1 ** 2
      t19 = t18 * t1
      t21 = t17 * t19 * x1
      t22 = 0.1D1 / z
      t23 = t5 * t22
      t24 = x4 * 0.3141592653589793D1
      t25 = cos(t24)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t15 * t8)
      t30 = t29 ** 2
      t31 = t26 * t30
      t32 = 0.1D1 / x2
      t40 = Sin(t24)
      t41 = t40 ** 2
      t43 = z ** 2
      t45 = t18 ** 2
      t48 = x1 ** 2
      t49 = t5 ** 2
      t55 = log(0.4D1 * t16 * t41 / t43 * t45 * t48 * t49 * x3 * t8)
      t66 = -0.32D2 / 0.3D1 * t21 * t23 * t31 * t32 + 0.16D2 / 0.135D3 *
     # (0.90D2 * t17 + (0.180D3 * t16 * lh + 0.90D2 * t55 * t16) * wd) *
     # t19 * x1 * t23 * t31
      t67 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, t
     #66)
      t69 = x3 * z
      t70 = t3 * z
      t71 = x2 * x3
      t72 = t71 * z
      t73 = t71 * x1
      t74 = t71 * t14
      t75 = sqrt(x2)
      t83 = Sqrt(-x3 * (-0.1D1 + t75) * (t75 + 0.1D1) * t15 * t8)
      t85 = 0.2D1 * t25 * t75 * t83
      t91 = x2 * x1
      t93 = z + x1 - t14 - x2 * z - t91 + t91 * z - t69 - t3 + t70 + t72
     # + t73 - t74 + t71 + t85
      t99 = x1 * t5
      t107 = (0.2D1 * t75 * x3 - t75 + 0.2D1 * t25 * t83) ** 2
      t112 = FJET(XB1, XB2, s, t2 * x1 * (-t69 - t3 + t70 + t72 + t73 - 
     #t74 - x2 + t71 + t85) * t16, -t7, -t2 * x1 * t93 * t16, t13, s * t
     #18 * x2 * t99 * t16, 0.8D1 / 0.3D1 * t21 * t23 * t107 * t32)
      rrqqbar2gght6s1e0 = t67 * t66 + 0.8D1 / 0.3D1 * t112 * t16 * wd * 
     #t19 * t99 * t22 * t107 * t32

      end function



      doubleprecision function rrqqbar2gght6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t15 = -z - x1 + x1 * z
      t16 = 0.1D1 / t15
      t18 = t1 ** 2
      t19 = t18 * t1
      t22 = 0.1D1 / z
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t15 * t8)
      t30 = t29 ** 2
      t35 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t5 * x3, -t9 * t1 * x1
     #, t9 * t1 * t5, 0.0D0, -0.32D2 / 0.3D1 * t16 * wd * t19 * x1 * t5 
     #* t22 * t26 * t30)
      rrqqbar2gght6s1em1 = -0.32D2 / 0.3D1 * t35 * t16 * wd * t19 * x1 *
     # t5 * t22 * t26 * t30

      end function



      doubleprecision function rrqqbar2gght6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2gght6s1em2 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2gght6s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2gght6s1em4 = 0.0D0

      end function
