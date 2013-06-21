  
      subroutine bbbbH6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH61J1  
      doubleprecision bbbbH61J2  
      doubleprecision bbbbH61J3  
      doubleprecision bbbbH6n1e1  
      doubleprecision bbbbH6n1e0  
      doubleprecision bbbbH6n1em1  
      doubleprecision bbbbH6n1em2  
      doubleprecision bbbbH6n1em3  
      doubleprecision bbbbH6n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH6n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH61J1
      doubleprecision bbbbH61J2
      doubleprecision bbbbH61J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t11 = x2 * t7 * x3 * t9
      t12 = Sqrt(t11)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t21 = t19 * t20
      t22 = x2 * z
      t24 = (0.1D1 + t22 - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = bbbbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t28 = x1 ** 2
      t29 = Sin(t5)
      t30 = t29 ** 2
      t31 = t28 * t30
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t37 = log(0.4D1 * t31 * t33 * t11)
      t39 = bbbbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t44 = lh * t19
      t45 = t20 * t25
      t46 = t45 * t39
      t50 = 0.1D1 / x1
      t53 = bbbbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t60 = t7 * x3
      t64 = log(0.4D1 * t30 * t33 * x2 * t60 * t9)
      t73 = lh ** 2
      t75 = 0.3141592653589793D1 ** 2
      t77 = t64 ** 2
      t83 = -(0.90D2 * t21 * (t25 * t26 - t37 * t25 * t39) - 0.180D3 * t
     #44 * t46) * t50 / 0.2880D4 - t21 * t25 * t53 / 0.64D2 - (-0.180D3 
     #* lh - 0.90D2 * t64) * t19 * t45 * t26 / 0.5760D4 - (0.180D3 * t64
     # * lh + 0.180D3 * t73 - 0.30D2 * t75 + 0.45D2 * t77) * t19 * t46 /
     # 0.5760D4
      t84 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t83)
      t86 = x3 * x1
      t87 = t2 * t86
      t88 = -0.1D1 + x1
      t89 = t86 * z
      t90 = x1 * t3
      t91 = x1 * z
      t92 = t3 * t91
      t93 = 0.1D1 - x1 + t91
      t97 = Sqrt(t60 * t93 * x2 * t9)
      t99 = 0.2D1 * t6 * t97
      t102 = 0.1D1 / t93
      t104 = t2 * t88 * (-x3 + t86 - t89 + t4 - t90 + t92 - x2 + t99) * 
     #t102
      t106 = t2 * x1 * t9
      t107 = x2 * x1
      t108 = t107 * z
      t109 = 0.1D1 - x1 + t91 - x2 + t107 - t108 - x3 + t86 - t89 + t4 -
     # t90 + t92 + t99
      t112 = t2 * t88 * t109 * t102
      t117 = s * t19 * x2 * x1 * t88 * t102
      t118 = t93 * t88
      t120 = (-0.1D1 - t22 - t107 + x1 + x2 - t91 + t108) ** 2
      t121 = 0.1D1 / t120
      t122 = -t88
      t123 = bbbbH61J2(s, XB1, XB2, z, lh, wd, t122, x2, x3, x4)
      t129 = t88 ** 2
      t134 = log(0.4D1 * t31 * t33 * x2 * t60 * t9 * t102 * t129)
      t137 = bbbbH61J1(s, XB1, XB2, z, lh, wd, t122, x2, x3, x4)
      t148 = 0.90D2 * t21 * (t118 * t121 * t123 - t134 * t93 * t88 * t12
     #1 * t137) - 0.180D3 * t44 * t20 * t118 * t121 * t137
      t150 = t148 * t50 / 0.2880D4
      t151 = FJET(XB1, XB2, s, t87, t104, -t106, -t112, -t117, -t150)
      t155 = FJET(XB1, XB2, s, t104, t87, -t112, -t106, -t117, -t150)
      t159 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t83)
      bbbbH6n1e1 = t84 * t83 - t151 * t148 * t50 / 0.2880D4 - t155 * t14
     #8 * t50 / 0.2880D4 + t159 * t83

      end function



      doubleprecision function bbbbH6n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH61J1
      doubleprecision bbbbH61J2
      doubleprecision bbbbH61J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t21 = t19 * t20
      t22 = x2 * z
      t24 = (0.1D1 + t22 - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = bbbbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t28 = 0.1D1 / x1
      t32 = bbbbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t37 = Sin(t5)
      t38 = t37 ** 2
      t39 = z ** 2
      t43 = t7 * x3
      t47 = log(0.4D1 * t38 / t39 * x2 * t43 * t9)
      t55 = -t21 * t25 * t26 * t28 / 0.32D2 - t21 * t25 * t32 / 0.64D2 -
     # (-0.180D3 * lh - 0.90D2 * t47) * t19 * t20 * t25 * t26 / 0.5760D4
      t56 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t55)
      t58 = x3 * x1
      t59 = t2 * t58
      t60 = -0.1D1 + x1
      t61 = t58 * z
      t62 = x1 * t3
      t63 = x1 * z
      t64 = t3 * t63
      t65 = 0.1D1 - x1 + t63
      t69 = Sqrt(t43 * t65 * x2 * t9)
      t71 = 0.2D1 * t6 * t69
      t74 = 0.1D1 / t65
      t76 = t2 * t60 * (-x3 + t58 - t61 + t4 - t62 + t64 - x2 + t71) * t
     #74
      t78 = t2 * x1 * t9
      t79 = x2 * x1
      t80 = t79 * z
      t81 = 0.1D1 - x1 + t63 - x2 + t79 - t80 - x3 + t58 - t61 + t4 - t6
     #2 + t64 + t71
      t84 = t2 * t60 * t81 * t74
      t89 = s * t19 * x2 * x1 * t60 * t74
      t92 = (-0.1D1 - t22 - t79 + x1 + x2 - t63 + t80) ** 2
      t96 = bbbbH61J1(s, XB1, XB2, z, lh, wd, -t60, x2, x3, x4)
      t98 = t60 / t92 * t96 * t28
      t100 = t21 * t65 * t98 / 0.32D2
      t101 = FJET(XB1, XB2, s, t59, t76, -t78, -t84, -t89, -t100)
      t103 = t20 * t65
      t107 = FJET(XB1, XB2, s, t76, t59, -t84, -t78, -t89, -t100)
      t112 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t55)
      bbbbH6n1e0 = t56 * t55 - t101 * t19 * t103 * t98 / 0.32D2 - t107 *
     # t19 * t103 * t98 / 0.32D2 + t112 * t55

      end function



      doubleprecision function bbbbH6n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH61J1
      doubleprecision bbbbH61J2
      doubleprecision bbbbH61J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t24 = (0.1D1 + x2 * z - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = bbbbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t29 = t19 * t20 * t25 * t26 / 0.64D2
      t30 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, -t29)
      t33 = t20 * t25 * t26
      t35 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, -t29)
      bbbbH6n1em1 = -t30 * t19 * t33 / 0.64D2 - t35 * t19 * t33 / 0.64D2

      end function



      doubleprecision function bbbbH6n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH61J1
      doubleprecision bbbbH61J2
      doubleprecision bbbbH61J3
      bbbbH6n1em2 = 0.0D0

      end function



      doubleprecision function bbbbH6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH61J1
      doubleprecision bbbbH61J2
      doubleprecision bbbbH61J3
      bbbbH6n1em3 = 0.0D0

      end function



      doubleprecision function bbbbH6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH61J1
      doubleprecision bbbbH61J2
      doubleprecision bbbbH61J3
      bbbbH6n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bbbbH61J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbbbH61J1 = -0.32D2 * wd * s * z

      end function
  
   
 

      doubleprecision function bbbbH61J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + x1 * t1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      bbbbH61J2 = 0.32D2 / 0.3D1 * wd * (s - t2 * t6 * (t8 * t4 + x2 * t
     #10 - t19) - t2 * t6 * (t10 * t7 * t4 + x2 * x3 + t19)) + 0.32D2 * 
     #wd * s * z

      end function
  
   
 

      doubleprecision function bbbbH61J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + x1 * t1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      bbbbH61J3 = -0.32D2 / 0.3D1 * wd * (s - t2 * t6 * (t8 * t4 + x2 * 
     #t10 - t19) - t2 * t6 * (t10 * t7 * t4 + x2 * x3 + t19))

      end function
  
 