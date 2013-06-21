  
      subroutine rrqg2qght11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght11s1e1  
      doubleprecision rrqg2qght11s1e0  
      doubleprecision rrqg2qght11s1em1  
      doubleprecision rrqg2qght11s1em2  
      doubleprecision rrqg2qght11s1em3  
      doubleprecision rrqg2qght11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght11s1e1
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
      t4 = -0.1D1 + x3
      t7 = 0.2D1 * x3 - 0.1D1
      t8 = z * t7
      t9 = x2 * x3
      t10 = t1 ** 2
      t11 = t10 ** 2
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t19 = t18 * t4
      t22 = log(-0.4D1 * t9 * t11 * t19)
      t23 = t22 ** 2
      t29 = 0.180D3 * t8 * wd * lh
      t30 = t8 * wd
      t31 = 0.90D2 * t30
      t32 = -t29 - t31
      t34 = lh ** 2
      t36 = 0.3141592653589793D1 ** 2
      t38 = 0.180D3 * t34 - 0.30D2 * t36
      t40 = t8 * wd * t38
      t42 = 0.1D1 / x2
      t56 = log(-0.4D1 * x3 * t11 * t19)
      t57 = t56 ** 2
      t59 = t7 * wd
      t60 = t57 * z * t59
      t67 = t56 * z * t59
      t75 = x1 ** 2
      t77 = t11 * t15
      t82 = log(-0.4D1 * t9 * t75 * t77 * t17 * t4)
      t88 = 0.1D1 / x1
      t91 = x3 * t75
      t95 = log(-0.4D1 * t91 * t11 * t19)
      t96 = t95 ** 2
      t104 = (-0.45D2 * t8 * wd * t23 + t32 * t22 - t29 - t40) * t42 / 0
     #.810D3 - t8 * wd * (0.60D2 * lh * t36 - 0.2884936567583026D3 - 0.1
     #20D3 * t34 * lh) / 0.810D3 + t60 / 0.18D2 + t57 * t56 * z * t59 / 
     #0.54D2 - (-t30 - t67) * t38 / 0.810D3 + 0.2D1 / 0.9D1 * (t67 + t60
     # / 0.2D1) * lh - (-0.90D2 * t8 * wd * t82 - t29 - t31) * t42 * t88
     # / 0.405D3 + (-0.45D2 * t8 * wd * t96 + t32 * t95 - t29 - t40) * t
     #88 / 0.405D3
      t105 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #104)
      t107 = 0.2D1 * t9
      t108 = cos(t13)
      t109 = -0.1D1 + x2
      t113 = Sqrt(x2 * t109 * x3 * t4)
      t115 = 0.2D1 * t108 * t113
      t123 = t17 * t11 * t4 * t109
      t126 = log(0.4D1 * t9 * t15 * t123)
      t127 = t126 ** 2
      t129 = x2 * z
      t131 = (0.1D1 + t129 - x2) ** 2
      t132 = 0.1D1 / t131
      t143 = t75 * t15
      t147 = log(0.4D1 * t9 * t143 * t123)
      t157 = (0.45D2 * t8 * wd * t127 * t132 - t32 * t126 * t132 + (t29 
     #+ t40) * t132) * t42 / 0.810D3 - (0.90D2 * t8 * wd * t147 * t132 -
     # t32 * t132) * t42 * t88 / 0.405D3
      t158 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t107 - x2 + t115), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t107 + t115), 0.0D0, t157)
      t160 = x3 * x1
      t161 = t2 * t160
      t162 = -0.1D1 + x1
      t163 = t160 * z
      t164 = t9 * x1
      t165 = x1 * z
      t166 = t9 * t165
      t168 = 0.1D1 - x1 + t165
      t172 = Sqrt(x3 * t109 * t168 * x2 * t4)
      t174 = 0.2D1 * t108 * t172
      t177 = 0.1D1 / t168
      t180 = t4 * s
      t182 = t180 * t1 * x1
      t183 = x2 * x1
      t184 = t183 * z
      t185 = 0.1D1 - x1 + t165 - x2 + t183 - t184 - x3 + t160 - t163 + t
     #107 - t164 + t166 + t174
      t197 = t162 ** 2
      t203 = log(0.4D1 * t9 * t143 * t17 * t11 * t177 * t197 * t109 * t4
     #)
      t204 = t168 ** 2
      t207 = (-0.1D1 + x1 - t129 + t184 + x2 - t183 - t165) ** 2
      t208 = 0.1D1 / t207
      t214 = -0.90D2 * t30 * t203 * t204 * t208 + t32 * t204 * t208
      t218 = FJET(XB1, XB2, s, t161, t2 * t162 * (-x3 + t160 - t163 + t1
     #07 - t164 + t166 - x2 + t174) * t177, -t182, -t2 * t162 * t185 * t
     #177, -s * t10 * x2 * x1 * t162 * t177, -t214 * t42 * t88 / 0.405D3
     #)
      t224 = t1 * t162
      t234 = log(-0.4D1 * t9 * t75 * t11 * t18 * t177 * t197 * t4)
      t247 = log(-0.4D1 * t91 * t77 * t17 * t177 * t197 * t4)
      t248 = t247 ** 2
      t256 = -(0.90D2 * t8 * wd * t234 + t29 + t31) * t42 * t88 / 0.405D
     #3 + (0.45D2 * t8 * wd * t248 - t32 * t247 + t29 + t40) * t88 / 0.4
     #05D3
      t257 = FJET(XB1, XB2, s, t161, -x3 * s * t224, -t182, t180 * t224,
     # 0.0D0, t256)
      rrqg2qght11s1e1 = t105 * t104 + t158 * t157 - t218 * t214 * t42 * 
     #t88 / 0.405D3 + t257 * t256

      end function



      doubleprecision function rrqg2qght11s1e0
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
      t4 = -0.1D1 + x3
      t7 = 0.2D1 * x3 - 0.1D1
      t8 = z * t7
      t9 = x2 * x3
      t10 = t1 ** 2
      t11 = t10 ** 2
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t19 = t15 * t17 * t4
      t22 = log(-0.4D1 * t9 * t11 * t19)
      t28 = 0.180D3 * t8 * wd * lh
      t29 = t8 * wd
      t30 = 0.90D2 * t29
      t32 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t39 = 0.2D1 / 0.9D1 * t8 * wd * t32 * t36
      t40 = x1 ** 2
      t41 = x3 * t40
      t45 = log(-0.4D1 * t41 * t11 * t19)
      t55 = log(-0.4D1 * x3 * t11 * t19)
      t57 = t7 * wd
      t58 = t55 * z * t57
      t63 = t55 ** 2
      t67 = lh ** 2
      t69 = 0.3141592653589793D1 ** 2
      t75 = (0.90D2 * t8 * wd * t22 + t28 + t30) * t32 / 0.810D3 - t39 +
     # (0.90D2 * t8 * wd * t45 + t28 + t30) * t36 / 0.405D3 + 0.2D1 / 0.
     #9D1 * (-t29 - t58) * lh - t58 / 0.9D1 - t63 * z * t57 / 0.18D2 - t
     #8 * wd * (0.180D3 * t67 - 0.30D2 * t69) / 0.810D3
      t76 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t7
     #5)
      t78 = 0.2D1 * t9
      t79 = cos(t13)
      t80 = -0.1D1 + x2
      t84 = Sqrt(x2 * t80 * x3 * t4)
      t86 = 0.2D1 * t79 * t84
      t97 = log(0.4D1 * t9 * t15 * t17 * t11 * t4 * t80)
      t99 = x2 * z
      t101 = (0.1D1 + t99 - x2) ** 2
      t102 = 0.1D1 / t101
      t115 = (-0.90D2 * t8 * wd * t97 * t102 + (-t28 - t30) * t102) * t3
     #2 / 0.810D3 + 0.2D1 / 0.9D1 * t29 * t102 * t32 * t36
      t116 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t78 - x2 + t86), 0.0D
     #0, t2 * (0.1D1 - x2 - x3 + t78 + t86), 0.0D0, t115)
      t118 = x3 * x1
      t119 = t2 * t118
      t120 = -0.1D1 + x1
      t121 = t118 * z
      t122 = t9 * x1
      t123 = x1 * z
      t124 = t9 * t123
      t126 = 0.1D1 - x1 + t123
      t130 = Sqrt(x3 * t80 * t126 * x2 * t4)
      t132 = 0.2D1 * t79 * t130
      t135 = 0.1D1 / t126
      t138 = t4 * s
      t140 = t138 * t1 * x1
      t141 = x2 * x1
      t142 = t141 * z
      t143 = 0.1D1 - x1 + t123 - x2 + t141 - t142 - x3 + t118 - t121 + t
     #78 - t122 + t124 + t132
      t152 = t126 ** 2
      t154 = (-0.1D1 + x1 - t99 + t142 + x2 - t141 - t123) ** 2
      t158 = t152 / t154 * t32 * t36
      t161 = FJET(XB1, XB2, s, t119, t2 * t120 * (-x3 + t118 - t121 + t7
     #8 - t122 + t124 - x2 + t132) * t135, -t140, -t2 * t120 * t143 * t1
     #35, -s * t10 * x2 * x1 * t120 * t135, -0.2D1 / 0.9D1 * t29 * t158)
      t167 = t1 * t120
      t173 = t120 ** 2
      t178 = log(-0.4D1 * t41 * t11 * t15 * t17 * t135 * t173 * t4)
      t185 = t39 + (-0.90D2 * t8 * wd * t178 - t28 - t30) * t36 / 0.405D
     #3
      t186 = FJET(XB1, XB2, s, t119, -x3 * s * t167, -t140, t138 * t167,
     # 0.0D0, t185)
      rrqg2qght11s1e0 = t76 * t75 + t116 * t115 - 0.2D1 / 0.9D1 * t161 *
     # z * t57 * t158 + t186 * t185

      end function



      doubleprecision function rrqg2qght11s1em1
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
      t4 = -0.1D1 + x3
      t7 = 0.2D1 * x3 - 0.1D1
      t8 = z * t7
      t9 = 0.1D1 / x1
      t12 = 0.2D1 / 0.9D1 * t8 * wd * t9
      t18 = t1 ** 2
      t19 = t18 ** 2
      t21 = x4 * 0.3141592653589793D1
      t22 = Sin(t21)
      t23 = t22 ** 2
      t24 = z ** 2
      t30 = log(-0.4D1 * x3 * t19 * t23 / t24 * t4)
      t32 = t7 * wd
      t35 = 0.1D1 / x2
      t39 = -t12 + 0.2D1 / 0.9D1 * t8 * wd * lh + t8 * wd / 0.9D1 + t30 
     #* z * t32 / 0.9D1 - t8 * wd * t35 / 0.9D1
      t40 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t3
     #9)
      t46 = t1 * (-0.1D1 + x1)
      t48 = t4 * s
      t52 = FJET(XB1, XB2, s, t2 * x1 * x3, -x3 * s * t46, -t48 * t1 * x
     #1, t48 * t46, 0.0D0, t12)
      t58 = 0.2D1 * x2 * x3
      t59 = cos(t21)
      t64 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t66 = 0.2D1 * t59 * t64
      t73 = (0.1D1 + x2 * z - x2) ** 2
      t76 = wd / t73 * t35
      t79 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t58 - x2 + t66), 0.0D0
     #, t2 * (0.1D1 - x2 - x3 + t58 + t66), 0.0D0, t8 * t76 / 0.9D1)
      rrqg2qght11s1em1 = t40 * t39 + 0.2D1 / 0.9D1 * t52 * z * t32 * t9 
     #+ t79 * z * t7 * t76 / 0.9D1

      end function



      doubleprecision function rrqg2qght11s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t7 = 0.2D1 * x3 - 0.1D1
      t11 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, -z * t7 * wd / 0.9D1)
      rrqg2qght11s1em2 = -t11 * z * t7 * wd / 0.9D1

      end function



      doubleprecision function rrqg2qght11s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght11s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght11s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght11s1em4 = 0.0D0

      end function
