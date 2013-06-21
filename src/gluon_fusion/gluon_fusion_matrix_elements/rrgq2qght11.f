  
      subroutine rrgq2qght11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght11s1e1  
      doubleprecision rrgq2qght11s1e0  
      doubleprecision rrgq2qght11s1em1  
      doubleprecision rrgq2qght11s1em2  
      doubleprecision rrgq2qght11s1em3  
      doubleprecision rrgq2qght11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght11s1e1
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
      t23 = t22 ** 2
      t27 = t8 * wd
      t28 = 0.90D2 * t27
      t29 = lh * z
      t33 = (0.180D3 * z + 0.180D3 * t29) * t7 * wd
      t34 = -t28 + t33
      t36 = 0.90D2 * z
      t38 = lh ** 2
      t39 = 0.180D3 * t38
      t40 = 0.3141592653589793D1 ** 2
      t41 = 0.30D2 * t40
      t42 = -t39 + t41
      t46 = (-t36 - 0.360D3 * t29 + t42 * z) * t7 * wd
      t48 = 0.1D1 / x2
      t57 = log(-0.4D1 * x3 * t11 * t19)
      t60 = (0.180D3 * lh + 0.90D2 * t57) * z
      t68 = t57 ** 2
      t71 = (-0.180D3 * t57 * lh - 0.45D2 * t68 - t39 + t41) * z
      t92 = x1 ** 2
      t93 = x2 * t92
      t95 = t17 * t11
      t96 = x3 * t4
      t100 = log(-0.4D1 * t93 * t15 * t95 * t96)
      t106 = 0.1D1 / x1
      t109 = x3 * t92
      t113 = log(-0.4D1 * t109 * t11 * t19)
      t114 = t113 ** 2
      t122 = (-0.45D2 * t8 * wd * t23 - t22 * t34 - t28 + t33 + t46) * t
     #48 / 0.810D3 - t27 / 0.9D1 + (0.180D3 * z + t60) * t7 * wd / 0.810
     #D3 + (-t36 - 0.2D1 * t60 + t71) * t7 * wd / 0.810D3 + (t60 - 0.2D1
     # * t71 + (0.90D2 * t68 * lh - 0.60D2 * lh * t40 + 0.28849365675830
     #26D3 + 0.120D3 * t38 * lh + 0.15D2 * t68 * t57 - t57 * t42) * z) *
     # t7 * wd / 0.810D3 - (-0.90D2 * t8 * wd * t100 + t28 - t33) * t48 
     #* t106 / 0.405D3 + (-0.45D2 * t8 * wd * t114 - t34 * t113 - t28 + 
     #t33 + t46) * t106 / 0.405D3
      t123 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #122)
      t125 = -0.1D1 + x1
      t126 = x3 * x1
      t127 = t126 * z
      t128 = 0.2D1 * t9
      t129 = t9 * x1
      t130 = x1 * z
      t131 = t9 * t130
      t132 = cos(t13)
      t133 = -0.1D1 + x2
      t134 = x3 * t133
      t135 = 0.1D1 - x1 + t130
      t139 = Sqrt(t134 * t135 * x2 * t4)
      t141 = 0.2D1 * t132 * t139
      t144 = 0.1D1 / t135
      t147 = t2 * t126
      t148 = x2 * x1
      t149 = t148 * z
      t150 = 0.1D1 - x1 + t130 - x2 + t148 - t149 - x3 + t126 - t127 + t
     #128 - t129 + t131 + t141
      t154 = t4 * s
      t156 = t154 * t1 * x1
      t162 = t92 * t15
      t166 = t125 ** 2
      t172 = log(0.4D1 * t9 * t162 * t17 * t11 * t144 * t166 * t133 * t4
     #)
      t173 = x2 * z
      t175 = (-0.1D1 + x1 - t130 + x2 - t148 - t173 + t149) ** 2
      t176 = 0.1D1 / t175
      t178 = t135 ** 2
      t184 = -0.90D2 * t27 * t172 * t176 * t178 - t34 * t176 * t178
      t188 = FJET(XB1, XB2, s, t2 * t125 * (-x3 + t126 - t127 + t128 - t
     #129 + t131 - x2 + t141) * t144, t147, -t2 * t125 * t150 * t144, -t
     #156, -s * t10 * x2 * x1 * t125 * t144, -t184 * t48 * t106 / 0.405D
     #3)
      t195 = Sqrt(t134 * x2 * t4)
      t197 = 0.2D1 * t132 * t195
      t204 = t95 * t133 * t4
      t207 = log(0.4D1 * t9 * t15 * t204)
      t208 = t207 ** 2
      t211 = (0.1D1 - x2 + t173) ** 2
      t212 = 0.1D1 / t211
      t226 = log(0.4D1 * t9 * t162 * t204)
      t236 = (0.45D2 * t8 * wd * t208 * t212 + t34 * t207 * t212 - (-t28
     # + t33 + t46) * t212) * t48 / 0.810D3 - (0.90D2 * t8 * wd * t226 *
     # t212 + t34 * t212) * t48 * t106 / 0.405D3
      t237 = FJET(XB1, XB2, s, -t2 * (-x3 + t128 - x2 + t197), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t128 + t197), 0.0D0, 0.0D0, t236)
      t240 = t1 * t125
      t249 = log(-0.4D1 * t144 * t15 * t95 * t93 * t96 * t166)
      t263 = log(-0.4D1 * t109 * t11 * t15 * t17 * t144 * t166 * t4)
      t264 = t263 ** 2
      t272 = -(0.90D2 * t8 * wd * t249 - t28 + t33) * t48 * t106 / 0.405
     #D3 + (0.45D2 * t8 * wd * t264 + t34 * t263 + t28 - t33 - t46) * t1
     #06 / 0.405D3
      t273 = FJET(XB1, XB2, s, -x3 * s * t240, t147, t154 * t240, -t156,
     # 0.0D0, t272)
      rrgq2qght11s1e1 = t123 * t122 - t188 * t184 * t48 * t106 / 0.405D3
     # + t237 * t236 + t273 * t272

      end function



      doubleprecision function rrgq2qght11s1e0
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
      t26 = t8 * wd
      t27 = 0.90D2 * t26
      t32 = (0.180D3 * z + 0.180D3 * lh * z) * t7 * wd
      t34 = 0.1D1 / x2
      t38 = 0.1D1 / x1
      t41 = 0.2D1 / 0.9D1 * t8 * wd * t34 * t38
      t42 = x1 ** 2
      t43 = x3 * t42
      t47 = log(-0.4D1 * t43 * t11 * t19)
      t60 = log(-0.4D1 * x3 * t11 * t19)
      t63 = (0.180D3 * lh + 0.90D2 * t60) * z
      t72 = t60 ** 2
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t84 = (0.90D2 * t8 * wd * t22 - t27 + t32) * t34 / 0.810D3 - t41 +
     # (0.90D2 * t8 * wd * t47 - t27 + t32) * t38 / 0.405D3 - t26 / 0.9D
     #1 + (0.180D3 * z + t63) * t7 * wd / 0.810D3 + (-0.90D2 * z - 0.2D1
     # * t63 + (-0.180D3 * t60 * lh - 0.45D2 * t72 - 0.180D3 * t74 + 0.3
     #0D2 * t76) * z) * t7 * wd / 0.810D3
      t85 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #4)
      t87 = -0.1D1 + x1
      t88 = x3 * x1
      t89 = t88 * z
      t90 = 0.2D1 * t9
      t91 = t9 * x1
      t92 = x1 * z
      t93 = t9 * t92
      t94 = cos(t13)
      t95 = -0.1D1 + x2
      t96 = x3 * t95
      t97 = 0.1D1 - x1 + t92
      t101 = Sqrt(t96 * t97 * x2 * t4)
      t103 = 0.2D1 * t94 * t101
      t106 = 0.1D1 / t97
      t109 = t2 * t88
      t110 = x2 * x1
      t111 = t110 * z
      t112 = 0.1D1 - x1 + t92 - x2 + t110 - t111 - x3 + t88 - t89 + t90 
     #- t91 + t93 + t103
      t116 = t4 * s
      t118 = t116 * t1 * x1
      t124 = x2 * z
      t126 = (-0.1D1 + x1 - t92 + x2 - t110 - t124 + t111) ** 2
      t128 = t97 ** 2
      t131 = 0.1D1 / t126 * t128 * t34 * t38
      t134 = FJET(XB1, XB2, s, t2 * t87 * (-x3 + t88 - t89 + t90 - t91 +
     # t93 - x2 + t103) * t106, t109, -t2 * t87 * t112 * t106, -t118, -s
     # * t10 * x2 * x1 * t87 * t106, -0.2D1 / 0.9D1 * t26 * t131)
      t142 = Sqrt(t96 * x2 * t4)
      t144 = 0.2D1 * t94 * t142
      t155 = log(0.4D1 * t9 * t15 * t17 * t11 * t95 * t4)
      t158 = (0.1D1 - x2 + t124) ** 2
      t159 = 0.1D1 / t158
      t172 = (-0.90D2 * t8 * wd * t155 * t159 - (-t27 + t32) * t159) * t
     #34 / 0.810D3 + 0.2D1 / 0.9D1 * t26 * t159 * t34 * t38
      t173 = FJET(XB1, XB2, s, -t2 * (-x3 + t90 - x2 + t144), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t90 + t144), 0.0D0, 0.0D0, t172)
      t176 = t1 * t87
      t182 = t87 ** 2
      t187 = log(-0.4D1 * t43 * t11 * t15 * t17 * t106 * t182 * t4)
      t194 = t41 + (-0.90D2 * t8 * wd * t187 + t27 - t32) * t38 / 0.405D
     #3
      t195 = FJET(XB1, XB2, s, -x3 * s * t176, t109, t116 * t176, -t118,
     # 0.0D0, t194)
      rrgq2qght11s1e0 = t85 * t84 - 0.2D1 / 0.9D1 * t134 * z * t7 * wd *
     # t131 + t173 * t172 + t195 * t194

      end function



      doubleprecision function rrgq2qght11s1em1
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = z ** 2
      t29 = log(-0.4D1 * x3 * t18 * t22 / t23 * t4)
      t37 = 0.1D1 / x2
      t41 = -t12 - t8 * wd / 0.9D1 + (0.180D3 * z + (0.180D3 * lh + 0.90
     #D2 * t29) * z) * t7 * wd / 0.810D3 - t8 * wd * t37 / 0.9D1
      t42 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #1)
      t46 = t1 * (-0.1D1 + x1)
      t50 = t4 * s
      t54 = FJET(XB1, XB2, s, -x3 * s * t46, t2 * x1 * x3, t50 * t46, -t
     #50 * t1 * x1, 0.0D0, t12)
      t61 = 0.2D1 * x2 * x3
      t62 = cos(t20)
      t67 = Sqrt(x3 * (-0.1D1 + x2) * x2 * t4)
      t69 = 0.2D1 * t62 * t67
      t76 = (0.1D1 - x2 + x2 * z) ** 2
      t79 = wd / t76 * t37
      t82 = FJET(XB1, XB2, s, -t2 * (-x3 + t61 - x2 + t69), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t61 + t69), 0.0D0, 0.0D0, t8 * t79 / 0.9D1)
      rrgq2qght11s1em1 = t42 * t41 + 0.2D1 / 0.9D1 * t54 * z * t7 * wd *
     # t9 + t82 * z * t7 * t79 / 0.9D1

      end function



      doubleprecision function rrgq2qght11s1em2
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
      t11 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, -z * t7 * wd / 0.9D1)
      rrgq2qght11s1em2 = -t11 * z * t7 * wd / 0.9D1

      end function



      doubleprecision function rrgq2qght11s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght11s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght11s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght11s1em4 = 0.0D0

      end function
