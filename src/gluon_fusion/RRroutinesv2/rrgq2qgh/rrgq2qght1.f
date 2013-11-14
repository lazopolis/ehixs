  
      subroutine rrgq2qght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh11J1  
      doubleprecision rrgq2qgh11J2  
      doubleprecision rrgq2qgh11J3  
      doubleprecision rrgq2qgh11J4  
      doubleprecision rrgq2qgh11J5  
      doubleprecision rrgq2qgh11J6  
      doubleprecision rrgq2qgh11J7  
      doubleprecision rrgq2qght1s1e1  
      doubleprecision rrgq2qght1s1e0  
      doubleprecision rrgq2qght1s1em1  
      doubleprecision rrgq2qght1s1em2  
      doubleprecision rrgq2qght1s1em3  
      doubleprecision rrgq2qght1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh11J1
      doubleprecision rrgq2qgh11J2
      doubleprecision rrgq2qgh11J3
      doubleprecision rrgq2qgh11J4
      doubleprecision rrgq2qgh11J5
      doubleprecision rrgq2qgh11J6
      doubleprecision rrgq2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / s
      t7 = pi * t6
      t8 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = x2 * t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x3
      t19 = t18 * t4
      t22 = log(-0.4D1 * t15 * t19)
      t23 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t25 = t22 ** 2
      t26 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t32 = pi * lh
      t38 = lh ** 2
      t40 = pi ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = pi * t42
      t44 = t6 * t26
      t45 = t43 * t44
      t47 = 0.1D1 / x2
      t50 = t11 * t14
      t53 = log(-0.4D1 * t50 * t19)
      t54 = t53 * pi
      t57 = t53 ** 2
      t58 = t57 * pi
      t64 = rrgq2qgh11J4(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t90 = x1 ** 2
      t91 = x2 * t90
      t93 = t14 * t17
      t94 = x3 * t4
      t98 = log(-0.4D1 * t91 * t11 * t93 * t94)
      t106 = 0.1D1 / x1
      t110 = t90 * t11
      t114 = log(-0.4D1 * t110 * t14 * t19)
      t116 = t114 ** 2
      t130 = (0.90D2 * t7 * (-t8 + t22 * t23 - t25 * t26 / 0.2D1) - 0.18
     #0D3 * t32 * t6 * (-t23 + t22 * t26) - t45) * t47 / 0.1440D4 - (0.1
     #80D3 * t54 * lh + 0.45D2 * t58 + t43) * t6 * t23 / 0.1440D4 - t7 *
     # t64 / 0.16D2 - (-0.90D2 * t58 * lh + pi * (0.60D2 * lh * t40 - 0.
     #240D3 * zeta3 - 0.120D3 * t38 * lh) - 0.15D2 * t57 * t53 * pi - t5
     #4 * t42) * t6 * t26 / 0.1440D4 - (-0.180D3 * t32 - 0.90D2 * t54) *
     # t6 * t8 / 0.1440D4 + (0.90D2 * t7 * (-t23 + t98 * t26) + 0.180D3 
     #* t32 * t44) * t106 * t47 / 0.720D3 - (0.90D2 * t7 * (t8 - t114 * 
     #t23 + t116 * t26 / 0.2D1) - 0.180D3 * t32 * t6 * (t23 - t114 * t26
     #) + t45) * t106 / 0.720D3
      t131 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t130)
      t133 = -0.1D1 + x1
      t134 = x3 * x1
      t135 = t134 * z
      t137 = 0.2D1 * x2 * x3
      t138 = t134 * x2
      t139 = x2 * z
      t140 = t134 * t139
      t141 = cos(t9)
      t142 = -0.1D1 + x2
      t144 = x1 * z
      t145 = 0.1D1 - x1 + t144
      t149 = Sqrt(x3 * t142 * t145 * x2 * t4)
      t151 = 0.2D1 * t141 * t149
      t154 = 0.1D1 / t145
      t156 = t2 * t133 * (-x3 + t134 - t135 + t137 - t138 + t140 - x2 + 
     #t151) * t154
      t157 = t2 * t134
      t158 = x1 * x2
      t159 = t158 * z
      t160 = 0.1D1 - x1 + t144 - x2 + t158 - t159 - x3 + t134 - t135 + t
     #137 - t138 + t140 + t151
      t163 = t2 * t133 * t160 * t154
      t165 = t2 * x1 * t4
      t170 = s * t16 * x2 * x1 * t133 * t154
      t172 = 0.1D1 / (-0.1D1 + x1 - t144 + x2 - t158 - t139 + t159)
      t173 = t145 * t172
      t174 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t156, -t163, t1
     #57, -t165, -t170)
      t178 = t133 ** 2
      t179 = t154 * t178
      t184 = log(0.4D1 * t91 * t50 * t17 * t94 * t179 * t142)
      t186 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t156, -t163, t1
     #57, -t165, -t170)
      t196 = 0.90D2 * t7 * (t173 * t174 - t184 * t145 * t172 * t186) - 0
     #.180D3 * t32 * t6 * t173 * t186
      t200 = FJET(XB1, XB2, s, t156, t157, -t163, -t165, -t170, t196 * t
     #106 * t47 / 0.720D3)
      t207 = Sqrt(x2 * t142 * t94)
      t209 = 0.2D1 * t141 * t207
      t211 = t2 * (-x3 + t137 - x2 + t209)
      t213 = t2 * (0.1D1 - x2 - x3 + t137 + t209)
      t215 = 0.1D1 / (0.1D1 - x2 + t139)
      t216 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, -t211, t213, 0.
     #0D0, 0.0D0, 0.0D0)
      t219 = t18 * t4 * t142
      t222 = log(0.4D1 * t15 * t219)
      t223 = t222 * t215
      t224 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t211, t213, 0.
     #0D0, 0.0D0, 0.0D0)
      t226 = t222 ** 2
      t228 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t211, t213, 0.
     #0D0, 0.0D0, 0.0D0)
      t234 = t215 * t224
      t241 = t6 * t215 * t228
      t246 = t91 * t50
      t249 = log(0.4D1 * t246 * t219)
      t261 = (0.90D2 * t7 * (t215 * t216 - t223 * t224 + t226 * t215 * t
     #228 / 0.2D1) - 0.180D3 * t32 * t6 * (t234 - t223 * t228) + t43 * t
     #241) * t47 / 0.1440D4 + (0.90D2 * t7 * (t234 - t249 * t215 * t228)
     # - 0.180D3 * t32 * t241) * t106 * t47 / 0.720D3
      t262 = FJET(XB1, XB2, s, -t211, 0.0D0, t213, 0.0D0, 0.0D0, t261)
      t265 = t2 * t133 * x3
      t267 = t2 * t133 * t4
      t268 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t265, t267, t1
     #57, -t165, 0.0D0)
      t274 = log(-0.4D1 * t246 * t18 * t4 * t154 * t178)
      t275 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t265, t267, t1
     #57, -t165, 0.0D0)
      t280 = t6 * t275
      t286 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, -t265, t267, t1
     #57, -t165, 0.0D0)
      t291 = log(-0.4D1 * t110 * t93 * t94 * t179)
      t293 = t291 ** 2
      t308 = (0.90D2 * t7 * (t268 - t274 * t275) - 0.180D3 * t32 * t280)
     # * t106 * t47 / 0.720D3 - (-0.90D2 * t7 * (t286 - t291 * t268 + t2
     #93 * t275 / 0.2D1) + 0.180D3 * t32 * t6 * (t268 - t291 * t275) - t
     #43 * t280) * t106 / 0.720D3
      t309 = FJET(XB1, XB2, s, -t265, t157, t267, -t165, 0.0D0, t308)
      rrgq2qght1s1e1 = t131 * t130 + t200 * t196 * t106 * t47 / 0.720D3 
     #+ t262 * t261 + t309 * t308

      end function



      doubleprecision function rrgq2qght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh11J1
      doubleprecision rrgq2qgh11J2
      doubleprecision rrgq2qgh11J3
      doubleprecision rrgq2qgh11J4
      doubleprecision rrgq2qgh11J5
      doubleprecision rrgq2qgh11J6
      doubleprecision rrgq2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / s
      t7 = pi * t6
      t8 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = x2 * t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x3
      t19 = t18 * t4
      t22 = log(-0.4D1 * t15 * t19)
      t23 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t28 = pi * lh
      t31 = 0.180D3 * t28 * t6 * t23
      t33 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t41 = x1 ** 2
      t42 = t41 * t11
      t46 = log(-0.4D1 * t42 * t14 * t19)
      t54 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t61 = log(-0.4D1 * t11 * t14 * t19)
      t62 = t61 * pi
      t70 = t61 ** 2
      t73 = lh ** 2
      t75 = pi ** 2
      t83 = (0.90D2 * t7 * (-t8 + t22 * t23) + t31) * t33 / 0.1440D4 - t
     #7 * t23 * t36 * t33 / 0.8D1 - (0.90D2 * t7 * (t8 - t46 * t23) - t3
     #1) * t36 / 0.720D3 - t7 * t54 / 0.16D2 - (-0.180D3 * t28 - 0.90D2 
     #* t62) * t6 * t8 / 0.1440D4 - (0.180D3 * t62 * lh + 0.45D2 * t70 *
     # pi + pi * (0.180D3 * t73 - 0.30D2 * t75)) * t6 * t23 / 0.1440D4
      t84 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t83)
      t86 = -0.1D1 + x1
      t87 = x3 * x1
      t88 = t87 * z
      t90 = 0.2D1 * x2 * x3
      t91 = t87 * x2
      t92 = x2 * z
      t93 = t87 * t92
      t94 = cos(t9)
      t95 = -0.1D1 + x2
      t97 = x1 * z
      t98 = 0.1D1 - x1 + t97
      t102 = Sqrt(x3 * t95 * t98 * x2 * t4)
      t104 = 0.2D1 * t94 * t102
      t107 = 0.1D1 / t98
      t109 = t2 * t86 * (-x3 + t87 - t88 + t90 - t91 + t93 - x2 + t104) 
     #* t107
      t110 = t2 * t87
      t111 = x1 * x2
      t112 = t111 * z
      t113 = 0.1D1 - x1 + t97 - x2 + t111 - t112 - x3 + t87 - t88 + t90 
     #- t91 + t93 + t104
      t116 = t2 * t86 * t113 * t107
      t118 = t2 * x1 * t4
      t123 = s * t16 * x2 * x1 * t86 * t107
      t127 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t109, -t116, t1
     #10, -t118, -t123)
      t130 = 0.1D1 / (-0.1D1 + x1 - t97 + x2 - t111 - t92 + t112) * t127
     # * t36 * t33
      t133 = FJET(XB1, XB2, s, t109, t110, -t116, -t118, -t123, t7 * t98
     # * t130 / 0.8D1)
      t140 = x3 * t4
      t142 = Sqrt(x2 * t95 * t140)
      t144 = 0.2D1 * t94 * t142
      t146 = t2 * (-x3 + t90 - x2 + t144)
      t148 = t2 * (0.1D1 - x2 - x3 + t90 + t144)
      t150 = 0.1D1 / (0.1D1 - x2 + t92)
      t151 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t146, t148, 0.
     #0D0, 0.0D0, 0.0D0)
      t157 = log(0.4D1 * t15 * t18 * t4 * t95)
      t159 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t146, t148, 0.
     #0D0, 0.0D0, 0.0D0)
      t176 = (0.90D2 * t7 * (t150 * t151 - t157 * t150 * t159) - 0.180D3
     # * t28 * t6 * t150 * t159) * t33 / 0.1440D4 + t7 * t150 * t159 * t
     #36 * t33 / 0.8D1
      t177 = FJET(XB1, XB2, s, -t146, 0.0D0, t148, 0.0D0, 0.0D0, t176)
      t180 = t2 * t86 * x3
      t182 = t2 * t86 * t4
      t183 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t180, t182, t1
     #10, -t118, 0.0D0)
      t188 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t180, t182, t1
     #10, -t118, 0.0D0)
      t191 = t86 ** 2
      t196 = log(-0.4D1 * t42 * t14 * t17 * t140 * t107 * t191)
      t207 = t7 * t183 * t36 * t33 / 0.8D1 - (-0.90D2 * t7 * (t188 - t19
     #6 * t183) + 0.180D3 * t28 * t6 * t183) * t36 / 0.720D3
      t208 = FJET(XB1, XB2, s, -t180, t110, t182, -t118, 0.0D0, t207)
      rrgq2qght1s1e0 = t84 * t83 + t133 * pi * t6 * t98 * t130 / 0.8D1 +
     # t177 * t176 + t208 * t207

      end function



      doubleprecision function rrgq2qght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh11J1
      doubleprecision rrgq2qgh11J2
      doubleprecision rrgq2qgh11J3
      doubleprecision rrgq2qgh11J4
      doubleprecision rrgq2qgh11J5
      doubleprecision rrgq2qgh11J6
      doubleprecision rrgq2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / s
      t7 = pi * t6
      t8 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t9 = 0.1D1 / x1
      t13 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t18 = x4 * pi
      t19 = Sin(t18)
      t20 = t19 ** 2
      t21 = z ** 2
      t24 = t1 ** 2
      t25 = t24 ** 2
      t30 = log(-0.4D1 * t20 / t21 * t25 * x3 * t4)
      t37 = 0.1D1 / x2
      t41 = -t7 * t8 * t9 / 0.8D1 - t7 * t13 / 0.16D2 - (-0.180D3 * pi *
     # lh - 0.90D2 * t30 * pi) * t6 * t8 / 0.1440D4 - t7 * t8 * t37 / 0.
     #16D2
      t42 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t41)
      t44 = -0.1D1 + x1
      t46 = t2 * t44 * x3
      t48 = t2 * x1 * x3
      t50 = t2 * t44 * t4
      t52 = t2 * x1 * t4
      t53 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t46, t50, t48, 
     #-t52, 0.0D0)
      t57 = FJET(XB1, XB2, s, -t46, t48, t50, -t52, 0.0D0, t7 * t53 * t9
     # / 0.8D1)
      t64 = 0.2D1 * x2 * x3
      t65 = cos(t18)
      t70 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t72 = 0.2D1 * t65 * t70
      t74 = t2 * (-x3 + t64 - x2 + t72)
      t76 = t2 * (0.1D1 - x2 - x3 + t64 + t72)
      t80 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t74, t76, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t80 * t37
      t85 = FJET(XB1, XB2, s, -t74, 0.0D0, t76, 0.0D0, 0.0D0, t7 * t82 /
     # 0.16D2)
      rrgq2qght1s1em1 = t42 * t41 + t57 * pi * t6 * t53 * t9 / 0.8D1 + t
     #85 * pi * t6 * t82 / 0.16D2

      end function



      doubleprecision function rrgq2qght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh11J1
      doubleprecision rrgq2qgh11J2
      doubleprecision rrgq2qgh11J3
      doubleprecision rrgq2qgh11J4
      doubleprecision rrgq2qgh11J5
      doubleprecision rrgq2qgh11J6
      doubleprecision rrgq2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t6 = 0.1D1 / s
      t8 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -pi * t6 * t
     #8 / 0.16D2)
      rrgq2qght1s1em2 = -t11 * pi * t6 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh11J1
      doubleprecision rrgq2qgh11J2
      doubleprecision rrgq2qgh11J3
      doubleprecision rrgq2qgh11J4
      doubleprecision rrgq2qgh11J5
      doubleprecision rrgq2qgh11J6
      doubleprecision rrgq2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght1s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh11J1
      doubleprecision rrgq2qgh11J2
      doubleprecision rrgq2qgh11J3
      doubleprecision rrgq2qgh11J4
      doubleprecision rrgq2qgh11J5
      doubleprecision rrgq2qgh11J6
      doubleprecision rrgq2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh11J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S12 + S13 + S23
      t2 = 0.1D1 / t1
      t4 = 0.1D1 / S12
      t7 = S23 - S13
      t9 = t1 ** 2
      t10 = 0.1D1 / t9
      t12 = S23 + S24 + S34
      t14 = S12 ** 2
      t15 = 0.1D1 / t14
      t16 = t12 * S34 * t15
      t19 = s ** 2
      t21 = z ** 2
      t23 = t2 * t12
      t40 = t10 * t12
      t42 = 0.16D2 * t2
      t46 = 0.7D1 / 0.9D1 * S23
      t47 = 0.4D1 * S14
      t56 = S34 ** 2
      t73 = 0.82D2 / 0.9D1 * S14
      t74 = 0.91D2 / 0.18D2 * S23
      t81 = 0.20D2 / 0.3D1 * S14
      t83 = 0.37D2 / 0.9D1 * S23
      t84 = S23 * S13
      t86 = S23 ** 2
      t87 = S14 ** 2
      t88 = 0.6D1 * t87
      t89 = S13 ** 2
      t91 = S14 * S23
      t92 = 0.28D2 / 0.9D1 * t91
      t93 = S13 * S14
      t99 = t56 * S34
      t117 = 0.136D3 / 0.3D1 * S14
      t118 = 0.1733D4 / 0.18D2 * S23
      t122 = S13 * S24
      t128 = S23 * S24
      t130 = S24 ** 2
      t134 = S24 * S14
      t145 = 0.32D2 / 0.9D1 * t91
      t153 = 0.44D2 / 0.3D1 * S14
      t154 = 0.275D3 / 0.36D2 * S23
      t156 = 0.16D2 / 0.3D1 * t86
      t157 = 0.14D2 / 0.9D1 * t87
      t159 = 0.100D3 / 0.9D1 * t91
      t170 = 0.8D1 / 0.3D1 * t122
      t172 = 0.26D2 / 0.3D1 * t87
      t174 = 0.47D2 / 0.36D2 * t86
      t176 = 0.4D1 * t87 * S14
      t178 = 0.7D1 / 0.9D1 * t86 * S23
      t183 = 0.14D2 / 0.3D1 * S23 * t87
      t192 = 0.4D1 * t86 * S14
      t195 = (0.68D2 / 0.9D1 * t40 - t42) * t99 + ((0.311D3 / 0.9D1 * t2
     # + (0.136D3 / 0.9D1 * S24 + 0.136D3 / 0.9D1 * S14 - 0.272D3 / 0.9D
     #1 * S13 - 0.272D3 / 0.9D1 * S23) * t10) * t12 - 0.3523D4 / 0.36D2 
     #+ (-0.24D2 * S13 + 0.32D2 * S24) * t2) * t56 + ((0.658D3 / 0.3D1 +
     # (-0.3523D4 / 0.36D2 * S13 + t117 - t118 + 0.311D3 / 0.9D1 * S24) 
     #* t2 + (-0.272D3 / 0.9D1 * t122 + 0.118D3 / 0.9D1 * t86 + 0.68D2 /
     # 0.9D1 * t87 + 0.94D2 / 0.9D1 * t89 + 0.212D3 / 0.9D1 * t84 - 0.27
     #2D3 / 0.9D1 * t128 + 0.68D2 / 0.9D1 * t130 - 0.272D3 / 0.9D1 * t91
     # - 0.272D3 / 0.9D1 * t93 + 0.136D3 / 0.9D1 * t134) * t10) * t12 - 
     #0.3523D4 / 0.36D2 * S24 + t117 + 0.311D3 / 0.9D1 * S13 - t118 + (0
     #.24D2 * t122 + 0.56D2 / 0.9D1 * t87 - 0.16D2 * t86 - t145 - 0.16D2
     # * t130 - 0.16D2 * t89) * t2) * S34 + (0.17D2 / 0.2D1 * S13 + t153
     # + t154 - 0.17D2 / 0.36D2 * S24 + (-t156 - t157 - 0.146D3 / 0.9D1 
     #* t93 - t159 - 0.265D3 / 0.18D2 * t84 - 0.215D3 / 0.18D2 * t89) * 
     #t2) * t12 - 0.187D3 / 0.9D1 * t89 - 0.47D2 / 0.36D2 * t130 - 0.32D
     #2 / 0.9D1 * t134 + 0.8D1 / 0.3D1 * t84 - t170 - 0.220D3 / 0.9D1 * 
     #t93 - t172 + 0.47D2 / 0.18D2 * t128 + t145 - t174 + (-t176 + t178 
     #- S13 * t86 + 0.28D2 / 0.9D1 * t93 * S23 + t183 - 0.6D1 * t87 * S1
     #3 - t89 * S13 - 0.4D1 * S14 * t89 + 0.7D1 / 0.9D1 * t89 * S23 - t1
     #92) * t2
      t197 = t56 ** 2
      t253 = (-0.48D2 * t2 * S34 * t4 + 0.16D2 / 0.9D1 * t7 * t10 * t16)
     # * t19 * t21 + (((0.48D2 * t23 + 0.48D2 - 0.48D2 * S23 * t2) * S34
     # + 0.32D2 / 0.9D1 * S24 - 0.32D2 / 0.9D1 * S23) * t4 - 0.32D2 / 0.
     #9D1 * t7 * t2 * t16) * s * z + t2 * t14 + ((0.94D2 / 0.9D1 * t40 -
     # t42) * S34 - 0.5D1 / 0.18D2 * t23 + 0.1D1 / 0.3D1 + (t46 - t47 - 
     #0.3D1 * S13) * t2) * S12 + (-0.272D3 / 0.9D1 * t40 + 0.24D2 * t2) 
     #* t56 + ((-0.3523D4 / 0.36D2 * t2 + (0.188D3 / 0.9D1 * S13 + 0.212
     #D3 / 0.9D1 * S23 - 0.272D3 / 0.9D1 * S14 - 0.272D3 / 0.9D1 * S24) 
     #* t10) * t12 + 0.311D3 / 0.9D1 + (0.32D2 * S13 - 0.24D2 * S24) * t
     #2) * S34 + (0.61D2 / 0.18D2 + (-t73 - t74 - 0.82D2 / 0.9D1 * S13) 
     #* t2) * t12 - 0.37D2 / 0.9D1 * S24 + t81 - 0.4D1 / 0.9D1 * S13 + t
     #83 + (-0.14D2 / 0.9D1 * t84 + t86 + t88 + 0.3D1 * t89 - t92 + 0.8D
     #1 * t93) * t2 + t195 * t4 + (t2 * t197 + (t23 / 0.3D1 - 0.5D1 / 0.
     #18D2 + (-t47 - 0.3D1 * S24 + t46) * t2) * t99 + ((0.61D2 / 0.18D2 
     #+ (-0.37D2 / 0.9D1 * S13 + t81 + t83 - 0.4D1 / 0.9D1 * S24) * t2) 
     #* t12 - 0.82D2 / 0.9D1 * S24 - t73 - t74 + (0.8D1 * t134 + t86 + 0
     #.3D1 * t130 + t88 - t92 - 0.14D2 / 0.9D1 * t128) * t2) * t56 + ((-
     #0.17D2 / 0.36D2 * S13 + t153 + 0.17D2 / 0.2D1 * S24 + t154 + (-t17
     #4 - 0.32D2 / 0.9D1 * t93 - 0.47D2 / 0.36D2 * t89 + t145 + 0.47D2 /
     # 0.18D2 * t84 + 0.8D1 / 0.3D1 * t128 - t170 - 0.220D3 / 0.9D1 * t1
     #34 - t172 - 0.187D3 / 0.9D1 * t130) * t2) * t12 - t159 - 0.146D3 /
     # 0.9D1 * t134 - 0.265D3 / 0.18D2 * t128 - 0.215D3 / 0.18D2 * t130 
     #- t156 - t157 + (-t192 + 0.28D2 / 0.9D1 * t134 * S23 + 0.7D1 / 0.9
     #D1 * t130 * S23 - t176 + t178 - S24 * t86 - 0.4D1 * S14 * t130 + t
     #183 - t130 * S24 - 0.6D1 * t87 * S24) * t2) * S34 + (-0.32D2 / 0.9
     #D1 * t91 + 0.32D2 / 0.9D1 * t122) * t12) * t15
      rrgq2qgh11J1 = t253 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh11J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -S23 + S13
      t3 = S12 + S13 + S23
      t4 = t3 ** 2
      t5 = 0.1D1 / t4
      t7 = S23 + S24 + S34
      t9 = S12 ** 2
      t10 = 0.1D1 / t9
      t12 = s ** 2
      t13 = z ** 2
      t19 = 0.1D1 / S12
      t23 = 0.1D1 / t3
      t32 = t5 * t7
      t34 = 0.16D2 * t23
      t37 = t23 * t7
      t39 = 0.7D1 / 0.9D1 * S23
      t40 = 0.4D1 * S14
      t49 = S34 ** 2
      t67 = 0.37D2 / 0.18D2 * S23
      t68 = 0.2D1 * S14
      t74 = 0.76D2 / 0.9D1 * S14
      t77 = 0.17D2 / 0.9D1 * S23
      t78 = S14 ** 2
      t79 = 0.4D1 * t78
      t80 = S13 ** 2
      t82 = S23 * S13
      t84 = S14 * S23
      t85 = 0.8D1 / 0.9D1 * t84
      t86 = S23 ** 2
      t87 = S13 * S14
      t93 = t49 * S34
      t111 = 0.796D3 / 0.9D1 * S14
      t112 = 0.467D3 / 0.2D1 * S23
      t116 = S13 * S24
      t122 = S23 * S24
      t124 = S24 ** 2
      t128 = S24 * S14
      t148 = 0.56D2 / 0.9D1 * S14
      t149 = 0.119D3 / 0.36D2 * S23
      t151 = 0.20D2 / 0.9D1 * t78
      t153 = 0.28D2 / 0.9D1 * t84
      t154 = 0.9D1 / 0.2D1 * t86
      t157 = 0.64D2 / 0.9D1 * t116
      t168 = 0.136D3 / 0.9D1 * t116
      t170 = 0.7D1 / 0.36D2 * t86
      t171 = 0.4D1 / 0.3D1 * t78
      t172 = 0.92D2 / 0.9D1 * t84
      t175 = 0.7D1 / 0.9D1 * t86 * S23
      t177 = S23 * t87
      t179 = t78 * S13
      t184 = S23 * t78
      t185 = 0.44D2 / 0.9D1 * t184
      t186 = t80 * S23
      t189 = 0.28D2 / 0.9D1 * t86 * S14
      t192 = (-0.40D2 / 0.9D1 * t32 - t34) * t93 + ((-0.211D3 / 0.3D1 * 
     #t23 + (-0.80D2 / 0.9D1 * S14 - 0.80D2 / 0.9D1 * S24 + 0.160D3 / 0.
     #9D1 * S23 + 0.160D3 / 0.9D1 * S13) * t5) * t7 - 0.8789D4 / 0.36D2 
     #+ (-0.24D2 * S13 + 0.32D2 * S24) * t23) * t49 + ((0.4310D4 / 0.9D1
     # + (-0.8789D4 / 0.36D2 * S13 - t111 - t112 - 0.211D3 / 0.3D1 * S24
     #) * t23 + (0.160D3 / 0.9D1 * t116 - 0.76D2 / 0.9D1 * t86 - 0.40D2 
     #/ 0.9D1 * t78 - 0.52D2 / 0.9D1 * t80 - 0.128D3 / 0.9D1 * t82 + 0.1
     #60D3 / 0.9D1 * t122 - 0.40D2 / 0.9D1 * t124 + 0.160D3 / 0.9D1 * t8
     #4 + 0.160D3 / 0.9D1 * t87 - 0.80D2 / 0.9D1 * t128) * t5) * t7 - 0.
     #8789D4 / 0.36D2 * S24 - t111 - 0.211D3 / 0.3D1 * S13 - t112 + (0.2
     #4D2 * t116 - 0.32D2 * t78 - 0.56D2 / 0.9D1 * t86 + 0.224D3 / 0.9D1
     # * t84 - 0.16D2 * t124 - 0.16D2 * t80) * t23) * S34 + (0.473D3 / 0
     #.36D2 * S24 + 0.29D2 / 0.9D1 * S13 - t148 + t149 + (-0.34D2 / 0.9D
     #1 * t124 - t151 - 0.10D2 / 0.9D1 * t87 - t153 - t154 + 0.28D2 / 0.
     #9D1 * t80 - 0.64D2 / 0.9D1 * t122 - t157 - 0.43D2 / 0.18D2 * t82 -
     # 0.6D1 * t128) * t23) * t7 - 0.13D2 / 0.3D1 * t80 - 0.136D3 / 0.9D
     #1 * t82 + 0.92D2 / 0.9D1 * t128 - 0.7D1 / 0.18D2 * t122 + t168 - 0
     #.20D2 / 0.3D1 * t87 + t170 - t171 - t172 + 0.7D1 / 0.36D2 * t124 +
     # (-t175 + S13 * t86 - 0.8D1 / 0.9D1 * t177 - 0.4D1 * t179 - t80 * 
     #S13 - 0.4D1 * S14 * t80 - t185 + 0.7D1 / 0.9D1 * t186 + t189) * t2
     #3
      t194 = t49 ** 2
      t235 = t78 * S24
      t239 = t124 * S23
      t243 = t128 * S23
      t247 = (0.473D3 / 0.36D2 * S13 - t148 + 0.29D2 / 0.9D1 * S24 + t14
     #9 + (t170 - 0.136D3 / 0.9D1 * t122 + 0.92D2 / 0.9D1 * t87 - t172 -
     # 0.7D1 / 0.18D2 * t82 - 0.13D2 / 0.3D1 * t124 + t168 - 0.20D2 / 0.
     #3D1 * t128 - t171 + 0.7D1 / 0.36D2 * t80) * t23) * t7 - 0.64D2 / 0
     #.9D1 * t82 - t157 - 0.10D2 / 0.9D1 * t128 - 0.43D2 / 0.18D2 * t122
     # - t153 + 0.28D2 / 0.9D1 * t124 - t154 - t151 - 0.34D2 / 0.9D1 * t
     #80 - 0.6D1 * t87 + (-0.4D1 * t235 - t124 * S24 + S24 * t86 + t189 
     #- t175 + 0.7D1 / 0.9D1 * t239 - t185 - 0.4D1 * S14 * t124 - 0.8D1 
     #/ 0.9D1 * t243) * t23
      t255 = 0.32D2 / 0.9D1 * t87 * S24
      t256 = 0.16D2 / 0.9D1 * t184
      t271 = t23 * t194 + (0.7D1 / 0.9D1 * t37 + 0.13D2 / 0.9D1 + (-t40 
     #- 0.3D1 * S24 + t39) * t23) * t93 + ((-0.1D1 / 0.3D1 + (0.148D3 / 
     #0.9D1 * S24 + 0.17D2 / 0.9D1 * S13 + t74 - t77) * t23) * t7 - t67 
     #- t68 + 0.23D2 / 0.3D1 * S24 - 0.64D2 / 0.9D1 * S13 + (-0.14D2 / 0
     #.9D1 * t122 + 0.8D1 * t128 + 0.3D1 * t124 + t85 + t79 - t86) * t23
     #) * t49 + t247 * S34 + (0.16D2 / 0.9D1 * t124 - 0.32D2 / 0.9D1 * t
     #78 + 0.32D2 / 0.9D1 * t116 + 0.16D2 / 0.9D1 * t80 - 0.32D2 / 0.9D1
     # * t84 + (t255 - t256 - 0.16D2 / 0.9D1 * t239 + 0.16D2 / 0.9D1 * S
     #13 * t124 + 0.16D2 / 0.9D1 * t179 - 0.32D2 / 0.9D1 * t243) * t23) 
     #* t7 - 0.16D2 / 0.9D1 * t186 + 0.16D2 / 0.9D1 * t235 + 0.16D2 / 0.
     #9D1 * t80 * S24 - t256 - 0.32D2 / 0.9D1 * t177 + t255
      t273 = 0.16D2 / 0.9D1 * t1 * t5 * t7 * S34 * t10 * t12 * t13 + ((-
     #0.32D2 / 0.9D1 * S24 + 0.32D2 / 0.9D1 * S23) * t19 - 0.32D2 / 0.9D
     #1 * t1 * t23 * t7 * S34 * t10) * s * z + t23 * t9 + ((-0.52D2 / 0.
     #9D1 * t32 - t34) * S34 + 0.13D2 / 0.9D1 * t37 + 0.7D1 / 0.9D1 + (t
     #39 - t40 - 0.3D1 * S13) * t23) * S12 + (0.160D3 / 0.9D1 * t32 + 0.
     #24D2 * t23) * t49 + ((-0.8789D4 / 0.36D2 * t23 + (-0.104D3 / 0.9D1
     # * S13 + 0.160D3 / 0.9D1 * S24 + 0.160D3 / 0.9D1 * S14 - 0.128D3 /
     # 0.9D1 * S23) * t5) * t7 - 0.211D3 / 0.3D1 + (0.32D2 * S13 - 0.24D
     #2 * S24) * t23) * S34 + (-0.1D1 / 0.3D1 + (0.23D2 / 0.3D1 * S13 - 
     #t67 - t68 - 0.64D2 / 0.9D1 * S24) * t23) * t7 + t74 + 0.148D3 / 0.
     #9D1 * S13 + 0.17D2 / 0.9D1 * S24 - t77 + (t79 + 0.3D1 * t80 - 0.14
     #D2 / 0.9D1 * t82 + t85 - t86 + 0.8D1 * t87) * t23 + t192 * t19 + t
     #271 * t10
      rrgq2qgh11J2 = t273 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh11J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S23 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t14 = S23 + S24 + S34
      t21 = 0.1D1 / S12
      t28 = S23 ** 2
      t29 = S23 * S24
      t35 = S23 * S13
      t46 = t2 * t14
      t48 = 0.7D1 / 0.9D1 * S23
      t49 = 0.4D1 * S14
      t55 = S34 ** 2
      t67 = 0.18598D5 / 0.9D1 * S14
      t68 = 0.14527D5 / 0.18D2 * S23
      t74 = 0.5D1 / 0.3D1 * S23
      t75 = 0.44D2 / 0.9D1 * S14
      t77 = S14 ** 2
      t78 = 0.4D1 * t77
      t79 = S13 ** 2
      t82 = S14 * S23
      t83 = 0.8D1 / 0.9D1 * t82
      t84 = S13 * S14
      t88 = t55 * S34
      t99 = 0.3991D4 / 0.18D2 * S23
      t101 = 0.568D3 / 0.9D1 * S14
      t111 = S24 ** 2
      t113 = S13 * S24
      t122 = 0.18518D5 / 0.9D1 * S14
      t123 = 0.28253D5 / 0.36D2 * S23
      t125 = 0.28D2 / 0.9D1 * t77
      t127 = 0.6200D4 / 0.3D1 * t82
      t128 = 0.14315D5 / 0.18D2 * t28
      t131 = 0.6380D4 / 0.9D1 * t113
      t133 = S24 * S14
      t143 = 0.136D3 / 0.9D1 * t113
      t145 = 0.259D3 / 0.36D2 * t28
      t146 = 0.10D2 * t82
      t148 = t28 * S23
      t149 = 0.29D2 / 0.9D1 * t148
      t150 = S13 * t28
      t151 = t84 * S23
      t153 = t77 * S13
      t155 = t79 * S13
      t156 = S14 * t79
      t158 = S23 * t77
      t159 = 0.8D1 / 0.9D1 * t158
      t160 = t79 * S23
      t162 = t28 * S14
      t163 = 0.44D2 / 0.9D1 * t162
      t166 = -0.16D2 * t2 * t88 + (-0.473D3 / 0.9D1 * t46 - 0.8009D4 / 0
     #.36D2 + (-0.24D2 * S13 + 0.32D2 * S24) * t2) * t55 + ((0.3878D4 / 
     #0.9D1 + (-0.8009D4 / 0.36D2 * S13 - t99 - 0.473D3 / 0.9D1 * S24 - 
     #t101) * t2) * t14 - t101 - 0.473D3 / 0.9D1 * S13 - t99 - 0.8009D4 
     #/ 0.36D2 * S24 + (-0.64D2 / 0.9D1 * t82 - 0.16D2 * t77 - 0.16D2 * 
     #t79 - 0.16D2 * t111 + 0.24D2 * t113 + 0.88D2 / 0.9D1 * t28) * t2) 
     #* S34 + (0.25693D5 / 0.36D2 * S24 + 0.7085D4 / 0.9D1 * S13 + t122 
     #- t123 + (-0.34D2 / 0.9D1 * t111 - t125 - 0.18562D5 / 0.9D1 * t84 
     #- t127 + t128 - 0.7024D4 / 0.9D1 * t79 - 0.6376D4 / 0.9D1 * t29 - 
     #t131 + 0.25D2 / 0.2D1 * t35 - 0.74D2 / 0.9D1 * t133) * t2) * t14 -
     # 0.55D2 / 0.9D1 * t79 - 0.8D1 * t35 + 0.10D2 * t133 - t29 / 0.6D1 
     #+ t143 - 0.92D2 / 0.9D1 * t84 + t145 - t125 - t146 + t111 / 0.12D2
     # + (t149 + t150 - 0.8D1 / 0.9D1 * t151 - 0.4D1 * t153 - t155 - 0.4
     #D1 * t156 - t159 + 0.7D1 / 0.9D1 * t160 - t163) * t2
      t168 = t55 ** 2
      t208 = t77 * S24
      t210 = t111 * S24
      t211 = S24 * t28
      t212 = t111 * S23
      t214 = S14 * t111
      t216 = t133 * S23
      t220 = (0.25693D5 / 0.36D2 * S13 + t122 + 0.7085D4 / 0.9D1 * S24 -
     # t123 + (t145 - 0.8D1 * t29 + 0.10D2 * t84 - t146 - t35 / 0.6D1 - 
     #0.55D2 / 0.9D1 * t111 + t143 - 0.92D2 / 0.9D1 * t133 - t125 + t79 
     #/ 0.12D2) * t2) * t14 - 0.6376D4 / 0.9D1 * t35 - t131 - 0.18562D5 
     #/ 0.9D1 * t133 + 0.25D2 / 0.2D1 * t29 - t127 - 0.7024D4 / 0.9D1 * 
     #t111 + t128 - t125 - 0.34D2 / 0.9D1 * t79 - 0.74D2 / 0.9D1 * t84 +
     # (-0.4D1 * t208 - t210 + t211 - t163 + t149 + 0.7D1 / 0.9D1 * t212
     # - t159 - 0.4D1 * t214 - 0.8D1 / 0.9D1 * t216) * t2
      t232 = 0.28D2 / 0.9D1 * t148
      t234 = 0.8D1 / 0.3D1 * t162
      t235 = 0.16D2 / 0.3D1 * t158
      t237 = 0.16D2 / 0.9D1 * t35 * S24
      t238 = S13 * t111
      t241 = 0.16D2 / 0.3D1 * t84 * S24
      t242 = t79 * S24
      t246 = 0.16D2 / 0.3D1 * t216
      t249 = 0.16D2 / 0.3D1 * t151
      t252 = t232 + 0.4D1 / 0.9D1 * t155 + t234 - t235 - t237 + 0.16D2 /
     # 0.9D1 * t238 + t241 + 0.8D1 / 0.9D1 * t242 + 0.16D2 / 0.9D1 * t21
     #2 - 0.4D1 / 0.3D1 * t160 - t246 + 0.8D1 * t211 + 0.16D2 / 0.3D1 * 
     #t153 - t249 + 0.4D1 / 0.3D1 * t150 + 0.8D1 / 0.3D1 * t156
      t254 = 0.4D1 / 0.3D1 * t79 + 0.4D1 / 0.3D1 * t111 + 0.16D2 / 0.9D1
     # * t82 + 0.56D2 / 0.3D1 * t28 - 0.64D2 / 0.9D1 * t77 + 0.104D3 / 0
     #.9D1 * t29 - 0.8D1 / 0.9D1 * t84 + 0.104D3 / 0.9D1 * t35 + 0.16D2 
     #/ 0.3D1 * t113 - 0.8D1 / 0.9D1 * t133 + t252 * t2
      t266 = t232 + 0.4D1 / 0.9D1 * t210 - t246 + 0.8D1 * t150 - t249 - 
     #t235 + 0.8D1 / 0.9D1 * t238 + t241 - 0.4D1 / 0.3D1 * t212 + 0.8D1 
     #/ 0.3D1 * t214 - t237
      t269 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.32D2 / 0.3
     #D1 * t3 * t14 + 0.32D2 / 0.3D1 * S23) * t5 * t6 * t9 + (-0.32D2 / 
     #0.3D1 * S23 * t21 + (-0.32D2 / 0.3D1 * t3 * t14 * S34 + (-0.64D2 /
     # 0.3D1 * S23 + (-0.32D2 / 0.3D1 * t28 - 0.32D2 / 0.3D1 * t29) * t2
     #) * t14 - 0.32D2 / 0.3D1 * t35 - 0.32D2 / 0.3D1 * t28) * t5) * s *
     # z + t2 * t4 + (-0.16D2 * t2 * S34 + 0.89D2 / 0.9D1 * t46 - 0.1D1 
     #+ (t48 - t49 - 0.3D1 * S13) * t2) * S12 + 0.24D2 * t2 * t55 + (-0.
     #8009D4 / 0.36D2 * t46 - 0.473D3 / 0.9D1 + (0.32D2 * S13 - 0.24D2 *
     # S24) * t2) * S34 + (-0.79D2 / 0.9D1 + (-0.6388D4 / 0.9D1 * S24 - 
     #0.6911D4 / 0.9D1 * S13 - t67 + t68) * t2) * t14 + 0.17D2 / 0.9D1 *
     # S24 + t74 + t75 + 0.116D3 / 0.9D1 * S13 + (t78 + 0.3D1 * t79 - 0.
     #14D2 / 0.9D1 * t35 + t83 - t28 + 0.8D1 * t84) * t2 + t166 * t21 + 
     #(t2 * t168 + (-t46 + 0.89D2 / 0.9D1 + (-t49 - 0.3D1 * S24 + t48) *
     # t2) * t88 + ((-0.79D2 / 0.9D1 + (0.116D3 / 0.9D1 * S24 + t75 + t7
     #4 + 0.17D2 / 0.9D1 * S13) * t2) * t14 - 0.6911D4 / 0.9D1 * S24 + t
     #68 - t67 - 0.6388D4 / 0.9D1 * S13 + (-0.14D2 / 0.9D1 * t29 + 0.8D1
     # * t133 + 0.3D1 * t111 + t83 + t78 - t28) * t2) * t55 + t220 * S34
     # + t254 * t14 + 0.4D1 / 0.3D1 * t211 + t234 + 0.16D2 / 0.9D1 * t16
     #0 + 0.16D2 / 0.3D1 * t208 + 0.16D2 / 0.9D1 * t242 + t266) * t5
      rrgq2qgh11J3 = t269 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh11J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S23 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t14 = S23 + S24 + S34
      t21 = 0.1D1 / S12
      t28 = S23 ** 2
      t29 = S23 * S24
      t35 = S23 * S13
      t46 = t2 * t14
      t48 = 0.7D1 / 0.9D1 * S23
      t49 = 0.4D1 * S14
      t55 = S34 ** 2
      t67 = 0.18598D5 / 0.9D1 * S14
      t68 = 0.14527D5 / 0.18D2 * S23
      t74 = 0.5D1 / 0.3D1 * S23
      t75 = 0.44D2 / 0.9D1 * S14
      t77 = S14 ** 2
      t78 = 0.4D1 * t77
      t79 = S13 ** 2
      t82 = S14 * S23
      t83 = 0.8D1 / 0.9D1 * t82
      t84 = S13 * S14
      t88 = t55 * S34
      t99 = 0.3991D4 / 0.18D2 * S23
      t101 = 0.568D3 / 0.9D1 * S14
      t111 = S24 ** 2
      t113 = S13 * S24
      t122 = 0.18518D5 / 0.9D1 * S14
      t123 = 0.28253D5 / 0.36D2 * S23
      t125 = 0.28D2 / 0.9D1 * t77
      t127 = 0.6200D4 / 0.3D1 * t82
      t128 = 0.14315D5 / 0.18D2 * t28
      t131 = 0.6380D4 / 0.9D1 * t113
      t133 = S24 * S14
      t143 = 0.136D3 / 0.9D1 * t113
      t145 = 0.259D3 / 0.36D2 * t28
      t146 = 0.10D2 * t82
      t148 = t28 * S23
      t149 = 0.29D2 / 0.9D1 * t148
      t150 = S13 * t28
      t151 = t84 * S23
      t153 = t77 * S13
      t155 = t79 * S13
      t156 = S14 * t79
      t158 = S23 * t77
      t159 = 0.8D1 / 0.9D1 * t158
      t160 = t79 * S23
      t162 = t28 * S14
      t163 = 0.44D2 / 0.9D1 * t162
      t166 = -0.16D2 * t2 * t88 + (-0.473D3 / 0.9D1 * t46 - 0.8009D4 / 0
     #.36D2 + (-0.24D2 * S13 + 0.32D2 * S24) * t2) * t55 + ((0.3878D4 / 
     #0.9D1 + (-0.8009D4 / 0.36D2 * S13 - t99 - 0.473D3 / 0.9D1 * S24 - 
     #t101) * t2) * t14 - t101 - 0.473D3 / 0.9D1 * S13 - t99 - 0.8009D4 
     #/ 0.36D2 * S24 + (-0.64D2 / 0.9D1 * t82 - 0.16D2 * t77 - 0.16D2 * 
     #t79 - 0.16D2 * t111 + 0.24D2 * t113 + 0.88D2 / 0.9D1 * t28) * t2) 
     #* S34 + (0.25693D5 / 0.36D2 * S24 + 0.7085D4 / 0.9D1 * S13 + t122 
     #- t123 + (-0.34D2 / 0.9D1 * t111 - t125 - 0.18562D5 / 0.9D1 * t84 
     #- t127 + t128 - 0.7024D4 / 0.9D1 * t79 - 0.6376D4 / 0.9D1 * t29 - 
     #t131 + 0.25D2 / 0.2D1 * t35 - 0.74D2 / 0.9D1 * t133) * t2) * t14 -
     # 0.55D2 / 0.9D1 * t79 - 0.8D1 * t35 + 0.10D2 * t133 - t29 / 0.6D1 
     #+ t143 - 0.92D2 / 0.9D1 * t84 + t145 - t125 - t146 + t111 / 0.12D2
     # + (t149 + t150 - 0.8D1 / 0.9D1 * t151 - 0.4D1 * t153 - t155 - 0.4
     #D1 * t156 - t159 + 0.7D1 / 0.9D1 * t160 - t163) * t2
      t168 = t55 ** 2
      t208 = t77 * S24
      t210 = t111 * S24
      t211 = S24 * t28
      t212 = t111 * S23
      t214 = S14 * t111
      t216 = t133 * S23
      t220 = (0.25693D5 / 0.36D2 * S13 + t122 + 0.7085D4 / 0.9D1 * S24 -
     # t123 + (t145 - 0.8D1 * t29 + 0.10D2 * t84 - t146 - t35 / 0.6D1 - 
     #0.55D2 / 0.9D1 * t111 + t143 - 0.92D2 / 0.9D1 * t133 - t125 + t79 
     #/ 0.12D2) * t2) * t14 - 0.6376D4 / 0.9D1 * t35 - t131 - 0.18562D5 
     #/ 0.9D1 * t133 + 0.25D2 / 0.2D1 * t29 - t127 - 0.7024D4 / 0.9D1 * 
     #t111 + t128 - t125 - 0.34D2 / 0.9D1 * t79 - 0.74D2 / 0.9D1 * t84 +
     # (-0.4D1 * t208 - t210 + t211 - t163 + t149 + 0.7D1 / 0.9D1 * t212
     # - t159 - 0.4D1 * t214 - 0.8D1 / 0.9D1 * t216) * t2
      t232 = 0.28D2 / 0.9D1 * t148
      t234 = 0.8D1 / 0.3D1 * t162
      t235 = 0.16D2 / 0.3D1 * t158
      t237 = 0.16D2 / 0.9D1 * t35 * S24
      t238 = S13 * t111
      t241 = 0.16D2 / 0.3D1 * t84 * S24
      t242 = t79 * S24
      t246 = 0.16D2 / 0.3D1 * t216
      t249 = 0.16D2 / 0.3D1 * t151
      t252 = t232 + 0.4D1 / 0.9D1 * t155 + t234 - t235 - t237 + 0.16D2 /
     # 0.9D1 * t238 + t241 + 0.8D1 / 0.9D1 * t242 + 0.16D2 / 0.9D1 * t21
     #2 - 0.4D1 / 0.3D1 * t160 - t246 + 0.8D1 * t211 + 0.16D2 / 0.3D1 * 
     #t153 - t249 + 0.4D1 / 0.3D1 * t150 + 0.8D1 / 0.3D1 * t156
      t254 = 0.4D1 / 0.3D1 * t79 + 0.4D1 / 0.3D1 * t111 + 0.16D2 / 0.9D1
     # * t82 + 0.56D2 / 0.3D1 * t28 - 0.64D2 / 0.9D1 * t77 + 0.104D3 / 0
     #.9D1 * t29 - 0.8D1 / 0.9D1 * t84 + 0.104D3 / 0.9D1 * t35 + 0.16D2 
     #/ 0.3D1 * t113 - 0.8D1 / 0.9D1 * t133 + t252 * t2
      t266 = t232 + 0.4D1 / 0.9D1 * t210 - t246 + 0.8D1 * t150 - t249 - 
     #t235 + 0.8D1 / 0.9D1 * t238 + t241 - 0.4D1 / 0.3D1 * t212 + 0.8D1 
     #/ 0.3D1 * t214 - t237
      t269 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.32D2 / 0.3
     #D1 * t3 * t14 + 0.32D2 / 0.3D1 * S23) * t5 * t6 * t9 + (-0.32D2 / 
     #0.3D1 * S23 * t21 + (-0.32D2 / 0.3D1 * t3 * t14 * S34 + (-0.64D2 /
     # 0.3D1 * S23 + (-0.32D2 / 0.3D1 * t28 - 0.32D2 / 0.3D1 * t29) * t2
     #) * t14 - 0.32D2 / 0.3D1 * t35 - 0.32D2 / 0.3D1 * t28) * t5) * s *
     # z + t2 * t4 + (-0.16D2 * t2 * S34 + 0.89D2 / 0.9D1 * t46 - 0.1D1 
     #+ (t48 - t49 - 0.3D1 * S13) * t2) * S12 + 0.24D2 * t2 * t55 + (-0.
     #8009D4 / 0.36D2 * t46 - 0.473D3 / 0.9D1 + (0.32D2 * S13 - 0.24D2 *
     # S24) * t2) * S34 + (-0.79D2 / 0.9D1 + (-0.6388D4 / 0.9D1 * S24 - 
     #0.6911D4 / 0.9D1 * S13 - t67 + t68) * t2) * t14 + 0.17D2 / 0.9D1 *
     # S24 + t74 + t75 + 0.116D3 / 0.9D1 * S13 + (t78 + 0.3D1 * t79 - 0.
     #14D2 / 0.9D1 * t35 + t83 - t28 + 0.8D1 * t84) * t2 + t166 * t21 + 
     #(t2 * t168 + (-t46 + 0.89D2 / 0.9D1 + (-t49 - 0.3D1 * S24 + t48) *
     # t2) * t88 + ((-0.79D2 / 0.9D1 + (0.116D3 / 0.9D1 * S24 + t75 + t7
     #4 + 0.17D2 / 0.9D1 * S13) * t2) * t14 - 0.6911D4 / 0.9D1 * S24 + t
     #68 - t67 - 0.6388D4 / 0.9D1 * S13 + (-0.14D2 / 0.9D1 * t29 + 0.8D1
     # * t133 + 0.3D1 * t111 + t83 + t78 - t28) * t2) * t55 + t220 * S34
     # + t254 * t14 + 0.4D1 / 0.3D1 * t211 + t234 + 0.16D2 / 0.9D1 * t16
     #0 + 0.16D2 / 0.3D1 * t208 + 0.16D2 / 0.9D1 * t242 + t266) * t5
      rrgq2qgh11J4 = t269 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh11J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S23 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t14 = S23 + S24 + S34
      t21 = 0.1D1 / S12
      t28 = S23 ** 2
      t29 = S23 * S24
      t35 = S23 * S13
      t46 = t2 * t14
      t48 = 0.7D1 / 0.9D1 * S23
      t49 = 0.4D1 * S14
      t55 = S34 ** 2
      t67 = 0.18598D5 / 0.9D1 * S14
      t68 = 0.14527D5 / 0.18D2 * S23
      t74 = 0.5D1 / 0.3D1 * S23
      t75 = 0.44D2 / 0.9D1 * S14
      t77 = S14 ** 2
      t78 = 0.4D1 * t77
      t79 = S13 ** 2
      t82 = S14 * S23
      t83 = 0.8D1 / 0.9D1 * t82
      t84 = S13 * S14
      t88 = t55 * S34
      t99 = 0.3991D4 / 0.18D2 * S23
      t101 = 0.568D3 / 0.9D1 * S14
      t111 = S24 ** 2
      t113 = S13 * S24
      t122 = 0.18518D5 / 0.9D1 * S14
      t123 = 0.28253D5 / 0.36D2 * S23
      t125 = 0.28D2 / 0.9D1 * t77
      t127 = 0.6200D4 / 0.3D1 * t82
      t128 = 0.14315D5 / 0.18D2 * t28
      t131 = 0.6380D4 / 0.9D1 * t113
      t133 = S24 * S14
      t143 = 0.136D3 / 0.9D1 * t113
      t145 = 0.259D3 / 0.36D2 * t28
      t146 = 0.10D2 * t82
      t148 = t28 * S23
      t149 = 0.29D2 / 0.9D1 * t148
      t150 = S13 * t28
      t151 = t84 * S23
      t153 = t77 * S13
      t155 = t79 * S13
      t156 = S14 * t79
      t158 = S23 * t77
      t159 = 0.8D1 / 0.9D1 * t158
      t160 = t79 * S23
      t162 = t28 * S14
      t163 = 0.44D2 / 0.9D1 * t162
      t166 = -0.16D2 * t2 * t88 + (-0.473D3 / 0.9D1 * t46 - 0.8009D4 / 0
     #.36D2 + (-0.24D2 * S13 + 0.32D2 * S24) * t2) * t55 + ((0.3878D4 / 
     #0.9D1 + (-0.8009D4 / 0.36D2 * S13 - t99 - 0.473D3 / 0.9D1 * S24 - 
     #t101) * t2) * t14 - t101 - 0.473D3 / 0.9D1 * S13 - t99 - 0.8009D4 
     #/ 0.36D2 * S24 + (-0.64D2 / 0.9D1 * t82 - 0.16D2 * t77 - 0.16D2 * 
     #t79 - 0.16D2 * t111 + 0.24D2 * t113 + 0.88D2 / 0.9D1 * t28) * t2) 
     #* S34 + (0.25693D5 / 0.36D2 * S24 + 0.7085D4 / 0.9D1 * S13 + t122 
     #- t123 + (-0.34D2 / 0.9D1 * t111 - t125 - 0.18562D5 / 0.9D1 * t84 
     #- t127 + t128 - 0.7024D4 / 0.9D1 * t79 - 0.6376D4 / 0.9D1 * t29 - 
     #t131 + 0.25D2 / 0.2D1 * t35 - 0.74D2 / 0.9D1 * t133) * t2) * t14 -
     # 0.55D2 / 0.9D1 * t79 - 0.8D1 * t35 + 0.10D2 * t133 - t29 / 0.6D1 
     #+ t143 - 0.92D2 / 0.9D1 * t84 + t145 - t125 - t146 + t111 / 0.12D2
     # + (t149 + t150 - 0.8D1 / 0.9D1 * t151 - 0.4D1 * t153 - t155 - 0.4
     #D1 * t156 - t159 + 0.7D1 / 0.9D1 * t160 - t163) * t2
      t168 = t55 ** 2
      t208 = t77 * S24
      t210 = t111 * S24
      t211 = S24 * t28
      t212 = t111 * S23
      t214 = S14 * t111
      t216 = t133 * S23
      t220 = (0.25693D5 / 0.36D2 * S13 + t122 + 0.7085D4 / 0.9D1 * S24 -
     # t123 + (t145 - 0.8D1 * t29 + 0.10D2 * t84 - t146 - t35 / 0.6D1 - 
     #0.55D2 / 0.9D1 * t111 + t143 - 0.92D2 / 0.9D1 * t133 - t125 + t79 
     #/ 0.12D2) * t2) * t14 - 0.6376D4 / 0.9D1 * t35 - t131 - 0.18562D5 
     #/ 0.9D1 * t133 + 0.25D2 / 0.2D1 * t29 - t127 - 0.7024D4 / 0.9D1 * 
     #t111 + t128 - t125 - 0.34D2 / 0.9D1 * t79 - 0.74D2 / 0.9D1 * t84 +
     # (-0.4D1 * t208 - t210 + t211 - t163 + t149 + 0.7D1 / 0.9D1 * t212
     # - t159 - 0.4D1 * t214 - 0.8D1 / 0.9D1 * t216) * t2
      t232 = 0.28D2 / 0.9D1 * t148
      t234 = 0.8D1 / 0.3D1 * t162
      t235 = 0.16D2 / 0.3D1 * t158
      t237 = 0.16D2 / 0.9D1 * t35 * S24
      t238 = S13 * t111
      t241 = 0.16D2 / 0.3D1 * t84 * S24
      t242 = t79 * S24
      t246 = 0.16D2 / 0.3D1 * t216
      t249 = 0.16D2 / 0.3D1 * t151
      t252 = t232 + 0.4D1 / 0.9D1 * t155 + t234 - t235 - t237 + 0.16D2 /
     # 0.9D1 * t238 + t241 + 0.8D1 / 0.9D1 * t242 + 0.16D2 / 0.9D1 * t21
     #2 - 0.4D1 / 0.3D1 * t160 - t246 + 0.8D1 * t211 + 0.16D2 / 0.3D1 * 
     #t153 - t249 + 0.4D1 / 0.3D1 * t150 + 0.8D1 / 0.3D1 * t156
      t254 = 0.4D1 / 0.3D1 * t79 + 0.4D1 / 0.3D1 * t111 + 0.16D2 / 0.9D1
     # * t82 + 0.56D2 / 0.3D1 * t28 - 0.64D2 / 0.9D1 * t77 + 0.104D3 / 0
     #.9D1 * t29 - 0.8D1 / 0.9D1 * t84 + 0.104D3 / 0.9D1 * t35 + 0.16D2 
     #/ 0.3D1 * t113 - 0.8D1 / 0.9D1 * t133 + t252 * t2
      t266 = t232 + 0.4D1 / 0.9D1 * t210 - t246 + 0.8D1 * t150 - t249 - 
     #t235 + 0.8D1 / 0.9D1 * t238 + t241 - 0.4D1 / 0.3D1 * t212 + 0.8D1 
     #/ 0.3D1 * t214 - t237
      t269 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.32D2 / 0.3
     #D1 * t3 * t14 + 0.32D2 / 0.3D1 * S23) * t5 * t6 * t9 + (-0.32D2 / 
     #0.3D1 * S23 * t21 + (-0.32D2 / 0.3D1 * t3 * t14 * S34 + (-0.64D2 /
     # 0.3D1 * S23 + (-0.32D2 / 0.3D1 * t28 - 0.32D2 / 0.3D1 * t29) * t2
     #) * t14 - 0.32D2 / 0.3D1 * t35 - 0.32D2 / 0.3D1 * t28) * t5) * s *
     # z + t2 * t4 + (-0.16D2 * t2 * S34 + 0.89D2 / 0.9D1 * t46 - 0.1D1 
     #+ (t48 - t49 - 0.3D1 * S13) * t2) * S12 + 0.24D2 * t2 * t55 + (-0.
     #8009D4 / 0.36D2 * t46 - 0.473D3 / 0.9D1 + (0.32D2 * S13 - 0.24D2 *
     # S24) * t2) * S34 + (-0.79D2 / 0.9D1 + (-0.6388D4 / 0.9D1 * S24 - 
     #0.6911D4 / 0.9D1 * S13 - t67 + t68) * t2) * t14 + 0.17D2 / 0.9D1 *
     # S24 + t74 + t75 + 0.116D3 / 0.9D1 * S13 + (t78 + 0.3D1 * t79 - 0.
     #14D2 / 0.9D1 * t35 + t83 - t28 + 0.8D1 * t84) * t2 + t166 * t21 + 
     #(t2 * t168 + (-t46 + 0.89D2 / 0.9D1 + (-t49 - 0.3D1 * S24 + t48) *
     # t2) * t88 + ((-0.79D2 / 0.9D1 + (0.116D3 / 0.9D1 * S24 + t75 + t7
     #4 + 0.17D2 / 0.9D1 * S13) * t2) * t14 - 0.6911D4 / 0.9D1 * S24 + t
     #68 - t67 - 0.6388D4 / 0.9D1 * S13 + (-0.14D2 / 0.9D1 * t29 + 0.8D1
     # * t133 + 0.3D1 * t111 + t83 + t78 - t28) * t2) * t55 + t220 * S34
     # + t254 * t14 + 0.4D1 / 0.3D1 * t211 + t234 + 0.16D2 / 0.9D1 * t16
     #0 + 0.16D2 / 0.3D1 * t208 + 0.16D2 / 0.9D1 * t242 + t266) * t5
      rrgq2qgh11J5 = t269 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh11J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S12 + S13 + S23
      t2 = 0.1D1 / t1
      t3 = S23 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t15 = 0.1D1 / S12
      t20 = t1 ** 2
      t21 = 0.1D1 / t20
      t23 = S23 + S24 + S34
      t24 = t23 * S34
      t34 = t2 * t23
      t38 = 0.64D2 / 0.9D1 * S23
      t47 = S23 ** 2
      t48 = S23 * S24
      t54 = S23 * S13
      t55 = 0.32D2 / 0.3D1 * t54
      t62 = t21 * t23
      t68 = S34 ** 2
      t82 = 0.6172D4 / 0.3D1 * S14
      t85 = 0.7309D4 / 0.9D1 * S23
      t92 = 0.16D2 / 0.9D1 * S14
      t93 = 0.22D2 / 0.9D1 * S23
      t94 = S14 ** 2
      t97 = S14 * S23
      t100 = (-0.2D1 * t94 - 0.2D1 * t47 + 0.4D1 * t97) * t2
      t101 = t68 * S34
      t115 = 0.976D3 / 0.9D1 * S14
      t117 = 0.1129D4 / 0.9D1 * S23
      t121 = S24 * S14
      t123 = S13 * S14
      t127 = S24 ** 2
      t131 = S13 * S24
      t133 = S13 ** 2
      t149 = 0.7132D4 / 0.9D1 * S23
      t152 = 0.18386D5 / 0.9D1 * S14
      t153 = 0.14D2 / 0.9D1 * t94
      t154 = 0.14411D5 / 0.18D2 * t47
      t160 = 0.18500D5 / 0.9D1 * t97
      t162 = 0.6380D4 / 0.9D1 * t131
      t168 = 0.160D3 / 0.9D1 * t131
      t172 = 0.122D3 / 0.9D1 * t97
      t174 = 0.17D2 / 0.2D1 * t47
      t175 = 0.50D2 / 0.9D1 * t94
      t177 = 0.4D1 * t94 * S14
      t178 = t123 * S23
      t180 = t47 * S14
      t181 = 0.8D1 / 0.9D1 * t180
      t182 = S13 * t47
      t184 = t47 * S23
      t185 = 0.22D2 / 0.9D1 * t184
      t186 = t94 * S13
      t188 = S23 * t94
      t189 = 0.50D2 / 0.9D1 * t188
      t192 = -0.68D2 / 0.9D1 * t62 * t101 + (-0.2243D4 / 0.18D2 + (-0.78
     #4D3 / 0.9D1 * t2 + (-0.136D3 / 0.9D1 * S14 - 0.136D3 / 0.9D1 * S24
     # + 0.272D3 / 0.9D1 * S23 + 0.272D3 / 0.9D1 * S13) * t21) * t23) * 
     #t68 + ((0.1904D4 / 0.9D1 + (-t115 - 0.784D3 / 0.9D1 * S24 - t117 -
     # 0.2243D4 / 0.18D2 * S13) * t2 + (-0.136D3 / 0.9D1 * t121 + 0.272D
     #3 / 0.9D1 * t123 - 0.212D3 / 0.9D1 * t54 + 0.272D3 / 0.9D1 * t48 -
     # 0.68D2 / 0.9D1 * t127 - 0.118D3 / 0.9D1 * t47 + 0.272D3 / 0.9D1 *
     # t97 + 0.272D3 / 0.9D1 * t131 - 0.94D2 / 0.9D1 * t133 - 0.68D2 / 0
     #.9D1 * t94) * t21) * t23 - t117 - 0.2243D4 / 0.18D2 * S24 - 0.784D
     #3 / 0.9D1 * S13 - t115 + (0.232D3 / 0.9D1 * t47 - 0.200D3 / 0.9D1 
     #* t94 - 0.32D2 / 0.9D1 * t97) * t2) * S34 + (-t149 + 0.4285D4 / 0.
     #6D1 * S24 + 0.14017D5 / 0.18D2 * S13 + t152 + (-t153 + t154 - 0.18
     #416D5 / 0.9D1 * t123 - 0.1537D4 / 0.2D1 * t133 + 0.245D3 / 0.9D1 *
     # t54 - 0.34D2 / 0.9D1 * t127 - 0.6376D4 / 0.9D1 * t48 - t160 - 0.7
     #4D2 / 0.9D1 * t121 - t162) * t2) * t23 + 0.44D2 / 0.3D1 * t133 - t
     #55 + t168 + 0.128D3 / 0.9D1 * t123 + 0.122D3 / 0.9D1 * t121 - 0.25
     #D2 / 0.9D1 * t48 - t172 + 0.25D2 / 0.18D2 * t127 + t174 + t175 + (
     #t177 - 0.4D1 * t178 - t181 + 0.2D1 * t182 + t185 + 0.2D1 * t186 - 
     #t189) * t2
      t225 = t94 * S24
      t227 = t121 * S23
      t229 = S24 * t47
      t233 = (-t149 + t152 + 0.14017D5 / 0.18D2 * S24 + 0.4285D4 / 0.6D1
     # * S13 + (t174 - t172 - 0.25D2 / 0.9D1 * t54 + t175 + 0.122D3 / 0.
     #9D1 * t123 + t168 + 0.128D3 / 0.9D1 * t121 + 0.25D2 / 0.18D2 * t13
     #3 - 0.32D2 / 0.3D1 * t48 + 0.44D2 / 0.3D1 * t127) * t2) * t23 - 0.
     #74D2 / 0.9D1 * t123 - 0.6376D4 / 0.9D1 * t54 - t162 - 0.18416D5 / 
     #0.9D1 * t121 + 0.245D3 / 0.9D1 * t48 - t160 - 0.34D2 / 0.9D1 * t13
     #3 - 0.1537D4 / 0.2D1 * t127 + t154 - t153 + (0.2D1 * t225 - 0.4D1 
     #* t227 + 0.2D1 * t229 - t181 + t177 + t185 - t189) * t2
      t245 = 0.28D2 / 0.9D1 * t184
      t248 = 0.8D1 / 0.3D1 * t180
      t249 = 0.16D2 / 0.3D1 * t188
      t251 = 0.16D2 / 0.9D1 * t54 * S24
      t252 = S13 * t127
      t255 = 0.16D2 / 0.3D1 * t123 * S24
      t256 = t133 * S24
      t258 = t127 * S23
      t260 = t133 * S23
      t262 = 0.16D2 / 0.3D1 * t227
      t265 = 0.16D2 / 0.3D1 * t178
      t269 = t245 + 0.4D1 / 0.9D1 * t133 * S13 + t248 - t249 - t251 + 0.
     #16D2 / 0.9D1 * t252 + t255 + 0.8D1 / 0.9D1 * t256 + 0.16D2 / 0.9D1
     # * t258 - 0.4D1 / 0.3D1 * t260 - t262 + 0.8D1 * t229 + 0.16D2 / 0.
     #3D1 * t186 - t265 + 0.4D1 / 0.3D1 * t182 + 0.8D1 / 0.3D1 * S14 * t
     #133
      t271 = 0.4D1 / 0.3D1 * t133 + 0.4D1 / 0.3D1 * t127 + 0.16D2 / 0.3D
     #1 * t97 + 0.56D2 / 0.3D1 * t47 - 0.64D2 / 0.9D1 * t94 + 0.104D3 / 
     #0.9D1 * t48 - 0.8D1 / 0.9D1 * t123 + 0.104D3 / 0.9D1 * t54 + 0.16D
     #2 / 0.9D1 * t131 - 0.8D1 / 0.9D1 * t121 + t269 * t2
      t284 = (0.61D2 / 0.6D1 - 0.4D1 / 0.3D1 * t34) * t101 + ((-0.73D2 /
     # 0.6D1 + (0.6D1 * S13 - t92 - t93 + 0.40D2 / 0.3D1 * S24) * t2) * 
     #t23 - 0.6388D4 / 0.9D1 * S13 - t82 - 0.6829D4 / 0.9D1 * S24 + t85 
     #+ t100) * t68 + t233 * S34 + t271 * t23 + 0.4D1 / 0.3D1 * t229 + t
     #248 + 0.16D2 / 0.9D1 * t260 + 0.16D2 / 0.3D1 * t225 + 0.16D2 / 0.9
     #D1 * t256 + t245 + 0.4D1 / 0.9D1 * t127 * S24 - t262 + 0.8D1 * t18
     #2 - t265 - t249 + 0.8D1 / 0.9D1 * t252 + t255 - 0.4D1 / 0.3D1 * t2
     #58 + 0.8D1 / 0.3D1 * S14 * t127 - t251
      t286 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.48D2 * t2 
     #* S34 * t15 + ((-0.16D2 / 0.9D1 * S23 + 0.16D2 / 0.9D1 * S13) * t2
     #1 * t24 + 0.32D2 / 0.3D1 * t3 * t23 + 0.32D2 / 0.3D1 * S23) * t5) 
     #* t6 * t9 + (((-0.48D2 * t34 - 0.48D2 + 0.48D2 * t3) * S34 - t38 -
     # 0.32D2 / 0.9D1 * S24) * t15 + ((-0.32D2 / 0.9D1 * S13 - t38) * t2
     # * t24 + (-0.64D2 / 0.3D1 * S23 + (-0.32D2 / 0.3D1 * t47 - 0.32D2 
     #/ 0.3D1 * t48) * t2) * t23 - t55 - 0.32D2 / 0.3D1 * t47) * t5) * s
     # * z + (-0.94D2 / 0.9D1 * t62 * S34 - 0.4D1 / 0.3D1 + 0.61D2 / 0.6
     #D1 * t34) * S12 + 0.272D3 / 0.9D1 * t62 * t68 + (-0.784D3 / 0.9D1 
     #+ (-0.2243D4 / 0.18D2 * t2 + (0.272D3 / 0.9D1 * S24 - 0.188D3 / 0.
     #9D1 * S13 - 0.212D3 / 0.9D1 * S23 + 0.272D3 / 0.9D1 * S14) * t21) 
     #* t23) * S34 + (-0.73D2 / 0.6D1 + (-t82 - 0.6388D4 / 0.9D1 * S24 -
     # 0.6829D4 / 0.9D1 * S13 + t85) * t2) * t23 + 0.6D1 * S24 + 0.40D2 
     #/ 0.3D1 * S13 - t92 - t93 + t100 + t192 * t15 + t284 * t5
      rrgq2qgh11J6 = t286 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh11J7
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S12 + S13 + S23
      t2 = 0.1D1 / t1
      t3 = S23 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t16 = t1 ** 2
      t17 = 0.1D1 / t16
      t19 = S23 + S24 + S34
      t20 = t19 * S34
      t29 = 0.128D3 / 0.9D1 * S23
      t32 = 0.1D1 / S12
      t34 = 0.32D2 / 0.9D1 * S13
      t39 = S23 ** 2
      t40 = S23 * S24
      t46 = S23 * S13
      t54 = t17 * t19
      t57 = t2 * t19
      t61 = S34 ** 2
      t65 = 0.160D3 / 0.9D1 * S24
      t75 = 0.18580D5 / 0.9D1 * S14
      t77 = 0.7282D4 / 0.9D1 * S23
      t84 = t61 * S34
      t90 = 0.160D3 / 0.9D1 * S13
      t99 = 0.76D2 / 0.3D1 * S14
      t100 = 0.106D3 / 0.9D1 * S23
      t103 = S13 * S24
      t105 = S24 * S14
      t107 = S13 * S14
      t110 = S14 ** 2
      t113 = S13 ** 2
      t116 = S24 ** 2
      t118 = S14 * S23
      t132 = 0.7093D4 / 0.9D1 * S23
      t135 = 0.18574D5 / 0.9D1 * S14
      t136 = 0.7198D4 / 0.9D1 * t39
      t140 = 0.18572D5 / 0.9D1 * t118
      t141 = 0.8D1 / 0.9D1 * t110
      t142 = 0.6316D4 / 0.9D1 * t103
      t149 = 0.7D1 * t39
      t153 = 0.16D2 / 0.9D1 * t110
      t156 = 0.2D1 / 0.9D1 * t118
      t158 = t39 * S23
      t160 = S23 * t110
      t162 = t39 * S14
      t165 = (0.4D1 * t158 + 0.4D1 * t160 - 0.8D1 * t162) * t2
      t166 = 0.40D2 / 0.9D1 * t54 * t84 + (0.65D2 / 0.3D1 + (0.160D3 / 0
     #.9D1 * t2 + (-0.160D3 / 0.9D1 * S23 + 0.80D2 / 0.9D1 * S24 - t90 +
     # 0.80D2 / 0.9D1 * S14) * t17) * t19) * t61 + ((-0.48D2 + (0.65D2 /
     # 0.3D1 * S13 + t99 + t65 + t100) * t2 + (-0.160D3 / 0.9D1 * t103 +
     # 0.80D2 / 0.9D1 * t105 - 0.160D3 / 0.9D1 * t107 + 0.76D2 / 0.9D1 *
     # t39 + 0.40D2 / 0.9D1 * t110 - 0.160D3 / 0.9D1 * t40 + 0.52D2 / 0.
     #9D1 * t113 + 0.128D3 / 0.9D1 * t46 + 0.40D2 / 0.9D1 * t116 - 0.160
     #D3 / 0.9D1 * t118) * t17) * t19 + t100 + 0.65D2 / 0.3D1 * S24 + t9
     #9 + t90 + (0.16D2 * t39 + 0.16D2 * t110 - 0.32D2 * t118) * t2) * S
     #34 + (-t132 + 0.6305D4 / 0.9D1 * S24 + 0.784D3 * S13 + t135 + (t13
     #6 + 0.134D3 / 0.9D1 * t46 - 0.7052D4 / 0.9D1 * t113 - 0.6184D4 / 0
     #.3D1 * t107 - t140 - t141 - t142 - 0.20D2 / 0.9D1 * t105 - 0.2104D
     #4 / 0.3D1 * t40) * t2) * t19 + t149 - 0.16D2 / 0.9D1 * t113 - t116
     # / 0.9D1 + 0.2D1 / 0.9D1 * t40 - t153 - 0.2D1 / 0.9D1 * t105 - 0.3
     #2D2 / 0.9D1 * t107 + t156 + 0.64D2 / 0.9D1 * t46 + t165
      t197 = (-t132 + 0.784D3 * S24 + 0.6305D4 / 0.9D1 * S13 + t135 + (t
     #149 + 0.2D1 / 0.9D1 * t46 - t153 - 0.2D1 / 0.9D1 * t107 + t156 - t
     #113 / 0.9D1 - 0.32D2 / 0.9D1 * t105 - 0.16D2 / 0.9D1 * t116 + 0.64
     #D2 / 0.9D1 * t40) * t2) * t19 - 0.7052D4 / 0.9D1 * t116 - t141 - 0
     #.20D2 / 0.9D1 * t107 + t136 - 0.2104D4 / 0.3D1 * t46 - 0.6184D4 / 
     #0.3D1 * t105 + 0.134D3 / 0.9D1 * t40 - t140 - t142 + t165
      t209 = 0.28D2 / 0.9D1 * t158
      t212 = 0.8D1 / 0.3D1 * t162
      t213 = 0.32D2 / 0.9D1 * t160
      t215 = 0.16D2 / 0.9D1 * t46 * S24
      t216 = t105 * S23
      t219 = 0.16D2 / 0.9D1 * t107 * S24
      t224 = t113 * S23
      t226 = S24 * t39
      t228 = t116 * S23
      t230 = t107 * S23
      t232 = S13 * t39
      t236 = t209 + 0.4D1 / 0.9D1 * t113 * S13 + t212 - t213 - t215 - 0.
     #16D2 / 0.9D1 * t216 + t219 + 0.8D1 / 0.9D1 * t113 * S24 + 0.32D2 /
     # 0.9D1 * t110 * S13 - 0.4D1 / 0.3D1 * t224 + 0.8D1 * t226 + 0.32D2
     # / 0.9D1 * t228 - 0.16D2 / 0.3D1 * t230 + 0.4D1 / 0.3D1 * t232 + 0
     #.8D1 / 0.3D1 * S14 * t113
      t238 = -0.4D1 / 0.9D1 * t113 - 0.8D1 / 0.9D1 * t105 - 0.4D1 / 0.9D
     #1 * t116 + 0.16D2 / 0.3D1 * t118 + 0.56D2 / 0.3D1 * t39 - 0.32D2 /
     # 0.9D1 * t110 + 0.104D3 / 0.9D1 * t40 - 0.8D1 / 0.9D1 * t107 + 0.1
     #04D3 / 0.9D1 * t46 + 0.16D2 / 0.9D1 * t103 + t236 * t2
      t254 = (0.76D2 / 0.9D1 - 0.16D2 / 0.9D1 * t57) * t84 + ((-0.76D2 /
     # 0.9D1 + (0.32D2 / 0.9D1 * S23 - 0.32D2 / 0.9D1 * S24 - 0.32D2 / 0
     #.9D1 * S14) * t2) * t19 - 0.6980D4 / 0.9D1 * S24 - t75 - 0.2108D4 
     #/ 0.3D1 * S13 + t77) * t61 + t197 * S34 + t238 * t19 + t212 - 0.16
     #D2 / 0.3D1 * t216 + 0.32D2 / 0.9D1 * t224 + 0.32D2 / 0.9D1 * t110 
     #* S24 + t209 + 0.4D1 / 0.9D1 * t116 * S24 + t219 + 0.8D1 * t232 + 
     #0.4D1 / 0.3D1 * t226 - t213 + 0.8D1 / 0.9D1 * S13 * t116 - 0.16D2 
     #/ 0.9D1 * t230 - 0.4D1 / 0.3D1 * t228 + 0.8D1 / 0.3D1 * S14 * t116
     # - t215
      t256 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + ((0.16D2 / 0.
     #9D1 * S23 - 0.16D2 / 0.9D1 * S13) * t17 * t20 + 0.32D2 / 0.3D1 * t
     #3 * t19 + 0.32D2 / 0.3D1 * S23) * t5 * t6 * t9 + ((-t29 + 0.32D2 /
     # 0.9D1 * S24) * t32 + ((t34 - t29) * t2 * t20 + (-0.64D2 / 0.3D1 *
     # S23 + (-0.32D2 / 0.3D1 * t39 - 0.32D2 / 0.3D1 * t40) * t2) * t19 
     #- 0.32D2 / 0.3D1 * t46 - 0.32D2 / 0.3D1 * t39) * t5) * s * z + (0.
     #52D2 / 0.9D1 * t54 * S34 - 0.16D2 / 0.9D1 + 0.76D2 / 0.9D1 * t57) 
     #* S12 - 0.160D3 / 0.9D1 * t54 * t61 + (0.160D3 / 0.9D1 + (0.65D2 /
     # 0.3D1 * t2 + (-t65 + t29 - 0.160D3 / 0.9D1 * S14 + 0.104D3 / 0.9D
     #1 * S13) * t17) * t19) * S34 + (-0.76D2 / 0.9D1 + (-0.6980D4 / 0.9
     #D1 * S13 - t75 - 0.2108D4 / 0.3D1 * S24 + t77) * t2) * t19 - t34 +
     # 0.32D2 / 0.9D1 * S23 - 0.32D2 / 0.9D1 * S14 + t166 * t32 + t254 *
     # t5
      rrgq2qgh11J7 = t256 / pi * wd / z

      end function
  
 