  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.1D1 / s
      t7 = 0.3141592653589793D1 * t6
      t8 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t9 = x4 * 0.3141592653589793D1
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
      t23 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t25 = t22 ** 2
      t26 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t32 = 0.3141592653589793D1 * lh
      t38 = lh ** 2
      t40 = 0.3141592653589793D1 ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = 0.3141592653589793D1 * t42
      t44 = t6 * t26
      t45 = t43 * t44
      t47 = 0.1D1 / x2
      t50 = t11 * t14
      t53 = log(-0.4D1 * t50 * t19)
      t54 = t53 * 0.3141592653589793D1
      t57 = t53 ** 2
      t58 = t57 * 0.3141592653589793D1
      t64 = rrgq2qgh11J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t89 = x1 ** 2
      t90 = x2 * t89
      t92 = t14 * t17
      t93 = x3 * t4
      t97 = log(-0.4D1 * t90 * t11 * t92 * t93)
      t105 = 0.1D1 / x1
      t109 = t89 * t11
      t113 = log(-0.4D1 * t109 * t14 * t19)
      t115 = t113 ** 2
      t129 = (0.90D2 * t7 * (-t8 + t22 * t23 - t25 * t26 / 0.2D1) - 0.18
     #0D3 * t32 * t6 * (-t23 + t22 * t26) - t45) * t47 / 0.1440D4 - (0.1
     #80D3 * t54 * lh + 0.45D2 * t58 + t43) * t6 * t23 / 0.1440D4 - t7 *
     # t64 / 0.16D2 - (-0.90D2 * t58 * lh + 0.3141592653589793D1 * (0.60
     #D2 * lh * t40 - 0.2884936567583026D3 - 0.120D3 * t38 * lh) - 0.15D
     #2 * t57 * t53 * 0.3141592653589793D1 - t54 * t42) * t6 * t26 / 0.1
     #440D4 - (-0.180D3 * t32 - 0.90D2 * t54) * t6 * t8 / 0.1440D4 + (0.
     #90D2 * t7 * (-t23 + t97 * t26) + 0.180D3 * t32 * t44) * t105 * t47
     # / 0.720D3 + (0.90D2 * t7 * (-t8 + t113 * t23 - t115 * t26 / 0.2D1
     #) - 0.180D3 * t32 * t6 * (-t23 + t113 * t26) - t45) * t105 / 0.720
     #D3
      t130 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #129)
      t132 = -0.1D1 + x1
      t133 = x3 * x1
      t134 = t133 * z
      t136 = 0.2D1 * x2 * x3
      t137 = t133 * x2
      t138 = x2 * z
      t139 = t133 * t138
      t140 = cos(t9)
      t141 = -0.1D1 + x2
      t143 = x1 * z
      t144 = 0.1D1 - x1 + t143
      t148 = Sqrt(x3 * t141 * t144 * x2 * t4)
      t150 = 0.2D1 * t140 * t148
      t153 = 0.1D1 / t144
      t156 = t2 * t133
      t157 = x1 * x2
      t158 = t157 * z
      t159 = 0.1D1 - x1 + t143 - x2 + t157 - t158 - x3 + t133 - t134 + t
     #136 - t137 + t139 + t150
      t164 = t2 * x1 * t4
      t171 = 0.1D1 / (-0.1D1 + x1 - t143 + x2 - t157 - t138 + t158)
      t172 = t144 * t171
      t173 = -t132
      t174 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, t173, x2, x3, x4)
      t178 = t132 ** 2
      t179 = t153 * t178
      t184 = log(0.4D1 * t90 * t50 * t17 * t93 * t179 * t141)
      t186 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, t173, x2, x3, x4)
      t196 = 0.90D2 * t7 * (t172 * t174 - t184 * t144 * t171 * t186) - 0
     #.180D3 * t32 * t6 * t172 * t186
      t200 = FJET(XB1, XB2, s, t2 * t132 * (-x3 + t133 - t134 + t136 - t
     #137 + t139 - x2 + t150) * t153, t156, -t2 * t132 * t159 * t153, -t
     #164, -s * t16 * x2 * x1 * t132 * t153, t196 * t105 * t47 / 0.720D3
     #)
      t207 = Sqrt(x2 * t141 * t93)
      t209 = 0.2D1 * t140 * t207
      t215 = 0.1D1 / (0.1D1 - x2 + t138)
      t216 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t219 = t18 * t4 * t141
      t222 = log(0.4D1 * t15 * t219)
      t223 = t222 * t215
      t224 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t226 = t222 ** 2
      t228 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t234 = t215 * t224
      t241 = t6 * t215 * t228
      t246 = t90 * t50
      t249 = log(0.4D1 * t246 * t219)
      t261 = (0.90D2 * t7 * (t215 * t216 - t223 * t224 + t226 * t215 * t
     #228 / 0.2D1) - 0.180D3 * t32 * t6 * (t234 - t223 * t228) + t43 * t
     #241) * t47 / 0.1440D4 + (0.90D2 * t7 * (t234 - t249 * t215 * t228)
     # - 0.180D3 * t32 * t241) * t105 * t47 / 0.720D3
      t262 = FJET(XB1, XB2, s, -t2 * (-x3 + t136 - x2 + t209), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t136 + t209), 0.0D0, 0.0D0, t261)
      t268 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, t173, 0.0D0, x3, x
     #4)
      t274 = log(-0.4D1 * t246 * t18 * t4 * t153 * t178)
      t275 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, t173, 0.0D0, x3, x
     #4)
      t280 = t6 * t275
      t286 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, t173, 0.0D0, x3, x
     #4)
      t291 = log(-0.4D1 * t109 * t92 * t93 * t179)
      t293 = t291 ** 2
      t308 = (0.90D2 * t7 * (t268 - t274 * t275) - 0.180D3 * t32 * t280)
     # * t105 * t47 / 0.720D3 + (0.90D2 * t7 * (t286 - t291 * t268 + t29
     #3 * t275 / 0.2D1) - 0.180D3 * t32 * t6 * (t268 - t291 * t275) + t4
     #3 * t280) * t105 / 0.720D3
      t309 = FJET(XB1, XB2, s, -t2 * t132 * x3, t156, t2 * t132 * t4, -t
     #164, 0.0D0, t308)
      rrgq2qght1s1e1 = t130 * t129 + t200 * t196 * t105 * t47 / 0.720D3 
     #+ t262 * t261 + t309 * t308

      end function



      doubleprecision function rrgq2qght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.1D1 / s
      t7 = 0.3141592653589793D1 * t6
      t8 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t9 = x4 * 0.3141592653589793D1
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
      t23 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t28 = 0.3141592653589793D1 * lh
      t31 = 0.180D3 * t28 * t6 * t23
      t33 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t41 = x1 ** 2
      t42 = t41 * t11
      t46 = log(-0.4D1 * t42 * t14 * t19)
      t54 = rrgq2qgh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t61 = log(-0.4D1 * t11 * t14 * t19)
      t62 = t61 * 0.3141592653589793D1
      t70 = t61 ** 2
      t73 = lh ** 2
      t75 = 0.3141592653589793D1 ** 2
      t83 = (0.90D2 * t7 * (-t8 + t22 * t23) + t31) * t33 / 0.1440D4 - t
     #7 * t23 * t36 * t33 / 0.8D1 + (0.90D2 * t7 * (-t8 + t46 * t23) + t
     #31) * t36 / 0.720D3 - t7 * t54 / 0.16D2 - (-0.180D3 * t28 - 0.90D2
     # * t62) * t6 * t8 / 0.1440D4 - (0.180D3 * t62 * lh + 0.45D2 * t70 
     #* 0.3141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t73 - 0
     #.30D2 * t75)) * t6 * t23 / 0.1440D4
      t84 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #3)
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
      t110 = t2 * t87
      t111 = x1 * x2
      t112 = t111 * z
      t113 = 0.1D1 - x1 + t97 - x2 + t111 - t112 - x3 + t87 - t88 + t90 
     #- t91 + t93 + t104
      t118 = t2 * x1 * t4
      t127 = -t86
      t128 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, x3, x4)
      t131 = 0.1D1 / (-0.1D1 + x1 - t97 + x2 - t111 - t92 + t112) * t128
     # * t36 * t33
      t134 = FJET(XB1, XB2, s, t2 * t86 * (-x3 + t87 - t88 + t90 - t91 +
     # t93 - x2 + t104) * t107, t110, -t2 * t86 * t113 * t107, -t118, -s
     # * t16 * x2 * x1 * t86 * t107, t7 * t98 * t131 / 0.8D1)
      t141 = x3 * t4
      t143 = Sqrt(x2 * t95 * t141)
      t145 = 0.2D1 * t94 * t143
      t151 = 0.1D1 / (0.1D1 - x2 + t92)
      t152 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t158 = log(0.4D1 * t15 * t18 * t4 * t95)
      t160 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t177 = (0.90D2 * t7 * (t151 * t152 - t158 * t151 * t160) - 0.180D3
     # * t28 * t6 * t151 * t160) * t33 / 0.1440D4 + t7 * t151 * t160 * t
     #36 * t33 / 0.8D1
      t178 = FJET(XB1, XB2, s, -t2 * (-x3 + t90 - x2 + t145), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t90 + t145), 0.0D0, 0.0D0, t177)
      t184 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, t127, 0.0D0, x3, x
     #4)
      t189 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, t127, 0.0D0, x3, x
     #4)
      t192 = t86 ** 2
      t197 = log(-0.4D1 * t42 * t14 * t17 * t141 * t107 * t192)
      t208 = t7 * t184 * t36 * t33 / 0.8D1 + (0.90D2 * t7 * (t189 - t197
     # * t184) - 0.180D3 * t28 * t6 * t184) * t36 / 0.720D3
      t209 = FJET(XB1, XB2, s, -t2 * t86 * x3, t110, t2 * t86 * t4, -t11
     #8, 0.0D0, t208)
      rrgq2qght1s1e0 = t84 * t83 + t134 * 0.3141592653589793D1 * t6 * t9
     #8 * t131 / 0.8D1 + t178 * t177 + t209 * t208

      end function



      doubleprecision function rrgq2qght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.1D1 / s
      t7 = 0.3141592653589793D1 * t6
      t8 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t9 = 0.1D1 / x1
      t13 = rrgq2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t18 = x4 * 0.3141592653589793D1
      t19 = Sin(t18)
      t20 = t19 ** 2
      t21 = z ** 2
      t24 = t1 ** 2
      t25 = t24 ** 2
      t30 = log(-0.4D1 * t20 / t21 * t25 * x3 * t4)
      t37 = 0.1D1 / x2
      t41 = -t7 * t8 * t9 / 0.8D1 - t7 * t13 / 0.16D2 - (-0.180D3 * 0.31
     #41592653589793D1 * lh - 0.90D2 * t30 * 0.3141592653589793D1) * t6 
     #* t8 / 0.1440D4 - t7 * t8 * t37 / 0.16D2
      t42 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #1)
      t44 = -0.1D1 + x1
      t54 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, -t44, 0.0D0, x3, x4
     #)
      t58 = FJET(XB1, XB2, s, -t2 * t44 * x3, t2 * x1 * x3, t2 * t44 * t
     #4, -t2 * x1 * t4, 0.0D0, t7 * t54 * t9 / 0.8D1)
      t65 = 0.2D1 * x2 * x3
      t66 = cos(t18)
      t71 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t73 = 0.2D1 * t66 * t71
      t81 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4)
      t83 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t81 * t37
      t86 = FJET(XB1, XB2, s, -t2 * (-x3 + t65 - x2 + t73), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t65 + t73), 0.0D0, 0.0D0, t7 * t83 / 0.16D2)
      rrgq2qght1s1em1 = t42 * t41 + t58 * 0.3141592653589793D1 * t6 * t5
     #4 * t9 / 0.8D1 + t86 * 0.3141592653589793D1 * t6 * t83 / 0.16D2

      end function



      doubleprecision function rrgq2qght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = s * (-0.1D1 + z)
      t6 = 0.1D1 / s
      t8 = rrgq2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t11 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, -0.3141592653589793D1 * t6 * t8 / 0.16D2)
      rrgq2qght1s1em2 = -t11 * 0.3141592653589793D1 * t6 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgq2qght1s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgq2qght1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh11J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t23 ** 2
      t34 = t32 * t33
      t35 = t30 * t34
      t38 = z + x1 * t7 * t1
      t39 = t4 ** 2
      t40 = 0.1D1 / t39
      t41 = t38 * t40
      t42 = x1 * t20
      t47 = t28 ** 2
      t48 = t31 ** 2
      t49 = t48 * t31
      t50 = t47 * t49
      t51 = x2 ** 2
      t52 = x1 ** 2
      t53 = t51 * t52
      t54 = t50 * t53
      t55 = t33 ** 2
      t57 = 0.1D1 / t39 / t4
      t58 = t55 * t57
      t59 = t26 * t38
      t64 = t48 * t1
      t65 = t47 * t64
      t66 = x1 * x2
      t67 = t65 * t66
      t68 = t55 * t40
      t69 = t10 ** 2
      t74 = t47 * s
      t75 = t74 * t64
      t76 = x2 * t52
      t77 = t75 * t76
      t78 = t33 * t23
      t79 = t78 * t57
      t80 = t38 * t20
      t81 = t80 * t10
      t85 = t26 * t47
      t86 = t64 * x2
      t87 = t85 * t86
      t88 = t52 * x1
      t89 = t88 * t33
      t93 = t10 * t7 * t4 + x2 * x3 + t19
      t94 = t93 ** 2
      t95 = t57 * t94
      t100 = t29 * t31
      t102 = t23 * t5
      t107 = t47 * t48
      t110 = t57 * t26
      t111 = t38 * z
      t116 = t74 * t49
      t117 = t51 * t88
      t118 = t116 * t117
      t119 = t39 ** 2
      t120 = 0.1D1 / t119
      t121 = t78 * t120
      t122 = t38 * t93
      t126 = t74 * t48
      t127 = t126 * t76
      t128 = t33 * t57
      t132 = t33 * t38
      t133 = t107 * t132
      t138 = t52 * t78
      t139 = t40 * t93
      t145 = t85 * t32 * t52
      t146 = t20 ** 2
      t151 = t31 * t23
      t152 = t30 * t151
      t153 = x3 * x1
      t154 = t5 * t20
      t158 = t29 * t48
      t159 = t158 * t76
      t160 = t27 * t38
      t165 = t85 * t34
      t171 = t38 * t5
      t181 = t40 * t26
      t186 = t30 * z
      t187 = t1 * t23
      t188 = t187 * x3
      t191 = -0.128D3 * t35 * t41 * t42 * t10 - 0.16D2 * t54 * t58 * t59
     # * t10 + 0.748D3 * t67 * t68 * t59 * t69 + 0.1088D4 * t77 * t79 * 
     #t81 + 0.216D3 * t87 * t89 * t95 * t10 + 0.1728D4 * t100 * t66 * t1
     #02 * t27 * z + 0.128D3 * t107 * t76 * t33 * t110 * t111 * t20 + 0.
     #544D3 * t118 * t121 * t122 + 0.752D3 * t127 * t128 * t80 + 0.1632D
     #4 * t133 * t110 * t76 * t93 + 0.144D3 * t87 * t138 * t139 * t69 - 
     #0.28D2 * t145 * t40 * t146 * t24 + 0.96D2 * t152 * t153 * t154 - 0
     #.17D2 * t159 * t128 * t160 * t20 + 0.3523D4 * t165 * t41 * t66 + 0
     #.10D2 * t47 * t1 * t23 * t171 * t26 + 0.148D3 * t54 * t58 * t59 * 
     #x3 - 0.128D3 * t107 * t66 * t78 * t181 * t111 * x3 + 0.128D3 * t18
     #6 * t188
      t192 = t30 * t31
      t197 = t30 * t1
      t198 = t23 * t38
      t202 = t85 * t32
      t203 = t88 * t57
      t208 = t85 * t1
      t211 = t6 * t93
      t214 = t94 * t93
      t222 = t85 * t31
      t223 = t52 * t40
      t224 = t223 * t146
      t229 = t223 * t94
      t234 = t187 * t10
      t237 = t126 * t66
      t238 = t78 * t40
      t239 = t38 * x3
      t244 = t65 * t76 * t78
      t248 = t75 * t66
      t249 = t239 * t10
      t253 = t78 * t38
      t258 = x2 * t88
      t259 = t65 * t258
      t260 = t33 * t120
      t267 = x3 ** 2
      t276 = 0.94D2 * t192 * t33 * x3 * t10 - 0.122D3 * t197 * t198 * t5
     # + 0.36D2 * t202 * t203 * t146 * t20 + 0.108D3 * t208 * t21 - 0.24
     #0D3 * t197 * t211 + 0.144D3 * t202 * t203 * t214 + 0.144D3 * t208 
     #* t211 + 0.16D2 * t197 * t21 + 0.108D3 * t222 * t224 - 0.748D3 * t
     #192 * t224 + 0.216D3 * t222 * t229 - 0.312D3 * t192 * t229 - 0.128
     #D3 * t186 * t234 + 0.848D3 * t237 * t238 * t239 + 0.96D2 * t244 * 
     #t110 * t81 + 0.1088D4 * t248 * t68 * t249 - 0.1244D4 * t65 * t253 
     #* t110 * t53 + 0.47D2 * t259 * t260 * t59 * t146 + 0.36D2 * t85 * 
     #t32 * x1 * t154 * t33 * t267 + 0.47D2 * t67 * t68 * t59 * t267
      t278 = t158 * t66
      t284 = t85 * t32 * x2
      t286 = t40 * t20
      t290 = t78 * t5
      t291 = t27 * x3
      t296 = t48 * x2
      t297 = t85 * t296
      t298 = t52 * t33
      t310 = z ** 2
      t312 = t74 * t310 * t296
      t313 = x1 * t78
      t318 = t38 * t10
      t326 = t116 * t53
      t330 = t31 * x1
      t335 = t29 * t32
      t336 = t335 * t66
      t337 = t33 * t40
      t345 = t75 * t258
      t354 = x1 * t55
      t355 = t5 * t69
      t360 = t48 * t32
      t361 = t51 * x2
      t363 = t85 * t360 * t361
      t364 = t88 * t55
      t365 = t57 * x3
      t369 = t88 * t23
      t374 = t33 * t5
      t387 = 0.275D3 * t278 * t238 * t160 * x3 - 0.1152D4 * t284 * t52 *
     # t23 * t286 - 0.530D3 * t278 * t290 * t291 * t10 + 0.864D3 * t297 
     #* t298 * t286 * t10 - 0.168D3 * t85 * t32 * t23 * x3 * t52 * t40 *
     # t94 + 0.64D2 * t312 * t313 * t41 * x3 - 0.1088D4 * t237 * t238 * 
     #t318 - 0.272D3 * t248 * t68 * t38 * t69 + 0.544D3 * t326 * t58 * t
     #318 - 0.56D2 * t85 * t330 * t154 * t24 - 0.7896D4 * t336 * t337 * 
     #t160 + 0.528D3 * t159 * t128 * t160 * t93 - 0.376D3 * t345 * t260 
     #* t38 * t146 - 0.3523D4 * t133 * t110 * t76 * t20 - 0.28D2 * t87 *
     # t354 * t355 * x3 - 0.28D2 * t363 * t364 * t365 - 0.576D3 * t297 *
     # t369 * t57 * t146 + 0.3466D4 * t336 * t374 * t291 - 0.96D2 * t65 
     #* t66 * t55 * t181 * t249 + 0.306D3 * t278 * t238 * t160 * t10
      t388 = t57 * t38
      t393 = t107 * t253
      t398 = t239 * t20
      t402 = x1 * t33
      t408 = t47 * t32
      t409 = t408 * t132
      t414 = t408 * t198
      t419 = t47 * t31
      t420 = t419 * t198
      t427 = t50 * t117
      t434 = x1 * t23
      t442 = t30 * t330
      t447 = t5 * t10
      t454 = t80 * t93
      t467 = t85 * t64 * t51
      t472 = t27 * t10
      t478 = t85 * t49 * t51
      t479 = t52 * t55
      t488 = -0.64D2 * t312 * t298 * t388 * t20 - 0.3466D4 * t393 * t181
     # * t66 * x3 - 0.94D2 * t244 * t110 * t398 + 0.1728D4 * t284 * t402
     # * t5 * z * x3 - 0.1728D4 * t409 * t181 * t66 * z + 0.430D3 * t414
     # * t110 * t52 * t146 - 0.328D3 * t420 * t181 * t42 - 0.576D3 * t29
     #7 * t313 * t355 - 0.148D3 * t427 * t121 * t59 * t20 - 0.1728D4 * t
     #85 * t31 * x2 * t434 * t5 * t310 + 0.306D3 * t152 * t41 * t42 - 0.
     #96D2 * t442 * t154 * t23 * t10 + 0.864D3 * t284 * t402 * t447 + 0.
     #128D3 * t65 * t258 * t33 * t120 * t26 * t454 + 0.312D3 * t259 * t2
     #60 * t59 * t94 - 0.430D3 * t278 * t290 * t27 * t69 - 0.1152D4 * t4
     #67 * t138 * t40 * t10 - 0.584D3 * t159 * t337 * t472 * t93 - 0.56D
     #2 * t478 * t479 * t40 * x3 * t10 - 0.1088D4 * t118 * t121 * t80
      t491 = t29 * t64
      t492 = t491 * t53
      t496 = t335 * t76
      t497 = t23 * t40
      t498 = t27 * t93
      t505 = x1 * t93
      t506 = t505 * x3
      t517 = t122 * x3
      t521 = t31 * t33
      t525 = t521 * t267
      t533 = t267 * x3
      t572 = 0.328D3 * t492 * t238 * t472 - 0.1632D4 * t496 * t497 * t49
     #8 + 0.3523D4 * t336 * t374 * t472 + 0.400D3 * t409 * t181 * t506 -
     # 0.112D3 * t85 * t86 * t52 * t238 * t93 * t10 * x3 - 0.128D3 * t24
     #4 * t110 * t517 - 0.47D2 * t30 * t521 * t69 + 0.36D2 * t85 * t525 
     #- 0.47D2 * t30 * t525 - 0.148D3 * t30 * t188 - 0.28D2 * t85 * t32 
     #* t78 * t533 - 0.28D2 * t85 * t188 + 0.148D3 * t30 * t234 - 0.1244
     #D4 * t496 * t497 * t27 * t20 - 0.1088D4 * t326 * t58 * t239 - 0.47
     #2D3 * t248 * t68 * t38 * t267 - 0.272D3 * t74 * t360 * t361 * t364
     # * t120 * t38 + 0.1088D4 * t75 * t51 * t138 * t388 - 0.376D3 * t74
     # * t32 * x2 * t402 * t41 + 0.192D3 * t408 * t78 * t171 * t26 * t26
     #7
      t588 = t361 * t88
      t598 = t223 * t20 * t93
      t603 = t48 ** 2
      t605 = t51 ** 2
      t606 = t52 ** 2
      t608 = t55 * t120
      t617 = t88 * t78
      t658 = -0.182D3 * t419 * t33 * t171 * t26 * x3 + 0.144D3 * t202 * 
     #t203 * t93 * t146 - 0.3523D4 * t158 * t51 * t298 * t40 * t27 - 0.5
     #76D3 * t85 * t49 * t588 * t79 + 0.1244D4 * t100 * x2 * t434 * t5 *
     # t27 + 0.288D3 * t222 * t598 - 0.880D3 * t192 * t598 + 0.36D2 * t8
     #5 * t603 * t605 * t606 * t608 - 0.576D3 * t222 * t66 * t102 - 0.10
     #D2 * t29 * t49 * t361 * t617 * t57 * t27 - 0.17D2 * t192 * t132 * 
     #t447 + 0.864D3 * t85 * t48 * t53 * t337 + 0.275D3 * t192 * t132 * 
     #t5 * x3 + 0.216D3 * t202 * t203 * t94 * t20 - 0.192D3 * t278 * t29
     #0 * t27 * t267 + 0.182D3 * t492 * t238 * t291 - 0.28D2 * t87 * t35
     #4 * t5 * t533 + 0.108D3 * t363 * t364 * t57 * t10 - 0.848D3 * t77 
     #* t79 * t398 - 0.122D3 * t492 * t79 * t160
      t667 = t40 * t267
      t672 = t318 * t93
      t677 = t5 * t93
      t745 = -0.328D3 * t420 * t181 * t505 + 0.56D2 * t414 * t110 * t52 
     #* t94 + 0.144D3 * t87 * t138 * t667 * t93 + 0.880D3 * t244 * t110 
     #* t672 - 0.112D3 * t85 * t151 * t153 * t677 - 0.128D3 * t152 * t10
     # * x1 * t677 + 0.144D3 * t165 * t267 * x1 * t677 + 0.864D3 * t467 
     #* t89 * t57 * t20 - 0.576D3 * t297 * t313 * t5 * t267 - 0.272D3 * 
     #t345 * t260 * t38 * t94 - 0.56D2 * t158 * t258 * t23 * t57 * t27 *
     # t94 + 0.328D3 * t491 * t117 * t128 * t498 + 0.216D3 * t478 * t606
     # * t33 * t120 * t94 + 0.144D3 * t363 * t606 * t78 * t120 * t93 + 0
     #.224D3 * t297 * t369 * t95 - 0.112D3 * t145 * t139 * t20 * t23 * x
     #3 - 0.168D3 * t87 * t89 * t365 * t94 + 0.144D3 * t87 * t606 * t23 
     #* t120 * t214 - 0.112D3 * t478 * t617 * t365 * t93 - 0.1088D4 * t1
     #27 * t128 * t122
      t817 = 0.1088D4 * t77 * t79 * t517 + 0.528D3 * t152 * t41 * t505 +
     # 0.530D3 * t409 * t181 * t153 * t20 - 0.12D2 * t47 * t360 * t588 *
     # t608 * t59 + 0.108D3 * t478 * t479 * t40 * t69 + 0.584D3 * t414 *
     # t110 * t52 * t20 * t93 + 0.240D3 * t427 * t121 * t59 * t93 + 0.28
     #8D3 * t478 * t617 * t57 * t93 * t10 + 0.36D2 * t87 * t354 * t5 * t
     #69 * t10 + 0.36D2 * t478 * t479 * t667 + 0.36D2 * t87 * t354 * t44
     #7 * t267 + 0.1244D4 * t393 * t181 * t66 * t10 - 0.544D3 * t77 * t7
     #9 * t672 + 0.36D2 * t85 + 0.12D2 * t30 + 0.1088D4 * t345 * t260 * 
     #t454 + 0.128D3 * t35 * t41 * t506 + 0.128D3 * t442 * t677 * t24 - 
     #0.400D3 * t159 * t337 * t498 * x3 - 0.128D3 * t297 * t298 * t139 *
     # x3
      rrgq2qgh11J1 = wd * (t191 + t276 + t387 + t488 + t572 + t658 + t74
     #5 + t817) / t28 / t27 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh11J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x2
      t10 = z + x1 * t7 * t3
      t12 = z + x1 * t3
      t13 = 0.1D1 / t12
      t14 = t10 * t13
      t15 = s * t3
      t16 = x1 * t13
      t17 = x3 * t7
      t19 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t12 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t12 + x2 * t19 - t28
      t30 = t16 * t29
      t32 = t5 * x3
      t34 = s - t15 * t30 - t15 * t32
      t36 = t2 * t3 * t5 * t14 * t34
      t38 = t2 * s
      t39 = t3 ** 2
      t40 = t39 ** 2
      t41 = t38 * t40
      t42 = x1 * x2
      t43 = t42 * t41
      t44 = t5 ** 2
      t45 = t44 * t5
      t46 = t12 ** 2
      t47 = 0.1D1 / t46
      t48 = t45 * t47
      t49 = t10 * x3
      t51 = t43 * t48 * t49
      t53 = t40 * t3
      t54 = t38 * t53
      t55 = t54 * t42
      t56 = t44 ** 2
      t57 = t56 * t47
      t58 = t49 * t19
      t60 = t55 * t57 * t58
      t62 = x1 ** 2
      t63 = t62 * x1
      t64 = x2 * t63
      t65 = t54 * t64
      t66 = t46 ** 2
      t67 = 0.1D1 / t66
      t68 = t44 * t67
      t69 = t29 ** 2
      t72 = t65 * t68 * t10 * t69
      t74 = t2 * t40
      t75 = t44 * t10
      t76 = t74 * t75
      t78 = 0.1D1 / t46 / t12
      t79 = t78 * t34
      t80 = x2 * t62
      t83 = t76 * t79 * t80 * t29
      t85 = t34 * t2
      t86 = t53 * x2
      t87 = t85 * t86
      t88 = x1 * t56
      t89 = t19 ** 2
      t90 = t13 * t89
      t95 = t40 * t39
      t96 = x2 ** 2
      t98 = t85 * t95 * t96
      t99 = t62 * t56
      t105 = s * t1
      t106 = t39 * t3
      t107 = t105 * t106
      t108 = t107 * t42
      t109 = t44 * t13
      t110 = t34 ** 2
      t111 = t110 * t19
      t113 = t108 * t109 * t111
      t115 = t105 * t40
      t116 = t115 * t42
      t117 = t45 * t13
      t118 = t110 * x3
      t121 = t116 * t117 * t118 * t19
      t123 = t110 * t105
      t124 = t39 * t44
      t126 = t123 * t124 * t89
      t128 = x3 ** 2
      t129 = t124 * t128
      t130 = t85 * t129
      t132 = t123 * t129
      t134 = t3 * t5
      t135 = t134 * x3
      t136 = t123 * t135
      t139 = t128 * x3
      t141 = t85 * t106 * t45 * t139
      t145 = t134 * t19
      t146 = t123 * t145
      t148 = t40 * x2
      t149 = t85 * t148
      t150 = t62 * t44
      t151 = t47 * t29
      t158 = t116 * t117 * t110 * t89
      t160 = t105 * t53
      t161 = t96 * t62
      t162 = t160 * t161
      t164 = t162 * t48 * t111
      t166 = 0.10D2 * t36 + 0.848D3 * t51 + 0.1088D4 * t60 - 0.376D3 * t
     #72 - 0.3523D4 * t83 - 0.28D2 * t87 * t88 * t90 * x3 - 0.56D2 * t98
     # * t99 * t47 * x3 * t19 + 0.3523D4 * t113 - 0.530D3 * t121 - 0.47D
     #2 * t126 + 0.36D2 * t130 - 0.47D2 * t132 - 0.148D3 * t136 - 0.28D2
     # * t141 - 0.28D2 * t85 * t135 + 0.148D3 * t146 + 0.864D3 * t149 * 
     #t150 * t151 * t19 - 0.430D3 * t158 + 0.328D3 * t164
      t176 = t40 * t106
      t177 = t96 * x2
      t179 = t85 * t176 * t177
      t180 = t63 * t56
      t185 = t107 * t80
      t186 = t5 * t47
      t190 = t19 * t7 * t12 + x2 * x3 + t28
      t191 = t110 * t190
      t193 = t185 * t186 * t191
      t195 = t2 * t106
      t196 = t5 * t10
      t197 = t195 * t196
      t201 = t197 * t79 * t62 * t29 * t190
      t203 = t2 * t95
      t204 = t96 * t63
      t205 = t203 * t204
      t206 = t45 * t67
      t207 = t34 * t10
      t210 = t205 * t206 * t207 * t190
      t212 = t63 * t45
      t218 = t63 * t44
      t219 = t190 ** 2
      t220 = t78 * t219
      t223 = t87 * t218 * t220 * t19
      t225 = t105 * t39
      t227 = t5 * t13
      t231 = 0.1728D4 * t225 * t42 * t227 * t110 * z
      t234 = t10 * z
      t237 = t74 * t80 * t44 * t79 * t234 * t29
      t241 = t47 * t34
      t244 = t74 * t42 * t45 * t241 * t234 * x3
      t246 = t115 * t80
      t247 = t44 * t47
      t250 = t246 * t247 * t191 * x3
      t252 = t47 * t190
      t255 = t149 * t150 * t252 * x3
      t259 = t76 * t79 * t80 * t190
      t261 = t62 * t45
      t268 = t246 * t247 * t111 * t190
      t273 = t190 * t19 * x3
      t275 = t85 * t86 * t62 * t48 * t273
      t277 = t2 * t53
      t279 = t277 * t80 * t45
      t280 = t10 * t190
      t281 = t280 * x3
      t283 = t279 * t79 * t281
      t285 = t39 * t5
      t286 = t123 * t285
      t287 = t10 * t47
      t288 = x1 * t190
      t290 = t286 * t287 * t288
      t292 = t195 * t75
      t293 = t288 * x3
      t295 = t292 * t241 * t293
      t297 = 0.36D2 * t87 * t88 * t13 * t89 * t19 + 0.108D3 * t98 * t99 
     #* t47 * t89 + 0.108D3 * t179 * t180 * t78 * t19 - 0.1632D4 * t193 
     #+ 0.584D3 * t201 + 0.240D3 * t210 + 0.288D3 * t98 * t212 * t78 * t
     #190 * t19 + 0.216D3 * t223 + t231 + 0.128D3 * t237 - 0.128D3 * t24
     #4 - 0.400D3 * t250 - 0.128D3 * t255 + 0.1632D4 * t259 + 0.144D3 * 
     #t87 * t261 * t252 * t89 - 0.584D3 * t268 - 0.112D3 * t275 - 0.128D
     #3 * t283 + 0.528D3 * t290 + 0.400D3 * t295
      t299 = t13 * t19
      t302 = t87 * t88 * t299 * t128
      t304 = t45 * t10
      t305 = t74 * t304
      t308 = t305 * t241 * t42 * t19
      t310 = t10 * t29
      t311 = t310 * t19
      t313 = t279 * t79 * t311
      t318 = t277 * t42 * t56 * t241 * t58
      t320 = t110 * t10
      t323 = t116 * t48 * t320 * t19
      t325 = z ** 2
      t327 = t38 * t325 * t148
      t328 = t78 * t10
      t331 = t327 * t150 * t328 * t29
      t335 = t305 * t241 * t42 * x3
      t337 = t49 * t29
      t339 = t279 * t79 * t337
      t341 = x1 * t29
      t343 = t286 * t287 * t341
      t345 = x3 * x1
      t348 = t292 * t241 * t345 * t29
      t351 = t177 * t63
      t353 = t56 * t67
      t355 = t2 * t176 * t351 * t353 * t207
      t357 = t45 * t78
      t359 = t162 * t357 * t320
      t361 = t54 * t80
      t363 = t361 * t357 * t337
      t367 = t277 * t304 * t79 * t161
      t369 = t277 * t64
      t372 = t369 * t68 * t207 * t69
      t376 = t13 * t29
      t379 = t85 * t106 * x1 * t376 * t44 * t128
      t381 = t106 * t44
      t382 = t123 * t381
      t383 = t341 * t19
      t387 = t203 * t161
      t388 = t56 * t78
      t391 = t387 * t388 * t207 * t19
      t393 = t277 * t42
      t396 = t393 * t57 * t207 * t89
      t399 = t361 * t357 * t311
      t401 = 0.36D2 * t302 + 0.1244D4 * t308 + 0.96D2 * t313 - 0.96D2 * 
     #t318 + 0.306D3 * t323 - 0.64D2 * t331 - 0.3466D4 * t335 - 0.94D2 *
     # t339 + 0.306D3 * t343 + 0.530D3 * t348 - 0.12D2 * t355 - 0.122D3 
     #* t359 - 0.848D3 * t363 - 0.1244D4 * t367 + 0.47D2 * t372 + 0.36D2
     # * t379 - 0.128D3 * t382 * t287 * t383 - 0.16D2 * t391 + 0.748D3 *
     # t396 + 0.1088D4 * t399
      t403 = t85 * t53 * t96
      t408 = t44 * t78
      t411 = t246 * t408 * t320 * t29
      t413 = t85 * t381
      t415 = t413 * t287 * t42
      t419 = t116 * t117 * t110 * t128
      t422 = t162 * t48 * t118
      t426 = t87 * t88 * t13 * t139
      t428 = t47 * t128
      t430 = t98 * t99 * t428
      t432 = t78 * x3
      t436 = t63 * t5
      t442 = t108 * t109 * t118
      t444 = t106 * t5
      t446 = x3 * t62
      t447 = t47 * t219
      t448 = t446 * t447
      t449 = t85 * t444 * t448
      t451 = t41 * t80
      t453 = t451 * t408 * t310
      t455 = t38 * t95
      t456 = t455 * t204
      t458 = t456 * t206 * t310
      t464 = x1 * t45
      t467 = t149 * t464 * t13 * t128
      t471 = t65 * t68 * t10 * t219
      t473 = t115 * t64
      t474 = t78 * t5
      t477 = t473 * t474 * t110 * t219
      t479 = t160 * t204
      t481 = t479 * t408 * t191
      t483 = t62 ** 2
      t485 = t219 * t190
      t489 = 0.144D3 * t87 * t483 * t5 * t67 * t485
      t493 = t98 * t483 * t44 * t67 * t219
      t495 = -0.1152D4 * t403 * t261 * t47 * t19 - 0.17D2 * t411 + 0.352
     #3D4 * t415 - 0.192D3 * t419 + 0.182D3 * t422 - 0.28D2 * t426 + 0.3
     #6D2 * t430 - 0.28D2 * t179 * t180 * t432 - 0.576D3 * t149 * t436 *
     # t78 * t69 + 0.3466D4 * t442 - 0.168D3 * t449 + 0.752D3 * t453 - 0
     #.1088D4 * t458 + 0.864D3 * t403 * t218 * t78 * t29 - 0.576D3 * t46
     #7 - 0.272D3 * t471 - 0.56D2 * t477 + 0.328D3 * t481 + t489 + 0.216
     #D3 * t493
      t504 = t149 * t436 * t220
      t506 = t10 * t19
      t507 = t506 * t190
      t509 = t361 * t357 * t507
      t511 = t310 * t190
      t513 = t65 * t68 * t511
      t518 = t39 * x1
      t519 = t123 * t518
      t520 = t13 * t190
      t522 = t519 * t520 * t32
      t526 = t85 * t285 * t345 * t520
      t528 = t19 * x1
      t530 = t286 * t528 * t520
      t534 = t413 * t128 * x1 * t520
      t537 = t279 * t79 * t507
      t539 = t106 * t62
      t540 = t85 * t539
      t541 = t29 * t5
      t543 = t252 * t541 * x3
      t544 = t540 * t543
      t548 = t87 * t218 * t432 * t219
      t554 = t98 * t212 * t432 * t190
      t557 = t451 * t408 * t280
      t560 = t361 * t357 * t281
      t564 = t246 * t408 * t320 * t190
      t567 = t456 * t206 * t280
      t573 = t277 * t64 * t44 * t67 * t34 * t511
      t575 = 0.144D3 * t179 * t483 * t45 * t67 * t190 + 0.224D3 * t504 -
     # 0.544D3 * t509 + 0.1088D4 * t513 + 0.128D3 * t382 * t287 * t293 +
     # 0.128D3 * t522 - 0.112D3 * t526 - 0.128D3 * t530 + 0.144D3 * t534
     # + 0.880D3 * t537 - 0.112D3 * t544 - 0.168D3 * t548 + 0.36D2 * t85
     # + 0.12D2 * t123 - 0.112D3 * t554 - 0.1088D4 * t557 + 0.1088D4 * t
     #560 + 0.528D3 * t564 + 0.544D3 * t567 + 0.128D3 * t573
      t578 = t369 * t68 * t207 * t219
      t580 = t2 * t39
      t581 = t580 * t196
      t583 = t581 * t241 * t288
      t585 = t62 * t219
      t587 = t197 * t79 * t585
      t591 = t87 * t261 * t428 * t190
      t593 = t5 * t19
      t595 = t519 * t376 * t593
      t597 = t47 * t69
      t598 = t597 * t32
      t602 = t286 * t345 * t376
      t609 = t108 * t247 * t320
      t613 = t393 * t57 * t207 * t128
      t617 = t116 * t48 * t320 * x3
      t620 = t85 * t106 * x2
      t625 = t62 * t69
      t627 = t197 * t79 * t625
      t630 = t581 * t241 * t341
      t635 = x1 * t44
      t641 = t387 * t388 * t207 * x3
      t645 = t205 * t206 * t207 * t29
      t649 = x1 * t5
      t653 = 0.1728D4 * t85 * t39 * x2 * t649 * t13 * t325
      t658 = 0.1728D4 * t620 * t635 * t13 * z * x3
      t659 = 0.312D3 * t578 - 0.328D3 * t583 + 0.56D2 * t587 + 0.144D3 *
     # t591 - 0.96D2 * t595 - 0.28D2 * t540 * t598 + 0.96D2 * t602 - 0.5
     #6D2 * t85 * t518 * t376 * t32 - 0.7896D4 * t609 + 0.47D2 * t613 + 
     #0.275D3 * t617 - 0.1152D4 * t620 * t62 * t5 * t151 + 0.430D3 * t62
     #7 - 0.328D3 * t630 - 0.576D3 * t149 * t464 * t90 + 0.864D3 * t620 
     #* t635 * t299 + 0.148D3 * t641 - 0.148D3 * t645 - t653 + t658
      t664 = 0.1728D4 * t292 * t241 * t42 * z
      t665 = t110 * t29
      t667 = t185 * t186 * t665
      t669 = t455 * t161
      t671 = t669 * t388 * t49
      t675 = t55 * t57 * t10 * t128
      t679 = t327 * t464 * t287 * x3
      t682 = t43 * t48 * t506
      t686 = t55 * t57 * t10 * t89
      t689 = t669 * t388 * t506
      t691 = t123 * z
      t692 = t691 * t135
      t694 = t123 * t39
      t697 = t694 * t44 * x3 * t19
      t699 = t123 * t3
      t701 = t699 * t196 * t13
      t703 = t85 * t106
      t704 = t63 * t78
      t709 = t85 * t3
      t712 = t16 * t190
      t713 = t699 * t712
      t717 = 0.144D3 * t703 * t704 * t485
      t720 = t699 * t30
      t722 = t85 * t39
      t723 = t62 * t47
      t724 = t723 * t69
      t727 = t694 * t724
      t729 = t723 * t219
      t730 = t722 * t729
      t732 = -t664 - 0.1244D4 * t667 - 0.1088D4 * t671 - 0.472D3 * t675 
     #+ 0.64D2 * t679 - 0.1088D4 * t682 - 0.272D3 * t686 + 0.544D3 * t68
     #9 + 0.128D3 * t692 + 0.94D2 * t697 - 0.122D3 * t701 + 0.36D2 * t70
     #3 * t704 * t69 * t29 + 0.108D3 * t709 * t30 - 0.240D3 * t713 + t71
     #7 + 0.144D3 * t709 * t712 + 0.16D2 * t720 + 0.108D3 * t722 * t724 
     #- 0.748D3 * t727 + 0.216D3 * t730
      t733 = t694 * t729
      t735 = t691 * t145
      t739 = t67 * t10
      t741 = t38 * t176 * t177 * t180 * t739
      t745 = t54 * t96 * t261 * t328
      t750 = t38 * t106 * x2 * t635 * t287
      t752 = t195 * t45
      t755 = t752 * t14 * t34 * t128
      t757 = t580 * t44
      t760 = t757 * t14 * t34 * x3
      t769 = t115 * t96 * t150 * t47 * t110
      t778 = t225 * x2 * t649 * t13 * t110
      t780 = t29 * t190
      t781 = t723 * t780
      t784 = t694 * t781
      t786 = t40 ** 2
      t788 = t96 ** 2
      t800 = t105 * t95 * t177 * t212 * t78 * t110
      t803 = t694 * t75 * t299
      t811 = t694 * t75 * t13 * x3
      t815 = t703 * t704 * t219 * t29
      t817 = -0.312D3 * t733 - 0.128D3 * t735 - 0.272D3 * t741 + 0.1088D
     #4 * t745 - 0.376D3 * t750 + 0.192D3 * t755 - 0.182D3 * t760 + 0.14
     #4D3 * t703 * t704 * t190 * t69 - 0.3523D4 * t769 - 0.576D3 * t85 *
     # t95 * t351 * t357 + 0.1244D4 * t778 + 0.288D3 * t722 * t781 - 0.8
     #80D3 * t784 + 0.36D2 * t85 * t786 * t788 * t483 * t353 - 0.576D3 *
     # t722 * t42 * t227 - 0.10D2 * t800 - 0.17D2 * t803 + 0.864D3 * t85
     # * t40 * t161 * t247 + 0.275D3 * t811 + 0.216D3 * t815
      t841 = -0.62D2 * t36 - 0.1360D4 * t51 - 0.1728D4 * t60 + 0.584D3 *
     # t72 - 0.5266D4 * t83 + 0.5266D4 * t113 + 0.444D3 * t121 + 0.54D2 
     #* t126 - 0.72D2 * t130 + 0.54D2 * t132 + 0.216D3 * t136 + 0.56D2 *
     # t141 - 0.216D3 * t146 + 0.542D3 * t158 - 0.604D3 * t164 + 0.4816D
     #4 * t193 - 0.544D3 * t201 + 0.64D2 * t210 - 0.72D2 * t223
      t860 = -t231 - 0.256D3 * t237 + 0.256D3 * t244 + 0.288D3 * t250 + 
     #0.1024D4 * t255 - 0.4816D4 * t259 + 0.544D3 * t268 + 0.144D3 * t27
     #5 + 0.496D3 * t283 - 0.752D3 * t290 - 0.288D3 * t295 - 0.72D2 * t3
     #02 - 0.3776D4 * t308 - 0.640D3 * t313 + 0.640D3 * t318 - 0.190D3 *
     # t323 + 0.128D3 * t331 - 0.4940D4 * t335 + 0.108D3 * t339
      t881 = -0.190D3 * t343 - 0.444D3 * t348 - 0.16D2 * t355 + 0.134D3 
     #* t359 + 0.1360D4 * t363 + 0.3776D4 * t367 - 0.54D2 * t372 - 0.72D
     #2 * t379 + 0.608D3 * t391 - 0.592D3 * t396 - 0.1728D4 * t399 + 0.4
     #90D3 * t411 + 0.5266D4 * t415 + 0.30D2 * t419 - 0.108D3 * t422 + 0
     #.56D2 * t426 - 0.72D2 * t430 + 0.4940D4 * t442 + 0.344D3 * t449
      t900 = -0.1168D4 * t453 + 0.1728D4 * t458 + 0.352D3 * t467 + 0.432
     #D3 * t471 - 0.24D2 * t477 - 0.256D3 * t481 - t489 - 0.72D2 * t493 
     #- 0.1376D4 * t504 + 0.864D3 * t509 - 0.1728D4 * t513 - 0.496D3 * t
     #522 + 0.144D3 * t526 + 0.496D3 * t530 - 0.256D3 * t534 - 0.640D3 *
     # t537 + 0.144D3 * t544 + 0.344D3 * t548 + 0.16D2 * t123
      t922 = 0.144D3 * t554 + 0.1728D4 * t557 - 0.1728D4 * t560 - 0.752D
     #3 * t564 - 0.864D3 * t567 - 0.496D3 * t573 - 0.264D3 * t578 + 0.25
     #6D3 * t583 + 0.24D2 * t587 - 0.256D3 * t591 + 0.640D3 * t595 - 0.6
     #40D3 * t602 - 0.9344D4 * t609 - 0.54D2 * t613 - 0.156D3 * t617 - 0
     #.542D3 * t627 + 0.604D3 * t630 - 0.216D3 * t641 + 0.216D3 * t645
      t938 = t653 - t658 + t664 + 0.3776D4 * t667 + 0.1728D4 * t671 + 0.
     #776D3 * t675 - 0.128D3 * t679 + 0.1728D4 * t682 + 0.432D3 * t686 -
     # 0.864D3 * t689 - 0.256D3 * t692 - 0.108D3 * t697 + 0.134D3 * t701
     # - 0.64D2 * t713 - t717 - 0.608D3 * t720 + 0.592D3 * t727 - 0.72D2
     # * t730 + 0.264D3 * t733
      t956 = t40 * t45
      t977 = t123 * t539
      t983 = t123 * t444
      t997 = t40 * t44
      t1023 = 0.256D3 * t735 + 0.256D3 * t292 * t241 * t383 + 0.256D3 * 
     #t195 * t304 * t13 * t34 * x3 * t19 - 0.64D2 * t85 * t40 * t56 * t1
     #4 * t89 * x3 + 0.64D2 * t85 * t956 * t287 * t341 * t89 - 0.256D3 *
     # t246 * t247 * t665 * t19 - 0.256D3 * t246 * t247 * t118 * t29 + 0
     #.256D3 * t479 * t408 * t665 - 0.136D3 * t473 * t474 * t110 * t69 +
     # 0.64D2 * t977 * t598 - 0.64D2 * t977 * t597 * t593 - 0.64D2 * t98
     #3 * t328 * t625 + 0.216D3 * t292 * t241 * t528 * t190 - 0.128D3 * 
     #t85 * t956 * t10 * t47 * x1 * t273 + 0.128D3 * t85 * t997 * t10 * 
     #t78 * t62 * t780 * t19 - 0.64D2 * t85 * t997 * t328 * t446 * t219 
     #+ 0.64D2 * t85 * t40 * t5 * t739 * t63 * t219 * t29 - 0.216D3 * t4
     #73 * t474 * t665 * t190 + 0.128D3 * t977 * t543
      t1060 = -0.128D3 * t977 * t252 * t541 * t19 + 0.64D2 * t983 * t448
     # - 0.64D2 * t977 * t447 * t593 + 0.128D3 * t983 * t328 * t585 + 0.
     #432D3 * t741 - 0.1728D4 * t745 + 0.584D3 * t750 - 0.30D2 * t755 + 
     #0.108D3 * t760 - 0.5266D4 * t769 - 0.3776D4 * t778 + 0.640D3 * t78
     #4 + 0.62D2 * t800 + 0.490D3 * t803 - 0.156D3 * t811 - 0.72D2 * t81
     #5 - 0.256D3 * t757 * t14 * t34 * t19 + 0.136D3 * t752 * t14 * t34 
     #* t89 - 0.64D2 * t123 * t106 * t304 * t90
      rrgq2qgh11J2 = (wd * (t166 + t297 + t401 + t495 + t575 + t659 + t7
     #32 + t817) + wd * (t841 + t860 + t881 + t900 + t922 + t938 + t1023
     # + t1060)) / t1 / t110 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh11J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t31 = t1 ** 2
      t32 = t31 * x1
      t33 = t30 * t32
      t34 = t5 * t20
      t35 = t23 * t10
      t37 = t33 * t34 * t35
      t39 = t28 ** 2
      t40 = t26 * t39
      t41 = t31 * t1
      t42 = x1 ** 2
      t43 = t42 * t41
      t44 = t40 * t43
      t45 = t4 ** 2
      t46 = 0.1D1 / t45
      t47 = t20 ** 2
      t48 = t46 * t47
      t49 = t48 * t24
      t52 = t31 * t23
      t53 = t30 * t52
      t54 = x3 * x1
      t56 = t53 * t54 * t34
      t62 = t29 * t41
      t63 = x1 * x2
      t64 = t62 * t63
      t65 = t23 ** 2
      t66 = t65 * t46
      t69 = z + x1 * t7 * t1
      t70 = t27 * t69
      t72 = t64 * t66 * t70
      t74 = t31 ** 2
      t75 = t74 * t1
      t76 = t39 * t75
      t77 = t76 * t63
      t78 = t65 ** 2
      t79 = t78 * t46
      t80 = t26 * t69
      t81 = x3 ** 2
      t84 = t77 * t79 * t80 * t81
      t87 = t39 * t1 * t23
      t88 = t69 * t5
      t90 = t87 * t88 * t26
      t92 = t39 * t41
      t93 = t23 * t69
      t94 = t92 * t93
      t96 = 0.1D1 / t45 / t4
      t97 = t96 * t26
      t101 = t10 * t7 * t4 + x2 * x3 + t19
      t102 = t101 ** 2
      t103 = t42 * t102
      t105 = t94 * t97 * t103
      t107 = t75 * x2
      t108 = t40 * t107
      t109 = t65 * t23
      t110 = t42 * t109
      t111 = t46 * t81
      t114 = t108 * t110 * t111 * t101
      t116 = t29 * t74
      t117 = t116 * t63
      t118 = t109 * t46
      t121 = t117 * t118 * t70 * t10
      t123 = t39 * s
      t124 = z ** 2
      t126 = t74 * x2
      t127 = t123 * t124 * t126
      t128 = t42 * t65
      t129 = t96 * t69
      t132 = t127 * t128 * t129 * t20
      t133 = 0.64D2 * t132
      t134 = t39 * t74
      t135 = t109 * t69
      t136 = t134 * t135
      t137 = t46 * t26
      t140 = t136 * t137 * t63 * x3
      t142 = x2 * t42
      t144 = t76 * t142 * t109
      t145 = t69 * x3
      t146 = t145 * t20
      t148 = t144 * t97 * t146
      t150 = t69 * t46
      t151 = x1 * t20
      t153 = t53 * t150 * t151
      t155 = t65 * t69
      t156 = t92 * t155
      t157 = t54 * t20
      t159 = t156 * t137 * t157
      t161 = t10 ** 2
      t164 = t77 * t79 * t80 * t161
      t166 = t123 * t75
      t167 = t166 * t142
      t168 = t109 * t96
      t169 = t69 * t20
      t170 = t169 * t10
      t172 = t167 * t168 * t170
      t174 = x2 ** 2
      t176 = t40 * t75 * t174
      t181 = t116 * t142
      t182 = t65 * t96
      t185 = t181 * t182 * t70 * t20
      t187 = -0.96D2 * t37 - 0.28D2 * t44 * t49 + 0.96D2 * t56 - 0.56D2 
     #* t40 * t32 * t34 * t24 - 0.7896D4 * t72 + 0.47D2 * t84 + 0.10D2 *
     # t90 + 0.56D2 * t105 + 0.144D3 * t114 + 0.306D3 * t121 - t133 - 0.
     #3466D4 * t140 - 0.94D2 * t148 + 0.306D3 * t153 + 0.530D3 * t159 + 
     #0.748D3 * t164 + 0.1088D4 * t172 - 0.1152D4 * t176 * t110 * t46 * 
     #t10 - 0.17D2 * t185
      t188 = t41 * t65
      t189 = t40 * t188
      t191 = t189 * t150 * t63
      t193 = t123 * t74
      t194 = t193 * t63
      t196 = t194 * t118 * t145
      t198 = t166 * t63
      t199 = t145 * t10
      t201 = t198 * t79 * t199
      t203 = t74 * t31
      t204 = t39 * t203
      t205 = t42 * x1
      t206 = t174 * t205
      t207 = t204 * t206
      t208 = t45 ** 2
      t209 = 0.1D1 / t208
      t210 = t109 * t209
      t213 = t207 * t210 * t80 * t20
      t217 = x1 * t23
      t221 = 0.1728D4 * t40 * t31 * x2 * t217 * t5 * t124
      t223 = t40 * t41 * x2
      t224 = x1 * t65
      t229 = 0.1728D4 * t223 * t224 * t5 * z * x3
      t233 = 0.1728D4 * t156 * t137 * t63 * z
      t234 = t62 * t142
      t235 = t23 * t46
      t236 = t27 * t20
      t238 = t234 * t235 * t236
      t240 = t123 * t203
      t241 = t174 * t42
      t242 = t240 * t241
      t243 = t78 * t96
      t245 = t242 * t243 * t145
      t249 = t198 * t79 * t69 * t81
      t251 = x1 * t109
      t254 = t127 * t251 * t150 * x3
      t255 = 0.64D2 * t254
      t256 = t69 * t10
      t258 = t194 * t118 * t256
      t262 = t198 * t79 * t69 * t161
      t265 = t40 * t203 * t174
      t266 = t205 * t109
      t267 = t96 * x3
      t270 = t265 * t266 * t267 * t101
      t272 = t193 * t142
      t273 = t69 * t101
      t275 = t272 * t182 * t273
      t277 = t273 * x3
      t279 = t167 * t168 * t277
      t283 = t181 * t182 * t70 * t101
      t285 = t240 * t206
      t287 = t285 * t210 * t273
      t289 = x2 * t205
      t293 = t169 * t101
      t295 = t76 * t289 * t65 * t209 * t26 * t293
      t297 = t76 * t289
      t298 = t65 * t209
      t301 = t297 * t298 * t80 * t102
      t303 = 0.3523D4 * t191 + 0.848D3 * t196 + 0.1088D4 * t201 - 0.148D
     #3 * t213 - t221 + t229 - t233 - 0.1244D4 * t238 - 0.1088D4 * t245 
     #- 0.472D3 * t249 + t255 - 0.1088D4 * t258 - 0.272D3 * t262 - 0.112
     #D3 * t270 - 0.1088D4 * t275 + 0.1088D4 * t279 + 0.528D3 * t283 + 0
     #.544D3 * t287 + 0.128D3 * t295 + 0.312D3 * t301
      t305 = t39 * t31
      t306 = t305 * t93
      t307 = x1 * t101
      t309 = t306 * t137 * t307
      t311 = t166 * t289
      t314 = t311 * t298 * t69 * t47
      t316 = t134 * t155
      t319 = t316 * t97 * t142 * t20
      t321 = x1 * t78
      t322 = t5 * t161
      t327 = t42 * t78
      t328 = t46 * x3
      t333 = t65 * t5
      t334 = t27 * t10
      t336 = t64 * t333 * t334
      t338 = t109 * t5
      t339 = t27 * x3
      t342 = t117 * t338 * t339 * t10
      t344 = t27 * t101
      t346 = t234 * t235 * t344
      t349 = t42 * t20 * t101
      t351 = t94 * t97 * t349
      t355 = t207 * t210 * t80 * t101
      t362 = t205 * t65
      t363 = t96 * t102
      t366 = t108 * t362 * t363 * t10
      t368 = t74 * t41
      t370 = t174 * x2
      t371 = t370 * t205
      t373 = t78 * t209
      t375 = t39 * t368 * t371 * t373 * t80
      t377 = t31 * t65
      t379 = t30 * t377 * t161
      t381 = t377 * t81
      t382 = t40 * t381
      t384 = t30 * t381
      t386 = t1 * t23
      t387 = t386 * x3
      t388 = t30 * t387
      t390 = t41 * t109
      t391 = t81 * x3
      t392 = t390 * t391
      t393 = t40 * t392
      t397 = t386 * t10
      t398 = t30 * t397
      t400 = -0.328D3 * t309 - 0.376D3 * t314 - 0.3523D4 * t319 - 0.28D2
     # * t108 * t321 * t322 * x3 - 0.56D2 * t265 * t327 * t328 * t10 + 0
     #.3523D4 * t336 - 0.530D3 * t342 - 0.1632D4 * t346 + 0.584D3 * t351
     # + 0.240D3 * t355 + 0.288D3 * t265 * t266 * t96 * t101 * t10 + 0.2
     #16D3 * t366 - 0.12D2 * t375 - 0.47D2 * t379 + 0.36D2 * t382 - 0.47
     #D2 * t384 - 0.148D3 * t388 - 0.28D2 * t393 - 0.28D2 * t40 * t387 +
     # 0.148D3 * t398
      t401 = t29 * t75
      t402 = t401 * t241
      t404 = t402 * t168 * t70
      t407 = t167 * t168 * t146
      t411 = t76 * t135 * t97 * t241
      t415 = t297 * t298 * t80 * t47
      t417 = t41 * x1
      t419 = t65 * t81
      t420 = t34 * t419
      t421 = t40 * t417 * t420
      t423 = t30 * t188
      t424 = t151 * t10
      t426 = t423 * t150 * t424
      t428 = t204 * t241
      t431 = t428 * t243 * t80 * t10
      t434 = t272 * t182 * t169
      t437 = t285 * t210 * t169
      t443 = t40 * t126
      t444 = t5 * t81
      t446 = t443 * t251 * t444
      t447 = 0.576D3 * t446
      t450 = t316 * t97 * t142 * t101
      t452 = t46 * t101
      t459 = t181 * t66 * t334 * t101
      t464 = t101 * t10 * x3
      t466 = t40 * t107 * t42 * t118 * t464
      t468 = t10 * x1
      t469 = t5 * t101
      t471 = t53 * t468 * t469
      t473 = t81 * x1
      t474 = t473 * t469
      t475 = t189 * t474
      t477 = t256 * t101
      t479 = t144 * t97 * t477
      t481 = t20 * t23
      t483 = t452 * t481 * x3
      t484 = t44 * t483
      t488 = t108 * t362 * t267 * t102
      t490 = -0.122D3 * t404 - 0.848D3 * t407 - 0.1244D4 * t411 + 0.47D2
     # * t415 + 0.36D2 * t421 - 0.128D3 * t426 - 0.16D2 * t431 + 0.752D3
     # * t434 - 0.1088D4 * t437 + 0.864D3 * t176 * t362 * t96 * t20 - t4
     #47 + 0.1632D4 * t450 + 0.144D3 * t108 * t110 * t452 * t161 - 0.584
     #D3 * t459 - 0.112D3 * t466 - 0.128D3 * t471 + 0.144D3 * t475 + 0.8
     #80D3 * t479 - 0.112D3 * t484 - 0.168D3 * t488
      t493 = t29 * t31
      t495 = t23 * t5
      t499 = 0.1728D4 * t493 * t63 * t495 * t27 * z
      t502 = t69 * z
      t505 = t134 * t142 * t65 * t97 * t502 * t20
      t506 = 0.128D3 * t505
      t511 = t134 * t63 * t109 * t137 * t502 * x3
      t515 = t181 * t66 * t344 * x3
      t519 = t443 * t128 * t452 * x3
      t522 = t144 * t97 * t277
      t525 = t53 * t150 * t307
      t527 = t307 * x3
      t529 = t156 * t137 * t527
      t531 = t5 * t10
      t534 = t108 * t321 * t531 * t81
      t538 = t136 * t137 * t63 * t10
      t541 = t144 * t97 * t170
      t546 = t76 * t63 * t78 * t137 * t199
      t550 = t117 * t338 * t27 * t81
      t553 = t402 * t118 * t339
      t555 = t5 * t391
      t557 = t108 * t321 * t555
      t560 = t265 * t327 * t111
      t563 = t40 * t368 * t370
      t564 = t205 * t78
      t569 = t242 * t243 * t256
      t573 = t311 * t298 * t69 * t102
      t575 = t116 * t289
      t576 = t23 * t96
      t579 = t575 * t576 * t27 * t102
      t581 = t499 + t506 - 0.128D3 * t511 - 0.400D3 * t515 - 0.128D3 * t
     #519 - 0.128D3 * t522 + 0.528D3 * t525 + 0.400D3 * t529 + 0.36D2 * 
     #t534 + 0.1244D4 * t538 + 0.96D2 * t541 - 0.96D2 * t546 - 0.192D3 *
     # t550 + 0.182D3 * t553 - 0.28D2 * t557 + 0.36D2 * t560 - 0.28D2 * 
     #t563 * t564 * t267 + 0.544D3 * t569 - 0.272D3 * t573 - 0.56D2 * t5
     #79
      t582 = t401 * t206
      t584 = t582 * t182 * t344
      t586 = t42 ** 2
      t588 = t102 * t101
      t592 = 0.144D3 * t108 * t586 * t23 * t209 * t588
      t596 = t265 * t586 * t65 * t209 * t102
      t603 = t205 * t23
      t605 = t443 * t603 * t363
      t608 = t167 * t168 * t477
      t613 = t311 * t298 * t293
      t616 = t423 * t150 * t527
      t619 = t33 * t469 * t24
      t623 = t40 * t52 * t54 * t469
      t625 = t30 * z
      t626 = t625 * t387
      t628 = t30 * t31
      t630 = t65 * x3 * t10
      t631 = t628 * t630
      t633 = t30 * t1
      t635 = t633 * t93 * t5
      t637 = t40 * t41
      t638 = t205 * t96
      t639 = t47 * t20
      t643 = t40 * t1
      t646 = t6 * t101
      t647 = t633 * t646
      t651 = 0.144D3 * t637 * t638 * t588
      t654 = 0.328D3 * t584 + t592 + 0.216D3 * t596 + 0.144D3 * t563 * t
     #586 * t109 * t209 * t101 + 0.224D3 * t605 - 0.544D3 * t608 + 0.36D
     #2 * t40 + 0.12D2 * t30 + 0.1088D4 * t613 + 0.128D3 * t616 + 0.128D
     #3 * t619 - 0.112D3 * t623 + 0.128D3 * t626 + 0.94D2 * t631 - 0.122
     #D3 * t635 + 0.36D2 * t637 * t638 * t639 + 0.108D3 * t643 * t21 - 0
     #.240D3 * t647 + t651 + 0.144D3 * t643 * t646
      t656 = t633 * t21
      t658 = t40 * t31
      t659 = t42 * t46
      t660 = t659 * t47
      t663 = t628 * t660
      t665 = t659 * t102
      t666 = t658 * t665
      t668 = t628 * t665
      t670 = t625 * t397
      t671 = 0.128D3 * t670
      t672 = t46 * t20
      t679 = t117 * t338 * t27 * t161
      t682 = t402 * t118 * t334
      t684 = t161 * t10
      t699 = t209 * t69
      t701 = t123 * t368 * t370 * t564 * t699
      t705 = t166 * t174 * t110 * t129
      t710 = t123 * t41 * x2 * t224 * t150
      t712 = t92 * t109
      t715 = t712 * t88 * t26 * t81
      t717 = t305 * t65
      t720 = t717 * t88 * t26 * x3
      t729 = t116 * t174 * t128 * t46 * t27
      t735 = 0.16D2 * t656 + 0.108D3 * t658 * t660 - 0.748D3 * t663 + 0.
     #216D3 * t666 - 0.312D3 * t668 - t671 + 0.864D3 * t443 * t128 * t67
     #2 * t10 - 0.430D3 * t679 + 0.328D3 * t682 + 0.36D2 * t108 * t321 *
     # t5 * t684 + 0.108D3 * t265 * t327 * t46 * t161 + 0.108D3 * t563 *
     # t564 * t96 * t10 - 0.272D3 * t701 + 0.1088D4 * t705 - 0.376D3 * t
     #710 + 0.192D3 * t715 - 0.182D3 * t720 + 0.144D3 * t637 * t638 * t1
     #01 * t47 - 0.3523D4 * t729 - 0.576D3 * t40 * t203 * t371 * t168
      t739 = t493 * x2 * t217 * t5 * t27
      t741 = t20 * t101
      t742 = t659 * t741
      t745 = t628 * t742
      t747 = t74 ** 2
      t749 = t174 ** 2
      t761 = t29 * t203 * t370 * t266 * t96 * t27
      t764 = t628 * t155 * t531
      t766 = t40 * t74
      t772 = t628 * t155 * t5 * x3
      t776 = t637 * t638 * t102 * t20
      t783 = t64 * t333 * t339
      t785 = t41 * t23
      t787 = x3 * t42
      t788 = t46 * t102
      t789 = t787 * t788
      t790 = t40 * t785 * t789
      t794 = t117 * t118 * t70 * x3
      t800 = t42 * t47
      t802 = t94 * t97 * t800
      t805 = t306 * t137 * t151
      t815 = t428 * t243 * t80 * x3
      t817 = 0.1244D4 * t739 + 0.288D3 * t658 * t742 - 0.880D3 * t745 + 
     #0.36D2 * t40 * t747 * t749 * t586 * t373 - 0.576D3 * t658 * t63 * 
     #t495 - 0.10D2 * t761 - 0.17D2 * t764 + 0.864D3 * t766 * t241 * t66
     # + 0.275D3 * t772 + 0.216D3 * t776 - 0.576D3 * t443 * t603 * t96 *
     # t47 + 0.3466D4 * t783 - 0.168D3 * t790 + 0.275D3 * t794 - 0.1152D
     #4 * t223 * t42 * t23 * t672 + 0.430D3 * t802 - 0.328D3 * t805 - 0.
     #576D3 * t443 * t251 * t322 + 0.864D3 * t223 * t224 * t531 + 0.148D
     #3 * t815
      t840 = t156 * t137 * t424
      t844 = t10 * x3
      t846 = t92 * t135 * t5 * t26 * t844
      t848 = 0.640D3 * t37 - 0.640D3 * t56 - 0.9344D4 * t72 - 0.54D2 * t
     #84 - 0.62D2 * t90 + 0.24D2 * t105 - 0.256D3 * t114 - 0.190D3 * t12
     #1 + 0.128D3 * t132 - 0.4940D4 * t140 + 0.108D3 * t148 - 0.190D3 * 
     #t153 - 0.444D3 * t159 - 0.592D3 * t164 - 0.1728D4 * t172 + 0.490D3
     # * t185 + 0.5266D4 * t191 + 0.256D3 * t840 + 0.256D3 * t846
      t850 = t40 * t74 * t78
      t853 = t850 * t88 * t161 * x3
      t855 = t74 * t109
      t856 = t40 * t855
      t863 = t181 * t66 * t236 * t10
      t867 = t181 * t66 * t339 * t20
      t870 = t582 * t182 * t236
      t876 = t30 * t43
      t877 = t876 * t49
      t882 = t30 * t785
      t884 = t882 * t129 * t800
      t886 = t468 * t101
      t888 = t156 * t137 * t886
      t891 = t40 * t855 * t69
      t894 = t891 * t46 * x1 * t464
      t896 = t74 * t65
      t898 = t40 * t896 * t69
      t899 = t96 * t42
      t902 = t898 * t899 * t741 * t10
      t904 = t40 * t896
      t907 = t904 * t129 * t787 * t102
      t910 = t40 * t74 * t23
      t914 = t910 * t699 * t205 * t102 * t20
      t918 = t575 * t576 * t236 * t101
      t920 = t876 * t483
      t924 = t876 * t452 * t481 * t10
      t926 = t882 * t789
      t929 = t876 * t788 * t35
      t931 = -0.64D2 * t853 + 0.64D2 * t856 * t150 * t151 * t161 - 0.256
     #D3 * t863 - 0.256D3 * t867 + 0.256D3 * t870 - 0.136D3 * t575 * t57
     #6 * t27 * t47 + 0.64D2 * t877 - 0.64D2 * t876 * t48 * t35 - 0.64D2
     # * t884 + 0.216D3 * t888 - 0.128D3 * t894 + 0.128D3 * t902 - 0.64D
     #2 * t907 + 0.64D2 * t914 - 0.216D3 * t918 + 0.128D3 * t920 - 0.128
     #D3 * t924 + 0.64D2 * t926 - 0.64D2 * t929
      t935 = 0.128D3 * t882 * t129 * t103
      t951 = t935 - 0.1360D4 * t196 - 0.1728D4 * t201 + 0.216D3 * t213 +
     # t221 - t229 + t233 + 0.3776D4 * t238 + 0.1728D4 * t245 + 0.776D3 
     #* t249 - 0.128D3 * t254 + 0.1728D4 * t258 + 0.432D3 * t262 + 0.144
     #D3 * t270 + 0.1728D4 * t275 - 0.1728D4 * t279 - 0.752D3 * t283 - 0
     #.864D3 * t287 - 0.496D3 * t295
      t971 = -0.264D3 * t301 + 0.256D3 * t309 + 0.584D3 * t314 - 0.5266D
     #4 * t319 + 0.5266D4 * t336 + 0.444D3 * t342 + 0.4816D4 * t346 - 0.
     #544D3 * t351 + 0.64D2 * t355 - 0.72D2 * t366 - 0.16D2 * t375 + 0.5
     #4D2 * t379 - 0.72D2 * t382 + 0.54D2 * t384 + 0.216D3 * t388 + 0.56
     #D2 * t393 - 0.216D3 * t398 + 0.134D3 * t404 + 0.1360D4 * t407
      t992 = 0.3776D4 * t411 - 0.54D2 * t415 - 0.72D2 * t421 + 0.608D3 *
     # t431 - 0.1168D4 * t434 + 0.1728D4 * t437 + 0.352D3 * t446 - 0.481
     #6D4 * t450 + 0.544D3 * t459 + 0.144D3 * t466 + 0.496D3 * t471 - 0.
     #256D3 * t475 - 0.640D3 * t479 + 0.144D3 * t484 + 0.344D3 * t488 - 
     #t499 - 0.256D3 * t505 + 0.256D3 * t511 + 0.288D3 * t515
      t1011 = 0.1024D4 * t519 + 0.496D3 * t522 - 0.752D3 * t525 - 0.288D
     #3 * t529 - 0.72D2 * t534 - 0.3776D4 * t538 - 0.640D3 * t541 + 0.64
     #0D3 * t546 + 0.30D2 * t550 - 0.108D3 * t553 + 0.56D2 * t557 - 0.72
     #D2 * t560 - 0.864D3 * t569 + 0.432D3 * t573 - 0.24D2 * t579 - 0.25
     #6D3 * t584 - t592 - 0.72D2 * t596 - 0.1376D4 * t605
      t1031 = 0.864D3 * t608 + 0.16D2 * t30 - 0.1728D4 * t613 - 0.496D3 
     #* t619 + 0.144D3 * t623 - 0.256D3 * t626 - 0.108D3 * t631 + 0.134D
     #3 * t635 - 0.64D2 * t647 - t651 - 0.608D3 * t656 + 0.592D3 * t663 
     #- 0.72D2 * t666 + 0.264D3 * t668 + 0.256D3 * t670 + 0.542D3 * t679
     # - 0.604D3 * t682 + 0.432D3 * t701 - 0.1728D4 * t705
      t1044 = t717 * t88 * t26 * t10
      t1050 = t30 * t41
      t1052 = t1050 * t135 * t322
      t1060 = 0.584D3 * t710 - 0.30D2 * t715 + 0.108D3 * t720 - 0.5266D4
     # * t729 - 0.3776D4 * t739 + 0.640D3 * t745 + 0.62D2 * t761 + 0.490
     #D3 * t764 - 0.156D3 * t772 - 0.72D2 * t776 - 0.256D3 * t1044 + 0.1
     #36D3 * t712 * t88 * t26 * t161 - 0.64D2 * t1052 + 0.4940D4 * t783 
     #+ 0.344D3 * t790 - 0.156D3 * t794 - 0.542D3 * t802 + 0.604D3 * t80
     #5 - 0.216D3 * t815
      t1084 = 0.256D3 * t56 + 0.1728D4 * t72 - 0.252D3 * t84 - 0.304D3 *
     # t90 + 0.32D2 * t105 + 0.288D3 * t114 + 0.28224D5 * t121 - t133 + 
     #0.424D3 * t140 - 0.8D1 * t148 + 0.28224D5 * t153 - 0.536D3 * t159 
     #+ 0.64D2 * t164 + 0.640D3 * t172 + 0.25220D5 * t185 - 0.780D3 * t1
     #91 + 0.25264D5 * t840 + 0.25248D5 * t846 + 0.128D3 * t853 - 0.2526
     #4D5 * t863
      t1107 = -0.25248D5 * t867 + 0.25296D5 * t870 - 0.128D3 * t877 + 0.
     #16D2 * t884 + 0.80D2 * t888 - 0.64D2 * t894 + 0.64D2 * t902 - 0.12
     #8D3 * t907 + 0.128D3 * t914 - 0.80D2 * t918 + 0.64D2 * t920 - 0.64
     #D2 * t924 + 0.128D3 * t926 - 0.128D3 * t929 + t935 + 0.512D3 * t19
     #6 + 0.640D3 * t201 - 0.16D2 * t30 * t390 * t684 - 0.112D3 * t30 * 
     #t392 - 0.640D3 * t238
      t1142 = -0.640D3 * t245 - 0.304D3 * t249 + t255 - 0.640D3 * t258 -
     # 0.160D3 * t262 - 0.640D3 * t275 + 0.640D3 * t279 + 0.74296D5 * t2
     #83 + 0.320D3 * t287 + 0.8D1 * t295 + 0.64D2 * t301 - 0.74320D5 * t
     #309 - 0.48D2 * t1050 * t109 * t10 * t81 + 0.48D2 * t1050 * t109 * 
     #t161 * x3 - 0.384D3 * t628 * t419 * z - 0.384D3 * t633 * t24 * t12
     #4 + 0.128D3 * t87 * x3 * t124 * z * t26 - 0.208D3 * t314 + 0.780D3
     # * t319 - 0.780D3 * t336
      t1161 = t30 * t417
      t1162 = t65 * t161
      t1204 = 0.536D3 * t342 + 0.32D2 * t882 * t129 * t349 + 0.96D2 * t9
     #10 * t699 * t205 * t101 * t47 + 0.96D2 * t856 * t150 * t473 * t101
     # + 0.32D2 * t423 * t150 * t886 - 0.96D2 * t423 * t474 - 0.96D2 * t
     #1161 * t469 * t1162 + 0.192D3 * t1161 * t469 * t630 - 0.192D3 * t8
     #98 * t899 * t741 * x3 - 0.912D3 * t346 + 0.74208D5 * t351 - 0.128D
     #3 * t355 + 0.16D2 * t910 * t699 * t205 * t639 - 0.288D3 * t1161 * 
     #t420 - 0.416D3 * t423 * t150 * t157 - 0.48D2 * t904 * t129 * t800 
     #* x3 + 0.48D2 * t856 * t150 * t151 * t81 - 0.64D2 * t891 * t328 * 
     #t424 - 0.416D3 * t30 * t390 * t88 * t844 + 0.32D2 * t904 * t129 * 
     #t800 * t10
      t1218 = x3 * z
      t1231 = t40 * t390
      t1252 = 0.288D3 * t850 * t88 * t10 * t81 + 0.64D2 * t423 * t54 * t
     #34 * t10 - 0.32D2 * t1161 * t34 * t1162 - 0.384D3 * t53 * t1218 * 
     #t21 - 0.768D3 * t30 * t377 * t88 * t1218 + 0.384D3 * t40 * t377 * 
     #t88 * x3 * t124 + 0.384D3 * t1231 * t88 * t81 * z + 0.384D3 * t123
     #1 * t88 * t1218 * t10 + 0.64D2 * t375 - 0.4D1 * t379 + 0.252D3 * t
     #384 - 0.128D3 * t388 - 0.144D3 * t393 + 0.304D3 * t404 - 0.512D3 *
     # t407 - 0.640D3 * t411 + 0.4D1 * t415 - 0.64D2 * t426 - 0.128D3 * 
     #t431 + 0.416D3 * t434
      t1271 = -0.640D3 * t437 + t447 + 0.912D3 * t450 - 0.74208D5 * t459
     # - 0.8D1 * t471 + 0.288D3 * t475 + 0.128D3 * t479 - 0.144D3 * t488
     # + t506 - 0.512D3 * t511 - 0.74288D5 * t515 - 0.1152D4 * t519 - 0.
     #8D1 * t522 + 0.74296D5 * t525 + 0.74288D5 * t529 + 0.640D3 * t538 
     #- 0.256D3 * t546 + 0.28792D5 * t550 - 0.29128D5 * t553 - 0.144D3 *
     # t557
      t1292 = 0.320D3 * t569 - 0.160D3 * t573 - 0.32D2 * t579 + 0.74320D
     #5 * t584 + 0.576D3 * t605 - 0.320D3 * t608 - 0.64D2 * t30 + 0.640D
     #3 * t613 - 0.192D3 * t616 + 0.8D1 * t619 + 0.512D3 * t626 + 0.8D1 
     #* t631 + 0.304D3 * t635 + 0.128D3 * t647 + 0.128D3 * t656 - 0.64D2
     # * t663 - 0.64D2 * t668 - t671 - 0.28208D5 * t679 + 0.27920D5 * t6
     #82
      t1320 = -0.28372D5 * t772 - 0.25296D5 * t1044 + 0.16D2 * t1052 + 0
     #.112D3 * t766 * t78 * t69 * t555 - 0.672D3 * t1050 * t135 * t444 -
     # 0.424D3 * t783 - 0.144D3 * t790 - 0.28372D5 * t794 + 0.28208D5 * 
     #t802 - 0.27920D5 * t805 + 0.128D3 * t815
      rrgq2qgh11J3 = (wd * (t187 + t303 + t400 + t490 + t581 + t654 + t7
     #35 + t817) + wd * (t848 + t931 + t951 + t971 + t992 + t1011 + t103
     #1 + t1060) + wd * (t1084 + t1107 + t1142 + t1204 + t1252 + t1271 +
     # t1292 - 0.160D3 * t701 + 0.640D3 * t705 - 0.208D3 * t710 - 0.2879
     #2D5 * t715 + 0.29128D5 * t720 + 0.780D3 * t729 + 0.640D3 * t739 - 
     #0.128D3 * t745 + 0.304D3 * t761 + 0.25220D5 * t764 + t1320)) / t28
     # / t27 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh11J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t31 = t1 ** 2
      t32 = t31 * t23
      t33 = t30 * t32
      t34 = t10 * x1
      t38 = t10 * t7 * t4 + x2 * x3 + t19
      t39 = t5 * t38
      t41 = t33 * t34 * t39
      t43 = t28 ** 2
      t44 = t26 * t43
      t45 = t31 * t1
      t46 = t23 ** 2
      t47 = t45 * t46
      t48 = t44 * t47
      t49 = x3 ** 2
      t50 = t49 * x1
      t51 = t50 * t39
      t52 = t48 * t51
      t56 = t31 ** 2
      t57 = t56 * t1
      t58 = t43 * t57
      t59 = x1 ** 2
      t60 = x2 * t59
      t61 = t23 * t46
      t63 = t58 * t60 * t61
      t64 = t4 ** 2
      t66 = 0.1D1 / t64 / t4
      t67 = t66 * t26
      t70 = z + x1 * t7 * t1
      t71 = t70 * t10
      t72 = t71 * t38
      t74 = t63 * t67 * t72
      t76 = t57 * x2
      t77 = t44 * t76
      t78 = t46 ** 2
      t79 = x1 * t78
      t80 = t10 ** 2
      t81 = t80 * t10
      t86 = t56 * t31
      t87 = x2 ** 2
      t89 = t44 * t86 * t87
      t90 = t59 * t78
      t91 = 0.1D1 / t64
      t96 = t56 * t45
      t97 = t87 * x2
      t99 = t44 * t96 * t97
      t100 = t59 * x1
      t101 = t100 * t78
      t106 = t29 * t45
      t107 = t106 * t60
      t108 = t23 * t91
      t109 = t27 * t38
      t111 = t107 * t108 * t109
      t113 = x2 * t100
      t114 = t58 * t113
      t115 = t64 ** 2
      t116 = 0.1D1 / t115
      t117 = t46 * t116
      t118 = t26 * t70
      t119 = t20 ** 2
      t122 = t114 * t117 * t118 * t119
      t124 = t45 * x1
      t126 = t5 * t20
      t127 = t46 * t49
      t128 = t126 * t127
      t129 = t44 * t124 * t128
      t131 = t30 * t47
      t132 = t70 * t91
      t133 = x1 * t20
      t134 = t133 * t10
      t136 = t131 * t132 * t134
      t138 = t43 * s
      t139 = t138 * t56
      t140 = t139 * t60
      t141 = t46 * t66
      t142 = t70 * t20
      t144 = t140 * t141 * t142
      t146 = t138 * t86
      t147 = t87 * t100
      t148 = t146 * t147
      t149 = t61 * t116
      t151 = t148 * t149 * t142
      t154 = t44 * t57 * t87
      t155 = t100 * t46
      t160 = t56 * x2
      t161 = t44 * t160
      t162 = t100 * t23
      t167 = x1 * x2
      t168 = t106 * t167
      t169 = t46 * t5
      t170 = t27 * x3
      t172 = t168 * t169 * t170
      t174 = t45 * t23
      t176 = x3 * t59
      t177 = t38 ** 2
      t178 = t91 * t177
      t179 = t176 * t178
      t180 = t44 * t174 * t179
      t182 = t43 * t45
      t183 = t46 * t70
      t184 = t182 * t183
      t185 = t91 * t26
      t186 = x3 * x1
      t187 = t186 * t20
      t189 = t184 * t185 * t187
      t191 = -0.128D3 * t41 + 0.144D3 * t52 + 0.36D2 * t44 + 0.12D2 * t3
     #0 + 0.880D3 * t74 + 0.36D2 * t77 * t79 * t5 * t81 + 0.108D3 * t89 
     #* t90 * t91 * t80 + 0.108D3 * t99 * t101 * t66 * t10 - 0.1632D4 * 
     #t111 + 0.47D2 * t122 + 0.36D2 * t129 - 0.128D3 * t136 + 0.752D3 * 
     #t144 - 0.1088D4 * t151 + 0.864D3 * t154 * t155 * t66 * t20 - 0.576
     #D3 * t161 * t162 * t66 * t119 + 0.3466D4 * t172 - 0.168D3 * t180 +
     # 0.530D3 * t189
      t193 = t97 * t100
      t195 = t78 * t116
      t197 = t43 * t96 * t193 * t195 * t118
      t199 = t29 * t57
      t200 = t87 * t59
      t201 = t199 * t200
      t202 = t61 * t66
      t203 = t27 * t70
      t205 = t201 * t202 * t203
      t207 = t138 * t57
      t208 = t207 * t60
      t209 = t70 * x3
      t210 = t209 * t20
      t212 = t208 * t202 * t210
      t214 = t61 * t70
      t217 = t58 * t214 * t67 * t200
      t219 = t70 * t38
      t221 = t148 * t149 * t219
      t226 = t142 * t38
      t228 = t58 * t113 * t46 * t116 * t26 * t226
      t232 = t114 * t117 * t118 * t177
      t234 = t43 * t31
      t235 = t23 * t70
      t236 = t234 * t235
      t237 = x1 * t38
      t239 = t236 * t185 * t237
      t241 = t182 * t235
      t242 = t59 * t177
      t244 = t241 * t67 * t242
      t246 = t59 * t46
      t247 = t91 * t38
      t250 = t161 * t246 * t247 * x3
      t252 = t43 * t56
      t253 = t252 * t183
      t256 = t253 * t67 * t60 * t38
      t258 = t59 * t61
      t263 = t91 * t49
      t266 = t77 * t258 * t263 * t38
      t268 = t31 * x1
      t269 = t30 * t268
      t270 = t23 * t10
      t272 = t269 * t126 * t270
      t274 = t45 * t59
      t275 = t44 * t274
      t276 = t91 * t119
      t277 = t276 * t24
      t280 = t66 * t177
      t283 = t77 * t155 * t280 * t10
      t285 = t29 * t31
      t287 = t23 * t5
      t291 = 0.1728D4 * t285 * t167 * t287 * t27 * z
      t294 = t70 * z
      t297 = t252 * t60 * t46 * t67 * t294 * t20
      t298 = 0.128D3 * t297
      t303 = t252 * t167 * t61 * t185 * t294 * x3
      t305 = t29 * t56
      t306 = t305 * t60
      t307 = t46 * t91
      t310 = t306 * t307 * t109 * x3
      t312 = -0.12D2 * t197 - 0.122D3 * t205 - 0.848D3 * t212 - 0.1244D4
     # * t217 + 0.544D3 * t221 + 0.128D3 * t228 + 0.312D3 * t232 - 0.328
     #D3 * t239 + 0.56D2 * t244 - 0.128D3 * t250 + 0.1632D4 * t256 + 0.1
     #44D3 * t77 * t258 * t247 * t80 + 0.144D3 * t266 - 0.96D2 * t272 - 
     #0.28D2 * t275 * t277 + 0.216D3 * t283 + t291 + t298 - 0.128D3 * t3
     #03 - 0.400D3 * t310
      t314 = t61 * t91
      t316 = t201 * t314 * t170
      t318 = t49 * x3
      t319 = t5 * t318
      t321 = t77 * t79 * t319
      t324 = t89 * t90 * t263
      t326 = t66 * x3
      t330 = t20 * t23
      t332 = t247 * t330 * x3
      t333 = t275 * t332
      t337 = t77 * t155 * t326 * t177
      t339 = t100 * t61
      t342 = t89 * t339 * t326 * t38
      t345 = t140 * t141 * t219
      t347 = t219 * x3
      t349 = t208 * t202 * t347
      t353 = t306 * t141 * t203 * t38
      t355 = t59 ** 2
      t362 = t161 * t162 * t280
      t365 = t208 * t202 * t72
      t367 = t207 * t113
      t369 = t367 * t117 * t226
      t371 = t237 * x3
      t373 = t131 * t132 * t371
      t376 = t269 * t39 * t24
      t380 = t44 * t32 * t186 * t39
      t383 = t43 * t1 * t23
      t384 = t70 * t5
      t386 = t383 * t384 * t26
      t388 = t30 * z
      t389 = t1 * t23
      t390 = t389 * x3
      t391 = t388 * t390
      t393 = t30 * t31
      t395 = t46 * x3 * t10
      t396 = t393 * t395
      t398 = 0.182D3 * t316 - 0.28D2 * t321 + 0.36D2 * t324 - 0.28D2 * t
     #99 * t101 * t326 - 0.112D3 * t333 - 0.168D3 * t337 - 0.112D3 * t34
     #2 - 0.1088D4 * t345 + 0.1088D4 * t349 + 0.528D3 * t353 + 0.144D3 *
     # t99 * t355 * t61 * t116 * t38 + 0.224D3 * t362 - 0.544D3 * t365 +
     # 0.1088D4 * t369 + 0.128D3 * t373 + 0.128D3 * t376 - 0.112D3 * t38
     #0 + 0.10D2 * t386 + 0.128D3 * t391 + 0.94D2 * t396
      t399 = t30 * t1
      t401 = t399 * t235 * t5
      t403 = t44 * t45
      t404 = t100 * t66
      t405 = t119 * t20
      t409 = t44 * t1
      t412 = t6 * t38
      t413 = t399 * t412
      t415 = t177 * t38
      t418 = 0.144D3 * t403 * t404 * t415
      t421 = t399 * t21
      t423 = t44 * t31
      t424 = t59 * t91
      t425 = t424 * t119
      t428 = t393 * t425
      t430 = t424 * t177
      t431 = t423 * t430
      t433 = t393 * t430
      t435 = t389 * t10
      t436 = t388 * t435
      t437 = 0.128D3 * t436
      t440 = t116 * t70
      t442 = t138 * t96 * t97 * t101 * t440
      t445 = t66 * t70
      t447 = t207 * t87 * t258 * t445
      t451 = x1 * t46
      t453 = t138 * t45 * x2 * t451 * t132
      t455 = t182 * t61
      t458 = t455 * t384 * t26 * t49
      t460 = t234 * t46
      t463 = t460 * t384 * t26 * x3
      t472 = t305 * t87 * t246 * t91 * t27
      t478 = -0.122D3 * t401 + 0.36D2 * t403 * t404 * t405 + 0.108D3 * t
     #409 * t21 - 0.240D3 * t413 + t418 + 0.144D3 * t409 * t412 + 0.16D2
     # * t421 + 0.108D3 * t423 * t425 - 0.748D3 * t428 + 0.216D3 * t431 
     #- 0.312D3 * t433 - t437 - 0.272D3 * t442 + 0.1088D4 * t447 - 0.376
     #D3 * t453 + 0.192D3 * t458 - 0.182D3 * t463 + 0.144D3 * t403 * t40
     #4 * t38 * t119 - 0.3523D4 * t472 - 0.576D3 * t44 * t86 * t193 * t2
     #02
      t482 = x1 * t23
      t485 = t285 * x2 * t482 * t5 * t27
      t487 = t20 * t38
      t488 = t424 * t487
      t491 = t393 * t488
      t493 = t56 ** 2
      t495 = t87 ** 2
      t507 = t29 * t86 * t97 * t339 * t66 * t27
      t509 = t5 * t10
      t511 = t393 * t183 * t509
      t513 = t44 * t56
      t519 = t393 * t183 * t5 * x3
      t523 = t403 * t404 * t177 * t20
      t527 = t77 * t79 * t509 * t49
      t529 = t252 * t214
      t532 = t529 * t185 * t167 * t10
      t534 = t142 * t10
      t536 = t63 * t67 * t534
      t540 = t209 * t10
      t542 = t58 * t167 * t78 * t185 * t540
      t544 = t43 * t86
      t545 = t544 * t147
      t548 = t545 * t149 * t118 * t20
      t552 = z ** 2
      t556 = 0.1728D4 * t44 * t31 * x2 * t482 * t5 * t552
      t558 = t44 * t45 * x2
      t563 = 0.1728D4 * t558 * t451 * t5 * z * x3
      t567 = 0.1728D4 * t184 * t185 * t167 * z
      t568 = t27 * t20
      t570 = t107 * t108 * t568
      t572 = t146 * t200
      t573 = t78 * t66
      t575 = t572 * t573 * t209
      t577 = 0.1244D4 * t485 + 0.288D3 * t423 * t488 - 0.880D3 * t491 + 
     #0.36D2 * t44 * t493 * t495 * t355 * t195 - 0.576D3 * t423 * t167 *
     # t287 - 0.10D2 * t507 - 0.17D2 * t511 + 0.864D3 * t513 * t200 * t3
     #07 + 0.275D3 * t519 + 0.216D3 * t523 + 0.36D2 * t527 + 0.1244D4 * 
     #t532 + 0.96D2 * t536 - 0.96D2 * t542 - 0.148D3 * t548 - t556 + t56
     #3 - t567 - 0.1244D4 * t570 - 0.1088D4 * t575
      t578 = t207 * t167
      t579 = t78 * t91
      t582 = t578 * t579 * t70 * t49
      t585 = t138 * t552 * t160
      t586 = x1 * t61
      t589 = t585 * t586 * t132 * x3
      t590 = 0.64D2 * t589
      t591 = t139 * t167
      t593 = t591 * t314 * t71
      t595 = t27 * t10
      t598 = t306 * t307 * t595 * t38
      t603 = t38 * t10 * x3
      t605 = t44 * t76 * t59 * t314 * t603
      t608 = t63 * t67 * t347
      t611 = t33 * t132 * t237
      t614 = t184 * t185 * t371
      t617 = t59 * t20 * t38
      t619 = t241 * t67 * t617
      t621 = t31 * t46
      t623 = t30 * t621 * t80
      t625 = t621 * t49
      t626 = t44 * t625
      t628 = t30 * t625
      t630 = t30 * t390
      t632 = t45 * t61
      t633 = t632 * t318
      t634 = t44 * t633
      t638 = t30 * t435
      t642 = t545 * t149 * t118 * t38
      t649 = t544 * t200
      t652 = t649 * t573 * t118 * t10
      t654 = t58 * t167
      t657 = t654 * t579 * t118 * t80
      t659 = -0.472D3 * t582 + t590 - 0.1088D4 * t593 - 0.584D3 * t598 -
     # 0.112D3 * t605 - 0.128D3 * t608 + 0.528D3 * t611 + 0.400D3 * t614
     # + 0.584D3 * t619 - 0.47D2 * t623 + 0.36D2 * t626 - 0.47D2 * t628 
     #- 0.148D3 * t630 - 0.28D2 * t634 - 0.28D2 * t44 * t390 + 0.148D3 *
     # t638 + 0.240D3 * t642 + 0.288D3 * t89 * t339 * t66 * t38 * t10 - 
     #0.16D2 * t652 + 0.748D3 * t657
      t662 = t208 * t202 * t534
      t670 = t306 * t141 * t203 * t20
      t673 = t48 * t132 * t167
      t675 = t305 * t167
      t676 = t61 * t5
      t679 = t675 * t676 * t27 * t49
      t682 = t591 * t314 * t209
      t685 = t578 * t579 * t540
      t689 = t367 * t117 * t70 * t119
      t693 = t253 * t67 * t60 * t20
      t695 = t5 * t80
      t700 = t91 * x3
      t707 = t675 * t314 * t203 * t10
      t711 = t585 * t246 * t445 * t20
      t712 = 0.64D2 * t711
      t715 = t529 * t185 * t167 * x3
      t718 = t63 * t67 * t210
      t721 = t33 * t132 * t133
      t724 = t168 * t169 * t595
      t728 = t675 * t676 * t170 * t10
      t730 = t91 * t20
      t737 = t675 * t676 * t27 * t80
      t739 = 0.1088D4 * t662 - 0.1152D4 * t154 * t258 * t91 * t10 - 0.17
     #D2 * t670 + 0.3523D4 * t673 - 0.192D3 * t679 + 0.848D3 * t682 + 0.
     #1088D4 * t685 - 0.376D3 * t689 - 0.3523D4 * t693 - 0.28D2 * t77 * 
     #t79 * t695 * x3 - 0.56D2 * t89 * t90 * t700 * t10 + 0.306D3 * t707
     # - t712 - 0.3466D4 * t715 - 0.94D2 * t718 + 0.306D3 * t721 + 0.352
     #3D4 * t724 - 0.530D3 * t728 + 0.864D3 * t161 * t246 * t730 * t10 -
     # 0.430D3 * t737
      t741 = t201 * t314 * t595
      t743 = t59 * t119
      t745 = t241 * t67 * t743
      t748 = t236 * t185 * t133
      t758 = t649 * t573 * t118 * x3
      t760 = t5 * t49
      t762 = t161 * t586 * t760
      t763 = 0.576D3 * t762
      t766 = t367 * t117 * t70 * t177
      t768 = t305 * t113
      t769 = t23 * t66
      t772 = t768 * t769 * t27 * t177
      t774 = t199 * t147
      t776 = t774 * t141 * t109
      t782 = 0.144D3 * t77 * t355 * t23 * t116 * t415
      t786 = t89 * t355 * t46 * t116 * t177
      t790 = t578 * t579 * t70 * t80
      t793 = t572 * t573 * t71
      t796 = t33 * t186 * t126
      t803 = t168 * t307 * t203
      t807 = t654 * t579 * t118 * t49
      t811 = t675 * t314 * t203 * x3
      t817 = 0.328D3 * t741 + 0.430D3 * t745 - 0.328D3 * t748 - 0.576D3 
     #* t161 * t586 * t695 + 0.864D3 * t558 * t451 * t509 + 0.148D3 * t7
     #58 - t763 - 0.272D3 * t766 - 0.56D2 * t772 + 0.328D3 * t776 + t782
     # + 0.216D3 * t786 - 0.272D3 * t790 + 0.544D3 * t793 + 0.96D2 * t79
     #6 - 0.56D2 * t44 * t268 * t126 * t24 - 0.7896D4 * t803 + 0.47D2 * 
     #t807 + 0.275D3 * t811 - 0.1152D4 * t558 * t59 * t23 * t730
      t828 = t44 * t56 * t23
      t832 = t828 * t440 * t100 * t177 * t20
      t836 = t768 * t769 * t568 * t38
      t838 = t30 * t274
      t839 = t838 * t332
      t852 = 0.496D3 * t41 - 0.256D3 * t52 + 0.16D2 * t30 - 0.640D3 * t7
     #4 + 0.4816D4 * t111 + 0.64D2 * t832 - 0.216D3 * t836 + 0.128D3 * t
     #839 - 0.54D2 * t122 - 0.72D2 * t129 - 0.1168D4 * t144 + 0.1728D4 *
     # t151 + 0.4940D4 * t172 + 0.344D3 * t180 - 0.444D3 * t189 - 0.16D2
     # * t197 + 0.134D3 * t205 + 0.1360D4 * t212 + 0.3776D4 * t217
      t871 = -0.864D3 * t221 - 0.496D3 * t228 - 0.264D3 * t232 + 0.256D3
     # * t239 + 0.24D2 * t244 + 0.1024D4 * t250 - 0.4816D4 * t256 - 0.25
     #6D3 * t266 + 0.640D3 * t272 - 0.72D2 * t283 - t291 - 0.256D3 * t29
     #7 + 0.256D3 * t303 + 0.288D3 * t310 - 0.108D3 * t316 + 0.56D2 * t3
     #21 - 0.72D2 * t324 + 0.144D3 * t333 + 0.344D3 * t337
      t877 = t56 * t61
      t879 = t44 * t877 * t70
      t882 = t879 * t91 * x1 * t603
      t884 = t56 * t46
      t886 = t44 * t884 * t70
      t887 = t66 * t59
      t890 = t886 * t887 * t487 * t10
      t892 = t44 * t884
      t895 = t892 * t445 * t176 * t177
      t908 = 0.144D3 * t342 + 0.1728D4 * t345 - 0.1728D4 * t349 - 0.752D
     #3 * t353 - 0.128D3 * t882 + 0.128D3 * t890 - 0.64D2 * t895 - 0.137
     #6D4 * t362 + 0.864D3 * t365 - 0.1728D4 * t369 - 0.496D3 * t376 + 0
     #.144D3 * t380 - 0.62D2 * t386 - 0.256D3 * t391 - 0.108D3 * t396 + 
     #0.134D3 * t401 - 0.64D2 * t413 - t418 - 0.608D3 * t421
      t927 = t460 * t384 * t26 * t10
      t933 = t30 * t45
      t935 = t933 * t214 * t695
      t937 = 0.592D3 * t428 - 0.72D2 * t431 + 0.264D3 * t433 + 0.256D3 *
     # t436 + 0.432D3 * t442 - 0.1728D4 * t447 + 0.584D3 * t453 - 0.30D2
     # * t458 + 0.108D3 * t463 - 0.5266D4 * t472 - 0.3776D4 * t485 + 0.6
     #40D3 * t491 + 0.62D2 * t507 + 0.490D3 * t511 - 0.156D3 * t519 - 0.
     #72D2 * t523 - 0.256D3 * t927 + 0.136D3 * t455 * t384 * t26 * t80 -
     # 0.64D2 * t935
      t945 = t184 * t185 * t134
      t949 = t10 * x3
      t951 = t182 * t214 * t5 * t26 * t949
      t954 = t44 * t56 * t78
      t957 = t954 * t384 * t80 * x3
      t959 = t44 * t877
      t966 = t306 * t307 * t568 * t10
      t970 = t306 * t307 * t170 * t20
      t978 = -0.72D2 * t527 - 0.3776D4 * t532 - 0.640D3 * t536 + 0.640D3
     # * t542 + 0.256D3 * t945 + 0.256D3 * t951 - 0.64D2 * t957 + 0.64D2
     # * t959 * t132 * t133 * t80 - 0.256D3 * t966 - 0.256D3 * t970 + 0.
     #216D3 * t548 + t556 - t563 + t567 + 0.3776D4 * t570 + 0.1728D4 * t
     #575 + 0.776D3 * t582 - 0.128D3 * t589 + 0.1728D4 * t593
      t998 = 0.544D3 * t598 + 0.144D3 * t605 + 0.496D3 * t608 - 0.752D3 
     #* t611 - 0.288D3 * t614 - 0.544D3 * t619 + 0.54D2 * t623 - 0.72D2 
     #* t626 + 0.54D2 * t628 + 0.216D3 * t630 + 0.56D2 * t634 - 0.216D3 
     #* t638 + 0.64D2 * t642 + 0.608D3 * t652 - 0.592D3 * t657 - 0.1728D
     #4 * t662 + 0.490D3 * t670 + 0.5266D4 * t673 + 0.30D2 * t679
      t1005 = t774 * t141 * t568
      t1011 = t838 * t277
      t1016 = t30 * t174
      t1018 = t1016 * t445 * t743
      t1020 = t34 * t38
      t1022 = t184 * t185 * t1020
      t1026 = t838 * t247 * t330 * t10
      t1028 = t1016 * t179
      t1031 = t838 * t178 * t270
      t1035 = 0.128D3 * t1016 * t445 * t242
      t1041 = -0.1360D4 * t682 - 0.1728D4 * t685 + 0.584D3 * t689 - 0.52
     #66D4 * t693 + 0.256D3 * t1005 - 0.136D3 * t768 * t769 * t27 * t119
     # + 0.64D2 * t1011 - 0.64D2 * t838 * t276 * t270 - 0.64D2 * t1018 +
     # 0.216D3 * t1022 - 0.128D3 * t1026 + 0.64D2 * t1028 - 0.64D2 * t10
     #31 + t1035 - 0.190D3 * t707 + 0.128D3 * t711 - 0.4940D4 * t715 + 0
     #.108D3 * t718 - 0.190D3 * t721
      t1060 = 0.5266D4 * t724 + 0.444D3 * t728 + 0.542D3 * t737 - 0.604D
     #3 * t741 - 0.542D3 * t745 + 0.604D3 * t748 - 0.216D3 * t758 + 0.35
     #2D3 * t762 + 0.432D3 * t766 - 0.24D2 * t772 - 0.256D3 * t776 - t78
     #2 - 0.72D2 * t786 + 0.432D3 * t790 - 0.864D3 * t793 - 0.640D3 * t7
     #96 - 0.9344D4 * t803 - 0.54D2 * t807 - 0.156D3 * t811
      t1109 = t30 * t124
      t1110 = t46 * t80
      t1117 = -0.8D1 * t41 + 0.288D3 * t52 - 0.64D2 * t30 + 0.128D3 * t7
     #4 - 0.912D3 * t111 - 0.48D2 * t933 * t61 * t10 * t49 + 0.48D2 * t9
     #33 * t61 * t80 * x3 - 0.384D3 * t393 * t127 * z - 0.384D3 * t399 *
     # t24 * t552 + 0.128D3 * t383 * x3 * t552 * z * t26 + 0.128D3 * t83
     #2 - 0.80D2 * t836 + 0.64D2 * t839 + 0.32D2 * t1016 * t445 * t617 +
     # 0.96D2 * t828 * t440 * t100 * t38 * t119 + 0.96D2 * t959 * t132 *
     # t50 * t38 + 0.32D2 * t131 * t132 * t1020 - 0.96D2 * t131 * t51 - 
     #0.96D2 * t1109 * t39 * t1110 + 0.192D3 * t1109 * t39 * t395
      t1161 = x3 * z
      t1174 = t44 * t632
      t1186 = -0.192D3 * t886 * t887 * t487 * x3 + 0.16D2 * t828 * t440 
     #* t100 * t405 - 0.288D3 * t1109 * t128 - 0.416D3 * t131 * t132 * t
     #187 - 0.48D2 * t892 * t445 * t743 * x3 + 0.48D2 * t959 * t132 * t1
     #33 * t49 - 0.64D2 * t879 * t700 * t134 - 0.416D3 * t30 * t632 * t3
     #84 * t949 + 0.32D2 * t892 * t445 * t743 * t10 + 0.288D3 * t954 * t
     #384 * t10 * t49 + 0.64D2 * t131 * t186 * t126 * t10 - 0.32D2 * t11
     #09 * t126 * t1110 - 0.384D3 * t33 * t1161 * t21 - 0.768D3 * t30 * 
     #t621 * t384 * t1161 + 0.384D3 * t44 * t621 * t384 * x3 * t552 + 0.
     #384D3 * t1174 * t384 * t49 * z + 0.384D3 * t1174 * t384 * t1161 * 
     #t10 + 0.4D1 * t122 - 0.64D2 * t136 + 0.416D3 * t144
      t1207 = -0.640D3 * t151 - 0.424D3 * t172 - 0.144D3 * t180 - 0.536D
     #3 * t189 + 0.64D2 * t197 + 0.304D3 * t205 - 0.512D3 * t212 - 0.640
     #D3 * t217 + 0.320D3 * t221 + 0.8D1 * t228 + 0.64D2 * t232 - 0.7432
     #0D5 * t239 + 0.32D2 * t244 - 0.1152D4 * t250 + 0.912D3 * t256 + 0.
     #288D3 * t266 + t298 - 0.512D3 * t303 - 0.74288D5 * t310 - 0.29128D
     #5 * t316
      t1228 = -0.144D3 * t321 - 0.144D3 * t337 - 0.640D3 * t345 + 0.640D
     #3 * t349 + 0.74296D5 * t353 - 0.64D2 * t882 + 0.64D2 * t890 - 0.12
     #8D3 * t895 + 0.576D3 * t362 - 0.320D3 * t365 + 0.640D3 * t369 - 0.
     #192D3 * t373 + 0.8D1 * t376 - 0.304D3 * t386 + 0.512D3 * t391 + 0.
     #8D1 * t396 + 0.304D3 * t401 + 0.128D3 * t413 + 0.128D3 * t421 - 0.
     #64D2 * t428
      t1255 = -0.64D2 * t433 - t437 - 0.160D3 * t442 + 0.640D3 * t447 - 
     #0.208D3 * t453 - 0.28792D5 * t458 + 0.29128D5 * t463 + 0.780D3 * t
     #472 + 0.640D3 * t485 - 0.128D3 * t491 + 0.304D3 * t507 + 0.25220D5
     # * t511 - 0.28372D5 * t519 - 0.25296D5 * t927 + 0.16D2 * t935 + 0.
     #112D3 * t513 * t78 * t70 * t319 - 0.672D3 * t933 * t214 * t760 + 0
     #.640D3 * t532 - 0.256D3 * t542 + 0.25264D5 * t945
      t1278 = 0.25248D5 * t951 + 0.128D3 * t957 - 0.16D2 * t30 * t632 * 
     #t81 - 0.112D3 * t30 * t633 - 0.25264D5 * t966 - 0.25248D5 * t970 -
     # 0.640D3 * t570 - 0.640D3 * t575 - 0.304D3 * t582 + t590 - 0.640D3
     # * t593 - 0.74208D5 * t598 - 0.8D1 * t608 + 0.74296D5 * t611 + 0.7
     #4288D5 * t614 + 0.74208D5 * t619 - 0.4D1 * t623 + 0.252D3 * t628 -
     # 0.128D3 * t630 - 0.144D3 * t634
      t1299 = -0.128D3 * t642 - 0.128D3 * t652 + 0.64D2 * t657 + 0.640D3
     # * t662 + 0.25220D5 * t670 - 0.780D3 * t673 + 0.28792D5 * t679 + 0
     #.512D3 * t682 + 0.640D3 * t685 - 0.208D3 * t689 + 0.780D3 * t693 +
     # 0.25296D5 * t1005 - 0.128D3 * t1011 + 0.16D2 * t1018 + 0.80D2 * t
     #1022 - 0.64D2 * t1026 + 0.128D3 * t1028 - 0.128D3 * t1031 + t1035 
     #+ 0.28224D5 * t707
      t1320 = 0.128D3 * t758 + t763 - 0.160D3 * t766 - 0.32D2 * t772 + 0
     #.74320D5 * t776 - 0.160D3 * t790 + 0.320D3 * t793 + 0.256D3 * t796
     # + 0.1728D4 * t803 - 0.252D3 * t807 - 0.28372D5 * t811
      rrgq2qgh11J4 = (wd * (t191 + t312 + t398 + t478 + t577 + t659 + t7
     #39 + t817) + wd * (t852 + t871 + t908 + t937 + t978 + t998 + t1041
     # + t1060) + wd * (t1117 + t1186 + t1207 + t1228 + t1255 + t1278 + 
     #t1299 - t712 + 0.424D3 * t715 - 0.8D1 * t718 + 0.28224D5 * t721 - 
     #0.780D3 * t724 + 0.536D3 * t728 - 0.28208D5 * t737 + 0.27920D5 * t
     #741 + 0.28208D5 * t745 - 0.27920D5 * t748 + t1320)) / t28 / t27 / 
     #z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh11J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = x1 ** 2
      t9 = x2 * t8
      t10 = t7 * t9
      t11 = 0.1D1 - x1
      t12 = t11 ** 2
      t14 = z + x1 * t4
      t15 = t14 ** 2
      t17 = 0.1D1 / t15 / t14
      t18 = t12 * t17
      t19 = 0.1D1 - x2
      t22 = z + x1 * t19 * t4
      t23 = x3 * t19
      t25 = 0.1D1 - x3
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t23 * t14 * x2 * t25)
      t34 = 0.2D1 * t28 * t32
      t35 = t23 * t14 + x2 * t25 - t34
      t36 = t22 * t35
      t38 = t10 * t18 * t36
      t40 = t6 * t5
      t41 = t3 * t40
      t42 = x2 ** 2
      t43 = t8 * x1
      t44 = t42 * t43
      t45 = t41 * t44
      t46 = t12 * t11
      t47 = t15 ** 2
      t48 = 0.1D1 / t47
      t49 = t46 * t48
      t51 = t45 * t49 * t36
      t53 = s * t4
      t54 = 0.1D1 / t14
      t55 = x1 * t54
      t56 = t55 * t35
      t58 = t11 * x3
      t60 = s - t53 * t56 - t53 * t58
      t61 = t60 * t2
      t62 = t6 * t4
      t64 = t61 * t62 * t42
      t65 = t43 * t12
      t70 = t6 * x2
      t71 = t61 * t70
      t72 = x1 * t46
      t73 = x3 ** 2
      t74 = t54 * t73
      t76 = t71 * t72 * t74
      t77 = 0.576D3 * t76
      t78 = t3 * t62
      t79 = x2 * t43
      t80 = t78 * t79
      t81 = t12 * t48
      t85 = t25 * t19 * t14 + x2 * x3 + t34
      t86 = t85 ** 2
      t89 = t80 * t81 * t22 * t86
      t92 = t2 * t4 * t11
      t93 = t22 * t54
      t95 = t92 * t93 * t60
      t97 = t2 * t6
      t98 = t12 * t22
      t99 = t97 * t98
      t100 = t17 * t60
      t103 = t99 * t100 * t9 * t85
      t105 = t62 * x2
      t106 = t61 * t105
      t107 = t8 * t46
      t108 = 0.1D1 / t15
      t109 = t108 * t85
      t110 = t25 ** 2
      t115 = s * t1
      t116 = t115 * t6
      t117 = t116 * t9
      t118 = t12 * t108
      t119 = t60 ** 2
      t120 = t119 * t25
      t123 = t117 * t118 * t120 * t85
      t127 = t46 * t108
      t129 = t85 * t25 * x3
      t131 = t61 * t105 * t8 * t127 * t129
      t133 = t2 * t62
      t135 = t133 * t9 * t46
      t136 = t22 * t85
      t137 = t136 * x3
      t139 = t135 * t100 * t137
      t141 = t119 * t115
      t142 = t5 * t11
      t143 = t141 * t142
      t144 = t22 * t108
      t145 = x1 * t85
      t147 = t143 * t144 * t145
      t149 = t5 * t4
      t150 = t149 * t8
      t151 = t61 * t150
      t152 = t35 ** 2
      t153 = t108 * t152
      t154 = t153 * t58
      t157 = x3 * x1
      t158 = t54 * t35
      t160 = t143 * t157 * t158
      t162 = x1 * t5
      t167 = t115 * t149
      t168 = x1 * x2
      t169 = t167 * t168
      t170 = t119 * t22
      t172 = t169 * t118 * t170
      t174 = t133 * t168
      t175 = t12 ** 2
      t176 = t175 * t108
      t177 = t60 * t22
      t180 = t174 * t176 * t177 * t73
      t182 = t54 * t110
      t187 = t61 * t149 * x2
      t188 = x1 * t12
      t189 = t54 * t25
      t193 = 0.752D3 * t38 - 0.1088D4 * t51 + 0.864D3 * t64 * t65 * t17 
     #* t35 - t77 - 0.272D3 * t89 + 0.10D2 * t95 + 0.1632D4 * t103 + 0.1
     #44D3 * t106 * t107 * t109 * t110 - 0.584D3 * t123 - 0.112D3 * t131
     # - 0.128D3 * t139 + 0.528D3 * t147 - 0.28D2 * t151 * t154 + 0.96D2
     # * t160 - 0.56D2 * t61 * t162 * t158 * t58 - 0.7896D4 * t172 + 0.4
     #7D2 * t180 - 0.576D3 * t71 * t72 * t182 + 0.864D3 * t187 * t188 * 
     #t189
      t194 = t2 * t40
      t195 = t42 * t8
      t196 = t194 * t195
      t197 = t175 * t17
      t200 = t196 * t197 * t177 * x3
      t202 = t194 * t44
      t205 = t202 * t49 * t177 * t35
      t209 = x1 * t11
      t210 = z ** 2
      t214 = 0.1728D4 * t61 * t5 * x2 * t209 * t54 * t210
      t219 = 0.1728D4 * t187 * t188 * t54 * z * x3
      t220 = t2 * t149
      t221 = t220 * t98
      t222 = t108 * t60
      t226 = 0.1728D4 * t221 * t222 * t168 * z
      t227 = t167 * t9
      t228 = t11 * t108
      t229 = t119 * t35
      t231 = t227 * t228 * t229
      t233 = t41 * t195
      t234 = t22 * x3
      t236 = t233 * t197 * t234
      t238 = t78 * t168
      t241 = t238 * t176 * t22 * t73
      t244 = t3 * t210 * t70
      t247 = t244 * t72 * t144 * x3
      t248 = 0.64D2 * t247
      t249 = t7 * t168
      t250 = t22 * t25
      t252 = t249 * t127 * t250
      t256 = t238 * t176 * t22 * t110
      t259 = t233 * t197 * t250
      t261 = t8 * t12
      t262 = t108 * t35
      t267 = t116 * t168
      t268 = t46 * t54
      t271 = t267 * t268 * t119 * t110
      t273 = t115 * t62
      t274 = t273 * t195
      t276 = t274 * t127 * t120
      t278 = x1 * t175
      t279 = t110 * t25
      t285 = t61 * t40 * t42
      t286 = t8 * t175
      t291 = t6 * t149
      t292 = t42 * x2
      t294 = t61 * t291 * t292
      t295 = t43 * t175
      t300 = t119 * t85
      t303 = t117 * t118 * t300 * x3
      t307 = t71 * t261 * t109 * x3
      t309 = 0.148D3 * t200 - 0.148D3 * t205 - t214 + t219 - t226 - 0.12
     #44D4 * t231 - 0.1088D4 * t236 - 0.472D3 * t241 + t248 - 0.1088D4 *
     # t252 - 0.272D3 * t256 + 0.544D3 * t259 + 0.864D3 * t71 * t261 * t
     #262 * t25 - 0.430D3 * t271 + 0.328D3 * t276 + 0.36D2 * t106 * t278
     # * t54 * t279 + 0.108D3 * t285 * t286 * t108 * t110 + 0.108D3 * t2
     #94 * t295 * t17 * t25 - 0.400D3 * t303 - 0.128D3 * t307
      t311 = t141 * t162
      t312 = t54 * t85
      t314 = t311 * t312 * t58
      t318 = t61 * t142 * t157 * t312
      t322 = t25 * x1
      t324 = t143 * t322 * t312
      t326 = t149 * t12
      t327 = t61 * t326
      t328 = t73 * x1
      t329 = t328 * t312
      t330 = t327 * t329
      t332 = t250 * t85
      t334 = t135 * t100 * t332
      t336 = t35 * t11
      t338 = t109 * t336 * x3
      t339 = t151 * t338
      t341 = t43 * t46
      t347 = t17 * t86
      t350 = t106 * t65 * t347 * t25
      t352 = t115 * t5
      t354 = t11 * t54
      t358 = 0.1728D4 * t352 * t168 * t354 * t119 * z
      t361 = t22 * z
      t364 = t97 * t9 * t12 * t100 * t361 * t35
      t365 = 0.128D3 * t364
      t370 = t97 * t168 * t46 * t222 * t361 * x3
      t372 = t141 * z
      t373 = t4 * t11
      t374 = t373 * x3
      t375 = t372 * t374
      t377 = t141 * t5
      t379 = t12 * x3 * t25
      t380 = t377 * t379
      t382 = t141 * t4
      t383 = t11 * t22
      t385 = t382 * t383 * t54
      t387 = t61 * t149
      t388 = t43 * t17
      t389 = t152 * t35
      t393 = t61 * t4
      t396 = t55 * t85
      t397 = t382 * t396
      t399 = t86 * t85
      t402 = 0.144D3 * t387 * t388 * t399
      t403 = 0.128D3 * t314 - 0.112D3 * t318 + 0.36D2 * t61 + 0.12D2 * t
     #141 - 0.128D3 * t324 + 0.144D3 * t330 + 0.880D3 * t334 - 0.112D3 *
     # t339 + 0.288D3 * t285 * t341 * t17 * t85 * t25 + 0.216D3 * t350 +
     # t358 + t365 - 0.128D3 * t370 + 0.128D3 * t375 + 0.94D2 * t380 - 0
     #.122D3 * t385 + 0.36D2 * t387 * t388 * t389 + 0.108D3 * t393 * t56
     # - 0.240D3 * t397 + t402
      t406 = t382 * t56
      t408 = t61 * t5
      t409 = t8 * t108
      t410 = t409 * t152
      t413 = t377 * t410
      t415 = t409 * t86
      t416 = t408 * t415
      t418 = t377 * t415
      t420 = t373 * t25
      t421 = t372 * t420
      t422 = 0.128D3 * t421
      t423 = t149 * x1
      t425 = t12 * t73
      t426 = t158 * t425
      t427 = t61 * t423 * t426
      t429 = t141 * t326
      t430 = x1 * t35
      t431 = t430 * t25
      t433 = t429 * t144 * t431
      t437 = t196 * t197 * t177 * t25
      t441 = t174 * t176 * t177 * t110
      t443 = t8 ** 2
      t449 = t43 * t11
      t451 = t71 * t449 * t347
      t453 = t78 * t9
      t454 = t46 * t17
      t456 = t453 * t454 * t332
      t458 = t36 * t85
      t460 = t80 * t81 * t458
      t462 = t145 * x3
      t464 = t429 * t144 * t462
      t466 = t234 * t35
      t468 = t453 * t454 * t466
      t470 = t46 * t22
      t473 = t133 * t470 * t100 * t195
      t475 = t133 * t79
      t478 = t475 * t81 * t177 * t152
      t482 = t267 * t127 * t170 * t25
      t484 = 0.144D3 * t393 * t396 + 0.16D2 * t406 + 0.108D3 * t408 * t4
     #10 - 0.748D3 * t413 + 0.216D3 * t416 - 0.312D3 * t418 - t422 + 0.3
     #6D2 * t427 - 0.128D3 * t433 - 0.16D2 * t437 + 0.748D3 * t441 + 0.1
     #44D3 * t294 * t443 * t46 * t48 * t85 + 0.224D3 * t451 - 0.544D3 * 
     #t456 + 0.1088D4 * t460 + 0.128D3 * t464 - 0.848D3 * t468 - 0.1244D
     #4 * t473 + 0.47D2 * t478 + 0.306D3 * t482
      t487 = t17 * t22
      t490 = t244 * t261 * t487 * t35
      t491 = 0.64D2 * t490
      t492 = t97 * t470
      t495 = t492 * t222 * t168 * x3
      t498 = t135 * t100 * t466
      t501 = t143 * t144 * t430
      t504 = t249 * t127 * t234
      t506 = t234 * t25
      t508 = t238 * t176 * t506
      t512 = t80 * t81 * t22 * t152
      t516 = t117 * t18 * t170 * t85
      t519 = t45 * t49 * t136
      t525 = t133 * t79 * t12 * t48 * t60 * t458
      t529 = t475 * t81 * t177 * t86
      t531 = t2 * t5
      t532 = t531 * t383
      t534 = t532 * t222 * t145
      t536 = t220 * t383
      t537 = t8 * t86
      t539 = t536 * t100 * t537
      t541 = t17 * x3
      t544 = t106 * t65 * t541 * t86
      t548 = t285 * t341 * t541 * t85
      t551 = t10 * t18 * t136
      t554 = t453 * t454 * t137
      t556 = t36 * t25
      t558 = t453 * t454 * t556
      t566 = t117 * t18 * t170 * t35
      t568 = -t491 - 0.3466D4 * t495 - 0.94D2 * t498 + 0.306D3 * t501 + 
     #0.848D3 * t504 + 0.1088D4 * t508 - 0.376D3 * t512 + 0.528D3 * t516
     # + 0.544D3 * t519 + 0.128D3 * t525 + 0.312D3 * t529 - 0.328D3 * t5
     #34 + 0.56D2 * t539 - 0.168D3 * t544 - 0.112D3 * t548 - 0.1088D4 * 
     #t551 + 0.1088D4 * t554 + 0.1088D4 * t558 - 0.1152D4 * t64 * t107 *
     # t108 * t25 - 0.17D2 * t566
      t570 = t327 * t144 * t168
      t574 = t267 * t268 * t119 * t73
      t576 = t119 * x3
      t578 = t274 * t127 * t576
      t580 = t73 * x3
      t581 = t54 * t580
      t583 = t106 * t278 * t581
      t585 = t116 * t79
      t586 = t11 * t17
      t589 = t585 * t586 * t119 * t86
      t591 = t273 * t44
      t593 = t591 * t18 * t300
      t599 = 0.144D3 * t106 * t443 * t11 * t48 * t399
      t603 = t285 * t443 * t12 * t48 * t86
      t607 = t267 * t127 * t170 * x3
      t613 = t8 * t152
      t615 = t536 * t100 * t613
      t618 = t532 * t222 * t430
      t621 = t227 * t228 * t300
      t624 = t8 * t35 * t85
      t626 = t536 * t100 * t624
      t630 = t202 * t49 * t177 * t85
      t632 = t108 * t73
      t634 = t285 * t286 * t632
      t643 = t12 * t54
      t645 = t169 * t643 * t576
      t649 = t99 * t100 * t9 * t35
      t651 = 0.3523D4 * t570 - 0.192D3 * t574 + 0.182D3 * t578 - 0.28D2 
     #* t583 - 0.56D2 * t589 + 0.328D3 * t593 + t599 + 0.216D3 * t603 + 
     #0.275D3 * t607 - 0.1152D4 * t187 * t8 * t11 * t262 + 0.430D3 * t61
     #5 - 0.328D3 * t618 - 0.1632D4 * t621 + 0.584D3 * t626 + 0.240D3 * 
     #t630 + 0.36D2 * t634 - 0.28D2 * t294 * t295 * t541 - 0.576D3 * t71
     # * t449 * t17 * t152 + 0.3466D4 * t645 - 0.3523D4 * t649
      t657 = t108 * x3
      t663 = t169 * t643 * t120
      t667 = t267 * t268 * t576 * t25
      t669 = t5 * t12
      t671 = t141 * t669 * t110
      t673 = t669 * t73
      t674 = t61 * t673
      t676 = t141 * t673
      t678 = t141 * t374
      t680 = t149 * t46
      t681 = t680 * t580
      t682 = t61 * t681
      t686 = t141 * t420
      t690 = t106 * t107 * t632 * t85
      t692 = t11 * t25
      t694 = t311 * t158 * t692
      t696 = t149 * t11
      t698 = x3 * t8
      t699 = t108 * t86
      t700 = t698 * t699
      t701 = t61 * t696 * t700
      t703 = t157 * t35
      t705 = t221 * t222 * t703
      t708 = t292 * t43
      t710 = t175 * t48
      t712 = t2 * t291 * t708 * t710 * t177
      t715 = t274 * t454 * t170
      t719 = t48 * t22
      t721 = t3 * t291 * t292 * t295 * t719
      t725 = t78 * t42 * t107 * t487
      t730 = t3 * t149 * x2 * t188 * t144
      t732 = -0.28D2 * t106 * t278 * t182 * x3 - 0.56D2 * t285 * t286 * 
     #t657 * t25 + 0.3523D4 * t663 - 0.530D3 * t667 - 0.47D2 * t671 + 0.
     #36D2 * t674 - 0.47D2 * t676 - 0.148D3 * t678 - 0.28D2 * t682 - 0.2
     #8D2 * t61 * t374 + 0.148D3 * t686 + 0.144D3 * t690 - 0.96D2 * t694
     # - 0.168D3 * t701 + 0.530D3 * t705 - 0.12D2 * t712 - 0.122D3 * t71
     #5 - 0.272D3 * t721 + 0.1088D4 * t725 - 0.376D3 * t730
      t733 = t220 * t46
      t736 = t733 * t93 * t60 * t73
      t738 = t531 * t12
      t741 = t738 * t93 * t60 * x3
      t750 = t116 * t42 * t261 * t108 * t119
      t759 = t352 * x2 * t209 * t54 * t119
      t761 = t35 * t85
      t762 = t409 * t761
      t765 = t377 * t762
      t767 = t6 ** 2
      t769 = t42 ** 2
      t781 = t115 * t40 * t292 * t341 * t17 * t119
      t784 = t377 * t98 * t189
      t786 = t61 * t6
      t792 = t377 * t98 * t54 * x3
      t796 = t387 * t388 * t86 * t35
      t799 = t221 * t222 * t462
      t803 = t106 * t278 * t189 * t73
      t807 = t492 * t222 * t168 * t25
      t810 = t135 * t100 * t556
      t815 = t133 * t168 * t175 * t222 * t506
      t817 = 0.192D3 * t736 - 0.182D3 * t741 + 0.144D3 * t387 * t388 * t
     #85 * t152 - 0.3523D4 * t750 - 0.576D3 * t61 * t40 * t708 * t454 + 
     #0.1244D4 * t759 + 0.288D3 * t408 * t762 - 0.880D3 * t765 + 0.36D2 
     #* t61 * t767 * t769 * t443 * t710 - 0.576D3 * t408 * t168 * t354 -
     # 0.10D2 * t781 - 0.17D2 * t784 + 0.864D3 * t786 * t195 * t118 + 0.
     #275D3 * t792 + 0.216D3 * t796 + 0.400D3 * t799 + 0.36D2 * t803 + 0
     #.1244D4 * t807 + 0.96D2 * t810 - 0.96D2 * t815
      t836 = t221 * t222 * t431
      t840 = -0.1168D4 * t38 + 0.1728D4 * t51 + 0.352D3 * t76 + 0.432D3 
     #* t89 - 0.62D2 * t95 - 0.4816D4 * t103 + 0.544D3 * t123 + 0.144D3 
     #* t131 + 0.496D3 * t139 - 0.752D3 * t147 - 0.640D3 * t160 - 0.9344
     #D4 * t172 - 0.54D2 * t180 + 0.256D3 * t836 - 0.216D3 * t200 + 0.21
     #6D3 * t205 + t214 - t219 + t226
      t854 = t738 * t93 * t60 * t25
      t860 = t141 * t149
      t862 = t860 * t470 * t182
      t869 = 0.3776D4 * t231 + 0.1728D4 * t236 + 0.776D3 * t241 - 0.128D
     #3 * t247 + 0.1728D4 * t252 + 0.432D3 * t256 - 0.864D3 * t259 + 0.5
     #42D3 * t271 - 0.604D3 * t276 + 0.288D3 * t303 + 0.1024D4 * t307 - 
     #0.256D3 * t854 + 0.136D3 * t733 * t93 * t60 * t110 - 0.64D2 * t862
     # - 0.496D3 * t314 + 0.144D3 * t318 + 0.16D2 * t141 + 0.496D3 * t32
     #4 - 0.256D3 * t330
      t873 = t141 * t150
      t877 = t141 * t696
      t879 = t877 * t487 * t613
      t881 = t322 * t85
      t883 = t221 * t222 * t881
      t885 = t6 * t46
      t887 = t61 * t885 * t22
      t890 = t887 * t108 * x1 * t129
      t892 = t6 * t12
      t894 = t61 * t892 * t22
      t895 = t17 * t8
      t898 = t894 * t895 * t761 * t25
      t900 = t61 * t892
      t903 = t900 * t487 * t698 * t86
      t906 = t61 * t6 * t11
      t910 = t906 * t719 * t43 * t86 * t35
      t920 = -0.640D3 * t334 + 0.144D3 * t339 - 0.64D2 * t873 * t153 * t
     #692 - 0.64D2 * t879 + 0.216D3 * t883 - 0.128D3 * t890 + 0.128D3 * 
     #t898 - 0.64D2 * t903 + 0.64D2 * t910 - 0.72D2 * t350 - t358 - 0.25
     #6D3 * t364 + 0.256D3 * t370 - 0.256D3 * t375 - 0.108D3 * t380 + 0.
     #134D3 * t385 - 0.64D2 * t397 - t402 - 0.608D3 * t406
      t933 = t117 * t118 * t576 * t35
      t936 = t591 * t18 * t229
      t942 = t873 * t154
      t946 = t585 * t586 * t229 * t85
      t948 = t873 * t338
      t953 = 0.592D3 * t413 - 0.72D2 * t416 + 0.264D3 * t418 + 0.256D3 *
     # t421 - 0.72D2 * t427 + 0.608D3 * t437 - 0.592D3 * t441 - 0.1376D4
     # * t451 + 0.864D3 * t456 - 0.1728D4 * t460 - 0.256D3 * t933 + 0.25
     #6D3 * t936 - 0.136D3 * t585 * t586 * t119 * t152 + 0.64D2 * t942 -
     # 0.216D3 * t946 + 0.128D3 * t948 + 0.1360D4 * t468 + 0.3776D4 * t4
     #73 - 0.54D2 * t478
      t975 = -0.190D3 * t482 + 0.128D3 * t490 - 0.4940D4 * t495 + 0.108D
     #3 * t498 - 0.190D3 * t501 - 0.1360D4 * t504 - 0.1728D4 * t508 + 0.
     #584D3 * t512 - 0.752D3 * t516 - 0.864D3 * t519 - 0.496D3 * t525 - 
     #0.264D3 * t529 + 0.256D3 * t534 + 0.24D2 * t539 + 0.344D3 * t544 +
     # 0.144D3 * t548 + 0.1728D4 * t551 - 0.1728D4 * t554 - 0.1728D4 * t
     #558
      t994 = 0.490D3 * t566 + 0.5266D4 * t570 + 0.30D2 * t574 - 0.108D3 
     #* t578 + 0.56D2 * t583 - 0.24D2 * t589 - 0.256D3 * t593 - t599 - 0
     #.72D2 * t603 - 0.156D3 * t607 - 0.542D3 * t615 + 0.604D3 * t618 + 
     #0.4816D4 * t621 - 0.544D3 * t626 + 0.64D2 * t630 - 0.72D2 * t634 +
     # 0.4940D4 * t645 - 0.5266D4 * t649 + 0.5266D4 * t663
      t999 = x3 * t25
      t1001 = t220 * t470 * t54 * t60 * t999
      t1004 = t61 * t6 * t175
      t1007 = t1004 * t93 * t110 * x3
      t1009 = t61 * t885
      t1016 = t117 * t118 * t229 * t25
      t1032 = 0.444D3 * t667 + 0.256D3 * t1001 - 0.64D2 * t1007 + 0.64D2
     # * t1009 * t144 * t430 * t110 - 0.256D3 * t1016 + 0.54D2 * t671 - 
     #0.72D2 * t674 + 0.54D2 * t676 + 0.216D3 * t678 + 0.56D2 * t682 - 0
     #.216D3 * t686 - 0.256D3 * t690 + 0.640D3 * t694 + 0.344D3 * t701 -
     # 0.444D3 * t705 - 0.16D2 * t712 + 0.134D3 * t715 + 0.432D3 * t721 
     #- 0.1728D4 * t725
      t1050 = t873 * t109 * t336 * t25
      t1052 = t877 * t700
      t1055 = t873 * t699 * t692
      t1059 = 0.128D3 * t877 * t487 * t537
      t1060 = 0.584D3 * t730 - 0.30D2 * t736 + 0.108D3 * t741 - 0.5266D4
     # * t750 - 0.3776D4 * t759 + 0.640D3 * t765 + 0.62D2 * t781 + 0.490
     #D3 * t784 - 0.156D3 * t792 - 0.72D2 * t796 - 0.288D3 * t799 - 0.72
     #D2 * t803 - 0.3776D4 * t807 - 0.640D3 * t810 + 0.640D3 * t815 - 0.
     #128D3 * t1050 + 0.64D2 * t1052 - 0.64D2 * t1055 + t1059
      t1105 = t141 * t423
      t1106 = t12 * t110
      t1123 = -0.48D2 * t860 * t46 * t25 * t73 + 0.48D2 * t860 * t46 * t
     #110 * x3 - 0.384D3 * t377 * t425 * z - 0.384D3 * t382 * t58 * t210
     # + 0.128D3 * t92 * x3 * t210 * z * t60 + 0.416D3 * t38 - 0.640D3 *
     # t51 + t77 - 0.160D3 * t89 - 0.304D3 * t95 + 0.32D2 * t877 * t487 
     #* t624 + 0.96D2 * t906 * t719 * t43 * t85 * t152 + 0.96D2 * t1009 
     #* t144 * t328 * t85 + 0.32D2 * t429 * t144 * t881 - 0.96D2 * t429 
     #* t329 - 0.96D2 * t1105 * t312 * t1106 + 0.192D3 * t1105 * t312 * 
     #t379 - 0.192D3 * t894 * t895 * t761 * x3 + 0.16D2 * t906 * t719 * 
     #t43 * t389 - 0.288D3 * t1105 * t426
      t1161 = x3 * z
      t1174 = t61 * t680
      t1185 = -0.416D3 * t429 * t144 * t703 - 0.48D2 * t900 * t487 * t61
     #3 * x3 + 0.48D2 * t1009 * t144 * t430 * t73 - 0.64D2 * t887 * t657
     # * t431 - 0.416D3 * t141 * t680 * t93 * t999 + 0.32D2 * t900 * t48
     #7 * t613 * t25 + 0.288D3 * t1004 * t93 * t25 * t73 + 0.64D2 * t429
     # * t157 * t158 * t25 + 0.912D3 * t103 - 0.74208D5 * t123 - 0.8D1 *
     # t139 + 0.74296D5 * t147 - 0.32D2 * t1105 * t158 * t1106 - 0.384D3
     # * t143 * t1161 * t56 - 0.768D3 * t141 * t669 * t93 * t1161 + 0.38
     #4D3 * t61 * t669 * t93 * x3 * t210 + 0.384D3 * t1174 * t93 * t73 *
     # z + 0.384D3 * t1174 * t93 * t1161 * t25 + 0.256D3 * t160 + 0.1728
     #D4 * t172
      t1211 = -0.252D3 * t180 + 0.25264D5 * t836 + 0.128D3 * t200 - 0.64
     #0D3 * t231 - 0.640D3 * t236 - 0.304D3 * t241 + t248 - 0.640D3 * t2
     #52 - 0.160D3 * t256 + 0.320D3 * t259 - 0.28208D5 * t271 + 0.27920D
     #5 * t276 - 0.74288D5 * t303 - 0.1152D4 * t307 - 0.25296D5 * t854 +
     # 0.16D2 * t862 + 0.112D3 * t786 * t175 * t22 * t581 - 0.672D3 * t8
     #60 * t470 * t74 + 0.8D1 * t314 - 0.64D2 * t141
      t1230 = -0.8D1 * t324 + 0.288D3 * t330 + 0.128D3 * t334 + 0.16D2 *
     # t879 + 0.80D2 * t883 - 0.64D2 * t890 + 0.64D2 * t898 - 0.128D3 * 
     #t903 + 0.128D3 * t910 + t365 - 0.512D3 * t370 + 0.512D3 * t375 + 0
     #.8D1 * t380 + 0.304D3 * t385 + 0.128D3 * t397 + 0.128D3 * t406 - 0
     #.64D2 * t413 - 0.64D2 * t418 - t422 - 0.64D2 * t433
      t1252 = -0.128D3 * t437 + 0.64D2 * t441 + 0.576D3 * t451 - 0.320D3
     # * t456 + 0.640D3 * t460 - 0.192D3 * t464 - 0.25248D5 * t933 + 0.2
     #5296D5 * t936 - 0.128D3 * t942 - 0.80D2 * t946 + 0.64D2 * t948 - 0
     #.512D3 * t468 - 0.640D3 * t473 + 0.4D1 * t478 + 0.28224D5 * t482 -
     # t491 + 0.424D3 * t495 - 0.8D1 * t498 + 0.28224D5 * t501 + 0.512D3
     # * t504
      t1273 = 0.640D3 * t508 - 0.208D3 * t512 + 0.74296D5 * t516 + 0.320
     #D3 * t519 + 0.8D1 * t525 + 0.64D2 * t529 - 0.74320D5 * t534 + 0.32
     #D2 * t539 - 0.144D3 * t544 - 0.640D3 * t551 + 0.640D3 * t554 + 0.6
     #40D3 * t558 + 0.25220D5 * t566 - 0.780D3 * t570 + 0.28792D5 * t574
     # - 0.29128D5 * t578 - 0.144D3 * t583 - 0.32D2 * t589 + 0.74320D5 *
     # t593 - 0.28372D5 * t607
      t1295 = 0.28208D5 * t615 - 0.27920D5 * t618 - 0.912D3 * t621 + 0.7
     #4208D5 * t626 - 0.128D3 * t630 - 0.424D3 * t645 + 0.780D3 * t649 -
     # 0.780D3 * t663 + 0.536D3 * t667 + 0.25248D5 * t1001 + 0.128D3 * t
     #1007 - 0.25264D5 * t1016 - 0.4D1 * t671 + 0.252D3 * t676 - 0.128D3
     # * t678 - 0.144D3 * t682 + 0.288D3 * t690 - 0.144D3 * t701 - 0.536
     #D3 * t705 + 0.64D2 * t712
      t1320 = -0.128D3 * t765 + 0.304D3 * t781 + 0.25220D5 * t784 - 0.28
     #372D5 * t792 + 0.74288D5 * t799 + 0.640D3 * t807 - 0.256D3 * t815 
     #- 0.64D2 * t1050 + 0.128D3 * t1052 - 0.128D3 * t1055 + t1059
      rrgq2qgh11J5 = (wd * (t193 + t309 + t403 + t484 + t568 + t651 + t7
     #32 + t817) + wd * (t840 + t869 + t920 + t953 + t975 + t994 + t1032
     # + t1060) + wd * (t1123 + t1185 + t1211 + t1230 + t1252 + t1273 + 
     #t1295 + 0.304D3 * t715 - 0.16D2 * t141 * t680 * t279 - 0.112D3 * t
     #141 * t681 - 0.160D3 * t721 + 0.640D3 * t725 - 0.208D3 * t730 - 0.
     #28792D5 * t736 + 0.29128D5 * t741 + 0.780D3 * t750 + 0.640D3 * t75
     #9 + t1320)) / t1 / t119 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh11J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t5 * t3
      t7 = t2 * t6
      t8 = x1 ** 2
      t9 = x2 * t8
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t11 * t10
      t14 = t7 * t9 * t12
      t16 = z + x1 * t3
      t17 = t16 ** 2
      t19 = 0.1D1 / t17 / t16
      t20 = s * t3
      t21 = 0.1D1 / t16
      t22 = x1 * t21
      t23 = 0.1D1 - x2
      t24 = x3 * t23
      t26 = 0.1D1 - x3
      t29 = cos(x4 * 0.3141592653589793D1)
      t33 = Sqrt(t24 * t16 * x2 * t26)
      t35 = 0.2D1 * t29 * t33
      t36 = t24 * t16 + x2 * t26 - t35
      t37 = t22 * t36
      t39 = t10 * x3
      t41 = s - t20 * t37 - t20 * t39
      t42 = t19 * t41
      t45 = z + x1 * t23 * t3
      t46 = t45 * x3
      t47 = t46 * t36
      t49 = t14 * t42 * t47
      t51 = t41 ** 2
      t52 = s * t1
      t53 = t51 * t52
      t54 = t4 * t10
      t55 = t53 * t54
      t56 = 0.1D1 / t17
      t57 = t45 * t56
      t58 = x1 * t36
      t60 = t55 * t57 * t58
      t62 = t4 * t3
      t63 = t2 * t62
      t64 = t11 * t45
      t65 = t63 * t64
      t66 = t56 * t41
      t67 = x3 * x1
      t68 = t67 * t36
      t70 = t65 * t66 * t68
      t72 = t41 * t2
      t73 = t6 * x2
      t74 = t72 * t73
      t75 = t8 * x1
      t76 = t75 * t11
      t77 = x3 * t19
      t81 = t26 * t23 * t16 + x2 * x3 + t35
      t82 = t81 ** 2
      t85 = t74 * t76 * t77 * t82
      t87 = t5 * t4
      t88 = x2 ** 2
      t90 = t72 * t87 * t88
      t91 = t75 * t12
      t96 = t2 * s
      t97 = t96 * t5
      t98 = t97 * t9
      t99 = t11 * t19
      t100 = t45 * t81
      t102 = t98 * t99 * t100
      t104 = t96 * t6
      t105 = t104 * t9
      t106 = t12 * t19
      t107 = t100 * x3
      t109 = t105 * t106 * t107
      t111 = t21 * t36
      t113 = t55 * t67 * t111
      t115 = t52 * t62
      t116 = x1 * x2
      t117 = t115 * t116
      t118 = t11 * t56
      t119 = t51 * t45
      t121 = t117 * t118 * t119
      t123 = t7 * t116
      t124 = t11 ** 2
      t125 = t124 * t56
      t126 = t41 * t45
      t127 = x3 ** 2
      t130 = t123 * t125 * t126 * t127
      t132 = t52 * t5
      t133 = t132 * t116
      t134 = t12 * t56
      t137 = t133 * t134 * t119 * x3
      t139 = t2 * t87
      t140 = t88 * t75
      t141 = t139 * t140
      t142 = t17 ** 2
      t143 = 0.1D1 / t142
      t144 = t12 * t143
      t151 = x1 * t10
      t152 = z ** 2
      t159 = x1 * t11
      t169 = t115 * t9
      t170 = t10 * t56
      t171 = t51 * t36
      t173 = t169 * t170 * t171
      t175 = t8 * t12
      t176 = t56 * t127
      t179 = t74 * t175 * t176 * t81
      t182 = t53 * t4 * x1
      t183 = t10 * t26
      t187 = t5 * t62
      t189 = t88 * x2
      t194 = t2 * t187 * t189 * t75 * t124 * t143 * t126
      t196 = 0.108D3 * t49 - 0.190D3 * t60 - 0.444D3 * t70 + 0.344D3 * t
     #85 + 0.144D3 * t90 * t91 * t77 * t81 + 0.1728D4 * t102 - 0.1728D4 
     #* t109 - 0.640D3 * t113 - 0.9344D4 * t121 - 0.54D2 * t130 - 0.156D
     #3 * t137 + 0.216D3 * t141 * t144 * t126 * t36 + 0.1728D4 * t72 * t
     #4 * x2 * t151 * t21 * t152 - 0.1728D4 * t72 * t62 * x2 * t159 * t2
     #1 * z * x3 + 0.1728D4 * t65 * t66 * t116 * z + 0.3776D4 * t173 - 0
     #.256D3 * t179 + 0.640D3 * t182 * t111 * t183 - 0.16D2 * t194
      t197 = x2 * t75
      t198 = t104 * t197
      t199 = t11 * t143
      t200 = t36 ** 2
      t203 = t198 * t199 * t45 * t200
      t205 = t2 * t5
      t206 = t205 * t64
      t209 = t206 * t42 * t9 * t36
      t211 = t11 * t21
      t212 = t51 * t26
      t214 = t117 * t211 * t212
      t216 = t62 * t11
      t217 = t72 * t216
      t218 = t127 * x1
      t219 = t21 * t81
      t220 = t218 * t219
      t221 = t217 * t220
      t224 = t45 * t26
      t225 = t224 * t81
      t227 = t14 * t42 * t225
      t229 = t62 * t8
      t231 = t56 * t81
      t232 = t36 * t10
      t234 = t231 * t232 * x3
      t237 = t62 * t10
      t238 = t53 * t237
      t239 = t45 * t19
      t240 = t8 * t82
      t243 = 0.128D3 * t238 * t239 * t240
      t244 = t45 * t36
      t246 = t98 * t99 * t244
      t248 = t96 * t87
      t249 = t248 * t140
      t251 = t249 * t144 * t244
      t253 = t5 * x2
      t254 = t72 * t253
      t255 = x1 * t12
      t256 = t21 * t127
      t258 = t254 * t255 * t256
      t262 = t198 * t199 * t45 * t82
      t264 = t53 * t229
      t265 = t264 * t234
      t269 = t264 * t231 * t232 * t26
      t271 = x3 * t8
      t272 = t56 * t82
      t273 = t271 * t272
      t274 = t238 * t273
      t277 = t264 * t272 * t183
      t281 = t206 * t42 * t9 * t81
      t283 = t132 * t9
      t286 = t283 * t118 * t212 * t81
      t291 = t81 * t26 * x3
      t295 = 0.584D3 * t203 - 0.5266D4 * t209 + 0.5266D4 * t214 - 0.256D
     #3 * t221 + 0.16D2 * t53 - 0.640D3 * t227 + 0.144D3 * t72 * t229 * 
     #t234 + t243 - 0.1168D4 * t246 + 0.1728D4 * t251 + 0.352D3 * t258 +
     # 0.432D3 * t262 + 0.128D3 * t265 - 0.128D3 * t269 + 0.64D2 * t274 
     #- 0.64D2 * t277 - 0.4816D4 * t281 + 0.544D3 * t286 + 0.144D3 * t72
     # * t73 * t8 * t134 * t291
      t297 = t52 * t6
      t298 = t88 * t8
      t299 = t297 * t298
      t301 = t299 * t106 * t119
      t304 = t105 * t106 * t47
      t306 = t12 * t45
      t309 = t7 * t306 * t42 * t298
      t311 = t7 * t197
      t314 = t311 * t199 * t126 * t200
      t316 = t244 * t26
      t322 = t46 * t26
      t324 = t7 * t116 * t124 * t66 * t322
      t328 = t133 * t134 * t119 * t26
      t331 = t96 * t152 * t253
      t332 = t8 * t11
      t335 = t331 * t332 * t239 * t36
      t337 = t205 * t306
      t340 = t337 * t66 * t116 * x3
      t342 = t5 * t11
      t344 = t72 * t342 * t45
      t345 = t19 * t8
      t346 = t81 * t36
      t349 = t344 * t345 * t346 * t26
      t351 = t72 * t342
      t354 = t351 * t239 * t271 * t82
      t357 = t72 * t5 * t10
      t358 = t45 * t143
      t362 = t357 * t358 * t75 * t82 * t36
      t366 = t283 * t99 * t119 * t81
      t369 = t249 * t144 * t100
      t374 = t244 * t81
      t376 = t7 * t197 * t11 * t143 * t41 * t374
      t379 = t198 * t199 * t374
      t382 = t182 * t219 * t39
      t388 = t26 * x1
      t390 = t55 * t388 * t219
      t392 = 0.134D3 * t301 + 0.1360D4 * t304 + 0.3776D4 * t309 - 0.54D2
     # * t314 - 0.640D3 * t14 * t42 * t316 + 0.640D3 * t324 - 0.190D3 * 
     #t328 + 0.128D3 * t335 - 0.4940D4 * t340 + 0.128D3 * t349 - 0.64D2 
     #* t354 + 0.64D2 * t362 - 0.752D3 * t366 - 0.864D3 * t369 - 0.496D3
     # * t376 - 0.1728D4 * t379 - 0.496D3 * t382 + 0.144D3 * t72 * t54 *
     # t67 * t219 + 0.496D3 * t390
      t393 = t104 * t116
      t394 = t26 ** 2
      t397 = t393 * t125 * t45 * t394
      t399 = t248 * t298
      t400 = t124 * t19
      t402 = t399 * t400 * t224
      t404 = t62 * x1
      t406 = t11 * t127
      t407 = t111 * t406
      t410 = t139 * t298
      t413 = t410 * t400 * t126 * t26
      t417 = t123 * t125 * t126 * t394
      t420 = t105 * t106 * t316
      t424 = t283 * t99 * t119 * t36
      t426 = t132 * t197
      t427 = t10 * t19
      t430 = t426 * t427 * t171 * t81
      t432 = t51 * x3
      t434 = t117 * t211 * t432
      t437 = t72 * t237 * t273
      t441 = t311 * t199 * t126 * t82
      t443 = t2 * t4
      t444 = t10 * t45
      t445 = t443 * t444
      t446 = x1 * t81
      t448 = t445 * t66 * t446
      t450 = t63 * t444
      t452 = t450 * t42 * t240
      t454 = t51 * t81
      t456 = t169 * t170 * t454
      t459 = t8 * t36 * t81
      t461 = t450 * t42 * t459
      t465 = t141 * t144 * t126 * t81
      t467 = t19 * t82
      t472 = t52 * t4
      t483 = t96 * t187 * t189 * t75 * t124 * t358
      t485 = 0.432D3 * t397 - 0.864D3 * t402 - 0.72D2 * t72 * t404 * t40
     #7 + 0.608D3 * t413 - 0.592D3 * t417 - 0.1728D4 * t420 + 0.490D3 * 
     #t424 - 0.216D3 * t430 + 0.4940D4 * t434 + 0.344D3 * t437 - 0.264D3
     # * t441 + 0.256D3 * t448 + 0.24D2 * t452 + 0.4816D4 * t456 - 0.544
     #D3 * t461 + 0.64D2 * t465 - 0.72D2 * t74 * t76 * t467 * t26 - 0.17
     #28D4 * t472 * t116 * t10 * t21 * t51 * z + 0.432D3 * t483
      t490 = t104 * t88 * t175 * t239
      t495 = t96 * t62 * x2 * t159 * t57
      t497 = t63 * t12
      t498 = t45 * t21
      t501 = t497 * t498 * t41 * t127
      t503 = t443 * t11
      t506 = t503 * t498 * t41 * x3
      t511 = t132 * t88 * t332 * t56 * t51
      t516 = t472 * x2 * t151 * t21 * t51
      t518 = t53 * t4
      t519 = t8 * t56
      t521 = t518 * t519 * t346
      t527 = t52 * t87 * t189 * t91 * t19 * t51
      t529 = t21 * t26
      t531 = t518 * t64 * t529
      t535 = t518 * t64 * t21 * x3
      t537 = t72 * t62
      t538 = t75 * t19
      t545 = t503 * t498 * t41 * t26
      t551 = t53 * t62
      t554 = t551 * t306 * t21 * t394
      t556 = t97 * t116
      t558 = t556 * t134 * t46
      t561 = t393 * t125 * t322
      t564 = t217 * t57 * t116
      t566 = t12 * t21
      t569 = t133 * t566 * t51 * t127
      t572 = t299 * t134 * t432
      t574 = -0.1728D4 * t490 + 0.584D3 * t495 - 0.30D2 * t501 + 0.108D3
     # * t506 - 0.5266D4 * t511 - 0.3776D4 * t516 + 0.640D3 * t521 + 0.6
     #2D2 * t527 + 0.490D3 * t531 - 0.156D3 * t535 - 0.72D2 * t537 * t53
     #8 * t82 * t36 - 0.256D3 * t545 + 0.136D3 * t497 * t498 * t41 * t39
     #4 - 0.64D2 * t554 - 0.1360D4 * t558 - 0.1728D4 * t561 + 0.5266D4 *
     # t564 + 0.30D2 * t569 - 0.108D3 * t572
      t575 = x1 * t124
      t576 = t127 * x3
      t577 = t21 * t576
      t579 = t74 * t575 * t577
      t586 = t2 * t3 * t10
      t588 = t586 * t498 * t41
      t590 = t53 * z
      t591 = t3 * t10
      t592 = t591 * x3
      t593 = t590 * t592
      t596 = t11 * x3 * t26
      t597 = t518 * t596
      t599 = t53 * t3
      t601 = t599 * t444 * t21
      t604 = t599 * t22 * t81
      t606 = t82 * t81
      t610 = t599 * t37
      t613 = t518 * t519 * t200
      t616 = t519 * t82
      t619 = t518 * t616
      t621 = t591 * t26
      t622 = t590 * t621
      t624 = t8 * t200
      t626 = t450 * t42 * t624
      t629 = t445 * t66 * t58
      t633 = t410 * t400 * t126 * x3
      t636 = t399 * t400 * t46
      t640 = t393 * t125 * t45 * t127
      t644 = t331 * t255 * t57 * x3
      t646 = 0.56D2 * t579 - 0.72D2 * t90 * t8 * t124 * t176 - 0.62D2 * 
     #t588 - 0.256D3 * t593 - 0.108D3 * t597 + 0.134D3 * t601 - 0.64D2 *
     # t604 - 0.144D3 * t537 * t538 * t606 - 0.608D3 * t610 + 0.592D3 * 
     #t613 - 0.72D2 * t72 * t4 * t616 + 0.264D3 * t619 + 0.256D3 * t622 
     #- 0.542D3 * t626 + 0.604D3 * t629 - 0.216D3 * t633 + 0.1728D4 * t6
     #36 + 0.776D3 * t640 - 0.128D3 * t644
      t649 = t556 * t134 * t224
      t652 = t55 * t57 * t446
      t654 = t446 * x3
      t656 = t65 * t66 * t654
      t664 = t337 * t66 * t116 * t26
      t667 = t14 * t42 * t107
      t671 = t426 * t427 * t51 * t82
      t673 = t297 * t140
      t675 = t673 * t99 * t454
      t677 = t8 ** 2
      t685 = t45 * z
      t688 = t205 * t9 * t11 * t42 * t685 * t36
      t694 = t205 * t116 * t12 * t66 * t685 * x3
      t698 = t283 * t118 * t454 * x3
      t702 = t254 * t332 * t231 * x3
      t705 = t238 * t239 * t624
      t707 = t388 * t81
      t709 = t65 * t66 * t707
      t711 = t5 * t12
      t713 = t72 * t711 * t45
      t716 = t713 * t56 * x1 * t291
      t720 = t133 * t566 * t432 * t26
      t724 = t133 * t566 * t51 * t394
      t727 = t299 * t134 * t212
      t729 = 0.1728D4 * t649 - 0.752D3 * t652 - 0.288D3 * t656 - 0.72D2 
     #* t74 * t575 * t529 * t127 - 0.3776D4 * t664 + 0.496D3 * t667 - 0.
     #24D2 * t671 - 0.256D3 * t675 - 0.144D3 * t74 * t677 * t10 * t143 *
     # t606 - 0.256D3 * t688 + 0.256D3 * t694 + 0.288D3 * t698 + 0.1024D
     #4 * t702 - 0.64D2 * t705 + 0.216D3 * t709 - 0.128D3 * t716 + 0.444
     #D3 * t720 + 0.542D3 * t724 - 0.604D3 * t727
      t730 = t4 * t11
      t732 = t53 * t730 * t394
      t734 = t730 * t127
      t737 = t53 * t734
      t739 = t53 * t592
      t741 = t62 * t12
      t742 = t741 * t576
      t743 = t72 * t742
      t747 = t58 * t26
      t749 = t65 * t66 * t747
      t753 = x3 * t26
      t755 = t63 * t306 * t21 * t41 * t753
      t764 = t254 * t75 * t10 * t467
      t767 = t105 * t106 * t225
      t770 = t72 * t5 * t124
      t773 = t770 * t498 * t394 * x3
      t775 = t72 * t711
      t782 = t283 * t118 * t171 * t26
      t786 = t283 * t118 * t432 * t36
      t789 = t673 * t99 * t171
      t795 = t56 * t200
      t797 = t264 * t795 * t39
      t802 = 0.54D2 * t732 - 0.72D2 * t72 * t734 + 0.54D2 * t737 + 0.216
     #D3 * t739 + 0.56D2 * t743 - 0.216D3 * t53 * t621 + 0.256D3 * t749 
     #+ 0.256D3 * t755 - 0.72D2 * t90 * t677 * t11 * t143 * t82 - 0.1376
     #D4 * t764 + 0.864D3 * t767 - 0.64D2 * t773 + 0.64D2 * t775 * t57 *
     # t58 * t394 - 0.256D3 * t782 - 0.256D3 * t786 + 0.256D3 * t789 - 0
     #.136D3 * t426 * t427 * t51 * t200 + 0.64D2 * t797 - 0.64D2 * t264 
     #* t795 * t183
      t826 = -0.8D1 * t49 + 0.28224D5 * t60 - 0.536D3 * t70 - 0.144D3 * 
     #t85 - 0.640D3 * t102 + 0.640D3 * t109 + 0.256D3 * t113 + 0.1728D4 
     #* t121 - 0.252D3 * t130 - 0.28372D5 * t137 - 0.640D3 * t173 + 0.28
     #8D3 * t179 + 0.64D2 * t194 - 0.208D3 * t203 + 0.780D3 * t209 - 0.7
     #80D3 * t214 + 0.288D3 * t221 - 0.64D2 * t53 + 0.128D3 * t227 + t24
     #3
      t844 = t53 * t216
      t852 = t53 * t404
      t853 = t11 * t394
      t871 = 0.416D3 * t246 - 0.640D3 * t251 + 0.576D3 * t258 - 0.160D3 
     #* t262 + 0.64D2 * t265 - 0.64D2 * t269 + 0.128D3 * t274 - 0.128D3 
     #* t277 + 0.96D2 * t357 * t358 * t75 * t81 * t200 + 0.96D2 * t775 *
     # t57 * t218 * t81 + 0.32D2 * t844 * t57 * t707 + 0.912D3 * t281 - 
     #0.74208D5 * t286 - 0.96D2 * t844 * t220 - 0.96D2 * t852 * t219 * t
     #853 + 0.192D3 * t852 * t219 * t596 - 0.192D3 * t344 * t345 * t346 
     #* x3 + 0.16D2 * t357 * t358 * t75 * t200 * t36 + 0.304D3 * t301 - 
     #0.512D3 * t304
      t913 = -0.640D3 * t309 + 0.4D1 * t314 - 0.192D3 * t844 * t57 * t65
     #4 - 0.256D3 * t324 + 0.28224D5 * t328 - 0.64D2 * t335 + 0.424D3 * 
     #t340 + 0.64D2 * t349 - 0.128D3 * t354 + 0.128D3 * t362 + 0.112D3 *
     # t72 * t5 * t124 * t45 * t577 + 0.74296D5 * t366 + 0.320D3 * t369 
     #+ 0.8D1 * t376 - 0.48D2 * t551 * t12 * t26 * t127 + 0.48D2 * t551 
     #* t12 * t394 * x3 - 0.384D3 * t518 * t406 * z - 0.384D3 * t599 * t
     #39 * t152 + 0.128D3 * t586 * x3 * t152 * z * t41 + 0.640D3 * t379
      t916 = x3 * z
      t929 = t72 * t741
      t951 = 0.8D1 * t382 - 0.8D1 * t390 - 0.384D3 * t55 * t916 * t37 - 
     #0.768D3 * t53 * t730 * t498 * t916 + 0.384D3 * t72 * t730 * t498 *
     # x3 * t152 + 0.384D3 * t929 * t498 * t127 * z + 0.384D3 * t929 * t
     #498 * t916 * t26 - 0.160D3 * t397 + 0.320D3 * t402 - 0.128D3 * t41
     #3 + 0.64D2 * t417 + 0.640D3 * t420 + 0.25220D5 * t424 - 0.80D2 * t
     #430 - 0.424D3 * t434 - 0.144D3 * t437 + 0.64D2 * t441 - 0.74320D5 
     #* t448 + 0.32D2 * t452 - 0.912D3 * t456
      t981 = 0.74208D5 * t461 - 0.128D3 * t465 - 0.288D3 * t852 * t407 -
     # 0.416D3 * t844 * t57 * t68 + 0.32D2 * t238 * t239 * t459 - 0.160D
     #3 * t483 + 0.640D3 * t490 - 0.208D3 * t495 - 0.28792D5 * t501 + 0.
     #29128D5 * t506 + 0.780D3 * t511 + 0.640D3 * t516 - 0.128D3 * t521 
     #+ 0.304D3 * t527 + 0.25220D5 * t531 - 0.28372D5 * t535 - 0.25296D5
     # * t545 + 0.16D2 * t554 - 0.672D3 * t551 * t306 * t256 + 0.512D3 *
     # t558
      t1006 = 0.640D3 * t561 - 0.780D3 * t564 + 0.28792D5 * t569 - 0.291
     #28D5 * t572 - 0.144D3 * t579 - 0.304D3 * t588 + 0.512D3 * t593 + 0
     #.8D1 * t597 + 0.304D3 * t601 + 0.128D3 * t604 + 0.128D3 * t610 - 0
     #.64D2 * t613 - 0.64D2 * t619 - 0.128D3 * t622 - 0.16D2 * t53 * t74
     #1 * t394 * t26 - 0.112D3 * t53 * t742 + 0.28208D5 * t626 - 0.27920
     #D5 * t629 + 0.128D3 * t633 - 0.640D3 * t636
      t1039 = -0.304D3 * t640 + 0.64D2 * t644 - 0.640D3 * t649 + 0.74296
     #D5 * t652 + 0.74288D5 * t656 + 0.640D3 * t664 - 0.8D1 * t667 - 0.3
     #2D2 * t671 + 0.74320D5 * t675 + 0.128D3 * t688 - 0.512D3 * t694 - 
     #0.74288D5 * t698 - 0.1152D4 * t702 + 0.32D2 * t351 * t239 * t624 *
     # t26 + 0.288D3 * t770 * t498 * t26 * t127 + 0.64D2 * t844 * t67 * 
     #t111 * t26 - 0.32D2 * t852 * t111 * t853 + 0.16D2 * t705 + 0.80D2 
     #* t709 - 0.64D2 * t716
      t1076 = -0.320D3 * t767 + 0.128D3 * t773 - 0.25264D5 * t782 - 0.25
     #248D5 * t786 - 0.64D2 * t844 * t57 * t747 - 0.48D2 * t351 * t239 *
     # t624 * x3 + 0.48D2 * t775 * t57 * t58 * t127 - 0.64D2 * t713 * t5
     #6 * x3 * t747 - 0.416D3 * t53 * t741 * t498 * t753 + 0.25296D5 * t
     #789 - 0.128D3 * t797
      rrgq2qgh11J6 = (wd * (t196 + t295 + t392 + t485 + t574 + t646 + t7
     #29 + t802) + wd * (t826 + t871 + t913 + t951 + t981 + t1006 + t103
     #9 + 0.536D3 * t720 - 0.28208D5 * t724 + 0.27920D5 * t727 - 0.4D1 *
     # t732 + 0.252D3 * t737 - 0.128D3 * t739 - 0.144D3 * t743 + 0.25264
     #D5 * t749 + 0.25248D5 * t755 + 0.576D3 * t764 + t1076)) / t1 / t51
     # / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh11J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = x3 * t23
      t26 = s - t2 * t21 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t31 = t1 ** 2
      t33 = t30 * t31 * t23
      t34 = x3 * x1
      t35 = t5 * t20
      t39 = t31 * t1
      t40 = t29 * t39
      t41 = x1 * x2
      t42 = t40 * t41
      t43 = t23 ** 2
      t44 = t43 * t5
      t45 = t27 * x3
      t49 = t28 ** 2
      t50 = t26 * t49
      t51 = t39 * t23
      t53 = x1 ** 2
      t54 = x3 * t53
      t55 = t4 ** 2
      t56 = 0.1D1 / t55
      t60 = t10 * t7 * t4 + x2 * x3 + t19
      t61 = t60 ** 2
      t62 = t56 * t61
      t63 = t54 * t62
      t66 = t31 ** 2
      t67 = t66 * t1
      t68 = t49 * t67
      t69 = t53 * x1
      t70 = x2 * t69
      t71 = t68 * t70
      t72 = t55 ** 2
      t73 = 0.1D1 / t72
      t74 = t43 * t73
      t77 = z + x1 * t7 * t1
      t78 = t26 * t77
      t83 = t49 * s
      t84 = t83 * t66
      t85 = t84 * t41
      t86 = t43 * t23
      t87 = t86 * t56
      t88 = t77 * t10
      t92 = t83 * t67
      t93 = t92 * t41
      t94 = t43 ** 2
      t95 = t94 * t56
      t96 = t10 ** 2
      t101 = t66 * t31
      t102 = t83 * t101
      t103 = x2 ** 2
      t104 = t103 * t53
      t105 = t102 * t104
      t107 = 0.1D1 / t55 / t4
      t108 = t94 * t107
      t112 = t66 * x2
      t113 = t50 * t112
      t114 = t53 * t43
      t115 = t56 * t60
      t120 = t39 * t86
      t125 = x3 ** 2
      t126 = t125 * x3
      t127 = t120 * t126
      t130 = t49 * t66
      t131 = t43 * t77
      t132 = t130 * t131
      t133 = t107 * t26
      t134 = x2 * t53
      t139 = t29 * t66
      t140 = t139 * t134
      t141 = t43 * t56
      t142 = t27 * t10
      t148 = t68 * t134 * t86
      t149 = t77 * t60
      t150 = t149 * x3
      t158 = t20 ** 2
      t163 = t39 * t43
      t164 = t30 * t163
      t165 = t77 * t56
      t166 = x1 * t20
      t167 = t166 * t10
      t171 = t49 * t101
      t172 = t171 * t104
      t177 = t50 * t163
      t178 = t125 * x1
      t179 = t5 * t60
      t180 = t178 * t179
      t183 = t88 * t60
      t188 = t50 * t67 * x2
      t195 = 0.256D3 * t33 * t34 * t35 - 0.424D3 * t42 * t44 * t45 - 0.1
     #44D3 * t50 * t51 * t63 + 0.64D2 * t71 * t74 * t78 * t61 - 0.640D3 
     #* t85 * t87 * t88 - 0.160D3 * t93 * t95 * t77 * t96 + 0.320D3 * t1
     #05 * t108 * t88 - 0.1152D4 * t113 * t114 * t115 * x3 - 0.16D2 * t3
     #0 * t120 * t96 * t10 - 0.112D3 * t30 * t127 + 0.912D3 * t132 * t13
     #3 * t134 * t60 - 0.74208D5 * t140 * t141 * t142 * t60 - 0.8D1 * t1
     #48 * t133 * t150 - 0.25248D5 * t140 * t141 * t45 * t20 + 0.4D1 * t
     #71 * t74 * t78 * t158 - 0.64D2 * t164 * t165 * t167 - 0.128D3 * t1
     #72 * t108 * t78 * t10 + 0.288D3 * t177 * t180 + 0.128D3 * t148 * t
     #133 * t183 - 0.144D3 * t188 * t69 * t43 * t107 * x3 * t61
      t196 = t43 * t107
      t197 = t27 * t77
      t202 = t66 * t39
      t204 = t103 * x2
      t211 = t29 * t67
      t212 = t211 * t104
      t213 = t86 * t107
      t217 = t92 * t134
      t218 = t77 * x3
      t219 = t218 * t20
      t223 = t86 * t77
      t230 = t77 * z
      t235 = t139 * t41
      t240 = t49 * t39
      t241 = t23 * t77
      t242 = t240 * t241
      t243 = t53 * t158
      t247 = t49 * t31
      t248 = t247 * t241
      t249 = t56 * t26
      t253 = t92 * t70
      t254 = t77 * t20
      t255 = t254 * t60
      t259 = x1 * t86
      t260 = t5 * t125
      t268 = t103 * t69
      t269 = t102 * t268
      t270 = t86 * t73
      t274 = t66 * t43
      t275 = t50 * t274
      t276 = t77 * t107
      t284 = t139 * t70
      t285 = t23 * t107
      t290 = t30 * t31
      t291 = t53 * t56
      t292 = t20 * t60
      t311 = 0.25220D5 * t140 * t196 * t197 * t20 + 0.64D2 * t49 * t202 
     #* t204 * t69 * t94 * t73 * t78 + 0.304D3 * t212 * t213 * t197 - 0.
     #512D3 * t217 * t213 * t219 - 0.640D3 * t68 * t223 * t133 * t104 + 
     #0.128D3 * t130 * t134 * t43 * t133 * t230 * t20 - 0.28372D5 * t235
     # * t87 * t197 * x3 + 0.28208D5 * t242 * t133 * t243 - 0.27920D5 * 
     #t248 * t249 * t166 + 0.640D3 * t253 * t74 * t255 + 0.576D3 * t113 
     #* t259 * t260 - 0.160D3 * t253 * t74 * t77 * t61 - 0.640D3 * t269 
     #* t270 * t254 + 0.32D2 * t275 * t276 * t243 * t10 + 0.512D3 * t85 
     #* t87 * t218 - 0.32D2 * t284 * t285 * t27 * t61 - 0.128D3 * t290 *
     # t291 * t292 + 0.304D3 * t29 * t101 * t204 * t69 * t86 * t107 * t2
     #7 + 0.25220D5 * t290 * t131 * t5 * t10 - 0.28372D5 * t290 * t131 *
     # t5 * x3
      t313 = t247 * t43
      t314 = t77 * t5
      t319 = t30 * t39
      t324 = t40 * t134
      t325 = t23 * t56
      t326 = t27 * t60
      t330 = t50 * t120
      t335 = t66 * t86
      t336 = t50 * t335
      t341 = x1 * t10
      t342 = t341 * t60
      t353 = t68 * t41
      t358 = t31 * t43
      t360 = x3 * z
      t365 = z ** 2
      t373 = t30 * t39 * x1
      t374 = t43 * t96
      t379 = t43 * t10 * x3
      t386 = t73 * t77
      t391 = t53 * t86
      t415 = -0.25296D5 * t313 * t314 * t26 * t10 + 0.16D2 * t319 * t223
     # * t5 * t96 - 0.912D3 * t324 * t325 * t326 + 0.384D3 * t330 * t314
     # * t125 * z + 0.96D2 * t336 * t165 * t178 * t60 + 0.32D2 * t164 * 
     #t165 * t342 + 0.640D3 * t217 * t213 * t254 * t10 + 0.1728D4 * t42 
     #* t141 * t197 - 0.252D3 * t353 * t95 * t78 * t125 - 0.768D3 * t30 
     #* t358 * t314 * t360 + 0.384D3 * t50 * t358 * t314 * x3 * t365 - 0
     #.96D2 * t164 * t180 - 0.96D2 * t373 * t179 * t374 + 0.192D3 * t373
     # * t179 * t379 - 0.160D3 * t83 * t202 * t204 * t69 * t94 * t386 + 
     #0.640D3 * t92 * t103 * t391 * t276 - 0.208D3 * t83 * t39 * x2 * x1
     # * t43 * t165 - 0.28792D5 * t240 * t86 * t314 * t26 * t125 + 0.291
     #28D5 * t313 * t314 * t26 * x3 + 0.780D3 * t139 * t103 * t114 * t56
     # * t27
      t423 = t86 * t5
      t435 = x1 * t60
      t449 = t53 * t20 * t60
      t454 = t30 * t39 * t53
      t459 = t30 * t51
      t467 = t5 * t126
      t475 = t50 * t274 * t77
      t476 = t107 * t53
      t491 = t20 * t23
      t496 = t30 * z
      t497 = t1 * t23
      t505 = 0.640D3 * t29 * t31 * x2 * x1 * t23 * t5 * t27 + 0.536D3 * 
     #t235 * t423 * t45 * t10 - 0.28208D5 * t235 * t423 * t27 * t96 + 0.
     #27920D5 * t212 * t87 * t142 + 0.74296D5 * t33 * t165 * t435 - 0.12
     #8D3 * t171 * t268 * t270 * t78 * t60 + 0.48D2 * t336 * t165 * t166
     # * t125 + 0.74208D5 * t242 * t133 * t449 - 0.128D3 * t454 * t56 * 
     #t158 * t24 + 0.16D2 * t459 * t276 * t243 - 0.29128D5 * t212 * t87 
     #* t45 - 0.144D3 * t188 * x1 * t94 * t467 - 0.8D1 * t148 * t133 * t
     #219 - 0.192D3 * t475 * t476 * t292 * x3 - 0.780D3 * t177 * t165 * 
     #t41 + 0.28792D5 * t235 * t423 * t27 * t125 - 0.8D1 * t33 * t341 * 
     #t179 + 0.64D2 * t454 * t115 * t491 * x3 - 0.128D3 * t496 * t497 * 
     #t10 - 0.48D2 * t319 * t86 * t10 * t125
      t512 = t43 * t125
      t516 = t30 * t1
      t521 = t49 * t1 * t23
      t527 = t240 * t131
      t528 = t34 * t20
      t537 = t83 * t365 * t112
      t542 = t130 * t223
      t547 = t497 * x3
      t569 = t435 * x3
      t584 = 0.48D2 * t319 * t86 * t96 * x3 - 0.384D3 * t290 * t512 * z 
     #- 0.384D3 * t516 * t24 * t365 + 0.128D3 * t521 * x3 * t365 * z * t
     #26 - 0.536D3 * t527 * t249 * t528 + 0.28224D5 * t235 * t87 * t197 
     #* t10 - 0.64D2 * t537 * t114 * t276 * t20 + 0.424D3 * t542 * t249 
     #* t41 * x3 + 0.512D3 * t496 * t547 + 0.8D1 * t290 * t379 + 0.304D3
     # * t516 * t241 * t5 + 0.128D3 * t516 * t6 * t60 + 0.128D3 * t516 *
     # t21 - 0.64D2 * t290 * t291 * t158 - 0.64D2 * t290 * t291 * t61 - 
     #0.384D3 * t33 * t360 * t21 + 0.74288D5 * t527 * t249 * t569 + 0.64
     #0D3 * t542 * t249 * t41 * t10 + 0.128D3 * t172 * t108 * t78 * x3 -
     # 0.4D1 * t30 * t358 * t96
      t607 = t50 * t66 * t23
      t613 = t211 * t268
      t614 = t27 * t20
      t619 = t50 * t66 * t94
      t633 = t218 * t10
      t637 = t84 * t134
      t665 = 0.252D3 * t30 * t358 * t125 - 0.128D3 * t30 * t547 - 0.144D
     #3 * t50 * t127 + 0.74296D5 * t140 * t196 * t197 * t60 + 0.320D3 * 
     #t269 * t270 * t149 + 0.384D3 * t330 * t314 * t360 * t10 + 0.32D2 *
     # t459 * t276 * t449 + 0.96D2 * t607 * t386 * t69 * t60 * t158 + 0.
     #25296D5 * t613 * t196 * t614 + 0.128D3 * t619 * t314 * t96 * x3 - 
     #0.25264D5 * t140 * t141 * t614 * t10 - 0.672D3 * t319 * t223 * t26
     #0 - 0.256D3 * t68 * t41 * t94 * t249 * t633 - 0.640D3 * t637 * t19
     #6 * t149 + 0.640D3 * t217 * t213 * t150 - 0.512D3 * t130 * t41 * t
     #86 * t249 * t230 * x3 - 0.74288D5 * t140 * t141 * t326 * x3 + 0.8D
     #1 * t68 * t70 * t43 * t73 * t26 * t255 - 0.320D3 * t217 * t213 * t
     #183 + 0.128D3 * t459 * t63
      t671 = t53 * t61
      t679 = t50 * t335 * t77
      t748 = -0.128D3 * t454 * t62 * t23 * t10 + 0.128D3 * t459 * t276 *
     # t671 + 0.416D3 * t637 * t196 * t254 - 0.64D2 * t679 * t56 * x1 * 
     #t60 * t10 * x3 + 0.64D2 * t475 * t476 * t292 * t10 - 0.640D3 * t10
     #5 * t108 * t218 - 0.304D3 * t93 * t95 * t77 * t125 + 0.64D2 * t537
     # * t259 * t165 * x3 - 0.128D3 * t275 * t276 * t54 * t61 + 0.128D3 
     #* t607 * t386 * t69 * t61 * t20 - 0.640D3 * t324 * t325 * t614 + 0
     #.288D3 * t619 * t314 * t10 * t125 + 0.64D2 * t164 * t34 * t35 * t1
     #0 + 0.74320D5 * t613 * t196 * t326 - 0.74320D5 * t248 * t249 * t43
     #5 + 0.32D2 * t242 * t133 * t671 + 0.288D3 * t188 * t391 * t56 * t1
     #25 * t60 - 0.192D3 * t164 * t165 * t569 + 0.8D1 * t30 * t31 * x1 *
     # t179 * t24 + 0.576D3 * t113 * t69 * t23 * t107 * t61
      t791 = t10 * x3
      t826 = -0.64D2 * t454 * t115 * t491 * t10 + 0.25248D5 * t240 * t22
     #3 * t5 * t26 * t791 - 0.64D2 * t30 + 0.25264D5 * t527 * t249 * t16
     #7 - 0.32D2 * t373 * t35 * t374 + 0.16D2 * t607 * t386 * t69 * t158
     # * t20 - 0.288D3 * t373 * t35 * t512 - 0.64D2 * t679 * t56 * x3 * 
     #t167 - 0.416D3 * t30 * t120 * t314 * t791 + 0.28224D5 * t33 * t165
     # * t166 + 0.112D3 * t50 * t66 * t94 * t77 * t467
      rrgq2qgh11J7 = wd * (t195 + t311 + t415 + t505 + t584 + t665 + t74
     #8 - 0.780D3 * t42 * t44 * t142 + 0.64D2 * t353 * t95 * t78 * t96 +
     # 0.80D2 * t527 * t249 * t342 + 0.640D3 * t93 * t95 * t633 - 0.208D
     #3 * t253 * t74 * t77 * t158 + 0.780D3 * t132 * t133 * t134 * t20 -
     # 0.416D3 * t164 * t165 * t528 - 0.48D2 * t275 * t276 * t243 * x3 -
     # 0.80D2 * t284 * t285 * t614 * t60 - 0.304D3 * t521 * t314 * t26 +
     # t826) / t28 / t27 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
 