  
      subroutine rrgq2qght14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght14s1e1  
      doubleprecision rrgq2qght14s1e0  
      doubleprecision rrgq2qght14s1em1  
      doubleprecision rrgq2qght14s1em2  
      doubleprecision rrgq2qght14s1em3  
      doubleprecision rrgq2qght14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght14s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x2 * 0.3141592653589793D1
      t16 = cos(t15)
      t17 = t16 ** 2
      t18 = t17 * wd
      t19 = t11 * t3
      t20 = x1 * t19
      t21 = t18 * t20
      t22 = 0.1D1 / z
      t23 = t22 * t6
      t24 = sin(t15)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t36 = t33 * x4 * t34
      t39 = log(0.4D1 * t30 * t36)
      t40 = t39 ** 2
      t42 = 0.1D1 / (-0.2D1 + t1)
      t49 = t18 * t20 * lh
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t64 = t21 * t59 * t22 * t6 * t42 * t34
      t66 = 0.1D1 / x4
      t80 = t18 * t42 * t34
      t84 = log(0.4D1 * t30 * t33 * t34)
      t88 = wd * t42 * t34
      t89 = (-0.1D1 - t84) * t17 * t88
      t94 = t84 ** 2
      t95 = t94 / 0.2D1
      t98 = (t84 + t95) * t17 * t88
      t101 = t19 * lh
      t117 = wd * x1
      t118 = t19 * t22
      t119 = t117 * t118
      t120 = x3 * t25
      t125 = log(0.4D1 * t120 * t27 * t29 * t36)
      t128 = t17 * t42 * t34
      t132 = t117 * t101
      t137 = 0.1D1 / x3
      t142 = t29 * t31
      t147 = log(0.4D1 * t120 * t27 * t142 * t32 * t34)
      t148 = t147 ** 2
      t162 = 0.4D1 / 0.45D2 * (-0.45D2 * t21 * t23 * t40 * t42 * t34 - 0
     #.180D3 * t49 * t23 * t39 * t42 * t34 - t64) * t66 - 0.4D1 / 0.45D2
     # * (t18 * t42 * t34 * x1 * t19 * (0.60D2 * lh * t57 - 0.2884936567
     #583026D3 - 0.120D3 * t55 * lh) + (t80 + t89) * x1 * t19 * t59 - 0.
     #180D3 * (t80 + t89 + t98) * x1 * t101 + 0.90D2 * (t80 + t89 + t98 
     #+ (-t95 - t94 * t84 / 0.6D1) * t17 * t88) * x1 * t19) * t22 * t6 -
     # (-0.360D3 * t119 * t6 * t125 * t128 - 0.720D3 * t132 * t23 * t128
     #) * t137 * t66 / 0.45D2 + 0.4D1 / 0.45D2 * (-0.45D2 * t21 * t23 * 
     #t148 * t42 * t34 - 0.180D3 * t49 * t23 * t147 * t42 * t34 - t64) *
     # t137
      t163 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t162)
      t165 = sqrt(x4)
      t166 = -0.1D1 + t165
      t167 = t165 + 0.1D1
      t168 = t166 * t167
      t169 = KAPPA2(x1, x2, 0.0D0, -t168, z)
      t170 = s * t169
      t172 = t7 * x4
      t176 = t6 * t166 * t167
      t178 = t169 ** 2
      t181 = x1 * t6
      t186 = t18 * t20 * t22
      t187 = t178 ** 2
      t190 = t28 * t32
      t191 = t168 * t190
      t194 = log(-0.4D1 * t187 * x4 * t142 * t191)
      t195 = t194 ** 2
      t197 = Sqrt(-t168)
      t198 = t197 ** 2
      t200 = 0.1D1 / (-0.2D1 + t169)
      t202 = t198 * t200 * t187
      t211 = t18 * t20 * t59
      t218 = t117 * t118 * t6
      t225 = log(-0.4D1 * x3 * t187 * x4 * t31 * t29 * t191)
      t228 = t17 * t200 * t187
      t233 = t117 * t101 * t22
      t242 = 0.4D1 / 0.45D2 * (0.45D2 * t186 * t6 * t195 * t202 + 0.180D
     #3 * t49 * t23 * t194 * t202 + t211 * t23 * t202) * t66 - (0.360D3 
     #* t218 * t225 * t198 * t228 + 0.720D3 * t233 * t6 * t198 * t228) *
     # t137 * t66 / 0.45D2
      t243 = FJET(XB1, XB2, s, t170 * t4, 0.0D0, -t170 * t172, t170 * t3
     # * t176, s * t178 * t11 * t181 * (-0.1D1 + x4), t242)
      t245 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t246 = s * t245
      t248 = sqrt(x3)
      t249 = -0.1D1 + t248
      t251 = t248 + 0.1D1
      t252 = x1 * t249 * t251
      t254 = t4 * x3
      t257 = t245 ** 2
      t263 = x3 * x4
      t264 = t257 ** 2
      t268 = t142 * t190
      t271 = log(-0.4D1 * t263 * t264 * t249 * t251 * t268)
      t273 = t249 * t251
      t274 = Sqrt(-t273)
      t275 = t274 ** 2
      t279 = t275 / (-0.2D1 + t245) * t264
      t295 = log(-0.4D1 * x3 * t264 * t273 * t268)
      t296 = t295 ** 2
      t310 = -(0.360D3 * t218 * t271 * t17 * t279 + 0.720D3 * t233 * t6 
     #* t17 * t279) * t137 * t66 / 0.45D2 + 0.4D1 / 0.45D2 * (0.45D2 * t
     #186 * t6 * t296 * t279 + 0.180D3 * t49 * t23 * t295 * t279 + t211 
     #* t23 * t279) * t137
      t311 = FJET(XB1, XB2, s, -t246 * t3 * t252, t246 * t254, 0.0D0, -t
     #246 * t7, s * t257 * t11 * t181 * (-0.1D1 + x3), t310)
      t313 = KAPPA2(x1, x2, x3, -t168, z)
      t314 = s * t313
      t315 = t314 * t3
      t320 = t313 ** 2
      t326 = Sqrt(t273 * t168)
      t337 = t320 ** 2
      t342 = log(0.4D1 * t263 * t249 * t251 * t31 * t29 * t168 * t337 * 
     #t190)
      t348 = (-t248 * t165 + 0.2D1 * t16 * t326) ** 2
      t352 = t348 / (-0.2D1 + t313) * t337
      t359 = -0.90D2 * t119 * t6 * t342 * t352 - 0.180D3 * t132 * t23 * 
     #t352
      t363 = FJET(XB1, XB2, s, -t315 * t252, t314 * t254, -t314 * t172, 
     #t315 * t176, s * t320 * t11 * t181 * (x3 - 0.1D1 + x4 - 0.2D1 * t2
     #63 + 0.2D1 * t16 * t248 * t165 * t326), -t359 * t137 * t66 / 0.45D
     #2)
      rrgq2qght14s1e1 = t163 * t162 + t243 * t242 + t311 * t310 - t363 *
     # t359 * t137 * t66 / 0.45D2

      end function



      doubleprecision function rrgq2qght14s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x2 * 0.3141592653589793D1
      t16 = cos(t15)
      t17 = t16 ** 2
      t18 = t17 * wd
      t19 = t11 * t3
      t20 = x1 * t19
      t21 = t18 * t20
      t22 = 0.1D1 / z
      t23 = t22 * t6
      t24 = sin(t15)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t41 = 0.1D1 / (-0.2D1 + t1)
      t47 = wd * x1
      t48 = t19 * lh
      t50 = t17 * t41
      t54 = 0.180D3 * t47 * t48 * t23 * t50 * t34
      t56 = 0.1D1 / x4
      t61 = t47 * t19 * t22 * t6
      t62 = 0.1D1 / x3
      t70 = t29 * t31
      t75 = log(0.4D1 * x3 * t25 * t27 * t70 * t32 * t34)
      t86 = lh ** 2
      t88 = 0.3141592653589793D1 ** 2
      t95 = t18 * t41 * t34
      t99 = log(0.4D1 * t30 * t33 * t34)
      t103 = wd * t41 * t34
      t104 = (-0.1D1 - t99) * t17 * t103
      t109 = t99 ** 2
      t122 = 0.4D1 / 0.45D2 * (0.90D2 * t21 * t23 * t39 * t41 * t34 + t5
     #4) * t56 - 0.8D1 * t61 * t50 * t34 * t62 * t56 + 0.4D1 / 0.45D2 * 
     #(0.90D2 * t21 * t23 * t75 * t41 * t34 + t54) * t62 - 0.4D1 / 0.45D
     #2 * (t18 * t41 * t34 * x1 * t19 * (0.180D3 * t86 - 0.30D2 * t88) -
     # 0.180D3 * (t95 + t104) * x1 * t48 + 0.90D2 * (t95 + t104 + (t99 +
     # t109 / 0.2D1) * t17 * t103) * x1 * t19) * t22 * t6
      t123 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t122)
      t125 = sqrt(x4)
      t126 = -0.1D1 + t125
      t127 = t125 + 0.1D1
      t128 = t126 * t127
      t129 = KAPPA2(x1, x2, 0.0D0, -t128, z)
      t130 = s * t129
      t132 = t7 * x4
      t136 = t6 * t126 * t127
      t138 = t129 ** 2
      t141 = x1 * t6
      t145 = t20 * t22
      t146 = t18 * t145
      t147 = t138 ** 2
      t150 = t28 * t32
      t154 = log(-0.4D1 * t147 * x4 * t70 * t128 * t150)
      t156 = Sqrt(-t128)
      t157 = t156 ** 2
      t159 = 0.1D1 / (-0.2D1 + t129)
      t166 = t47 * t48 * t22
      t183 = 0.4D1 / 0.45D2 * (-0.90D2 * t146 * t6 * t154 * t157 * t159 
     #* t147 - 0.180D3 * t166 * t6 * t157 * t17 * t159 * t147) * t56 + 0
     #.8D1 * t61 * t157 * t17 * t159 * t147 * t62 * t56
      t184 = FJET(XB1, XB2, s, t130 * t4, 0.0D0, -t130 * t132, t130 * t3
     # * t136, s * t138 * t11 * t141 * (-0.1D1 + x4), t183)
      t186 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t187 = s * t186
      t189 = sqrt(x3)
      t190 = -0.1D1 + t189
      t192 = t189 + 0.1D1
      t193 = x1 * t190 * t192
      t195 = t4 * x3
      t198 = t186 ** 2
      t204 = t190 * t192
      t205 = Sqrt(-t204)
      t206 = t205 ** 2
      t209 = 0.1D1 / (-0.2D1 + t186)
      t211 = t198 ** 2
      t222 = log(-0.4D1 * x3 * t211 * t204 * t70 * t150)
      t225 = t206 * t209 * t211
      t236 = 0.8D1 * t61 * t17 * t206 * t209 * t211 * t62 * t56 + 0.4D1 
     #/ 0.45D2 * (-0.90D2 * t146 * t6 * t222 * t225 - 0.180D3 * t166 * t
     #6 * t17 * t225) * t62
      t237 = FJET(XB1, XB2, s, -t187 * t3 * t193, t187 * t195, 0.0D0, -t
     #187 * t7, s * t198 * t11 * t141 * (-0.1D1 + x3), t236)
      t239 = KAPPA2(x1, x2, x3, -t128, z)
      t240 = s * t239
      t241 = t240 * t3
      t246 = t239 ** 2
      t253 = Sqrt(t204 * t128)
      t264 = (-t189 * t125 + 0.2D1 * t16 * t253) ** 2
      t266 = 0.1D1 / (-0.2D1 + t239)
      t268 = t246 ** 2
      t270 = t268 * t62 * t56
      t274 = FJET(XB1, XB2, s, -t241 * t193, t240 * t195, -t240 * t132, 
     #t241 * t136, s * t246 * t11 * t141 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t16 * t189 * t125 * t253), -0.2D1 * t61 * t264 * t
     #266 * t270)
      rrgq2qght14s1e0 = t123 * t122 + t184 * t183 + t237 * t236 - 0.2D1 
     #* t274 * wd * t145 * t6 * t264 * t266 * t270

      end function



      doubleprecision function rrgq2qght14s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = wd * x1
      t16 = t11 * t3
      t17 = 0.1D1 / z
      t19 = t15 * t16 * t17
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = t22 * t6
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = t9 ** 2
      t27 = t25 * t26
      t28 = 0.1D1 / x3
      t33 = t22 * wd
      t41 = sin(t20)
      t42 = t41 ** 2
      t43 = z ** 2
      t46 = t11 ** 2
      t48 = x1 ** 2
      t49 = t6 ** 2
      t54 = log(0.4D1 * t42 / t43 * t46 * t48 * t49 * t26)
      t68 = 0.1D1 / x4
      t73 = -0.8D1 * t19 * t23 * t27 * t28 - 0.4D1 / 0.45D2 * (-0.180D3 
     #* t33 * t25 * t26 * x1 * t16 * lh + 0.90D2 * (t33 * t27 + (-0.1D1 
     #- t54) * t22 * wd * t25 * t26) * x1 * t16) * t17 * t6 - 0.8D1 * t1
     #9 * t23 * t27 * t68
      t74 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 *
     # t11 * x1 * t6, t73)
      t76 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t77 = s * t76
      t79 = sqrt(x3)
      t80 = -0.1D1 + t79
      t82 = t79 + 0.1D1
      t88 = t76 ** 2
      t91 = x1 * t6
      t97 = t33 * x1 * t16 * t17
      t99 = Sqrt(-t80 * t82)
      t100 = t99 ** 2
      t104 = t88 ** 2
      t106 = 0.1D1 / (-0.2D1 + t76) * t104 * t28
      t110 = FJET(XB1, XB2, s, -t77 * t3 * x1 * t80 * t82, t77 * t4 * x3
     #, 0.0D0, -t77 * t7, s * t88 * t11 * t91 * (-0.1D1 + x3), 0.8D1 * t
     #97 * t6 * t100 * t106)
      t112 = t15 * t16
      t114 = t17 * t6
      t119 = sqrt(x4)
      t120 = -0.1D1 + t119
      t121 = t119 + 0.1D1
      t122 = t120 * t121
      t123 = KAPPA2(x1, x2, 0.0D0, -t122, z)
      t124 = s * t123
      t132 = t123 ** 2
      t138 = Sqrt(-t122)
      t139 = t138 ** 2
      t143 = t132 ** 2
      t145 = 0.1D1 / (-0.2D1 + t123) * t143 * t68
      t149 = FJET(XB1, XB2, s, t124 * t4, 0.0D0, -t124 * t7 * x4, t124 *
     # t3 * t6 * t120 * t121, s * t132 * t11 * t91 * (-0.1D1 + x4), 0.8D
     #1 * t97 * t6 * t139 * t145)
      rrgq2qght14s1em1 = t74 * t73 + 0.8D1 * t110 * t22 * t112 * t114 * 
     #t100 * t106 + 0.8D1 * t149 * t22 * t112 * t114 * t139 * t145

      end function



      doubleprecision function rrgq2qght14s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = t11 * t3
      t17 = 0.1D1 / z
      t21 = cos(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = t9 ** 2
      t31 = FJET(XB1, XB2, s, t2 * t3 * x1, 0.0D0, 0.0D0, -t2 * t3 * t6,
     # -s * t9 * t11 * x1 * t6, -0.8D1 * wd * x1 * t16 * t17 * t6 * t22 
     #* t25 * t26)
      rrgq2qght14s1em2 = -0.8D1 * t31 * wd * x1 * t16 * t17 * t6 * t22 *
     # t25 * t26

      end function



      doubleprecision function rrgq2qght14s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght14s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght14s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght14s1em4 = 0.0D0

      end function
