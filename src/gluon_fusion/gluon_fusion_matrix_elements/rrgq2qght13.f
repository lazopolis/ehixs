  
      subroutine rrgq2qght13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght13s1e1  
      doubleprecision rrgq2qght13s1e0  
      doubleprecision rrgq2qght13s1em1  
      doubleprecision rrgq2qght13s1em2  
      doubleprecision rrgq2qght13s1em3  
      doubleprecision rrgq2qght13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght13s1e1
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
      t4 = -0.1D1 + x2
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t17 = t16 * t4
      t20 = log(-0.4D1 * t7 * t10 * t17)
      t21 = t20 ** 2
      t23 = t14 * t1
      t24 = cos(t8)
      t25 = t24 ** 2
      t26 = t23 * t25
      t27 = t4 * x2
      t28 = Sqrt(-t27)
      t29 = t28 ** 2
      t30 = x2 * z
      t32 = (0.1D1 - x2 + t30) ** 2
      t33 = t32 ** 2
      t34 = 0.1D1 / t33
      t35 = t29 * t34
      t36 = t26 * t35
      t39 = 0.90D2 * t6
      t40 = 0.180D3 * lh
      t43 = (-t40 - 0.90D2) * z * wd
      t44 = t39 + t43
      t47 = t25 * t29
      t48 = t47 * t34
      t51 = lh ** 2
      t52 = 0.180D3 * t51
      t53 = 0.3141592653589793D1 ** 2
      t54 = 0.30D2 * t53
      t58 = t39 + t43 + (t40 + t52 - t54) * z * wd
      t63 = 0.1D1 / x3
      t76 = log(-0.4D1 * x2 * t10 * t17)
      t78 = (-0.1D1 - t76) * t23
      t84 = t47 * lh
      t87 = t76 ** 2
      t88 = t87 / 0.2D1
      t90 = (t76 + t88) * t23
      t93 = t52 - t54
      t122 = x1 ** 2
      t123 = x3 * t122
      t124 = t123 * x2
      t125 = t10 * t13
      t130 = log(-0.4D1 * t124 * t125 * t15 * t4)
      t139 = 0.1D1 / x1
      t143 = x2 * t122
      t147 = log(-0.4D1 * t143 * t10 * t17)
      t148 = t147 ** 2
      t153 = t26 * t6
      t154 = 0.90D2 * t153
      t156 = 0.180D3 * t26 * lh
      t160 = (-t156 - 0.90D2 * t26) * z * wd
      t161 = t154 + t160
      t169 = t154 + t160 + (t156 + t26 * t93) * z * wd
      t176 = (-0.720D3 * t6 * t21 * t36 + 0.16D2 * t44 * t20 * t23 * t48
     # - 0.16D2 * t58 * t23 * t48) * t63 / 0.360D3 - 0.2D1 / 0.45D2 * (0
     #.90D2 * t26 * t29 * z * wd + (-0.180D3 * t26 * t29 * lh + 0.90D2 *
     # t78 * t47) * z * wd + (-0.180D3 * t78 * t84 + 0.90D2 * t90 * t47 
     #+ t26 * t29 * t93) * z * wd + (-0.180D3 * t90 * t84 + t26 * t29 * 
     #(0.60D2 * lh * t53 - 0.2884936567583026D3 - 0.120D3 * t51 * lh) + 
     #0.90D2 * (-t88 - t87 * t76 / 0.6D1) * t23 * t47 + t78 * t47 * t93)
     # * z * wd) * t34 - (-0.5760D4 * t6 * t130 * t36 + 0.64D2 * t44 * t
     #23 * t48) * t63 * t139 / 0.720D3 + (-0.2880D4 * t26 * z * wd * t14
     #8 * t35 + 0.64D2 * t161 * t147 * t35 - 0.64D2 * t169 * t29 * t34) 
     #* t139 / 0.720D3
      t177 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #176)
      t179 = -0.1D1 + x1
      t180 = x3 * x1
      t181 = t180 * z
      t182 = 0.2D1 * t7
      t183 = t7 * x1
      t184 = x1 * z
      t185 = t7 * t184
      t186 = sqrt(x3)
      t187 = t24 * t186
      t188 = 0.1D1 - x1 + t184
      t189 = t4 * t188
      t190 = -0.1D1 + t186
      t192 = t186 + 0.1D1
      t195 = Sqrt(t189 * x2 * t190 * t192)
      t197 = 0.2D1 * t187 * t195
      t200 = 0.1D1 / t188
      t204 = x2 * x1
      t205 = t204 * z
      t206 = 0.1D1 - x1 + t184 - x2 + t204 - t205 - x3 + t180 - t181 + t
     #182 - t183 + t185 + t197
      t217 = s * t14 * x2 * x1 * t179 * t200
      t218 = t190 * t192
      t219 = x3 * t4
      t222 = t179 ** 2
      t223 = t222 * t200
      t225 = t10 * t122
      t226 = t225 * t15
      t230 = log(0.4D1 * t218 * t219 * x2 * t223 * t13 * t226)
      t231 = x3 * z
      t233 = x3 * t12
      t237 = t12 * x2
      t239 = t195 * z
      t242 = 0.1D1 + t30 + t204 + t231 + t180 - x1 + t182 + 0.4D1 * t185
     # - t233 * t204 - 0.2D1 * t123 * t30 + t123 * t237 - 0.2D1 * t187 *
     # t239
      t248 = 0.2D1 * t7 * z
      t251 = t195 * x1
      t254 = 0.2D1 * t187 * t239 * x1 - t205 - 0.2D1 * t181 - t248 - 0.3
     #D1 * t183 + t233 * x1 + t124 + t184 - x2 - x3 + t197 - 0.2D1 * t18
     #7 * t251
      t256 = (t242 + t254) ** 2
      t257 = 0.1D1 / t256
      t260 = t24 * t195
      t263 = t24 * x3
      t266 = t186 * t12
      t271 = t186 * x3
      t272 = x1 * t271
      t275 = t186 * x1
      t278 = t186 * x2
      t281 = t186 * t122
      t290 = t271 * x2
      t293 = 0.5D1 * t278 * z
      t295 = 0.2D1 * t290 * z
      t296 = 0.2D1 * t290
      t297 = 0.5D1 * t278
      t299 = 0.2D1 * t186 * z
      t300 = 0.4D1 * t260 * z + 0.2D1 * t263 * t195 - 0.2D1 * t266 * x1 
     #+ 0.4D1 * t260 * x1 - 0.3D1 * t272 * x2 + 0.6D1 * t275 * z + 0.8D1
     # * t278 * x1 - 0.2D1 * t281 * z - 0.3D1 * t281 * x2 + t281 * t12 -
     # 0.2D1 * t272 * z + t272 * t12 + t290 * t122 + t293 - t295 + t296 
     #- t297 - t299
      t302 = t271 * z
      t314 = z * t24
      t315 = x3 * t195
      t331 = 0.3D1 * t186
      t332 = -0.4D1 * t275 + t281 + t302 + t272 - 0.4D1 * t260 + 0.6D1 *
     # t281 * t30 - 0.3D1 * t281 * t237 - 0.2D1 * t122 * t271 * t30 - t2
     #72 * t237 + t290 * t122 * t12 + 0.2D1 * t314 * t315 * x1 - t271 - 
     #0.4D1 * t260 * t184 - 0.2D1 * t263 * t251 + 0.4D1 * t290 * t184 - 
     #0.2D1 * t314 * t315 - 0.11D2 * t278 * t184 + 0.3D1 * t266 * t204 +
     # t331
      t334 = (t300 + t332) ** 2
      t337 = (-0.1D1 + x1 - t184 + x2 - t204 - t30 + t205) ** 2
      t338 = 0.1D1 / t337
      t351 = 0.360D3 * t6 * t230 * t257 * t334 * t179 * t188 * t338 * t1
     # - 0.4D1 * t44 * t257 * t334 * t179 * t188 * t338 * t1
      t355 = FJET(XB1, XB2, s, t2 * t179 * (-x3 + t180 - t181 + t182 - t
     #183 + t185 - x2 + t197) * t200, t2 * t180, -t2 * t179 * t206 * t20
     #0, -t2 * x1 * t190 * t192, -t217, -t351 * t63 * t139 / 0.720D3)
      t361 = Sqrt(t27 * t218)
      t363 = 0.2D1 * t187 * t361
      t368 = t218 * t219
      t369 = x2 * t13
      t374 = log(0.4D1 * t368 * t369 * t10 * t15)
      t375 = t374 ** 2
      t380 = t24 * t361
      t386 = 0.2D1 * t314 * x3 * t361 - t331 + t271 - t296 + t297 + 0.4D
     #1 * t380 + t299 - t302 - t293 + t295 - 0.4D1 * t380 * z - 0.2D1 * 
     #t263 * t361
      t387 = t386 ** 2
      t388 = 0.1D1 / t32
      t394 = (x2 - 0.1D1 + t248 - t363 + 0.2D1 * t187 * t361 * z - t182 
     #- t30 - t231 + x3) ** 2
      t395 = 0.1D1 / t394
      t397 = t387 * t388 * t395 * t1
      t403 = t388 * t395 * t1
      t413 = log(0.4D1 * t368 * t369 * t226)
      t424 = (0.45D2 * t6 * t375 * t397 - t44 * t374 * t387 * t403 + t58
     # * t387 * t403) * t63 / 0.360D3 - (0.360D3 * t6 * t413 * t397 - 0.
     #4D1 * t44 * t387 * t403) * t63 * t139 / 0.720D3
      t425 = FJET(XB1, XB2, s, -t2 * (-x3 + t182 - x2 + t363), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t182 + t363), 0.0D0, 0.0D0, t424)
      t439 = log(-0.4D1 * t7 * t225 * t16 * t223 * t4)
      t442 = t222 * t179
      t445 = Sqrt(-t189 * x2)
      t446 = t445 ** 2
      t447 = t337 ** 2
      t448 = 0.1D1 / t447
      t456 = t442 * t446
      t470 = log(-0.4D1 * t143 * t125 * t15 * t200 * t222 * t4)
      t471 = t470 ** 2
      t473 = t456 * t448
      t487 = -(-0.5760D4 * t6 * t439 * t25 * t188 * t442 * t446 * t448 *
     # t23 + 0.64D2 * t44 * t25 * t188 * t456 * t448 * t23) * t63 * t139
     # / 0.720D3 + (-0.2880D4 * t153 * t471 * t188 * t473 + 0.64D2 * t16
     #1 * t470 * t188 * t473 - 0.64D2 * t169 * t188 * t473) * t139 / 0.7
     #20D3
      t488 = FJET(XB1, XB2, s, -t2 * t179 * x2 * t200, 0.0D0, t4 * s * t
     #1 * t179, t2 * x1, -t217, t487)
      rrgq2qght13s1e1 = t177 * t176 - t355 * t351 * t63 * t139 / 0.720D3
     # + t425 * t424 + t488 * t487

      end function



      doubleprecision function rrgq2qght13s1e0
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
      t4 = -0.1D1 + x2
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t17 = t13 * t15 * t4
      t20 = log(-0.4D1 * t7 * t10 * t17)
      t22 = t14 * t1
      t23 = cos(t8)
      t24 = t23 ** 2
      t25 = t22 * t24
      t26 = t4 * x2
      t27 = Sqrt(-t26)
      t28 = t27 ** 2
      t29 = x2 * z
      t31 = (0.1D1 - x2 + t29) ** 2
      t32 = t31 ** 2
      t33 = 0.1D1 / t32
      t34 = t28 * t33
      t43 = 0.90D2 * t6 + (-0.180D3 * lh - 0.90D2) * z * wd
      t45 = t24 * t28
      t50 = 0.1D1 / x3
      t53 = t25 * t6
      t54 = 0.1D1 / x1
      t55 = t50 * t54
      t60 = x1 ** 2
      t61 = x2 * t60
      t65 = log(-0.4D1 * t61 * t10 * t17)
      t77 = 0.90D2 * t53 + (-0.180D3 * t25 * lh - 0.90D2 * t25) * z * wd
      t94 = log(-0.4D1 * x2 * t10 * t17)
      t96 = (-0.1D1 - t94) * t22
      t105 = t94 ** 2
      t111 = lh ** 2
      t113 = 0.3141592653589793D1 ** 2
      t124 = (0.1440D4 * t6 * t20 * t25 * t34 - 0.16D2 * t43 * t22 * t45
     # * t33) * t50 / 0.360D3 - 0.8D1 * t53 * t34 * t55 + (0.5760D4 * t2
     #5 * z * wd * t65 * t34 - 0.64D2 * t77 * t28 * t33) * t54 / 0.720D3
     # - 0.2D1 / 0.45D2 * (0.90D2 * t25 * t28 * z * wd + (-0.180D3 * t25
     # * t28 * lh + 0.90D2 * t96 * t45) * z * wd + (-0.180D3 * t96 * t45
     # * lh + 0.90D2 * (t94 + t105 / 0.2D1) * t22 * t45 + t25 * t28 * (0
     #.180D3 * t111 - 0.30D2 * t113)) * z * wd) * t33
      t125 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #124)
      t127 = -0.1D1 + x1
      t128 = x3 * x1
      t129 = t128 * z
      t130 = 0.2D1 * t7
      t131 = t7 * x1
      t132 = x1 * z
      t133 = t7 * t132
      t134 = sqrt(x3)
      t135 = t23 * t134
      t136 = 0.1D1 - x1 + t132
      t137 = t4 * t136
      t138 = -0.1D1 + t134
      t140 = t134 + 0.1D1
      t143 = Sqrt(t137 * x2 * t138 * t140)
      t145 = 0.2D1 * t135 * t143
      t148 = 0.1D1 / t136
      t152 = x2 * x1
      t153 = t152 * z
      t154 = 0.1D1 - x1 + t132 - x2 + t152 - t153 - x3 + t128 - t129 + t
     #130 - t131 + t133 + t145
      t165 = s * t14 * x2 * x1 * t127 * t148
      t166 = x3 * z
      t168 = x3 * t12
      t170 = x3 * t60
      t173 = t12 * x2
      t175 = t143 * z
      t178 = 0.1D1 + t29 + t152 + t166 + t128 - x1 + t130 + 0.4D1 * t133
     # - t168 * t152 - 0.2D1 * t170 * t29 + t170 * t173 - 0.2D1 * t135 *
     # t175
      t184 = 0.2D1 * t7 * z
      t188 = t143 * x1
      t191 = 0.2D1 * t135 * t175 * x1 - t153 - 0.2D1 * t129 - t184 - 0.3
     #D1 * t131 + t168 * x1 + t170 * x2 + t132 - x2 - x3 + t145 - 0.2D1 
     #* t135 * t188
      t193 = (t178 + t191) ** 2
      t194 = 0.1D1 / t193
      t195 = t23 * t143
      t198 = t23 * x3
      t201 = t134 * t12
      t206 = t134 * x3
      t207 = x1 * t206
      t210 = t134 * x1
      t213 = t134 * x2
      t216 = t134 * t60
      t225 = t206 * x2
      t228 = 0.5D1 * t213 * z
      t230 = 0.2D1 * t225 * z
      t231 = z * t23
      t232 = x3 * t143
      t240 = 0.4D1 * t195 * z + 0.2D1 * t198 * t143 - 0.2D1 * t201 * x1 
     #+ 0.4D1 * t195 * x1 - 0.3D1 * t207 * x2 + 0.6D1 * t210 * z + 0.8D1
     # * t213 * x1 - 0.2D1 * t216 * z - 0.3D1 * t216 * x2 + t216 * t12 -
     # 0.2D1 * t207 * z + t207 * t12 + t225 * t60 + t228 - t230 + 0.2D1 
     #* t231 * t232 * x1 - 0.4D1 * t195 * t132 - 0.2D1 * t198 * t188
      t259 = 0.3D1 * t134
      t260 = 0.2D1 * t225
      t261 = 0.5D1 * t213
      t263 = 0.2D1 * t134 * z
      t265 = t206 * z
      t267 = 0.4D1 * t225 * t132 - 0.2D1 * t231 * t232 - 0.11D2 * t213 *
     # t132 + 0.3D1 * t201 * t152 + 0.6D1 * t216 * t29 - 0.3D1 * t216 * 
     #t173 - 0.2D1 * t60 * t206 * t29 - t207 * t173 + t225 * t60 * t12 +
     # t259 + t260 - t261 - t263 - 0.4D1 * t210 + t216 + t265 + t207 - 0
     #.4D1 * t195 - t206
      t269 = (t240 + t267) ** 2
      t274 = (-0.1D1 + x1 - t132 + x2 - t152 - t29 + t153) ** 2
      t275 = 0.1D1 / t274
      t278 = t1 * t50 * t54
      t282 = FJET(XB1, XB2, s, t2 * t127 * (-x3 + t128 - t129 + t130 - t
     #131 + t133 - x2 + t145) * t148, t2 * t128, -t2 * t127 * t154 * t14
     #8, -t2 * x1 * t138 * t140, -t165, t6 * t194 * t269 * t127 * t136 *
     # t275 * t278 / 0.2D1)
      t292 = t138 * t140
      t294 = Sqrt(t26 * t292)
      t296 = 0.2D1 * t135 * t294
      t308 = log(0.4D1 * t292 * x3 * t4 * x2 * t13 * t10 * t15)
      t313 = t23 * t294
      t319 = 0.2D1 * t231 * x3 * t294 - t259 + t206 - t260 + t261 + 0.4D
     #1 * t313 + t263 - t265 - t228 + t230 - 0.4D1 * t313 * z - 0.2D1 * 
     #t198 * t294
      t320 = t319 ** 2
      t321 = 0.1D1 / t31
      t322 = t320 * t321
      t327 = (x2 - 0.1D1 + t184 - t296 + 0.2D1 * t135 * t294 * z - t130 
     #- t29 - t166 + x3) ** 2
      t328 = 0.1D1 / t327
      t329 = t328 * t1
      t344 = (-0.90D2 * t6 * t308 * t322 * t329 + t43 * t320 * t321 * t3
     #28 * t1) * t50 / 0.360D3 + t6 * t322 * t329 * t55 / 0.2D1
      t345 = FJET(XB1, XB2, s, -t2 * (-x3 + t130 - x2 + t296), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t130 + t296), 0.0D0, 0.0D0, t344)
      t355 = t127 ** 2
      t356 = t355 * t127
      t360 = Sqrt(-t137 * x2)
      t361 = t360 ** 2
      t362 = t274 ** 2
      t363 = 0.1D1 / t362
      t377 = log(-0.4D1 * t61 * t10 * t13 * t15 * t148 * t355 * t4)
      t380 = t356 * t361 * t363
      t390 = -0.8D1 * t6 * t24 * t136 * t356 * t361 * t363 * t22 * t50 *
     # t54 + (0.5760D4 * t53 * t377 * t136 * t380 - 0.64D2 * t77 * t136 
     #* t380) * t54 / 0.720D3
      t391 = FJET(XB1, XB2, s, -t2 * t127 * x2 * t148, 0.0D0, t4 * s * t
     #1 * t127, t2 * x1, -t165, t390)
      rrgq2qght13s1e0 = t125 * t124 + t282 * z * wd * t194 * t269 * t127
     # * t136 * t275 * t278 / 0.2D1 + t345 * t344 + t391 * t390

      end function



      doubleprecision function rrgq2qght13s1em1
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
      t4 = -0.1D1 + x2
      t6 = z * wd
      t7 = t1 ** 2
      t8 = t7 * t1
      t9 = t6 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t12 = t11 ** 2
      t13 = t4 * x2
      t14 = Sqrt(-t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = x2 * z
      t19 = (0.1D1 - x2 + t17) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 / x1
      t27 = t8 * t12
      t35 = Sin(t10)
      t36 = t35 ** 2
      t38 = z ** 2
      t40 = t7 ** 2
      t45 = log(-0.4D1 * x2 * t36 / t38 * t40 * t4)
      t56 = 0.1D1 / x3
      t61 = -0.8D1 * t9 * t16 * t21 * t22 - 0.2D1 / 0.45D2 * (0.90D2 * t
     #27 * t15 * z * wd + (-0.180D3 * t27 * t15 * lh + 0.90D2 * (-0.1D1 
     #- t45) * t8 * t16) * z * wd) * t21 - 0.4D1 * t9 * t16 * t21 * t56
      t62 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t6
     #1)
      t64 = -0.1D1 + x1
      t66 = x1 * z
      t67 = 0.1D1 - x1 + t66
      t68 = 0.1D1 / t67
      t82 = t64 ** 2
      t86 = Sqrt(-t4 * t67 * x2)
      t87 = t86 ** 2
      t89 = x2 * x1
      t92 = (-0.1D1 + x1 - t66 + x2 - t89 - t17 + t89 * z) ** 2
      t93 = t92 ** 2
      t97 = t82 * t64 * t87 / t93 * t8 * t22
      t100 = FJET(XB1, XB2, s, -t2 * t64 * x2 * t68, 0.0D0, t4 * s * t1 
     #* t64, t2 * x1, -s * t7 * x2 * x1 * t64 * t68, -0.8D1 * t6 * t12 *
     # t67 * t97)
      t107 = x2 * x3
      t108 = 0.2D1 * t107
      t109 = sqrt(x3)
      t110 = t11 * t109
      t115 = Sqrt(t13 * (-0.1D1 + t109) * (t109 + 0.1D1))
      t117 = 0.2D1 * t110 * t115
      t127 = t109 * x3
      t128 = t127 * x2
      t130 = t109 * x2
      t132 = t11 * t115
      t146 = 0.2D1 * z * t11 * x3 * t115 - 0.3D1 * t109 + t127 - 0.2D1 *
     # t128 + 0.5D1 * t130 + 0.4D1 * t132 + 0.2D1 * t109 * z - t127 * z 
     #- 0.5D1 * t130 * z + 0.2D1 * t128 * z - 0.4D1 * t132 * z - 0.2D1 *
     # t11 * x3 * t115
      t147 = t146 ** 2
      t157 = (x2 - 0.1D1 + 0.2D1 * t107 * z - t117 + 0.2D1 * t110 * t115
     # * z - t108 - t17 - x3 * z + x3) ** 2
      t161 = 0.1D1 / t19 / t157 * t1 * t56
      t164 = FJET(XB1, XB2, s, -t2 * (-x3 + t108 - x2 + t117), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t108 + t117), 0.0D0, 0.0D0, t6 * t147 * t161
     # / 0.4D1)
      rrgq2qght13s1em1 = t62 * t61 - 0.8D1 * t100 * z * wd * t12 * t67 *
     # t97 + t164 * z * wd * t147 * t161 / 0.4D1

      end function



      doubleprecision function rrgq2qght13s1em2
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
      t4 = -0.1D1 + x2
      t7 = t1 ** 2
      t8 = t7 * t1
      t11 = cos(x4 * 0.3141592653589793D1)
      t12 = t11 ** 2
      t14 = Sqrt(-t4 * x2)
      t15 = t14 ** 2
      t19 = (0.1D1 - x2 + x2 * z) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t25 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.4D1 * z * wd * t8 * t12 * t15 * t21)
      rrgq2qght13s1em2 = -0.4D1 * t25 * z * wd * t8 * t12 * t15 * t21

      end function



      doubleprecision function rrgq2qght13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght13s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght13s1em4 = 0.0D0

      end function
