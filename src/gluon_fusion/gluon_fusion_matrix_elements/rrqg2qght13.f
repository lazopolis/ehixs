  
      subroutine rrqg2qght13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght13s1e1  
      doubleprecision rrqg2qght13s1e0  
      doubleprecision rrqg2qght13s1em1  
      doubleprecision rrqg2qght13s1em2  
      doubleprecision rrqg2qght13s1em3  
      doubleprecision rrqg2qght13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght13s1e1
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
      t23 = cos(t8)
      t24 = t23 ** 2
      t25 = t4 * x2
      t26 = Sqrt(-t25)
      t27 = t26 ** 2
      t29 = t14 * t1
      t30 = x2 * z
      t32 = (0.1D1 + t30 - x2) ** 2
      t33 = t32 ** 2
      t34 = 0.1D1 / t33
      t36 = t24 * t27 * t29 * t34
      t43 = lh ** 2
      t45 = 0.3141592653589793D1 ** 2
      t47 = 0.180D3 * t43 - 0.30D2 * t45
      t48 = t6 * t47
      t50 = 0.64D2 * t48 * t36
      t52 = 0.1D1 / x3
      t58 = log(-0.4D1 * x2 * t10 * t17)
      t59 = t58 ** 2
      t83 = x1 ** 2
      t84 = x3 * t83
      t85 = t84 * x2
      t86 = t10 * t13
      t87 = t15 * t4
      t91 = log(-0.4D1 * t85 * t86 * t87)
      t95 = t6 * lh
      t100 = 0.1D1 / x1
      t104 = x2 * t83
      t108 = log(-0.4D1 * t104 * t10 * t17)
      t109 = t108 ** 2
      t111 = t27 * t34
      t115 = lh * t24
      t124 = -(0.2880D4 * t6 * t21 * t36 + 0.11520D5 * t6 * lh * t20 * t
     #36 + t50) * t52 / 0.1440D4 - 0.2D1 / 0.45D2 * (-0.90D2 * t59 * z *
     # wd * lh + t6 * (0.60D2 * lh * t45 - 0.2884936567583026D3 - 0.120D
     #3 * t43 * lh) - 0.15D2 * t59 * t58 * z * wd - t58 * z * wd * t47) 
     #* t24 * t27 * t29 * t34 - (-0.5760D4 * t6 * t91 * t36 - 0.11520D5 
     #* t95 * t36) * t52 * t100 / 0.720D3 + (-0.2880D4 * t6 * t24 * t29 
     #* t109 * t111 - 0.11520D5 * t6 * t115 * t29 * t108 * t111 - t50) *
     # t100 / 0.720D3
      t125 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t
     #124)
      t127 = 0.2D1 * t7
      t128 = sqrt(x3)
      t129 = t23 * t128
      t130 = -0.1D1 + t128
      t131 = t128 + 0.1D1
      t134 = Sqrt(t25 * t130 * t131)
      t136 = 0.2D1 * t129 * t134
      t141 = t13 * t130
      t142 = t131 * x2
      t143 = t141 * t142
      t144 = t4 * x3
      t149 = log(0.4D1 * t143 * t144 * t15 * t10)
      t150 = t149 ** 2
      t152 = x3 * z
      t154 = 0.2D1 * t7 * z
      t159 = (-0.1D1 - t152 - t127 + x3 + t154 - t136 - t30 + x2 + 0.2D1
     # * t129 * t134 * z) ** 2
      t161 = t128 * x3
      t162 = t161 * z
      t164 = 0.2D1 * t128 * z
      t165 = 0.3D1 * t128
      t166 = t23 * x3
      t169 = t23 * t134
      t172 = t161 * x2
      t174 = 0.2D1 * t172 * z
      t175 = t128 * x2
      t177 = 0.5D1 * t175 * z
      t178 = 0.5D1 * t175
      t179 = 0.2D1 * t172
      t181 = z * t23
      t185 = t161 - t162 + t164 - t165 - 0.2D1 * t166 * t134 - 0.4D1 * t
     #169 * z + t174 - t177 + t178 - t179 + 0.4D1 * t169 + 0.2D1 * t181 
     #* x3 * t134
      t186 = t185 ** 2
      t190 = 0.1D1 / t159 * t186 / t32 * t1
      t203 = t83 * t15 * t10
      t207 = log(0.4D1 * t143 * t144 * t203)
      t217 = -(-0.180D3 * t6 * t150 * t190 - 0.720D3 * t6 * lh * t149 * 
     #t190 - 0.4D1 * t48 * t190) * t52 / 0.1440D4 - (0.360D3 * t6 * t207
     # * t190 + 0.720D3 * t95 * t190) * t52 * t100 / 0.720D3
      t218 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t127 - x2 + t136), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t127 + t136), 0.0D0, t217)
      t220 = -0.1D1 + x1
      t222 = x1 * z
      t223 = 0.1D1 - x1 + t222
      t224 = 0.1D1 / t223
      t235 = s * t14 * x2 * x1 * t220 * t224
      t238 = t220 ** 2
      t239 = t224 * t238
      t244 = log(-0.4D1 * t7 * t83 * t10 * t16 * t239 * t4)
      t247 = t29 * t24
      t248 = x2 * x1
      t249 = t248 * z
      t251 = (-0.1D1 + x1 - t30 + t249 + x2 - t248 - t222) ** 2
      t252 = t251 ** 2
      t256 = t4 * t223
      t258 = Sqrt(-t256 * x2)
      t259 = t258 ** 2
      t260 = 0.1D1 / t252 * t238 * t220 * t259
      t261 = t247 * t260
      t276 = log(-0.4D1 * t104 * t86 * t87 * t239)
      t277 = t276 ** 2
      t297 = -(-0.5760D4 * t6 * t244 * t223 * t261 - 0.11520D5 * t6 * lh
     # * t223 * t261) * t52 * t100 / 0.720D3 + (-0.2880D4 * t6 * t247 * 
     #t277 * t223 * t260 - 0.11520D5 * t6 * t115 * t29 * t276 * t223 * t
     #260 - 0.64D2 * t6 * t47 * t24 * t29 * t223 * t260) * t100 / 0.720D
     #3
      t298 = FJET(XB1, XB2, s, 0.0D0, -t2 * t220 * x2 * t224, t2 * x1, t
     #4 * s * t1 * t220, -t235, t297)
      t300 = x3 * x1
      t302 = t300 * z
      t303 = t7 * x1
      t304 = t7 * t222
      t308 = Sqrt(t256 * x2 * t130 * t131)
      t310 = 0.2D1 * t129 * t308
      t318 = 0.1D1 - x1 + t222 - x2 + t248 - t249 - x3 + t300 - t302 + t
     #127 - t303 + t304 + t310
      t329 = log(0.4D1 * t141 * t142 * t4 * x3 * t238 * t224 * t203)
      t330 = x3 * t308
      t334 = t128 * t12
      t337 = t23 * t308
      t349 = x1 * t161
      t350 = x2 * t12
      t352 = t128 * x1
      t354 = -t161 + 0.2D1 * t181 * t330 * x1 + t165 - 0.2D1 * t334 * x1
     # + 0.4D1 * t337 * x1 + 0.2D1 * t166 * t308 + 0.4D1 * t337 * z + t1
     #72 * t83 * t12 - 0.2D1 * t83 * t161 * t30 - t349 * t350 + t177 - t
     #174 + t162 - t164 - t178 + t179 + t349 - 0.4D1 * t352
      t355 = t128 * t83
      t384 = t308 * x1
      t389 = t355 - 0.4D1 * t337 + 0.6D1 * t352 * z + 0.8D1 * t175 * x1 
     #- 0.2D1 * t355 * z - 0.3D1 * t355 * x2 + t355 * t12 - 0.2D1 * t349
     # * z + t349 * t12 + t172 * t83 - 0.3D1 * t349 * x2 - 0.11D2 * t175
     # * t222 + 0.3D1 * t334 * t248 + 0.6D1 * t355 * t30 - 0.3D1 * t355 
     #* t350 - 0.2D1 * t181 * t330 + 0.4D1 * t172 * t222 - 0.2D1 * t166 
     #* t384 - 0.4D1 * t337 * t222
      t391 = (t354 + t389) ** 2
      t397 = x3 * t12
      t403 = 0.1D1 - x1 - t249 - 0.2D1 * t302 - t154 - 0.3D1 * t303 + t3
     #97 * x1 + t85 + t127 + 0.4D1 * t304 - t397 * t248 - 0.2D1 * t84 * 
     #t30
      t407 = t308 * z
      t413 = t84 * t350 - x2 - x3 + t30 + t248 + t152 + t300 + t222 + t3
     #10 - 0.2D1 * t129 * t384 - 0.2D1 * t129 * t407 + 0.2D1 * t129 * t4
     #07 * x1
      t415 = (t403 + t413) ** 2
      t420 = t220 * t223 / t415 * t1 / t251
      t427 = 0.360D3 * t6 * t329 * t391 * t420 + 0.720D3 * t6 * lh * t39
     #1 * t420
      t431 = FJET(XB1, XB2, s, t2 * t300, t2 * t220 * (-x3 + t300 - t302
     # + t127 - t303 + t304 - x2 + t310) * t224, -t2 * x1 * t130 * t131,
     # -t2 * t220 * t318 * t224, -t235, -t427 * t52 * t100 / 0.720D3)
      rrqg2qght13s1e1 = t125 * t124 + t218 * t217 + t298 * t297 - t431 *
     # t427 * t52 * t100 / 0.720D3

      end function



      doubleprecision function rrqg2qght13s1e0
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
      t22 = cos(t8)
      t23 = t22 ** 2
      t24 = t4 * x2
      t25 = Sqrt(-t24)
      t26 = t25 ** 2
      t27 = t23 * t26
      t28 = t14 * t1
      t29 = x2 * z
      t31 = (0.1D1 + t29 - x2) ** 2
      t32 = t31 ** 2
      t33 = 0.1D1 / t32
      t34 = t28 * t33
      t35 = t27 * t34
      t38 = t6 * lh
      t40 = 0.11520D5 * t38 * t35
      t42 = 0.1D1 / x3
      t46 = 0.1D1 / x1
      t47 = t42 * t46
      t52 = x1 ** 2
      t53 = x2 * t52
      t57 = log(-0.4D1 * t53 * t10 * t17)
      t69 = log(-0.4D1 * x2 * t10 * t17)
      t74 = t69 ** 2
      t78 = lh ** 2
      t80 = 0.3141592653589793D1 ** 2
      t90 = -(-0.5760D4 * t6 * t20 * t35 - t40) * t42 / 0.1440D4 - 0.8D1
     # * t6 * t27 * t34 * t47 + (0.5760D4 * t6 * t23 * t28 * t57 * t26 *
     # t33 + t40) * t46 / 0.720D3 - 0.2D1 / 0.45D2 * (0.180D3 * t69 * z 
     #* wd * lh + 0.45D2 * t74 * z * wd + t6 * (0.180D3 * t78 - 0.30D2 *
     # t80)) * t23 * t26 * t28 * t33
      t91 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t9
     #0)
      t93 = 0.2D1 * t7
      t94 = sqrt(x3)
      t95 = t22 * t94
      t96 = -0.1D1 + t94
      t97 = t94 + 0.1D1
      t100 = Sqrt(t24 * t96 * t97)
      t102 = 0.2D1 * t95 * t100
      t115 = log(0.4D1 * t13 * t96 * t97 * x2 * t4 * x3 * t15 * t10)
      t117 = x3 * z
      t119 = 0.2D1 * t7 * z
      t124 = (-0.1D1 - t117 - t93 + x3 + t119 - t102 - t29 + x2 + 0.2D1 
     #* t95 * t100 * z) ** 2
      t126 = t94 * x3
      t127 = t126 * z
      t129 = 0.2D1 * t94 * z
      t130 = 0.3D1 * t94
      t131 = t22 * x3
      t134 = t22 * t100
      t137 = t126 * x2
      t139 = 0.2D1 * t137 * z
      t140 = t94 * x2
      t142 = 0.5D1 * t140 * z
      t143 = 0.5D1 * t140
      t144 = 0.2D1 * t137
      t146 = z * t22
      t150 = t126 - t127 + t129 - t130 - 0.2D1 * t131 * t100 - 0.4D1 * t
     #134 * z + t139 - t142 + t143 - t144 + 0.4D1 * t134 + 0.2D1 * t146 
     #* x3 * t100
      t151 = t150 ** 2
      t152 = 0.1D1 / t124 * t151
      t154 = 0.1D1 / t31 * t1
      t155 = t152 * t154
      t167 = -(0.360D3 * t6 * t115 * t155 + 0.720D3 * t38 * t155) * t42 
     #/ 0.1440D4 + t6 * t152 * t154 * t47 / 0.2D1
      t168 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t93 - x2 + t102), 0.0
     #D0, t2 * (0.1D1 - x2 - x3 + t93 + t102), 0.0D0, t167)
      t170 = -0.1D1 + x1
      t172 = x1 * z
      t173 = 0.1D1 - x1 + t172
      t174 = 0.1D1 / t173
      t185 = s * t14 * x2 * x1 * t170 * t174
      t189 = x2 * x1
      t190 = t189 * z
      t192 = (-0.1D1 + x1 - t29 + t190 + x2 - t189 - t172) ** 2
      t193 = t192 ** 2
      t195 = t170 ** 2
      t197 = 0.1D1 / t193 * t195 * t170
      t198 = t4 * t173
      t200 = Sqrt(-t198 * x2)
      t201 = t200 ** 2
      t207 = t23 * t28
      t216 = log(-0.4D1 * t53 * t10 * t13 * t15 * t4 * t195 * t174)
      t218 = t197 * t201
      t230 = -0.8D1 * t6 * t173 * t28 * t23 * t197 * t201 * t42 * t46 + 
     #(0.5760D4 * t6 * t207 * t216 * t173 * t218 + 0.11520D5 * t6 * lh *
     # t173 * t207 * t218) * t46 / 0.720D3
      t231 = FJET(XB1, XB2, s, 0.0D0, -t2 * t170 * x2 * t174, t2 * x1, t
     #4 * s * t1 * t170, -t185, t230)
      t233 = x3 * x1
      t235 = t233 * z
      t236 = t7 * x1
      t237 = t7 * t172
      t241 = Sqrt(t198 * x2 * t96 * t97)
      t243 = 0.2D1 * t95 * t241
      t251 = 0.1D1 - x1 + t172 - x2 + t189 - t190 - x3 + t233 - t235 + t
     #93 - t236 + t237 + t243
      t255 = x1 * t126
      t256 = t94 * x1
      t258 = t94 * t52
      t259 = t22 * t241
      t261 = x3 * t241
      t270 = x2 * t12
      t274 = t94 * t12
      t277 = t255 - 0.4D1 * t256 + t258 - 0.4D1 * t259 + 0.2D1 * t146 * 
     #t261 * x1 - t139 + t142 + t127 - t129 - t143 + t144 - t126 + t130 
     #+ t137 * t52 * t12 - 0.2D1 * t52 * t126 * t29 - t255 * t270 - 0.11
     #D2 * t140 * t172 + 0.3D1 * t274 * t189
      t286 = t241 * x1
      t314 = 0.6D1 * t258 * t29 - 0.3D1 * t258 * t270 - 0.2D1 * t146 * t
     #261 + 0.4D1 * t137 * t172 - 0.2D1 * t131 * t286 - 0.4D1 * t259 * t
     #172 + 0.6D1 * t256 * z + 0.8D1 * t140 * x1 - 0.2D1 * t258 * z - 0.
     #3D1 * t258 * x2 + t258 * t12 - 0.2D1 * t255 * z + t255 * t12 + t13
     #7 * t52 - 0.3D1 * t255 * x2 - 0.2D1 * t274 * x1 + 0.4D1 * t259 * x
     #1 + 0.2D1 * t131 * t241 + 0.4D1 * t259 * z
      t316 = (t277 + t314) ** 2
      t322 = x3 * t12
      t324 = x3 * t52
      t330 = 0.1D1 - x1 - t190 - 0.2D1 * t235 - t119 - 0.3D1 * t236 + t3
     #22 * x1 + t324 * x2 + t93 + 0.4D1 * t237 - t322 * t189 - 0.2D1 * t
     #324 * t29
      t334 = t241 * z
      t340 = t324 * t270 - x2 - x3 + t29 + t189 + t117 + t233 + t172 + t
     #243 - 0.2D1 * t95 * t286 - 0.2D1 * t95 * t334 + 0.2D1 * t95 * t334
     # * x1
      t342 = (t330 + t340) ** 2
      t343 = 0.1D1 / t342
      t347 = 0.1D1 / t192 * t42 * t46
      t351 = FJET(XB1, XB2, s, t2 * t233, t2 * t170 * (-x3 + t233 - t235
     # + t93 - t236 + t237 - x2 + t243) * t174, -t2 * x1 * t96 * t97, -t
     #2 * t170 * t251 * t174, -t185, t6 * t316 * t170 * t173 * t343 * t1
     # * t347 / 0.2D1)
      rrqg2qght13s1e0 = t91 * t90 + t168 * t167 + t231 * t230 + t351 * z
     # * wd * t316 * t170 * t173 * t343 * t1 * t347 / 0.2D1

      end function



      doubleprecision function rrqg2qght13s1em1
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
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = t4 * x2
      t12 = Sqrt(-t11)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 * t1
      t16 = t13 * t15
      t17 = x2 * z
      t19 = (0.1D1 + t17 - x2) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 / x1
      t29 = Sin(t7)
      t30 = t29 ** 2
      t32 = z ** 2
      t34 = t14 ** 2
      t39 = log(-0.4D1 * x2 * t30 / t32 * t34 * t4)
      t48 = 0.1D1 / x3
      t53 = -0.8D1 * t10 * t16 * t21 * t22 - 0.2D1 / 0.45D2 * (-0.180D3 
     #* t6 * lh - 0.90D2 * t39 * z * wd) * t9 * t16 * t21 - 0.4D1 * t10 
     #* t16 * t21 * t48
      t54 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t5
     #3)
      t56 = -0.1D1 + x1
      t58 = x1 * z
      t59 = 0.1D1 - x1 + t58
      t60 = 0.1D1 / t59
      t74 = x2 * x1
      t77 = (-0.1D1 + x1 - t17 + t74 * z + x2 - t74 - t58) ** 2
      t78 = t77 ** 2
      t81 = t56 ** 2
      t85 = Sqrt(-t4 * t59 * x2)
      t86 = t85 ** 2
      t89 = t59 / t78 * t81 * t56 * t86 * t22
      t92 = FJET(XB1, XB2, s, 0.0D0, -t2 * t56 * x2 * t60, t2 * x1, t4 *
     # s * t1 * t56, -s * t14 * x2 * x1 * t56 * t60, -0.8D1 * t6 * t9 * 
     #t15 * t89)
      t99 = x2 * x3
      t100 = 0.2D1 * t99
      t101 = sqrt(x3)
      t102 = t8 * t101
      t107 = Sqrt(t11 * (-0.1D1 + t101) * (t101 + 0.1D1))
      t109 = 0.2D1 * t102 * t107
      t121 = (-0.1D1 - x3 * z - t100 + x3 + 0.2D1 * t99 * z - t109 - t17
     # + x2 + 0.2D1 * t102 * t107 * z) ** 2
      t122 = 0.1D1 / t121
      t124 = t101 * x3
      t132 = t8 * t107
      t135 = t124 * x2
      t138 = t101 * x2
      t148 = t124 - t124 * z + 0.2D1 * t101 * z - 0.3D1 * t101 - 0.2D1 *
     # t8 * x3 * t107 - 0.4D1 * t132 * z + 0.2D1 * t135 * z - 0.5D1 * t1
     #38 * z + 0.5D1 * t138 - 0.2D1 * t135 + 0.4D1 * t132 + 0.2D1 * z * 
     #t8 * x3 * t107
      t149 = t148 ** 2
      t153 = t149 / t19 * t1 * t48
      t156 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t100 - x2 + t109), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t100 + t109), 0.0D0, t6 * t122 * t153
     # / 0.4D1)
      rrqg2qght13s1em1 = t54 * t53 - 0.8D1 * t92 * z * wd * t9 * t15 * t
     #89 + t156 * z * wd * t122 * t153 / 0.4D1

      end function



      doubleprecision function rrqg2qght13s1em2
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
      t8 = cos(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t12 = Sqrt(-t4 * x2)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 * t1
      t19 = (0.1D1 + x2 * z - x2) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t25 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, -0
     #.4D1 * z * wd * t9 * t13 * t15 * t21)
      rrqg2qght13s1em2 = -0.4D1 * t25 * z * wd * t9 * t13 * t15 * t21

      end function



      doubleprecision function rrqg2qght13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght13s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght13s1em4 = 0.0D0

      end function
