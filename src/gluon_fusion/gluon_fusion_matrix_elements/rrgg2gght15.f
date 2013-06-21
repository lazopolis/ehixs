  
      subroutine rrgg2gght15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght15s1e1  
      doubleprecision rrgg2gght15s1e0  
      doubleprecision rrgg2gght15s1em1  
      doubleprecision rrgg2gght15s1em2  
      doubleprecision rrgg2gght15s1em3  
      doubleprecision rrgg2gght15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght15s1e1
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 ** 2
      t16 = x2 * 0.3141592653589793D1
      t17 = cos(t16)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = 0.1D1 / z
      t21 = wd * t20
      t22 = t19 * t21
      t23 = t6 * x1
      t24 = sin(t16)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = x1 ** 2
      t31 = t6 ** 2
      t33 = t9 ** 2
      t37 = log(0.4D1 * t28 * t29 * t31 * x4 * t33)
      t38 = t37 ** 2
      t40 = 0.1D1 / (-0.2D1 + t1)
      t46 = t19 * wd
      t47 = t20 * t6
      t48 = t47 * x1
      t49 = t46 * t48
      t51 = t47 * lh
      t52 = t46 * t51
      t54 = t19 * t21 * t6
      t57 = (-0.180D3 * t52 - 0.180D3 * t54) * x1
      t58 = 0.180D3 * t49 + t57
      t60 = t40 * t33
      t64 = lh ** 2
      t66 = 0.3141592653589793D1 ** 2
      t68 = 0.180D3 * t64 - 0.30D2 * t66
      t69 = t47 * t68
      t75 = 0.270D3 * t49 + 0.2D1 * t57 + (t46 * t69 + 0.360D3 * t52 + 0
     #.90D2 * t54) * x1
      t77 = t75 * t40 * t33
      t79 = 0.1D1 / x4
      t82 = t19 * t60
      t90 = t29 * t31
      t91 = t90 * t33
      t94 = log(0.4D1 * t28 * t91)
      t97 = t18 * t40
      t98 = (-0.2D1 - t94) * t15 * t97
      t99 = t33 * wd
      t100 = t99 * t47
      t109 = t99 * t51
      t113 = t94 ** 2
      t117 = (0.1D1 + 0.2D1 * t94 + t113 / 0.2D1) * t15 * t97
      t145 = t15 * wd
      t146 = t145 * t47
      t148 = x3 * t25 * t27
      t153 = log(0.4D1 * t148 * t90 * x4 * t33)
      t159 = t145 * t48
      t165 = 0.180D3 * t159 + (-0.180D3 * t145 * t51 - 0.180D3 * t146) *
     # x1
      t166 = t165 * t18
      t170 = 0.1D1 / x3
      t176 = log(0.4D1 * t148 * t91)
      t177 = t176 ** 2
      t188 = -(-0.45D2 * t22 * t23 * t38 * t40 * t33 + t58 * t37 * t60 -
     # t77) * t79 / 0.10D2 + 0.36D2 * t82 * t21 * t23 + 0.3D1 / 0.10D2 *
     # (-0.180D3 * t82 * t21 * t6 * lh + 0.90D2 * t98 * t100) * x1 + (t8
     #2 * t21 * t6 * t68 - 0.180D3 * t98 * t109 + 0.90D2 * t117 * t100) 
     #* x1 / 0.5D1 + (t82 * t21 * t6 * (0.60D2 * lh * t66 - 0.2884936567
     #583026D3 - 0.120D3 * t64 * lh) + t98 * t99 * t69 - 0.180D3 * t117 
     #* t109 + 0.90D2 * (-t94 - t113 - t113 * t94 / 0.6D1) * t15 * t97 *
     # t100) * x1 / 0.10D2 + (-0.360D3 * t146 * x1 * t153 * t97 * t33 + 
     #0.4D1 * t166 * t60) * t170 * t79 / 0.40D2 - (-0.45D2 * t22 * t23 *
     # t177 * t40 * t33 + t58 * t176 * t60 - t77) * t170 / 0.10D2
      t189 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t188)
      t191 = sqrt(x4)
      t192 = -0.1D1 + t191
      t193 = t191 + 0.1D1
      t194 = t192 * t193
      t195 = KAPPA2(x1, x2, 0.0D0, -t194, z)
      t196 = s * t195
      t197 = t196 * t4
      t200 = t6 * t192 * t193
      t201 = t196 * t3 * t200
      t202 = t7 * x4
      t203 = t196 * t202
      t204 = t195 ** 2
      t209 = s * t204 * t11 * t23 * (-0.1D1 + x4)
      t210 = t204 ** 2
      t216 = log(-0.4D1 * t210 * x4 * t194 * t28 * t90)
      t217 = t216 ** 2
      t220 = 0.1D1 / (-0.2D1 + t195)
      t222 = Sqrt(-t194)
      t223 = t222 ** 2
      t224 = t220 * t210 * t223
      t231 = t210 * t223
      t240 = t25 * t31
      t245 = log(-0.4D1 * x3 * t210 * x4 * t192 * t193 * t27 * t240 * t2
     #9)
      t247 = t231 * t18
      t258 = -(0.45D2 * t54 * x1 * t217 * t224 - t58 * t216 * t224 + t75
     # * t220 * t231) * t79 / 0.10D2 + (0.360D3 * t159 * t245 * t220 * t
     #247 - 0.4D1 * t165 * t220 * t247) * t170 * t79 / 0.40D2
      t259 = FJET(XB1, XB2, s, 0.0D0, t197, t201, -t203, t209, t258)
      t261 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t188)
      t263 = FJET(XB1, XB2, s, t197, 0.0D0, -t203, t201, t209, t258)
      t265 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t266 = s * t265
      t267 = t4 * x3
      t268 = t266 * t267
      t270 = sqrt(x3)
      t271 = -0.1D1 + t270
      t273 = t270 + 0.1D1
      t274 = x1 * t271 * t273
      t275 = t266 * t3 * t274
      t276 = t266 * t7
      t277 = t265 ** 2
      t282 = s * t277 * t11 * t23 * (-0.1D1 + x3)
      t283 = x3 * x4
      t284 = t277 ** 2
      t287 = t271 * t273
      t288 = t287 * t29
      t292 = log(-0.4D1 * t283 * t284 * t27 * t240 * t288)
      t294 = Sqrt(-t287)
      t295 = t294 ** 2
      t297 = 0.1D1 / (-0.2D1 + t265)
      t299 = t295 * t297 * t284
      t316 = log(-0.4D1 * x3 * t284 * t28 * t31 * t271 * t273 * t29)
      t317 = t316 ** 2
      t330 = (0.360D3 * t159 * t292 * t18 * t299 - 0.4D1 * t166 * t299) 
     #* t170 * t79 / 0.40D2 - (0.45D2 * t54 * x1 * t317 * t299 - t58 * t
     #316 * t299 + t75 * t295 * t297 * t284) * t170 / 0.10D2
      t331 = FJET(XB1, XB2, s, t268, -t275, -t276, 0.0D0, t282, t330)
      t333 = KAPPA2(x1, x2, x3, -t194, z)
      t334 = s * t333
      t335 = t334 * t267
      t336 = t334 * t3
      t337 = t336 * t274
      t338 = t336 * t200
      t339 = t334 * t202
      t340 = t333 ** 2
      t346 = Sqrt(t287 * t194)
      t352 = s * t340 * t11 * t23 * (x3 - 0.1D1 + x4 - 0.2D1 * t283 + 0.
     #2D1 * t17 * t270 * t191 * t346)
      t353 = t340 ** 2
      t360 = log(0.4D1 * t283 * t194 * t353 * t28 * t31 * t288)
      t366 = (-t270 * t191 + 0.2D1 * t17 * t346) ** 2
      t368 = 0.1D1 / (-0.2D1 + t333)
      t377 = -0.90D2 * t146 * x1 * t360 * t366 * t368 * t353 + t165 * t3
     #66 * t368 * t353
      t380 = t377 * t170 * t79 / 0.40D2
      t381 = FJET(XB1, XB2, s, t335, -t337, t338, -t339, t352, t380)
      t383 = t170 * t79
      t386 = FJET(XB1, XB2, s, -t275, t268, 0.0D0, -t276, t282, t330)
      t388 = FJET(XB1, XB2, s, -t337, t335, -t339, t338, t352, t380)
      rrgg2gght15s1e1 = t189 * t188 + t259 * t258 + t261 * t188 + t263 *
     # t258 + t331 * t330 + t381 * t377 * t383 / 0.40D2 + t386 * t330 + 
     #t388 * t377 * t383 / 0.40D2

      end function



      doubleprecision function rrgg2gght15s1e0
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 ** 2
      t16 = x2 * 0.3141592653589793D1
      t17 = cos(t16)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = 0.1D1 / z
      t21 = wd * t20
      t22 = t19 * t21
      t23 = t6 * x1
      t24 = sin(t16)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = x1 ** 2
      t31 = t6 ** 2
      t33 = t9 ** 2
      t37 = log(0.4D1 * t28 * t29 * t31 * x4 * t33)
      t39 = 0.1D1 / (-0.2D1 + t1)
      t45 = t19 * wd
      t46 = t20 * t6
      t47 = t46 * x1
      t50 = t46 * lh
      t52 = t21 * t6
      t53 = t19 * t52
      t57 = 0.180D3 * t45 * t47 + (-0.180D3 * t45 * t50 - 0.180D3 * t53)
     # * x1
      t59 = t57 * t39 * t33
      t61 = 0.1D1 / x4
      t64 = t39 * t33
      t67 = 0.1D1 / x3
      t75 = t29 * t31
      t76 = t75 * t33
      t79 = log(0.4D1 * x3 * t25 * t27 * t76)
      t88 = t19 * t64
      t98 = log(0.4D1 * t28 * t76)
      t101 = t18 * t39
      t102 = (-0.2D1 - t98) * t15 * t101
      t103 = t33 * wd
      t104 = t103 * t46
      t110 = lh ** 2
      t112 = 0.3141592653589793D1 ** 2
      t122 = t98 ** 2
      t132 = -(0.90D2 * t22 * t23 * t37 * t39 * t33 - t59) * t61 / 0.10D
     #2 + 0.9D1 * t19 * t64 * wd * t46 * x1 * t67 * t61 - (0.90D2 * t22 
     #* t23 * t79 * t39 * t33 - t59) * t67 / 0.10D2 + 0.27D2 * t88 * t21
     # * t23 + (-0.180D3 * t88 * t21 * t6 * lh + 0.90D2 * t102 * t104) *
     # x1 / 0.5D1 + (t88 * t21 * t6 * (0.180D3 * t110 - 0.30D2 * t112) -
     # 0.180D3 * t102 * t103 * t50 + 0.90D2 * (0.1D1 + 0.2D1 * t98 + t12
     #2 / 0.2D1) * t15 * t101 * t104) * x1 / 0.10D2
      t133 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t132)
      t135 = sqrt(x4)
      t136 = -0.1D1 + t135
      t137 = t135 + 0.1D1
      t138 = t136 * t137
      t139 = KAPPA2(x1, x2, 0.0D0, -t138, z)
      t140 = s * t139
      t141 = t140 * t4
      t144 = t6 * t136 * t137
      t145 = t140 * t3 * t144
      t146 = t7 * x4
      t147 = t140 * t146
      t148 = t139 ** 2
      t153 = s * t148 * t11 * t23 * (-0.1D1 + x4)
      t155 = t15 * wd * t47
      t157 = 0.1D1 / (-0.2D1 + t139)
      t158 = t148 ** 2
      t160 = Sqrt(-t138)
      t161 = t160 ** 2
      t162 = t157 * t158 * t161
      t173 = log(-0.4D1 * t158 * x4 * t138 * t28 * t75)
      t184 = -0.9D1 * t155 * t162 * t18 * t67 * t61 - (-0.90D2 * t53 * x
     #1 * t173 * t162 + t57 * t157 * t158 * t161) * t61 / 0.10D2
      t185 = FJET(XB1, XB2, s, 0.0D0, t141, t145, -t147, t153, t184)
      t187 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t132)
      t189 = FJET(XB1, XB2, s, t141, 0.0D0, -t147, t145, t153, t184)
      t191 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t192 = s * t191
      t193 = t4 * x3
      t194 = t192 * t193
      t196 = sqrt(x3)
      t197 = -0.1D1 + t196
      t199 = t196 + 0.1D1
      t200 = x1 * t197 * t199
      t201 = t192 * t3 * t200
      t202 = t192 * t7
      t203 = t191 ** 2
      t208 = s * t203 * t11 * t23 * (-0.1D1 + x3)
      t209 = t197 * t199
      t210 = Sqrt(-t209)
      t211 = t210 ** 2
      t214 = 0.1D1 / (-0.2D1 + t191)
      t216 = t203 ** 2
      t229 = log(-0.4D1 * x3 * t216 * t28 * t31 * t197 * t199 * t29)
      t242 = -0.9D1 * t155 * t18 * t211 * t214 * t216 * t67 * t61 - (-0.
     #90D2 * t53 * x1 * t229 * t211 * t214 * t216 + t57 * t211 * t214 * 
     #t216) * t67 / 0.10D2
      t243 = FJET(XB1, XB2, s, t194, -t201, -t202, 0.0D0, t208, t242)
      t245 = KAPPA2(x1, x2, x3, -t138, z)
      t246 = s * t245
      t247 = t246 * t193
      t248 = t246 * t3
      t249 = t248 * t200
      t250 = t248 * t144
      t251 = t246 * t146
      t252 = t245 ** 2
      t259 = Sqrt(t209 * t138)
      t265 = s * t252 * t11 * t23 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 +
     # 0.2D1 * t17 * t196 * t135 * t259)
      t270 = (-t196 * t135 + 0.2D1 * t17 * t259) ** 2
      t272 = 0.1D1 / (-0.2D1 + t245)
      t274 = t252 ** 2
      t276 = t274 * t67 * t61
      t279 = 0.9D1 / 0.4D1 * t155 * t270 * t272 * t276
      t280 = FJET(XB1, XB2, s, t247, -t249, t250, -t251, t265, t279)
      t285 = x1 * t270 * t272 * t276
      t288 = FJET(XB1, XB2, s, -t201, t194, 0.0D0, -t202, t208, t242)
      t290 = FJET(XB1, XB2, s, -t249, t247, -t251, t250, t265, t279)
      rrgg2gght15s1e0 = t133 * t132 + t185 * t184 + t187 * t132 + t189 *
     # t184 + t243 * t242 + 0.9D1 / 0.4D1 * t280 * t15 * t52 * t285 + t2
     #88 * t242 + 0.9D1 / 0.4D1 * t290 * t15 * t52 * t285

      end function



      doubleprecision function rrgg2gght15s1em1
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 ** 2
      t16 = x2 * 0.3141592653589793D1
      t17 = cos(t16)
      t18 = t17 ** 2
      t21 = 0.1D1 / (-0.2D1 + t1)
      t22 = t9 ** 2
      t24 = t15 * t18 * t21 * t22
      t25 = 0.1D1 / z
      t26 = wd * t25
      t27 = t6 * x1
      t28 = 0.1D1 / x3
      t40 = sin(t16)
      t41 = t40 ** 2
      t42 = z ** 2
      t45 = x1 ** 2
      t46 = t6 ** 2
      t51 = log(0.4D1 * t41 / t42 * t45 * t46 * t22)
      t57 = t25 * t6
      t64 = 0.1D1 / x4
      t69 = 0.9D1 * t24 * t26 * t27 * t28 + 0.18D2 * t24 * t26 * t27 + (
     #-0.180D3 * t24 * t26 * t6 * lh + 0.90D2 * (-0.2D1 - t51) * t15 * t
     #18 * t21 * t22 * wd * t57) * x1 / 0.10D2 + 0.9D1 * t24 * t26 * t27
     # * t64
      t70 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t69)
      t72 = sqrt(x4)
      t73 = -0.1D1 + t72
      t74 = t72 + 0.1D1
      t75 = t73 * t74
      t76 = KAPPA2(x1, x2, 0.0D0, -t75, z)
      t77 = s * t76
      t78 = t77 * t4
      t82 = t77 * t3 * t6 * t73 * t74
      t84 = t77 * t7 * x4
      t85 = t76 ** 2
      t90 = s * t85 * t11 * t27 * (-0.1D1 + x4)
      t93 = t15 * wd * t57 * x1
      t95 = 0.1D1 / (-0.2D1 + t76)
      t96 = t85 ** 2
      t98 = Sqrt(-t75)
      t99 = t98 ** 2
      t101 = t99 * t18 * t64
      t104 = 0.9D1 * t93 * t95 * t96 * t101
      t105 = FJET(XB1, XB2, s, 0.0D0, t78, t82, -t84, t90, -t104)
      t107 = t26 * t6
      t111 = x1 * t95 * t96 * t101
      t114 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t69)
      t116 = FJET(XB1, XB2, s, t78, 0.0D0, -t84, t82, t90, -t104)
      t121 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t122 = s * t121
      t124 = t122 * t4 * x3
      t126 = sqrt(x3)
      t127 = -0.1D1 + t126
      t129 = t126 + 0.1D1
      t131 = t122 * t3 * x1 * t127 * t129
      t132 = t122 * t7
      t133 = t121 ** 2
      t138 = s * t133 * t11 * t27 * (-0.1D1 + x3)
      t140 = Sqrt(-t127 * t129)
      t141 = t140 ** 2
      t145 = t133 ** 2
      t147 = 0.1D1 / (-0.2D1 + t121) * t145 * t28
      t150 = 0.9D1 * t93 * t18 * t141 * t147
      t151 = FJET(XB1, XB2, s, t124, -t131, -t132, 0.0D0, t138, -t150)
      t156 = x1 * t18 * t141 * t147
      t159 = FJET(XB1, XB2, s, -t131, t124, 0.0D0, -t132, t138, -t150)
      rrgg2gght15s1em1 = t70 * t69 - 0.9D1 * t105 * t15 * t107 * t111 + 
     #t114 * t69 - 0.9D1 * t116 * t15 * t107 * t111 - 0.9D1 * t151 * t15
     # * t107 * t156 - 0.9D1 * t159 * t15 * t107 * t156

      end function



      doubleprecision function rrgg2gght15s1em2
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
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 ** 2
      t17 = cos(x2 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t21 = 0.1D1 / (-0.2D1 + t1)
      t22 = t9 ** 2
      t25 = 0.1D1 / z
      t30 = 0.9D1 * t15 * t18 * t21 * t22 * wd * t25 * t6 * x1
      t31 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t30)
      t33 = t18 * t21
      t38 = t22 * wd * t25 * t6 * x1
      t40 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t30)
      rrgg2gght15s1em2 = 0.9D1 * t31 * t15 * t33 * t38 + 0.9D1 * t40 * t
     #15 * t33 * t38

      end function



      doubleprecision function rrgg2gght15s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght15s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght15s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght15s1em4 = 0.0D0

      end function
