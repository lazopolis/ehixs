  
      subroutine bggbH9n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH9n1e1  
      doubleprecision bggbH9n1e0  
      doubleprecision bggbH9n1em1  
      doubleprecision bggbH9n1em2  
      doubleprecision bggbH9n1em3  
      doubleprecision bggbH9n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH9n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH9n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH9n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH9n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH9n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH9n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH9n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t15 = log(-0.4D1 * x2 * t8 * t11 * t4)
      t16 = x2 * z
      t18 = (0.1D1 - x2 + t16) ** 2
      t19 = t18 ** 2
      t20 = 0.1D1 / t19
      t22 = t15 * t20 * wd
      t23 = t15 ** 2
      t26 = t23 * t20 * wd / 0.2D1
      t28 = cos(t6)
      t29 = t28 ** 2
      t31 = t4 * x2
      t32 = Sqrt(-t31)
      t33 = t32 ** 2
      t35 = t1 ** 2
      t36 = t35 ** 2
      t37 = t36 * z
      t38 = t37 * lh
      t47 = t33 * t36
      t51 = t20 * wd
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t60 = t37 * t59
      t63 = wd * z
      t65 = 0.180D3 * t63 * lh
      t67 = -t65 - 0.90D2 * t63
      t68 = x2 * x3
      t69 = t8 * t11
      t70 = t69 * t4
      t73 = log(-0.4D1 * t68 * t70)
      t77 = t29 * t33 * t36
      t80 = t73 ** 2
      t83 = t20 * t29 * t47
      t87 = t65 + t63 * t59
      t92 = 0.1D1 / x3
      t105 = x1 ** 2
      t106 = t105 * x3
      t107 = t106 * x2
      t110 = log(-0.4D1 * t107 * t70)
      t118 = 0.1D1 / x1
      t122 = wd * t29
      t124 = 0.180D3 * t122 * t38
      t125 = t122 * t37
      t127 = -t124 - 0.90D2 * t125
      t128 = x2 * t105
      t131 = log(-0.4D1 * t128 * t70)
      t133 = t20 * t33
      t137 = t131 ** 2
      t143 = t124 + t122 * t60
      t150 = -0.24D2 * (t22 + t26) * t29 * t33 * t38 + 0.12D2 * (-t26 - 
     #t23 * t15 * t20 * wd / 0.6D1) * t29 * t47 * z + 0.2D1 / 0.15D2 * (
     #-t51 - t22) * t29 * t33 * t60 + (-0.4D1 * t67 * t73 * t20 * t77 + 
     #0.180D3 * t63 * t80 * t83 + 0.4D1 * t87 * t20 * t77) * t92 / 0.30D
     #2 + 0.2D1 / 0.15D2 * t51 * t29 * t47 * z * (-0.2884936567583026D3 
     #- 0.120D3 * t55 * lh + 0.60D2 * lh * t57) + (-0.360D3 * t63 * t110
     # * t83 + 0.4D1 * t67 * t20 * t77) * t118 * t92 / 0.15D2 - (0.4D1 *
     # t127 * t131 * t133 - 0.180D3 * t122 * t36 * z * t137 * t133 - 0.4
     #D1 * t143 * t20 * t33) * t118 / 0.15D2
      t151 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #150)
      t153 = -0.1D1 + x1
      t154 = x1 * x3
      t155 = t154 * z
      t156 = 0.2D1 * t68
      t157 = t68 * x1
      t158 = x1 * z
      t159 = t68 * t158
      t160 = sqrt(x3)
      t161 = t28 * t160
      t162 = 0.1D1 - x1 + t158
      t163 = t4 * t162
      t164 = -0.1D1 + t160
      t166 = t160 + 0.1D1
      t169 = Sqrt(t163 * x2 * t164 * t166)
      t171 = 0.2D1 * t161 * t169
      t174 = 0.1D1 / t162
      t178 = x2 * x1
      t179 = t178 * z
      t180 = 0.1D1 - x1 + t158 - x2 + t178 - t179 - x3 + t154 - t155 + t
     #156 - t157 + t159 + t171
      t191 = s * t35 * x2 * t153 * x1 * t174
      t192 = t153 ** 2
      t202 = log(0.4D1 * t106 * t174 * t192 * t8 * t11 * x2 * t4 * t164 
     #* t166)
      t203 = x3 * z
      t204 = z * t28
      t205 = t160 * t169
      t211 = 0.1D1 + t171 + t178 + t203 + t154 - 0.2D1 * t204 * t205 + 0
     #.2D1 * t204 * t205 * x1 + t156 + t16 - x1 - x2 - x3
      t212 = x3 * t10
      t216 = 0.2D1 * t68 * z
      t218 = t169 * x1
      t224 = t10 * x2
      t227 = t158 + t212 * x1 + t107 - t179 - 0.2D1 * t155 - t216 - 0.3D
     #1 * t157 - 0.2D1 * t161 * t218 - t212 * t178 - 0.2D1 * t106 * t16 
     #+ t106 * t224 + 0.4D1 * t159
      t229 = (t211 + t227) ** 2
      t230 = 0.1D1 / t229
      t234 = (-0.1D1 + x1 - t158 + x2 - t178 - t16 + t179) ** 2
      t235 = 0.1D1 / t234
      t238 = t160 * x3
      t239 = t238 * x1
      t242 = t238 * x2
      t245 = t238 * t10
      t247 = t160 * t10
      t249 = t105 * t160
      t251 = t160 * x1
      t254 = t160 * x2
      t259 = 0.2D1 * t242 * z
      t261 = 0.3D1 * t254 * z
      t262 = x3 * t28
      t267 = t28 * t169
      t268 = t267 * x1
      t272 = 0.2D1 * t242
      t273 = 0.3D1 * t254
      t274 = -0.2D1 * t239 * z - 0.3D1 * t242 * x1 + t245 * x1 - t247 * 
     #x1 - t249 * x2 + 0.3D1 * t251 * z + 0.4D1 * t254 * x1 + t242 * t10
     #5 - t259 + t261 + 0.2D1 * t262 * t169 + 0.2D1 * t204 * t169 + 0.2D
     #1 * t268 + 0.2D1 * t203 * t268 - t238 + t272 - t273
      t275 = t160 * z
      t277 = t238 * z
      t279 = 0.2D1 * t160
      t299 = -t275 - 0.2D1 * t251 + t277 + t239 - 0.2D1 * t267 + t279 - 
     #t245 * t178 + t245 * t128 - 0.2D1 * t242 * t105 * z + t247 * t178 
     #+ 0.2D1 * t249 * t16 - t249 * t224 - 0.5D1 * t254 * t158 - 0.2D1 *
     # t262 * t218 + 0.4D1 * t242 * t158 - 0.2D1 * t204 * t218 - 0.2D1 *
     # t203 * t267
      t301 = (t274 + t299) ** 2
      t312 = 0.90D2 * t63 * t202 * t230 * t235 * t162 * t153 * t35 * t30
     #1 - t67 * t230 * t235 * t162 * t153 * t35 * t301
      t316 = FJET(XB1, XB2, s, t2 * t153 * (-x3 + t154 - t155 + t156 - t
     #157 + t159 - x2 + t171) * t174, t2 * t154, -t2 * t153 * t180 * t17
     #4, -t2 * x1 * t164 * t166, -t191, t312 * t118 * t92 / 0.15D2)
      t322 = t31 * t164 * t166
      t323 = Sqrt(t322)
      t325 = 0.2D1 * t161 * t323
      t334 = log(0.4D1 * x3 * t8 * t11 * t322)
      t336 = 0.1D1 / t18
      t342 = (-t156 + x2 - t203 - 0.1D1 - t325 + t216 - t16 + 0.2D1 * t2
     #04 * t160 * t323 + x3) ** 2
      t343 = 0.1D1 / t342
      t348 = t28 * t323
      t352 = t259 - 0.2D1 * t204 * t323 - 0.2D1 * t262 * t323 - t261 - t
     #272 + 0.2D1 * t348 + t273 + t275 - t277 - t279 + t238 + 0.2D1 * t2
     #03 * t348
      t353 = t352 ** 2
      t355 = t343 * t353 * t35
      t357 = t334 ** 2
      t361 = t336 * t343 * t353 * t35
      t372 = log(0.4D1 * t106 * t69 * t322)
      t382 = (t67 * t334 * t336 * t355 - 0.45D2 * t63 * t357 * t361 - t8
     #7 * t336 * t355) * t92 / 0.30D2 + (0.90D2 * t63 * t372 * t361 - t6
     #7 * t336 * t355) * t118 * t92 / 0.15D2
      t383 = FJET(XB1, XB2, s, -t2 * (-x3 + t156 - x2 + t325), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t156 + t325), 0.0D0, 0.0D0, t382)
      t396 = t11 * t174 * t192 * t4
      t399 = log(-0.4D1 * t68 * t105 * t8 * t396)
      t400 = t234 ** 2
      t401 = 0.1D1 / t400
      t405 = Sqrt(-t163 * x2)
      t406 = t405 ** 2
      t409 = t192 * t153
      t427 = log(-0.4D1 * t128 * t8 * t396)
      t431 = t406 * t162 * t409
      t434 = t427 ** 2
      t445 = (-0.360D3 * t63 * t399 * t401 * t36 * t406 * t29 * t162 * t
     #409 + 0.4D1 * t67 * t401 * t36 * t406 * t29 * t162 * t409) * t118 
     #* t92 / 0.15D2 - (0.4D1 * t127 * t427 * t401 * t431 - 0.180D3 * t1
     #25 * t434 * t401 * t431 - 0.4D1 * t143 * t401 * t431) * t118 / 0.1
     #5D2
      t446 = FJET(XB1, XB2, s, -t2 * t153 * x2 * t174, 0.0D0, t4 * s * t
     #1 * t153, t2 * x1, -t191, t445)
      bggbH9n1e1 = t151 * t150 + t316 * t312 * t118 * t92 / 0.15D2 + t38
     #3 * t382 + t446 * t445

      end function



      doubleprecision function bggbH9n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t19 = x2 * z
      t21 = (0.1D1 - x2 + t19) ** 2
      t22 = t21 ** 2
      t23 = 0.1D1 / t22
      t24 = cos(t8)
      t25 = t24 ** 2
      t26 = t23 * t25
      t27 = t4 * x2
      t28 = Sqrt(-t27)
      t29 = t28 ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t39 = -0.180D3 * t6 * lh - 0.90D2 * t6
      t46 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t51 = t50 * t46
      t55 = wd * t25
      t57 = x1 ** 2
      t58 = x2 * t57
      t61 = log(-0.4D1 * t58 * t14)
      t67 = t31 * z
      t68 = t67 * lh
      t71 = t55 * t67
      t73 = -0.180D3 * t55 * t68 - 0.90D2 * t71
      t80 = t23 * wd
      t85 = log(-0.4D1 * x2 * t10 * t12 * t4)
      t87 = t85 * t23 * wd
      t94 = lh ** 2
      t96 = 0.3141592653589793D1 ** 2
      t103 = t85 ** 2
      t112 = (-0.360D3 * t6 * t17 * t26 * t32 + 0.4D1 * t39 * t23 * t25 
     #* t29 * t31) * t46 / 0.30D2 + 0.24D2 * t6 * t26 * t32 * t51 - (0.3
     #60D3 * t55 * t31 * z * t61 * t23 * t29 - 0.4D1 * t73 * t23 * t29) 
     #* t50 / 0.15D2 - 0.24D2 * (-t80 - t87) * t25 * t29 * t68 + 0.2D1 /
     # 0.15D2 * t80 * t25 * t32 * z * (0.180D3 * t94 - 0.30D2 * t96) + 0
     #.12D2 * (t87 + t103 * t23 * wd / 0.2D1) * t25 * t32 * z
      t113 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #112)
      t115 = -0.1D1 + x1
      t116 = x1 * x3
      t117 = t116 * z
      t118 = 0.2D1 * t7
      t119 = t7 * x1
      t120 = x1 * z
      t121 = t7 * t120
      t122 = sqrt(x3)
      t123 = t24 * t122
      t124 = 0.1D1 - x1 + t120
      t125 = t4 * t124
      t126 = -0.1D1 + t122
      t128 = t122 + 0.1D1
      t131 = Sqrt(t125 * x2 * t126 * t128)
      t133 = 0.2D1 * t123 * t131
      t136 = 0.1D1 / t124
      t140 = x2 * x1
      t141 = t140 * z
      t142 = 0.1D1 - x1 + t120 - x2 + t140 - t141 - x3 + t116 - t117 + t
     #118 - t119 + t121 + t133
      t153 = s * t30 * x2 * t115 * x1 * t136
      t154 = x3 * z
      t155 = z * t24
      t156 = t122 * t131
      t162 = 0.1D1 + t133 + t140 + t154 + t116 - 0.2D1 * t155 * t156 + 0
     #.2D1 * t155 * t156 * x1 + t118 + t19 - x1 - x2 - x3
      t163 = x3 * t11
      t165 = t57 * x3
      t169 = 0.2D1 * t7 * z
      t171 = t131 * x1
      t177 = t11 * x2
      t180 = t120 + t163 * x1 + t165 * x2 - t141 - 0.2D1 * t117 - t169 -
     # 0.3D1 * t119 - 0.2D1 * t123 * t171 - t163 * t140 - 0.2D1 * t165 *
     # t19 + t165 * t177 + 0.4D1 * t121
      t182 = (t162 + t180) ** 2
      t183 = 0.1D1 / t182
      t185 = (-0.1D1 + x1 - t120 + x2 - t140 - t19 + t141) ** 2
      t186 = 0.1D1 / t185
      t191 = t24 * t131
      t192 = t191 * x1
      t195 = t122 * x3
      t196 = t195 * t11
      t199 = t195 * x2
      t203 = t122 * t11
      t205 = t57 * t122
      t209 = t122 * x2
      t212 = t195 * x1
      t220 = t122 * x1
      t226 = 0.2D1 * t154 * t192 - t195 - t196 * t140 + t196 * t58 - 0.2
     #D1 * t199 * t57 * z + t203 * t140 + 0.2D1 * t205 * t19 - t205 * t1
     #77 - 0.5D1 * t209 * t120 - 0.2D1 * t212 * z - 0.3D1 * t199 * x1 + 
     #t196 * x1 - t203 * x1 - t205 * x2 + 0.3D1 * t220 * z + 0.4D1 * t20
     #9 * x1 + t199 * t57
      t228 = 0.2D1 * t199 * z
      t230 = 0.3D1 * t209 * z
      t231 = x3 * t24
      t237 = 0.2D1 * t199
      t238 = 0.3D1 * t209
      t239 = t122 * z
      t241 = t195 * z
      t243 = 0.2D1 * t122
      t252 = -t228 + t230 + 0.2D1 * t231 * t131 + 0.2D1 * t155 * t131 + 
     #0.2D1 * t192 + t237 - t238 - t239 - 0.2D1 * t220 + t241 + t212 - 0
     #.2D1 * t191 + t243 - 0.2D1 * t231 * t171 + 0.4D1 * t199 * t120 - 0
     #.2D1 * t155 * t171 - 0.2D1 * t154 * t191
      t254 = (t226 + t252) ** 2
      t256 = t254 * t50 * t46
      t260 = FJET(XB1, XB2, s, t2 * t115 * (-x3 + t116 - t117 + t118 - t
     #119 + t121 - x2 + t133) * t136, t2 * t116, -t2 * t115 * t142 * t13
     #6, -t2 * x1 * t126 * t128, -t153, -0.6D1 * t6 * t183 * t186 * t124
     # * t115 * t30 * t256)
      t271 = t27 * t126 * t128
      t272 = Sqrt(t271)
      t274 = 0.2D1 * t123 * t272
      t283 = log(0.4D1 * x3 * t10 * t12 * t271)
      t285 = 0.1D1 / t21
      t290 = (-t118 + x2 - t154 - 0.1D1 - t274 + t169 - t19 + 0.2D1 * t1
     #55 * t122 * t272 + x3) ** 2
      t291 = 0.1D1 / t290
      t292 = t285 * t291
      t297 = t24 * t272
      t301 = t228 - 0.2D1 * t155 * t272 - 0.2D1 * t231 * t272 - t230 - t
     #237 + 0.2D1 * t297 + t238 + t239 - t241 - t243 + t195 + 0.2D1 * t1
     #54 * t297
      t302 = t301 ** 2
      t303 = t302 * t30
      t318 = (0.90D2 * t6 * t283 * t292 * t303 - t39 * t285 * t291 * t30
     #2 * t30) * t46 / 0.30D2 - 0.6D1 * t6 * t292 * t303 * t51
      t319 = FJET(XB1, XB2, s, -t2 * (-x3 + t118 - x2 + t274), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t118 + t274), 0.0D0, 0.0D0, t318)
      t328 = t185 ** 2
      t329 = 0.1D1 / t328
      t332 = Sqrt(-t125 * x2)
      t333 = t332 ** 2
      t337 = t115 ** 2
      t338 = t337 * t115
      t350 = log(-0.4D1 * t58 * t10 * t12 * t136 * t337 * t4)
      t353 = t333 * t124 * t338
      t363 = 0.24D2 * t6 * t329 * t31 * t333 * t25 * t124 * t338 * t50 *
     # t46 - (0.360D3 * t71 * t350 * t329 * t353 - 0.4D1 * t73 * t329 * 
     #t353) * t50 / 0.15D2
      t364 = FJET(XB1, XB2, s, -t2 * t115 * x2 * t136, 0.0D0, t4 * s * t
     #1 * t115, t2 * x1, -t153, t363)
      bggbH9n1e0 = t113 * t112 - 0.6D1 * t260 * wd * z * t183 * t186 * t
     #124 * t115 * t30 * t256 + t319 * t318 + t364 * t363

      end function



      doubleprecision function bggbH9n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x2 * z
      t8 = (0.1D1 - x2 + t6) ** 2
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t11 = t10 * wd
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t21 = log(-0.4D1 * x2 * t14 / t16 * t4)
      t25 = cos(t12)
      t26 = t25 ** 2
      t28 = t4 * x2
      t29 = Sqrt(-t28)
      t30 = t29 ** 2
      t31 = t1 ** 2
      t32 = t31 ** 2
      t33 = t30 * t32
      t37 = wd * z
      t38 = t37 * t10
      t39 = t26 * t30
      t40 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t55 = 0.12D2 * (-t11 - t21 * t10 * wd) * t26 * t33 * z + 0.12D2 * 
     #t38 * t39 * t32 * t40 - 0.24D2 * t11 * t26 * t33 * z * lh + 0.24D2
     # * t38 * t39 * t32 * t50
      t56 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t5
     #5)
      t58 = -0.1D1 + x1
      t60 = x1 * z
      t61 = 0.1D1 - x1 + t60
      t62 = 0.1D1 / t61
      t74 = x2 * x1
      t77 = (-0.1D1 + x1 - t60 + x2 - t74 - t6 + t74 * z) ** 2
      t78 = t77 ** 2
      t79 = 0.1D1 / t78
      t84 = Sqrt(-t4 * t61 * x2)
      t85 = t84 ** 2
      t87 = t58 ** 2
      t91 = t85 * t26 * t61 * t87 * t58 * t50
      t94 = FJET(XB1, XB2, s, -t2 * t58 * x2 * t62, 0.0D0, t4 * s * t1 *
     # t58, t2 * x1, -s * t31 * x2 * t58 * x1 * t62, 0.24D2 * t37 * t79 
     #* t32 * t91)
      t101 = x2 * x3
      t102 = 0.2D1 * t101
      t103 = sqrt(x3)
      t109 = Sqrt(t28 * (-0.1D1 + t103) * (t103 + 0.1D1))
      t111 = 0.2D1 * t25 * t103 * t109
      t116 = 0.1D1 / t8
      t118 = x3 * z
      t121 = z * t25
      t126 = (-t102 + x2 - t118 - 0.1D1 - t111 + 0.2D1 * t101 * z - t6 +
     # 0.2D1 * t121 * t103 * t109 + x3) ** 2
      t128 = t103 * x3
      t129 = t128 * x2
      t137 = t103 * x2
      t141 = t25 * t109
      t149 = 0.2D1 * t129 * z - 0.2D1 * t121 * t109 - 0.2D1 * x3 * t25 *
     # t109 - 0.3D1 * t137 * z - 0.2D1 * t129 + 0.2D1 * t141 + 0.3D1 * t
     #137 + t103 * z - t128 * z - 0.2D1 * t103 + t128 + 0.2D1 * t118 * t
     #141
      t150 = t149 ** 2
      t153 = 0.1D1 / t126 * t150 * t31 * t40
      t156 = FJET(XB1, XB2, s, -t2 * (-x3 + t102 - x2 + t111), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t102 + t111), 0.0D0, 0.0D0, -0.3D1 * t37 * t
     #116 * t153)
      bggbH9n1em1 = t56 * t55 + 0.24D2 * t94 * wd * z * t79 * t32 * t91 
     #- 0.3D1 * t156 * wd * z * t116 * t153

      end function



      doubleprecision function bggbH9n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t9 = (0.1D1 - x2 + x2 * z) ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t14 = cos(x4 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t17 = Sqrt(-t4 * x2)
      t18 = t17 ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t25 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, 0.
     #12D2 * wd * z * t11 * t15 * t18 * t21)
      bggbH9n1em2 = 0.12D2 * t25 * wd * z * t11 * t15 * t18 * t21

      end function



      doubleprecision function bggbH9n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bggbH9n1em3 = 0.0D0

      end function



      doubleprecision function bggbH9n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bggbH9n1em4 = 0.0D0

      end function
