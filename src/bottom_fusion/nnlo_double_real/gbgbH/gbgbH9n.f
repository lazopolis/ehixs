  
      subroutine gbgbH9n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH9n1e1  
      doubleprecision gbgbH9n1e0  
      doubleprecision gbgbH9n1em1  
      doubleprecision gbgbH9n1em2  
      doubleprecision gbgbH9n1em3  
      doubleprecision gbgbH9n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH9n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH9n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH9n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH9n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH9n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH9n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH9n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = -0.1D1 + z
      t4 = -0.1D1 + x2
      t5 = t4 * s
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t16 = log(-0.4D1 * x2 * t9 * t12 * t4)
      t17 = cos(t7)
      t18 = t17 ** 2
      t20 = t4 * x2
      t21 = Sqrt(-t20)
      t22 = t21 ** 2
      t23 = t16 * t18 * t22
      t24 = t16 ** 2
      t27 = t24 * t18 * t22 / 0.2D1
      t29 = t2 ** 2
      t30 = t29 ** 2
      t33 = x2 * z
      t35 = (0.1D1 + t33 - x2) ** 2
      t36 = t35 ** 2
      t37 = 0.1D1 / t36
      t38 = t37 * z
      t48 = wd * t37
      t52 = t18 * t22
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t60 = 0.180D3 * t56 - 0.30D2 * t58
      t64 = wd * z
      t65 = t64 * lh
      t66 = 0.180D3 * t65
      t68 = -t66 - 0.90D2 * t64
      t69 = x2 * x3
      t70 = t9 * t12
      t71 = t70 * t4
      t74 = log(-0.4D1 * t69 * t71)
      t78 = t22 * t30 * t37
      t81 = t74 ** 2
      t84 = t52 * t30 * t37
      t87 = t64 * t60
      t88 = t66 + t87
      t93 = 0.1D1 / x3
      t106 = x1 ** 2
      t107 = t106 * x3
      t108 = t107 * x2
      t111 = log(-0.4D1 * t108 * t71)
      t119 = 0.1D1 / x1
      t123 = t18 * t30
      t125 = 0.180D3 * t123 * t65
      t126 = t123 * t64
      t128 = -t125 - 0.90D2 * t126
      t129 = x2 * t106
      t132 = log(-0.4D1 * t129 * t71)
      t134 = t22 * t37
      t138 = t132 ** 2
      t144 = t125 + t123 * t87
      t151 = -0.24D2 * (t23 + t27) * t30 * wd * t38 * lh + 0.12D2 * (-t2
     #7 - t24 * t16 * t18 * t22 / 0.6D1) * t30 * t48 * z + 0.2D1 / 0.15D
     #2 * (-t52 - t23) * t30 * wd * t38 * t60 - (0.4D1 * t68 * t74 * t18
     # * t78 - 0.180D3 * t64 * t81 * t84 - 0.4D1 * t88 * t18 * t78) * t9
     #3 / 0.30D2 + 0.2D1 / 0.15D2 * t52 * t30 * t48 * z * (-0.2884936567
     #583026D3 - 0.120D3 * t56 * lh + 0.60D2 * lh * t58) - (0.360D3 * t6
     #4 * t111 * t84 - 0.4D1 * t68 * t18 * t78) * t119 * t93 / 0.15D2 - 
     #(0.4D1 * t128 * t132 * t134 - 0.180D3 * t123 * wd * z * t138 * t13
     #4 - 0.4D1 * t144 * t22 * t37) * t119 / 0.15D2
      t152 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t2, 0.0D0, -t5 * t2, 0.0D
     #0, t151)
      t154 = s * t2
      t155 = 0.2D1 * t69
      t156 = sqrt(x3)
      t157 = t17 * t156
      t158 = -0.1D1 + t156
      t159 = t156 + 0.1D1
      t161 = t20 * t158 * t159
      t162 = Sqrt(t161)
      t164 = 0.2D1 * t157 * t162
      t173 = log(0.4D1 * x3 * t9 * t12 * t161)
      t175 = t156 * x3
      t176 = t156 * x2
      t177 = 0.3D1 * t176
      t178 = t175 * x2
      t179 = 0.2D1 * t178
      t180 = t17 * t162
      t182 = x3 * z
      t186 = 0.3D1 * t176 * z
      t188 = 0.2D1 * t178 * z
      t189 = x3 * t17
      t192 = z * t17
      t195 = 0.2D1 * t156
      t196 = t156 * z
      t197 = t175 * z
      t198 = t175 + t177 - t179 + 0.2D1 * t180 + 0.2D1 * t182 * t180 - t
     #186 + t188 - 0.2D1 * t189 * t162 - 0.2D1 * t192 * t162 - t195 + t1
     #96 - t197
      t199 = t198 ** 2
      t205 = 0.2D1 * t69 * z
      t207 = (-t182 - t155 + x2 + 0.2D1 * t192 * t156 * t162 - 0.1D1 + t
     #205 - t164 + x3 - t33) ** 2
      t208 = 0.1D1 / t207
      t210 = 0.1D1 / t35
      t211 = t29 * t208 * t210
      t213 = t173 ** 2
      t217 = t199 * t29 * t208 * t210
      t228 = log(0.4D1 * t107 * t70 * t161)
      t238 = -(-t68 * t173 * t199 * t211 + 0.45D2 * t64 * t213 * t217 + 
     #t88 * t199 * t211) * t93 / 0.30D2 - (-0.90D2 * t64 * t228 * t217 +
     # t68 * t199 * t211) * t119 * t93 / 0.15D2
      t239 = FJET(XB1, XB2, s, 0.0D0, -t154 * (-x3 + t155 - x2 + t164), 
     #0.0D0, t154 * (0.1D1 - x2 - x3 + t155 + t164), 0.0D0, t238)
      t241 = -0.1D1 + x1
      t243 = x1 * z
      t244 = 0.1D1 - x1 + t243
      t245 = 0.1D1 / t244
      t255 = s * t29 * x2 * t241 * x1 * t245
      t259 = t241 ** 2
      t261 = t12 * t245 * t259 * t4
      t264 = log(-0.4D1 * t69 * t106 * t9 * t261)
      t267 = t259 * t241
      t269 = x2 * x1
      t270 = t269 * z
      t272 = (-0.1D1 + x2 - t269 - t33 + t270 - t243 + x1) ** 2
      t273 = t272 ** 2
      t274 = 0.1D1 / t273
      t275 = t4 * t244
      t277 = Sqrt(-t275 * x2)
      t278 = t277 ** 2
      t280 = t274 * t278 * t244
      t297 = log(-0.4D1 * t129 * t9 * t261)
      t302 = t297 ** 2
      t313 = -(0.360D3 * t64 * t264 * t30 * t267 * t18 * t280 - 0.4D1 * 
     #t68 * t30 * t267 * t18 * t274 * t278 * t244) * t119 * t93 / 0.15D2
     # - (0.4D1 * t128 * t297 * t267 * t280 - 0.180D3 * t126 * t302 * t2
     #67 * t280 - 0.4D1 * t144 * t267 * t280) * t119 / 0.15D2
      t314 = FJET(XB1, XB2, s, 0.0D0, -t154 * t241 * x2 * t245, t154 * x
     #1, t5 * t2 * t241, -t255, t313)
      t316 = x1 * x3
      t318 = t316 * z
      t319 = t69 * x1
      t320 = t69 * t243
      t324 = Sqrt(t275 * x2 * t158 * t159)
      t326 = 0.2D1 * t157 * t324
      t334 = 0.1D1 - x1 + t243 - x2 + t269 - t270 - x3 + t316 - t318 + t
     #155 - t319 + t320 + t326
      t347 = log(0.4D1 * t107 * t245 * t259 * t9 * t12 * x2 * t4 * t158 
     #* t159)
      t350 = 0.1D1 / t272
      t351 = t324 * x1
      t354 = 0.1D1 + t269 + t182 + t316 + t326 - 0.2D1 * t157 * t351 + t
     #155 + t33 - x1 - x2 - x3 + t243
      t355 = t156 * t324
      t361 = x3 * t11
      t368 = t11 * x2
      t371 = -0.2D1 * t192 * t355 + 0.2D1 * t192 * t355 * x1 + t361 * x1
     # + t108 - t270 - 0.2D1 * t318 - t205 - 0.3D1 * t319 - t361 * t269 
     #- 0.2D1 * t107 * t33 + t107 * t368 + 0.4D1 * t320
      t373 = (t354 + t371) ** 2
      t374 = 0.1D1 / t373
      t376 = t17 * t324
      t377 = t376 * x1
      t380 = t156 * t11
      t382 = t106 * t156
      t384 = t156 * x1
      t389 = t175 * x1
      t394 = t175 * t11
      t397 = t186 - t188 + 0.2D1 * t182 * t377 - t177 + t179 - t196 + t1
     #97 + t195 - t175 - t380 * x1 - t382 * x2 + 0.3D1 * t384 * z + 0.4D
     #1 * t176 * x1 - 0.2D1 * t389 * z - 0.3D1 * t178 * x1 + t394 * x1 +
     # t178 * t106
      t424 = 0.2D1 * t189 * t324 + 0.2D1 * t192 * t324 + 0.2D1 * t377 - 
     #0.2D1 * t189 * t351 - 0.2D1 * t192 * t351 + 0.4D1 * t178 * t243 - 
     #0.2D1 * t182 * t376 - 0.5D1 * t176 * t243 + t394 * t129 - 0.2D1 * 
     #t178 * t106 * z - t394 * t269 + t380 * t269 + 0.2D1 * t382 * t33 -
     # t382 * t368 - 0.2D1 * t376 - 0.2D1 * t384 + t389
      t426 = (t397 + t424) ** 2
      t438 = -0.90D2 * t64 * t347 * t29 * t350 * t374 * t426 * t244 * t2
     #41 + t68 * t29 * t350 * t374 * t426 * t244 * t241
      t442 = FJET(XB1, XB2, s, t154 * t316, t154 * t241 * (-x3 + t316 - 
     #t318 + t155 - t319 + t320 - x2 + t326) * t245, -t154 * x1 * t158 *
     # t159, -t154 * t241 * t334 * t245, -t255, -t438 * t119 * t93 / 0.1
     #5D2)
      gbgbH9n1e1 = t152 * t151 + t239 * t238 + t314 * t313 - t442 * t438
     # * t119 * t93 / 0.15D2

      end function



      doubleprecision function gbgbH9n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = -0.1D1 + z
      t4 = -0.1D1 + x2
      t5 = t4 * s
      t7 = wd * z
      t8 = x2 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t15 = t11 * t13 * t4
      t18 = log(-0.4D1 * t8 * t15)
      t20 = cos(t9)
      t21 = t20 ** 2
      t22 = t4 * x2
      t23 = Sqrt(-t22)
      t24 = t23 ** 2
      t25 = t21 * t24
      t26 = t2 ** 2
      t27 = t26 ** 2
      t28 = x2 * z
      t30 = (0.1D1 + t28 - x2) ** 2
      t31 = t30 ** 2
      t32 = 0.1D1 / t31
      t33 = t27 * t32
      t37 = t7 * lh
      t40 = -0.180D3 * t37 - 0.90D2 * t7
      t47 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t52 = t51 * t47
      t56 = t21 * t27
      t58 = x1 ** 2
      t59 = x2 * t58
      t62 = log(-0.4D1 * t59 * t15)
      t70 = t56 * t7
      t72 = -0.180D3 * t56 * t37 - 0.90D2 * t70
      t83 = log(-0.4D1 * x2 * t11 * t13 * t4)
      t85 = t83 * t21 * t24
      t94 = wd * t32
      t95 = lh ** 2
      t97 = 0.3141592653589793D1 ** 2
      t104 = t83 ** 2
      t113 = -(0.360D3 * t7 * t18 * t25 * t33 - 0.4D1 * t40 * t21 * t24 
     #* t27 * t32) * t47 / 0.30D2 + 0.24D2 * t7 * t25 * t33 * t52 - (0.3
     #60D3 * t56 * wd * z * t62 * t24 * t32 - 0.4D1 * t72 * t24 * t32) *
     # t51 / 0.15D2 - 0.24D2 * (-t25 - t85) * t27 * wd * t32 * z * lh + 
     #0.2D1 / 0.15D2 * t25 * t27 * t94 * z * (0.180D3 * t95 - 0.30D2 * t
     #97) + 0.12D2 * (t85 + t104 * t21 * t24 / 0.2D1) * t27 * t94 * z
      t114 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t2, 0.0D0, -t5 * t2, 0.0D
     #0, t113)
      t116 = s * t2
      t117 = 0.2D1 * t8
      t118 = sqrt(x3)
      t119 = t20 * t118
      t120 = -0.1D1 + t118
      t121 = t118 + 0.1D1
      t123 = t22 * t120 * t121
      t124 = Sqrt(t123)
      t126 = 0.2D1 * t119 * t124
      t135 = log(0.4D1 * x3 * t11 * t13 * t123)
      t137 = t118 * x3
      t138 = t118 * x2
      t139 = 0.3D1 * t138
      t140 = t137 * x2
      t141 = 0.2D1 * t140
      t142 = t20 * t124
      t144 = x3 * z
      t148 = 0.3D1 * t138 * z
      t150 = 0.2D1 * t140 * z
      t151 = x3 * t20
      t154 = z * t20
      t157 = 0.2D1 * t118
      t158 = t118 * z
      t159 = t137 * z
      t160 = t137 + t139 - t141 + 0.2D1 * t142 + 0.2D1 * t144 * t142 - t
     #148 + t150 - 0.2D1 * t151 * t124 - 0.2D1 * t154 * t124 - t157 + t1
     #58 - t159
      t161 = t160 ** 2
      t162 = t161 * t26
      t167 = 0.2D1 * t8 * z
      t169 = (-t144 - t117 + x2 + 0.2D1 * t154 * t118 * t124 - 0.1D1 + t
     #167 - t126 + x3 - t28) ** 2
      t170 = 0.1D1 / t169
      t171 = 0.1D1 / t30
      t172 = t170 * t171
      t187 = -(-0.90D2 * t7 * t135 * t162 * t172 + t40 * t161 * t26 * t1
     #70 * t171) * t47 / 0.30D2 - 0.6D1 * t7 * t162 * t172 * t52
      t188 = FJET(XB1, XB2, s, 0.0D0, -t116 * (-x3 + t117 - x2 + t126), 
     #0.0D0, t116 * (0.1D1 - x2 - x3 + t117 + t126), 0.0D0, t187)
      t190 = -0.1D1 + x1
      t192 = x1 * z
      t193 = 0.1D1 - x1 + t192
      t194 = 0.1D1 / t193
      t204 = s * t26 * x2 * t190 * x1 * t194
      t205 = t190 ** 2
      t206 = t205 * t190
      t210 = x2 * x1
      t211 = t210 * z
      t213 = (-0.1D1 + x2 - t210 - t28 + t211 - t192 + x1) ** 2
      t214 = t213 ** 2
      t216 = t4 * t193
      t218 = Sqrt(-t216 * x2)
      t219 = t218 ** 2
      t220 = 0.1D1 / t214 * t219
      t232 = log(-0.4D1 * t59 * t11 * t13 * t194 * t205 * t4)
      t234 = t220 * t193
      t244 = 0.24D2 * t7 * t27 * t206 * t21 * t220 * t193 * t51 * t47 - 
     #(0.360D3 * t70 * t232 * t206 * t234 - 0.4D1 * t72 * t206 * t234) *
     # t51 / 0.15D2
      t245 = FJET(XB1, XB2, s, 0.0D0, -t116 * t190 * x2 * t194, t116 * x
     #1, t5 * t2 * t190, -t204, t244)
      t247 = x1 * x3
      t249 = t247 * z
      t250 = t8 * x1
      t251 = t8 * t192
      t255 = Sqrt(t216 * x2 * t120 * t121)
      t257 = 0.2D1 * t119 * t255
      t265 = 0.1D1 - x1 + t192 - x2 + t210 - t211 - x3 + t247 - t249 + t
     #117 - t250 + t251 + t257
      t269 = 0.1D1 / t213
      t271 = t255 * x1
      t274 = 0.1D1 + t210 + t144 + t247 + t257 - 0.2D1 * t119 * t271 + t
     #117 + t28 - x1 - x2 - x3 + t192
      t275 = t118 * t255
      t281 = x3 * t12
      t283 = t58 * x3
      t290 = t12 * x2
      t293 = -0.2D1 * t154 * t275 + 0.2D1 * t154 * t275 * x1 + t281 * x1
     # + t283 * x2 - t211 - 0.2D1 * t249 - t167 - 0.3D1 * t250 - t281 * 
     #t210 - 0.2D1 * t283 * t28 + t283 * t290 + 0.4D1 * t251
      t295 = (t274 + t293) ** 2
      t296 = 0.1D1 / t295
      t299 = t20 * t255
      t300 = t299 * x1
      t313 = t137 * t12
      t319 = t118 * t12
      t321 = t58 * t118
      t326 = t148 - t150 + t157 - t137 + 0.2D1 * t144 * t300 - 0.2D1 * t
     #151 * t271 - 0.2D1 * t154 * t271 + 0.4D1 * t140 * t192 - 0.2D1 * t
     #144 * t299 - 0.5D1 * t138 * t192 + t313 * t59 - 0.2D1 * t140 * t58
     # * z - t313 * t210 + t319 * t210 + 0.2D1 * t321 * t28 - t321 * t29
     #0 - t319 * x1
      t328 = t118 * x1
      t333 = t137 * x1
      t347 = -t321 * x2 + 0.3D1 * t328 * z + 0.4D1 * t138 * x1 - 0.2D1 *
     # t333 * z - 0.3D1 * t140 * x1 + t313 * x1 + t140 * t58 + 0.2D1 * t
     #151 * t255 + 0.2D1 * t154 * t255 + 0.2D1 * t300 - t139 + t141 - t1
     #58 + t159 - 0.2D1 * t299 - 0.2D1 * t328 + t333
      t349 = (t326 + t347) ** 2
      t352 = t190 * t51 * t47
      t356 = FJET(XB1, XB2, s, t116 * t247, t116 * t190 * (-x3 + t247 - 
     #t249 + t117 - t250 + t251 - x2 + t257) * t194, -t116 * x1 * t120 *
     # t121, -t116 * t190 * t265 * t194, -t204, -0.6D1 * t7 * t26 * t269
     # * t296 * t349 * t193 * t352)
      gbgbH9n1e0 = t114 * t113 + t188 * t187 + t245 * t244 - 0.6D1 * t35
     #6 * wd * z * t26 * t269 * t296 * t349 * t193 * t352

      end function



      doubleprecision function gbgbH9n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = -0.1D1 + z
      t4 = -0.1D1 + x2
      t5 = t4 * s
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t9 = t8 ** 2
      t10 = t4 * x2
      t11 = Sqrt(-t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = Sin(t7)
      t15 = t14 ** 2
      t17 = z ** 2
      t22 = log(-0.4D1 * x2 * t15 / t17 * t4)
      t26 = t2 ** 2
      t27 = t26 ** 2
      t29 = x2 * z
      t31 = (0.1D1 + t29 - x2) ** 2
      t32 = t31 ** 2
      t33 = 0.1D1 / t32
      t34 = wd * t33
      t38 = wd * z
      t39 = t38 * t9
      t40 = t12 * t27
      t41 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t56 = 0.12D2 * (-t13 - t22 * t9 * t12) * t27 * t34 * z + 0.12D2 * 
     #t39 * t40 * t33 * t41 - 0.24D2 * t13 * t27 * t34 * z * lh + 0.24D2
     # * t39 * t40 * t33 * t51
      t57 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t2, 0.0D0, -t5 * t2, 0.0D0
     #, t56)
      t59 = s * t2
      t60 = -0.1D1 + x1
      t62 = x1 * z
      t63 = 0.1D1 - x1 + t62
      t64 = 0.1D1 / t63
      t77 = t60 ** 2
      t79 = x2 * x1
      t82 = (-0.1D1 + x2 - t79 - t29 + t79 * z - t62 + x1) ** 2
      t83 = t82 ** 2
      t88 = Sqrt(-t4 * t63 * x2)
      t89 = t88 ** 2
      t92 = t77 * t60 / t83 * t89 * t63 * t51
      t95 = FJET(XB1, XB2, s, 0.0D0, -t59 * t60 * x2 * t64, t59 * x1, t5
     # * t2 * t60, -s * t26 * x2 * t60 * x1 * t64, 0.24D2 * t9 * t27 * t
     #38 * t92)
      t102 = x2 * x3
      t103 = 0.2D1 * t102
      t104 = sqrt(x3)
      t110 = Sqrt(t10 * (-0.1D1 + t104) * (t104 + 0.1D1))
      t112 = 0.2D1 * t8 * t104 * t110
      t117 = t104 * x3
      t118 = t104 * x2
      t120 = t117 * x2
      t122 = t8 * t110
      t124 = x3 * z
      t134 = t8 * z
      t140 = t117 + 0.3D1 * t118 - 0.2D1 * t120 + 0.2D1 * t122 + 0.2D1 *
     # t124 * t122 - 0.3D1 * t118 * z + 0.2D1 * t120 * z - 0.2D1 * x3 * 
     #t8 * t110 - 0.2D1 * t134 * t110 - 0.2D1 * t104 + t104 * z - t117 *
     # z
      t141 = t140 ** 2
      t149 = (-t124 - t103 + x2 + 0.2D1 * t134 * t104 * t110 - 0.1D1 + 0
     #.2D1 * t102 * z - t112 + x3 - t29) ** 2
      t154 = t26 / t149 / t31 * t41
      t157 = FJET(XB1, XB2, s, 0.0D0, -t59 * (-x3 + t103 - x2 + t112), 0
     #.0D0, t59 * (0.1D1 - x2 - x3 + t103 + t112), 0.0D0, -0.3D1 * t38 *
     # t141 * t154)
      gbgbH9n1em1 = t57 * t56 + 0.24D2 * t95 * t9 * t27 * wd * z * t92 -
     # 0.3D1 * t157 * wd * z * t141 * t154

      end function



      doubleprecision function gbgbH9n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = -0.1D1 + z
      t4 = -0.1D1 + x2
      t9 = cos(x4 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t13 = Sqrt(-t4 * x2)
      t14 = t13 ** 2
      t15 = t2 ** 2
      t16 = t15 ** 2
      t20 = (0.1D1 + x2 * z - x2) ** 2
      t21 = t20 ** 2
      t22 = 0.1D1 / t21
      t26 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t2, 0.0D0, -t4 * s * t2, 0
     #.0D0, 0.12D2 * wd * z * t10 * t14 * t16 * t22)
      gbgbH9n1em2 = 0.12D2 * t26 * wd * z * t10 * t14 * t16 * t22

      end function



      doubleprecision function gbgbH9n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      gbgbH9n1em3 = 0.0D0

      end function



      doubleprecision function gbgbH9n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      gbgbH9n1em4 = 0.0D0

      end function
