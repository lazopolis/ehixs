  
      subroutine bbbbH8n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH8n1e1  
      doubleprecision bbbbH8n1e0  
      doubleprecision bbbbH8n1em1  
      doubleprecision bbbbH8n1em2  
      doubleprecision bbbbH8n1em3  
      doubleprecision bbbbH8n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH8n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH8n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH8n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH8n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH8n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH8n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH8n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z ** 2
      t7 = 0.1D1 / t6
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t4 * t11
      t15 = log(-0.4D1 * t7 * x2 * t12)
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = x2 * z
      t21 = (0.1D1 + t19 - x2) ** 2
      t22 = t21 ** 2
      t23 = 0.1D1 / t22
      t24 = t18 * t23
      t26 = cos(t9)
      t27 = t26 ** 2
      t28 = x2 * t4
      t29 = Sqrt(-t28)
      t30 = t29 ** 2
      t31 = t27 * t30
      t32 = lh ** 2
      t34 = 0.3141592653589793D1 ** 2
      t36 = 0.180D3 * t32 - 0.30D2 * t34
      t41 = t15 ** 2
      t44 = wd * lh
      t48 = z * t18
      t67 = t48 * t44
      t72 = log(-0.4D1 * x3 * t7 * t28 * t11)
      t77 = t48 * wd
      t78 = t72 ** 2
      t85 = t77 * t36 * t23 * t31
      t88 = 0.1D1 / x3
      t91 = x1 ** 2
      t92 = x3 * t91
      t93 = t92 * x2
      t94 = t12 * t7
      t97 = log(-0.4D1 * t93 * t94)
      t108 = 0.1D1 / x1
      t111 = t27 * wd
      t112 = t48 * t111
      t113 = t91 * x2
      t116 = log(-0.4D1 * t113 * t94)
      t118 = t23 * t30
      t123 = t116 ** 2
      t131 = t15 * z * t24 * t31 * wd * t36 / 0.45D2 + 0.2D1 * t41 * z *
     # t24 * t31 * t44 - t48 * t23 * t31 * wd * (0.60D2 * lh * t34 - 0.2
     #884936567583026D3 - 0.120D3 * t32 * lh) / 0.45D2 + t41 * t15 * z *
     # t18 * t23 * t27 * t30 * wd / 0.3D1 + (-0.720D3 * t67 * t72 * t23 
     #* t31 - 0.180D3 * t77 * t78 * t23 * t31 - 0.4D1 * t85) * t88 / 0.1
     #80D3 + (0.360D3 * t77 * t97 * t23 * t31 + 0.720D3 * t77 * lh * t23
     # * t31) * t88 * t108 / 0.90D2 - 0.2D1 / 0.45D2 * (0.180D3 * t112 *
     # lh * t116 * t118 + 0.45D2 * t48 * t27 * wd * t123 * t118 + t85) *
     # t108
      t132 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t131)
      t134 = x2 * x3
      t135 = 0.2D1 * t134
      t136 = sqrt(x3)
      t137 = t26 * t136
      t138 = -0.1D1 + t136
      t139 = t136 + 0.1D1
      t140 = t138 * t139
      t142 = Sqrt(t28 * t140)
      t144 = 0.2D1 * t137 * t142
      t146 = t2 * (0.1D1 - x2 - x3 + t135 + t144)
      t148 = t2 * (-x3 + t135 - x2 + t144)
      t152 = x2 * t11 * t140
      t155 = log(0.4D1 * x3 * t4 * t7 * t152)
      t156 = 0.1D1 / t21
      t158 = t136 * x2
      t159 = 0.2D1 * t158
      t163 = (-t136 + t159 + 0.2D1 * t26 * t142) ** 2
      t164 = x3 * z
      t166 = 0.2D1 * t134 * z
      t171 = (-t144 - t164 + x3 - t135 + x2 - 0.1D1 + t166 + 0.2D1 * t13
     #7 * t142 * z - t19) ** 2
      t173 = t163 / t171
      t177 = t155 ** 2
      t188 = t4 * t7
      t192 = log(0.4D1 * t92 * t188 * t152)
      t205 = (0.180D3 * t67 * t155 * t156 * t173 + 0.45D2 * t77 * t177 *
     # t156 * t173 + t77 * t36 * t156 * t173) * t88 / 0.180D3 + (-0.90D2
     # * t77 * t192 * t156 * t173 - 0.180D3 * t77 * lh * t156 * t173) * 
     #t88 * t108 / 0.90D2
      t206 = FJET(XB1, XB2, s, 0.0D0, t146, 0.0D0, -t148, 0.0D0, t205)
      t208 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t131)
      t210 = FJET(XB1, XB2, s, 0.0D0, -t148, 0.0D0, t146, 0.0D0, t205)
      t212 = -0.1D1 + x1
      t214 = x1 * z
      t215 = 0.1D1 - x1 + t214
      t216 = 0.1D1 / t215
      t218 = t2 * t212 * x2 * t216
      t219 = t2 * x1
      t222 = t4 * s * t1 * t212
      t227 = s * t17 * x2 * x1 * t212 * t216
      t230 = t212 ** 2
      t231 = t230 * t216
      t232 = t11 * t7 * t231
      t235 = log(-0.4D1 * t92 * t28 * t232)
      t238 = x2 * x1
      t239 = t238 * z
      t241 = (-0.1D1 - t19 - t238 + x1 + x2 - t214 + t239) ** 2
      t242 = t241 ** 2
      t243 = 0.1D1 / t242
      t245 = t230 * t212
      t247 = t4 * t215
      t249 = Sqrt(-t247 * x2)
      t250 = t249 ** 2
      t252 = t243 * t215 * t245 * t27 * t250
      t266 = log(-0.4D1 * t113 * t4 * t232)
      t268 = t215 * t245
      t269 = t268 * t250
      t273 = t266 ** 2
      t284 = (0.360D3 * t48 * wd * t235 * t252 + 0.720D3 * t67 * t252) *
     # t88 * t108 / 0.90D2 - 0.2D1 / 0.45D2 * (0.180D3 * t48 * t111 * lh
     # * t266 * t243 * t269 + 0.45D2 * t112 * t273 * t243 * t269 + t112 
     #* t36 * t243 * t269) * t108
      t285 = FJET(XB1, XB2, s, 0.0D0, -t218, t219, t222, -t227, t284)
      t287 = FJET(XB1, XB2, s, t219, t222, 0.0D0, -t218, -t227, t284)
      t289 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t131)
      t291 = FJET(XB1, XB2, s, t146, 0.0D0, -t148, 0.0D0, 0.0D0, t205)
      t293 = x3 * x1
      t294 = t2 * t293
      t295 = t293 * z
      t296 = t134 * x1
      t297 = t134 * t214
      t299 = x2 * t138 * t139
      t301 = Sqrt(t247 * t299)
      t303 = 0.2D1 * t137 * t301
      t307 = t2 * t212 * (-x3 + t293 - t295 + t135 - t296 + t297 - x2 + 
     #t303) * t216
      t310 = t2 * x1 * t138 * t139
      t311 = 0.1D1 - x1 + t214 - x2 + t238 - t239 - x3 + t293 - t295 + t
     #135 - t296 + t297 + t303
      t314 = t2 * t212 * t311 * t216
      t320 = log(0.4D1 * t188 * t299 * t231 * t92 * t11)
      t324 = t301 * z
      t330 = 0.1D1 - 0.2D1 * t137 * t324 + 0.2D1 * t137 * t324 * x1 + t2
     #93 + t19 + t238 + t164 - x1 - x2 - x3 + t214 + t135
      t332 = x3 * t6
      t344 = 0.4D1 * t297 - t332 * t238 - 0.2D1 * t92 * t19 + t92 * t6 *
     # x2 - t239 - 0.2D1 * t295 - t166 - 0.3D1 * t296 + t332 * x1 + t93 
     #+ t303 - 0.2D1 * t137 * t301 * x1
      t346 = (t330 + t344) ** 2
      t349 = t136 * x1
      t358 = (-t349 * z + 0.2D1 * t158 * t214 - t136 + t349 + t159 - 0.2
     #D1 * t158 * x1 + 0.2D1 * t26 * t301) ** 2
      t360 = 0.1D1 / t241 / t346 * t268 * t358
      t365 = -0.90D2 * t48 * wd * t320 * t360 - 0.180D3 * t67 * t360
      t368 = t365 * t88 * t108 / 0.90D2
      t369 = FJET(XB1, XB2, s, t294, t307, -t310, -t314, -t227, t368)
      t371 = t88 * t108
      t374 = FJET(XB1, XB2, s, t222, t219, -t218, 0.0D0, -t227, t284)
      t376 = FJET(XB1, XB2, s, t307, t294, -t314, -t310, -t227, t368)
      t380 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t131)
      t382 = FJET(XB1, XB2, s, -t148, 0.0D0, t146, 0.0D0, 0.0D0, t205)
      t384 = FJET(XB1, XB2, s, -t310, -t314, t294, t307, -t227, t368)
      t388 = FJET(XB1, XB2, s, -t218, 0.0D0, t222, t219, -t227, t284)
      t390 = FJET(XB1, XB2, s, -t314, -t310, t307, t294, -t227, t368)
      bbbbH8n1e1 = t132 * t131 + t206 * t205 + t208 * t131 + t210 * t205
     # + t285 * t284 + t287 * t284 + t289 * t131 + t291 * t205 + t369 * 
     #t365 * t371 / 0.90D2 + t374 * t284 + t376 * t365 * t371 / 0.90D2 +
     # t380 * t131 + t382 * t205 + t384 * t365 * t371 / 0.90D2 + t388 * 
     #t284 + t390 * t365 * t371 / 0.90D2

      end function



      doubleprecision function bbbbH8n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = t6 ** 2
      t8 = z * t7
      t9 = t8 * wd
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t13 = x2 * t4
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t20 = log(-0.4D1 * x3 * t11 * t13 * t16)
      t21 = x2 * z
      t23 = (0.1D1 + t21 - x2) ** 2
      t24 = t23 ** 2
      t25 = 0.1D1 / t24
      t27 = cos(t14)
      t28 = t27 ** 2
      t29 = Sqrt(-t13)
      t30 = t29 ** 2
      t31 = t28 * t30
      t37 = t9 * lh * t25 * t31
      t40 = 0.1D1 / x3
      t43 = t25 * t28
      t45 = t30 * wd
      t46 = 0.1D1 / x1
      t47 = t40 * t46
      t52 = x1 ** 2
      t53 = t52 * x2
      t54 = t4 * t16
      t58 = log(-0.4D1 * t53 * t54 * t11)
      t71 = log(-0.4D1 * t11 * x2 * t54)
      t75 = wd * lh
      t79 = lh ** 2
      t81 = 0.3141592653589793D1 ** 2
      t88 = t71 ** 2
      t93 = (0.360D3 * t9 * t20 * t25 * t31 + 0.720D3 * t37) * t40 / 0.1
     #80D3 - 0.4D1 * t8 * t43 * t45 * t47 - 0.2D1 / 0.45D2 * (-0.90D2 * 
     #t8 * t28 * wd * t58 * t25 * t30 - 0.180D3 * t37) * t46 - 0.4D1 * t
     #71 * z * t7 * t25 * t31 * t75 - t9 * (0.180D3 * t79 - 0.30D2 * t81
     #) * t25 * t31 / 0.45D2 - t88 * z * t7 * t43 * t45
      t94 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t93)
      t96 = x2 * x3
      t97 = 0.2D1 * t96
      t98 = sqrt(x3)
      t99 = t27 * t98
      t100 = -0.1D1 + t98
      t101 = t98 + 0.1D1
      t102 = t100 * t101
      t104 = Sqrt(t13 * t102)
      t106 = 0.2D1 * t99 * t104
      t108 = t2 * (0.1D1 - x2 - x3 + t97 + t106)
      t110 = t2 * (-x3 + t97 - x2 + t106)
      t111 = 0.1D1 / t23
      t114 = t98 * x2
      t115 = 0.2D1 * t114
      t119 = (-t98 + t115 + 0.2D1 * t27 * t104) ** 2
      t120 = x3 * z
      t122 = 0.2D1 * t96 * z
      t127 = (-t106 - t120 + x3 - t97 + x2 - 0.1D1 + t122 + 0.2D1 * t99 
     #* t104 * z - t21) ** 2
      t129 = t119 / t127
      t138 = log(0.4D1 * x3 * t4 * t11 * x2 * t16 * t102)
      t150 = t8 * wd * t111 * t129 * t47 + (-0.90D2 * t9 * t138 * t111 *
     # t129 - 0.180D3 * t9 * lh * t111 * t129) * t40 / 0.180D3
      t151 = FJET(XB1, XB2, s, 0.0D0, t108, 0.0D0, -t110, 0.0D0, t150)
      t153 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t93)
      t155 = FJET(XB1, XB2, s, 0.0D0, -t110, 0.0D0, t108, 0.0D0, t150)
      t157 = -0.1D1 + x1
      t159 = x1 * z
      t160 = 0.1D1 - x1 + t159
      t161 = 0.1D1 / t160
      t163 = t2 * t157 * x2 * t161
      t164 = t2 * x1
      t167 = t4 * s * t1 * t157
      t172 = s * t6 * x2 * x1 * t157 * t161
      t173 = x2 * x1
      t174 = t173 * z
      t176 = (-0.1D1 - t21 - t173 + x1 + x2 - t159 + t174) ** 2
      t177 = t176 ** 2
      t178 = 0.1D1 / t177
      t182 = t157 ** 2
      t183 = t182 * t157
      t184 = t183 * t28
      t185 = t4 * t160
      t187 = Sqrt(-t185 * x2)
      t188 = t187 ** 2
      t202 = log(-0.4D1 * t53 * t4 * t16 * t11 * t182 * t161)
      t204 = t160 * t183
      t218 = -0.4D1 * t8 * wd * t178 * t160 * t184 * t188 * t40 * t46 - 
     #0.2D1 / 0.45D2 * (-0.90D2 * t8 * t28 * wd * t202 * t178 * t204 * t
     #188 - 0.180D3 * t8 * t75 * t178 * t160 * t184 * t188) * t46
      t219 = FJET(XB1, XB2, s, 0.0D0, -t163, t164, t167, -t172, t218)
      t221 = FJET(XB1, XB2, s, t164, t167, 0.0D0, -t163, -t172, t218)
      t223 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t93)
      t225 = FJET(XB1, XB2, s, t108, 0.0D0, -t110, 0.0D0, 0.0D0, t150)
      t227 = x3 * x1
      t228 = t2 * t227
      t229 = t227 * z
      t230 = t96 * x1
      t231 = t96 * t159
      t235 = Sqrt(t185 * x2 * t100 * t101)
      t237 = 0.2D1 * t99 * t235
      t241 = t2 * t157 * (-x3 + t227 - t229 + t97 - t230 + t231 - x2 + t
     #237) * t161
      t244 = t2 * x1 * t100 * t101
      t245 = 0.1D1 - x1 + t159 - x2 + t173 - t174 - x3 + t227 - t229 + t
     #97 - t230 + t231 + t237
      t248 = t2 * t157 * t245 * t161
      t249 = 0.1D1 / t176
      t251 = t235 * z
      t257 = 0.1D1 - 0.2D1 * t99 * t251 + 0.2D1 * t99 * t251 * x1 + t227
     # + t21 + t173 + t120 - x1 - x2 - x3 + t159 + t97
      t259 = x3 * t10
      t261 = x3 * t52
      t273 = 0.4D1 * t231 - t259 * t173 - 0.2D1 * t261 * t21 + t261 * t1
     #0 * x2 - t174 - 0.2D1 * t229 - t122 - 0.3D1 * t230 + t259 * x1 + t
     #261 * x2 + t237 - 0.2D1 * t99 * t235 * x1
      t275 = (t257 + t273) ** 2
      t276 = 0.1D1 / t275
      t279 = t98 * x1
      t288 = (-t279 * z + 0.2D1 * t114 * t159 - t98 + t279 + t115 - 0.2D
     #1 * t114 * x1 + 0.2D1 * t27 * t235) ** 2
      t290 = t288 * t40 * t46
      t292 = t8 * wd * t249 * t276 * t204 * t290
      t293 = FJET(XB1, XB2, s, t228, t241, -t244, -t248, -t172, t292)
      t296 = t7 * wd * t249
      t300 = t276 * t160 * t183 * t290
      t302 = FJET(XB1, XB2, s, t167, t164, -t163, 0.0D0, -t172, t218)
      t304 = FJET(XB1, XB2, s, t241, t228, -t248, -t244, -t172, t292)
      t308 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t93)
      t310 = FJET(XB1, XB2, s, -t110, 0.0D0, t108, 0.0D0, 0.0D0, t150)
      t312 = FJET(XB1, XB2, s, -t244, -t248, t228, t241, -t172, t292)
      t316 = FJET(XB1, XB2, s, -t163, 0.0D0, t167, t164, -t172, t218)
      t318 = FJET(XB1, XB2, s, -t248, -t244, t241, t228, -t172, t292)
      bbbbH8n1e0 = t93 * t94 + t151 * t150 + t153 * t93 + t155 * t150 + 
     #t219 * t218 + t221 * t218 + t223 * t93 + t225 * t150 + t293 * z * 
     #t296 * t300 + t302 * t218 + t304 * z * t296 * t300 + t308 * t93 + 
     #t310 * t150 + t312 * z * t296 * t300 + t316 * t218 + t318 * z * t2
     #96 * t300

      end function



      doubleprecision function bbbbH8n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = t6 ** 2
      t8 = z * t7
      t9 = t8 * wd
      t10 = x2 * z
      t12 = (0.1D1 + t10 - x2) ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t16 = x4 * 0.3141592653589793D1
      t17 = cos(t16)
      t18 = t17 ** 2
      t19 = x2 * t4
      t20 = Sqrt(-t19)
      t21 = t20 ** 2
      t22 = t18 * t21
      t26 = z ** 2
      t29 = Sin(t16)
      t30 = t29 ** 2
      t34 = log(-0.4D1 / t26 * x2 * t4 * t30)
      t42 = t8 * t14
      t43 = 0.1D1 / x3
      t48 = 0.1D1 / x1
      t53 = 0.4D1 * t9 * lh * t14 * t22 + 0.2D1 * t34 * z * t7 * t14 * t
     #18 * t21 * wd - 0.2D1 * t42 * t22 * wd * t43 - 0.4D1 * t42 * t22 *
     # wd * t48
      t54 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t53)
      t56 = x2 * x3
      t57 = 0.2D1 * t56
      t58 = sqrt(x3)
      t59 = t17 * t58
      t64 = Sqrt(t19 * (-0.1D1 + t58) * (t58 + 0.1D1))
      t66 = 0.2D1 * t59 * t64
      t68 = t2 * (0.1D1 - x2 - x3 + t57 + t66)
      t70 = t2 * (-x3 + t57 - x2 + t66)
      t77 = (-t58 + 0.2D1 * t58 * x2 + 0.2D1 * t17 * t64) ** 2
      t86 = (-t66 - x3 * z + x3 - t57 + x2 - 0.1D1 + 0.2D1 * t56 * z + 0
     #.2D1 * t59 * t64 * z - t10) ** 2
      t89 = 0.1D1 / t12 * t77 / t86 * t43
      t91 = t9 * t89 / 0.2D1
      t92 = FJET(XB1, XB2, s, 0.0D0, t68, 0.0D0, -t70, 0.0D0, t91)
      t94 = t7 * wd
      t98 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t53)
      t100 = FJET(XB1, XB2, s, 0.0D0, -t70, 0.0D0, t68, 0.0D0, t91)
      t105 = -0.1D1 + x1
      t107 = x1 * z
      t108 = 0.1D1 - x1 + t107
      t109 = 0.1D1 / t108
      t111 = t2 * t105 * x2 * t109
      t112 = t2 * x1
      t115 = t4 * s * t1 * t105
      t120 = s * t6 * x2 * x1 * t105 * t109
      t121 = x2 * x1
      t124 = (-0.1D1 - t10 - t121 + x1 + x2 - t107 + t121 * z) ** 2
      t125 = t124 ** 2
      t126 = 0.1D1 / t125
      t129 = t105 ** 2
      t134 = Sqrt(-t4 * t108 * x2)
      t135 = t134 ** 2
      t138 = t108 * t129 * t105 * t18 * t135 * t48
      t140 = 0.4D1 * t8 * wd * t126 * t138
      t141 = FJET(XB1, XB2, s, 0.0D0, -t111, t112, t115, -t120, -t140)
      t143 = t94 * t126
      t147 = FJET(XB1, XB2, s, t112, t115, 0.0D0, -t111, -t120, -t140)
      t152 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t53)
      t154 = FJET(XB1, XB2, s, t68, 0.0D0, -t70, 0.0D0, 0.0D0, t91)
      t159 = FJET(XB1, XB2, s, t115, t112, -t111, 0.0D0, -t120, -t140)
      t164 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t53)
      t166 = FJET(XB1, XB2, s, -t111, 0.0D0, t115, t112, -t120, -t140)
      t171 = FJET(XB1, XB2, s, -t70, 0.0D0, t68, 0.0D0, 0.0D0, t91)
      bbbbH8n1em1 = t54 * t53 + t92 * z * t94 * t89 / 0.2D1 + t98 * t53 
     #+ t100 * z * t94 * t89 / 0.2D1 - 0.4D1 * t141 * z * t143 * t138 - 
     #0.4D1 * t147 * z * t143 * t138 + t152 * t53 + t154 * z * t94 * t89
     # / 0.2D1 - 0.4D1 * t159 * z * t143 * t138 + t164 * t53 - 0.4D1 * t
     #166 * z * t143 * t138 + t171 * z * t94 * t89 / 0.2D1

      end function



      doubleprecision function bbbbH8n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = t6 ** 2
      t11 = (0.1D1 + x2 * z - x2) ** 2
      t12 = t11 ** 2
      t13 = 0.1D1 / t12
      t16 = cos(x4 * 0.3141592653589793D1)
      t17 = t16 ** 2
      t19 = Sqrt(-x2 * t4)
      t20 = t19 ** 2
      t24 = 0.2D1 * z * t7 * t13 * t17 * t20 * wd
      t25 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t24)
      t30 = t13 * t17 * t20 * wd
      t32 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, -t24)
      t36 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t24)
      t40 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, -t24)
      bbbbH8n1em2 = -0.2D1 * t25 * z * t7 * t30 - 0.2D1 * t32 * z * t7 *
     # t30 - 0.2D1 * t36 * z * t7 * t30 - 0.2D1 * t40 * z * t7 * t30

      end function



      doubleprecision function bbbbH8n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbbbH8n1em3 = 0.0D0

      end function



      doubleprecision function bbbbH8n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbbbH8n1em4 = 0.0D0

      end function
