  
      subroutine bbggh8n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh8n1e1  
      doubleprecision bbggh8n1e0  
      doubleprecision bbggh8n1em1  
      doubleprecision bbggh8n1em2  
      doubleprecision bbggh8n1em3  
      doubleprecision bbggh8n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh8n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh8n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh8n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh8n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh8n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh8n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh8n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * lh
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x2 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t17 = log(-0.4D1 * t11 * t13 * t4)
      t18 = t17 * z
      t21 = t1 ** 2
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = z * t30
      t32 = t17 ** 2
      t33 = t32 * z
      t55 = x2 * z
      t57 = (0.1D1 - x2 + t55) ** 2
      t58 = 0.1D1 / t57
      t59 = ((-0.180D3 * t6 - 0.90D2 * t18) * t21 * wd - 0.2D1 * (0.180D
     #3 * t18 * lh + t31 + 0.45D2 * t33) * t21 * wd + (-t18 * t30 - 0.90
     #D2 * t33 * lh + z * (-0.2884936567583026D3 - 0.120D3 * t26 * lh + 
     #0.60D2 * lh * t28) - 0.15D2 * t32 * t17 * z) * t21 * wd) * t58
      t61 = z * t21
      t62 = t61 * wd
      t63 = t21 * wd
      t64 = t6 * t63
      t66 = -0.180D3 * t62 - 0.180D3 * t64
      t67 = t66 * t58
      t68 = x2 * x3
      t69 = t10 * t13
      t70 = t69 * t4
      t73 = log(-0.4D1 * t68 * t70)
      t76 = t73 ** 2
      t83 = 0.90D2 * t62 + 0.360D3 * t64 + t31 * t63
      t84 = t83 * t58
      t86 = 0.1D1 / x3
      t89 = x1 ** 2
      t90 = t68 * t89
      t93 = log(-0.4D1 * t90 * t70)
      t99 = 0.1D1 / x1
      t103 = x2 * t89
      t106 = log(-0.4D1 * t103 * t70)
      t107 = t66 * t106
      t109 = t106 ** 2
      t117 = -t59 / 0.135D3 - (-t67 * t73 + 0.45D2 * t61 * wd * t58 * t7
     #6 + t84) * t86 / 0.135D3 - 0.2D1 / 0.135D3 * (-0.90D2 * t61 * wd *
     # t93 * t58 + t67) * t99 * t86 - 0.2D1 / 0.135D3 * (-t107 * t58 + 0
     #.45D2 * t61 * wd * t109 * t58 + t84) * t99
      t118 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t117)
      t120 = 0.2D1 * t68
      t121 = cos(t8)
      t123 = -0.1D1 + x3
      t124 = x3 * t123
      t126 = Sqrt(x2 * t4 * t124)
      t128 = 0.2D1 * t121 * t126
      t130 = t2 * (0.1D1 - x2 - x3 + t120 + t128)
      t132 = t2 * (-x3 + t120 - x2 + t128)
      t137 = log(0.4D1 * t11 * t13 * t124 * t4)
      t138 = -x2 + t55
      t141 = t137 ** 2
      t142 = t58 * t141
      t154 = log(0.4D1 * t90 * t69 * t4 * t123)
      t155 = t154 * t58
      t164 = -(-t67 * t137 * t138 + 0.45D2 * t62 * t142 * t138 + t84 * t
     #138) * t86 / 0.135D3 - 0.2D1 / 0.135D3 * (-0.90D2 * t62 * t155 * t
     #138 + t67 * t138) * t99 * t86
      t165 = FJET(XB1, XB2, s, 0.0D0, t130, 0.0D0, -t132, 0.0D0, t164)
      t167 = -t138
      t176 = t84 * t167
      t198 = -t59 * t167 / 0.135D3 - (-t67 * t73 * t167 + 0.45D2 * t62 *
     # t58 * t76 * t167 + t176) * t86 / 0.135D3 - 0.2D1 / 0.135D3 * (-0.
     #90D2 * t62 * t93 * t58 * t167 + t67 * t167) * t99 * t86 - 0.2D1 / 
     #0.135D3 * (-t107 * t58 * t167 + 0.45D2 * t62 * t109 * t58 * t167 +
     # t176) * t99
      t199 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t198)
      t218 = -(-t67 * t137 * t123 + 0.45D2 * t62 * t142 * t123 + t84 * t
     #123) * t86 / 0.135D3 - 0.2D1 / 0.135D3 * (-0.90D2 * t62 * t155 * t
     #123 + t67 * t123) * t99 * t86
      t219 = FJET(XB1, XB2, s, 0.0D0, -t132, 0.0D0, t130, 0.0D0, t218)
      t221 = -0.1D1 + x1
      t223 = x1 * z
      t224 = 0.1D1 - x1 + t223
      t225 = 0.1D1 / t224
      t227 = t2 * t221 * x2 * t225
      t228 = t2 * x1
      t231 = t4 * s * t1 * t221
      t236 = s * t21 * x2 * x1 * t221 * t225
      t238 = t68 * t89 * t10
      t239 = t13 * t225
      t240 = t221 ** 2
      t241 = t240 * t4
      t242 = t239 * t241
      t245 = log(-0.4D1 * t238 * t242)
      t246 = x2 * x1
      t247 = t246 * z
      t249 = (-0.1D1 + x1 - t223 + x2 - t246 - t55 + t247) ** 2
      t250 = 0.1D1 / t249
      t251 = t245 * t250
      t252 = t221 * t224
      t256 = t66 * t250
      t264 = log(-0.4D1 * t103 * t10 * t242)
      t265 = t66 * t264
      t266 = t250 * t221
      t269 = t264 ** 2
      t270 = t269 * t250
      t274 = t83 * t250
      t279 = -0.2D1 / 0.135D3 * (-0.90D2 * t62 * t251 * t252 + t256 * t2
     #52) * t99 * t86 - 0.2D1 / 0.135D3 * (-t265 * t266 * t224 + 0.45D2 
     #* t62 * t270 * t252 + t274 * t252) * t99
      t280 = FJET(XB1, XB2, s, 0.0D0, -t227, t228, t231, -t236, t279)
      t282 = -t246 + x2 - t55 + t247
      t283 = t221 * t282
      t287 = t256 * t283
      t300 = -0.2D1 / 0.135D3 * (-0.90D2 * t62 * t251 * t283 + t287) * t
     #99 * t86 - 0.2D1 / 0.135D3 * (-t265 * t266 * t282 + 0.45D2 * t62 *
     # t270 * t283 + t274 * t283) * t99
      t301 = FJET(XB1, XB2, s, t228, t231, 0.0D0, -t227, -t236, t300)
      t303 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t117)
      t305 = FJET(XB1, XB2, s, t130, 0.0D0, -t132, 0.0D0, 0.0D0, t164)
      t307 = x1 * x3
      t308 = t2 * t307
      t309 = t307 * z
      t310 = t68 * x1
      t311 = t68 * t223
      t316 = Sqrt(x3 * t4 * t224 * x2 * t123)
      t318 = 0.2D1 * t121 * t316
      t322 = t2 * t221 * (-x3 + t307 - t309 + t120 - t310 + t311 - x2 + 
     #t318) * t225
      t325 = t123 * s * t1 * x1
      t326 = 0.1D1 - x1 + t223 - x2 + t246 - t247 - x3 + t307 - t309 + t
     #120 - t310 + t311 + t318
      t329 = t2 * t221 * t326 * t225
      t334 = log(0.4D1 * t238 * t239 * t241 * t123)
      t335 = t334 * t250
      t337 = t221 * (t307 - x3 - t309 + 0.1D1 - x1 + t223)
      t342 = 0.90D2 * t62 * t335 * t337 - t256 * t337
      t345 = 0.2D1 / 0.135D3 * t342 * t99 * t86
      t346 = FJET(XB1, XB2, s, t308, t322, -t325, -t329, -t236, -t345)
      t348 = t99 * t86
      t351 = FJET(XB1, XB2, s, t231, t228, -t227, 0.0D0, -t236, t300)
      t353 = FJET(XB1, XB2, s, t322, t308, -t329, -t325, -t236, -t345)
      t357 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t198)
      t359 = FJET(XB1, XB2, s, -t132, 0.0D0, t130, 0.0D0, 0.0D0, t218)
      t364 = 0.90D2 * t62 * t335 * t283 - t287
      t367 = 0.2D1 / 0.135D3 * t364 * t99 * t86
      t368 = FJET(XB1, XB2, s, -t325, -t329, t308, t322, -t236, -t367)
      t372 = FJET(XB1, XB2, s, -t227, 0.0D0, t231, t228, -t236, t279)
      t374 = FJET(XB1, XB2, s, -t329, -t325, t322, t308, -t236, -t367)
      bbggh8n1e1 = t118 * t117 + t165 * t164 + t199 * t198 + t219 * t218
     # + t280 * t279 + t301 * t300 + t303 * t117 + t305 * t164 - 0.2D1 /
     # 0.135D3 * t346 * t342 * t348 + t351 * t300 - 0.2D1 / 0.135D3 * t3
     #53 * t342 * t348 + t357 * t198 + t359 * t218 - 0.2D1 / 0.135D3 * t
     #368 * t364 * t348 + t372 * t279 - 0.2D1 / 0.135D3 * t374 * t364 * 
     #t348

      end function



      doubleprecision function bbggh8n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = x2 * z
      t10 = (0.1D1 - x2 + t8) ** 2
      t11 = 0.1D1 / t10
      t13 = x2 * x3
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t16 * t18 * t4
      t23 = log(-0.4D1 * t13 * t20)
      t27 = t7 * wd
      t28 = z * lh
      t29 = t6 * wd
      t32 = -0.180D3 * t27 - 0.180D3 * t28 * t29
      t33 = t32 * t11
      t35 = 0.1D1 / x3
      t38 = 0.1D1 / x1
      t43 = x1 ** 2
      t44 = x2 * t43
      t47 = log(-0.4D1 * t44 * t20)
      t57 = x2 * t16
      t61 = log(-0.4D1 * t57 * t18 * t4)
      t62 = t61 * z
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t76 = t61 ** 2
      t83 = (0.90D2 * t27 - 0.2D1 * (-0.180D3 * t28 - 0.90D2 * t62) * t6
     # * wd + (0.180D3 * t62 * lh + z * (0.180D3 * t70 - 0.30D2 * t72) +
     # 0.45D2 * t76 * z) * t6 * wd) * t11
      t85 = -(-0.90D2 * t7 * wd * t11 * t23 + t33) * t35 / 0.135D3 - 0.4
     #D1 / 0.3D1 * t27 * t11 * t38 * t35 - 0.2D1 / 0.135D3 * (-0.90D2 * 
     #t7 * wd * t47 * t11 + t33) * t38 - t83 / 0.135D3
      t86 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t85)
      t88 = 0.2D1 * t13
      t89 = cos(t14)
      t91 = -0.1D1 + x3
      t92 = x3 * t91
      t94 = Sqrt(x2 * t4 * t92)
      t96 = 0.2D1 * t89 * t94
      t98 = t2 * (0.1D1 - x2 - x3 + t88 + t96)
      t100 = t2 * (-x3 + t88 - x2 + t96)
      t105 = log(0.4D1 * t57 * t18 * t92 * t4)
      t106 = t11 * t105
      t107 = -x2 + t8
      t116 = t38 * t35
      t120 = -(-0.90D2 * t27 * t106 * t107 + t33 * t107) * t35 / 0.135D3
     # - 0.4D1 / 0.3D1 * t27 * t11 * t107 * t116
      t121 = FJET(XB1, XB2, s, 0.0D0, t98, 0.0D0, -t100, 0.0D0, t120)
      t124 = -t107
      t128 = t33 * t124
      t145 = -(-0.90D2 * t27 * t11 * t23 * t124 + t128) * t35 / 0.135D3 
     #- 0.4D1 / 0.3D1 * t27 * t11 * t124 * t116 - 0.2D1 / 0.135D3 * (-0.
     #90D2 * t27 * t47 * t11 * t124 + t128) * t38 - t83 * t124 / 0.135D3
      t146 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t145)
      t159 = -(-0.90D2 * t27 * t106 * t91 + t33 * t91) * t35 / 0.135D3 -
     # 0.4D1 / 0.3D1 * t27 * t11 * t91 * t116
      t160 = FJET(XB1, XB2, s, 0.0D0, -t100, 0.0D0, t98, 0.0D0, t159)
      t162 = -0.1D1 + x1
      t164 = x1 * z
      t165 = 0.1D1 - x1 + t164
      t166 = 0.1D1 / t165
      t168 = t2 * t162 * x2 * t166
      t169 = t2 * x1
      t172 = t4 * s * t1 * t162
      t177 = s * t6 * x2 * x1 * t162 * t166
      t178 = x2 * x1
      t179 = t178 * z
      t181 = (-0.1D1 + x1 - t164 + x2 - t178 - t8 + t179) ** 2
      t182 = 0.1D1 / t181
      t184 = t7 * wd * t182
      t185 = t162 * t165
      t191 = t162 ** 2
      t196 = log(-0.4D1 * t44 * t16 * t18 * t166 * t191 * t4)
      t197 = t196 * t182
      t201 = t32 * t182
      t206 = -0.4D1 / 0.3D1 * t184 * t185 * t116 - 0.2D1 / 0.135D3 * (-0
     #.90D2 * t27 * t197 * t185 + t201 * t185) * t38
      t207 = FJET(XB1, XB2, s, 0.0D0, -t168, t169, t172, -t177, t206)
      t209 = -t178 + x2 - t8 + t179
      t210 = t162 * t209
      t213 = 0.4D1 / 0.3D1 * t184 * t210 * t116
      t221 = -t213 - 0.2D1 / 0.135D3 * (-0.90D2 * t27 * t197 * t210 + t2
     #01 * t210) * t38
      t222 = FJET(XB1, XB2, s, t169, t172, 0.0D0, -t168, -t177, t221)
      t224 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t85)
      t226 = FJET(XB1, XB2, s, t98, 0.0D0, -t100, 0.0D0, 0.0D0, t120)
      t228 = x1 * x3
      t229 = t2 * t228
      t230 = t228 * z
      t231 = t13 * x1
      t232 = t13 * t164
      t237 = Sqrt(x3 * t4 * t165 * x2 * t91)
      t239 = 0.2D1 * t89 * t237
      t243 = t2 * t162 * (-x3 + t228 - t230 + t88 - t231 + t232 - x2 + t
     #239) * t166
      t246 = t91 * s * t1 * x1
      t247 = 0.1D1 - x1 + t164 - x2 + t178 - t179 - x3 + t228 - t230 + t
     #88 - t231 + t232 + t239
      t250 = t2 * t162 * t247 * t166
      t251 = t228 - x3 - t230 + 0.1D1 - x1 + t164
      t255 = 0.4D1 / 0.3D1 * t184 * t162 * t251 * t116
      t256 = FJET(XB1, XB2, s, t229, t243, -t246, -t250, -t177, t255)
      t259 = t182 * t162
      t262 = t259 * t251 * t38 * t35
      t265 = FJET(XB1, XB2, s, t172, t169, -t168, 0.0D0, -t177, t221)
      t267 = FJET(XB1, XB2, s, t243, t229, -t250, -t246, -t177, t255)
      t272 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t145)
      t274 = FJET(XB1, XB2, s, -t100, 0.0D0, t98, 0.0D0, 0.0D0, t159)
      t276 = FJET(XB1, XB2, s, -t246, -t250, t229, t243, -t177, t213)
      t281 = t259 * t209 * t38 * t35
      t284 = FJET(XB1, XB2, s, -t168, 0.0D0, t172, t169, -t177, t206)
      t286 = FJET(XB1, XB2, s, -t250, -t246, t243, t229, -t177, t213)
      bbggh8n1e0 = t86 * t85 + t121 * t120 + t146 * t145 + t160 * t159 +
     # t207 * t206 + t222 * t221 + t224 * t85 + t226 * t120 + 0.4D1 / 0.
     #3D1 * t256 * z * t29 * t262 + t265 * t221 + 0.4D1 / 0.3D1 * t267 *
     # z * t29 * t262 + t272 * t145 + t274 * t159 + 0.4D1 / 0.3D1 * t276
     # * z * t29 * t281 + t284 * t206 + 0.4D1 / 0.3D1 * t286 * z * t29 *
     # t281

      end function



      doubleprecision function bbggh8n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = t7 * wd
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t21 = log(-0.4D1 * x2 * t14 / t16 * t4)
      t28 = x2 * z
      t30 = (0.1D1 - x2 + t28) ** 2
      t31 = 0.1D1 / t30
      t32 = (-0.180D3 * t8 + (-0.180D3 * z * lh - 0.90D2 * z * t21) * t6
     # * wd) * t31
      t34 = wd * t31
      t35 = 0.1D1 / x3
      t39 = 0.1D1 / x1
      t43 = -t32 / 0.135D3 - 0.2D1 / 0.3D1 * t7 * t34 * t35 - 0.4D1 / 0.
     #3D1 * t7 * t34 * t39
      t44 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t43)
      t47 = 0.2D1 * x2 * x3
      t48 = cos(t12)
      t50 = -0.1D1 + x3
      t53 = Sqrt(x2 * t4 * x3 * t50)
      t55 = 0.2D1 * t48 * t53
      t57 = t2 * (0.1D1 - x2 - x3 + t47 + t55)
      t59 = t2 * (-x3 + t47 - x2 + t55)
      t60 = -x2 + t28
      t64 = 0.2D1 / 0.3D1 * t8 * t31 * t60 * t35
      t65 = FJET(XB1, XB2, s, 0.0D0, t57, 0.0D0, -t59, 0.0D0, -t64)
      t69 = t34 * t60 * t35
      t72 = -t60
      t75 = t31 * t72
      t82 = -t32 * t72 / 0.135D3 - 0.2D1 / 0.3D1 * t8 * t75 * t35 - 0.4D
     #1 / 0.3D1 * t8 * t75 * t39
      t83 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t82)
      t88 = 0.2D1 / 0.3D1 * t8 * t31 * t50 * t35
      t89 = FJET(XB1, XB2, s, 0.0D0, -t59, 0.0D0, t57, 0.0D0, -t88)
      t93 = t34 * t50 * t35
      t96 = -0.1D1 + x1
      t98 = x1 * z
      t99 = 0.1D1 - x1 + t98
      t100 = 0.1D1 / t99
      t102 = t2 * t96 * x2 * t100
      t103 = t2 * x1
      t106 = t4 * s * t1 * t96
      t111 = s * t6 * x2 * x1 * t96 * t100
      t112 = x2 * x1
      t113 = t112 * z
      t115 = (-0.1D1 + x1 - t98 + x2 - t112 - t28 + t113) ** 2
      t117 = 0.1D1 / t115 * t96
      t119 = t117 * t99 * t39
      t121 = 0.4D1 / 0.3D1 * t8 * t119
      t122 = FJET(XB1, XB2, s, 0.0D0, -t102, t103, t106, -t111, -t121)
      t124 = t6 * wd
      t130 = t117 * (-t112 + x2 - t28 + t113) * t39
      t132 = 0.4D1 / 0.3D1 * t8 * t130
      t133 = FJET(XB1, XB2, s, t103, t106, 0.0D0, -t102, -t111, -t132)
      t138 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t43)
      t140 = FJET(XB1, XB2, s, t57, 0.0D0, -t59, 0.0D0, 0.0D0, -t64)
      t145 = FJET(XB1, XB2, s, t106, t103, -t102, 0.0D0, -t111, -t132)
      t150 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t82)
      t152 = FJET(XB1, XB2, s, -t59, 0.0D0, t57, 0.0D0, 0.0D0, -t88)
      t157 = FJET(XB1, XB2, s, -t102, 0.0D0, t106, t103, -t111, -t121)
      bbggh8n1em1 = t44 * t43 - 0.2D1 / 0.3D1 * t65 * z * t6 * t69 + t83
     # * t82 - 0.2D1 / 0.3D1 * t89 * z * t6 * t93 - 0.4D1 / 0.3D1 * t122
     # * z * t124 * t119 - 0.4D1 / 0.3D1 * t133 * z * t124 * t130 + t138
     # * t43 - 0.2D1 / 0.3D1 * t140 * z * t6 * t69 - 0.4D1 / 0.3D1 * t14
     #5 * z * t124 * t130 + t150 * t82 - 0.2D1 / 0.3D1 * t152 * z * t6 *
     # t93 - 0.4D1 / 0.3D1 * t157 * z * t124 * t119

      end function



      doubleprecision function bbggh8n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t5 = t2 * (-0.1D1 + x2)
      t6 = t1 ** 2
      t7 = z * t6
      t8 = x2 * z
      t10 = (0.1D1 - x2 + t8) ** 2
      t11 = 0.1D1 / t10
      t12 = wd * t11
      t14 = 0.2D1 / 0.3D1 * t7 * t12
      t15 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t14)
      t18 = t6 * wd * t11
      t21 = t12 * (x2 - t8)
      t23 = 0.2D1 / 0.3D1 * t7 * t21
      t24 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, -t23)
      t28 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t14)
      t31 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, -t23)
      bbggh8n1em2 = -0.2D1 / 0.3D1 * t15 * z * t18 - 0.2D1 / 0.3D1 * t24
     # * z * t6 * t21 - 0.2D1 / 0.3D1 * t28 * z * t18 - 0.2D1 / 0.3D1 * 
     #t31 * z * t6 * t21

      end function



      doubleprecision function bbggh8n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh8n1em3 = 0.0D0

      end function



      doubleprecision function bbggh8n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh8n1em4 = 0.0D0

      end function
