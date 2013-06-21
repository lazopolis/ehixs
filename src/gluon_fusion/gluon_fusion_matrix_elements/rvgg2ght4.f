  
      subroutine rvgg2ght4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision rvgg2ght4s1e1  
      doubleprecision rvgg2ght4s1e0  
      doubleprecision rvgg2ght4s1em1  
      doubleprecision rvgg2ght4s1em2  
      doubleprecision rvgg2ght4s1em3  
      doubleprecision rvgg2ght4s1em4  
      doubleprecision rvgg2ght4s2e1  
      doubleprecision rvgg2ght4s2e0  
      doubleprecision rvgg2ght4s2em1  
      doubleprecision rvgg2ght4s2em2  
      doubleprecision rvgg2ght4s2em3  
      doubleprecision rvgg2ght4s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rvgg2ght4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght4s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rvgg2ght4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght4s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rvgg2ght4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght4s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rvgg2ght4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght4s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rvgg2ght4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght4s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rvgg2ght4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght4s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rvgg2ght4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t6 = x1 ** 2
      t7 = 0.1D1 / z
      t9 = log(t6 * t7)
      t10 = t9 * t5
      t11 = -t5 - t10
      t14 = 0.720D3 * lh
      t15 = log(z)
      t16 = 0.360D3 * t15
      t17 = -t14 + t16
      t18 = t17 * wd
      t21 = 0.1D1 / x1
      t27 = 0.3141592653589793D1 ** 2
      t29 = t27 * t15
      t30 = 0.60D2 * t29
      t31 = lh ** 2
      t32 = t15 * t31
      t33 = 0.720D3 * t32
      t34 = t31 * lh
      t35 = 0.480D3 * t34
      t36 = t15 ** 2
      t37 = t36 * t15
      t38 = 0.60D2 * t37
      t39 = t27 * lh
      t40 = 0.120D3 * t39
      t41 = t36 * lh
      t42 = 0.360D3 * t41
      t43 = -t14 + 0.720D3 + t16
      t51 = (0.360D3 * t27 - t30 + t33 - 0.1153974627033210D4 - t35 + t3
     #8 + t40 - t42 - t43 * t27 / 0.2D1 + 0.60D2 * (-0.2D1 * lh + t15) *
     # t27) * wd
      t53 = t9 ** 2
      t55 = t53 * t5 / 0.2D1
      t62 = t10 + t55
      t65 = 0.180D3 * t36
      t66 = t15 * lh
      t67 = 0.720D3 * t66
      t68 = 0.720D3 * t31
      t69 = -0.180D3 * t27 + t65 - t67 + t68
      t70 = t69 * wd
      t86 = t36 ** 2
      t88 = t27 ** 2
      t90 = t31 ** 2
      t105 = log(t7)
      t106 = t105 ** 2
      t108 = 0.720D3 * t15
      t109 = 0.1440D4 * lh
      t110 = 0.60D2 * t27
      t111 = 0.1440D4 + t65 + t108 - t109 - t67 + t68 - t110
      t114 = t106 * t105
      t125 = -t30 + t33 + 0.1440D4 * t31 + 0.1440D4 * t15 + 0.1726025372
     #966790D4 - 0.2880D4 * lh - t35 + 0.360D3 * t36 + t38 - 0.1440D4 * 
     #t66 - 0.120D3 * t27 + t40 - t42
      t136 = t106 ** 2
      t138 = t105 * lh
      t140 = t105 * t15
      t147 = -0.30D2 * t27 * t36 - t106 * t43 + 0.2D1 * t105 * t111 - t1
     #14 * t43 / 0.6D1 + t106 * t111 / 0.2D1 - t105 * t125 + (-t14 + 0.7
     #20D3 + t16 - 0.360D3 * t105) * t27 - (-t105 * t43 + 0.180D3 * t106
     # + 0.1440D4 + t65 + t108 - t109 - t67 + t68 - t110) * t27 / 0.2D1 
     #+ 0.120D3 * t114 + 0.15D2 * t136 - 0.30D2 * (t27 - 0.4D1 * t138 + 
     #0.2D1 * t140 - t106 - t36 + 0.4D1 * t66 - 0.4D1 * t31) * t27
      t149 = 0.1D1 / 0.3141592653589793D1
      t163 = 0.12D2 * lh + 0.3D1 * t27 - 0.12D2 * t31 - 0.3D1 * t36 - t3
     #7 - 0.6D1 * t15 + 0.8D1 * t34 - 0.12D2 * t32 + 0.3D1 * t29 + 0.6D1
     # * t41 - 0.6D1 * t39 + 0.12D2 * t66
      t180 = 0.1323291045055350D2 - 0.12D2 * t138 + 0.6D1 * t140 - 0.12D
     #2 * t140 * lh + 0.6D1 * t105 - 0.3D1 * t106 + t114 + 0.6D1 * t106 
     #* lh - 0.3D1 * t106 * t15 + 0.3D1 * t105 * t36 - 0.3D1 * t105 * t2
     #7 + 0.12D2 * t105 * t31
      t190 = 0.5772156649015329D0 ** 2
      t193 = t190 ** 2
      t199 = 0.4006856343865313D0 + t27 * 0.5772156649015329D0 / 0.12D2 
     #+ t190 * 0.5772156649015329D0 / 0.6D1
      t204 = (t27 / 0.12D2 + t190 / 0.2D1) ** 2
      t218 = (0.3D1 * (0.360D3 * wd * t11 + t18 * t5) * t21 + 0.1440D4 *
     # wd * t5 * t21 + (t51 * t5 + 0.360D3 * wd * (-t55 - t53 * t9 * t5 
     #/ 0.6D1) + t18 * t62 + t70 * t11) * t21 + 0.2D1 * (t18 * t11 + t70
     # * t5 + 0.360D3 * wd * t62) * t21) * t7 / 0.80D2 - (-((0.120D3 * t
     #29 * lh + 0.15D2 * t86 + 0.3D1 * t88 + 0.240D3 * t90 + 0.230794925
     #4066420D4 * lh - 0.1153974627033210D4 * t15 - 0.120D3 * t37 * lh +
     # 0.360D3 * t36 * t31 - 0.480D3 * t15 * t34 - 0.120D3 * t27 * t31 +
     # t147) * t149 * t5 - 0.60D2 * t5 * (t163 + t180) * t149) * 0.31415
     #92653589793D1 + 0.360D3 * t149 * t5 * (0.3141592653589793D1 * (t88
     # / 0.80D2 + 0.8013712687730627D0 * 0.5772156649015329D0 + t27 * t1
     #90 / 0.12D2 + t193 / 0.12D2 - 0.2D1 * 0.5772156649015329D0 * t199 
     #+ t204) - 0.7D1 / 0.360D3 * t88 * 0.3141592653589793D1)) * wd * t7
     # / 0.160D3
      t219 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t218)
      t222 = -0.1D1 + x1
      t224 = t149 * wd
      t225 = x1 * z
      t227 = x1 * t3
      t229 = t3 * z
      t230 = x1 * t229
      t232 = t3 ** 2
      t234 = 0.2D1 * x1 * t232
      t235 = t6 * z
      t237 = t6 * t3
      t239 = t6 * t229
      t242 = 0.3D1 * t6 * t232
      t243 = t6 * x1
      t245 = 0.8D1 * t243 * z
      t247 = 0.12D2 * t243 * t3
      t249 = 0.8D1 * t243 * t229
      t250 = 0.1D1 + 0.8D1 * t225 - 0.12D2 * t227 + 0.8D1 * t230 - t234 
     #- 0.12D2 * t235 + 0.18D2 * t237 - 0.12D2 * t239 + t242 + t245 - t2
     #47 + t249
      t252 = 0.2D1 * t243 * t232
      t253 = t6 ** 2
      t255 = 0.4D1 * t253 * z
      t257 = 0.6D1 * t253 * t3
      t259 = 0.4D1 * t253 * t229
      t260 = t253 * t232
      t261 = 0.2D1 * z
      t262 = 0.2D1 * x1
      t263 = 0.3D1 * t3
      t264 = 0.3D1 * t6
      t265 = 0.2D1 * t243
      t266 = 0.2D1 * t229
      t267 = -t252 - t255 + t257 - t259 + t260 - t261 - t262 + t263 + t2
     #64 - t265 + t253 - t266 + t232
      t268 = t250 + t267
      t269 = t222 ** 2
      t270 = 0.1D1 / t269
      t272 = log(t270 * z)
      t275 = 0.1D1 - x1 + t225
      t276 = -z - x1 + t225
      t279 = log(-t275 * t276 * t270)
      t281 = -0.2D1 * t272 * 0.3141592653589793D1 + t279 * 0.31415926535
     #89793D1
      t283 = 0.11D2 * t225
      t284 = 0.18D2 * t227
      t285 = 0.11D2 * t230
      t286 = 0.15D2 * t235
      t287 = 0.24D2 * t237
      t288 = 0.15D2 * t239
      t289 = -0.1D1 - t283 + t284 - t285 + t234 + t286 - t287 + t288 - t
     #242 - t245 + t247 - t249 + t252
      t290 = log(x1)
      t293 = t255 - t257 + t259 - t260 - 0.2D1 * t290 * t268 + t261 + t2
     #62 - t263 - t264 + t265 - t253 + t266 - t232
      t294 = t289 + t293
      t296 = t268 * t281 - t294 * 0.3141592653589793D1
      t307 = t27 * 0.3141592653589793D1
      t312 = z / t275 / t276
      t313 = polylog(2, -t312)
      t314 = t279 ** 2
      t318 = t272 ** 2
      t320 = -t307 / 0.6D1 + (t313 + t314 / 0.2D1) * 0.3141592653589793D
     #1 - t318 * 0.3141592653589793D1
      t322 = -0.1D1 - t283 + t284 - t285 + t234 + t286 - t287 + t288 - t
     #242 - t245 + t247 - t249
      t324 = t322 - t267
      t326 = t290 ** 2
      t329 = -0.2D1 * t290 * t324 + 0.2D1 * t326 * t268
      t331 = 0.1D1 + t312
      t332 = log(t331)
      t335 = t332 ** 2
      t336 = log(-t312)
      t340 = polylog(3, t331)
      t341 = polylog(3, -t312)
      t364 = t17 * t149
      t368 = t294 * t281 + t268 * t320 - t329 * 0.3141592653589793D1
      t372 = wd * t296
      t383 = 0.3D1 * (0.360D3 * t224 * t296 - t18 * t268) * t21 - 0.1440
     #D4 * wd * t268 * t21 + (-t51 * t268 + 0.360D3 * t224 * (t294 * t32
     #0 + t329 * t281 + t268 * ((-t27 * t332 / 0.6D1 + t335 * t336 / 0.2
     #D1 + t332 * t313 + t340 + t341 - 0.1202056903159594D1 + t279 * t31
     #3 + t314 * t279 / 0.6D1) * 0.3141592653589793D1 - t279 * t307 / 0.
     #6D1 - t318 * t272 * 0.3141592653589793D1 / 0.3D1) - (0.2D1 * t326 
     #* t324 - 0.4D1 / 0.3D1 * t326 * t290 * t268) * 0.3141592653589793D
     #1) + t364 * wd * t368 + t69 * t149 * t372) * t21 + 0.2D1 * (t364 *
     # t372 - t70 * t268 + 0.360D3 * t224 * t368) * t21
      t386 = FJET(XB1, XB2, s, t2 * x1, -t2 * t222, 0.0D0, 0.0D0, 0.0D0,
     # t383 * t7 / 0.80D2)
      rvgg2ght4s1e1 = t219 * t218 + t386 * t383 * t7 / 0.80D2

      end function



      doubleprecision function rvgg2ght4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 ** 2
      t5 = log(z)
      t6 = t5 ** 2
      t9 = lh ** 2
      t20 = 0.1D1 / z
      t21 = log(t20)
      t22 = 0.720D3 * lh
      t23 = 0.360D3 * t5
      t24 = -t22 + 0.720D3 + t23
      t27 = t21 ** 2
      t30 = 0.180D3 * t6
      t33 = t5 * lh
      t34 = 0.720D3 * t33
      t35 = 0.720D3 * t9
      t50 = -0.1153974627033210D4 + 0.360D3 * t3 + 0.60D2 * t6 * t5 - 0.
     #480D3 * t9 * lh + 0.720D3 * t9 * t5 - 0.60D2 * t3 * t5 - 0.360D3 *
     # t6 * lh + 0.120D3 * t3 * lh + 0.2D1 * t21 * t24 + t27 * t24 / 0.2
     #D1 - t21 * (0.1440D4 + t30 + 0.720D3 * t5 - 0.1440D4 * lh - t34 + 
     #t35 - 0.60D2 * t3) - (-t22 + 0.720D3 + t23 - 0.360D3 * t21) * t3 /
     # 0.2D1 - 0.360D3 * t27 - 0.60D2 * t27 * t21 + 0.60D2 * (-0.2D1 * l
     #h + t5 - t21) * t3
      t51 = 0.1D1 / 0.3141592653589793D1
      t53 = z ** 2
      t55 = (t53 - z + 0.1D1) ** 2
      t66 = t3 - 0.4D1 * t21 * lh + 0.2D1 * t21 * t5 - t27 - t6 + 0.4D1 
     #* t33 - 0.4D1 * t9 + 0.4D1 * lh - 0.2D1 * t5 + 0.2D1 * t21 - 0.2D1
      t76 = 0.1D1 / x1
      t79 = x1 ** 2
      t81 = log(t79 * t20)
      t82 = t81 * t55
      t83 = -t55 - t82
      t86 = -t22 + t23
      t87 = t86 * wd
      t95 = (-0.180D3 * t3 + t30 - t34 + t35) * wd
      t97 = t81 ** 2
      t108 = (t50 * t51 * t55 - 0.180D3 * t55 * t66 * t51) * 0.314159265
     #3589793D1 * wd * t20 / 0.160D3 + (0.1080D4 * wd * t55 * t76 + 0.2D
     #1 * (0.360D3 * wd * t83 + t87 * t55) * t76 + (t87 * t83 + t95 * t5
     #5 + 0.360D3 * wd * (t82 + t97 * t55 / 0.2D1)) * t76) * t20 / 0.80D
     #2
      t109 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t112 = -0.1D1 + x1
      t114 = x1 * z
      t116 = x1 * t53
      t118 = t53 * z
      t119 = x1 * t118
      t121 = t53 ** 2
      t123 = 0.2D1 * x1 * t121
      t124 = t79 * z
      t126 = t79 * t53
      t128 = t79 * t118
      t131 = 0.3D1 * t79 * t121
      t132 = t79 * x1
      t134 = 0.8D1 * t132 * z
      t136 = 0.12D2 * t132 * t53
      t138 = 0.8D1 * t132 * t118
      t139 = 0.1D1 + 0.8D1 * t114 - 0.12D2 * t116 + 0.8D1 * t119 - t123 
     #- 0.12D2 * t124 + 0.18D2 * t126 - 0.12D2 * t128 + t131 + t134 - t1
     #36 + t138
      t141 = 0.2D1 * t132 * t121
      t142 = t79 ** 2
      t144 = 0.4D1 * t142 * z
      t146 = 0.6D1 * t142 * t53
      t148 = 0.4D1 * t142 * t118
      t149 = t142 * t121
      t150 = 0.2D1 * z
      t151 = 0.2D1 * x1
      t152 = 0.3D1 * t53
      t153 = 0.3D1 * t79
      t154 = 0.2D1 * t132
      t155 = 0.2D1 * t118
      t156 = -t141 - t144 + t146 - t148 + t149 - t150 - t151 + t152 + t1
     #53 - t154 + t142 - t155 + t121
      t157 = t139 + t156
      t161 = t51 * wd
      t162 = t112 ** 2
      t163 = 0.1D1 / t162
      t165 = log(t163 * z)
      t168 = 0.1D1 - x1 + t114
      t169 = -z - x1 + t114
      t172 = log(-t168 * t169 * t163)
      t174 = -0.2D1 * t165 * 0.3141592653589793D1 + t172 * 0.31415926535
     #89793D1
      t176 = 0.11D2 * t114
      t177 = 0.18D2 * t116
      t178 = 0.11D2 * t119
      t179 = 0.15D2 * t124
      t180 = 0.24D2 * t126
      t181 = 0.15D2 * t128
      t182 = -0.1D1 - t176 + t177 - t178 + t123 + t179 - t180 + t181 - t
     #131 - t134 + t136 - t138 + t141
      t183 = log(x1)
      t186 = t144 - t146 + t148 - t149 - 0.2D1 * t183 * t157 + t150 + t1
     #51 - t152 - t153 + t154 - t142 + t155 - t121
      t187 = t182 + t186
      t189 = t157 * t174 - t187 * 0.3141592653589793D1
      t207 = polylog(2, -z / t168 / t169)
      t208 = t172 ** 2
      t212 = t165 ** 2
      t216 = -0.1D1 - t176 + t177 - t178 + t123 + t179 - t180 + t181 - t
     #131 - t134 + t136 - t138
      t220 = t183 ** 2
      t230 = -0.1080D4 * wd * t157 * t76 + 0.2D1 * (0.360D3 * t161 * t18
     #9 - t87 * t157) * t76 + (t86 * t51 * wd * t189 - t95 * t157 + 0.36
     #0D3 * t161 * (t187 * t174 + t157 * (-t3 * 0.3141592653589793D1 / 0
     #.6D1 + (t207 + t208 / 0.2D1) * 0.3141592653589793D1 - t212 * 0.314
     #1592653589793D1) - (-0.2D1 * t183 * (t216 - t156) + 0.2D1 * t220 *
     # t157) * 0.3141592653589793D1)) * t76
      t233 = FJET(XB1, XB2, s, t2 * x1, -t2 * t112, 0.0D0, 0.0D0, 0.0D0,
     # t230 * t20 / 0.80D2)
      rvgg2ght4s1e0 = t109 * t108 + t233 * t230 * t20 / 0.80D2

      end function



      doubleprecision function rvgg2ght4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = 0.1D1 / x1
      t10 = x1 ** 2
      t11 = 0.1D1 / z
      t13 = log(t10 * t11)
      t18 = 0.720D3 * lh
      t19 = log(z)
      t20 = 0.360D3 * t19
      t22 = (-t18 + t20) * wd
      t29 = log(t11)
      t31 = 0.3141592653589793D1 ** 2
      t35 = t29 ** 2
      t37 = t19 ** 2
      t41 = lh ** 2
      t44 = 0.1D1 / 0.3141592653589793D1
      t57 = (0.720D3 * wd * t5 * t7 + (0.360D3 * wd * (-t5 - t13 * t5) +
     # t22 * t5) * t7) * t11 / 0.80D2 + ((0.720D3 * t29 - 0.180D3 * t31 
     #- t29 * (-t18 + 0.720D3 + t20) + 0.180D3 * t35 + 0.180D3 * t37 - 0
     #.720D3 * t19 * lh + 0.720D3 * t41) * t44 * t5 + 0.360D3 * t5 * (-0
     #.2D1 * lh + t19 - t29 + 0.1D1) * t44) * 0.3141592653589793D1 * wd 
     #* t11 / 0.160D3
      t58 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t57)
      t61 = -0.1D1 + x1
      t63 = x1 * z
      t65 = x1 * t3
      t67 = t3 * z
      t68 = x1 * t67
      t70 = t3 ** 2
      t72 = 0.2D1 * x1 * t70
      t73 = t10 * z
      t75 = t10 * t3
      t77 = t10 * t67
      t80 = 0.3D1 * t10 * t70
      t81 = t10 * x1
      t83 = 0.8D1 * t81 * z
      t85 = 0.12D2 * t81 * t3
      t87 = 0.8D1 * t81 * t67
      t88 = 0.1D1 + 0.8D1 * t63 - 0.12D2 * t65 + 0.8D1 * t68 - t72 - 0.1
     #2D2 * t73 + 0.18D2 * t75 - 0.12D2 * t77 + t80 + t83 - t85 + t87
      t90 = 0.2D1 * t81 * t70
      t91 = t10 ** 2
      t93 = 0.4D1 * t91 * z
      t95 = 0.6D1 * t91 * t3
      t97 = 0.4D1 * t91 * t67
      t98 = t91 * t70
      t99 = 0.2D1 * z
      t100 = 0.2D1 * x1
      t101 = 0.3D1 * t3
      t102 = 0.3D1 * t10
      t103 = 0.2D1 * t81
      t104 = 0.2D1 * t67
      t105 = -t90 - t93 + t95 - t97 + t98 - t99 - t100 + t70 + t101 + t1
     #02 - t103 + t91 - t104
      t106 = t88 + t105
      t111 = t61 ** 2
      t112 = 0.1D1 / t111
      t114 = log(t112 * z)
      t121 = log(-(0.1D1 - x1 + t63) * (-z - x1 + t63) * t112)
      t131 = -0.1D1 - 0.11D2 * t63 + 0.18D2 * t65 - 0.11D2 * t68 + t72 +
     # 0.15D2 * t73 - 0.24D2 * t75 + 0.15D2 * t77 - t80 - t83 + t85 - t8
     #7 + t90
      t132 = log(x1)
      t135 = t93 - t95 + t97 - t98 - 0.2D1 * t132 * t106 + t99 + t100 - 
     #t70 - t101 - t102 + t103 - t91 + t104
      t144 = -0.720D3 * wd * t106 * t7 + (0.360D3 * t44 * wd * (t106 * (
     #-0.2D1 * t114 * 0.3141592653589793D1 + t121 * 0.3141592653589793D1
     #) - (t131 + t135) * 0.3141592653589793D1) - t22 * t106) * t7
      t147 = FJET(XB1, XB2, s, t2 * x1, -t2 * t61, 0.0D0, 0.0D0, 0.0D0, 
     #t144 * t11 / 0.80D2)
      rvgg2ght4s1em1 = t58 * t57 + t147 * t144 * t11 / 0.80D2

      end function



      doubleprecision function rvgg2ght4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = 0.1D1 / x1
      t8 = 0.1D1 / z
      t9 = t7 * t8
      t13 = log(z)
      t15 = log(t8)
      t18 = 0.1D1 / 0.3141592653589793D1
      t28 = 0.9D1 / 0.2D1 * wd * t5 * t9 + ((-0.720D3 * lh + 0.360D3 * t
     #13 - 0.360D3 * t15) * t18 * t5 + 0.360D3 * t18 * t5) * 0.314159265
     #3589793D1 * wd * t8 / 0.160D3
      t29 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t28)
      t38 = t3 * z
      t41 = t3 ** 2
      t44 = x1 ** 2
      t53 = t44 * x1
      t60 = 0.1D1 + 0.8D1 * x1 * z - 0.12D2 * x1 * t3 + 0.8D1 * x1 * t38
     # - 0.2D1 * x1 * t41 - 0.12D2 * t44 * z + 0.18D2 * t44 * t3 - 0.12D
     #2 * t44 * t38 + 0.3D1 * t44 * t41 + 0.8D1 * t53 * z - 0.12D2 * t53
     # * t3 + 0.8D1 * t53 * t38
      t63 = t44 ** 2
      t77 = -0.2D1 * t53 * t41 - 0.4D1 * t63 * z + 0.6D1 * t63 * t3 - 0.
     #4D1 * t63 * t38 + t63 * t41 - 0.2D1 * z + 0.3D1 * t3 - 0.2D1 * x1 
     #+ 0.3D1 * t44 - 0.2D1 * t53 + t63 - 0.2D1 * t38 + t41
      t78 = t60 + t77
      t82 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, -0.9D1 / 0.2D1 * wd * t78 * t9)
      rvgg2ght4s1em2 = t29 * t28 - 0.9D1 / 0.2D1 * t82 * wd * t78 * t7 *
     # t8

      end function



      doubleprecision function rvgg2ght4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = 0.1D1 / z
      t10 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, 0.9D1 / 0.4D1 * wd * t5 * t7)
      rvgg2ght4s1em3 = 0.9D1 / 0.4D1 * t10 * t5 * wd * t7

      end function



      doubleprecision function rvgg2ght4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght4s1em4 = 0.0D0

      end function


      doubleprecision function rvgg2ght4s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t6 = x1 ** 2
      t7 = 0.1D1 / z
      t9 = log(t6 * t7)
      t10 = t9 * t5
      t11 = -t5 - t10
      t14 = 0.720D3 * lh
      t15 = log(z)
      t16 = 0.360D3 * t15
      t17 = -t14 + t16
      t18 = t17 * wd
      t21 = 0.1D1 / x1
      t27 = 0.3141592653589793D1 ** 2
      t29 = t27 * t15
      t30 = 0.60D2 * t29
      t31 = lh ** 2
      t32 = t15 * t31
      t33 = 0.720D3 * t32
      t34 = t31 * lh
      t35 = 0.480D3 * t34
      t36 = t15 ** 2
      t37 = t36 * t15
      t38 = 0.60D2 * t37
      t39 = t27 * lh
      t40 = 0.120D3 * t39
      t41 = t36 * lh
      t42 = 0.360D3 * t41
      t43 = -t14 + 0.720D3 + t16
      t51 = (0.360D3 * t27 - t30 + t33 - 0.1153974627033210D4 - t35 + t3
     #8 + t40 - t42 - t43 * t27 / 0.2D1 + 0.60D2 * (-0.2D1 * lh + t15) *
     # t27) * wd
      t53 = t9 ** 2
      t55 = t53 * t5 / 0.2D1
      t62 = t10 + t55
      t65 = 0.180D3 * t36
      t66 = t15 * lh
      t67 = 0.720D3 * t66
      t68 = 0.720D3 * t31
      t69 = -0.180D3 * t27 + t65 - t67 + t68
      t70 = t69 * wd
      t86 = t36 ** 2
      t88 = t27 ** 2
      t90 = t31 ** 2
      t105 = log(t7)
      t106 = t105 ** 2
      t108 = 0.720D3 * t15
      t109 = 0.1440D4 * lh
      t110 = 0.60D2 * t27
      t111 = 0.1440D4 + t65 + t108 - t109 - t67 + t68 - t110
      t114 = t106 * t105
      t125 = -t30 + t33 + 0.1440D4 * t31 + 0.1440D4 * t15 + 0.1726025372
     #966790D4 - 0.2880D4 * lh - t35 + 0.360D3 * t36 + t38 - 0.1440D4 * 
     #t66 - 0.120D3 * t27 + t40 - t42
      t136 = t106 ** 2
      t138 = t105 * lh
      t140 = t105 * t15
      t147 = -0.30D2 * t27 * t36 - t106 * t43 + 0.2D1 * t105 * t111 - t1
     #14 * t43 / 0.6D1 + t106 * t111 / 0.2D1 - t105 * t125 + (-t14 + 0.7
     #20D3 + t16 - 0.360D3 * t105) * t27 - (-t105 * t43 + 0.180D3 * t106
     # + 0.1440D4 + t65 + t108 - t109 - t67 + t68 - t110) * t27 / 0.2D1 
     #+ 0.120D3 * t114 + 0.15D2 * t136 - 0.30D2 * (t27 - 0.4D1 * t138 + 
     #0.2D1 * t140 - t106 - t36 + 0.4D1 * t66 - 0.4D1 * t31) * t27
      t149 = 0.1D1 / 0.3141592653589793D1
      t163 = 0.12D2 * lh + 0.3D1 * t27 - 0.12D2 * t31 - 0.3D1 * t36 - t3
     #7 - 0.6D1 * t15 + 0.8D1 * t34 - 0.12D2 * t32 + 0.3D1 * t29 + 0.6D1
     # * t41 - 0.6D1 * t39 + 0.12D2 * t66
      t180 = 0.1323291045055350D2 - 0.12D2 * t138 + 0.6D1 * t140 - 0.12D
     #2 * t140 * lh + 0.6D1 * t105 - 0.3D1 * t106 + t114 + 0.6D1 * t106 
     #* lh - 0.3D1 * t106 * t15 + 0.3D1 * t105 * t36 - 0.3D1 * t105 * t2
     #7 + 0.12D2 * t105 * t31
      t190 = 0.5772156649015329D0 ** 2
      t193 = t190 ** 2
      t199 = 0.4006856343865313D0 + t27 * 0.5772156649015329D0 / 0.12D2 
     #+ t190 * 0.5772156649015329D0 / 0.6D1
      t204 = (t27 / 0.12D2 + t190 / 0.2D1) ** 2
      t218 = (0.3D1 * (0.360D3 * wd * t11 + t18 * t5) * t21 + 0.1440D4 *
     # wd * t5 * t21 + (t51 * t5 + 0.360D3 * wd * (-t55 - t53 * t9 * t5 
     #/ 0.6D1) + t18 * t62 + t70 * t11) * t21 + 0.2D1 * (t18 * t11 + t70
     # * t5 + 0.360D3 * wd * t62) * t21) * t7 / 0.80D2 - (-((0.120D3 * t
     #29 * lh + 0.15D2 * t86 + 0.3D1 * t88 + 0.240D3 * t90 + 0.230794925
     #4066420D4 * lh - 0.1153974627033210D4 * t15 - 0.120D3 * t37 * lh +
     # 0.360D3 * t36 * t31 - 0.480D3 * t15 * t34 - 0.120D3 * t27 * t31 +
     # t147) * t149 * t5 - 0.60D2 * t5 * (t163 + t180) * t149) * 0.31415
     #92653589793D1 + 0.360D3 * t149 * t5 * (0.3141592653589793D1 * (t88
     # / 0.80D2 + 0.8013712687730627D0 * 0.5772156649015329D0 + t27 * t1
     #90 / 0.12D2 + t193 / 0.12D2 - 0.2D1 * 0.5772156649015329D0 * t199 
     #+ t204) - 0.7D1 / 0.360D3 * t88 * 0.3141592653589793D1)) * wd * t7
     # / 0.160D3
      t219 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t218)
      t221 = -0.1D1 + x1
      t224 = t149 * wd
      t225 = 0.3D1 * t3
      t226 = 0.2D1 * z
      t227 = 0.2D1 * x1
      t228 = 0.3D1 * t6
      t229 = x1 * z
      t231 = x1 * t3
      t233 = t3 * z
      t234 = x1 * t233
      t236 = t3 ** 2
      t238 = 0.2D1 * x1 * t236
      t239 = t6 * z
      t241 = t6 * t3
      t243 = t6 * t233
      t245 = 0.1D1 + t225 - t226 - t227 + t228 + 0.8D1 * t229 - 0.12D2 *
     # t231 + 0.8D1 * t234 - t238 - 0.12D2 * t239 + 0.18D2 * t241 - 0.12
     #D2 * t243
      t247 = 0.3D1 * t6 * t236
      t248 = t6 * x1
      t250 = 0.8D1 * t248 * z
      t252 = 0.12D2 * t248 * t3
      t254 = 0.8D1 * t248 * t233
      t256 = 0.2D1 * t248 * t236
      t257 = t6 ** 2
      t259 = 0.4D1 * t257 * z
      t261 = 0.6D1 * t257 * t3
      t263 = 0.4D1 * t257 * t233
      t264 = t257 * t236
      t265 = 0.2D1 * t248
      t266 = 0.2D1 * t233
      t267 = t247 + t250 - t252 + t254 - t256 - t259 + t261 - t263 + t26
     #4 - t265 + t257 - t266 + t236
      t268 = t245 + t267
      t269 = t221 ** 2
      t270 = 0.1D1 / t269
      t272 = log(t270 * z)
      t275 = 0.1D1 - x1 + t229
      t276 = -z - x1 + t229
      t279 = log(-t275 * t276 * t270)
      t281 = -0.2D1 * t272 * 0.3141592653589793D1 + t279 * 0.31415926535
     #89793D1
      t283 = 0.11D2 * t229
      t284 = 0.18D2 * t231
      t285 = 0.11D2 * t234
      t286 = 0.15D2 * t239
      t287 = 0.24D2 * t241
      t288 = 0.15D2 * t243
      t289 = -0.1D1 - t225 + t226 + t227 - t228 - t283 + t284 - t285 + t
     #238 + t286 - t287 + t288 - t247
      t290 = log(x1)
      t293 = -t250 + t252 - t254 + t256 + t259 - t261 + t263 - t264 - 0.
     #2D1 * t290 * t268 + t265 - t257 + t266 - t236
      t294 = t289 + t293
      t296 = t268 * t281 - t294 * 0.3141592653589793D1
      t307 = t27 * 0.3141592653589793D1
      t312 = z / t275 / t276
      t313 = polylog(2, -t312)
      t314 = t279 ** 2
      t318 = t272 ** 2
      t320 = -t307 / 0.6D1 + (t313 + t314 / 0.2D1) * 0.3141592653589793D
     #1 - t318 * 0.3141592653589793D1
      t322 = -0.1D1 - t225 + t226 + t227 - t228 - t283 + t284 - t285 + t
     #238 + t286 - t287 + t288
      t324 = t322 - t267
      t326 = t290 ** 2
      t329 = -0.2D1 * t290 * t324 + 0.2D1 * t326 * t268
      t331 = 0.1D1 + t312
      t332 = log(t331)
      t335 = t332 ** 2
      t336 = log(-t312)
      t340 = polylog(3, t331)
      t341 = polylog(3, -t312)
      t364 = t17 * t149
      t368 = t294 * t281 + t268 * t320 - t329 * 0.3141592653589793D1
      t372 = wd * t296
      t383 = 0.3D1 * (0.360D3 * t224 * t296 - t18 * t268) * t21 - 0.1440
     #D4 * wd * t268 * t21 + (-t51 * t268 + 0.360D3 * t224 * (t294 * t32
     #0 + t329 * t281 + t268 * ((-t27 * t332 / 0.6D1 + t335 * t336 / 0.2
     #D1 + t332 * t313 + t340 + t341 - 0.1202056903159594D1 + t279 * t31
     #3 + t314 * t279 / 0.6D1) * 0.3141592653589793D1 - t279 * t307 / 0.
     #6D1 - t318 * t272 * 0.3141592653589793D1 / 0.3D1) - (0.2D1 * t326 
     #* t324 - 0.4D1 / 0.3D1 * t326 * t290 * t268) * 0.3141592653589793D
     #1) + t364 * wd * t368 + t69 * t149 * t372) * t21 + 0.2D1 * (t364 *
     # t372 - t70 * t268 + 0.360D3 * t224 * t368) * t21
      t386 = FJET(XB1, XB2, s, -t2 * t221, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # t383 * t7 / 0.80D2)
      rvgg2ght4s2e1 = t219 * t218 + t386 * t383 * t7 / 0.80D2

      end function



      doubleprecision function rvgg2ght4s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 ** 2
      t5 = log(z)
      t6 = t5 ** 2
      t9 = lh ** 2
      t20 = 0.1D1 / z
      t21 = log(t20)
      t22 = 0.720D3 * lh
      t23 = 0.360D3 * t5
      t24 = -t22 + 0.720D3 + t23
      t27 = t21 ** 2
      t30 = 0.180D3 * t6
      t33 = t5 * lh
      t34 = 0.720D3 * t33
      t35 = 0.720D3 * t9
      t50 = -0.1153974627033210D4 + 0.360D3 * t3 + 0.60D2 * t6 * t5 - 0.
     #480D3 * t9 * lh + 0.720D3 * t5 * t9 - 0.60D2 * t3 * t5 - 0.360D3 *
     # t6 * lh + 0.120D3 * t3 * lh + 0.2D1 * t21 * t24 + t27 * t24 / 0.2
     #D1 - t21 * (0.1440D4 + t30 + 0.720D3 * t5 - 0.1440D4 * lh - t34 + 
     #t35 - 0.60D2 * t3) - (-t22 + 0.720D3 + t23 - 0.360D3 * t21) * t3 /
     # 0.2D1 - 0.360D3 * t27 - 0.60D2 * t27 * t21 + 0.60D2 * (-0.2D1 * l
     #h + t5 - t21) * t3
      t51 = 0.1D1 / 0.3141592653589793D1
      t53 = z ** 2
      t55 = (t53 - z + 0.1D1) ** 2
      t66 = t3 - 0.4D1 * t21 * lh + 0.2D1 * t21 * t5 - t27 - t6 + 0.4D1 
     #* t33 - 0.4D1 * t9 + 0.4D1 * lh - 0.2D1 * t5 + 0.2D1 * t21 - 0.2D1
      t76 = 0.1D1 / x1
      t79 = x1 ** 2
      t81 = log(t79 * t20)
      t82 = t81 * t55
      t83 = -t55 - t82
      t86 = -t22 + t23
      t87 = t86 * wd
      t95 = (-0.180D3 * t3 + t30 - t34 + t35) * wd
      t97 = t81 ** 2
      t108 = (t50 * t51 * t55 - 0.180D3 * t55 * t66 * t51) * 0.314159265
     #3589793D1 * wd * t20 / 0.160D3 + (0.1080D4 * wd * t55 * t76 + 0.2D
     #1 * (0.360D3 * wd * t83 + t87 * t55) * t76 + (t87 * t83 + t95 * t5
     #5 + 0.360D3 * wd * (t82 + t97 * t55 / 0.2D1)) * t76) * t20 / 0.80D
     #2
      t109 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t108)
      t111 = -0.1D1 + x1
      t114 = 0.3D1 * t79
      t115 = 0.3D1 * t53
      t116 = t53 * z
      t117 = 0.2D1 * t116
      t118 = t53 ** 2
      t119 = t79 * x1
      t120 = 0.2D1 * t119
      t121 = t79 ** 2
      t122 = x1 * z
      t124 = x1 * t53
      t126 = x1 * t116
      t129 = 0.2D1 * x1 * t118
      t130 = t79 * z
      t132 = 0.1D1 + t114 + t115 - t117 + t118 - t120 + t121 + 0.8D1 * t
     #122 - 0.12D2 * t124 + 0.8D1 * t126 - t129 - 0.12D2 * t130
      t133 = t79 * t53
      t135 = t79 * t116
      t138 = 0.3D1 * t79 * t118
      t140 = 0.8D1 * t119 * z
      t142 = 0.12D2 * t119 * t53
      t144 = 0.8D1 * t119 * t116
      t146 = 0.2D1 * t119 * t118
      t148 = 0.4D1 * t121 * z
      t150 = 0.6D1 * t121 * t53
      t152 = 0.4D1 * t121 * t116
      t153 = t121 * t118
      t154 = 0.2D1 * z
      t155 = 0.2D1 * x1
      t156 = 0.18D2 * t133 - 0.12D2 * t135 + t138 + t140 - t142 + t144 -
     # t146 - t148 + t150 - t152 + t153 - t154 - t155
      t157 = t132 + t156
      t161 = t51 * wd
      t162 = t111 ** 2
      t163 = 0.1D1 / t162
      t165 = log(t163 * z)
      t168 = 0.1D1 - x1 + t122
      t169 = -z - x1 + t122
      t172 = log(-t168 * t169 * t163)
      t174 = -0.2D1 * t165 * 0.3141592653589793D1 + t172 * 0.31415926535
     #89793D1
      t176 = log(x1)
      t179 = 0.11D2 * t122
      t180 = 0.18D2 * t124
      t181 = 0.11D2 * t126
      t182 = 0.15D2 * t130
      t183 = -0.1D1 - 0.2D1 * t176 * t157 - t114 - t115 + t117 - t118 + 
     #t120 - t121 - t179 + t180 - t181 + t129 + t182
      t186 = -0.24D2 * t133 + 0.15D2 * t135 - t138 - t140 + t142 - t144 
     #+ t146 + t148 - t150 + t152 - t153 + t154 + t155
      t187 = t183 + t186
      t189 = t157 * t174 - t187 * 0.3141592653589793D1
      t207 = polylog(2, -z / t168 / t169)
      t208 = t172 ** 2
      t212 = t165 ** 2
      t216 = -0.1D1 - t114 - t115 + t117 - t118 + t120 - t121 - t179 + t
     #180 - t181 + t129 + t182
      t219 = t176 ** 2
      t229 = -0.1080D4 * wd * t157 * t76 + 0.2D1 * (0.360D3 * t161 * t18
     #9 - t87 * t157) * t76 + (t86 * t51 * wd * t189 - t95 * t157 + 0.36
     #0D3 * t161 * (t187 * t174 + t157 * (-t3 * 0.3141592653589793D1 / 0
     #.6D1 + (t207 + t208 / 0.2D1) * 0.3141592653589793D1 - t212 * 0.314
     #1592653589793D1) - (-0.2D1 * t176 * (t216 + t186) + 0.2D1 * t219 *
     # t157) * 0.3141592653589793D1)) * t76
      t232 = FJET(XB1, XB2, s, -t2 * t111, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # t229 * t20 / 0.80D2)
      rvgg2ght4s2e0 = t109 * t108 + t232 * t229 * t20 / 0.80D2

      end function



      doubleprecision function rvgg2ght4s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = 0.1D1 / x1
      t10 = x1 ** 2
      t11 = 0.1D1 / z
      t13 = log(t10 * t11)
      t18 = 0.720D3 * lh
      t19 = log(z)
      t20 = 0.360D3 * t19
      t22 = (-t18 + t20) * wd
      t29 = log(t11)
      t31 = 0.3141592653589793D1 ** 2
      t35 = t29 ** 2
      t37 = t19 ** 2
      t41 = lh ** 2
      t44 = 0.1D1 / 0.3141592653589793D1
      t57 = (0.720D3 * wd * t5 * t7 + (0.360D3 * wd * (-t5 - t13 * t5) +
     # t22 * t5) * t7) * t11 / 0.80D2 + ((0.720D3 * t29 - 0.180D3 * t31 
     #- t29 * (-t18 + 0.720D3 + t20) + 0.180D3 * t35 + 0.180D3 * t37 - 0
     #.720D3 * t19 * lh + 0.720D3 * t41) * t44 * t5 + 0.360D3 * t5 * (-0
     #.2D1 * lh + t19 - t29 + 0.1D1) * t44) * 0.3141592653589793D1 * wd 
     #* t11 / 0.160D3
      t58 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t57)
      t60 = -0.1D1 + x1
      t63 = t10 * x1
      t64 = 0.2D1 * t63
      t65 = t10 ** 2
      t66 = t3 * z
      t67 = 0.2D1 * t66
      t68 = t3 ** 2
      t69 = 0.2D1 * z
      t70 = 0.2D1 * x1
      t71 = x1 * z
      t73 = x1 * t3
      t75 = x1 * t66
      t78 = 0.2D1 * x1 * t68
      t79 = t10 * z
      t81 = 0.1D1 - t64 + t65 - t67 + t68 - t69 - t70 + 0.8D1 * t71 - 0.
     #12D2 * t73 + 0.8D1 * t75 - t78 - 0.12D2 * t79
      t82 = t10 * t3
      t84 = t10 * t66
      t87 = 0.3D1 * t10 * t68
      t89 = 0.8D1 * t63 * z
      t91 = 0.12D2 * t63 * t3
      t93 = 0.8D1 * t63 * t66
      t95 = 0.2D1 * t63 * t68
      t97 = 0.4D1 * t65 * z
      t99 = 0.6D1 * t65 * t3
      t101 = 0.4D1 * t65 * t66
      t102 = t65 * t68
      t103 = 0.3D1 * t3
      t104 = 0.3D1 * t10
      t105 = 0.18D2 * t82 - 0.12D2 * t84 + t87 + t89 - t91 + t93 - t95 -
     # t97 + t99 - t101 + t102 + t103 + t104
      t106 = t81 + t105
      t111 = t60 ** 2
      t112 = 0.1D1 / t111
      t114 = log(t112 * z)
      t121 = log(-(0.1D1 - x1 + t71) * (-z - x1 + t71) * t112)
      t130 = -0.1D1 + t64 - t65 + t67 - t68 + t69 + t70 - 0.11D2 * t71 +
     # 0.18D2 * t73 - 0.11D2 * t75 + t78 + 0.15D2 * t79 - 0.24D2 * t82
      t132 = log(x1)
      t135 = 0.15D2 * t84 - t87 - t89 + t91 - t93 + t95 + t97 - t99 + t1
     #01 - t102 - 0.2D1 * t132 * t106 - t103 - t104
      t144 = -0.720D3 * wd * t106 * t7 + (0.360D3 * t44 * wd * (t106 * (
     #-0.2D1 * t114 * 0.3141592653589793D1 + t121 * 0.3141592653589793D1
     #) - (t130 + t135) * 0.3141592653589793D1) - t22 * t106) * t7
      t147 = FJET(XB1, XB2, s, -t2 * t60, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #t144 * t11 / 0.80D2)
      rvgg2ght4s2em1 = t58 * t57 + t147 * t144 * t11 / 0.80D2

      end function



      doubleprecision function rvgg2ght4s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = 0.1D1 / x1
      t8 = 0.1D1 / z
      t9 = t7 * t8
      t13 = log(z)
      t15 = log(t8)
      t18 = 0.1D1 / 0.3141592653589793D1
      t28 = 0.9D1 / 0.2D1 * wd * t5 * t9 + ((-0.720D3 * lh + 0.360D3 * t
     #13 - 0.360D3 * t15) * t18 * t5 + 0.360D3 * t18 * t5) * 0.314159265
     #3589793D1 * wd * t8 / 0.160D3
      t29 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t28)
      t38 = t3 * z
      t41 = t3 ** 2
      t44 = x1 ** 2
      t53 = t44 * x1
      t60 = 0.1D1 + 0.8D1 * x1 * z - 0.12D2 * x1 * t3 + 0.8D1 * x1 * t38
     # - 0.2D1 * x1 * t41 - 0.12D2 * t44 * z + 0.18D2 * t44 * t3 - 0.12D
     #2 * t44 * t38 + 0.3D1 * t44 * t41 + 0.8D1 * t53 * z - 0.12D2 * t53
     # * t3 + 0.8D1 * t53 * t38
      t63 = t44 ** 2
      t77 = -0.2D1 * t53 * t41 - 0.4D1 * t63 * z + 0.6D1 * t63 * t3 - 0.
     #4D1 * t63 * t38 + t63 * t41 + 0.3D1 * t3 - 0.2D1 * z - 0.2D1 * x1 
     #+ 0.3D1 * t44 - 0.2D1 * t53 + t63 - 0.2D1 * t38 + t41
      t78 = t60 + t77
      t82 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, -0.9D1 / 0.2D1 * wd * t78 * t9)
      rvgg2ght4s2em2 = t29 * t28 - 0.9D1 / 0.2D1 * t82 * wd * t78 * t7 *
     # t8

      end function



      doubleprecision function rvgg2ght4s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = 0.1D1 / z
      t10 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, 0.9D1 / 0.4D1 * wd * t5 * t7)
      rvgg2ght4s2em3 = 0.9D1 / 0.4D1 * t10 * t5 * wd * t7

      end function



      doubleprecision function rvgg2ght4s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght4s2em4 = 0.0D0

      end function
