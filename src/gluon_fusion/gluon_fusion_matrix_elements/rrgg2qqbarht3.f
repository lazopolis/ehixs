  
      subroutine rrgg2qqbarht3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh31J1  
      doubleprecision rrgg2qqbarh31J2  
      doubleprecision rrgg2qqbarh31J3  
      doubleprecision rrgg2qqbarh31J4  
      doubleprecision rrgg2qqbarh31J5  
      doubleprecision rrgg2qqbarh31J6  
      doubleprecision rrgg2qqbarh31J7  
      doubleprecision rrgg2qqbarht3s1e1  
      doubleprecision rrgg2qqbarht3s1e0  
      doubleprecision rrgg2qqbarht3s1em1  
      doubleprecision rrgg2qqbarht3s1em2  
      doubleprecision rrgg2qqbarht3s1em3  
      doubleprecision rrgg2qqbarht3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh31J1
      doubleprecision rrgg2qqbarh31J2
      doubleprecision rrgg2qqbarh31J3
      doubleprecision rrgg2qqbarh31J4
      doubleprecision rrgg2qqbarh31J5
      doubleprecision rrgg2qqbarh31J6
      doubleprecision rrgg2qqbarh31J7
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
      t15 = 0.3141592653589793D1 * t11
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrgg2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = x1 ** 2
      t29 = t27 * t28
      t30 = t6 ** 2
      t31 = t30 * x4
      t32 = t9 ** 2
      t36 = log(0.4D1 * t29 * t31 * t32)
      t37 = t36 * t9
      t38 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t39 = t18 * t38
      t41 = t36 ** 2
      t43 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t44 = t18 * t43
      t51 = 0.3141592653589793D1 * lh
      t52 = t11 * t16
      t53 = t19 * t38
      t59 = lh ** 2
      t61 = 0.3141592653589793D1 ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t64 = 0.3141592653589793D1 * t63
      t65 = t64 * t11
      t67 = t16 * t9 * t44
      t68 = t65 * t67
      t70 = 0.1D1 / x4
      t73 = t28 * t30
      t74 = t73 * t32
      t77 = log(0.4D1 * t27 * t74)
      t78 = t77 * 0.3141592653589793D1
      t81 = t77 ** 2
      t82 = t81 * 0.3141592653589793D1
      t90 = rrgg2qqbarh31J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t119 = x3 * t24
      t120 = t119 * t26
      t125 = log(0.4D1 * t120 * t73 * x4 * t32)
      t132 = t51 * t11
      t136 = 0.1D1 / x3
      t142 = log(0.4D1 * t120 * t74)
      t143 = t142 * t9
      t145 = t142 ** 2
      t161 = (0.90D2 * t15 * t16 * (-t21 + t37 * t39 - t41 * t9 * t44 / 
     #0.2D1) - 0.180D3 * t51 * t52 * (-t53 + t37 * t44) - t68) * t70 / 0
     #.720D3 - (t64 + 0.180D3 * t78 * lh + 0.45D2 * t82) * t11 * t16 * t
     #53 / 0.720D3 - t15 * t16 * t19 * t90 / 0.8D1 - (0.3141592653589793
     #D1 * (0.60D2 * lh * t61 - 0.2884936567583026D3 - 0.120D3 * t59 * l
     #h) - t78 * t63 - 0.90D2 * t82 * lh - 0.15D2 * t81 * t77 * 0.314159
     #2653589793D1) * t11 * t16 * t19 * t43 / 0.720D3 - (-0.180D3 * t51 
     #- 0.90D2 * t78) * t11 * t16 * t21 / 0.720D3 + (0.90D2 * t15 * t16 
     #* (-t53 + t125 * t9 * t44) + 0.180D3 * t132 * t67) * t136 * t70 / 
     #0.720D3 - (0.90D2 * t15 * t16 * (t21 - t143 * t39 + t145 * t9 * t4
     #4 / 0.2D1) - 0.180D3 * t51 * t52 * (t53 - t143 * t44) + t68) * t13
     #6 / 0.720D3
      t162 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t161)
      t164 = 0.1D1 - x4
      t165 = KAPPA2(x1, x2, 0.0D0, t164, z)
      t166 = s * t165
      t167 = t166 * t4
      t168 = -t164
      t169 = t7 * t168
      t170 = t166 * t169
      t171 = t7 * x4
      t172 = t166 * t171
      t173 = t165 ** 2
      t176 = x1 * t6
      t178 = s * t173 * t11 * t176 * t168
      t180 = 0.1D1 / (-0.2D1 + t165)
      t181 = t173 * t180
      t182 = rrgg2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t164)
      t184 = t173 ** 2
      t186 = t31 * t168 * t184
      t189 = log(-0.4D1 * t29 * t186)
      t190 = t189 * t173
      t191 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t164)
      t194 = t189 ** 2
      t196 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t164)
      t197 = t180 * t196
      t204 = t181 * t191
      t211 = t16 * t173 * t197
      t216 = t119 * t26 * t28
      t219 = log(-0.4D1 * t216 * t186)
      t232 = (0.90D2 * t15 * t16 * (t181 * t182 - t190 * t180 * t191 + t
     #194 * t173 * t197 / 0.2D1) - 0.180D3 * t51 * t52 * (t204 - t190 * 
     #t197) + t65 * t211) * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (t204 
     #- t219 * t173 * t197) - 0.180D3 * t132 * t211) * t136 * t70 / 0.72
     #0D3
      t233 = FJET(XB1, XB2, s, 0.0D0, t167, t170, -t172, t178, t232)
      t235 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t161)
      t237 = FJET(XB1, XB2, s, t167, 0.0D0, -t172, t170, t178, t232)
      t239 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t240 = s * t239
      t241 = t4 * x3
      t242 = t240 * t241
      t243 = -0.1D1 + x3
      t244 = t4 * t243
      t245 = t240 * t244
      t246 = t240 * t7
      t247 = t239 ** 2
      t251 = s * t247 * t11 * t176 * t243
      t253 = 0.1D1 / (-0.2D1 + t239)
      t254 = t247 * t253
      t255 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t256 = t254 * t255
      t257 = t30 * t243
      t258 = t247 ** 2
      t263 = log(-0.4D1 * t216 * t257 * x4 * t258)
      t265 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t266 = t253 * t265
      t273 = t16 * t247 * t266
      t279 = rrgg2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t285 = log(-0.4D1 * t120 * t73 * t243 * t258)
      t286 = t285 * t247
      t289 = t285 ** 2
      t306 = (0.90D2 * t15 * t16 * (t256 - t263 * t247 * t266) - 0.180D3
     # * t132 * t273) * t136 * t70 / 0.720D3 - (-0.90D2 * t15 * t16 * (t
     #254 * t279 - t286 * t253 * t255 + t289 * t247 * t266 / 0.2D1) + 0.
     #180D3 * t51 * t52 * (t256 - t286 * t266) - t65 * t273) * t136 / 0.
     #720D3
      t307 = FJET(XB1, XB2, s, t242, -t245, -t246, 0.0D0, t251, t306)
      t309 = KAPPA2(x1, x2, x3, t164, z)
      t310 = s * t309
      t311 = t310 * t241
      t312 = t310 * t244
      t313 = t310 * t169
      t314 = t310 * t171
      t315 = t309 ** 2
      t320 = cos(t22)
      t322 = x4 * t168
      t324 = Sqrt(x3 * t243 * t322)
      t329 = s * t315 * t11 * t176 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t320 * t324)
      t331 = 0.1D1 / (-0.2D1 + t309)
      t333 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t16
     #4)
      t335 = t315 ** 2
      t340 = log(0.4D1 * t216 * t257 * t322 * t335)
      t342 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t16
     #4)
      t343 = t331 * t342
      t345 = -t315 * t331 * t333 + t340 * t315 * t343
      t352 = 0.180D3 * t132 * t16 * t315 * t343
      t353 = 0.90D2 * t15 * t16 * t345 + t352
      t357 = FJET(XB1, XB2, s, t311, -t312, t313, -t314, t329, t353 * t1
     #36 * t70 / 0.720D3)
      t359 = t136 * t70
      t362 = FJET(XB1, XB2, s, -t245, t242, 0.0D0, -t246, t251, t306)
      t368 = 0.90D2 * t15 * t16 * t345 + t352
      t372 = FJET(XB1, XB2, s, -t312, t311, -t314, t313, t329, t368 * t1
     #36 * t70 / 0.720D3)
      rrgg2qqbarht3s1e1 = t162 * t161 + t233 * t232 + t235 * t161 + t237
     # * t232 + t307 * t306 + t357 * t353 * t359 / 0.720D3 + t362 * t306
     # + t372 * t368 * t359 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh31J1
      doubleprecision rrgg2qqbarh31J2
      doubleprecision rrgg2qqbarh31J3
      doubleprecision rrgg2qqbarh31J4
      doubleprecision rrgg2qqbarh31J5
      doubleprecision rrgg2qqbarh31J6
      doubleprecision rrgg2qqbarh31J7
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
      t15 = 0.3141592653589793D1 * t11
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = x1 ** 2
      t29 = t27 * t28
      t30 = t6 ** 2
      t31 = t30 * x4
      t32 = t9 ** 2
      t36 = log(0.4D1 * t29 * t31 * t32)
      t38 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t39 = t18 * t38
      t45 = 0.3141592653589793D1 * lh
      t46 = t45 * t11
      t47 = t16 * t9
      t50 = 0.180D3 * t46 * t47 * t39
      t52 = 0.1D1 / x4
      t56 = 0.1D1 / x3
      t57 = t56 * t52
      t62 = x3 * t24 * t26
      t63 = t28 * t30
      t64 = t63 * t32
      t67 = log(0.4D1 * t62 * t64)
      t78 = rrgg2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t85 = log(0.4D1 * t27 * t64)
      t86 = t85 * 0.3141592653589793D1
      t93 = lh ** 2
      t95 = 0.3141592653589793D1 ** 2
      t101 = t85 ** 2
      t110 = (0.90D2 * t15 * t16 * (-t21 + t36 * t9 * t39) + t50) * t52 
     #/ 0.720D3 - t15 * t47 * t39 * t57 / 0.8D1 - (0.90D2 * t15 * t16 * 
     #(t21 - t67 * t9 * t39) - t50) * t56 / 0.720D3 - t15 * t16 * t19 * 
     #t78 / 0.8D1 - (-0.180D3 * t45 - 0.90D2 * t86) * t11 * t16 * t21 / 
     #0.720D3 - (0.3141592653589793D1 * (0.180D3 * t93 - 0.30D2 * t95) +
     # 0.180D3 * t86 * lh + 0.45D2 * t101 * 0.3141592653589793D1) * t11 
     #* t16 * t19 * t38 / 0.720D3
      t111 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t110)
      t113 = 0.1D1 - x4
      t114 = KAPPA2(x1, x2, 0.0D0, t113, z)
      t115 = s * t114
      t116 = t115 * t4
      t117 = -t113
      t118 = t7 * t117
      t119 = t115 * t118
      t120 = t7 * x4
      t121 = t115 * t120
      t122 = t114 ** 2
      t125 = x1 * t6
      t127 = s * t122 * t11 * t125 * t117
      t129 = 0.1D1 / (-0.2D1 + t114)
      t131 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t113)
      t133 = t122 ** 2
      t138 = log(-0.4D1 * t29 * t31 * t117 * t133)
      t140 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t113)
      t141 = t129 * t140
      t147 = t16 * t122
      t158 = (0.90D2 * t15 * t16 * (t122 * t129 * t131 - t138 * t122 * t
     #141) - 0.180D3 * t46 * t147 * t141) * t52 / 0.720D3 + t15 * t147 *
     # t141 * t57 / 0.8D1
      t159 = FJET(XB1, XB2, s, 0.0D0, t116, t119, -t121, t127, t158)
      t161 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t163 = FJET(XB1, XB2, s, t116, 0.0D0, -t121, t119, t127, t158)
      t165 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t166 = s * t165
      t167 = t4 * x3
      t168 = t166 * t167
      t169 = -0.1D1 + x3
      t170 = t4 * t169
      t171 = t166 * t170
      t172 = t166 * t7
      t173 = t165 ** 2
      t177 = s * t173 * t11 * t125 * t169
      t178 = t16 * t173
      t181 = 0.1D1 / (-0.2D1 + t165)
      t182 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t183 = t181 * t182
      t188 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t190 = t173 ** 2
      t195 = log(-0.4D1 * t62 * t63 * t169 * t190)
      t208 = t15 * t178 * t183 * t57 / 0.8D1 - (-0.90D2 * t15 * t16 * (t
     #173 * t181 * t188 - t195 * t173 * t183) + 0.180D3 * t46 * t178 * t
     #183) * t56 / 0.720D3
      t209 = FJET(XB1, XB2, s, t168, -t171, -t172, 0.0D0, t177, t208)
      t211 = KAPPA2(x1, x2, x3, t113, z)
      t212 = s * t211
      t213 = t212 * t167
      t214 = t212 * t170
      t215 = t212 * t118
      t216 = t212 * t120
      t217 = t211 ** 2
      t222 = cos(t22)
      t226 = Sqrt(x3 * t169 * x4 * t117)
      t231 = s * t217 * t11 * t125 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t222 * t226)
      t235 = 0.1D1 / (-0.2D1 + t211)
      t236 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t11
     #3)
      t240 = t15 * t16 * t217 * t235 * t236 * t57 / 0.8D1
      t241 = FJET(XB1, XB2, s, t213, -t214, t215, -t216, t231, -t240)
      t243 = t11 * t16
      t248 = t217 * t235 * t236 * t56 * t52
      t251 = FJET(XB1, XB2, s, -t171, t168, 0.0D0, -t172, t177, t208)
      t253 = FJET(XB1, XB2, s, -t214, t213, -t216, t215, t231, -t240)
      rrgg2qqbarht3s1e0 = t111 * t110 + t159 * t158 + t161 * t110 + t163
     # * t158 + t209 * t208 - t241 * 0.3141592653589793D1 * t243 * t248 
     #/ 0.8D1 + t251 * t208 - t253 * 0.3141592653589793D1 * t243 * t248 
     #/ 0.8D1

      end function



      doubleprecision function rrgg2qqbarht3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh31J1
      doubleprecision rrgg2qqbarh31J2
      doubleprecision rrgg2qqbarh31J3
      doubleprecision rrgg2qqbarh31J4
      doubleprecision rrgg2qqbarh31J5
      doubleprecision rrgg2qqbarh31J6
      doubleprecision rrgg2qqbarh31J7
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
      t16 = 0.1D1 / s
      t17 = 0.3141592653589793D1 * t11 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t22 = 0.1D1 / x3
      t27 = rrgg2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t34 = sin(x2 * 0.3141592653589793D1)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = x1 ** 2
      t40 = t6 ** 2
      t42 = t9 ** 2
      t46 = log(0.4D1 * t35 / t36 * t39 * t40 * t42)
      t55 = 0.1D1 / x4
      t60 = -t17 * t20 * t21 * t22 / 0.8D1 - t17 * t20 * t27 / 0.8D1 - (
     #-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t46 * 0.3141592653
     #589793D1) * t11 * t16 * t20 * t21 / 0.720D3 - t17 * t20 * t21 * t5
     #5 / 0.8D1
      t61 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t60)
      t63 = 0.1D1 - x4
      t64 = KAPPA2(x1, x2, 0.0D0, t63, z)
      t65 = s * t64
      t66 = t65 * t4
      t67 = -t63
      t69 = t65 * t7 * t67
      t71 = t65 * t7 * x4
      t72 = t64 ** 2
      t75 = x1 * t6
      t77 = s * t72 * t11 * t75 * t67
      t81 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t
     #63)
      t83 = t72 / (-0.2D1 + t64) * t81 * t55
      t85 = t17 * t83 / 0.8D1
      t86 = FJET(XB1, XB2, s, 0.0D0, t66, t69, -t71, t77, t85)
      t88 = t11 * t16
      t92 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t60)
      t94 = FJET(XB1, XB2, s, t66, 0.0D0, -t71, t69, t77, t85)
      t99 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t100 = s * t99
      t102 = t100 * t4 * x3
      t103 = -0.1D1 + x3
      t105 = t100 * t4 * t103
      t106 = t100 * t7
      t107 = t99 ** 2
      t111 = s * t107 * t11 * t75 * t103
      t115 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t117 = t107 / (-0.2D1 + t99) * t115 * t22
      t119 = t17 * t117 / 0.8D1
      t120 = FJET(XB1, XB2, s, t102, -t105, -t106, 0.0D0, t111, t119)
      t125 = FJET(XB1, XB2, s, -t105, t102, 0.0D0, -t106, t111, t119)
      rrgg2qqbarht3s1em1 = t61 * t60 + t86 * 0.3141592653589793D1 * t88 
     #* t83 / 0.8D1 + t92 * t60 + t94 * 0.3141592653589793D1 * t88 * t83
     # / 0.8D1 + t120 * 0.3141592653589793D1 * t88 * t117 / 0.8D1 + t125
     # * 0.3141592653589793D1 * t88 * t117 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh31J1
      doubleprecision rrgg2qqbarh31J2
      doubleprecision rrgg2qqbarh31J3
      doubleprecision rrgg2qqbarh31J4
      doubleprecision rrgg2qqbarh31J5
      doubleprecision rrgg2qqbarh31J6
      doubleprecision rrgg2qqbarh31J7
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t19 = 0.1D1 / (-0.2D1 + t1)
      t21 = rrgg2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t24 = 0.3141592653589793D1 * t11 * t16 * t9 * t19 * t21 / 0.8D1
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, -t24)
      t30 = t16 * t9 * t19 * t21
      t32 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, -t24)
      rrgg2qqbarht3s1em2 = -t25 * 0.3141592653589793D1 * t11 * t30 / 0.8
     #D1 - t32 * 0.3141592653589793D1 * t11 * t30 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh31J1
      doubleprecision rrgg2qqbarh31J2
      doubleprecision rrgg2qqbarh31J3
      doubleprecision rrgg2qqbarh31J4
      doubleprecision rrgg2qqbarh31J5
      doubleprecision rrgg2qqbarh31J6
      doubleprecision rrgg2qqbarh31J7
      rrgg2qqbarht3s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh31J1
      doubleprecision rrgg2qqbarh31J2
      doubleprecision rrgg2qqbarh31J3
      doubleprecision rrgg2qqbarh31J4
      doubleprecision rrgg2qqbarh31J5
      doubleprecision rrgg2qqbarh31J6
      doubleprecision rrgg2qqbarh31J7
      rrgg2qqbarht3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh31J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t6 = (0.1D1 - z) ** 2
      t7 = t1 * t3 * t6
      t8 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t19 = Sqrt(x3 * t14 * x4 * (0.1D1 - x4))
      rrgg2qqbarh31J1 = wd * (0.8D1 * t7 * x1 * t8 * (x3 + x4 - 0.2D1 * 
     #x3 * x4 - 0.2D1 * t13 * t19) - 0.8D1 * t7 * t8 * x4 * x1 * t14) * 
     #nf / s / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh31J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t6 = (0.1D1 - z) ** 2
      t7 = t4 * t6
      t8 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t19 = Sqrt(x3 * t14 * x4 * (0.1D1 - x4))
      t33 = t8 ** 2
      t35 = x4 ** 2
      t39 = x1 ** 2
      t41 = t14 ** 2
      rrgg2qqbarh31J2 = (0.2D1 * wd * (0.8D1 * t7 * x1 * t8 * (x3 + x4 -
     # 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19) - 0.8D1 * t7 * t8 * x4 * x1 
     #* t14) + wd * (-0.4D1 * t4 * t6 * t33 * t35 - 0.4D1 * t4 * t6 * t3
     #9 * t41 + 0.8D1 * t1)) * nf / s / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh31J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = t4 * t6
      t8 = 0.1D1 - x1
      t9 = x1 * t8
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t19 = Sqrt(x3 * t14 * x4 * (0.1D1 - x4))
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t24 = t7 * t9 * t22
      t28 = t7 * t8 * x4 * x1 * t14
      t33 = t8 ** 2
      t35 = x4 ** 2
      t37 = t4 * t6 * t33 * t35
      t39 = x1 ** 2
      t41 = t14 ** 2
      t43 = t4 * t6 * t39 * t41
      t45 = 0.8D1 * t1
      t55 = t1 * t2
      t63 = t1 * t3 * t2 * t6 * t5
      t78 = t3 ** 2
      t80 = t6 ** 2
      t83 = t22 ** 2
      t87 = -0.12D2 * t24 + 0.48D2 * t7 * t9 * t22 * z - 0.4D1 * t28 - 0
     #.2D1 * t55 * t5 * x1 * t14 + 0.26D2 * t63 * t39 * t14 * t8 * t22 +
     # 0.26D2 * t63 * t33 * x4 * x1 * t22 - 0.2D1 * t55 * t5 * t8 * x4 -
     # 0.42D2 * t1 * t78 * t80 * t39 * t33 * t83 + t45 + t37 + t43
      rrgg2qqbarh31J3 = (0.3D1 * wd * (0.8D1 * t24 - 0.8D1 * t28) + 0.2D
     #1 * wd * (-0.4D1 * t37 - 0.4D1 * t43 + t45) + wd * t87) * nf / s /
     # z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh31J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = t4 * t6
      t8 = 0.1D1 - x1
      t9 = x1 * t8
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t19 = Sqrt(x3 * t14 * x4 * (0.1D1 - x4))
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t24 = t7 * t9 * t22
      t28 = t7 * t8 * x4 * x1 * t14
      t33 = t8 ** 2
      t35 = x4 ** 2
      t37 = t4 * t6 * t33 * t35
      t39 = x1 ** 2
      t41 = t14 ** 2
      t43 = t4 * t6 * t39 * t41
      t45 = 0.8D1 * t1
      t55 = t1 * t2
      t63 = t1 * t3 * t2 * t6 * t5
      t78 = t3 ** 2
      t80 = t6 ** 2
      t83 = t22 ** 2
      t87 = -0.12D2 * t24 + 0.48D2 * t7 * t9 * t22 * z - 0.4D1 * t28 - 0
     #.2D1 * t55 * t5 * x1 * t14 + 0.26D2 * t63 * t39 * t14 * t8 * t22 +
     # 0.26D2 * t63 * t33 * x4 * x1 * t22 - 0.2D1 * t55 * t5 * t8 * x4 -
     # 0.42D2 * t1 * t78 * t80 * t39 * t33 * t83 + t45 + t37 + t43
      rrgg2qqbarh31J4 = (0.4D1 * wd * (0.8D1 * t24 - 0.8D1 * t28) + 0.3D
     #1 * wd * (-0.4D1 * t37 - 0.4D1 * t43 + t45) + 0.2D1 * wd * t87) * 
     #nf / s / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh31J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = t4 * t6
      t8 = 0.1D1 - x1
      t9 = x1 * t8
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t19 = Sqrt(x3 * t14 * x4 * (0.1D1 - x4))
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t24 = t7 * t9 * t22
      t28 = t7 * t8 * x4 * x1 * t14
      t33 = t8 ** 2
      t35 = x4 ** 2
      t37 = t4 * t6 * t33 * t35
      t39 = x1 ** 2
      t41 = t14 ** 2
      t43 = t4 * t6 * t39 * t41
      t45 = 0.8D1 * t1
      t55 = t1 * t2
      t63 = t1 * t3 * t2 * t6 * t5
      t78 = t3 ** 2
      t80 = t6 ** 2
      t83 = t22 ** 2
      t87 = -0.12D2 * t24 + 0.48D2 * t7 * t9 * t22 * z - 0.4D1 * t28 - 0
     #.2D1 * t55 * t5 * x1 * t14 + 0.26D2 * t63 * t39 * t14 * t8 * t22 +
     # 0.26D2 * t63 * t33 * x4 * x1 * t22 - 0.2D1 * t55 * t5 * t8 * x4 -
     # 0.42D2 * t1 * t78 * t80 * t39 * t33 * t83 + t45 + t37 + t43
      rrgg2qqbarh31J5 = (0.5D1 * wd * (0.8D1 * t24 - 0.8D1 * t28) + 0.4D
     #1 * wd * (-0.4D1 * t37 - 0.4D1 * t43 + t45) + 0.3D1 * wd * t87) * 
     #nf / s / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh31J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t10 = x4 ** 2
      t12 = t4 * t6 * t8 * t10
      t14 = x1 ** 2
      t16 = 0.1D1 - x3
      t17 = t16 ** 2
      t19 = t4 * t6 * t14 * t17
      t21 = 0.8D1 * t1
      t25 = t4 * t6
      t26 = x1 * t7
      t30 = cos(x2 * 0.3141592653589793D1)
      t35 = Sqrt(x3 * t16 * x4 * (0.1D1 - x4))
      t38 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t30 * t35
      t51 = t1 * t2
      t59 = t1 * t3 * t2 * t6 * t5
      t74 = t3 ** 2
      t76 = t6 ** 2
      t79 = t38 ** 2
      t83 = -0.12D2 * t25 * t26 * t38 + 0.48D2 * t25 * t26 * t38 * z - 0
     #.4D1 * t25 * t7 * x4 * x1 * t16 - 0.2D1 * t51 * t5 * x1 * t16 + 0.
     #26D2 * t59 * t14 * t16 * t7 * t38 + 0.26D2 * t59 * t8 * x4 * x1 * 
     #t38 - 0.2D1 * t51 * t5 * t7 * x4 - 0.42D2 * t1 * t74 * t76 * t14 *
     # t8 * t79 + t21 + t12 + t19
      rrgg2qqbarh31J6 = (0.5D1 * wd * (-0.4D1 * t12 - 0.4D1 * t19 + t21)
     # + 0.4D1 * wd * t83) * nf / s / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh31J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = t4 * t6
      t8 = 0.1D1 - x1
      t9 = x1 * t8
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t19 = Sqrt(x3 * t14 * x4 * (0.1D1 - x4))
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t35 = t1 * t2
      t43 = t1 * t3 * t2 * t6 * t5
      t44 = x1 ** 2
      t50 = t8 ** 2
      t60 = t3 ** 2
      t62 = t6 ** 2
      t65 = t22 ** 2
      t71 = x4 ** 2
      t75 = t14 ** 2
      t78 = -0.12D2 * t7 * t9 * t22 + 0.48D2 * t7 * t9 * t22 * z - 0.4D1
     # * t7 * t8 * x4 * x1 * t14 - 0.2D1 * t35 * t5 * x1 * t14 + 0.26D2 
     #* t43 * t44 * t14 * t8 * t22 + 0.26D2 * t43 * t50 * x4 * x1 * t22 
     #- 0.2D1 * t35 * t5 * t8 * x4 - 0.42D2 * t1 * t60 * t62 * t44 * t50
     # * t65 + 0.8D1 * t1 + t4 * t6 * t50 * t71 + t4 * t6 * t44 * t75
      rrgg2qqbarh31J7 = 0.5D1 / 0.6D1 * wd * t78 * nf / s / z / 0.314159
     #2653589793D1

      end function
  
 