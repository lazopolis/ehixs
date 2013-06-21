  
      subroutine rrgg2gght3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh31J1  
      doubleprecision rrgg2ggh31J2  
      doubleprecision rrgg2ggh31J3  
      doubleprecision rrgg2ggh31J4  
      doubleprecision rrgg2ggh31J5  
      doubleprecision rrgg2ggh31J6  
      doubleprecision rrgg2ggh31J7  
      doubleprecision rrgg2gght3s1e1  
      doubleprecision rrgg2gght3s1e0  
      doubleprecision rrgg2gght3s1em1  
      doubleprecision rrgg2gght3s1em2  
      doubleprecision rrgg2gght3s1em3  
      doubleprecision rrgg2gght3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh31J1
      doubleprecision rrgg2ggh31J2
      doubleprecision rrgg2ggh31J3
      doubleprecision rrgg2ggh31J4
      doubleprecision rrgg2ggh31J5
      doubleprecision rrgg2ggh31J6
      doubleprecision rrgg2ggh31J7
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
      t20 = rrgg2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t38 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t39 = t18 * t38
      t41 = t36 ** 2
      t43 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t90 = rrgg2ggh31J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
     #- 0.90D2 * t78) * t11 * t16 * t21 / 0.720D3 - (0.90D2 * t15 * t16 
     #* (t53 - t125 * t9 * t44) - 0.180D3 * t132 * t67) * t136 * t70 / 0
     #.720D3 + (0.90D2 * t15 * t16 * (-t21 + t143 * t39 - t145 * t9 * t4
     #4 / 0.2D1) - 0.180D3 * t51 * t52 * (-t53 + t143 * t44) - t68) * t1
     #36 / 0.720D3
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
      t176 = t6 * x1
      t178 = s * t173 * t11 * t176 * t168
      t180 = 0.1D1 / (-0.2D1 + t165)
      t181 = t173 * t180
      t182 = rrgg2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #4)
      t184 = t173 ** 2
      t186 = t31 * t168 * t184
      t189 = log(-0.4D1 * t29 * t186)
      t190 = t189 * t173
      t191 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #4)
      t194 = t189 ** 2
      t196 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #4)
      t197 = t180 * t196
      t204 = t181 * t191
      t211 = t16 * t173 * t197
      t216 = t119 * t26 * t28
      t219 = log(-0.4D1 * t216 * t186)
      t232 = (0.90D2 * t15 * t16 * (t181 * t182 - t190 * t180 * t191 + t
     #194 * t173 * t197 / 0.2D1) - 0.180D3 * t51 * t52 * (t204 - t190 * 
     #t197) + t65 * t211) * t70 / 0.720D3 - (0.90D2 * t15 * t16 * (-t204
     # + t219 * t173 * t197) + 0.180D3 * t132 * t211) * t136 * t70 / 0.7
     #20D3
      t233 = FJET(XB1, XB2, s, 0.0D0, t167, t170, -t172, t178, t232)
      t235 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t161)
      t237 = FJET(XB1, XB2, s, t167, 0.0D0, -t172, t170, t178, t232)
      t239 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t240 = s * t239
      t241 = x3 * t4
      t242 = t240 * t241
      t243 = -0.1D1 + x3
      t244 = t4 * t243
      t245 = t240 * t244
      t246 = t240 * t7
      t247 = t239 ** 2
      t251 = s * t247 * t11 * t176 * t243
      t253 = 0.1D1 / (-0.2D1 + t239)
      t254 = t247 * t253
      t255 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t256 = t254 * t255
      t257 = t30 * t243
      t258 = t247 ** 2
      t263 = log(-0.4D1 * t216 * t257 * x4 * t258)
      t265 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t266 = t253 * t265
      t273 = t16 * t247 * t266
      t279 = rrgg2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t285 = log(-0.4D1 * t120 * t73 * t243 * t258)
      t286 = t285 * t247
      t289 = t285 ** 2
      t306 = -(0.90D2 * t15 * t16 * (-t256 + t263 * t247 * t266) + 0.180
     #D3 * t132 * t273) * t136 * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (
     #t254 * t279 - t286 * t253 * t255 + t289 * t247 * t266 / 0.2D1) - 0
     #.180D3 * t51 * t52 * (t256 - t286 * t266) + t65 * t273) * t136 / 0
     #.720D3
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
      t333 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t164)
      t335 = t315 ** 2
      t340 = log(0.4D1 * t216 * t257 * t322 * t335)
      t342 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t164)
      t343 = t331 * t342
      t353 = 0.90D2 * t15 * t16 * (t315 * t331 * t333 - t340 * t315 * t3
     #43) - 0.180D3 * t132 * t16 * t315 * t343
      t356 = t353 * t136 * t70 / 0.720D3
      t357 = FJET(XB1, XB2, s, t311, -t312, t313, -t314, t329, -t356)
      t359 = t136 * t70
      t362 = FJET(XB1, XB2, s, -t245, t242, 0.0D0, -t246, t251, t306)
      t364 = FJET(XB1, XB2, s, -t312, t311, -t314, t313, t329, -t356)
      rrgg2gght3s1e1 = t162 * t161 + t233 * t232 + t235 * t161 + t237 * 
     #t232 + t307 * t306 - t357 * t353 * t359 / 0.720D3 + t362 * t306 - 
     #t364 * t353 * t359 / 0.720D3

      end function



      doubleprecision function rrgg2gght3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh31J1
      doubleprecision rrgg2ggh31J2
      doubleprecision rrgg2ggh31J3
      doubleprecision rrgg2ggh31J4
      doubleprecision rrgg2ggh31J5
      doubleprecision rrgg2ggh31J6
      doubleprecision rrgg2ggh31J7
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
      t20 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t38 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t78 = rrgg2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t85 = log(0.4D1 * t27 * t64)
      t86 = t85 * 0.3141592653589793D1
      t93 = lh ** 2
      t95 = 0.3141592653589793D1 ** 2
      t101 = t85 ** 2
      t110 = (0.90D2 * t15 * t16 * (-t21 + t36 * t9 * t39) + t50) * t52 
     #/ 0.720D3 - t15 * t47 * t39 * t57 / 0.8D1 + (0.90D2 * t15 * t16 * 
     #(-t21 + t67 * t9 * t39) + t50) * t56 / 0.720D3 - t15 * t16 * t19 *
     # t78 / 0.8D1 - (-0.180D3 * t45 - 0.90D2 * t86) * t11 * t16 * t21 /
     # 0.720D3 - (0.3141592653589793D1 * (0.180D3 * t93 - 0.30D2 * t95) 
     #+ 0.180D3 * t86 * lh + 0.45D2 * t101 * 0.3141592653589793D1) * t11
     # * t16 * t19 * t38 / 0.720D3
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
      t125 = t6 * x1
      t127 = s * t122 * t11 * t125 * t117
      t129 = 0.1D1 / (-0.2D1 + t114)
      t131 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #3)
      t133 = t122 ** 2
      t138 = log(-0.4D1 * t29 * t31 * t117 * t133)
      t140 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #3)
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
      t167 = x3 * t4
      t168 = t166 * t167
      t169 = -0.1D1 + x3
      t170 = t4 * t169
      t171 = t166 * t170
      t172 = t166 * t7
      t173 = t165 ** 2
      t177 = s * t173 * t11 * t125 * t169
      t178 = t16 * t173
      t181 = 0.1D1 / (-0.2D1 + t165)
      t182 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t183 = t181 * t182
      t188 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t190 = t173 ** 2
      t195 = log(-0.4D1 * t62 * t63 * t169 * t190)
      t208 = t15 * t178 * t183 * t57 / 0.8D1 + (0.90D2 * t15 * t16 * (t1
     #73 * t181 * t188 - t195 * t173 * t183) - 0.180D3 * t46 * t178 * t1
     #83) * t56 / 0.720D3
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
      t236 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t113)
      t240 = t15 * t16 * t217 * t235 * t236 * t57 / 0.8D1
      t241 = FJET(XB1, XB2, s, t213, -t214, t215, -t216, t231, -t240)
      t243 = t11 * t16
      t248 = t217 * t235 * t236 * t56 * t52
      t251 = FJET(XB1, XB2, s, -t171, t168, 0.0D0, -t172, t177, t208)
      t253 = FJET(XB1, XB2, s, -t214, t213, -t216, t215, t231, -t240)
      rrgg2gght3s1e0 = t111 * t110 + t159 * t158 + t161 * t110 + t163 * 
     #t158 + t209 * t208 - t241 * 0.3141592653589793D1 * t243 * t248 / 0
     #.8D1 + t251 * t208 - t253 * 0.3141592653589793D1 * t243 * t248 / 0
     #.8D1

      end function



      doubleprecision function rrgg2gght3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh31J1
      doubleprecision rrgg2ggh31J2
      doubleprecision rrgg2ggh31J3
      doubleprecision rrgg2ggh31J4
      doubleprecision rrgg2ggh31J5
      doubleprecision rrgg2ggh31J6
      doubleprecision rrgg2ggh31J7
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
      t21 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t22 = 0.1D1 / x3
      t27 = rrgg2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t75 = t6 * x1
      t77 = s * t72 * t11 * t75 * t67
      t81 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t63)
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
      t115 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t117 = t107 / (-0.2D1 + t99) * t115 * t22
      t119 = t17 * t117 / 0.8D1
      t120 = FJET(XB1, XB2, s, t102, -t105, -t106, 0.0D0, t111, t119)
      t125 = FJET(XB1, XB2, s, -t105, t102, 0.0D0, -t106, t111, t119)
      rrgg2gght3s1em1 = t61 * t60 + t86 * 0.3141592653589793D1 * t88 * t
     #83 / 0.8D1 + t92 * t60 + t94 * 0.3141592653589793D1 * t88 * t83 / 
     #0.8D1 + t120 * 0.3141592653589793D1 * t88 * t117 / 0.8D1 + t125 * 
     #0.3141592653589793D1 * t88 * t117 / 0.8D1

      end function



      doubleprecision function rrgg2gght3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh31J1
      doubleprecision rrgg2ggh31J2
      doubleprecision rrgg2ggh31J3
      doubleprecision rrgg2ggh31J4
      doubleprecision rrgg2ggh31J5
      doubleprecision rrgg2ggh31J6
      doubleprecision rrgg2ggh31J7
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
      t21 = rrgg2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t24 = 0.3141592653589793D1 * t11 * t16 * t9 * t19 * t21 / 0.8D1
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, -t24)
      t30 = t16 * t9 * t19 * t21
      t32 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, -t24)
      rrgg2gght3s1em2 = -t25 * 0.3141592653589793D1 * t11 * t30 / 0.8D1 
     #- t32 * 0.3141592653589793D1 * t11 * t30 / 0.8D1

      end function



      doubleprecision function rrgg2gght3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh31J1
      doubleprecision rrgg2ggh31J2
      doubleprecision rrgg2ggh31J3
      doubleprecision rrgg2ggh31J4
      doubleprecision rrgg2ggh31J5
      doubleprecision rrgg2ggh31J6
      doubleprecision rrgg2ggh31J7
      rrgg2gght3s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh31J1
      doubleprecision rrgg2ggh31J2
      doubleprecision rrgg2ggh31J3
      doubleprecision rrgg2ggh31J4
      doubleprecision rrgg2ggh31J5
      doubleprecision rrgg2ggh31J6
      doubleprecision rrgg2ggh31J7
      rrgg2gght3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh31J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = 0.1D1 - x1
      t12 = t3 * t11
      t13 = t12 * x4
      t15 = 0.1D1 - x4
      t18 = s - t2 * t13 - t2 * t12 * t15
      t19 = t10 * t18
      t20 = s ** 2
      t21 = t20 * s
      t22 = t19 * t21
      t23 = t1 * t3
      t24 = x1 * t7
      t25 = t23 * t24
      t28 = t1 ** 2
      t29 = t3 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t7 ** 2
      t33 = t31 * t32
      t34 = t30 * t33
      t37 = t11 * x4
      t38 = t23 * t37
      t41 = t11 ** 2
      t42 = x4 ** 2
      t43 = t41 * t42
      t44 = t30 * t43
      t47 = t20 ** 2
      t48 = t10 * t47
      t49 = t28 ** 2
      t51 = t29 ** 2
      t52 = t51 * t31
      t56 = cos(x2 * 0.3141592653589793D1)
      t60 = Sqrt(x3 * t7 * x4 * t15)
      t63 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t56 * t60
      t64 = t63 ** 2
      t65 = t41 * t64
      t66 = t52 * t65
      t71 = t11 * t63
      t72 = t29 * x1 * t71
      t75 = t18 * t47
      t82 = t48 * z
      t91 = t75 * z
      t98 = -0.2D1 * t22 * t25 - 0.9D1 * t22 * t34 - 0.2D1 * t22 * t38 -
     # 0.9D1 * t22 * t44 + 0.2D1 * t48 * t49 * t66 - 0.4D1 * t48 * t28 *
     # t72 + 0.2D1 * t75 * t49 * t66 - 0.4D1 * t75 * t28 * t72 + 0.2D1 *
     # t82 * t34 - 0.4D1 * t82 * t25 - 0.2D1 * t82 * t44 + 0.4D1 * t82 *
     # t38 - 0.2D1 * t91 * t34 + 0.4D1 * t91 * t25 + 0.2D1 * t91 * t44
      t102 = z ** 2
      t103 = t47 * s * t102
      t113 = t28 * t1
      t114 = z * t113
      t115 = t48 * t114
      t116 = t29 * t3
      t118 = x4 * x1
      t120 = t116 * t41 * t118 * t63
      t123 = t21 * z
      t125 = t123 * t19 * t113
      t127 = z * t28
      t128 = t48 * t127
      t131 = t29 * t11 * t118 * t7
      t138 = t123 * t19
      t143 = t116 * t31
      t146 = t143 * t7 * t11 * t63
      t155 = t75 * t113 * t116
      t156 = t31 * t7
      t162 = t49 * t51
      t164 = t41 * t11
      t166 = x1 * t63
      t167 = t164 * t42 * t166
      t170 = -0.4D1 * t91 * t38 - 0.4D1 * t103 * t28 * t72 + 0.2D1 * t10
     #3 * t49 * t66 - 0.2D1 * t82 - 0.2D1 * t91 - 0.9D1 * t22 - 0.4D1 * 
     #t115 * t120 - t125 * t120 + 0.4D1 * t128 * t131 + 0.4D1 * t123 * t
     #19 * t28 * t131 + 0.2D1 * t138 * t34 + 0.2D1 * t138 * t44 - 0.4D1 
     #* t115 * t146 + 0.4D1 * t75 * t30 * t24 * t37 * z - 0.4D1 * t155 *
     # t156 * z * t11 * t63 + 0.2D1 * t48 * t162 * t167
      t172 = t75 * t162
      t175 = t49 * t1
      t176 = t51 * t3
      t184 = t41 * x4
      t192 = t19 * t21 * t49
      t199 = t19 * t21 * t113
      t202 = t47 * t113
      t205 = t7 * t10
      t209 = t202 * t143
      t213 = t47 * t49
      t214 = t31 * x1
      t217 = t32 * t10
      t225 = t184 * t63
      t249 = 0.2D1 * t172 * t167 - 0.2D1 * t75 * t175 * t176 * t164 * x4
     # * t31 * t64 + 0.4D1 * t155 * t184 * t166 - 0.2D1 * t155 * t24 * t
     #43 - 0.9D1 * t192 * t66 + 0.4D1 * t75 * t127 * t72 - 0.2D1 * t199 
     #* t120 + 0.4D1 * t202 * t116 * x1 * t205 * t43 + 0.4D1 * t209 * t2
     #05 * t71 + 0.2D1 * t213 * t51 * t214 * t217 * t71 - 0.2D1 * t209 *
     # t217 * t37 - 0.4D1 * t213 * t52 * t205 * t225 - 0.2D1 * t47 * t17
     #5 * t176 * t214 * t205 * t65 - 0.2D1 * t48 * t113 * t116 * t164 * 
     #t42 * x4 - 0.2D1 * t75 * t1 * t13 - 0.2D1 * t47 * t1 * t3 * t24 * 
     #t10
      t262 = z * t49
      t273 = t19 * t21 * t28
      t293 = -0.2D1 * t75 * t113 * t116 * t214 * t32 * t7 - t125 * t146 
     #- 0.4D1 * t75 * t114 * t120 + 0.4D1 * t128 * t72 + 0.2D1 * t48 * t
     #262 * t66 + 0.2D1 * t75 * t262 * t66 + t192 * t52 * t65 * z + 0.13
     #D2 * t273 * t72 + 0.2D1 * t172 * t214 * t32 * t71 - 0.4D1 * t172 *
     # t156 * t225 + 0.4D1 * t155 * t33 * t37 - 0.2D1 * t199 * t146 + 0.
     #13D2 * t273 * t131 + 0.2D1 * t103 + 0.2D1 * t48 + 0.2D1 * t75
      rrgg2ggh31J1 = 0.9D1 * wd * (t98 + t170 + t249 + t293) / t20 / t10
     # / t18 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh31J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t6 = t5 * x4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t6 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = z * t16
      t19 = t3 ** 2
      t20 = t19 ** 2
      t21 = x1 ** 2
      t22 = t20 * t21
      t23 = t4 ** 2
      t27 = cos(x2 * 0.3141592653589793D1)
      t28 = 0.1D1 - x3
      t32 = Sqrt(x3 * t28 * x4 * t8)
      t35 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t27 * t32
      t36 = t35 ** 2
      t37 = t23 * t36
      t38 = t22 * t37
      t40 = 0.2D1 * t14 * t17 * t38
      t41 = t3 * x1
      t46 = s - t2 * t41 * x3 - t2 * t41 * t28
      t47 = t46 * t11
      t48 = t12 * s
      t50 = t47 * t48 * t16
      t53 = t50 * t22 * t37 * z
      t54 = z * t15
      t57 = t4 * t35
      t58 = t19 * x1 * t57
      t59 = t14 * t54 * t58
      t61 = t15 * t1
      t63 = t47 * t48 * t61
      t64 = t19 * t3
      t66 = x4 * x1
      t68 = t64 * t23 * t66 * t35
      t69 = t63 * t68
      t71 = t46 * t13
      t72 = z * t61
      t73 = t71 * t72
      t74 = t73 * t68
      t76 = t48 * z
      t78 = t76 * t47 * t61
      t79 = t78 * t68
      t80 = t64 * t21
      t83 = t80 * t28 * t4 * t35
      t84 = t78 * t83
      t87 = 0.4D1 * t14 * t72 * t68
      t88 = t71 * t54
      t89 = t88 * t58
      t92 = t23 * t4
      t94 = x4 ** 2
      t98 = 0.2D1 * t71 * t61 * t64 * t92 * t94 * x4
      t101 = 0.2D1 * t14 * t1 * t6
      t102 = t47 * t48
      t103 = t15 * t19
      t104 = t28 ** 2
      t105 = t21 * t104
      t106 = t103 * t105
      t107 = t102 * t106
      t109 = t1 * t3
      t110 = x1 * t28
      t111 = t109 * t110
      t112 = t102 * t111
      t114 = t23 * t94
      t115 = t103 * t114
      t116 = t102 * t115
      t118 = t4 * x4
      t119 = t109 * t118
      t120 = t102 * t119
      t122 = t40 + t53 + 0.4D1 * t59 - 0.2D1 * t69 - 0.4D1 * t74 - t79 -
     # t84 - t87 + 0.4D1 * t89 - t98 - t101 - 0.9D1 * t107 - 0.2D1 * t11
     #2 - 0.9D1 * t116 - 0.2D1 * t120
      t125 = 0.2D1 * t71 * t16 * t38
      t128 = 0.4D1 * t14 * t15 * t58
      t131 = 0.2D1 * t14 * t16 * t38
      t134 = 0.4D1 * t71 * t15 * t58
      t135 = t71 * z
      t137 = 0.2D1 * t135 * t106
      t138 = t13 * t16
      t140 = t28 * t46
      t141 = t23 * x4
      t142 = t141 * t35
      t145 = 0.4D1 * t138 * t22 * t140 * t142
      t146 = t16 * t1
      t148 = t20 * t3
      t149 = t21 * x1
      t154 = 0.2D1 * t13 * t146 * t148 * t149 * t140 * t37
      t155 = t135 * t111
      t157 = t135 * t115
      t159 = t135 * t119
      t162 = t14 * t61 * t64
      t163 = x1 * t35
      t166 = 0.4D1 * t162 * t141 * t163
      t169 = t19 * t4 * t66 * t28
      t170 = t88 * t169
      t174 = t76 * t47 * t15 * t169
      t176 = t76 * t47
      t178 = 0.2D1 * t176 * t106
      t180 = 0.2D1 * t176 * t115
      t184 = t14 * t103 * t110 * t118 * z
      t186 = t125 - t128 + t131 - t134 + t137 - t145 - t154 - 0.4D1 * t1
     #55 - 0.2D1 * t157 + 0.4D1 * t159 + t166 + 0.4D1 * t170 + 0.4D1 * t
     #174 + t178 + t180 + 0.4D1 * t184
      t188 = t14 * z
      t189 = t188 * t106
      t192 = z ** 2
      t193 = t13 * s * t192
      t196 = 0.2D1 * t193 * t16 * t38
      t199 = 0.4D1 * t193 * t15 * t58
      t200 = t188 * t119
      t203 = 0.2D1 * t188 * t115
      t204 = t188 * t111
      t206 = t16 * t20
      t207 = t14 * t206
      t208 = t21 * t28
      t211 = 0.4D1 * t207 * t208 * t142
      t214 = 0.4D1 * t162 * t105 * t118
      t215 = t13 * t61
      t216 = t215 * t80
      t217 = t104 * t46
      t220 = 0.2D1 * t216 * t217 * t118
      t223 = 0.2D1 * t162 * t110 * t114
      t224 = t50 * t38
      t226 = t63 * t83
      t230 = 0.2D1 * t71 * t17 * t38
      t232 = t47 * t48 * t15
      t233 = t232 * t169
      t237 = 0.4D1 * t216 * t140 * t57
      t242 = 0.2D1 * t138 * t20 * t149 * t217 * t57
      t243 = -0.2D1 * t189 + t196 - t199 - 0.4D1 * t200 + t203 + 0.4D1 *
     # t204 - t211 + t214 - t220 - t223 - 0.9D1 * t224 - 0.2D1 * t226 + 
     #t230 + 0.13D2 * t233 + t237 + t242
      t252 = 0.2D1 * t14 * t61 * t64 * t149 * t104 * t28
      t254 = t92 * t94 * t163
      t256 = 0.2D1 * t207 * t254
      t263 = 0.2D1 * t14 * t146 * t148 * t92 * x4 * t21 * t36
      t268 = 0.2D1 * t13 * t1 * t3 * t110 * t46
      t272 = t162 * t208 * z * t4 * t35
      t276 = 0.2D1 * t71 * t206 * t254
      t278 = 0.4D1 * t73 * t83
      t283 = 0.4D1 * t215 * t64 * x1 * t140 * t114
      t284 = t232 * t58
      t289 = 0.2D1 * t207 * t149 * t104 * t57
      t290 = 0.2D1 * t193
      t291 = 0.2D1 * t71
      t292 = 0.2D1 * t14
      t293 = -0.2D1 * t135 - 0.2D1 * t188 - 0.9D1 * t102 - t252 + t256 -
     # t263 - t268 - 0.4D1 * t272 + t276 - t278 + t283 + 0.13D2 * t284 +
     # t289 + t290 + t291 + t292
      t306 = -t40 - t53 - 0.3D1 * t59 - 0.4D1 * t69 + 0.5D1 * t74 + t79 
     #+ t84 + t87 - 0.3D1 * t89 + t98 + t101 + 0.12D2 * t107 - 0.4D1 * t
     #112 + 0.12D2 * t116 - 0.4D1 * t120
      t313 = -t125 + t128 - t131 + t134 - t137 + t145 + t154 + 0.5D1 * t
     #155 + 0.3D1 * t157 - 0.2D1 * t159 - t166 - 0.3D1 * t170 - 0.7D1 * 
     #t174 - t178 - t180 - 0.3D1 * t184
      t321 = 0.3D1 * t189 - t196 + t199 + 0.5D1 * t200 - t203 - 0.2D1 * 
     #t204 + t211 - t214 + t220 + t223 + 0.12D2 * t224 - 0.4D1 * t226 - 
     #t230 - 0.7D1 * t233 - t237 - t242
      t327 = 0.3D1 * t135 + 0.3D1 * t188 + 0.12D2 * t102 + t252 - t256 +
     # t263 + t268 + 0.5D1 * t272 - t276 + t278 - t283 - 0.7D1 * t284 - 
     #t289 - t290 - t291 - t292
      rrgg2ggh31J2 = 0.9D1 * (0.2D1 * wd * (t122 + t186 + t243 + t293) +
     # wd * (t306 + t313 + t321 + t327)) / t12 / t46 / t11 / z / 0.31415
     #92653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh31J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t6 = t5 * x4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t6 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 * t1
      t17 = t3 ** 2
      t18 = t17 * t3
      t20 = t14 * t16 * t18
      t21 = x1 ** 2
      t22 = 0.1D1 - x3
      t23 = t21 * t22
      t28 = cos(x2 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t22 * x4 * t8)
      t35 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t32
      t38 = t20 * t23 * z * t4 * t35
      t40 = t15 ** 2
      t41 = t40 * t1
      t43 = t17 ** 2
      t44 = t43 * t3
      t45 = t21 * x1
      t48 = t3 * x1
      t53 = s - t2 * t48 * x3 - t2 * t48 * t22
      t54 = t22 * t53
      t55 = t4 ** 2
      t56 = t35 ** 2
      t57 = t55 * t56
      t60 = 0.2D1 * t13 * t41 * t44 * t45 * t54 * t57
      t61 = t22 ** 2
      t62 = t21 * t61
      t63 = t4 * x4
      t66 = 0.4D1 * t20 * t62 * t63
      t67 = t12 * s
      t68 = t67 * z
      t69 = t53 * t11
      t71 = t68 * t69 * t16
      t73 = x4 * x1
      t75 = t18 * t55 * t73 * t35
      t76 = t71 * t75
      t77 = t18 * t21
      t80 = t77 * t22 * t4 * t35
      t81 = t71 * t80
      t82 = z * t16
      t85 = 0.4D1 * t14 * t82 * t75
      t86 = t53 * t13
      t87 = z * t15
      t88 = t86 * t87
      t90 = t4 * t35
      t91 = t17 * x1 * t90
      t92 = t88 * t91
      t94 = z * t40
      t96 = t43 * t21
      t97 = t96 * t57
      t99 = 0.2D1 * t86 * t94 * t97
      t100 = x1 * t22
      t101 = x4 ** 2
      t102 = t55 * t101
      t105 = 0.2D1 * t20 * t100 * t102
      t107 = t69 * t67 * t40
      t108 = t107 * t97
      t111 = t14 * t87 * t91
      t114 = t69 * t67 * t16
      t115 = t114 * t75
      t117 = t13 * t16
      t122 = 0.4D1 * t117 * t18 * x1 * t54 * t102
      t126 = t13 * t1 * t3 * t100 * t53
      t127 = 0.2D1 * t126
      t132 = t14 * t16 * t18 * t45 * t61 * t22
      t133 = 0.2D1 * t132
      t134 = -0.4D1 * t38 - t60 + t66 - t76 - t81 - t85 + 0.4D1 * t92 + 
     #t99 - t105 - 0.9D1 * t108 + 0.4D1 * t111 - 0.2D1 * t115 + t122 - t
     #127 - t133
      t135 = t117 * t77
      t136 = t61 * t53
      t139 = 0.2D1 * t135 * t136 * t63
      t140 = t13 * t40
      t142 = t55 * x4
      t143 = t142 * t35
      t146 = 0.4D1 * t140 * t96 * t54 * t143
      t147 = t15 * t17
      t151 = t14 * t147 * t100 * t63 * z
      t153 = t114 * t80
      t155 = x1 * t35
      t158 = 0.4D1 * t20 * t142 * t155
      t161 = 0.2D1 * t14 * t94 * t97
      t164 = t107 * t96 * t57 * z
      t166 = t55 * t4
      t170 = t86 * t16 * t18 * t166 * t101 * x4
      t171 = 0.2D1 * t170
      t173 = t14 * t1 * t6
      t174 = 0.2D1 * t173
      t176 = t69 * t67 * t15
      t177 = t176 * t91
      t179 = t40 * t43
      t180 = t14 * t179
      t183 = t180 * t45 * t61 * t90
      t184 = 0.2D1 * t183
      t187 = 0.4D1 * t180 * t23 * t143
      t190 = t17 * t4 * t73 * t22
      t191 = t88 * t190
      t195 = t68 * t69 * t15 * t190
      t197 = t68 * t69
      t198 = t147 * t62
      t200 = 0.2D1 * t197 * t198
      t201 = t147 * t102
      t203 = 0.2D1 * t197 * t201
      t204 = -t139 - t146 + 0.4D1 * t151 - 0.2D1 * t153 + t158 + t161 + 
     #t164 - t171 - t174 + 0.13D2 * t177 + t184 - t187 + 0.4D1 * t191 + 
     #0.4D1 * t195 + t200 + t203
      t206 = t86 * t82
      t208 = 0.4D1 * t206 * t80
      t209 = t206 * t75
      t211 = t176 * t190
      t215 = 0.4D1 * t135 * t54 * t90
      t220 = 0.2D1 * t140 * t43 * t45 * t136 * t90
      t221 = t69 * t67
      t223 = t1 * t3
      t224 = t223 * t100
      t225 = t221 * t224
      t227 = t86 * z
      t229 = t14 * z
      t231 = t221 * t198
      t235 = 0.2D1 * t86 * t40 * t97
      t238 = 0.4D1 * t86 * t15 * t91
      t241 = 0.2D1 * t14 * t40 * t97
      t244 = 0.4D1 * t14 * t15 * t91
      t246 = 0.2D1 * t227 * t198
      t247 = t227 * t224
      t249 = -t208 - 0.4D1 * t209 + 0.13D2 * t211 + t215 + t220 - 0.9D1 
     #* t221 - 0.2D1 * t225 - 0.2D1 * t227 - 0.2D1 * t229 - 0.9D1 * t231
     # + t235 - t238 + t241 - t244 + t246 - 0.4D1 * t247
      t250 = t227 * t201
      t252 = t223 * t63
      t253 = t227 * t252
      t254 = 0.4D1 * t253
      t255 = t229 * t198
      t257 = t229 * t224
      t258 = 0.4D1 * t257
      t260 = 0.2D1 * t229 * t201
      t261 = t229 * t252
      t264 = z ** 2
      t265 = t13 * s * t264
      t268 = 0.4D1 * t265 * t15 * t91
      t271 = 0.2D1 * t265 * t40 * t97
      t274 = t166 * t101 * t155
      t275 = t86 * t179 * t274
      t276 = 0.2D1 * t275
      t278 = 0.2D1 * t180 * t274
      t285 = 0.2D1 * t14 * t41 * t44 * t166 * x4 * t21 * t56
      t286 = t221 * t201
      t288 = t221 * t252
      t290 = 0.2D1 * t265
      t291 = 0.2D1 * t86
      t292 = 0.2D1 * t14
      t293 = -0.2D1 * t250 + t254 - 0.2D1 * t255 + t258 + t260 - 0.4D1 *
     # t261 - t268 + t271 + t276 + t278 - t285 - 0.9D1 * t286 - 0.2D1 * 
     #t288 + t290 + t291 + t292
      t302 = 0.4D1 * t115
      t303 = 0.5D1 * t38 + t60 - t66 + t76 + t81 + t85 - 0.3D1 * t92 - t
     #99 + t105 + 0.12D2 * t108 - 0.3D1 * t111 - t302 - t122 + t127 + t1
     #33
      t305 = 0.4D1 * t153
      t309 = t139 + t146 - 0.3D1 * t151 - t305 - t158 - t161 - t164 + t1
     #71 + t174 - 0.7D1 * t177 - t184 + t187 - 0.3D1 * t191 - 0.7D1 * t1
     #95 - t200 - t203
      t314 = 0.4D1 * t225
      t319 = t208 + 0.5D1 * t209 - 0.7D1 * t211 - t215 - t220 + 0.12D2 *
     # t221 - t314 + 0.3D1 * t227 + 0.3D1 * t229 + 0.12D2 * t231 - t235 
     #+ t238 - t241 + t244 - t246 + 0.5D1 * t247
      t326 = 0.4D1 * t288
      t327 = 0.3D1 * t250 - 0.2D1 * t253 + 0.3D1 * t255 - 0.2D1 * t257 -
     # t260 + 0.5D1 * t261 + t268 - t271 - t276 - t278 + t285 + 0.12D2 *
     # t286 - t326 - t290 - t291 - t292
      t340 = -t38 - t92 - 0.2D1 * t108 - t111 - t302 - 0.4D1 * t126 - 0.
     #4D1 * t132 - t151 - t305 - 0.4D1 * t170 - 0.4D1 * t173 + 0.12D2 * 
     #t177 + 0.4D1 * t183 - t191 - t209 + 0.12D2 * t211
      t352 = -0.2D1 * t221 - t314 - 0.6D1 * t227 - 0.6D1 * t229 - 0.2D1 
     #* t231 - t247 - 0.6D1 * t250 + t254 - 0.6D1 * t255 + t258 - t261 +
     # 0.4D1 * t275 - 0.2D1 * t286 - t326 + 0.4D1 * t265 + 0.4D1 * t86 +
     # 0.4D1 * t14
      rrgg2ggh31J3 = 0.9D1 * (0.3D1 * wd * (t134 + t204 + t249 + t293) +
     # 0.2D1 * wd * (t303 + t309 + t319 + t327) + wd * (t340 + t352)) / 
     #t12 / t53 / t11 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh31J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t6 = t5 * x4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t6 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t3 ** 2
      t17 = t15 * t16
      t19 = 0.1D1 - x3
      t20 = x1 * t19
      t21 = t4 * x4
      t24 = t14 * t17 * t20 * t21 * z
      t26 = t15 * t1
      t27 = t16 * t3
      t29 = t14 * t26 * t27
      t30 = x1 ** 2
      t31 = t30 * t19
      t36 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t19 * x4 * t8)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t36 * t40
      t46 = t29 * t31 * z * t4 * t43
      t48 = t12 * s
      t49 = t48 * z
      t50 = t3 * x1
      t55 = s - t2 * t50 * x3 - t2 * t50 * t19
      t56 = t55 * t11
      t58 = t49 * t56 * t26
      t59 = t27 * t30
      t60 = t19 * t4
      t62 = t59 * t60 * t43
      t63 = t58 * t62
      t64 = z * t26
      t66 = t4 ** 2
      t70 = t27 * t66 * x4 * x1 * t43
      t72 = 0.4D1 * t14 * t64 * t70
      t73 = t55 * t13
      t74 = z * t15
      t75 = t73 * t74
      t76 = t16 * x1
      t77 = t4 * t43
      t78 = t76 * t77
      t79 = t75 * t78
      t81 = t15 ** 2
      t82 = z * t81
      t84 = t16 ** 2
      t85 = t84 * t30
      t86 = t43 ** 2
      t87 = t66 * t86
      t88 = t85 * t87
      t90 = 0.2D1 * t73 * t82 * t88
      t91 = 0.2D1 * t14
      t93 = t30 * x1
      t95 = t19 ** 2
      t98 = t14 * t26 * t27 * t93 * t95 * t19
      t99 = 0.2D1 * t98
      t101 = t56 * t48 * t15
      t103 = t76 * t60 * x4
      t104 = t101 * t103
      t106 = t56 * t48
      t107 = t1 * t3
      t108 = t107 * t20
      t109 = t106 * t108
      t111 = t30 * t95
      t112 = t17 * t111
      t113 = t106 * t112
      t115 = t107 * t21
      t116 = t106 * t115
      t118 = x4 ** 2
      t119 = t66 * t118
      t120 = t17 * t119
      t121 = t106 * t120
      t125 = 0.2D1 * t73 * t81 * t88
      t128 = 0.4D1 * t73 * t15 * t78
      t129 = 0.4D1 * t24 - 0.4D1 * t46 - t63 - t72 + 0.4D1 * t79 + t90 +
     # t91 - t99 + 0.13D2 * t104 - 0.2D1 * t109 - 0.9D1 * t113 - 0.2D1 *
     # t116 - 0.9D1 * t121 + t125 - t128
      t132 = 0.2D1 * t14 * t81 * t88
      t135 = 0.4D1 * t14 * t15 * t78
      t136 = t73 * z
      t138 = 0.2D1 * t136 * t112
      t139 = t136 * t108
      t141 = t136 * t120
      t143 = t136 * t115
      t144 = 0.4D1 * t143
      t145 = t14 * z
      t146 = t145 * t112
      t148 = t145 * t108
      t149 = 0.4D1 * t148
      t151 = 0.2D1 * t145 * t120
      t152 = t145 * t115
      t155 = z ** 2
      t156 = t13 * s * t155
      t159 = 0.4D1 * t156 * t15 * t78
      t162 = 0.2D1 * t156 * t81 * t88
      t163 = t81 * t84
      t164 = t14 * t163
      t165 = t66 * x4
      t166 = t165 * t43
      t169 = 0.4D1 * t164 * t31 * t166
      t172 = 0.4D1 * t29 * t111 * t21
      t174 = t56 * t48 * t26
      t175 = t174 * t62
      t178 = t132 - t135 + t138 - 0.4D1 * t139 - 0.2D1 * t141 + t144 - 0
     #.2D1 * t146 + t149 + t151 - 0.4D1 * t152 - t159 + t162 - t169 + t1
     #72 - 0.2D1 * t175 - 0.2D1 * t136
      t184 = t49 * t56 * t15 * t103
      t186 = t49 * t56
      t188 = 0.2D1 * t186 * t112
      t190 = 0.2D1 * t186 * t120
      t191 = t66 * t4
      t193 = x1 * t43
      t194 = t191 * t118 * t193
      t196 = 0.2D1 * t164 * t194
      t197 = t81 * t1
      t198 = t84 * t3
      t205 = 0.2D1 * t14 * t197 * t198 * t191 * x4 * t30 * t86
      t208 = 0.4D1 * t29 * t165 * t193
      t209 = t75 * t103
      t211 = t13 * t26
      t212 = t211 * t59
      t213 = t19 * t55
      t216 = 0.4D1 * t212 * t213 * t77
      t217 = t13 * t81
      t220 = t95 * t55
      t223 = 0.2D1 * t217 * t84 * t93 * t220 * t77
      t224 = 0.2D1 * t156
      t225 = 0.2D1 * t73
      t226 = t73 * t64
      t228 = 0.4D1 * t226 * t62
      t229 = t226 * t70
      t231 = t58 * t70
      t232 = -0.2D1 * t145 - 0.9D1 * t106 + 0.4D1 * t184 + t188 + t190 +
     # t196 - t205 + t208 + 0.4D1 * t209 + t216 + t223 + t224 + t225 - t
     #228 - 0.4D1 * t229 - t231
      t235 = 0.2D1 * t14 * t82 * t88
      t237 = t56 * t48 * t81
      t240 = t237 * t85 * t87 * z
      t242 = t14 * t74 * t78
      t244 = t174 * t70
      t250 = 0.4D1 * t211 * t27 * x1 * t213 * t119
      t251 = t101 * t78
      t255 = t164 * t93 * t95 * t77
      t256 = 0.2D1 * t255
      t261 = t73 * t26 * t27 * t191 * t118 * x4
      t262 = 0.2D1 * t261
      t264 = t14 * t1 * t6
      t265 = 0.2D1 * t264
      t269 = t13 * t1 * t3 * t20 * t55
      t270 = 0.2D1 * t269
      t272 = t73 * t163 * t194
      t273 = 0.2D1 * t272
      t276 = 0.2D1 * t29 * t20 * t119
      t277 = t237 * t88
      t281 = 0.2D1 * t212 * t220 * t21
      t285 = 0.4D1 * t217 * t85 * t213 * t166
      t291 = 0.2D1 * t13 * t197 * t198 * t93 * t213 * t87
      t292 = t235 + t240 + 0.4D1 * t242 - 0.2D1 * t244 + t250 + 0.13D2 *
     # t251 + t256 - t262 - t265 - t270 + t273 - t276 - 0.9D1 * t277 - t
     #281 - t285 - t291
      t301 = 0.4D1 * t109
      t303 = 0.4D1 * t116
      t305 = -0.3D1 * t24 + 0.5D1 * t46 + t63 + t72 - 0.3D1 * t79 - t90 
     #- t91 + t99 - 0.7D1 * t104 - t301 + 0.12D2 * t113 - t303 + 0.12D2 
     #* t121 - t125 + t128
      t312 = 0.4D1 * t175
      t314 = -t132 + t135 - t138 + 0.5D1 * t139 + 0.3D1 * t141 - 0.2D1 *
     # t143 + 0.3D1 * t146 - 0.2D1 * t148 - t151 + 0.5D1 * t152 + t159 -
     # t162 + t169 - t172 - t312 + 0.3D1 * t136
      t321 = 0.3D1 * t145 + 0.12D2 * t106 - 0.7D1 * t184 - t188 - t190 -
     # t196 + t205 - t208 - 0.3D1 * t209 - t216 - t223 - t224 - t225 + t
     #228 + 0.5D1 * t229 + t231
      t323 = 0.4D1 * t244
      t326 = -t235 - t240 - 0.3D1 * t242 - t323 - t250 - 0.7D1 * t251 - 
     #t256 + t262 + t265 + t270 - t273 + t276 + 0.12D2 * t277 + t281 + t
     #285 + t291
      t338 = -t24 - t46 - t79 + 0.4D1 * t14 - 0.4D1 * t98 + 0.12D2 * t10
     #4 - t301 - 0.2D1 * t113 - t303 - 0.2D1 * t121 - t139 - 0.6D1 * t14
     #1 + t144 - 0.6D1 * t146 + t149 - t152
      t351 = -t312 - 0.6D1 * t136 - 0.6D1 * t145 - 0.2D1 * t106 - t209 +
     # 0.4D1 * t156 + 0.4D1 * t73 - t229 - t242 - t323 + 0.12D2 * t251 +
     # 0.4D1 * t255 - 0.4D1 * t261 - 0.4D1 * t264 - 0.4D1 * t269 + 0.4D1
     # * t272 - 0.2D1 * t277
      rrgg2ggh31J4 = 0.9D1 * (0.4D1 * wd * (t129 + t178 + t232 + t292) +
     # 0.3D1 * wd * (t305 + t314 + t321 + t326) + 0.2D1 * wd * (t338 + t
     #351)) / t12 / t55 / t11 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh31J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t6 = t5 * x4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t6 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t3 ** 2
      t17 = t15 * t16
      t19 = 0.1D1 - x3
      t20 = x1 * t19
      t21 = t4 * x4
      t24 = t14 * t17 * t20 * t21 * z
      t26 = t15 * t1
      t27 = t16 * t3
      t29 = t14 * t26 * t27
      t30 = x1 ** 2
      t31 = t30 * t19
      t36 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t19 * x4 * t8)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t36 * t40
      t46 = t29 * t31 * z * t4 * t43
      t49 = z ** 2
      t50 = t13 * s * t49
      t51 = 0.2D1 * t50
      t52 = t3 * x1
      t57 = s - t2 * t52 * x3 - t2 * t52 * t19
      t58 = t57 * t13
      t59 = 0.2D1 * t58
      t60 = 0.2D1 * t14
      t63 = t4 * t43
      t64 = t16 * x1 * t63
      t66 = 0.4D1 * t58 * t15 * t64
      t67 = t15 ** 2
      t69 = t16 ** 2
      t70 = t69 * t30
      t71 = t4 ** 2
      t72 = t43 ** 2
      t73 = t71 * t72
      t74 = t70 * t73
      t76 = 0.2D1 * t14 * t67 * t74
      t79 = 0.4D1 * t14 * t15 * t64
      t80 = t58 * z
      t81 = t19 ** 2
      t82 = t30 * t81
      t83 = t17 * t82
      t85 = 0.2D1 * t80 * t83
      t86 = t1 * t3
      t87 = t86 * t20
      t88 = t80 * t87
      t90 = x4 ** 2
      t91 = t71 * t90
      t92 = t17 * t91
      t93 = t80 * t92
      t95 = t86 * t21
      t96 = t80 * t95
      t97 = 0.4D1 * t96
      t98 = t14 * z
      t99 = t98 * t83
      t101 = t98 * t87
      t102 = 0.4D1 * t101
      t104 = 0.2D1 * t98 * t92
      t105 = 0.4D1 * t24 - 0.4D1 * t46 + t51 + t59 + t60 - t66 + t76 - t
     #79 + t85 - 0.4D1 * t88 - 0.2D1 * t93 + t97 - 0.2D1 * t99 + t102 + 
     #t104
      t106 = t98 * t95
      t110 = 0.4D1 * t50 * t15 * t64
      t113 = 0.2D1 * t50 * t67 * t74
      t116 = 0.2D1 * t29 * t20 * t91
      t117 = t57 * t11
      t118 = t12 * s
      t119 = t117 * t118
      t120 = t119 * t87
      t122 = t119 * t83
      t124 = t119 * t95
      t126 = t119 * t92
      t130 = 0.2D1 * t58 * t67 * t74
      t131 = t67 * t1
      t132 = t69 * t3
      t135 = t71 * t4
      t140 = 0.2D1 * t14 * t131 * t132 * t135 * x4 * t30 * t72
      t141 = t71 * x4
      t142 = x1 * t43
      t145 = 0.4D1 * t29 * t141 * t142
      t149 = t13 * t1 * t3 * t20 * t57
      t150 = 0.2D1 * t149
      t152 = t30 * x1
      t156 = t14 * t26 * t27 * t152 * t81 * t19
      t157 = 0.2D1 * t156
      t158 = z * t15
      t159 = t58 * t158
      t161 = x4 * x1
      t163 = t16 * t4 * t161 * t19
      t164 = t159 * t163
      t166 = t118 * z
      t169 = t166 * t117 * t15 * t163
      t172 = t117 * t118 * t26
      t175 = t27 * t71 * t161 * t43
      t176 = t172 * t175
      t178 = -0.4D1 * t106 - t110 + t113 - t116 - 0.2D1 * t120 - 0.9D1 *
     # t122 - 0.2D1 * t124 - 0.9D1 * t126 + t130 - t140 + t145 - t150 - 
     #t157 + 0.4D1 * t164 + 0.4D1 * t169 - 0.2D1 * t176
      t180 = t13 * t26
      t183 = t19 * t57
      t186 = 0.4D1 * t180 * t27 * x1 * t183 * t91
      t188 = t117 * t118 * t15
      t189 = t188 * t64
      t191 = t69 * t67
      t192 = t14 * t191
      t195 = t192 * t152 * t81 * t63
      t196 = 0.2D1 * t195
      t197 = t141 * t43
      t200 = 0.4D1 * t192 * t31 * t197
      t202 = t117 * t118 * t67
      t203 = t202 * t74
      t205 = t27 * t30
      t206 = t180 * t205
      t207 = t81 * t57
      t210 = 0.2D1 * t206 * t207 * t21
      t211 = t13 * t67
      t215 = 0.4D1 * t211 * t70 * t183 * t197
      t221 = 0.2D1 * t13 * t131 * t132 * t152 * t183 * t73
      t226 = t58 * t26 * t27 * t135 * t90 * x4
      t227 = 0.2D1 * t226
      t229 = t14 * t1 * t6
      t230 = 0.2D1 * t229
      t233 = 0.4D1 * t29 * t82 * t21
      t236 = t205 * t19 * t4 * t43
      t237 = t172 * t236
      t242 = t188 * t163
      t244 = t186 + 0.13D2 * t189 + t196 - t200 - 0.9D1 * t203 - t210 - 
     #t215 - t221 - t227 - t230 + t233 - 0.2D1 * t237 - 0.2D1 * t80 - 0.
     #2D1 * t98 - 0.9D1 * t119 + 0.13D2 * t242
      t247 = 0.4D1 * t206 * t183 * t63
      t252 = 0.2D1 * t211 * t69 * t152 * t207 * t63
      t254 = t166 * t117 * t26
      t255 = t254 * t175
      t256 = t254 * t236
      t257 = z * t26
      t260 = 0.4D1 * t14 * t257 * t175
      t261 = t159 * t64
      t263 = t67 * z
      t266 = 0.2D1 * t58 * t263 * t74
      t269 = 0.2D1 * t14 * t263 * t74
      t272 = t202 * t70 * t73 * z
      t274 = t14 * t158 * t64
      t276 = t166 * t117
      t278 = 0.2D1 * t276 * t83
      t280 = 0.2D1 * t276 * t92
      t281 = t58 * t257
      t283 = 0.4D1 * t281 * t236
      t284 = t281 * t175
      t288 = t135 * t90 * t142
      t289 = t58 * t191 * t288
      t290 = 0.2D1 * t289
      t292 = 0.2D1 * t192 * t288
      t293 = t247 + t252 - t255 - t256 - t260 + 0.4D1 * t261 + t266 + t2
     #69 + t272 + 0.4D1 * t274 + t278 + t280 - t283 - 0.4D1 * t284 + t29
     #0 + t292
      t305 = -0.3D1 * t24 + 0.5D1 * t46 - t51 - t59 - t60 + t66 - t76 + 
     #t79 - t85 + 0.5D1 * t88 + 0.3D1 * t93 - 0.2D1 * t96 + 0.3D1 * t99 
     #- 0.2D1 * t101 - t104
      t307 = 0.4D1 * t120
      t309 = 0.4D1 * t124
      t313 = 0.4D1 * t176
      t314 = 0.5D1 * t106 + t110 - t113 + t116 - t307 + 0.12D2 * t122 - 
     #t309 + 0.12D2 * t126 - t130 + t140 - t145 + t150 + t157 - 0.3D1 * 
     #t164 - 0.7D1 * t169 - t313
      t318 = 0.4D1 * t237
      t323 = -t186 - 0.7D1 * t189 - t196 + t200 + 0.12D2 * t203 + t210 +
     # t215 + t221 + t227 + t230 - t233 - t318 + 0.3D1 * t80 + 0.3D1 * t
     #98 + 0.12D2 * t119 - 0.7D1 * t242
      t327 = -t247 - t252 + t255 + t256 + t260 - 0.3D1 * t261 - t266 - t
     #269 - t272 - 0.3D1 * t274 - t278 - t280 + t283 + 0.5D1 * t284 - t2
     #90 - t292
      t340 = -t24 - t46 + 0.4D1 * t50 + 0.4D1 * t58 + 0.4D1 * t14 - t88 
     #- 0.6D1 * t93 + t97 - 0.6D1 * t99 + t102 - t106 - t307 - 0.2D1 * t
     #122 - t309 - 0.2D1 * t126 - 0.4D1 * t149
      t352 = -0.4D1 * t156 - t164 - t313 + 0.12D2 * t189 + 0.4D1 * t195 
     #- 0.2D1 * t203 - 0.4D1 * t226 - 0.4D1 * t229 - t318 - 0.6D1 * t80 
     #- 0.6D1 * t98 - 0.2D1 * t119 + 0.12D2 * t242 - t261 - t274 - t284 
     #+ 0.4D1 * t289
      rrgg2ggh31J5 = 0.9D1 * (0.5D1 * wd * (t105 + t178 + t244 + t293) +
     # 0.4D1 * wd * (t305 + t314 + t323 + t327) + 0.3D1 * wd * (t340 + t
     #352)) / t12 / t57 / t11 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh31J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t3 * t4
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t12 = 0.1D1 - x3
      t13 = s * t3
      t14 = t7 * x1
      t19 = s - t13 * t14 * x3 - t13 * t14 * t12
      t20 = t12 * t19
      t21 = 0.1D1 - x1
      t22 = t21 ** 2
      t23 = x4 ** 2
      t24 = t22 * t23
      t28 = t7 * t21
      t29 = t28 * x4
      t31 = 0.1D1 - x4
      t34 = s - t13 * t29 - t13 * t28 * t31
      t35 = t19 * t34
      t36 = s * t1
      t38 = t35 * t36 * t4
      t43 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t12 * x4 * t31)
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t43 * t47
      t51 = t21 * t50
      t52 = t8 * x1 * t51
      t53 = t38 * t52
      t55 = t34 * t2
      t56 = t4 * t8
      t58 = x1 * t12
      t59 = t21 * x4
      t62 = t55 * t56 * t58 * t59 * z
      t64 = t4 ** 2
      t65 = t2 * t64
      t66 = t8 ** 2
      t67 = x1 ** 2
      t68 = t66 * t67
      t70 = t22 * x4
      t71 = t70 * t50
      t75 = t64 * t3
      t77 = t66 * t7
      t78 = t67 * x1
      t81 = t50 ** 2
      t82 = t22 * t81
      t87 = t55 * t5 * t9
      t91 = t35 * t36
      t92 = t3 * t7
      t93 = t92 * t58
      t95 = 0.4D1 * t91 * t93
      t96 = t36 * z
      t100 = x4 * x1
      t102 = t8 * t21 * t100 * t12
      t105 = t96 * t35
      t106 = t12 ** 2
      t107 = t67 * t106
      t108 = t56 * t107
      t111 = t56 * t24
      t114 = t91 * t111
      t116 = t92 * t59
      t118 = 0.4D1 * t91 * t116
      t119 = t91 * t108
      t121 = t19 * t2
      t123 = t68 * t82
      t129 = -0.4D1 * t6 * t9 * x1 * t20 * t24 - 0.7D1 * t53 - 0.3D1 * t
     #62 + 0.4D1 * t65 * t68 * t20 * t71 + 0.2D1 * t2 * t75 * t77 * t78 
     #* t20 * t82 + 0.2D1 * t87 * t58 * t24 - t95 - 0.7D1 * t96 * t35 * 
     #t4 * t102 - 0.2D1 * t105 * t108 - 0.2D1 * t105 * t111 + 0.12D2 * t
     #114 - t118 + 0.12D2 * t119 - 0.2D1 * t121 * t64 * t123 + 0.4D1 * t
     #121 * t4 * t52
      t133 = t121 * z
      t139 = t133 * t116
      t141 = t133 * t111
      t143 = t133 * t93
      t145 = t55 * z
      t146 = t145 * t93
      t148 = t145 * t108
      t151 = z ** 2
      t152 = t2 * s * t151
      t156 = t145 * t116
      t167 = -0.2D1 * t55 * t64 * t123 - 0.2D1 * t133 * t108 + 0.4D1 * t
     #55 * t4 * t52 - 0.2D1 * t139 + 0.3D1 * t141 + 0.5D1 * t143 - 0.2D1
     # * t146 + 0.3D1 * t148 + 0.4D1 * t152 * t4 * t52 + 0.5D1 * t156 - 
     #0.2D1 * t145 * t111 - 0.2D1 * t152 * t64 * t123 + 0.12D2 * t91 + 0
     #.3D1 * t133 + 0.3D1 * t145 - 0.2D1 * t152
      t171 = z * t5
      t175 = t9 * t22 * t100 * t50
      t178 = z * t4
      t179 = t121 * t178
      t180 = t179 * t52
      t183 = t35 * t36 * t64
      t184 = t183 * t123
      t186 = t121 * t171
      t187 = t186 * t175
      t190 = t96 * t35 * t5
      t192 = t9 * t67
      t195 = t192 * t12 * t21 * t50
      t201 = t55 * t5 * t9 * t78 * t106 * t12
      t206 = t2 * t3 * t7 * t58 * t19
      t209 = t35 * t36 * t5
      t211 = 0.4D1 * t209 * t175
      t212 = t64 * t66
      t213 = t55 * t212
      t216 = t213 * t78 * t106 * t51
      t218 = t67 * t12
      t227 = t22 * t21
      t233 = x1 * t50
      t237 = -0.2D1 * t121 - 0.2D1 * t55 + 0.4D1 * t55 * t171 * t175 - 0
     #.3D1 * t180 + 0.12D2 * t184 + 0.5D1 * t187 + t190 * t175 + t190 * 
     #t195 + 0.2D1 * t201 + 0.2D1 * t206 - t211 - 0.2D1 * t216 + 0.4D1 *
     # t213 * t218 * t71 - 0.4D1 * t87 * t107 * t59 + 0.2D1 * t55 * t75 
     #* t77 * t227 * x4 * t67 * t81 - 0.4D1 * t87 * t70 * t233
      t238 = t179 * t102
      t241 = 0.4D1 * t209 * t195
      t246 = t121 * t5 * t9 * t227 * t23 * x4
      t249 = t55 * t3 * t29
      t254 = t87 * t218 * z * t21 * t50
      t258 = t227 * t23 * t233
      t259 = t121 * t212 * t258
      t263 = z * t64
      t274 = t55 * t178 * t52
      t276 = t38 * t102
      t278 = t6 * t192
      t284 = t106 * t19
      t293 = -0.3D1 * t238 - t241 + 0.2D1 * t246 + 0.2D1 * t249 + 0.5D1 
     #* t254 - 0.2D1 * t259 - 0.2D1 * t213 * t258 - 0.2D1 * t121 * t263 
     #* t123 - 0.2D1 * t55 * t263 * t123 - t183 * t68 * t82 * z - 0.3D1 
     #* t274 - 0.7D1 * t276 - 0.4D1 * t278 * t20 * t51 - 0.2D1 * t65 * t
     #66 * t78 * t284 * t51 + 0.2D1 * t278 * t284 * t59 + 0.4D1 * t186 *
     # t195
      t309 = 0.12D2 * t53 - t62 - t95 - 0.2D1 * t114 - t118 - 0.2D1 * t1
     #19 + 0.4D1 * t139 - 0.6D1 * t141 - t143 + 0.4D1 * t146 - 0.6D1 * t
     #148 - t156 - 0.2D1 * t91 - 0.6D1 * t133 - 0.6D1 * t145 + 0.4D1 * t
     #152
      t320 = 0.4D1 * t121 + 0.4D1 * t55 - t180 - 0.2D1 * t184 - t187 - 0
     #.4D1 * t201 - 0.4D1 * t206 - t211 + 0.4D1 * t216 - t238 - t241 - 0
     #.4D1 * t246 - 0.4D1 * t249 - t254 + 0.4D1 * t259 - t274 + 0.12D2 *
     # t276
      rrgg2ggh31J6 = 0.9D1 * (0.5D1 * wd * (t129 + t167 + t237 + t293) +
     # 0.4D1 * wd * (t309 + t320)) / t1 / t19 / t34 / z / 0.314159265358
     #9793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh31J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t11 ** 2
      t13 = t10 * t12
      t14 = t13 * z
      t16 = 0.1D1 - x1
      t17 = t16 * t3
      t18 = t17 * x4
      t20 = 0.1D1 - x4
      t23 = s - t2 * t18 - t2 * t17 * t20
      t24 = t23 * t12
      t25 = t24 * z
      t27 = t10 * t23
      t28 = t11 * s
      t29 = t27 * t28
      t31 = t1 ** 2
      t32 = t31 ** 2
      t33 = t3 ** 2
      t34 = t33 ** 2
      t35 = t32 * t34
      t37 = t16 ** 2
      t38 = t37 * t16
      t39 = x4 ** 2
      t44 = cos(x2 * 0.3141592653589793D1)
      t48 = Sqrt(x3 * t7 * x4 * t20)
      t51 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t44 * t48
      t56 = z * t31
      t57 = t13 * t56
      t59 = x4 * x1
      t61 = t33 * t16 * t59 * t7
      t63 = t31 * t1
      t66 = t33 * t3
      t69 = t66 * t37 * t59 * t51
      t72 = t16 * t51
      t73 = t33 * x1 * t72
      t76 = t27 * t28 * t63
      t80 = t27 * t28 * t31
      t84 = x1 ** 2
      t85 = t84 * x1
      t86 = t7 ** 2
      t101 = t7 * x1
      t111 = t31 * t33
      t113 = t16 * x4
      t118 = z ** 2
      t121 = -0.6D1 * t14 - 0.6D1 * t25 - 0.2D1 * t29 + 0.4D1 * t13 * t3
     #5 * t38 * t39 * x1 * t51 - t57 * t61 - t13 * z * t63 * t69 - t57 *
     # t73 - 0.4D1 * t76 * t69 + 0.12D2 * t80 * t73 + 0.4D1 * t24 * t35 
     #* t85 * t86 * t72 - 0.4D1 * t76 * t66 * t84 * t7 * t16 * t51 - t24
     # * t56 * t73 - 0.4D1 * t12 * t1 * t3 * t101 * t10 - 0.4D1 * t24 * 
     #t63 * t66 * t85 * t86 * t7 - t24 * t111 * t101 * t113 * z + 0.4D1 
     #* t12 * s * t118
      t124 = t1 * t3
      t125 = t124 * t101
      t129 = t111 * t84 * t86
      t132 = t124 * t113
      t136 = t111 * t37 * t39
      t163 = t51 ** 2
      t175 = 0.4D1 * t13 + 0.4D1 * t24 - 0.4D1 * t29 * t125 - 0.2D1 * t2
     #9 * t129 - 0.4D1 * t29 * t132 - 0.2D1 * t29 * t136 - 0.4D1 * t13 *
     # t63 * t66 * t38 * t39 * x4 - 0.4D1 * t24 * t1 * t18 - 0.6D1 * t25
     # * t129 + 0.4D1 * t14 * t132 - 0.6D1 * t14 * t136 - t14 * t125 - t
     #25 * t132 + 0.4D1 * t25 * t125 + 0.12D2 * t80 * t61 - 0.2D1 * t27 
     #* t28 * t32 * t34 * t84 * t37 * t163 - t24 * t63 * t66 * t84 * t7 
     #* z * t16 * t51
      rrgg2ggh31J7 = 0.45D2 * wd * (t121 + t175) / t11 / t10 / t23 / z /
     # 0.3141592653589793D1

      end function
  
 