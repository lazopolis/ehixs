  
      subroutine rrqqbar2gght3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh31J1  
      doubleprecision rrqqbar2ggh31J2  
      doubleprecision rrqqbar2ggh31J3  
      doubleprecision rrqqbar2gght3s1e1  
      doubleprecision rrqqbar2gght3s1e0  
      doubleprecision rrqqbar2gght3s1em1  
      doubleprecision rrqqbar2gght3s1em2  
      doubleprecision rrqqbar2gght3s1em3  
      doubleprecision rrqqbar2gght3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3
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
      t15 = 0.3141592653589793D1 * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = t11 ** 2
      t29 = t27 * t28
      t30 = x1 ** 2
      t31 = t6 ** 2
      t32 = t30 * t31
      t33 = t9 ** 2
      t35 = t32 * x4 * t33
      t38 = log(0.4D1 * t29 * t35)
      t39 = t38 * t9
      t40 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t41 = t18 * t40
      t43 = t38 ** 2
      t45 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t46 = t18 * t45
      t53 = 0.3141592653589793D1 * lh
      t54 = t3 * t16
      t55 = t19 * t40
      t61 = lh ** 2
      t63 = 0.3141592653589793D1 ** 2
      t65 = 0.180D3 * t61 - 0.30D2 * t63
      t66 = 0.3141592653589793D1 * t65
      t67 = t66 * t3
      t69 = t16 * t9 * t46
      t70 = t67 * t69
      t72 = 0.1D1 / x4
      t78 = log(0.4D1 * t29 * t32 * t33)
      t79 = t78 * 0.3141592653589793D1
      t87 = t78 ** 2
      t88 = t87 * 0.3141592653589793D1
      t111 = x3 * t24
      t112 = t26 * t28
      t113 = t111 * t112
      t116 = log(0.4D1 * t113 * t35)
      t123 = t53 * t3
      t127 = 0.1D1 / x3
      t131 = t28 * t30
      t136 = log(0.4D1 * t111 * t26 * t131 * t31 * t33)
      t137 = t136 * t9
      t139 = t136 ** 2
      t155 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41 - t43 * t9 * t46 /
     # 0.2D1) - 0.180D3 * t53 * t54 * (-t55 + t39 * t46) - t70) * t72 / 
     #0.720D3 + (-0.180D3 * t53 - 0.90D2 * t79) * t3 * t16 * t21 / 0.720
     #D3 + (t66 + 0.180D3 * t79 * lh + 0.45D2 * t88) * t3 * t16 * t55 / 
     #0.720D3 + (0.3141592653589793D1 * (0.60D2 * lh * t63 - 0.288493656
     #7583026D3 - 0.120D3 * t61 * lh) - t79 * t65 - 0.90D2 * t88 * lh - 
     #0.15D2 * t87 * t78 * 0.3141592653589793D1) * t3 * t16 * t19 * t45 
     #/ 0.720D3 + (0.90D2 * t15 * t16 * (t55 - t116 * t9 * t46) - 0.180D
     #3 * t123 * t69) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t
     #21 + t137 * t41 - t139 * t9 * t46 / 0.2D1) - 0.180D3 * t53 * t54 *
     # (-t55 + t137 * t46) - t70) * t127 / 0.720D3
      t156 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t155)
      t158 = 0.1D1 - x4
      t159 = KAPPA2(x1, x2, 0.0D0, t158, z)
      t160 = s * t159
      t161 = t160 * t4
      t162 = -t158
      t163 = t7 * t162
      t164 = t160 * t163
      t165 = t7 * x4
      t166 = t160 * t165
      t167 = t159 ** 2
      t170 = x1 * t6
      t172 = s * t167 * t11 * t170 * t162
      t174 = 0.1D1 / (-0.2D1 + t159)
      t175 = t167 * t174
      t176 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t158)
      t180 = t167 ** 2
      t185 = log(-0.4D1 * t27 * t131 * t31 * x4 * t162 * t180)
      t186 = t185 * t167
      t187 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t158)
      t190 = t185 ** 2
      t192 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t158)
      t193 = t174 * t192
      t200 = t175 * t187
      t207 = t16 * t167 * t193
      t211 = x4 * t162
      t216 = log(-0.4D1 * t113 * t32 * t211 * t180)
      t229 = -(0.90D2 * t15 * t16 * (t175 * t176 - t186 * t174 * t187 + 
     #t190 * t167 * t193 / 0.2D1) - 0.180D3 * t53 * t54 * (t200 - t186 *
     # t193) + t67 * t207) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (-t20
     #0 + t216 * t167 * t193) + 0.180D3 * t123 * t207) * t127 * t72 / 0.
     #720D3
      t230 = FJET(XB1, XB2, s, 0.0D0, t161, t164, -t166, t172, t229)
      t232 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t155)
      t234 = FJET(XB1, XB2, s, t161, 0.0D0, -t166, t164, t172, t229)
      t236 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t237 = s * t236
      t238 = t4 * x3
      t239 = t237 * t238
      t240 = -0.1D1 + x3
      t241 = t4 * t240
      t242 = t237 * t241
      t243 = t237 * t7
      t244 = t236 ** 2
      t248 = s * t244 * t11 * t170 * t240
      t250 = 0.1D1 / (-0.2D1 + t236)
      t251 = t244 * t250
      t252 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t253 = t251 * t252
      t255 = t244 ** 2
      t260 = log(-0.4D1 * t113 * t32 * t240 * x4 * t255)
      t262 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t263 = t250 * t262
      t270 = t16 * t244 * t263
      t276 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t282 = log(-0.4D1 * t113 * t32 * t240 * t255)
      t283 = t282 * t244
      t286 = t282 ** 2
      t303 = (0.90D2 * t15 * t16 * (-t253 + t260 * t244 * t263) + 0.180D
     #3 * t123 * t270) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t
     #251 * t276 - t283 * t250 * t252 + t286 * t244 * t263 / 0.2D1) - 0.
     #180D3 * t53 * t54 * (t253 - t283 * t263) + t67 * t270) * t127 / 0.
     #720D3
      t304 = FJET(XB1, XB2, s, t239, -t242, -t243, 0.0D0, t248, t303)
      t306 = KAPPA2(x1, x2, x3, t158, z)
      t307 = s * t306
      t308 = t307 * t238
      t309 = t307 * t241
      t310 = t307 * t163
      t311 = t307 * t165
      t312 = t306 ** 2
      t317 = cos(t22)
      t320 = Sqrt(x3 * t240 * t211)
      t325 = s * t312 * t11 * t170 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t317 * t320)
      t327 = 0.1D1 / (-0.2D1 + t306)
      t329 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t15
     #8)
      t334 = t312 ** 2
      t339 = log(0.4D1 * t111 * t112 * t30 * t31 * t240 * t211 * t334)
      t341 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t15
     #8)
      t342 = t327 * t341
      t352 = 0.90D2 * t15 * t16 * (t312 * t327 * t329 - t339 * t312 * t3
     #42) - 0.180D3 * t123 * t16 * t312 * t342
      t355 = t352 * t127 * t72 / 0.720D3
      t356 = FJET(XB1, XB2, s, t308, -t309, t310, -t311, t325, t355)
      t358 = t127 * t72
      t361 = FJET(XB1, XB2, s, -t242, t239, 0.0D0, -t243, t248, t303)
      t363 = FJET(XB1, XB2, s, -t309, t308, -t311, t310, t325, t355)
      rrqqbar2gght3s1e1 = t156 * t155 + t230 * t229 + t232 * t155 + t234
     # * t229 + t304 * t303 + t356 * t352 * t358 / 0.720D3 + t361 * t303
     # + t363 * t352 * t358 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3
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
      t15 = 0.3141592653589793D1 * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = t11 ** 2
      t29 = t27 * t28
      t30 = x1 ** 2
      t31 = t6 ** 2
      t32 = t30 * t31
      t33 = t9 ** 2
      t38 = log(0.4D1 * t29 * t32 * x4 * t33)
      t40 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t41 = t18 * t40
      t47 = 0.3141592653589793D1 * lh
      t48 = t47 * t3
      t49 = t16 * t9
      t52 = 0.180D3 * t48 * t49 * t41
      t54 = 0.1D1 / x4
      t58 = 0.1D1 / x3
      t59 = t58 * t54
      t63 = x3 * t24
      t65 = t28 * t30
      t70 = log(0.4D1 * t63 * t26 * t65 * t31 * t33)
      t81 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t89 = log(0.4D1 * t29 * t32 * t33)
      t90 = t89 * 0.3141592653589793D1
      t97 = lh ** 2
      t99 = 0.3141592653589793D1 ** 2
      t105 = t89 ** 2
      t114 = -(0.90D2 * t15 * t16 * (-t21 + t38 * t9 * t41) + t52) * t54
     # / 0.720D3 + t15 * t49 * t41 * t59 / 0.8D1 - (0.90D2 * t15 * t16 *
     # (-t21 + t70 * t9 * t41) + t52) * t58 / 0.720D3 + t15 * t16 * t19 
     #* t81 / 0.8D1 + (-0.180D3 * t47 - 0.90D2 * t90) * t3 * t16 * t21 /
     # 0.720D3 + (0.3141592653589793D1 * (0.180D3 * t97 - 0.30D2 * t99) 
     #+ 0.180D3 * t90 * lh + 0.45D2 * t105 * 0.3141592653589793D1) * t3 
     #* t16 * t19 * t40 / 0.720D3
      t115 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t114)
      t117 = 0.1D1 - x4
      t118 = KAPPA2(x1, x2, 0.0D0, t117, z)
      t119 = s * t118
      t120 = t119 * t4
      t121 = -t117
      t122 = t7 * t121
      t123 = t119 * t122
      t124 = t7 * x4
      t125 = t119 * t124
      t126 = t118 ** 2
      t129 = x1 * t6
      t131 = s * t126 * t11 * t129 * t121
      t133 = 0.1D1 / (-0.2D1 + t118)
      t135 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t117)
      t139 = t126 ** 2
      t144 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t139)
      t146 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #t117)
      t147 = t133 * t146
      t153 = t16 * t126
      t164 = -(0.90D2 * t15 * t16 * (t126 * t133 * t135 - t144 * t126 * 
     #t147) - 0.180D3 * t48 * t153 * t147) * t54 / 0.720D3 - t15 * t153 
     #* t147 * t59 / 0.8D1
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t114)
      t169 = FJET(XB1, XB2, s, t120, 0.0D0, -t125, t123, t131, t164)
      t171 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t172 = s * t171
      t173 = t4 * x3
      t174 = t172 * t173
      t175 = -0.1D1 + x3
      t176 = t4 * t175
      t177 = t172 * t176
      t178 = t172 * t7
      t179 = t171 ** 2
      t183 = s * t179 * t11 * t129 * t175
      t184 = t16 * t179
      t187 = 0.1D1 / (-0.2D1 + t171)
      t188 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t189 = t187 * t188
      t194 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t198 = t179 ** 2
      t203 = log(-0.4D1 * t63 * t26 * t28 * t32 * t175 * t198)
      t216 = -t15 * t184 * t189 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (t
     #179 * t187 * t194 - t203 * t179 * t189) - 0.180D3 * t48 * t184 * t
     #189) * t58 / 0.720D3
      t217 = FJET(XB1, XB2, s, t174, -t177, -t178, 0.0D0, t183, t216)
      t219 = KAPPA2(x1, x2, x3, t117, z)
      t220 = s * t219
      t221 = t220 * t173
      t222 = t220 * t176
      t223 = t220 * t122
      t224 = t220 * t124
      t225 = t219 ** 2
      t230 = cos(t22)
      t234 = Sqrt(x3 * t175 * x4 * t121)
      t239 = s * t225 * t11 * t129 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t230 * t234)
      t243 = 0.1D1 / (-0.2D1 + t219)
      t244 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t11
     #7)
      t248 = t15 * t16 * t225 * t243 * t244 * t59 / 0.8D1
      t249 = FJET(XB1, XB2, s, t221, -t222, t223, -t224, t239, t248)
      t251 = t3 * t16
      t256 = t225 * t243 * t244 * t58 * t54
      t259 = FJET(XB1, XB2, s, -t177, t174, 0.0D0, -t178, t183, t216)
      t261 = FJET(XB1, XB2, s, -t222, t221, -t224, t223, t239, t248)
      rrqqbar2gght3s1e0 = t115 * t114 + t165 * t164 + t167 * t114 + t169
     # * t164 + t217 * t216 + t249 * 0.3141592653589793D1 * t251 * t256 
     #/ 0.8D1 + t259 * t216 + t261 * 0.3141592653589793D1 * t251 * t256 
     #/ 0.8D1

      end function



      doubleprecision function rrqqbar2gght3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3
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
      t17 = 0.3141592653589793D1 * t3 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t22 = 0.1D1 / x3
      t27 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t34 = sin(x2 * 0.3141592653589793D1)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t44 = t9 ** 2
      t48 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t44)
      t57 = 0.1D1 / x4
      t62 = t17 * t20 * t21 * t22 / 0.8D1 + t17 * t20 * t27 / 0.8D1 + (-
     #0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t48 * 0.31415926535
     #89793D1) * t3 * t16 * t20 * t21 / 0.720D3 + t17 * t20 * t21 * t57 
     #/ 0.8D1
      t63 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t62)
      t65 = 0.1D1 - x4
      t66 = KAPPA2(x1, x2, 0.0D0, t65, z)
      t67 = s * t66
      t68 = t67 * t4
      t69 = -t65
      t71 = t67 * t7 * t69
      t73 = t67 * t7 * x4
      t74 = t66 ** 2
      t77 = x1 * t6
      t79 = s * t74 * t11 * t77 * t69
      t83 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t
     #65)
      t85 = t74 / (-0.2D1 + t66) * t83 * t57
      t87 = t17 * t85 / 0.8D1
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t87)
      t90 = t3 * t16
      t94 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t62)
      t96 = FJET(XB1, XB2, s, t68, 0.0D0, -t73, t71, t79, -t87)
      t101 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t102 = s * t101
      t104 = t102 * t4 * x3
      t105 = -0.1D1 + x3
      t107 = t102 * t4 * t105
      t108 = t102 * t7
      t109 = t101 ** 2
      t113 = s * t109 * t11 * t77 * t105
      t117 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.1
     #0D1)
      t119 = t109 / (-0.2D1 + t101) * t117 * t22
      t121 = t17 * t119 / 0.8D1
      t122 = FJET(XB1, XB2, s, t104, -t107, -t108, 0.0D0, t113, -t121)
      t127 = FJET(XB1, XB2, s, -t107, t104, 0.0D0, -t108, t113, -t121)
      rrqqbar2gght3s1em1 = t63 * t62 - t88 * 0.3141592653589793D1 * t90 
     #* t85 / 0.8D1 + t94 * t62 - t96 * 0.3141592653589793D1 * t90 * t85
     # / 0.8D1 - t122 * 0.3141592653589793D1 * t90 * t119 / 0.8D1 - t127
     # * 0.3141592653589793D1 * t90 * t119 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3
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
      t21 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.10D1)
      t24 = 0.3141592653589793D1 * t3 * t16 * t9 * t19 * t21 / 0.8D1
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t24)
      t30 = t16 * t9 * t19 * t21
      t32 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t24)
      rrqqbar2gght3s1em2 = t25 * 0.3141592653589793D1 * t3 * t30 / 0.8D1
     # + t32 * 0.3141592653589793D1 * t3 * t30 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3
      rrqqbar2gght3s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3
      rrqqbar2gght3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh31J1
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
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t9 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t24 = s * t2
      t25 = t5 * x1
      t30 = s - t24 * t25 * x3 - t24 * t25 * t14
      t31 = t5 * t9
      t36 = s - t24 * t31 * x4 - t24 * t31 * t16
      rrqqbar2ggh31J1 = -0.16D2 / 0.27D2 * wd * (-0.8D1 * t1 * t3 * t6 *
     # x1 * t9 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19) * t30 *
     # t36 + 0.8D1 * t30 * t1 * t3 * t6 * t9 * x4 * t36 * x1 * t14) / s 
     #/ t30 / t36 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2ggh31J2
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
      t15 = 0.1D1 - x4
      t18 = s - t2 * t12 * x4 - t2 * t12 * t15
      t19 = t10 * t18
      t20 = s ** 2
      t21 = t19 * t20
      t22 = t1 ** 2
      t23 = t3 ** 2
      t24 = t22 * t23
      t25 = x1 ** 2
      t26 = t7 ** 2
      t27 = t25 * t26
      t31 = t11 ** 2
      t32 = x4 ** 2
      t33 = t31 * t32
      t37 = t20 * s
      t38 = t18 * t37
      t39 = t22 ** 2
      t40 = t23 ** 2
      t41 = t39 * t40
      t47 = cos(x2 * 0.3141592653589793D1)
      t51 = Sqrt(x3 * t7 * x4 * t15)
      t54 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t47 * t51
      t57 = t25 * t31 * t54 * t7 * x4
      t60 = t10 * t37
      t66 = t22 * t1 * t23 * t3
      t67 = t38 * t66
      t78 = t40 * t25
      t79 = t54 ** 2
      t80 = t31 * t79
      t81 = t78 * t80
      t86 = t39 * t1 * t40 * t3
      t111 = t60 * t66
      t128 = 0.4D1 * t21 * t24 * t27 + 0.4D1 * t21 * t24 * t33 + 0.8D1 *
     # t38 * t41 * t57 + 0.8D1 * t60 * t41 * t57 - 0.4D1 * t67 * x1 * t7
     # * t33 - 0.4D1 * t38 * t22 * t23 * t31 * t32 - 0.4D1 * t60 * t39 *
     # t81 - 0.4D1 * t38 * t86 * t25 * x1 * t7 * t80 - 0.4D1 * t38 * t39
     # * t81 - 0.8D1 * t20 * t39 * t78 * t80 * t19 - 0.4D1 * t60 * t22 *
     # t23 * t25 * t26 + 0.8D1 * t67 * t31 * x4 * x1 * t54 + 0.8D1 * t11
     #1 * t25 * t7 * t11 * t54 - 0.4D1 * t111 * t27 * t11 * x4 - 0.4D1 *
     # t60 * t86 * t31 * t11 * x4 * t25 * t79
      rrqqbar2ggh31J2 = -0.16D2 / 0.27D2 * wd * t128 / s / t10 / t18 / z
     # / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2ggh31J3
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
      t13 = t12 * s
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 * t1
      t17 = t3 ** 2
      t18 = t17 * t3
      t19 = t16 * t18
      t20 = t14 * t19
      t21 = t4 ** 2
      t26 = cos(x2 * 0.3141592653589793D1)
      t27 = 0.1D1 - x3
      t31 = Sqrt(x3 * t27 * x4 * t8)
      t34 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t26 * t31
      t35 = x1 * t34
      t36 = t21 * x4 * t35
      t39 = t3 * x1
      t42 = t39 * t27
      t44 = s - t2 * t39 * x3 - t2 * t42
      t45 = t44 * t13
      t46 = t45 * t19
      t47 = x1 ** 2
      t49 = t4 * t34
      t50 = t47 * t27 * t49
      t53 = t15 ** 2
      t55 = t17 ** 2
      t57 = t53 * t1 * t55 * t3
      t59 = t21 * t4
      t61 = t34 ** 2
      t68 = t44 * t11
      t70 = t68 * t12 * t16
      t85 = x1 * t27
      t86 = x4 ** 2
      t87 = t21 * t86
      t91 = t15 * t17
      t93 = t4 * x4
      t94 = t93 * t85
      t97 = t53 * t55
      t98 = t14 * t97
      t102 = t47 * t21 * t34 * t27 * x4
      t105 = t45 * t97
      t108 = 0.4D1 * t20 * t36 + 0.4D1 * t46 * t50 - 0.8D1 * t45 * t57 *
     # t59 * x4 * t47 * t61 + 0.7D1 * t45 + 0.7D1 * t14 + 0.2D1 * t70 * 
     #t18 * t21 * x4 * x1 * t34 + 0.12D2 * t20 * t50 + 0.2D1 * t70 * t18
     # * t47 * t27 * t4 * t34 - 0.2D1 * t46 * t85 * t87 - 0.4D1 * t45 * 
     #t91 * t94 + 0.4D1 * t98 * t102 + 0.4D1 * t105 * t102
      t110 = t47 * x1
      t112 = t21 * t61
      t117 = t55 * t47
      t125 = t27 ** 2
      t126 = t47 * t125
      t133 = t44 * t12
      t151 = t17 * x1
      t156 = t45 * z
      t158 = t133 * t11
      t160 = -0.8D1 * t14 * t57 * t110 * t27 * t112 - 0.8D1 * t12 * t53 
     #* t117 * t112 * t68 - 0.4D1 * t14 * t91 * t94 - 0.2D1 * t20 * t126
     # * t93 - 0.48D2 * t68 * t12 * z + 0.4D1 * t133 * t91 * t93 * t11 *
     # x1 * t27 + 0.6D1 * t98 * t110 * t125 * t49 + 0.6D1 * t105 * t59 *
     # t86 * t35 + 0.12D2 * t46 * t36 + 0.12D2 * t12 * t15 * t151 * t49 
     #* t68 - 0.24D2 * t156 + 0.42D2 * t158
      t162 = z ** 2
      t165 = t14 * z
      t169 = t12 ** 2
      t176 = t117 * t112
      t184 = t1 * t3
      t185 = t184 * t85
      t188 = t184 * t93
      t195 = 0.24D2 * t45 * t162 - 0.24D2 * t165 + 0.24D2 * t14 * t162 -
     # 0.8D1 * t169 * t162 * z - t158 * t91 * t87 - 0.8D1 * t45 * t53 * 
     #t176 - 0.8D1 * t14 * t53 * t176 - t158 * t91 * t126 + 0.24D2 * t15
     #6 * t185 + 0.24D2 * t165 * t188 - 0.26D2 * t158 * t185 - 0.26D2 * 
     #t158 * t188
      t196 = t14 * t15
      t197 = t151 * t49
      t200 = t45 * t15
      t214 = t17 * t21 * t86
      t217 = t45 * t1
      t222 = t14 * t1
      t226 = t17 * t47 * t125
      t235 = 0.6D1 * t196 * t197 + 0.6D1 * t200 * t197 - t45 * t16 * t18
     # * t59 * t86 * x4 - t14 * t16 * t18 * t110 * t125 * t27 - 0.3D1 * 
     #t200 * t214 - 0.18D2 * t217 * t42 - 0.3D1 * t217 * t6 - 0.18D2 * t
     #222 * t6 - 0.3D1 * t196 * t226 - 0.3D1 * t222 * t42 + 0.8D1 * t196
     # * t214 + 0.8D1 * t200 * t226
      rrqqbar2ggh31J3 = -0.16D2 / 0.27D2 * wd * (t108 + t160 + t195 + t2
     #35) / s / t44 / t11 / z / 0.3141592653589793D1

      end function
  
 