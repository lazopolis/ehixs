  
      subroutine rrqqbar2qqbarht3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2qqbarh31J1  
      doubleprecision rrqqbar2qqbarh31J2  
      doubleprecision rrqqbar2qqbarh31J3  
      doubleprecision rrqqbar2qqbarht3s1e1  
      doubleprecision rrqqbar2qqbarht3s1e0  
      doubleprecision rrqqbar2qqbarht3s1em1  
      doubleprecision rrqqbar2qqbarht3s1em2  
      doubleprecision rrqqbar2qqbarht3s1em3  
      doubleprecision rrqqbar2qqbarht3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2qqbarht3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh31J1
      doubleprecision rrqqbar2qqbarh31J2
      doubleprecision rrqqbar2qqbarh31J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.3141592653589793D1 * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrqqbar2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
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
      t40 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
      t41 = t18 * t40
      t43 = t38 ** 2
      t45 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
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
      t156 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t155)
      t158 = 0.1D1 - x4
      t159 = KAPPA2(x1, x2, 0.0D0, t158, z)
      t160 = s * t159
      t162 = t7 * x4
      t164 = -t158
      t165 = t7 * t164
      t167 = t159 ** 2
      t170 = x1 * t6
      t174 = 0.1D1 / (-0.2D1 + t159)
      t175 = t167 * t174
      t176 = rrqqbar2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, t158)
      t180 = t167 ** 2
      t185 = log(-0.4D1 * t27 * t131 * t31 * x4 * t164 * t180)
      t186 = t185 * t167
      t187 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, t158)
      t190 = t185 ** 2
      t192 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, t158)
      t193 = t174 * t192
      t200 = t175 * t187
      t207 = t16 * t167 * t193
      t211 = x4 * t164
      t216 = log(-0.4D1 * t113 * t32 * t211 * t180)
      t229 = -(0.90D2 * t15 * t16 * (t175 * t176 - t186 * t174 * t187 + 
     #t190 * t167 * t193 / 0.2D1) - 0.180D3 * t53 * t54 * (t200 - t186 *
     # t193) + t67 * t207) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (-t20
     #0 + t216 * t167 * t193) + 0.180D3 * t123 * t207) * t127 * t72 / 0.
     #720D3
      t230 = FJET(XB1, XB2, s, t160 * t4, 0.0D0, -t160 * t162, t160 * t1
     #65, s * t167 * t11 * t170 * t164, t229)
      t232 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t233 = s * t232
      t234 = -0.1D1 + x3
      t235 = t4 * t234
      t237 = t4 * x3
      t240 = t232 ** 2
      t246 = 0.1D1 / (-0.2D1 + t232)
      t247 = t240 * t246
      t248 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #0.10D1)
      t249 = t247 * t248
      t251 = t240 ** 2
      t256 = log(-0.4D1 * t113 * t32 * t234 * x4 * t251)
      t258 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #0.10D1)
      t259 = t246 * t258
      t266 = t16 * t240 * t259
      t272 = rrqqbar2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #0.10D1)
      t278 = log(-0.4D1 * t113 * t32 * t234 * t251)
      t279 = t278 * t240
      t282 = t278 ** 2
      t299 = (0.90D2 * t15 * t16 * (-t249 + t256 * t240 * t259) + 0.180D
     #3 * t123 * t266) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t
     #247 * t272 - t279 * t246 * t248 + t282 * t240 * t259 / 0.2D1) - 0.
     #180D3 * t53 * t54 * (t249 - t279 * t259) + t67 * t266) * t127 / 0.
     #720D3
      t300 = FJET(XB1, XB2, s, -t233 * t235, t233 * t237, 0.0D0, -t233 *
     # t7, s * t240 * t11 * t170 * t234, t299)
      t302 = KAPPA2(x1, x2, x3, t158, z)
      t303 = s * t302
      t308 = t302 ** 2
      t313 = cos(t22)
      t316 = Sqrt(x3 * t234 * t211)
      t323 = 0.1D1 / (-0.2D1 + t302)
      t325 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #t158)
      t330 = t308 ** 2
      t335 = log(0.4D1 * t111 * t112 * t30 * t31 * t234 * t211 * t330)
      t337 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #t158)
      t338 = t323 * t337
      t348 = 0.90D2 * t15 * t16 * (t308 * t323 * t325 - t335 * t308 * t3
     #38) - 0.180D3 * t123 * t16 * t308 * t338
      t352 = FJET(XB1, XB2, s, -t303 * t235, t303 * t237, -t303 * t162, 
     #t303 * t165, s * t308 * t11 * t170 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t313 * t316), t348 * t127 * t72 / 0.720D3)
      rrqqbar2qqbarht3s1e1 = t155 * t156 + t230 * t229 + t300 * t299 + t
     #352 * t348 * t127 * t72 / 0.720D3

      end function



      doubleprecision function rrqqbar2qqbarht3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh31J1
      doubleprecision rrqqbar2qqbarh31J2
      doubleprecision rrqqbar2qqbarh31J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.3141592653589793D1 * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
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
      t40 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
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
      t81 = rrqqbar2qqbarh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
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
      t115 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t114)
      t117 = 0.1D1 - x4
      t118 = KAPPA2(x1, x2, 0.0D0, t117, z)
      t119 = s * t118
      t121 = t7 * x4
      t123 = -t117
      t124 = t7 * t123
      t126 = t118 ** 2
      t129 = x1 * t6
      t133 = 0.1D1 / (-0.2D1 + t118)
      t135 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, t117)
      t139 = t126 ** 2
      t144 = log(-0.4D1 * t27 * t65 * t31 * x4 * t123 * t139)
      t146 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, t117)
      t147 = t133 * t146
      t153 = t16 * t126
      t164 = -(0.90D2 * t15 * t16 * (t126 * t133 * t135 - t144 * t126 * 
     #t147) - 0.180D3 * t48 * t153 * t147) * t54 / 0.720D3 - t15 * t153 
     #* t147 * t59 / 0.8D1
      t165 = FJET(XB1, XB2, s, t119 * t4, 0.0D0, -t119 * t121, t119 * t1
     #24, s * t126 * t11 * t129 * t123, t164)
      t167 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t168 = s * t167
      t169 = -0.1D1 + x3
      t170 = t4 * t169
      t172 = t4 * x3
      t175 = t167 ** 2
      t180 = t16 * t175
      t183 = 0.1D1 / (-0.2D1 + t167)
      t184 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #0.10D1)
      t185 = t183 * t184
      t190 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #0.10D1)
      t194 = t175 ** 2
      t199 = log(-0.4D1 * t63 * t26 * t28 * t32 * t169 * t194)
      t212 = -t15 * t180 * t185 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (t
     #175 * t183 * t190 - t199 * t175 * t185) - 0.180D3 * t48 * t180 * t
     #185) * t58 / 0.720D3
      t213 = FJET(XB1, XB2, s, -t168 * t170, t168 * t172, 0.0D0, -t168 *
     # t7, s * t175 * t11 * t129 * t169, t212)
      t215 = KAPPA2(x1, x2, x3, t117, z)
      t216 = s * t215
      t221 = t215 ** 2
      t226 = cos(t22)
      t230 = Sqrt(x3 * t169 * x4 * t123)
      t239 = 0.1D1 / (-0.2D1 + t215)
      t240 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #t117)
      t245 = FJET(XB1, XB2, s, -t216 * t170, t216 * t172, -t216 * t121, 
     #t216 * t124, s * t221 * t11 * t129 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t226 * t230), t15 * t16 * t221 * t239 * t240 * t5
     #9 / 0.8D1)
      rrqqbar2qqbarht3s1e0 = t115 * t114 + t165 * t164 + t213 * t212 + t
     #245 * 0.3141592653589793D1 * t3 * t16 * t221 * t239 * t240 * t58 *
     # t54 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh31J1
      doubleprecision rrqqbar2qqbarh31J2
      doubleprecision rrqqbar2qqbarh31J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = 0.1D1 / s
      t17 = 0.3141592653589793D1 * t3 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
      t22 = 0.1D1 / x3
      t27 = rrqqbar2qqbarh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
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
      t63 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 *
     # t11 * x1 * t6, t62)
      t65 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t66 = s * t65
      t67 = -0.1D1 + x3
      t73 = t65 ** 2
      t76 = x1 * t6
      t82 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0
     #.10D1)
      t84 = t73 / (-0.2D1 + t65) * t82 * t22
      t87 = FJET(XB1, XB2, s, -t66 * t4 * t67, t66 * t4 * x3, 0.0D0, -t6
     #6 * t7, s * t73 * t11 * t76 * t67, -t17 * t84 / 0.8D1)
      t89 = t3 * t16
      t93 = 0.1D1 - x4
      t94 = KAPPA2(x1, x2, 0.0D0, t93, z)
      t95 = s * t94
      t99 = -t93
      t102 = t94 ** 2
      t110 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, t93)
      t112 = t102 / (-0.2D1 + t94) * t110 * t57
      t115 = FJET(XB1, XB2, s, t95 * t4, 0.0D0, -t95 * t7 * x4, t95 * t7
     # * t99, s * t102 * t11 * t76 * t99, -t17 * t112 / 0.8D1)
      rrqqbar2qqbarht3s1em1 = t63 * t62 - t87 * 0.3141592653589793D1 * t
     #89 * t84 / 0.8D1 - t115 * 0.3141592653589793D1 * t89 * t112 / 0.8D
     #1

      end function



      doubleprecision function rrqqbar2qqbarht3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh31J1
      doubleprecision rrqqbar2qqbarh31J2
      doubleprecision rrqqbar2qqbarh31J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = 0.1D1 / s
      t19 = 0.1D1 / (-0.2D1 + t1)
      t21 = rrqqbar2qqbarh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, 0.10D1)
      t25 = FJET(XB1, XB2, s, t2 * t3 * x1, 0.0D0, 0.0D0, -t2 * t3 * t6,
     # -s * t9 * t11 * x1 * t6, 0.3141592653589793D1 * t3 * t16 * t9 * t
     #19 * t21 / 0.8D1)
      rrqqbar2qqbarht3s1em2 = t25 * 0.3141592653589793D1 * t3 * t16 * t9
     # * t19 * t21 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh31J1
      doubleprecision rrqqbar2qqbarh31J2
      doubleprecision rrqqbar2qqbarh31J3
      rrqqbar2qqbarht3s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh31J1
      doubleprecision rrqqbar2qqbarh31J2
      doubleprecision rrqqbar2qqbarh31J3
      rrqqbar2qqbarht3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2qqbarh31J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t6 = t2 * t5
      t8 = (0.1D1 - z) ** 2
      t9 = t6 * t8
      t10 = 0.1D1 - x1
      t15 = cos(x2 * 0.3141592653589793D1)
      t16 = 0.1D1 - x3
      t21 = Sqrt(x3 * t16 * x4 * (0.1D1 - x4))
      t24 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t15 * t21
      t28 = x1 ** 2
      t30 = t16 ** 2
      t34 = t10 ** 2
      t36 = x4 ** 2
      t40 = t5 ** 2
      t42 = t8 ** 2
      t45 = t24 ** 2
      rrqqbar2qqbarh31J1 = -0.32D2 / 0.27D2 * wd * (-0.3D1 * t2 + 0.4D1 
     #* t9 * x1 * t10 * t24 - 0.4D1 * t6 * t8 * t28 * t30 - 0.4D1 * t6 *
     # t8 * t34 * t36 - 0.3D1 * t2 * t40 * t42 * t28 * t34 * t45 + 0.8D1
     # * t9 * t10 * x4 * x1 * t16) / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh31J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t11 = t2 * t5 * t4 * t9 * t8
      t12 = x1 ** 2
      t13 = 0.1D1 - x1
      t18 = cos(x2 * 0.3141592653589793D1)
      t19 = 0.1D1 - x3
      t24 = Sqrt(x3 * t19 * x4 * (0.1D1 - x4))
      t27 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t18 * t24
      t32 = t2 * t4
      t41 = t2 * t5
      t42 = t41 * t9
      t47 = t13 ** 2
      t54 = t19 ** 2
      t59 = x4 ** 2
      t63 = t5 ** 2
      t65 = t9 ** 2
      t68 = t27 ** 2
      rrqqbar2qqbarh31J2 = -0.32D2 / 0.27D2 * wd * (0.3D1 * t2 - 0.6D1 *
     # t11 * t12 * t13 * t27 * t19 - 0.6D1 * t32 * t8 * t13 * x4 - 0.6D1
     # * t32 * t8 * x1 * t19 + 0.6D1 * t42 * x1 * t13 * t27 - 0.6D1 * t1
     #1 * t47 * x4 * x1 * t27 + 0.4D1 * t41 * t9 * t12 * t54 + 0.4D1 * t
     #41 * t9 * t47 * t59 + 0.3D1 * t2 * t63 * t65 * t12 * t47 * t68 + 0
     #.8D1 * t42 * t13 * x4 * x1 * t19) / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh31J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t11 = x4 ** 2
      t17 = t2 * t4 * t3 * t7 * t6
      t18 = x1 ** 2
      t19 = 0.1D1 - x3
      t20 = t19 ** 2
      t22 = t8 * x4
      t26 = x1 * t19
      t32 = t2 * t3
      t40 = cos(x2 * 0.3141592653589793D1)
      t45 = Sqrt(x3 * t19 * x4 * (0.1D1 - x4))
      t48 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t40 * t45
      rrqqbar2qqbarh31J3 = -0.32D2 / 0.27D2 * wd * (t5 * t7 * t9 * t11 +
     # t17 * t18 * t20 * t22 + t17 * t9 * t11 * t26 + t5 * t7 * t18 * t2
     #0 - t32 * t6 * x1 * t19 - t17 * t9 * x4 * x1 * t48 - t17 * t18 * t
     #8 * t48 * t19 - t32 * t6 * t8 * x4 + 0.2D1 * t5 * t7 * t22 * t26) 
     #/ t1 / z / 0.3141592653589793D1

      end function
  
 