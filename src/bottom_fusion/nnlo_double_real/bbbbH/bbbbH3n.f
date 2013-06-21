  
      subroutine bbbbH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH31J1  
      doubleprecision bbbbH31J2  
      doubleprecision bbbbH31J3  
      doubleprecision bbbbH32J1  
      doubleprecision bbbbH32J2  
      doubleprecision bbbbH32J3  
      doubleprecision bbbbH3n1e1  
      doubleprecision bbbbH3n1e0  
      doubleprecision bbbbH3n1em1  
      doubleprecision bbbbH3n1em2  
      doubleprecision bbbbH3n1em3  
      doubleprecision bbbbH3n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH3n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH3n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH3n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH3n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH3n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH3n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH3n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH31J1
      doubleprecision bbbbH31J2
      doubleprecision bbbbH31J3
      doubleprecision bbbbH32J1
      doubleprecision bbbbH32J2
      doubleprecision bbbbH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = x1 ** 2
      t14 = t4 ** 2
      t15 = t13 * t14
      t18 = log(0.4D1 * t12 * t15)
      t21 = t1 ** 2
      t22 = (-0.180D3 * lh - 0.90D2 * t18) * t21
      t23 = 0.1D1 / s
      t24 = bbbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t33 = t18 ** 2
      t36 = (t28 - t30 + 0.180D3 * t18 * lh + 0.45D2 * t33) * t21
      t37 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t44 = t28 - t30
      t51 = (0.60D2 * lh * t29 - 0.2884936567583026D3 - 0.120D3 * t27 * 
     #lh - t18 * t44 - 0.90D2 * t33 * lh - 0.15D2 * t33 * t18) * t21
      t52 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t53 = t23 * t52
      t55 = lh * t21
      t56 = t15 * x4
      t59 = log(0.4D1 * t12 * t56)
      t65 = t21 * t23
      t66 = t59 ** 2
      t73 = t44 * t21
      t74 = t73 * t53
      t76 = 0.1D1 / x4
      t78 = x3 * t9
      t79 = t78 * t11
      t82 = log(0.4D1 * t79 * t56)
      t90 = 0.1D1 / x3
      t97 = log(0.4D1 * t78 * t11 * t13 * t14)
      t104 = t97 ** 2
      t113 = t22 * t23 * t24 / 0.2880D4 + t36 * t23 * t37 / 0.2880D4 + t
     #51 * t53 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t37 - t59 * t52) + 
     #0.90D2 * t65 * (t66 * t52 / 0.2D1 - t59 * t37 + t24) + t74) * t76 
     #/ 0.2880D4 - (0.90D2 * t65 * (t82 * t52 - t37) + 0.180D3 * t55 * t
     #53) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 * (-t97 * t52 +
     # t37) + 0.90D2 * t65 * (-t97 * t37 + t24 + t104 * t52 / 0.2D1) + t
     #74) * t90 / 0.2880D4
      t114 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t113)
      t116 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t117 = s * t116
      t118 = t1 * x1
      t119 = t117 * t118
      t120 = t1 * t4
      t121 = t120 * x4
      t122 = t117 * t121
      t123 = -0.1D1 + x4
      t124 = t120 * t123
      t125 = t117 * t124
      t126 = t116 ** 2
      t129 = t4 * x1
      t131 = s * t126 * t21 * t129 * x4
      t134 = x4 * t123
      t135 = t126 ** 2
      t137 = t134 * t135 * t14
      t140 = log(-0.4D1 * t13 * t9 * t11 * t137)
      t141 = t140 * t126
      t143 = 0.1D1 / (-0.2D1 + t116)
      t144 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t145 = t143 * t144
      t147 = t126 * t143
      t148 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t149 = t147 * t148
      t156 = t140 ** 2
      t157 = t156 * t126
      t160 = bbbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t165 = t73 * t23
      t166 = t147 * t144
      t171 = x3 * t13 * t12
      t174 = log(-0.4D1 * t171 * t137)
      t175 = t174 * t126
      t180 = t55 * t23
      t187 = (-0.180D3 * t55 * t23 * (-t141 * t145 + t149) + 0.90D2 * t6
     #5 * (-t141 * t143 * t148 + t157 * t145 / 0.2D1 + t147 * t160) + t1
     #65 * t166) * t76 / 0.2880D4 - (0.90D2 * t65 * (-t149 + t175 * t145
     #) + 0.180D3 * t180 * t166) * t90 * t76 / 0.2880D4
      t188 = FJET(XB1, XB2, s, 0.0D0, t119, -t122, t125, -t131, t187)
      t190 = bbbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t193 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t194 = t23 * t193
      t197 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t208 = t73 * t194
      t236 = t22 * t23 * t190 / 0.2880D4 + t51 * t194 / 0.2880D4 + (-0.1
     #80D3 * t55 * t23 * (-t59 * t193 + t197) + 0.90D2 * t65 * (-t59 * t
     #197 + t66 * t193 / 0.2D1 + t190) + t208) * t76 / 0.2880D4 + t36 * 
     #t23 * t197 / 0.2880D4 - (0.90D2 * t65 * (-t197 + t82 * t193) + 0.1
     #80D3 * t55 * t194) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 
     #* (t197 - t97 * t193) + 0.90D2 * t65 * (t104 * t193 / 0.2D1 + t190
     # - t97 * t197) + t208) * t90 / 0.2880D4
      t237 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t236)
      t239 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t240 = t147 * t239
      t241 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t242 = t143 * t241
      t247 = t147 * t241
      t260 = bbbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t271 = -(0.90D2 * t65 * (-t240 + t175 * t242) + 0.180D3 * t180 * t
     #247) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t240 - t141
     # * t242) + 0.90D2 * t65 * (t157 * t242 / 0.2D1 + t147 * t260 - t14
     #1 * t143 * t239) + t165 * t247) * t76 / 0.2880D4
      t272 = FJET(XB1, XB2, s, t119, 0.0D0, t125, -t122, -t131, t271)
      t274 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t275 = s * t274
      t276 = t118 * x3
      t277 = t275 * t276
      t278 = -0.1D1 + x3
      t279 = t118 * t278
      t280 = t275 * t279
      t281 = t275 * t120
      t282 = t274 ** 2
      t286 = s * t282 * t21 * t129 * x3
      t288 = 0.1D1 / (-0.2D1 + t274)
      t289 = t282 * t288
      t290 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t291 = t289 * t290
      t292 = t14 * t278
      t293 = t282 ** 2
      t298 = log(-0.4D1 * t171 * t292 * x4 * t293)
      t299 = t298 * t282
      t300 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t301 = t288 * t300
      t306 = t289 * t300
      t316 = log(-0.4D1 * t79 * t15 * t278 * t293)
      t317 = t316 * t282
      t323 = bbbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t327 = t316 ** 2
      t328 = t327 * t282
      t338 = -(0.90D2 * t65 * (-t291 + t299 * t301) + 0.180D3 * t180 * t
     #306) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t291 - t317
     # * t301) + 0.90D2 * t65 * (t289 * t323 - t317 * t288 * t290 + t328
     # * t301 / 0.2D1) + t165 * t306) * t90 / 0.2880D4
      t339 = FJET(XB1, XB2, s, t277, -t280, 0.0D0, -t281, -t286, t338)
      t341 = KAPPA2(x1, x2, x3, x4, z)
      t342 = s * t341
      t343 = t342 * t276
      t344 = t342 * t279
      t345 = t342 * t121
      t346 = t342 * t124
      t347 = t341 ** 2
      t352 = cos(t7)
      t355 = sqrt(x3 * t278 * t134)
      t360 = s * t347 * t21 * t129 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t352 * t355)
      t361 = t347 ** 2
      t366 = log(0.4D1 * t171 * t134 * t292 * t361)
      t367 = t366 * t347
      t369 = 0.1D1 / (-0.2D1 + t341)
      t370 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t373 = t347 * t369
      t374 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t382 = 0.90D2 * t65 * (-t367 * t369 * t370 + t373 * t374) - 0.180D
     #3 * t180 * t373 * t370
      t386 = FJET(XB1, XB2, s, t343, -t344, -t345, t346, t360, -t382 * t
     #90 * t76 / 0.2880D4)
      t388 = t90 * t76
      t391 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t392 = t288 * t391
      t394 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t395 = t289 * t394
      t399 = t289 * t391
      t410 = bbbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t423 = -(0.90D2 * t65 * (t299 * t392 - t395) + 0.180D3 * t180 * t3
     #99) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t395 - t317 
     #* t392) + 0.90D2 * t65 * (t289 * t410 - t317 * t288 * t394 + t328 
     #* t392 / 0.2D1) + t165 * t399) * t90 / 0.2880D4
      t424 = FJET(XB1, XB2, s, -t280, t277, -t281, 0.0D0, -t286, t423)
      t426 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t428 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t437 = 0.90D2 * t65 * (t373 * t426 - t367 * t369 * t428) - 0.180D3
     # * t180 * t373 * t428
      t441 = FJET(XB1, XB2, s, -t344, t343, t346, -t345, t360, -t437 * t
     #90 * t76 / 0.2880D4)
      bbbbH3n1e1 = t114 * t113 + t188 * t187 + t237 * t236 + t272 * t271
     # + t339 * t338 - t386 * t382 * t388 / 0.2880D4 + t424 * t423 - t44
     #1 * t437 * t388 / 0.2880D4

      end function



      doubleprecision function bbbbH3n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH31J1
      doubleprecision bbbbH31J2
      doubleprecision bbbbH31J3
      doubleprecision bbbbH32J1
      doubleprecision bbbbH32J2
      doubleprecision bbbbH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = x1 ** 2
      t17 = t4 ** 2
      t18 = t16 * t17
      t22 = log(0.4D1 * t15 * t18 * x4)
      t23 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t28 = lh * t6
      t29 = t7 * t23
      t31 = 0.180D3 * t28 * t29
      t33 = 0.1D1 / x4
      t36 = 0.1D1 / x3
      t41 = x3 * t12
      t46 = log(0.4D1 * t41 * t14 * t16 * t17)
      t54 = bbbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t60 = log(0.4D1 * t15 * t18)
      t63 = (-0.180D3 * lh - 0.90D2 * t60) * t6
      t67 = lh ** 2
      t69 = 0.3141592653589793D1 ** 2
      t73 = t60 ** 2
      t76 = (0.180D3 * t67 - 0.30D2 * t69 + 0.180D3 * t60 * lh + 0.45D2 
     #* t73) * t6
      t79 = (0.90D2 * t8 * (t9 - t22 * t23) - t31) * t33 / 0.2880D4 + t8
     # * t23 * t36 * t33 / 0.32D2 + (0.90D2 * t8 * (-t46 * t23 + t9) - t
     #31) * t36 / 0.2880D4 + t8 * t54 / 0.32D2 + t63 * t7 * t9 / 0.2880D
     #4 + t76 * t29 / 0.2880D4
      t80 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t79)
      t82 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t83 = s * t82
      t84 = t1 * x1
      t85 = t83 * t84
      t86 = t1 * t4
      t87 = t86 * x4
      t88 = t83 * t87
      t89 = -0.1D1 + x4
      t90 = t86 * t89
      t91 = t83 * t90
      t92 = t82 ** 2
      t95 = t4 * x1
      t97 = s * t92 * t6 * t95 * x4
      t98 = t8 * t92
      t100 = 0.1D1 / (-0.2D1 + t82)
      t101 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t102 = t100 * t101
      t103 = t36 * t33
      t109 = x4 * t89
      t110 = t92 ** 2
      t115 = log(-0.4D1 * t16 * t12 * t14 * t109 * t110 * t17)
      t116 = t115 * t92
      t118 = t92 * t100
      t119 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t124 = t28 * t7
      t131 = t98 * t102 * t103 / 0.32D2 + (0.90D2 * t8 * (-t116 * t102 +
     # t118 * t119) - 0.180D3 * t124 * t118 * t101) * t33 / 0.2880D4
      t132 = FJET(XB1, XB2, s, 0.0D0, t85, -t88, t91, -t97, t131)
      t134 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t136 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t140 = t7 * t134
      t142 = 0.180D3 * t28 * t140
      t160 = bbbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t165 = (0.90D2 * t8 * (-t22 * t134 + t136) - t142) * t33 / 0.2880D
     #4 + t63 * t7 * t136 / 0.2880D4 + t8 * t134 * t36 * t33 / 0.32D2 + 
     #(0.90D2 * t8 * (t136 - t46 * t134) - t142) * t36 / 0.2880D4 + t8 *
     # t160 / 0.32D2 + t76 * t140 / 0.2880D4
      t166 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t165)
      t168 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t170 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t171 = t100 * t170
      t185 = (0.90D2 * t8 * (t118 * t168 - t116 * t171) - 0.180D3 * t124
     # * t118 * t170) * t33 / 0.2880D4 + t98 * t171 * t103 / 0.32D2
      t186 = FJET(XB1, XB2, s, t85, 0.0D0, t91, -t88, -t97, t185)
      t188 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t189 = s * t188
      t190 = t84 * x3
      t191 = t189 * t190
      t192 = -0.1D1 + x3
      t193 = t84 * t192
      t194 = t189 * t193
      t195 = t189 * t86
      t196 = t188 ** 2
      t200 = s * t196 * t6 * t95 * x3
      t201 = t8 * t196
      t203 = 0.1D1 / (-0.2D1 + t188)
      t204 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t205 = t203 * t204
      t209 = t196 * t203
      t210 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t213 = t196 ** 2
      t218 = log(-0.4D1 * t41 * t14 * t18 * t192 * t213)
      t219 = t218 * t196
      t230 = t201 * t205 * t103 / 0.32D2 + (0.90D2 * t8 * (t209 * t210 -
     # t219 * t205) - 0.180D3 * t124 * t209 * t204) * t36 / 0.2880D4
      t231 = FJET(XB1, XB2, s, t191, -t194, 0.0D0, -t195, -t200, t230)
      t233 = KAPPA2(x1, x2, x3, x4, z)
      t234 = s * t233
      t235 = t234 * t190
      t236 = t234 * t193
      t237 = t234 * t87
      t238 = t234 * t90
      t239 = t233 ** 2
      t244 = cos(t10)
      t247 = sqrt(x3 * t192 * t109)
      t252 = s * t239 * t6 * t95 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 *
     # t244 * t247)
      t253 = t8 * t239
      t255 = 0.1D1 / (-0.2D1 + t233)
      t256 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t258 = t255 * t256 * t103
      t261 = FJET(XB1, XB2, s, t235, -t236, -t237, t238, t252, -t253 * t
     #258 / 0.32D2)
      t263 = t7 * t239
      t267 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t268 = t203 * t267
      t272 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t284 = t201 * t268 * t103 / 0.32D2 + (0.90D2 * t8 * (t209 * t272 -
     # t219 * t268) - 0.180D3 * t124 * t209 * t267) * t36 / 0.2880D4
      t285 = FJET(XB1, XB2, s, -t194, t191, -t195, 0.0D0, -t200, t284)
      t287 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t289 = t255 * t287 * t103
      t292 = FJET(XB1, XB2, s, -t236, t235, t238, -t237, t252, -t253 * t
     #289 / 0.32D2)
      bbbbH3n1e0 = t80 * t79 + t132 * t131 + t165 * t166 + t186 * t185 +
     # t231 * t230 - t261 * t6 * t263 * t258 / 0.32D2 + t285 * t284 - t2
     #92 * t6 * t263 * t289 / 0.32D2

      end function



      doubleprecision function bbbbH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH31J1
      doubleprecision bbbbH31J2
      doubleprecision bbbbH31J3
      doubleprecision bbbbH32J1
      doubleprecision bbbbH32J2
      doubleprecision bbbbH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t19 = x1 ** 2
      t20 = t4 ** 2
      t24 = log(0.4D1 * t15 / t16 * t19 * t20)
      t27 = (-0.180D3 * lh - 0.90D2 * t24) * t6
      t28 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t32 = 0.1D1 / x4
      t36 = 0.1D1 / x3
      t40 = t8 * t9 / 0.32D2 + t27 * t7 * t28 / 0.2880D4 + t8 * t28 * t3
     #2 / 0.32D2 + t8 * t28 * t36 / 0.32D2
      t41 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t40)
      t43 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t44 = s * t43
      t45 = t1 * x1
      t46 = t44 * t45
      t47 = t1 * t4
      t49 = t44 * t47 * x4
      t52 = t44 * t47 * (-0.1D1 + x4)
      t53 = t43 ** 2
      t56 = t4 * x1
      t58 = s * t53 * t6 * t56 * x4
      t59 = t8 * t53
      t61 = 0.1D1 / (-0.2D1 + t43)
      t62 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t67 = FJET(XB1, XB2, s, 0.0D0, t46, -t49, t52, -t58, t59 * t61 * t
     #62 * t32 / 0.32D2)
      t70 = t53 * t61
      t75 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t82 = bbbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t88 = t27 * t7 * t75 / 0.2880D4 + t8 * t75 * t32 / 0.32D2 + t8 * t
     #82 / 0.32D2 + t8 * t75 * t36 / 0.32D2
      t89 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t88)
      t91 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t96 = FJET(XB1, XB2, s, t46, 0.0D0, t52, -t49, -t58, t59 * t61 * t
     #91 * t32 / 0.32D2)
      t103 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t104 = s * t103
      t106 = t104 * t45 * x3
      t109 = t104 * t45 * (-0.1D1 + x3)
      t110 = t104 * t47
      t111 = t103 ** 2
      t115 = s * t111 * t6 * t56 * x3
      t116 = t8 * t111
      t118 = 0.1D1 / (-0.2D1 + t103)
      t119 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t124 = FJET(XB1, XB2, s, t106, -t109, 0.0D0, -t110, -t115, t116 * 
     #t118 * t119 * t36 / 0.32D2)
      t127 = t111 * t118
      t132 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t137 = FJET(XB1, XB2, s, -t109, t106, -t110, 0.0D0, -t115, t116 * 
     #t118 * t132 * t36 / 0.32D2)
      bbbbH3n1em1 = t41 * t40 + t67 * t6 * t7 * t70 * t62 * t32 / 0.32D2
     # + t89 * t88 + t96 * t6 * t7 * t70 * t91 * t32 / 0.32D2 + t124 * t
     #6 * t7 * t127 * t119 * t36 / 0.32D2 + t137 * t6 * t7 * t127 * t132
     # * t36 / 0.32D2

      end function



      doubleprecision function bbbbH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH31J1
      doubleprecision bbbbH31J2
      doubleprecision bbbbH31J3
      doubleprecision bbbbH32J1
      doubleprecision bbbbH32J2
      doubleprecision bbbbH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t8 * t9 / 0.
     #32D2)
      t16 = bbbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t19 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t8 * t16 / 0
     #.32D2)
      bbbbH3n1em2 = t12 * t6 * t7 * t9 / 0.32D2 + t19 * t6 * t7 * t16 / 
     #0.32D2

      end function



      doubleprecision function bbbbH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH31J1
      doubleprecision bbbbH31J2
      doubleprecision bbbbH31J3
      doubleprecision bbbbH32J1
      doubleprecision bbbbH32J2
      doubleprecision bbbbH32J3
      bbbbH3n1em3 = 0.0D0

      end function



      doubleprecision function bbbbH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH31J1
      doubleprecision bbbbH31J2
      doubleprecision bbbbH31J3
      doubleprecision bbbbH32J1
      doubleprecision bbbbH32J2
      doubleprecision bbbbH32J3
      bbbbH3n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bbbbH31J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = x1 ** 2
      t11 = t10 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t19 = t4 ** 2
      t21 = t8 ** 2
      t24 = 0.1D1 - x1
      t28 = cos(x2 * 0.3141592653589793D1)
      t30 = 0.1D1 - x4
      t33 = sqrt(x3 * t13 * x4 * t30)
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33
      t37 = t24 * t36
      t46 = t24 ** 2
      t47 = t36 ** 2
      t48 = t46 * t47
      t51 = s * t3
      t52 = t7 * x1
      t55 = t52 * t13
      t57 = s - t51 * t52 * x3 - t51 * t55
      t58 = t57 * t1
      t59 = t58 * t4
      t61 = t8 * t10 * t14
      t65 = t30 ** 2
      t73 = t7 * t24
      t78 = s - t51 * t73 * x4 - t51 * t73 * t30
      t79 = t78 * t1
      t82 = t5 * t9
      t83 = t58 * t82
      t85 = t10 * t13 * t37
      t93 = t4 * t8
      t95 = t24 * t30
      t96 = x1 * t13
      t97 = t95 * t96
      t102 = t57 * t78
      t103 = t102 * s
      t104 = t3 * t7
      t111 = t8 * x1 * t37
      t124 = 0.2D1 * t2 * t5 * t9 * t11 * t14 * t13 - 0.2D1 * t2 * t19 *
     # t21 * t11 * t14 * t37 + t2 * t19 * t3 * t21 * t7 * t11 * t13 * t4
     #8 + 0.2D1 * t59 * t61 - t59 * t8 * t46 * t65 + t58 * t19 * t21 * t
     #10 * t48 + 0.2D1 * t58 - t79 * t4 * t61 - 0.2D1 * t83 * t85 + 0.2D
     #1 * t83 * t46 * t30 * x1 * t36 - t79 * t93 * t97 + 0.2D1 * t58 * t
     #3 * t55 - 0.2D1 * t103 * t104 * t96 + 0.4D1 * t102 * s * t4 * t111
     # - 0.2D1 * t59 * t111 - t58 * t93 * t97 + 0.2D1 * t79 * t82 * t85 
     #- 0.2D1 * t103 * t104 * t95
      bbbbH31J1 = 0.16D2 / 0.3D1 * wd * t124 / t57 / t78

      end function
  
   
 

      doubleprecision function bbbbH31J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t13 = x1 ** 2
      t15 = 0.1D1 - x3
      t17 = 0.1D1 - x1
      t18 = t17 ** 2
      t22 = cos(x2 * 0.3141592653589793D1)
      t24 = 0.1D1 - x4
      t27 = sqrt(x3 * t15 * x4 * t24)
      t30 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27
      t31 = t30 ** 2
      t32 = t18 * t31
      t35 = s * t3
      t36 = t8 * x1
      t41 = s - t35 * t36 * x3 - t35 * t36 * t15
      t42 = t41 * t1
      t45 = t24 ** 2
      t48 = t8 * t17
      t53 = s - t35 * t48 * x4 - t35 * t48 * t24
      t54 = t53 * t1
      t55 = t4 * t9
      t59 = t17 * t24 * x1 * t15
      t69 = t4 * t3 * t9 * t8
      t72 = t17 * t30
      t76 = t41 * t53
      t87 = t15 ** 2
      bbbbH31J2 = 0.16D2 / 0.3D1 * wd * (-t1 * s * t5 * t3 * t10 * t8 * 
     #t13 * x1 * t15 * t32 - t42 * t4 * t9 * t18 * t45 + t54 * t55 * t59
     # - t42 * t5 * t10 * t13 * t32 + t42 * t55 * t59 - 0.2D1 * t54 * t6
     #9 * t13 * t15 * t72 - 0.4D1 * t76 * s * t4 * t9 * x1 * t72 - 0.3D1
     # * t76 * s - t54 * t4 * t9 * t13 * t87 - 0.2D1 * t42 * t69 * t18 *
     # t24 * x1 * t30) / t41 / t53

      end function
  
   
 

      doubleprecision function bbbbH31J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t10 * t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t16 = t3 ** 2
      t17 = t16 ** 2
      t18 = x1 ** 2
      t20 = 0.1D1 - x1
      t21 = t20 ** 2
      t25 = cos(x2 * 0.3141592653589793D1)
      t27 = 0.1D1 - x4
      t30 = sqrt(x3 * t7 * x4 * t27)
      t33 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t25 * t30
      t34 = t33 ** 2
      t35 = t21 * t34
      t38 = t3 * t20
      t43 = s - t2 * t38 * x4 - t2 * t38 * t27
      t44 = t10 * t43
      t49 = t20 * t33
      t64 = t13 * t1 * t16 * t3
      bbbbH31J3 = 0.16D2 / 0.3D1 * wd * (-t12 * t14 * t17 * t18 * t35 - 
     #t44 * s - 0.2D1 * t44 * s * t13 * t16 * x1 * t49 - t11 * s * t14 *
     # t1 * t17 * t3 * t18 * x1 * t7 * t35 - t12 * t64 * t21 * t27 * x1 
     #* t33 - t43 * t11 * t64 * t18 * t7 * t49) / t10 / t43

      end function
  
   
 

      doubleprecision function bbbbH32J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t10 * t11
      t14 = 0.1D1 - x4
      t15 = t14 ** 2
      t20 = t4 ** 2
      t22 = t8 ** 2
      t28 = cos(x2 * 0.3141592653589793D1)
      t29 = 0.1D1 - x3
      t33 = sqrt(x3 * t29 * x4 * t14)
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33
      t37 = x1 * t36
      t46 = x1 ** 2
      t47 = t36 ** 2
      t51 = s * t3
      t52 = t7 * x1
      t57 = s - t51 * t52 * x3 - t51 * t52 * t29
      t58 = t57 * t1
      t61 = t8 * t11 * t15
      t63 = t7 * t10
      t66 = t63 * t14
      t68 = s - t51 * t63 * x4 - t51 * t66
      t69 = t68 * t1
      t75 = t69 * t4
      t79 = t29 ** 2
      t83 = t5 * t9
      t84 = t69 * t83
      t86 = t11 * t14 * t37
      t92 = t4 * t8
      t94 = t10 * t14
      t95 = x1 * t29
      t96 = t94 * t95
      t98 = t57 * t68
      t99 = t98 * s
      t100 = t3 * t7
      t107 = t10 * t36
      t108 = t8 * x1 * t107
      t125 = 0.2D1 * t2 * t5 * t9 * t12 * t15 * t14 - 0.2D1 * t2 * t20 *
     # t22 * t12 * t15 * t37 + t2 * t20 * t3 * t22 * t7 * t12 * t14 * t4
     #6 * t47 - t58 * t4 * t61 + t69 * t20 * t22 * t46 * t11 * t47 + 0.2
     #D1 * t75 * t61 - t75 * t8 * t46 * t79 + 0.2D1 * t69 - 0.2D1 * t84 
     #* t86 + 0.2D1 * t58 * t83 * t86 - t58 * t92 * t96 - 0.2D1 * t99 * 
     #t100 * t95 + 0.4D1 * t98 * s * t4 * t108 - 0.2D1 * t99 * t100 * t9
     #4 + 0.2D1 * t84 * t46 * t29 * t107 + 0.2D1 * t69 * t3 * t66 - t69 
     #* t92 * t96 - 0.2D1 * t75 * t108
      bbbbH32J1 = 0.16D2 / 0.3D1 * wd * t125 / t57 / t68

      end function
  
   
 

      doubleprecision function bbbbH32J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t16 = 0.1D1 - x4
      t18 = x1 ** 2
      t22 = cos(x2 * 0.3141592653589793D1)
      t23 = 0.1D1 - x3
      t27 = sqrt(x3 * t23 * x4 * t16)
      t30 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27
      t31 = t30 ** 2
      t35 = s * t3
      t36 = t8 * x1
      t41 = s - t35 * t36 * x3 - t35 * t36 * t23
      t42 = t41 * t1
      t45 = t16 ** 2
      t48 = t8 * t13
      t53 = s - t35 * t48 * x4 - t35 * t48 * t16
      t54 = t53 * t1
      t55 = t4 * t9
      t59 = t13 * t16 * x1 * t23
      t63 = t23 ** 2
      t73 = t4 * t3 * t9 * t8
      t76 = t13 * t30
      t80 = t41 * t53
      bbbbH32J2 = 0.16D2 / 0.3D1 * wd * (-t1 * s * t5 * t3 * t10 * t8 * 
     #t14 * t13 * t16 * t18 * t31 - t42 * t4 * t9 * t14 * t45 + t54 * t5
     #5 * t59 - t54 * t4 * t9 * t18 * t63 - t54 * t5 * t10 * t18 * t14 *
     # t31 - 0.2D1 * t54 * t73 * t18 * t23 * t76 - 0.4D1 * t80 * s * t4 
     #* t9 * x1 * t76 - 0.3D1 * t80 * s + t42 * t55 * t59 - 0.2D1 * t42 
     #* t73 * t14 * t16 * x1 * t30) / t41 / t53

      end function
  
   
 

      doubleprecision function bbbbH32J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t13 = t1 ** 2
      t15 = t3 ** 2
      t17 = t13 * t1 * t15 * t3
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t21 = 0.1D1 - x4
      t26 = cos(x2 * 0.3141592653589793D1)
      t30 = sqrt(x3 * t7 * x4 * t21)
      t33 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t26 * t30
      t38 = t13 ** 2
      t41 = t15 ** 2
      t46 = x1 ** 2
      t47 = t33 ** 2
      t51 = t3 * t19
      t56 = s - t2 * t51 * x4 - t2 * t51 * t21
      t57 = t10 * t56
      t61 = t19 * t33
      t65 = t56 * t11
      bbbbH32J3 = 0.16D2 / 0.3D1 * wd * (-t10 * t11 * t17 * t20 * t21 * 
     #x1 * t33 - t11 * s * t38 * t1 * t41 * t3 * t20 * t19 * t21 * t46 *
     # t47 - 0.2D1 * t57 * s * t13 * t15 * x1 * t61 - t65 * t38 * t41 * 
     #t46 * t20 * t47 - t57 * s - t65 * t17 * t46 * t7 * t61) / t10 / t5
     #6

      end function
  
 