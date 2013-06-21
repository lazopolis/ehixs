  
      subroutine rrqg2qght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh61J1  
      doubleprecision rrqg2qgh61J2  
      doubleprecision rrqg2qgh61J3  
      doubleprecision rrqg2qgh61J4  
      doubleprecision rrqg2qgh61J5  
      doubleprecision rrqg2qgh61J6  
      doubleprecision rrqg2qgh62J1  
      doubleprecision rrqg2qgh62J2  
      doubleprecision rrqg2qgh62J3  
      doubleprecision rrqg2qgh62J4  
      doubleprecision rrqg2qgh62J5  
      doubleprecision rrqg2qgh62J6  
      doubleprecision rrqg2qgh62J7  
      doubleprecision rrqg2qgh63J1  
      doubleprecision rrqg2qgh63J2  
      doubleprecision rrqg2qgh63J3  
      doubleprecision rrqg2qgh63J4  
      doubleprecision rrqg2qgh63J5  
      doubleprecision rrqg2qgh63J6  
      doubleprecision rrqg2qgh63J7  
      doubleprecision rrqg2qgh64J1  
      doubleprecision rrqg2qgh64J2  
      doubleprecision rrqg2qgh64J3  
      doubleprecision rrqg2qgh64J4  
      doubleprecision rrqg2qgh64J5  
      doubleprecision rrqg2qgh64J6  
      doubleprecision rrqg2qght6s1e1  
      doubleprecision rrqg2qght6s1e0  
      doubleprecision rrqg2qght6s1em1  
      doubleprecision rrqg2qght6s1em2  
      doubleprecision rrqg2qght6s1em3  
      doubleprecision rrqg2qght6s1em4  
      doubleprecision rrqg2qght6s2e1  
      doubleprecision rrqg2qght6s2e0  
      doubleprecision rrqg2qght6s2em1  
      doubleprecision rrqg2qght6s2em2  
      doubleprecision rrqg2qght6s2em3  
      doubleprecision rrqg2qght6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = 0.3141592653589793D1 * t3
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = rrqg2qgh63J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x4 * 0.3141592653589793D1
      t17 = Sin(t16)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t19 * t21
      t24 = log(0.4D1 * t22)
      t25 = t24 ** 2
      t26 = t25 * 0.3141592653589793D1
      t27 = t3 * lh
      t30 = 0.3141592653589793D1 ** 2
      t33 = lh ** 2
      t36 = -0.60D2 * lh * t30 + 0.2884936567583026D3 + 0.120D3 * t33 * 
     #lh
      t39 = t25 * t24 * 0.3141592653589793D1
      t42 = t24 * 0.3141592653589793D1
      t43 = 0.180D3 * t33
      t44 = 0.30D2 * t30
      t45 = -t43 + t44
      t46 = t3 * t45
      t49 = (0.90D2 * t26 * t27 + t4 * t36 + 0.15D2 * t39 * t3 - t42 * t
     #46) * t5
      t50 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t60 = (-0.180D3 * t42 * t27 - 0.45D2 * t26 * t3 + t4 * t45) * t5
      t61 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t65 = t4 * lh
      t70 = (0.180D3 * t65 + 0.90D2 * t42 * t3) * t5
      t71 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t82 = t30 ** 2
      t83 = t33 ** 2
      t89 = t25 ** 2
      t94 = (-0.30D2 * t39 * t27 + t26 * t46 / 0.2D1 - t42 * t3 * t36 + 
     #t4 * (-0.5769873135166051D3 * lh - t82 - 0.60D2 * t83 + 0.60D2 * t
     #33 * t30) - 0.15D2 / 0.4D1 * t89 * 0.3141592653589793D1 * t3) * t5
      t95 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t99 = 0.3141592653589793D1 * t5
      t100 = x1 ** 2
      t101 = x3 * t100
      t102 = t101 * t18
      t103 = t15 * t21
      t104 = -0.1D1 + x3
      t105 = 0.1D1 / t104
      t106 = t103 * t105
      t109 = log(-0.4D1 * t102 * t106)
      t110 = cos(t16)
      t112 = Sqrt(-x3 * t104)
      t117 = 0.1D1 / (-z - x3 + 0.2D1 * t110 * t112 * z)
      t118 = t109 * t117
      t120 = t109 ** 2
      t121 = t120 * t117
      t124 = t3 * t61
      t127 = log(0.4D1 * t101 * t22)
      t128 = t127 * t3
      t131 = t127 ** 2
      t132 = t131 * t3
      t139 = 0.3141592653589793D1 * lh
      t141 = t3 * t50
      t148 = -t45
      t149 = 0.3141592653589793D1 * t148
      t152 = t117 * t95 + t3 * t95
      t156 = 0.1D1 / x3
      t158 = 0.1D1 / x1
      t161 = t4 * t148
      t162 = t100 * t18
      t165 = log(0.4D1 * t162 * t103)
      t170 = t165 ** 2
      t173 = t170 * t165
      t182 = -t4 * t36
      t195 = x2 ** 2
      t196 = x3 * t195
      t197 = t196 * t100
      t200 = log(0.4D1 * t197 * t22)
      t201 = t200 * t3
      t207 = log(-0.4D1 * t197 * t19 * t21 * t105)
      t221 = 0.1D1 / x2
      t222 = t221 * t158
      t225 = t195 * t100
      t228 = log(0.4D1 * t225 * t22)
      t229 = t228 ** 2
      t230 = t229 * t3
      t233 = t228 * t3
      t244 = 0.3141592653589793D1 * t45
      t245 = t244 * t5
      t246 = t8 * t3
      t247 = t246 * t95
      t253 = t50 * 0.3141592653589793D1
      t256 = t61 * 0.3141592653589793D1
      t258 = t95 * 0.3141592653589793D1
      t262 = x3 * t18
      t265 = log(-0.4D1 * t262 * t106)
      t269 = log(0.4D1 * t262 * t103)
      t272 = t8 * (-t265 * t117 - t269 * t3)
      t274 = t269 ** 2
      t277 = t265 ** 2
      t282 = t9 * (-t274 * t269 * t3 / 0.6D1 - t277 * t265 * t117 / 0.6D
     #1)
      t294 = t8 * (t117 + t3)
      t305 = t8 * (t277 * t117 / 0.2D1 + t274 * t3 / 0.2D1)
      t310 = t99 * t8
      t313 = 0.180D3 * lh
      t314 = t196 * t18
      t317 = log(-0.4D1 * t314 * t106)
      t319 = -t313 - 0.90D2 * t317
      t323 = t317 ** 2
      t325 = 0.180D3 * t317 * lh + 0.45D2 * t323 + t43 - t44
      t331 = log(0.4D1 * t196 * t22)
      t332 = t331 * t3
      t335 = t331 ** 2
      t338 = -0.180D3 * t332 * lh - 0.45D2 * t335 * t3 + t46
      t342 = 0.180D3 * t27 + 0.90D2 * t332
      t349 = t195 * t18
      t352 = log(0.4D1 * t349 * t103)
      t353 = t352 * t3
      t358 = t352 ** 2
      t359 = t358 * t3
      t364 = t358 * t352 * t3
      t373 = 0.3141592653589793D1 * t36 * t5
      t385 = t4 * t9 * t10 / 0.32D2 - t49 * t8 * t50 / 0.2880D4 - t60 * 
     #t8 * t61 / 0.2880D4 - t70 * t8 * t71 / 0.2880D4 - t94 * t8 * t95 /
     # 0.2880D4 + (0.90D2 * t99 * t8 * (-t118 * t50 + t121 * t95 / 0.2D1
     # + t124 - t128 * t50 + t117 * t61 + t132 * t95 / 0.2D1) - 0.180D3 
     #* t139 * t9 * (-t118 * t95 + t141 + t117 * t50 - t128 * t95) + t14
     #9 * t9 * t152) * t156 * t158 / 0.1440D4 + (t161 * t9 * (t50 - t165
     # * t95) + 0.90D2 * t4 * t9 * (t170 * t50 / 0.2D1 + t71 - t173 * t9
     #5 / 0.6D1 - t165 * t61) + t182 * t9 * t95 - 0.180D3 * t65 * t9 * (
     #t61 + t170 * t95 / 0.2D1 - t165 * t50)) * t158 / 0.1440D4 + (-0.90
     #D2 * t99 * t8 * (t201 * t95 + (-t50 + t207 * t95) * t117 - t141) -
     # 0.180D3 * t139 * t9 * t152) * t156 * t222 / 0.720D3 + (-0.90D2 * 
     #t99 * t8 * (-t124 - t230 * t95 / 0.2D1 + t233 * t50) + 0.180D3 * t
     #139 * t9 * (-t141 + t233 * t95) - t245 * t247) * t221 * t158 / 0.7
     #20D3 + ((-0.180D3 * t253 * lh + 0.90D2 * t256 - t258 * t45) * t5 *
     # t272 + 0.90D2 * t258 * t282 + (-0.180D3 * t256 * lh - t258 * t36 
     #+ 0.90D2 * t71 * 0.3141592653589793D1 - t253 * t45) * t5 * t294 + 
     #(-0.180D3 * t258 * lh + 0.90D2 * t253) * t5 * t305) * t156 / 0.288
     #0D4 + t310 * (0.90D2 * t124 + (0.90D2 * t61 + t319 * t50 + t325 * 
     #t95) * t117 - t338 * t95 - t342 * t50) * t156 * t221 / 0.1440D4 + 
     #(t244 * t9 * (-t141 + t353 * t95) - 0.90D2 * t99 * t8 * (-t359 * t
     #50 / 0.2D1 - t3 * t71 + t364 * t95 / 0.6D1 + t353 * t61) - t373 * 
     #t247 + 0.180D3 * t139 * t9 * (-t124 + t353 * t50 - t359 * t95 / 0.
     #2D1)) * t221 / 0.1440D4
      t386 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t385)
      t388 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t390 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t392 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t393 = t3 * t392
      t405 = t3 * t390
      t412 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t421 = t246 * t388
      t433 = rrqg2qgh61J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t449 = t390 * 0.3141592653589793D1
      t452 = t392 * 0.3141592653589793D1
      t454 = t388 * 0.3141592653589793D1
      t499 = t117 * t388 + t3 * t388
      t564 = t310 * (-t338 * t388 - t342 * t390 + 0.90D2 * t393 + (0.90D
     #2 * t392 + t319 * t390 + t325 * t388) * t117) * t156 * t221 / 0.14
     #40D4 + (t244 * t9 * (-t405 + t353 * t388) - 0.90D2 * t99 * t8 * (-
     #t359 * t390 / 0.2D1 - t3 * t412 + t353 * t392 + t364 * t388 / 0.6D
     #1) - t373 * t421 + 0.180D3 * t139 * t9 * (-t393 + t353 * t390 - t3
     #59 * t388 / 0.2D1)) * t221 / 0.1440D4 + t4 * t9 * t433 / 0.32D2 - 
     #t49 * t8 * t390 / 0.2880D4 - t60 * t8 * t392 / 0.2880D4 - t70 * t8
     # * t412 / 0.2880D4 - t94 * t8 * t388 / 0.2880D4 + ((-0.180D3 * t44
     #9 * lh + 0.90D2 * t452 - t454 * t45) * t5 * t272 + 0.90D2 * t454 *
     # t282 + (-0.180D3 * t452 * lh - t454 * t36 + 0.90D2 * t412 * 0.314
     #1592653589793D1 - t449 * t45) * t5 * t294 + (-0.180D3 * t454 * lh 
     #+ 0.90D2 * t449) * t5 * t305) * t156 / 0.2880D4 + (0.90D2 * t99 * 
     #t8 * (-t118 * t390 + t132 * t388 / 0.2D1 + t121 * t388 / 0.2D1 + t
     #393 + t117 * t392 - t128 * t390) - 0.180D3 * t139 * t9 * (t117 * t
     #390 - t118 * t388 - t128 * t388 + t405) + t149 * t9 * t499) * t156
     # * t158 / 0.1440D4 + (t161 * t9 * (t390 - t165 * t388) + 0.90D2 * 
     #t4 * t9 * (t170 * t390 / 0.2D1 - t173 * t388 / 0.6D1 + t412 - t165
     # * t392) + t182 * t9 * t388 - 0.180D3 * t65 * t9 * (-t165 * t390 +
     # t392 + t170 * t388 / 0.2D1)) * t158 / 0.1440D4 + (-0.90D2 * t99 *
     # t8 * (t201 * t388 + (t207 * t388 - t390) * t117 - t405) - 0.180D3
     # * t139 * t9 * t499) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8
     # * (t233 * t390 - t393 - t230 * t388 / 0.2D1) + 0.180D3 * t139 * t
     #9 * (t233 * t388 - t405) - t245 * t421) * t221 * t158 / 0.720D3
      t565 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t564)
      t567 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t569 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t572 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t575 = t3 * t567
      t582 = t3 * t572
      t592 = t117 * t569 + t3 * t569
      t607 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t653 = t246 * t569
      t697 = rrqg2qgh64J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t710 = t572 * 0.3141592653589793D1
      t713 = t567 * 0.3141592653589793D1
      t715 = t569 * 0.3141592653589793D1
      t743 = (0.90D2 * t99 * t8 * (t117 * t567 + t132 * t569 / 0.2D1 - t
     #128 * t572 - t118 * t572 + t575 + t121 * t569 / 0.2D1) - 0.180D3 *
     # t139 * t9 * (t582 - t128 * t569 + t117 * t572 - t118 * t569) + t1
     #49 * t9 * t592) * t156 * t158 / 0.1440D4 + (t161 * t9 * (-t165 * t
     #569 + t572) + 0.90D2 * t4 * t9 * (t170 * t572 / 0.2D1 - t173 * t56
     #9 / 0.6D1 + t607 - t165 * t567) + t182 * t9 * t569 - 0.180D3 * t65
     # * t9 * (t567 - t165 * t572 + t170 * t569 / 0.2D1)) * t158 / 0.144
     #0D4 + (-0.90D2 * t99 * t8 * ((t207 * t569 - t572) * t117 + t201 * 
     #t569 - t582) - 0.180D3 * t139 * t9 * t592) * t156 * t222 / 0.720D3
     # + (-0.90D2 * t99 * t8 * (-t575 + t233 * t572 - t230 * t569 / 0.2D
     #1) + 0.180D3 * t139 * t9 * (-t582 + t233 * t569) - t245 * t653) * 
     #t221 * t158 / 0.720D3 + t310 * ((0.90D2 * t567 + t319 * t572 + t32
     #5 * t569) * t117 - t342 * t572 - t338 * t569 + 0.90D2 * t575) * t1
     #56 * t221 / 0.1440D4 + (t244 * t9 * (-t582 + t353 * t569) - 0.90D2
     # * t99 * t8 * (-t359 * t572 / 0.2D1 - t3 * t607 + t364 * t569 / 0.
     #6D1 + t353 * t567) - t373 * t653 + 0.180D3 * t139 * t9 * (-t575 + 
     #t353 * t572 - t359 * t569 / 0.2D1)) * t221 / 0.1440D4 + t4 * t9 * 
     #t697 / 0.32D2 - t49 * t8 * t572 / 0.2880D4 - t60 * t8 * t567 / 0.2
     #880D4 - t70 * t8 * t607 / 0.2880D4 + ((-0.180D3 * t710 * lh + 0.90
     #D2 * t713 - t715 * t45) * t5 * t272 + 0.90D2 * t715 * t282 + (-0.1
     #80D3 * t713 * lh - t715 * t36 + 0.90D2 * t607 * 0.3141592653589793
     #D1 - t710 * t45) * t5 * t294 + (-0.180D3 * t715 * lh + 0.90D2 * t7
     #10) * t5 * t305) * t156 / 0.2880D4 - t94 * t8 * t569 / 0.2880D4
      t744 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t743)
      t746 = t2 * x1
      t747 = -0.1D1 + x1
      t748 = x1 * z
      t749 = 0.1D1 - x1 + t748
      t750 = 0.1D1 / t749
      t752 = t2 * t747 * t750
      t753 = s * t20
      t755 = x1 * t747 * t750
      t756 = t753 * t755
      t757 = -t747
      t758 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.10
     #D1, x4)
      t759 = t3 * t758
      t760 = t747 ** 2
      t761 = t750 * t760
      t762 = t103 * t761
      t765 = log(0.4D1 * t102 * t762)
      t766 = t765 ** 2
      t767 = t766 * t3
      t768 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.10
     #D1, x4)
      t772 = t21 * t750
      t777 = log(-0.4D1 * t101 * t19 * t772 * t760 * t105)
      t778 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.10
     #D1, x4)
      t780 = t777 ** 2
      t785 = x3 * t749
      t787 = Sqrt(-t785 * t104)
      t791 = x3 * x1
      t792 = t791 * z
      t793 = 0.3D1 * t792
      t794 = x1 * t14
      t795 = x3 * t14
      t796 = t795 * x1
      t798 = 0.2D1 * t101 * z
      t799 = t101 * t14
      t800 = 0.2D1 * t791
      t801 = -z + 0.2D1 * t110 * t787 * z + t748 - t793 - t794 + t796 + 
     #t798 - t799 - x3 + t800 - t101
      t802 = 0.1D1 / t801
      t804 = t765 * t3
      t810 = t3 * t778
      t823 = -t3 * t768 - t749 * t768 * t802
      t830 = t162 * t15
      t831 = t772 * t760
      t834 = log(0.4D1 * t830 * t831)
      t839 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.10
     #D1, x4)
      t840 = t834 ** 2
      t843 = t840 * t834
      t864 = t196 * t162
      t869 = log(-0.4D1 * t864 * t103 * t761 * t105)
      t870 = t869 * t749
      t876 = log(0.4D1 * t864 * t762)
      t877 = t876 * t3
      t891 = t225 * t18
      t894 = log(0.4D1 * t891 * t762)
      t895 = t894 * t3
      t897 = t894 ** 2
      t898 = t897 * t3
      t916 = (0.90D2 * t99 * t8 * (-t759 - t767 * t768 / 0.2D1 + (t777 *
     # t778 - t758 - t780 * t768 / 0.2D1) * t749 * t802 + t804 * t778) -
     # 0.180D3 * t139 * t9 * (-t810 + t804 * t768 + (-t778 + t777 * t768
     #) * t749 * t802) + t149 * t9 * t823) * t156 * t158 / 0.1440D4 + (t
     #161 * t9 * (-t778 + t834 * t768) + 0.90D2 * t4 * t9 * (-t839 - t84
     #0 * t778 / 0.2D1 + t843 * t768 / 0.6D1 + t834 * t758) - t182 * t9 
     #* t768 - 0.180D3 * t65 * t9 * (-t840 * t768 / 0.2D1 + t834 * t778 
     #- t758)) * t158 / 0.1440D4 + (-0.90D2 * t99 * t8 * (-(-t749 * t778
     # + t870 * t768) * t802 - t877 * t768 + t810) - 0.180D3 * t139 * t9
     # * t823) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (t759 - t
     #895 * t778 + t898 * t768 / 0.2D1) + 0.180D3 * t139 * t9 * (-t895 *
     # t768 + t810) + t245 * t246 * t768) * t221 * t158 / 0.720D3
      t917 = FJET(XB1, XB2, s, 0.0D0, t746, -t752, 0.0D0, -t756, t916)
      t919 = x2 * s
      t920 = t919 * t1
      t921 = -0.1D1 + x2
      t922 = t921 * s
      t923 = t922 * t1
      t924 = x2 * z
      t926 = 0.1D1 / (-z + t924 - x2)
      t927 = -t921
      t928 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.10
     #D1, x4)
      t929 = t926 * t928
      t931 = t103 * t921
      t934 = log(-0.4D1 * t314 * t931)
      t937 = t934 ** 2
      t940 = (0.180D3 * t934 * lh + 0.45D2 * t937 + t43 - t44) * t926
      t941 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.10
     #D1, x4)
      t945 = (-t313 - 0.90D2 * t934) * t926
      t946 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.10
     #D1, x4)
      t955 = log(-0.4D1 * t349 * t931)
      t956 = t955 * t926
      t958 = t926 * t946
      t962 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.10
     #D1, x4)
      t964 = t955 ** 2
      t965 = t964 * t926
      t970 = t964 * t955 * t926
      t977 = t8 * t926
      t978 = t977 * t941
      t990 = t21 * t921
      t994 = log(-0.4D1 * t197 * t19 * t990)
      t995 = t994 * t926
      t1001 = t139 * t5
      t1010 = log(-0.4D1 * t891 * t931)
      t1011 = t1010 ** 2
      t1012 = t1011 * t926
      t1015 = t1010 * t926
      t1031 = t310 * (0.90D2 * t929 + t940 * t941 + t945 * t946) * t156 
     #* t221 / 0.1440D4 + (t244 * t9 * (t956 * t941 - t958) - 0.90D2 * t
     #99 * t8 * (-t926 * t962 - t965 * t946 / 0.2D1 + t956 * t928 + t970
     # * t941 / 0.6D1) - t373 * t978 + 0.180D3 * t139 * t9 * (-t929 - t9
     #65 * t941 / 0.2D1 + t956 * t946)) * t221 / 0.1440D4 + (-0.90D2 * t
     #99 * t8 * (-t958 + t995 * t941) - 0.180D3 * t1001 * t978) * t156 *
     # t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (-t929 - t1012 * t941 / 0.
     #2D1 + t1015 * t946) + 0.180D3 * t139 * t9 * (t1015 * t941 - t958) 
     #- t245 * t978) * t221 * t158 / 0.720D3
      t1032 = FJET(XB1, XB2, s, 0.0D0, t920, 0.0D0, -t923, 0.0D0, t1031)
      t1034 = x2 * x3
      t1037 = Sqrt(x3 * t921 * t104)
      t1038 = t110 * t1037
      t1040 = 0.2D1 * t1038 * x2
      t1042 = 0.1D1 - x3 + t1034
      t1043 = 0.1D1 / t1042
      t1045 = t2 * (0.1D1 - x3 - x2 + t1034 + t196 + t1040) * t1043
      t1050 = t2 * x2 * (-0.1D1 + t1034 + 0.2D1 * t1038) * t1043
      t1051 = t1034 * z
      t1052 = t196 * z
      t1058 = 0.1D1 / (z - t924 + x2 + x3 - t196 - t1051 + t1052 - t1040
     # - 0.2D1 * t1038 * z + 0.2D1 * t1038 * t924)
      t1059 = t104 * t1043
      t1060 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1062 = t921 * t104
      t1063 = t1042 ** 2
      t1064 = 0.1D1 / t1063
      t1065 = t1062 * t1064
      t1069 = log(0.4D1 * t864 * t103 * t1065)
      t1070 = t1069 * t1058
      t1071 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1077 = t8 * t1058
      t1090 = log(0.4D1 * t196 * t19 * t990 * t104 * t1064)
      t1093 = t1090 ** 2
      t1096 = (0.180D3 * t1090 * lh + 0.45D2 * t1093 + t43 - t44) * t105
     #8
      t1100 = (-t313 - 0.90D2 * t1090) * t1058
      t1102 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1110 = (-0.90D2 * t99 * t8 * (-t1058 * t1060 + t1070 * t1071) - 0
     #.180D3 * t1001 * t1077 * t1071) * t156 * t222 / 0.720D3 + t310 * (
     #t1096 * t1071 + t1100 * t1060 + 0.90D2 * t1058 * t1102) * t156 * t
     #221 / 0.1440D4
      t1111 = FJET(XB1, XB2, s, 0.0D0, t1045, 0.0D0, -t1050, 0.0D0, t111
     #0)
      t1113 = t1 * t747
      t1115 = t922 * t1113 * t750
      t1116 = t919 * t1113
      t1118 = t753 * t921 * t755
      t1123 = log(-0.4D1 * t864 * t103 * t761 * t921)
      t1124 = x2 * x1
      t1125 = t1124 * z
      t1127 = 0.1D1 / (z - t924 - t1124 + t1125 + x2)
      t1128 = t1123 * t1127
      t1129 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1131 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1132 = t1127 * t1131
      t1137 = t8 * t1127
      t1138 = t1137 * t1129
      t1149 = log(-0.4D1 * t225 * t19 * t772 * t760 * t921)
      t1150 = t1149 * t1127
      t1152 = t1149 ** 2
      t1153 = t1152 * t1127
      t1156 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1172 = (-0.90D2 * t99 * t8 * (t1128 * t1129 - t1132) - 0.180D3 * 
     #t1001 * t1138) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (t1
     #150 * t1131 - t1153 * t1129 / 0.2D1 - t1127 * t1156) + 0.180D3 * t
     #139 * t9 * (t1150 * t1129 - t1132) - t245 * t1138) * t221 * t158 /
     # 0.720D3
      t1173 = FJET(XB1, XB2, s, 0.0D0, t1115, t746, -t1116, t1118, t1172
     #)
      t1175 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1176 = t926 * t1175
      t1177 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1183 = t977 * t1177
      t1190 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1191 = t926 * t1190
      t1224 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1243 = (-0.90D2 * t99 * t8 * (-t1176 + t995 * t1177) - 0.180D3 * 
     #t1001 * t1183) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (-t
     #1191 + t1015 * t1175 - t1012 * t1177 / 0.2D1) + 0.180D3 * t139 * t
     #9 * (-t1176 + t1015 * t1177) - t245 * t1183) * t221 * t158 / 0.720
     #D3 + t310 * (0.90D2 * t1191 + t945 * t1175 + t940 * t1177) * t156 
     #* t221 / 0.1440D4 + (t244 * t9 * (t956 * t1177 - t1176) - 0.90D2 *
     # t99 * t8 * (t956 * t1190 + t970 * t1177 / 0.6D1 - t926 * t1224 - 
     #t965 * t1175 / 0.2D1) - t373 * t1183 + 0.180D3 * t139 * t9 * (-t11
     #91 - t965 * t1177 / 0.2D1 + t956 * t1175)) * t221 / 0.1440D4
      t1244 = FJET(XB1, XB2, s, 0.0D0, -t923, 0.0D0, t920, 0.0D0, t1243)
      t1246 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1248 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1251 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1256 = t3 * t1251
      t1263 = t3 * t1246
      t1276 = -t3 * t1248 - t749 * t1248 * t802
      t1287 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1344 = (0.90D2 * t99 * t8 * (t804 * t1246 + (-t780 * t1248 / 0.2D
     #1 - t1251 + t777 * t1246) * t749 * t802 - t1256 - t767 * t1248 / 0
     #.2D1) - 0.180D3 * t139 * t9 * (-t1263 + t804 * t1248 + (-t1246 + t
     #777 * t1248) * t749 * t802) + t149 * t9 * t1276) * t156 * t158 / 0
     #.1440D4 + (t161 * t9 * (-t1246 + t834 * t1248) + 0.90D2 * t4 * t9 
     #* (-t1287 - t840 * t1246 / 0.2D1 + t834 * t1251 + t843 * t1248 / 0
     #.6D1) - t182 * t9 * t1248 - 0.180D3 * t65 * t9 * (-t1251 - t840 * 
     #t1248 / 0.2D1 + t834 * t1246)) * t158 / 0.1440D4 + (-0.90D2 * t99 
     #* t8 * (-t877 * t1248 + t1263 - (-t749 * t1246 + t870 * t1248) * t
     #802) - 0.180D3 * t139 * t9 * t1276) * t156 * t222 / 0.720D3 + (-0.
     #90D2 * t99 * t8 * (-t895 * t1246 + t1256 + t898 * t1248 / 0.2D1) +
     # 0.180D3 * t139 * t9 * (-t895 * t1248 + t1263) + t245 * t246 * t12
     #48) * t221 * t158 / 0.720D3
      t1345 = FJET(XB1, XB2, s, 0.0D0, -t752, t746, 0.0D0, -t756, t1344)
      t1347 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1349 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1351 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1372 = t310 * (t1100 * t1347 + t1096 * t1349 + 0.90D2 * t1058 * t
     #1351) * t156 * t221 / 0.1440D4 + (-0.90D2 * t99 * t8 * (-t1058 * t
     #1347 + t1070 * t1349) - 0.180D3 * t1001 * t1077 * t1349) * t156 * 
     #t222 / 0.720D3
      t1373 = FJET(XB1, XB2, s, 0.0D0, -t1050, 0.0D0, t1045, 0.0D0, t137
     #2)
      t1375 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1376 = t1375 * 0.3141592653589793D1
      t1379 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1380 = t1379 * 0.3141592653589793D1
      t1382 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1383 = t1382 * 0.3141592653589793D1
      t1393 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1414 = t3 * t1379
      t1427 = t3 * t1375
      t1434 = t117 * t1382 + t3 * t1382
      t1494 = t246 * t1382
      t1544 = rrqg2qgh62J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1551 = ((-0.180D3 * t1376 * lh + 0.90D2 * t1380 - t1383 * t45) * 
     #t5 * t272 + 0.90D2 * t1383 * t282 + (-0.180D3 * t1380 * lh - t1383
     # * t36 + 0.90D2 * t1393 * 0.3141592653589793D1 - t1376 * t45) * t5
     # * t294 + (-0.180D3 * t1383 * lh + 0.90D2 * t1376) * t5 * t305) * 
     #t156 / 0.2880D4 - t94 * t8 * t1382 / 0.2880D4 + (0.90D2 * t99 * t8
     # * (-t128 * t1375 + t117 * t1379 + t1414 + t132 * t1382 / 0.2D1 + 
     #t121 * t1382 / 0.2D1 - t118 * t1375) - 0.180D3 * t139 * t9 * (-t12
     #8 * t1382 + t117 * t1375 - t118 * t1382 + t1427) + t149 * t9 * t14
     #34) * t156 * t158 / 0.1440D4 + (t161 * t9 * (t1375 - t165 * t1382)
     # + 0.90D2 * t4 * t9 * (t1393 - t165 * t1379 - t173 * t1382 / 0.6D1
     # + t170 * t1375 / 0.2D1) + t182 * t9 * t1382 - 0.180D3 * t65 * t9 
     #* (-t165 * t1375 + t1379 + t170 * t1382 / 0.2D1)) * t158 / 0.1440D
     #4 + (-0.90D2 * t99 * t8 * ((t207 * t1382 - t1375) * t117 - t1427 +
     # t201 * t1382) - 0.180D3 * t139 * t9 * t1434) * t156 * t222 / 0.72
     #0D3 + (-0.90D2 * t99 * t8 * (-t1414 - t230 * t1382 / 0.2D1 + t233 
     #* t1375) + 0.180D3 * t139 * t9 * (-t1427 + t233 * t1382) - t245 * 
     #t1494) * t221 * t158 / 0.720D3 - t49 * t8 * t1375 / 0.2880D4 - t60
     # * t8 * t1379 / 0.2880D4 + t310 * ((0.90D2 * t1379 + t319 * t1375 
     #+ t325 * t1382) * t117 + 0.90D2 * t1414 - t338 * t1382 - t342 * t1
     #375) * t156 * t221 / 0.1440D4 + (t244 * t9 * (-t1427 + t353 * t138
     #2) - 0.90D2 * t99 * t8 * (t353 * t1379 - t359 * t1375 / 0.2D1 - t3
     # * t1393 + t364 * t1382 / 0.6D1) - t373 * t1494 + 0.180D3 * t139 *
     # t9 * (-t1414 + t353 * t1375 - t359 * t1382 / 0.2D1)) * t221 / 0.1
     #440D4 + t4 * t9 * t1544 / 0.32D2 - t70 * t8 * t1393 / 0.2880D4
      t1552 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1551)
      t1554 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1556 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1559 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1564 = t3 * t1559
      t1571 = t3 * t1554
      t1584 = -t3 * t1556 - t749 * t1556 * t802
      t1599 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t1652 = (0.90D2 * t99 * t8 * ((t777 * t1554 - t780 * t1556 / 0.2D1
     # - t1559) * t749 * t802 + t804 * t1554 - t1564 - t767 * t1556 / 0.
     #2D1) - 0.180D3 * t139 * t9 * (-t1571 + t804 * t1556 + (-t1554 + t7
     #77 * t1556) * t749 * t802) + t149 * t9 * t1584) * t156 * t158 / 0.
     #1440D4 + (t161 * t9 * (-t1554 + t834 * t1556) + 0.90D2 * t4 * t9 *
     # (-t840 * t1554 / 0.2D1 + t843 * t1556 / 0.6D1 - t1599 + t834 * t1
     #559) - t182 * t9 * t1556 - 0.180D3 * t65 * t9 * (-t840 * t1556 / 0
     #.2D1 - t1559 + t834 * t1554)) * t158 / 0.1440D4 + (-0.90D2 * t99 *
     # t8 * (t1571 - t877 * t1556 - (-t749 * t1554 + t870 * t1556) * t80
     #2) - 0.180D3 * t139 * t9 * t1584) * t156 * t222 / 0.720D3 + (-0.90
     #D2 * t99 * t8 * (-t895 * t1554 + t1564 + t898 * t1556 / 0.2D1) + 0
     #.180D3 * t139 * t9 * (-t895 * t1556 + t1571) + t245 * t246 * t1556
     #) * t221 * t158 / 0.720D3
      t1653 = FJET(XB1, XB2, s, t746, 0.0D0, 0.0D0, -t752, -t756, t1652)
      t1655 = t386 * t385 + t565 * t564 + t744 * t743 + t917 * t916 + t1
     #032 * t1031 + t1111 * t1110 + t1173 * t1172 + t1244 * t1243 + t134
     #5 * t1344 + t1373 * t1372 + t1552 * t1551 + t1653 * t1652
      t1656 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1657 = t1127 * t1656
      t1658 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1664 = t1137 * t1658
      t1671 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1689 = (-0.90D2 * t99 * t8 * (-t1657 + t1128 * t1658) - 0.180D3 *
     # t1001 * t1664) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (t
     #1150 * t1656 - t1127 * t1671 - t1153 * t1658 / 0.2D1) + 0.180D3 * 
     #t139 * t9 * (t1150 * t1658 - t1657) - t245 * t1664) * t221 * t158 
     #/ 0.720D3
      t1690 = FJET(XB1, XB2, s, t746, -t1116, 0.0D0, t1115, t1118, t1689
     #)
      t1692 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1694 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1695 = t926 * t1694
      t1700 = t977 * t1692
      t1710 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1711 = t926 * t1710
      t1738 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1760 = (-0.90D2 * t99 * t8 * (t995 * t1692 - t1695) - 0.180D3 * t
     #1001 * t1700) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (t10
     #15 * t1694 - t1012 * t1692 / 0.2D1 - t1711) + 0.180D3 * t139 * t9 
     #* (t1015 * t1692 - t1695) - t245 * t1700) * t221 * t158 / 0.720D3 
     #+ t310 * (0.90D2 * t1711 + t940 * t1692 + t945 * t1694) * t156 * t
     #221 / 0.1440D4 + (t244 * t9 * (-t1695 + t956 * t1692) - 0.90D2 * t
     #99 * t8 * (-t926 * t1738 + t956 * t1710 + t970 * t1692 / 0.6D1 - t
     #965 * t1694 / 0.2D1) - t373 * t1700 + 0.180D3 * t139 * t9 * (-t171
     #1 - t965 * t1692 / 0.2D1 + t956 * t1694)) * t221 / 0.1440D4
      t1761 = FJET(XB1, XB2, s, t920, 0.0D0, -t923, 0.0D0, 0.0D0, t1760)
      t1763 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1765 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1779 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t1788 = (-0.90D2 * t99 * t8 * (t1070 * t1763 - t1058 * t1765) - 0.
     #180D3 * t1001 * t1077 * t1763) * t156 * t222 / 0.720D3 + t310 * (t
     #1100 * t1765 + 0.90D2 * t1058 * t1779 + t1096 * t1763) * t156 * t2
     #21 / 0.1440D4
      t1789 = FJET(XB1, XB2, s, t1045, 0.0D0, -t1050, 0.0D0, 0.0D0, t178
     #8)
      t1791 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1793 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1794 = t1127 * t1793
      t1799 = t1137 * t1791
      t1805 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t1824 = (-0.90D2 * t99 * t8 * (t1128 * t1791 - t1794) - 0.180D3 * 
     #t1001 * t1799) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (-t
     #1127 * t1805 - t1153 * t1791 / 0.2D1 + t1150 * t1793) + 0.180D3 * 
     #t139 * t9 * (-t1794 + t1150 * t1791) - t245 * t1799) * t221 * t158
     # / 0.720D3
      t1825 = FJET(XB1, XB2, s, t1115, 0.0D0, -t1116, t746, t1118, t1824
     #)
      t1828 = t746 * t1034 * t1043
      t1829 = t2 * t747
      t1830 = t196 * x1
      t1832 = Sqrt(t785 * t1062)
      t1833 = t110 * t1832
      t1835 = 0.2D1 * t1833 * x2
      t1836 = t196 * t748
      t1840 = t1829 * (-t1830 - x2 + t1034 + t1835 + t1836 + 0.1D1 - x3 
     #+ t196) * t750 * t1043
      t1844 = t104 * s * t1 * x1 * t1043
      t1850 = t1829 * x2 * (-0.1D1 + t1034 + x1 - t791 - t748 + t792 + 0
     #.2D1 * t1833) * t750 * t1043
      t1854 = x2 * t100
      t1862 = -t1034 * x1 + t101 * x2 + t1124 * t14 + 0.2D1 * t1854 * z 
     #- t1854 * t14 + 0.2D1 * t1833 * z - z + t748 - t794 + t800 - t101 
     #+ 0.2D1 * t1833 * t1125 - x3 + t924 + t196 - t1854 - t1830
      t1876 = t1835 - x2 + t1836 - 0.2D1 * t1833 * t924 - 0.2D1 * t1833 
     #* t1124 + 0.2D1 * t1034 * t748 - t795 * t1124 - 0.2D1 * t101 * t92
     #4 + t101 * t14 * x2 + 0.2D1 * t1124 - t793 + t796 + t798 - t799 + 
     #t1051 - t1052 - 0.3D1 * t1125
      t1878 = 0.1D1 / (t1862 + t1876)
      t1879 = t749 * t1878
      t1880 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t1886 = log(0.4D1 * t196 * t830 * t831 * t1065)
      t1887 = t1886 * t749
      t1888 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t1889 = t1878 * t1888
      t1895 = t8 * t749
      t1899 = -0.90D2 * t99 * t8 * (-t1879 * t1880 + t1887 * t1889) - 0.
     #180D3 * t1001 * t1895 * t1889
      t1903 = FJET(XB1, XB2, s, t1828, -t1840, -t1844, t1850, t1118, t18
     #99 * t156 * t222 / 0.720D3)
      t1906 = t156 * t221 * t158
      t1909 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t1911 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t1912 = t1878 * t1911
      t1921 = -0.90D2 * t99 * t8 * (-t1879 * t1909 + t1887 * t1912) - 0.
     #180D3 * t1001 * t1895 * t1912
      t1925 = FJET(XB1, XB2, s, t1850, -t1844, -t1840, t1828, t1118, t19
     #21 * t156 * t222 / 0.720D3)
      t1929 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1930 = t926 * t1929
      t1931 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1937 = t977 * t1931
      t1946 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1947 = t926 * t1946
      t1975 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, 0.1
     #0D1, x4)
      t1997 = (-0.90D2 * t99 * t8 * (-t1930 + t995 * t1931) - 0.180D3 * 
     #t1001 * t1937) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (-t
     #1012 * t1931 / 0.2D1 - t1947 + t1015 * t1929) + 0.180D3 * t139 * t
     #9 * (t1015 * t1931 - t1930) - t245 * t1937) * t221 * t158 / 0.720D
     #3 + t310 * (t940 * t1931 + t945 * t1929 + 0.90D2 * t1947) * t156 *
     # t221 / 0.1440D4 + (t244 * t9 * (-t1930 + t956 * t1931) - 0.90D2 *
     # t99 * t8 * (-t926 * t1975 + t956 * t1946 + t970 * t1931 / 0.6D1 -
     # t965 * t1929 / 0.2D1) - t373 * t1937 + 0.180D3 * t139 * t9 * (-t1
     #947 - t965 * t1931 / 0.2D1 + t956 * t1929)) * t221 / 0.1440D4
      t1998 = FJET(XB1, XB2, s, -t923, 0.0D0, t920, 0.0D0, 0.0D0, t1997)
      t2000 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t2001 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t2003 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t2010 = t3 * t2000
      t2017 = t3 * t2001
      t2030 = -t3 * t2003 - t749 * t2003 * t802
      t2043 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, t757, 0.10D1, 0.1
     #0D1, x4)
      t2098 = (0.90D2 * t99 * t8 * ((-t2000 + t777 * t2001 - t780 * t200
     #3 / 0.2D1) * t749 * t802 + t804 * t2001 - t2010 - t767 * t2003 / 0
     #.2D1) - 0.180D3 * t139 * t9 * (-t2017 + t804 * t2003 + (-t2001 + t
     #777 * t2003) * t749 * t802) + t149 * t9 * t2030) * t156 * t158 / 0
     #.1440D4 + (t161 * t9 * (t834 * t2003 - t2001) + 0.90D2 * t4 * t9 *
     # (-t840 * t2001 / 0.2D1 - t2043 + t843 * t2003 / 0.6D1 + t834 * t2
     #000) - t182 * t9 * t2003 - 0.180D3 * t65 * t9 * (-t2000 - t840 * t
     #2003 / 0.2D1 + t834 * t2001)) * t158 / 0.1440D4 + (-0.90D2 * t99 *
     # t8 * (t2017 - (-t749 * t2001 + t870 * t2003) * t802 - t877 * t200
     #3) - 0.180D3 * t139 * t9 * t2030) * t156 * t222 / 0.720D3 + (-0.90
     #D2 * t99 * t8 * (t898 * t2003 / 0.2D1 - t895 * t2001 + t2010) + 0.
     #180D3 * t139 * t9 * (-t895 * t2003 + t2017) + t245 * t246 * t2003)
     # * t221 * t158 / 0.720D3
      t2099 = FJET(XB1, XB2, s, -t752, 0.0D0, 0.0D0, t746, -t756, t2098)
      t2101 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t2103 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t2104 = t1127 * t2103
      t2109 = t1137 * t2101
      t2115 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, t757, t927, 0.10D
     #1, x4)
      t2134 = (-0.90D2 * t99 * t8 * (t1128 * t2101 - t2104) - 0.180D3 * 
     #t1001 * t2109) * t156 * t222 / 0.720D3 + (-0.90D2 * t99 * t8 * (-t
     #1127 * t2115 + t1150 * t2103 - t1153 * t2101 / 0.2D1) + 0.180D3 * 
     #t139 * t9 * (-t2104 + t1150 * t2101) - t245 * t2109) * t221 * t158
     # / 0.720D3
      t2135 = FJET(XB1, XB2, s, -t1116, t746, t1115, 0.0D0, t1118, t2134
     #)
      t2137 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t2139 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t2153 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t927, -t1
     #059, x4)
      t2162 = (-0.90D2 * t99 * t8 * (t1070 * t2137 - t1058 * t2139) - 0.
     #180D3 * t1001 * t1077 * t2137) * t156 * t222 / 0.720D3 + t310 * (t
     #1100 * t2139 + 0.90D2 * t1058 * t2153 + t1096 * t2137) * t156 * t2
     #21 / 0.1440D4
      t2163 = FJET(XB1, XB2, s, -t1050, 0.0D0, t1045, 0.0D0, 0.0D0, t216
     #2)
      t2165 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t2167 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t2168 = t1878 * t2167
      t2177 = -0.90D2 * t99 * t8 * (-t1879 * t2165 + t1887 * t2168) - 0.
     #180D3 * t1001 * t1895 * t2168
      t2181 = FJET(XB1, XB2, s, -t1844, t1850, t1828, -t1840, t1118, t21
     #77 * t156 * t222 / 0.720D3)
      t2185 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t2187 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t757, t927, -t105
     #9, x4)
      t2188 = t1878 * t2187
      t2197 = -0.90D2 * t99 * t8 * (-t1879 * t2185 + t1887 * t2188) - 0.
     #180D3 * t1001 * t1895 * t2188
      t2201 = FJET(XB1, XB2, s, -t1840, t1828, t1850, -t1844, t1118, t21
     #97 * t156 * t222 / 0.720D3)
      t2205 = t1690 * t1689 + t1761 * t1760 + t1789 * t1788 + t1825 * t1
     #824 + t1903 * t1899 * t1906 / 0.720D3 + t1925 * t1921 * t1906 / 0.
     #720D3 + t1998 * t1997 + t2099 * t2098 + t2135 * t2134 + t2163 * t2
     #162 + t2181 * t2177 * t1906 / 0.720D3 + t2201 * t2197 * t1906 / 0.
     #720D3
      rrqg2qght6s1e1 = t1655 + t2205

      end function



      doubleprecision function rrqg2qght6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t20 = 0.1D1 / t19
      t21 = t18 * t20
      t24 = log(-0.4D1 * t13 * t21)
      t25 = t24 ** 2
      t26 = cos(t10)
      t28 = Sqrt(-x3 * t19)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t28 * z)
      t37 = log(0.4D1 * t13 * t18)
      t38 = t37 ** 2
      t39 = 0.1D1 / z
      t43 = t9 * (t25 * t33 / 0.2D1 + t38 * t39 / 0.2D1)
      t48 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t49 = t48 * 0.3141592653589793D1
      t56 = t8 * (-t24 * t33 - t37 * t39)
      t60 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t63 = lh ** 2
      t65 = 0.3141592653589793D1 ** 2
      t67 = -0.180D3 * t63 + 0.30D2 * t65
      t72 = t8 * (t33 + t39)
      t75 = 0.1D1 / x3
      t78 = t15 * t12
      t79 = t78 * t17
      t81 = log(0.4D1 * t79)
      t82 = t81 * 0.3141592653589793D1
      t83 = t39 * lh
      t86 = t81 ** 2
      t87 = t86 * 0.3141592653589793D1
      t90 = 0.3141592653589793D1 * t39
      t93 = (-0.180D3 * t82 * t83 - 0.45D2 * t87 * t39 + t90 * t67) * t5
      t97 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t116 = (0.90D2 * t87 * t83 + t90 * (-0.60D2 * lh * t65 + 0.2884936
     #567583026D3 + 0.120D3 * t63 * lh) + 0.15D2 * t86 * t81 * 0.3141592
     #653589793D1 * t39 - t82 * t39 * t67) * t5
      t120 = t90 * lh
      t125 = (0.180D3 * t120 + 0.90D2 * t82 * t39) * t5
      t129 = 0.3141592653589793D1 * t5
      t130 = t129 * t8
      t132 = 0.180D3 * lh
      t133 = x2 ** 2
      t134 = x3 * t133
      t135 = t134 * t12
      t138 = log(-0.4D1 * t135 * t21)
      t140 = -t132 - 0.90D2 * t138
      t144 = t39 * t48
      t149 = log(0.4D1 * t134 * t79)
      t152 = 0.180D3 * t83 + 0.90D2 * t149 * t39
      t156 = 0.1D1 / x2
      t161 = t133 * t12
      t164 = log(0.4D1 * t161 * t18)
      t165 = t164 * t39
      t167 = t164 ** 2
      t168 = t167 * t39
      t175 = 0.3141592653589793D1 * lh
      t182 = 0.3141592653589793D1 * t67 * t5
      t183 = t8 * t39
      t184 = t183 * t3
      t189 = x1 ** 2
      t190 = x3 * t189
      t191 = t190 * t12
      t194 = log(-0.4D1 * t191 * t21)
      t195 = t194 * t33
      t200 = log(0.4D1 * t190 * t79)
      t201 = t200 * t39
      t209 = t33 * t3 + t39 * t3
      t215 = 0.1D1 / x1
      t220 = t156 * t215
      t224 = t133 * t189
      t227 = log(0.4D1 * t224 * t79)
      t228 = t227 * t39
      t234 = t175 * t5
      t241 = t189 * t12
      t244 = log(0.4D1 * t241 * t18)
      t245 = t244 ** 2
      t259 = -t90 * t67
      t265 = (0.90D2 * t4 * t43 + (-0.180D3 * t4 * lh + 0.90D2 * t49) * 
     #t5 * t56 + (-0.180D3 * t49 * lh + 0.90D2 * t60 * 0.314159265358979
     #3D1 - t4 * t67) * t5 * t72) * t75 / 0.2880D4 - t93 * t8 * t48 / 0.
     #2880D4 + t90 * t9 * t97 / 0.32D2 - t116 * t8 * t3 / 0.2880D4 - t12
     #5 * t8 * t60 / 0.2880D4 + t130 * ((0.90D2 * t48 + t140 * t3) * t33
     # + 0.90D2 * t144 - t152 * t3) * t75 * t156 / 0.1440D4 + (-0.90D2 *
     # t129 * t8 * (-t39 * t60 + t165 * t48 - t168 * t3 / 0.2D1) + 0.180
     #D3 * t175 * t9 * (-t144 + t165 * t3) - t182 * t184) * t156 / 0.144
     #0D4 + (0.90D2 * t129 * t8 * (-t195 * t3 + t144 + t33 * t48 - t201 
     #* t3) - 0.180D3 * t175 * t9 * t209) * t75 * t215 / 0.1440D4 + t130
     # * t209 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * t8 * (-t144 + t22
     #8 * t3) - 0.180D3 * t234 * t184) * t156 * t215 / 0.720D3 + (0.90D2
     # * t90 * t9 * (t60 + t245 * t3 / 0.2D1 - t244 * t48) - 0.180D3 * t
     #120 * t9 * (t48 - t244 * t3) + t259 * t9 * t3) * t215 / 0.1440D4
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t265)
      t268 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t269 = t268 * 0.3141592653589793D1
      t274 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t275 = t274 * 0.3141592653589793D1
      t282 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t295 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t306 = t39 * t274
      t330 = t183 * t268
      t344 = t33 * t268 + t39 * t268
      t385 = (0.90D2 * t269 * t43 + (-0.180D3 * t269 * lh + 0.90D2 * t27
     #5) * t5 * t56 + (-0.180D3 * t275 * lh + 0.90D2 * t282 * 0.31415926
     #53589793D1 - t269 * t67) * t5 * t72) * t75 / 0.2880D4 - t93 * t8 *
     # t274 / 0.2880D4 + t90 * t9 * t295 / 0.32D2 - t116 * t8 * t268 / 0
     #.2880D4 - t125 * t8 * t282 / 0.2880D4 + t130 * (-t152 * t268 + 0.9
     #0D2 * t306 + (t140 * t268 + 0.90D2 * t274) * t33) * t75 * t156 / 0
     #.1440D4 + (-0.90D2 * t129 * t8 * (-t39 * t282 + t165 * t274 - t168
     # * t268 / 0.2D1) + 0.180D3 * t175 * t9 * (-t306 + t165 * t268) - t
     #182 * t330) * t156 / 0.1440D4 + (0.90D2 * t129 * t8 * (t33 * t274 
     #- t195 * t268 - t201 * t268 + t306) - 0.180D3 * t175 * t9 * t344) 
     #* t75 * t215 / 0.1440D4 + t130 * t344 * t75 * t220 / 0.8D1 + (-0.9
     #0D2 * t129 * t8 * (t228 * t268 - t306) - 0.180D3 * t234 * t330) * 
     #t156 * t215 / 0.720D3 + (0.90D2 * t90 * t9 * (-t244 * t274 + t282 
     #+ t245 * t268 / 0.2D1) - 0.180D3 * t120 * t9 * (t274 - t244 * t268
     #) + t259 * t9 * t268) * t215 / 0.1440D4
      t386 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t385)
      t388 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t389 = t388 * 0.3141592653589793D1
      t394 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t395 = t394 * 0.3141592653589793D1
      t402 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t415 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t429 = t39 * t394
      t450 = t183 * t388
      t464 = t33 * t388 + t39 * t388
      t505 = (0.90D2 * t389 * t43 + (-0.180D3 * t389 * lh + 0.90D2 * t39
     #5) * t5 * t56 + (-0.180D3 * t395 * lh + 0.90D2 * t402 * 0.31415926
     #53589793D1 - t389 * t67) * t5 * t72) * t75 / 0.2880D4 - t93 * t8 *
     # t394 / 0.2880D4 + t90 * t9 * t415 / 0.32D2 - t116 * t8 * t388 / 0
     #.2880D4 - t125 * t8 * t402 / 0.2880D4 + t130 * ((t140 * t388 + 0.9
     #0D2 * t394) * t33 + 0.90D2 * t429 - t152 * t388) * t75 * t156 / 0.
     #1440D4 + (-0.90D2 * t129 * t8 * (-t39 * t402 + t165 * t394 - t168 
     #* t388 / 0.2D1) + 0.180D3 * t175 * t9 * (-t429 + t165 * t388) - t1
     #82 * t450) * t156 / 0.1440D4 + (0.90D2 * t129 * t8 * (t429 - t201 
     #* t388 + t33 * t394 - t195 * t388) - 0.180D3 * t175 * t9 * t464) *
     # t75 * t215 / 0.1440D4 + t130 * t464 * t75 * t220 / 0.8D1 + (-0.90
     #D2 * t129 * t8 * (-t429 + t228 * t388) - 0.180D3 * t234 * t450) * 
     #t156 * t215 / 0.720D3 + (0.90D2 * t90 * t9 * (t402 - t244 * t394 +
     # t245 * t388 / 0.2D1) - 0.180D3 * t120 * t9 * (-t244 * t388 + t394
     #) + t259 * t9 * t388) * t215 / 0.1440D4
      t506 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t505)
      t508 = t2 * x1
      t509 = -0.1D1 + x1
      t510 = x1 * z
      t511 = 0.1D1 - x1 + t510
      t512 = 0.1D1 / t511
      t514 = t2 * t509 * t512
      t515 = s * t16
      t517 = x1 * t509 * t512
      t518 = t515 * t517
      t519 = -t509
      t520 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.10
     #D1, x4)
      t521 = t39 * t520
      t522 = t509 ** 2
      t524 = t18 * t512 * t522
      t527 = log(0.4D1 * t191 * t524)
      t528 = t527 * t39
      t529 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.10
     #D1, x4)
      t532 = t17 * t512
      t537 = log(-0.4D1 * t190 * t78 * t532 * t522 * t20)
      t541 = x3 * t511
      t543 = Sqrt(-t541 * t19)
      t547 = x3 * x1
      t548 = t547 * z
      t549 = 0.3D1 * t548
      t550 = x1 * t14
      t551 = x3 * t14
      t552 = t551 * x1
      t554 = 0.2D1 * t190 * z
      t555 = t190 * t14
      t556 = 0.2D1 * t547
      t557 = -z + 0.2D1 * t26 * t543 * z + t510 - t549 - t550 + t552 + t
     #554 - t555 - x3 + t556 - t190
      t558 = 0.1D1 / t557
      t567 = -t39 * t529 - t511 * t529 * t558
      t580 = t224 * t12
      t583 = log(0.4D1 * t580 * t524)
      t584 = t583 * t39
      t601 = log(0.4D1 * t241 * t15 * t532 * t522)
      t602 = t601 ** 2
      t606 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.10
     #D1, x4)
      t621 = (0.90D2 * t129 * t8 * (-t521 + t528 * t529 + (-t520 + t537 
     #* t529) * t511 * t558) - 0.180D3 * t175 * t9 * t567) * t75 * t215 
     #/ 0.1440D4 + t130 * t567 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * 
     #t8 * (-t584 * t529 + t521) + 0.180D3 * t234 * t183 * t529) * t156 
     #* t215 / 0.720D3 + (0.90D2 * t90 * t9 * (-t602 * t529 / 0.2D1 + t6
     #01 * t520 - t606) - 0.180D3 * t120 * t9 * (-t520 + t601 * t529) - 
     #t259 * t9 * t529) * t215 / 0.1440D4
      t622 = FJET(XB1, XB2, s, 0.0D0, t508, -t514, 0.0D0, -t518, t621)
      t624 = x2 * s
      t625 = t624 * t1
      t626 = -0.1D1 + x2
      t627 = t626 * s
      t628 = t627 * t1
      t629 = t18 * t626
      t632 = log(-0.4D1 * t135 * t629)
      t635 = x2 * z
      t637 = 0.1D1 / (-z + t635 - x2)
      t638 = (-t132 - 0.90D2 * t632) * t637
      t639 = -t626
      t640 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.10
     #D1, x4)
      t642 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.10
     #D1, x4)
      t643 = t637 * t642
      t650 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.10
     #D1, x4)
      t654 = log(-0.4D1 * t161 * t629)
      t655 = t654 ** 2
      t656 = t655 * t637
      t659 = t654 * t637
      t670 = t8 * t637
      t671 = t670 * t640
      t676 = t129 * t670
      t683 = log(-0.4D1 * t580 * t629)
      t684 = t683 * t637
      t696 = t130 * (t638 * t640 + 0.90D2 * t643) * t75 * t156 / 0.1440D
     #4 + (-0.90D2 * t129 * t8 * (-t637 * t650 - t656 * t640 / 0.2D1 + t
     #659 * t642) + 0.180D3 * t175 * t9 * (t659 * t640 - t643) - t182 * 
     #t671) * t156 / 0.1440D4 + t676 * t640 * t75 * t220 / 0.8D1 + (-0.9
     #0D2 * t129 * t8 * (t684 * t640 - t643) - 0.180D3 * t234 * t671) * 
     #t156 * t215 / 0.720D3
      t697 = FJET(XB1, XB2, s, 0.0D0, t625, 0.0D0, -t628, 0.0D0, t696)
      t699 = x2 * x3
      t702 = Sqrt(x3 * t626 * t19)
      t703 = t26 * t702
      t705 = 0.2D1 * t703 * x2
      t707 = 0.1D1 - x3 + t699
      t708 = 0.1D1 / t707
      t710 = t2 * (0.1D1 - x3 - x2 + t699 + t134 + t705) * t708
      t715 = t2 * x2 * (-0.1D1 + t699 + 0.2D1 * t703) * t708
      t718 = t707 ** 2
      t724 = log(0.4D1 * t134 * t78 * t17 * t626 * t19 / t718)
      t727 = t699 * z
      t728 = t134 * z
      t734 = 0.1D1 / (z - t635 + x2 + x3 - t134 - t727 + t728 - t705 - 0
     #.2D1 * t703 * z + 0.2D1 * t703 * t635)
      t735 = (-t132 - 0.90D2 * t724) * t734
      t736 = t19 * t708
      t737 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t73
     #6, x4)
      t739 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t73
     #6, x4)
      t748 = t129 * t8 * t734
      t753 = t130 * (t735 * t737 + 0.90D2 * t734 * t739) * t75 * t156 / 
     #0.1440D4 + t748 * t737 * t75 * t220 / 0.8D1
      t754 = FJET(XB1, XB2, s, 0.0D0, t710, 0.0D0, -t715, 0.0D0, t753)
      t756 = t1 * t509
      t758 = t627 * t756 * t512
      t759 = t624 * t756
      t761 = t515 * t626 * t517
      t762 = x2 * x1
      t763 = t762 * z
      t765 = 0.1D1 / (z - t635 - t762 + t763 + x2)
      t766 = t8 * t765
      t767 = t129 * t766
      t768 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D1
     #, x4)
      t778 = log(-0.4D1 * t224 * t78 * t532 * t522 * t626)
      t779 = t778 * t765
      t781 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D1
     #, x4)
      t794 = t767 * t768 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * t8 * (
     #t779 * t768 - t765 * t781) - 0.180D3 * t234 * t766 * t768) * t156 
     #* t215 / 0.720D3
      t795 = FJET(XB1, XB2, s, 0.0D0, t758, t508, -t759, t761, t794)
      t797 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.10
     #D1, x4)
      t799 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.10
     #D1, x4)
      t800 = t637 * t799
      t807 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.10
     #D1, x4)
      t821 = t670 * t797
      t841 = t130 * (t638 * t797 + 0.90D2 * t800) * t75 * t156 / 0.1440D
     #4 + (-0.90D2 * t129 * t8 * (-t637 * t807 - t656 * t797 / 0.2D1 + t
     #659 * t799) + 0.180D3 * t175 * t9 * (t659 * t797 - t800) - t182 * 
     #t821) * t156 / 0.1440D4 + t676 * t797 * t75 * t220 / 0.8D1 + (-0.9
     #0D2 * t129 * t8 * (-t800 + t684 * t797) - 0.180D3 * t234 * t821) *
     # t156 * t215 / 0.720D3
      t842 = FJET(XB1, XB2, s, 0.0D0, -t628, 0.0D0, t625, 0.0D0, t841)
      t844 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.10
     #D1, x4)
      t845 = t39 * t844
      t846 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.10
     #D1, x4)
      t859 = -t39 * t846 - t511 * t846 * t558
      t884 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.10
     #D1, x4)
      t902 = (0.90D2 * t129 * t8 * (-t845 + t528 * t846 + (-t844 + t537 
     #* t846) * t511 * t558) - 0.180D3 * t175 * t9 * t859) * t75 * t215 
     #/ 0.1440D4 + t130 * t859 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * 
     #t8 * (-t584 * t846 + t845) + 0.180D3 * t234 * t183 * t846) * t156 
     #* t215 / 0.720D3 + (0.90D2 * t90 * t9 * (-t884 - t602 * t846 / 0.2
     #D1 + t601 * t844) - 0.180D3 * t120 * t9 * (-t844 + t601 * t846) - 
     #t259 * t9 * t846) * t215 / 0.1440D4
      t903 = FJET(XB1, XB2, s, 0.0D0, -t514, t508, 0.0D0, -t518, t902)
      t905 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t73
     #6, x4)
      t907 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t73
     #6, x4)
      t919 = t130 * (t735 * t905 + 0.90D2 * t734 * t907) * t75 * t156 / 
     #0.1440D4 + t748 * t905 * t75 * t220 / 0.8D1
      t920 = FJET(XB1, XB2, s, 0.0D0, -t715, 0.0D0, t710, 0.0D0, t919)
      t922 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t923 = t922 * 0.3141592653589793D1
      t928 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t929 = t928 * 0.3141592653589793D1
      t936 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t946 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t956 = t39 * t928
      t981 = t183 * t922
      t998 = t33 * t922 + t39 * t922
      t1039 = (0.90D2 * t923 * t43 + (-0.180D3 * t923 * lh + 0.90D2 * t9
     #29) * t5 * t56 + (-0.180D3 * t929 * lh + 0.90D2 * t936 * 0.3141592
     #653589793D1 - t923 * t67) * t5 * t72) * t75 / 0.2880D4 + t90 * t9 
     #* t946 / 0.32D2 - t125 * t8 * t936 / 0.2880D4 - t93 * t8 * t928 / 
     #0.2880D4 + t130 * (0.90D2 * t956 + (t140 * t922 + 0.90D2 * t928) *
     # t33 - t152 * t922) * t75 * t156 / 0.1440D4 + (-0.90D2 * t129 * t8
     # * (-t39 * t936 + t165 * t928 - t168 * t922 / 0.2D1) + 0.180D3 * t
     #175 * t9 * (-t956 + t165 * t922) - t182 * t981) * t156 / 0.1440D4 
     #- t116 * t8 * t922 / 0.2880D4 + (0.90D2 * t129 * t8 * (-t201 * t92
     #2 + t33 * t928 - t195 * t922 + t956) - 0.180D3 * t175 * t9 * t998)
     # * t75 * t215 / 0.1440D4 + t130 * t998 * t75 * t220 / 0.8D1 + (-0.
     #90D2 * t129 * t8 * (-t956 + t228 * t922) - 0.180D3 * t234 * t981) 
     #* t156 * t215 / 0.720D3 + (0.90D2 * t90 * t9 * (-t244 * t928 + t93
     #6 + t245 * t922 / 0.2D1) - 0.180D3 * t120 * t9 * (t928 - t244 * t9
     #22) + t259 * t9 * t922) * t215 / 0.1440D4
      t1040 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1039)
      t1042 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.1
     #0D1, x4)
      t1043 = t39 * t1042
      t1044 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.1
     #0D1, x4)
      t1057 = -t39 * t1044 - t511 * t1044 * t558
      t1084 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.1
     #0D1, x4)
      t1100 = (0.90D2 * t129 * t8 * (-t1043 + t528 * t1044 + (-t1042 + t
     #537 * t1044) * t511 * t558) - 0.180D3 * t175 * t9 * t1057) * t75 *
     # t215 / 0.1440D4 + t130 * t1057 * t75 * t220 / 0.8D1 + (-0.90D2 * 
     #t129 * t8 * (-t584 * t1044 + t1043) + 0.180D3 * t234 * t183 * t104
     #4) * t156 * t215 / 0.720D3 + (0.90D2 * t90 * t9 * (-t602 * t1044 /
     # 0.2D1 - t1084 + t601 * t1042) - 0.180D3 * t120 * t9 * (-t1042 + t
     #601 * t1044) - t259 * t9 * t1044) * t215 / 0.1440D4
      t1101 = FJET(XB1, XB2, s, t508, 0.0D0, 0.0D0, -t514, -t518, t1100)
      t1103 = t266 * t265 + t386 * t385 + t506 * t505 + t622 * t621 + t6
     #97 * t696 + t754 * t753 + t795 * t794 + t842 * t841 + t903 * t902 
     #+ t920 * t919 + t1040 * t1039 + t1101 * t1100
      t1104 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D
     #1, x4)
      t1110 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D
     #1, x4)
      t1123 = t767 * t1104 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * t8 *
     # (t779 * t1104 - t765 * t1110) - 0.180D3 * t234 * t766 * t1104) * 
     #t156 * t215 / 0.720D3
      t1124 = FJET(XB1, XB2, s, t508, -t759, 0.0D0, t758, t761, t1123)
      t1126 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.1
     #0D1, x4)
      t1128 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.1
     #0D1, x4)
      t1129 = t637 * t1128
      t1136 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.1
     #0D1, x4)
      t1150 = t670 * t1126
      t1170 = t130 * (t638 * t1126 + 0.90D2 * t1129) * t75 * t156 / 0.14
     #40D4 + (-0.90D2 * t129 * t8 * (-t637 * t1136 - t656 * t1126 / 0.2D
     #1 + t659 * t1128) + 0.180D3 * t175 * t9 * (-t1129 + t659 * t1126) 
     #- t182 * t1150) * t156 / 0.1440D4 + t676 * t1126 * t75 * t220 / 0.
     #8D1 + (-0.90D2 * t129 * t8 * (t684 * t1126 - t1129) - 0.180D3 * t2
     #34 * t1150) * t156 * t215 / 0.720D3
      t1171 = FJET(XB1, XB2, s, t625, 0.0D0, -t628, 0.0D0, 0.0D0, t1170)
      t1173 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t7
     #36, x4)
      t1175 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t7
     #36, x4)
      t1187 = t130 * (t735 * t1173 + 0.90D2 * t734 * t1175) * t75 * t156
     # / 0.1440D4 + t748 * t1173 * t75 * t220 / 0.8D1
      t1188 = FJET(XB1, XB2, s, t710, 0.0D0, -t715, 0.0D0, 0.0D0, t1187)
      t1190 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D
     #1, x4)
      t1195 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D
     #1, x4)
      t1209 = t767 * t1190 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * t8 *
     # (-t765 * t1195 + t779 * t1190) - 0.180D3 * t234 * t766 * t1190) *
     # t156 * t215 / 0.720D3
      t1210 = FJET(XB1, XB2, s, t758, 0.0D0, -t759, t508, t761, t1209)
      t1213 = t508 * t699 * t708
      t1214 = t2 * t509
      t1215 = t134 * x1
      t1218 = Sqrt(t541 * t626 * t19)
      t1219 = t26 * t1218
      t1221 = 0.2D1 * t1219 * x2
      t1222 = t134 * t510
      t1226 = t1214 * (-t1215 - x2 + t699 + t1221 + t1222 + 0.1D1 - x3 +
     # t134) * t512 * t708
      t1230 = t19 * s * t1 * x1 * t708
      t1236 = t1214 * x2 * (-0.1D1 + t699 + x1 - t547 - t510 + t548 + 0.
     #2D1 * t1219) * t512 * t708
      t1238 = t129 * t8 * t511
      t1241 = x2 * t189
      t1257 = 0.2D1 * t1219 * t763 - x3 - t1241 - x2 - t1215 + t1221 - t
     #699 * x1 + t190 * x2 + t762 * t14 + 0.2D1 * t1241 * z - t1241 * t1
     #4 + 0.2D1 * t1219 * z + t1222 - 0.2D1 * t1219 * t635 - 0.2D1 * t12
     #19 * t762 + 0.2D1 * t699 * t510 - t551 * t762
      t1264 = -0.2D1 * t190 * t635 + t190 * t14 * x2 - z + t510 - t550 +
     # t556 - t190 + t635 + t134 + 0.2D1 * t762 - t549 + t552 + t554 - t
     #555 + t727 - t728 - 0.3D1 * t763
      t1266 = 0.1D1 / (t1257 + t1264)
      t1267 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, -t736
     #, x4)
      t1270 = t75 * t156 * t215
      t1271 = t1266 * t1267 * t1270
      t1274 = FJET(XB1, XB2, s, t1213, -t1226, -t1230, t1236, t761, t123
     #8 * t1271 / 0.8D1)
      t1276 = t9 * t511
      t1280 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, -t736
     #, x4)
      t1282 = t1266 * t1280 * t1270
      t1285 = FJET(XB1, XB2, s, t1236, -t1230, -t1226, t1213, t761, t123
     #8 * t1282 / 0.8D1)
      t1290 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.1
     #0D1, x4)
      t1291 = t637 * t1290
      t1293 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.1
     #0D1, x4)
      t1300 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, 0.1
     #0D1, x4)
      t1314 = t670 * t1293
      t1334 = t130 * (0.90D2 * t1291 + t638 * t1293) * t75 * t156 / 0.14
     #40D4 + (-0.90D2 * t129 * t8 * (-t637 * t1300 - t656 * t1293 / 0.2D
     #1 + t659 * t1290) + 0.180D3 * t175 * t9 * (-t1291 + t659 * t1293) 
     #- t182 * t1314) * t156 / 0.1440D4 + t676 * t1293 * t75 * t220 / 0.
     #8D1 + (-0.90D2 * t129 * t8 * (t684 * t1293 - t1291) - 0.180D3 * t2
     #34 * t1314) * t156 * t215 / 0.720D3
      t1335 = FJET(XB1, XB2, s, -t628, 0.0D0, t625, 0.0D0, 0.0D0, t1334)
      t1337 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.1
     #0D1, x4)
      t1338 = t39 * t1337
      t1339 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.1
     #0D1, x4)
      t1352 = -t39 * t1339 - t511 * t1339 * t558
      t1377 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, t519, 0.10D1, 0.1
     #0D1, x4)
      t1395 = (0.90D2 * t129 * t8 * (-t1338 + t528 * t1339 + (-t1337 + t
     #537 * t1339) * t511 * t558) - 0.180D3 * t175 * t9 * t1352) * t75 *
     # t215 / 0.1440D4 + t130 * t1352 * t75 * t220 / 0.8D1 + (-0.90D2 * 
     #t129 * t8 * (-t584 * t1339 + t1338) + 0.180D3 * t234 * t183 * t133
     #9) * t156 * t215 / 0.720D3 + (0.90D2 * t90 * t9 * (-t1377 - t602 *
     # t1339 / 0.2D1 + t601 * t1337) - 0.180D3 * t120 * t9 * (t601 * t13
     #39 - t1337) - t259 * t9 * t1339) * t215 / 0.1440D4
      t1396 = FJET(XB1, XB2, s, -t514, 0.0D0, 0.0D0, t508, -t518, t1395)
      t1398 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D
     #1, x4)
      t1403 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t519, t639, 0.10D
     #1, x4)
      t1417 = t767 * t1398 * t75 * t220 / 0.8D1 + (-0.90D2 * t129 * t8 *
     # (-t765 * t1403 + t779 * t1398) - 0.180D3 * t234 * t766 * t1398) *
     # t156 * t215 / 0.720D3
      t1418 = FJET(XB1, XB2, s, -t759, t508, t758, 0.0D0, t761, t1417)
      t1420 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t7
     #36, x4)
      t1423 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t639, -t7
     #36, x4)
      t1434 = t130 * (0.90D2 * t734 * t1420 + t735 * t1423) * t75 * t156
     # / 0.1440D4 + t748 * t1423 * t75 * t220 / 0.8D1
      t1435 = FJET(XB1, XB2, s, -t715, 0.0D0, t710, 0.0D0, 0.0D0, t1434)
      t1437 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, -t736
     #, x4)
      t1439 = t1266 * t1437 * t1270
      t1442 = FJET(XB1, XB2, s, -t1230, t1236, t1213, -t1226, t761, t123
     #8 * t1439 / 0.8D1)
      t1447 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t519, t639, -t736
     #, x4)
      t1449 = t1266 * t1447 * t1270
      t1452 = FJET(XB1, XB2, s, -t1226, t1213, t1236, -t1230, t761, t123
     #8 * t1449 / 0.8D1)
      t1457 = t1124 * t1123 + t1171 * t1170 + t1188 * t1187 + t1210 * t1
     #209 + t1274 * 0.3141592653589793D1 * t1276 * t1271 / 0.8D1 + t1285
     # * 0.3141592653589793D1 * t1276 * t1282 / 0.8D1 + t1335 * t1334 + 
     #t1396 * t1395 + t1418 * t1417 + t1435 * t1434 + t1442 * 0.31415926
     #53589793D1 * t1276 * t1439 / 0.8D1 + t1452 * 0.3141592653589793D1 
     #* t1276 * t1449 / 0.8D1
      rrqg2qght6s1e0 = t1103 + t1457

      end function



      doubleprecision function rrqg2qght6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t13 * t18 / t19)
      t25 = cos(t10)
      t27 = Sqrt(-x3 * t19)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t13 * t18)
      t37 = 0.1D1 / z
      t40 = t9 * (-t24 * t32 - t36 * t37)
      t45 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t51 = t8 * (t32 + t37)
      t54 = 0.1D1 / x3
      t57 = 0.3141592653589793D1 * t37
      t58 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t67 = log(0.4D1 * t15 * t12 * t17)
      t68 = t67 * 0.3141592653589793D1
      t72 = (0.180D3 * t57 * lh + 0.90D2 * t68 * t37) * t5
      t79 = t67 ** 2
      t83 = lh ** 2
      t85 = 0.3141592653589793D1 ** 2
      t90 = (-0.180D3 * t68 * t37 * lh - 0.45D2 * t79 * 0.31415926535897
     #93D1 * t37 + t57 * (-0.180D3 * t83 + 0.30D2 * t85)) * t5
      t94 = 0.3141592653589793D1 * t5
      t95 = t94 * t8
      t97 = t37 * t3
      t98 = t32 * t3 + t97
      t101 = 0.1D1 / x2
      t106 = x2 ** 2
      t107 = t106 * t12
      t110 = log(0.4D1 * t107 * t18)
      t111 = t110 * t37
      t118 = 0.3141592653589793D1 * lh * t5
      t119 = t8 * t37
      t122 = 0.180D3 * t118 * t119 * t3
      t126 = 0.1D1 / x1
      t127 = t101 * t126
      t131 = x1 ** 2
      t132 = t131 * t12
      t135 = log(0.4D1 * t132 * t18)
      t148 = (0.90D2 * t4 * t40 + (-0.180D3 * t4 * lh + 0.90D2 * t45 * 0
     #.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 + t57 * t9 * t58
     # / 0.32D2 - t72 * t8 * t45 / 0.2880D4 - t90 * t8 * t3 / 0.2880D4 +
     # t95 * t98 * t54 * t101 / 0.16D2 + (-0.90D2 * t94 * t8 * (-t37 * t
     #45 + t111 * t3) - t122) * t101 / 0.1440D4 + t95 * t97 * t127 / 0.8
     #D1 + (0.90D2 * t57 * t9 * (t45 - t135 * t3) - t122) * t126 / 0.144
     #0D4 + t95 * t98 * t54 * t126 / 0.16D2
      t149 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t148)
      t151 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t152 = t151 * 0.3141592653589793D1
      t157 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t166 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t177 = t37 * t151
      t178 = t32 * t151 + t177
      t192 = 0.180D3 * t118 * t119 * t151
      t211 = (0.90D2 * t152 * t40 + (-0.180D3 * t152 * lh + 0.90D2 * t15
     #7 * 0.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 + t57 * t9 
     #* t166 / 0.32D2 - t72 * t8 * t157 / 0.2880D4 - t90 * t8 * t151 / 0
     #.2880D4 + t95 * t178 * t54 * t101 / 0.16D2 + (-0.90D2 * t94 * t8 *
     # (-t37 * t157 + t111 * t151) - t192) * t101 / 0.1440D4 + t95 * t17
     #7 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (t157 - t135 * t151) - t19
     #2) * t126 / 0.1440D4 + t95 * t178 * t54 * t126 / 0.16D2
      t212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t211)
      t214 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t215 = t214 * 0.3141592653589793D1
      t220 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t229 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t240 = t37 * t214
      t241 = t32 * t214 + t240
      t255 = 0.180D3 * t118 * t119 * t214
      t274 = (0.90D2 * t215 * t40 + (-0.180D3 * t215 * lh + 0.90D2 * t22
     #0 * 0.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 + t57 * t9 
     #* t229 / 0.32D2 - t72 * t8 * t220 / 0.2880D4 - t90 * t8 * t214 / 0
     #.2880D4 + t95 * t241 * t54 * t101 / 0.16D2 + (-0.90D2 * t94 * t8 *
     # (-t37 * t220 + t111 * t214) - t255) * t101 / 0.1440D4 + t95 * t24
     #0 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (-t135 * t214 + t220) - t2
     #55) * t126 / 0.1440D4 + t95 * t241 * t54 * t126 / 0.16D2
      t275 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t274)
      t277 = t2 * x1
      t278 = -0.1D1 + x1
      t279 = x1 * z
      t280 = 0.1D1 - x1 + t279
      t281 = 0.1D1 / t280
      t283 = t2 * t278 * t281
      t284 = s * t16
      t286 = x1 * t278 * t281
      t287 = t284 * t286
      t288 = -t278
      t289 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t290 = t37 * t289
      t294 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t297 = t278 ** 2
      t301 = log(0.4D1 * t132 * t15 * t17 * t281 * t297)
      t316 = Sqrt(-x3 * t280 * t19)
      t320 = x3 * x1
      t326 = x3 * t131
      t331 = -z + 0.2D1 * t25 * t316 * z + t279 - 0.3D1 * t320 * z - x1 
     #* t14 + x3 * t14 * x1 + 0.2D1 * t326 * z - t326 * t14 - x3 + 0.2D1
     # * t320 - t326
      t332 = 0.1D1 / t331
      t339 = -t95 * t290 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (-t294 + 
     #t301 * t289) + 0.180D3 * t118 * t119 * t289) * t126 / 0.1440D4 + t
     #95 * (-t290 - t280 * t289 * t332) * t54 * t126 / 0.16D2
      t340 = FJET(XB1, XB2, s, 0.0D0, t277, -t283, 0.0D0, -t287, t339)
      t342 = x2 * s
      t343 = t342 * t1
      t344 = -0.1D1 + x2
      t345 = t344 * s
      t346 = t345 * t1
      t347 = x2 * z
      t349 = 0.1D1 / (-z + t347 - x2)
      t350 = -t344
      t351 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t352 = t349 * t351
      t353 = t54 * t101
      t360 = log(-0.4D1 * t107 * t18 * t344)
      t361 = t360 * t349
      t363 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t369 = t8 * t349
      t379 = t95 * t352 * t353 / 0.16D2 + (-0.90D2 * t94 * t8 * (t361 * 
     #t351 - t349 * t363) - 0.180D3 * t118 * t369 * t351) * t101 / 0.144
     #0D4 + t95 * t352 * t127 / 0.8D1
      t380 = FJET(XB1, XB2, s, 0.0D0, t343, 0.0D0, -t346, 0.0D0, t379)
      t382 = x2 * x3
      t383 = t106 * x3
      t386 = Sqrt(x3 * t344 * t19)
      t387 = t25 * t386
      t389 = 0.2D1 * t387 * x2
      t392 = 0.1D1 / (0.1D1 - x3 + t382)
      t394 = t2 * (0.1D1 - x3 - x2 + t382 + t383 + t389) * t392
      t399 = t2 * x2 * (-0.1D1 + t382 + 0.2D1 * t387) * t392
      t407 = 0.1D1 / (z - t347 + x2 + x3 - t383 - t382 * z + t383 * z - 
     #t389 - 0.2D1 * t387 * z + 0.2D1 * t387 * t347)
      t408 = t19 * t392
      t409 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, -t40
     #8, x4)
      t411 = t407 * t409 * t353
      t414 = FJET(XB1, XB2, s, 0.0D0, t394, 0.0D0, -t399, 0.0D0, t95 * t
     #411 / 0.16D2)
      t419 = t1 * t278
      t421 = t345 * t419 * t281
      t422 = t342 * t419
      t424 = t284 * t344 * t286
      t425 = x2 * x1
      t428 = 0.1D1 / (z - t347 - t425 + t425 * z + x2)
      t429 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t288, t350, 0.10D1
     #, x4)
      t431 = t428 * t429 * t127
      t434 = FJET(XB1, XB2, s, 0.0D0, t421, t277, -t422, t424, t95 * t43
     #1 / 0.8D1)
      t439 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t440 = t349 * t439
      t445 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t460 = t95 * t440 * t353 / 0.16D2 + (-0.90D2 * t94 * t8 * (t361 * 
     #t439 - t349 * t445) - 0.180D3 * t118 * t369 * t439) * t101 / 0.144
     #0D4 + t95 * t440 * t127 / 0.8D1
      t461 = FJET(XB1, XB2, s, 0.0D0, -t346, 0.0D0, t343, 0.0D0, t460)
      t463 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t464 = t37 * t463
      t468 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t487 = -t95 * t464 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (-t468 + 
     #t301 * t463) + 0.180D3 * t118 * t119 * t463) * t126 / 0.1440D4 + t
     #95 * (-t464 - t280 * t463 * t332) * t54 * t126 / 0.16D2
      t488 = FJET(XB1, XB2, s, 0.0D0, -t283, t277, 0.0D0, -t287, t487)
      t490 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, -t40
     #8, x4)
      t492 = t407 * t490 * t353
      t495 = FJET(XB1, XB2, s, 0.0D0, -t399, 0.0D0, t394, 0.0D0, t95 * t
     #492 / 0.16D2)
      t500 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t504 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t508 = t504 * 0.3141592653589793D1
      t513 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t523 = t37 * t504
      t524 = t32 * t504 + t523
      t538 = 0.180D3 * t118 * t119 * t504
      t560 = t57 * t9 * t500 / 0.32D2 - t90 * t8 * t504 / 0.2880D4 + (0.
     #90D2 * t508 * t40 + (-0.180D3 * t508 * lh + 0.90D2 * t513 * 0.3141
     #592653589793D1) * t5 * t51) * t54 / 0.2880D4 + t95 * t524 * t54 * 
     #t101 / 0.16D2 + (-0.90D2 * t94 * t8 * (-t37 * t513 + t111 * t504) 
     #- t538) * t101 / 0.1440D4 - t72 * t8 * t513 / 0.2880D4 + t95 * t52
     #3 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (t513 - t135 * t504) - t53
     #8) * t126 / 0.1440D4 + t95 * t524 * t54 * t126 / 0.16D2
      t561 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t560)
      t563 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t564 = t37 * t563
      t568 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t587 = -t95 * t564 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (-t568 + 
     #t301 * t563) + 0.180D3 * t118 * t119 * t563) * t126 / 0.1440D4 + t
     #95 * (-t564 - t280 * t563 * t332) * t54 * t126 / 0.16D2
      t588 = FJET(XB1, XB2, s, t277, 0.0D0, 0.0D0, -t283, -t287, t587)
      t590 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t288, t350, 0.10D1
     #, x4)
      t592 = t428 * t590 * t127
      t595 = FJET(XB1, XB2, s, t277, -t422, 0.0D0, t421, t424, t95 * t59
     #2 / 0.8D1)
      t600 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t601 = t349 * t600
      t605 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t621 = t95 * t601 * t353 / 0.16D2 + (-0.90D2 * t94 * t8 * (-t349 *
     # t605 + t361 * t600) - 0.180D3 * t118 * t369 * t600) * t101 / 0.14
     #40D4 + t95 * t601 * t127 / 0.8D1
      t622 = FJET(XB1, XB2, s, t343, 0.0D0, -t346, 0.0D0, 0.0D0, t621)
      t624 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, -t40
     #8, x4)
      t626 = t407 * t624 * t353
      t629 = FJET(XB1, XB2, s, t394, 0.0D0, -t399, 0.0D0, 0.0D0, t95 * t
     #626 / 0.16D2)
      t634 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t288, t350, 0.10D1
     #, x4)
      t636 = t428 * t634 * t127
      t639 = FJET(XB1, XB2, s, t421, 0.0D0, -t422, t277, t424, t95 * t63
     #6 / 0.8D1)
      t644 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t645 = t349 * t644
      t649 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, 0.10
     #D1, x4)
      t665 = t95 * t645 * t353 / 0.16D2 + (-0.90D2 * t94 * t8 * (-t349 *
     # t649 + t361 * t644) - 0.180D3 * t118 * t369 * t644) * t101 / 0.14
     #40D4 + t95 * t645 * t127 / 0.8D1
      t666 = FJET(XB1, XB2, s, -t346, 0.0D0, t343, 0.0D0, 0.0D0, t665)
      t668 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t669 = t37 * t668
      t674 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t288, 0.10D1, 0.10
     #D1, x4)
      t692 = -t95 * t669 * t127 / 0.8D1 + (0.90D2 * t57 * t9 * (t301 * t
     #668 - t674) + 0.180D3 * t118 * t119 * t668) * t126 / 0.1440D4 + t9
     #5 * (-t669 - t280 * t668 * t332) * t54 * t126 / 0.16D2
      t693 = FJET(XB1, XB2, s, -t283, 0.0D0, 0.0D0, t277, -t287, t692)
      t695 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t350, -t40
     #8, x4)
      t697 = t407 * t695 * t353
      t700 = FJET(XB1, XB2, s, -t399, 0.0D0, t394, 0.0D0, 0.0D0, t95 * t
     #697 / 0.16D2)
      t705 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t288, t350, 0.10D1
     #, x4)
      t707 = t428 * t705 * t127
      t710 = FJET(XB1, XB2, s, -t422, t277, t421, 0.0D0, t424, t95 * t70
     #7 / 0.8D1)
      rrqg2qght6s1em1 = t149 * t148 + t212 * t211 + t275 * t274 + t340 *
     # t339 + t380 * t379 + t414 * 0.3141592653589793D1 * t9 * t411 / 0.
     #16D2 + t434 * 0.3141592653589793D1 * t9 * t431 / 0.8D1 + t461 * t4
     #60 + t488 * t487 + t495 * 0.3141592653589793D1 * t9 * t492 / 0.16D
     #2 + t561 * t560 + t588 * t587 + t595 * 0.3141592653589793D1 * t9 *
     # t592 / 0.8D1 + t622 * t621 + t629 * 0.3141592653589793D1 * t9 * t
     #626 / 0.16D2 + t639 * 0.3141592653589793D1 * t9 * t636 / 0.8D1 + t
     #666 * t665 + t693 * t692 + t700 * 0.3141592653589793D1 * t9 * t697
     # / 0.16D2 + t710 * 0.3141592653589793D1 * t9 * t707 / 0.8D1

      end function



      doubleprecision function rrqg2qght6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t5 = 0.1D1 / t1
      t7 = s ** 2
      t9 = 0.1D1 / t7 / s
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t14 = Sqrt(-x3 * (-0.1D1 + x3))
      t20 = 0.1D1 / z
      t24 = t9 * (0.1D1 / (-z - x3 + 0.2D1 * t11 * t14 * z) + t20) / x3
      t28 = 0.3141592653589793D1 * t5 * t9
      t29 = t20 * t3
      t30 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t38 = 0.3141592653589793D1 * t20
      t39 = t5 * t9
      t40 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t46 = z ** 2
      t48 = Sin(t10)
      t49 = t48 ** 2
      t51 = t1 ** 2
      t52 = t51 ** 2
      t55 = log(0.4D1 / t46 * t49 * t52)
      t60 = (0.180D3 * t38 * lh + 0.90D2 * t55 * 0.3141592653589793D1 * 
     #t20) * t5
      t64 = t3 * 0.3141592653589793D1 * t5 * t24 / 0.32D2 + t28 * t29 * 
     #t30 / 0.16D2 + t28 * t29 * t34 / 0.16D2 + t38 * t39 * t40 / 0.32D2
     # - t60 * t9 * t3 / 0.2880D4
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t64)
      t67 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t72 = t20 * t67
      t79 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t86 = t67 * 0.3141592653589793D1 * t5 * t24 / 0.32D2 + t28 * t72 *
     # t30 / 0.16D2 + t28 * t72 * t34 / 0.16D2 + t38 * t39 * t79 / 0.32D
     #2 - t60 * t9 * t67 / 0.2880D4
      t87 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t86)
      t89 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t94 = t20 * t89
      t101 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t108 = t89 * 0.3141592653589793D1 * t5 * t24 / 0.32D2 + t28 * t94 
     #* t30 / 0.16D2 + t28 * t94 * t34 / 0.16D2 + t38 * t39 * t101 / 0.3
     #2D2 - t60 * t9 * t89 / 0.2880D4
      t109 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t111 = t2 * x1
      t112 = -0.1D1 + x1
      t115 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t117 = t2 * t112 * t115
      t121 = s * t51 * x1 * t112 * t115
      t122 = -t112
      t123 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t122, 0.10D1, 0.10
     #D1, x4)
      t128 = FJET(XB1, XB2, s, 0.0D0, t111, -t117, 0.0D0, -t121, -t28 * 
     #t20 * t123 * t34 / 0.16D2)
      t131 = t9 * t20
      t137 = x2 * s * t1
      t138 = -0.1D1 + x2
      t140 = t138 * s * t1
      t143 = 0.1D1 / (-z + x2 * z - x2)
      t144 = -t138
      t145 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t144, 0.10
     #D1, x4)
      t150 = FJET(XB1, XB2, s, 0.0D0, t137, 0.0D0, -t140, 0.0D0, t28 * t
     #143 * t145 * t30 / 0.16D2)
      t153 = t9 * t143
      t158 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t144, 0.10
     #D1, x4)
      t163 = FJET(XB1, XB2, s, 0.0D0, -t140, 0.0D0, t137, 0.0D0, t28 * t
     #143 * t158 * t30 / 0.16D2)
      t170 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t122, 0.10D1, 0.10
     #D1, x4)
      t175 = FJET(XB1, XB2, s, 0.0D0, -t117, t111, 0.0D0, -t121, -t28 * 
     #t20 * t170 * t34 / 0.16D2)
      t182 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t187 = t20 * t182
      t194 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t201 = t182 * 0.3141592653589793D1 * t5 * t24 / 0.32D2 + t28 * t18
     #7 * t30 / 0.16D2 + t28 * t187 * t34 / 0.16D2 + t38 * t39 * t194 / 
     #0.32D2 - t60 * t9 * t182 / 0.2880D4
      t202 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t201)
      t204 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t122, 0.10D1, 0.10
     #D1, x4)
      t209 = FJET(XB1, XB2, s, t111, 0.0D0, 0.0D0, -t117, -t121, -t28 * 
     #t20 * t204 * t34 / 0.16D2)
      t216 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t144, 0.10
     #D1, x4)
      t221 = FJET(XB1, XB2, s, t137, 0.0D0, -t140, 0.0D0, 0.0D0, t28 * t
     #143 * t216 * t30 / 0.16D2)
      t228 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t144, 0.10
     #D1, x4)
      t233 = FJET(XB1, XB2, s, -t140, 0.0D0, t137, 0.0D0, 0.0D0, t28 * t
     #143 * t228 * t30 / 0.16D2)
      t241 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t122, 0.10D1, 0.10
     #D1, x4)
      t246 = FJET(XB1, XB2, s, -t117, 0.0D0, 0.0D0, t111, -t121, -t38 * 
     #t5 * t9 * t241 * t34 / 0.16D2)
      rrqg2qght6s1em2 = t65 * t64 + t87 * t86 + t109 * t108 - t128 * 0.3
     #141592653589793D1 * t5 * t131 * t123 * t34 / 0.16D2 + t150 * 0.314
     #1592653589793D1 * t5 * t153 * t145 * t30 / 0.16D2 + t163 * 0.31415
     #92653589793D1 * t5 * t153 * t158 * t30 / 0.16D2 - t175 * 0.3141592
     #653589793D1 * t5 * t131 * t170 * t34 / 0.16D2 + t202 * t201 - t209
     # * 0.3141592653589793D1 * t5 * t131 * t204 * t34 / 0.16D2 + t221 *
     # 0.3141592653589793D1 * t5 * t153 * t216 * t30 / 0.16D2 + t233 * 0
     #.3141592653589793D1 * t5 * t153 * t228 * t30 / 0.16D2 - t246 * 0.3
     #141592653589793D1 * t20 * t39 * t241 * t34 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t9 = 0.1D1 / t5 / s / z
      t10 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t11 = t9 * t10
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t34 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t35 = t9 * t34
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t35 /
     # 0.32D2)
      rrqg2qght6s1em3 = t14 * 0.3141592653589793D1 * t3 * t11 / 0.32D2 +
     # t22 * 0.3141592653589793D1 * t3 * t19 / 0.32D2 + t30 * 0.31415926
     #53589793D1 * t3 * t27 / 0.32D2 + t38 * 0.3141592653589793D1 * t3 *
     # t35 / 0.32D2

      end function



      doubleprecision function rrqg2qght6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      rrqg2qght6s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght6s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t12 = 0.3141592653589793D1 * lh
      t13 = t3 * t7
      t14 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t15 = t13 * t14
      t18 = lh ** 2
      t20 = 0.3141592653589793D1 ** 2
      t22 = -0.180D3 * t18 + 0.30D2 * t20
      t23 = 0.3141592653589793D1 * t22
      t24 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t25 = t13 * t24
      t28 = z ** 2
      t30 = 0.1D1 / t28 / z
      t31 = x3 * t30
      t32 = x4 * 0.3141592653589793D1
      t33 = Sin(t32)
      t34 = t33 ** 2
      t35 = t1 ** 2
      t36 = t35 ** 2
      t37 = t34 * t36
      t38 = -0.1D1 + x3
      t39 = 0.1D1 / t38
      t40 = t37 * t39
      t43 = log(-0.4D1 * t31 * t40)
      t44 = cos(t32)
      t45 = x3 * z
      t47 = Sqrt(-t45 * t38)
      t51 = 0.1D1 / (-z - x3 + 0.2D1 * t44 * t47)
      t55 = log(0.4D1 * t31 * t37)
      t56 = 0.1D1 / z
      t58 = t43 * t51 + t55 * t56
      t61 = t55 ** 2
      t64 = t43 ** 2
      t68 = t61 * t55 * t56 / 0.6D1 + t64 * t43 * t51 / 0.6D1
      t73 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t81 = -0.60D2 * lh * t20 + 0.2884936567583026D3 + 0.120D3 * t18 * 
     #lh
      t82 = 0.3141592653589793D1 * t81
      t88 = -t51 - t56
      t99 = -t64 * t51 / 0.2D1 - t61 * t56 / 0.2D1
      t102 = 0.1D1 / x3
      t105 = t30 * t34
      t106 = t105 * t36
      t108 = log(0.4D1 * t106)
      t109 = t108 ** 2
      t112 = t109 * t108
      t130 = t24 * t56
      t132 = t20 ** 2
      t133 = t18 ** 2
      t138 = 0.3141592653589793D1 * (-0.5769873135166051D3 * lh - t132 -
     # 0.60D2 * t133 + 0.60D2 * t18 * t20)
      t140 = rrqg2qgh62J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t146 = t109 ** 2
      t157 = x1 ** 2
      t158 = x3 * t157
      t159 = t158 * t34
      t160 = t30 * t36
      t164 = log(-0.4D1 * t159 * t160 * t39)
      t166 = t164 ** 2
      t173 = log(0.4D1 * t158 * t106)
      t175 = t173 ** 2
      t194 = t24 * t51
      t200 = 0.1D1 / x1
      t203 = 0.3141592653589793D1 * t56
      t204 = t203 * t22
      t205 = t157 * t34
      t208 = log(0.4D1 * t205 * t160)
      t214 = t208 ** 2
      t217 = t214 * t208
      t224 = t203 * t81
      t226 = t203 * lh
      t237 = 0.1D1 - x2
      t238 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t239 = t56 * t238
      t240 = x2 ** 2
      t241 = x3 * t240
      t242 = t241 * t157
      t247 = log(-0.4D1 * t242 * t105 * t36 * t39)
      t251 = -t237
      t252 = t36 * t251
      t256 = log(-0.4D1 * t242 * t105 * t252)
      t257 = t256 * t56
      t258 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t262 = log(0.4D1 * t242 * t106)
      t270 = t56 * t258
      t271 = t270 - t194 - t130
      t277 = 0.1D1 / x2
      t278 = t277 * t200
      t281 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t282 = t56 * t281
      t283 = t240 * t157
      t284 = t283 * t34
      t288 = log(-0.4D1 * t284 * t160 * t251)
      t289 = t288 * t56
      t291 = t288 ** 2
      t292 = t291 * t56
      t297 = log(0.4D1 * t283 * t106)
      t299 = t297 ** 2
      t323 = t241 * t30
      t326 = log(-0.4D1 * t323 * t40)
      t328 = t326 ** 2
      t335 = log(0.4D1 * t241 * t106)
      t337 = t335 ** 2
      t342 = t37 * t251
      t345 = log(-0.4D1 * t323 * t342)
      t346 = t345 * t56
      t348 = t345 ** 2
      t349 = t348 * t56
      t374 = t30 * t240
      t377 = log(-0.4D1 * t374 * t342)
      t381 = log(0.4D1 * t374 * t37)
      t387 = t381 ** 2
      t388 = t387 * t381
      t391 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t394 = t377 ** 2
      t397 = t394 * t377
      t421 = -((0.90D2 * t4 * t7 * t8 - 0.180D3 * t12 * t15 - t23 * t25)
     # * t58 + 0.90D2 * t4 * t7 * t24 * t68 + (-t23 * t15 + 0.90D2 * t4 
     #* t7 * t73 - t82 * t25 - 0.180D3 * t12 * t13 * t8) * t88 + (0.90D2
     # * t4 * t7 * t14 - 0.180D3 * t12 * t25) * t99) * t102 / 0.2880D4 +
     # (0.180D3 * (-t109 * t14 / 0.2D1 - t73 + t112 * t24 / 0.6D1 + t108
     # * t8) * t56 * t12 + (t108 * t14 - t8 - t109 * t24 / 0.2D1) * t56 
     #* t23 + (-t14 + t108 * t24) * t56 * t82 - t130 * t138 - 0.90D2 * (
     #-t140 + t112 * t14 / 0.6D1 - t109 * t8 / 0.2D1 + t108 * t73 - t146
     # * t24 / 0.24D2) * t56 * 0.3141592653589793D1) * t3 * t7 / 0.2880D
     #4 - (-0.90D2 * t4 * t7 * (-(t164 * t14 - t8 - t166 * t24 / 0.2D1) 
     #* t51 - (t173 * t14 - t8 - t175 * t24 / 0.2D1) * t56) + 0.180D3 * 
     #t12 * t13 * (-(-t14 + t173 * t24) * t56 - (-t14 + t164 * t24) * t5
     #1) + t23 * t13 * (t194 + t130)) * t102 * t200 / 0.1440D4 + (t204 *
     # t13 * (-t14 + t208 * t24) - 0.90D2 * t203 * t13 * (-t73 + t208 * 
     #t8 - t214 * t14 / 0.2D1 + t217 * t24 / 0.6D1) - t224 * t25 + 0.180
     #D3 * t226 * t13 * (t208 * t14 - t8 - t214 * t24 / 0.2D1)) * t200 /
     # 0.1440D4 + (-0.90D2 * t4 * t7 * (t239 + (-t14 + t247 * t24) * t51
     # - t257 * t258 + (-t14 + t262 * t24) * t56) + 0.180D3 * t12 * t13 
     #* t271) * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t282 - t28
     #9 * t238 + t292 * t258 / 0.2D1 + (t297 * t14 - t8 - t299 * t24 / 0
     #.2D1) * t56) + 0.180D3 * t12 * t13 * (t239 - t289 * t258 + (-t14 +
     # t297 * t24) * t56) + t23 * t13 * (-t130 + t270)) * t277 * t200 / 
     #0.720D3 - (-0.90D2 * t4 * t7 * (-(t326 * t14 - t8 - t328 * t24 / 0
     #.2D1) * t51 - t282 - (t335 * t14 - t8 - t337 * t24 / 0.2D1) * t56 
     #+ t346 * t238 - t349 * t258 / 0.2D1) + 0.180D3 * t12 * t13 * (-(-t
     #14 + t335 * t24) * t56 - (-t14 + t326 * t24) * t51 - t239 + t346 *
     # t258) - t23 * t13 * t271) * t102 * t277 / 0.1440D4 + (t204 * t13 
     #* (-t14 + t238 - t377 * t258 + t381 * t24) - 0.90D2 * t203 * t13 *
     # (t381 * t8 + t388 * t24 / 0.6D1 + t391 - t387 * t14 / 0.2D1 + t39
     #4 * t238 / 0.2D1 - t397 * t258 / 0.6D1 - t73 - t377 * t281) + t224
     # * t13 * (t258 - t24) + 0.180D3 * t226 * t13 * (-t377 * t238 + t28
     #1 + t394 * t258 / 0.2D1 + t381 * t14 - t8 - t387 * t24 / 0.2D1)) *
     # t277 / 0.1440D4
      t422 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t421)
      t424 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t428 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t429 = t13 * t428
      t432 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t433 = t13 * t432
      t442 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t481 = t432 * t56
      t488 = rrqg2qgh64J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t523 = t432 * t51
      t555 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t556 = t56 * t555
      t560 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t569 = t56 * t560
      t570 = t569 - t481 - t523
      t578 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t579 = t56 * t578
      t655 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t678 = -((0.90D2 * t4 * t7 * t424 - 0.180D3 * t12 * t429 - t23 * t
     #433) * t58 + 0.90D2 * t4 * t7 * t432 * t68 + (-t23 * t429 + 0.90D2
     # * t4 * t7 * t442 - t82 * t433 - 0.180D3 * t12 * t13 * t424) * t88
     # + (0.90D2 * t4 * t7 * t428 - 0.180D3 * t12 * t433) * t99) * t102 
     #/ 0.2880D4 + (0.180D3 * (-t442 - t109 * t428 / 0.2D1 + t112 * t432
     # / 0.6D1 + t108 * t424) * t56 * t12 + (-t109 * t432 / 0.2D1 - t424
     # + t108 * t428) * t56 * t23 + (-t428 + t108 * t432) * t56 * t82 - 
     #t481 * t138 - 0.90D2 * (t112 * t428 / 0.6D1 - t109 * t424 / 0.2D1 
     #+ t108 * t442 - t488 - t146 * t432 / 0.24D2) * t56 * 0.31415926535
     #89793D1) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t175 * t4
     #32 / 0.2D1 - t424 + t173 * t428) * t56 - (-t166 * t432 / 0.2D1 - t
     #424 + t164 * t428) * t51) + 0.180D3 * t12 * t13 * (-(-t428 + t173 
     #* t432) * t56 - (-t428 + t164 * t432) * t51) + t23 * t13 * (t523 +
     # t481)) * t102 * t200 / 0.1440D4 + (t204 * t13 * (-t428 + t208 * t
     #432) - 0.90D2 * t203 * t13 * (-t442 + t217 * t432 / 0.6D1 - t214 *
     # t428 / 0.2D1 + t208 * t424) - t224 * t433 + 0.180D3 * t226 * t13 
     #* (-t214 * t432 / 0.2D1 - t424 + t208 * t428)) * t200 / 0.1440D4 +
     # (-0.90D2 * t4 * t7 * (t556 + (-t428 + t247 * t432) * t51 - t257 *
     # t560 + (-t428 + t262 * t432) * t56) + 0.180D3 * t12 * t13 * t570)
     # * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t579 - t289 * t55
     #5 + t292 * t560 / 0.2D1 + (-t299 * t432 / 0.2D1 - t424 + t297 * t4
     #28) * t56) + 0.180D3 * t12 * t13 * (t556 - t289 * t560 + (-t428 + 
     #t297 * t432) * t56) + t23 * t13 * (t569 - t481)) * t277 * t200 / 0
     #.720D3 - (-0.90D2 * t4 * t7 * (t346 * t555 - (-t337 * t432 / 0.2D1
     # - t424 + t335 * t428) * t56 - t579 - (-t328 * t432 / 0.2D1 - t424
     # + t326 * t428) * t51 - t349 * t560 / 0.2D1) + 0.180D3 * t12 * t13
     # * (-(-t428 + t335 * t432) * t56 - (-t428 + t326 * t432) * t51 - t
     #556 + t346 * t560) - t23 * t13 * t570) * t102 * t277 / 0.1440D4 + 
     #(t204 * t13 * (-t377 * t560 + t555 - t428 + t381 * t432) - 0.90D2 
     #* t203 * t13 * (-t387 * t428 / 0.2D1 - t377 * t578 + t381 * t424 +
     # t388 * t432 / 0.6D1 + t394 * t555 / 0.2D1 + t655 - t442 - t397 * 
     #t560 / 0.6D1) + t224 * t13 * (t560 - t432) + 0.180D3 * t226 * t13 
     #* (-t377 * t555 - t387 * t432 / 0.2D1 - t424 + t394 * t560 / 0.2D1
     # + t578 + t381 * t428)) * t277 / 0.1440D4
      t679 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t678)
      t681 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t685 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t686 = t13 * t685
      t689 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t690 = t13 * t689
      t699 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t738 = t689 * t56
      t740 = rrqg2qgh61J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t780 = t689 * t51
      t812 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t813 = t56 * t812
      t817 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t826 = t56 * t817
      t827 = t826 - t738 - t780
      t835 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t836 = t56 * t835
      t914 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10D
     #1, x4)
      t935 = -((0.90D2 * t4 * t7 * t681 - 0.180D3 * t12 * t686 - t23 * t
     #690) * t58 + 0.90D2 * t4 * t7 * t689 * t68 + (-t23 * t686 + 0.90D2
     # * t4 * t7 * t699 - t82 * t690 - 0.180D3 * t12 * t13 * t681) * t88
     # + (0.90D2 * t4 * t7 * t685 - 0.180D3 * t12 * t690) * t99) * t102 
     #/ 0.2880D4 + (0.180D3 * (-t109 * t685 / 0.2D1 + t112 * t689 / 0.6D
     #1 - t699 + t108 * t681) * t56 * t12 + (-t109 * t689 / 0.2D1 - t681
     # + t108 * t685) * t56 * t23 + (-t685 + t108 * t689) * t56 * t82 - 
     #t738 * t138 - 0.90D2 * (-t740 + t112 * t685 / 0.6D1 - t109 * t681 
     #/ 0.2D1 - t146 * t689 / 0.24D2 + t108 * t699) * t56 * 0.3141592653
     #589793D1) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t166 * t
     #689 / 0.2D1 - t681 + t164 * t685) * t51 - (-t175 * t689 / 0.2D1 - 
     #t681 + t173 * t685) * t56) + 0.180D3 * t12 * t13 * (-(-t685 + t164
     # * t689) * t51 - (-t685 + t173 * t689) * t56) + t23 * t13 * (t780 
     #+ t738)) * t102 * t200 / 0.1440D4 + (t204 * t13 * (-t685 + t208 * 
     #t689) - 0.90D2 * t203 * t13 * (t217 * t689 / 0.6D1 - t699 + t208 *
     # t681 - t214 * t685 / 0.2D1) - t224 * t690 + 0.180D3 * t226 * t13 
     #* (-t214 * t689 / 0.2D1 - t681 + t208 * t685)) * t200 / 0.1440D4 +
     # (-0.90D2 * t4 * t7 * (t813 + (-t685 + t247 * t689) * t51 - t257 *
     # t817 + (-t685 + t262 * t689) * t56) + 0.180D3 * t12 * t13 * t827)
     # * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t836 - t289 * t81
     #2 + t292 * t817 / 0.2D1 + (-t299 * t689 / 0.2D1 - t681 + t297 * t6
     #85) * t56) + 0.180D3 * t12 * t13 * (t813 - t289 * t817 + (-t685 + 
     #t297 * t689) * t56) + t23 * t13 * (t826 - t738)) * t277 * t200 / 0
     #.720D3 - (-0.90D2 * t4 * t7 * (-(-t337 * t689 / 0.2D1 - t681 + t33
     #5 * t685) * t56 - (-t328 * t689 / 0.2D1 - t681 + t326 * t685) * t5
     #1 - t836 + t346 * t812 - t349 * t817 / 0.2D1) + 0.180D3 * t12 * t1
     #3 * (-(-t685 + t335 * t689) * t56 - (-t685 + t326 * t689) * t51 - 
     #t813 + t346 * t817) - t23 * t13 * t827) * t102 * t277 / 0.1440D4 +
     # (t204 * t13 * (-t377 * t817 + t812 - t685 + t381 * t689) - 0.90D2
     # * t203 * t13 * (-t699 + t394 * t812 / 0.2D1 - t387 * t685 / 0.2D1
     # - t377 * t835 + t381 * t681 - t397 * t817 / 0.6D1 + t388 * t689 /
     # 0.6D1 + t914) + t224 * t13 * (t817 - t689) + 0.180D3 * t226 * t13
     # * (t835 + t394 * t817 / 0.2D1 - t377 * t812 - t681 - t387 * t689 
     #/ 0.2D1 + t381 * t685)) * t277 / 0.1440D4
      t936 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t935)
      t939 = x1 * z
      t940 = -z - x1 + t939
      t941 = 0.1D1 / t940
      t943 = t2 * x1 * t251 * t941
      t944 = -0.1D1 + x1
      t945 = t2 * t944
      t948 = x2 * s * t1 * x1
      t949 = s * t35
      t952 = x1 * t944 * t941
      t953 = t949 * t251 * t952
      t954 = x2 * x1
      t955 = t954 * z
      t957 = 0.1D1 / (-z + t955 - t954)
      t958 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1, 
     #x4)
      t959 = t957 * t958
      t960 = t241 * t205
      t961 = 0.1D1 / t28
      t962 = t961 * t36
      t963 = t944 ** 2
      t964 = t941 * t963
      t969 = log(0.4D1 * t960 * t962 * t964 * t251)
      t970 = t969 * t957
      t971 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1, 
     #x4)
      t977 = t12 * t3
      t978 = t7 * t957
      t979 = t978 * t971
      t985 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1, 
     #x4)
      t987 = t34 * t961
      t989 = t36 * t941
      t994 = log(0.4D1 * t283 * t987 * t989 * t963 * t251)
      t995 = t994 ** 2
      t996 = t995 * t957
      t999 = t994 * t957
      t1010 = t23 * t3
      t1016 = (-0.90D2 * t4 * t7 * (t959 - t970 * t971) + 0.180D3 * t977
     # * t979) * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t957 * t9
     #85 + t996 * t971 / 0.2D1 - t999 * t958) + 0.180D3 * t12 * t13 * (t
     #959 - t999 * t971) + t1010 * t979) * t277 * t200 / 0.720D3
      t1017 = FJET(XB1, XB2, s, 0.0D0, t943, -t945, t948, -t953, t1016)
      t1019 = x2 * x3
      t1020 = 0.1D1 - x3 + t1019
      t1021 = 0.1D1 / t1020
      t1022 = t1019 * t1021
      t1023 = t2 * t1022
      t1024 = t38 * t1021
      t1025 = t2 * t1024
      t1026 = t251 * t38
      t1027 = t1020 ** 2
      t1028 = 0.1D1 / t1027
      t1029 = t1026 * t1028
      t1033 = log(0.4D1 * t960 * t160 * t1029)
      t1035 = Sqrt(t45 * t1026)
      t1039 = 0.1D1 / (-z - x3 + t1019 + 0.2D1 * t44 * t1035)
      t1040 = t1033 * t1039
      t1041 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1043 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1044 = t1039 * t1043
      t1049 = t7 * t1039
      t1050 = t1049 * t1041
      t1062 = log(0.4D1 * t241 * t105 * t252 * t38 * t1028)
      t1063 = t1062 * t1039
      t1065 = t1062 ** 2
      t1066 = t1065 * t1039
      t1069 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1085 = (-0.90D2 * t4 * t7 * (-t1040 * t1041 + t1044) + 0.180D3 * 
     #t977 * t1050) * t102 * t278 / 0.720D3 - (-0.90D2 * t4 * t7 * (t106
     #3 * t1043 - t1066 * t1041 / 0.2D1 - t1039 * t1069) + 0.180D3 * t12
     # * t13 * (t1063 * t1041 - t1044) - t1010 * t1050) * t102 * t277 / 
     #0.1440D4
      t1086 = FJET(XB1, XB2, s, 0.0D0, t1023, 0.0D0, -t1025, 0.0D0, t108
     #5)
      t1089 = t2 * x1 * t941
      t1090 = t949 * t952
      t1091 = t962 * t964
      t1094 = log(-0.4D1 * t159 * t1091)
      t1095 = t1094 ** 2
      t1096 = t1095 * t56
      t1097 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1100 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1107 = log(0.4D1 * t158 * t987 * t989 * t963 * t39)
      t1108 = t1107 * t940
      t1109 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1111 = t1107 ** 2
      t1112 = t1111 * t940
      t1116 = x3 * x1
      t1117 = t1116 * z
      t1118 = t158 * t28
      t1119 = x1 * t28
      t1120 = x3 * t28
      t1121 = t1120 * x1
      t1123 = 0.2D1 * t158 * z
      t1124 = x3 * t940
      t1126 = Sqrt(t1124 * t38)
      t1131 = 0.1D1 / (-t939 - t1117 - t158 - t1118 - t45 + t1119 + t112
     #1 + t1123 - t28 + 0.2D1 * t44 * t1126 * z)
      t1133 = t56 * t1100
      t1134 = t1094 * t56
      t1140 = t56 * t1109
      t1141 = t940 * t1109
      t1153 = -t56 * t1097 + t940 * t1097 * t1131
      t1160 = t205 * t961
      t1161 = t989 * t963
      t1164 = log(-0.4D1 * t1160 * t1161)
      t1169 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1170 = t1164 ** 2
      t1171 = t1170 * t1164
      t1197 = log(0.4D1 * t960 * t962 * t964 * t39)
      t1198 = t1197 * t940
      t1204 = log(-0.4D1 * t960 * t1091)
      t1205 = t1204 * t56
      t1221 = log(-0.4D1 * t284 * t1091)
      t1222 = t1221 ** 2
      t1223 = t1222 * t56
      t1226 = t1221 * t56
      t1237 = t7 * t56
      t1244 = -(-0.90D2 * t4 * t7 * (-t1096 * t1097 / 0.2D1 + (t940 * t1
     #100 - t1108 * t1109 + t1112 * t1097 / 0.2D1) * t1131 - t1133 + t11
     #34 * t1109) + 0.180D3 * t12 * t13 * (-t1140 + (t1141 - t1108 * t10
     #97) * t1131 + t1134 * t1097) + t23 * t13 * t1153) * t102 * t200 / 
     #0.1440D4 + (t204 * t13 * (t1109 - t1164 * t1097) - 0.90D2 * t203 *
     # t13 * (t1169 - t1171 * t1097 / 0.6D1 - t1164 * t1100 + t1170 * t1
     #109 / 0.2D1) + t224 * t13 * t1097 + 0.180D3 * t226 * t13 * (t1170 
     #* t1097 / 0.2D1 + t1100 - t1164 * t1109)) * t200 / 0.1440D4 + (-0.
     #90D2 * t4 * t7 * (-(t1141 - t1198 * t1097) * t1131 + t1140 - t1205
     # * t1097) - 0.180D3 * t12 * t13 * t1153) * t102 * t278 / 0.720D3 +
     # (-0.90D2 * t4 * t7 * (t1223 * t1097 / 0.2D1 + t1133 - t1226 * t11
     #09) + 0.180D3 * t12 * t13 * (-t1226 * t1097 + t1140) + t1010 * t12
     #37 * t1097) * t277 * t200 / 0.720D3
      t1245 = FJET(XB1, XB2, s, 0.0D0, -t945, -t1089, 0.0D0, t1090, t124
     #4)
      t1247 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1250 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1252 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1258 = t56 * t1250
      t1264 = t940 * t1252
      t1269 = t56 * t1252
      t1277 = t940 * t1247 * t1131 - t56 * t1247
      t1292 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1344 = -(-0.90D2 * t4 * t7 * (-t1096 * t1247 / 0.2D1 + (t940 * t1
     #250 - t1108 * t1252 + t1112 * t1247 / 0.2D1) * t1131 - t1258 + t11
     #34 * t1252) + 0.180D3 * t12 * t13 * ((t1264 - t1108 * t1247) * t11
     #31 + t1134 * t1247 - t1269) + t23 * t13 * t1277) * t102 * t200 / 0
     #.1440D4 + (t204 * t13 * (-t1164 * t1247 + t1252) - 0.90D2 * t203 *
     # t13 * (-t1171 * t1247 / 0.6D1 + t1170 * t1252 / 0.2D1 + t1292 - t
     #1164 * t1250) + t224 * t13 * t1247 + 0.180D3 * t226 * t13 * (t1250
     # + t1170 * t1247 / 0.2D1 - t1164 * t1252)) * t200 / 0.1440D4 + (-0
     #.90D2 * t4 * t7 * (-t1205 * t1247 + t1269 - (t1264 - t1198 * t1247
     #) * t1131) - 0.180D3 * t12 * t13 * t1277) * t102 * t278 / 0.720D3 
     #+ (-0.90D2 * t4 * t7 * (-t1226 * t1252 + t1258 + t1223 * t1247 / 0
     #.2D1) + 0.180D3 * t12 * t13 * (-t1226 * t1247 + t1269) + t1010 * t
     #1237 * t1247) * t277 * t200 / 0.720D3
      t1345 = FJET(XB1, XB2, s, 0.0D0, -t1089, -t945, 0.0D0, t1090, t134
     #4)
      t1347 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1348 = t1039 * t1347
      t1349 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1355 = t1049 * t1349
      t1365 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1381 = (-0.90D2 * t4 * t7 * (t1348 - t1040 * t1349) + 0.180D3 * t
     #977 * t1355) * t102 * t278 / 0.720D3 - (-0.90D2 * t4 * t7 * (t1063
     # * t1347 - t1066 * t1349 / 0.2D1 - t1039 * t1365) + 0.180D3 * t12 
     #* t13 * (t1063 * t1349 - t1348) - t1010 * t1355) * t102 * t277 / 0
     #.1440D4
      t1382 = FJET(XB1, XB2, s, 0.0D0, -t1025, 0.0D0, t1023, 0.0D0, t138
     #1)
      t1384 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1387 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1390 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1391 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1407 = t1387 * t56
      t1409 = rrqg2qgh63J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1449 = t1387 * t51
      t1470 = t13 * t1387
      t1482 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10
     #D1, x4)
      t1484 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10
     #D1, x4)
      t1485 = t56 * t1484
      t1496 = t56 * t1482
      t1497 = -t1449 - t1407 + t1496
      t1513 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10
     #D1, x4)
      t1514 = t56 * t1513
      t1537 = t13 * t1384
      t1610 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, 0.10
     #D1, x4)
      t1638 = (0.180D3 * (-t109 * t1384 / 0.2D1 + t112 * t1387 / 0.6D1 -
     # t1390 + t108 * t1391) * t56 * t12 + (t108 * t1384 - t1391 - t109 
     #* t1387 / 0.2D1) * t56 * t23 + (-t1384 + t108 * t1387) * t56 * t82
     # - t1407 * t138 - 0.90D2 * (-t1409 - t109 * t1391 / 0.2D1 - t146 *
     # t1387 / 0.24D2 + t112 * t1384 / 0.6D1 + t108 * t1390) * t56 * 0.3
     #141592653589793D1) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(
     #t164 * t1384 - t1391 - t166 * t1387 / 0.2D1) * t51 - (t173 * t1384
     # - t1391 - t175 * t1387 / 0.2D1) * t56) + 0.180D3 * t12 * t13 * (-
     #(-t1384 + t164 * t1387) * t51 - (-t1384 + t173 * t1387) * t56) + t
     #23 * t13 * (t1407 + t1449)) * t102 * t200 / 0.1440D4 + (t204 * t13
     # * (t208 * t1387 - t1384) - 0.90D2 * t203 * t13 * (-t214 * t1384 /
     # 0.2D1 - t1390 + t208 * t1391 + t217 * t1387 / 0.6D1) - t224 * t14
     #70 + 0.180D3 * t226 * t13 * (t208 * t1384 - t214 * t1387 / 0.2D1 -
     # t1391)) * t200 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t257 * t1482 +
     # t1485 + (-t1384 + t262 * t1387) * t56 + (-t1384 + t247 * t1387) *
     # t51) + 0.180D3 * t12 * t13 * t1497) * t102 * t278 / 0.720D3 + (-0
     #.90D2 * t4 * t7 * (t292 * t1482 / 0.2D1 - t289 * t1484 + (t297 * t
     #1384 - t1391 - t299 * t1387 / 0.2D1) * t56 + t1514) + 0.180D3 * t1
     #2 * t13 * (-t289 * t1482 + t1485 + (-t1384 + t297 * t1387) * t56) 
     #+ t23 * t13 * (t1496 - t1407)) * t277 * t200 / 0.720D3 - ((0.90D2 
     #* t4 * t7 * t1391 - 0.180D3 * t12 * t1537 - t23 * t1470) * t58 + 0
     #.90D2 * t4 * t7 * t1387 * t68 + (-t23 * t1537 + 0.90D2 * t4 * t7 *
     # t1390 - t82 * t1470 - 0.180D3 * t12 * t13 * t1391) * t88 + (0.90D
     #2 * t4 * t7 * t1384 - 0.180D3 * t12 * t1470) * t99) * t102 / 0.288
     #0D4 - (-0.90D2 * t4 * t7 * (-(t335 * t1384 - t1391 - t337 * t1387 
     #/ 0.2D1) * t56 - (t326 * t1384 - t1391 - t328 * t1387 / 0.2D1) * t
     #51 - t1514 + t346 * t1484 - t349 * t1482 / 0.2D1) + 0.180D3 * t12 
     #* t13 * (-(-t1384 + t326 * t1387) * t51 - t1485 - (-t1384 + t335 *
     # t1387) * t56 + t346 * t1482) - t23 * t13 * t1497) * t102 * t277 /
     # 0.1440D4 + (t204 * t13 * (-t377 * t1482 + t381 * t1387 - t1384 + 
     #t1484) - 0.90D2 * t203 * t13 * (-t387 * t1384 / 0.2D1 - t377 * t15
     #13 - t1390 + t1610 - t397 * t1482 / 0.6D1 + t388 * t1387 / 0.6D1 +
     # t394 * t1484 / 0.2D1 + t381 * t1391) + t224 * t13 * (t1482 - t138
     #7) + 0.180D3 * t226 * t13 * (t1513 - t387 * t1387 / 0.2D1 + t394 *
     # t1482 / 0.2D1 - t377 * t1484 - t1391 + t381 * t1384)) * t277 / 0.
     #1440D4
      t1639 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1638)
      t1641 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1642 = t957 * t1641
      t1643 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1649 = t978 * t1643
      t1655 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1674 = (-0.90D2 * t4 * t7 * (t1642 - t970 * t1643) + 0.180D3 * t9
     #77 * t1649) * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t957 *
     # t1655 - t999 * t1641 + t996 * t1643 / 0.2D1) + 0.180D3 * t12 * t1
     #3 * (-t999 * t1643 + t1642) + t1010 * t1649) * t277 * t200 / 0.720
     #D3
      t1675 = FJET(XB1, XB2, s, t948, -t945, t943, 0.0D0, -t953, t1674)
      t1677 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1678 = t957 * t1677
      t1679 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1685 = t978 * t1679
      t1691 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1710 = (-0.90D2 * t4 * t7 * (t1678 - t970 * t1679) + 0.180D3 * t9
     #77 * t1685) * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t957 *
     # t1691 - t999 * t1677 + t996 * t1679 / 0.2D1) + 0.180D3 * t12 * t1
     #3 * (t1678 - t999 * t1679) + t1010 * t1685) * t277 * t200 / 0.720D
     #3
      t1711 = FJET(XB1, XB2, s, t943, 0.0D0, t948, -t945, -t953, t1710)
      t1713 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1716 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1718 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t1725 = t1039 * t1716
      t1730 = t1049 * t1713
      t1747 = -(-0.90D2 * t4 * t7 * (-t1066 * t1713 / 0.2D1 + t1063 * t1
     #716 - t1039 * t1718) + 0.180D3 * t12 * t13 * (t1063 * t1713 - t172
     #5) - t1010 * t1730) * t102 * t277 / 0.1440D4 + (-0.90D2 * t4 * t7 
     #* (-t1040 * t1713 + t1725) + 0.180D3 * t977 * t1730) * t102 * t278
     # / 0.720D3
      t1748 = FJET(XB1, XB2, s, t1023, 0.0D0, -t1025, 0.0D0, 0.0D0, t174
     #7)
      t1753 = t38 * s * t1 * t944 * t1021
      t1754 = t2 * x1
      t1756 = Sqrt(-t1124 * t1026)
      t1757 = t44 * t1756
      t1763 = t1754 * x2 * (-x3 + t1019 - z + t45 - x1 + t1116 + t939 - 
     #t1117 + 0.2D1 * t1757) * t941 * t1021
      t1764 = t945 * t1022
      t1767 = t241 * x1
      t1769 = t241 * t939
      t1773 = t1754 * (0.2D1 * t1757 * x2 + t1767 + t241 * z - x2 + t101
     #9 - t1769 + 0.1D1 - x3) * t941 * t1021
      t1778 = log(-0.4D1 * t241 * t1160 * t1161 * t1029)
      t1779 = t1778 * t940
      t1786 = x2 * t157
      t1790 = -t1123 + t158 + t45 - t1119 - t1767 - 0.2D1 * t1757 * z - 
     #t1019 * z + t1019 * x1 - t158 * x2 - t954 * t28 - 0.2D1 * t1786 * 
     #z + t1786 * t28 + t28
      t1803 = t1786 + t1769 - 0.2D1 * t1019 * t939 + t1120 * t954 + 0.2D
     #1 * t158 * x2 * z - t158 * t28 * x2 - 0.2D1 * t1757 * t954 + 0.2D1
     # * t1757 * t955 + t955 + t1117 + t1118 - t1121 + t939
      t1805 = 0.1D1 / (t1790 + t1803)
      t1806 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t1807 = t1805 * t1806
      t1809 = t940 * t1805
      t1810 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t1816 = t7 * t940
      t1820 = -0.90D2 * t4 * t7 * (t1779 * t1807 - t1809 * t1810) - 0.18
     #0D3 * t977 * t1816 * t1807
      t1824 = FJET(XB1, XB2, s, t1753, t1763, -t1764, -t1773, -t953, t18
     #20 * t102 * t278 / 0.720D3)
      t1827 = t102 * t277 * t200
      t1830 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t1831 = t1805 * t1830
      t1833 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t1842 = -0.90D2 * t4 * t7 * (t1779 * t1831 - t1809 * t1833) - 0.18
     #0D3 * t977 * t1816 * t1831
      t1846 = FJET(XB1, XB2, s, t1763, t1753, -t1773, -t1764, -t953, t18
     #42 * t102 * t278 / 0.720D3)
      t1850 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1852 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1855 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1862 = t56 * t1855
      t1867 = t940 * t1850
      t1872 = t56 * t1850
      t1880 = t940 * t1852 * t1131 - t56 * t1852
      t1891 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1947 = -(-0.90D2 * t4 * t7 * (t1134 * t1850 - t1096 * t1852 / 0.2
     #D1 + (t940 * t1855 - t1108 * t1850 + t1112 * t1852 / 0.2D1) * t113
     #1 - t1862) + 0.180D3 * t12 * t13 * ((t1867 - t1108 * t1852) * t113
     #1 + t1134 * t1852 - t1872) + t23 * t13 * t1880) * t102 * t200 / 0.
     #1440D4 + (t204 * t13 * (t1850 - t1164 * t1852) - 0.90D2 * t203 * t
     #13 * (t1891 - t1164 * t1855 - t1171 * t1852 / 0.6D1 + t1170 * t185
     #0 / 0.2D1) + t224 * t13 * t1852 + 0.180D3 * t226 * t13 * (t1855 + 
     #t1170 * t1852 / 0.2D1 - t1164 * t1850)) * t200 / 0.1440D4 + (-0.90
     #D2 * t4 * t7 * (t1872 - t1205 * t1852 - (t1867 - t1198 * t1852) * 
     #t1131) - 0.180D3 * t12 * t13 * t1880) * t102 * t278 / 0.720D3 + (-
     #0.90D2 * t4 * t7 * (-t1226 * t1850 + t1223 * t1852 / 0.2D1 + t1862
     #) + 0.180D3 * t12 * t13 * (t1872 - t1226 * t1852) + t1010 * t1237 
     #* t1852) * t277 * t200 / 0.720D3
      t1948 = FJET(XB1, XB2, s, -t945, 0.0D0, 0.0D0, -t1089, t1090, t194
     #7)
      t1950 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1952 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1953 = t957 * t1952
      t1958 = t978 * t1950
      t1966 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, x1, t237, 0.10D1,
     # x4)
      t1983 = (-0.90D2 * t4 * t7 * (-t970 * t1950 + t1953) + 0.180D3 * t
     #977 * t1958) * t102 * t278 / 0.720D3 + (-0.90D2 * t4 * t7 * (t996 
     #* t1950 / 0.2D1 + t957 * t1966 - t999 * t1952) + 0.180D3 * t12 * t
     #13 * (t1953 - t999 * t1950) + t1010 * t1958) * t277 * t200 / 0.720
     #D3
      t1984 = FJET(XB1, XB2, s, -t945, t948, 0.0D0, t943, -t953, t1983)
      t1986 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1988 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1991 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1998 = t56 * t1991
      t2003 = t940 * t1986
      t2008 = t56 * t1986
      t2016 = t940 * t1988 * t1131 - t56 * t1988
      t2032 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t2083 = -(-0.90D2 * t4 * t7 * (t1134 * t1986 - t1096 * t1988 / 0.2
     #D1 + (t940 * t1991 - t1108 * t1986 + t1112 * t1988 / 0.2D1) * t113
     #1 - t1998) + 0.180D3 * t12 * t13 * ((t2003 - t1108 * t1988) * t113
     #1 + t1134 * t1988 - t2008) + t23 * t13 * t2016) * t102 * t200 / 0.
     #1440D4 + (t204 * t13 * (t1986 - t1164 * t1988) - 0.90D2 * t203 * t
     #13 * (-t1164 * t1991 - t1171 * t1988 / 0.6D1 + t1170 * t1986 / 0.2
     #D1 + t2032) + t224 * t13 * t1988 + 0.180D3 * t226 * t13 * (-t1164 
     #* t1986 + t1991 + t1170 * t1988 / 0.2D1)) * t200 / 0.1440D4 + (-0.
     #90D2 * t4 * t7 * (t2008 - (t2003 - t1198 * t1988) * t1131 - t1205 
     #* t1988) - 0.180D3 * t12 * t13 * t2016) * t102 * t278 / 0.720D3 + 
     #(-0.90D2 * t4 * t7 * (t1223 * t1988 / 0.2D1 + t1998 - t1226 * t198
     #6) + 0.180D3 * t12 * t13 * (t2008 - t1226 * t1988) + t1010 * t1237
     # * t1988) * t277 * t200 / 0.720D3
      t2084 = FJET(XB1, XB2, s, -t1089, 0.0D0, 0.0D0, -t945, t1090, t208
     #3)
      t2086 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t2089 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t2091 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t237, -t10
     #24, x4)
      t2098 = t1039 * t2089
      t2103 = t1049 * t2086
      t2120 = -(-0.90D2 * t4 * t7 * (-t1066 * t2086 / 0.2D1 + t1063 * t2
     #089 - t1039 * t2091) + 0.180D3 * t12 * t13 * (t1063 * t2086 - t209
     #8) - t1010 * t2103) * t102 * t277 / 0.1440D4 + (-0.90D2 * t4 * t7 
     #* (-t1040 * t2086 + t2098) + 0.180D3 * t977 * t2103) * t102 * t278
     # / 0.720D3
      t2121 = FJET(XB1, XB2, s, -t1025, 0.0D0, t1023, 0.0D0, 0.0D0, t212
     #0)
      t2123 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t2125 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t2126 = t1805 * t2125
      t2135 = -0.90D2 * t4 * t7 * (-t1809 * t2123 + t1779 * t2126) - 0.1
     #80D3 * t977 * t1816 * t2126
      t2139 = FJET(XB1, XB2, s, -t1773, -t1764, t1763, t1753, -t953, t21
     #35 * t102 * t278 / 0.720D3)
      t2143 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t2145 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t237, -t1024,
     # x4)
      t2146 = t1805 * t2145
      t2155 = -0.90D2 * t4 * t7 * (-t1809 * t2143 + t1779 * t2146) - 0.1
     #80D3 * t977 * t1816 * t2146
      t2159 = FJET(XB1, XB2, s, -t1764, -t1773, t1753, t1763, -t953, t21
     #55 * t102 * t278 / 0.720D3)
      rrqg2qght6s2e1 = t422 * t421 + t679 * t678 + t936 * t935 + t1017 *
     # t1016 + t1086 * t1085 + t1245 * t1244 + t1345 * t1344 + t1382 * t
     #1381 + t1639 * t1638 + t1675 * t1674 + t1711 * t1710 + t1748 * t17
     #47 + t1824 * t1820 * t1827 / 0.720D3 + t1846 * t1842 * t1827 / 0.7
     #20D3 + t1948 * t1947 + t1984 * t1983 + t2084 * t2083 + t2121 * t21
     #20 + t2139 * t2135 * t1827 / 0.720D3 + t2159 * t2155 * t1827 / 0.7
     #20D3

      end function



      doubleprecision function rrqg2qght6s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = -0.1D1 + x3
      t21 = 0.1D1 / t20
      t22 = t19 * t21
      t25 = log(-0.4D1 * t13 * t22)
      t26 = t25 ** 2
      t27 = cos(t14)
      t28 = x3 * z
      t30 = Sqrt(-t28 * t20)
      t34 = 0.1D1 / (-z - x3 + 0.2D1 * t27 * t30)
      t38 = log(0.4D1 * t13 * t19)
      t39 = t38 ** 2
      t40 = 0.1D1 / z
      t43 = -t26 * t34 / 0.2D1 - t39 * t40 / 0.2D1
      t47 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t51 = 0.3141592653589793D1 * lh
      t52 = t3 * t7
      t53 = t52 * t8
      t59 = t25 * t34 + t38 * t40
      t61 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t68 = lh ** 2
      t70 = 0.3141592653589793D1 ** 2
      t72 = -0.180D3 * t68 + 0.30D2 * t70
      t73 = 0.3141592653589793D1 * t72
      t76 = -t34 - t40
      t79 = 0.1D1 / x3
      t82 = t12 * t16
      t83 = t82 * t18
      t85 = log(0.4D1 * t83)
      t87 = t85 ** 2
      t94 = t8 * t40
      t100 = 0.3141592653589793D1 * (-0.60D2 * lh * t70 + 0.288493656758
     #3026D3 + 0.120D3 * t68 * lh)
      t104 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t105 = t87 * t85
      t121 = x2 ** 2
      t122 = x3 * t121
      t125 = log(0.4D1 * t122 * t83)
      t129 = t122 * t12
      t132 = log(-0.4D1 * t129 * t22)
      t136 = 0.1D1 - x2
      t137 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t138 = t40 * t137
      t139 = -t136
      t140 = t19 * t139
      t143 = log(-0.4D1 * t129 * t140)
      t144 = t143 * t40
      t145 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t151 = t8 * t34
      t152 = t40 * t145
      t153 = t151 + t94 - t152
      t159 = 0.1D1 / x2
      t162 = 0.3141592653589793D1 * t40
      t163 = t12 * t121
      t166 = log(-0.4D1 * t163 * t140)
      t168 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t169 = t166 ** 2
      t174 = log(0.4D1 * t163 * t19)
      t176 = t174 ** 2
      t183 = t162 * lh
      t190 = t162 * t72
      t197 = x1 ** 2
      t198 = x3 * t197
      t201 = log(0.4D1 * t198 * t83)
      t205 = t198 * t16
      t206 = t12 * t18
      t210 = log(-0.4D1 * t205 * t206 * t21)
      t224 = 0.1D1 / x1
      t227 = t4 * t7
      t230 = t159 * t224
      t234 = t121 * t197
      t235 = t234 * t16
      t239 = log(-0.4D1 * t235 * t206 * t139)
      t240 = t239 * t40
      t244 = log(0.4D1 * t234 * t83)
      t260 = t197 * t16
      t263 = log(0.4D1 * t260 * t206)
      t265 = t263 ** 2
      t281 = -(0.90D2 * t4 * t7 * t8 * t43 + (0.90D2 * t4 * t7 * t47 - 0
     #.180D3 * t51 * t53) * t59 + (0.90D2 * t4 * t7 * t61 - 0.180D3 * t5
     #1 * t52 * t47 - t73 * t53) * t76) * t79 / 0.2880D4 + (0.180D3 * (t
     #85 * t47 - t61 - t87 * t8 / 0.2D1) * t40 * t51 - t94 * t100 - 0.90
     #D2 * (-t87 * t47 / 0.2D1 - t104 + t105 * t8 / 0.6D1 + t85 * t61) *
     # t40 * 0.3141592653589793D1 + (-t47 + t85 * t8) * t40 * t73) * t3 
     #* t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t47 + t125 * t8) * t40 
     #- (-t47 + t132 * t8) * t34 - t138 + t144 * t145) + 0.180D3 * t51 *
     # t52 * t153) * t79 * t159 / 0.1440D4 + (-0.90D2 * t162 * t52 * (-t
     #166 * t137 + t168 + t169 * t145 / 0.2D1 + t174 * t47 - t61 - t176 
     #* t8 / 0.2D1) + 0.180D3 * t183 * t52 * (-t47 + t137 - t166 * t145 
     #+ t174 * t8) + t190 * t52 * (t145 - t8)) * t159 / 0.1440D4 - (-0.9
     #0D2 * t4 * t7 * (-(-t47 + t201 * t8) * t40 - (-t47 + t210 * t8) * 
     #t34) + 0.180D3 * t51 * t52 * (t151 + t94)) * t79 * t224 / 0.1440D4
     # + t227 * t153 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 * (t138 -
     # t240 * t145 + (-t47 + t244 * t8) * t40) + 0.180D3 * t51 * t52 * (
     #-t94 + t152)) * t159 * t224 / 0.720D3 + (-0.90D2 * t162 * t52 * (t
     #263 * t47 - t61 - t265 * t8 / 0.2D1) + 0.180D3 * t183 * t52 * (-t4
     #7 + t263 * t8) - t190 * t53) * t224 / 0.1440D4
      t282 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t281)
      t284 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t289 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t293 = t52 * t284
      t298 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t318 = t284 * t40
      t320 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t344 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t345 = t40 * t344
      t346 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t352 = t284 * t34
      t353 = t40 * t346
      t354 = t352 + t318 - t353
      t367 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t440 = -(0.90D2 * t4 * t7 * t284 * t43 + (0.90D2 * t4 * t7 * t289 
     #- 0.180D3 * t51 * t293) * t59 + (0.90D2 * t4 * t7 * t298 - 0.180D3
     # * t51 * t52 * t289 - t73 * t293) * t76) * t79 / 0.2880D4 + (0.180
     #D3 * (-t87 * t284 / 0.2D1 - t298 + t85 * t289) * t40 * t51 - t318 
     #* t100 - 0.90D2 * (-t320 - t87 * t289 / 0.2D1 + t105 * t284 / 0.6D
     #1 + t85 * t298) * t40 * 0.3141592653589793D1 + (-t289 + t85 * t284
     #) * t40 * t73) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t28
     #9 + t125 * t284) * t40 - (-t289 + t132 * t284) * t34 - t345 + t144
     # * t346) + 0.180D3 * t51 * t52 * t354) * t79 * t159 / 0.1440D4 + (
     #-0.90D2 * t162 * t52 * (-t166 * t344 - t176 * t284 / 0.2D1 - t298 
     #+ t169 * t346 / 0.2D1 + t367 + t174 * t289) + 0.180D3 * t183 * t52
     # * (-t166 * t346 + t344 - t289 + t174 * t284) + t190 * t52 * (t346
     # - t284)) * t159 / 0.1440D4 - (-0.90D2 * t4 * t7 * (-(-t289 + t201
     # * t284) * t40 - (-t289 + t210 * t284) * t34) + 0.180D3 * t51 * t5
     #2 * (t352 + t318)) * t79 * t224 / 0.1440D4 + t227 * t354 * t79 * t
     #230 / 0.8D1 + (-0.90D2 * t4 * t7 * (t345 - t240 * t346 + (-t289 + 
     #t244 * t284) * t40) + 0.180D3 * t51 * t52 * (t353 - t318)) * t159 
     #* t224 / 0.720D3 + (-0.90D2 * t162 * t52 * (-t265 * t284 / 0.2D1 -
     # t298 + t263 * t289) + 0.180D3 * t183 * t52 * (-t289 + t263 * t284
     #) - t190 * t293) * t224 / 0.1440D4
      t441 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t440)
      t443 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t448 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t452 = t52 * t443
      t457 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t477 = t443 * t40
      t483 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t503 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t504 = t40 * t503
      t505 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t511 = t443 * t34
      t512 = t40 * t505
      t513 = t511 + t477 - t512
      t521 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t599 = -(0.90D2 * t4 * t7 * t443 * t43 + (0.90D2 * t4 * t7 * t448 
     #- 0.180D3 * t51 * t452) * t59 + (0.90D2 * t4 * t7 * t457 - 0.180D3
     # * t51 * t52 * t448 - t73 * t452) * t76) * t79 / 0.2880D4 + (0.180
     #D3 * (-t87 * t443 / 0.2D1 - t457 + t85 * t448) * t40 * t51 - t477 
     #* t100 - 0.90D2 * (-t87 * t448 / 0.2D1 + t105 * t443 / 0.6D1 - t48
     #3 + t85 * t457) * t40 * 0.3141592653589793D1 + (-t448 + t85 * t443
     #) * t40 * t73) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t44
     #8 + t125 * t443) * t40 - (-t448 + t132 * t443) * t34 - t504 + t144
     # * t505) + 0.180D3 * t51 * t52 * t513) * t79 * t159 / 0.1440D4 + (
     #-0.90D2 * t162 * t52 * (t521 + t169 * t505 / 0.2D1 - t166 * t503 -
     # t457 - t176 * t443 / 0.2D1 + t174 * t448) + 0.180D3 * t183 * t52 
     #* (-t166 * t505 + t503 - t448 + t174 * t443) + t190 * t52 * (t505 
     #- t443)) * t159 / 0.1440D4 - (-0.90D2 * t4 * t7 * (-(-t448 + t210 
     #* t443) * t34 - (-t448 + t201 * t443) * t40) + 0.180D3 * t51 * t52
     # * (t511 + t477)) * t79 * t224 / 0.1440D4 + t227 * t513 * t79 * t2
     #30 / 0.8D1 + (-0.90D2 * t4 * t7 * (t504 - t240 * t505 + (-t448 + t
     #244 * t443) * t40) + 0.180D3 * t51 * t52 * (t512 - t477)) * t159 *
     # t224 / 0.720D3 + (-0.90D2 * t162 * t52 * (-t265 * t443 / 0.2D1 - 
     #t457 + t263 * t448) + 0.180D3 * t183 * t52 * (-t448 + t263 * t443)
     # - t190 * t452) * t224 / 0.1440D4
      t600 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t599)
      t603 = x1 * z
      t604 = -z - x1 + t603
      t605 = 0.1D1 / t604
      t607 = t2 * x1 * t139 * t605
      t608 = -0.1D1 + x1
      t609 = t2 * t608
      t612 = x2 * s * t1 * x1
      t613 = s * t17
      t616 = x1 * t608 * t605
      t617 = t613 * t139 * t616
      t618 = x2 * x1
      t619 = t618 * z
      t621 = 0.1D1 / (-z + t619 - t618)
      t622 = t7 * t621
      t623 = t4 * t622
      t624 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1, 
     #x4)
      t629 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1, 
     #x4)
      t631 = 0.1D1 / t10
      t632 = t16 * t631
      t634 = t18 * t605
      t635 = t608 ** 2
      t640 = log(0.4D1 * t234 * t632 * t634 * t635 * t139)
      t641 = t640 * t621
      t647 = t51 * t3
      t655 = -t623 * t624 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 * (t
     #621 * t629 - t641 * t624) + 0.180D3 * t647 * t622 * t624) * t159 *
     # t224 / 0.720D3
      t656 = FJET(XB1, XB2, s, 0.0D0, t607, -t609, t612, -t617, t655)
      t658 = x2 * x3
      t659 = 0.1D1 - x3 + t658
      t660 = 0.1D1 / t659
      t661 = t658 * t660
      t662 = t2 * t661
      t663 = t20 * t660
      t664 = t2 * t663
      t667 = t659 ** 2
      t673 = log(0.4D1 * t122 * t82 * t18 * t139 * t20 / t667)
      t674 = t139 * t20
      t676 = Sqrt(t28 * t674)
      t680 = 0.1D1 / (-z - x3 + t658 + 0.2D1 * t27 * t676)
      t681 = t673 * t680
      t682 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t663
     #, x4)
      t684 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t663
     #, x4)
      t690 = t7 * t680
      t698 = t4 * t690
      t703 = -(-0.90D2 * t4 * t7 * (t681 * t682 - t680 * t684) - 0.180D3
     # * t647 * t690 * t682) * t79 * t159 / 0.1440D4 - t698 * t682 * t79
     # * t230 / 0.8D1
      t704 = FJET(XB1, XB2, s, 0.0D0, t662, 0.0D0, -t664, 0.0D0, t703)
      t707 = t2 * x1 * t605
      t708 = t613 * t616
      t709 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t710 = t40 * t709
      t717 = log(0.4D1 * t198 * t632 * t634 * t635 * t21)
      t718 = t717 * t604
      t719 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t722 = x3 * x1
      t723 = t722 * z
      t724 = t198 * t10
      t725 = x1 * t10
      t726 = x3 * t10
      t727 = t726 * x1
      t729 = 0.2D1 * t198 * z
      t730 = x3 * t604
      t732 = Sqrt(t730 * t20)
      t737 = 0.1D1 / (-t603 - t723 - t198 - t724 - t28 + t725 + t727 + t
     #729 - t10 + 0.2D1 * t27 * t732 * z)
      t741 = t631 * t18 * t605 * t635
      t744 = log(-0.4D1 * t205 * t741)
      t745 = t744 * t40
      t754 = -t40 * t719 + t604 * t719 * t737
      t769 = log(-0.4D1 * t235 * t741)
      t770 = t769 * t40
      t776 = t7 * t40
      t777 = t776 * t719
      t788 = log(-0.4D1 * t260 * t631 * t634 * t635)
      t789 = t788 ** 2
      t792 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t803 = t73 * t3
      t808 = -(-0.90D2 * t4 * t7 * (-t710 + (t604 * t709 - t718 * t719) 
     #* t737 + t745 * t719) + 0.180D3 * t51 * t52 * t754) * t79 * t224 /
     # 0.1440D4 + t227 * t754 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 
     #* (-t770 * t719 + t710) + 0.180D3 * t647 * t777) * t159 * t224 / 0
     #.720D3 + (-0.90D2 * t162 * t52 * (t789 * t719 / 0.2D1 + t792 - t78
     #8 * t709) + 0.180D3 * t183 * t52 * (t709 - t788 * t719) + t803 * t
     #777) * t224 / 0.1440D4
      t809 = FJET(XB1, XB2, s, 0.0D0, -t609, -t707, 0.0D0, t708, t808)
      t811 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t813 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t818 = t40 * t811
      t826 = t604 * t813 * t737 - t40 * t813
      t844 = t776 * t813
      t851 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t868 = -(-0.90D2 * t4 * t7 * ((t604 * t811 - t718 * t813) * t737 +
     # t745 * t813 - t818) + 0.180D3 * t51 * t52 * t826) * t79 * t224 / 
     #0.1440D4 + t227 * t826 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 *
     # (-t770 * t813 + t818) + 0.180D3 * t647 * t844) * t159 * t224 / 0.
     #720D3 + (-0.90D2 * t162 * t52 * (t851 + t789 * t813 / 0.2D1 - t788
     # * t811) + 0.180D3 * t183 * t52 * (-t788 * t813 + t811) + t803 * t
     #844) * t224 / 0.1440D4
      t869 = FJET(XB1, XB2, s, 0.0D0, -t707, -t609, 0.0D0, t708, t868)
      t871 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t663
     #, x4)
      t873 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t663
     #, x4)
      t890 = -(-0.90D2 * t4 * t7 * (t681 * t871 - t680 * t873) - 0.180D3
     # * t647 * t690 * t871) * t79 * t159 / 0.1440D4 - t698 * t871 * t79
     # * t230 / 0.8D1
      t891 = FJET(XB1, XB2, s, 0.0D0, -t664, 0.0D0, t662, 0.0D0, t890)
      t893 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t895 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t896 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t903 = t896 * t40
      t909 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t926 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t927 = t40 * t926
      t931 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t937 = t896 * t34
      t938 = t40 * t931
      t939 = t937 + t903 - t938
      t947 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t977 = t52 * t896
      t1049 = (0.180D3 * (t85 * t893 - t895 - t87 * t896 / 0.2D1) * t40 
     #* t51 - t903 * t100 - 0.90D2 * (-t87 * t893 / 0.2D1 + t105 * t896 
     #/ 0.6D1 - t909 + t85 * t895) * t40 * 0.3141592653589793D1 + (-t893
     # + t85 * t896) * t40 * t73) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 *
     # t7 * (-(-t893 + t132 * t896) * t34 - t927 - (-t893 + t125 * t896)
     # * t40 + t144 * t931) + 0.180D3 * t51 * t52 * t939) * t79 * t159 /
     # 0.1440D4 + (-0.90D2 * t162 * t52 * (t947 - t176 * t896 / 0.2D1 + 
     #t169 * t931 / 0.2D1 - t166 * t926 - t895 + t174 * t893) + 0.180D3 
     #* t183 * t52 * (-t166 * t931 + t174 * t896 - t893 + t926) + t190 *
     # t52 * (t931 - t896)) * t159 / 0.1440D4 - (0.90D2 * t4 * t7 * t896
     # * t43 + (0.90D2 * t4 * t7 * t893 - 0.180D3 * t51 * t977) * t59 + 
     #(0.90D2 * t4 * t7 * t895 - 0.180D3 * t51 * t52 * t893 - t73 * t977
     #) * t76) * t79 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t893 + t210 *
     # t896) * t34 - (-t893 + t201 * t896) * t40) + 0.180D3 * t51 * t52 
     #* (t903 + t937)) * t79 * t224 / 0.1440D4 + t227 * t939 * t79 * t23
     #0 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t240 * t931 + t927 + (-t893 + t
     #244 * t896) * t40) + 0.180D3 * t51 * t52 * (t938 - t903)) * t159 *
     # t224 / 0.720D3 + (-0.90D2 * t162 * t52 * (t263 * t893 - t265 * t8
     #96 / 0.2D1 - t895) + 0.180D3 * t183 * t52 * (t263 * t896 - t893) -
     # t190 * t977) * t224 / 0.1440D4
      t1050 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1049)
      t1052 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1058 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1071 = -t623 * t1052 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 * 
     #(-t641 * t1052 + t621 * t1058) + 0.180D3 * t647 * t622 * t1052) * 
     #t159 * t224 / 0.720D3
      t1072 = FJET(XB1, XB2, s, t612, -t609, t607, 0.0D0, -t617, t1071)
      t1074 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1079 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1093 = -t623 * t1074 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 * 
     #(t621 * t1079 - t641 * t1074) + 0.180D3 * t647 * t622 * t1074) * t
     #159 * t224 / 0.720D3
      t1094 = FJET(XB1, XB2, s, t607, 0.0D0, t612, -t609, -t617, t1093)
      t1096 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t66
     #3, x4)
      t1102 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t66
     #3, x4)
      t1115 = -t698 * t1096 * t79 * t230 / 0.8D1 - (-0.90D2 * t4 * t7 * 
     #(t681 * t1096 - t680 * t1102) - 0.180D3 * t647 * t690 * t1096) * t
     #79 * t159 / 0.1440D4
      t1116 = FJET(XB1, XB2, s, t662, 0.0D0, -t664, 0.0D0, 0.0D0, t1115)
      t1121 = t20 * s * t1 * t608 * t660
      t1122 = t2 * x1
      t1124 = Sqrt(-t730 * t674)
      t1125 = t27 * t1124
      t1131 = t1122 * x2 * (-x3 + t658 - z + t28 - x1 + t722 + t603 - t7
     #23 + 0.2D1 * t1125) * t605 * t660
      t1132 = t609 * t661
      t1135 = t122 * x1
      t1137 = t122 * t603
      t1141 = t1122 * (0.2D1 * t1125 * x2 + t1135 + t122 * z - x2 + t658
     # - t1137 + 0.1D1 - x3) * t605 * t660
      t1143 = t4 * t7 * t604
      t1150 = x2 * t197
      t1154 = -t1135 - 0.2D1 * t1125 * z - t658 * z + t658 * x1 - t198 *
     # x2 - t618 * t10 - 0.2D1 * t1150 * z + t1150 * t10 + t619 + t723 +
     # t724 - t727 - t729
      t1167 = t1137 - 0.2D1 * t658 * t603 + t726 * t618 + 0.2D1 * t198 *
     # x2 * z - t198 * t10 * x2 - 0.2D1 * t1125 * t618 + t10 + 0.2D1 * t
     #1125 * t619 + t1150 + t198 + t28 - t725 + t603
      t1169 = 0.1D1 / (t1154 + t1167)
      t1170 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t663, 
     #x4)
      t1173 = t79 * t159 * t224
      t1174 = t1169 * t1170 * t1173
      t1177 = FJET(XB1, XB2, s, t1121, t1131, -t1132, -t1141, -t617, t11
     #43 * t1174 / 0.8D1)
      t1179 = t52 * t604
      t1183 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t663, 
     #x4)
      t1185 = t1169 * t1183 * t1173
      t1188 = FJET(XB1, XB2, s, t1131, t1121, -t1141, -t1132, -t617, t11
     #43 * t1185 / 0.8D1)
      t1193 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1195 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1200 = t40 * t1193
      t1208 = t604 * t1195 * t737 - t40 * t1195
      t1226 = t776 * t1195
      t1233 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1250 = -(-0.90D2 * t4 * t7 * ((t604 * t1193 - t718 * t1195) * t73
     #7 + t745 * t1195 - t1200) + 0.180D3 * t51 * t52 * t1208) * t79 * t
     #224 / 0.1440D4 + t227 * t1208 * t79 * t230 / 0.8D1 + (-0.90D2 * t4
     # * t7 * (t1200 - t770 * t1195) + 0.180D3 * t647 * t1226) * t159 * 
     #t224 / 0.720D3 + (-0.90D2 * t162 * t52 * (t1233 + t789 * t1195 / 0
     #.2D1 - t788 * t1193) + 0.180D3 * t183 * t52 * (t1193 - t788 * t119
     #5) + t803 * t1226) * t224 / 0.1440D4
      t1251 = FJET(XB1, XB2, s, -t609, 0.0D0, 0.0D0, -t707, t708, t1250)
      t1253 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1258 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1272 = -t623 * t1253 * t79 * t230 / 0.8D1 + (-0.90D2 * t4 * t7 * 
     #(t621 * t1258 - t641 * t1253) + 0.180D3 * t647 * t622 * t1253) * t
     #159 * t224 / 0.720D3
      t1273 = FJET(XB1, XB2, s, -t609, t612, 0.0D0, t607, -t617, t1272)
      t1275 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1277 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1282 = t40 * t1275
      t1290 = t604 * t1277 * t737 - t40 * t1277
      t1308 = t776 * t1277
      t1316 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1332 = -(-0.90D2 * t4 * t7 * ((t604 * t1275 - t718 * t1277) * t73
     #7 + t745 * t1277 - t1282) + 0.180D3 * t51 * t52 * t1290) * t79 * t
     #224 / 0.1440D4 + t227 * t1290 * t79 * t230 / 0.8D1 + (-0.90D2 * t4
     # * t7 * (t1282 - t770 * t1277) + 0.180D3 * t647 * t1308) * t159 * 
     #t224 / 0.720D3 + (-0.90D2 * t162 * t52 * (-t788 * t1275 + t1316 + 
     #t789 * t1277 / 0.2D1) + 0.180D3 * t183 * t52 * (t1275 - t788 * t12
     #77) + t803 * t1308) * t224 / 0.1440D4
      t1333 = FJET(XB1, XB2, s, -t707, 0.0D0, 0.0D0, -t609, t708, t1332)
      t1335 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t66
     #3, x4)
      t1341 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t66
     #3, x4)
      t1354 = -t698 * t1335 * t79 * t230 / 0.8D1 - (-0.90D2 * t4 * t7 * 
     #(t681 * t1335 - t680 * t1341) - 0.180D3 * t647 * t690 * t1335) * t
     #79 * t159 / 0.1440D4
      t1355 = FJET(XB1, XB2, s, -t664, 0.0D0, t662, 0.0D0, 0.0D0, t1354)
      t1357 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t663, 
     #x4)
      t1359 = t1169 * t1357 * t1173
      t1362 = FJET(XB1, XB2, s, -t1141, -t1132, t1131, t1121, -t617, t11
     #43 * t1359 / 0.8D1)
      t1367 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t663, 
     #x4)
      t1369 = t1169 * t1367 * t1173
      t1372 = FJET(XB1, XB2, s, -t1132, -t1141, t1121, t1131, -t617, t11
     #43 * t1369 / 0.8D1)
      rrqg2qght6s2e0 = t282 * t281 + t441 * t440 + t600 * t599 + t656 * 
     #t655 + t704 * t703 + t809 * t808 + t869 * t868 + t891 * t890 + t10
     #50 * t1049 + t1072 * t1071 + t1094 * t1093 + t1116 * t1115 + t1177
     # * 0.3141592653589793D1 * t1179 * t1174 / 0.8D1 + t1188 * 0.314159
     #2653589793D1 * t1179 * t1185 / 0.8D1 + t1251 * t1250 + t1273 * t12
     #72 + t1333 * t1332 + t1355 * t1354 + t1362 * 0.3141592653589793D1 
     #* t1179 * t1359 / 0.8D1 + t1372 * 0.3141592653589793D1 * t1179 * t
     #1369 / 0.8D1

      end function



      doubleprecision function rrqg2qght6s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = -0.1D1 + x3
      t25 = log(-0.4D1 * t13 * t19 / t20)
      t26 = cos(t14)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t20)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t13 * t19)
      t38 = 0.1D1 / z
      t40 = t25 * t33 + t37 * t38
      t44 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t48 = 0.3141592653589793D1 * lh
      t49 = t3 * t7
      t50 = t49 * t8
      t54 = -t33 - t38
      t57 = 0.1D1 / x3
      t63 = log(0.4D1 * t12 * t16 * t18)
      t70 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t71 = t63 ** 2
      t78 = t8 * t38
      t79 = lh ** 2
      t81 = 0.3141592653589793D1 ** 2
      t84 = 0.3141592653589793D1 * (-0.180D3 * t79 + 0.30D2 * t81)
      t90 = t4 * t7
      t91 = t8 * t33
      t92 = 0.1D1 - x2
      t93 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1,
     # x4)
      t94 = t38 * t93
      t97 = 0.1D1 / x2
      t101 = 0.3141592653589793D1 * t38
      t102 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t103 = x2 ** 2
      t104 = t12 * t103
      t105 = -t92
      t109 = log(-0.4D1 * t104 * t19 * t105)
      t113 = log(0.4D1 * t104 * t19)
      t119 = t101 * lh
      t129 = 0.1D1 / x1
      t133 = x1 ** 2
      t134 = t133 * t16
      t138 = log(0.4D1 * t134 * t12 * t18)
      t154 = -(0.90D2 * t4 * t7 * t8 * t40 + (0.90D2 * t4 * t7 * t44 - 0
     #.180D3 * t48 * t50) * t54) * t57 / 0.2880D4 + (0.180D3 * (-t44 + t
     #63 * t8) * t38 * t48 - 0.90D2 * (t63 * t44 - t70 - t71 * t8 / 0.2D
     #1) * t38 * 0.3141592653589793D1 - t78 * t84) * t3 * t7 / 0.2880D4 
     #+ t90 * (t91 + t78 - t94) * t57 * t97 / 0.16D2 + (-0.90D2 * t101 *
     # t49 * (-t44 + t102 - t109 * t93 + t113 * t8) + 0.180D3 * t119 * t
     #49 * (t93 - t8)) * t97 / 0.1440D4 - t90 * (-t78 + t94) * t97 * t12
     #9 / 0.8D1 + (-0.90D2 * t101 * t49 * (-t44 + t138 * t8) - 0.180D3 *
     # t119 * t50) * t129 / 0.1440D4 + t90 * (t91 + t78) * t57 * t129 / 
     #0.16D2
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t154)
      t157 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t162 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t166 = t49 * t157
      t181 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t187 = t157 * t38
      t193 = t157 * t33
      t194 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t195 = t38 * t194
      t202 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t235 = -(0.90D2 * t4 * t7 * t157 * t40 + (0.90D2 * t4 * t7 * t162 
     #- 0.180D3 * t48 * t166) * t54) * t57 / 0.2880D4 + (0.180D3 * (-t16
     #2 + t63 * t157) * t38 * t48 - 0.90D2 * (-t71 * t157 / 0.2D1 - t181
     # + t63 * t162) * t38 * 0.3141592653589793D1 - t187 * t84) * t3 * t
     #7 / 0.2880D4 + t90 * (t193 + t187 - t195) * t57 * t97 / 0.16D2 + (
     #-0.90D2 * t101 * t49 * (-t109 * t194 + t202 - t162 + t113 * t157) 
     #+ 0.180D3 * t119 * t49 * (t194 - t157)) * t97 / 0.1440D4 - t90 * (
     #t195 - t187) * t97 * t129 / 0.8D1 + (-0.90D2 * t101 * t49 * (-t162
     # + t138 * t157) - 0.180D3 * t119 * t166) * t129 / 0.1440D4 + t90 *
     # (t193 + t187) * t57 * t129 / 0.16D2
      t236 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t235)
      t238 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t243 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t247 = t49 * t238
      t262 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t268 = t238 * t38
      t274 = t238 * t33
      t275 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t276 = t38 * t275
      t283 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t316 = -(0.90D2 * t4 * t7 * t238 * t40 + (0.90D2 * t4 * t7 * t243 
     #- 0.180D3 * t48 * t247) * t54) * t57 / 0.2880D4 + (0.180D3 * (-t24
     #3 + t63 * t238) * t38 * t48 - 0.90D2 * (-t71 * t238 / 0.2D1 - t262
     # + t63 * t243) * t38 * 0.3141592653589793D1 - t268 * t84) * t3 * t
     #7 / 0.2880D4 + t90 * (t274 + t268 - t276) * t57 * t97 / 0.16D2 + (
     #-0.90D2 * t101 * t49 * (-t109 * t275 + t283 - t243 + t113 * t238) 
     #+ 0.180D3 * t119 * t49 * (t275 - t238)) * t97 / 0.1440D4 - t90 * (
     #t276 - t268) * t97 * t129 / 0.8D1 + (-0.90D2 * t101 * t49 * (-t243
     # + t138 * t238) - 0.180D3 * t119 * t247) * t129 / 0.1440D4 + t90 *
     # (t274 + t268) * t57 * t129 / 0.16D2
      t317 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t316)
      t320 = x1 * z
      t321 = -z - x1 + t320
      t322 = 0.1D1 / t321
      t324 = t2 * x1 * t105 * t322
      t325 = -0.1D1 + x1
      t326 = t2 * t325
      t329 = x2 * s * t1 * x1
      t330 = s * t17
      t333 = x1 * t325 * t322
      t334 = t330 * t105 * t333
      t335 = x2 * x1
      t338 = 0.1D1 / (-z + t335 * z - t335)
      t339 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t92, 0.10D1, x
     #4)
      t341 = t97 * t129
      t342 = t338 * t339 * t341
      t345 = FJET(XB1, XB2, s, 0.0D0, t324, -t326, t329, -t334, -t90 * t
     #342 / 0.8D1)
      t350 = x2 * x3
      t352 = 0.1D1 / (0.1D1 - x3 + t350)
      t354 = t2 * t350 * t352
      t355 = t20 * t352
      t356 = t2 * t355
      t359 = Sqrt(t27 * t105 * t20)
      t363 = 0.1D1 / (-z - x3 + t350 + 0.2D1 * t26 * t359)
      t364 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, -t355,
     # x4)
      t366 = t57 * t97
      t367 = t363 * t364 * t366
      t370 = FJET(XB1, XB2, s, 0.0D0, t354, 0.0D0, -t356, 0.0D0, -t90 * 
     #t367 / 0.16D2)
      t376 = t2 * x1 * t322
      t377 = t330 * t333
      t378 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t379 = t38 * t378
      t383 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t387 = t325 ** 2
      t391 = log(-0.4D1 * t134 / t10 * t18 * t322 * t387)
      t397 = t48 * t3
      t398 = t7 * t38
      t408 = x3 * t133
      t417 = Sqrt(x3 * t321 * t20)
      t422 = 0.1D1 / (-t320 - x3 * x1 * z - t408 - t408 * t10 - t27 + x1
     # * t10 + x3 * t10 * x1 + 0.2D1 * t408 * z - t10 + 0.2D1 * t26 * t4
     #17 * z)
      t429 = -t90 * t379 * t341 / 0.8D1 + (-0.90D2 * t101 * t49 * (t383 
     #- t391 * t378) + 0.180D3 * t397 * t398 * t378) * t129 / 0.1440D4 +
     # t90 * (-t379 + t321 * t378 * t422) * t57 * t129 / 0.16D2
      t430 = FJET(XB1, XB2, s, 0.0D0, -t326, -t376, 0.0D0, t377, t429)
      t432 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t433 = t38 * t432
      t438 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t456 = -t90 * t433 * t341 / 0.8D1 + (-0.90D2 * t101 * t49 * (-t391
     # * t432 + t438) + 0.180D3 * t397 * t398 * t432) * t129 / 0.1440D4 
     #+ t90 * (t321 * t432 * t422 - t433) * t57 * t129 / 0.16D2
      t457 = FJET(XB1, XB2, s, 0.0D0, -t376, -t326, 0.0D0, t377, t456)
      t459 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, -t355,
     # x4)
      t461 = t363 * t459 * t366
      t464 = FJET(XB1, XB2, s, 0.0D0, -t356, 0.0D0, t354, 0.0D0, -t90 * 
     #t461 / 0.16D2)
      t469 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t470 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t477 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t484 = t470 * t38
      t490 = t470 * t33
      t491 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t492 = t38 * t491
      t500 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, 0.10D1
     #, x4)
      t519 = t49 * t470
      t547 = (0.180D3 * (-t469 + t63 * t470) * t38 * t48 - 0.90D2 * (t63
     # * t469 - t477 - t71 * t470 / 0.2D1) * t38 * 0.3141592653589793D1 
     #- t484 * t84) * t3 * t7 / 0.2880D4 + t90 * (t490 + t484 - t492) * 
     #t57 * t97 / 0.16D2 + (-0.90D2 * t101 * t49 * (-t109 * t491 + t113 
     #* t470 - t469 + t500) + 0.180D3 * t119 * t49 * (t491 - t470)) * t9
     #7 / 0.1440D4 - (0.90D2 * t4 * t7 * t470 * t40 + (0.90D2 * t4 * t7 
     #* t469 - 0.180D3 * t48 * t519) * t54) * t57 / 0.2880D4 - t90 * (t4
     #92 - t484) * t97 * t129 / 0.8D1 + (-0.90D2 * t101 * t49 * (t138 * 
     #t470 - t469) - 0.180D3 * t119 * t519) * t129 / 0.1440D4 + t90 * (t
     #484 + t490) * t57 * t129 / 0.16D2
      t548 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t547)
      t550 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t92, 0.10D1, x
     #4)
      t552 = t338 * t550 * t341
      t555 = FJET(XB1, XB2, s, t329, -t326, t324, 0.0D0, -t334, -t90 * t
     #552 / 0.8D1)
      t560 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t92, 0.10D1, x
     #4)
      t562 = t338 * t560 * t341
      t565 = FJET(XB1, XB2, s, t324, 0.0D0, t329, -t326, -t334, -t90 * t
     #562 / 0.8D1)
      t570 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, -t355,
     # x4)
      t572 = t363 * t570 * t366
      t575 = FJET(XB1, XB2, s, t354, 0.0D0, -t356, 0.0D0, 0.0D0, -t90 * 
     #t572 / 0.16D2)
      t580 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t581 = t38 * t580
      t585 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t604 = -t90 * t581 * t341 / 0.8D1 + (-0.90D2 * t101 * t49 * (t585 
     #- t391 * t580) + 0.180D3 * t397 * t398 * t580) * t129 / 0.1440D4 +
     # t90 * (t321 * t580 * t422 - t581) * t57 * t129 / 0.16D2
      t605 = FJET(XB1, XB2, s, -t326, 0.0D0, 0.0D0, -t376, t377, t604)
      t607 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t92, 0.10D1, x
     #4)
      t609 = t338 * t607 * t341
      t612 = FJET(XB1, XB2, s, -t326, t329, 0.0D0, t324, -t334, -t90 * t
     #609 / 0.8D1)
      t617 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t618 = t38 * t617
      t622 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t641 = -t90 * t618 * t341 / 0.8D1 + (-0.90D2 * t101 * t49 * (t622 
     #- t391 * t617) + 0.180D3 * t397 * t398 * t617) * t129 / 0.1440D4 +
     # t90 * (t321 * t617 * t422 - t618) * t57 * t129 / 0.16D2
      t642 = FJET(XB1, XB2, s, -t376, 0.0D0, 0.0D0, -t326, t377, t641)
      t644 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t92, -t355,
     # x4)
      t646 = t363 * t644 * t366
      t649 = FJET(XB1, XB2, s, -t356, 0.0D0, t354, 0.0D0, 0.0D0, -t90 * 
     #t646 / 0.16D2)
      rrqg2qght6s2em1 = t155 * t154 + t236 * t235 + t317 * t316 - t345 *
     # 0.3141592653589793D1 * t49 * t342 / 0.8D1 - t370 * 0.314159265358
     #9793D1 * t49 * t367 / 0.16D2 + t430 * t429 + t457 * t456 - t464 * 
     #0.3141592653589793D1 * t49 * t461 / 0.16D2 + t548 * t547 - t555 * 
     #0.3141592653589793D1 * t49 * t552 / 0.8D1 - t565 * 0.3141592653589
     #793D1 * t49 * t562 / 0.8D1 - t575 * 0.3141592653589793D1 * t49 * t
     #572 / 0.16D2 + t605 * t604 - t612 * 0.3141592653589793D1 * t49 * t
     #609 / 0.8D1 + t642 * t641 - t649 * 0.3141592653589793D1 * t49 * t6
     #46 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = 0.3141592653589793D1 * t3 * t7
      t9 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t15 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t20 = 0.1D1 / z
      t21 = -0.1D1 / (-z - x3 + 0.2D1 * t11 * t15) - t20
      t23 = 0.1D1 / x3
      t28 = 0.3141592653589793D1 * t20 * t3
      t29 = 0.1D1 - x2
      t30 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1,
     # x4)
      t33 = 0.1D1 / x2
      t38 = 0.1D1 / x1
      t43 = 0.3141592653589793D1 * lh
      t46 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t47 = z ** 2
      t50 = Sin(t10)
      t51 = t50 ** 2
      t53 = t1 ** 2
      t54 = t53 ** 2
      t57 = log(0.4D1 / t47 / z * t51 * t54)
      t67 = -t8 * t9 * t21 * t23 / 0.32D2 - t28 * t7 * (t30 - t9) * t33 
     #/ 0.16D2 + t28 * t7 * t9 * t38 / 0.16D2 + (-0.180D3 * t9 * t20 * t
     #43 - 0.90D2 * (-t46 + t57 * t9) * t20 * 0.3141592653589793D1) * t3
     # * t7 / 0.2880D4
      t68 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t67)
      t70 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t75 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1,
     # x4)
      t88 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t98 = -t8 * t70 * t21 * t23 / 0.32D2 - t28 * t7 * (t75 - t70) * t3
     #3 / 0.16D2 + t28 * t7 * t70 * t38 / 0.16D2 + (-0.180D3 * t70 * t20
     # * t43 - 0.90D2 * (-t88 + t57 * t70) * t20 * 0.3141592653589793D1)
     # * t3 * t7 / 0.2880D4
      t99 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t98)
      t101 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t106 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1
     #, x4)
      t119 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t129 = -t8 * t101 * t21 * t23 / 0.32D2 - t28 * t7 * (t106 - t101) 
     #* t33 / 0.16D2 + t28 * t7 * t101 * t38 / 0.16D2 + (-0.180D3 * t101
     # * t20 * t43 - 0.90D2 * (-t119 + t57 * t101) * t20 * 0.31415926535
     #89793D1) * t3 * t7 / 0.2880D4
      t130 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t129)
      t132 = -0.1D1 + x1
      t133 = t2 * t132
      t136 = 0.1D1 / (-z - x1 + x1 * z)
      t138 = t2 * x1 * t136
      t142 = s * t53 * x1 * t132 * t136
      t143 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t148 = FJET(XB1, XB2, s, 0.0D0, -t133, -t138, 0.0D0, t142, -t8 * t
     #20 * t143 * t38 / 0.16D2)
      t151 = t7 * t20
      t156 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t161 = FJET(XB1, XB2, s, 0.0D0, -t138, -t133, 0.0D0, t142, -t8 * t
     #20 * t156 * t38 / 0.16D2)
      t168 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t173 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1
     #, x4)
      t186 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t196 = -t8 * t168 * t21 * t23 / 0.32D2 - t28 * t7 * (t173 - t168) 
     #* t33 / 0.16D2 + t28 * t7 * t168 * t38 / 0.16D2 + (-0.180D3 * t168
     # * t20 * t43 - 0.90D2 * (-t186 + t57 * t168) * t20 * 0.31415926535
     #89793D1) * t3 * t7 / 0.2880D4
      t197 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t196)
      t199 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t204 = FJET(XB1, XB2, s, -t133, 0.0D0, 0.0D0, -t138, t142, -t8 * t
     #20 * t199 * t38 / 0.16D2)
      t211 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t216 = FJET(XB1, XB2, s, -t138, 0.0D0, 0.0D0, -t133, t142, -t8 * t
     #20 * t211 * t38 / 0.16D2)
      rrqg2qght6s2em2 = t68 * t67 + t99 * t98 + t130 * t129 - t148 * 0.3
     #141592653589793D1 * t3 * t151 * t143 * t38 / 0.16D2 - t161 * 0.314
     #1592653589793D1 * t3 * t151 * t156 * t38 / 0.16D2 + t197 * t196 - 
     #t204 * 0.3141592653589793D1 * t3 * t151 * t199 * t38 / 0.16D2 - t2
     #16 * 0.3141592653589793D1 * t3 * t151 * t211 * t38 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = 0.3141592653589793D1 * t3
      t6 = s ** 2
      t9 = 0.1D1 / t1 / t6 / s
      t10 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t11 = t9 * t10
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t34 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t35 = t9 * t34
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t35 /
     # 0.32D2)
      rrqg2qght6s2em3 = t14 * 0.3141592653589793D1 * t3 * t11 / 0.32D2 +
     # t22 * 0.3141592653589793D1 * t3 * t19 / 0.32D2 + t30 * 0.31415926
     #53589793D1 * t3 * t27 / 0.32D2 + t38 * 0.3141592653589793D1 * t3 *
     # t35 / 0.32D2

      end function



      doubleprecision function rrqg2qght6s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh63J7
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      rrqg2qght6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh61J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t5
      t8 = t3 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 ** 2
      t12 = t11 * t9
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t15 = t14 * t13
      t18 = s * t4
      t20 = z + x1 * t4
      t21 = 0.1D1 / t20
      t23 = 0.1D1 - x2
      t24 = x3 * t23
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t24 * t20 * x2 * t13)
      t34 = 0.2D1 * t28 * t32
      t35 = t24 * t20 + x2 * t13 - t34
      t40 = s - t18 * x1 * t21 * t35 - t18 * t9 * x3
      t43 = z + x1 * t23 * t4
      t44 = t40 * t43
      t45 = t20 ** 2
      t46 = 0.1D1 / t45
      t48 = t46 * x2 * x1
      t49 = t44 * t48
      t52 = t11 * t13
      t55 = 0.1D1 / t45 / t20
      t56 = x2 ** 2
      t58 = x1 ** 2
      t59 = t55 * t56 * t58
      t60 = t44 * t59
      t63 = t5 * t4
      t64 = t3 * t63
      t65 = t9 * t10
      t66 = t65 * t13
      t68 = t21 * z
      t73 = t3 * z
      t74 = t40 * t63
      t75 = t73 * t74
      t76 = t9 * t43
      t77 = t55 * t58
      t80 = x2 * x3
      t81 = t13 * t23 * t20 + t80 + t34
      t82 = t81 ** 2
      t83 = t77 * t82
      t87 = t2 * t1
      t88 = t87 * z
      t91 = t13 * t43
      t92 = x3 ** 2
      t93 = t21 * t92
      t97 = t6 * t65
      t102 = t6 * t4
      t103 = t3 * t102
      t104 = t58 * x1
      t105 = t45 ** 2
      t106 = 0.1D1 / t105
      t107 = t104 * t106
      t109 = t35 ** 2
      t110 = t109 * t40
      t111 = t10 * t43
      t112 = t111 * x2
      t116 = t3 * t6
      t117 = t116 * t77
      t118 = t35 * t40
      t122 = t40 * t6
      t125 = t43 * t46
      t127 = t125 * t80 * x1
      t134 = t125 * x1 * t81 * x3
      t142 = t43 * t55 * t58 * t81 * x2
      t154 = 0.72D2 * t8 * t12 * t15 * t49 - 0.288D3 * t8 * t52 * t60 - 
     #0.1728D4 * t64 * t66 * t44 * t68 * x3 - 0.144D3 * t75 * t76 * t83 
     #+ 0.576D3 * t88 * t6 * t11 * t91 * t93 - 0.1152D4 * t88 * t97 * t9
     #1 * t48 + 0.144D3 * t103 * t107 * t110 * t112 - 0.14976D5 * t117 *
     # t118 * t112 + 0.288D3 * t73 * t122 * t65 * t127 + 0.288D3 * t73 *
     # t74 * t10 * t134 - 0.288D3 * t73 * t122 * t10 * t142 + 0.1152D4 *
     # t88 * t102 * t65 * t13 * t142 - 0.1152D4 * t88 * t97 * t13 * t134
      t157 = t46 * x1
      t158 = t157 * t81
      t159 = t44 * t158
      t177 = t64 * t77
      t181 = t11 * t14
      t186 = t10 * t13
      t188 = t44 * t83
      t191 = t77 * t35
      t193 = t40 * t10
      t194 = t43 * t81
      t200 = t111 * x3
      t216 = t43 * t21
      t226 = t186 * t216
      t229 = 0.72D2 * t103 * t11 * t15 * t159 + 0.288D3 * t73 * t40 * t5
     # * t76 * t158 - 0.1152D4 * t88 * t63 * t10 * t91 * t158 + 0.576D3 
     #* t88 * t7 * t11 * t91 * t59 + 0.14832D5 * t177 * t110 * t76 + 0.1
     #44D3 * t8 * t181 * t40 * t142 - 0.288D3 * t116 * t186 * t188 - 0.1
     #4976D5 * t116 * t191 * t193 * t194 * x3 + 0.576D3 * t64 * t157 * t
     #118 * t200 - 0.14832D5 * t117 * t110 * t200 + 0.14832D5 * t103 * t
     #191 * t40 * t65 * t43 * x3 * x2 - 0.288D3 * t73 * t40 * t4 * t9 * 
     #t216 + 0.1440D4 * t116 * t11 * t15 * t40 * t216 + 0.576D3 * t88 * 
     #t5 * t226
      t231 = z ** 2
      t251 = t3 * t6 * t63
      t256 = t6 ** 2
      t271 = t52 * t40
      t273 = t43 * t106
      t296 = t21 * x3
      t300 = 0.576D3 * t87 * t231 * z * t5 * t226 - 0.1440D4 * t64 * t65
     # * t14 * t40 * t216 - 0.1152D4 * t88 * t102 * t11 * t13 * t127 + 0
     #.144D3 * t116 * t66 * t49 + 0.72D2 * t251 * t12 * t14 * t60 + 0.36
     #D2 * t3 * t256 * t12 * t13 * t44 * t106 * t56 * x2 * t104 + 0.576D
     #3 * t88 * t6 * t10 * t91 * t83 + 0.108D3 * t251 * t271 * t273 * t1
     #04 * t81 * t56 + 0.144D3 * t64 * t186 * t159 + 0.72D2 * t103 * t65
     # * t14 * t188 + 0.36D2 * t103 * t186 * t44 * t107 * t82 * t81 + 0.
     #144D3 * t103 * t271 * t127 + 0.1440D4 * t116 * t181 * t44 * t296
      t312 = t66 * t40
      t317 = t3 * t5 * t186
      t330 = t65 * t43
      t337 = t116 * t107
      t338 = t76 * t81
      t363 = 0.288D3 * t103 * t107 * t35 * t193 * t194 * x2 + 0.1152D4 *
     # t88 * t63 * t65 * t91 * t296 + 0.144D3 * t116 * t312 * t134 + 0.1
     #728D4 * t317 * t44 * t68 - 0.1728D4 * t317 * t44 * t21 * t231 + 0.
     #288D3 * t75 * t111 * t48 - 0.144D3 * t73 * t40 * t102 * t330 * t59
     # - 0.288D3 * t75 * t330 * t93 - 0.144D3 * t337 * t110 * t338 + 0.1
     #4832D5 * t177 * t118 * t338 - 0.144D3 * t337 * t118 * t76 * t82 - 
     #0.576D3 * t103 * t312 * t142 + 0.108D3 * t8 * t312 * t273 * t104 *
     # t82 * x2 - 0.144D3 * t8 * t107 * t118 * t330 * t56
      rrqg2qgh61J1 = -wd * (t154 + t229 + t300 + t363) / t1 / t40 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh61J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 - x3
      t12 = t11 ** 2
      t13 = t10 * t12
      t15 = s * t4
      t17 = z + x1 * t4
      t18 = 0.1D1 / t17
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t21 * t17 * x2 * t11)
      t31 = 0.2D1 * t25 * t29
      t32 = t21 * t17 + x2 * t11 - t31
      t37 = s - t15 * x1 * t18 * t32 - t15 * t8 * x3
      t40 = z + x1 * t20 * t4
      t41 = t37 * t40
      t42 = t18 * x3
      t43 = t41 * t42
      t44 = t7 * t13 * t43
      t46 = t5 * t4
      t47 = t3 * t46
      t48 = x1 ** 2
      t49 = t17 ** 2
      t51 = 0.1D1 / t49 / t17
      t52 = t48 * t51
      t53 = t47 * t52
      t54 = t32 * t37
      t55 = t8 * t40
      t58 = x2 * x3
      t59 = t11 * t20 * t17 + t58 + t31
      t60 = t55 * t59
      t62 = t53 * t54 * t60
      t64 = t48 * x1
      t65 = t49 ** 2
      t66 = 0.1D1 / t65
      t67 = t64 * t66
      t68 = t7 * t67
      t69 = t59 ** 2
      t72 = t68 * t54 * t55 * t69
      t74 = t11 * t9
      t76 = 0.1D1 / t49
      t77 = t76 * x1
      t78 = t77 * t59
      t79 = t41 * t78
      t80 = t47 * t74 * t79
      t82 = t9 * t8
      t83 = t82 * t11
      t84 = t47 * t83
      t85 = t18 * z
      t89 = 0.1728D4 * t84 * t41 * t85 * x3
      t91 = t52 * t69
      t92 = t41 * t91
      t95 = t52 * t32
      t97 = t37 * t9
      t98 = t40 * t59
      t101 = t7 * t95 * t97 * t98 * x3
      t103 = t3 * z
      t104 = t37 * t46
      t105 = t103 * t104
      t106 = t9 * t40
      t108 = t76 * x2 * x1
      t111 = 0.288D3 * t105 * t106 * t108
      t112 = t6 * t4
      t113 = t3 * t112
      t115 = t32 ** 2
      t116 = t115 * t37
      t117 = t106 * x2
      t119 = t113 * t67 * t116 * t117
      t121 = t6 * t5
      t122 = t3 * t121
      t123 = t13 * t37
      t128 = t40 * t51 * x2 * t48 * t59
      t132 = t3 * t6 * t46
      t133 = t10 * t8
      t134 = t133 * t12
      t136 = x2 ** 2
      t138 = t51 * t136 * t48
      t139 = t41 * t138
      t142 = t6 ** 2
      t144 = t133 * t11
      t152 = t12 * t11
      t155 = t41 * t108
      t157 = 0.72D2 * t122 * t133 * t152 * t155
      t158 = 0.1440D4 * t44 + 0.14832D5 * t62 - 0.144D3 * t72 + 0.144D3 
     #* t80 - t89 - 0.288D3 * t7 * t74 * t92 - 0.14976D5 * t101 + t111 +
     # 0.144D3 * t119 + 0.144D3 * t122 * t123 * t128 + 0.72D2 * t132 * t
     #134 * t139 + 0.36D2 * t3 * t142 * t144 * t41 * t66 * t136 * x2 * t
     #64 + t157
      t161 = t82 * t40
      t163 = t103 * t37 * t112 * t161 * t138
      t169 = t113 * t67 * t32 * t97 * t98 * x2
      t172 = t68 * t116 * t60
      t174 = x3 ** 2
      t175 = t18 * t174
      t178 = 0.288D3 * t105 * t161 * t175
      t179 = t3 * t5
      t180 = t179 * t74
      t183 = 0.1728D4 * t180 * t41 * t85
      t184 = z ** 2
      t188 = 0.1728D4 * t180 * t41 * t18 * t184
      t189 = t37 * t6
      t192 = t103 * t189 * t9 * t128
      t194 = t2 * t1
      t195 = t194 * z
      t204 = t40 * t18
      t205 = t74 * t204
      t207 = 0.576D3 * t194 * t184 * z * t5 * t205
      t209 = t105 * t55 * t91
      t212 = t103 * t37 * t5
      t215 = 0.288D3 * t212 * t55 * t78
      t219 = t7 * t52
      t221 = t219 * t54 * t117
      t226 = t122 * t67 * t54 * t161 * t136
      t228 = -0.144D3 * t163 + 0.288D3 * t169 - 0.144D3 * t172 - t178 + 
     #t183 - t188 - 0.288D3 * t192 + 0.1152D4 * t195 * t112 * t82 * t11 
     #* t128 + t207 - 0.144D3 * t209 + t215 + 0.576D3 * t195 * t5 * t205
     # - 0.14976D5 * t221 - 0.144D3 * t226
      t234 = 0.1440D4 * t7 * t10 * t152 * t37 * t204
      t239 = 0.288D3 * t103 * t37 * t4 * t8 * t204
      t243 = t47 * t82 * t12 * t37 * t204
      t247 = t11 * t40
      t251 = t10 * t11
      t252 = t251 * t37
      t253 = t113 * t252
      t254 = t40 * t76
      t256 = t254 * t58 * x1
      t257 = t253 * t256
      t276 = t254 * x1 * t59 * x3
      t278 = 0.288D3 * t103 * t104 * t9 * t276
      t298 = t234 - t239 - 0.1440D4 * t243 + 0.576D3 * t195 * t121 * t10
     # * t247 * t138 + 0.144D3 * t257 - 0.288D3 * t122 * t251 * t139 - 0
     #.1152D4 * t195 * t46 * t9 * t247 * t78 + 0.576D3 * t195 * t6 * t9 
     #* t247 * t91 + t278 + 0.72D2 * t113 * t82 * t12 * t92 - 0.1152D4 *
     # t195 * t112 * t10 * t11 * t256 + 0.1152D4 * t195 * t46 * t82 * t2
     #47 * t42 + 0.576D3 * t195 * t6 * t10 * t247 * t175
      t299 = t6 * t82
      t307 = 0.72D2 * t113 * t10 * t152 * t79
      t308 = t83 * t37
      t310 = t7 * t308 * t276
      t316 = t53 * t116 * t55
      t323 = t113 * t95 * t37 * t82 * t40 * x3 * x2
      t338 = 0.288D3 * t103 * t189 * t82 * t256
      t340 = t7 * t83 * t155
      t343 = t66 * t40
      t356 = t106 * x3
      t358 = t47 * t77 * t54 * t356
      t361 = t219 * t116 * t356
      t363 = -0.1152D4 * t195 * t299 * t247 * t108 + t307 + 0.144D3 * t3
     #10 - 0.576D3 * t113 * t308 * t128 + 0.14832D5 * t316 + 0.14832D5 *
     # t323 + 0.36D2 * t113 * t74 * t41 * t67 * t69 * t59 - 0.1152D4 * t
     #195 * t299 * t11 * t276 + t338 + 0.144D3 * t340 + 0.108D3 * t122 *
     # t308 * t343 * t64 * t69 * x2 + 0.108D3 * t132 * t252 * t343 * t64
     # * t59 * t136 + 0.576D3 * t358 - 0.14832D5 * t361
      t387 = -0.1728D4 * t44 + 0.432D3 * t7 * t77 * t54 * t161 * t174 + 
     #0.72D2 * t253 * t254 * t174 * x1 * t59 + 0.72D2 * t113 * t123 * t2
     #76 - 0.15696D5 * t62 - 0.936D3 * t72 - 0.72D2 * t80 + t89 + 0.1555
     #2D5 * t101 + t111 + 0.576D3 * t84 * t43
      t400 = -0.2160D4 * t119 - t157 + 0.288D3 * t163 + 0.1872D4 * t169 
     #+ 0.2160D4 * t172 + t178 - t183 + t188 + 0.576D3 * t192 + 0.72D2 *
     # t122 * t144 * t37 * t254 * t174 * x2 * x1 - t207
      t419 = 0.288D3 * t209 + t215 + 0.15552D5 * t221 - 0.936D3 * t226 -
     # t234 + t239 + 0.1728D4 * t243 - 0.288D3 * t179 * t9 * t11 * t37 *
     # t204 - 0.72D2 * t257 - 0.576D3 * t212 * t106 * t42 + 0.72D2 * t12
     #2 * t134 * t37 * t256
      t439 = t278 - t307 - 0.72D2 * t310 - 0.15552D5 * t316 - 0.15696D5 
     #* t323 - 0.288D3 * t7 * t251 * t41 * t175 + 0.432D3 * t179 * t77 *
     # t54 * t55 + 0.3096D4 * t68 * t115 * t32 * t37 * t55 + t338 - 0.72
     #D2 * t340 - 0.864D3 * t358 + 0.15552D5 * t361
      rrqg2qgh61J2 = -(wd * (t158 + t228 + t298 + t363) + wd * (t387 + t
     #400 + t419 + t439)) / t1 / t37 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh61J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 * t8
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t14 = s * t4
      t16 = z + x1 * t4
      t17 = 0.1D1 / t16
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t20 * t16 * x2 * t12)
      t30 = 0.2D1 * t24 * t28
      t31 = t20 * t16 + x2 * t12 - t30
      t36 = s - t14 * x1 * t17 * t31 - t14 * t8 * x3
      t40 = z + x1 * t19 * t4
      t41 = t40 * t17
      t43 = t7 * t10 * t13 * t36 * t41
      t45 = t3 * z
      t50 = 0.288D3 * t45 * t36 * t4 * t8 * t41
      t51 = t5 ** 2
      t52 = t3 * t51
      t53 = t9 ** 2
      t55 = t13 * t12
      t59 = 0.1440D4 * t52 * t53 * t55 * t36 * t41
      t60 = t2 * t1
      t61 = t60 * z
      t63 = t9 * t12
      t64 = t63 * t41
      t68 = t36 * t40
      t69 = t16 ** 2
      t70 = 0.1D1 / t69
      t71 = t70 * x1
      t74 = x2 * x3
      t75 = t12 * t19 * t16 + t74 + t30
      t76 = t71 * t75
      t77 = t68 * t76
      t78 = t7 * t63 * t77
      t81 = t45 * t36 * t5
      t82 = t8 * t40
      t85 = 0.288D3 * t81 * t82 * t76
      t88 = t12 * t40
      t95 = 0.1D1 / t69 / t16
      t96 = x1 ** 2
      t97 = t95 * t96
      t98 = t75 ** 2
      t99 = t97 * t98
      t105 = t17 * x3
      t109 = t51 * t4
      t113 = t40 * t70
      t115 = t113 * t74 * x1
      t118 = t10 * t12
      t119 = t118 * t36
      t123 = t113 * x1 * t75 * x3
      t124 = t52 * t119 * t123
      t126 = t3 * t109
      t131 = t40 * t95 * t96 * t75 * x2
      t134 = t52 * t97
      t135 = t31 ** 2
      t136 = t135 * t36
      t137 = t9 * t40
      t138 = t137 * x3
      t140 = t134 * t136 * t138
      t142 = -0.1440D4 * t43 - t50 + t59 + 0.576D3 * t61 * t5 * t64 + 0.
     #144D3 * t78 + t85 - 0.1152D4 * t61 * t6 * t9 * t88 * t76 + 0.576D3
     # * t61 * t51 * t9 * t88 * t99 + 0.1152D4 * t61 * t6 * t10 * t88 * 
     #t105 - 0.1152D4 * t61 * t109 * t53 * t12 * t115 + 0.144D3 * t124 -
     # 0.576D3 * t126 * t119 * t131 - 0.14832D5 * t140
      t145 = t70 * x2 * x1
      t146 = t68 * t145
      t147 = t52 * t118 * t146
      t150 = t3 * t51 * t6
      t151 = t53 * t8
      t152 = t151 * t13
      t154 = x2 ** 2
      t156 = t95 * t154 * t96
      t157 = t68 * t156
      t160 = t51 ** 2
      t162 = t151 * t12
      t164 = t69 ** 2
      t165 = 0.1D1 / t164
      t168 = t96 * x1
      t175 = x3 ** 2
      t176 = t17 * t175
      t180 = t51 * t5
      t181 = t3 * t180
      t183 = t40 * t165
      t189 = t53 * t12
      t190 = t189 * t36
      t197 = t36 * t6
      t198 = t45 * t197
      t199 = t10 * t40
      t202 = 0.288D3 * t198 * t199 * t176
      t203 = t3 * t5
      t204 = t203 * t63
      t205 = t17 * z
      t208 = 0.1728D4 * t204 * t68 * t205
      t209 = t97 * t31
      t215 = t126 * t209 * t36 * t10 * t40 * x3 * x2
      t220 = 0.288D3 * t45 * t197 * t9 * t123
      t221 = t168 * t165
      t222 = t52 * t221
      t223 = t82 * t75
      t225 = t222 * t136 * t223
      t227 = t51 * t10
      t237 = t53 * t13
      t238 = t237 * t36
      t242 = 0.144D3 * t147 + 0.72D2 * t150 * t152 * t157 + 0.36D2 * t3 
     #* t160 * t162 * t68 * t165 * t154 * x2 * t168 + 0.576D3 * t61 * t5
     #1 * t53 * t88 * t176 + 0.108D3 * t181 * t119 * t183 * t168 * t98 *
     # x2 + 0.108D3 * t150 * t190 * t183 * t168 * t75 * t154 - t202 + t2
     #08 + 0.14832D5 * t215 + t220 - 0.144D3 * t225 - 0.1152D4 * t61 * t
     #227 * t88 * t145 + 0.576D3 * t61 * t180 * t53 * t88 * t156 + 0.144
     #D3 * t181 * t238 * t131
      t244 = t7 * t97
      t246 = t244 * t136 * t82
      t248 = t126 * t190
      t249 = t248 * t115
      t261 = t31 * t36
      t264 = t181 * t221 * t261 * t199 * t154
      t268 = t7 * t71 * t261 * t138
      t272 = t68 * t99
      t278 = 0.72D2 * t126 * t53 * t55 * t77
      t283 = t36 * t9
      t284 = t40 * t75
      t287 = t52 * t209 * t283 * t284 * x3
      t291 = 0.288D3 * t198 * t137 * t145
      t296 = t36 * t51
      t300 = 0.288D3 * t45 * t296 * t10 * t115
      t301 = 0.14832D5 * t246 + 0.144D3 * t249 - 0.288D3 * t181 * t189 *
     # t157 + 0.36D2 * t126 * t63 * t68 * t221 * t98 * t75 - 0.144D3 * t
     #264 + 0.576D3 * t268 + 0.72D2 * t126 * t10 * t13 * t272 + t278 - 0
     #.288D3 * t52 * t63 * t272 - 0.14976D5 * t287 + t291 - 0.1152D4 * t
     #61 * t227 * t12 * t123 + t300
      t304 = t45 * t296 * t9 * t131
      t311 = z ** 2
      t315 = 0.1728D4 * t204 * t68 * t17 * t311
      t317 = t137 * x2
      t319 = t126 * t221 * t136 * t317
      t322 = t134 * t261 * t317
      t327 = t45 * t36 * t109 * t199 * t156
      t330 = t244 * t261 * t223
      t334 = t222 * t261 * t82 * t98
      t337 = t68 * t105
      t338 = t52 * t237 * t337
      t344 = t126 * t221 * t31 * t283 * t284 * x2
      t349 = 0.72D2 * t181 * t151 * t55 * t146
      t354 = 0.576D3 * t60 * t311 * z * t5 * t64
      t355 = t7 * t118
      t359 = 0.1728D4 * t355 * t68 * t205 * x3
      t361 = t198 * t82 * t99
      t363 = -0.288D3 * t304 + 0.1152D4 * t61 * t109 * t10 * t12 * t131 
     #- t315 + 0.144D3 * t319 - 0.14976D5 * t322 - 0.144D3 * t327 + 0.14
     #832D5 * t330 - 0.144D3 * t334 + 0.1440D4 * t338 + 0.288D3 * t344 +
     # t349 + t354 - t359 - 0.144D3 * t361
      t386 = -0.288D3 * t203 * t9 * t12 * t36 * t41 + 0.1728D4 * t43 + t
     #50 - t59 - 0.72D2 * t78 + t85 - 0.72D2 * t124 + 0.15552D5 * t140 +
     # 0.3096D4 * t222 * t135 * t31 * t36 * t82 - 0.72D2 * t147 - 0.288D
     #3 * t52 * t189 * t68 * t176
      t406 = 0.432D3 * t203 * t71 * t261 * t82 + t202 - t208 - 0.15696D5
     # * t215 + t220 + 0.2160D4 * t225 + 0.72D2 * t126 * t238 * t123 + 0
     #.72D2 * t181 * t162 * t36 * t113 * t175 * x2 * x1 - 0.15552D5 * t2
     #46 - 0.72D2 * t249 - 0.936D3 * t264
      t424 = -0.864D3 * t268 - t278 + 0.15552D5 * t287 + t291 + t300 + 0
     #.576D3 * t304 + 0.576D3 * t355 * t337 + 0.72D2 * t248 * t113 * t17
     #5 * x1 * t75 + t315 - 0.2160D4 * t319 + 0.432D3 * t52 * t71 * t261
     # * t199 * t175
      t439 = -0.576D3 * t81 * t137 * t105 + 0.15552D5 * t322 + 0.288D3 *
     # t327 + 0.72D2 * t181 * t152 * t36 * t115 - 0.15696D5 * t330 - 0.9
     #36D3 * t334 - 0.1728D4 * t338 + 0.1872D4 * t344 - t349 - t354 + t3
     #59 + 0.288D3 * t361
      rrqg2qgh61J3 = -(wd * (t142 + t242 + t301 + t363) + wd * (t386 + t
     #406 + t424 + t439)) / t1 / t36 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh61J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 * t8
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t14 = s * t4
      t16 = z + x1 * t4
      t17 = 0.1D1 / t16
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t20 * t16 * x2 * t12)
      t30 = 0.2D1 * t24 * t28
      t31 = t20 * t16 + x2 * t12 - t30
      t36 = s - t14 * x1 * t17 * t31 - t14 * t8 * x3
      t40 = z + x1 * t19 * t4
      t41 = t40 * t17
      t43 = t7 * t10 * t13 * t36 * t41
      t45 = t3 * z
      t50 = 0.288D3 * t45 * t36 * t4 * t8 * t41
      t51 = t5 ** 2
      t52 = t3 * t51
      t53 = t9 ** 2
      t55 = t13 * t12
      t59 = 0.1440D4 * t52 * t53 * t55 * t36 * t41
      t60 = t2 * t1
      t61 = t60 * z
      t63 = t9 * t12
      t64 = t63 * t41
      t68 = t3 * t51 * t6
      t69 = t53 * t8
      t70 = t69 * t13
      t72 = t36 * t40
      t73 = t16 ** 2
      t75 = 0.1D1 / t73 / t16
      t76 = x2 ** 2
      t78 = x1 ** 2
      t79 = t75 * t76 * t78
      t80 = t72 * t79
      t83 = t51 ** 2
      t85 = t69 * t12
      t87 = t73 ** 2
      t88 = 0.1D1 / t87
      t91 = t78 * x1
      t96 = t51 * t5
      t97 = t3 * t96
      t100 = 0.1D1 / t73
      t102 = t100 * x2 * x1
      t103 = t72 * t102
      t105 = 0.72D2 * t97 * t69 * t55 * t103
      t106 = t51 * t4
      t107 = t3 * t106
      t108 = t53 * t12
      t109 = t108 * t36
      t110 = t107 * t109
      t111 = t40 * t100
      t112 = x2 * x3
      t114 = t111 * t112 * x1
      t115 = t110 * t114
      t127 = t12 * t40
      t128 = t17 * x3
      t132 = t53 * t13
      t133 = t132 * t36
      t139 = t12 * t19 * t16 + t112 + t30
      t141 = t40 * t75 * x2 * t78 * t139
      t144 = t10 * t12
      t145 = t7 * t144
      t146 = t17 * z
      t150 = 0.1728D4 * t145 * t72 * t146 * x3
      t151 = -0.1440D4 * t43 - t50 + t59 + 0.576D3 * t61 * t5 * t64 + 0.
     #72D2 * t68 * t70 * t80 + 0.36D2 * t3 * t83 * t85 * t72 * t88 * t76
     # * x2 * t91 + t105 + 0.144D3 * t115 - 0.288D3 * t97 * t108 * t80 -
     # 0.1152D4 * t61 * t106 * t53 * t12 * t114 + 0.1152D4 * t61 * t6 * 
     #t10 * t127 * t128 + 0.144D3 * t97 * t133 * t141 - t150
      t152 = t36 * t6
      t153 = t45 * t152
      t154 = t8 * t40
      t155 = t75 * t78
      t156 = t139 ** 2
      t157 = t155 * t156
      t159 = t153 * t154 * t157
      t162 = t45 * t36 * t5
      t163 = t100 * x1
      t164 = t163 * t139
      t167 = 0.288D3 * t162 * t154 * t164
      t169 = t31 * t36
      t170 = t9 * t40
      t171 = t170 * x3
      t173 = t7 * t163 * t169 * t171
      t175 = t7 * t155
      t176 = t154 * t139
      t178 = t175 * t169 * t176
      t180 = t91 * t88
      t181 = t52 * t180
      t184 = t181 * t169 * t154 * t156
      t187 = t72 * t164
      t188 = t7 * t63 * t187
      t197 = t36 * t9
      t198 = t40 * t139
      t201 = t107 * t180 * t31 * t197 * t198 * x2
      t204 = t52 * t144 * t103
      t208 = 0.288D3 * t153 * t170 * t102
      t210 = t31 ** 2
      t211 = t210 * t36
      t212 = t170 * x2
      t214 = t107 * t180 * t211 * t212
      t216 = t52 * t155
      t218 = t216 * t169 * t212
      t221 = t10 * t40
      t224 = t97 * t180 * t169 * t221 * t76
      t226 = t144 * t36
      t230 = t111 * x1 * t139 * x3
      t231 = t52 * t226 * t230
      t233 = -0.144D3 * t159 + t167 + 0.576D3 * t173 + 0.14832D5 * t178 
     #- 0.144D3 * t184 + 0.144D3 * t188 - 0.1152D4 * t61 * t6 * t9 * t12
     #7 * t164 + 0.288D3 * t201 + 0.144D3 * t204 + t208 + 0.144D3 * t214
     # - 0.14976D5 * t218 - 0.144D3 * t224 + 0.144D3 * t231
      t239 = t40 * t88
      t250 = t51 * t10
      t255 = z ** 2
      t260 = 0.576D3 * t60 * t255 * z * t5 * t64
      t262 = t175 * t211 * t154
      t264 = t155 * t31
      t270 = t107 * t264 * t36 * t10 * t40 * x3 * x2
      t275 = 0.288D3 * t45 * t152 * t9 * t230
      t276 = t36 * t51
      t279 = t45 * t276 * t9 * t141
      t283 = x3 ** 2
      t284 = t17 * t283
      t298 = t181 * t211 * t176
      t300 = -0.576D3 * t107 * t226 * t141 + 0.108D3 * t97 * t226 * t239
     # * t91 * t156 * x2 + 0.1152D4 * t61 * t106 * t10 * t12 * t141 - 0.
     #1152D4 * t61 * t250 * t12 * t230 + t260 + 0.14832D5 * t262 + 0.148
     #32D5 * t270 + t275 - 0.288D3 * t279 + 0.576D3 * t61 * t51 * t53 * 
     #t127 * t284 - 0.1152D4 * t61 * t250 * t127 * t102 + 0.576D3 * t61 
     #* t96 * t53 * t127 * t79 - 0.144D3 * t298
      t302 = t216 * t211 * t171
      t305 = t72 * t128
      t306 = t52 * t132 * t305
      t309 = t72 * t157
      t315 = t52 * t264 * t197 * t198 * x3
      t320 = t45 * t36 * t106 * t221 * t79
      t324 = 0.288D3 * t153 * t221 * t284
      t325 = t3 * t5
      t326 = t325 * t63
      t329 = 0.1728D4 * t326 * t72 * t146
      t333 = 0.1728D4 * t326 * t72 * t17 * t255
      t337 = 0.288D3 * t45 * t276 * t10 * t114
      t362 = 0.72D2 * t107 * t53 * t55 * t187
      t363 = -0.14832D5 * t302 + 0.1440D4 * t306 - 0.288D3 * t52 * t63 *
     # t309 - 0.14976D5 * t315 - 0.144D3 * t320 - t324 + t329 - t333 + t
     #337 + 0.108D3 * t68 * t109 * t239 * t91 * t139 * t76 + 0.576D3 * t
     #61 * t51 * t9 * t127 * t157 + 0.72D2 * t107 * t10 * t13 * t309 + 0
     #.36D2 * t107 * t63 * t72 * t180 * t156 * t139 + t362
      t379 = 0.1728D4 * t43 + t50 - t59 - 0.288D3 * t325 * t9 * t12 * t3
     #6 * t41 - t105 - 0.72D2 * t115 + t150 + 0.288D3 * t159 + t167 - 0.
     #864D3 * t173 + 0.72D2 * t107 * t133 * t230
      t406 = -0.15696D5 * t178 - 0.936D3 * t184 - 0.72D2 * t188 + 0.432D
     #3 * t52 * t163 * t169 * t221 * t283 + 0.72D2 * t97 * t85 * t36 * t
     #111 * t283 * x2 * x1 + 0.1872D4 * t201 - 0.72D2 * t204 + 0.576D3 *
     # t145 * t305 - 0.576D3 * t162 * t170 * t128 + 0.72D2 * t97 * t70 *
     # t36 * t114 + t208
      t428 = -0.2160D4 * t214 + 0.15552D5 * t218 - 0.936D3 * t224 + 0.43
     #2D3 * t325 * t163 * t169 * t154 + 0.3096D4 * t181 * t210 * t31 * t
     #36 * t154 + 0.72D2 * t110 * t111 * t283 * x1 * t139 - 0.72D2 * t23
     #1 - t260 - 0.15552D5 * t262 - 0.15696D5 * t270 + t275
      t439 = 0.576D3 * t279 - 0.288D3 * t52 * t108 * t72 * t284 + 0.2160
     #D4 * t298 + 0.15552D5 * t302 - 0.1728D4 * t306 + 0.15552D5 * t315 
     #+ 0.288D3 * t320 + t324 - t329 + t333 + t337 - t362
      rrqg2qgh61J4 = -(wd * (t151 + t233 + t300 + t363) + wd * (t379 + t
     #406 + t428 + t439)) / t1 / t36 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh61J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 * t8
      t11 = 0.1D1 - x3
      t12 = t10 * t11
      t14 = s * t4
      t16 = z + x1 * t4
      t17 = 0.1D1 / t16
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t20 * t16 * x2 * t11)
      t30 = 0.2D1 * t24 * t28
      t31 = t20 * t16 + x2 * t11 - t30
      t36 = s - t14 * x1 * t17 * t31 - t14 * t8 * x3
      t39 = z + x1 * t19 * t4
      t40 = t36 * t39
      t41 = t16 ** 2
      t42 = 0.1D1 / t41
      t44 = t42 * x2 * x1
      t45 = t40 * t44
      t46 = t7 * t12 * t45
      t48 = t6 * t5
      t49 = t3 * t48
      t50 = t9 ** 2
      t51 = t11 ** 2
      t52 = t50 * t51
      t53 = t52 * t36
      t56 = 0.1D1 / t41 / t16
      t58 = x1 ** 2
      t62 = x2 * x3
      t63 = t11 * t19 * t16 + t62 + t30
      t65 = t39 * t56 * x2 * t58 * t63
      t68 = t9 * t11
      t70 = t56 * t58
      t71 = t63 ** 2
      t72 = t70 * t71
      t73 = t40 * t72
      t76 = t3 * z
      t77 = t36 * t6
      t80 = t39 * t42
      t82 = t80 * t62 * x1
      t84 = 0.288D3 * t76 * t77 * t10 * t82
      t85 = t6 * t4
      t86 = t3 * t85
      t87 = t51 * t11
      t90 = t42 * x1
      t91 = t90 * t63
      t92 = t40 * t91
      t94 = 0.72D2 * t86 * t50 * t87 * t92
      t95 = t12 * t36
      t99 = t80 * x1 * t63 * x3
      t100 = t7 * t95 * t99
      t102 = t2 * t1
      t103 = z ** 2
      t107 = t39 * t17
      t108 = t68 * t107
      t110 = 0.576D3 * t102 * t103 * z * t5 * t108
      t111 = t102 * z
      t119 = 0.1440D4 * t7 * t50 * t87 * t36 * t107
      t124 = 0.288D3 * t76 * t36 * t4 * t8 * t107
      t125 = t5 * t4
      t126 = t3 * t125
      t130 = t126 * t10 * t51 * t36 * t107
      t132 = t126 * t12
      t133 = t17 * z
      t137 = 0.1728D4 * t132 * t40 * t133 * x3
      t138 = t36 * t125
      t139 = t76 * t138
      t140 = t8 * t39
      t142 = t139 * t140 * t72
      t144 = 0.144D3 * t46 + 0.144D3 * t49 * t53 * t65 - 0.288D3 * t7 * 
     #t68 * t73 + t84 + t94 + 0.144D3 * t100 + t110 + 0.576D3 * t111 * t
     #5 * t108 + t119 - t124 - 0.1440D4 * t130 - t137 - 0.144D3 * t142
      t146 = t76 * t36 * t5
      t149 = 0.288D3 * t146 * t140 * t91
      t152 = t11 * t39
      t164 = 0.288D3 * t76 * t138 * t9 * t99
      t166 = t41 ** 2
      t167 = 0.1D1 / t166
      t168 = t39 * t167
      t169 = t58 * x1
      t176 = t3 * t6 * t125
      t177 = t50 * t11
      t178 = t177 * t36
      t181 = x2 ** 2
      t186 = t3 * t5
      t187 = t186 * t68
      t191 = 0.1728D4 * t187 * t40 * t17 * t103
      t192 = t126 * t70
      t193 = t31 * t36
      t194 = t140 * t63
      t196 = t192 * t193 * t194
      t198 = t169 * t167
      t199 = t7 * t198
      t202 = t199 * t193 * t140 * t71
      t204 = t70 * t31
      t210 = t86 * t204 * t36 * t10 * t39 * x3 * x2
      t213 = t31 ** 2
      t214 = t213 * t36
      t215 = t9 * t39
      t216 = t215 * x2
      t218 = t86 * t198 * t214 * t216
      t220 = t7 * t70
      t222 = t220 * t193 * t216
      t229 = t6 * t10
      t234 = t149 - 0.1152D4 * t111 * t125 * t9 * t152 * t91 + 0.576D3 *
     # t111 * t6 * t9 * t152 * t72 + t164 + 0.108D3 * t49 * t95 * t168 *
     # t169 * t71 * x2 + 0.108D3 * t176 * t178 * t168 * t169 * t63 * t18
     #1 - t191 + 0.14832D5 * t196 - 0.144D3 * t202 + 0.14832D5 * t210 + 
     #0.144D3 * t218 - 0.14976D5 * t222 + 0.1152D4 * t111 * t85 * t10 * 
     #t11 * t65 - 0.1152D4 * t111 * t229 * t11 * t99
      t237 = t126 * t68 * t92
      t250 = t36 * t9
      t251 = t39 * t63
      t254 = t7 * t204 * t250 * t251 * x3
      t256 = t6 ** 2
      t258 = t50 * t8
      t259 = t258 * t11
      t268 = t10 * t39
      t271 = t49 * t198 * t193 * t268 * t181
      t274 = t215 * x3
      t276 = t126 * t90 * t193 * t274
      t279 = t220 * t214 * t274
      t288 = t86 * t198 * t31 * t250 * t251 * x2
      t291 = t199 * t214 * t194
      t295 = t76 * t77 * t9 * t65
      t299 = 0.288D3 * t139 * t215 * t44
      t300 = 0.144D3 * t237 + 0.72D2 * t86 * t10 * t51 * t73 + 0.36D2 * 
     #t86 * t68 * t40 * t198 * t71 * t63 - 0.14976D5 * t254 + 0.36D2 * t
     #3 * t256 * t259 * t40 * t167 * t181 * x2 * t169 - 0.144D3 * t271 +
     # 0.576D3 * t276 - 0.14832D5 * t279 - 0.576D3 * t86 * t95 * t65 + 0
     #.288D3 * t288 - 0.144D3 * t291 - 0.288D3 * t295 + t299
      t304 = t56 * t181 * t58
      t306 = t76 * t36 * t85 * t268 * t304
      t308 = x3 ** 2
      t309 = t17 * t308
      t312 = 0.288D3 * t139 * t268 * t309
      t314 = t17 * x3
      t315 = t40 * t314
      t316 = t7 * t52 * t315
      t330 = 0.1728D4 * t187 * t40 * t133
      t348 = 0.72D2 * t49 * t258 * t87 * t45
      t349 = t86 * t178
      t350 = t349 * t82
      t353 = t40 * t304
      t357 = t192 * t214 * t140
      t359 = t258 * t51
      t363 = -0.144D3 * t306 - t312 + 0.1440D4 * t316 - 0.1152D4 * t111 
     #* t85 * t50 * t11 * t82 + 0.1152D4 * t111 * t125 * t10 * t152 * t3
     #14 + t330 + 0.576D3 * t111 * t6 * t50 * t152 * t309 - 0.1152D4 * t
     #111 * t229 * t152 * t44 + 0.576D3 * t111 * t48 * t50 * t152 * t304
     # + t348 + 0.144D3 * t350 - 0.288D3 * t49 * t177 * t353 + 0.14832D5
     # * t357 + 0.72D2 * t176 * t359 * t353
      t376 = -0.72D2 * t46 + 0.3096D4 * t199 * t213 * t31 * t36 * t140 +
     # t84 - t94 - 0.72D2 * t100 - t110 - t119 + t124 + 0.1728D4 * t130 
     #+ t137 + 0.288D3 * t142
      t392 = -0.288D3 * t186 * t9 * t11 * t36 * t107 + t149 + t164 + t19
     #1 - 0.15696D5 * t196 - 0.936D3 * t202 - 0.15696D5 * t210 - 0.2160D
     #4 * t218 + 0.15552D5 * t222 + 0.576D3 * t132 * t315 - 0.576D3 * t1
     #46 * t215 * t314
      t419 = 0.72D2 * t86 * t53 * t99 + 0.72D2 * t49 * t259 * t36 * t80 
     #* t308 * x2 * x1 - 0.72D2 * t237 + 0.15552D5 * t254 - 0.936D3 * t2
     #71 - 0.864D3 * t276 + 0.15552D5 * t279 + 0.1872D4 * t288 + 0.2160D
     #4 * t291 + 0.72D2 * t49 * t359 * t36 * t82 - 0.288D3 * t7 * t177 *
     # t40 * t309
      t439 = 0.432D3 * t186 * t90 * t193 * t140 + 0.576D3 * t295 + t299 
     #+ 0.288D3 * t306 + t312 - 0.1728D4 * t316 - t330 + 0.432D3 * t7 * 
     #t90 * t193 * t268 * t308 + 0.72D2 * t349 * t80 * t308 * x1 * t63 -
     # t348 - 0.72D2 * t350 - 0.15552D5 * t357
      rrqg2qgh61J5 = -(wd * (t144 + t234 + t300 + t363) + wd * (t376 + t
     #392 + t419 + t439)) / t1 / t36 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh61J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t3 * t6
      t9 = z + x1 * t4
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t12 = x1 * t11
      t14 = 0.1D1 - x2
      t15 = x3 * t14
      t17 = 0.1D1 - x3
      t20 = cos(x4 * 0.3141592653589793D1)
      t24 = Sqrt(t15 * t9 * x2 * t17)
      t26 = 0.2D1 * t20 * t24
      t27 = t15 * t9 + x2 * t17 - t26
      t28 = s * t4
      t29 = 0.1D1 / t9
      t33 = 0.1D1 - x1
      t36 = s - t28 * x1 * t29 * t27 - t28 * t33 * x3
      t37 = t27 * t36
      t38 = t33 ** 2
      t41 = z + x1 * t14 * t4
      t42 = t38 * t41
      t43 = t42 * x3
      t47 = t5 ** 2
      t48 = t47 * t4
      t49 = t3 * t48
      t50 = x1 ** 2
      t52 = t10 ** 2
      t54 = t50 * x1 / t52
      t57 = t36 * t38
      t60 = x2 * x3
      t61 = t17 * t14 * t9 + t60 + t26
      t62 = t41 * t61
      t67 = t3 * t47
      t68 = t38 ** 2
      t69 = t68 * t17
      t71 = t36 * t41
      t72 = x3 ** 2
      t73 = t29 * t72
      t78 = t49 * t69 * t36
      t79 = t41 * t11
      t85 = t17 ** 2
      t86 = t68 * t85
      t91 = t79 * x3 * x1 * t61
      t94 = t67 * t54
      t95 = t27 ** 2
      t96 = t95 * t36
      t97 = t33 * t41
      t98 = t97 * t61
      t103 = 0.1D1 / t10 / t9
      t104 = t50 * t103
      t105 = t7 * t104
      t109 = t67 * t104
      t114 = t29 * x3
      t115 = t71 * t114
      t118 = t104 * t27
      t124 = t3 * z
      t125 = t36 * t6
      t126 = t124 * t125
      t128 = t11 * x2 * x1
      t132 = -0.864D3 * t7 * t12 * t37 * t43 + 0.1872D4 * t49 * t54 * t2
     #7 * t57 * t62 * x2 - 0.288D3 * t67 * t69 * t71 * t73 + 0.72D2 * t7
     #8 * t79 * t72 * x1 * t61 + 0.72D2 * t49 * t86 * t36 * t91 + 0.2160
     #D4 * t94 * t96 * t98 - 0.15696D5 * t105 * t37 * t98 + 0.15552D5 * 
     #t109 * t96 * t43 - 0.1728D4 * t67 * t86 * t115 + 0.15552D5 * t67 *
     # t118 * t57 * t62 * x3 + 0.288D3 * t126 * t42 * t128
      t135 = t38 * t33
      t136 = t135 * t41
      t137 = x2 ** 2
      t151 = t42 * x2
      t158 = t3 * t5
      t161 = t41 * t29
      t165 = t135 * t17
      t167 = t71 * t128
      t170 = t61 ** 2
      t178 = t38 * t17
      t180 = t12 * t61
      t181 = t71 * t180
      t184 = t85 * t17
      t193 = 0.288D3 * t124 * t36 * t48 * t136 * t103 * t137 * t50 - 0.1
     #5696D5 * t49 * t118 * t36 * t135 * t41 * x3 * x2 - 0.2160D4 * t49 
     #* t54 * t96 * t151 + 0.15552D5 * t109 * t37 * t151 - 0.288D3 * t15
     #8 * t38 * t17 * t36 * t161 - 0.72D2 * t67 * t165 * t167 - 0.936D3 
     #* t94 * t37 * t97 * t170 - 0.15552D5 * t105 * t96 * t97 - 0.72D2 *
     # t7 * t178 * t181 - 0.72D2 * t49 * t68 * t184 * t181 + 0.432D3 * t
     #158 * t12 * t37 * t97
      t203 = t158 * t178
      t204 = t29 * z
      t208 = t36 * t47
      t212 = t79 * t60 * x1
      t216 = t3 * t47 * t5
      t217 = t68 * t33
      t231 = t7 * t165
      t241 = t124 * t36 * t5
      t250 = 0.3096D4 * t94 * t95 * t27 * t36 * t97 + 0.288D3 * t126 * t
     #136 * t73 - 0.1728D4 * t203 * t71 * t204 + 0.288D3 * t124 * t208 *
     # t135 * t212 + 0.72D2 * t216 * t217 * t85 * t36 * t212 + 0.72D2 * 
     #t216 * t217 * t17 * t36 * t79 * t72 * x2 * x1 + 0.1728D4 * t231 * 
     #t71 * t204 * x3 + 0.288D3 * t126 * t97 * t104 * t170 + 0.288D3 * t
     #241 * t97 * t180 + 0.576D3 * t231 * t115 - 0.576D3 * t241 * t42 * 
     #t114
      t251 = z ** 2
      t310 = 0.1728D4 * t203 * t71 * t29 * t251 + 0.432D3 * t67 * t12 * 
     #t37 * t136 * t72 - 0.72D2 * t67 * t165 * t36 * t91 - 0.72D2 * t216
     # * t217 * t184 * t167 - 0.72D2 * t78 * t212 + 0.1728D4 * t7 * t135
     # * t85 * t36 * t161 + 0.288D3 * t124 * t36 * t4 * t33 * t161 - 0.1
     #440D4 * t67 * t68 * t184 * t36 * t161 - 0.576D3 * t2 * t1 * t251 *
     # z * t5 * t178 * t161 - 0.936D3 * t216 * t54 * t37 * t136 * t137 +
     # 0.288D3 * t124 * t125 * t38 * t91 + 0.576D3 * t124 * t208 * t38 *
     # t41 * t103 * t50 * t61 * x2
      rrqg2qgh61J6 = -wd * (t132 + t193 + t250 + t310) / t1 / t36 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 * t8
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = t10 * t14
      t17 = z + x1 * t4
      t18 = t17 ** 2
      t19 = 0.1D1 / t18
      t21 = t7 * t15 * t19
      t22 = 0.1D1 - x3
      t23 = s * t4
      t24 = 0.1D1 / t17
      t28 = x2 * x3
      t30 = cos(x4 * 0.3141592653589793D1)
      t31 = x3 * t11
      t35 = Sqrt(t31 * t17 * x2 * t22)
      t37 = 0.2D1 * t30 * t35
      t38 = t22 * t11 * t17 + t28 + t37
      t43 = s - t23 * x1 * t24 * t38 - t23 * t8 * t22
      t44 = t22 * t43
      t46 = x3 * x1 * t38
      t50 = t2 * t1
      t51 = t6 * t4
      t52 = t50 * t51
      t53 = t52 * t15
      t55 = 0.1D1 / t18 / t17
      t56 = t55 * t22
      t57 = x1 ** 2
      t58 = t38 ** 2
      t59 = t57 * t58
      t64 = t5 * t4
      t65 = t50 * t64
      t66 = t65 * t10
      t67 = t14 * t24
      t68 = t22 ** 2
      t72 = t9 ** 2
      t73 = t72 * t8
      t74 = t52 * t73
      t75 = t68 ** 2
      t84 = t50 * t6
      t85 = t84 * t72
      t86 = t68 * t22
      t90 = t9 * t14
      t91 = t52 * t90
      t92 = t18 ** 2
      t93 = 0.1D1 / t92
      t94 = t57 * x1
      t95 = t93 * t94
      t98 = t31 * t17 + x2 * t22 - t37
      t99 = t98 ** 2
      t100 = t99 * t38
      t105 = t72 * t14
      t106 = t52 * t105
      t108 = x1 * t38
      t112 = t6 ** 2
      t116 = 0.1D1 / t92 / t17
      t117 = t57 ** 2
      t118 = t116 * t117
      t119 = x2 ** 2
      t125 = t8 * t14
      t126 = t7 * t125
      t127 = t98 * t43
      t132 = t6 * t5
      t133 = t3 * t132
      t139 = t55 * t57
      t140 = x3 ** 2
      t145 = t99 * t43
      t150 = t7 * t90
      t156 = t3 * t64
      t158 = t19 * t22
      t160 = t43 * x1 * t38
      t164 = t52 * t125
      t165 = t58 * t38
      t170 = t50 * t132
      t171 = t170 * t90
      t172 = t98 * t58
      t177 = -0.128D3 * t21 * t44 * t46 - 0.144D3 * t53 * t56 * t59 * x3
     # + 0.108D3 * t66 * t67 * t68 + 0.36D2 * t74 * t67 * t75 + 0.36D2 *
     # t50 * t5 * t9 * t67 * t22 + 0.108D3 * t85 * t67 * t86 - 0.112D3 *
     # t91 * t95 * t100 * x3 + 0.28D2 * t106 * t19 * t86 * t108 + 0.36D2
     # * t50 * t112 * t105 * t118 * t98 * t119 * x2 + 0.192D3 * t126 * t
     #95 * t127 * t58 + 0.430D3 * t133 * t15 * t95 * t127 * t119 + 0.216
     #D3 * t53 * t139 * t99 * t140 + 0.182D3 * t126 * t95 * t145 * t38 +
     # 0.47D2 * t150 * t56 * t43 * t57 * t58 + 0.96D2 * t156 * t90 * t15
     #8 * t160 + 0.28D2 * t164 * t118 * t98 * t165 + 0.36D2 * t171 * t11
     #8 * t172 * x2
      t182 = x1 * t19
      t185 = t140 * x3
      t211 = t7 * t15
      t216 = t7 * t105
      t218 = t43 * x3
      t231 = t99 * t98
      t237 = t50 * t6 * t64
      t238 = t237 * t15
      t269 = 0.56D2 * t171 * t118 * t100 * x2 + 0.16D2 * t170 * t182 * t
     #38 * t73 * t185 * t14 * t22 + 0.16D2 * t170 * t95 * t165 * t10 * x
     #3 * t14 * t22 + 0.36D2 * t53 * t55 * t68 * t59 + 0.28D2 * t65 * t9
     #0 * t158 * t108 - 0.128D3 * t21 * t44 * x2 * x1 * z + 0.56D2 * t21
     #1 * t182 * t127 * t140 - 0.240D3 * t216 * t24 * t68 * t218 + 0.36D
     #2 * t84 * t90 * t56 * t59 + 0.28D2 * t91 * t93 * t22 * t94 * t165 
     #+ 0.108D3 * t171 * t118 * t231 * x2 + 0.108D3 * t238 * t118 * t99 
     #* t119 - 0.144D3 * t237 * t105 * t95 * t98 * x3 * t119 + 0.216D3 *
     # t170 * t105 * t139 * t98 * t140 * x2 - 0.288D3 * t170 * t15 * t95
     # * t99 * x2 * x3 - 0.144D3 * t74 * t67 * t22 * t185 + 0.216D3 * t7
     #4 * t67 * t68 * t140
      t304 = t99 ** 2
      t309 = t19 * t68
      t313 = t3 * t51
      t315 = t313 * t15 * t55
      t321 = t98 * t38
      t333 = t84 * t15
      t349 = -0.288D3 * t85 * t67 * t68 * x3 - 0.144D3 * t74 * t67 * t86
     # * x3 + 0.216D3 * t85 * t67 * t22 * t140 - 0.144D3 * t66 * t67 * t
     #22 * x3 - 0.16D2 * t156 * t10 * t67 * t68 * t43 + 0.748D3 * t3 * t
     #5 * t9 * t67 * t44 - 0.12D2 * t7 * t72 * t67 * t86 * t43 + 0.36D2 
     #* t52 * t8 * t14 * t116 * t117 * t304 - 0.148D3 * t211 * t309 * t1
     #60 + 0.94D2 * t315 * t44 * t57 * t38 * x2 + 0.28D2 * t238 * t118 *
     # t321 * t119 + 0.168D3 * t53 * t139 * t321 * t140 - 0.112D3 * t106
     # * t309 * t46 - 0.112D3 * t333 * t158 * t46 - 0.144D3 * t91 * t95 
     #* t172 * x3 + 0.28D2 * t164 * t118 * t231 * t38 + 0.168D3 * t106 *
     # t158 * t108 * t140
      t350 = t57 * t98
      t372 = t24 * t22
      t397 = t94 * t98
      t405 = t43 * x2 * x1
      t441 = -0.584D3 * t315 * t350 * t218 * x2 - 0.128D3 * t156 * t90 *
     # t19 * t44 * z * x1 * t38 + 0.400D3 * t7 * t90 * t55 * t350 * t218
     # * t38 + 0.56D2 * t333 * t309 * t108 - 0.880D3 * t156 * t15 * t372
     # * t218 + 0.312D3 * t216 * t372 * t43 * t140 - 0.128D3 * t313 * t1
     #05 * t19 * t44 * t28 * x1 - 0.328D3 * t313 * t90 * t95 * t145 * x2
     # + 0.36D2 * t164 * t118 * t99 * t58 - 0.112D3 * t170 * t15 * t93 *
     # t397 * x2 * t38 * x3 - 0.148D3 * t313 * t105 * t309 * t405 + 0.96
     #D2 * t211 * t158 * t405 + 0.47D2 * t133 * t105 * t56 * t43 * t119 
     #* t57 - 0.144D3 * t91 * t95 * t231 * x3 - 0.144D3 * t106 * t182 * 
     #t98 * t185 - 0.530D3 * t313 * t90 * t93 * t397 * t43 * t38 * x2 + 
     #0.328D3 * t150 * t139 * t145 * x3 + 0.10D2 * t126 * t95 * t231 * t
     #43
      rrqg2qgh62J1 = -wd * (t177 + t269 + t349 + t441) / t1 / t43 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t4
      t17 = z + x1 * t4
      t18 = 0.1D1 / t17
      t19 = t15 * t18
      t20 = 0.1D1 - x3
      t21 = t20 ** 2
      t22 = t21 * t20
      t23 = s * t4
      t27 = x2 * x3
      t29 = cos(x4 * 0.3141592653589793D1)
      t30 = x3 * t12
      t34 = Sqrt(t30 * t17 * x2 * t20)
      t36 = 0.2D1 * t29 * t34
      t37 = t20 * t12 * t17 + t27 + t36
      t42 = s - t23 * x1 * t18 * t37 - t23 * t8 * t20
      t45 = t7 * t10 * t19 * t22 * t42
      t47 = t2 * t1
      t48 = t6 * t4
      t49 = t47 * t48
      t51 = t17 ** 2
      t52 = t51 ** 2
      t54 = 0.1D1 / t52 / t17
      t56 = x1 ** 2
      t57 = t56 ** 2
      t60 = t30 * t17 + x2 * t20 - t36
      t61 = t60 ** 2
      t62 = t61 ** 2
      t67 = t3 * t5
      t69 = t20 * t42
      t71 = t67 * t9 * t19 * t69
      t73 = t5 * t4
      t74 = t3 * t73
      t75 = t9 * t8
      t79 = t74 * t75 * t19 * t21 * t42
      t81 = t47 * t6
      t82 = t81 * t10
      t87 = t10 * t8
      t88 = t49 * t87
      t93 = x3 ** 2
      t96 = t82 * t19 * t20 * t93
      t98 = t47 * t73
      t99 = t98 * t75
      t106 = t88 * t19 * t21 * t93
      t108 = t10 * t15
      t109 = t7 * t108
      t110 = t18 * t20
      t113 = t109 * t110 * t42 * t93
      t115 = t3 * t48
      t116 = 0.1D1 / t51
      t119 = t27 * x1
      t121 = t115 * t108 * t116 * t69 * t119
      t124 = t47 * t6 * t73
      t126 = 0.1D1 / t52
      t127 = t56 * x1
      t128 = t126 * t127
      t130 = x2 ** 2
      t135 = t9 * t15
      t141 = t74 * t135 * t116 * t69 * z * x1 * t37
      t143 = t6 * t5
      t144 = t47 * t143
      t147 = 0.1D1 / t51 / t17
      t148 = t147 * t56
      t152 = t144 * t108 * t148 * t60 * t93 * x2
      t154 = t75 * t15
      t161 = t49 * t154
      t164 = t161 * t148 * t61 * t93
      t167 = t147 * t20
      t168 = t37 ** 2
      t169 = t56 * t168
      t171 = t81 * t135 * t167 * t169
      t173 = -0.12D2 * t45 + 0.36D2 * t49 * t8 * t15 * t54 * t57 * t62 +
     # 0.748D3 * t71 - 0.16D2 * t79 - 0.288D3 * t82 * t19 * t21 * x3 - 0
     #.144D3 * t88 * t19 * t22 * x3 + 0.216D3 * t96 - 0.144D3 * t99 * t1
     #9 * t20 * x3 + 0.216D3 * t106 + 0.312D3 * t113 - 0.128D3 * t121 - 
     #0.144D3 * t124 * t108 * t128 * t60 * x3 * t130 - 0.128D3 * t141 + 
     #0.216D3 * t152 - 0.288D3 * t144 * t154 * t128 * t61 * x2 * x3 + 0.
     #216D3 * t164 + 0.36D2 * t171
      t174 = t49 * t135
      t176 = t168 * t37
      t179 = t174 * t126 * t20 * t127 * t176
      t181 = t49 * t108
      t183 = x1 * t37
      t187 = t6 ** 2
      t190 = t54 * t57
      t196 = t144 * t135
      t197 = t61 * t60
      t203 = t61 * t42
      t206 = t115 * t135 * t128 * t203 * x2
      t209 = t7 * t154 * t116
      t210 = x2 * x1
      t213 = t209 * t69 * t210 * z
      t215 = t8 * t15
      t216 = t49 * t215
      t219 = t216 * t190 * t60 * t176
      t221 = t60 * t168
      t224 = t196 * t190 * t221 * x2
      t226 = t61 * t37
      t231 = t124 * t154
      t232 = t60 * t37
      t241 = t116 * x1
      t242 = t93 * x3
      t246 = 0.144D3 * t181 * t241 * t60 * t242
      t249 = t161 * t148 * t232 * t93
      t253 = t127 * t60
      t257 = t115 * t135 * t126 * t253 * t42 * t37 * x2
      t259 = t7 * t135
      t262 = t259 * t148 * t203 * x3
      t264 = t7 * t215
      t267 = t264 * t128 * t197 * t42
      t272 = 0.28D2 * t179 + 0.28D2 * t181 * t116 * t22 * t183 + 0.36D2 
     #* t47 * t187 * t108 * t190 * t60 * t130 * x2 + 0.108D3 * t196 * t1
     #90 * t197 * x2 - 0.328D3 * t206 - 0.128D3 * t213 + 0.28D2 * t219 +
     # 0.36D2 * t224 + 0.56D2 * t196 * t190 * t226 * x2 + 0.28D2 * t231 
     #* t190 * t232 * t130 - 0.144D3 * t174 * t128 * t197 * x3 - t246 + 
     #0.168D3 * t249 - 0.530D3 * t257 + 0.328D3 * t262 + 0.10D2 * t267 +
     # 0.108D3 * t82 * t19 * t22
      t275 = t115 * t154 * t147
      t279 = t275 * t69 * t56 * t37 * x2
      t281 = t115 * t108
      t282 = t116 * t21
      t284 = t42 * x2 * x1
      t286 = t281 * t282 * t284
      t288 = t7 * t154
      t290 = t42 * x1 * t37
      t292 = t288 * t282 * t290
      t297 = t21 ** 2
      t307 = t42 * x3
      t309 = t74 * t154 * t110 * t307
      t312 = x3 * x1 * t37
      t314 = t209 * t69 * t312
      t322 = 0.16D2 * t144 * t128 * t176 * t75 * x3 * t15 * t20
      t324 = t116 * t20
      t328 = t81 * t154
      t332 = t3 * t143
      t334 = t60 * t42
      t337 = t332 * t154 * t128 * t334 * t130
      t341 = t174 * t128 * t221 * x3
      t349 = t216 * t190 * t61 * t168
      t353 = t56 * t60
      t356 = t7 * t135 * t147 * t353 * t307 * t37
      t358 = t183 * t93
      t360 = t181 * t324 * t358
      t362 = 0.94D2 * t279 - 0.148D3 * t286 - 0.148D3 * t292 + 0.108D3 *
     # t99 * t19 * t21 + 0.36D2 * t88 * t19 * t297 + 0.36D2 * t47 * t5 *
     # t9 * t19 * t20 - 0.880D3 * t309 - 0.128D3 * t314 + t322 + 0.28D2 
     #* t98 * t135 * t324 * t183 + 0.56D2 * t328 * t282 * t183 + 0.430D3
     # * t337 - 0.144D3 * t341 + 0.28D2 * t216 * t190 * t197 * t37 + 0.3
     #6D2 * t349 + 0.400D3 * t356 + 0.168D3 * t360
      t365 = t161 * t167 * t169 * x3
      t372 = t144 * t154 * t126 * t253 * x2 * t37 * x3
      t376 = t174 * t128 * t226 * x3
      t380 = t109 * t18 * t21 * t307
      t383 = t181 * t282 * t312
      t386 = t328 * t324 * t312
      t395 = 0.144D3 * t88 * t19 * t20 * t242
      t399 = t259 * t167 * t42 * t56 * t168
      t401 = t74 * t135
      t403 = t401 * t324 * t290
      t407 = t264 * t128 * t203 * t37
      t411 = t288 * t241 * t334 * t93
      t414 = t288 * t324 * t284
      t420 = t332 * t108 * t167 * t42 * t130 * t56
      t424 = t275 * t353 * t307 * x2
      t428 = t264 * t128 * t334 * t168
      t436 = 0.16D2 * t144 * t241 * t37 * t87 * t242 * t15 * t20
      t439 = t161 * t147 * t21 * t169
      t441 = -0.144D3 * t365 - 0.112D3 * t372 - 0.112D3 * t376 - 0.240D3
     # * t380 - 0.112D3 * t383 - 0.112D3 * t386 + 0.108D3 * t231 * t190 
     #* t61 * t130 - t395 + 0.47D2 * t399 + 0.96D2 * t403 + 0.182D3 * t4
     #07 + 0.56D2 * t411 + 0.96D2 * t414 + 0.47D2 * t420 - 0.584D3 * t42
     #4 + 0.192D3 * t428 + t436 + 0.36D2 * t439
      t455 = t116 * t42
      t468 = -0.16D2 * t45 - 0.592D3 * t71 + 0.608D3 * t79 - 0.72D2 * t9
     #6 - 0.72D2 * t106 - 0.264D3 * t113 + 0.496D3 * t121 + 0.256D3 * t1
     #41 - 0.72D2 * t152 - 0.72D2 * t164 - 0.128D3 * t288 * t455 * t119 
     #+ 0.256D3 * t259 * t148 * t334 * x2 - 0.128D3 * t401 * t455 * t312
     # - 0.72D2 * t171 - 0.56D2 * t179
      t482 = t67 * t215
      t488 = 0.604D3 * t206 + 0.64D2 * t288 * t455 * t358 + 0.256D3 * t2
     #13 - 0.56D2 * t219 - 0.72D2 * t224 + t246 - 0.344D3 * t249 + 0.444
     #D3 * t257 - 0.256D3 * t262 - 0.62D2 * t267 - 0.108D3 * t279 + 0.21
     #6D3 * t286 + 0.136D3 * t482 * t241 * t334 + 0.216D3 * t292 + 0.640
     #D3 * t309
      t519 = 0.496D3 * t314 - t322 - 0.542D3 * t337 + 0.256D3 * t341 - 0
     #.72D2 * t349 + 0.64D2 * t401 * t455 * t210 + 0.32D2 * t144 * t148 
     #* t168 * t10 * t93 * t15 * t20 - 0.288D3 * t356 - 0.344D3 * t360 -
     # 0.216D3 * t401 * t241 * t334 * x3 + 0.64D2 * t281 * t455 * t93 * 
     #x2 * x1 + 0.256D3 * t365 + 0.144D3 * t372 + 0.144D3 * t376 - 0.64D
     #2 * t380
      t522 = t74 * t215
      t542 = 0.144D3 * t383 + 0.144D3 * t386 - 0.256D3 * t522 * t148 * t
     #334 * t37 + 0.64D2 * t482 * t455 * t183 - 0.256D3 * t522 * t148 * 
     #t203 + t395 - 0.54D2 * t399 - 0.640D3 * t403 - 0.108D3 * t407 + 0.
     #24D2 * t411 - 0.640D3 * t414 - 0.54D2 * t420 + 0.544D3 * t424 - 0.
     #30D2 * t428 - t436 - 0.72D2 * t439
      rrqg2qgh62J2 = -(wd * (t173 + t272 + t362 + t441) + wd * (t468 + t
     #488 + t519 + t542)) / t1 / t42 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = t10 * t14
      t17 = z + x1 * t4
      t18 = t17 ** 2
      t19 = t18 ** 2
      t20 = 0.1D1 / t19
      t23 = x1 ** 2
      t24 = t23 * x1
      t25 = x3 * t11
      t27 = 0.1D1 - x3
      t30 = cos(x4 * 0.3141592653589793D1)
      t34 = Sqrt(t25 * t17 * x2 * t27)
      t36 = 0.2D1 * t30 * t34
      t37 = t25 * t17 + x2 * t27 - t36
      t38 = t24 * t37
      t39 = s * t4
      t40 = 0.1D1 / t17
      t44 = x2 * x3
      t45 = t27 * t11 * t17 + t44 + t36
      t50 = s - t39 * x1 * t40 * t45 - t39 * t9 * t27
      t54 = t8 * t15 * t20 * t38 * t50 * t45 * x2
      t56 = t3 * t6
      t57 = t56 * t15
      t59 = 0.1D1 / t18 / t17
      t60 = t59 * t23
      t61 = t37 ** 2
      t62 = t61 * t50
      t65 = t57 * t60 * t62 * x3
      t67 = t9 * t14
      t68 = t56 * t67
      t69 = t20 * t24
      t70 = t61 * t37
      t73 = t68 * t69 * t70 * t50
      t75 = t10 * t9
      t76 = t75 * t14
      t78 = t8 * t76 * t59
      t79 = t23 * t37
      t80 = t50 * x3
      t83 = t78 * t79 * t80 * x2
      t89 = t56 * t15 * t59 * t79 * t80 * t45
      t91 = t10 ** 2
      t92 = t91 * t14
      t93 = 0.1D1 / t18
      t96 = t27 * t50
      t97 = t44 * x1
      t99 = t8 * t92 * t93 * t96 * t97
      t101 = t5 * t4
      t102 = t3 * t101
      t104 = t40 * t27
      t106 = t102 * t76 * t104 * t80
      t108 = t56 * t92
      t109 = x3 ** 2
      t112 = t108 * t104 * t50 * t109
      t114 = t2 * t1
      t115 = t114 * t7
      t116 = t115 * t15
      t117 = t61 * t45
      t120 = t116 * t69 * t117 * x3
      t122 = t114 * t6
      t123 = t122 * t91
      t124 = t14 * t40
      t125 = t27 ** 2
      t126 = t125 * t27
      t130 = t6 * t101
      t131 = t114 * t130
      t132 = t131 * t76
      t134 = 0.1D1 / t19 / t17
      t135 = t23 ** 2
      t136 = t134 * t135
      t137 = t37 * t45
      t138 = x2 ** 2
      t143 = t56 * t76
      t144 = t93 * t125
      t146 = t50 * x1 * t45
      t148 = t143 * t144 * t146
      t150 = t23 * t45
      t151 = t150 * x2
      t153 = t78 * t96 * t151
      t155 = t8 * t92
      t156 = t50 * x2
      t157 = t156 * x1
      t159 = t155 * t144 * t157
      t171 = t6 * t5
      t172 = t114 * t171
      t177 = t172 * t92 * t60 * t37 * t109 * x2
      t179 = -0.530D3 * t54 + 0.328D3 * t65 + 0.10D2 * t73 - 0.584D3 * t
     #83 + 0.400D3 * t89 - 0.128D3 * t99 - 0.880D3 * t106 + 0.312D3 * t1
     #12 - 0.112D3 * t120 + 0.108D3 * t123 * t124 * t126 + 0.28D2 * t132
     # * t136 * t137 * t138 - 0.148D3 * t148 + 0.94D2 * t153 - 0.148D3 *
     # t159 + 0.108D3 * t132 * t136 * t61 * t138 - 0.144D3 * t131 * t92 
     #* t69 * t37 * x3 * t138 + 0.216D3 * t177
      t186 = t115 * t76
      t189 = t186 * t60 * t61 * t109
      t191 = t172 * t15
      t198 = t186 * t60 * t137 * t109
      t200 = t45 ** 2
      t201 = t37 * t200
      t204 = t116 * t69 * t201 * x3
      t206 = t115 * t67
      t213 = t206 * t136 * t61 * t200
      t215 = t3 * t171
      t216 = t215 * t76
      t217 = t37 * t50
      t220 = t216 * t69 * t217 * t138
      t222 = t91 * t9
      t223 = t115 * t222
      t224 = t109 * x3
      t228 = 0.144D3 * t223 * t124 * t27 * t224
      t230 = t56 * t76 * t93
      t231 = x2 * x1
      t234 = t230 * t96 * t231 * z
      t235 = 0.128D3 * t234
      t236 = t93 * x1
      t239 = t143 * t236 * t217 * t109
      t243 = t223 * t124 * t125 * t109
      t245 = t114 * t101
      t246 = t245 * t75
      t253 = t123 * t124 * t27 * t109
      t266 = t102 * t75 * t124 * t125 * t50
      t268 = -0.288D3 * t172 * t76 * t69 * t61 * x2 * x3 + 0.216D3 * t18
     #9 + 0.56D2 * t191 * t136 * t117 * x2 + 0.168D3 * t198 - 0.144D3 * 
     #t204 + 0.28D2 * t206 * t136 * t70 * t45 + 0.36D2 * t213 + 0.430D3 
     #* t220 - t228 - t235 + 0.56D2 * t239 + 0.216D3 * t243 - 0.144D3 * 
     #t246 * t124 * t27 * x3 + 0.216D3 * t253 - 0.144D3 * t223 * t124 * 
     #t126 * x3 - 0.288D3 * t123 * t124 * t125 * x3 - 0.16D2 * t266
      t270 = t3 * t5
      t273 = t270 * t10 * t124 * t96
      t278 = t56 * t91 * t124 * t126 * t50
      t282 = t61 ** 2
      t287 = t93 * t27
      t289 = t143 * t287 * t157
      t300 = t172 * t76 * t20 * t38 * x2 * t45 * x3
      t303 = t59 * t27
      t304 = t23 * t200
      t306 = t122 * t15 * t303 * t304
      t309 = t200 * t45
      t310 = t24 * t309
      t312 = t116 * t20 * t27 * t310
      t314 = t115 * t92
      t316 = x1 * t45
      t320 = t6 ** 2
      t323 = t138 * x2
      t335 = 0.144D3 * t314 * t236 * t37 * t224
      t339 = t57 * t303 * t50 * t23 * t200
      t341 = t102 * t15
      t343 = t341 * t287 * t146
      t347 = t206 * t136 * t37 * t309
      t351 = t191 * t136 * t201 * x2
      t356 = 0.748D3 * t273 - 0.12D2 * t278 + 0.36D2 * t115 * t9 * t14 *
     # t134 * t135 * t282 + 0.96D2 * t289 + 0.108D3 * t191 * t136 * t70 
     #* x2 - 0.112D3 * t300 + 0.36D2 * t306 + 0.28D2 * t312 + 0.28D2 * t
     #314 * t93 * t126 * t316 + 0.36D2 * t114 * t320 * t92 * t136 * t37 
     #* t323 - 0.144D3 * t116 * t69 * t70 * x3 - t335 + 0.47D2 * t339 + 
     #0.96D2 * t343 + 0.28D2 * t347 + 0.36D2 * t351 + 0.108D3 * t246 * t
     #124 * t125
      t357 = t125 ** 2
      t361 = t114 * t5
      t369 = z * x1 * t45
      t371 = t102 * t15 * t93 * t96 * t369
      t373 = t316 * t109
      t375 = t314 * t287 * t373
      t377 = t8 * t15
      t380 = t377 * t69 * t62 * x2
      t382 = t122 * t76
      t387 = x3 * x1 * t45
      t389 = t230 * t96 * t387
      t391 = t304 * x3
      t393 = t186 * t303 * t391
      t396 = t314 * t144 * t387
      t399 = t382 * t287 * t387
      t403 = t68 * t69 * t217 * t200
      t407 = t68 * t69 * t62 * t45
      t415 = 0.16D2 * t172 * t236 * t45 * t222 * t224 * t14 * t27
      t422 = 0.16D2 * t172 * t69 * t309 * t75 * x3 * t14 * t27
      t425 = t186 * t59 * t125 * t304
      t431 = t215 * t92
      t435 = t431 * t303 * t50 * t138 * t23
      t439 = t108 * t40 * t125 * t80
      t441 = 0.36D2 * t223 * t124 * t357 + 0.36D2 * t361 * t10 * t124 * 
     #t27 - 0.128D3 * t371 + 0.168D3 * t375 - 0.328D3 * t380 + 0.56D2 * 
     #t382 * t144 * t316 - 0.128D3 * t389 - 0.144D3 * t393 - 0.112D3 * t
     #396 - 0.112D3 * t399 + 0.192D3 * t403 + 0.182D3 * t407 + t415 + t4
     #22 + 0.36D2 * t425 + 0.28D2 * t245 * t15 * t287 * t316 + 0.47D2 * 
     #t435 - 0.240D3 * t439
      t451 = t93 * t50
      t453 = t143 * t451 * t373
      t464 = 0.32D2 * t172 * t60 * t200 * t91 * t109 * t14 * t27
      t468 = t270 * t67
      t474 = 0.444D3 * t54 - 0.256D3 * t65 - 0.62D2 * t73 + 0.544D3 * t8
     #3 - 0.288D3 * t89 + 0.496D3 * t99 + 0.64D2 * t453 + 0.64D2 * t341 
     #* t451 * t231 + t464 + 0.640D3 * t106 - 0.264D3 * t112 + 0.144D3 *
     # t120 + 0.136D3 * t468 * t236 * t217 + 0.216D3 * t148 - 0.108D3 * 
     #t153
      t478 = t102 * t67
      t481 = t478 * t60 * t217 * t45
      t484 = t468 * t451 * t316
      t495 = 0.216D3 * t159 - 0.72D2 * t177 - 0.72D2 * t189 - 0.256D3 * 
     #t481 + 0.64D2 * t484 - 0.344D3 * t198 + 0.256D3 * t204 - 0.72D2 * 
     #t213 - 0.542D3 * t220 + t228 + 0.256D3 * t234 + 0.24D2 * t239 - 0.
     #72D2 * t243 - 0.72D2 * t253 + 0.608D3 * t266
      t503 = t57 * t60 * t217 * x2
      t510 = t478 * t60 * t62
      t515 = t341 * t451 * t387
      t518 = -0.592D3 * t273 - 0.16D2 * t278 - 0.640D3 * t289 + 0.144D3 
     #* t300 + 0.256D3 * t503 - 0.72D2 * t306 - 0.56D2 * t312 + t335 - 0
     #.54D2 * t339 - 0.640D3 * t343 - 0.256D3 * t510 - 0.56D2 * t347 - 0
     #.72D2 * t351 - 0.128D3 * t515 + 0.256D3 * t371
      t528 = t155 * t451 * t109 * x2 * x1
      t531 = t143 * t451 * t97
      t540 = t341 * t236 * t217 * x3
      t542 = -0.344D3 * t375 + 0.604D3 * t380 + 0.496D3 * t389 + 0.256D3
     # * t393 + 0.144D3 * t396 + 0.144D3 * t399 + 0.64D2 * t528 - 0.128D
     #3 * t531 - 0.30D2 * t403 - 0.108D3 * t407 - t415 - t422 - 0.72D2 *
     # t425 - 0.54D2 * t435 - 0.64D2 * t439 - 0.216D3 * t540
      t549 = t59 * t50
      t553 = t20 * t50
      t565 = t3 * t130
      t572 = z ** 2
      t579 = 0.536D3 * t54 + 0.74320D5 * t65 - 0.304D3 * t73 - 0.96D2 * 
     #t57 * t549 * t391 - 0.112D3 * t68 * t553 * t310 - 0.74208D5 * t83 
     #+ 0.74288D5 * t89 - 0.192D3 * t78 * t156 * t150 * x3 - 0.8D1 * t99
     # + 0.128D3 * t453 + t464 + 0.16D2 * t565 * t92 * t553 * t323 * t24
     # + 0.128D3 * t361 * t236 * t45 * t572 * z * t67 - 0.128D3 * t106
      t585 = t14 * t27 * t50
      t629 = 0.64D2 * t112 - 0.16D2 * t565 * t222 * x3 * t138 * t60 * t5
     #85 - 0.384D3 * t468 * t451 * t316 * t572 + 0.32D2 * t8 * t76 * t54
     #9 * t138 * t23 - 0.384D3 * t478 * t549 * t304 * z + 0.48D2 * t377 
     #* t553 * t24 * t200 * x2 + 0.48D2 * t216 * t553 * t24 * t45 * t138
     # + 0.16D2 * t215 * t222 * t109 * x2 * t236 * t585 - 0.96D2 * t431 
     #* t549 * x3 * t138 * t23 - 0.128D3 * t148 + 0.8D1 * t153 - 0.25248
     #D5 * t481 - 0.128D3 * t484 + 0.144D3 * t198 - 0.288D3 * t204
      t649 = 0.28208D5 * t220 - t235 + 0.32D2 * t239 - 0.128D3 * t266 + 
     #0.64D2 * t273 + 0.64D2 * t278 + 0.64D2 * t57 * t549 * t151 + 0.252
     #64D5 * t503 + 0.384D3 * t468 * t451 * t369 + 0.144D3 * t312 - 0.25
     #2D3 * t339 + 0.256D3 * t343 - 0.25296D5 * t510 + 0.144D3 * t347 - 
     #0.64D2 * t515
      t665 = -0.512D3 * t371 + 0.144D3 * t375 - 0.27920D5 * t380 - 0.8D1
     # * t389 - 0.288D3 * t393 + 0.128D3 * t528 - 0.64D2 * t531 - 0.2879
     #2D5 * t403 - 0.29128D5 * t407 - t415 - t422 + 0.288D3 * t478 * t54
     #9 * t304 + 0.4D1 * t435 + 0.128D3 * t439 - 0.80D2 * t540
      rrqg2qgh62J3 = -(wd * (t179 + t268 + t356 + t441) + wd * (t474 + t
     #495 + t518 + t542) + wd * (t579 + t629 + t649 + t665)) / t1 / t50 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t4
      t17 = z + x1 * t4
      t18 = 0.1D1 / t17
      t19 = t15 * t18
      t20 = 0.1D1 - x3
      t21 = t20 ** 2
      t22 = t21 * t20
      t23 = s * t4
      t27 = x2 * x3
      t29 = cos(x4 * 0.3141592653589793D1)
      t30 = x3 * t12
      t34 = Sqrt(t30 * t17 * x2 * t20)
      t36 = 0.2D1 * t29 * t34
      t37 = t20 * t12 * t17 + t27 + t36
      t42 = s - t23 * x1 * t18 * t37 - t23 * t8 * t20
      t45 = t7 * t10 * t19 * t22 * t42
      t47 = t2 * t1
      t48 = t6 * t4
      t49 = t47 * t48
      t51 = t17 ** 2
      t52 = t51 ** 2
      t54 = 0.1D1 / t52 / t17
      t56 = x1 ** 2
      t57 = t56 ** 2
      t60 = t30 * t17 + x2 * t20 - t36
      t61 = t60 ** 2
      t62 = t61 ** 2
      t67 = t3 * t5
      t69 = t20 * t42
      t71 = t67 * t9 * t19 * t69
      t73 = t5 * t4
      t74 = t3 * t73
      t75 = t9 * t8
      t79 = t74 * t75 * t19 * t21 * t42
      t81 = t47 * t6
      t82 = t81 * t10
      t87 = t10 * t8
      t88 = t49 * t87
      t93 = t75 * t15
      t94 = t81 * t93
      t95 = 0.1D1 / t51
      t96 = t95 * t20
      t97 = x1 * t37
      t98 = t97 * x3
      t100 = t94 * t96 * t98
      t102 = t9 * t15
      t106 = z * x1 * t37
      t108 = t74 * t102 * t95 * t69 * t106
      t111 = t7 * t93 * t95
      t112 = x2 * x1
      t115 = t111 * t69 * t112 * z
      t116 = 0.128D3 * t115
      t117 = t8 * t15
      t118 = t7 * t117
      t119 = 0.1D1 / t52
      t120 = t56 * x1
      t121 = t119 * t120
      t122 = t61 * t60
      t125 = t118 * t121 * t122 * t42
      t127 = t3 * t48
      t128 = t10 * t15
      t131 = t27 * x1
      t133 = t127 * t128 * t95 * t69 * t131
      t135 = t6 * t5
      t136 = t47 * t135
      t137 = t136 * t102
      t138 = t54 * t57
      t143 = t6 * t73
      t144 = t47 * t143
      t145 = t144 * t93
      t146 = x2 ** 2
      t157 = t7 * t93
      t158 = t95 * t21
      t160 = t42 * x1 * t37
      t162 = t157 * t158 * t160
      t164 = t61 * t37
      t169 = t60 * t37
      t174 = -0.12D2 * t45 + 0.36D2 * t49 * t8 * t15 * t54 * t57 * t62 +
     # 0.748D3 * t71 - 0.16D2 * t79 - 0.288D3 * t82 * t19 * t21 * x3 - 0
     #.144D3 * t88 * t19 * t22 * x3 - 0.112D3 * t100 - 0.128D3 * t108 - 
     #t116 + 0.10D2 * t125 - 0.128D3 * t133 + 0.108D3 * t137 * t138 * t1
     #22 * x2 + 0.108D3 * t145 * t138 * t61 * t146 - 0.144D3 * t144 * t1
     #28 * t121 * t60 * x3 * t146 - 0.148D3 * t162 + 0.56D2 * t137 * t13
     #8 * t164 * x2 + 0.28D2 * t145 * t138 * t169 * t146
      t175 = t49 * t128
      t176 = x3 ** 2
      t177 = t97 * t176
      t179 = t175 * t96 * t177
      t181 = t49 * t117
      t182 = t37 ** 2
      t183 = t182 * t37
      t186 = t181 * t138 * t60 * t183
      t188 = t127 * t102
      t189 = t61 * t42
      t192 = t188 * t121 * t189 * x2
      t196 = t120 * t60
      t200 = t127 * t102 * t119 * t196 * t42 * t37 * x2
      t202 = t47 * t73
      t203 = t202 * t75
      t208 = 0.1D1 / t51 / t17
      t210 = t127 * t93 * t208
      t211 = t56 * t37
      t212 = t211 * x2
      t214 = t210 * t69 * t212
      t216 = t21 ** 2
      t226 = t47 * t5
      t233 = t118 * t121 * t189 * t37
      t235 = t49 * t93
      t236 = t208 * t56
      t239 = t235 * t236 * t61 * t176
      t241 = t49 * t102
      t246 = t127 * t128
      t247 = t42 * x2
      t248 = t247 * x1
      t250 = t246 * t158 * t248
      t253 = t157 * t96 * t248
      t264 = 0.16D2 * t136 * t121 * t183 * t75 * x3 * t15 * t20
      t266 = t56 * t182
      t268 = t235 * t208 * t21 * t266
      t270 = 0.168D3 * t179 + 0.28D2 * t186 - 0.328D3 * t192 - 0.530D3 *
     # t200 + 0.108D3 * t203 * t19 * t21 + 0.94D2 * t214 + 0.36D2 * t88 
     #* t19 * t216 - 0.288D3 * t136 * t93 * t121 * t61 * x2 * x3 + 0.36D
     #2 * t226 * t9 * t19 * t20 + 0.182D3 * t233 + 0.216D3 * t239 - 0.14
     #4D3 * t241 * t121 * t122 * x3 - 0.148D3 * t250 + 0.96D2 * t253 + 0
     #.108D3 * t82 * t19 * t22 + t264 + 0.36D2 * t268
      t272 = x1 * t95
      t275 = t176 * x3
      t280 = 0.16D2 * t136 * t272 * t37 * t87 * t275 * t15 * t20
      t283 = t235 * t236 * t169 * t176
      t285 = t60 * t182
      t288 = t241 * t121 * t285 * x3
      t291 = t111 * t69 * t98
      t295 = t137 * t138 * t285 * x2
      t297 = t208 * t20
      t298 = t266 * x3
      t300 = t235 * t297 * t298
      t302 = t56 * t60
      t303 = t42 * x3
      t306 = t210 * t302 * t303 * x2
      t309 = t175 * t158 * t98
      t315 = t7 * t102 * t208 * t302 * t303 * t37
      t325 = t18 * t20
      t327 = t74 * t93 * t325 * t303
      t330 = t120 * t183
      t332 = t241 * t119 * t20 * t330
      t344 = t181 * t138 * t61 * t182
      t351 = t136 * t93 * t119 * t196 * x2 * t37 * x3
      t353 = t280 + 0.168D3 * t283 - 0.144D3 * t288 - 0.128D3 * t291 + 0
     #.36D2 * t295 - 0.144D3 * t300 - 0.584D3 * t306 - 0.112D3 * t309 + 
     #0.400D3 * t315 + 0.28D2 * t202 * t102 * t96 * t97 + 0.56D2 * t94 *
     # t158 * t97 - 0.880D3 * t327 + 0.28D2 * t332 + 0.28D2 * t175 * t95
     # * t22 * t97 + 0.28D2 * t181 * t138 * t122 * t37 + 0.36D2 * t344 -
     # 0.112D3 * t351
      t354 = t7 * t102
      t358 = t354 * t297 * t42 * t56 * t182
      t362 = t241 * t121 * t164 * x3
      t364 = t6 ** 2
      t367 = t146 * x2
      t372 = t60 * t42
      t375 = t118 * t121 * t372 * t182
      t377 = t3 * t135
      t378 = t377 * t93
      t381 = t378 * t121 * t372 * t146
      t386 = 0.144D3 * t88 * t19 * t20 * t275
      t387 = t7 * t128
      t390 = t387 * t325 * t42 * t176
      t396 = t136 * t128 * t236 * t60 * t176 * x2
      t400 = t354 * t236 * t189 * x3
      t404 = t81 * t102 * t297 * t266
      t406 = t377 * t128
      t410 = t406 * t297 * t42 * t146 * t56
      t414 = t387 * t18 * t21 * t303
      t418 = t157 * t272 * t372 * t176
      t420 = t74 * t102
      t422 = t420 * t96 * t160
      t427 = 0.144D3 * t175 * t272 * t60 * t275
      t430 = t82 * t19 * t20 * t176
      t438 = t88 * t19 * t21 * t176
      t440 = 0.47D2 * t358 - 0.112D3 * t362 + 0.36D2 * t47 * t364 * t128
     # * t138 * t60 * t367 + 0.192D3 * t375 + 0.430D3 * t381 - t386 + 0.
     #312D3 * t390 + 0.216D3 * t396 + 0.328D3 * t400 + 0.36D2 * t404 + 0
     #.47D2 * t410 - 0.240D3 * t414 + 0.56D2 * t418 + 0.96D2 * t422 - t4
     #27 + 0.216D3 * t430 - 0.144D3 * t203 * t19 * t20 * x3 + 0.216D3 * 
     #t438
      t452 = t74 * t117
      t455 = t452 * t236 * t372 * t37
      t458 = t95 * t42
      t462 = t246 * t458 * t176 * x2 * x1
      t465 = t157 * t458 * t131
      t469 = t354 * t236 * t372 * x2
      t477 = 0.32D2 * t136 * t236 * t182 * t10 * t176 * t15 * t20
      t479 = -0.16D2 * t45 - 0.592D3 * t71 + 0.608D3 * t79 + 0.144D3 * t
     #100 + 0.256D3 * t108 + 0.256D3 * t115 - 0.62D2 * t125 + 0.496D3 * 
     #t133 - 0.256D3 * t455 + 0.216D3 * t162 + 0.64D2 * t462 - 0.128D3 *
     # t465 + 0.256D3 * t469 + t477 - 0.344D3 * t179
      t482 = t420 * t458 * t98
      t485 = t157 * t458 * t177
      t500 = t420 * t272 * t372 * x3
      t502 = -0.56D2 * t186 - 0.128D3 * t482 + 0.64D2 * t485 + 0.64D2 * 
     #t420 * t458 * t112 + 0.604D3 * t192 + 0.444D3 * t200 - 0.108D3 * t
     #214 - 0.108D3 * t233 - 0.72D2 * t239 + 0.216D3 * t250 - 0.640D3 * 
     #t253 - t264 - 0.72D2 * t268 - t280 - 0.216D3 * t500
      t507 = t67 * t117
      t509 = t507 * t458 * t97
      t518 = t452 * t236 * t189
      t524 = -0.344D3 * t283 + 0.256D3 * t288 + 0.496D3 * t291 + 0.64D2 
     #* t509 - 0.72D2 * t295 + 0.256D3 * t300 + 0.544D3 * t306 + 0.144D3
     # * t309 - 0.288D3 * t315 + 0.640D3 * t327 - 0.256D3 * t518 - 0.56D
     #2 * t332 - 0.72D2 * t344 + 0.144D3 * t351 - 0.54D2 * t358
      t541 = 0.144D3 * t362 - 0.30D2 * t375 - 0.542D3 * t381 + t386 - 0.
     #264D3 * t390 - 0.72D2 * t396 - 0.256D3 * t400 - 0.72D2 * t404 - 0.
     #54D2 * t410 - 0.64D2 * t414 + 0.24D2 * t418 - 0.640D3 * t422 + t42
     #7 - 0.72D2 * t430 + 0.136D3 * t507 * t272 * t372 - 0.72D2 * t438
      t548 = t208 * t42
      t565 = 0.64D2 * t45 + 0.64D2 * t71 - 0.128D3 * t79 - 0.384D3 * t45
     #2 * t548 * t266 * z - 0.512D3 * t108 - t116 - 0.304D3 * t125 - 0.8
     #D1 * t133 - 0.25248D5 * t455 - 0.128D3 * t162 - 0.192D3 * t210 * t
     #247 * t211 * x3 + 0.128D3 * t462 - 0.64D2 * t465 + 0.25264D5 * t46
     #9
      t574 = t3 * t143
      t579 = t15 * t20 * t42
      t583 = t119 * t42
      t595 = z ** 2
      t601 = t477 + 0.144D3 * t179 + 0.144D3 * t186 - 0.64D2 * t482 + 0.
     #128D3 * t485 - 0.27920D5 * t192 + 0.536D3 * t200 + 0.8D1 * t214 - 
     #0.29128D5 * t233 - 0.16D2 * t574 * t87 * x3 * t146 * t236 * t579 +
     # 0.48D2 * t188 * t583 * t120 * t182 * x2 + 0.48D2 * t378 * t583 * 
     #t120 * t37 * t146 + 0.128D3 * t226 * t272 * t37 * t595 * z * t117 
     #- t264 - t280
      t631 = -0.80D2 * t500 + 0.144D3 * t283 - 0.288D3 * t288 - 0.8D1 * 
     #t291 - 0.128D3 * t509 - 0.288D3 * t300 - 0.74208D5 * t306 + 0.7428
     #8D5 * t315 - 0.128D3 * t327 + 0.64D2 * t354 * t548 * t212 + 0.384D
     #3 * t507 * t458 * t106 - 0.384D3 * t507 * t458 * t97 * t595 - 0.25
     #296D5 * t518 + 0.288D3 * t452 * t548 * t266 + 0.32D2 * t127 * t93 
     #* t548 * t146 * t56
      t664 = 0.144D3 * t332 + 0.16D2 * t377 * t87 * t176 * x2 * t272 * t
     #579 - 0.96D2 * t406 * t548 * x3 * t146 * t56 - 0.252D3 * t358 - 0.
     #28792D5 * t375 + 0.28208D5 * t381 + 0.64D2 * t390 - 0.112D3 * t118
     # * t583 * t330 + 0.16D2 * t574 * t128 * t583 * t367 * t120 + 0.743
     #20D5 * t400 + 0.4D1 * t410 + 0.128D3 * t414 + 0.32D2 * t418 + 0.25
     #6D3 * t422 - 0.96D2 * t354 * t548 * t298
      rrqg2qgh62J4 = -(wd * (t174 + t270 + t353 + t440) + wd * (t479 + t
     #502 + t524 + t541) + wd * (t565 + t601 + t631 + t664)) / t1 / t42 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 * t8
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = t10 * t14
      t16 = t7 * t15
      t18 = z + x1 * t4
      t19 = t18 ** 2
      t20 = 0.1D1 / t19
      t21 = t20 * x1
      t22 = x3 * t11
      t24 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t31 = Sqrt(t22 * t18 * x2 * t24)
      t33 = 0.2D1 * t27 * t31
      t34 = t22 * t18 + x2 * t24 - t33
      t35 = s * t4
      t36 = 0.1D1 / t18
      t40 = x2 * x3
      t41 = t24 * t11 * t18 + t40 + t33
      t46 = s - t35 * x1 * t36 * t41 - t35 * t8 * t24
      t47 = t34 * t46
      t48 = x3 ** 2
      t51 = t16 * t21 * t47 * t48
      t53 = t2 * t1
      t54 = t5 * t4
      t55 = t6 * t54
      t56 = t53 * t55
      t57 = t9 ** 2
      t58 = t57 * t14
      t60 = t19 ** 2
      t61 = 0.1D1 / t60
      t62 = x1 ** 2
      t63 = t62 * x1
      t64 = t61 * t63
      t66 = x2 ** 2
      t71 = t6 * t5
      t72 = t53 * t71
      t75 = t63 * t34
      t79 = t72 * t15 * t61 * t75 * x2 * t41 * x3
      t82 = t41 ** 2
      t83 = t82 * t41
      t89 = 0.16D2 * t72 * t64 * t83 * t10 * x3 * t14 * t24
      t90 = t6 * t4
      t91 = t53 * t90
      t92 = t57 * t8
      t93 = t91 * t92
      t94 = t14 * t36
      t95 = t24 ** 2
      t96 = t95 * t24
      t101 = t53 * t6
      t102 = t101 * t57
      t105 = t102 * t94 * t24 * t48
      t107 = t53 * t54
      t108 = t107 * t10
      t113 = t3 * t54
      t117 = t113 * t10 * t94 * t95 * t46
      t125 = 0.1D1 / t60 / t18
      t127 = t62 ** 2
      t128 = t34 ** 2
      t129 = t128 ** 2
      t134 = t3 * t5
      t136 = t24 * t46
      t138 = t134 * t9 * t94 * t136
      t143 = t7 * t57 * t94 * t96 * t46
      t145 = t3 * t90
      t146 = t145 * t58
      t147 = t95 * t20
      t148 = t46 * x2
      t149 = t148 * x1
      t151 = t146 * t147 * t149
      t153 = t20 * t24
      t155 = t16 * t153 * t149
      t157 = t9 * t14
      t159 = 0.1D1 / t19 / t18
      t162 = t62 * t34
      t163 = t46 * x3
      t166 = t7 * t157 * t159 * t162 * t163 * t41
      t168 = t91 * t58
      t169 = x1 * t41
      t170 = t169 * t48
      t172 = t168 * t153 * t170
      t174 = t8 * t14
      t175 = t91 * t174
      t176 = t125 * t127
      t179 = t175 * t176 * t34 * t83
      t181 = 0.56D2 * t51 - 0.144D3 * t56 * t58 * t64 * t34 * x3 * t66 -
     # 0.112D3 * t79 + t89 - 0.144D3 * t93 * t94 * t96 * x3 + 0.216D3 * 
     #t105 - 0.144D3 * t108 * t94 * t24 * x3 - 0.16D2 * t117 - 0.288D3 *
     # t102 * t94 * t95 * x3 + 0.36D2 * t91 * t8 * t14 * t125 * t127 * t
     #129 + 0.748D3 * t138 - 0.12D2 * t143 - 0.148D3 * t151 + 0.96D2 * t
     #155 + 0.400D3 * t166 + 0.168D3 * t172 + 0.28D2 * t179
      t183 = t159 * t24
      t184 = t62 * t82
      t186 = t101 * t157 * t183 * t184
      t188 = t91 * t157
      t190 = t63 * t83
      t192 = t188 * t61 * t24 * t190
      t194 = t169 * x3
      t196 = t168 * t147 * t194
      t198 = t7 * t174
      t201 = t198 * t64 * t47 * t82
      t203 = t3 * t71
      t204 = t203 * t15
      t207 = t204 * t64 * t47 * t66
      t209 = t113 * t157
      t211 = t46 * x1 * t41
      t213 = t209 * t153 * t211
      t216 = t16 * t147 * t211
      t218 = t7 * t58
      t219 = t36 * t24
      t222 = t218 * t219 * t46 * t48
      t226 = t40 * x1
      t228 = t145 * t58 * t20 * t136 * t226
      t231 = t7 * t15 * t20
      t233 = t231 * t136 * t194
      t235 = t91 * t15
      t236 = t184 * x3
      t238 = t235 * t183 * t236
      t240 = t203 * t58
      t244 = t240 * t183 * t46 * t66 * t62
      t248 = t218 * t36 * t95 * t163
      t250 = x2 * x1
      t253 = t231 * t136 * t250 * z
      t254 = 0.128D3 * t253
      t259 = t6 ** 2
      t262 = t66 * x2
      t267 = t72 * t157
      t268 = t128 * t34
      t273 = 0.36D2 * t186 + 0.28D2 * t192 - 0.112D3 * t196 + 0.192D3 * 
     #t201 + 0.430D3 * t207 + 0.96D2 * t213 - 0.148D3 * t216 + 0.312D3 *
     # t222 - 0.128D3 * t228 - 0.128D3 * t233 - 0.144D3 * t238 + 0.47D2 
     #* t244 - 0.240D3 * t248 - t254 + 0.28D2 * t168 * t20 * t96 * t169 
     #+ 0.36D2 * t53 * t259 * t58 * t176 * t34 * t262 + 0.108D3 * t267 *
     # t176 * t268 * x2
      t275 = t128 * t41
      t278 = t188 * t64 * t275 * x3
      t286 = t175 * t176 * t128 * t82
      t288 = t56 * t15
      t294 = t145 * t15 * t159
      t295 = t62 * t41
      t296 = t295 * x2
      t298 = t294 * t136 * t296
      t300 = t7 * t157
      t304 = t300 * t183 * t46 * t62 * t82
      t306 = t34 * t82
      t309 = t267 * t176 * t306 * x2
      t315 = t34 * t41
      t320 = t159 * t62
      t323 = t235 * t320 * t315 * t48
      t327 = t188 * t64 * t306 * x3
      t331 = t198 * t64 * t268 * t46
      t333 = t128 * t46
      t336 = t198 * t64 * t333 * t41
      t340 = t235 * t159 * t95 * t184
      t346 = t145 * t157
      t349 = t346 * t64 * t333 * x2
      t356 = t145 * t157 * t61 * t75 * t46 * t41 * x2
      t358 = -0.112D3 * t278 + 0.28D2 * t175 * t176 * t268 * t41 + 0.36D
     #2 * t286 + 0.108D3 * t288 * t176 * t128 * t66 + 0.94D2 * t298 + 0.
     #47D2 * t304 + 0.36D2 * t309 + 0.56D2 * t267 * t176 * t275 * x2 + 0
     #.28D2 * t288 * t176 * t315 * t66 + 0.168D3 * t323 - 0.144D3 * t327
     # + 0.10D2 * t331 + 0.182D3 * t336 + 0.36D2 * t340 + 0.28D2 * t107 
     #* t157 * t153 * t169 - 0.328D3 * t349 - 0.530D3 * t356
      t359 = t101 * t15
      t361 = t359 * t153 * t194
      t366 = z * x1 * t41
      t368 = t113 * t157 * t20 * t136 * t366
      t372 = t294 * t162 * t163 * x2
      t381 = t72 * t58 * t320 * t34 * t48 * x2
      t391 = t235 * t320 * t128 * t48
      t397 = t48 * x3
      t401 = 0.144D3 * t168 * t21 * t34 * t397
      t405 = t95 ** 2
      t409 = t53 * t5
      t419 = t113 * t15 * t219 * t163
      t423 = t300 * t320 * t333 * x3
      t427 = t93 * t94 * t95 * t48
      t432 = 0.144D3 * t93 * t94 * t24 * t397
      t439 = 0.16D2 * t72 * t21 * t41 * t92 * t397 * t14 * t24
      t440 = -0.112D3 * t361 - 0.128D3 * t368 - 0.584D3 * t372 + 0.108D3
     # * t102 * t94 * t96 + 0.216D3 * t381 - 0.288D3 * t72 * t15 * t64 *
     # t128 * x2 * x3 + 0.216D3 * t391 - 0.144D3 * t188 * t64 * t268 * x
     #3 - t401 + 0.108D3 * t108 * t94 * t95 + 0.36D2 * t93 * t94 * t405 
     #+ 0.36D2 * t409 * t9 * t94 * t24 + 0.56D2 * t359 * t147 * t169 - 0
     #.880D3 * t419 + 0.328D3 * t423 + 0.216D3 * t427 - t432 + t439
      t457 = t300 * t320 * t47 * x2
      t461 = 0.24D2 * t51 + 0.144D3 * t79 - t89 - 0.72D2 * t105 + 0.608D
     #3 * t117 - 0.592D3 * t138 - 0.16D2 * t143 + 0.216D3 * t151 - 0.640
     #D3 * t155 - 0.288D3 * t166 - 0.344D3 * t172 - 0.56D2 * t179 + 0.25
     #6D3 * t457 - 0.72D2 * t186 - 0.56D2 * t192
      t474 = t134 * t174
      t475 = t20 * t46
      t477 = t474 * t475 * t169
      t479 = t113 * t174
      t481 = t479 * t320 * t333
      t489 = 0.32D2 * t72 * t320 * t82 * t57 * t48 * t14 * t24
      t490 = 0.144D3 * t196 - 0.30D2 * t201 - 0.542D3 * t207 - 0.640D3 *
     # t213 + 0.216D3 * t216 - 0.264D3 * t222 + 0.496D3 * t228 + 0.496D3
     # * t233 + 0.256D3 * t238 - 0.54D2 * t244 - 0.64D2 * t248 + 0.256D3
     # * t253 + 0.64D2 * t477 - 0.256D3 * t481 + t489
      t499 = t479 * t320 * t47 * t41
      t502 = t209 * t475 * t194
      t505 = t16 * t475 * t170
      t516 = t209 * t21 * t47 * x3
      t521 = t146 * t475 * t48 * x2 * x1
      t523 = 0.144D3 * t278 - 0.72D2 * t286 - 0.108D3 * t298 - 0.54D2 * 
     #t304 - 0.72D2 * t309 - 0.256D3 * t499 - 0.128D3 * t502 + 0.64D2 * 
     #t505 - 0.344D3 * t323 + 0.256D3 * t327 - 0.62D2 * t331 - 0.108D3 *
     # t336 + 0.64D2 * t209 * t475 * t250 - 0.216D3 * t516 + 0.64D2 * t5
     #21
      t525 = t16 * t475 * t226
      t541 = -0.128D3 * t525 - 0.72D2 * t340 + 0.604D3 * t349 + 0.444D3 
     #* t356 + 0.144D3 * t361 + 0.256D3 * t368 + 0.544D3 * t372 - 0.72D2
     # * t381 - 0.72D2 * t391 + t401 + 0.640D3 * t419 - 0.256D3 * t423 -
     # 0.72D2 * t427 + t432 + 0.136D3 * t474 * t21 * t47 - t439
      t558 = 0.32D2 * t51 - t89 - 0.128D3 * t117 + 0.64D2 * t138 + 0.64D
     #2 * t143 + 0.74288D5 * t166 + 0.144D3 * t172 + 0.144D3 * t179 + 0.
     #25264D5 * t457 + 0.144D3 * t192 - 0.28792D5 * t201 + 0.28208D5 * t
     #207 + 0.256D3 * t213 - 0.128D3 * t216
      t566 = t61 * t46
      t575 = t159 * t46
      t583 = z ** 2
      t589 = 0.64D2 * t222 - 0.8D1 * t228 - 0.8D1 * t233 - 0.288D3 * t23
     #8 + 0.4D1 * t244 + 0.128D3 * t248 - t254 - 0.128D3 * t477 - 0.112D
     #3 * t198 * t566 * t190 - 0.192D3 * t294 * t148 * t295 * x3 + 0.32D
     #2 * t145 * t15 * t575 * t66 * t62 + 0.384D3 * t474 * t475 * t366 -
     # 0.384D3 * t474 * t475 * t169 * t583 - 0.25296D5 * t481 + t489
      t601 = t3 * t55
      t606 = t14 * t24 * t46
      t629 = 0.64D2 * t300 * t575 * t296 + 0.48D2 * t346 * t566 * t63 * 
     #t82 * x2 + 0.8D1 * t298 - 0.252D3 * t304 - 0.16D2 * t601 * t92 * x
     #3 * t66 * t320 * t606 - 0.25248D5 * t499 - 0.64D2 * t502 + 0.128D3
     # * t505 + 0.144D3 * t323 - 0.288D3 * t327 + 0.48D2 * t204 * t566 *
     # t63 * t41 * t66 - 0.384D3 * t479 * t575 * t184 * z - 0.96D2 * t30
     #0 * t575 * t236 - 0.304D3 * t331 - 0.29128D5 * t336
      t664 = 0.16D2 * t601 * t58 * t566 * t262 * t63 + 0.128D3 * t409 * 
     #t21 * t41 * t583 * z * t174 + 0.288D3 * t479 * t575 * t184 + 0.16D
     #2 * t203 * t92 * t48 * x2 * t21 * t606 - 0.96D2 * t240 * t575 * x3
     # * t66 * t62 - 0.80D2 * t516 + 0.128D3 * t521 - 0.64D2 * t525 - 0.
     #27920D5 * t349 + 0.536D3 * t356 - 0.512D3 * t368 - 0.74208D5 * t37
     #2 - 0.128D3 * t419 + 0.74320D5 * t423 - t439
      rrqg2qgh62J5 = -(wd * (t181 + t273 + t358 + t440) + wd * (t461 + t
     #490 + t523 + t541) + wd * (t558 + t589 + t629 + t664)) / t1 / t46 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = t10 * t14
      t17 = z + x1 * t4
      t18 = t17 ** 2
      t19 = t18 ** 2
      t20 = 0.1D1 / t19
      t23 = x1 ** 2
      t24 = t23 * x1
      t25 = x3 * t11
      t27 = 0.1D1 - x3
      t30 = cos(x4 * 0.3141592653589793D1)
      t34 = Sqrt(t25 * t17 * x2 * t27)
      t36 = 0.2D1 * t30 * t34
      t37 = t25 * t17 + x2 * t27 - t36
      t38 = t24 * t37
      t39 = s * t4
      t40 = 0.1D1 / t17
      t45 = t27 * t11 * t17 + x2 * x3 + t36
      t50 = s - t39 * x1 * t40 * t45 - t39 * t9 * t27
      t54 = t8 * t15 * t20 * t38 * t50 * t45 * x2
      t56 = t3 * t5
      t57 = t9 * t14
      t58 = t56 * t57
      t59 = 0.1D1 / t18
      t60 = t59 * t50
      t61 = x1 * t45
      t63 = t58 * t60 * t61
      t65 = t10 ** 2
      t66 = t65 * t14
      t67 = t8 * t66
      t68 = x3 ** 2
      t72 = t67 * t60 * t68 * x2 * x1
      t74 = t3 * t6
      t75 = t10 * t9
      t76 = t75 * t14
      t77 = t74 * t76
      t78 = x2 * x1
      t79 = t78 * x3
      t81 = t77 * t60 * t79
      t83 = t74 * t57
      t84 = t20 * t24
      t85 = t37 ** 2
      t86 = t85 * t50
      t89 = t83 * t84 * t86 * t45
      t91 = t74 * t15
      t93 = 0.1D1 / t18 / t17
      t94 = t93 * t27
      t96 = t45 ** 2
      t99 = t91 * t94 * t50 * t23 * t96
      t101 = t2 * t1
      t102 = t101 * t7
      t103 = t102 * t76
      t104 = t23 * t96
      t105 = t104 * x3
      t107 = t103 * t94 * t105
      t109 = t102 * t66
      t110 = t27 ** 2
      t111 = t59 * t110
      t112 = t61 * x3
      t116 = t5 * t4
      t117 = t3 * t116
      t118 = t117 * t57
      t119 = t93 * t23
      t121 = t118 * t119 * t86
      t124 = t8 * t76 * t93
      t125 = t23 * t37
      t126 = t50 * x3
      t129 = t124 * t125 * t126 * x2
      t131 = t59 * t27
      t132 = t50 * x2
      t133 = t132 * x1
      t137 = t101 * t6
      t144 = t27 * t50
      t146 = z * x1 * t45
      t148 = t117 * t15 * t59 * t144 * t146
      t152 = t91 * t119 * t86 * x3
      t157 = t83 * t84 * t85 * t37 * t50
      t159 = 0.444D3 * t54 + 0.64D2 * t63 + 0.64D2 * t72 - 0.128D3 * t81
     # - 0.108D3 * t89 - 0.54D2 * t99 + 0.256D3 * t107 + 0.144D3 * t109 
     #* t111 * t112 - 0.256D3 * t121 + 0.544D3 * t129 - 0.640D3 * t77 * 
     #t131 * t133 + 0.144D3 * t137 * t76 * t131 * t112 + 0.256D3 * t148 
     #- 0.256D3 * t152 - 0.62D2 * t157
      t160 = t74 * t66
      t163 = t160 * t40 * t110 * t126
      t169 = t74 * t15 * t93 * t125 * t126 * t45
      t171 = t61 * t68
      t173 = t109 * t131 * t171
      t175 = t102 * t15
      t181 = t37 * t50
      t184 = t83 * t84 * t181 * t96
      t186 = t117 * t15
      t188 = t186 * t60 * t112
      t191 = t77 * t60 * t171
      t197 = t74 * t76 * t59
      t200 = t197 * t144 * t78 * z
      t202 = t59 * x1
      t205 = t77 * t202 * t181 * t68
      t207 = t6 * t5
      t208 = t3 * t207
      t209 = t208 * t76
      t210 = x2 ** 2
      t213 = t209 * t84 * t181 * t210
      t215 = t8 * t15
      t218 = t215 * t84 * t86 * x2
      t223 = t65 * t9
      t224 = t102 * t223
      t225 = t14 * t40
      t226 = t68 * x3
      t236 = -0.64D2 * t163 - 0.288D3 * t169 - 0.344D3 * t173 + 0.144D3 
     #* t175 * t84 * t85 * t45 * x3 - 0.30D2 * t184 - 0.128D3 * t188 + 0
     #.64D2 * t191 + 0.64D2 * t186 * t60 * t78 + 0.256D3 * t200 + 0.24D2
     # * t205 - 0.542D3 * t213 + 0.604D3 * t218 + 0.136D3 * t58 * t202 *
     # t181 + 0.144D3 * t224 * t225 * t27 * t226 - 0.72D2 * t137 * t65 *
     # t225 * t27 * t68
      t245 = t117 * t75 * t225 * t110 * t50
      t251 = t74 * t65 * t225 * t110 * t27 * t50
      t255 = t56 * t10 * t225 * t144
      t257 = t23 * t45
      t258 = t257 * x2
      t260 = t124 * t144 * t258
      t266 = t50 * x1 * t45
      t268 = t186 * t131 * t266
      t270 = t101 * t207
      t272 = t96 * t45
      t278 = 0.16D2 * t270 * t84 * t272 * t75 * x3 * t14 * t27
      t284 = t40 * t27
      t286 = t117 * t76 * t284 * t126
      t290 = t186 * t202 * t181 * x3
      t297 = t24 * t272
      t299 = t175 * t20 * t27 * t297
      t311 = -0.72D2 * t224 * t225 * t110 * t68 + 0.608D3 * t245 - 0.16D
     #2 * t251 - 0.592D3 * t255 - 0.108D3 * t260 + 0.216D3 * t67 * t111 
     #* t133 - 0.640D3 * t268 - t278 - 0.72D2 * t103 * t93 * t110 * t104
     # + 0.640D3 * t286 - 0.216D3 * t290 - 0.72D2 * t137 * t15 * t94 * t
     #104 - 0.56D2 * t299 - 0.72D2 * t270 * t66 * t119 * t37 * t68 * x2 
     #- 0.72D2 * t103 * t119 * t85 * t68
      t318 = t91 * t119 * t181 * x2
      t322 = t118 * t119 * t181 * t45
      t327 = t103 * t119 * t37 * t45 * t68
      t329 = t37 * t96
      t332 = t175 * t84 * t329 * x3
      t334 = t102 * t57
      t337 = t23 ** 2
      t338 = 0.1D1 / t19 / t17 * t337
      t355 = t208 * t66
      t359 = t355 * t94 * t50 * t210 * t23
      t367 = 0.32D2 * t270 * t119 * t96 * t65 * t68 * t14 * t27
      t370 = t160 * t284 * t50 * t68
      t375 = t8 * t66 * t59 * t144 * t79
      t378 = t197 * t144 * t112
      t382 = t334 * t338 * t37 * t272
      t385 = t77 * t111 * t266
      t393 = 0.16D2 * t270 * t202 * t45 * t223 * t226 * t14 * t27
      t394 = 0.144D3 * t109 * t202 * t37 * t226 + 0.256D3 * t318 - 0.256
     #D3 * t322 - 0.344D3 * t327 + 0.256D3 * t332 - 0.72D2 * t334 * t338
     # * t85 * t96 - 0.72D2 * t270 * t15 * t338 * t329 * x2 + 0.144D3 * 
     #t270 * t76 * t20 * t38 * x2 * t45 * x3 - 0.54D2 * t359 + t367 - 0.
     #264D3 * t370 + 0.496D3 * t375 + 0.496D3 * t378 - 0.56D2 * t382 + 0
     #.216D3 * t385 - t393
      t400 = t93 * t50
      t417 = z ** 2
      t426 = 0.536D3 * t54 - 0.128D3 * t63 - 0.96D2 * t355 * t400 * x3 *
     # t210 * t23 - 0.192D3 * t124 * t132 * t257 * x3 + 0.128D3 * t72 - 
     #0.64D2 * t81 - 0.29128D5 * t89 - 0.252D3 * t99 + 0.384D3 * t58 * t
     #60 * t146 - 0.384D3 * t58 * t60 * t61 * t417 - 0.288D3 * t107 - 0.
     #25296D5 * t121 - 0.74208D5 * t129 - 0.512D3 * t148
      t434 = t3 * t6 * t116
      t439 = t14 * t27 * t50
      t455 = 0.74320D5 * t152 - 0.304D3 * t157 + 0.128D3 * t163 + 0.7428
     #8D5 * t169 + 0.144D3 * t173 - 0.28792D5 * t184 - 0.16D2 * t434 * t
     #223 * x3 * t210 * t119 * t439 + 0.288D3 * t118 * t400 * t104 - 0.6
     #4D2 * t188 + 0.128D3 * t191 - 0.128D3 * t200 + 0.32D2 * t205 + 0.2
     #8208D5 * t213 - 0.27920D5 * t218 + 0.64D2 * t91 * t400 * t258
      t457 = t20 * t50
      t487 = 0.48D2 * t215 * t457 * t24 * t96 * x2 + 0.48D2 * t209 * t45
     #7 * t24 * t45 * t210 - 0.128D3 * t245 + 0.64D2 * t251 + 0.64D2 * t
     #255 + 0.8D1 * t260 + 0.256D3 * t268 - t278 - 0.128D3 * t286 - 0.80
     #D2 * t290 - 0.384D3 * t118 * t400 * t104 * z - 0.96D2 * t91 * t400
     # * t105 - 0.112D3 * t83 * t457 * t297 + 0.144D3 * t299 + 0.25264D5
     # * t318
      t521 = -0.25248D5 * t322 + 0.16D2 * t434 * t66 * t457 * t210 * x2 
     #* t24 + 0.128D3 * t101 * t5 * t202 * t45 * t417 * z * t57 + 0.144D
     #3 * t327 - 0.288D3 * t332 + 0.16D2 * t208 * t223 * t68 * x2 * t202
     # * t439 + 0.4D1 * t359 + t367 + 0.32D2 * t8 * t76 * t400 * t210 * 
     #t23 + 0.64D2 * t370 - 0.8D1 * t375 - 0.8D1 * t378 + 0.144D3 * t382
     # - 0.128D3 * t385 - t393
      rrqg2qgh62J6 = -(wd * (t159 + t236 + t311 + t394) + wd * (t426 + t
     #455 + t487 + t521)) / t1 / t50 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh62J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t4
      t17 = z + x1 * t4
      t18 = 0.1D1 / t17
      t19 = t15 * t18
      t20 = 0.1D1 - x3
      t21 = t20 ** 2
      t23 = s * t4
      t29 = cos(x4 * 0.3141592653589793D1)
      t30 = x3 * t12
      t34 = Sqrt(t30 * t17 * x2 * t20)
      t36 = 0.2D1 * t29 * t34
      t37 = t20 * t12 * t17 + x2 * x3 + t36
      t42 = s - t23 * x1 * t18 * t37 - t23 * t8 * t20
      t47 = t3 * t5
      t49 = t20 * t42
      t53 = t5 * t4
      t54 = t3 * t53
      t55 = t9 * t8
      t61 = t2 * t1
      t62 = t6 * t4
      t63 = t61 * t62
      t64 = t10 * t15
      t66 = t17 ** 2
      t67 = 0.1D1 / t66
      t68 = t67 * t20
      t69 = x1 * t37
      t70 = x3 ** 2
      t71 = t69 * t70
      t75 = t9 * t15
      t77 = 0.1D1 / t66 / t17
      t80 = x1 ** 2
      t83 = t30 * t17 + x2 * t20 - t36
      t84 = t80 * t83
      t85 = t42 * x3
      t90 = t3 * t62
      t91 = t55 * t15
      t93 = t77 * t42
      t94 = x2 ** 2
      t99 = t7 * t75
      t100 = t77 * t80
      t101 = t83 ** 2
      t102 = t101 * t42
      t107 = t8 * t15
      t108 = t7 * t107
      t109 = t66 ** 2
      t110 = 0.1D1 / t109
      t111 = t80 * x1
      t112 = t110 * t111
      t118 = t47 * t107
      t119 = t67 * t42
      t123 = t7 * t64
      t128 = t63 * t75
      t130 = t37 ** 2
      t131 = t130 * t37
      t132 = t111 * t131
      t136 = t6 * t5
      t137 = t3 * t136
      t138 = t137 * t64
      t139 = t77 * t20
      t145 = t7 * t91
      t146 = x2 * x1
      t147 = t146 * x3
      t151 = t54 * t107
      t152 = t83 * t42
      t157 = 0.64D2 * t7 * t10 * t19 * t21 * t20 * t42 + 0.64D2 * t47 * 
     #t9 * t19 * t49 - 0.128D3 * t54 * t55 * t19 * t21 * t42 + 0.144D3 *
     # t63 * t64 * t68 * t71 + 0.74288D5 * t7 * t75 * t77 * t84 * t85 * 
     #t37 + 0.32D2 * t90 * t91 * t93 * t94 * t80 + 0.74320D5 * t99 * t10
     #0 * t102 * x3 - 0.304D3 * t108 * t112 * t101 * t83 * t42 - 0.128D3
     # * t118 * t119 * t69 + 0.128D3 * t123 * t18 * t21 * t85 + 0.144D3 
     #* t128 * t110 * t20 * t132 + 0.4D1 * t138 * t139 * t42 * t94 * t80
     # - 0.64D2 * t145 * t119 * t147 - 0.25248D5 * t151 * t100 * t152 * 
     #t37
      t158 = t137 * t91
      t163 = t80 * t130
      t167 = t110 * t42
      t183 = t3 * t6 * t53
      t184 = t10 * t8
      t189 = t15 * t20 * t42
      t198 = t61 * t136
      t199 = x1 * t67
      t216 = t90 * t91 * t77
      t217 = t80 * t37
      t218 = t217 * x2
      t228 = t80 ** 2
      t236 = z ** 2
      t242 = t69 * z
      t250 = 0.28208D5 * t158 * t112 * t152 * t94 + 0.288D3 * t151 * t93
     # * t163 + 0.48D2 * t158 * t167 * t111 * t37 * t94 - 0.384D3 * t151
     # * t93 * t163 * z - 0.8D1 * t90 * t64 * t67 * t49 * t147 - 0.16D2 
     #* t183 * t184 * x3 * t94 * t100 * t189 - 0.96D2 * t138 * t93 * x3 
     #* t94 * t80 - 0.16D2 * t198 * t199 * t37 * t184 * t70 * x3 * t15 *
     # t20 - 0.16D2 * t198 * t112 * t131 * t55 * x3 * t15 * t20 + 0.8D1 
     #* t216 * t49 * t218 - 0.25296D5 * t151 * t100 * t102 + 0.144D3 * t
     #63 * t107 / t109 / t17 * t228 * t83 * t131 + 0.128D3 * t61 * t5 * 
     #t199 * t37 * t236 * z * t107 + 0.384D3 * t118 * t119 * t242 - 0.29
     #128D5 * t108 * t112 * t102 * t37
      t264 = t7 * t91 * t67
      t273 = t54 * t75
      t275 = x3 * x1 * t37
      t289 = t63 * t91
      t298 = t90 * t75
      t329 = -0.252D3 * t99 * t139 * t42 * t80 * t130 + 0.16D2 * t137 * 
     #t184 * t70 * x2 * t199 * t189 - 0.128D3 * t264 * t49 * t146 * z + 
     #0.25264D5 * t99 * t100 * t152 * x2 - 0.64D2 * t273 * t119 * t275 +
     # 0.128D3 * t145 * t119 * t71 + 0.32D2 * t198 * t100 * t130 * t10 *
     # t70 * t15 * t20 + 0.144D3 * t289 * t100 * t83 * t37 * t70 + 0.64D
     #2 * t99 * t93 * t218 + 0.48D2 * t298 * t167 * t111 * t130 * x2 - 0
     #.8D1 * t264 * t49 * t275 - 0.28792D5 * t108 * t112 * t152 * t130 -
     # 0.288D3 * t128 * t112 * t83 * t130 * x3 - 0.192D3 * t216 * t42 * 
     #x2 * t217 * x3 + 0.536D3 * t90 * t75 * t110 * t111 * t83 * t42 * t
     #37 * x2
      t330 = t163 * x3
      t349 = t42 * x1 * t37
      t383 = t18 * t20
      t395 = -0.96D2 * t99 * t93 * t330 - 0.384D3 * t118 * t119 * t69 * 
     #t236 - 0.80D2 * t273 * t199 * t152 * x3 + 0.128D3 * t90 * t64 * t1
     #19 * t70 * x2 * x1 + 0.256D3 * t273 * t68 * t349 - 0.27920D5 * t29
     #8 * t112 * t102 * x2 - 0.112D3 * t108 * t167 * t132 + 0.16D2 * t18
     #3 * t64 * t167 * t94 * x2 * t111 - 0.288D3 * t289 * t139 * t330 - 
     #0.512D3 * t54 * t75 * t67 * t49 * t242 + 0.32D2 * t145 * t199 * t1
     #52 * t70 - 0.74208D5 * t216 * t84 * t85 * x2 - 0.128D3 * t54 * t91
     # * t383 * t85 + 0.64D2 * t123 * t383 * t42 * t70 - 0.128D3 * t145 
     #* t67 * t21 * t349
      rrqg2qgh62J7 = -wd * (t157 + t250 + t329 + t395) / t1 / t42 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = x1 ** 2
      t9 = t8 * x1
      t11 = z + x1 * t4
      t12 = t11 ** 2
      t13 = 0.1D1 / t12
      t14 = t9 * t13
      t15 = t7 * t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t20 = x2 * t19
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t11 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t11 + t20 - t28
      t30 = s * t4
      t31 = 0.1D1 / t11
      t35 = 0.1D1 - x1
      t36 = t35 * x3
      t38 = s - t30 * x1 * t31 * t29 - t30 * t36
      t39 = t29 * t38
      t40 = x2 * t35
      t44 = t6 * t4
      t45 = t3 * t44
      t46 = t8 ** 2
      t48 = 0.1D1 / t12 / t11
      t49 = t46 * t48
      t50 = t45 * t49
      t51 = t29 ** 2
      t52 = t51 * t38
      t56 = t6 * t5
      t57 = t3 * t56
      t58 = t35 ** 2
      t59 = t58 * t35
      t60 = t9 * t59
      t62 = t19 * t38
      t63 = x2 ** 2
      t64 = t63 * t13
      t74 = t2 * t1
      t75 = t74 * t44
      t76 = t8 * t59
      t77 = t75 * t76
      t78 = t19 ** 2
      t79 = t78 * x3
      t82 = x2 * x3
      t83 = t19 * t16 * t11 + t82 + t28
      t84 = t31 * t83
      t89 = t7 * t8 * t58
      t100 = t83 ** 2
      t101 = t13 * t100
      t107 = x3 ** 2
      t112 = t74 * t56
      t113 = t112 * t60
      t119 = t19 * t107
      t123 = t5 * t4
      t124 = t74 * t123
      t126 = t13 * t29
      t130 = t46 * x1
      t131 = t75 * t130
      t132 = t12 ** 2
      t133 = 0.1D1 / t132
      t135 = t100 * t83
      t139 = t7 * t46
      t140 = t51 * t29
      t141 = t48 * t140
      t147 = t31 * t29
      t155 = t74 * t6
      t156 = t155 * t46
      t157 = t48 * t51
      t161 = 0.96D2 * t15 * t39 * t40 - 0.148D3 * t50 * t52 * t40 + 0.43
     #0D3 * t57 * t60 * t62 * t64 + 0.94D2 * t45 * t14 * t39 * t58 * x3 
     #* x2 - 0.112D3 * t77 * t79 * t84 + 0.400D3 * t89 * t62 * t84 * x3 
     #- 0.128D3 * t15 * t39 * t40 * z + 0.56D2 * t7 * t9 * t35 * t62 * t
     #101 - 0.144D3 * t75 * t14 * t29 * t58 * t107 * t83 - 0.112D3 * t11
     #3 * t20 * t13 * x3 * t83 - 0.144D3 * t77 * t119 * t84 - 0.144D3 * 
     #t124 * t9 * t126 * t83 - 0.144D3 * t131 * t133 * t29 * t135 - 0.12
     #D2 * t139 * t141 * t38 + 0.748D3 * t3 * t5 * t8 * t147 * t38 - 0.1
     #44D3 * t131 * t133 * t140 * t83 - 0.288D3 * t156 * t157 * t83
      t162 = t3 * t123
      t163 = t162 * t9
      t164 = t13 * t51
      t172 = t48 * t29
      t176 = t7 * x1
      t177 = t78 * t19
      t178 = t59 * t177
      t182 = t9 * t58
      t184 = t19 * x3
      t189 = t74 * t6 * t123
      t190 = t58 ** 2
      t197 = t112 * t8 * t190
      t198 = x2 * t31
      t205 = t75 * t49
      t206 = t29 * t35
      t212 = x3 * t83
      t221 = t13 * t83
      t226 = t75 * x1
      t227 = t190 * t19
      t228 = t107 * x3
      t237 = t59 * t228
      t243 = t45 * t76
      t250 = t19 * t48
      t255 = -0.16D2 * t163 * t164 * t38 + 0.216D3 * t131 * t133 * t51 *
     # t100 + 0.216D3 * t156 * t172 * t100 + 0.10D2 * t176 * t178 * t38 
     #+ 0.168D3 * t75 * t182 * t184 * t101 + 0.28D2 * t189 * t9 * t190 *
     # t184 * t64 + 0.56D2 * t197 * t79 * t198 + 0.36D2 * t197 * t119 * 
     #t198 + 0.168D3 * t205 * t206 * x3 * t100 - 0.112D3 * t155 * t14 * 
     #t206 * t212 - 0.112D3 * t205 * t51 * t35 * t212 - 0.584D3 * t45 * 
     #t182 * t62 * t221 * x2 + 0.28D2 * t226 * t227 * t228 + 0.47D2 * t5
     #7 * t49 * t39 * t63 * t58 + 0.16D2 * t112 * t237 * t14 * t83 * t29
     # - 0.530D3 * t243 * t62 * t82 * t31 + 0.216D3 * t112 * t46 * t58 *
     # t250 * t100 * x2
      t264 = t130 * t133
      t275 = t8 * t31
      t279 = t162 * t275
      t287 = t190 * t78
      t291 = t6 ** 2
      t300 = t190 * t177
      t304 = t38 * t83
      t308 = t78 * t38
      t331 = t58 * t107
      t335 = -0.144D3 * t189 * t46 * t59 * t250 * t83 * t63 + 0.16D2 * t
     #112 * t36 * t264 * t135 * t29 - 0.128D3 * t50 * t39 * t83 * x2 * t
     #35 + 0.36D2 * t74 * t5 * t275 * t29 + 0.96D2 * t279 * t39 * t36 - 
     #0.148D3 * t15 * t52 * t36 + 0.108D3 * t189 * t9 * t287 * t64 + 0.3
     #6D2 * t74 * t291 * t46 * t227 * t63 * x2 * t48 + 0.108D3 * t112 * 
     #t8 * t300 * t198 - 0.240D3 * t139 * t157 * t304 + 0.328D3 * t89 * 
     #t308 * t84 + 0.28D2 * t226 * t300 * x3 + 0.36D2 * t226 * t287 * t1
     #07 - 0.288D3 * t113 * t78 * x2 * t221 - 0.328D3 * t243 * t308 * t1
     #98 - 0.128D3 * t279 * t39 * z * t35 * x3 + 0.47D2 * t7 * t275 * t3
     #9 * t331
      t337 = t78 ** 2
      t347 = t51 ** 2
      t368 = t75 * t46
      t381 = t75 * t9
      t389 = t75 * t8
      t409 = 0.36D2 * t75 * x1 * t190 * t337 + 0.108D3 * t155 * t49 * t1
     #40 + 0.108D3 * t124 * t14 * t51 + 0.36D2 * t75 * t264 * t347 - 0.1
     #28D3 * t15 * t39 * t83 * t35 * x3 + 0.312D3 * t139 * t172 * t38 * 
     #t100 - 0.880D3 * t163 * t126 * t304 + 0.182D3 * t176 * t59 * t78 *
     # t38 * x3 - 0.144D3 * t368 * t35 * t19 * t48 * t135 + 0.28D2 * t36
     #8 * t141 * t36 + 0.36D2 * t155 * t8 * t147 * t331 + 0.216D3 * t381
     # * t58 * t78 * t101 + 0.36D2 * t381 * t164 * t331 + 0.28D2 * t389 
     #* t147 * t237 - 0.144D3 * t389 * t178 * t84 + 0.56D2 * t155 * t9 *
     # t164 * t36 + 0.192D3 * t176 * t59 * t19 * t38 * t107 + 0.28D2 * t
     #124 * t8 * t147 * t36
      rrqg2qgh63J1 = -wd * (t161 + t255 + t335 + t409) / t1 / t38 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = x1 ** 2
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t11 * t10
      t13 = t9 * t12
      t14 = t8 * t13
      t15 = 0.1D1 - x3
      t16 = t15 ** 2
      t17 = s * t4
      t19 = z + x1 * t4
      t20 = 0.1D1 / t19
      t22 = 0.1D1 - x2
      t23 = x3 * t22
      t25 = x2 * t15
      t27 = cos(x4 * 0.3141592653589793D1)
      t31 = Sqrt(t23 * t19 * x2 * t15)
      t33 = 0.2D1 * t27 * t31
      t34 = t23 * t19 + t25 - t33
      t37 = t10 * x3
      t39 = s - t17 * x1 * t20 * t34 - t17 * t37
      t40 = t16 * t39
      t41 = x2 * t20
      t43 = t14 * t40 * t41
      t45 = t2 * t1
      t46 = t6 ** 2
      t48 = t9 ** 2
      t50 = t11 ** 2
      t51 = t50 * t15
      t52 = x2 ** 2
      t54 = t19 ** 2
      t56 = 0.1D1 / t54 / t19
      t61 = t5 * t4
      t63 = t45 * t6 * t61
      t64 = t9 * x1
      t66 = t50 * t16
      t67 = 0.1D1 / t54
      t68 = t52 * t67
      t72 = t6 * t5
      t73 = t45 * t72
      t75 = t16 * t15
      t76 = t50 * t75
      t80 = t3 * t6
      t81 = t80 * t48
      t82 = t34 ** 2
      t83 = t56 * t82
      t86 = x2 * x3
      t87 = t15 * t22 * t19 + t86 + t33
      t88 = t39 * t87
      t90 = t81 * t83 * t88
      t92 = t45 * t61
      t94 = t20 * t34
      t98 = t80 * x1
      t100 = x3 ** 2
      t103 = t98 * t12 * t15 * t39 * t100
      t105 = t45 * t6
      t107 = t67 * t82
      t111 = t45 * t7
      t112 = t111 * t9
      t113 = t12 * t75
      t114 = t20 * t87
      t118 = t100 * x3
      t119 = t12 * t118
      t121 = t112 * t94 * t119
      t123 = t111 * t64
      t124 = t11 * t100
      t126 = t123 * t107 * t124
      t128 = t11 * t16
      t129 = t87 ** 2
      t130 = t67 * t129
      t132 = t123 * t128 * t130
      t136 = t105 * t9 * t94 * t124
      t138 = t111 * t48
      t139 = t82 * t34
      t140 = t56 * t139
      t144 = t10 * t15
      t145 = t129 * t87
      t149 = 0.144D3 * t138 * t144 * t56 * t145
      t151 = t39 * x3
      t153 = t98 * t12 * t16 * t151
      t155 = t3 * t61
      t156 = t155 * t64
      t157 = t67 * t34
      t159 = t156 * t157 * t88
      t161 = -0.328D3 * t43 + 0.36D2 * t45 * t46 * t48 * t51 * t52 * x2 
     #* t56 + 0.108D3 * t63 * t64 * t66 * t68 + 0.108D3 * t73 * t9 * t76
     # * t41 - 0.240D3 * t90 + 0.28D2 * t92 * t9 * t94 * t37 + 0.192D3 *
     # t103 + 0.56D2 * t105 * t64 * t107 * t37 - 0.144D3 * t112 * t113 *
     # t114 + 0.28D2 * t121 + 0.36D2 * t126 + 0.216D3 * t132 + 0.36D2 * 
     #t136 + 0.28D2 * t138 * t140 * t37 - t149 + 0.182D3 * t153 - 0.880D
     #3 * t159
      t162 = t56 * t34
      t165 = t81 * t162 * t39 * t129
      t168 = t9 * t20
      t172 = t3 * t72
      t173 = t64 * t12
      t175 = t15 * t39
      t177 = t172 * t173 * t175 * t68
      t179 = t64 * t67
      t181 = t34 * t39
      t185 = t8 * t179 * t181 * t11 * x3 * x2
      t187 = t48 * t56
      t191 = t172 * t187 * t181 * t52 * t11
      t193 = t155 * t168
      t195 = t193 * t181 * t37
      t197 = t111 * t13
      t198 = t15 * t100
      t200 = t197 * t198 * t114
      t202 = t80 * t179
      t203 = x2 * t10
      t206 = t202 * t181 * t203 * z
      t209 = t34 * t10
      t210 = x3 * t87
      t212 = t105 * t179 * t209 * t210
      t214 = t111 * t187
      t217 = t214 * t82 * t10 * t210
      t220 = t202 * t181 * t203
      t222 = t8 * t187
      t223 = t82 * t39
      t225 = t222 * t223 * t203
      t229 = t14 * t175 * t86 * t20
      t231 = t64 * t11
      t233 = t15 * x3
      t235 = t111 * t231 * t233 * t130
      t239 = t15 * t56
      t242 = t73 * t48 * t11 * t239 * t129 * x2
      t253 = t80 * t64 * t10 * t175 * t130
      t255 = 0.312D3 * t165 + 0.36D2 * t45 * t5 * t168 * t34 + 0.430D3 *
     # t177 + 0.94D2 * t185 + 0.47D2 * t191 + 0.96D2 * t195 - 0.144D3 * 
     #t200 - 0.128D3 * t206 - 0.112D3 * t212 - 0.112D3 * t217 + 0.96D2 *
     # t220 - 0.148D3 * t225 - 0.530D3 * t229 + 0.168D3 * t235 + 0.216D3
     # * t242 - 0.144D3 * t63 * t48 * t12 * t239 * t87 * t52 + 0.56D2 * 
     #t253
      t257 = t73 * t173
      t259 = t67 * t87
      t266 = t8 * t231 * t175 * t259 * x2
      t268 = x3 * t16
      t270 = t197 * t268 * t114
      t273 = t80 * t9 * t11
      t276 = t273 * t175 * t114 * x3
      t279 = t16 ** 2
      t289 = t48 * x1
      t290 = t54 ** 2
      t291 = 0.1D1 / t290
      t292 = t289 * t291
      t293 = t82 ** 2
      t300 = t193 * t181 * z * t10 * x3
      t305 = t222 * t181 * t87 * x2 * t10
      t310 = t202 * t181 * t87 * t10 * x3
      t313 = t273 * t40 * t114
      t315 = t111 * x1
      t317 = t315 * t51 * t118
      t323 = t111 * t289
      t327 = 0.144D3 * t323 * t291 * t34 * t145
      t329 = t81 * t140 * t39
      t331 = t3 * t5
      t334 = t331 * t9 * t94 * t39
      t336 = -0.288D3 * t257 * t16 * x2 * t259 - 0.584D3 * t266 - 0.112D
     #3 * t270 + 0.400D3 * t276 + 0.36D2 * t111 * x1 * t50 * t279 + 0.10
     #8D3 * t105 * t187 * t139 + 0.108D3 * t92 * t179 * t82 + 0.36D2 * t
     #111 * t292 * t293 - 0.128D3 * t300 - 0.128D3 * t305 - 0.128D3 * t3
     #10 + 0.328D3 * t313 + 0.28D2 * t317 - 0.144D3 * t92 * t64 * t157 *
     # t87 - t327 - 0.12D2 * t329 + 0.748D3 * t334
      t341 = t105 * t48
      t346 = t156 * t107 * t39
      t350 = t323 * t291 * t82 * t129
      t353 = t341 * t162 * t129
      t356 = t98 * t113 * t39
      t362 = t315 * t66 * t100
      t368 = t111 * t179 * t34 * t11 * t100 * t87
      t372 = t214 * t209 * x3 * t129
      t375 = t202 * t223 * t37
      t383 = t73 * t9 * t50
      t388 = t383 * t198 * t41
      t394 = 0.16D2 * t73 * t119 * t179 * t87 * t34
      t399 = 0.16D2 * t73 * t37 * t292 * t145 * t34
      t402 = t80 * t168 * t181 * t124
      t407 = t257 * t25 * t67 * x3 * t87
      t409 = -0.144D3 * t323 * t291 * t139 * t87 - 0.288D3 * t341 * t83 
     #* t87 - 0.16D2 * t346 + 0.216D3 * t350 + 0.216D3 * t353 + 0.10D2 *
     # t356 + 0.28D2 * t315 * t76 * x3 + 0.36D2 * t362 - 0.144D3 * t368 
     #+ 0.168D3 * t372 - 0.148D3 * t375 + 0.28D2 * t63 * t64 * t50 * t23
     #3 * t68 + 0.56D2 * t383 * t268 * t41 + 0.36D2 * t388 + t394 + t399
     # + 0.47D2 * t402 - 0.112D3 * t407
      t423 = t155 * x1
      t436 = 0.604D3 * t43 - 0.64D2 * t90 - 0.30D2 * t103 - 0.56D2 * t12
     #1 - 0.72D2 * t126 - 0.72D2 * t132 - 0.72D2 * t136 + t149 - 0.108D3
     # * t153 + 0.640D3 * t159 - 0.264D3 * t165 - 0.256D3 * t423 * t11 *
     # t15 * t151 + 0.64D2 * t155 * t9 * t39 * x2 * t10 * t20 - 0.542D3 
     #* t177 - 0.108D3 * t185
      t455 = t80 * t64 * t39
      t467 = -0.128D3 * t155 * t9 * t39 * t114 * t37 - 0.54D2 * t191 - 0
     #.640D3 * t195 + 0.256D3 * t200 + 0.256D3 * t206 + 0.144D3 * t212 +
     # 0.144D3 * t217 + 0.64D2 * t8 * t48 * t39 * t56 * t129 * t203 - 0.
     #128D3 * t455 * t203 * t259 + 0.64D2 * t455 * t37 * t130 - 0.640D3 
     #* t220 + 0.216D3 * t225 + 0.444D3 * t229 - 0.344D3 * t235 - 0.72D2
     # * t242
      t478 = t331 * x1
      t495 = 0.24D2 * t253 + 0.32D2 * t73 * t124 * t187 * t129 * t34 + 0
     #.544D3 * t266 + 0.144D3 * t270 - 0.288D3 * t276 + 0.64D2 * t478 * 
     #t39 * t10 * x3 - 0.256D3 * t423 * t128 * t39 + 0.136D3 * t478 * t1
     #44 * t39 + 0.256D3 * t300 + 0.496D3 * t305 + 0.496D3 * t310 - 0.25
     #6D3 * t313 - 0.56D2 * t317 + t327 - 0.16D2 * t329
      t516 = -0.592D3 * t334 + 0.608D3 * t346 - 0.72D2 * t350 - 0.72D2 *
     # t353 - 0.62D2 * t356 - 0.72D2 * t362 + 0.256D3 * t273 * t175 * t4
     #1 - 0.216D3 * t155 * t9 * t10 * t175 * t114 + 0.256D3 * t368 - 0.3
     #44D3 * t372 + 0.216D3 * t375 - 0.72D2 * t388 - t394 - t399 - 0.54D
     #2 * t402 + 0.144D3 * t407
      rrqg2qgh63J2 = -(wd * (t161 + t255 + t336 + t409) + wd * (t436 + t
     #467 + t495 + t516)) / t1 / t39 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * t1
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t5 ** 2
      t8 = t7 * t6
      t9 = t3 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t12 = 0.1D1 - x1
      t13 = t12 ** 2
      t14 = t13 ** 2
      t17 = 0.1D1 - x3
      t18 = t17 * x3
      t19 = x2 ** 2
      t21 = z + x1 * t4
      t22 = t21 ** 2
      t23 = 0.1D1 / t22
      t24 = t23 * t19
      t28 = t7 * t5
      t29 = t3 * t28
      t31 = t29 * t10 * t14
      t32 = t17 ** 2
      t33 = t32 * x3
      t34 = 0.1D1 / t21
      t35 = x2 * t34
      t39 = x3 ** 2
      t40 = t17 * t39
      t42 = t31 * t40 * t35
      t44 = t2 * s
      t45 = t7 * t4
      t46 = t44 * t45
      t47 = t13 * t12
      t48 = t10 * t47
      t49 = t46 * t48
      t50 = s * t4
      t52 = 0.1D1 - x2
      t53 = x3 * t52
      t55 = x2 * t17
      t57 = cos(x4 * 0.3141592653589793D1)
      t61 = Sqrt(t53 * t21 * x2 * t17)
      t63 = 0.2D1 * t57 * t61
      t64 = t53 * t21 + t55 - t63
      t67 = x3 * t12
      t69 = s - t50 * x1 * t34 * t64 - t50 * t67
      t70 = t17 * t69
      t71 = x2 * x3
      t74 = t49 * t70 * t71 * t34
      t76 = t3 * t45
      t77 = t11 * t13
      t81 = t17 * t52 * t21 + t71 + t63
      t82 = t81 ** 2
      t83 = t23 * t82
      t85 = t76 * t77 * t18 * t83
      t87 = t10 ** 2
      t89 = 0.1D1 / t22 / t21
      t90 = t87 * t89
      t91 = t76 * t90
      t92 = t64 * t12
      t95 = t91 * t92 * x3 * t82
      t97 = t3 * t7
      t98 = t11 * t23
      t100 = x3 * t81
      t102 = t97 * t98 * t92 * t100
      t104 = t64 ** 2
      t107 = t91 * t104 * t12 * t100
      t110 = t23 * t81
      t113 = t46 * t77 * t70 * t110 * x2
      t116 = t32 ** 2
      t120 = t104 * t64
      t124 = t3 * t6
      t128 = t87 * x1
      t129 = t22 ** 2
      t130 = 0.1D1 / t129
      t131 = t128 * t130
      t132 = t104 ** 2
      t136 = t3 * t5
      t137 = t34 * t10
      t141 = t76 * t48
      t142 = t34 * t81
      t144 = t141 * t33 * t142
      t146 = t44 * t7
      t147 = t146 * t98
      t148 = t104 * t69
      t150 = t147 * t148 * t67
      t153 = t64 * t69
      t154 = t13 * t39
      t156 = t146 * t137 * t153 * t154
      t158 = 0.28D2 * t9 * t11 * t14 * t18 * t24 + 0.56D2 * t31 * t33 * 
     #t35 + 0.36D2 * t42 - 0.530D3 * t74 + 0.168D3 * t85 + 0.168D3 * t95
     # - 0.112D3 * t102 - 0.112D3 * t107 - 0.584D3 * t113 + 0.36D2 * t76
     # * x1 * t14 * t116 + 0.108D3 * t97 * t90 * t120 + 0.108D3 * t124 *
     # t98 * t104 + 0.36D2 * t76 * t131 * t132 + 0.36D2 * t136 * t137 * 
     #t64 - 0.112D3 * t144 - 0.148D3 * t150 + 0.47D2 * t156
      t159 = x2 * t12
      t161 = t147 * t153 * t159
      t166 = t147 * t153 * t81 * t12 * x3
      t169 = t146 * t10 * t13
      t170 = t32 * t69
      t172 = t169 * t170 * t142
      t174 = t39 * x3
      t175 = t47 * t174
      t180 = 0.16D2 * t29 * t175 * t98 * t81 * t64
      t182 = t82 * t81
      t186 = 0.16D2 * t29 * t67 * t131 * t182 * t64
      t187 = t44 * t6
      t188 = t187 * t137
      t192 = t188 * t153 * z * t12 * x3
      t194 = t46 * t90
      t196 = t194 * t148 * t159
      t198 = t44 * t28
      t200 = t19 * t13
      t202 = t198 * t90 * t153 * t200
      t205 = t188 * t153 * t67
      t209 = t147 * t153 * t159 * z
      t210 = 0.128D3 * t209
      t211 = t76 * x1
      t212 = t14 * t17
      t214 = t211 * t212 * t174
      t217 = t23 * t64
      t221 = t76 * t128
      t225 = 0.144D3 * t221 * t130 * t64 * t182
      t226 = t146 * t87
      t227 = t89 * t120
      t229 = t226 * t227 * t69
      t231 = t44 * t5
      t233 = t34 * t64
      t235 = t231 * t10 * t233 * t69
      t241 = t97 * t87
      t242 = t89 * t104
      t246 = 0.96D2 * t161 - 0.128D3 * t166 + 0.328D3 * t172 + t180 + t1
     #86 - 0.128D3 * t192 - 0.148D3 * t196 + 0.47D2 * t202 + 0.96D2 * t2
     #05 - t210 + 0.28D2 * t214 - 0.144D3 * t124 * t11 * t217 * t81 - t2
     #25 - 0.12D2 * t229 + 0.748D3 * t235 - 0.144D3 * t221 * t130 * t120
     # * t81 - 0.288D3 * t241 * t242 * t81
      t248 = t187 * t11
      t249 = t23 * t104
      t251 = t248 * t249 * t69
      t255 = t221 * t130 * t104 * t82
      t257 = t89 * t64
      t259 = t241 * t257 * t82
      t261 = t146 * x1
      t262 = t32 * t17
      t263 = t47 * t262
      t265 = t261 * t263 * t69
      t267 = t14 * t262
      t271 = t14 * t32
      t273 = t211 * t271 * t39
      t276 = t49 * t170 * t35
      t278 = t11 * t47
      t281 = t198 * t278 * t70 * t24
      t283 = t7 ** 2
      t286 = x2 * t19
      t299 = t69 * t81
      t301 = t226 * t242 * t299
      t310 = t261 * t47 * t17 * t69 * t39
      t316 = t76 * t10
      t321 = t316 * t233 * t175
      t323 = -0.16D2 * t251 + 0.216D3 * t255 + 0.216D3 * t259 + 0.10D2 *
     # t265 + 0.28D2 * t211 * t267 * x3 + 0.36D2 * t273 - 0.328D3 * t276
     # + 0.430D3 * t281 + 0.36D2 * t3 * t283 * t87 * t212 * t286 * t89 +
     # 0.108D3 * t9 * t11 * t271 * t24 + 0.108D3 * t29 * t10 * t267 * t3
     #5 - 0.240D3 * t301 + 0.28D2 * t124 * t10 * t233 * t67 + 0.192D3 * 
     #t310 + 0.56D2 * t97 * t11 * t249 * t67 - 0.144D3 * t316 * t263 * t
     #142 + 0.28D2 * t321
      t324 = t76 * t11
      t326 = t324 * t249 * t154
      t328 = t13 * t32
      t330 = t324 * t328 * t83
      t334 = t97 * t10 * t233 * t154
      t336 = t76 * t87
      t340 = t12 * t17
      t344 = 0.144D3 * t336 * t340 * t89 * t182
      t346 = t69 * x3
      t348 = t261 * t47 * t32 * t346
      t351 = t248 * t217 * t299
      t355 = t226 * t257 * t69 * t82
      t360 = t194 * t153 * t81 * x2 * t12
      t363 = t13 * x3
      t366 = t46 * t98 * t153 * t363 * x2
      t371 = t146 * t11 * t12 * t70 * t83
      t374 = t64 * t13
      t377 = t76 * t98 * t374 * t39 * t81
      t381 = t17 * t89
      t382 = t82 * x2
      t384 = t29 * t87 * t13 * t381 * t382
      t388 = t81 * t19
      t392 = t29 * t278
      t399 = t169 * t70 * t142 * x3
      t402 = t23 * x3 * t81
      t404 = t392 * t55 * t402
      t407 = t141 * t40 * t142
      t409 = 0.36D2 * t326 + 0.216D3 * t330 + 0.36D2 * t334 + 0.28D2 * t
     #336 * t227 * t67 - t344 + 0.182D3 * t348 - 0.880D3 * t351 + 0.312D
     #3 * t355 - 0.128D3 * t360 + 0.94D2 * t366 + 0.56D2 * t371 - 0.144D
     #3 * t377 + 0.216D3 * t384 - 0.144D3 * t9 * t87 * t47 * t381 * t388
     # - 0.288D3 * t392 * t32 * x2 * t110 + 0.400D3 * t399 - 0.112D3 * t
     #404 - 0.144D3 * t407
      t424 = t11 * t69
      t425 = t146 * t424
      t427 = t425 * t67 * t83
      t429 = t10 * t69
      t432 = t187 * t429 * t142 * t67
      t436 = -0.72D2 * t42 + 0.444D3 * t74 - 0.344D3 * t85 - 0.344D3 * t
     #95 + 0.144D3 * t102 + 0.144D3 * t107 + 0.544D3 * t113 + 0.144D3 * 
     #t144 + 0.216D3 * t150 - 0.54D2 * t156 - 0.640D3 * t161 + 0.64D2 * 
     #t427 - 0.128D3 * t432 + 0.496D3 * t166 - 0.256D3 * t172
      t437 = t87 * t69
      t441 = t46 * t437 * t89 * t82 * t159
      t444 = t425 * t159 * t110
      t456 = 0.64D2 * t441 - 0.128D3 * t444 - t180 - t186 + 0.256D3 * t1
     #92 + 0.216D3 * t196 - 0.54D2 * t202 - 0.640D3 * t205 + 0.256D3 * t
     #209 - 0.56D2 * t214 + t225 - 0.16D2 * t229 - 0.592D3 * t235 + 0.60
     #8D3 * t251 - 0.72D2 * t255
      t461 = t231 * x1
      t462 = t69 * t12
      t464 = t461 * t462 * x3
      t466 = t187 * x1
      t468 = t466 * t328 * t69
      t481 = -0.72D2 * t259 - 0.62D2 * t265 - 0.72D2 * t273 + 0.64D2 * t
     #464 - 0.256D3 * t468 + 0.136D3 * t461 * t340 * t69 + 0.604D3 * t27
     #6 - 0.542D3 * t281 - 0.64D2 * t301 - 0.30D2 * t310 - 0.56D2 * t321
     # - 0.72D2 * t326 - 0.72D2 * t330 - 0.72D2 * t334 + t344
      t487 = t466 * t13 * t17 * t346
      t504 = t169 * t70 * t35
      t509 = t187 * t10 * t12 * t70 * t142
      t515 = 0.32D2 * t29 * t154 * t90 * t82 * t64
      t516 = -0.108D3 * t348 + 0.640D3 * t351 - 0.264D3 * t355 - 0.256D3
     # * t487 + 0.64D2 * t187 * t10 * t69 * x2 * t12 * t34 + 0.496D3 * t
     #360 - 0.108D3 * t366 + 0.24D2 * t371 + 0.256D3 * t377 - 0.72D2 * t
     #384 - 0.288D3 * t399 + 0.144D3 * t404 + 0.256D3 * t407 + 0.256D3 *
     # t504 - 0.216D3 * t509 + t515
      t520 = t146 * t429
      t541 = -0.96D2 * t520 * t154 * t142 - 0.192D3 * t46 * t424 * x2 * 
     #t13 * t402 + 0.536D3 * t74 + 0.144D3 * t85 + 0.144D3 * t95 - 0.742
     #08D5 * t113 - 0.128D3 * t150 - 0.252D3 * t156 + 0.128D3 * t427 - 0
     #.64D2 * t432 - 0.8D1 * t166 + 0.74320D5 * t172 + 0.128D3 * t441 - 
     #0.64D2 * t444
      t551 = z ** 2
      t557 = t69 * t13
      t570 = t44 * t8
      t583 = -t180 - t186 - 0.512D3 * t192 + 0.4D1 * t202 + 0.48D2 * t19
     #8 * t424 * t47 * x3 * t24 + 0.256D3 * t205 - t210 + 0.128D3 * t136
     # * t12 * x3 * t551 * z * x1 + 0.288D3 * t466 * t557 * t39 - 0.112D
     #3 * t261 * t69 * t47 * t174 + 0.16D2 * t198 * t131 * t382 * t92 * 
     #t69 - 0.16D2 * t570 * t131 * t388 * t374 * t69 - 0.96D2 * t198 * t
     #437 * t89 * t81 * t200 + 0.144D3 * t214 + 0.64D2 * t229
      t602 = 0.64D2 * t235 - 0.128D3 * t251 - 0.304D3 * t265 - 0.128D3 *
     # t464 - 0.25296D5 * t468 - 0.27920D5 * t276 + 0.28208D5 * t281 + 0
     #.64D2 * t520 * t363 * t35 + 0.128D3 * t301 - 0.28792D5 * t310 + 0.
     #144D3 * t321 - 0.29128D5 * t348 - 0.128D3 * t351 + 0.64D2 * t355 -
     # 0.25248D5 * t487
      t640 = 0.32D2 * t46 * t11 * t69 * t19 * t13 * t23 + 0.16D2 * t570 
     #* t87 * t69 * t286 * t47 * t89 - 0.384D3 * t466 * t557 * t39 * z -
     # 0.384D3 * t461 * t462 * x3 * t551 + 0.384D3 * t461 * t462 * x3 * 
     #z - 0.8D1 * t360 + 0.8D1 * t366 + 0.32D2 * t371 - 0.288D3 * t377 +
     # 0.74288D5 * t399 - 0.288D3 * t407 + 0.25264D5 * t504 - 0.80D2 * t
     #509 + t515 + 0.48D2 * t46 * t429 * t47 * t39 * t35
      rrqg2qgh63J3 = -(wd * (t158 + t246 + t323 + t409) + wd * (t436 + t
     #456 + t481 + t516) + wd * (t541 + t583 + t602 + t640)) / t1 / t69 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = x1 ** 2
      t10 = t9 ** 2
      t12 = z + x1 * t4
      t13 = t12 ** 2
      t15 = 0.1D1 / t13 / t12
      t16 = t10 * t15
      t17 = t8 * t16
      t18 = 0.1D1 - x2
      t19 = x3 * t18
      t21 = 0.1D1 - x3
      t22 = x2 * t21
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t19 * t12 * x2 * t21)
      t30 = 0.2D1 * t24 * t28
      t31 = t19 * t12 + t22 - t30
      t32 = t31 ** 2
      t33 = s * t4
      t34 = 0.1D1 / t12
      t38 = 0.1D1 - x1
      t39 = t38 * x3
      t41 = s - t33 * x1 * t34 * t31 - t33 * t39
      t42 = t32 * t41
      t43 = x2 * t38
      t45 = t17 * t42 * t43
      t47 = t6 * t5
      t48 = t3 * t47
      t50 = t31 * t41
      t51 = x2 ** 2
      t52 = t38 ** 2
      t53 = t51 * t52
      t55 = t48 * t16 * t50 * t53
      t57 = t2 * t1
      t58 = t57 * t7
      t59 = t9 * x1
      t60 = 0.1D1 / t13
      t61 = t59 * t60
      t63 = t31 * t52
      t64 = x3 ** 2
      t67 = x2 * x3
      t68 = t21 * t18 * t12 + t67 + t30
      t71 = t58 * t61 * t63 * t64 * t68
      t73 = t58 * t16
      t74 = t31 * t38
      t75 = t68 ** 2
      t78 = t73 * t74 * x3 * t75
      t83 = t17 * t50 * t68 * x2 * t38
      t85 = t3 * t5
      t87 = t34 * t31
      t89 = t85 * t9 * t87 * t41
      t91 = t10 * x1
      t92 = t58 * t91
      t93 = t13 ** 2
      t94 = 0.1D1 / t93
      t95 = t32 * t31
      t100 = t57 * t6
      t101 = t100 * t10
      t102 = t15 * t32
      t106 = t5 * t4
      t107 = t3 * t106
      t108 = t107 * t59
      t109 = t60 * t32
      t111 = t108 * t109 * t41
      t115 = t92 * t94 * t32 * t75
      t117 = t15 * t31
      t119 = t101 * t117 * t75
      t121 = t3 * t6
      t122 = t121 * x1
      t123 = t52 * t38
      t124 = t21 ** 2
      t125 = t124 * t21
      t126 = t123 * t125
      t128 = t122 * t126 * t41
      t130 = t58 * x1
      t131 = t52 ** 2
      t132 = t131 * t125
      t136 = t131 * t124
      t138 = t130 * t136 * t64
      t141 = t121 * t9 * t52
      t142 = t124 * t41
      t143 = t34 * t68
      t145 = t141 * t142 * t143
      t147 = t131 * t21
      t148 = t64 * x3
      t150 = t130 * t147 * t148
      t152 = t57 * t106
      t154 = t60 * t31
      t158 = -0.148D3 * t45 + 0.47D2 * t55 - 0.144D3 * t71 + 0.168D3 * t
     #78 - 0.128D3 * t83 + 0.748D3 * t89 - 0.144D3 * t92 * t94 * t95 * t
     #68 - 0.288D3 * t101 * t102 * t68 - 0.16D2 * t111 + 0.216D3 * t115 
     #+ 0.216D3 * t119 + 0.10D2 * t128 + 0.28D2 * t130 * t132 * x3 + 0.3
     #6D2 * t138 + 0.328D3 * t145 + 0.28D2 * t150 - 0.144D3 * t152 * t59
     # * t154 * t68
      t160 = t75 * t68
      t163 = 0.144D3 * t92 * t94 * t31 * t160
      t164 = t121 * t10
      t165 = t15 * t95
      t167 = t164 * t165 * t41
      t169 = t9 * t123
      t170 = t8 * t169
      t171 = x2 * t34
      t173 = t170 * t142 * t171
      t175 = t59 * t123
      t177 = t21 * t41
      t178 = t51 * t60
      t180 = t48 * t175 * t177 * t178
      t183 = x3 * t52
      t186 = t8 * t61 * t50 * t183 * x2
      t188 = t121 * t61
      t191 = t188 * t50 * t43 * z
      t192 = 0.128D3 * t191
      t193 = t9 * t34
      t194 = t107 * t193
      t198 = t194 * t50 * z * t38 * x3
      t202 = t60 * t75
      t204 = t121 * t59 * t38 * t177 * t202
      t206 = t57 * t47
      t208 = t206 * t9 * t131
      t209 = t124 * x3
      t213 = t21 * t64
      t215 = t208 * t213 * t171
      t217 = t123 * t148
      t222 = 0.16D2 * t206 * t217 * t61 * t68 * t31
      t224 = t91 * t94
      t228 = 0.16D2 * t206 * t39 * t224 * t160 * t31
      t230 = t188 * t50 * t43
      t233 = t52 * t64
      t235 = t121 * t193 * t50 * t233
      t239 = t164 * t117 * t41 * t75
      t242 = t41 * x3
      t244 = t122 * t123 * t124 * t242
      t246 = t41 * t68
      t248 = t108 * t154 * t246
      t250 = -t163 - 0.12D2 * t167 - 0.328D3 * t173 + 0.430D3 * t180 + 0
     #.94D2 * t186 - t192 - 0.128D3 * t198 + 0.56D2 * t204 + 0.56D2 * t2
     #08 * t209 * t171 + 0.36D2 * t215 + t222 + t228 + 0.96D2 * t230 + 0
     #.47D2 * t235 + 0.312D3 * t239 + 0.182D3 * t244 - 0.880D3 * t248
      t257 = t164 * t102 * t246
      t266 = t122 * t123 * t21 * t41 * t64
      t272 = t58 * t9
      t277 = t272 * t87 * t217
      t279 = t58 * t59
      t281 = t279 * t109 * t233
      t283 = t52 * t124
      t285 = t279 * t283 * t202
      t289 = t100 * t9 * t87 * t233
      t291 = t58 * t10
      t295 = t38 * t21
      t299 = 0.144D3 * t291 * t295 * t15 * t160
      t300 = t206 * t175
      t302 = t60 * x3 * t68
      t304 = t300 * t22 * t302
      t306 = t58 * t169
      t308 = t306 * t213 * t143
      t310 = t6 ** 2
      t313 = t51 * x2
      t318 = t6 * t106
      t319 = t57 * t318
      t324 = t57 * t5
      t328 = 0.108D3 * t206 * t9 * t132 * t171 - 0.240D3 * t257 + 0.28D2
     # * t152 * t9 * t87 * t39 + 0.192D3 * t266 + 0.56D2 * t100 * t59 * 
     #t109 * t39 - 0.144D3 * t272 * t126 * t143 + 0.28D2 * t277 + 0.36D2
     # * t281 + 0.216D3 * t285 + 0.36D2 * t289 + 0.28D2 * t291 * t165 * 
     #t39 - t299 - 0.112D3 * t304 - 0.144D3 * t308 + 0.36D2 * t57 * t310
     # * t10 * t147 * t313 * t15 + 0.108D3 * t319 * t59 * t136 * t178 + 
     #0.36D2 * t324 * t193 * t31
      t331 = t141 * t177 * t143 * x3
      t335 = t21 * x3
      t340 = t194 * t50 * t39
      t344 = t170 * t177 * t67 * t34
      t346 = t59 * t52
      t349 = t58 * t346 * t335 * t202
      t353 = t21 * t15
      t354 = t75 * x2
      t356 = t206 * t10 * t52 * t353 * t354
      t360 = t68 * t51
      t365 = t60 * t68
      t372 = t8 * t346 * t177 * t365 * x2
      t375 = t306 * t209 * t143
      t378 = x3 * t68
      t380 = t100 * t61 * t74 * t378
      t384 = t73 * t32 * t38 * t378
      t387 = t124 ** 2
      t397 = t32 ** 2
      t404 = t188 * t50 * t68 * t38 * x3
      t407 = t188 * t42 * t39
      t409 = 0.400D3 * t331 + 0.28D2 * t319 * t59 * t131 * t335 * t178 +
     # 0.96D2 * t340 - 0.530D3 * t344 + 0.168D3 * t349 + 0.216D3 * t356 
     #- 0.144D3 * t319 * t10 * t123 * t353 * t360 - 0.288D3 * t300 * t12
     #4 * x2 * t365 - 0.584D3 * t372 - 0.112D3 * t375 - 0.112D3 * t380 -
     # 0.112D3 * t384 + 0.36D2 * t58 * x1 * t131 * t387 + 0.108D3 * t100
     # * t16 * t95 + 0.108D3 * t152 * t61 * t32 + 0.36D2 * t58 * t224 * 
     #t397 - 0.128D3 * t404 - 0.148D3 * t407
      t427 = 0.216D3 * t45 - 0.54D2 * t55 + 0.256D3 * t71 - 0.344D3 * t7
     #8 + 0.496D3 * t83 - 0.592D3 * t89 + 0.608D3 * t111 - 0.72D2 * t115
     # - 0.72D2 * t119 - 0.62D2 * t128 - 0.72D2 * t138 - 0.256D3 * t145 
     #- 0.56D2 * t150 + t163 - 0.16D2 * t167
      t437 = t141 * t177 * t171
      t442 = t107 * t9 * t38 * t177 * t143
      t446 = t107 * x1
      t449 = t446 * t52 * t21 * t242
      t451 = 0.604D3 * t173 - 0.542D3 * t180 - 0.108D3 * t186 + 0.256D3 
     #* t191 + 0.256D3 * t198 + 0.24D2 * t204 - 0.72D2 * t215 - t222 - t
     #228 - 0.640D3 * t230 + 0.256D3 * t437 - 0.216D3 * t442 - 0.54D2 * 
     #t235 - 0.264D3 * t239 - 0.256D3 * t449
      t469 = t10 * t41
      t473 = t8 * t469 * t15 * t75 * t43
      t477 = 0.64D2 * t107 * t9 * t41 * x2 * t38 * t34 - 0.108D3 * t244 
     #+ 0.640D3 * t248 - 0.64D2 * t257 - 0.30D2 * t266 - 0.56D2 * t277 -
     # 0.72D2 * t281 - 0.72D2 * t285 - 0.72D2 * t289 + t299 + 0.144D3 * 
     #t304 + 0.256D3 * t308 + 0.64D2 * t473 - 0.288D3 * t331 - 0.640D3 *
     # t340
      t481 = t59 * t41
      t482 = t121 * t481
      t484 = t482 * t43 * t365
      t487 = t482 * t39 * t202
      t489 = t9 * t41
      t492 = t107 * t489 * t143 * t39
      t496 = t85 * x1
      t497 = t41 * t38
      t499 = t496 * t497 * x3
      t502 = t446 * t283 * t41
      t513 = 0.32D2 * t206 * t233 * t16 * t75 * t31
      t516 = 0.444D3 * t344 - 0.344D3 * t349 - 0.72D2 * t356 - 0.128D3 *
     # t484 + 0.64D2 * t487 - 0.128D3 * t492 + 0.544D3 * t372 + 0.144D3 
     #* t375 + 0.64D2 * t499 - 0.256D3 * t502 + 0.136D3 * t496 * t295 * 
     #t41 + 0.144D3 * t380 + 0.144D3 * t384 + t513 + 0.496D3 * t404 + 0.
     #216D3 * t407
      t535 = t3 * t318
      t541 = t121 * t489
      t546 = 0.4D1 * t55 - 0.288D3 * t71 + 0.144D3 * t78 - 0.8D1 * t83 +
     # 0.64D2 * t89 - 0.128D3 * t111 - 0.304D3 * t128 - 0.96D2 * t48 * t
     #469 * t15 * t68 * t53 + 0.74320D5 * t145 + 0.144D3 * t150 + 0.64D2
     # * t167 - 0.16D2 * t535 * t224 * t360 * t63 * t41 + 0.64D2 * t541 
     #* t183 * t171 - 0.27920D5 * t173
      t567 = t41 * t52
      t572 = z ** 2
      t580 = 0.28208D5 * t180 + 0.8D1 * t186 - t192 - 0.512D3 * t198 + 0
     #.32D2 * t204 - t222 - t228 + 0.384D3 * t496 * t497 * x3 * z + 0.32
     #D2 * t8 * t59 * t41 * t51 * t52 * t60 + 0.16D2 * t535 * t10 * t41 
     #* t313 * t123 * t15 - 0.384D3 * t446 * t567 * t64 * z - 0.384D3 * 
     #t496 * t497 * x3 * t572 + 0.25264D5 * t437 - 0.80D2 * t442 - 0.252
     #D3 * t235
      t605 = 0.64D2 * t239 - 0.25248D5 * t449 - 0.29128D5 * t244 - 0.128
     #D3 * t248 + 0.128D3 * t257 - 0.28792D5 * t266 + 0.144D3 * t277 + 0
     #.48D2 * t8 * t489 * t123 * t64 * t171 + 0.48D2 * t48 * t481 * t123
     # * x3 * t178 - 0.288D3 * t308 + 0.128D3 * t473 + 0.74288D5 * t331 
     #+ 0.256D3 * t340 + 0.536D3 * t344 + 0.144D3 * t349
      t640 = 0.16D2 * t48 * t224 * t354 * t74 * t41 - 0.96D2 * t541 * t2
     #33 * t143 - 0.192D3 * t8 * t481 * x2 * t52 * t302 - 0.64D2 * t484 
     #+ 0.128D3 * t487 - 0.64D2 * t492 - 0.74208D5 * t372 + 0.128D3 * t3
     #24 * t38 * x3 * t572 * z * x1 + 0.288D3 * t446 * t567 * t64 - 0.11
     #2D3 * t122 * t41 * t123 * t148 - 0.128D3 * t499 - 0.25296D5 * t502
     # + t513 - 0.8D1 * t404 - 0.128D3 * t407
      rrqg2qgh63J4 = -(wd * (t158 + t250 + t328 + t409) + wd * (t427 + t
     #451 + t477 + t516) + wd * (t546 + t580 + t605 + t640)) / t1 / t41 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = x1 ** 2
      t10 = t9 ** 2
      t12 = z + x1 * t4
      t13 = t12 ** 2
      t15 = 0.1D1 / t13 / t12
      t16 = t10 * t15
      t17 = t8 * t16
      t18 = 0.1D1 - x2
      t19 = x3 * t18
      t21 = 0.1D1 - x3
      t22 = x2 * t21
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t19 * t12 * x2 * t21)
      t30 = 0.2D1 * t24 * t28
      t31 = t19 * t12 + t22 - t30
      t32 = t31 ** 2
      t33 = s * t4
      t34 = 0.1D1 / t12
      t38 = 0.1D1 - x1
      t39 = t38 * x3
      t41 = s - t33 * x1 * t34 * t31 - t33 * t39
      t42 = t32 * t41
      t43 = x2 * t38
      t45 = t17 * t42 * t43
      t47 = t3 * t6
      t48 = t38 ** 2
      t50 = t47 * t9 * t48
      t51 = t21 * t41
      t54 = x2 * x3
      t55 = t21 * t18 * t12 + t54 + t30
      t56 = t34 * t55
      t59 = t50 * t51 * t56 * x3
      t61 = t2 * t1
      t62 = t6 * t5
      t63 = t61 * t62
      t64 = t9 * x1
      t65 = t48 * t38
      t66 = t64 * t65
      t67 = t63 * t66
      t68 = 0.1D1 / t13
      t70 = t68 * x3 * t55
      t72 = t67 * t22 * t70
      t74 = t61 * t7
      t75 = t9 * t65
      t76 = t74 * t75
      t77 = x3 ** 2
      t78 = t21 * t77
      t80 = t76 * t78 * t56
      t82 = t48 ** 2
      t84 = t21 ** 2
      t85 = t84 ** 2
      t89 = t61 * t6
      t90 = t32 * t31
      t94 = t5 * t4
      t95 = t61 * t94
      t96 = t64 * t68
      t100 = t10 * x1
      t101 = t13 ** 2
      t102 = 0.1D1 / t101
      t103 = t100 * t102
      t104 = t32 ** 2
      t108 = t61 * t5
      t109 = t9 * t34
      t113 = t74 * t16
      t115 = x3 * t55
      t117 = t113 * t32 * t38 * t115
      t119 = t6 * t94
      t120 = t61 * t119
      t123 = t21 * x3
      t124 = x2 ** 2
      t125 = t124 * t68
      t130 = t63 * t9 * t82
      t131 = t84 * x3
      t132 = x2 * t34
      t137 = t130 * t78 * t132
      t139 = t3 * t62
      t141 = t31 * t41
      t142 = t124 * t48
      t144 = t139 * t16 * t141 * t142
      t146 = t3 * t94
      t147 = t146 * t109
      t149 = t147 * t141 * t39
      t151 = t6 ** 2
      t154 = t82 * t21
      t155 = t124 * x2
      t161 = t82 * t84
      t165 = -0.148D3 * t45 + 0.400D3 * t59 - 0.112D3 * t72 - 0.144D3 * 
     #t80 + 0.36D2 * t74 * x1 * t82 * t85 + 0.108D3 * t89 * t16 * t90 + 
     #0.108D3 * t95 * t96 * t32 + 0.36D2 * t74 * t103 * t104 + 0.36D2 * 
     #t108 * t109 * t31 - 0.112D3 * t117 + 0.28D2 * t120 * t64 * t82 * t
     #123 * t125 + 0.56D2 * t130 * t131 * t132 + 0.36D2 * t137 + 0.47D2 
     #* t144 + 0.96D2 * t149 + 0.36D2 * t61 * t151 * t10 * t154 * t155 *
     # t15 + 0.108D3 * t120 * t64 * t161 * t125
      t167 = t84 * t21
      t168 = t82 * t167
      t172 = t10 * t47
      t173 = t15 * t32
      t174 = t41 * t55
      t176 = t172 * t173 * t174
      t179 = t34 * t31
      t183 = t47 * x1
      t187 = t183 * t65 * t21 * t41 * t77
      t190 = t68 * t32
      t194 = t74 * t9
      t195 = t65 * t167
      t199 = t77 * x3
      t200 = t65 * t199
      t202 = t194 * t179 * t200
      t204 = t74 * t64
      t205 = t48 * t77
      t207 = t204 * t190 * t205
      t209 = t48 * t84
      t210 = t55 ** 2
      t211 = t68 * t210
      t213 = t204 * t209 * t211
      t217 = t89 * t9 * t179 * t205
      t219 = t74 * t10
      t220 = t15 * t90
      t224 = t38 * t21
      t225 = t210 * t55
      t229 = 0.144D3 * t219 * t224 * t15 * t225
      t231 = t41 * x3
      t233 = t183 * t65 * t84 * t231
      t235 = t146 * t64
      t236 = t68 * t31
      t238 = t235 * t236 * t174
      t240 = t15 * t31
      t243 = t172 * t240 * t41 * t210
      t245 = t47 * t96
      t248 = t245 * t141 * t43 * z
      t249 = 0.128D3 * t248
      t253 = t147 * t141 * z * t38 * x3
      t255 = 0.108D3 * t63 * t9 * t168 * t132 - 0.240D3 * t176 + 0.28D2 
     #* t95 * t9 * t179 * t39 + 0.192D3 * t187 + 0.56D2 * t89 * t64 * t1
     #90 * t39 - 0.144D3 * t194 * t195 * t56 + 0.28D2 * t202 + 0.36D2 * 
     #t207 + 0.216D3 * t213 + 0.36D2 * t217 + 0.28D2 * t219 * t220 * t39
     # - t229 + 0.182D3 * t233 - 0.880D3 * t238 + 0.312D3 * t243 - t249 
     #- 0.128D3 * t253
      t259 = t47 * t109 * t141 * t205
      t262 = t245 * t141 * t43
      t265 = t48 * x3
      t268 = t8 * t96 * t141 * t265 * x2
      t270 = t89 * t10
      t275 = t235 * t190 * t41
      t277 = t74 * t100
      t280 = t277 * t102 * t32 * t210
      t283 = t270 * t240 * t210
      t286 = t183 * t195 * t41
      t288 = t74 * x1
      t293 = t288 * t161 * t77
      t295 = t64 * t48
      t297 = t68 * t55
      t300 = t8 * t295 * t51 * t297 * x2
      t303 = t76 * t131 * t56
      t308 = t245 * t141 * t55 * t38 * x3
      t312 = t21 * t15
      t313 = t210 * x2
      t315 = t63 * t10 * t48 * t312 * t313
      t319 = t55 * t124
      t327 = t8 * t75
      t328 = t84 * t41
      t330 = t327 * t328 * t132
      t332 = 0.47D2 * t259 + 0.96D2 * t262 + 0.94D2 * t268 - 0.288D3 * t
     #270 * t173 * t55 - 0.16D2 * t275 + 0.216D3 * t280 + 0.216D3 * t283
     # + 0.10D2 * t286 + 0.28D2 * t288 * t168 * x3 + 0.36D2 * t293 - 0.5
     #84D3 * t300 - 0.112D3 * t303 - 0.128D3 * t308 + 0.216D3 * t315 - 0
     #.144D3 * t120 * t10 * t65 * t312 * t319 - 0.288D3 * t67 * t84 * x2
     # * t297 - 0.328D3 * t330
      t335 = t139 * t66 * t51 * t125
      t338 = t50 * t328 * t56
      t344 = 0.16D2 * t63 * t200 * t96 * t55 * t31
      t345 = t31 * t38
      t348 = t113 * t345 * x3 * t210
      t352 = t89 * t96 * t345 * t115
      t358 = 0.16D2 * t63 * t39 * t103 * t225 * t31
      t362 = t17 * t141 * t55 * x2 * t38
      t366 = t327 * t51 * t54 * t34
      t370 = t74 * t295 * t123 * t211
      t373 = t245 * t42 * t39
      t378 = t47 * t64 * t38 * t51 * t211
      t381 = t31 * t48
      t384 = t74 * t96 * t381 * t77 * t55
      t387 = t288 * t154 * t199
      t396 = 0.144D3 * t277 * t102 * t31 * t225
      t398 = t172 * t220 * t41
      t400 = t3 * t5
      t403 = t400 * t9 * t179 * t41
      t409 = 0.430D3 * t335 + 0.328D3 * t338 + t344 + 0.168D3 * t348 - 0
     #.112D3 * t352 + t358 - 0.128D3 * t362 - 0.530D3 * t366 + 0.168D3 *
     # t370 - 0.148D3 * t373 + 0.56D2 * t378 - 0.144D3 * t384 + 0.28D2 *
     # t387 - 0.144D3 * t95 * t64 * t236 * t55 - t396 - 0.12D2 * t398 + 
     #0.748D3 * t403 - 0.144D3 * t277 * t102 * t90 * t55
      t427 = 0.216D3 * t45 - 0.288D3 * t59 + 0.144D3 * t72 + 0.256D3 * t
     #80 + 0.144D3 * t117 - 0.72D2 * t137 - 0.54D2 * t144 - 0.640D3 * t1
     #49 - 0.64D2 * t176 - 0.30D2 * t187 - 0.56D2 * t202 - 0.72D2 * t207
     # - 0.72D2 * t213 - 0.72D2 * t217 + t229
      t431 = t146 * x1
      t434 = t431 * t48 * t21 * t231
      t447 = t64 * t41
      t448 = t47 * t447
      t450 = t448 * t39 * t211
      t456 = -0.108D3 * t233 + 0.640D3 * t238 - 0.264D3 * t243 - 0.256D3
     # * t434 + 0.64D2 * t146 * t9 * t41 * x2 * t38 * t34 + 0.256D3 * t2
     #48 + 0.256D3 * t253 - 0.54D2 * t259 - 0.640D3 * t262 - 0.108D3 * t
     #268 + 0.64D2 * t450 + 0.608D3 * t275 - 0.72D2 * t280 - 0.72D2 * t2
     #83 - 0.62D2 * t286
      t459 = t400 * x1
      t460 = t41 * t38
      t462 = t459 * t460 * x3
      t465 = t431 * t209 * t41
      t479 = -0.72D2 * t293 + 0.64D2 * t462 - 0.256D3 * t465 + 0.136D3 *
     # t459 * t224 * t41 + 0.544D3 * t300 + 0.144D3 * t303 + 0.496D3 * t
     #308 - 0.72D2 * t315 + 0.604D3 * t330 - 0.542D3 * t335 - 0.256D3 * 
     #t338 - t344 - 0.344D3 * t348 + 0.144D3 * t352 - t358
      t481 = t9 * t41
      t484 = t146 * t481 * t56 * t39
      t490 = 0.32D2 * t63 * t205 * t16 * t210 * t31
      t494 = t146 * t9 * t38 * t51 * t56
      t496 = t10 * t41
      t500 = t8 * t496 * t15 * t210 * t43
      t503 = t448 * t43 * t297
      t514 = t50 * t51 * t132
      t516 = 0.496D3 * t362 - 0.128D3 * t484 + t490 - 0.216D3 * t494 + 0
     #.64D2 * t500 - 0.128D3 * t503 + 0.444D3 * t366 - 0.344D3 * t370 + 
     #0.216D3 * t373 + 0.24D2 * t378 + 0.256D3 * t384 - 0.56D2 * t387 + 
     #t396 - 0.16D2 * t398 - 0.592D3 * t403 + 0.256D3 * t514
      t533 = 0.74288D5 * t59 - 0.288D3 * t80 + 0.4D1 * t144 + 0.256D3 * 
     #t149 + 0.128D3 * t176 - 0.28792D5 * t187 + 0.144D3 * t202 - 0.2912
     #8D5 * t233 - 0.128D3 * t238 + 0.64D2 * t243 - 0.25248D5 * t434 - t
     #249 - 0.512D3 * t253 - 0.252D3 * t259
      t551 = z ** 2
      t557 = t41 * t48
      t565 = t47 * t481
      t580 = 0.8D1 * t268 + 0.128D3 * t450 - 0.192D3 * t8 * t447 * x2 * 
     #t48 * t70 + 0.16D2 * t139 * t103 * t313 * t345 * t41 - 0.128D3 * t
     #275 - 0.304D3 * t286 - 0.128D3 * t462 - 0.25296D5 * t465 + 0.128D3
     # * t108 * t38 * x3 * t551 * z * x1 + 0.288D3 * t431 * t557 * t77 -
     # 0.112D3 * t183 * t41 * t65 * t199 + 0.64D2 * t565 * t265 * t132 +
     # 0.48D2 * t8 * t481 * t65 * t77 * t132 + 0.48D2 * t139 * t447 * t6
     #5 * x3 * t125 - 0.74208D5 * t300
      t586 = t3 * t119
      t601 = -0.8D1 * t308 - 0.27920D5 * t330 + 0.28208D5 * t335 + 0.743
     #20D5 * t338 - t344 - 0.16D2 * t586 * t103 * t319 * t381 * t41 + 0.
     #144D3 * t348 - t358 - 0.8D1 * t362 - 0.64D2 * t484 + t490 - 0.96D2
     # * t565 * t205 * t56 - 0.80D2 * t494 + 0.128D3 * t500 - 0.64D2 * t
     #503
      t640 = 0.536D3 * t366 + 0.144D3 * t370 - 0.96D2 * t139 * t496 * t1
     #5 * t55 * t142 + 0.32D2 * t8 * t64 * t41 * t124 * t48 * t68 + 0.16
     #D2 * t586 * t10 * t41 * t155 * t65 * t15 - 0.384D3 * t431 * t557 *
     # t77 * z - 0.384D3 * t459 * t460 * x3 * t551 + 0.384D3 * t459 * t4
     #60 * x3 * z - 0.128D3 * t373 + 0.32D2 * t378 - 0.288D3 * t384 + 0.
     #144D3 * t387 + 0.64D2 * t398 + 0.64D2 * t403 + 0.25264D5 * t514
      rrqg2qgh63J5 = -(wd * (t165 + t255 + t332 + t409) + wd * (t427 + t
     #456 + t479 + t516) + wd * (t533 + t580 + t601 + t640)) / t1 / t41 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = x1 ** 2
      t9 = t8 ** 2
      t10 = t7 * t9
      t12 = z + x1 * t4
      t13 = t12 ** 2
      t15 = 0.1D1 / t13 / t12
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t20 = x2 * t19
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t12 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t12 + t20 - t28
      t30 = t15 * t29
      t31 = s * t4
      t32 = 0.1D1 / t12
      t36 = 0.1D1 - x1
      t37 = t36 * x3
      t39 = s - t31 * x1 * t32 * t29 - t31 * t37
      t42 = x2 * x3
      t43 = t19 * t16 * t12 + t42 + t28
      t44 = t43 ** 2
      t47 = t10 * t30 * t39 * t44
      t49 = t5 * t4
      t50 = t3 * t49
      t51 = t8 * x1
      t52 = t50 * t51
      t53 = 0.1D1 / t13
      t55 = t39 * t43
      t57 = t52 * t53 * t29 * t55
      t59 = t7 * x1
      t60 = t36 ** 2
      t61 = t60 * t36
      t62 = t19 ** 2
      t64 = t39 * x3
      t66 = t59 * t61 * t62 * t64
      t68 = t2 * t1
      t69 = t6 * t4
      t70 = t68 * t69
      t72 = t36 * t19
      t73 = t44 * t43
      t78 = t68 * t6
      t80 = t32 * t29
      t81 = x3 ** 2
      t82 = t60 * t81
      t86 = t70 * t51
      t87 = t60 * t62
      t88 = t53 * t44
      t92 = t29 ** 2
      t93 = t53 * t92
      t98 = t81 * x3
      t99 = t61 * t98
      t101 = t70 * t8 * t80 * t99
      t106 = t59 * t61 * t19 * t39 * t81
      t110 = t10 * t15 * t92 * t55
      t118 = t50 * x1
      t121 = t118 * t60 * t19 * t64
      t123 = t6 * t5
      t124 = t68 * t123
      t125 = t60 ** 2
      t128 = t19 * t81
      t129 = x2 * t32
      t133 = t3 * t69
      t134 = t8 * t61
      t135 = t133 * t134
      t136 = t62 * t39
      t138 = t135 * t136 * t129
      t140 = t3 * t123
      t141 = t51 * t61
      t143 = t19 * t39
      t144 = x2 ** 2
      t145 = t144 * t53
      t147 = t140 * t141 * t143 * t145
      t149 = -0.264D3 * t47 + 0.640D3 * t57 - 0.108D3 * t66 + 0.144D3 * 
     #t70 * t9 * t72 * t15 * t73 - 0.72D2 * t78 * t8 * t80 * t82 - 0.72D
     #2 * t86 * t87 * t88 - 0.72D2 * t86 * t93 * t82 - 0.56D2 * t101 - 0
     #.30D2 * t106 - 0.64D2 * t110 + 0.64D2 * t50 * t8 * t39 * x2 * t36 
     #* t32 - 0.256D3 * t121 - 0.72D2 * t124 * t8 * t125 * t128 * t129 +
     # 0.604D3 * t138 - 0.542D3 * t147
      t150 = t51 * t53
      t152 = t29 * t39
      t153 = t60 * x3
      t156 = t133 * t150 * t152 * t153 * x2
      t158 = t9 * t15
      t160 = t144 * t60
      t162 = t140 * t158 * t152 * t160
      t164 = t70 * t158
      t166 = x3 * t43
      t170 = t51 * t60
      t172 = t53 * t43
      t175 = t133 * t170 * t143 * t172 * x2
      t177 = t3 * t5
      t178 = t177 * x1
      t179 = t39 * t36
      t181 = t178 * t179 * x3
      t185 = t135 * t143 * t42 * t32
      t188 = t118 * t87 * t39
      t193 = t70 * t134
      t195 = t32 * t43
      t200 = t7 * t8 * t60
      t203 = t200 * t143 * t195 * x3
      t206 = t9 * x1
      t207 = t13 ** 2
      t208 = 0.1D1 / t207
      t209 = t206 * t208
      t213 = 0.16D2 * t124 * t37 * t209 * t73 * t29
      t214 = t133 * t158
      t218 = t214 * t152 * t43 * x2 * t36
      t220 = t7 * t150
      t221 = t92 * t39
      t223 = t220 * t221 * t37
      t225 = t70 * x1
      t233 = t59 * t61 * t62 * t19 * t39
      t235 = -0.108D3 * t156 - 0.54D2 * t162 + 0.144D3 * t164 * t92 * t3
     #6 * t166 + 0.544D3 * t175 + 0.64D2 * t181 + 0.444D3 * t185 - 0.256
     #D3 * t188 + 0.136D3 * t178 * t72 * t39 + 0.144D3 * t193 * t62 * x3
     # * t195 - 0.288D3 * t203 - t213 + 0.496D3 * t218 + 0.216D3 * t223 
     #- 0.72D2 * t225 * t125 * t62 * t81 - 0.62D2 * t233
      t241 = t70 * t206
      t247 = t52 * t93 * t39
      t251 = t177 * t8 * t80 * t39
      t256 = t10 * t15 * t92 * t29 * t39
      t264 = t225 * t125 * t19 * t98
      t268 = t53 * x3 * t43
      t273 = t193 * t128 * t195
      t275 = x2 * t36
      t278 = t220 * t152 * t275 * z
      t280 = t8 * t32
      t281 = t50 * t280
      t285 = t281 * t152 * z * t36 * x3
      t287 = t51 * t39
      t288 = t7 * t287
      t290 = t288 * t37 * t88
      t292 = t8 * t39
      t295 = t50 * t292 * t195 * t37
      t301 = 0.32D2 * t124 * t82 * t158 * t44 * t29
      t303 = t200 * t143 * t129
      t305 = -0.72D2 * t78 * t9 * t30 * t44 - 0.72D2 * t241 * t208 * t92
     # * t44 + 0.608D3 * t247 - 0.592D3 * t251 - 0.16D2 * t256 + 0.144D3
     # * t241 * t208 * t29 * t73 - 0.56D2 * t264 + 0.144D3 * t124 * t141
     # * t20 * t268 + 0.256D3 * t273 + 0.256D3 * t278 + 0.256D3 * t285 +
     # 0.64D2 * t290 - 0.128D3 * t295 + t301 + 0.256D3 * t303
      t309 = t50 * t8 * t36 * t143 * t195
      t311 = t9 * t39
      t315 = t133 * t311 * t15 * t44 * t275
      t318 = t288 * t275 * t172
      t323 = t220 * t152 * t43 * t36 * x3
      t326 = t200 * t136 * t195
      t333 = t7 * t280 * t152 * t82
      t339 = t29 * t60
      t342 = t70 * t150 * t339 * t81 * t43
      t344 = t29 * t36
      t347 = t164 * t344 * x3 * t44
      t352 = t7 * t51 * t36 * t143 * t88
      t361 = t70 * t170 * t19 * x3 * t88
      t366 = t44 * x2
      t374 = 0.16D2 * t124 * t99 * t150 * t43 * t29
      t376 = t281 * t152 * t37
      t378 = -0.216D3 * t309 + 0.64D2 * t315 - 0.128D3 * t318 + 0.496D3 
     #* t323 - 0.256D3 * t326 + 0.216D3 * t214 * t221 * t275 - 0.54D2 * 
     #t333 - 0.640D3 * t220 * t152 * t275 + 0.256D3 * t342 - 0.344D3 * t
     #347 + 0.24D2 * t352 + 0.144D3 * t78 * t150 * t344 * t166 - 0.344D3
     # * t361 - 0.72D2 * t124 * t9 * t60 * t19 * t15 * t366 - t374 - 0.6
     #40D3 * t376
      t395 = t39 * t60
      t401 = t3 * t6 * t49
      t409 = z ** 2
      t425 = 0.32D2 * t133 * t51 * t39 * t144 * t60 * t53 + 0.64D2 * t47
     # - 0.128D3 * t57 - 0.29128D5 * t66 + 0.144D3 * t101 - 0.28792D5 * 
     #t106 + 0.128D3 * t110 - 0.25248D5 * t121 - 0.384D3 * t118 * t395 *
     # t81 * z + 0.16D2 * t401 * t9 * t39 * t144 * x2 * t61 * t15 - 0.38
     #4D3 * t178 * t179 * x3 * t409 + 0.48D2 * t133 * t292 * t61 * t81 *
     # t129 - 0.96D2 * t140 * t311 * t15 * t43 * t160 - 0.27920D5 * t138
      t429 = t7 * t292
      t452 = 0.28208D5 * t147 + 0.8D1 * t156 + 0.4D1 * t162 - 0.96D2 * t
     #429 * t82 * t195 - 0.74208D5 * t175 - 0.128D3 * t181 + 0.536D3 * t
     #185 - 0.25296D5 * t188 - 0.16D2 * t401 * t209 * t43 * t144 * t339 
     #* t39 + 0.74288D5 * t203 + 0.48D2 * t140 * t287 * t61 * x3 * t145 
     #- t213 - 0.8D1 * t218 - 0.128D3 * t223 - 0.304D3 * t233
      t481 = -0.128D3 * t247 + 0.64D2 * t251 + 0.64D2 * t256 + 0.144D3 *
     # t264 - 0.288D3 * t273 - 0.128D3 * t278 + 0.288D3 * t118 * t395 * 
     #t81 - 0.512D3 * t285 - 0.192D3 * t133 * t287 * x2 * t60 * t268 + 0
     #.16D2 * t140 * t209 * t366 * t344 * t39 - 0.112D3 * t59 * t39 * t6
     #1 * t98 + 0.128D3 * t290 - 0.64D2 * t295 + t301 + 0.25264D5 * t303
      t507 = -0.80D2 * t309 + 0.128D3 * t315 - 0.64D2 * t318 - 0.8D1 * t
     #323 + 0.74320D5 * t326 - 0.252D3 * t333 - 0.288D3 * t342 + 0.144D3
     # * t347 + 0.32D2 * t352 + 0.384D3 * t178 * t179 * x3 * z + 0.128D3
     # * t68 * t5 * t36 * x3 * t409 * z * x1 + 0.144D3 * t361 - t374 + 0
     #.256D3 * t376 + 0.64D2 * t429 * t153 * t129
      rrqg2qgh63J6 = -(wd * (t149 + t235 + t305 + t378) + wd * (t425 + t
     #452 + t481 + t507)) / t1 / t39 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh63J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t3 * t6
      t8 = x1 ** 2
      t9 = t8 * x1
      t10 = t7 * t9
      t12 = z + x1 * t4
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t12 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t12 + x2 * t18 - t27
      t29 = t28 ** 2
      t31 = s * t4
      t32 = 0.1D1 / t12
      t36 = 0.1D1 - x1
      t37 = t36 * x3
      t39 = s - t31 * x1 * t32 * t28 - t31 * t37
      t43 = t5 ** 2
      t44 = t3 * t43
      t45 = t44 * x1
      t46 = t36 ** 2
      t47 = t46 * t36
      t48 = t18 ** 2
      t54 = t2 * t1
      t55 = t43 * t4
      t56 = t54 * t55
      t58 = t46 ** 2
      t60 = x3 ** 2
      t61 = t60 * x3
      t65 = t8 ** 2
      t66 = t44 * t65
      t68 = 0.1D1 / t13 / t12
      t74 = t3 * t5
      t76 = t32 * t28
      t80 = t43 * t5
      t81 = t3 * t80
      t82 = t65 * t39
      t86 = x2 * x3
      t87 = t18 * t15 * t12 + t86 + t27
      t89 = x2 ** 2
      t90 = t89 * t46
      t94 = t74 * x1
      t95 = t39 * t36
      t99 = t7 * x1
      t106 = z ** 2
      t112 = t39 * t46
      t121 = t3 * t43 * t6
      t123 = t13 ** 2
      t125 = t65 * x1 / t123
      t128 = t46 * t28
      t133 = t3 * t55
      t134 = t9 * t39
      t143 = t87 ** 2
      t145 = t36 * t28
      t150 = -0.128D3 * t10 * t14 * t29 * t39 - 0.304D3 * t45 * t47 * t4
     #8 * t18 * t39 + 0.144D3 * t56 * x1 * t58 * t18 * t61 + 0.64D2 * t6
     #6 * t68 * t29 * t28 * t39 + 0.64D2 * t74 * t8 * t76 * t39 - 0.96D2
     # * t81 * t82 * t68 * t87 * t90 - 0.128D3 * t94 * t95 * x3 - 0.2529
     #6D5 * t99 * t46 * t48 * t39 + 0.128D3 * t54 * t5 * t36 * x3 * t106
     # * z * x1 + 0.288D3 * t99 * t112 * t60 - 0.112D3 * t45 * t39 * t47
     # * t61 - 0.16D2 * t121 * t125 * t87 * t89 * t128 * t39 - 0.192D3 *
     # t133 * t134 * x2 * t46 * t14 * x3 * t87 + 0.16D2 * t81 * t125 * t
     #143 * x2 * t145 * t39
      t151 = t54 * t80
      t158 = t65 * t68
      t160 = t28 * t39
      t166 = t9 * t14
      t167 = t44 * t166
      t174 = t46 * x3
      t181 = t89 * t14
      t185 = t8 * t39
      t186 = t44 * t185
      t187 = t46 * t60
      t188 = t32 * t87
      t192 = t44 * t134
      t193 = x2 * t36
      t194 = t14 * t87
      t198 = t14 * t143
      t212 = x2 * t32
      t220 = t8 * t47
      t221 = t133 * t220
      t222 = t18 * t39
      t228 = t44 * t8 * t46
      t238 = -0.16D2 * t151 * t37 * t125 * t143 * t87 * t28 - 0.8D1 * t1
     #33 * t158 * t160 * t87 * x2 * t36 - 0.8D1 * t167 * t160 * t87 * t3
     #6 * x3 + 0.8D1 * t133 * t166 * t160 * t174 * x2 + 0.48D2 * t81 * t
     #134 * t47 * x3 * t181 - 0.96D2 * t186 * t187 * t188 - 0.64D2 * t19
     #2 * t193 * t194 + 0.128D3 * t192 * t37 * t198 - 0.64D2 * t7 * t185
     # * t188 * t37 - 0.128D3 * t167 * t160 * t193 * z + 0.48D2 * t133 *
     # t185 * t47 * t60 * t212 - 0.128D3 * t167 * t29 * t39 * t37 + 0.53
     #6D3 * t221 * t222 * t86 * t32 + 0.74288D5 * t228 * t222 * t188 * x
     #3 + 0.32D2 * t151 * t187 * t158 * t143 * t28
      t249 = t39 * x3
      t259 = t39 * t87
      t268 = t47 * t61
      t298 = t9 * t46
      t304 = t48 * t39
      t313 = 0.25264D5 * t228 * t222 * t212 - 0.80D2 * t7 * t8 * t36 * t
     #222 * t188 - 0.25248D5 * t99 * t46 * t18 * t249 - 0.28792D5 * t45 
     #* t47 * t18 * t39 * t60 + 0.128D3 * t66 * t68 * t29 * t259 - 0.291
     #28D5 * t45 * t47 * t48 * t249 + 0.144D3 * t56 * t8 * t76 * t268 - 
     #0.128D3 * t10 * t14 * t28 * t259 - 0.288D3 * t56 * t220 * t18 * t6
     #0 * t188 - 0.384D3 * t99 * t112 * t60 * z + 0.16D2 * t121 * t65 * 
     #t39 * t89 * x2 * t47 * t68 + 0.32D2 * t133 * t9 * t39 * t89 * t46 
     #* t14 + 0.144D3 * t56 * t298 * t18 * x3 * t198 - 0.27920D5 * t221 
     #* t304 * t212 + 0.28208D5 * t81 * t9 * t47 * t222 * t181
      t335 = t8 * t32
      t336 = t7 * t335
      t381 = 0.64D2 * t66 * t68 * t28 * t39 * t143 + 0.384D3 * t94 * t95
     # * x3 * z - 0.384D3 * t94 * t95 * x3 * t106 + 0.128D3 * t133 * t82
     # * t68 * t143 * t193 + 0.64D2 * t186 * t174 * t212 - 0.512D3 * t33
     #6 * t160 * z * t36 * x3 - 0.252D3 * t44 * t335 * t160 * t187 + 0.4
     #D1 * t81 * t158 * t160 * t90 + 0.74320D5 * t228 * t304 * t188 + 0.
     #32D2 * t44 * t9 * t36 * t222 * t198 - 0.288D3 * t56 * t166 * t128 
     #* t60 * t87 + 0.144D3 * t56 * t158 * t145 * x3 * t143 - 0.74208D5 
     #* t133 * t298 * t222 * t194 * x2 + 0.256D3 * t336 * t160 * t37 - 0
     #.16D2 * t151 * t268 * t166 * t87 * t28
      rrqg2qgh63J7 = -wd * (t150 + t238 + t313 + t381) / t1 / t39 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh64J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = s * t5
      t8 = z + x1 * t5
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t15 = x2 * x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + t15 + t24
      t28 = 0.1D1 - x1
      t29 = t28 * t11
      t31 = s - t6 * x1 * t9 * t25 - t6 * t29
      t32 = t5 ** 2
      t33 = t32 * t5
      t35 = t4 * t31 * t33
      t36 = x1 ** 2
      t42 = t32 ** 2
      t44 = t4 * t31 * t42
      t45 = t28 ** 2
      t47 = t9 * t15
      t51 = t3 * t33
      t52 = t36 * x1
      t53 = t8 ** 2
      t54 = 0.1D1 / t53
      t55 = t52 * t54
      t59 = t18 * t8 + x2 * t11 - t24
      t60 = t59 * t31
      t65 = t2 * t1
      t66 = t65 * z
      t69 = t9 * t59
      t70 = t28 * x3
      t76 = x3 * t9 * t25
      t80 = t3 * t42
      t81 = t11 ** 2
      t84 = t31 * t36
      t86 = t84 * t9 * t25
      t89 = t45 * t11
      t90 = t80 * t89
      t95 = t84 * x2 * t9
      t100 = x3 ** 2
      t101 = t45 * t100
      t105 = t42 * t32
      t106 = t36 ** 2
      t110 = 0.1D1 / t53 / t8
      t111 = t110 * t59
      t112 = x2 ** 2
      t113 = t112 * t45
      t117 = t42 * t5
      t120 = t54 * t59
      t122 = t45 * x3 * x2
      t126 = t36 * t9
      t131 = t3 * t117
      t132 = t106 * t110
      t133 = t131 * t132
      t134 = t59 ** 2
      t135 = t134 * t59
      t136 = t135 * t31
      t140 = 0.288D3 * t35 * t36 * x2 * t28 * t9 - 0.288D3 * t44 * t36 *
     # t45 * t47 - 0.1728D4 * t51 * t55 * t60 * z * t25 - 0.1152D4 * t66
     # * t33 * t36 * t69 * t70 + 0.288D3 * t35 * t36 * t28 * t76 - 0.148
     #32D5 * t80 * t45 * t81 * t86 - 0.14976D5 * t90 * t84 * t76 - 0.149
     #76D5 * t90 * t95 + 0.576D3 * t66 * t42 * t36 * t69 * t101 + 0.576D
     #3 * t66 * t105 * t106 * t111 * t113 + 0.1152D4 * t66 * t117 * t52 
     #* t120 * t122 - 0.288D3 * t80 * t126 * t60 * t101 + 0.72D2 * t133 
     #* t136 * t70
      t142 = t45 * t28
      t153 = t80 * t106
      t159 = t126 * t59
      t162 = t51 * t45
      t163 = t81 * t31
      t167 = z ** 2
      t173 = t3 * t105
      t174 = t142 * t11
      t176 = t31 * t52
      t194 = t173 * t132
      t199 = t3 * t42 * t33
      t206 = t42 ** 2
      t209 = t53 ** 2
      t211 = t106 * x1 / t209
      t218 = t131 * t55
      t219 = t134 * t31
      t223 = 0.36D2 * t131 * t126 * t60 * t142 * t100 * x3 - 0.1440D4 * 
     #t51 * t52 * t54 * t134 * t31 + 0.1440D4 * t153 * t110 * t135 * t31
     # + 0.576D3 * t66 * t32 * t159 + 0.14832D5 * t162 * t163 * x1 + 0.5
     #76D3 * t65 * t167 * z * t32 * t159 - 0.144D3 * t173 * t174 * t176 
     #* t112 * t54 + 0.288D3 * t131 * t174 * t84 * t47 + 0.576D3 * t51 *
     # t29 * t86 + 0.108D3 * t173 * t55 * t60 * t142 * t100 * x2 - 0.288
     #D3 * t194 * t60 * t113 + 0.108D3 * t199 * t132 * t60 * t142 * x3 *
     # t112 + 0.36D2 * t3 * t206 * t211 * t60 * t112 * x2 * t142 + 0.72D
     #2 * t218 * t219 * t101
      t226 = t25 * x2 * t28
      t235 = x2 * t28
      t244 = t3 * t32 * t36
      t249 = t11 * t31
      t250 = x3 * x1
      t254 = t4 * t31
      t264 = t25 ** 2
      t269 = t80 * t142
      t288 = 0.288D3 * t44 * t55 * t226 + 0.72D2 * t199 * t211 * t219 * 
     #t113 + 0.72D2 * t173 * t211 * t136 * t235 + 0.144D3 * t131 * t142 
     #* t81 * t95 + 0.1728D4 * t244 * t69 * t31 * z + 0.14832D5 * t162 *
     # t249 * t250 + 0.288D3 * t254 * t32 * x1 * t70 - 0.144D3 * t254 * 
     #t33 * x1 * t101 - 0.288D3 * t254 * t33 * t52 * t54 * t264 - 0.144D
     #3 * t269 * t249 * x1 * t100 + 0.576D3 * t66 * t42 * t132 * t59 * t
     #264 + 0.1440D4 * t153 * t110 * t134 * t31 * t25 - 0.1728D4 * t244 
     #* t69 * t31 * t167
      t307 = t80 * t55
      t311 = t70 * t25
      t326 = t66 * t42 * t52
      t348 = 0.1152D4 * t66 * t33 * t55 * t59 * t25 - 0.144D3 * t269 * t
     #163 * t250 - 0.144D3 * t4 * t31 * t117 * t52 * t112 * t45 * t54 + 
     #0.144D3 * t133 * t60 * t226 + 0.144D3 * t307 * t60 * t235 + 0.144D
     #3 * t307 * t60 * t311 + 0.144D3 * t51 * t126 * t60 * t70 + 0.144D3
     # * t194 * t219 * t122 - 0.576D3 * t218 * t60 * t122 - 0.1152D4 * t
     #326 * t120 * t235 - 0.288D3 * t4 * t31 * t5 * x1 + 0.14832D5 * t13
     #1 * t89 * t176 * t54 * t25 * x2 - 0.1152D4 * t326 * t120 * t311 - 
     #0.1152D4 * t66 * t117 * t106 * t111 * t226
      rrqg2qgh64J1 = -wd * (t140 + t223 + t288 + t348) / t1 / t31 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh64J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 * t9
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t16 = s * t4
      t18 = z + x1 * t4
      t19 = 0.1D1 / t18
      t21 = 0.1D1 - x2
      t24 = x2 * x3
      t26 = cos(x4 * 0.3141592653589793D1)
      t27 = x3 * t21
      t31 = Sqrt(t27 * t18 * x2 * t12)
      t33 = 0.2D1 * t26 * t31
      t34 = t12 * t21 * t18 + t24 + t33
      t37 = t9 * t12
      t39 = s - t16 * x1 * t19 * t34 - t16 * t37
      t40 = x1 ** 2
      t41 = t39 * t40
      t43 = t41 * x2 * t19
      t44 = t8 * t11 * t13 * t43
      t46 = t6 * t5
      t47 = t3 * t46
      t48 = t11 * t12
      t50 = t40 * x1
      t51 = t39 * t50
      t52 = x2 ** 2
      t53 = t18 ** 2
      t54 = 0.1D1 / t53
      t57 = t47 * t48 * t51 * t52 * t54
      t59 = t5 * t4
      t60 = t3 * t59
      t62 = t19 * t34
      t63 = t41 * t62
      t64 = t60 * t37 * t63
      t66 = t3 * t6
      t69 = t66 * t10 * t13 * t63
      t72 = t3 * t6 * t59
      t73 = t40 ** 2
      t75 = t53 ** 2
      t77 = t73 * x1 / t75
      t81 = t27 * t18 + x2 * t12 - t33
      t82 = t81 ** 2
      t83 = t82 * t39
      t84 = t52 * t10
      t88 = t2 * t1
      t89 = t88 * z
      t92 = t19 * t81
      t93 = t9 * x3
      t97 = t40 * t19
      t99 = t81 * t39
      t100 = x3 ** 2
      t101 = t10 * t100
      t106 = t89 * t6 * t50
      t107 = t54 * t81
      t108 = t93 * t34
      t115 = 0.1D1 / t53 / t18
      t116 = t115 * t81
      t118 = t34 * x2 * t9
      t123 = t24 * t19
      t125 = t8 * t48 * t41 * t123
      t127 = t73 * t115
      t134 = t47 * t127
      t136 = x2 * t10 * x3
      t140 = t50 * t54
      t141 = t8 * t140
      t145 = 0.144D3 * t44 - 0.144D3 * t57 + 0.576D3 * t64 - 0.14832D5 *
     # t69 + 0.72D2 * t72 * t77 * t83 * t84 - 0.1152D4 * t89 * t59 * t40
     # * t92 * t93 - 0.288D3 * t66 * t97 * t99 * t101 - 0.1152D4 * t106 
     #* t107 * t108 - 0.1152D4 * t89 * t7 * t73 * t116 * t118 + 0.288D3 
     #* t125 + 0.108D3 * t72 * t127 * t99 * t11 * x3 * t52 + 0.144D3 * t
     #134 * t83 * t136 - 0.576D3 * t141 * t99 * t136
      t146 = t3 * z
      t148 = t146 * t39 * t6
      t151 = t148 * t40 * t10 * t123
      t153 = t6 ** 2
      t166 = 0.288D3 * t148 * t140 * t118
      t167 = x2 * t9
      t186 = t8 * t127
      t187 = t82 * t81
      t188 = t187 * t39
      t191 = 0.72D2 * t186 * t188 * t93
      t198 = t60 * t50
      t201 = t198 * t54 * t82 * t39
      t203 = t66 * t73
      t207 = 0.1440D4 * t203 * t115 * t187 * t39
      t209 = t97 * t81
      t212 = t60 * t10
      t213 = t13 * t39
      t215 = t212 * t213 * x1
      t217 = -0.288D3 * t151 + 0.36D2 * t3 * t153 * t77 * t99 * t52 * x2
     # * t11 + 0.72D2 * t141 * t83 * t101 + t166 - 0.1152D4 * t106 * t10
     #7 * t167 + 0.576D3 * t89 * t6 * t40 * t92 * t101 + 0.576D3 * t89 *
     # t46 * t73 * t116 * t84 + 0.1152D4 * t89 * t7 * t50 * t107 * t136 
     #+ t191 + 0.36D2 * t8 * t97 * t99 * t11 * t100 * x3 - 0.1440D4 * t2
     #01 + t207 + 0.576D3 * t89 * t5 * t209 + 0.14832D5 * t215
      t219 = z ** 2
      t224 = 0.576D3 * t88 * t219 * z * t5 * t209
      t225 = t10 * t12
      t226 = t66 * t225
      t228 = x3 * t19 * t34
      t230 = t226 * t41 * t228
      t232 = t226 * t43
      t238 = t146 * t39 * t59
      t242 = 0.288D3 * t238 * t40 * t9 * t228
      t247 = 0.288D3 * t238 * t40 * x2 * t9 * t19
      t253 = t146 * t39 * t7 * t50 * t52 * t10 * t54
      t259 = t8 * t225 * t51 * t54 * t34 * x2
      t262 = t186 * t99 * t118
      t264 = t3 * t5
      t265 = t264 * t40
      t269 = 0.1728D4 * t265 * t92 * t39 * z
      t270 = t12 * t39
      t271 = x3 * x1
      t273 = t212 * t270 * t271
      t275 = t146 * t39
      t279 = 0.288D3 * t275 * t5 * x1 * t93
      t282 = t275 * t59 * x1 * t101
      t284 = t224 - 0.14976D5 * t230 - 0.14976D5 * t232 - 0.288D3 * t134
     # * t99 * t84 + t242 + t247 - 0.144D3 * t253 + 0.14832D5 * t259 + 0
     #.144D3 * t262 + t269 + 0.14832D5 * t273 + t279 - 0.144D3 * t282
      t286 = t34 ** 2
      t287 = t54 * t286
      t290 = 0.288D3 * t275 * t59 * t50 * t287
      t291 = t66 * t11
      t294 = t291 * t270 * x1 * t100
      t302 = t39 * t34
      t304 = t203 * t115 * t82 * t302
      t309 = 0.1728D4 * t265 * t92 * t39 * t219
      t316 = t291 * t213 * t271
      t322 = 0.1728D4 * t60 * t140 * t99 * z * t34
      t323 = t66 * t140
      t325 = t323 * t99 * t167
      t327 = t47 * t77
      t330 = 0.72D2 * t327 * t188 * t167
      t332 = t323 * t99 * t108
      t336 = t60 * t97 * t99 * t93
      t341 = 0.288D3 * t146 * t39 * t4 * x1
      t348 = -t290 - 0.144D3 * t294 + 0.576D3 * t89 * t6 * t127 * t81 * 
     #t286 + 0.1440D4 * t304 - t309 + 0.1152D4 * t89 * t59 * t140 * t81 
     #* t34 - 0.144D3 * t316 - t322 + 0.144D3 * t325 + t330 + 0.144D3 * 
     #t332 + 0.144D3 * t336 - t341 + 0.108D3 * t47 * t140 * t99 * t11 * 
     #t100 * x2
      t373 = -0.2160D4 * t44 - 0.936D3 * t57 - 0.864D3 * t64 + 0.15552D5
     # * t69 + 0.3096D4 * t291 * t13 * t12 * t39 * x1 + 0.432D3 * t264 *
     # t9 * t270 * x1 - 0.288D3 * t265 * t92 * t39 + 0.1872D4 * t125 + 0
     #.576D3 * t151 + 0.72D2 * t186 * t83 * t108 + t166
      t386 = -t191 + 0.72D2 * t327 * t99 * t286 * x2 * t9 + 0.72D2 * t32
     #7 * t83 * t118 + 0.1728D4 * t201 - t207 - 0.15552D5 * t215 - t224 
     #+ 0.15552D5 * t230 + 0.15552D5 * t232 + t242 + t247
      t399 = 0.288D3 * t253 - 0.15696D5 * t259 - 0.72D2 * t262 + 0.432D3
     # * t66 * t37 * t51 * t287 - t269 - 0.15696D5 * t273 + t279 + 0.288
     #D3 * t282 + t290 - 0.936D3 * t294 - 0.1728D4 * t304
      t420 = t309 + 0.2160D4 * t316 - 0.576D3 * t275 * t5 * t40 * t62 - 
     #0.288D3 * t203 * t116 * t39 * t286 + 0.576D3 * t198 * t107 * t302 
     #+ t322 - 0.72D2 * t325 + 0.72D2 * t186 * t99 * t286 * t9 * x3 - t3
     #30 - 0.72D2 * t332 - 0.72D2 * t336 + t341
      rrqg2qgh64J2 = -(wd * (t145 + t217 + t284 + t348) + wd * (t373 + t
     #386 + t399 + t420)) / t1 / t39 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh64J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = s * t5
      t8 = z + x1 * t5
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t15 = x2 * x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + t15 + t24
      t28 = 0.1D1 - x1
      t29 = t28 * t11
      t31 = s - t6 * x1 * t9 * t25 - t6 * t29
      t32 = t5 ** 2
      t33 = t32 * t5
      t35 = t4 * t31 * t33
      t36 = x1 ** 2
      t41 = 0.288D3 * t35 * t36 * x2 * t28 * t9
      t42 = t32 ** 2
      t44 = t4 * t31 * t42
      t45 = t28 ** 2
      t47 = t9 * t15
      t49 = t44 * t36 * t45 * t47
      t51 = t3 * t33
      t53 = t31 * t36
      t54 = t9 * t25
      t55 = t53 * t54
      t56 = t51 * t29 * t55
      t58 = t3 * t42
      t59 = t36 * x1
      t60 = t8 ** 2
      t61 = 0.1D1 / t60
      t62 = t59 * t61
      t63 = t58 * t62
      t66 = t18 * t8 + x2 * t11 - t24
      t67 = t66 * t31
      t68 = t28 * x3
      t69 = t68 * t25
      t71 = t63 * t67 * t69
      t73 = t36 * t9
      t76 = t51 * t73 * t67 * t68
      t79 = x3 ** 2
      t80 = t45 * t79
      t85 = t3 * t42 * t33
      t86 = t36 ** 2
      t88 = t60 ** 2
      t90 = t86 * x1 / t88
      t92 = t66 ** 2
      t93 = t92 * t31
      t94 = x2 ** 2
      t95 = t94 * t45
      t99 = t42 * t32
      t100 = t3 * t99
      t101 = t100 * t90
      t102 = t92 * t66
      t103 = t102 * t31
      t104 = x2 * t28
      t107 = 0.72D2 * t101 * t103 * t104
      t108 = t3 * t32
      t109 = t108 * t36
      t110 = t9 * t66
      t114 = 0.1728D4 * t109 * t110 * t31 * z
      t115 = t51 * t45
      t116 = t11 * t31
      t117 = x3 * x1
      t119 = t115 * t116 * t117
      t121 = t4 * t31
      t125 = 0.288D3 * t121 * t32 * x1 * t68
      t128 = t121 * t33 * x1 * t80
      t131 = t25 ** 2
      t132 = t61 * t131
      t135 = 0.288D3 * t121 * t33 * t59 * t132
      t136 = t41 - 0.288D3 * t49 + 0.576D3 * t56 + 0.144D3 * t71 + 0.144
     #D3 * t76 - 0.288D3 * t58 * t73 * t67 * t80 + 0.72D2 * t85 * t90 * 
     #t93 * t95 + t107 + t114 + 0.14832D5 * t119 + t125 - 0.144D3 * t128
     # - t135
      t137 = t45 * t28
      t138 = t58 * t137
      t141 = t138 * t116 * x1 * t79
      t143 = t2 * t1
      t144 = t143 * z
      t147 = 0.1D1 / t60 / t8
      t148 = t86 * t147
      t153 = t58 * t86
      t155 = t31 * t25
      t157 = t153 * t147 * t92 * t155
      t159 = z ** 2
      t163 = 0.1728D4 * t109 * t110 * t31 * t159
      t169 = t11 ** 2
      t170 = t169 * t31
      t172 = t138 * t170 * t117
      t174 = t42 * t5
      t175 = t3 * t174
      t176 = t175 * t148
      t178 = t25 * x2 * t28
      t180 = t176 * t67 * t178
      t184 = t58 * t45 * t169 * t55
      t188 = t147 * t66
      t196 = 0.1728D4 * t51 * t62 * t67 * z * t25
      t198 = t144 * t42 * t59
      t199 = t61 * t66
      t205 = 0.72D2 * t176 * t103 * t68
      t214 = x3 * t9 * t25
      t217 = 0.288D3 * t35 * t36 * t28 * t214
      t218 = -0.144D3 * t141 + 0.576D3 * t144 * t42 * t148 * t66 * t131 
     #+ 0.1440D4 * t157 - t163 + 0.1152D4 * t144 * t33 * t62 * t66 * t25
     # - 0.144D3 * t172 + 0.144D3 * t180 - 0.14832D5 * t184 - 0.1152D4 *
     # t144 * t174 * t86 * t188 * t178 - t196 - 0.1152D4 * t198 * t199 *
     # t69 + t205 + 0.36D2 * t175 * t73 * t67 * t137 * t79 * x3 + t217
      t220 = t100 * t148
      t222 = t45 * x3 * x2
      t226 = t175 * t62
      t242 = t51 * t59
      t245 = t242 * t61 * t92 * t31
      t250 = 0.1440D4 * t153 * t147 * t102 * t31
      t252 = t66 * t73
      t256 = t115 * t170 * x1
      t262 = 0.576D3 * t143 * t159 * z * t32 * t252
      t263 = t45 * t11
      t264 = t58 * t263
      t266 = t264 * t53 * t214
      t269 = t53 * x2 * t9
      t270 = t264 * t269
      t274 = t175 * t137 * t169 * t269
      t279 = 0.288D3 * t4 * t31 * t5 * x1
      t280 = 0.144D3 * t220 * t93 * t222 - 0.576D3 * t226 * t67 * t222 +
     # 0.108D3 * t100 * t62 * t67 * t137 * t79 * x2 + 0.108D3 * t85 * t1
     #48 * t67 * t137 * x3 * t94 - 0.1440D4 * t245 + t250 + 0.576D3 * t1
     #44 * t32 * t252 + 0.14832D5 * t256 + t262 - 0.14976D5 * t266 - 0.1
     #4976D5 * t270 + 0.144D3 * t274 - t279
      t285 = t59 * t31
      t289 = t175 * t263 * t285 * t61 * t25 * x2
      t306 = 0.288D3 * t44 * t62 * t178
      t308 = t63 * t67 * t104
      t328 = t4 * t31 * t174 * t59 * t94 * t45 * t61
      t330 = t137 * t11
      t334 = t100 * t330 * t285 * t94 * t61
      t338 = t175 * t330 * t53 * t47
      t340 = t42 ** 2
      t348 = -0.1152D4 * t198 * t199 * t104 + 0.14832D5 * t289 + 0.576D3
     # * t144 * t42 * t36 * t110 * t80 + 0.576D3 * t144 * t99 * t86 * t1
     #88 * t95 + 0.72D2 * t226 * t93 * t80 + t306 + 0.144D3 * t308 - 0.2
     #88D3 * t220 * t67 * t95 + 0.1152D4 * t144 * t174 * t59 * t199 * t2
     #22 - 0.1152D4 * t144 * t33 * t36 * t110 * t68 - 0.144D3 * t328 - 0
     #.144D3 * t334 + 0.288D3 * t338 + 0.36D2 * t3 * t340 * t90 * t67 * 
     #t94 * x2 * t137
      t358 = t41 + 0.576D3 * t49 - 0.864D3 * t56 - 0.72D2 * t71 - 0.72D2
     # * t76 - t107 - t114 - 0.15696D5 * t119 + t125 + 0.288D3 * t128 + 
     #t135
      t383 = -0.936D3 * t141 - 0.1728D4 * t157 + t163 + 0.2160D4 * t172 
     #- 0.576D3 * t121 * t32 * t36 * t54 - 0.288D3 * t153 * t188 * t31 *
     # t131 + 0.576D3 * t242 * t199 * t155 + 0.72D2 * t101 * t67 * t131 
     #* x2 * t28 + 0.72D2 * t101 * t93 * t178 - 0.72D2 * t180 + 0.15552D
     #5 * t184
      t400 = t196 - t205 + t217 + 0.1728D4 * t245 - t250 - 0.15552D5 * t
     #256 - t262 + 0.3096D4 * t138 * t169 * t11 * t31 * x1 + 0.432D3 * t
     #108 * t28 * t116 * x1 - 0.288D3 * t109 * t110 * t31 + 0.15552D5 * 
     #t266
      t420 = 0.15552D5 * t270 - 0.2160D4 * t274 + t279 - 0.15696D5 * t28
     #9 + t306 - 0.72D2 * t308 + 0.432D3 * t58 * t29 * t285 * t132 + 0.2
     #88D3 * t328 + 0.72D2 * t176 * t93 * t69 - 0.936D3 * t334 + 0.1872D
     #4 * t338 + 0.72D2 * t176 * t67 * t131 * t28 * x3
      rrqg2qgh64J3 = -(wd * (t136 + t218 + t280 + t348) + wd * (t358 + t
     #383 + t400 + t420)) / t1 / t31 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh64J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = s * t5
      t8 = z + x1 * t5
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t15 = x2 * x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + t15 + t24
      t28 = 0.1D1 - x1
      t29 = t28 * t11
      t31 = s - t6 * x1 * t9 * t25 - t6 * t29
      t32 = t5 ** 2
      t33 = t32 ** 2
      t35 = t4 * t31 * t33
      t36 = x1 ** 2
      t37 = t36 * x1
      t38 = t8 ** 2
      t39 = 0.1D1 / t38
      t40 = t37 * t39
      t42 = t25 * x2 * t28
      t45 = 0.288D3 * t35 * t40 * t42
      t46 = t33 * t32
      t47 = t3 * t46
      t48 = t28 ** 2
      t49 = t48 * t28
      t50 = t49 * t11
      t52 = t31 * t37
      t53 = x2 ** 2
      t56 = t47 * t50 * t52 * t53 * t39
      t61 = 0.288D3 * t4 * t31 * t5 * x1
      t62 = t3 * t33
      t63 = t62 * t40
      t66 = t18 * t8 + x2 * t11 - t24
      t67 = t66 * t31
      t68 = t28 * x3
      t69 = t68 * t25
      t71 = t63 * t67 * t69
      t73 = t33 ** 2
      t75 = t36 ** 2
      t77 = t38 ** 2
      t79 = t75 * x1 / t77
      t86 = t33 * t5
      t87 = t3 * t86
      t88 = t87 * t40
      t90 = t48 * x3 * x2
      t95 = x3 ** 2
      t101 = t2 * t1
      t102 = t101 * z
      t104 = t102 * t33 * t37
      t105 = t39 * t66
      t106 = x2 * t28
      t112 = t9 * t66
      t113 = t48 * t95
      t117 = z ** 2
      t121 = t36 * t9
      t122 = t121 * t66
      t124 = 0.576D3 * t101 * t117 * z * t32 * t122
      t125 = t11 ** 2
      t128 = t31 * t36
      t130 = t128 * x2 * t9
      t131 = t87 * t49 * t125 * t130
      t133 = t32 * t5
      t134 = t3 * t133
      t135 = t134 * t48
      t136 = t125 * t31
      t138 = t135 * t136 * x1
      t140 = t62 * t75
      t142 = 0.1D1 / t38 / t8
      t143 = t66 ** 2
      t144 = t143 * t66
      t148 = 0.1440D4 * t140 * t142 * t144 * t31
      t149 = t45 - 0.144D3 * t56 - t61 + 0.144D3 * t71 + 0.36D2 * t3 * t
     #73 * t79 * t67 * t53 * x2 * t49 - 0.576D3 * t88 * t67 * t90 + 0.10
     #8D3 * t47 * t40 * t67 * t49 * t95 * x2 - 0.1152D4 * t104 * t105 * 
     #t106 + 0.576D3 * t102 * t33 * t36 * t112 * t113 + t124 + 0.144D3 *
     # t131 + 0.14832D5 * t138 + t148
      t153 = t134 * t37
      t156 = t153 * t39 * t143 * t31
      t160 = t134 * t121 * t67 * t68
      t170 = 0.1728D4 * t134 * t40 * t67 * z * t25
      t176 = t142 * t66
      t182 = t9 * t25
      t183 = t128 * t182
      t184 = t62 * t48 * t125 * t183
      t187 = t3 * t33 * t133
      t189 = t143 * t31
      t190 = t53 * t48
      t194 = t47 * t79
      t195 = t144 * t31
      t198 = 0.72D2 * t194 * t195 * t106
      t200 = t4 * t31 * t133
      t203 = x3 * t9 * t25
      t206 = 0.288D3 * t200 * t36 * t28 * t203
      t207 = t48 * t11
      t212 = t87 * t207 * t52 * t39 * t25 * x2
      t214 = t62 * t207
      t216 = t214 * t128 * t203
      t218 = t214 * t130
      t220 = 0.576D3 * t102 * t32 * t122 - 0.1440D4 * t156 + 0.144D3 * t
     #160 - 0.288D3 * t62 * t121 * t67 * t113 - t170 - 0.1152D4 * t104 *
     # t105 * t69 - 0.1152D4 * t102 * t86 * t75 * t176 * t42 - 0.14832D5
     # * t184 + 0.72D2 * t187 * t79 * t189 * t190 + t198 + t206 + 0.1483
     #2D5 * t212 - 0.14976D5 * t216 - 0.14976D5 * t218
      t226 = t9 * t15
      t228 = t87 * t50 * t128 * t226
      t231 = t134 * t29 * t183
      t237 = 0.288D3 * t200 * t36 * x2 * t28 * t9
      t240 = t35 * t36 * t48 * t226
      t242 = t75 * t142
      t243 = t87 * t242
      t245 = t243 * t67 * t42
      t247 = t3 * t32
      t248 = t247 * t36
      t252 = 0.1728D4 * t248 * t112 * t31 * z
      t253 = t4 * t31
      t255 = t25 ** 2
      t256 = t39 * t255
      t259 = 0.288D3 * t253 * t133 * t37 * t256
      t262 = t253 * t133 * x1 * t113
      t267 = 0.288D3 * t253 * t32 * x1 * t68
      t268 = t11 * t31
      t269 = x3 * x1
      t271 = t135 * t268 * t269
      t273 = t62 * t49
      t276 = t273 * t268 * x1 * t95
      t283 = 0.72D2 * t88 * t189 * t113 + 0.288D3 * t228 + 0.576D3 * t23
     #1 + t237 - 0.288D3 * t240 + 0.144D3 * t245 + t252 - t259 - 0.144D3
     # * t262 + t267 + 0.14832D5 * t271 - 0.144D3 * t276 + 0.576D3 * t10
     #2 * t33 * t242 * t66 * t255
      t287 = 0.1728D4 * t248 * t112 * t31 * t117
      t289 = t31 * t25
      t291 = t140 * t142 * t143 * t289
      t294 = t273 * t136 * t269
      t301 = t47 * t242
      t307 = 0.72D2 * t243 * t195 * t68
      t315 = t63 * t67 * t106
      t346 = t4 * t31 * t86 * t37 * t53 * t48 * t39
      t348 = -t287 + 0.1440D4 * t291 - 0.144D3 * t294 + 0.1152D4 * t102 
     #* t133 * t40 * t66 * t25 + 0.144D3 * t301 * t189 * t90 + t307 + 0.
     #36D2 * t87 * t121 * t67 * t49 * t95 * x3 + 0.144D3 * t315 - 0.288D
     #3 * t301 * t67 * t190 + 0.108D3 * t187 * t242 * t67 * t49 * x3 * t
     #53 + 0.576D3 * t102 * t46 * t75 * t176 * t190 + 0.1152D4 * t102 * 
     #t86 * t37 * t105 * t90 - 0.1152D4 * t102 * t133 * t36 * t112 * t68
     # - 0.144D3 * t346
      t368 = t45 - 0.936D3 * t56 + t61 - 0.72D2 * t71 - 0.288D3 * t248 *
     # t112 * t31 + 0.432D3 * t247 * t28 * t268 * x1 + 0.3096D4 * t273 *
     # t125 * t11 * t31 * x1 - t124 - 0.2160D4 * t131 - 0.15552D5 * t138
     # - t148
      t377 = 0.1728D4 * t156 - 0.72D2 * t160 + t170 + 0.15552D5 * t184 -
     # t198 + t206 - 0.15696D5 * t212 + 0.15552D5 * t216 + 0.15552D5 * t
     #218 + 0.1872D4 * t228 - 0.864D3 * t231
      t385 = t237 + 0.576D3 * t240 - 0.72D2 * t245 - t252 + t259 + 0.288
     #D3 * t262 + t267 - 0.15696D5 * t271 - 0.936D3 * t276 + t287 - 0.17
     #28D4 * t291
      t420 = 0.2160D4 * t294 + 0.576D3 * t153 * t105 * t289 - 0.288D3 * 
     #t140 * t176 * t31 * t255 - 0.576D3 * t253 * t32 * t36 * t182 + 0.4
     #32D3 * t62 * t29 * t52 * t256 - t307 - 0.72D2 * t315 + 0.72D2 * t2
     #43 * t67 * t255 * t28 * x3 + 0.72D2 * t243 * t189 * t69 + 0.72D2 *
     # t194 * t67 * t255 * x2 * t28 + 0.72D2 * t194 * t189 * t42 + 0.288
     #D3 * t346
      rrqg2qgh64J4 = -(wd * (t149 + t220 + t283 + t348) + wd * (t368 + t
     #377 + t385 + t420)) / t1 / t31 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh64J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = s * t5
      t8 = z + x1 * t5
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t15 = x2 * x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + t15 + t24
      t28 = 0.1D1 - x1
      t29 = t28 * t11
      t31 = s - t6 * x1 * t9 * t25 - t6 * t29
      t32 = t5 ** 2
      t33 = t32 ** 2
      t35 = t4 * t31 * t33
      t36 = x1 ** 2
      t37 = t36 * x1
      t38 = t8 ** 2
      t39 = 0.1D1 / t38
      t40 = t37 * t39
      t42 = t25 * x2 * t28
      t45 = 0.288D3 * t35 * t40 * t42
      t46 = t32 * t5
      t47 = t3 * t46
      t51 = t18 * t8 + x2 * t11 - t24
      t52 = t51 * t31
      t56 = 0.1728D4 * t47 * t40 * t52 * z * t25
      t58 = t3 * t33 * t46
      t59 = t36 ** 2
      t61 = 0.1D1 / t38 / t8
      t62 = t59 * t61
      t64 = t28 ** 2
      t65 = t64 * t28
      t67 = x2 ** 2
      t72 = t33 * t32
      t73 = t3 * t72
      t74 = t73 * t62
      t75 = t51 ** 2
      t76 = t75 * t31
      t78 = x2 * t64 * x3
      t82 = t33 * t5
      t83 = t3 * t82
      t84 = t83 * t40
      t88 = t33 ** 2
      t91 = t38 ** 2
      t93 = t59 * x1 / t91
      t100 = x3 ** 2
      t101 = t64 * t100
      t110 = t4 * t31 * t82 * t37 * t67 * t64 * t39
      t112 = t3 * t33
      t113 = t11 ** 2
      t116 = t31 * t36
      t117 = t9 * t25
      t118 = t116 * t117
      t119 = t112 * t64 * t113 * t118
      t121 = t64 * t11
      t122 = t112 * t121
      t124 = t116 * x2 * t9
      t125 = t122 * t124
      t129 = t83 * t65 * t113 * t124
      t131 = t65 * t11
      t133 = t31 * t37
      t136 = t73 * t131 * t133 * t67 * t39
      t139 = t9 * t15
      t141 = t35 * t36 * t64 * t139
      t143 = t45 - t56 + 0.108D3 * t58 * t62 * t52 * t65 * x3 * t67 + 0.
     #144D3 * t74 * t76 * t78 - 0.576D3 * t84 * t52 * t78 + 0.36D2 * t3 
     #* t88 * t93 * t52 * t67 * x2 * t65 + 0.72D2 * t84 * t76 * t101 - 0
     #.144D3 * t110 - 0.14832D5 * t119 - 0.14976D5 * t125 + 0.144D3 * t1
     #29 - 0.144D3 * t136 - 0.288D3 * t141
      t145 = t4 * t31 * t46
      t148 = x3 * t9 * t25
      t151 = 0.288D3 * t145 * t36 * t28 * t148
      t156 = 0.288D3 * t145 * t36 * x2 * t28 * t9
      t158 = t47 * t29 * t118
      t160 = t36 * t9
      t168 = 0.288D3 * t4 * t31 * t5 * x1
      t169 = t2 * t1
      t170 = t169 * z
      t172 = t170 * t33 * t37
      t173 = t39 * t51
      t174 = x2 * t28
      t180 = t9 * t51
      t186 = t61 * t51
      t187 = t67 * t64
      t198 = t28 * x3
      t202 = t3 * t32
      t203 = t202 * t36
      t207 = 0.1728D4 * t203 * t180 * t31 * z
      t208 = t47 * t64
      t209 = t11 * t31
      t210 = x3 * x1
      t212 = t208 * t209 * t210
      t214 = t4 * t31
      t218 = 0.288D3 * t214 * t32 * x1 * t198
      t221 = t214 * t46 * x1 * t101
      t223 = t151 + t156 + 0.576D3 * t158 - 0.288D3 * t112 * t160 * t52 
     #* t101 - t168 - 0.1152D4 * t172 * t173 * t174 + 0.576D3 * t170 * t
     #33 * t36 * t180 * t101 + 0.576D3 * t170 * t72 * t59 * t186 * t187 
     #+ 0.1152D4 * t170 * t82 * t37 * t173 * t78 - 0.1152D4 * t170 * t46
     # * t36 * t180 * t198 + t207 + 0.14832D5 * t212 + t218 - 0.144D3 * 
     #t221
      t226 = t25 ** 2
      t227 = t39 * t226
      t230 = 0.288D3 * t214 * t46 * t37 * t227
      t231 = t112 * t65
      t234 = t231 * t209 * x1 * t100
      t241 = t112 * t59
      t243 = t31 * t25
      t245 = t241 * t61 * t75 * t243
      t247 = z ** 2
      t251 = 0.1728D4 * t203 * t180 * t31 * t247
      t257 = t113 * t31
      t259 = t231 * t257 * t210
      t261 = t83 * t62
      t263 = t261 * t52 * t42
      t265 = t112 * t40
      t267 = t265 * t52 * t174
      t272 = t198 * t25
      t283 = t83 * t131 * t116 * t139
      t285 = -t230 - 0.144D3 * t234 + 0.576D3 * t170 * t33 * t62 * t51 *
     # t226 + 0.1440D4 * t245 - t251 + 0.1152D4 * t170 * t46 * t40 * t51
     # * t25 - 0.144D3 * t259 + 0.144D3 * t263 + 0.144D3 * t267 - 0.288D
     #3 * t74 * t52 * t187 - 0.1152D4 * t172 * t173 * t272 - 0.1152D4 * 
     #t170 * t82 * t59 * t186 * t42 + 0.288D3 * t283
      t286 = t73 * t93
      t287 = t75 * t51
      t288 = t287 * t31
      t291 = 0.72D2 * t286 * t288 * t174
      t293 = t265 * t52 * t272
      t297 = t47 * t160 * t52 * t198
      t301 = 0.72D2 * t261 * t288 * t198
      t318 = t83 * t121 * t133 * t39 * t25 * x2
      t321 = t122 * t116 * t148
      t323 = t47 * t37
      t326 = t323 * t39 * t75 * t31
      t331 = 0.1440D4 * t241 * t61 * t287 * t31
      t333 = t160 * t51
      t337 = t208 * t257 * x1
      t343 = 0.576D3 * t169 * t247 * z * t32 * t333
      t348 = t291 + 0.144D3 * t293 + 0.144D3 * t297 + t301 + 0.36D2 * t8
     #3 * t160 * t52 * t65 * t100 * x3 + 0.108D3 * t73 * t40 * t52 * t65
     # * t100 * x2 + 0.14832D5 * t318 - 0.14976D5 * t321 - 0.1440D4 * t3
     #26 + t331 + 0.576D3 * t170 * t32 * t333 + 0.14832D5 * t337 + t343 
     #+ 0.72D2 * t58 * t93 * t76 * t187
      t366 = t45 + t56 + 0.288D3 * t110 + 0.15552D5 * t119 + 0.15552D5 *
     # t125 - 0.2160D4 * t129 - 0.936D3 * t136 + 0.576D3 * t141 + 0.72D2
     # * t261 * t76 * t272 + 0.72D2 * t286 * t52 * t226 * x2 * t28 + t15
     #1
      t382 = t156 + 0.72D2 * t286 * t76 * t42 - 0.864D3 * t158 + 0.432D3
     # * t112 * t29 * t133 * t227 + t168 + 0.72D2 * t261 * t52 * t226 * 
     #t28 * x3 - t207 - 0.15696D5 * t212 + t218 + 0.288D3 * t221 + t230
      t392 = -0.936D3 * t234 - 0.1728D4 * t245 + t251 + 0.2160D4 * t259 
     #- 0.72D2 * t263 - 0.72D2 * t267 + 0.1872D4 * t283 - t291 - 0.72D2 
     #* t293 - 0.72D2 * t297 - t301
      t420 = -0.576D3 * t214 * t32 * t36 * t117 - 0.288D3 * t241 * t186 
     #* t31 * t226 + 0.576D3 * t323 * t173 * t243 - 0.15696D5 * t318 + 0
     #.15552D5 * t321 + 0.1728D4 * t326 - t331 - 0.15552D5 * t337 - t343
     # + 0.3096D4 * t231 * t113 * t11 * t31 * x1 + 0.432D3 * t202 * t28 
     #* t209 * x1 - 0.288D3 * t203 * t180 * t31
      rrqg2qgh64J5 = -(wd * (t143 + t223 + t285 + t348) + wd * (t366 + t
     #382 + t392 + t420)) / t1 / t31 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh64J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = s * t5
      t8 = z + x1 * t5
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t15 = x2 * x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + t15 + t24
      t28 = 0.1D1 - x1
      t29 = t28 * t11
      t31 = s - t6 * x1 * t9 * t25 - t6 * t29
      t32 = t5 ** 2
      t33 = t32 * t5
      t35 = t4 * t31 * t33
      t36 = x1 ** 2
      t42 = t32 ** 2
      t44 = t4 * t31 * t42
      t45 = t28 ** 2
      t47 = t9 * t15
      t51 = t42 * t5
      t52 = t3 * t51
      t53 = t36 ** 2
      t54 = t8 ** 2
      t56 = 0.1D1 / t54 / t8
      t58 = t52 * t53 * t56
      t61 = t18 * t8 + x2 * t11 - t24
      t62 = t61 ** 2
      t63 = t62 * t61
      t64 = t63 * t31
      t65 = t28 * x3
      t71 = x3 * t9 * t25
      t75 = t61 * t31
      t76 = t25 ** 2
      t82 = t62 * t31
      t84 = t25 * t28 * x3
      t90 = t36 * x1
      t91 = x2 ** 2
      t93 = 0.1D1 / t54
      t98 = t45 * t11
      t100 = t31 * t90
      t106 = t45 * t28
      t107 = t106 * t11
      t109 = t31 * t36
      t113 = t3 * t33
      t115 = t9 * t25
      t116 = t109 * t115
      t119 = t3 * t32
      t120 = t119 * t36
      t121 = t9 * t61
      t122 = z ** 2
      t127 = 0.288D3 * t35 * t36 * x2 * t28 * t9 + 0.576D3 * t44 * t36 *
     # t45 * t47 - 0.72D2 * t58 * t64 * t65 + 0.288D3 * t35 * t36 * t28 
     #* t71 + 0.72D2 * t58 * t75 * t76 * t28 * x3 + 0.72D2 * t58 * t82 *
     # t84 + 0.288D3 * t4 * t31 * t51 * t90 * t91 * t45 * t93 - 0.15696D
     #5 * t52 * t98 * t100 * t93 * t25 * x2 + 0.1872D4 * t52 * t107 * t1
     #09 * t47 - 0.864D3 * t113 * t29 * t116 + 0.1728D4 * t120 * t121 * 
     #t31 * t122
      t128 = t3 * t42
      t129 = t128 * t106
      t130 = t11 ** 2
      t131 = t130 * t31
      t132 = x3 * x1
      t141 = t93 * t76
      t146 = t3 * t42 * t32
      t148 = t54 ** 2
      t151 = t146 * t53 * x1 / t148
      t157 = x2 * t28
      t158 = t157 * t25
      t162 = t90 * t93
      t163 = t128 * t162
      t167 = t36 * t9
      t175 = t128 * t98
      t180 = t109 * x2 * t9
      t187 = 0.2160D4 * t129 * t131 * t132 + 0.15552D5 * t128 * t45 * t1
     #30 * t116 + 0.432D3 * t128 * t29 * t100 * t141 + 0.72D2 * t151 * t
     #75 * t76 * x2 * t28 + 0.72D2 * t151 * t82 * t158 - 0.72D2 * t163 *
     # t75 * t84 - 0.72D2 * t113 * t167 * t75 * t65 - 0.72D2 * t151 * t6
     #4 * t157 + 0.15552D5 * t175 * t109 * t71 + 0.15552D5 * t175 * t180
     # - 0.2160D4 * t52 * t106 * t130 * t180
      t195 = t11 * t31
      t202 = t113 * t90
      t207 = t128 * t53
      t212 = t113 * t45
      t237 = 0.3096D4 * t129 * t130 * t11 * t31 * x1 + 0.432D3 * t119 * 
     #t28 * t195 * x1 - 0.288D3 * t120 * t121 * t31 + 0.1728D4 * t202 * 
     #t93 * t62 * t31 - 0.1440D4 * t207 * t56 * t63 * t31 - 0.15552D5 * 
     #t212 * t131 * x1 - 0.576D3 * t2 * t1 * t122 * z * t32 * t167 * t61
     # - 0.72D2 * t58 * t75 * t158 - 0.72D2 * t163 * t75 * t157 + 0.288D
     #3 * t44 * t162 * t158 + 0.1728D4 * t113 * t162 * t75 * z * t25
      t247 = t4 * t31
      t258 = t31 * t25
      t274 = x3 ** 2
      t291 = -0.936D3 * t146 * t107 * t100 * t91 * t93 + 0.288D3 * t4 * 
     #t31 * t5 * x1 - 0.576D3 * t247 * t32 * t36 * t115 - 0.288D3 * t207
     # * t56 * t61 * t31 * t76 + 0.576D3 * t202 * t93 * t61 * t258 - 0.1
     #728D4 * t120 * t121 * t31 * z - 0.15696D5 * t212 * t195 * t132 + 0
     #.288D3 * t247 * t32 * x1 * t65 + 0.288D3 * t247 * t33 * x1 * t45 *
     # t274 + 0.288D3 * t247 * t33 * t90 * t141 - 0.936D3 * t129 * t195 
     #* x1 * t274 - 0.1728D4 * t207 * t56 * t62 * t258
      rrqg2qgh64J6 = -wd * (t127 + t187 + t237 + t291) / t1 / t31 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
 