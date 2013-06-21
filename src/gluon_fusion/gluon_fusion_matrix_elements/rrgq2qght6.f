  
      subroutine rrgq2qght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh61J1  
      doubleprecision rrgq2qgh61J2  
      doubleprecision rrgq2qgh61J3  
      doubleprecision rrgq2qgh61J4  
      doubleprecision rrgq2qgh61J5  
      doubleprecision rrgq2qgh61J6  
      doubleprecision rrgq2qgh61J7  
      doubleprecision rrgq2qgh62J1  
      doubleprecision rrgq2qgh62J2  
      doubleprecision rrgq2qgh62J3  
      doubleprecision rrgq2qgh62J4  
      doubleprecision rrgq2qgh62J5  
      doubleprecision rrgq2qgh62J6  
      doubleprecision rrgq2qgh63J1  
      doubleprecision rrgq2qgh63J2  
      doubleprecision rrgq2qgh63J3  
      doubleprecision rrgq2qgh63J4  
      doubleprecision rrgq2qgh63J5  
      doubleprecision rrgq2qgh63J6  
      doubleprecision rrgq2qgh64J1  
      doubleprecision rrgq2qgh64J2  
      doubleprecision rrgq2qgh64J3  
      doubleprecision rrgq2qgh64J4  
      doubleprecision rrgq2qgh64J5  
      doubleprecision rrgq2qgh64J6  
      doubleprecision rrgq2qgh64J7  
      doubleprecision rrgq2qght6s1e1  
      doubleprecision rrgq2qght6s1e0  
      doubleprecision rrgq2qght6s1em1  
      doubleprecision rrgq2qght6s1em2  
      doubleprecision rrgq2qght6s1em3  
      doubleprecision rrgq2qght6s1em4  
      doubleprecision rrgq2qght6s2e1  
      doubleprecision rrgq2qght6s2e0  
      doubleprecision rrgq2qght6s2em1  
      doubleprecision rrgq2qght6s2em2  
      doubleprecision rrgq2qght6s2em3  
      doubleprecision rrgq2qght6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = 0.3141592653589793D1 * t3
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = rrgq2qgh63J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
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
      t37 = t4 * t36
      t39 = t25 * t24 * 0.3141592653589793D1
      t42 = t24 * 0.3141592653589793D1
      t45 = -0.180D3 * t33 + 0.30D2 * t30
      t46 = t3 * t45
      t49 = (0.90D2 * t26 * t27 + t37 + 0.15D2 * t39 * t3 - t42 * t46) *
     # t5
      t50 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t51 = t8 * t50
      t58 = t4 * t45
      t60 = (-0.180D3 * t42 * t27 - 0.45D2 * t26 * t3 + t58) * t5
      t61 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t62 = t8 * t61
      t65 = t4 * lh
      t70 = (0.180D3 * t65 + 0.90D2 * t42 * t3) * t5
      t71 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t72 = t8 * t71
      t82 = t30 ** 2
      t83 = t33 ** 2
      t89 = t25 ** 2
      t94 = (-0.30D2 * t39 * t27 + t26 * t46 / 0.2D1 - t42 * t3 * t36 + 
     #t4 * (-0.5769873135166051D3 * lh - t82 - 0.60D2 * t83 + 0.60D2 * t
     #33 * t30) - 0.15D2 / 0.4D1 * t89 * 0.3141592653589793D1 * t3) * t5
      t95 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t96 = t8 * t95
      t99 = 0.3141592653589793D1 * t5
      t100 = x1 ** 2
      t101 = x3 * t100
      t102 = t101 * t18
      t103 = t15 * t21
      t104 = -0.1D1 + x3
      t105 = 0.1D1 / t104
      t106 = t103 * t105
      t109 = log(-0.4D1 * t102 * t106)
      t111 = t109 ** 2
      t115 = cos(t16)
      t117 = Sqrt(-x3 * t104)
      t122 = 0.1D1 / (-z - x3 + 0.2D1 * t115 * t117 * z)
      t124 = t3 * t61
      t127 = log(0.4D1 * t101 * t22)
      t128 = t127 * t3
      t130 = t127 ** 2
      t131 = t130 * t3
      t138 = 0.3141592653589793D1 * lh
      t142 = t3 * t50
      t148 = 0.3141592653589793D1 * t45
      t152 = t9 * (t3 * t95 + t95 * t122)
      t153 = t148 * t152
      t155 = 0.1D1 / x3
      t157 = 0.1D1 / x1
      t160 = t100 * t18
      t163 = log(0.4D1 * t160 * t103)
      t168 = t163 ** 2
      t171 = t168 * t163
      t179 = t9 * t95
      t180 = t37 * t179
      t191 = x2 ** 2
      t192 = x3 * t191
      t193 = t192 * t100
      t196 = log(0.4D1 * t193 * t22)
      t197 = t196 * t3
      t203 = log(-0.4D1 * t193 * t19 * t21 * t105)
      t215 = 0.1D1 / x2
      t216 = t215 * t157
      t219 = t191 * t100
      t222 = log(0.4D1 * t219 * t22)
      t223 = t222 ** 2
      t224 = t223 * t3
      t227 = t222 * t3
      t238 = t148 * t5
      t239 = t8 * t3
      t248 = t9 * t50
      t253 = x3 * t18
      t256 = log(0.4D1 * t253 * t103)
      t260 = log(-0.4D1 * t253 * t106)
      t262 = t256 * t3 + t260 * t122
      t264 = t260 ** 2
      t267 = t256 ** 2
      t271 = t264 * t260 * t122 / 0.6D1 + t267 * t256 * t3 / 0.6D1
      t278 = 0.3141592653589793D1 * t36
      t284 = -t3 - t122
      t294 = -t267 * t3 / 0.2D1 - t264 * t122 / 0.2D1
      t299 = t192 * t18
      t302 = log(-0.4D1 * t299 * t106)
      t304 = t302 ** 2
      t311 = log(0.4D1 * t192 * t22)
      t312 = t311 * t3
      t314 = t311 ** 2
      t315 = t314 * t3
      t334 = t191 * t18
      t337 = log(0.4D1 * t334 * t103)
      t338 = t337 * t3
      t343 = t337 ** 2
      t344 = t343 * t3
      t349 = t343 * t337 * t3
      t367 = t4 * t9 * t10 / 0.32D2 - t49 * t51 / 0.2880D4 - t60 * t62 /
     # 0.2880D4 - t70 * t72 / 0.2880D4 - t94 * t96 / 0.2880D4 - (-0.90D2
     # * t99 * t8 * (-(-t61 + t109 * t50 - t111 * t95 / 0.2D1) * t122 + 
     #t124 - t128 * t50 + t131 * t95 / 0.2D1) + 0.180D3 * t138 * t9 * (-
     #(-t50 + t109 * t95) * t122 + t142 - t128 * t95) + t153) * t155 * t
     #157 / 0.1440D4 - (t58 * t9 * (-t163 * t95 + t50) - 0.90D2 * t4 * t
     #9 * (t71 + t168 * t50 / 0.2D1 - t171 * t95 / 0.6D1 - t163 * t61) +
     # t180 + 0.180D3 * t65 * t9 * (-t163 * t50 + t168 * t95 / 0.2D1 + t
     #61)) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 * (-t197 * t95 + t142
     # - (-t50 + t203 * t95) * t122) + 0.180D3 * t138 * t152) * t155 * t
     #216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t124 + t224 * t95 / 0.2D1 -
     # t227 * t50) + 0.180D3 * t138 * t9 * (-t227 * t95 + t142) + t238 *
     # t239 * t95) * t215 * t157 / 0.720D3 - ((0.90D2 * t99 * t62 - 0.18
     #0D3 * t138 * t248 - t148 * t179) * t262 + 0.90D2 * t99 * t96 * t27
     #1 + (-t148 * t248 + 0.90D2 * t99 * t72 - t278 * t179 - 0.180D3 * t
     #138 * t9 * t61) * t284 + (0.90D2 * t99 * t51 - 0.180D3 * t138 * t1
     #79) * t294) * t155 / 0.2880D4 - (-0.90D2 * t99 * t8 * (-(-t61 + t3
     #02 * t50 - t304 * t95 / 0.2D1) * t122 + t124 - t312 * t50 + t315 *
     # t95 / 0.2D1) + 0.180D3 * t138 * t9 * (-(-t50 + t302 * t95) * t122
     # + t142 - t312 * t95) + t153) * t155 * t215 / 0.1440D4 + (t148 * t
     #9 * (t338 * t95 - t142) - 0.90D2 * t99 * t8 * (-t344 * t50 / 0.2D1
     # - t3 * t71 + t349 * t95 / 0.6D1 + t338 * t61) - t180 + 0.180D3 * 
     #t138 * t9 * (-t124 - t344 * t95 / 0.2D1 + t338 * t50)) * t215 / 0.
     #1440D4
      t368 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t367)
      t370 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t371 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t373 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t378 = t3 * t370
      t389 = t3 * t371
      t398 = t9 * (t373 * t122 + t3 * t373)
      t399 = t148 * t398
      t410 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t419 = t278 * t5
      t420 = t239 * t373
      t421 = t419 * t420
      t432 = rrgq2qgh61J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t436 = t8 * t371
      t439 = t8 * t370
      t442 = t8 * t410
      t445 = t8 * t373
      t450 = t9 * t371
      t453 = t9 * t373
      t556 = -(-0.90D2 * t99 * t8 * (-(-t370 + t302 * t371 - t304 * t373
     # / 0.2D1) * t122 + t378 - t312 * t371 + t315 * t373 / 0.2D1) + 0.1
     #80D3 * t138 * t9 * (-(-t371 + t302 * t373) * t122 + t389 - t312 * 
     #t373) + t399) * t155 * t215 / 0.1440D4 + (t148 * t9 * (t338 * t373
     # - t389) - 0.90D2 * t99 * t8 * (-t344 * t371 / 0.2D1 - t3 * t410 +
     # t349 * t373 / 0.6D1 + t338 * t370) - t421 + 0.180D3 * t138 * t9 *
     # (-t378 - t344 * t373 / 0.2D1 + t338 * t371)) * t215 / 0.1440D4 + 
     #t4 * t9 * t432 / 0.32D2 - t49 * t436 / 0.2880D4 - t60 * t439 / 0.2
     #880D4 - t70 * t442 / 0.2880D4 - t94 * t445 / 0.2880D4 - ((0.90D2 *
     # t99 * t439 - 0.180D3 * t138 * t450 - t148 * t453) * t262 + 0.90D2
     # * t99 * t445 * t271 + (-t148 * t450 + 0.90D2 * t99 * t442 - t278 
     #* t453 - 0.180D3 * t138 * t9 * t370) * t284 + (0.90D2 * t99 * t436
     # - 0.180D3 * t138 * t453) * t294) * t155 / 0.2880D4 - (-0.90D2 * t
     #99 * t8 * (-(-t370 + t109 * t371 - t111 * t373 / 0.2D1) * t122 + t
     #378 - t128 * t371 + t131 * t373 / 0.2D1) + 0.180D3 * t138 * t9 * (
     #-(-t371 + t109 * t373) * t122 + t389 - t128 * t373) + t399) * t155
     # * t157 / 0.1440D4 - (t58 * t9 * (t371 - t163 * t373) - 0.90D2 * t
     #4 * t9 * (t168 * t371 / 0.2D1 - t171 * t373 / 0.6D1 + t410 - t163 
     #* t370) + t421 + 0.180D3 * t65 * t9 * (t370 - t163 * t371 + t168 *
     # t373 / 0.2D1)) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 * (-t197 *
     # t373 - (-t371 + t203 * t373) * t122 + t389) + 0.180D3 * t138 * t3
     #98) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t378 + t224 *
     # t373 / 0.2D1 - t227 * t371) + 0.180D3 * t138 * t9 * (-t227 * t373
     # + t389) + t238 * t420) * t215 * t157 / 0.720D3
      t557 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t556)
      t559 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t561 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t564 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t567 = t3 * t564
      t578 = t3 * t559
      t587 = t9 * (t561 * t122 + t3 * t561)
      t588 = t148 * t587
      t597 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t607 = t9 * t561
      t608 = t37 * t607
      t699 = rrgq2qgh64J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t703 = t8 * t559
      t706 = t8 * t564
      t709 = t8 * t597
      t714 = t9 * t559
      t720 = t8 * t561
      t744 = -(-0.90D2 * t99 * t8 * (-(t109 * t559 - t111 * t561 / 0.2D1
     # - t564) * t122 + t567 - t128 * t559 + t131 * t561 / 0.2D1) + 0.18
     #0D3 * t138 * t9 * (-(-t559 + t109 * t561) * t122 + t578 - t128 * t
     #561) + t588) * t155 * t157 / 0.1440D4 - (t58 * t9 * (-t163 * t561 
     #+ t559) - 0.90D2 * t4 * t9 * (t597 + t168 * t559 / 0.2D1 - t163 * 
     #t564 - t171 * t561 / 0.6D1) + t608 + 0.180D3 * t65 * t9 * (t168 * 
     #t561 / 0.2D1 - t163 * t559 + t564)) * t157 / 0.1440D4 - (-0.90D2 *
     # t99 * t8 * (-t197 * t561 + t578 - (-t559 + t203 * t561) * t122) +
     # 0.180D3 * t138 * t587) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 *
     # t8 * (t567 - t227 * t559 + t224 * t561 / 0.2D1) + 0.180D3 * t138 
     #* t9 * (t578 - t227 * t561) + t238 * t239 * t561) * t215 * t157 / 
     #0.720D3 - (-0.90D2 * t99 * t8 * (-(t302 * t559 - t304 * t561 / 0.2
     #D1 - t564) * t122 + t567 - t312 * t559 + t315 * t561 / 0.2D1) + 0.
     #180D3 * t138 * t9 * (-(-t559 + t302 * t561) * t122 + t578 - t312 *
     # t561) + t588) * t155 * t215 / 0.1440D4 + (t148 * t9 * (-t578 + t3
     #38 * t561) - 0.90D2 * t99 * t8 * (-t344 * t559 / 0.2D1 - t3 * t597
     # + t349 * t561 / 0.6D1 + t338 * t564) - t608 + 0.180D3 * t138 * t9
     # * (-t567 + t338 * t559 - t344 * t561 / 0.2D1)) * t215 / 0.1440D4 
     #+ t4 * t9 * t699 / 0.32D2 - t49 * t703 / 0.2880D4 - t60 * t706 / 0
     #.2880D4 - t70 * t709 / 0.2880D4 - ((0.90D2 * t99 * t706 - 0.180D3 
     #* t138 * t714 - t148 * t607) * t262 + 0.90D2 * t99 * t720 * t271 +
     # (-t148 * t714 + 0.90D2 * t99 * t709 - t278 * t607 - 0.180D3 * t13
     #8 * t9 * t564) * t284 + (0.90D2 * t99 * t703 - 0.180D3 * t138 * t6
     #07) * t294) * t155 / 0.2880D4 - t94 * t720 / 0.2880D4
      t745 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t744)
      t747 = t2 * x1
      t748 = -0.1D1 + x1
      t749 = x1 * z
      t750 = 0.1D1 - x1 + t749
      t751 = 0.1D1 / t750
      t753 = t2 * t748 * t751
      t754 = s * t20
      t756 = x1 * t748 * t751
      t757 = t754 * t756
      t758 = -t748
      t759 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.10
     #D1, x4)
      t761 = t21 * t751
      t762 = t748 ** 2
      t767 = log(-0.4D1 * t101 * t19 * t761 * t762 * t105)
      t768 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.10
     #D1, x4)
      t770 = t767 ** 2
      t771 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.10
     #D1, x4)
      t776 = x3 * x1
      t777 = 0.2D1 * t776
      t778 = x3 * t750
      t780 = Sqrt(-t778 * t104)
      t784 = t776 * z
      t785 = 0.3D1 * t784
      t786 = x1 * t14
      t787 = x3 * t14
      t788 = t787 * x1
      t790 = 0.2D1 * t101 * z
      t791 = t101 * t14
      t792 = -z + t777 - t101 + 0.2D1 * t115 * t780 * z + t749 - t785 - 
     #t786 + t788 + t790 - t791 - x3
      t793 = 0.1D1 / t792
      t795 = t3 * t759
      t796 = t751 * t762
      t797 = t103 * t796
      t800 = log(0.4D1 * t102 * t797)
      t801 = t800 * t3
      t803 = t800 ** 2
      t804 = t803 * t3
      t816 = t3 * t768
      t825 = t9 * (-t3 * t771 - t771 * t750 * t793)
      t831 = t160 * t15
      t832 = t761 * t762
      t835 = log(0.4D1 * t831 * t832)
      t840 = t835 ** 2
      t843 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.10
     #D1, x4)
      t844 = t840 * t835
      t864 = t192 * t160
      t867 = log(0.4D1 * t864 * t797)
      t868 = t867 * t3
      t874 = log(-0.4D1 * t864 * t103 * t796 * t105)
      t889 = t219 * t18
      t892 = log(0.4D1 * t889 * t797)
      t893 = t892 ** 2
      t894 = t893 * t3
      t897 = t892 * t3
      t914 = -(-0.90D2 * t99 * t8 * (-(t759 - t767 * t768 + t770 * t771 
     #/ 0.2D1) * t750 * t793 - t795 + t801 * t768 - t804 * t771 / 0.2D1)
     # + 0.180D3 * t138 * t9 * (-(t768 - t767 * t771) * t750 * t793 + t8
     #01 * t771 - t816) + t148 * t825) * t155 * t157 / 0.1440D4 - (t58 *
     # t9 * (-t768 + t835 * t771) - 0.90D2 * t4 * t9 * (-t840 * t768 / 0
     #.2D1 - t843 + t844 * t771 / 0.6D1 + t835 * t759) - t37 * t9 * t771
     # + 0.180D3 * t65 * t9 * (t835 * t768 - t759 - t840 * t771 / 0.2D1)
     #) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 * (t868 * t771 - (t768 -
     # t874 * t771) * t750 * t793 - t816) + 0.180D3 * t138 * t825) * t15
     #5 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (-t795 - t894 * t771 / 
     #0.2D1 + t897 * t768) + 0.180D3 * t138 * t9 * (-t816 + t897 * t771)
     # - t238 * t239 * t771) * t215 * t157 / 0.720D3
      t915 = FJET(XB1, XB2, s, 0.0D0, t747, -t753, 0.0D0, -t757, t914)
      t917 = x2 * s
      t918 = t917 * t1
      t919 = -0.1D1 + x2
      t920 = t919 * s
      t921 = t920 * t1
      t922 = x2 * z
      t924 = 0.1D1 / (-x2 + t922 - z)
      t925 = -t919
      t926 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.10
     #D1, x4)
      t927 = t924 * t926
      t928 = t103 * t919
      t931 = log(-0.4D1 * t299 * t928)
      t932 = t931 * t924
      t933 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.10
     #D1, x4)
      t935 = t931 ** 2
      t936 = t935 * t924
      t937 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.10
     #D1, x4)
      t945 = t924 * t933
      t950 = t8 * t924
      t951 = t950 * t937
      t952 = t238 * t951
      t959 = log(-0.4D1 * t334 * t928)
      t960 = t959 * t924
      t965 = t959 ** 2
      t966 = t965 * t924
      t969 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.10
     #D1, x4)
      t972 = t965 * t959 * t924
      t991 = t21 * t919
      t995 = log(-0.4D1 * t193 * t19 * t991)
      t996 = t995 * t924
      t1002 = t138 * t5
      t1011 = log(-0.4D1 * t889 * t928)
      t1012 = t1011 * t924
      t1014 = t1011 ** 2
      t1015 = t1014 * t924
      t1031 = -(-0.90D2 * t99 * t8 * (t927 - t932 * t933 + t936 * t937 /
     # 0.2D1) + 0.180D3 * t138 * t9 * (-t932 * t937 + t945) + t952) * t1
     #55 * t215 / 0.1440D4 + (t148 * t9 * (-t945 + t960 * t937) - 0.90D2
     # * t99 * t8 * (-t966 * t933 / 0.2D1 - t924 * t969 + t972 * t937 / 
     #0.6D1 + t960 * t926) - t419 * t951 + 0.180D3 * t138 * t9 * (-t927 
     #+ t960 * t933 - t966 * t937 / 0.2D1)) * t215 / 0.1440D4 - (-0.90D2
     # * t99 * t8 * (t945 - t996 * t937) + 0.180D3 * t1002 * t951) * t15
     #5 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (-t1012 * t933 + t1015 
     #* t937 / 0.2D1 + t927) + 0.180D3 * t138 * t9 * (-t1012 * t937 + t9
     #45) + t952) * t215 * t157 / 0.720D3
      t1032 = FJET(XB1, XB2, s, 0.0D0, t918, 0.0D0, -t921, 0.0D0, t1031)
      t1034 = x2 * x3
      t1037 = Sqrt(x3 * t919 * t104)
      t1038 = t115 * t1037
      t1040 = 0.2D1 * t1038 * x2
      t1042 = 0.1D1 - x3 + t1034
      t1043 = 0.1D1 / t1042
      t1045 = t2 * (0.1D1 - x3 - x2 + t1034 + t192 + t1040) * t1043
      t1050 = t2 * x2 * (-0.1D1 + t1034 + 0.2D1 * t1038) * t1043
      t1051 = t1034 * z
      t1052 = t192 * z
      t1058 = 0.1D1 / (x2 - t922 + z - t1051 + t1052 - t192 + x3 - 0.2D1
     # * t1038 * z + 0.2D1 * t1038 * t922 - t1040)
      t1059 = t104 * t1043
      t1060 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1061 = t1058 * t1060
      t1062 = t919 * t104
      t1063 = t1042 ** 2
      t1064 = 0.1D1 / t1063
      t1065 = t1062 * t1064
      t1069 = log(0.4D1 * t864 * t103 * t1065)
      t1070 = t1069 * t1058
      t1071 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1077 = t8 * t1058
      t1078 = t1077 * t1071
      t1085 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1092 = log(0.4D1 * t192 * t19 * t991 * t104 * t1064)
      t1093 = t1092 ** 2
      t1094 = t1093 * t1058
      t1097 = t1092 * t1058
      t1113 = -(-0.90D2 * t99 * t8 * (t1061 - t1070 * t1071) + 0.180D3 *
     # t1002 * t1078) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t
     #1058 * t1085 + t1094 * t1071 / 0.2D1 - t1097 * t1060) + 0.180D3 * 
     #t138 * t9 * (t1061 - t1097 * t1071) + t238 * t1078) * t155 * t215 
     #/ 0.1440D4
      t1114 = FJET(XB1, XB2, s, 0.0D0, t1045, 0.0D0, -t1050, 0.0D0, t111
     #3)
      t1116 = t1 * t748
      t1118 = t920 * t1116 * t751
      t1119 = t917 * t1116
      t1121 = t754 * t919 * t756
      t1122 = x2 * x1
      t1123 = t1122 * z
      t1125 = 0.1D1 / (-t1122 + z - t922 + x2 + t1123)
      t1126 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1127 = t1125 * t1126
      t1132 = log(-0.4D1 * t864 * t103 * t796 * t919)
      t1133 = t1132 * t1125
      t1134 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1140 = t8 * t1125
      t1141 = t1140 * t1134
      t1147 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1154 = log(-0.4D1 * t219 * t19 * t761 * t762 * t919)
      t1155 = t1154 * t1125
      t1157 = t1154 ** 2
      t1158 = t1157 * t1125
      t1175 = -(-0.90D2 * t99 * t8 * (t1127 - t1133 * t1134) + 0.180D3 *
     # t1002 * t1141) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t
     #1125 * t1147 - t1155 * t1126 + t1158 * t1134 / 0.2D1) + 0.180D3 * 
     #t138 * t9 * (t1127 - t1155 * t1134) + t238 * t1141) * t215 * t157 
     #/ 0.720D3
      t1176 = FJET(XB1, XB2, s, 0.0D0, t1118, t747, -t1119, t1121, t1175
     #)
      t1178 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1180 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1181 = t924 * t1180
      t1186 = t950 * t1178
      t1196 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1197 = t924 * t1196
      t1207 = t238 * t1186
      t1234 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1254 = -(-0.90D2 * t99 * t8 * (-t996 * t1178 + t1181) + 0.180D3 *
     # t1002 * t1186) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t
     #1015 * t1178 / 0.2D1 - t1012 * t1180 + t1197) + 0.180D3 * t138 * t
     #9 * (t1181 - t1012 * t1178) + t1207) * t215 * t157 / 0.720D3 - (-0
     #.90D2 * t99 * t8 * (-t932 * t1180 + t1197 + t936 * t1178 / 0.2D1) 
     #+ 0.180D3 * t138 * t9 * (t1181 - t932 * t1178) + t1207) * t155 * t
     #215 / 0.1440D4 + (t148 * t9 * (-t1181 + t960 * t1178) - 0.90D2 * t
     #99 * t8 * (-t966 * t1180 / 0.2D1 - t924 * t1234 + t960 * t1196 + t
     #972 * t1178 / 0.6D1) - t419 * t1186 + 0.180D3 * t138 * t9 * (-t966
     # * t1178 / 0.2D1 + t960 * t1180 - t1197)) * t215 / 0.1440D4
      t1255 = FJET(XB1, XB2, s, 0.0D0, -t921, 0.0D0, t918, 0.0D0, t1254)
      t1257 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1258 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1260 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1266 = t3 * t1257
      t1279 = t3 * t1258
      t1288 = t9 * (-t3 * t1260 - t1260 * t750 * t793)
      t1300 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1353 = -(-0.90D2 * t99 * t8 * (-(t1257 - t767 * t1258 + t770 * t1
     #260 / 0.2D1) * t750 * t793 - t1266 - t804 * t1260 / 0.2D1 + t801 *
     # t1258) + 0.180D3 * t138 * t9 * (-(t1258 - t767 * t1260) * t750 * 
     #t793 + t801 * t1260 - t1279) + t148 * t1288) * t155 * t157 / 0.144
     #0D4 - (t58 * t9 * (-t1258 + t835 * t1260) - 0.90D2 * t4 * t9 * (-t
     #840 * t1258 / 0.2D1 - t1300 + t835 * t1257 + t844 * t1260 / 0.6D1)
     # - t37 * t9 * t1260 + 0.180D3 * t65 * t9 * (-t1257 + t835 * t1258 
     #- t840 * t1260 / 0.2D1)) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 *
     # (-t1279 - (t1258 - t874 * t1260) * t750 * t793 + t868 * t1260) + 
     #0.180D3 * t138 * t1288) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 *
     # t8 * (t897 * t1258 - t894 * t1260 / 0.2D1 - t1266) + 0.180D3 * t1
     #38 * t9 * (-t1279 + t897 * t1260) - t238 * t239 * t1260) * t215 * 
     #t157 / 0.720D3
      t1354 = FJET(XB1, XB2, s, 0.0D0, -t753, t747, 0.0D0, -t757, t1353)
      t1356 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1358 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1361 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1367 = t1058 * t1361
      t1373 = t1077 * t1358
      t1390 = -(-0.90D2 * t99 * t8 * (t1058 * t1356 + t1094 * t1358 / 0.
     #2D1 - t1097 * t1361) + 0.180D3 * t138 * t9 * (t1367 - t1097 * t135
     #8) + t238 * t1373) * t155 * t215 / 0.1440D4 - (-0.90D2 * t99 * t8 
     #* (t1367 - t1070 * t1358) + 0.180D3 * t1002 * t1373) * t155 * t216
     # / 0.720D3
      t1391 = FJET(XB1, XB2, s, 0.0D0, -t1050, 0.0D0, t1045, 0.0D0, t139
     #0)
      t1393 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1394 = t8 * t1393
      t1397 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1398 = t9 * t1397
      t1401 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1402 = t9 * t1401
      t1406 = t8 * t1401
      t1411 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1412 = t8 * t1411
      t1421 = t8 * t1397
      t1433 = t3 * t1393
      t1449 = t3 * t1397
      t1458 = t9 * (t1401 * t122 + t3 * t1401)
      t1459 = t148 * t1458
      t1477 = t37 * t1402
      t1572 = rrgq2qgh62J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1578 = -((0.90D2 * t99 * t1394 - 0.180D3 * t138 * t1398 - t148 * 
     #t1402) * t262 + 0.90D2 * t99 * t1406 * t271 + (-t148 * t1398 + 0.9
     #0D2 * t99 * t1412 - t278 * t1402 - 0.180D3 * t138 * t9 * t1393) * 
     #t284 + (0.90D2 * t99 * t1421 - 0.180D3 * t138 * t1402) * t294) * t
     #155 / 0.2880D4 - t94 * t1406 / 0.2880D4 - (-0.90D2 * t99 * t8 * (t
     #1433 + t131 * t1401 / 0.2D1 - t128 * t1397 - (t109 * t1397 - t111 
     #* t1401 / 0.2D1 - t1393) * t122) + 0.180D3 * t138 * t9 * (-(-t1397
     # + t109 * t1401) * t122 + t1449 - t128 * t1401) + t1459) * t155 * 
     #t157 / 0.1440D4 - (t58 * t9 * (t1397 - t163 * t1401) - 0.90D2 * t4
     # * t9 * (t168 * t1397 / 0.2D1 - t163 * t1393 + t1411 - t171 * t140
     #1 / 0.6D1) + t1477 + 0.180D3 * t65 * t9 * (t1393 + t168 * t1401 / 
     #0.2D1 - t163 * t1397)) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 * (
     #-(-t1397 + t203 * t1401) * t122 + t1449 - t197 * t1401) + 0.180D3 
     #* t138 * t1458) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t
     #224 * t1401 / 0.2D1 - t227 * t1397 + t1433) + 0.180D3 * t138 * t9 
     #* (t1449 - t227 * t1401) + t238 * t239 * t1401) * t215 * t157 / 0.
     #720D3 - t49 * t1421 / 0.2880D4 - t60 * t1394 / 0.2880D4 - (-0.90D2
     # * t99 * t8 * (t315 * t1401 / 0.2D1 + t1433 - t312 * t1397 - (t302
     # * t1397 - t304 * t1401 / 0.2D1 - t1393) * t122) + 0.180D3 * t138 
     #* t9 * (-t312 * t1401 - (-t1397 + t302 * t1401) * t122 + t1449) + 
     #t1459) * t155 * t215 / 0.1440D4 + (t148 * t9 * (-t1449 + t338 * t1
     #401) - 0.90D2 * t99 * t8 * (-t3 * t1411 + t338 * t1393 + t349 * t1
     #401 / 0.6D1 - t344 * t1397 / 0.2D1) - t1477 + 0.180D3 * t138 * t9 
     #* (-t344 * t1401 / 0.2D1 - t1433 + t338 * t1397)) * t215 / 0.1440D
     #4 + t4 * t9 * t1572 / 0.32D2 - t70 * t1412 / 0.2880D4
      t1579 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1578)
      t1581 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1583 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1584 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1591 = t3 * t1583
      t1603 = t3 * t1581
      t1612 = t9 * (-t3 * t1584 - t1584 * t750 * t793)
      t1626 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t1677 = -(-0.90D2 * t99 * t8 * (-(-t767 * t1581 + t1583 + t770 * t
     #1584 / 0.2D1) * t750 * t793 + t801 * t1581 - t1591 - t804 * t1584 
     #/ 0.2D1) + 0.180D3 * t138 * t9 * (-(t1581 - t767 * t1584) * t750 *
     # t793 + t801 * t1584 - t1603) + t148 * t1612) * t155 * t157 / 0.14
     #40D4 - (t58 * t9 * (t835 * t1584 - t1581) - 0.90D2 * t4 * t9 * (-t
     #840 * t1581 / 0.2D1 + t844 * t1584 / 0.6D1 - t1626 + t835 * t1583)
     # - t37 * t9 * t1584 + 0.180D3 * t65 * t9 * (-t840 * t1584 / 0.2D1 
     #- t1583 + t835 * t1581)) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 *
     # (t868 * t1584 - t1603 - (t1581 - t874 * t1584) * t750 * t793) + 0
     #.180D3 * t138 * t1612) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * 
     #t8 * (-t1591 + t897 * t1581 - t894 * t1584 / 0.2D1) + 0.180D3 * t1
     #38 * t9 * (-t1603 + t897 * t1584) - t238 * t239 * t1584) * t215 * 
     #t157 / 0.720D3
      t1678 = FJET(XB1, XB2, s, t747, 0.0D0, 0.0D0, -t753, -t757, t1677)
      t1680 = t368 * t367 + t557 * t556 + t745 * t744 + t915 * t914 + t1
     #032 * t1031 + t1114 * t1113 + t1176 * t1175 + t1255 * t1254 + t135
     #4 * t1353 + t1391 * t1390 + t1579 * t1578 + t1678 * t1677
      t1681 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1683 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1684 = t1125 * t1683
      t1689 = t1140 * t1681
      t1695 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1714 = -(-0.90D2 * t99 * t8 * (-t1133 * t1681 + t1684) + 0.180D3 
     #* t1002 * t1689) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (
     #t1125 * t1695 - t1155 * t1683 + t1158 * t1681 / 0.2D1) + 0.180D3 *
     # t138 * t9 * (-t1155 * t1681 + t1684) + t238 * t1689) * t215 * t15
     #7 / 0.720D3
      t1715 = FJET(XB1, XB2, s, t747, -t1119, 0.0D0, t1118, t1121, t1714
     #)
      t1717 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1719 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1720 = t924 * t1719
      t1725 = t950 * t1717
      t1733 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1734 = t924 * t1733
      t1746 = t238 * t1725
      t1775 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1793 = -(-0.90D2 * t99 * t8 * (-t996 * t1717 + t1720) + 0.180D3 *
     # t1002 * t1725) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (-
     #t1012 * t1719 + t1734 + t1015 * t1717 / 0.2D1) + 0.180D3 * t138 * 
     #t9 * (t1720 - t1012 * t1717) + t1746) * t215 * t157 / 0.720D3 - (-
     #0.90D2 * t99 * t8 * (t936 * t1717 / 0.2D1 + t1734 - t932 * t1719) 
     #+ 0.180D3 * t138 * t9 * (t1720 - t932 * t1717) + t1746) * t155 * t
     #215 / 0.1440D4 + (t148 * t9 * (-t1720 + t960 * t1717) - 0.90D2 * t
     #99 * t8 * (-t966 * t1719 / 0.2D1 + t972 * t1717 / 0.6D1 - t924 * t
     #1775 + t960 * t1733) - t419 * t1725 + 0.180D3 * t138 * t9 * (-t966
     # * t1717 / 0.2D1 + t960 * t1719 - t1734)) * t215 / 0.1440D4
      t1794 = FJET(XB1, XB2, s, t918, 0.0D0, -t921, 0.0D0, 0.0D0, t1793)
      t1796 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1798 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1799 = t1058 * t1798
      t1804 = t1077 * t1796
      t1811 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t1830 = -(-0.90D2 * t99 * t8 * (-t1070 * t1796 + t1799) + 0.180D3 
     #* t1002 * t1804) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (
     #t1058 * t1811 - t1097 * t1798 + t1094 * t1796 / 0.2D1) + 0.180D3 *
     # t138 * t9 * (t1799 - t1097 * t1796) + t238 * t1804) * t155 * t215
     # / 0.1440D4
      t1831 = FJET(XB1, XB2, s, t1045, 0.0D0, -t1050, 0.0D0, 0.0D0, t183
     #0)
      t1833 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1835 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1836 = t1125 * t1835
      t1841 = t1140 * t1833
      t1850 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t1866 = -(-0.90D2 * t99 * t8 * (-t1133 * t1833 + t1836) + 0.180D3 
     #* t1002 * t1841) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (
     #t1158 * t1833 / 0.2D1 - t1155 * t1835 + t1125 * t1850) + 0.180D3 *
     # t138 * t9 * (-t1155 * t1833 + t1836) + t238 * t1841) * t215 * t15
     #7 / 0.720D3
      t1867 = FJET(XB1, XB2, s, t1118, 0.0D0, -t1119, t747, t1121, t1866
     #)
      t1870 = t747 * t1034 * t1043
      t1871 = t2 * t748
      t1873 = Sqrt(t778 * t1062)
      t1874 = t115 * t1873
      t1876 = 0.2D1 * t1874 * x2
      t1877 = t192 * t749
      t1878 = t192 * x1
      t1882 = t1871 * (t192 - x2 + t1034 + t1876 + t1877 - t1878 + 0.1D1
     # - x3) * t751 * t1043
      t1886 = t104 * s * t1 * x1 * t1043
      t1892 = t1871 * x2 * (-0.1D1 + t1034 + x1 - t776 - t749 + t784 + 0
     #.2D1 * t1874) * t751 * t1043
      t1897 = log(0.4D1 * t192 * t831 * t832 * t1065)
      t1898 = t1897 * t750
      t1901 = x2 * t100
      t1911 = 0.2D1 * t1874 * t1123 - t1901 - z - 0.3D1 * t1123 - x3 + t
     #101 * x2 + t1122 * t14 + 0.2D1 * t1901 * z - t1901 * t14 + t1876 -
     # t1878 + 0.2D1 * t1874 * z - t1034 * x1 - t785 + t788 + t790 - t79
     #1
      t1924 = t922 + t749 + t777 - t101 - t786 + t1051 - t1052 + t1877 -
     # 0.2D1 * t1874 * t1122 - 0.2D1 * t1874 * t922 + 0.2D1 * t1034 * t7
     #49 - t787 * t1122 - 0.2D1 * t101 * t922 + t101 * t14 * x2 - x2 + 0
     #.2D1 * t1122 + t192
      t1926 = 0.1D1 / (t1911 + t1924)
      t1927 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t1928 = t1926 * t1927
      t1930 = t750 * t1926
      t1931 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t1937 = t8 * t750
      t1941 = -0.90D2 * t99 * t8 * (-t1898 * t1928 + t1930 * t1931) + 0.
     #180D3 * t1002 * t1937 * t1928
      t1945 = FJET(XB1, XB2, s, t1870, -t1882, -t1886, t1892, t1121, -t1
     #941 * t155 * t216 / 0.720D3)
      t1948 = t155 * t215 * t157
      t1951 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t1952 = t1926 * t1951
      t1954 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t1963 = -0.90D2 * t99 * t8 * (-t1898 * t1952 + t1930 * t1954) + 0.
     #180D3 * t1002 * t1937 * t1952
      t1967 = FJET(XB1, XB2, s, t1892, -t1886, -t1882, t1870, t1121, -t1
     #963 * t155 * t216 / 0.720D3)
      t1971 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1973 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1974 = t924 * t1973
      t1979 = t950 * t1971
      t1986 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t1987 = t924 * t1986
      t2000 = t238 * t1979
      t2029 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, 0.1
     #0D1, x4)
      t2047 = -(-0.90D2 * t99 * t8 * (-t996 * t1971 + t1974) + 0.180D3 *
     # t1002 * t1979) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (t
     #1987 + t1015 * t1971 / 0.2D1 - t1012 * t1973) + 0.180D3 * t138 * t
     #9 * (t1974 - t1012 * t1971) + t2000) * t215 * t157 / 0.720D3 - (-0
     #.90D2 * t99 * t8 * (t936 * t1971 / 0.2D1 - t932 * t1973 + t1987) +
     # 0.180D3 * t138 * t9 * (t1974 - t932 * t1971) + t2000) * t155 * t2
     #15 / 0.1440D4 + (t148 * t9 * (-t1974 + t960 * t1971) - 0.90D2 * t9
     #9 * t8 * (-t966 * t1973 / 0.2D1 + t972 * t1971 / 0.6D1 - t924 * t2
     #029 + t960 * t1986) - t419 * t1979 + 0.180D3 * t138 * t9 * (-t966 
     #* t1971 / 0.2D1 + t960 * t1973 - t1987)) * t215 / 0.1440D4
      t2048 = FJET(XB1, XB2, s, -t921, 0.0D0, t918, 0.0D0, 0.0D0, t2047)
      t2050 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t2053 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t2054 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t2060 = t3 * t2053
      t2072 = t3 * t2054
      t2081 = t9 * (-t3 * t2050 - t2050 * t750 * t793)
      t2094 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, t758, 0.10D1, 0.1
     #0D1, x4)
      t2146 = -(-0.90D2 * t99 * t8 * (-(t770 * t2050 / 0.2D1 + t2053 - t
     #767 * t2054) * t750 * t793 + t801 * t2054 - t2060 - t804 * t2050 /
     # 0.2D1) + 0.180D3 * t138 * t9 * (-(t2054 - t767 * t2050) * t750 * 
     #t793 + t801 * t2050 - t2072) + t148 * t2081) * t155 * t157 / 0.144
     #0D4 - (t58 * t9 * (t835 * t2050 - t2054) - 0.90D2 * t4 * t9 * (-t8
     #40 * t2054 / 0.2D1 + t835 * t2053 - t2094 + t844 * t2050 / 0.6D1) 
     #- t37 * t9 * t2050 + 0.180D3 * t65 * t9 * (-t840 * t2050 / 0.2D1 -
     # t2053 + t835 * t2054)) * t157 / 0.1440D4 - (-0.90D2 * t99 * t8 * 
     #(t868 * t2050 - t2072 - (t2054 - t874 * t2050) * t750 * t793) + 0.
     #180D3 * t138 * t2081) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t
     #8 * (-t2060 + t897 * t2054 - t894 * t2050 / 0.2D1) + 0.180D3 * t13
     #8 * t9 * (-t2072 + t897 * t2050) - t238 * t239 * t2050) * t215 * t
     #157 / 0.720D3
      t2147 = FJET(XB1, XB2, s, -t753, 0.0D0, 0.0D0, t747, -t757, t2146)
      t2149 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t2151 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t2152 = t1125 * t2151
      t2157 = t1140 * t2149
      t2163 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, t758, t925, 0.10D
     #1, x4)
      t2182 = -(-0.90D2 * t99 * t8 * (-t1133 * t2149 + t2152) + 0.180D3 
     #* t1002 * t2157) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (
     #t1125 * t2163 + t1158 * t2149 / 0.2D1 - t1155 * t2151) + 0.180D3 *
     # t138 * t9 * (-t1155 * t2149 + t2152) + t238 * t2157) * t215 * t15
     #7 / 0.720D3
      t2183 = FJET(XB1, XB2, s, -t1119, t747, t1118, 0.0D0, t1121, t2182
     #)
      t2185 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t2187 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t2188 = t1058 * t2187
      t2193 = t1077 * t2185
      t2200 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t925, -t1
     #059, x4)
      t2219 = -(-0.90D2 * t99 * t8 * (-t1070 * t2185 + t2188) + 0.180D3 
     #* t1002 * t2193) * t155 * t216 / 0.720D3 - (-0.90D2 * t99 * t8 * (
     #t1058 * t2200 - t1097 * t2187 + t1094 * t2185 / 0.2D1) + 0.180D3 *
     # t138 * t9 * (t2188 - t1097 * t2185) + t238 * t2193) * t155 * t215
     # / 0.1440D4
      t2220 = FJET(XB1, XB2, s, -t1050, 0.0D0, t1045, 0.0D0, 0.0D0, t221
     #9)
      t2222 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t2224 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t2225 = t1926 * t2224
      t2234 = -0.90D2 * t99 * t8 * (t1930 * t2222 - t1898 * t2225) + 0.1
     #80D3 * t1002 * t1937 * t2225
      t2238 = FJET(XB1, XB2, s, -t1886, t1892, t1870, -t1882, t1121, -t2
     #234 * t155 * t216 / 0.720D3)
      t2242 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t2244 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t758, t925, -t105
     #9, x4)
      t2245 = t1926 * t2244
      t2254 = -0.90D2 * t99 * t8 * (t1930 * t2242 - t1898 * t2245) + 0.1
     #80D3 * t1002 * t1937 * t2245
      t2258 = FJET(XB1, XB2, s, -t1882, t1870, t1892, -t1886, t1121, -t2
     #254 * t155 * t216 / 0.720D3)
      t2262 = t1715 * t1714 + t1794 * t1793 + t1831 * t1830 + t1867 * t1
     #866 - t1945 * t1941 * t1948 / 0.720D3 - t1967 * t1963 * t1948 / 0.
     #720D3 + t2048 * t2047 + t2147 * t2146 + t2183 * t2182 + t2220 * t2
     #219 - t2238 * t2234 * t1948 / 0.720D3 - t2258 * t2254 * t1948 / 0.
     #720D3
      rrgq2qght6s1e1 = t1680 + t2262

      end function



      doubleprecision function rrgq2qght6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = t7 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = t21 ** 2
      t23 = 0.1D1 / z
      t25 = -0.1D1 + x3
      t26 = 0.1D1 / t25
      t27 = t18 * t26
      t30 = log(-0.4D1 * t13 * t27)
      t31 = t30 ** 2
      t32 = cos(t10)
      t34 = Sqrt(-x3 * t25)
      t39 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t34 * z)
      t42 = -t22 * t23 / 0.2D1 - t31 * t39 / 0.2D1
      t46 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t47 = t7 * t46
      t50 = 0.3141592653589793D1 * lh
      t51 = t3 * t7
      t52 = t51 * t8
      t58 = t21 * t23 + t30 * t39
      t60 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t61 = t7 * t60
      t67 = lh ** 2
      t69 = 0.3141592653589793D1 ** 2
      t71 = -0.180D3 * t67 + 0.30D2 * t69
      t72 = 0.3141592653589793D1 * t71
      t75 = -t23 - t39
      t78 = 0.1D1 / x3
      t81 = t15 * t12
      t82 = t81 * t17
      t84 = log(0.4D1 * t82)
      t85 = t84 * 0.3141592653589793D1
      t86 = t23 * lh
      t89 = t84 ** 2
      t90 = t89 * 0.3141592653589793D1
      t93 = 0.3141592653589793D1 * t23
      t96 = (-0.180D3 * t85 * t86 - 0.45D2 * t90 * t23 + t93 * t71) * t3
      t99 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t118 = (0.90D2 * t90 * t86 + t93 * (-0.60D2 * lh * t69 + 0.2884936
     #567583026D3 + 0.120D3 * t67 * lh) + 0.15D2 * t89 * t84 * 0.3141592
     #653589793D1 * t23 - t85 * t23 * t71) * t3
      t121 = t93 * lh
      t126 = (0.180D3 * t121 + 0.90D2 * t85 * t23) * t3
      t129 = x2 ** 2
      t130 = x3 * t129
      t131 = t130 * t12
      t134 = log(-0.4D1 * t131 * t27)
      t138 = t23 * t46
      t141 = log(0.4D1 * t130 * t82)
      t142 = t141 * t23
      t150 = t23 * t8 + t8 * t39
      t153 = 0.180D3 * t50 * t51 * t150
      t156 = 0.1D1 / x2
      t160 = t129 * t12
      t163 = log(0.4D1 * t160 * t18)
      t164 = t163 ** 2
      t165 = t164 * t23
      t168 = t163 * t23
      t179 = t72 * t3
      t180 = t7 * t23
      t181 = t180 * t8
      t182 = t179 * t181
      t186 = x1 ** 2
      t187 = x3 * t186
      t188 = t187 * t12
      t191 = log(-0.4D1 * t188 * t27)
      t197 = log(0.4D1 * t187 * t82)
      t198 = t197 * t23
      t206 = 0.1D1 / x1
      t209 = t4 * t7
      t211 = t156 * t206
      t215 = t129 * t186
      t218 = log(0.4D1 * t215 * t82)
      t219 = t218 * t23
      t225 = t50 * t3
      t232 = t186 * t12
      t235 = log(0.4D1 * t232 * t18)
      t237 = t235 ** 2
      t252 = -(0.90D2 * t4 * t9 * t42 + (0.90D2 * t4 * t47 - 0.180D3 * t
     #50 * t52) * t58 + (0.90D2 * t4 * t61 - 0.180D3 * t50 * t51 * t46 -
     # t72 * t52) * t75) * t78 / 0.2880D4 - t96 * t47 / 0.2880D4 + t93 *
     # t51 * t99 / 0.32D2 - t118 * t9 / 0.2880D4 - t126 * t61 / 0.2880D4
     # - (-0.90D2 * t4 * t7 * (-(-t46 + t134 * t8) * t39 + t138 - t142 *
     # t8) + t153) * t78 * t156 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t23 
     #* t60 - t165 * t8 / 0.2D1 + t168 * t46) + 0.180D3 * t50 * t51 * (t
     #168 * t8 - t138) - t182) * t156 / 0.1440D4 - (-0.90D2 * t4 * t7 * 
     #(-(-t46 + t191 * t8) * t39 + t138 - t198 * t8) + t153) * t78 * t20
     #6 / 0.1440D4 + t209 * t150 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * 
     #t7 * (-t219 * t8 + t138) + 0.180D3 * t225 * t181) * t156 * t206 / 
     #0.720D3 - (-0.90D2 * t93 * t51 * (-t235 * t46 + t237 * t8 / 0.2D1 
     #+ t60) + 0.180D3 * t121 * t51 * (-t235 * t8 + t46) + t182) * t206 
     #/ 0.1440D4
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t252)
      t255 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t256 = t7 * t255
      t260 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t261 = t7 * t260
      t264 = t51 * t255
      t269 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t270 = t7 * t269
      t284 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t295 = t23 * t260
      t303 = t255 * t39 + t23 * t255
      t306 = 0.180D3 * t50 * t51 * t303
      t324 = t180 * t255
      t325 = t179 * t324
      t371 = -(0.90D2 * t4 * t256 * t42 + (0.90D2 * t4 * t261 - 0.180D3 
     #* t50 * t264) * t58 + (0.90D2 * t4 * t270 - 0.180D3 * t50 * t51 * 
     #t260 - t72 * t264) * t75) * t78 / 0.2880D4 - t96 * t261 / 0.2880D4
     # + t93 * t51 * t284 / 0.32D2 - t118 * t256 / 0.2880D4 - t126 * t27
     #0 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t260 + t134 * t255) * t39 
     #+ t295 - t142 * t255) + t306) * t78 * t156 / 0.1440D4 + (-0.90D2 *
     # t4 * t7 * (-t23 * t269 - t165 * t255 / 0.2D1 + t168 * t260) + 0.1
     #80D3 * t50 * t51 * (t168 * t255 - t295) - t325) * t156 / 0.1440D4 
     #- (-0.90D2 * t4 * t7 * (-(-t260 + t191 * t255) * t39 + t295 - t198
     # * t255) + t306) * t78 * t206 / 0.1440D4 + t209 * t303 * t78 * t21
     #1 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t219 * t255 + t295) + 0.180D3 *
     # t225 * t324) * t156 * t206 / 0.720D3 - (-0.90D2 * t93 * t51 * (t2
     #69 - t235 * t260 + t237 * t255 / 0.2D1) + 0.180D3 * t121 * t51 * (
     #t260 - t235 * t255) + t325) * t206 / 0.1440D4
      t372 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t371)
      t374 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t375 = t7 * t374
      t379 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t380 = t7 * t379
      t383 = t51 * t374
      t388 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t389 = t7 * t388
      t403 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t414 = t23 * t379
      t422 = t374 * t39 + t23 * t374
      t425 = 0.180D3 * t50 * t51 * t422
      t443 = t180 * t374
      t444 = t179 * t443
      t490 = -(0.90D2 * t4 * t375 * t42 + (0.90D2 * t4 * t380 - 0.180D3 
     #* t50 * t383) * t58 + (0.90D2 * t4 * t389 - 0.180D3 * t50 * t51 * 
     #t379 - t72 * t383) * t75) * t78 / 0.2880D4 - t96 * t380 / 0.2880D4
     # + t93 * t51 * t403 / 0.32D2 - t118 * t375 / 0.2880D4 - t126 * t38
     #9 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t379 + t134 * t374) * t39 
     #+ t414 - t142 * t374) + t425) * t78 * t156 / 0.1440D4 + (-0.90D2 *
     # t4 * t7 * (-t23 * t388 + t168 * t379 - t165 * t374 / 0.2D1) + 0.1
     #80D3 * t50 * t51 * (-t414 + t168 * t374) - t444) * t156 / 0.1440D4
     # - (-0.90D2 * t4 * t7 * (-(-t379 + t191 * t374) * t39 + t414 - t19
     #8 * t374) + t425) * t78 * t206 / 0.1440D4 + t209 * t422 * t78 * t2
     #11 / 0.8D1 - (-0.90D2 * t4 * t7 * (t414 - t219 * t374) + 0.180D3 *
     # t225 * t443) * t156 * t206 / 0.720D3 - (-0.90D2 * t93 * t51 * (t2
     #37 * t374 / 0.2D1 - t235 * t379 + t388) + 0.180D3 * t121 * t51 * (
     #-t235 * t374 + t379) + t444) * t206 / 0.1440D4
      t491 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t490)
      t493 = t2 * x1
      t494 = -0.1D1 + x1
      t495 = x1 * z
      t496 = 0.1D1 - x1 + t495
      t497 = 0.1D1 / t496
      t499 = t2 * t494 * t497
      t500 = s * t16
      t502 = x1 * t494 * t497
      t503 = t500 * t502
      t504 = -t494
      t505 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.10
     #D1, x4)
      t507 = t17 * t497
      t508 = t494 ** 2
      t513 = log(-0.4D1 * t187 * t81 * t507 * t508 * t26)
      t514 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.10
     #D1, x4)
      t518 = x3 * x1
      t519 = 0.2D1 * t518
      t520 = x3 * t496
      t522 = Sqrt(-t520 * t25)
      t526 = t518 * z
      t527 = 0.3D1 * t526
      t528 = x1 * t14
      t529 = x3 * t14
      t530 = t529 * x1
      t532 = 0.2D1 * t187 * z
      t533 = t187 * t14
      t534 = -z + t519 - t187 + 0.2D1 * t32 * t522 * z + t495 - t527 - t
     #528 + t530 + t532 - t533 - x3
      t535 = 0.1D1 / t534
      t538 = t18 * t497 * t508
      t541 = log(0.4D1 * t188 * t538)
      t542 = t541 * t23
      t544 = t23 * t505
      t552 = -t23 * t514 - t514 * t496 * t535
      t564 = t215 * t12
      t567 = log(0.4D1 * t564 * t538)
      t568 = t567 * t23
      t574 = t180 * t514
      t585 = log(0.4D1 * t232 * t15 * t507 * t508)
      t587 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.10
     #D1, x4)
      t588 = t585 ** 2
      t604 = -(-0.90D2 * t4 * t7 * (-(t505 - t513 * t514) * t496 * t535 
     #+ t542 * t514 - t544) + 0.180D3 * t50 * t51 * t552) * t78 * t206 /
     # 0.1440D4 + t209 * t552 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 
     #* (-t544 + t568 * t514) - 0.180D3 * t225 * t574) * t156 * t206 / 0
     #.720D3 - (-0.90D2 * t93 * t51 * (t585 * t505 - t587 - t588 * t514 
     #/ 0.2D1) + 0.180D3 * t121 * t51 * (-t505 + t585 * t514) - t179 * t
     #574) * t206 / 0.1440D4
      t605 = FJET(XB1, XB2, s, 0.0D0, t493, -t499, 0.0D0, -t503, t604)
      t607 = x2 * s
      t608 = t607 * t1
      t609 = -0.1D1 + x2
      t610 = t609 * s
      t611 = t610 * t1
      t612 = t18 * t609
      t615 = log(-0.4D1 * t131 * t612)
      t616 = x2 * z
      t618 = 0.1D1 / (-x2 + t616 - z)
      t619 = t615 * t618
      t620 = -t609
      t621 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.10
     #D1, x4)
      t623 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.10
     #D1, x4)
      t624 = t618 * t623
      t629 = t7 * t618
      t630 = t629 * t621
      t632 = 0.180D3 * t225 * t630
      t637 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.10
     #D1, x4)
      t641 = log(-0.4D1 * t160 * t612)
      t642 = t641 * t618
      t644 = t641 ** 2
      t645 = t644 * t618
      t661 = t4 * t629
      t668 = log(-0.4D1 * t564 * t612)
      t669 = t668 * t618
      t679 = -(-0.90D2 * t4 * t7 * (-t619 * t621 + t624) + t632) * t78 *
     # t156 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t618 * t637 + t642 * t62
     #3 - t645 * t621 / 0.2D1) + 0.180D3 * t50 * t51 * (-t624 + t642 * t
     #621) - t179 * t630) * t156 / 0.1440D4 + t661 * t621 * t78 * t211 /
     # 0.8D1 - (-0.90D2 * t4 * t7 * (-t669 * t621 + t624) + t632) * t156
     # * t206 / 0.720D3
      t680 = FJET(XB1, XB2, s, 0.0D0, t608, 0.0D0, -t611, 0.0D0, t679)
      t682 = x2 * x3
      t685 = Sqrt(x3 * t609 * t25)
      t686 = t32 * t685
      t688 = 0.2D1 * t686 * x2
      t690 = 0.1D1 - x3 + t682
      t691 = 0.1D1 / t690
      t693 = t2 * (0.1D1 - x3 - x2 + t682 + t130 + t688) * t691
      t698 = t2 * x2 * (-0.1D1 + t682 + 0.2D1 * t686) * t691
      t699 = t682 * z
      t700 = t130 * z
      t706 = 0.1D1 / (x2 - t616 + z - t699 + t700 - t130 + x3 - 0.2D1 * 
     #t686 * z + 0.2D1 * t686 * t616 - t688)
      t707 = t25 * t691
      t708 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t70
     #7, x4)
      t712 = t690 ** 2
      t718 = log(0.4D1 * t130 * t81 * t17 * t609 * t25 / t712)
      t719 = t718 * t706
      t720 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t70
     #7, x4)
      t726 = t7 * t706
      t734 = t4 * t726
      t739 = -(-0.90D2 * t4 * t7 * (t706 * t708 - t719 * t720) + 0.180D3
     # * t225 * t726 * t720) * t78 * t156 / 0.1440D4 + t734 * t720 * t78
     # * t211 / 0.8D1
      t740 = FJET(XB1, XB2, s, 0.0D0, t693, 0.0D0, -t698, 0.0D0, t739)
      t742 = t1 * t494
      t744 = t610 * t742 * t497
      t745 = t607 * t742
      t747 = t500 * t609 * t502
      t748 = x2 * x1
      t749 = t748 * z
      t751 = 0.1D1 / (-t748 + z - t616 + x2 + t749)
      t752 = t7 * t751
      t753 = t4 * t752
      t754 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D1
     #, x4)
      t759 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D1
     #, x4)
      t766 = log(-0.4D1 * t215 * t81 * t507 * t508 * t609)
      t767 = t766 * t751
      t780 = t753 * t754 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 * (t7
     #51 * t759 - t767 * t754) + 0.180D3 * t225 * t752 * t754) * t156 * 
     #t206 / 0.720D3
      t781 = FJET(XB1, XB2, s, 0.0D0, t744, t493, -t745, t747, t780)
      t783 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.10
     #D1, x4)
      t784 = t618 * t783
      t785 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.10
     #D1, x4)
      t791 = t629 * t785
      t793 = 0.180D3 * t225 * t791
      t801 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.10
     #D1, x4)
      t829 = -(-0.90D2 * t4 * t7 * (t784 - t619 * t785) + t793) * t78 * 
     #t156 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t645 * t785 / 0.2D1 + t64
     #2 * t783 - t618 * t801) + 0.180D3 * t50 * t51 * (-t784 + t642 * t7
     #85) - t179 * t791) * t156 / 0.1440D4 + t661 * t785 * t78 * t211 / 
     #0.8D1 - (-0.90D2 * t4 * t7 * (t784 - t669 * t785) + t793) * t156 *
     # t206 / 0.720D3
      t830 = FJET(XB1, XB2, s, 0.0D0, -t611, 0.0D0, t608, 0.0D0, t829)
      t832 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.10
     #D1, x4)
      t833 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.10
     #D1, x4)
      t839 = t23 * t832
      t847 = -t23 * t833 - t833 * t496 * t535
      t864 = t180 * t833
      t871 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.10
     #D1, x4)
      t888 = -(-0.90D2 * t4 * t7 * (-(t832 - t513 * t833) * t496 * t535 
     #+ t542 * t833 - t839) + 0.180D3 * t50 * t51 * t847) * t78 * t206 /
     # 0.1440D4 + t209 * t847 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 
     #* (-t839 + t568 * t833) - 0.180D3 * t225 * t864) * t156 * t206 / 0
     #.720D3 - (-0.90D2 * t93 * t51 * (-t871 + t585 * t832 - t588 * t833
     # / 0.2D1) + 0.180D3 * t121 * t51 * (-t832 + t585 * t833) - t179 * 
     #t864) * t206 / 0.1440D4
      t889 = FJET(XB1, XB2, s, 0.0D0, -t499, t493, 0.0D0, -t503, t888)
      t891 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t70
     #7, x4)
      t893 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t70
     #7, x4)
      t910 = -(-0.90D2 * t4 * t7 * (t706 * t891 - t719 * t893) + 0.180D3
     # * t225 * t726 * t893) * t78 * t156 / 0.1440D4 + t734 * t893 * t78
     # * t211 / 0.8D1
      t911 = FJET(XB1, XB2, s, 0.0D0, -t698, 0.0D0, t693, 0.0D0, t910)
      t913 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t914 = t7 * t913
      t918 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t919 = t7 * t918
      t922 = t51 * t913
      t927 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t928 = t7 * t927
      t940 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t952 = t23 * t918
      t959 = t913 * t39 + t23 * t913
      t962 = 0.180D3 * t50 * t51 * t959
      t980 = t180 * t913
      t981 = t179 * t980
      t1029 = -(0.90D2 * t4 * t914 * t42 + (0.90D2 * t4 * t919 - 0.180D3
     # * t50 * t922) * t58 + (0.90D2 * t4 * t928 - 0.180D3 * t50 * t51 *
     # t918 - t72 * t922) * t75) * t78 / 0.2880D4 + t93 * t51 * t940 / 0
     #.32D2 - t126 * t928 / 0.2880D4 - t96 * t919 / 0.2880D4 - (-0.90D2 
     #* t4 * t7 * (-t142 * t913 - (-t918 + t134 * t913) * t39 + t952) + 
     #t962) * t78 * t156 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t165 * t913
     # / 0.2D1 - t23 * t927 + t168 * t918) + 0.180D3 * t50 * t51 * (-t95
     #2 + t168 * t913) - t981) * t156 / 0.1440D4 - t118 * t914 / 0.2880D
     #4 - (-0.90D2 * t4 * t7 * (-(-t918 + t191 * t913) * t39 + t952 - t1
     #98 * t913) + t962) * t78 * t206 / 0.1440D4 + t209 * t959 * t78 * t
     #211 / 0.8D1 - (-0.90D2 * t4 * t7 * (t952 - t219 * t913) + 0.180D3 
     #* t225 * t980) * t156 * t206 / 0.720D3 - (-0.90D2 * t93 * t51 * (t
     #927 + t237 * t913 / 0.2D1 - t235 * t918) + 0.180D3 * t121 * t51 * 
     #(t918 - t235 * t913) + t981) * t206 / 0.1440D4
      t1030 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1029)
      t1032 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.1
     #0D1, x4)
      t1033 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.1
     #0D1, x4)
      t1039 = t23 * t1032
      t1047 = -t23 * t1033 - t1033 * t496 * t535
      t1064 = t180 * t1033
      t1073 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.1
     #0D1, x4)
      t1088 = -(-0.90D2 * t4 * t7 * (-(t1032 - t513 * t1033) * t496 * t5
     #35 + t542 * t1033 - t1039) + 0.180D3 * t50 * t51 * t1047) * t78 * 
     #t206 / 0.1440D4 + t209 * t1047 * t78 * t211 / 0.8D1 - (-0.90D2 * t
     #4 * t7 * (-t1039 + t568 * t1033) - 0.180D3 * t225 * t1064) * t156 
     #* t206 / 0.720D3 - (-0.90D2 * t93 * t51 * (-t588 * t1033 / 0.2D1 -
     # t1073 + t585 * t1032) + 0.180D3 * t121 * t51 * (t585 * t1033 - t1
     #032) - t179 * t1064) * t206 / 0.1440D4
      t1089 = FJET(XB1, XB2, s, t493, 0.0D0, 0.0D0, -t499, -t503, t1088)
      t1091 = t253 * t252 + t372 * t371 + t491 * t490 + t605 * t604 + t6
     #80 * t679 + t740 * t739 + t781 * t780 + t830 * t829 + t889 * t888 
     #+ t911 * t910 + t1030 * t1029 + t1089 * t1088
      t1092 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D
     #1, x4)
      t1098 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D
     #1, x4)
      t1111 = t753 * t1092 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #-t767 * t1092 + t751 * t1098) + 0.180D3 * t225 * t752 * t1092) * t
     #156 * t206 / 0.720D3
      t1112 = FJET(XB1, XB2, s, t493, -t745, 0.0D0, t744, t747, t1111)
      t1114 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.1
     #0D1, x4)
      t1115 = t618 * t1114
      t1116 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.1
     #0D1, x4)
      t1122 = t629 * t1116
      t1124 = 0.180D3 * t225 * t1122
      t1132 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.1
     #0D1, x4)
      t1160 = -(-0.90D2 * t4 * t7 * (t1115 - t619 * t1116) + t1124) * t7
     #8 * t156 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t645 * t1116 / 0.2D1 
     #+ t642 * t1114 - t618 * t1132) + 0.180D3 * t50 * t51 * (-t1115 + t
     #642 * t1116) - t179 * t1122) * t156 / 0.1440D4 + t661 * t1116 * t7
     #8 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 * (t1115 - t669 * t1116) + t
     #1124) * t156 * t206 / 0.720D3
      t1161 = FJET(XB1, XB2, s, t608, 0.0D0, -t611, 0.0D0, 0.0D0, t1160)
      t1163 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t7
     #07, x4)
      t1165 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t7
     #07, x4)
      t1182 = -(-0.90D2 * t4 * t7 * (t706 * t1163 - t719 * t1165) + 0.18
     #0D3 * t225 * t726 * t1165) * t78 * t156 / 0.1440D4 + t734 * t1165 
     #* t78 * t211 / 0.8D1
      t1183 = FJET(XB1, XB2, s, t693, 0.0D0, -t698, 0.0D0, 0.0D0, t1182)
      t1185 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D
     #1, x4)
      t1191 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D
     #1, x4)
      t1204 = t753 * t1185 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #-t767 * t1185 + t751 * t1191) + 0.180D3 * t225 * t752 * t1185) * t
     #156 * t206 / 0.720D3
      t1205 = FJET(XB1, XB2, s, t744, 0.0D0, -t745, t493, t747, t1204)
      t1208 = t493 * t682 * t691
      t1209 = t2 * t494
      t1212 = Sqrt(t520 * t609 * t25)
      t1213 = t32 * t1212
      t1215 = 0.2D1 * t1213 * x2
      t1216 = t130 * t495
      t1217 = t130 * x1
      t1221 = t1209 * (t130 - x2 + t682 + t1215 + t1216 - t1217 + 0.1D1 
     #- x3) * t497 * t691
      t1225 = t25 * s * t1 * x1 * t691
      t1231 = t1209 * x2 * (-0.1D1 + t682 + x1 - t518 - t495 + t526 + 0.
     #2D1 * t1213) * t497 * t691
      t1233 = t4 * t7 * t496
      t1235 = x2 * t186
      t1244 = 0.2D1 * t748 - t527 + t530 + t532 - t533 - t1235 - t1217 +
     # 0.2D1 * t1213 * z - t682 * x1 + t187 * x2 + t748 * t14 + 0.2D1 * 
     #t1235 * z - t1235 * t14 + t130 + t1215 - x3 + t616
      t1259 = t1216 - 0.2D1 * t1213 * t748 - 0.2D1 * t1213 * t616 + 0.2D
     #1 * t682 * t495 - t529 * t748 - 0.2D1 * t187 * t616 + t187 * t14 *
     # x2 - z + 0.2D1 * t1213 * t749 + t495 + t519 - t187 - t528 - x2 + 
     #t699 - t700 - 0.3D1 * t749
      t1261 = 0.1D1 / (t1244 + t1259)
      t1262 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, -t707
     #, x4)
      t1265 = t78 * t156 * t206
      t1266 = t1261 * t1262 * t1265
      t1269 = FJET(XB1, XB2, s, t1208, -t1221, -t1225, t1231, t747, t123
     #3 * t1266 / 0.8D1)
      t1271 = t51 * t496
      t1275 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, -t707
     #, x4)
      t1277 = t1261 * t1275 * t1265
      t1280 = FJET(XB1, XB2, s, t1231, -t1225, -t1221, t1208, t747, t123
     #3 * t1277 / 0.8D1)
      t1285 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.1
     #0D1, x4)
      t1286 = t618 * t1285
      t1287 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.1
     #0D1, x4)
      t1293 = t629 * t1287
      t1295 = 0.180D3 * t225 * t1293
      t1303 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, 0.1
     #0D1, x4)
      t1331 = -(-0.90D2 * t4 * t7 * (t1286 - t619 * t1287) + t1295) * t7
     #8 * t156 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t645 * t1287 / 0.2D1 
     #+ t642 * t1285 - t618 * t1303) + 0.180D3 * t50 * t51 * (-t1286 + t
     #642 * t1287) - t179 * t1293) * t156 / 0.1440D4 + t661 * t1287 * t7
     #8 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 * (t1286 - t669 * t1287) + t
     #1295) * t156 * t206 / 0.720D3
      t1332 = FJET(XB1, XB2, s, -t611, 0.0D0, t608, 0.0D0, 0.0D0, t1331)
      t1334 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.1
     #0D1, x4)
      t1335 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.1
     #0D1, x4)
      t1341 = t23 * t1334
      t1349 = -t23 * t1335 - t1335 * t496 * t535
      t1366 = t180 * t1335
      t1375 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, t504, 0.10D1, 0.1
     #0D1, x4)
      t1390 = -(-0.90D2 * t4 * t7 * (-(t1334 - t513 * t1335) * t496 * t5
     #35 + t542 * t1335 - t1341) + 0.180D3 * t50 * t51 * t1349) * t78 * 
     #t206 / 0.1440D4 + t209 * t1349 * t78 * t211 / 0.8D1 - (-0.90D2 * t
     #4 * t7 * (-t1341 + t568 * t1335) - 0.180D3 * t225 * t1366) * t156 
     #* t206 / 0.720D3 - (-0.90D2 * t93 * t51 * (-t588 * t1335 / 0.2D1 -
     # t1375 + t585 * t1334) + 0.180D3 * t121 * t51 * (t585 * t1335 - t1
     #334) - t179 * t1366) * t206 / 0.1440D4
      t1391 = FJET(XB1, XB2, s, -t499, 0.0D0, 0.0D0, t493, -t503, t1390)
      t1393 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D
     #1, x4)
      t1399 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t504, t620, 0.10D
     #1, x4)
      t1412 = t753 * t1393 * t78 * t211 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #-t767 * t1393 + t751 * t1399) + 0.180D3 * t225 * t752 * t1393) * t
     #156 * t206 / 0.720D3
      t1413 = FJET(XB1, XB2, s, -t745, t493, t744, 0.0D0, t747, t1412)
      t1415 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t7
     #07, x4)
      t1417 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t620, -t7
     #07, x4)
      t1434 = -(-0.90D2 * t4 * t7 * (t706 * t1415 - t719 * t1417) + 0.18
     #0D3 * t225 * t726 * t1417) * t78 * t156 / 0.1440D4 + t734 * t1417 
     #* t78 * t211 / 0.8D1
      t1435 = FJET(XB1, XB2, s, -t698, 0.0D0, t693, 0.0D0, 0.0D0, t1434)
      t1437 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, -t707
     #, x4)
      t1439 = t1261 * t1437 * t1265
      t1442 = FJET(XB1, XB2, s, -t1225, t1231, t1208, -t1221, t747, t123
     #3 * t1439 / 0.8D1)
      t1447 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t504, t620, -t707
     #, x4)
      t1449 = t1261 * t1447 * t1265
      t1452 = FJET(XB1, XB2, s, -t1221, t1208, t1231, -t1225, t747, t123
     #3 * t1449 / 0.8D1)
      t1457 = t1112 * t1111 + t1161 * t1160 + t1183 * t1182 + t1205 * t1
     #204 + t1269 * 0.3141592653589793D1 * t1271 * t1266 / 0.8D1 + t1280
     # * 0.3141592653589793D1 * t1271 * t1277 / 0.8D1 + t1332 * t1331 + 
     #t1391 * t1390 + t1413 * t1412 + t1435 * t1434 + t1442 * 0.31415926
     #53589793D1 * t1271 * t1439 / 0.8D1 + t1452 * 0.3141592653589793D1 
     #* t1271 * t1449 / 0.8D1
      rrgq2qght6s1e0 = t1091 + t1457

      end function



      doubleprecision function rrgq2qght6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = t7 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = 0.1D1 / z
      t24 = -0.1D1 + x3
      t29 = log(-0.4D1 * t13 * t18 / t24)
      t30 = cos(t10)
      t32 = Sqrt(-x3 * t24)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t32 * z)
      t39 = t21 * t22 + t29 * t37
      t43 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t44 = t7 * t43
      t47 = 0.3141592653589793D1 * lh
      t48 = t3 * t7
      t53 = -t22 - t37
      t56 = 0.1D1 / x3
      t59 = 0.3141592653589793D1 * t22
      t60 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t69 = log(0.4D1 * t15 * t12 * t17)
      t70 = t69 * 0.3141592653589793D1
      t74 = (0.180D3 * t59 * lh + 0.90D2 * t70 * t22) * t3
      t80 = t69 ** 2
      t84 = lh ** 2
      t86 = 0.3141592653589793D1 ** 2
      t91 = (-0.180D3 * t70 * t22 * lh - 0.45D2 * t80 * 0.31415926535897
     #93D1 * t22 + t59 * (-0.180D3 * t84 + 0.30D2 * t86)) * t3
      t94 = t4 * t7
      t95 = t22 * t8
      t98 = (t95 + t8 * t37) * t56
      t99 = 0.1D1 / x2
      t103 = x2 ** 2
      t104 = t103 * t12
      t107 = log(0.4D1 * t104 * t18)
      t108 = t107 * t22
      t115 = t47 * t3
      t116 = t7 * t22
      t119 = 0.180D3 * t115 * t116 * t8
      t123 = 0.1D1 / x1
      t124 = t99 * t123
      t128 = x1 ** 2
      t129 = t128 * t12
      t132 = log(0.4D1 * t129 * t18)
      t144 = -(0.90D2 * t4 * t9 * t39 + (0.90D2 * t4 * t44 - 0.180D3 * t
     #47 * t48 * t8) * t53) * t56 / 0.2880D4 + t59 * t48 * t60 / 0.32D2 
     #- t74 * t44 / 0.2880D4 - t91 * t9 / 0.2880D4 + t94 * t98 * t99 / 0
     #.16D2 + (-0.90D2 * t4 * t7 * (t108 * t8 - t22 * t43) - t119) * t99
     # / 0.1440D4 + t94 * t95 * t124 / 0.8D1 - (-0.90D2 * t59 * t48 * (-
     #t132 * t8 + t43) + t119) * t123 / 0.1440D4 + t94 * t98 * t123 / 0.
     #16D2
      t145 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t144)
      t147 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t148 = t7 * t147
      t152 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t153 = t7 * t152
      t164 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t173 = t22 * t147
      t175 = (t147 * t37 + t173) * t56
      t187 = 0.180D3 * t115 * t116 * t147
      t205 = -(0.90D2 * t4 * t148 * t39 + (0.90D2 * t4 * t153 - 0.180D3 
     #* t47 * t48 * t147) * t53) * t56 / 0.2880D4 + t59 * t48 * t164 / 0
     #.32D2 - t74 * t153 / 0.2880D4 - t91 * t148 / 0.2880D4 + t94 * t175
     # * t99 / 0.16D2 + (-0.90D2 * t4 * t7 * (t108 * t147 - t22 * t152) 
     #- t187) * t99 / 0.1440D4 + t94 * t173 * t124 / 0.8D1 - (-0.90D2 * 
     #t59 * t48 * (t152 - t132 * t147) + t187) * t123 / 0.1440D4 + t94 *
     # t175 * t123 / 0.16D2
      t206 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t205)
      t208 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t209 = t7 * t208
      t213 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t214 = t7 * t213
      t225 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t234 = t22 * t208
      t236 = (t208 * t37 + t234) * t56
      t248 = 0.180D3 * t115 * t116 * t208
      t266 = -(0.90D2 * t4 * t209 * t39 + (0.90D2 * t4 * t214 - 0.180D3 
     #* t47 * t48 * t208) * t53) * t56 / 0.2880D4 + t59 * t48 * t225 / 0
     #.32D2 - t74 * t214 / 0.2880D4 - t91 * t209 / 0.2880D4 + t94 * t236
     # * t99 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t22 * t213 + t108 * t208)
     # - t248) * t99 / 0.1440D4 + t94 * t234 * t124 / 0.8D1 - (-0.90D2 *
     # t59 * t48 * (-t132 * t208 + t213) + t248) * t123 / 0.1440D4 + t94
     # * t236 * t123 / 0.16D2
      t267 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t266)
      t269 = t2 * x1
      t270 = -0.1D1 + x1
      t271 = x1 * z
      t272 = 0.1D1 - x1 + t271
      t273 = 0.1D1 / t272
      t275 = t2 * t270 * t273
      t276 = s * t16
      t278 = x1 * t270 * t273
      t279 = t276 * t278
      t280 = -t270
      t281 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t282 = t22 * t281
      t286 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t289 = t270 ** 2
      t293 = log(0.4D1 * t129 * t15 * t17 * t273 * t289)
      t306 = x3 * x1
      t308 = x3 * t128
      t311 = Sqrt(-x3 * t272 * t24)
      t323 = -z + 0.2D1 * t306 - t308 + 0.2D1 * t30 * t311 * z + t271 - 
     #0.3D1 * t306 * z - x1 * t14 + x3 * t14 * x1 + 0.2D1 * t308 * z - t
     #308 * t14 - x3
      t324 = 0.1D1 / t323
      t331 = -t94 * t282 * t124 / 0.8D1 - (-0.90D2 * t59 * t48 * (-t286 
     #+ t293 * t281) - 0.180D3 * t115 * t116 * t281) * t123 / 0.1440D4 +
     # t94 * (-t282 - t281 * t272 * t324) * t56 * t123 / 0.16D2
      t332 = FJET(XB1, XB2, s, 0.0D0, t269, -t275, 0.0D0, -t279, t331)
      t334 = x2 * s
      t335 = t334 * t1
      t336 = -0.1D1 + x2
      t337 = t336 * s
      t338 = t337 * t1
      t339 = x2 * z
      t341 = 0.1D1 / (-x2 + t339 - z)
      t342 = -t336
      t343 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t344 = t341 * t343
      t345 = t56 * t99
      t349 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t354 = log(-0.4D1 * t104 * t18 * t336)
      t355 = t354 * t341
      t361 = t7 * t341
      t371 = t94 * t344 * t345 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t341 * 
     #t349 + t355 * t343) - 0.180D3 * t115 * t361 * t343) * t99 / 0.1440
     #D4 + t94 * t344 * t124 / 0.8D1
      t372 = FJET(XB1, XB2, s, 0.0D0, t335, 0.0D0, -t338, 0.0D0, t371)
      t374 = x2 * x3
      t375 = t103 * x3
      t378 = Sqrt(x3 * t336 * t24)
      t379 = t30 * t378
      t381 = 0.2D1 * t379 * x2
      t384 = 0.1D1 / (0.1D1 - x3 + t374)
      t386 = t2 * (0.1D1 - x3 - x2 + t374 + t375 + t381) * t384
      t391 = t2 * x2 * (-0.1D1 + t374 + 0.2D1 * t379) * t384
      t399 = 0.1D1 / (x2 - t339 + z - t374 * z + t375 * z - t375 + x3 - 
     #0.2D1 * t379 * z + 0.2D1 * t379 * t339 - t381)
      t400 = t24 * t384
      t401 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, -t40
     #0, x4)
      t403 = t399 * t401 * t345
      t406 = FJET(XB1, XB2, s, 0.0D0, t386, 0.0D0, -t391, 0.0D0, t94 * t
     #403 / 0.16D2)
      t411 = t1 * t270
      t413 = t337 * t411 * t273
      t414 = t334 * t411
      t416 = t276 * t336 * t278
      t417 = x2 * x1
      t420 = 0.1D1 / (-t417 + z - t339 + x2 + t417 * z)
      t421 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t280, t342, 0.10D1
     #, x4)
      t423 = t420 * t421 * t124
      t426 = FJET(XB1, XB2, s, 0.0D0, t413, t269, -t414, t416, t94 * t42
     #3 / 0.8D1)
      t431 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t432 = t341 * t431
      t436 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t452 = t94 * t432 * t345 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t341 * 
     #t436 + t355 * t431) - 0.180D3 * t115 * t361 * t431) * t99 / 0.1440
     #D4 + t94 * t432 * t124 / 0.8D1
      t453 = FJET(XB1, XB2, s, 0.0D0, -t338, 0.0D0, t335, 0.0D0, t452)
      t455 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t456 = t22 * t455
      t460 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t479 = -t94 * t456 * t124 / 0.8D1 - (-0.90D2 * t59 * t48 * (-t460 
     #+ t293 * t455) - 0.180D3 * t115 * t116 * t455) * t123 / 0.1440D4 +
     # t94 * (-t456 - t455 * t272 * t324) * t56 * t123 / 0.16D2
      t480 = FJET(XB1, XB2, s, 0.0D0, -t275, t269, 0.0D0, -t279, t479)
      t482 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, -t40
     #0, x4)
      t484 = t399 * t482 * t345
      t487 = FJET(XB1, XB2, s, 0.0D0, -t391, 0.0D0, t386, 0.0D0, t94 * t
     #484 / 0.16D2)
      t492 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t496 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t497 = t7 * t496
      t503 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t504 = t7 * t503
      t516 = t22 * t496
      t518 = (t496 * t37 + t516) * t56
      t530 = 0.180D3 * t115 * t116 * t496
      t550 = t59 * t48 * t492 / 0.32D2 - t91 * t497 / 0.2880D4 - (0.90D2
     # * t4 * t497 * t39 + (0.90D2 * t4 * t504 - 0.180D3 * t47 * t48 * t
     #496) * t53) * t56 / 0.2880D4 + t94 * t518 * t99 / 0.16D2 + (-0.90D
     #2 * t4 * t7 * (-t22 * t503 + t108 * t496) - t530) * t99 / 0.1440D4
     # - t74 * t504 / 0.2880D4 + t94 * t516 * t124 / 0.8D1 - (-0.90D2 * 
     #t59 * t48 * (t503 - t132 * t496) + t530) * t123 / 0.1440D4 + t94 *
     # t518 * t123 / 0.16D2
      t551 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t550)
      t553 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t554 = t22 * t553
      t559 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t577 = -t94 * t554 * t124 / 0.8D1 - (-0.90D2 * t59 * t48 * (t293 *
     # t553 - t559) - 0.180D3 * t115 * t116 * t553) * t123 / 0.1440D4 + 
     #t94 * (-t554 - t553 * t272 * t324) * t56 * t123 / 0.16D2
      t578 = FJET(XB1, XB2, s, t269, 0.0D0, 0.0D0, -t275, -t279, t577)
      t580 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t280, t342, 0.10D1
     #, x4)
      t582 = t420 * t580 * t124
      t585 = FJET(XB1, XB2, s, t269, -t414, 0.0D0, t413, t416, t94 * t58
     #2 / 0.8D1)
      t590 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t591 = t341 * t590
      t595 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t611 = t94 * t591 * t345 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t341 * 
     #t595 + t355 * t590) - 0.180D3 * t115 * t361 * t590) * t99 / 0.1440
     #D4 + t94 * t591 * t124 / 0.8D1
      t612 = FJET(XB1, XB2, s, t335, 0.0D0, -t338, 0.0D0, 0.0D0, t611)
      t614 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, -t40
     #0, x4)
      t616 = t399 * t614 * t345
      t619 = FJET(XB1, XB2, s, t386, 0.0D0, -t391, 0.0D0, 0.0D0, t94 * t
     #616 / 0.16D2)
      t624 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t280, t342, 0.10D1
     #, x4)
      t626 = t420 * t624 * t124
      t629 = FJET(XB1, XB2, s, t413, 0.0D0, -t414, t269, t416, t94 * t62
     #6 / 0.8D1)
      t634 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t635 = t341 * t634
      t639 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, 0.10
     #D1, x4)
      t655 = t94 * t635 * t345 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t341 * 
     #t639 + t355 * t634) - 0.180D3 * t115 * t361 * t634) * t99 / 0.1440
     #D4 + t94 * t635 * t124 / 0.8D1
      t656 = FJET(XB1, XB2, s, -t338, 0.0D0, t335, 0.0D0, 0.0D0, t655)
      t658 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t659 = t22 * t658
      t664 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, t280, 0.10D1, 0.10
     #D1, x4)
      t682 = -t94 * t659 * t124 / 0.8D1 - (-0.90D2 * t59 * t48 * (t293 *
     # t658 - t664) - 0.180D3 * t115 * t116 * t658) * t123 / 0.1440D4 + 
     #t94 * (-t659 - t658 * t272 * t324) * t56 * t123 / 0.16D2
      t683 = FJET(XB1, XB2, s, -t275, 0.0D0, 0.0D0, t269, -t279, t682)
      t685 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t342, -t40
     #0, x4)
      t687 = t399 * t685 * t345
      t690 = FJET(XB1, XB2, s, -t391, 0.0D0, t386, 0.0D0, 0.0D0, t94 * t
     #687 / 0.16D2)
      t695 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t280, t342, 0.10D1
     #, x4)
      t697 = t420 * t695 * t124
      t700 = FJET(XB1, XB2, s, -t414, t269, t413, 0.0D0, t416, t94 * t69
     #7 / 0.8D1)
      rrgq2qght6s1em1 = t145 * t144 + t206 * t205 + t267 * t266 + t332 *
     # t331 + t372 * t371 + t406 * 0.3141592653589793D1 * t48 * t403 / 0
     #.16D2 + t426 * 0.3141592653589793D1 * t48 * t423 / 0.8D1 + t453 * 
     #t452 + t480 * t479 + t487 * 0.3141592653589793D1 * t48 * t484 / 0.
     #16D2 + t551 * t550 + t578 * t577 + t585 * 0.3141592653589793D1 * t
     #48 * t582 / 0.8D1 + t612 * t611 + t619 * 0.3141592653589793D1 * t4
     #8 * t616 / 0.16D2 + t629 * 0.3141592653589793D1 * t48 * t626 / 0.8
     #D1 + t656 * t655 + t683 * t682 + t690 * 0.3141592653589793D1 * t48
     # * t687 / 0.16D2 + t700 * 0.3141592653589793D1 * t48 * t697 / 0.8D
     #1

      end function



      doubleprecision function rrgq2qght6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = 0.3141592653589793D1 * t3 * t7
      t9 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t10 = 0.1D1 / z
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t15 = Sqrt(-x3 * (-0.1D1 + x3))
      t21 = -t10 - 0.1D1 / (-z - x3 + 0.2D1 * t12 * t15 * z)
      t23 = 0.1D1 / x3
      t27 = t10 * t9
      t28 = 0.1D1 / x2
      t32 = 0.1D1 / x1
      t36 = 0.3141592653589793D1 * t10
      t37 = t3 * t7
      t38 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t44 = z ** 2
      t46 = Sin(t11)
      t47 = t46 ** 2
      t49 = t1 ** 2
      t50 = t49 ** 2
      t53 = log(0.4D1 / t44 * t47 * t50)
      t58 = (0.180D3 * t36 * lh + 0.90D2 * t53 * 0.3141592653589793D1 * 
     #t10) * t3
      t62 = -t8 * t9 * t21 * t23 / 0.32D2 + t8 * t27 * t28 / 0.16D2 + t8
     # * t27 * t32 / 0.16D2 + t36 * t37 * t38 / 0.32D2 - t58 * t7 * t9 /
     # 0.2880D4
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t62)
      t65 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t70 = t10 * t65
      t77 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t84 = -t8 * t65 * t21 * t23 / 0.32D2 + t8 * t70 * t28 / 0.16D2 + t
     #8 * t70 * t32 / 0.16D2 + t36 * t37 * t77 / 0.32D2 - t58 * t7 * t65
     # / 0.2880D4
      t85 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t84)
      t87 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t92 = t10 * t87
      t99 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t106 = -t8 * t87 * t21 * t23 / 0.32D2 + t8 * t92 * t28 / 0.16D2 + 
     #t8 * t92 * t32 / 0.16D2 + t36 * t37 * t99 / 0.32D2 - t58 * t7 * t8
     #7 / 0.2880D4
      t107 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t106)
      t109 = t2 * x1
      t110 = -0.1D1 + x1
      t113 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t115 = t2 * t110 * t113
      t119 = s * t49 * x1 * t110 * t113
      t120 = -t110
      t121 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, t120, 0.10D1, 0.10
     #D1, x4)
      t126 = FJET(XB1, XB2, s, 0.0D0, t109, -t115, 0.0D0, -t119, -t8 * t
     #10 * t121 * t32 / 0.16D2)
      t129 = t7 * t10
      t135 = x2 * s * t1
      t136 = -0.1D1 + x2
      t138 = t136 * s * t1
      t141 = 0.1D1 / (-x2 + x2 * z - z)
      t142 = -t136
      t143 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t142, 0.10
     #D1, x4)
      t148 = FJET(XB1, XB2, s, 0.0D0, t135, 0.0D0, -t138, 0.0D0, t8 * t1
     #41 * t143 * t28 / 0.16D2)
      t151 = t7 * t141
      t156 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t142, 0.10
     #D1, x4)
      t161 = FJET(XB1, XB2, s, 0.0D0, -t138, 0.0D0, t135, 0.0D0, t8 * t1
     #41 * t156 * t28 / 0.16D2)
      t168 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, t120, 0.10D1, 0.10
     #D1, x4)
      t173 = FJET(XB1, XB2, s, 0.0D0, -t115, t109, 0.0D0, -t119, -t8 * t
     #10 * t168 * t32 / 0.16D2)
      t180 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t185 = t10 * t180
      t192 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t199 = -t8 * t180 * t21 * t23 / 0.32D2 + t8 * t185 * t28 / 0.16D2 
     #+ t8 * t185 * t32 / 0.16D2 + t36 * t37 * t192 / 0.32D2 - t58 * t7 
     #* t180 / 0.2880D4
      t200 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t199)
      t202 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, t120, 0.10D1, 0.10
     #D1, x4)
      t207 = FJET(XB1, XB2, s, t109, 0.0D0, 0.0D0, -t115, -t119, -t8 * t
     #10 * t202 * t32 / 0.16D2)
      t214 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t142, 0.10
     #D1, x4)
      t219 = FJET(XB1, XB2, s, t135, 0.0D0, -t138, 0.0D0, 0.0D0, t8 * t1
     #41 * t214 * t28 / 0.16D2)
      t226 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t142, 0.10
     #D1, x4)
      t231 = FJET(XB1, XB2, s, -t138, 0.0D0, t135, 0.0D0, 0.0D0, t8 * t1
     #41 * t226 * t28 / 0.16D2)
      t239 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, t120, 0.10D1, 0.10
     #D1, x4)
      t244 = FJET(XB1, XB2, s, -t115, 0.0D0, 0.0D0, t109, -t119, -t36 * 
     #t3 * t7 * t239 * t32 / 0.16D2)
      rrgq2qght6s1em2 = t63 * t62 + t85 * t84 + t107 * t106 - t126 * 0.3
     #141592653589793D1 * t3 * t129 * t121 * t32 / 0.16D2 + t148 * 0.314
     #1592653589793D1 * t3 * t151 * t143 * t28 / 0.16D2 + t161 * 0.31415
     #92653589793D1 * t3 * t151 * t156 * t28 / 0.16D2 - t173 * 0.3141592
     #653589793D1 * t3 * t129 * t168 * t32 / 0.16D2 + t200 * t199 - t207
     # * 0.3141592653589793D1 * t3 * t129 * t202 * t32 / 0.16D2 + t219 *
     # 0.3141592653589793D1 * t3 * t151 * t214 * t28 / 0.16D2 + t231 * 0
     #.3141592653589793D1 * t3 * t151 * t226 * t28 / 0.16D2 - t244 * 0.3
     #141592653589793D1 * t10 * t37 * t239 * t32 / 0.16D2

      end function



      doubleprecision function rrgq2qght6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t9 = 0.1D1 / t5 / s / z
      t10 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t11 = t10 * t9
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t34 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t35 = t9 * t34
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t35 /
     # 0.32D2)
      rrgq2qght6s1em3 = t14 * 0.3141592653589793D1 * t3 * t11 / 0.32D2 +
     # t22 * 0.3141592653589793D1 * t3 * t19 / 0.32D2 + t30 * 0.31415926
     #53589793D1 * t3 * t27 / 0.32D2 + t38 * 0.3141592653589793D1 * t3 *
     # t35 / 0.32D2

      end function



      doubleprecision function rrgq2qght6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      rrgq2qght6s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght6s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = 0.3141592653589793D1 * t3
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = rrgq2qgh62J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = x4 * 0.3141592653589793D1
      t18 = Sin(t17)
      t19 = t18 ** 2
      t20 = t16 * t19
      t21 = t1 ** 2
      t22 = t21 ** 2
      t23 = t20 * t22
      t25 = log(0.4D1 * t23)
      t26 = t25 ** 2
      t27 = t26 * t3
      t28 = 0.3141592653589793D1 * lh
      t31 = 0.3141592653589793D1 ** 2
      t34 = lh ** 2
      t37 = -0.60D2 * lh * t31 + 0.2884936567583026D3 + 0.120D3 * t34 * 
     #lh
      t38 = t4 * t37
      t40 = t26 * t25 * t3
      t43 = t25 * t3
      t46 = -0.180D3 * t34 + 0.30D2 * t31
      t47 = 0.3141592653589793D1 * t46
      t50 = (0.90D2 * t27 * t28 + t38 + 0.15D2 * t40 * 0.314159265358979
     #3D1 - t43 * t47) * t5
      t51 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t52 = t8 * t51
      t59 = t4 * t46
      t61 = (-0.180D3 * t43 * t28 - 0.45D2 * t27 * 0.3141592653589793D1 
     #+ t59) * t5
      t62 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t63 = t8 * t62
      t66 = t4 * lh
      t71 = (0.180D3 * t66 + 0.90D2 * t43 * 0.3141592653589793D1) * t5
      t72 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t73 = t8 * t72
      t80 = 0.3141592653589793D1 * t37
      t83 = t31 ** 2
      t84 = t34 ** 2
      t90 = t26 ** 2
      t95 = (-0.30D2 * t40 * t28 + t27 * t47 / 0.2D1 - t43 * t80 + t4 * 
     #(-0.5769873135166051D3 * lh - t83 - 0.60D2 * t84 + 0.60D2 * t34 * 
     #t31) - 0.15D2 / 0.4D1 * t90 * t3 * 0.3141592653589793D1) * t5
      t96 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t97 = t8 * t96
      t100 = 0.3141592653589793D1 * t5
      t101 = t3 * t62
      t102 = x1 ** 2
      t103 = x3 * t102
      t106 = log(0.4D1 * t103 * t23)
      t107 = t106 * t3
      t109 = t106 ** 2
      t110 = t109 * t3
      t113 = t103 * t19
      t114 = t16 * t22
      t115 = -0.1D1 + x3
      t116 = 0.1D1 / t115
      t120 = log(-0.4D1 * t113 * t114 * t116)
      t122 = t120 ** 2
      t126 = cos(t17)
      t127 = x3 * z
      t129 = Sqrt(-t127 * t115)
      t133 = 0.1D1 / (-z - x3 + 0.2D1 * t126 * t129)
      t140 = t3 * t51
      t148 = t96 * t133
      t149 = t3 * t96
      t154 = 0.1D1 / x3
      t156 = 0.1D1 / x1
      t159 = t102 * t19
      t162 = log(0.4D1 * t159 * t114)
      t167 = t162 ** 2
      t170 = t167 * t162
      t178 = t9 * t96
      t190 = x2 ** 2
      t191 = x3 * t190
      t192 = t191 * t102
      t197 = log(-0.4D1 * t192 * t20 * t22 * t116)
      t203 = log(0.4D1 * t192 * t23)
      t204 = t203 * t3
      t206 = 0.1D1 - x2
      t207 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t208 = t3 * t207
      t209 = -t206
      t210 = t22 * t209
      t214 = log(-0.4D1 * t192 * t20 * t210)
      t215 = t214 * t3
      t216 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t222 = t3 * t216
      t223 = -t149 - t148 + t222
      t229 = 0.1D1 / x2
      t230 = t229 * t156
      t233 = t190 * t102
      t236 = log(0.4D1 * t233 * t23)
      t237 = t236 * t3
      t239 = t236 ** 2
      t240 = t239 * t3
      t243 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t244 = t3 * t243
      t245 = t233 * t19
      t249 = log(-0.4D1 * t245 * t114 * t209)
      t250 = t249 * t3
      t252 = t249 ** 2
      t253 = t252 * t3
      t275 = t9 * t51
      t280 = x3 * t16
      t281 = t19 * t22
      t284 = log(0.4D1 * t280 * t281)
      t286 = t281 * t116
      t289 = log(-0.4D1 * t280 * t286)
      t291 = t284 * t3 + t289 * t133
      t293 = t289 ** 2
      t296 = t284 ** 2
      t300 = t293 * t289 * t133 / 0.6D1 + t296 * t284 * t3 / 0.6D1
      t312 = -t3 - t133
      t322 = -t296 * t3 / 0.2D1 - t293 * t133 / 0.2D1
      t329 = log(0.4D1 * t191 * t23)
      t330 = t329 * t3
      t332 = t329 ** 2
      t333 = t332 * t3
      t336 = t191 * t16
      t337 = t281 * t209
      t340 = log(-0.4D1 * t336 * t337)
      t341 = t340 * t3
      t343 = t340 ** 2
      t344 = t343 * t3
      t349 = log(-0.4D1 * t336 * t286)
      t351 = t349 ** 2
      t376 = t16 * t190
      t379 = log(-0.4D1 * t376 * t337)
      t383 = log(0.4D1 * t376 * t281)
      t389 = t383 ** 2
      t390 = t389 * t383
      t393 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t394 = t379 ** 2
      t397 = t394 * t379
      t423 = t4 * t9 * t10 / 0.32D2 - t50 * t52 / 0.2880D4 - t61 * t63 /
     # 0.2880D4 - t71 * t73 / 0.2880D4 - t95 * t97 / 0.2880D4 - (-0.90D2
     # * t100 * t8 * (t101 - t107 * t51 + t110 * t96 / 0.2D1 + (t62 - t1
     #20 * t51 + t122 * t96 / 0.2D1) * t133) + 0.180D3 * t28 * t9 * (-t1
     #07 * t96 + t140 + (t51 - t120 * t96) * t133) + t47 * t9 * (t148 + 
     #t149)) * t154 * t156 / 0.1440D4 - (t59 * t9 * (t51 - t162 * t96) -
     # 0.90D2 * t4 * t9 * (t72 + t167 * t51 / 0.2D1 - t170 * t96 / 0.6D1
     # - t162 * t62) + t38 * t178 + 0.180D3 * t66 * t9 * (-t162 * t51 + 
     #t167 * t96 / 0.2D1 + t62)) * t156 / 0.1440D4 + (-0.90D2 * t100 * t
     #8 * (-(t51 - t197 * t96) * t133 + t204 * t96 + t208 - t140 - t215 
     #* t216) + 0.180D3 * t28 * t9 * t223) * t154 * t230 / 0.720D3 + (-0
     #.90D2 * t100 * t8 * (-t101 + t237 * t51 - t240 * t96 / 0.2D1 + t24
     #4 - t250 * t207 + t253 * t216 / 0.2D1) + 0.180D3 * t28 * t9 * (-t1
     #40 + t237 * t96 + t208 - t250 * t216) + t47 * t9 * (t222 - t149)) 
     #* t229 * t156 / 0.720D3 + ((-0.90D2 * t100 * t63 + 0.180D3 * t28 *
     # t275 + t47 * t178) * t291 - 0.90D2 * t100 * t97 * t300 + (t47 * t
     #275 - 0.90D2 * t100 * t73 + t80 * t178 + 0.180D3 * t28 * t9 * t62)
     # * t312 + (-0.90D2 * t100 * t52 + 0.180D3 * t28 * t178) * t322) * 
     #t154 / 0.2880D4 - (-0.90D2 * t100 * t8 * (t101 - t330 * t51 + t333
     # * t96 / 0.2D1 - t244 + t341 * t207 - t344 * t216 / 0.2D1 + (t62 -
     # t349 * t51 + t351 * t96 / 0.2D1) * t133) + 0.180D3 * t28 * t9 * (
     #t140 - t330 * t96 - t208 + t341 * t216 + (t51 - t349 * t96) * t133
     #) - t47 * t9 * t223) * t154 * t229 / 0.1440D4 + (t59 * t9 * (-t379
     # * t216 + t207 - t51 + t383 * t96) - 0.90D2 * t4 * t9 * (t383 * t6
     #2 + t390 * t96 / 0.6D1 + t393 + t394 * t207 / 0.2D1 - t72 - t397 *
     # t216 / 0.6D1 - t389 * t51 / 0.2D1 - t379 * t243) + t38 * t9 * (t2
     #16 - t96) + 0.180D3 * t66 * t9 * (t243 - t379 * t207 + t394 * t216
     # / 0.2D1 - t62 + t383 * t51 - t389 * t96 / 0.2D1)) * t229 / 0.1440
     #D4
      t424 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t423)
      t426 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t427 = t3 * t426
      t428 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t430 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t433 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t434 = t3 * t433
      t435 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t437 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t449 = t3 * t428
      t451 = t3 * t435
      t460 = t3 * t430
      t461 = t3 * t437
      t462 = t430 * t133
      t463 = t460 - t461 + t462
      t479 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t484 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t507 = rrgq2qgh64J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t511 = t8 * t428
      t514 = t8 * t426
      t517 = t8 * t479
      t520 = t8 * t430
      t525 = t9 * t428
      t528 = t9 * t430
      t644 = -(-0.90D2 * t100 * t8 * (t427 - t330 * t428 + t333 * t430 /
     # 0.2D1 - t434 + t341 * t435 - t344 * t437 / 0.2D1 + (t426 - t349 *
     # t428 + t351 * t430 / 0.2D1) * t133) + 0.180D3 * t28 * t9 * (t449 
     #- t330 * t430 - t451 + t341 * t437 + (t428 - t349 * t430) * t133) 
     #+ t47 * t9 * t463) * t154 * t229 / 0.1440D4 + (t59 * t9 * (t435 - 
     #t379 * t437 - t428 + t383 * t430) - 0.90D2 * t4 * t9 * (-t389 * t4
     #28 / 0.2D1 - t397 * t437 / 0.6D1 - t479 + t390 * t430 / 0.6D1 + t3
     #94 * t435 / 0.2D1 + t484 + t383 * t426 - t379 * t433) + t38 * t9 *
     # (-t430 + t437) + 0.180D3 * t66 * t9 * (-t379 * t435 + t433 + t394
     # * t437 / 0.2D1 + t383 * t428 - t426 - t389 * t430 / 0.2D1)) * t22
     #9 / 0.1440D4 + t4 * t9 * t507 / 0.32D2 - t50 * t511 / 0.2880D4 - t
     #61 * t514 / 0.2880D4 - t71 * t517 / 0.2880D4 - t95 * t520 / 0.2880
     #D4 + ((-0.90D2 * t100 * t514 + 0.180D3 * t28 * t525 + t47 * t528) 
     #* t291 - 0.90D2 * t100 * t520 * t300 + (t47 * t525 - 0.90D2 * t100
     # * t517 + t80 * t528 + 0.180D3 * t28 * t9 * t426) * t312 + (-0.90D
     #2 * t100 * t511 + 0.180D3 * t28 * t528) * t322) * t154 / 0.2880D4 
     #- (-0.90D2 * t100 * t8 * (t427 + (t426 - t120 * t428 + t122 * t430
     # / 0.2D1) * t133 + t110 * t430 / 0.2D1 - t107 * t428) + 0.180D3 * 
     #t28 * t9 * (-t107 * t430 + t449 + (t428 - t120 * t430) * t133) + t
     #47 * t9 * (t462 + t460)) * t154 * t156 / 0.1440D4 - (t59 * t9 * (t
     #428 - t162 * t430) - 0.90D2 * t4 * t9 * (t479 + t167 * t428 / 0.2D
     #1 - t170 * t430 / 0.6D1 - t162 * t426) + t38 * t528 + 0.180D3 * t6
     #6 * t9 * (-t162 * t428 + t426 + t167 * t430 / 0.2D1)) * t156 / 0.1
     #440D4 + (-0.90D2 * t100 * t8 * (-t215 * t437 - t449 + t451 - (t428
     # - t197 * t430) * t133 + t204 * t430) - 0.180D3 * t28 * t9 * t463)
     # * t154 * t230 / 0.720D3 + (-0.90D2 * t100 * t8 * (-t427 + t237 * 
     #t428 - t240 * t430 / 0.2D1 + t434 - t250 * t435 + t253 * t437 / 0.
     #2D1) + 0.180D3 * t28 * t9 * (-t449 + t237 * t430 + t451 - t250 * t
     #437) + t47 * t9 * (-t460 + t461)) * t229 * t156 / 0.720D3
      t645 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t644)
      t647 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t650 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t651 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t656 = t3 * t650
      t663 = t3 * t651
      t672 = t647 * t133
      t673 = t3 * t647
      t687 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t695 = t9 * t647
      t707 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t712 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t713 = t3 * t712
      t719 = t3 * t707
      t720 = t719 - t673 - t672
      t731 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t732 = t3 * t731
      t795 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10D
     #1, x4)
      t820 = rrgq2qgh61J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t824 = t8 * t651
      t827 = t8 * t650
      t830 = t8 * t687
      t835 = t9 * t651
      t841 = t8 * t647
      t865 = -(-0.90D2 * t100 * t8 * ((t122 * t647 / 0.2D1 + t650 - t120
     # * t651) * t133 - t107 * t651 + t656 + t110 * t647 / 0.2D1) + 0.18
     #0D3 * t28 * t9 * (t663 - t107 * t647 + (t651 - t120 * t647) * t133
     #) + t47 * t9 * (t672 + t673)) * t154 * t156 / 0.1440D4 - (t59 * t9
     # * (-t162 * t647 + t651) - 0.90D2 * t4 * t9 * (-t170 * t647 / 0.6D
     #1 + t687 + t167 * t651 / 0.2D1 - t162 * t650) + t38 * t695 + 0.180
     #D3 * t66 * t9 * (t650 - t162 * t651 + t167 * t647 / 0.2D1)) * t156
     # / 0.1440D4 + (-0.90D2 * t100 * t8 * (-t215 * t707 - t663 - (t651 
     #- t197 * t647) * t133 + t713 + t204 * t647) + 0.180D3 * t28 * t9 *
     # t720) * t154 * t230 / 0.720D3 + (-0.90D2 * t100 * t8 * (-t656 + t
     #237 * t651 - t240 * t647 / 0.2D1 + t732 - t250 * t712 + t253 * t70
     #7 / 0.2D1) + 0.180D3 * t28 * t9 * (-t663 + t237 * t647 + t713 - t2
     #50 * t707) + t47 * t9 * (-t673 + t719)) * t229 * t156 / 0.720D3 - 
     #(-0.90D2 * t100 * t8 * (t656 - t330 * t651 + t333 * t647 / 0.2D1 -
     # t732 + t341 * t712 - t344 * t707 / 0.2D1 + (t351 * t647 / 0.2D1 +
     # t650 - t349 * t651) * t133) + 0.180D3 * t28 * t9 * (t663 - t330 *
     # t647 - t713 + t341 * t707 + (t651 - t349 * t647) * t133) - t47 * 
     #t9 * t720) * t154 * t229 / 0.1440D4 + (t59 * t9 * (t712 - t379 * t
     #707 - t651 + t383 * t647) - 0.90D2 * t4 * t9 * (t394 * t712 / 0.2D
     #1 - t397 * t707 / 0.6D1 - t687 + t390 * t647 / 0.6D1 + t795 + t383
     # * t650 - t389 * t651 / 0.2D1 - t379 * t731) + t38 * t9 * (-t647 +
     # t707) + 0.180D3 * t66 * t9 * (-t379 * t712 - t650 + t383 * t651 +
     # t731 + t394 * t707 / 0.2D1 - t389 * t647 / 0.2D1)) * t229 / 0.144
     #0D4 + t4 * t9 * t820 / 0.32D2 - t50 * t824 / 0.2880D4 - t61 * t827
     # / 0.2880D4 - t71 * t830 / 0.2880D4 + ((-0.90D2 * t100 * t827 + 0.
     #180D3 * t28 * t835 + t47 * t695) * t291 - 0.90D2 * t100 * t841 * t
     #300 + (t47 * t835 - 0.90D2 * t100 * t830 + t80 * t695 + 0.180D3 * 
     #t28 * t9 * t650) * t312 + (-0.90D2 * t100 * t824 + 0.180D3 * t28 *
     # t695) * t322) * t154 / 0.2880D4 - t95 * t841 / 0.2880D4
      t866 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t865)
      t869 = x1 * z
      t870 = -z - x1 + t869
      t871 = 0.1D1 / t870
      t873 = t2 * x1 * t209 * t871
      t874 = -0.1D1 + x1
      t875 = t2 * t874
      t878 = x2 * s * t1 * x1
      t879 = s * t21
      t882 = x1 * t874 * t871
      t883 = t879 * t209 * t882
      t884 = t191 * t159
      t885 = 0.1D1 / t14
      t886 = t885 * t22
      t887 = t874 ** 2
      t888 = t871 * t887
      t893 = log(0.4D1 * t884 * t886 * t888 * t209)
      t894 = x2 * x1
      t895 = t894 * z
      t897 = 0.1D1 / (t895 - t894 - z)
      t898 = t893 * t897
      t899 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1, 
     #x4)
      t901 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1, 
     #x4)
      t902 = t897 * t901
      t907 = t28 * t5
      t908 = t8 * t897
      t909 = t908 * t899
      t915 = t19 * t885
      t917 = t22 * t871
      t922 = log(0.4D1 * t233 * t915 * t917 * t887 * t209)
      t923 = t922 * t897
      t925 = t922 ** 2
      t926 = t925 * t897
      t929 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1, 
     #x4)
      t940 = t47 * t5
      t946 = (-0.90D2 * t100 * t8 * (-t898 * t899 + t902) + 0.180D3 * t9
     #07 * t909) * t154 * t230 / 0.720D3 + (-0.90D2 * t100 * t8 * (-t923
     # * t901 + t926 * t899 / 0.2D1 + t897 * t929) + 0.180D3 * t28 * t9 
     #* (-t923 * t899 + t902) + t940 * t909) * t229 * t156 / 0.720D3
      t947 = FJET(XB1, XB2, s, 0.0D0, t873, -t875, t878, -t883, t946)
      t949 = x2 * x3
      t950 = 0.1D1 - x3 + t949
      t951 = 0.1D1 / t950
      t952 = t949 * t951
      t953 = t2 * t952
      t954 = t115 * t951
      t955 = t2 * t954
      t956 = t209 * t115
      t958 = Sqrt(t127 * t956)
      t962 = 0.1D1 / (-z - x3 + t949 + 0.2D1 * t126 * t958)
      t963 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t954
     #, x4)
      t966 = t950 ** 2
      t967 = 0.1D1 / t966
      t972 = log(0.4D1 * t191 * t20 * t210 * t115 * t967)
      t973 = t972 ** 2
      t974 = t973 * t962
      t975 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t954
     #, x4)
      t978 = t972 * t962
      t979 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t954
     #, x4)
      t985 = t962 * t979
      t991 = t8 * t962
      t992 = t991 * t975
      t998 = t956 * t967
      t1002 = log(0.4D1 * t884 * t114 * t998)
      t1003 = t1002 * t962
      t1015 = -(-0.90D2 * t100 * t8 * (-t962 * t963 - t974 * t975 / 0.2D
     #1 + t978 * t979) + 0.180D3 * t28 * t9 * (-t985 + t978 * t975) - t9
     #40 * t992) * t154 * t229 / 0.1440D4 + (-0.90D2 * t100 * t8 * (t985
     # - t1003 * t975) + 0.180D3 * t907 * t992) * t154 * t230 / 0.720D3
      t1016 = FJET(XB1, XB2, s, 0.0D0, t953, 0.0D0, -t955, 0.0D0, t1015)
      t1019 = t2 * x1 * t871
      t1020 = t879 * t882
      t1021 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1028 = log(0.4D1 * t103 * t915 * t917 * t887 * t116)
      t1029 = t1028 * t870
      t1030 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1032 = t1028 ** 2
      t1033 = t1032 * t870
      t1034 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1038 = x3 * x1
      t1039 = t1038 * z
      t1040 = x1 * t14
      t1041 = x3 * t14
      t1042 = t1041 * x1
      t1044 = 0.2D1 * t103 * z
      t1045 = t103 * t14
      t1046 = x3 * t870
      t1048 = Sqrt(t1046 * t115)
      t1053 = 0.1D1 / (-t869 - t1039 + t1040 + t1042 + t1044 - t1045 - t
     #127 - t103 + 0.2D1 * t126 * t1048 * z - t14)
      t1055 = t886 * t888
      t1058 = log(-0.4D1 * t113 * t1055)
      t1059 = t1058 * t3
      t1061 = t3 * t1021
      t1062 = t1058 ** 2
      t1063 = t1062 * t3
      t1070 = t870 * t1030
      t1075 = t3 * t1030
      t1083 = t870 * t1034 * t1053 - t3 * t1034
      t1090 = t159 * t885
      t1091 = t917 * t887
      t1094 = log(-0.4D1 * t1090 * t1091)
      t1099 = t1094 ** 2
      t1102 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1103 = t1099 * t1094
      t1127 = log(0.4D1 * t884 * t886 * t888 * t116)
      t1128 = t1127 * t870
      t1134 = log(-0.4D1 * t884 * t1055)
      t1135 = t1134 * t3
      t1151 = log(-0.4D1 * t245 * t1055)
      t1152 = t1151 * t3
      t1154 = t1151 ** 2
      t1155 = t1154 * t3
      t1167 = t8 * t3
      t1174 = -(-0.90D2 * t100 * t8 * ((t870 * t1021 - t1029 * t1030 + t
     #1033 * t1034 / 0.2D1) * t1053 + t1059 * t1030 - t1061 - t1063 * t1
     #034 / 0.2D1) + 0.180D3 * t28 * t9 * ((t1070 - t1029 * t1034) * t10
     #53 + t1059 * t1034 - t1075) + t47 * t9 * t1083) * t154 * t156 / 0.
     #1440D4 - (t59 * t9 * (t1094 * t1034 - t1030) - 0.90D2 * t4 * t9 * 
     #(-t1099 * t1030 / 0.2D1 - t1102 + t1103 * t1034 / 0.6D1 + t1094 * 
     #t1021) - t38 * t9 * t1034 + 0.180D3 * t66 * t9 * (-t1099 * t1034 /
     # 0.2D1 + t1094 * t1030 - t1021)) * t156 / 0.1440D4 + (-0.90D2 * t1
     #00 * t8 * (-(t1070 - t1128 * t1034) * t1053 - t1135 * t1034 + t107
     #5) - 0.180D3 * t28 * t9 * t1083) * t154 * t230 / 0.720D3 + (-0.90D
     #2 * t100 * t8 * (-t1152 * t1030 + t1155 * t1034 / 0.2D1 + t1061) +
     # 0.180D3 * t28 * t9 * (-t1152 * t1034 + t1075) + t940 * t1167 * t1
     #034) * t229 * t156 / 0.720D3
      t1175 = FJET(XB1, XB2, s, 0.0D0, -t875, -t1019, 0.0D0, t1020, t117
     #4)
      t1177 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1180 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1182 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1189 = t3 * t1180
      t1194 = t870 * t1182
      t1198 = t3 * t1182
      t1207 = t870 * t1177 * t1053 - t3 * t1177
      t1220 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1274 = -(-0.90D2 * t100 * t8 * (-t1063 * t1177 / 0.2D1 + (t870 * 
     #t1180 - t1029 * t1182 + t1033 * t1177 / 0.2D1) * t1053 + t1059 * t
     #1182 - t1189) + 0.180D3 * t28 * t9 * ((t1194 - t1029 * t1177) * t1
     #053 - t1198 + t1059 * t1177) + t47 * t9 * t1207) * t154 * t156 / 0
     #.1440D4 - (t59 * t9 * (-t1182 + t1094 * t1177) - 0.90D2 * t4 * t9 
     #* (-t1099 * t1182 / 0.2D1 - t1220 + t1094 * t1180 + t1103 * t1177 
     #/ 0.6D1) - t38 * t9 * t1177 + 0.180D3 * t66 * t9 * (-t1099 * t1177
     # / 0.2D1 - t1180 + t1094 * t1182)) * t156 / 0.1440D4 + (-0.90D2 * 
     #t100 * t8 * (-(t1194 - t1128 * t1177) * t1053 + t1198 - t1135 * t1
     #177) - 0.180D3 * t28 * t9 * t1207) * t154 * t230 / 0.720D3 + (-0.9
     #0D2 * t100 * t8 * (-t1152 * t1182 + t1155 * t1177 / 0.2D1 + t1189)
     # + 0.180D3 * t28 * t9 * (-t1152 * t1177 + t1198) + t940 * t1167 * 
     #t1177) * t229 * t156 / 0.720D3
      t1275 = FJET(XB1, XB2, s, 0.0D0, -t1019, -t875, 0.0D0, t1020, t127
     #4)
      t1277 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1279 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1280 = t962 * t1279
      t1285 = t991 * t1277
      t1292 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1311 = (-0.90D2 * t100 * t8 * (-t1003 * t1277 + t1280) + 0.180D3 
     #* t907 * t1285) * t154 * t230 / 0.720D3 - (-0.90D2 * t100 * t8 * (
     #-t962 * t1292 - t974 * t1277 / 0.2D1 + t978 * t1279) + 0.180D3 * t
     #28 * t9 * (-t1280 + t978 * t1277) - t940 * t1285) * t154 * t229 / 
     #0.1440D4
      t1312 = FJET(XB1, XB2, s, 0.0D0, -t955, 0.0D0, t953, 0.0D0, t1311)
      t1314 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1315 = t8 * t1314
      t1318 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1319 = t9 * t1318
      t1322 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1323 = t9 * t1322
      t1327 = t8 * t1322
      t1332 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1333 = t8 * t1332
      t1342 = t8 * t1318
      t1353 = t3 * t1314
      t1368 = t3 * t1318
      t1374 = t1322 * t133
      t1375 = t3 * t1322
      t1407 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10
     #D1, x4)
      t1408 = t3 * t1407
      t1413 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10
     #D1, x4)
      t1419 = t3 * t1413
      t1420 = -t1375 - t1374 + t1419
      t1434 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10
     #D1, x4)
      t1435 = t3 * t1434
      t1453 = rrgq2qgh63J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1500 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, 0.10
     #D1, x4)
      t1532 = ((-0.90D2 * t100 * t1315 + 0.180D3 * t28 * t1319 + t47 * t
     #1323) * t291 - 0.90D2 * t100 * t1327 * t300 + (t47 * t1319 - 0.90D
     #2 * t100 * t1333 + t80 * t1323 + 0.180D3 * t28 * t9 * t1314) * t31
     #2 + (-0.90D2 * t100 * t1342 + 0.180D3 * t28 * t1323) * t322) * t15
     #4 / 0.2880D4 - (-0.90D2 * t100 * t8 * (-t107 * t1318 + t1353 + t11
     #0 * t1322 / 0.2D1 + (-t120 * t1318 + t1314 + t122 * t1322 / 0.2D1)
     # * t133) + 0.180D3 * t28 * t9 * ((t1318 - t120 * t1322) * t133 + t
     #1368 - t107 * t1322) + t47 * t9 * (t1374 + t1375)) * t154 * t156 /
     # 0.1440D4 - (t59 * t9 * (-t162 * t1322 + t1318) - 0.90D2 * t4 * t9
     # * (-t162 * t1314 + t167 * t1318 / 0.2D1 + t1332 - t170 * t1322 / 
     #0.6D1) + t38 * t1323 + 0.180D3 * t66 * t9 * (-t162 * t1318 + t1314
     # + t167 * t1322 / 0.2D1)) * t156 / 0.1440D4 + (-0.90D2 * t100 * t8
     # * (-t1368 + t1408 + t204 * t1322 - (t1318 - t197 * t1322) * t133 
     #- t215 * t1413) + 0.180D3 * t28 * t9 * t1420) * t154 * t230 / 0.72
     #0D3 + (-0.90D2 * t100 * t8 * (t253 * t1413 / 0.2D1 - t250 * t1407 
     #- t1353 + t237 * t1318 - t240 * t1322 / 0.2D1 + t1435) + 0.180D3 *
     # t28 * t9 * (t237 * t1322 - t250 * t1413 - t1368 + t1408) + t47 * 
     #t9 * (-t1375 + t1419)) * t229 * t156 / 0.720D3 + t4 * t9 * t1453 /
     # 0.32D2 - t95 * t1327 / 0.2880D4 - (-0.90D2 * t100 * t8 * (-t330 *
     # t1318 + t1353 - t344 * t1413 / 0.2D1 - t1435 + t341 * t1407 + (-t
     #349 * t1318 + t1314 + t351 * t1322 / 0.2D1) * t133 + t333 * t1322 
     #/ 0.2D1) + 0.180D3 * t28 * t9 * (t341 * t1413 - t330 * t1322 + (t1
     #318 - t349 * t1322) * t133 + t1368 - t1408) - t47 * t9 * t1420) * 
     #t154 * t229 / 0.1440D4 + (t59 * t9 * (t1407 - t1318 - t379 * t1413
     # + t383 * t1322) - 0.90D2 * t4 * t9 * (-t389 * t1318 / 0.2D1 - t37
     #9 * t1434 + t394 * t1407 / 0.2D1 + t1500 - t1332 - t397 * t1413 / 
     #0.6D1 + t383 * t1314 + t390 * t1322 / 0.6D1) + t38 * t9 * (t1413 -
     # t1322) + 0.180D3 * t66 * t9 * (-t379 * t1407 + t383 * t1318 - t13
     #14 + t394 * t1413 / 0.2D1 + t1434 - t389 * t1322 / 0.2D1)) * t229 
     #/ 0.1440D4 - t71 * t1333 / 0.2880D4 - t50 * t1342 / 0.2880D4 - t61
     # * t1315 / 0.2880D4
      t1533 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1532)
      t1535 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1536 = t897 * t1535
      t1537 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1543 = t908 * t1537
      t1550 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1568 = (-0.90D2 * t100 * t8 * (t1536 - t898 * t1537) + 0.180D3 * 
     #t907 * t1543) * t154 * t230 / 0.720D3 + (-0.90D2 * t100 * t8 * (-t
     #923 * t1535 + t897 * t1550 + t926 * t1537 / 0.2D1) + 0.180D3 * t28
     # * t9 * (-t923 * t1537 + t1536) + t940 * t1543) * t229 * t156 / 0.
     #720D3
      t1569 = FJET(XB1, XB2, s, t878, -t875, t873, 0.0D0, -t883, t1568)
      t1571 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1572 = t897 * t1571
      t1573 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1579 = t908 * t1573
      t1585 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1604 = (-0.90D2 * t100 * t8 * (t1572 - t898 * t1573) + 0.180D3 * 
     #t907 * t1579) * t154 * t230 / 0.720D3 + (-0.90D2 * t100 * t8 * (t8
     #97 * t1585 + t926 * t1573 / 0.2D1 - t923 * t1571) + 0.180D3 * t28 
     #* t9 * (t1572 - t923 * t1573) + t940 * t1579) * t229 * t156 / 0.72
     #0D3
      t1605 = FJET(XB1, XB2, s, t873, 0.0D0, t878, -t875, -t883, t1604)
      t1607 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1609 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1612 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1618 = t962 * t1612
      t1624 = t991 * t1609
      t1641 = -(-0.90D2 * t100 * t8 * (-t962 * t1607 - t974 * t1609 / 0.
     #2D1 + t978 * t1612) + 0.180D3 * t28 * t9 * (-t1618 + t978 * t1609)
     # - t940 * t1624) * t154 * t229 / 0.1440D4 + (-0.90D2 * t100 * t8 *
     # (t1618 - t1003 * t1609) + 0.180D3 * t907 * t1624) * t154 * t230 /
     # 0.720D3
      t1642 = FJET(XB1, XB2, s, t953, 0.0D0, -t955, 0.0D0, 0.0D0, t1641)
      t1647 = t115 * s * t1 * t874 * t951
      t1648 = t2 * x1
      t1650 = Sqrt(-t1046 * t956)
      t1651 = t126 * t1650
      t1657 = t1648 * x2 * (-x3 + t949 - z + t127 - x1 + t1038 + t869 - 
     #t1039 + 0.2D1 * t1651) * t871 * t951
      t1658 = t875 * t952
      t1659 = t191 * t869
      t1660 = t191 * x1
      t1667 = t1648 * (-x2 + t949 - t1659 + t1660 + t191 * z + 0.1D1 - x
     #3 + 0.2D1 * t1651 * x2) * t871 * t951
      t1672 = log(-0.4D1 * t191 * t1090 * t1091 * t998)
      t1673 = t1672 * t870
      t1674 = x2 * t102
      t1678 = t1039 - t1042 - t1044 + t1045 + t1674 - t1040 + t127 + t10
     #3 + t869 + t895 + 0.2D1 * t1651 * t895 + t14 - t949 * z
      t1697 = t949 * x1 - t103 * x2 - t894 * t14 - 0.2D1 * t1674 * z + t
     #1674 * t14 - 0.2D1 * t1651 * z - t1660 + t1041 * t894 + 0.2D1 * t1
     #03 * x2 * z - t103 * t14 * x2 - 0.2D1 * t1651 * t894 + t1659 - 0.2
     #D1 * t949 * t869
      t1699 = 0.1D1 / (t1678 + t1697)
      t1700 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t1701 = t1699 * t1700
      t1703 = t870 * t1699
      t1704 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t1710 = t8 * t870
      t1714 = -0.90D2 * t100 * t8 * (t1673 * t1701 - t1703 * t1704) - 0.
     #180D3 * t907 * t1710 * t1701
      t1718 = FJET(XB1, XB2, s, t1647, t1657, -t1658, -t1667, -t883, t17
     #14 * t154 * t230 / 0.720D3)
      t1721 = t154 * t229 * t156
      t1724 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t1725 = t1699 * t1724
      t1727 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t1736 = -0.90D2 * t100 * t8 * (t1673 * t1725 - t1703 * t1727) - 0.
     #180D3 * t907 * t1710 * t1725
      t1740 = FJET(XB1, XB2, s, t1657, t1647, -t1667, -t1658, -t883, t17
     #36 * t154 * t230 / 0.720D3)
      t1744 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1746 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1748 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1756 = t3 * t1744
      t1761 = t870 * t1746
      t1765 = t3 * t1746
      t1774 = -t3 * t1748 + t870 * t1748 * t1053
      t1786 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1841 = -(-0.90D2 * t100 * t8 * ((t870 * t1744 - t1029 * t1746 + t
     #1033 * t1748 / 0.2D1) * t1053 + t1059 * t1746 - t1063 * t1748 / 0.
     #2D1 - t1756) + 0.180D3 * t28 * t9 * ((t1761 - t1029 * t1748) * t10
     #53 - t1765 + t1059 * t1748) + t47 * t9 * t1774) * t154 * t156 / 0.
     #1440D4 - (t59 * t9 * (t1094 * t1748 - t1746) - 0.90D2 * t4 * t9 * 
     #(t1094 * t1744 - t1786 - t1099 * t1746 / 0.2D1 + t1103 * t1748 / 0
     #.6D1) - t38 * t9 * t1748 + 0.180D3 * t66 * t9 * (-t1099 * t1748 / 
     #0.2D1 - t1744 + t1094 * t1746)) * t156 / 0.1440D4 + (-0.90D2 * t10
     #0 * t8 * (-(t1761 - t1128 * t1748) * t1053 + t1765 - t1135 * t1748
     #) - 0.180D3 * t28 * t9 * t1774) * t154 * t230 / 0.720D3 + (-0.90D2
     # * t100 * t8 * (-t1152 * t1746 + t1155 * t1748 / 0.2D1 + t1756) + 
     #0.180D3 * t28 * t9 * (t1765 - t1152 * t1748) + t940 * t1167 * t174
     #8) * t229 * t156 / 0.720D3
      t1842 = FJET(XB1, XB2, s, -t875, 0.0D0, 0.0D0, -t1019, t1020, t184
     #1)
      t1844 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1846 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1847 = t897 * t1846
      t1852 = t908 * t1844
      t1858 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, x1, t206, 0.10D1,
     # x4)
      t1877 = (-0.90D2 * t100 * t8 * (-t898 * t1844 + t1847) + 0.180D3 *
     # t907 * t1852) * t154 * t230 / 0.720D3 + (-0.90D2 * t100 * t8 * (t
     #897 * t1858 + t926 * t1844 / 0.2D1 - t923 * t1846) + 0.180D3 * t28
     # * t9 * (t1847 - t923 * t1844) + t940 * t1852) * t229 * t156 / 0.7
     #20D3
      t1878 = FJET(XB1, XB2, s, -t875, t878, 0.0D0, t873, -t883, t1877)
      t1880 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1882 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1884 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1892 = t3 * t1880
      t1897 = t870 * t1882
      t1901 = t3 * t1882
      t1910 = -t3 * t1884 + t870 * t1884 * t1053
      t1923 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1977 = -(-0.90D2 * t100 * t8 * ((t870 * t1880 - t1029 * t1882 + t
     #1033 * t1884 / 0.2D1) * t1053 + t1059 * t1882 - t1063 * t1884 / 0.
     #2D1 - t1892) + 0.180D3 * t28 * t9 * ((t1897 - t1029 * t1884) * t10
     #53 - t1901 + t1059 * t1884) + t47 * t9 * t1910) * t154 * t156 / 0.
     #1440D4 - (t59 * t9 * (t1094 * t1884 - t1882) - 0.90D2 * t4 * t9 * 
     #(-t1099 * t1882 / 0.2D1 - t1923 + t1094 * t1880 + t1103 * t1884 / 
     #0.6D1) - t38 * t9 * t1884 + 0.180D3 * t66 * t9 * (-t1099 * t1884 /
     # 0.2D1 - t1880 + t1094 * t1882)) * t156 / 0.1440D4 + (-0.90D2 * t1
     #00 * t8 * (t1901 - (t1897 - t1128 * t1884) * t1053 - t1135 * t1884
     #) - 0.180D3 * t28 * t9 * t1910) * t154 * t230 / 0.720D3 + (-0.90D2
     # * t100 * t8 * (-t1152 * t1882 + t1155 * t1884 / 0.2D1 + t1892) + 
     #0.180D3 * t28 * t9 * (t1901 - t1152 * t1884) + t940 * t1167 * t188
     #4) * t229 * t156 / 0.720D3
      t1978 = FJET(XB1, XB2, s, -t1019, 0.0D0, 0.0D0, -t875, t1020, t197
     #7)
      t1980 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1982 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1985 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t206, -t95
     #4, x4)
      t1991 = t962 * t1985
      t1997 = t991 * t1982
      t2014 = -(-0.90D2 * t100 * t8 * (-t962 * t1980 - t974 * t1982 / 0.
     #2D1 + t978 * t1985) + 0.180D3 * t28 * t9 * (-t1991 + t978 * t1982)
     # - t940 * t1997) * t154 * t229 / 0.1440D4 + (-0.90D2 * t100 * t8 *
     # (t1991 - t1003 * t1982) + 0.180D3 * t907 * t1997) * t154 * t230 /
     # 0.720D3
      t2015 = FJET(XB1, XB2, s, -t955, 0.0D0, t953, 0.0D0, 0.0D0, t2014)
      t2017 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t2018 = t1699 * t2017
      t2020 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t2029 = -0.90D2 * t100 * t8 * (t1673 * t2018 - t1703 * t2020) - 0.
     #180D3 * t907 * t1710 * t2018
      t2033 = FJET(XB1, XB2, s, -t1667, -t1658, t1657, t1647, -t883, t20
     #29 * t154 * t230 / 0.720D3)
      t2037 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t2038 = t1699 * t2037
      t2040 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, t206, -t954, 
     #x4)
      t2049 = -0.90D2 * t100 * t8 * (t1673 * t2038 - t1703 * t2040) - 0.
     #180D3 * t907 * t1710 * t2038
      t2053 = FJET(XB1, XB2, s, -t1658, -t1667, t1647, t1657, -t883, t20
     #49 * t154 * t230 / 0.720D3)
      rrgq2qght6s2e1 = t424 * t423 + t645 * t644 + t866 * t865 + t947 * 
     #t946 + t1016 * t1015 + t1175 * t1174 + t1275 * t1274 + t1312 * t13
     #11 + t1533 * t1532 + t1569 * t1568 + t1605 * t1604 + t1642 * t1641
     # + t1718 * t1714 * t1721 / 0.720D3 + t1740 * t1736 * t1721 / 0.720
     #D3 + t1842 * t1841 + t1878 * t1877 + t1978 * t1977 + t2015 * t2014
     # + t2033 * t2029 * t1721 / 0.720D3 + t2053 * t2049 * t1721 / 0.720
     #D3

      end function



      doubleprecision function rrgq2qght6s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = t7 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t22 = log(0.4D1 * t13 * t19)
      t23 = t22 ** 2
      t24 = 0.1D1 / z
      t26 = -0.1D1 + x3
      t27 = 0.1D1 / t26
      t28 = t19 * t27
      t31 = log(-0.4D1 * t13 * t28)
      t32 = t31 ** 2
      t33 = cos(t14)
      t34 = x3 * z
      t36 = Sqrt(-t34 * t26)
      t40 = 0.1D1 / (-z - x3 + 0.2D1 * t33 * t36)
      t43 = -t23 * t24 / 0.2D1 - t32 * t40 / 0.2D1
      t47 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t48 = t7 * t47
      t51 = 0.3141592653589793D1 * lh
      t52 = t3 * t7
      t53 = t52 * t8
      t59 = t22 * t24 + t31 * t40
      t61 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t62 = t7 * t61
      t68 = lh ** 2
      t70 = 0.3141592653589793D1 ** 2
      t72 = -0.180D3 * t68 + 0.30D2 * t70
      t73 = 0.3141592653589793D1 * t72
      t76 = -t24 - t40
      t79 = 0.1D1 / x3
      t82 = t12 * t16
      t83 = t82 * t18
      t85 = log(0.4D1 * t83)
      t86 = t85 * t24
      t89 = t85 ** 2
      t90 = t89 * t24
      t93 = 0.3141592653589793D1 * t24
      t94 = t93 * t72
      t96 = (-0.180D3 * t86 * t51 - 0.45D2 * t90 * 0.3141592653589793D1 
     #+ t94) * t3
      t99 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t117 = (0.90D2 * t90 * t51 + t93 * (-0.60D2 * lh * t70 + 0.2884936
     #567583026D3 + 0.120D3 * t68 * lh) + 0.15D2 * t89 * t85 * t24 * 0.3
     #141592653589793D1 - t86 * t73) * t3
      t120 = t93 * lh
      t125 = (0.180D3 * t120 + 0.90D2 * t86 * 0.3141592653589793D1) * t3
      t128 = t24 * t47
      t129 = x2 ** 2
      t130 = x3 * t129
      t133 = log(0.4D1 * t130 * t83)
      t134 = t133 * t24
      t136 = 0.1D1 - x2
      t137 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t138 = t24 * t137
      t139 = t130 * t12
      t140 = -t136
      t141 = t19 * t140
      t144 = log(-0.4D1 * t139 * t141)
      t145 = t144 * t24
      t146 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t150 = log(-0.4D1 * t139 * t28)
      t158 = t8 * t40
      t159 = t24 * t146
      t160 = t24 * t8
      t161 = t158 - t159 + t160
      t167 = 0.1D1 / x2
      t170 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t171 = t12 * t129
      t174 = log(-0.4D1 * t171 * t141)
      t176 = t174 ** 2
      t181 = log(0.4D1 * t171 * t19)
      t183 = t181 ** 2
      t202 = x1 ** 2
      t203 = x3 * t202
      t206 = log(0.4D1 * t203 * t83)
      t207 = t206 * t24
      t209 = t203 * t16
      t210 = t12 * t18
      t214 = log(-0.4D1 * t209 * t210 * t27)
      t228 = 0.1D1 / x1
      t231 = t4 * t7
      t234 = t167 * t228
      t238 = t129 * t202
      t241 = log(0.4D1 * t238 * t83)
      t242 = t241 * t24
      t244 = t238 * t16
      t248 = log(-0.4D1 * t244 * t210 * t140)
      t249 = t248 * t24
      t263 = t202 * t16
      t266 = log(0.4D1 * t263 * t210)
      t268 = t266 ** 2
      t284 = (-0.90D2 * t4 * t9 * t43 + (-0.90D2 * t4 * t48 + 0.180D3 * 
     #t51 * t53) * t59 + (-0.90D2 * t4 * t62 + 0.180D3 * t51 * t52 * t47
     # + t73 * t53) * t76) * t79 / 0.2880D4 - t96 * t48 / 0.2880D4 + t93
     # * t52 * t99 / 0.32D2 - t117 * t9 / 0.2880D4 - t125 * t62 / 0.2880
     #D4 - (-0.90D2 * t4 * t7 * (t128 - t134 * t8 - t138 + t145 * t146 +
     # (t47 - t150 * t8) * t40) + 0.180D3 * t51 * t52 * t161) * t79 * t1
     #67 / 0.1440D4 + (-0.90D2 * t93 * t52 * (t170 - t174 * t137 + t176 
     #* t146 / 0.2D1 - t61 + t181 * t47 - t183 * t8 / 0.2D1) + 0.180D3 *
     # t120 * t52 * (-t174 * t146 + t137 - t47 + t181 * t8) + t94 * t52 
     #* (t146 - t8)) * t167 / 0.1440D4 - (-0.90D2 * t4 * t7 * (-t207 * t
     #8 + t128 + (t47 - t214 * t8) * t40) + 0.180D3 * t51 * t52 * (t158 
     #+ t160)) * t79 * t228 / 0.1440D4 + t231 * t161 * t79 * t234 / 0.8D
     #1 + (-0.90D2 * t4 * t7 * (-t128 + t242 * t8 + t138 - t249 * t146) 
     #+ 0.180D3 * t51 * t52 * (t159 - t160)) * t167 * t228 / 0.720D3 - (
     #-0.90D2 * t93 * t52 * (-t266 * t47 + t268 * t8 / 0.2D1 + t61) + 0.
     #180D3 * t120 * t52 * (t47 - t266 * t8) + t94 * t53) * t228 / 0.144
     #0D4
      t285 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t284)
      t287 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t288 = t7 * t287
      t292 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t293 = t7 * t292
      t296 = t52 * t287
      t301 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t302 = t7 * t301
      t316 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t324 = t24 * t292
      t326 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t327 = t24 * t326
      t328 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t337 = t24 * t287
      t338 = t24 * t328
      t339 = t287 * t40
      t340 = t337 - t338 + t339
      t349 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t422 = (-0.90D2 * t4 * t288 * t43 + (-0.90D2 * t4 * t293 + 0.180D3
     # * t51 * t296) * t59 + (-0.90D2 * t4 * t302 + 0.180D3 * t51 * t52 
     #* t292 + t73 * t296) * t76) * t79 / 0.2880D4 - t96 * t293 / 0.2880
     #D4 + t93 * t52 * t316 / 0.32D2 - t117 * t288 / 0.2880D4 - t125 * t
     #302 / 0.2880D4 - (-0.90D2 * t4 * t7 * (t324 - t134 * t287 - t327 +
     # t145 * t328 + (t292 - t150 * t287) * t40) + 0.180D3 * t51 * t52 *
     # t340) * t79 * t167 / 0.1440D4 + (-0.90D2 * t93 * t52 * (-t174 * t
     #326 + t349 + t176 * t328 / 0.2D1 + t181 * t292 - t301 - t183 * t28
     #7 / 0.2D1) + 0.180D3 * t120 * t52 * (t326 - t174 * t328 - t292 + t
     #181 * t287) + t94 * t52 * (-t287 + t328)) * t167 / 0.1440D4 - (-0.
     #90D2 * t4 * t7 * (-t207 * t287 + t324 + (t292 - t214 * t287) * t40
     #) + 0.180D3 * t51 * t52 * (t339 + t337)) * t79 * t228 / 0.1440D4 +
     # t231 * t340 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t324 + 
     #t242 * t287 + t327 - t249 * t328) + 0.180D3 * t51 * t52 * (-t337 +
     # t338)) * t167 * t228 / 0.720D3 - (-0.90D2 * t93 * t52 * (-t266 * 
     #t292 + t301 + t268 * t287 / 0.2D1) + 0.180D3 * t120 * t52 * (t292 
     #- t266 * t287) + t94 * t296) * t228 / 0.1440D4
      t423 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t422)
      t425 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t426 = t7 * t425
      t430 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t431 = t7 * t430
      t434 = t52 * t425
      t439 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t440 = t7 * t439
      t454 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t462 = t24 * t430
      t464 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t465 = t24 * t464
      t466 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t475 = t24 * t466
      t476 = t425 * t40
      t477 = t24 * t425
      t478 = -t475 + t476 + t477
      t488 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t560 = (-0.90D2 * t4 * t426 * t43 + (-0.90D2 * t4 * t431 + 0.180D3
     # * t51 * t434) * t59 + (-0.90D2 * t4 * t440 + 0.180D3 * t51 * t52 
     #* t430 + t73 * t434) * t76) * t79 / 0.2880D4 - t96 * t431 / 0.2880
     #D4 + t93 * t52 * t454 / 0.32D2 - t117 * t426 / 0.2880D4 - t125 * t
     #440 / 0.2880D4 - (-0.90D2 * t4 * t7 * (t462 - t134 * t425 - t465 +
     # t145 * t466 + (t430 - t150 * t425) * t40) + 0.180D3 * t51 * t52 *
     # t478) * t79 * t167 / 0.1440D4 + (-0.90D2 * t93 * t52 * (-t174 * t
     #464 - t439 + t181 * t430 + t488 + t176 * t466 / 0.2D1 - t183 * t42
     #5 / 0.2D1) + 0.180D3 * t120 * t52 * (t464 - t174 * t466 - t430 + t
     #181 * t425) + t94 * t52 * (-t425 + t466)) * t167 / 0.1440D4 - (-0.
     #90D2 * t4 * t7 * (t462 - t207 * t425 + (t430 - t214 * t425) * t40)
     # + 0.180D3 * t51 * t52 * (t476 + t477)) * t79 * t228 / 0.1440D4 + 
     #t231 * t478 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t462 + t
     #242 * t425 + t465 - t249 * t466) + 0.180D3 * t51 * t52 * (-t477 + 
     #t475)) * t167 * t228 / 0.720D3 - (-0.90D2 * t93 * t52 * (t439 - t2
     #66 * t430 + t268 * t425 / 0.2D1) + 0.180D3 * t120 * t52 * (-t266 *
     # t425 + t430) + t94 * t434) * t228 / 0.1440D4
      t561 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t560)
      t564 = x1 * z
      t565 = -z - x1 + t564
      t566 = 0.1D1 / t565
      t568 = t2 * x1 * t140 * t566
      t569 = -0.1D1 + x1
      t570 = t2 * t569
      t573 = x2 * s * t1 * x1
      t574 = s * t17
      t577 = x1 * t569 * t566
      t578 = t574 * t140 * t577
      t579 = x2 * x1
      t580 = t579 * z
      t582 = 0.1D1 / (t580 - t579 - z)
      t583 = t7 * t582
      t584 = t4 * t583
      t585 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1, 
     #x4)
      t590 = 0.1D1 / t10
      t591 = t16 * t590
      t593 = t18 * t566
      t594 = t569 ** 2
      t599 = log(0.4D1 * t238 * t591 * t593 * t594 * t140)
      t600 = t599 * t582
      t602 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1, 
     #x4)
      t608 = t51 * t3
      t616 = -t584 * t585 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * (-
     #t600 * t585 + t582 * t602) + 0.180D3 * t608 * t583 * t585) * t167 
     #* t228 / 0.720D3
      t617 = FJET(XB1, XB2, s, 0.0D0, t568, -t570, t573, -t578, t616)
      t619 = x2 * x3
      t620 = 0.1D1 - x3 + t619
      t621 = 0.1D1 / t620
      t622 = t619 * t621
      t623 = t2 * t622
      t624 = t26 * t621
      t625 = t2 * t624
      t626 = t140 * t26
      t628 = Sqrt(t34 * t626)
      t632 = 0.1D1 / (-z - x3 + t619 + 0.2D1 * t33 * t628)
      t633 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t624
     #, x4)
      t637 = t620 ** 2
      t643 = log(0.4D1 * t130 * t82 * t18 * t140 * t26 / t637)
      t644 = t643 * t632
      t645 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t624
     #, x4)
      t651 = t7 * t632
      t659 = t4 * t651
      t664 = -(-0.90D2 * t4 * t7 * (-t632 * t633 + t644 * t645) - 0.180D
     #3 * t608 * t651 * t645) * t79 * t167 / 0.1440D4 - t659 * t645 * t7
     #9 * t234 / 0.8D1
      t665 = FJET(XB1, XB2, s, 0.0D0, t623, 0.0D0, -t625, 0.0D0, t664)
      t668 = t2 * x1 * t566
      t669 = t574 * t577
      t670 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t677 = log(0.4D1 * t203 * t591 * t593 * t594 * t27)
      t678 = t677 * t565
      t679 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t682 = x3 * x1
      t683 = t682 * z
      t684 = x1 * t10
      t685 = x3 * t10
      t686 = t685 * x1
      t688 = 0.2D1 * t203 * z
      t689 = t203 * t10
      t690 = x3 * t565
      t692 = Sqrt(t690 * t26)
      t697 = 0.1D1 / (-t564 - t683 + t684 + t686 + t688 - t689 - t34 - t
     #203 + 0.2D1 * t33 * t692 * z - t10)
      t701 = t590 * t18 * t566 * t594
      t704 = log(-0.4D1 * t209 * t701)
      t705 = t704 * t24
      t707 = t24 * t670
      t715 = t565 * t679 * t697 - t24 * t679
      t730 = log(-0.4D1 * t244 * t701)
      t731 = t730 * t24
      t737 = t7 * t24
      t738 = t737 * t679
      t749 = log(-0.4D1 * t263 * t590 * t593 * t594)
      t750 = t749 ** 2
      t754 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t764 = t73 * t3
      t769 = -(-0.90D2 * t4 * t7 * ((t565 * t670 - t678 * t679) * t697 +
     # t705 * t679 - t707) + 0.180D3 * t51 * t52 * t715) * t79 * t228 / 
     #0.1440D4 + t231 * t715 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 *
     # (-t731 * t679 + t707) + 0.180D3 * t608 * t738) * t167 * t228 / 0.
     #720D3 - (-0.90D2 * t93 * t52 * (-t750 * t679 / 0.2D1 + t749 * t670
     # - t754) + 0.180D3 * t120 * t52 * (t749 * t679 - t670) - t764 * t7
     #38) * t228 / 0.1440D4
      t770 = FJET(XB1, XB2, s, 0.0D0, -t570, -t668, 0.0D0, t669, t769)
      t772 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t774 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t778 = t24 * t772
      t787 = t565 * t774 * t697 - t24 * t774
      t805 = t737 * t774
      t814 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t829 = -(-0.90D2 * t4 * t7 * ((t565 * t772 - t678 * t774) * t697 -
     # t778 + t705 * t774) + 0.180D3 * t51 * t52 * t787) * t79 * t228 / 
     #0.1440D4 + t231 * t787 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 *
     # (-t731 * t774 + t778) + 0.180D3 * t608 * t805) * t167 * t228 / 0.
     #720D3 - (-0.90D2 * t93 * t52 * (-t750 * t774 / 0.2D1 - t814 + t749
     # * t772) + 0.180D3 * t120 * t52 * (-t772 + t749 * t774) - t764 * t
     #805) * t228 / 0.1440D4
      t830 = FJET(XB1, XB2, s, 0.0D0, -t668, -t570, 0.0D0, t669, t829)
      t832 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t624
     #, x4)
      t834 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t624
     #, x4)
      t851 = -(-0.90D2 * t4 * t7 * (-t632 * t832 + t644 * t834) - 0.180D
     #3 * t608 * t651 * t834) * t79 * t167 / 0.1440D4 - t659 * t834 * t7
     #9 * t234 / 0.8D1
      t852 = FJET(XB1, XB2, s, 0.0D0, -t625, 0.0D0, t623, 0.0D0, t851)
      t854 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t855 = t7 * t854
      t859 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t860 = t7 * t859
      t863 = t52 * t854
      t868 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t869 = t7 * t868
      t881 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t889 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t895 = t24 * t859
      t896 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t897 = t24 * t896
      t902 = t24 * t889
      t903 = t854 * t40
      t904 = t24 * t854
      t905 = -t902 + t903 + t904
      t917 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, 0.10D
     #1, x4)
      t989 = (-0.90D2 * t4 * t855 * t43 + (-0.90D2 * t4 * t860 + 0.180D3
     # * t51 * t863) * t59 + (-0.90D2 * t4 * t869 + 0.180D3 * t51 * t52 
     #* t859 + t73 * t863) * t76) * t79 / 0.2880D4 + t93 * t52 * t881 / 
     #0.32D2 - t125 * t869 / 0.2880D4 - t96 * t860 / 0.2880D4 - (-0.90D2
     # * t4 * t7 * (t145 * t889 - t134 * t854 + (t859 - t150 * t854) * t
     #40 + t895 - t897) + 0.180D3 * t51 * t52 * t905) * t79 * t167 / 0.1
     #440D4 + (-0.90D2 * t93 * t52 * (-t174 * t896 + t181 * t859 - t868 
     #+ t176 * t889 / 0.2D1 + t917 - t183 * t854 / 0.2D1) + 0.180D3 * t1
     #20 * t52 * (t896 - t859 - t174 * t889 + t181 * t854) + t94 * t52 *
     # (t889 - t854)) * t167 / 0.1440D4 - t117 * t855 / 0.2880D4 - (-0.9
     #0D2 * t4 * t7 * ((t859 - t214 * t854) * t40 + t895 - t207 * t854) 
     #+ 0.180D3 * t51 * t52 * (t903 + t904)) * t79 * t228 / 0.1440D4 + t
     #231 * t905 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * (t242 * t85
     #4 - t249 * t889 - t895 + t897) + 0.180D3 * t51 * t52 * (-t904 + t9
     #02)) * t167 * t228 / 0.720D3 - (-0.90D2 * t93 * t52 * (-t266 * t85
     #9 + t868 + t268 * t854 / 0.2D1) + 0.180D3 * t120 * t52 * (-t266 * 
     #t854 + t859) + t94 * t863) * t228 / 0.1440D4
      t990 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t989)
      t992 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1, 
     #x4)
      t998 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1, 
     #x4)
      t1011 = -t584 * t992 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #-t600 * t992 + t582 * t998) + 0.180D3 * t608 * t583 * t992) * t167
     # * t228 / 0.720D3
      t1012 = FJET(XB1, XB2, s, t573, -t570, t568, 0.0D0, -t578, t1011)
      t1014 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1019 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1033 = -t584 * t1014 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * 
     #(t582 * t1019 - t600 * t1014) + 0.180D3 * t608 * t583 * t1014) * t
     #167 * t228 / 0.720D3
      t1034 = FJET(XB1, XB2, s, t568, 0.0D0, t573, -t570, -t578, t1033)
      t1036 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t62
     #4, x4)
      t1038 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t62
     #4, x4)
      t1055 = -(-0.90D2 * t4 * t7 * (-t632 * t1036 + t644 * t1038) - 0.1
     #80D3 * t608 * t651 * t1038) * t79 * t167 / 0.1440D4 - t659 * t1038
     # * t79 * t234 / 0.8D1
      t1056 = FJET(XB1, XB2, s, t623, 0.0D0, -t625, 0.0D0, 0.0D0, t1055)
      t1061 = t26 * s * t1 * t569 * t621
      t1062 = t2 * x1
      t1064 = Sqrt(-t690 * t626)
      t1065 = t33 * t1064
      t1071 = t1062 * x2 * (-x3 + t619 - z + t34 - x1 + t682 + t564 - t6
     #83 + 0.2D1 * t1065) * t566 * t621
      t1072 = t570 * t622
      t1073 = t130 * t564
      t1074 = t130 * x1
      t1081 = t1062 * (-x2 + t619 - t1073 + t1074 + t130 * z + 0.1D1 - x
     #3 + 0.2D1 * t1065 * x2) * t566 * t621
      t1083 = t4 * t7 * t565
      t1084 = x2 * t202
      t1096 = t10 + t1084 + 0.2D1 * t1065 * t580 - t619 * z + t619 * x1 
     #- t203 * x2 - t579 * t10 - 0.2D1 * t1084 * z + t1084 * t10 - 0.2D1
     # * t1065 * z - t1074 - t684 + t34
      t1107 = t203 + t1073 + 0.2D1 * t203 * x2 * z + t685 * t579 - 0.2D1
     # * t619 * t564 - t203 * t10 * x2 + t580 + t689 - t688 - t686 + t68
     #3 + t564 - 0.2D1 * t1065 * t579
      t1109 = 0.1D1 / (t1096 + t1107)
      t1110 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t624, 
     #x4)
      t1113 = t79 * t167 * t228
      t1114 = t1109 * t1110 * t1113
      t1117 = FJET(XB1, XB2, s, t1061, t1071, -t1072, -t1081, -t578, t10
     #83 * t1114 / 0.8D1)
      t1119 = t52 * t565
      t1123 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t624, 
     #x4)
      t1125 = t1109 * t1123 * t1113
      t1128 = FJET(XB1, XB2, s, t1071, t1061, -t1081, -t1072, -t578, t10
     #83 * t1125 / 0.8D1)
      t1133 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1135 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1139 = t24 * t1133
      t1148 = -t24 * t1135 + t565 * t1135 * t697
      t1166 = t737 * t1135
      t1175 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1190 = -(-0.90D2 * t4 * t7 * ((t565 * t1133 - t678 * t1135) * t69
     #7 - t1139 + t705 * t1135) + 0.180D3 * t51 * t52 * t1148) * t79 * t
     #228 / 0.1440D4 + t231 * t1148 * t79 * t234 / 0.8D1 + (-0.90D2 * t4
     # * t7 * (t1139 - t731 * t1135) + 0.180D3 * t608 * t1166) * t167 * 
     #t228 / 0.720D3 - (-0.90D2 * t93 * t52 * (-t750 * t1135 / 0.2D1 - t
     #1175 + t749 * t1133) + 0.180D3 * t120 * t52 * (t749 * t1135 - t113
     #3) - t764 * t1166) * t228 / 0.1440D4
      t1191 = FJET(XB1, XB2, s, -t570, 0.0D0, 0.0D0, -t668, t669, t1190)
      t1193 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1198 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, t136, 0.10D1,
     # x4)
      t1212 = -t584 * t1193 * t79 * t234 / 0.8D1 + (-0.90D2 * t4 * t7 * 
     #(t582 * t1198 - t600 * t1193) + 0.180D3 * t608 * t583 * t1193) * t
     #167 * t228 / 0.720D3
      t1213 = FJET(XB1, XB2, s, -t570, t573, 0.0D0, t568, -t578, t1212)
      t1215 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1217 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1221 = t24 * t1215
      t1230 = -t24 * t1217 + t565 * t1217 * t697
      t1248 = t737 * t1217
      t1257 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1272 = -(-0.90D2 * t4 * t7 * ((t565 * t1215 - t678 * t1217) * t69
     #7 - t1221 + t705 * t1217) + 0.180D3 * t51 * t52 * t1230) * t79 * t
     #228 / 0.1440D4 + t231 * t1230 * t79 * t234 / 0.8D1 + (-0.90D2 * t4
     # * t7 * (t1221 - t731 * t1217) + 0.180D3 * t608 * t1248) * t167 * 
     #t228 / 0.720D3 - (-0.90D2 * t93 * t52 * (-t750 * t1217 / 0.2D1 - t
     #1257 + t749 * t1215) + 0.180D3 * t120 * t52 * (t749 * t1217 - t121
     #5) - t764 * t1248) * t228 / 0.1440D4
      t1273 = FJET(XB1, XB2, s, -t668, 0.0D0, 0.0D0, -t570, t669, t1272)
      t1275 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t62
     #4, x4)
      t1277 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t136, -t62
     #4, x4)
      t1294 = -(-0.90D2 * t4 * t7 * (-t632 * t1275 + t644 * t1277) - 0.1
     #80D3 * t608 * t651 * t1277) * t79 * t167 / 0.1440D4 - t659 * t1277
     # * t79 * t234 / 0.8D1
      t1295 = FJET(XB1, XB2, s, -t625, 0.0D0, t623, 0.0D0, 0.0D0, t1294)
      t1297 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t624, 
     #x4)
      t1299 = t1109 * t1297 * t1113
      t1302 = FJET(XB1, XB2, s, -t1081, -t1072, t1071, t1061, -t578, t10
     #83 * t1299 / 0.8D1)
      t1307 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t136, -t624, 
     #x4)
      t1309 = t1109 * t1307 * t1113
      t1312 = FJET(XB1, XB2, s, -t1072, -t1081, t1061, t1071, -t578, t10
     #83 * t1309 / 0.8D1)
      rrgq2qght6s2e0 = t285 * t284 + t423 * t422 + t561 * t560 + t617 * 
     #t616 + t665 * t664 + t770 * t769 + t830 * t829 + t852 * t851 + t99
     #0 * t989 + t1012 * t1011 + t1034 * t1033 + t1056 * t1055 + t1117 *
     # 0.3141592653589793D1 * t1119 * t1114 / 0.8D1 + t1128 * 0.31415926
     #53589793D1 * t1119 * t1125 / 0.8D1 + t1191 * t1190 + t1213 * t1212
     # + t1273 * t1272 + t1295 * t1294 + t1302 * 0.3141592653589793D1 * 
     #t1119 * t1299 / 0.8D1 + t1312 * 0.3141592653589793D1 * t1119 * t13
     #09 / 0.8D1

      end function



      doubleprecision function rrgq2qght6s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = t7 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t22 = log(0.4D1 * t13 * t19)
      t23 = 0.1D1 / z
      t25 = -0.1D1 + x3
      t30 = log(-0.4D1 * t13 * t19 / t25)
      t31 = cos(t14)
      t32 = x3 * z
      t34 = Sqrt(-t32 * t25)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t34)
      t40 = t22 * t23 + t30 * t38
      t44 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t45 = t7 * t44
      t48 = 0.3141592653589793D1 * lh
      t49 = t3 * t7
      t50 = t49 * t8
      t54 = -t23 - t38
      t57 = 0.1D1 / x3
      t60 = 0.3141592653589793D1 * t23
      t61 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t65 = t60 * lh
      t70 = log(0.4D1 * t12 * t16 * t18)
      t71 = t70 * t23
      t75 = (0.180D3 * t65 + 0.90D2 * t71 * 0.3141592653589793D1) * t3
      t80 = t70 ** 2
      t84 = lh ** 2
      t86 = 0.3141592653589793D1 ** 2
      t91 = (-0.180D3 * t71 * t48 - 0.45D2 * t80 * t23 * 0.3141592653589
     #793D1 + t60 * (-0.180D3 * t84 + 0.30D2 * t86)) * t3
      t94 = t4 * t7
      t95 = t8 * t38
      t96 = 0.1D1 - x2
      t97 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1,
     # x4)
      t98 = t23 * t97
      t99 = t23 * t8
      t102 = 0.1D1 / x2
      t106 = x2 ** 2
      t107 = t12 * t106
      t108 = -t96
      t112 = log(-0.4D1 * t107 * t19 * t108)
      t114 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t117 = log(0.4D1 * t107 * t19)
      t132 = 0.1D1 / x1
      t136 = x1 ** 2
      t137 = t136 * t16
      t141 = log(0.4D1 * t137 * t12 * t18)
      t157 = (-0.90D2 * t4 * t9 * t40 + (-0.90D2 * t4 * t45 + 0.180D3 * 
     #t48 * t50) * t54) * t57 / 0.2880D4 + t60 * t49 * t61 / 0.32D2 - t7
     #5 * t45 / 0.2880D4 - t91 * t9 / 0.2880D4 + t94 * (t95 - t98 + t99)
     # * t57 * t102 / 0.16D2 + (-0.90D2 * t60 * t49 * (-t112 * t97 + t11
     #4 - t44 + t117 * t8) + 0.180D3 * t65 * t49 * (t97 - t8)) * t102 / 
     #0.1440D4 - t94 * (t98 - t99) * t102 * t132 / 0.8D1 - (-0.90D2 * t6
     #0 * t49 * (t44 - t141 * t8) + 0.180D3 * t65 * t50) * t132 / 0.1440
     #D4 + t94 * (t95 + t99) * t57 * t132 / 0.16D2
      t158 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t157)
      t160 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t161 = t7 * t160
      t165 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t166 = t7 * t165
      t169 = t49 * t160
      t177 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t185 = t23 * t160
      t186 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t187 = t23 * t186
      t188 = t160 * t38
      t194 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t228 = (-0.90D2 * t4 * t161 * t40 + (-0.90D2 * t4 * t166 + 0.180D3
     # * t48 * t169) * t54) * t57 / 0.2880D4 + t60 * t49 * t177 / 0.32D2
     # - t75 * t166 / 0.2880D4 - t91 * t161 / 0.2880D4 + t94 * (t185 - t
     #187 + t188) * t57 * t102 / 0.16D2 + (-0.90D2 * t60 * t49 * (t194 -
     # t112 * t186 - t165 + t117 * t160) + 0.180D3 * t65 * t49 * (-t160 
     #+ t186)) * t102 / 0.1440D4 - t94 * (-t185 + t187) * t102 * t132 / 
     #0.8D1 - (-0.90D2 * t60 * t49 * (t165 - t141 * t160) + 0.180D3 * t6
     #5 * t169) * t132 / 0.1440D4 + t94 * (t188 + t185) * t57 * t132 / 0
     #.16D2
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t228)
      t231 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t232 = t7 * t231
      t236 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t237 = t7 * t236
      t240 = t49 * t231
      t248 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t256 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t257 = t23 * t256
      t258 = t231 * t38
      t259 = t23 * t231
      t265 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t299 = (-0.90D2 * t4 * t232 * t40 + (-0.90D2 * t4 * t237 + 0.180D3
     # * t48 * t240) * t54) * t57 / 0.2880D4 + t60 * t49 * t248 / 0.32D2
     # - t75 * t237 / 0.2880D4 - t91 * t232 / 0.2880D4 + t94 * (-t257 + 
     #t258 + t259) * t57 * t102 / 0.16D2 + (-0.90D2 * t60 * t49 * (t265 
     #- t112 * t256 - t236 + t117 * t231) + 0.180D3 * t65 * t49 * (-t231
     # + t256)) * t102 / 0.1440D4 - t94 * (-t259 + t257) * t102 * t132 /
     # 0.8D1 - (-0.90D2 * t60 * t49 * (-t141 * t231 + t236) + 0.180D3 * 
     #t65 * t240) * t132 / 0.1440D4 + t94 * (t258 + t259) * t57 * t132 /
     # 0.16D2
      t300 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t299)
      t303 = x1 * z
      t304 = -z - x1 + t303
      t305 = 0.1D1 / t304
      t307 = t2 * x1 * t108 * t305
      t308 = -0.1D1 + x1
      t309 = t2 * t308
      t312 = x2 * s * t1 * x1
      t313 = s * t17
      t316 = x1 * t308 * t305
      t317 = t313 * t108 * t316
      t318 = x2 * x1
      t321 = 0.1D1 / (t318 * z - t318 - z)
      t322 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, t96, 0.10D1, x
     #4)
      t324 = t102 * t132
      t325 = t321 * t322 * t324
      t328 = FJET(XB1, XB2, s, 0.0D0, t307, -t309, t312, -t317, -t94 * t
     #325 / 0.8D1)
      t333 = x2 * x3
      t335 = 0.1D1 / (0.1D1 - x3 + t333)
      t337 = t2 * t333 * t335
      t338 = t25 * t335
      t339 = t2 * t338
      t342 = Sqrt(t32 * t108 * t25)
      t346 = 0.1D1 / (-z - x3 + t333 + 0.2D1 * t31 * t342)
      t347 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, -t338,
     # x4)
      t349 = t57 * t102
      t350 = t346 * t347 * t349
      t353 = FJET(XB1, XB2, s, 0.0D0, t337, 0.0D0, -t339, 0.0D0, -t94 * 
     #t350 / 0.16D2)
      t359 = t2 * x1 * t305
      t360 = t313 * t316
      t361 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t362 = t23 * t361
      t369 = t308 ** 2
      t373 = log(-0.4D1 * t137 / t10 * t18 * t305 * t369)
      t375 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t380 = t48 * t3
      t381 = t7 * t23
      t394 = x3 * t136
      t400 = Sqrt(x3 * t304 * t25)
      t405 = 0.1D1 / (-t303 - x3 * x1 * z + x1 * t10 + x3 * t10 * x1 + 0
     #.2D1 * t394 * z - t394 * t10 - t32 - t394 + 0.2D1 * t31 * t400 * z
     # - t10)
      t412 = -t94 * t362 * t324 / 0.8D1 - (-0.90D2 * t60 * t49 * (t373 *
     # t361 - t375) - 0.180D3 * t380 * t381 * t361) * t132 / 0.1440D4 + 
     #t94 * (t304 * t361 * t405 - t362) * t57 * t132 / 0.16D2
      t413 = FJET(XB1, XB2, s, 0.0D0, -t309, -t359, 0.0D0, t360, t412)
      t415 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t416 = t23 * t415
      t420 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t439 = -t94 * t416 * t324 / 0.8D1 - (-0.90D2 * t60 * t49 * (-t420 
     #+ t373 * t415) - 0.180D3 * t380 * t381 * t415) * t132 / 0.1440D4 +
     # t94 * (t304 * t415 * t405 - t416) * t57 * t132 / 0.16D2
      t440 = FJET(XB1, XB2, s, 0.0D0, -t359, -t309, 0.0D0, t360, t439)
      t442 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, -t338,
     # x4)
      t444 = t346 * t442 * t349
      t447 = FJET(XB1, XB2, s, 0.0D0, -t339, 0.0D0, t337, 0.0D0, -t94 * 
     #t444 / 0.16D2)
      t452 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t456 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t457 = t7 * t456
      t463 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t464 = t7 * t463
      t467 = t49 * t456
      t475 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t476 = t23 * t475
      t477 = t456 * t38
      t478 = t23 * t456
      t484 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, 0.10D1
     #, x4)
      t520 = t60 * t49 * t452 / 0.32D2 - t91 * t457 / 0.2880D4 + (-0.90D
     #2 * t4 * t457 * t40 + (-0.90D2 * t4 * t464 + 0.180D3 * t48 * t467)
     # * t54) * t57 / 0.2880D4 + t94 * (-t476 + t477 + t478) * t57 * t10
     #2 / 0.16D2 + (-0.90D2 * t60 * t49 * (t484 - t463 - t112 * t475 + t
     #117 * t456) + 0.180D3 * t65 * t49 * (t475 - t456)) * t102 / 0.1440
     #D4 - t75 * t464 / 0.2880D4 - t94 * (-t478 + t476) * t102 * t132 / 
     #0.8D1 - (-0.90D2 * t60 * t49 * (-t141 * t456 + t463) + 0.180D3 * t
     #65 * t467) * t132 / 0.1440D4 + t94 * (t477 + t478) * t57 * t132 / 
     #0.16D2
      t521 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t520)
      t523 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t96, 0.10D1, x
     #4)
      t525 = t321 * t523 * t324
      t528 = FJET(XB1, XB2, s, t312, -t309, t307, 0.0D0, -t317, -t94 * t
     #525 / 0.8D1)
      t533 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, t96, 0.10D1, x
     #4)
      t535 = t321 * t533 * t324
      t538 = FJET(XB1, XB2, s, t307, 0.0D0, t312, -t309, -t317, -t94 * t
     #535 / 0.8D1)
      t543 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, -t338,
     # x4)
      t545 = t346 * t543 * t349
      t548 = FJET(XB1, XB2, s, t337, 0.0D0, -t339, 0.0D0, 0.0D0, -t94 * 
     #t545 / 0.16D2)
      t553 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t554 = t23 * t553
      t559 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t577 = -t94 * t554 * t324 / 0.8D1 - (-0.90D2 * t60 * t49 * (t373 *
     # t553 - t559) - 0.180D3 * t380 * t381 * t553) * t132 / 0.1440D4 + 
     #t94 * (-t554 + t304 * t553 * t405) * t57 * t132 / 0.16D2
      t578 = FJET(XB1, XB2, s, -t309, 0.0D0, 0.0D0, -t359, t360, t577)
      t580 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, t96, 0.10D1, x
     #4)
      t582 = t321 * t580 * t324
      t585 = FJET(XB1, XB2, s, -t309, t312, 0.0D0, t307, -t317, -t94 * t
     #582 / 0.8D1)
      t590 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t591 = t23 * t590
      t596 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t614 = -t94 * t591 * t324 / 0.8D1 - (-0.90D2 * t60 * t49 * (t373 *
     # t590 - t596) - 0.180D3 * t380 * t381 * t590) * t132 / 0.1440D4 + 
     #t94 * (-t591 + t304 * t590 * t405) * t57 * t132 / 0.16D2
      t615 = FJET(XB1, XB2, s, -t359, 0.0D0, 0.0D0, -t309, t360, t614)
      t617 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t96, -t338,
     # x4)
      t619 = t346 * t617 * t349
      t622 = FJET(XB1, XB2, s, -t339, 0.0D0, t337, 0.0D0, 0.0D0, -t94 * 
     #t619 / 0.16D2)
      rrgq2qght6s2em1 = t158 * t157 + t229 * t228 + t300 * t299 - t328 *
     # 0.3141592653589793D1 * t49 * t325 / 0.8D1 - t353 * 0.314159265358
     #9793D1 * t49 * t350 / 0.16D2 + t413 * t412 + t440 * t439 - t447 * 
     #0.3141592653589793D1 * t49 * t444 / 0.16D2 + t521 * t520 - t528 * 
     #0.3141592653589793D1 * t49 * t525 / 0.8D1 - t538 * 0.3141592653589
     #793D1 * t49 * t535 / 0.8D1 - t548 * 0.3141592653589793D1 * t49 * t
     #545 / 0.16D2 + t578 * t577 - t585 * 0.3141592653589793D1 * t49 * t
     #582 / 0.8D1 + t615 * t614 - t622 * 0.3141592653589793D1 * t49 * t6
     #19 / 0.16D2

      end function



      doubleprecision function rrgq2qght6s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = 0.3141592653589793D1 * t3 * t7
      t9 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t10 = 0.1D1 / z
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t16 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t21 = -t10 - 0.1D1 / (-z - x3 + 0.2D1 * t12 * t16)
      t23 = 0.1D1 / x3
      t27 = 0.3141592653589793D1 * t10
      t28 = t27 * t3
      t29 = 0.1D1 - x2
      t30 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1,
     # x4)
      t33 = 0.1D1 / x2
      t37 = t7 * t9
      t38 = 0.1D1 / x1
      t42 = t3 * t7
      t43 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t49 = z ** 2
      t52 = Sin(t11)
      t53 = t52 ** 2
      t55 = t1 ** 2
      t56 = t55 ** 2
      t59 = log(0.4D1 / t49 / z * t53 * t56)
      t64 = (0.180D3 * t27 * lh + 0.90D2 * t59 * t10 * 0.314159265358979
     #3D1) * t3
      t67 = -t8 * t9 * t21 * t23 / 0.32D2 - t28 * t7 * (t30 - t9) * t33 
     #/ 0.16D2 + t28 * t37 * t38 / 0.16D2 + t27 * t42 * t43 / 0.32D2 - t
     #64 * t37 / 0.2880D4
      t68 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t67)
      t70 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t75 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1,
     # x4)
      t81 = t7 * t70
      t85 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t91 = -t8 * t70 * t21 * t23 / 0.32D2 - t28 * t7 * (-t70 + t75) * t
     #33 / 0.16D2 + t28 * t81 * t38 / 0.16D2 + t27 * t42 * t85 / 0.32D2 
     #- t64 * t81 / 0.2880D4
      t92 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t91)
      t94 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t99 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1,
     # x4)
      t105 = t7 * t94
      t109 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t115 = -t8 * t94 * t21 * t23 / 0.32D2 - t28 * t7 * (-t94 + t99) * 
     #t33 / 0.16D2 + t28 * t105 * t38 / 0.16D2 + t27 * t42 * t109 / 0.32
     #D2 - t64 * t105 / 0.2880D4
      t116 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t115)
      t118 = -0.1D1 + x1
      t119 = t2 * t118
      t122 = 0.1D1 / (-z - x1 + x1 * z)
      t124 = t2 * x1 * t122
      t128 = s * t55 * x1 * t118 * t122
      t129 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t134 = FJET(XB1, XB2, s, 0.0D0, -t119, -t124, 0.0D0, t128, -t8 * t
     #10 * t129 * t38 / 0.16D2)
      t137 = t7 * t10
      t142 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t147 = FJET(XB1, XB2, s, 0.0D0, -t124, -t119, 0.0D0, t128, -t8 * t
     #10 * t142 * t38 / 0.16D2)
      t154 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t159 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t29, 0.10D1
     #, x4)
      t165 = t7 * t154
      t169 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t175 = -t8 * t154 * t21 * t23 / 0.32D2 - t28 * t7 * (t159 - t154) 
     #* t33 / 0.16D2 + t28 * t165 * t38 / 0.16D2 + t27 * t42 * t169 / 0.
     #32D2 - t64 * t165 / 0.2880D4
      t176 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t175)
      t178 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t183 = FJET(XB1, XB2, s, -t119, 0.0D0, 0.0D0, -t124, t128, -t8 * t
     #10 * t178 * t38 / 0.16D2)
      t190 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t195 = FJET(XB1, XB2, s, -t124, 0.0D0, 0.0D0, -t119, t128, -t8 * t
     #10 * t190 * t38 / 0.16D2)
      rrgq2qght6s2em2 = t68 * t67 + t92 * t91 + t116 * t115 - t134 * 0.3
     #141592653589793D1 * t3 * t137 * t129 * t38 / 0.16D2 - t147 * 0.314
     #1592653589793D1 * t3 * t137 * t142 * t38 / 0.16D2 + t176 * t175 - 
     #t183 * 0.3141592653589793D1 * t3 * t137 * t178 * t38 / 0.16D2 - t1
     #95 * 0.3141592653589793D1 * t3 * t137 * t190 * t38 / 0.16D2

      end function



      doubleprecision function rrgq2qght6s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = 0.3141592653589793D1 * t3
      t6 = s ** 2
      t9 = 0.1D1 / t1 / t6 / s
      t10 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t11 = t9 * t10
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t34 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t35 = t9 * t34
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t35 /
     # 0.32D2)
      rrgq2qght6s2em3 = t14 * 0.3141592653589793D1 * t3 * t11 / 0.32D2 +
     # t22 * 0.3141592653589793D1 * t3 * t19 / 0.32D2 + t30 * 0.31415926
     #53589793D1 * t3 * t27 / 0.32D2 + t38 * 0.3141592653589793D1 * t3 *
     # t35 / 0.32D2

      end function



      doubleprecision function rrgq2qght6s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6
      doubleprecision rrgq2qgh64J7
      rrgq2qght6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh61J1
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
      t11 = t7 * t10
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t14 = t13 * t12
      t15 = s * t4
      t17 = z + x1 * t4
      t18 = 0.1D1 / t17
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t21 * t17 * x2 * t12)
      t31 = 0.2D1 * t25 * t29
      t32 = t21 * t17 + x2 * t12 - t31
      t35 = t8 * x3
      t37 = s - t15 * x1 * t18 * t32 - t15 * t35
      t42 = t2 * t1
      t43 = t42 * t6
      t44 = x1 ** 2
      t45 = t44 ** 2
      t46 = t43 * t45
      t47 = t17 ** 2
      t49 = 0.1D1 / t47 / t17
      t50 = t32 ** 2
      t51 = t49 * t50
      t54 = x2 * x3
      t55 = t12 * t20 * t17 + t54 + t31
      t59 = t6 * t4
      t60 = t42 * t59
      t61 = t45 * x1
      t62 = t60 * t61
      t63 = t47 ** 2
      t64 = 0.1D1 / t63
      t66 = t55 ** 2
      t67 = t66 * t55
      t71 = t5 * t4
      t72 = t42 * t71
      t73 = t44 * x1
      t75 = 0.1D1 / t47
      t76 = t75 * t32
      t80 = t50 * t32
      t85 = t49 * t32
      t89 = t9 ** 2
      t90 = t13 ** 2
      t95 = t45 * t49
      t100 = t44 * t18
      t104 = t61 * t64
      t105 = t50 ** 2
      t109 = t73 * t75
      t113 = t9 * t12
      t115 = t37 * t44
      t116 = t18 * t55
      t121 = t3 * t59
      t123 = t37 * t73
      t131 = t75 * t66
      t135 = t60 * t89
      t137 = x3 ** 2
      t146 = t137 * x3
      t150 = 0.10D2 * t11 * t14 * t37 * x1 - 0.288D3 * t46 * t51 * t55 -
     # 0.144D3 * t62 * t64 * t32 * t67 - 0.144D3 * t72 * t73 * t76 * t55
     # - 0.144D3 * t62 * t64 * t80 * t55 + 0.216D3 * t46 * t85 * t66 + 0
     #.36D2 * t60 * t89 * t90 * x1 + 0.108D3 * t43 * t95 * t80 + 0.36D2 
     #* t42 * t5 * t100 * t32 + 0.36D2 * t60 * t104 * t105 + 0.108D3 * t
     #72 * t109 * t50 + 0.400D3 * t7 * t113 * t115 * t116 * x3 - 0.584D3
     # * t121 * t113 * t123 * t75 * t55 * x2 + 0.56D2 * t7 * t8 * t12 * 
     #t123 * t131 + 0.36D2 * t135 * t13 * x1 * t137 + 0.28D2 * t135 * t1
     #4 * x1 * x3 + 0.28D2 * t135 * t12 * x1 * t146
      t151 = t3 * t71
      t152 = t151 * t73
      t153 = t75 * t50
      t163 = t18 * t32
      t167 = t7 * t45
      t168 = t49 * t80
      t172 = t6 * t5
      t173 = t42 * t172
      t180 = t32 * t37
      t186 = t7 * t109
      t187 = x2 * t8
      t193 = t9 * t137
      t197 = t121 * t95
      t198 = t50 * t37
      t202 = t151 * t100
      t208 = t10 * t12
      t215 = t44 * t137
      t219 = t10 * t13
      t221 = t55 * x3
      t226 = t73 * x3
      t238 = t32 * t8
      t242 = t60 * t95
      t247 = -0.16D2 * t152 * t153 * t37 + 0.216D3 * t62 * t64 * t50 * t
     #66 + 0.748D3 * t3 * t5 * t44 * t163 * t37 - 0.12D2 * t167 * t168 *
     # t37 + 0.16D2 * t173 * t35 * t104 * t67 * t32 + 0.94D2 * t121 * t1
     #09 * t180 * t9 * x3 * x2 - 0.128D3 * t186 * t180 * t187 * z + 0.47
     #D2 * t7 * t100 * t180 * t193 - 0.148D3 * t197 * t198 * t187 - 0.12
     #8D3 * t202 * t180 * z * t8 * x3 - 0.530D3 * t121 * t208 * t115 * t
     #54 * t18 - 0.144D3 * t60 * t208 * t215 * t116 - 0.112D3 * t60 * t2
     #19 * t100 * t221 + 0.168D3 * t60 * t113 * t226 * t131 - 0.112D3 * 
     #t173 * t208 * t73 * x2 * t75 * x3 * t55 - 0.112D3 * t43 * t109 * t
     #238 * t221 - 0.112D3 * t242 * t50 * t55 * t35
      t274 = t55 * x2
      t280 = t42 * t6 * t71
      t282 = t13 * t73
      t283 = x2 ** 2
      t284 = t283 * t75
      t289 = t14 * t44
      t290 = x2 * t18
      t294 = t6 ** 2
      t297 = t12 * t45
      t330 = t10 * t146
      t334 = -0.144D3 * t60 * t109 * t32 * t9 * t137 * t55 - 0.128D3 * t
     #186 * t180 * t55 * t8 * x3 + 0.96D2 * t186 * t180 * t187 + 0.328D3
     # * t7 * t9 * t13 * t115 * t116 + 0.96D2 * t202 * t180 * t35 - 0.14
     #8D3 * t186 * t198 * t35 - 0.128D3 * t197 * t180 * t274 * t8 + 0.10
     #8D3 * t280 * t89 * t282 * t284 + 0.108D3 * t173 * t89 * t289 * t29
     #0 + 0.36D2 * t42 * t294 * t89 * t297 * t283 * x2 * t49 + 0.192D3 *
     # t11 * t12 * t37 * x1 * t137 + 0.182D3 * t11 * t13 * t37 * x1 * x3
     # + 0.28D2 * t60 * t45 * t168 * t35 + 0.28D2 * t72 * t44 * t163 * t
     #35 + 0.56D2 * t43 * t73 * t153 * t35 + 0.36D2 * t43 * t44 * t163 *
     # t193 + 0.28D2 * t60 * t44 * t163 * t330
      t335 = t37 * t55
      t368 = t3 * t172
      t406 = t89 * t12
      t415 = -0.880D3 * t152 * t76 * t335 - 0.240D3 * t167 * t51 * t335 
     #+ 0.36D2 * t60 * t73 * t153 * t193 + 0.312D3 * t167 * t85 * t37 * 
     #t66 + 0.216D3 * t60 * t9 * t282 * t131 - 0.144D3 * t60 * t8 * t297
     # * t49 * t67 - 0.144D3 * t60 * t10 * t289 * t116 + 0.16D2 * t173 *
     # t330 * t109 * t55 * t32 + 0.47D2 * t368 * t95 * t180 * t283 * t9 
     #+ 0.216D3 * t173 * t113 * t95 * t66 * x2 - 0.288D3 * t173 * t219 *
     # t109 * t274 - 0.144D3 * t280 * t208 * t95 * t55 * t283 + 0.168D3 
     #* t242 * t238 * x3 * t66 + 0.430D3 * t368 * t208 * t123 * t284 - 0
     #.328D3 * t121 * t219 * t115 * t290 + 0.56D2 * t173 * t89 * t13 * t
     #44 * x3 * t290 + 0.36D2 * t173 * t406 * t215 * t290 + 0.28D2 * t28
     #0 * t406 * t226 * t284
      rrgq2qgh61J1 = -wd * (t150 + t247 + t334 + t415) / t1 / t37 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh61J2
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
      t9 = 0.1D1 - x3
      t10 = t9 * t8
      t12 = s * t4
      t14 = z + x1 * t4
      t15 = 0.1D1 / t14
      t17 = 0.1D1 - x2
      t18 = x3 * t17
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t18 * t14 * x2 * t9)
      t28 = 0.2D1 * t22 * t26
      t29 = t18 * t14 + x2 * t9 - t28
      t32 = t8 * x3
      t34 = s - t12 * x1 * t15 * t29 - t12 * t32
      t35 = x1 ** 2
      t36 = t35 * x1
      t37 = t34 * t36
      t38 = t14 ** 2
      t39 = 0.1D1 / t38
      t42 = x2 * x3
      t43 = t9 * t17 * t14 + t42 + t28
      t44 = t43 ** 2
      t45 = t39 * t44
      t47 = t7 * t10 * t37 * t45
      t49 = t5 * t4
      t50 = t3 * t49
      t51 = t15 * t35
      t52 = t50 * t51
      t53 = t29 * t34
      t57 = t52 * t53 * z * t8 * x3
      t59 = t6 * t5
      t60 = t3 * t59
      t61 = t8 ** 2
      t62 = t61 * t8
      t63 = t62 * t9
      t65 = x2 ** 2
      t66 = t65 * t39
      t68 = t60 * t63 * t37 * t66
      t71 = t52 * t53 * t32
      t73 = t36 * t39
      t74 = t7 * t73
      t75 = t29 ** 2
      t76 = t75 * t34
      t78 = t74 * t76 * t32
      t80 = t2 * t1
      t81 = t6 * t4
      t82 = t80 * t81
      t83 = t35 ** 2
      t85 = 0.1D1 / t38 / t14
      t86 = t83 * t85
      t87 = t82 * t86
      t88 = t29 * t8
      t91 = t87 * t88 * x3 * t44
      t93 = t80 * t6
      t95 = x3 * t43
      t97 = t93 * t73 * t88 * t95
      t101 = t87 * t75 * t43 * t32
      t103 = t61 * t9
      t105 = t36 * x3
      t107 = t82 * t103 * t105 * t45
      t109 = t80 * t59
      t115 = t109 * t63 * t36 * x2 * t39 * x3 * t43
      t117 = x2 * t8
      t119 = t74 * t53 * t117
      t124 = t60 * t86 * t53 * t65 * t61
      t129 = t109 * t103 * t86 * t44 * x2
      t131 = t3 * t81
      t132 = t131 * t86
      t134 = t132 * t76 * t117
      t136 = t61 ** 2
      t137 = t9 ** 2
      t141 = x2 * t15
      t145 = t136 * t9
      t147 = x3 ** 2
      t148 = t35 * t147
      t150 = t109 * t145 * t148 * t141
      t153 = t80 * t6 * t49
      t158 = 0.56D2 * t47 - 0.128D3 * t57 + 0.430D3 * t68 + 0.96D2 * t71
     # - 0.148D3 * t78 + 0.168D3 * t91 - 0.112D3 * t97 - 0.112D3 * t101 
     #+ 0.168D3 * t107 - 0.112D3 * t115 + 0.96D2 * t119 + 0.47D2 * t124 
     #+ 0.216D3 * t129 - 0.148D3 * t134 + 0.56D2 * t109 * t136 * t137 * 
     #t35 * x3 * t141 + 0.36D2 * t150 + 0.28D2 * t153 * t145 * t105 * t6
     #6
      t161 = t74 * t53 * t117 * z
      t165 = t34 * t35
      t166 = t15 * t43
      t167 = t165 * t166
      t168 = t7 * t61 * t137 * t167
      t170 = t62 * t137
      t172 = t165 * t141
      t173 = t131 * t170 * t172
      t178 = t131 * t63 * t165 * t42 * t15
      t182 = t82 * t63 * t148 * t166
      t186 = t82 * t170 * t51 * t95
      t189 = t61 * t147
      t191 = t7 * t51 * t53 * t189
      t193 = t7 * t62
      t194 = t137 * t34
      t195 = x3 * x1
      t197 = t193 * t194 * t195
      t200 = t15 * t29
      t202 = t93 * t35 * t200 * t189
      t205 = t39 * t75
      t209 = t49 * t80
      t215 = t75 * t29
      t216 = t85 * t215
      t220 = t50 * t36
      t221 = t39 * t29
      t222 = t34 * t43
      t224 = t220 * t221 * t222
      t227 = t147 * x3
      t228 = t62 * t227
      t230 = t82 * t35 * t200 * t228
      t234 = t82 * t36 * t205 * t189
      t236 = t7 * t83
      t237 = t85 * t75
      t239 = t236 * t237 * t222
      t241 = t85 * t29
      t244 = t236 * t241 * t34 * t44
      t246 = -0.128D3 * t161 + 0.328D3 * t168 - 0.328D3 * t173 - 0.530D3
     # * t178 - 0.144D3 * t182 - 0.112D3 * t186 + 0.47D2 * t191 + 0.182D
     #3 * t197 + 0.36D2 * t202 + 0.56D2 * t93 * t36 * t205 * t32 + 0.28D
     #2 * t209 * t35 * t200 * t32 + 0.28D2 * t82 * t83 * t216 * t32 - 0.
     #880D3 * t224 + 0.28D2 * t230 + 0.36D2 * t234 - 0.240D3 * t239 + 0.
     #312D3 * t244
      t249 = t137 * t36
      t251 = t82 * t61 * t249 * t45
      t254 = t9 * t137
      t255 = t254 * t35
      t260 = t9 * t83
      t261 = t44 * t43
      t265 = 0.144D3 * t82 * t8 * t260 * t85 * t261
      t267 = t43 * x2
      t280 = 0.16D2 * t109 * t228 * t73 * t43 * t29
      t282 = t83 * x1
      t283 = t38 ** 2
      t284 = 0.1D1 / t283
      t285 = t282 * t284
      t289 = 0.16D2 * t109 * t32 * t285 * t261 * t29
      t294 = t131 * t73 * t53 * t61 * x3 * x2
      t300 = t82 * t73 * t29 * t61 * t147 * t43
      t305 = t74 * t53 * t43 * t8 * x3
      t307 = t137 ** 2
      t319 = t75 ** 2
      t326 = t82 * t282
      t331 = t93 * t83
      t333 = t331 * t241 * t44
      t335 = 0.216D3 * t251 - 0.144D3 * t82 * t62 * t255 * t166 - t265 -
     # 0.288D3 * t109 * t170 * t73 * t267 - 0.144D3 * t153 * t63 * t86 *
     # t43 * t65 + t280 + t289 + 0.94D2 * t294 - 0.144D3 * t300 - 0.128D
     #3 * t305 + 0.36D2 * t82 * t136 * t307 * x1 + 0.108D3 * t93 * t86 *
     # t215 + 0.36D2 * t80 * t5 * t51 * t29 + 0.36D2 * t82 * t285 * t319
     # + 0.108D3 * t209 * t73 * t75 - 0.144D3 * t326 * t284 * t215 * t43
     # + 0.216D3 * t333
      t336 = t82 * t136
      t339 = t336 * t137 * x1 * t147
      t347 = t336 * t9 * x1 * t227
      t350 = t220 * t205 * t34
      t354 = t326 * t284 * t75 * t44
      t356 = t3 * t5
      t359 = t356 * t35 * t200 * t34
      t362 = t236 * t216 * t34
      t366 = t193 * t254 * t34 * x1
      t374 = 0.144D3 * t326 * t284 * t29 * t261
      t381 = t132 * t53 * t267 * t8
      t383 = t7 * t103
      t386 = t383 * t165 * t166 * x3
      t388 = t9 * t34
      t391 = t193 * t388 * x1 * t147
      t393 = t6 ** 2
      t410 = t39 * t43
      t413 = t131 * t103 * t37 * t410 * x2
      t415 = 0.36D2 * t339 + 0.28D2 * t336 * t254 * x1 * x3 + 0.28D2 * t
     #347 - 0.16D2 * t350 + 0.216D3 * t354 + 0.748D3 * t359 - 0.12D2 * t
     #362 + 0.10D2 * t366 - 0.288D3 * t331 * t237 * t43 - t374 - 0.144D3
     # * t209 * t36 * t221 * t43 - 0.128D3 * t381 + 0.400D3 * t386 + 0.1
     #92D3 * t391 + 0.36D2 * t80 * t393 * t136 * t260 * t65 * x2 * t85 +
     # 0.108D3 * t109 * t136 * t255 * t141 + 0.108D3 * t153 * t136 * t24
     #9 * t66 - 0.584D3 * t413
      t432 = t3 * t34
      t434 = t432 * t6 * t36
      t439 = 0.24D2 * t47 + 0.256D3 * t57 - 0.542D3 * t68 - 0.640D3 * t7
     #1 + 0.216D3 * t78 - 0.344D3 * t91 + 0.144D3 * t97 + 0.144D3 * t101
     # - 0.344D3 * t107 + 0.144D3 * t115 - 0.640D3 * t119 - 0.54D2 * t12
     #4 - 0.72D2 * t129 + 0.64D2 * t434 * t32 * t45 + 0.216D3 * t134
      t459 = -0.72D2 * t150 + 0.32D2 * t109 * t189 * t86 * t44 * t29 + 0
     #.256D3 * t161 - 0.256D3 * t168 + 0.604D3 * t173 + 0.444D3 * t178 +
     # 0.256D3 * t182 + 0.144D3 * t186 - 0.54D2 * t191 - 0.108D3 * t197 
     #- 0.72D2 * t202 + 0.640D3 * t224 - 0.56D2 * t230 - 0.72D2 * t234 -
     # 0.64D2 * t239
      t463 = t50 * t61
      t492 = -0.264D3 * t244 - 0.72D2 * t251 + t265 - 0.256D3 * t463 * t
     #388 * t195 + 0.64D2 * t432 * t49 * t35 * x2 * t8 * t15 - t280 - t2
     #89 - 0.108D3 * t294 + 0.256D3 * t300 + 0.496D3 * t305 - 0.128D3 * 
     #t434 * t410 * t117 + 0.64D2 * t432 * t5 * x1 * t8 * x3 - 0.256D3 *
     # t463 * t194 * x1 + 0.136D3 * t356 * t8 * t388 * x1 - 0.72D2 * t33
     #3
      t520 = -0.72D2 * t339 - 0.56D2 * t347 + 0.608D3 * t350 - 0.72D2 * 
     #t354 - 0.592D3 * t359 - 0.16D2 * t362 - 0.62D2 * t366 + t374 + 0.4
     #96D3 * t381 - 0.288D3 * t386 + 0.64D2 * t432 * t81 * t83 * t85 * t
     #44 * t117 + 0.256D3 * t383 * t172 - 0.216D3 * t50 * t10 * t167 - 0
     #.128D3 * t432 * t49 * t35 * t32 * t166 - 0.30D2 * t391 + 0.544D3 *
     # t413
      rrgq2qgh61J2 = -(wd * (t158 + t246 + t335 + t415) + wd * (t439 + t
     #459 + t492 + t520)) / t1 / t34 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh61J3
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
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t11 ** 2
      t14 = 0.1D1 - x3
      t15 = t14 ** 2
      t16 = x1 ** 2
      t17 = t16 * x1
      t18 = t15 * t17
      t19 = x2 ** 2
      t21 = z + x1 * t4
      t22 = t21 ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t28 = t7 * t5
      t29 = t3 * t28
      t31 = t15 * t14
      t32 = t31 * t16
      t33 = 0.1D1 / t21
      t34 = x2 * t33
      t38 = t7 ** 2
      t41 = t16 ** 2
      t42 = t14 * t41
      t43 = t19 * x2
      t45 = 0.1D1 / t22 / t21
      t50 = t2 * s
      t51 = t50 * t7
      t52 = t11 * t10
      t53 = t51 * t52
      t54 = s * t4
      t56 = 0.1D1 - x2
      t57 = x3 * t56
      t61 = cos(x4 * 0.3141592653589793D1)
      t65 = Sqrt(t57 * t21 * x2 * t14)
      t67 = 0.2D1 * t61 * t65
      t68 = t57 * t21 + x2 * t14 - t67
      t71 = t10 * x3
      t73 = s - t54 * x1 * t33 * t68 - t54 * t71
      t74 = t14 * t73
      t75 = x3 ** 2
      t78 = t53 * t74 * x1 * t75
      t80 = t15 * t73
      t81 = x3 * x1
      t83 = t53 * t80 * t81
      t85 = t7 * t4
      t86 = t3 * t85
      t88 = t68 ** 2
      t89 = t88 * t68
      t90 = t45 * t89
      t94 = t3 * t6
      t96 = t33 * t68
      t100 = t3 * t7
      t102 = t23 * t88
      t107 = t11 * t75
      t109 = t100 * t16 * t96 * t107
      t112 = t75 * x3
      t113 = t52 * t112
      t115 = t86 * t16 * t96 * t113
      t117 = t50 * t6
      t118 = t117 * t17
      t119 = t23 * t68
      t122 = x2 * x3
      t123 = t14 * t56 * t21 + t122 + t67
      t124 = t73 * t123
      t126 = t118 * t119 * t124
      t128 = t51 * t41
      t129 = t45 * t88
      t131 = t128 * t129 * t124
      t135 = t86 * t17 * t102 * t107
      t137 = t45 * t68
      t138 = t123 ** 2
      t141 = t128 * t137 * t73 * t138
      t144 = t23 * t138
      t146 = t86 * t11 * t18 * t144
      t149 = t138 * t123
      t153 = 0.144D3 * t86 * t10 * t42 * t45 * t149
      t155 = t33 * t123
      t159 = 0.108D3 * t9 * t12 * t18 * t24 + 0.108D3 * t29 * t12 * t32 
     #* t34 + 0.36D2 * t3 * t38 * t12 * t42 * t43 * t45 + 0.192D3 * t78 
     #+ 0.182D3 * t83 + 0.28D2 * t86 * t41 * t90 * t71 + 0.28D2 * t94 * 
     #t16 * t96 * t71 + 0.56D2 * t100 * t17 * t102 * t71 + 0.36D2 * t109
     # + 0.28D2 * t115 - 0.880D3 * t126 - 0.240D3 * t131 + 0.36D2 * t135
     # + 0.312D3 * t141 + 0.216D3 * t146 - t153 - 0.144D3 * t86 * t52 * 
     #t32 * t155
      t160 = t50 * t85
      t161 = t52 * t15
      t163 = t73 * t16
      t164 = t163 * t34
      t165 = t160 * t161 * t164
      t167 = t17 * t23
      t168 = t51 * t167
      t169 = t68 * t73
      t170 = x2 * t10
      t172 = t168 * t169 * t170
      t174 = t50 * t28
      t175 = t41 * t45
      t177 = t19 * t11
      t179 = t174 * t175 * t169 * t177
      t181 = t11 * t14
      t183 = t138 * x2
      t185 = t29 * t181 * t175 * t183
      t187 = t86 * t175
      t188 = t68 * t10
      t191 = t187 * t188 * x3 * t138
      t194 = x3 * t123
      t196 = t100 * t167 * t188 * t194
      t200 = t187 * t88 * t123 * t71
      t206 = t160 * t167 * t169 * t11 * x3 * x2
      t208 = t52 * t14
      t210 = t73 * t17
      t212 = t174 * t208 * t210 * t24
      t216 = t168 * t169 * t170 * z
      t217 = 0.128D3 * t216
      t224 = t12 * t14
      t226 = t16 * t75
      t228 = t29 * t224 * t226 * t34
      t231 = x3 * t17
      t238 = t160 * t208 * t163 * t122 * t33
      t242 = t86 * t208 * t226 * t155
      t245 = t16 * t33
      t247 = t86 * t161 * t245 * t194
      t251 = t86 * t181 * t231 * t144
      t253 = -0.328D3 * t165 + 0.96D2 * t172 + 0.47D2 * t179 + 0.216D3 *
     # t185 + 0.168D3 * t191 - 0.112D3 * t196 - 0.112D3 * t200 + 0.94D2 
     #* t206 + 0.430D3 * t212 - t217 + 0.56D2 * t29 * t12 * t15 * t16 * 
     #x3 * t34 + 0.36D2 * t228 + 0.28D2 * t9 * t224 * t231 * t24 - 0.530
     #D3 * t238 - 0.144D3 * t242 - 0.112D3 * t247 + 0.168D3 * t251
      t256 = t41 * x1
      t257 = t22 ** 2
      t258 = 0.1D1 / t257
      t259 = t256 * t258
      t263 = 0.16D2 * t29 * t71 * t259 * t149 * t68
      t265 = t68 * t11
      t268 = t86 * t167 * t265 * t75 * t123
      t273 = t168 * t169 * t123 * t10 * x3
      t275 = t160 * t175
      t276 = t88 * t73
      t278 = t275 * t276 * t170
      t281 = t23 * t123
      t284 = t160 * t181 * t210 * t281 * x2
      t286 = t10 * t14
      t289 = t51 * t286 * t210 * t144
      t293 = t163 * t155
      t294 = t51 * t11 * t15 * t293
      t296 = t15 ** 2
      t304 = t3 * t5
      t308 = t88 ** 2
      t315 = t86 * t12
      t318 = t315 * t15 * x1 * t75
      t326 = t315 * t14 * x1 * t112
      t329 = t118 * t102 * t73
      t331 = t86 * t256
      t334 = t331 * t258 * t88 * t138
      t336 = t263 - 0.144D3 * t268 - 0.128D3 * t273 - 0.148D3 * t278 - 0
     #.584D3 * t284 + 0.56D2 * t289 + 0.328D3 * t294 + 0.36D2 * t86 * t1
     #2 * t296 * x1 + 0.108D3 * t100 * t175 * t89 + 0.36D2 * t304 * t245
     # * t68 + 0.36D2 * t86 * t259 * t308 + 0.108D3 * t94 * t167 * t88 +
     # 0.36D2 * t318 + 0.28D2 * t315 * t31 * x1 * x3 + 0.28D2 * t326 - 0
     #.16D2 * t329 + 0.216D3 * t334
      t337 = t50 * t5
      t340 = t337 * t16 * t96 * t73
      t343 = t128 * t90 * t73
      t347 = t53 * t31 * t73 * x1
      t349 = t100 * t41
      t356 = 0.144D3 * t331 * t258 * t68 * t149
      t366 = t349 * t137 * t138
      t368 = t117 * t245
      t372 = t368 * t169 * z * t10 * x3
      t378 = 0.16D2 * t29 * t113 * t167 * t123 * t68
      t380 = t123 * x2
      t385 = t123 * t19
      t392 = t23 * x3 * t123
      t394 = t29 * t208 * t17 * x2 * t392
      t398 = t275 * t169 * t380 * t10
      t401 = t368 * t169 * t71
      t404 = t168 * t276 * t71
      t408 = t51 * t245 * t169 * t107
      t410 = t51 * t181
      t413 = t410 * t163 * t155 * x3
      t415 = 0.748D3 * t340 - 0.12D2 * t343 + 0.10D2 * t347 - 0.288D3 * 
     #t349 * t129 * t123 - t356 - 0.144D3 * t94 * t17 * t119 * t123 - 0.
     #144D3 * t331 * t258 * t89 * t123 + 0.216D3 * t366 - 0.128D3 * t372
     # + t378 - 0.288D3 * t29 * t161 * t167 * t380 - 0.144D3 * t9 * t208
     # * t175 * t385 - 0.112D3 * t394 - 0.128D3 * t398 + 0.96D2 * t401 -
     # 0.148D3 * t404 + 0.47D2 * t408 + 0.400D3 * t413
      t428 = t50 * t73
      t433 = t428 * t85 * t41 * t45 * t138 * t170
      t439 = -0.30D2 * t78 - 0.108D3 * t83 - 0.72D2 * t109 - 0.56D2 * t1
     #15 + 0.640D3 * t126 - 0.64D2 * t131 - 0.72D2 * t135 - 0.264D3 * t1
     #41 - 0.72D2 * t146 + t153 + 0.64D2 * t433 + 0.604D3 * t165 - 0.640
     #D3 * t172 - 0.54D2 * t179 - 0.72D2 * t185
      t441 = t117 * t286 * t293
      t446 = t428 * t6 * t16 * t71 * t155
      t452 = 0.32D2 * t29 * t107 * t175 * t138 * t68
      t460 = t428 * t5
      t461 = x1 * t10
      t463 = t460 * t461 * x3
      t465 = t117 * t11
      t467 = t465 * t80 * x1
      t475 = -0.216D3 * t441 - 0.128D3 * t446 + t452 - 0.344D3 * t191 + 
     #0.144D3 * t196 + 0.144D3 * t200 - 0.108D3 * t206 - 0.542D3 * t212 
     #+ 0.256D3 * t216 - 0.72D2 * t228 + 0.64D2 * t463 - 0.256D3 * t467 
     #+ 0.136D3 * t337 * t10 * t74 * x1 + 0.444D3 * t238 + 0.256D3 * t24
     #2
      t477 = t410 * t164
      t482 = t428 * t7 * t17
      t484 = t482 * t281 * t170
      t496 = 0.256D3 * t477 + 0.144D3 * t247 - 0.344D3 * t251 - t263 - 0
     #.128D3 * t484 + 0.256D3 * t268 + 0.496D3 * t273 + 0.216D3 * t278 +
     # 0.544D3 * t284 + 0.24D2 * t289 - 0.256D3 * t294 - 0.72D2 * t318 -
     # 0.56D2 * t326 + 0.608D3 * t329 - 0.72D2 * t334
      t504 = t428 * t6
      t511 = t465 * t74 * t81
      t517 = t482 * t71 * t144
      t520 = -0.592D3 * t340 - 0.16D2 * t343 - 0.62D2 * t347 + t356 - 0.
     #72D2 * t366 + 0.256D3 * t372 - t378 + 0.144D3 * t394 + 0.496D3 * t
     #398 + 0.64D2 * t504 * t16 * x2 * t10 * t33 - 0.256D3 * t511 - 0.64
     #0D3 * t401 + 0.216D3 * t404 - 0.54D2 * t408 + 0.64D2 * t517 - 0.28
     #8D3 * t413
      t537 = x2 * t11
      t542 = -0.28792D5 * t78 - 0.29128D5 * t83 + 0.144D3 * t115 - 0.128
     #D3 * t126 + 0.128D3 * t131 + 0.64D2 * t141 + 0.128D3 * t433 - 0.27
     #920D5 * t165 + 0.4D1 * t179 - 0.80D2 * t441 - 0.64D2 * t446 + t452
     # - 0.192D3 * t428 * t85 * t17 * t537 * t392 + 0.144D3 * t191
      t546 = t428 * t7 * t16
      t560 = z ** 2
      t566 = x1 * t11
      t579 = 0.8D1 * t206 + 0.28208D5 * t212 - t217 + 0.64D2 * t546 * t5
     #37 * t33 * x3 - 0.96D2 * t428 * t28 * t41 * t45 * t123 * t177 - 0.
     #128D3 * t463 - 0.25296D5 * t467 + 0.128D3 * t304 * t10 * x3 * t560
     # * z * x1 + 0.288D3 * t504 * t566 * t75 - 0.112D3 * t428 * t7 * x1
     # * t52 * t112 + 0.536D3 * t238 - 0.288D3 * t242 + 0.25264D5 * t477
     # + 0.144D3 * t251 - t263
      t604 = -0.64D2 * t484 - 0.288D3 * t268 - 0.8D1 * t273 - 0.74208D5 
     #* t284 + 0.32D2 * t289 + 0.74320D5 * t294 + 0.144D3 * t326 - 0.128
     #D3 * t329 + 0.64D2 * t340 + 0.64D2 * t343 - 0.304D3 * t347 - 0.512
     #D3 * t372 + 0.16D2 * t174 * t259 * t183 * t188 * t73 + 0.48D2 * t4
     #28 * t28 * t17 * t52 * x3 * t24 - t378
      t651 = -0.8D1 * t398 + 0.48D2 * t428 * t85 * t16 * t52 * t75 * t34
     # - 0.96D2 * t546 * t107 * t155 - 0.25248D5 * t511 - 0.384D3 * t504
     # * t566 * t75 * z - 0.384D3 * t460 * t461 * x3 * t560 + 0.384D3 * 
     #t460 * t461 * x3 * z + 0.32D2 * t428 * t85 * t17 * t19 * t11 * t23
     # + 0.16D2 * t428 * t8 * t41 * t43 * t52 * t45 + 0.256D3 * t401 - 0
     #.128D3 * t404 - 0.252D3 * t408 - 0.16D2 * t50 * t8 * t259 * t385 *
     # t265 * t73 + 0.128D3 * t517 + 0.74288D5 * t413
      rrgq2qgh61J3 = -(wd * (t159 + t253 + t336 + t415) + wd * (t439 + t
     #475 + t496 + t520) + wd * (t542 + t579 + t604 + t651)) / t1 / t73 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh61J4
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
      t9 = 0.1D1 - x3
      t10 = t8 * t9
      t12 = s * t4
      t14 = z + x1 * t4
      t15 = 0.1D1 / t14
      t17 = 0.1D1 - x2
      t18 = x3 * t17
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t18 * t14 * x2 * t9)
      t28 = 0.2D1 * t22 * t26
      t29 = t18 * t14 + x2 * t9 - t28
      t32 = t8 * x3
      t34 = s - t12 * x1 * t15 * t29 - t12 * t32
      t35 = x1 ** 2
      t36 = t35 * x1
      t37 = t34 * t36
      t38 = t14 ** 2
      t39 = 0.1D1 / t38
      t42 = x2 * x3
      t43 = t9 * t17 * t14 + t42 + t28
      t44 = t43 ** 2
      t45 = t39 * t44
      t47 = t7 * t10 * t37 * t45
      t49 = t8 ** 2
      t50 = t9 ** 2
      t53 = t34 * t35
      t54 = t15 * t43
      t55 = t53 * t54
      t56 = t7 * t49 * t50 * t55
      t58 = t6 * t5
      t59 = t3 * t58
      t60 = t49 * t8
      t61 = t60 * t9
      t63 = x2 ** 2
      t64 = t63 * t39
      t66 = t59 * t61 * t37 * t64
      t68 = t6 * t4
      t69 = t3 * t68
      t70 = t60 * t50
      t72 = x2 * t15
      t73 = t53 * t72
      t74 = t69 * t70 * t73
      t76 = t49 * t9
      t77 = t7 * t76
      t80 = t77 * t53 * t54 * x3
      t83 = t39 * t43
      t86 = t69 * t76 * t37 * t83 * x2
      t88 = t5 * t4
      t89 = t3 * t88
      t90 = t35 * t15
      t91 = t89 * t90
      t92 = t29 * t34
      t96 = t91 * t92 * z * t8 * x3
      t98 = t36 * t39
      t99 = t7 * t98
      t100 = x2 * t8
      t103 = t99 * t92 * t100 * z
      t104 = 0.128D3 * t103
      t106 = t91 * t92 * t32
      t108 = t2 * t1
      t109 = t108 * t58
      t110 = x3 ** 2
      t111 = t110 * x3
      t112 = t60 * t111
      t117 = 0.16D2 * t109 * t112 * t98 * t43 * t29
      t118 = t6 * t88
      t119 = t108 * t118
      t120 = t49 ** 2
      t122 = t50 * t36
      t127 = t50 * t9
      t128 = t127 * t35
      t132 = t6 ** 2
      t135 = t35 ** 2
      t136 = t9 * t135
      t137 = t63 * x2
      t139 = 0.1D1 / t38 / t14
      t144 = t7 * t60
      t145 = t9 * t34
      t148 = t144 * t145 * x1 * t110
      t150 = t50 * t34
      t151 = x3 * x1
      t153 = t144 * t150 * t151
      t155 = t108 * t68
      t157 = t29 ** 2
      t158 = t157 * t29
      t159 = t139 * t158
      t163 = t108 * t88
      t165 = t15 * t29
      t169 = 0.56D2 * t47 + 0.328D3 * t56 + 0.430D3 * t66 - 0.328D3 * t7
     #4 + 0.400D3 * t80 - 0.584D3 * t86 - 0.128D3 * t96 - t104 + 0.96D2 
     #* t106 + t117 + 0.108D3 * t119 * t120 * t122 * t64 + 0.108D3 * t10
     #9 * t120 * t128 * t72 + 0.36D2 * t108 * t132 * t120 * t136 * t137 
     #* t139 + 0.192D3 * t148 + 0.182D3 * t153 + 0.28D2 * t155 * t135 * 
     #t159 * t32 + 0.28D2 * t163 * t35 * t165 * t32
      t170 = t108 * t6
      t172 = t39 * t157
      t177 = t49 * t110
      t179 = t170 * t35 * t165 * t177
      t183 = t155 * t35 * t165 * t112
      t185 = t89 * t36
      t186 = t39 * t29
      t187 = t34 * t43
      t189 = t185 * t186 * t187
      t191 = t7 * t135
      t192 = t139 * t157
      t194 = t191 * t192 * t187
      t198 = t155 * t36 * t172 * t177
      t200 = t139 * t29
      t203 = t191 * t200 * t34 * t44
      t207 = t155 * t49 * t122 * t45
      t210 = t44 * t43
      t214 = 0.144D3 * t155 * t8 * t136 * t139 * t210
      t219 = t135 * t139
      t220 = t69 * t219
      t221 = t157 * t34
      t223 = t220 * t221 * t100
      t225 = t155 * t219
      t226 = t29 * t8
      t229 = t225 * t226 * x3 * t44
      t231 = t50 ** 2
      t239 = t108 * t5
      t243 = t135 * x1
      t244 = t38 ** 2
      t245 = 0.1D1 / t244
      t246 = t243 * t245
      t247 = t157 ** 2
      t254 = 0.56D2 * t170 * t36 * t172 * t32 + 0.36D2 * t179 + 0.28D2 *
     # t183 - 0.880D3 * t189 - 0.240D3 * t194 + 0.36D2 * t198 + 0.312D3 
     #* t203 + 0.216D3 * t207 - t214 - 0.144D3 * t155 * t60 * t128 * t54
     # - 0.148D3 * t223 + 0.168D3 * t229 + 0.36D2 * t155 * t120 * t231 *
     # x1 + 0.108D3 * t170 * t219 * t158 + 0.36D2 * t239 * t90 * t29 + 0
     #.36D2 * t155 * t246 * t247 + 0.108D3 * t163 * t98 * t157
      t260 = 0.16D2 * t109 * t32 * t246 * t210 * t29
      t262 = t99 * t92 * t100
      t265 = t63 * t49
      t267 = t59 * t219 * t92 * t265
      t270 = t44 * x2
      t272 = t109 * t76 * t219 * t270
      t275 = t43 * x2
      t280 = t43 * t63
      t286 = t220 * t92 * t275 * t8
      t289 = t99 * t221 * t32
      t293 = t7 * t90 * t92 * t177
      t305 = t69 * t98 * t92 * t49 * x3 * x2
      t307 = t155 * t120
      t310 = t307 * t50 * x1 * t110
      t318 = t307 * t9 * x1 * t111
      t321 = t185 * t172 * t34
      t323 = t155 * t243
      t326 = t323 * t245 * t157 * t44
      t328 = t3 * t5
      t331 = t328 * t35 * t165 * t34
      t333 = t260 + 0.96D2 * t262 + 0.47D2 * t267 + 0.216D3 * t272 - 0.2
     #88D3 * t109 * t70 * t98 * t275 - 0.144D3 * t119 * t61 * t219 * t28
     #0 - 0.128D3 * t286 - 0.148D3 * t289 + 0.47D2 * t293 + 0.56D2 * t10
     #9 * t120 * t50 * t35 * x3 * t72 + 0.94D2 * t305 + 0.36D2 * t310 + 
     #0.28D2 * t307 * t127 * x1 * x3 + 0.28D2 * t318 - 0.16D2 * t321 + 0
     #.216D3 * t326 + 0.748D3 * t331
      t335 = t191 * t159 * t34
      t339 = t144 * t127 * t34 * x1
      t341 = t170 * t135
      t348 = 0.144D3 * t323 * t245 * t29 * t210
      t358 = t341 * t200 * t44
      t361 = x3 * t43
      t363 = t170 * t98 * t226 * t361
      t367 = t225 * t157 * t43 * t32
      t370 = t29 * t49
      t373 = t155 * t98 * t370 * t110 * t43
      t378 = t99 * t92 * t43 * t8 * x3
      t380 = t120 * t9
      t382 = t35 * t110
      t384 = t109 * t380 * t382 * t72
      t387 = t36 * x3
      t394 = t69 * t61 * t53 * t42 * t15
      t398 = t155 * t61 * t382 * t54
      t402 = t155 * t70 * t90 * t361
      t406 = t155 * t76 * t387 * t45
      t411 = t39 * x3 * t43
      t413 = t109 * t61 * t36 * x2 * t411
      t415 = -0.12D2 * t335 + 0.10D2 * t339 - 0.288D3 * t341 * t192 * t4
     #3 - t348 - 0.144D3 * t163 * t36 * t186 * t43 - 0.144D3 * t323 * t2
     #45 * t158 * t43 + 0.216D3 * t358 - 0.112D3 * t363 - 0.112D3 * t367
     # - 0.144D3 * t373 - 0.128D3 * t378 + 0.36D2 * t384 + 0.28D2 * t119
     # * t380 * t387 * t64 - 0.530D3 * t394 - 0.144D3 * t398 - 0.112D3 *
     # t402 + 0.168D3 * t406 - 0.112D3 * t413
      t419 = t3 * t34
      t423 = t419 * t88 * t35 * t32 * t54
      t435 = t419 * t6 * t36
      t437 = t435 * t32 * t45
      t440 = t89 * t10 * t55
      t444 = -0.128D3 * t423 + 0.24D2 * t47 - 0.256D3 * t56 - 0.542D3 * 
     #t66 + 0.604D3 * t74 - 0.288D3 * t80 + 0.544D3 * t86 + 0.256D3 * t9
     #6 + 0.256D3 * t103 - 0.640D3 * t106 + 0.64D2 * t437 - 0.216D3 * t4
     #40 - t117 - 0.30D2 * t148 - 0.108D3 * t153
      t452 = t419 * t88
      t458 = t89 * t49
      t460 = t458 * t145 * t151
      t466 = -0.72D2 * t179 - 0.56D2 * t183 + 0.640D3 * t189 - 0.64D2 * 
     #t194 - 0.72D2 * t198 - 0.264D3 * t203 - 0.72D2 * t207 + t214 + 0.6
     #4D2 * t452 * t35 * x2 * t8 * t15 - 0.256D3 * t460 + 0.216D3 * t223
     # - 0.344D3 * t229 - t260 - 0.640D3 * t262 - 0.54D2 * t267
      t470 = t435 * t83 * t100
      t476 = t77 * t73
      t482 = t419 * t68 * t135 * t139 * t44 * t100
      t491 = -0.72D2 * t272 - 0.128D3 * t470 + 0.496D3 * t286 + 0.216D3 
     #* t289 - 0.54D2 * t293 - 0.108D3 * t305 + 0.256D3 * t476 + 0.64D2 
     #* t482 - 0.72D2 * t310 - 0.56D2 * t318 + 0.608D3 * t321 - 0.72D2 *
     # t326 - 0.592D3 * t331 - 0.16D2 * t335 - 0.62D2 * t339
      t493 = t419 * t5
      t494 = x1 * t8
      t496 = t493 * t494 * x3
      t499 = t458 * t150 * x1
      t514 = 0.32D2 * t109 * t177 * t219 * t44 * t29
      t520 = t348 - 0.72D2 * t358 + 0.64D2 * t496 - 0.256D3 * t499 + 0.1
     #36D3 * t328 * t8 * t145 * x1 + 0.144D3 * t363 + 0.144D3 * t367 + 0
     #.256D3 * t373 + 0.496D3 * t378 - 0.72D2 * t384 + t514 + 0.444D3 * 
     #t394 + 0.256D3 * t398 + 0.144D3 * t402 - 0.344D3 * t406 + 0.144D3 
     #* t413
      t530 = t419 * t6 * t35
      t550 = -0.64D2 * t423 + 0.32D2 * t47 + 0.74320D5 * t56 + 0.28208D5
     # * t66 - 0.27920D5 * t74 - 0.96D2 * t530 * t177 * t54 + 0.74288D5 
     #* t80 - 0.74208D5 * t86 + 0.16D2 * t59 * t246 * t270 * t226 * t34 
     #+ 0.48D2 * t419 * t58 * t36 * t60 * x3 * t64 - 0.512D3 * t96 - t10
     #4 + 0.256D3 * t106 + 0.128D3 * t437
      t559 = x1 * t49
      t564 = z ** 2
      t585 = x2 * t49
      t590 = -0.80D2 * t440 - t117 - 0.28792D5 * t148 - 0.29128D5 * t153
     # + 0.144D3 * t183 - 0.128D3 * t189 + 0.128D3 * t194 + 0.64D2 * t20
     #3 - 0.25248D5 * t460 - 0.384D3 * t452 * t559 * t110 * z - 0.384D3 
     #* t493 * t494 * x3 * t564 + 0.384D3 * t493 * t494 * x3 * z + 0.32D
     #2 * t419 * t68 * t36 * t63 * t49 * t39 + 0.16D2 * t419 * t118 * t1
     #35 * t137 * t60 * t139 + 0.64D2 * t530 * t585 * t15 * x3
      t625 = 0.48D2 * t419 * t68 * t35 * t60 * t110 * t72 + 0.144D3 * t2
     #29 - t260 + 0.4D1 * t267 - 0.64D2 * t470 - 0.8D1 * t286 - 0.128D3 
     #* t289 - 0.252D3 * t293 - 0.96D2 * t419 * t58 * t135 * t139 * t43 
     #* t265 - 0.16D2 * t3 * t118 * t246 * t280 * t370 * t34 + 0.8D1 * t
     #305 + 0.25264D5 * t476 + 0.128D3 * t482 - 0.192D3 * t419 * t68 * t
     #36 * t585 * t411 + 0.144D3 * t318
      t651 = -0.128D3 * t321 + 0.64D2 * t331 + 0.64D2 * t335 - 0.304D3 *
     # t339 - 0.128D3 * t496 - 0.25296D5 * t499 + 0.128D3 * t239 * t8 * 
     #x3 * t564 * z * x1 + 0.288D3 * t452 * t559 * t110 - 0.112D3 * t419
     # * t6 * x1 * t60 * t111 - 0.288D3 * t373 - 0.8D1 * t378 + t514 + 0
     #.536D3 * t394 - 0.288D3 * t398 + 0.144D3 * t406
      rrgq2qgh61J4 = -(wd * (t169 + t254 + t333 + t415) + wd * (t444 + t
     #466 + t491 + t520) + wd * (t550 + t590 + t625 + t651)) / t1 / t34 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh61J5
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
      t14 = t11 * t13
      t16 = s * t4
      t18 = z + x1 * t4
      t19 = 0.1D1 / t18
      t21 = 0.1D1 - x2
      t22 = x3 * t21
      t26 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(t22 * t18 * x2 * t12)
      t32 = 0.2D1 * t26 * t30
      t33 = t22 * t18 + x2 * t12 - t32
      t36 = t9 * x3
      t38 = s - t16 * x1 * t19 * t33 - t16 * t36
      t39 = x1 ** 2
      t40 = t38 * t39
      t41 = x2 * t19
      t42 = t40 * t41
      t43 = t8 * t14 * t42
      t45 = t2 * t1
      t46 = t6 ** 2
      t48 = t10 ** 2
      t50 = t39 ** 2
      t51 = t12 * t50
      t52 = x2 ** 2
      t53 = t52 * x2
      t54 = t18 ** 2
      t56 = 0.1D1 / t54 / t18
      t61 = t3 * t6
      t62 = t61 * t11
      t63 = t12 * t38
      t64 = x3 ** 2
      t67 = t62 * t63 * x1 * t64
      t69 = t13 * t38
      t70 = x3 * x1
      t72 = t62 * t69 * t70
      t74 = t45 * t7
      t76 = t33 ** 2
      t77 = t76 * t33
      t78 = t56 * t77
      t82 = t5 * t4
      t83 = t45 * t82
      t85 = t19 * t33
      t89 = t45 * t6
      t90 = t39 * x1
      t92 = 0.1D1 / t54
      t93 = t92 * t76
      t98 = t10 * t64
      t100 = t89 * t39 * t85 * t98
      t103 = t64 * x3
      t104 = t11 * t103
      t106 = t74 * t39 * t85 * t104
      t108 = t3 * t82
      t109 = t108 * t90
      t110 = t92 * t33
      t113 = x2 * x3
      t114 = t12 * t21 * t18 + t113 + t32
      t115 = t38 * t114
      t117 = t109 * t110 * t115
      t121 = t74 * t90 * t93 * t98
      t123 = t61 * t50
      t124 = t56 * t33
      t125 = t114 ** 2
      t128 = t123 * t124 * t38 * t125
      t131 = t13 * t90
      t132 = t92 * t125
      t134 = t74 * t10 * t131 * t132
      t137 = t125 * t114
      t141 = 0.144D3 * t74 * t9 * t51 * t56 * t137
      t143 = t13 * t12
      t144 = t143 * t39
      t145 = t19 * t114
      t149 = t90 * t92
      t151 = t33 * t9
      t152 = x3 * t114
      t154 = t89 * t149 * t151 * t152
      t156 = t50 * t56
      t157 = t74 * t156
      t160 = t157 * t76 * t114 * t36
      t162 = -0.328D3 * t43 + 0.36D2 * t45 * t46 * t48 * t51 * t53 * t56
     # + 0.192D3 * t67 + 0.182D3 * t72 + 0.28D2 * t74 * t50 * t78 * t36 
     #+ 0.28D2 * t83 * t39 * t85 * t36 + 0.56D2 * t89 * t90 * t93 * t36 
     #+ 0.36D2 * t100 + 0.28D2 * t106 - 0.880D3 * t117 + 0.36D2 * t121 +
     # 0.312D3 * t128 + 0.216D3 * t134 - t141 - 0.144D3 * t74 * t11 * t1
     #44 * t145 - 0.112D3 * t154 - 0.112D3 * t160
      t164 = t33 * t10
      t167 = t74 * t149 * t164 * t64 * t114
      t169 = t61 * t149
      t170 = t33 * t38
      t171 = x2 * t9
      t173 = t169 * t170 * t171
      t175 = t6 * t5
      t176 = t3 * t175
      t178 = t52 * t10
      t180 = t176 * t156 * t170 * t178
      t182 = t45 * t175
      t183 = t10 * t12
      t185 = t125 * x2
      t187 = t182 * t183 * t156 * t185
      t190 = t114 * x2
      t198 = 0.16D2 * t182 * t104 * t149 * t114 * t33
      t199 = t56 * t76
      t201 = t123 * t199 * t115
      t203 = t11 * t12
      t207 = t92 * x3 * t114
      t209 = t182 * t203 * t90 * x2 * t207
      t211 = t8 * t156
      t214 = t211 * t170 * t190 * t9
      t216 = t13 ** 2
      t224 = t45 * t5
      t225 = t39 * t19
      t229 = t50 * x1
      t230 = t54 ** 2
      t231 = 0.1D1 / t230
      t232 = t229 * t231
      t233 = t76 ** 2
      t246 = t48 * t12
      t248 = t39 * t64
      t250 = t182 * t246 * t248 * t41
      t252 = t6 * t82
      t253 = t45 * t252
      t255 = t90 * x3
      t256 = t52 * t92
      t260 = -0.144D3 * t167 + 0.96D2 * t173 + 0.47D2 * t180 + 0.216D3 *
     # t187 - 0.288D3 * t182 * t14 * t149 * t190 + t198 - 0.240D3 * t201
     # - 0.112D3 * t209 - 0.128D3 * t214 + 0.36D2 * t74 * t48 * t216 * x
     #1 + 0.108D3 * t89 * t156 * t77 + 0.36D2 * t224 * t225 * t33 + 0.36
     #D2 * t74 * t232 * t233 + 0.108D3 * t83 * t149 * t76 + 0.56D2 * t18
     #2 * t48 * t13 * t39 * x3 * t41 + 0.36D2 * t250 + 0.28D2 * t253 * t
     #246 * t255 * t256
      t264 = t74 * t203 * t248 * t145
      t268 = t74 * t14 * t225 * t152
      t272 = t74 * t183 * t255 * t132
      t278 = t8 * t149 * t170 * t10 * x3 * x2
      t281 = t38 * t90
      t283 = t176 * t203 * t281 * t256
      t285 = t76 * t38
      t287 = t169 * t285 * t36
      t292 = t8 * t203 * t40 * t113 * t19
      t296 = t61 * t225 * t170 * t98
      t300 = t169 * t170 * t171 * z
      t301 = 0.128D3 * t300
      t303 = t211 * t285 * t171
      t308 = t169 * t170 * t114 * t9 * x3
      t310 = t108 * t225
      t314 = t310 * t170 * z * t9 * x3
      t320 = 0.16D2 * t182 * t36 * t232 * t137 * t33
      t321 = t61 * t183
      t324 = t321 * t40 * t145 * x3
      t326 = t74 * t229
      t330 = 0.144D3 * t326 * t231 * t33 * t137
      t339 = -0.144D3 * t264 - 0.112D3 * t268 + 0.168D3 * t272 + 0.94D2 
     #* t278 + 0.430D3 * t283 - 0.148D3 * t287 - 0.530D3 * t292 + 0.47D2
     # * t296 - t301 - 0.148D3 * t303 - 0.128D3 * t308 - 0.128D3 * t314 
     #+ t320 + 0.400D3 * t324 - t330 - 0.144D3 * t83 * t90 * t110 * t114
     # - 0.144D3 * t326 * t231 * t77 * t114
      t340 = t89 * t50
      t342 = t340 * t124 * t125
      t344 = t74 * t48
      t347 = t344 * t13 * x1 * t64
      t355 = t344 * t12 * x1 * t103
      t358 = t109 * t93 * t38
      t362 = t326 * t231 * t76 * t125
      t364 = t3 * t5
      t367 = t364 * t39 * t85 * t38
      t370 = t123 * t78 * t38
      t374 = t62 * t143 * t38 * x1
      t380 = t310 * t170 * t36
      t384 = t40 * t145
      t385 = t61 * t10 * t13 * t384
      t388 = t92 * t114
      t391 = t8 * t183 * t281 * t388 * x2
      t393 = t9 * t12
      t396 = t61 * t393 * t281 * t132
      t399 = t114 * t52
      t405 = t157 * t151 * x3 * t125
      t415 = 0.216D3 * t342 + 0.36D2 * t347 + 0.28D2 * t344 * t143 * x1 
     #* x3 + 0.28D2 * t355 - 0.16D2 * t358 + 0.216D3 * t362 + 0.748D3 * 
     #t367 - 0.12D2 * t370 + 0.10D2 * t374 - 0.288D3 * t340 * t199 * t11
     #4 + 0.96D2 * t380 + 0.328D3 * t385 - 0.584D3 * t391 + 0.56D2 * t39
     #6 - 0.144D3 * t253 * t203 * t156 * t399 + 0.168D3 * t405 + 0.108D3
     # * t253 * t48 * t131 * t256 + 0.108D3 * t182 * t48 * t144 * t41
      t420 = t321 * t42
      t434 = 0.604D3 * t43 + 0.256D3 * t420 - 0.30D2 * t67 - 0.108D3 * t
     #72 - 0.72D2 * t100 - 0.56D2 * t106 + 0.640D3 * t117 - 0.72D2 * t12
     #1 - 0.264D3 * t128 - 0.72D2 * t134 + t141 + 0.144D3 * t154 + 0.144
     #D3 * t160 + 0.256D3 * t167 - 0.640D3 * t173
      t440 = t3 * t38
      t445 = t440 * t7 * t50 * t56 * t125 * t171
      t451 = t440 * t82
      t457 = t108 * t10
      t459 = t457 * t63 * t70
      t462 = t440 * t6 * t90
      t464 = t462 * t388 * t171
      t467 = -0.54D2 * t180 - 0.72D2 * t187 - t198 - 0.64D2 * t201 + 0.1
     #44D3 * t209 + 0.496D3 * t214 + 0.64D2 * t445 - 0.72D2 * t250 + 0.2
     #56D3 * t264 + 0.144D3 * t268 - 0.344D3 * t272 + 0.64D2 * t451 * t3
     #9 * x2 * t9 * t19 - 0.256D3 * t459 - 0.128D3 * t464 - 0.108D3 * t2
     #78
      t474 = 0.32D2 * t182 * t98 * t156 * t125 * t33
      t475 = t440 * t5
      t476 = x1 * t9
      t478 = t475 * t476 * x3
      t481 = t457 * t69 * x1
      t490 = t462 * t36 * t132
      t498 = -0.542D3 * t283 + t474 + 0.64D2 * t478 - 0.256D3 * t481 + 0
     #.136D3 * t364 * t9 * t63 * x1 + 0.216D3 * t287 + 0.444D3 * t292 + 
     #0.64D2 * t490 - 0.54D2 * t296 + 0.256D3 * t300 + 0.216D3 * t303 + 
     #0.496D3 * t308 + 0.256D3 * t314 - t320 - 0.288D3 * t324
      t512 = t108 * t393 * t384
      t517 = t440 * t82 * t39 * t36 * t145
      t520 = t330 - 0.72D2 * t342 - 0.72D2 * t347 - 0.56D2 * t355 + 0.60
     #8D3 * t358 - 0.72D2 * t362 - 0.592D3 * t367 - 0.16D2 * t370 - 0.62
     #D2 * t374 - 0.640D3 * t380 - 0.256D3 * t385 + 0.544D3 * t391 + 0.2
     #4D2 * t396 - 0.216D3 * t512 - 0.128D3 * t517 - 0.344D3 * t405
      t541 = x2 * t10
      t546 = t440 * t6 * t39
      t552 = -0.27920D5 * t43 - 0.16D2 * t3 * t252 * t232 * t399 * t164 
     #* t38 + 0.25264D5 * t420 - 0.28792D5 * t67 - 0.29128D5 * t72 + 0.1
     #44D3 * t106 - 0.128D3 * t117 + 0.64D2 * t128 - 0.288D3 * t167 + 0.
     #4D1 * t180 - t198 - 0.192D3 * t440 * t7 * t90 * t541 * t207 + 0.64
     #D2 * t546 * t541 * t19 * x3 + 0.128D3 * t201
      t575 = x1 * t10
      t580 = z ** 2
      t605 = 0.16D2 * t176 * t232 * t185 * t151 * t38 + 0.48D2 * t440 * 
     #t175 * t90 * t11 * x3 * t256 - 0.8D1 * t214 - 0.96D2 * t546 * t98 
     #* t145 - 0.96D2 * t440 * t175 * t50 * t56 * t114 * t178 + 0.128D3 
     #* t445 - 0.384D3 * t451 * t575 * t64 * z - 0.384D3 * t475 * t476 *
     # x3 * t580 + 0.384D3 * t475 * t476 * x3 * z + 0.32D2 * t440 * t7 *
     # t90 * t52 * t10 * t92 + 0.16D2 * t440 * t252 * t50 * t53 * t11 * 
     #t56 - 0.288D3 * t264 + 0.144D3 * t272 - 0.25248D5 * t459 - 0.64D2 
     #* t464
      t624 = 0.8D1 * t278 + 0.28208D5 * t283 + 0.48D2 * t440 * t7 * t39 
     #* t11 * t64 * t41 + t474 - 0.128D3 * t478 - 0.25296D5 * t481 - 0.1
     #28D3 * t287 + 0.536D3 * t292 + 0.128D3 * t490 - 0.252D3 * t296 - t
     #301 - 0.8D1 * t308 - 0.512D3 * t314 - t320 + 0.74288D5 * t324
      t651 = 0.144D3 * t355 - 0.128D3 * t358 + 0.64D2 * t367 + 0.64D2 * 
     #t370 - 0.304D3 * t374 + 0.128D3 * t224 * t9 * x3 * t580 * z * x1 +
     # 0.288D3 * t451 * t575 * t64 - 0.112D3 * t440 * t6 * x1 * t11 * t1
     #03 + 0.256D3 * t380 + 0.74320D5 * t385 - 0.74208D5 * t391 + 0.32D2
     # * t396 - 0.80D2 * t512 - 0.64D2 * t517 + 0.144D3 * t405
      rrgq2qgh61J5 = -(wd * (t162 + t260 + t339 + t415) + wd * (t434 + t
     #467 + t498 + t520) + wd * (t552 + t605 + t624 + t651)) / t1 / t38 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh61J6
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
      t6 = t5 ** 2
      t7 = t6 * t5
      t8 = t3 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 ** 2
      t12 = 0.1D1 - x3
      t15 = x1 ** 2
      t16 = x3 ** 2
      t17 = t15 * t16
      t19 = z + x1 * t4
      t20 = 0.1D1 / t19
      t21 = x2 * t20
      t25 = t6 * t4
      t26 = t3 * t25
      t27 = t10 * t9
      t28 = t27 * t12
      t30 = 0.1D1 - x2
      t33 = x2 * x3
      t35 = cos(x4 * 0.3141592653589793D1)
      t36 = x3 * t30
      t40 = Sqrt(t36 * t19 * x2 * t12)
      t42 = 0.2D1 * t35 * t40
      t43 = t12 * t30 * t19 + t33 + t42
      t44 = t20 * t43
      t46 = t26 * t28 * t17 * t44
      t48 = t12 ** 2
      t49 = t27 * t48
      t51 = t15 * t20
      t52 = x3 * t43
      t56 = t10 * t12
      t58 = t15 * x1
      t60 = t19 ** 2
      t61 = 0.1D1 / t60
      t62 = t43 ** 2
      t63 = t61 * t62
      t65 = t26 * t56 * t58 * x3 * t63
      t70 = t61 * x3 * t43
      t74 = t2 * s
      t75 = t74 * t25
      t76 = t15 ** 2
      t78 = 0.1D1 / t60 / t19
      t79 = t76 * t78
      t80 = t75 * t79
      t83 = t36 * t19 + x2 * t12 - t42
      t84 = s * t4
      t88 = t9 * x3
      t90 = s - t84 * x1 * t20 * t83 - t84 * t88
      t91 = t83 * t90
      t95 = t80 * t91 * t43 * x2 * t9
      t97 = t74 * t90
      t99 = t97 * t6 * t58
      t100 = t61 * t43
      t101 = x2 * t9
      t103 = t99 * t100 * t101
      t105 = t26 * t11
      t111 = t16 * x3
      t113 = t105 * t12 * x1 * t111
      t115 = t5 * t4
      t116 = t74 * t115
      t117 = t116 * t58
      t118 = t83 ** 2
      t119 = t61 * t118
      t121 = t117 * t119 * t90
      t123 = t76 * x1
      t124 = t26 * t123
      t125 = t60 ** 2
      t126 = 0.1D1 / t125
      t131 = t74 * t5
      t133 = t20 * t83
      t135 = t131 * t15 * t133 * t90
      t137 = t74 * t6
      t138 = t137 * t76
      t142 = t138 * t78 * t118 * t83 * t90
      t144 = t137 * t27
      t148 = t144 * t48 * t12 * t90 * x1
      t151 = t62 * t43
      t155 = -0.72D2 * t8 * t11 * t12 * t17 * t21 + 0.256D3 * t46 + 0.14
     #4D3 * t26 * t49 * t51 * t52 - 0.344D3 * t65 + 0.144D3 * t8 * t28 *
     # t58 * x2 * t70 + 0.496D3 * t95 - 0.128D3 * t103 - 0.72D2 * t105 *
     # t48 * x1 * t16 - 0.56D2 * t113 + 0.608D3 * t121 - 0.72D2 * t124 *
     # t126 * t118 * t62 - 0.592D3 * t135 - 0.16D2 * t142 - 0.62D2 * t14
     #8 + 0.144D3 * t124 * t126 * t83 * t151
      t156 = t3 * t6
      t158 = t78 * t83
      t162 = t97 * t5
      t163 = x1 * t9
      t165 = t162 * t163 * x3
      t167 = t116 * t10
      t168 = t48 * t90
      t170 = t167 * t168 * x1
      t173 = t10 * t16
      t175 = t137 * t51 * t91 * t173
      t177 = t118 * t90
      t181 = t58 * t61
      t182 = t137 * t181
      t186 = t137 * t56
      t187 = t90 * t15
      t190 = t186 * t187 * t44 * x3
      t193 = t90 * t58
      t196 = t75 * t56 * t193 * t100 * x2
      t198 = t74 * t7
      t200 = x2 ** 2
      t201 = t200 * t10
      t203 = t198 * t79 * t91 * t201
      t206 = t62 * x2
      t210 = t26 * t79
      t211 = t83 * t9
      t214 = t210 * t211 * x3 * t62
      t224 = t12 * t90
      t227 = t144 * t224 * x1 * t16
      t229 = x3 * x1
      t231 = t144 * t168 * t229
      t233 = -0.72D2 * t156 * t76 * t158 * t62 + 0.64D2 * t165 - 0.256D3
     # * t170 - 0.54D2 * t175 + 0.216D3 * t80 * t177 * t101 - 0.640D3 * 
     #t182 * t91 * t101 - 0.288D3 * t190 + 0.544D3 * t196 - 0.54D2 * t20
     #3 - 0.72D2 * t8 * t56 * t79 * t206 - 0.344D3 * t214 + 0.144D3 * t1
     #56 * t181 * t211 * t52 + 0.144D3 * t210 * t118 * t43 * t88 - 0.30D
     #2 * t227 - 0.108D3 * t231
      t240 = t27 * t111
      t242 = t26 * t15 * t133 * t240
      t245 = t90 * t43
      t247 = t117 * t61 * t83 * t245
      t251 = t138 * t78 * t118 * t245
      t259 = t138 * t158 * t90 * t62
      t272 = t97 * t115
      t279 = t167 * t224 * t229
      t282 = t123 * t126
      t286 = 0.16D2 * t8 * t88 * t282 * t151 * t83
      t287 = t9 * t12
      t290 = t137 * t287 * t193 * t63
      t292 = t116 * t51
      t294 = t292 * t91 * t88
      t297 = t182 * t177 * t88
      t301 = t187 * t44
      t302 = t137 * t10 * t48 * t301
      t304 = -0.72D2 * t156 * t15 * t133 * t173 - 0.56D2 * t242 + 0.640D
     #3 * t247 - 0.64D2 * t251 - 0.72D2 * t26 * t58 * t119 * t173 - 0.26
     #4D3 * t259 - 0.72D2 * t26 * t10 * t48 * t58 * t63 + 0.144D3 * t26 
     #* t9 * t12 * t76 * t78 * t151 + 0.64D2 * t272 * t15 * x2 * t9 * t2
     #0 - 0.256D3 * t279 - t286 + 0.24D2 * t290 - 0.640D3 * t294 + 0.216
     #D3 * t297 - 0.256D3 * t302
      t308 = t97 * t115 * t15 * t88 * t44
      t311 = t99 * t88 * t63
      t317 = t75 * t181 * t91 * t10 * x3 * x2
      t320 = t200 * t61
      t322 = t198 * t28 * t193 * t320
      t325 = t187 * t21
      t326 = t75 * t49 * t325
      t331 = t75 * t28 * t187 * t33 * t20
      t337 = t186 * t325
      t340 = t116 * t287 * t301
      t343 = t83 * t10
      t346 = t26 * t181 * t343 * t16 * t43
      t351 = t182 * t91 * t43 * t9 * x3
      t356 = t292 * t91 * z * t9 * x3
      t360 = t182 * t91 * t101 * z
      t366 = t97 * t25 * t76 * t78 * t62 * t101
      t372 = 0.32D2 * t8 * t173 * t79 * t62 * t83
      t377 = 0.16D2 * t8 * t240 * t181 * t43 * t83
      t378 = -0.128D3 * t308 + 0.64D2 * t311 - 0.108D3 * t317 - 0.542D3 
     #* t322 + 0.604D3 * t326 + 0.444D3 * t331 + 0.136D3 * t131 * t9 * t
     #224 * x1 + 0.256D3 * t337 - 0.216D3 * t340 + 0.256D3 * t346 + 0.49
     #6D3 * t351 + 0.256D3 * t356 + 0.256D3 * t360 + 0.64D2 * t366 + t37
     #2 - t377
      t396 = -0.288D3 * t46 + 0.144D3 * t65 - 0.8D1 * t95 - 0.64D2 * t10
     #3 + 0.144D3 * t113 - 0.128D3 * t121 + 0.64D2 * t135 + 0.64D2 * t14
     #2 - 0.304D3 * t148 - 0.128D3 * t165 - 0.25296D5 * t170 - 0.252D3 *
     # t175 + 0.74288D5 * t190 - 0.74208D5 * t196
      t406 = x1 * t10
      t411 = z ** 2
      t426 = t6 * t115
      t434 = 0.4D1 * t203 + 0.144D3 * t214 - 0.28792D5 * t227 - 0.29128D
     #5 * t231 + 0.144D3 * t242 - 0.128D3 * t247 + 0.128D3 * t251 + 0.64
     #D2 * t259 - 0.25248D5 * t279 - 0.384D3 * t272 * t406 * t16 * z - 0
     #.384D3 * t162 * t163 * x3 * t411 + 0.384D3 * t162 * t163 * x3 * z 
     #+ 0.32D2 * t97 * t25 * t58 * t200 * t10 * t61 + 0.16D2 * t97 * t42
     #6 * t76 * t200 * x2 * t27 * t78 - t286
      t461 = t97 * t6 * t15
      t462 = x2 * t10
      t479 = 0.32D2 * t290 + 0.256D3 * t294 - 0.128D3 * t297 + 0.74320D5
     # * t302 - 0.96D2 * t97 * t7 * t76 * t78 * t43 * t201 - 0.16D2 * t7
     #4 * t426 * t282 * t43 * t200 * t343 * t90 + 0.16D2 * t198 * t282 *
     # t206 * t211 * t90 - 0.64D2 * t308 + 0.128D3 * t311 + 0.64D2 * t46
     #1 * t462 * t20 * x3 + 0.48D2 * t97 * t25 * t15 * t27 * t16 * t21 -
     # 0.96D2 * t461 * t173 * t44 + 0.8D1 * t317 + 0.28208D5 * t322 - 0.
     #27920D5 * t326
      t514 = 0.536D3 * t331 + 0.128D3 * t3 * t5 * t9 * x3 * t411 * z * x
     #1 + 0.288D3 * t272 * t406 * t16 - 0.112D3 * t97 * t6 * x1 * t27 * 
     #t111 + 0.25264D5 * t337 - 0.80D2 * t340 - 0.288D3 * t346 - 0.8D1 *
     # t351 - 0.512D3 * t356 - 0.128D3 * t360 + 0.128D3 * t366 + t372 - 
     #t377 - 0.192D3 * t97 * t25 * t58 * t462 * t70 + 0.48D2 * t97 * t7 
     #* t58 * t27 * x3 * t320
      rrgq2qgh61J6 = -(wd * (t155 + t233 + t304 + t378) + wd * (t396 + t
     #434 + t479 + t514)) / t1 / t90 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh61J7
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
      t9 = x1 ** 2
      t10 = t9 ** 2
      t13 = z + x1 * t4
      t14 = t13 ** 2
      t15 = t14 ** 2
      t17 = t10 * x1 / t15
      t19 = 0.1D1 - x3
      t20 = 0.1D1 - x2
      t23 = x2 * x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = x3 * t20
      t30 = Sqrt(t26 * t13 * x2 * t19)
      t32 = 0.2D1 * t25 * t30
      t33 = t19 * t20 * t13 + t23 + t32
      t34 = t33 ** 2
      t36 = 0.1D1 - x1
      t39 = t26 * t13 + x2 * t19 - t32
      t40 = t36 * t39
      t41 = s * t4
      t42 = 0.1D1 / t13
      t46 = t36 * x3
      t48 = s - t41 * x1 * t42 * t39 - t41 * t46
      t53 = t3 * t48
      t54 = x1 * t9
      t57 = t36 ** 2
      t58 = t57 * t36
      t60 = x2 ** 2
      t61 = 0.1D1 / t14
      t62 = t60 * t61
      t66 = t5 * t4
      t67 = t3 * t66
      t68 = t36 * t19
      t70 = t48 * t9
      t71 = t42 * t33
      t72 = t70 * t71
      t75 = t2 * t1
      t76 = t6 * t4
      t77 = t75 * t76
      t78 = t57 * t19
      t81 = t61 * t34
      t85 = t3 * t76
      t87 = 0.1D1 / t14 / t13
      t88 = t10 * t87
      t90 = t39 * t48
      t98 = z ** 2
      t104 = t53 * t66
      t105 = x1 * t57
      t106 = x3 ** 2
      t113 = x2 * t36
      t121 = t53 * t5
      t122 = x1 * t36
      t137 = t6 * t66
      t145 = t3 * t6
      t146 = t145 * t58
      t147 = t19 * t48
      t152 = 0.16D2 * t8 * t17 * t34 * x2 * t40 * t48 + 0.48D2 * t53 * t
     #7 * t54 * t58 * x3 * t62 - 0.80D2 * t67 * t68 * t72 + 0.144D3 * t7
     #7 * t78 * t54 * x3 * t81 - 0.8D1 * t85 * t88 * t90 * t33 * x2 * t3
     #6 + 0.128D3 * t75 * t5 * t36 * x3 * t98 * z * x1 + 0.288D3 * t104 
     #* t105 * t106 + 0.128D3 * t53 * t76 * t10 * t87 * t34 * t113 - 0.3
     #84D3 * t104 * t105 * t106 * z - 0.384D3 * t121 * t122 * x3 * t98 +
     # 0.384D3 * t121 * t122 * x3 * z + 0.32D2 * t53 * t76 * t54 * t60 *
     # t57 * t61 + 0.16D2 * t53 * t137 * t10 * t60 * x2 * t58 * t87 - 0.
     #28792D5 * t146 * t147 * x1 * t106
      t153 = t19 ** 2
      t154 = t153 * t48
      t155 = x3 * x1
      t160 = t42 * t39
      t161 = t106 * x3
      t162 = t58 * t161
      t166 = t67 * t54
      t168 = t48 * t33
      t172 = t145 * t10
      t173 = t39 ** 2
      t183 = t67 * t57
      t193 = t53 * t6 * t54
      t197 = t9 * t42
      t198 = t67 * t197
      t202 = t54 * t61
      t203 = t145 * t202
      t209 = t57 * t106
      t214 = t60 * t57
      t226 = t57 * t39
      t232 = t53 * t6 * t9
      t236 = -0.29128D5 * t146 * t154 * t155 + 0.144D3 * t77 * t9 * t160
     # * t162 - 0.128D3 * t166 * t61 * t39 * t168 + 0.128D3 * t172 * t87
     # * t173 * t168 + 0.64D2 * t172 * t87 * t39 * t48 * t34 - 0.25248D5
     # * t183 * t147 * t155 - 0.64D2 * t53 * t66 * t9 * t46 * t71 + 0.12
     #8D3 * t193 * t46 * t81 + 0.256D3 * t198 * t90 * t46 - 0.128D3 * t2
     #03 * t173 * t48 * t46 - 0.252D3 * t145 * t197 * t90 * t209 + 0.4D1
     # * t8 * t88 * t90 * t214 - 0.512D3 * t198 * t90 * z * t36 * x3 - 0
     #.16D2 * t3 * t137 * t17 * t33 * t60 * t226 * t48 - 0.96D2 * t232 *
     # t209 * t71
      t238 = t75 * t7
      t266 = t57 ** 2
      t297 = t61 * t33
      t303 = x2 * t57
      t314 = 0.32D2 * t238 * t209 * t88 * t34 * t39 - 0.16D2 * t238 * t1
     #62 * t202 * t33 * t39 - 0.16D2 * t238 * t46 * t17 * t34 * t33 * t3
     #9 + 0.8D1 * t85 * t202 * t90 * t57 * x3 * x2 - 0.112D3 * t53 * t6 
     #* x1 * t58 * t161 + 0.144D3 * t77 * t266 * t19 * x1 * t161 - 0.128
     #D3 * t166 * t61 * t173 * t48 + 0.64D2 * t3 * t5 * t9 * t160 * t48 
     #+ 0.64D2 * t172 * t87 * t173 * t39 * t48 - 0.304D3 * t146 * t153 *
     # t19 * t48 * x1 - 0.128D3 * t121 * t122 * x3 - 0.25296D5 * t183 * 
     #t154 * x1 - 0.64D2 * t193 * t297 * t113 - 0.192D3 * t53 * t76 * t5
     #4 * t303 * t61 * x3 * t33 + 0.144D3 * t77 * t88 * t40 * x3 * t34
      t331 = t145 * t78
      t336 = t58 * t19
      t354 = x2 * t42
      t359 = t48 * t54
      t382 = t70 * t354
      t387 = -0.288D3 * t77 * t202 * t226 * t106 * t33 - 0.8D1 * t203 * 
     #t90 * t33 * t36 * x3 - 0.96D2 * t53 * t7 * t10 * t87 * t33 * t214 
     #+ 0.74288D5 * t331 * t70 * t71 * x3 + 0.536D3 * t85 * t336 * t70 *
     # t23 * t42 - 0.288D3 * t77 * t336 * t9 * t106 * t71 + 0.64D2 * t23
     #2 * t303 * t42 * x3 + 0.48D2 * t53 * t76 * t9 * t58 * t106 * t354 
     #- 0.74208D5 * t85 * t78 * t359 * t297 * x2 + 0.32D2 * t145 * t68 *
     # t359 * t81 + 0.74320D5 * t145 * t57 * t153 * t72 - 0.128D3 * t203
     # * t90 * t113 * z + 0.28208D5 * t8 * t336 * t359 * t62 - 0.27920D5
     # * t85 * t58 * t153 * t382 + 0.25264D5 * t331 * t382
      rrgq2qgh61J7 = -wd * (t152 + t236 + t314 + t387) / t1 / t48 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh62J1
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
      t7 = x1 ** 2
      t8 = t3 * t5 * t7
      t9 = x1 * t4
      t10 = z + t9
      t11 = 0.1D1 / t10
      t12 = 0.1D1 - x2
      t13 = x3 * t12
      t15 = 0.1D1 - x3
      t18 = cos(x4 * 0.3141592653589793D1)
      t22 = Sqrt(t13 * t10 * x2 * t15)
      t24 = 0.2D1 * t18 * t22
      t25 = t13 * t10 + x2 * t15 - t24
      t26 = t11 * t25
      t27 = s * t4
      t31 = x2 * x3
      t32 = t15 * t12 * t10 + t31 + t24
      t35 = 0.1D1 - x1
      t38 = s - t27 * x1 * t11 * t32 - t27 * t35 * t15
      t43 = z ** 2
      t48 = t2 * t1
      t49 = t48 * z
      t50 = t5 ** 2
      t52 = t7 ** 2
      t53 = t10 ** 2
      t55 = 0.1D1 / t53 / t10
      t56 = t52 * t55
      t57 = t32 ** 2
      t62 = t5 * t4
      t64 = t7 * x1
      t65 = 0.1D1 / t53
      t66 = t64 * t65
      t71 = t3 * z
      t73 = x1 * t38
      t74 = t35 * x3
      t78 = t71 * t62
      t79 = t35 ** 2
      t80 = x3 ** 2
      t81 = t79 * t80
      t90 = t3 * t50
      t91 = t90 * t52
      t92 = t25 ** 2
      t98 = t90 * x1
      t99 = t79 * t35
      t105 = t3 * t62
      t106 = t105 * x1
      t108 = t38 * x3
      t112 = t15 ** 2
      t117 = t7 * t11
      t119 = t25 * t38
      t123 = t90 * t66
      t124 = t74 * t32
      t128 = 0.1728D4 * t8 * t26 * t38 * z - 0.1728D4 * t8 * t26 * t38 *
     # t43 + 0.576D3 * t49 * t50 * t56 * t25 * t57 + 0.1152D4 * t49 * t6
     #2 * t66 * t25 * t32 + 0.288D3 * t71 * t5 * t73 * t74 - 0.144D3 * t
     #78 * t73 * t81 - 0.288D3 * t78 * t64 * t38 * t65 * t57 + 0.1440D4 
     #* t91 * t55 * t92 * t38 * t32 - 0.144D3 * t98 * t99 * t15 * t38 * 
     #t80 + 0.14832D5 * t106 * t79 * t15 * t108 - 0.144D3 * t98 * t99 * 
     #t112 * t108 + 0.144D3 * t105 * t117 * t119 * t74 + 0.144D3 * t123 
     #* t119 * t124
      t129 = t50 * t5
      t130 = t3 * t129
      t138 = t90 * t7 * t79
      t139 = t15 * t38
      t141 = x3 * t11 * t32
      t145 = t50 * t4
      t146 = t3 * t145
      t148 = t146 * t7 * t99
      t149 = t31 * t11
      t159 = t146 * t66
      t161 = t79 * x3 * x2
      t165 = t50 * t64
      t166 = t49 * t165
      t167 = t65 * t25
      t168 = x2 * t35
      t172 = t112 * t38
      t173 = t11 * t32
      t177 = x2 * t11
      t189 = x2 ** 2
      t201 = t145 * t64
      t206 = t62 * t7
      t207 = t71 * t206
      t213 = 0.108D3 * t130 * t66 * t119 * t99 * t80 * x2 - 0.14976D5 * 
     #t138 * t139 * t141 + 0.288D3 * t148 * t139 * t149 + 0.36D2 * t146 
     #* t117 * t119 * t99 * t80 * x3 - 0.576D3 * t159 * t119 * t161 - 0.
     #1152D4 * t166 * t167 * t168 - 0.14832D5 * t138 * t172 * t173 + 0.1
     #44D3 * t148 * t172 * t177 + 0.144D3 * t123 * t119 * t168 - 0.14976
     #D5 * t138 * t139 * t177 - 0.144D3 * t130 * t64 * t99 * t139 * t189
     # * t65 + 0.14832D5 * t146 * t64 * t79 * t139 * t65 * t32 * x2 + 0.
     #1152D4 * t49 * t201 * t167 * t161 + 0.288D3 * t207 * t38 * x2 * t3
     #5 * t11
      t218 = t32 * x2 * t35
      t222 = t146 * t56
      t231 = t53 ** 2
      t233 = t52 * x1 / t231
      t235 = t92 * t25
      t236 = t235 * t38
      t241 = t3 * t50 * t62
      t255 = t50 * t7
      t263 = t55 * t25
      t267 = t92 * t38
      t271 = t130 * t56
      t272 = t189 * t79
      t283 = 0.288D3 * t71 * t165 * t38 * t65 * t218 + 0.144D3 * t222 * 
     #t119 * t218 - 0.288D3 * t90 * t117 * t119 * t81 + 0.72D2 * t130 * 
     #t233 * t236 * t168 + 0.108D3 * t241 * t56 * t119 * t99 * x3 * t189
     # - 0.288D3 * t71 * t9 * t38 + 0.288D3 * t207 * t38 * t35 * t141 - 
     #0.288D3 * t71 * t255 * t38 * t79 * t149 - 0.1152D4 * t49 * t145 * 
     #t52 * t263 * t218 + 0.72D2 * t159 * t267 * t81 - 0.288D3 * t271 * 
     #t119 * t272 - 0.1152D4 * t166 * t167 * t124 + 0.72D2 * t241 * t233
     # * t267 * t272
      t324 = t50 ** 2
      t340 = t117 * t25
      t349 = 0.1440D4 * t91 * t55 * t235 * t38 - 0.144D3 * t71 * t201 * 
     #t38 * t189 * t79 * t65 + 0.14832D5 * t106 * t79 * t112 * t38 - 0.1
     #440D4 * t105 * t64 * t65 * t92 * t38 + 0.576D3 * t105 * t7 * t35 *
     # t139 * t173 + 0.576D3 * t49 * t129 * t52 * t263 * t272 - 0.1152D4
     # * t49 * t206 * t26 * t74 + 0.576D3 * t49 * t255 * t26 * t81 + 0.1
     #44D3 * t271 * t267 * t161 + 0.36D2 * t3 * t324 * t233 * t119 * t18
     #9 * x2 * t99 - 0.1728D4 * t105 * t66 * t119 * z * t32 + 0.576D3 * 
     #t48 * t43 * z * t5 * t340 + 0.576D3 * t49 * t5 * t340 + 0.72D2 * t
     #222 * t236 * t74
      rrgq2qgh62J1 = -wd * (t128 + t213 + t283 + t349) / t1 / t38 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh62J2
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
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t12 = t7 * t8 * t10
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t15 = s * t4
      t16 = x1 * t4
      t17 = z + t16
      t18 = 0.1D1 / t17
      t20 = 0.1D1 - x2
      t23 = x2 * x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = x3 * t20
      t30 = Sqrt(t26 * t17 * x2 * t13)
      t32 = 0.2D1 * t25 * t30
      t33 = t13 * t20 * t17 + t23 + t32
      t36 = t9 * t13
      t38 = s - t15 * x1 * t18 * t33 - t15 * t36
      t39 = t14 * t38
      t40 = t18 * t33
      t42 = t12 * t39 * t40
      t44 = t13 * t38
      t45 = x2 * t18
      t47 = t12 * t44 * t45
      t49 = t8 * t18
      t53 = t26 * t17 + x2 * t13 - t32
      t54 = t53 * t38
      t55 = x3 ** 2
      t56 = t10 * t55
      t60 = t5 * t4
      t61 = t3 * t60
      t63 = t9 * x3
      t65 = t61 * t49 * t54 * t63
      t67 = t3 * z
      t68 = t60 * t8
      t69 = t67 * t68
      t74 = 0.288D3 * t69 * t38 * x2 * t9 * t18
      t75 = t8 * x1
      t76 = t6 * t75
      t78 = t17 ** 2
      t79 = 0.1D1 / t78
      t82 = t33 * x2 * t9
      t85 = 0.288D3 * t67 * t76 * t38 * t79 * t82
      t88 = x3 * t18 * t33
      t91 = 0.288D3 * t69 * t38 * t9 * t88
      t92 = t6 * t8
      t95 = t23 * t18
      t97 = t67 * t92 * t38 * t10 * t95
      t99 = t2 * t1
      t100 = t99 * z
      t101 = t6 * t4
      t102 = t8 ** 2
      t106 = 0.1D1 / t78 / t17
      t107 = t106 * t53
      t111 = t3 * t101
      t112 = t10 * t9
      t114 = t111 * t8 * t112
      t116 = t114 * t44 * t95
      t119 = t114 * t39 * t45
      t121 = t7 * t102
      t122 = t53 ** 2
      t123 = t122 * t53
      t127 = 0.1440D4 * t121 * t106 * t123 * t38
      t128 = t61 * x1
      t131 = t128 * t10 * t14 * t38
      t133 = -0.14832D5 * t42 - 0.14976D5 * t47 - 0.288D3 * t7 * t49 * t
     #54 * t56 + 0.144D3 * t65 + t74 + t85 + t91 - 0.288D3 * t97 - 0.115
     #2D4 * t100 * t101 * t102 * t107 * t82 + 0.288D3 * t116 + 0.144D3 *
     # t119 + t127 + 0.14832D5 * t131
      t134 = t61 * t75
      t137 = t134 * t79 * t122 * t38
      t140 = t49 * t53
      t143 = z ** 2
      t148 = 0.576D3 * t99 * t143 * z * t5 * t140
      t149 = t100 * t76
      t150 = t79 * t53
      t151 = t63 * t33
      t155 = t75 * t79
      t160 = 0.1728D4 * t61 * t155 * t54 * z * t33
      t161 = t111 * t155
      t162 = t122 * t38
      t166 = t6 * t5
      t167 = t3 * t166
      t168 = t102 * t106
      t169 = t167 * t168
      t170 = x2 ** 2
      t171 = t170 * t10
      t175 = t7 * t155
      t177 = t175 * t54 * t151
      t186 = t3 * t6 * t60
      t193 = t111 * t168
      t195 = t193 * t54 * t82
      t200 = t61 * t8 * t9 * t44 * t40
      t203 = t12 * t44 * t88
      t209 = t167 * t75 * t112 * t44 * t170 * t79
      t211 = -0.1440D4 * t137 + 0.576D3 * t100 * t5 * t140 + t148 - 0.11
     #52D4 * t149 * t150 * t151 - t160 + 0.72D2 * t161 * t162 * t56 - 0.
     #288D3 * t169 * t54 * t171 + 0.144D3 * t177 + 0.108D3 * t167 * t155
     # * t54 * t112 * t55 * x2 + 0.108D3 * t186 * t168 * t54 * t112 * x3
     # * t170 + 0.144D3 * t195 + 0.576D3 * t200 - 0.14976D5 * t203 - 0.1
     #44D3 * t209
      t213 = t7 * x1
      t215 = t38 * x3
      t217 = t213 * t112 * t14 * t215
      t220 = t38 * t33
      t222 = t121 * t106 * t122 * t220
      t227 = t213 * t112 * t13 * t38 * t55
      t231 = t128 * t10 * t13 * t215
      t233 = t67 * t60
      t235 = t33 ** 2
      t236 = t79 * t235
      t239 = 0.288D3 * t233 * t75 * t38 * t236
      t240 = t3 * t5
      t241 = t240 * t8
      t242 = t18 * t53
      t246 = 0.1728D4 * t241 * t242 * t38 * t143
      t257 = t67 * t5
      t258 = x1 * t38
      t261 = 0.288D3 * t257 * t258 * t63
      t263 = t233 * t258 * t56
      t268 = 0.1728D4 * t241 * t242 * t38 * z
      t275 = t123 * t38
      t278 = 0.72D2 * t193 * t275 * t63
      t279 = -0.144D3 * t217 + 0.1440D4 * t222 - 0.144D3 * t227 + 0.1483
     #2D5 * t231 - t239 - t246 + 0.576D3 * t100 * t6 * t168 * t53 * t235
     # + 0.1152D4 * t100 * t60 * t155 * t53 * t33 + t261 - 0.144D3 * t26
     #3 + t268 + 0.36D2 * t111 * t49 * t54 * t112 * t55 * x3 + t278
      t285 = t111 * t75 * t10 * t44 * t79 * t33 * x2
      t287 = x2 * t9
      t289 = t175 * t54 * t287
      t291 = t6 ** 2
      t294 = t78 ** 2
      t296 = t102 * x1 / t294
      t307 = t101 * t75
      t310 = t10 * x3 * x2
      t318 = t67 * t307 * t38 * t170 * t10 * t79
      t337 = 0.288D3 * t67 * t16 * t38
      t345 = t167 * t296
      t348 = 0.72D2 * t345 * t275 * t287
      t349 = 0.14832D5 * t285 + 0.144D3 * t289 + 0.36D2 * t3 * t291 * t2
     #96 * t54 * t170 * x2 * t112 + 0.576D3 * t100 * t92 * t242 * t56 + 
     #0.1152D4 * t100 * t307 * t150 * t310 - 0.144D3 * t318 - 0.576D3 * 
     #t161 * t54 * t310 - 0.1152D4 * t149 * t150 * t287 + 0.576D3 * t100
     # * t166 * t102 * t107 * t171 - 0.1152D4 * t100 * t68 * t242 * t63 
     #- t337 + 0.144D3 * t169 * t162 * t310 + 0.72D2 * t186 * t296 * t16
     #2 * t171 + t348
      t370 = 0.15552D5 * t42 + 0.15552D5 * t47 - 0.288D3 * t241 * t242 *
     # t38 + 0.3096D4 * t213 * t112 * t14 * t13 * t38 + 0.432D3 * t240 *
     # x1 * t36 * t38 - 0.72D2 * t65 + t74 + t85 + t91 + 0.576D3 * t97 +
     # 0.1872D4 * t116
      t383 = -0.2160D4 * t119 - t127 - 0.15552D5 * t131 + 0.1728D4 * t13
     #7 - t148 + t160 + 0.72D2 * t345 * t54 * t235 * x2 * t9 - 0.72D2 * 
     #t177 - 0.72D2 * t195 - 0.864D3 * t200 + 0.15552D5 * t203
      t397 = -0.936D3 * t209 + 0.72D2 * t345 * t162 * t82 + 0.72D2 * t19
     #3 * t162 * t151 + 0.2160D4 * t217 - 0.1728D4 * t222 - 0.936D3 * t2
     #27 - 0.15696D5 * t231 + t239 + t246 + t261 + 0.288D3 * t263
      t422 = -t268 + 0.72D2 * t193 * t54 * t235 * t9 * x3 - t278 - 0.288
     #D3 * t121 * t107 * t38 * t235 - 0.576D3 * t257 * t8 * t38 * t40 + 
     #0.576D3 * t134 * t150 * t220 + 0.432D3 * t7 * t75 * t9 * t44 * t23
     #6 - 0.15696D5 * t285 - 0.72D2 * t289 + 0.288D3 * t318 + t337 - t34
     #8
      rrgq2qgh62J2 = -(wd * (t133 + t211 + t279 + t349) + wd * (t370 + t
     #383 + t397 + t422)) / t1 / t38 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh62J3
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
      t8 = t7 * x1
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 * t9
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t15 = s * t4
      t16 = x1 * t4
      t17 = z + t16
      t18 = 0.1D1 / t17
      t20 = 0.1D1 - x2
      t23 = x2 * x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = x3 * t20
      t30 = Sqrt(t26 * t17 * x2 * t12)
      t32 = 0.2D1 * t25 * t30
      t33 = t12 * t20 * t17 + t23 + t32
      t36 = t9 * t12
      t38 = s - t15 * x1 * t18 * t33 - t15 * t36
      t39 = t38 * x3
      t41 = t8 * t11 * t13 * t39
      t43 = t5 * t4
      t44 = t3 * t43
      t45 = t44 * x1
      t48 = t45 * t10 * t12 * t39
      t51 = x3 ** 2
      t54 = t8 * t11 * t12 * t38 * t51
      t56 = x1 ** 2
      t57 = t56 ** 2
      t58 = t7 * t57
      t59 = t17 ** 2
      t61 = 0.1D1 / t59 / t17
      t64 = t26 * t17 + x2 * t12 - t32
      t65 = t64 ** 2
      t67 = t38 * t33
      t69 = t58 * t61 * t65 * t67
      t71 = t3 * z
      t72 = t71 * t43
      t73 = t56 * x1
      t75 = 0.1D1 / t59
      t76 = t33 ** 2
      t77 = t75 * t76
      t80 = 0.288D3 * t72 * t73 * t38 * t77
      t83 = 0.288D3 * t71 * t16 * t38
      t84 = t6 * t5
      t85 = t3 * t84
      t86 = t57 * t61
      t87 = t85 * t86
      t88 = t65 * t38
      t90 = t10 * x3 * x2
      t94 = t6 * t4
      t95 = t3 * t94
      t96 = t56 * t18
      t98 = t64 * t38
      t104 = t95 * t86
      t105 = t64 * t65
      t106 = t105 * t38
      t107 = t9 * x3
      t110 = 0.72D2 * t104 * t106 * t107
      t114 = 0.1440D4 * t58 * t61 * t105 * t38
      t117 = t45 * t10 * t13 * t38
      t119 = t44 * t73
      t122 = t119 * t75 * t65 * t38
      t124 = t2 * t1
      t125 = t124 * z
      t127 = t96 * t64
      t130 = -0.144D3 * t41 + 0.14832D5 * t48 - 0.144D3 * t54 + 0.1440D4
     # * t69 - t80 - t83 + 0.144D3 * t87 * t88 * t90 + 0.36D2 * t95 * t9
     #6 * t98 * t11 * t51 * x3 + t110 + t114 + 0.14832D5 * t117 - 0.1440
     #D4 * t122 + 0.576D3 * t125 * t5 * t127
      t131 = z ** 2
      t136 = 0.576D3 * t124 * t131 * z * t5 * t127
      t138 = t7 * t56 * t10
      t139 = t13 * t38
      t140 = t18 * t33
      t142 = t138 * t139 * t140
      t145 = t95 * t56 * t11
      t146 = x2 * t18
      t148 = t145 * t139 * t146
      t150 = t12 * t38
      t151 = t23 * t18
      t153 = t145 * t150 * t151
      t155 = t6 * t73
      t156 = t125 * t155
      t157 = t75 * t64
      t158 = t107 * t33
      t162 = t73 * t75
      t167 = 0.1728D4 * t44 * t162 * t98 * z * t33
      t169 = t138 * t150 * t146
      t174 = t33 * x2 * t9
      t177 = 0.288D3 * t71 * t155 * t38 * t75 * t174
      t178 = t43 * t56
      t179 = t71 * t178
      t182 = x3 * t18 * t33
      t185 = 0.288D3 * t179 * t38 * t9 * t182
      t186 = t6 * t56
      t190 = t71 * t186 * t38 * t10 * t151
      t194 = t61 * t64
      t199 = t18 * t64
      t200 = t10 * t51
      t204 = t94 * t73
      t210 = x2 ** 2
      t214 = t71 * t204 * t38 * t210 * t10 * t75
      t216 = t136 - 0.14832D5 * t142 + 0.144D3 * t148 + 0.288D3 * t153 -
     # 0.1152D4 * t156 * t157 * t158 - t167 - 0.14976D5 * t169 + t177 + 
     #t185 - 0.288D3 * t190 - 0.1152D4 * t125 * t94 * t57 * t194 * t174 
     #+ 0.576D3 * t125 * t186 * t199 * t200 + 0.1152D4 * t125 * t204 * t
     #157 * t90 - 0.144D3 * t214
      t218 = t7 * t162
      t219 = x2 * t9
      t221 = t218 * t98 * t219
      t223 = t95 * t162
      t232 = t210 * t10
      t244 = t85 * t73 * t11 * t150 * t210 * t75
      t251 = t95 * t73 * t10 * t150 * t75 * t33 * x2
      t254 = t138 * t150 * t182
      t260 = t104 * t98 * t174
      t268 = t44 * t96 * t98 * t107
      t271 = t3 * t6 * t43
      t278 = 0.144D3 * t221 - 0.576D3 * t223 * t98 * t90 - 0.1152D4 * t1
     #56 * t157 * t219 + 0.576D3 * t125 * t84 * t57 * t194 * t232 - 0.11
     #52D4 * t125 * t178 * t199 * t107 - 0.144D3 * t244 + 0.14832D5 * t2
     #51 - 0.14976D5 * t254 - 0.288D3 * t87 * t98 * t232 + 0.144D3 * t26
     #0 - 0.288D3 * t7 * t96 * t98 * t200 + 0.144D3 * t268 + 0.108D3 * t
     #271 * t86 * t98 * t11 * x3 * t210
      t279 = x1 * t38
      t281 = t72 * t279 * t200
      t283 = t71 * t5
      t286 = 0.288D3 * t283 * t279 * t107
      t297 = t3 * t5
      t298 = t297 * t56
      t302 = 0.1728D4 * t298 * t199 * t38 * t131
      t306 = 0.1728D4 * t298 * t199 * t38 * z
      t308 = t59 ** 2
      t310 = t57 * x1 / t308
      t311 = t85 * t310
      t314 = 0.72D2 * t311 * t106 * t219
      t316 = t218 * t98 * t158
      t324 = t6 ** 2
      t339 = t44 * t56 * t9 * t150 * t140
      t348 = 0.288D3 * t179 * t38 * x2 * t9 * t18
      t349 = -0.144D3 * t281 + t286 + 0.1152D4 * t125 * t43 * t162 * t64
     # * t33 + 0.576D3 * t125 * t6 * t86 * t64 * t76 - t302 + t306 + t31
     #4 + 0.144D3 * t316 + 0.108D3 * t85 * t162 * t98 * t11 * t51 * x2 +
     # 0.36D2 * t3 * t324 * t310 * t98 * t210 * x2 * t11 + 0.72D2 * t271
     # * t310 * t88 * t232 + 0.576D3 * t339 + 0.72D2 * t223 * t88 * t200
     # + t348
      t364 = 0.2160D4 * t41 - 0.15696D5 * t48 - 0.936D3 * t54 - 0.1728D4
     # * t69 + t80 + 0.72D2 * t311 * t98 * t76 * x2 * t9 + t83 - t110 - 
     #t114 - 0.15552D5 * t117 + 0.1728D4 * t122
      t386 = -t136 - 0.288D3 * t298 * t199 * t38 + 0.3096D4 * t8 * t11 *
     # t13 * t12 * t38 + 0.432D3 * t297 * x1 * t36 * t38 + 0.15552D5 * t
     #142 - 0.2160D4 * t148 + 0.1872D4 * t153 + 0.432D3 * t7 * t73 * t9 
     #* t150 * t77 + t167 + 0.15552D5 * t169 + t177
      t404 = t185 + 0.576D3 * t190 + 0.288D3 * t214 - 0.72D2 * t221 + 0.
     #72D2 * t104 * t98 * t76 * t9 * x3 - 0.936D3 * t244 - 0.15696D5 * t
     #251 + 0.15552D5 * t254 - 0.72D2 * t260 - 0.72D2 * t268 + 0.72D2 * 
     #t311 * t88 * t174
      t422 = 0.72D2 * t104 * t88 * t158 + 0.288D3 * t281 + t286 + t302 -
     # t306 - t314 + 0.576D3 * t119 * t157 * t67 - 0.576D3 * t283 * t56 
     #* t38 * t140 - 0.288D3 * t58 * t194 * t38 * t76 - 0.72D2 * t316 - 
     #0.864D3 * t339 + t348
      rrgq2qgh62J3 = -(wd * (t130 + t216 + t278 + t349) + wd * (t364 + t
     #386 + t404 + t422)) / t1 / t38 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh62J4
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
      t6 = t5 * x1
      t7 = s * t5
      t8 = z + t6
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
      t31 = s - t7 * x1 * t9 * t25 - t7 * t29
      t34 = 0.288D3 * t4 * t6 * t31
      t35 = t5 ** 2
      t36 = t35 * t5
      t37 = t3 * t36
      t38 = x1 ** 2
      t41 = t31 * t11
      t42 = t9 * t25
      t44 = t37 * t38 * t28 * t41 * t42
      t46 = t35 ** 2
      t47 = t3 * t46
      t48 = t47 * x1
      t49 = t28 ** 2
      t50 = t49 * t28
      t51 = t11 ** 2
      t53 = t31 * x3
      t55 = t48 * t50 * t51 * t53
      t57 = t37 * x1
      t60 = t57 * t49 * t11 * t53
      t63 = x3 ** 2
      t66 = t48 * t50 * t11 * t31 * t63
      t68 = t38 ** 2
      t69 = t47 * t68
      t70 = t8 ** 2
      t72 = 0.1D1 / t70 / t8
      t75 = t8 * t18 + x2 * t11 - t24
      t76 = t75 ** 2
      t78 = t31 * t25
      t80 = t69 * t72 * t76 * t78
      t82 = t4 * t36
      t83 = x1 * t38
      t85 = 0.1D1 / t70
      t86 = t25 ** 2
      t87 = t85 * t86
      t90 = 0.288D3 * t82 * t83 * t31 * t87
      t91 = x1 * t31
      t92 = t49 * t63
      t94 = t82 * t91 * t92
      t96 = t4 * t35
      t97 = t28 * x3
      t100 = 0.288D3 * t96 * t91 * t97
      t101 = t2 * t1
      t102 = t101 * z
      t104 = t83 * t85
      t110 = t68 * t72
      t115 = t3 * t35
      t116 = t115 * t38
      t117 = t9 * t75
      t118 = z ** 2
      t122 = 0.1728D4 * t116 * t117 * t31 * t118
      t126 = 0.1728D4 * t116 * t117 * t31 * z
      t127 = -t34 + 0.576D3 * t44 - 0.144D3 * t55 + 0.14832D5 * t60 - 0.
     #144D3 * t66 + 0.1440D4 * t80 - t90 - 0.144D3 * t94 + t100 + 0.1152
     #D4 * t102 * t36 * t104 * t75 * t25 + 0.576D3 * t102 * t46 * t110 *
     # t75 * t86 - t122 + t126
      t128 = t36 * t38
      t129 = t4 * t128
      t134 = 0.288D3 * t129 * t31 * x2 * t28 * t9
      t135 = t46 * t5
      t136 = t3 * t135
      t137 = t136 * t110
      t138 = t75 * t31
      t140 = t25 * x2 * t28
      t142 = t137 * t138 * t140
      t144 = t38 * t9
      t149 = t47 * t104
      t150 = t97 * t25
      t152 = t149 * t138 * t150
      t154 = t46 * t35
      t155 = t3 * t154
      t163 = t3 * t46 * t36
      t166 = x2 ** 2
      t176 = t136 * t83 * t49 * t41 * t85 * t25 * x2
      t179 = t47 * t38 * t49
      t181 = x3 * t9 * t25
      t183 = t179 * t41 * t181
      t185 = t136 * t104
      t186 = t76 * t31
      t194 = t155 * t83 * t50 * t41 * t166 * t85
      t198 = t37 * t144 * t138 * t97
      t200 = t155 * t110
      t201 = t166 * t49
      t205 = x2 * t28
      t207 = t149 * t138 * t205
      t209 = t46 ** 2
      t212 = t70 ** 2
      t214 = t68 * x1 / t212
      t221 = t134 + 0.144D3 * t142 - 0.288D3 * t47 * t144 * t138 * t92 +
     # 0.144D3 * t152 + 0.108D3 * t155 * t104 * t138 * t50 * t63 * x2 + 
     #0.108D3 * t163 * t110 * t138 * t50 * x3 * t166 + 0.14832D5 * t176 
     #- 0.14976D5 * t183 + 0.72D2 * t185 * t186 * t92 - 0.144D3 * t194 +
     # 0.144D3 * t198 - 0.288D3 * t200 * t138 * t201 + 0.144D3 * t207 + 
     #0.36D2 * t3 * t209 * t214 * t138 * t166 * x2 * t50
      t223 = t75 * t76
      t227 = 0.1440D4 * t69 * t72 * t223 * t31
      t230 = t57 * t49 * t51 * t31
      t232 = t37 * t83
      t235 = t232 * t85 * t76 * t31
      t238 = t144 * t75
      t245 = 0.576D3 * t101 * t118 * z * t35 * t238
      t247 = t49 * x3 * x2
      t251 = t46 * t83
      t252 = t102 * t251
      t253 = t85 * t75
      t259 = t72 * t75
      t267 = t46 * t38
      t272 = t135 * t83
      t281 = t4 * t272 * t31 * t166 * t49 * t85
      t283 = t51 * t31
      t285 = t179 * t283 * t42
      t287 = t227 + 0.14832D5 * t230 - 0.1440D4 * t235 + 0.576D3 * t102 
     #* t35 * t238 + t245 - 0.576D3 * t185 * t138 * t247 - 0.1152D4 * t2
     #52 * t253 * t205 + 0.576D3 * t102 * t154 * t68 * t259 * t201 - 0.1
     #152D4 * t102 * t128 * t117 * t97 + 0.576D3 * t102 * t267 * t117 * 
     #t92 + 0.1152D4 * t102 * t272 * t253 * t247 - 0.144D3 * t281 - 0.14
     #832D5 * t285
      t289 = t136 * t38 * t50
      t290 = x2 * t9
      t292 = t289 * t283 * t290
      t298 = 0.288D3 * t4 * t251 * t31 * t85 * t140
      t302 = 0.288D3 * t129 * t31 * t28 * t181
      t305 = t15 * t9
      t307 = t4 * t267 * t31 * t49 * t305
      t310 = t179 * t41 * t290
      t320 = t289 * t41 * t305
      t328 = t223 * t31
      t331 = 0.72D2 * t137 * t328 * t97
      t332 = t155 * t214
      t335 = 0.72D2 * t332 * t328 * t205
      t348 = 0.1728D4 * t37 * t104 * t138 * z * t25
      t349 = 0.144D3 * t292 + t298 + t302 - 0.288D3 * t307 - 0.14976D5 *
     # t310 + 0.72D2 * t163 * t214 * t186 * t201 + 0.144D3 * t200 * t186
     # * t247 + 0.288D3 * t320 + 0.36D2 * t136 * t144 * t138 * t50 * t63
     # * x3 + t331 + t335 - 0.1152D4 * t102 * t135 * t68 * t259 * t140 -
     # 0.1152D4 * t252 * t253 * t150 - t348
      t364 = 0.72D2 * t332 * t138 * t86 * x2 * t28 + t34 - 0.864D3 * t44
     # + 0.2160D4 * t55 - 0.15696D5 * t60 - 0.936D3 * t66 - 0.1728D4 * t
     #80 + t90 + 0.288D3 * t94 + t100 + t122
      t386 = -t126 + 0.576D3 * t232 * t253 * t78 - 0.576D3 * t96 * t38 *
     # t31 * t42 - 0.288D3 * t69 * t259 * t31 * t86 + 0.72D2 * t137 * t1
     #38 * t86 * t28 * x3 + t134 - 0.72D2 * t142 - 0.72D2 * t152 - 0.156
     #96D5 * t176 + 0.15552D5 * t183 - 0.936D3 * t194
      t410 = -0.72D2 * t198 - 0.72D2 * t207 - t227 - 0.15552D5 * t230 + 
     #0.1728D4 * t235 - t245 - 0.288D3 * t116 * t117 * t31 + 0.3096D4 * 
     #t48 * t50 * t51 * t11 * t31 + 0.432D3 * t115 * x1 * t29 * t31 + 0.
     #432D3 * t47 * t83 * t28 * t41 * t87 + 0.288D3 * t281
      t422 = 0.15552D5 * t285 - 0.2160D4 * t292 + t298 + t302 + 0.576D3 
     #* t307 + 0.15552D5 * t310 + 0.1872D4 * t320 - t331 - t335 + t348 +
     # 0.72D2 * t332 * t186 * t140 + 0.72D2 * t137 * t186 * t150
      rrgq2qgh62J4 = -(wd * (t127 + t221 + t287 + t349) + wd * (t364 + t
     #386 + t410 + t422)) / t1 / t31 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh62J5
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
      t6 = t5 * x1
      t7 = s * t5
      t8 = z + t6
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
      t31 = s - t7 * x1 * t9 * t25 - t7 * t29
      t34 = 0.288D3 * t4 * t6 * t31
      t35 = t5 ** 2
      t36 = t3 * t35
      t37 = x1 ** 2
      t38 = t36 * t37
      t41 = t18 * t8 + x2 * t11 - t24
      t42 = t9 * t41
      t46 = 0.1728D4 * t38 * t42 * t31 * z
      t47 = z ** 2
      t51 = 0.1728D4 * t38 * t42 * t31 * t47
      t52 = t2 * t1
      t53 = t52 * z
      t54 = t35 ** 2
      t56 = t37 ** 2
      t57 = t8 ** 2
      t59 = 0.1D1 / t57 / t8
      t60 = t56 * t59
      t61 = t25 ** 2
      t66 = t35 * t5
      t68 = t37 * x1
      t69 = 0.1D1 / t57
      t70 = t68 * t69
      t75 = t4 * t35
      t76 = x1 * t31
      t77 = t28 * x3
      t80 = 0.288D3 * t75 * t76 * t77
      t81 = t4 * t66
      t82 = t28 ** 2
      t83 = x3 ** 2
      t84 = t82 * t83
      t86 = t81 * t76 * t84
      t89 = t69 * t61
      t92 = 0.288D3 * t81 * t68 * t31 * t89
      t93 = t3 * t54
      t94 = t93 * t56
      t95 = t41 ** 2
      t97 = t31 * t25
      t99 = t94 * t59 * t95 * t97
      t101 = t93 * x1
      t102 = t82 * t28
      t106 = t101 * t102 * t11 * t31 * t83
      t108 = t3 * t66
      t109 = t108 * x1
      t111 = t31 * x3
      t113 = t109 * t82 * t11 * t111
      t115 = t11 ** 2
      t118 = t101 * t102 * t115 * t111
      t122 = t11 * t31
      t123 = t9 * t25
      t125 = t108 * t37 * t28 * t122 * t123
      t127 = -t34 + t46 - t51 + 0.576D3 * t53 * t54 * t60 * t41 * t61 + 
     #0.1152D4 * t53 * t66 * t70 * t41 * t25 + t80 - 0.144D3 * t86 - t92
     # + 0.1440D4 * t99 - 0.144D3 * t106 + 0.14832D5 * t113 - 0.144D3 * 
     #t118 + 0.576D3 * t125
      t129 = t93 * t37 * t82
      t130 = t115 * t31
      t132 = t129 * t130 * t123
      t134 = t54 * t68
      t135 = t53 * t134
      t136 = t69 * t41
      t137 = t77 * t25
      t141 = t95 * t41
      t145 = 0.1440D4 * t94 * t59 * t141 * t31
      t146 = t93 * t70
      t147 = t41 * t31
      t149 = t146 * t147 * t137
      t151 = t54 * t35
      t152 = t3 * t151
      t159 = t54 * t5
      t160 = t3 * t159
      t162 = t160 * t37 * t102
      t163 = x2 * t9
      t165 = t162 * t130 * t163
      t169 = t109 * t82 * t115 * t31
      t171 = t108 * t68
      t174 = t171 * t69 * t95 * t31
      t177 = t3 * t54 * t66
      t180 = x2 ** 2
      t186 = t57 ** 2
      t188 = t56 * x1 / t186
      t189 = t152 * t188
      t190 = t141 * t31
      t191 = x2 * t28
      t194 = 0.72D2 * t189 * t190 * t191
      t196 = t37 * t9
      t197 = t196 * t41
      t200 = t152 * t60
      t201 = t95 * t31
      t203 = t82 * x3 * x2
      t211 = 0.576D3 * t52 * t47 * z * t35 * t197
      t212 = t160 * t70
      t216 = -0.14832D5 * t132 - 0.1152D4 * t135 * t136 * t137 + t145 + 
     #0.144D3 * t149 + 0.108D3 * t152 * t70 * t147 * t102 * t83 * x2 + 0
     #.144D3 * t165 + 0.14832D5 * t169 - 0.1440D4 * t174 + 0.108D3 * t17
     #7 * t60 * t147 * t102 * x3 * t180 + t194 + 0.576D3 * t53 * t35 * t
     #197 + 0.144D3 * t200 * t201 * t203 + t211 - 0.576D3 * t212 * t147 
     #* t203
      t221 = t66 * t37
      t222 = t4 * t221
      t227 = 0.288D3 * t222 * t31 * x2 * t28 * t9
      t231 = t25 * x2 * t28
      t234 = 0.288D3 * t4 * t134 * t31 * t69 * t231
      t237 = x3 * t9 * t25
      t240 = 0.288D3 * t222 * t31 * t28 * t237
      t241 = t54 * t37
      t244 = t15 * t9
      t246 = t4 * t241 * t31 * t82 * t244
      t248 = t54 ** 2
      t257 = t180 * t82
      t262 = t162 * t122 * t244
      t270 = t160 * t60
      t273 = 0.72D2 * t270 * t190 * t77
      t282 = t270 * t147 * t231
      t284 = -0.1152D4 * t135 * t136 * t191 + t227 + t234 + t240 - 0.288
     #D3 * t246 + 0.36D2 * t3 * t248 * t188 * t147 * t180 * x2 * t102 + 
     #0.72D2 * t177 * t188 * t201 * t257 + 0.288D3 * t262 + 0.36D2 * t16
     #0 * t196 * t147 * t102 * t83 * x3 + t273 + 0.72D2 * t212 * t201 * 
     #t84 - 0.288D3 * t93 * t196 * t147 * t84 + 0.144D3 * t282
      t289 = t146 * t147 * t191
      t292 = t129 * t122 * t163
      t298 = t152 * t68 * t102 * t122 * t180 * t69
      t305 = t160 * t68 * t82 * t122 * t69 * t25 * x2
      t309 = t108 * t196 * t147 * t77
      t317 = t59 * t41
      t325 = t159 * t68
      t334 = t4 * t325 * t31 * t180 * t82 * t69
      t337 = t129 * t122 * t237
      t348 = 0.1728D4 * t108 * t70 * t147 * z * t25
      t349 = -0.288D3 * t200 * t147 * t257 + 0.144D3 * t289 - 0.14976D5 
     #* t292 - 0.144D3 * t298 + 0.14832D5 * t305 + 0.144D3 * t309 - 0.11
     #52D4 * t53 * t221 * t42 * t77 + 0.576D3 * t53 * t151 * t56 * t317 
     #* t257 + 0.576D3 * t53 * t241 * t42 * t84 + 0.1152D4 * t53 * t325 
     #* t136 * t203 - 0.144D3 * t334 - 0.14976D5 * t337 - 0.1152D4 * t53
     # * t159 * t56 * t317 * t231 - t348
      t365 = t34 - 0.288D3 * t94 * t317 * t31 * t61 - 0.576D3 * t75 * t3
     #7 * t31 * t123 - t46 + t51 + t80 + 0.288D3 * t86 + t92 - 0.1728D4 
     #* t99 - 0.936D3 * t106 - 0.15696D5 * t113
      t373 = 0.2160D4 * t118 - 0.864D3 * t125 + 0.15552D5 * t132 - t145 
     #- 0.72D2 * t149 - 0.2160D4 * t165 - 0.15552D5 * t169 + 0.1728D4 * 
     #t174 - t194 - t211 + t227
      t391 = t234 + t240 + 0.576D3 * t246 + 0.1872D4 * t262 - t273 + 0.7
     #2D2 * t270 * t201 * t137 - 0.72D2 * t282 + 0.72D2 * t189 * t201 * 
     #t231 - 0.72D2 * t289 + 0.72D2 * t270 * t147 * t61 * t28 * x3 + 0.1
     #5552D5 * t292
      t422 = 0.432D3 * t93 * t68 * t28 * t122 * t89 - 0.936D3 * t298 - 0
     #.288D3 * t38 * t42 * t31 - 0.15696D5 * t305 + 0.432D3 * t36 * x1 *
     # t29 * t31 + 0.3096D4 * t101 * t102 * t115 * t11 * t31 - 0.72D2 * 
     #t309 + 0.288D3 * t334 + 0.576D3 * t171 * t136 * t97 + 0.72D2 * t18
     #9 * t147 * t61 * x2 * t28 + 0.15552D5 * t337 + t348
      rrgq2qgh62J5 = -(wd * (t127 + t216 + t284 + t349) + wd * (t365 + t
     #373 + t391 + t422)) / t1 / t31 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh62J6
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
      t8 = t3 * t6 * t5
      t9 = x1 ** 2
      t10 = t9 ** 2
      t12 = x1 * t4
      t13 = z + t12
      t14 = t13 ** 2
      t15 = t14 ** 2
      t18 = t8 * t10 * x1 / t15
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t22 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t20 * t13 * x2 * t22)
      t31 = 0.2D1 * t25 * t29
      t32 = t20 * t13 + x2 * t22 - t31
      t33 = s * t4
      t34 = 0.1D1 / t13
      t38 = x2 * x3
      t39 = t22 * t19 * t13 + t38 + t31
      t42 = 0.1D1 - x1
      t43 = t42 * t22
      t45 = s - t33 * x1 * t34 * t39 - t33 * t43
      t46 = t32 * t45
      t47 = t39 ** 2
      t53 = t3 * t6
      t54 = t9 * x1
      t57 = t22 * t45
      t58 = 0.1D1 / t14
      t59 = t58 * t47
      t63 = t3 * z
      t66 = t42 ** 2
      t68 = t38 * t34
      t72 = t5 * t4
      t73 = t3 * t72
      t74 = t54 * t58
      t81 = t53 * t9 * t66
      t83 = x3 * t34 * t39
      t87 = t6 * t4
      t88 = t3 * t87
      t89 = t66 * t42
      t91 = t88 * t9 * t89
      t96 = 0.1D1 / t14 / t13
      t98 = t88 * t10 * t96
      t100 = t39 * x2 * t42
      t104 = t9 * t34
      t106 = t42 * x3
      t110 = t53 * t74
      t111 = t106 * t39
      t115 = t3 * t5
      t116 = t115 * t9
      t117 = t34 * t32
      t121 = t53 * x1
      t122 = t22 ** 2
      t128 = 0.72D2 * t18 * t46 * t47 * x2 * t42 + 0.432D3 * t53 * t54 *
     # t42 * t57 * t59 + 0.576D3 * t63 * t6 * t9 * t45 * t66 * t68 + 0.1
     #728D4 * t73 * t74 * t46 * z * t39 + 0.15552D5 * t81 * t57 * t83 + 
     #0.1872D4 * t91 * t57 * t68 - 0.72D2 * t98 * t46 * t100 - 0.72D2 * 
     #t73 * t104 * t46 * t106 - 0.72D2 * t110 * t46 * t111 - 0.288D3 * t
     #116 * t117 * t45 + 0.3096D4 * t121 * t89 * t122 * t22 * t45
      t133 = t53 * t10
      t134 = t32 ** 2
      t135 = t134 * t32
      t140 = t73 * x1
      t145 = t73 * t54
      t151 = z ** 2
      t162 = t63 * t72 * t9
      t168 = t45 * x3
      t173 = t45 * t39
      t178 = x3 ** 2
      t187 = 0.432D3 * t115 * x1 * t43 * t45 - 0.1440D4 * t133 * t96 * t
     #135 * t45 - 0.15552D5 * t140 * t66 * t122 * t45 + 0.1728D4 * t145 
     #* t58 * t134 * t45 - 0.576D3 * t2 * t1 * t151 * z * t5 * t104 * t3
     #2 + 0.288D3 * t63 * t12 * t45 + 0.288D3 * t162 * t45 * t42 * t83 +
     # 0.2160D4 * t121 * t89 * t122 * t168 - 0.1728D4 * t133 * t96 * t13
     #4 * t173 - 0.936D3 * t121 * t89 * t22 * t45 * t178 - 0.15696D5 * t
     #140 * t66 * t22 * t168
      t189 = t63 * t72
      t194 = x1 * t45
      t207 = t63 * t5
      t217 = t34 * t39
      t225 = t135 * t45
      t229 = x2 * t42
      t240 = 0.288D3 * t189 * t54 * t45 * t59 + 0.288D3 * t189 * t194 * 
     #t66 * t178 - 0.1728D4 * t116 * t117 * t45 * z + 0.1728D4 * t116 * 
     #t117 * t45 * t151 + 0.288D3 * t207 * t194 * t106 - 0.288D3 * t133 
     #* t96 * t32 * t45 * t47 - 0.576D3 * t207 * t9 * t45 * t217 + 0.576
     #D3 * t145 * t58 * t32 * t173 - 0.72D2 * t98 * t225 * t106 - 0.72D2
     # * t110 * t46 * t229 - 0.15696D5 * t88 * t54 * t66 * t57 * t58 * t
     #39 * x2
      t246 = t122 * t45
      t250 = x2 * t34
      t259 = x2 ** 2
      t285 = t134 * t45
      t297 = -0.864D3 * t73 * t9 * t42 * t57 * t217 + 0.15552D5 * t81 * 
     #t246 * t217 - 0.2160D4 * t91 * t246 * t250 + 0.15552D5 * t81 * t57
     # * t250 - 0.936D3 * t8 * t54 * t89 * t57 * t259 * t58 - 0.72D2 * t
     #18 * t225 * t229 + 0.288D3 * t63 * t87 * t54 * t45 * t259 * t66 * 
     #t58 + 0.288D3 * t162 * t45 * x2 * t42 * t34 + 0.288D3 * t63 * t6 *
     # t54 * t45 * t58 * t100 + 0.72D2 * t18 * t285 * t100 + 0.72D2 * t9
     #8 * t285 * t111 + 0.72D2 * t98 * t46 * t47 * t42 * x3
      rrgq2qgh62J6 = -wd * (t128 + t187 + t240 + t297) / t1 / t45 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh63J1
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
      t10 = 0.1D1 - x2
      t13 = z + x1 * t10 * t4
      t14 = t9 * t13
      t16 = z + x1 * t4
      t17 = t16 ** 2
      t19 = 0.1D1 / t17 / t16
      t22 = x1 ** 2
      t23 = x3 * t10
      t25 = 0.1D1 - x3
      t26 = x2 * t25
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t23 * t16 * x2 * t25)
      t34 = 0.2D1 * t28 * t32
      t35 = t23 * t16 + t26 - t34
      t36 = t22 * t35
      t37 = s * t4
      t38 = 0.1D1 / t16
      t44 = s - t37 * x1 * t38 * t35 - t37 * t8 * x3
      t47 = x2 * x3
      t48 = t25 * t10 * t16 + t47 + t34
      t49 = t44 * t48
      t54 = t6 * t4
      t55 = t3 * t54
      t56 = t9 * t8
      t57 = t56 * t13
      t59 = t55 * t57 * t19
      t60 = t25 * t44
      t62 = t22 * t48 * x2
      t66 = t3 * z
      t67 = t5 * t4
      t68 = t67 * t56
      t70 = t13 * t38
      t71 = x3 ** 2
      t76 = t54 * t56
      t78 = t13 * t19
      t79 = x2 ** 2
      t81 = t44 * t79 * t22
      t88 = t48 ** 2
      t89 = t44 * t22 * t88
      t95 = 0.1D1 / t17
      t96 = t13 * t95
      t98 = t44 * x1 * t48
      t102 = t17 ** 2
      t103 = 0.1D1 / t102
      t106 = t22 * x1
      t112 = t2 * t1
      t113 = t112 * z
      t114 = t6 * t56
      t115 = t114 * t13
      t117 = t95 * t25
      t119 = x1 * t48 * x3
      t125 = t19 * t25
      t129 = t67 * t9
      t137 = t3 * t5 * t14
      t138 = t38 * t25
      t139 = t44 * z
      t143 = t6 * t9
      t151 = t3 * t6 * t67
      t152 = t9 ** 2
      t154 = t152 * t8 * t13
      t156 = t25 ** 2
      t157 = t19 * t156
      t161 = -0.14976D5 * t7 * t14 * t19 * t36 * t49 * x3 - 0.576D3 * t5
     #9 * t60 * t62 - 0.288D3 * t66 * t68 * t70 * t44 * t71 - 0.144D3 * 
     #t66 * t76 * t78 * t81 - 0.144D3 * t66 * t67 * t8 * t78 * t89 + 0.2
     #88D3 * t66 * t5 * t8 * t96 * t98 + 0.288D3 * t55 * t14 * t103 * t1
     #06 * t35 * t49 * x2 - 0.1152D4 * t113 * t115 * t117 * t119 + 0.115
     #2D4 * t113 * t76 * t13 * t125 * t62 - 0.1152D4 * t113 * t129 * t96
     # * t25 * x1 * t48 + 0.1728D4 * t137 * t138 * t139 + 0.576D3 * t113
     # * t143 * t78 * t25 * t22 * t88 + 0.72D2 * t151 * t154 * t157 * t8
     #1
      t162 = t6 * t5
      t163 = t3 * t162
      t165 = t156 * t25
      t166 = t95 * t165
      t168 = t44 * x2 * x1
      t172 = t152 * t13
      t177 = t7 * t14
      t181 = t3 * t67
      t182 = t181 * t14
      t193 = t95 * t44
      t194 = t47 * x1
      t202 = t8 * t13
      t203 = t181 * t202
      t204 = t19 * t22
      t205 = t35 * t44
      t210 = t35 ** 2
      t211 = t210 * t44
      t216 = t6 ** 2
      t219 = t103 * t25
      t226 = t7 * t202
      t227 = t103 * t106
      t234 = t156 * t44
      t249 = 0.72D2 * t163 * t154 * t166 * t168 + 0.72D2 * t55 * t172 * 
     #t166 * t98 - 0.288D3 * t177 * t125 * t89 + 0.144D3 * t182 * t117 *
     # t98 - 0.288D3 * t66 * t143 * t13 * t19 * t44 * t62 + 0.288D3 * t6
     #6 * t115 * t193 * t194 + 0.288D3 * t66 * t129 * t96 * t168 + 0.148
     #32D5 * t203 * t204 * t205 * t48 - 0.14832D5 * t177 * t204 * t211 *
     # x3 + 0.36D2 * t3 * t216 * t154 * t219 * t44 * t79 * x2 * t106 - 0
     #.144D3 * t226 * t227 * t211 * t48 + 0.144D3 * t163 * t172 * t19 * 
     #t234 * t62 + 0.108D3 * t163 * t57 * t103 * t60 * t106 * t88 * x2 +
     # 0.72D2 * t55 * t57 * t157 * t89
      t265 = z ** 2
      t289 = t44 * x3
      t296 = t14 * t138
      t312 = t55 * t14
      t317 = 0.576D3 * t182 * t95 * x1 * t205 * x3 + 0.14832D5 * t203 * 
     #t204 * t211 - 0.1152D4 * t113 * t54 * t152 * t13 * t117 * t194 - 0
     #.1728D4 * t137 * t138 * t44 * t265 - 0.1728D4 * t181 * t57 * t138 
     #* t139 * x3 + 0.288D3 * t66 * t129 * t13 * t193 * t119 + 0.108D3 *
     # t151 * t172 * t103 * t60 * t106 * t48 * t79 + 0.1440D4 * t7 * t17
     #2 * t38 * t156 * t289 + 0.576D3 * t112 * t265 * z * t5 * t296 + 0.
     #14832D5 * t59 * t36 * t289 * x2 + 0.144D3 * t7 * t57 * t95 * t60 *
     # t119 - 0.144D3 * t226 * t227 * t205 * t88 + 0.144D3 * t312 * t227
     # * t211 * x2
      t386 = -0.288D3 * t163 * t172 * t125 * t81 + 0.144D3 * t7 * t57 * 
     #t117 * t168 + 0.36D2 * t312 * t219 * t44 * t106 * t88 * t48 + 0.11
     #52D4 * t113 * t68 * t70 * t25 * x3 + 0.576D3 * t113 * t6 * t152 * 
     #t70 * t25 * t71 - 0.1152D4 * t113 * t114 * t96 * t26 * x1 + 0.576D
     #3 * t113 * t162 * t152 * t78 * t25 * t79 * t22 + 0.1440D4 * t7 * t
     #152 * t70 * t165 * t44 - 0.1440D4 * t181 * t56 * t70 * t234 - 0.28
     #8D3 * t66 * t4 * t202 * t38 * t44 + 0.576D3 * t113 * t5 * t296 - 0
     #.14976D5 * t177 * t204 * t205 * x2 - 0.144D3 * t163 * t57 * t227 *
     # t205 * t79 + 0.144D3 * t55 * t172 * t95 * t60 * t194
      rrgq2qgh63J1 = -wd * (t161 + t249 + t317 + t386) / t1 / t44 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh63J2
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
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = t10 * t14
      t16 = t7 * t15
      t18 = z + x1 * t4
      t19 = 0.1D1 / t18
      t20 = 0.1D1 - x3
      t21 = t20 ** 2
      t23 = s * t4
      t25 = x3 * t11
      t27 = x2 * t20
      t29 = cos(x4 * 0.3141592653589793D1)
      t33 = Sqrt(t25 * t18 * x2 * t20)
      t35 = 0.2D1 * t29 * t33
      t36 = t25 * t18 + t27 - t35
      t41 = s - t23 * x1 * t19 * t36 - t23 * t8 * x3
      t42 = t41 * x3
      t44 = t16 * t19 * t21 * t42
      t46 = t5 * t4
      t47 = t3 * t46
      t48 = t8 * t14
      t49 = t47 * t48
      t50 = t18 ** 2
      t52 = 0.1D1 / t50 / t18
      t53 = x1 ** 2
      t54 = t52 * t53
      t55 = t36 ** 2
      t56 = t55 * t41
      t58 = t49 * t54 * t56
      t60 = t2 * t1
      t61 = t60 * z
      t62 = t6 * t4
      t66 = 0.1D1 / t50
      t67 = t66 * t20
      t68 = x2 * x3
      t69 = t68 * x1
      t73 = t3 * t62
      t74 = t9 * t8
      t75 = t74 * t14
      t77 = t73 * t75 * t52
      t78 = t20 * t41
      t81 = t20 * t11 * t18 + t68 + t35
      t83 = t53 * t81 * x2
      t87 = t9 * t14
      t88 = t73 * t87
      t89 = t50 ** 2
      t90 = 0.1D1 / t89
      t91 = t90 * t20
      t92 = t53 * x1
      t94 = t81 ** 2
      t100 = t6 * t5
      t101 = t3 * t100
      t104 = t21 * t41
      t115 = t47 * t87
      t116 = t66 * x1
      t117 = t36 * t41
      t120 = t115 * t116 * t117 * x3
      t122 = t46 * t9
      t124 = t14 * t66
      t130 = t6 * t9
      t132 = t14 * t52
      t139 = t3 * t6 * t46
      t143 = x2 ** 2
      t148 = t3 * z
      t152 = t41 * x1 * t81
      t155 = 0.288D3 * t148 * t5 * t8 * t124 * t152
      t157 = t10 * t8 * t14
      t159 = t21 * t20
      t160 = t66 * t159
      t162 = t41 * x2 * x1
      t165 = 0.72D2 * t101 * t157 * t160 * t162
      t166 = 0.1440D4 * t44 + 0.14832D5 * t58 - 0.1152D4 * t61 * t62 * t
     #10 * t14 * t67 * t69 - 0.576D3 * t77 * t78 * t83 + 0.36D2 * t88 * 
     #t91 * t41 * t92 * t94 * t81 + 0.144D3 * t101 * t15 * t52 * t104 * 
     #t83 + 0.108D3 * t101 * t75 * t90 * t78 * t92 * t94 * x2 + 0.576D3 
     #* t120 - 0.1152D4 * t61 * t122 * t124 * t20 * x1 * t81 + 0.576D3 *
     # t61 * t130 * t132 * t20 * t53 * t94 + 0.108D3 * t139 * t15 * t90 
     #* t78 * t92 * t81 * t143 + t155 + t165
      t174 = t46 * t74
      t176 = t14 * t19
      t181 = t90 * t92
      t184 = t88 * t181 * t56 * x2
      t188 = x3 ** 2
      t193 = t6 * t74
      t201 = t66 * t41
      t203 = x1 * t81 * x3
      t206 = 0.288D3 * t148 * t122 * t14 * t201 * t203
      t207 = t6 ** 2
      t217 = t52 * t21
      t219 = t41 * t143 * t53
      t225 = t41 * t53 * t94
      t229 = t7 * t48
      t232 = t229 * t181 * t56 * t81
      t235 = t41 * t188
      t238 = 0.288D3 * t148 * t174 * t176 * t235
      t239 = t7 * t87
      t242 = t239 * t54 * t56 * x3
      t247 = 0.72D2 * t73 * t15 * t160 * t152
      t248 = t52 * t20
      t252 = 0.576D3 * t61 * t100 * t10 * t132 * t20 * t143 * t53 + 0.11
     #52D4 * t61 * t174 * t176 * t20 * x3 + 0.144D3 * t184 + 0.576D3 * t
     #61 * t6 * t10 * t176 * t20 * t188 - 0.1152D4 * t61 * t193 * t124 *
     # t27 * x1 + t206 + 0.36D2 * t3 * t207 * t157 * t91 * t41 * t143 * 
     #x2 * t92 + 0.72D2 * t139 * t157 * t217 * t219 + 0.72D2 * t73 * t75
     # * t217 * t225 - 0.144D3 * t232 - t238 - 0.14832D5 * t242 + t247 -
     # 0.288D3 * t239 * t248 * t225
      t254 = z ** 2
      t258 = t19 * t20
      t259 = t87 * t258
      t261 = 0.576D3 * t60 * t254 * z * t5 * t259
      t269 = 0.288D3 * t148 * t4 * t48 * t19 * t41
      t272 = t47 * t74 * t176 * t104
      t278 = 0.1440D4 * t7 * t10 * t176 * t159 * t41
      t279 = t62 * t74
      t282 = t148 * t279 * t132 * t219
      t284 = t3 * t5
      t285 = t284 * t87
      t286 = t41 * z
      t289 = 0.1728D4 * t285 * t258 * t286
      t293 = 0.1728D4 * t285 * t258 * t41 * t254
      t294 = t47 * t75
      t298 = 0.1728D4 * t294 * t258 * t286 * x3
      t303 = t148 * t130 * t14 * t52 * t41 * t83
      t308 = t41 * t81
      t311 = t73 * t87 * t90 * t92 * t36 * t308 * x2
      t313 = t193 * t14
      t323 = t261 + 0.576D3 * t61 * t5 * t259 - t269 - 0.1440D4 * t272 +
     # t278 - 0.144D3 * t282 + t289 - t293 - t298 - 0.288D3 * t303 + 0.2
     #88D3 * t311 - 0.1152D4 * t61 * t313 * t67 * t203 + 0.1152D4 * t61 
     #* t279 * t14 * t248 * t83
      t326 = t239 * t54 * t117 * x2
      t331 = t101 * t75 * t181 * t117 * t143
      t334 = t73 * t15 * t66
      t336 = t334 * t78 * t69
      t342 = t7 * t75
      t344 = t342 * t67 * t162
      t349 = t148 * t46 * t8 * t132 * t225
      t353 = t229 * t181 * t117 * t94
      t357 = t53 * t36
      t360 = t7 * t87 * t52 * t357 * t308 * x3
      t365 = 0.288D3 * t148 * t313 * t201 * t69
      t369 = 0.288D3 * t148 * t122 * t124 * t162
      t372 = t49 * t54 * t117 * t81
      t376 = t77 * t357 * t42 * x2
      t379 = t115 * t67 * t152
      t384 = t7 * t75 * t66 * t78 * t203
      t386 = -0.14976D5 * t326 - 0.144D3 * t331 + 0.144D3 * t336 - 0.288
     #D3 * t101 * t15 * t248 * t219 + 0.144D3 * t344 - 0.144D3 * t349 - 
     #0.144D3 * t353 - 0.14976D5 * t360 + t365 + t369 + 0.14832D5 * t372
     # + 0.14832D5 * t376 + 0.144D3 * t379 + 0.144D3 * t384
      t400 = t101 * t157 * t66
      t413 = -0.1728D4 * t44 - 0.15552D5 * t58 - 0.864D3 * t120 - 0.288D
     #3 * t16 * t258 * t235 + 0.576D3 * t294 * t258 * t42 + 0.72D2 * t40
     #0 * t78 * t188 * x2 * x1 + 0.72D2 * t400 * t104 * t69 + t155 - t16
     #5 - 0.2160D4 * t184 + 0.72D2 * t334 * t104 * t203
      t422 = t206 + 0.2160D4 * t232 + t238 + 0.15552D5 * t242 - t247 - t
     #261 + t269 + 0.1728D4 * t272 - t278 - 0.288D3 * t284 * t9 * t176 *
     # t78 + 0.288D3 * t282
      t439 = -t289 + t293 + t298 + 0.576D3 * t303 + 0.1872D4 * t311 + 0.
     #15552D5 * t326 - 0.936D3 * t331 - 0.72D2 * t336 + 0.432D3 * t342 *
     # t116 * t117 * t188 - 0.72D2 * t344 + 0.3096D4 * t229 * t181 * t55
     # * t36 * t41
      t461 = 0.288D3 * t349 - 0.936D3 * t353 + 0.15552D5 * t360 + t365 +
     # t369 - 0.15696D5 * t372 - 0.576D3 * t148 * t5 * t9 * t176 * t42 +
     # 0.432D3 * t284 * t48 * t116 * t117 + 0.72D2 * t334 * t78 * t188 *
     # x1 * t81 - 0.15696D5 * t376 - 0.72D2 * t379 - 0.72D2 * t384
      rrgq2qgh63J2 = -(wd * (t166 + t252 + t323 + t386) + wd * (t413 + t
     #422 + t439 + t461)) / t1 / t41 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh63J3
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
      t6 = t5 ** 2
      t7 = t6 * t5
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t7 * t9
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t5
      t18 = z + t5 * x1
      t19 = t18 ** 2
      t20 = 0.1D1 / t19
      t21 = s * t5
      t22 = 0.1D1 / t18
      t24 = x3 * t11
      t26 = 0.1D1 - x3
      t27 = x2 * t26
      t29 = cos(x4 * 0.3141592653589793D1)
      t33 = Sqrt(t24 * t18 * x2 * t26)
      t35 = 0.2D1 * t29 * t33
      t36 = t24 * t18 + t27 - t35
      t41 = s - t21 * x1 * t22 * t36 - t21 * t8 * x3
      t42 = t20 * t41
      t45 = x2 * x3
      t46 = t26 * t11 * t18 + t45 + t35
      t48 = x1 * t46 * x3
      t51 = 0.288D3 * t4 * t10 * t14 * t42 * t48
      t52 = t6 ** 2
      t53 = t52 * t5
      t54 = t9 * t8
      t55 = t53 * t54
      t58 = 0.1D1 / t19 / t18
      t59 = t14 * t58
      t60 = x2 ** 2
      t62 = x1 ** 2
      t63 = t41 * t60 * t62
      t65 = t4 * t55 * t59 * t63
      t67 = t2 * t1
      t68 = t67 * z
      t69 = t7 * t54
      t71 = t14 * t22
      t76 = t9 ** 2
      t79 = x3 ** 2
      t84 = t3 * t52
      t85 = t54 * t14
      t88 = t26 * t41
      t90 = t84 * t85 * t20 * t88 * t48
      t92 = t3 * t53
      t93 = t9 * t14
      t94 = t92 * t93
      t95 = t19 ** 2
      t96 = 0.1D1 / t95
      t97 = t96 * t26
      t98 = t62 * x1
      t100 = t46 ** 2
      t106 = t84 * t93
      t107 = t58 * t26
      t109 = t41 * t62 * t100
      t115 = t14 * t20
      t117 = t41 * x1 * t46
      t120 = 0.288D3 * t4 * t6 * t8 * t115 * t117
      t121 = t52 * t6
      t122 = t3 * t121
      t123 = t76 * t14
      t128 = t84 * t85
      t129 = t20 * t26
      t131 = t41 * x2 * x1
      t133 = t128 * t129 * t131
      t135 = t52 * t9
      t145 = t4 * t7 * t8 * t59 * t109
      t150 = t62 * t46 * x2
      t154 = t51 - 0.144D3 * t65 + 0.1152D4 * t68 * t69 * t71 * t26 * x3
     # + 0.576D3 * t68 * t52 * t76 * t71 * t26 * t79 + 0.144D3 * t90 + 0
     #.36D2 * t94 * t97 * t41 * t98 * t100 * t46 - 0.288D3 * t106 * t107
     # * t109 + t120 - 0.288D3 * t122 * t123 * t107 * t63 + 0.144D3 * t1
     #33 + 0.576D3 * t68 * t135 * t59 * t26 * t62 * t100 - 0.144D3 * t14
     #5 + 0.1152D4 * t68 * t55 * t14 * t107 * t150
      t162 = t26 ** 2
      t163 = t58 * t162
      t167 = t8 * t14
      t168 = t84 * t167
      t169 = t96 * t98
      t170 = t36 ** 2
      t171 = t170 * t41
      t174 = t168 * t169 * t171 * t46
      t177 = t3 * t52 * t7
      t188 = t41 * t46
      t191 = t92 * t93 * t96 * t98 * t36 * t188 * x2
      t193 = t3 * t7
      t194 = t193 * t93
      t195 = t20 * x1
      t196 = t36 * t41
      t199 = t194 * t195 * t196 * x3
      t201 = t52 * t54
      t215 = t41 * t79
      t218 = 0.288D3 * t4 * t69 * t71 * t215
      t219 = t201 * t14
      t221 = x1 * t45
      t224 = 0.288D3 * t4 * t219 * t42 * t221
      t228 = 0.288D3 * t4 * t10 * t115 * t131
      t230 = t194 * t129 * t117
      t232 = t193 * t167
      t233 = t58 * t62
      t235 = t232 * t233 * t171
      t243 = -0.1152D4 * t68 * t10 * t115 * t26 * x1 * t46 + 0.72D2 * t9
     #2 * t85 * t163 * t109 - 0.144D3 * t174 + 0.108D3 * t177 * t123 * t
     #96 * t88 * t98 * t46 * t60 + 0.288D3 * t191 + 0.576D3 * t199 - 0.1
     #152D4 * t68 * t201 * t115 * t27 * x1 + 0.576D3 * t68 * t121 * t76 
     #* t59 * t26 * t60 * t62 - t218 + t224 + t228 + 0.144D3 * t230 + 0.
     #14832D5 * t235 - 0.1152D4 * t68 * t53 * t76 * t14 * t129 * t221
      t246 = t92 * t85 * t58
      t247 = t62 * t36
      t248 = t41 * x3
      t251 = t246 * t247 * t248 * x2
      t257 = t84 * t93 * t58 * t247 * t188 * x3
      t259 = t52 ** 2
      t262 = t76 * t8 * t14
      t275 = t162 * t26
      t276 = t20 * t275
      t279 = 0.72D2 * t122 * t262 * t276 * t131
      t286 = t106 * t233 * t171 * x3
      t288 = t84 * t123
      t291 = t288 * t22 * t162 * t248
      t295 = t162 * t41
      t306 = z ** 2
      t310 = t22 * t26
      t311 = t93 * t310
      t313 = 0.576D3 * t67 * t306 * z * t6 * t311
      t321 = 0.288D3 * t4 * t5 * t167 * t22 * t41
      t322 = 0.14832D5 * t251 - 0.14976D5 * t257 + 0.36D2 * t3 * t259 * 
     #t262 * t97 * t41 * t60 * x2 * t98 + 0.72D2 * t177 * t262 * t163 * 
     #t63 + t279 - 0.1152D4 * t68 * t219 * t129 * t48 - 0.14832D5 * t286
     # + 0.1440D4 * t291 + 0.144D3 * t122 * t123 * t58 * t295 * t150 + 0
     #.108D3 * t122 * t85 * t96 * t88 * t98 * t100 * x2 + t313 + 0.576D3
     # * t68 * t6 * t311 - t321
      t325 = t193 * t54 * t71 * t295
      t331 = 0.1440D4 * t84 * t76 * t71 * t275 * t41
      t334 = t106 * t233 * t196 * x2
      t339 = t122 * t85 * t169 * t196 * t60
      t343 = t232 * t233 * t196 * t46
      t347 = t168 * t169 * t196 * t100
      t355 = 0.72D2 * t92 * t123 * t276 * t117
      t358 = t94 * t169 * t171 * x2
      t360 = t3 * t6
      t361 = t360 * t93
      t365 = 0.1728D4 * t361 * t310 * t41 * t306
      t366 = t193 * t85
      t367 = t41 * z
      t371 = 0.1728D4 * t366 * t310 * t367 * x3
      t376 = t4 * t135 * t14 * t58 * t41 * t150
      t379 = t92 * t123 * t20
      t381 = t379 * t88 * t221
      t385 = 0.1728D4 * t361 * t310 * t367
      t386 = -0.1440D4 * t325 + t331 - 0.14976D5 * t334 - 0.144D3 * t339
     # + 0.14832D5 * t343 - 0.144D3 * t347 - 0.576D3 * t246 * t88 * t150
     # + t355 + 0.144D3 * t358 - t365 - t371 - 0.288D3 * t376 + 0.144D3 
     #* t381 + t385
      t403 = t51 + 0.288D3 * t65 - 0.72D2 * t90 + t120 - 0.72D2 * t133 +
     # 0.288D3 * t145 + 0.2160D4 * t174 - 0.288D3 * t288 * t310 * t215 +
     # 0.576D3 * t366 * t310 * t248 + 0.1872D4 * t191 - 0.864D3 * t199
      t420 = 0.432D3 * t360 * t167 * t195 * t196 + 0.72D2 * t379 * t88 *
     # t79 * x1 * t46 + 0.72D2 * t379 * t295 * t48 + t218 + t224 + t228 
     #- 0.72D2 * t230 - 0.15552D5 * t235 - 0.15696D5 * t251 + 0.15552D5 
     #* t257 - t279
      t433 = 0.15552D5 * t286 - 0.1728D4 * t291 - t313 + t321 + 0.1728D4
     # * t325 - t331 - 0.288D3 * t360 * t9 * t71 * t88 + 0.15552D5 * t33
     #4 - 0.936D3 * t339 - 0.15696D5 * t343 - 0.936D3 * t347
      t437 = t122 * t262 * t20
      t461 = -t355 - 0.2160D4 * t358 + t365 + t371 + 0.576D3 * t376 + 0.
     #72D2 * t437 * t295 * t221 + 0.432D3 * t128 * t195 * t196 * t79 + 0
     #.3096D4 * t168 * t169 * t170 * t36 * t41 - 0.72D2 * t381 - t385 + 
     #0.72D2 * t437 * t88 * t79 * x2 * x1 - 0.576D3 * t4 * t6 * t9 * t71
     # * t248
      rrgq2qgh63J3 = -(wd * (t154 + t243 + t322 + t386) + wd * (t403 + t
     #420 + t433 + t461)) / t1 / t41 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh63J4
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
      t25 = x3 * t12
      t27 = x2 * t20
      t29 = cos(x4 * 0.3141592653589793D1)
      t33 = Sqrt(t25 * t17 * x2 * t20)
      t35 = 0.2D1 * t29 * t33
      t36 = t25 * t17 + t27 - t35
      t41 = s - t23 * x1 * t18 * t36 - t23 * t8 * x3
      t45 = 0.1440D4 * t7 * t10 * t19 * t22 * t41
      t46 = t5 * t4
      t47 = t3 * t46
      t48 = t9 * t8
      t50 = t21 * t41
      t52 = t47 * t48 * t19 * t50
      t54 = t3 * z
      t56 = t8 * t15
      t60 = 0.288D3 * t54 * t4 * t56 * t18 * t41
      t61 = t2 * t1
      t62 = t61 * z
      t64 = t9 * t15
      t65 = t18 * t20
      t66 = t64 * t65
      t69 = t6 * t48
      t71 = t17 ** 2
      t72 = 0.1D1 / t71
      t73 = t15 * t72
      t78 = t6 * t5
      t82 = 0.1D1 / t71 / t17
      t83 = t15 * t82
      t84 = x2 ** 2
      t86 = x1 ** 2
      t91 = t46 * t9
      t94 = t72 * t41
      t97 = x2 * x3
      t98 = t20 * t12 * t17 + t97 + t35
      t100 = x1 * t98 * x3
      t103 = 0.288D3 * t54 * t91 * t15 * t94 * t100
      t104 = t3 * t78
      t105 = t48 * t15
      t107 = t71 ** 2
      t108 = 0.1D1 / t107
      t109 = t86 * x1
      t110 = t108 * t109
      t111 = t36 * t41
      t114 = t104 * t105 * t110 * t111 * t84
      t116 = t6 * t4
      t117 = t3 * t116
      t118 = t10 * t15
      t120 = t117 * t118 * t72
      t121 = t20 * t41
      t122 = t97 * x1
      t124 = t120 * t121 * t122
      t128 = t98 ** 2
      t134 = t47 * t56
      t135 = t82 * t86
      t136 = t36 ** 2
      t137 = t136 * t41
      t139 = t134 * t135 * t137
      t144 = t72 * t20
      t148 = z ** 2
      t153 = 0.576D3 * t61 * t148 * z * t5 * t66
      t154 = t45 - 0.1440D4 * t52 - t60 + 0.576D3 * t62 * t5 * t66 - 0.1
     #152D4 * t62 * t69 * t73 * t27 * x1 + 0.576D3 * t62 * t78 * t10 * t
     #83 * t20 * t84 * t86 + t103 - 0.144D3 * t114 + 0.144D3 * t124 + 0.
     #108D3 * t104 * t105 * t108 * t121 * t109 * t128 * x2 + 0.14832D5 *
     # t139 - 0.1152D4 * t62 * t116 * t10 * t15 * t144 * t122 + t153
      t157 = t86 * t36
      t158 = t41 * t98
      t161 = t7 * t64 * t82 * t157 * t158 * x3
      t163 = t3 * t5
      t164 = t163 * t64
      t165 = t41 * z
      t168 = 0.1728D4 * t164 * t65 * t165
      t172 = t41 * t86 * t128
      t174 = t54 * t46 * t8 * t83 * t172
      t179 = t41 * x1 * t98
      t182 = 0.288D3 * t54 * t5 * t8 * t73 * t179
      t184 = t82 * t20
      t186 = t41 * t84 * t86
      t191 = t72 * t22
      t194 = 0.72D2 * t117 * t118 * t191 * t179
      t196 = t3 * t6 * t46
      t198 = t10 * t8 * t15
      t200 = t82 * t21
      t204 = t69 * t15
      t208 = 0.288D3 * t54 * t204 * t94 * t122
      t209 = t6 * t9
      t214 = t86 * t98 * x2
      t216 = t54 * t209 * t15 * t82 * t41 * t214
      t218 = t7 * t56
      t221 = t218 * t110 * t111 * t128
      t231 = t117 * t105 * t82
      t238 = 0.1728D4 * t164 * t65 * t41 * t148
      t239 = t47 * t105
      t243 = 0.1728D4 * t239 * t65 * t165 * x3
      t244 = -0.14976D5 * t161 + t168 - 0.144D3 * t174 + t182 - 0.288D3 
     #* t104 * t118 * t184 * t186 + t194 + 0.72D2 * t196 * t198 * t200 *
     # t186 + t208 - 0.288D3 * t216 - 0.144D3 * t221 + 0.108D3 * t196 * 
     #t118 * t108 * t121 * t109 * t98 * t84 - 0.576D3 * t231 * t121 * t2
     #14 - t238 - t243
      t246 = t7 * t118
      t248 = t41 * x3
      t250 = t246 * t18 * t21 * t248
      t252 = t117 * t64
      t255 = t252 * t110 * t137 * x2
      t257 = t7 * t64
      t260 = t257 * t135 * t111 * x2
      t269 = t41 * x2 * x1
      t272 = 0.288D3 * t54 * t91 * t73 * t269
      t273 = t116 * t48
      t296 = t117 * t64 * t108 * t109 * t36 * t158 * x2
      t304 = t134 * t135 * t111 * t98
      t309 = t6 ** 2
      t312 = t108 * t20
      t319 = 0.1440D4 * t250 + 0.144D3 * t255 - 0.14976D5 * t260 + 0.144
     #D3 * t104 * t118 * t82 * t50 * t214 + t272 + 0.1152D4 * t62 * t273
     # * t15 * t184 * t214 - 0.1152D4 * t62 * t91 * t73 * t20 * x1 * t98
     # + 0.576D3 * t62 * t209 * t83 * t20 * t86 * t128 + 0.288D3 * t296 
     #- 0.1152D4 * t62 * t204 * t144 * t100 + 0.14832D5 * t304 - 0.288D3
     # * t257 * t184 * t172 + 0.36D2 * t3 * t309 * t198 * t312 * t41 * t
     #84 * x2 * t109
      t320 = t46 * t48
      t322 = x3 ** 2
      t323 = t41 * t322
      t326 = 0.288D3 * t54 * t320 * t19 * t323
      t329 = t257 * t135 * t137 * x3
      t331 = t47 * t64
      t333 = t331 * t144 * t179
      t335 = t7 * t105
      t337 = t335 * t144 * t269
      t352 = t7 * t105 * t72 * t121 * t100
      t367 = t218 * t110 * t137 * t98
      t371 = t54 * t273 * t83 * t186
      t376 = 0.72D2 * t104 * t198 * t191 * t269
      t379 = t231 * t157 * t248 * x2
      t381 = t72 * x1
      t384 = t331 * t381 * t111 * x3
      t386 = -t326 - 0.14832D5 * t329 + 0.144D3 * t333 + 0.144D3 * t337 
     #+ 0.36D2 * t252 * t312 * t41 * t109 * t128 * t98 + 0.72D2 * t117 *
     # t105 * t200 * t172 + 0.144D3 * t352 + 0.1152D4 * t62 * t320 * t19
     # * t20 * x3 + 0.576D3 * t62 * t6 * t10 * t19 * t20 * t322 - 0.144D
     #3 * t367 - 0.144D3 * t371 + t376 + 0.14832D5 * t379 + 0.576D3 * t3
     #84
      t410 = -0.288D3 * t163 * t9 * t19 * t121 - t45 + 0.1728D4 * t52 + 
     #t60 + t103 - 0.936D3 * t114 - 0.72D2 * t124 + 0.3096D4 * t218 * t1
     #10 * t136 * t36 * t41 - 0.15552D5 * t139 - 0.288D3 * t246 * t65 * 
     #t323 + 0.432D3 * t163 * t56 * t381 * t111
      t427 = 0.72D2 * t120 * t121 * t322 * x1 * t98 - t153 + 0.15552D5 *
     # t161 - 0.576D3 * t54 * t5 * t9 * t19 * t248 - t168 + 0.288D3 * t1
     #74 + t182 + 0.72D2 * t120 * t50 * t100 - t194 + t208 + 0.576D3 * t
     #216
      t431 = t104 * t198 * t72
      t450 = -0.936D3 * t221 + 0.72D2 * t431 * t121 * t322 * x2 * x1 + t
     #238 + t243 - 0.1728D4 * t250 + 0.576D3 * t239 * t65 * t248 - 0.216
     #0D4 * t255 + 0.15552D5 * t260 + 0.72D2 * t431 * t50 * t122 + 0.432
     #D3 * t335 * t381 * t111 * t322 + t272
      t461 = 0.1872D4 * t296 - 0.15696D5 * t304 + t326 + 0.15552D5 * t32
     #9 - 0.72D2 * t333 - 0.72D2 * t337 - 0.72D2 * t352 + 0.2160D4 * t36
     #7 + 0.288D3 * t371 - t376 - 0.15696D5 * t379 - 0.864D3 * t384
      rrgq2qgh63J4 = -(wd * (t154 + t244 + t319 + t386) + wd * (t410 + t
     #427 + t450 + t461)) / t1 / t41 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh63J5
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
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = 0.1D1 - x2
      t13 = z + x1 * t10 * t5
      t14 = t9 * t13
      t16 = z + x1 * t5
      t17 = 0.1D1 / t16
      t18 = 0.1D1 - x3
      t19 = t17 * t18
      t20 = t14 * t19
      t23 = z ** 2
      t28 = 0.576D3 * t3 * t23 * z * t6 * t20
      t29 = t2 * s
      t30 = t29 * z
      t32 = t8 * t13
      t33 = s * t5
      t35 = x3 * t10
      t37 = x2 * t18
      t39 = cos(x4 * 0.3141592653589793D1)
      t43 = Sqrt(t35 * t16 * x2 * t18)
      t45 = 0.2D1 * t39 * t43
      t46 = t35 * t16 + t37 - t45
      t51 = s - t33 * x1 * t17 * t46 - t33 * t8 * x3
      t55 = 0.288D3 * t30 * t5 * t32 * t17 * t51
      t56 = t6 * t5
      t57 = t29 * t56
      t58 = t9 * t8
      t60 = t13 * t17
      t61 = t18 ** 2
      t62 = t61 * t51
      t64 = t57 * t58 * t60 * t62
      t66 = t6 ** 2
      t67 = t29 * t66
      t68 = t9 ** 2
      t70 = t61 * t18
      t74 = 0.1440D4 * t67 * t68 * t60 * t70 * t51
      t75 = t67 * t32
      t76 = t16 ** 2
      t77 = t76 ** 2
      t78 = 0.1D1 / t77
      t79 = x1 ** 2
      t80 = t79 * x1
      t81 = t78 * t80
      t82 = t46 * t51
      t85 = x2 * x3
      t86 = t18 * t10 * t16 + t85 + t45
      t87 = t86 ** 2
      t90 = t75 * t81 * t82 * t87
      t93 = 0.1D1 / t76 / t16
      t96 = t79 * t46
      t97 = t51 * t86
      t100 = t67 * t14 * t93 * t96 * t97 * x3
      t102 = t56 * t9
      t105 = 0.1D1 / t76
      t106 = t105 * t51
      t108 = x1 * t86 * x3
      t111 = 0.288D3 * t30 * t102 * t13 * t106 * t108
      t112 = t66 * t5
      t113 = t112 * t58
      t115 = t13 * t93
      t116 = x2 ** 2
      t118 = t51 * t116 * t79
      t120 = t30 * t113 * t115 * t118
      t122 = t56 * t58
      t124 = x3 ** 2
      t125 = t51 * t124
      t128 = 0.288D3 * t30 * t122 * t60 * t125
      t129 = t67 * t14
      t130 = t93 * t79
      t131 = t46 ** 2
      t132 = t131 * t51
      t135 = t129 * t130 * t132 * x3
      t137 = t66 * t58
      t139 = t13 * t105
      t144 = t66 * t6
      t152 = 0.576D3 * t4 * t6 * t20 + t28 - t55 - 0.1440D4 * t64 + t74 
     #- 0.144D3 * t90 - 0.14976D5 * t100 + t111 - 0.144D3 * t120 - t128 
     #- 0.14832D5 * t135 - 0.1152D4 * t4 * t137 * t139 * t37 * x1 + 0.57
     #6D3 * t4 * t144 * t68 * t115 * t18 * t116 * t79
      t153 = t29 * t112
      t154 = t58 * t13
      t156 = t153 * t154 * t93
      t157 = t18 * t51
      t159 = t79 * t86 * x2
      t163 = t68 * t13
      t165 = t105 * t70
      t167 = t51 * x1 * t86
      t170 = 0.72D2 * t153 * t163 * t165 * t167
      t171 = t51 * x3
      t174 = t156 * t96 * t171 * x2
      t176 = t153 * t14
      t179 = t176 * t81 * t132 * x2
      t183 = t129 * t130 * t82 * x2
      t185 = t29 * t144
      t189 = t185 * t154 * t81 * t82 * t116
      t191 = t57 * t14
      t192 = t105 * t18
      t194 = t191 * t192 * t167
      t204 = t75 * t81 * t132 * t86
      t206 = t67 * t154
      t208 = t51 * x2 * x1
      t210 = t206 * t192 * t208
      t217 = t66 * t9
      t227 = t51 * t79 * t87
      t229 = t30 * t56 * t8 * t115 * t227
      t231 = t67 * t163
      t234 = t231 * t17 * t61 * t171
      t236 = -0.576D3 * t156 * t157 * t159 + t170 + 0.14832D5 * t174 + 0
     #.144D3 * t179 - 0.14976D5 * t183 - 0.144D3 * t189 + 0.144D3 * t194
     # + 0.576D3 * t4 * t66 * t68 * t60 * t18 * t124 - 0.144D3 * t204 + 
     #0.144D3 * t210 + 0.1152D4 * t4 * t122 * t60 * t18 * x3 + 0.576D3 *
     # t4 * t217 * t115 * t18 * t79 * t87 - 0.144D3 * t229 + 0.1440D4 * 
     #t234
      t239 = t93 * t61
      t244 = t29 * t66 * t56
      t257 = t153 * t14 * t78 * t80 * t46 * t97 * x2
      t259 = t137 * t13
      t268 = t30 * t217 * t13 * t93 * t51 * t159
      t271 = t85 * x1
      t274 = 0.288D3 * t30 * t259 * t106 * t271
      t278 = 0.288D3 * t30 * t102 * t139 * t208
      t280 = t68 * t8 * t13
      t284 = 0.72D2 * t185 * t280 * t165 * t208
      t285 = t105 * x1
      t288 = t191 * t285 * t82 * x3
      t290 = t57 * t32
      t292 = t290 * t130 * t132
      t300 = t66 ** 2
      t303 = t78 * t18
      t314 = 0.72D2 * t153 * t154 * t239 * t227 + 0.108D3 * t244 * t163 
     #* t78 * t157 * t80 * t86 * t116 + 0.288D3 * t257 - 0.1152D4 * t4 *
     # t259 * t192 * t108 - 0.288D3 * t268 + t274 + t278 + t284 + 0.576D
     #3 * t288 + 0.14832D5 * t292 - 0.1152D4 * t4 * t112 * t68 * t13 * t
     #192 * t271 + 0.36D2 * t29 * t300 * t280 * t303 * t51 * t116 * x2 *
     # t80 + 0.72D2 * t244 * t280 * t239 * t118
      t317 = t93 * t18
      t334 = t67 * t154 * t105 * t157 * t108
      t346 = t153 * t163 * t105
      t348 = t346 * t157 * t271
      t350 = t29 * t6
      t351 = t350 * t14
      t352 = t51 * z
      t355 = 0.1728D4 * t351 * t19 * t352
      t359 = 0.1728D4 * t351 * t19 * t51 * t23
      t360 = t57 * t154
      t364 = 0.1728D4 * t360 * t19 * t352 * x3
      t379 = t290 * t130 * t82 * t86
      t385 = 0.288D3 * t30 * t6 * t8 * t139 * t167
      t386 = 0.1152D4 * t4 * t113 * t13 * t317 * t159 - 0.1152D4 * t4 * 
     #t102 * t139 * t18 * x1 * t86 - 0.288D3 * t185 * t163 * t317 * t118
     # + 0.144D3 * t334 + 0.36D2 * t176 * t303 * t51 * t80 * t87 * t86 -
     # 0.288D3 * t129 * t317 * t227 + 0.144D3 * t348 + t355 - t359 - t36
     #4 + 0.144D3 * t185 * t163 * t93 * t62 * t159 + 0.108D3 * t185 * t1
     #54 * t78 * t157 * t80 * t87 * x2 + 0.14832D5 * t379 + t385
      t404 = t185 * t280 * t105
      t410 = -t28 + t55 + 0.1728D4 * t64 - t74 - 0.288D3 * t350 * t9 * t
     #60 * t157 - 0.936D3 * t90 + 0.15552D5 * t100 - 0.288D3 * t231 * t1
     #9 * t125 + 0.576D3 * t360 * t19 * t171 + 0.72D2 * t404 * t157 * t1
     #24 * x2 * x1 + t111
      t424 = 0.288D3 * t120 + t128 + 0.15552D5 * t135 - t170 + 0.72D2 * 
     #t404 * t62 * t271 + 0.72D2 * t346 * t62 * t108 - 0.15696D5 * t174 
     #- 0.2160D4 * t179 + 0.15552D5 * t183 - 0.936D3 * t189 - 0.72D2 * t
     #194
      t438 = 0.2160D4 * t204 - 0.72D2 * t210 + 0.288D3 * t229 - 0.1728D4
     # * t234 - 0.576D3 * t30 * t6 * t9 * t60 * t171 + 0.1872D4 * t257 +
     # 0.576D3 * t268 + t274 + t278 - t284 - 0.864D3 * t288
      t461 = -0.15552D5 * t292 - 0.72D2 * t334 + 0.432D3 * t206 * t285 *
     # t82 * t124 + 0.3096D4 * t75 * t81 * t131 * t46 * t51 + 0.432D3 * 
     #t350 * t32 * t285 * t82 + 0.72D2 * t346 * t157 * t124 * x1 * t86 -
     # 0.72D2 * t348 - t355 + t359 + t364 - 0.15696D5 * t379 + t385
      rrgq2qgh63J5 = -(wd * (t152 + t236 + t314 + t386) + wd * (t410 + t
     #424 + t438 + t461)) / t1 / t51 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh63J6
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
      t9 = 0.1D1 - x2
      t12 = z + x1 * t9 * t4
      t13 = t8 * t12
      t14 = t7 * t13
      t16 = z + x1 * t4
      t17 = t16 ** 2
      t18 = t17 ** 2
      t19 = 0.1D1 / t18
      t20 = x1 ** 2
      t21 = t20 * x1
      t22 = t19 * t21
      t23 = x3 * t9
      t25 = 0.1D1 - x3
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t23 * t16 * x2 * t25)
      t34 = 0.2D1 * t28 * t32
      t35 = t23 * t16 + x2 * t25 - t34
      t36 = t35 ** 2
      t37 = s * t4
      t38 = 0.1D1 / t16
      t44 = s - t37 * x1 * t38 * t35 - t37 * t8 * x3
      t45 = t36 * t44
      t49 = t25 * t9 * t16 + x2 * x3 + t34
      t54 = t6 * t4
      t55 = t3 * t54
      t56 = t8 ** 2
      t57 = t56 * t8
      t58 = t57 * t12
      t60 = 0.1D1 / t17 / t16
      t63 = t20 * t35
      t64 = t44 * x3
      t69 = t56 ** 2
      t70 = t69 * t12
      t71 = 0.1D1 / t17
      t73 = t55 * t70 * t71
      t74 = t25 ** 2
      t75 = t74 * t44
      t77 = x1 * t49 * x3
      t81 = t3 * z
      t84 = t12 * t38
      t89 = t3 * t6 * t5
      t91 = t69 * t8 * t12
      t93 = t89 * t91 * t71
      t95 = x3 * x2 * x1
      t99 = t7 * t70
      t100 = t38 * t25
      t101 = x3 ** 2
      t102 = t44 * t101
      t106 = t5 * t4
      t107 = t3 * t106
      t108 = t107 * t58
      t112 = t56 * t12
      t113 = t7 * t112
      t114 = t60 * t20
      t119 = t107 * t112
      t120 = t71 * t25
      t122 = t44 * x1 * t49
      t131 = t3 * t5
      t133 = t71 * x1
      t134 = t35 * t44
      t138 = 0.2160D4 * t14 * t22 * t45 * t49 - 0.15696D5 * t55 * t58 * 
     #t60 * t63 * t64 * x2 + 0.72D2 * t73 * t75 * t77 - 0.576D3 * t81 * 
     #t5 * t56 * t84 * t64 + 0.72D2 * t93 * t75 * t95 - 0.288D3 * t99 * 
     #t100 * t102 + 0.576D3 * t108 * t100 * t64 + 0.15552D5 * t113 * t11
     #4 * t45 * x3 - 0.72D2 * t119 * t120 * t122 + 0.3096D4 * t14 * t22 
     #* t36 * t35 * t44 + 0.432D3 * t131 * t13 * t133 * t134
      t139 = t25 * t44
      t155 = z ** 2
      t167 = t74 * t25
      t175 = t44 * t49
      t182 = t12 * t60
      t184 = t49 ** 2
      t192 = t71 * t44
      t198 = t12 * t71
      t207 = 0.72D2 * t73 * t139 * t101 * x1 * t49 + 0.1728D4 * t107 * t
     #57 * t84 * t75 + 0.288D3 * t81 * t4 * t13 * t38 * t44 - 0.576D3 * 
     #t2 * t1 * t155 * z * t5 * t112 * t100 - 0.288D3 * t131 * t56 * t84
     # * t139 - 0.1440D4 * t7 * t69 * t84 * t167 * t44 + 0.1872D4 * t55 
     #* t112 * t19 * t21 * t35 * t175 * x2 + 0.288D3 * t81 * t106 * t8 *
     # t182 * t44 * t20 * t184 + 0.288D3 * t81 * t6 * t57 * t12 * t192 *
     # t95 + 0.288D3 * t81 * t5 * t8 * t198 * t122 + 0.288D3 * t81 * t10
     #6 * t57 * t84 * t102
      t211 = x2 ** 2
      t222 = t44 * z
      t227 = t106 * t56
      t235 = t44 * x2 * x1
      t239 = t7 * t58
      t253 = t71 * t167
      t261 = t107 * t13
      t266 = 0.288D3 * t81 * t54 * t57 * t182 * t44 * t211 * t20 - 0.72D
     #2 * t7 * t58 * t71 * t139 * t77 + 0.1728D4 * t108 * t100 * t222 * 
     #x3 + 0.288D3 * t81 * t227 * t12 * t192 * t77 + 0.288D3 * t81 * t22
     #7 * t198 * t235 - 0.72D2 * t239 * t120 * t235 - 0.2160D4 * t55 * t
     #112 * t22 * t45 * x2 + 0.15552D5 * t113 * t114 * t134 * x2 - 0.72D
     #2 * t89 * t91 * t253 * t235 - 0.1728D4 * t99 * t38 * t74 * t64 - 0
     #.15696D5 * t261 * t114 * t134 * t49
      t296 = t131 * t112
      t322 = -0.936D3 * t14 * t22 * t134 * t184 + 0.15552D5 * t7 * t112 
     #* t60 * t63 * t175 * x3 - 0.864D3 * t119 * t133 * t134 * x3 - 0.15
     #552D5 * t261 * t114 * t45 + 0.432D3 * t239 * t133 * t134 * t101 - 
     #0.936D3 * t89 * t58 * t22 * t134 * t211 - 0.72D2 * t73 * t139 * t9
     #5 - 0.1728D4 * t296 * t100 * t222 + 0.1728D4 * t296 * t100 * t44 *
     # t155 - 0.72D2 * t55 * t70 * t253 * t122 + 0.576D3 * t81 * t6 * t5
     #6 * t12 * t60 * t44 * t20 * t49 * x2 + 0.72D2 * t93 * t139 * t101 
     #* x2 * x1
      rrgq2qgh63J6 = -wd * (t138 + t207 + t266 + t322) / t1 / t44 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J1
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
      t9 = t3 * t7 * t6
      t10 = x1 ** 2
      t11 = t10 ** 2
      t13 = z + x1 * t4
      t14 = t13 ** 2
      t15 = t14 ** 2
      t17 = 0.1D1 / t15 / t13
      t18 = t11 * t17
      t19 = t9 * t18
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t23 = 0.1D1 - x3
      t26 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(t21 * t13 * x2 * t23)
      t32 = 0.2D1 * t26 * t30
      t33 = t21 * t13 + x2 * t23 - t32
      t34 = 0.1D1 - x1
      t35 = t34 ** 2
      t36 = t35 * t34
      t37 = t33 * t36
      t40 = z + x1 * t20 * t4
      t43 = x2 * x3
      t44 = t23 * t20 * t13 + t43 + t32
      t45 = t40 * t44
      t46 = x2 ** 2
      t51 = t7 * t4
      t52 = t3 * t51
      t53 = t35 ** 2
      t54 = t53 * t23
      t56 = 0.1D1 / t14
      t57 = t40 * t56
      t58 = x1 * t44
      t59 = x3 ** 2
      t64 = t23 ** 2
      t65 = t53 * t64
      t69 = t57 * x3 * x1 * t44
      t72 = t3 * t7
      t73 = t36 * t23
      t77 = t10 * x1
      t78 = 0.1D1 / t15
      t79 = t77 * t78
      t81 = t33 * t53
      t82 = t40 * x3
      t87 = t7 * t5
      t88 = t3 * t87
      t90 = 0.1D1 / t14 / t13
      t91 = t10 * t90
      t93 = t40 * t59
      t98 = t35 * t23
      t101 = t44 ** 2
      t102 = t44 * t101
      t107 = t2 * s
      t108 = t107 * t7
      t110 = s * t4
      t111 = 0.1D1 / t13
      t117 = s - t110 * x1 * t111 * t44 - t110 * t34 * t23
      t118 = t117 * t40
      t119 = t111 * t59
      t123 = t107 * t6
      t125 = t56 * x1
      t127 = t118 * t125 * t44
      t130 = t107 * t87
      t132 = t33 * t117
      t133 = t36 * t40
      t138 = t88 * t18
      t139 = t33 ** 2
      t140 = t139 * t33
      t141 = t140 * t35
      t146 = t108 * t79
      t147 = t139 * t117
      t148 = t34 * t40
      t153 = t3 * t6
      t154 = t153 * t36
      t155 = t64 * t40
      t159 = t53 * t34
      t160 = t52 * t159
      t161 = t64 ** 2
      t168 = t23 * t40
      t172 = t72 * t53
      t173 = t64 * t23
      t174 = t173 * t40
      t178 = t107 * t51
      t179 = t73 * t117
      t181 = t40 * t90
      t187 = 0.28D2 * t19 * t37 * t45 * t46 + 0.168D3 * t52 * t54 * t57 
     #* t58 * t59 - 0.112D3 * t52 * t65 * t69 - 0.112D3 * t72 * t73 * t6
     #9 - 0.144D3 * t9 * t79 * t81 * t82 * t46 + 0.216D3 * t88 * t91 * t
     #81 * t93 * x2 + 0.28D2 * t52 * t98 * t40 * t78 * t77 * t102 + 0.31
     #2D3 * t108 * t54 * t118 * t119 + 0.96D2 * t123 * t98 * t127 + 0.43
     #0D3 * t130 * t79 * t132 * t133 * t46 + 0.108D3 * t138 * t141 * t40
     # * x2 + 0.182D3 * t146 * t147 * t148 * t44 + 0.108D3 * t154 * t155
     # * t111 + 0.36D2 * t160 * t161 * t40 * t111 + 0.36D2 * t3 * t5 * t
     #35 * t168 * t111 + 0.108D3 * t172 * t174 * t111 + 0.94D2 * t178 * 
     #t179 * t181 * t10 * t44 * x2
      t189 = t35 * t40
      t194 = t52 * t18
      t196 = t40 * t101
      t200 = t139 * t35
      t201 = t45 * x2
      t205 = t52 * t79
      t206 = t82 * t44
      t210 = t52 * t91
      t215 = t33 * t35
      t221 = t57 * t58
      t225 = t10 * t101
      t226 = t181 * t225
      t229 = t36 * t64
      t236 = t118 * t56 * x2 * x1
      t239 = t108 * t179
      t242 = t7 ** 2
      t257 = t139 * t36
      t275 = -0.328D3 * t178 * t79 * t147 * t189 * x2 + 0.36D2 * t194 * 
     #t139 * t34 * t196 + 0.56D2 * t138 * t200 * t201 - 0.112D3 * t205 *
     # t200 * t206 + 0.168D3 * t210 * t37 * t45 * t59 - 0.144D3 * t205 *
     # t215 * t196 * x3 + 0.28D2 * t153 * t98 * t221 + 0.36D2 * t72 * t9
     #8 * t226 + 0.56D2 * t72 * t229 * t221 + 0.96D2 * t108 * t73 * t236
     # - 0.128D3 * t239 * t69 + 0.36D2 * t3 * t242 * t18 * t81 * t40 * t
     #46 * x2 - 0.128D3 * t123 * t98 * t117 * t57 * z * x1 * t44 + 0.108
     #D3 * t19 * t257 * t40 * t46 + 0.10D2 * t146 * t140 * t117 * t148 +
     # 0.36D2 * t138 * t215 * t196 * x2 + 0.56D2 * t108 * t125 * t132 * 
     #t133 * t59
      t277 = t88 * t79
      t285 = t111 * x3
      t286 = t118 * t285
      t289 = t59 * x3
      t312 = t40 * t111
      t317 = t139 ** 2
      t335 = t91 * t33
      t337 = t117 * t35
      t343 = t82 * x2
      t356 = 0.16D2 * t277 * t102 * t36 * x3 * t23 * t40 - 0.880D3 * t12
     #3 * t73 * t286 - 0.144D3 * t160 * t168 * t111 * t289 - 0.288D3 * t
     #172 * t155 * t285 + 0.216D3 * t160 * t155 * t119 - 0.144D3 * t160 
     #* t174 * t285 - 0.144D3 * t154 * t168 * t285 + 0.216D3 * t172 * t1
     #68 * t119 + 0.748D3 * t107 * t5 * t35 * t23 * t117 * t312 + 0.36D2
     # * t52 * t11 * t17 * t317 * t148 - 0.16D2 * t123 * t36 * t64 * t11
     #7 * t312 - 0.12D2 * t108 * t53 * t173 * t117 * t312 + 0.36D2 * t52
     # * t229 * t226 + 0.400D3 * t108 * t335 * t337 * t206 - 0.584D3 * t
     #178 * t335 * t117 * t36 * t343 + 0.28D2 * t194 * t140 * t34 * t45 
     #+ 0.28D2 * t194 * t33 * t34 * t40 * t102
      t365 = t79 * t33
      t438 = 0.328D3 * t108 * t91 * t147 * t189 * x3 - 0.148D3 * t108 * 
     #t229 * t127 - 0.530D3 * t178 * t365 * t337 * t201 + 0.216D3 * t210
     # * t257 * t93 - 0.144D3 * t52 * t125 * t81 * t40 * t289 - 0.288D3 
     #* t277 * t257 * t343 - 0.128D3 * t178 * t54 * t117 * t57 * t43 * x
     #1 + 0.47D2 * t108 * t98 * t118 * t91 * t101 - 0.144D3 * t205 * t14
     #1 * t82 - 0.240D3 * t108 * t65 * t286 + 0.16D2 * t88 * t125 * t44 
     #* t159 * t289 * t23 * t40 - 0.128D3 * t239 * t57 * x2 * x1 * z + 0
     #.47D2 * t130 * t54 * t118 * t90 * t46 * t10 - 0.148D3 * t178 * t65
     # * t236 + 0.192D3 * t146 * t132 * t148 * t101 - 0.112D3 * t88 * t3
     #65 * t133 * x2 * t44 * x3 + 0.28D2 * t52 * t53 * t173 * t221 - 0.1
     #44D3 * t52 * t73 * t181 * t225 * x3
      rrgq2qgh64J1 = -wd * (t187 + t275 + t356 + t438) / t1 / t117 / z /
     # 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J2
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
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 ** 2
      t12 = 0.1D1 - x3
      t13 = t11 * t12
      t15 = 0.1D1 - x2
      t18 = z + x1 * t15 * t4
      t20 = z + x1 * t4
      t21 = t20 ** 2
      t22 = 0.1D1 / t21
      t23 = t18 * t22
      t26 = x2 * x3
      t28 = cos(x4 * 0.3141592653589793D1)
      t29 = x3 * t15
      t33 = Sqrt(t29 * t20 * x2 * t12)
      t35 = 0.2D1 * t28 * t33
      t36 = t12 * t15 * t20 + t26 + t35
      t37 = x1 * t36
      t38 = x3 ** 2
      t40 = t23 * t37 * t38
      t41 = t8 * t13 * t40
      t43 = t12 ** 2
      t44 = t11 * t43
      t48 = t23 * x3 * x1 * t36
      t49 = t8 * t44 * t48
      t51 = t6 * t5
      t52 = t3 * t51
      t53 = x1 ** 2
      t54 = t53 * x1
      t55 = t21 ** 2
      t56 = 0.1D1 / t55
      t57 = t54 * t56
      t58 = t52 * t57
      t61 = t29 * t20 + x2 * t12 - t35
      t62 = t61 ** 2
      t63 = t10 * t9
      t64 = t62 * t63
      t65 = t18 * x3
      t66 = t65 * x2
      t70 = t5 * t4
      t72 = t3 * t6 * t70
      t74 = t61 * t11
      t75 = x2 ** 2
      t80 = t2 * s
      t81 = t80 * t70
      t82 = t10 * t12
      t84 = s * t4
      t85 = 0.1D1 / t20
      t91 = s - t84 * x1 * t85 * t36 - t84 * t9 * t12
      t92 = t91 * t18
      t93 = t22 * x1
      t95 = t92 * t93 * t36
      t96 = t81 * t82 * t95
      t99 = t11 * t9
      t101 = t38 * x3
      t106 = 0.16D2 * t52 * t93 * t36 * t99 * t101 * t12 * t18
      t107 = t80 * t6
      t108 = t63 * t12
      t109 = t108 * t91
      t110 = t107 * t109
      t111 = x2 * x1
      t114 = t110 * t23 * t111 * z
      t116 = t8 * t99
      t117 = t12 * t18
      t121 = 0.144D3 * t116 * t117 * t85 * t101
      t122 = t3 * t6
      t123 = t122 * t11
      t124 = t85 * t38
      t126 = t123 * t117 * t124
      t128 = t3 * t70
      t129 = t128 * t63
      t130 = t85 * x3
      t134 = t43 * t12
      t135 = t134 * t18
      t139 = t43 * t18
      t141 = t116 * t139 * t124
      t148 = t18 * t85
      t150 = t81 * t63 * t43 * t91 * t148
      t152 = t53 ** 2
      t155 = 0.1D1 / t55 / t20
      t156 = t62 ** 2
      t158 = t9 * t18
      t162 = t80 * t5
      t166 = t162 * t10 * t12 * t91 * t148
      t171 = t107 * t11 * t134 * t91 * t148
      t173 = 0.168D3 * t41 - 0.112D3 * t49 - 0.288D3 * t58 * t64 * t66 -
     # 0.144D3 * t72 * t57 * t74 * t65 * t75 + 0.96D2 * t96 + t106 - 0.1
     #28D3 * t114 - t121 + 0.216D3 * t126 - 0.144D3 * t129 * t117 * t130
     # - 0.144D3 * t116 * t135 * t130 + 0.216D3 * t141 - 0.288D3 * t123 
     #* t139 * t130 - 0.16D2 * t150 + 0.36D2 * t8 * t152 * t155 * t156 *
     # t158 + 0.748D3 * t166 - 0.12D2 * t171
      t177 = t43 ** 2
      t190 = t152 * t155
      t191 = t8 * t190
      t192 = t62 * t61
      t194 = t18 * t36
      t200 = 0.1D1 / t21 / t20
      t201 = t200 * t53
      t202 = t36 ** 2
      t205 = t107 * t82 * t92 * t201 * t202
      t207 = t63 * t43
      t209 = t107 * t207 * t95
      t212 = t122 * t108 * t48
      t215 = t92 * t130
      t216 = t107 * t44 * t215
      t218 = t8 * t57
      t219 = t62 * t10
      t220 = t65 * t36
      t222 = t218 * t219 * t220
      t224 = t192 * t10
      t228 = t8 * t201
      t229 = t18 * t38
      t231 = t228 * t64 * t229
      t237 = 0.144D3 * t8 * t93 * t74 * t18 * t101
      t238 = t61 * t63
      t241 = t228 * t238 * t194 * t38
      t243 = t61 * t10
      t244 = t18 * t202
      t247 = t218 * t243 * t244 * x3
      t249 = t202 * t36
      t255 = 0.16D2 * t58 * t249 * t63 * x3 * t12 * t18
      t257 = t81 * t108 * t215
      t259 = 0.108D3 * t129 * t139 * t85 + 0.36D2 * t116 * t177 * t18 * 
     #t85 + 0.36D2 * t3 * t5 * t10 * t117 * t85 + 0.108D3 * t123 * t135 
     #* t85 + 0.28D2 * t191 * t192 * t9 * t194 + 0.47D2 * t205 - 0.148D3
     # * t209 - 0.112D3 * t212 - 0.240D3 * t216 - 0.112D3 * t222 - 0.144
     #D3 * t218 * t224 * t65 + 0.216D3 * t231 - t237 + 0.168D3 * t241 - 
     #0.144D3 * t247 + t255 - 0.880D3 * t257
      t261 = t72 * t190
      t266 = t52 * t190
      t271 = t6 ** 2
      t279 = t80 * t7
      t280 = t57 * t61
      t282 = t91 * t10
      t283 = t194 * x2
      t285 = t279 * t280 * t282 * t283
      t287 = t80 * t51
      t292 = t287 * t13 * t92 * t200 * t75 * t53
      t294 = t107 * t57
      t295 = t61 * t91
      t298 = t294 * t295 * t158 * t202
      t301 = t63 * t18
      t305 = t52 * t280 * t301 * x2 * t36 * x3
      t309 = t23 * t37
      t315 = t23 * t26 * x1
      t316 = t279 * t13 * t91 * t315
      t318 = t62 * t91
      t319 = t158 * t36
      t321 = t294 * t318 * t319
      t326 = t92 * t22 * x2 * x1
      t327 = t107 * t108 * t326
      t329 = t52 * t201
      t332 = t329 * t74 * t229 * x2
      t338 = t8 * t82 * t18 * t56 * t54 * t249
      t341 = t18 * t200
      t345 = t279 * t109 * t341 * t53 * t36 * x2
      t349 = t294 * t192 * t91 * t158
      t353 = t266 * t243 * t244 * x2
      t358 = 0.108D3 * t261 * t64 * t18 * t75 + 0.108D3 * t266 * t224 * 
     #t18 * x2 + 0.36D2 * t3 * t271 * t190 * t74 * t18 * t75 * x2 - 0.53
     #0D3 * t285 + 0.47D2 * t292 + 0.192D3 * t298 - 0.112D3 * t305 + 0.2
     #8D2 * t8 * t11 * t134 * t309 - 0.128D3 * t316 + 0.182D3 * t321 + 0
     #.96D2 * t327 + 0.216D3 * t332 + 0.28D2 * t338 + 0.94D2 * t345 + 0.
     #10D2 * t349 + 0.36D2 * t353 + 0.56D2 * t266 * t219 * t283
      t360 = t10 * t18
      t361 = t360 * x2
      t363 = t279 * t57 * t318 * t361
      t367 = t191 * t62 * t9 * t244
      t369 = t201 * t61
      t372 = t107 * t369 * t282 * t220
      t377 = t279 * t369 * t91 * t63 * t66
      t383 = t53 * t202
      t384 = t341 * t383
      t385 = t8 * t207 * t384
      t390 = t191 * t61 * t9 * t18 * t249
      t392 = t107 * t201
      t393 = t360 * x3
      t395 = t392 * t318 * t393
      t400 = t107 * t93 * t295 * t301 * t38
      t407 = t81 * t82 * t91 * t23 * z * x1 * t36
      t411 = t107 * t13 * t92 * t124
      t416 = t8 * t108 * t341 * t383 * x3
      t426 = t122 * t82 * t384
      t429 = t279 * t44 * t326
      t434 = t287 * t57 * t295 * t301 * t75
      t436 = t110 * t48
      t438 = -0.328D3 * t363 + 0.36D2 * t367 + 0.400D3 * t372 - 0.584D3 
     #* t377 + 0.56D2 * t122 * t207 * t309 + 0.36D2 * t385 + 0.28D2 * t3
     #90 + 0.328D3 * t395 + 0.56D2 * t400 - 0.128D3 * t407 + 0.312D3 * t
     #411 - 0.144D3 * t416 + 0.28D2 * t261 * t238 * t194 * t75 + 0.28D2 
     #* t128 * t82 * t309 + 0.36D2 * t426 - 0.148D3 * t429 + 0.430D3 * t
     #434 - 0.128D3 * t436
      t446 = t80 * t91
      t448 = t446 * t6 * t63
      t459 = -0.344D3 * t41 + 0.144D3 * t49 - 0.640D3 * t96 - t106 + 0.2
     #56D3 * t114 - 0.128D3 * t448 * t315 + t121 - 0.72D2 * t126 - 0.72D
     #2 * t141 + 0.608D3 * t150 - 0.592D3 * t166 - 0.16D2 * t171 - 0.54D
     #2 * t205 + 0.216D3 * t209 + 0.144D3 * t212
      t478 = t81 * t201
      t487 = t446 * t70 * t10
      t491 = -0.64D2 * t216 + 0.144D3 * t222 - 0.72D2 * t231 + t237 - 0.
     #344D3 * t241 + 0.256D3 * t247 - t255 + 0.640D3 * t257 + 0.64D2 * t
     #446 * t7 * t11 * t23 * t38 * x2 * x1 + 0.444D3 * t285 - 0.54D2 * t
     #292 + 0.256D3 * t392 * t295 * t361 - 0.256D3 * t478 * t295 * t319 
     #+ 0.136D3 * t162 * t93 * t295 * t158 + 0.64D2 * t487 * t23 * t111
      t510 = -0.30D2 * t298 + 0.144D3 * t305 + 0.496D3 * t316 - 0.108D3 
     #* t321 - 0.640D3 * t327 - 0.72D2 * t332 - 0.56D2 * t338 - 0.108D3 
     #* t345 + 0.64D2 * t448 * t40 - 0.128D3 * t487 * t48 - 0.62D2 * t34
     #9 - 0.72D2 * t353 + 0.604D3 * t363 - 0.72D2 * t367 - 0.288D3 * t37
     #2
      t540 = 0.544D3 * t377 - 0.72D2 * t385 - 0.56D2 * t390 - 0.256D3 * 
     #t395 + 0.24D2 * t400 + 0.256D3 * t407 - 0.264D3 * t411 + 0.256D3 *
     # t416 - 0.72D2 * t426 + 0.216D3 * t429 + 0.64D2 * t446 * t5 * t9 *
     # t309 + 0.32D2 * t329 * t202 * t11 * t38 * t12 * t18 - 0.542D3 * t
     #434 + 0.496D3 * t436 - 0.216D3 * t81 * t93 * t295 * t393 - 0.256D3
     # * t478 * t318 * t158
      rrgq2qgh64J2 = -(wd * (t173 + t259 + t358 + t438) + wd * (t459 + t
     #491 + t510 + t540)) / t1 / t91 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J3
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
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = t9 * t14
      t16 = t7 * t15
      t17 = 0.1D1 - x2
      t18 = x3 * t17
      t20 = 0.1D1 - x3
      t23 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(t18 * t11 * x2 * t20)
      t29 = 0.2D1 * t23 * t27
      t30 = t18 * t11 + x2 * t20 - t29
      t31 = t30 ** 2
      t32 = t31 * t30
      t33 = s * t4
      t34 = 0.1D1 / t11
      t38 = x2 * x3
      t39 = t20 * t17 * t11 + t38 + t29
      t42 = 0.1D1 - x1
      t45 = s - t33 * x1 * t34 * t39 - t33 * t42 * t20
      t49 = z + x1 * t17 * t4
      t50 = t42 * t49
      t52 = t16 * t32 * t45 * t50
      t54 = t42 ** 2
      t55 = t54 ** 2
      t57 = t20 ** 2
      t58 = t57 * t20
      t60 = t49 * t34
      t62 = t7 * t55 * t58 * t45 * t60
      t64 = t3 * t5
      t66 = t20 * t45
      t68 = t64 * t54 * t66 * t60
      t70 = t2 * t1
      t71 = t6 * t4
      t72 = t70 * t71
      t73 = t8 ** 2
      t76 = 0.1D1 / t13 / t11
      t77 = t31 ** 2
      t82 = t5 * t4
      t83 = t3 * t82
      t84 = t54 * t42
      t88 = t83 * t84 * t57 * t45 * t60
      t90 = t70 * t6
      t91 = t90 * t55
      t92 = t57 * t49
      t93 = t34 * x3
      t97 = t55 * t42
      t98 = t72 * t97
      t99 = x3 ** 2
      t100 = t34 * t99
      t102 = t98 * t92 * t100
      t104 = t58 * t49
      t108 = t70 * t82
      t109 = t108 * t84
      t110 = t20 * t49
      t115 = t91 * t110 * t100
      t117 = t99 * x3
      t121 = 0.144D3 * t98 * t110 * t34 * t117
      t122 = t72 * t15
      t123 = t30 * t54
      t124 = t39 ** 2
      t125 = t49 * t124
      t128 = t122 * t123 * t125 * x3
      t130 = t30 * t45
      t133 = t16 * t130 * t50 * t124
      t135 = 0.1D1 / t12
      t136 = x1 * t135
      t138 = t30 * t55
      t142 = 0.144D3 * t72 * t136 * t138 * t49 * t117
      t143 = t6 * t5
      t144 = t70 * t143
      t145 = t144 * t15
      t146 = t31 * t84
      t147 = t49 * x3
      t148 = t147 * x2
      t152 = t6 * t82
      t153 = t70 * t152
      t155 = x2 ** 2
      t161 = 0.1D1 / t12 / t11
      t162 = t8 * t161
      t163 = t144 * t162
      t164 = t49 * t99
      t167 = t163 * t138 * t164 * x2
      t169 = 0.10D2 * t52 - 0.12D2 * t62 + 0.748D3 * t68 + 0.36D2 * t72 
     #* t73 * t76 * t77 * t50 - 0.16D2 * t88 - 0.288D3 * t91 * t92 * t93
     # + 0.216D3 * t102 - 0.144D3 * t98 * t104 * t93 - 0.144D3 * t109 * 
     #t110 * t93 + 0.216D3 * t115 - t121 - 0.144D3 * t128 + 0.192D3 * t1
     #33 - t142 - 0.288D3 * t145 * t146 * t148 - 0.144D3 * t153 * t15 * 
     #t138 * t147 * t155 + 0.216D3 * t167
      t170 = t73 * t76
      t171 = t72 * t170
      t173 = t49 * t39
      t177 = t72 * t162
      t178 = t30 * t84
      t181 = t177 * t178 * t173 * t99
      t183 = t54 * t20
      t186 = t49 * t135
      t189 = t186 * z * x1 * t39
      t190 = t83 * t183 * t45 * t189
      t192 = t153 * t170
      t197 = t144 * t170
      t198 = t32 * t54
      t203 = t6 ** 2
      t206 = t155 * x2
      t214 = t84 * t57
      t216 = t45 * t49
      t218 = t216 * t136 * t39
      t219 = t7 * t214 * t218
      t221 = t84 * t20
      t222 = t221 * t45
      t223 = t7 * t222
      t226 = t186 * x3 * x1 * t39
      t227 = t223 * t226
      t229 = t55 * t20
      t232 = t7 * t229 * t216 * t100
      t235 = t49 * t161
      t236 = t8 * t124
      t238 = t235 * t236 * x3
      t239 = t72 * t221 * t238
      t242 = x1 * t39
      t244 = t186 * t242 * t99
      t245 = t72 * t229 * t244
      t248 = t124 * t39
      t251 = t171 * t30 * t42 * t49 * t248
      t253 = t7 * t162
      t254 = t31 * t45
      t255 = t54 * t49
      t256 = t255 * x3
      t258 = t253 * t254 * t256
      t262 = t197 * t123 * t125 * x2
      t264 = t31 * t54
      t265 = t173 * x2
      t270 = t49 * t14
      t272 = t270 * t9 * t248
      t273 = t72 * t183 * t272
      t275 = 0.28D2 * t171 * t32 * t42 * t173 + 0.168D3 * t181 - 0.128D3
     # * t190 + 0.108D3 * t192 * t146 * t49 * t155 + 0.108D3 * t197 * t1
     #98 * t49 * x2 + 0.36D2 * t70 * t203 * t170 * t138 * t49 * t206 - 0
     #.144D3 * t122 * t198 * t147 - 0.148D3 * t219 - 0.128D3 * t227 + 0.
     #312D3 * t232 - 0.144D3 * t239 + 0.168D3 * t245 + 0.28D2 * t251 + 0
     #.328D3 * t258 + 0.36D2 * t262 + 0.56D2 * t197 * t264 * t265 + 0.28
     #D2 * t273
      t277 = t55 * t57
      t279 = t216 * t93
      t280 = t7 * t277 * t279
      t288 = 0.16D2 * t144 * t136 * t39 * t97 * t117 * t20 * t49
      t289 = t57 ** 2
      t294 = t70 * t5
      t302 = t3 * t71
      t304 = t8 * t39
      t306 = t235 * t304 * x2
      t307 = t302 * t222 * t306
      t309 = x2 * x1
      t312 = t223 * t186 * t309 * z
      t313 = 0.128D3 * t312
      t316 = t186 * t242
      t323 = t235 * t236
      t324 = t90 * t183 * t323
      t327 = t83 * t183 * t218
      t332 = t7 * t183 * t216 * t162 * t124
      t334 = t3 * t143
      t339 = t334 * t229 * t216 * t161 * t155 * t8
      t344 = t216 * t135 * x2 * x1
      t345 = t302 * t277 * t344
      t348 = t7 * t221 * t344
      t350 = t50 * t39
      t352 = t16 * t254 * t350
      t354 = t15 * t30
      t356 = t84 * t49
      t360 = t144 * t354 * t356 * x2 * t39 * x3
      t362 = -0.240D3 * t280 + t288 + 0.36D2 * t98 * t289 * t49 * t34 + 
     #0.36D2 * t294 * t54 * t110 * t34 + 0.108D3 * t91 * t104 * t34 + 0.
     #94D2 * t307 - t313 + 0.28D2 * t72 * t55 * t58 * t316 + 0.28D2 * t1
     #08 * t183 * t316 + 0.36D2 * t324 + 0.96D2 * t327 + 0.47D2 * t332 +
     # 0.47D2 * t339 - 0.148D3 * t345 + 0.96D2 * t348 + 0.182D3 * t352 -
     # 0.112D3 * t360
      t371 = 0.16D2 * t145 * t248 * t84 * x3 * t20 * t49
      t373 = t83 * t221 * t279
      t378 = t186 * t38 * x1
      t379 = t302 * t229 * t45 * t378
      t382 = t72 * t277 * t226
      t385 = t90 * t221 * t226
      t387 = t147 * t39
      t389 = t122 * t264 * t387
      t394 = t7 * t136 * t130 * t356 * t99
      t400 = t72 * t214 * t323
      t403 = t255 * x2
      t405 = t302 * t15 * t254 * t403
      t409 = t171 * t31 * t42 * t125
      t411 = t162 * t30
      t413 = t45 * t54
      t415 = t7 * t411 * t413 * t387
      t420 = t302 * t411 * t45 * t84 * t148
      t427 = t177 * t146 * t164
      t432 = t334 * t15 * t130 * t356 * t155
      t436 = t302 * t354 * t413 * t265
      t438 = 0.108D3 * t109 * t92 * t34 + t371 - 0.880D3 * t373 - 0.128D
     #3 * t379 - 0.112D3 * t382 - 0.112D3 * t385 - 0.112D3 * t389 + 0.56
     #D2 * t394 + 0.56D2 * t90 * t214 * t316 + 0.36D2 * t400 - 0.328D3 *
     # t405 + 0.36D2 * t409 + 0.400D3 * t415 - 0.584D3 * t420 + 0.28D2 *
     # t192 * t178 * t173 * t155 + 0.216D3 * t427 + 0.430D3 * t432 - 0.5
     #30D3 * t436
      t448 = t3 * t45
      t450 = t448 * t6 * t84
      t451 = t450 * t378
      t453 = t450 * t244
      t456 = t448 * t82 * t54
      t457 = t456 * t226
      t463 = -0.62D2 * t52 - 0.16D2 * t62 - 0.592D3 * t68 + 0.608D3 * t8
     #8 - 0.72D2 * t102 - 0.72D2 * t115 + t121 - 0.128D3 * t451 + 0.64D2
     # * t453 - 0.128D3 * t457 + 0.256D3 * t128 - 0.30D2 * t133 + t142 -
     # 0.72D2 * t167 - 0.344D3 * t181
      t475 = t83 * t136 * t130 * t256
      t490 = t448 * t71 * t55 * t186 * t99 * x2 * x1
      t493 = 0.256D3 * t190 + 0.216D3 * t219 + 0.496D3 * t227 - 0.264D3 
     #* t232 + 0.256D3 * t239 - 0.344D3 * t245 - 0.56D2 * t251 - 0.256D3
     # * t258 - 0.72D2 * t262 - 0.216D3 * t475 - 0.56D2 * t273 + 0.136D3
     # * t64 * t136 * t130 * t50 + 0.64D2 * t456 * t186 * t309 + 0.64D2 
     #* t490 - 0.64D2 * t280
      t503 = 0.32D2 * t163 * t124 * t55 * t99 * t20 * t49
      t506 = t253 * t130 * t403
      t509 = t83 * t162
      t511 = t509 * t254 * t50
      t518 = -t288 - 0.108D3 * t307 + 0.256D3 * t312 - 0.72D2 * t324 + t
     #503 - 0.640D3 * t327 + 0.256D3 * t506 - 0.54D2 * t332 - 0.256D3 * 
     #t511 - 0.54D2 * t339 + 0.216D3 * t345 - 0.640D3 * t348 - 0.108D3 *
     # t352 + 0.144D3 * t360 - t371
      t531 = t509 * t130 * t350
      t537 = t448 * t5 * t42
      t538 = t537 * t316
      t540 = 0.640D3 * t373 + 0.496D3 * t379 + 0.144D3 * t382 + 0.144D3 
     #* t385 + 0.144D3 * t389 + 0.24D2 * t394 - 0.72D2 * t400 + 0.604D3 
     #* t405 - 0.72D2 * t409 - 0.288D3 * t415 + 0.544D3 * t420 - 0.256D3
     # * t531 - 0.72D2 * t427 - 0.542D3 * t432 + 0.444D3 * t436 + 0.64D2
     # * t538
      t557 = t448 * t6 * t54
      t567 = -0.304D3 * t52 + 0.64D2 * t62 + 0.64D2 * t68 - 0.128D3 * t8
     #8 - 0.64D2 * t451 + 0.128D3 * t453 - 0.64D2 * t457 - 0.288D3 * t12
     #8 - 0.28792D5 * t133 + 0.144D3 * t181 - 0.512D3 * t190 - 0.128D3 *
     # t219 + 0.64D2 * t557 * t306 + 0.48D2 * t448 * t143 * t84 * t270 *
     # t9 * t39 * t155
      t584 = t448 * t82 * t42
      t589 = z ** 2
      t598 = -0.8D1 * t227 + 0.64D2 * t232 - 0.288D3 * t239 + 0.144D3 * 
     #t245 + 0.144D3 * t251 + 0.74320D5 * t258 - 0.96D2 * t448 * t143 * 
     #t55 * t235 * x3 * t155 * t8 - 0.80D2 * t475 + 0.144D3 * t273 - 0.3
     #84D3 * t584 * t235 * t236 * z - 0.384D3 * t537 * t186 * t242 * t58
     #9 + 0.288D3 * t584 * t323 + 0.128D3 * t490 + 0.128D3 * t280 - t288
      t625 = t66 * t49
      t630 = 0.48D2 * t448 * t71 * t54 * t270 * t9 * t124 * x2 + 0.8D1 *
     # t307 - t313 + t503 + 0.384D3 * t537 * t189 + 0.256D3 * t327 + 0.2
     #5264D5 * t506 - 0.252D3 * t332 - 0.25296D5 * t511 + 0.4D1 * t339 -
     # 0.112D3 * t448 * t6 * t42 * t272 - 0.96D2 * t557 * t238 - 0.16D2 
     #* t3 * t152 * t97 * x3 * t155 * t162 * t625 - 0.29128D5 * t352 - t
     #371
      t639 = t71 * t84
      t672 = -0.128D3 * t373 - 0.8D1 * t379 + 0.128D3 * t294 * t136 * t3
     #9 * t589 * z * t50 + 0.32D2 * t448 * t639 * t235 * t155 * t8 + 0.1
     #6D2 * t448 * t152 * t55 * t270 * t206 * t9 - 0.192D3 * t448 * t639
     # * t49 * t161 * x2 * t304 * x3 + 0.32D2 * t394 - 0.27920D5 * t405 
     #+ 0.74288D5 * t415 - 0.74208D5 * t420 + 0.16D2 * t334 * t97 * t99 
     #* x2 * t136 * t625 - 0.25248D5 * t531 + 0.28208D5 * t432 + 0.536D3
     # * t436 - 0.128D3 * t538
      rrgq2qgh64J3 = -(wd * (t169 + t275 + t362 + t438) + wd * (t463 + t
     #493 + t518 + t540) + wd * (t567 + t598 + t630 + t672)) / t1 / t45 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J4
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
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = x1 ** 2
      t10 = t9 * x1
      t12 = z + x1 * t4
      t13 = t12 ** 2
      t14 = t13 ** 2
      t15 = 0.1D1 / t14
      t16 = t10 * t15
      t17 = t8 * t16
      t18 = 0.1D1 - x2
      t19 = t18 * x3
      t21 = 0.1D1 - x3
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t19 * t12 * x2 * t21)
      t30 = 0.2D1 * t24 * t28
      t31 = t19 * t12 + x2 * t21 - t30
      t32 = t31 ** 2
      t33 = 0.1D1 - x1
      t34 = t33 ** 2
      t35 = t32 * t34
      t38 = z + x1 * t18 * t4
      t39 = t38 * x3
      t42 = x2 * x3
      t43 = t21 * t18 * t12 + t42 + t30
      t44 = t39 * t43
      t46 = t17 * t35 * t44
      t49 = 0.1D1 / t13 / t12
      t50 = t9 * t49
      t51 = t8 * t50
      t52 = t34 * t33
      t53 = t31 * t52
      t54 = t38 * t43
      t55 = x3 ** 2
      t58 = t51 * t53 * t54 * t55
      t60 = t2 * s
      t61 = t60 * t6
      t62 = t52 * t21
      t63 = s * t4
      t64 = 0.1D1 / t12
      t70 = s - t63 * x1 * t64 * t43 - t63 * t33 * t21
      t71 = t62 * t70
      t72 = t61 * t71
      t73 = 0.1D1 / t13
      t74 = t38 * t73
      t77 = t74 * x3 * x1 * t43
      t78 = t72 * t77
      t80 = t61 * t50
      t81 = t32 * t70
      t82 = t34 * t38
      t83 = t82 * x3
      t85 = t80 * t81 * t83
      t87 = x1 * t73
      t89 = t31 * t70
      t90 = t52 * t38
      t93 = t61 * t87 * t89 * t90 * t55
      t95 = t9 ** 2
      t97 = 0.1D1 / t14 / t12
      t98 = t95 * t97
      t99 = t8 * t98
      t101 = t43 ** 2
      t102 = t101 * t43
      t105 = t99 * t31 * t33 * t38 * t102
      t107 = t34 ** 2
      t108 = t107 * t21
      t110 = x1 * t43
      t112 = t74 * t110 * t55
      t113 = t8 * t108 * t112
      t115 = t21 ** 2
      t116 = t107 * t115
      t118 = t8 * t116 * t77
      t120 = t5 * t4
      t121 = t3 * t120
      t122 = t34 * t21
      t124 = t74 * t110
      t127 = t3 * t6
      t129 = t38 * t49
      t130 = t9 * t101
      t131 = t129 * t130
      t132 = t127 * t122 * t131
      t134 = t32 * t31
      t139 = t60 * t7
      t140 = t16 * t31
      t142 = t70 * t34
      t143 = t54 * x2
      t145 = t139 * t140 * t142 * t143
      t148 = t70 * t38
      t151 = t148 * t73 * x2 * x1
      t152 = t139 * t116 * t151
      t155 = t61 * t62 * t151
      t157 = t6 * t120
      t158 = t3 * t157
      t159 = t158 * t98
      t160 = t32 * t52
      t161 = x2 ** 2
      t166 = t6 * t5
      t167 = t3 * t166
      t168 = t167 * t98
      t169 = t134 * t34
      t174 = t6 ** 2
      t177 = t31 * t107
      t178 = t161 * x2
      t183 = -0.112D3 * t46 + 0.168D3 * t58 - 0.128D3 * t78 + 0.328D3 * 
     #t85 + 0.56D2 * t93 + 0.28D2 * t105 + 0.168D3 * t113 - 0.112D3 * t1
     #18 + 0.28D2 * t121 * t122 * t124 + 0.36D2 * t132 + 0.28D2 * t99 * 
     #t134 * t33 * t54 - 0.530D3 * t145 - 0.148D3 * t152 + 0.96D2 * t155
     # + 0.108D3 * t159 * t160 * t38 * t161 + 0.108D3 * t168 * t169 * t3
     #8 * x2 + 0.36D2 * t3 * t174 * t98 * t177 * t38 * t178
      t188 = t64 * t55
      t190 = t61 * t108 * t148 * t188
      t194 = t129 * t130 * x3
      t195 = t8 * t62 * t194
      t197 = t167 * t16
      t203 = 0.16D2 * t197 * t102 * t52 * x3 * t21 * t38
      t204 = t60 * t120
      t206 = t64 * x3
      t207 = t148 * t206
      t208 = t204 * t62 * t207
      t211 = t82 * x2
      t213 = t139 * t16 * t81 * t211
      t216 = t38 * t101
      t218 = t99 * t32 * t33 * t216
      t220 = t107 * t33
      t221 = t8 * t220
      t222 = t115 * t21
      t223 = t222 * t38
      t227 = t121 * t52
      t228 = t21 * t38
      t232 = t127 * t107
      t234 = t232 * t228 * t188
      t236 = t55 * x3
      t240 = 0.144D3 * t221 * t228 * t64 * t236
      t242 = t32 ** 2
      t244 = t33 * t38
      t250 = t38 * t64
      t252 = t204 * t52 * t115 * t70 * t250
      t254 = t115 * t38
      t259 = t221 * t254 * t188
      t264 = t61 * t107 * t222 * t70 * t250
      t266 = t60 * t5
      t268 = t21 * t70
      t270 = t266 * t34 * t268 * t250
      t272 = -0.144D3 * t17 * t169 * t39 + 0.312D3 * t190 - 0.144D3 * t1
     #95 + t203 - 0.880D3 * t208 - 0.328D3 * t213 + 0.36D2 * t218 - 0.14
     #4D3 * t221 * t223 * t206 - 0.144D3 * t227 * t228 * t206 + 0.216D3 
     #* t234 - t240 + 0.36D2 * t8 * t95 * t97 * t242 * t244 - 0.16D2 * t
     #252 - 0.288D3 * t232 * t254 * t206 + 0.216D3 * t259 - 0.12D2 * t26
     #4 + 0.748D3 * t270
      t274 = t50 * t31
      t277 = t61 * t274 * t142 * t44
      t282 = t74 * t42 * x1
      t283 = t139 * t108 * t70 * t282
      t285 = t52 * t115
      t290 = t8 * t285 * t131
      t295 = t115 ** 2
      t300 = t3 * t5
      t310 = t39 * x2
      t312 = t139 * t274 * t70 * t52 * t310
      t318 = t60 * t166
      t323 = t318 * t108 * t148 * t49 * t161 * t9
      t326 = t38 * t15
      t328 = t326 * t10 * t102
      t329 = t8 * t122 * t328
      t332 = t9 * t43
      t334 = t129 * t332 * x2
      t335 = t139 * t71 * t334
      t337 = x2 * x1
      t340 = t72 * t74 * t337 * z
      t341 = 0.128D3 * t340
      t342 = t38 * t55
      t344 = t51 * t160 * t342
      t350 = 0.144D3 * t8 * t87 * t177 * t38 * t236
      t351 = t61 * t16
      t354 = t351 * t134 * t70 * t244
      t356 = 0.400D3 * t277 - 0.128D3 * t283 + 0.56D2 * t127 * t285 * t1
     #24 + 0.36D2 * t290 + 0.108D3 * t227 * t254 * t64 + 0.36D2 * t221 *
     # t295 * t38 * t64 + 0.36D2 * t300 * t34 * t228 * t64 + 0.108D3 * t
     #232 * t223 * t64 - 0.584D3 * t312 + 0.28D2 * t159 * t53 * t54 * t1
     #61 + 0.47D2 * t323 + 0.28D2 * t329 + 0.94D2 * t335 - t341 + 0.216D
     #3 * t344 - t350 + 0.10D2 * t354
      t357 = t31 * t34
      t360 = t168 * t357 * t216 * x2
      t365 = t244 * t43
      t367 = t351 * t81 * t365
      t372 = t318 * t16 * t89 * t90 * t161
      t377 = t61 * t122 * t148 * t50 * t101
      t381 = t148 * t87 * t43
      t382 = t61 * t285 * t381
      t385 = t204 * t122 * t381
      t391 = t74 * z * x1 * t43
      t392 = t204 * t122 * t70 * t391
      t398 = t167 * t140 * t90 * x2 * t43 * x3
      t412 = t167 * t50
      t415 = t412 * t177 * t342 * x2
      t418 = t127 * t62 * t77
      t421 = t61 * t116 * t207
      t429 = 0.16D2 * t167 * t87 * t43 * t220 * t236 * t21 * t38
      t432 = t17 * t357 * t216 * x3
      t436 = t351 * t89 * t244 * t101
      t438 = 0.36D2 * t360 + 0.56D2 * t168 * t35 * t143 + 0.182D3 * t367
     # + 0.430D3 * t372 + 0.47D2 * t377 - 0.148D3 * t382 + 0.96D2 * t385
     # - 0.128D3 * t392 - 0.112D3 * t398 + 0.28D2 * t8 * t107 * t222 * t
     #124 - 0.288D3 * t197 * t160 * t310 - 0.144D3 * t158 * t16 * t177 *
     # t39 * t161 + 0.216D3 * t415 - 0.112D3 * t418 - 0.240D3 * t421 + t
     #429 - 0.144D3 * t432 + 0.192D3 * t436
      t445 = t60 * t70
      t447 = t445 * t6 * t52
      t448 = t447 * t112
      t451 = t445 * t120 * t34
      t452 = t451 * t77
      t472 = t445 * t7 * t107 * t74 * t55 * x2 * x1
      t475 = 0.144D3 * t46 - 0.344D3 * t58 + 0.496D3 * t78 + 0.64D2 * t4
     #48 - 0.128D3 * t452 - 0.256D3 * t85 + 0.24D2 * t93 - 0.56D2 * t105
     # - 0.344D3 * t113 + 0.144D3 * t118 - 0.72D2 * t132 + 0.136D3 * t26
     #6 * t87 * t89 * t244 + 0.64D2 * t451 * t74 * t337 + 0.64D2 * t472 
     #+ 0.444D3 * t145
      t477 = t445 * t5 * t33
      t478 = t477 * t124
      t485 = 0.32D2 * t412 * t101 * t107 * t55 * t21 * t38
      t488 = t204 * t50
      t490 = t488 * t89 * t365
      t500 = 0.64D2 * t478 + t485 + 0.216D3 * t152 - 0.640D3 * t155 - 0.
     #256D3 * t490 - 0.264D3 * t190 + 0.256D3 * t195 - t203 + 0.640D3 * 
     #t208 + 0.604D3 * t213 - 0.72D2 * t218 - 0.72D2 * t234 + t240 + 0.6
     #08D3 * t252 - 0.72D2 * t259
      t508 = t204 * t87 * t89 * t83
      t511 = t488 * t81 * t244
      t514 = t447 * t282
      t522 = t80 * t89 * t211
      t525 = -0.16D2 * t264 - 0.592D3 * t270 - 0.288D3 * t277 + 0.496D3 
     #* t283 - 0.216D3 * t508 - 0.256D3 * t511 - 0.72D2 * t290 - 0.128D3
     # * t514 + 0.544D3 * t312 - 0.54D2 * t323 - 0.56D2 * t329 - 0.108D3
     # * t335 + 0.256D3 * t340 + 0.256D3 * t522 - 0.72D2 * t344
      t540 = t350 - 0.62D2 * t354 - 0.72D2 * t360 - 0.108D3 * t367 - 0.5
     #42D3 * t372 - 0.54D2 * t377 + 0.216D3 * t382 - 0.640D3 * t385 + 0.
     #256D3 * t392 + 0.144D3 * t398 - 0.72D2 * t415 + 0.144D3 * t418 - 0
     #.64D2 * t421 - t429 + 0.256D3 * t432 - 0.30D2 * t436
      t552 = z ** 2
      t558 = t445 * t120 * t33
      t566 = 0.144D3 * t58 - 0.8D1 * t78 + 0.128D3 * t448 - 0.64D2 * t45
     #2 + 0.74320D5 * t85 + 0.32D2 * t93 + 0.144D3 * t105 + 0.144D3 * t1
     #13 - 0.384D3 * t477 * t74 * t110 * t552 + 0.288D3 * t558 * t131 + 
     #0.128D3 * t472 + 0.384D3 * t477 * t391 + 0.536D3 * t145 - 0.128D3 
     #* t478
      t580 = t485 - 0.25248D5 * t490 + 0.64D2 * t190 - 0.288D3 * t195 - 
     #t203 - 0.128D3 * t208 - 0.27920D5 * t213 - 0.128D3 * t252 + 0.64D2
     # * t264 + 0.64D2 * t270 + 0.74288D5 * t277 - 0.8D1 * t283 - 0.80D2
     # * t508 - 0.25296D5 * t511 - 0.64D2 * t514
      t590 = t445 * t6 * t34
      t607 = t268 * t38
      t611 = t7 * t52
      t635 = -0.96D2 * t445 * t166 * t107 * t129 * x3 * t161 * t9 + 0.64
     #D2 * t590 * t334 + 0.48D2 * t445 * t166 * t52 * t326 * t10 * t43 *
     # t161 - 0.384D3 * t558 * t129 * t130 * z + 0.16D2 * t318 * t220 * 
     #t55 * x2 * t87 * t607 + 0.32D2 * t445 * t611 * t129 * t161 * t9 + 
     #0.16D2 * t445 * t157 * t107 * t326 * t178 * t10 - 0.74208D5 * t312
     # + 0.4D1 * t323 + 0.144D3 * t329 + 0.8D1 * t335 - t341 + 0.25264D5
     # * t522 - 0.304D3 * t354 + 0.128D3 * t300 * t87 * t43 * t552 * z *
     # t244
      t672 = -0.192D3 * t445 * t611 * t38 * t49 * x2 * t332 * x3 - 0.291
     #28D5 * t367 + 0.28208D5 * t372 - 0.252D3 * t377 - 0.128D3 * t382 +
     # 0.256D3 * t385 - 0.512D3 * t392 + 0.48D2 * t445 * t7 * t34 * t326
     # * t10 * t101 * x2 - 0.96D2 * t590 * t194 - 0.16D2 * t60 * t157 * 
     #t220 * x3 * t161 * t50 * t607 + 0.128D3 * t421 - 0.112D3 * t445 * 
     #t6 * t33 * t328 - t429 - 0.288D3 * t432 - 0.28792D5 * t436
      rrgq2qgh64J4 = -(wd * (t183 + t272 + t356 + t438) + wd * (t475 + t
     #500 + t525 + t540) + wd * (t566 + t580 + t635 + t672)) / t1 / t70 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J5
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
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t14 = t13 * t12
      t15 = s * t4
      t17 = z + x1 * t4
      t18 = 0.1D1 / t17
      t20 = 0.1D1 - x2
      t23 = x2 * x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = x3 * t20
      t30 = Sqrt(t26 * t17 * x2 * t12)
      t32 = 0.2D1 * t25 * t30
      t33 = t12 * t20 * t17 + t23 + t32
      t38 = s - t15 * x1 * t18 * t33 - t15 * t8 * t12
      t42 = z + x1 * t20 * t4
      t43 = t42 * t18
      t45 = t7 * t10 * t14 * t38 * t43
      t47 = t3 * t5
      t49 = t12 * t38
      t51 = t47 * t9 * t49 * t43
      t53 = t2 * t1
      t54 = t6 * t4
      t55 = t53 * t54
      t56 = x1 ** 2
      t57 = t56 ** 2
      t59 = t17 ** 2
      t60 = t59 ** 2
      t62 = 0.1D1 / t60 / t17
      t65 = t26 * t17 + x2 * t12 - t32
      t66 = t65 ** 2
      t67 = t66 ** 2
      t69 = t8 * t42
      t73 = t5 * t4
      t74 = t3 * t73
      t75 = t9 * t8
      t79 = t74 * t75 * t13 * t38 * t43
      t81 = t53 * t6
      t82 = t81 * t10
      t83 = t13 * t42
      t84 = t18 * x3
      t88 = t10 * t8
      t89 = t55 * t88
      t90 = x3 ** 2
      t91 = t18 * t90
      t93 = t89 * t83 * t91
      t95 = t14 * t42
      t99 = t53 * t73
      t100 = t99 * t75
      t101 = t12 * t42
      t106 = t82 * t101 * t91
      t108 = t3 * t54
      t109 = t75 * t12
      t110 = t109 * t38
      t113 = 0.1D1 / t59 / t17
      t114 = t42 * t113
      t115 = t56 * t33
      t117 = t114 * t115 * x2
      t118 = t108 * t110 * t117
      t122 = 0.1D1 / t59
      t123 = t42 * t122
      t124 = x1 * t33
      t125 = t123 * t124
      t128 = t7 * t110
      t129 = x2 * x1
      t132 = t128 * t123 * t129 * z
      t133 = 0.128D3 * t132
      t134 = t6 * t5
      t135 = t3 * t134
      t136 = t10 * t12
      t138 = t38 * t42
      t139 = x2 ** 2
      t143 = t135 * t136 * t138 * t113 * t139 * t56
      t145 = t6 * t73
      t146 = t53 * t145
      t147 = t57 * t62
      t148 = t146 * t147
      t149 = t65 * t75
      t150 = t42 * t33
      t155 = t56 * x1
      t156 = 0.1D1 / t60
      t157 = t155 * t156
      t159 = t66 * t38
      t160 = t9 * t42
      t161 = t160 * x2
      t163 = t108 * t157 * t159 * t161
      t165 = t55 * t157
      t166 = t65 * t9
      t167 = t33 ** 2
      t168 = t42 * t167
      t171 = t165 * t166 * t168 * x3
      t173 = t7 * t157
      t174 = t65 * t38
      t177 = t173 * t174 * t69 * t167
      t179 = -0.12D2 * t45 + 0.748D3 * t51 + 0.36D2 * t55 * t57 * t62 * 
     #t67 * t69 - 0.16D2 * t79 - 0.288D3 * t82 * t83 * t84 + 0.216D3 * t
     #93 - 0.144D3 * t89 * t95 * t84 - 0.144D3 * t100 * t101 * t84 + 0.2
     #16D3 * t106 + 0.94D2 * t118 + 0.28D2 * t55 * t10 * t14 * t125 - t1
     #33 + 0.47D2 * t143 + 0.28D2 * t148 * t149 * t150 * t139 - 0.328D3 
     #* t163 - 0.144D3 * t171 + 0.192D3 * t177
      t180 = t10 * t13
      t184 = t138 * t122 * x2 * x1
      t185 = t108 * t180 * t184
      t187 = x1 * t122
      t189 = t65 * t10
      t190 = t90 * x3
      t194 = 0.144D3 * t55 * t187 * t189 * t42 * t190
      t195 = t53 * t134
      t196 = t195 * t147
      t197 = t66 * t65
      t198 = t197 * t9
      t203 = t6 ** 2
      t206 = t139 * x2
      t213 = t123 * t124 * t90
      t214 = t55 * t136 * t213
      t219 = t123 * x3 * x1 * t33
      t220 = t55 * t180 * t219
      t222 = t195 * t157
      t223 = t167 * t33
      t229 = 0.16D2 * t222 * t223 * t75 * x3 * t12 * t42
      t231 = t138 * t84
      t232 = t74 * t109 * t231
      t235 = t75 * t42
      t238 = t7 * t187 * t174 * t235 * t90
      t240 = t56 * t113
      t241 = t240 * t65
      t243 = t38 * t9
      t244 = t42 * x3
      t245 = t244 * t33
      t247 = t7 * t241 * t243 * t245
      t249 = t75 * t13
      t252 = t138 * t187 * t33
      t253 = t7 * t249 * t252
      t255 = t9 * t12
      t257 = t74 * t255 * t252
      t259 = t13 ** 2
      t264 = t53 * t5
      t275 = t55 * t240
      t276 = t66 * t75
      t277 = t42 * t90
      t279 = t275 * t276 * t277
      t281 = -0.148D3 * t185 - t194 + 0.108D3 * t196 * t198 * t42 * x2 +
     # 0.36D2 * t53 * t203 * t147 * t189 * t42 * t206 + 0.168D3 * t214 -
     # 0.112D3 * t220 + t229 - 0.880D3 * t232 + 0.56D2 * t238 + 0.400D3 
     #* t247 - 0.148D3 * t253 + 0.96D2 * t257 + 0.36D2 * t89 * t259 * t4
     #2 * t18 + 0.36D2 * t264 * t9 * t101 * t18 + 0.108D3 * t100 * t83 *
     # t18 - 0.144D3 * t165 * t198 * t244 + 0.216D3 * t279
      t284 = t81 * t109 * t219
      t288 = t196 * t166 * t168 * x2
      t294 = t56 * t167
      t295 = t114 * t294
      t296 = t81 * t255 * t295
      t302 = t55 * t249 * t295
      t304 = t244 * x2
      t309 = t42 * t156
      t311 = t309 * t155 * t223
      t312 = t55 * t255 * t311
      t319 = t195 * t240
      t322 = t319 * t189 * t277 * x2
      t326 = t114 * t294 * x3
      t327 = t55 * t109 * t326
      t329 = t55 * t147
      t332 = t329 * t66 * t8 * t168
      t341 = t329 * t65 * t8 * t42 * t223
      t343 = t7 * t240
      t344 = t160 * x3
      t346 = t343 * t159 * t344
      t349 = t7 * t180 * t231
      t357 = 0.16D2 * t195 * t187 * t33 * t88 * t190 * t12 * t42
      t358 = -0.112D3 * t284 + 0.36D2 * t288 + 0.28D2 * t99 * t255 * t12
     #5 + 0.36D2 * t296 + 0.56D2 * t81 * t249 * t125 + 0.36D2 * t302 - 0
     #.288D3 * t222 * t276 * t304 + 0.28D2 * t312 - 0.144D3 * t146 * t15
     #7 * t189 * t244 * t139 + 0.216D3 * t322 - 0.144D3 * t327 + 0.36D2 
     #* t332 + 0.28D2 * t329 * t197 * t8 * t150 + 0.28D2 * t341 + 0.328D
     #3 * t346 - 0.240D3 * t349 + t357
      t361 = t173 * t197 * t38 * t69
      t363 = t157 * t65
      t365 = t150 * x2
      t367 = t108 * t363 * t243 * t365
      t373 = t123 * z * x1 * t33
      t374 = t74 * t255 * t38 * t373
      t379 = t123 * t23 * x1
      t380 = t108 * t136 * t38 * t379
      t385 = t7 * t255 * t138 * t240 * t167
      t390 = t108 * t241 * t38 * t75 * t304
      t392 = t69 * t33
      t394 = t173 * t159 * t392
      t399 = t135 * t157 * t174 * t235 * t139
      t402 = t7 * t109 * t184
      t404 = t128 * t219
      t408 = t7 * t136 * t138 * t91
      t413 = 0.144D3 * t89 * t101 * t18 * t190
      t417 = t66 * t9
      t422 = t165 * t417 * t245
      t426 = t275 * t149 * t150 * t90
      t432 = t195 * t363 * t235 * x2 * t33 * x3
      t438 = 0.10D2 * t361 - 0.530D3 * t367 - 0.128D3 * t374 - 0.128D3 *
     # t380 + 0.47D2 * t385 - 0.584D3 * t390 + 0.182D3 * t394 + 0.430D3 
     #* t399 + 0.96D2 * t402 - 0.128D3 * t404 + 0.312D3 * t408 - t413 + 
     #0.108D3 * t82 * t95 * t18 + 0.56D2 * t196 * t417 * t365 - 0.112D3 
     #* t422 + 0.168D3 * t426 - 0.112D3 * t432 + 0.108D3 * t148 * t276 *
     # t42 * t139
      t448 = t3 * t38
      t450 = t448 * t5 * t8
      t451 = t450 * t125
      t458 = 0.32D2 * t319 * t167 * t10 * t90 * t12 * t42
      t463 = t448 * t6 * t75
      t464 = t463 * t213
      t467 = t448 * t73 * t9
      t468 = t467 * t219
      t472 = -0.16D2 * t45 - 0.592D3 * t51 + 0.608D3 * t79 - 0.72D2 * t9
     #3 - 0.72D2 * t106 - 0.108D3 * t118 + 0.64D2 * t451 + t458 + 0.256D
     #3 * t132 - 0.54D2 * t143 + 0.604D3 * t163 + 0.64D2 * t464 - 0.128D
     #3 * t468 + 0.256D3 * t171 - 0.30D2 * t177
      t479 = t448 * t54 * t10 * t123 * t90 * x2 * x1
      t481 = t463 * t379
      t484 = t343 * t174 * t161
      t495 = t74 * t187 * t174 * t344
      t498 = 0.216D3 * t185 + 0.64D2 * t479 - 0.128D3 * t481 + 0.256D3 *
     # t484 + t194 - 0.344D3 * t214 + 0.144D3 * t220 - t229 + 0.640D3 * 
     #t232 + 0.24D2 * t238 - 0.288D3 * t247 + 0.216D3 * t253 - 0.640D3 *
     # t257 - 0.216D3 * t495 - 0.72D2 * t279
      t510 = t74 * t240
      t512 = t510 * t159 * t69
      t515 = t510 * t174 * t392
      t522 = 0.144D3 * t284 - 0.72D2 * t288 - 0.72D2 * t296 - 0.72D2 * t
     #302 - 0.56D2 * t312 - 0.72D2 * t322 + 0.256D3 * t327 - 0.72D2 * t3
     #32 - 0.56D2 * t341 - 0.256D3 * t346 - 0.256D3 * t512 - 0.256D3 * t
     #515 + 0.136D3 * t47 * t187 * t174 * t69 - 0.64D2 * t349 - t357
      t540 = -0.62D2 * t361 + 0.444D3 * t367 + 0.256D3 * t374 + 0.496D3 
     #* t380 - 0.54D2 * t385 + 0.544D3 * t390 - 0.108D3 * t394 - 0.542D3
     # * t399 + 0.64D2 * t467 * t123 * t129 - 0.640D3 * t402 + 0.496D3 *
     # t404 - 0.264D3 * t408 + t413 + 0.144D3 * t422 - 0.344D3 * t426 + 
     #0.144D3 * t432
      t553 = t448 * t73 * t8
      t565 = 0.64D2 * t45 + 0.64D2 * t51 - 0.128D3 * t79 + 0.8D1 * t118 
     #- 0.112D3 * t448 * t6 * t8 * t311 - 0.384D3 * t553 * t114 * t294 *
     # z - 0.128D3 * t451 + t458 + 0.384D3 * t450 * t373 - t133 + 0.4D1 
     #* t143 - 0.27920D5 * t163 + 0.128D3 * t464 - 0.64D2 * t468
      t586 = -0.288D3 * t171 - 0.28792D5 * t177 + 0.128D3 * t479 - 0.64D
     #2 * t481 + 0.25264D5 * t484 + 0.48D2 * t448 * t54 * t9 * t309 * t1
     #55 * t167 * x2 + 0.144D3 * t214 - t229 - 0.128D3 * t232 + 0.32D2 *
     # t238 + 0.74288D5 * t247 - 0.128D3 * t253 + 0.256D3 * t257 - 0.80D
     #2 * t495 + 0.144D3 * t312
      t598 = t49 * t42
      t603 = t448 * t6 * t9
      t617 = z ** 2
      t630 = 0.48D2 * t448 * t134 * t75 * t309 * t155 * t33 * t139 + 0.1
     #6D2 * t135 * t88 * t90 * x2 * t187 * t598 - 0.96D2 * t603 * t326 -
     # 0.16D2 * t3 * t145 * t88 * x3 * t139 * t240 * t598 - 0.288D3 * t3
     #27 + 0.144D3 * t341 + 0.74320D5 * t346 + 0.128D3 * t264 * t187 * t
     #33 * t617 * z * t69 + 0.64D2 * t603 * t117 - 0.25296D5 * t512 - 0.
     #25248D5 * t515 + 0.128D3 * t349 - t357 - 0.304D3 * t361 + 0.536D3 
     #* t367
      t635 = t54 * t75
      t672 = -0.512D3 * t374 - 0.8D1 * t380 - 0.252D3 * t385 - 0.74208D5
     # * t390 - 0.192D3 * t448 * t635 * t42 * t113 * x2 * t115 * x3 + 0.
     #288D3 * t553 * t295 - 0.96D2 * t448 * t134 * t10 * t114 * x3 * t13
     #9 * t56 + 0.32D2 * t448 * t635 * t114 * t139 * t56 + 0.16D2 * t448
     # * t145 * t10 * t309 * t206 * t155 - 0.29128D5 * t394 + 0.28208D5 
     #* t399 - 0.384D3 * t450 * t123 * t124 * t617 - 0.8D1 * t404 + 0.64
     #D2 * t408 + 0.144D3 * t426
      rrgq2qgh64J5 = -(wd * (t179 + t281 + t358 + t438) + wd * (t472 + t
     #498 + t522 + t540) + wd * (t565 + t586 + t630 + t672)) / t1 / t38 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J6
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
      t10 = z + x1 * t4
      t11 = t10 ** 2
      t13 = 0.1D1 / t11 / t10
      t14 = t8 * t13
      t15 = t7 * t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t10 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t10 + x2 * t19 - t28
      t30 = s * t4
      t31 = 0.1D1 / t10
      t35 = x2 * x3
      t36 = t19 * t16 * t10 + t35 + t28
      t39 = 0.1D1 - x1
      t42 = s - t30 * x1 * t31 * t36 - t30 * t39 * t19
      t43 = t29 * t42
      t46 = z + x1 * t16 * t4
      t47 = t39 * t46
      t48 = t47 * t36
      t50 = t15 * t43 * t48
      t52 = t3 * t5
      t53 = 0.1D1 / t11
      t54 = x1 * t53
      t59 = t5 ** 2
      t60 = t3 * t59
      t62 = t39 ** 2
      t63 = t62 * t39
      t64 = t63 * t46
      t65 = x3 ** 2
      t68 = t60 * t54 * t43 * t64 * t65
      t70 = t14 * t29
      t72 = t42 * t62
      t73 = t46 * x3
      t74 = t73 * t36
      t76 = t60 * t70 * t72 * t74
      t78 = t3 * t42
      t80 = t78 * t59 * t63
      t81 = t46 * t53
      t83 = t81 * t35 * x1
      t84 = t80 * t83
      t86 = t59 * t5
      t87 = t3 * t86
      t88 = t8 * x1
      t89 = t11 ** 2
      t90 = 0.1D1 / t89
      t91 = t88 * t90
      t93 = x2 ** 2
      t96 = t87 * t91 * t43 * t64 * t93
      t98 = t59 * t4
      t99 = t3 * t98
      t100 = t91 * t29
      t102 = t46 * t36
      t105 = t99 * t100 * t72 * t102 * x2
      t107 = t62 ** 2
      t108 = t107 * t19
      t110 = t42 * t46
      t111 = t31 * t65
      t113 = t60 * t108 * t110 * t111
      t115 = t2 * t1
      t116 = t115 * t98
      t117 = t63 * t19
      t119 = t46 * t13
      t120 = t36 ** 2
      t121 = t8 * t120
      t123 = t119 * t121 * x3
      t124 = t116 * t117 * t123
      t126 = t62 * t19
      t129 = t110 * t54 * t36
      t130 = t7 * t126 * t129
      t132 = t60 * t91
      t133 = t29 ** 2
      t137 = t132 * t133 * t29 * t42 * t47
      t139 = t115 * t86
      t140 = t8 ** 2
      t143 = t140 / t89 / t10
      t145 = t29 * t62
      t146 = t46 * t120
      t151 = t116 * t91
      t160 = t81 * z * x1 * t36
      t161 = t7 * t126 * t42 * t160
      t163 = t116 * t14
      t165 = t46 * t65
      t169 = -0.256D3 * t50 + 0.136D3 * t52 * t54 * t43 * t47 + 0.24D2 *
     # t68 - 0.288D3 * t76 - 0.128D3 * t84 - 0.542D3 * t96 + 0.444D3 * t
     #105 - 0.264D3 * t113 + 0.256D3 * t124 - 0.640D3 * t130 - 0.62D2 * 
     #t137 - 0.72D2 * t139 * t143 * t145 * t146 * x2 + 0.144D3 * t151 * 
     #t133 * t62 * t74 + 0.256D3 * t161 - 0.72D2 * t163 * t133 * t63 * t
     #165
      t171 = t29 * t107
      t172 = t65 * x3
      t177 = t139 * t14
      t183 = t46 * t90
      t184 = t120 * t36
      t186 = t183 * t88 * t184
      t187 = t116 * t126 * t186
      t189 = t60 * t14
      t190 = t133 * t42
      t191 = t62 * t46
      t192 = t191 * x3
      t194 = t189 * t190 * t192
      t196 = t115 * t59
      t198 = x1 * t36
      t200 = t81 * t198 * x3
      t203 = t19 ** 2
      t204 = t107 * t203
      t207 = t110 * t31 * x3
      t208 = t60 * t204 * t207
      t213 = t60 * t126 * t110 * t14 * t120
      t215 = t63 * t203
      t217 = t60 * t215 * t129
      t220 = t132 * t190 * t48
      t223 = t81 * t198 * t65
      t224 = t80 * t223
      t227 = t78 * t6 * t62
      t228 = t227 * t200
      t231 = t78 * t5 * t39
      t233 = t231 * t81 * t198
      t235 = x2 * x1
      t244 = t78 * t98 * t107 * t81 * t65 * x2 * x1
      t246 = t191 * x2
      t248 = t189 * t43 * t246
      t250 = 0.144D3 * t116 * t54 * t171 * t46 * t172 - 0.72D2 * t177 * 
     #t171 * t165 * x2 - 0.56D2 * t187 - 0.256D3 * t194 + 0.144D3 * t196
     # * t117 * t200 - 0.64D2 * t208 - 0.54D2 * t213 + 0.216D3 * t217 - 
     #0.108D3 * t220 + 0.64D2 * t224 - 0.128D3 * t228 + 0.64D2 * t233 + 
     #0.64D2 * t227 * t81 * t235 + 0.64D2 * t244 + 0.256D3 * t248
      t254 = t7 * t54 * t43 * t192
      t263 = t119 * t121
      t269 = t107 * t39
      t270 = t116 * t269
      t271 = t19 * t46
      t287 = t46 * t31
      t289 = t60 * t107 * t203 * t19 * t42 * t287
      t292 = t19 * t42
      t294 = t52 * t62 * t292 * t287
      t299 = t7 * t63 * t203 * t42 * t287
      t304 = t110 * t53 * x2 * x1
      t307 = t117 * t42
      t308 = t60 * t307
      t309 = t308 * t200
      t312 = t8 * t36
      t314 = t119 * t312 * x2
      t315 = t99 * t307 * t314
      t319 = t308 * t81 * t235 * z
      t325 = t87 * t108 * t110 * t13 * t93 * t8
      t327 = -0.216D3 * t254 + 0.144D3 * t139 * t100 * t64 * x2 * t36 * 
     #x3 - 0.72D2 * t196 * t126 * t263 - 0.72D2 * t116 * t215 * t263 + 0
     #.144D3 * t270 * t271 * t31 * t172 - 0.72D2 * t270 * t203 * t46 * t
     #111 - 0.72D2 * t196 * t107 * t271 * t111 - 0.16D2 * t289 - 0.592D3
     # * t294 + 0.608D3 * t299 - 0.640D3 * t60 * t117 * t304 + 0.496D3 *
     # t309 - 0.108D3 * t315 + 0.256D3 * t319 - 0.54D2 * t325
      t333 = t99 * t91 * t190 * t246
      t339 = t99 * t70 * t42 * t63 * t73 * x2
      t342 = t116 * t108 * t223
      t350 = t163 * t29 * t63 * t102 * t65
      t354 = t99 * t108 * t42 * t83
      t361 = 0.32D2 * t177 * t120 * t107 * t65 * t19 * t46
      t362 = t116 * t143
      t370 = t362 * t29 * t39 * t46 * t184
      t374 = t151 * t145 * t146 * x3
      t378 = t132 * t43 * t47 * t120
      t386 = 0.16D2 * t139 * t54 * t36 * t269 * t172 * t19 * t46
      t393 = 0.16D2 * t139 * t91 * t184 * t63 * x3 * t19 * t46
      t395 = t7 * t117 * t207
      t398 = t15 * t190 * t47
      t400 = 0.216D3 * t99 * t204 * t304 + 0.604D3 * t333 + 0.544D3 * t3
     #39 - 0.344D3 * t342 + 0.144D3 * t116 * t204 * t200 - 0.344D3 * t35
     #0 + 0.496D3 * t354 + t361 - 0.72D2 * t362 * t133 * t39 * t146 - 0.
     #56D2 * t370 + 0.256D3 * t374 - 0.30D2 * t378 - t386 - t393 + 0.640
     #D3 * t395 - 0.256D3 * t398
      t421 = t98 * t63
      t431 = -0.25248D5 * t50 + 0.32D2 * t68 + 0.74288D5 * t76 - 0.64D2 
     #* t84 + 0.28208D5 * t96 + 0.536D3 * t105 + 0.64D2 * t113 - 0.288D3
     # * t124 + 0.256D3 * t130 - 0.304D3 * t137 + 0.48D2 * t78 * t98 * t
     #62 * t183 * t88 * t120 * x2 - 0.192D3 * t78 * t421 * t46 * t13 * x
     #2 * t312 * x3 - 0.512D3 * t161 + 0.144D3 * t187
      t453 = -0.96D2 * t78 * t86 * t107 * t119 * x3 * t93 * t8 + 0.74320
     #D5 * t194 + 0.128D3 * t208 - 0.252D3 * t213 - 0.128D3 * t217 - 0.2
     #9128D5 * t220 + 0.128D3 * t224 - 0.64D2 * t228 - 0.128D3 * t233 + 
     #0.128D3 * t244 + 0.25264D5 * t248 - 0.80D2 * t254 + 0.64D2 * t289 
     #+ 0.64D2 * t294 - 0.128D3 * t299
      t457 = t78 * t6 * t39
      t467 = t78 * t59 * t62
      t479 = t59 * t6
      t494 = t292 * t46
      t504 = -0.8D1 * t309 - 0.384D3 * t457 * t119 * t121 * z - 0.112D3 
     #* t78 * t59 * t39 * t186 - 0.96D2 * t467 * t123 + 0.8D1 * t315 - 0
     #.128D3 * t319 + 0.4D1 * t325 - 0.27920D5 * t333 + 0.32D2 * t78 * t
     #421 * t119 * t93 * t8 + 0.16D2 * t78 * t479 * t107 * t183 * t93 * 
     #x2 * t88 - 0.74208D5 * t339 + 0.144D3 * t342 + 0.144D3 * t350 - 0.
     #16D2 * t3 * t479 * t269 * x3 * t93 * t14 * t494 + 0.16D2 * t87 * t
     #269 * t65 * x2 * t54 * t494
      t510 = z ** 2
      t536 = -0.8D1 * t354 + 0.288D3 * t457 * t263 + t361 + 0.384D3 * t2
     #31 * t160 - 0.384D3 * t231 * t81 * t198 * t510 + 0.144D3 * t370 - 
     #0.288D3 * t374 - 0.28792D5 * t378 - t386 - t393 - 0.128D3 * t395 -
     # 0.25296D5 * t398 + 0.128D3 * t115 * t5 * t54 * t36 * t510 * z * t
     #47 + 0.64D2 * t467 * t314 + 0.48D2 * t78 * t86 * t63 * t183 * t88 
     #* t36 * t93
      rrgq2qgh64J6 = -(wd * (t169 + t250 + t327 + t400) + wd * (t431 + t
     #453 + t504 + t536)) / t1 / t42 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh64J7
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
      t11 = 0.1D1 - x3
      t12 = t10 * t11
      t14 = s * t4
      t16 = z + x1 * t4
      t17 = 0.1D1 / t16
      t19 = 0.1D1 - x2
      t22 = x2 * x3
      t24 = cos(x4 * 0.3141592653589793D1)
      t25 = x3 * t19
      t29 = Sqrt(t25 * t16 * x2 * t11)
      t31 = 0.2D1 * t24 * t29
      t32 = t11 * t19 * t16 + t22 + t31
      t37 = s - t14 * x1 * t17 * t32 - t14 * t8 * t11
      t40 = z + x1 * t19 * t4
      t41 = t37 * t40
      t43 = t41 * t17 * x3
      t46 = t5 ** 2
      t47 = t46 * t4
      t48 = t3 * t47
      t49 = t12 * t37
      t51 = t16 ** 2
      t53 = 0.1D1 / t51 / t16
      t54 = t40 * t53
      t55 = x1 ** 2
      t56 = t55 * t32
      t58 = t54 * t56 * x2
      t61 = t3 * t46
      t62 = t61 * t49
      t63 = 0.1D1 / t51
      t64 = t40 * t63
      t70 = t11 ** 2
      t73 = t63 * x1
      t75 = t41 * t73 * t32
      t78 = t9 * t11
      t82 = t3 * t37
      t84 = t82 * t5 * t8
      t85 = x1 * t32
      t87 = t64 * t85 * z
      t90 = z ** 2
      t95 = t2 * t1
      t96 = t95 * t47
      t97 = t55 * x1
      t98 = t51 ** 2
      t99 = 0.1D1 / t98
      t100 = t97 * t99
      t104 = t25 * t16 + x2 * t11 - t31
      t106 = t32 ** 2
      t114 = t40 * t99
      t120 = t46 * t5
      t121 = t3 * t120
      t122 = t9 ** 2
      t123 = t122 * t11
      t125 = x2 ** 2
      t134 = t95 * t120
      t135 = t55 * t53
      t138 = x3 ** 2
      t148 = t8 * t40
      t152 = t61 * t100
      t153 = t104 * t37
      t158 = -0.128D3 * t7 * t12 * t43 + 0.8D1 * t48 * t49 * t58 - 0.128
     #D3 * t62 * t64 * x2 * x1 * z - 0.128D3 * t61 * t10 * t70 * t75 + 0
     #.256D3 * t7 * t78 * t75 + 0.384D3 * t84 * t87 - 0.384D3 * t84 * t6
     #4 * t85 * t90 - 0.288D3 * t96 * t100 * t104 * t9 * t40 * t106 * x3
     # + 0.48D2 * t82 * t47 * t9 * t114 * t97 * t106 * x2 + 0.4D1 * t121
     # * t123 * t41 * t53 * t125 * t55 - 0.128D3 * t84 * t64 * t85 + 0.3
     #2D2 * t134 * t135 * t106 * t122 * t138 * t11 * t40 + 0.128D3 * t95
     # * t5 * t73 * t32 * t90 * z * t148 - 0.28792D5 * t152 * t153 * t14
     #8 * t106
      t159 = t47 * t10
      t165 = t46 * t6
      t182 = t106 * t32
      t184 = t114 * t97 * t182
      t188 = t82 * t46 * t10
      t190 = t64 * t85 * t138
      t197 = t64 * x3 * x1 * t32
      t201 = t64 * t22 * x1
      t205 = t104 ** 2
      t206 = t205 * t37
      t207 = t9 * t40
      t208 = t207 * x2
      t212 = t55 ** 2
      t222 = t61 * t135
      t223 = t207 * x3
      t241 = t7 * t135
      t245 = 0.32D2 * t82 * t159 * t54 * t125 * t55 + 0.16D2 * t82 * t16
     #5 * t122 * t114 * t125 * x2 * t97 - 0.192D3 * t82 * t159 * t40 * t
     #53 * x2 * t56 * x3 - 0.112D3 * t82 * t46 * t8 * t184 + 0.128D3 * t
     #188 * t190 - 0.64D2 * t82 * t6 * t9 * t197 - 0.64D2 * t188 * t201 
     #- 0.27920D5 * t48 * t100 * t206 * t208 + 0.144D3 * t96 * t212 / t9
     #8 / t16 * t104 * t8 * t40 * t182 + 0.74320D5 * t222 * t206 * t223 
     #- 0.512D3 * t7 * t78 * t37 * t87 + 0.144D3 * t96 * t78 * t184 + 0.
     #25264D5 * t222 * t153 * t208 - 0.80D2 * t7 * t73 * t153 * t223 - 0
     #.25296D5 * t241 * t206 * t148
      t248 = t82 * t46 * t9
      t249 = t55 * t106
      t251 = t54 * t249 * x3
      t255 = t122 * t8
      t259 = t11 * t37
      t260 = t259 * t40
      t270 = t148 * t32
      t275 = t10 * t40
      t281 = t82 * t6 * t8
      t321 = t37 * t9
      t322 = t40 * t32
      t338 = -0.96D2 * t248 * t251 - 0.16D2 * t3 * t165 * t255 * x3 * t1
     #25 * t135 * t260 + 0.16D2 * t121 * t255 * t138 * x2 * t73 * t260 -
     # 0.29128D5 * t152 * t206 * t270 + 0.28208D5 * t121 * t100 * t153 *
     # t275 * t125 + 0.288D3 * t281 * t54 * t249 - 0.96D2 * t82 * t120 *
     # t122 * t54 * x3 * t125 * t55 - 0.16D2 * t134 * t100 * t182 * t10 
     #* x3 * t11 * t40 - 0.304D3 * t152 * t205 * t104 * t37 * t148 - 0.2
     #5248D5 * t241 * t153 * t270 + 0.128D3 * t82 * t47 * t122 * t64 * t
     #138 * x2 * x1 + 0.32D2 * t61 * t73 * t153 * t275 * t138 + 0.536D3 
     #* t48 * t100 * t104 * t321 * t322 * x2 + 0.48D2 * t82 * t120 * t10
     # * t114 * t97 * t32 * t125 - 0.384D3 * t281 * t54 * t249 * z
      t360 = t40 * t17
      t383 = t135 * t104
      t386 = t40 * x3
      t411 = 0.144D3 * t96 * t135 * t104 * t10 * t322 * t138 - 0.8D1 * t
     #62 * t197 + 0.64D2 * t61 * t123 * t41 * t17 * t138 - 0.288D3 * t96
     # * t12 * t251 + 0.64D2 * t248 * t58 + 0.64D2 * t61 * t122 * t70 * 
     #t11 * t37 * t360 + 0.64D2 * t3 * t5 * t9 * t259 * t360 - 0.128D3 *
     # t7 * t10 * t70 * t37 * t360 - 0.8D1 * t48 * t123 * t37 * t201 - 0
     #.252D3 * t61 * t78 * t41 * t135 * t106 - 0.74208D5 * t48 * t383 * 
     #t37 * t10 * t386 * x2 + 0.144D3 * t96 * t123 * t190 + 0.128D3 * t6
     #1 * t122 * t70 * t43 - 0.16D2 * t134 * t73 * t32 * t255 * t138 * x
     #3 * t11 * t40 + 0.74288D5 * t61 * t383 * t321 * t386 * t32
      rrgq2qgh64J7 = -wd * (t158 + t245 + t338 + t411) / t1 / t37 / z / 
     #0.3141592653589793D1 / 0.36D2

      end function
  
 