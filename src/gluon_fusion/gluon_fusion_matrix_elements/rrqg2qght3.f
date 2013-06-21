  
      subroutine rrqg2qght3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh31J1  
      doubleprecision rrqg2qgh31J2  
      doubleprecision rrqg2qgh31J3  
      doubleprecision rrqg2qgh31J4  
      doubleprecision rrqg2qgh31J5  
      doubleprecision rrqg2qgh31J6  
      doubleprecision rrqg2qgh32J1  
      doubleprecision rrqg2qgh32J2  
      doubleprecision rrqg2qgh32J3  
      doubleprecision rrqg2qgh32J4  
      doubleprecision rrqg2qgh32J5  
      doubleprecision rrqg2qgh32J6  
      doubleprecision rrqg2qgh32J7  
      doubleprecision rrqg2qght3s1e1  
      doubleprecision rrqg2qght3s1e0  
      doubleprecision rrqg2qght3s1em1  
      doubleprecision rrqg2qght3s1em2  
      doubleprecision rrqg2qght3s1em3  
      doubleprecision rrqg2qght3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh31J1
      doubleprecision rrqg2qgh31J2
      doubleprecision rrqg2qgh31J3
      doubleprecision rrqg2qgh31J4
      doubleprecision rrqg2qgh31J5
      doubleprecision rrqg2qgh31J6
      doubleprecision rrqg2qgh32J1
      doubleprecision rrqg2qgh32J2
      doubleprecision rrqg2qgh32J3
      doubleprecision rrqg2qgh32J4
      doubleprecision rrqg2qgh32J5
      doubleprecision rrqg2qgh32J6
      doubleprecision rrqg2qgh32J7
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
      t20 = rrqg2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t40 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t41 = t18 * t40
      t43 = t38 ** 2
      t44 = t43 * t9
      t45 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t46 = t18 * t45
      t53 = 0.3141592653589793D1 * lh
      t54 = t3 * t16
      t55 = t19 * t40
      t61 = lh ** 2
      t63 = 0.3141592653589793D1 ** 2
      t65 = 0.180D3 * t61 - 0.30D2 * t63
      t66 = 0.3141592653589793D1 * t65
      t67 = t66 * t3
      t68 = t16 * t9
      t69 = t68 * t46
      t70 = t67 * t69
      t72 = 0.1D1 / x4
      t78 = log(0.4D1 * t29 * t32 * t33)
      t79 = t78 * 0.3141592653589793D1
      t82 = t78 ** 2
      t83 = t82 * 0.3141592653589793D1
      t87 = (t66 + 0.180D3 * t79 * lh + 0.45D2 * t83) * t3 * t16
      t90 = t15 * t16
      t91 = rrqg2qgh31J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t109 = (0.3141592653589793D1 * (0.60D2 * lh * t63 - 0.288493656758
     #3026D3 - 0.120D3 * t61 * lh) - t79 * t65 - 0.90D2 * t83 * lh - 0.1
     #5D2 * t82 * t78 * 0.3141592653589793D1) * t3 * t16
      t117 = (-0.180D3 * t53 - 0.90D2 * t79) * t3 * t16
      t120 = x3 * t24
      t121 = t26 * t28
      t122 = t120 * t121
      t125 = log(0.4D1 * t122 * t35)
      t126 = t125 * t9
      t132 = t53 * t3
      t136 = 0.1D1 / x3
      t141 = t28 * t30
      t146 = log(0.4D1 * t120 * t26 * t141 * t31 * t33)
      t147 = t146 ** 2
      t148 = t147 * t9
      t151 = t146 * t9
      t165 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41 - t44 * t46 / 0.2D
     #1) - 0.180D3 * t53 * t54 * (-t55 + t39 * t46) - t70) * t72 / 0.720
     #D3 + t87 * t55 / 0.720D3 + t90 * t19 * t91 / 0.8D1 + t109 * t19 * 
     #t45 / 0.720D3 + t117 * t21 / 0.720D3 - (0.90D2 * t15 * t16 * (-t55
     # + t126 * t46) + 0.180D3 * t132 * t69) * t136 * t72 / 0.720D3 - (0
     #.90D2 * t15 * t16 * (-t21 - t148 * t46 / 0.2D1 + t151 * t41) - 0.1
     #80D3 * t53 * t54 * (t151 * t46 - t55) - t70) * t136 / 0.720D3
      t166 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t165)
      t168 = 0.1D1 - x4
      t169 = KAPPA2(x1, x2, 0.0D0, t168, z)
      t170 = s * t169
      t171 = t170 * t4
      t172 = -t168
      t173 = t7 * t172
      t174 = t170 * t173
      t175 = t7 * x4
      t176 = t170 * t175
      t177 = t169 ** 2
      t180 = x1 * t6
      t182 = s * t177 * t11 * t180 * t172
      t184 = 0.1D1 / (-0.2D1 + t169)
      t185 = t177 * t184
      t186 = rrqg2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t190 = t177 ** 2
      t195 = log(-0.4D1 * t27 * t141 * t31 * x4 * t172 * t190)
      t196 = t195 ** 2
      t197 = t196 * t177
      t198 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t199 = t184 * t198
      t202 = t195 * t177
      t203 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t211 = t185 * t203
      t216 = t16 * t177
      t217 = t216 * t199
      t221 = x4 * t172
      t226 = log(-0.4D1 * t122 * t32 * t221 * t190)
      t227 = t226 * t177
      t239 = -(0.90D2 * t15 * t16 * (t185 * t186 + t197 * t199 / 0.2D1 -
     # t202 * t184 * t203) - 0.180D3 * t53 * t54 * (-t202 * t199 + t211)
     # + t67 * t217) * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t227 * t1
     #99 + t211) - 0.180D3 * t132 * t217) * t136 * t72 / 0.720D3
      t240 = FJET(XB1, XB2, s, 0.0D0, t171, t174, -t176, t182, t239)
      t242 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t243 = t18 * t242
      t245 = rrqg2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t246 = t19 * t245
      t247 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t248 = t18 * t247
      t256 = t19 * t242
      t261 = t68 * t248
      t262 = t67 * t261
      t268 = rrqg2qgh32J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t303 = -(0.90D2 * t15 * t16 * (t39 * t243 - t246 - t44 * t248 / 0.
     #2D1) - 0.180D3 * t53 * t54 * (t39 * t248 - t256) - t262) * t72 / 0
     #.720D3 + t87 * t256 / 0.720D3 + t90 * t19 * t268 / 0.8D1 + t109 * 
     #t19 * t247 / 0.720D3 + t117 * t246 / 0.720D3 - (0.90D2 * t15 * t16
     # * (-t256 + t126 * t248) + 0.180D3 * t132 * t261) * t136 * t72 / 0
     #.720D3 - (0.90D2 * t15 * t16 * (-t148 * t248 / 0.2D1 - t246 + t151
     # * t243) - 0.180D3 * t53 * t54 * (-t256 + t151 * t248) - t262) * t
     #136 / 0.720D3
      t304 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t303)
      t306 = rrqg2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t308 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t311 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t312 = t184 * t311
      t319 = t185 * t308
      t325 = t216 * t312
      t340 = -(0.90D2 * t15 * t16 * (t185 * t306 - t202 * t184 * t308 + 
     #t197 * t312 / 0.2D1) - 0.180D3 * t53 * t54 * (t319 - t202 * t312) 
     #+ t67 * t325) * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t227 * t31
     #2 + t319) - 0.180D3 * t132 * t325) * t136 * t72 / 0.720D3
      t341 = FJET(XB1, XB2, s, t171, 0.0D0, -t176, t174, t182, t340)
      t343 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t344 = s * t343
      t345 = t4 * x3
      t346 = t344 * t345
      t347 = -0.1D1 + x3
      t348 = t4 * t347
      t349 = t344 * t348
      t350 = t344 * t7
      t351 = t343 ** 2
      t355 = s * t351 * t11 * t180 * t347
      t357 = t351 ** 2
      t362 = log(-0.4D1 * t122 * t32 * t347 * x4 * t357)
      t363 = t362 * t351
      t365 = 0.1D1 / (-0.2D1 + t343)
      t366 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t367 = t365 * t366
      t369 = t351 * t365
      t370 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t371 = t369 * t370
      t376 = t16 * t351
      t377 = t376 * t367
      t383 = rrqg2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t389 = log(-0.4D1 * t122 * t32 * t347 * t357)
      t390 = t389 ** 2
      t391 = t390 * t351
      t394 = t389 * t351
      t410 = -(0.90D2 * t15 * t16 * (-t363 * t367 + t371) - 0.180D3 * t1
     #32 * t377) * t136 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t369 * 
     #t383 + t391 * t367 / 0.2D1 - t394 * t365 * t370) - 0.180D3 * t53 *
     # t54 * (-t394 * t367 + t371) + t67 * t377) * t136 / 0.720D3
      t411 = FJET(XB1, XB2, s, t346, -t349, -t350, 0.0D0, t355, t410)
      t413 = KAPPA2(x1, x2, x3, t168, z)
      t414 = s * t413
      t415 = t414 * t345
      t416 = t414 * t348
      t417 = t414 * t173
      t418 = t414 * t175
      t419 = t413 ** 2
      t424 = cos(t22)
      t427 = Sqrt(x3 * t347 * t221)
      t432 = s * t419 * t11 * t180 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t424 * t427)
      t436 = t419 ** 2
      t441 = log(0.4D1 * t120 * t121 * t30 * t31 * t347 * t221 * t436)
      t442 = t441 * t419
      t444 = 0.1D1 / (-0.2D1 + t413)
      t445 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t446 = t444 * t445
      t448 = t419 * t444
      t449 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t455 = t16 * t419
      t459 = 0.90D2 * t15 * t16 * (t442 * t446 - t448 * t449) + 0.180D3 
     #* t132 * t455 * t446
      t463 = FJET(XB1, XB2, s, t415, -t416, t417, -t418, t432, -t459 * t
     #136 * t72 / 0.720D3)
      t465 = t136 * t72
      t468 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t469 = t369 * t468
      t470 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t471 = t365 * t470
      t477 = t376 * t471
      t483 = rrqg2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t502 = -(0.90D2 * t15 * t16 * (t469 - t363 * t471) - 0.180D3 * t13
     #2 * t477) * t136 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t369 * t
     #483 + t391 * t471 / 0.2D1 - t394 * t365 * t468) - 0.180D3 * t53 * 
     #t54 * (-t394 * t471 + t469) + t67 * t477) * t136 / 0.720D3
      t503 = FJET(XB1, XB2, s, -t349, t346, 0.0D0, -t350, t355, t502)
      t505 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t507 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t508 = t444 * t507
      t517 = 0.90D2 * t15 * t16 * (-t448 * t505 + t442 * t508) + 0.180D3
     # * t132 * t455 * t508
      t521 = FJET(XB1, XB2, s, -t416, t415, -t418, t417, t432, -t517 * t
     #136 * t72 / 0.720D3)
      rrqg2qght3s1e1 = t166 * t165 + t240 * t239 + t304 * t303 + t341 * 
     #t340 + t411 * t410 - t463 * t459 * t465 / 0.720D3 + t503 * t502 - 
     #t521 * t517 * t465 / 0.720D3

      end function



      doubleprecision function rrqg2qght3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh31J1
      doubleprecision rrqg2qgh31J2
      doubleprecision rrqg2qgh31J3
      doubleprecision rrqg2qgh31J4
      doubleprecision rrqg2qgh31J5
      doubleprecision rrqg2qgh31J6
      doubleprecision rrqg2qgh32J1
      doubleprecision rrqg2qgh32J2
      doubleprecision rrqg2qgh32J3
      doubleprecision rrqg2qgh32J4
      doubleprecision rrqg2qgh32J5
      doubleprecision rrqg2qgh32J6
      doubleprecision rrqg2qgh32J7
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
      t20 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
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
      t39 = t38 * t9
      t40 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t41 = t18 * t40
      t47 = 0.3141592653589793D1 * lh
      t48 = t47 * t3
      t49 = t16 * t9
      t52 = 0.180D3 * t48 * t49 * t41
      t54 = 0.1D1 / x4
      t57 = t15 * t49
      t58 = 0.1D1 / x3
      t59 = t58 * t54
      t63 = x3 * t24
      t65 = t28 * t30
      t70 = log(0.4D1 * t63 * t26 * t65 * t31 * t33)
      t71 = t70 * t9
      t80 = t15 * t16
      t81 = rrqg2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t89 = log(0.4D1 * t29 * t32 * t33)
      t90 = t89 * 0.3141592653589793D1
      t94 = (-0.180D3 * t47 - 0.90D2 * t90) * t3 * t16
      t97 = lh ** 2
      t99 = 0.3141592653589793D1 ** 2
      t105 = t89 ** 2
      t110 = (0.3141592653589793D1 * (0.180D3 * t97 - 0.30D2 * t99) + 0.
     #180D3 * t90 * lh + 0.45D2 * t105 * 0.3141592653589793D1) * t3 * t1
     #6
      t114 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41) + t52) * t54 / 0.
     #720D3 + t57 * t41 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (t71 * t41
     # - t21) + t52) * t58 / 0.720D3 + t80 * t19 * t81 / 0.8D1 + t94 * t
     #21 / 0.720D3 + t110 * t19 * t40 / 0.720D3
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
      t132 = t16 * t126
      t133 = t15 * t132
      t135 = 0.1D1 / (-0.2D1 + t118)
      t136 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t137 = t135 * t136
      t143 = t126 ** 2
      t148 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t143)
      t149 = t148 * t126
      t151 = t126 * t135
      t152 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t164 = -t133 * t137 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t149 *
     # t137 + t151 * t152) - 0.180D3 * t48 * t132 * t137) * t54 / 0.720D
     #3
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t168 = t18 * t167
      t170 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t171 = t19 * t170
      t178 = 0.180D3 * t48 * t49 * t168
      t195 = rrqg2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t202 = -(0.90D2 * t15 * t16 * (t39 * t168 - t171) + t178) * t54 / 
     #0.720D3 + t94 * t171 / 0.720D3 + t57 * t168 * t59 / 0.8D1 - (0.90D
     #2 * t15 * t16 * (-t171 + t71 * t168) + t178) * t58 / 0.720D3 + t80
     # * t19 * t195 / 0.8D1 + t110 * t19 * t167 / 0.720D3
      t203 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t202)
      t205 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t207 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t208 = t135 * t207
      t223 = -(0.90D2 * t15 * t16 * (t151 * t205 - t149 * t208) - 0.180D
     #3 * t48 * t132 * t208) * t54 / 0.720D3 - t133 * t208 * t59 / 0.8D1
      t224 = FJET(XB1, XB2, s, t120, 0.0D0, -t125, t123, t131, t223)
      t226 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t227 = s * t226
      t228 = t4 * x3
      t229 = t227 * t228
      t230 = -0.1D1 + x3
      t231 = t4 * t230
      t232 = t227 * t231
      t233 = t227 * t7
      t234 = t226 ** 2
      t238 = s * t234 * t11 * t129 * t230
      t239 = t16 * t234
      t240 = t15 * t239
      t242 = 0.1D1 / (-0.2D1 + t226)
      t243 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t244 = t242 * t243
      t250 = t234 ** 2
      t255 = log(-0.4D1 * t63 * t26 * t28 * t32 * t230 * t250)
      t256 = t255 * t234
      t258 = t234 * t242
      t259 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t271 = -t240 * t244 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t256 *
     # t244 + t258 * t259) - 0.180D3 * t48 * t239 * t244) * t58 / 0.720D
     #3
      t272 = FJET(XB1, XB2, s, t229, -t232, -t233, 0.0D0, t238, t271)
      t274 = KAPPA2(x1, x2, x3, t117, z)
      t275 = s * t274
      t276 = t275 * t228
      t277 = t275 * t231
      t278 = t275 * t122
      t279 = t275 * t124
      t280 = t274 ** 2
      t285 = cos(t22)
      t289 = Sqrt(x3 * t230 * x4 * t121)
      t294 = s * t280 * t11 * t129 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t285 * t289)
      t296 = t15 * t16 * t280
      t298 = 0.1D1 / (-0.2D1 + t274)
      t299 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t117)
      t304 = FJET(XB1, XB2, s, t276, -t277, t278, -t279, t294, t296 * t2
     #98 * t299 * t59 / 0.8D1)
      t306 = t3 * t16
      t308 = t280 * t298
      t314 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t315 = t242 * t314
      t320 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t332 = -t240 * t315 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t256 *
     # t315 + t258 * t320) - 0.180D3 * t48 * t239 * t315) * t58 / 0.720D
     #3
      t333 = FJET(XB1, XB2, s, -t232, t229, 0.0D0, -t233, t238, t332)
      t335 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t117)
      t340 = FJET(XB1, XB2, s, -t277, t276, -t279, t278, t294, t296 * t2
     #98 * t335 * t59 / 0.8D1)
      rrqg2qght3s1e0 = t115 * t114 + t165 * t164 + t203 * t202 + t224 * 
     #t223 + t272 * t271 + t304 * 0.3141592653589793D1 * t306 * t308 * t
     #299 * t58 * t54 / 0.8D1 + t333 * t332 + t340 * 0.3141592653589793D
     #1 * t306 * t308 * t335 * t58 * t54 / 0.8D1

      end function



      doubleprecision function rrqg2qght3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh31J1
      doubleprecision rrqg2qgh31J2
      doubleprecision rrqg2qgh31J3
      doubleprecision rrqg2qgh31J4
      doubleprecision rrqg2qgh31J5
      doubleprecision rrqg2qgh31J6
      doubleprecision rrqg2qgh32J1
      doubleprecision rrqg2qgh32J2
      doubleprecision rrqg2qgh32J3
      doubleprecision rrqg2qgh32J4
      doubleprecision rrqg2qgh32J5
      doubleprecision rrqg2qgh32J6
      doubleprecision rrqg2qgh32J7
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
      t21 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t22 = 0.1D1 / x3
      t27 = rrqg2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t34 = sin(x2 * 0.3141592653589793D1)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t44 = t9 ** 2
      t48 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t44)
      t53 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t48 * 0.314
     #1592653589793D1) * t3 * t16
      t57 = 0.1D1 / x4
      t62 = t17 * t20 * t21 * t22 / 0.8D1 + t17 * t20 * t27 / 0.8D1 + t5
     #3 * t20 * t21 / 0.720D3 + t17 * t20 * t21 * t57 / 0.8D1
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
      t82 = t74 / (-0.2D1 + t66)
      t83 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t65)
      t85 = t82 * t83 * t57
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t17 * t85 / 0
     #.8D1)
      t90 = t3 * t16
      t94 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t99 = rrqg2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t110 = t17 * t20 * t94 * t22 / 0.8D1 + t17 * t20 * t99 / 0.8D1 + t
     #53 * t20 * t94 / 0.720D3 + t17 * t20 * t94 * t57 / 0.8D1
      t111 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t113 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t65
     #)
      t115 = t82 * t113 * t57
      t118 = FJET(XB1, XB2, s, t68, 0.0D0, -t73, t71, t79, -t17 * t115 /
     # 0.8D1)
      t123 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t124 = s * t123
      t126 = t124 * t4 * x3
      t127 = -0.1D1 + x3
      t129 = t124 * t4 * t127
      t130 = t124 * t7
      t131 = t123 ** 2
      t135 = s * t131 * t11 * t77 * t127
      t138 = t131 / (-0.2D1 + t123)
      t139 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t141 = t138 * t139 * t22
      t144 = FJET(XB1, XB2, s, t126, -t129, -t130, 0.0D0, t135, -t17 * t
     #141 / 0.8D1)
      t149 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t151 = t138 * t149 * t22
      t154 = FJET(XB1, XB2, s, -t129, t126, 0.0D0, -t130, t135, -t17 * t
     #151 / 0.8D1)
      rrqg2qght3s1em1 = t63 * t62 - t88 * 0.3141592653589793D1 * t90 * t
     #85 / 0.8D1 + t111 * t110 - t118 * 0.3141592653589793D1 * t90 * t11
     #5 / 0.8D1 - t144 * 0.3141592653589793D1 * t90 * t141 / 0.8D1 - t15
     #4 * 0.3141592653589793D1 * t90 * t151 / 0.8D1

      end function



      doubleprecision function rrqg2qght3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh31J1
      doubleprecision rrqg2qgh31J2
      doubleprecision rrqg2qgh31J3
      doubleprecision rrqg2qgh31J4
      doubleprecision rrqg2qgh31J5
      doubleprecision rrqg2qgh31J6
      doubleprecision rrqg2qgh32J1
      doubleprecision rrqg2qgh32J2
      doubleprecision rrqg2qgh32J3
      doubleprecision rrqg2qgh32J4
      doubleprecision rrqg2qgh32J5
      doubleprecision rrqg2qgh32J6
      doubleprecision rrqg2qgh32J7
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
      t17 = 0.3141592653589793D1 * t3 * t16
      t19 = 0.1D1 / (-0.2D1 + t1)
      t20 = t9 * t19
      t21 = rrqg2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t17 * t20 * t
     #21 / 0.8D1)
      t28 = t16 * t9
      t32 = rrqg2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t36 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t17 * t20 * t
     #32 / 0.8D1)
      rrqg2qght3s1em2 = t25 * 0.3141592653589793D1 * t3 * t28 * t19 * t2
     #1 / 0.8D1 + t36 * 0.3141592653589793D1 * t3 * t28 * t19 * t32 / 0.
     #8D1

      end function



      doubleprecision function rrqg2qght3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh31J1
      doubleprecision rrqg2qgh31J2
      doubleprecision rrqg2qgh31J3
      doubleprecision rrqg2qgh31J4
      doubleprecision rrqg2qgh31J5
      doubleprecision rrqg2qgh31J6
      doubleprecision rrqg2qgh32J1
      doubleprecision rrqg2qgh32J2
      doubleprecision rrqg2qgh32J3
      doubleprecision rrqg2qgh32J4
      doubleprecision rrqg2qgh32J5
      doubleprecision rrqg2qgh32J6
      doubleprecision rrqg2qgh32J7
      rrqg2qght3s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh31J1
      doubleprecision rrqg2qgh31J2
      doubleprecision rrqg2qgh31J3
      doubleprecision rrqg2qgh31J4
      doubleprecision rrqg2qgh31J5
      doubleprecision rrqg2qgh31J6
      doubleprecision rrqg2qgh32J1
      doubleprecision rrqg2qgh32J2
      doubleprecision rrqg2qgh32J3
      doubleprecision rrqg2qgh32J4
      doubleprecision rrqg2qgh32J5
      doubleprecision rrqg2qgh32J6
      doubleprecision rrqg2qgh32J7
      rrqg2qght3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh31J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 * t3
      t6 = 0.1D1 - z
      t7 = x1 * t6
      t8 = 0.1D1 - x3
      t9 = t8 * t7
      t12 = 0.1D1 - x1
      t14 = t6 * t12 * x4
      t18 = s * t4
      t22 = s - t18 * t7 * x3 - t9 * t18
      t23 = s * t1
      t24 = t22 * t23
      t26 = t4 ** 2
      t27 = t26 ** 2
      t30 = t6 ** 2
      t31 = t30 ** 2
      t33 = t12 ** 2
      t34 = t33 * t12
      t38 = x1 ** 2
      t42 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t8 * x4 * (0.1D1 - x4))
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t42 * t47
      t51 = t50 ** 2
      t56 = t3 * t26
      t58 = t12 * t50
      t59 = t30 * x1 * t58
      t62 = t24 * t4
      t65 = t26 * t4
      t66 = t30 * t6
      t67 = t65 * t66
      t68 = t24 * t67
      t70 = t38 * t8 * t58
      t73 = t26 * t30
      t77 = t12 * x4 * x1 * t8
      t86 = t24 * t26
      t93 = t8 ** 2
      t94 = t30 * t38 * t93
      t99 = x4 ** 2
      t109 = t30 * t33 * t99
      t115 = t31 * t38 * t33 * t51
      t127 = -0.90D2 * t86 * t59 + 0.18D2 * t3 * t73 * t77 - 0.9D1 * t56
     # * t94 + 0.9D1 * t23 * t65 * t66 * t34 * t99 * x4 * t22 + 0.18D2 *
     # t3 * t67 * t70 - 0.27D2 * t86 * t109 - 0.27D2 * t24 * t27 * t115 
     #- 0.27D2 * t86 * t94 + 0.54D2 * t62 * t14 - 0.18D2 * t56 * t109 - 
     #0.18D2 * t3 * t27 * t115
      rrqg2qgh31J1 = 0.4D1 / 0.9D1 * wd * (-0.18D2 * t5 * t9 + 0.18D2 * 
     #t5 * t14 - 0.9D1 * t3 - 0.27D2 * t24 + 0.9D1 * t23 * t27 * t4 * t3
     #1 * t6 * t34 * x4 * t22 * t38 * t51 + 0.18D2 * t56 * t59 + 0.54D2 
     #* t62 * t9 + 0.54D2 * t68 * t70 - 0.90D2 * t24 * t73 * t77 + 0.54D
     #2 * t68 * t33 * x4 * x1 * t50 + t127) / t22 / t1 / z / 0.314159265
     #3589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh31J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 * t3
      t6 = 0.1D1 - z
      t7 = x1 * t6
      t8 = 0.1D1 - x3
      t9 = t8 * t7
      t10 = t5 * t9
      t12 = 0.1D1 - x1
      t14 = t6 * t12 * x4
      t16 = 0.18D2 * t5 * t14
      t18 = s * t4
      t22 = s - t18 * t7 * x3 - t9 * t18
      t23 = s * t1
      t24 = t22 * t23
      t26 = t4 ** 2
      t27 = t26 ** 2
      t30 = t6 ** 2
      t31 = t30 ** 2
      t33 = t12 ** 2
      t34 = t33 * t12
      t37 = x4 * t22
      t38 = x1 ** 2
      t42 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t8 * x4 * (0.1D1 - x4))
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t42 * t47
      t51 = t50 ** 2
      t55 = 0.9D1 * t23 * t27 * t4 * t31 * t6 * t34 * t37 * t38 * t51
      t56 = t3 * t26
      t58 = t12 * t50
      t59 = t30 * x1 * t58
      t61 = 0.18D2 * t56 * t59
      t62 = t24 * t4
      t63 = t62 * t9
      t65 = t26 * t4
      t66 = t30 * t6
      t67 = t65 * t66
      t68 = t24 * t67
      t69 = t38 * t8
      t70 = t69 * t58
      t71 = t68 * t70
      t73 = t26 * t30
      t76 = t8 * x1
      t77 = t12 * x4 * t76
      t78 = t24 * t73 * t77
      t81 = x1 * t50
      t82 = t33 * x4 * t81
      t83 = t68 * t82
      t86 = t24 * t26
      t87 = t86 * t59
      t91 = 0.18D2 * t3 * t73 * t77
      t93 = t8 ** 2
      t94 = t30 * t38 * t93
      t95 = t56 * t94
      t97 = t23 * t65
      t99 = x4 ** 2
      t104 = 0.9D1 * t97 * t66 * t34 * t99 * x4 * t22
      t105 = t3 * t67
      t107 = 0.18D2 * t105 * t70
      t109 = t30 * t33 * t99
      t110 = t86 * t109
      t115 = t31 * t38 * t33 * t51
      t116 = t24 * t27 * t115
      t118 = t86 * t94
      t120 = t62 * t14
      t123 = 0.18D2 * t56 * t109
      t126 = 0.18D2 * t3 * t27 * t115
      t127 = -0.90D2 * t87 + t91 - 0.9D1 * t95 + t104 + t107 - 0.27D2 * 
     #t110 - 0.27D2 * t116 - 0.27D2 * t118 + 0.54D2 * t120 - t123 - t126
      t139 = -t55 - 0.126D3 * t71 + 0.108D3 * t78 - 0.126D3 * t83 + 0.36
     #D2 * t10 + t16 - 0.126D3 * t63 + 0.18D2 * t95 - t104 + 0.63D2 * t1
     #10 + 0.63D2 * t118 - 0.126D3 * t120 + t123
      t146 = t99 * t22
      t156 = t23 * t27
      t170 = t91 + t107 + t61 + 0.108D3 * t87 + 0.63D2 * t116 + t126 + 0
     #.18D2 * t3 + 0.63D2 * t24 + 0.9D1 * t97 * t66 * t33 * t146 * t76 +
     # 0.9D1 * t97 * t66 * t12 * t37 * t38 * t93 + 0.18D2 * t156 * t31 *
     # t34 * t146 * t81 - 0.9D1 * t156 * t31 * t33 * t37 * t69 * t50 - 0
     #.36D2 * t105 * t82
      rrqg2qgh31J2 = 0.4D1 / 0.9D1 * (wd * (-0.18D2 * t10 + t16 - 0.9D1 
     #* t3 - 0.27D2 * t24 + t55 + t61 + 0.54D2 * t63 + 0.54D2 * t71 - 0.
     #90D2 * t78 + 0.54D2 * t83 + t127) + wd * (t139 + t170)) / t22 / t1
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh31J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 * t3
      t6 = 0.1D1 - z
      t7 = x1 * t6
      t8 = 0.1D1 - x3
      t9 = t8 * t7
      t10 = t5 * t9
      t12 = 0.1D1 - x1
      t14 = t6 * t12 * x4
      t16 = 0.18D2 * t5 * t14
      t18 = s * t4
      t22 = s - t18 * t7 * x3 - t9 * t18
      t23 = s * t1
      t24 = t22 * t23
      t26 = t4 ** 2
      t27 = t26 ** 2
      t30 = t6 ** 2
      t31 = t30 ** 2
      t33 = t12 ** 2
      t34 = t33 * t12
      t37 = x4 * t22
      t38 = x1 ** 2
      t42 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t8 * x4 * (0.1D1 - x4))
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t42 * t47
      t51 = t50 ** 2
      t55 = 0.9D1 * t23 * t27 * t4 * t31 * t6 * t34 * t37 * t38 * t51
      t56 = t3 * t26
      t58 = t12 * t50
      t59 = t30 * x1 * t58
      t61 = 0.18D2 * t56 * t59
      t62 = t24 * t4
      t63 = t62 * t9
      t65 = t26 * t4
      t66 = t30 * t6
      t67 = t65 * t66
      t68 = t24 * t67
      t69 = t38 * t8
      t70 = t69 * t58
      t71 = t68 * t70
      t73 = t26 * t30
      t76 = t8 * x1
      t77 = t12 * x4 * t76
      t78 = t24 * t73 * t77
      t81 = x1 * t50
      t82 = t33 * x4 * t81
      t83 = t68 * t82
      t86 = t24 * t26
      t87 = t86 * t59
      t91 = 0.18D2 * t3 * t73 * t77
      t93 = t8 ** 2
      t94 = t30 * t38 * t93
      t95 = t56 * t94
      t97 = t23 * t65
      t99 = x4 ** 2
      t104 = 0.9D1 * t97 * t66 * t34 * t99 * x4 * t22
      t105 = t3 * t67
      t107 = 0.18D2 * t105 * t70
      t109 = t30 * t33 * t99
      t110 = t86 * t109
      t115 = t31 * t38 * t33 * t51
      t116 = t24 * t27 * t115
      t118 = t86 * t94
      t120 = t62 * t14
      t123 = 0.18D2 * t56 * t109
      t126 = 0.18D2 * t3 * t27 * t115
      t127 = -0.90D2 * t87 + t91 - 0.9D1 * t95 + t104 + t107 - 0.27D2 * 
     #t110 - 0.27D2 * t116 - 0.27D2 * t118 + 0.54D2 * t120 - t123 - t126
      t137 = t99 * t22
      t147 = t23 * t27
      t153 = t61 + 0.108D3 * t87 + 0.63D2 * t116 + t126 - t55 - 0.126D3 
     #* t71 + 0.108D3 * t78 - 0.126D3 * t83 + t91 + t107 + 0.9D1 * t97 *
     # t66 * t33 * t137 * t76 + 0.9D1 * t97 * t66 * t12 * t37 * t38 * t9
     #3 + 0.18D2 * t147 * t31 * t34 * t137 * t81
      t170 = -0.9D1 * t147 * t31 * t33 * t37 * t69 * t50 - 0.36D2 * t105
     # * t82 + 0.18D2 * t3 + 0.63D2 * t24 + 0.36D2 * t10 + t16 - 0.126D3
     # * t63 + 0.18D2 * t95 - t104 + 0.63D2 * t110 + 0.63D2 * t118 - 0.1
     #26D3 * t120 + t123
      rrqg2qgh31J3 = 0.4D1 / 0.9D1 * (wd * (-0.18D2 * t10 + t16 - 0.9D1 
     #* t3 - 0.27D2 * t24 + t55 + t61 + 0.54D2 * t63 + 0.54D2 * t71 - 0.
     #90D2 * t78 + 0.54D2 * t83 + t127) + wd * (t153 + t170)) / t22 / t1
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh31J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t3 * t4
      t6 = 0.1D1 - z
      t7 = t6 * x1
      t8 = 0.1D1 - x3
      t9 = t7 * t8
      t10 = t5 * t9
      t12 = 0.1D1 - x1
      t14 = t6 * t12 * x4
      t16 = 0.18D2 * t5 * t14
      t18 = s * t4
      t22 = s - t18 * t7 * x3 - t18 * t9
      t23 = t1 * s
      t24 = t22 * t23
      t26 = t4 ** 2
      t27 = t26 ** 2
      t30 = t6 ** 2
      t31 = t30 ** 2
      t33 = t12 ** 2
      t34 = t33 * t12
      t37 = x4 * t22
      t38 = x1 ** 2
      t42 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t8 * x4 * (0.1D1 - x4))
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t42 * t47
      t51 = t50 ** 2
      t55 = 0.9D1 * t23 * t27 * t4 * t31 * t6 * t34 * t37 * t38 * t51
      t56 = t3 * t26
      t58 = t12 * t50
      t59 = t30 * x1 * t58
      t61 = 0.18D2 * t56 * t59
      t62 = t24 * t4
      t63 = t62 * t9
      t65 = t26 * t4
      t66 = t30 * t6
      t67 = t65 * t66
      t68 = t24 * t67
      t69 = t38 * t8
      t70 = t69 * t58
      t71 = t68 * t70
      t73 = t26 * t30
      t76 = x1 * t8
      t77 = t12 * x4 * t76
      t78 = t24 * t73 * t77
      t81 = x1 * t50
      t82 = t33 * x4 * t81
      t83 = t68 * t82
      t86 = t24 * t26
      t87 = t86 * t59
      t91 = 0.18D2 * t3 * t73 * t77
      t93 = t8 ** 2
      t94 = t30 * t38 * t93
      t95 = t56 * t94
      t97 = t23 * t65
      t99 = x4 ** 2
      t104 = 0.9D1 * t97 * t66 * t34 * t99 * x4 * t22
      t105 = t3 * t67
      t107 = 0.18D2 * t105 * t70
      t109 = t30 * t33 * t99
      t110 = t86 * t109
      t115 = t31 * t38 * t33 * t51
      t116 = t24 * t27 * t115
      t118 = t86 * t94
      t120 = t62 * t14
      t123 = 0.18D2 * t56 * t109
      t126 = 0.18D2 * t3 * t27 * t115
      t127 = -0.90D2 * t87 + t91 - 0.9D1 * t95 + t104 + t107 - 0.27D2 * 
     #t110 - 0.27D2 * t116 - 0.27D2 * t118 + 0.54D2 * t120 - t123 - t126
      t130 = t23 * t27
      t143 = t99 * t22
      t150 = -0.9D1 * t130 * t31 * t33 * t37 * t69 * t50 + 0.108D3 * t87
     # + t126 - t104 + t61 + 0.63D2 * t110 + 0.63D2 * t118 + t123 - 0.12
     #6D3 * t120 + 0.18D2 * t130 * t31 * t34 * t143 * t81 - 0.36D2 * t10
     #5 * t82 - t55 - 0.126D3 * t71
      t170 = 0.108D3 * t78 - 0.126D3 * t83 + 0.36D2 * t10 + t16 - 0.126D
     #3 * t63 + 0.18D2 * t3 + 0.63D2 * t24 + t107 + t91 + 0.9D1 * t97 * 
     #t66 * t12 * t37 * t38 * t93 + 0.9D1 * t97 * t66 * t33 * t143 * t76
     # + 0.18D2 * t95 + 0.63D2 * t116
      rrqg2qgh31J4 = 0.4D1 / 0.9D1 * (wd * (-0.18D2 * t10 + t16 - 0.9D1 
     #* t3 - 0.27D2 * t24 + t55 + t61 + 0.54D2 * t63 + 0.54D2 * t71 - 0.
     #90D2 * t78 + 0.54D2 * t83 + t127) + wd * (t150 + t170)) / t22 / t1
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh31J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t3 * t4
      t6 = 0.1D1 - z
      t7 = t6 * x1
      t8 = 0.1D1 - x3
      t9 = t7 * t8
      t10 = t5 * t9
      t12 = 0.1D1 - x1
      t14 = t6 * t12 * x4
      t16 = 0.18D2 * t5 * t14
      t18 = s * t4
      t22 = s - t18 * t7 * x3 - t18 * t9
      t23 = t1 * s
      t24 = t22 * t23
      t26 = t4 ** 2
      t27 = t26 ** 2
      t30 = t6 ** 2
      t31 = t30 ** 2
      t33 = t12 ** 2
      t34 = t33 * t12
      t37 = x4 * t22
      t38 = x1 ** 2
      t42 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t8 * x4 * (0.1D1 - x4))
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t42 * t47
      t51 = t50 ** 2
      t55 = 0.9D1 * t23 * t27 * t4 * t31 * t6 * t34 * t37 * t38 * t51
      t56 = t3 * t26
      t58 = t12 * t50
      t59 = t30 * x1 * t58
      t61 = 0.18D2 * t56 * t59
      t62 = t24 * t4
      t63 = t62 * t9
      t65 = t26 * t4
      t66 = t30 * t6
      t67 = t65 * t66
      t68 = t24 * t67
      t69 = t38 * t8
      t70 = t69 * t58
      t71 = t68 * t70
      t73 = t26 * t30
      t76 = x1 * t8
      t77 = t12 * x4 * t76
      t78 = t24 * t73 * t77
      t81 = x1 * t50
      t82 = t33 * x4 * t81
      t83 = t68 * t82
      t86 = t24 * t26
      t87 = t86 * t59
      t91 = 0.18D2 * t3 * t73 * t77
      t93 = t8 ** 2
      t94 = t30 * t38 * t93
      t95 = t56 * t94
      t97 = t23 * t65
      t99 = x4 ** 2
      t104 = 0.9D1 * t97 * t66 * t34 * t99 * x4 * t22
      t105 = t3 * t67
      t107 = 0.18D2 * t105 * t70
      t109 = t30 * t33 * t99
      t110 = t86 * t109
      t115 = t31 * t38 * t33 * t51
      t116 = t24 * t27 * t115
      t118 = t86 * t94
      t120 = t62 * t14
      t123 = 0.18D2 * t56 * t109
      t126 = 0.18D2 * t3 * t27 * t115
      t127 = -0.90D2 * t87 + t91 - 0.9D1 * t95 + t104 + t107 - 0.27D2 * 
     #t110 - 0.27D2 * t116 - 0.27D2 * t118 + 0.54D2 * t120 - t123 - t126
      t134 = t99 * t22
      t144 = t23 * t27
      t161 = t107 + 0.63D2 * t116 + 0.108D3 * t87 + t61 + t126 + 0.9D1 *
     # t97 * t66 * t33 * t134 * t76 + 0.9D1 * t97 * t66 * t12 * t37 * t3
     #8 * t93 + 0.18D2 * t144 * t31 * t34 * t134 * t81 - 0.9D1 * t144 * 
     #t31 * t33 * t37 * t69 * t50 - 0.36D2 * t105 * t82 + 0.18D2 * t3 + 
     #0.63D2 * t24 - 0.126D3 * t120
      t170 = t123 + 0.18D2 * t95 - t104 + 0.63D2 * t110 + 0.63D2 * t118 
     #- 0.126D3 * t63 + 0.36D2 * t10 + t16 - t55 - 0.126D3 * t83 + t91 -
     # 0.126D3 * t71 + 0.108D3 * t78
      rrqg2qgh31J5 = 0.4D1 / 0.9D1 * (wd * (-0.18D2 * t10 + t16 - 0.9D1 
     #* t3 - 0.27D2 * t24 + t55 + t61 + 0.54D2 * t63 + 0.54D2 * t71 - 0.
     #90D2 * t78 + 0.54D2 * t83 + t127) + wd * (t161 + t170)) / t22 / t1
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh31J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = s ** 2
      t12 = t11 * s
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 * t1
      t16 = t3 ** 2
      t17 = t16 * t3
      t18 = t15 * t17
      t19 = t13 * t18
      t20 = x1 ** 2
      t21 = t20 * t7
      t22 = 0.1D1 - x1
      t26 = cos(x2 * 0.3141592653589793D1)
      t31 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t34 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t26 * t31
      t35 = t22 * t34
      t36 = t21 * t35
      t39 = t14 * t16
      t42 = x1 * t7
      t43 = t22 * x4 * t42
      t46 = t22 ** 2
      t48 = x1 * t34
      t49 = t46 * x4 * t48
      t52 = t11 ** 2
      t53 = t52 * z
      t57 = t53 * t18
      t60 = t14 ** 2
      t63 = t16 ** 2
      t65 = t46 * t22
      t68 = x4 * t10
      t69 = t34 ** 2
      t76 = t13 * t14
      t78 = t16 * x1 * t35
      t81 = t53 * t14
      t87 = t63 * t20 * t46 * t69
      t93 = t12 * t15
      t96 = x4 ** 2
      t97 = t96 * t10
      t101 = -0.126D3 * t19 * t36 + 0.108D3 * t13 * t39 * t43 - 0.126D3 
     #* t19 * t49 + 0.18D2 * t53 * t39 * t43 + 0.18D2 * t57 * t36 - 0.9D
     #1 * t12 * t60 * t1 * t63 * t3 * t65 * t68 * t20 * t69 + 0.18D2 * t
     #53 + 0.63D2 * t13 + 0.108D3 * t76 * t78 + 0.18D2 * t81 * t78 + 0.1
     #8D2 * t53 * t60 * t87 + 0.63D2 * t13 * t60 * t87 + 0.9D1 * t93 * t
     #17 * t46 * t97 * t42
      t102 = t53 * t1
      t106 = t3 * t22 * x4
      t109 = t13 * t1
      t113 = t7 ** 2
      t114 = t16 * t20 * t113
      t124 = t16 * t46 * t96
      t139 = t12 * t60
      t153 = 0.36D2 * t102 * t8 + 0.18D2 * t102 * t106 - 0.126D3 * t109 
     #* t8 + 0.18D2 * t81 * t114 - 0.9D1 * t93 * t17 * t65 * t96 * x4 * 
     #t10 + 0.63D2 * t76 * t124 + 0.63D2 * t76 * t114 - 0.126D3 * t109 *
     # t106 + 0.18D2 * t81 * t124 + 0.9D1 * t93 * t17 * t22 * t68 * t20 
     #* t113 + 0.18D2 * t139 * t63 * t65 * t97 * t48 - 0.9D1 * t139 * t6
     #3 * t46 * t68 * t21 * t34 - 0.36D2 * t57 * t49
      rrqg2qgh31J6 = 0.4D1 / 0.9D1 * wd * (t101 + t153) / t10 / t11 / z 
     #/ 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t13 = t10 * t11 * s
      t14 = t1 ** 2
      t15 = t3 ** 2
      t18 = 0.1D1 - x1
      t28 = cos(x2 * 0.3141592653589793D1)
      t33 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      rrqg2qgh32J1 = 0.4D1 / 0.9D1 * wd * (-0.8D1 * t13 * t14 * t15 * t1
     #8 * x4 * x1 * t7 + 0.8D1 * t13 * t14 * t15 * x1 * t18 * (x3 + x4 -
     # 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33)) / t10 / t11 / z / 0.3141592
     #653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t13 = t10 * t11 * s
      t14 = t1 ** 2
      t15 = t3 ** 2
      t18 = 0.1D1 - x1
      t20 = x1 * t7
      t23 = t13 * t14
      t28 = cos(x2 * 0.3141592653589793D1)
      t33 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33
      t43 = t11 ** 2
      t47 = t43 * t14 * t1 * t15 * t3
      t48 = t18 ** 2
      t49 = x4 ** 2
      t55 = t15 * t48 * t49
      t58 = t14 ** 2
      t59 = t43 * t58
      t60 = t15 ** 2
      t61 = x1 ** 2
      t62 = t60 * t61
      t71 = t36 ** 2
      rrqg2qgh32J2 = 0.4D1 / 0.9D1 * (wd * (-0.8D1 * t13 * t14 * t15 * t
     #18 * x4 * t20 + 0.8D1 * t23 * t15 * x1 * t18 * t36) + wd * (0.4D1 
     #* t47 * t20 * t48 * t49 - 0.8D1 * t23 * t55 - 0.8D1 * t59 * t62 * 
     #t48 * t36 * t7 * x4 + 0.4D1 * t13 + 0.4D1 * t13 * t58 * t62 * t48 
     #* t71 + 0.4D1 * t43 * t58 * t1 * t60 * t3 * t61 * x1 * t48 * t71 *
     # t7 + 0.4D1 * t43 * t14 * t55 - 0.8D1 * t47 * x1 * t48 * t36 * x4 
     #+ 0.4D1 * t59 * t60 * t61 * t48 * t71)) / t10 / t11 / z / 0.314159
     #2653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = s ** 2
      t13 = t10 * t11 * s
      t14 = t1 ** 2
      t15 = t3 ** 2
      t18 = 0.1D1 - x1
      t19 = t18 * x4
      t20 = x1 * t7
      t21 = t19 * t20
      t22 = t13 * t14 * t15 * t21
      t23 = t13 * t14
      t28 = cos(x2 * 0.3141592653589793D1)
      t33 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33
      t37 = t18 * t36
      t39 = t23 * t15 * x1 * t37
      t43 = t11 ** 2
      t44 = t14 * t1
      t45 = t43 * t44
      t46 = t15 * t3
      t47 = t45 * t46
      t48 = t18 ** 2
      t49 = x4 ** 2
      t52 = t47 * t20 * t48 * t49
      t55 = t15 * t48 * t49
      t57 = 0.8D1 * t23 * t55
      t58 = t14 ** 2
      t59 = t43 * t58
      t60 = t15 ** 2
      t61 = x1 ** 2
      t62 = t60 * t61
      t67 = t59 * t62 * t48 * t36 * t7 * x4
      t71 = t36 ** 2
      t74 = t13 * t58 * t62 * t48 * t71
      t80 = t61 * x1
      t84 = t43 * t58 * t1 * t60 * t3 * t80 * t48 * t71 * t7
      t86 = t43 * t14
      t87 = t86 * t55
      t91 = x1 * t48 * t36 * x4
      t92 = t47 * t91
      t94 = t59 * t60
      t103 = t7 ** 2
      t108 = t43 * t1
      t112 = t3 * t18 * x4
      t116 = t15 * t61 * t103
      t128 = t86 * t15
      t132 = t61 * t7 * t37
      t140 = z ** 2
      t148 = t13 * t1
      t151 = t43 * z
      t155 = 0.8D1 * t87 - 0.7D1 * t45 * t46 * t80 * t103 * t7 + 0.3D1 *
     # t108 * t8 - 0.6D1 * t108 * t112 + 0.3D1 * t86 * t116 + 0.18D2 * t
     #94 * t80 * t103 * t37 + 0.48D2 * t13 * z * t1 * t3 * t20 - 0.12D2 
     #* t128 * t21 + 0.4D1 * t47 * t132 - 0.6D1 * t47 * t61 * t103 * t19
     # - 0.24D2 * t108 * t3 * t20 * t140 + 0.2D1 * t128 * x1 * t18 * t36
     # - 0.26D2 * t148 * t8 - 0.24D2 * t151 * t14 * t116
      t164 = t44 * t46
      t165 = t13 * t164
      t175 = 0.42D2 * t23 * t116 + 0.2D1 * t148 * t112 - t57 + t43 - t13
     # + 0.4D1 * t39 + 0.8D1 * t52 - t74 - 0.8D1 * t84 - 0.4D1 * t92 - 0
     #.26D2 * t165 * t132 + 0.2D1 * t165 * t91 + 0.24D2 * t151 * t164 * 
     #t132 + 0.12D2 * t22 - 0.4D1 * t67
      rrqg2qgh32J3 = 0.4D1 / 0.9D1 * (wd * (-0.8D1 * t22 + 0.8D1 * t39) 
     #+ wd * (0.4D1 * t52 - t57 - 0.8D1 * t67 + 0.4D1 * t13 + 0.4D1 * t7
     #4 + 0.4D1 * t84 + 0.4D1 * t87 - 0.8D1 * t92 + 0.4D1 * t94 * t61 * 
     #t48 * t71) + wd * (t155 + t175)) / t10 / t11 / z / 0.3141592653589
     #793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = s ** 2
      t13 = t10 * t11 * s
      t14 = t1 ** 2
      t15 = t3 ** 2
      t18 = 0.1D1 - x1
      t19 = t18 * x4
      t20 = x1 * t7
      t21 = t19 * t20
      t22 = t13 * t14 * t15 * t21
      t23 = t13 * t14
      t28 = cos(x2 * 0.3141592653589793D1)
      t33 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33
      t37 = t18 * t36
      t39 = t23 * t15 * x1 * t37
      t43 = t11 ** 2
      t44 = t14 * t1
      t45 = t43 * t44
      t46 = t15 * t3
      t47 = t45 * t46
      t48 = t18 ** 2
      t49 = x4 ** 2
      t52 = t47 * t20 * t48 * t49
      t55 = t15 * t48 * t49
      t57 = 0.8D1 * t23 * t55
      t58 = t14 ** 2
      t59 = t43 * t58
      t60 = t15 ** 2
      t61 = x1 ** 2
      t62 = t60 * t61
      t67 = t59 * t62 * t48 * t36 * t7 * x4
      t71 = t36 ** 2
      t74 = t13 * t58 * t62 * t48 * t71
      t80 = t61 * x1
      t84 = t43 * t58 * t1 * t60 * t3 * t80 * t48 * t71 * t7
      t86 = t43 * t14
      t87 = t86 * t55
      t91 = x1 * t48 * t36 * x4
      t92 = t47 * t91
      t94 = t59 * t60
      t101 = t44 * t46
      t102 = t13 * t101
      t104 = t61 * t7 * t37
      t109 = t7 ** 2
      t114 = t43 * t1
      t118 = t3 * t18 * x4
      t122 = t15 * t61 * t109
      t138 = -0.26D2 * t102 * t104 - t13 + 0.8D1 * t87 - 0.7D1 * t45 * t
     #46 * t80 * t109 * t7 + 0.3D1 * t114 * t8 - 0.6D1 * t114 * t118 + 0
     #.3D1 * t86 * t122 + 0.4D1 * t39 + 0.8D1 * t52 - t74 - 0.8D1 * t84 
     #- 0.4D1 * t92 + 0.18D2 * t94 * t80 * t109 * t37 + 0.48D2 * t13 * z
     # * t1 * t3 * t20
      t139 = t86 * t15
      t149 = t13 * t1
      t155 = t43 * z
      t160 = z ** 2
      t175 = -0.12D2 * t139 * t21 + 0.4D1 * t47 * t104 - 0.6D1 * t47 * t
     #61 * t109 * t19 - 0.4D1 * t67 + 0.2D1 * t149 * t118 + t43 + 0.12D2
     # * t22 + 0.2D1 * t102 * t91 + 0.24D2 * t155 * t101 * t104 - t57 - 
     #0.24D2 * t114 * t3 * t20 * t160 + 0.2D1 * t139 * x1 * t18 * t36 - 
     #0.26D2 * t149 * t8 - 0.24D2 * t155 * t14 * t122 + 0.42D2 * t23 * t
     #122
      rrqg2qgh32J4 = 0.4D1 / 0.9D1 * (wd * (-0.8D1 * t22 + 0.8D1 * t39) 
     #+ wd * (0.4D1 * t52 - t57 - 0.8D1 * t67 + 0.4D1 * t13 + 0.4D1 * t7
     #4 + 0.4D1 * t84 + 0.4D1 * t87 - 0.8D1 * t92 + 0.4D1 * t94 * t61 * 
     #t48 * t71) + wd * (t138 + t175)) / t10 / t11 / z / 0.3141592653589
     #793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = s ** 2
      t13 = t10 * t11 * s
      t14 = t1 ** 2
      t15 = t3 ** 2
      t18 = 0.1D1 - x1
      t19 = t18 * x4
      t20 = x1 * t7
      t21 = t19 * t20
      t22 = t13 * t14 * t15 * t21
      t23 = t13 * t14
      t28 = cos(x2 * 0.3141592653589793D1)
      t33 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t33
      t37 = t18 * t36
      t39 = t23 * t15 * x1 * t37
      t43 = t11 ** 2
      t44 = t14 * t1
      t45 = t43 * t44
      t46 = t15 * t3
      t47 = t45 * t46
      t48 = t18 ** 2
      t49 = x4 ** 2
      t52 = t47 * t20 * t48 * t49
      t55 = t15 * t48 * t49
      t57 = 0.8D1 * t23 * t55
      t58 = t14 ** 2
      t59 = t43 * t58
      t60 = t15 ** 2
      t61 = x1 ** 2
      t62 = t60 * t61
      t67 = t59 * t62 * t48 * t36 * t7 * x4
      t71 = t36 ** 2
      t74 = t13 * t58 * t62 * t48 * t71
      t80 = t61 * x1
      t84 = t43 * t58 * t1 * t60 * t3 * t80 * t48 * t71 * t7
      t86 = t43 * t14
      t87 = t86 * t55
      t91 = x1 * t48 * t36 * x4
      t92 = t47 * t91
      t94 = t59 * t60
      t101 = t44 * t46
      t102 = t13 * t101
      t107 = t7 ** 2
      t112 = t43 * t1
      t116 = t3 * t18 * x4
      t120 = t15 * t61 * t107
      t123 = t43 * z
      t126 = t61 * t7 * t37
      t130 = z ** 2
      t134 = t86 * t15
      t139 = t13 * t1
      t147 = 0.2D1 * t102 * t91 + 0.8D1 * t87 - 0.7D1 * t45 * t46 * t80 
     #* t107 * t7 + 0.3D1 * t112 * t8 - 0.6D1 * t112 * t116 + 0.3D1 * t8
     #6 * t120 - t13 + 0.24D2 * t123 * t101 * t126 - t57 - 0.24D2 * t112
     # * t3 * t20 * t130 + 0.2D1 * t134 * x1 * t18 * t36 - 0.26D2 * t139
     # * t8 - 0.24D2 * t123 * t14 * t120 + 0.42D2 * t23 * t120
      t175 = 0.2D1 * t139 * t116 + 0.12D2 * t22 - 0.4D1 * t67 - 0.26D2 *
     # t102 * t126 + 0.4D1 * t39 + 0.8D1 * t52 - t74 - 0.8D1 * t84 - 0.4
     #D1 * t92 + 0.18D2 * t94 * t80 * t107 * t37 + 0.48D2 * t13 * z * t1
     # * t3 * t20 - 0.12D2 * t134 * t21 + 0.4D1 * t47 * t126 - 0.6D1 * t
     #47 * t61 * t107 * t19 + t43
      rrqg2qgh32J5 = 0.4D1 / 0.9D1 * (wd * (-0.8D1 * t22 + 0.8D1 * t39) 
     #+ wd * (0.4D1 * t52 - t57 - 0.8D1 * t67 + 0.4D1 * t13 + 0.4D1 * t7
     #4 + 0.4D1 * t84 + 0.4D1 * t87 - 0.8D1 * t92 + 0.4D1 * t94 * t61 * 
     #t48 * t71) + wd * (t147 + t175)) / t10 / t11 / z / 0.3141592653589
     #793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J6
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
      t9 = t7 * t8
      t10 = t6 * t9
      t11 = 0.1D1 - x3
      t12 = x1 * t11
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t15 = x4 ** 2
      t18 = t10 * t12 * t14 * t15
      t20 = s * t3
      t21 = x1 * t7
      t24 = t21 * t11
      t26 = s - t20 * t21 * x3 - t20 * t24
      t28 = t26 * t1 * s
      t29 = t28 * t4
      t31 = t8 * t14 * t15
      t33 = 0.8D1 * t29 * t31
      t34 = t4 ** 2
      t35 = t2 * t34
      t36 = t8 ** 2
      t37 = x1 ** 2
      t38 = t36 * t37
      t43 = cos(x2 * 0.3141592653589793D1)
      t48 = Sqrt(x3 * t11 * x4 * (0.1D1 - x4))
      t51 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t43 * t48
      t55 = t35 * t38 * t14 * t51 * t11 * x4
      t59 = t51 ** 2
      t62 = t28 * t34 * t38 * t14 * t59
      t68 = t37 * x1
      t72 = t2 * t34 * t3 * t36 * t7 * t68 * t14 * t59 * t11
      t74 = t2 * t4
      t75 = t74 * t31
      t79 = x1 * t14 * t51 * x4
      t80 = t10 * t79
      t82 = t35 * t36
      t89 = t5 * t9
      t90 = t28 * t89
      t92 = t13 * t51
      t93 = t37 * t11 * t92
      t98 = t13 * x4
      t99 = t98 * t12
      t106 = t11 ** 2
      t111 = t2 * t3
      t115 = t7 * t13 * x4
      t119 = t8 * t37 * t106
      t122 = t2 * z
      t127 = z ** 2
      t131 = t74 * t8
      t136 = t28 * t3
      t139 = -0.26D2 * t90 * t93 + 0.12D2 * t28 * t4 * t8 * t99 + 0.2D1 
     #* t90 * t79 + 0.8D1 * t75 - 0.7D1 * t6 * t9 * t68 * t106 * t11 + 0
     #.3D1 * t111 * t24 - 0.6D1 * t111 * t115 + 0.3D1 * t74 * t119 + 0.2
     #4D2 * t122 * t89 * t93 + t2 - t33 - 0.24D2 * t111 * t7 * t12 * t12
     #7 + 0.2D1 * t131 * x1 * t13 * t51 - 0.26D2 * t136 * t24
      t172 = -0.24D2 * t122 * t4 * t119 + 0.42D2 * t29 * t119 + 0.2D1 * 
     #t136 * t115 + 0.8D1 * t18 - t62 - 0.8D1 * t72 - 0.4D1 * t80 + 0.4D
     #1 * t29 * t8 * x1 * t92 + 0.18D2 * t82 * t68 * t106 * t92 + 0.48D2
     # * t28 * z * t3 * t7 * t12 - 0.12D2 * t131 * t99 + 0.4D1 * t10 * t
     #93 - 0.6D1 * t10 * t37 * t106 * t98 - t28 - 0.4D1 * t55
      rrqg2qgh32J6 = 0.4D1 / 0.9D1 * (wd * (0.4D1 * t18 - t33 - 0.8D1 * 
     #t55 + 0.4D1 * t28 + 0.4D1 * t62 + 0.4D1 * t72 + 0.4D1 * t75 - 0.8D
     #1 * t80 + 0.4D1 * t82 * t37 * t14 * t59) + wd * (t139 + t172)) / t
     #26 / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh32J7
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
      t9 = t7 * t8
      t10 = t6 * t9
      t11 = 0.1D1 - x3
      t12 = x1 * t11
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t15 = x4 ** 2
      t24 = cos(x2 * 0.3141592653589793D1)
      t29 = Sqrt(x3 * t11 * x4 * (0.1D1 - x4))
      t32 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t24 * t29
      t34 = x1 * t14 * t32 * x4
      t37 = t4 ** 2
      t40 = t8 ** 2
      t43 = x1 ** 2
      t44 = t43 * x1
      t46 = t32 ** 2
      t51 = s * t3
      t52 = x1 * t7
      t55 = t52 * t11
      t57 = s - t51 * t52 * x3 - t51 * t55
      t59 = t57 * t1 * s
      t60 = t59 * t4
      t62 = t13 * t32
      t67 = t40 * t43
      t71 = t2 * t37
      t73 = t11 ** 2
      t83 = t2 * t4
      t84 = t83 * t8
      t85 = t13 * x4
      t86 = t85 * t12
      t90 = t43 * t11 * t62
      t97 = t5 * t9
      t98 = t59 * t97
      t105 = t2 + 0.8D1 * t10 * t12 * t14 * t15 - 0.4D1 * t10 * t34 - 0.
     #8D1 * t2 * t37 * t3 * t40 * t7 * t44 * t14 * t46 * t11 + 0.4D1 * t
     #60 * t8 * x1 * t62 - t59 * t37 * t67 * t14 * t46 + 0.18D2 * t71 * 
     #t40 * t44 * t73 * t62 + 0.48D2 * t59 * z * t3 * t7 * t12 - 0.12D2 
     #* t84 * t86 + 0.4D1 * t10 * t90 - 0.6D1 * t10 * t43 * t73 * t85 - 
     #0.26D2 * t98 * t90 + 0.12D2 * t59 * t4 * t8 * t86 - t59
      t106 = t2 * t3
      t108 = z ** 2
      t116 = t59 * t3
      t119 = t2 * z
      t122 = t8 * t43 * t73
      t126 = t8 * t14 * t15
      t139 = t7 * t13 * x4
      t159 = -0.24D2 * t106 * t7 * t12 * t108 + 0.2D1 * t84 * x1 * t13 *
     # t32 - 0.26D2 * t116 * t55 - 0.24D2 * t122 * t4 * t119 - 0.8D1 * t
     #60 * t126 + 0.8D1 * t83 * t126 - 0.7D1 * t6 * t9 * t44 * t73 * t11
     # + 0.3D1 * t106 * t55 - 0.6D1 * t106 * t139 + 0.3D1 * t83 * t122 +
     # 0.2D1 * t98 * t34 + 0.24D2 * t119 * t97 * t90 + 0.42D2 * t60 * t1
     #22 + 0.2D1 * t139 * t116 - 0.4D1 * t71 * t67 * t14 * t32 * t11 * x
     #4
      rrqg2qgh32J7 = 0.4D1 / 0.9D1 * wd * (t105 + t159) / t57 / t1 / z /
     # 0.3141592653589793D1

      end function
  
 