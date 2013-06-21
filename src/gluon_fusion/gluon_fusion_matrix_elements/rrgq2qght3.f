  
      subroutine rrgq2qght3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh31J1  
      doubleprecision rrgq2qgh31J2  
      doubleprecision rrgq2qgh31J3  
      doubleprecision rrgq2qgh31J4  
      doubleprecision rrgq2qgh31J5  
      doubleprecision rrgq2qgh31J6  
      doubleprecision rrgq2qgh31J7  
      doubleprecision rrgq2qgh32J1  
      doubleprecision rrgq2qgh32J2  
      doubleprecision rrgq2qgh32J3  
      doubleprecision rrgq2qgh32J4  
      doubleprecision rrgq2qgh32J5  
      doubleprecision rrgq2qgh32J6  
      doubleprecision rrgq2qght3s1e1  
      doubleprecision rrgq2qght3s1e0  
      doubleprecision rrgq2qght3s1em1  
      doubleprecision rrgq2qght3s1em2  
      doubleprecision rrgq2qght3s1em3  
      doubleprecision rrgq2qght3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = t1 * s
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
      t20 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t40 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t41 = t18 * t40
      t43 = t38 ** 2
      t44 = t43 * t9
      t45 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t91 = rrgq2qgh31J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
     #t45 / 0.720D3 + t117 * t21 / 0.720D3 + (0.90D2 * t15 * t16 * (t55 
     #- t126 * t46) - 0.180D3 * t132 * t69) * t136 * t72 / 0.720D3 - (0.
     #90D2 * t15 * t16 * (-t21 - t148 * t46 / 0.2D1 + t151 * t41) - 0.18
     #0D3 * t53 * t54 * (t151 * t46 - t55) - t70) * t136 / 0.720D3
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
      t186 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t190 = t177 ** 2
      t195 = log(-0.4D1 * t27 * t141 * t31 * x4 * t172 * t190)
      t196 = t195 ** 2
      t197 = t196 * t177
      t198 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t199 = t184 * t198
      t202 = t195 * t177
      t203 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t211 = t185 * t203
      t216 = t16 * t177
      t217 = t216 * t199
      t221 = x4 * t172
      t226 = log(-0.4D1 * t122 * t32 * t221 * t190)
      t227 = t226 * t177
      t239 = -(0.90D2 * t15 * t16 * (t185 * t186 + t197 * t199 / 0.2D1 -
     # t202 * t184 * t203) - 0.180D3 * t53 * t54 * (-t202 * t199 + t211)
     # + t67 * t217) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (t227 * t19
     #9 - t211) + 0.180D3 * t132 * t217) * t136 * t72 / 0.720D3
      t240 = FJET(XB1, XB2, s, 0.0D0, t171, t174, -t176, t182, t239)
      t242 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t243 = t18 * t242
      t245 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t246 = t19 * t245
      t247 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t248 = t18 * t247
      t256 = t19 * t242
      t261 = t68 * t248
      t262 = t67 * t261
      t268 = rrgq2qgh32J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t303 = -(0.90D2 * t15 * t16 * (t39 * t243 - t246 - t44 * t248 / 0.
     #2D1) - 0.180D3 * t53 * t54 * (t39 * t248 - t256) - t262) * t72 / 0
     #.720D3 + t87 * t256 / 0.720D3 + t90 * t19 * t268 / 0.8D1 + t109 * 
     #t19 * t247 / 0.720D3 + t117 * t246 / 0.720D3 + (0.90D2 * t15 * t16
     # * (t256 - t126 * t248) - 0.180D3 * t132 * t261) * t136 * t72 / 0.
     #720D3 - (0.90D2 * t15 * t16 * (-t148 * t248 / 0.2D1 - t246 + t151 
     #* t243) - 0.180D3 * t53 * t54 * (-t256 + t151 * t248) - t262) * t1
     #36 / 0.720D3
      t304 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t303)
      t306 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t308 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t311 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t16
     #8)
      t312 = t184 * t311
      t319 = t185 * t308
      t325 = t216 * t312
      t340 = -(0.90D2 * t15 * t16 * (t185 * t306 - t202 * t184 * t308 + 
     #t197 * t312 / 0.2D1) - 0.180D3 * t53 * t54 * (t319 - t202 * t312) 
     #+ t67 * t325) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (t227 * t312
     # - t319) + 0.180D3 * t132 * t325) * t136 * t72 / 0.720D3
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
      t366 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t367 = t365 * t366
      t369 = t351 * t365
      t370 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t371 = t369 * t370
      t376 = t16 * t351
      t377 = t376 * t367
      t383 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t389 = log(-0.4D1 * t122 * t32 * t347 * t357)
      t390 = t389 ** 2
      t391 = t390 * t351
      t394 = t389 * t351
      t410 = (0.90D2 * t15 * t16 * (t363 * t367 - t371) + 0.180D3 * t132
     # * t377) * t136 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t369 * t3
     #83 + t391 * t367 / 0.2D1 - t394 * t365 * t370) - 0.180D3 * t53 * t
     #54 * (-t394 * t367 + t371) + t67 * t377) * t136 / 0.720D3
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
      t445 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t446 = t444 * t445
      t448 = t419 * t444
      t449 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t455 = t16 * t419
      t459 = 0.90D2 * t15 * t16 * (-t442 * t446 + t448 * t449) - 0.180D3
     # * t132 * t455 * t446
      t463 = FJET(XB1, XB2, s, t415, -t416, t417, -t418, t432, t459 * t1
     #36 * t72 / 0.720D3)
      t465 = t136 * t72
      t468 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t469 = t369 * t468
      t470 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t471 = t365 * t470
      t477 = t376 * t471
      t483 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t502 = (0.90D2 * t15 * t16 * (-t469 + t363 * t471) + 0.180D3 * t13
     #2 * t477) * t136 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t369 * t
     #483 + t391 * t471 / 0.2D1 - t394 * t365 * t468) - 0.180D3 * t53 * 
     #t54 * (-t394 * t471 + t469) + t67 * t477) * t136 / 0.720D3
      t503 = FJET(XB1, XB2, s, -t349, t346, 0.0D0, -t350, t355, t502)
      t505 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t507 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t168)
      t508 = t444 * t507
      t517 = 0.90D2 * t15 * t16 * (t448 * t505 - t442 * t508) - 0.180D3 
     #* t132 * t455 * t508
      t521 = FJET(XB1, XB2, s, -t416, t415, -t418, t417, t432, t517 * t1
     #36 * t72 / 0.720D3)
      rrgq2qght3s1e1 = t166 * t165 + t240 * t239 + t304 * t303 + t341 * 
     #t340 + t411 * t410 + t463 * t459 * t465 / 0.720D3 + t503 * t502 + 
     #t521 * t517 * t465 / 0.720D3

      end function



      doubleprecision function rrgq2qght3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = t1 * s
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
      t20 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t40 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t81 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t136 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t137 = t135 * t136
      t143 = t126 ** 2
      t148 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t143)
      t149 = t148 * t126
      t151 = t126 * t135
      t152 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t164 = -t133 * t137 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t149 *
     # t137 + t151 * t152) - 0.180D3 * t48 * t132 * t137) * t54 / 0.720D
     #3
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t168 = t18 * t167
      t170 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t171 = t19 * t170
      t178 = 0.180D3 * t48 * t49 * t168
      t195 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t202 = -(0.90D2 * t15 * t16 * (t39 * t168 - t171) + t178) * t54 / 
     #0.720D3 + t94 * t171 / 0.720D3 + t57 * t168 * t59 / 0.8D1 - (0.90D
     #2 * t15 * t16 * (-t171 + t71 * t168) + t178) * t58 / 0.720D3 + t80
     # * t19 * t195 / 0.8D1 + t110 * t19 * t167 / 0.720D3
      t203 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t202)
      t205 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t207 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
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
      t243 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t244 = t242 * t243
      t250 = t234 ** 2
      t255 = log(-0.4D1 * t63 * t26 * t28 * t32 * t230 * t250)
      t256 = t255 * t234
      t258 = t234 * t242
      t259 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
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
      t299 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t117)
      t304 = FJET(XB1, XB2, s, t276, -t277, t278, -t279, t294, t296 * t2
     #98 * t299 * t59 / 0.8D1)
      t306 = t3 * t16
      t308 = t280 * t298
      t314 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t315 = t242 * t314
      t320 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t332 = -t240 * t315 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t256 *
     # t315 + t258 * t320) - 0.180D3 * t48 * t239 * t315) * t58 / 0.720D
     #3
      t333 = FJET(XB1, XB2, s, -t232, t229, 0.0D0, -t233, t238, t332)
      t335 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t117)
      t340 = FJET(XB1, XB2, s, -t277, t276, -t279, t278, t294, t296 * t2
     #98 * t335 * t59 / 0.8D1)
      rrgq2qght3s1e0 = t115 * t114 + t165 * t164 + t203 * t202 + t224 * 
     #t223 + t272 * t271 + t304 * 0.3141592653589793D1 * t306 * t308 * t
     #299 * t58 * t54 / 0.8D1 + t333 * t332 + t340 * 0.3141592653589793D
     #1 * t306 * t308 * t335 * t58 * t54 / 0.8D1

      end function



      doubleprecision function rrgq2qght3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = t1 * s
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
      t21 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t22 = 0.1D1 / x3
      t27 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t83 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t65)
      t85 = t82 * t83 * t57
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t17 * t85 / 0
     #.8D1)
      t90 = t3 * t16
      t94 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t99 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t110 = t17 * t20 * t94 * t22 / 0.8D1 + t17 * t20 * t99 / 0.8D1 + t
     #53 * t20 * t94 / 0.720D3 + t17 * t20 * t94 * t57 / 0.8D1
      t111 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t113 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t65
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
      t139 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t141 = t138 * t139 * t22
      t144 = FJET(XB1, XB2, s, t126, -t129, -t130, 0.0D0, t135, -t17 * t
     #141 / 0.8D1)
      t149 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t151 = t138 * t149 * t22
      t154 = FJET(XB1, XB2, s, -t129, t126, 0.0D0, -t130, t135, -t17 * t
     #151 / 0.8D1)
      rrgq2qght3s1em1 = t63 * t62 - t88 * 0.3141592653589793D1 * t90 * t
     #85 / 0.8D1 + t111 * t110 - t118 * 0.3141592653589793D1 * t90 * t11
     #5 / 0.8D1 - t144 * 0.3141592653589793D1 * t90 * t141 / 0.8D1 - t15
     #4 * 0.3141592653589793D1 * t90 * t151 / 0.8D1

      end function



      doubleprecision function rrgq2qght3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = t1 * s
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
      t21 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t17 * t20 * t
     #21 / 0.8D1)
      t28 = t16 * t9
      t32 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t36 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t17 * t20 * t
     #32 / 0.8D1)
      rrgq2qght3s1em2 = t25 * 0.3141592653589793D1 * t3 * t28 * t19 * t2
     #1 / 0.8D1 + t36 * 0.3141592653589793D1 * t3 * t28 * t19 * t32 / 0.
     #8D1

      end function



      doubleprecision function rrgq2qght3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6
      rrgq2qght3s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6
      rrgq2qght3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh31J1
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
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      rrgq2qgh31J1 = 0.4D1 / 0.9D1 * wd * (0.8D1 * t13 * t14 * t16 * x1 
     #* t18 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27) - 0.8D1 * 
     #t13 * t14 * t16 * t18 * x4 * x1 * t7) / t11 / t10 / z / 0.31415926
     #53589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh31J2
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
      t15 = t13 * t14
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t30 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27
      t37 = x1 * t7
      t44 = t11 ** 2
      t46 = t18 ** 2
      t48 = x4 ** 2
      t49 = t16 * t46 * t48
      t52 = t14 ** 2
      t54 = t16 ** 2
      t55 = x1 ** 2
      t56 = t54 * t55
      t57 = t30 ** 2
      t58 = t46 * t57
      t62 = t44 * t52
      t71 = t44 * t14 * t1 * t16 * t3
      rrgq2qgh31J2 = 0.4D1 / 0.9D1 * (wd * (0.8D1 * t15 * t16 * x1 * t18
     # * t30 - 0.8D1 * t13 * t14 * t16 * t18 * x4 * t37) + wd * (0.4D1 *
     # t13 + 0.4D1 * t44 * t14 * t49 + 0.4D1 * t13 * t52 * t56 * t58 + 0
     #.4D1 * t62 * t54 * t55 * t46 * t57 - 0.8D1 * t71 * t46 * x4 * x1 *
     # t30 - 0.8D1 * t15 * t49 - 0.8D1 * t62 * t56 * t46 * t30 * t7 * x4
     # + 0.4D1 * t44 * t52 * t1 * t54 * t3 * t55 * x1 * t7 * t58 + 0.4D1
     # * t71 * t37 * t46 * t48)) / t11 / t10 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh31J3
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
      t15 = t13 * t14
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t30 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27
      t31 = t18 * t30
      t33 = t15 * t16 * x1 * t31
      t36 = t18 * x4
      t37 = x1 * t7
      t38 = t36 * t37
      t39 = t13 * t14 * t16 * t38
      t44 = t11 ** 2
      t45 = t44 * t14
      t46 = t18 ** 2
      t48 = x4 ** 2
      t49 = t16 * t46 * t48
      t50 = t45 * t49
      t52 = t14 ** 2
      t54 = t16 ** 2
      t55 = x1 ** 2
      t56 = t54 * t55
      t57 = t30 ** 2
      t58 = t46 * t57
      t60 = t13 * t52 * t56 * t58
      t62 = t44 * t52
      t63 = t62 * t54
      t68 = t14 * t1
      t69 = t44 * t68
      t70 = t16 * t3
      t71 = t69 * t70
      t74 = t46 * x4 * x1 * t30
      t75 = t71 * t74
      t78 = 0.8D1 * t15 * t49
      t83 = t62 * t56 * t46 * t30 * t7 * x4
      t89 = t55 * x1
      t92 = t44 * t52 * t1 * t54 * t3 * t89 * t7 * t58
      t96 = t71 * t37 * t46 * t48
      t101 = t7 ** 2
      t102 = t16 * t55 * t101
      t105 = t13 * t1
      t108 = t44 * t1
      t110 = z ** 2
      t114 = t44 * z
      t118 = t45 * t16
      t123 = t68 * t70
      t126 = t55 * t7 * t31
      t129 = t13 * t123
      t135 = 0.42D2 * t15 * t102 - 0.26D2 * t105 * t8 - 0.24D2 * t108 * 
     #t3 * t37 * t110 - t78 - 0.24D2 * t114 * t14 * t102 + 0.2D1 * t118 
     #* x1 * t18 * t30 + 0.24D2 * t114 * t123 * t126 - 0.26D2 * t129 * t
     #126 + t44 - t13 + 0.12D2 * t39 - t60 - 0.8D1 * t92 - 0.4D1 * t75
      t155 = t3 * t18 * x4
      t174 = 0.8D1 * t96 - 0.6D1 * t71 * t55 * t101 * t36 + 0.18D2 * t63
     # * t89 * t101 * t31 + 0.4D1 * t71 * t126 + 0.48D2 * t13 * z * t1 *
     # t3 * t37 - 0.12D2 * t118 * t38 - 0.6D1 * t108 * t155 + 0.3D1 * t4
     #5 * t102 + 0.3D1 * t108 * t8 - 0.7D1 * t69 * t70 * t89 * t101 * t7
     # + 0.8D1 * t50 + 0.2D1 * t105 * t155 + 0.2D1 * t129 * t74 + 0.4D1 
     #* t33 - 0.4D1 * t83
      rrgq2qgh31J3 = 0.4D1 / 0.9D1 * (wd * (0.8D1 * t33 - 0.8D1 * t39) +
     # wd * (0.4D1 * t13 + 0.4D1 * t50 + 0.4D1 * t60 + 0.4D1 * t63 * t55
     # * t46 * t57 - 0.8D1 * t75 - t78 - 0.8D1 * t83 + 0.4D1 * t92 + 0.4
     #D1 * t96) + wd * (t135 + t174)) / t11 / t10 / z / 0.31415926535897
     #93D1

      end function
  
   
 

      doubleprecision function rrgq2qgh31J4
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
      t15 = t13 * t14
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t30 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27
      t31 = t18 * t30
      t33 = t15 * t16 * x1 * t31
      t36 = t18 * x4
      t37 = x1 * t7
      t38 = t36 * t37
      t39 = t13 * t14 * t16 * t38
      t44 = t11 ** 2
      t45 = t44 * t14
      t46 = t18 ** 2
      t48 = x4 ** 2
      t49 = t16 * t46 * t48
      t50 = t45 * t49
      t52 = t14 ** 2
      t54 = t16 ** 2
      t55 = x1 ** 2
      t56 = t54 * t55
      t57 = t30 ** 2
      t58 = t46 * t57
      t60 = t13 * t52 * t56 * t58
      t62 = t44 * t52
      t63 = t62 * t54
      t68 = t14 * t1
      t69 = t44 * t68
      t70 = t16 * t3
      t71 = t69 * t70
      t74 = t46 * x4 * x1 * t30
      t75 = t71 * t74
      t78 = 0.8D1 * t15 * t49
      t83 = t62 * t56 * t46 * t30 * t7 * x4
      t89 = t55 * x1
      t92 = t44 * t52 * t1 * t54 * t3 * t89 * t7 * t58
      t96 = t71 * t37 * t46 * t48
      t100 = t68 * t70
      t101 = t13 * t100
      t104 = t44 * z
      t107 = t55 * t7 * t31
      t114 = t7 ** 2
      t115 = t16 * t55 * t114
      t126 = t45 * t16
      t134 = 0.2D1 * t101 * t74 + 0.24D2 * t104 * t100 * t107 - 0.26D2 *
     # t101 * t107 - t78 - 0.24D2 * t104 * t14 * t115 - t13 + 0.4D1 * t3
     #3 - t60 - 0.4D1 * t75 - 0.8D1 * t92 + 0.8D1 * t96 - 0.6D1 * t71 * 
     #t55 * t114 * t36 - 0.12D2 * t126 * t38 + 0.48D2 * t13 * z * t1 * t
     #3 * t37
      t143 = t44 * t1
      t145 = t3 * t18 * x4
      t162 = t13 * t1
      t166 = z ** 2
      t174 = 0.4D1 * t71 * t107 + 0.18D2 * t63 * t89 * t114 * t31 + 0.12
     #D2 * t39 + t44 - 0.4D1 * t83 - 0.6D1 * t143 * t145 + 0.3D1 * t45 *
     # t115 + 0.3D1 * t143 * t8 - 0.7D1 * t69 * t70 * t89 * t114 * t7 + 
     #0.8D1 * t50 + 0.2D1 * t126 * x1 * t18 * t30 - 0.26D2 * t162 * t8 -
     # 0.24D2 * t143 * t3 * t37 * t166 + 0.42D2 * t15 * t115 + 0.2D1 * t
     #162 * t145
      rrgq2qgh31J4 = 0.4D1 / 0.9D1 * (wd * (0.8D1 * t33 - 0.8D1 * t39) +
     # wd * (0.4D1 * t13 + 0.4D1 * t50 + 0.4D1 * t60 + 0.4D1 * t63 * t55
     # * t46 * t57 - 0.8D1 * t75 - t78 - 0.8D1 * t83 + 0.4D1 * t92 + 0.4
     #D1 * t96) + wd * (t134 + t174)) / t11 / t10 / z / 0.31415926535897
     #93D1

      end function
  
   
 

      doubleprecision function rrgq2qgh31J5
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
      t15 = t13 * t14
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t30 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27
      t31 = t18 * t30
      t33 = t15 * t16 * x1 * t31
      t36 = t18 * x4
      t37 = x1 * t7
      t38 = t36 * t37
      t39 = t13 * t14 * t16 * t38
      t44 = t11 ** 2
      t45 = t44 * t14
      t46 = t18 ** 2
      t48 = x4 ** 2
      t49 = t16 * t46 * t48
      t50 = t45 * t49
      t52 = t14 ** 2
      t54 = t16 ** 2
      t55 = x1 ** 2
      t56 = t54 * t55
      t57 = t30 ** 2
      t58 = t46 * t57
      t60 = t13 * t52 * t56 * t58
      t62 = t44 * t52
      t63 = t62 * t54
      t68 = t14 * t1
      t69 = t44 * t68
      t70 = t16 * t3
      t71 = t69 * t70
      t74 = t46 * x4 * x1 * t30
      t75 = t71 * t74
      t78 = 0.8D1 * t15 * t49
      t83 = t62 * t56 * t46 * t30 * t7 * x4
      t89 = t55 * x1
      t92 = t44 * t52 * t1 * t54 * t3 * t89 * t7 * t58
      t96 = t71 * t37 * t46 * t48
      t101 = t44 * t1
      t103 = t3 * t18 * x4
      t108 = t68 * t70
      t109 = t13 * t108
      t112 = t44 * z
      t115 = t55 * t7 * t31
      t121 = t7 ** 2
      t122 = t16 * t55 * t121
      t135 = t45 * t16
      t140 = 0.8D1 * t50 - 0.6D1 * t101 * t103 - 0.4D1 * t83 + 0.12D2 * 
     #t39 - t13 + 0.2D1 * t109 * t74 + 0.24D2 * t112 * t108 * t115 - 0.2
     #6D2 * t109 * t115 + t44 + 0.3D1 * t45 * t122 + 0.3D1 * t101 * t8 -
     # 0.7D1 * t69 * t70 * t89 * t121 * t7 - 0.24D2 * t112 * t14 * t122 
     #+ 0.2D1 * t135 * x1 * t18 * t30
      t141 = t13 * t1
      t145 = z ** 2
      t174 = -0.26D2 * t141 * t8 - 0.24D2 * t101 * t3 * t37 * t145 + 0.4
     #2D2 * t15 * t122 + 0.2D1 * t141 * t103 - t78 + 0.4D1 * t33 - t60 -
     # 0.4D1 * t75 - 0.8D1 * t92 + 0.8D1 * t96 - 0.6D1 * t71 * t55 * t12
     #1 * t36 - 0.12D2 * t135 * t38 + 0.48D2 * t13 * z * t1 * t3 * t37 +
     # 0.4D1 * t71 * t115 + 0.18D2 * t63 * t89 * t121 * t31
      rrgq2qgh31J5 = 0.4D1 / 0.9D1 * (wd * (0.8D1 * t33 - 0.8D1 * t39) +
     # wd * (0.4D1 * t13 + 0.4D1 * t50 + 0.4D1 * t60 + 0.4D1 * t63 * t55
     # * t46 * t57 - 0.8D1 * t75 - t78 - 0.8D1 * t83 + 0.4D1 * t92 + 0.4
     #D1 * t96) + wd * (t140 + t174)) / t11 / t10 / z / 0.31415926535897
     #93D1

      end function
  
   
 

      doubleprecision function rrgq2qgh31J6
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
      t15 = t11 ** 2
      t16 = t1 ** 2
      t17 = t15 * t16
      t18 = t3 ** 2
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t22 = x4 ** 2
      t23 = t18 * t20 * t22
      t24 = t17 * t23
      t26 = t16 ** 2
      t28 = t18 ** 2
      t29 = x1 ** 2
      t30 = t28 * t29
      t34 = cos(x2 * 0.3141592653589793D1)
      t39 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t42 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t39
      t43 = t42 ** 2
      t44 = t20 * t43
      t46 = t13 * t26 * t30 * t44
      t48 = t15 * t26
      t49 = t48 * t28
      t54 = t16 * t1
      t55 = t15 * t54
      t56 = t18 * t3
      t57 = t55 * t56
      t60 = t20 * x4 * x1 * t42
      t61 = t57 * t60
      t63 = t13 * t16
      t65 = 0.8D1 * t63 * t23
      t70 = t48 * t30 * t20 * t42 * t7 * x4
      t76 = t29 * x1
      t79 = t15 * t26 * t1 * t28 * t3 * t76 * t7 * t44
      t81 = x1 * t7
      t84 = t57 * t81 * t20 * t22
      t90 = t19 * x4
      t91 = t90 * t81
      t94 = t15 * z
      t97 = t7 ** 2
      t98 = t18 * t29 * t97
      t101 = t17 * t18
      t106 = t13 * t1
      t109 = t15 * t1
      t111 = z ** 2
      t118 = t3 * t19 * x4
      t135 = 0.12D2 * t13 * t16 * t18 * t91 - 0.24D2 * t94 * t16 * t98 +
     # 0.2D1 * t101 * x1 * t19 * t42 - 0.26D2 * t106 * t8 - 0.24D2 * t10
     #9 * t3 * t81 * t111 + 0.42D2 * t63 * t98 + 0.2D1 * t106 * t118 - t
     #46 - 0.4D1 * t61 - 0.8D1 * t79 + 0.8D1 * t84 - 0.6D1 * t57 * t29 *
     # t97 * t90 - 0.12D2 * t101 * t91 + 0.48D2 * t13 * z * t1 * t3 * t8
     #1
      t138 = t29 * t19 * t42 * t7
      t142 = t19 * t42
      t146 = t54 * t56
      t147 = t13 * t146
      t172 = 0.4D1 * t57 * t138 + 0.18D2 * t49 * t76 * t97 * t142 + 0.2D
     #1 * t147 * t60 + 0.4D1 * t63 * t18 * x1 * t142 - t13 - t65 - 0.4D1
     # * t70 - 0.6D1 * t109 * t118 + 0.3D1 * t17 * t98 + 0.3D1 * t109 * 
     #t8 - 0.7D1 * t55 * t56 * t76 * t97 * t7 + 0.8D1 * t24 + 0.24D2 * t
     #94 * t146 * t138 - 0.26D2 * t147 * t138 + t15
      rrgq2qgh31J6 = 0.4D1 / 0.9D1 * (wd * (0.4D1 * t13 + 0.4D1 * t24 + 
     #0.4D1 * t46 + 0.4D1 * t49 * t29 * t20 * t43 - 0.8D1 * t61 - t65 - 
     #0.8D1 * t70 + 0.4D1 * t79 + 0.4D1 * t84) + wd * (t135 + t172)) / t
     #11 / t10 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh31J7
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
      t24 = t11 ** 2
      t25 = t14 ** 2
      t26 = t24 * t25
      t27 = t15 ** 2
      t28 = x1 ** 2
      t29 = t27 * t28
      t31 = t18 ** 2
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t49 = t13 * t14
      t51 = x4 ** 2
      t52 = t15 * t31 * t51
      t55 = t24 * z
      t58 = t7 ** 2
      t59 = t15 * t28 * t58
      t62 = t24 * t14
      t63 = t62 * t15
      t68 = t13 * t1
      t71 = t24 * t1
      t73 = z ** 2
      t80 = t3 * t18 * x4
      t83 = t14 * t1
      t84 = t15 * t3
      t85 = t83 * t84
      t86 = t13 * t85
      t89 = t31 * x4 * x1 * t43
      t92 = t24 * t83
      t93 = t92 * t84
      t102 = t28 * x1
      t104 = t43 ** 2
      t105 = t31 * t104
      t114 = 0.12D2 * t13 * t14 * t15 * t21 - 0.4D1 * t26 * t29 * t31 * 
     #t43 * t7 * x4 - 0.8D1 * t49 * t52 - 0.24D2 * t55 * t14 * t59 + 0.2
     #D1 * t63 * x1 * t18 * t43 - 0.26D2 * t68 * t8 - 0.24D2 * t71 * t3 
     #* t20 * t73 + 0.42D2 * t49 * t59 + 0.2D1 * t68 * t80 + 0.2D1 * t86
     # * t89 + 0.8D1 * t93 * t20 * t31 * t51 - 0.8D1 * t24 * t25 * t1 * 
     #t27 * t3 * t102 * t7 * t105 - 0.4D1 * t93 * t89 - t13 * t25 * t29 
     #* t105
      t128 = t28 * t18 * t43 * t7
      t133 = t18 * t43
      t159 = -0.6D1 * t93 * t28 * t58 * t19 - 0.12D2 * t63 * t21 + 0.48D
     #2 * t13 * z * t1 * t3 * t20 + 0.4D1 * t93 * t128 + 0.18D2 * t26 * 
     #t27 * t102 * t58 * t133 + 0.4D1 * t49 * t15 * x1 * t133 + t24 + 0.
     #24D2 * t55 * t85 * t128 - 0.26D2 * t86 * t128 + 0.8D1 * t62 * t52 
     #- 0.6D1 * t71 * t80 + 0.3D1 * t62 * t59 + 0.3D1 * t71 * t8 - 0.7D1
     # * t92 * t84 * t102 * t58 * t7 - t13
      rrgq2qgh31J7 = 0.4D1 / 0.9D1 * wd * (t114 + t159) / t11 / t10 / z 
     #/ 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh32J1
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
      t15 = t3 ** 2
      t16 = t14 * t15
      t18 = 0.1D1 - x1
      t21 = t18 * x4 * x1 * t7
      t24 = t14 * t1
      t25 = t15 * t3
      t26 = t24 * t25
      t27 = t13 * t26
      t28 = t18 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t38 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t46 = t11 ** 2
      t47 = t46 * z
      t48 = t47 * t1
      t50 = t3 * t18 * x4
      t53 = t47 * t14
      t54 = x1 ** 2
      t56 = t7 ** 2
      t57 = t15 * t54 * t56
      t60 = t14 ** 2
      t62 = t15 ** 2
      t64 = t41 ** 2
      t66 = t62 * t54 * t28 * t64
      t70 = x4 ** 2
      t71 = t15 * t28 * t70
      t80 = t18 * t41
      t81 = t54 * t7 * t80
      t88 = t13 * t1
      t93 = t13 * t14
      t97 = t15 * x1 * t80
      t104 = t28 * t18
      t127 = 0.54D2 * t27 * t81 + 0.54D2 * t88 * t8 + 0.54D2 * t88 * t50
     # - 0.27D2 * t93 * t57 + 0.18D2 * t53 * t97 - 0.18D2 * t48 * t8 + 0
     #.9D1 * t12 * t24 * t25 * t104 * t70 * x4 * t10 + 0.18D2 * t47 * t1
     #6 * t21 - 0.27D2 * t93 * t71 + 0.9D1 * t12 * t60 * t1 * t62 * t3 *
     # t104 * x4 * t10 * t54 * t64 - 0.90D2 * t93 * t97
      rrgq2qgh32J1 = 0.4D1 / 0.9D1 * wd * (-0.90D2 * t13 * t16 * t21 + 0
     #.54D2 * t27 * t28 * x4 * x1 * t41 + 0.18D2 * t48 * t50 - 0.9D1 * t
     #53 * t57 - 0.18D2 * t47 * t60 * t66 - 0.18D2 * t53 * t71 - 0.27D2 
     #* t13 - 0.27D2 * t13 * t60 * t66 + 0.18D2 * t47 * t26 * t81 - 0.9D
     #1 * t47 + t127) / t11 / t10 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh32J2
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
      t15 = t3 ** 2
      t16 = t14 * t15
      t18 = 0.1D1 - x1
      t20 = x1 * t7
      t21 = t18 * x4 * t20
      t22 = t13 * t16 * t21
      t24 = t14 * t1
      t25 = t15 * t3
      t26 = t24 * t25
      t27 = t13 * t26
      t28 = t18 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t38 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t42 = x1 * t41
      t43 = t28 * x4 * t42
      t44 = t27 * t43
      t46 = t11 ** 2
      t47 = t46 * z
      t48 = t47 * t1
      t50 = t3 * t18 * x4
      t52 = 0.18D2 * t48 * t50
      t53 = t47 * t14
      t54 = x1 ** 2
      t56 = t7 ** 2
      t57 = t15 * t54 * t56
      t58 = t53 * t57
      t60 = t14 ** 2
      t62 = t15 ** 2
      t64 = t41 ** 2
      t66 = t62 * t54 * t28 * t64
      t68 = 0.18D2 * t47 * t60 * t66
      t70 = x4 ** 2
      t71 = t15 * t28 * t70
      t73 = 0.18D2 * t53 * t71
      t76 = t13 * t60 * t66
      t78 = t47 * t26
      t79 = t54 * t7
      t80 = t18 * t41
      t81 = t79 * t80
      t83 = 0.18D2 * t78 * t81
      t86 = t27 * t81
      t88 = t13 * t1
      t89 = t88 * t8
      t91 = t88 * t50
      t93 = t13 * t14
      t94 = t93 * t57
      t97 = t15 * x1 * t80
      t99 = 0.18D2 * t53 * t97
      t100 = t48 * t8
      t102 = t12 * t24
      t104 = t28 * t18
      t109 = 0.9D1 * t102 * t25 * t104 * t70 * x4 * t10
      t112 = 0.18D2 * t47 * t16 * t21
      t113 = t93 * t71
      t120 = x4 * t10
      t124 = 0.9D1 * t12 * t60 * t1 * t62 * t3 * t104 * t120 * t54 * t64
      t125 = t93 * t97
      t127 = 0.54D2 * t86 + 0.54D2 * t89 + 0.54D2 * t91 - 0.27D2 * t94 +
     # t99 - 0.18D2 * t100 + t109 + t112 - 0.27D2 * t113 + t124 - 0.90D2
     # * t125
      t132 = t70 * t10
      t142 = t12 * t60
      t152 = t83 + 0.9D1 * t102 * t25 * t28 * t132 * t20 + 0.9D1 * t102 
     #* t25 * t18 * t120 * t54 * t56 - 0.9D1 * t142 * t62 * t28 * t120 *
     # t79 * t41 - 0.126D3 * t86 + t52 + 0.18D2 * t58 + t73 + t112 - t12
     #4 + t68 + 0.63D2 * t76 + t99
      t170 = 0.108D3 * t125 + 0.108D3 * t22 - 0.126D3 * t89 - 0.126D3 * 
     #t91 + 0.63D2 * t94 + 0.36D2 * t100 - t109 + 0.63D2 * t113 + 0.18D2
     # * t142 * t62 * t104 * t132 * t42 - 0.36D2 * t78 * t43 + 0.63D2 * 
     #t13 + 0.18D2 * t47 - 0.126D3 * t44
      rrgq2qgh32J2 = 0.4D1 / 0.9D1 * (wd * (-0.90D2 * t22 + 0.54D2 * t44
     # + t52 - 0.9D1 * t58 - t68 - t73 - 0.27D2 * t13 - 0.27D2 * t76 + t
     #83 - 0.9D1 * t47 + t127) + wd * (t152 + t170)) / t11 / t10 / z / 0
     #.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh32J3
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
      t15 = t3 ** 2
      t16 = t14 * t15
      t18 = 0.1D1 - x1
      t20 = x1 * t7
      t21 = t18 * x4 * t20
      t22 = t13 * t16 * t21
      t24 = t14 * t1
      t25 = t15 * t3
      t26 = t24 * t25
      t27 = t13 * t26
      t28 = t18 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t38 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t42 = x1 * t41
      t43 = t28 * x4 * t42
      t44 = t27 * t43
      t46 = t11 ** 2
      t47 = t46 * z
      t48 = t47 * t1
      t50 = t3 * t18 * x4
      t52 = 0.18D2 * t48 * t50
      t53 = t47 * t14
      t54 = x1 ** 2
      t56 = t7 ** 2
      t57 = t15 * t54 * t56
      t58 = t53 * t57
      t60 = t14 ** 2
      t62 = t15 ** 2
      t64 = t41 ** 2
      t66 = t62 * t54 * t28 * t64
      t68 = 0.18D2 * t47 * t60 * t66
      t70 = x4 ** 2
      t71 = t15 * t28 * t70
      t73 = 0.18D2 * t53 * t71
      t76 = t13 * t60 * t66
      t78 = t47 * t26
      t79 = t54 * t7
      t80 = t18 * t41
      t81 = t79 * t80
      t83 = 0.18D2 * t78 * t81
      t86 = t27 * t81
      t88 = t13 * t1
      t89 = t88 * t8
      t91 = t88 * t50
      t93 = t13 * t14
      t94 = t93 * t57
      t97 = t15 * x1 * t80
      t99 = 0.18D2 * t53 * t97
      t100 = t48 * t8
      t102 = t12 * t24
      t104 = t28 * t18
      t109 = 0.9D1 * t102 * t25 * t104 * t70 * x4 * t10
      t112 = 0.18D2 * t47 * t16 * t21
      t113 = t93 * t71
      t120 = x4 * t10
      t124 = 0.9D1 * t12 * t60 * t1 * t62 * t3 * t104 * t120 * t54 * t64
      t125 = t93 * t97
      t127 = 0.54D2 * t86 + 0.54D2 * t89 + 0.54D2 * t91 - 0.27D2 * t94 +
     # t99 - 0.18D2 * t100 + t109 + t112 - 0.27D2 * t113 + t124 - 0.90D2
     # * t125
      t132 = t70 * t10
      t142 = t12 * t60
      t161 = 0.9D1 * t102 * t25 * t28 * t132 * t20 + 0.9D1 * t102 * t25 
     #* t18 * t120 * t54 * t56 - 0.9D1 * t142 * t62 * t28 * t120 * t79 *
     # t41 + 0.18D2 * t142 * t62 * t104 * t132 * t42 - 0.36D2 * t78 * t4
     #3 + 0.63D2 * t13 + 0.18D2 * t47 + t68 + 0.63D2 * t76 + t99 + 0.108
     #D3 * t125 + t52 + 0.18D2 * t58
      t170 = t73 - 0.126D3 * t89 - 0.126D3 * t91 + 0.63D2 * t94 + 0.36D2
     # * t100 - t109 + 0.63D2 * t113 - t124 + t83 - 0.126D3 * t86 + t112
     # + 0.108D3 * t22 - 0.126D3 * t44
      rrgq2qgh32J3 = 0.4D1 / 0.9D1 * (wd * (-0.90D2 * t22 + 0.54D2 * t44
     # + t52 - 0.9D1 * t58 - t68 - t73 - 0.27D2 * t13 - 0.27D2 * t76 + t
     #83 - 0.9D1 * t47 + t127) + wd * (t161 + t170)) / t11 / t10 / z / 0
     #.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh32J4
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
      t15 = t3 ** 2
      t16 = t14 * t15
      t18 = 0.1D1 - x1
      t20 = x1 * t7
      t21 = t18 * x4 * t20
      t22 = t13 * t16 * t21
      t24 = t14 * t1
      t25 = t15 * t3
      t26 = t24 * t25
      t27 = t13 * t26
      t28 = t18 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t38 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t42 = x1 * t41
      t43 = t28 * x4 * t42
      t44 = t27 * t43
      t46 = t11 ** 2
      t47 = t46 * z
      t48 = t47 * t1
      t50 = t3 * t18 * x4
      t52 = 0.18D2 * t48 * t50
      t53 = t47 * t14
      t54 = x1 ** 2
      t56 = t7 ** 2
      t57 = t15 * t54 * t56
      t58 = t53 * t57
      t60 = t14 ** 2
      t62 = t15 ** 2
      t64 = t41 ** 2
      t66 = t62 * t54 * t28 * t64
      t68 = 0.18D2 * t47 * t60 * t66
      t70 = x4 ** 2
      t71 = t15 * t28 * t70
      t73 = 0.18D2 * t53 * t71
      t76 = t13 * t60 * t66
      t78 = t47 * t26
      t79 = t54 * t7
      t80 = t18 * t41
      t81 = t79 * t80
      t83 = 0.18D2 * t78 * t81
      t86 = t27 * t81
      t88 = t13 * t1
      t89 = t88 * t8
      t91 = t88 * t50
      t93 = t13 * t14
      t94 = t93 * t57
      t97 = t15 * x1 * t80
      t99 = 0.18D2 * t53 * t97
      t100 = t48 * t8
      t102 = t12 * t24
      t104 = t28 * t18
      t109 = 0.9D1 * t102 * t25 * t104 * t70 * x4 * t10
      t112 = 0.18D2 * t47 * t16 * t21
      t113 = t93 * t71
      t120 = x4 * t10
      t124 = 0.9D1 * t12 * t60 * t1 * t62 * t3 * t104 * t120 * t54 * t64
      t125 = t93 * t97
      t127 = 0.54D2 * t86 + 0.54D2 * t89 + 0.54D2 * t91 - 0.27D2 * t94 +
     # t99 - 0.18D2 * t100 + t109 + t112 - 0.27D2 * t113 + t124 - 0.90D2
     # * t125
      t132 = t12 * t60
      t141 = t70 * t10
      t152 = 0.63D2 * t13 + 0.18D2 * t47 - 0.9D1 * t132 * t62 * t28 * t1
     #20 * t79 * t41 + 0.18D2 * t132 * t62 * t104 * t141 * t42 - 0.36D2 
     #* t78 * t43 + t52 + 0.18D2 * t58 + t73 - 0.126D3 * t89 - 0.126D3 *
     # t91 + 0.63D2 * t94 + 0.36D2 * t100 - t109
      t170 = 0.63D2 * t113 + 0.108D3 * t22 - 0.126D3 * t44 + t68 + 0.63D
     #2 * t76 + t99 + 0.108D3 * t125 - 0.126D3 * t86 + t112 - t124 + 0.9
     #D1 * t102 * t25 * t28 * t141 * t20 + 0.9D1 * t102 * t25 * t18 * t1
     #20 * t54 * t56 + t83
      rrgq2qgh32J4 = 0.4D1 / 0.9D1 * (wd * (-0.90D2 * t22 + 0.54D2 * t44
     # + t52 - 0.9D1 * t58 - t68 - t73 - 0.27D2 * t13 - 0.27D2 * t76 + t
     #83 - 0.9D1 * t47 + t127) + wd * (t152 + t170)) / t11 / t10 / z / 0
     #.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh32J5
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
      t15 = t3 ** 2
      t16 = t14 * t15
      t18 = 0.1D1 - x1
      t20 = x1 * t7
      t21 = t18 * x4 * t20
      t22 = t13 * t16 * t21
      t24 = t14 * t1
      t25 = t15 * t3
      t26 = t24 * t25
      t27 = t13 * t26
      t28 = t18 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t38 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t42 = x1 * t41
      t43 = t28 * x4 * t42
      t44 = t27 * t43
      t46 = t11 ** 2
      t47 = t46 * z
      t48 = t47 * t1
      t50 = t3 * t18 * x4
      t52 = 0.18D2 * t48 * t50
      t53 = t47 * t14
      t54 = x1 ** 2
      t56 = t7 ** 2
      t57 = t15 * t54 * t56
      t58 = t53 * t57
      t60 = t14 ** 2
      t62 = t15 ** 2
      t64 = t41 ** 2
      t66 = t62 * t54 * t28 * t64
      t68 = 0.18D2 * t47 * t60 * t66
      t70 = x4 ** 2
      t71 = t15 * t28 * t70
      t73 = 0.18D2 * t53 * t71
      t76 = t13 * t60 * t66
      t78 = t47 * t26
      t79 = t54 * t7
      t80 = t18 * t41
      t81 = t79 * t80
      t83 = 0.18D2 * t78 * t81
      t86 = t27 * t81
      t88 = t13 * t1
      t89 = t88 * t8
      t91 = t88 * t50
      t93 = t13 * t14
      t94 = t93 * t57
      t97 = t15 * x1 * t80
      t99 = 0.18D2 * t53 * t97
      t100 = t48 * t8
      t102 = t12 * t24
      t104 = t28 * t18
      t109 = 0.9D1 * t102 * t25 * t104 * t70 * x4 * t10
      t112 = 0.18D2 * t47 * t16 * t21
      t113 = t93 * t71
      t120 = x4 * t10
      t124 = 0.9D1 * t12 * t60 * t1 * t62 * t3 * t104 * t120 * t54 * t64
      t125 = t93 * t97
      t127 = 0.54D2 * t86 + 0.54D2 * t89 + 0.54D2 * t91 - 0.27D2 * t94 +
     # t99 - 0.18D2 * t100 + t109 + t112 - 0.27D2 * t113 + t124 - 0.90D2
     # * t125
      t132 = t12 * t60
      t141 = t70 * t10
      t154 = -0.126D3 * t44 + t83 - 0.126D3 * t86 + t112 - t124 - 0.9D1 
     #* t132 * t62 * t28 * t120 * t79 * t41 + 0.18D2 * t132 * t62 * t104
     # * t141 * t42 - 0.36D2 * t78 * t43 + t68 + 0.63D2 * t76 + t99 + 0.
     #108D3 * t125 + 0.9D1 * t102 * t25 * t28 * t141 * t20
      t170 = 0.9D1 * t102 * t25 * t18 * t120 * t54 * t56 + 0.108D3 * t22
     # - 0.126D3 * t89 - 0.126D3 * t91 + 0.63D2 * t94 + 0.36D2 * t100 - 
     #t109 + 0.63D2 * t113 + t52 + 0.18D2 * t58 + t73 + 0.63D2 * t13 + 0
     #.18D2 * t47
      rrgq2qgh32J5 = 0.4D1 / 0.9D1 * (wd * (-0.90D2 * t22 + 0.54D2 * t44
     # + t52 - 0.9D1 * t58 - t68 - t73 - 0.27D2 * t13 - 0.27D2 * t76 + t
     #83 - 0.9D1 * t47 + t127) + wd * (t154 + t170)) / t11 / t10 / z / 0
     #.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh32J6
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
      t5 = t4 ** 2
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t5 * t7
      t10 = 0.1D1 - x1
      t12 = 0.1D1 - x3
      t13 = x1 * t12
      t14 = t10 * x4 * t13
      t17 = t1 * s
      t18 = t5 ** 2
      t21 = t7 ** 2
      t23 = t10 ** 2
      t24 = t23 * t10
      t27 = s * t4
      t28 = t6 * x1
      t31 = t28 * t12
      t33 = s - t27 * t28 * x3 - t27 * t31
      t34 = x4 * t33
      t35 = x1 ** 2
      t39 = cos(x2 * 0.3141592653589793D1)
      t44 = Sqrt(x3 * t12 * x4 * (0.1D1 - x4))
      t47 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t39 * t44
      t48 = t47 ** 2
      t53 = t5 * t4
      t54 = t17 * t53
      t55 = t7 * t6
      t58 = x4 ** 2
      t59 = t58 * t33
      t65 = t12 ** 2
      t70 = t17 * t18
      t73 = t35 * t12
      t78 = t33 * t17
      t82 = t21 * t35 * t23 * t48
      t88 = t3 * t5
      t90 = t10 * t47
      t91 = t7 * x1 * t90
      t94 = t78 * t5
      t101 = x1 * t47
      t105 = t53 * t55
      t106 = t3 * t105
      t108 = t23 * x4 * t101
      t111 = 0.18D2 * t3 * t8 * t14 - 0.9D1 * t17 * t18 * t4 * t21 * t6 
     #* t24 * t34 * t35 * t48 + 0.9D1 * t54 * t55 * t23 * t59 * t13 + 0.
     #9D1 * t54 * t55 * t10 * t34 * t35 * t65 - 0.9D1 * t70 * t21 * t23 
     #* t34 * t73 * t47 + 0.63D2 * t78 * t18 * t82 + 0.18D2 * t3 * t18 *
     # t82 + 0.18D2 * t88 * t91 + 0.108D3 * t94 * t91 + 0.63D2 * t78 + 0
     #.18D2 * t3 + 0.18D2 * t70 * t21 * t24 * t59 * t101 - 0.36D2 * t106
     # * t108
      t115 = t78 * t105
      t118 = t73 * t90
      t124 = t7 * t23 * t58
      t128 = t7 * t35 * t65
      t131 = t3 * t4
      t133 = t6 * t10 * x4
      t146 = t78 * t4
      t153 = 0.108D3 * t78 * t8 * t14 - 0.126D3 * t115 * t108 + 0.18D2 *
     # t106 * t118 - 0.126D3 * t115 * t118 + 0.63D2 * t94 * t124 + 0.18D
     #2 * t88 * t128 + 0.18D2 * t131 * t133 + 0.18D2 * t88 * t124 + 0.36
     #D2 * t131 * t31 - 0.9D1 * t54 * t55 * t24 * t58 * x4 * t33 - 0.126
     #D3 * t146 * t31 + 0.63D2 * t94 * t128 - 0.126D3 * t146 * t133
      rrgq2qgh32J6 = 0.4D1 / 0.9D1 * wd * (t111 + t153) / t1 / t33 / z /
     # 0.3141592653589793D1

      end function
  
 