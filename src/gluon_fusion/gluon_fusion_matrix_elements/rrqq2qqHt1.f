  
      subroutine rrqq2qqht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqq2qqH11J1  
      doubleprecision rrqq2qqH11J2  
      doubleprecision rrqq2qqH11J3  
      doubleprecision rrqq2qqH12J1  
      doubleprecision rrqq2qqH12J2  
      doubleprecision rrqq2qqH12J3  
      doubleprecision rrqq2qqht1s1e1  
      doubleprecision rrqq2qqht1s1e0  
      doubleprecision rrqq2qqht1s1em1  
      doubleprecision rrqq2qqht1s1em2  
      doubleprecision rrqq2qqht1s1em3  
      doubleprecision rrqq2qqht1s1em4

      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqq2qqht1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH11J1
      doubleprecision rrqq2qqH11J2
      doubleprecision rrqq2qqH11J3
      doubleprecision rrqq2qqH12J1
      doubleprecision rrqq2qqH12J2
      doubleprecision rrqq2qqH12J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
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
      t20 = rrqq2qqH11J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t40 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t41 = t18 * t40
      t43 = t38 ** 2
      t44 = t43 * t9
      t45 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t46 = t18 * t45
      t53 = 0.3141592653589793D1 * lh
      t54 = t3 * t16
      t55 = t19 * t40
      t61 = 0.3141592653589793D1 ** 2
      t63 = lh ** 2
      t65 = -0.30D2 * t61 + 0.180D3 * t63
      t66 = 0.3141592653589793D1 * t65
      t67 = t66 * t3
      t68 = t16 * t9
      t69 = t68 * t46
      t70 = t67 * t69
      t72 = 0.1D1 / x4
      t78 = log(0.4D1 * t29 * t32 * t33)
      t79 = t78 * 0.3141592653589793D1
      t83 = (-0.180D3 * t53 - 0.90D2 * t79) * t3 * t16
      t87 = t78 ** 2
      t88 = t87 * 0.3141592653589793D1
      t92 = (t66 + 0.180D3 * t79 * lh + 0.45D2 * t88) * t3 * t16
      t108 = (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3 * 
     #t63 * lh + 0.60D2 * lh * t61) - t79 * t65 - 0.90D2 * t88 * lh - 0.
     #15D2 * t87 * t78 * 0.3141592653589793D1) * t3 * t16
      t111 = x3 * t24
      t112 = t26 * t28
      t113 = t111 * t112
      t116 = log(0.4D1 * t113 * t35)
      t117 = t116 * t9
      t123 = t53 * t3
      t127 = 0.1D1 / x3
      t131 = t28 * t30
      t136 = log(0.4D1 * t111 * t26 * t131 * t31 * t33)
      t137 = t136 ** 2
      t138 = t137 * t9
      t141 = t136 * t9
      t155 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41 - t44 * t46 / 0.2D
     #1) - 0.180D3 * t53 * t54 * (-t55 + t39 * t46) - t70) * t72 / 0.720
     #D3 + t83 * t21 / 0.720D3 + t92 * t55 / 0.720D3 + t108 * t19 * t45 
     #/ 0.720D3 + (0.90D2 * t15 * t16 * (t55 - t117 * t46) - 0.180D3 * t
     #123 * t69) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t21 - 
     #t138 * t46 / 0.2D1 + t141 * t41) - 0.180D3 * t53 * t54 * (t141 * t
     #46 - t55) - t70) * t127 / 0.720D3
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
      t176 = rrqq2qqH11J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t15
     #8)
      t180 = t167 ** 2
      t185 = log(-0.4D1 * t27 * t131 * t31 * x4 * t162 * t180)
      t186 = t185 ** 2
      t187 = t186 * t167
      t188 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t15
     #8)
      t189 = t174 * t188
      t192 = t185 * t167
      t193 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t15
     #8)
      t201 = t175 * t193
      t206 = t16 * t167
      t207 = t206 * t189
      t211 = x4 * t162
      t216 = log(-0.4D1 * t113 * t32 * t211 * t180)
      t217 = t216 * t167
      t229 = -(0.90D2 * t15 * t16 * (t175 * t176 + t187 * t189 / 0.2D1 -
     # t192 * t174 * t193) - 0.180D3 * t53 * t54 * (-t192 * t189 + t201)
     # + t67 * t207) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (t217 * t18
     #9 - t201) + 0.180D3 * t123 * t207) * t127 * t72 / 0.720D3
      t230 = FJET(XB1, XB2, s, 0.0D0, t161, t164, -t166, t172, t229)
      t232 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t233 = t18 * t232
      t235 = rrqq2qqH12J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t236 = t19 * t235
      t237 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t238 = t18 * t237
      t246 = t19 * t232
      t251 = t68 * t238
      t252 = t67 * t251
      t284 = -(0.90D2 * t15 * t16 * (t39 * t233 - t236 - t44 * t238 / 0.
     #2D1) - 0.180D3 * t53 * t54 * (t39 * t238 - t246) - t252) * t72 / 0
     #.720D3 + (0.90D2 * t15 * t16 * (t246 - t117 * t238) - 0.180D3 * t1
     #23 * t251) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t138 *
     # t238 / 0.2D1 - t236 + t141 * t233) - 0.180D3 * t53 * t54 * (-t246
     # + t141 * t238) - t252) * t127 / 0.720D3 + t83 * t236 / 0.720D3 + 
     #t92 * t246 / 0.720D3 + t108 * t19 * t237 / 0.720D3
      t285 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t284)
      t287 = rrqq2qqH12J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t15
     #8)
      t289 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t15
     #8)
      t292 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t15
     #8)
      t293 = t174 * t292
      t300 = t175 * t289
      t306 = t206 * t293
      t321 = -(0.90D2 * t15 * t16 * (t175 * t287 - t192 * t174 * t289 + 
     #t187 * t293 / 0.2D1) - 0.180D3 * t53 * t54 * (t300 - t192 * t293) 
     #+ t67 * t306) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (t217 * t293
     # - t300) + 0.180D3 * t123 * t306) * t127 * t72 / 0.720D3
      t322 = FJET(XB1, XB2, s, t161, 0.0D0, -t166, t164, t172, t321)
      t324 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t325 = s * t324
      t326 = t4 * x3
      t327 = t325 * t326
      t328 = -0.1D1 + x3
      t329 = t4 * t328
      t330 = t325 * t329
      t331 = t325 * t7
      t332 = t324 ** 2
      t336 = s * t332 * t11 * t170 * t328
      t338 = t332 ** 2
      t343 = log(-0.4D1 * t113 * t32 * t328 * x4 * t338)
      t344 = t343 * t332
      t346 = 0.1D1 / (-0.2D1 + t324)
      t347 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t348 = t346 * t347
      t350 = t332 * t346
      t351 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t352 = t350 * t351
      t357 = t16 * t332
      t358 = t357 * t348
      t364 = rrqq2qqH11J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t370 = log(-0.4D1 * t113 * t32 * t328 * t338)
      t371 = t370 ** 2
      t372 = t371 * t332
      t375 = t370 * t332
      t391 = (0.90D2 * t15 * t16 * (t344 * t348 - t352) + 0.180D3 * t123
     # * t358) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t350 * t3
     #64 + t372 * t348 / 0.2D1 - t375 * t346 * t351) - 0.180D3 * t53 * t
     #54 * (-t375 * t348 + t352) + t67 * t358) * t127 / 0.720D3
      t392 = FJET(XB1, XB2, s, t327, -t330, -t331, 0.0D0, t336, t391)
      t394 = KAPPA2(x1, x2, x3, t158, z)
      t395 = s * t394
      t396 = t395 * t326
      t397 = t395 * t329
      t398 = t395 * t163
      t399 = t395 * t165
      t400 = t394 ** 2
      t405 = cos(t22)
      t408 = Sqrt(x3 * t328 * t211)
      t413 = s * t400 * t11 * t170 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t405 * t408)
      t417 = t400 ** 2
      t422 = log(0.4D1 * t111 * t112 * t30 * t31 * t328 * t211 * t417)
      t423 = t422 * t400
      t425 = 0.1D1 / (-0.2D1 + t394)
      t426 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t158)
      t427 = t425 * t426
      t429 = t400 * t425
      t430 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t158)
      t436 = t16 * t400
      t440 = 0.90D2 * t15 * t16 * (-t423 * t427 + t429 * t430) - 0.180D3
     # * t123 * t436 * t427
      t444 = FJET(XB1, XB2, s, t396, -t397, t398, -t399, t413, t440 * t1
     #27 * t72 / 0.720D3)
      t446 = t127 * t72
      t449 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t450 = t350 * t449
      t451 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t452 = t346 * t451
      t458 = t357 * t452
      t464 = rrqq2qqH12J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t483 = (0.90D2 * t15 * t16 * (-t450 + t344 * t452) + 0.180D3 * t12
     #3 * t458) * t127 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t350 * t
     #464 + t372 * t452 / 0.2D1 - t375 * t346 * t449) - 0.180D3 * t53 * 
     #t54 * (-t375 * t452 + t450) + t67 * t458) * t127 / 0.720D3
      t484 = FJET(XB1, XB2, s, -t330, t327, 0.0D0, -t331, t336, t483)
      t486 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t158)
      t488 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t158)
      t489 = t425 * t488
      t498 = 0.90D2 * t15 * t16 * (t429 * t486 - t423 * t489) - 0.180D3 
     #* t123 * t436 * t489
      t502 = FJET(XB1, XB2, s, -t397, t396, -t399, t398, t413, t498 * t1
     #27 * t72 / 0.720D3)
      rrqq2qqht1s1e1 = t156 * t155 + t230 * t229 + t285 * t284 + t322 * 
     #t321 + t392 * t391 + t444 * t440 * t446 / 0.720D3 + t484 * t483 + 
     #t502 * t498 * t446 / 0.720D3

      end function



      doubleprecision function rrqq2qqht1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH11J1
      doubleprecision rrqq2qqH11J2
      doubleprecision rrqq2qqH11J3
      doubleprecision rrqq2qqH12J1
      doubleprecision rrqq2qqH12J2
      doubleprecision rrqq2qqH12J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
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
      t20 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t40 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t81 = rrqq2qqH11J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t89 = log(0.4D1 * t29 * t32 * t33)
      t90 = t89 * 0.3141592653589793D1
      t94 = (-0.180D3 * t47 - 0.90D2 * t90) * t3 * t16
      t97 = 0.3141592653589793D1 ** 2
      t99 = lh ** 2
      t105 = t89 ** 2
      t110 = (0.3141592653589793D1 * (-0.30D2 * t97 + 0.180D3 * t99) + 0
     #.180D3 * t90 * lh + 0.45D2 * t105 * 0.3141592653589793D1) * t3 * t
     #16
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
      t136 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t137 = t135 * t136
      t143 = t126 ** 2
      t148 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t143)
      t149 = t148 * t126
      t151 = t126 * t135
      t152 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t164 = -t133 * t137 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t149 *
     # t137 + t151 * t152) - 0.180D3 * t48 * t132 * t137) * t54 / 0.720D
     #3
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t168 = t18 * t167
      t170 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t171 = t19 * t170
      t178 = 0.180D3 * t48 * t49 * t168
      t195 = rrqq2qqH12J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t202 = -(0.90D2 * t15 * t16 * (t39 * t168 - t171) + t178) * t54 / 
     #0.720D3 + t94 * t171 / 0.720D3 + t57 * t168 * t59 / 0.8D1 - (0.90D
     #2 * t15 * t16 * (-t171 + t71 * t168) + t178) * t58 / 0.720D3 + t80
     # * t19 * t195 / 0.8D1 + t110 * t19 * t167 / 0.720D3
      t203 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t202)
      t205 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #7)
      t207 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
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
      t243 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t244 = t242 * t243
      t250 = t234 ** 2
      t255 = log(-0.4D1 * t63 * t26 * t28 * t32 * t230 * t250)
      t256 = t255 * t234
      t258 = t234 * t242
      t259 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
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
      t294 = s * t280 * t11 * t129 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t285 * t289)
      t296 = t15 * t16 * t280
      t298 = 0.1D1 / (-0.2D1 + t274)
      t299 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t117)
      t304 = FJET(XB1, XB2, s, t276, -t277, t278, -t279, t294, t296 * t2
     #98 * t299 * t59 / 0.8D1)
      t306 = t3 * t16
      t308 = t280 * t298
      t314 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t315 = t242 * t314
      t320 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t332 = -t240 * t315 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t256 *
     # t315 + t258 * t320) - 0.180D3 * t48 * t239 * t315) * t58 / 0.720D
     #3
      t333 = FJET(XB1, XB2, s, -t232, t229, 0.0D0, -t233, t238, t332)
      t335 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t117)
      t340 = FJET(XB1, XB2, s, -t277, t276, -t279, t278, t294, t296 * t2
     #98 * t335 * t59 / 0.8D1)
      rrqq2qqht1s1e0 = t115 * t114 + t165 * t164 + t203 * t202 + t224 * 
     #t223 + t272 * t271 + t304 * 0.3141592653589793D1 * t306 * t308 * t
     #299 * t58 * t54 / 0.8D1 + t333 * t332 + t340 * 0.3141592653589793D
     #1 * t306 * t308 * t335 * t58 * t54 / 0.8D1

      end function



      doubleprecision function rrqq2qqht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH11J1
      doubleprecision rrqq2qqH11J2
      doubleprecision rrqq2qqH11J3
      doubleprecision rrqq2qqH12J1
      doubleprecision rrqq2qqH12J2
      doubleprecision rrqq2qqH12J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
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
      t21 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t22 = 0.1D1 / x3
      t27 = rrqq2qqH11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
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
      t83 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t65)
      t85 = t82 * t83 * t57
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t17 * t85 / 0
     #.8D1)
      t90 = t3 * t16
      t94 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t99 = rrqq2qqH12J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t110 = t17 * t20 * t94 * t22 / 0.8D1 + t17 * t20 * t99 / 0.8D1 + t
     #53 * t20 * t94 / 0.720D3 + t17 * t20 * t94 * t57 / 0.8D1
      t111 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t113 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t65
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
      t139 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t141 = t138 * t139 * t22
      t144 = FJET(XB1, XB2, s, t126, -t129, -t130, 0.0D0, t135, -t17 * t
     #141 / 0.8D1)
      t149 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t151 = t138 * t149 * t22
      t154 = FJET(XB1, XB2, s, -t129, t126, 0.0D0, -t130, t135, -t17 * t
     #151 / 0.8D1)
      rrqq2qqht1s1em1 = t63 * t62 - t88 * 0.3141592653589793D1 * t90 * t
     #85 / 0.8D1 + t111 * t110 - t118 * 0.3141592653589793D1 * t90 * t11
     #5 / 0.8D1 - t144 * 0.3141592653589793D1 * t90 * t141 / 0.8D1 - t15
     #4 * 0.3141592653589793D1 * t90 * t151 / 0.8D1

      end function



      doubleprecision function rrqq2qqht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH11J1
      doubleprecision rrqq2qqH11J2
      doubleprecision rrqq2qqH11J3
      doubleprecision rrqq2qqH12J1
      doubleprecision rrqq2qqH12J2
      doubleprecision rrqq2qqH12J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
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
      t21 = rrqq2qqH11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t17 * t20 * t
     #21 / 0.8D1)
      t28 = t16 * t9
      t32 = rrqq2qqH12J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t36 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t17 * t20 * t
     #32 / 0.8D1)
      rrqq2qqht1s1em2 = t25 * 0.3141592653589793D1 * t3 * t28 * t19 * t2
     #1 / 0.8D1 + t36 * 0.3141592653589793D1 * t3 * t28 * t19 * t32 / 0.
     #8D1

      end function



      doubleprecision function rrqq2qqht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH11J1
      doubleprecision rrqq2qqH11J2
      doubleprecision rrqq2qqH11J3
      doubleprecision rrqq2qqH12J1
      doubleprecision rrqq2qqH12J2
      doubleprecision rrqq2qqH12J3
      rrqq2qqht1s1em3 = 0.0D0

      end function



      doubleprecision function rrqq2qqht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH11J1
      doubleprecision rrqq2qqH11J2
      doubleprecision rrqq2qqH11J3
      doubleprecision rrqq2qqH12J1
      doubleprecision rrqq2qqH12J2
      doubleprecision rrqq2qqH12J3
      rrqq2qqht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqq2qqH11J1
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
      t9 = 0.1D1 - x1
      t10 = 0.1D1 - x4
      t19 = cos(x2 * 0.3141592653589793D1)
      t20 = 0.1D1 - x3
      t24 = Sqrt(x3 * t20 * x4 * t10)
      t27 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t19 * t24
      t37 = t3 ** 2
      t39 = t6 ** 2
      t41 = x1 ** 2
      t42 = t9 ** 2
      t44 = t27 ** 2
      t49 = x4 ** 2
      t54 = t20 ** 2
      rrqq2qqH11J1 = -0.16D2 / 0.27D2 * wd * (-0.8D1 * t7 * x1 * x3 * t9
     # * t10 + 0.8D1 * t7 * x1 * t9 * t27 + 0.4D1 * t7 * t9 * x4 * x1 * 
     #t20 - 0.4D1 * t1 - 0.4D1 * t1 * t37 * t39 * t41 * t42 * t44 - 0.3D
     #1 * t4 * t6 * t42 * t49 - 0.3D1 * t4 * t6 * t41 * t54) / s / z / 0
     #.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqq2qqH11J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = 0.1D1 - x1
      t11 = t3 ** 2
      t12 = t1 * t11
      t13 = t5 ** 2
      t14 = t12 * t13
      t15 = t6 * x4
      t16 = 0.1D1 - x3
      t17 = x1 * t16
      t28 = t1 * t11 * t3 * t13 * t5
      t29 = t6 ** 2
      t30 = t29 * x4
      t34 = cos(x2 * 0.3141592653589793D1)
      t36 = 0.1D1 - x4
      t39 = Sqrt(x3 * t16 * x4 * t36)
      t42 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t39
      t54 = x1 ** 2
      t55 = t54 * t16
      t61 = x4 ** 2
      t70 = t11 ** 2
      t72 = t13 ** 2
      t75 = t42 ** 2
      t82 = t16 ** 2
      t86 = 0.4D1 * t1 - 0.6D1 * t4 * t5 * t6 * x4 + 0.6D1 * t14 * t15 *
     # t17 - 0.6D1 * t4 * t5 * x1 * t16 - 0.6D1 * t28 * t30 * x1 * t42 -
     # t14 * t15 * x1 * x3 + 0.8D1 * t14 * x1 * t6 * t42 - 0.6D1 * t28 *
     # t55 * t6 * t42 + 0.3D1 * t12 * t13 * t29 * t61 - t14 * t30 * t36 
     #- t14 * t17 * t6 * t36 + 0.4D1 * t1 * t70 * t72 * t54 * t29 * t75 
     #- t14 * t55 * x3 + 0.3D1 * t12 * t13 * t54 * t82
      rrqq2qqH11J2 = -0.16D2 / 0.27D2 * wd * t86 / s / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrqq2qqH11J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t6 = t4 * t5
      t9 = t2 ** 2
      t11 = t4 ** 2
      t12 = t1 * t9 * t11
      t17 = cos(x2 * 0.3141592653589793D1)
      t18 = 0.1D1 - x3
      t20 = 0.1D1 - x4
      t23 = Sqrt(x3 * t18 * x4 * t20)
      t26 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t17 * t23
      t33 = t1 * t9 * t2 * t11 * t4
      t34 = x1 ** 2
      t35 = t34 * t18
      t36 = t5 * t26
      t39 = t5 ** 2
      t40 = t39 * x4
      t41 = x1 * t26
      t48 = t9 ** 2
      t50 = t11 ** 2
      t53 = t26 ** 2
      t68 = t4 * x1
      t77 = -t3 * t6 * x4 + 0.2D1 * t12 * x1 * t5 * t26 - t33 * t35 * t3
     #6 - t33 * t40 * t41 + t1 + t12 * x1 * t18 * t5 * t20 + t1 * t48 * 
     #t50 * t34 * t39 * t53 - t33 * t39 * t20 * t41 - t3 * t6 * t20 - t3
     #3 * t34 * x3 * t36 + t12 * t40 * t20 + t12 * t35 * x3 - t3 * t68 *
     # t18 + t12 * t5 * x4 * x1 * x3 - t3 * t68 * x3
      rrqq2qqH11J3 = -0.16D2 / 0.27D2 * wd * t77 / s / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrqq2qqH12J1
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
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t32 = t3 ** 2
      t34 = t6 ** 2
      t36 = x1 ** 2
      t37 = t8 ** 2
      t39 = t22 ** 2
      t44 = x4 ** 2
      t49 = t14 ** 2
      rrqq2qqH12J1 = -0.16D2 / 0.27D2 * wd * (0.8D1 * t7 * x1 * t8 * t22
     # + 0.4D1 * t7 * t8 * x4 * x1 * t14 - 0.4D1 * t1 - 0.4D1 * t1 * t32
     # * t34 * t36 * t37 * t39 - 0.3D1 * t4 * t6 * t37 * t44 - 0.3D1 * t
     #4 * t6 * t36 * t49) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqq2qqH12J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = 0.1D1 - x1
      t11 = t3 ** 2
      t12 = t1 * t11
      t13 = t5 ** 2
      t14 = t12 * t13
      t15 = t6 * x4
      t16 = 0.1D1 - x3
      t17 = x1 * t16
      t28 = t1 * t11 * t3 * t13 * t5
      t29 = t6 ** 2
      t30 = t29 * x4
      t34 = cos(x2 * 0.3141592653589793D1)
      t36 = 0.1D1 - x4
      t39 = Sqrt(x3 * t16 * x4 * t36)
      t42 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t39
      t54 = x1 ** 2
      t55 = t54 * t16
      t61 = x4 ** 2
      t70 = t11 ** 2
      t72 = t13 ** 2
      t75 = t42 ** 2
      t82 = t16 ** 2
      t86 = 0.4D1 * t1 - 0.6D1 * t4 * t5 * t6 * x4 + 0.6D1 * t14 * t15 *
     # t17 - 0.6D1 * t4 * t5 * x1 * t16 - 0.6D1 * t28 * t30 * x1 * t42 -
     # t14 * t15 * x1 * x3 + 0.8D1 * t14 * x1 * t6 * t42 - 0.6D1 * t28 *
     # t55 * t6 * t42 + 0.3D1 * t12 * t13 * t29 * t61 - t14 * t30 * t36 
     #- t14 * t17 * t6 * t36 + 0.4D1 * t1 * t70 * t72 * t54 * t29 * t75 
     #- t14 * t55 * x3 + 0.3D1 * t12 * t13 * t54 * t82
      rrqq2qqH12J2 = -0.16D2 / 0.27D2 * wd * t86 / s / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrqq2qqH12J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t6 = t4 * t5
      t9 = t2 ** 2
      t11 = t4 ** 2
      t12 = t1 * t9 * t11
      t17 = cos(x2 * 0.3141592653589793D1)
      t18 = 0.1D1 - x3
      t20 = 0.1D1 - x4
      t23 = Sqrt(x3 * t18 * x4 * t20)
      t26 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t17 * t23
      t33 = t1 * t9 * t2 * t11 * t4
      t34 = x1 ** 2
      t35 = t34 * t18
      t36 = t5 * t26
      t39 = t5 ** 2
      t40 = t39 * x4
      t41 = x1 * t26
      t48 = t9 ** 2
      t50 = t11 ** 2
      t53 = t26 ** 2
      t68 = t4 * x1
      t77 = -t3 * t6 * x4 + 0.2D1 * t12 * x1 * t5 * t26 - t33 * t35 * t3
     #6 - t33 * t40 * t41 + t1 + t12 * x1 * t18 * t5 * t20 + t1 * t48 * 
     #t50 * t34 * t39 * t53 - t33 * t39 * t20 * t41 - t3 * t6 * t20 - t3
     #3 * t34 * x3 * t36 + t12 * t40 * t20 + t12 * t35 * x3 - t3 * t68 *
     # t18 + t12 * t5 * x4 * x1 * x3 - t3 * t68 * x3
      rrqq2qqH12J3 = -0.16D2 / 0.27D2 * wd * t77 / s / z / 0.31415926535
     #89793D1

      end function
  
 