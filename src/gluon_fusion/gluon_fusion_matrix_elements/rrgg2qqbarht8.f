  
      subroutine rrgg2qqbarht8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh81J1  
      doubleprecision rrgg2qqbarh81J2  
      doubleprecision rrgg2qqbarh81J3  
      doubleprecision rrgg2qqbarh81J4  
      doubleprecision rrgg2qqbarh81J5  
      doubleprecision rrgg2qqbarh81J6  
      doubleprecision rrgg2qqbarht8s1e1  
      doubleprecision rrgg2qqbarht8s1e0  
      doubleprecision rrgg2qqbarht8s1em1  
      doubleprecision rrgg2qqbarht8s1em2  
      doubleprecision rrgg2qqbarht8s1em3  
      doubleprecision rrgg2qqbarht8s1em4  
      doubleprecision rrgg2qqbarht8s2e1  
      doubleprecision rrgg2qqbarht8s2e0  
      doubleprecision rrgg2qqbarht8s2em1  
      doubleprecision rrgg2qqbarht8s2em2  
      doubleprecision rrgg2qqbarht8s2em3  
      doubleprecision rrgg2qqbarht8s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht8s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t17 = x1 * z
      t18 = 0.1D1 - x1 + t17
      t19 = 0.1D1 / t18
      t20 = t16 * t19
      t21 = t3 ** 2
      t22 = t20 * t21
      t25 = log(0.4D1 * t10 * t13 * t22)
      t26 = t25 ** 2
      t27 = -t3
      t28 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, 0.0D
     #0, x4)
      t31 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, 0.0D
     #0, x4)
      t33 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, 0.0D
     #0, x4)
      t37 = 0.3141592653589793D1 * lh
      t43 = lh ** 2
      t45 = 0.3141592653589793D1 ** 2
      t47 = 0.180D3 * t43 - 0.30D2 * t45
      t48 = 0.3141592653589793D1 * t47
      t49 = t7 * t28
      t50 = t48 * t49
      t52 = 0.1D1 / x3
      t54 = 0.1D1 / x1
      t56 = t9 * t13
      t59 = log(0.4D1 * t56 * t22)
      t64 = t59 ** 2
      t71 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, 0.0D
     #0, x4)
      t79 = 0.60D2 * lh * t45 - 0.2884936567583026D3 - 0.120D3 * t43 * l
     #h
      t80 = 0.3141592653589793D1 * t79
      t91 = x2 * x3
      t92 = t91 * t9
      t93 = t16 * t13
      t94 = t21 * t19
      t98 = log(0.4D1 * t92 * t93 * t94)
      t107 = 0.1D1 / x2
      t108 = t107 * t54
      t110 = x2 * t9
      t111 = t110 * t13
      t114 = log(0.4D1 * t111 * t22)
      t115 = t114 ** 2
      t131 = -(0.90D2 * t8 * (-t26 * t28 / 0.2D1 + t25 * t31 - t33) - 0.
     #180D3 * t37 * t7 * (t25 * t28 - t31) - t50) * t52 * t54 / 0.720D3 
     #- (t48 * t7 * (-t31 + t59 * t28) + 0.90D2 * t8 * (t64 * t59 * t28 
     #/ 0.6D1 + t59 * t33 - t64 * t31 / 0.2D1 - t71) - t80 * t49 - 0.180
     #D3 * t37 * t7 * (t59 * t31 - t64 * t28 / 0.2D1 - t33)) * t54 / 0.7
     #20D3 - (0.90D2 * t8 * (-t31 + t98 * t28) + 0.180D3 * t37 * t49) * 
     #t52 * t108 / 0.720D3 - (0.90D2 * t8 * (-t115 * t28 / 0.2D1 + t114 
     #* t31 - t33) - 0.180D3 * t37 * t7 * (t114 * t28 - t31) - t50) * t1
     #07 * t54 / 0.720D3
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t131)
      t134 = -0.1D1 + x2
      t136 = t134 * t1 * s
      t138 = x2 * t1 * s
      t139 = t93 * t134
      t142 = log(-0.4D1 * t91 * t139)
      t143 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t145 = t142 ** 2
      t146 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t149 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t158 = t7 * t146
      t159 = t48 * t158
      t163 = (0.90D2 * t8 * (t142 * t143 - t145 * t146 / 0.2D1 - t149) -
     # 0.180D3 * t37 * t7 * (-t143 + t142 * t146) - t159) * t52 * t107 /
     # 0.1440D4
      t164 = x2 * t13
      t168 = log(-0.4D1 * t164 * t16 * t134)
      t170 = -t143 + t168 * t146
      t173 = t168 ** 2
      t176 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t181 = -t173 * t143 / 0.2D1 - t176 + t173 * t168 * t146 / 0.6D1 + 
     #t168 * t149
      t184 = t80 * t158
      t188 = t168 * t143 - t173 * t146 / 0.2D1 - t149
      t197 = log(-0.4D1 * t92 * t139)
      t207 = (0.90D2 * t8 * (-t143 + t197 * t146) + 0.180D3 * t37 * t158
     #) * t52 * t108 / 0.720D3
      t210 = log(-0.4D1 * t110 * t139)
      t212 = t210 ** 2
      t226 = (0.90D2 * t8 * (t210 * t143 - t212 * t146 / 0.2D1 - t149) -
     # 0.180D3 * t37 * t7 * (-t143 + t210 * t146) - t159) * t107 * t54 /
     # 0.720D3
      t227 = -t163 - (t48 * t7 * t170 + 0.90D2 * t8 * t181 - t184 - 0.18
     #0D3 * t37 * t7 * t188) * t107 / 0.1440D4 - t207 - t226
      t228 = FJET(XB1, XB2, s, 0.0D0, -t136, 0.0D0, t138, 0.0D0, t227)
      t230 = -0.1D1 + x3
      t231 = t230 * s
      t233 = t231 * t1 * x1
      t234 = t1 * t3
      t235 = t231 * t234
      t236 = x3 * x1
      t237 = t2 * t236
      t239 = t2 * t3 * x3
      t240 = t56 * t16
      t241 = x3 * t230
      t245 = log(-0.4D1 * t240 * t241 * t94)
      t246 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, x3,
     # x4)
      t248 = t245 ** 2
      t249 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, x3,
     # x4)
      t252 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, t27, 0.0D0, x3,
     # x4)
      t261 = t7 * t249
      t272 = log(-0.4D1 * t91 * t230 * t19 * t93 * t21 * t9)
      t283 = -(0.90D2 * t8 * (-t245 * t246 + t248 * t249 / 0.2D1 + t252)
     # - 0.180D3 * t37 * t7 * (t246 - t245 * t249) + t48 * t261) * t52 *
     # t54 / 0.720D3 - (0.90D2 * t8 * (t246 - t272 * t249) - 0.180D3 * t
     #37 * t261) * t52 * t108 / 0.720D3
      t284 = FJET(XB1, XB2, s, -t233, t235, t237, -t239, 0.0D0, t283)
      t286 = FJET(XB1, XB2, s, t237, -t239, -t233, t235, 0.0D0, t283)
      t288 = 0.2D1 * t91
      t289 = cos(t11)
      t292 = Sqrt(x2 * t134 * t241)
      t294 = 0.2D1 * t289 * t292
      t296 = t2 * (0.1D1 - x2 - x3 + t288 + t294)
      t298 = t2 * (-x3 + t288 - x2 + t294)
      t299 = t164 * t16
      t303 = log(0.4D1 * t299 * t241 * t134)
      t304 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t306 = t303 ** 2
      t307 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t310 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t319 = t7 * t307
      t329 = log(0.4D1 * t92 * t93 * t134 * t230)
      t340 = -(0.90D2 * t8 * (-t303 * t304 + t306 * t307 / 0.2D1 + t310)
     # - 0.180D3 * t37 * t7 * (t304 - t303 * t307) + t48 * t319) * t52 *
     # t107 / 0.1440D4 - (0.90D2 * t8 * (t304 - t329 * t307) - 0.180D3 *
     # t37 * t319) * t52 * t108 / 0.720D3
      t341 = FJET(XB1, XB2, s, t296, 0.0D0, -t298, 0.0D0, 0.0D0, t340)
      t343 = FJET(XB1, XB2, s, 0.0D0, t138, 0.0D0, -t136, 0.0D0, t227)
      t345 = FJET(XB1, XB2, s, t235, -t233, -t239, t237, 0.0D0, t283)
      t347 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t131)
      t349 = t236 * z
      t350 = t91 * x1
      t351 = t91 * t17
      t356 = Sqrt(x3 * t134 * t18 * x2 * t230)
      t358 = 0.2D1 * t289 * t356
      t362 = t2 * t3 * (-x3 + t236 - t349 + t288 - t350 + t351 - x2 + t3
     #58) * t19
      t363 = x2 * x1
      t365 = 0.1D1 - x1 + t17 - x2 + t363 - t363 * z - x3 + t236 - t349 
     #+ t288 - t350 + t351 + t358
      t368 = t2 * t3 * t365 * t19
      t369 = t1 ** 2
      t374 = s * t369 * x2 * x1 * t3 * t19
      t375 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t27, x2, x3, x4
     #)
      t376 = t91 * t56
      t377 = t21 * t134
      t382 = log(0.4D1 * t376 * t20 * t377 * t230)
      t383 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t27, x2, x3, x4
     #)
      t391 = 0.90D2 * t8 * (-t375 + t382 * t383) + 0.180D3 * t37 * t7 * 
     #t383
      t394 = t391 * t52 * t108 / 0.720D3
      t395 = FJET(XB1, XB2, s, t237, t362, -t233, -t368, -t374, -t394)
      t398 = t52 * t107 * t54
      t401 = FJET(XB1, XB2, s, 0.0D0, t296, 0.0D0, -t298, 0.0D0, t340)
      t403 = t2 * x3
      t404 = t2 * t230
      t405 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t408 = log(-0.4D1 * t93 * t241)
      t409 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t411 = t405 - t408 * t409
      t414 = t408 ** 2
      t417 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t421 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t423 = t414 * t405 / 0.2D1 + t417 - t414 * t408 * t409 / 0.6D1 - t
     #408 * t421
      t426 = t7 * t409
      t427 = t80 * t426
      t431 = -t408 * t405 + t414 * t409 / 0.2D1 + t421
      t437 = (-t48 * t7 * t411 - 0.90D2 * t8 * t423 - t427 + 0.180D3 * t
     #37 * t7 * t431) * t52 / 0.1440D4
      t439 = t16 * x3 * t230
      t442 = log(-0.4D1 * t56 * t439)
      t444 = t442 ** 2
      t455 = t48 * t426
      t459 = (0.90D2 * t8 * (t442 * t405 - t444 * t409 / 0.2D1 - t421) -
     # 0.180D3 * t37 * t7 * (-t405 + t442 * t409) - t455) * t52 * t54 / 
     #0.720D3
      t462 = log(-0.4D1 * t111 * t439)
      t472 = (0.90D2 * t8 * (-t405 + t462 * t409) + 0.180D3 * t37 * t426
     #) * t52 * t108 / 0.720D3
      t475 = log(-0.4D1 * t164 * t439)
      t477 = t475 ** 2
      t480 = t475 * t405 - t477 * t409 / 0.2D1 - t421
      t484 = -t405 + t475 * t409
      t491 = (0.90D2 * t8 * t480 - 0.180D3 * t37 * t7 * t484 - t455) * t
     #52 * t107 / 0.1440D4
      t492 = -t437 - t459 - t472 - t491
      t493 = FJET(XB1, XB2, s, t403, 0.0D0, -t404, 0.0D0, 0.0D0, t492)
      t508 = -t459 - t472 - (-t48 * t7 * t411 - 0.90D2 * t8 * t423 - t42
     #7 + 0.180D3 * t37 * t7 * t431) * t52 / 0.1440D4 - t491
      t509 = FJET(XB1, XB2, s, 0.0D0, -t404, 0.0D0, t403, 0.0D0, t508)
      t512 = t134 * s * t234
      t515 = t2 * t3 * x2 * t19
      t516 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t27, x2, 0.0D0,
     # x4)
      t517 = t20 * t377
      t520 = log(-0.4D1 * t376 * t517)
      t521 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t27, x2, 0.0D0,
     # x4)
      t526 = t7 * t521
      t534 = log(-0.4D1 * t111 * t517)
      t536 = t534 ** 2
      t539 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, t27, x2, 0.0D0,
     # x4)
      t553 = -(0.90D2 * t8 * (t516 - t520 * t521) - 0.180D3 * t37 * t526
     #) * t52 * t108 / 0.720D3 - (0.90D2 * t8 * (-t534 * t516 + t536 * t
     #521 / 0.2D1 + t539) - 0.180D3 * t37 * t7 * (t516 - t534 * t521) + 
     #t48 * t526) * t107 * t54 / 0.720D3
      t554 = FJET(XB1, XB2, s, t5, t512, 0.0D0, -t515, -t374, t553)
      t556 = FJET(XB1, XB2, s, 0.0D0, -t515, t5, t512, -t374, t553)
      t558 = FJET(XB1, XB2, s, 0.0D0, -t298, 0.0D0, t296, 0.0D0, t340)
      t560 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t564 = log(0.4D1 * x3 * t13 * t16)
      t565 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t570 = t564 ** 2
      t573 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t577 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t582 = t7 * t565
      t583 = t80 * t582
      t595 = log(0.4D1 * t93)
      t596 = t595 ** 2
      t597 = t596 * 0.3141592653589793D1
      t601 = t596 * t595 * 0.3141592653589793D1
      t603 = t595 * 0.3141592653589793D1
      t622 = rrgg2qqbarh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t631 = t45 ** 2
      t632 = t43 ** 2
      t638 = t596 ** 2
      t647 = log(0.4D1 * t10 * t93)
      t649 = t647 ** 2
      t660 = t48 * t582
      t666 = log(0.4D1 * t240)
      t671 = t666 ** 2
      t693 = log(0.4D1 * t91 * t240)
      t706 = log(0.4D1 * t110 * t93)
      t708 = t706 ** 2
      t725 = log(0.4D1 * t91 * t93)
      t727 = t725 ** 2
      t743 = log(0.4D1 * t299)
      t748 = t743 ** 2
      t768 = -(t48 * t7 * (t560 - t564 * t565) + 0.90D2 * t8 * (t570 * t
     #560 / 0.2D1 + t573 - t570 * t564 * t565 / 0.6D1 - t564 * t577) + t
     #583 - 0.180D3 * t37 * t7 * (-t564 * t560 + t570 * t565 / 0.2D1 + t
     #577)) * t52 / 0.1440D4 - (-0.90D2 * t597 * lh + t80 - 0.15D2 * t60
     #1 - t603 * t47) * t7 * t560 / 0.1440D4 - (0.180D3 * t603 * lh + 0.
     #45D2 * t597 + t48) * t7 * t577 / 0.1440D4 - (-0.180D3 * t37 - 0.90
     #D2 * t603) * t7 * t573 / 0.1440D4 - t8 * t622 / 0.16D2 - (0.30D2 *
     # t601 * lh + t597 * t47 / 0.2D1 - t603 * t79 + 0.3141592653589793D
     #1 * (0.5769873135166051D3 * lh + t631 + 0.60D2 * t632 - 0.60D2 * t
     #43 * t45) + 0.15D2 / 0.4D1 * t638 * 0.3141592653589793D1) * t7 * t
     #565 / 0.1440D4 - (0.90D2 * t8 * (-t647 * t560 + t649 * t565 / 0.2D
     #1 + t577) - 0.180D3 * t37 * t7 * (t560 - t647 * t565) + t660) * t5
     #2 * t54 / 0.720D3 - (t48 * t7 * (t560 - t666 * t565) + 0.90D2 * t8
     # * (t671 * t560 / 0.2D1 + t573 - t671 * t666 * t565 / 0.6D1 - t666
     # * t577) + t583 - 0.180D3 * t37 * t7 * (-t666 * t560 + t671 * t565
     # / 0.2D1 + t577)) * t54 / 0.720D3 - (0.90D2 * t8 * (t560 - t693 * 
     #t565) - 0.180D3 * t37 * t582) * t52 * t108 / 0.720D3 - (0.90D2 * t
     #8 * (-t706 * t560 + t708 * t565 / 0.2D1 + t577) - 0.180D3 * t37 * 
     #t7 * (t560 - t706 * t565) + t660) * t107 * t54 / 0.720D3 - (0.90D2
     # * t8 * (-t725 * t560 + t727 * t565 / 0.2D1 + t577) - 0.180D3 * t3
     #7 * t7 * (t560 - t725 * t565) + t660) * t52 * t107 / 0.1440D4 - (t
     #48 * t7 * (t560 - t743 * t565) + 0.90D2 * t8 * (t748 * t560 / 0.2D
     #1 + t573 - t748 * t743 * t565 / 0.6D1 - t743 * t577) + t583 - 0.18
     #0D3 * t37 * t7 * (-t743 * t560 + t748 * t565 / 0.2D1 + t577)) * t1
     #07 / 0.1440D4
      t769 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t768)
      t771 = t132 * t131 + t228 * t227 + t284 * t283 + t286 * t283 + t34
     #1 * t340 + t343 * t227 + t345 * t283 + t347 * t131 - t395 * t391 *
     # t398 / 0.720D3 + t401 * t340 + t493 * t492 + t509 * t508 + t554 *
     # t553 + t556 * t553 + t558 * t340 + t769 * t768
      t772 = FJET(XB1, XB2, s, -t298, 0.0D0, t296, 0.0D0, 0.0D0, t340)
      t774 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t131)
      t787 = -t437 - t459 - t472 - (0.90D2 * t8 * t480 - 0.180D3 * t37 *
     # t7 * t484 - t455) * t52 * t107 / 0.1440D4
      t788 = FJET(XB1, XB2, s, -t404, 0.0D0, t403, 0.0D0, 0.0D0, t787)
      t790 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t768)
      t792 = FJET(XB1, XB2, s, t362, t237, -t368, -t233, -t374, -t394)
      t796 = FJET(XB1, XB2, s, -t368, -t233, t362, t237, -t374, -t394)
      t800 = FJET(XB1, XB2, s, -t233, -t368, t237, t362, -t374, -t394)
      t804 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t768)
      t819 = -t163 - (t48 * t7 * t170 + 0.90D2 * t8 * t181 - t184 - 0.18
     #0D3 * t37 * t7 * t188) * t107 / 0.1440D4 - t207 - t226
      t820 = FJET(XB1, XB2, s, -t136, 0.0D0, t138, 0.0D0, 0.0D0, t819)
      t822 = FJET(XB1, XB2, s, t138, 0.0D0, -t136, 0.0D0, 0.0D0, t819)
      t824 = FJET(XB1, XB2, s, 0.0D0, t403, 0.0D0, -t404, 0.0D0, t508)
      t826 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t131)
      t828 = FJET(XB1, XB2, s, t512, t5, -t515, 0.0D0, -t374, t553)
      t830 = FJET(XB1, XB2, s, -t239, t237, t235, -t233, 0.0D0, t283)
      t832 = FJET(XB1, XB2, s, -t515, 0.0D0, t512, t5, -t374, t553)
      t834 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t768)
      t836 = t772 * t340 + t774 * t131 + t788 * t787 + t790 * t768 - t79
     #2 * t391 * t398 / 0.720D3 - t796 * t391 * t398 / 0.720D3 - t800 * 
     #t391 * t398 / 0.720D3 + t804 * t768 + t820 * t819 + t822 * t819 + 
     #t824 * t508 + t826 * t131 + t828 * t553 + t830 * t283 + t832 * t55
     #3 + t834 * t768
      rrgg2qqbarht8s1e1 = t771 + t836

      end function



      doubleprecision function rrgg2qqbarht8s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t17 = x1 * z
      t18 = 0.1D1 - x1 + t17
      t19 = 0.1D1 / t18
      t20 = t16 * t19
      t21 = t4 ** 2
      t22 = t20 * t21
      t25 = log(0.4D1 * t10 * t13 * t22)
      t26 = -t4
      t27 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, 0.0D
     #0, x4)
      t29 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, 0.0D
     #0, x4)
      t33 = 0.3141592653589793D1 * lh
      t34 = t7 * t27
      t36 = 0.180D3 * t33 * t34
      t38 = 0.1D1 / x3
      t40 = 0.1D1 / x1
      t44 = 0.1D1 / x2
      t46 = t38 * t44 * t40
      t49 = x2 * t9
      t50 = t49 * t13
      t53 = log(0.4D1 * t50 * t22)
      t62 = t9 * t13
      t65 = log(0.4D1 * t62 * t22)
      t67 = t65 ** 2
      t70 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, 0.0D
     #0, x4)
      t79 = lh ** 2
      t81 = 0.3141592653589793D1 ** 2
      t83 = 0.180D3 * t79 - 0.30D2 * t81
      t84 = 0.3141592653589793D1 * t83
      t89 = -(0.90D2 * t8 * (t25 * t27 - t29) + t36) * t38 * t40 / 0.720
     #D3 + t8 * t27 * t46 / 0.8D1 - (0.90D2 * t8 * (t53 * t27 - t29) + t
     #36) * t44 * t40 / 0.720D3 - (0.90D2 * t8 * (t65 * t29 - t67 * t27 
     #/ 0.2D1 - t70) - 0.180D3 * t33 * t7 * (-t29 + t65 * t27) - t84 * t
     #34) * t40 / 0.720D3
      t90 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = -0.1D1 + x3
      t93 = t2 * t92
      t94 = t2 * x3
      t95 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t96 = x2 * t13
      t98 = t16 * x3 * t92
      t101 = log(-0.4D1 * t96 * t98)
      t102 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t104 = -t95 + t101 * t102
      t107 = t7 * t102
      t109 = 0.180D3 * t33 * t107
      t113 = (0.90D2 * t8 * t104 + t109) * t38 * t44 / 0.1440D4
      t114 = t16 * t13
      t115 = x3 * t92
      t118 = log(-0.4D1 * t114 * t115)
      t120 = t118 ** 2
      t123 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t124 = t118 * t95 - t120 * t102 / 0.2D1 - t123
      t128 = -t95 + t118 * t102
      t132 = t84 * t107
      t138 = log(-0.4D1 * t62 * t98)
      t146 = (0.90D2 * t8 * (-t95 + t138 * t102) + t109) * t38 * t40 / 0
     #.720D3
      t149 = t8 * t102 * t46 / 0.8D1
      t150 = -t113 - (0.90D2 * t8 * t124 - 0.180D3 * t33 * t7 * t128 - t
     #132) * t38 / 0.1440D4 - t146 + t149
      t151 = FJET(XB1, XB2, s, 0.0D0, -t93, 0.0D0, t94, 0.0D0, t150)
      t154 = t2 * t4 * x3
      t155 = x3 * x1
      t156 = t2 * t155
      t157 = t92 * s
      t158 = t1 * t4
      t159 = t157 * t158
      t161 = t157 * t1 * x1
      t162 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, x3,
     # x4)
      t163 = t62 * t16
      t168 = log(-0.4D1 * t163 * t115 * t19 * t21)
      t169 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, x3,
     # x4)
      t184 = -(0.90D2 * t8 * (t162 - t168 * t169) - 0.180D3 * t33 * t7 *
     # t169) * t38 * t40 / 0.720D3 - t8 * t169 * t46 / 0.8D1
      t185 = FJET(XB1, XB2, s, -t154, t156, t159, -t161, 0.0D0, t184)
      t187 = FJET(XB1, XB2, s, -t161, t159, t156, -t154, 0.0D0, t184)
      t190 = x2 * t1 * s
      t191 = -0.1D1 + x2
      t193 = t191 * t1 * s
      t194 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t195 = x2 * x3
      t196 = t114 * t191
      t199 = log(-0.4D1 * t195 * t196)
      t200 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t205 = t7 * t200
      t207 = 0.180D3 * t33 * t205
      t211 = (0.90D2 * t8 * (-t194 + t199 * t200) + t207) * t38 * t44 / 
     #0.1440D4
      t215 = log(-0.4D1 * t96 * t16 * t191)
      t217 = t215 ** 2
      t220 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t221 = -t215 * t194 + t217 * t200 / 0.2D1 + t220
      t225 = t194 - t215 * t200
      t229 = t84 * t205
      t235 = t8 * t200 * t46 / 0.8D1
      t238 = log(-0.4D1 * t49 * t196)
      t246 = (0.90D2 * t8 * (-t194 + t238 * t200) + t207) * t44 * t40 / 
     #0.720D3
      t247 = -t211 - (-0.90D2 * t8 * t221 + 0.180D3 * t33 * t7 * t225 - 
     #t229) * t44 / 0.1440D4 + t235 - t246
      t248 = FJET(XB1, XB2, s, t190, 0.0D0, -t193, 0.0D0, 0.0D0, t247)
      t259 = (0.90D2 * t8 * t124 - 0.180D3 * t33 * t7 * t128 - t132) * t
     #38 / 0.1440D4
      t260 = -t259 - t113 - t146 + t149
      t261 = FJET(XB1, XB2, s, t94, 0.0D0, -t93, 0.0D0, 0.0D0, t260)
      t263 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t89)
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t89)
      t267 = FJET(XB1, XB2, s, 0.0D0, t94, 0.0D0, -t93, 0.0D0, t150)
      t279 = -t211 - (-0.90D2 * t8 * t221 + 0.180D3 * t33 * t7 * t225 - 
     #t229) * t44 / 0.1440D4 + t235 - t246
      t280 = FJET(XB1, XB2, s, 0.0D0, -t193, 0.0D0, t190, 0.0D0, t279)
      t282 = FJET(XB1, XB2, s, 0.0D0, t190, 0.0D0, -t193, 0.0D0, t279)
      t284 = FJET(XB1, XB2, s, t156, -t154, -t161, t159, 0.0D0, t184)
      t286 = FJET(XB1, XB2, s, -t193, 0.0D0, t190, 0.0D0, 0.0D0, t247)
      t288 = x2 * x1
      t290 = t155 * z
      t291 = 0.2D1 * t195
      t292 = t195 * x1
      t293 = t195 * t17
      t294 = cos(t11)
      t299 = Sqrt(x3 * t191 * t18 * x2 * t92)
      t301 = 0.2D1 * t294 * t299
      t302 = 0.1D1 - x1 + t17 - x2 + t288 - t288 * z - x3 + t155 - t290 
     #+ t291 - t292 + t293 + t301
      t305 = t2 * t4 * t302 * t19
      t309 = t2 * t4 * (-x3 + t155 - t290 + t291 - t292 + t293 - x2 + t3
     #01) * t19
      t310 = t1 ** 2
      t315 = s * t310 * x2 * x1 * t4 * t19
      t316 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t26, x2, x3, x4
     #)
      t319 = t8 * t316 * t46 / 0.8D1
      t320 = FJET(XB1, XB2, s, -t161, -t305, t156, t309, -t315, t319)
      t325 = t316 * t38 * t44 * t40
      t328 = FJET(XB1, XB2, s, -t305, -t161, t309, t156, -t315, t319)
      t333 = FJET(XB1, XB2, s, t309, t156, -t305, -t161, -t315, t319)
      t338 = t90 * t89 + t151 * t150 + t185 * t184 + t187 * t184 + t248 
     #* t247 + t261 * t260 + t263 * t89 + t265 * t89 + t267 * t150 + t28
     #0 * t279 + t282 * t279 + t284 * t184 + t286 * t247 + t320 * 0.3141
     #592653589793D1 * t7 * t325 / 0.8D1 + t328 * 0.3141592653589793D1 *
     # t7 * t325 / 0.8D1 + t333 * 0.3141592653589793D1 * t7 * t325 / 0.8
     #D1
      t339 = FJET(XB1, XB2, s, t156, t309, -t161, -t305, -t315, t319)
      t347 = log(0.4D1 * x3 * t13 * t16)
      t348 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t350 = t347 ** 2
      t351 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t354 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t363 = t7 * t351
      t364 = t84 * t363
      t369 = log(0.4D1 * t114)
      t370 = t369 * 0.3141592653589793D1
      t373 = t369 ** 2
      t374 = t373 * 0.3141592653589793D1
      t380 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t407 = log(0.4D1 * t195 * t114)
      t413 = 0.180D3 * t33 * t363
      t418 = t96 * t16
      t420 = log(0.4D1 * t418)
      t422 = t420 ** 2
      t438 = log(0.4D1 * t10 * t114)
      t452 = log(0.4D1 * t49 * t114)
      t462 = log(0.4D1 * t163)
      t464 = t462 ** 2
      t478 = -(0.90D2 * t8 * (-t347 * t348 + t350 * t351 / 0.2D1 + t354)
     # - 0.180D3 * t33 * t7 * (t348 - t347 * t351) + t364) * t38 / 0.144
     #0D4 - (0.180D3 * t370 * lh + 0.45D2 * t374 + t84) * t7 * t348 / 0.
     #1440D4 - t8 * t380 / 0.16D2 - (-0.90D2 * t374 * lh + 0.31415926535
     #89793D1 * (0.60D2 * lh * t81 - 0.2884936567583026D3 - 0.120D3 * t7
     #9 * lh) - 0.15D2 * t373 * t369 * 0.3141592653589793D1 - t370 * t83
     #) * t7 * t351 / 0.1440D4 - (-0.180D3 * t33 - 0.90D2 * t370) * t7 *
     # t354 / 0.1440D4 - (0.90D2 * t8 * (t348 - t407 * t351) - t413) * t
     #38 * t44 / 0.1440D4 - (0.90D2 * t8 * (-t420 * t348 + t422 * t351 /
     # 0.2D1 + t354) - 0.180D3 * t33 * t7 * (t348 - t420 * t351) + t364)
     # * t44 / 0.1440D4 - (0.90D2 * t8 * (t348 - t438 * t351) - t413) * 
     #t38 * t40 / 0.720D3 - t8 * t351 * t46 / 0.8D1 - (0.90D2 * t8 * (t3
     #48 - t452 * t351) - t413) * t44 * t40 / 0.720D3 - (0.90D2 * t8 * (
     #-t462 * t348 + t464 * t351 / 0.2D1 + t354) - 0.180D3 * t33 * t7 * 
     #(t348 - t462 * t351) + t364) * t40 / 0.720D3
      t479 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t478)
      t483 = Sqrt(x2 * t191 * t115)
      t485 = 0.2D1 * t294 * t483
      t487 = t2 * (0.1D1 - x2 - x3 + t291 + t485)
      t489 = t2 * (-x3 + t291 - x2 + t485)
      t490 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t494 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t498 = log(0.4D1 * t418 * t115 * t191)
      t510 = -t8 * t490 * t46 / 0.8D1 - (0.90D2 * t8 * (t494 - t498 * t4
     #90) - 0.180D3 * t33 * t7 * t490) * t38 * t44 / 0.1440D4
      t511 = FJET(XB1, XB2, s, 0.0D0, t487, 0.0D0, -t489, 0.0D0, t510)
      t520 = -t259 - (0.90D2 * t8 * t104 + t109) * t38 * t44 / 0.1440D4 
     #- t146 + t149
      t521 = FJET(XB1, XB2, s, -t93, 0.0D0, t94, 0.0D0, 0.0D0, t520)
      t523 = FJET(XB1, XB2, s, t487, 0.0D0, -t489, 0.0D0, 0.0D0, t510)
      t525 = FJET(XB1, XB2, s, t159, -t161, -t154, t156, 0.0D0, t184)
      t527 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t89)
      t530 = t191 * s * t158
      t533 = t2 * t4 * x2 * t19
      t534 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t26, x2, 0.0D0,
     # x4)
      t538 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t26, x2, 0.0D0,
     # x4)
      t543 = log(-0.4D1 * t50 * t20 * t21 * t191)
      t555 = -t8 * t534 * t46 / 0.8D1 - (0.90D2 * t8 * (t538 - t543 * t5
     #34) - 0.180D3 * t33 * t7 * t534) * t44 * t40 / 0.720D3
      t556 = FJET(XB1, XB2, s, t3, t530, 0.0D0, -t533, -t315, t555)
      t558 = FJET(XB1, XB2, s, 0.0D0, -t489, 0.0D0, t487, 0.0D0, t510)
      t560 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t478)
      t562 = FJET(XB1, XB2, s, -t489, 0.0D0, t487, 0.0D0, 0.0D0, t510)
      t564 = FJET(XB1, XB2, s, 0.0D0, -t533, t3, t530, -t315, t555)
      t566 = FJET(XB1, XB2, s, t530, t3, -t533, 0.0D0, -t315, t555)
      t568 = FJET(XB1, XB2, s, -t533, 0.0D0, t530, t3, -t315, t555)
      t570 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t478)
      t572 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t478)
      t574 = t339 * 0.3141592653589793D1 * t7 * t325 / 0.8D1 + t479 * t4
     #78 + t511 * t510 + t520 * t521 + t523 * t510 + t525 * t184 + t527 
     #* t89 + t556 * t555 + t558 * t510 + t560 * t478 + t562 * t510 + t5
     #64 * t555 + t566 * t555 + t568 * t555 + t570 * t478 + t572 * t478
      rrgg2qqbarht8s1e0 = t338 + t574

      end function



      doubleprecision function rrgg2qqbarht8s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + x2
      t3 = -0.1D1 + z
      t4 = -0.1D1 + x1
      t5 = t3 * t4
      t6 = t1 * s * t5
      t7 = s * t3
      t8 = t7 * x1
      t12 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t14 = t7 * t4 * x2 * t12
      t15 = t3 ** 2
      t20 = s * t15 * x2 * x1 * t4 * t12
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = 0.3141592653589793D1 * t22
      t24 = -t4
      t25 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.0D0, 
     #x4)
      t26 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t29 = t25 * t26 * t28
      t31 = t23 * t29 / 0.8D1
      t32 = FJET(XB1, XB2, s, t6, t8, -t14, 0.0D0, -t20, -t31)
      t37 = -0.1D1 + x3
      t38 = t37 * s
      t39 = t38 * t5
      t41 = t38 * t3 * x1
      t43 = t7 * t4 * x3
      t45 = t7 * x1 * x3
      t46 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, x3, 
     #x4)
      t47 = 0.1D1 / x3
      t49 = t46 * t47 * t28
      t51 = t23 * t49 / 0.8D1
      t52 = FJET(XB1, XB2, s, t39, -t41, -t43, t45, 0.0D0, -t51)
      t57 = FJET(XB1, XB2, s, -t41, t39, t45, -t43, 0.0D0, -t51)
      t62 = FJET(XB1, XB2, s, t45, -t43, -t41, t39, 0.0D0, -t51)
      t67 = FJET(XB1, XB2, s, 0.0D0, -t14, t8, t6, -t20, -t31)
      t72 = FJET(XB1, XB2, s, -t43, t45, t39, -t41, 0.0D0, -t51)
      t78 = 0.2D1 * x2 * x3
      t79 = x4 * 0.3141592653589793D1
      t80 = cos(t79)
      t82 = x3 * t37
      t84 = Sqrt(x2 * t1 * t82)
      t86 = 0.2D1 * t80 * t84
      t88 = t7 * (-x3 + t78 - x2 + t86)
      t90 = t7 * (0.1D1 - x2 - x3 + t78 + t86)
      t91 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t93 = t91 * t47 * t26
      t95 = t23 * t93 / 0.16D2
      t96 = FJET(XB1, XB2, s, 0.0D0, -t88, 0.0D0, t90, 0.0D0, -t95)
      t101 = FJET(XB1, XB2, s, t90, 0.0D0, -t88, 0.0D0, 0.0D0, -t95)
      t106 = FJET(XB1, XB2, s, 0.0D0, t90, 0.0D0, -t88, 0.0D0, -t95)
      t111 = FJET(XB1, XB2, s, -t14, 0.0D0, t6, t8, -t20, -t31)
      t116 = FJET(XB1, XB2, s, -t88, 0.0D0, t90, 0.0D0, 0.0D0, -t95)
      t121 = FJET(XB1, XB2, s, t8, t6, 0.0D0, -t14, -t20, -t31)
      t126 = t7 * t4
      t127 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.0
     #D0, x4)
      t132 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.0
     #D0, x4)
      t133 = x1 ** 2
      t134 = Sin(t79)
      t135 = t134 ** 2
      t136 = t133 * t135
      t137 = z ** 2
      t138 = 0.1D1 / t137
      t140 = t4 ** 2
      t144 = log(0.4D1 * t136 * t138 * t12 * t140)
      t149 = 0.3141592653589793D1 * lh
      t160 = t23 * t127 * t26 * t28 / 0.8D1 - (0.90D2 * t23 * (-t132 + t
     #144 * t127) + 0.180D3 * t149 * t22 * t127) * t28 / 0.720D3 + t23 *
     # t127 * t47 * t28 / 0.8D1
      t161 = FJET(XB1, XB2, s, t8, -t126, 0.0D0, 0.0D0, 0.0D0, t160)
      t163 = FJET(XB1, XB2, s, -t126, t8, 0.0D0, 0.0D0, 0.0D0, t160)
      t165 = -t32 * 0.3141592653589793D1 * t22 * t29 / 0.8D1 - t52 * 0.3
     #141592653589793D1 * t22 * t49 / 0.8D1 - t57 * 0.3141592653589793D1
     # * t22 * t49 / 0.8D1 - t62 * 0.3141592653589793D1 * t22 * t49 / 0.
     #8D1 - t67 * 0.3141592653589793D1 * t22 * t29 / 0.8D1 - t72 * 0.314
     #1592653589793D1 * t22 * t49 / 0.8D1 - t96 * 0.3141592653589793D1 *
     # t22 * t93 / 0.16D2 - t101 * 0.3141592653589793D1 * t22 * t93 / 0.
     #16D2 - t106 * 0.3141592653589793D1 * t22 * t93 / 0.16D2 - t111 * 0
     #.3141592653589793D1 * t22 * t29 / 0.8D1 - t116 * 0.314159265358979
     #3D1 * t22 * t93 / 0.16D2 - t121 * 0.3141592653589793D1 * t22 * t29
     # / 0.8D1 + t161 * t160 + t163 * t160
      t166 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t8, -t126, 0.0D0, t160)
      t168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t126, t8, 0.0D0, t160)
      t171 = x2 * t3 * s
      t173 = t1 * t3 * s
      t174 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t178 = t23 * t174 * t47 * t26 / 0.16D2
      t179 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t180 = x2 * t135
      t184 = log(-0.4D1 * t180 * t138 * t1)
      t186 = t179 - t184 * t174
      t191 = 0.180D3 * t149 * t22 * t174
      t198 = t23 * t174 * t26 * t28 / 0.8D1
      t199 = t178 - (-0.90D2 * t23 * t186 + t191) * t26 / 0.1440D4 + t19
     #8
      t200 = FJET(XB1, XB2, s, t171, 0.0D0, -t173, 0.0D0, 0.0D0, t199)
      t202 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t206 = log(0.4D1 * x3 * t135 * t138)
      t207 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t214 = 0.180D3 * t149 * t22 * t207
      t219 = t138 * t135
      t221 = log(0.4D1 * t219)
      t222 = t221 * 0.3141592653589793D1
      t230 = t221 ** 2
      t233 = lh ** 2
      t235 = 0.3141592653589793D1 ** 2
      t243 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.0D0, x4)
      t246 = t207 * t47
      t252 = log(0.4D1 * t180 * t138)
      t266 = log(0.4D1 * t136 * t138)
      t277 = -(0.90D2 * t23 * (t202 - t206 * t207) - t214) * t47 / 0.144
     #0D4 - (-0.180D3 * t149 - 0.90D2 * t222) * t22 * t202 / 0.1440D4 - 
     #(0.180D3 * t222 * lh + 0.45D2 * t230 * 0.3141592653589793D1 + 0.31
     #41592653589793D1 * (0.180D3 * t233 - 0.30D2 * t235)) * t22 * t207 
     #/ 0.1440D4 - t23 * t243 / 0.16D2 - t23 * t246 * t26 / 0.16D2 - (0.
     #90D2 * t23 * (t202 - t252 * t207) - t214) * t26 / 0.1440D4 - t23 *
     # t207 * t26 * t28 / 0.8D1 - (0.90D2 * t23 * (t202 - t266 * t207) -
     # t214) * t28 / 0.720D3 - t23 * t246 * t28 / 0.8D1
      t278 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t7, 0.0D0, t277)
      t286 = t178 - (-0.90D2 * t23 * t186 + t191) * t26 / 0.1440D4 + t19
     #8
      t287 = FJET(XB1, XB2, s, 0.0D0, -t173, 0.0D0, t171, 0.0D0, t286)
      t289 = FJET(XB1, XB2, s, -t173, 0.0D0, t171, 0.0D0, 0.0D0, t199)
      t291 = t7 * t37
      t292 = t7 * x3
      t293 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t296 = log(-0.4D1 * t219 * t82)
      t297 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #x3, x4)
      t299 = t293 - t296 * t297
      t304 = 0.180D3 * t149 * t22 * t297
      t308 = t297 * t47
      t311 = t23 * t308 * t26 / 0.16D2
      t314 = t23 * t308 * t28 / 0.8D1
      t315 = -(-0.90D2 * t23 * t299 + t304) * t47 / 0.1440D4 + t311 + t3
     #14
      t316 = FJET(XB1, XB2, s, -t291, 0.0D0, t292, 0.0D0, 0.0D0, t315)
      t324 = -(-0.90D2 * t23 * t299 + t304) * t47 / 0.1440D4 + t311 + t3
     #14
      t325 = FJET(XB1, XB2, s, 0.0D0, -t291, 0.0D0, t292, 0.0D0, t324)
      t327 = FJET(XB1, XB2, s, 0.0D0, t171, 0.0D0, -t173, 0.0D0, t286)
      t329 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t7, 0.0D0, 0.0D0, t277)
      t331 = FJET(XB1, XB2, s, t7, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t277)
      t333 = FJET(XB1, XB2, s, 0.0D0, t292, 0.0D0, -t291, 0.0D0, t324)
      t335 = FJET(XB1, XB2, s, t292, 0.0D0, -t291, 0.0D0, 0.0D0, t315)
      t337 = FJET(XB1, XB2, s, 0.0D0, t7, 0.0D0, 0.0D0, 0.0D0, t277)
      t339 = t166 * t160 + t168 * t160 + t199 * t200 + t278 * t277 + t28
     #7 * t286 + t289 * t199 + t316 * t315 + t325 * t324 + t327 * t286 +
     # t329 * t277 + t331 * t277 + t333 * t324 + t335 * t315 + t337 * t2
     #77
      rrgg2qqbarht8s1em1 = t165 + t339

      end function



      doubleprecision function rrgg2qqbarht8s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t24 = z ** 2
      t27 = Sin(x4 * 0.3141592653589793D1)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 - (-0.180D3 * 0.3141592653589793D1
     # * lh - 0.90D2 * t31 * 0.3141592653589793D1) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t44 = -0.1D1 + x1
      t45 = t2 * t44
      t47 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, -t44, 0.0D0, 0.0
     #D0, x4)
      t50 = t5 * t47 * t15 / 0.8D1
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t50)
      t54 = t4 * t47 * t15
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t50)
      t61 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t63 = t2 * x3
      t65 = t2 * (-0.1D1 + x3)
      t66 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t69 = t5 * t66 * t7 / 0.16D2
      t70 = FJET(XB1, XB2, s, 0.0D0, t63, 0.0D0, -t65, 0.0D0, t69)
      t73 = t4 * t66 * t7
      t77 = x2 * t1 * s
      t80 = (-0.1D1 + x2) * t1 * s
      t81 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D
     #0, x4)
      t84 = t5 * t81 * t11 / 0.16D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t77, 0.0D0, -t80, 0.0D0, t84)
      t88 = t4 * t81 * t11
      t91 = FJET(XB1, XB2, s, 0.0D0, -t80, 0.0D0, t77, 0.0D0, t84)
      t95 = FJET(XB1, XB2, s, 0.0D0, -t65, 0.0D0, t63, 0.0D0, t69)
      t99 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t101 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t50)
      t105 = FJET(XB1, XB2, s, t63, 0.0D0, -t65, 0.0D0, 0.0D0, t69)
      t109 = FJET(XB1, XB2, s, t77, 0.0D0, -t80, 0.0D0, 0.0D0, t84)
      t113 = FJET(XB1, XB2, s, -t65, 0.0D0, t63, 0.0D0, 0.0D0, t69)
      t117 = FJET(XB1, XB2, s, -t80, 0.0D0, t77, 0.0D0, 0.0D0, t84)
      t121 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t50)
      rrgg2qqbarht8s1em2 = t39 * t38 + t41 * t38 + t51 * 0.3141592653589
     #793D1 * t54 / 0.8D1 + t57 * 0.3141592653589793D1 * t54 / 0.8D1 + t
     #61 * t38 + t70 * 0.3141592653589793D1 * t73 / 0.16D2 + t85 * 0.314
     #1592653589793D1 * t88 / 0.16D2 + t91 * 0.3141592653589793D1 * t88 
     #/ 0.16D2 + t95 * 0.3141592653589793D1 * t73 / 0.16D2 + t99 * t38 +
     # t101 * 0.3141592653589793D1 * t54 / 0.8D1 + t105 * 0.314159265358
     #9793D1 * t73 / 0.16D2 + t109 * 0.3141592653589793D1 * t88 / 0.16D2
     # + t113 * 0.3141592653589793D1 * t73 / 0.16D2 + t117 * 0.314159265
     #3589793D1 * t88 / 0.16D2 + t121 * 0.3141592653589793D1 * t54 / 0.8
     #D1

      end function



      doubleprecision function rrgg2qqbarht8s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht8s1em3 = -t9 * 0.3141592653589793D1 * t11 / 0.16D2 - t
     #13 * 0.3141592653589793D1 * t11 / 0.16D2 - t16 * 0.314159265358979
     #3D1 * t11 / 0.16D2 - t19 * 0.3141592653589793D1 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht8s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      rrgg2qqbarht8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht8s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = z ** 2
      t5 = 0.1D1 / t3 / z
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = log(0.4D1 * t9)
      t12 = t11 ** 2
      t13 = t12 * 0.3141592653589793D1
      t16 = 0.3141592653589793D1 ** 2
      t19 = lh ** 2
      t22 = 0.60D2 * lh * t16 - 0.2884936567583026D3 - 0.120D3 * t19 * l
     #h
      t23 = 0.3141592653589793D1 * t22
      t25 = t12 * t11 * 0.3141592653589793D1
      t27 = t11 * 0.3141592653589793D1
      t30 = 0.180D3 * t19 - 0.30D2 * t16
      t33 = s ** 2
      t34 = 0.1D1 / t33
      t36 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t42 = 0.3141592653589793D1 * t30
      t45 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2qqbarh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t66 = t16 ** 2
      t67 = t19 ** 2
      t73 = t12 ** 2
      t78 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t81 = x1 ** 2
      t82 = x3 * t81
      t85 = log(0.4D1 * t82 * t9)
      t87 = t85 ** 2
      t98 = t34 * t78
      t101 = 0.1D1 / x3
      t103 = 0.1D1 / x1
      t106 = t81 * t8
      t107 = t106 * t5
      t109 = log(0.4D1 * t107)
      t114 = t109 ** 2
      t124 = t23 * t98
      t135 = x2 * x3
      t138 = log(0.4D1 * t135 * t107)
      t140 = t135 * t81
      t141 = -0.1D1 + x2
      t142 = t9 * t141
      t145 = log(-0.4D1 * t140 * t142)
      t146 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t148 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t152 = -t78 + t146
      t153 = t34 * t152
      t158 = 0.1D1 / x2
      t159 = t158 * t103
      t162 = x2 * t81
      t165 = log(0.4D1 * t162 * t9)
      t169 = log(-0.4D1 * t162 * t142)
      t171 = t169 ** 2
      t174 = t165 ** 2
      t177 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t188 = -t34 * t152
      t194 = x3 * t5
      t197 = log(0.4D1 * t194 * t8)
      t202 = t197 ** 2
      t224 = log(-0.4D1 * t135 * t142)
      t226 = t224 ** 2
      t231 = log(0.4D1 * t135 * t9)
      t233 = t231 ** 2
      t250 = x2 * t5
      t253 = log(0.4D1 * t250 * t8)
      t255 = t8 * t141
      t258 = log(-0.4D1 * t250 * t255)
      t263 = t258 ** 2
      t266 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t271 = t253 ** 2
      t295 = -(-0.90D2 * t13 * lh + t23 - 0.15D2 * t25 - t27 * t30) * t3
     #4 * t36 / 0.1440D4 - (0.180D3 * t27 * lh + 0.45D2 * t13 + t42) * t
     #34 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t27) * t34 * t53 
     #/ 0.1440D4 - t56 * t57 / 0.16D2 - (0.30D2 * t25 * lh + t13 * t30 /
     # 0.2D1 - t27 * t22 + 0.3141592653589793D1 * (0.5769873135166051D3 
     #* lh + t66 + 0.60D2 * t67 - 0.60D2 * t19 * t16) + 0.15D2 / 0.4D1 *
     # t73 * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t5
     #6 * (t85 * t36 - t87 * t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * 
     #(-t36 + t85 * t78) - t42 * t98) * t101 * t103 / 0.720D3 + (t42 * t
     #34 * (-t36 + t109 * t78) + 0.90D2 * t56 * (-t114 * t36 / 0.2D1 - t
     #53 + t114 * t109 * t78 / 0.6D1 + t109 * t45) - t124 - 0.180D3 * t4
     #8 * t34 * (t109 * t36 - t114 * t78 / 0.2D1 - t45)) * t103 / 0.720D
     #3 + (0.90D2 * t56 * (t138 * t78 - t36 - t145 * t146 + t148) - 0.18
     #0D3 * t48 * t153) * t101 * t159 / 0.720D3 - (0.90D2 * t56 * (-t165
     # * t36 + t169 * t148 + t45 - t171 * t146 / 0.2D1 + t174 * t78 / 0.
     #2D1 - t177) - 0.180D3 * t48 * t34 * (t36 - t148 - t165 * t78 + t16
     #9 * t146) + t42 * t188) * t158 * t103 / 0.720D3 + (t42 * t34 * (-t
     #36 + t197 * t78) + 0.90D2 * t56 * (-t202 * t36 / 0.2D1 - t53 + t20
     #2 * t197 * t78 / 0.6D1 + t197 * t45) - t124 - 0.180D3 * t48 * t34 
     #* (t197 * t36 - t202 * t78 / 0.2D1 - t45)) * t101 / 0.1440D4 + (0.
     #90D2 * t56 * (-t224 * t148 + t226 * t146 / 0.2D1 + t177 + t231 * t
     #36 - t233 * t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (t148 - t22
     #4 * t146 - t36 + t231 * t78) + t42 * t153) * t101 * t158 / 0.1440D
     #4 - (t42 * t34 * (t36 - t253 * t78 - t148 + t258 * t146) + 0.90D2 
     #* t56 * (-t263 * t148 / 0.2D1 - t266 + t263 * t258 * t146 / 0.6D1 
     #+ t258 * t177 + t271 * t36 / 0.2D1 + t53 - t271 * t253 * t78 / 0.6
     #D1 - t253 * t45) + t23 * t188 - 0.180D3 * t48 * t34 * (-t253 * t36
     # + t271 * t78 / 0.2D1 + t45 + t258 * t148 - t263 * t146 / 0.2D1 - 
     #t177)) * t158 / 0.1440D4
      t296 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t295)
      t298 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t295)
      t300 = t2 * x1
      t301 = -0.1D1 + x1
      t302 = t2 * t301
      t304 = 0.1D1 / t3
      t305 = x1 * z
      t306 = -z - x1 + t305
      t307 = 0.1D1 / t306
      t308 = t304 * t307
      t309 = t301 ** 2
      t310 = t308 * t309
      t313 = log(-0.4D1 * t82 * t8 * t310)
      t314 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t316 = t313 ** 2
      t317 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t320 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t329 = t34 * t317
      t330 = t42 * t329
      t336 = log(-0.4D1 * t106 * t310)
      t341 = t336 ** 2
      t344 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t362 = t8 * t304
      t363 = t309 * t307
      t367 = log(-0.4D1 * t140 * t362 * t363)
      t377 = t162 * t8
      t380 = log(-0.4D1 * t377 * t310)
      t382 = t380 ** 2
      t397 = (0.90D2 * t56 * (-t313 * t314 + t316 * t317 / 0.2D1 + t320)
     # - 0.180D3 * t48 * t34 * (t314 - t313 * t317) + t330) * t101 * t10
     #3 / 0.720D3 + (t42 * t34 * (t314 - t336 * t317) + 0.90D2 * t56 * (
     #t341 * t314 / 0.2D1 + t344 - t341 * t336 * t317 / 0.6D1 - t336 * t
     #320) + t23 * t329 - 0.180D3 * t48 * t34 * (-t336 * t314 + t341 * t
     #317 / 0.2D1 + t320)) * t103 / 0.720D3 + (0.90D2 * t56 * (t314 - t3
     #67 * t317) - 0.180D3 * t48 * t329) * t101 * t159 / 0.720D3 - (0.90
     #D2 * t56 * (t380 * t314 - t382 * t317 / 0.2D1 - t320) - 0.180D3 * 
     #t48 * t34 * (-t314 + t380 * t317) - t330) * t158 * t103 / 0.720D3
      t398 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t300, -t302, 0.0D0, t397)
      t400 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t302, t300, 0.0D0, t397)
      t402 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t295)
      t404 = t2 * x3
      t405 = -0.1D1 + x3
      t406 = t2 * t405
      t410 = log(-0.4D1 * t82 * t9 * t405)
      t411 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t413 = t410 ** 2
      t414 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t417 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t426 = t34 * t414
      t436 = log(0.4D1 * t377 * t194 * t405 * t141)
      t437 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 
     #x4)
      t442 = log(-0.4D1 * t377 * t194 * t405)
      t444 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 
     #x4)
      t449 = t34 * (-t437 + t414)
      t460 = log(-0.4D1 * t250 * t8 * x3 * t405)
      t462 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 
     #x4)
      t467 = log(0.4D1 * t135 * t5 * t255 * t405)
      t469 = t467 ** 2
      t472 = t460 ** 2
      t492 = log(-0.4D1 * t194 * t8 * t405)
      t497 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t498 = t492 ** 2
      t519 = (0.90D2 * t56 * (-t410 * t411 + t413 * t414 / 0.2D1 + t417)
     # - 0.180D3 * t48 * t34 * (t411 - t410 * t414) + t42 * t426) * t101
     # * t103 / 0.720D3 + (0.90D2 * t56 * (t436 * t437 - t442 * t414 - t
     #444 + t411) - 0.180D3 * t48 * t449) * t101 * t159 / 0.720D3 + (0.9
     #0D2 * t56 * (-t460 * t411 - t462 + t467 * t444 + t417 - t469 * t43
     #7 / 0.2D1 + t472 * t414 / 0.2D1) - 0.180D3 * t48 * t34 * (-t444 + 
     #t467 * t437 + t411 - t460 * t414) + t42 * t449) * t101 * t158 / 0.
     #1440D4 + (t42 * t34 * (-t492 * t414 + t411) + 0.90D2 * t56 * (t497
     # + t498 * t411 / 0.2D1 - t492 * t417 - t498 * t492 * t414 / 0.6D1)
     # + t23 * t426 - 0.180D3 * t48 * t34 * (-t492 * t411 + t417 + t498 
     #* t414 / 0.2D1)) * t101 / 0.1440D4
      t520 = FJET(XB1, XB2, s, 0.0D0, t404, 0.0D0, -t406, 0.0D0, t519)
      t522 = FJET(XB1, XB2, s, 0.0D0, -t406, 0.0D0, t404, 0.0D0, t519)
      t524 = x2 * x1
      t526 = t2 * t524 * t307
      t528 = t1 * x1
      t529 = t141 * s * t528
      t530 = t1 ** 2
      t535 = s * t530 * x2 * x1 * t301 * t307
      t536 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t539 = t308 * t309 * t141
      t542 = log(0.4D1 * t135 * t106 * t539)
      t543 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t548 = t34 * t543
      t556 = log(0.4D1 * t377 * t539)
      t558 = t556 ** 2
      t561 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t575 = (0.90D2 * t56 * (-t536 + t542 * t543) + 0.180D3 * t48 * t54
     #8) * t101 * t159 / 0.720D3 - (0.90D2 * t56 * (-t556 * t536 + t558 
     #* t543 / 0.2D1 + t561) - 0.180D3 * t48 * t34 * (t536 - t556 * t543
     #) + t42 * t548) * t158 * t103 / 0.720D3
      t576 = FJET(XB1, XB2, s, 0.0D0, -t526, -t302, -t529, t535, t575)
      t578 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t295)
      t580 = FJET(XB1, XB2, s, t300, -t302, 0.0D0, 0.0D0, 0.0D0, t397)
      t582 = FJET(XB1, XB2, s, t404, 0.0D0, -t406, 0.0D0, 0.0D0, t519)
      t584 = x3 * x1
      t585 = t2 * t584
      t587 = t2 * t301 * x3
      t588 = t405 * s
      t589 = t588 * t528
      t591 = t588 * t1 * t301
      t593 = x3 * t405
      t594 = t593 * t363
      t597 = log(0.4D1 * t106 * t304 * t594)
      t598 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t600 = t597 ** 2
      t601 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t604 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t605 = t597 * t598 - t600 * t601 / 0.2D1 - t604
      t609 = -t598 + t597 * t601
      t613 = t34 * t601
      t614 = t42 * t613
      t618 = t162 * t362
      t621 = log(0.4D1 * t618 * t594)
      t630 = (0.90D2 * t56 * (-t598 + t621 * t601) + 0.180D3 * t48 * t61
     #3) * t101 * t159
      t632 = (0.90D2 * t56 * t605 - 0.180D3 * t48 * t34 * t609 - t614) *
     # t101 * t103 / 0.720D3 + t630 / 0.720D3
      t633 = FJET(XB1, XB2, s, t585, -t587, -t589, t591, 0.0D0, t632)
      t635 = t296 * t295 + t298 * t295 + t398 * t397 + t400 * t397 + t40
     #2 * t295 + t520 * t519 + t522 * t519 + t576 * t575 + t578 * t295 +
     # t580 * t397 + t582 * t519 + t633 * t632
      t636 = FJET(XB1, XB2, s, t591, -t589, -t587, t585, 0.0D0, t632)
      t640 = x3 * z
      t641 = t584 * z
      t642 = t135 * z
      t643 = t135 * x1
      t644 = t135 * t305
      t645 = cos(t6)
      t650 = Sqrt(-x3 * t141 * t306 * x2 * t405)
      t652 = 0.2D1 * t645 * t650
      t653 = z + x1 - t305 - x2 * z - t524 + t524 * z - t640 - t584 + t6
     #41 + t642 + t643 - t644 + t135 + t652
      t656 = t2 * x1 * t653 * t307
      t660 = t2 * x1 * (-t640 - t584 + t641 + t642 + t643 - t644 - x2 + 
     #t135 + t652) * t307
      t661 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t666 = log(-0.4D1 * t618 * t593 * t363 * t141)
      t667 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t675 = 0.90D2 * t56 * (t661 - t666 * t667) - 0.180D3 * t48 * t34 *
     # t667
      t678 = t675 * t101 * t159 / 0.720D3
      t679 = FJET(XB1, XB2, s, t591, -t656, -t587, t660, t535, t678)
      t682 = t101 * t158 * t103
      t685 = FJET(XB1, XB2, s, t660, -t587, -t656, t591, t535, t678)
      t689 = FJET(XB1, XB2, s, -t302, t300, 0.0D0, 0.0D0, 0.0D0, t397)
      t691 = FJET(XB1, XB2, s, -t302, -t529, 0.0D0, -t526, t535, t575)
      t693 = FJET(XB1, XB2, s, -t406, 0.0D0, t404, 0.0D0, 0.0D0, t519)
      t706 = (0.90D2 * t56 * t605 - 0.180D3 * t48 * t34 * t609 - t614) *
     # t101 * t103 / 0.720D3 + t630 / 0.720D3
      t707 = FJET(XB1, XB2, s, -t587, t585, t591, -t589, 0.0D0, t706)
      t709 = FJET(XB1, XB2, s, -t587, t660, t591, -t656, t535, t678)
      t713 = FJET(XB1, XB2, s, -t529, -t302, -t526, 0.0D0, t535, t575)
      t715 = FJET(XB1, XB2, s, -t589, t591, t585, -t587, 0.0D0, t706)
      t717 = FJET(XB1, XB2, s, -t526, 0.0D0, -t529, -t302, t535, t575)
      t719 = FJET(XB1, XB2, s, -t656, t591, t660, -t587, t535, t678)
      t723 = t636 * t632 + t679 * t675 * t682 / 0.720D3 + t685 * t675 * 
     #t682 / 0.720D3 + t689 * t397 + t691 * t575 + t693 * t519 + t707 * 
     #t706 + t709 * t675 * t682 / 0.720D3 + t713 * t575 + t715 * t706 + 
     #t717 * t575 + t719 * t675 * t682 / 0.720D3
      rrgg2qqbarht8s2e1 = t635 + t723

      end function



      doubleprecision function rrgg2qqbarht8s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t9 = x3 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t15 = log(0.4D1 * t9 * t12)
      t16 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t18 = t15 ** 2
      t19 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t22 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t26 = 0.3141592653589793D1 * lh
      t32 = lh ** 2
      t34 = 0.3141592653589793D1 ** 2
      t36 = 0.180D3 * t32 - 0.30D2 * t34
      t37 = 0.3141592653589793D1 * t36
      t38 = t4 * t19
      t39 = t37 * t38
      t41 = 0.1D1 / x3
      t44 = t8 * t12
      t46 = log(0.4D1 * t44)
      t47 = t46 * 0.3141592653589793D1
      t50 = t46 ** 2
      t51 = t50 * 0.3141592653589793D1
      t57 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t82 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t83 = x2 * x3
      t84 = -0.1D1 + x2
      t85 = t44 * t84
      t88 = log(-0.4D1 * t83 * t85)
      t89 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t93 = log(0.4D1 * t83 * t44)
      t98 = -t19 + t89
      t104 = 0.1D1 / x2
      t107 = x2 * t8
      t110 = log(0.4D1 * t107 * t12)
      t112 = t110 ** 2
      t115 = t12 * t84
      t118 = log(-0.4D1 * t107 * t115)
      t120 = t118 ** 2
      t123 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t134 = -t4 * t98
      t139 = x1 ** 2
      t140 = x3 * t139
      t143 = log(0.4D1 * t140 * t44)
      t152 = 0.1D1 / x1
      t157 = t41 * t104 * t152
      t160 = x2 * t139
      t163 = log(0.4D1 * t160 * t44)
      t167 = log(-0.4D1 * t160 * t85)
      t178 = t139 * t12
      t181 = log(0.4D1 * t178 * t8)
      t183 = t181 ** 2
      t197 = (0.90D2 * t5 * (t15 * t16 - t18 * t19 / 0.2D1 - t22) - 0.18
     #0D3 * t26 * t4 * (-t16 + t15 * t19) - t39) * t41 / 0.1440D4 - (0.1
     #80D3 * t47 * lh + 0.45D2 * t51 + t37) * t4 * t16 / 0.1440D4 - t5 *
     # t57 / 0.16D2 - (-0.90D2 * t51 * lh + 0.3141592653589793D1 * (0.60
     #D2 * lh * t34 - 0.2884936567583026D3 - 0.120D3 * t32 * lh) - 0.15D
     #2 * t50 * t46 * 0.3141592653589793D1 - t47 * t36) * t4 * t19 / 0.1
     #440D4 - (-0.180D3 * t26 - 0.90D2 * t47) * t4 * t22 / 0.1440D4 + (0
     #.90D2 * t5 * (t82 - t88 * t89 - t16 + t93 * t19) - 0.180D3 * t26 *
     # t4 * t98) * t41 * t104 / 0.1440D4 - (0.90D2 * t5 * (-t110 * t16 +
     # t112 * t19 / 0.2D1 + t22 + t118 * t82 - t120 * t89 / 0.2D1 - t123
     #) - 0.180D3 * t26 * t4 * (t16 - t110 * t19 - t82 + t118 * t89) + t
     #37 * t134) * t104 / 0.1440D4 + (0.90D2 * t5 * (-t16 + t143 * t19) 
     #+ 0.180D3 * t26 * t38) * t41 * t152 / 0.720D3 + t5 * t98 * t157 / 
     #0.8D1 - (0.90D2 * t5 * (t16 - t82 - t163 * t19 + t167 * t89) - 0.1
     #80D3 * t26 * t134) * t104 * t152 / 0.720D3 + (0.90D2 * t5 * (t181 
     #* t16 - t183 * t19 / 0.2D1 - t22) - 0.180D3 * t26 * t4 * (-t16 + t
     #181 * t19) - t39) * t152 / 0.720D3
      t198 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t197)
      t200 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t197)
      t202 = t2 * x1
      t203 = -0.1D1 + x1
      t204 = t2 * t203
      t205 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t207 = 0.1D1 / t6
      t208 = x1 * z
      t209 = -z - x1 + t208
      t210 = 0.1D1 / t209
      t211 = t207 * t210
      t212 = t203 ** 2
      t213 = t211 * t212
      t216 = log(-0.4D1 * t140 * t12 * t213)
      t217 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t222 = t4 * t217
      t224 = 0.180D3 * t26 * t222
      t232 = t160 * t12
      t235 = log(-0.4D1 * t232 * t213)
      t246 = log(-0.4D1 * t178 * t213)
      t248 = t246 ** 2
      t251 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t264 = (0.90D2 * t5 * (t205 - t216 * t217) - t224) * t41 * t152 / 
     #0.720D3 + t5 * t217 * t157 / 0.8D1 - (0.90D2 * t5 * (-t205 + t235 
     #* t217) + t224) * t104 * t152 / 0.720D3 + (0.90D2 * t5 * (-t246 * 
     #t205 + t248 * t217 / 0.2D1 + t251) - 0.180D3 * t26 * t4 * (t205 - 
     #t246 * t217) + t37 * t222) * t152 / 0.720D3
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t202, -t204, 0.0D0, t264)
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t204, t202, 0.0D0, t264)
      t269 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t197)
      t271 = t2 * x3
      t272 = -0.1D1 + x3
      t273 = t2 * t272
      t277 = log(-0.4D1 * t9 * t12 * t272)
      t278 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t280 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t281 = t277 ** 2
      t282 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t293 = t4 * t282
      t298 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 
     #x4)
      t303 = log(0.4D1 * t83 * t8 * t115 * t272)
      t304 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 
     #x4)
      t310 = log(-0.4D1 * t107 * t12 * x3 * t272)
      t315 = -t304 + t282
      t326 = log(-0.4D1 * t140 * t44 * t272)
      t340 = (0.90D2 * t5 * (-t278 * t277 + t280 + t281 * t282 / 0.2D1) 
     #- 0.180D3 * t26 * t4 * (-t277 * t282 + t278) + t37 * t293) * t41 /
     # 0.1440D4 + (0.90D2 * t5 * (-t298 + t303 * t304 + t278 - t310 * t2
     #82) - 0.180D3 * t26 * t4 * t315) * t41 * t104 / 0.1440D4 + (0.90D2
     # * t5 * (t278 - t326 * t282) - 0.180D3 * t26 * t293) * t41 * t152 
     #/ 0.720D3 + t5 * t315 * t157 / 0.8D1
      t341 = FJET(XB1, XB2, s, 0.0D0, t271, 0.0D0, -t273, 0.0D0, t340)
      t343 = FJET(XB1, XB2, s, 0.0D0, -t273, 0.0D0, t271, 0.0D0, t340)
      t345 = x2 * x1
      t347 = t2 * t345 * t210
      t349 = t1 * x1
      t350 = t84 * s * t349
      t351 = t1 ** 2
      t356 = s * t351 * x2 * x1 * t203 * t210
      t357 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t361 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t366 = log(0.4D1 * t232 * t211 * t212 * t84)
      t378 = -t5 * t357 * t157 / 0.8D1 - (0.90D2 * t5 * (t361 - t366 * t
     #357) - 0.180D3 * t26 * t4 * t357) * t104 * t152 / 0.720D3
      t379 = FJET(XB1, XB2, s, 0.0D0, -t347, -t204, -t350, t356, t378)
      t381 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t197)
      t383 = FJET(XB1, XB2, s, t202, -t204, 0.0D0, 0.0D0, 0.0D0, t264)
      t385 = FJET(XB1, XB2, s, t271, 0.0D0, -t273, 0.0D0, 0.0D0, t340)
      t387 = x3 * x1
      t388 = t2 * t387
      t390 = t2 * t203 * x3
      t391 = t272 * s
      t392 = t391 * t349
      t394 = t391 * t1 * t203
      t395 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t402 = log(0.4D1 * t178 * t207 * x3 * t272 * t210 * t212)
      t403 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t405 = -t395 + t402 * t403
      t410 = 0.180D3 * t26 * t4 * t403
      t417 = t5 * t403 * t157 / 0.8D1
      t418 = (0.90D2 * t5 * t405 + t410) * t41 * t152 / 0.720D3 - t417
      t419 = FJET(XB1, XB2, s, t388, -t390, -t392, t394, 0.0D0, t418)
      t421 = t198 * t197 + t200 * t197 + t265 * t264 + t267 * t264 + t26
     #9 * t197 + t341 * t340 + t343 * t340 + t379 * t378 + t381 * t197 +
     # t383 * t264 + t385 * t340 + t419 * t418
      t422 = FJET(XB1, XB2, s, t394, -t392, -t390, t388, 0.0D0, t418)
      t426 = x3 * z
      t427 = t387 * z
      t428 = t83 * z
      t429 = t83 * x1
      t430 = t83 * t208
      t431 = cos(t10)
      t436 = Sqrt(-x3 * t84 * t209 * x2 * t272)
      t438 = 0.2D1 * t431 * t436
      t439 = z + x1 - t208 - x2 * z - t345 + t345 * z - t426 - t387 + t4
     #27 + t428 + t429 - t430 + t83 + t438
      t442 = t2 * x1 * t439 * t210
      t446 = t2 * x1 * (-t426 - t387 + t427 + t428 + t429 - t430 - x2 + 
     #t83 + t438) * t210
      t447 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t450 = t5 * t447 * t157 / 0.8D1
      t451 = FJET(XB1, XB2, s, t394, -t442, -t390, t446, t356, t450)
      t456 = t447 * t41 * t104 * t152
      t459 = FJET(XB1, XB2, s, t446, -t390, -t442, t394, t356, t450)
      t464 = FJET(XB1, XB2, s, -t204, t202, 0.0D0, 0.0D0, 0.0D0, t264)
      t466 = FJET(XB1, XB2, s, -t204, -t350, 0.0D0, -t347, t356, t378)
      t468 = FJET(XB1, XB2, s, -t273, 0.0D0, t271, 0.0D0, 0.0D0, t340)
      t477 = (0.90D2 * t5 * t405 + t410) * t41 * t152 / 0.720D3 - t417
      t478 = FJET(XB1, XB2, s, -t390, t388, t394, -t392, 0.0D0, t477)
      t480 = FJET(XB1, XB2, s, -t390, t446, t394, -t442, t356, t450)
      t485 = FJET(XB1, XB2, s, -t350, -t204, -t347, 0.0D0, t356, t378)
      t487 = FJET(XB1, XB2, s, -t392, t394, t388, -t390, 0.0D0, t477)
      t489 = FJET(XB1, XB2, s, -t347, 0.0D0, -t350, -t204, t356, t378)
      t491 = FJET(XB1, XB2, s, -t442, t394, t446, -t390, t356, t450)
      t496 = t422 * t418 + t451 * 0.3141592653589793D1 * t4 * t456 / 0.8
     #D1 + t459 * 0.3141592653589793D1 * t4 * t456 / 0.8D1 + t464 * t264
     # + t466 * t378 + t468 * t340 + t478 * t477 + t480 * 0.314159265358
     #9793D1 * t4 * t456 / 0.8D1 + t485 * t378 + t487 * t477 + t489 * t3
     #78 + t491 * 0.3141592653589793D1 * t4 * t456 / 0.8D1
      rrgg2qqbarht8s2e0 = t421 + t496

      end function



      doubleprecision function rrgg2qqbarht8s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * 0.3141592653589793D1)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t22 = 0.3141592653589793D1 * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t33 = log(0.4D1 * t9 * t13)
      t34 = t33 * 0.3141592653589793D1
      t42 = t33 ** 2
      t45 = lh ** 2
      t47 = 0.3141592653589793D1 ** 2
      t55 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t58 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t59 = -t17 + t58
      t61 = 0.1D1 / x2
      t65 = x2 * t9
      t68 = log(0.4D1 * t65 * t13)
      t70 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t71 = -0.1D1 + x2
      t75 = log(-0.4D1 * t65 * t13 * t71)
      t80 = -t59
      t88 = 0.1D1 / x1
      t92 = x1 ** 2
      t93 = t92 * t13
      t96 = log(0.4D1 * t93 * t9)
      t108 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 - 
     #(-0.180D3 * t22 - 0.90D2 * t34) * t4 * t6 / 0.1440D4 - (0.180D3 * 
     #lh * t34 + 0.45D2 * t42 * 0.3141592653589793D1 + 0.314159265358979
     #3D1 * (0.180D3 * t45 - 0.30D2 * t47)) * t4 * t17 / 0.1440D4 - t5 *
     # t55 / 0.16D2 + t5 * t59 * t27 * t61 / 0.16D2 - (0.90D2 * t5 * (t6
     # - t68 * t17 - t70 + t75 * t58) - 0.180D3 * t22 * t4 * t80) * t61 
     #/ 0.1440D4 - t5 * t80 * t61 * t88 / 0.8D1 + (0.90D2 * t5 * (-t6 + 
     #t96 * t17) + t25) * t88 / 0.720D3 - t5 * t17 * t27 * t88 / 0.8D1
      t109 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t108)
      t111 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t108)
      t113 = t2 * x1
      t114 = -0.1D1 + x1
      t115 = t2 * t114
      t116 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t121 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t125 = 0.1D1 / (-z - x1 + x1 * z)
      t127 = t114 ** 2
      t131 = log(-0.4D1 * t93 / t7 * t125 * t127)
      t146 = t5 * t116 * t61 * t88 / 0.8D1 + (0.90D2 * t5 * (t121 - t131
     # * t116) - 0.180D3 * t22 * t4 * t116) * t88 / 0.720D3 + t5 * t116 
     #* t27 * t88 / 0.8D1
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t113, -t115, 0.0D0, t146)
      t149 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t115, t113, 0.0D0, t146)
      t151 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t153 = t2 * x3
      t154 = -0.1D1 + x3
      t155 = t2 * t154
      t159 = log(-0.4D1 * t10 * t13 * t154)
      t160 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t162 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x
     #3, x4)
      t172 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 
     #x4)
      t182 = (0.90D2 * t5 * (-t159 * t160 + t162) - 0.180D3 * t22 * t4 *
     # t160) * t27 / 0.1440D4 + t5 * (-t172 + t160) * t27 * t61 / 0.16D2
     # + t5 * t160 * t27 * t88 / 0.8D1
      t183 = FJET(XB1, XB2, s, 0.0D0, t153, 0.0D0, -t155, 0.0D0, t182)
      t185 = FJET(XB1, XB2, s, 0.0D0, -t155, 0.0D0, t153, 0.0D0, t182)
      t189 = t2 * x1 * x2 * t125
      t191 = t1 * x1
      t192 = t71 * s * t191
      t193 = t1 ** 2
      t198 = s * t193 * x2 * x1 * t114 * t125
      t199 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t201 = t199 * t61 * t88
      t203 = t5 * t201 / 0.8D1
      t204 = FJET(XB1, XB2, s, 0.0D0, -t189, -t115, -t192, t198, -t203)
      t209 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t108)
      t211 = FJET(XB1, XB2, s, t113, -t115, 0.0D0, 0.0D0, 0.0D0, t146)
      t213 = FJET(XB1, XB2, s, t153, 0.0D0, -t155, 0.0D0, 0.0D0, t182)
      t216 = t2 * x1 * x3
      t218 = t2 * t114 * x3
      t219 = t154 * s
      t220 = t219 * t191
      t222 = t219 * t1 * t114
      t223 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t225 = t223 * t27 * t88
      t227 = t5 * t225 / 0.8D1
      t228 = FJET(XB1, XB2, s, t216, -t218, -t220, t222, 0.0D0, -t227)
      t233 = FJET(XB1, XB2, s, t222, -t220, -t218, t216, 0.0D0, -t227)
      t238 = FJET(XB1, XB2, s, -t115, t113, 0.0D0, 0.0D0, 0.0D0, t146)
      t240 = FJET(XB1, XB2, s, -t115, -t192, 0.0D0, -t189, t198, -t203)
      t245 = FJET(XB1, XB2, s, -t155, 0.0D0, t153, 0.0D0, 0.0D0, t182)
      t247 = FJET(XB1, XB2, s, -t218, t216, t222, -t220, 0.0D0, -t227)
      t252 = FJET(XB1, XB2, s, -t192, -t115, -t189, 0.0D0, t198, -t203)
      t257 = FJET(XB1, XB2, s, -t220, t222, t216, -t218, 0.0D0, -t227)
      t262 = FJET(XB1, XB2, s, -t189, 0.0D0, -t192, -t115, t198, -t203)
      rrgg2qqbarht8s2em1 = t109 * t108 + t111 * t108 + t147 * t146 + t14
     #9 * t146 + t151 * t108 + t183 * t182 + t185 * t182 - t204 * 0.3141
     #592653589793D1 * t4 * t201 / 0.8D1 + t209 * t108 + t211 * t146 + t
     #213 * t182 - t228 * 0.3141592653589793D1 * t4 * t225 / 0.8D1 - t23
     #3 * 0.3141592653589793D1 * t4 * t225 / 0.8D1 + t238 * t146 - t240 
     #* 0.3141592653589793D1 * t4 * t201 / 0.8D1 + t245 * t182 - t247 * 
     #0.3141592653589793D1 * t4 * t225 / 0.8D1 - t252 * 0.31415926535897
     #93D1 * t4 * t201 / 0.8D1 - t257 * 0.3141592653589793D1 * t4 * t225
     # / 0.8D1 - t262 * 0.3141592653589793D1 * t4 * t201 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht8s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t7 = 0.1D1 / x3
      t11 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t17 = 0.1D1 / x1
      t21 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t26 = z ** 2
      t30 = Sin(x4 * 0.3141592653589793D1)
      t31 = t30 ** 2
      t34 = log(0.4D1 / t26 / z * t31)
      t41 = -t5 * t6 * t7 / 0.16D2 - t5 * (t6 - t11) / x2 / 0.16D2 - t5 
     #* t6 * t17 / 0.8D1 - t5 * t21 / 0.16D2 - (-0.180D3 * 0.31415926535
     #89793D1 * lh - 0.90D2 * t34 * 0.3141592653589793D1) * t4 * t6 / 0.
     #1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t46 = t2 * x1
      t48 = t2 * (-0.1D1 + x1)
      t49 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0
     #, x4)
      t52 = t5 * t49 * t17 / 0.8D1
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t46, -t48, 0.0D0, t52)
      t56 = t4 * t49 * t17
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t46, 0.0D0, t52)
      t63 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t65 = t2 * x3
      t67 = t2 * (-0.1D1 + x3)
      t68 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t71 = t5 * t68 * t7 / 0.16D2
      t72 = FJET(XB1, XB2, s, 0.0D0, t65, 0.0D0, -t67, 0.0D0, t71)
      t75 = t4 * t68 * t7
      t78 = FJET(XB1, XB2, s, 0.0D0, -t67, 0.0D0, t65, 0.0D0, t71)
      t82 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t84 = FJET(XB1, XB2, s, t46, -t48, 0.0D0, 0.0D0, 0.0D0, t52)
      t88 = FJET(XB1, XB2, s, t65, 0.0D0, -t67, 0.0D0, 0.0D0, t71)
      t92 = FJET(XB1, XB2, s, -t67, 0.0D0, t65, 0.0D0, 0.0D0, t71)
      t96 = FJET(XB1, XB2, s, -t48, t46, 0.0D0, 0.0D0, 0.0D0, t52)
      rrgg2qqbarht8s2em2 = t42 * t41 + t44 * t41 + t53 * 0.3141592653589
     #793D1 * t56 / 0.8D1 + t59 * 0.3141592653589793D1 * t56 / 0.8D1 + t
     #63 * t41 + t72 * 0.3141592653589793D1 * t75 / 0.16D2 + t78 * 0.314
     #1592653589793D1 * t75 / 0.16D2 + t82 * t41 + t84 * 0.3141592653589
     #793D1 * t56 / 0.8D1 + t88 * 0.3141592653589793D1 * t75 / 0.16D2 + 
     #t92 * 0.3141592653589793D1 * t75 / 0.16D2 + t96 * 0.31415926535897
     #93D1 * t56 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht8s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht8s2em3 = -t9 * 0.3141592653589793D1 * t11 / 0.16D2 - t
     #13 * 0.3141592653589793D1 * t11 / 0.16D2 - t16 * 0.314159265358979
     #3D1 * t11 / 0.16D2 - t19 * 0.3141592653589793D1 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht8s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6
      rrgg2qqbarht8s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh81J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + t1 * x1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t26 = t10 * t7 * t4 + x2 * x3 + t19
      t27 = t6 * t26
      t31 = s ** 2
      t33 = t1 ** 2
      t34 = t31 * t33
      t35 = x1 ** 2
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t38 = t35 * t37
      t39 = t26 ** 2
      t47 = t31 * t1
      t52 = t20 ** 2
      rrgg2qqbarh81J1 = 0.3D1 / 0.8D1 * wd * (-t2 * t21 + t2 * t27) * (-
     #0.2D1 * t31 - t34 * t38 * t39 + 0.2D1 * t34 * t35 * t37 * t20 * t2
     #6 - 0.2D1 * t47 * t27 + 0.2D1 * t47 * t21 - t34 * t38 * t52) * nf 
     #/ s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + t1 * x1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t26 = t10 * t7 * t4 + x2 * x3 + t19
      t27 = t6 * t26
      t30 = wd * (-t2 * t21 + t2 * t27)
      t31 = s ** 2
      t33 = t1 ** 2
      t34 = t31 * t33
      t35 = x1 ** 2
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t38 = t35 * t37
      t39 = t26 ** 2
      t47 = t31 * t1
      t52 = t20 ** 2
      t58 = 0.1D1 - x1
      t59 = t58 ** 2
      t60 = t10 ** 2
      rrgg2qqbarh81J2 = 0.3D1 / 0.8D1 * (0.2D1 * t30 * (-0.2D1 * t31 - t
     #34 * t38 * t39 + 0.2D1 * t34 * t35 * t37 * t20 * t26 - 0.2D1 * t47
     # * t27 + 0.2D1 * t47 * t21 - t34 * t38 * t52) + t30 * (0.2D1 * t31
     # - 0.2D1 * t34 * t59 * t60 + 0.2D1 * t47 * t58 * t10)) * nf / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + t1 * x1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t26 = t10 * t7 * t4 + x2 * x3 + t19
      t27 = t6 * t26
      t30 = wd * (-t2 * t21 + t2 * t27)
      t31 = s ** 2
      t33 = t1 ** 2
      t34 = t31 * t33
      t35 = x1 ** 2
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t38 = t35 * t37
      t39 = t26 ** 2
      t47 = t31 * t1
      t52 = t20 ** 2
      t58 = 0.1D1 - x1
      t59 = t58 ** 2
      t60 = t10 ** 2
      rrgg2qqbarh81J3 = 0.3D1 / 0.8D1 * (0.3D1 * t30 * (-0.2D1 * t31 - t
     #34 * t38 * t39 + 0.2D1 * t34 * t35 * t37 * t20 * t26 - 0.2D1 * t47
     # * t27 + 0.2D1 * t47 * t21 - t34 * t38 * t52) + 0.2D1 * t30 * (0.2
     #D1 * t31 - 0.2D1 * t34 * t59 * t60 + 0.2D1 * t47 * t58 * t10)) * n
     #f / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + t1 * x1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t26 = t10 * t7 * t4 + x2 * x3 + t19
      t27 = t6 * t26
      t30 = wd * (-t2 * t21 + t2 * t27)
      t31 = s ** 2
      t33 = t1 ** 2
      t34 = t31 * t33
      t35 = x1 ** 2
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t38 = t35 * t37
      t39 = t26 ** 2
      t47 = t31 * t1
      t52 = t20 ** 2
      t58 = 0.1D1 - x1
      t59 = t58 ** 2
      t60 = t10 ** 2
      rrgg2qqbarh81J4 = 0.3D1 / 0.8D1 * (0.4D1 * t30 * (-0.2D1 * t31 - t
     #34 * t38 * t39 + 0.2D1 * t34 * t35 * t37 * t20 * t26 - 0.2D1 * t47
     # * t27 + 0.2D1 * t47 * t21 - t34 * t38 * t52) + 0.3D1 * t30 * (0.2
     #D1 * t31 - 0.2D1 * t34 * t59 * t60 + 0.2D1 * t47 * t58 * t10)) * n
     #f / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + t1 * x1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t26 = t10 * t7 * t4 + x2 * x3 + t19
      t27 = t6 * t26
      t30 = wd * (-t2 * t21 + t2 * t27)
      t31 = s ** 2
      t33 = t1 ** 2
      t34 = t31 * t33
      t35 = x1 ** 2
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t38 = t35 * t37
      t39 = t26 ** 2
      t47 = t31 * t1
      t52 = t20 ** 2
      t58 = 0.1D1 - x1
      t59 = t58 ** 2
      t60 = t10 ** 2
      rrgg2qqbarh81J5 = 0.3D1 / 0.8D1 * (0.5D1 * t30 * (-0.2D1 * t31 - t
     #34 * t38 * t39 + 0.2D1 * t34 * t35 * t37 * t20 * t26 - 0.2D1 * t47
     # * t27 + 0.2D1 * t47 * t21 - t34 * t38 * t52) + 0.4D1 * t30 * (0.2
     #D1 * t31 - 0.2D1 * t34 * t59 * t60 + 0.2D1 * t47 * t58 * t10)) * n
     #f / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * s
      t4 = z + t1 * x1
      t6 = x1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t31 = s ** 2
      t32 = t1 ** 2
      t34 = 0.1D1 - x1
      t35 = t34 ** 2
      t36 = t10 ** 2
      rrgg2qqbarh81J6 = 0.15D2 / 0.8D1 * wd * (-t2 * t6 * (t8 * t4 + x2 
     #* t10 - t19) + t2 * t6 * (t10 * t7 * t4 + x2 * x3 + t19)) * (0.2D1
     # * t31 - 0.2D1 * t31 * t32 * t35 * t36 + 0.2D1 * t31 * t1 * t34 * 
     #t10) * nf / s / z / 0.3141592653589793D1

      end function
  
 