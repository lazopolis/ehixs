  
      subroutine rrgg2gght7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh71J1  
      doubleprecision rrgg2ggh71J2  
      doubleprecision rrgg2ggh71J3  
      doubleprecision rrgg2ggh71J4  
      doubleprecision rrgg2ggh71J5  
      doubleprecision rrgg2ggh71J6  
      doubleprecision rrgg2gght7s1e1  
      doubleprecision rrgg2gght7s1e0  
      doubleprecision rrgg2gght7s1em1  
      doubleprecision rrgg2gght7s1em2  
      doubleprecision rrgg2gght7s1em3  
      doubleprecision rrgg2gght7s1em4  
      doubleprecision rrgg2gght7s2e1  
      doubleprecision rrgg2gght7s2e0  
      doubleprecision rrgg2gght7s2em1  
      doubleprecision rrgg2gght7s2em2  
      doubleprecision rrgg2gght7s2em3  
      doubleprecision rrgg2gght7s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght7s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = 0.3141592653589793D1 * t7
      t11 = lh ** 2
      t12 = 0.180D3 * t11
      t13 = 0.3141592653589793D1 ** 2
      t14 = 0.30D2 * t13
      t15 = -t12 + t14
      t16 = t15 * t5
      t17 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t18 = 0.3141592653589793D1 * t17
      t19 = t16 * t18
      t20 = t5 * 0.3141592653589793D1
      t21 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t25 = x4 * 0.3141592653589793D1
      t26 = Sin(t25)
      t27 = t26 ** 2
      t28 = x3 * t27
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t28 * t30 * t32)
      t37 = cos(t25)
      t39 = Sqrt(-x3 * t31)
      t43 = 0.1D1 / (-0.1D1 + 0.2D1 * t37 * t39 - x3)
      t47 = log(0.4D1 * t28 * t30)
      t50 = t47 ** 2
      t52 = t36 ** 2
      t61 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t65 = 0.60D2 * lh * t13
      t67 = 0.120D3 * t11 * lh
      t68 = -t65 + 0.2884936567583026D3 + t67
      t69 = t68 * t5
      t70 = t69 * t18
      t71 = 0.3141592653589793D1 * t21
      t87 = 0.1D1 / x3
      t90 = t30 * t27
      t92 = log(0.4D1 * t90)
      t93 = t92 ** 2
      t96 = t93 * t92
      t117 = rrgg2ggh71J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t126 = t13 ** 2
      t127 = t11 ** 2
      t131 = t93 ** 2
      t137 = x1 ** 2
      t138 = x3 * t137
      t139 = t90 * t32
      t142 = log(-0.4D1 * t138 * t139)
      t144 = t142 ** 2
      t151 = log(0.4D1 * t138 * t90)
      t153 = t151 ** 2
      t169 = 0.3141592653589793D1 * (t17 + t17 * t43)
      t170 = t16 * t169
      t173 = 0.1D1 / x1
      t176 = t137 * t27
      t177 = t176 * t30
      t179 = log(0.4D1 * t177)
      t184 = t179 ** 2
      t204 = x2 ** 2
      t205 = t204 * x3
      t206 = t205 * t177
      t208 = log(0.4D1 * t206)
      t210 = t137 * t204
      t211 = t210 * x3
      t214 = log(-0.4D1 * t211 * t139)
      t225 = 0.1D1 / x2
      t226 = t173 * t225
      t231 = log(0.4D1 * t210 * t90)
      t233 = t231 ** 2
      t250 = log(-0.4D1 * t205 * t139)
      t252 = t250 ** 2
      t259 = log(0.4D1 * t205 * t90)
      t261 = t259 ** 2
      t279 = t204 * t27
      t282 = log(0.4D1 * t279 * t30)
      t287 = t282 ** 2
      t307 = ((0.180D3 * t6 * t8 + t19 - 0.90D2 * t20 * t21) * (-t36 * t
     #43 - t47) - 0.90D2 * t20 * t17 * (-t50 * t47 / 0.6D1 - t52 * t36 *
     # t43 / 0.6D1) + (t16 * t8 - 0.90D2 * t20 * t61 + t70 + 0.180D3 * t
     #6 * t71) * (t43 + 0.1D1) + (-0.90D2 * t20 * t7 + 0.180D3 * t6 * t1
     #8) * (t52 * t43 / 0.2D1 + t50 / 0.2D1)) * t87 / 0.2880D4 + (0.90D2
     # * t93 * lh - t65 + 0.2884936567583026D3 + t67 + 0.15D2 * t96 - t9
     #2 * t15) * t5 * t8 / 0.2880D4 + (-0.180D3 * t92 * lh - 0.45D2 * t9
     #3 - t12 + t14) * t5 * t71 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t9
     #2) * t5 * 0.3141592653589793D1 * t61 / 0.2880D4 - t20 * t117 / 0.3
     #2D2 + (-0.30D2 * t96 * lh + t93 * t15 / 0.2D1 - t92 * t68 - 0.5769
     #873135166051D3 * lh - t126 - 0.60D2 * t127 + 0.60D2 * t11 * t13 - 
     #0.15D2 / 0.4D1 * t131) * t5 * t18 / 0.2880D4 + (-0.90D2 * t20 * ((
     #-t142 * t7 + t144 * t17 / 0.2D1 + t21) * t43 - t151 * t7 + t153 * 
     #t17 / 0.2D1 + t21) + 0.180D3 * t6 * 0.3141592653589793D1 * ((t7 - 
     #t142 * t17) * t43 + t7 - t151 * t17) + t170) * t87 * t173 / 0.1440
     #D4 - (t16 * 0.3141592653589793D1 * (-t7 + t179 * t17) - 0.90D2 * t
     #20 * (-t184 * t7 / 0.2D1 - t61 + t184 * t179 * t17 / 0.6D1 + t179 
     #* t21) - t70 + 0.180D3 * t6 * 0.3141592653589793D1 * (t179 * t7 - 
     #t184 * t17 / 0.2D1 - t21)) * t173 / 0.1440D4 + (-0.90D2 * t20 * (t
     #7 - t208 * t17 + (t7 - t214 * t17) * t43) + 0.180D3 * t6 * t169) *
     # t87 * t226 / 0.720D3 + (-0.90D2 * t20 * (-t231 * t7 + t233 * t17 
     #/ 0.2D1 + t21) + 0.180D3 * t6 * 0.3141592653589793D1 * (t7 - t231 
     #* t17) + t19) * t173 * t225 / 0.720D3 + (-0.90D2 * t20 * ((-t250 *
     # t7 + t252 * t17 / 0.2D1 + t21) * t43 - t259 * t7 + t261 * t17 / 0
     #.2D1 + t21) + 0.180D3 * t6 * 0.3141592653589793D1 * ((t7 - t250 * 
     #t17) * t43 + t7 - t259 * t17) + t170) * t87 * t225 / 0.1440D4 + (t
     #16 * 0.3141592653589793D1 * (t7 - t282 * t17) - 0.90D2 * t20 * (t2
     #87 * t7 / 0.2D1 + t61 - t287 * t282 * t17 / 0.6D1 - t282 * t21) + 
     #t70 + 0.180D3 * t6 * 0.3141592653589793D1 * (-t282 * t7 + t287 * t
     #17 / 0.2D1 + t21)) * t225 / 0.1440D4
      t308 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t307)
      t310 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t307)
      t312 = t2 * x1
      t313 = -0.1D1 + x1
      t314 = t2 * t313
      t315 = t138 * t27
      t316 = t313 ** 2
      t317 = t30 * t316
      t318 = x1 * z
      t319 = 0.1D1 - x1 + t318
      t320 = 0.1D1 / t319
      t321 = t317 * t320
      t324 = log(0.4D1 * t315 * t321)
      t325 = -t313
      t326 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, t325, 0.0D0, 0.0D0
     #, x4)
      t328 = t324 ** 2
      t329 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t325, 0.0D0, 0.0D0
     #, x4)
      t332 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, t325, 0.0D0, 0.0D0
     #, x4)
      t334 = t317 * t320 * t32
      t337 = log(-0.4D1 * t315 * t334)
      t339 = t337 ** 2
      t343 = x3 * x1
      t344 = t343 * z
      t347 = x3 * t319
      t349 = Sqrt(-t347 * t31)
      t353 = 0.1D1 / (-0.2D1 * t344 + 0.2D1 * t343 - 0.1D1 + 0.2D1 * t37
     # * t349 - x3)
      t368 = 0.3141592653589793D1 * (-t329 - t329 * t353)
      t376 = log(0.4D1 * t176 * t321)
      t381 = t376 ** 2
      t384 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, t325, 0.0D0, 0.0D0
     #, x4)
      t392 = 0.3141592653589793D1 * t329
      t404 = t316 * t320
      t408 = log(0.4D1 * t211 * t90 * t404)
      t410 = t205 * t176
      t413 = log(-0.4D1 * t410 * t334)
      t426 = t210 * t27
      t429 = log(0.4D1 * t426 * t321)
      t431 = t429 ** 2
      t447 = (-0.90D2 * t20 * (t324 * t326 - t328 * t329 / 0.2D1 - t332 
     #- (-t337 * t326 + t339 * t329 / 0.2D1 + t332) * t353) + 0.180D3 * 
     #t6 * 0.3141592653589793D1 * (-t326 + t324 * t329 - (t326 - t337 * 
     #t329) * t353) + t16 * t368) * t87 * t173 / 0.1440D4 - (t16 * 0.314
     #1592653589793D1 * (t326 - t376 * t329) - 0.90D2 * t20 * (t381 * t3
     #26 / 0.2D1 + t384 - t381 * t376 * t329 / 0.6D1 - t376 * t332) + t6
     #9 * t392 + 0.180D3 * t6 * 0.3141592653589793D1 * (-t376 * t326 + t
     #381 * t329 / 0.2D1 + t332)) * t173 / 0.1440D4 + (-0.90D2 * t20 * (
     #-t326 + t408 * t329 - (t326 - t413 * t329) * t353) + 0.180D3 * t6 
     #* t368) * t87 * t226 / 0.720D3 + (-0.90D2 * t20 * (t429 * t326 - t
     #431 * t329 / 0.2D1 - t332) + 0.180D3 * t6 * 0.3141592653589793D1 *
     # (-t326 + t429 * t329) - t16 * t392) * t173 * t225 / 0.720D3
      t448 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t312, -t314, 0.0D0, t447)
      t450 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t314, t312, 0.0D0, t447)
      t452 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t307)
      t455 = x2 * s * t1
      t456 = -0.1D1 + x2
      t457 = t456 * s
      t458 = t457 * t1
      t459 = x2 * z
      t461 = 0.1D1 / (0.1D1 - x2 + t459)
      t462 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t463 = t461 * t462
      t464 = t90 * t456
      t467 = log(-0.4D1 * t211 * t464)
      t469 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t475 = 0.3141592653589793D1 * t461 * t469
      t481 = (-0.90D2 * t20 * (-t463 + t467 * t461 * t469) - 0.180D3 * t
     #6 * t475) * t87 * t226 / 0.720D3
      t484 = log(-0.4D1 * t210 * t464)
      t485 = t484 * t461
      t487 = t484 ** 2
      t491 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t492 = t461 * t491
      t501 = t16 * t475
      t505 = (-0.90D2 * t20 * (t485 * t462 - t487 * t461 * t469 / 0.2D1 
     #- t492) + 0.180D3 * t6 * 0.3141592653589793D1 * (-t463 + t485 * t4
     #69) - t501) * t173 * t225 / 0.720D3
      t508 = log(-0.4D1 * t205 * t464)
      t509 = t508 * t461
      t511 = t508 ** 2
      t526 = (-0.90D2 * t20 * (t509 * t462 - t511 * t461 * t469 / 0.2D1 
     #- t492) + 0.180D3 * t6 * 0.3141592653589793D1 * (-t463 + t509 * t4
     #69) - t501) * t87 * t225 / 0.1440D4
      t527 = t30 * t456
      t530 = log(-0.4D1 * t279 * t527)
      t531 = t530 * t461
      t533 = -t463 + t531 * t469
      t536 = t530 ** 2
      t537 = t536 * t461
      t540 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t547 = -t537 * t462 / 0.2D1 - t461 * t540 + t536 * t530 * t461 * t
     #469 / 0.6D1 + t531 * t491
      t550 = t69 * t475
      t554 = t531 * t462 - t537 * t469 / 0.2D1 - t492
      t561 = t481 + t505 + t526 + (t16 * 0.3141592653589793D1 * t533 - 0
     #.90D2 * t20 * t547 - t550 + 0.180D3 * t6 * 0.3141592653589793D1 * 
     #t554) * t225 / 0.1440D4
      t562 = FJET(XB1, XB2, s, 0.0D0, t455, 0.0D0, -t458, 0.0D0, t561)
      t564 = x2 * x3
      t567 = Sqrt(x3 * t456 * t31)
      t568 = t37 * t567
      t570 = 0.2D1 * t568 * x2
      t572 = 0.1D1 - x3 + t564
      t573 = 0.1D1 / t572
      t575 = t2 * (0.1D1 - x3 - x2 + t564 + t205 + t570) * t573
      t576 = 0.2D1 * t568
      t580 = t2 * x2 * (-0.1D1 + t564 + t576) * t573
      t581 = z * t37
      t586 = z * t204 * x3
      t588 = 0.1D1 / (-0.1D1 + t564 - t570 - t205 - t459 + t576 + 0.2D1 
     #* t581 * t567 * x2 + x2 - x3 + t586)
      t589 = t564 * t573
      t590 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t589, 
     #x4)
      t591 = t588 * t590
      t592 = t572 ** 2
      t593 = 0.1D1 / t592
      t595 = t527 * t31 * t593
      t598 = log(0.4D1 * t410 * t595)
      t600 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t589, 
     #x4)
      t606 = 0.3141592653589793D1 * t588 * t600
      t612 = (-0.90D2 * t20 * (-t591 + t598 * t588 * t600) - 0.180D3 * t
     #6 * t606) * t87 * t226 / 0.720D3
      t616 = log(0.4D1 * t205 * t27 * t595)
      t617 = t616 * t588
      t619 = t616 ** 2
      t623 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t589, 
     #x4)
      t625 = t617 * t590 - t619 * t588 * t600 / 0.2D1 - t588 * t623
      t629 = -t591 + t617 * t600
      t633 = t16 * t606
      t638 = t612 + (-0.90D2 * t20 * t625 + 0.180D3 * t6 * 0.31415926535
     #89793D1 * t629 - t633) * t87 * t225 / 0.1440D4
      t639 = FJET(XB1, XB2, s, 0.0D0, t575, 0.0D0, -t580, 0.0D0, t638)
      t641 = FJET(XB1, XB2, s, 0.0D0, -t458, 0.0D0, t455, 0.0D0, t561)
      t643 = FJET(XB1, XB2, s, 0.0D0, -t580, 0.0D0, t575, 0.0D0, t638)
      t647 = t2 * t313 * x2 * t320
      t649 = t457 * t1 * t313
      t650 = t1 ** 2
      t655 = s * t650 * x2 * x1 * t313 * t320
      t656 = x1 * x2
      t657 = t656 * z
      t659 = 0.1D1 / (-0.1D1 - t459 + t657 + x2 - t318 + x1 - t656)
      t660 = t319 * t659
      t661 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, t325, x2, 0.0D0, x
     #4)
      t662 = t660 * t661
      t664 = t317 * t320 * t456
      t667 = log(-0.4D1 * t410 * t664)
      t669 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t325, x2, 0.0D0, x
     #4)
      t670 = t659 * t669
      t675 = t6 * 0.3141592653589793D1
      t676 = t660 * t669
      t681 = (-0.90D2 * t20 * (-t662 + t667 * t319 * t670) - 0.180D3 * t
     #675 * t676) * t87 * t226
      t684 = log(-0.4D1 * t426 * t664)
      t685 = t684 * t319
      t688 = t684 ** 2
      t692 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, t325, x2, 0.0D0, x
     #4)
      t694 = t685 * t659 * t661 - t688 * t319 * t670 / 0.2D1 - t660 * t6
     #92
      t698 = -t662 + t685 * t670
      t703 = t16 * 0.3141592653589793D1 * t676
      t708 = t681 / 0.720D3 + (-0.90D2 * t20 * t694 + 0.180D3 * t6 * 0.3
     #141592653589793D1 * t698 - t703) * t173 * t225 / 0.720D3
      t709 = FJET(XB1, XB2, s, 0.0D0, -t647, t312, t649, -t655, t708)
      t711 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t307)
      t713 = FJET(XB1, XB2, s, t312, t649, 0.0D0, -t647, -t655, t708)
      t715 = t308 * t307 + t310 * t307 + t448 * t447 + t450 * t447 + t45
     #2 * t307 + t562 * t561 + t639 * t638 + t641 * t561 + t643 * t638 +
     # t709 * t708 + t711 * t307 + t713 * t708
      t716 = FJET(XB1, XB2, s, t312, -t314, 0.0D0, 0.0D0, 0.0D0, t447)
      t731 = t481 + t505 + t526 + (t16 * 0.3141592653589793D1 * t533 - 0
     #.90D2 * t20 * t547 - t550 + 0.180D3 * t6 * 0.3141592653589793D1 * 
     #t554) * t225 / 0.1440D4
      t732 = FJET(XB1, XB2, s, t455, 0.0D0, -t458, 0.0D0, 0.0D0, t731)
      t734 = FJET(XB1, XB2, s, t575, 0.0D0, -t580, 0.0D0, 0.0D0, t638)
      t736 = FJET(XB1, XB2, s, t649, t312, -t647, 0.0D0, -t655, t708)
      t738 = t312 * t589
      t739 = t343 * x2
      t740 = t343 * t459
      t741 = t456 * t31
      t743 = Sqrt(t347 * t741)
      t744 = t37 * t743
      t745 = 0.2D1 * t744
      t750 = t314 * x2 * (t343 - t344 + t564 - t739 + t740 - 0.1D1 + t74
     #5) * t320 * t573
      t754 = t31 * s * t1 * x1 * t573
      t756 = 0.2D1 * t744 * x2
      t757 = 0.1D1 - x1 + t318 - x2 + t656 - t657 - x3 + t343 - t344 + t
     #564 - t739 + t740 + t205 + t756
      t760 = t314 * t757 * t320 * t573
      t762 = t137 * t29
      t779 = x1 * t204
      t780 = t29 * x3
      t783 = t743 * x1
      t787 = 0.1D1 - 0.4D1 * t740 - 0.2D1 * t762 * t564 + 0.4D1 * t138 *
     # t459 + t343 * x2 * t29 + 0.3D1 * t318 * t205 - 0.2D1 * t581 * t74
     #3 * x2 - 0.2D1 * t744 * t656 - 0.2D1 * t210 * x3 * z - t779 * t780
     # + t210 * t780 - 0.2D1 * t581 * t783 + t318 - 0.3D1 * t343 + t459 
     #- x2 - t657
      t804 = -t564 + t205 + t656 - x1 + 0.2D1 * t581 * t783 * x2 + 0.3D1
     # * t739 + t756 + t211 + 0.2D1 * t744 * x1 - 0.4D1 * t138 * z - 0.2
     #D1 * t138 * x2 - 0.2D1 * t779 * x3 + 0.2D1 * t762 * x3 + 0.3D1 * t
     #344 - t586 - t745 + 0.2D1 * t138 + x3
      t806 = 0.1D1 / (t787 + t804)
      t807 = t319 * t806
      t808 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, t325, x2, t589, x4
     #)
      t814 = log(0.4D1 * t206 * t404 * t741 * t593)
      t816 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t325, x2, t589, x4
     #)
      t819 = -t807 * t808 + t814 * t319 * t806 * t816
      t824 = 0.180D3 * t675 * t807 * t816
      t825 = -0.90D2 * t20 * t819 - t824
      t828 = t825 * t87 * t226 / 0.720D3
      t829 = FJET(XB1, XB2, s, t738, t750, -t754, -t760, -t655, t828)
      t832 = t87 * t173 * t225
      t835 = FJET(XB1, XB2, s, t750, t738, -t760, -t754, -t655, t828)
      t839 = FJET(XB1, XB2, s, -t314, t312, 0.0D0, 0.0D0, 0.0D0, t447)
      t841 = FJET(XB1, XB2, s, -t458, 0.0D0, t455, 0.0D0, 0.0D0, t731)
      t854 = t612 + (-0.90D2 * t20 * t625 + 0.180D3 * t6 * 0.31415926535
     #89793D1 * t629 - t633) * t87 * t225 / 0.1440D4
      t855 = FJET(XB1, XB2, s, -t580, 0.0D0, t575, 0.0D0, 0.0D0, t854)
      t868 = t681 / 0.720D3 + (-0.90D2 * t20 * t694 + 0.180D3 * t6 * 0.3
     #141592653589793D1 * t698 - t703) * t173 * t225 / 0.720D3
      t869 = FJET(XB1, XB2, s, -t647, 0.0D0, t649, t312, -t655, t868)
      t874 = -0.90D2 * t20 * t819 - t824
      t877 = t874 * t87 * t226 / 0.720D3
      t878 = FJET(XB1, XB2, s, -t754, -t760, t738, t750, -t655, t877)
      t882 = FJET(XB1, XB2, s, -t760, -t754, t750, t738, -t655, t877)
      t886 = t716 * t447 + t732 * t731 + t734 * t638 + t736 * t708 + t82
     #9 * t825 * t832 / 0.720D3 + t835 * t825 * t832 / 0.720D3 + t839 * 
     #t447 + t841 * t731 + t855 * t854 + t869 * t868 + t878 * t874 * t83
     #2 / 0.720D3 + t882 * t874 * t832 / 0.720D3
      rrgg2gght7s1e1 = t715 + t886

      end function



      doubleprecision function rrgg2gght7s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = -0.1D1 + x3
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t11 * t13 * t15)
      t20 = t19 ** 2
      t21 = cos(t8)
      t23 = Sqrt(-x3 * t14)
      t27 = 0.1D1 / (-0.1D1 + 0.2D1 * t21 * t23 - x3)
      t31 = log(0.4D1 * t11 * t13)
      t32 = t31 ** 2
      t38 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t41 = lh * t5
      t42 = 0.3141592653589793D1 * t7
      t44 = 0.180D3 * t41 * t42
      t49 = 0.3141592653589793D1 * t38
      t52 = lh ** 2
      t53 = 0.180D3 * t52
      t54 = 0.3141592653589793D1 ** 2
      t55 = 0.30D2 * t54
      t56 = -t53 + t55
      t57 = t56 * t5
      t58 = t57 * t42
      t59 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t66 = 0.1D1 / x3
      t69 = t13 * t10
      t71 = log(0.4D1 * t69)
      t74 = t71 ** 2
      t80 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t103 = x2 ** 2
      t104 = t103 * x3
      t105 = t69 * t15
      t108 = log(-0.4D1 * t104 * t105)
      t114 = log(0.4D1 * t104 * t69)
      t120 = t7 + t7 * t27
      t123 = 0.180D3 * t41 * 0.3141592653589793D1 * t120
      t126 = 0.1D1 / x2
      t129 = t103 * t10
      t132 = log(0.4D1 * t129 * t13)
      t134 = t132 ** 2
      t148 = x1 ** 2
      t149 = x3 * t148
      t152 = log(-0.4D1 * t149 * t105)
      t158 = log(0.4D1 * t149 * t69)
      t165 = 0.1D1 / x1
      t170 = t66 * t165 * t126
      t173 = t103 * t148
      t176 = log(0.4D1 * t173 * t69)
      t185 = t148 * t10
      t188 = log(0.4D1 * t185 * t13)
      t190 = t188 ** 2
      t204 = (-0.90D2 * t6 * t7 * (t20 * t27 / 0.2D1 + t32 / 0.2D1) + (-
     #0.90D2 * t6 * t38 + t44) * (-t19 * t27 - t31) + (0.180D3 * t41 * t
     #49 + t58 - 0.90D2 * t6 * t59) * (t27 + 0.1D1)) * t66 / 0.2880D4 + 
     #(-0.180D3 * t71 * lh - 0.45D2 * t74 - t53 + t55) * t5 * t49 / 0.28
     #80D4 - t6 * t80 / 0.32D2 + (0.90D2 * t74 * lh - 0.60D2 * lh * t54 
     #+ 0.2884936567583026D3 + 0.120D3 * t52 * lh + 0.15D2 * t74 * t71 -
     # t71 * t56) * t5 * t42 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t71) 
     #* t5 * 0.3141592653589793D1 * t59 / 0.2880D4 + (-0.90D2 * t6 * ((t
     #38 - t108 * t7) * t27 + t38 - t114 * t7) + t123) * t66 * t126 / 0.
     #1440D4 + (-0.90D2 * t6 * (-t132 * t38 + t134 * t7 / 0.2D1 + t59) +
     # 0.180D3 * t41 * 0.3141592653589793D1 * (t38 - t132 * t7) + t58) *
     # t126 / 0.1440D4 + (-0.90D2 * t6 * ((t38 - t152 * t7) * t27 + t38 
     #- t158 * t7) + t123) * t66 * t165 / 0.1440D4 - t6 * t120 * t170 / 
     #0.8D1 + (-0.90D2 * t6 * (t38 - t176 * t7) + t44) * t165 * t126 / 0
     #.720D3 - (-0.90D2 * t6 * (t188 * t38 - t190 * t7 / 0.2D1 - t59) + 
     #0.180D3 * t41 * 0.3141592653589793D1 * (-t38 + t188 * t7) - t58) *
     # t165 / 0.1440D4
      t205 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t204)
      t207 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t204)
      t209 = t2 * x1
      t210 = -0.1D1 + x1
      t211 = t2 * t210
      t212 = -t210
      t213 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, t212, 0.0D0, 0.0D0
     #, x4)
      t214 = t149 * t10
      t215 = t210 ** 2
      t216 = t13 * t215
      t217 = x1 * z
      t218 = 0.1D1 - x1 + t217
      t219 = 0.1D1 / t218
      t220 = t216 * t219
      t223 = log(0.4D1 * t214 * t220)
      t224 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t212, 0.0D0, 0.0D0
     #, x4)
      t230 = log(-0.4D1 * t214 * t216 * t219 * t15)
      t233 = x3 * x1
      t234 = t233 * z
      t237 = x3 * t218
      t239 = Sqrt(-t237 * t14)
      t243 = 0.1D1 / (-0.2D1 * t234 + 0.2D1 * t233 - 0.1D1 + 0.2D1 * t21
     # * t239 - x3)
      t249 = -t224 - t224 * t243
      t260 = t173 * t10
      t263 = log(0.4D1 * t260 * t220)
      t268 = 0.3141592653589793D1 * t224
      t277 = log(0.4D1 * t185 * t220)
      t279 = t277 ** 2
      t282 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, t212, 0.0D0, 0.0D0
     #, x4)
      t295 = (-0.90D2 * t6 * (-t213 + t223 * t224 - (t213 - t230 * t224)
     # * t243) + 0.180D3 * t41 * 0.3141592653589793D1 * t249) * t66 * t1
     #65 / 0.1440D4 - t6 * t249 * t170 / 0.8D1 + (-0.90D2 * t6 * (-t213 
     #+ t263 * t224) - 0.180D3 * t41 * t268) * t165 * t126 / 0.720D3 - (
     #-0.90D2 * t6 * (-t277 * t213 + t279 * t224 / 0.2D1 + t282) + 0.180
     #D3 * t41 * 0.3141592653589793D1 * (t213 - t277 * t224) + t57 * t26
     #8) * t165 / 0.1440D4
      t296 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t209, -t211, 0.0D0, t295)
      t298 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t211, t209, 0.0D0, t295)
      t300 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t204)
      t303 = x2 * s * t1
      t304 = -0.1D1 + x2
      t305 = t304 * s
      t306 = t305 * t1
      t307 = x2 * z
      t309 = 0.1D1 / (0.1D1 - x2 + t307)
      t310 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t311 = t309 * t310
      t312 = t69 * t304
      t315 = log(-0.4D1 * t104 * t312)
      t317 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t323 = 0.3141592653589793D1 * t309 * t317
      t325 = 0.180D3 * t41 * t323
      t329 = (-0.90D2 * t6 * (-t311 + t315 * t309 * t317) - t325) * t66 
     #* t126 / 0.1440D4
      t330 = t13 * t304
      t333 = log(-0.4D1 * t129 * t330)
      t334 = t333 * t309
      t336 = t333 ** 2
      t340 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t342 = t334 * t310 - t336 * t309 * t317 / 0.2D1 - t309 * t340
      t346 = -t311 + t334 * t317
      t350 = t57 * t323
      t356 = t165 * t126
      t359 = t6 * t309 * t317 * t66 * t356 / 0.8D1
      t362 = log(-0.4D1 * t173 * t312)
      t371 = (-0.90D2 * t6 * (-t311 + t362 * t309 * t317) - t325) * t165
     # * t126 / 0.720D3
      t372 = t329 + (-0.90D2 * t6 * t342 + 0.180D3 * t41 * 0.31415926535
     #89793D1 * t346 - t350) * t126 / 0.1440D4 + t359 + t371
      t373 = FJET(XB1, XB2, s, 0.0D0, t303, 0.0D0, -t306, 0.0D0, t372)
      t375 = x2 * x3
      t378 = Sqrt(x3 * t304 * t14)
      t379 = t21 * t378
      t381 = 0.2D1 * t379 * x2
      t383 = 0.1D1 - x3 + t375
      t384 = 0.1D1 / t383
      t386 = t2 * (0.1D1 - x3 - x2 + t375 + t104 + t381) * t384
      t387 = 0.2D1 * t379
      t391 = t2 * x2 * (-0.1D1 + t375 + t387) * t384
      t392 = z * t21
      t397 = z * t103 * x3
      t399 = 0.1D1 / (-0.1D1 + t375 - t381 - t104 - t307 + t387 + 0.2D1 
     #* t392 * t378 * x2 + x2 - x3 + t397)
      t400 = t375 * t384
      t401 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t400, 
     #x4)
      t404 = t383 ** 2
      t410 = log(0.4D1 * t104 * t10 * t330 * t14 / t404)
      t412 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t400, 
     #x4)
      t414 = -t399 * t401 + t410 * t399 * t412
      t420 = 0.180D3 * t41 * 0.3141592653589793D1 * t399 * t412
      t429 = t6 * t399 * t412 * t66 * t356 / 0.8D1
      t430 = (-0.90D2 * t6 * t414 - t420) * t66 * t126 / 0.1440D4 + t429
      t431 = FJET(XB1, XB2, s, 0.0D0, t386, 0.0D0, -t391, 0.0D0, t430)
      t433 = FJET(XB1, XB2, s, 0.0D0, -t306, 0.0D0, t303, 0.0D0, t372)
      t435 = FJET(XB1, XB2, s, 0.0D0, -t391, 0.0D0, t386, 0.0D0, t430)
      t439 = t2 * t210 * x2 * t219
      t441 = t305 * t1 * t210
      t442 = t1 ** 2
      t447 = s * t442 * x2 * x1 * t210 * t219
      t448 = x1 * x2
      t449 = t448 * z
      t451 = 0.1D1 / (-0.1D1 - t307 + t449 + x2 - t217 + x1 - t448)
      t452 = t218 * t451
      t454 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t212, x2, 0.0D0, x
     #4)
      t458 = t6 * t452 * t454 * t66 * t356 / 0.8D1
      t459 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, t212, x2, 0.0D0, x
     #4)
      t466 = log(-0.4D1 * t260 * t13 * t219 * t215 * t304)
      t470 = -t452 * t459 + t466 * t218 * t451 * t454
      t476 = 0.180D3 * t41 * 0.3141592653589793D1 * t452 * t454
      t481 = t458 + (-0.90D2 * t6 * t470 - t476) * t165 * t126 / 0.720D3
      t482 = FJET(XB1, XB2, s, 0.0D0, -t439, t209, t441, -t447, t481)
      t484 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t204)
      t486 = FJET(XB1, XB2, s, t209, t441, 0.0D0, -t439, -t447, t481)
      t488 = t205 * t204 + t207 * t204 + t296 * t295 + t298 * t295 + t30
     #0 * t204 + t373 * t372 + t431 * t430 + t433 * t372 + t435 * t430 +
     # t482 * t481 + t484 * t204 + t486 * t481
      t489 = FJET(XB1, XB2, s, t209, -t211, 0.0D0, 0.0D0, 0.0D0, t295)
      t501 = t329 + (-0.90D2 * t6 * t342 + 0.180D3 * t41 * 0.31415926535
     #89793D1 * t346 - t350) * t126 / 0.1440D4 + t359 + t371
      t502 = FJET(XB1, XB2, s, t303, 0.0D0, -t306, 0.0D0, 0.0D0, t501)
      t504 = FJET(XB1, XB2, s, t386, 0.0D0, -t391, 0.0D0, 0.0D0, t430)
      t506 = FJET(XB1, XB2, s, t441, t209, -t439, 0.0D0, -t447, t481)
      t508 = t209 * t400
      t509 = t233 * x2
      t510 = t233 * t307
      t513 = Sqrt(t237 * t304 * t14)
      t514 = t21 * t513
      t515 = 0.2D1 * t514
      t520 = t211 * x2 * (t233 - t234 + t375 - t509 + t510 - 0.1D1 + t51
     #5) * t219 * t384
      t524 = t14 * s * t1 * x1 * t384
      t526 = 0.2D1 * t514 * x2
      t527 = 0.1D1 - x1 + t217 - x2 + t448 - t449 - x3 + t233 - t234 + t
     #375 - t509 + t510 + t104 + t526
      t530 = t211 * t527 * t219 * t384
      t531 = t513 * x1
      t544 = 0.1D1 + 0.2D1 * t392 * t531 * x2 - t449 - t515 + 0.2D1 * t1
     #49 + t448 - x2 + t307 - t375 + t104 + x3 + 0.3D1 * t509 + t526 + t
     #173 * x3 + 0.2D1 * t514 * x1 - 0.4D1 * t149 * z - 0.2D1 * t149 * x
     #2
      t545 = x1 * t103
      t548 = t148 * t12
      t570 = t12 * x3
      t575 = -0.2D1 * t545 * x3 + 0.2D1 * t548 * x3 + 0.3D1 * t234 + t21
     #7 - 0.3D1 * t233 - x1 - 0.4D1 * t510 - 0.2D1 * t548 * t375 + 0.4D1
     # * t149 * t307 + t233 * x2 * t12 + 0.3D1 * t217 * t104 - 0.2D1 * t
     #392 * t513 * x2 - 0.2D1 * t514 * t448 - 0.2D1 * t173 * x3 * z - t5
     #45 * t570 + t173 * t570 - 0.2D1 * t392 * t531 - t397
      t577 = 0.1D1 / (t544 + t575)
      t580 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t212, x2, t400, x4
     #)
      t584 = t6 * t218 * t577 * t580 * t66 * t356 / 0.8D1
      t585 = FJET(XB1, XB2, s, t508, t520, -t524, -t530, -t447, t584)
      t587 = 0.3141592653589793D1 * t218
      t590 = t577 * t580 * t170
      t593 = FJET(XB1, XB2, s, t520, t508, -t530, -t524, -t447, t584)
      t598 = FJET(XB1, XB2, s, -t211, t209, 0.0D0, 0.0D0, 0.0D0, t295)
      t600 = FJET(XB1, XB2, s, -t306, 0.0D0, t303, 0.0D0, 0.0D0, t501)
      t609 = (-0.90D2 * t6 * t414 - t420) * t66 * t126 / 0.1440D4 + t429
      t610 = FJET(XB1, XB2, s, -t391, 0.0D0, t386, 0.0D0, 0.0D0, t609)
      t619 = t458 + (-0.90D2 * t6 * t470 - t476) * t165 * t126 / 0.720D3
      t620 = FJET(XB1, XB2, s, -t439, 0.0D0, t441, t209, -t447, t619)
      t622 = FJET(XB1, XB2, s, -t524, -t530, t508, t520, -t447, t584)
      t627 = FJET(XB1, XB2, s, -t530, -t524, t520, t508, -t447, t584)
      t632 = t489 * t295 + t502 * t501 + t504 * t430 + t506 * t481 + t58
     #5 * t5 * t587 * t590 / 0.8D1 + t593 * t5 * t587 * t590 / 0.8D1 + t
     #598 * t295 + t600 * t501 + t610 * t609 + t620 * t619 + t622 * t5 *
     # t587 * t590 / 0.8D1 + t627 * t5 * t587 * t590 / 0.8D1
      rrgg2gght7s1e0 = t488 + t632

      end function



      doubleprecision function rrgg2gght7s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = -0.1D1 + x3
      t19 = log(-0.4D1 * t11 * t13 / t14)
      t20 = cos(t8)
      t22 = Sqrt(-x3 * t14)
      t26 = 0.1D1 / (-0.1D1 + 0.2D1 * t20 * t22 - x3)
      t30 = log(0.4D1 * t11 * t13)
      t35 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t38 = lh * t5
      t39 = 0.3141592653589793D1 * t7
      t41 = 0.180D3 * t38 * t39
      t46 = 0.1D1 / x3
      t52 = log(0.4D1 * t13 * t10)
      t61 = t52 ** 2
      t63 = lh ** 2
      t65 = 0.3141592653589793D1 ** 2
      t71 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t76 = (t7 + t7 * t26) * t46
      t77 = 0.1D1 / x2
      t81 = x2 ** 2
      t82 = t81 * t10
      t85 = log(0.4D1 * t82 * t13)
      t93 = 0.1D1 / x1
      t98 = x1 ** 2
      t99 = t98 * t10
      t102 = log(0.4D1 * t99 * t13)
      t113 = (-0.90D2 * t6 * t7 * (-t19 * t26 - t30) + (-0.90D2 * t6 * t
     #35 + t41) * (t26 + 0.1D1)) * t46 / 0.2880D4 + (0.180D3 * lh + 0.90
     #D2 * t52) * t5 * 0.3141592653589793D1 * t35 / 0.2880D4 + (-0.180D3
     # * t52 * lh - 0.45D2 * t61 - 0.180D3 * t63 + 0.30D2 * t65) * t5 * 
     #t39 / 0.2880D4 - t6 * t71 / 0.32D2 - t6 * t76 * t77 / 0.16D2 + (-0
     #.90D2 * t6 * (t35 - t85 * t7) + t41) * t77 / 0.1440D4 - t6 * t7 * 
     #t93 * t77 / 0.8D1 - (-0.90D2 * t6 * (-t35 + t102 * t7) - t41) * t9
     #3 / 0.1440D4 - t6 * t76 * t93 / 0.16D2
      t114 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t113)
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t113)
      t118 = t2 * x1
      t119 = -0.1D1 + x1
      t120 = t2 * t119
      t121 = -t119
      t122 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t121, 0.0D0, 0.0D0
     #, x4)
      t127 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, t121, 0.0D0, 0.0D0
     #, x4)
      t128 = x1 * z
      t129 = 0.1D1 - x1 + t128
      t130 = 0.1D1 / t129
      t132 = t119 ** 2
      t136 = log(0.4D1 * t99 * t13 * t130 * t132)
      t147 = x3 * x1
      t153 = Sqrt(-x3 * t129 * t14)
      t164 = t6 * t122 * t93 * t77 / 0.8D1 - (-0.90D2 * t6 * (t127 - t13
     #6 * t122) + 0.180D3 * t38 * 0.3141592653589793D1 * t122) * t93 / 0
     #.1440D4 - t6 * (-t122 - t122 / (-0.2D1 * t147 * z + 0.2D1 * t147 -
     # 0.1D1 + 0.2D1 * t20 * t153 - x3)) * t46 * t93 / 0.16D2
      t165 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t118, -t120, 0.0D0, t164)
      t167 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t120, t118, 0.0D0, t164)
      t169 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t113)
      t172 = x2 * s * t1
      t173 = -0.1D1 + x2
      t174 = t173 * s
      t175 = t174 * t1
      t176 = x2 * z
      t178 = 0.1D1 / (0.1D1 - x2 + t176)
      t179 = t6 * t178
      t180 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t184 = t179 * t180 * t46 * t77 / 0.16D2
      t185 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t190 = log(-0.4D1 * t82 * t13 * t173)
      t193 = -t178 * t185 + t190 * t178 * t180
      t199 = 0.180D3 * t38 * 0.3141592653589793D1 * t178 * t180
      t206 = t179 * t180 * t93 * t77 / 0.8D1
      t207 = t184 + (-0.90D2 * t6 * t193 - t199) * t77 / 0.1440D4 + t206
      t208 = FJET(XB1, XB2, s, 0.0D0, t172, 0.0D0, -t175, 0.0D0, t207)
      t210 = x2 * x3
      t211 = t81 * x3
      t214 = Sqrt(x3 * t173 * t14)
      t215 = t20 * t214
      t217 = 0.2D1 * t215 * x2
      t220 = 0.1D1 / (0.1D1 - x3 + t210)
      t222 = t2 * (0.1D1 - x3 - x2 + t210 + t211 + t217) * t220
      t223 = 0.2D1 * t215
      t227 = t2 * x2 * (-0.1D1 + t210 + t223) * t220
      t235 = 0.1D1 / (-0.1D1 + t210 - t217 - t211 - t176 + t223 + 0.2D1 
     #* z * t20 * t214 * x2 + x2 - x3 + z * t81 * x3)
      t238 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t210 *
     # t220, x4)
      t242 = t6 * t235 * t238 * t46 * t77 / 0.16D2
      t243 = FJET(XB1, XB2, s, 0.0D0, t222, 0.0D0, -t227, 0.0D0, t242)
      t248 = t235 * t238 * t46 * t77
      t251 = FJET(XB1, XB2, s, 0.0D0, -t175, 0.0D0, t172, 0.0D0, t207)
      t253 = FJET(XB1, XB2, s, 0.0D0, -t227, 0.0D0, t222, 0.0D0, t242)
      t260 = t2 * t119 * x2 * t130
      t262 = t174 * t1 * t119
      t263 = t1 ** 2
      t268 = s * t263 * x2 * x1 * t119 * t130
      t270 = x1 * x2
      t274 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, t121, x2, 0.0D0, x
     #4)
      t277 = 0.1D1 / (-0.1D1 - t176 + t270 * z + x2 - t128 + x1 - t270) 
     #* t274 * t93 * t77
      t279 = t6 * t129 * t277 / 0.8D1
      t280 = FJET(XB1, XB2, s, 0.0D0, -t260, t118, t262, -t268, t279)
      t282 = 0.3141592653589793D1 * t129
      t286 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t113)
      t288 = FJET(XB1, XB2, s, t118, t262, 0.0D0, -t260, -t268, t279)
      t293 = FJET(XB1, XB2, s, t118, -t120, 0.0D0, 0.0D0, 0.0D0, t164)
      t301 = t184 + (-0.90D2 * t6 * t193 - t199) * t77 / 0.1440D4 + t206
      t302 = FJET(XB1, XB2, s, t172, 0.0D0, -t175, 0.0D0, 0.0D0, t301)
      t304 = FJET(XB1, XB2, s, t222, 0.0D0, -t227, 0.0D0, 0.0D0, t242)
      t309 = FJET(XB1, XB2, s, t262, t118, -t260, 0.0D0, -t268, t279)
      t314 = FJET(XB1, XB2, s, -t120, t118, 0.0D0, 0.0D0, 0.0D0, t164)
      t316 = FJET(XB1, XB2, s, -t175, 0.0D0, t172, 0.0D0, 0.0D0, t301)
      t318 = FJET(XB1, XB2, s, -t260, 0.0D0, t262, t118, -t268, t279)
      t323 = FJET(XB1, XB2, s, -t227, 0.0D0, t222, 0.0D0, 0.0D0, t242)
      rrgg2gght7s1em1 = t114 * t113 + t116 * t113 + t165 * t164 + t167 *
     # t164 + t169 * t113 + t208 * t207 + t243 * t5 * 0.3141592653589793
     #D1 * t248 / 0.16D2 + t251 * t207 + t253 * t5 * 0.3141592653589793D
     #1 * t248 / 0.16D2 + t280 * t5 * t282 * t277 / 0.8D1 + t286 * t113 
     #+ t288 * t5 * t282 * t277 / 0.8D1 + t293 * t164 + t302 * t301 + t3
     #04 * t5 * 0.3141592653589793D1 * t248 / 0.16D2 + t309 * t5 * t282 
     #* t277 / 0.8D1 + t314 * t164 + t316 * t301 + t318 * t5 * t282 * t2
     #77 / 0.8D1 + t323 * t5 * 0.3141592653589793D1 * t248 / 0.16D2

      end function



      doubleprecision function rrgg2gght7s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t23 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t31 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t35 = z ** 2
      t37 = Sin(t8)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t35 * t38)
      t48 = -t6 * t7 * (0.1D1 / (-0.1D1 + 0.2D1 * t9 * t12 - x3) + 0.1D1
     #) / x3 / 0.32D2 - t6 * t7 * t23 / 0.16D2 - t6 * t7 * t27 / 0.16D2 
     #- t6 * t31 / 0.32D2 + (0.180D3 * lh + 0.90D2 * t41) * t5 * 0.31415
     #92653589793D1 * t7 / 0.2880D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t53 = t2 * x1
      t54 = -0.1D1 + x1
      t55 = t2 * t54
      t57 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, -t54, 0.0D0, 0.0D0,
     # x4)
      t60 = t6 * t57 * t27 / 0.16D2
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t53, -t55, 0.0D0, t60)
      t64 = 0.3141592653589793D1 * t57 * t27
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t55, t53, 0.0D0, t60)
      t71 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t74 = x2 * s * t1
      t77 = (-0.1D1 + x2) * s * t1
      t81 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t83 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t81 * t23
      t85 = t6 * t83 / 0.16D2
      t86 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, -t77, 0.0D0, t85)
      t91 = FJET(XB1, XB2, s, 0.0D0, -t77, 0.0D0, t74, 0.0D0, t85)
      t96 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t98 = FJET(XB1, XB2, s, t53, -t55, 0.0D0, 0.0D0, 0.0D0, t60)
      t102 = FJET(XB1, XB2, s, t74, 0.0D0, -t77, 0.0D0, 0.0D0, t85)
      t107 = FJET(XB1, XB2, s, -t77, 0.0D0, t74, 0.0D0, 0.0D0, t85)
      t112 = FJET(XB1, XB2, s, -t55, t53, 0.0D0, 0.0D0, 0.0D0, t60)
      rrgg2gght7s1em2 = t49 * t48 + t51 * t48 + t61 * t5 * t64 / 0.16D2 
     #+ t67 * t5 * t64 / 0.16D2 + t71 * t48 + t86 * t5 * 0.3141592653589
     #793D1 * t83 / 0.16D2 + t91 * t5 * 0.3141592653589793D1 * t83 / 0.1
     #6D2 + t96 * t48 + t98 * t5 * t64 / 0.16D2 + t102 * t5 * 0.31415926
     #53589793D1 * t83 / 0.16D2 + t107 * t5 * 0.3141592653589793D1 * t83
     # / 0.16D2 + t112 * t5 * t64 / 0.16D2

      end function



      doubleprecision function rrgg2gght7s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t9 = t5 * 0.3141592653589793D1 * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = 0.3141592653589793D1 * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2gght7s1em3 = -t10 * t5 * t12 / 0.32D2 - t14 * t5 * t12 / 0.32
     #D2 - t17 * t5 * t12 / 0.32D2 - t20 * t5 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2gght7s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      rrgg2gght7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght7s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = 0.3141592653589793D1 * t7
      t11 = lh ** 2
      t12 = 0.180D3 * t11
      t13 = 0.3141592653589793D1 ** 2
      t14 = 0.30D2 * t13
      t15 = t12 - t14
      t16 = t15 * t5
      t17 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t18 = 0.3141592653589793D1 * t17
      t20 = t5 * 0.3141592653589793D1
      t21 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t25 = z ** 2
      t27 = 0.1D1 / t25 / z
      t28 = x3 * t27
      t29 = x4 * 0.3141592653589793D1
      t30 = Sin(t29)
      t31 = t30 ** 2
      t34 = log(0.4D1 * t28 * t31)
      t35 = -0.1D1 + x3
      t36 = 0.1D1 / t35
      t40 = log(-0.4D1 * t28 * t31 * t36)
      t41 = x3 * z
      t42 = 0.2D1 * t41
      t43 = cos(t29)
      t45 = Sqrt(-t41 * t35)
      t49 = 0.1D1 / (-0.1D1 - t42 + x3 + 0.2D1 * t43 * t45)
      t53 = t40 ** 2
      t56 = t34 ** 2
      t64 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t68 = 0.60D2 * lh * t13
      t70 = 0.120D3 * t11 * lh
      t71 = t68 - 0.2884936567583026D3 - t70
      t72 = t71 * t5
      t74 = 0.3141592653589793D1 * t21
      t90 = 0.1D1 / x3
      t93 = t27 * t31
      t95 = log(0.4D1 * t93)
      t96 = t95 ** 2
      t99 = t96 * t95
      t101 = -t15
      t114 = 0.180D3 * lh
      t121 = rrgg2ggh71J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t128 = -t71
      t131 = t13 ** 2
      t132 = t11 ** 2
      t136 = t96 ** 2
      t142 = x1 ** 2
      t143 = x3 * t142
      t146 = log(0.4D1 * t143 * t93)
      t148 = t146 ** 2
      t151 = t93 * t36
      t154 = log(-0.4D1 * t143 * t151)
      t156 = t154 ** 2
      t172 = t101 * t5
      t179 = 0.1D1 / x1
      t182 = t142 * t31
      t183 = t182 * t27
      t185 = log(0.4D1 * t183)
      t190 = t185 ** 2
      t200 = t128 * t5
      t212 = 0.90D2 * t7
      t213 = x2 ** 2
      t214 = t142 * t213
      t215 = t214 * x3
      t218 = log(-0.4D1 * t215 * t151)
      t224 = t213 * x3
      t227 = log(0.4D1 * t224 * t183)
      t231 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t233 = -0.1D1 + x2
      t234 = t93 * t233
      t237 = log(-0.4D1 * t215 * t234)
      t240 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t245 = 0.1D1 / x2
      t246 = t90 * t179 * t245
      t251 = log(-0.4D1 * t214 * t234)
      t253 = t251 ** 2
      t256 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t259 = log(0.4D1 * t214 * t93)
      t261 = t259 ** 2
      t274 = 0.3141592653589793D1 * (-t240 + t17)
      t282 = log(0.4D1 * t224 * t93)
      t288 = t282 ** 2
      t292 = 0.90D2 * t21
      t295 = log(-0.4D1 * t224 * t234)
      t301 = t295 ** 2
      t308 = log(-0.4D1 * t224 * t151)
      t314 = t308 ** 2
      t325 = t27 * t213
      t326 = t31 * t233
      t329 = log(-0.4D1 * t325 * t326)
      t333 = log(0.4D1 * t325 * t31)
      t338 = t333 ** 2
      t345 = t329 ** 2
      t348 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t370 = -((-0.180D3 * t6 * t8 + t16 * t18 + 0.90D2 * t20 * t21) * (
     #-t34 - t40 * t49) + 0.90D2 * t20 * t17 * (-t53 * t40 * t49 / 0.6D1
     # - t56 * t34 / 0.6D1) + (t16 * t8 + 0.90D2 * t20 * t64 + t72 * t18
     # - 0.180D3 * t6 * t74) * (0.1D1 + t49) + (0.90D2 * t20 * t7 - 0.18
     #0D3 * t6 * t18) * (t56 / 0.2D1 + t53 * t49 / 0.2D1)) * t90 / 0.288
     #0D4 + (0.90D2 * t96 * lh - t68 + 0.2884936567583026D3 + t70 + 0.15
     #D2 * t99 - t95 * t101) * t5 * t8 / 0.2880D4 + (-0.180D3 * t95 * lh
     # - 0.45D2 * t96 - t12 + t14) * t5 * t74 / 0.2880D4 + (t114 + 0.90D
     #2 * t95) * t5 * 0.3141592653589793D1 * t64 / 0.2880D4 - t20 * t121
     # / 0.32D2 + (-0.30D2 * t99 * lh + t96 * t101 / 0.2D1 - t95 * t128 
     #- 0.5769873135166051D3 * lh - t131 - 0.60D2 * t132 + 0.60D2 * t11 
     #* t13 - 0.15D2 / 0.4D1 * t136) * t5 * t18 / 0.2880D4 + (-0.90D2 * 
     #t20 * (-t146 * t7 + t148 * t17 / 0.2D1 + t21 + (-t154 * t7 + t156 
     #* t17 / 0.2D1 + t21) * t49) + 0.180D3 * t6 * 0.3141592653589793D1 
     #* (t7 - t146 * t17 + (t7 - t154 * t17) * t49) + t172 * 0.314159265
     #3589793D1 * (t17 * t49 + t17)) * t90 * t179 / 0.1440D4 + (t172 * 0
     #.3141592653589793D1 * (t7 - t185 * t17) - 0.90D2 * t20 * (t190 * t
     #7 / 0.2D1 + t64 - t190 * t185 * t17 / 0.6D1 - t185 * t21) + t200 *
     # t18 + 0.180D3 * t6 * 0.3141592653589793D1 * (-t185 * t7 + t190 * 
     #t17 / 0.2D1 + t21)) * t179 / 0.1440D4 + t20 * ((-t212 + (t114 + 0.
     #90D2 * t218) * t17) * t49 - t212 + (t114 + 0.90D2 * t227) * t17 + 
     #0.90D2 * t231 + (-t114 - 0.90D2 * t237) * t240) * t246 / 0.720D3 +
     # (-0.90D2 * t20 * (t251 * t231 - t253 * t240 / 0.2D1 - t256 - t259
     # * t7 + t261 * t17 / 0.2D1 + t21) + 0.180D3 * t6 * 0.3141592653589
     #793D1 * (-t231 + t251 * t240 + t7 - t259 * t17) + t172 * t274) * t
     #179 * t245 / 0.720D3 + t20 * (-(-t114 - 0.90D2 * t282) * t7 - (0.1
     #80D3 * t282 * lh + 0.45D2 * t288 + t12 - t14) * t17 - t292 - (t114
     # + 0.90D2 * t295) * t231 - (-0.180D3 * t295 * lh - 0.45D2 * t301 -
     # t12 + t14) * t240 + 0.90D2 * t256 + ((t114 + 0.90D2 * t308) * t7 
     #+ (-0.180D3 * t308 * lh - 0.45D2 * t314 - t12 + t14) * t17 - t292)
     # * t49) * t90 * t245 / 0.1440D4 - (t16 * 0.3141592653589793D1 * (-
     #t231 + t329 * t240 + t7 - t333 * t17) + 0.90D2 * t20 * (t338 * t7 
     #/ 0.2D1 + t64 - t338 * t333 * t17 / 0.6D1 - t333 * t21 - t345 * t2
     #31 / 0.2D1 - t348 + t345 * t329 * t240 / 0.6D1 + t329 * t256) + t7
     #2 * t274 - 0.180D3 * t6 * 0.3141592653589793D1 * (t329 * t231 - t3
     #45 * t240 / 0.2D1 - t256 - t333 * t7 + t338 * t17 / 0.2D1 + t21)) 
     #* t245 / 0.1440D4
      t371 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t370)
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t370)
      t375 = t2 * x1
      t376 = -0.1D1 + x1
      t377 = t2 * t376
      t378 = t143 * t31
      t379 = 0.1D1 / t25
      t380 = t376 ** 2
      t381 = t379 * t380
      t382 = x1 * z
      t383 = -z - x1 + t382
      t384 = 0.1D1 / t383
      t385 = t381 * t384
      t388 = log(-0.4D1 * t378 * t385)
      t389 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t391 = t388 ** 2
      t392 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t395 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t397 = t381 * t384 * t36
      t400 = log(0.4D1 * t378 * t397)
      t402 = t400 ** 2
      t406 = x3 * x1
      t407 = t406 * z
      t410 = x3 * t383
      t412 = Sqrt(t410 * t35)
      t416 = 0.1D1 / (0.2D1 * t407 - 0.2D1 * t406 - t42 - 0.1D1 + 0.2D1 
     #* t43 * t412 + x3)
      t436 = (-0.90D2 * t20 * (t388 * t389 - t391 * t392 / 0.2D1 - t395 
     #- (-t400 * t389 + t402 * t392 / 0.2D1 + t395) * t416) + 0.180D3 * 
     #t6 * 0.3141592653589793D1 * (-t389 + t388 * t392 - (t389 - t400 * 
     #t392) * t416) + t172 * 0.3141592653589793D1 * (-t392 - t392 * t416
     #)) * t90 * t179 / 0.1440D4
      t439 = log(-0.4D1 * t182 * t385)
      t441 = -t389 + t439 * t392
      t444 = t439 ** 2
      t447 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t452 = -t444 * t389 / 0.2D1 - t447 + t444 * t439 * t392 / 0.6D1 + 
     #t439 * t395
      t455 = 0.3141592653589793D1 * t392
      t456 = t200 * t455
      t460 = t439 * t389 - t444 * t392 / 0.2D1 - t395
      t467 = 0.90D2 * t389
      t469 = t380 * t384
      t473 = log(-0.4D1 * t215 * t31 * t379 * t469)
      t477 = t224 * t182
      t480 = log(0.4D1 * t477 * t397)
      t489 = t20 * (t467 - (t114 + 0.90D2 * t473) * t392 - (-t467 + (t11
     #4 + 0.90D2 * t480) * t392) * t416) * t246 / 0.720D3
      t490 = t214 * t31
      t493 = log(-0.4D1 * t490 * t385)
      t495 = t493 ** 2
      t510 = (-0.90D2 * t20 * (t493 * t389 - t495 * t392 / 0.2D1 - t395)
     # + 0.180D3 * t6 * 0.3141592653589793D1 * (-t389 + t493 * t392) - t
     #172 * t455) * t179 * t245 / 0.720D3
      t511 = t436 + (t172 * 0.3141592653589793D1 * t441 - 0.90D2 * t20 *
     # t452 - t456 + 0.180D3 * t6 * 0.3141592653589793D1 * t460) * t179 
     #/ 0.1440D4 + t489 + t510
      t512 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t375, -t377, 0.0D0, t511)
      t514 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t377, t375, 0.0D0, t511)
      t516 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t370)
      t518 = x2 * x3
      t519 = 0.1D1 - x3 + t518
      t520 = 0.1D1 / t519
      t521 = t518 * t520
      t522 = t2 * t521
      t524 = t2 * t35 * t520
      t525 = t518 * z
      t526 = t233 * t35
      t528 = Sqrt(t41 * t526)
      t532 = 0.1D1 / (t525 - t42 - 0.1D1 + x3 + 0.2D1 * t43 * t528)
      t533 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t521, x
     #4)
      t537 = t519 ** 2
      t538 = 0.1D1 / t537
      t539 = t35 * t538
      t543 = log(0.4D1 * t477 * t27 * t233 * t539)
      t547 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t521, x
     #4)
      t552 = t20 * (0.90D2 * t532 * t533 - (t114 + 0.90D2 * t543) * t532
     # * t547) * t246 / 0.720D3
      t557 = log(0.4D1 * t224 * t27 * t326 * t539)
      t564 = t557 ** 2
      t569 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t521, x
     #4)
      t572 = -(t114 + 0.90D2 * t557) * t532 * t533 - (-0.180D3 * t557 * 
     #lh - 0.45D2 * t564 - t12 + t14) * t532 * t547 + 0.90D2 * t532 * t5
     #69
      t577 = t552 + t20 * t572 * t90 * t245 / 0.1440D4
      t578 = FJET(XB1, XB2, s, 0.0D0, t522, 0.0D0, -t524, 0.0D0, t577)
      t580 = FJET(XB1, XB2, s, 0.0D0, -t524, 0.0D0, t522, 0.0D0, t577)
      t582 = x1 * x2
      t584 = t2 * t582 * t384
      t587 = t233 * s * t1 * x1
      t588 = t1 ** 2
      t593 = s * t588 * x2 * x1 * t376 * t384
      t595 = -0.90D2 * t383
      t596 = t582 * z
      t598 = 0.1D1 / (z + x1 - t382 - t582 + t596)
      t600 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t603 = t381 * t384 * t233
      t606 = log(0.4D1 * t477 * t603)
      t610 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t611 = t598 * t610
      t615 = t20 * (-t595 * t598 * t600 - (t114 + 0.90D2 * t606) * t383 
     #* t611) * t246
      t618 = log(0.4D1 * t490 * t603)
      t619 = t618 * t383
      t622 = t618 ** 2
      t626 = t383 * t598
      t627 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t629 = t619 * t598 * t600 - t622 * t383 * t611 / 0.2D1 - t626 * t6
     #27
      t634 = -t626 * t600 + t619 * t611
      t640 = t172 * 0.3141592653589793D1 * t626 * t610
      t645 = t615 / 0.720D3 + (-0.90D2 * t20 * t629 + 0.180D3 * t6 * 0.3
     #141592653589793D1 * t634 - t640) * t179 * t245 / 0.720D3
      t646 = FJET(XB1, XB2, s, 0.0D0, -t584, -t377, -t587, t593, t645)
      t648 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t370)
      t663 = t436 + (t172 * 0.3141592653589793D1 * t441 - 0.90D2 * t20 *
     # t452 - t456 + 0.180D3 * t6 * 0.3141592653589793D1 * t460) * t179 
     #/ 0.1440D4 + t489 + t510
      t664 = FJET(XB1, XB2, s, t375, -t377, 0.0D0, 0.0D0, 0.0D0, t663)
      t671 = t552 + t20 * t572 * t90 * t245 / 0.1440D4
      t672 = FJET(XB1, XB2, s, t522, 0.0D0, -t524, 0.0D0, 0.0D0, t671)
      t677 = t35 * s * t1 * t376 * t520
      t678 = x2 * z
      t679 = t406 * x2
      t680 = t406 * t678
      t682 = Sqrt(-t410 * t526)
      t683 = t43 * t682
      t684 = t683 * x2
      t686 = z + x1 - t382 - t678 - t582 + t596 - t41 - t406 + t407 + t5
     #25 + t679 - t680 + t224 + 0.2D1 * t684
      t689 = t375 * t686 * t384 * t520
      t690 = t377 * t521
      t696 = t375 * x2 * (-t41 - t406 + t407 + t525 + t679 - t680 - 0.1D
     #1 + x3 + 0.2D1 * t683) * t384 * t520
      t703 = x1 * t43
      t712 = t142 * t25
      t725 = -0.5D1 * t407 - t679 + 0.2D1 * z * t43 * t682 + t518 * t25 
     #- t215 + 0.2D1 * t703 * t682 + 0.4D1 * t406 * t25 + 0.4D1 * t143 *
     # z + 0.2D1 * t143 * x2 - 0.2D1 * t712 * x3 + 0.2D1 * t712 * t518 -
     # 0.4D1 * t143 * t678 - 0.3D1 * t406 * x2 * t25 - t382 * t224 - 0.2
     #D1 * t382 * t683
      t729 = x3 * t25
      t740 = 0.2D1 * t214 * t41 + x1 * t213 * t729 - t214 * t729 - 0.2D1
     # * t703 * t682 * x2 + t582 + t382 + t406 - 0.2D1 * t143 - 0.2D1 * 
     #t729 + 0.2D1 * t382 * t684 + t41 - z - x1 - t596 + 0.4D1 * t680
      t742 = 0.1D1 / (t725 + t740)
      t744 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t521, x4)
      t752 = log(-0.4D1 * t224 * t182 * t379 * t469 * t526 * t538)
      t756 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t521, x4)
      t759 = -t595 * t742 * t744 + (-t114 - 0.90D2 * t752) * t383 * t742
     # * t756
      t762 = t20 * t759 * t246 / 0.720D3
      t763 = FJET(XB1, XB2, s, t677, -t689, -t690, t696, t593, t762)
      t768 = t759 * t90 * t179 * t245
      t771 = FJET(XB1, XB2, s, t696, -t690, -t689, t677, t593, t762)
      t776 = FJET(XB1, XB2, s, -t377, t375, 0.0D0, 0.0D0, 0.0D0, t663)
      t789 = t615 / 0.720D3 + (-0.90D2 * t20 * t629 + 0.180D3 * t6 * 0.3
     #141592653589793D1 * t634 - t640) * t179 * t245 / 0.720D3
      t790 = FJET(XB1, XB2, s, -t377, -t587, 0.0D0, -t584, t593, t789)
      t792 = FJET(XB1, XB2, s, -t524, 0.0D0, t522, 0.0D0, 0.0D0, t671)
      t794 = FJET(XB1, XB2, s, -t587, -t377, -t584, 0.0D0, t593, t789)
      t796 = FJET(XB1, XB2, s, -t584, 0.0D0, -t587, -t377, t593, t789)
      t798 = FJET(XB1, XB2, s, -t689, t677, t696, -t690, t593, t762)
      t803 = FJET(XB1, XB2, s, -t690, t696, t677, -t689, t593, t762)
      rrgg2gght7s2e1 = t371 * t370 + t373 * t370 + t512 * t511 + t514 * 
     #t511 + t516 * t370 + t578 * t577 + t580 * t577 + t646 * t645 + t64
     #8 * t370 + t664 * t663 + t672 * t671 + t763 * t5 * 0.3141592653589
     #793D1 * t768 / 0.720D3 + t771 * t5 * 0.3141592653589793D1 * t768 /
     # 0.720D3 + t776 * t663 + t790 * t789 + t792 * t671 + t794 * t789 +
     # t796 * t789 + t798 * t5 * 0.3141592653589793D1 * t768 / 0.720D3 +
     # t803 * t5 * 0.3141592653589793D1 * t768 / 0.720D3

      end function



      doubleprecision function rrgg2gght7s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t17 = log(0.4D1 * t11 * t14)
      t18 = t17 ** 2
      t19 = -0.1D1 + x3
      t20 = 0.1D1 / t19
      t24 = log(-0.4D1 * t11 * t14 * t20)
      t25 = t24 ** 2
      t26 = x3 * z
      t27 = 0.2D1 * t26
      t28 = cos(t12)
      t30 = Sqrt(-t26 * t19)
      t34 = 0.1D1 / (-0.1D1 - t27 + x3 + 0.2D1 * t28 * t30)
      t41 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t44 = lh * t5
      t45 = 0.3141592653589793D1 * t7
      t52 = 0.3141592653589793D1 * t41
      t55 = lh ** 2
      t56 = 0.180D3 * t55
      t57 = 0.3141592653589793D1 ** 2
      t58 = 0.30D2 * t57
      t59 = t56 - t58
      t60 = t59 * t5
      t62 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t69 = 0.1D1 / x3
      t72 = t10 * t14
      t74 = log(0.4D1 * t72)
      t77 = t74 ** 2
      t83 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t94 = -t59
      t100 = 0.180D3 * lh
      t107 = 0.90D2 * t41
      t108 = x2 ** 2
      t109 = t108 * x3
      t112 = log(0.4D1 * t109 * t72)
      t116 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t118 = -0.1D1 + x2
      t119 = t72 * t118
      t122 = log(-0.4D1 * t109 * t119)
      t125 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t127 = t72 * t20
      t130 = log(-0.4D1 * t109 * t127)
      t138 = 0.1D1 / x2
      t142 = t10 * t108
      t143 = t14 * t118
      t146 = log(-0.4D1 * t142 * t143)
      t148 = t146 ** 2
      t151 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t154 = log(0.4D1 * t142 * t14)
      t156 = t154 ** 2
      t169 = 0.3141592653589793D1 * (-t125 + t7)
      t174 = x1 ** 2
      t175 = x3 * t174
      t178 = log(0.4D1 * t175 * t72)
      t182 = log(-0.4D1 * t175 * t127)
      t189 = t7 * t34
      t196 = 0.1D1 / x1
      t203 = t69 * t196 * t138
      t206 = t108 * t174
      t209 = log(-0.4D1 * t206 * t119)
      t213 = log(0.4D1 * t206 * t72)
      t224 = t174 * t14
      t227 = log(0.4D1 * t224 * t10)
      t229 = t227 ** 2
      t240 = t94 * t5
      t245 = -(0.90D2 * t6 * t7 * (t18 / 0.2D1 + t25 * t34 / 0.2D1) + (0
     #.90D2 * t6 * t41 - 0.180D3 * t44 * t45) * (-t17 - t24 * t34) + (-0
     #.180D3 * t44 * t52 + t60 * t45 + 0.90D2 * t6 * t62) * (0.1D1 + t34
     #)) * t69 / 0.2880D4 + (-0.180D3 * t74 * lh - 0.45D2 * t77 - t56 + 
     #t58) * t5 * t52 / 0.2880D4 - t6 * t83 / 0.32D2 + (0.90D2 * t77 * l
     #h - 0.60D2 * lh * t57 + 0.2884936567583026D3 + 0.120D3 * t55 * lh 
     #+ 0.15D2 * t77 * t74 - t74 * t94) * t5 * t45 / 0.2880D4 + (t100 + 
     #0.90D2 * t74) * t5 * 0.3141592653589793D1 * t62 / 0.2880D4 + t6 * 
     #(-t107 - (-t100 - 0.90D2 * t112) * t7 + 0.90D2 * t116 - (t100 + 0.
     #90D2 * t122) * t125 + (-t107 + (t100 + 0.90D2 * t130) * t7) * t34)
     # * t69 * t138 / 0.1440D4 - (0.90D2 * t6 * (t146 * t116 - t148 * t1
     #25 / 0.2D1 - t151 - t154 * t41 + t156 * t7 / 0.2D1 + t62) - 0.180D
     #3 * t44 * 0.3141592653589793D1 * (-t116 + t146 * t125 + t41 - t154
     # * t7) + t60 * t169) * t138 / 0.1440D4 + (-0.90D2 * t6 * (t41 - t1
     #78 * t7 + (t41 - t182 * t7) * t34) + 0.180D3 * t44 * 0.31415926535
     #89793D1 * (t189 + t7)) * t69 * t196 / 0.1440D4 + t6 * (0.90D2 * t1
     #25 - 0.90D2 * t7 - 0.90D2 * t189) * t203 / 0.720D3 + (-0.90D2 * t6
     # * (-t116 + t209 * t125 + t41 - t213 * t7) + 0.180D3 * t44 * t169)
     # * t196 * t138 / 0.720D3 + (-0.90D2 * t6 * (-t227 * t41 + t229 * t
     #7 / 0.2D1 + t62) + 0.180D3 * t44 * 0.3141592653589793D1 * (t41 - t
     #227 * t7) + t240 * t45) * t196 / 0.1440D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t245)
      t248 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t245)
      t250 = t2 * x1
      t251 = -0.1D1 + x1
      t252 = t2 * t251
      t253 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t254 = t175 * t14
      t255 = 0.1D1 / t8
      t256 = t251 ** 2
      t257 = t255 * t256
      t258 = x1 * z
      t259 = -z - x1 + t258
      t260 = 0.1D1 / t259
      t261 = t257 * t260
      t264 = log(-0.4D1 * t254 * t261)
      t265 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t271 = log(0.4D1 * t254 * t257 * t260 * t20)
      t274 = x3 * x1
      t275 = t274 * z
      t278 = x3 * t259
      t280 = Sqrt(t278 * t19)
      t284 = 0.1D1 / (0.2D1 * t275 - 0.2D1 * t274 - t27 - 0.1D1 + 0.2D1 
     #* t28 * t280 + x3)
      t290 = -t265 - t265 * t284
      t297 = (-0.90D2 * t6 * (-t253 + t264 * t265 - (t253 - t271 * t265)
     # * t284) + 0.180D3 * t44 * 0.3141592653589793D1 * t290) * t69 * t1
     #96 / 0.1440D4
      t302 = -t6 * t290 * t203 / 0.8D1
      t303 = t206 * t14
      t306 = log(-0.4D1 * t303 * t261)
      t311 = 0.3141592653589793D1 * t265
      t317 = (-0.90D2 * t6 * (-t253 + t306 * t265) - 0.180D3 * t44 * t31
     #1) * t196 * t138 / 0.720D3
      t320 = log(-0.4D1 * t224 * t261)
      t322 = t320 ** 2
      t325 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t326 = t320 * t253 - t322 * t265 / 0.2D1 - t325
      t330 = -t253 + t320 * t265
      t334 = t240 * t311
      t338 = t297 + t302 + t317 + (-0.90D2 * t6 * t326 + 0.180D3 * t44 *
     # 0.3141592653589793D1 * t330 - t334) * t196 / 0.1440D4
      t339 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t250, -t252, 0.0D0, t338)
      t341 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t252, t250, 0.0D0, t338)
      t343 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t245)
      t345 = x2 * x3
      t346 = 0.1D1 - x3 + t345
      t347 = 0.1D1 / t346
      t348 = t345 * t347
      t349 = t2 * t348
      t351 = t2 * t19 * t347
      t352 = t345 * z
      t353 = t118 * t19
      t355 = Sqrt(t26 * t353)
      t359 = 0.1D1 / (t352 - t27 - 0.1D1 + x3 + 0.2D1 * t28 * t355)
      t360 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t348, x
     #4)
      t364 = t346 ** 2
      t370 = log(0.4D1 * t109 * t10 * t143 * t19 / t364)
      t374 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t348, x
     #4)
      t376 = 0.90D2 * t359 * t360 - (t100 + 0.90D2 * t370) * t359 * t374
      t383 = t196 * t138
      t386 = t6 * t359 * t374 * t69 * t383 / 0.8D1
      t387 = t6 * t376 * t69 * t138 / 0.1440D4 + t386
      t388 = FJET(XB1, XB2, s, 0.0D0, t349, 0.0D0, -t351, 0.0D0, t387)
      t390 = FJET(XB1, XB2, s, 0.0D0, -t351, 0.0D0, t349, 0.0D0, t387)
      t392 = x1 * x2
      t394 = t2 * t392 * t260
      t397 = t118 * s * t1 * x1
      t398 = t1 ** 2
      t403 = s * t398 * x2 * x1 * t251 * t260
      t405 = -0.90D2 * t259
      t406 = t392 * z
      t408 = 0.1D1 / (z + x1 - t258 - t392 + t406)
      t411 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t414 = t6 * t405 * t408 * t411 * t69 * t383
      t415 = t259 * t408
      t416 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t423 = log(0.4D1 * t303 * t255 * t260 * t256 * t118)
      t427 = -t415 * t416 + t423 * t259 * t408 * t411
      t433 = 0.180D3 * t44 * 0.3141592653589793D1 * t415 * t411
      t438 = -t414 / 0.720D3 + (-0.90D2 * t6 * t427 - t433) * t196 * t13
     #8 / 0.720D3
      t439 = FJET(XB1, XB2, s, 0.0D0, -t394, -t252, -t397, t403, t438)
      t441 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t245)
      t453 = t297 + t302 + t317 + (-0.90D2 * t6 * t326 + 0.180D3 * t44 *
     # 0.3141592653589793D1 * t330 - t334) * t196 / 0.1440D4
      t454 = FJET(XB1, XB2, s, t250, -t252, 0.0D0, 0.0D0, 0.0D0, t453)
      t461 = t6 * t376 * t69 * t138 / 0.1440D4 + t386
      t462 = FJET(XB1, XB2, s, t349, 0.0D0, -t351, 0.0D0, 0.0D0, t461)
      t467 = t19 * s * t1 * t251 * t347
      t468 = x2 * z
      t469 = t274 * x2
      t470 = t274 * t468
      t472 = Sqrt(-t278 * t353)
      t473 = t28 * t472
      t474 = t473 * x2
      t476 = z + x1 - t258 - t468 - t392 + t406 - t26 - t274 + t275 + t3
     #52 + t469 - t470 + t109 + 0.2D1 * t474
      t479 = t250 * t476 * t260 * t347
      t480 = t252 * t348
      t486 = t250 * x2 * (-t26 - t274 + t275 + t352 + t469 - t470 - 0.1D
     #1 + x3 + 0.2D1 * t473) * t260 * t347
      t487 = -t405
      t491 = t174 * t8
      t494 = x3 * t8
      t506 = x1 * t28
      t517 = 0.4D1 * t470 - 0.4D1 * t175 * t468 + 0.2D1 * t491 * t345 - 
     #t206 * t494 + x1 * t108 * t494 + 0.2D1 * t206 * t26 - 0.2D1 * t258
     # * t473 - t258 * t109 - 0.3D1 * t274 * x2 * t8 - 0.2D1 * t506 * t4
     #72 * x2 + 0.2D1 * t258 * t474 - t469 - t206 * x3 + t345 * t8 + 0.2
     #D1 * z * t28 * t472
      t531 = -0.2D1 * t491 * x3 + 0.2D1 * t175 * x2 + 0.4D1 * t175 * z +
     # 0.4D1 * t274 * t8 + 0.2D1 * t506 * t472 - z - x1 - 0.2D1 * t494 -
     # 0.2D1 * t175 - t406 - 0.5D1 * t275 + t392 + t26 + t258 + t274
      t533 = 0.1D1 / (t517 + t531)
      t536 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t348, x4)
      t540 = t6 * t487 * t533 * t536 * t69 * t383 / 0.720D3
      t541 = FJET(XB1, XB2, s, t467, -t479, -t480, t486, t403, t540)
      t543 = 0.3141592653589793D1 * t487
      t546 = t533 * t536 * t203
      t549 = FJET(XB1, XB2, s, t486, -t480, -t479, t467, t403, t540)
      t554 = FJET(XB1, XB2, s, -t252, t250, 0.0D0, 0.0D0, 0.0D0, t453)
      t563 = -t414 / 0.720D3 + (-0.90D2 * t6 * t427 - t433) * t196 * t13
     #8 / 0.720D3
      t564 = FJET(XB1, XB2, s, -t252, -t397, 0.0D0, -t394, t403, t563)
      t566 = FJET(XB1, XB2, s, -t351, 0.0D0, t349, 0.0D0, 0.0D0, t461)
      t568 = FJET(XB1, XB2, s, -t397, -t252, -t394, 0.0D0, t403, t563)
      t570 = FJET(XB1, XB2, s, -t394, 0.0D0, -t397, -t252, t403, t563)
      t572 = FJET(XB1, XB2, s, -t479, t467, t486, -t480, t403, t540)
      t577 = FJET(XB1, XB2, s, -t480, t486, t467, -t479, t403, t540)
      rrgg2gght7s2e0 = t246 * t245 + t248 * t245 + t339 * t338 + t341 * 
     #t338 + t343 * t245 + t388 * t387 + t390 * t387 + t439 * t438 + t44
     #1 * t245 + t454 * t453 + t462 * t461 + t541 * t5 * t543 * t546 / 0
     #.720D3 + t549 * t5 * t543 * t546 / 0.720D3 + t554 * t453 + t564 * 
     #t563 + t566 * t461 + t568 * t563 + t570 * t563 + t572 * t5 * t543 
     #* t546 / 0.720D3 + t577 * t5 * t543 * t546 / 0.720D3

      end function



      doubleprecision function rrgg2gght7s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t17 = log(0.4D1 * t11 * t14)
      t18 = -0.1D1 + x3
      t23 = log(-0.4D1 * t11 * t14 / t18)
      t24 = x3 * z
      t25 = 0.2D1 * t24
      t26 = cos(t12)
      t28 = Sqrt(-t24 * t18)
      t32 = 0.1D1 / (-0.1D1 - t25 + x3 + 0.2D1 * t26 * t28)
      t38 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t41 = lh * t5
      t42 = 0.3141592653589793D1 * t7
      t44 = 0.180D3 * t41 * t42
      t49 = 0.1D1 / x3
      t55 = log(0.4D1 * t10 * t14)
      t64 = t55 ** 2
      t66 = lh ** 2
      t68 = 0.3141592653589793D1 ** 2
      t74 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t77 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t78 = t7 * t32
      t82 = 0.1D1 / x2
      t86 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t87 = x2 ** 2
      t88 = t10 * t87
      t89 = -0.1D1 + x2
      t93 = log(-0.4D1 * t88 * t14 * t89)
      t97 = log(0.4D1 * t88 * t14)
      t102 = -t77 + t7
      t109 = 0.1D1 / x1
      t114 = x1 ** 2
      t115 = t114 * t14
      t118 = log(0.4D1 * t115 * t10)
      t131 = -(0.90D2 * t6 * t7 * (-t17 - t23 * t32) + (0.90D2 * t6 * t3
     #8 - t44) * (0.1D1 + t32)) * t49 / 0.2880D4 + (0.180D3 * lh + 0.90D
     #2 * t55) * t5 * 0.3141592653589793D1 * t38 / 0.2880D4 + (-0.180D3 
     #* t55 * lh - 0.45D2 * t64 - 0.180D3 * t66 + 0.30D2 * t68) * t5 * t
     #42 / 0.2880D4 - t6 * t74 / 0.32D2 + t6 * (0.90D2 * t77 - 0.90D2 * 
     #t7 - 0.90D2 * t78) * t49 * t82 / 0.1440D4 - (0.90D2 * t6 * (-t86 +
     # t93 * t77 + t38 - t97 * t7) - 0.180D3 * t41 * 0.3141592653589793D
     #1 * t102) * t82 / 0.1440D4 - t6 * t102 * t109 * t82 / 0.8D1 + (-0.
     #90D2 * t6 * (t38 - t118 * t7) + t44) * t109 / 0.1440D4 - t6 * (t78
     # + t7) * t49 * t109 / 0.16D2
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t131)
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t131)
      t136 = t2 * x1
      t137 = -0.1D1 + x1
      t138 = t2 * t137
      t139 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t143 = t6 * t139 * t109 * t82 / 0.8D1
      t144 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t146 = x1 * z
      t147 = -z - x1 + t146
      t148 = 0.1D1 / t147
      t150 = t137 ** 2
      t154 = log(-0.4D1 * t115 / t8 * t148 * t150)
      t156 = -t144 + t154 * t139
      t161 = 0.180D3 * t41 * 0.3141592653589793D1 * t139
      t165 = x3 * x1
      t171 = Sqrt(x3 * t147 * t18)
      t181 = t6 * (-t139 - t139 / (0.2D1 * t165 * z - 0.2D1 * t165 - t25
     # - 0.1D1 + 0.2D1 * t26 * t171 + x3)) * t49 * t109 / 0.16D2
      t182 = t143 + (-0.90D2 * t6 * t156 - t161) * t109 / 0.1440D4 - t18
     #1
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t136, -t138, 0.0D0, t182)
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t138, t136, 0.0D0, t182)
      t187 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t131)
      t189 = x2 * x3
      t191 = 0.1D1 / (0.1D1 - x3 + t189)
      t192 = t189 * t191
      t193 = t2 * t192
      t195 = t2 * t18 * t191
      t199 = Sqrt(t24 * t89 * t18)
      t203 = 0.1D1 / (t189 * z - t25 - 0.1D1 + x3 + 0.2D1 * t26 * t199)
      t205 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t192, x
     #4)
      t209 = t6 * t203 * t205 * t49 * t82 / 0.16D2
      t210 = FJET(XB1, XB2, s, 0.0D0, t193, 0.0D0, -t195, 0.0D0, t209)
      t215 = t203 * t205 * t49 * t82
      t218 = FJET(XB1, XB2, s, 0.0D0, -t195, 0.0D0, t193, 0.0D0, t209)
      t223 = x1 * x2
      t225 = t2 * t223 * t148
      t228 = t89 * s * t1 * x1
      t229 = t1 ** 2
      t234 = s * t229 * x2 * x1 * t137 * t148
      t239 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t242 = 0.1D1 / (z + x1 - t146 - t223 + t223 * z) * t239 * t109 * t
     #82
      t244 = t6 * t147 * t242 / 0.8D1
      t245 = FJET(XB1, XB2, s, 0.0D0, -t225, -t138, -t228, t234, t244)
      t247 = 0.3141592653589793D1 * t147
      t251 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t131)
      t259 = t143 + (-0.90D2 * t6 * t156 - t161) * t109 / 0.1440D4 - t18
     #1
      t260 = FJET(XB1, XB2, s, t136, -t138, 0.0D0, 0.0D0, 0.0D0, t259)
      t262 = FJET(XB1, XB2, s, t193, 0.0D0, -t195, 0.0D0, 0.0D0, t209)
      t267 = FJET(XB1, XB2, s, -t138, t136, 0.0D0, 0.0D0, 0.0D0, t259)
      t269 = FJET(XB1, XB2, s, -t138, -t228, 0.0D0, -t225, t234, t244)
      t274 = FJET(XB1, XB2, s, -t195, 0.0D0, t193, 0.0D0, 0.0D0, t209)
      t279 = FJET(XB1, XB2, s, -t228, -t138, -t225, 0.0D0, t234, t244)
      t284 = FJET(XB1, XB2, s, -t225, 0.0D0, -t228, -t138, t234, t244)
      rrgg2gght7s2em1 = t132 * t131 + t134 * t131 + t183 * t182 + t185 *
     # t182 + t187 * t131 + t210 * t5 * 0.3141592653589793D1 * t215 / 0.
     #16D2 + t218 * t5 * 0.3141592653589793D1 * t215 / 0.16D2 + t245 * t
     #5 * t247 * t242 / 0.8D1 + t251 * t131 + t260 * t259 + t262 * t5 * 
     #0.3141592653589793D1 * t215 / 0.16D2 + t267 * t259 + t269 * t5 * t
     #247 * t242 / 0.8D1 + t274 * t5 * 0.3141592653589793D1 * t215 / 0.1
     #6D2 + t279 * t5 * t247 * t242 / 0.8D1 + t284 * t5 * t247 * t242 / 
     #0.8D1

      end function



      doubleprecision function rrgg2gght7s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = x3 * z
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t14 = Sqrt(-t8 * (-0.1D1 + x3))
      t25 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t31 = 0.1D1 / x1
      t35 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t39 = z ** 2
      t42 = Sin(t10)
      t43 = t42 ** 2
      t46 = log(0.4D1 / t39 / z * t43)
      t53 = -t6 * t7 * (0.1D1 + 0.1D1 / (-0.1D1 - 0.2D1 * t8 + x3 + 0.2D
     #1 * t11 * t14)) / x3 / 0.32D2 - t6 * (-t25 + t7) / x2 / 0.16D2 - t
     #6 * t7 * t31 / 0.16D2 - t6 * t35 / 0.32D2 + (0.180D3 * lh + 0.90D2
     # * t46) * t5 * 0.3141592653589793D1 * t7 / 0.2880D4
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t53)
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t53)
      t58 = t2 * x1
      t60 = t2 * (-0.1D1 + x1)
      t61 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, x
     #4)
      t64 = t6 * t61 * t31 / 0.16D2
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t58, -t60, 0.0D0, t64)
      t68 = 0.3141592653589793D1 * t61 * t31
      t71 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t60, t58, 0.0D0, t64)
      t75 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t53)
      t77 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t53)
      t79 = FJET(XB1, XB2, s, t58, -t60, 0.0D0, 0.0D0, 0.0D0, t64)
      t83 = FJET(XB1, XB2, s, -t60, t58, 0.0D0, 0.0D0, 0.0D0, t64)
      rrgg2gght7s2em2 = t54 * t53 + t56 * t53 + t65 * t5 * t68 / 0.16D2 
     #+ t71 * t5 * t68 / 0.16D2 + t75 * t53 + t77 * t53 + t79 * t5 * t68
     # / 0.16D2 + t83 * t5 * t68 / 0.16D2

      end function



      doubleprecision function rrgg2gght7s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t9 = t5 * 0.3141592653589793D1 * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = 0.3141592653589793D1 * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2gght7s2em3 = -t10 * t5 * t12 / 0.32D2 - t14 * t5 * t12 / 0.32
     #D2 - t17 * t5 * t12 / 0.32D2 - t20 * t5 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2gght7s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6
      rrgg2gght7s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh71J1
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
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t3 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = 0.1D1 - x2
      t12 = z + x1 * t9 * t4
      t14 = x1 * t4
      t15 = z + t14
      t16 = 0.1D1 / t15
      t17 = 0.1D1 - x3
      t22 = t5 * t4
      t23 = t3 * t22
      t24 = t8 * t7
      t26 = t17 ** 2
      t31 = t5 ** 2
      t32 = t31 * t4
      t34 = x1 ** 2
      t35 = t34 * x1
      t36 = x2 ** 2
      t38 = t15 ** 2
      t39 = 0.1D1 / t38
      t51 = 0.1D1 / t38 / t15
      t52 = t12 * t51
      t57 = t3 * t22 * t8
      t58 = t12 * t39
      t65 = t17 * t12
      t85 = t31 ** 2
      t87 = t36 ** 2
      t89 = t34 ** 2
      t90 = t8 ** 2
      t92 = t38 ** 2
      t100 = -0.2D1 * t6 * t8 * t12 * t16 * t17 + 0.2D1 * t23 * t24 * t1
     #2 * t16 * t26 + t3 * t32 * t35 * t36 * t8 * t39 - 0.3D1 * t23 * t3
     #4 * x2 * t7 * t16 + t3 * t32 * t24 * t52 * t36 * t34 - 0.3D1 * t57
     # * t58 * x2 * x1 - t3 * t31 * t24 * t65 * t39 * x2 * x1 + 0.2D1 * 
     #t3 * t31 * t34 * t8 * t17 * x2 * t16 + 0.4D1 * t3 * t14 - 0.6D1 * 
     #t2 * t31 * t36 * t34 * t8 * t39 - 0.2D1 * t2 * t85 * t87 * t89 * t
     #90 / t92 + t23 * x1 * t8 * t26
      t101 = x1 * t7
      t110 = cos(x4 * 0.3141592653589793D1)
      t115 = Sqrt(x3 * t9 * t15 * x2 * t17)
      t118 = t17 * t9 * t15 + x2 * x3 + 0.2D1 * t110 * t115
      t119 = t118 ** 2
      t180 = -0.3D1 * t6 * t101 * t17 + 0.2D1 * t23 * t35 * t39 * t119 -
     # 0.2D1 * t6 * t34 * t16 * t118 + 0.4D1 * t2 * t5 * x2 * t101 * t16
     # + 0.4D1 * t2 * t31 * t5 * t36 * x2 * t35 * t24 * t51 + 0.4D1 * t3
     # * t4 * t7 * t12 * t16 - 0.2D1 * t2 - 0.3D1 * t3 * t5 * t7 * t58 *
     # x1 * t118 - t3 * t22 * t34 * t16 * t118 * t7 * t17 + t3 * t22 * t
     #7 * t52 * t34 * t119 + 0.2D1 * t3 * t31 * t8 * t52 * t34 * t118 * 
     #x2 - t3 * t31 * t35 * t39 * t118 * x2 * t7 - t57 * t65 * t39 * x1 
     #* t118
      rrgg2ggh71J1 = -0.9D1 / 0.2D1 * wd * (t100 + t180) / s / z / 0.314
     #1592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh71J2
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
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t10 = t3 * t6 * t8
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = x1 * t4
      t16 = z + t15
      t17 = t16 ** 2
      t18 = 0.1D1 / t17
      t19 = t14 * t18
      t22 = t10 * t19 * x2 * x1
      t24 = t5 ** 2
      t25 = t8 * t7
      t28 = 0.1D1 - x3
      t29 = t28 * t14
      t33 = t3 * t24 * t25 * t29 * t18 * x2 * x1
      t34 = x1 ** 2
      t38 = 0.1D1 / t16
      t42 = 0.2D1 * t3 * t24 * t34 * t8 * t28 * x2 * t38
      t48 = cos(x4 * 0.3141592653589793D1)
      t53 = Sqrt(x3 * t11 * t16 * x2 * t28)
      t56 = t28 * t11 * t16 + x2 * x3 + 0.2D1 * t48 * t53
      t59 = t10 * t29 * t18 * x1 * t56
      t64 = t3 * t5 * t7 * t19 * x1 * t56
      t66 = 0.2D1 * t2
      t67 = t3 * t6
      t71 = t67 * t34 * x2 * t7 * t38
      t74 = t28 ** 2
      t78 = 0.2D1 * t67 * t25 * t14 * t38 * t74
      t79 = t24 * t4
      t81 = t34 * x1
      t82 = x2 ** 2
      t86 = t3 * t79 * t81 * t82 * t8 * t18
      t87 = t3 * t5
      t92 = 0.2D1 * t87 * t8 * t14 * t38 * t28
      t96 = 0.1D1 / t17 / t16
      t97 = t14 * t96
      t100 = t3 * t79 * t25 * t97 * t82 * t34
      t102 = 0.4D1 * t3 * t15
      t103 = -0.3D1 * t22 - t33 + t42 - t59 - 0.3D1 * t64 - t66 - 0.3D1 
     #* t71 + t78 + t86 - t92 + t100 + t102
      t106 = x1 * t7
      t109 = 0.4D1 * t2 * t5 * x2 * t106 * t38
      t117 = 0.4D1 * t2 * t24 * t5 * t82 * x2 * t81 * t25 * t96
      t122 = 0.4D1 * t3 * t4 * t7 * t14 * t38
      t128 = 0.6D1 * t2 * t24 * t82 * t34 * t8 * t18
      t129 = t24 ** 2
      t131 = t82 ** 2
      t133 = t34 ** 2
      t134 = t8 ** 2
      t136 = t17 ** 2
      t140 = 0.2D1 * t2 * t129 * t131 * t133 * t134 / t136
      t143 = t67 * x1 * t8 * t74
      t145 = t87 * t106 * t28
      t148 = t56 ** 2
      t151 = 0.2D1 * t67 * t81 * t18 * t148
      t155 = 0.2D1 * t87 * t34 * t38 * t56
      t161 = t3 * t6 * t34 * t38 * t56 * t7 * t28
      t166 = t3 * t6 * t7 * t97 * t34 * t148
      t173 = 0.2D1 * t3 * t24 * t8 * t97 * t34 * t56 * x2
      t179 = t3 * t24 * t81 * t18 * t56 * x2 * t7
      t180 = t109 + t117 + t122 - t128 - t140 + t143 - 0.3D1 * t145 + t1
     #51 - t155 - t161 + t166 + t173 - t179
      t187 = 0.6D1 * t22 + t33 - t42 + t59 + 0.6D1 * t64 + t66 + 0.6D1 *
     # t71 - t78 - t86 + t92 - t100 - t102
      t189 = -t109 - t117 - t122 + t128 + t140 - t143 + 0.6D1 * t145 - t
     #151 + t155 + t161 - t166 - t173 + t179
      rrgg2ggh71J2 = -0.9D1 / 0.2D1 * (0.2D1 * wd * (t103 + t180) + wd *
     # (t187 + t189)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh71J3
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
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t10 = t3 * t6 * t8
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = x1 * t4
      t16 = z + t15
      t17 = t16 ** 2
      t18 = 0.1D1 / t17
      t19 = t14 * t18
      t22 = t10 * t19 * x2 * x1
      t24 = t5 ** 2
      t25 = t8 * t7
      t28 = 0.1D1 - x3
      t29 = t28 * t14
      t33 = t3 * t24 * t25 * t29 * t18 * x2 * x1
      t34 = x1 ** 2
      t38 = 0.1D1 / t16
      t42 = 0.2D1 * t3 * t24 * t34 * t8 * t28 * x2 * t38
      t49 = cos(x4 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t11 * t16 * x2 * t28)
      t57 = t28 * t11 * t16 + x2 * x3 + 0.2D1 * t49 * t54
      t61 = t3 * t6 * t34 * t38 * t57 * t7 * t28
      t65 = 0.1D1 / t17 / t16
      t66 = t14 * t65
      t67 = t57 ** 2
      t70 = t3 * t6 * t7 * t66 * t34 * t67
      t71 = t24 * t4
      t74 = x2 ** 2
      t77 = t3 * t71 * t25 * t66 * t74 * t34
      t84 = 0.2D1 * t3 * t24 * t8 * t66 * t34 * t57 * x2
      t86 = t34 * x1
      t90 = t3 * t71 * t86 * t74 * t8 * t18
      t91 = t3 * t6
      t95 = t91 * t34 * x2 * t7 * t38
      t98 = t28 ** 2
      t102 = 0.2D1 * t91 * t25 * t14 * t38 * t98
      t103 = t3 * t5
      t108 = 0.2D1 * t103 * t8 * t14 * t38 * t28
      t110 = 0.4D1 * t3 * t15
      t111 = -0.3D1 * t22 - t33 + t42 - t61 + t70 + t77 + t84 + t90 - 0.
     #3D1 * t95 + t102 - t108 + t110
      t114 = x1 * t7
      t117 = 0.4D1 * t2 * t5 * x2 * t114 * t38
      t125 = 0.4D1 * t2 * t24 * t5 * t74 * x2 * t86 * t25 * t65
      t130 = 0.4D1 * t3 * t4 * t7 * t14 * t38
      t136 = 0.6D1 * t2 * t24 * t74 * t34 * t8 * t18
      t137 = t24 ** 2
      t139 = t74 ** 2
      t141 = t34 ** 2
      t142 = t8 ** 2
      t144 = t17 ** 2
      t148 = 0.2D1 * t2 * t137 * t139 * t141 * t142 / t144
      t151 = t91 * x1 * t8 * t98
      t153 = t103 * t114 * t28
      t158 = 0.2D1 * t91 * t86 * t18 * t67
      t162 = 0.2D1 * t103 * t34 * t38 * t57
      t163 = 0.2D1 * t2
      t169 = t3 * t24 * t86 * t18 * t57 * x2 * t7
      t173 = t10 * t29 * t18 * x1 * t57
      t178 = t3 * t5 * t7 * t19 * x1 * t57
      t180 = t117 + t125 + t130 - t136 - t148 + t151 - 0.3D1 * t153 + t1
     #58 - t162 - t163 - t169 - t173 - 0.3D1 * t178
      t186 = 0.6D1 * t22 + t33 - t42 + t61 - t70 - t77 - t84 - t90 + 0.6
     #D1 * t95 - t102 + t108 - t110
      t189 = -t117 - t125 - t130 + t136 + t148 - t151 + 0.6D1 * t153 - t
     #158 + t162 + t163 + t169 + t173 + 0.6D1 * t178
      rrgg2ggh71J3 = -0.9D1 / 0.2D1 * (0.3D1 * wd * (t111 + t180) + 0.2D
     #1 * wd * (t186 + t189)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh71J4
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
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t11 = 0.1D1 - x2
      t14 = z + x1 * t11 * t4
      t15 = x1 * t4
      t16 = z + t15
      t17 = t16 ** 2
      t19 = 0.1D1 / t17 / t16
      t20 = t14 * t19
      t21 = x1 ** 2
      t22 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t11 * t16 * x2 * t22)
      t35 = t22 * t11 * t16 + x2 * x3 + 0.2D1 * t27 * t32
      t40 = 0.2D1 * t3 * t6 * t8 * t20 * t21 * t35 * x2
      t41 = t5 * t4
      t43 = t3 * t41 * t8
      t44 = 0.1D1 / t17
      t45 = t14 * t44
      t48 = t43 * t45 * x2 * x1
      t50 = t8 * t7
      t53 = t22 * t14
      t57 = t3 * t6 * t50 * t53 * t44 * x2 * x1
      t61 = 0.1D1 / t16
      t65 = 0.2D1 * t3 * t6 * t21 * t8 * t22 * x2 * t61
      t66 = 0.2D1 * t2
      t72 = t3 * t41 * t21 * t61 * t35 * t7 * t22
      t75 = t35 ** 2
      t78 = t3 * t41 * t7 * t20 * t21 * t75
      t79 = t21 * x1
      t85 = t3 * t6 * t79 * t44 * t35 * x2 * t7
      t89 = t43 * t53 * t44 * x1 * t35
      t94 = t3 * t5 * t7 * t45 * x1 * t35
      t96 = t3 * t5
      t101 = 0.2D1 * t96 * t8 * t14 * t61 * t22
      t102 = t3 * t41
      t104 = t22 ** 2
      t108 = 0.2D1 * t102 * t50 * t14 * t61 * t104
      t109 = t40 - 0.3D1 * t48 - t57 + t65 - t66 - t72 + t78 - t85 - t89
     # - 0.3D1 * t94 - t101 + t108
      t110 = t6 * t4
      t112 = x2 ** 2
      t116 = t3 * t110 * t79 * t112 * t8 * t44
      t120 = t102 * t21 * x2 * t7 * t61
      t123 = 0.4D1 * t3 * t15
      t128 = t3 * t110 * t50 * t20 * t112 * t21
      t131 = x1 * t7
      t134 = 0.4D1 * t2 * t5 * x2 * t131 * t61
      t142 = 0.4D1 * t2 * t6 * t5 * t112 * x2 * t79 * t50 * t19
      t147 = 0.4D1 * t3 * t4 * t7 * t14 * t61
      t153 = 0.6D1 * t2 * t6 * t112 * t21 * t8 * t44
      t154 = t6 ** 2
      t156 = t112 ** 2
      t158 = t21 ** 2
      t159 = t8 ** 2
      t161 = t17 ** 2
      t165 = 0.2D1 * t2 * t154 * t156 * t158 * t159 / t161
      t168 = t102 * x1 * t8 * t104
      t170 = t96 * t131 * t22
      t175 = 0.2D1 * t102 * t79 * t44 * t75
      t179 = 0.2D1 * t96 * t21 * t61 * t35
      t180 = t116 - 0.3D1 * t120 + t123 + t128 + t134 + t142 + t147 - t1
     #53 - t165 + t168 - 0.3D1 * t170 + t175 - t179
      t186 = -t40 + 0.6D1 * t48 + t57 - t65 + t66 + t72 - t78 + t85 + t8
     #9 + 0.6D1 * t94 + t101 - t108
      t189 = -t116 + 0.6D1 * t120 - t123 - t128 - t134 - t142 - t147 + t
     #153 + t165 - t168 + 0.6D1 * t170 - t175 + t179
      rrgg2ggh71J4 = -0.9D1 / 0.2D1 * (0.4D1 * wd * (t109 + t180) + 0.3D
     #1 * wd * (t186 + t189)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh71J5
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
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = x1 ** 2
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = 0.1D1 - x3
      t14 = x1 * t4
      t15 = z + t14
      t16 = 0.1D1 / t15
      t20 = 0.2D1 * t3 * t6 * t7 * t11 * t12 * x2 * t16
      t21 = t5 * t4
      t23 = t3 * t21 * t11
      t24 = 0.1D1 - x2
      t27 = z + x1 * t24 * t4
      t28 = t15 ** 2
      t29 = 0.1D1 / t28
      t30 = t27 * t29
      t33 = t23 * t30 * x2 * x1
      t35 = t11 * t10
      t38 = t12 * t27
      t42 = t3 * t6 * t35 * t38 * t29 * x2 * x1
      t43 = t6 * t4
      t47 = 0.1D1 / t28 / t15
      t48 = t27 * t47
      t49 = x2 ** 2
      t52 = t3 * t43 * t35 * t48 * t49 * t7
      t55 = x1 * t10
      t58 = 0.4D1 * t2 * t5 * x2 * t55 * t16
      t63 = x1 * t7
      t67 = 0.4D1 * t2 * t6 * t5 * t49 * x2 * t63 * t35 * t47
      t72 = 0.4D1 * t3 * t4 * t10 * t27 * t16
      t78 = 0.6D1 * t2 * t6 * t49 * t7 * t11 * t29
      t79 = t6 ** 2
      t81 = t49 ** 2
      t83 = t7 ** 2
      t84 = t11 ** 2
      t86 = t28 ** 2
      t90 = 0.2D1 * t2 * t79 * t81 * t83 * t84 / t86
      t91 = t3 * t21
      t93 = t12 ** 2
      t95 = t91 * x1 * t11 * t93
      t96 = t3 * t5
      t98 = t96 * t55 * t12
      t105 = cos(x4 * 0.3141592653589793D1)
      t110 = Sqrt(x3 * t24 * t15 * x2 * t12)
      t113 = t12 * t24 * t15 + x2 * x3 + 0.2D1 * t105 * t110
      t114 = t113 ** 2
      t117 = 0.2D1 * t91 * t63 * t29 * t114
      t118 = t20 - 0.3D1 * t33 - t42 + t52 + t58 + t67 + t72 - t78 - t90
     # + t95 - 0.3D1 * t98 + t117
      t122 = 0.2D1 * t96 * t7 * t16 * t113
      t126 = t23 * t38 * t29 * x1 * t113
      t131 = t3 * t5 * t10 * t30 * x1 * t113
      t136 = t91 * t7 * x2 * t10 * t16
      t142 = t3 * t43 * t63 * t49 * t11 * t29
      t147 = 0.2D1 * t91 * t35 * t27 * t16 * t93
      t152 = 0.2D1 * t96 * t11 * t27 * t16 * t12
      t158 = t3 * t21 * t7 * t16 * t113 * t10 * t12
      t163 = t3 * t21 * t10 * t48 * t7 * t114
      t170 = 0.2D1 * t3 * t6 * t11 * t48 * t7 * t113 * x2
      t176 = t3 * t6 * t63 * t29 * t113 * x2 * t10
      t178 = 0.4D1 * t3 * t14
      t179 = 0.2D1 * t2
      t180 = -t122 - t126 - 0.3D1 * t131 - 0.3D1 * t136 + t142 + t147 - 
     #t152 - t158 + t163 + t170 - t176 + t178 - t179
      t186 = -t20 + 0.6D1 * t33 + t42 - t52 - t58 - t67 - t72 + t78 + t9
     #0 - t95 + 0.6D1 * t98 - t117
      t189 = t122 + t126 + 0.6D1 * t131 + 0.6D1 * t136 - t142 - t147 + t
     #152 + t158 - t163 - t170 + t176 - t178 + t179
      rrgg2ggh71J5 = -0.9D1 / 0.2D1 * (0.5D1 * wd * (t118 + t180) + 0.4D
     #1 * wd * (t186 + t189)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh71J6
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
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t3 * t6
      t8 = x1 ** 2
      t10 = 0.1D1 - x1
      t11 = x1 * t4
      t12 = z + t11
      t13 = 0.1D1 / t12
      t18 = t5 ** 2
      t19 = t18 * t4
      t21 = t8 * x1
      t22 = x2 ** 2
      t24 = t10 ** 2
      t25 = t12 ** 2
      t26 = 0.1D1 / t25
      t30 = t24 * t10
      t31 = 0.1D1 - x2
      t34 = z + x1 * t31 * t4
      t36 = 0.1D1 - x3
      t37 = t36 ** 2
      t42 = t3 * t5
      t50 = x1 * t10
      t60 = 0.1D1 / t25 / t12
      t75 = t18 ** 2
      t77 = t22 ** 2
      t79 = t8 ** 2
      t80 = t24 ** 2
      t82 = t25 ** 2
      t98 = cos(x4 * 0.3141592653589793D1)
      t103 = Sqrt(x3 * t31 * t12 * x2 * t36)
      t106 = t36 * t31 * t12 + x2 * x3 + 0.2D1 * t98 * t103
      t107 = t106 ** 2
      t111 = 0.6D1 * t7 * t8 * x2 * t10 * t13 - t3 * t19 * t21 * t22 * t
     #24 * t26 - 0.2D1 * t7 * t30 * t34 * t13 * t37 + 0.2D1 * t42 * t24 
     #* t34 * t13 * t36 - 0.4D1 * t2 * t5 * x2 * t50 * t13 - 0.4D1 * t2 
     #* t18 * t5 * t22 * x2 * t21 * t30 * t60 - 0.4D1 * t3 * t4 * t10 * 
     #t34 * t13 + 0.6D1 * t2 * t18 * t22 * t8 * t24 * t26 + 0.2D1 * t2 *
     # t75 * t77 * t79 * t80 / t82 - t7 * x1 * t24 * t37 + 0.6D1 * t42 *
     # t50 * t36 - 0.2D1 * t7 * t21 * t26 * t107
      t118 = t34 * t60
      t123 = t3 * t6 * t24
      t124 = t34 * t26
      t131 = t36 * t34
      t180 = 0.2D1 * t42 * t8 * t13 * t106 - t3 * t19 * t30 * t118 * t22
     # * t8 + 0.6D1 * t123 * t124 * x2 * x1 + t3 * t18 * t30 * t131 * t2
     #6 * x2 * x1 - 0.2D1 * t3 * t18 * t8 * t24 * t36 * x2 * t13 + t3 * 
     #t6 * t8 * t13 * t106 * t10 * t36 - t3 * t6 * t10 * t118 * t8 * t10
     #7 - 0.2D1 * t3 * t18 * t24 * t118 * t8 * t106 * x2 + t3 * t18 * t2
     #1 * t26 * t106 * x2 * t10 + t123 * t131 * t26 * x1 * t106 + 0.2D1 
     #* t2 + 0.6D1 * t3 * t5 * t10 * t124 * x1 * t106 - 0.4D1 * t3 * t11
      rrgg2ggh71J6 = -0.45D2 / 0.2D1 * wd * (t111 + t180) / s / z / 0.31
     #41592653589793D1

      end function
  
 