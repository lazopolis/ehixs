      subroutine rrgg2qqbarht8
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt8
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt8
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhard81J1  
      doubleprecision rrgg2qqbarhhard81J2  
      doubleprecision rrgg2qqbarhhard81J3  
      doubleprecision rrgg2qqbarhhard81J4  
      doubleprecision rrgg2qqbarhhard81J5  
      doubleprecision rrgg2qqbarhhard81J6  
      doubleprecision rrgg2qqbarhhardt8s1e1  
      doubleprecision rrgg2qqbarhhardt8s1e0  
      doubleprecision rrgg2qqbarhhardt8s1em1  
      doubleprecision rrgg2qqbarhhardt8s1em2  
      doubleprecision rrgg2qqbarhhardt8s1em3  
      doubleprecision rrgg2qqbarhhardt8s1em4  
      doubleprecision rrgg2qqbarhhardt8s2e1  
      doubleprecision rrgg2qqbarhhardt8s2e0  
      doubleprecision rrgg2qqbarhhardt8s2em1  
      doubleprecision rrgg2qqbarhhardt8s2em2  
      doubleprecision rrgg2qqbarhhardt8s2em3  
      doubleprecision rrgg2qqbarhhardt8s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt8s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x3
      t4 = t2 * t3
      t5 = t2 * x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = x1 ** 2
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t15 * x3 * t3
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t5, -t4, 
     #0.0D0, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t5, -t4, 
     #0.0D0, 0.0D0, 0.0D0)
      t27 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t5, -t4, 
     #0.0D0, 0.0D0, 0.0D0)
      t31 = pi * lh
      t37 = lh ** 2
      t39 = pi ** 2
      t41 = -0.180D3 * t37 + 0.30D2 * t39
      t42 = pi * t41
      t43 = t7 * t24
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t48 = 0.1D1 / x1
      t51 = x2 * t9
      t52 = t51 * t12
      t55 = log(-0.4D1 * t52 * t17)
      t64 = 0.1D1 / x2
      t65 = t64 * t48
      t68 = t15 * t12
      t69 = x3 * t3
      t72 = log(-0.4D1 * t68 * t69)
      t77 = t72 ** 2
      t80 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, t5, -t4, 
     #0.0D0, 0.0D0, 0.0D0)
      t93 = -0.60D2 * lh * t39 + 0.240D3 * zeta3 + 0.120D3 * t37 * lh
      t94 = pi * t93
      t106 = x2 * x3
      t110 = log(-0.4D1 * t106 * t68 * t3)
      t112 = t110 ** 2
      t127 = -(-0.90D2 * t8 * (-t20 * t21 + t23 * t24 / 0.2D1 + t27) + 0
     #.180D3 * t31 * t7 * (t21 - t20 * t24) + t44) * t46 * t48 / 0.720D3
     # - (-0.90D2 * t8 * (t21 - t55 * t24) + 0.180D3 * t31 * t43) * t46 
     #* t65 / 0.720D3 - (t42 * t7 * (t21 - t72 * t24) - 0.90D2 * t8 * (t
     #77 * t21 / 0.2D1 + t80 - t77 * t72 * t24 / 0.6D1 - t72 * t27) + t9
     #4 * t43 + 0.180D3 * t31 * t7 * (-t72 * t21 + t77 * t24 / 0.2D1 + t
     #27)) * t46 / 0.1440D4 + (-0.90D2 * t8 * (t110 * t21 - t112 * t24 /
     # 0.2D1 - t27) + 0.180D3 * t31 * t7 * (-t21 + t110 * t24) - t44) * 
     #t46 * t64 / 0.1440D4
      t128 = FJET(XB1, XB2, s, -t4, 0.0D0, t5, 0.0D0, 0.0D0, t127)
      t130 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t134 = log(0.4D1 * x3 * t12 * t15)
      t135 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t140 = t134 ** 2
      t143 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t147 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t152 = t7 * t135
      t153 = t94 * t152
      t165 = log(0.4D1 * t68)
      t166 = t165 ** 2
      t169 = t166 * t165
      t190 = t39 ** 2
      t191 = t37 ** 2
      t202 = rrgg2qqbarhhard81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t203 = t166 ** 2
      t212 = x3 * t9
      t215 = log(0.4D1 * t212 * t68)
      t217 = t215 ** 2
      t228 = t42 * t152
      t233 = t13 * t15
      t235 = log(0.4D1 * t233)
      t240 = t235 ** 2
      t262 = log(0.4D1 * t106 * t233)
      t275 = log(0.4D1 * t51 * t68)
      t277 = t275 ** 2
      t294 = log(0.4D1 * t106 * t68)
      t296 = t294 ** 2
      t311 = x2 * t12
      t314 = log(0.4D1 * t311 * t15)
      t319 = t314 ** 2
      t339 = -(t42 * t7 * (-t130 + t134 * t135) - 0.90D2 * t8 * (-t140 *
     # t130 / 0.2D1 - t143 + t140 * t134 * t135 / 0.6D1 + t134 * t147) -
     # t153 + 0.180D3 * t31 * t7 * (t134 * t130 - t140 * t135 / 0.2D1 - 
     #t147)) * t46 / 0.1440D4 + (0.180D3 * (t166 * t130 / 0.2D1 + t143 -
     # t169 * t135 / 0.6D1 - t165 * t147) * pi * lh + (-t165 * t130 + t1
     #66 * t135 / 0.2D1 + t147) * pi * t41 + (t130 - t165 * t135) * pi *
     # t93 + t135 * pi * (-0.480D3 * lh * zeta3 - t190 - 0.60D2 * t191 +
     # 0.60D2 * t37 * t39) - 0.90D2 * (-t169 * t130 / 0.6D1 + t166 * t14
     #7 / 0.2D1 - t165 * t143 + t202 + t203 * t135 / 0.24D2) * pi) * t7 
     #/ 0.1440D4 - (-0.90D2 * t8 * (t215 * t130 - t217 * t135 / 0.2D1 - 
     #t147) + 0.180D3 * t31 * t7 * (-t130 + t215 * t135) - t228) * t46 *
     # t48 / 0.720D3 - (t42 * t7 * (-t130 + t235 * t135) - 0.90D2 * t8 *
     # (-t240 * t130 / 0.2D1 - t143 + t240 * t235 * t135 / 0.6D1 + t235 
     #* t147) - t153 + 0.180D3 * t31 * t7 * (t235 * t130 - t240 * t135 /
     # 0.2D1 - t147)) * t48 / 0.720D3 - (-0.90D2 * t8 * (-t130 + t262 * 
     #t135) - 0.180D3 * t31 * t152) * t46 * t65 / 0.720D3 - (-0.90D2 * t
     #8 * (t275 * t130 - t277 * t135 / 0.2D1 - t147) + 0.180D3 * t31 * t
     #7 * (-t130 + t275 * t135) - t228) * t64 * t48 / 0.720D3 + (-0.90D2
     # * t8 * (-t294 * t130 + t296 * t135 / 0.2D1 + t147) + 0.180D3 * t3
     #1 * t7 * (t130 - t294 * t135) + t228) * t46 * t64 / 0.1440D4 - (t4
     #2 * t7 * (-t130 + t314 * t135) - 0.90D2 * t8 * (-t319 * t130 / 0.2
     #D1 - t143 + t319 * t314 * t135 / 0.6D1 + t314 * t147) - t153 + 0.1
     #80D3 * t31 * t7 * (t314 * t130 - t319 * t135 / 0.2D1 - t147)) * t6
     #4 / 0.1440D4
      t340 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t339)
      t343 = x2 * t1 * s
      t344 = -0.1D1 + x2
      t346 = t344 * t1 * s
      t347 = t68 * t344
      t350 = log(-0.4D1 * t106 * t347)
      t351 = t350 ** 2
      t352 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t343, -t
     #346, 0.0D0, 0.0D0, 0.0D0)
      t355 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t343, -t
     #346, 0.0D0, 0.0D0, 0.0D0)
      t357 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t343, -t
     #346, 0.0D0, 0.0D0, 0.0D0)
      t366 = t7 * t352
      t367 = t42 * t366
      t375 = log(-0.4D1 * t311 * t15 * t344)
      t380 = t375 ** 2
      t383 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, t343, -t
     #346, 0.0D0, 0.0D0, 0.0D0)
      t402 = t106 * t9
      t405 = log(-0.4D1 * t402 * t347)
      t418 = log(-0.4D1 * t51 * t347)
      t420 = t418 ** 2
      t435 = (-0.90D2 * t8 * (-t351 * t352 / 0.2D1 + t350 * t355 - t357)
     # + 0.180D3 * t31 * t7 * (t350 * t352 - t355) - t367) * t46 * t64 /
     # 0.1440D4 - (t42 * t7 * (t355 - t375 * t352) - 0.90D2 * t8 * (t380
     # * t355 / 0.2D1 + t383 - t380 * t375 * t352 / 0.6D1 - t375 * t357)
     # + t94 * t366 + 0.180D3 * t31 * t7 * (-t375 * t355 + t380 * t352 /
     # 0.2D1 + t357)) * t64 / 0.1440D4 - (-0.90D2 * t8 * (t355 - t405 * 
     #t352) + 0.180D3 * t31 * t366) * t46 * t65 / 0.720D3 - (-0.90D2 * t
     #8 * (-t418 * t355 + t357 + t420 * t352 / 0.2D1) + 0.180D3 * t31 * 
     #t7 * (-t418 * t352 + t355) + t367) * t64 * t48 / 0.720D3
      t436 = FJET(XB1, XB2, s, t343, 0.0D0, -t346, 0.0D0, 0.0D0, t435)
      t439 = -0.1D1 + x1
      t440 = t1 * t439
      t441 = x3 * s * t440
      t442 = x3 * x1
      t443 = t2 * t442
      t444 = t3 * s
      t445 = t444 * t440
      t447 = t444 * t1 * x1
      t448 = x1 * z
      t449 = 0.1D1 - x1 + t448
      t450 = 0.1D1 / t449
      t451 = t9 * t450
      t453 = t439 ** 2
      t458 = log(-0.4D1 * t451 * t12 * t15 * t453 * t69)
      t459 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t441, t
     #445, t443, -t447, 0.0D0)
      t461 = t458 ** 2
      t462 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t441, t
     #445, t443, -t447, 0.0D0)
      t465 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t441, t
     #445, t443, -t447, 0.0D0)
      t466 = -t458 * t459 + t461 * t462 / 0.2D1 + t465
      t470 = t459 - t458 * t462
      t474 = t7 * t462
      t475 = t42 * t474
      t484 = log(-0.4D1 * t451 * t68 * t453 * x2 * t69)
      t493 = (-0.90D2 * t8 * (-t459 + t484 * t462) - 0.180D3 * t31 * t47
     #4) * t46 * t65
      t495 = -(0.90D2 * t8 * t466 - 0.180D3 * t31 * t7 * t470 - t475) * 
     #t46 * t48 / 0.720D3 - t493 / 0.720D3
      t496 = FJET(XB1, XB2, s, -t441, t443, t445, -t447, 0.0D0, t495)
      t498 = x2 * x1
      t500 = t442 * z
      t501 = 0.2D1 * t106
      t502 = t106 * x1
      t503 = t106 * t448
      t504 = cos(t10)
      t509 = Sqrt(x3 * t344 * t449 * x2 * t3)
      t511 = 0.2D1 * t504 * t509
      t512 = 0.1D1 - x1 + t448 - x2 + t498 - t498 * z - x3 + t442 - t500
     # + t501 - t502 + t503 + t511
      t515 = t2 * t439 * t512 * t450
      t519 = t2 * t439 * (-x3 + t442 - t500 + t501 - t502 + t503 - x2 + 
     #t511) * t450
      t520 = t1 ** 2
      t525 = s * t520 * x2 * x1 * t439 * t450
      t526 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t519, -t
     #515, t443, -t447, -t525)
      t527 = t106 * t13
      t528 = t15 * t450
      t529 = t453 * t344
      t534 = log(0.4D1 * t527 * t528 * t529 * t3)
      t535 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t519, -t
     #515, t443, -t447, -t525)
      t543 = -0.90D2 * t8 * (t526 - t534 * t535) + 0.180D3 * t31 * t7 * 
     #t535
      t546 = t543 * t46 * t65 / 0.720D3
      t547 = FJET(XB1, XB2, s, -t447, -t515, t443, t519, -t525, -t546)
      t550 = t46 * t64 * t48
      t553 = FJET(XB1, XB2, s, -t515, -t447, t519, t443, -t525, -t546)
      t559 = t2 * t439 * x2 * t450
      t560 = t2 * x1
      t562 = t344 * s * t440
      t563 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t559, t
     #562, 0.0D0, t560, -t525)
      t564 = t528 * t529
      t567 = log(-0.4D1 * t527 * t564)
      t568 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t559, t
     #562, 0.0D0, t560, -t525)
      t573 = t7 * t568
      t578 = (-0.90D2 * t8 * (-t563 + t567 * t568) - 0.180D3 * t31 * t57
     #3) * t46 * t65
      t581 = log(-0.4D1 * t52 * t564)
      t583 = t581 ** 2
      t586 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t559, t
     #562, 0.0D0, t560, -t525)
      t587 = t581 * t563 - t583 * t568 / 0.2D1 - t586
      t591 = -t563 + t581 * t568
      t595 = t42 * t573
      t600 = -t578 / 0.720D3 - (-0.90D2 * t8 * t587 + 0.180D3 * t31 * t7
     # * t591 - t595) * t64 * t48 / 0.720D3
      t601 = FJET(XB1, XB2, s, 0.0D0, -t559, t560, t562, -t525, t600)
      t614 = -(0.90D2 * t8 * t466 - 0.180D3 * t31 * t7 * t470 - t475) * 
     #t46 * t48 / 0.720D3 - t493 / 0.720D3
      t615 = FJET(XB1, XB2, s, t445, -t447, -t441, t443, 0.0D0, t614)
      t628 = -t578 / 0.720D3 - (-0.90D2 * t8 * t587 + 0.180D3 * t31 * t7
     # * t591 - t595) * t64 * t48 / 0.720D3
      t629 = FJET(XB1, XB2, s, -t559, 0.0D0, t562, t560, -t525, t628)
      t631 = FJET(XB1, XB2, s, -t346, 0.0D0, t343, 0.0D0, 0.0D0, t435)
      t635 = Sqrt(x2 * t344 * t69)
      t637 = 0.2D1 * t504 * t635
      t639 = t2 * (-x3 + t501 - x2 + t637)
      t641 = t2 * (0.1D1 - x2 - x3 + t501 + t637)
      t647 = log(0.4D1 * t106 * t12 * t15 * t3 * t344)
      t648 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t639, t
     #641, 0.0D0, 0.0D0, 0.0D0)
      t650 = t647 ** 2
      t651 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t639, t
     #641, 0.0D0, 0.0D0, 0.0D0)
      t654 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t639, t
     #641, 0.0D0, 0.0D0, 0.0D0)
      t663 = t7 * t651
      t673 = log(0.4D1 * t402 * t68 * t344 * t3)
      t684 = (-0.90D2 * t8 * (-t647 * t648 + t650 * t651 / 0.2D1 + t654)
     # + 0.180D3 * t31 * t7 * (t648 - t647 * t651) + t42 * t663) * t46 *
     # t64 / 0.1440D4 - (-0.90D2 * t8 * (-t648 + t673 * t651) - 0.180D3 
     #* t31 * t663) * t46 * t65 / 0.720D3
      t685 = FJET(XB1, XB2, s, 0.0D0, -t639, 0.0D0, t641, 0.0D0, t684)
      t687 = t2 * t439
      t689 = t528 * t453
      t692 = log(0.4D1 * t212 * t12 * t689)
      t693 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t687, 0.0D0, t560, 0.0D0)
      t695 = t692 ** 2
      t696 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t687, 0.0D0, t560, 0.0D0)
      t699 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t687, 0.0D0, t560, 0.0D0)
      t708 = t7 * t696
      t709 = t42 * t708
      t715 = log(0.4D1 * t13 * t689)
      t720 = t715 ** 2
      t723 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t687, 0.0D0, t560, 0.0D0)
      t745 = log(0.4D1 * t402 * t68 * t450 * t453)
      t757 = log(0.4D1 * t52 * t689)
      t759 = t757 ** 2
      t774 = -(-0.90D2 * t8 * (-t692 * t693 + t695 * t696 / 0.2D1 + t699
     #) + 0.180D3 * t31 * t7 * (t693 - t692 * t696) + t709) * t46 * t48 
     #/ 0.720D3 - (t42 * t7 * (t693 - t715 * t696) - 0.90D2 * t8 * (t720
     # * t693 / 0.2D1 + t723 - t720 * t715 * t696 / 0.6D1 - t715 * t699)
     # + t94 * t708 + 0.180D3 * t31 * t7 * (-t715 * t693 + t720 * t696 /
     # 0.2D1 + t699)) * t48 / 0.720D3 - (-0.90D2 * t8 * (t693 - t745 * t
     #696) + 0.180D3 * t31 * t708) * t46 * t65 / 0.720D3 - (-0.90D2 * t8
     # * (-t757 * t693 + t759 * t696 / 0.2D1 + t699) + 0.180D3 * t31 * t
     #7 * (t693 - t757 * t696) + t709) * t64 * t48 / 0.720D3
      t775 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t560, -t687, 0.0D0, t774)
      t777 = FJET(XB1, XB2, s, 0.0D0, t343, 0.0D0, -t346, 0.0D0, t435)
      t779 = FJET(XB1, XB2, s, t443, t519, -t447, -t515, -t525, -t546)
      t783 = FJET(XB1, XB2, s, t519, t443, -t515, -t447, -t525, -t546)
      t787 = FJET(XB1, XB2, s, t560, t562, 0.0D0, -t559, -t525, t600)
      t789 = t128 * t127 + t340 * t339 + t436 * t435 + t496 * t495 - t54
     #7 * t543 * t550 / 0.720D3 - t553 * t543 * t550 / 0.720D3 + t601 * 
     #t600 + t615 * t614 + t629 * t628 + t631 * t435 + t685 * t684 + t77
     #5 * t774 + t777 * t435 - t779 * t543 * t550 / 0.720D3 - t783 * t54
     #3 * t550 / 0.720D3 + t787 * t600
      t790 = FJET(XB1, XB2, s, -t639, 0.0D0, t641, 0.0D0, 0.0D0, t684)
      t792 = FJET(XB1, XB2, s, t560, -t687, 0.0D0, 0.0D0, 0.0D0, t774)
      t794 = FJET(XB1, XB2, s, -t687, t560, 0.0D0, 0.0D0, 0.0D0, t774)
      t796 = FJET(XB1, XB2, s, 0.0D0, t5, 0.0D0, -t4, 0.0D0, t127)
      t798 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t339)
      t800 = FJET(XB1, XB2, s, 0.0D0, -t346, 0.0D0, t343, 0.0D0, t435)
      t802 = FJET(XB1, XB2, s, t443, -t441, -t447, t445, 0.0D0, t614)
      t804 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t687, t560, 0.0D0, t774)
      t806 = FJET(XB1, XB2, s, t641, 0.0D0, -t639, 0.0D0, 0.0D0, t684)
      t808 = FJET(XB1, XB2, s, 0.0D0, -t4, 0.0D0, t5, 0.0D0, t127)
      t810 = FJET(XB1, XB2, s, t562, t560, -t559, 0.0D0, -t525, t600)
      t812 = FJET(XB1, XB2, s, 0.0D0, t641, 0.0D0, -t639, 0.0D0, t684)
      t814 = FJET(XB1, XB2, s, -t447, t445, t443, -t441, 0.0D0, t495)
      t816 = FJET(XB1, XB2, s, t5, 0.0D0, -t4, 0.0D0, 0.0D0, t127)
      t818 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t339)
      t820 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t339)
      t822 = t790 * t684 + t792 * t774 + t794 * t774 + t796 * t127 + t79
     #8 * t339 + t800 * t435 + t802 * t614 + t804 * t774 + t806 * t684 +
     # t808 * t127 + t810 * t600 + t812 * t684 + t814 * t495 + t816 * t1
     #27 + t818 * t339 + t820 * t339
      rrgg2qqbarhhardt8s1e1 = t789 + t822

      end function



      doubleprecision function rrgg2qqbarhhardt8s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = x4 * pi
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t14 = log(0.4D1 * x3 * t8 * t11)
      t15 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t17 = t14 ** 2
      t18 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t21 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t25 = pi * lh
      t31 = lh ** 2
      t33 = pi ** 2
      t35 = -0.180D3 * t31 + 0.30D2 * t33
      t36 = pi * t35
      t37 = t4 * t18
      t38 = t36 * t37
      t40 = 0.1D1 / x3
      t43 = t11 * t8
      t45 = log(0.4D1 * t43)
      t47 = t45 ** 2
      t64 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t79 = x2 * x3
      t82 = log(0.4D1 * t79 * t43)
      t88 = 0.180D3 * t25 * t37
      t91 = 0.1D1 / x2
      t94 = x2 * t8
      t97 = log(0.4D1 * t94 * t11)
      t99 = t97 ** 2
      t113 = x1 ** 2
      t114 = x3 * t113
      t117 = log(0.4D1 * t114 * t43)
      t124 = 0.1D1 / x1
      t129 = t40 * t91 * t124
      t132 = x2 * t113
      t135 = log(0.4D1 * t132 * t43)
      t144 = t113 * t8
      t147 = log(0.4D1 * t144 * t11)
      t149 = t147 ** 2
      t163 = -(-0.90D2 * t5 * (t14 * t15 - t17 * t18 / 0.2D1 - t21) + 0.
     #180D3 * t25 * t4 * (-t15 + t14 * t18) - t38) * t40 / 0.1440D4 + (0
     #.180D3 * (-t45 * t15 + t47 * t18 / 0.2D1 + t21) * pi * lh + t18 * 
     #pi * (-0.60D2 * lh * t33 + 0.240D3 * zeta3 + 0.120D3 * t31 * lh) -
     # 0.90D2 * (t47 * t15 / 0.2D1 + t64 - t47 * t45 * t18 / 0.6D1 - t45
     # * t21) * pi + (t15 - t45 * t18) * pi * t35) * t4 / 0.1440D4 + (-0
     #.90D2 * t5 * (t15 - t82 * t18) + t88) * t40 * t91 / 0.1440D4 - (-0
     #.90D2 * t5 * (t97 * t15 - t99 * t18 / 0.2D1 - t21) + 0.180D3 * t25
     # * t4 * (-t15 + t97 * t18) - t38) * t91 / 0.1440D4 - (-0.90D2 * t5
     # * (-t15 + t117 * t18) - t88) * t40 * t124 / 0.720D3 - t5 * t18 * 
     #t129 / 0.8D1 - (-0.90D2 * t5 * (-t15 + t135 * t18) - t88) * t91 * 
     #t124 / 0.720D3 - (-0.90D2 * t5 * (t147 * t15 - t149 * t18 / 0.2D1 
     #- t21) + 0.180D3 * t25 * t4 * (-t15 + t147 * t18) - t38) * t124 / 
     #0.720D3
      t164 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t163)
      t166 = -0.1D1 + x2
      t168 = t166 * t1 * s
      t170 = x2 * t1 * s
      t171 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t170, -t
     #168, 0.0D0, 0.0D0, 0.0D0)
      t175 = t43 * t166
      t178 = log(-0.4D1 * t132 * t175)
      t180 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t170, -t
     #168, 0.0D0, 0.0D0, 0.0D0)
      t184 = t4 * t171
      t186 = 0.180D3 * t25 * t184
      t193 = log(-0.4D1 * t79 * t175)
      t202 = t11 * t166
      t205 = log(-0.4D1 * t94 * t202)
      t207 = t205 ** 2
      t210 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t170, -t
     #168, 0.0D0, 0.0D0, 0.0D0)
      t223 = t5 * t171 * t129 / 0.8D1 - (-0.90D2 * t5 * (-t178 * t171 + 
     #t180) + t186) * t91 * t124 / 0.720D3 + (-0.90D2 * t5 * (t193 * t17
     #1 - t180) - t186) * t40 * t91 / 0.1440D4 - (-0.90D2 * t5 * (-t205 
     #* t180 + t207 * t171 / 0.2D1 + t210) + 0.180D3 * t25 * t4 * (t180 
     #- t205 * t171) + t36 * t184) * t91 / 0.1440D4
      t224 = FJET(XB1, XB2, s, -t168, 0.0D0, t170, 0.0D0, 0.0D0, t223)
      t227 = -0.1D1 + x1
      t228 = t1 * t227
      t229 = x3 * s * t228
      t230 = x3 * x1
      t231 = t2 * t230
      t232 = -0.1D1 + x3
      t233 = t232 * s
      t234 = t233 * t228
      t236 = t233 * t1 * x1
      t237 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t229, t
     #234, t231, -t236, 0.0D0)
      t238 = x1 * z
      t239 = 0.1D1 - x1 + t238
      t240 = 0.1D1 / t239
      t243 = t227 ** 2
      t245 = x3 * t232
      t249 = log(-0.4D1 * t113 * t240 * t8 * t11 * t243 * t245)
      t250 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t229, t
     #234, t231, -t236, 0.0D0)
      t252 = t237 - t249 * t250
      t257 = 0.180D3 * t25 * t4 * t250
      t264 = t5 * t250 * t129 / 0.8D1
      t265 = -(0.90D2 * t5 * t252 - t257) * t40 * t124 / 0.720D3 - t264
      t266 = FJET(XB1, XB2, s, -t229, t231, t234, -t236, 0.0D0, t265)
      t275 = -(0.90D2 * t5 * t252 - t257) * t40 * t124 / 0.720D3 - t264
      t276 = FJET(XB1, XB2, s, t231, -t229, -t236, t234, 0.0D0, t275)
      t278 = FJET(XB1, XB2, s, 0.0D0, t170, 0.0D0, -t168, 0.0D0, t223)
      t280 = 0.2D1 * t79
      t281 = cos(t6)
      t284 = Sqrt(x2 * t166 * t245)
      t286 = 0.2D1 * t281 * t284
      t288 = t2 * (0.1D1 - x2 - x3 + t280 + t286)
      t290 = t2 * (-x3 + t280 - x2 + t286)
      t291 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t290, t
     #288, 0.0D0, 0.0D0, 0.0D0)
      t295 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t290, t
     #288, 0.0D0, 0.0D0, 0.0D0)
      t301 = log(0.4D1 * t79 * t8 * t11 * t232 * t166)
      t313 = -t5 * t291 * t129 / 0.8D1 + (-0.90D2 * t5 * (t295 - t301 * 
     #t291) + 0.180D3 * t25 * t4 * t291) * t40 * t91 / 0.1440D4
      t314 = FJET(XB1, XB2, s, 0.0D0, t288, 0.0D0, -t290, 0.0D0, t313)
      t317 = t166 * s * t228
      t318 = t2 * x1
      t321 = t2 * t227 * x2 * t240
      t322 = t1 ** 2
      t327 = s * t322 * x2 * x1 * t227 * t240
      t328 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t321, t
     #317, 0.0D0, t318, -t327)
      t331 = t5 * t328 * t129 / 0.8D1
      t332 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t321, t
     #317, 0.0D0, t318, -t327)
      t333 = t132 * t8
      t338 = log(-0.4D1 * t333 * t202 * t243 * t240)
      t340 = -t332 + t338 * t328
      t345 = 0.180D3 * t25 * t4 * t328
      t350 = -t331 - (-0.90D2 * t5 * t340 - t345) * t91 * t124 / 0.720D3
      t351 = FJET(XB1, XB2, s, t317, t318, -t321, 0.0D0, -t327, t350)
      t353 = FJET(XB1, XB2, s, t234, -t236, -t229, t231, 0.0D0, t275)
      t355 = t2 * x3
      t356 = t2 * t232
      t357 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t355, -t
     #356, 0.0D0, 0.0D0, 0.0D0)
      t361 = log(-0.4D1 * t79 * t43 * t232)
      t362 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t355, -t
     #356, 0.0D0, 0.0D0, 0.0D0)
      t367 = t4 * t362
      t369 = 0.180D3 * t25 * t367
      t376 = log(-0.4D1 * t43 * t245)
      t378 = t376 ** 2
      t381 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t355, -t
     #356, 0.0D0, 0.0D0, 0.0D0)
      t398 = log(-0.4D1 * t144 * t11 * x3 * t232)
      t410 = (-0.90D2 * t5 * (-t357 + t361 * t362) - t369) * t40 * t91 /
     # 0.1440D4 - (-0.90D2 * t5 * (-t376 * t357 + t378 * t362 / 0.2D1 + 
     #t381) + 0.180D3 * t25 * t4 * (t357 - t376 * t362) + t36 * t367) * 
     #t40 / 0.1440D4 - (-0.90D2 * t5 * (t357 - t398 * t362) + t369) * t4
     #0 * t124 / 0.720D3 + t5 * t362 * t129 / 0.8D1
      t411 = FJET(XB1, XB2, s, 0.0D0, t355, 0.0D0, -t356, 0.0D0, t410)
      t413 = t2 * t227
      t414 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t413, 0.0D0, t318, 0.0D0)
      t417 = t11 * t240 * t243
      t420 = log(0.4D1 * t114 * t8 * t417)
      t421 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t413, 0.0D0, t318, 0.0D0)
      t426 = t4 * t421
      t428 = 0.180D3 * t25 * t426
      t438 = log(0.4D1 * t333 * t417)
      t449 = log(0.4D1 * t144 * t417)
      t451 = t449 ** 2
      t454 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t413, 0.0D0, t318, 0.0D0)
      t467 = -(-0.90D2 * t5 * (t414 - t420 * t421) + t428) * t40 * t124 
     #/ 0.720D3 + t5 * t421 * t129 / 0.8D1 - (-0.90D2 * t5 * (t414 - t43
     #8 * t421) + t428) * t91 * t124 / 0.720D3 - (-0.90D2 * t5 * (-t449 
     #* t414 + t451 * t421 / 0.2D1 + t454) + 0.180D3 * t25 * t4 * (t414 
     #- t449 * t421) + t36 * t426) * t124 / 0.720D3
      t468 = FJET(XB1, XB2, s, t318, -t413, 0.0D0, 0.0D0, 0.0D0, t467)
      t470 = x2 * x1
      t472 = t230 * z
      t473 = t79 * x1
      t474 = t79 * t238
      t479 = Sqrt(x3 * t166 * t239 * x2 * t232)
      t481 = 0.2D1 * t281 * t479
      t482 = 0.1D1 - x1 + t238 - x2 + t470 - t470 * z - x3 + t230 - t472
     # + t280 - t473 + t474 + t481
      t485 = t2 * t227 * t482 * t240
      t489 = t2 * t227 * (-x3 + t230 - t472 + t280 - t473 + t474 - x2 + 
     #t481) * t240
      t490 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t489, -t
     #485, t231, -t236, -t327)
      t493 = t5 * t490 * t129 / 0.8D1
      t494 = FJET(XB1, XB2, s, -t485, -t236, t489, t231, -t327, t493)
      t499 = t490 * t40 * t91 * t124
      t502 = FJET(XB1, XB2, s, t489, t231, -t485, -t236, -t327, t493)
      t507 = FJET(XB1, XB2, s, -t290, 0.0D0, t288, 0.0D0, 0.0D0, t313)
      t509 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t163)
      t511 = FJET(XB1, XB2, s, -t356, 0.0D0, t355, 0.0D0, 0.0D0, t410)
      t513 = FJET(XB1, XB2, s, t231, t489, -t236, -t485, -t327, t493)
      t518 = t164 * t163 + t224 * t223 + t266 * t265 + t276 * t275 + t27
     #8 * t223 + t314 * t313 + t351 * t350 + t353 * t275 + t411 * t410 +
     # t468 * t467 + t494 * pi * t4 * t499 / 0.8D1 + t502 * pi * t4 * t4
     #99 / 0.8D1 + t507 * t313 + t509 * t163 + t511 * t410 + t513 * pi *
     # t4 * t499 / 0.8D1
      t519 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t163)
      t521 = FJET(XB1, XB2, s, -t236, t234, t231, -t229, 0.0D0, t265)
      t523 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t318, -t413, 0.0D0, t467)
      t532 = -t331 - (-0.90D2 * t5 * t340 - t345) * t91 * t124 / 0.720D3
      t533 = FJET(XB1, XB2, s, -t321, 0.0D0, t317, t318, -t327, t532)
      t535 = FJET(XB1, XB2, s, 0.0D0, -t290, 0.0D0, t288, 0.0D0, t313)
      t537 = FJET(XB1, XB2, s, -t413, t318, 0.0D0, 0.0D0, 0.0D0, t467)
      t539 = FJET(XB1, XB2, s, 0.0D0, -t356, 0.0D0, t355, 0.0D0, t410)
      t541 = FJET(XB1, XB2, s, t288, 0.0D0, -t290, 0.0D0, 0.0D0, t313)
      t543 = FJET(XB1, XB2, s, t170, 0.0D0, -t168, 0.0D0, 0.0D0, t223)
      t545 = FJET(XB1, XB2, s, t355, 0.0D0, -t356, 0.0D0, 0.0D0, t410)
      t547 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t413, t318, 0.0D0, t467)
      t549 = FJET(XB1, XB2, s, 0.0D0, -t168, 0.0D0, t170, 0.0D0, t223)
      t551 = FJET(XB1, XB2, s, t318, t317, 0.0D0, -t321, -t327, t350)
      t553 = FJET(XB1, XB2, s, 0.0D0, -t321, t318, t317, -t327, t350)
      t555 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t163)
      t557 = FJET(XB1, XB2, s, -t236, -t485, t231, t489, -t327, t493)
      t562 = t519 * t163 + t521 * t265 + t523 * t467 + t533 * t532 + t53
     #5 * t313 + t537 * t467 + t539 * t410 + t541 * t313 + t543 * t223 +
     # t545 * t410 + t547 * t467 + t549 * t223 + t551 * t350 + t553 * t3
     #50 + t555 * t163 + t557 * pi * t4 * t499 / 0.8D1
      rrgg2qqbarhhardt8s1e0 = t518 + t562

      end function



      doubleprecision function rrgg2qqbarhhardt8s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x2
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = t4 * s * t7
      t12 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t14 = t2 * t6 * x2 * t12
      t15 = t1 ** 2
      t20 = s * t15 * x2 * x1 * t6 * t12
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = pi * t22
      t24 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t14, t8,
     # 0.0D0, t3, -t20)
      t25 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t28 = t24 * t25 * t27
      t30 = t23 * t28 / 0.8D1
      t31 = FJET(XB1, XB2, s, t3, t8, 0.0D0, -t14, -t20, -t30)
      t36 = -0.1D1 + x3
      t37 = t36 * s
      t38 = t37 * t7
      t40 = t37 * t1 * x1
      t42 = x3 * s * t7
      t44 = t2 * x1 * x3
      t45 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t42, t38
     #, t44, -t40, 0.0D0)
      t46 = 0.1D1 / x3
      t48 = t45 * t46 * t27
      t50 = t23 * t48 / 0.8D1
      t51 = FJET(XB1, XB2, s, t38, -t40, -t42, t44, 0.0D0, -t50)
      t57 = 0.2D1 * x2 * x3
      t58 = x4 * pi
      t59 = cos(t58)
      t61 = x3 * t36
      t63 = Sqrt(x2 * t4 * t61)
      t65 = 0.2D1 * t59 * t63
      t67 = t2 * (0.1D1 - x2 - x3 + t57 + t65)
      t69 = t2 * (-x3 + t57 - x2 + t65)
      t70 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t69, t67
     #, 0.0D0, 0.0D0, 0.0D0)
      t72 = t70 * t46 * t25
      t74 = t23 * t72 / 0.16D2
      t75 = FJET(XB1, XB2, s, 0.0D0, t67, 0.0D0, -t69, 0.0D0, -t74)
      t80 = FJET(XB1, XB2, s, -t69, 0.0D0, t67, 0.0D0, 0.0D0, -t74)
      t85 = FJET(XB1, XB2, s, -t42, t44, t38, -t40, 0.0D0, -t50)
      t90 = FJET(XB1, XB2, s, -t14, 0.0D0, t8, t3, -t20, -t30)
      t95 = FJET(XB1, XB2, s, t67, 0.0D0, -t69, 0.0D0, 0.0D0, -t74)
      t100 = FJET(XB1, XB2, s, 0.0D0, -t14, t3, t8, -t20, -t30)
      t105 = FJET(XB1, XB2, s, t44, -t42, -t40, t38, 0.0D0, -t50)
      t110 = FJET(XB1, XB2, s, t8, t3, -t14, 0.0D0, -t20, -t30)
      t115 = FJET(XB1, XB2, s, 0.0D0, -t69, 0.0D0, t67, 0.0D0, -t74)
      t120 = FJET(XB1, XB2, s, -t40, t38, t44, -t42, 0.0D0, -t50)
      t125 = t2 * x3
      t126 = t2 * t36
      t127 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t
     #126, 0.0D0, 0.0D0, 0.0D0)
      t128 = t127 * t46
      t132 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t125, -t
     #126, 0.0D0, 0.0D0, 0.0D0)
      t133 = z ** 2
      t134 = 0.1D1 / t133
      t135 = Sin(t58)
      t136 = t135 ** 2
      t137 = t134 * t136
      t140 = log(-0.4D1 * t137 * t61)
      t145 = pi * lh
      t155 = t23 * t128 * t25 / 0.16D2 - (-0.90D2 * t23 * (t132 - t140 *
     # t127) + 0.180D3 * t145 * t22 * t127) * t46 / 0.1440D4 + t23 * t12
     #8 * t27 / 0.8D1
      t156 = FJET(XB1, XB2, s, t125, 0.0D0, -t126, 0.0D0, 0.0D0, t155)
      t159 = x2 * t1 * s
      t161 = t4 * t1 * s
      t162 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t159, -t
     #161, 0.0D0, 0.0D0, 0.0D0)
      t171 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t159, -t
     #161, 0.0D0, 0.0D0, 0.0D0)
      t172 = x2 * t136
      t176 = log(-0.4D1 * t172 * t134 * t4)
      t187 = t23 * t162 * t25 * t27 / 0.8D1 + t23 * t162 * t46 * t25 / 0
     #.16D2 - (-0.90D2 * t23 * (t171 - t176 * t162) + 0.180D3 * t145 * t
     #22 * t162) * t25 / 0.1440D4
      t188 = FJET(XB1, XB2, s, 0.0D0, t159, 0.0D0, -t161, 0.0D0, t187)
      t190 = -t31 * pi * t22 * t28 / 0.8D1 - t51 * pi * t22 * t48 / 0.8D
     #1 - t75 * pi * t22 * t72 / 0.16D2 - t80 * pi * t22 * t72 / 0.16D2 
     #- t85 * pi * t22 * t48 / 0.8D1 - t90 * pi * t22 * t28 / 0.8D1 - t9
     #5 * pi * t22 * t72 / 0.16D2 - t100 * pi * t22 * t28 / 0.8D1 - t105
     # * pi * t22 * t48 / 0.8D1 - t110 * pi * t22 * t28 / 0.8D1 - t115 *
     # pi * t22 * t72 / 0.16D2 - t120 * pi * t22 * t48 / 0.8D1 + t156 * 
     #t155 + t188 * t187
      t191 = t2 * t6
      t192 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t191, 0.0D0, t3, 0.0D0)
      t197 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t191, 0.0D0, t3, 0.0D0)
      t198 = x1 ** 2
      t199 = t198 * t136
      t201 = t6 ** 2
      t205 = log(0.4D1 * t199 * t134 * t12 * t201)
      t220 = t23 * t192 * t25 * t27 / 0.8D1 - (-0.90D2 * t23 * (t197 - t
     #205 * t192) + 0.180D3 * t145 * t22 * t192) * t27 / 0.720D3 + t23 *
     # t192 * t46 * t27 / 0.8D1
      t221 = FJET(XB1, XB2, s, -t191, t3, 0.0D0, 0.0D0, 0.0D0, t220)
      t223 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t191, t3, 0.0D0, t220)
      t225 = FJET(XB1, XB2, s, 0.0D0, -t126, 0.0D0, t125, 0.0D0, t155)
      t227 = FJET(XB1, XB2, s, -t126, 0.0D0, t125, 0.0D0, 0.0D0, t155)
      t229 = FJET(XB1, XB2, s, t3, -t191, 0.0D0, 0.0D0, 0.0D0, t220)
      t231 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t235 = log(0.4D1 * x3 * t136 * t134)
      t236 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t243 = 0.180D3 * t145 * t22 * t236
      t248 = log(0.4D1 * t137)
      t255 = t248 ** 2
      t258 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t263 = lh ** 2
      t265 = pi ** 2
      t272 = t236 * t46
      t278 = log(0.4D1 * t172 * t134)
      t292 = log(0.4D1 * t199 * t134)
      t303 = -(-0.90D2 * t23 * (-t231 + t235 * t236) - t243) * t46 / 0.1
     #440D4 + (0.180D3 * (t231 - t248 * t236) * pi * lh - 0.90D2 * (-t24
     #8 * t231 + t255 * t236 / 0.2D1 + t258) * pi + t236 * pi * (-0.180D
     #3 * t263 + 0.30D2 * t265)) * t22 / 0.1440D4 - t23 * t272 * t25 / 0
     #.16D2 - (-0.90D2 * t23 * (-t231 + t278 * t236) - t243) * t25 / 0.1
     #440D4 - t23 * t236 * t25 * t27 / 0.8D1 - (-0.90D2 * t23 * (-t231 +
     # t292 * t236) - t243) * t27 / 0.720D3 - t23 * t272 * t27 / 0.8D1
      t304 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t303)
      t306 = FJET(XB1, XB2, s, t159, 0.0D0, -t161, 0.0D0, 0.0D0, t187)
      t308 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t303)
      t310 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t303)
      t312 = FJET(XB1, XB2, s, 0.0D0, t125, 0.0D0, -t126, 0.0D0, t155)
      t314 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t191, 0.0D0, t220)
      t316 = FJET(XB1, XB2, s, -t161, 0.0D0, t159, 0.0D0, 0.0D0, t187)
      t318 = FJET(XB1, XB2, s, 0.0D0, -t161, 0.0D0, t159, 0.0D0, t187)
      t320 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t303)
      t322 = t221 * t220 + t223 * t220 + t225 * t155 + t227 * t155 + t22
     #9 * t220 + t304 * t303 + t306 * t187 + t308 * t303 + t310 * t303 +
     # t312 * t155 + t314 * t220 + t316 * t187 + t318 * t187 + t320 * t3
     #03
      rrgg2qqbarhhardt8s1em1 = t190 + t322

      end function



      doubleprecision function rrgg2qqbarhhardt8s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t22 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t23 = z ** 2
      t26 = Sin(x4 * pi)
      t27 = t26 ** 2
      t30 = log(0.4D1 / t23 * t27)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 + (0.180D3 * t6 * pi * lh - 0.90D2 * (t22 - t30 * t6) 
     #* pi) * t4 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t45 = t2 * (-0.1D1 + x1)
      t46 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #45, 0.0D0, t43, 0.0D0)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t49)
      t53 = t4 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t62 = t2 * x3
      t64 = t2 * (-0.1D1 + x3)
      t65 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t62, -t64
     #, 0.0D0, 0.0D0, 0.0D0)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t68)
      t72 = t4 * t65 * t7
      t76 = x2 * t1 * s
      t79 = (-0.1D1 + x2) * t1 * s
      t80 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t76, -t79
     #, 0.0D0, 0.0D0, 0.0D0)
      t83 = t5 * t80 * t11 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t76, 0.0D0, -t79, 0.0D0, t83)
      t87 = t4 * t80 * t11
      t90 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t68)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t76, 0.0D0, t83)
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t100 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t68)
      t108 = FJET(XB1, XB2, s, t76, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t112 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t68)
      t116 = FJET(XB1, XB2, s, -t79, 0.0D0, t76, 0.0D0, 0.0D0, t83)
      t120 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2qqbarhhardt8s1em2 = t39 * t38 + t41 * t38 + t50 * pi * t53 / 
     #0.8D1 + t56 * pi * t53 / 0.8D1 + t60 * t38 + t69 * pi * t72 / 0.16
     #D2 + t84 * pi * t87 / 0.16D2 + t90 * pi * t72 / 0.16D2 + t94 * pi 
     #* t87 / 0.16D2 + t98 * t38 + t100 * pi * t53 / 0.8D1 + t104 * pi *
     # t72 / 0.16D2 + t108 * pi * t87 / 0.16D2 + t112 * pi * t72 / 0.16D
     #2 + t116 * pi * t87 / 0.16D2 + t120 * pi * t53 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt8s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarhhardt8s1em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 
     #/ 0.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt8s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt8s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t24 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t28 = pi * lh
      t34 = lh ** 2
      t36 = pi ** 2
      t38 = 0.180D3 * t34 - 0.30D2 * t36
      t39 = pi * t38
      t40 = t4 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t6 * t13
      t49 = t48 * t10
      t51 = log(0.4D1 * t49)
      t56 = t51 ** 2
      t59 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t72 = 0.60D2 * lh * t36 - 0.240D3 * zeta3 - 0.120D3 * t34 * lh
      t73 = pi * t72
      t74 = t73 * t40
      t85 = x2 * x3
      t86 = t85 * t6
      t87 = -0.1D1 + x2
      t88 = t14 * t87
      t91 = log(-0.4D1 * t86 * t88)
      t95 = log(0.4D1 * t85 * t49)
      t99 = 0.1D1 / x2
      t101 = t43 * t99 * t45
      t104 = x2 * t6
      t107 = log(0.4D1 * t104 * t14)
      t109 = t107 ** 2
      t114 = log(-0.4D1 * t104 * t88)
      t116 = t114 ** 2
      t132 = x3 * t10
      t135 = log(0.4D1 * t132 * t13)
      t140 = t135 ** 2
      t162 = log(-0.4D1 * t85 * t88)
      t164 = t162 ** 2
      t169 = log(0.4D1 * t85 * t14)
      t171 = t169 ** 2
      t192 = x2 * t10
      t193 = t192 * t13
      t195 = log(0.4D1 * t193)
      t196 = t195 ** 2
      t200 = log(-0.4D1 * t192 * t13 * t87)
      t201 = t200 ** 2
      t224 = log(0.4D1 * t14)
      t225 = t224 ** 2
      t226 = t225 * pi
      t230 = t225 * t224 * pi
      t232 = t224 * pi
      t251 = rrgg2qqbarhhard81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, 0.0D0, t2, 0.0D0)
      t259 = t36 ** 2
      t260 = t34 ** 2
      t268 = t225 ** 2
      t275 = (0.90D2 * t5 * (t17 * t18 - t20 * t21 / 0.2D1 - t24) - 0.18
     #0D3 * t28 * t4 * (-t18 + t17 * t21) - t41) * t43 * t45 / 0.720D3 +
     # (t39 * t4 * (-t18 + t51 * t21) + 0.90D2 * t5 * (-t56 * t18 / 0.2D
     #1 - t59 + t56 * t51 * t21 / 0.6D1 + t51 * t24) - t74 - 0.180D3 * t
     #28 * t4 * (t51 * t18 - t56 * t21 / 0.2D1 - t24)) * t45 / 0.720D3 +
     # t5 * (-t91 * t21 + t95 * t21) * t101 / 0.8D1 + (0.90D2 * t5 * (t1
     #07 * t18 - t109 * t21 / 0.2D1 - t114 * t18 + t116 * t21 / 0.2D1) -
     # 0.180D3 * t28 * t4 * (t107 * t21 - t114 * t21)) * t99 * t45 / 0.7
     #20D3 + (t39 * t4 * (-t18 + t135 * t21) + 0.90D2 * t5 * (-t140 * t1
     #8 / 0.2D1 - t59 + t140 * t135 * t21 / 0.6D1 + t135 * t24) - t74 - 
     #0.180D3 * t28 * t4 * (t135 * t18 - t140 * t21 / 0.2D1 - t24)) * t4
     #3 / 0.1440D4 + (0.90D2 * t5 * (-t162 * t18 + t164 * t21 / 0.2D1 + 
     #t169 * t18 - t171 * t21 / 0.2D1) - 0.180D3 * t28 * t4 * (-t162 * t
     #21 + t169 * t21)) * t43 * t99 / 0.1440D4 + ((0.90D2 * t5 * t18 - 0
     #.180D3 * t28 * t40) * (-t196 / 0.2D1 + t201 / 0.2D1) + 0.90D2 * t5
     # * t21 * (-t201 * t200 / 0.6D1 + t196 * t195 / 0.6D1) + (-0.180D3 
     #* t28 * t4 * t18 + t41 + 0.90D2 * t5 * t24) * (t195 - t200)) * t99
     # / 0.1440D4 - (-0.90D2 * t226 * lh + t73 - 0.15D2 * t230 - t232 * 
     #t38) * t4 * t18 / 0.1440D4 - (0.180D3 * t232 * lh + 0.45D2 * t226 
     #+ t39) * t4 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t232) * 
     #t4 * t59 / 0.1440D4 - t5 * t251 / 0.16D2 - (0.30D2 * t230 * lh + t
     #226 * t38 / 0.2D1 - t232 * t72 + pi * (t259 + 0.60D2 * t260 + 0.48
     #0D3 * lh * zeta3 - 0.60D2 * t34 * t36) + 0.15D2 / 0.4D1 * t268 * p
     #i) * t4 * t21 / 0.1440D4
      t276 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t275)
      t278 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t275)
      t280 = t2 * x1
      t281 = -0.1D1 + x1
      t282 = t2 * t281
      t284 = 0.1D1 / t8
      t285 = x1 * z
      t286 = -z - x1 + t285
      t287 = 0.1D1 / t286
      t288 = t284 * t287
      t289 = t281 ** 2
      t290 = t288 * t289
      t293 = log(-0.4D1 * t7 * t13 * t290)
      t294 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #280, 0.0D0, -t282, 0.0D0)
      t296 = t293 ** 2
      t297 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #280, 0.0D0, -t282, 0.0D0)
      t300 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #280, 0.0D0, -t282, 0.0D0)
      t309 = t4 * t297
      t310 = t39 * t309
      t316 = log(-0.4D1 * t48 * t290)
      t321 = t316 ** 2
      t324 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #280, 0.0D0, -t282, 0.0D0)
      t342 = t13 * t284
      t343 = t287 * t289
      t347 = log(-0.4D1 * t86 * t342 * t343)
      t356 = t99 * t45
      t358 = t104 * t13
      t361 = log(-0.4D1 * t358 * t290)
      t363 = t361 ** 2
      t378 = (0.90D2 * t5 * (-t293 * t294 + t296 * t297 / 0.2D1 + t300) 
     #- 0.180D3 * t28 * t4 * (t294 - t293 * t297) + t310) * t43 * t45 / 
     #0.720D3 + (t39 * t4 * (t294 - t316 * t297) + 0.90D2 * t5 * (t321 *
     # t294 / 0.2D1 + t324 - t321 * t316 * t297 / 0.6D1 - t316 * t300) +
     # t73 * t309 - 0.180D3 * t28 * t4 * (-t316 * t294 + t321 * t297 / 0
     #.2D1 + t300)) * t45 / 0.720D3 + (0.90D2 * t5 * (t294 - t347 * t297
     #) - 0.180D3 * t28 * t309) * t43 * t356 / 0.720D3 + (0.90D2 * t5 * 
     #(-t361 * t294 + t363 * t297 / 0.2D1 + t300) - 0.180D3 * t28 * t4 *
     # (t294 - t361 * t297) + t310) * t99 * t45 / 0.720D3
      t379 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t280, -t282, 0.0D0, t378)
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t282, t280, 0.0D0, t378)
      t383 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t275)
      t385 = t2 * x3
      t386 = -0.1D1 + x3
      t387 = t2 * t386
      t391 = log(-0.4D1 * t7 * t14 * t386)
      t392 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t385, -t387, 0.0D0)
      t394 = t391 ** 2
      t395 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t385, -t387, 0.0D0)
      t398 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t385, -t387, 0.0D0)
      t407 = t4 * t395
      t417 = log(0.4D1 * t86 * t14 * t87 * t386)
      t422 = log(-0.4D1 * t358 * t132 * t386)
      t431 = log(-0.4D1 * t132 * t13 * t386)
      t436 = t431 ** 2
      t439 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t385, -t387, 0.0D0)
      t462 = log(-0.4D1 * t192 * t13 * x3 * t386)
      t464 = t462 ** 2
      t467 = x3 * t386
      t471 = log(0.4D1 * t193 * t467 * t87)
      t473 = t471 ** 2
      t489 = (0.90D2 * t5 * (-t391 * t392 + t394 * t395 / 0.2D1 + t398) 
     #- 0.180D3 * t28 * t4 * (t392 - t391 * t395) + t39 * t407) * t43 * 
     #t45 / 0.720D3 + t5 * (t417 * t395 - t422 * t395) * t101 / 0.8D1 + 
     #(t39 * t4 * (t392 - t431 * t395) + 0.90D2 * t5 * (t436 * t392 / 0.
     #2D1 + t439 - t436 * t431 * t395 / 0.6D1 - t431 * t398) + t73 * t40
     #7 - 0.180D3 * t28 * t4 * (-t431 * t392 + t436 * t395 / 0.2D1 + t39
     #8)) * t43 / 0.1440D4 + (0.90D2 * t5 * (-t462 * t392 + t464 * t395 
     #/ 0.2D1 + t471 * t392 - t473 * t395 / 0.2D1) - 0.180D3 * t28 * t4 
     #* (-t462 * t395 + t471 * t395)) * t43 * t99 / 0.1440D4
      t490 = FJET(XB1, XB2, s, 0.0D0, t385, 0.0D0, -t387, 0.0D0, t489)
      t492 = FJET(XB1, XB2, s, 0.0D0, -t387, 0.0D0, t385, 0.0D0, t489)
      t494 = x2 * x1
      t496 = t2 * t494 * t287
      t498 = t1 * x1
      t499 = t87 * s * t498
      t500 = t1 ** 2
      t505 = s * t500 * x2 * x1 * t281 * t287
      t506 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t496, -
     #t499, 0.0D0, -t282, t505)
      t507 = t85 * t48
      t508 = t289 * t87
      t509 = t288 * t508
      t512 = log(0.4D1 * t507 * t509)
      t513 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t496, -
     #t499, 0.0D0, -t282, t505)
      t518 = t4 * t513
      t526 = log(0.4D1 * t358 * t509)
      t528 = t526 ** 2
      t531 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t496, -
     #t499, 0.0D0, -t282, t505)
      t545 = (0.90D2 * t5 * (-t506 + t512 * t513) + 0.180D3 * t28 * t518
     #) * t43 * t356 / 0.720D3 + (0.90D2 * t5 * (t526 * t506 - t528 * t5
     #13 / 0.2D1 - t531) - 0.180D3 * t28 * t4 * (-t506 + t526 * t513) - 
     #t39 * t518) * t99 * t45 / 0.720D3
      t546 = FJET(XB1, XB2, s, 0.0D0, -t496, -t282, -t499, t505, t545)
      t548 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t275)
      t550 = FJET(XB1, XB2, s, t280, -t282, 0.0D0, 0.0D0, 0.0D0, t378)
      t552 = FJET(XB1, XB2, s, t385, 0.0D0, -t387, 0.0D0, 0.0D0, t489)
      t554 = x3 * x1
      t555 = t2 * t554
      t557 = t1 * t281
      t558 = x3 * s * t557
      t559 = t386 * s
      t560 = t559 * t498
      t561 = t559 * t557
      t563 = t467 * t343
      t566 = log(0.4D1 * t48 * t284 * t563)
      t567 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t555, -t
     #560, -t558, t561, 0.0D0)
      t569 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t555, -t
     #560, -t558, t561, 0.0D0)
      t570 = t566 ** 2
      t571 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t555, -t
     #560, -t558, t561, 0.0D0)
      t582 = t4 * t571
      t590 = log(0.4D1 * t104 * t342 * t563)
      t601 = (0.90D2 * t5 * (t566 * t567 - t569 - t570 * t571 / 0.2D1) -
     # 0.180D3 * t28 * t4 * (-t567 + t566 * t571) - t39 * t582) * t43 * 
     #t45 / 0.720D3 + (0.90D2 * t5 * (-t567 + t590 * t571) + 0.180D3 * t
     #28 * t582) * t43 * t356 / 0.720D3
      t602 = FJET(XB1, XB2, s, t555, -t558, -t560, t561, 0.0D0, t601)
      t604 = t276 * t275 + t278 * t275 + t379 * t378 + t381 * t378 + t38
     #3 * t275 + t490 * t489 + t492 * t489 + t546 * t545 + t548 * t275 +
     # t550 * t378 + t552 * t489 + t602 * t601
      t605 = FJET(XB1, XB2, s, t561, -t560, -t558, t555, 0.0D0, t601)
      t609 = x3 * z
      t610 = t554 * z
      t611 = t85 * z
      t612 = t85 * x1
      t613 = t85 * t285
      t614 = cos(t11)
      t619 = Sqrt(-x3 * t87 * t286 * x2 * t386)
      t621 = 0.2D1 * t614 * t619
      t622 = z + x1 - t285 - x2 * z - t494 + t494 * z - t609 - t554 + t6
     #10 + t611 + t612 - t613 + t85 + t621
      t625 = t2 * x1 * t622 * t287
      t629 = t2 * x1 * (-t609 - t554 + t610 + t611 + t612 - t613 - x2 + 
     #t85 + t621) * t287
      t630 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t629, -t
     #625, -t558, t561, t505)
      t635 = log(-0.4D1 * t507 * t288 * t508 * t386)
      t636 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t629, -t
     #625, -t558, t561, t505)
      t644 = 0.90D2 * t5 * (t630 - t635 * t636) - 0.180D3 * t28 * t4 * t
     #636
      t647 = t644 * t43 * t356 / 0.720D3
      t648 = FJET(XB1, XB2, s, t561, -t625, -t558, t629, t505, t647)
      t652 = FJET(XB1, XB2, s, t629, -t558, -t625, t561, t505, t647)
      t656 = FJET(XB1, XB2, s, -t282, t280, 0.0D0, 0.0D0, 0.0D0, t378)
      t658 = FJET(XB1, XB2, s, -t282, -t499, 0.0D0, -t496, t505, t545)
      t660 = FJET(XB1, XB2, s, -t387, 0.0D0, t385, 0.0D0, 0.0D0, t489)
      t662 = FJET(XB1, XB2, s, -t558, t555, t561, -t560, 0.0D0, t601)
      t664 = FJET(XB1, XB2, s, -t558, t629, t561, -t625, t505, t647)
      t668 = FJET(XB1, XB2, s, -t499, -t282, -t496, 0.0D0, t505, t545)
      t670 = FJET(XB1, XB2, s, -t560, t561, t555, -t558, 0.0D0, t601)
      t672 = FJET(XB1, XB2, s, -t496, 0.0D0, -t499, -t282, t505, t545)
      t674 = FJET(XB1, XB2, s, -t625, t561, t629, -t558, t505, t647)
      t678 = t605 * t601 + t648 * t644 * t101 / 0.720D3 + t652 * t644 * 
     #t101 / 0.720D3 + t656 * t378 + t658 * t545 + t660 * t489 + t662 * 
     #t601 + t664 * t644 * t101 / 0.720D3 + t668 * t545 + t670 * t601 + 
     #t672 * t545 + t674 * t644 * t101 / 0.720D3
      rrgg2qqbarhhardt8s2e1 = t604 + t678

      end function



      doubleprecision function rrgg2qqbarhhardt8s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t24 = pi * lh
      t25 = t4 * t19
      t27 = 0.180D3 * t24 * t25
      t29 = 0.1D1 / x3
      t31 = 0.1D1 / x1
      t34 = x2 * t7
      t37 = log(0.4D1 * t34 * t15)
      t39 = -0.1D1 + x2
      t40 = t15 * t39
      t43 = log(-0.4D1 * t34 * t40)
      t46 = 0.1D1 / x2
      t51 = t7 * t14
      t54 = log(0.4D1 * t51 * t11)
      t56 = t54 ** 2
      t59 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t68 = lh ** 2
      t70 = pi ** 2
      t72 = 0.180D3 * t68 - 0.30D2 * t70
      t73 = pi * t72
      t74 = t73 * t25
      t78 = x3 * t11
      t81 = log(0.4D1 * t78 * t14)
      t83 = t81 ** 2
      t98 = log(0.4D1 * t15)
      t99 = t98 * pi
      t102 = t98 ** 2
      t103 = t102 * pi
      t109 = rrgg2qqbarhhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, 0.0D0, t2, 0.0D0)
      t135 = x2 * x3
      t138 = log(-0.4D1 * t135 * t40)
      t142 = log(0.4D1 * t135 * t15)
      t149 = x2 * t11
      t150 = t149 * t14
      t152 = log(0.4D1 * t150)
      t153 = t152 ** 2
      t157 = log(-0.4D1 * t149 * t14 * t39)
      t158 = t157 ** 2
      t172 = (0.90D2 * t5 * (-t6 + t18 * t19) + t27) * t29 * t31 / 0.720
     #D3 + t5 * (t37 * t19 - t43 * t19) * t46 * t31 / 0.8D1 + (0.90D2 * 
     #t5 * (t54 * t6 - t56 * t19 / 0.2D1 - t59) - 0.180D3 * t24 * t4 * (
     #-t6 + t54 * t19) - t74) * t31 / 0.720D3 + (0.90D2 * t5 * (t81 * t6
     # - t83 * t19 / 0.2D1 - t59) - 0.180D3 * t24 * t4 * (-t6 + t81 * t1
     #9) - t74) * t29 / 0.1440D4 - (0.180D3 * t99 * lh + 0.45D2 * t103 +
     # t73) * t4 * t6 / 0.1440D4 - t5 * t109 / 0.16D2 - (-0.90D2 * t103 
     #* lh + pi * (0.60D2 * lh * t70 - 0.240D3 * zeta3 - 0.120D3 * t68 *
     # lh) - 0.15D2 * t102 * t98 * pi - t99 * t72) * t4 * t19 / 0.1440D4
     # - (-0.180D3 * t24 - 0.90D2 * t99) * t4 * t59 / 0.1440D4 + t5 * (-
     #t138 * t19 + t142 * t19) * t29 * t46 / 0.16D2 + (0.90D2 * t5 * t19
     # * (-t153 / 0.2D1 + t158 / 0.2D1) + (0.90D2 * t5 * t6 - t27) * (t1
     #52 - t157)) * t46 / 0.1440D4
      t173 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t172)
      t175 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t172)
      t177 = t2 * x1
      t178 = -0.1D1 + x1
      t179 = t2 * t178
      t180 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #177, 0.0D0, -t179, 0.0D0)
      t182 = 0.1D1 / t9
      t183 = x1 * z
      t184 = -z - x1 + t183
      t185 = 0.1D1 / t184
      t186 = t182 * t185
      t187 = t178 ** 2
      t188 = t186 * t187
      t191 = log(-0.4D1 * t8 * t14 * t188)
      t192 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #177, 0.0D0, -t179, 0.0D0)
      t197 = t4 * t192
      t199 = 0.180D3 * t24 * t197
      t206 = t29 * t46 * t31
      t209 = t34 * t14
      t212 = log(-0.4D1 * t209 * t188)
      t223 = log(-0.4D1 * t51 * t188)
      t225 = t223 ** 2
      t228 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #177, 0.0D0, -t179, 0.0D0)
      t241 = (0.90D2 * t5 * (t180 - t191 * t192) - t199) * t29 * t31 / 0
     #.720D3 + t5 * t192 * t206 / 0.8D1 + (0.90D2 * t5 * (t180 - t212 * 
     #t192) - t199) * t46 * t31 / 0.720D3 + (0.90D2 * t5 * (-t223 * t180
     # + t225 * t192 / 0.2D1 + t228) - 0.180D3 * t24 * t4 * (t180 - t223
     # * t192) + t73 * t197) * t31 / 0.720D3
      t242 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t177, -t179, 0.0D0, t241)
      t244 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t179, t177, 0.0D0, t241)
      t246 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t172)
      t248 = t2 * x3
      t249 = -0.1D1 + x3
      t250 = t2 * t249
      t254 = log(-0.4D1 * t78 * t14 * t249)
      t255 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t248, -t250, 0.0D0)
      t257 = t254 ** 2
      t258 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t248, -t250, 0.0D0)
      t261 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t248, -t250, 0.0D0)
      t270 = t4 * t258
      t278 = log(-0.4D1 * t8 * t15 * t249)
      t293 = log(-0.4D1 * t149 * t14 * x3 * t249)
      t295 = x3 * t249
      t299 = log(0.4D1 * t150 * t295 * t39)
      t306 = (0.90D2 * t5 * (-t254 * t255 + t257 * t258 / 0.2D1 + t261) 
     #- 0.180D3 * t24 * t4 * (t255 - t254 * t258) + t73 * t270) * t29 / 
     #0.1440D4 + (0.90D2 * t5 * (t255 - t278 * t258) - 0.180D3 * t24 * t
     #270) * t29 * t31 / 0.720D3 + t5 * (-t293 * t258 + t299 * t258) * t
     #29 * t46 / 0.16D2
      t307 = FJET(XB1, XB2, s, 0.0D0, t248, 0.0D0, -t250, 0.0D0, t306)
      t309 = FJET(XB1, XB2, s, 0.0D0, -t250, 0.0D0, t248, 0.0D0, t306)
      t311 = x2 * x1
      t313 = t2 * t311 * t185
      t315 = t1 * x1
      t316 = t39 * s * t315
      t317 = t1 ** 2
      t322 = s * t317 * x2 * x1 * t178 * t185
      t323 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t313, -
     #t316, 0.0D0, -t179, t322)
      t327 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t313, -
     #t316, 0.0D0, -t179, t322)
      t332 = log(0.4D1 * t209 * t186 * t187 * t39)
      t344 = -t5 * t323 * t206 / 0.8D1 + (0.90D2 * t5 * (-t327 + t332 * 
     #t323) + 0.180D3 * t24 * t4 * t323) * t46 * t31 / 0.720D3
      t345 = FJET(XB1, XB2, s, 0.0D0, -t313, -t179, -t316, t322, t344)
      t347 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t172)
      t349 = FJET(XB1, XB2, s, t177, -t179, 0.0D0, 0.0D0, 0.0D0, t241)
      t351 = FJET(XB1, XB2, s, t248, 0.0D0, -t250, 0.0D0, 0.0D0, t306)
      t353 = x3 * x1
      t354 = t2 * t353
      t356 = t1 * t178
      t357 = x3 * s * t356
      t358 = t249 * s
      t359 = t358 * t315
      t360 = t358 * t356
      t361 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t354, -t
     #359, -t357, t360, 0.0D0)
      t367 = log(0.4D1 * t51 * t182 * t295 * t185 * t187)
      t368 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t354, -t
     #359, -t357, t360, 0.0D0)
      t383 = (0.90D2 * t5 * (-t361 + t367 * t368) + 0.180D3 * t24 * t4 *
     # t368) * t29 * t31 / 0.720D3 - t5 * t368 * t206 / 0.8D1
      t384 = FJET(XB1, XB2, s, t354, -t357, -t359, t360, 0.0D0, t383)
      t386 = t173 * t172 + t175 * t172 + t242 * t241 + t244 * t241 + t24
     #6 * t172 + t307 * t306 + t309 * t306 + t345 * t344 + t347 * t172 +
     # t349 * t241 + t351 * t306 + t384 * t383
      t387 = FJET(XB1, XB2, s, t360, -t359, -t357, t354, 0.0D0, t383)
      t391 = x3 * z
      t392 = t353 * z
      t393 = t135 * z
      t394 = t135 * x1
      t395 = t135 * t183
      t396 = cos(t12)
      t401 = Sqrt(-x3 * t39 * t184 * x2 * t249)
      t403 = 0.2D1 * t396 * t401
      t404 = z + x1 - t183 - x2 * z - t311 + t311 * z - t391 - t353 + t3
     #92 + t393 + t394 - t395 + t135 + t403
      t407 = t2 * x1 * t404 * t185
      t411 = t2 * x1 * (-t391 - t353 + t392 + t393 + t394 - t395 - x2 + 
     #t135 + t403) * t185
      t412 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t411, -t
     #407, -t357, t360, t322)
      t415 = t5 * t412 * t206 / 0.8D1
      t416 = FJET(XB1, XB2, s, t360, -t407, -t357, t411, t322, t415)
      t421 = t412 * t29 * t46 * t31
      t424 = FJET(XB1, XB2, s, t411, -t357, -t407, t360, t322, t415)
      t429 = FJET(XB1, XB2, s, -t179, t177, 0.0D0, 0.0D0, 0.0D0, t241)
      t431 = FJET(XB1, XB2, s, -t179, -t316, 0.0D0, -t313, t322, t344)
      t433 = FJET(XB1, XB2, s, -t250, 0.0D0, t248, 0.0D0, 0.0D0, t306)
      t435 = FJET(XB1, XB2, s, -t357, t354, t360, -t359, 0.0D0, t383)
      t437 = FJET(XB1, XB2, s, -t357, t411, t360, -t407, t322, t415)
      t442 = FJET(XB1, XB2, s, -t316, -t179, -t313, 0.0D0, t322, t344)
      t444 = FJET(XB1, XB2, s, -t359, t360, t354, -t357, 0.0D0, t383)
      t446 = FJET(XB1, XB2, s, -t313, 0.0D0, -t316, -t179, t322, t344)
      t448 = FJET(XB1, XB2, s, -t407, t360, t411, -t357, t322, t415)
      t453 = t387 * t383 + t416 * pi * t4 * t421 / 0.8D1 + t424 * pi * t
     #4 * t421 / 0.8D1 + t429 * t241 + t431 * t344 + t433 * t306 + t435 
     #* t383 + t437 * pi * t4 * t421 / 0.8D1 + t442 * t344 + t444 * t383
     # + t446 * t344 + t448 * pi * t4 * t421 / 0.8D1
      rrgg2qqbarhhardt8s2e0 = t386 + t453

      end function



      doubleprecision function rrgg2qqbarhhardt8s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * pi)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t31 = t30 * t13
      t34 = log(0.4D1 * t31 * t9)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t50 = log(0.4D1 * t47 * t13)
      t51 = -0.1D1 + x2
      t55 = log(-0.4D1 * t47 * t13 * t51)
      t58 = 0.1D1 / x2
      t65 = log(0.4D1 * t9 * t13)
      t66 = t65 * pi
      t74 = t65 ** 2
      t77 = lh ** 2
      t79 = pi ** 2
      t87 = rrgg2qqbarhhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t90 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 + (
     #0.90D2 * t5 * (-t6 + t34 * t17) + t25) * t40 / 0.720D3 - t5 * t17 
     #* t27 * t40 / 0.8D1 + t5 * t17 * (t50 - t55) * t58 / 0.16D2 - (-0.
     #180D3 * t22 - 0.90D2 * t66) * t4 * t6 / 0.1440D4 - (0.180D3 * t66 
     #* lh + 0.45D2 * t74 * pi + pi * (0.180D3 * t77 - 0.30D2 * t79)) * 
     #t4 * t17 / 0.1440D4 - t5 * t87 / 0.16D2
      t91 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t90)
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t90)
      t95 = t2 * x1
      t96 = -0.1D1 + x1
      t97 = t2 * t96
      t98 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t9
     #5, 0.0D0, -t97, 0.0D0)
      t103 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #95, 0.0D0, -t97, 0.0D0)
      t107 = 0.1D1 / (-z - x1 + x1 * z)
      t109 = t96 ** 2
      t113 = log(-0.4D1 * t31 / t7 * t107 * t109)
      t128 = t5 * t98 * t58 * t40 / 0.8D1 + (0.90D2 * t5 * (t103 - t113 
     #* t98) - 0.180D3 * t22 * t4 * t98) * t40 / 0.720D3 + t5 * t98 * t2
     #7 * t40 / 0.8D1
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t95, -t97, 0.0D0, t128)
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t97, t95, 0.0D0, t128)
      t133 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t90)
      t135 = t2 * x3
      t136 = -0.1D1 + x3
      t137 = t2 * t136
      t138 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t135, -t137, 0.0D0)
      t143 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t135, -t137, 0.0D0)
      t147 = log(-0.4D1 * t10 * t13 * t136)
      t158 = t5 * t138 * t27 * t40 / 0.8D1 + (0.90D2 * t5 * (t143 - t147
     # * t138) - 0.180D3 * t22 * t4 * t138) * t27 / 0.1440D4
      t159 = FJET(XB1, XB2, s, 0.0D0, t135, 0.0D0, -t137, 0.0D0, t158)
      t161 = FJET(XB1, XB2, s, 0.0D0, -t137, 0.0D0, t135, 0.0D0, t158)
      t165 = t2 * x1 * x2 * t107
      t167 = t1 * x1
      t168 = t51 * s * t167
      t169 = t1 ** 2
      t174 = s * t169 * x2 * x1 * t96 * t107
      t175 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t165, -
     #t168, 0.0D0, -t97, t174)
      t177 = t175 * t58 * t40
      t179 = t5 * t177 / 0.8D1
      t180 = FJET(XB1, XB2, s, 0.0D0, -t165, -t97, -t168, t174, -t179)
      t185 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t90)
      t187 = FJET(XB1, XB2, s, t95, -t97, 0.0D0, 0.0D0, 0.0D0, t128)
      t189 = FJET(XB1, XB2, s, t135, 0.0D0, -t137, 0.0D0, 0.0D0, t158)
      t192 = t2 * x1 * x3
      t194 = t1 * t96
      t195 = x3 * s * t194
      t196 = t136 * s
      t197 = t196 * t167
      t198 = t196 * t194
      t199 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t192, -t
     #197, -t195, t198, 0.0D0)
      t201 = t199 * t27 * t40
      t203 = t5 * t201 / 0.8D1
      t204 = FJET(XB1, XB2, s, t192, -t195, -t197, t198, 0.0D0, -t203)
      t209 = FJET(XB1, XB2, s, t198, -t197, -t195, t192, 0.0D0, -t203)
      t214 = FJET(XB1, XB2, s, -t97, t95, 0.0D0, 0.0D0, 0.0D0, t128)
      t216 = FJET(XB1, XB2, s, -t97, -t168, 0.0D0, -t165, t174, -t179)
      t221 = FJET(XB1, XB2, s, -t137, 0.0D0, t135, 0.0D0, 0.0D0, t158)
      t223 = FJET(XB1, XB2, s, -t195, t192, t198, -t197, 0.0D0, -t203)
      t228 = FJET(XB1, XB2, s, -t168, -t97, -t165, 0.0D0, t174, -t179)
      t233 = FJET(XB1, XB2, s, -t197, t198, t192, -t195, 0.0D0, -t203)
      t238 = FJET(XB1, XB2, s, -t165, 0.0D0, -t168, -t97, t174, -t179)
      rrgg2qqbarhhardt8s2em1 = t91 * t90 + t93 * t90 + t129 * t128 + t13
     #1 * t128 + t133 * t90 + t159 * t158 + t161 * t158 - t180 * pi * t4
     # * t177 / 0.8D1 + t185 * t90 + t187 * t128 + t189 * t158 - t204 * 
     #pi * t4 * t201 / 0.8D1 - t209 * pi * t4 * t201 / 0.8D1 + t214 * t1
     #28 - t216 * pi * t4 * t177 / 0.8D1 + t221 * t158 - t223 * pi * t4 
     #* t201 / 0.8D1 - t228 * pi * t4 * t177 / 0.8D1 - t233 * pi * t4 * 
     #t201 / 0.8D1 - t238 * pi * t4 * t177 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt8s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t15 = rrgg2qqbarhhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t20 = z ** 2
      t24 = Sin(x4 * pi)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t35 = -t5 * t6 * t7 / 0.8D1 - t5 * t6 * t11 / 0.16D2 - t5 * t15 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t4 * t6 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t40 = t2 * x1
      t42 = t2 * (-0.1D1 + x1)
      t43 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t4
     #0, 0.0D0, -t42, 0.0D0)
      t46 = t5 * t43 * t7 / 0.8D1
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t40, -t42, 0.0D0, t46)
      t50 = t4 * t43 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t40, 0.0D0, t46)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t59 = t2 * x3
      t61 = t2 * (-0.1D1 + x3)
      t62 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t59, -t61, 0.0D0)
      t65 = t5 * t62 * t11 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t61, 0.0D0, t65)
      t69 = t4 * t62 * t11
      t72 = FJET(XB1, XB2, s, 0.0D0, -t61, 0.0D0, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t78 = FJET(XB1, XB2, s, t40, -t42, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t61, 0.0D0, 0.0D0, t65)
      t86 = FJET(XB1, XB2, s, -t42, t40, 0.0D0, 0.0D0, 0.0D0, t46)
      t90 = FJET(XB1, XB2, s, -t61, 0.0D0, t59, 0.0D0, 0.0D0, t65)
      rrgg2qqbarhhardt8s2em2 = t36 * t35 + t38 * t35 + t47 * pi * t50 / 
     #0.8D1 + t53 * pi * t50 / 0.8D1 + t57 * t35 + t66 * pi * t69 / 0.16
     #D2 + t72 * pi * t69 / 0.16D2 + t76 * t35 + t78 * pi * t50 / 0.8D1 
     #+ t82 * pi * t69 / 0.16D2 + t86 * pi * t50 / 0.8D1 + t90 * pi * t6
     #9 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt8s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarhhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarhhardt8s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 
     #/ 0.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt8s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarhhard81J1
      doubleprecision rrgg2qqbarhhard81J2
      doubleprecision rrgg2qqbarhhard81J3
      doubleprecision rrgg2qqbarhhard81J4
      doubleprecision rrgg2qqbarhhard81J5
      doubleprecision rrgg2qqbarhhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt8s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarhhard81J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t8 = S14 ** 2
      t11 = S13 ** 2
      rrgg2qqbarhhard81J1 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8
     #D1 * t5 * t2 * nf + 0.3D1 / 0.8D1 * t5 * (-t8 + 0.2D1 * S13 * S14 
     #- t11) * nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard81J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t11 = S14 ** 2
      t14 = S13 ** 2
      t17 = S24 ** 2
      rrgg2qqbarhhard81J2 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8
     #D1 * (0.2D1 * t5 * t2 - 0.2D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (
     #0.2D1 * t5 * (-t11 + 0.2D1 * S13 * S14 - t14) - 0.2D1 * t5 * t17) 
     #* nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard81J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t12 = S14 ** 2
      t15 = S13 ** 2
      t19 = S24 ** 2
      rrgg2qqbarhhard81J3 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8
     #D1 * (0.3D1 * t5 * t2 - 0.4D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (
     #0.3D1 * t5 * (-t12 + 0.2D1 * S13 * S14 - t15) - 0.4D1 * t5 * t19) 
     #* nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard81J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t12 = S14 ** 2
      t15 = S13 ** 2
      t19 = S24 ** 2
      rrgg2qqbarhhard81J4 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8
     #D1 * (0.4D1 * t5 * t2 - 0.6D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (
     #0.4D1 * t5 * (-t12 + 0.2D1 * S13 * S14 - t15) - 0.6D1 * t5 * t19) 
     #* nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard81J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t12 = S14 ** 2
      t15 = S13 ** 2
      t19 = S24 ** 2
      rrgg2qqbarhhard81J5 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8
     #D1 * (0.5D1 * t5 * t2 - 0.8D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (
     #0.5D1 * t5 * (-t12 + 0.2D1 * S13 * S14 - t15) - 0.8D1 * t5 * t19) 
     #* nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard81J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 - S14
      t9 = S24 ** 2
      rrgg2qqbarhhard81J6 = (0.15D2 / 0.4D1 * t1 * nf * S12 - 0.15D2 / 0
     #.4D1 * t1 * S24 * nf - 0.15D2 / 0.4D1 * t1 * t9 * nf / S12) / pi *
     # wd / z

      end function
  
   
      subroutine rrgg2qqbarhsoftt8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt8s1e1  
      doubleprecision rrgg2qqbarhsoftt8s1e0  
      doubleprecision rrgg2qqbarhsoftt8s1em1  
      doubleprecision rrgg2qqbarhsoftt8s1em2  
      doubleprecision rrgg2qqbarhsoftt8s1em3  
      doubleprecision rrgg2qqbarhsoftt8s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt8s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt8s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt8s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt8s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt8s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt8s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt8s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt8s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt8s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt8s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt8s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt8s1em4 = 0.0D0

      end function
