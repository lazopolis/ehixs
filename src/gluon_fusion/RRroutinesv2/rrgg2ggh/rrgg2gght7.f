  
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * pi
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t10 = log(0.4D1 * t8)
      t11 = t10 ** 2
      t14 = pi ** 2
      t16 = 0.60D2 * lh * t14
      t17 = 0.240D3 * zeta3
      t18 = lh ** 2
      t20 = 0.120D3 * t18 * lh
      t21 = t11 * t10
      t23 = 0.180D3 * t18
      t24 = 0.30D2 * t14
      t25 = -t23 + t24
      t28 = s ** 2
      t30 = 0.1D1 / t28 / s
      t32 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t33 = pi * t32
      t41 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t42 = pi * t41
      t49 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t53 = t30 * pi
      t54 = rrgg2ggh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = -t16 + t17 + t20
      t65 = t14 ** 2
      t66 = t18 ** 2
      t70 = t11 ** 2
      t74 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t75 = pi * t74
      t78 = x1 ** 2
      t79 = x3 * t78
      t82 = log(0.4D1 * t79 * t8)
      t84 = -0.1D1 + x3
      t85 = 0.1D1 / t84
      t86 = t8 * t85
      t89 = log(-0.4D1 * t79 * t86)
      t91 = t89 ** 2
      t95 = cos(t5)
      t97 = Sqrt(-x3 * t84)
      t101 = 0.1D1 / (-0.1D1 + 0.2D1 * t95 * t97 - x3)
      t103 = t82 ** 2
      t109 = lh * t30
      t118 = t25 * t30
      t120 = t74 * t101 + t74
      t124 = 0.1D1 / x3
      t126 = 0.1D1 / x1
      t129 = t78 * t7
      t130 = t129 * t4
      t132 = log(0.4D1 * t130)
      t137 = t132 ** 2
      t147 = t61 * t30
      t148 = t147 * t75
      t159 = x2 ** 2
      t160 = t78 * t159
      t161 = t160 * x3
      t164 = log(-0.4D1 * t161 * t86)
      t168 = t159 * x3
      t169 = t168 * t130
      t171 = log(0.4D1 * t169)
      t177 = -pi * t120
      t182 = 0.1D1 / x2
      t183 = t126 * t182
      t188 = log(0.4D1 * t160 * t8)
      t190 = t188 ** 2
      t201 = t118 * t75
      t211 = x3 * t7
      t215 = log(-0.4D1 * t211 * t4 * t85)
      t219 = log(0.4D1 * t211 * t4)
      t222 = t219 ** 2
      t224 = t215 ** 2
      t254 = log(0.4D1 * t168 * t8)
      t256 = t254 ** 2
      t261 = log(-0.4D1 * t168 * t86)
      t263 = t261 ** 2
      t284 = t159 * t7
      t287 = log(0.4D1 * t284 * t4)
      t292 = t287 ** 2
      t312 = (0.90D2 * t11 * lh - t16 + t17 + t20 + 0.15D2 * t21 - t10 *
     # t25) * t30 * t33 / 0.2880D4 + (-0.180D3 * t10 * lh - 0.45D2 * t11
     # - t23 + t24) * t30 * t42 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t1
     #0) * t30 * pi * t49 / 0.2880D4 - t53 * t54 / 0.32D2 + (-0.30D2 * t
     #21 * lh + t11 * t25 / 0.2D1 - t10 * t61 - 0.480D3 * lh * zeta3 - t
     #65 - 0.60D2 * t66 + 0.60D2 * t18 * t14 - 0.15D2 / 0.4D1 * t70) * t
     #30 * t75 / 0.2880D4 + (-0.90D2 * t53 * (-t82 * t32 + (-t89 * t32 +
     # t91 * t74 / 0.2D1 + t41) * t101 + t41 + t103 * t74 / 0.2D1) + 0.1
     #80D3 * t109 * pi * ((t32 - t89 * t74) * t101 - t82 * t74 + t32) + 
     #t118 * pi * t120) * t124 * t126 / 0.1440D4 - (t118 * pi * (-t32 + 
     #t132 * t74) - 0.90D2 * t53 * (-t137 * t32 / 0.2D1 - t49 + t137 * t
     #132 * t74 / 0.6D1 + t132 * t41) - t148 + 0.180D3 * t109 * pi * (t1
     #32 * t32 - t137 * t74 / 0.2D1 - t41)) * t126 / 0.1440D4 - (-0.90D2
     # * t53 * (-t32 - (t32 - t164 * t74) * t101 + t171 * t74) + 0.180D3
     # * t109 * t177) * t124 * t183 / 0.720D3 - (-0.90D2 * t53 * (t188 *
     # t32 - t190 * t74 / 0.2D1 - t41) + 0.180D3 * t109 * pi * (-t32 + t
     #188 * t74) - t201) * t126 * t182 / 0.720D3 + ((0.180D3 * t109 * t3
     #3 + t201 - 0.90D2 * t53 * t41) * (-t215 * t101 - t219) - 0.90D2 * 
     #t53 * t74 * (-t222 * t219 / 0.6D1 - t224 * t215 * t101 / 0.6D1) + 
     #(t118 * t33 - 0.90D2 * t53 * t49 + t148 + 0.180D3 * t109 * t42) * 
     #(t101 + 0.1D1) + (-0.90D2 * t53 * t32 + 0.180D3 * t109 * t75) * (t
     #224 * t101 / 0.2D1 + t222 / 0.2D1)) * t124 / 0.2880D4 - (-0.90D2 *
     # t53 * (t254 * t32 - t41 - t256 * t74 / 0.2D1 - (-t261 * t32 + t26
     #3 * t74 / 0.2D1 + t41) * t101) + 0.180D3 * t109 * pi * (t254 * t74
     # - t32 - (t32 - t261 * t74) * t101) + t118 * t177) * t124 * t182 /
     # 0.1440D4 - (t118 * pi * (-t32 + t287 * t74) - 0.90D2 * t53 * (-t2
     #92 * t32 / 0.2D1 - t49 + t292 * t287 * t74 / 0.6D1 + t287 * t41) -
     # t148 + 0.180D3 * t109 * pi * (t287 * t32 - t292 * t74 / 0.2D1 - t
     #41)) * t182 / 0.1440D4
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t312)
      t315 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t312)
      t317 = t2 * x1
      t318 = -0.1D1 + x1
      t319 = t2 * t318
      t320 = t79 * t7
      t321 = t318 ** 2
      t322 = t4 * t321
      t323 = x1 * z
      t324 = 0.1D1 - x1 + t323
      t325 = 0.1D1 / t324
      t327 = t322 * t325 * t85
      t330 = log(-0.4D1 * t320 * t327)
      t331 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319, 0
     #.0D0, t317, 0.0D0)
      t333 = t330 ** 2
      t334 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319, 0
     #.0D0, t317, 0.0D0)
      t337 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319, 0
     #.0D0, t317, 0.0D0)
      t339 = x3 * x1
      t340 = t339 * z
      t343 = x3 * t324
      t345 = Sqrt(-t343 * t84)
      t349 = 0.1D1 / (-0.2D1 * t340 + 0.2D1 * t339 - 0.1D1 - x3 + 0.2D1 
     #* t95 * t345)
      t351 = t322 * t325
      t354 = log(0.4D1 * t320 * t351)
      t356 = t354 ** 2
      t371 = -t334 - t334 * t349
      t380 = log(0.4D1 * t129 * t351)
      t385 = t380 ** 2
      t388 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319, 0
     #.0D0, t317, 0.0D0)
      t396 = pi * t334
      t408 = t321 * t325
      t412 = log(0.4D1 * t161 * t8 * t408)
      t414 = t168 * t129
      t417 = log(-0.4D1 * t414 * t327)
      t432 = t160 * t7
      t435 = log(0.4D1 * t432 * t351)
      t437 = t435 ** 2
      t453 = (-0.90D2 * t53 * (-(-t330 * t331 + t333 * t334 / 0.2D1 + t3
     #37) * t349 + t354 * t331 - t356 * t334 / 0.2D1 - t337) + 0.180D3 *
     # t109 * pi * (-(t331 - t330 * t334) * t349 - t331 + t354 * t334) +
     # t118 * pi * t371) * t124 * t126 / 0.1440D4 - (t118 * pi * (t331 -
     # t380 * t334) - 0.90D2 * t53 * (t385 * t331 / 0.2D1 + t388 - t385 
     #* t380 * t334 / 0.6D1 - t380 * t337) + t147 * t396 + 0.180D3 * t10
     #9 * pi * (-t380 * t331 + t385 * t334 / 0.2D1 + t337)) * t126 / 0.1
     #440D4 - (-0.90D2 * t53 * (t331 - t412 * t334 + (t331 - t417 * t334
     #) * t349) - 0.180D3 * t109 * pi * t371) * t124 * t183 / 0.720D3 - 
     #(-0.90D2 * t53 * (-t435 * t331 + t437 * t334 / 0.2D1 + t337) + 0.1
     #80D3 * t109 * pi * (t331 - t435 * t334) + t118 * t396) * t126 * t1
     #82 / 0.720D3
      t454 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t317, -t319, 0.0D0, t453)
      t456 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t319, t317, 0.0D0, t453)
      t458 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t312)
      t461 = x2 * s * t1
      t462 = -0.1D1 + x2
      t463 = t462 * s
      t464 = t463 * t1
      t465 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, t461, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t466 = t8 * t462
      t469 = log(-0.4D1 * t161 * t466)
      t470 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t461, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t473 = x2 * z
      t475 = 0.1D1 / (0.1D1 - x2 + t473)
      t480 = pi * t470 * t475
      t489 = log(-0.4D1 * t160 * t466)
      t491 = t489 ** 2
      t494 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, t461, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t505 = t118 * t480
      t512 = log(-0.4D1 * t168 * t466)
      t514 = t512 ** 2
      t531 = t4 * t462
      t534 = log(-0.4D1 * t284 * t531)
      t540 = t534 ** 2
      t543 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, t461, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t564 = -(-0.90D2 * t53 * (t465 - t469 * t470) * t475 + 0.180D3 * t
     #109 * t480) * t124 * t183 / 0.720D3 - (-0.90D2 * t53 * (-t489 * t4
     #65 + t491 * t470 / 0.2D1 + t494) * t475 + 0.180D3 * t109 * pi * (t
     #465 - t489 * t470) * t475 + t505) * t126 * t182 / 0.720D3 - (-0.90
     #D2 * t53 * (-t512 * t465 + t514 * t470 / 0.2D1 + t494) * t475 + 0.
     #180D3 * t109 * pi * (t465 - t512 * t470) * t475 + t505) * t124 * t
     #182 / 0.1440D4 - (t118 * pi * (t465 - t534 * t470) * t475 - 0.90D2
     # * t53 * (t540 * t465 / 0.2D1 + t543 - t540 * t534 * t470 / 0.6D1 
     #- t534 * t494) * t475 + t147 * t480 + 0.180D3 * t109 * pi * (-t534
     # * t465 + t540 * t470 / 0.2D1 + t494) * t475) * t182 / 0.1440D4
      t565 = FJET(XB1, XB2, s, 0.0D0, t461, 0.0D0, -t464, 0.0D0, t564)
      t567 = x2 * x3
      t570 = Sqrt(x3 * t462 * t84)
      t571 = t95 * t570
      t573 = 0.2D1 * t571 * x2
      t575 = 0.1D1 - x3 + t567
      t576 = 0.1D1 / t575
      t578 = t2 * (0.1D1 - x3 - x2 + t567 + t168 + t573) * t576
      t579 = 0.2D1 * t571
      t583 = t2 * x2 * (-0.1D1 + t567 + t579) * t576
      t584 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t583, t578, 0.
     #0D0, 0.0D0, 0.0D0)
      t585 = t575 ** 2
      t586 = 0.1D1 / t585
      t588 = t531 * t84 * t586
      t591 = log(0.4D1 * t414 * t588)
      t592 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t583, t578, 0.
     #0D0, 0.0D0, 0.0D0)
      t595 = z * t95
      t600 = z * t159 * x3
      t602 = 0.1D1 / (-0.1D1 + t567 - t168 - t473 + t579 + 0.2D1 * t595 
     #* t570 * x2 + x2 - x3 - t573 + t600)
      t607 = pi * t592 * t602
      t617 = log(0.4D1 * t168 * t7 * t588)
      t619 = t617 ** 2
      t622 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t583, t578, 0.
     #0D0, 0.0D0, 0.0D0)
      t638 = -(-0.90D2 * t53 * (t584 - t591 * t592) * t602 + 0.180D3 * t
     #109 * t607) * t124 * t183 / 0.720D3 - (-0.90D2 * t53 * (-t617 * t5
     #84 + t619 * t592 / 0.2D1 + t622) * t602 + 0.180D3 * t109 * pi * (t
     #584 - t617 * t592) * t602 + t118 * t607) * t124 * t182 / 0.1440D4
      t639 = FJET(XB1, XB2, s, 0.0D0, t578, 0.0D0, -t583, 0.0D0, t638)
      t641 = FJET(XB1, XB2, s, 0.0D0, -t464, 0.0D0, t461, 0.0D0, t564)
      t643 = FJET(XB1, XB2, s, 0.0D0, -t583, 0.0D0, t578, 0.0D0, t638)
      t647 = t2 * t318 * x2 * t325
      t649 = t463 * t1 * t318
      t650 = t1 ** 2
      t655 = s * t650 * x2 * x1 * t318 * t325
      t656 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t647, t649, 0.
     #0D0, t317, -t655)
      t657 = t324 * t656
      t659 = t322 * t325 * t462
      t662 = log(-0.4D1 * t414 * t659)
      t664 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t647, t649, 0.
     #0D0, t317, -t655)
      t667 = x1 * x2
      t668 = t667 * z
      t670 = 0.1D1 / (-0.1D1 - t667 + x1 + t668 + x2 - t323 - t473)
      t674 = t109 * pi
      t676 = t324 * t664 * t670
      t684 = log(-0.4D1 * t432 * t659)
      t685 = t684 * t324
      t687 = t684 ** 2
      t691 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t647, t649, 0.
     #0D0, t317, -t655)
      t709 = -(-0.90D2 * t53 * (t657 - t662 * t324 * t664) * t670 + 0.18
     #0D3 * t674 * t676) * t124 * t183 / 0.720D3 - (-0.90D2 * t53 * (-t6
     #85 * t656 + t687 * t324 * t664 / 0.2D1 + t324 * t691) * t670 + 0.1
     #80D3 * t109 * pi * (t657 - t685 * t664) * t670 + t118 * pi * t676)
     # * t126 * t182 / 0.720D3
      t710 = FJET(XB1, XB2, s, 0.0D0, -t647, t317, t649, -t655, t709)
      t712 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t312)
      t714 = FJET(XB1, XB2, s, t317, t649, 0.0D0, -t647, -t655, t709)
      t716 = t313 * t312 + t315 * t312 + t454 * t453 + t456 * t453 + t45
     #8 * t312 + t565 * t564 + t639 * t638 + t641 * t564 + t643 * t638 +
     # t710 * t709 + t712 * t312 + t714 * t709
      t717 = FJET(XB1, XB2, s, t317, -t319, 0.0D0, 0.0D0, 0.0D0, t453)
      t719 = FJET(XB1, XB2, s, t461, 0.0D0, -t464, 0.0D0, 0.0D0, t564)
      t721 = FJET(XB1, XB2, s, t578, 0.0D0, -t583, 0.0D0, 0.0D0, t638)
      t723 = FJET(XB1, XB2, s, t649, t317, -t647, 0.0D0, -t655, t709)
      t726 = t317 * t567 * t576
      t727 = t339 * x2
      t728 = t339 * t473
      t729 = t462 * t84
      t731 = Sqrt(t343 * t729)
      t732 = t95 * t731
      t733 = 0.2D1 * t732
      t738 = t319 * x2 * (t339 - t340 + t567 - t727 + t728 - 0.1D1 + t73
     #3) * t325 * t576
      t742 = t84 * s * t1 * x1 * t576
      t744 = 0.2D1 * t732 * x2
      t745 = 0.1D1 - x1 + t323 - x2 + t667 - t668 - x3 + t339 - t340 + t
     #567 - t727 + t728 + t168 + t744
      t748 = t319 * t745 * t325 * t576
      t749 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, t738, -t748, t7
     #26, -t742, -t655)
      t755 = log(0.4D1 * t169 * t408 * t729 * t586)
      t757 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t738, -t748, t7
     #26, -t742, -t655)
      t760 = t731 * x1
      t770 = x1 * t159
      t773 = t78 * t3
      t777 = 0.1D1 - t733 + x3 - x2 + t473 + 0.2D1 * t595 * t760 * x2 - 
     #t668 - t567 + t168 + t161 + 0.2D1 * t732 * x1 - 0.4D1 * t79 * z - 
     #0.2D1 * t79 * x2 - 0.2D1 * t770 * x3 + 0.2D1 * t773 * x3 + t323 - 
     #0.3D1 * t339
      t794 = t3 * x3
      t803 = 0.3D1 * t340 + t667 - 0.4D1 * t728 - 0.2D1 * t732 * t667 - 
     #0.2D1 * t595 * t731 * x2 - 0.2D1 * t595 * t760 + t339 * x2 * t3 + 
     #0.4D1 * t79 * t473 - 0.2D1 * t160 * x3 * z - t770 * t794 + t160 * 
     #t794 + 0.3D1 * t323 * t168 - 0.2D1 * t773 * t567 - x1 + 0.3D1 * t7
     #27 + t744 - t600 + 0.2D1 * t79
      t805 = 0.1D1 / (t777 + t803)
      t813 = -0.90D2 * t53 * (t324 * t749 - t755 * t324 * t757) * t805 +
     # 0.180D3 * t674 * t324 * t757 * t805
      t816 = t813 * t124 * t183 / 0.720D3
      t817 = FJET(XB1, XB2, s, t726, t738, -t742, -t748, -t655, -t816)
      t820 = t124 * t126 * t182
      t823 = FJET(XB1, XB2, s, t738, t726, -t748, -t742, -t655, -t816)
      t827 = FJET(XB1, XB2, s, -t319, t317, 0.0D0, 0.0D0, 0.0D0, t453)
      t829 = FJET(XB1, XB2, s, -t464, 0.0D0, t461, 0.0D0, 0.0D0, t564)
      t831 = FJET(XB1, XB2, s, -t583, 0.0D0, t578, 0.0D0, 0.0D0, t638)
      t833 = FJET(XB1, XB2, s, -t647, 0.0D0, t649, t317, -t655, t709)
      t835 = FJET(XB1, XB2, s, -t742, -t748, t726, t738, -t655, -t816)
      t839 = FJET(XB1, XB2, s, -t748, -t742, t738, t726, -t655, -t816)
      t843 = t717 * t453 + t719 * t564 + t721 * t638 + t723 * t709 - t81
     #7 * t813 * t820 / 0.720D3 - t823 * t813 * t820 / 0.720D3 + t827 * 
     #t453 + t829 * t564 + t831 * t638 + t833 * t709 - t835 * t813 * t82
     #0 / 0.720D3 - t839 * t813 * t820 / 0.720D3
      rrgg2gght7s1e1 = t716 + t843

      end function



      doubleprecision function rrgg2gght7s1e0
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = x4 * pi
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
      t38 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = lh * t5
      t42 = pi * t7
      t44 = 0.180D3 * t41 * t42
      t49 = pi * t38
      t52 = lh ** 2
      t53 = 0.180D3 * t52
      t54 = pi ** 2
      t55 = 0.30D2 * t54
      t56 = -t53 + t55
      t57 = t56 * t5
      t58 = t57 * t42
      t59 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t66 = 0.1D1 / x3
      t69 = t13 * t10
      t71 = log(0.4D1 * t69)
      t74 = t71 ** 2
      t80 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t104 = x2 ** 2
      t105 = t104 * x3
      t108 = log(0.4D1 * t105 * t69)
      t110 = t69 * t15
      t113 = log(-0.4D1 * t105 * t110)
      t121 = -t7 * t27 - t7
      t127 = 0.1D1 / x2
      t130 = t104 * t10
      t133 = log(0.4D1 * t130 * t13)
      t135 = t133 ** 2
      t149 = x1 ** 2
      t150 = x3 * t149
      t153 = log(-0.4D1 * t150 * t110)
      t159 = log(0.4D1 * t150 * t69)
      t170 = 0.1D1 / x1
      t175 = t66 * t170 * t127
      t178 = t104 * t149
      t181 = log(0.4D1 * t178 * t69)
      t190 = t149 * t10
      t193 = log(0.4D1 * t190 * t13)
      t195 = t193 ** 2
      t209 = (-0.90D2 * t6 * t7 * (t20 * t27 / 0.2D1 + t32 / 0.2D1) + (-
     #0.90D2 * t6 * t38 + t44) * (-t19 * t27 - t31) + (0.180D3 * t41 * t
     #49 + t58 - 0.90D2 * t6 * t59) * (t27 + 0.1D1)) * t66 / 0.2880D4 + 
     #(-0.180D3 * t71 * lh - 0.45D2 * t74 - t53 + t55) * t5 * t49 / 0.28
     #80D4 - t6 * t80 / 0.32D2 + (0.90D2 * t74 * lh - 0.60D2 * lh * t54 
     #+ 0.240D3 * zeta3 + 0.120D3 * t52 * lh + 0.15D2 * t74 * t71 - t71 
     #* t56) * t5 * t42 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t71) * t5 
     #* pi * t59 / 0.2880D4 - (-0.90D2 * t6 * (t108 * t7 - t38 - (t38 - 
     #t113 * t7) * t27) + 0.180D3 * t41 * pi * t121) * t66 * t127 / 0.14
     #40D4 - (-0.90D2 * t6 * (t133 * t38 - t135 * t7 / 0.2D1 - t59) + 0.
     #180D3 * t41 * pi * (-t38 + t133 * t7) - t58) * t127 / 0.1440D4 + (
     #-0.90D2 * t6 * ((t38 - t153 * t7) * t27 - t159 * t7 + t38) - 0.180
     #D3 * t41 * pi * t121) * t66 * t170 / 0.1440D4 + t6 * t121 * t175 /
     # 0.8D1 - (-0.90D2 * t6 * (-t38 + t181 * t7) - t44) * t170 * t127 /
     # 0.720D3 - (-0.90D2 * t6 * (t193 * t38 - t195 * t7 / 0.2D1 - t59) 
     #+ 0.180D3 * t41 * pi * (-t38 + t193 * t7) - t58) * t170 / 0.1440D4
      t210 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t209)
      t212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t209)
      t214 = t2 * x1
      t215 = -0.1D1 + x1
      t216 = t2 * t215
      t217 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t216, 0
     #.0D0, t214, 0.0D0)
      t218 = t150 * t10
      t219 = t215 ** 2
      t220 = t13 * t219
      t221 = x1 * z
      t222 = 0.1D1 - x1 + t221
      t223 = 0.1D1 / t222
      t228 = log(-0.4D1 * t218 * t220 * t223 * t15)
      t229 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t216, 0
     #.0D0, t214, 0.0D0)
      t232 = x3 * x1
      t233 = t232 * z
      t236 = x3 * t222
      t238 = Sqrt(-t236 * t14)
      t242 = 0.1D1 / (-0.2D1 * t233 + 0.2D1 * t232 - 0.1D1 - x3 + 0.2D1 
     #* t21 * t238)
      t244 = t220 * t223
      t247 = log(0.4D1 * t218 * t244)
      t253 = -t229 - t229 * t242
      t265 = t178 * t10
      t268 = log(0.4D1 * t265 * t244)
      t273 = pi * t229
      t282 = log(0.4D1 * t190 * t244)
      t284 = t282 ** 2
      t287 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t216, 0
     #.0D0, t214, 0.0D0)
      t300 = (-0.90D2 * t6 * (-(t217 - t228 * t229) * t242 - t217 + t247
     # * t229) + 0.180D3 * t41 * pi * t253) * t66 * t170 / 0.1440D4 - t6
     # * t253 * t175 / 0.8D1 - (-0.90D2 * t6 * (t217 - t268 * t229) + 0.
     #180D3 * t41 * t273) * t170 * t127 / 0.720D3 - (-0.90D2 * t6 * (-t2
     #82 * t217 + t284 * t229 / 0.2D1 + t287) + 0.180D3 * t41 * pi * (t2
     #17 - t282 * t229) + t57 * t273) * t170 / 0.1440D4
      t301 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t214, -t216, 0.0D0, t300)
      t303 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t216, t214, 0.0D0, t300)
      t305 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t209)
      t308 = x2 * s * t1
      t309 = -0.1D1 + x2
      t310 = t309 * s
      t311 = t310 * t1
      t312 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, t308, -t311, 0.
     #0D0, 0.0D0, 0.0D0)
      t313 = t69 * t309
      t316 = log(-0.4D1 * t105 * t313)
      t317 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t308, -t311, 0.
     #0D0, 0.0D0, 0.0D0)
      t320 = x2 * z
      t322 = 0.1D1 / (0.1D1 - x2 + t320)
      t327 = pi * t317 * t322
      t329 = 0.180D3 * t41 * t327
      t334 = t13 * t309
      t337 = log(-0.4D1 * t130 * t334)
      t339 = t337 ** 2
      t342 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, t308, -t311, 0.
     #0D0, 0.0D0, 0.0D0)
      t359 = t170 * t127
      t365 = log(-0.4D1 * t178 * t313)
      t375 = -(-0.90D2 * t6 * (t312 - t316 * t317) * t322 + t329) * t66 
     #* t127 / 0.1440D4 - (-0.90D2 * t6 * (-t337 * t312 + t339 * t317 / 
     #0.2D1 + t342) * t322 + 0.180D3 * t41 * pi * (t312 - t337 * t317) *
     # t322 + t57 * t327) * t127 / 0.1440D4 + t6 * t317 * t322 * t66 * t
     #359 / 0.8D1 - (-0.90D2 * t6 * (t312 - t365 * t317) * t322 + t329) 
     #* t170 * t127 / 0.720D3
      t376 = FJET(XB1, XB2, s, 0.0D0, t308, 0.0D0, -t311, 0.0D0, t375)
      t378 = x2 * x3
      t381 = Sqrt(x3 * t309 * t14)
      t382 = t21 * t381
      t384 = 0.2D1 * t382 * x2
      t386 = 0.1D1 - x3 + t378
      t387 = 0.1D1 / t386
      t389 = t2 * (0.1D1 - x3 - x2 + t378 + t105 + t384) * t387
      t390 = 0.2D1 * t382
      t394 = t2 * x2 * (-0.1D1 + t378 + t390) * t387
      t395 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t394, t389, 0.
     #0D0, 0.0D0, 0.0D0)
      t397 = t386 ** 2
      t403 = log(0.4D1 * t105 * t10 * t334 * t14 / t397)
      t404 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t394, t389, 0.
     #0D0, 0.0D0, 0.0D0)
      t407 = z * t21
      t412 = z * t104 * x3
      t414 = 0.1D1 / (-0.1D1 + t378 - t105 - t320 + t390 + 0.2D1 * t407 
     #* t381 * x2 + x2 - x3 - t384 + t412)
      t431 = -(-0.90D2 * t6 * (t395 - t403 * t404) * t414 + 0.180D3 * t4
     #1 * pi * t404 * t414) * t66 * t127 / 0.1440D4 + t6 * t404 * t414 *
     # t66 * t359 / 0.8D1
      t432 = FJET(XB1, XB2, s, 0.0D0, t389, 0.0D0, -t394, 0.0D0, t431)
      t434 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t308, 0.0D0, t375)
      t436 = FJET(XB1, XB2, s, 0.0D0, -t394, 0.0D0, t389, 0.0D0, t431)
      t440 = t2 * t215 * x2 * t223
      t442 = t310 * t1 * t215
      t443 = t1 ** 2
      t448 = s * t443 * x2 * x1 * t215 * t223
      t449 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t440, t442, 0.
     #0D0, t214, -t448)
      t450 = t222 * t449
      t452 = x1 * x2
      t453 = t452 * z
      t455 = 0.1D1 / (-0.1D1 - t452 + x1 + t453 + x2 - t221 - t320)
      t460 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t440, t442, 0.
     #0D0, t214, -t448)
      t467 = log(-0.4D1 * t265 * t13 * t223 * t219 * t309)
      t482 = t6 * t450 * t455 * t66 * t359 / 0.8D1 - (-0.90D2 * t6 * (t2
     #22 * t460 - t467 * t222 * t449) * t455 + 0.180D3 * t41 * pi * t450
     # * t455) * t170 * t127 / 0.720D3
      t483 = FJET(XB1, XB2, s, 0.0D0, -t440, t214, t442, -t448, t482)
      t485 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t209)
      t487 = FJET(XB1, XB2, s, t214, t442, 0.0D0, -t440, -t448, t482)
      t489 = t210 * t209 + t212 * t209 + t301 * t300 + t303 * t300 + t30
     #5 * t209 + t376 * t375 + t432 * t431 + t434 * t375 + t436 * t431 +
     # t483 * t482 + t485 * t209 + t487 * t482
      t490 = FJET(XB1, XB2, s, t214, -t216, 0.0D0, 0.0D0, 0.0D0, t300)
      t492 = FJET(XB1, XB2, s, t308, 0.0D0, -t311, 0.0D0, 0.0D0, t375)
      t494 = FJET(XB1, XB2, s, t389, 0.0D0, -t394, 0.0D0, 0.0D0, t431)
      t496 = FJET(XB1, XB2, s, t442, t214, -t440, 0.0D0, -t448, t482)
      t499 = t214 * t378 * t387
      t500 = t232 * x2
      t501 = t232 * t320
      t504 = Sqrt(t236 * t309 * t14)
      t505 = t21 * t504
      t506 = 0.2D1 * t505
      t511 = t216 * x2 * (t232 - t233 + t378 - t500 + t501 - 0.1D1 + t50
     #6) * t223 * t387
      t515 = t14 * s * t1 * x1 * t387
      t517 = 0.2D1 * t505 * x2
      t518 = 0.1D1 - x1 + t221 - x2 + t452 - t453 - x3 + t232 - t233 + t
     #378 - t500 + t501 + t105 + t517
      t521 = t216 * t518 * t223 * t387
      t522 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t511, -t521, t4
     #99, -t515, -t448)
      t527 = t504 * x1
      t533 = 0.1D1 + 0.3D1 * t233 - t412 - t453 - x2 - x1 + t452 + t221 
     #- 0.3D1 * t232 + t320 - t378 + t105 + 0.2D1 * t407 * t527 * x2 + x
     #3 - t506 + 0.2D1 * t150 - 0.4D1 * t501
      t548 = x1 * t104
      t549 = t12 * x3
      t554 = t149 * t12
      t569 = -0.2D1 * t505 * t452 - 0.2D1 * t407 * t504 * x2 - 0.2D1 * t
     #407 * t527 + t232 * x2 * t12 + 0.4D1 * t150 * t320 - 0.2D1 * t178 
     #* x3 * z - t548 * t549 + t178 * t549 + 0.3D1 * t221 * t105 - 0.2D1
     # * t554 * t378 + 0.3D1 * t500 + t517 + t178 * x3 + 0.2D1 * t505 * 
     #x1 - 0.4D1 * t150 * z - 0.2D1 * t150 * x2 - 0.2D1 * t548 * x3 + 0.
     #2D1 * t554 * x3
      t571 = 0.1D1 / (t533 + t569)
      t575 = t6 * t222 * t522 * t571 * t66 * t359 / 0.8D1
      t576 = FJET(XB1, XB2, s, t499, t511, -t515, -t521, -t448, t575)
      t578 = pi * t222
      t581 = t522 * t571 * t175
      t584 = FJET(XB1, XB2, s, t511, t499, -t521, -t515, -t448, t575)
      t589 = FJET(XB1, XB2, s, -t216, t214, 0.0D0, 0.0D0, 0.0D0, t300)
      t591 = FJET(XB1, XB2, s, -t311, 0.0D0, t308, 0.0D0, 0.0D0, t375)
      t593 = FJET(XB1, XB2, s, -t394, 0.0D0, t389, 0.0D0, 0.0D0, t431)
      t595 = FJET(XB1, XB2, s, -t440, 0.0D0, t442, t214, -t448, t482)
      t597 = FJET(XB1, XB2, s, -t515, -t521, t499, t511, -t448, t575)
      t602 = FJET(XB1, XB2, s, -t521, -t515, t511, t499, -t448, t575)
      t607 = t490 * t300 + t492 * t375 + t494 * t431 + t496 * t482 + t57
     #6 * t5 * t578 * t581 / 0.8D1 + t584 * t5 * t578 * t581 / 0.8D1 + t
     #589 * t300 + t591 * t375 + t593 * t431 + t595 * t482 + t597 * t5 *
     # t578 * t581 / 0.8D1 + t602 * t5 * t578 * t581 / 0.8D1
      rrgg2gght7s1e0 = t489 + t607

      end function



      doubleprecision function rrgg2gght7s1em1
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = x4 * pi
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
      t35 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t38 = lh * t5
      t39 = pi * t7
      t41 = 0.180D3 * t38 * t39
      t46 = 0.1D1 / x3
      t52 = log(0.4D1 * t13 * t10)
      t61 = t52 ** 2
      t63 = lh ** 2
      t65 = pi ** 2
      t71 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t75 = -t7 * t26 - t7
      t77 = 0.1D1 / x2
      t81 = x2 ** 2
      t82 = t81 * t10
      t85 = log(0.4D1 * t82 * t13)
      t93 = 0.1D1 / x1
      t98 = x1 ** 2
      t99 = t98 * t10
      t102 = log(0.4D1 * t99 * t13)
      t115 = (-0.90D2 * t6 * t7 * (-t19 * t26 - t30) + (-0.90D2 * t6 * t
     #35 + t41) * (t26 + 0.1D1)) * t46 / 0.2880D4 + (0.180D3 * lh + 0.90
     #D2 * t52) * t5 * pi * t35 / 0.2880D4 + (-0.180D3 * t52 * lh - 0.45
     #D2 * t61 - 0.180D3 * t63 + 0.30D2 * t65) * t5 * t39 / 0.2880D4 - t
     #6 * t71 / 0.32D2 + t6 * t75 * t46 * t77 / 0.16D2 - (-0.90D2 * t6 *
     # (-t35 + t85 * t7) - t41) * t77 / 0.1440D4 - t6 * t7 * t93 * t77 /
     # 0.8D1 - (-0.90D2 * t6 * (-t35 + t102 * t7) - t41) * t93 / 0.1440D
     #4 + t6 * t75 * t46 * t93 / 0.16D2
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t115)
      t118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t115)
      t120 = t2 * x1
      t121 = -0.1D1 + x1
      t122 = t2 * t121
      t123 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t122, 0
     #.0D0, t120, 0.0D0)
      t128 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t122, 0
     #.0D0, t120, 0.0D0)
      t129 = x1 * z
      t130 = 0.1D1 - x1 + t129
      t131 = 0.1D1 / t130
      t133 = t121 ** 2
      t137 = log(0.4D1 * t99 * t13 * t131 * t133)
      t148 = x3 * x1
      t154 = Sqrt(-x3 * t130 * t14)
      t165 = t6 * t123 * t93 * t77 / 0.8D1 - (-0.90D2 * t6 * (t128 - t13
     #7 * t123) + 0.180D3 * t38 * pi * t123) * t93 / 0.1440D4 - t6 * (-t
     #123 - t123 / (-0.2D1 * t148 * z + 0.2D1 * t148 - 0.1D1 - x3 + 0.2D
     #1 * t20 * t154)) * t46 * t93 / 0.16D2
      t166 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t120, -t122, 0.0D0, t165)
      t168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t122, t120, 0.0D0, t165)
      t170 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t115)
      t173 = x2 * s * t1
      t174 = -0.1D1 + x2
      t175 = t174 * s
      t176 = t175 * t1
      t177 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t173, -t176, 0.
     #0D0, 0.0D0, 0.0D0)
      t178 = t6 * t177
      t179 = x2 * z
      t181 = 0.1D1 / (0.1D1 - x2 + t179)
      t186 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, t173, -t176, 0.
     #0D0, 0.0D0, 0.0D0)
      t190 = log(-0.4D1 * t82 * t13 * t174)
      t207 = t178 * t181 * t46 * t77 / 0.16D2 - (-0.90D2 * t6 * (t186 - 
     #t190 * t177) * t181 + 0.180D3 * t38 * pi * t177 * t181) * t77 / 0.
     #1440D4 + t178 * t181 * t93 * t77 / 0.8D1
      t208 = FJET(XB1, XB2, s, 0.0D0, t173, 0.0D0, -t176, 0.0D0, t207)
      t210 = x2 * x3
      t211 = t81 * x3
      t214 = Sqrt(x3 * t174 * t14)
      t215 = t20 * t214
      t217 = 0.2D1 * t215 * x2
      t220 = 0.1D1 / (0.1D1 - x3 + t210)
      t222 = t2 * (0.1D1 - x3 - x2 + t210 + t211 + t217) * t220
      t223 = 0.2D1 * t215
      t227 = t2 * x2 * (-0.1D1 + t210 + t223) * t220
      t228 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t227, t222, 0.
     #0D0, 0.0D0, 0.0D0)
      t237 = 0.1D1 / (-0.1D1 + t210 - t211 - t179 + t223 + 0.2D1 * z * t
     #20 * t214 * x2 + x2 - x3 - t217 + z * t81 * x3)
      t241 = t6 * t228 * t237 * t46 * t77 / 0.16D2
      t242 = FJET(XB1, XB2, s, 0.0D0, t222, 0.0D0, -t227, 0.0D0, t241)
      t247 = t228 * t237 * t46 * t77
      t250 = FJET(XB1, XB2, s, 0.0D0, -t176, 0.0D0, t173, 0.0D0, t207)
      t252 = FJET(XB1, XB2, s, 0.0D0, -t227, 0.0D0, t222, 0.0D0, t241)
      t259 = t2 * t121 * x2 * t131
      t261 = t175 * t1 * t121
      t262 = t1 ** 2
      t267 = s * t262 * x2 * x1 * t121 * t131
      t269 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t259, t261, 0.
     #0D0, t120, -t267)
      t270 = x1 * x2
      t276 = t269 / (-0.1D1 - t270 + x1 + t270 * z + x2 - t129 - t179) *
     # t93 * t77
      t278 = t6 * t130 * t276 / 0.8D1
      t279 = FJET(XB1, XB2, s, 0.0D0, -t259, t120, t261, -t267, t278)
      t281 = pi * t130
      t285 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t115)
      t287 = FJET(XB1, XB2, s, t120, t261, 0.0D0, -t259, -t267, t278)
      t292 = FJET(XB1, XB2, s, t120, -t122, 0.0D0, 0.0D0, 0.0D0, t165)
      t294 = FJET(XB1, XB2, s, t173, 0.0D0, -t176, 0.0D0, 0.0D0, t207)
      t296 = FJET(XB1, XB2, s, t222, 0.0D0, -t227, 0.0D0, 0.0D0, t241)
      t301 = FJET(XB1, XB2, s, t261, t120, -t259, 0.0D0, -t267, t278)
      t306 = FJET(XB1, XB2, s, -t122, t120, 0.0D0, 0.0D0, 0.0D0, t165)
      t308 = FJET(XB1, XB2, s, -t176, 0.0D0, t173, 0.0D0, 0.0D0, t207)
      t310 = FJET(XB1, XB2, s, -t227, 0.0D0, t222, 0.0D0, 0.0D0, t241)
      t315 = FJET(XB1, XB2, s, -t259, 0.0D0, t261, t120, -t267, t278)
      rrgg2gght7s1em1 = t116 * t115 + t118 * t115 + t166 * t165 + t168 *
     # t165 + t170 * t115 + t208 * t207 + t242 * t5 * pi * t247 / 0.16D2
     # + t250 * t207 + t252 * t5 * pi * t247 / 0.16D2 + t279 * t5 * t281
     # * t276 / 0.8D1 + t285 * t115 + t287 * t5 * t281 * t276 / 0.8D1 + 
     #t292 * t165 + t294 * t207 + t296 * t5 * pi * t247 / 0.16D2 + t301 
     #* t5 * t281 * t276 / 0.8D1 + t306 * t165 + t308 * t207 + t310 * t5
     # * pi * t247 / 0.16D2 + t315 * t5 * t281 * t276 / 0.8D1

      end function



      doubleprecision function rrgg2gght7s1em2
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t23 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t31 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t35 = z ** 2
      t37 = Sin(t8)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t35 * t38)
      t48 = -t6 * t7 * (0.1D1 / (-0.1D1 + 0.2D1 * t9 * t12 - x3) + 0.1D1
     #) / x3 / 0.32D2 - t6 * t7 * t23 / 0.16D2 - t6 * t7 * t27 / 0.16D2 
     #- t6 * t31 / 0.32D2 + (0.180D3 * lh + 0.90D2 * t41) * t5 * pi * t7
     # / 0.2880D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t53 = t2 * x1
      t55 = t2 * (-0.1D1 + x1)
      t56 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t55, 0.0
     #D0, t53, 0.0D0)
      t59 = t6 * t56 * t27 / 0.16D2
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t53, -t55, 0.0D0, t59)
      t63 = pi * t56 * t27
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t55, t53, 0.0D0, t59)
      t70 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t73 = x2 * s * t1
      t76 = (-0.1D1 + x2) * s * t1
      t77 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t73, -t76, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = t77 / (0.1D1 - x2 + x2 * z) * t23
      t84 = t6 * t82 / 0.16D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t73, 0.0D0, -t76, 0.0D0, t84)
      t90 = FJET(XB1, XB2, s, 0.0D0, -t76, 0.0D0, t73, 0.0D0, t84)
      t95 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t97 = FJET(XB1, XB2, s, t53, -t55, 0.0D0, 0.0D0, 0.0D0, t59)
      t101 = FJET(XB1, XB2, s, t73, 0.0D0, -t76, 0.0D0, 0.0D0, t84)
      t106 = FJET(XB1, XB2, s, -t76, 0.0D0, t73, 0.0D0, 0.0D0, t84)
      t111 = FJET(XB1, XB2, s, -t55, t53, 0.0D0, 0.0D0, 0.0D0, t59)
      rrgg2gght7s1em2 = t49 * t48 + t51 * t48 + t60 * t5 * t63 / 0.16D2 
     #+ t66 * t5 * t63 / 0.16D2 + t70 * t48 + t85 * t5 * pi * t82 / 0.16
     #D2 + t90 * t5 * pi * t82 / 0.16D2 + t95 * t48 + t97 * t5 * t63 / 0
     #.16D2 + t101 * t5 * pi * t82 / 0.16D2 + t106 * t5 * pi * t82 / 0.1
     #6D2 + t111 * t5 * t63 / 0.16D2

      end function



      doubleprecision function rrgg2gght7s1em3
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t5 * pi * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = pi * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2gght7s1em3 = -t10 * t5 * t12 / 0.32D2 - t14 * t5 * t12 / 0.32
     #D2 - t17 * t5 * t12 / 0.32D2 - t20 * t5 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2gght7s1em4
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght7s2e1
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t16 = -0.1D1 + x3
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t21 = log(-0.4D1 * t8 * t18)
      t22 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t28 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t9)
      t34 = Sqrt(-t30 * t16)
      t38 = 0.1D1 / (-t31 - 0.1D1 + 0.2D1 * t32 * t34 + x3)
      t42 = log(0.4D1 * t8 * t15)
      t44 = t42 ** 2
      t50 = lh * t5
      t59 = pi ** 2
      t61 = lh ** 2
      t63 = -0.30D2 * t59 + 0.180D3 * t61
      t64 = t63 * t5
      t70 = 0.1D1 / x3
      t72 = 0.1D1 / x1
      t75 = t7 * t11
      t76 = t75 * t14
      t78 = log(0.4D1 * t76)
      t83 = t78 ** 2
      t86 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t99 = -0.240D3 * zeta3 - 0.120D3 * t61 * lh + 0.60D2 * lh * t59
      t100 = t99 * t5
      t101 = pi * t25
      t102 = t100 * t101
      t113 = x2 ** 2
      t114 = t7 * t113
      t115 = t114 * x3
      t116 = -0.1D1 + x2
      t117 = t15 * t116
      t120 = log(-0.4D1 * t115 * t117)
      t124 = log(-0.4D1 * t115 * t18)
      t128 = t113 * x3
      t131 = log(0.4D1 * t128 * t76)
      t136 = t101 * t38
      t141 = 0.1D1 / x2
      t142 = t72 * t141
      t147 = log(-0.4D1 * t114 * t117)
      t149 = t147 ** 2
      t154 = log(0.4D1 * t114 * t15)
      t156 = t154 ** 2
      t172 = pi * t22
      t179 = x3 * t14
      t183 = log(-0.4D1 * t179 * t11 * t17)
      t187 = log(0.4D1 * t179 * t11)
      t190 = t25 * t5
      t191 = t187 ** 2
      t193 = t183 ** 2
      t224 = log(-0.4D1 * t128 * t117)
      t226 = t224 ** 2
      t231 = log(-0.4D1 * t128 * t18)
      t233 = t231 ** 2
      t240 = log(0.4D1 * t128 * t15)
      t242 = t240 ** 2
      t267 = t14 * t113
      t270 = log(0.4D1 * t267 * t11)
      t271 = t270 ** 2
      t272 = t11 * t116
      t275 = log(-0.4D1 * t267 * t272)
      t276 = t275 ** 2
      t301 = log(0.4D1 * t15)
      t302 = t301 ** 2
      t305 = t302 * t301
      t320 = t59 ** 2
      t321 = t61 ** 2
      t335 = rrgg2ggh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t337 = t302 ** 2
      t344 = -(0.90D2 * t6 * (-(t21 * t22 - t24 * t25 / 0.2D1 - t28) * t
     #38 - t42 * t22 + t44 * t25 / 0.2D1 + t28) - 0.180D3 * t50 * pi * (
     #-(-t22 + t21 * t25) * t38 + t22 - t42 * t25) + t64 * pi * (t25 + t
     #25 * t38)) * t70 * t72 / 0.1440D4 - (t64 * pi * (t22 - t78 * t25) 
     #+ 0.90D2 * t6 * (t83 * t22 / 0.2D1 + t86 - t83 * t78 * t25 / 0.6D1
     # - t78 * t28) + t102 - 0.180D3 * t50 * pi * (-t78 * t22 + t83 * t2
     #5 / 0.2D1 + t28)) * t72 / 0.1440D4 - (0.90D2 * t6 * (t120 * t25 - 
     #(-t22 + t124 * t25) * t38 - t131 * t25) - 0.180D3 * t50 * t136) * 
     #t70 * t142 / 0.720D3 + (0.90D2 * t6 * (-t147 * t22 + t149 * t25 / 
     #0.2D1 + t154 * t22 - t156 * t25 / 0.2D1) - 0.180D3 * t50 * pi * (-
     #t147 * t25 + t154 * t25)) * t72 * t141 / 0.720D3 + ((0.180D3 * t50
     # * t172 - t64 * t101 - 0.90D2 * t6 * t28) * (-t183 * t38 - t187) -
     # 0.90D2 * t190 * pi * (-t191 * t187 / 0.6D1 - t193 * t183 * t38 / 
     #0.6D1) + (-t64 * t172 - 0.90D2 * t6 * t86 - t102 + 0.180D3 * t50 *
     # pi * t28) * (t38 + 0.1D1) + (-0.90D2 * t6 * t22 + 0.180D3 * t50 *
     # t101) * (t193 * t38 / 0.2D1 + t191 / 0.2D1)) * t70 / 0.2880D4 + (
     #0.90D2 * t6 * (-t224 * t22 + t226 * t25 / 0.2D1 + (t231 * t22 - t2
     #33 * t25 / 0.2D1 - t28) * t38 + t240 * t22 - t242 * t25 / 0.2D1) -
     # 0.180D3 * t50 * pi * (-t224 * t25 + (-t22 + t231 * t25) * t38 + t
     #240 * t25) - t64 * t136) * t70 * t141 / 0.1440D4 + ((0.180D3 * t25
     # * lh - 0.90D2 * t22) * t5 * pi * (t271 / 0.2D1 - t276 / 0.2D1) - 
     #0.90D2 * t190 * pi * (t276 * t275 / 0.6D1 - t271 * t270 / 0.6D1) +
     # (0.180D3 * t22 * lh - 0.90D2 * t28 - t25 * t63) * t5 * pi * (-t27
     #0 + t275)) * t141 / 0.1440D4 + (-0.180D3 * (-t302 * t22 / 0.2D1 - 
     #t86 + t305 * t25 / 0.6D1 + t301 * t28) * lh + (t301 * t22 - t302 *
     # t25 / 0.2D1 - t28) * t63 + (-t22 + t301 * t25) * t99 - t25 * (t32
     #0 + 0.60D2 * t321 + 0.480D3 * lh * zeta3 - 0.60D2 * t61 * t59) + 0
     #.15D2 * t305 * t22 - 0.45D2 * t302 * t28 + 0.90D2 * t301 * t86 - 0
     #.90D2 * t335 - 0.15D2 / 0.4D1 * t337 * t25) * t5 * pi / 0.2880D4
      t345 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t344)
      t347 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t344)
      t349 = t2 * x1
      t350 = -0.1D1 + x1
      t351 = t2 * t350
      t352 = t8 * t11
      t353 = 0.1D1 / t12
      t354 = t350 ** 2
      t355 = t353 * t354
      t356 = x1 * z
      t357 = -z - x1 + t356
      t358 = 0.1D1 / t357
      t359 = t355 * t358
      t362 = log(-0.4D1 * t352 * t359)
      t363 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t349, 0.
     #0D0, -t351, 0.0D0)
      t365 = t362 ** 2
      t366 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t349, 0.
     #0D0, -t351, 0.0D0)
      t369 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t349, 0.
     #0D0, -t351, 0.0D0)
      t371 = t355 * t358 * t17
      t374 = log(0.4D1 * t352 * t371)
      t376 = t374 ** 2
      t380 = x3 * x1
      t381 = t380 * z
      t384 = x3 * t357
      t386 = Sqrt(t384 * t16)
      t390 = 0.1D1 / (0.2D1 * t381 - 0.2D1 * t380 - t31 + 0.2D1 * t32 * 
     #t386 - 0.1D1 + x3)
      t405 = pi * (-t366 * t390 - t366)
      t410 = (0.90D2 * t6 * (t362 * t363 - t365 * t366 / 0.2D1 - t369 - 
     #(-t374 * t363 + t376 * t366 / 0.2D1 + t369) * t390) - 0.180D3 * t5
     #0 * pi * (-t363 + t362 * t366 - (t363 - t374 * t366) * t390) + t64
     # * t405) * t70 * t72 / 0.1440D4
      t413 = log(-0.4D1 * t75 * t359)
      t415 = -t363 + t413 * t366
      t418 = t413 ** 2
      t421 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t349, 0.
     #0D0, -t351, 0.0D0)
      t426 = -t418 * t363 / 0.2D1 - t421 + t418 * t413 * t366 / 0.6D1 + 
     #t413 * t369
      t429 = pi * t366
      t430 = t100 * t429
      t434 = t413 * t363 - t418 * t366 / 0.2D1 - t369
      t442 = t354 * t358
      t446 = log(-0.4D1 * t115 * t11 * t353 * t442)
      t448 = t128 * t75
      t451 = log(0.4D1 * t448 * t371)
      t463 = (0.90D2 * t6 * (-t363 + t446 * t366 - (t363 - t451 * t366) 
     #* t390) - 0.180D3 * t50 * t405) * t70 * t142 / 0.720D3
      t464 = t114 * t11
      t467 = log(-0.4D1 * t464 * t359)
      t469 = t467 ** 2
      t484 = (0.90D2 * t6 * (-t467 * t363 + t469 * t366 / 0.2D1 + t369) 
     #- 0.180D3 * t50 * pi * (t363 - t467 * t366) + t64 * t429) * t72 * 
     #t141 / 0.720D3
      t485 = -t410 - (t64 * pi * t415 + 0.90D2 * t6 * t426 - t430 - 0.18
     #0D3 * t50 * pi * t434) * t72 / 0.1440D4 - t463 + t484
      t486 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t349, -t351, 0.0D0, t485)
      t488 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t351, t349, 0.0D0, t485)
      t490 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t344)
      t492 = x2 * x3
      t493 = 0.1D1 - x3 + t492
      t494 = 0.1D1 / t493
      t495 = t492 * t494
      t496 = t2 * t495
      t498 = t2 * t16 * t494
      t500 = t493 ** 2
      t501 = 0.1D1 / t500
      t502 = t16 * t501
      t506 = log(0.4D1 * t128 * t14 * t272 * t502)
      t507 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #496, -t498, 0.0D0)
      t509 = t506 ** 2
      t510 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #496, -t498, 0.0D0)
      t513 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #496, -t498, 0.0D0)
      t515 = t492 * z
      t516 = t116 * t16
      t518 = Sqrt(t30 * t516)
      t522 = 0.1D1 / (-t31 + t515 - 0.1D1 + 0.2D1 * t32 * t518 + x3)
      t533 = pi * t510 * t522
      t543 = log(0.4D1 * t448 * t14 * t116 * t502)
      t555 = (0.90D2 * t6 * (-t506 * t507 + t509 * t510 / 0.2D1 + t513) 
     #* t522 - 0.180D3 * t50 * pi * (t507 - t506 * t510) * t522 + t64 * 
     #t533) * t70 * t141 / 0.1440D4 - (-0.90D2 * t6 * (t507 - t543 * t51
     #0) * t522 + 0.180D3 * t50 * t533) * t70 * t142 / 0.720D3
      t556 = FJET(XB1, XB2, s, 0.0D0, t496, 0.0D0, -t498, 0.0D0, t555)
      t558 = FJET(XB1, XB2, s, 0.0D0, -t498, 0.0D0, t496, 0.0D0, t555)
      t560 = x1 * x2
      t562 = t2 * t560 * t358
      t565 = t116 * s * t1 * x1
      t566 = t1 ** 2
      t571 = s * t566 * x2 * x1 * t350 * t358
      t572 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t562, -t565, 0
     #.0D0, -t351, t571)
      t573 = t357 * t572
      t575 = t355 * t358 * t116
      t578 = log(0.4D1 * t448 * t575)
      t580 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t562, -t565, 0
     #.0D0, -t351, t571)
      t583 = t560 * z
      t585 = 0.1D1 / (z - t560 + x1 + t583 - t356)
      t589 = t50 * pi
      t591 = t357 * t580 * t585
      t599 = log(0.4D1 * t464 * t575)
      t600 = t599 * t357
      t602 = t599 ** 2
      t606 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t562, -t565, 0
     #.0D0, -t351, t571)
      t624 = -(0.90D2 * t6 * (-t573 + t578 * t357 * t580) * t585 + 0.180
     #D3 * t589 * t591) * t70 * t142 / 0.720D3 + (-0.90D2 * t6 * (t600 *
     # t572 - t602 * t357 * t580 / 0.2D1 - t357 * t606) * t585 + 0.180D3
     # * t50 * pi * (-t573 + t600 * t580) * t585 + t64 * pi * t591) * t7
     #2 * t141 / 0.720D3
      t625 = FJET(XB1, XB2, s, 0.0D0, -t562, -t351, -t565, t571, t624)
      t627 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t344)
      t642 = -t410 - (t64 * pi * t415 + 0.90D2 * t6 * t426 - t430 - 0.18
     #0D3 * t50 * pi * t434) * t72 / 0.1440D4 - t463 + t484
      t643 = FJET(XB1, XB2, s, t349, -t351, 0.0D0, 0.0D0, 0.0D0, t642)
      t645 = FJET(XB1, XB2, s, t496, 0.0D0, -t498, 0.0D0, 0.0D0, t555)
      t650 = t16 * s * t1 * t350 * t494
      t651 = x2 * z
      t652 = t380 * x2
      t653 = t380 * t651
      t655 = Sqrt(-t384 * t516)
      t656 = t32 * t655
      t657 = t656 * x2
      t659 = z + x1 - t356 - t651 - t560 + t583 - t30 - t380 + t381 + t5
     #15 + t652 - t653 + t128 + 0.2D1 * t657
      t662 = t349 * t659 * t358 * t494
      t663 = t351 * t495
      t669 = t349 * x2 * (-t30 - t380 + t381 + t515 + t652 - t653 - 0.1D
     #1 + x3 + 0.2D1 * t656) * t358 * t494
      t670 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, t669, -t662, -t
     #663, t650, t571)
      t678 = log(-0.4D1 * t128 * t75 * t353 * t442 * t516 * t501)
      t680 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t669, -t662, -t
     #663, t650, t571)
      t683 = x1 * t32
      t696 = t7 * t12
      t700 = -t652 - z + 0.2D1 * t683 * t655 + 0.2D1 * z * t32 * t655 + 
     #t492 * t12 - t115 + 0.4D1 * t380 * t12 + 0.4D1 * t8 * z + 0.2D1 * 
     #t8 * x2 - 0.2D1 * t696 * x3 + t380 + t30 + t356 - t583 - 0.2D1 * t
     #8
      t701 = x3 * t12
      t725 = -0.2D1 * t701 - 0.5D1 * t381 - 0.4D1 * t8 * t651 + 0.2D1 * 
     #t114 * t30 + x1 * t113 * t701 - t114 * t701 - t356 * t128 + 0.2D1 
     #* t696 * t492 - 0.2D1 * t356 * t656 - 0.2D1 * t683 * t655 * x2 + 0
     #.4D1 * t653 - 0.3D1 * t380 * x2 * t12 + t560 - x1 + 0.2D1 * t356 *
     # t657
      t727 = 0.1D1 / (t700 + t725)
      t735 = 0.90D2 * t6 * (-t357 * t670 + t678 * t357 * t680) * t727 + 
     #0.180D3 * t589 * t357 * t680 * t727
      t738 = t735 * t70 * t142 / 0.720D3
      t739 = FJET(XB1, XB2, s, t650, -t662, -t663, t669, t571, -t738)
      t742 = t70 * t72 * t141
      t745 = FJET(XB1, XB2, s, t669, -t663, -t662, t650, t571, -t738)
      t749 = FJET(XB1, XB2, s, -t351, t349, 0.0D0, 0.0D0, 0.0D0, t642)
      t751 = FJET(XB1, XB2, s, -t351, -t565, 0.0D0, -t562, t571, t624)
      t753 = FJET(XB1, XB2, s, -t498, 0.0D0, t496, 0.0D0, 0.0D0, t555)
      t755 = FJET(XB1, XB2, s, -t565, -t351, -t562, 0.0D0, t571, t624)
      t757 = FJET(XB1, XB2, s, -t562, 0.0D0, -t565, -t351, t571, t624)
      t759 = FJET(XB1, XB2, s, -t662, t650, t669, -t663, t571, -t738)
      t763 = FJET(XB1, XB2, s, -t663, t669, t650, -t662, t571, -t738)
      rrgg2gght7s2e1 = t345 * t344 + t347 * t344 + t486 * t485 + t488 * 
     #t485 + t490 * t344 + t556 * t555 + t558 * t555 + t625 * t624 + t62
     #7 * t344 + t643 * t642 + t645 * t555 - t739 * t735 * t742 / 0.720D
     #3 - t745 * t735 * t742 / 0.720D3 + t749 * t642 + t751 * t624 + t75
     #3 * t555 + t755 * t624 + t757 * t624 - t759 * t735 * t742 / 0.720D
     #3 - t763 * t735 * t742 / 0.720D3

      end function



      doubleprecision function rrgg2gght7s2e0
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t23 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t26 = x3 * z
      t27 = 0.2D1 * t26
      t28 = cos(t10)
      t30 = Sqrt(-t26 * t17)
      t34 = 0.1D1 / (-t27 - 0.1D1 + 0.2D1 * t28 * t30 + x3)
      t38 = log(0.4D1 * t9 * t16)
      t43 = lh * t5
      t50 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t57 = 0.1D1 / x2
      t58 = t52 * t57
      t62 = x2 ** 2
      t63 = t62 * t8
      t64 = -0.1D1 + x2
      t65 = t16 * t64
      t68 = log(-0.4D1 * t63 * t65)
      t72 = log(0.4D1 * t63 * t16)
      t79 = t8 * t12
      t82 = log(0.4D1 * t79 * t15)
      t84 = t82 ** 2
      t87 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t96 = pi ** 2
      t98 = lh ** 2
      t100 = -0.30D2 * t96 + 0.180D3 * t98
      t101 = t100 * t5
      t102 = pi * t23
      t103 = t101 * t102
      t107 = t23 * t5
      t108 = x3 * t15
      t112 = log(-0.4D1 * t108 * t12 * t18)
      t113 = t112 ** 2
      t117 = log(0.4D1 * t108 * t12)
      t118 = t117 ** 2
      t144 = log(0.4D1 * t16)
      t146 = t144 ** 2
      t161 = rrgg2ggh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t175 = t62 * x3
      t178 = log(-0.4D1 * t175 * t65)
      t182 = log(-0.4D1 * t175 * t19)
      t188 = log(0.4D1 * t175 * t16)
      t200 = t15 * t62
      t203 = log(0.4D1 * t200 * t12)
      t204 = t203 ** 2
      t205 = t12 * t64
      t208 = log(-0.4D1 * t200 * t205)
      t209 = t208 ** 2
      t226 = -(0.90D2 * t6 * (-(-t7 + t22 * t23) * t34 + t7 - t38 * t23)
     # - 0.180D3 * t43 * pi * (t23 + t23 * t34)) * t50 * t52 / 0.1440D4 
     #- t6 * t23 * t34 * t50 * t58 / 0.8D1 + t6 * (-t68 * t23 + t72 * t2
     #3) * t52 * t57 / 0.8D1 - (0.90D2 * t6 * (-t82 * t7 + t84 * t23 / 0
     #.2D1 + t87) - 0.180D3 * t43 * pi * (t7 - t82 * t23) + t103) * t52 
     #/ 0.1440D4 + (-0.90D2 * t107 * pi * (t113 * t34 / 0.2D1 + t118 / 0
     #.2D1) + (-0.90D2 * t6 * t7 + 0.180D3 * t43 * t102) * (-t112 * t34 
     #- t117) + (0.180D3 * t43 * pi * t7 - t103 - 0.90D2 * t6 * t87) * (
     #t34 + 0.1D1)) * t50 / 0.2880D4 + (-0.180D3 * (t144 * t7 - t146 * t
     #23 / 0.2D1 - t87) * lh - t23 * (-0.240D3 * zeta3 - 0.120D3 * t98 *
     # lh + 0.60D2 * lh * t96) - 0.45D2 * t146 * t7 - 0.90D2 * t161 + 0.
     #15D2 * t146 * t144 * t23 + 0.90D2 * t144 * t87 + (-t7 + t144 * t23
     #) * t100) * t5 * pi / 0.2880D4 + (0.90D2 * t6 * (-t178 * t23 + (-t
     #7 + t182 * t23) * t34 + t188 * t23) + 0.180D3 * t43 * t102 * t34) 
     #* t50 * t57 / 0.1440D4 + (-0.90D2 * t107 * pi * (t204 / 0.2D1 - t2
     #09 / 0.2D1) + (0.180D3 * t23 * lh - 0.90D2 * t7) * t5 * pi * (-t20
     #3 + t208)) * t57 / 0.1440D4
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t226)
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t226)
      t231 = t2 * x1
      t232 = -0.1D1 + x1
      t233 = t2 * t232
      t234 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t231, 0.
     #0D0, -t233, 0.0D0)
      t235 = t9 * t12
      t236 = 0.1D1 / t13
      t237 = t232 ** 2
      t238 = t236 * t237
      t239 = x1 * z
      t240 = -z - x1 + t239
      t241 = 0.1D1 / t240
      t242 = t238 * t241
      t245 = log(-0.4D1 * t235 * t242)
      t246 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t231, 0.
     #0D0, -t233, 0.0D0)
      t252 = log(0.4D1 * t235 * t238 * t241 * t18)
      t255 = x3 * x1
      t256 = t255 * z
      t259 = x3 * t240
      t261 = Sqrt(t259 * t17)
      t265 = 0.1D1 / (0.2D1 * t256 - 0.2D1 * t255 - t27 + 0.2D1 * t28 * 
     #t261 - 0.1D1 + x3)
      t271 = -t246 * t265 - t246
      t278 = (0.90D2 * t6 * (-t234 + t245 * t246 - (t234 - t252 * t246) 
     #* t265) - 0.180D3 * t43 * pi * t271) * t50 * t52 / 0.1440D4
      t281 = t50 * t52 * t57
      t283 = t6 * t271 * t281 / 0.8D1
      t284 = t63 * t12
      t287 = log(-0.4D1 * t284 * t242)
      t292 = pi * t246
      t298 = (0.90D2 * t6 * (t234 - t287 * t246) - 0.180D3 * t43 * t292)
     # * t52 * t57 / 0.720D3
      t301 = log(-0.4D1 * t79 * t242)
      t303 = t301 ** 2
      t306 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t231, 0.
     #0D0, -t233, 0.0D0)
      t307 = t301 * t234 - t303 * t246 / 0.2D1 - t306
      t311 = -t234 + t301 * t246
      t315 = t101 * t292
      t319 = -t278 - t283 + t298 - (0.90D2 * t6 * t307 - 0.180D3 * t43 *
     # pi * t311 - t315) * t52 / 0.1440D4
      t320 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t231, -t233, 0.0D0, t319)
      t322 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t233, t231, 0.0D0, t319)
      t324 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t226)
      t326 = x2 * x3
      t327 = 0.1D1 - x3 + t326
      t328 = 0.1D1 / t327
      t329 = t326 * t328
      t330 = t2 * t329
      t332 = t2 * t17 * t328
      t333 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #330, -t332, 0.0D0)
      t335 = t326 * z
      t336 = t64 * t17
      t338 = Sqrt(t26 * t336)
      t342 = 0.1D1 / (-t27 + t335 - 0.1D1 + 0.2D1 * t28 * t338 + x3)
      t347 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #330, -t332, 0.0D0)
      t349 = t327 ** 2
      t355 = log(0.4D1 * t175 * t15 * t205 * t17 / t349)
      t369 = t6 * t333 * t342 * t50 * t58 / 0.8D1 + (0.90D2 * t6 * (t347
     # - t355 * t333) * t342 - 0.180D3 * t43 * pi * t333 * t342) * t50 *
     # t57 / 0.1440D4
      t370 = FJET(XB1, XB2, s, 0.0D0, t330, 0.0D0, -t332, 0.0D0, t369)
      t372 = FJET(XB1, XB2, s, 0.0D0, -t332, 0.0D0, t330, 0.0D0, t369)
      t374 = x1 * x2
      t376 = t2 * t374 * t241
      t379 = t64 * s * t1 * x1
      t380 = t1 ** 2
      t385 = s * t380 * x2 * x1 * t232 * t241
      t386 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t376, -t379, 0
     #.0D0, -t233, t385)
      t387 = t240 * t386
      t389 = t374 * z
      t391 = 0.1D1 / (z - t374 + x1 + t389 - t239)
      t396 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t376, -t379, 0
     #.0D0, -t233, t385)
      t403 = log(0.4D1 * t284 * t236 * t241 * t237 * t64)
      t418 = t6 * t387 * t391 * t50 * t58 / 0.8D1 + (-0.90D2 * t6 * (-t2
     #40 * t396 + t403 * t240 * t386) * t391 - 0.180D3 * t43 * pi * t387
     # * t391) * t52 * t57 / 0.720D3
      t419 = FJET(XB1, XB2, s, 0.0D0, -t376, -t233, -t379, t385, t418)
      t421 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t226)
      t433 = -t278 - t283 + t298 - (0.90D2 * t6 * t307 - 0.180D3 * t43 *
     # pi * t311 - t315) * t52 / 0.1440D4
      t434 = FJET(XB1, XB2, s, t231, -t233, 0.0D0, 0.0D0, 0.0D0, t433)
      t436 = FJET(XB1, XB2, s, t330, 0.0D0, -t332, 0.0D0, 0.0D0, t369)
      t441 = t17 * s * t1 * t232 * t328
      t442 = x2 * z
      t443 = t255 * x2
      t444 = t255 * t442
      t446 = Sqrt(-t259 * t336)
      t447 = t28 * t446
      t448 = t447 * x2
      t450 = z + x1 - t239 - t442 - t374 + t389 - t26 - t255 + t256 + t3
     #35 + t443 - t444 + t175 + 0.2D1 * t448
      t453 = t231 * t450 * t241 * t328
      t454 = t233 * t329
      t460 = t231 * x2 * (-t26 - t255 + t256 + t335 + t443 - t444 - 0.1D
     #1 + x3 + 0.2D1 * t447) * t241 * t328
      t461 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, t460, -t453, -t
     #454, t441, t385)
      t473 = x3 * t13
      t475 = x1 * t28
      t489 = t8 * t13
      t492 = 0.4D1 * t444 - 0.3D1 * t255 * x2 * t13 - 0.4D1 * t9 * t442 
     #+ 0.2D1 * t63 * t26 + x1 * t62 * t473 - t443 + 0.2D1 * t475 * t446
     # + 0.2D1 * z * t28 * t446 + t326 * t13 - t63 * x3 + 0.4D1 * t255 *
     # t13 + 0.4D1 * t9 * z + 0.2D1 * t9 * x2 - 0.2D1 * t489 * x3 + t239
      t507 = t255 - t389 - z - x1 + t374 + t26 - 0.2D1 * t9 - 0.2D1 * t4
     #73 - t63 * t473 - t239 * t175 + 0.2D1 * t489 * t326 - 0.2D1 * t239
     # * t447 - 0.2D1 * t475 * t446 * x2 + 0.2D1 * t239 * t448 - 0.5D1 *
     # t256
      t509 = 0.1D1 / (t492 + t507)
      t513 = t6 * t240 * t461 * t509 * t50 * t58 / 0.8D1
      t514 = FJET(XB1, XB2, s, t441, -t453, -t454, t460, t385, t513)
      t516 = pi * t240
      t519 = t461 * t509 * t281
      t522 = FJET(XB1, XB2, s, t460, -t454, -t453, t441, t385, t513)
      t527 = FJET(XB1, XB2, s, -t233, t231, 0.0D0, 0.0D0, 0.0D0, t433)
      t529 = FJET(XB1, XB2, s, -t233, -t379, 0.0D0, -t376, t385, t418)
      t531 = FJET(XB1, XB2, s, -t332, 0.0D0, t330, 0.0D0, 0.0D0, t369)
      t533 = FJET(XB1, XB2, s, -t379, -t233, -t376, 0.0D0, t385, t418)
      t535 = FJET(XB1, XB2, s, -t376, 0.0D0, -t379, -t233, t385, t418)
      t537 = FJET(XB1, XB2, s, -t453, t441, t460, -t454, t385, t513)
      t542 = FJET(XB1, XB2, s, -t454, t460, t441, -t453, t385, t513)
      rrgg2gght7s2e0 = t227 * t226 + t229 * t226 + t320 * t319 + t322 * 
     #t319 + t324 * t226 + t370 * t369 + t372 * t369 + t419 * t418 + t42
     #1 * t226 + t434 * t433 + t436 * t369 + t514 * t5 * t516 * t519 / 0
     #.8D1 + t522 * t5 * t516 * t519 / 0.8D1 + t527 * t433 + t529 * t418
     # + t531 * t369 + t533 * t418 + t535 * t418 + t537 * t5 * t516 * t5
     #19 / 0.8D1 + t542 * t5 * t516 * t519 / 0.8D1

      end function



      doubleprecision function rrgg2gght7s2em1
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

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
      t3 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t20 = log(-0.4D1 * t11 * t14 / t15)
      t21 = x3 * z
      t22 = 0.2D1 * t21
      t23 = cos(t12)
      t25 = Sqrt(-t21 * t15)
      t29 = 0.1D1 / (-t22 - 0.1D1 + 0.2D1 * t23 * t25 + x3)
      t33 = log(0.4D1 * t11 * t14)
      t38 = t6 * pi
      t39 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t42 = lh * t6
      t45 = 0.180D3 * t42 * pi * t3
      t50 = 0.1D1 / x3
      t53 = x1 ** 2
      t54 = t53 * t14
      t57 = log(0.4D1 * t54 * t10)
      t63 = 0.1D1 / x1
      t74 = 0.1D1 / x2
      t78 = x2 ** 2
      t79 = t10 * t78
      t82 = log(0.4D1 * t79 * t14)
      t83 = -0.1D1 + x2
      t87 = log(-0.4D1 * t79 * t14 * t83)
      t95 = log(0.4D1 * t10 * t14)
      t102 = t95 ** 2
      t105 = rrgg2ggh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t107 = pi ** 2
      t109 = lh ** 2
      t117 = (-0.90D2 * t7 * pi * (-t20 * t29 - t33) + (-0.90D2 * t38 * 
     #t39 + t45) * (t29 + 0.1D1)) * t50 / 0.2880D4 - (0.90D2 * t38 * (t3
     #9 - t57 * t3) - t45) * t63 / 0.1440D4 - t38 * (t3 + t3 * t29) * t5
     #0 * t63 / 0.16D2 - t38 * t3 * t29 * t50 * t74 / 0.16D2 - t7 * pi *
     # (-t82 + t87) * t74 / 0.16D2 + (-0.180D3 * (-t39 + t95 * t3) * lh 
     #+ 0.90D2 * t95 * t39 - 0.45D2 * t102 * t3 - 0.90D2 * t105 - t3 * (
     #-0.30D2 * t107 + 0.180D3 * t109)) * t6 * pi / 0.2880D4
      t118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t117)
      t120 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t117)
      t122 = t2 * x1
      t123 = -0.1D1 + x1
      t124 = t2 * t123
      t125 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t122, 0.
     #0D0, -t124, 0.0D0)
      t129 = t38 * t125 * t63 * t74 / 0.8D1
      t130 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t122, 0.
     #0D0, -t124, 0.0D0)
      t132 = x1 * z
      t133 = -z - x1 + t132
      t134 = 0.1D1 / t133
      t136 = t123 ** 2
      t140 = log(-0.4D1 * t54 / t8 * t134 * t136)
      t142 = -t130 + t140 * t125
      t147 = 0.180D3 * t42 * pi * t125
      t151 = x3 * x1
      t157 = Sqrt(x3 * t133 * t15)
      t167 = t38 * (-t125 / (0.2D1 * t151 * z - 0.2D1 * t151 - t22 + 0.2
     #D1 * t23 * t157 - 0.1D1 + x3) - t125) * t50 * t63 / 0.16D2
      t168 = t129 - (0.90D2 * t38 * t142 + t147) * t63 / 0.1440D4 - t167
      t169 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t122, -t124, 0.0D0, t168)
      t171 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t124, t122, 0.0D0, t168)
      t173 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t117)
      t175 = x2 * x3
      t177 = 0.1D1 / (0.1D1 - x3 + t175)
      t179 = t2 * t175 * t177
      t181 = t2 * t15 * t177
      t182 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #179, -t181, 0.0D0)
      t187 = Sqrt(t21 * t83 * t15)
      t191 = 0.1D1 / (-t22 + t175 * z - 0.1D1 + 0.2D1 * t23 * t187 + x3)
      t195 = t38 * t182 * t191 * t50 * t74 / 0.16D2
      t196 = FJET(XB1, XB2, s, 0.0D0, t179, 0.0D0, -t181, 0.0D0, t195)
      t201 = t182 * t191 * t50 * t74
      t204 = FJET(XB1, XB2, s, 0.0D0, -t181, 0.0D0, t179, 0.0D0, t195)
      t209 = x1 * x2
      t211 = t2 * t209 * t134
      t214 = t83 * s * t1 * x1
      t215 = t1 ** 2
      t220 = s * t215 * x2 * x1 * t123 * t134
      t222 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t211, -t214, 0
     #.0D0, -t124, t220)
      t228 = t222 / (z - t209 + x1 + t209 * z - t132) * t63 * t74
      t230 = t38 * t133 * t228 / 0.8D1
      t231 = FJET(XB1, XB2, s, 0.0D0, -t211, -t124, -t214, t220, t230)
      t233 = pi * t133
      t237 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t117)
      t245 = t129 - (0.90D2 * t38 * t142 + t147) * t63 / 0.1440D4 - t167
      t246 = FJET(XB1, XB2, s, t122, -t124, 0.0D0, 0.0D0, 0.0D0, t245)
      t248 = FJET(XB1, XB2, s, t179, 0.0D0, -t181, 0.0D0, 0.0D0, t195)
      t253 = FJET(XB1, XB2, s, -t124, t122, 0.0D0, 0.0D0, 0.0D0, t245)
      t255 = FJET(XB1, XB2, s, -t124, -t214, 0.0D0, -t211, t220, t230)
      t260 = FJET(XB1, XB2, s, -t181, 0.0D0, t179, 0.0D0, 0.0D0, t195)
      t265 = FJET(XB1, XB2, s, -t214, -t124, -t211, 0.0D0, t220, t230)
      t270 = FJET(XB1, XB2, s, -t211, 0.0D0, -t214, -t124, t220, t230)
      rrgg2gght7s2em1 = t118 * t117 + t120 * t117 + t169 * t168 + t171 *
     # t168 + t173 * t117 + t196 * t6 * pi * t201 / 0.16D2 + t204 * t6 *
     # pi * t201 / 0.16D2 + t231 * t6 * t233 * t228 / 0.8D1 + t237 * t11
     #7 + t245 * t246 + t248 * t6 * pi * t201 / 0.16D2 + t253 * t245 + t
     #255 * t6 * t233 * t228 / 0.8D1 + t260 * t6 * pi * t201 / 0.16D2 + 
     #t265 * t6 * t233 * t228 / 0.8D1 + t270 * t6 * t233 * t228 / 0.8D1

      end function



      doubleprecision function rrgg2gght7s2em2
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = 0.1D1 / x1
      t12 = x3 * z
      t14 = x4 * pi
      t15 = cos(t14)
      t18 = Sqrt(-t12 * (-0.1D1 + x3))
      t31 = rrgg2ggh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t33 = z ** 2
      t36 = Sin(t14)
      t37 = t36 ** 2
      t40 = log(0.4D1 / t33 / z * t37)
      t47 = -t7 * pi * t8 / 0.16D2 - t7 * pi * (0.1D1 / (-0.2D1 * t12 - 
     #0.1D1 + 0.2D1 * t15 * t18 + x3) + 0.1D1) / x3 / 0.32D2 + (0.180D3 
     #* t3 * lh - 0.90D2 * t31 + 0.90D2 * t40 * t3) * t6 * pi / 0.2880D4
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t47)
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t47)
      t52 = t2 * x1
      t54 = t2 * (-0.1D1 + x1)
      t56 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t52, 0.0D
     #0, -t54, 0.0D0)
      t59 = t6 * pi * t56 * t8 / 0.16D2
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t52, -t54, 0.0D0, t59)
      t63 = pi * t56 * t8
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t54, t52, 0.0D0, t59)
      t70 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t47)
      t72 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t47)
      t74 = FJET(XB1, XB2, s, t52, -t54, 0.0D0, 0.0D0, 0.0D0, t59)
      t78 = FJET(XB1, XB2, s, -t54, t52, 0.0D0, 0.0D0, 0.0D0, t59)
      rrgg2gght7s2em2 = t48 * t47 + t50 * t47 + t60 * t6 * t63 / 0.16D2 
     #+ t66 * t6 * t63 / 0.16D2 + t70 * t47 + t72 * t47 + t74 * t6 * t63
     # / 0.16D2 + t78 * t6 * t63 / 0.16D2

      end function



      doubleprecision function rrgg2gght7s2em3
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t9 = t3 * t6 * pi / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t6 * pi
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2gght7s2em3 = -t10 * t3 * t12 / 0.32D2 - t14 * t3 * t12 / 0.32
     #D2 - t17 * t3 * t12 / 0.32D2 - t20 * t3 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2gght7s2em4
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
      doubleprecision rrgg2ggh71J1
      doubleprecision rrgg2ggh71J2
      doubleprecision rrgg2ggh71J3
      doubleprecision rrgg2ggh71J4
      doubleprecision rrgg2ggh71J5
      doubleprecision rrgg2ggh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght7s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh71J1
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
      t9 = 0.27D2 / 0.2D1 * S24
      t12 = 0.27D2 / 0.2D1 * S14
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S14
      t27 = 0.9D1 / 0.2D1 * S24
      t29 = S34 ** 2
      t37 = S24 ** 2
      t39 = S14 ** 2
      t42 = 0.9D1 / 0.2D1 * S14 * S24
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2ggh71J1 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 + 0.1
     #8D2 * S23 + 0.18D2 * S24) * S12 + (-0.27D2 / 0.2D1 * S23 - t9 - 0.
     #27D2 * S34 - 0.27D2 / 0.2D1 * S13 - t12) * S34 + (t9 + t15) * t17 
     #+ (t19 + t12) * t21 + ((0.9D1 / 0.2D1 * S13 + t24 + 0.9D1 * S34 + 
     #0.9D1 / 0.2D1 * S23 + t27) * t29 + ((t24 - t19) * t17 + (t27 - t15
     #) * t21) * S34 + (0.9D1 / 0.2D1 * t37 + 0.9D1 * t39 - t42) * t17 +
     # (0.9D1 * t37 - t42 + 0.9D1 / 0.2D1 * t39) * t21) * t50) * s * z +
     # 0.9D1 * t55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29 - 0.1
     #8D2 * t29 * S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh71J2
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
      t8 = S23 + S24 + S34
      t11 = S13 + S14 + S34
      t15 = 0.9D1 / 0.2D1 * S14
      t18 = 0.9D1 / 0.2D1 * S24
      t20 = S34 ** 2
      t30 = S24 ** 2
      t32 = S14 ** 2
      t35 = 0.9D1 / 0.2D1 * S14 * S24
      t43 = 0.1D1 / S12
      t48 = S12 ** 2
      t57 = t20 ** 2
      rrgg2ggh71J2 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 + 0.1
     #8D2 * S23 + 0.18D2 * S24) * S12 + 0.9D1 * t8 * S24 + 0.9D1 * t11 *
     # S14 + ((0.9D1 / 0.2D1 * S13 + t15 + 0.9D1 * S34 + 0.9D1 / 0.2D1 *
     # S23 + t18) * t20 + ((t15 - 0.9D1 * S24) * t11 + (t18 - 0.9D1 * S1
     #4) * t8) * S34 + (0.9D1 / 0.2D1 * t30 + 0.9D1 * t32 - t35) * t11 +
     # (0.9D1 * t30 - t35 + 0.9D1 / 0.2D1 * t32) * t8) * t43) * s * z + 
     #0.9D1 * t48 * S12 - 0.18D2 * t48 * S34 + 0.27D2 * S12 * t20 - 0.18
     #D2 * t20 * S34 + 0.9D1 * t57 * t43) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh71J3
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
      t9 = 0.27D2 / 0.2D1 * S24
      t12 = 0.27D2 / 0.2D1 * S14
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S14
      t27 = 0.9D1 / 0.2D1 * S24
      t29 = S34 ** 2
      t37 = S24 ** 2
      t39 = S14 ** 2
      t42 = 0.9D1 / 0.2D1 * S14 * S24
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2ggh71J3 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 + 0.1
     #8D2 * S23 + 0.18D2 * S24) * S12 + (0.27D2 / 0.2D1 * S23 + t9 + 0.2
     #7D2 * S34 + 0.27D2 / 0.2D1 * S13 + t12) * S34 + (-t9 + t15) * t17 
     #+ (t19 - t12) * t21 + ((0.9D1 / 0.2D1 * S13 + t24 + 0.9D1 * S34 + 
     #0.9D1 / 0.2D1 * S23 + t27) * t29 + ((t24 - t19) * t17 + (t27 - t15
     #) * t21) * S34 + (0.9D1 / 0.2D1 * t37 + 0.9D1 * t39 - t42) * t17 +
     # (0.9D1 * t37 - t42 + 0.9D1 / 0.2D1 * t39) * t21) * t50) * s * z +
     # 0.9D1 * t55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29 - 0.1
     #8D2 * t29 * S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh71J4
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
      t9 = 0.27D2 * S24
      t12 = 0.27D2 * S14
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S14
      t27 = 0.9D1 / 0.2D1 * S24
      t29 = S34 ** 2
      t37 = S24 ** 2
      t39 = S14 ** 2
      t42 = 0.9D1 / 0.2D1 * S14 * S24
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2ggh71J4 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 + 0.1
     #8D2 * S23 + 0.18D2 * S24) * S12 + (0.27D2 * S23 + t9 + 0.54D2 * S3
     #4 + 0.27D2 * S13 + t12) * S34 + (-t9 + t15) * t17 + (t19 - t12) * 
     #t21 + ((0.9D1 / 0.2D1 * S13 + t24 + 0.9D1 * S34 + 0.9D1 / 0.2D1 * 
     #S23 + t27) * t29 + ((t24 - t19) * t17 + (t27 - t15) * t21) * S34 +
     # (0.9D1 / 0.2D1 * t37 + 0.9D1 * t39 - t42) * t17 + (0.9D1 * t37 - 
     #t42 + 0.9D1 / 0.2D1 * t39) * t21) * t50) * s * z + 0.9D1 * t55 * S
     #12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29 - 0.18D2 * t29 * S34 
     #+ 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh71J5
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
      t9 = 0.81D2 / 0.2D1 * S24
      t12 = 0.81D2 / 0.2D1 * S14
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S14
      t27 = 0.9D1 / 0.2D1 * S24
      t29 = S34 ** 2
      t37 = S24 ** 2
      t39 = S14 ** 2
      t42 = 0.9D1 / 0.2D1 * S14 * S24
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2ggh71J5 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 + 0.1
     #8D2 * S23 + 0.18D2 * S24) * S12 + (0.81D2 / 0.2D1 * S23 + t9 + 0.8
     #1D2 * S34 + 0.81D2 / 0.2D1 * S13 + t12) * S34 + (-t9 + t15) * t17 
     #+ (t19 - t12) * t21 + ((0.9D1 / 0.2D1 * S13 + t24 + 0.9D1 * S34 + 
     #0.9D1 / 0.2D1 * S23 + t27) * t29 + ((t24 - t19) * t17 + (t27 - t15
     #) * t21) * S34 + (0.9D1 / 0.2D1 * t37 + 0.9D1 * t39 - t42) * t17 +
     # (0.9D1 * t37 - t42 + 0.9D1 / 0.2D1 * t39) * t21) * t50) * s * z +
     # 0.9D1 * t55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29 - 0.1
     #8D2 * t29 * S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh71J6
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
      t9 = 0.135D3 * S24
      t12 = 0.135D3 * S14
      t15 = 0.45D2 * S14
      t17 = S13 + S14 + S34
      t19 = 0.45D2 * S24
      t21 = S23 + S24 + S34
      t24 = 0.45D2 / 0.2D1 * S14
      t27 = 0.45D2 / 0.2D1 * S24
      t29 = S34 ** 2
      t37 = S24 ** 2
      t39 = S14 ** 2
      t42 = 0.45D2 / 0.2D1 * S14 * S24
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2ggh71J6 = (((-0.90D2 * S13 - 0.90D2 * S14 - 0.180D3 * S34 - 0
     #.90D2 * S23 - 0.90D2 * S24) * S12 + (0.135D3 * S23 + t9 + 0.270D3 
     #* S34 + 0.135D3 * S13 + t12) * S34 + (-t9 - t15) * t17 + (-t19 - t
     #12) * t21 + ((-0.45D2 / 0.2D1 * S13 - t24 - 0.45D2 * S34 - 0.45D2 
     #/ 0.2D1 * S23 - t27) * t29 + ((-t24 + t19) * t17 + (-t27 + t15) * 
     #t21) * S34 + (-0.45D2 / 0.2D1 * t37 - 0.45D2 * t39 + t42) * t17 + 
     #(-0.45D2 * t37 + t42 - 0.45D2 / 0.2D1 * t39) * t21) * t50) * s * z
     # - 0.45D2 * t55 * S12 + 0.90D2 * t55 * S34 - 0.135D3 * S12 * t29 +
     # 0.90D2 * t29 * S34 - 0.45D2 * t64 * t50) / pi * wd / z

      end function
  
 