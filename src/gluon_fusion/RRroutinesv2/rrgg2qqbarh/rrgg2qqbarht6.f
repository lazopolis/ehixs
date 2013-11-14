  
      subroutine rrgg2qqbarht6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh61J1  
      doubleprecision rrgg2qqbarh61J2  
      doubleprecision rrgg2qqbarh61J3  
      doubleprecision rrgg2qqbarh61J4  
      doubleprecision rrgg2qqbarh61J5  
      doubleprecision rrgg2qqbarh61J6  
      doubleprecision rrgg2qqbarh61J7  
      doubleprecision rrgg2qqbarht6s1e1  
      doubleprecision rrgg2qqbarht6s1e0  
      doubleprecision rrgg2qqbarht6s1em1  
      doubleprecision rrgg2qqbarht6s1em2  
      doubleprecision rrgg2qqbarht6s1em3  
      doubleprecision rrgg2qqbarht6s1em4  
      doubleprecision rrgg2qqbarht6s2e1  
      doubleprecision rrgg2qqbarht6s2e0  
      doubleprecision rrgg2qqbarht6s2em1  
      doubleprecision rrgg2qqbarht6s2em2  
      doubleprecision rrgg2qqbarht6s2em3  
      doubleprecision rrgg2qqbarht6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht6s1e1
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t17 = log(0.4D1 * t15)
      t18 = t17 ** 2
      t21 = pi ** 2
      t23 = 0.60D2 * lh * t21
      t24 = 0.240D3 * zeta3
      t25 = lh ** 2
      t27 = 0.120D3 * t25 * lh
      t28 = t18 * t17
      t30 = 0.180D3 * t25
      t31 = 0.30D2 * t21
      t32 = -t30 + t31
      t36 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t37 = t5 * t36
      t45 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t46 = t5 * t45
      t53 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t61 = -t23 + t24 + t27
      t65 = t21 ** 2
      t66 = t25 ** 2
      t70 = t18 ** 2
      t74 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t75 = t5 * t74
      t78 = x1 ** 2
      t79 = x3 * t78
      t82 = log(0.4D1 * t79 * t15)
      t84 = t82 ** 2
      t87 = -0.1D1 + x3
      t88 = 0.1D1 / t87
      t89 = t15 * t88
      t92 = log(-0.4D1 * t79 * t89)
      t94 = t92 ** 2
      t98 = cos(t12)
      t100 = Sqrt(-x3 * t87)
      t104 = 0.1D1 / (-0.1D1 + 0.2D1 * t98 * t100 - x3)
      t109 = lh * pi
      t118 = t32 * pi
      t120 = -t74 - t74 * t104
      t124 = 0.1D1 / x3
      t126 = 0.1D1 / x1
      t129 = t78 * t11
      t130 = t129 * t14
      t132 = log(0.4D1 * t130)
      t137 = t132 ** 2
      t147 = t61 * pi
      t148 = t147 * t75
      t159 = x2 ** 2
      t160 = x3 * t159
      t161 = t160 * t78
      t164 = log(-0.4D1 * t161 * t89)
      t168 = t160 * t130
      t170 = log(0.4D1 * t168)
      t176 = -t5 * t120
      t181 = 0.1D1 / x2
      t182 = t181 * t126
      t185 = t159 * t78
      t188 = log(0.4D1 * t185 * t15)
      t190 = t188 ** 2
      t201 = t118 * t75
      t211 = x3 * t11
      t214 = log(0.4D1 * t211 * t14)
      t218 = log(-0.4D1 * t211 * t14 * t88)
      t222 = t218 ** 2
      t225 = t214 ** 2
      t254 = log(0.4D1 * t160 * t15)
      t256 = t254 ** 2
      t261 = log(-0.4D1 * t160 * t89)
      t263 = t261 ** 2
      t284 = t159 * t11
      t287 = log(0.4D1 * t284 * t14)
      t292 = t287 ** 2
      t312 = -t6 * t7 / 0.32D2 + (0.90D2 * t18 * lh - t23 + t24 + t27 + 
     #0.15D2 * t28 - t17 * t32) * pi * t37 / 0.2880D4 + (-0.180D3 * t17 
     #* lh - 0.45D2 * t18 - t30 + t31) * pi * t46 / 0.2880D4 + (0.180D3 
     #* lh + 0.90D2 * t17) * pi * t5 * t53 / 0.2880D4 + (-0.30D2 * t28 *
     # lh + t18 * t32 / 0.2D1 - t17 * t61 - 0.480D3 * lh * zeta3 - t65 -
     # 0.60D2 * t66 + 0.60D2 * t25 * t21 - 0.15D2 / 0.4D1 * t70) * pi * 
     #t75 / 0.2880D4 - (-0.90D2 * t6 * (-t45 + t82 * t36 - t84 * t74 / 0
     #.2D1 - (t45 - t92 * t36 + t94 * t74 / 0.2D1) * t104) + 0.180D3 * t
     #109 * t5 * (-t36 - (t36 - t92 * t74) * t104 + t82 * t74) + t118 * 
     #t5 * t120) * t124 * t126 / 0.1440D4 + (t118 * t5 * (-t132 * t74 + 
     #t36) - 0.90D2 * t6 * (t137 * t36 / 0.2D1 + t53 - t137 * t132 * t74
     # / 0.6D1 - t132 * t45) + t148 + 0.180D3 * t109 * t5 * (t45 + t137 
     #* t74 / 0.2D1 - t132 * t36)) * t126 / 0.1440D4 + (-0.90D2 * t6 * (
     #(t36 - t164 * t74) * t104 + t36 - t170 * t74) + 0.180D3 * t109 * t
     #176) * t124 * t182 / 0.720D3 + (-0.90D2 * t6 * (-t188 * t36 + t190
     # * t74 / 0.2D1 + t45) + 0.180D3 * t109 * t5 * (-t188 * t74 + t36) 
     #+ t201) * t181 * t126 / 0.720D3 + ((-0.90D2 * t6 * t45 + 0.180D3 *
     # t109 * t37 + t201) * (-t214 - t218 * t104) - 0.90D2 * t6 * t74 * 
     #(-t222 * t218 * t104 / 0.6D1 - t225 * t214 / 0.6D1) + (t118 * t37 
     #- 0.90D2 * t6 * t53 + t148 + 0.180D3 * t109 * t46) * (0.1D1 + t104
     #) + (-0.90D2 * t6 * t36 + 0.180D3 * t109 * t75) * (t225 / 0.2D1 + 
     #t222 * t104 / 0.2D1)) * t124 / 0.2880D4 + (-0.90D2 * t6 * (t45 - t
     #254 * t36 + t256 * t74 / 0.2D1 + (t45 - t261 * t36 + t263 * t74 / 
     #0.2D1) * t104) + 0.180D3 * t109 * t5 * (-t254 * t74 + t36 + (t36 -
     # t261 * t74) * t104) + t118 * t176) * t124 * t181 / 0.1440D4 - (t1
     #18 * t5 * (t287 * t74 - t36) - 0.90D2 * t6 * (-t292 * t36 / 0.2D1 
     #- t53 + t292 * t287 * t74 / 0.6D1 + t287 * t45) - t148 + 0.180D3 *
     # t109 * t5 * (-t45 - t292 * t74 / 0.2D1 + t287 * t36)) * t181 / 0.
     #1440D4
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t312)
      t315 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t312)
      t317 = t2 * x1
      t318 = -0.1D1 + x1
      t319 = t2 * t318
      t320 = t79 * t11
      t321 = t318 ** 2
      t322 = t14 * t321
      t323 = x1 * z
      t324 = 0.1D1 - x1 + t323
      t325 = 0.1D1 / t324
      t326 = t322 * t325
      t329 = log(0.4D1 * t320 * t326)
      t330 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319
     #, 0.0D0, t317, 0.0D0)
      t332 = t329 ** 2
      t333 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319
     #, 0.0D0, t317, 0.0D0)
      t336 = x3 * x1
      t337 = t336 * z
      t340 = x3 * t324
      t342 = Sqrt(-t340 * t87)
      t346 = 0.1D1 / (-0.2D1 * t337 + 0.2D1 * t336 - 0.1D1 - x3 + 0.2D1 
     #* t98 * t342)
      t347 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319
     #, 0.0D0, t317, 0.0D0)
      t350 = t322 * t325 * t88
      t353 = log(-0.4D1 * t320 * t350)
      t354 = t353 * t346
      t356 = t353 ** 2
      t364 = t346 * t330
      t371 = t346 * t333 + t333
      t380 = log(0.4D1 * t129 * t326)
      t385 = t380 ** 2
      t388 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319
     #, 0.0D0, t317, 0.0D0)
      t396 = t5 * t333
      t408 = t321 * t325
      t412 = log(0.4D1 * t161 * t15 * t408)
      t414 = t160 * t129
      t417 = log(-0.4D1 * t414 * t350)
      t431 = t185 * t11
      t434 = log(0.4D1 * t431 * t326)
      t435 = t434 ** 2
      t452 = -(-0.90D2 * t6 * (-t329 * t330 + t332 * t333 / 0.2D1 + t346
     # * t347 - t354 * t330 + t356 * t346 * t333 / 0.2D1 + t347) + 0.180
     #D3 * t109 * t5 * (t330 - t329 * t333 + t364 - t354 * t333) + t118 
     #* t5 * t371) * t124 * t126 / 0.1440D4 + (t118 * t5 * (-t330 + t380
     # * t333) - 0.90D2 * t6 * (-t385 * t330 / 0.2D1 - t388 + t385 * t38
     #0 * t333 / 0.6D1 + t380 * t347) - t147 * t396 + 0.180D3 * t109 * t
     #5 * (-t347 + t380 * t330 - t385 * t333 / 0.2D1)) * t126 / 0.1440D4
     # + (-0.90D2 * t6 * (t412 * t333 - t364 + t417 * t346 * t333 - t330
     #) - 0.180D3 * t109 * t5 * t371) * t124 * t182 / 0.720D3 + (-0.90D2
     # * t6 * (-t435 * t333 / 0.2D1 - t347 + t434 * t330) + 0.180D3 * t1
     #09 * t5 * (t434 * t333 - t330) - t118 * t396) * t181 * t126 / 0.72
     #0D3
      t453 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t317, -t319, 0.0D0, t452)
      t455 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t319, t317, 0.0D0, t452)
      t457 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t312)
      t460 = x2 * t1 * s
      t461 = -0.1D1 + x2
      t463 = t461 * t1 * s
      t464 = t15 * t461
      t467 = log(-0.4D1 * t161 * t464)
      t468 = x2 * z
      t470 = 0.1D1 / (0.1D1 - x2 + t468)
      t472 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t460, -t463,
     # 0.0D0, 0.0D0, 0.0D0)
      t474 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, t460, -t463,
     # 0.0D0, 0.0D0, 0.0D0)
      t475 = t470 * t474
      t480 = t5 * t470 * t472
      t487 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, t460, -t463,
     # 0.0D0, 0.0D0, 0.0D0)
      t488 = t470 * t487
      t491 = log(-0.4D1 * t185 * t464)
      t492 = t491 * t470
      t494 = t491 ** 2
      t506 = t118 * t480
      t513 = log(-0.4D1 * t160 * t464)
      t514 = t513 * t470
      t516 = t513 ** 2
      t532 = t14 * t461
      t535 = log(-0.4D1 * t284 * t532)
      t536 = t535 * t470
      t541 = t535 ** 2
      t542 = t541 * t470
      t545 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, t460, -t463,
     # 0.0D0, 0.0D0, 0.0D0)
      t566 = (-0.90D2 * t6 * (t467 * t470 * t472 - t475) - 0.180D3 * t10
     #9 * t480) * t124 * t182 / 0.720D3 + (-0.90D2 * t6 * (-t488 + t492 
     #* t474 - t494 * t470 * t472 / 0.2D1) + 0.180D3 * t109 * t5 * (t492
     # * t472 - t475) - t506) * t181 * t126 / 0.720D3 + (-0.90D2 * t6 * 
     #(t514 * t474 - t488 - t516 * t470 * t472 / 0.2D1) + 0.180D3 * t109
     # * t5 * (t514 * t472 - t475) - t506) * t124 * t181 / 0.1440D4 - (t
     #118 * t5 * (t475 - t536 * t472) - 0.90D2 * t6 * (t542 * t474 / 0.2
     #D1 + t470 * t545 - t541 * t535 * t470 * t472 / 0.6D1 - t536 * t487
     #) + t147 * t480 + 0.180D3 * t109 * t5 * (t488 - t536 * t474 + t542
     # * t472 / 0.2D1)) * t181 / 0.1440D4
      t567 = FJET(XB1, XB2, s, 0.0D0, t460, 0.0D0, -t463, 0.0D0, t566)
      t569 = x2 * x3
      t572 = Sqrt(x3 * t461 * t87)
      t573 = t98 * t572
      t575 = 0.2D1 * t573 * x2
      t577 = 0.1D1 - x3 + t569
      t578 = 0.1D1 / t577
      t580 = t2 * (0.1D1 - x3 - x2 + t569 + t160 + t575) * t578
      t581 = 0.2D1 * t573
      t585 = t2 * x2 * (-0.1D1 + t569 + t581) * t578
      t586 = t577 ** 2
      t587 = 0.1D1 / t586
      t589 = t532 * t87 * t587
      t592 = log(0.4D1 * t414 * t589)
      t595 = t160 * z
      t597 = 0.1D1 / (-0.1D1 + 0.2D1 * t573 * t468 + t569 + t595 + t581 
     #+ x2 - x3 - t160 - t468 - t575)
      t599 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t585, t580,
     # 0.0D0, 0.0D0, 0.0D0)
      t601 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t585, t580,
     # 0.0D0, 0.0D0, 0.0D0)
      t602 = t597 * t601
      t607 = t5 * t597 * t599
      t614 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t585, t580,
     # 0.0D0, 0.0D0, 0.0D0)
      t619 = log(0.4D1 * t160 * t11 * t589)
      t620 = t619 ** 2
      t624 = t619 * t597
      t639 = (-0.90D2 * t6 * (t592 * t597 * t599 - t602) - 0.180D3 * t10
     #9 * t607) * t124 * t182 / 0.720D3 + (-0.90D2 * t6 * (-t597 * t614 
     #- t620 * t597 * t599 / 0.2D1 + t624 * t601) + 0.180D3 * t109 * t5 
     #* (-t602 + t624 * t599) - t118 * t607) * t124 * t181 / 0.1440D4
      t640 = FJET(XB1, XB2, s, 0.0D0, t580, 0.0D0, -t585, 0.0D0, t639)
      t642 = FJET(XB1, XB2, s, 0.0D0, -t463, 0.0D0, t460, 0.0D0, t566)
      t644 = FJET(XB1, XB2, s, 0.0D0, -t585, 0.0D0, t580, 0.0D0, t639)
      t648 = t2 * t318 * x2 * t325
      t651 = t461 * s * t1 * t318
      t652 = t1 ** 2
      t657 = s * t652 * x2 * x1 * t318 * t325
      t659 = t322 * t325 * t461
      t662 = log(-0.4D1 * t414 * t659)
      t664 = x2 * x1
      t665 = t664 * z
      t667 = 0.1D1 / (-0.1D1 - t323 + x1 - t468 - t664 + t665 + x2)
      t668 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t648, t651,
     # 0.0D0, t317, -t657)
      t669 = t667 * t668
      t671 = t324 * t667
      t672 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t648, t651,
     # 0.0D0, t317, -t657)
      t673 = t671 * t672
      t677 = t109 * t5
      t678 = t671 * t668
      t686 = log(-0.4D1 * t431 * t659)
      t687 = t686 ** 2
      t691 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t648, t651,
     # 0.0D0, t317, -t657)
      t693 = t686 * t324
      t710 = (-0.90D2 * t6 * (t662 * t324 * t669 - t673) - 0.180D3 * t67
     #7 * t678) * t124 * t182 / 0.720D3 + (-0.90D2 * t6 * (-t687 * t324 
     #* t669 / 0.2D1 - t671 * t691 + t693 * t667 * t672) + 0.180D3 * t10
     #9 * t5 * (t693 * t669 - t673) - t118 * t5 * t678) * t181 * t126 / 
     #0.720D3
      t711 = FJET(XB1, XB2, s, 0.0D0, -t648, t317, t651, -t657, t710)
      t713 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t312)
      t715 = FJET(XB1, XB2, s, t317, t651, 0.0D0, -t648, -t657, t710)
      t717 = t313 * t312 + t315 * t312 + t453 * t452 + t455 * t452 + t45
     #7 * t312 + t567 * t566 + t640 * t639 + t642 * t566 + t644 * t639 +
     # t711 * t710 + t713 * t312 + t715 * t710
      t718 = FJET(XB1, XB2, s, t317, -t319, 0.0D0, 0.0D0, 0.0D0, t452)
      t720 = FJET(XB1, XB2, s, t460, 0.0D0, -t463, 0.0D0, 0.0D0, t566)
      t722 = FJET(XB1, XB2, s, t580, 0.0D0, -t585, 0.0D0, 0.0D0, t639)
      t724 = FJET(XB1, XB2, s, t651, t317, -t648, 0.0D0, -t657, t710)
      t727 = t317 * t569 * t578
      t728 = t569 * x1
      t729 = t569 * t323
      t730 = t461 * t87
      t732 = Sqrt(t340 * t730)
      t733 = t98 * t732
      t734 = 0.2D1 * t733
      t739 = t319 * x2 * (t336 - t337 + t569 - t728 + t729 - 0.1D1 + t73
     #4) * t325 * t578
      t743 = t87 * s * t1 * x1 * t578
      t745 = 0.2D1 * t733 * x2
      t746 = 0.1D1 - x1 + t323 - x2 + t664 - t665 - x3 + t336 - t337 + t
     #569 - t728 + t729 + t160 + t745
      t749 = t319 * t746 * t325 * t578
      t756 = 0.1D1 - t595 - t734 + 0.2D1 * t79 + t468 + 0.2D1 * t733 * t
     #665 - x2 + x3 - x1 - t569 + t160 + t664 - t665 + 0.3D1 * t728 + t7
     #45 + t161 + 0.2D1 * t733 * x1
      t790 = 0.2D1 * t79 * t10 - 0.2D1 * t160 * x1 - 0.4D1 * t79 * z - 0
     #.2D1 * t79 * x2 + 0.3D1 * t337 - 0.4D1 * t729 + 0.4D1 * t79 * t468
     # + x3 * t10 * t664 + 0.3D1 * t160 * t323 - 0.2D1 * t79 * t10 * x2 
     #- t160 * t10 * x1 - 0.2D1 * t160 * t78 * z + t160 * t78 * t10 - 0.
     #2D1 * t733 * t468 - 0.2D1 * t733 * t664 - 0.2D1 * t733 * t323 + t3
     #23 - 0.3D1 * t336
      t792 = 0.1D1 / (t756 + t790)
      t793 = t324 * t792
      t794 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, t739, -t749,
     # t727, -t743, -t657)
      t800 = log(0.4D1 * t168 * t408 * t730 * t587)
      t802 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t739, -t749,
     # t727, -t743, -t657)
      t811 = -0.90D2 * t6 * (-t793 * t794 + t800 * t324 * t792 * t802) -
     # 0.180D3 * t677 * t793 * t802
      t814 = t811 * t124 * t182 / 0.720D3
      t815 = FJET(XB1, XB2, s, t727, t739, -t743, -t749, -t657, t814)
      t818 = t124 * t181 * t126
      t821 = FJET(XB1, XB2, s, t739, t727, -t749, -t743, -t657, t814)
      t825 = FJET(XB1, XB2, s, -t319, t317, 0.0D0, 0.0D0, 0.0D0, t452)
      t827 = FJET(XB1, XB2, s, -t463, 0.0D0, t460, 0.0D0, 0.0D0, t566)
      t829 = FJET(XB1, XB2, s, -t585, 0.0D0, t580, 0.0D0, 0.0D0, t639)
      t831 = FJET(XB1, XB2, s, -t648, 0.0D0, t651, t317, -t657, t710)
      t833 = FJET(XB1, XB2, s, -t743, -t749, t727, t739, -t657, t814)
      t837 = FJET(XB1, XB2, s, -t749, -t743, t739, t727, -t657, t814)
      t841 = t718 * t452 + t720 * t566 + t722 * t639 + t724 * t710 + t81
     #5 * t811 * t818 / 0.720D3 + t821 * t811 * t818 / 0.720D3 + t825 * 
     #t452 + t827 * t566 + t829 * t639 + t831 * t710 + t833 * t811 * t81
     #8 / 0.720D3 + t837 * t811 * t818 / 0.720D3
      rrgg2qqbarht6s1e1 = t717 + t841

      end function



      doubleprecision function rrgg2qqbarht6s1e0
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = t16 ** 2
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t23 = log(-0.4D1 * t10 * t13 * t19)
      t24 = t23 ** 2
      t25 = cos(t11)
      t27 = Sqrt(-x3 * t18)
      t31 = 0.1D1 / (-0.1D1 + 0.2D1 * t25 * t27 - x3)
      t38 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t41 = lh * pi
      t42 = t5 * t7
      t44 = 0.180D3 * t41 * t42
      t49 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t52 = t5 * t38
      t55 = lh ** 2
      t56 = 0.180D3 * t55
      t57 = pi ** 2
      t58 = 0.30D2 * t57
      t59 = -t56 + t58
      t60 = t59 * pi
      t61 = t60 * t42
      t66 = 0.1D1 / x3
      t69 = t9 * t13
      t71 = log(0.4D1 * t69)
      t74 = t71 ** 2
      t80 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t104 = x2 ** 2
      t105 = t104 * x3
      t108 = log(0.4D1 * t105 * t69)
      t110 = t69 * t19
      t113 = log(-0.4D1 * t105 * t110)
      t121 = t7 * t31 + t7
      t127 = 0.1D1 / x2
      t130 = t104 * t9
      t133 = log(0.4D1 * t130 * t13)
      t134 = t133 ** 2
      t149 = x1 ** 2
      t150 = x3 * t149
      t153 = log(-0.4D1 * t150 * t110)
      t159 = log(0.4D1 * t150 * t69)
      t170 = 0.1D1 / x1
      t175 = t66 * t127 * t170
      t178 = t104 * t149
      t181 = log(0.4D1 * t178 * t69)
      t190 = t149 * t9
      t193 = log(0.4D1 * t190 * t13)
      t194 = t193 ** 2
      t209 = (-0.90D2 * t6 * t7 * (t17 / 0.2D1 + t24 * t31 / 0.2D1) + (-
     #0.90D2 * t6 * t38 + t44) * (-t16 - t23 * t31) + (-0.90D2 * t6 * t4
     #9 + 0.180D3 * t41 * t52 + t61) * (0.1D1 + t31)) * t66 / 0.2880D4 +
     # (-0.180D3 * t71 * lh - 0.45D2 * t74 - t56 + t58) * pi * t52 / 0.2
     #880D4 - t6 * t80 / 0.32D2 + (0.90D2 * t74 * lh - 0.60D2 * lh * t57
     # + 0.240D3 * zeta3 + 0.120D3 * t55 * lh + 0.15D2 * t74 * t71 - t71
     # * t59) * pi * t42 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t71) * pi
     # * t5 * t49 / 0.2880D4 + (-0.90D2 * t6 * (-t108 * t7 + t38 + (t38 
     #- t113 * t7) * t31) + 0.180D3 * t41 * t5 * t121) * t66 * t127 / 0.
     #1440D4 - (-0.90D2 * t6 * (-t49 - t134 * t7 / 0.2D1 + t133 * t38) +
     # 0.180D3 * t41 * t5 * (t133 * t7 - t38) - t61) * t127 / 0.1440D4 -
     # (-0.90D2 * t6 * (-t38 - (t38 - t153 * t7) * t31 + t159 * t7) - 0.
     #180D3 * t41 * t5 * t121) * t66 * t170 / 0.1440D4 - t6 * t121 * t17
     #5 / 0.8D1 + (-0.90D2 * t6 * (-t181 * t7 + t38) + t44) * t127 * t17
     #0 / 0.720D3 + (-0.90D2 * t6 * (t49 + t194 * t7 / 0.2D1 - t193 * t3
     #8) + 0.180D3 * t41 * t5 * (-t193 * t7 + t38) + t61) * t170 / 0.144
     #0D4
      t210 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t209)
      t212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t209)
      t214 = t2 * x1
      t215 = -0.1D1 + x1
      t216 = t2 * t215
      t217 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t216
     #, 0.0D0, t214, 0.0D0)
      t218 = t150 * t9
      t219 = t215 ** 2
      t220 = t13 * t219
      t221 = x1 * z
      t222 = 0.1D1 - x1 + t221
      t223 = 0.1D1 / t222
      t224 = t220 * t223
      t227 = log(0.4D1 * t218 * t224)
      t228 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t216
     #, 0.0D0, t214, 0.0D0)
      t230 = x3 * x1
      t231 = t230 * z
      t234 = x3 * t222
      t236 = Sqrt(-t234 * t18)
      t240 = 0.1D1 / (-0.2D1 * t231 + 0.2D1 * t230 - 0.1D1 - x3 + 0.2D1 
     #* t25 * t236)
      t246 = log(-0.4D1 * t218 * t220 * t223 * t19)
      t253 = t240 * t228 + t228
      t265 = t178 * t9
      t268 = log(0.4D1 * t265 * t224)
      t273 = t5 * t228
      t280 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t216
     #, 0.0D0, t214, 0.0D0)
      t283 = log(0.4D1 * t190 * t224)
      t285 = t283 ** 2
      t300 = -(-0.90D2 * t6 * (t217 - t227 * t228 + t240 * t217 - t246 *
     # t240 * t228) + 0.180D3 * t41 * t5 * t253) * t66 * t170 / 0.1440D4
     # + t6 * t253 * t175 / 0.8D1 + (-0.90D2 * t6 * (t268 * t228 - t217)
     # - 0.180D3 * t41 * t273) * t127 * t170 / 0.720D3 + (-0.90D2 * t6 *
     # (-t280 + t283 * t217 - t285 * t228 / 0.2D1) + 0.180D3 * t41 * t5 
     #* (-t217 + t283 * t228) - t60 * t273) * t170 / 0.1440D4
      t301 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t214, -t216, 0.0D0, t300)
      t303 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t216, t214, 0.0D0, t300)
      t305 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t209)
      t308 = x2 * t1 * s
      t309 = -0.1D1 + x2
      t311 = t309 * t1 * s
      t312 = t69 * t309
      t315 = log(-0.4D1 * t105 * t312)
      t316 = x2 * z
      t318 = 0.1D1 / (0.1D1 - x2 + t316)
      t320 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t308, -t311,
     # 0.0D0, 0.0D0, 0.0D0)
      t322 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, t308, -t311,
     # 0.0D0, 0.0D0, 0.0D0)
      t323 = t318 * t322
      t328 = t5 * t318 * t320
      t330 = 0.180D3 * t41 * t328
      t335 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, t308, -t311,
     # 0.0D0, 0.0D0, 0.0D0)
      t337 = t13 * t309
      t340 = log(-0.4D1 * t130 * t337)
      t341 = t340 * t318
      t343 = t340 ** 2
      t361 = t127 * t170
      t367 = log(-0.4D1 * t178 * t312)
      t377 = (-0.90D2 * t6 * (t315 * t318 * t320 - t323) - t330) * t66 *
     # t127 / 0.1440D4 - (-0.90D2 * t6 * (t318 * t335 - t341 * t322 + t3
     #43 * t318 * t320 / 0.2D1) + 0.180D3 * t41 * t5 * (t323 - t341 * t3
     #20) + t60 * t328) * t127 / 0.1440D4 + t6 * t318 * t320 * t66 * t36
     #1 / 0.8D1 + (-0.90D2 * t6 * (t367 * t318 * t320 - t323) - t330) * 
     #t127 * t170 / 0.720D3
      t378 = FJET(XB1, XB2, s, 0.0D0, t308, 0.0D0, -t311, 0.0D0, t377)
      t380 = x2 * x3
      t383 = Sqrt(x3 * t309 * t18)
      t384 = t25 * t383
      t386 = 0.2D1 * t384 * x2
      t388 = 0.1D1 - x3 + t380
      t389 = 0.1D1 / t388
      t391 = t2 * (0.1D1 - x3 - x2 + t380 + t105 + t386) * t389
      t392 = 0.2D1 * t384
      t396 = t2 * x2 * (-0.1D1 + t380 + t392) * t389
      t399 = t105 * z
      t401 = 0.1D1 / (-0.1D1 + 0.2D1 * t384 * t316 + t380 + t399 + t392 
     #+ x2 - x3 - t105 - t316 - t386)
      t402 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t396, t391,
     # 0.0D0, 0.0D0, 0.0D0)
      t405 = t388 ** 2
      t411 = log(0.4D1 * t105 * t9 * t337 * t18 / t405)
      t413 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t396, t391,
     # 0.0D0, 0.0D0, 0.0D0)
      t431 = (-0.90D2 * t6 * (-t401 * t402 + t411 * t401 * t413) - 0.180
     #D3 * t41 * t5 * t401 * t413) * t66 * t127 / 0.1440D4 + t6 * t401 *
     # t413 * t66 * t361 / 0.8D1
      t432 = FJET(XB1, XB2, s, 0.0D0, t391, 0.0D0, -t396, 0.0D0, t431)
      t434 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t308, 0.0D0, t377)
      t436 = FJET(XB1, XB2, s, 0.0D0, -t396, 0.0D0, t391, 0.0D0, t431)
      t440 = t2 * t215 * x2 * t223
      t443 = t309 * s * t1 * t215
      t444 = t1 ** 2
      t449 = s * t444 * x2 * x1 * t215 * t223
      t450 = x2 * x1
      t451 = t450 * z
      t453 = 0.1D1 / (-0.1D1 - t221 + x1 - t316 - t450 + t451 + x2)
      t454 = t222 * t453
      t456 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t440, t443,
     # 0.0D0, t214, -t449)
      t466 = log(-0.4D1 * t265 * t13 * t223 * t219 * t309)
      t470 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t440, t443,
     # 0.0D0, t214, -t449)
      t483 = t6 * t454 * t456 * t66 * t361 / 0.8D1 + (-0.90D2 * t6 * (t4
     #66 * t222 * t453 * t456 - t454 * t470) - 0.180D3 * t41 * t5 * t454
     # * t456) * t127 * t170 / 0.720D3
      t484 = FJET(XB1, XB2, s, 0.0D0, -t440, t214, t443, -t449, t483)
      t486 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t209)
      t488 = FJET(XB1, XB2, s, t214, t443, 0.0D0, -t440, -t449, t483)
      t490 = t210 * t209 + t212 * t209 + t301 * t300 + t303 * t300 + t30
     #5 * t209 + t378 * t377 + t432 * t431 + t434 * t377 + t436 * t431 +
     # t484 * t483 + t486 * t209 + t488 * t483
      t491 = FJET(XB1, XB2, s, t214, -t216, 0.0D0, 0.0D0, 0.0D0, t300)
      t493 = FJET(XB1, XB2, s, t308, 0.0D0, -t311, 0.0D0, 0.0D0, t377)
      t495 = FJET(XB1, XB2, s, t391, 0.0D0, -t396, 0.0D0, 0.0D0, t431)
      t497 = FJET(XB1, XB2, s, t443, t214, -t440, 0.0D0, -t449, t483)
      t500 = t214 * t380 * t389
      t501 = t380 * x1
      t502 = t380 * t221
      t505 = Sqrt(t234 * t309 * t18)
      t506 = t25 * t505
      t507 = 0.2D1 * t506
      t512 = t216 * x2 * (t230 - t231 + t380 - t501 + t502 - 0.1D1 + t50
     #7) * t223 * t389
      t516 = t18 * s * t1 * x1 * t389
      t518 = 0.2D1 * t506 * x2
      t519 = 0.1D1 - x1 + t221 - x2 + t450 - t451 - x3 + t230 - t231 + t
     #380 - t501 + t502 + t105 + t518
      t522 = t216 * t519 * t223 * t389
      t542 = 0.1D1 + 0.3D1 * t501 + t518 + t105 * t149 + 0.2D1 * t506 * 
     #x1 + 0.2D1 * t150 * t8 - 0.2D1 * t105 * x1 - 0.4D1 * t150 * z - 0.
     #2D1 * t150 * x2 + 0.2D1 * t506 * t451 - x2 + x3 + t450 - t399 - 0.
     #4D1 * t502 + 0.4D1 * t150 * t316 + x3 * t8 * t450
      t564 = 0.3D1 * t105 * t221 - 0.2D1 * t150 * t8 * x2 - t105 * t8 * 
     #x1 - 0.2D1 * t105 * t149 * z + t105 * t149 * t8 - 0.2D1 * t506 * t
     #316 - 0.2D1 * t506 * t450 - 0.2D1 * t506 * t221 - t451 + t221 - 0.
     #3D1 * t230 + 0.3D1 * t231 - x1 - t507 + 0.2D1 * t150 + t316 - t380
     # + t105
      t566 = 0.1D1 / (t542 + t564)
      t569 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t512, -t522,
     # t500, -t516, -t449)
      t573 = t6 * t222 * t566 * t569 * t66 * t361 / 0.8D1
      t574 = FJET(XB1, XB2, s, t500, t512, -t516, -t522, -t449, t573)
      t576 = t5 * t222
      t579 = t566 * t569 * t175
      t582 = FJET(XB1, XB2, s, t512, t500, -t522, -t516, -t449, t573)
      t587 = FJET(XB1, XB2, s, -t216, t214, 0.0D0, 0.0D0, 0.0D0, t300)
      t589 = FJET(XB1, XB2, s, -t311, 0.0D0, t308, 0.0D0, 0.0D0, t377)
      t591 = FJET(XB1, XB2, s, -t396, 0.0D0, t391, 0.0D0, 0.0D0, t431)
      t593 = FJET(XB1, XB2, s, -t440, 0.0D0, t443, t214, -t449, t483)
      t595 = FJET(XB1, XB2, s, -t516, -t522, t500, t512, -t449, t573)
      t600 = FJET(XB1, XB2, s, -t522, -t516, t512, t500, -t449, t573)
      t605 = t491 * t300 + t493 * t377 + t495 * t431 + t497 * t483 + t57
     #4 * pi * t576 * t579 / 0.8D1 + t582 * pi * t576 * t579 / 0.8D1 + t
     #587 * t300 + t589 * t377 + t591 * t431 + t593 * t483 + t595 * pi *
     # t576 * t579 / 0.8D1 + t600 * pi * t576 * t579 / 0.8D1
      rrgg2qqbarht6s1e0 = t490 + t605

      end function



      doubleprecision function rrgg2qqbarht6s1em1
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = -0.1D1 + x3
      t22 = log(-0.4D1 * t10 * t13 / t17)
      t23 = cos(t11)
      t25 = Sqrt(-x3 * t17)
      t29 = 0.1D1 / (-0.1D1 + 0.2D1 * t23 * t25 - x3)
      t35 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t38 = lh * pi
      t39 = t5 * t7
      t41 = 0.180D3 * t38 * t39
      t46 = 0.1D1 / x3
      t49 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t55 = log(0.4D1 * t9 * t13)
      t64 = t55 ** 2
      t66 = lh ** 2
      t68 = pi ** 2
      t75 = t7 * t29 + t7
      t77 = 0.1D1 / x2
      t81 = x2 ** 2
      t82 = t81 * t9
      t85 = log(0.4D1 * t82 * t13)
      t94 = 0.1D1 / x1
      t98 = x1 ** 2
      t99 = t98 * t9
      t102 = log(0.4D1 * t99 * t13)
      t115 = (-0.90D2 * t6 * t7 * (-t16 - t22 * t29) + (-0.90D2 * t6 * t
     #35 + t41) * (0.1D1 + t29)) * t46 / 0.2880D4 - t6 * t49 / 0.32D2 + 
     #(0.180D3 * lh + 0.90D2 * t55) * pi * t5 * t35 / 0.2880D4 + (-0.180
     #D3 * t55 * lh - 0.45D2 * t64 - 0.180D3 * t66 + 0.30D2 * t68) * pi 
     #* t39 / 0.2880D4 - t6 * t75 * t46 * t77 / 0.16D2 - (-0.90D2 * t6 *
     # (t85 * t7 - t35) - t41) * t77 / 0.1440D4 - t6 * t7 * t77 * t94 / 
     #0.8D1 + (-0.90D2 * t6 * (-t102 * t7 + t35) + t41) * t94 / 0.1440D4
     # - t6 * t75 * t46 * t94 / 0.16D2
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t115)
      t118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t115)
      t120 = t2 * x1
      t121 = -0.1D1 + x1
      t122 = t2 * t121
      t123 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t122
     #, 0.0D0, t120, 0.0D0)
      t128 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t122
     #, 0.0D0, t120, 0.0D0)
      t129 = x1 * z
      t130 = 0.1D1 - x1 + t129
      t131 = 0.1D1 / t130
      t133 = t121 ** 2
      t137 = log(0.4D1 * t99 * t13 * t131 * t133)
      t148 = x3 * x1
      t154 = Sqrt(-x3 * t130 * t17)
      t165 = t6 * t123 * t77 * t94 / 0.8D1 + (-0.90D2 * t6 * (-t128 + t1
     #37 * t123) - 0.180D3 * t38 * t5 * t123) * t94 / 0.1440D4 + t6 * (0
     #.1D1 / (-0.2D1 * t148 * z + 0.2D1 * t148 - 0.1D1 - x3 + 0.2D1 * t2
     #3 * t154) * t123 + t123) * t46 * t94 / 0.16D2
      t166 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t120, -t122, 0.0D0, t165)
      t168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t122, t120, 0.0D0, t165)
      t170 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t115)
      t173 = x2 * t1 * s
      t174 = -0.1D1 + x2
      t176 = t174 * t1 * s
      t177 = x2 * z
      t179 = 0.1D1 / (0.1D1 - x2 + t177)
      t180 = t6 * t179
      t181 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t173, -t176,
     # 0.0D0, 0.0D0, 0.0D0)
      t186 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, t173, -t176,
     # 0.0D0, 0.0D0, 0.0D0)
      t191 = log(-0.4D1 * t82 * t13 * t174)
      t208 = t180 * t181 * t46 * t77 / 0.16D2 - (-0.90D2 * t6 * (t179 * 
     #t186 - t191 * t179 * t181) + 0.180D3 * t38 * t5 * t179 * t181) * t
     #77 / 0.1440D4 + t180 * t181 * t77 * t94 / 0.8D1
      t209 = FJET(XB1, XB2, s, 0.0D0, t173, 0.0D0, -t176, 0.0D0, t208)
      t211 = x2 * x3
      t212 = t81 * x3
      t215 = Sqrt(x3 * t174 * t17)
      t216 = t23 * t215
      t218 = 0.2D1 * t216 * x2
      t221 = 0.1D1 / (0.1D1 - x3 + t211)
      t223 = t2 * (0.1D1 - x3 - x2 + t211 + t212 + t218) * t221
      t224 = 0.2D1 * t216
      t228 = t2 * x2 * (-0.1D1 + t211 + t224) * t221
      t233 = 0.1D1 / (-0.1D1 + 0.2D1 * t216 * t177 + t211 + t212 * z + t
     #224 + x2 - x3 - t212 - t177 - t218)
      t235 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t228, t223,
     # 0.0D0, 0.0D0, 0.0D0)
      t239 = t6 * t233 * t235 * t46 * t77 / 0.16D2
      t240 = FJET(XB1, XB2, s, 0.0D0, t223, 0.0D0, -t228, 0.0D0, t239)
      t245 = t233 * t235 * t46 * t77
      t248 = FJET(XB1, XB2, s, 0.0D0, -t176, 0.0D0, t173, 0.0D0, t208)
      t250 = FJET(XB1, XB2, s, 0.0D0, -t228, 0.0D0, t223, 0.0D0, t239)
      t257 = t2 * t121 * x2 * t131
      t260 = t174 * s * t1 * t121
      t261 = t1 ** 2
      t266 = s * t261 * x2 * x1 * t121 * t131
      t268 = x2 * x1
      t272 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t257, t260,
     # 0.0D0, t120, -t266)
      t275 = 0.1D1 / (-0.1D1 - t129 + x1 - t177 - t268 + t268 * z + x2) 
     #* t272 * t77 * t94
      t277 = t6 * t130 * t275 / 0.8D1
      t278 = FJET(XB1, XB2, s, 0.0D0, -t257, t120, t260, -t266, t277)
      t280 = t5 * t130
      t284 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t115)
      t286 = FJET(XB1, XB2, s, t120, t260, 0.0D0, -t257, -t266, t277)
      t291 = FJET(XB1, XB2, s, t120, -t122, 0.0D0, 0.0D0, 0.0D0, t165)
      t293 = FJET(XB1, XB2, s, t173, 0.0D0, -t176, 0.0D0, 0.0D0, t208)
      t295 = FJET(XB1, XB2, s, t223, 0.0D0, -t228, 0.0D0, 0.0D0, t239)
      t300 = FJET(XB1, XB2, s, t260, t120, -t257, 0.0D0, -t266, t277)
      t305 = FJET(XB1, XB2, s, -t122, t120, 0.0D0, 0.0D0, 0.0D0, t165)
      t307 = FJET(XB1, XB2, s, -t176, 0.0D0, t173, 0.0D0, 0.0D0, t208)
      t309 = FJET(XB1, XB2, s, -t228, 0.0D0, t223, 0.0D0, 0.0D0, t239)
      t314 = FJET(XB1, XB2, s, -t257, 0.0D0, t260, t120, -t266, t277)
      rrgg2qqbarht6s1em1 = t116 * t115 + t118 * t115 + t166 * t165 + t16
     #8 * t165 + t170 * t115 + t209 * t208 + t240 * pi * t5 * t245 / 0.1
     #6D2 + t248 * t208 + t250 * pi * t5 * t245 / 0.16D2 + t278 * pi * t
     #280 * t275 / 0.8D1 + t284 * t115 + t286 * pi * t280 * t275 / 0.8D1
     # + t291 * t165 + t293 * t208 + t295 * pi * t5 * t245 / 0.16D2 + t3
     #00 * pi * t280 * t275 / 0.8D1 + t305 * t165 + t307 * t208 + t309 *
     # pi * t5 * t245 / 0.16D2 + t314 * pi * t280 * t275 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht6s1em2
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t23 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t31 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t35 = z ** 2
      t37 = Sin(t8)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t35 * t38)
      t48 = -t6 * t7 * (0.1D1 + 0.1D1 / (-0.1D1 + 0.2D1 * t9 * t12 - x3)
     #) / x3 / 0.32D2 - t6 * t7 * t23 / 0.16D2 - t6 * t7 * t27 / 0.16D2 
     #- t6 * t31 / 0.32D2 + (0.180D3 * lh + 0.90D2 * t41) * pi * t5 * t7
     # / 0.2880D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t53 = t2 * x1
      t55 = t2 * (-0.1D1 + x1)
      t56 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t55, 
     #0.0D0, t53, 0.0D0)
      t59 = t6 * t56 * t27 / 0.16D2
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t53, -t55, 0.0D0, t59)
      t63 = t5 * t56 * t27
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t55, t53, 0.0D0, t59)
      t70 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t73 = x2 * t1 * s
      t76 = (-0.1D1 + x2) * t1 * s
      t80 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t73, -t76, 0.
     #0D0, 0.0D0, 0.0D0)
      t82 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t80 * t23
      t84 = t6 * t82 / 0.16D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t73, 0.0D0, -t76, 0.0D0, t84)
      t90 = FJET(XB1, XB2, s, 0.0D0, -t76, 0.0D0, t73, 0.0D0, t84)
      t95 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t97 = FJET(XB1, XB2, s, t53, -t55, 0.0D0, 0.0D0, 0.0D0, t59)
      t101 = FJET(XB1, XB2, s, t73, 0.0D0, -t76, 0.0D0, 0.0D0, t84)
      t106 = FJET(XB1, XB2, s, -t76, 0.0D0, t73, 0.0D0, 0.0D0, t84)
      t111 = FJET(XB1, XB2, s, -t55, t53, 0.0D0, 0.0D0, 0.0D0, t59)
      rrgg2qqbarht6s1em2 = t49 * t48 + t51 * t48 + t60 * pi * t63 / 0.16
     #D2 + t66 * pi * t63 / 0.16D2 + t70 * t48 + t85 * pi * t5 * t82 / 0
     #.16D2 + t90 * pi * t5 * t82 / 0.16D2 + t95 * t48 + t97 * pi * t63 
     #/ 0.16D2 + t101 * pi * t5 * t82 / 0.16D2 + t106 * pi * t5 * t82 / 
     #0.16D2 + t111 * pi * t63 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht6s1em3
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t9 = pi * t5 * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t5 * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2qqbarht6s1em3 = -t10 * pi * t12 / 0.32D2 - t14 * pi * t12 / 0
     #.32D2 - t17 * pi * t12 / 0.32D2 - t20 * pi * t12 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht6s1em4
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht6s2e1
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t10 = lh * pi
      t11 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t12 = t5 * t11
      t15 = lh ** 2
      t16 = 0.180D3 * t15
      t17 = pi ** 2
      t18 = 0.30D2 * t17
      t19 = -t16 + t18
      t20 = t19 * pi
      t21 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t22 = t5 * t21
      t24 = -0.90D2 * t6 * t7 + 0.180D3 * t10 * t12 + t20 * t22
      t25 = z ** 2
      t27 = 0.1D1 / t25 / z
      t28 = x3 * t27
      t29 = x4 * pi
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
      t64 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t68 = 0.60D2 * lh * t17
      t69 = 0.240D3 * zeta3
      t71 = 0.120D3 * t15 * lh
      t72 = -t68 + t69 + t71
      t73 = t72 * pi
      t74 = t73 * t22
      t75 = t5 * t7
      t85 = -0.90D2 * t6 * t11 + 0.180D3 * t10 * t22
      t91 = 0.1D1 / x3
      t94 = x2 ** 2
      t95 = t94 * x3
      t96 = t27 * t31
      t99 = log(0.4D1 * t95 * t96)
      t101 = -0.1D1 + x2
      t102 = t96 * t101
      t105 = log(-0.4D1 * t95 * t102)
      t107 = t99 ** 2
      t110 = t105 ** 2
      t113 = t96 * t36
      t116 = log(-0.4D1 * t95 * t113)
      t118 = t116 ** 2
      t135 = t22 * t49
      t139 = 0.1D1 / x2
      t142 = t27 * t94
      t145 = log(0.4D1 * t142 * t31)
      t146 = t145 ** 2
      t147 = t31 * t101
      t150 = log(-0.4D1 * t142 * t147)
      t151 = t150 ** 2
      t167 = rrgg2qqbarh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t171 = log(0.4D1 * t96)
      t172 = t171 ** 2
      t175 = t172 * t171
      t196 = x1 ** 2
      t197 = x3 * t196
      t200 = log(0.4D1 * t197 * t96)
      t202 = t200 ** 2
      t207 = log(-0.4D1 * t197 * t113)
      t209 = t207 ** 2
      t231 = 0.1D1 / x1
      t234 = t196 * t27
      t235 = t234 * t31
      t237 = log(0.4D1 * t235)
      t242 = t237 ** 2
      t262 = t95 * t196
      t265 = log(-0.4D1 * t262 * t102)
      t269 = log(-0.4D1 * t262 * t113)
      t275 = log(0.4D1 * t95 * t235)
      t284 = t139 * t231
      t287 = t94 * t196
      t290 = log(0.4D1 * t287 * t96)
      t291 = t290 ** 2
      t296 = log(-0.4D1 * t287 * t102)
      t299 = t296 ** 2
      t322 = t17 ** 2
      t323 = t15 ** 2
      t327 = t172 ** 2
      t333 = (t24 * (-t34 - t40 * t49) - 0.90D2 * t6 * t21 * (-t53 * t40
     # * t49 / 0.6D1 - t56 * t34 / 0.6D1) + (t20 * t12 - 0.90D2 * t6 * t
     #64 + t74 + 0.180D3 * t10 * t75) * (0.1D1 + t49) + t85 * (t56 / 0.2
     #D1 + t53 * t49 / 0.2D1)) * t91 / 0.2880D4 + (-0.90D2 * t6 * (-t99 
     #* t11 + t105 * t11 + t107 * t21 / 0.2D1 - t110 * t21 / 0.2D1 + (t7
     # - t116 * t11 + t118 * t21 / 0.2D1) * t49) + 0.180D3 * t10 * t5 * 
     #(t105 * t21 - t99 * t21 + (t11 - t116 * t21) * t49) + t20 * t135) 
     #* t91 * t139 / 0.1440D4 + (t85 * (t146 / 0.2D1 - t151 / 0.2D1) - 0
     #.90D2 * t6 * t21 * (t151 * t150 / 0.6D1 - t146 * t145 / 0.6D1) + t
     #24 * (-t145 + t150)) * t139 / 0.1440D4 - t6 * t167 / 0.32D2 + (0.9
     #0D2 * t172 * lh - t68 + t69 + t71 + 0.15D2 * t175 - t171 * t19) * 
     #pi * t12 / 0.2880D4 + (-0.180D3 * t171 * lh - 0.45D2 * t172 - t16 
     #+ t18) * pi * t75 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t171) * pi
     # * t5 * t64 / 0.2880D4 + (-0.90D2 * t6 * (t7 - t200 * t11 + t202 *
     # t21 / 0.2D1 + (t7 - t207 * t11 + t209 * t21 / 0.2D1) * t49) + 0.1
     #80D3 * t10 * t5 * (t11 - t200 * t21 + (t11 - t207 * t21) * t49) + 
     #t20 * t5 * (t21 + t21 * t49)) * t91 * t231 / 0.1440D4 + (t20 * t5 
     #* (t11 - t237 * t21) - 0.90D2 * t6 * (t242 * t11 / 0.2D1 + t64 - t
     #242 * t237 * t21 / 0.6D1 - t237 * t7) + t74 + 0.180D3 * t10 * t5 *
     # (t7 - t237 * t11 + t242 * t21 / 0.2D1)) * t231 / 0.1440D4 + (-0.9
     #0D2 * t6 * (t265 * t21 + (t11 - t269 * t21) * t49 - t275 * t21) + 
     #0.180D3 * t10 * t135) * t91 * t284 / 0.720D3 + (-0.90D2 * t6 * (t2
     #91 * t21 / 0.2D1 + t296 * t11 - t290 * t11 - t299 * t21 / 0.2D1) +
     # 0.180D3 * t10 * t5 * (-t290 * t21 + t296 * t21)) * t139 * t231 / 
     #0.720D3 + (-0.30D2 * t175 * lh + t172 * t19 / 0.2D1 - t171 * t72 -
     # 0.480D3 * lh * zeta3 - t322 - 0.60D2 * t323 + 0.60D2 * t15 * t17 
     #- 0.15D2 / 0.4D1 * t327) * pi * t22 / 0.2880D4
      t334 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t333)
      t336 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t333)
      t338 = t2 * x1
      t339 = -0.1D1 + x1
      t340 = t2 * t339
      t341 = 0.1D1 / t25
      t342 = t197 * t341
      t343 = t339 ** 2
      t344 = t31 * t343
      t345 = x1 * z
      t346 = -z - x1 + t345
      t347 = 0.1D1 / t346
      t348 = t344 * t347
      t351 = log(-0.4D1 * t342 * t348)
      t352 = t351 ** 2
      t353 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t338,
     # 0.0D0, -t340, 0.0D0)
      t356 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t338,
     # 0.0D0, -t340, 0.0D0)
      t358 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t338,
     # 0.0D0, -t340, 0.0D0)
      t360 = t344 * t347 * t36
      t363 = log(0.4D1 * t342 * t360)
      t365 = t363 ** 2
      t369 = x3 * x1
      t370 = t369 * z
      t373 = x3 * t346
      t375 = Sqrt(t373 * t35)
      t379 = 0.1D1 / (0.2D1 * t370 - 0.2D1 * t369 - t42 + 0.2D1 * t43 * 
     #t375 + x3 - 0.1D1)
      t394 = t5 * (-t353 - t353 * t379)
      t400 = t196 * t341
      t403 = log(-0.4D1 * t400 * t348)
      t408 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t338,
     # 0.0D0, -t340, 0.0D0)
      t409 = t403 ** 2
      t419 = t5 * t353
      t432 = t343 * t347
      t436 = log(-0.4D1 * t262 * t341 * t31 * t432)
      t438 = t95 * t400
      t441 = log(0.4D1 * t438 * t360)
      t454 = t287 * t341
      t457 = log(-0.4D1 * t454 * t348)
      t459 = t457 ** 2
      t475 = (-0.90D2 * t6 * (-t352 * t353 / 0.2D1 + t351 * t356 - t358 
     #- (t358 - t363 * t356 + t365 * t353 / 0.2D1) * t379) + 0.180D3 * t
     #10 * t5 * (t351 * t353 - t356 - (t356 - t363 * t353) * t379) + t20
     # * t394) * t91 * t231 / 0.1440D4 + (t20 * t5 * (t403 * t353 - t356
     #) - 0.90D2 * t6 * (-t408 + t409 * t403 * t353 / 0.6D1 + t403 * t35
     #8 - t409 * t356 / 0.2D1) - t73 * t419 + 0.180D3 * t10 * t5 * (-t35
     #8 - t409 * t353 / 0.2D1 + t403 * t356)) * t231 / 0.1440D4 + (-0.90
     #D2 * t6 * (t436 * t353 - (t356 - t441 * t353) * t379 - t356) + 0.1
     #80D3 * t10 * t394) * t91 * t284 / 0.720D3 + (-0.90D2 * t6 * (t457 
     #* t356 - t459 * t353 / 0.2D1 - t358) + 0.180D3 * t10 * t5 * (-t356
     # + t457 * t353) - t20 * t419) * t139 * t231 / 0.720D3
      t476 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t338, -t340, 0.0D0, t475)
      t478 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t340, t338, 0.0D0, t475)
      t480 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t333)
      t482 = x2 * x3
      t483 = 0.1D1 - x3 + t482
      t484 = 0.1D1 / t483
      t485 = t482 * t484
      t486 = t2 * t485
      t488 = t2 * t35 * t484
      t490 = t483 ** 2
      t491 = 0.1D1 / t490
      t493 = t147 * t35 * t491
      t496 = log(0.4D1 * t95 * t234 * t493)
      t497 = t482 * z
      t498 = t101 * t35
      t500 = Sqrt(t41 * t498)
      t504 = 0.1D1 / (t497 - t42 - 0.1D1 + x3 + 0.2D1 * t43 * t500)
      t506 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t486, -t488, 0.0D0)
      t508 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t486, -t488, 0.0D0)
      t509 = t504 * t508
      t514 = t5 * t504 * t506
      t524 = log(0.4D1 * t95 * t27 * t493)
      t525 = t524 * t504
      t527 = t524 ** 2
      t531 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t486, -t488, 0.0D0)
      t546 = (-0.90D2 * t6 * (t496 * t504 * t506 - t509) - 0.180D3 * t10
     # * t514) * t91 * t284 / 0.720D3 + (-0.90D2 * t6 * (t525 * t508 - t
     #527 * t504 * t506 / 0.2D1 - t504 * t531) + 0.180D3 * t10 * t5 * (t
     #525 * t506 - t509) - t20 * t514) * t91 * t139 / 0.1440D4
      t547 = FJET(XB1, XB2, s, 0.0D0, t486, 0.0D0, -t488, 0.0D0, t546)
      t549 = FJET(XB1, XB2, s, 0.0D0, -t488, 0.0D0, t486, 0.0D0, t546)
      t551 = x2 * x1
      t553 = t2 * t551 * t347
      t556 = t101 * s * t1 * x1
      t557 = t1 ** 2
      t562 = s * t557 * x2 * x1 * t339 * t347
      t563 = t551 * z
      t565 = 0.1D1 / (z + t563 - t345 + x1 - t551)
      t566 = t346 * t565
      t567 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t553, -t556
     #, 0.0D0, -t340, t562)
      t568 = t566 * t567
      t570 = t344 * t347 * t101
      t573 = log(0.4D1 * t438 * t570)
      t575 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t553, -t556
     #, 0.0D0, -t340, t562)
      t576 = t565 * t575
      t581 = t10 * t5
      t582 = t566 * t575
      t588 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t553, -t556
     #, 0.0D0, -t340, t562)
      t592 = log(0.4D1 * t454 * t570)
      t593 = t592 ** 2
      t597 = t592 * t346
      t614 = (-0.90D2 * t6 * (-t568 + t573 * t346 * t576) - 0.180D3 * t5
     #81 * t582) * t91 * t284 / 0.720D3 + (-0.90D2 * t6 * (-t566 * t588 
     #- t593 * t346 * t576 / 0.2D1 + t597 * t565 * t567) + 0.180D3 * t10
     # * t5 * (-t568 + t597 * t576) - t20 * t5 * t582) * t139 * t231 / 0
     #.720D3
      t615 = FJET(XB1, XB2, s, 0.0D0, -t553, -t340, -t556, t562, t614)
      t617 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t333)
      t619 = FJET(XB1, XB2, s, t338, -t340, 0.0D0, 0.0D0, 0.0D0, t475)
      t621 = FJET(XB1, XB2, s, t486, 0.0D0, -t488, 0.0D0, 0.0D0, t546)
      t626 = t35 * s * t1 * t339 * t484
      t627 = x2 * z
      t628 = t482 * x1
      t629 = t482 * t345
      t631 = Sqrt(-t373 * t498)
      t632 = t43 * t631
      t635 = z + x1 - t345 - t627 - t551 + t563 - t41 - t369 + t370 + t4
     #97 + t628 - t629 + t95 + 0.2D1 * t632 * x2
      t638 = t338 * t635 * t347 * t484
      t639 = t340 * t485
      t645 = t338 * x2 * (-t41 - t369 + t370 + t497 + t628 - t629 - 0.1D
     #1 + x3 + 0.2D1 * t632) * t347 * t484
      t652 = log(-0.4D1 * t95 * t400 * t31 * t432 * t498 * t491)
      t658 = x3 * t25
      t678 = -0.5D1 * t370 + t41 + 0.4D1 * t629 - 0.4D1 * t197 * t627 - 
     #0.3D1 * t658 * t551 - t95 * t345 + 0.2D1 * t197 * t25 * x2 + t95 *
     # t25 * x1 + 0.2D1 * t95 * t196 * z - t95 * t196 * t25 - 0.2D1 * t6
     #32 * t345 - 0.2D1 * t632 * t551 - z + t551 + 0.2D1 * t632 * t563
      t694 = t345 + t369 - t563 - 0.2D1 * t197 - 0.2D1 * t658 - x1 - t62
     #8 + t482 * t25 - t262 + 0.2D1 * t632 * x1 + 0.2D1 * t632 * z - 0.2
     #D1 * t197 * t25 + 0.4D1 * t658 * x1 + 0.4D1 * t197 * z + 0.2D1 * t
     #197 * x2
      t696 = 0.1D1 / (t678 + t694)
      t697 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t645, -t638,
     # -t639, t626, t562)
      t700 = t346 * t696
      t701 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, t645, -t638,
     # -t639, t626, t562)
      t709 = -0.90D2 * t6 * (t652 * t346 * t696 * t697 - t700 * t701) - 
     #0.180D3 * t581 * t700 * t697
      t712 = t709 * t91 * t284 / 0.720D3
      t713 = FJET(XB1, XB2, s, t626, -t638, -t639, t645, t562, t712)
      t716 = t91 * t139 * t231
      t719 = FJET(XB1, XB2, s, t645, -t639, -t638, t626, t562, t712)
      t723 = FJET(XB1, XB2, s, -t340, t338, 0.0D0, 0.0D0, 0.0D0, t475)
      t725 = FJET(XB1, XB2, s, -t340, -t556, 0.0D0, -t553, t562, t614)
      t727 = FJET(XB1, XB2, s, -t556, -t340, -t553, 0.0D0, t562, t614)
      t729 = FJET(XB1, XB2, s, -t488, 0.0D0, t486, 0.0D0, 0.0D0, t546)
      t731 = FJET(XB1, XB2, s, -t553, 0.0D0, -t556, -t340, t562, t614)
      t733 = FJET(XB1, XB2, s, -t638, t626, t645, -t639, t562, t712)
      t737 = FJET(XB1, XB2, s, -t639, t645, t626, -t638, t562, t712)
      rrgg2qqbarht6s2e1 = t334 * t333 + t336 * t333 + t476 * t475 + t478
     # * t475 + t480 * t333 + t547 * t546 + t549 * t546 + t615 * t614 + 
     #t617 * t333 + t619 * t475 + t621 * t546 + t713 * t709 * t716 / 0.7
     #20D3 + t719 * t709 * t716 / 0.720D3 + t723 * t475 + t725 * t614 + 
     #t727 * t614 + t729 * t546 + t731 * t614 + t733 * t709 * t716 / 0.7
     #20D3 + t737 * t709 * t716 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht6s2e0
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t19 = log(0.4D1 * t9 * t16)
      t20 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t22 = -0.1D1 + x3
      t23 = 0.1D1 / t22
      t24 = t16 * t23
      t27 = log(-0.4D1 * t9 * t24)
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t13)
      t34 = Sqrt(-t30 * t22)
      t38 = 0.1D1 / (-0.1D1 - t31 + x3 + 0.2D1 * t32 * t34)
      t43 = lh * pi
      t50 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t57 = 0.1D1 / x2
      t58 = t57 * t52
      t62 = x2 ** 2
      t63 = t62 * t8
      t66 = log(0.4D1 * t63 * t16)
      t68 = -0.1D1 + x2
      t69 = t16 * t68
      t72 = log(-0.4D1 * t63 * t69)
      t79 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t83 = log(0.4D1 * t8 * t12 * t15)
      t85 = t83 ** 2
      t96 = lh ** 2
      t97 = 0.180D3 * t96
      t98 = pi ** 2
      t99 = 0.30D2 * t98
      t100 = -t97 + t99
      t101 = t100 * pi
      t102 = t5 * t20
      t103 = t101 * t102
      t107 = x3 * t12
      t110 = log(0.4D1 * t107 * t15)
      t111 = t110 ** 2
      t115 = log(-0.4D1 * t107 * t15 * t23)
      t116 = t115 ** 2
      t127 = -0.90D2 * t6 * t7 + 0.180D3 * t43 * t102
      t133 = t5 * t7
      t143 = log(0.4D1 * t16)
      t146 = t143 ** 2
      t152 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t176 = t62 * x3
      t179 = log(-0.4D1 * t176 * t69)
      t183 = log(0.4D1 * t176 * t16)
      t187 = log(-0.4D1 * t176 * t24)
      t201 = t12 * t62
      t204 = log(0.4D1 * t201 * t15)
      t205 = t204 ** 2
      t206 = t15 * t68
      t209 = log(-0.4D1 * t201 * t206)
      t210 = t209 ** 2
      t221 = (-0.90D2 * t6 * (t7 - t19 * t20 + (t7 - t27 * t20) * t38) +
     # 0.180D3 * t43 * t5 * (t20 + t20 * t38)) * t50 * t52 / 0.1440D4 - 
     #t6 * t20 * t38 * t50 * t58 / 0.8D1 - t6 * (-t66 * t20 + t72 * t20)
     # * t57 * t52 / 0.8D1 + (-0.90D2 * t6 * (t79 - t83 * t7 + t85 * t20
     # / 0.2D1) + 0.180D3 * t43 * t5 * (t7 - t83 * t20) + t103) * t52 / 
     #0.1440D4 + (-0.90D2 * t6 * t20 * (t111 / 0.2D1 + t116 * t38 / 0.2D
     #1) + t127 * (-t110 - t115 * t38) + (-0.90D2 * t6 * t79 + 0.180D3 *
     # t43 * t133 + t103) * (0.1D1 + t38)) * t50 / 0.2880D4 + (-0.180D3 
     #* t143 * lh - 0.45D2 * t146 - t97 + t99) * pi * t133 / 0.2880D4 - 
     #t6 * t152 / 0.32D2 + (0.90D2 * t146 * lh - 0.60D2 * lh * t98 + 0.2
     #40D3 * zeta3 + 0.120D3 * t96 * lh + 0.15D2 * t146 * t143 - t143 * 
     #t100) * pi * t102 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t143) * pi
     # * t5 * t79 / 0.2880D4 + (-0.90D2 * t6 * (t179 * t20 - t183 * t20 
     #+ (t7 - t187 * t20) * t38) + 0.180D3 * t43 * t102 * t38) * t50 * t
     #57 / 0.1440D4 + (-0.90D2 * t6 * t20 * (t205 / 0.2D1 - t210 / 0.2D1
     #) + t127 * (-t204 + t209)) * t57 / 0.1440D4
      t222 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t221)
      t224 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t221)
      t226 = t2 * x1
      t227 = -0.1D1 + x1
      t228 = t2 * t227
      t229 = 0.1D1 / t10
      t230 = t9 * t229
      t231 = t227 ** 2
      t232 = t15 * t231
      t233 = x1 * z
      t234 = -z - x1 + t233
      t235 = 0.1D1 / t234
      t236 = t232 * t235
      t239 = log(-0.4D1 * t230 * t236)
      t240 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t226,
     # 0.0D0, -t228, 0.0D0)
      t242 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t226,
     # 0.0D0, -t228, 0.0D0)
      t247 = log(0.4D1 * t230 * t232 * t235 * t23)
      t250 = x3 * x1
      t251 = t250 * z
      t254 = x3 * t234
      t256 = Sqrt(t254 * t22)
      t260 = 0.1D1 / (0.2D1 * t251 - 0.2D1 * t250 - t31 + 0.2D1 * t32 * 
     #t256 + x3 - 0.1D1)
      t266 = -t240 - t240 * t260
      t276 = t50 * t57 * t52
      t279 = t63 * t229
      t282 = log(-0.4D1 * t279 * t236)
      t287 = t5 * t240
      t294 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t226,
     # 0.0D0, -t228, 0.0D0)
      t298 = log(-0.4D1 * t8 * t229 * t236)
      t299 = t298 ** 2
      t315 = (-0.90D2 * t6 * (t239 * t240 - t242 - (t242 - t247 * t240) 
     #* t260) + 0.180D3 * t43 * t5 * t266) * t50 * t52 / 0.1440D4 - t6 *
     # t266 * t276 / 0.8D1 + (-0.90D2 * t6 * (-t242 + t282 * t240) - 0.1
     #80D3 * t43 * t287) * t57 * t52 / 0.720D3 + (-0.90D2 * t6 * (-t294 
     #- t299 * t240 / 0.2D1 + t298 * t242) + 0.180D3 * t43 * t5 * (t298 
     #* t240 - t242) - t101 * t287) * t52 / 0.1440D4
      t316 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t226, -t228, 0.0D0, t315)
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t228, t226, 0.0D0, t315)
      t320 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t221)
      t322 = x2 * x3
      t323 = 0.1D1 - x3 + t322
      t324 = 0.1D1 / t323
      t325 = t322 * t324
      t326 = t2 * t325
      t328 = t2 * t22 * t324
      t329 = t322 * z
      t330 = t68 * t22
      t332 = Sqrt(t30 * t330)
      t336 = 0.1D1 / (t329 - t31 - 0.1D1 + x3 + 0.2D1 * t32 * t332)
      t338 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t326, -t328, 0.0D0)
      t344 = t323 ** 2
      t350 = log(0.4D1 * t176 * t12 * t206 * t22 / t344)
      t353 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t326, -t328, 0.0D0)
      t366 = t6 * t336 * t338 * t50 * t58 / 0.8D1 + (-0.90D2 * t6 * (t35
     #0 * t336 * t338 - t336 * t353) - 0.180D3 * t43 * t5 * t336 * t338)
     # * t50 * t57 / 0.1440D4
      t367 = FJET(XB1, XB2, s, 0.0D0, t326, 0.0D0, -t328, 0.0D0, t366)
      t369 = FJET(XB1, XB2, s, 0.0D0, -t328, 0.0D0, t326, 0.0D0, t366)
      t371 = x2 * x1
      t373 = t2 * t371 * t235
      t376 = t68 * s * t1 * x1
      t377 = t1 ** 2
      t382 = s * t377 * x2 * x1 * t227 * t235
      t383 = t371 * z
      t385 = 0.1D1 / (z + t383 - t233 + x1 - t371)
      t386 = t234 * t385
      t388 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, -t376
     #, 0.0D0, -t228, t382)
      t393 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, -t376
     #, 0.0D0, -t228, t382)
      t400 = log(0.4D1 * t279 * t15 * t235 * t231 * t68)
      t415 = t6 * t386 * t388 * t50 * t58 / 0.8D1 + (-0.90D2 * t6 * (-t3
     #86 * t393 + t400 * t234 * t385 * t388) - 0.180D3 * t43 * t5 * t386
     # * t388) * t57 * t52 / 0.720D3
      t416 = FJET(XB1, XB2, s, 0.0D0, -t373, -t228, -t376, t382, t415)
      t418 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t221)
      t420 = FJET(XB1, XB2, s, t226, -t228, 0.0D0, 0.0D0, 0.0D0, t315)
      t422 = FJET(XB1, XB2, s, t326, 0.0D0, -t328, 0.0D0, 0.0D0, t366)
      t427 = t22 * s * t1 * t227 * t324
      t428 = x2 * z
      t429 = t322 * x1
      t430 = t322 * t233
      t432 = Sqrt(-t254 * t330)
      t433 = t32 * t432
      t436 = z + x1 - t233 - t428 - t371 + t383 - t30 - t250 + t251 + t3
     #29 + t429 - t430 + t176 + 0.2D1 * t433 * x2
      t439 = t226 * t436 * t235 * t324
      t440 = t228 * t325
      t446 = t226 * x2 * (-t30 - t250 + t251 + t329 + t429 - t430 - 0.1D
     #1 + x3 + 0.2D1 * t433) * t235 * t324
      t455 = x3 * t10
      t466 = -t429 + t322 * t10 - t176 * t8 + 0.2D1 * t433 * x1 + 0.2D1 
     #* t433 * z - 0.2D1 * t9 * t10 + 0.4D1 * t455 * x1 + 0.4D1 * t9 * z
     # + 0.2D1 * t9 * x2 + t30 + 0.2D1 * t433 * t383 - 0.2D1 * t9 - 0.2D
     #1 * t455 + t233 + t250
      t488 = t371 - x1 - z + 0.4D1 * t430 - 0.4D1 * t9 * t428 - 0.3D1 * 
     #t455 * t371 - t176 * t233 + 0.2D1 * t9 * t10 * x2 + t176 * t10 * x
     #1 + 0.2D1 * t176 * t8 * z - t176 * t8 * t10 - 0.2D1 * t433 * t233 
     #- 0.2D1 * t433 * t371 - 0.5D1 * t251 - t383
      t490 = 0.1D1 / (t466 + t488)
      t493 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, t446, -t439,
     # -t440, t427, t382)
      t497 = t6 * t234 * t490 * t493 * t50 * t58 / 0.8D1
      t498 = FJET(XB1, XB2, s, t427, -t439, -t440, t446, t382, t497)
      t500 = t5 * t234
      t503 = t490 * t493 * t276
      t506 = FJET(XB1, XB2, s, t446, -t440, -t439, t427, t382, t497)
      t511 = FJET(XB1, XB2, s, -t228, t226, 0.0D0, 0.0D0, 0.0D0, t315)
      t513 = FJET(XB1, XB2, s, -t228, -t376, 0.0D0, -t373, t382, t415)
      t515 = FJET(XB1, XB2, s, -t376, -t228, -t373, 0.0D0, t382, t415)
      t517 = FJET(XB1, XB2, s, -t328, 0.0D0, t326, 0.0D0, 0.0D0, t366)
      t519 = FJET(XB1, XB2, s, -t373, 0.0D0, -t376, -t228, t382, t415)
      t521 = FJET(XB1, XB2, s, -t439, t427, t446, -t440, t382, t497)
      t526 = FJET(XB1, XB2, s, -t440, t446, t427, -t439, t382, t497)
      rrgg2qqbarht6s2e0 = t222 * t221 + t224 * t221 + t316 * t315 + t318
     # * t315 + t320 * t221 + t367 * t366 + t369 * t366 + t416 * t415 + 
     #t418 * t221 + t420 * t315 + t422 * t366 + t498 * pi * t500 * t503 
     #/ 0.8D1 + t506 * pi * t500 * t503 / 0.8D1 + t511 * t315 + t513 * t
     #415 + t515 * t415 + t517 * t366 + t519 * t415 + t521 * pi * t500 *
     # t503 / 0.8D1 + t526 * pi * t500 * t503 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht6s2em1
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
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
      t38 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t41 = lh * pi
      t42 = t5 * t7
      t44 = 0.180D3 * t41 * t42
      t49 = 0.1D1 / x3
      t52 = x1 ** 2
      t56 = log(0.4D1 * t52 * t10 * t14)
      t62 = 0.1D1 / x1
      t73 = 0.1D1 / x2
      t77 = x2 ** 2
      t78 = t10 * t77
      t81 = log(0.4D1 * t78 * t14)
      t82 = -0.1D1 + x2
      t86 = log(-0.4D1 * t78 * t14 * t82)
      t92 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t98 = log(0.4D1 * t10 * t14)
      t107 = t98 ** 2
      t109 = lh ** 2
      t111 = pi ** 2
      t117 = (-0.90D2 * t6 * t7 * (-t17 - t23 * t32) + (-0.90D2 * t6 * t
     #38 + t44) * (0.1D1 + t32)) * t49 / 0.2880D4 + (-0.90D2 * t6 * (t38
     # - t56 * t7) + t44) * t62 / 0.1440D4 - t6 * (t7 + t7 * t32) * t49 
     #* t62 / 0.16D2 - t6 * t7 * t32 * t49 * t73 / 0.16D2 - t6 * t7 * (-
     #t81 + t86) * t73 / 0.16D2 - t6 * t92 / 0.32D2 + (0.180D3 * lh + 0.
     #90D2 * t98) * pi * t5 * t38 / 0.2880D4 + (-0.180D3 * lh * t98 - 0.
     #45D2 * t107 - 0.180D3 * t109 + 0.30D2 * t111) * pi * t42 / 0.2880D
     #4
      t118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t117)
      t120 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t117)
      t122 = t2 * x1
      t123 = -0.1D1 + x1
      t124 = t2 * t123
      t125 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t122,
     # 0.0D0, -t124, 0.0D0)
      t132 = x1 * z
      t133 = -z - x1 + t132
      t134 = 0.1D1 / t133
      t136 = t123 ** 2
      t140 = log(-0.4D1 * t52 / t8 * t14 * t134 * t136)
      t142 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t122,
     # 0.0D0, -t124, 0.0D0)
      t152 = x3 * x1
      t158 = Sqrt(x3 * t133 * t18)
      t169 = t6 * t125 * t73 * t62 / 0.8D1 + (-0.90D2 * t6 * (t140 * t12
     #5 - t142) - 0.180D3 * t41 * t5 * t125) * t62 / 0.1440D4 - t6 * (-t
     #125 - t125 / (0.2D1 * t152 * z - 0.2D1 * t152 - t25 + 0.2D1 * t26 
     #* t158 + x3 - 0.1D1)) * t49 * t62 / 0.16D2
      t170 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t122, -t124, 0.0D0, t169)
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t124, t122, 0.0D0, t169)
      t174 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t117)
      t176 = x2 * x3
      t178 = 0.1D1 / (0.1D1 - x3 + t176)
      t180 = t2 * t176 * t178
      t182 = t2 * t18 * t178
      t186 = Sqrt(t24 * t82 * t18)
      t190 = 0.1D1 / (t176 * z - t25 - 0.1D1 + x3 + 0.2D1 * t26 * t186)
      t192 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t180, -t182, 0.0D0)
      t196 = t6 * t190 * t192 * t49 * t73 / 0.16D2
      t197 = FJET(XB1, XB2, s, 0.0D0, t180, 0.0D0, -t182, 0.0D0, t196)
      t202 = t190 * t192 * t49 * t73
      t205 = FJET(XB1, XB2, s, 0.0D0, -t182, 0.0D0, t180, 0.0D0, t196)
      t210 = x2 * x1
      t212 = t2 * t210 * t134
      t215 = t82 * s * t1 * x1
      t216 = t1 ** 2
      t221 = s * t216 * x2 * x1 * t123 * t134
      t226 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t212, -t215
     #, 0.0D0, -t124, t221)
      t229 = 0.1D1 / (z + t210 * z - t132 + x1 - t210) * t226 * t73 * t6
     #2
      t231 = t6 * t133 * t229 / 0.8D1
      t232 = FJET(XB1, XB2, s, 0.0D0, -t212, -t124, -t215, t221, t231)
      t234 = t5 * t133
      t238 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t117)
      t240 = FJET(XB1, XB2, s, t122, -t124, 0.0D0, 0.0D0, 0.0D0, t169)
      t242 = FJET(XB1, XB2, s, t180, 0.0D0, -t182, 0.0D0, 0.0D0, t196)
      t247 = FJET(XB1, XB2, s, -t124, t122, 0.0D0, 0.0D0, 0.0D0, t169)
      t249 = FJET(XB1, XB2, s, -t124, -t215, 0.0D0, -t212, t221, t231)
      t254 = FJET(XB1, XB2, s, -t215, -t124, -t212, 0.0D0, t221, t231)
      t259 = FJET(XB1, XB2, s, -t212, 0.0D0, -t215, -t124, t221, t231)
      t264 = FJET(XB1, XB2, s, -t182, 0.0D0, t180, 0.0D0, 0.0D0, t196)
      rrgg2qqbarht6s2em1 = t118 * t117 + t120 * t117 + t170 * t169 + t17
     #2 * t169 + t174 * t117 + t197 * pi * t5 * t202 / 0.16D2 + t205 * p
     #i * t5 * t202 / 0.16D2 + t232 * pi * t234 * t229 / 0.8D1 + t238 * 
     #t117 + t240 * t169 + t242 * pi * t5 * t202 / 0.16D2 + t247 * t169 
     #+ t249 * pi * t234 * t229 / 0.8D1 + t254 * pi * t234 * t229 / 0.8D
     #1 + t259 * pi * t234 * t229 / 0.8D1 + t264 * pi * t5 * t202 / 0.16
     #D2

      end function



      doubleprecision function rrgg2qqbarht6s2em2
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = x3 * z
      t14 = x4 * pi
      t15 = cos(t14)
      t18 = Sqrt(-t12 * (-0.1D1 + x3))
      t29 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t33 = z ** 2
      t36 = Sin(t14)
      t37 = t36 ** 2
      t40 = log(0.4D1 / t33 / z * t37)
      t47 = -t6 * t7 * t8 / 0.16D2 - t6 * t7 * (0.1D1 + 0.1D1 / (-0.1D1 
     #- 0.2D1 * t12 + x3 + 0.2D1 * t15 * t18)) / x3 / 0.32D2 - t6 * t29 
     #/ 0.32D2 + (0.180D3 * lh + 0.90D2 * t40) * pi * t5 * t7 / 0.2880D4
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t47)
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t47)
      t52 = t2 * x1
      t54 = t2 * (-0.1D1 + x1)
      t55 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t52, 0
     #.0D0, -t54, 0.0D0)
      t58 = t6 * t55 * t8 / 0.16D2
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t52, -t54, 0.0D0, t58)
      t62 = t5 * t55 * t8
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t54, t52, 0.0D0, t58)
      t69 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t47)
      t71 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t47)
      t73 = FJET(XB1, XB2, s, t52, -t54, 0.0D0, 0.0D0, 0.0D0, t58)
      t77 = FJET(XB1, XB2, s, -t54, t52, 0.0D0, 0.0D0, 0.0D0, t58)
      rrgg2qqbarht6s2em2 = t48 * t47 + t50 * t47 + t59 * pi * t62 / 0.16
     #D2 + t65 * pi * t62 / 0.16D2 + t69 * t47 + t71 * t47 + t73 * pi * 
     #t62 / 0.16D2 + t77 * pi * t62 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht6s2em3
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

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
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t9 = pi * t5 * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t5 * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2qqbarht6s2em3 = -t10 * pi * t12 / 0.32D2 - t14 * pi * t12 / 0
     #.32D2 - t17 * pi * t12 / 0.32D2 - t20 * pi * t12 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht6s2em4
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
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh61J1
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
      t2 = S12 ** 2
      t4 = S34 ** 2
      rrgg2qqbarh61J1 = (S34 * nf * t2 / 0.6D1 + t4 * S34 * nf / 0.6D1) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J2
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      rrgg2qqbarh61J2 = (S34 * nf * t2 / 0.6D1 + t5 * nf * S12 / 0.3D1 +
     # t5 * S34 * nf / 0.6D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J3
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
      t1 = S34 ** 2
      t4 = S13 + S14 + S34
      t7 = S23 + S24 + S34
      t13 = S24 ** 2
      t17 = S14 ** 2
      rrgg2qqbarh61J3 = ((t1 * nf + S24 * nf * t4 / 0.6D1 + t7 * S14 * n
     #f / 0.6D1) * S12 + t13 * nf * t4 / 0.6D1 + t7 * t17 * nf / 0.6D1) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J4
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarh61J4 = (-S34 * nf * t2 / 0.6D1 + (0.5D1 / 0.3D1 * t5 * 
     #nf + S24 * nf * t9 / 0.3D1 + t12 * S14 * nf / 0.3D1) * S12 - t5 * 
     #S34 * nf / 0.6D1 + t21 * nf * t9 / 0.3D1 + t12 * t25 * nf / 0.3D1)
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J5
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarh61J5 = (-S34 * nf * t2 / 0.3D1 + (0.7D1 / 0.3D1 * t5 * 
     #nf + S24 * nf * t9 / 0.2D1 + t12 * S14 * nf / 0.2D1) * S12 - t5 * 
     #S34 * nf / 0.3D1 + t21 * nf * t9 / 0.2D1 + t12 * t25 * nf / 0.2D1)
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J6
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarh61J6 = (-0.3D1 / 0.2D1 * S34 * nf * t2 + (0.3D1 * t5 * 
     #nf + 0.2D1 / 0.3D1 * S24 * nf * t9 + 0.2D1 / 0.3D1 * t12 * S14 * n
     #f) * S12 - 0.3D1 / 0.2D1 * t5 * S34 * nf + 0.2D1 / 0.3D1 * t21 * n
     #f * t9 + 0.2D1 / 0.3D1 * t12 * t25 * nf) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J7
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarh61J7 = (-0.5D1 / 0.6D1 * S34 * nf * t2 + (0.5D1 / 0.3D1
     # * t5 * nf + 0.5D1 / 0.6D1 * S24 * nf * t9 + 0.5D1 / 0.6D1 * t12 *
     # S14 * nf) * S12 - 0.5D1 / 0.6D1 * t5 * S34 * nf + 0.5D1 / 0.6D1 *
     # t21 * nf * t9 + 0.5D1 / 0.6D1 * t12 * t25 * nf) / pi * wd / z

      end function
  
 