  
      subroutine rrgg2qqbarht5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh51J1  
      doubleprecision rrgg2qqbarh51J2  
      doubleprecision rrgg2qqbarh51J3  
      doubleprecision rrgg2qqbarh51J4  
      doubleprecision rrgg2qqbarh51J5  
      doubleprecision rrgg2qqbarh51J6  
      doubleprecision rrgg2qqbarh51J7  
      doubleprecision rrgg2qqbarht5s1e1  
      doubleprecision rrgg2qqbarht5s1e0  
      doubleprecision rrgg2qqbarht5s1em1  
      doubleprecision rrgg2qqbarht5s1em2  
      doubleprecision rrgg2qqbarht5s1em3  
      doubleprecision rrgg2qqbarht5s1em4  
      doubleprecision rrgg2qqbarht5s2e1  
      doubleprecision rrgg2qqbarht5s2e0  
      doubleprecision rrgg2qqbarht5s2em1  
      doubleprecision rrgg2qqbarht5s2em2  
      doubleprecision rrgg2qqbarht5s2em3  
      doubleprecision rrgg2qqbarht5s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht5s1e1
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t6 = t5 * pi
      t7 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t10 = t5 * lh
      t11 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t12 = pi * t11
      t15 = lh ** 2
      t17 = pi ** 2
      t19 = -0.180D3 * t15 + 0.30D2 * t17
      t20 = t5 * t19
      t21 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t22 = pi * t21
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = x3 * t26
      t28 = x4 * pi
      t29 = Sin(t28)
      t30 = t29 ** 2
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t27 * t30 * t32)
      t37 = cos(t28)
      t39 = Sqrt(-x3 * t31)
      t44 = 0.1D1 / (-z - x3 + 0.2D1 * t37 * t39 * z)
      t48 = log(0.4D1 * t27 * t30)
      t49 = 0.1D1 / z
      t53 = t48 ** 2
      t56 = t36 ** 2
      t65 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t73 = -0.60D2 * lh * t17 + 0.240D3 * zeta3 + 0.120D3 * t15 * lh
      t74 = t5 * t73
      t93 = 0.1D1 / x3
      t96 = t26 * t30
      t98 = log(0.4D1 * t96)
      t99 = t98 ** 2
      t100 = t99 * t49
      t103 = t49 * t65
      t105 = t99 * t98 * t49
      t108 = t98 * t49
      t114 = t49 * t7
      t121 = t49 * t11
      t126 = t49 * t21
      t129 = t17 ** 2
      t130 = t15 ** 2
      t137 = rrgg2qqbarh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t144 = t99 ** 2
      t154 = x1 ** 2
      t155 = x3 * t154
      t156 = t96 * t32
      t159 = log(-0.4D1 * t155 * t156)
      t161 = t159 ** 2
      t168 = log(0.4D1 * t155 * t96)
      t169 = t168 * t49
      t171 = t168 ** 2
      t187 = -t126 - t21 * t44
      t188 = pi * t187
      t192 = 0.1D1 / x1
      t195 = t5 * t49
      t196 = t19 * pi
      t197 = t154 * t26
      t198 = t197 * t30
      t200 = log(0.4D1 * t198)
      t205 = t200 ** 2
      t216 = t73 * pi
      t218 = t195 * t216 * t21
      t219 = lh * pi
      t230 = x2 ** 2
      t231 = t230 * x3
      t232 = t231 * t198
      t234 = log(0.4D1 * t232)
      t237 = t231 * t154
      t240 = log(-0.4D1 * t237 * t156)
      t251 = 0.1D1 / x2
      t252 = t251 * t192
      t255 = t230 * t154
      t258 = log(0.4D1 * t255 * t96)
      t259 = t258 * t49
      t261 = t258 ** 2
      t273 = pi * t49
      t282 = log(-0.4D1 * t231 * t156)
      t284 = t282 ** 2
      t291 = log(0.4D1 * t231 * t96)
      t292 = t291 * t49
      t294 = t291 ** 2
      t316 = t230 * t26
      t319 = log(0.4D1 * t316 * t30)
      t320 = t319 * t49
      t325 = t319 ** 2
      t326 = t325 * t49
      t347 = ((-0.90D2 * t6 * t7 + 0.180D3 * t10 * t12 + t20 * t22) * (-
     #t36 * t44 - t48 * t49) - 0.90D2 * t6 * t21 * (-t53 * t48 * t49 / 0
     #.6D1 - t56 * t36 * t44 / 0.6D1) + (t20 * t12 - 0.90D2 * t6 * t65 +
     # t74 * t22 + 0.180D3 * t10 * pi * t7) * (t44 + t49) + (-0.90D2 * t
     #6 * t11 + 0.180D3 * t10 * t22) * (t56 * t44 / 0.2D1 + t53 * t49 / 
     #0.2D1)) * t93 / 0.2880D4 + (0.180D3 * (t100 * t11 / 0.2D1 + t103 -
     # t105 * t21 / 0.6D1 - t108 * t7) * t5 * lh + (t114 - t108 * t11 + 
     #t100 * t21 / 0.2D1) * t5 * t19 + (t121 - t108 * t21) * t5 * t73 + 
     #t126 * t5 * (-0.480D3 * lh * zeta3 - t129 - 0.60D2 * t130 + 0.60D2
     # * t15 * t17) - 0.90D2 * (t49 * t137 - t105 * t11 / 0.6D1 + t100 *
     # t7 / 0.2D1 - t108 * t65 + t144 * t49 * t21 / 0.24D2) * t5) * pi /
     # 0.2880D4 - (-0.90D2 * t6 * (-(t7 - t159 * t11 + t161 * t21 / 0.2D
     #1) * t44 - t114 + t169 * t11 - t171 * t49 * t21 / 0.2D1) + 0.180D3
     # * t10 * pi * (-(t11 - t159 * t21) * t44 - t121 + t169 * t21) + t2
     #0 * t188) * t93 * t192 / 0.1440D4 + (t195 * t196 * (t11 - t200 * t
     #21) - 0.90D2 * t195 * pi * (t205 * t11 / 0.2D1 + t65 - t205 * t200
     # * t21 / 0.6D1 - t200 * t7) + t218 + 0.180D3 * t195 * t219 * (t7 -
     # t200 * t11 + t205 * t21 / 0.2D1)) * t192 / 0.1440D4 - (-0.90D2 * 
     #t6 * (-t121 + t234 * t49 * t21 - (t11 - t240 * t21) * t44) + 0.180
     #D3 * t10 * t188) * t93 * t252 / 0.720D3 + (-0.90D2 * t6 * (t114 - 
     #t259 * t11 + t261 * t49 * t21 / 0.2D1) + 0.180D3 * t10 * pi * (t12
     #1 - t259 * t21) + t20 * t273 * t21) * t251 * t192 / 0.720D3 + (-0.
     #90D2 * t6 * ((t7 - t282 * t11 + t284 * t21 / 0.2D1) * t44 + t114 -
     # t292 * t11 + t294 * t49 * t21 / 0.2D1) + 0.180D3 * t10 * pi * ((t
     #11 - t282 * t21) * t44 + t121 - t292 * t21) - t20 * pi * t187) * t
     #93 * t251 / 0.1440D4 + (t20 * pi * (t121 - t320 * t21) - 0.90D2 * 
     #t6 * (t326 * t11 / 0.2D1 + t103 - t325 * t319 * t49 * t21 / 0.6D1 
     #- t320 * t7) + t218 + 0.180D3 * t10 * pi * (t114 - t320 * t11 + t3
     #26 * t21 / 0.2D1)) * t251 / 0.1440D4
      t348 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t347)
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t347)
      t352 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t347)
      t354 = t2 * x1
      t355 = -0.1D1 + x1
      t356 = x1 * z
      t357 = 0.1D1 - x1 + t356
      t358 = 0.1D1 / t357
      t360 = t2 * t355 * t358
      t361 = t1 ** 2
      t362 = s * t361
      t364 = x1 * t355 * t358
      t365 = t362 * t364
      t366 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t360
     #, t354, 0.0D0, -t365)
      t367 = t49 * t366
      t368 = t155 * t26
      t369 = t355 ** 2
      t370 = t30 * t369
      t371 = t370 * t358
      t374 = log(0.4D1 * t368 * t371)
      t375 = t374 * t49
      t376 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t360
     #, t354, 0.0D0, -t365)
      t378 = t374 ** 2
      t380 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t360
     #, t354, 0.0D0, -t365)
      t385 = t370 * t358 * t32
      t388 = log(-0.4D1 * t368 * t385)
      t389 = t388 * t357
      t391 = t388 ** 2
      t396 = t155 * t25
      t397 = x1 * t25
      t398 = x3 * t25
      t399 = t398 * x1
      t401 = 0.2D1 * t155 * z
      t402 = x3 * x1
      t403 = t402 * z
      t404 = 0.3D1 * t403
      t405 = 0.2D1 * t402
      t406 = x3 * t357
      t408 = Sqrt(-t406 * t31)
      t412 = -z - t396 - t397 + t399 + t401 + t356 - t404 - x3 - t155 + 
     #t405 + 0.2D1 * t37 * t408 * z
      t413 = 0.1D1 / t412
      t418 = t49 * t376
      t420 = t357 * t376
      t432 = pi * (t49 * t380 + t357 * t380 * t413)
      t437 = (-0.90D2 * t6 * (t367 - t375 * t376 + t378 * t49 * t380 / 0
     #.2D1 + (t357 * t366 - t389 * t376 + t391 * t357 * t380 / 0.2D1) * 
     #t413) + 0.180D3 * t10 * pi * (t418 - t375 * t380 + (t420 - t389 * 
     #t380) * t413) + t20 * t432) * t93 * t192 / 0.1440D4
      t440 = log(0.4D1 * t197 * t371)
      t442 = -t376 + t440 * t380
      t445 = t440 ** 2
      t448 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t360
     #, t354, 0.0D0, -t365)
      t453 = -t445 * t376 / 0.2D1 - t448 + t445 * t440 * t380 / 0.6D1 + 
     #t440 * t366
      t458 = t195 * t216 * t380
      t462 = -t366 + t440 * t376 - t445 * t380 / 0.2D1
      t469 = t231 * t197
      t472 = log(-0.4D1 * t469 * t385)
      t477 = t369 * t358
      t481 = log(0.4D1 * t237 * t96 * t477)
      t492 = (-0.90D2 * t6 * ((t420 - t472 * t357 * t380) * t413 + t418 
     #- t481 * t49 * t380) + 0.180D3 * t10 * t432) * t93 * t252 / 0.720D
     #3
      t493 = t255 * t26
      t496 = log(0.4D1 * t493 * t371)
      t497 = t496 * t49
      t499 = t496 ** 2
      t516 = (-0.90D2 * t6 * (-t367 + t497 * t376 - t499 * t49 * t380 / 
     #0.2D1) + 0.180D3 * t10 * pi * (-t418 + t497 * t380) - t20 * t273 *
     # t380) * t251 * t192 / 0.720D3
      t517 = -t437 + (t195 * t196 * t442 - 0.90D2 * t195 * pi * t453 - t
     #458 + 0.180D3 * t195 * t219 * t462) * t192 / 0.1440D4 - t492 + t51
     #6
      t518 = FJET(XB1, XB2, s, 0.0D0, t354, -t360, 0.0D0, -t365, t517)
      t521 = x2 * t1 * s
      t522 = -0.1D1 + x2
      t524 = t522 * t1 * s
      t525 = x2 * z
      t527 = 0.1D1 / (-z + t525 - x2)
      t528 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t521, -t524,
     # 0.0D0, 0.0D0, 0.0D0)
      t529 = t527 * t528
      t530 = t96 * t522
      t533 = log(-0.4D1 * t237 * t530)
      t535 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t521, -t524,
     # 0.0D0, 0.0D0, 0.0D0)
      t541 = pi * t527 * t535
      t548 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, t521, -t524,
     # 0.0D0, 0.0D0, 0.0D0)
      t549 = t527 * t548
      t552 = log(-0.4D1 * t255 * t530)
      t553 = t552 * t527
      t555 = t552 ** 2
      t567 = t20 * t541
      t574 = log(-0.4D1 * t231 * t530)
      t575 = t574 * t527
      t577 = t574 ** 2
      t593 = t30 * t522
      t596 = log(-0.4D1 * t316 * t593)
      t597 = t596 * t527
      t602 = t596 ** 2
      t603 = t602 * t527
      t606 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, t521, -t524,
     # 0.0D0, 0.0D0, 0.0D0)
      t627 = -(-0.90D2 * t6 * (-t529 + t533 * t527 * t535) - 0.180D3 * t
     #10 * t541) * t93 * t252 / 0.720D3 + (-0.90D2 * t6 * (t549 - t553 *
     # t528 + t555 * t527 * t535 / 0.2D1) + 0.180D3 * t10 * pi * (t529 -
     # t553 * t535) + t567) * t251 * t192 / 0.720D3 + (-0.90D2 * t6 * (t
     #549 - t575 * t528 + t577 * t527 * t535 / 0.2D1) + 0.180D3 * t10 * 
     #pi * (t529 - t575 * t535) + t567) * t93 * t251 / 0.1440D4 + (t20 *
     # pi * (t529 - t597 * t535) - 0.90D2 * t6 * (t603 * t528 / 0.2D1 + 
     #t527 * t606 - t602 * t596 * t527 * t535 / 0.6D1 - t597 * t548) + t
     #74 * t541 + 0.180D3 * t10 * pi * (t549 - t597 * t528 + t603 * t535
     # / 0.2D1)) * t251 / 0.1440D4
      t628 = FJET(XB1, XB2, s, 0.0D0, t521, 0.0D0, -t524, 0.0D0, t627)
      t630 = x2 * x3
      t633 = Sqrt(x3 * t522 * t31)
      t634 = t37 * t633
      t636 = 0.2D1 * t634 * x2
      t638 = 0.1D1 - x3 + t630
      t639 = 0.1D1 / t638
      t641 = t2 * (0.1D1 - x3 - x2 + t630 + t231 + t636) * t639
      t646 = t2 * x2 * (-0.1D1 + t630 + 0.2D1 * t634) * t639
      t647 = t630 * z
      t648 = t231 * z
      t654 = 0.1D1 / (z - t525 + x3 - t231 + x2 - t647 + t648 - t636 - 0
     #.2D1 * t634 * z + 0.2D1 * t634 * t525)
      t655 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t646, t641,
     # 0.0D0, 0.0D0, 0.0D0)
      t656 = t654 * t655
      t657 = t638 ** 2
      t658 = 0.1D1 / t657
      t660 = t593 * t31 * t658
      t663 = log(0.4D1 * t469 * t660)
      t665 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t646, t641,
     # 0.0D0, 0.0D0, 0.0D0)
      t671 = pi * t654 * t665
      t678 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t646, t641,
     # 0.0D0, 0.0D0, 0.0D0)
      t683 = log(0.4D1 * t231 * t26 * t660)
      t684 = t683 * t654
      t686 = t683 ** 2
      t703 = -(-0.90D2 * t6 * (-t656 + t663 * t654 * t665) - 0.180D3 * t
     #10 * t671) * t93 * t252 / 0.720D3 + (-0.90D2 * t6 * (t654 * t678 -
     # t684 * t655 + t686 * t654 * t665 / 0.2D1) + 0.180D3 * t10 * pi * 
     #(t656 - t684 * t665) + t20 * t671) * t93 * t251 / 0.1440D4
      t704 = FJET(XB1, XB2, s, 0.0D0, t641, 0.0D0, -t646, 0.0D0, t703)
      t707 = t1 * t355
      t709 = t522 * s * t707 * t358
      t711 = x2 * s * t707
      t713 = t362 * t522 * t364
      t714 = x2 * x1
      t715 = t714 * z
      t717 = 0.1D1 / (z - t525 - t714 + x2 + t715)
      t718 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t711, t709,
     # t354, 0.0D0, t713)
      t719 = t717 * t718
      t721 = t370 * t358 * t522
      t724 = log(-0.4D1 * t469 * t721)
      t726 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t711, t709,
     # t354, 0.0D0, t713)
      t732 = pi * t717 * t726
      t738 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t711, t709,
     # t354, 0.0D0, t713)
      t742 = log(-0.4D1 * t493 * t721)
      t743 = t742 * t717
      t745 = t742 ** 2
      t762 = -(-0.90D2 * t6 * (-t719 + t724 * t717 * t726) - 0.180D3 * t
     #10 * t732) * t93 * t252 / 0.720D3 + (-0.90D2 * t6 * (t717 * t738 -
     # t743 * t718 + t745 * t717 * t726 / 0.2D1) + 0.180D3 * t10 * pi * 
     #(t719 - t743 * t726) + t20 * t732) * t251 * t192 / 0.720D3
      t763 = FJET(XB1, XB2, s, 0.0D0, t709, t354, -t711, t713, t762)
      t765 = FJET(XB1, XB2, s, 0.0D0, -t524, 0.0D0, t521, 0.0D0, t627)
      t767 = FJET(XB1, XB2, s, 0.0D0, -t360, t354, 0.0D0, -t365, t517)
      t769 = FJET(XB1, XB2, s, 0.0D0, -t646, 0.0D0, t641, 0.0D0, t703)
      t771 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t347)
      t787 = -t437 + (t195 * t196 * t442 - 0.90D2 * t195 * pi * t453 - t
     #458 + 0.180D3 * t195 * t219 * t462) * t192 / 0.1440D4 - t492 + t51
     #6
      t788 = FJET(XB1, XB2, s, t354, 0.0D0, 0.0D0, -t360, -t365, t787)
      t790 = t348 * t347 + t350 * t347 + t352 * t347 + t518 * t517 + t62
     #8 * t627 + t704 * t703 + t763 * t762 + t765 * t627 + t767 * t517 +
     # t769 * t703 + t771 * t347 + t788 * t787
      t791 = FJET(XB1, XB2, s, t354, -t711, 0.0D0, t709, t713, t762)
      t793 = FJET(XB1, XB2, s, t521, 0.0D0, -t524, 0.0D0, 0.0D0, t627)
      t795 = FJET(XB1, XB2, s, t641, 0.0D0, -t646, 0.0D0, 0.0D0, t703)
      t797 = FJET(XB1, XB2, s, t709, 0.0D0, -t711, t354, t713, t762)
      t800 = t354 * t630 * t639
      t801 = t2 * t355
      t802 = t231 * t356
      t803 = t231 * x1
      t804 = t522 * t31
      t806 = Sqrt(t406 * t804)
      t807 = t37 * t806
      t809 = 0.2D1 * t807 * x2
      t813 = t801 * (t802 - x2 + t630 - t803 + 0.1D1 - x3 + t231 + t809)
     # * t358 * t639
      t817 = t31 * s * t1 * x1 * t639
      t823 = t801 * x2 * (-0.1D1 + t630 + x1 - t402 - t356 + t403 + 0.2D
     #1 * t807) * t358 * t639
      t828 = x2 * t154
      t840 = -z + t647 - t648 - 0.2D1 * t807 * t525 - 0.2D1 * t807 * t71
     #4 - t828 - x2 + t802 + 0.2D1 * t630 * t356 - 0.2D1 * t155 * t525 -
     # t398 * t714 + t155 * t25 * x2 + 0.2D1 * t714 - 0.3D1 * t715 + 0.2
     #D1 * t807 * t715 + t231 - t803
      t849 = t809 + t714 * t25 + 0.2D1 * t828 * z - t828 * t25 + 0.2D1 *
     # t807 * z + t155 * x2 - t630 * x1 - t396 + t399 + t401 - t404 + t3
     #56 - t397 - t155 + t405 - x3 + t525
      t851 = 0.1D1 / (t840 + t849)
      t852 = t357 * t851
      t853 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t823, -t813,
     # -t817, t800, t713)
      t859 = log(0.4D1 * t232 * t477 * t804 * t658)
      t861 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t823, -t813,
     # -t817, t800, t713)
      t864 = -t852 * t853 + t859 * t357 * t851 * t861
      t870 = 0.180D3 * t10 * pi * t852 * t861
      t871 = -0.90D2 * t6 * t864 - t870
      t874 = t871 * t93 * t252 / 0.720D3
      t875 = FJET(XB1, XB2, s, t800, -t813, -t817, t823, t713, -t874)
      t878 = t93 * t251 * t192
      t881 = FJET(XB1, XB2, s, t823, -t817, -t813, t800, t713, -t874)
      t885 = FJET(XB1, XB2, s, -t524, 0.0D0, t521, 0.0D0, 0.0D0, t627)
      t887 = FJET(XB1, XB2, s, -t360, 0.0D0, 0.0D0, t354, -t365, t787)
      t889 = FJET(XB1, XB2, s, -t711, t354, t709, 0.0D0, t713, t762)
      t891 = FJET(XB1, XB2, s, -t646, 0.0D0, t641, 0.0D0, 0.0D0, t703)
      t896 = -0.90D2 * t6 * t864 - t870
      t899 = t896 * t93 * t252 / 0.720D3
      t900 = FJET(XB1, XB2, s, -t817, t823, t800, -t813, t713, -t899)
      t904 = FJET(XB1, XB2, s, -t813, t800, t823, -t817, t713, -t899)
      t908 = t791 * t762 + t793 * t627 + t795 * t703 + t797 * t762 - t87
     #5 * t871 * t878 / 0.720D3 - t881 * t871 * t878 / 0.720D3 + t885 * 
     #t627 + t887 * t787 + t889 * t762 + t891 * t703 - t900 * t896 * t87
     #8 / 0.720D3 - t904 * t896 * t878 / 0.720D3
      rrgg2qqbarht5s1e1 = t790 + t908

      end function



      doubleprecision function rrgg2qqbarht5s1e0
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t6 = t5 * pi
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = -0.1D1 + x3
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t10 * t13 * t15)
      t20 = t19 ** 2
      t21 = cos(t11)
      t23 = Sqrt(-x3 * t14)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t23 * z)
      t32 = log(0.4D1 * t10 * t13)
      t33 = t32 ** 2
      t34 = 0.1D1 / z
      t41 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t44 = t5 * lh
      t45 = pi * t7
      t53 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t59 = lh ** 2
      t61 = pi ** 2
      t63 = -0.180D3 * t59 + 0.30D2 * t61
      t64 = t5 * t63
      t70 = 0.1D1 / x3
      t73 = t34 * t53
      t74 = t9 * t13
      t76 = log(0.4D1 * t74)
      t77 = t76 * t34
      t79 = t76 ** 2
      t80 = t79 * t34
      t87 = t34 * t7
      t98 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t108 = t34 * t41
      t116 = x2 ** 2
      t117 = t116 * x3
      t118 = t74 * t15
      t121 = log(-0.4D1 * t117 * t118)
      t127 = log(0.4D1 * t117 * t74)
      t134 = t87 + t7 * t28
      t140 = 0.1D1 / x2
      t143 = t116 * t9
      t146 = log(0.4D1 * t143 * t13)
      t147 = t146 * t34
      t149 = t146 ** 2
      t161 = pi * t34
      t162 = t161 * t7
      t163 = t64 * t162
      t167 = x1 ** 2
      t168 = x3 * t167
      t171 = log(-0.4D1 * t168 * t118)
      t177 = log(0.4D1 * t168 * t74)
      t183 = -t134
      t189 = 0.1D1 / x1
      t194 = t70 * t140 * t189
      t197 = t116 * t167
      t200 = log(0.4D1 * t197 * t74)
      t212 = t5 * t34
      t213 = t167 * t9
      t216 = log(0.4D1 * t213 * t13)
      t218 = t216 ** 2
      t225 = lh * pi
      t234 = (-0.90D2 * t6 * t7 * (t20 * t28 / 0.2D1 + t33 * t34 / 0.2D1
     #) + (-0.90D2 * t6 * t41 + 0.180D3 * t44 * t45) * (-t19 * t28 - t32
     # * t34) + (-0.90D2 * t6 * t53 + 0.180D3 * t44 * pi * t41 + t64 * t
     #45) * (t28 + t34)) * t70 / 0.2880D4 + (0.180D3 * (t73 - t77 * t41 
     #+ t80 * t7 / 0.2D1) * t5 * lh + t87 * t5 * (-0.60D2 * lh * t61 + 0
     #.240D3 * zeta3 + 0.120D3 * t59 * lh) - 0.90D2 * (t80 * t41 / 0.2D1
     # + t34 * t98 - t79 * t76 * t34 * t7 / 0.6D1 - t77 * t53) * t5 + (t
     #108 - t77 * t7) * t5 * t63) * pi / 0.2880D4 + (-0.90D2 * t6 * ((t4
     #1 - t121 * t7) * t28 + t108 - t127 * t34 * t7) + 0.180D3 * t44 * p
     #i * t134) * t70 * t140 / 0.1440D4 + (-0.90D2 * t6 * (t73 - t147 * 
     #t41 + t149 * t34 * t7 / 0.2D1) + 0.180D3 * t44 * pi * (t108 - t147
     # * t7) + t163) * t140 / 0.1440D4 - (-0.90D2 * t6 * (-(t41 - t171 *
     # t7) * t28 - t108 + t177 * t34 * t7) + 0.180D3 * t44 * pi * t183) 
     #* t70 * t189 / 0.1440D4 + t6 * t183 * t194 / 0.8D1 + (-0.90D2 * t6
     # * (t108 - t200 * t34 * t7) + 0.180D3 * t44 * t162) * t140 * t189 
     #/ 0.720D3 + (-0.90D2 * t212 * pi * (t53 - t216 * t41 + t218 * t7 /
     # 0.2D1) + 0.180D3 * t212 * t225 * (t41 - t216 * t7) + t163) * t189
     # / 0.1440D4
      t235 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t234)
      t237 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t234)
      t239 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t234)
      t241 = t2 * x1
      t242 = -0.1D1 + x1
      t243 = x1 * z
      t244 = 0.1D1 - x1 + t243
      t245 = 0.1D1 / t244
      t247 = t2 * t242 * t245
      t248 = t1 ** 2
      t249 = s * t248
      t251 = x1 * t242 * t245
      t252 = t249 * t251
      t253 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t247
     #, t241, 0.0D0, -t252)
      t254 = t34 * t253
      t255 = t168 * t9
      t256 = t242 ** 2
      t257 = t13 * t256
      t258 = t257 * t245
      t261 = log(0.4D1 * t255 * t258)
      t263 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t247
     #, t241, 0.0D0, -t252)
      t270 = log(-0.4D1 * t255 * t257 * t245 * t15)
      t274 = t168 * t8
      t275 = x1 * t8
      t276 = x3 * t8
      t277 = t276 * x1
      t279 = 0.2D1 * t168 * z
      t280 = x3 * x1
      t281 = t280 * z
      t282 = 0.3D1 * t281
      t283 = 0.2D1 * t280
      t284 = x3 * t244
      t286 = Sqrt(-t284 * t14)
      t290 = -z - t274 - t275 + t277 + t279 + t243 - t282 - x3 - t168 + 
     #t283 + 0.2D1 * t21 * t286 * z
      t291 = 0.1D1 / t290
      t299 = t34 * t263 + t244 * t263 * t291
      t306 = (-0.90D2 * t6 * (t254 - t261 * t34 * t263 + (t244 * t253 - 
     #t270 * t244 * t263) * t291) + 0.180D3 * t44 * pi * t299) * t70 * t
     #189 / 0.1440D4
      t309 = t6 * t299 * t194 / 0.8D1
      t310 = t197 * t9
      t313 = log(0.4D1 * t310 * t258)
      t319 = t161 * t263
      t325 = (-0.90D2 * t6 * (-t254 + t313 * t34 * t263) - 0.180D3 * t44
     # * t319) * t140 * t189 / 0.720D3
      t326 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t247
     #, t241, 0.0D0, -t252)
      t329 = log(0.4D1 * t213 * t258)
      t331 = t329 ** 2
      t334 = -t326 + t329 * t253 - t331 * t263 / 0.2D1
      t339 = -t253 + t329 * t263
      t343 = t64 * t319
      t347 = -t306 + t309 + t325 + (-0.90D2 * t212 * pi * t334 + 0.180D3
     # * t212 * t225 * t339 - t343) * t189 / 0.1440D4
      t348 = FJET(XB1, XB2, s, 0.0D0, t241, -t247, 0.0D0, -t252, t347)
      t351 = x2 * t1 * s
      t352 = -0.1D1 + x2
      t354 = t352 * t1 * s
      t355 = x2 * z
      t357 = 0.1D1 / (-z + t355 - x2)
      t358 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t351, -t354,
     # 0.0D0, 0.0D0, 0.0D0)
      t359 = t357 * t358
      t360 = t74 * t352
      t363 = log(-0.4D1 * t117 * t360)
      t365 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t351, -t354,
     # 0.0D0, 0.0D0, 0.0D0)
      t371 = pi * t357 * t365
      t373 = 0.180D3 * t44 * t371
      t378 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, t351, -t354,
     # 0.0D0, 0.0D0, 0.0D0)
      t380 = t13 * t352
      t383 = log(-0.4D1 * t143 * t380)
      t384 = t383 * t357
      t386 = t383 ** 2
      t404 = t140 * t189
      t410 = log(-0.4D1 * t197 * t360)
      t420 = (-0.90D2 * t6 * (t359 - t363 * t357 * t365) + t373) * t70 *
     # t140 / 0.1440D4 + (-0.90D2 * t6 * (t357 * t378 - t384 * t358 + t3
     #86 * t357 * t365 / 0.2D1) + 0.180D3 * t44 * pi * (t359 - t384 * t3
     #65) + t64 * t371) * t140 / 0.1440D4 - t6 * t357 * t365 * t70 * t40
     #4 / 0.8D1 + (-0.90D2 * t6 * (t359 - t410 * t357 * t365) + t373) * 
     #t140 * t189 / 0.720D3
      t421 = FJET(XB1, XB2, s, 0.0D0, t351, 0.0D0, -t354, 0.0D0, t420)
      t423 = x2 * x3
      t426 = Sqrt(x3 * t352 * t14)
      t427 = t21 * t426
      t429 = 0.2D1 * t427 * x2
      t431 = 0.1D1 - x3 + t423
      t432 = 0.1D1 / t431
      t434 = t2 * (0.1D1 - x3 - x2 + t423 + t117 + t429) * t432
      t439 = t2 * x2 * (-0.1D1 + t423 + 0.2D1 * t427) * t432
      t440 = t423 * z
      t441 = t117 * z
      t447 = 0.1D1 / (z - t355 + x3 - t117 + x2 - t440 + t441 - t429 - 0
     #.2D1 * t427 * z + 0.2D1 * t427 * t355)
      t448 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t439, t434,
     # 0.0D0, 0.0D0, 0.0D0)
      t451 = t431 ** 2
      t457 = log(0.4D1 * t117 * t9 * t380 * t14 / t451)
      t459 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t439, t434,
     # 0.0D0, 0.0D0, 0.0D0)
      t477 = (-0.90D2 * t6 * (t447 * t448 - t457 * t447 * t459) + 0.180D
     #3 * t44 * pi * t447 * t459) * t70 * t140 / 0.1440D4 - t6 * t447 * 
     #t459 * t70 * t404 / 0.8D1
      t478 = FJET(XB1, XB2, s, 0.0D0, t434, 0.0D0, -t439, 0.0D0, t477)
      t481 = t1 * t242
      t483 = t352 * s * t481 * t245
      t485 = x2 * s * t481
      t487 = t249 * t352 * t251
      t488 = x2 * x1
      t489 = t488 * z
      t491 = 0.1D1 / (z - t355 - t488 + x2 + t489)
      t493 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t485, t483,
     # t241, 0.0D0, t487)
      t498 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t485, t483,
     # t241, 0.0D0, t487)
      t505 = log(-0.4D1 * t310 * t13 * t245 * t256 * t352)
      t519 = -t6 * t491 * t493 * t70 * t404 / 0.8D1 + (-0.90D2 * t6 * (t
     #491 * t498 - t505 * t491 * t493) + 0.180D3 * t44 * pi * t491 * t49
     #3) * t140 * t189 / 0.720D3
      t520 = FJET(XB1, XB2, s, 0.0D0, t483, t241, -t485, t487, t519)
      t522 = FJET(XB1, XB2, s, 0.0D0, -t354, 0.0D0, t351, 0.0D0, t420)
      t524 = FJET(XB1, XB2, s, 0.0D0, -t247, t241, 0.0D0, -t252, t347)
      t526 = FJET(XB1, XB2, s, 0.0D0, -t439, 0.0D0, t434, 0.0D0, t477)
      t528 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t234)
      t541 = -t306 + t309 + t325 + (-0.90D2 * t212 * pi * t334 + 0.180D3
     # * t212 * t225 * t339 - t343) * t189 / 0.1440D4
      t542 = FJET(XB1, XB2, s, t241, 0.0D0, 0.0D0, -t247, -t252, t541)
      t544 = t235 * t234 + t237 * t234 + t239 * t234 + t348 * t347 + t42
     #1 * t420 + t478 * t477 + t520 * t519 + t522 * t420 + t524 * t347 +
     # t526 * t477 + t528 * t234 + t542 * t541
      t545 = FJET(XB1, XB2, s, t241, -t485, 0.0D0, t483, t487, t519)
      t547 = FJET(XB1, XB2, s, t351, 0.0D0, -t354, 0.0D0, 0.0D0, t420)
      t549 = FJET(XB1, XB2, s, t434, 0.0D0, -t439, 0.0D0, 0.0D0, t477)
      t551 = FJET(XB1, XB2, s, t483, 0.0D0, -t485, t241, t487, t519)
      t554 = t241 * t423 * t432
      t555 = t2 * t242
      t556 = t117 * t243
      t557 = t117 * x1
      t560 = Sqrt(t284 * t352 * t14)
      t561 = t21 * t560
      t563 = 0.2D1 * t561 * x2
      t567 = t555 * (t556 - x2 + t423 - t557 + 0.1D1 - x3 + t117 + t563)
     # * t245 * t432
      t571 = t14 * s * t1 * x1 * t432
      t577 = t555 * x2 * (-0.1D1 + t423 + x1 - t280 - t243 + t281 + 0.2D
     #1 * t561) * t245 * t432
      t578 = x2 * t167
      t600 = -t578 + t556 + 0.2D1 * t423 * t243 - 0.2D1 * t168 * t355 - 
     #t276 * t488 + t168 * t8 * x2 - 0.2D1 * t561 * t355 - 0.2D1 * t561 
     #* t488 + 0.2D1 * t561 * t489 - t557 + t563 + t488 * t8 + 0.2D1 * t
     #578 * z - t578 * t8 + 0.2D1 * t561 * z + t168 * x2 - t423 * x1
      t603 = t355 + t243 - t275 - t168 + t283 - z + t117 + 0.2D1 * t488 
     #- t274 + t277 + t279 - t282 - x3 - x2 + t440 - t441 - 0.3D1 * t489
      t605 = 0.1D1 / (t600 + t603)
      t608 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t577, -t567,
     # -t571, t554, t487)
      t612 = t6 * t244 * t605 * t608 * t70 * t404 / 0.8D1
      t613 = FJET(XB1, XB2, s, t554, -t567, -t571, t577, t487, -t612)
      t615 = pi * t244
      t618 = t605 * t608 * t194
      t621 = FJET(XB1, XB2, s, t577, -t571, -t567, t554, t487, -t612)
      t626 = FJET(XB1, XB2, s, -t354, 0.0D0, t351, 0.0D0, 0.0D0, t420)
      t628 = FJET(XB1, XB2, s, -t247, 0.0D0, 0.0D0, t241, -t252, t541)
      t630 = FJET(XB1, XB2, s, -t485, t241, t483, 0.0D0, t487, t519)
      t632 = FJET(XB1, XB2, s, -t439, 0.0D0, t434, 0.0D0, 0.0D0, t477)
      t634 = FJET(XB1, XB2, s, -t571, t577, t554, -t567, t487, -t612)
      t639 = FJET(XB1, XB2, s, -t567, t554, t577, -t571, t487, -t612)
      t644 = t545 * t519 + t547 * t420 + t549 * t477 + t551 * t519 - t61
     #3 * t5 * t615 * t618 / 0.8D1 - t621 * t5 * t615 * t618 / 0.8D1 + t
     #626 * t420 + t628 * t541 + t630 * t519 + t632 * t477 - t634 * t5 *
     # t615 * t618 / 0.8D1 - t639 * t5 * t615 * t618 / 0.8D1
      rrgg2qqbarht5s1e0 = t544 + t644

      end function



      doubleprecision function rrgg2qqbarht5s1em1
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t6 = t5 * pi
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = -0.1D1 + x3
      t19 = log(-0.4D1 * t10 * t13 / t14)
      t20 = cos(t11)
      t22 = Sqrt(-x3 * t14)
      t27 = 0.1D1 / (-z - x3 + 0.2D1 * t20 * t22 * z)
      t31 = log(0.4D1 * t10 * t13)
      t32 = 0.1D1 / z
      t38 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t41 = t5 * lh
      t49 = 0.1D1 / x3
      t52 = t32 * t38
      t55 = log(0.4D1 * t9 * t13)
      t56 = t55 * t32
      t62 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t65 = t55 ** 2
      t72 = t32 * t7
      t73 = lh ** 2
      t75 = pi ** 2
      t84 = t72 + t7 * t27
      t86 = 0.1D1 / x2
      t90 = x2 ** 2
      t91 = t90 * t9
      t94 = log(0.4D1 * t91 * t13)
      t100 = pi * t32
      t103 = 0.180D3 * t41 * t100 * t7
      t107 = t6 * t32
      t109 = 0.1D1 / x1
      t113 = t5 * t32
      t114 = x1 ** 2
      t115 = t114 * t9
      t118 = log(0.4D1 * t115 * t13)
      t132 = (-0.90D2 * t6 * t7 * (-t19 * t27 - t31 * t32) + (-0.90D2 * 
     #t6 * t38 + 0.180D3 * t41 * pi * t7) * (t27 + t32)) * t49 / 0.2880D
     #4 + (0.180D3 * (t52 - t56 * t7) * t5 * lh - 0.90D2 * (t32 * t62 - 
     #t56 * t38 + t65 * t32 * t7 / 0.2D1) * t5 + t72 * t5 * (-0.180D3 * 
     #t73 + 0.30D2 * t75)) * pi / 0.2880D4 - t6 * t84 * t49 * t86 / 0.16
     #D2 + (-0.90D2 * t6 * (t52 - t94 * t32 * t7) + t103) * t86 / 0.1440
     #D4 - t107 * t7 * t86 * t109 / 0.8D1 + (-0.90D2 * t113 * pi * (t38 
     #- t118 * t7) + t103) * t109 / 0.1440D4 - t6 * t84 * t49 * t109 / 0
     #.16D2
      t133 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t132)
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t132)
      t137 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t132)
      t139 = t2 * x1
      t140 = -0.1D1 + x1
      t141 = x1 * z
      t142 = 0.1D1 - x1 + t141
      t143 = 0.1D1 / t142
      t145 = t2 * t140 * t143
      t146 = t1 ** 2
      t147 = s * t146
      t149 = x1 * t140 * t143
      t150 = t147 * t149
      t151 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t145
     #, t139, 0.0D0, -t150)
      t155 = t107 * t151 * t86 * t109 / 0.8D1
      t156 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t145
     #, t139, 0.0D0, -t150)
      t158 = t140 ** 2
      t162 = log(0.4D1 * t115 * t13 * t143 * t158)
      t164 = -t156 + t162 * t151
      t170 = 0.180D3 * t41 * t100 * t151
      t176 = x3 * t114
      t183 = x3 * x1
      t189 = Sqrt(-x3 * t142 * t14)
      t193 = -z - t176 * t8 - x1 * t8 + x3 * t8 * x1 + 0.2D1 * t176 * z 
     #+ t141 - 0.3D1 * t183 * z - x3 - t176 + 0.2D1 * t183 + 0.2D1 * t20
     # * t189 * z
      t200 = t6 * (t32 * t151 + t142 * t151 / t193) * t49 * t109 / 0.16D
     #2
      t201 = t155 + (-0.90D2 * t113 * pi * t164 - t170) * t109 / 0.1440D
     #4 + t200
      t202 = FJET(XB1, XB2, s, 0.0D0, t139, -t145, 0.0D0, -t150, t201)
      t205 = x2 * t1 * s
      t206 = -0.1D1 + x2
      t208 = t206 * t1 * s
      t209 = x2 * z
      t211 = 0.1D1 / (-z + t209 - x2)
      t212 = t6 * t211
      t213 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t205, -t208,
     # 0.0D0, 0.0D0, 0.0D0)
      t218 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t205, -t208,
     # 0.0D0, 0.0D0, 0.0D0)
      t223 = log(-0.4D1 * t91 * t13 * t206)
      t240 = -t212 * t213 * t49 * t86 / 0.16D2 + (-0.90D2 * t6 * (t211 *
     # t218 - t223 * t211 * t213) + 0.180D3 * t41 * pi * t211 * t213) * 
     #t86 / 0.1440D4 - t212 * t213 * t86 * t109 / 0.8D1
      t241 = FJET(XB1, XB2, s, 0.0D0, t205, 0.0D0, -t208, 0.0D0, t240)
      t243 = x2 * x3
      t244 = t90 * x3
      t247 = Sqrt(x3 * t206 * t14)
      t248 = t20 * t247
      t250 = 0.2D1 * t248 * x2
      t253 = 0.1D1 / (0.1D1 - x3 + t243)
      t255 = t2 * (0.1D1 - x3 - x2 + t243 + t244 + t250) * t253
      t260 = t2 * x2 * (-0.1D1 + t243 + 0.2D1 * t248) * t253
      t268 = 0.1D1 / (z - t209 + x3 - t244 + x2 - t243 * z + t244 * z - 
     #t250 - 0.2D1 * t248 * z + 0.2D1 * t248 * t209)
      t270 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t260, t255,
     # 0.0D0, 0.0D0, 0.0D0)
      t274 = t6 * t268 * t270 * t49 * t86 / 0.16D2
      t275 = FJET(XB1, XB2, s, 0.0D0, t255, 0.0D0, -t260, 0.0D0, -t274)
      t280 = t268 * t270 * t49 * t86
      t284 = t1 * t140
      t286 = t206 * s * t284 * t143
      t288 = x2 * s * t284
      t290 = t147 * t206 * t149
      t291 = x2 * x1
      t294 = 0.1D1 / (z - t209 - t291 + x2 + t291 * z)
      t296 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t288, t286,
     # t139, 0.0D0, t290)
      t300 = t6 * t294 * t296 * t86 * t109 / 0.8D1
      t301 = FJET(XB1, XB2, s, 0.0D0, t286, t139, -t288, t290, -t300)
      t306 = t294 * t296 * t86 * t109
      t309 = FJET(XB1, XB2, s, 0.0D0, -t208, 0.0D0, t205, 0.0D0, t240)
      t311 = FJET(XB1, XB2, s, 0.0D0, -t145, t139, 0.0D0, -t150, t201)
      t313 = FJET(XB1, XB2, s, 0.0D0, -t260, 0.0D0, t255, 0.0D0, -t274)
      t318 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t327 = t155 + (-0.90D2 * t113 * pi * t164 - t170) * t109 / 0.1440D
     #4 + t200
      t328 = FJET(XB1, XB2, s, t139, 0.0D0, 0.0D0, -t145, -t150, t327)
      t330 = FJET(XB1, XB2, s, t139, -t288, 0.0D0, t286, t290, -t300)
      t335 = FJET(XB1, XB2, s, t205, 0.0D0, -t208, 0.0D0, 0.0D0, t240)
      t337 = FJET(XB1, XB2, s, t255, 0.0D0, -t260, 0.0D0, 0.0D0, -t274)
      t342 = FJET(XB1, XB2, s, t286, 0.0D0, -t288, t139, t290, -t300)
      t347 = FJET(XB1, XB2, s, -t208, 0.0D0, t205, 0.0D0, 0.0D0, t240)
      t349 = FJET(XB1, XB2, s, -t145, 0.0D0, 0.0D0, t139, -t150, t327)
      t351 = FJET(XB1, XB2, s, -t288, t139, t286, 0.0D0, t290, -t300)
      t356 = FJET(XB1, XB2, s, -t260, 0.0D0, t255, 0.0D0, 0.0D0, -t274)
      rrgg2qqbarht5s1em1 = t133 * t132 + t135 * t132 + t137 * t132 + t20
     #2 * t201 + t241 * t240 - t275 * t5 * pi * t280 / 0.16D2 - t301 * t
     #5 * pi * t306 / 0.8D1 + t309 * t240 + t311 * t201 - t313 * t5 * pi
     # * t280 / 0.16D2 + t318 * t132 + t328 * t327 - t330 * t5 * pi * t3
     #06 / 0.8D1 + t335 * t240 - t337 * t5 * pi * t280 / 0.16D2 - t342 *
     # t5 * pi * t306 / 0.8D1 + t347 * t240 + t349 * t327 - t351 * t5 * 
     #pi * t306 / 0.8D1 - t356 * t5 * pi * t280 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s1em2
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t6 = t5 * pi
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t18 = 0.1D1 / z
      t25 = t18 * t7
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t37 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t39 = z ** 2
      t41 = Sin(t8)
      t42 = t41 ** 2
      t45 = log(0.4D1 / t39 * t42)
      t54 = -t6 * t7 * (0.1D1 / (-z - x3 + 0.2D1 * t9 * t12 * z) + t18) 
     #/ x3 / 0.32D2 - t6 * t25 * t26 / 0.16D2 - t6 * t25 * t30 / 0.16D2 
     #+ (0.180D3 * t25 * t5 * lh - 0.90D2 * (t18 * t37 - t45 * t18 * t7)
     # * t5) * pi / 0.2880D4
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t54)
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t54)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t54)
      t61 = t2 * x1
      t62 = -0.1D1 + x1
      t65 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t67 = t2 * t62 * t65
      t68 = t1 ** 2
      t72 = s * t68 * x1 * t62 * t65
      t73 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t67, 
     #t61, 0.0D0, -t72)
      t75 = t18 * t73 * t30
      t77 = t6 * t75 / 0.16D2
      t78 = FJET(XB1, XB2, s, 0.0D0, t61, -t67, 0.0D0, -t72, t77)
      t84 = x2 * t1 * s
      t87 = (-0.1D1 + x2) * t1 * s
      t91 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t84, -t87, 0.
     #0D0, 0.0D0, 0.0D0)
      t93 = 0.1D1 / (-z + x2 * z - x2) * t91 * t26
      t95 = t6 * t93 / 0.16D2
      t96 = FJET(XB1, XB2, s, 0.0D0, t84, 0.0D0, -t87, 0.0D0, -t95)
      t101 = FJET(XB1, XB2, s, 0.0D0, -t87, 0.0D0, t84, 0.0D0, -t95)
      t106 = FJET(XB1, XB2, s, 0.0D0, -t67, t61, 0.0D0, -t72, t77)
      t111 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t54)
      t113 = FJET(XB1, XB2, s, t61, 0.0D0, 0.0D0, -t67, -t72, t77)
      t118 = FJET(XB1, XB2, s, t84, 0.0D0, -t87, 0.0D0, 0.0D0, -t95)
      t123 = FJET(XB1, XB2, s, -t87, 0.0D0, t84, 0.0D0, 0.0D0, -t95)
      t128 = FJET(XB1, XB2, s, -t67, 0.0D0, 0.0D0, t61, -t72, t77)
      rrgg2qqbarht5s1em2 = t55 * t54 + t57 * t54 + t59 * t54 + t78 * t5 
     #* pi * t75 / 0.16D2 - t96 * t5 * pi * t93 / 0.16D2 - t101 * t5 * p
     #i * t93 / 0.16D2 + t106 * t5 * pi * t75 / 0.16D2 + t111 * t54 + t1
     #13 * t5 * pi * t75 / 0.16D2 - t118 * t5 * pi * t93 / 0.16D2 - t123
     # * t5 * pi * t93 / 0.16D2 + t128 * t5 * pi * t75 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s1em3
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t7 = 0.1D1 / z
      t8 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t11 = t5 * pi * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = pi * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2qqbarht5s1em3 = -t12 * t5 * t15 / 0.32D2 - t17 * t5 * t15 / 0
     #.32D2 - t20 * t5 * t15 / 0.32D2 - t23 * t5 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht5s1em4
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht5s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht5s2e1
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t6 = t5 * pi
      t7 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t23 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t25 = t22 ** 2
      t26 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t30 = cos(t13)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t17)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t39 = 0.1D1 / z
      t40 = t39 * t7
      t43 = log(0.4D1 * t9 * t16)
      t44 = t43 * t39
      t46 = t43 ** 2
      t53 = t5 * lh
      t57 = t39 * t23
      t63 = lh ** 2
      t65 = pi ** 2
      t67 = -0.180D3 * t63 + 0.30D2 * t65
      t68 = t5 * t67
      t69 = t39 * t26
      t75 = 0.1D1 / x3
      t77 = 0.1D1 / x1
      t80 = t5 * t39
      t81 = t67 * pi
      t82 = t8 * t12
      t83 = t82 * t15
      t85 = log(0.4D1 * t83)
      t90 = t85 ** 2
      t93 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t107 = -0.60D2 * lh * t65 + 0.240D3 * zeta3 + 0.120D3 * t63 * lh
      t108 = t107 * pi
      t111 = lh * pi
      t122 = x2 ** 2
      t123 = x3 * t122
      t124 = t123 * t8
      t125 = -0.1D1 + x2
      t126 = t16 * t125
      t129 = log(-0.4D1 * t124 * t126)
      t134 = log(0.4D1 * t123 * t83)
      t139 = log(-0.4D1 * t124 * t19)
      t147 = pi * t26 * t37
      t152 = 0.1D1 / x2
      t153 = t152 * t77
      t156 = t122 * t8
      t159 = log(-0.4D1 * t156 * t126)
      t160 = t159 * t39
      t162 = t159 ** 2
      t168 = log(0.4D1 * t156 * t16)
      t169 = t168 * t39
      t171 = t168 ** 2
      t188 = t23 * t5
      t191 = t7 * t5
      t193 = t26 * t5
      t197 = x3 * t12
      t200 = log(0.4D1 * t197 * t15)
      t205 = log(-0.4D1 * t197 * t15 * t18)
      t209 = t205 ** 2
      t212 = t200 ** 2
      t245 = log(0.4D1 * t123 * t16)
      t246 = t245 * t39
      t248 = t245 ** 2
      t254 = log(-0.4D1 * t123 * t19)
      t256 = t254 ** 2
      t263 = log(-0.4D1 * t123 * t126)
      t264 = t263 * t39
      t266 = t263 ** 2
      t293 = t12 * t122
      t296 = log(0.4D1 * t293 * t15)
      t297 = t296 ** 2
      t298 = t15 * t125
      t301 = log(-0.4D1 * t293 * t298)
      t302 = t301 ** 2
      t326 = log(0.4D1 * t16)
      t327 = t326 ** 2
      t328 = t327 * t39
      t333 = t327 * t326 * t39
      t336 = t326 * t39
      t354 = t65 ** 2
      t355 = t63 ** 2
      t362 = rrgg2qqbarh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t369 = t327 ** 2
      t379 = (-0.90D2 * t6 * ((t7 - t22 * t23 + t25 * t26 / 0.2D1) * t37
     # + t40 - t44 * t23 + t46 * t39 * t26 / 0.2D1) + 0.180D3 * t53 * pi
     # * ((t23 - t22 * t26) * t37 + t57 - t44 * t26) + t68 * pi * (t69 +
     # t26 * t37)) * t75 * t77 / 0.1440D4 - (t80 * t81 * (-t23 + t85 * t
     #26) - 0.90D2 * t80 * pi * (-t90 * t23 / 0.2D1 - t93 + t90 * t85 * 
     #t26 / 0.6D1 + t85 * t7) - t80 * t108 * t26 + 0.180D3 * t80 * t111 
     #* (-t7 + t85 * t23 - t90 * t26 / 0.2D1)) * t77 / 0.1440D4 + (-0.90
     #D2 * t6 * (t129 * t39 * t26 - t134 * t39 * t26 + (t23 - t139 * t26
     #) * t37) + 0.180D3 * t53 * t147) * t75 * t153 / 0.720D3 + (-0.90D2
     # * t6 * (t160 * t23 - t162 * t39 * t26 / 0.2D1 - t169 * t23 + t171
     # * t39 * t26 / 0.2D1) + 0.180D3 * t53 * pi * (t160 * t26 - t169 * 
     #t26)) * t152 * t77 / 0.720D3 - ((0.180D3 * t188 * lh - 0.90D2 * t1
     #91 + t193 * t67) * pi * (t200 * t39 + t205 * t37) - 0.90D2 * t193 
     #* pi * (t209 * t205 * t37 / 0.6D1 + t212 * t200 * t39 / 0.6D1) + (
     #0.180D3 * t191 * lh + t193 * t107 - 0.90D2 * t93 * t5 + t188 * t67
     #) * pi * (-t39 - t37) + (0.180D3 * t193 * lh - 0.90D2 * t188) * pi
     # * (-t212 * t39 / 0.2D1 - t209 * t37 / 0.2D1)) * t75 / 0.2880D4 - 
     #(-0.90D2 * t6 * (t246 * t23 - t248 * t39 * t26 / 0.2D1 - (t7 - t25
     #4 * t23 + t256 * t26 / 0.2D1) * t37 - t264 * t23 + t266 * t39 * t2
     #6 / 0.2D1) + 0.180D3 * t53 * pi * (t246 * t26 - (t23 - t254 * t26)
     # * t37 - t264 * t26) - t68 * t147) * t75 * t152 / 0.1440D4 + ((0.1
     #80D3 * t69 * t53 - 0.90D2 * t57 * t5) * pi * (t297 / 0.2D1 - t302 
     #/ 0.2D1) - 0.90D2 * t69 * t6 * (t302 * t301 / 0.6D1 - t297 * t296 
     #/ 0.6D1) + (0.180D3 * t57 * t53 - 0.90D2 * t40 * t5 + t69 * t68) *
     # pi * (-t296 + t301)) * t152 / 0.1440D4 + (0.180D3 * (t328 * t23 /
     # 0.2D1 + t39 * t93 - t333 * t26 / 0.6D1 - t336 * t7) * t5 * lh + (
     #t40 - t336 * t23 + t328 * t26 / 0.2D1) * t5 * t67 + (t57 - t336 * 
     #t26) * t5 * t107 + t69 * t5 * (-0.480D3 * lh * zeta3 - t354 - 0.60
     #D2 * t355 + 0.60D2 * t63 * t65) - 0.90D2 * (t39 * t362 - t333 * t2
     #3 / 0.6D1 + t328 * t7 / 0.2D1 - t336 * t93 + t369 * t39 * t26 / 0.
     #24D2) * t5) * pi / 0.2880D4
      t380 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t379)
      t382 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t379)
      t384 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t379)
      t386 = x2 * x3
      t387 = 0.1D1 - x3 + t386
      t388 = 0.1D1 / t387
      t389 = t386 * t388
      t390 = t2 * t389
      t392 = t2 * t17 * t388
      t393 = t125 * t17
      t395 = Sqrt(t31 * t393)
      t399 = 0.1D1 / (-z - x3 + t386 + 0.2D1 * t30 * t395)
      t400 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t392, t390, 0.0D0)
      t403 = t387 ** 2
      t404 = 0.1D1 / t403
      t406 = t298 * t17 * t404
      t409 = log(0.4D1 * t123 * t12 * t406)
      t410 = t409 * t399
      t411 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t392, t390, 0.0D0)
      t413 = t409 ** 2
      t415 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t392, t390, 0.0D0)
      t421 = t399 * t411
      t428 = pi * t399 * t415
      t437 = log(0.4D1 * t123 * t82 * t406)
      t449 = -(-0.90D2 * t6 * (t399 * t400 - t410 * t411 + t413 * t399 *
     # t415 / 0.2D1) + 0.180D3 * t53 * pi * (t421 - t410 * t415) + t68 *
     # t428) * t75 * t152 / 0.1440D4 + (-0.90D2 * t6 * (-t421 + t437 * t
     #399 * t415) - 0.180D3 * t53 * t428) * t75 * t153 / 0.720D3
      t450 = FJET(XB1, XB2, s, 0.0D0, t390, 0.0D0, -t392, 0.0D0, t449)
      t453 = t1 * x1
      t454 = x1 * z
      t455 = -z - x1 + t454
      t456 = 0.1D1 / t455
      t458 = t125 * s * t453 * t456
      t459 = -0.1D1 + x1
      t460 = t2 * t459
      t462 = x2 * s * t453
      t463 = t1 ** 2
      t464 = s * t463
      t467 = x1 * t459 * t456
      t468 = t464 * t125 * t467
      t469 = x2 * x1
      t470 = t469 * z
      t472 = 0.1D1 / (-z - t469 + t470)
      t473 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t462, t458, 
     #-t460, 0.0D0, -t468)
      t474 = t472 * t473
      t475 = 0.1D1 / t10
      t476 = t8 * t475
      t477 = t123 * t476
      t478 = t459 ** 2
      t479 = t15 * t478
      t481 = t479 * t456 * t125
      t484 = log(0.4D1 * t477 * t481)
      t486 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t462, t458, 
     #-t460, 0.0D0, -t468)
      t492 = pi * t472 * t486
      t498 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, t462, t458, 
     #-t460, 0.0D0, -t468)
      t500 = t156 * t475
      t503 = log(0.4D1 * t500 * t481)
      t504 = t503 * t472
      t506 = t503 ** 2
      t523 = (-0.90D2 * t6 * (-t474 + t484 * t472 * t486) - 0.180D3 * t5
     #3 * t492) * t75 * t153 / 0.720D3 + (-0.90D2 * t6 * (-t472 * t498 +
     # t504 * t473 - t506 * t472 * t486 / 0.2D1) + 0.180D3 * t53 * pi * 
     #(-t474 + t504 * t486) - t68 * t492) * t152 * t77 / 0.720D3
      t524 = FJET(XB1, XB2, s, 0.0D0, t458, -t460, t462, -t468, t523)
      t527 = t2 * x1 * t456
      t528 = t464 * t467
      t529 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t527
     #, -t460, 0.0D0, t528)
      t530 = t39 * t529
      t531 = t9 * t475
      t532 = t479 * t456
      t535 = log(-0.4D1 * t531 * t532)
      t536 = t535 * t39
      t537 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t527
     #, -t460, 0.0D0, t528)
      t539 = t535 ** 2
      t541 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t527
     #, -t460, 0.0D0, t528)
      t546 = t479 * t456 * t18
      t549 = log(0.4D1 * t531 * t546)
      t550 = t549 * t455
      t552 = t549 ** 2
      t557 = x3 * x1
      t558 = t557 * z
      t559 = x1 * t10
      t560 = x3 * t10
      t561 = t560 * x1
      t562 = t9 * t10
      t564 = 0.2D1 * t9 * z
      t565 = x3 * t455
      t567 = Sqrt(t565 * t17)
      t572 = 0.1D1 / (-t454 - t558 + t559 + t561 - t562 - t9 - t31 + t56
     #4 - t10 + 0.2D1 * t30 * t567 * z)
      t577 = t39 * t537
      t579 = t455 * t537
      t591 = pi * (-t39 * t541 + t455 * t541 * t572)
      t596 = (-0.90D2 * t6 * (-t530 + t536 * t537 - t539 * t39 * t541 / 
     #0.2D1 + (t455 * t529 - t550 * t537 + t552 * t455 * t541 / 0.2D1) *
     # t572) + 0.180D3 * t53 * pi * (-t577 + t536 * t541 + (t579 - t550 
     #* t541) * t572) + t68 * t591) * t75 * t77 / 0.1440D4
      t599 = log(-0.4D1 * t476 * t532)
      t604 = t599 ** 2
      t607 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t527
     #, -t460, 0.0D0, t528)
      t627 = (t80 * t81 * (t537 - t599 * t541) - 0.90D2 * t80 * pi * (t6
     #04 * t537 / 0.2D1 + t607 - t604 * t599 * t541 / 0.6D1 - t599 * t52
     #9) + t80 * t108 * t541 + 0.180D3 * t80 * t111 * (t529 - t599 * t53
     #7 + t604 * t541 / 0.2D1)) * t77 / 0.1440D4
      t630 = log(0.4D1 * t477 * t546)
      t636 = t478 * t456
      t640 = log(-0.4D1 * t124 * t475 * t15 * t636)
      t651 = (-0.90D2 * t6 * ((t579 - t630 * t455 * t541) * t572 - t577 
     #+ t640 * t39 * t541) + 0.180D3 * t53 * t591) * t75 * t153 / 0.720D
     #3
      t654 = log(-0.4D1 * t500 * t532)
      t655 = t654 * t39
      t657 = t654 ** 2
      t661 = -t530 + t655 * t537 - t657 * t39 * t541 / 0.2D1
      t665 = -t577 + t655 * t541
      t671 = t68 * pi * t39 * t541
      t676 = t596 - t627 + t651 + (-0.90D2 * t6 * t661 + 0.180D3 * t53 *
     # pi * t665 - t671) * t152 * t77 / 0.720D3
      t677 = FJET(XB1, XB2, s, 0.0D0, -t460, -t527, 0.0D0, t528, t676)
      t679 = FJET(XB1, XB2, s, 0.0D0, -t527, -t460, 0.0D0, t528, t676)
      t681 = FJET(XB1, XB2, s, 0.0D0, -t392, 0.0D0, t390, 0.0D0, t449)
      t683 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t379)
      t685 = FJET(XB1, XB2, s, t462, -t460, t458, 0.0D0, -t468, t523)
      t687 = FJET(XB1, XB2, s, t390, 0.0D0, -t392, 0.0D0, 0.0D0, t449)
      t689 = FJET(XB1, XB2, s, t458, 0.0D0, t462, -t460, -t468, t523)
      t694 = t17 * s * t1 * t459 * t388
      t695 = t2 * x1
      t697 = Sqrt(-t565 * t393)
      t698 = t30 * t697
      t704 = t695 * x2 * (-x3 + t386 - z + t31 - x1 + t557 + t454 - t558
     # + 0.2D1 * t698) * t456 * t388
      t705 = t460 * t389
      t706 = t123 * t454
      t709 = t123 * x1
      t714 = t695 * (-t706 - x2 + t386 + 0.2D1 * t698 * x2 + t709 + t123
     # * z + 0.1D1 - x3) * t456 * t388
      t717 = x2 * t8
      t718 = 0.2D1 * t698 * t470 + t10 + t717 - t559 + t9 + t31 + t470 +
     # t558 - t561 + t562 - t564 + t454 - t709
      t738 = -0.2D1 * t698 * z - t469 * t10 - 0.2D1 * t717 * z + t717 * 
     #t10 - t9 * x2 - t386 * z + t386 * x1 + t706 - 0.2D1 * t386 * t454 
     #+ 0.2D1 * t9 * x2 * z + t560 * t469 - t9 * t10 * x2 - 0.2D1 * t698
     # * t469
      t740 = 0.1D1 / (t718 + t738)
      t741 = t455 * t740
      t742 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t704, -t714,
     # t694, -t705, -t468)
      t750 = log(-0.4D1 * t123 * t476 * t15 * t636 * t393 * t404)
      t752 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t704, -t714,
     # t694, -t705, -t468)
      t762 = -0.90D2 * t6 * (t741 * t742 - t750 * t455 * t740 * t752) + 
     #0.180D3 * t53 * pi * t741 * t752
      t765 = t762 * t75 * t153 / 0.720D3
      t766 = FJET(XB1, XB2, s, t694, t704, -t705, -t714, -t468, t765)
      t769 = t75 * t152 * t77
      t772 = FJET(XB1, XB2, s, t704, t694, -t714, -t705, -t468, t765)
      t776 = FJET(XB1, XB2, s, -t460, 0.0D0, 0.0D0, -t527, t528, t676)
      t778 = FJET(XB1, XB2, s, -t460, t462, 0.0D0, t458, -t468, t523)
      t791 = t596 - t627 + t651 + (-0.90D2 * t6 * t661 + 0.180D3 * t53 *
     # pi * t665 - t671) * t152 * t77 / 0.720D3
      t792 = FJET(XB1, XB2, s, -t527, 0.0D0, 0.0D0, -t460, t528, t791)
      t794 = FJET(XB1, XB2, s, -t392, 0.0D0, t390, 0.0D0, 0.0D0, t449)
      t796 = FJET(XB1, XB2, s, -t714, -t705, t704, t694, -t468, t765)
      t800 = FJET(XB1, XB2, s, -t705, -t714, t694, t704, -t468, t765)
      rrgg2qqbarht5s2e1 = t380 * t379 + t382 * t379 + t384 * t379 + t450
     # * t449 + t524 * t523 + t677 * t676 + t679 * t676 + t681 * t449 + 
     #t683 * t379 + t685 * t523 + t687 * t449 + t689 * t523 + t766 * t76
     #2 * t769 / 0.720D3 + t772 * t762 * t769 / 0.720D3 + t776 * t676 + 
     #t778 * t523 + t792 * t791 + t794 * t449 + t796 * t762 * t769 / 0.7
     #20D3 + t800 * t762 * t769 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht5s2e0
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t6 = t5 * pi
      t7 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t23 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t26 = cos(t13)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t17)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t35 = 0.1D1 / z
      t36 = t35 * t7
      t39 = log(0.4D1 * t9 * t16)
      t45 = t5 * lh
      t46 = t35 * t23
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t60 = 0.1D1 / x2
      t61 = t60 * t55
      t65 = x2 ** 2
      t66 = t65 * t8
      t67 = -0.1D1 + x2
      t68 = t16 * t67
      t71 = log(-0.4D1 * t66 * t68)
      t76 = log(0.4D1 * t66 * t16)
      t84 = t5 * t35
      t85 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t89 = log(0.4D1 * t8 * t12 * t15)
      t91 = t89 ** 2
      t98 = lh * pi
      t104 = lh ** 2
      t106 = pi ** 2
      t108 = -0.180D3 * t104 + 0.30D2 * t106
      t115 = t23 * t5
      t116 = x3 * t12
      t119 = log(0.4D1 * t116 * t15)
      t120 = t119 ** 2
      t125 = log(-0.4D1 * t116 * t15 * t18)
      t126 = t125 ** 2
      t135 = t7 * t5
      t157 = log(0.4D1 * t16)
      t158 = t157 * t35
      t160 = t157 ** 2
      t161 = t160 * t35
      t178 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t195 = x3 * t65
      t198 = log(0.4D1 * t195 * t16)
      t203 = log(-0.4D1 * t195 * t19)
      t209 = log(-0.4D1 * t195 * t68)
      t223 = t12 * t65
      t226 = log(0.4D1 * t223 * t15)
      t227 = t226 ** 2
      t228 = t15 * t67
      t231 = log(-0.4D1 * t223 * t228)
      t232 = t231 ** 2
      t249 = (-0.90D2 * t6 * ((t7 - t22 * t23) * t33 + t36 - t39 * t35 *
     # t23) + 0.180D3 * t45 * pi * (t46 + t23 * t33)) * t53 * t55 / 0.14
     #40D4 - t6 * t23 * t33 * t53 * t61 / 0.8D1 - t6 * (t71 * t35 * t23 
     #- t76 * t35 * t23) * t60 * t55 / 0.8D1 - (-0.90D2 * t84 * pi * (-t
     #85 + t89 * t7 - t91 * t23 / 0.2D1) + 0.180D3 * t84 * t98 * (-t7 + 
     #t89 * t23) - t84 * t108 * pi * t23) * t55 / 0.1440D4 - (-0.90D2 * 
     #t115 * pi * (-t120 * t35 / 0.2D1 - t126 * t33 / 0.2D1) + (0.180D3 
     #* t115 * lh - 0.90D2 * t135) * pi * (t119 * t35 + t125 * t33) + (0
     #.180D3 * t135 * lh - 0.90D2 * t85 * t5 + t115 * t108) * pi * (-t35
     # - t33)) * t53 / 0.2880D4 + (0.180D3 * (t35 * t85 - t158 * t7 + t1
     #61 * t23 / 0.2D1) * t5 * lh + t46 * t5 * (-0.60D2 * lh * t106 + 0.
     #240D3 * zeta3 + 0.120D3 * t104 * lh) - 0.90D2 * (t161 * t7 / 0.2D1
     # + t35 * t178 - t160 * t157 * t35 * t23 / 0.6D1 - t158 * t85) * t5
     # + (t36 - t158 * t23) * t5 * t108) * pi / 0.2880D4 - (-0.90D2 * t6
     # * (t198 * t35 * t23 - (t7 - t203 * t23) * t33 - t209 * t35 * t23)
     # - 0.180D3 * t45 * pi * t23 * t33) * t53 * t60 / 0.1440D4 + (-0.90
     #D2 * t46 * t6 * (t227 / 0.2D1 - t232 / 0.2D1) + (0.180D3 * t46 * t
     #45 - 0.90D2 * t36 * t5) * pi * (-t226 + t231)) * t60 / 0.1440D4
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t249)
      t252 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t249)
      t254 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t249)
      t256 = x2 * x3
      t257 = 0.1D1 - x3 + t256
      t258 = 0.1D1 / t257
      t259 = t256 * t258
      t260 = t2 * t259
      t262 = t2 * t17 * t258
      t263 = t67 * t17
      t265 = Sqrt(t27 * t263)
      t269 = 0.1D1 / (-z - x3 + t256 + 0.2D1 * t26 * t265)
      t271 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t262, t260, 0.0D0)
      t276 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t262, t260, 0.0D0)
      t279 = t257 ** 2
      t285 = log(0.4D1 * t195 * t12 * t228 * t17 / t279)
      t299 = t6 * t269 * t271 * t53 * t61 / 0.8D1 - (-0.90D2 * t6 * (t26
     #9 * t276 - t285 * t269 * t271) + 0.180D3 * t45 * pi * t269 * t271)
     # * t53 * t60 / 0.1440D4
      t300 = FJET(XB1, XB2, s, 0.0D0, t260, 0.0D0, -t262, 0.0D0, t299)
      t303 = t1 * x1
      t304 = x1 * z
      t305 = -z - x1 + t304
      t306 = 0.1D1 / t305
      t308 = t67 * s * t303 * t306
      t309 = -0.1D1 + x1
      t310 = t2 * t309
      t312 = x2 * s * t303
      t313 = t1 ** 2
      t314 = s * t313
      t317 = x1 * t309 * t306
      t318 = t314 * t67 * t317
      t319 = x2 * x1
      t320 = t319 * z
      t322 = 0.1D1 / (-z - t319 + t320)
      t324 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t312, t308, 
     #-t310, 0.0D0, -t318)
      t329 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, t312, t308, 
     #-t310, 0.0D0, -t318)
      t331 = 0.1D1 / t10
      t332 = t66 * t331
      t334 = t309 ** 2
      t339 = log(0.4D1 * t332 * t15 * t306 * t334 * t67)
      t353 = t6 * t322 * t324 * t53 * t61 / 0.8D1 + (-0.90D2 * t6 * (-t3
     #22 * t329 + t339 * t322 * t324) - 0.180D3 * t45 * pi * t322 * t324
     #) * t60 * t55 / 0.720D3
      t354 = FJET(XB1, XB2, s, 0.0D0, t308, -t310, t312, -t318, t353)
      t357 = t2 * x1 * t306
      t358 = t314 * t317
      t359 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t357
     #, -t310, 0.0D0, t358)
      t360 = t35 * t359
      t361 = t9 * t331
      t362 = t15 * t334
      t363 = t362 * t306
      t366 = log(-0.4D1 * t361 * t363)
      t368 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t357
     #, -t310, 0.0D0, t358)
      t375 = log(0.4D1 * t361 * t362 * t306 * t18)
      t379 = x3 * x1
      t380 = t379 * z
      t381 = x1 * t10
      t382 = x3 * t10
      t383 = t382 * x1
      t384 = t9 * t10
      t386 = 0.2D1 * t9 * z
      t387 = x3 * t305
      t389 = Sqrt(t387 * t17)
      t394 = 0.1D1 / (-t304 - t380 + t381 + t383 - t384 - t9 - t27 + t38
     #6 - t10 + 0.2D1 * t26 * t389 * z)
      t402 = -t35 * t368 + t305 * t368 * t394
      t409 = (-0.90D2 * t6 * (-t360 + t366 * t35 * t368 + (t305 * t359 -
     # t375 * t305 * t368) * t394) + 0.180D3 * t45 * pi * t402) * t53 * 
     #t55 / 0.1440D4
      t412 = t53 * t60 * t55
      t414 = t6 * t402 * t412 / 0.8D1
      t417 = log(-0.4D1 * t332 * t363)
      t420 = -t360 + t417 * t35 * t368
      t424 = pi * t35 * t368
      t426 = 0.180D3 * t45 * t424
      t431 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t357
     #, -t310, 0.0D0, t358)
      t435 = log(-0.4D1 * t8 * t331 * t363)
      t437 = t435 ** 2
      t453 = (-0.90D2 * t84 * pi * (t431 - t435 * t359 + t437 * t368 / 0
     #.2D1) + 0.180D3 * t84 * t98 * (t359 - t435 * t368) + t5 * t108 * t
     #424) * t55 / 0.1440D4
      t454 = t409 - t414 + (-0.90D2 * t6 * t420 - t426) * t60 * t55 / 0.
     #720D3 - t453
      t455 = FJET(XB1, XB2, s, 0.0D0, -t310, -t357, 0.0D0, t358, t454)
      t457 = FJET(XB1, XB2, s, 0.0D0, -t357, -t310, 0.0D0, t358, t454)
      t459 = FJET(XB1, XB2, s, 0.0D0, -t262, 0.0D0, t260, 0.0D0, t299)
      t461 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t249)
      t463 = FJET(XB1, XB2, s, t312, -t310, t308, 0.0D0, -t318, t353)
      t465 = FJET(XB1, XB2, s, t260, 0.0D0, -t262, 0.0D0, 0.0D0, t299)
      t467 = FJET(XB1, XB2, s, t308, 0.0D0, t312, -t310, -t318, t353)
      t472 = t17 * s * t1 * t309 * t258
      t473 = t2 * x1
      t475 = Sqrt(-t387 * t263)
      t476 = t26 * t475
      t482 = t473 * x2 * (-x3 + t256 - z + t27 - x1 + t379 + t304 - t380
     # + 0.2D1 * t476) * t306 * t258
      t483 = t310 * t259
      t484 = t195 * t304
      t487 = t195 * x1
      t492 = t473 * (-t484 - x2 + t256 + 0.2D1 * t476 * x2 + t487 + t195
     # * z + 0.1D1 - x3) * t306 * t258
      t505 = t484 - 0.2D1 * t256 * t304 + 0.2D1 * t9 * x2 * z + t382 * t
     #319 - t9 * t10 * x2 - 0.2D1 * t476 * t319 + 0.2D1 * t476 * t320 + 
     #t320 + t380 - t383 + t384 - t386 - t381
      t509 = x2 * t8
      t516 = t9 + t27 + t304 - t487 - 0.2D1 * t476 * z - t319 * t10 - 0.
     #2D1 * t509 * z + t509 * t10 - t9 * x2 - t256 * z + t256 * x1 + t50
     #9 + t10
      t518 = 0.1D1 / (t505 + t516)
      t521 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t482, -t492,
     # t472, -t483, -t318)
      t525 = t6 * t305 * t518 * t521 * t53 * t61 / 0.8D1
      t526 = FJET(XB1, XB2, s, t472, t482, -t483, -t492, -t318, -t525)
      t528 = pi * t305
      t531 = t518 * t521 * t412
      t534 = FJET(XB1, XB2, s, t482, t472, -t492, -t483, -t318, -t525)
      t539 = FJET(XB1, XB2, s, -t310, 0.0D0, 0.0D0, -t357, t358, t454)
      t541 = FJET(XB1, XB2, s, -t310, t312, 0.0D0, t308, -t318, t353)
      t550 = t409 - t414 + (-0.90D2 * t6 * t420 - t426) * t60 * t55 / 0.
     #720D3 - t453
      t551 = FJET(XB1, XB2, s, -t357, 0.0D0, 0.0D0, -t310, t358, t550)
      t553 = FJET(XB1, XB2, s, -t262, 0.0D0, t260, 0.0D0, 0.0D0, t299)
      t555 = FJET(XB1, XB2, s, -t492, -t483, t482, t472, -t318, -t525)
      t560 = FJET(XB1, XB2, s, -t483, -t492, t472, t482, -t318, -t525)
      rrgg2qqbarht5s2e0 = t250 * t249 + t252 * t249 + t254 * t249 + t300
     # * t299 + t354 * t353 + t455 * t454 + t457 * t454 + t459 * t299 + 
     #t461 * t249 + t463 * t353 + t465 * t299 + t467 * t353 - t526 * t5 
     #* t528 * t531 / 0.8D1 - t534 * t5 * t528 * t531 / 0.8D1 + t539 * t
     #454 + t541 * t353 + t551 * t550 + t553 * t299 - t555 * t5 * t528 *
     # t531 / 0.8D1 - t560 * t5 * t528 * t531 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht5s2em1
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t3 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t17 = log(0.4D1 * t11 * t14)
      t18 = 0.1D1 / z
      t20 = -0.1D1 + x3
      t25 = log(-0.4D1 * t11 * t14 / t20)
      t26 = cos(t12)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t20)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t41 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t49 = 0.1D1 / x3
      t52 = t6 * t18
      t53 = x1 ** 2
      t57 = log(0.4D1 * t53 * t10 * t14)
      t68 = 0.1D1 / x1
      t71 = t6 * pi
      t72 = t18 * t3
      t81 = 0.1D1 / x2
      t86 = x2 ** 2
      t87 = t10 * t86
      t90 = log(0.4D1 * t87 * t14)
      t91 = -0.1D1 + x2
      t95 = log(-0.4D1 * t87 * t14 * t91)
      t104 = log(0.4D1 * t10 * t14)
      t105 = t104 * t18
      t111 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t114 = t104 ** 2
      t121 = lh ** 2
      t123 = pi ** 2
      t131 = -(-0.90D2 * t7 * pi * (t17 * t18 + t25 * t33) + (0.180D3 * 
     #t7 * lh - 0.90D2 * t41 * t6) * pi * (-t18 - t33)) * t49 / 0.2880D4
     # - (-0.90D2 * t52 * pi * (-t41 + t57 * t3) - 0.180D3 * t52 * lh * 
     #pi * t3) * t68 / 0.1440D4 - t71 * (t72 + t3 * t33) * t49 * t68 / 0
     #.16D2 - t71 * t3 * t33 * t49 * t81 / 0.16D2 - t72 * t6 * pi * (-t9
     #0 + t95) * t81 / 0.16D2 + (0.180D3 * (t18 * t41 - t105 * t3) * t6 
     #* lh - 0.90D2 * (t18 * t111 - t105 * t41 + t114 * t18 * t3 / 0.2D1
     #) * t6 + t72 * t6 * (-0.180D3 * t121 + 0.30D2 * t123)) * pi / 0.28
     #80D4
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t131)
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t131)
      t136 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t131)
      t138 = x2 * x3
      t140 = 0.1D1 / (0.1D1 - x3 + t138)
      t142 = t2 * t138 * t140
      t144 = t2 * t20 * t140
      t147 = Sqrt(t27 * t91 * t20)
      t151 = 0.1D1 / (-z - x3 + t138 + 0.2D1 * t26 * t147)
      t153 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t144, t142, 0.0D0)
      t157 = t71 * t151 * t153 * t49 * t81 / 0.16D2
      t158 = FJET(XB1, XB2, s, 0.0D0, t142, 0.0D0, -t144, 0.0D0, t157)
      t163 = t151 * t153 * t49 * t81
      t167 = t1 * x1
      t168 = x1 * z
      t169 = -z - x1 + t168
      t170 = 0.1D1 / t169
      t172 = t91 * s * t167 * t170
      t173 = -0.1D1 + x1
      t174 = t2 * t173
      t176 = x2 * s * t167
      t177 = t1 ** 2
      t178 = s * t177
      t181 = x1 * t173 * t170
      t182 = t178 * t91 * t181
      t183 = x2 * x1
      t186 = 0.1D1 / (-z - t183 + t183 * z)
      t188 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, t176, t172, 
     #-t174, 0.0D0, -t182)
      t192 = t71 * t186 * t188 * t81 * t68 / 0.8D1
      t193 = FJET(XB1, XB2, s, 0.0D0, t172, -t174, t176, -t182, t192)
      t198 = t186 * t188 * t81 * t68
      t202 = t2 * x1 * t170
      t203 = t178 * t181
      t205 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t202
     #, -t174, 0.0D0, t203)
      t210 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t202
     #, -t174, 0.0D0, t203)
      t214 = t173 ** 2
      t218 = log(-0.4D1 * t53 / t8 * t14 * t170 * t214)
      t239 = x3 * t53
      t245 = Sqrt(x3 * t169 * t20)
      t257 = t71 * t18 * t205 * t81 * t68 / 0.8D1 - (-0.90D2 * t52 * pi 
     #* (t210 - t218 * t205) + 0.180D3 * t6 * lh * pi * t18 * t205) * t6
     #8 / 0.1440D4 - t71 * (-t18 * t205 + t169 * t205 / (-t168 - x3 * x1
     # * z + x1 * t8 + x3 * t8 * x1 - t239 * t8 - t239 - t27 + 0.2D1 * t
     #239 * z - t8 + 0.2D1 * t26 * t245 * z)) * t49 * t68 / 0.16D2
      t258 = FJET(XB1, XB2, s, 0.0D0, -t174, -t202, 0.0D0, t203, t257)
      t260 = FJET(XB1, XB2, s, 0.0D0, -t202, -t174, 0.0D0, t203, t257)
      t262 = FJET(XB1, XB2, s, 0.0D0, -t144, 0.0D0, t142, 0.0D0, t157)
      t267 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t131)
      t269 = FJET(XB1, XB2, s, t176, -t174, t172, 0.0D0, -t182, t192)
      t274 = FJET(XB1, XB2, s, t142, 0.0D0, -t144, 0.0D0, 0.0D0, t157)
      t279 = FJET(XB1, XB2, s, t172, 0.0D0, t176, -t174, -t182, t192)
      t284 = FJET(XB1, XB2, s, -t174, 0.0D0, 0.0D0, -t202, t203, t257)
      t286 = FJET(XB1, XB2, s, -t174, t176, 0.0D0, t172, -t182, t192)
      t291 = FJET(XB1, XB2, s, -t202, 0.0D0, 0.0D0, -t174, t203, t257)
      t293 = FJET(XB1, XB2, s, -t144, 0.0D0, t142, 0.0D0, 0.0D0, t157)
      rrgg2qqbarht5s2em1 = t132 * t131 + t134 * t131 + t136 * t131 + t15
     #8 * t6 * pi * t163 / 0.16D2 + t193 * t6 * pi * t198 / 0.8D1 + t258
     # * t257 + t260 * t257 + t262 * t6 * pi * t163 / 0.16D2 + t267 * t1
     #31 + t269 * t6 * pi * t198 / 0.8D1 + t274 * t6 * pi * t163 / 0.16D
     #2 + t279 * t6 * pi * t198 / 0.8D1 + t284 * t257 + t286 * t6 * pi *
     # t198 / 0.8D1 + t291 * t257 + t293 * t6 * pi * t163 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s2em2
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

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
      t3 = 0.1D1 / z
      t4 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t5 = t3 * t4
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t8 * pi
      t10 = 0.1D1 / x1
      t15 = x4 * pi
      t16 = cos(t15)
      t20 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t34 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t36 = z ** 2
      t39 = Sin(t15)
      t40 = t39 ** 2
      t43 = log(0.4D1 / t36 / z * t40)
      t52 = -t5 * t9 * t10 / 0.16D2 + t4 * t8 * pi * (-t3 - 0.1D1 / (-z 
     #- x3 + 0.2D1 * t16 * t20)) / x3 / 0.32D2 + (0.180D3 * t5 * t8 * lh
     # - 0.90D2 * (t3 * t34 - t43 * t3 * t4) * t8) * pi / 0.2880D4
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t52)
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t52)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t52)
      t59 = -0.1D1 + x1
      t60 = t2 * t59
      t63 = 0.1D1 / (-z - x1 + x1 * z)
      t65 = t2 * x1 * t63
      t66 = t1 ** 2
      t70 = s * t66 * x1 * t59 * t63
      t71 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t65, 
     #-t60, 0.0D0, t70)
      t73 = t3 * t71 * t10
      t75 = t9 * t73 / 0.16D2
      t76 = FJET(XB1, XB2, s, 0.0D0, -t60, -t65, 0.0D0, t70, t75)
      t81 = FJET(XB1, XB2, s, 0.0D0, -t65, -t60, 0.0D0, t70, t75)
      t86 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t52)
      t88 = FJET(XB1, XB2, s, -t60, 0.0D0, 0.0D0, -t65, t70, t75)
      t93 = FJET(XB1, XB2, s, -t65, 0.0D0, 0.0D0, -t60, t70, t75)
      rrgg2qqbarht5s2em2 = t53 * t52 + t55 * t52 + t57 * t52 + t76 * t8 
     #* pi * t73 / 0.16D2 + t81 * t8 * pi * t73 / 0.16D2 + t86 * t52 + t
     #88 * t8 * pi * t73 / 0.16D2 + t93 * t8 * pi * t73 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s2em3
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = 0.1D1 / z
      t4 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t11 = t3 * t4 * t8 * pi / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t4 * t8 * pi
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2qqbarht5s2em3 = -t12 * t3 * t15 / 0.32D2 - t17 * t3 * t15 / 0
     #.32D2 - t20 * t3 * t15 / 0.32D2 - t23 * t3 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht5s2em4
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
      doubleprecision rrgg2qqbarh51J1
      doubleprecision rrgg2qqbarh51J2
      doubleprecision rrgg2qqbarh51J3
      doubleprecision rrgg2qqbarh51J4
      doubleprecision rrgg2qqbarh51J5
      doubleprecision rrgg2qqbarh51J6
      doubleprecision rrgg2qqbarh51J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht5s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh51J1
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
      t2 = S13 + S14 + S34
      t4 = S23 + S24 + S34
      t5 = t4 * S24
      t10 = S23 * S13
      t18 = 0.1D1 / S12
      t22 = -S24 - S13
      t24 = 0.144D3 * t22 * nf
      t25 = S12 ** 2
      t28 = -t22
      t42 = S13 ** 2
      t43 = S13 * S14
      t44 = S23 * S24
      t45 = S24 ** 2
      t46 = t42 - t43 - t44 + t45
      t52 = S34 ** 2
      t84 = S24 * S14
      t90 = t45 * S24
      t92 = t42 * S13
      t94 = t42 * S14
      t96 = S14 ** 2
      t97 = S13 * t96
      t99 = S23 ** 2
      t100 = t99 * S24
      t102 = S23 * t45
      t186 = t45 ** 2
      t190 = S14 * t92
      t192 = t42 ** 2
      t197 = t96 * S14 * S13
      t200 = t99 * S23 * S24
      t202 = S23 * t90
      rrgg2qqbarh51J1 = (((0.4D1 / 0.3D1 * S13 * nf * t2 + 0.4D1 / 0.3D1
     # * t5 * nf) * S34 - 0.4D1 / 0.3D1 * t10 * nf * t2 - 0.4D1 / 0.3D1 
     #* t5 * S14 * nf) * t18 * s * z - t24 * t25 / 0.96D2 + (-0.7D1 / 0.
     #4D1 * t28 * nf * S34 - (-0.56D2 * S24 - 0.312D3 * S13) * nf * t2 /
     # 0.96D2 - (-0.56D2 * S13 - 0.312D3 * S24) * nf * t4 / 0.96D2 - 0.9
     #D1 / 0.4D1 * t46 * nf) * S12 - t24 * t52 / 0.96D2 + (-(-0.400D3 * 
     #S24 + 0.128D3 * S13) * nf * t2 / 0.96D2 - (-0.400D3 * S13 + 0.128D
     #3 * S24) * nf * t4 / 0.96D2 + 0.7D1 / 0.6D1 * t46 * nf) * S34 - (-
     #0.128D3 * t10 - 0.584D3 * t44 + 0.240D3 * t42 - 0.328D3 * t45 - 0.
     #880D3 * t43) * nf * t2 / 0.96D2 - (-0.584D3 * t43 - 0.328D3 * t42 
     #- 0.880D3 * t44 + 0.240D3 * t45 - 0.128D3 * t84) * nf * t4 / 0.96D
     #2 - (-0.144D3 * t90 - 0.144D3 * t92 + 0.288D3 * t94 - 0.144D3 * t9
     #7 - 0.144D3 * t100 + 0.288D3 * t102) * nf / 0.96D2 + (-0.7D1 / 0.2
     #4D2 * t28 * nf * t52 * S34 + (-(-0.47D2 * S13 - 0.192D3 * S24) * n
     #f * t2 / 0.96D2 - (-0.47D2 * S24 - 0.192D3 * S13) * nf * t4 / 0.96
     #D2 - 0.3D1 / 0.8D1 * t46 * nf) * t52 + (-(0.94D2 * t10 + 0.148D3 *
     # t42 - 0.182D3 * t45 - 0.530D3 * t44 + 0.96D2 * t43) * nf * t2 / 0
     #.96D2 - (0.96D2 * t44 - 0.530D3 * t43 + 0.94D2 * t84 - 0.182D3 * t
     #42 + 0.148D3 * t45) * nf * t4 / 0.96D2 - (0.28D2 * t97 + 0.28D2 * 
     #t92 + 0.28D2 * t100 - 0.56D2 * t94 + 0.28D2 * t90 - 0.56D2 * t102)
     # * nf / 0.96D2) * S34 - (0.12D2 * t92 - 0.10D2 * t90 - 0.430D3 * t
     #100 - 0.328D3 * t102 - 0.96D2 * t43 * S23 - 0.47D2 * S13 * t99 - 0
     #.748D3 * t97 - 0.16D2 * t94 - 0.148D3 * t42 * S23) * nf * t2 / 0.9
     #6D2 - (0.12D2 * t90 - 0.96D2 * t84 * S23 - 0.748D3 * t100 - 0.10D2
     # * t92 - 0.328D3 * t94 - 0.47D2 * S24 * t96 - 0.430D3 * t97 - 0.14
     #8D3 * t45 * S14 - 0.16D2 * t102) * nf * t4 / 0.96D2 - (0.36D2 * t1
     #86 + 0.108D3 * t45 * t99 - 0.108D3 * t190 + 0.36D2 * t192 + 0.108D
     #3 * t42 * t96 - 0.36D2 * t197 - 0.36D2 * t200 - 0.108D3 * t202) * 
     #nf / 0.96D2) * t18 + (-(-0.288D3 * t94 * S23 - 0.144D3 * t197 - 0.
     #144D3 * t190 - 0.144D3 * t43 * t99) * nf * t2 / 0.96D2 - (-0.144D3
     # * t200 - 0.144D3 * t44 * t96 - 0.288D3 * t102 * S14 - 0.144D3 * t
     #202) * nf * t4 / 0.96D2) / t25) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J2
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
      t1 = -S24 - S13
      t4 = S12 ** 2
      t7 = S13 + S14 + S34
      t10 = S23 + S24 + S34
      t32 = S13 * S14
      t33 = S13 ** 2
      t34 = S24 ** 2
      t35 = S23 * S24
      t36 = -t32 + t33 + t34 - t35
      t44 = S34 ** 2
      t66 = 0.128D3 * S14 * S23
      t68 = S23 * S13
      t71 = S24 * S14
      t89 = t34 * S24
      t91 = t33 * S13
      t93 = t33 * S14
      t94 = 0.576D3 * t93
      t95 = S14 ** 2
      t96 = S13 * t95
      t98 = S23 ** 2
      t99 = t98 * S24
      t101 = S23 * t34
      t102 = 0.576D3 * t101
      t150 = t32 * S23
      t152 = t71 * S23
      t154 = t34 * S14
      t160 = S24 * t95
      t162 = t33 * S23
      t164 = S13 * t98
      t169 = 0.448D3 * t150 - 0.256D3 * t152 - 0.256D3 * t154 + 0.40D2 *
     # t91 + 0.42D2 * t89 - 0.318D3 * t99 - 0.52D2 * t101 - 0.136D3 * t1
     #60 - 0.80D2 * t162 - 0.40D2 * t164 - 0.904D3 * t96 + t94 + 0.64D2 
     #* S23 * t95
      t186 = -0.136D3 * t164 - 0.52D2 * t93 + 0.40D2 * t89 + 0.64D2 * t9
     #8 * S14 - 0.80D2 * t154 + 0.42D2 * t91 + 0.448D3 * t152 - 0.256D3 
     #* t150 - 0.318D3 * t96 - 0.256D3 * t162 - 0.40D2 * t160 + t102 - 0
     #.904D3 * t99
      t191 = t98 * S23 * S24
      t193 = S23 * t89
      t196 = t95 * S14 * S13
      t198 = S14 * t91
      t200 = t33 ** 2
      t202 = t33 * t95
      t204 = t34 * t98
      t206 = t34 ** 2
      rrgg2qqbarh51J2 = (-0.3D1 / 0.2D1 * t1 * nf * t4 + ((0.2D1 / 0.3D1
     # * nf * t7 + 0.2D1 / 0.3D1 * nf * t10 - t1 * nf / 0.12D2) * S34 - 
     #(-0.360D3 * S13 + 0.64D2 * S23 - 0.136D3 * S24) * nf * t7 / 0.96D2
     # - (-0.136D3 * S13 + 0.64D2 * S14 - 0.360D3 * S24) * nf * t10 / 0.
     #96D2 - 0.15D2 / 0.4D1 * t36 * nf) * S12 - t1 * nf * t44 / 0.3D1 + 
     #(-(-0.128D3 * S14 - 0.240D3 * S13 - 0.512D3 * S24) * nf * t7 / 0.9
     #6D2 - (-0.240D3 * S24 - 0.128D3 * S23 - 0.512D3 * S13) * nf * t10 
     #/ 0.96D2 + 0.5D1 / 0.6D1 * t36 * nf) * S34 - (t66 - 0.1120D4 * t32
     # + 0.240D3 * t68 - 0.400D3 * t34 - 0.216D3 * t71 - 0.624D3 * t35 +
     # 0.544D3 * t33) * nf * t7 / 0.96D2 - (-0.624D3 * t32 - 0.216D3 * t
     #68 + 0.544D3 * t34 + t66 - 0.1120D4 * t35 + 0.240D3 * t71 - 0.400D
     #3 * t33) * nf * t10 / 0.96D2 - (-0.288D3 * t89 - 0.288D3 * t91 + t
     #94 - 0.288D3 * t96 - 0.288D3 * t99 + t102) * nf / 0.96D2 + ((-(-0.
     #40D2 * S13 - 0.354D3 * S24) * nf * t7 / 0.96D2 - (-0.40D2 * S24 - 
     #0.354D3 * S13) * nf * t10 / 0.96D2) * t44 + (-(-0.256D3 * t71 + 0.
     #80D2 * t68 - 0.616D3 * t35 - 0.448D3 * t32 - 0.64D2 * t95 + 0.80D2
     # * t33 - 0.256D3 * t34) * nf * t7 / 0.96D2 - (0.80D2 * t71 + 0.80D
     #2 * t34 - 0.616D3 * t32 - 0.256D3 * t33 - 0.448D3 * t35 - 0.64D2 *
     # t98 - 0.256D3 * t68) * nf * t10 / 0.96D2 - (0.56D2 * t89 - 0.112D
     #3 * t101 + 0.56D2 * t99 + 0.56D2 * t96 - 0.112D3 * t93 + 0.56D2 * 
     #t91) * nf / 0.96D2) * S34 - t169 * nf * t7 / 0.96D2 - t186 * nf * 
     #t10 / 0.96D2 - (-0.72D2 * t191 - 0.216D3 * t193 - 0.72D2 * t196 - 
     #0.216D3 * t198 + 0.72D2 * t200 + 0.216D3 * t202 + 0.216D3 * t204 +
     # 0.72D2 * t206) * nf / 0.96D2) / S12 + (-(-0.144D3 * t196 + 0.288D
     #3 * t202 - 0.144D3 * t32 * t98 + 0.432D3 * t96 * S23 - 0.144D3 * t
     #198 - 0.144D3 * t93 * S23) * nf * t7 / 0.96D2 - (-0.144D3 * t191 -
     # 0.144D3 * t35 * t95 + 0.432D3 * t99 * S14 - 0.144D3 * t193 + 0.28
     #8D3 * t204 - 0.144D3 * t101 * S14) * nf * t10 / 0.96D2) / t4) / pi
     # * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J3
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.4D1 * t12 - 0.4D1 * t14
      t22 = S34 ** 2
      t39 = -S24 - S13
      t42 = S12 ** 2
      t66 = S13 * S14
      t67 = S24 ** 2
      t68 = S23 * S24
      t69 = S13 ** 2
      t70 = -t66 + t67 - t68 + t69
      t95 = -t70
      t101 = S23 * S13
      t106 = S23 ** 2
      t109 = S14 * S23
      t110 = 0.320D3 * t109
      t111 = S24 * S14
      t119 = S14 ** 2
      t129 = t67 * S24
      t131 = t69 * S13
      t133 = t69 * S14
      t135 = t106 * S24
      t137 = S23 * t67
      t139 = S13 * t119
      t173 = 0.64D2 * t109
      t204 = t66 * S23
      t206 = S23 * t119
      t208 = t106 * S23
      t212 = t67 * S14
      t214 = t111 * S23
      t219 = S24 * t119
      t221 = S13 * t106
      t223 = t106 * S14
      t226 = t69 * S23
      t228 = 0.992D3 * t204 + 0.128D3 * t206 + 0.16D2 * t208 + 0.398D3 *
     # t129 - 0.27696D5 * t137 - 0.25808D5 * t212 - 0.25776D5 * t214 - 0
     #.28414D5 * t135 - 0.1124D4 * t139 + 0.1040D4 * t133 - 0.272D3 * t2
     #19 - 0.37D2 * t221 + 0.32D2 * t223 + 0.4D1 * t131 - 0.12D2 * t226
      t232 = t119 * S14
      t248 = 0.16D2 * t232 - 0.25808D5 * t226 + 0.398D3 * t131 + 0.4D1 *
     # t129 + 0.32D2 * t206 - 0.272D3 * t221 + 0.992D3 * t214 - 0.28414D
     #5 * t139 - 0.27696D5 * t133 - 0.12D2 * t212 - 0.37D2 * t219 + 0.12
     #8D3 * t223 - 0.25776D5 * t204 - 0.1124D4 * t135 + 0.1040D4 * t137
      t252 = t67 * t106
      t254 = S23 * t129
      t256 = t232 * S13
      t258 = S14 * t131
      t260 = t69 * t119
      t262 = t69 ** 2
      t264 = t67 ** 2
      t266 = t208 * S24
      t297 = 0.4D1 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S34
     # * t1 * t3 * t6 + (-t16 * t22 + (-(-0.384D3 * S14 - 0.384D3 * S13)
     # * nf * t11 / 0.96D2 - (-0.384D3 * S23 - 0.384D3 * S24) * nf * t13
     # / 0.96D2) * S34) * t1 * s * z - 0.3D1 / 0.2D1 * t39 * nf * t42 + 
     #((0.8D1 / 0.3D1 * t12 + 0.8D1 / 0.3D1 * t14 - 0.5D1 / 0.12D2 * t39
     # * nf) * S34 - (0.256D3 * S23 - 0.248D3 * S24 - 0.472D3 * S13) * n
     #f * t11 / 0.96D2 - (0.256D3 * S14 - 0.472D3 * S24 - 0.248D3 * S13)
     # * nf * t13 / 0.96D2 - 0.21D2 / 0.4D1 * t70 * nf) * S12 + (-t12 - 
     #t14 - 0.13D2 / 0.6D1 * t39 * nf) * t22 + (-(-0.320D3 * S14 - 0.600
     #D3 * S13 - 0.192D3 * S23 - 0.74912D5 * S24) * nf * t11 / 0.96D2 - 
     #(-0.320D3 * S23 - 0.74912D5 * S13 - 0.600D3 * S24 - 0.192D3 * S14)
     # * nf * t13 / 0.96D2 - t95 * nf / 0.2D1) * S34 - (0.600D3 * t101 -
     # 0.74792D5 * t67 - 0.1488D4 * t66 + 0.720D3 * t69 + 0.96D2 * t106 
     #- 0.74872D5 * t68 + t110 - 0.512D3 * t111) * nf * t11 / 0.96D2 - (
     #-0.512D3 * t101 + 0.720D3 * t67 + 0.96D2 * t119 + t110 - 0.1488D4 
     #* t68 - 0.74792D5 * t69 + 0.600D3 * t111 - 0.74872D5 * t66) * nf *
     # t13 / 0.96D2 - (-0.432D3 * t129 - 0.432D3 * t131 + 0.864D3 * t133
     # - 0.432D3 * t135 + 0.864D3 * t137 - 0.432D3 * t139) * nf / 0.96D2
     # + ((-0.7D1 / 0.6D1 * t12 - 0.7D1 / 0.6D1 * t14 + 0.29D2 / 0.24D2 
     #* t39 * nf) * t22 * S34 + (-(0.48D2 * S23 + 0.28276D5 * S24 + 0.21
     #9D3 * S13 + 0.288D3 * S14) * nf * t11 / 0.96D2 - (0.288D3 * S23 + 
     #0.28276D5 * S13 + 0.48D2 * S14 + 0.219D3 * S24) * nf * t13 / 0.96D
     #2 - 0.3D1 / 0.8D1 * t95 * nf) * t22 + (-(0.74D2 * t101 - t173 - 0.
     #48D2 * t106 + 0.140D3 * t69 - 0.736D3 * t66 - 0.25760D5 * t111 - 0
     #.166D3 * t68 + 0.28798D5 * t67) * nf * t11 / 0.96D2 - (0.140D3 * t
     #67 + 0.28798D5 * t69 - 0.736D3 * t68 - 0.25760D5 * t101 - 0.48D2 *
     # t119 + 0.74D2 * t111 - t173 - 0.166D3 * t66) * nf * t13 / 0.96D2 
     #- (0.84D2 * t139 + 0.84D2 * t131 - 0.168D3 * t137 - 0.168D3 * t133
     # + 0.84D2 * t135 + 0.84D2 * t129) * nf / 0.96D2) * S34 - t228 * nf
     # * t11 / 0.96D2 - t248 * nf * t13 / 0.96D2 - (0.324D3 * t252 - 0.3
     #24D3 * t254 - 0.108D3 * t256 - 0.324D3 * t258 + 0.324D3 * t260 + 0
     #.108D3 * t262 + 0.108D3 * t264 - 0.108D3 * t266) * nf / 0.96D2) * 
     #t1 + (-(0.864D3 * t139 * S23 - 0.144D3 * t258 - 0.144D3 * t66 * t1
     #06 - 0.144D3 * t256 + 0.576D3 * t260) * nf * t11 / 0.96D2 - (-0.14
     #4D3 * t266 + 0.864D3 * t135 * S14 - 0.144D3 * t68 * t119 + 0.576D3
     # * t252 - 0.144D3 * t254) * nf * t13 / 0.96D2) / t42
      rrgg2qqbarh51J3 = t297 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J4
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.8D1 * t12 - 0.8D1 * t14
      t22 = S34 ** 2
      t39 = -S24 - S13
      t42 = S12 ** 2
      t66 = S13 * S14
      t67 = S23 * S24
      t68 = S13 ** 2
      t69 = S24 ** 2
      t70 = -t66 - t67 + t68 + t69
      t97 = -t70
      t103 = S23 * S13
      t108 = S23 ** 2
      t111 = S14 * S23
      t112 = 0.512D3 * t111
      t113 = S24 * S14
      t121 = S14 ** 2
      t131 = t69 * S24
      t133 = t68 * S13
      t135 = t68 * S14
      t137 = t108 * S24
      t139 = t69 * S23
      t141 = S13 * t121
      t175 = 0.128D3 * t111
      t208 = t66 * S23
      t210 = S23 * t121
      t212 = t108 * S23
      t216 = t69 * S14
      t218 = t113 * S23
      t223 = S24 * t121
      t225 = S13 * t108
      t227 = t108 * S14
      t230 = t68 * S23
      t232 = 0.1536D4 * t208 + 0.192D3 * t210 + 0.32D2 * t212 + 0.754D3 
     #* t131 - 0.55340D5 * t139 - 0.51360D5 * t216 - 0.51296D5 * t218 - 
     #0.56510D5 * t137 - 0.1344D4 * t141 + 0.1504D4 * t135 - 0.408D3 * t
     #223 - 0.34D2 * t225 + 0.64D2 * t227 - 0.32D2 * t133 + 0.56D2 * t23
     #0
      t236 = t121 * S14
      t252 = 0.32D2 * t236 - 0.51360D5 * t230 + 0.754D3 * t133 - 0.32D2 
     #* t131 + 0.64D2 * t210 - 0.408D3 * t225 + 0.1536D4 * t218 - 0.5651
     #0D5 * t141 - 0.55340D5 * t135 + 0.56D2 * t216 - 0.34D2 * t223 + 0.
     #192D3 * t227 - 0.51296D5 * t208 - 0.1344D4 * t137 + 0.1504D4 * t13
     #9
      t256 = t69 * t108
      t258 = S23 * t131
      t261 = 0.144D3 * t236 * S13
      t262 = S14 * t133
      t264 = t68 * t121
      t266 = t68 ** 2
      t268 = t69 ** 2
      t271 = 0.144D3 * t212 * S24
      t303 = 0.8D1 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S34
     # * t1 * t3 * t6 + (-t16 * t22 + (-(-0.768D3 * S13 - 0.768D3 * S14)
     # * nf * t11 / 0.96D2 - (-0.768D3 * S23 - 0.768D3 * S24) * nf * t13
     # / 0.96D2) * S34) * t1 * s * z - 0.3D1 / 0.2D1 * t39 * nf * t42 + 
     #((0.14D2 / 0.3D1 * t12 + 0.14D2 / 0.3D1 * t14 - 0.3D1 / 0.4D1 * t3
     #9 * nf) * S34 - (-0.360D3 * S24 + 0.448D3 * S23 - 0.584D3 * S13) *
     # nf * t11 / 0.96D2 - (-0.360D3 * S13 + 0.448D3 * S14 - 0.584D3 * S
     #24) * nf * t13 / 0.96D2 - 0.27D2 / 0.4D1 * t70 * nf) * S12 + (-0.2
     #D1 * t12 - 0.2D1 * t14 - 0.4D1 * t39 * nf) * t22 + (-(-0.512D3 * S
     #14 - 0.960D3 * S13 - 0.384D3 * S23 - 0.149312D6 * S24) * nf * t11 
     #/ 0.96D2 - (-0.512D3 * S23 - 0.149312D6 * S13 - 0.960D3 * S24 - 0.
     #384D3 * S14) * nf * t13 / 0.96D2 - t97 * nf / 0.6D1) * S34 - (0.96
     #0D3 * t103 - 0.149184D6 * t69 - 0.1856D4 * t66 + 0.896D3 * t68 + 0
     #.192D3 * t108 - 0.149120D6 * t67 + t112 - 0.808D3 * t113) * nf * t
     #11 / 0.96D2 - (-0.808D3 * t103 + 0.896D3 * t69 + 0.192D3 * t121 + 
     #t112 - 0.1856D4 * t67 - 0.149184D6 * t68 + 0.960D3 * t113 - 0.1491
     #20D6 * t66) * nf * t13 / 0.96D2 - (-0.576D3 * t131 - 0.576D3 * t13
     #3 + 0.1152D4 * t135 - 0.576D3 * t137 + 0.1152D4 * t139 - 0.576D3 *
     # t141) * nf / 0.96D2 + ((-0.7D1 / 0.3D1 * t12 - 0.7D1 / 0.3D1 * t1
     #4 + 0.29D2 / 0.12D2 * t39 * nf) * t22 * S34 + (-(0.96D2 * S23 + 0.
     #56906D5 * S24 + 0.478D3 * S13 + 0.576D3 * S14) * nf * t11 / 0.96D2
     # - (0.576D3 * S23 + 0.56906D5 * S13 + 0.96D2 * S14 + 0.478D3 * S24
     #) * nf * t13 / 0.96D2 - 0.3D1 / 0.4D1 * t97 * nf) * t22 + (-(0.68D
     #2 * t103 - t175 - 0.96D2 * t108 + 0.200D3 * t68 - 0.1024D4 * t66 -
     # 0.51264D5 * t113 + 0.64D2 * t121 + 0.284D3 * t67 + 0.57852D5 * t6
     #9) * nf * t11 / 0.96D2 - (0.200D3 * t69 + 0.284D3 * t66 + 0.57852D
     #5 * t68 - 0.1024D4 * t67 - t175 - 0.51264D5 * t103 - 0.96D2 * t121
     # + 0.68D2 * t113 + 0.64D2 * t108) * nf * t13 / 0.96D2 - (0.112D3 *
     # t133 + 0.112D3 * t141 - 0.224D3 * t135 + 0.112D3 * t131 - 0.224D3
     # * t139 + 0.112D3 * t137) * nf / 0.96D2) * S34 - t232 * nf * t11 /
     # 0.96D2 - t252 * nf * t13 / 0.96D2 - (0.432D3 * t256 - 0.432D3 * t
     #258 - t261 - 0.432D3 * t262 + 0.432D3 * t264 + 0.144D3 * t266 + 0.
     #144D3 * t268 - t271) * nf / 0.96D2) * t1 + (-(0.1296D4 * t141 * S2
     #3 - t261 - 0.144D3 * t262 + 0.144D3 * t135 * S23 - 0.144D3 * t66 *
     # t108 + 0.864D3 * t264) * nf * t11 / 0.96D2 - (-0.144D3 * t258 - t
     #271 + 0.1296D4 * t137 * S14 - 0.144D3 * t67 * t121 + 0.144D3 * t13
     #9 * S14 + 0.864D3 * t256) * nf * t13 / 0.96D2) / t42
      rrgg2qqbarh51J4 = t303 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J5
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.12D2 * t12 - 0.12D2 * t14
      t22 = S34 ** 2
      t39 = -S24 - S13
      t42 = S12 ** 2
      t66 = S13 * S14
      t67 = S23 * S24
      t68 = S13 ** 2
      t69 = S24 ** 2
      t70 = -t66 - t67 + t68 + t69
      t102 = S23 * S13
      t107 = S23 ** 2
      t110 = S14 * S23
      t111 = 0.704D3 * t110
      t112 = S24 * S14
      t120 = S14 ** 2
      t130 = t69 * S24
      t132 = t68 * S13
      t134 = t68 * S14
      t136 = t107 * S24
      t138 = t69 * S23
      t140 = S13 * t120
      t175 = 0.192D3 * t110
      t208 = t66 * S23
      t210 = S23 * t120
      t212 = t107 * S23
      t216 = t69 * S14
      t218 = t112 * S23
      t223 = S24 * t120
      t225 = S13 * t107
      t227 = t107 * S14
      t230 = t68 * S23
      t232 = 0.2080D4 * t208 + 0.256D3 * t210 + 0.48D2 * t212 + 0.1110D4
     # * t130 - 0.82984D5 * t138 - 0.76912D5 * t216 - 0.76816D5 * t218 -
     # 0.84606D5 * t136 - 0.1564D4 * t140 + 0.1968D4 * t134 - 0.544D3 * 
     #t223 - 0.31D2 * t225 + 0.96D2 * t227 - 0.68D2 * t132 + 0.124D3 * t
     #230
      t236 = t120 * S14
      t252 = 0.48D2 * t236 - 0.76912D5 * t230 + 0.1110D4 * t132 - 0.68D2
     # * t130 + 0.96D2 * t210 - 0.544D3 * t225 + 0.2080D4 * t218 - 0.846
     #06D5 * t140 - 0.82984D5 * t134 + 0.124D3 * t216 - 0.31D2 * t223 + 
     #0.256D3 * t227 - 0.76816D5 * t208 - 0.1564D4 * t136 + 0.1968D4 * t
     #138
      t256 = t69 * t107
      t258 = S23 * t130
      t260 = t236 * S13
      t262 = S14 * t132
      t264 = t68 * t120
      t266 = t68 ** 2
      t268 = t69 ** 2
      t270 = t212 * S24
      t305 = 0.4D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S34 * t1 * 
     #t3 * t6 + (-t16 * t22 + (-(-0.1152D4 * S13 - 0.1152D4 * S14) * nf 
     #* t11 / 0.96D2 - (-0.1152D4 * S23 - 0.1152D4 * S24) * nf * t13 / 0
     #.96D2) * S34) * t1 * s * z - 0.3D1 / 0.2D1 * t39 * nf * t42 + ((0.
     #20D2 / 0.3D1 * t12 + 0.20D2 / 0.3D1 * t14 - 0.13D2 / 0.12D2 * t39 
     #* nf) * S34 - (-0.472D3 * S24 + 0.640D3 * S23 - 0.696D3 * S13) * n
     #f * t11 / 0.96D2 - (-0.472D3 * S13 + 0.640D3 * S14 - 0.696D3 * S24
     #) * nf * t13 / 0.96D2 - 0.33D2 / 0.4D1 * t70 * nf) * S12 + (-0.3D1
     # * t12 - 0.3D1 * t14 - 0.35D2 / 0.6D1 * t39 * nf) * t22 + (-(-0.70
     #4D3 * S14 - 0.1320D4 * S13 - 0.576D3 * S23 - 0.223712D6 * S24) * n
     #f * t11 / 0.96D2 - (-0.704D3 * S23 - 0.223712D6 * S13 - 0.1320D4 *
     # S24 - 0.576D3 * S14) * nf * t13 / 0.96D2 - t70 * nf / 0.6D1) * S3
     #4 - (0.1320D4 * t102 - 0.223576D6 * t69 - 0.2224D4 * t66 + 0.1072D
     #4 * t68 + 0.288D3 * t107 - 0.223368D6 * t67 + t111 - 0.1104D4 * t1
     #12) * nf * t11 / 0.96D2 - (-0.1104D4 * t102 + 0.1072D4 * t69 + 0.2
     #88D3 * t120 + t111 - 0.2224D4 * t67 - 0.223576D6 * t68 + 0.1320D4 
     #* t112 - 0.223368D6 * t66) * nf * t13 / 0.96D2 - (-0.720D3 * t130 
     #- 0.720D3 * t132 + 0.1440D4 * t134 - 0.720D3 * t136 + 0.1440D4 * t
     #138 - 0.720D3 * t140) * nf / 0.96D2 + ((-0.7D1 / 0.2D1 * t12 - 0.7
     #D1 / 0.2D1 * t14 + 0.29D2 / 0.8D1 * t39 * nf) * t22 * S34 + (-(0.1
     #44D3 * S23 + 0.85536D5 * S24 + 0.737D3 * S13 + 0.864D3 * S14) * nf
     # * t11 / 0.96D2 - (0.864D3 * S23 + 0.85536D5 * S13 + 0.144D3 * S14
     # + 0.737D3 * S24) * nf * t13 / 0.96D2 + 0.9D1 / 0.8D1 * t70 * nf) 
     #* t22 + (-(0.62D2 * t102 - t175 - 0.144D3 * t107 + 0.260D3 * t68 -
     # 0.1312D4 * t66 - 0.76768D5 * t112 + 0.128D3 * t120 + 0.734D3 * t6
     #7 + 0.86906D5 * t69) * nf * t11 / 0.96D2 - (0.260D3 * t69 + 0.734D
     #3 * t66 + 0.86906D5 * t68 - 0.1312D4 * t67 - t175 - 0.76768D5 * t1
     #02 - 0.144D3 * t120 + 0.62D2 * t112 + 0.128D3 * t107) * nf * t13 /
     # 0.96D2 - (0.140D3 * t132 + 0.140D3 * t140 - 0.280D3 * t134 + 0.14
     #0D3 * t130 - 0.280D3 * t138 + 0.140D3 * t136) * nf / 0.96D2) * S34
     # - t232 * nf * t11 / 0.96D2 - t252 * nf * t13 / 0.96D2 - (0.540D3 
     #* t256 - 0.540D3 * t258 - 0.180D3 * t260 - 0.540D3 * t262 + 0.540D
     #3 * t264 + 0.180D3 * t266 + 0.180D3 * t268 - 0.180D3 * t270) * nf 
     #/ 0.96D2) * t1 + (-(0.1728D4 * t140 * S23 - 0.144D3 * t260 - 0.144
     #D3 * t262 + 0.288D3 * t134 * S23 - 0.144D3 * t66 * t107 + 0.1152D4
     # * t264) * nf * t11 / 0.96D2 - (-0.144D3 * t258 - 0.144D3 * t270 +
     # 0.1728D4 * t136 * S14 - 0.144D3 * t67 * t120 + 0.288D3 * t138 * S
     #14 + 0.1152D4 * t256) * nf * t13 / 0.96D2) / t42
      rrgg2qqbarh51J5 = t305 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J6
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.16D2 * t12 - 0.16D2 * t14
      t22 = S34 ** 2
      t37 = S23 * S13
      t48 = S24 + S13
      t51 = S12 ** 2
      t76 = S23 * S24
      t77 = S24 ** 2
      t78 = S13 * S14
      t79 = S13 ** 2
      t80 = t76 - t77 + t78 - t79
      t82 = 0.360D3 * t80 * nf
      t118 = S23 ** 2
      t120 = S14 * S23
      t121 = 0.896D3 * t120
      t122 = S24 * S14
      t133 = S14 ** 2
      t165 = 0.192D3 * t133
      t168 = 0.192D3 * t118
      t169 = 0.256D3 * t120
      t189 = t79 * S13
      t191 = t77 * S24
      t193 = t118 * S14
      t195 = t118 * S24
      t197 = t77 * S14
      t199 = S23 * t133
      t201 = t122 * S23
      t203 = S13 * t118
      t205 = S23 * t77
      t207 = t79 * S14
      t209 = t79 * S23
      t211 = t78 * S23
      t213 = S13 * t133
      t215 = t118 * S23
      t217 = S24 * t133
      t219 = -0.176D3 * t189 + 0.1526D4 * t191 + 0.128D3 * t193 - 0.1101
     #22D6 * t195 - 0.102464D6 * t197 + 0.320D3 * t199 - 0.102336D6 * t2
     #01 + 0.254D3 * t203 - 0.108660D6 * t205 + 0.2528D4 * t207 + 0.1080
     #D4 * t209 + 0.3200D4 * t211 + 0.2704D4 * t213 + 0.64D2 * t215 - 0.
     #680D3 * t217
      t233 = S14 * t133
      t239 = 0.1526D4 * t189 + 0.3200D4 * t201 + 0.2528D4 * t205 - 0.102
     #336D6 * t211 - 0.102464D6 * t209 - 0.680D3 * t203 + 0.320D3 * t193
     # - 0.110122D6 * t213 + 0.128D3 * t199 + 0.2704D4 * t195 + 0.64D2 *
     # t233 - 0.176D3 * t191 + 0.1080D4 * t197 - 0.108660D6 * t207 + 0.2
     #54D3 * t217
      t279 = 0.16D2 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S3
     #4 * t1 * t3 * t6 + (-t16 * t22 + (-(-0.768D3 * S13 - 0.1536D4 * S1
     #4) * nf * t11 / 0.96D2 - (-0.1536D4 * S23 - 0.768D3 * S24) * nf * 
     #t13 / 0.96D2) * S34 + 0.8D1 * t37 * t12 + 0.8D1 * t13 * S24 * S14 
     #* nf) * t1 * s * z - 0.15D2 / 0.2D1 * t48 * nf * t51 + ((0.26D2 / 
     #0.3D1 * t12 + 0.26D2 / 0.3D1 * t14 + 0.143D3 / 0.12D2 * t48 * nf) 
     #* S34 - (-0.248D3 * S24 + 0.1064D4 * S13 + 0.832D3 * S23) * nf * t
     #11 / 0.96D2 - (0.832D3 * S14 - 0.248D3 * S13 + 0.1064D4 * S24) * n
     #f * t13 / 0.96D2 - t82 / 0.96D2) * S12 + (-0.4D1 * t12 - 0.4D1 * t
     #14 - 0.4D1 / 0.3D1 * t48 * nf) * t22 + (-(-0.896D3 * S14 - 0.2448D
     #4 * S13 - 0.768D3 * S23 - 0.295712D6 * S24) * nf * t11 / 0.96D2 - 
     #(-0.768D3 * S14 - 0.2448D4 * S24 - 0.295712D6 * S13 - 0.896D3 * S2
     #3) * nf * t13 / 0.96D2 + 0.15D2 / 0.2D1 * t80 * nf) * S34 - (-0.29
     #4112D6 * t76 + 0.2688D4 * t78 + 0.2448D4 * t37 - 0.192D3 * t79 - 0
     #.296000D6 * t77 + 0.384D3 * t118 + t121 - 0.1400D4 * t122) * nf * 
     #t11 / 0.96D2 - (t121 + 0.2688D4 * t76 - 0.192D3 * t77 + 0.2448D4 *
     # t122 - 0.296000D6 * t79 - 0.1400D4 * t37 + 0.384D3 * t133 - 0.294
     #112D6 * t78) * nf * t13 / 0.96D2 + ((-0.14D2 / 0.3D1 * t12 - 0.14D
     #2 / 0.3D1 * t14 - 0.37D2 / 0.12D2 * t48 * nf) * t22 * S34 + (-(0.1
     #15318D6 * S24 + 0.192D3 * S23 + 0.1278D4 * S13 + 0.1152D4 * S14) *
     # nf * t11 / 0.96D2 - (0.1152D4 * S23 + 0.192D3 * S14 + 0.1278D4 * 
     #S24 + 0.115318D6 * S13) * nf * t13 / 0.96D2 - t82 / 0.96D2) * t22 
     #+ (-(t165 - 0.102272D6 * t122 + 0.4364D4 * t76 - t168 - t169 - 0.5
     #08D3 * t37 - 0.568D3 * t79 - 0.2176D4 * t78 + 0.117052D6 * t77) * 
     #nf * t11 / 0.96D2 - (0.4364D4 * t78 - t169 - 0.102272D6 * t37 - 0.
     #2176D4 * t76 - 0.508D3 * t122 + 0.117052D6 * t79 - 0.568D3 * t77 -
     # t165 + t168) * nf * t13 / 0.96D2) * S34 - t219 * nf * t11 / 0.96D
     #2 - t239 * nf * t13 / 0.96D2) * t1 + (-(0.2160D4 * t213 * S23 + 0.
     #2160D4 * t207 * S23 + 0.720D3 * t233 * S13 + 0.720D3 * S14 * t189 
     #+ 0.1440D4 * t79 * t133 + 0.720D3 * t78 * t118) * nf * t11 / 0.96D
     #2 - (0.720D3 * t76 * t133 + 0.2160D4 * t205 * S14 + 0.720D3 * S23 
     #* t191 + 0.1440D4 * t77 * t118 + 0.720D3 * t215 * S24 + 0.2160D4 *
     # t195 * S14) * nf * t13 / 0.96D2) / t51
      rrgg2qqbarh51J6 = t279 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J7
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.20D2 * t12 - 0.20D2 * t14
      t22 = S34 ** 2
      t37 = S23 * S13
      t50 = S24 + S13
      t53 = 0.15D2 / 0.2D1 * t50 * nf
      t97 = 0.8D1 * t37
      t98 = S13 * S14
      t100 = S23 * S24
      t102 = S24 * S14
      t104 = S24 ** 2
      t106 = S23 ** 2
      t109 = 0.64D2 * S14 * S23
      t110 = S13 ** 2
      t111 = 0.128D3 * t110
      t118 = 0.8D1 * t102
      t119 = 0.128D3 * t104
      t121 = S14 ** 2
      t173 = t104 * S24
      t177 = t106 * S24
      t181 = t110 * S14
      t185 = S13 * t121
      t187 = t110 * S13
      t191 = S23 * t104
      t193 = 0.32D2 * t106 * S14 + 0.304D3 * t173 - 0.25296D5 * t104 * S
     #14 - 0.28208D5 * t177 + 0.16D2 * t106 * S23 - 0.128D3 * t181 - 0.4
     #D1 * S13 * t106 - 0.64D2 * t185 - 0.64D2 * t187 - 0.25264D5 * t102
     # * S23 - 0.27920D5 * t191
      t213 = 0.16D2 * t121 * S14 - 0.25296D5 * t110 * S23 - 0.28208D5 * 
     #t185 - 0.27920D5 * t181 + 0.32D2 * S23 * t121 - 0.4D1 * S24 * t121
     # - 0.128D3 * t191 - 0.64D2 * t173 - 0.64D2 * t177 + 0.304D3 * t187
     # - 0.25264D5 * t98 * S23
      rrgg2qqbarh51J7 = (0.20D2 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * 
     #z + t16 * S34 * t1 * t3 * t6 + (-t16 * t22 + (-0.5D1 / 0.96D2 * (-
     #0.512D3 * S13 - 0.384D3 * S14) * nf * t11 - 0.5D1 / 0.96D2 * (-0.5
     #12D3 * S24 - 0.384D3 * S23) * nf * t13) * S34 - 0.20D2 / 0.3D1 * t
     #37 * t12 - 0.20D2 / 0.3D1 * t13 * S24 * S14 * nf) * t1 * s * z + (
     #(0.20D2 / 0.3D1 * t12 + 0.20D2 / 0.3D1 * t14 - t53) * S34 - 0.5D1 
     #/ 0.96D2 * (-0.64D2 * S13 + 0.128D3 * S23 - 0.32D2 * S24) * nf * t
     #11 - 0.5D1 / 0.96D2 * (-0.32D2 * S13 - 0.64D2 * S24 + 0.128D3 * S1
     #4) * nf * t13) * S12 + (-0.5D1 * t12 - 0.5D1 * t14 + 0.15D2 * t50 
     #* nf) * t22 + (-0.5D1 / 0.96D2 * (-0.192D3 * S23 + 0.8D1 * S13 - 0
     #.64D2 * S14 - 0.74288D5 * S24) * nf * t11 - 0.5D1 / 0.96D2 * (0.8D
     #1 * S24 - 0.64D2 * S23 - 0.74288D5 * S13 - 0.192D3 * S14) * nf * t
     #13) * S34 - 0.5D1 / 0.96D2 * (-t97 - 0.128D3 * t98 - 0.74208D5 * t
     #100 - 0.80D2 * t102 - 0.74320D5 * t104 + 0.96D2 * t106 + t109 - t1
     #11) * nf * t11 - 0.5D1 / 0.96D2 * (t109 - 0.74320D5 * t110 - 0.742
     #08D5 * t98 - t118 - t119 - 0.80D2 * t37 + 0.96D2 * t121 - 0.128D3 
     #* t100) * nf * t13 + ((-0.35D2 / 0.6D1 * t12 - 0.35D2 / 0.6D1 * t1
     #4 - t53) * t22 * S34 + (-0.5D1 / 0.96D2 * (0.48D2 * S23 + 0.252D3 
     #* S13 + 0.28792D5 * S24 + 0.288D3 * S14) * nf * t11 - 0.5D1 / 0.96
     #D2 * (0.48D2 * S14 + 0.28792D5 * S13 + 0.288D3 * S23 + 0.252D3 * S
     #24) * nf * t13) * t22 + (-0.5D1 / 0.96D2 * (0.256D3 * t98 - 0.48D2
     # * t106 + 0.536D3 * t100 + 0.128D3 * t121 - t109 + t97 - 0.25248D5
     # * t102 + t111 + 0.29128D5 * t104) * nf * t11 - 0.5D1 / 0.96D2 * (
     #0.536D3 * t98 + 0.256D3 * t100 - t109 + t118 + 0.29128D5 * t110 - 
     #0.48D2 * t121 - 0.25248D5 * t37 + 0.128D3 * t106 + t119) * nf * t1
     #3) * S34 - 0.5D1 / 0.96D2 * t193 * nf * t11 - 0.5D1 / 0.96D2 * t21
     #3 * nf * t13) * t1) / pi * wd / z

      end function
  
 