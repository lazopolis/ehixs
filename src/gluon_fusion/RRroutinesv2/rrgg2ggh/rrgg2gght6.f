      subroutine rrgg2gght6
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt6
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt6
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhard61J1  
      doubleprecision rrgg2gghhard61J2  
      doubleprecision rrgg2gghhard61J3  
      doubleprecision rrgg2gghhard61J4  
      doubleprecision rrgg2gghhard61J5  
      doubleprecision rrgg2gghhard61J6  
      doubleprecision rrgg2gghhard61J7  
      doubleprecision rrgg2gghhardt6s1e1  
      doubleprecision rrgg2gghhardt6s1e0  
      doubleprecision rrgg2gghhardt6s1em1  
      doubleprecision rrgg2gghhardt6s1em2  
      doubleprecision rrgg2gghhardt6s1em3  
      doubleprecision rrgg2gghhardt6s1em4  
      doubleprecision rrgg2gghhardt6s2e1  
      doubleprecision rrgg2gghhardt6s2e0  
      doubleprecision rrgg2gghhardt6s2em1  
      doubleprecision rrgg2gghhardt6s2em2  
      doubleprecision rrgg2gghhardt6s2em3  
      doubleprecision rrgg2gghhardt6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt6s1e1
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t10 = pi * lh
      t11 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t12 = t5 * t11
      t15 = lh ** 2
      t17 = pi ** 2
      t19 = -0.180D3 * t15 + 0.30D2 * t17
      t20 = pi * t19
      t21 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t22 = t5 * t21
      t25 = x4 * pi
      t26 = Sin(t25)
      t27 = t26 ** 2
      t28 = x3 * t27
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t33 = log(0.4D1 * t28 * t30)
      t34 = 0.1D1 / z
      t36 = -0.1D1 + x3
      t37 = 0.1D1 / t36
      t41 = log(-0.4D1 * t28 * t30 * t37)
      t42 = cos(t25)
      t43 = z * t42
      t45 = Sqrt(-x3 * t36)
      t49 = 0.1D1 / (-x3 - z + 0.2D1 * t43 * t45)
      t53 = t41 ** 2
      t56 = t33 ** 2
      t65 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t73 = -0.60D2 * lh * t17 + 0.240D3 * zeta3 + 0.120D3 * t15 * lh
      t74 = pi * t73
      t93 = 0.1D1 / x3
      t96 = t30 * t27
      t98 = log(0.4D1 * t96)
      t99 = t98 ** 2
      t100 = t99 * t34
      t103 = t34 * t65
      t105 = t99 * t98 * t34
      t108 = t98 * t34
      t114 = t34 * t7
      t121 = t34 * t11
      t126 = t34 * t21
      t129 = t17 ** 2
      t130 = t15 ** 2
      t137 = rrgg2gghhard61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 
     #0.0D0, 0.0D0, 0.0D0)
      t144 = t99 ** 2
      t154 = x1 ** 2
      t155 = x3 * t154
      t156 = t96 * t37
      t159 = log(-0.4D1 * t155 * t156)
      t161 = t159 ** 2
      t168 = log(0.4D1 * t155 * t96)
      t169 = t168 * t34
      t171 = t168 ** 2
      t187 = t126 + t21 * t49
      t188 = t5 * t187
      t192 = 0.1D1 / x1
      t195 = pi * t34
      t196 = t19 * t5
      t197 = t154 * t27
      t198 = t197 * t30
      t200 = log(0.4D1 * t198)
      t205 = t200 ** 2
      t216 = t73 * t5
      t218 = t195 * t216 * t21
      t219 = lh * t5
      t230 = x2 ** 2
      t231 = x3 * t230
      t232 = t231 * t154
      t235 = log(-0.4D1 * t232 * t156)
      t239 = t231 * t198
      t241 = log(0.4D1 * t239)
      t251 = 0.1D1 / x2
      t252 = t192 * t251
      t255 = t230 * t154
      t258 = log(0.4D1 * t255 * t96)
      t259 = t258 * t34
      t261 = t258 ** 2
      t273 = t5 * t34
      t282 = log(-0.4D1 * t231 * t156)
      t284 = t282 ** 2
      t291 = log(0.4D1 * t231 * t96)
      t292 = t291 * t34
      t294 = t291 ** 2
      t316 = t230 * t27
      t319 = log(0.4D1 * t316 * t30)
      t320 = t319 * t34
      t325 = t319 ** 2
      t326 = t325 * t34
      t347 = -((0.90D2 * t6 * t7 - 0.180D3 * t10 * t12 - t20 * t22) * (-
     #t33 * t34 - t41 * t49) + 0.90D2 * t6 * t21 * (-t53 * t41 * t49 / 0
     #.6D1 - t56 * t33 * t34 / 0.6D1) + (-t20 * t12 + 0.90D2 * t6 * t65 
     #- t74 * t22 - 0.180D3 * t10 * t5 * t7) * (t34 + t49) + (0.90D2 * t
     #6 * t11 - 0.180D3 * t10 * t22) * (t56 * t34 / 0.2D1 + t53 * t49 / 
     #0.2D1)) * t93 / 0.2880D4 - (0.180D3 * (-t100 * t11 / 0.2D1 - t103 
     #+ t105 * t21 / 0.6D1 + t108 * t7) * pi * lh + (-t114 + t108 * t11 
     #- t100 * t21 / 0.2D1) * pi * t19 + (-t121 + t108 * t21) * pi * t73
     # - t126 * pi * (-0.480D3 * lh * zeta3 - t129 - 0.60D2 * t130 + 0.6
     #0D2 * t15 * t17) - 0.90D2 * (-t34 * t137 + t105 * t11 / 0.6D1 - t1
     #00 * t7 / 0.2D1 + t108 * t65 - t144 * t34 * t21 / 0.24D2) * pi) * 
     #t5 / 0.2880D4 + (-0.90D2 * t6 * (-(-t7 + t159 * t11 - t161 * t21 /
     # 0.2D1) * t49 + t114 - t169 * t11 + t171 * t34 * t21 / 0.2D1) + 0.
     #180D3 * t10 * t5 * (-(-t11 + t159 * t21) * t49 + t121 - t169 * t21
     #) + t20 * t188) * t93 * t192 / 0.1440D4 + (t195 * t196 * (t11 - t2
     #00 * t21) - 0.90D2 * t195 * t5 * (t205 * t11 / 0.2D1 + t65 - t205 
     #* t200 * t21 / 0.6D1 - t200 * t7) + t218 + 0.180D3 * t195 * t219 *
     # (t7 - t200 * t11 + t205 * t21 / 0.2D1)) * t192 / 0.1440D4 + (-0.9
     #0D2 * t6 * (-(-t11 + t235 * t21) * t49 + t121 - t241 * t34 * t21) 
     #+ 0.180D3 * t10 * t188) * t93 * t252 / 0.720D3 + (-0.90D2 * t6 * (
     #t114 - t259 * t11 + t261 * t34 * t21 / 0.2D1) + 0.180D3 * t10 * t5
     # * (t121 - t259 * t21) + t20 * t273 * t21) * t192 * t251 / 0.720D3
     # - (-0.90D2 * t6 * ((-t7 + t282 * t11 - t284 * t21 / 0.2D1) * t49 
     #- t114 + t292 * t11 - t294 * t34 * t21 / 0.2D1) + 0.180D3 * t10 * 
     #t5 * ((-t11 + t282 * t21) * t49 - t121 + t292 * t21) - t20 * t5 * 
     #t187) * t93 * t251 / 0.1440D4 - (t20 * t5 * (-t121 + t320 * t21) -
     # 0.90D2 * t6 * (-t326 * t11 / 0.2D1 - t103 + t325 * t319 * t34 * t
     #21 / 0.6D1 + t320 * t7) - t218 + 0.180D3 * t10 * t5 * (-t114 + t32
     #0 * t11 - t326 * t21 / 0.2D1)) * t251 / 0.1440D4
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
      t366 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #0, t354, 0.0D0, -t365)
      t367 = t34 * t366
      t368 = t155 * t27
      t369 = t30 * t358
      t370 = t355 ** 2
      t371 = t369 * t370
      t374 = log(0.4D1 * t368 * t371)
      t375 = t374 * t34
      t376 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #0, t354, 0.0D0, -t365)
      t378 = t374 ** 2
      t380 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #0, t354, 0.0D0, -t365)
      t385 = t369 * t370 * t37
      t388 = log(-0.4D1 * t368 * t385)
      t389 = t388 * t357
      t391 = t388 ** 2
      t396 = x3 * t357
      t398 = Sqrt(-t396 * t36)
      t401 = x3 * x1
      t402 = t401 * z
      t403 = 0.3D1 * t402
      t404 = x1 * t29
      t405 = t401 * t29
      t407 = 0.2D1 * t155 * z
      t408 = t154 * t29
      t409 = t408 * x3
      t410 = 0.2D1 * t401
      t411 = -z - x3 + 0.2D1 * t43 * t398 + t356 - t403 - t404 + t405 + 
     #t407 - t409 + t410 - t155
      t412 = 0.1D1 / t411
      t417 = t34 * t376
      t419 = t357 * t376
      t431 = t5 * (-t357 * t380 * t412 - t34 * t380)
      t436 = (-0.90D2 * t6 * (-t367 + t375 * t376 - t378 * t34 * t380 / 
     #0.2D1 - (t357 * t366 - t389 * t376 + t391 * t357 * t380 / 0.2D1) *
     # t412) + 0.180D3 * t10 * t5 * (-t417 + t375 * t380 - (t419 - t389 
     #* t380) * t412) + t20 * t431) * t93 * t192 / 0.1440D4
      t439 = log(0.4D1 * t197 * t371)
      t441 = -t376 + t439 * t380
      t444 = t439 ** 2
      t447 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #0, t354, 0.0D0, -t365)
      t452 = -t444 * t376 / 0.2D1 - t447 + t444 * t439 * t380 / 0.6D1 + 
     #t439 * t366
      t457 = t195 * t216 * t380
      t461 = -t366 + t439 * t376 - t444 * t380 / 0.2D1
      t468 = t358 * t370
      t472 = log(0.4D1 * t232 * t96 * t468)
      t475 = t231 * t197
      t478 = log(-0.4D1 * t475 * t385)
      t491 = (-0.90D2 * t6 * (-t417 + t472 * t34 * t380 - (t419 - t478 *
     # t357 * t380) * t412) + 0.180D3 * t10 * t431) * t93 * t252 / 0.720
     #D3
      t492 = t255 * t27
      t495 = log(0.4D1 * t492 * t371)
      t496 = t495 * t34
      t498 = t495 ** 2
      t515 = (-0.90D2 * t6 * (-t367 + t496 * t376 - t498 * t34 * t380 / 
     #0.2D1) + 0.180D3 * t10 * t5 * (-t417 + t496 * t380) - t20 * t273 *
     # t380) * t192 * t251 / 0.720D3
      t516 = t436 + (t195 * t196 * t441 - 0.90D2 * t195 * t5 * t452 - t4
     #57 + 0.180D3 * t195 * t219 * t461) * t192 / 0.1440D4 + t491 + t515
      t517 = FJET(XB1, XB2, s, 0.0D0, t354, -t360, 0.0D0, -t365, t516)
      t519 = x2 * s
      t520 = t519 * t1
      t521 = -0.1D1 + x2
      t522 = t521 * s
      t523 = t522 * t1
      t524 = x2 * z
      t526 = 0.1D1 / (t524 - x2 - z)
      t527 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t520, -t523
     #, 0.0D0, 0.0D0, 0.0D0)
      t528 = t526 * t527
      t529 = t96 * t521
      t532 = log(-0.4D1 * t232 * t529)
      t534 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t520, -t523
     #, 0.0D0, 0.0D0, 0.0D0)
      t540 = t5 * t526 * t534
      t546 = (-0.90D2 * t6 * (t528 - t532 * t526 * t534) + 0.180D3 * t10
     # * t540) * t93 * t252 / 0.720D3
      t547 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, t520, -t523
     #, 0.0D0, 0.0D0, 0.0D0)
      t548 = t526 * t547
      t551 = log(-0.4D1 * t255 * t529)
      t552 = t551 * t526
      t554 = t551 ** 2
      t566 = t20 * t540
      t570 = (-0.90D2 * t6 * (t548 - t552 * t527 + t554 * t526 * t534 / 
     #0.2D1) + 0.180D3 * t10 * t5 * (t528 - t552 * t534) + t566) * t192 
     #* t251 / 0.720D3
      t573 = log(-0.4D1 * t231 * t529)
      t574 = t573 * t526
      t576 = t573 ** 2
      t591 = (-0.90D2 * t6 * (-t548 + t574 * t527 - t576 * t526 * t534 /
     # 0.2D1) + 0.180D3 * t10 * t5 * (-t528 + t574 * t534) - t566) * t93
     # * t251 / 0.1440D4
      t592 = t30 * t521
      t595 = log(-0.4D1 * t316 * t592)
      t596 = t595 * t526
      t598 = -t528 + t596 * t534
      t601 = t595 ** 2
      t602 = t601 * t526
      t605 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, t520, -t523
     #, 0.0D0, 0.0D0, 0.0D0)
      t612 = -t602 * t527 / 0.2D1 - t526 * t605 + t601 * t595 * t526 * t
     #534 / 0.6D1 + t596 * t547
      t615 = t74 * t540
      t619 = -t548 + t596 * t527 - t602 * t534 / 0.2D1
      t626 = t546 + t570 - t591 - (t20 * t5 * t598 - 0.90D2 * t6 * t612 
     #- t615 + 0.180D3 * t10 * t5 * t619) * t251 / 0.1440D4
      t627 = FJET(XB1, XB2, s, 0.0D0, t520, 0.0D0, -t523, 0.0D0, t626)
      t629 = x2 * x3
      t632 = Sqrt(x3 * t521 * t36)
      t633 = t42 * t632
      t635 = 0.2D1 * t633 * x2
      t637 = 0.1D1 - x3 + t629
      t638 = 0.1D1 / t637
      t640 = t2 * (0.1D1 - x3 - x2 + t629 + t231 + t635) * t638
      t645 = t2 * x2 * (-0.1D1 + t629 + 0.2D1 * t633) * t638
      t646 = t629 * z
      t648 = z * t230 * x3
      t655 = 0.1D1 / (-t524 - t646 + t648 + x2 - t231 + x3 + z - 0.2D1 *
     # t43 * t632 + 0.2D1 * t43 * t632 * x2 - t635)
      t656 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t645, t640
     #, 0.0D0, 0.0D0, 0.0D0)
      t657 = t655 * t656
      t658 = t637 ** 2
      t659 = 0.1D1 / t658
      t661 = t592 * t36 * t659
      t664 = log(0.4D1 * t475 * t661)
      t666 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t645, t640
     #, 0.0D0, 0.0D0, 0.0D0)
      t672 = t5 * t655 * t666
      t678 = (-0.90D2 * t6 * (t657 - t664 * t655 * t666) + 0.180D3 * t10
     # * t672) * t93 * t252 / 0.720D3
      t679 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, -t645, t640
     #, 0.0D0, 0.0D0, 0.0D0)
      t684 = log(0.4D1 * t231 * t27 * t661)
      t685 = t684 * t655
      t687 = t684 ** 2
      t691 = -t655 * t679 + t685 * t656 - t687 * t655 * t666 / 0.2D1
      t695 = -t657 + t685 * t666
      t699 = t20 * t672
      t704 = t678 - (-0.90D2 * t6 * t691 + 0.180D3 * t10 * t5 * t695 - t
     #699) * t93 * t251 / 0.1440D4
      t705 = FJET(XB1, XB2, s, 0.0D0, t640, 0.0D0, -t645, 0.0D0, t704)
      t707 = t1 * t355
      t709 = t522 * t707 * t358
      t710 = t519 * t707
      t712 = t362 * t521 * t364
      t713 = x1 * x2
      t714 = t713 * z
      t716 = 0.1D1 / (x2 - t713 + z - t524 + t714)
      t717 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t710, t709
     #, t354, 0.0D0, t712)
      t718 = t716 * t717
      t720 = t369 * t370 * t521
      t723 = log(-0.4D1 * t475 * t720)
      t725 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t710, t709
     #, t354, 0.0D0, t712)
      t731 = t5 * t716 * t725
      t737 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, -t710, t709
     #, t354, 0.0D0, t712)
      t741 = log(-0.4D1 * t492 * t720)
      t742 = t741 * t716
      t744 = t741 ** 2
      t761 = (-0.90D2 * t6 * (t718 - t723 * t716 * t725) + 0.180D3 * t10
     # * t731) * t93 * t252 / 0.720D3 + (-0.90D2 * t6 * (t716 * t737 - t
     #742 * t717 + t744 * t716 * t725 / 0.2D1) + 0.180D3 * t10 * t5 * (t
     #718 - t742 * t725) + t20 * t731) * t192 * t251 / 0.720D3
      t762 = FJET(XB1, XB2, s, 0.0D0, t709, t354, -t710, t712, t761)
      t764 = FJET(XB1, XB2, s, 0.0D0, -t523, 0.0D0, t520, 0.0D0, t626)
      t766 = FJET(XB1, XB2, s, 0.0D0, -t360, t354, 0.0D0, -t365, t516)
      t768 = FJET(XB1, XB2, s, 0.0D0, -t645, 0.0D0, t640, 0.0D0, t704)
      t770 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t347)
      t786 = t436 + (t195 * t196 * t441 - 0.90D2 * t195 * t5 * t452 - t4
     #57 + 0.180D3 * t195 * t219 * t461) * t192 / 0.1440D4 + t491 + t515
      t787 = FJET(XB1, XB2, s, t354, 0.0D0, 0.0D0, -t360, -t365, t786)
      t789 = t348 * t347 + t350 * t347 + t352 * t347 + t517 * t516 + t62
     #7 * t626 + t705 * t704 + t762 * t761 + t764 * t626 + t766 * t516 +
     # t768 * t704 + t770 * t347 + t787 * t786
      t790 = FJET(XB1, XB2, s, t354, -t710, 0.0D0, t709, t712, t761)
      t805 = t546 + t570 - t591 - (t20 * t5 * t598 - 0.90D2 * t6 * t612 
     #- t615 + 0.180D3 * t10 * t5 * t619) * t251 / 0.1440D4
      t806 = FJET(XB1, XB2, s, t520, 0.0D0, -t523, 0.0D0, 0.0D0, t805)
      t808 = FJET(XB1, XB2, s, t640, 0.0D0, -t645, 0.0D0, 0.0D0, t704)
      t810 = FJET(XB1, XB2, s, t709, 0.0D0, -t710, t354, t712, t761)
      t813 = t354 * t629 * t638
      t814 = t2 * t355
      t815 = t521 * t36
      t817 = Sqrt(t396 * t815)
      t818 = t42 * t817
      t820 = 0.2D1 * t818 * x2
      t821 = t356 * t231
      t823 = x1 * t230 * x3
      t827 = t814 * (t820 + 0.1D1 - x3 - x2 + t629 + t821 - t823 + t231)
     # * t358 * t638
      t831 = t36 * s * t1 * x1 * t638
      t837 = t814 * x2 * (-0.1D1 + t629 + x1 - t401 - t356 + t402 + 0.2D
     #1 * t818) * t358 * t638
      t852 = t524 + t231 + 0.2D1 * t713 + t821 + 0.2D1 * t401 * t524 - t
     #401 * x2 * t29 - 0.2D1 * t155 * t524 + t408 * t629 - 0.2D1 * t818 
     #* t713 - 0.2D1 * t43 * t817 * x2 + t646 - t648 - 0.3D1 * t714 - t4
     #03 + t405 + t407 - t409
      t860 = x2 * t154
      t866 = -z - x3 + 0.2D1 * t43 * t817 * x1 * x2 + t356 - t404 + t410
     # - t155 + t820 - t823 - t401 * x2 + t155 * x2 + t713 * t29 + 0.2D1
     # * t860 * z - t860 * t29 + 0.2D1 * t43 * t817 - x2 - t860
      t868 = 0.1D1 / (t852 + t866)
      t869 = t357 * t868
      t870 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t837, -t827
     #, -t831, t813, t712)
      t876 = log(0.4D1 * t239 * t468 * t815 * t659)
      t878 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t837, -t827
     #, -t831, t813, t712)
      t888 = -0.90D2 * t6 * (t869 * t870 - t876 * t357 * t868 * t878) + 
     #0.180D3 * t10 * t5 * t869 * t878
      t891 = t888 * t93 * t252 / 0.720D3
      t892 = FJET(XB1, XB2, s, t813, -t827, -t831, t837, t712, t891)
      t895 = t93 * t192 * t251
      t898 = FJET(XB1, XB2, s, t837, -t831, -t827, t813, t712, t891)
      t902 = FJET(XB1, XB2, s, -t523, 0.0D0, t520, 0.0D0, 0.0D0, t805)
      t904 = FJET(XB1, XB2, s, -t360, 0.0D0, 0.0D0, t354, -t365, t786)
      t906 = FJET(XB1, XB2, s, -t710, t354, t709, 0.0D0, t712, t761)
      t919 = t678 - (-0.90D2 * t6 * t691 + 0.180D3 * t10 * t5 * t695 - t
     #699) * t93 * t251 / 0.1440D4
      t920 = FJET(XB1, XB2, s, -t645, 0.0D0, t640, 0.0D0, 0.0D0, t919)
      t922 = FJET(XB1, XB2, s, -t831, t837, t813, -t827, t712, t891)
      t926 = FJET(XB1, XB2, s, -t827, t813, t837, -t831, t712, t891)
      t930 = t790 * t761 + t806 * t805 + t808 * t704 + t810 * t761 + t89
     #2 * t888 * t895 / 0.720D3 + t898 * t888 * t895 / 0.720D3 + t902 * 
     #t805 + t904 * t786 + t906 * t761 + t920 * t919 + t922 * t888 * t89
     #5 / 0.720D3 + t926 * t888 * t895 / 0.720D3
      rrgg2gghhardt6s1e1 = t789 + t930

      end function



      doubleprecision function rrgg2gghhardt6s1e0
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = t16 ** 2
      t18 = 0.1D1 / z
      t20 = -0.1D1 + x3
      t21 = 0.1D1 / t20
      t25 = log(-0.4D1 * t11 * t13 * t21)
      t26 = t25 ** 2
      t27 = cos(t8)
      t28 = z * t27
      t30 = Sqrt(-x3 * t20)
      t34 = 0.1D1 / (-x3 - z + 0.2D1 * t28 * t30)
      t41 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t44 = pi * lh
      t45 = t5 * t7
      t53 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t59 = lh ** 2
      t61 = pi ** 2
      t63 = -0.180D3 * t59 + 0.30D2 * t61
      t64 = pi * t63
      t70 = 0.1D1 / x3
      t73 = t18 * t53
      t74 = t13 * t10
      t76 = log(0.4D1 * t74)
      t77 = t76 * t18
      t79 = t76 ** 2
      t80 = t79 * t18
      t87 = t18 * t7
      t98 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t108 = t18 * t41
      t116 = x2 ** 2
      t117 = t116 * x3
      t118 = t74 * t21
      t121 = log(-0.4D1 * t117 * t118)
      t127 = log(0.4D1 * t117 * t74)
      t134 = -t87 - t34 * t7
      t140 = 0.1D1 / x2
      t143 = t116 * t10
      t146 = log(0.4D1 * t143 * t13)
      t147 = t146 * t18
      t149 = t146 ** 2
      t161 = t5 * t18
      t162 = t161 * t7
      t163 = t64 * t162
      t167 = x1 ** 2
      t168 = x3 * t167
      t171 = log(-0.4D1 * t168 * t118)
      t177 = log(0.4D1 * t168 * t74)
      t183 = -t134
      t189 = 0.1D1 / x1
      t194 = t70 * t189 * t140
      t197 = t116 * t167
      t200 = log(0.4D1 * t197 * t74)
      t212 = pi * t18
      t213 = t167 * t10
      t216 = log(0.4D1 * t213 * t13)
      t218 = t216 ** 2
      t225 = lh * t5
      t234 = -(0.90D2 * t6 * t7 * (t17 * t18 / 0.2D1 + t26 * t34 / 0.2D1
     #) + (0.90D2 * t6 * t41 - 0.180D3 * t44 * t45) * (-t16 * t18 - t25 
     #* t34) + (0.90D2 * t6 * t53 - 0.180D3 * t44 * t5 * t41 - t64 * t45
     #) * (t18 + t34)) * t70 / 0.2880D4 - (0.180D3 * (-t73 + t77 * t41 -
     # t80 * t7 / 0.2D1) * pi * lh - t87 * pi * (-0.60D2 * lh * t61 + 0.
     #240D3 * zeta3 + 0.120D3 * t59 * lh) - 0.90D2 * (-t80 * t41 / 0.2D1
     # - t18 * t98 + t79 * t76 * t18 * t7 / 0.6D1 + t77 * t53) * pi + (-
     #t108 + t77 * t7) * pi * t63) * t5 / 0.2880D4 - (-0.90D2 * t6 * ((-
     #t41 + t121 * t7) * t34 - t108 + t127 * t18 * t7) + 0.180D3 * t44 *
     # t5 * t134) * t70 * t140 / 0.1440D4 - (-0.90D2 * t6 * (-t73 + t147
     # * t41 - t149 * t18 * t7 / 0.2D1) + 0.180D3 * t44 * t5 * (-t108 + 
     #t147 * t7) - t163) * t140 / 0.1440D4 + (-0.90D2 * t6 * (-(-t41 + t
     #171 * t7) * t34 + t108 - t177 * t18 * t7) + 0.180D3 * t44 * t5 * t
     #183) * t70 * t189 / 0.1440D4 - t6 * t183 * t194 / 0.8D1 + (-0.90D2
     # * t6 * (t108 - t200 * t18 * t7) + 0.180D3 * t44 * t162) * t189 * 
     #t140 / 0.720D3 + (-0.90D2 * t212 * t5 * (t53 - t216 * t41 + t218 *
     # t7 / 0.2D1) + 0.180D3 * t212 * t225 * (t41 - t216 * t7) + t163) *
     # t189 / 0.1440D4
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
      t253 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t24
     #7, t241, 0.0D0, -t252)
      t254 = t18 * t253
      t255 = t168 * t10
      t256 = t13 * t245
      t257 = t242 ** 2
      t258 = t256 * t257
      t261 = log(0.4D1 * t255 * t258)
      t263 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t24
     #7, t241, 0.0D0, -t252)
      t270 = log(-0.4D1 * t255 * t256 * t257 * t21)
      t274 = x3 * t244
      t276 = Sqrt(-t274 * t20)
      t279 = x3 * x1
      t280 = t279 * z
      t281 = 0.3D1 * t280
      t282 = x1 * t12
      t283 = t279 * t12
      t285 = 0.2D1 * t168 * z
      t286 = t167 * t12
      t287 = t286 * x3
      t288 = 0.2D1 * t279
      t289 = -z - x3 + 0.2D1 * t28 * t276 + t243 - t281 - t282 + t283 + 
     #t285 - t287 + t288 - t168
      t290 = 0.1D1 / t289
      t298 = -t244 * t263 * t290 - t18 * t263
      t305 = (-0.90D2 * t6 * (-t254 + t261 * t18 * t263 - (t244 * t253 -
     # t270 * t244 * t263) * t290) + 0.180D3 * t44 * t5 * t298) * t70 * 
     #t189 / 0.1440D4
      t308 = t6 * t298 * t194 / 0.8D1
      t309 = t197 * t10
      t312 = log(0.4D1 * t309 * t258)
      t318 = t161 * t263
      t324 = (-0.90D2 * t6 * (-t254 + t312 * t18 * t263) - 0.180D3 * t44
     # * t318) * t189 * t140 / 0.720D3
      t325 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t24
     #7, t241, 0.0D0, -t252)
      t328 = log(0.4D1 * t213 * t258)
      t330 = t328 ** 2
      t333 = -t325 + t328 * t253 - t330 * t263 / 0.2D1
      t338 = -t253 + t328 * t263
      t342 = t64 * t318
      t346 = t305 - t308 + t324 + (-0.90D2 * t212 * t5 * t333 + 0.180D3 
     #* t212 * t225 * t338 - t342) * t189 / 0.1440D4
      t347 = FJET(XB1, XB2, s, 0.0D0, t241, -t247, 0.0D0, -t252, t346)
      t349 = x2 * s
      t350 = t349 * t1
      t351 = -0.1D1 + x2
      t352 = t351 * s
      t353 = t352 * t1
      t354 = x2 * z
      t356 = 0.1D1 / (t354 - x2 - z)
      t357 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t350, -t353
     #, 0.0D0, 0.0D0, 0.0D0)
      t358 = t356 * t357
      t359 = t74 * t351
      t362 = log(-0.4D1 * t117 * t359)
      t364 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t350, -t353
     #, 0.0D0, 0.0D0, 0.0D0)
      t370 = t5 * t356 * t364
      t372 = 0.180D3 * t44 * t370
      t376 = (-0.90D2 * t6 * (-t358 + t362 * t356 * t364) - t372) * t70 
     #* t140 / 0.1440D4
      t377 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, t350, -t353
     #, 0.0D0, 0.0D0, 0.0D0)
      t379 = t13 * t351
      t382 = log(-0.4D1 * t143 * t379)
      t383 = t382 * t356
      t385 = t382 ** 2
      t389 = -t356 * t377 + t383 * t357 - t385 * t356 * t364 / 0.2D1
      t393 = -t358 + t383 * t364
      t397 = t64 * t370
      t403 = t189 * t140
      t406 = t6 * t356 * t364 * t70 * t403 / 0.8D1
      t409 = log(-0.4D1 * t197 * t359)
      t418 = (-0.90D2 * t6 * (t358 - t409 * t356 * t364) + t372) * t189 
     #* t140 / 0.720D3
      t419 = -t376 - (-0.90D2 * t6 * t389 + 0.180D3 * t44 * t5 * t393 - 
     #t397) * t140 / 0.1440D4 - t406 + t418
      t420 = FJET(XB1, XB2, s, 0.0D0, t350, 0.0D0, -t353, 0.0D0, t419)
      t422 = x2 * x3
      t425 = Sqrt(x3 * t351 * t20)
      t426 = t27 * t425
      t428 = 0.2D1 * t426 * x2
      t430 = 0.1D1 - x3 + t422
      t431 = 0.1D1 / t430
      t433 = t2 * (0.1D1 - x3 - x2 + t422 + t117 + t428) * t431
      t438 = t2 * x2 * (-0.1D1 + t422 + 0.2D1 * t426) * t431
      t439 = t422 * z
      t441 = z * t116 * x3
      t448 = 0.1D1 / (-t354 - t439 + t441 + x2 - t117 + x3 + z - 0.2D1 *
     # t28 * t425 + 0.2D1 * t28 * t425 * x2 - t428)
      t449 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t438, t433
     #, 0.0D0, 0.0D0, 0.0D0)
      t452 = t430 ** 2
      t458 = log(0.4D1 * t117 * t10 * t379 * t20 / t452)
      t460 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t438, t433
     #, 0.0D0, 0.0D0, 0.0D0)
      t462 = -t448 * t449 + t458 * t448 * t460
      t468 = 0.180D3 * t44 * t5 * t448 * t460
      t477 = t6 * t448 * t460 * t70 * t403 / 0.8D1
      t478 = -(-0.90D2 * t6 * t462 - t468) * t70 * t140 / 0.1440D4 - t47
     #7
      t479 = FJET(XB1, XB2, s, 0.0D0, t433, 0.0D0, -t438, 0.0D0, t478)
      t481 = t1 * t242
      t483 = t352 * t481 * t245
      t484 = t349 * t481
      t486 = t249 * t351 * t251
      t487 = x1 * x2
      t488 = t487 * z
      t490 = 0.1D1 / (x2 - t487 + z - t354 + t488)
      t492 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t484, t483
     #, t241, 0.0D0, t486)
      t497 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t484, t483
     #, t241, 0.0D0, t486)
      t503 = log(-0.4D1 * t309 * t256 * t257 * t351)
      t517 = -t6 * t490 * t492 * t70 * t403 / 0.8D1 + (-0.90D2 * t6 * (t
     #490 * t497 - t503 * t490 * t492) + 0.180D3 * t44 * t5 * t490 * t49
     #2) * t189 * t140 / 0.720D3
      t518 = FJET(XB1, XB2, s, 0.0D0, t483, t241, -t484, t486, t517)
      t520 = FJET(XB1, XB2, s, 0.0D0, -t353, 0.0D0, t350, 0.0D0, t419)
      t522 = FJET(XB1, XB2, s, 0.0D0, -t247, t241, 0.0D0, -t252, t346)
      t524 = FJET(XB1, XB2, s, 0.0D0, -t438, 0.0D0, t433, 0.0D0, t478)
      t526 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t234)
      t539 = t305 - t308 + t324 + (-0.90D2 * t212 * t5 * t333 + 0.180D3 
     #* t212 * t225 * t338 - t342) * t189 / 0.1440D4
      t540 = FJET(XB1, XB2, s, t241, 0.0D0, 0.0D0, -t247, -t252, t539)
      t542 = t235 * t234 + t237 * t234 + t239 * t234 + t347 * t346 + t42
     #0 * t419 + t479 * t478 + t518 * t517 + t520 * t419 + t522 * t346 +
     # t524 * t478 + t526 * t234 + t540 * t539
      t543 = FJET(XB1, XB2, s, t241, -t484, 0.0D0, t483, t486, t517)
      t555 = -t376 - (-0.90D2 * t6 * t389 + 0.180D3 * t44 * t5 * t393 - 
     #t397) * t140 / 0.1440D4 - t406 + t418
      t556 = FJET(XB1, XB2, s, t350, 0.0D0, -t353, 0.0D0, 0.0D0, t555)
      t558 = FJET(XB1, XB2, s, t433, 0.0D0, -t438, 0.0D0, 0.0D0, t478)
      t560 = FJET(XB1, XB2, s, t483, 0.0D0, -t484, t241, t486, t517)
      t563 = t241 * t422 * t431
      t564 = t2 * t242
      t567 = Sqrt(t274 * t351 * t20)
      t568 = t27 * t567
      t570 = 0.2D1 * t568 * x2
      t571 = t243 * t117
      t573 = x1 * t116 * x3
      t577 = t564 * (t570 + 0.1D1 - x3 - x2 + t422 + t571 - t573 + t117)
     # * t245 * t431
      t581 = t20 * s * t1 * x1 * t431
      t587 = t564 * x2 * (-0.1D1 + t422 + x1 - t279 - t243 + t280 + 0.2D
     #1 * t568) * t245 * t431
      t604 = t571 + 0.2D1 * t279 * t354 - t279 * x2 * t12 - 0.2D1 * t168
     # * t354 + t286 * t422 - 0.2D1 * t568 * t487 - 0.2D1 * t28 * t567 *
     # x2 + 0.2D1 * t28 * t567 * x1 * x2 - t281 + t283 + t285 - t287 - x
     #2 - z - x3 + t570 - t573
      t608 = x2 * t167
      t616 = -t279 * x2 + t168 * x2 + t487 * t12 + 0.2D1 * t608 * z - t6
     #08 * t12 + 0.2D1 * t28 * t567 + t117 + 0.2D1 * t487 - t282 + t288 
     #- t168 + t354 + t243 - 0.3D1 * t488 - t608 + t439 - t441
      t618 = 0.1D1 / (t604 + t616)
      t621 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t587, -t577
     #, -t581, t563, t486)
      t625 = t6 * t244 * t618 * t621 * t70 * t403 / 0.8D1
      t626 = FJET(XB1, XB2, s, t563, -t577, -t581, t587, t486, -t625)
      t628 = t5 * t244
      t631 = t618 * t621 * t194
      t634 = FJET(XB1, XB2, s, t587, -t581, -t577, t563, t486, -t625)
      t639 = FJET(XB1, XB2, s, -t353, 0.0D0, t350, 0.0D0, 0.0D0, t555)
      t641 = FJET(XB1, XB2, s, -t247, 0.0D0, 0.0D0, t241, -t252, t539)
      t643 = FJET(XB1, XB2, s, -t484, t241, t483, 0.0D0, t486, t517)
      t652 = -(-0.90D2 * t6 * t462 - t468) * t70 * t140 / 0.1440D4 - t47
     #7
      t653 = FJET(XB1, XB2, s, -t438, 0.0D0, t433, 0.0D0, 0.0D0, t652)
      t655 = FJET(XB1, XB2, s, -t581, t587, t563, -t577, t486, -t625)
      t660 = FJET(XB1, XB2, s, -t577, t563, t587, -t581, t486, -t625)
      t665 = t543 * t517 + t556 * t555 + t558 * t478 + t560 * t517 - t62
     #6 * pi * t628 * t631 / 0.8D1 - t634 * pi * t628 * t631 / 0.8D1 + t
     #639 * t555 + t641 * t539 + t643 * t517 + t653 * t652 - t655 * pi *
     # t628 * t631 / 0.8D1 - t660 * pi * t628 * t631 / 0.8D1
      rrgg2gghhardt6s1e0 = t542 + t665

      end function



      doubleprecision function rrgg2gghhardt6s1em1
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = 0.1D1 / z
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t11 * t13 / t19)
      t25 = cos(t8)
      t26 = z * t25
      t28 = Sqrt(-x3 * t19)
      t32 = 0.1D1 / (-x3 - z + 0.2D1 * t26 * t28)
      t38 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t41 = pi * lh
      t49 = 0.1D1 / x3
      t52 = t17 * t38
      t55 = log(0.4D1 * t13 * t10)
      t56 = t55 * t17
      t62 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t65 = t55 ** 2
      t72 = t17 * t7
      t73 = lh ** 2
      t75 = pi ** 2
      t84 = -t72 - t7 * t32
      t86 = 0.1D1 / x2
      t90 = x2 ** 2
      t91 = t90 * t10
      t94 = log(0.4D1 * t91 * t13)
      t100 = t5 * t17
      t103 = 0.180D3 * t41 * t100 * t7
      t107 = t6 * t17
      t108 = 0.1D1 / x1
      t113 = pi * t17
      t114 = x1 ** 2
      t115 = t114 * t10
      t118 = log(0.4D1 * t115 * t13)
      t132 = -(0.90D2 * t6 * t7 * (-t16 * t17 - t24 * t32) + (0.90D2 * t
     #6 * t38 - 0.180D3 * t41 * t5 * t7) * (t17 + t32)) * t49 / 0.2880D4
     # - (0.180D3 * (-t52 + t56 * t7) * pi * lh - 0.90D2 * (-t17 * t62 +
     # t56 * t38 - t65 * t17 * t7 / 0.2D1) * pi - t72 * pi * (-0.180D3 *
     # t73 + 0.30D2 * t75)) * t5 / 0.2880D4 + t6 * t84 * t49 * t86 / 0.1
     #6D2 - (-0.90D2 * t6 * (-t52 + t94 * t17 * t7) - t103) * t86 / 0.14
     #40D4 - t107 * t7 * t108 * t86 / 0.8D1 + (-0.90D2 * t113 * t5 * (t3
     #8 - t118 * t7) + t103) * t108 / 0.1440D4 + t6 * t84 * t49 * t108 /
     # 0.16D2
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
      t151 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t14
     #5, t139, 0.0D0, -t150)
      t155 = t107 * t151 * t108 * t86 / 0.8D1
      t156 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t14
     #5, t139, 0.0D0, -t150)
      t158 = t140 ** 2
      t162 = log(0.4D1 * t115 * t13 * t143 * t158)
      t164 = -t156 + t162 * t151
      t170 = 0.180D3 * t41 * t100 * t151
      t177 = Sqrt(-x3 * t142 * t19)
      t180 = x3 * x1
      t185 = x3 * t114
      t191 = -z - x3 + 0.2D1 * t26 * t177 + t141 - 0.3D1 * t180 * z - x1
     # * t12 + t180 * t12 + 0.2D1 * t185 * z - t114 * t12 * x3 + 0.2D1 *
     # t180 - t185
      t199 = t6 * (-t142 * t151 / t191 - t17 * t151) * t49 * t108 / 0.16
     #D2
      t200 = t155 + (-0.90D2 * t113 * t5 * t164 - t170) * t108 / 0.1440D
     #4 - t199
      t201 = FJET(XB1, XB2, s, 0.0D0, t139, -t145, 0.0D0, -t150, t200)
      t203 = x2 * s
      t204 = t203 * t1
      t205 = -0.1D1 + x2
      t206 = t205 * s
      t207 = t206 * t1
      t208 = x2 * z
      t210 = 0.1D1 / (t208 - x2 - z)
      t211 = t6 * t210
      t212 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t204, -t207
     #, 0.0D0, 0.0D0, 0.0D0)
      t216 = t211 * t212 * t49 * t86 / 0.16D2
      t217 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t204, -t207
     #, 0.0D0, 0.0D0, 0.0D0)
      t222 = log(-0.4D1 * t91 * t13 * t205)
      t225 = -t210 * t217 + t222 * t210 * t212
      t231 = 0.180D3 * t41 * t5 * t210 * t212
      t238 = t211 * t212 * t108 * t86 / 0.8D1
      t239 = -t216 - (-0.90D2 * t6 * t225 - t231) * t86 / 0.1440D4 - t23
     #8
      t240 = FJET(XB1, XB2, s, 0.0D0, t204, 0.0D0, -t207, 0.0D0, t239)
      t242 = x2 * x3
      t243 = t90 * x3
      t246 = Sqrt(x3 * t205 * t19)
      t247 = t25 * t246
      t249 = 0.2D1 * t247 * x2
      t252 = 0.1D1 / (0.1D1 - x3 + t242)
      t254 = t2 * (0.1D1 - x3 - x2 + t242 + t243 + t249) * t252
      t259 = t2 * x2 * (-0.1D1 + t242 + 0.2D1 * t247) * t252
      t269 = 0.1D1 / (-t208 - t242 * z + z * t90 * x3 + x2 - t243 + x3 +
     # z - 0.2D1 * t26 * t246 + 0.2D1 * t26 * t246 * x2 - t249)
      t271 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t259, t254
     #, 0.0D0, 0.0D0, 0.0D0)
      t275 = t6 * t269 * t271 * t49 * t86 / 0.16D2
      t276 = FJET(XB1, XB2, s, 0.0D0, t254, 0.0D0, -t259, 0.0D0, -t275)
      t281 = t269 * t271 * t49 * t86
      t284 = t1 * t140
      t286 = t206 * t284 * t143
      t287 = t203 * t284
      t289 = t147 * t205 * t149
      t290 = x1 * x2
      t293 = 0.1D1 / (x2 - t290 + z - t208 + t290 * z)
      t295 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t287, t286
     #, t139, 0.0D0, t289)
      t299 = t6 * t293 * t295 * t108 * t86 / 0.8D1
      t300 = FJET(XB1, XB2, s, 0.0D0, t286, t139, -t287, t289, -t299)
      t305 = t293 * t295 * t108 * t86
      t308 = FJET(XB1, XB2, s, 0.0D0, -t207, 0.0D0, t204, 0.0D0, t239)
      t310 = FJET(XB1, XB2, s, 0.0D0, -t145, t139, 0.0D0, -t150, t200)
      t312 = FJET(XB1, XB2, s, 0.0D0, -t259, 0.0D0, t254, 0.0D0, -t275)
      t317 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t326 = t155 + (-0.90D2 * t113 * t5 * t164 - t170) * t108 / 0.1440D
     #4 - t199
      t327 = FJET(XB1, XB2, s, t139, 0.0D0, 0.0D0, -t145, -t150, t326)
      t329 = FJET(XB1, XB2, s, t139, -t287, 0.0D0, t286, t289, -t299)
      t340 = -t216 - (-0.90D2 * t6 * t225 - t231) * t86 / 0.1440D4 - t23
     #8
      t341 = FJET(XB1, XB2, s, t204, 0.0D0, -t207, 0.0D0, 0.0D0, t340)
      t343 = FJET(XB1, XB2, s, t254, 0.0D0, -t259, 0.0D0, 0.0D0, -t275)
      t348 = FJET(XB1, XB2, s, t286, 0.0D0, -t287, t139, t289, -t299)
      t353 = FJET(XB1, XB2, s, -t207, 0.0D0, t204, 0.0D0, 0.0D0, t340)
      t355 = FJET(XB1, XB2, s, -t145, 0.0D0, 0.0D0, t139, -t150, t326)
      t357 = FJET(XB1, XB2, s, -t287, t139, t286, 0.0D0, t289, -t299)
      t362 = FJET(XB1, XB2, s, -t259, 0.0D0, t254, 0.0D0, 0.0D0, -t275)
      rrgg2gghhardt6s1em1 = t133 * t132 + t135 * t132 + t137 * t132 + t2
     #01 * t200 + t240 * t239 - t276 * pi * t5 * t281 / 0.16D2 - t300 * 
     #pi * t5 * t305 / 0.8D1 + t308 * t239 + t310 * t200 - t312 * pi * t
     #5 * t281 / 0.16D2 + t317 * t132 + t327 * t326 - t329 * pi * t5 * t
     #305 / 0.8D1 + t341 * t340 - t343 * pi * t5 * t281 / 0.16D2 - t348 
     #* pi * t5 * t305 / 0.8D1 + t353 * t340 + t355 * t326 - t357 * pi *
     # t5 * t305 / 0.8D1 - t362 * pi * t5 * t281 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt6s1em2
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t8 = 0.1D1 / z
      t9 = x4 * pi
      t10 = cos(t9)
      t14 = Sqrt(-x3 * (-0.1D1 + x3))
      t25 = t8 * t7
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t37 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t39 = z ** 2
      t41 = Sin(t9)
      t42 = t41 ** 2
      t45 = log(0.4D1 / t39 * t42)
      t54 = -t6 * t7 * (t8 + 0.1D1 / (-x3 - z + 0.2D1 * z * t10 * t14)) 
     #/ x3 / 0.32D2 - t6 * t25 * t26 / 0.16D2 - t6 * t25 * t30 / 0.16D2 
     #- (-0.180D3 * t25 * pi * lh - 0.90D2 * (-t8 * t37 + t45 * t8 * t7)
     # * pi) * t5 / 0.2880D4
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t54)
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t54)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t54)
      t61 = t2 * x1
      t62 = -0.1D1 + x1
      t65 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t67 = t2 * t62 * t65
      t68 = t1 ** 2
      t72 = s * t68 * x1 * t62 * t65
      t73 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t67,
     # t61, 0.0D0, -t72)
      t75 = t8 * t73 * t30
      t77 = t6 * t75 / 0.16D2
      t78 = FJET(XB1, XB2, s, 0.0D0, t61, -t67, 0.0D0, -t72, t77)
      t84 = x2 * s * t1
      t87 = (-0.1D1 + x2) * s * t1
      t91 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t84, -t87, 0
     #.0D0, 0.0D0, 0.0D0)
      t93 = 0.1D1 / (x2 * z - x2 - z) * t91 * t26
      t95 = t6 * t93 / 0.16D2
      t96 = FJET(XB1, XB2, s, 0.0D0, t84, 0.0D0, -t87, 0.0D0, -t95)
      t101 = FJET(XB1, XB2, s, 0.0D0, -t87, 0.0D0, t84, 0.0D0, -t95)
      t106 = FJET(XB1, XB2, s, 0.0D0, -t67, t61, 0.0D0, -t72, t77)
      t111 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t54)
      t113 = FJET(XB1, XB2, s, t61, 0.0D0, 0.0D0, -t67, -t72, t77)
      t118 = FJET(XB1, XB2, s, t84, 0.0D0, -t87, 0.0D0, 0.0D0, -t95)
      t123 = FJET(XB1, XB2, s, -t87, 0.0D0, t84, 0.0D0, 0.0D0, -t95)
      t128 = FJET(XB1, XB2, s, -t67, 0.0D0, 0.0D0, t61, -t72, t77)
      rrgg2gghhardt6s1em2 = t55 * t54 + t57 * t54 + t59 * t54 + t78 * pi
     # * t5 * t75 / 0.16D2 - t96 * pi * t5 * t93 / 0.16D2 - t101 * pi * 
     #t5 * t93 / 0.16D2 + t106 * pi * t5 * t75 / 0.16D2 + t111 * t54 + t
     #113 * pi * t5 * t75 / 0.16D2 - t118 * pi * t5 * t93 / 0.16D2 - t12
     #3 * pi * t5 * t93 / 0.16D2 + t128 * pi * t5 * t75 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt6s1em3
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t7 = 0.1D1 / z
      t8 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t11 = pi * t5 * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t5 * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2gghhardt6s1em3 = -t12 * pi * t15 / 0.32D2 - t17 * pi * t15 / 
     #0.32D2 - t20 * pi * t15 / 0.32D2 - t23 * pi * t15 / 0.32D2

      end function



      doubleprecision function rrgg2gghhardt6s1em4
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt6s2e1
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
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
      t23 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t25 = t22 ** 2
      t26 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t30 = cos(t10)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t17)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t39 = 0.1D1 / z
      t43 = log(0.4D1 * t9 * t16)
      t44 = t43 * t39
      t46 = t43 ** 2
      t53 = pi * lh
      t63 = lh ** 2
      t65 = pi ** 2
      t67 = -0.180D3 * t63 + 0.30D2 * t65
      t68 = pi * t67
      t75 = 0.1D1 / x3
      t77 = 0.1D1 / x1
      t80 = pi * t39
      t81 = t67 * t5
      t82 = t8 * t12
      t83 = t82 * t15
      t85 = log(0.4D1 * t83)
      t90 = t85 ** 2
      t93 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t107 = -0.60D2 * lh * t65 + 0.240D3 * zeta3 + 0.120D3 * t63 * lh
      t108 = t107 * t5
      t111 = lh * t5
      t122 = x2 ** 2
      t123 = x3 * t122
      t124 = t123 * t8
      t125 = -0.1D1 + x2
      t126 = t16 * t125
      t129 = log(-0.4D1 * t124 * t126)
      t134 = log(-0.4D1 * t124 * t19)
      t140 = log(0.4D1 * t123 * t83)
      t146 = t5 * t26
      t147 = t146 * t37
      t152 = 0.1D1 / x2
      t153 = t77 * t152
      t156 = t122 * t8
      t159 = log(-0.4D1 * t156 * t126)
      t160 = t159 * t39
      t162 = t159 ** 2
      t168 = log(0.4D1 * t156 * t16)
      t169 = t168 * t39
      t171 = t168 ** 2
      t190 = t5 * t23
      t195 = x3 * t15
      t199 = log(-0.4D1 * t195 * t12 * t18)
      t203 = log(0.4D1 * t195 * t12)
      t207 = t203 ** 2
      t210 = t199 ** 2
      t223 = t5 * t7
      t244 = log(-0.4D1 * t123 * t19)
      t246 = t244 ** 2
      t253 = log(-0.4D1 * t123 * t126)
      t254 = t253 * t39
      t256 = t253 ** 2
      t262 = log(0.4D1 * t123 * t16)
      t263 = t262 * t39
      t265 = t262 ** 2
      t292 = t15 * t122
      t295 = log(0.4D1 * t292 * t12)
      t296 = t295 ** 2
      t297 = t12 * t125
      t300 = log(-0.4D1 * t292 * t297)
      t301 = t300 ** 2
      t325 = rrgg2gghhard61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t330 = log(0.4D1 * t16)
      t331 = t330 ** 2
      t332 = t331 * pi
      t333 = t39 * lh
      t338 = t331 * t330 * pi
      t341 = t330 * pi
      t342 = t39 * t67
      t373 = t65 ** 2
      t374 = t63 ** 2
      t380 = t331 ** 2
      t388 = (-0.90D2 * t6 * ((t7 - t22 * t23 + t25 * t26 / 0.2D1) * t37
     # + t39 * t7 - t44 * t23 + t46 * t39 * t26 / 0.2D1) + 0.180D3 * t53
     # * t5 * ((t23 - t22 * t26) * t37 + t39 * t23 - t44 * t26) + t68 * 
     #t5 * (t39 * t26 + t26 * t37)) * t75 * t77 / 0.1440D4 + (t80 * t81 
     #* (t23 - t85 * t26) - 0.90D2 * t80 * t5 * (t90 * t23 / 0.2D1 + t93
     # - t90 * t85 * t26 / 0.6D1 - t85 * t7) + t80 * t108 * t26 + 0.180D
     #3 * t80 * t111 * (t7 - t85 * t23 + t90 * t26 / 0.2D1)) * t77 / 0.1
     #440D4 + (-0.90D2 * t6 * (t129 * t39 * t26 + (t23 - t134 * t26) * t
     #37 - t140 * t39 * t26) + 0.180D3 * t53 * t147) * t75 * t153 / 0.72
     #0D3 + (-0.90D2 * t6 * (t160 * t23 - t162 * t39 * t26 / 0.2D1 - t16
     #9 * t23 + t171 * t39 * t26 / 0.2D1) + 0.180D3 * t53 * t5 * (t160 *
     # t26 - t169 * t26)) * t77 * t152 / 0.720D3 + ((-0.90D2 * t6 * t7 +
     # 0.180D3 * t53 * t190 + t68 * t146) * (-t199 * t37 - t203 * t39) -
     # 0.90D2 * t6 * t26 * (-t207 * t203 * t39 / 0.6D1 - t210 * t199 * t
     #37 / 0.6D1) + (t68 * t190 - 0.90D2 * t6 * t93 + pi * t107 * t146 +
     # 0.180D3 * t53 * t223) * (t37 + t39) + (-0.90D2 * t6 * t23 + 0.180
     #D3 * t53 * t146) * (t210 * t37 / 0.2D1 + t207 * t39 / 0.2D1)) * t7
     #5 / 0.2880D4 + (-0.90D2 * t6 * ((t7 - t244 * t23 + t246 * t26 / 0.
     #2D1) * t37 + t254 * t23 - t256 * t39 * t26 / 0.2D1 - t263 * t23 + 
     #t265 * t39 * t26 / 0.2D1) + 0.180D3 * t53 * t5 * ((t23 - t244 * t2
     #6) * t37 + t254 * t26 - t263 * t26) + t68 * t147) * t75 * t152 / 0
     #.1440D4 + ((-0.90D2 * t80 * t190 + 0.180D3 * t80 * t111 * t26) * (
     #t296 / 0.2D1 - t301 / 0.2D1) - 0.90D2 * t80 * t146 * (t301 * t300 
     #/ 0.6D1 - t296 * t295 / 0.6D1) + (-0.90D2 * t80 * t223 + 0.180D3 *
     # t80 * t111 * t23 + t80 * t81 * t26) * (-t295 + t300)) * t152 / 0.
     #1440D4 - t80 * t5 * t325 / 0.32D2 + (0.90D2 * t332 * t333 + t80 * 
     #t107 + 0.15D2 * t338 * t39 - t341 * t342) * t5 * t23 / 0.2880D4 + 
     #(-0.180D3 * t341 * t333 - 0.45D2 * t332 * t39 + t80 * t67) * t5 * 
     #t7 / 0.2880D4 + (0.180D3 * t80 * lh + 0.90D2 * t341 * t39) * t5 * 
     #t93 / 0.2880D4 + (-0.30D2 * t338 * t333 + t332 * t342 / 0.2D1 - t3
     #41 * t39 * t107 + t80 * (-0.480D3 * lh * zeta3 - t373 - 0.60D2 * t
     #374 + 0.60D2 * t63 * t65) - 0.15D2 / 0.4D1 * t380 * pi * t39) * t5
     # * t26 / 0.2880D4
      t389 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t388)
      t391 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t388)
      t393 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t388)
      t396 = x1 * z
      t397 = -z - x1 + t396
      t398 = 0.1D1 / t397
      t400 = t2 * x1 * t125 * t398
      t401 = -0.1D1 + x1
      t402 = t2 * t401
      t405 = x2 * s * t1 * x1
      t406 = t1 ** 2
      t407 = s * t406
      t410 = x1 * t401 * t398
      t411 = t407 * t125 * t410
      t412 = x1 * x2
      t413 = t412 * z
      t415 = 0.1D1 / (-z - t412 + t413)
      t416 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t405, t400,
     # -t402, 0.0D0, -t411)
      t417 = t415 * t416
      t418 = t123 * t82
      t419 = 0.1D1 / t13
      t420 = t419 * t398
      t421 = t401 ** 2
      t423 = t420 * t421 * t125
      t426 = log(0.4D1 * t418 * t423)
      t428 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t405, t400,
     # -t402, 0.0D0, -t411)
      t434 = t5 * t415 * t428
      t440 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, t405, t400,
     # -t402, 0.0D0, -t411)
      t442 = t156 * t12
      t445 = log(0.4D1 * t442 * t423)
      t446 = t445 * t415
      t448 = t445 ** 2
      t465 = (-0.90D2 * t6 * (-t417 + t426 * t415 * t428) - 0.180D3 * t5
     #3 * t434) * t75 * t153 / 0.720D3 + (-0.90D2 * t6 * (-t415 * t440 +
     # t446 * t416 - t448 * t415 * t428 / 0.2D1) + 0.180D3 * t53 * t5 * 
     #(-t417 + t446 * t428) - t68 * t434) * t77 * t152 / 0.720D3
      t466 = FJET(XB1, XB2, s, 0.0D0, t400, -t402, t405, -t411, t465)
      t468 = x2 * x3
      t469 = 0.1D1 - x3 + t468
      t470 = 0.1D1 / t469
      t471 = t468 * t470
      t472 = t2 * t471
      t474 = t2 * t17 * t470
      t475 = t125 * t17
      t477 = Sqrt(t31 * t475)
      t481 = 0.1D1 / (-z - x3 + t468 + 0.2D1 * t30 * t477)
      t482 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t474, t472, 0.0D0)
      t485 = t469 ** 2
      t486 = 0.1D1 / t485
      t487 = t17 * t486
      t491 = log(0.4D1 * t123 * t15 * t297 * t487)
      t492 = t491 * t481
      t493 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t474, t472, 0.0D0)
      t495 = t491 ** 2
      t497 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t474, t472, 0.0D0)
      t500 = -t481 * t482 + t492 * t493 - t495 * t481 * t497 / 0.2D1
      t503 = t481 * t493
      t505 = -t503 + t492 * t497
      t510 = t5 * t481 * t497
      t511 = t68 * t510
      t520 = log(0.4D1 * t418 * t15 * t125 * t487)
      t531 = (-0.90D2 * t6 * (-t503 + t520 * t481 * t497) - 0.180D3 * t5
     #3 * t510) * t75 * t153 / 0.720D3
      t532 = (-0.90D2 * t6 * t500 + 0.180D3 * t53 * t5 * t505 - t511) * 
     #t75 * t152 / 0.1440D4 + t531
      t533 = FJET(XB1, XB2, s, 0.0D0, t472, 0.0D0, -t474, 0.0D0, t532)
      t536 = t2 * x1 * t398
      t537 = t407 * t410
      t538 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t53
     #6, -t402, 0.0D0, t537)
      t539 = t39 * t538
      t540 = t9 * t12
      t541 = t420 * t421
      t544 = log(-0.4D1 * t540 * t541)
      t545 = t544 * t39
      t546 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t53
     #6, -t402, 0.0D0, t537)
      t548 = t544 ** 2
      t550 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t53
     #6, -t402, 0.0D0, t537)
      t555 = t420 * t421 * t18
      t558 = log(0.4D1 * t540 * t555)
      t559 = t558 * t397
      t561 = t558 ** 2
      t566 = x3 * x1
      t567 = t566 * z
      t569 = 0.2D1 * t9 * z
      t570 = x1 * t13
      t571 = t566 * t13
      t572 = t8 * t13
      t573 = t572 * x3
      t574 = z * t30
      t575 = x3 * t397
      t577 = Sqrt(t575 * t17)
      t581 = 0.1D1 / (-t396 - t567 + t569 + t570 + t571 - t573 - t31 - t
     #9 - t13 + 0.2D1 * t574 * t577)
      t586 = t39 * t546
      t588 = t397 * t546
      t600 = t5 * (-t39 * t550 + t397 * t550 * t581)
      t605 = (-0.90D2 * t6 * (-t539 + t545 * t546 - t548 * t39 * t550 / 
     #0.2D1 + (t397 * t538 - t559 * t546 + t561 * t397 * t550 / 0.2D1) *
     # t581) + 0.180D3 * t53 * t5 * (-t586 + t545 * t550 + (t588 - t559 
     #* t550) * t581) + t68 * t600) * t75 * t77 / 0.1440D4
      t608 = log(-0.4D1 * t82 * t541)
      t610 = -t546 + t608 * t550
      t613 = t608 ** 2
      t616 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t53
     #6, -t402, 0.0D0, t537)
      t621 = -t613 * t546 / 0.2D1 - t616 + t613 * t608 * t550 / 0.6D1 + 
     #t608 * t538
      t626 = t80 * t108 * t550
      t630 = -t538 + t608 * t546 - t613 * t550 / 0.2D1
      t639 = log(0.4D1 * t418 * t555)
      t645 = t398 * t421
      t649 = log(-0.4D1 * t124 * t12 * t419 * t645)
      t660 = (-0.90D2 * t6 * ((t588 - t639 * t397 * t550) * t581 - t586 
     #+ t649 * t39 * t550) + 0.180D3 * t53 * t600) * t75 * t153 / 0.720D
     #3
      t663 = log(-0.4D1 * t442 * t541)
      t664 = t663 * t39
      t666 = t663 ** 2
      t670 = -t539 + t664 * t546 - t666 * t39 * t550 / 0.2D1
      t674 = -t586 + t664 * t550
      t680 = t68 * t5 * t39 * t550
      t684 = (-0.90D2 * t6 * t670 + 0.180D3 * t53 * t5 * t674 - t680) * 
     #t77 * t152 / 0.720D3
      t685 = t605 + (t80 * t81 * t610 - 0.90D2 * t80 * t5 * t621 - t626 
     #+ 0.180D3 * t80 * t111 * t630) * t77 / 0.1440D4 + t660 + t684
      t686 = FJET(XB1, XB2, s, 0.0D0, -t402, -t536, 0.0D0, t537, t685)
      t688 = FJET(XB1, XB2, s, 0.0D0, -t536, -t402, 0.0D0, t537, t685)
      t690 = FJET(XB1, XB2, s, 0.0D0, -t474, 0.0D0, t472, 0.0D0, t532)
      t692 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t388)
      t694 = FJET(XB1, XB2, s, t405, -t402, t400, 0.0D0, -t411, t465)
      t696 = FJET(XB1, XB2, s, t400, 0.0D0, t405, -t402, -t411, t465)
      t709 = (-0.90D2 * t6 * t500 + 0.180D3 * t53 * t5 * t505 - t511) * 
     #t75 * t152 / 0.1440D4 + t531
      t710 = FJET(XB1, XB2, s, t472, 0.0D0, -t474, 0.0D0, 0.0D0, t709)
      t715 = t17 * s * t1 * t401 * t470
      t716 = t2 * x1
      t718 = Sqrt(-t575 * t475)
      t719 = t30 * t718
      t725 = t716 * x2 * (-x3 + t468 - z + t31 - x1 + t566 + t396 - t567
     # + 0.2D1 * t719) * t398 * t470
      t726 = t402 * t471
      t727 = t396 * t123
      t728 = t719 * x2
      t733 = x1 * t122 * x3
      t737 = t716 * (0.1D1 - x3 - x2 + t468 - t727 + 0.2D1 * t728 + z * 
     #t122 * x3 + t733) * t398 * t470
      t740 = x2 * z
      t752 = 0.2D1 * t396 * t728 + t396 + t727 - 0.2D1 * t566 * t740 + t
     #566 * x2 * t13 + 0.2D1 * t9 * t740 - t572 * t468 - 0.2D1 * x1 * t3
     #0 * t718 * x2 + t413 + t567 - t569 - t571 + t573
      t753 = x2 * t8
      t763 = -t570 + t31 + t9 + t753 + t13 - t733 - 0.2D1 * t574 * t718 
     #- t468 * z + t566 * x2 - t9 * x2 - t412 * t13 - 0.2D1 * t753 * z +
     # t753 * t13
      t765 = 0.1D1 / (t752 + t763)
      t766 = t397 * t765
      t767 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t725, -t737
     #, t715, -t726, -t411)
      t775 = log(-0.4D1 * t123 * t82 * t419 * t645 * t475 * t486)
      t777 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t725, -t737
     #, t715, -t726, -t411)
      t787 = -0.90D2 * t6 * (t766 * t767 - t775 * t397 * t765 * t777) + 
     #0.180D3 * t53 * t5 * t766 * t777
      t790 = t787 * t75 * t153 / 0.720D3
      t791 = FJET(XB1, XB2, s, t715, t725, -t726, -t737, -t411, t790)
      t794 = t75 * t77 * t152
      t797 = FJET(XB1, XB2, s, t725, t715, -t737, -t726, -t411, t790)
      t814 = (t80 * t81 * t610 - 0.90D2 * t80 * t5 * t621 - t626 + 0.180
     #D3 * t80 * t111 * t630) * t77 / 0.1440D4
      t815 = t605 + t814 + t660 + t684
      t816 = FJET(XB1, XB2, s, -t402, 0.0D0, 0.0D0, -t536, t537, t815)
      t818 = FJET(XB1, XB2, s, -t402, t405, 0.0D0, t400, -t411, t465)
      t831 = t605 + t814 + t660 + (-0.90D2 * t6 * t670 + 0.180D3 * t53 *
     # t5 * t674 - t680) * t77 * t152 / 0.720D3
      t832 = FJET(XB1, XB2, s, -t536, 0.0D0, 0.0D0, -t402, t537, t831)
      t834 = FJET(XB1, XB2, s, -t474, 0.0D0, t472, 0.0D0, 0.0D0, t709)
      t836 = FJET(XB1, XB2, s, -t737, -t726, t725, t715, -t411, t790)
      t840 = FJET(XB1, XB2, s, -t726, -t737, t715, t725, -t411, t790)
      rrgg2gghhardt6s2e1 = t389 * t388 + t391 * t388 + t393 * t388 + t46
     #6 * t465 + t533 * t532 + t686 * t685 + t688 * t685 + t690 * t532 +
     # t692 * t388 + t694 * t465 + t696 * t465 + t710 * t709 + t791 * t7
     #87 * t794 / 0.720D3 + t797 * t787 * t794 / 0.720D3 + t816 * t815 +
     # t818 * t465 + t832 * t831 + t834 * t709 + t836 * t787 * t794 / 0.
     #720D3 + t840 * t787 * t794 / 0.720D3

      end function



      doubleprecision function rrgg2gghhardt6s2e0
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
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
      t23 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t26 = cos(t10)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t17)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t35 = 0.1D1 / z
      t39 = log(0.4D1 * t9 * t16)
      t45 = pi * lh
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t60 = 0.1D1 / x2
      t61 = t55 * t60
      t65 = x2 ** 2
      t66 = t65 * t8
      t67 = -0.1D1 + x2
      t68 = t16 * t67
      t71 = log(-0.4D1 * t66 * t68)
      t76 = log(0.4D1 * t66 * t16)
      t84 = pi * t35
      t85 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t86 = t8 * t12
      t89 = log(0.4D1 * t86 * t15)
      t91 = t89 ** 2
      t98 = lh * t5
      t104 = lh ** 2
      t106 = pi ** 2
      t108 = -0.180D3 * t104 + 0.30D2 * t106
      t115 = x3 * t15
      t119 = log(-0.4D1 * t115 * t12 * t18)
      t120 = t119 ** 2
      t124 = log(0.4D1 * t115 * t12)
      t125 = t124 ** 2
      t134 = t5 * t23
      t144 = t5 * t7
      t147 = pi * t108
      t156 = log(0.4D1 * t16)
      t157 = t156 * pi
      t158 = t35 * lh
      t161 = t156 ** 2
      t162 = t161 * pi
      t170 = rrgg2gghhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t201 = x3 * t65
      t204 = log(-0.4D1 * t201 * t19)
      t210 = log(-0.4D1 * t201 * t68)
      t215 = log(0.4D1 * t201 * t16)
      t228 = t15 * t65
      t231 = log(0.4D1 * t228 * t12)
      t232 = t231 ** 2
      t233 = t12 * t67
      t236 = log(-0.4D1 * t228 * t233)
      t237 = t236 ** 2
      t254 = (-0.90D2 * t6 * ((t7 - t22 * t23) * t33 + t35 * t7 - t39 * 
     #t35 * t23) + 0.180D3 * t45 * t5 * (t35 * t23 + t23 * t33)) * t53 *
     # t55 / 0.1440D4 - t6 * t23 * t33 * t53 * t61 / 0.8D1 - t6 * (t71 *
     # t35 * t23 - t76 * t35 * t23) * t55 * t60 / 0.8D1 + (-0.90D2 * t84
     # * t5 * (t85 - t89 * t7 + t91 * t23 / 0.2D1) + 0.180D3 * t84 * t98
     # * (t7 - t89 * t23) + t84 * t108 * t5 * t23) * t55 / 0.1440D4 + (-
     #0.90D2 * t6 * t23 * (t120 * t33 / 0.2D1 + t125 * t35 / 0.2D1) + (-
     #0.90D2 * t6 * t7 + 0.180D3 * t45 * t134) * (-t119 * t33 - t124 * t
     #35) + (-0.90D2 * t6 * t85 + 0.180D3 * t45 * t144 + t147 * t134) * 
     #(t33 + t35)) * t53 / 0.2880D4 + (-0.180D3 * t157 * t158 - 0.45D2 *
     # t162 * t35 + t84 * t108) * t5 * t7 / 0.2880D4 - t84 * t5 * t170 /
     # 0.32D2 + (0.90D2 * t162 * t158 + t84 * (-0.60D2 * lh * t106 + 0.2
     #40D3 * zeta3 + 0.120D3 * t104 * lh) + 0.15D2 * t161 * t156 * pi * 
     #t35 - t157 * t35 * t108) * t5 * t23 / 0.2880D4 + (0.180D3 * t84 * 
     #lh + 0.90D2 * t157 * t35) * t5 * t85 / 0.2880D4 + (-0.90D2 * t6 * 
     #((t7 - t204 * t23) * t33 + t210 * t35 * t23 - t215 * t35 * t23) + 
     #0.180D3 * t45 * t134 * t33) * t53 * t60 / 0.1440D4 + (-0.90D2 * t8
     #4 * t134 * (t232 / 0.2D1 - t237 / 0.2D1) + (-0.90D2 * t84 * t144 +
     # 0.180D3 * t84 * t98 * t23) * (-t231 + t236)) * t60 / 0.1440D4
      t255 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t254)
      t257 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t254)
      t259 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t254)
      t262 = x1 * z
      t263 = -z - x1 + t262
      t264 = 0.1D1 / t263
      t266 = t2 * x1 * t67 * t264
      t267 = -0.1D1 + x1
      t268 = t2 * t267
      t271 = x2 * s * t1 * x1
      t272 = t1 ** 2
      t273 = s * t272
      t276 = x1 * t267 * t264
      t277 = t273 * t67 * t276
      t278 = x1 * x2
      t279 = t278 * z
      t281 = 0.1D1 / (-z - t278 + t279)
      t283 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t271, t266,
     # -t268, 0.0D0, -t277)
      t288 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t271, t266,
     # -t268, 0.0D0, -t277)
      t290 = t66 * t12
      t292 = 0.1D1 / t13 * t264
      t293 = t267 ** 2
      t298 = log(0.4D1 * t290 * t292 * t293 * t67)
      t312 = t6 * t281 * t283 * t53 * t61 / 0.8D1 + (-0.90D2 * t6 * (-t2
     #81 * t288 + t298 * t281 * t283) - 0.180D3 * t45 * t5 * t281 * t283
     #) * t55 * t60 / 0.720D3
      t313 = FJET(XB1, XB2, s, 0.0D0, t266, -t268, t271, -t277, t312)
      t315 = x2 * x3
      t316 = 0.1D1 - x3 + t315
      t317 = 0.1D1 / t316
      t318 = t315 * t317
      t319 = t2 * t318
      t321 = t2 * t17 * t317
      t322 = t67 * t17
      t324 = Sqrt(t27 * t322)
      t328 = 0.1D1 / (-z - x3 + t315 + 0.2D1 * t26 * t324)
      t330 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t321, t319, 0.0D0)
      t334 = t6 * t328 * t330 * t53 * t61 / 0.8D1
      t335 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t321, t319, 0.0D0)
      t338 = t316 ** 2
      t344 = log(0.4D1 * t201 * t15 * t233 * t17 / t338)
      t347 = -t328 * t335 + t344 * t328 * t330
      t353 = 0.180D3 * t45 * t5 * t328 * t330
      t358 = t334 + (-0.90D2 * t6 * t347 - t353) * t53 * t60 / 0.1440D4
      t359 = FJET(XB1, XB2, s, 0.0D0, t319, 0.0D0, -t321, 0.0D0, t358)
      t362 = t2 * x1 * t264
      t363 = t273 * t276
      t364 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #2, -t268, 0.0D0, t363)
      t365 = t35 * t364
      t366 = t9 * t12
      t367 = t292 * t293
      t370 = log(-0.4D1 * t366 * t367)
      t372 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #2, -t268, 0.0D0, t363)
      t379 = log(0.4D1 * t366 * t292 * t293 * t18)
      t383 = x3 * x1
      t384 = t383 * z
      t386 = 0.2D1 * t9 * z
      t387 = x1 * t13
      t388 = t383 * t13
      t389 = t8 * t13
      t390 = t389 * x3
      t391 = z * t26
      t392 = x3 * t263
      t394 = Sqrt(t392 * t17)
      t398 = 0.1D1 / (-t262 - t384 + t386 + t387 + t388 - t390 - t27 - t
     #9 - t13 + 0.2D1 * t391 * t394)
      t406 = -t35 * t372 + t263 * t372 * t398
      t413 = (-0.90D2 * t6 * (-t365 + t370 * t35 * t372 + (t263 * t364 -
     # t379 * t263 * t372) * t398) + 0.180D3 * t45 * t5 * t406) * t53 * 
     #t55 / 0.1440D4
      t416 = t53 * t55 * t60
      t418 = t6 * t406 * t416 / 0.8D1
      t421 = log(-0.4D1 * t290 * t367)
      t424 = -t365 + t421 * t35 * t372
      t428 = t5 * t35 * t372
      t430 = 0.180D3 * t45 * t428
      t434 = (-0.90D2 * t6 * t424 - t430) * t55 * t60 / 0.720D3
      t435 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t36
     #2, -t268, 0.0D0, t363)
      t438 = log(-0.4D1 * t86 * t367)
      t440 = t438 ** 2
      t443 = -t435 + t438 * t364 - t440 * t372 / 0.2D1
      t448 = -t364 + t438 * t372
      t452 = t147 * t428
      t456 = t413 - t418 + t434 + (-0.90D2 * t84 * t5 * t443 + 0.180D3 *
     # t84 * t98 * t448 - t452) * t55 / 0.1440D4
      t457 = FJET(XB1, XB2, s, 0.0D0, -t268, -t362, 0.0D0, t363, t456)
      t459 = FJET(XB1, XB2, s, 0.0D0, -t362, -t268, 0.0D0, t363, t456)
      t461 = FJET(XB1, XB2, s, 0.0D0, -t321, 0.0D0, t319, 0.0D0, t358)
      t463 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t254)
      t465 = FJET(XB1, XB2, s, t271, -t268, t266, 0.0D0, -t277, t312)
      t467 = FJET(XB1, XB2, s, t266, 0.0D0, t271, -t268, -t277, t312)
      t476 = t334 + (-0.90D2 * t6 * t347 - t353) * t53 * t60 / 0.1440D4
      t477 = FJET(XB1, XB2, s, t319, 0.0D0, -t321, 0.0D0, 0.0D0, t476)
      t482 = t17 * s * t1 * t267 * t317
      t483 = t2 * x1
      t485 = Sqrt(-t392 * t322)
      t486 = t26 * t485
      t492 = t483 * x2 * (-x3 + t315 - z + t27 - x1 + t383 + t262 - t384
     # + 0.2D1 * t486) * t264 * t317
      t493 = t268 * t318
      t494 = t262 * t201
      t495 = t486 * x2
      t500 = x1 * t65 * x3
      t504 = t483 * (0.1D1 - x3 - x2 + t315 - t494 + 0.2D1 * t495 + z * 
     #t65 * x3 + t500) * t264 * t317
      t511 = x2 * t8
      t517 = t262 - 0.2D1 * t391 * t485 - t315 * z + t383 * x2 - t9 * x2
     # - t278 * t13 - 0.2D1 * t511 * z + t511 * t13 - t500 + t13 + 0.2D1
     # * t262 * t495 - t387 + t27
      t518 = x2 * z
      t530 = t9 + t494 - 0.2D1 * t383 * t518 + t383 * x2 * t13 + 0.2D1 *
     # t9 * t518 - t389 * t315 - 0.2D1 * x1 * t26 * t485 * x2 + t511 + t
     #279 + t384 - t386 - t388 + t390
      t532 = 0.1D1 / (t517 + t530)
      t535 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t492, -t504
     #, t482, -t493, -t277)
      t539 = t6 * t263 * t532 * t535 * t53 * t61 / 0.8D1
      t540 = FJET(XB1, XB2, s, t482, t492, -t493, -t504, -t277, -t539)
      t542 = t5 * t263
      t545 = t532 * t535 * t416
      t548 = FJET(XB1, XB2, s, t492, t482, -t504, -t493, -t277, -t539)
      t563 = (-0.90D2 * t84 * t5 * t443 + 0.180D3 * t84 * t98 * t448 - t
     #452) * t55 / 0.1440D4
      t564 = t413 - t418 + t434 + t563
      t565 = FJET(XB1, XB2, s, -t268, 0.0D0, 0.0D0, -t362, t363, t564)
      t567 = FJET(XB1, XB2, s, -t268, t271, 0.0D0, t266, -t277, t312)
      t576 = t413 - t418 + (-0.90D2 * t6 * t424 - t430) * t55 * t60 / 0.
     #720D3 + t563
      t577 = FJET(XB1, XB2, s, -t362, 0.0D0, 0.0D0, -t268, t363, t576)
      t579 = FJET(XB1, XB2, s, -t321, 0.0D0, t319, 0.0D0, 0.0D0, t476)
      t581 = FJET(XB1, XB2, s, -t504, -t493, t492, t482, -t277, -t539)
      t586 = FJET(XB1, XB2, s, -t493, -t504, t482, t492, -t277, -t539)
      rrgg2gghhardt6s2e0 = t255 * t254 + t257 * t254 + t259 * t254 + t31
     #3 * t312 + t359 * t358 + t457 * t456 + t459 * t456 + t461 * t358 +
     # t463 * t254 + t465 * t312 + t467 * t312 + t477 * t476 - t540 * pi
     # * t542 * t545 / 0.8D1 - t548 * pi * t542 * t545 / 0.8D1 + t565 * 
     #t564 + t567 * t312 + t577 * t576 + t579 * t476 - t581 * pi * t542 
     #* t545 / 0.8D1 - t586 * pi * t542 * t545 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt6s2em1
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t6 = pi * t5
      t7 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t20 = log(-0.4D1 * t11 * t14 / t15)
      t21 = cos(t12)
      t22 = x3 * z
      t24 = Sqrt(-t22 * t15)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t24)
      t32 = log(0.4D1 * t11 * t14)
      t33 = 0.1D1 / z
      t39 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t42 = pi * lh
      t50 = 0.1D1 / x3
      t53 = pi * t33
      t54 = x1 ** 2
      t55 = t54 * t14
      t58 = log(0.4D1 * t55 * t10)
      t69 = 0.1D1 / x1
      t81 = 0.1D1 / x2
      t85 = t53 * t5
      t86 = x2 ** 2
      t87 = t10 * t86
      t90 = log(0.4D1 * t87 * t14)
      t91 = -0.1D1 + x2
      t95 = log(-0.4D1 * t87 * t14 * t91)
      t101 = rrgg2gghhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t109 = log(0.4D1 * t10 * t14)
      t110 = t109 * pi
      t120 = t109 ** 2
      t124 = lh ** 2
      t126 = pi ** 2
      t134 = (-0.90D2 * t6 * t7 * (-t20 * t28 - t32 * t33) + (-0.90D2 * 
     #t6 * t39 + 0.180D3 * t42 * t5 * t7) * (t28 + t33)) * t50 / 0.2880D
     #4 + (-0.90D2 * t53 * t5 * (t39 - t58 * t7) + 0.180D3 * t53 * lh * 
     #t5 * t7) * t69 / 0.1440D4 - t6 * (t33 * t7 + t7 * t28) * t50 * t69
     # / 0.16D2 - t6 * t7 * t28 * t50 * t81 / 0.16D2 - t85 * t7 * (-t90 
     #+ t95) * t81 / 0.16D2 - t53 * t5 * t101 / 0.32D2 + (0.180D3 * t53 
     #* lh + 0.90D2 * t110 * t33) * t5 * t39 / 0.2880D4 + (-0.180D3 * t1
     #10 * t33 * lh - 0.45D2 * t120 * pi * t33 + t53 * (-0.180D3 * t124 
     #+ 0.30D2 * t126)) * t5 * t7 / 0.2880D4
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t134)
      t137 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t134)
      t139 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t134)
      t142 = x1 * z
      t143 = -z - x1 + t142
      t144 = 0.1D1 / t143
      t146 = t2 * x1 * t91 * t144
      t147 = -0.1D1 + x1
      t148 = t2 * t147
      t151 = x2 * s * t1 * x1
      t152 = t1 ** 2
      t153 = s * t152
      t156 = x1 * t147 * t144
      t157 = t153 * t91 * t156
      t158 = x1 * x2
      t161 = 0.1D1 / (-z - t158 + t158 * z)
      t163 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t151, t146,
     # -t148, 0.0D0, -t157)
      t167 = t6 * t161 * t163 * t69 * t81 / 0.8D1
      t168 = FJET(XB1, XB2, s, 0.0D0, t146, -t148, t151, -t157, t167)
      t173 = t161 * t163 * t69 * t81
      t176 = x2 * x3
      t178 = 0.1D1 / (0.1D1 - x3 + t176)
      t180 = t2 * t176 * t178
      t182 = t2 * t15 * t178
      t185 = Sqrt(t22 * t91 * t15)
      t189 = 0.1D1 / (-z - x3 + t176 + 0.2D1 * t21 * t185)
      t191 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t182, t180, 0.0D0)
      t195 = t6 * t189 * t191 * t50 * t81 / 0.16D2
      t196 = FJET(XB1, XB2, s, 0.0D0, t180, 0.0D0, -t182, 0.0D0, t195)
      t201 = t189 * t191 * t50 * t81
      t205 = t2 * x1 * t144
      t206 = t153 * t156
      t207 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t20
     #5, -t148, 0.0D0, t206)
      t211 = t85 * t207 * t69 * t81 / 0.8D1
      t212 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t20
     #5, -t148, 0.0D0, t206)
      t215 = t147 ** 2
      t219 = log(-0.4D1 * t55 / t8 * t144 * t215)
      t221 = -t212 + t219 * t207
      t228 = 0.180D3 * t42 * t5 * t33 * t207
      t234 = x3 * x1
      t236 = x3 * t54
      t246 = Sqrt(x3 * t143 * t15)
      t256 = t6 * (-t33 * t207 + t143 * t207 / (-t142 - t234 * z + 0.2D1
     # * t236 * z + x1 * t8 + t234 * t8 - t54 * t8 * x3 - t22 - t236 - t
     #8 + 0.2D1 * z * t21 * t246)) * t50 * t69 / 0.16D2
      t257 = t211 + (-0.90D2 * t53 * t5 * t221 - t228) * t69 / 0.1440D4 
     #- t256
      t258 = FJET(XB1, XB2, s, 0.0D0, -t148, -t205, 0.0D0, t206, t257)
      t260 = FJET(XB1, XB2, s, 0.0D0, -t205, -t148, 0.0D0, t206, t257)
      t262 = FJET(XB1, XB2, s, 0.0D0, -t182, 0.0D0, t180, 0.0D0, t195)
      t267 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t134)
      t269 = FJET(XB1, XB2, s, t151, -t148, t146, 0.0D0, -t157, t167)
      t274 = FJET(XB1, XB2, s, t146, 0.0D0, t151, -t148, -t157, t167)
      t279 = FJET(XB1, XB2, s, t180, 0.0D0, -t182, 0.0D0, 0.0D0, t195)
      t291 = t211 + (-0.90D2 * t53 * t5 * t221 - t228) * t69 / 0.1440D4 
     #- t256
      t292 = FJET(XB1, XB2, s, -t148, 0.0D0, 0.0D0, -t205, t206, t291)
      t294 = FJET(XB1, XB2, s, -t148, t151, 0.0D0, t146, -t157, t167)
      t299 = FJET(XB1, XB2, s, -t205, 0.0D0, 0.0D0, -t148, t206, t291)
      t301 = FJET(XB1, XB2, s, -t182, 0.0D0, t180, 0.0D0, 0.0D0, t195)
      rrgg2gghhardt6s2em1 = t135 * t134 + t137 * t134 + t139 * t134 + t1
     #68 * pi * t5 * t173 / 0.8D1 + t196 * pi * t5 * t201 / 0.16D2 + t25
     #8 * t257 + t260 * t257 + t262 * pi * t5 * t201 / 0.16D2 + t267 * t
     #134 + t269 * pi * t5 * t173 / 0.8D1 + t274 * pi * t5 * t173 / 0.8D
     #1 + t279 * pi * t5 * t201 / 0.16D2 + t292 * t291 + t294 * pi * t5 
     #* t173 / 0.8D1 + t299 * t291 + t301 * pi * t5 * t201 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt6s2em2
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

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
      t3 = 0.1D1 / z
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t10 = 0.1D1 / x1
      t14 = pi * t7
      t15 = x4 * pi
      t16 = cos(t15)
      t20 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t31 = rrgg2gghhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t37 = z ** 2
      t40 = Sin(t15)
      t41 = t40 ** 2
      t44 = log(0.4D1 / t37 / z * t41)
      t52 = -t4 * t7 * t8 * t10 / 0.16D2 - t14 * t8 * (0.1D1 / (-z - x3 
     #+ 0.2D1 * t16 * t20) + t3) / x3 / 0.32D2 - t4 * t7 * t31 / 0.32D2 
     #+ (0.180D3 * t4 * lh + 0.90D2 * t44 * pi * t3) * t7 * t8 / 0.2880D
     #4
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t52)
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t52)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t52)
      t59 = -0.1D1 + x1
      t60 = t2 * t59
      t63 = 0.1D1 / (-z - x1 + x1 * z)
      t65 = t2 * x1 * t63
      t66 = t1 ** 2
      t70 = s * t66 * x1 * t59 * t63
      t71 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t65,
     # -t60, 0.0D0, t70)
      t73 = t3 * t71 * t10
      t75 = t14 * t73 / 0.16D2
      t76 = FJET(XB1, XB2, s, 0.0D0, -t60, -t65, 0.0D0, t70, t75)
      t81 = FJET(XB1, XB2, s, 0.0D0, -t65, -t60, 0.0D0, t70, t75)
      t86 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t52)
      t88 = FJET(XB1, XB2, s, -t60, 0.0D0, 0.0D0, -t65, t70, t75)
      t93 = FJET(XB1, XB2, s, -t65, 0.0D0, 0.0D0, -t60, t70, t75)
      rrgg2gghhardt6s2em2 = t53 * t52 + t55 * t52 + t57 * t52 + t76 * pi
     # * t7 * t73 / 0.16D2 + t81 * pi * t7 * t73 / 0.16D2 + t86 * t52 + 
     #t88 * pi * t7 * t73 / 0.16D2 + t93 * pi * t7 * t73 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt6s2em3
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgg2gghhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t11 = pi * t3 * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t3 * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2gghhardt6s2em3 = -t12 * pi * t15 / 0.32D2 - t17 * pi * t15 / 
     #0.32D2 - t20 * pi * t15 / 0.32D2 - t23 * pi * t15 / 0.32D2

      end function



      doubleprecision function rrgg2gghhardt6s2em4
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
      doubleprecision rrgg2gghhard61J1
      doubleprecision rrgg2gghhard61J2
      doubleprecision rrgg2gghhard61J3
      doubleprecision rrgg2gghhard61J4
      doubleprecision rrgg2gghhard61J5
      doubleprecision rrgg2gghhard61J6
      doubleprecision rrgg2gghhard61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2gghhard61J1
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S23 ** 2
      t27 = 0.18D2 * t26
      t28 = S14 * S23
      t29 = 0.36D2 * t28
      t30 = S14 ** 2
      t31 = 0.18D2 * t30
      t32 = t27 - t29 + t31
      t36 = 0.216D3 * S24
      t48 = S24 * t9
      t54 = 0.1D1 + t3
      t57 = 0.1D1 + t48
      t62 = 0.36D2 * S23
      t63 = 0.36D2 * S14
      t64 = S13 * S23
      t65 = S13 * S14
      t67 = -0.36D2 * t64 + 0.36D2 * t65
      t71 = S14 * S24
      t72 = S23 * S24
      t74 = -0.36D2 * t71 + 0.36D2 * t72
      t97 = S13 * t30
      t98 = 0.18D2 * t97
      t99 = S13 * t26
      t101 = t65 * S23
      t108 = t30 * S24
      t110 = t26 * S24
      t111 = 0.18D2 * t110
      t112 = t28 * S24
      t124 = S13 ** 2
      t126 = 0.27D2 / 0.2D1 * t65
      t130 = 0.27D2 / 0.2D1 * t72
      t132 = S24 ** 2
      t139 = t124 * S23
      t141 = t124 * S13
      t143 = t124 * S14
      t144 = 0.9D1 * t143
      t149 = S14 * t132
      t151 = t132 * S23
      t152 = 0.9D1 * t151
      t154 = t132 * S24
      t159 = S12 ** 2
      t160 = 0.1D1 / t159
      t176 = 0.18D2 * S14
      t177 = 0.18D2 * S23
      t183 = 0.27D2 / 0.8D1 * t124
      t194 = 0.27D2 / 0.8D1 * t132
      t229 = 0.27D2 / 0.8D1 * t141
      t230 = 0.27D2 * t101
      t246 = 0.27D2 * t112
      t247 = 0.27D2 / 0.8D1 * t154
      t297 = 0.27D2 / 0.4D1 * t151
      t309 = 0.27D2 / 0.4D1 * t143
      t319 = S14 * t26
      t325 = t26 * S23
      t331 = t30 * S23
      t336 = t30 * S14
      t337 = S13 * t336
      t339 = S13 * t325
      t341 = t141 * S23
      t343 = t141 * S14
      t345 = t124 * t30
      t347 = t124 * t26
      t349 = S24 * t336
      t351 = S24 * t325
      t353 = t132 * t30
      t355 = t132 * t26
      t357 = t154 * S23
      t358 = 0.27D2 / 0.8D1 * t357
      t359 = t154 * S14
      t361 = t97 * S23
      t363 = t65 * t26
      t366 = 0.27D2 * t143 * S23
      t367 = t108 * S23
      t369 = t71 * t26
      t372 = 0.27D2 * t149 * S23
      t373 = t124 ** 2
      t374 = 0.9D1 / 0.8D1 * t373
      t375 = t132 ** 2
      t376 = 0.9D1 / 0.8D1 * t375
      t377 = -0.18D2 * t337 + 0.189D3 / 0.8D1 * t339 + 0.27D2 / 0.8D1 * 
     #t341 - 0.9D1 / 0.2D1 * t343 + 0.27D2 / 0.2D1 * t345 + 0.63D2 / 0.8
     #D1 * t347 - 0.18D2 * t349 + 0.189D3 / 0.8D1 * t351 + 0.27D2 / 0.2D
     #1 * t353 + 0.63D2 / 0.8D1 * t355 + t358 - 0.9D1 / 0.2D1 * t359 + 0
     #.135D3 / 0.2D1 * t361 - 0.117D3 / 0.2D1 * t363 - t366 + 0.135D3 / 
     #0.2D1 * t367 - 0.117D3 / 0.2D1 * t369 - t372 + t374 + t376
      t379 = -0.36D2 * t319 - 0.45D2 / 0.16D2 * t99 + 0.1845D4 / 0.16D2 
     #* t149 + 0.27D2 * t108 - 0.585D3 / 0.4D1 * t141 + 0.18D2 * t325 + 
     #0.9D1 * t101 - 0.99D2 * t143 - 0.171D3 / 0.8D1 * t97 - 0.603D3 / 0
     #.16D2 * t151 + 0.18D2 * t331 + 0.1197D4 / 0.16D2 * t112 - 0.333D3 
     #/ 0.8D1 * t110 + 0.63D2 / 0.16D2 * t154 + t377 * t9
      t402 = 0.27D2 / 0.8D1 * t343
      t411 = t376 + t374 - 0.117D3 / 0.2D1 * t361 + 0.135D3 / 0.2D1 * t3
     #63 - t366 - 0.117D3 / 0.2D1 * t367 + 0.135D3 / 0.2D1 * t369 - t372
     # + 0.189D3 / 0.8D1 * t337 - 0.18D2 * t339 - 0.9D1 / 0.2D1 * t341 +
     # t402 + 0.63D2 / 0.8D1 * t345 + 0.27D2 / 0.2D1 * t347 + 0.189D3 / 
     #0.8D1 * t349 - 0.18D2 * t351 + 0.63D2 / 0.8D1 * t353 + 0.27D2 / 0.
     #2D1 * t355 - 0.9D1 / 0.2D1 * t357 + 0.27D2 / 0.8D1 * t359
      t413 = -0.333D3 / 0.8D1 * t97 + 0.27D2 * t99 + 0.1845D4 / 0.16D2 *
     # t139 - 0.603D3 / 0.16D2 * t143 - 0.99D2 * t151 + 0.1197D4 / 0.16D
     #2 * t101 - 0.36D2 * t331 + 0.18D2 * t319 + 0.63D2 / 0.16D2 * t141 
     #- 0.585D3 / 0.4D1 * t154 + 0.9D1 * t112 - 0.171D3 / 0.8D1 * t110 -
     # 0.45D2 / 0.16D2 * t108 + 0.18D2 * t336 + t411 * t2
      t419 = ((0.18D2 - 0.9D1 / 0.8D1 * t48) * t4 + (0.18D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.189D3 / 0.8D1 * S24 + 0.189D3 / 0.8D1 * S13) * 
     #t23 * S34 + ((-0.333D3 / 0.8D1 * S24 - 0.45D2 / 0.16D2 * S13 + t17
     #6 + (t194 - 0.9D1 / 0.2D1 * t71 + 0.27D2 / 0.8D1 * t72) * t9) * t4
     # + (t177 - 0.333D3 / 0.8D1 * S13 - 0.45D2 / 0.16D2 * S24 + (-0.9D1
     # / 0.2D1 * t64 + 0.27D2 / 0.8D1 * t65 + t183) * t2) * t6 + 0.63D2 
     #/ 0.8D1 * t132 - 0.63D2 / 0.8D1 * t65 + 0.63D2 / 0.8D1 * t124 - 0.
     #63D2 / 0.8D1 * t72) * t23 + ((0.1197D4 / 0.16D2 * t71 + 0.333D3 / 
     #0.8D1 * t64 - 0.333D3 / 0.4D1 * t72 - 0.603D3 / 0.16D2 * t132 - 0.
     #45D2 * t65 + (0.9D1 * t149 - 0.27D2 / 0.2D1 * t108 - 0.63D2 / 0.8D
     #1 * t110 - t297 + t246 - t247) * t9) * t4 + (0.1197D4 / 0.16D2 * t
     #64 - 0.45D2 * t72 + 0.333D3 / 0.8D1 * t71 - 0.333D3 / 0.4D1 * t65 
     #- 0.603D3 / 0.16D2 * t124 + (-0.27D2 / 0.2D1 * t99 + t230 - t229 +
     # 0.9D1 * t139 - t309 - 0.63D2 / 0.8D1 * t97) * t2) * t6 + t229 + t
     #247 - t309 + 0.27D2 / 0.8D1 * t97 - t297 + 0.27D2 / 0.8D1 * t110) 
     #* S34 + t379 * t4 + t413 * t6 - t402 + 0.27D2 / 0.8D1 * t345 + 0.2
     #7D2 / 0.8D1 * t355 + t376 + t374 - 0.9D1 / 0.8D1 * t351 - t358 - 0
     #.9D1 / 0.8D1 * t337
      t423 = t26 ** 2
      t426 = t30 ** 2
      t451 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.18D2 * t23 + (t25 + t32 * t9) * t
     #4 + (t36 + t32 * t2) * t6) * t13) * t15 * t17 + (((0.18D2 - 0.18D2
     # * t3) * t4 + (0.18D2 - 0.18D2 * t48) * t6) * S12 + (0.36D2 * t54 
     #* t4 + 0.36D2 * t57 * t6) * S34 + (t62 - t25 + t63 + t67 * t2) * t
     #4 + (t62 + t63 - t36 + t74 * t9) * t6 + ((-0.18D2 * t54 * t4 - 0.1
     #8D2 * t57 * t6) * t23 + ((-t62 + t63 - t67 * t2) * t4 + (-t63 + t6
     #2 - t74 * t9) * t6) * S34 + (t31 + t29 - 0.216D3 * t65 - t27 + (-t
     #98 - 0.18D2 * t99 + 0.36D2 * t101) * t2) * t4 + (-0.216D3 * t72 + 
     #t29 + t27 - t31 + (-0.18D2 * t108 - t111 + 0.36D2 * t112) * t9) * 
     #t6) * t13 + ((0.9D1 / 0.2D1 * S13 * t4 + 0.9D1 / 0.2D1 * t7) * t23
     # + ((-0.9D1 / 0.2D1 * t124 + t126 - 0.9D1 * t64) * t4 + (t130 - 0.
     #9D1 * t71 - 0.9D1 / 0.2D1 * t132) * t6) * S34 + (t98 + 0.9D1 / 0.2
     #D1 * t99 + 0.9D1 / 0.2D1 * t139 + 0.9D1 * t141 + t144 - 0.27D2 / 0
     #.2D1 * t101) * t4 + (-0.27D2 / 0.2D1 * t112 + 0.9D1 / 0.2D1 * t149
     # + t152 + 0.9D1 / 0.2D1 * t108 + t111 + 0.9D1 * t154) * t6) * t160
     #) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.9D1 / 0.8D1 * S24 
     #* t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t159 + ((0.171D3 / 0.2D
     #1 * S13 + t176 + t22 + t177 + 0.171D3 / 0.2D1 * S24) * S34 + (0.27
     #D2 * S24 + t176 - 0.171D3 / 0.8D1 * S13 + (t183 + 0.27D2 / 0.8D1 *
     # t64 - 0.9D1 / 0.2D1 * t65) * t9) * t4 + (0.27D2 * S13 - 0.171D3 /
     # 0.8D1 * S24 + t177 + (-0.9D1 / 0.2D1 * t72 + 0.27D2 / 0.8D1 * t71
     # + t194) * t2) * t6 - t126 - t130 + 0.27D2 / 0.2D1 * t132 + 0.27D2
     # / 0.2D1 * t124) * S12 + (-0.189D3 / 0.2D1 * S13 - t63 - 0.72D2 * 
     #S34 - t62 - 0.189D3 / 0.2D1 * S24) * t23 + ((0.9D1 * S13 - t63 + 0
     #.1197D4 / 0.16D2 * S24) * t4 + (-t62 + 0.9D1 * S24 + 0.1197D4 / 0.
     #16D2 * S13) * t6 + 0.27D2 * t72 + 0.27D2 * t65 - 0.27D2 * t132 - 0
     #.27D2 * t124) * S34 + (0.54D2 * t71 - 0.99D2 * t124 + 0.1197D4 / 0
     #.16D2 * t72 + 0.1845D4 / 0.16D2 * t132 + t31 + t27 + 0.9D1 * t65 -
     # t29 - 0.45D2 * t64 + (-0.27D2 / 0.4D1 * t139 - t229 + t230 - 0.63
     #D2 / 0.8D1 * t99 + t144 - 0.27D2 / 0.2D1 * t97) * t9) * t4 + (t27 
     #+ 0.1845D4 / 0.16D2 * t124 - 0.45D2 * t71 + 0.54D2 * t64 + t31 + 0
     #.9D1 * t72 - t29 - 0.99D2 * t132 + 0.1197D4 / 0.16D2 * t65 + (-0.2
     #7D2 / 0.2D1 * t110 - 0.27D2 / 0.4D1 * t149 + t152 - 0.63D2 / 0.8D1
     # * t108 + t246 - t247) * t2) * t6 - 0.9D1 / 0.2D1 * t154 - 0.9D1 /
     # 0.2D1 * t141 + t152 - 0.9D1 / 0.2D1 * t110 + t144 - 0.9D1 / 0.2D1
     # * t97 + t419 * t13 + ((-0.18D2 * t64 * t336 + 0.9D1 * S13 * t423 
     #+ 0.9D1 * S13 * t426 - 0.18D2 * t339 * S14 + 0.27D2 * t99 * t30) *
     # t9 * t4 + (0.9D1 * S24 * t426 - 0.18D2 * t351 * S14 + 0.27D2 * t1
     #10 * t30 - 0.18D2 * t72 * t336 + 0.9D1 * S24 * t423) * t2 * t6) * 
     #t160
      rrgg2gghhard61J1 = t451 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard61J2
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S23 ** 2
      t27 = 0.18D2 * t26
      t28 = S14 * S23
      t29 = 0.36D2 * t28
      t30 = S14 ** 2
      t31 = 0.18D2 * t30
      t32 = t27 - t29 + t31
      t36 = 0.216D3 * S24
      t48 = S24 * t9
      t62 = 0.36D2 * S14
      t63 = 0.27D2 * S23
      t64 = S13 * S23
      t65 = S13 * S14
      t67 = -0.36D2 * t64 + 0.36D2 * t65
      t71 = 0.27D2 * S14
      t72 = 0.36D2 * S23
      t73 = S14 * S24
      t74 = S23 * S24
      t76 = -0.36D2 * t73 + 0.36D2 * t74
      t101 = 0.45D2 * t28
      t103 = S13 * t30
      t104 = 0.18D2 * t103
      t105 = S13 * t26
      t107 = t65 * S23
      t115 = t30 * S24
      t117 = t26 * S24
      t118 = 0.18D2 * t117
      t119 = t28 * S24
      t132 = S13 ** 2
      t136 = S24 ** 2
      t143 = S14 * t132
      t144 = 0.9D1 * t143
      t145 = t132 * S13
      t148 = t132 * S23
      t152 = t136 * S23
      t153 = 0.9D1 * t152
      t155 = S14 * t136
      t157 = t136 * S24
      t162 = S12 ** 2
      t163 = 0.1D1 / t162
      t179 = 0.18D2 * S14
      t180 = 0.18D2 * S23
      t186 = 0.27D2 / 0.8D1 * t132
      t197 = 0.27D2 / 0.8D1 * t136
      t234 = 0.27D2 / 0.8D1 * t145
      t235 = 0.27D2 * t107
      t251 = 0.27D2 * t119
      t252 = 0.27D2 / 0.8D1 * t157
      t300 = 0.27D2 / 0.2D1 * t115
      t302 = 0.27D2 / 0.4D1 * t152
      t312 = 0.27D2 / 0.2D1 * t105
      t314 = 0.27D2 / 0.4D1 * t143
      t327 = t30 * S23
      t329 = S14 * t26
      t335 = t26 * S23
      t340 = t30 * S14
      t341 = S13 * t340
      t343 = S13 * t335
      t345 = t145 * S23
      t347 = t145 * S14
      t349 = t132 * t30
      t351 = t132 * t26
      t353 = S24 * t340
      t355 = S24 * t335
      t357 = t136 * t30
      t359 = t136 * t26
      t361 = t157 * S23
      t362 = 0.27D2 / 0.8D1 * t361
      t363 = t157 * S14
      t365 = t103 * S23
      t367 = t65 * t26
      t370 = 0.27D2 * t143 * S23
      t371 = t115 * S23
      t373 = t73 * t26
      t376 = 0.27D2 * t155 * S23
      t377 = t132 ** 2
      t378 = 0.9D1 / 0.8D1 * t377
      t379 = t136 ** 2
      t380 = 0.9D1 / 0.8D1 * t379
      t381 = -0.18D2 * t341 + 0.189D3 / 0.8D1 * t343 + 0.27D2 / 0.8D1 * 
     #t345 - 0.9D1 / 0.2D1 * t347 + 0.27D2 / 0.2D1 * t349 + 0.63D2 / 0.8
     #D1 * t351 - 0.18D2 * t353 + 0.189D3 / 0.8D1 * t355 + 0.27D2 / 0.2D
     #1 * t357 + 0.63D2 / 0.8D1 * t359 + t362 - 0.9D1 / 0.2D1 * t363 + 0
     #.135D3 / 0.2D1 * t365 - 0.117D3 / 0.2D1 * t367 - t370 + 0.135D3 / 
     #0.2D1 * t371 - 0.117D3 / 0.2D1 * t373 - t376 + t378 + t380
      t383 = -0.171D3 / 0.8D1 * t103 - 0.243D3 / 0.4D1 * t117 - 0.657D3 
     #/ 0.8D1 * t152 + 0.18D2 * t327 - 0.36D2 * t329 - 0.99D2 * t143 - 0
     #.45D2 / 0.16D2 * t105 - 0.585D3 / 0.4D1 * t145 + 0.693D3 / 0.4D1 *
     # t155 - t300 + 0.18D2 * t335 - 0.171D3 / 0.8D1 * t157 + 0.9D1 * t1
     #07 + 0.531D3 / 0.4D1 * t119 + t381 * t9
      t405 = 0.27D2 / 0.8D1 * t347
      t414 = t380 + t378 - 0.117D3 / 0.2D1 * t365 + 0.135D3 / 0.2D1 * t3
     #67 - t370 - 0.117D3 / 0.2D1 * t371 + 0.135D3 / 0.2D1 * t373 - t376
     # + 0.189D3 / 0.8D1 * t341 - 0.18D2 * t343 - 0.9D1 / 0.2D1 * t345 +
     # t405 + 0.63D2 / 0.8D1 * t349 + 0.27D2 / 0.2D1 * t351 + 0.189D3 / 
     #0.8D1 * t353 - 0.18D2 * t355 + 0.63D2 / 0.8D1 * t357 + 0.27D2 / 0.
     #2D1 * t359 - 0.9D1 / 0.2D1 * t361 + 0.27D2 / 0.8D1 * t363
      t416 = 0.18D2 * t329 - 0.36D2 * t327 - 0.171D3 / 0.8D1 * t145 - 0.
     #585D3 / 0.4D1 * t157 - 0.45D2 / 0.16D2 * t115 - t312 + 0.18D2 * t3
     #40 + 0.531D3 / 0.4D1 * t107 + 0.9D1 * t119 - 0.171D3 / 0.8D1 * t11
     #7 + 0.693D3 / 0.4D1 * t148 - 0.99D2 * t152 - 0.657D3 / 0.8D1 * t14
     #3 - 0.243D3 / 0.4D1 * t103 + t414 * t2
      t422 = ((0.18D2 - 0.9D1 / 0.8D1 * t48) * t4 + (0.18D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.189D3 / 0.8D1 * S24 + 0.189D3 / 0.8D1 * S13) * 
     #t23 * S34 + ((-0.45D2 / 0.16D2 * S13 + t179 - 0.243D3 / 0.4D1 * S2
     #4 + (t197 - 0.9D1 / 0.2D1 * t73 + 0.27D2 / 0.8D1 * t74) * t9) * t4
     # + (t180 - 0.45D2 / 0.16D2 * S24 - 0.243D3 / 0.4D1 * S13 + (-0.9D1
     # / 0.2D1 * t64 + 0.27D2 / 0.8D1 * t65 + t186) * t2) * t6 - 0.63D2 
     #/ 0.8D1 * t65 + 0.63D2 / 0.8D1 * t132 - 0.63D2 / 0.8D1 * t74 + 0.6
     #3D2 / 0.8D1 * t136) * t23 + ((0.333D3 / 0.8D1 * t64 + 0.531D3 / 0.
     #4D1 * t73 - 0.45D2 * t65 - 0.657D3 / 0.8D1 * t136 - 0.243D3 / 0.2D
     #1 * t74 + (0.9D1 * t155 - t300 - 0.63D2 / 0.8D1 * t117 - t302 + t2
     #51 - t252) * t9) * t4 + (-0.45D2 * t74 + 0.333D3 / 0.8D1 * t73 + 0
     #.531D3 / 0.4D1 * t64 - 0.657D3 / 0.8D1 * t132 - 0.243D3 / 0.2D1 * 
     #t65 + (-t312 + t235 - t234 + 0.9D1 * t148 - t314 - 0.63D2 / 0.8D1 
     #* t103) * t2) * t6 - t314 + 0.27D2 / 0.8D1 * t103 - t302 + 0.27D2 
     #/ 0.8D1 * t117 + t234 + t252) * S34 + t383 * t4 + t416 * t6 + 0.27
     #D2 / 0.8D1 * t359 + t380 - 0.9D1 / 0.8D1 * t355 - t362 - t405 + 0.
     #27D2 / 0.8D1 * t349 + t378 - 0.9D1 / 0.8D1 * t341
      t426 = t26 ** 2
      t429 = t30 ** 2
      t454 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.18D2 * t23 + (t25 + t32 * t9) * t
     #4 + (t36 + t32 * t2) * t6) * t13) * t15 * t17 + (((0.18D2 - 0.18D2
     # * t3) * t4 + (0.18D2 - 0.18D2 * t48) * t6) * S12 + ((0.45D2 + 0.3
     #6D2 * t3) * t4 + (0.45D2 + 0.36D2 * t48) * t6) * S34 + (t62 - t25 
     #+ t63 + t67 * t2) * t4 + (t71 - t36 + t72 + t76 * t9) * t6 + (((-0
     #.9D1 - 0.18D2 * t3) * t4 + (-0.9D1 - 0.18D2 * t48) * t6) * t23 + (
     #(-0.54D2 * S23 + t71 - t67 * t2) * t4 + (-0.54D2 * S14 + t63 - t76
     # * t9) * t6) * S34 + (-0.216D3 * t65 + t31 + t101 - 0.9D1 * t26 + 
     #(-t104 - 0.18D2 * t105 + 0.36D2 * t107) * t2) * t4 + (t27 - 0.9D1 
     #* t30 - 0.216D3 * t74 + t101 + (-0.18D2 * t115 - t118 + 0.36D2 * t
     #119) * t9) * t6) * t13 + ((0.9D1 / 0.2D1 * S13 * t4 + 0.9D1 / 0.2D
     #1 * t7) * t23 + ((-0.9D1 * t64 - 0.9D1 / 0.2D1 * t132) * t4 + (-0.
     #9D1 / 0.2D1 * t136 - 0.9D1 * t73) * t6) * S34 + (t144 + 0.9D1 * t1
     #45 + t104 + 0.9D1 / 0.2D1 * t105 + 0.9D1 / 0.2D1 * t148) * t4 + (t
     #118 + t153 + 0.9D1 / 0.2D1 * t115 + 0.9D1 / 0.2D1 * t155 + 0.9D1 *
     # t157) * t6) * t163) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0
     #.9D1 / 0.8D1 * S24 * t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t162
     # + ((0.171D3 / 0.2D1 * S13 + t179 + t22 + t180 + 0.171D3 / 0.2D1 *
     # S24) * S34 + (-0.27D2 / 0.2D1 * S24 + t179 - 0.171D3 / 0.8D1 * S1
     #3 + (t186 + 0.27D2 / 0.8D1 * t64 - 0.9D1 / 0.2D1 * t65) * t9) * t4
     # + (-0.27D2 / 0.2D1 * S13 - 0.171D3 / 0.8D1 * S24 + t180 + (-0.9D1
     # / 0.2D1 * t74 + 0.27D2 / 0.8D1 * t73 + t197) * t2) * t6 - 0.27D2 
     #/ 0.2D1 * t65 - 0.27D2 / 0.2D1 * t74 + 0.27D2 / 0.2D1 * t136 + 0.2
     #7D2 / 0.2D1 * t132) * S12 + (-0.189D3 / 0.2D1 * S13 - t62 - 0.72D2
     # * S34 - t72 - 0.189D3 / 0.2D1 * S24) * t23 + ((0.531D3 / 0.4D1 * 
     #S24 + 0.9D1 * S13 - t62) * t4 + (-t72 + 0.9D1 * S24 + 0.531D3 / 0.
     #4D1 * S13) * t6 + 0.27D2 * t65 - 0.27D2 * t136 - 0.27D2 * t132 + 0
     #.27D2 * t74) * S34 + (0.9D1 * t65 - 0.45D2 * t64 + t31 + 0.693D3 /
     # 0.4D1 * t136 - 0.99D2 * t132 - 0.27D2 * t73 - t29 + t27 + 0.531D3
     # / 0.4D1 * t74 + (-0.27D2 / 0.4D1 * t148 - t234 + t235 - 0.63D2 / 
     #0.8D1 * t105 + t144 - 0.27D2 / 0.2D1 * t103) * t9) * t4 + (t27 - 0
     #.45D2 * t73 + 0.693D3 / 0.4D1 * t132 - t29 + t31 - 0.99D2 * t136 +
     # 0.9D1 * t74 - 0.27D2 * t64 + 0.531D3 / 0.4D1 * t65 + (-0.27D2 / 0
     #.2D1 * t117 - 0.27D2 / 0.4D1 * t155 + t153 - 0.63D2 / 0.8D1 * t115
     # + t251 - t252) * t2) * t6 + t144 - 0.9D1 / 0.2D1 * t103 + t153 - 
     #0.9D1 / 0.2D1 * t117 - 0.9D1 / 0.2D1 * t157 - 0.9D1 / 0.2D1 * t145
     # + t422 * t13 + ((-0.18D2 * t64 * t340 + 0.9D1 * S13 * t426 + 0.9D
     #1 * S13 * t429 - 0.18D2 * t343 * S14 + 0.27D2 * t105 * t30) * t9 *
     # t4 + (0.9D1 * S24 * t429 - 0.18D2 * t355 * S14 + 0.27D2 * t117 * 
     #t30 - 0.18D2 * t74 * t340 + 0.9D1 * S24 * t426) * t2 * t6) * t163
      rrgg2gghhard61J2 = t454 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard61J3
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S23 ** 2
      t27 = 0.54D2 * t26
      t28 = S14 * S23
      t29 = 0.36D2 * t28
      t30 = S14 ** 2
      t31 = 0.18D2 * t30
      t36 = 0.216D3 * S24
      t37 = 0.18D2 * t26
      t38 = 0.54D2 * t30
      t51 = S24 * t9
      t65 = 0.36D2 * S14
      t66 = 0.27D2 * S23
      t67 = S13 * S23
      t68 = S13 * S14
      t70 = -0.36D2 * t67 + 0.36D2 * t68
      t74 = 0.27D2 * S14
      t75 = 0.36D2 * S23
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = -0.36D2 * t76 + 0.36D2 * t77
      t104 = 0.45D2 * t28
      t105 = S13 * t30
      t106 = 0.18D2 * t105
      t107 = S13 * t26
      t109 = t68 * S23
      t116 = t30 * S24
      t118 = t26 * S24
      t119 = 0.18D2 * t118
      t120 = t28 * S24
      t132 = S13 ** 2
      t135 = 0.27D2 / 0.2D1 * t68
      t138 = 0.27D2 / 0.2D1 * t77
      t140 = S24 ** 2
      t146 = S14 * t132
      t147 = 0.9D1 * t146
      t149 = t132 * S23
      t151 = t132 * S13
      t156 = t140 * S23
      t157 = 0.9D1 * t156
      t159 = S14 * t140
      t162 = t140 * S24
      t167 = S12 ** 2
      t168 = 0.1D1 / t167
      t179 = 0.18D2 * S24
      t180 = 0.18D2 * S13
      t184 = 0.18D2 * S14
      t185 = 0.18D2 * S23
      t191 = 0.27D2 / 0.8D1 * t132
      t202 = 0.27D2 / 0.8D1 * t140
      t235 = 0.27D2 / 0.8D1 * t151
      t236 = 0.27D2 * t109
      t252 = 0.27D2 * t120
      t253 = 0.27D2 / 0.8D1 * t162
      t305 = 0.27D2 / 0.4D1 * t156
      t317 = 0.27D2 / 0.4D1 * t146
      t330 = t30 * S23
      t332 = S14 * t26
      t339 = t26 * S23
      t344 = S13 * t339
      t346 = t30 * S14
      t347 = S13 * t346
      t349 = t132 * t30
      t351 = t151 * S14
      t353 = t151 * S23
      t355 = t132 * t26
      t357 = t140 * t30
      t359 = S24 * t339
      t361 = S24 * t346
      t363 = t162 * S14
      t365 = t162 * S23
      t366 = 0.27D2 / 0.8D1 * t365
      t367 = t140 * t26
      t370 = 0.27D2 * t159 * S23
      t371 = t68 * t26
      t373 = t140 ** 2
      t374 = 0.9D1 / 0.8D1 * t373
      t375 = t132 ** 2
      t376 = 0.9D1 / 0.8D1 * t375
      t377 = t105 * S23
      t380 = 0.27D2 * t146 * S23
      t381 = t116 * S23
      t383 = t76 * t26
      t385 = 0.333D3 / 0.8D1 * t344 - 0.18D2 * t347 + 0.27D2 / 0.2D1 * t
     #349 - 0.9D1 / 0.2D1 * t351 + 0.27D2 / 0.8D1 * t353 + 0.63D2 / 0.8D
     #1 * t355 + 0.27D2 / 0.2D1 * t357 + 0.333D3 / 0.8D1 * t359 - 0.18D2
     # * t361 - 0.9D1 / 0.2D1 * t363 + t366 + 0.63D2 / 0.8D1 * t367 - t3
     #70 - 0.189D3 / 0.2D1 * t371 + t374 + t376 + 0.171D3 / 0.2D1 * t377
     # - t380 + 0.171D3 / 0.2D1 * t381 - 0.189D3 / 0.2D1 * t383
      t387 = -0.171D3 / 0.8D1 * t105 - 0.927D3 / 0.8D1 * t118 - 0.2169D4
     # / 0.16D2 * t156 + 0.18D2 * t330 - 0.36D2 * t332 - 0.99D2 * t146 +
     # 0.243D3 / 0.16D2 * t107 - 0.585D3 / 0.4D1 * t151 + 0.4131D4 / 0.1
     #6D2 * t159 - 0.54D2 * t116 + 0.54D2 * t339 - 0.27D2 / 0.16D2 * t16
     #2 + 0.18D2 * t109 + 0.3339D4 / 0.16D2 * t120 + t385 * t9
      t411 = 0.27D2 / 0.8D1 * t351
      t419 = 0.63D2 / 0.8D1 * t349 + 0.27D2 / 0.2D1 * t355 + t374 + t376
     # + 0.171D3 / 0.2D1 * t371 - t380 - 0.189D3 / 0.2D1 * t381 + 0.171D
     #3 / 0.2D1 * t383 - t370 + 0.333D3 / 0.8D1 * t347 - 0.18D2 * t344 -
     # 0.9D1 / 0.2D1 * t353 + t411 + 0.63D2 / 0.8D1 * t357 + 0.27D2 / 0.
     #2D1 * t367 + 0.333D3 / 0.8D1 * t361 - 0.189D3 / 0.2D1 * t377 - 0.1
     #8D2 * t359 - 0.9D1 / 0.2D1 * t365 + 0.27D2 / 0.8D1 * t363
      t421 = 0.18D2 * t332 - 0.36D2 * t330 - 0.27D2 / 0.16D2 * t151 - 0.
     #585D3 / 0.4D1 * t162 + 0.243D3 / 0.16D2 * t116 - 0.54D2 * t107 + 0
     #.54D2 * t346 + 0.3339D4 / 0.16D2 * t109 + 0.18D2 * t120 - 0.171D3 
     #/ 0.8D1 * t118 + 0.4131D4 / 0.16D2 * t149 - 0.99D2 * t156 - 0.2169
     #D4 / 0.16D2 * t146 - 0.927D3 / 0.8D1 * t105 + t419 * t2
      t427 = ((0.54D2 - 0.9D1 / 0.8D1 * t51) * t4 + (0.54D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.333D3 / 0.8D1 * S13 + 0.333D3 / 0.8D1 * S24) * 
     #t23 * S34 + ((0.243D3 / 0.16D2 * S13 + 0.54D2 * S14 - 0.927D3 / 0.
     #8D1 * S24 + (t202 - 0.9D1 / 0.2D1 * t76 + 0.27D2 / 0.8D1 * t77) * 
     #t9) * t4 + (0.54D2 * S23 + 0.243D3 / 0.16D2 * S24 - 0.927D3 / 0.8D
     #1 * S13 + (-0.9D1 / 0.2D1 * t67 + 0.27D2 / 0.8D1 * t68 + t191) * t
     #2) * t6 - 0.63D2 / 0.8D1 * t68 + 0.63D2 / 0.8D1 * t132 - 0.63D2 / 
     #0.8D1 * t77 + 0.63D2 / 0.8D1 * t140) * t23 + ((0.621D3 / 0.8D1 * t
     #67 + 0.3627D4 / 0.16D2 * t76 - 0.54D2 * t68 - 0.2169D4 / 0.16D2 * 
     #t140 - 0.1215D4 / 0.4D1 * t77 + (0.9D1 * t159 - 0.27D2 / 0.2D1 * t
     #116 - 0.63D2 / 0.8D1 * t118 - t305 + t252 - t253) * t9) * t4 + (-0
     #.54D2 * t77 + 0.621D3 / 0.8D1 * t76 + 0.3627D4 / 0.16D2 * t67 - 0.
     #2169D4 / 0.16D2 * t132 - 0.1215D4 / 0.4D1 * t68 + (-0.27D2 / 0.2D1
     # * t107 + t236 - t235 + 0.9D1 * t149 - t317 - 0.63D2 / 0.8D1 * t10
     #5) * t2) * t6 - t317 + 0.27D2 / 0.8D1 * t105 - t305 + 0.27D2 / 0.8
     #D1 * t118 + t235 + t253) * S34 + t387 * t4 + t421 * t6 + 0.27D2 / 
     #0.8D1 * t367 + t374 - 0.9D1 / 0.8D1 * t359 - t366 - t411 + 0.27D2 
     #/ 0.8D1 * t349 + t376 - 0.9D1 / 0.8D1 * t347
      t431 = t26 ** 2
      t434 = t30 ** 2
      t459 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.54D2 * t23 + (t25 + (t27 - t29 + 
     #t31) * t9) * t4 + (t36 + (t37 + t38 - t29) * t2) * t6) * t13) * t1
     #5 * t17 + (((0.18D2 - 0.18D2 * t3) * t4 + (0.18D2 - 0.18D2 * t51) 
     #* t6) * S12 + ((0.45D2 + 0.36D2 * t3) * t4 + (0.45D2 + 0.36D2 * t5
     #1) * t6) * S34 + (t65 - t25 + t66 + t70 * t2) * t4 + (t74 - t36 + 
     #t75 + t79 * t9) * t6 + (((-0.54D2 - 0.18D2 * t3) * t4 + (-0.54D2 -
     # 0.18D2 * t51) * t6) * t23 + ((-0.108D3 * S23 + t74 - t70 * t2) * 
     #t4 + (-0.108D3 * S14 + t66 - t79 * t9) * t6) * S34 + (-0.216D3 * t
     #68 + t31 + t104 - t27 + (-t106 - 0.18D2 * t107 + 0.36D2 * t109) * 
     #t2) * t4 + (t37 - t38 - 0.216D3 * t77 + t104 + (-0.18D2 * t116 - t
     #119 + 0.36D2 * t120) * t9) * t6) * t13 + ((0.9D1 / 0.2D1 * S13 * t
     #4 + 0.9D1 / 0.2D1 * t7) * t23 + ((-0.9D1 / 0.2D1 * t132 - 0.9D1 * 
     #t67 - t135) * t4 + (-t138 - 0.9D1 * t76 - 0.9D1 / 0.2D1 * t140) * 
     #t6) * S34 + (t147 + t106 + 0.9D1 / 0.2D1 * t107 + 0.9D1 / 0.2D1 * 
     #t149 + 0.9D1 * t151 + 0.27D2 / 0.2D1 * t109) * t4 + (t119 + t157 +
     # 0.9D1 / 0.2D1 * t116 + 0.9D1 / 0.2D1 * t159 + 0.27D2 / 0.2D1 * t1
     #20 + 0.9D1 * t162) * t6) * t168) * s * z + (-0.9D1 / 0.8D1 * S13 *
     # t9 * t4 - 0.9D1 / 0.8D1 * S24 * t2 * t6 - t179 - t180) * t167 + (
     #(0.207D3 / 0.2D1 * S13 + t184 + t22 + t185 + 0.207D3 / 0.2D1 * S24
     #) * S34 + (-0.54D2 * S24 + t184 - 0.171D3 / 0.8D1 * S13 + (t191 + 
     #0.27D2 / 0.8D1 * t67 - 0.9D1 / 0.2D1 * t68) * t9) * t4 + (-0.54D2 
     #* S13 - 0.171D3 / 0.8D1 * S24 + t185 + (-0.9D1 / 0.2D1 * t77 + 0.2
     #7D2 / 0.8D1 * t76 + t202) * t2) * t6 - t135 - t138 + 0.27D2 / 0.2D
     #1 * t140 + 0.27D2 / 0.2D1 * t132) * S12 + (-0.261D3 / 0.2D1 * S13 
     #- t65 - 0.72D2 * S34 - t75 - 0.261D3 / 0.2D1 * S24) * t23 + ((0.33
     #39D4 / 0.16D2 * S24 + t180 - t65) * t4 + (-t75 + t179 + 0.3339D4 /
     # 0.16D2 * S13) * t6 + 0.27D2 * t68 - 0.27D2 * t140 - 0.27D2 * t132
     # + 0.27D2 * t77) * S34 + (0.9D1 * t68 - 0.54D2 * t67 + t31 + 0.413
     #1D4 / 0.16D2 * t140 - 0.99D2 * t132 - 0.108D3 * t76 - t29 + t27 + 
     #0.3627D4 / 0.16D2 * t77 + (-0.27D2 / 0.4D1 * t149 - t235 + t236 - 
     #0.63D2 / 0.8D1 * t107 + t147 - 0.27D2 / 0.2D1 * t105) * t9) * t4 +
     # (t37 - 0.54D2 * t76 + 0.4131D4 / 0.16D2 * t132 - t29 + t38 - 0.99
     #D2 * t140 + 0.9D1 * t77 - 0.108D3 * t67 + 0.3627D4 / 0.16D2 * t68 
     #+ (-0.27D2 / 0.2D1 * t118 - 0.27D2 / 0.4D1 * t159 + t157 - 0.63D2 
     #/ 0.8D1 * t116 + t252 - t253) * t2) * t6 + t147 - 0.9D1 / 0.2D1 * 
     #t105 + t157 - 0.9D1 / 0.2D1 * t118 - 0.9D1 / 0.2D1 * t162 - 0.9D1 
     #/ 0.2D1 * t151 + t427 * t13 + ((-0.18D2 * t67 * t346 + 0.9D1 * S13
     # * t431 + 0.9D1 * S13 * t434 - 0.18D2 * t344 * S14 + 0.27D2 * t107
     # * t30) * t9 * t4 + (0.9D1 * S24 * t434 - 0.18D2 * t359 * S14 + 0.
     #27D2 * t118 * t30 - 0.18D2 * t77 * t346 + 0.9D1 * S24 * t431) * t2
     # * t6) * t168
      rrgg2gghhard61J3 = t459 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard61J4
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S23 ** 2
      t27 = 0.90D2 * t26
      t28 = S14 * S23
      t29 = 0.36D2 * t28
      t30 = S14 ** 2
      t31 = 0.18D2 * t30
      t36 = 0.216D3 * S24
      t37 = 0.18D2 * t26
      t38 = 0.90D2 * t30
      t51 = S24 * t9
      t65 = 0.36D2 * S14
      t66 = 0.27D2 * S23
      t67 = S13 * S23
      t68 = S13 * S14
      t70 = -0.36D2 * t67 + 0.36D2 * t68
      t74 = 0.27D2 * S14
      t75 = 0.36D2 * S23
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = -0.36D2 * t76 + 0.36D2 * t77
      t104 = 0.45D2 * t28
      t106 = S13 * t30
      t107 = 0.18D2 * t106
      t108 = S13 * t26
      t110 = t68 * S23
      t118 = t30 * S24
      t120 = t26 * S24
      t121 = 0.18D2 * t120
      t122 = t28 * S24
      t134 = S13 ** 2
      t137 = 0.27D2 * t68
      t140 = 0.27D2 * t77
      t142 = S24 ** 2
      t148 = t134 * S14
      t149 = 0.9D1 * t148
      t151 = t134 * S23
      t153 = t134 * S13
      t155 = 0.27D2 * t110
      t158 = t142 * S23
      t159 = 0.9D1 * t158
      t161 = S14 * t142
      t163 = 0.27D2 * t122
      t164 = t142 * S24
      t169 = S12 ** 2
      t170 = 0.1D1 / t169
      t186 = 0.18D2 * S14
      t187 = 0.18D2 * S23
      t193 = 0.27D2 / 0.8D1 * t134
      t204 = 0.27D2 / 0.8D1 * t142
      t239 = 0.27D2 / 0.8D1 * t153
      t255 = 0.27D2 / 0.8D1 * t164
      t307 = 0.27D2 / 0.4D1 * t158
      t319 = 0.27D2 / 0.4D1 * t148
      t332 = t30 * S23
      t334 = S14 * t26
      t341 = t26 * S23
      t345 = S13 * t341
      t347 = t30 * S14
      t348 = S13 * t347
      t350 = t134 * t30
      t352 = t153 * S14
      t354 = t153 * S23
      t356 = t134 * t26
      t358 = t142 * t30
      t360 = S24 * t341
      t362 = S24 * t347
      t364 = t164 * S14
      t366 = t164 * S23
      t367 = 0.27D2 / 0.8D1 * t366
      t368 = t142 * t26
      t371 = 0.27D2 * t161 * S23
      t372 = t68 * t26
      t374 = t142 ** 2
      t375 = 0.9D1 / 0.8D1 * t374
      t376 = t134 ** 2
      t377 = 0.9D1 / 0.8D1 * t376
      t378 = t106 * S23
      t381 = 0.27D2 * t148 * S23
      t382 = t118 * S23
      t384 = t76 * t26
      t386 = 0.477D3 / 0.8D1 * t345 - 0.18D2 * t348 + 0.27D2 / 0.2D1 * t
     #350 - 0.9D1 / 0.2D1 * t352 + 0.27D2 / 0.8D1 * t354 + 0.63D2 / 0.8D
     #1 * t356 + 0.27D2 / 0.2D1 * t358 + 0.477D3 / 0.8D1 * t360 - 0.18D2
     # * t362 - 0.9D1 / 0.2D1 * t364 + t367 + 0.63D2 / 0.8D1 * t368 - t3
     #71 - 0.261D3 / 0.2D1 * t372 + t375 + t377 + 0.207D3 / 0.2D1 * t378
     # - t381 + 0.207D3 / 0.2D1 * t382 - 0.261D3 / 0.2D1 * t384
      t388 = -0.171D3 / 0.8D1 * t106 - 0.171D3 * t120 - 0.189D3 * t158 +
     # 0.18D2 * t332 - 0.36D2 * t334 - 0.99D2 * t148 + 0.531D3 / 0.16D2 
     #* t108 - 0.585D3 / 0.4D1 * t153 + 0.2745D4 / 0.8D1 * t161 - 0.189D
     #3 / 0.2D1 * t118 + 0.90D2 * t341 + 0.18D2 * t164 + t155 + 0.2277D4
     # / 0.8D1 * t122 + t386 * t9
      t411 = 0.27D2 / 0.8D1 * t352
      t419 = 0.63D2 / 0.8D1 * t350 + 0.27D2 / 0.2D1 * t356 + t375 + t377
     # + 0.207D3 / 0.2D1 * t372 - t381 - 0.261D3 / 0.2D1 * t382 + 0.207D
     #3 / 0.2D1 * t384 - t371 + 0.477D3 / 0.8D1 * t348 - 0.18D2 * t345 -
     # 0.9D1 / 0.2D1 * t354 + t411 + 0.63D2 / 0.8D1 * t358 + 0.27D2 / 0.
     #2D1 * t368 + 0.477D3 / 0.8D1 * t362 - 0.261D3 / 0.2D1 * t378 - 0.1
     #8D2 * t360 - 0.9D1 / 0.2D1 * t366 + 0.27D2 / 0.8D1 * t364
      t421 = 0.18D2 * t334 - 0.36D2 * t332 + 0.18D2 * t153 - 0.585D3 / 0
     #.4D1 * t164 + 0.531D3 / 0.16D2 * t118 - 0.189D3 / 0.2D1 * t108 + 0
     #.90D2 * t347 + 0.2277D4 / 0.8D1 * t110 + t163 - 0.171D3 / 0.8D1 * 
     #t120 + 0.2745D4 / 0.8D1 * t151 - 0.99D2 * t158 - 0.189D3 * t148 - 
     #0.171D3 * t106 + t419 * t2
      t427 = ((0.90D2 - 0.9D1 / 0.8D1 * t51) * t4 + (0.90D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.477D3 / 0.8D1 * S13 + 0.477D3 / 0.8D1 * S24) * 
     #t23 * S34 + ((0.531D3 / 0.16D2 * S13 + 0.90D2 * S14 - 0.171D3 * S2
     #4 + (t204 - 0.9D1 / 0.2D1 * t76 + 0.27D2 / 0.8D1 * t77) * t9) * t4
     # + (0.90D2 * S23 + 0.531D3 / 0.16D2 * S24 - 0.171D3 * S13 + (-0.9D
     #1 / 0.2D1 * t67 + 0.27D2 / 0.8D1 * t68 + t193) * t2) * t6 - 0.63D2
     # / 0.8D1 * t68 + 0.63D2 / 0.8D1 * t134 - 0.63D2 / 0.8D1 * t77 + 0.
     #63D2 / 0.8D1 * t142) * t23 + ((0.909D3 / 0.8D1 * t67 + 0.2565D4 / 
     #0.8D1 * t76 - 0.63D2 * t68 - 0.189D3 * t142 - 0.486D3 * t77 + (0.9
     #D1 * t161 - 0.27D2 / 0.2D1 * t118 - 0.63D2 / 0.8D1 * t120 - t307 +
     # t163 - t255) * t9) * t4 + (-0.63D2 * t77 + 0.909D3 / 0.8D1 * t76 
     #+ 0.2565D4 / 0.8D1 * t67 - 0.189D3 * t134 - 0.486D3 * t68 + (-0.27
     #D2 / 0.2D1 * t108 + t155 - t239 + 0.9D1 * t151 - t319 - 0.63D2 / 0
     #.8D1 * t106) * t2) * t6 - t319 + 0.27D2 / 0.8D1 * t106 - t307 + 0.
     #27D2 / 0.8D1 * t120 + t239 + t255) * S34 + t388 * t4 + t421 * t6 +
     # 0.27D2 / 0.8D1 * t368 + t375 - 0.9D1 / 0.8D1 * t360 - t367 - t411
     # + 0.27D2 / 0.8D1 * t350 + t377 - 0.9D1 / 0.8D1 * t348
      t431 = t26 ** 2
      t434 = t30 ** 2
      t459 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.90D2 * t23 + (t25 + (t27 - t29 + 
     #t31) * t9) * t4 + (t36 + (t37 + t38 - t29) * t2) * t6) * t13) * t1
     #5 * t17 + (((0.18D2 - 0.18D2 * t3) * t4 + (0.18D2 - 0.18D2 * t51) 
     #* t6) * S12 + ((0.45D2 + 0.36D2 * t3) * t4 + (0.45D2 + 0.36D2 * t5
     #1) * t6) * S34 + (t65 - t25 + t66 + t70 * t2) * t4 + (t74 - t36 + 
     #t75 + t79 * t9) * t6 + (((-0.99D2 - 0.18D2 * t3) * t4 + (-0.99D2 -
     # 0.18D2 * t51) * t6) * t23 + ((-0.162D3 * S23 + t74 - t70 * t2) * 
     #t4 + (-0.162D3 * S14 + t66 - t79 * t9) * t6) * S34 + (-0.216D3 * t
     #68 + t31 + t104 - 0.99D2 * t26 + (-t107 - 0.18D2 * t108 + 0.36D2 *
     # t110) * t2) * t4 + (t37 - 0.99D2 * t30 - 0.216D3 * t77 + t104 + (
     #-0.18D2 * t118 - t121 + 0.36D2 * t122) * t9) * t6) * t13 + ((0.9D1
     # / 0.2D1 * S13 * t4 + 0.9D1 / 0.2D1 * t7) * t23 + ((-0.9D1 / 0.2D1
     # * t134 - 0.9D1 * t67 - t137) * t4 + (-t140 - 0.9D1 * t76 - 0.9D1 
     #/ 0.2D1 * t142) * t6) * S34 + (t149 + t107 + 0.9D1 / 0.2D1 * t108 
     #+ 0.9D1 / 0.2D1 * t151 + 0.9D1 * t153 + t155) * t4 + (t121 + t159 
     #+ 0.9D1 / 0.2D1 * t118 + 0.9D1 / 0.2D1 * t161 + t163 + 0.9D1 * t16
     #4) * t6) * t170) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.9D1
     # / 0.8D1 * S24 * t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t169 + (
     #(0.243D3 / 0.2D1 * S13 + t186 + t22 + t187 + 0.243D3 / 0.2D1 * S24
     #) * S34 + (-0.189D3 / 0.2D1 * S24 + t186 - 0.171D3 / 0.8D1 * S13 +
     # (t193 + 0.27D2 / 0.8D1 * t67 - 0.9D1 / 0.2D1 * t68) * t9) * t4 + 
     #(-0.189D3 / 0.2D1 * S13 - 0.171D3 / 0.8D1 * S24 + t187 + (-0.9D1 /
     # 0.2D1 * t77 + 0.27D2 / 0.8D1 * t76 + t204) * t2) * t6 - 0.27D2 / 
     #0.2D1 * t68 - 0.27D2 / 0.2D1 * t77 + 0.27D2 / 0.2D1 * t142 + 0.27D
     #2 / 0.2D1 * t134) * S12 + (-0.333D3 / 0.2D1 * S13 - t65 - 0.72D2 *
     # S34 - t75 - 0.333D3 / 0.2D1 * S24) * t23 + ((0.2277D4 / 0.8D1 * S
     #24 + 0.27D2 * S13 - t65) * t4 + (-t75 + 0.27D2 * S24 + 0.2277D4 / 
     #0.8D1 * S13) * t6 + t137 - 0.27D2 * t142 - 0.27D2 * t134 + t140) *
     # S34 + (0.9D1 * t68 - 0.63D2 * t67 + t31 + 0.2745D4 / 0.8D1 * t142
     # - 0.99D2 * t134 - 0.189D3 * t76 - t29 + t27 + 0.2565D4 / 0.8D1 * 
     #t77 + (-0.27D2 / 0.4D1 * t151 - t239 + t155 - 0.63D2 / 0.8D1 * t10
     #8 + t149 - 0.27D2 / 0.2D1 * t106) * t9) * t4 + (t37 - 0.63D2 * t76
     # + 0.2745D4 / 0.8D1 * t134 - t29 + t38 - 0.99D2 * t142 + 0.9D1 * t
     #77 - 0.189D3 * t67 + 0.2565D4 / 0.8D1 * t68 + (-0.27D2 / 0.2D1 * t
     #120 - 0.27D2 / 0.4D1 * t161 + t159 - 0.63D2 / 0.8D1 * t118 + t163 
     #- t255) * t2) * t6 + t149 - 0.9D1 / 0.2D1 * t106 + t159 - 0.9D1 / 
     #0.2D1 * t120 - 0.9D1 / 0.2D1 * t164 - 0.9D1 / 0.2D1 * t153 + t427 
     #* t13 + ((-0.18D2 * t67 * t347 + 0.9D1 * S13 * t431 + 0.9D1 * S13 
     #* t434 - 0.18D2 * t345 * S14 + 0.27D2 * t108 * t30) * t9 * t4 + (0
     #.9D1 * S24 * t434 - 0.18D2 * t360 * S14 + 0.27D2 * t120 * t30 - 0.
     #18D2 * t77 * t347 + 0.9D1 * S24 * t431) * t2 * t6) * t170
      rrgg2gghhard61J4 = t459 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard61J5
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S23 ** 2
      t27 = 0.126D3 * t26
      t28 = S14 * S23
      t29 = 0.36D2 * t28
      t30 = S14 ** 2
      t31 = 0.18D2 * t30
      t36 = 0.216D3 * S24
      t37 = 0.18D2 * t26
      t38 = 0.126D3 * t30
      t51 = S24 * t9
      t65 = 0.36D2 * S14
      t66 = 0.27D2 * S23
      t67 = S13 * S23
      t68 = S13 * S14
      t70 = -0.36D2 * t67 + 0.36D2 * t68
      t74 = 0.27D2 * S14
      t75 = 0.36D2 * S23
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = -0.36D2 * t76 + 0.36D2 * t77
      t104 = 0.45D2 * t28
      t106 = S13 * t30
      t107 = 0.18D2 * t106
      t108 = S13 * t26
      t110 = t68 * S23
      t111 = 0.36D2 * t110
      t118 = t30 * S24
      t120 = t26 * S24
      t121 = 0.18D2 * t120
      t122 = t28 * S24
      t123 = 0.36D2 * t122
      t134 = S13 ** 2
      t142 = S24 ** 2
      t148 = t134 * S14
      t149 = 0.9D1 * t148
      t151 = t134 * S23
      t153 = t134 * S13
      t158 = t142 * S23
      t159 = 0.9D1 * t158
      t161 = S14 * t142
      t164 = t142 * S24
      t169 = S12 ** 2
      t170 = 0.1D1 / t169
      t186 = 0.18D2 * S14
      t187 = 0.18D2 * S23
      t193 = 0.27D2 / 0.8D1 * t134
      t204 = 0.27D2 / 0.8D1 * t142
      t241 = 0.27D2 / 0.8D1 * t153
      t242 = 0.27D2 * t110
      t258 = 0.27D2 * t122
      t259 = 0.27D2 / 0.8D1 * t164
      t311 = 0.27D2 / 0.4D1 * t158
      t323 = 0.27D2 / 0.4D1 * t148
      t336 = t30 * S23
      t338 = S14 * t26
      t345 = t26 * S23
      t349 = S13 * t345
      t351 = t30 * S14
      t352 = S13 * t351
      t354 = t134 * t30
      t356 = t153 * S14
      t358 = t153 * S23
      t360 = t134 * t26
      t362 = t142 * t30
      t364 = S24 * t345
      t366 = S24 * t351
      t368 = t164 * S14
      t370 = t164 * S23
      t371 = 0.27D2 / 0.8D1 * t370
      t372 = t142 * t26
      t375 = 0.27D2 * t161 * S23
      t376 = t68 * t26
      t378 = t142 ** 2
      t379 = 0.9D1 / 0.8D1 * t378
      t380 = t134 ** 2
      t381 = 0.9D1 / 0.8D1 * t380
      t382 = t106 * S23
      t385 = 0.27D2 * t148 * S23
      t386 = t118 * S23
      t388 = t76 * t26
      t390 = 0.621D3 / 0.8D1 * t349 - 0.18D2 * t352 + 0.27D2 / 0.2D1 * t
     #354 - 0.9D1 / 0.2D1 * t356 + 0.27D2 / 0.8D1 * t358 + 0.63D2 / 0.8D
     #1 * t360 + 0.27D2 / 0.2D1 * t362 + 0.621D3 / 0.8D1 * t364 - 0.18D2
     # * t366 - 0.9D1 / 0.2D1 * t368 + t371 + 0.63D2 / 0.8D1 * t372 - t3
     #75 - 0.333D3 / 0.2D1 * t376 + t379 + t381 + 0.243D3 / 0.2D1 * t382
     # - t385 + 0.243D3 / 0.2D1 * t386 - 0.333D3 / 0.2D1 * t388
      t392 = -0.171D3 / 0.8D1 * t106 - 0.1809D4 / 0.8D1 * t120 - 0.3879D
     #4 / 0.16D2 * t158 + 0.18D2 * t336 - 0.36D2 * t338 - 0.99D2 * t148 
     #+ 0.819D3 / 0.16D2 * t108 - 0.585D3 / 0.4D1 * t153 + 0.6849D4 / 0.
     #16D2 * t161 - 0.135D3 * t118 + 0.126D3 * t345 + 0.603D3 / 0.16D2 *
     # t164 + t111 + 0.5769D4 / 0.16D2 * t122 + t390 * t9
      t415 = 0.27D2 / 0.8D1 * t356
      t423 = 0.63D2 / 0.8D1 * t354 + 0.27D2 / 0.2D1 * t360 + t379 + t381
     # + 0.243D3 / 0.2D1 * t376 - t385 - 0.333D3 / 0.2D1 * t386 + 0.243D
     #3 / 0.2D1 * t388 - t375 + 0.621D3 / 0.8D1 * t352 - 0.18D2 * t349 -
     # 0.9D1 / 0.2D1 * t358 + t415 + 0.63D2 / 0.8D1 * t362 + 0.27D2 / 0.
     #2D1 * t372 + 0.621D3 / 0.8D1 * t366 - 0.333D3 / 0.2D1 * t382 - 0.1
     #8D2 * t364 - 0.9D1 / 0.2D1 * t370 + 0.27D2 / 0.8D1 * t368
      t425 = 0.18D2 * t338 - 0.36D2 * t336 + 0.603D3 / 0.16D2 * t153 - 0
     #.585D3 / 0.4D1 * t164 + 0.819D3 / 0.16D2 * t118 - 0.135D3 * t108 +
     # 0.126D3 * t351 + 0.5769D4 / 0.16D2 * t110 + t123 - 0.171D3 / 0.8D
     #1 * t120 + 0.6849D4 / 0.16D2 * t151 - 0.99D2 * t158 - 0.3879D4 / 0
     #.16D2 * t148 - 0.1809D4 / 0.8D1 * t106 + t423 * t2
      t431 = ((0.126D3 - 0.9D1 / 0.8D1 * t51) * t4 + (0.126D3 - 0.9D1 / 
     #0.8D1 * t3) * t6 + 0.621D3 / 0.8D1 * S13 + 0.621D3 / 0.8D1 * S24) 
     #* t23 * S34 + ((0.819D3 / 0.16D2 * S13 + 0.126D3 * S14 - 0.1809D4 
     #/ 0.8D1 * S24 + (t204 - 0.9D1 / 0.2D1 * t76 + 0.27D2 / 0.8D1 * t77
     #) * t9) * t4 + (0.126D3 * S23 + 0.819D3 / 0.16D2 * S24 - 0.1809D4 
     #/ 0.8D1 * S13 + (-0.9D1 / 0.2D1 * t67 + 0.27D2 / 0.8D1 * t68 + t19
     #3) * t2) * t6 - 0.63D2 / 0.8D1 * t68 + 0.63D2 / 0.8D1 * t134 - 0.6
     #3D2 / 0.8D1 * t77 + 0.63D2 / 0.8D1 * t142) * t23 + ((0.1197D4 / 0.
     #8D1 * t67 + 0.6633D4 / 0.16D2 * t76 - 0.72D2 * t68 - 0.3879D4 / 0.
     #16D2 * t142 - 0.2673D4 / 0.4D1 * t77 + (0.9D1 * t161 - 0.27D2 / 0.
     #2D1 * t118 - 0.63D2 / 0.8D1 * t120 - t311 + t258 - t259) * t9) * t
     #4 + (-0.72D2 * t77 + 0.1197D4 / 0.8D1 * t76 + 0.6633D4 / 0.16D2 * 
     #t67 - 0.3879D4 / 0.16D2 * t134 - 0.2673D4 / 0.4D1 * t68 + (-0.27D2
     # / 0.2D1 * t108 + t242 - t241 + 0.9D1 * t151 - t323 - 0.63D2 / 0.8
     #D1 * t106) * t2) * t6 - t323 + 0.27D2 / 0.8D1 * t106 - t311 + 0.27
     #D2 / 0.8D1 * t120 + t241 + t259) * S34 + t392 * t4 + t425 * t6 + 0
     #.27D2 / 0.8D1 * t372 + t379 - 0.9D1 / 0.8D1 * t364 - t371 - t415 +
     # 0.27D2 / 0.8D1 * t354 + t381 - 0.9D1 / 0.8D1 * t352
      t435 = t26 ** 2
      t438 = t30 ** 2
      t463 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.126D3 * t23 + (t25 + (t27 - t29 +
     # t31) * t9) * t4 + (t36 + (t37 + t38 - t29) * t2) * t6) * t13) * t
     #15 * t17 + (((0.18D2 - 0.18D2 * t3) * t4 + (0.18D2 - 0.18D2 * t51)
     # * t6) * S12 + ((0.45D2 + 0.36D2 * t3) * t4 + (0.45D2 + 0.36D2 * t
     #51) * t6) * S34 + (t65 - t25 + t66 + t70 * t2) * t4 + (t74 - t36 +
     # t75 + t79 * t9) * t6 + (((-0.144D3 - 0.18D2 * t3) * t4 + (-0.144D
     #3 - 0.18D2 * t51) * t6) * t23 + ((-0.216D3 * S23 + t74 - t70 * t2)
     # * t4 + (-0.216D3 * S14 + t66 - t79 * t9) * t6) * S34 + (-0.216D3 
     #* t68 + t31 + t104 - 0.144D3 * t26 + (-t107 - 0.18D2 * t108 + t111
     #) * t2) * t4 + (t37 - 0.144D3 * t30 - 0.216D3 * t77 + t104 + (-0.1
     #8D2 * t118 - t121 + t123) * t9) * t6) * t13 + ((0.9D1 / 0.2D1 * S1
     #3 * t4 + 0.9D1 / 0.2D1 * t7) * t23 + ((-0.9D1 / 0.2D1 * t134 - 0.9
     #D1 * t67 - 0.81D2 / 0.2D1 * t68) * t4 + (-0.81D2 / 0.2D1 * t77 - 0
     #.9D1 * t76 - 0.9D1 / 0.2D1 * t142) * t6) * S34 + (t149 + t107 + 0.
     #9D1 / 0.2D1 * t108 + 0.9D1 / 0.2D1 * t151 + 0.9D1 * t153 + 0.81D2 
     #/ 0.2D1 * t110) * t4 + (t121 + t159 + 0.9D1 / 0.2D1 * t118 + 0.9D1
     # / 0.2D1 * t161 + 0.81D2 / 0.2D1 * t122 + 0.9D1 * t164) * t6) * t1
     #70) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.9D1 / 0.8D1 * S2
     #4 * t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t169 + ((0.279D3 / 0.
     #2D1 * S13 + t186 + t22 + t187 + 0.279D3 / 0.2D1 * S24) * S34 + (-0
     #.135D3 * S24 + t186 - 0.171D3 / 0.8D1 * S13 + (t193 + 0.27D2 / 0.8
     #D1 * t67 - 0.9D1 / 0.2D1 * t68) * t9) * t4 + (-0.135D3 * S13 - 0.1
     #71D3 / 0.8D1 * S24 + t187 + (-0.9D1 / 0.2D1 * t77 + 0.27D2 / 0.8D1
     # * t76 + t204) * t2) * t6 - 0.27D2 / 0.2D1 * t68 - 0.27D2 / 0.2D1 
     #* t77 + 0.27D2 / 0.2D1 * t142 + 0.27D2 / 0.2D1 * t134) * S12 + (-0
     #.405D3 / 0.2D1 * S13 - t65 - 0.72D2 * S34 - t75 - 0.405D3 / 0.2D1 
     #* S24) * t23 + ((0.5769D4 / 0.16D2 * S24 + 0.36D2 * S13 - t65) * t
     #4 + (-t75 + 0.36D2 * S24 + 0.5769D4 / 0.16D2 * S13) * t6 + 0.27D2 
     #* t68 - 0.27D2 * t142 - 0.27D2 * t134 + 0.27D2 * t77) * S34 + (0.9
     #D1 * t68 - 0.72D2 * t67 + t31 + 0.6849D4 / 0.16D2 * t142 - 0.99D2 
     #* t134 - 0.270D3 * t76 - t29 + t27 + 0.6633D4 / 0.16D2 * t77 + (-0
     #.27D2 / 0.4D1 * t151 - t241 + t242 - 0.63D2 / 0.8D1 * t108 + t149 
     #- 0.27D2 / 0.2D1 * t106) * t9) * t4 + (t37 - 0.72D2 * t76 + 0.6849
     #D4 / 0.16D2 * t134 - t29 + t38 - 0.99D2 * t142 + 0.9D1 * t77 - 0.2
     #70D3 * t67 + 0.6633D4 / 0.16D2 * t68 + (-0.27D2 / 0.2D1 * t120 - 0
     #.27D2 / 0.4D1 * t161 + t159 - 0.63D2 / 0.8D1 * t118 + t258 - t259)
     # * t2) * t6 + t149 - 0.9D1 / 0.2D1 * t106 + t159 - 0.9D1 / 0.2D1 *
     # t120 - 0.9D1 / 0.2D1 * t164 - 0.9D1 / 0.2D1 * t153 + t431 * t13 +
     # ((-0.18D2 * t67 * t351 + 0.9D1 * S13 * t435 + 0.9D1 * S13 * t438 
     #- 0.18D2 * t349 * S14 + 0.27D2 * t108 * t30) * t9 * t4 + (0.9D1 * 
     #S24 * t438 - 0.18D2 * t364 * S14 + 0.27D2 * t120 * t30 - 0.18D2 * 
     #t77 * t351 + 0.9D1 * S24 * t435) * t2 * t6) * t170
      rrgg2gghhard61J5 = t463 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard61J6
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.180D3 * S34
      t23 = S34 ** 2
      t25 = 0.1080D4 * S13
      t26 = S23 ** 2
      t27 = 0.54D2 * t26
      t28 = S14 * S23
      t29 = 0.180D3 * t28
      t30 = S14 ** 2
      t31 = 0.90D2 * t30
      t36 = 0.1080D4 * S24
      t37 = 0.90D2 * t26
      t38 = 0.54D2 * t30
      t51 = S24 * t9
      t65 = 0.180D3 * S14
      t66 = 0.189D3 * S23
      t67 = S13 * S14
      t68 = S13 * S23
      t70 = -0.180D3 * t67 + 0.180D3 * t68
      t74 = 0.189D3 * S14
      t75 = 0.180D3 * S23
      t76 = S23 * S24
      t77 = S14 * S24
      t79 = -0.180D3 * t76 + 0.180D3 * t77
      t91 = 0.54D2 * S23
      t96 = 0.54D2 * S14
      t104 = 0.171D3 * t28
      t106 = S13 * t26
      t108 = t67 * S23
      t110 = S13 * t30
      t111 = 0.90D2 * t110
      t118 = t30 * S24
      t120 = t26 * S24
      t121 = 0.90D2 * t120
      t122 = t28 * S24
      t134 = S13 ** 2
      t137 = 0.135D3 * t67
      t140 = 0.135D3 * t76
      t142 = S24 ** 2
      t148 = t134 * S14
      t149 = 0.45D2 * t148
      t151 = t134 * S23
      t153 = t134 * S13
      t155 = 0.135D3 * t108
      t158 = t142 * S23
      t159 = 0.45D2 * t158
      t161 = S14 * t142
      t163 = 0.135D3 * t122
      t164 = t142 * S24
      t169 = S12 ** 2
      t170 = 0.1D1 / t169
      t186 = 0.90D2 * S14
      t187 = 0.90D2 * S23
      t193 = 0.135D3 / 0.8D1 * t134
      t204 = 0.135D3 / 0.8D1 * t142
      t241 = 0.135D3 / 0.8D1 * t153
      t253 = 0.135D3 / 0.8D1 * t164
      t302 = 0.135D3 / 0.4D1 * t158
      t317 = 0.135D3 / 0.4D1 * t148
      t330 = t30 * S23
      t332 = S14 * t26
      t339 = t26 * S23
      t344 = S13 * t339
      t346 = t30 * S14
      t347 = S13 * t346
      t349 = t134 * t30
      t351 = t153 * S14
      t353 = t153 * S23
      t355 = t134 * t26
      t357 = t142 * t30
      t359 = S24 * t339
      t361 = S24 * t346
      t363 = t164 * S14
      t365 = t164 * S23
      t366 = 0.135D3 / 0.8D1 * t365
      t367 = t142 * t26
      t370 = 0.135D3 * t161 * S23
      t371 = t67 * t26
      t373 = t142 ** 2
      t374 = 0.45D2 / 0.8D1 * t373
      t375 = t134 ** 2
      t376 = 0.45D2 / 0.8D1 * t375
      t377 = t110 * S23
      t380 = 0.135D3 * t148 * S23
      t381 = t118 * S23
      t383 = t77 * t26
      t385 = -0.369D3 / 0.8D1 * t344 + 0.90D2 * t347 - 0.135D3 / 0.2D1 *
     # t349 + 0.45D2 / 0.2D1 * t351 - 0.135D3 / 0.8D1 * t353 - 0.315D3 /
     # 0.8D1 * t355 - 0.135D3 / 0.2D1 * t357 - 0.369D3 / 0.8D1 * t359 + 
     #0.90D2 * t361 + 0.45D2 / 0.2D1 * t363 - t366 - 0.315D3 / 0.8D1 * t
     #367 + t370 + 0.297D3 / 0.2D1 * t371 - t374 - t376 - 0.531D3 / 0.2D
     #1 * t377 + t380 - 0.531D3 / 0.2D1 * t381 + 0.297D3 / 0.2D1 * t383
      t387 = 0.855D3 / 0.8D1 * t110 - 0.63D2 / 0.2D1 * t120 - 0.279D3 / 
     #0.4D1 * t158 - 0.90D2 * t330 + 0.180D3 * t332 + 0.495D3 * t148 + 0
     #.1377D4 / 0.16D2 * t106 + 0.2925D4 / 0.4D1 * t153 - 0.1431D4 / 0.8
     #D1 * t161 - 0.675D3 / 0.2D1 * t118 + 0.54D2 * t339 + 0.135D3 / 0.4
     #D1 * t164 - 0.9D1 * t108 - 0.99D2 / 0.8D1 * t122 + t385 * t9
      t411 = 0.135D3 / 0.8D1 * t351
      t419 = -0.315D3 / 0.8D1 * t349 - 0.135D3 / 0.2D1 * t355 - t374 - t
     #376 - 0.531D3 / 0.2D1 * t371 + t380 + 0.297D3 / 0.2D1 * t381 - 0.5
     #31D3 / 0.2D1 * t383 + t370 - 0.369D3 / 0.8D1 * t347 + 0.90D2 * t34
     #4 + 0.45D2 / 0.2D1 * t353 - t411 - 0.315D3 / 0.8D1 * t357 - 0.135D
     #3 / 0.2D1 * t367 - 0.369D3 / 0.8D1 * t361 + 0.297D3 / 0.2D1 * t377
     # + 0.90D2 * t359 + 0.45D2 / 0.2D1 * t365 - 0.135D3 / 0.8D1 * t363
      t421 = -0.90D2 * t332 + 0.180D3 * t330 + 0.135D3 / 0.4D1 * t153 + 
     #0.2925D4 / 0.4D1 * t164 + 0.1377D4 / 0.16D2 * t118 - 0.675D3 / 0.2
     #D1 * t106 + 0.54D2 * t346 - 0.99D2 / 0.8D1 * t108 - 0.9D1 * t122 +
     # 0.855D3 / 0.8D1 * t120 - 0.1431D4 / 0.8D1 * t151 + 0.495D3 * t158
     # - 0.279D3 / 0.4D1 * t148 - 0.63D2 / 0.2D1 * t110 + t419 * t2
      t427 = ((0.54D2 + 0.45D2 / 0.8D1 * t51) * t4 + (0.54D2 + 0.45D2 / 
     #0.8D1 * t3) * t6 - 0.369D3 / 0.8D1 * S13 - 0.369D3 / 0.8D1 * S24) 
     #* t23 * S34 + ((0.1377D4 / 0.16D2 * S13 + t96 - 0.63D2 / 0.2D1 * S
     #24 + (0.45D2 / 0.2D1 * t77 - 0.135D3 / 0.8D1 * t76 - t204) * t9) *
     # t4 + (t91 + 0.1377D4 / 0.16D2 * S24 - 0.63D2 / 0.2D1 * S13 + (-t1
     #93 - 0.135D3 / 0.8D1 * t67 + 0.45D2 / 0.2D1 * t68) * t2) * t6 + 0.
     #315D3 / 0.8D1 * t67 - 0.315D3 / 0.8D1 * t134 + 0.315D3 / 0.8D1 * t
     #76 - 0.315D3 / 0.8D1 * t142) * t23 + ((-0.513D3 / 0.8D1 * t68 + 0.
     #477D3 / 0.8D1 * t77 + 0.189D3 * t67 - 0.279D3 / 0.4D1 * t142 - 0.3
     #51D3 * t76 + (t253 + t302 - t163 + 0.135D3 / 0.2D1 * t118 + 0.315D
     #3 / 0.8D1 * t120 - 0.45D2 * t161) * t9) * t4 + (0.189D3 * t76 - 0.
     #513D3 / 0.8D1 * t77 + 0.477D3 / 0.8D1 * t68 - 0.279D3 / 0.4D1 * t1
     #34 - 0.351D3 * t67 + (-0.45D2 * t151 - t155 + 0.135D3 / 0.2D1 * t1
     #06 + t317 + 0.315D3 / 0.8D1 * t110 + t241) * t2) * t6 + t317 - 0.1
     #35D3 / 0.8D1 * t110 + t302 - 0.135D3 / 0.8D1 * t120 - t241 - t253)
     # * S34 + t387 * t4 + t421 * t6 - 0.135D3 / 0.8D1 * t367 - t374 + 0
     #.45D2 / 0.8D1 * t359 + t366 + t411 - 0.135D3 / 0.8D1 * t349 - t376
     # + 0.45D2 / 0.8D1 * t347
      t431 = t26 ** 2
      t434 = t30 ** 2
      t459 = (0.360D3 * t3 * t4 + 0.360D3 * t7 * t9) * t13 * t15 * s * t
     #17 * z + (-0.90D2 * S12 + t22 + (0.54D2 * t23 + (-t25 + (t27 + t29
     # - t31) * t9) * t4 + (-t36 + (-t37 + t38 + t29) * t2) * t6) * t13)
     # * t15 * t17 + (((-0.90D2 + 0.90D2 * t3) * t4 + (-0.90D2 + 0.90D2 
     #* t51) * t6) * S12 + ((-0.171D3 - 0.180D3 * t3) * t4 + (-0.171D3 -
     # 0.180D3 * t51) * t6) * S34 + (-t65 - t66 + t25 + t70 * t2) * t4 +
     # (-t74 - t75 + t36 + t79 * t9) * t6 + (((-0.81D2 + 0.90D2 * t3) * 
     #t4 + (-0.81D2 + 0.90D2 * t51) * t6) * t23 + ((-t91 - t74 - t70 * t
     #2) * t4 + (-t96 - t66 - t79 * t9) * t6) * S34 + (0.1080D4 * t67 - 
     #t31 - t104 - 0.81D2 * t26 + (0.90D2 * t106 - 0.180D3 * t108 + t111
     #) * t2) * t4 + (-t37 - 0.81D2 * t30 + 0.1080D4 * t76 - t104 + (0.9
     #0D2 * t118 + t121 - 0.180D3 * t122) * t9) * t6) * t13 + ((-0.45D2 
     #/ 0.2D1 * S13 * t4 - 0.45D2 / 0.2D1 * t7) * t23 + ((0.45D2 / 0.2D1
     # * t134 + 0.45D2 * t68 - t137) * t4 + (-t140 + 0.45D2 * t77 + 0.45
     #D2 / 0.2D1 * t142) * t6) * S34 + (-t149 - t111 - 0.45D2 / 0.2D1 * 
     #t106 - 0.45D2 / 0.2D1 * t151 - 0.45D2 * t153 + t155) * t4 + (-t121
     # - t159 - 0.45D2 / 0.2D1 * t118 - 0.45D2 / 0.2D1 * t161 + t163 - 0
     #.45D2 * t164) * t6) * t170) * s * z + (0.45D2 / 0.8D1 * S13 * t9 *
     # t4 + 0.45D2 / 0.8D1 * S24 * t2 * t6 + 0.90D2 * S24 + 0.90D2 * S13
     #) * t169 + ((-0.711D3 / 0.2D1 * S13 - t186 - t22 - t187 - 0.711D3 
     #/ 0.2D1 * S24) * S34 + (-0.675D3 / 0.2D1 * S24 - t186 + 0.855D3 / 
     #0.8D1 * S13 + (-t193 - 0.135D3 / 0.8D1 * t68 + 0.45D2 / 0.2D1 * t6
     #7) * t9) * t4 + (-0.675D3 / 0.2D1 * S13 + 0.855D3 / 0.8D1 * S24 - 
     #t187 + (0.45D2 / 0.2D1 * t76 - 0.135D3 / 0.8D1 * t77 - t204) * t2)
     # * t6 + 0.135D3 / 0.2D1 * t67 + 0.135D3 / 0.2D1 * t76 - 0.135D3 / 
     #0.2D1 * t142 - 0.135D3 / 0.2D1 * t134) * S12 + (0.657D3 / 0.2D1 * 
     #S13 + t65 + 0.360D3 * S34 + t75 + 0.657D3 / 0.2D1 * S24) * t23 + (
     #(-0.99D2 / 0.8D1 * S24 - 0.9D1 * S13 + t65) * t4 + (t75 - 0.9D1 * 
     #S24 - 0.99D2 / 0.8D1 * S13) * t6 - t137 + 0.135D3 * t142 + 0.135D3
     # * t134 - t140) * S34 + (-0.45D2 * t67 + 0.189D3 * t68 - t31 - 0.1
     #431D4 / 0.8D1 * t142 + 0.495D3 * t134 - 0.675D3 * t77 + t29 + t27 
     #+ 0.477D3 / 0.8D1 * t76 + (0.315D3 / 0.8D1 * t106 + 0.135D3 / 0.4D
     #1 * t151 - t149 + 0.135D3 / 0.2D1 * t110 + t241 - t155) * t9) * t4
     # + (-t37 + 0.189D3 * t77 - 0.1431D4 / 0.8D1 * t134 + t29 + t38 + 0
     #.495D3 * t142 - 0.45D2 * t76 - 0.675D3 * t68 + 0.477D3 / 0.8D1 * t
     #67 + (0.135D3 / 0.2D1 * t120 + t253 + 0.135D3 / 0.4D1 * t161 - t16
     #3 + 0.315D3 / 0.8D1 * t118 - t159) * t2) * t6 - t149 + 0.45D2 / 0.
     #2D1 * t110 - t159 + 0.45D2 / 0.2D1 * t120 + 0.45D2 / 0.2D1 * t164 
     #+ 0.45D2 / 0.2D1 * t153 + t427 * t13 + ((0.90D2 * t68 * t346 - 0.4
     #5D2 * S13 * t431 - 0.45D2 * S13 * t434 + 0.90D2 * t344 * S14 - 0.1
     #35D3 * t106 * t30) * t9 * t4 + (-0.45D2 * S24 * t434 + 0.90D2 * t3
     #59 * S14 - 0.135D3 * t120 * t30 + 0.90D2 * t76 * t346 - 0.45D2 * S
     #24 * t431) * t2 * t6) * t170
      rrgg2gghhard61J6 = t459 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard61J7
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
      t2 = S23 ** 2
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = S13 + S14 + S34
      t8 = S23 + S24 + S34
      t9 = S14 ** 2
      t12 = 0.1D1 / (S12 + S14 + S24)
      t16 = 0.1D1 / S12
      t18 = s ** 2
      t19 = z ** 2
      t22 = 0.45D2 * S13
      t23 = 0.45D2 * S14
      t25 = 0.45D2 * S23
      t26 = 0.45D2 * S24
      t33 = 0.270D3 * S13
      t37 = 0.270D3 * S24
      t40 = 0.180D3 * S23
      t43 = 0.180D3 * S14
      t49 = S14 * S23
      t50 = 0.45D2 * t49
      t61 = S13 + S24
      t68 = 0.90D2 * S24
      t71 = 0.90D2 * S13
      t76 = S24 ** 2
      t79 = S23 * S24
      t81 = S13 * S23
      t85 = S13 ** 2
      t88 = S13 * S14
      t90 = S14 * S24
      t106 = 0.180D3 * t90
      t109 = 0.180D3 * t81
      t120 = t49 * S24
      t122 = t2 * S23
      t128 = t88 * S23
      t138 = S24 * t9
      t139 = t138 * S23
      t141 = t90 * t2
      t143 = S13 * t9
      t144 = t143 * S23
      t148 = t88 * t2
      t156 = t9 * S14
      rrgg2gghhard61J7 = ((0.180D3 * t1 + 0.180D3 * t2 * t4 * t6 + 0.180
     #D3 * t8 * t9 * t12) * t16 * t18 * t19 + ((-t22 - t23 - 0.90D2 * S3
     #4 - t25 - t26) * S34 + 0.45D2 * t6 * S23 + 0.45D2 * t8 * S14 + ((-
     #t33 - 0.270D3 * S14 - 0.540D3 * S34 - 0.270D3 * S23 - t37) * t1 + 
     #((-t40 + t23) * t6 + (-t43 + t25) * t8) * S34 + (-0.270D3 * t2 - t
     #50) * t6 + (-0.270D3 * t9 - t50) * t8) * t16) * s * z + 0.90D2 * t
     #61 * S34 * S12 - 0.180D3 * t61 * t1 + ((t68 + t22) * t6 + (t26 + t
     #71) * t8) * S34 + (0.135D3 * t76 + 0.180D3 * t2 + 0.180D3 * t79 - 
     #0.45D2 * t81) * t6 + (0.135D3 * t85 + 0.180D3 * t9 + 0.180D3 * t88
     # - 0.45D2 * t90) * t8 + ((t33 + t43 + 0.360D3 * S34 + t40 + t37) *
     # t1 * S34 + ((-0.180D3 * S24 + t43 + t71) * t6 + (t68 - 0.180D3 * 
     #S13 + t40) * t8) * t1 + ((t106 - 0.720D3 * t79 - 0.45D2 * t88 + t1
     #09 - 0.45D2 * t76) * t6 + (t109 - 0.45D2 * t85 + t106 - 0.720D3 * 
     #t88 - 0.45D2 * t79) * t8) * S34 + (0.90D2 * t120 + 0.180D3 * t122 
     #+ 0.135D3 * S14 * t76 + 0.225D3 * t76 * S24 + 0.45D2 * t128 + 0.90
     #D2 * S13 * t2 - 0.45D2 * t76 * S23 - 0.180D3 * t2 * S24 + (0.90D2 
     #* S24 * t122 + 0.90D2 * t139 - 0.180D3 * t141 + 0.90D2 * t144 + 0.
     #90D2 * S13 * t122 - 0.180D3 * t148) * t4) * t6 + (0.225D3 * t85 * 
     #S13 + 0.180D3 * t156 + 0.135D3 * t85 * S23 + 0.45D2 * t120 + 0.90D
     #2 * t128 - 0.45D2 * t85 * S14 + 0.90D2 * t138 - 0.180D3 * t143 + (
     #-0.180D3 * t139 - 0.180D3 * t144 + 0.90D2 * t141 + 0.90D2 * t148 +
     # 0.90D2 * S13 * t156 + 0.90D2 * S24 * t156) * t12) * t8) * t16) / 
     #pi * wd / z

      end function
  
   
      subroutine rrgg2gghsoftt6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt6s1e1  
      doubleprecision rrgg2gghsoftt6s1e0  
      doubleprecision rrgg2gghsoftt6s1em1  
      doubleprecision rrgg2gghsoftt6s1em2  
      doubleprecision rrgg2gghsoftt6s1em3  
      doubleprecision rrgg2gghsoftt6s1em4  
      doubleprecision rrgg2gghsoftt6s2e1  
      doubleprecision rrgg2gghsoftt6s2e0  
      doubleprecision rrgg2gghsoftt6s2em1  
      doubleprecision rrgg2gghsoftt6s2em2  
      doubleprecision rrgg2gghsoftt6s2em3  
      doubleprecision rrgg2gghsoftt6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt6s1e1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t2 = x3 * t1
      t3 = x4 * pi
      t4 = Sin(t3)
      t5 = t4 ** 2
      t6 = t2 * t5
      t8 = log(0.4D1 * t6)
      t9 = t8 ** 2
      t10 = x3 * t5
      t12 = (-0.1D1 + x1) ** 2
      t14 = -0.1D1 + x3
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t10 * t1 * t12 * t15)
      t20 = t19 ** 2
      t21 = cos(t3)
      t23 = Sqrt(-x3 * t14)
      t27 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t21 * t23)
      t32 = log(-0.4D1 * t10 * t1 * t15)
      t33 = t32 ** 2
      t35 = t5 * t12
      t36 = t2 * t35
      t38 = log(0.4D1 * t36)
      t39 = t38 ** 2
      t44 = 0.90D2 * wd
      t46 = 0.180D3 * wd * lh
      t47 = -t44 + t46
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t58 = x2 ** 2
      t59 = t12 * t58
      t63 = log(-0.4D1 * t6 * t59 * t15)
      t65 = t1 * t58
      t69 = log(-0.4D1 * t10 * t65 * t15)
      t71 = x2 * x3
      t73 = (0.1D1 - x3 + t71) ** 2
      t74 = 0.1D1 / t73
      t76 = -0.1D1 + x2
      t78 = t58 * t74 * t76 * t14
      t81 = log(0.4D1 * t6 * t78)
      t84 = Sqrt(x3 * t76 * t14)
      t88 = 0.1D1 / (-0.1D1 - x3 + t71 + 0.2D1 * t21 * t84)
      t90 = t59 * t76
      t93 = log(-0.4D1 * t6 * t90)
      t96 = log(0.4D1 * t36 * t78)
      t101 = log(-0.4D1 * t10 * t65 * t76)
      t102 = x3 * t58
      t103 = t1 * t5
      t106 = log(0.4D1 * t102 * t103)
      t107 = t103 * t12
      t110 = log(0.4D1 * t102 * t107)
      t114 = 0.1D1 / x2
      t120 = log(-0.4D1 * t103 * t90)
      t121 = t120 ** 2
      t122 = t5 * t76
      t125 = log(-0.4D1 * t65 * t122)
      t126 = t125 ** 2
      t129 = log(0.4D1 * t65 * t35)
      t130 = t129 ** 2
      t133 = log(0.4D1 * t65 * t5)
      t134 = t133 ** 2
      t146 = log(0.4D1 * t107)
      t147 = t146 ** 2
      t149 = log(0.4D1 * t103)
      t150 = t149 ** 2
      t160 = lh ** 2
      t162 = pi ** 2
      t164 = -0.180D3 * t160 + 0.30D2 * t162
      t165 = wd * t164
      t166 = t46 - t44 + t165
      t173 = log(0.4D1 * t10)
      t176 = log(-0.4D1 * t10 * t15)
      t180 = t176 ** 2
      t183 = t173 ** 2
      t194 = -0.60D2 * lh * t162 + 0.240D3 * zeta3 + 0.120D3 * t160 * lh
      t195 = wd * t194
      t208 = log(-0.4D1 * t102 * t122)
      t209 = t208 ** 2
      t213 = log(-0.4D1 * t10 * t58 * t15)
      t214 = t213 ** 2
      t216 = t102 * t5
      t218 = log(0.4D1 * t216)
      t219 = t218 ** 2
      t224 = log(0.4D1 * t216 * t74 * t76 * t14)
      t225 = t224 ** 2
      t241 = t58 * t5
      t243 = log(0.4D1 * t241)
      t244 = t243 ** 2
      t247 = log(-0.4D1 * t241 * t76)
      t248 = t247 ** 2
      t265 = log(0.4D1 * t5)
      t266 = t265 * wd
      t268 = t265 ** 2
      t269 = t268 * wd
      t271 = 0.3D1 * wd - 0.2D1 * t266 + t269 / 0.2D1
      t279 = t268 * t265 * wd
      t282 = 0.2D1 * wd - t266
      t297 = t162 ** 2
      t298 = t160 ** 2
      t305 = t268 ** 2
      t308 = -(-0.90D2 * wd * (-t9 / 0.2D1 + t20 * t27 / 0.2D1 - t33 * t
     #27 / 0.2D1 + t39 / 0.2D1) + t47 * (t8 - t19 * t27 + t32 * t27 - t3
     #8)) * t53 * t55 / 0.20D2 + 0.9D1 * wd * (-t63 * t27 + t69 * t27 - 
     #t81 * t88 + t93 + t96 * t88 - t101 + t106 - t110) * t53 * t55 * t1
     #14 - (-0.90D2 * wd * (-t121 / 0.2D1 + t126 / 0.2D1 + t130 / 0.2D1 
     #- t134 / 0.2D1) + t47 * (t120 - t125 - t129 + t133)) * t55 * t114 
     #/ 0.10D2 + (t47 * (-t147 / 0.2D1 + t150 / 0.2D1) - 0.90D2 * wd * (
     #-t150 * t149 / 0.6D1 + t147 * t146 / 0.6D1) + t166 * (t146 - t149)
     #) * t55 / 0.20D2 + (t166 * (-t173 - t176 * t27) - 0.90D2 * wd * (-
     #t180 * t176 * t27 / 0.6D1 - t183 * t173 / 0.6D1) + (t46 - t44 + t1
     #65 + t195) * (0.1D1 + t27) + t47 * (t183 / 0.2D1 + t180 * t27 / 0.
     #2D1)) * t53 / 0.40D2 - (-0.90D2 * wd * (t209 / 0.2D1 - t214 * t27 
     #/ 0.2D1 - t219 / 0.2D1 + t225 * t88 / 0.2D1) + t47 * (-t208 + t213
     # * t27 + t218 - t224 * t88) + t166 * (t88 - t27)) * t53 * t114 / 0
     #.20D2 + (t47 * (t244 / 0.2D1 - t248 / 0.2D1) - 0.90D2 * wd * (t248
     # * t247 / 0.6D1 - t244 * t243 / 0.6D1) + t166 * (-t243 + t247)) * 
     #t114 / 0.20D2 - 0.9D1 / 0.2D1 * t271 * lh - t195 / 0.40D2 - 0.9D1 
     #/ 0.4D1 * wd + 0.9D1 / 0.4D1 * t266 - 0.9D1 / 0.8D1 * t269 + 0.3D1
     # / 0.8D1 * t279 - t282 * t164 / 0.40D2 + 0.9D1 / 0.2D1 * (0.4D1 * 
     #wd - 0.3D1 * t266 + t269 - t279 / 0.6D1) * lh + t271 * t164 / 0.40
     #D2 + t282 * t194 / 0.40D2 + wd * (-0.480D3 * lh * zeta3 - t297 - 0
     #.60D2 * t298 + 0.60D2 * t160 * t162) / 0.40D2 - 0.3D1 / 0.32D2 * t
     #305 * wd
      t309 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t308)
      rrgg2gghsoftt6s1e1 = t309 * t308

      end function



      doubleprecision function rrgg2gghsoftt6s1e0
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t2 = x4 * pi
      t3 = Sin(t2)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t11 = t10 ** 2
      t13 = log(0.4D1 * t5)
      t14 = t13 ** 2
      t19 = 0.90D2 * wd
      t21 = 0.180D3 * wd * lh
      t22 = -t19 + t21
      t26 = 0.1D1 / x1
      t29 = x3 * t1
      t32 = log(0.4D1 * t29 * t4)
      t33 = x3 * t4
      t35 = -0.1D1 + x3
      t36 = 0.1D1 / t35
      t40 = log(-0.4D1 * t33 * t1 * t7 * t36)
      t41 = cos(t2)
      t43 = Sqrt(-x3 * t35)
      t47 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t41 * t43)
      t52 = log(-0.4D1 * t33 * t1 * t36)
      t54 = t4 * t7
      t57 = log(0.4D1 * t29 * t54)
      t60 = 0.1D1 / x3
      t64 = x2 ** 2
      t66 = -0.1D1 + x2
      t70 = log(-0.4D1 * t5 * t7 * t64 * t66)
      t71 = t64 * t1
      t72 = t4 * t66
      t75 = log(-0.4D1 * t71 * t72)
      t78 = log(0.4D1 * t71 * t54)
      t81 = log(0.4D1 * t71 * t4)
      t84 = 0.1D1 / x2
      t89 = log(0.4D1 * t33)
      t90 = t89 ** 2
      t93 = log(-0.4D1 * t33 * t36)
      t94 = t93 ** 2
      t103 = lh ** 2
      t105 = pi ** 2
      t107 = -0.180D3 * t103 + 0.30D2 * t105
      t108 = wd * t107
      t117 = log(0.4D1 * t4)
      t118 = t117 * wd
      t119 = 0.2D1 * wd - t118
      t124 = t117 ** 2
      t125 = t124 * wd
      t147 = x3 * t64
      t150 = log(-0.4D1 * t147 * t72)
      t154 = log(-0.4D1 * t33 * t64 * t36)
      t156 = t147 * t4
      t158 = log(0.4D1 * t156)
      t159 = x2 * x3
      t161 = (0.1D1 - x3 + t159) ** 2
      t167 = log(0.4D1 * t156 / t161 * t66 * t35)
      t170 = Sqrt(x3 * t66 * t35)
      t174 = 0.1D1 / (-0.1D1 - x3 + t159 + 0.2D1 * t41 * t170)
      t185 = t64 * t4
      t187 = log(0.4D1 * t185)
      t188 = t187 ** 2
      t191 = log(-0.4D1 * t185 * t66)
      t192 = t191 ** 2
      t202 = (-0.90D2 * wd * (-t11 / 0.2D1 + t14 / 0.2D1) + t22 * (t10 -
     # t13)) * t26 / 0.20D2 + 0.9D1 / 0.2D1 * wd * (t32 - t40 * t47 + t5
     #2 * t47 - t57) * t60 * t26 + 0.9D1 * wd * (t70 - t75 - t78 + t81) 
     #* t26 * t84 + (-0.90D2 * wd * (t90 / 0.2D1 + t94 * t47 / 0.2D1) + 
     #t22 * (-t89 - t93 * t47) + (t21 - t19 + t108) * (0.1D1 + t47)) * t
     #60 / 0.40D2 - 0.9D1 / 0.2D1 * t119 * lh - 0.9D1 / 0.4D1 * wd + 0.9
     #D1 / 0.4D1 * t118 - 0.9D1 / 0.8D1 * t125 - t108 / 0.40D2 + 0.9D1 /
     # 0.2D1 * (0.3D1 * wd - 0.2D1 * t118 + t125 / 0.2D1) * lh + wd * (-
     #0.60D2 * lh * t105 + 0.240D3 * zeta3 + 0.120D3 * t103 * lh) / 0.40
     #D2 + 0.3D1 / 0.8D1 * t124 * t117 * wd + t119 * t107 / 0.40D2 - (-0
     #.90D2 * wd * (-t150 + t154 * t47 + t158 - t167 * t174) + t22 * (t1
     #74 - t47)) * t60 * t84 / 0.20D2 + (-0.90D2 * wd * (t188 / 0.2D1 - 
     #t192 / 0.2D1) + t22 * (-t187 + t191)) * t84 / 0.20D2
      t203 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t202)
      rrgg2gghsoftt6s1e0 = t203 * t202

      end function



      doubleprecision function rrgg2gghsoftt6s1em1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t4 = x3 * t3
      t6 = log(0.4D1 * t4)
      t7 = -0.1D1 + x3
      t11 = log(-0.4D1 * t4 / t7)
      t12 = cos(t1)
      t14 = Sqrt(-x3 * t7)
      t18 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t12 * t14)
      t24 = wd * lh
      t30 = 0.1D1 / x3
      t33 = x1 ** 2
      t34 = t33 * t3
      t36 = (-0.1D1 + x1) ** 2
      t39 = log(0.4D1 * t34 * t36)
      t41 = log(0.4D1 * t34)
      t48 = -0.1D1 + x2
      t51 = Sqrt(x3 * t48 * t7)
      t58 = 0.1D1 / x2
      t62 = x2 ** 2
      t63 = t62 * t3
      t65 = log(0.4D1 * t63)
      t68 = log(-0.4D1 * t63 * t48)
      t76 = log(0.4D1 * t3)
      t77 = t76 * wd
      t83 = t76 ** 2
      t86 = lh ** 2
      t88 = pi ** 2
      t93 = (-0.90D2 * wd * (-t6 - t11 * t18) + (-0.90D2 * wd + 0.180D3 
     #* t24) * (0.1D1 + t18)) * t30 / 0.40D2 - 0.9D1 / 0.2D1 * wd * (t39
     # - t41) / x1 + 0.9D1 / 0.2D1 * wd * (0.1D1 / (-0.1D1 - x3 + x2 * x
     #3 + 0.2D1 * t12 * t51) - t18) * t30 * t58 - 0.9D1 / 0.2D1 * wd * (
     #-t65 + t68) * t58 - 0.9D1 / 0.2D1 * t24 - 0.9D1 / 0.4D1 * wd + 0.9
     #D1 / 0.4D1 * t77 + 0.9D1 / 0.2D1 * (0.2D1 * wd - t77) * lh - 0.9D1
     # / 0.8D1 * t83 * wd + wd * (-0.180D3 * t86 + 0.30D2 * t88) / 0.40D
     #2
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      rrgg2gghsoftt6s1em1 = t94 * t93

      end function



      doubleprecision function rrgg2gghsoftt6s1em2
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = cos(t1)
      t5 = Sqrt(-x3 * (-0.1D1 + x3))
      t18 = Sin(t1)
      t19 = t18 ** 2
      t21 = log(0.4D1 * t19)
      t24 = -0.9D1 / 0.4D1 * wd * (0.1D1 + 0.1D1 / (-0.1D1 - x3 + 0.2D1 
     #* t2 * t5)) / x3 - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.2D1 * wd * lh + 
     #0.9D1 / 0.4D1 * t21 * wd
      t25 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t24)
      rrgg2gghsoftt6s1em2 = t25 * t24

      end function



      doubleprecision function rrgg2gghsoftt6s1em3
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.4D1 * wd)
      rrgg2gghsoftt6s1em3 = -0.9D1 / 0.4D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt6s1em4
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt6s2e1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t4 = x3 * t3
      t5 = x1 ** 2
      t7 = (-0.1D1 + x1) ** 2
      t9 = -0.1D1 + x3
      t10 = 0.1D1 / t9
      t14 = log(-0.4D1 * t4 * t5 * t7 * t10)
      t15 = t14 ** 2
      t16 = cos(t1)
      t18 = Sqrt(-x3 * t9)
      t22 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t16 * t18)
      t27 = log(-0.4D1 * t4 * t5 * t10)
      t28 = t27 ** 2
      t30 = x3 * t5
      t31 = t3 * t7
      t32 = t30 * t31
      t34 = log(0.4D1 * t32)
      t35 = t34 ** 2
      t36 = t30 * t3
      t38 = log(0.4D1 * t36)
      t39 = t38 ** 2
      t44 = 0.90D2 * wd
      t46 = 0.180D3 * wd * lh
      t47 = -t44 + t46
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t58 = x2 ** 2
      t59 = t58 * t5
      t60 = -0.1D1 + x2
      t64 = log(-0.4D1 * t4 * t59 * t60)
      t65 = x2 * x3
      t67 = (0.1D1 - x3 + t65) ** 2
      t68 = 0.1D1 / t67
      t71 = t58 * t68 * t60 * t9
      t74 = log(0.4D1 * t36 * t71)
      t77 = Sqrt(x3 * t60 * t9)
      t81 = 0.1D1 / (-0.1D1 - x3 + t65 + 0.2D1 * t16 * t77)
      t83 = x3 * t58
      t84 = t5 * t3
      t87 = log(0.4D1 * t83 * t84)
      t91 = log(-0.4D1 * t4 * t59 * t10)
      t93 = t7 * t58
      t94 = t93 * t60
      t97 = log(-0.4D1 * t36 * t94)
      t101 = log(-0.4D1 * t36 * t93 * t10)
      t105 = log(0.4D1 * t32 * t71)
      t107 = t84 * t7
      t110 = log(0.4D1 * t83 * t107)
      t114 = 0.1D1 / x2
      t120 = log(-0.4D1 * t84 * t94)
      t121 = t120 ** 2
      t122 = t3 * t60
      t125 = log(-0.4D1 * t59 * t122)
      t126 = t125 ** 2
      t129 = log(0.4D1 * t59 * t31)
      t130 = t129 ** 2
      t133 = log(0.4D1 * t59 * t3)
      t134 = t133 ** 2
      t146 = log(0.4D1 * t107)
      t147 = t146 ** 2
      t149 = log(0.4D1 * t84)
      t150 = t149 ** 2
      t160 = lh ** 2
      t162 = pi ** 2
      t164 = -0.180D3 * t160 + 0.30D2 * t162
      t165 = wd * t164
      t166 = t46 - t44 + t165
      t173 = log(0.4D1 * t4)
      t176 = log(-0.4D1 * t4 * t10)
      t180 = t176 ** 2
      t183 = t173 ** 2
      t194 = -0.60D2 * lh * t162 + 0.240D3 * zeta3 + 0.120D3 * t160 * lh
      t195 = wd * t194
      t206 = t83 * t3
      t208 = log(0.4D1 * t206)
      t209 = t208 ** 2
      t213 = log(-0.4D1 * t4 * t58 * t10)
      t214 = t213 ** 2
      t218 = log(-0.4D1 * t83 * t122)
      t219 = t218 ** 2
      t224 = log(0.4D1 * t206 * t68 * t60 * t9)
      t225 = t224 ** 2
      t241 = t58 * t3
      t243 = log(0.4D1 * t241)
      t244 = t243 ** 2
      t247 = log(-0.4D1 * t241 * t60)
      t248 = t247 ** 2
      t265 = log(0.4D1 * t3)
      t266 = t265 * wd
      t268 = t265 ** 2
      t269 = t268 * wd
      t271 = 0.3D1 * wd - 0.2D1 * t266 + t269 / 0.2D1
      t279 = t268 * t265 * wd
      t282 = 0.2D1 * wd - t266
      t297 = t162 ** 2
      t298 = t160 ** 2
      t305 = t268 ** 2
      t308 = (-0.90D2 * wd * (-t15 * t22 / 0.2D1 + t28 * t22 / 0.2D1 - t
     #35 / 0.2D1 + t39 / 0.2D1) + t47 * (t14 * t22 - t27 * t22 + t34 - t
     #38)) * t53 * t55 / 0.20D2 - 0.9D1 * wd * (t64 + t74 * t81 - t87 - 
     #t91 * t22 - t97 + t101 * t22 - t105 * t81 + t110) * t53 * t55 * t1
     #14 - (-0.90D2 * wd * (-t121 / 0.2D1 + t126 / 0.2D1 + t130 / 0.2D1 
     #- t134 / 0.2D1) + t47 * (t120 - t125 - t129 + t133)) * t55 * t114 
     #/ 0.10D2 + (t47 * (-t147 / 0.2D1 + t150 / 0.2D1) - 0.90D2 * wd * (
     #-t150 * t149 / 0.6D1 + t147 * t146 / 0.6D1) + t166 * (t146 - t149)
     #) * t55 / 0.20D2 - (t166 * (t173 + t176 * t22) - 0.90D2 * wd * (t1
     #80 * t176 * t22 / 0.6D1 + t183 * t173 / 0.6D1) + (t46 - t44 + t165
     # + t195) * (-0.1D1 - t22) + t47 * (-t183 / 0.2D1 - t180 * t22 / 0.
     #2D1)) * t53 / 0.40D2 + (-0.90D2 * wd * (t209 / 0.2D1 + t214 * t22 
     #/ 0.2D1 - t219 / 0.2D1 - t225 * t81 / 0.2D1) + t47 * (-t208 - t213
     # * t22 + t218 + t224 * t81) + t166 * (-t81 + t22)) * t53 * t114 / 
     #0.20D2 + (t47 * (t244 / 0.2D1 - t248 / 0.2D1) - 0.90D2 * wd * (t24
     #8 * t247 / 0.6D1 - t244 * t243 / 0.6D1) + t166 * (-t243 + t247)) *
     # t114 / 0.20D2 - 0.9D1 / 0.2D1 * t271 * lh - t195 / 0.40D2 - 0.9D1
     # / 0.4D1 * wd + 0.9D1 / 0.4D1 * t266 - 0.9D1 / 0.8D1 * t269 + 0.3D
     #1 / 0.8D1 * t279 - t282 * t164 / 0.40D2 + 0.9D1 / 0.2D1 * (0.4D1 *
     # wd - 0.3D1 * t266 + t269 - t279 / 0.6D1) * lh + t271 * t164 / 0.4
     #0D2 + t282 * t194 / 0.40D2 + wd * (-0.480D3 * lh * zeta3 - t297 - 
     #0.60D2 * t298 + 0.60D2 * t160 * t162) / 0.40D2 - 0.3D1 / 0.32D2 * 
     #t305 * wd
      t309 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t308)
      rrgg2gghsoftt6s2e1 = t309 * t308

      end function



      doubleprecision function rrgg2gghsoftt6s2e0
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t2 = x4 * pi
      t3 = Sin(t2)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t11 = t10 ** 2
      t13 = log(0.4D1 * t5)
      t14 = t13 ** 2
      t19 = 0.90D2 * wd
      t21 = 0.180D3 * wd * lh
      t22 = -t19 + t21
      t26 = 0.1D1 / x1
      t29 = x3 * t4
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t29 * t1 * t7 * t32)
      t37 = cos(t2)
      t39 = Sqrt(-x3 * t31)
      t43 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t37 * t39)
      t48 = log(-0.4D1 * t29 * t1 * t32)
      t50 = x3 * t1
      t51 = t4 * t7
      t54 = log(0.4D1 * t50 * t51)
      t57 = log(0.4D1 * t50 * t4)
      t60 = 0.1D1 / x3
      t64 = x2 ** 2
      t66 = -0.1D1 + x2
      t70 = log(-0.4D1 * t5 * t7 * t64 * t66)
      t71 = t64 * t1
      t72 = t4 * t66
      t75 = log(-0.4D1 * t71 * t72)
      t78 = log(0.4D1 * t71 * t51)
      t81 = log(0.4D1 * t71 * t4)
      t84 = 0.1D1 / x2
      t89 = log(0.4D1 * t29)
      t90 = t89 ** 2
      t93 = log(-0.4D1 * t29 * t32)
      t94 = t93 ** 2
      t103 = lh ** 2
      t105 = pi ** 2
      t107 = -0.180D3 * t103 + 0.30D2 * t105
      t108 = wd * t107
      t117 = log(0.4D1 * t4)
      t118 = t117 * wd
      t119 = 0.2D1 * wd - t118
      t124 = t117 ** 2
      t125 = t124 * wd
      t147 = x3 * t64
      t148 = t147 * t4
      t150 = log(0.4D1 * t148)
      t154 = log(-0.4D1 * t29 * t64 * t32)
      t158 = log(-0.4D1 * t147 * t72)
      t159 = x2 * x3
      t161 = (0.1D1 - x3 + t159) ** 2
      t167 = log(0.4D1 * t148 / t161 * t66 * t31)
      t170 = Sqrt(x3 * t66 * t31)
      t174 = 0.1D1 / (-0.1D1 - x3 + t159 + 0.2D1 * t37 * t170)
      t185 = t64 * t4
      t187 = log(0.4D1 * t185)
      t188 = t187 ** 2
      t191 = log(-0.4D1 * t185 * t66)
      t192 = t191 ** 2
      t202 = (-0.90D2 * wd * (-t11 / 0.2D1 + t14 / 0.2D1) + t22 * (t10 -
     # t13)) * t26 / 0.20D2 - 0.9D1 / 0.2D1 * wd * (t36 * t43 - t48 * t4
     #3 + t54 - t57) * t60 * t26 + 0.9D1 * wd * (t70 - t75 - t78 + t81) 
     #* t26 * t84 - (-0.90D2 * wd * (-t90 / 0.2D1 - t94 * t43 / 0.2D1) +
     # t22 * (t89 + t93 * t43) + (t21 - t19 + t108) * (-0.1D1 - t43)) * 
     #t60 / 0.40D2 - 0.9D1 / 0.2D1 * t119 * lh - 0.9D1 / 0.4D1 * wd + 0.
     #9D1 / 0.4D1 * t118 - 0.9D1 / 0.8D1 * t125 - t108 / 0.40D2 + 0.9D1 
     #/ 0.2D1 * (0.3D1 * wd - 0.2D1 * t118 + t125 / 0.2D1) * lh + wd * (
     #-0.60D2 * lh * t105 + 0.240D3 * zeta3 + 0.120D3 * t103 * lh) / 0.4
     #0D2 + 0.3D1 / 0.8D1 * t124 * t117 * wd + t119 * t107 / 0.40D2 + (-
     #0.90D2 * wd * (-t150 - t154 * t43 + t158 + t167 * t174) + t22 * (-
     #t174 + t43)) * t60 * t84 / 0.20D2 + (-0.90D2 * wd * (t188 / 0.2D1 
     #- t192 / 0.2D1) + t22 * (-t187 + t191)) * t84 / 0.20D2
      t203 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t202)
      rrgg2gghsoftt6s2e0 = t203 * t202

      end function



      doubleprecision function rrgg2gghsoftt6s2em1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t4 = x3 * t3
      t6 = log(0.4D1 * t4)
      t7 = -0.1D1 + x3
      t11 = log(-0.4D1 * t4 / t7)
      t12 = cos(t1)
      t14 = Sqrt(-x3 * t7)
      t18 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t12 * t14)
      t24 = wd * lh
      t30 = 0.1D1 / x3
      t33 = x1 ** 2
      t34 = t33 * t3
      t36 = (-0.1D1 + x1) ** 2
      t39 = log(0.4D1 * t34 * t36)
      t41 = log(0.4D1 * t34)
      t48 = -0.1D1 + x2
      t51 = Sqrt(x3 * t48 * t7)
      t58 = 0.1D1 / x2
      t62 = x2 ** 2
      t63 = t62 * t3
      t65 = log(0.4D1 * t63)
      t68 = log(-0.4D1 * t63 * t48)
      t76 = log(0.4D1 * t3)
      t77 = t76 * wd
      t83 = t76 ** 2
      t86 = lh ** 2
      t88 = pi ** 2
      t93 = -(-0.90D2 * wd * (t6 + t11 * t18) + (-0.90D2 * wd + 0.180D3 
     #* t24) * (-0.1D1 - t18)) * t30 / 0.40D2 - 0.9D1 / 0.2D1 * wd * (t3
     #9 - t41) / x1 - 0.9D1 / 0.2D1 * wd * (-0.1D1 / (-0.1D1 - x3 + x2 *
     # x3 + 0.2D1 * t12 * t51) + t18) * t30 * t58 - 0.9D1 / 0.2D1 * wd *
     # (-t65 + t68) * t58 - 0.9D1 / 0.2D1 * t24 - 0.9D1 / 0.4D1 * wd + 0
     #.9D1 / 0.4D1 * t77 + 0.9D1 / 0.2D1 * (0.2D1 * wd - t77) * lh - 0.9
     #D1 / 0.8D1 * t83 * wd + wd * (-0.180D3 * t86 + 0.30D2 * t88) / 0.4
     #0D2
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      rrgg2gghsoftt6s2em1 = t94 * t93

      end function



      doubleprecision function rrgg2gghsoftt6s2em2
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = cos(t1)
      t5 = Sqrt(-x3 * (-0.1D1 + x3))
      t18 = Sin(t1)
      t19 = t18 ** 2
      t21 = log(0.4D1 * t19)
      t24 = 0.9D1 / 0.4D1 * wd * (-0.1D1 - 0.1D1 / (-0.1D1 - x3 + 0.2D1 
     #* t2 * t5)) / x3 - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.2D1 * wd * lh + 
     #0.9D1 / 0.4D1 * t21 * wd
      t25 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t24)
      rrgg2gghsoftt6s2em2 = t25 * t24

      end function



      doubleprecision function rrgg2gghsoftt6s2em3
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.4D1 * wd)
      rrgg2gghsoftt6s2em3 = -0.9D1 / 0.4D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt6s2em4
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt6s2em4 = 0.0D0

      end function
