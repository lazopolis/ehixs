  
      subroutine bggbH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH21J1  
      doubleprecision bggbH21J2  
      doubleprecision bggbH21J3  
      doubleprecision bggbH2n1e1  
      doubleprecision bggbH2n1e0  
      doubleprecision bggbH2n1em1  
      doubleprecision bggbH2n1em2  
      doubleprecision bggbH2n1em3  
      doubleprecision bggbH2n1em4  
      doubleprecision bggbH2n2e1  
      doubleprecision bggbH2n2e0  
      doubleprecision bggbH2n2em1  
      doubleprecision bggbH2n2em2  
      doubleprecision bggbH2n2em3  
      doubleprecision bggbH2n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH2n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH2n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH2n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH2n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH2n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH2n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t14 = log(0.4D1 * t12)
      t17 = t14 ** 2
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t43 = t5 ** 2
      t44 = t3 ** 2
      t49 = -0.2884936567583026D3 - t28 + t30
      t51 = t17 ** 2
      t59 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t60 = t22 * t59
      t63 = lh * t1
      t64 = t12 * x4
      t66 = log(0.4D1 * t64)
      t68 = t66 ** 2
      t75 = t31 * t1
      t81 = 0.90D2 * t1 * t22
      t90 = t49 * t1
      t91 = t90 * t60
      t93 = 0.1D1 / x4
      t96 = x1 ** 2
      t97 = t96 * t9
      t98 = t97 * t11
      t100 = log(0.4D1 * t98)
      t102 = t100 ** 2
      t122 = 0.1D1 / x1
      t125 = t11 * x4
      t128 = log(0.4D1 * t97 * t125)
      t135 = t128 ** 2
      t145 = x3 * t96
      t148 = log(0.4D1 * t145 * t64)
      t150 = 0.1D1 - x3
      t151 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, 0.10D1)
      t152 = t145 * t9
      t153 = -t150
      t154 = t11 * t153
      t155 = t154 * x4
      t158 = log(-0.4D1 * t152 * t155)
      t159 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, 0.10D1)
      t164 = t22 * (t59 - t159)
      t169 = 0.1D1 / x3
      t170 = t169 * t93
      t176 = log(-0.4D1 * t145 * t12 * t153)
      t178 = t145 * t12
      t180 = log(0.4D1 * t178)
      t186 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, 0.10D1)
      t188 = t176 ** 2
      t192 = t180 ** 2
      t197 = t75 * t164
      t202 = x3 * t9
      t205 = log(-0.4D1 * t202 * t154)
      t207 = t205 ** 2
      t210 = t202 * t11
      t212 = log(0.4D1 * t210)
      t214 = t212 ** 2
      t246 = log(0.4D1 * t202 * t125)
      t250 = log(-0.4D1 * t202 * t155)
      t257 = t250 ** 2
      t261 = t246 ** 2
      t270 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t1 * t22 *
     # t23 / 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 -
     # 0.90D2 * t17 * lh - 0.15D2 * t35) * t1 * t22 * t39 / 0.5760D4 - (
     #t43 + 0.60D2 * t44 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 
     #- t14 * t49 + 0.15D2 / 0.4D1 * t51 + t17 * t31 / 0.2D1 + 0.30D2 * 
     #t35 * lh) * t1 * t60 / 0.5760D4 + (-0.180D3 * t63 * t22 * (-t23 + 
     #t66 * t39 - t68 * t59 / 0.2D1) + t75 * t22 * (-t39 + t66 * t59) + 
     #t81 * (t66 * t23 - t68 * t39 / 0.2D1 + t68 * t66 * t59 / 0.6D1) - 
     #t91) * t93 / 0.5760D4 - (-0.180D3 * t63 * t22 * (t23 - t100 * t39 
     #+ t102 * t59 / 0.2D1) + t75 * t22 * (t39 - t100 * t59) + t81 * (-t
     #100 * t23 + t102 * t39 / 0.2D1 - t102 * t100 * t59 / 0.6D1) + t91)
     # * t122 / 0.2880D4 - (-0.180D3 * t63 * t22 * (t39 - t128 * t59) + 
     #t81 * (t23 - t128 * t39 + t135 * t59 / 0.2D1) + t75 * t60) * t122 
     #* t93 / 0.2880D4 - (t81 * (t39 - t148 * t59 - t151 + t158 * t159) 
     #- 0.180D3 * t63 * t164) * t122 * t170 / 0.2880D4 - (-0.180D3 * t63
     # * t22 * (-t151 + t176 * t159 + t39 - t180 * t59) + t81 * (-t186 +
     # t176 * t151 - t188 * t159 / 0.2D1 + t23 - t180 * t39 + t192 * t59
     # / 0.2D1) + t197) * t122 * t169 / 0.2880D4 - (-0.180D3 * t63 * t22
     # * (-t186 + t205 * t151 - t207 * t159 / 0.2D1 + t23 - t212 * t39 +
     # t214 * t59 / 0.2D1) + t75 * t22 * (t39 - t212 * t59 - t151 + t205
     # * t159) + t81 * (t205 * t186 - t207 * t151 / 0.2D1 + t207 * t205 
     #* t159 / 0.6D1 - t212 * t23 + t214 * t39 / 0.2D1 - t214 * t212 * t
     #59 / 0.6D1) + t90 * t164) * t169 / 0.5760D4 - (-0.180D3 * t63 * t2
     #2 * (t39 - t246 * t59 - t151 + t250 * t159) + t81 * (-t186 + t250 
     #* t151 - t257 * t159 / 0.2D1 + t23 - t246 * t39 + t261 * t59 / 0.2
     #D1) + t197) * t169 * t93 / 0.5760D4
      t271 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t270)
      t273 = -0.1D1 + x4
      t276 = -t273
      t277 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t276)
      t278 = t125 * t273
      t281 = log(-0.4D1 * t97 * t278)
      t282 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t276)
      t288 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t276)
      t290 = t281 ** 2
      t295 = t22 * t282
      t303 = log(-0.4D1 * t152 * t278)
      t305 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, t276)
      t310 = log(0.4D1 * t152 * t125 * t273 * t153)
      t311 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, t276)
      t316 = t22 * (t311 - t282)
      t323 = x4 * t273
      t327 = log(0.4D1 * t210 * t323 * t153)
      t331 = log(-0.4D1 * t202 * t278)
      t338 = t331 ** 2
      t341 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, t276)
      t343 = t327 ** 2
      t355 = log(-0.4D1 * t12 * t323)
      t357 = t355 ** 2
      t380 = -(-0.180D3 * t63 * t22 * (-t277 + t281 * t282) + t81 * (-t2
     #88 + t281 * t277 - t290 * t282 / 0.2D1) - t75 * t295) * t122 * t93
     # / 0.2880D4 - (t81 * (-t277 + t303 * t282 + t305 - t310 * t311) - 
     #0.180D3 * t63 * t316) * t122 * t170 / 0.2880D4 - (-0.180D3 * t63 *
     # t22 * (t305 - t327 * t311 - t277 + t331 * t282) + t81 * (-t288 + 
     #t331 * t277 - t338 * t282 / 0.2D1 + t341 - t327 * t305 + t343 * t3
     #11 / 0.2D1) + t75 * t316) * t169 * t93 / 0.5760D4 + (-0.180D3 * t6
     #3 * t22 * (t288 - t355 * t277 + t357 * t282 / 0.2D1) + t75 * t22 *
     # (t277 - t355 * t282) + t81 * (-t355 * t288 + t357 * t277 / 0.2D1 
     #- t357 * t355 * t282 / 0.6D1) + t90 * t295) * t93 / 0.5760D4
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t273, t2 * x4, 0.0D0,
     # t380)
      t384 = -0.1D1 + x1
      t386 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t387 = t384 ** 2
      t388 = t96 * t387
      t391 = log(0.4D1 * t12 * t388)
      t392 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t394 = t391 ** 2
      t395 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t414 = t22 * t395
      t418 = t388 * x4
      t421 = log(0.4D1 * t12 * t418)
      t428 = t421 ** 2
      t433 = t75 * t414
      t439 = log(0.4D1 * t210 * t418)
      t452 = log(0.4D1 * t202 * t11 * t96 * t387)
      t459 = t452 ** 2
      t468 = -(0.180D3 * t63 * t22 * (t386 - t391 * t392 + t394 * t395 /
     # 0.2D1) - t75 * t22 * (t392 - t391 * t395) - t81 * (-t391 * t386 +
     # t394 * t392 / 0.2D1 - t394 * t391 * t395 / 0.6D1) - t90 * t414) *
     # t122 / 0.2880D4 - (-0.180D3 * t63 * t22 * (-t392 + t421 * t395) +
     # t81 * (-t386 + t421 * t392 - t428 * t395 / 0.2D1) - t433) * t122 
     #* t93 / 0.2880D4 - (t81 * (-t392 + t439 * t395) + 0.180D3 * t63 * 
     #t414) * t122 * t170 / 0.2880D4 - (-0.180D3 * t63 * t22 * (-t392 + 
     #t452 * t395) + t81 * (-t386 + t452 * t392 - t459 * t395 / 0.2D1) -
     # t433) * t122 * t169 / 0.2880D4
      t469 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t384, 0.0D0, 0.0D0,
     # t468)
      t471 = KAPPA2(x1, x2, 0.10D1, t276, z)
      t472 = s * t471
      t473 = t1 * x1
      t475 = t1 * t384
      t476 = t475 * t273
      t478 = t475 * x4
      t480 = t471 ** 2
      t482 = t1 ** 2
      t484 = x1 * t384
      t488 = 0.1D1 / (-0.2D1 + t471)
      t489 = t471 * t488
      t490 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t276)
      t491 = t489 * t490
      t493 = t480 ** 2
      t495 = t387 * x4 * t273 * t493
      t498 = log(-0.4D1 * t98 * t495)
      t499 = t498 * t471
      t500 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t276)
      t501 = t488 * t500
      t507 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t276)
      t511 = t498 ** 2
      t517 = t75 * t22
      t518 = t489 * t500
      t525 = log(-0.4D1 * t178 * t495)
      t530 = t63 * t22
      t537 = -(0.180D3 * t63 * t22 * (t491 - t499 * t501) - t81 * (t489 
     #* t507 - t499 * t488 * t490 + t511 * t471 * t501 / 0.2D1) - t517 *
     # t518) * t122 * t93 / 0.2880D4 - (t81 * (-t491 + t525 * t471 * t50
     #1) + 0.180D3 * t530 * t518) * t122 * t170 / 0.2880D4
      t538 = FJET(XB1, XB2, s, t472 * t473, 0.0D0, t472 * t476, -t472 * 
     #t478, -s * t480 * t482 * t484 * x4, t537)
      t540 = KAPPA2(x1, x2, t150, 0.10D1, z)
      t541 = s * t540
      t542 = t473 * t153
      t544 = t473 * x3
      t547 = t540 ** 2
      t553 = 0.1D1 / (-0.2D1 + t540)
      t554 = t540 * t553
      t555 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t150, 0.10D1)
      t556 = t554 * t555
      t557 = t387 * t153
      t558 = t547 ** 2
      t563 = log(-0.4D1 * t178 * t557 * x4 * t558)
      t565 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t150, 0.10D1)
      t566 = t553 * t565
      t570 = t554 * t565
      t581 = log(-0.4D1 * t152 * t11 * t387 * t153 * t558)
      t582 = t581 * t540
      t588 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, t150, 0.10D1)
      t592 = t581 ** 2
      t603 = -(t81 * (-t556 + t563 * t540 * t566) + 0.180D3 * t530 * t57
     #0) * t122 * t170 / 0.2880D4 - (0.180D3 * t63 * t22 * (t556 - t582 
     #* t566) - t81 * (t554 * t588 - t582 * t553 * t555 + t592 * t540 * 
     #t566 / 0.2D1) - t517 * t570) * t122 * t169 / 0.2880D4
      t604 = FJET(XB1, XB2, s, -t541 * t542, t541 * t544, -t541 * t475, 
     #0.0D0, -s * t547 * t482 * t484 * x3, t603)
      t606 = KAPPA2(x1, x2, t150, t276, z)
      t607 = s * t606
      t612 = t606 ** 2
      t617 = cos(t7)
      t620 = sqrt(x3 * t153 * t323)
      t627 = 0.1D1 / (-0.2D1 + t606)
      t628 = t606 * t627
      t629 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t150, t276)
      t631 = t612 ** 2
      t636 = log(0.4D1 * t178 * t557 * t323 * t631)
      t638 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t150, t276)
      t646 = t81 * (t628 * t629 - t636 * t606 * t627 * t638) - 0.180D3 *
     # t530 * t628 * t638
      t650 = FJET(XB1, XB2, s, -t607 * t542, t607 * t544, t607 * t476, -
     #t607 * t478, s * t612 * t482 * t484 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t617 * t620), -t646 * t122 * t170 / 0.2880D4)
      bggbH2n1e1 = t271 * t270 + t381 * t380 + t469 * t468 + t538 * t537
     # + t604 * t603 - t650 * t646 * t122 * t169 * t93 / 0.2880D4

      end function



      doubleprecision function bggbH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * x4
      t18 = log(0.4D1 * t12 * t15)
      t19 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t23 = lh * t1
      t24 = t5 * t19
      t28 = 0.1D1 / x1
      t30 = 0.1D1 / x4
      t33 = 0.1D1 - x3
      t34 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, 0.10D1)
      t35 = t19 - t34
      t37 = 0.1D1 / x3
      t39 = t28 * t37 * t30
      t42 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, 0.10D1)
      t43 = x3 * t8
      t44 = t11 * t14
      t45 = -t33
      t49 = log(-0.4D1 * t43 * t44 * t45)
      t53 = log(0.4D1 * t43 * t44)
      t57 = t5 * t35
      t59 = 0.180D3 * t23 * t57
      t64 = t12 * t14
      t66 = log(0.4D1 * t64)
      t72 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t74 = t66 ** 2
      t79 = lh ** 2
      t80 = 0.180D3 * t79
      t81 = 0.3141592653589793D1 ** 2
      t82 = 0.30D2 * t81
      t83 = t80 - t82
      t84 = t83 * t1
      t85 = t84 * t24
      t91 = log(0.4D1 * t44 * x4)
      t98 = t91 ** 2
      t106 = x3 * t11
      t109 = log(0.4D1 * t106 * t15)
      t111 = t14 * t45
      t115 = log(-0.4D1 * t106 * t111 * x4)
      t123 = t106 * t14
      t125 = log(0.4D1 * t123)
      t129 = log(-0.4D1 * t106 * t111)
      t135 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, 0.10D1)
      t137 = t129 ** 2
      t141 = t125 ** 2
      t152 = log(0.4D1 * t44)
      t161 = t152 ** 2
      t181 = -(t6 * (t7 - t18 * t19) - 0.180D3 * t23 * t24) * t28 * t30 
     #/ 0.2880D4 - t6 * t35 * t39 / 0.2880D4 - (t6 * (-t42 + t49 * t34 +
     # t7 - t53 * t19) - t59) * t28 * t37 / 0.2880D4 - (-0.180D3 * t23 *
     # t5 * (t7 - t66 * t19) + t6 * (t72 - t66 * t7 + t74 * t19 / 0.2D1)
     # + t85) * t28 / 0.2880D4 + (-0.180D3 * t23 * t5 * (-t7 + t91 * t19
     #) + t6 * (-t72 + t91 * t7 - t98 * t19 / 0.2D1) - t85) * t30 / 0.57
     #60D4 - (t6 * (t7 - t109 * t19 - t42 + t115 * t34) - t59) * t37 * t
     #30 / 0.5760D4 - (-0.180D3 * t23 * t5 * (t7 - t125 * t19 - t42 + t1
     #29 * t34) + t6 * (-t135 + t129 * t42 - t137 * t34 / 0.2D1 + t72 - 
     #t125 * t7 + t141 * t19 / 0.2D1) + t84 * t57) * t37 / 0.5760D4 - (-
     #0.180D3 * lh - 0.90D2 * t152) * t1 * t5 * t72 / 0.5760D4 - (t80 - 
     #t82 + 0.180D3 * t152 * lh + 0.45D2 * t161) * t1 * t5 * t7 / 0.5760
     #D4 - (-0.2884936567583026D3 - 0.120D3 * t79 * lh + 0.60D2 * lh * t
     #81 - t152 * t83 - 0.90D2 * t161 * lh - 0.15D2 * t161 * t152) * t1 
     #* t24 / 0.5760D4
      t182 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t181)
      t184 = -0.1D1 + x4
      t187 = -t184
      t188 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t187)
      t189 = t15 * t184
      t192 = log(-0.4D1 * t12 * t189)
      t193 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t187)
      t197 = t5 * t193
      t204 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, t187)
      t205 = t204 - t193
      t209 = x4 * t184
      t212 = log(-0.4D1 * t44 * t209)
      t218 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t187)
      t220 = t212 ** 2
      t229 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, t187)
      t233 = log(0.4D1 * t123 * t209 * t45)
      t237 = log(-0.4D1 * t106 * t189)
      t248 = -(t6 * (-t188 + t192 * t193) + 0.180D3 * t23 * t197) * t28 
     #* t30 / 0.2880D4 - t6 * t205 * t39 / 0.2880D4 + (-0.180D3 * t23 * 
     #t5 * (t188 - t212 * t193) + t6 * (t218 - t212 * t188 + t220 * t193
     # / 0.2D1) + t84 * t197) * t30 / 0.5760D4 - (t6 * (t229 - t233 * t2
     #04 - t188 + t237 * t193) - 0.180D3 * t23 * t5 * t205) * t37 * t30 
     #/ 0.5760D4
      t249 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t184, t2 * x4, 0.0D0,
     # t248)
      t252 = -0.1D1 + x1
      t254 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t255 = t252 ** 2
      t256 = t8 * t255
      t260 = log(0.4D1 * t44 * t256 * x4)
      t261 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t265 = t5 * t261
      t267 = 0.180D3 * t23 * t265
      t277 = log(0.4D1 * t106 * t14 * t8 * t255)
      t286 = log(0.4D1 * t44 * t256)
      t292 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t294 = t286 ** 2
      t303 = -(t6 * (-t254 + t260 * t261) + t267) * t28 * t30 / 0.2880D4
     # + t6 * t261 * t39 / 0.2880D4 - (t6 * (-t254 + t277 * t261) + t267
     #) * t28 * t37 / 0.2880D4 - (0.180D3 * t23 * t5 * (t254 - t286 * t2
     #61) - t6 * (t292 - t286 * t254 + t294 * t261 / 0.2D1) - t84 * t265
     #) * t28 / 0.2880D4
      t304 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t252, 0.0D0, 0.0D0,
     # t303)
      t306 = KAPPA2(x1, x2, 0.10D1, t187, z)
      t307 = s * t306
      t308 = t1 * x1
      t310 = t1 * t252
      t311 = t310 * t184
      t313 = t310 * x4
      t315 = t306 ** 2
      t317 = t1 ** 2
      t319 = x1 * t252
      t323 = 0.1D1 / (-0.2D1 + t306)
      t324 = t306 * t323
      t325 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t187)
      t328 = t315 ** 2
      t333 = log(-0.4D1 * t64 * t255 * x4 * t184 * t328)
      t335 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t187)
      t340 = t23 * t5
      t349 = t37 * t30
      t353 = -(-t6 * (t324 * t325 - t333 * t306 * t323 * t335) + 0.180D3
     # * t340 * t324 * t335) * t28 * t30 / 0.2880D4 + t6 * t324 * t335 *
     # t28 * t349 / 0.2880D4
      t354 = FJET(XB1, XB2, s, t307 * t308, 0.0D0, t307 * t311, -t307 * 
     #t313, -s * t315 * t317 * t319 * x4, t353)
      t356 = KAPPA2(x1, x2, t33, 0.10D1, z)
      t357 = s * t356
      t358 = t308 * t45
      t360 = t308 * x3
      t363 = t356 ** 2
      t369 = 0.1D1 / (-0.2D1 + t356)
      t370 = t356 * t369
      t372 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t33, 0.10D1)
      t376 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t33, 0.10D1)
      t380 = t363 ** 2
      t385 = log(-0.4D1 * t43 * t11 * t14 * t255 * t45 * t380)
      t398 = t6 * t370 * t372 * t28 * t349 / 0.2880D4 - (-t6 * (t370 * t
     #376 - t385 * t356 * t369 * t372) + 0.180D3 * t340 * t370 * t372) *
     # t28 * t37 / 0.2880D4
      t399 = FJET(XB1, XB2, s, -t357 * t358, t357 * t360, -t357 * t310, 
     #0.0D0, -s * t363 * t317 * t319 * x3, t398)
      t401 = KAPPA2(x1, x2, t33, t187, z)
      t402 = s * t401
      t407 = t401 ** 2
      t412 = cos(t9)
      t415 = sqrt(x3 * t45 * t209)
      t422 = 0.1D1 / (-0.2D1 + t401)
      t425 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t33, t187)
      t430 = FJET(XB1, XB2, s, -t402 * t358, t402 * t360, t402 * t311, -
     #t402 * t313, s * t407 * t317 * t319 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t412 * t415), -t6 * t401 * t422 * t425 * t28 * t349 / 0.
     #2880D4)
      bggbH2n1e0 = t182 * t181 + t249 * t248 + t304 * t303 + t354 * t353
     # + t399 * t398 - t430 * t3 * t5 * t401 * t422 * t425 * t39 / 0.288
     #0D4

      end function



      doubleprecision function bggbH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t8 = 0.1D1 - x3
      t9 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t8, 0.10D1)
      t10 = t7 - t9
      t11 = 0.1D1 / x3
      t13 = 0.1D1 / x4
      t17 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = x3 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t26 = log(0.4D1 * t21 * t23)
      t28 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t8, 0.10D1)
      t29 = -t8
      t33 = log(-0.4D1 * t21 * t23 * t29)
      t37 = lh * t1
      t44 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t48 = t20 * t23
      t50 = log(0.4D1 * t48)
      t57 = lh ** 2
      t59 = 0.3141592653589793D1 ** 2
      t63 = t50 ** 2
      t67 = t5 * t7
      t70 = x1 ** 2
      t74 = log(0.4D1 * t70 * t20 * t23)
      t79 = 0.180D3 * t37 * t67
      t81 = 0.1D1 / x1
      t94 = log(0.4D1 * t48 * x4)
      t101 = -t6 * t10 * t11 * t13 / 0.5760D4 - (t6 * (t17 - t26 * t7 - 
     #t28 + t33 * t9) - 0.180D3 * t37 * t5 * t10) * t11 / 0.5760D4 - t6 
     #* t44 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t50) * t1 * t5 * t17 
     #/ 0.5760D4 - (0.180D3 * t57 - 0.30D2 * t59 + 0.180D3 * t50 * lh + 
     #0.45D2 * t63) * t1 * t67 / 0.5760D4 - (t6 * (t17 - t74 * t7) - t79
     #) * t81 / 0.2880D4 - t6 * t7 * t81 * t13 / 0.2880D4 - t6 * t10 * t
     #81 * t11 / 0.2880D4 + (t6 * (-t17 + t94 * t7) + t79) * t13 / 0.576
     #0D4
      t102 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t101)
      t104 = -0.1D1 + x4
      t107 = -t104
      t108 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t107)
      t113 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t107)
      t117 = log(-0.4D1 * t48 * x4 * t104)
      t127 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t8, t107)
      t133 = t6 * t108 * t81 * t13 / 0.2880D4 + (t6 * (t113 - t117 * t10
     #8) - 0.180D3 * t37 * t5 * t108) * t13 / 0.5760D4 - t6 * (t127 - t1
     #08) * t11 * t13 / 0.5760D4
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t104, t2 * x4, 0.0D0,
     # t133)
      t137 = -0.1D1 + x1
      t139 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t140 = t137 ** 2
      t144 = log(0.4D1 * t48 * t70 * t140)
      t145 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t154 = t145 * t81
      t160 = -(-t6 * (t139 - t144 * t145) + 0.180D3 * t37 * t5 * t145) *
     # t81 / 0.2880D4 + t6 * t154 * t13 / 0.2880D4 + t6 * t154 * t11 / 0
     #.2880D4
      t161 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t137, 0.0D0, 0.0D0,
     # t160)
      t163 = KAPPA2(x1, x2, 0.10D1, t107, z)
      t164 = s * t163
      t165 = t1 * x1
      t167 = t1 * t137
      t172 = t163 ** 2
      t174 = t1 ** 2
      t176 = x1 * t137
      t182 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t107)
      t185 = 0.1D1 / (-0.2D1 + t163) * t182 * t81 * t13
      t188 = FJET(XB1, XB2, s, t164 * t165, 0.0D0, t164 * t167 * t104, -
     #t164 * t167 * x4, -s * t172 * t174 * t176 * x4, t6 * t163 * t185 /
     # 0.2880D4)
      t194 = KAPPA2(x1, x2, t8, 0.10D1, z)
      t195 = s * t194
      t201 = t194 ** 2
      t209 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t8, 0.10D1)
      t212 = 0.1D1 / (-0.2D1 + t194) * t209 * t81 * t11
      t215 = FJET(XB1, XB2, s, -t195 * t165 * t29, t195 * t165 * x3, -t1
     #95 * t167, 0.0D0, -s * t201 * t174 * t176 * x3, t6 * t194 * t212 /
     # 0.2880D4)
      bggbH2n1em1 = t102 * t101 + t134 * t133 + t161 * t160 + t188 * t3 
     #* t5 * t163 * t185 / 0.2880D4 + t215 * t3 * t5 * t194 * t212 / 0.2
     #880D4

      end function



      doubleprecision function bggbH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t8 = 0.1D1 / x1
      t13 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, 0.1
     #0D1)
      t19 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t24 = sin(x2 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t26 = z ** 2
      t30 = log(0.4D1 * t25 / t26)
      t37 = 0.1D1 / x4
      t41 = -t6 * t7 * t8 / 0.2880D4 - t6 * (t7 - t13) / x3 / 0.5760D4 -
     # t6 * t19 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t30) * t1 * t5 * 
     #t7 / 0.5760D4 - t6 * t7 * t37 / 0.5760D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t47 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t51 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, t6 * t47 * t8 / 0.2880D4)
      t57 = -0.1D1 + x4
      t61 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, -t57)
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t57, t2 * x4, 0.0D0, t
     #6 * t61 * t37 / 0.5760D4)
      bggbH2n1em2 = t42 * t41 + t51 * t3 * t5 * t47 * t8 / 0.2880D4 + t6
     #5 * t3 * t5 * t61 * t37 / 0.5760D4

      end function



      doubleprecision function bggbH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, -t3 * 
     #t5 * t7 / 0.5760D4)
      bggbH2n1em3 = -t10 * t3 * t5 * t7 / 0.5760D4

      end function



      doubleprecision function bggbH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      bggbH2n1em4 = 0.0D0

      end function


      doubleprecision function bggbH2n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t14 = log(0.4D1 * t12)
      t17 = t14 ** 2
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t43 = t5 ** 2
      t44 = t3 ** 2
      t49 = -0.2884936567583026D3 - t28 + t30
      t51 = t17 ** 2
      t59 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t60 = t22 * t59
      t63 = lh * t1
      t64 = t12 * x4
      t66 = log(0.4D1 * t64)
      t68 = t66 ** 2
      t75 = t31 * t1
      t81 = 0.90D2 * t1 * t22
      t90 = t49 * t1
      t91 = t90 * t60
      t93 = 0.1D1 / x4
      t96 = x1 ** 2
      t97 = t96 * t9
      t98 = t97 * t11
      t100 = log(0.4D1 * t98)
      t102 = t100 ** 2
      t122 = 0.1D1 / x1
      t125 = t11 * x4
      t128 = log(0.4D1 * t97 * t125)
      t135 = t128 ** 2
      t145 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t146 = x3 * t96
      t147 = t146 * t9
      t148 = -0.1D1 + x3
      t149 = t11 * t148
      t150 = t149 * x4
      t153 = log(-0.4D1 * t147 * t150)
      t154 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t158 = log(0.4D1 * t146 * t64)
      t162 = -t154 + t59
      t163 = t22 * t162
      t168 = 0.1D1 / x3
      t169 = t168 * t93
      t175 = log(-0.4D1 * t146 * t12 * t148)
      t177 = t146 * t12
      t179 = log(0.4D1 * t177)
      t186 = t179 ** 2
      t189 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t191 = t175 ** 2
      t201 = x3 * t9
      t202 = t201 * t11
      t204 = log(0.4D1 * t202)
      t206 = t204 ** 2
      t211 = log(-0.4D1 * t201 * t149)
      t213 = t211 ** 2
      t245 = log(0.4D1 * t201 * t125)
      t249 = log(-0.4D1 * t201 * t150)
      t256 = t249 ** 2
      t260 = t245 ** 2
      t272 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t1 * t22 *
     # t23 / 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 -
     # 0.90D2 * t17 * lh - 0.15D2 * t35) * t1 * t22 * t39 / 0.5760D4 - (
     #t43 + 0.60D2 * t44 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 
     #- t14 * t49 + 0.15D2 / 0.4D1 * t51 + t17 * t31 / 0.2D1 + 0.30D2 * 
     #t35 * lh) * t1 * t60 / 0.5760D4 + (-0.180D3 * t63 * t22 * (-t23 + 
     #t66 * t39 - t68 * t59 / 0.2D1) + t75 * t22 * (-t39 + t66 * t59) + 
     #t81 * (t66 * t23 - t68 * t39 / 0.2D1 + t68 * t66 * t59 / 0.6D1) - 
     #t91) * t93 / 0.5760D4 - (-0.180D3 * t63 * t22 * (t23 - t100 * t39 
     #+ t102 * t59 / 0.2D1) + t75 * t22 * (t39 - t100 * t59) + t81 * (-t
     #100 * t23 + t102 * t39 / 0.2D1 - t102 * t100 * t59 / 0.6D1) + t91)
     # * t122 / 0.2880D4 - (-0.180D3 * t63 * t22 * (t39 - t128 * t59) + 
     #t81 * (t23 - t128 * t39 + t135 * t59 / 0.2D1) + t75 * t60) * t122 
     #* t93 / 0.2880D4 - (t81 * (-t145 + t153 * t154 + t39 - t158 * t59)
     # - 0.180D3 * t63 * t163) * t122 * t169 / 0.2880D4 - (-0.180D3 * t6
     #3 * t22 * (-t145 + t175 * t154 + t39 - t179 * t59) + t81 * (t23 - 
     #t179 * t39 + t186 * t59 / 0.2D1 - t189 + t175 * t145 - t191 * t154
     # / 0.2D1) + t75 * t163) * t122 * t168 / 0.2880D4 - (-0.180D3 * t63
     # * t22 * (t23 - t204 * t39 + t206 * t59 / 0.2D1 - t189 + t211 * t1
     #45 - t213 * t154 / 0.2D1) + t75 * t22 * (-t145 + t211 * t154 + t39
     # - t204 * t59) + t81 * (-t204 * t23 + t206 * t39 / 0.2D1 - t206 * 
     #t204 * t59 / 0.6D1 + t211 * t189 - t213 * t145 / 0.2D1 + t213 * t2
     #11 * t154 / 0.6D1) + t90 * t163) * t168 / 0.5760D4 + (-0.180D3 * t
     #63 * t22 * (-t39 + t245 * t59 + t145 - t249 * t154) + t81 * (t189 
     #- t249 * t145 + t256 * t154 / 0.2D1 - t23 + t245 * t39 - t260 * t5
     #9 / 0.2D1) - t75 * t22 * t162) * t168 * t93 / 0.5760D4
      t273 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t272)
      t275 = -0.1D1 + x4
      t278 = -t275
      t279 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t278)
      t280 = t125 * t275
      t283 = log(-0.4D1 * t97 * t280)
      t284 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t278)
      t290 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t278)
      t292 = t283 ** 2
      t297 = t22 * t284
      t305 = log(-0.4D1 * t147 * t280)
      t307 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t278)
      t312 = log(0.4D1 * t147 * t125 * t275 * t148)
      t313 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t278)
      t317 = -t284 + t313
      t327 = log(-0.4D1 * t201 * t280)
      t329 = x4 * t275
      t333 = log(0.4D1 * t202 * t329 * t148)
      t339 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t278)
      t341 = t333 ** 2
      t345 = t327 ** 2
      t359 = log(-0.4D1 * t12 * t329)
      t361 = t359 ** 2
      t384 = -(-0.180D3 * t63 * t22 * (-t279 + t283 * t284) + t81 * (-t2
     #90 + t283 * t279 - t292 * t284 / 0.2D1) - t75 * t297) * t122 * t93
     # / 0.2880D4 - (t81 * (-t279 + t305 * t284 + t307 - t312 * t313) - 
     #0.180D3 * t63 * t22 * t317) * t122 * t169 / 0.2880D4 + (-0.180D3 *
     # t63 * t22 * (t279 - t327 * t284 - t307 + t333 * t313) + t81 * (-t
     #339 + t333 * t307 - t341 * t313 / 0.2D1 + t290 - t327 * t279 + t34
     #5 * t284 / 0.2D1) - t75 * t22 * t317) * t168 * t93 / 0.5760D4 + (-
     #0.180D3 * t63 * t22 * (t290 - t359 * t279 + t361 * t284 / 0.2D1) +
     # t75 * t22 * (t279 - t359 * t284) + t81 * (-t359 * t290 + t361 * t
     #279 / 0.2D1 - t361 * t359 * t284 / 0.6D1) + t90 * t297) * t93 / 0.
     #5760D4
      t385 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t275, t2 * x4, 0.0D0,
     # t384)
      t387 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t388 = s * t387
      t389 = t1 * x1
      t391 = -0.1D1 + x1
      t392 = t1 * t391
      t394 = t387 ** 2
      t396 = t1 ** 2
      t401 = 0.1D1 / (-0.2D1 + t387)
      t402 = t387 * t401
      t403 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t404 = t402 * t403
      t405 = t391 ** 2
      t406 = t96 * t405
      t407 = t394 ** 2
      t408 = t406 * t407
      t411 = log(0.4D1 * t12 * t408)
      t412 = t411 * t387
      t413 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t414 = t401 * t413
      t416 = t411 ** 2
      t417 = t416 * t387
      t418 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t419 = t401 * t418
      t426 = t402 * t413
      t442 = t402 * t418
      t446 = t405 * x4
      t450 = log(0.4D1 * t98 * t446 * t407)
      t451 = t450 * t387
      t458 = t450 ** 2
      t464 = t75 * t22
      t465 = t464 * t442
      t473 = log(0.4D1 * t202 * t406 * x4 * t407)
      t478 = t63 * t22
      t486 = log(0.4D1 * t202 * t408)
      t487 = t486 * t387
      t494 = t486 ** 2
      t504 = -(-0.180D3 * t63 * t22 * (t404 - t412 * t414 + t417 * t419 
     #/ 0.2D1) + t75 * t22 * (t426 - t412 * t419) + t81 * (-t412 * t401 
     #* t403 + t417 * t414 / 0.2D1 - t416 * t411 * t387 * t419 / 0.6D1) 
     #+ t90 * t22 * t442) * t122 / 0.2880D4 - (-0.180D3 * t63 * t22 * (t
     #426 - t451 * t419) + t81 * (t404 - t451 * t414 + t458 * t387 * t41
     #9 / 0.2D1) + t465) * t122 * t93 / 0.2880D4 - (t81 * (t426 - t473 *
     # t387 * t419) - 0.180D3 * t478 * t442) * t122 * t169 / 0.2880D4 - 
     #(-0.180D3 * t63 * t22 * (t426 - t487 * t419) + t81 * (t404 - t487 
     #* t414 + t494 * t387 * t419 / 0.2D1) + t465) * t122 * t168 / 0.288
     #0D4
      t505 = FJET(XB1, XB2, s, 0.0D0, t388 * t389, -t388 * t392, 0.0D0, 
     #-s * t394 * t396 * x1 * t391, t504)
      t507 = KAPPA2(x1, x2, 0.0D0, t278, z)
      t508 = s * t507
      t510 = t392 * t275
      t512 = t392 * x4
      t514 = t507 ** 2
      t517 = x1 * t391
      t521 = 0.1D1 / (-0.2D1 + t507)
      t522 = t507 * t521
      t523 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t278)
      t524 = t522 * t523
      t525 = t514 ** 2
      t527 = t446 * t275 * t525
      t530 = log(-0.4D1 * t98 * t527)
      t531 = t530 * t507
      t532 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t278)
      t533 = t521 * t532
      t539 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t278)
      t543 = t530 ** 2
      t549 = t522 * t532
      t556 = log(-0.4D1 * t177 * t527)
      t567 = -(0.180D3 * t63 * t22 * (t524 - t531 * t533) - t81 * (t522 
     #* t539 - t531 * t521 * t523 + t543 * t507 * t533 / 0.2D1) - t464 *
     # t549) * t122 * t93 / 0.2880D4 - (t81 * (-t524 + t556 * t507 * t53
     #3) + 0.180D3 * t478 * t549) * t122 * t169 / 0.2880D4
      t568 = FJET(XB1, XB2, s, 0.0D0, t508 * t389, t508 * t510, -t508 * 
     #t512, s * t514 * t396 * t517 * t275, t567)
      t570 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t571 = s * t570
      t572 = t389 * x3
      t574 = t389 * t148
      t577 = t570 ** 2
      t583 = 0.1D1 / (-0.2D1 + t570)
      t584 = t570 * t583
      t585 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t586 = t584 * t585
      t587 = t405 * t148
      t588 = t577 ** 2
      t593 = log(-0.4D1 * t177 * t587 * x4 * t588)
      t595 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t596 = t583 * t595
      t600 = t584 * t595
      t610 = log(-0.4D1 * t202 * t406 * t148 * t588)
      t611 = t610 * t570
      t617 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t621 = t610 ** 2
      t632 = -(t81 * (-t586 + t593 * t570 * t596) + 0.180D3 * t478 * t60
     #0) * t122 * t169 / 0.2880D4 - (0.180D3 * t63 * t22 * (t586 - t611 
     #* t596) - t81 * (t584 * t617 - t611 * t583 * t585 + t621 * t570 * 
     #t596 / 0.2D1) - t464 * t600) * t122 * t168 / 0.2880D4
      t633 = FJET(XB1, XB2, s, t571 * t572, -t571 * t574, -t571 * t392, 
     #0.0D0, s * t577 * t396 * t517 * t148, t632)
      t635 = KAPPA2(x1, x2, x3, t278, z)
      t636 = s * t635
      t641 = t635 ** 2
      t646 = cos(t7)
      t649 = sqrt(x3 * t148 * t329)
      t656 = 0.1D1 / (-0.2D1 + t635)
      t657 = t635 * t656
      t658 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, t278)
      t660 = t641 ** 2
      t665 = log(0.4D1 * t177 * t587 * t329 * t660)
      t667 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t278)
      t675 = t81 * (t657 * t658 - t665 * t635 * t656 * t667) - 0.180D3 *
     # t478 * t657 * t667
      t679 = FJET(XB1, XB2, s, t636 * t572, -t636 * t574, t636 * t510, -
     #t636 * t512, s * t641 * t396 * t517 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t646 * t649), -t675 * t122 * t169 / 0.2880D4)
      bggbH2n2e1 = t273 * t272 + t385 * t384 + t505 * t504 + t568 * t567
     # + t633 * t632 - t679 * t675 * t122 * t168 * t93 / 0.2880D4

      end function



      doubleprecision function bggbH2n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * x4
      t18 = log(0.4D1 * t12 * t15)
      t19 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t23 = lh * t1
      t24 = t5 * t19
      t28 = 0.1D1 / x1
      t30 = 0.1D1 / x4
      t33 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t34 = -t33 + t19
      t36 = 0.1D1 / x3
      t38 = t28 * t36 * t30
      t41 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t42 = x3 * t8
      t43 = t11 * t14
      t44 = -0.1D1 + x3
      t48 = log(-0.4D1 * t42 * t43 * t44)
      t52 = log(0.4D1 * t42 * t43)
      t56 = t5 * t34
      t63 = t12 * t14
      t65 = log(0.4D1 * t63)
      t71 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t73 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t1
      t84 = t83 * t24
      t90 = log(0.4D1 * t43 * x4)
      t97 = t90 ** 2
      t105 = x3 * t11
      t108 = log(0.4D1 * t105 * t15)
      t110 = t14 * t44
      t114 = log(-0.4D1 * t105 * t110 * x4)
      t128 = log(-0.4D1 * t105 * t110)
      t130 = t105 * t14
      t132 = log(0.4D1 * t130)
      t139 = t132 ** 2
      t142 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t144 = t128 ** 2
      t155 = log(0.4D1 * t43)
      t164 = t155 ** 2
      t184 = -(t6 * (t7 - t18 * t19) - 0.180D3 * t23 * t24) * t28 * t30 
     #/ 0.2880D4 - t6 * t34 * t38 / 0.2880D4 - (t6 * (-t41 + t48 * t33 +
     # t7 - t52 * t19) - 0.180D3 * t23 * t56) * t28 * t36 / 0.2880D4 - (
     #-0.180D3 * t23 * t5 * (t7 - t65 * t19) + t6 * (t71 - t65 * t7 + t7
     #3 * t19 / 0.2D1) + t84) * t28 / 0.2880D4 + (-0.180D3 * t23 * t5 * 
     #(-t7 + t90 * t19) + t6 * (-t71 + t90 * t7 - t97 * t19 / 0.2D1) - t
     #84) * t30 / 0.5760D4 + (t6 * (-t7 + t108 * t19 + t41 - t114 * t33)
     # + 0.180D3 * t23 * t5 * t34) * t36 * t30 / 0.5760D4 - (-0.180D3 * 
     #t23 * t5 * (-t41 + t128 * t33 + t7 - t132 * t19) + t6 * (t71 - t13
     #2 * t7 + t139 * t19 / 0.2D1 - t142 + t128 * t41 - t144 * t33 / 0.2
     #D1) + t83 * t56) * t36 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t155
     #) * t1 * t5 * t71 / 0.5760D4 - (t79 - t81 + 0.180D3 * t155 * lh + 
     #0.45D2 * t164) * t1 * t5 * t7 / 0.5760D4 - (-0.2884936567583026D3 
     #- 0.120D3 * t78 * lh + 0.60D2 * lh * t80 - t155 * t82 - 0.90D2 * t
     #164 * lh - 0.15D2 * t164 * t155) * t1 * t24 / 0.5760D4
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t184)
      t187 = -0.1D1 + x4
      t190 = -t187
      t191 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t190)
      t192 = t15 * t187
      t195 = log(-0.4D1 * t12 * t192)
      t196 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t190)
      t200 = t5 * t196
      t207 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t190)
      t208 = -t196 + t207
      t212 = x4 * t187
      t215 = log(-0.4D1 * t43 * t212)
      t221 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t190)
      t223 = t215 ** 2
      t234 = log(-0.4D1 * t105 * t192)
      t236 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t190)
      t240 = log(0.4D1 * t130 * t212 * t44)
      t252 = -(t6 * (-t191 + t195 * t196) + 0.180D3 * t23 * t200) * t28 
     #* t30 / 0.2880D4 - t6 * t208 * t38 / 0.2880D4 + (-0.180D3 * t23 * 
     #t5 * (t191 - t215 * t196) + t6 * (t221 - t215 * t191 + t223 * t196
     # / 0.2D1) + t83 * t200) * t30 / 0.5760D4 + (t6 * (t191 - t234 * t1
     #96 - t236 + t240 * t207) + 0.180D3 * t23 * t5 * t208) * t36 * t30 
     #/ 0.5760D4
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t187, t2 * x4, 0.0D0,
     # t252)
      t255 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t256 = s * t255
      t257 = t1 * x1
      t259 = -0.1D1 + x1
      t260 = t1 * t259
      t262 = t255 ** 2
      t264 = t1 ** 2
      t269 = 0.1D1 / (-0.2D1 + t255)
      t270 = t255 * t269
      t271 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t272 = t270 * t271
      t273 = t259 ** 2
      t274 = t273 * x4
      t275 = t262 ** 2
      t279 = log(0.4D1 * t63 * t274 * t275)
      t281 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t282 = t269 * t281
      t286 = t23 * t5
      t287 = t270 * t281
      t289 = 0.180D3 * t286 * t287
      t295 = t36 * t30
      t298 = t8 * t273
      t299 = t298 * t275
      t302 = log(0.4D1 * t130 * t299)
      t312 = log(0.4D1 * t43 * t299)
      t313 = t312 * t255
      t319 = bggbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t323 = t312 ** 2
      t334 = -(t6 * (t272 - t279 * t255 * t282) - t289) * t28 * t30 / 0.
     #2880D4 - t6 * t270 * t281 * t28 * t295 / 0.2880D4 - (t6 * (t272 - 
     #t302 * t255 * t282) - t289) * t28 * t36 / 0.2880D4 - (-0.180D3 * t
     #23 * t5 * (t272 - t313 * t282) + t6 * (t270 * t319 - t313 * t269 *
     # t271 + t323 * t255 * t282 / 0.2D1) + t83 * t5 * t287) * t28 / 0.2
     #880D4
      t335 = FJET(XB1, XB2, s, 0.0D0, t256 * t257, -t256 * t260, 0.0D0, 
     #-s * t262 * t264 * x1 * t259, t334)
      t337 = KAPPA2(x1, x2, 0.0D0, t190, z)
      t338 = s * t337
      t340 = t260 * t187
      t342 = t260 * x4
      t344 = t337 ** 2
      t347 = x1 * t259
      t351 = 0.1D1 / (-0.2D1 + t337)
      t352 = t337 * t351
      t353 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t190)
      t355 = t344 ** 2
      t360 = log(-0.4D1 * t63 * t274 * t187 * t355)
      t362 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t190)
      t378 = -(-t6 * (t352 * t353 - t360 * t337 * t351 * t362) + 0.180D3
     # * t286 * t352 * t362) * t28 * t30 / 0.2880D4 + t6 * t352 * t362 *
     # t28 * t295 / 0.2880D4
      t379 = FJET(XB1, XB2, s, 0.0D0, t338 * t257, t338 * t340, -t338 * 
     #t342, s * t344 * t264 * t347 * t187, t378)
      t381 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t382 = s * t381
      t383 = t257 * x3
      t385 = t257 * t44
      t388 = t381 ** 2
      t394 = 0.1D1 / (-0.2D1 + t381)
      t395 = t381 * t394
      t397 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t401 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t403 = t388 ** 2
      t408 = log(-0.4D1 * t130 * t298 * t44 * t403)
      t421 = t6 * t395 * t397 * t28 * t295 / 0.2880D4 - (-t6 * (t395 * t
     #401 - t408 * t381 * t394 * t397) + 0.180D3 * t286 * t395 * t397) *
     # t28 * t36 / 0.2880D4
      t422 = FJET(XB1, XB2, s, t382 * t383, -t382 * t385, -t382 * t260, 
     #0.0D0, s * t388 * t264 * t347 * t44, t421)
      t424 = KAPPA2(x1, x2, x3, t190, z)
      t425 = s * t424
      t430 = t424 ** 2
      t435 = cos(t9)
      t438 = sqrt(x3 * t44 * t212)
      t445 = 0.1D1 / (-0.2D1 + t424)
      t448 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t190)
      t453 = FJET(XB1, XB2, s, t425 * t383, -t425 * t385, t425 * t340, -
     #t425 * t342, s * t430 * t264 * t347 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t435 * t438), -t6 * t424 * t445 * t448 * t28 * t
     #295 / 0.2880D4)
      bggbH2n2e0 = t185 * t184 + t253 * t252 + t335 * t334 + t379 * t378
     # + t422 * t421 - t453 * t3 * t5 * t424 * t445 * t448 * t38 / 0.288
     #0D4

      end function



      doubleprecision function bggbH2n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t8 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t9 = -t7 + t8
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x4
      t16 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t18 = sin(x2 * 0.3141592653589793D1)
      t19 = t18 ** 2
      t20 = x3 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t23 = -0.1D1 + x3
      t27 = log(-0.4D1 * t20 * t22 * t23)
      t29 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t32 = log(0.4D1 * t20 * t22)
      t36 = lh * t1
      t37 = -t9
      t44 = bggbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t48 = t19 * t22
      t50 = log(0.4D1 * t48)
      t57 = lh ** 2
      t59 = 0.3141592653589793D1 ** 2
      t63 = t50 ** 2
      t67 = t5 * t7
      t70 = x1 ** 2
      t74 = log(0.4D1 * t70 * t19 * t22)
      t79 = 0.180D3 * t36 * t67
      t81 = 0.1D1 / x1
      t94 = log(0.4D1 * t48 * x4)
      t101 = t6 * t9 * t10 * t12 / 0.5760D4 - (t6 * (-t16 + t27 * t8 + t
     #29 - t32 * t7) - 0.180D3 * t36 * t5 * t37) * t10 / 0.5760D4 - t6 *
     # t44 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t50) * t1 * t5 * t29 /
     # 0.5760D4 - (0.180D3 * t57 - 0.30D2 * t59 + 0.180D3 * t50 * lh + 0
     #.45D2 * t63) * t1 * t67 / 0.5760D4 - (t6 * (t29 - t74 * t7) - t79)
     # * t81 / 0.2880D4 - t6 * t7 * t81 * t12 / 0.2880D4 - t6 * t37 * t8
     #1 * t10 / 0.2880D4 + (t6 * (-t29 + t94 * t7) + t79) * t12 / 0.5760
     #D4
      t102 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t101)
      t104 = -0.1D1 + x4
      t107 = -t104
      t108 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t107)
      t113 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t107)
      t117 = log(-0.4D1 * t48 * x4 * t104)
      t127 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t107)
      t133 = t6 * t108 * t81 * t12 / 0.2880D4 + (t6 * (t113 - t117 * t10
     #8) - 0.180D3 * t36 * t5 * t108) * t12 / 0.5760D4 + t6 * (-t127 + t
     #108) * t10 * t12 / 0.5760D4
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t104, t2 * x4, 0.0D0,
     # t133)
      t136 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t137 = s * t136
      t138 = t1 * x1
      t140 = -0.1D1 + x1
      t141 = t1 * t140
      t143 = t136 ** 2
      t145 = t1 ** 2
      t150 = 0.1D1 / (-0.2D1 + t136)
      t151 = t136 * t150
      t152 = bggbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t154 = t140 ** 2
      t156 = t143 ** 2
      t160 = log(0.4D1 * t48 * t70 * t154 * t156)
      t162 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t163 = t150 * t162
      t173 = t6 * t136
      t174 = t81 * t12
      t177 = t81 * t10
      t181 = -(t6 * (t151 * t152 - t160 * t136 * t163) - 0.180D3 * t36 *
     # t5 * t151 * t162) * t81 / 0.2880D4 - t173 * t163 * t174 / 0.2880D
     #4 - t173 * t163 * t177 / 0.2880D4
      t182 = FJET(XB1, XB2, s, 0.0D0, t137 * t138, -t137 * t141, 0.0D0, 
     #-s * t143 * t145 * x1 * t140, t181)
      t184 = KAPPA2(x1, x2, 0.0D0, t107, z)
      t185 = s * t184
      t191 = t184 ** 2
      t194 = x1 * t140
      t200 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t107)
      t202 = 0.1D1 / (-0.2D1 + t184) * t200 * t174
      t205 = FJET(XB1, XB2, s, 0.0D0, t185 * t138, t185 * t141 * t104, -
     #t185 * t141 * x4, s * t191 * t145 * t194 * t104, t6 * t184 * t202 
     #/ 0.2880D4)
      t211 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t212 = s * t211
      t218 = t211 ** 2
      t226 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t228 = 0.1D1 / (-0.2D1 + t211) * t226 * t177
      t231 = FJET(XB1, XB2, s, t212 * t138 * x3, -t212 * t138 * t23, -t2
     #12 * t141, 0.0D0, s * t218 * t145 * t194 * t23, t6 * t211 * t228 /
     # 0.2880D4)
      bggbH2n2em1 = t102 * t101 + t134 * t133 + t182 * t181 + t205 * t3 
     #* t5 * t184 * t202 / 0.2880D4 + t231 * t3 * t5 * t211 * t228 / 0.2
     #880D4

      end function



      doubleprecision function bggbH2n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t8 = 0.1D1 / x1
      t12 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t18 = bggbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t23 = sin(x2 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t25 = z ** 2
      t29 = log(0.4D1 * t24 / t25)
      t36 = 0.1D1 / x4
      t40 = -t6 * t7 * t8 / 0.2880D4 - t6 * (-t12 + t7) / x3 / 0.5760D4 
     #- t6 * t18 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t29) * t1 * t5 *
     # t7 / 0.5760D4 - t6 * t7 * t36 / 0.5760D4
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t40)
      t43 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t44 = s * t43
      t47 = -0.1D1 + x1
      t50 = t43 ** 2
      t52 = t1 ** 2
      t58 = 0.1D1 / (-0.2D1 + t43)
      t59 = bggbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t64 = FJET(XB1, XB2, s, 0.0D0, t44 * t1 * x1, -t44 * t1 * t47, 0.0
     #D0, -s * t50 * t52 * x1 * t47, -t6 * t43 * t58 * t59 * t8 / 0.2880
     #D4)
      t72 = -0.1D1 + x4
      t76 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, -t72)
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t72, t2 * x4, 0.0D0, t
     #6 * t76 * t36 / 0.5760D4)
      bggbH2n2em2 = t41 * t40 - t64 * t3 * t5 * t43 * t58 * t59 * t8 / 0
     #.2880D4 + t80 * t3 * t5 * t76 * t36 / 0.5760D4

      end function



      doubleprecision function bggbH2n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      t1 = -0.1D1 + z
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = bggbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, -t3 * 
     #t5 * t7 / 0.5760D4)
      bggbH2n2em3 = -t10 * t3 * t5 * t7 / 0.5760D4

      end function



      doubleprecision function bggbH2n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH21J1
      doubleprecision bggbH21J2
      doubleprecision bggbH21J3
      bggbH2n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bggbH21J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = t10 ** 2
      t12 = s ** 2
      t13 = t12 * s
      t14 = t11 * t13
      t15 = z ** 2
      t20 = t1 ** 2
      t21 = t20 * t1
      t22 = t13 * t21
      t23 = t3 ** 2
      t24 = t23 * t3
      t25 = x1 ** 2
      t26 = t24 * t25
      t28 = x3 * t11
      t29 = 0.1D1 - x1
      t33 = cos(x2 * 0.3141592653589793D1)
      t35 = 0.1D1 - x4
      t38 = sqrt(x3 * t7 * x4 * t35)
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t42 = t29 * t41
      t47 = t14 * t21 * t24
      t48 = t25 * t7
      t52 = t20 ** 2
      t53 = t52 * t1
      t54 = t23 ** 2
      t55 = t54 * t3
      t58 = t25 * x1
      t60 = t29 ** 2
      t61 = t41 ** 2
      t62 = t60 * t61
      t66 = z * t29 * t41
      t70 = t12 ** 2
      t71 = t10 * t70
      t75 = t7 * t29 * t41
      t79 = t70 * t21
      t80 = t79 * t26
      t81 = x3 * t10
      t88 = t25 ** 2
      t95 = t70 * t52
      t96 = t54 * t58
      t104 = t70 * t53
      t107 = t7 ** 2
      t112 = t13 * t52
      t113 = t54 * t60
      t114 = t112 * t113
      t115 = t35 * t11
      t123 = t23 * x1 * t42
      t129 = t70 * s * z * t20 * t23
      t130 = t29 * t35
      t131 = x1 * x3
      t135 = t35 * t25
      t140 = -0.18D2 * t14 * t15 + 0.18D2 * t14 * z + 0.36D2 * t22 * t26
     # * t28 * t42 + 0.21D2 * t47 * t48 * t42 + t14 * t53 * t55 * t58 * 
     #t7 * t62 - 0.3D1 * t47 * t48 * t66 + 0.8D1 * t71 * z * t21 * t26 *
     # t75 - 0.18D2 * t80 * t81 * t66 - t70 * t52 * t20 * t54 * t23 * t8
     #8 * t81 * t7 * t60 * t61 + 0.11D2 * t95 * t96 * t81 * t75 - 0.18D2
     # * t80 * t81 * t42 - t104 * t55 * t88 * t81 * t107 * t29 * t41 - 0
     #.3D1 * t114 * t115 * t48 * t41 + 0.18D2 * t14 * z * t20 * t123 - 0
     #.36D2 * t129 * t130 * t131 - 0.18D2 * t114 * t135 * t28 * t41
      t141 = t24 * t60
      t143 = t35 ** 2
      t144 = t143 * x1
      t148 = t70 * t20
      t149 = t23 * t29
      t150 = t148 * t149
      t151 = t35 * x1
      t156 = t22 * t24 * t29
      t161 = t13 * t20
      t162 = t23 * t25
      t164 = t7 * z
      t168 = t148 * t162
      t169 = t7 * t15
      t173 = t24 * t58
      t175 = t107 * z
      t179 = x3 ** 2
      t184 = t22 * t141
      t206 = t10 * t7
      t210 = t14 * t1
      t216 = x1 * t7
      t217 = t1 * t3 * t216
      t228 = 0.18D2 * t79 * t141 * t144 * t81 - 0.36D2 * t150 * t151 * t
     #81 + 0.6D1 * t156 * t115 * t25 * t107 + 0.12D2 * t161 * t162 * t28
     # * t164 - 0.4D1 * t168 * t81 * t169 + 0.3D1 * t79 * t173 * t81 * t
     #175 + 0.36D2 * t156 * t135 * t179 * t11 + 0.18D2 * t184 * t115 * x
     #1 * t41 - 0.6D1 * t112 * t96 * t28 * t75 - 0.3D1 * t14 * t52 * t54
     # * t58 * t107 * t42 + 0.9D1 * t104 * t55 * t58 * t81 * t62 + 0.3D1
     # * t95 * t96 * x3 * t206 * t66 - 0.16D2 * t210 * t4 * t164 - 0.8D1
     # * t71 * z * t217 - 0.9D1 * t14 * t52 * t54 * t25 * t62 - 0.16D2 *
     # t71 * t15 * t217
      t230 = t14 * t20
      t234 = t13 * t1 * t3
      t235 = t11 * z
      t240 = t58 * x3
      t249 = t25 * x3
      t254 = t70 * t1 * t3
      t264 = t107 * t7
      t275 = t22 * t24
      t277 = t11 * t7
      t285 = t161 * t23
      t289 = t161 * t149
      t299 = -0.18D2 * t230 * t123 - 0.36D2 * t234 * t131 * t235 + 0.11D
     #2 * t79 * t24 * t240 * t10 * t107 - 0.18D2 * t234 * t130 * t235 - 
     #0.19D2 * t148 * t23 * t249 * t206 + 0.18D2 * t254 * t131 * t10 * z
     # + 0.4D1 * t210 * t4 * t169 - t95 * t54 * t88 * x3 * t10 * t264 + 
     #0.6D1 * t230 * t162 * t175 + 0.18D2 * t254 * t131 * t10 * t15 + 0.
     #6D1 * t275 * t58 * t179 * t277 + 0.6D1 * t275 * t240 * t11 * t107 
     #- 0.14D2 * t285 * t249 * t277 - 0.24D2 * t289 * t115 * t216 + 0.18
     #D2 * t95 * t113 * t135 * t81 * t41 - 0.9D1 * t14
      t337 = t216 * z
      t355 = -0.15D2 * t230 * t162 * t107 + 0.9D1 * t254 * t131 * t10 + 
     #0.21D2 * t210 * t8 + 0.9D1 * t234 * t131 * t11 - 0.9D1 * t285 * t6
     #0 * t143 * t11 + 0.18D2 * t234 * t130 * t11 - 0.18D2 * t285 * t25 
     #* t179 * t11 + 0.3D1 * t14 * t21 * t173 * t264 - 0.30D2 * t168 * t
     #81 * t164 + 0.8D1 * t129 * t130 * t216 + 0.36D2 * t289 * t151 * t2
     #8 * z - 0.8D1 * t150 * t35 * t10 * t337 - 0.72D2 * t150 * t151 * t
     #81 * z + 0.6D1 * t289 * t115 * t337 + 0.3D1 * t184 * t143 * t11 * 
     #t216 + 0.9D1 * t184 * t144 * t28
      bggbH21J1 = -0.16D2 / 0.3D1 * wd * (t140 + t228 + t299 + t355) / t
     #11 / s

      end function
  
   
 

      doubleprecision function bggbH21J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t9 * t11
      t13 = t6 * t12
      t14 = 0.1D1 - x4
      t15 = s * t3
      t16 = t7 * x1
      t19 = 0.1D1 - x3
      t20 = t16 * t19
      t22 = s - t15 * t16 * x3 - t15 * t20
      t23 = t22 ** 2
      t24 = t14 * t23
      t28 = cos(x2 * 0.3141592653589793D1)
      t32 = sqrt(x3 * t19 * x4 * t14)
      t35 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t28 * t32
      t40 = t4 ** 2
      t41 = t2 * t40
      t42 = t8 ** 2
      t43 = x1 ** 2
      t44 = t43 * x1
      t45 = t42 * t44
      t47 = x3 * t23
      t49 = t19 * t10 * t35
      t53 = t23 * t2
      t56 = t35 ** 2
      t57 = t11 * t56
      t61 = t1 ** 2
      t62 = t22 * t61
      t65 = x1 * t19
      t69 = t61 * t4
      t71 = t43 * x3
      t72 = t22 * t19
      t76 = t61 * t5
      t78 = t44 * x3
      t79 = t19 ** 2
      t84 = t53 * t4
      t86 = t10 * t35
      t90 = t53 * t3
      t91 = t19 * z
      t95 = t6 * t9
      t96 = x3 ** 2
      t98 = t23 * t19
      t102 = t61 * t40
      t104 = t43 ** 2
      t106 = t79 * t19
      t110 = t8 * t43
      t111 = t79 * z
      t115 = t2 * t4
      t116 = t115 * t8
      t120 = t40 * t3
      t121 = t42 * t7
      t127 = -0.18D2 * t13 * t24 * x1 * t35 + 0.6D1 * t41 * t45 * t47 * 
     #t49 + 0.9D1 * t53 * t40 * t42 * t43 * t57 + 0.16D2 * t62 * z * t3 
     #* t7 * t65 + 0.19D2 * t69 * t8 * t71 * t72 - 0.11D2 * t76 * t9 * t
     #78 * t22 * t79 + 0.27D2 * t84 * t8 * x1 * t86 + 0.11D2 * t90 * t16
     # * t91 - 0.6D1 * t95 * t44 * t96 * t98 + t102 * t42 * t104 * x3 * 
     #t22 * t106 - 0.2D1 * t84 * t110 * t111 + 0.14D2 * t116 * t71 * t98
     # - t53 * t120 * t121 * t44 * t19 * t57
      t141 = t61 * t120
      t144 = x3 * t22
      t152 = x1 * x3
      t157 = t2 * t3 * t7
      t161 = t9 * t43
      t166 = t10 * t14
      t176 = t14 ** 2
      t194 = -0.6D1 * t95 * t78 * t23 * t79 + 0.3D1 * t53 * t40 * t42 * 
     #t44 * t79 * t86 + 0.16D2 * t84 * t110 * t79 - 0.9D1 * t141 * t121 
     #* t44 * t144 * t57 - 0.22D2 * t90 * t20 - 0.9D1 * t61 * t3 * t7 * 
     #t152 * t22 - 0.9D1 * t157 * t152 * t23 + 0.27D2 * t76 * t161 * t14
     #4 * t86 - 0.9D1 * t157 * t166 * t23 + t141 * t121 * t104 * t144 * 
     #t79 * t10 * t35 + 0.9D1 * t116 * t11 * t176 * t23 + 0.18D2 * t116 
     #* t43 * t96 * t23 + t61 * t40 * t4 * t42 * t8 * t104 * t144 * t19 
     #* t11 * t56
      t200 = t42 * t11
      t201 = t41 * t200
      t202 = t14 * t43
      t212 = t53 * t5 * t9
      t213 = t43 * t19
      t215 = z * t10 * t35
      t222 = t76 * t12
      t223 = t176 * x1
      t227 = t9 * t44
      t231 = t8 * t10
      t232 = t69 * t231
      t238 = t6 * t9 * t10
      t246 = t102 * t200
      t247 = t14 * t22
      t248 = t213 * t35
      t252 = t115 * t231
      t260 = -0.36D2 * t6 * t161 * t47 * t86 + 0.18D2 * t201 * t202 * t4
     #7 * t35 - 0.4D1 * t115 * t110 * t47 * t91 + t212 * t213 * t215 - t
     #102 * t45 * x3 * t72 * t215 - 0.18D2 * t222 * t223 * t144 - t76 * 
     #t227 * t144 * t111 + 0.36D2 * t232 * t14 * x1 * t144 - 0.5D1 * t23
     #8 * t24 * t43 * t79 - t222 * t176 * t22 * t65 - 0.8D1 * t246 * t24
     #7 * t248 + 0.24D2 * t252 * t24 * t65 + 0.11D2 * t69 * t110 * t144 
     #* t91
      t264 = t61 * s * z * t4 * t8
      t268 = t65 * z
      t311 = -0.16D2 * t264 * t166 * t65 - t232 * t247 * t268 - t252 * t
     #24 * t268 - 0.2D1 * t13 * t176 * t23 * t65 - 0.9D1 * t13 * t223 * 
     #t47 - 0.18D2 * t246 * t202 * t144 * t35 + 0.36D2 * t264 * t166 * t
     #152 + 0.2D1 * t201 * t24 * t248 - 0.8D1 * t62 * z * t5 * t161 * t4
     #9 - 0.36D2 * t238 * t202 * t96 * t23 - 0.12D2 * t102 * t45 * t144 
     #* t49 - 0.22D2 * t212 * t213 * t86 + 0.9D1 * t53 - 0.3D1 * t53 * t
     #5 * t227 * t106
      bggbH21J2 = -0.16D2 / 0.3D1 * wd * (t127 + t194 + t260 + t311) / t
     #23 / s

      end function
  
   
 

      doubleprecision function bggbH21J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 ** 2
      t11 = x1 ** 2
      t12 = t11 ** 2
      t14 = s * t3
      t15 = t7 * x1
      t18 = 0.1D1 - x3
      t19 = t15 * t18
      t21 = s - t14 * t15 * x3 - t14 * t19
      t22 = t18 ** 2
      t23 = t22 * t18
      t27 = t21 ** 2
      t28 = t1 * s
      t29 = t27 * t28
      t30 = t4 * t3
      t31 = t8 * t7
      t34 = t11 * t18
      t35 = 0.1D1 - x1
      t39 = cos(x2 * 0.3141592653589793D1)
      t41 = 0.1D1 - x4
      t44 = sqrt(x3 * t18 * x4 * t41)
      t47 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t39 * t44
      t48 = t35 * t47
      t52 = t29 * t4
      t53 = t8 * t11
      t57 = t28 * t30
      t58 = t35 ** 2
      t59 = t31 * t58
      t61 = t41 ** 2
      t63 = x1 * t18
      t67 = t8 * t35
      t68 = t28 * t4 * t67
      t69 = t41 * t27
      t70 = t63 * z
      t73 = t2 * t4
      t75 = t41 * t21
      t79 = t22 * z
      t84 = t11 * x1
      t88 = t29 * t3
      t89 = t18 * z
      t95 = x3 * t21
      t109 = t6 * t9 * t12 * x3 * t21 * t23 - 0.9D1 * t29 * t30 * t31 * 
     #t34 * t48 + 0.11D2 * t52 * t53 * t22 - t57 * t59 * t61 * t27 * t63
     # - t68 * t69 * t70 - 0.7D1 * t73 * t67 * t75 * t70 - t52 * t53 * t
     #79 + t29 * t5 * t9 * t84 * t22 * t48 + 0.8D1 * t88 * t15 * t89 - 0
     #.9D1 * t6 * t9 * t84 * t95 * t18 * t35 * t47 + 0.9D1 * t73 * t8 * 
     #t11 * x3 * t21 * t18 - 0.9D1 * t88 * t19
      t110 = t2 * t30
      t124 = t9 * t58
      t126 = t34 * t47
      t155 = t31 * t84
      t175 = -0.10D2 * t110 * t31 * t84 * x3 * t21 * t22 - 0.3D1 * t57 *
     # t31 * t35 * t69 * t11 * t22 + t28 * t5 * t124 * t69 * t126 + t2 *
     # t5 * t3 * t9 * t7 * t12 * t95 * t22 * t35 * t47 + 0.8D1 * t73 * t
     #53 * t95 * t89 + t110 * t59 * t61 * t21 * t63 + 0.9D1 * t68 * t69 
     #* t63 - 0.8D1 * t21 * t2 * z * t3 * t7 * t63 - t110 * t155 * t95 *
     # t79 - 0.2D1 * t29 * t30 * t155 * t23 + 0.8D1 * t6 * t124 * t75 * 
     #t126 + 0.8D1 * t2 * s * z * t4 * t8 * t35 * t41 * t63
      bggbH21J3 = -0.16D2 / 0.3D1 * wd * (t109 + t175) / t27 / s

      end function
  
 