  
      subroutine gbgbH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH21J1  
      doubleprecision gbgbH21J2  
      doubleprecision gbgbH21J3  
      doubleprecision gbgbH2n1e1  
      doubleprecision gbgbH2n1e0  
      doubleprecision gbgbH2n1em1  
      doubleprecision gbgbH2n1em2  
      doubleprecision gbgbH2n1em3  
      doubleprecision gbgbH2n1em4  
      doubleprecision gbgbH2n2e1  
      doubleprecision gbgbH2n2e0  
      doubleprecision gbgbH2n2em1  
      doubleprecision gbgbH2n2em2  
      doubleprecision gbgbH2n2em3  
      doubleprecision gbgbH2n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH2n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH2n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH2n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH2n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH2n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH2n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
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
      t23 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t43 = t5 ** 2
      t44 = t3 ** 2
      t49 = -0.2884936567583026D3 - t28 + t30
      t51 = t17 ** 2
      t59 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
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
      t151 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, 0.0D0)
      t152 = t145 * t9
      t153 = -t150
      t154 = t11 * t153
      t155 = t154 * x4
      t158 = log(-0.4D1 * t152 * t155)
      t159 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, 0.0D0)
      t163 = t59 - t159
      t164 = t22 * t163
      t169 = 0.1D1 / x3
      t170 = t169 * t93
      t176 = log(-0.4D1 * t145 * t12 * t153)
      t178 = t145 * t12
      t180 = log(0.4D1 * t178)
      t186 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, 0.0D0)
      t188 = t176 ** 2
      t192 = t180 ** 2
      t199 = -t75 * t22 * t163
      t204 = x3 * t9
      t207 = log(-0.4D1 * t204 * t154)
      t209 = t207 ** 2
      t212 = t204 * t11
      t214 = log(0.4D1 * t212)
      t216 = t214 ** 2
      t248 = log(-0.4D1 * t204 * t155)
      t252 = log(0.4D1 * t204 * t125)
      t259 = t248 ** 2
      t263 = t252 ** 2
      t272 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t1 * t22 *
     # t23 / 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 -
     # 0.90D2 * t17 * lh - 0.15D2 * t35) * t1 * t22 * t39 / 0.5760D4 - (
     #t43 + 0.60D2 * t44 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 
     #- t14 * t49 + 0.15D2 / 0.4D1 * t51 + t17 * t31 / 0.2D1 + 0.30D2 * 
     #t35 * lh) * t1 * t60 / 0.5760D4 + (-0.180D3 * t63 * t22 * (-t23 + 
     #t66 * t39 - t68 * t59 / 0.2D1) + t75 * t22 * (-t39 + t66 * t59) + 
     #t81 * (t66 * t23 - t68 * t39 / 0.2D1 + t68 * t66 * t59 / 0.6D1) - 
     #t91) * t93 / 0.5760D4 + (-0.180D3 * t63 * t22 * (-t23 + t100 * t39
     # - t102 * t59 / 0.2D1) + t75 * t22 * (-t39 + t100 * t59) + t81 * (
     #t100 * t23 - t102 * t39 / 0.2D1 + t102 * t100 * t59 / 0.6D1) - t91
     #) * t122 / 0.2880D4 - (-0.180D3 * t63 * t22 * (t39 - t128 * t59) +
     # t81 * (t23 - t128 * t39 + t135 * t59 / 0.2D1) + t75 * t60) * t122
     # * t93 / 0.2880D4 - (t81 * (t39 - t148 * t59 - t151 + t158 * t159)
     # - 0.180D3 * t63 * t164) * t122 * t170 / 0.2880D4 + (-0.180D3 * t6
     #3 * t22 * (t151 - t176 * t159 - t39 + t180 * t59) + t81 * (t186 - 
     #t176 * t151 + t188 * t159 / 0.2D1 - t23 + t180 * t39 - t192 * t59 
     #/ 0.2D1) + t199) * t122 * t169 / 0.2880D4 - (-0.180D3 * t63 * t22 
     #* (-t186 + t207 * t151 - t209 * t159 / 0.2D1 + t23 - t214 * t39 + 
     #t216 * t59 / 0.2D1) + t75 * t22 * (t39 - t214 * t59 - t151 + t207 
     #* t159) + t81 * (t207 * t186 - t209 * t151 / 0.2D1 + t209 * t207 *
     # t159 / 0.6D1 - t214 * t23 + t216 * t39 / 0.2D1 - t216 * t214 * t5
     #9 / 0.6D1) + t90 * t164) * t169 / 0.5760D4 + (-0.180D3 * t63 * t22
     # * (t151 - t248 * t159 - t39 + t252 * t59) + t81 * (t186 - t248 * 
     #t151 + t259 * t159 / 0.2D1 - t23 + t252 * t39 - t263 * t59 / 0.2D1
     #) + t199) * t169 * t93 / 0.5760D4
      t273 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t272)
      t276 = -0.1D1 + x4
      t278 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t279 = t125 * t276
      t282 = log(-0.4D1 * t97 * t279)
      t283 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t289 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t291 = t282 ** 2
      t296 = t22 * t283
      t302 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, x4)
      t307 = log(0.4D1 * t152 * t125 * t276 * t153)
      t308 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, x4)
      t312 = log(-0.4D1 * t152 * t279)
      t316 = -t283 + t308
      t324 = x4 * t276
      t328 = log(0.4D1 * t212 * t324 * t153)
      t332 = log(-0.4D1 * t204 * t279)
      t338 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t150, x4)
      t340 = t328 ** 2
      t344 = t332 ** 2
      t358 = log(-0.4D1 * t12 * t324)
      t360 = t358 ** 2
      t383 = -(-0.180D3 * t63 * t22 * (-t278 + t282 * t283) + t81 * (-t2
     #89 + t282 * t278 - t291 * t283 / 0.2D1) - t75 * t296) * t122 * t93
     # / 0.2880D4 - (t81 * (t302 - t307 * t308 - t278 + t312 * t283) - 0
     #.180D3 * t63 * t22 * t316) * t122 * t170 / 0.2880D4 + (-0.180D3 * 
     #t63 * t22 * (-t302 + t328 * t308 + t278 - t332 * t283) + t81 * (-t
     #338 + t328 * t302 - t340 * t308 / 0.2D1 + t289 - t332 * t278 + t34
     #4 * t283 / 0.2D1) - t75 * t22 * t316) * t169 * t93 / 0.5760D4 + (-
     #0.180D3 * t63 * t22 * (t289 - t358 * t278 + t360 * t283 / 0.2D1) +
     # t75 * t22 * (t278 - t358 * t283) + t81 * (-t358 * t289 + t360 * t
     #278 / 0.2D1 - t360 * t358 * t283 / 0.6D1) + t90 * t296) * t93 / 0.
     #5760D4
      t384 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t276, 0.0D0,
     # t383)
      t386 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t387 = s * t386
      t388 = t1 * x1
      t390 = -0.1D1 + x1
      t391 = t1 * t390
      t393 = t386 ** 2
      t395 = t1 ** 2
      t400 = 0.1D1 / (-0.2D1 + t386)
      t401 = t386 * t400
      t402 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t403 = t401 * t402
      t404 = t390 ** 2
      t405 = t11 * t404
      t406 = t393 ** 2
      t407 = t405 * t406
      t410 = log(0.4D1 * t97 * t407)
      t411 = t410 * t386
      t412 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t413 = t400 * t412
      t415 = t410 ** 2
      t416 = t415 * t386
      t417 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t418 = t400 * t417
      t425 = t401 * t412
      t441 = t401 * t417
      t449 = log(0.4D1 * t98 * t404 * x4 * t406)
      t450 = t449 * t386
      t457 = t449 ** 2
      t463 = t75 * t22
      t464 = t463 * t441
      t472 = log(0.4D1 * t152 * t405 * x4 * t406)
      t477 = t63 * t22
      t485 = log(0.4D1 * t152 * t407)
      t486 = t485 * t386
      t493 = t485 ** 2
      t503 = (0.180D3 * t63 * t22 * (t403 - t411 * t413 + t416 * t418 / 
     #0.2D1) - t75 * t22 * (t425 - t411 * t418) - t81 * (-t411 * t400 * 
     #t402 + t416 * t413 / 0.2D1 - t415 * t410 * t386 * t418 / 0.6D1) - 
     #t90 * t22 * t441) * t122 / 0.2880D4 - (-0.180D3 * t63 * t22 * (t42
     #5 - t450 * t418) + t81 * (t403 - t450 * t413 + t457 * t386 * t418 
     #/ 0.2D1) + t464) * t122 * t93 / 0.2880D4 - (t81 * (t425 - t472 * t
     #386 * t418) - 0.180D3 * t477 * t441) * t122 * t170 / 0.2880D4 + (-
     #0.180D3 * t63 * t22 * (-t425 + t486 * t418) + t81 * (-t403 + t486 
     #* t413 - t493 * t386 * t418 / 0.2D1) - t464) * t122 * t169 / 0.288
     #0D4
      t504 = FJET(XB1, XB2, s, t387 * t388, 0.0D0, 0.0D0, -t387 * t391, 
     #-s * t393 * t395 * x1 * t390, t503)
      t506 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t507 = s * t506
      t509 = t391 * x4
      t511 = t391 * t276
      t513 = t506 ** 2
      t516 = x1 * t390
      t520 = 0.1D1 / (-0.2D1 + t506)
      t521 = t506 * t520
      t522 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t523 = t521 * t522
      t524 = t513 ** 2
      t526 = t324 * t524 * t404
      t529 = log(-0.4D1 * t98 * t526)
      t530 = t529 * t506
      t531 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t532 = t520 * t531
      t538 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t542 = t529 ** 2
      t548 = t521 * t531
      t555 = log(-0.4D1 * t178 * t526)
      t566 = -(0.180D3 * t63 * t22 * (t523 - t530 * t532) - t81 * (t521 
     #* t538 - t530 * t520 * t522 + t542 * t506 * t532 / 0.2D1) - t463 *
     # t548) * t122 * t93 / 0.2880D4 - (t81 * (-t523 + t555 * t506 * t53
     #2) + 0.180D3 * t477 * t548) * t122 * t170 / 0.2880D4
      t567 = FJET(XB1, XB2, s, t507 * t388, 0.0D0, -t507 * t509, t507 * 
     #t511, s * t513 * t395 * t516 * t276, t566)
      t569 = KAPPA2(x1, x2, t150, 0.0D0, z)
      t570 = s * t569
      t571 = t388 * t153
      t573 = t388 * x3
      t576 = t569 ** 2
      t582 = 0.1D1 / (-0.2D1 + t569)
      t583 = t569 * t582
      t584 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t150, 0.0D0)
      t585 = t583 * t584
      t586 = t404 * t153
      t587 = t576 ** 2
      t592 = log(-0.4D1 * t178 * t586 * x4 * t587)
      t594 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t150, 0.0D0)
      t595 = t582 * t594
      t599 = t583 * t594
      t609 = log(-0.4D1 * t152 * t405 * t153 * t587)
      t610 = t609 * t569
      t616 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, t150, 0.0D0)
      t620 = t609 ** 2
      t631 = -(t81 * (-t585 + t592 * t569 * t595) + 0.180D3 * t477 * t59
     #9) * t122 * t170 / 0.2880D4 + (-0.180D3 * t63 * t22 * (t585 - t610
     # * t595) + t81 * (t583 * t616 - t610 * t582 * t584 + t620 * t569 *
     # t595 / 0.2D1) + t463 * t599) * t122 * t169 / 0.2880D4
      t632 = FJET(XB1, XB2, s, -t570 * t571, t570 * t573, 0.0D0, -t570 *
     # t391, s * t576 * t395 * t516 * t153, t631)
      t634 = KAPPA2(x1, x2, t150, x4, z)
      t635 = s * t634
      t640 = t634 ** 2
      t645 = cos(t7)
      t648 = sqrt(x3 * t153 * t324)
      t655 = 0.1D1 / (-0.2D1 + t634)
      t656 = t634 * t655
      t657 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t150, x4)
      t659 = t640 ** 2
      t664 = log(0.4D1 * t178 * t324 * t586 * t659)
      t666 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t150, x4)
      t674 = t81 * (t656 * t657 - t664 * t634 * t655 * t666) - 0.180D3 *
     # t477 * t656 * t666
      t678 = FJET(XB1, XB2, s, -t635 * t571, t635 * t573, -t635 * t509, 
     #t635 * t511, s * t640 * t395 * t516 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t645 * t648), -t674 * t122 * t170 / 0.2880D4)
      gbgbH2n1e1 = t273 * t272 + t384 * t383 + t504 * t503 + t567 * t566
     # + t632 * t631 - t678 * t674 * t122 * t169 * t93 / 0.2880D4

      end function



      doubleprecision function gbgbH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * x4
      t18 = log(0.4D1 * t12 * t15)
      t19 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t23 = lh * t1
      t24 = t5 * t19
      t28 = 0.1D1 / x1
      t30 = 0.1D1 / x4
      t33 = 0.1D1 - x3
      t34 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, 0.0D0)
      t35 = t19 - t34
      t37 = 0.1D1 / x3
      t39 = t28 * t37 * t30
      t42 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, 0.0D0)
      t43 = x3 * t8
      t44 = t11 * t14
      t45 = -t33
      t49 = log(-0.4D1 * t43 * t44 * t45)
      t53 = log(0.4D1 * t43 * t44)
      t60 = -0.180D3 * t23 * t5 * t35
      t65 = t12 * t14
      t67 = log(0.4D1 * t65)
      t73 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t75 = t67 ** 2
      t80 = lh ** 2
      t81 = 0.180D3 * t80
      t82 = 0.3141592653589793D1 ** 2
      t83 = 0.30D2 * t82
      t84 = t81 - t83
      t85 = t84 * t1
      t86 = t85 * t24
      t92 = log(0.4D1 * t44 * x4)
      t99 = t92 ** 2
      t107 = x3 * t11
      t108 = t14 * t45
      t112 = log(-0.4D1 * t107 * t108 * x4)
      t116 = log(0.4D1 * t107 * t15)
      t124 = t107 * t14
      t126 = log(0.4D1 * t124)
      t130 = log(-0.4D1 * t107 * t108)
      t136 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, 0.0D0)
      t138 = t130 ** 2
      t142 = t126 ** 2
      t154 = log(0.4D1 * t44)
      t163 = t154 ** 2
      t183 = -(t6 * (t7 - t18 * t19) - 0.180D3 * t23 * t24) * t28 * t30 
     #/ 0.2880D4 - t6 * t35 * t39 / 0.2880D4 + (t6 * (t42 - t49 * t34 - 
     #t7 + t53 * t19) - t60) * t28 * t37 / 0.2880D4 + (-0.180D3 * t23 * 
     #t5 * (-t7 + t67 * t19) + t6 * (-t73 + t67 * t7 - t75 * t19 / 0.2D1
     #) - t86) * t28 / 0.2880D4 + (-0.180D3 * t23 * t5 * (-t7 + t92 * t1
     #9) + t6 * (-t73 + t92 * t7 - t99 * t19 / 0.2D1) - t86) * t30 / 0.5
     #760D4 + (t6 * (t42 - t112 * t34 - t7 + t116 * t19) - t60) * t37 * 
     #t30 / 0.5760D4 - (-0.180D3 * t23 * t5 * (t7 - t126 * t19 - t42 + t
     #130 * t34) + t6 * (-t136 + t130 * t42 - t138 * t34 / 0.2D1 + t73 -
     # t126 * t7 + t142 * t19 / 0.2D1) + t85 * t5 * t35) * t37 / 0.5760D
     #4 - (-0.180D3 * lh - 0.90D2 * t154) * t1 * t5 * t73 / 0.5760D4 - (
     #t81 - t83 + 0.180D3 * t154 * lh + 0.45D2 * t163) * t1 * t5 * t7 / 
     #0.5760D4 - (-0.2884936567583026D3 - 0.120D3 * t80 * lh + 0.60D2 * 
     #lh * t82 - t154 * t84 - 0.90D2 * t163 * lh - 0.15D2 * t163 * t154)
     # * t1 * t24 / 0.5760D4
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t183)
      t187 = -0.1D1 + x4
      t189 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t190 = t15 * t187
      t193 = log(-0.4D1 * t12 * t190)
      t194 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t198 = t5 * t194
      t205 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, x4)
      t206 = -t194 + t205
      t210 = x4 * t187
      t213 = log(-0.4D1 * t44 * t210)
      t219 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t221 = t213 ** 2
      t230 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t33, x4)
      t234 = log(0.4D1 * t124 * t210 * t45)
      t238 = log(-0.4D1 * t107 * t190)
      t250 = -(t6 * (-t189 + t193 * t194) + 0.180D3 * t23 * t198) * t28 
     #* t30 / 0.2880D4 - t6 * t206 * t39 / 0.2880D4 + (-0.180D3 * t23 * 
     #t5 * (t189 - t213 * t194) + t6 * (t219 - t213 * t189 + t221 * t194
     # / 0.2D1) + t85 * t198) * t30 / 0.5760D4 + (t6 * (-t230 + t234 * t
     #205 + t189 - t238 * t194) + 0.180D3 * t23 * t5 * t206) * t37 * t30
     # / 0.5760D4
      t251 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t187, 0.0D0,
     # t250)
      t253 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t254 = s * t253
      t255 = t1 * x1
      t257 = -0.1D1 + x1
      t258 = t1 * t257
      t260 = t253 ** 2
      t262 = t1 ** 2
      t267 = 0.1D1 / (-0.2D1 + t253)
      t268 = t253 * t267
      t269 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t270 = t268 * t269
      t271 = t257 ** 2
      t273 = t260 ** 2
      t277 = log(0.4D1 * t65 * t271 * x4 * t273)
      t279 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t280 = t267 * t279
      t284 = t23 * t5
      t285 = t268 * t279
      t287 = 0.180D3 * t284 * t285
      t293 = t37 * t30
      t296 = t43 * t11
      t297 = t14 * t271
      t298 = t297 * t273
      t301 = log(0.4D1 * t296 * t298)
      t311 = log(0.4D1 * t12 * t298)
      t312 = t311 * t253
      t318 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t322 = t311 ** 2
      t333 = -(t6 * (t270 - t277 * t253 * t280) - t287) * t28 * t30 / 0.
     #2880D4 - t6 * t268 * t279 * t28 * t293 / 0.2880D4 + (t6 * (-t270 +
     # t301 * t253 * t280) + t287) * t28 * t37 / 0.2880D4 + (0.180D3 * t
     #23 * t5 * (t270 - t312 * t280) - t6 * (t268 * t318 - t312 * t267 *
     # t269 + t322 * t253 * t280 / 0.2D1) - t85 * t5 * t285) * t28 / 0.2
     #880D4
      t334 = FJET(XB1, XB2, s, t254 * t255, 0.0D0, 0.0D0, -t254 * t258, 
     #-s * t260 * t262 * x1 * t257, t333)
      t336 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t337 = s * t336
      t339 = t258 * x4
      t341 = t258 * t187
      t343 = t336 ** 2
      t346 = x1 * t257
      t350 = 0.1D1 / (-0.2D1 + t336)
      t351 = t336 * t350
      t352 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t354 = t343 ** 2
      t359 = log(-0.4D1 * t65 * t210 * t354 * t271)
      t361 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t377 = -(-t6 * (t351 * t352 - t359 * t336 * t350 * t361) + 0.180D3
     # * t284 * t351 * t361) * t28 * t30 / 0.2880D4 + t6 * t351 * t361 *
     # t28 * t293 / 0.2880D4
      t378 = FJET(XB1, XB2, s, t337 * t255, 0.0D0, -t337 * t339, t337 * 
     #t341, s * t343 * t262 * t346 * t187, t377)
      t380 = KAPPA2(x1, x2, t33, 0.0D0, z)
      t381 = s * t380
      t382 = t255 * t45
      t384 = t255 * x3
      t387 = t380 ** 2
      t393 = 0.1D1 / (-0.2D1 + t380)
      t394 = t380 * t393
      t396 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t33, 0.0D0)
      t400 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t33, 0.0D0)
      t402 = t387 ** 2
      t407 = log(-0.4D1 * t296 * t297 * t45 * t402)
      t420 = t6 * t394 * t396 * t28 * t293 / 0.2880D4 + (t6 * (t394 * t4
     #00 - t407 * t380 * t393 * t396) - 0.180D3 * t284 * t394 * t396) * 
     #t28 * t37 / 0.2880D4
      t421 = FJET(XB1, XB2, s, -t381 * t382, t381 * t384, 0.0D0, -t381 *
     # t258, s * t387 * t262 * t346 * t45, t420)
      t423 = KAPPA2(x1, x2, t33, x4, z)
      t424 = s * t423
      t429 = t423 ** 2
      t434 = cos(t9)
      t437 = sqrt(x3 * t45 * t210)
      t444 = 0.1D1 / (-0.2D1 + t423)
      t447 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t33, x4)
      t452 = FJET(XB1, XB2, s, -t424 * t382, t424 * t384, -t424 * t339, 
     #t424 * t341, s * t429 * t262 * t346 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t434 * t437), -t6 * t423 * t444 * t447 * t28 * t
     #293 / 0.2880D4)
      gbgbH2n1e0 = t184 * t183 + t251 * t250 + t334 * t333 + t378 * t377
     # + t421 * t420 - t452 * t3 * t5 * t423 * t444 * t447 * t39 / 0.288
     #0D4

      end function



      doubleprecision function gbgbH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 - x3
      t8 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, 0.0D0)
      t9 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t10 = t8 - t9
      t11 = 0.1D1 / x3
      t13 = 0.1D1 / x4
      t17 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = x3 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t26 = log(0.4D1 * t21 * t23)
      t28 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, 0.0D0)
      t29 = -t7
      t33 = log(-0.4D1 * t21 * t23 * t29)
      t37 = lh * t1
      t45 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t49 = t20 * t23
      t51 = log(0.4D1 * t49)
      t58 = lh ** 2
      t60 = 0.3141592653589793D1 ** 2
      t64 = t51 ** 2
      t68 = t5 * t9
      t71 = x1 ** 2
      t72 = t71 * t20
      t75 = log(0.4D1 * t72 * t23)
      t80 = 0.180D3 * t37 * t68
      t82 = 0.1D1 / x1
      t95 = log(0.4D1 * t49 * x4)
      t102 = t6 * t10 * t11 * t13 / 0.5760D4 - (t6 * (t17 - t26 * t9 - t
     #28 + t33 * t8) + 0.180D3 * t37 * t5 * t10) * t11 / 0.5760D4 - t6 *
     # t45 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t51) * t1 * t5 * t17 /
     # 0.5760D4 - (0.180D3 * t58 - 0.30D2 * t60 + 0.180D3 * t51 * lh + 0
     #.45D2 * t64) * t1 * t68 / 0.5760D4 + (t6 * (-t17 + t75 * t9) + t80
     #) * t82 / 0.2880D4 - t6 * t9 * t82 * t13 / 0.2880D4 + t6 * t10 * t
     #82 * t11 / 0.2880D4 + (t6 * (-t17 + t95 * t9) + t80) * t13 / 0.576
     #0D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t102)
      t106 = -0.1D1 + x4
      t108 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t113 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t117 = log(-0.4D1 * t49 * x4 * t106)
      t127 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, x4)
      t133 = t6 * t108 * t82 * t13 / 0.2880D4 + (t6 * (t113 - t117 * t10
     #8) - 0.180D3 * t37 * t5 * t108) * t13 / 0.5760D4 + t6 * (-t127 + t
     #108) * t11 * t13 / 0.5760D4
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t106, 0.0D0,
     # t133)
      t136 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t137 = s * t136
      t138 = t1 * x1
      t140 = -0.1D1 + x1
      t141 = t1 * t140
      t143 = t136 ** 2
      t145 = t1 ** 2
      t150 = 0.1D1 / (-0.2D1 + t136)
      t151 = t136 * t150
      t152 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t154 = t140 ** 2
      t156 = t143 ** 2
      t160 = log(0.4D1 * t72 * t23 * t154 * t156)
      t162 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t163 = t150 * t162
      t173 = t6 * t136
      t174 = t82 * t13
      t177 = t82 * t11
      t181 = (-t6 * (t151 * t152 - t160 * t136 * t163) + 0.180D3 * t37 *
     # t5 * t151 * t162) * t82 / 0.2880D4 - t173 * t163 * t174 / 0.2880D
     #4 - t173 * t163 * t177 / 0.2880D4
      t182 = FJET(XB1, XB2, s, t137 * t138, 0.0D0, 0.0D0, -t137 * t141, 
     #-s * t143 * t145 * x1 * t140, t181)
      t184 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t185 = s * t184
      t191 = t184 ** 2
      t194 = x1 * t140
      t200 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t202 = 0.1D1 / (-0.2D1 + t184) * t200 * t174
      t205 = FJET(XB1, XB2, s, t185 * t138, 0.0D0, -t185 * t141 * x4, t1
     #85 * t141 * t106, s * t191 * t145 * t194 * t106, t6 * t184 * t202 
     #/ 0.2880D4)
      t211 = KAPPA2(x1, x2, t7, 0.0D0, z)
      t212 = s * t211
      t218 = t211 ** 2
      t226 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t7, 0.0D0)
      t228 = 0.1D1 / (-0.2D1 + t211) * t226 * t177
      t231 = FJET(XB1, XB2, s, -t212 * t138 * t29, t212 * t138 * x3, 0.0
     #D0, -t212 * t141, s * t218 * t145 * t194 * t29, t6 * t211 * t228 /
     # 0.2880D4)
      gbgbH2n1em1 = t103 * t102 + t134 * t133 + t182 * t181 + t205 * t3 
     #* t5 * t184 * t202 / 0.2880D4 + t231 * t3 * t5 * t211 * t228 / 0.2
     #880D4

      end function



      doubleprecision function gbgbH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t8 = 0.1D1 / x1
      t13 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, 0.0
     #D0)
      t19 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t24 = sin(x2 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t26 = z ** 2
      t30 = log(0.4D1 * t25 / t26)
      t37 = 0.1D1 / x4
      t41 = -t6 * t7 * t8 / 0.2880D4 - t6 * (t7 - t13) / x3 / 0.5760D4 -
     # t6 * t19 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t30) * t1 * t5 * 
     #t7 / 0.5760D4 - t6 * t7 * t37 / 0.5760D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t45 = s * t44
      t48 = -0.1D1 + x1
      t51 = t44 ** 2
      t53 = t1 ** 2
      t59 = 0.1D1 / (-0.2D1 + t44)
      t60 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t65 = FJET(XB1, XB2, s, t45 * t1 * x1, 0.0D0, 0.0D0, -t45 * t1 * t
     #48, -s * t51 * t53 * x1 * t48, -t6 * t44 * t59 * t60 * t8 / 0.2880
     #D4)
      t76 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t6 * t76 * t37 / 0.5760D4)
      gbgbH2n1em2 = t42 * t41 - t65 * t3 * t5 * t44 * t59 * t60 * t8 / 0
     #.2880D4 + t80 * t3 * t5 * t76 * t37 / 0.5760D4

      end function



      doubleprecision function gbgbH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, -t3 * 
     #t5 * t7 / 0.5760D4)
      gbgbH2n1em3 = -t10 * t3 * t5 * t7 / 0.5760D4

      end function



      doubleprecision function gbgbH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      gbgbH2n1em4 = 0.0D0

      end function


      doubleprecision function gbgbH2n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
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
      t23 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t43 = t5 ** 2
      t44 = t3 ** 2
      t49 = -0.2884936567583026D3 - t28 + t30
      t51 = t17 ** 2
      t59 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
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
      t150 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t151 = t145 * t9
      t152 = -0.1D1 + x3
      t153 = t11 * t152
      t154 = t153 * x4
      t157 = log(-0.4D1 * t151 * t154)
      t158 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t162 = t158 - t59
      t163 = t22 * t162
      t168 = 0.1D1 / x3
      t169 = t168 * t93
      t172 = t145 * t12
      t174 = log(0.4D1 * t172)
      t179 = log(-0.4D1 * t145 * t12 * t152)
      t186 = t174 ** 2
      t189 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t191 = t179 ** 2
      t201 = x3 * t9
      t202 = t201 * t11
      t204 = log(0.4D1 * t202)
      t206 = t204 ** 2
      t211 = log(-0.4D1 * t201 * t153)
      t213 = t211 ** 2
      t240 = -t22 * t162
      t247 = log(0.4D1 * t201 * t125)
      t251 = log(-0.4D1 * t201 * t154)
      t258 = t247 ** 2
      t262 = t251 ** 2
      t272 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t1 * t22 *
     # t23 / 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 -
     # 0.90D2 * t17 * lh - 0.15D2 * t35) * t1 * t22 * t39 / 0.5760D4 - (
     #t43 + 0.60D2 * t44 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 
     #- t14 * t49 + 0.15D2 / 0.4D1 * t51 + t17 * t31 / 0.2D1 + 0.30D2 * 
     #t35 * lh) * t1 * t60 / 0.5760D4 - (-0.180D3 * t63 * t22 * (t23 - t
     #66 * t39 + t68 * t59 / 0.2D1) + t75 * t22 * (t39 - t66 * t59) + t8
     #1 * (-t66 * t23 + t68 * t39 / 0.2D1 - t68 * t66 * t59 / 0.6D1) + t
     #91) * t93 / 0.5760D4 - (-0.180D3 * t63 * t22 * (t23 - t100 * t39 +
     # t102 * t59 / 0.2D1) + t75 * t22 * (t39 - t100 * t59) + t81 * (-t1
     #00 * t23 + t102 * t39 / 0.2D1 - t102 * t100 * t59 / 0.6D1) + t91) 
     #* t122 / 0.2880D4 + (-0.180D3 * t63 * t22 * (-t39 + t128 * t59) + 
     #t81 * (-t23 + t128 * t39 - t135 * t59 / 0.2D1) - t75 * t60) * t122
     # * t93 / 0.2880D4 + (t81 * (-t39 + t148 * t59 + t150 - t157 * t158
     #) - 0.180D3 * t63 * t163) * t122 * t169 / 0.2880D4 + (-0.180D3 * t
     #63 * t22 * (-t39 + t174 * t59 + t150 - t179 * t158) + t81 * (-t23 
     #+ t174 * t39 - t186 * t59 / 0.2D1 + t189 - t179 * t150 + t191 * t1
     #58 / 0.2D1) + t75 * t163) * t122 * t168 / 0.2880D4 - (-0.180D3 * t
     #63 * t22 * (t23 - t204 * t39 + t206 * t59 / 0.2D1 - t189 + t211 * 
     #t150 - t213 * t158 / 0.2D1) + t75 * t22 * (-t150 + t211 * t158 + t
     #39 - t204 * t59) + t81 * (-t204 * t23 + t206 * t39 / 0.2D1 - t206 
     #* t204 * t59 / 0.6D1 + t211 * t189 - t213 * t150 / 0.2D1 + t213 * 
     #t211 * t158 / 0.6D1) + t90 * t240) * t168 / 0.5760D4 - (-0.180D3 *
     # t63 * t22 * (t39 - t247 * t59 - t150 + t251 * t158) + t81 * (t23 
     #- t247 * t39 + t258 * t59 / 0.2D1 - t189 + t251 * t150 - t262 * t1
     #58 / 0.2D1) + t75 * t240) * t168 * t93 / 0.5760D4
      t273 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t272)
      t276 = -0.1D1 + x4
      t278 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t279 = t125 * t276
      t282 = log(-0.4D1 * t97 * t279)
      t283 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t289 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t291 = t282 ** 2
      t296 = t22 * t283
      t302 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t307 = log(0.4D1 * t151 * t125 * t276 * t152)
      t308 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t312 = log(-0.4D1 * t151 * t279)
      t316 = t283 - t308
      t326 = log(-0.4D1 * t201 * t279)
      t328 = x4 * t276
      t332 = log(0.4D1 * t202 * t328 * t152)
      t339 = t326 ** 2
      t342 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t344 = t332 ** 2
      t358 = log(-0.4D1 * t12 * t328)
      t360 = t358 ** 2
      t383 = (-0.180D3 * t63 * t22 * (t278 - t282 * t283) + t81 * (t289 
     #- t282 * t278 + t291 * t283 / 0.2D1) + t75 * t296) * t122 * t93 / 
     #0.2880D4 + (t81 * (-t302 + t307 * t308 + t278 - t312 * t283) - 0.1
     #80D3 * t63 * t22 * t316) * t122 * t169 / 0.2880D4 - (-0.180D3 * t6
     #3 * t22 * (-t278 + t326 * t283 + t302 - t332 * t308) + t81 * (-t28
     #9 + t326 * t278 - t339 * t283 / 0.2D1 + t342 - t332 * t302 + t344 
     #* t308 / 0.2D1) - t75 * t22 * t316) * t168 * t93 / 0.5760D4 - (0.1
     #80D3 * t63 * t22 * (t289 - t358 * t278 + t360 * t283 / 0.2D1) - t7
     #5 * t22 * (t278 - t358 * t283) - t81 * (-t358 * t289 + t360 * t278
     # / 0.2D1 - t360 * t358 * t283 / 0.6D1) - t90 * t296) * t93 / 0.576
     #0D4
      t384 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t276, 0.0D0,
     # t383)
      t387 = -0.1D1 + x1
      t389 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t390 = t387 ** 2
      t391 = t96 * t390
      t394 = log(0.4D1 * t12 * t391)
      t395 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t397 = t394 ** 2
      t398 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t417 = t22 * t398
      t421 = t391 * x4
      t424 = log(0.4D1 * t12 * t421)
      t431 = t424 ** 2
      t436 = t75 * t417
      t442 = log(0.4D1 * t202 * t421)
      t455 = log(0.4D1 * t201 * t11 * t96 * t390)
      t462 = t455 ** 2
      t471 = -(0.180D3 * t63 * t22 * (t389 - t394 * t395 + t397 * t398 /
     # 0.2D1) - t75 * t22 * (t395 - t394 * t398) - t81 * (-t394 * t389 +
     # t397 * t395 / 0.2D1 - t397 * t394 * t398 / 0.6D1) - t90 * t417) *
     # t122 / 0.2880D4 + (-0.180D3 * t63 * t22 * (t395 - t424 * t398) + 
     #t81 * (t389 - t424 * t395 + t431 * t398 / 0.2D1) + t436) * t122 * 
     #t93 / 0.2880D4 + (t81 * (t395 - t442 * t398) - 0.180D3 * t63 * t41
     #7) * t122 * t169 / 0.2880D4 + (-0.180D3 * t63 * t22 * (t395 - t455
     # * t398) + t81 * (t389 - t455 * t395 + t462 * t398 / 0.2D1) + t436
     #) * t122 * t168 / 0.2880D4
      t472 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t387, 0.0D0,
     # t471)
      t474 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t475 = s * t474
      t476 = t1 * x1
      t478 = t1 * t387
      t479 = t478 * x4
      t481 = t478 * t276
      t483 = t474 ** 2
      t485 = t1 ** 2
      t487 = x1 * t387
      t491 = 0.1D1 / (-0.2D1 + t474)
      t492 = t474 * t491
      t493 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t494 = t492 * t493
      t495 = t483 ** 2
      t497 = t328 * t495 * t390
      t500 = log(-0.4D1 * t98 * t497)
      t501 = t500 * t474
      t502 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t503 = t491 * t502
      t509 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t513 = t500 ** 2
      t519 = t75 * t22
      t520 = t492 * t502
      t527 = log(-0.4D1 * t172 * t497)
      t532 = t63 * t22
      t539 = (-0.180D3 * t63 * t22 * (t494 - t501 * t503) + t81 * (t492 
     #* t509 - t501 * t491 * t493 + t513 * t474 * t503 / 0.2D1) + t519 *
     # t520) * t122 * t93 / 0.2880D4 + (t81 * (t494 - t527 * t474 * t503
     #) - 0.180D3 * t532 * t520) * t122 * t169 / 0.2880D4
      t540 = FJET(XB1, XB2, s, 0.0D0, t475 * t476, -t475 * t479, t475 * 
     #t481, -s * t483 * t485 * t487 * x4, t539)
      t542 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t543 = s * t542
      t544 = t476 * x3
      t546 = t476 * t152
      t549 = t542 ** 2
      t555 = 0.1D1 / (-0.2D1 + t542)
      t556 = t542 * t555
      t557 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t558 = t556 * t557
      t559 = t390 * t152
      t560 = t549 ** 2
      t565 = log(-0.4D1 * t172 * t559 * x4 * t560)
      t567 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t568 = t555 * t567
      t572 = t556 * t567
      t582 = log(-0.4D1 * t202 * t391 * t152 * t560)
      t583 = t582 * t542
      t589 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t593 = t582 ** 2
      t604 = (t81 * (t558 - t565 * t542 * t568) - 0.180D3 * t532 * t572)
     # * t122 * t169 / 0.2880D4 + (-0.180D3 * t63 * t22 * (t558 - t583 *
     # t568) + t81 * (t556 * t589 - t583 * t555 * t557 + t593 * t542 * t
     #568 / 0.2D1) + t519 * t572) * t122 * t168 / 0.2880D4
      t605 = FJET(XB1, XB2, s, t543 * t544, -t543 * t546, 0.0D0, -t543 *
     # t478, -s * t549 * t485 * t487 * x3, t604)
      t607 = KAPPA2(x1, x2, x3, x4, z)
      t608 = s * t607
      t613 = t607 ** 2
      t618 = cos(t7)
      t621 = sqrt(x3 * t152 * t328)
      t628 = 0.1D1 / (-0.2D1 + t607)
      t629 = t607 * t628
      t630 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t632 = t613 ** 2
      t637 = log(0.4D1 * t172 * t328 * t559 * t632)
      t639 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t647 = -t81 * (t629 * t630 - t637 * t607 * t628 * t639) + 0.180D3 
     #* t532 * t629 * t639
      t651 = FJET(XB1, XB2, s, t608 * t544, -t608 * t546, -t608 * t479, 
     #t608 * t481, s * t613 * t485 * t487 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t618 * t621), t647 * t122 * t169 / 0.2880D4)
      gbgbH2n2e1 = t273 * t272 + t384 * t383 + t472 * t471 + t540 * t539
     # + t605 * t604 + t651 * t647 * t122 * t168 * t93 / 0.2880D4

      end function



      doubleprecision function gbgbH2n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * x4
      t18 = log(0.4D1 * t12 * t15)
      t19 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t23 = lh * t1
      t24 = t5 * t19
      t28 = 0.1D1 / x1
      t30 = 0.1D1 / x4
      t33 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t34 = t33 - t19
      t36 = 0.1D1 / x3
      t38 = t28 * t36 * t30
      t41 = x3 * t8
      t42 = t11 * t14
      t45 = log(0.4D1 * t41 * t42)
      t47 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t48 = -0.1D1 + x3
      t52 = log(-0.4D1 * t41 * t42 * t48)
      t63 = t12 * t14
      t65 = log(0.4D1 * t63)
      t71 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t73 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t1
      t84 = t83 * t24
      t90 = log(0.4D1 * t42 * x4)
      t97 = t90 ** 2
      t105 = x3 * t11
      t108 = log(0.4D1 * t105 * t15)
      t110 = t14 * t48
      t114 = log(-0.4D1 * t105 * t110 * x4)
      t119 = -t5 * t34
      t128 = log(-0.4D1 * t105 * t110)
      t130 = t105 * t14
      t132 = log(0.4D1 * t130)
      t139 = t132 ** 2
      t142 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t144 = t128 ** 2
      t155 = log(0.4D1 * t42)
      t164 = t155 ** 2
      t184 = (t6 * (-t7 + t18 * t19) + 0.180D3 * t23 * t24) * t28 * t30 
     #/ 0.2880D4 + t6 * t34 * t38 / 0.2880D4 + (t6 * (-t7 + t45 * t19 + 
     #t47 - t52 * t33) - 0.180D3 * t23 * t5 * t34) * t28 * t36 / 0.2880D
     #4 - (-0.180D3 * t23 * t5 * (t7 - t65 * t19) + t6 * (t71 - t65 * t7
     # + t73 * t19 / 0.2D1) + t84) * t28 / 0.2880D4 - (-0.180D3 * t23 * 
     #t5 * (t7 - t90 * t19) + t6 * (t71 - t90 * t7 + t97 * t19 / 0.2D1) 
     #+ t84) * t30 / 0.5760D4 - (t6 * (t7 - t108 * t19 - t47 + t114 * t3
     #3) - 0.180D3 * t23 * t119) * t36 * t30 / 0.5760D4 - (-0.180D3 * t2
     #3 * t5 * (-t47 + t128 * t33 + t7 - t132 * t19) + t6 * (t71 - t132 
     #* t7 + t139 * t19 / 0.2D1 - t142 + t128 * t47 - t144 * t33 / 0.2D1
     #) + t83 * t119) * t36 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t155)
     # * t1 * t5 * t71 / 0.5760D4 - (t79 - t81 + 0.180D3 * t155 * lh + 0
     #.45D2 * t164) * t1 * t5 * t7 / 0.5760D4 - (-0.2884936567583026D3 -
     # 0.120D3 * t78 * lh + 0.60D2 * lh * t80 - t155 * t82 - 0.90D2 * t1
     #64 * lh - 0.15D2 * t164 * t155) * t1 * t24 / 0.5760D4
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t184)
      t188 = -0.1D1 + x4
      t190 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t191 = t15 * t188
      t194 = log(-0.4D1 * t12 * t191)
      t195 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t199 = t5 * t195
      t206 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t207 = t195 - t206
      t211 = x4 * t188
      t214 = log(-0.4D1 * t42 * t211)
      t220 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t222 = t214 ** 2
      t233 = log(-0.4D1 * t105 * t191)
      t235 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t239 = log(0.4D1 * t130 * t211 * t48)
      t251 = (t6 * (t190 - t194 * t195) - 0.180D3 * t23 * t199) * t28 * 
     #t30 / 0.2880D4 + t6 * t207 * t38 / 0.2880D4 - (0.180D3 * t23 * t5 
     #* (t190 - t214 * t195) - t6 * (t220 - t214 * t190 + t222 * t195 / 
     #0.2D1) - t83 * t199) * t30 / 0.5760D4 - (t6 * (-t190 + t233 * t195
     # + t235 - t239 * t206) + 0.180D3 * t23 * t5 * t207) * t36 * t30 / 
     #0.5760D4
      t252 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t188, 0.0D0,
     # t251)
      t255 = -0.1D1 + x1
      t257 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t258 = t255 ** 2
      t259 = t8 * t258
      t263 = log(0.4D1 * t42 * t259 * x4)
      t264 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t268 = t5 * t264
      t270 = 0.180D3 * t23 * t268
      t280 = log(0.4D1 * t105 * t14 * t8 * t258)
      t289 = log(0.4D1 * t42 * t259)
      t295 = gbgbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t297 = t289 ** 2
      t306 = (t6 * (t257 - t263 * t264) - t270) * t28 * t30 / 0.2880D4 +
     # t6 * t264 * t38 / 0.2880D4 + (t6 * (t257 - t280 * t264) - t270) *
     # t28 * t36 / 0.2880D4 - (0.180D3 * t23 * t5 * (t257 - t289 * t264)
     # - t6 * (t295 - t289 * t257 + t297 * t264 / 0.2D1) - t83 * t268) *
     # t28 / 0.2880D4
      t307 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t255, 0.0D0,
     # t306)
      t309 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t310 = s * t309
      t311 = t1 * x1
      t313 = t1 * t255
      t314 = t313 * x4
      t316 = t313 * t188
      t318 = t309 ** 2
      t320 = t1 ** 2
      t322 = x1 * t255
      t326 = 0.1D1 / (-0.2D1 + t309)
      t327 = t309 * t326
      t328 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t330 = t318 ** 2
      t335 = log(-0.4D1 * t63 * t211 * t330 * t258)
      t337 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t342 = t23 * t5
      t351 = t36 * t30
      t355 = (t6 * (t327 * t328 - t335 * t309 * t326 * t337) - 0.180D3 *
     # t342 * t327 * t337) * t28 * t30 / 0.2880D4 + t6 * t327 * t337 * t
     #28 * t351 / 0.2880D4
      t356 = FJET(XB1, XB2, s, 0.0D0, t310 * t311, -t310 * t314, t310 * 
     #t316, -s * t318 * t320 * t322 * x4, t355)
      t358 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t359 = s * t358
      t360 = t311 * x3
      t362 = t311 * t48
      t365 = t358 ** 2
      t371 = 0.1D1 / (-0.2D1 + t358)
      t372 = t358 * t371
      t374 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t378 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t380 = t365 ** 2
      t385 = log(-0.4D1 * t130 * t259 * t48 * t380)
      t398 = t6 * t372 * t374 * t28 * t351 / 0.2880D4 + (t6 * (t372 * t3
     #78 - t385 * t358 * t371 * t374) - 0.180D3 * t342 * t372 * t374) * 
     #t28 * t36 / 0.2880D4
      t399 = FJET(XB1, XB2, s, t359 * t360, -t359 * t362, 0.0D0, -t359 *
     # t313, -s * t365 * t320 * t322 * x3, t398)
      t401 = KAPPA2(x1, x2, x3, x4, z)
      t402 = s * t401
      t407 = t401 ** 2
      t412 = cos(t9)
      t415 = sqrt(x3 * t48 * t211)
      t422 = 0.1D1 / (-0.2D1 + t401)
      t425 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t430 = FJET(XB1, XB2, s, t402 * t360, -t402 * t362, -t402 * t314, 
     #t402 * t316, s * t407 * t320 * t322 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t412 * t415), -t6 * t401 * t422 * t425 * t28 * t351 / 0.
     #2880D4)
      gbgbH2n2e0 = t185 * t184 + t252 * t251 + t307 * t306 + t356 * t355
     # + t399 * t398 - t430 * t3 * t5 * t401 * t422 * t425 * t38 / 0.288
     #0D4

      end function



      doubleprecision function gbgbH2n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t8 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t9 = -t7 + t8
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x4
      t16 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t18 = sin(x2 * 0.3141592653589793D1)
      t19 = t18 ** 2
      t20 = x3 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t23 = -0.1D1 + x3
      t27 = log(-0.4D1 * t20 * t22 * t23)
      t29 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t32 = log(0.4D1 * t20 * t22)
      t36 = lh * t1
      t43 = gbgbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t47 = t19 * t22
      t49 = log(0.4D1 * t47)
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t62 = t49 ** 2
      t66 = t5 * t8
      t69 = x1 ** 2
      t73 = log(0.4D1 * t69 * t19 * t22)
      t78 = 0.180D3 * t36 * t66
      t80 = 0.1D1 / x1
      t94 = log(0.4D1 * t47 * x4)
      t101 = -t6 * t9 * t10 * t12 / 0.5760D4 - (t6 * (-t16 + t27 * t7 + 
     #t29 - t32 * t8) - 0.180D3 * t36 * t5 * t9) * t10 / 0.5760D4 - t6 *
     # t43 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t49) * t1 * t5 * t29 /
     # 0.5760D4 - (0.180D3 * t56 - 0.30D2 * t58 + 0.180D3 * t49 * lh + 0
     #.45D2 * t62) * t1 * t66 / 0.5760D4 - (t6 * (t29 - t73 * t8) - t78)
     # * t80 / 0.2880D4 - t6 * t8 * t80 * t12 / 0.2880D4 - t6 * t9 * t80
     # * t10 / 0.2880D4 - (t6 * (t29 - t94 * t8) - t78) * t12 / 0.5760D4
      t102 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t101)
      t105 = -0.1D1 + x4
      t107 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t112 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t116 = log(-0.4D1 * t47 * x4 * t105)
      t126 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t132 = t6 * t107 * t80 * t12 / 0.2880D4 - (-t6 * (t112 - t116 * t1
     #07) + 0.180D3 * t36 * t5 * t107) * t12 / 0.5760D4 - t6 * (t126 - t
     #107) * t10 * t12 / 0.5760D4
      t133 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t105, 0.0D0,
     # t132)
      t136 = -0.1D1 + x1
      t138 = gbgbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t139 = t136 ** 2
      t143 = log(0.4D1 * t47 * t69 * t139)
      t144 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t153 = t144 * t80
      t159 = -(-t6 * (t138 - t143 * t144) + 0.180D3 * t36 * t5 * t144) *
     # t80 / 0.2880D4 + t6 * t153 * t12 / 0.2880D4 + t6 * t153 * t10 / 0
     #.2880D4
      t160 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t136, 0.0D0,
     # t159)
      t162 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t163 = s * t162
      t164 = t1 * x1
      t166 = t1 * t136
      t171 = t162 ** 2
      t173 = t1 ** 2
      t175 = x1 * t136
      t181 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t184 = 0.1D1 / (-0.2D1 + t162) * t181 * t80 * t12
      t187 = FJET(XB1, XB2, s, 0.0D0, t163 * t164, -t163 * t166 * x4, t1
     #63 * t166 * t105, -s * t171 * t173 * t175 * x4, t6 * t162 * t184 /
     # 0.2880D4)
      t193 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t194 = s * t193
      t200 = t193 ** 2
      t208 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t211 = 0.1D1 / (-0.2D1 + t193) * t208 * t80 * t10
      t214 = FJET(XB1, XB2, s, t194 * t164 * x3, -t194 * t164 * t23, 0.0
     #D0, -t194 * t166, -s * t200 * t173 * t175 * x3, t6 * t193 * t211 /
     # 0.2880D4)
      gbgbH2n2em1 = t102 * t101 + t133 * t132 + t160 * t159 + t187 * t3 
     #* t5 * t162 * t184 / 0.2880D4 + t214 * t3 * t5 * t193 * t211 / 0.2
     #880D4

      end function



      doubleprecision function gbgbH2n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t18 = gbgbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t23 = sin(x2 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t25 = z ** 2
      t29 = log(0.4D1 * t24 / t25)
      t36 = 0.1D1 / x4
      t40 = -t6 * t7 * t8 / 0.2880D4 - t6 * (-t12 + t7) / x3 / 0.5760D4 
     #- t6 * t18 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t29) * t1 * t5 *
     # t7 / 0.5760D4 - t6 * t7 * t36 / 0.5760D4
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t40)
      t46 = gbgbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t50 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, t6 * t46 * t8 / 0.2880D4)
      t59 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t6 * t59 * t36 / 0.5760D4)
      gbgbH2n2em2 = t41 * t40 + t50 * t3 * t5 * t46 * t8 / 0.2880D4 + t6
     #3 * t3 * t5 * t59 * t36 / 0.5760D4

      end function



      doubleprecision function gbgbH2n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      t1 = -0.1D1 + z
      t3 = 0.90D2 * t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = gbgbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, -t3 * 
     #t5 * t7 / 0.5760D4)
      gbgbH2n2em3 = -t10 * t3 * t5 * t7 / 0.5760D4

      end function



      doubleprecision function gbgbH2n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH21J1
      doubleprecision gbgbH21J2
      doubleprecision gbgbH21J3
      gbgbH2n2em4 = 0.0D0

      end function
  
 

      doubleprecision function gbgbH21J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = 0.1D1 - x1
      t12 = t6 * t9 * t10
      t13 = x1 ** 2
      t14 = x4 * t13
      t15 = 0.1D1 - x3
      t16 = t15 ** 2
      t17 = s * t3
      t18 = t7 * x1
      t21 = t18 * t15
      t23 = s - t17 * t18 * x3 - t17 * t21
      t24 = t23 ** 2
      t25 = t16 * t24
      t29 = t24 * t2
      t30 = z ** 2
      t35 = t1 ** 2
      t36 = t35 * t4
      t37 = t8 * t10
      t38 = t36 * t37
      t39 = x4 * x1
      t40 = t15 * t23
      t45 = t2 * t3 * t7
      t46 = x1 * x3
      t50 = t29 * t4
      t51 = t8 * t13
      t56 = t35 * t3 * t7
      t57 = x1 * t15
      t61 = t29 * t3
      t64 = t2 * t4
      t65 = t64 * t8
      t66 = x3 ** 2
      t67 = t13 * t66
      t71 = t10 ** 2
      t72 = x4 ** 2
      t77 = t10 * x4
      t82 = t13 * x1
      t83 = t9 * t82
      t84 = t66 * x3
      t88 = t9 * t71
      t89 = t6 * t88
      t90 = x4 * t24
      t94 = cos(x2 * 0.3141592653589793D1)
      t99 = sqrt(x3 * t15 * x4 * (0.1D1 - x4))
      t102 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t94 * t99
      t108 = t46 * z
      t113 = t29 * t5 * t9
      t114 = t13 * x3
      t116 = z * t10 * t102
      t120 = t23 * t35
      t123 = t9 * t13
      t125 = t15 * t10 * t102
      t129 = 0.36D2 * t12 * t14 * t25 - 0.18D2 * t29 * t30 + 0.18D2 * t2
     #9 * z - 0.36D2 * t38 * t39 * t40 + 0.21D2 * t45 * t46 * t24 - 0.18
     #D2 * t50 * t51 * t16 + 0.9D1 * t56 * t57 * t23 + 0.9D1 * t61 * t21
     # - 0.15D2 * t65 * t67 * t24 - 0.9D1 * t65 * t71 * t72 * t24 + 0.18
     #D2 * t45 * t77 * t24 + 0.3D1 * t29 * t5 * t83 * t84 + 0.18D2 * t89
     # * t90 * x1 * t102 - 0.8D1 * t38 * x4 * t23 * t108 - 0.3D1 * t113 
     #* t114 * t116 - 0.18D2 * t120 * z * t5 * t123 * t125
      t131 = t4 ** 2
      t132 = t131 * t3
      t133 = t35 * t132
      t134 = t8 ** 2
      t135 = t134 * t7
      t136 = t13 ** 2
      t144 = t10 * t102
      t150 = t82 * t66
      t154 = t2 * t131
      t155 = t134 * t71
      t156 = t154 * t155
      t157 = t15 * t24
      t162 = t35 * t131
      t168 = t36 * t51
      t169 = x3 * t23
      t177 = t35 * s * z * t4 * t8
      t181 = t35 * t5
      t183 = t72 * x1
      t189 = t3 * t7 * t57
      t192 = t15 * z
      t200 = t24 * z
      t205 = t8 * x1 * t144
      t215 = -0.9D1 * t29 - t133 * t135 * t136 * t40 * t66 * t10 * t102 
     #+ 0.36D2 * t113 * t13 * t15 * t144 - 0.3D1 * t29 * t131 * t134 * t
     #150 * t144 - 0.18D2 * t156 * t14 * t157 * t102 + 0.18D2 * t162 * t
     #155 * t14 * t40 * t102 - 0.4D1 * t168 * t169 * t15 * t30 + 0.8D1 *
     # t177 * t77 * t46 + 0.18D2 * t181 * t88 * t183 * t40 + 0.18D2 * t1
     #20 * z * t189 - 0.36D2 * t61 * t18 * t192 - 0.8D1 * t56 * t46 * t2
     #3 * z - 0.18D2 * t45 * t77 * t200 - 0.18D2 * t50 * t205 - 0.16D2 *
     # t45 * t46 * t200 - 0.16D2 * t56 * t46 * t23 * t30
      t230 = t102 ** 2
      t231 = t71 * t230
      t249 = t6 * t9
      t250 = t82 * x3
      t254 = t66 * z
      t261 = t64 * t37
      t276 = t181 * t123
      t280 = t134 * t82
      t286 = 0.18D2 * t120 * t30 * t189 + 0.4D1 * t61 * t18 * x3 * t30 -
     # 0.19D2 * t36 * t8 * t114 * t40 - 0.9D1 * t29 * t131 * t134 * t13 
     #* t231 + 0.11D2 * t181 * t9 * t82 * t15 * t23 * t66 - t162 * t134 
     #* t136 * t15 * t23 * t84 - 0.14D2 * t65 * t114 * t157 + 0.6D1 * t2
     #49 * t250 * t25 + 0.6D1 * t50 * t51 * t254 + 0.6D1 * t249 * t150 *
     # t157 + 0.6D1 * t261 * t90 * t108 + 0.36D2 * t261 * t39 * t157 * z
     # - 0.72D2 * t38 * t39 * t40 * z - 0.36D2 * t177 * t77 * t57 + 0.8D
     #1 * t276 * t169 * t116 + 0.3D1 * t162 * t280 * x3 * t40 * t116
      t298 = x3 * t24
      t353 = -0.30D2 * t168 * t169 * t192 - 0.18D2 * t276 * t40 * t144 +
     # 0.3D1 * t181 * t83 * t40 * t254 + 0.12D2 * t64 * t51 * t298 * t19
     #2 - 0.24D2 * t261 * t90 * t46 + 0.11D2 * t162 * t280 * t169 * t125
     # + 0.18D2 * t29 * z * t4 * t205 + 0.3D1 * t89 * t72 * t24 * t46 + 
     #0.9D1 * t89 * t183 * t157 + 0.6D1 * t12 * t90 * t67 + 0.21D2 * t6 
     #* t123 * t298 * t144 - 0.6D1 * t154 * t280 * t298 * t125 - t35 * t
     #131 * t4 * t134 * t8 * t136 * t169 * t15 * t71 * t230 + 0.9D1 * t1
     #33 * t135 * t82 * t40 * t231 - 0.3D1 * t156 * t90 * t114 * t102 + 
     #t29 * t132 * t135 * t250 * t231
      gbgbH21J1 = -0.16D2 / 0.3D1 * wd * (t129 + t215 + t286 + t353) / t
     #24 / s

      end function
  
   
 

      doubleprecision function gbgbH21J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
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
      t14 = s * t3
      t15 = t7 * x1
      t18 = 0.1D1 - x3
      t19 = t18 * t15
      t21 = s - t14 * t15 * x3 - t14 * t19
      t22 = t21 ** 2
      t23 = x4 * t22
      t27 = cos(x2 * 0.3141592653589793D1)
      t32 = sqrt(x3 * t18 * x4 * (0.1D1 - x4))
      t35 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t27 * t32
      t40 = t1 ** 2
      t44 = t40 * s * z * t4 * t8
      t45 = t10 * x4
      t46 = x1 * t18
      t50 = t40 * t4
      t51 = t8 * t10
      t52 = t50 * t51
      t53 = x4 * t21
      t54 = x1 * x3
      t55 = t54 * z
      t58 = t22 * t2
      t60 = t58 * t5 * t9
      t61 = x1 ** 2
      t62 = t61 * x3
      t64 = z * t10 * t35
      t67 = t40 * t5
      t68 = t9 * t61
      t69 = t67 * t68
      t70 = x3 * t21
      t74 = t4 ** 2
      t75 = t40 * t74
      t76 = t8 ** 2
      t77 = t61 * x1
      t78 = t76 * t77
      t81 = t21 * t18
      t84 = t8 * t61
      t86 = t18 * z
      t90 = t2 * t74
      t91 = t76 * t11
      t92 = t90 * t91
      t93 = t62 * t35
      t97 = t74 * t3
      t98 = t76 * t7
      t101 = t77 * x3
      t102 = t35 ** 2
      t103 = t11 * t102
      t108 = x3 ** 2
      t109 = t77 * t108
      t110 = t10 * t35
      t114 = x4 * t61
      t115 = t18 * t22
      t121 = x3 * t22
      t123 = t18 * t10 * t35
      t130 = t61 ** 2
      t137 = -0.18D2 * t13 * t23 * x1 * t35 + 0.36D2 * t44 * t45 * t46 -
     # t52 * t53 * t55 + t60 * t62 * t64 - 0.8D1 * t69 * t70 * t64 - t75
     # * t78 * x3 * t81 * t64 + 0.11D2 * t50 * t84 * t70 * t86 + 0.2D1 *
     # t92 * t23 * t93 - t58 * t97 * t98 * t101 * t103 + 0.3D1 * t58 * t
     #74 * t76 * t109 * t110 + 0.18D2 * t92 * t114 * t115 * t35 + 0.6D1 
     #* t90 * t78 * t121 * t123 + t40 * t74 * t4 * t76 * t8 * t130 * t70
     # * t18 * t11 * t102
      t138 = t40 * t97
      t145 = t6 * t9 * t10
      t146 = t18 ** 2
      t147 = t146 * t22
      t151 = t67 * t12
      t152 = x4 ** 2
      t153 = t152 * x1
      t173 = t75 * t91
      t194 = -0.9D1 * t138 * t98 * t77 * t81 * t103 - 0.36D2 * t145 * t1
     #14 * t147 - 0.18D2 * t151 * t153 * t81 + 0.36D2 * t52 * x4 * x1 * 
     #t81 - 0.2D1 * t13 * t152 * t22 * t54 - 0.9D1 * t13 * t153 * t115 +
     # 0.9D1 * t58 - 0.22D2 * t6 * t68 * t121 * t110 - 0.8D1 * t173 * t5
     #3 * t93 - t151 * t152 * t21 * t54 - 0.12D2 * t75 * t78 * t70 * t12
     #3 + t138 * t98 * t130 * t81 * t108 * t10 * t35 - 0.36D2 * t60 * t6
     #1 * t18 * t110
      t199 = t9 * t77
      t201 = t108 * z
      t204 = t61 * t108
      t208 = t2 * t4
      t213 = t208 * t51
      t226 = t58 * t4
      t247 = t2 * t3 * t7
      t252 = 0.27D2 * t69 * t81 * t110 - t67 * t199 * t81 * t201 - 0.5D1
     # * t145 * t23 * t204 - 0.4D1 * t208 * t84 * t121 * t86 + 0.24D2 * 
     #t213 * t23 * t54 - 0.18D2 * t173 * t114 * t81 * t35 - 0.16D2 * t44
     # * t45 * t54 - t213 * t23 * t55 + 0.27D2 * t226 * t8 * x1 * t110 -
     # 0.11D2 * t67 * t9 * t77 * t18 * t21 * t108 + 0.9D1 * t58 * t74 * 
     #t76 * t61 * t103 + 0.19D2 * t50 * t8 * t62 * t81 + 0.11D2 * t247 *
     # t54 * t22 * z
      t253 = t208 * t8
      t259 = t108 * x3
      t263 = t6 * t9
      t274 = t40 * t3 * t7
      t305 = 0.14D2 * t253 * t62 * t115 + t75 * t76 * t130 * t18 * t21 *
     # t259 - 0.6D1 * t263 * t101 * t147 - 0.2D1 * t226 * t84 * t201 - 0
     #.6D1 * t263 * t109 * t115 + 0.16D2 * t274 * t54 * t21 * z - 0.22D2
     # * t247 * t54 * t22 + 0.18D2 * t226 * t84 * t146 - 0.9D1 * t274 * 
     #t46 * t21 - 0.9D1 * t58 * t3 * t19 + 0.16D2 * t253 * t204 * t22 + 
     #0.9D1 * t253 * t11 * t152 * t22 - 0.9D1 * t247 * t45 * t22 - 0.3D1
     # * t58 * t5 * t199 * t259
      gbgbH21J2 = -0.16D2 / 0.3D1 * wd * (t137 + t194 + t252 + t305) / t
     #22 / s

      end function
  
   
 

      doubleprecision function gbgbH21J3
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
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = x1 ** 2
      t9 = t8 * t7
      t11 = s * t3
      t12 = t6 * x1
      t15 = 0.1D1 - x3
      t18 = s - t11 * t12 * x3 - t11 * t12 * t15
      t19 = x3 * t18
      t24 = t4 ** 2
      t25 = t24 * t2
      t26 = t7 ** 2
      t27 = t8 * x1
      t30 = 0.1D1 - x1
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = sqrt(x3 * t15 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t51 = t8 ** 2
      t54 = t18 * t15
      t55 = x3 ** 2
      t60 = t18 ** 2
      t61 = s * t1
      t62 = t60 * t61
      t66 = t30 * t43
      t69 = t4 * t3
      t70 = t2 * t69
      t71 = t7 * t6
      t72 = t30 ** 2
      t73 = t71 * t72
      t75 = x4 ** 2
      t77 = x1 * x3
      t80 = t61 * t4
      t82 = t8 * t55
      t86 = t26 * t72
      t88 = x4 * t18
      t89 = x3 * t8
      t90 = t89 * t43
      t104 = t7 * t30
      t106 = t77 * z
      t110 = t80 * t104
      t111 = x4 * t60
      t118 = 0.8D1 * t5 * t9 * t19 * t15 * z - 0.9D1 * t25 * t26 * t27 *
     # t19 * t15 * t30 * t43 + t2 * t24 * t3 * t26 * t6 * t51 * t54 * t5
     #5 * t30 * t43 + t62 * t24 * t26 * t27 * t55 * t66 + t70 * t73 * t7
     #5 * t18 * t77 + 0.11D2 * t80 * t7 * t82 * t60 + 0.8D1 * t25 * t86 
     #* t88 * t90 + 0.9D1 * t5 * t7 * t89 * t54 - 0.8D1 * t2 * t3 * t6 *
     # t77 * t18 * z - 0.7D1 * t5 * t104 * t88 * t106 - t110 * t111 * t1
     #06 + t61 * t24 * t86 * t111 * t90
      t126 = t61 * t3 * t6
      t131 = t55 * z
      t138 = t61 * t69
      t143 = t71 * t27
      t157 = t55 * x3
      t179 = -0.10D2 * t70 * t71 * t27 * t15 * t18 * t55 - 0.9D1 * t126 
     #* t77 * t60 - t62 * t4 * t9 * t131 + 0.8D1 * t126 * t77 * t60 * z 
     #- t138 * t73 * t75 * t60 * t77 - t70 * t143 * t54 * t131 + 0.9D1 *
     # t110 * t111 * t77 - 0.9D1 * t138 * t71 * t8 * x3 * t60 * t66 - 0.
     #2D1 * t62 * t69 * t143 * t157 + 0.8D1 * t2 * s * z * t4 * t7 * t30
     # * x4 * t77 - 0.3D1 * t138 * t71 * t30 * t111 * t82 + t25 * t26 * 
     #t51 * t15 * t18 * t157
      gbgbH21J3 = -0.16D2 / 0.3D1 * wd * (t118 + t179) / t60 / s

      end function
  
 