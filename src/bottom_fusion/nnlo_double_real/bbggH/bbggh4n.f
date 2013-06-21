  
      subroutine bbggh4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh41J1  
      doubleprecision bbggh41J2  
      doubleprecision bbggh41J3  
      doubleprecision bbggh4n1e1  
      doubleprecision bbggh4n1e0  
      doubleprecision bbggh4n1em1  
      doubleprecision bbggh4n1em2  
      doubleprecision bbggh4n1em3  
      doubleprecision bbggh4n1em4  
      doubleprecision bbggh4n2e1  
      doubleprecision bbggh4n2e0  
      doubleprecision bbggh4n2em1  
      doubleprecision bbggh4n2em2  
      doubleprecision bbggh4n2em3  
      doubleprecision bbggh4n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh4n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh4n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh4n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh4n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh4n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh4n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t20 = t17 ** 2
      t21 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t27 = lh ** 2
      t29 = 0.3141592653589793D1 ** 2
      t31 = 0.180D3 * t27 - 0.30D2 * t29
      t32 = t31 * t5
      t49 = -0.2884936567583026D3 - 0.120D3 * t27 * lh + 0.60D2 * lh * t
     #29
      t50 = t49 * t5
      t51 = t50 * t21
      t53 = 0.1D1 / x1
      t56 = t8 * x3
      t57 = t11 * t14
      t58 = -0.1D1 + x3
      t59 = 0.1D1 / t58
      t60 = t57 * t59
      t63 = log(-0.4D1 * t56 * t60)
      t66 = cos(t9)
      t68 = Sqrt(-x3 * t58)
      t72 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t66 * t68)
      t76 = log(0.4D1 * t56 * t57)
      t82 = t63 ** 2
      t88 = t76 ** 2
      t95 = -t21 - t21 * t72
      t96 = t32 * t95
      t98 = 0.1D1 / x3
      t102 = x2 ** 2
      t103 = x3 * t102
      t104 = t103 * t8
      t107 = log(-0.4D1 * t104 * t60)
      t111 = t103 * t15
      t113 = log(0.4D1 * t111)
      t123 = 0.1D1 / x2
      t124 = t123 * t53
      t127 = t102 * t8
      t130 = log(0.4D1 * t127 * t57)
      t136 = t130 ** 2
      t142 = t32 * t21
      t148 = log(0.4D1 * t57)
      t149 = t148 ** 2
      t152 = t149 * t148
      t155 = t149 ** 2
      t162 = t29 ** 2
      t163 = t27 ** 2
      t190 = x3 * t11
      t193 = log(0.4D1 * t190 * t14)
      t194 = t193 ** 2
      t198 = log(-0.4D1 * t190 * t14 * t59)
      t199 = t198 ** 2
      t229 = t102 * t11
      t232 = log(0.4D1 * t229 * t14)
      t234 = t232 ** 2
      t257 = log(0.4D1 * t103 * t57)
      t261 = log(-0.4D1 * t103 * t60)
      t269 = t261 ** 2
      t275 = t257 ** 2
      t285 = (-0.180D3 * t6 * (-t7 + t17 * t18 - t20 * t21 / 0.2D1) + t3
     #2 * (-t18 + t17 * t21) + 0.90D2 * t5 * (t17 * t7 - t20 * t18 / 0.2
     #D1 + t20 * t17 * t21 / 0.6D1) - t51) * t53 / 0.5760D4 + (-0.180D3 
     #* t6 * (-(t18 - t63 * t21) * t72 - t18 + t76 * t21) + 0.90D2 * t5 
     #* (-(t7 - t63 * t18 + t82 * t21 / 0.2D1) * t72 - t7 + t76 * t18 - 
     #t88 * t21 / 0.2D1) + t96) * t98 * t53 / 0.5760D4 - (0.90D2 * t5 * 
     #((t18 - t107 * t21) * t72 + t18 - t113 * t21) + 0.180D3 * t6 * t95
     #) * t98 * t124 / 0.2880D4 - (-0.180D3 * t6 * (t18 - t130 * t21) + 
     #0.90D2 * t5 * (t7 - t130 * t18 + t136 * t21 / 0.2D1) + t142) * t12
     #3 * t53 / 0.2880D4 - (0.45D2 * t149 * t7 - 0.15D2 * t152 * t18 + 0
     #.15D2 / 0.4D1 * t155 * t21 + (t18 - t148 * t21) * t49 + t21 * (0.5
     #769873135166051D3 * lh + t162 + 0.60D2 * t163 - 0.60D2 * t27 * t29
     #) + (t7 - t148 * t18 + t149 * t21 / 0.2D1) * t31 - 0.180D3 * (-t14
     #8 * t7 + t149 * t18 / 0.2D1 - t152 * t21 / 0.6D1) * lh) * t5 / 0.1
     #1520D5 - ((0.90D2 * t5 * t18 - 0.180D3 * t6 * t21) * (t194 / 0.2D1
     # + t199 * t72 / 0.2D1) + (0.90D2 * t5 * t7 - 0.180D3 * t6 * t18 + 
     #t142) * (-t198 * t72 - t193) + 0.90D2 * t5 * t21 * (-t194 * t193 /
     # 0.6D1 - t199 * t198 * t72 / 0.6D1) + (-0.180D3 * t6 * t7 + t32 * 
     #t18 + t51) * (t72 + 0.1D1)) * t98 / 0.11520D5 + (-0.180D3 * t6 * (
     #-t7 + t232 * t18 - t234 * t21 / 0.2D1) + t32 * (-t18 + t232 * t21)
     # + 0.90D2 * t5 * (t232 * t7 - t234 * t18 / 0.2D1 + t234 * t232 * t
     #21 / 0.6D1) - t51) * t123 / 0.5760D4 + (-0.180D3 * t6 * (-t18 + t2
     #57 * t21 - (t18 - t261 * t21) * t72) + 0.90D2 * t5 * (-(t7 - t261 
     #* t18 + t269 * t21 / 0.2D1) * t72 - t7 + t257 * t18 - t275 * t21 /
     # 0.2D1) + t96) * t98 * t123 / 0.5760D4
      t286 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t285)
      t288 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t285)
      t290 = t2 * x1
      t291 = -0.1D1 + x1
      t292 = t2 * t291
      t293 = -t291
      t294 = bbggh41J3(s, XB1, XB2, z, lh, wd, t293, 0.0D0, 0.0D0, x4)
      t295 = x1 * z
      t296 = 0.1D1 - x1 + t295
      t297 = 0.1D1 / t296
      t298 = t14 * t297
      t299 = t291 ** 2
      t300 = t298 * t299
      t303 = log(0.4D1 * t12 * t300)
      t304 = bbggh41J2(s, XB1, XB2, z, lh, wd, t293, 0.0D0, 0.0D0, x4)
      t306 = t303 ** 2
      t307 = bbggh41J1(s, XB1, XB2, z, lh, wd, t293, 0.0D0, 0.0D0, x4)
      t329 = t56 * t11
      t331 = t298 * t299 * t59
      t334 = log(-0.4D1 * t329 * t331)
      t337 = x1 * x3
      t338 = t337 * z
      t341 = x3 * t296
      t343 = Sqrt(-t341 * t58)
      t347 = 0.1D1 / (-0.2D1 * t338 + 0.2D1 * t337 - 0.1D1 - x3 + 0.2D1 
     #* t66 * t343)
      t351 = log(0.4D1 * t329 * t300)
      t357 = t334 ** 2
      t363 = t351 ** 2
      t370 = t307 * t347 + t307
      t376 = t297 * t299
      t380 = log(0.4D1 * t104 * t57 * t376)
      t382 = t103 * t12
      t385 = log(-0.4D1 * t382 * t331)
      t399 = t127 * t11
      t402 = log(0.4D1 * t399 * t300)
      t408 = t402 ** 2
      t419 = (-0.180D3 * t6 * (t294 - t303 * t304 + t306 * t307 / 0.2D1)
     # + t32 * (t304 - t303 * t307) + 0.90D2 * t5 * (-t303 * t294 + t306
     # * t304 / 0.2D1 - t306 * t303 * t307 / 0.6D1) + t50 * t307) * t53 
     #/ 0.5760D4 + (-0.180D3 * t6 * ((t304 - t334 * t307) * t347 + t304 
     #- t351 * t307) + 0.90D2 * t5 * ((t294 - t334 * t304 + t357 * t307 
     #/ 0.2D1) * t347 + t294 - t351 * t304 + t363 * t307 / 0.2D1) + t32 
     #* t370) * t98 * t53 / 0.5760D4 - (0.90D2 * t5 * (-t304 + t380 * t3
     #07 - (t304 - t385 * t307) * t347) + 0.180D3 * t6 * t370) * t98 * t
     #124 / 0.2880D4 - (-0.180D3 * t6 * (-t304 + t402 * t307) + 0.90D2 *
     # t5 * (-t294 + t402 * t304 - t408 * t307 / 0.2D1) - t32 * t307) * 
     #t123 * t53 / 0.2880D4
      t420 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t290, -t292, 0.0D0, t419)
      t422 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t292, t290, 0.0D0, t419)
      t424 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t285)
      t427 = x2 * s * t1
      t428 = -0.1D1 + x2
      t429 = t428 * s
      t430 = t429 * t1
      t431 = x2 * z
      t433 = 0.1D1 / (0.1D1 + t431 - x2)
      t434 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t435 = t433 * t434
      t436 = t57 * t428
      t439 = log(-0.4D1 * t104 * t436)
      t441 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t446 = t433 * t441
      t455 = log(-0.4D1 * t127 * t436)
      t456 = t455 * t433
      t461 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t462 = t433 * t461
      t464 = t455 ** 2
      t471 = t32 * t446
      t476 = t14 * t428
      t479 = log(-0.4D1 * t229 * t476)
      t480 = t479 * t433
      t482 = t479 ** 2
      t483 = t482 * t433
      t508 = log(-0.4D1 * t103 * t436)
      t509 = t508 * t433
      t515 = t508 ** 2
      t526 = -(0.90D2 * t5 * (-t435 + t439 * t433 * t441) + 0.180D3 * t6
     # * t446) * t98 * t124 / 0.2880D4 - (-0.180D3 * t6 * (-t435 + t456 
     #* t441) + 0.90D2 * t5 * (-t462 + t456 * t434 - t464 * t433 * t441 
     #/ 0.2D1) - t471) * t123 * t53 / 0.2880D4 + (-0.180D3 * t6 * (t462 
     #- t480 * t434 + t483 * t441 / 0.2D1) + t32 * (t435 - t480 * t441) 
     #+ 0.90D2 * t5 * (-t480 * t461 + t483 * t434 / 0.2D1 - t482 * t479 
     #* t433 * t441 / 0.6D1) + t50 * t446) * t123 / 0.5760D4 + (-0.180D3
     # * t6 * (t435 - t509 * t441) + 0.90D2 * t5 * (t462 - t509 * t434 +
     # t515 * t433 * t441 / 0.2D1) + t471) * t98 * t123 / 0.5760D4
      t527 = FJET(XB1, XB2, s, 0.0D0, t427, 0.0D0, -t430, 0.0D0, t526)
      t529 = x2 * x3
      t532 = Sqrt(x3 * t428 * t58)
      t533 = t66 * t532
      t535 = 0.2D1 * t533 * x2
      t537 = 0.1D1 - x3 + t529
      t538 = 0.1D1 / t537
      t540 = t2 * (0.1D1 - x3 - x2 + t529 + t103 + t535) * t538
      t541 = 0.2D1 * t533
      t545 = t2 * x2 * (-0.1D1 + t529 + t541) * t538
      t546 = t103 * z
      t550 = 0.1D1 / (-0.1D1 - t431 - t103 - t535 + t529 + x2 - x3 + t54
     #6 + t541 + 0.2D1 * t533 * t431)
      t551 = t529 * t538
      t552 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t551, x4)
      t553 = t550 * t552
      t554 = t537 ** 2
      t555 = 0.1D1 / t554
      t557 = t476 * t58 * t555
      t560 = log(0.4D1 * t382 * t557)
      t562 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t551, x4)
      t567 = t550 * t562
      t577 = log(0.4D1 * t103 * t11 * t557)
      t578 = t577 * t550
      t583 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t551, x4)
      t586 = t577 ** 2
      t598 = -(0.90D2 * t5 * (-t553 + t560 * t550 * t562) + 0.180D3 * t6
     # * t567) * t98 * t124 / 0.2880D4 + (-0.180D3 * t6 * (t553 - t578 *
     # t562) + 0.90D2 * t5 * (t550 * t583 - t578 * t552 + t586 * t550 * 
     #t562 / 0.2D1) + t32 * t567) * t98 * t123 / 0.5760D4
      t599 = FJET(XB1, XB2, s, 0.0D0, t540, 0.0D0, -t545, 0.0D0, t598)
      t601 = FJET(XB1, XB2, s, 0.0D0, -t430, 0.0D0, t427, 0.0D0, t526)
      t603 = FJET(XB1, XB2, s, 0.0D0, -t545, 0.0D0, t540, 0.0D0, t598)
      t607 = t2 * t291 * x2 * t297
      t609 = t429 * t1 * t291
      t610 = t1 ** 2
      t615 = s * t610 * x2 * x1 * t291 * t297
      t616 = x2 * x1
      t617 = t616 * z
      t619 = 0.1D1 / (-0.1D1 - t431 - t616 + x1 - t295 + t617 + x2)
      t620 = bbggh41J2(s, XB1, XB2, z, lh, wd, t293, x2, 0.0D0, x4)
      t621 = t619 * t620
      t623 = t298 * t299 * t428
      t626 = log(-0.4D1 * t382 * t623)
      t628 = bbggh41J1(s, XB1, XB2, z, lh, wd, t293, x2, 0.0D0, x4)
      t635 = t619 * t628 * t296
      t643 = log(-0.4D1 * t399 * t623)
      t644 = t643 * t619
      t650 = bbggh41J3(s, XB1, XB2, z, lh, wd, t293, x2, 0.0D0, x4)
      t653 = t643 ** 2
      t666 = -(-0.90D2 * t5 * (t621 - t626 * t619 * t628) * t296 + 0.180
     #D3 * t6 * t635) * t98 * t124 / 0.2880D4 - (0.180D3 * t6 * (t621 - 
     #t644 * t628) * t296 - 0.90D2 * t5 * (t619 * t650 - t644 * t620 + t
     #653 * t619 * t628 / 0.2D1) * t296 - t32 * t635) * t123 * t53 / 0.2
     #880D4
      t667 = FJET(XB1, XB2, s, 0.0D0, -t607, t290, t609, -t615, t666)
      t669 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t285)
      t671 = FJET(XB1, XB2, s, t290, t609, 0.0D0, -t607, -t615, t666)
      t673 = t286 * t285 + t288 * t285 + t420 * t419 + t422 * t419 + t42
     #4 * t285 + t527 * t526 + t599 * t598 + t601 * t526 + t603 * t598 +
     # t667 * t666 + t669 * t285 + t671 * t666
      t674 = FJET(XB1, XB2, s, t290, -t292, 0.0D0, 0.0D0, 0.0D0, t419)
      t676 = FJET(XB1, XB2, s, t427, 0.0D0, -t430, 0.0D0, 0.0D0, t526)
      t678 = FJET(XB1, XB2, s, t540, 0.0D0, -t545, 0.0D0, 0.0D0, t598)
      t680 = FJET(XB1, XB2, s, t609, t290, -t607, 0.0D0, -t615, t666)
      t682 = t290 * t551
      t683 = t529 * x1
      t684 = t529 * t295
      t685 = t428 * t58
      t687 = Sqrt(t341 * t685)
      t688 = t66 * t687
      t689 = 0.2D1 * t688
      t694 = t292 * x2 * (t337 - t338 + t529 - t683 + t684 - 0.1D1 + t68
     #9) * t297 * t538
      t698 = t58 * s * t1 * x1 * t538
      t700 = 0.2D1 * t688 * x2
      t701 = 0.1D1 - x1 + t295 - x2 + t616 - t617 - x3 + t337 - t338 + t
     #529 - t683 + t684 + t103 + t700
      t704 = t292 * t701 * t297 * t538
      t731 = 0.1D1 + t295 - 0.3D1 * t337 + x3 + 0.2D1 * t688 * t617 - 0.
     #4D1 * t684 + t103 * t8 * t13 - 0.2D1 * t103 * t8 * z - t103 * t13 
     #* x1 - 0.2D1 * t688 * t295 - 0.2D1 * t688 * t431 - 0.2D1 * t688 * 
     #t616 + x3 * t13 * t616 + 0.4D1 * t56 * t431 - 0.2D1 * t56 * t13 * 
     #x2 + 0.3D1 * t103 * t295 - t617
      t745 = -x1 + t616 - t689 + 0.2D1 * t56 - t529 + t103 + 0.3D1 * t33
     #8 - x2 - t546 + 0.3D1 * t683 + t700 + 0.2D1 * t688 * x1 - 0.4D1 * 
     #t56 * z - 0.2D1 * t56 * x2 + 0.2D1 * t56 * t13 - 0.2D1 * t103 * x1
     # + t104 + t431
      t747 = 0.1D1 / (t731 + t745)
      t748 = t296 * t747
      t749 = bbggh41J2(s, XB1, XB2, z, lh, wd, t293, x2, t551, x4)
      t755 = log(0.4D1 * t111 * t376 * t685 * t555)
      t757 = bbggh41J1(s, XB1, XB2, z, lh, wd, t293, x2, t551, x4)
      t760 = -t748 * t749 + t755 * t296 * t747 * t757
      t765 = 0.180D3 * t6 * t748 * t757
      t766 = 0.90D2 * t5 * t760 + t765
      t769 = t766 * t98 * t124 / 0.2880D4
      t770 = FJET(XB1, XB2, s, t682, t694, -t698, -t704, -t615, -t769)
      t773 = t98 * t123 * t53
      t776 = FJET(XB1, XB2, s, t694, t682, -t704, -t698, -t615, -t769)
      t780 = FJET(XB1, XB2, s, -t292, t290, 0.0D0, 0.0D0, 0.0D0, t419)
      t782 = FJET(XB1, XB2, s, -t430, 0.0D0, t427, 0.0D0, 0.0D0, t526)
      t784 = FJET(XB1, XB2, s, -t545, 0.0D0, t540, 0.0D0, 0.0D0, t598)
      t786 = FJET(XB1, XB2, s, -t607, 0.0D0, t609, t290, -t615, t666)
      t791 = 0.90D2 * t5 * t760 + t765
      t794 = t791 * t98 * t124 / 0.2880D4
      t795 = FJET(XB1, XB2, s, -t698, -t704, t682, t694, -t615, -t794)
      t799 = FJET(XB1, XB2, s, -t704, -t698, t694, t682, -t615, -t794)
      t803 = t674 * t419 + t676 * t526 + t678 * t598 + t680 * t666 - t77
     #0 * t766 * t773 / 0.2880D4 - t776 * t766 * t773 / 0.2880D4 + t780 
     #* t419 + t782 * t526 + t784 * t598 + t786 * t666 - t795 * t791 * t
     #773 / 0.2880D4 - t799 * t791 * t773 / 0.2880D4
      bbggh4n1e1 = t673 + t803

      end function



      doubleprecision function bbggh4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = -0.1D1 + x3
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t20 = log(-0.4D1 * t8 * t17)
      t21 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t24 = cos(t9)
      t26 = Sqrt(-x3 * t15)
      t30 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t24 * t26)
      t34 = log(0.4D1 * t8 * t14)
      t39 = lh * t5
      t41 = -t21 - t21 * t30
      t43 = 0.180D3 * t39 * t41
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t52 = 0.1D1 / x2
      t54 = t45 * t52 * t47
      t57 = x2 ** 2
      t58 = t57 * t7
      t61 = log(0.4D1 * t58 * t14)
      t67 = 0.180D3 * t39 * t21
      t72 = t7 * t11
      t75 = log(0.4D1 * t72 * t13)
      t80 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t82 = t75 ** 2
      t88 = lh ** 2
      t90 = 0.3141592653589793D1 ** 2
      t92 = 0.180D3 * t88 - 0.30D2 * t90
      t93 = t92 * t5
      t94 = t93 * t21
      t101 = x3 * t11
      t105 = log(-0.4D1 * t101 * t13 * t16)
      t109 = log(0.4D1 * t101 * t13)
      t113 = t109 ** 2
      t114 = t105 ** 2
      t130 = t57 * x3
      t133 = log(0.4D1 * t130 * t14)
      t137 = log(-0.4D1 * t130 * t17)
      t148 = t57 * t11
      t151 = log(0.4D1 * t148 * t13)
      t157 = t151 ** 2
      t167 = log(0.4D1 * t14)
      t172 = t167 ** 2
      t194 = (0.90D2 * t5 * (-(t6 - t20 * t21) * t30 - t6 + t34 * t21) -
     # t43) * t45 * t47 / 0.5760D4 + t5 * t41 * t54 / 0.32D2 - (0.90D2 *
     # t5 * (t6 - t61 * t21) - t67) * t52 * t47 / 0.2880D4 + (-0.180D3 *
     # t39 * (-t6 + t75 * t21) + 0.90D2 * t5 * (-t80 + t75 * t6 - t82 * 
     #t21 / 0.2D1) - t94) * t47 / 0.5760D4 - ((0.90D2 * t5 * t6 - t67) *
     # (-t105 * t30 - t109) + 0.90D2 * t5 * t21 * (t113 / 0.2D1 + t114 *
     # t30 / 0.2D1) + (0.90D2 * t5 * t80 - 0.180D3 * t39 * t6 + t94) * (
     #t30 + 0.1D1)) * t45 / 0.11520D5 + (0.90D2 * t5 * (-t6 + t133 * t21
     # - (t6 - t137 * t21) * t30) - t43) * t45 * t52 / 0.5760D4 + (-0.18
     #0D3 * t39 * (-t6 + t151 * t21) + 0.90D2 * t5 * (-t80 + t151 * t6 -
     # t157 * t21 / 0.2D1) - t94) * t52 / 0.5760D4 - ((t6 - t167 * t21) 
     #* t92 - 0.180D3 * (t80 - t167 * t6 + t172 * t21 / 0.2D1) * lh + t2
     #1 * (-0.2884936567583026D3 - 0.120D3 * t88 * lh + 0.60D2 * lh * t9
     #0) - 0.90D2 * t167 * t80 + 0.45D2 * t172 * t6 - 0.15D2 * t172 * t1
     #67 * t21) * t5 / 0.11520D5
      t195 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t194)
      t197 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t194)
      t199 = t2 * x1
      t200 = -0.1D1 + x1
      t201 = t2 * t200
      t202 = -t200
      t203 = bbggh41J2(s, XB1, XB2, z, lh, wd, t202, 0.0D0, 0.0D0, x4)
      t204 = t8 * t11
      t205 = x1 * z
      t206 = 0.1D1 - x1 + t205
      t207 = 0.1D1 / t206
      t208 = t13 * t207
      t209 = t200 ** 2
      t214 = log(-0.4D1 * t204 * t208 * t209 * t16)
      t215 = bbggh41J1(s, XB1, XB2, z, lh, wd, t202, 0.0D0, 0.0D0, x4)
      t218 = x1 * x3
      t219 = t218 * z
      t222 = x3 * t206
      t224 = Sqrt(-t222 * t15)
      t228 = 0.1D1 / (-0.2D1 * t219 + 0.2D1 * t218 - 0.1D1 - x3 + 0.2D1 
     #* t24 * t224)
      t230 = t208 * t209
      t233 = log(0.4D1 * t204 * t230)
      t239 = t215 * t228 + t215
      t250 = t58 * t11
      t253 = log(0.4D1 * t250 * t230)
      t266 = log(0.4D1 * t72 * t230)
      t271 = bbggh41J3(s, XB1, XB2, z, lh, wd, t202, 0.0D0, 0.0D0, x4)
      t273 = t266 ** 2
      t283 = (0.90D2 * t5 * ((t203 - t214 * t215) * t228 + t203 - t233 *
     # t215) - 0.180D3 * t39 * t239) * t45 * t47 / 0.5760D4 + t5 * t239 
     #* t54 / 0.32D2 - (0.90D2 * t5 * (-t203 + t253 * t215) + 0.180D3 * 
     #t39 * t215) * t52 * t47 / 0.2880D4 + (-0.180D3 * t39 * (t203 - t26
     #6 * t215) + 0.90D2 * t5 * (t271 - t266 * t203 + t273 * t215 / 0.2D
     #1) + t93 * t215) * t47 / 0.5760D4
      t284 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t199, -t201, 0.0D0, t283)
      t286 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t201, t199, 0.0D0, t283)
      t288 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t194)
      t291 = x2 * s * t1
      t292 = -0.1D1 + x2
      t293 = t292 * s
      t294 = t293 * t1
      t295 = x2 * z
      t297 = 0.1D1 / (0.1D1 + t295 - x2)
      t298 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t299 = t297 * t298
      t300 = t14 * t292
      t303 = log(-0.4D1 * t130 * t300)
      t305 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t310 = t297 * t305
      t312 = 0.180D3 * t39 * t310
      t317 = t13 * t292
      t320 = log(-0.4D1 * t148 * t317)
      t321 = t320 * t297
      t326 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t329 = t320 ** 2
      t346 = log(-0.4D1 * t58 * t300)
      t356 = (0.90D2 * t5 * (t299 - t303 * t297 * t305) - t312) * t45 * 
     #t52 / 0.5760D4 + (-0.180D3 * t39 * (t299 - t321 * t305) + 0.90D2 *
     # t5 * (t297 * t326 - t321 * t298 + t329 * t297 * t305 / 0.2D1) + t
     #93 * t310) * t52 / 0.5760D4 + t5 * t297 * t305 * t54 / 0.32D2 - (0
     #.90D2 * t5 * (-t299 + t346 * t297 * t305) + t312) * t52 * t47 / 0.
     #2880D4
      t357 = FJET(XB1, XB2, s, 0.0D0, t291, 0.0D0, -t294, 0.0D0, t356)
      t359 = x2 * x3
      t362 = Sqrt(x3 * t292 * t15)
      t363 = t24 * t362
      t365 = 0.2D1 * t363 * x2
      t367 = 0.1D1 - x3 + t359
      t368 = 0.1D1 / t367
      t370 = t2 * (0.1D1 - x3 - x2 + t359 + t130 + t365) * t368
      t371 = 0.2D1 * t363
      t375 = t2 * x2 * (-0.1D1 + t359 + t371) * t368
      t376 = t130 * z
      t380 = 0.1D1 / (-0.1D1 - t295 - t130 - t365 + t359 + x2 - x3 + t37
     #6 + t371 + 0.2D1 * t363 * t295)
      t382 = t359 * t368
      t383 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t382, x4)
      t387 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t382, x4)
      t390 = t367 ** 2
      t396 = log(0.4D1 * t130 * t11 * t317 * t15 / t390)
      t409 = t5 * t380 * t383 * t54 / 0.32D2 + (0.90D2 * t5 * (t380 * t3
     #87 - t396 * t380 * t383) - 0.180D3 * t39 * t380 * t383) * t45 * t5
     #2 / 0.5760D4
      t410 = FJET(XB1, XB2, s, 0.0D0, t370, 0.0D0, -t375, 0.0D0, t409)
      t412 = FJET(XB1, XB2, s, 0.0D0, -t294, 0.0D0, t291, 0.0D0, t356)
      t414 = FJET(XB1, XB2, s, 0.0D0, -t375, 0.0D0, t370, 0.0D0, t409)
      t418 = t2 * t200 * x2 * t207
      t420 = t293 * t1 * t200
      t421 = t1 ** 2
      t426 = s * t421 * x2 * x1 * t200 * t207
      t427 = x2 * x1
      t428 = t427 * z
      t430 = 0.1D1 / (-0.1D1 - t295 - t427 + x1 - t205 + t428 + x2)
      t432 = bbggh41J1(s, XB1, XB2, z, lh, wd, t202, x2, 0.0D0, x4)
      t435 = t52 * t47
      t439 = bbggh41J2(s, XB1, XB2, z, lh, wd, t202, x2, 0.0D0, x4)
      t445 = log(-0.4D1 * t250 * t208 * t209 * t292)
      t460 = t5 * t430 * t432 * t206 * t45 * t435 / 0.32D2 - (-0.90D2 * 
     #t5 * (t430 * t439 - t445 * t430 * t432) * t206 + 0.180D3 * t39 * t
     #430 * t432 * t206) * t52 * t47 / 0.2880D4
      t461 = FJET(XB1, XB2, s, 0.0D0, -t418, t199, t420, -t426, t460)
      t463 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t194)
      t465 = FJET(XB1, XB2, s, t199, t420, 0.0D0, -t418, -t426, t460)
      t467 = t195 * t194 + t197 * t194 + t284 * t283 + t286 * t283 + t28
     #8 * t194 + t357 * t356 + t410 * t409 + t412 * t356 + t414 * t409 +
     # t461 * t460 + t463 * t194 + t465 * t460
      t468 = FJET(XB1, XB2, s, t199, -t201, 0.0D0, 0.0D0, 0.0D0, t283)
      t470 = FJET(XB1, XB2, s, t291, 0.0D0, -t294, 0.0D0, 0.0D0, t356)
      t472 = FJET(XB1, XB2, s, t370, 0.0D0, -t375, 0.0D0, 0.0D0, t409)
      t474 = FJET(XB1, XB2, s, t420, t199, -t418, 0.0D0, -t426, t460)
      t476 = t199 * t382
      t477 = t359 * x1
      t478 = t359 * t205
      t481 = Sqrt(t222 * t292 * t15)
      t482 = t24 * t481
      t483 = 0.2D1 * t482
      t488 = t201 * x2 * (t218 - t219 + t359 - t477 + t478 - 0.1D1 + t48
     #3) * t207 * t368
      t492 = t15 * s * t1 * x1 * t368
      t494 = 0.2D1 * t482 * x2
      t495 = 0.1D1 - x1 + t205 - x2 + t427 - t428 - x3 + t218 - t219 + t
     #359 - t477 + t478 + t130 + t494
      t498 = t201 * t495 * t207 * t368
      t514 = 0.1D1 + 0.2D1 * t482 * t428 + t427 + 0.2D1 * t8 - t483 - t3
     #59 + t130 - x2 + t130 * t7 + t494 + 0.2D1 * t482 * x1 - 0.4D1 * t8
     # * z - 0.2D1 * t8 * x2 + 0.2D1 * t8 * t12 - 0.2D1 * t130 * x1 + t2
     #95 - t428
      t541 = 0.3D1 * t477 + 0.3D1 * t219 - t376 - 0.4D1 * t478 - 0.2D1 *
     # t482 * t205 - t130 * t12 * x1 - 0.2D1 * t130 * t7 * z + t130 * t7
     # * t12 + x3 - x1 - 0.2D1 * t482 * t295 + x3 * t12 * t427 - 0.2D1 *
     # t482 * t427 - 0.2D1 * t8 * t12 * x2 + 0.4D1 * t8 * t295 + 0.3D1 *
     # t130 * t205 + t205 - 0.3D1 * t218
      t543 = 0.1D1 / (t514 + t541)
      t545 = bbggh41J1(s, XB1, XB2, z, lh, wd, t202, x2, t382, x4)
      t547 = t545 * t45 * t435
      t549 = t5 * t206 * t543 * t547 / 0.32D2
      t550 = FJET(XB1, XB2, s, t476, t488, -t492, -t498, -t426, t549)
      t552 = t206 * t543
      t556 = FJET(XB1, XB2, s, t488, t476, -t498, -t492, -t426, t549)
      t561 = FJET(XB1, XB2, s, -t201, t199, 0.0D0, 0.0D0, 0.0D0, t283)
      t563 = FJET(XB1, XB2, s, -t294, 0.0D0, t291, 0.0D0, 0.0D0, t356)
      t565 = FJET(XB1, XB2, s, -t375, 0.0D0, t370, 0.0D0, 0.0D0, t409)
      t567 = FJET(XB1, XB2, s, -t418, 0.0D0, t420, t199, -t426, t460)
      t569 = FJET(XB1, XB2, s, -t492, -t498, t476, t488, -t426, t549)
      t574 = FJET(XB1, XB2, s, -t498, -t492, t488, t476, -t426, t549)
      t579 = t468 * t283 + t470 * t356 + t472 * t409 + t474 * t460 + t55
     #0 * t5 * t552 * t547 / 0.32D2 + t556 * t5 * t552 * t547 / 0.32D2 +
     # t561 * t283 + t563 * t356 + t565 * t409 + t567 * t460 + t569 * t5
     # * t552 * t547 / 0.32D2 + t574 * t5 * t552 * t547 / 0.32D2
      bbggh4n1e0 = t467 + t579

      end function



      doubleprecision function bbggh4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = cos(t8)
      t30 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t30)
      t36 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t29 * t32)
      t39 = t5 * (-t17 - t17 * t36)
      t40 = 0.1D1 / x3
      t41 = t40 * t26
      t44 = t5 * t17
      t45 = 0.1D1 / x2
      t46 = t45 * t26
      t52 = x2 ** 2
      t53 = t52 * t10
      t56 = log(0.4D1 * t53 * t13)
      t66 = log(0.4D1 * t13 * t10)
      t71 = lh ** 2
      t73 = 0.3141592653589793D1 ** 2
      t77 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t81 = t66 ** 2
      t87 = x3 * t10
      t92 = log(-0.4D1 * t87 * t13 / t30)
      t96 = log(0.4D1 * t87 * t13)
      t108 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.5760D4 + 
     #t39 * t41 / 0.64D2 - t44 * t46 / 0.32D2 + t39 * t40 * t45 / 0.64D2
     # + (0.90D2 * t5 * (-t6 + t56 * t17) + t24) * t45 / 0.5760D4 - (-0.
     #180D3 * (t6 - t66 * t17) * lh + t17 * (0.180D3 * t71 - 0.30D2 * t7
     #3) + 0.90D2 * t77 - 0.90D2 * t66 * t6 + 0.45D2 * t81 * t17) * t5 /
     # 0.11520D5 - (0.90D2 * t44 * (-t92 * t36 - t96) + (0.90D2 * t5 * t
     #6 - t24) * (t36 + 0.1D1)) * t40 / 0.11520D5
      t109 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t108)
      t111 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t108)
      t113 = t2 * x1
      t114 = -0.1D1 + x1
      t115 = t2 * t114
      t116 = -t114
      t117 = bbggh41J2(s, XB1, XB2, z, lh, wd, t116, 0.0D0, 0.0D0, x4)
      t118 = x1 * z
      t119 = 0.1D1 - x1 + t118
      t120 = 0.1D1 / t119
      t122 = t114 ** 2
      t126 = log(0.4D1 * t11 * t13 * t120 * t122)
      t127 = bbggh41J1(s, XB1, XB2, z, lh, wd, t116, 0.0D0, 0.0D0, x4)
      t137 = x1 * x3
      t143 = Sqrt(-x3 * t119 * t30)
      t156 = (0.90D2 * t5 * (t117 - t126 * t127) - 0.180D3 * t22 * t127)
     # * t26 / 0.5760D4 + t5 * (t127 / (-0.2D1 * t137 * z + 0.2D1 * t137
     # - 0.1D1 - x3 + 0.2D1 * t29 * t143) + t127) * t41 / 0.64D2 + t5 * 
     #t127 * t46 / 0.32D2
      t157 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t113, -t115, 0.0D0, t156)
      t159 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t115, t113, 0.0D0, t156)
      t161 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t164 = x2 * s * t1
      t165 = -0.1D1 + x2
      t166 = t165 * s
      t167 = t166 * t1
      t168 = x2 * z
      t170 = 0.1D1 / (0.1D1 + t168 - x2)
      t171 = t5 * t170
      t172 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t177 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t182 = log(-0.4D1 * t53 * t13 * t165)
      t198 = t171 * t172 * t40 * t45 / 0.64D2 + (0.90D2 * t5 * (t170 * t
     #177 - t182 * t170 * t172) - 0.180D3 * t22 * t170 * t172) * t45 / 0
     #.5760D4 + t171 * t172 * t45 * t26 / 0.32D2
      t199 = FJET(XB1, XB2, s, 0.0D0, t164, 0.0D0, -t167, 0.0D0, t198)
      t201 = x2 * x3
      t202 = t52 * x3
      t205 = Sqrt(x3 * t165 * t30)
      t206 = t29 * t205
      t208 = 0.2D1 * t206 * x2
      t211 = 0.1D1 / (0.1D1 - x3 + t201)
      t213 = t2 * (0.1D1 - x3 - x2 + t201 + t202 + t208) * t211
      t214 = 0.2D1 * t206
      t218 = t2 * x2 * (-0.1D1 + t201 + t214) * t211
      t223 = 0.1D1 / (-0.1D1 - t168 - t202 - t208 + t201 + x2 - x3 + t20
     #2 * z + t214 + 0.2D1 * t206 * t168)
      t226 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t201 * t211, 
     #x4)
      t228 = t226 * t40 * t45
      t230 = t5 * t223 * t228 / 0.64D2
      t231 = FJET(XB1, XB2, s, 0.0D0, t213, 0.0D0, -t218, 0.0D0, t230)
      t236 = FJET(XB1, XB2, s, 0.0D0, -t167, 0.0D0, t164, 0.0D0, t198)
      t238 = FJET(XB1, XB2, s, 0.0D0, -t218, 0.0D0, t213, 0.0D0, t230)
      t245 = t2 * t114 * x2 * t120
      t247 = t166 * t1 * t114
      t248 = t1 ** 2
      t253 = s * t248 * x2 * x1 * t114 * t120
      t254 = x2 * x1
      t257 = 0.1D1 / (-0.1D1 - t168 - t254 + x1 - t118 + t254 * z + x2)
      t259 = bbggh41J1(s, XB1, XB2, z, lh, wd, t116, x2, 0.0D0, x4)
      t264 = t5 * t257 * t259 * t119 * t45 * t26 / 0.32D2
      t265 = FJET(XB1, XB2, s, 0.0D0, -t245, t113, t247, -t253, t264)
      t269 = t259 * t119 * t46
      t272 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t108)
      t274 = FJET(XB1, XB2, s, t113, t247, 0.0D0, -t245, -t253, t264)
      t279 = FJET(XB1, XB2, s, t113, -t115, 0.0D0, 0.0D0, 0.0D0, t156)
      t281 = FJET(XB1, XB2, s, t164, 0.0D0, -t167, 0.0D0, 0.0D0, t198)
      t283 = FJET(XB1, XB2, s, t213, 0.0D0, -t218, 0.0D0, 0.0D0, t230)
      t288 = FJET(XB1, XB2, s, t247, t113, -t245, 0.0D0, -t253, t264)
      t293 = FJET(XB1, XB2, s, -t115, t113, 0.0D0, 0.0D0, 0.0D0, t156)
      t295 = FJET(XB1, XB2, s, -t167, 0.0D0, t164, 0.0D0, 0.0D0, t198)
      t297 = FJET(XB1, XB2, s, -t245, 0.0D0, t247, t113, -t253, t264)
      t302 = FJET(XB1, XB2, s, -t218, 0.0D0, t213, 0.0D0, 0.0D0, t230)
      bbggh4n1em1 = t109 * t108 + t111 * t108 + t157 * t156 + t159 * t15
     #6 + t161 * t108 + t199 * t198 + t231 * t5 * t223 * t228 / 0.64D2 +
     # t236 * t198 + t238 * t5 * t223 * t228 / 0.64D2 + t265 * t5 * t257
     # * t269 / 0.32D2 + t272 * t108 + t274 * t5 * t257 * t269 / 0.32D2 
     #+ t279 * t156 + t281 * t198 + t283 * t5 * t223 * t228 / 0.64D2 + t
     #288 * t5 * t257 * t269 / 0.32D2 + t293 * t156 + t295 * t198 + t297
     # * t5 * t257 * t269 / 0.32D2 + t302 * t5 * t223 * t228 / 0.64D2

      end function



      doubleprecision function bbggh4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x2
      t16 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t18 = z ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t31 = cos(t20)
      t34 = Sqrt(-x3 * (-0.1D1 + x3))
      t44 = -t7 * t8 / 0.64D2 - t7 * t11 / 0.64D2 - (-0.180D3 * t6 * lh 
     #+ 0.90D2 * t16 - 0.90D2 * t25 * t6) * t5 / 0.11520D5 - t7 * (0.1D1
     # / (-0.1D1 - x3 + 0.2D1 * t31 * t34) + 0.1D1) / x3 / 0.128D3
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t44)
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t44)
      t49 = t2 * x1
      t50 = -0.1D1 + x1
      t51 = t2 * t50
      t53 = bbggh41J1(s, XB1, XB2, z, lh, wd, -t50, 0.0D0, 0.0D0, x4)
      t56 = t5 * t53 * t8 / 0.64D2
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t49, -t51, 0.0D0, t56)
      t59 = t53 * t8
      t62 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t51, t49, 0.0D0, t56)
      t66 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t44)
      t69 = x2 * s * t1
      t72 = (-0.1D1 + x2) * s * t1
      t75 = 0.1D1 / (0.1D1 + x2 * z - x2)
      t77 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t80 = t5 * t75 * t77 * t11 / 0.64D2
      t81 = FJET(XB1, XB2, s, 0.0D0, t69, 0.0D0, -t72, 0.0D0, t80)
      t84 = t75 * t77 * t11
      t87 = FJET(XB1, XB2, s, 0.0D0, -t72, 0.0D0, t69, 0.0D0, t80)
      t91 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t44)
      t93 = FJET(XB1, XB2, s, t49, -t51, 0.0D0, 0.0D0, 0.0D0, t56)
      t97 = FJET(XB1, XB2, s, t69, 0.0D0, -t72, 0.0D0, 0.0D0, t80)
      t101 = FJET(XB1, XB2, s, -t51, t49, 0.0D0, 0.0D0, 0.0D0, t56)
      t105 = FJET(XB1, XB2, s, -t72, 0.0D0, t69, 0.0D0, 0.0D0, t80)
      bbggh4n1em2 = t45 * t44 + t47 * t44 + t57 * t5 * t59 / 0.64D2 + t6
     #2 * t5 * t59 / 0.64D2 + t66 * t44 + t81 * t5 * t84 / 0.64D2 + t87 
     #* t5 * t84 / 0.64D2 + t91 * t44 + t93 * t5 * t59 / 0.64D2 + t97 * 
     #t5 * t84 / 0.64D2 + t101 * t5 * t59 / 0.64D2 + t105 * t5 * t84 / 0
     #.64D2

      end function



      doubleprecision function bbggh4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t8 = t5 * t6 / 0.128D3
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t15 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t18 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      bbggh4n1em3 = -t9 * t5 * t6 / 0.128D3 - t12 * t5 * t6 / 0.128D3 - 
     #t15 * t5 * t6 / 0.128D3 - t18 * t5 * t6 / 0.128D3

      end function



      doubleprecision function bbggh4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      bbggh4n1em4 = 0.0D0

      end function


      doubleprecision function bbggh4n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t18 = log(0.4D1 * t16)
      t19 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = t18 ** 2
      t22 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t28 = lh ** 2
      t29 = 0.180D3 * t28
      t30 = 0.3141592653589793D1 ** 2
      t31 = 0.30D2 * t30
      t32 = t29 - t31
      t33 = t32 * t5
      t47 = 0.120D3 * t28 * lh
      t49 = 0.60D2 * lh * t30
      t50 = -0.2884936567583026D3 - t47 + t49
      t51 = t50 * t5
      t52 = t51 * t22
      t54 = 0.1D1 / x1
      t57 = t8 * x3
      t58 = t15 * t11
      t61 = log(0.4D1 * t57 * t58)
      t63 = -0.1D1 + x3
      t64 = 0.1D1 / t63
      t65 = t58 * t64
      t68 = log(-0.4D1 * t57 * t65)
      t71 = x3 * z
      t72 = 0.2D1 * t71
      t73 = cos(t9)
      t75 = Sqrt(-t71 * t63)
      t79 = 0.1D1 / (-t72 - 0.1D1 + x3 + 0.2D1 * t73 * t75)
      t85 = t68 ** 2
      t91 = t61 ** 2
      t97 = t22 * t79
      t101 = 0.1D1 / x3
      t105 = x2 ** 2
      t106 = x3 * t105
      t107 = t106 * t8
      t110 = log(-0.4D1 * t107 * t65)
      t114 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t115 = -0.1D1 + x2
      t116 = t58 * t115
      t119 = log(-0.4D1 * t107 * t116)
      t120 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t124 = log(0.4D1 * t106 * t16)
      t129 = -t97 + t120 - t22
      t134 = 0.1D1 / x2
      t135 = t134 * t54
      t138 = t105 * t8
      t141 = log(-0.4D1 * t138 * t116)
      t145 = log(0.4D1 * t138 * t58)
      t151 = t145 ** 2
      t154 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t156 = t141 ** 2
      t162 = t120 - t22
      t169 = log(0.4D1 * t58)
      t172 = t169 ** 2
      t181 = t172 * t169
      t187 = t172 ** 2
      t191 = t30 ** 2
      t192 = t28 ** 2
      t209 = x3 * t15
      t212 = log(0.4D1 * t209 * t11)
      t213 = t212 ** 2
      t217 = log(-0.4D1 * t209 * t11 * t64)
      t218 = t217 ** 2
      t249 = t15 * t105
      t252 = log(0.4D1 * t249 * t11)
      t254 = t252 ** 2
      t257 = t11 * t115
      t260 = log(-0.4D1 * t249 * t257)
      t262 = t260 ** 2
      t294 = log(-0.4D1 * t106 * t65)
      t300 = log(-0.4D1 * t106 * t116)
      t304 = log(0.4D1 * t106 * t58)
      t310 = t300 ** 2
      t314 = t304 ** 2
      t318 = t294 ** 2
      t332 = -(-0.180D3 * t6 * (t7 - t18 * t19 + t21 * t22 / 0.2D1) + t3
     #3 * (t19 - t18 * t22) + 0.90D2 * t5 * (-t18 * t7 + t21 * t19 / 0.2
     #D1 - t21 * t18 * t22 / 0.6D1) + t52) * t54 / 0.5760D4 + (-0.180D3 
     #* t6 * (-t19 + t61 * t22 - (t19 - t68 * t22) * t79) + 0.90D2 * t5 
     #* (-(t7 - t68 * t19 + t85 * t22 / 0.2D1) * t79 - t7 + t61 * t19 - 
     #t91 * t22 / 0.2D1) + t33 * (-t97 - t22)) * t101 * t54 / 0.5760D4 +
     # (0.90D2 * t5 * (-(t19 - t110 * t22) * t79 + t114 - t119 * t120 - 
     #t19 + t124 * t22) - 0.180D3 * t6 * t129) * t101 * t135 / 0.2880D4 
     #+ (-0.180D3 * t6 * (t114 - t141 * t120 - t19 + t145 * t22) + 0.90D
     #2 * t5 * (-t7 + t145 * t19 - t151 * t22 / 0.2D1 + t154 - t141 * t1
     #14 + t156 * t120 / 0.2D1) + t33 * t162) * t134 * t54 / 0.2880D4 - 
     #(0.180D3 * t169 * lh + t29 - t31 + 0.45D2 * t172) * t5 * t7 / 0.11
     #520D5 - (-t169 * t32 - 0.90D2 * t172 * lh - 0.2884936567583026D3 -
     # t47 + t49 - 0.15D2 * t181) * t5 * t19 / 0.11520D5 - (0.15D2 / 0.4
     #D1 * t187 - t169 * t50 + 0.5769873135166051D3 * lh + t191 + 0.60D2
     # * t192 - 0.60D2 * t28 * t30 + t172 * t32 / 0.2D1 + 0.30D2 * t181 
     #* lh) * t5 * t22 / 0.11520D5 - ((0.90D2 * t5 * t19 - 0.180D3 * t6 
     #* t22) * (t213 / 0.2D1 + t218 * t79 / 0.2D1) + (0.90D2 * t5 * t7 -
     # 0.180D3 * t6 * t19 + t33 * t22) * (-t217 * t79 - t212) + 0.90D2 *
     # t5 * t22 * (-t213 * t212 / 0.6D1 - t218 * t217 * t79 / 0.6D1) + (
     #-0.180D3 * t6 * t7 + t33 * t19 + t52) * (t79 + 0.1D1)) * t101 / 0.
     #11520D5 - (-0.180D3 * t6 * (t7 - t252 * t19 + t254 * t22 / 0.2D1 -
     # t154 + t260 * t114 - t262 * t120 / 0.2D1) + t33 * (-t114 + t260 *
     # t120 + t19 - t252 * t22) + 0.90D2 * t5 * (-t252 * t7 + t254 * t19
     # / 0.2D1 - t254 * t252 * t22 / 0.6D1 + t260 * t154 - t262 * t114 /
     # 0.2D1 + t262 * t260 * t120 / 0.6D1) - t51 * t162) * t134 / 0.5760
     #D4 - (-0.180D3 * t6 * ((t19 - t294 * t22) * t79 - t114 + t300 * t1
     #20 + t19 - t304 * t22) + 0.90D2 * t5 * (-t154 + t300 * t114 - t310
     # * t120 / 0.2D1 + t7 - t304 * t19 + t314 * t22 / 0.2D1 + (t7 - t29
     #4 * t19 + t318 * t22 / 0.2D1) * t79) - t33 * t129) * t101 * t134 /
     # 0.5760D4
      t333 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t332)
      t335 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t332)
      t337 = t2 * x1
      t338 = -0.1D1 + x1
      t339 = t2 * t338
      t340 = bbggh41J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t341 = 0.1D1 / t13
      t342 = x1 * z
      t343 = -z - x1 + t342
      t344 = 0.1D1 / t343
      t345 = t341 * t344
      t346 = t338 ** 2
      t347 = t345 * t346
      t350 = log(-0.4D1 * t12 * t347)
      t351 = bbggh41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t353 = t350 ** 2
      t354 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t357 = -t340 + t350 * t351 - t353 * t354 / 0.2D1
      t361 = -t351 + t350 * t354
      t369 = t350 * t340 - t353 * t351 / 0.2D1 + t353 * t350 * t354 / 0.
     #6D1
      t372 = t51 * t354
      t376 = t57 * t11
      t379 = log(-0.4D1 * t376 * t347)
      t382 = t345 * t346 * t64
      t385 = log(0.4D1 * t376 * t382)
      t388 = x1 * x3
      t389 = t388 * z
      t392 = x3 * t343
      t394 = Sqrt(t392 * t63)
      t398 = 0.1D1 / (0.2D1 * t389 - 0.2D1 * t388 - t72 + 0.2D1 * t73 * 
     #t394 - 0.1D1 + x3)
      t404 = t385 ** 2
      t410 = t379 ** 2
      t417 = t354 * t398 + t354
      t422 = (-0.180D3 * t6 * (t351 - t379 * t354 + (t351 - t385 * t354)
     # * t398) + 0.90D2 * t5 * ((t340 - t385 * t351 + t404 * t354 / 0.2D
     #1) * t398 + t340 - t379 * t351 + t410 * t354 / 0.2D1) + t33 * t417
     #) * t101 * t54 / 0.5760D4
      t424 = t344 * t346
      t428 = log(-0.4D1 * t107 * t11 * t341 * t424)
      t430 = t106 * t12
      t433 = log(0.4D1 * t430 * t382)
      t445 = (0.90D2 * t5 * (t351 - t428 * t354 + (t351 - t433 * t354) *
     # t398) - 0.180D3 * t6 * t417) * t101 * t135 / 0.2880D4
      t446 = t138 * t11
      t449 = log(-0.4D1 * t446 * t347)
      t455 = t449 ** 2
      t465 = (-0.180D3 * t6 * (t351 - t449 * t354) + 0.90D2 * t5 * (t340
     # - t449 * t351 + t455 * t354 / 0.2D1) + t33 * t354) * t134 * t54 /
     # 0.2880D4
      t466 = -(-0.180D3 * t6 * t357 + t33 * t361 + 0.90D2 * t5 * t369 - 
     #t372) * t54 / 0.5760D4 + t422 + t445 + t465
      t467 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t337, -t339, 0.0D0, t466)
      t469 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t339, t337, 0.0D0, t466)
      t471 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t332)
      t473 = x2 * x3
      t474 = 0.1D1 - x3 + t473
      t475 = 0.1D1 / t474
      t476 = t473 * t475
      t477 = t2 * t476
      t479 = t2 * t63 * t475
      t480 = t473 * z
      t481 = t115 * t63
      t483 = Sqrt(t71 * t481)
      t487 = 0.1D1 / (-t72 + t480 - 0.1D1 + x3 + 0.2D1 * t73 * t483)
      t488 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t476, x4)
      t489 = t487 * t488
      t491 = t474 ** 2
      t492 = 0.1D1 / t491
      t493 = t63 * t492
      t497 = log(0.4D1 * t430 * t15 * t115 * t493)
      t499 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t476, x4)
      t504 = t487 * t499
      t510 = (0.90D2 * t5 * (t489 - t497 * t487 * t499) - 0.180D3 * t6 *
     # t504) * t101 * t135 / 0.2880D4
      t515 = log(0.4D1 * t106 * t15 * t257 * t493)
      t516 = t515 * t487
      t518 = -t489 + t516 * t499
      t521 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t476, x4)
      t524 = t515 ** 2
      t528 = -t487 * t521 + t516 * t488 - t524 * t487 * t499 / 0.2D1
      t531 = t33 * t504
      t536 = t510 - (-0.180D3 * t6 * t518 + 0.90D2 * t5 * t528 - t531) *
     # t101 * t134 / 0.5760D4
      t537 = FJET(XB1, XB2, s, 0.0D0, t477, 0.0D0, -t479, 0.0D0, t536)
      t539 = FJET(XB1, XB2, s, 0.0D0, -t479, 0.0D0, t477, 0.0D0, t536)
      t541 = x2 * x1
      t543 = t2 * t541 * t344
      t546 = t115 * s * t1 * x1
      t547 = t1 ** 2
      t552 = s * t547 * x2 * x1 * t338 * t344
      t553 = t541 * z
      t555 = 0.1D1 / (z + x1 - t342 - t541 + t553)
      t556 = t343 * t555
      t557 = bbggh41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t558 = t556 * t557
      t560 = t345 * t346 * t115
      t563 = log(0.4D1 * t430 * t560)
      t565 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t566 = t555 * t565
      t571 = t556 * t565
      t579 = log(0.4D1 * t446 * t560)
      t580 = t579 * t343
      t585 = bbggh41J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t589 = t579 ** 2
      t601 = (0.90D2 * t5 * (t558 - t563 * t343 * t566) - 0.180D3 * t6 *
     # t571) * t101 * t135 / 0.2880D4 + (-0.180D3 * t6 * (t558 - t580 * 
     #t566) + 0.90D2 * t5 * (t556 * t585 - t580 * t555 * t557 + t589 * t
     #343 * t566 / 0.2D1) + t33 * t571) * t134 * t54 / 0.2880D4
      t602 = FJET(XB1, XB2, s, 0.0D0, -t543, -t339, -t546, t552, t601)
      t604 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t332)
      t617 = -(-0.180D3 * t6 * t357 + t33 * t361 + 0.90D2 * t5 * t369 - 
     #t372) * t54 / 0.5760D4 + t422 + t445 + t465
      t618 = FJET(XB1, XB2, s, t337, -t339, 0.0D0, 0.0D0, 0.0D0, t617)
      t630 = t510 - (-0.180D3 * t6 * t518 + 0.90D2 * t5 * t528 - t531) *
     # t101 * t134 / 0.5760D4
      t631 = FJET(XB1, XB2, s, t477, 0.0D0, -t479, 0.0D0, 0.0D0, t630)
      t636 = t63 * s * t1 * t338 * t475
      t637 = x2 * z
      t638 = t473 * x1
      t639 = t473 * t342
      t641 = Sqrt(-t392 * t481)
      t642 = t73 * t641
      t645 = z + x1 - t342 - t637 - t541 + t553 - t71 - t388 + t389 + t4
     #80 + t638 - t639 + t106 + 0.2D1 * t642 * x2
      t648 = t337 * t645 * t344 * t475
      t649 = t339 * t476
      t655 = t337 * x2 * (-t71 - t388 + t389 + t480 + t638 - t639 - 0.1D
     #1 + x3 + 0.2D1 * t642) * t344 * t475
      t659 = x3 * t13
      t675 = -x1 - z + t541 + 0.2D1 * t642 * t553 + t388 + t342 - 0.5D1 
     #* t389 + t71 - 0.3D1 * t659 * t541 - 0.4D1 * t57 * t637 + 0.2D1 * 
     #t57 * t13 * x2 - t106 * t342 - 0.2D1 * t642 * t541 - t106 * t8 * t
     #13 + 0.2D1 * t106 * t8 * z
      t696 = t106 * t13 * x1 - 0.2D1 * t642 * t342 + 0.4D1 * t639 - t553
     # + 0.2D1 * t642 * z + 0.4D1 * t659 * x1 + 0.4D1 * t57 * z + 0.2D1 
     #* t57 * x2 - 0.2D1 * t57 * t13 + t473 * t13 - t107 + 0.2D1 * t642 
     #* x1 - t638 - 0.2D1 * t57 - 0.2D1 * t659
      t698 = 0.1D1 / (t675 + t696)
      t699 = t343 * t698
      t700 = bbggh41J2(s, XB1, XB2, z, lh, wd, x1, x2, t476, x4)
      t708 = log(-0.4D1 * t106 * t12 * t341 * t424 * t481 * t492)
      t710 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, x2, t476, x4)
      t719 = 0.90D2 * t5 * (t699 * t700 - t708 * t343 * t698 * t710) - 0
     #.180D3 * t6 * t699 * t710
      t722 = t719 * t101 * t135 / 0.2880D4
      t723 = FJET(XB1, XB2, s, t636, -t648, -t649, t655, t552, t722)
      t726 = t101 * t134 * t54
      t729 = FJET(XB1, XB2, s, t655, -t649, -t648, t636, t552, t722)
      t733 = FJET(XB1, XB2, s, -t339, t337, 0.0D0, 0.0D0, 0.0D0, t617)
      t735 = FJET(XB1, XB2, s, -t339, -t546, 0.0D0, -t543, t552, t601)
      t737 = FJET(XB1, XB2, s, -t479, 0.0D0, t477, 0.0D0, 0.0D0, t630)
      t739 = FJET(XB1, XB2, s, -t546, -t339, -t543, 0.0D0, t552, t601)
      t741 = FJET(XB1, XB2, s, -t543, 0.0D0, -t546, -t339, t552, t601)
      t743 = FJET(XB1, XB2, s, -t648, t636, t655, -t649, t552, t722)
      t747 = FJET(XB1, XB2, s, -t649, t655, t636, -t648, t552, t722)
      bbggh4n2e1 = t333 * t332 + t335 * t332 + t467 * t466 + t469 * t466
     # + t471 * t332 + t537 * t536 + t539 * t536 + t602 * t601 + t604 * 
     #t332 + t618 * t617 + t631 * t630 + t723 * t719 * t726 / 0.2880D4 +
     # t729 * t719 * t726 / 0.2880D4 + t733 * t617 + t735 * t601 + t737 
     #* t630 + t739 * t601 + t741 * t601 + t743 * t719 * t726 / 0.2880D4
     # + t747 * t719 * t726 / 0.2880D4

      end function



      doubleprecision function bbggh4n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t23 = t15 * t22
      t26 = log(-0.4D1 * t8 * t23)
      t29 = x3 * z
      t30 = 0.2D1 * t29
      t31 = cos(t12)
      t33 = Sqrt(-t29 * t21)
      t37 = 0.1D1 / (-t30 - 0.1D1 + x3 + 0.2D1 * t31 * t33)
      t42 = lh * t5
      t43 = t19 * t37
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t53 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t54 = -t43 + t53 - t19
      t56 = 0.1D1 / x2
      t58 = t48 * t56 * t50
      t61 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t62 = x2 ** 2
      t63 = t62 * t7
      t64 = -0.1D1 + x2
      t65 = t15 * t64
      t68 = log(-0.4D1 * t63 * t65)
      t72 = log(0.4D1 * t63 * t15)
      t77 = t53 - t19
      t84 = t7 * t14
      t87 = log(0.4D1 * t84 * t11)
      t92 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t94 = t87 ** 2
      t100 = lh ** 2
      t101 = 0.180D3 * t100
      t102 = 0.3141592653589793D1 ** 2
      t103 = 0.30D2 * t102
      t104 = t101 - t103
      t105 = t104 * t5
      t106 = t105 * t19
      t115 = x3 * t11
      t119 = log(-0.4D1 * t115 * t14 * t22)
      t123 = log(0.4D1 * t115 * t14)
      t127 = t123 ** 2
      t128 = t119 ** 2
      t144 = t62 * x3
      t147 = log(-0.4D1 * t144 * t23)
      t153 = log(-0.4D1 * t144 * t65)
      t157 = log(0.4D1 * t144 * t15)
      t169 = t11 * t62
      t170 = t14 * t64
      t173 = log(-0.4D1 * t169 * t170)
      t177 = log(0.4D1 * t169 * t14)
      t183 = t177 ** 2
      t186 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t188 = t173 ** 2
      t201 = log(0.4D1 * t15)
      t209 = t201 ** 2
      t228 = (0.90D2 * t5 * (-t6 + t18 * t19 - (t6 - t26 * t19) * t37) -
     # 0.180D3 * t42 * (-t43 - t19)) * t48 * t50 / 0.5760D4 + t5 * t54 *
     # t58 / 0.32D2 + (0.90D2 * t5 * (t61 - t68 * t53 - t6 + t72 * t19) 
     #- 0.180D3 * t42 * t77) * t56 * t50 / 0.2880D4 - (-0.180D3 * t42 * 
     #(t6 - t87 * t19) + 0.90D2 * t5 * (t92 - t87 * t6 + t94 * t19 / 0.2
     #D1) + t106) * t50 / 0.5760D4 - ((0.90D2 * t5 * t6 - 0.180D3 * t42 
     #* t19) * (-t119 * t37 - t123) + 0.90D2 * t5 * t19 * (t127 / 0.2D1 
     #+ t128 * t37 / 0.2D1) + (0.90D2 * t5 * t92 - 0.180D3 * t42 * t6 + 
     #t106) * (t37 + 0.1D1)) * t48 / 0.11520D5 - (0.90D2 * t5 * ((t6 - t
     #147 * t19) * t37 - t61 + t153 * t53 + t6 - t157 * t19) + 0.180D3 *
     # t42 * t54) * t48 * t56 / 0.5760D4 - (-0.180D3 * t42 * (-t61 + t17
     #3 * t53 + t6 - t177 * t19) + 0.90D2 * t5 * (t92 - t177 * t6 + t183
     # * t19 / 0.2D1 - t186 + t173 * t61 - t188 * t53 / 0.2D1) - t105 * 
     #t77) * t56 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t201) * t5 * t92
     # / 0.11520D5 - (0.180D3 * t201 * lh + t101 - t103 + 0.45D2 * t209)
     # * t5 * t6 / 0.11520D5 - (-t201 * t104 - 0.90D2 * t209 * lh - 0.28
     #84936567583026D3 - 0.120D3 * t100 * lh + 0.60D2 * lh * t102 - 0.15
     #D2 * t209 * t201) * t5 * t19 / 0.11520D5
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t228)
      t231 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t228)
      t233 = t2 * x1
      t234 = -0.1D1 + x1
      t235 = t2 * t234
      t236 = bbggh41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t237 = t8 * t14
      t239 = x1 * z
      t240 = -z - x1 + t239
      t241 = 0.1D1 / t240
      t242 = 0.1D1 / t9 * t241
      t243 = t234 ** 2
      t244 = t242 * t243
      t247 = log(-0.4D1 * t237 * t244)
      t248 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t254 = log(0.4D1 * t237 * t242 * t243 * t22)
      t257 = x1 * x3
      t258 = t257 * z
      t261 = x3 * t240
      t263 = Sqrt(t261 * t21)
      t267 = 0.1D1 / (0.2D1 * t258 - 0.2D1 * t257 - t30 + 0.2D1 * t31 * 
     #t263 - 0.1D1 + x3)
      t273 = t248 * t267 + t248
      t279 = (0.90D2 * t5 * (t236 - t247 * t248 + (t236 - t254 * t248) *
     # t267) - 0.180D3 * t42 * t273) * t48 * t50 / 0.5760D4
      t282 = t5 * t273 * t58 / 0.32D2
      t283 = t63 * t14
      t286 = log(-0.4D1 * t283 * t244)
      t296 = (0.90D2 * t5 * (t236 - t286 * t248) - 0.180D3 * t42 * t248)
     # * t56 * t50 / 0.2880D4
      t299 = log(-0.4D1 * t84 * t244)
      t301 = -t236 + t299 * t248
      t304 = bbggh41J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t306 = t299 ** 2
      t309 = -t304 + t299 * t236 - t306 * t248 / 0.2D1
      t312 = t105 * t248
      t316 = t279 + t282 + t296 - (-0.180D3 * t42 * t301 + 0.90D2 * t5 *
     # t309 - t312) * t50 / 0.5760D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t233, -t235, 0.0D0, t316)
      t319 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t235, t233, 0.0D0, t316)
      t321 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t228)
      t323 = x2 * x3
      t324 = 0.1D1 - x3 + t323
      t325 = 0.1D1 / t324
      t326 = t323 * t325
      t327 = t2 * t326
      t329 = t2 * t21 * t325
      t330 = t323 * z
      t331 = t64 * t21
      t333 = Sqrt(t29 * t331)
      t337 = 0.1D1 / (-t30 + t330 - 0.1D1 + x3 + 0.2D1 * t31 * t333)
      t338 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t326, x4)
      t341 = t324 ** 2
      t347 = log(0.4D1 * t144 * t11 * t170 * t21 / t341)
      t349 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t326, x4)
      t351 = -t337 * t338 + t347 * t337 * t349
      t356 = 0.180D3 * t42 * t337 * t349
      t364 = t5 * t337 * t349 * t58 / 0.32D2
      t365 = -(0.90D2 * t5 * t351 + t356) * t48 * t56 / 0.5760D4 + t364
      t366 = FJET(XB1, XB2, s, 0.0D0, t327, 0.0D0, -t329, 0.0D0, t365)
      t368 = FJET(XB1, XB2, s, 0.0D0, -t329, 0.0D0, t327, 0.0D0, t365)
      t370 = x2 * x1
      t372 = t2 * t370 * t241
      t375 = t64 * s * t1 * x1
      t376 = t1 ** 2
      t381 = s * t376 * x2 * x1 * t234 * t241
      t382 = t5 * t240
      t383 = t370 * z
      t385 = 0.1D1 / (z + x1 - t239 - t370 + t383)
      t387 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t389 = t56 * t50
      t393 = t240 * t385
      t394 = bbggh41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t400 = log(0.4D1 * t283 * t242 * t243 * t64)
      t414 = t382 * t385 * t387 * t48 * t389 / 0.32D2 + (0.90D2 * t5 * (
     #t393 * t394 - t400 * t240 * t385 * t387) - 0.180D3 * t42 * t393 * 
     #t387) * t56 * t50 / 0.2880D4
      t415 = FJET(XB1, XB2, s, 0.0D0, -t372, -t235, -t375, t381, t414)
      t417 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t228)
      t428 = t279 + t282 + t296 - (-0.180D3 * t42 * t301 + 0.90D2 * t5 *
     # t309 - t312) * t50 / 0.5760D4
      t429 = FJET(XB1, XB2, s, t233, -t235, 0.0D0, 0.0D0, 0.0D0, t428)
      t438 = -(0.90D2 * t5 * t351 + t356) * t48 * t56 / 0.5760D4 + t364
      t439 = FJET(XB1, XB2, s, t327, 0.0D0, -t329, 0.0D0, 0.0D0, t438)
      t444 = t21 * s * t1 * t234 * t325
      t445 = x2 * z
      t446 = t323 * x1
      t447 = t323 * t239
      t449 = Sqrt(-t261 * t331)
      t450 = t31 * t449
      t453 = z + x1 - t239 - t445 - t370 + t383 - t29 - t257 + t258 + t3
     #30 + t446 - t447 + t144 + 0.2D1 * t450 * x2
      t456 = t233 * t453 * t241 * t325
      t457 = t235 * t326
      t463 = t233 * x2 * (-t29 - t257 + t258 + t330 + t446 - t447 - 0.1D
     #1 + x3 + 0.2D1 * t450) * t241 * t325
      t466 = x3 * t9
      t488 = -0.5D1 * t258 - 0.2D1 * t8 - 0.2D1 * t466 - 0.3D1 * t466 * 
     #t370 - 0.4D1 * t8 * t445 + 0.2D1 * t8 * t9 * x2 - t144 * t239 + t3
     #23 * t9 - t144 * t7 + 0.2D1 * t450 * x1 - t446 + 0.2D1 * t450 * z 
     #+ 0.4D1 * t466 * x1 + 0.4D1 * t8 * z + 0.2D1 * t8 * x2
      t505 = -0.2D1 * t8 * t9 + t239 + t257 - t383 - z - x1 + t29 + 0.2D
     #1 * t450 * t383 + 0.4D1 * t447 - 0.2D1 * t450 * t370 - t144 * t7 *
     # t9 + 0.2D1 * t144 * t7 * z + t144 * t9 * x1 - 0.2D1 * t450 * t239
     # + t370
      t507 = 0.1D1 / (t488 + t505)
      t509 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, x2, t326, x4)
      t511 = t509 * t48 * t389
      t513 = t382 * t507 * t511 / 0.32D2
      t514 = FJET(XB1, XB2, s, t444, -t456, -t457, t463, t381, t513)
      t516 = t240 * t507
      t520 = FJET(XB1, XB2, s, t463, -t457, -t456, t444, t381, t513)
      t525 = FJET(XB1, XB2, s, -t235, t233, 0.0D0, 0.0D0, 0.0D0, t428)
      t527 = FJET(XB1, XB2, s, -t235, -t375, 0.0D0, -t372, t381, t414)
      t529 = FJET(XB1, XB2, s, -t329, 0.0D0, t327, 0.0D0, 0.0D0, t438)
      t531 = FJET(XB1, XB2, s, -t375, -t235, -t372, 0.0D0, t381, t414)
      t533 = FJET(XB1, XB2, s, -t372, 0.0D0, -t375, -t235, t381, t414)
      t535 = FJET(XB1, XB2, s, -t456, t444, t463, -t457, t381, t513)
      t540 = FJET(XB1, XB2, s, -t457, t463, t444, -t456, t381, t513)
      bbggh4n2e0 = t229 * t228 + t231 * t228 + t317 * t316 + t319 * t316
     # + t321 * t228 + t366 * t365 + t368 * t365 + t415 * t414 + t417 * 
     #t228 + t429 * t428 + t439 * t438 + t514 * t5 * t516 * t511 / 0.32D
     #2 + t520 * t5 * t516 * t511 / 0.32D2 + t525 * t428 + t527 * t414 +
     # t529 * t438 + t531 * t414 + t533 * t414 + t535 * t5 * t516 * t511
     # / 0.32D2 + t540 * t5 * t516 * t511 / 0.32D2

      end function



      doubleprecision function bbggh4n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t23 = lh * t5
      t25 = 0.180D3 * t23 * t18
      t27 = 0.1D1 / x1
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t8)
      t33 = -0.1D1 + x3
      t35 = Sqrt(-t30 * t33)
      t39 = 0.1D1 / (-t31 - 0.1D1 + x3 + 0.2D1 * t32 * t35)
      t40 = t18 * t39
      t43 = 0.1D1 / x3
      t44 = t43 * t27
      t47 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t48 = t47 - t18
      t50 = 0.1D1 / x2
      t51 = t50 * t27
      t59 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t60 = x2 ** 2
      t61 = t14 * t60
      t62 = -0.1D1 + x2
      t66 = log(-0.4D1 * t61 * t10 * t62)
      t70 = log(0.4D1 * t61 * t10)
      t81 = bbggh41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t87 = log(0.4D1 * t14 * t10)
      t95 = lh ** 2
      t97 = 0.3141592653589793D1 ** 2
      t99 = t87 ** 2
      t106 = x3 * t14
      t111 = log(-0.4D1 * t106 * t10 / t33)
      t115 = log(0.4D1 * t106 * t10)
      t127 = -(0.90D2 * t5 * (t6 - t17 * t18) - t25) * t27 / 0.5760D4 + 
     #t5 * (-t40 - t18) * t44 / 0.64D2 + t5 * t48 * t51 / 0.32D2 - t5 * 
     #(t40 - t47 + t18) * t43 * t50 / 0.64D2 - (0.90D2 * t5 * (-t59 + t6
     #6 * t47 + t6 - t70 * t18) + 0.180D3 * t23 * t48) * t50 / 0.5760D4 
     #- t5 * t81 / 0.128D3 - (-0.180D3 * lh - 0.90D2 * t87) * t5 * t6 / 
     #0.11520D5 - (0.180D3 * t87 * lh + 0.180D3 * t95 - 0.30D2 * t97 + 0
     #.45D2 * t99) * t5 * t18 / 0.11520D5 - (0.90D2 * t5 * t18 * (-t111 
     #* t39 - t115) + (0.90D2 * t5 * t6 - t25) * (t39 + 0.1D1)) * t43 / 
     #0.11520D5
      t128 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t127)
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t127)
      t132 = t2 * x1
      t133 = -0.1D1 + x1
      t134 = t2 * t133
      t135 = bbggh41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t137 = x1 * z
      t138 = -z - x1 + t137
      t139 = 0.1D1 / t138
      t141 = t133 ** 2
      t145 = log(-0.4D1 * t11 / t12 * t139 * t141)
      t146 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t148 = -t135 + t145 * t146
      t152 = 0.180D3 * t23 * t146
      t156 = x1 * x3
      t162 = Sqrt(x3 * t138 * t33)
      t171 = t5 * (t146 / (0.2D1 * t156 * z - 0.2D1 * t156 - t31 + 0.2D1
     # * t32 * t162 - 0.1D1 + x3) + t146) * t44 / 0.64D2
      t174 = t5 * t146 * t51 / 0.32D2
      t175 = -(0.90D2 * t5 * t148 + t152) * t27 / 0.5760D4 + t171 + t174
      t176 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t132, -t134, 0.0D0, t175)
      t178 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t134, t132, 0.0D0, t175)
      t180 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t127)
      t182 = x2 * x3
      t184 = 0.1D1 / (0.1D1 - x3 + t182)
      t185 = t182 * t184
      t186 = t2 * t185
      t188 = t2 * t33 * t184
      t192 = Sqrt(t30 * t62 * t33)
      t196 = 0.1D1 / (-t31 + t182 * z - 0.1D1 + x3 + 0.2D1 * t32 * t192)
      t198 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t185, x4)
      t200 = t198 * t43 * t50
      t202 = t5 * t196 * t200 / 0.64D2
      t203 = FJET(XB1, XB2, s, 0.0D0, t186, 0.0D0, -t188, 0.0D0, t202)
      t208 = FJET(XB1, XB2, s, 0.0D0, -t188, 0.0D0, t186, 0.0D0, t202)
      t213 = x2 * x1
      t215 = t2 * t213 * t139
      t218 = t62 * s * t1 * x1
      t219 = t1 ** 2
      t224 = s * t219 * x2 * x1 * t133 * t139
      t228 = 0.1D1 / (z + x1 - t137 - t213 + t213 * z)
      t230 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t234 = t5 * t138 * t228 * t230 * t50 * t27 / 0.32D2
      t235 = FJET(XB1, XB2, s, 0.0D0, -t215, -t134, -t218, t224, t234)
      t239 = t228 * t230 * t51
      t242 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t127)
      t250 = -(0.90D2 * t5 * t148 + t152) * t27 / 0.5760D4 + t171 + t174
      t251 = FJET(XB1, XB2, s, t132, -t134, 0.0D0, 0.0D0, 0.0D0, t250)
      t253 = FJET(XB1, XB2, s, t186, 0.0D0, -t188, 0.0D0, 0.0D0, t202)
      t258 = FJET(XB1, XB2, s, -t134, t132, 0.0D0, 0.0D0, 0.0D0, t250)
      t260 = FJET(XB1, XB2, s, -t134, -t218, 0.0D0, -t215, t224, t234)
      t265 = FJET(XB1, XB2, s, -t188, 0.0D0, t186, 0.0D0, 0.0D0, t202)
      t270 = FJET(XB1, XB2, s, -t218, -t134, -t215, 0.0D0, t224, t234)
      t275 = FJET(XB1, XB2, s, -t215, 0.0D0, -t218, -t134, t224, t234)
      bbggh4n2em1 = t128 * t127 + t130 * t127 + t176 * t175 + t178 * t17
     #5 + t180 * t127 + t203 * t5 * t196 * t200 / 0.64D2 + t208 * t5 * t
     #196 * t200 / 0.64D2 + t235 * t5 * t138 * t239 / 0.32D2 + t242 * t1
     #27 + t251 * t250 + t253 * t5 * t196 * t200 / 0.64D2 + t258 * t250 
     #+ t260 * t5 * t138 * t239 / 0.32D2 + t265 * t5 * t196 * t200 / 0.6
     #4D2 + t270 * t5 * t138 * t239 / 0.32D2 + t275 * t5 * t138 * t239 /
     # 0.32D2

      end function



      doubleprecision function bbggh4n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t17 = bbggh41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t35 = x3 * z
      t37 = cos(t24)
      t40 = Sqrt(-t35 * (-0.1D1 + x3))
      t50 = -t7 * t8 / 0.64D2 - t5 * (-t11 + t6) / x2 / 0.64D2 - t5 * t1
     #7 / 0.128D3 - (-0.180D3 * lh - 0.90D2 * t29) * t5 * t6 / 0.11520D5
     # - t7 * (0.1D1 / (-0.2D1 * t35 - 0.1D1 + x3 + 0.2D1 * t37 * t40) +
     # 0.1D1) / x3 / 0.128D3
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t50)
      t55 = t2 * x1
      t57 = t2 * (-0.1D1 + x1)
      t58 = bbggh41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t61 = t5 * t58 * t8 / 0.64D2
      t62 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t55, -t57, 0.0D0, t61)
      t64 = t58 * t8
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t57, t55, 0.0D0, t61)
      t71 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t50)
      t73 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t50)
      t75 = FJET(XB1, XB2, s, t55, -t57, 0.0D0, 0.0D0, 0.0D0, t61)
      t79 = FJET(XB1, XB2, s, -t57, t55, 0.0D0, 0.0D0, 0.0D0, t61)
      bbggh4n2em2 = t51 * t50 + t53 * t50 + t62 * t5 * t64 / 0.64D2 + t6
     #7 * t5 * t64 / 0.64D2 + t71 * t50 + t73 * t50 + t75 * t5 * t64 / 0
     #.64D2 + t79 * t5 * t64 / 0.64D2

      end function



      doubleprecision function bbggh4n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t8 = t5 * t6 / 0.128D3
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t15 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t18 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      bbggh4n2em3 = -t9 * t5 * t6 / 0.128D3 - t12 * t5 * t6 / 0.128D3 - 
     #t15 * t5 * t6 / 0.128D3 - t18 * t5 * t6 / 0.128D3

      end function



      doubleprecision function bbggh4n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh41J1
      doubleprecision bbggh41J2
      doubleprecision bbggh41J3
      bbggh4n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbggh41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t11 = 0.1D1 / t10
      t12 = z ** 2
      t17 = t4 ** 2
      t19 = t2 * t17 * t3
      t20 = x1 ** 2
      t21 = t20 * x1
      t23 = x2 ** 2
      t24 = t7 ** 2
      t26 = t10 ** 2
      t27 = 0.1D1 / t26
      t30 = t24 * t7
      t32 = 0.1D1 - x2
      t35 = z + x1 * t32 * t3
      t37 = 0.1D1 / t26 / t10
      t39 = t23 * t20
      t43 = t2 * t4 * t3
      t45 = t35 * t11
      t46 = 0.1D1 - x3
      t47 = t46 ** 2
      t55 = cos(x4 * 0.3141592653589793D1)
      t60 = Sqrt(x3 * t32 * t10 * x2 * t46)
      t63 = t46 * t32 * t10 + x2 * x3 + 0.2D1 * t55 * t60
      t64 = t63 ** 2
      t77 = t2 * t3
      t88 = t2 * t17
      t92 = x2 * x1
      t125 = 0.4D1 * t5 * x2 * x1 * t7 * t11 * t12 + t19 * t21 * t23 * t
     #24 * t27 + t19 * t30 * t35 * t37 * t39 + t43 * t30 * t45 * t47 + t
     #43 * t21 * t27 * t64 + 0.3D1 * t5 * t20 * z * t11 * t63 + 0.3D1 * 
     #t5 * t24 * t45 * t46 * z + 0.4D1 * t77 * t7 * t45 * t12 + 0.3D1 * 
     #t43 * t20 * z * x2 * t7 * t11 + t88 * t30 * t35 * t27 * t46 * t92 
     #+ 0.3D1 * t43 * t24 * t35 * t27 * z * t92 + t88 * t21 * t27 * t63 
     #* x2 * t7 + 0.3D1 * t2 * z * t17 * t39 * t24 * t27 + t2 * t17 * t4
     # * t23 * x2 * t21 * t30 * t37 + 0.2D1 * t2 * t12 * z + 0.4D1 * t77
     # * x1 * t12
      bbggh41J1 = -0.16D2 / 0.3D1 * wd * t125

      end function
  
   
 

      doubleprecision function bbggh41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t8 = x2 ** 2
      t9 = x1 ** 2
      t10 = t8 * t9
      t11 = 0.1D1 - x1
      t12 = t11 ** 2
      t14 = z + x1 * t4
      t15 = t14 ** 2
      t16 = 0.1D1 / t15
      t24 = t9 * x1
      t25 = t12 * t11
      t28 = 0.1D1 / t15 / t14
      t32 = t2 * t6 * t4
      t38 = t2 * t5 * t4
      t41 = 0.1D1 / t14
      t46 = 0.1D1 - x3
      t47 = 0.1D1 - x2
      t52 = cos(x4 * 0.3141592653589793D1)
      t57 = Sqrt(x3 * t47 * t14 * x2 * t46)
      t60 = t46 * t47 * t14 + x2 * x3 + 0.2D1 * t52 * t57
      t61 = t60 ** 2
      t64 = t2 * t6
      t67 = z + x1 * t47 * t4
      t71 = x2 * x1
      t74 = t2 * t5
      t76 = t67 * t41
      t90 = t46 ** 2
      t102 = -t2 * z * t6 * t10 * t12 * t16 - t2 * t6 * t5 * t8 * x2 * t
     #24 * t25 * t28 - t32 * t24 * t8 * t12 * t16 - t38 * t9 * z * x2 * 
     #t11 * t41 - t38 * t24 * t16 * t61 - t64 * t25 * t67 * t16 * t46 * 
     #t71 - t74 * t12 * t76 * t46 * z - t64 * t24 * t16 * t60 * x2 * t11
     # - t74 * t9 * z * t41 * t60 - t38 * t25 * t76 * t90 - t38 * t12 * 
     #t67 * t16 * z * t71 - t32 * t25 * t67 * t28 * t10
      bbggh41J2 = -0.16D2 / 0.3D1 * wd * t102

      end function
  
   
 

      doubleprecision function bbggh41J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = 0.1D1 - x2
      t13 = z + x1 * t10 * t3
      t17 = z + x1 * t3
      t18 = t17 ** 2
      t19 = 0.1D1 / t18
      t20 = 0.1D1 - x3
      t22 = x2 * x1
      t26 = t2 * t5 * t3
      t29 = 0.1D1 / t18 / t17
      t31 = x2 ** 2
      t32 = x1 ** 2
      t33 = t31 * t32
      t36 = t32 * x1
      t42 = t2 * t4 * t3
      t71 = cos(x4 * 0.3141592653589793D1)
      t76 = Sqrt(x3 * t10 * t17 * x2 * t20)
      bbggh41J3 = -0.16D2 / 0.3D1 * wd * (-t6 * t9 * t13 * t19 * t20 * t
     #22 - t26 * t9 * t13 * t29 * t33 - t26 * t36 * t31 * t8 * t19 - t42
     # * t32 * z * x2 * t7 / t17 - t2 * t5 * t4 * t31 * x2 * t36 * t9 * 
     #t29 - t2 * z * t5 * t33 * t8 * t19 - t42 * t8 * t13 * t19 * z * t2
     #2 - t6 * t36 * t19 * (t20 * t10 * t17 + x2 * x3 + 0.2D1 * t71 * t7
     #6) * x2 * t7)

      end function
  
 