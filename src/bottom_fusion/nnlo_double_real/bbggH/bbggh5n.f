  
      subroutine bbggh5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh51J1  
      doubleprecision bbggh51J2  
      doubleprecision bbggh52J1  
      doubleprecision bbggh52J2  
      doubleprecision bbggh5n1e1  
      doubleprecision bbggh5n1e0  
      doubleprecision bbggh5n1em1  
      doubleprecision bbggh5n1em2  
      doubleprecision bbggh5n1em3  
      doubleprecision bbggh5n1em4  
      doubleprecision bbggh5n2e1  
      doubleprecision bbggh5n2e0  
      doubleprecision bbggh5n2em1  
      doubleprecision bbggh5n2em2  
      doubleprecision bbggh5n2em3  
      doubleprecision bbggh5n2em4  
      doubleprecision bbggh5n3e1  
      doubleprecision bbggh5n3e0  
      doubleprecision bbggh5n3em1  
      doubleprecision bbggh5n3em2  
      doubleprecision bbggh5n3em3  
      doubleprecision bbggh5n3em4  
      doubleprecision bbggh5n4e1  
      doubleprecision bbggh5n4e0  
      doubleprecision bbggh5n4em1  
      doubleprecision bbggh5n4em2  
      doubleprecision bbggh5n4em3  
      doubleprecision bbggh5n4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh5n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh5n3e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh5n4e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh5n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh5n3e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh5n4e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh5n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh5n3em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh5n4em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh5n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh5n3em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh5n4em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh5n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh5n3em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh5n4em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh5n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh5n3em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh5n4em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = lh * t4
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t15 = log(0.4D1 * t13)
      t16 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t18 = t15 ** 2
      t19 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t25 = lh ** 2
      t27 = 0.3141592653589793D1 ** 2
      t29 = 0.180D3 * t25 - 0.30D2 * t27
      t30 = t29 * t4
      t36 = t18 * t15
      t43 = 0.120D3 * t25 * lh
      t45 = 0.60D2 * lh * t27
      t46 = -0.2884936567583026D3 - t43 + t45
      t47 = t46 * t4
      t48 = t47 * t19
      t50 = 0.1D1 / x1
      t53 = z * t16
      t54 = t6 * x3
      t55 = t9 * t12
      t56 = -0.1D1 + x3
      t57 = 0.1D1 / t56
      t58 = t55 * t57
      t61 = log(-0.4D1 * t54 * t58)
      t62 = t61 * z
      t65 = cos(t7)
      t67 = Sqrt(-x3 * t56)
      t72 = 0.1D1 / (-z - x3 + 0.2D1 * t65 * t67 * z)
      t76 = log(0.4D1 * t54 * t55)
      t82 = t61 ** 2
      t83 = t82 * z
      t88 = t76 ** 2
      t97 = t19 + z * t19 * t72
      t98 = t30 * t97
      t100 = 0.1D1 / x3
      t104 = x2 ** 2
      t105 = x3 * t104
      t106 = t105 * t6
      t109 = log(-0.4D1 * t106 * t58)
      t110 = t109 * z
      t114 = t105 * t13
      t116 = log(0.4D1 * t114)
      t126 = 0.1D1 / x2
      t127 = t126 * t50
      t130 = t104 * t6
      t133 = log(0.4D1 * t130 * t55)
      t138 = t133 ** 2
      t145 = t30 * t19
      t151 = log(0.4D1 * t55)
      t153 = t151 ** 2
      t156 = t153 * t151
      t159 = (-t151 * t29 - 0.90D2 * t153 * lh - 0.2884936567583026D3 - 
     #t43 + t45 - 0.15D2 * t156) * t4
      t162 = t153 ** 2
      t166 = t27 ** 2
      t167 = t25 ** 2
      t176 = (0.15D2 / 0.4D1 * t162 - t151 * t46 + 0.5769873135166051D3 
     #* lh + t166 + 0.60D2 * t167 - 0.60D2 * t25 * t27 + t153 * t29 / 0.
     #2D1 + 0.30D2 * t156 * lh) * t4
      t184 = x3 * t9
      t188 = log(-0.4D1 * t184 * t12 * t57)
      t189 = t188 ** 2
      t194 = log(0.4D1 * t184 * t12)
      t195 = t194 ** 2
      t197 = -t189 * z * t72 / 0.2D1 - t195 / 0.2D1
      t204 = t194 + t188 * z * t72
      t212 = t189 * t188 * z * t72 / 0.6D1 + t195 * t194 / 0.6D1
      t218 = -0.1D1 - z * t72
      t223 = t104 * t9
      t226 = log(0.4D1 * t223 * t12)
      t228 = t226 ** 2
      t239 = t228 * t226
      t250 = log(-0.4D1 * t105 * t58)
      t251 = t250 * z
      t257 = log(0.4D1 * t105 * t55)
      t263 = t250 ** 2
      t264 = t263 * z
      t269 = t257 ** 2
      t280 = (-0.180D3 * t5 * (t15 * t16 - t18 * t19 / 0.2D1) + t30 * (-
     #t16 + t15 * t19) + 0.90D2 * t4 * (-t18 * t16 / 0.2D1 + t36 * t19 /
     # 0.6D1) - t48) * t50 / 0.5760D4 - (-0.180D3 * t5 * (t16 + (t53 - t
     #62 * t19) * t72 - t76 * t19) + 0.90D2 * t4 * ((-t62 * t16 + t83 * 
     #t19 / 0.2D1) * t72 + t88 * t19 / 0.2D1 - t76 * t16) + t98) * t100 
     #* t50 / 0.5760D4 + (0.90D2 * t4 * (-(t53 - t110 * t19) * t72 + t11
     #6 * t19 - t16) + 0.180D3 * t5 * t97) * t100 * t127 / 0.2880D4 + (-
     #0.180D3 * t5 * (-t16 + t133 * t19) + 0.90D2 * t4 * (-t138 * t19 / 
     #0.2D1 + t133 * t16) - t145) * t126 * t50 / 0.2880D4 - t159 * t16 /
     # 0.11520D5 - t176 * t19 / 0.11520D5 + ((0.90D2 * t4 * t16 - 0.180D
     #3 * t5 * t19) * t197 + (-0.180D3 * t5 * t16 + t145) * t204 + 0.90D
     #2 * t4 * t19 * t212 + (t30 * t16 + t48) * t218) * t100 / 0.11520D5
     # + (-0.180D3 * t5 * (t226 * t16 - t228 * t19 / 0.2D1) + t30 * (-t1
     #6 + t226 * t19) + 0.90D2 * t4 * (-t228 * t16 / 0.2D1 + t239 * t19 
     #/ 0.6D1) - t48) * t126 / 0.5760D4 - (-0.180D3 * t5 * (t16 + (t53 -
     # t251 * t19) * t72 - t257 * t19) + 0.90D2 * t4 * ((-t251 * t16 + t
     #264 * t19 / 0.2D1) * t72 + t269 * t19 / 0.2D1 - t257 * t16) + t98)
     # * t100 * t126 / 0.5760D4
      t281 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t280)
      t283 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t280)
      t285 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t287 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t303 = t47 * t287
      t308 = z * t285
      t328 = z * t287 * t72 + t287
      t329 = t30 * t328
      t358 = t30 * t287
      t427 = (-0.180D3 * t5 * (t15 * t285 - t18 * t287 / 0.2D1) + t30 * 
     #(t15 * t287 - t285) + 0.90D2 * t4 * (-t18 * t285 / 0.2D1 + t36 * t
     #287 / 0.6D1) - t303) * t50 / 0.5760D4 - (-0.180D3 * t5 * (-t76 * t
     #287 + (t308 - t62 * t287) * t72 + t285) + 0.90D2 * t4 * ((-t62 * t
     #285 + t83 * t287 / 0.2D1) * t72 - t76 * t285 + t88 * t287 / 0.2D1)
     # + t329) * t100 * t50 / 0.5760D4 + (0.90D2 * t4 * (-(t308 - t110 *
     # t287) * t72 + t116 * t287 - t285) + 0.180D3 * t5 * t328) * t100 *
     # t127 / 0.2880D4 + (-0.180D3 * t5 * (-t285 + t133 * t287) + 0.90D2
     # * t4 * (t133 * t285 - t138 * t287 / 0.2D1) - t358) * t126 * t50 /
     # 0.2880D4 - t159 * t285 / 0.11520D5 - t176 * t287 / 0.11520D5 + ((
     #0.90D2 * t4 * t285 - 0.180D3 * t5 * t287) * t197 + (-0.180D3 * t5 
     #* t285 + t358) * t204 + 0.90D2 * t4 * t287 * t212 + (t30 * t285 + 
     #t303) * t218) * t100 / 0.11520D5 + (-0.180D3 * t5 * (t226 * t285 -
     # t228 * t287 / 0.2D1) + t30 * (t226 * t287 - t285) + 0.90D2 * t4 *
     # (-t228 * t285 / 0.2D1 + t239 * t287 / 0.6D1) - t303) * t126 / 0.5
     #760D4 - (-0.180D3 * t5 * (-t257 * t287 + (t308 - t251 * t287) * t7
     #2 + t285) + 0.90D2 * t4 * ((-t251 * t285 + t264 * t287 / 0.2D1) * 
     #t72 - t257 * t285 + t269 * t287 / 0.2D1) + t329) * t100 * t126 / 0
     #.5760D4
      t428 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t427)
      t430 = t2 * x1
      t431 = -0.1D1 + x1
      t432 = x1 * z
      t433 = 0.1D1 - x1 + t432
      t434 = 0.1D1 / t433
      t436 = t2 * t431 * t434
      t437 = t1 ** 2
      t438 = s * t437
      t440 = x1 * t431 * t434
      t441 = t438 * t440
      t442 = t12 * t434
      t443 = t431 ** 2
      t444 = t442 * t443
      t447 = log(0.4D1 * t10 * t444)
      t448 = t447 ** 2
      t449 = -t431
      t450 = bbggh51J1(s, XB1, XB2, z, lh, wd, t449, 0.10D1, 0.10D1, x4)
      t453 = bbggh51J2(s, XB1, XB2, z, lh, wd, t449, 0.10D1, 0.10D1, x4)
      t461 = t448 * t447
      t473 = t54 * t9
      t476 = log(0.4D1 * t473 * t444)
      t478 = z * t453
      t480 = t442 * t443 * t57
      t483 = log(-0.4D1 * t473 * t480)
      t484 = t483 * z
      t488 = x1 * x3
      t489 = 0.2D1 * t488
      t490 = x3 * t433
      t492 = Sqrt(-t490 * t56)
      t496 = x1 * t11
      t497 = x3 * t11
      t498 = t497 * x1
      t500 = 0.2D1 * t54 * z
      t501 = t54 * t11
      t502 = t488 * z
      t503 = 0.3D1 * t502
      t504 = -t54 - z + t489 + 0.2D1 * t65 * t492 * z - x3 + t432 - t496
     # + t498 + t500 - t501 - t503
      t505 = 0.1D1 / t504
      t511 = t476 ** 2
      t515 = t483 ** 2
      t516 = t515 * z
      t526 = t433 * t505
      t528 = -t450 - z * t450 * t526
      t534 = t105 * t10
      t537 = log(-0.4D1 * t534 * t480)
      t538 = t537 * z
      t543 = t434 * t443
      t547 = log(0.4D1 * t106 * t55 * t543)
      t559 = t130 * t9
      t562 = log(0.4D1 * t559 * t444)
      t567 = t562 ** 2
      t579 = (-0.180D3 * t5 * (t448 * t450 / 0.2D1 - t447 * t453) + t30 
     #* (-t447 * t450 + t453) + 0.90D2 * t4 * (-t461 * t450 / 0.6D1 + t4
     #48 * t453 / 0.2D1) + t47 * t450) * t50 / 0.5760D4 - (-0.180D3 * t5
     # * (-t453 + t476 * t450 + (-t478 + t484 * t450) * t433 * t505) + 0
     #.90D2 * t4 * (t476 * t453 - t511 * t450 / 0.2D1 + (t484 * t453 - t
     #516 * t450 / 0.2D1) * t433 * t505) + t30 * t528) * t100 * t50 / 0.
     #5760D4 + (0.90D2 * t4 * (-(-t478 + t538 * t450) * t433 * t505 - t5
     #47 * t450 + t453) + 0.180D3 * t5 * t528) * t100 * t127 / 0.2880D4 
     #+ (-0.180D3 * t5 * (-t562 * t450 + t453) + 0.90D2 * t4 * (t567 * t
     #450 / 0.2D1 - t562 * t453) + t30 * t450) * t126 * t50 / 0.2880D4
      t580 = FJET(XB1, XB2, s, 0.0D0, t430, -t436, 0.0D0, -t441, t579)
      t582 = x2 * s
      t583 = t582 * t1
      t584 = -0.1D1 + x2
      t585 = t584 * s
      t586 = t585 * t1
      t587 = t12 * t584
      t590 = log(-0.4D1 * t223 * t587)
      t591 = t590 ** 2
      t592 = -t584
      t593 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t592, 0.10D1, x4)
      t596 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t592, 0.10D1, x4)
      t604 = t591 * t590
      t616 = t55 * t584
      t619 = log(-0.4D1 * t105 * t616)
      t625 = t619 ** 2
      t631 = t30 * t593
      t638 = log(-0.4D1 * t106 * t616)
      t651 = log(-0.4D1 * t130 * t616)
      t656 = t651 ** 2
      t667 = (-0.180D3 * t5 * (t591 * t593 / 0.2D1 - t590 * t596) + t30 
     #* (-t590 * t593 + t596) + 0.90D2 * t4 * (-t604 * t593 / 0.6D1 + t5
     #91 * t596 / 0.2D1) + t47 * t593) * t126 / 0.5760D4 - (-0.180D3 * t
     #5 * (-t596 + t619 * t593) + 0.90D2 * t4 * (t619 * t596 - t625 * t5
     #93 / 0.2D1) - t631) * t100 * t126 / 0.5760D4 + (0.90D2 * t4 * (-t6
     #38 * t593 + t596) - 0.180D3 * t5 * t593) * t100 * t127 / 0.2880D4 
     #+ (-0.180D3 * t5 * (-t651 * t593 + t596) + 0.90D2 * t4 * (t656 * t
     #593 / 0.2D1 - t651 * t596) + t631) * t126 * t50 / 0.2880D4
      t668 = FJET(XB1, XB2, s, 0.0D0, t583, 0.0D0, -t586, 0.0D0, t667)
      t670 = x2 * x3
      t673 = Sqrt(x3 * t584 * t56)
      t674 = t65 * t673
      t676 = 0.2D1 * t674 * x2
      t678 = 0.1D1 - x3 + t670
      t679 = 0.1D1 / t678
      t681 = t2 * (0.1D1 - x3 - x2 + t670 + t105 + t676) * t679
      t686 = t2 * x2 * (-0.1D1 + t670 + 0.2D1 * t674) * t679
      t687 = x2 * z
      t688 = t687 - x2 - z
      t689 = t56 * t679
      t690 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t592, -t689, x4)
      t691 = t688 * t690
      t693 = t678 ** 2
      t694 = 0.1D1 / t693
      t696 = t587 * t56 * t694
      t699 = log(0.4D1 * t105 * t9 * t696)
      t700 = t699 * t688
      t701 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t592, -t689, x4)
      t704 = t670 * z
      t705 = t105 * z
      t711 = 0.1D1 / (-t687 - t704 + t705 + x2 + z + x3 - t105 - 0.2D1 *
     # t674 * z + 0.2D1 * t674 * t687 - t676)
      t716 = t699 ** 2
      t717 = t716 * t688
      t725 = t688 * t701 * t711
      t733 = log(0.4D1 * t534 * t696)
      t734 = t733 * t688
      t746 = -(0.180D3 * t5 * (t691 - t700 * t701) * t711 - 0.90D2 * t4 
     #* (-t700 * t690 + t717 * t701 / 0.2D1) * t711 - t30 * t725) * t100
     # * t126 / 0.5760D4 + (0.90D2 * t4 * (t691 - t734 * t701) * t711 - 
     #0.180D3 * t5 * t725) * t100 * t127 / 0.2880D4
      t747 = FJET(XB1, XB2, s, 0.0D0, t681, 0.0D0, -t686, 0.0D0, t746)
      t749 = t1 * t431
      t751 = t585 * t749 * t434
      t752 = t582 * t749
      t754 = t438 * t584 * t440
      t756 = t442 * t443 * t584
      t759 = log(-0.4D1 * t534 * t756)
      t760 = bbggh52J1(s, XB1, XB2, z, lh, wd, t449, t592, 0.10D1, x4)
      t762 = bbggh52J2(s, XB1, XB2, z, lh, wd, t449, t592, 0.10D1, x4)
      t773 = log(-0.4D1 * t559 * t756)
      t779 = t773 ** 2
      t790 = (0.90D2 * t4 * (t759 * t760 - t762) + 0.180D3 * t5 * t760) 
     #* t100 * t127 / 0.2880D4 + (-0.180D3 * t5 * (-t762 + t773 * t760) 
     #+ 0.90D2 * t4 * (t773 * t762 - t779 * t760 / 0.2D1) - t30 * t760) 
     #* t126 * t50 / 0.2880D4
      t791 = FJET(XB1, XB2, s, 0.0D0, t751, t430, -t752, t754, t790)
      t793 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t592, 0.10D1, x4)
      t795 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t592, 0.10D1, x4)
      t815 = t30 * t793
      t854 = (0.90D2 * t4 * (-t638 * t793 + t795) - 0.180D3 * t5 * t793)
     # * t100 * t127 / 0.2880D4 + (-0.180D3 * t5 * (-t651 * t793 + t795)
     # + 0.90D2 * t4 * (t656 * t793 / 0.2D1 - t651 * t795) + t815) * t12
     #6 * t50 / 0.2880D4 + (-0.180D3 * t5 * (t591 * t793 / 0.2D1 - t590 
     #* t795) + t30 * (t795 - t590 * t793) + 0.90D2 * t4 * (-t604 * t793
     # / 0.6D1 + t591 * t795 / 0.2D1) + t47 * t793) * t126 / 0.5760D4 - 
     #(-0.180D3 * t5 * (-t795 + t619 * t793) + 0.90D2 * t4 * (-t625 * t7
     #93 / 0.2D1 + t619 * t795) - t815) * t100 * t126 / 0.5760D4
      t855 = FJET(XB1, XB2, s, 0.0D0, -t586, 0.0D0, t583, 0.0D0, t854)
      t857 = bbggh52J1(s, XB1, XB2, z, lh, wd, t449, 0.10D1, 0.10D1, x4)
      t860 = bbggh52J2(s, XB1, XB2, z, lh, wd, t449, 0.10D1, 0.10D1, x4)
      t880 = z * t860
      t902 = -z * t857 * t526 - t857
      t938 = (-0.180D3 * t5 * (t448 * t857 / 0.2D1 - t447 * t860) + t30 
     #* (t860 - t447 * t857) + 0.90D2 * t4 * (-t461 * t857 / 0.6D1 + t44
     #8 * t860 / 0.2D1) + t47 * t857) * t50 / 0.5760D4 - (-0.180D3 * t5 
     #* (-t860 + t476 * t857 + (-t880 + t484 * t857) * t433 * t505) + 0.
     #90D2 * t4 * ((t484 * t860 - t516 * t857 / 0.2D1) * t433 * t505 - t
     #511 * t857 / 0.2D1 + t476 * t860) + t30 * t902) * t100 * t50 / 0.5
     #760D4 + (0.90D2 * t4 * (t860 - (-t880 + t538 * t857) * t433 * t505
     # - t547 * t857) + 0.180D3 * t5 * t902) * t100 * t127 / 0.2880D4 + 
     #(-0.180D3 * t5 * (-t562 * t857 + t860) + 0.90D2 * t4 * (-t562 * t8
     #60 + t567 * t857 / 0.2D1) + t30 * t857) * t126 * t50 / 0.2880D4
      t939 = FJET(XB1, XB2, s, 0.0D0, -t436, t430, 0.0D0, -t441, t938)
      t941 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t592, -t689, x4)
      t942 = t688 * t941
      t943 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t592, -t689, x4)
      t957 = t688 * t943 * t711
      t974 = -(0.180D3 * t5 * (t942 - t700 * t943) * t711 - 0.90D2 * t4 
     #* (-t700 * t941 + t717 * t943 / 0.2D1) * t711 - t30 * t957) * t100
     # * t126 / 0.5760D4 + (0.90D2 * t4 * (t942 - t734 * t943) * t711 - 
     #0.180D3 * t5 * t957) * t100 * t127 / 0.2880D4
      t975 = FJET(XB1, XB2, s, 0.0D0, -t686, 0.0D0, t681, 0.0D0, t974)
      t977 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t427)
      t979 = FJET(XB1, XB2, s, t430, 0.0D0, 0.0D0, -t436, -t441, t579)
      t981 = t281 * t280 + t283 * t280 + t428 * t427 + t580 * t579 + t66
     #8 * t667 + t747 * t746 + t791 * t790 + t855 * t854 + t939 * t938 +
     # t975 * t974 + t977 * t427 + t979 * t579
      t982 = bbggh51J1(s, XB1, XB2, z, lh, wd, t449, t592, 0.10D1, x4)
      t984 = bbggh51J2(s, XB1, XB2, z, lh, wd, t449, t592, 0.10D1, x4)
      t1008 = (0.90D2 * t4 * (t759 * t982 - t984) + 0.180D3 * t5 * t982)
     # * t100 * t127 / 0.2880D4 + (-0.180D3 * t5 * (-t984 + t773 * t982)
     # + 0.90D2 * t4 * (t773 * t984 - t779 * t982 / 0.2D1) - t30 * t982)
     # * t126 * t50 / 0.2880D4
      t1009 = FJET(XB1, XB2, s, t430, -t752, 0.0D0, t751, t754, t1008)
      t1011 = FJET(XB1, XB2, s, t583, 0.0D0, -t586, 0.0D0, 0.0D0, t667)
      t1013 = FJET(XB1, XB2, s, t681, 0.0D0, -t686, 0.0D0, 0.0D0, t746)
      t1015 = FJET(XB1, XB2, s, t751, 0.0D0, -t752, t430, t754, t790)
      t1018 = t430 * t670 * t679
      t1019 = t2 * t431
      t1020 = t584 * t56
      t1022 = Sqrt(t490 * t1020)
      t1023 = t65 * t1022
      t1025 = 0.2D1 * t1023 * x2
      t1026 = t105 * x1
      t1027 = t105 * t432
      t1031 = t1019 * (t1025 + t105 - x2 + t670 + 0.1D1 - x3 - t1026 + t
     #1027) * t434 * t679
      t1035 = t56 * s * t1 * x1 * t679
      t1041 = t1019 * x2 * (-0.1D1 + t670 + x1 - t488 - t432 + t502 + 0.
     #2D1 * t1023) * t434 * t679
      t1042 = x2 * x1
      t1043 = t1042 * z
      t1044 = x2 - t1042 + z - t687 + t1043
      t1045 = t433 * t1044
      t1046 = bbggh52J2(s, XB1, XB2, z, lh, wd, t449, t592, -t689, x4)
      t1052 = log(0.4D1 * t114 * t543 * t1020 * t694)
      t1053 = t1052 * t433
      t1054 = bbggh52J1(s, XB1, XB2, z, lh, wd, t449, t592, -t689, x4)
      t1055 = t1044 * t1054
      t1073 = t432 - t54 + t489 - t496 + t1027 - 0.2D1 * t1023 * t687 - 
     #0.2D1 * t1023 * t1042 - t497 * t1042 - 0.2D1 * t54 * t687 + t54 * 
     #t11 * x2 + 0.2D1 * t670 * t432 + 0.2D1 * t1023 * t1043 + t105 + t6
     #87 + t704 - t705 + 0.2D1 * t1042
      t1074 = x2 * t6
      t1084 = -t1074 - x2 + t498 + t500 - t501 - t503 - z - x3 + t1025 -
     # t1026 - 0.3D1 * t1043 + 0.2D1 * t1023 * z + t54 * x2 - t670 * x1 
     #+ t1042 * t11 + 0.2D1 * t1074 * z - t1074 * t11
      t1086 = 0.1D1 / (t1073 + t1084)
      t1089 = t5 * t433
      t1093 = 0.90D2 * t4 * (-t1045 * t1046 + t1053 * t1055) * t1086 + 0
     #.180D3 * t1089 * t1055 * t1086
      t1096 = t1093 * t100 * t127 / 0.2880D4
      t1097 = FJET(XB1, XB2, s, t1018, -t1031, -t1035, t1041, t754, t109
     #6)
      t1100 = t100 * t126 * t50
      t1103 = bbggh51J2(s, XB1, XB2, z, lh, wd, t449, t592, -t689, x4)
      t1105 = bbggh51J1(s, XB1, XB2, z, lh, wd, t449, t592, -t689, x4)
      t1106 = t1044 * t1105
      t1115 = 0.90D2 * t4 * (-t1045 * t1103 + t1053 * t1106) * t1086 + 0
     #.180D3 * t1089 * t1106 * t1086
      t1118 = t1115 * t100 * t127 / 0.2880D4
      t1119 = FJET(XB1, XB2, s, t1041, -t1035, -t1031, t1018, t754, t111
     #8)
      t1123 = FJET(XB1, XB2, s, -t586, 0.0D0, t583, 0.0D0, 0.0D0, t854)
      t1125 = FJET(XB1, XB2, s, -t436, 0.0D0, 0.0D0, t430, -t441, t938)
      t1127 = FJET(XB1, XB2, s, -t752, t430, t751, 0.0D0, t754, t1008)
      t1129 = FJET(XB1, XB2, s, -t686, 0.0D0, t681, 0.0D0, 0.0D0, t974)
      t1131 = FJET(XB1, XB2, s, -t1035, t1041, t1018, -t1031, t754, t111
     #8)
      t1135 = FJET(XB1, XB2, s, -t1031, t1018, t1041, -t1035, t754, t109
     #6)
      t1139 = t1009 * t1008 + t1011 * t667 + t1013 * t746 + t1015 * t790
     # + t1097 * t1093 * t1100 / 0.2880D4 + t1119 * t1115 * t1100 / 0.28
     #80D4 + t1123 * t854 + t1125 * t938 + t1127 * t1008 + t1129 * t974 
     #+ t1131 * t1115 * t1100 / 0.2880D4 + t1135 * t1093 * t1100 / 0.288
     #0D4
      bbggh5n1e1 = t981 + t1139

      end function



      doubleprecision function bbggh5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t6 = z * t5
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
      t21 = t20 * z
      t22 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t25 = cos(t9)
      t27 = Sqrt(-x3 * t15)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t8 * t14)
      t41 = lh * t4
      t44 = t22 + z * t22 * t32
      t46 = 0.180D3 * t41 * t44
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t55 = 0.1D1 / x2
      t57 = t48 * t55 * t50
      t60 = x2 ** 2
      t61 = t60 * t7
      t64 = log(0.4D1 * t61 * t14)
      t70 = 0.180D3 * t41 * t22
      t75 = t7 * t11
      t78 = log(0.4D1 * t75 * t13)
      t84 = t78 ** 2
      t90 = lh ** 2
      t91 = 0.180D3 * t90
      t92 = 0.3141592653589793D1 ** 2
      t93 = 0.30D2 * t92
      t94 = t91 - t93
      t95 = t94 * t4
      t96 = t95 * t22
      t103 = x3 * t11
      t106 = log(0.4D1 * t103 * t13)
      t110 = log(-0.4D1 * t103 * t13 * t16)
      t113 = t106 + t110 * z * t32
      t116 = t110 ** 2
      t119 = t106 ** 2
      t121 = -t116 * z * t32 / 0.2D1 - t119 / 0.2D1
      t128 = -0.1D1 - z * t32
      t133 = t60 * x3
      t136 = log(-0.4D1 * t133 * t17)
      t137 = t136 * z
      t143 = log(0.4D1 * t133 * t14)
      t152 = t60 * t11
      t155 = log(0.4D1 * t152 * t13)
      t161 = t155 ** 2
      t171 = log(0.4D1 * t14)
      t174 = t171 ** 2
      t177 = (0.180D3 * t171 * lh + t91 - t93 + 0.45D2 * t174) * t4
      t190 = (-t171 * t94 - 0.90D2 * t174 * lh - 0.2884936567583026D3 - 
     #0.120D3 * t90 * lh + 0.60D2 * lh * t92 - 0.15D2 * t174 * t171) * t
     #4
      t193 = -(0.90D2 * t4 * (t5 + (t6 - t21 * t22) * t32 - t36 * t22) -
     # t46) * t48 * t50 / 0.5760D4 - t4 * t44 * t57 / 0.32D2 + (0.90D2 *
     # t4 * (-t5 + t64 * t22) + t70) * t55 * t50 / 0.2880D4 + (-0.180D3 
     #* t41 * (-t5 + t78 * t22) + 0.90D2 * t4 * (t78 * t5 - t84 * t22 / 
     #0.2D1) - t96) * t50 / 0.5760D4 + ((0.90D2 * t4 * t5 - t70) * t113 
     #+ 0.90D2 * t4 * t22 * t121 + (-0.180D3 * t41 * t5 + t96) * t128) *
     # t48 / 0.11520D5 - (0.90D2 * t4 * (t5 + (t6 - t137 * t22) * t32 - 
     #t143 * t22) - t46) * t48 * t55 / 0.5760D4 + (-0.180D3 * t41 * (-t5
     # + t155 * t22) + 0.90D2 * t4 * (t155 * t5 - t161 * t22 / 0.2D1) - 
     #t96) * t55 / 0.5760D4 - t177 * t5 / 0.11520D5 - t190 * t22 / 0.115
     #20D5
      t194 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t193)
      t196 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t193)
      t198 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t200 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t201 = z * t200
      t210 = z * t198 * t32 + t198
      t212 = 0.180D3 * t41 * t210
      t226 = 0.180D3 * t41 * t198
      t241 = t95 * t198
      t287 = -(0.90D2 * t4 * (-t36 * t198 + (t201 - t21 * t198) * t32 + 
     #t200) - t212) * t48 * t50 / 0.5760D4 - t4 * t210 * t57 / 0.32D2 + 
     #(0.90D2 * t4 * (-t200 + t64 * t198) + t226) * t55 * t50 / 0.2880D4
     # + (-0.180D3 * t41 * (t78 * t198 - t200) + 0.90D2 * t4 * (t78 * t2
     #00 - t84 * t198 / 0.2D1) - t241) * t50 / 0.5760D4 + ((0.90D2 * t4 
     #* t200 - t226) * t113 + 0.90D2 * t4 * t198 * t121 + (-0.180D3 * t4
     #1 * t200 + t241) * t128) * t48 / 0.11520D5 - (0.90D2 * t4 * (-t143
     # * t198 + (t201 - t137 * t198) * t32 + t200) - t212) * t48 * t55 /
     # 0.5760D4 + (-0.180D3 * t41 * (t155 * t198 - t200) + 0.90D2 * t4 *
     # (t155 * t200 - t161 * t198 / 0.2D1) - t241) * t55 / 0.5760D4 - t1
     #77 * t200 / 0.11520D5 - t190 * t198 / 0.11520D5
      t288 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t287)
      t290 = t2 * x1
      t291 = -0.1D1 + x1
      t292 = x1 * z
      t293 = 0.1D1 - x1 + t292
      t294 = 0.1D1 / t293
      t296 = t2 * t291 * t294
      t297 = t1 ** 2
      t298 = s * t297
      t300 = x1 * t291 * t294
      t301 = t298 * t300
      t302 = -t291
      t303 = bbggh51J2(s, XB1, XB2, z, lh, wd, t302, 0.10D1, 0.10D1, x4)
      t304 = t8 * t11
      t305 = t13 * t294
      t306 = t291 ** 2
      t307 = t305 * t306
      t310 = log(0.4D1 * t304 * t307)
      t311 = bbggh51J1(s, XB1, XB2, z, lh, wd, t302, 0.10D1, 0.10D1, x4)
      t318 = log(-0.4D1 * t304 * t305 * t306 * t16)
      t319 = t318 * z
      t323 = x1 * x3
      t324 = 0.2D1 * t323
      t325 = x3 * t293
      t327 = Sqrt(-t325 * t15)
      t331 = x1 * t12
      t332 = x3 * t12
      t333 = t332 * x1
      t335 = 0.2D1 * t8 * z
      t336 = t8 * t12
      t337 = t323 * z
      t338 = 0.3D1 * t337
      t339 = -t8 - z + t324 + 0.2D1 * t25 * t327 * z - x3 + t292 - t331 
     #+ t333 + t335 - t336 - t338
      t340 = 0.1D1 / t339
      t346 = t293 * t340
      t348 = -t311 - z * t311 * t346
      t359 = t61 * t11
      t362 = log(0.4D1 * t359 * t307)
      t375 = log(0.4D1 * t75 * t307)
      t380 = t375 ** 2
      t391 = -(0.90D2 * t4 * (-t303 + t310 * t311 + (-z * t303 + t319 * 
     #t311) * t293 * t340) - 0.180D3 * t41 * t348) * t48 * t50 / 0.5760D
     #4 - t4 * t348 * t57 / 0.32D2 + (0.90D2 * t4 * (-t362 * t311 + t303
     #) - 0.180D3 * t41 * t311) * t55 * t50 / 0.2880D4 + (-0.180D3 * t41
     # * (-t375 * t311 + t303) + 0.90D2 * t4 * (t380 * t311 / 0.2D1 - t3
     #75 * t303) + t95 * t311) * t50 / 0.5760D4
      t392 = FJET(XB1, XB2, s, 0.0D0, t290, -t296, 0.0D0, -t301, t391)
      t394 = x2 * s
      t395 = t394 * t1
      t396 = -0.1D1 + x2
      t397 = t396 * s
      t398 = t397 * t1
      t399 = -t396
      t400 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t399, 0.10D1, x4)
      t404 = t14 * t396
      t407 = log(-0.4D1 * t61 * t404)
      t409 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t399, 0.10D1, x4)
      t414 = 0.180D3 * t41 * t400
      t421 = log(-0.4D1 * t133 * t404)
      t430 = t13 * t396
      t433 = log(-0.4D1 * t152 * t430)
      t438 = t433 ** 2
      t449 = t4 * t400 * t57 / 0.32D2 + (0.90D2 * t4 * (-t407 * t400 + t
     #409) - t414) * t55 * t50 / 0.2880D4 - (0.90D2 * t4 * (-t409 + t421
     # * t400) + t414) * t48 * t55 / 0.5760D4 + (-0.180D3 * t41 * (-t433
     # * t400 + t409) + 0.90D2 * t4 * (t438 * t400 / 0.2D1 - t433 * t409
     #) + t95 * t400) * t55 / 0.5760D4
      t450 = FJET(XB1, XB2, s, 0.0D0, t395, 0.0D0, -t398, 0.0D0, t449)
      t452 = x2 * x3
      t455 = Sqrt(x3 * t396 * t15)
      t456 = t25 * t455
      t458 = 0.2D1 * t456 * x2
      t460 = 0.1D1 - x3 + t452
      t461 = 0.1D1 / t460
      t463 = t2 * (0.1D1 - x3 - x2 + t452 + t133 + t458) * t461
      t468 = t2 * x2 * (-0.1D1 + t452 + 0.2D1 * t456) * t461
      t469 = x2 * z
      t470 = t469 - x2 - z
      t471 = t4 * t470
      t472 = t15 * t461
      t473 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t399, -t472, x4)
      t475 = t452 * z
      t476 = t133 * z
      t482 = 0.1D1 / (-t469 - t475 + t476 + x2 + z + x3 - t133 - 0.2D1 *
     # t456 * z + 0.2D1 * t456 * t469 - t458)
      t484 = t55 * t50
      t485 = t482 * t48 * t484
      t488 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t399, -t472, x4)
      t491 = t460 ** 2
      t497 = log(0.4D1 * t133 * t11 * t430 * t15 / t491)
      t498 = t497 * t470
      t512 = t471 * t473 * t485 / 0.32D2 - (-0.90D2 * t4 * (t470 * t488 
     #- t498 * t473) * t482 + 0.180D3 * t41 * t470 * t473 * t482) * t48 
     #* t55 / 0.5760D4
      t513 = FJET(XB1, XB2, s, 0.0D0, t463, 0.0D0, -t468, 0.0D0, t512)
      t515 = t1 * t291
      t517 = t397 * t515 * t294
      t518 = t394 * t515
      t520 = t298 * t396 * t300
      t521 = bbggh52J1(s, XB1, XB2, z, lh, wd, t302, t399, 0.10D1, x4)
      t525 = bbggh52J2(s, XB1, XB2, z, lh, wd, t302, t399, 0.10D1, x4)
      t530 = log(-0.4D1 * t359 * t305 * t306 * t396)
      t541 = -t4 * t521 * t57 / 0.32D2 + (0.90D2 * t4 * (-t525 + t530 * 
     #t521) + 0.180D3 * t41 * t521) * t55 * t50 / 0.2880D4
      t542 = FJET(XB1, XB2, s, 0.0D0, t517, t290, -t518, t520, t541)
      t544 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t399, 0.10D1, x4)
      t549 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t399, 0.10D1, x4)
      t554 = 0.180D3 * t41 * t544
      t581 = t4 * t544 * t57 / 0.32D2 + (0.90D2 * t4 * (-t407 * t544 + t
     #549) - t554) * t55 * t50 / 0.2880D4 - (0.90D2 * t4 * (-t549 + t421
     # * t544) + t554) * t48 * t55 / 0.5760D4 + (-0.180D3 * t41 * (t549 
     #- t433 * t544) + 0.90D2 * t4 * (t438 * t544 / 0.2D1 - t433 * t549)
     # + t95 * t544) * t55 / 0.5760D4
      t582 = FJET(XB1, XB2, s, 0.0D0, -t398, 0.0D0, t395, 0.0D0, t581)
      t584 = bbggh52J2(s, XB1, XB2, z, lh, wd, t302, 0.10D1, 0.10D1, x4)
      t585 = bbggh52J1(s, XB1, XB2, z, lh, wd, t302, 0.10D1, 0.10D1, x4)
      t597 = -z * t585 * t346 - t585
      t632 = -(0.90D2 * t4 * (-t584 + t310 * t585 + (-z * t584 + t319 * 
     #t585) * t293 * t340) - 0.180D3 * t41 * t597) * t48 * t50 / 0.5760D
     #4 - t4 * t597 * t57 / 0.32D2 + (0.90D2 * t4 * (-t362 * t585 + t584
     #) - 0.180D3 * t41 * t585) * t55 * t50 / 0.2880D4 + (-0.180D3 * t41
     # * (t584 - t375 * t585) + 0.90D2 * t4 * (t380 * t585 / 0.2D1 - t37
     #5 * t584) + t95 * t585) * t50 / 0.5760D4
      t633 = FJET(XB1, XB2, s, 0.0D0, -t296, t290, 0.0D0, -t301, t632)
      t635 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t399, -t472, x4)
      t637 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t399, -t472, x4)
      t654 = -(-0.90D2 * t4 * (t470 * t635 - t498 * t637) * t482 + 0.180
     #D3 * t41 * t470 * t637 * t482) * t48 * t55 / 0.5760D4 + t471 * t63
     #7 * t485 / 0.32D2
      t655 = FJET(XB1, XB2, s, 0.0D0, -t468, 0.0D0, t463, 0.0D0, t654)
      t657 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t287)
      t659 = FJET(XB1, XB2, s, t290, 0.0D0, 0.0D0, -t296, -t301, t391)
      t661 = t194 * t193 + t196 * t193 + t288 * t287 + t392 * t391 + t45
     #0 * t449 + t513 * t512 + t542 * t541 + t582 * t581 + t633 * t632 +
     # t655 * t654 + t657 * t287 + t659 * t391
      t662 = bbggh51J1(s, XB1, XB2, z, lh, wd, t302, t399, 0.10D1, x4)
      t666 = bbggh51J2(s, XB1, XB2, z, lh, wd, t302, t399, 0.10D1, x4)
      t677 = -t4 * t662 * t57 / 0.32D2 + (0.90D2 * t4 * (-t666 + t530 * 
     #t662) + 0.180D3 * t41 * t662) * t55 * t50 / 0.2880D4
      t678 = FJET(XB1, XB2, s, t290, -t518, 0.0D0, t517, t520, t677)
      t680 = FJET(XB1, XB2, s, t395, 0.0D0, -t398, 0.0D0, 0.0D0, t449)
      t682 = FJET(XB1, XB2, s, t463, 0.0D0, -t468, 0.0D0, 0.0D0, t512)
      t684 = FJET(XB1, XB2, s, t517, 0.0D0, -t518, t290, t520, t541)
      t687 = t290 * t452 * t461
      t688 = t2 * t291
      t691 = Sqrt(t325 * t396 * t15)
      t692 = t25 * t691
      t694 = 0.2D1 * t692 * x2
      t695 = t133 * x1
      t696 = t133 * t292
      t700 = t688 * (t694 + t133 - x2 + t452 + 0.1D1 - x3 - t695 + t696)
     # * t294 * t461
      t704 = t15 * s * t1 * x1 * t461
      t710 = t688 * x2 * (-0.1D1 + t452 + x1 - t323 - t292 + t337 + 0.2D
     #1 * t692) * t294 * t461
      t711 = t4 * t293
      t712 = x2 * x1
      t713 = t712 * z
      t714 = x2 - t712 + z - t469 + t713
      t715 = bbggh52J1(s, XB1, XB2, z, lh, wd, t302, t399, -t472, x4)
      t719 = x2 * t7
      t729 = t292 - x3 + 0.2D1 * t712 - t719 + t694 - t695 - 0.3D1 * t71
     #3 + 0.2D1 * t692 * z + t8 * x2 - t452 * x1 + t712 * t12 + 0.2D1 * 
     #t719 * z - t719 * t12 - z + t475 - t476 + t333
      t743 = t335 - t336 - t338 + t696 - 0.2D1 * t692 * t469 - 0.2D1 * t
     #692 * t712 - t332 * t712 - 0.2D1 * t8 * t469 + t8 * t12 * x2 + 0.2
     #D1 * t452 * t292 - x2 + t133 + t469 + 0.2D1 * t692 * t713 - t8 + t
     #324 - t331
      t745 = 0.1D1 / (t729 + t743)
      t747 = t745 * t48 * t484
      t749 = t711 * t714 * t715 * t747 / 0.32D2
      t750 = FJET(XB1, XB2, s, t687, -t700, -t704, t710, t520, -t749)
      t752 = t293 * t714
      t755 = t715 * t745 * t57
      t758 = bbggh51J1(s, XB1, XB2, z, lh, wd, t302, t399, -t472, x4)
      t762 = t711 * t714 * t758 * t747 / 0.32D2
      t763 = FJET(XB1, XB2, s, t710, -t704, -t700, t687, t520, -t762)
      t767 = t758 * t745 * t57
      t770 = FJET(XB1, XB2, s, -t398, 0.0D0, t395, 0.0D0, 0.0D0, t581)
      t772 = FJET(XB1, XB2, s, -t296, 0.0D0, 0.0D0, t290, -t301, t632)
      t774 = FJET(XB1, XB2, s, -t518, t290, t517, 0.0D0, t520, t677)
      t776 = FJET(XB1, XB2, s, -t468, 0.0D0, t463, 0.0D0, 0.0D0, t654)
      t778 = FJET(XB1, XB2, s, -t704, t710, t687, -t700, t520, -t762)
      t783 = FJET(XB1, XB2, s, -t700, t687, t710, -t704, t520, -t749)
      t788 = t678 * t677 + t680 * t449 + t682 * t512 + t684 * t541 - t75
     #0 * t4 * t752 * t755 / 0.32D2 - t763 * t4 * t752 * t767 / 0.32D2 +
     # t770 * t581 + t772 * t632 + t774 * t677 + t776 * t654 - t778 * t4
     # * t752 * t767 / 0.32D2 - t783 * t4 * t752 * t755 / 0.32D2
      bbggh5n1e0 = t661 + t788

      end function



      doubleprecision function bbggh5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t21 = lh * t4
      t23 = 0.180D3 * t21 * t16
      t25 = 0.1D1 / x1
      t29 = cos(t7)
      t30 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t30)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t32 * z)
      t40 = t4 * (t16 + z * t16 * t37)
      t41 = 0.1D1 / x3
      t42 = t41 * t25
      t45 = t4 * t16
      t46 = 0.1D1 / x2
      t47 = t46 * t25
      t50 = t41 * t46
      t53 = x2 ** 2
      t54 = t53 * t9
      t57 = log(0.4D1 * t54 * t12)
      t68 = log(0.4D1 * t9 * t12)
      t71 = (-0.180D3 * lh - 0.90D2 * t68) * t4
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t80 = t68 ** 2
      t83 = (0.180D3 * t68 * lh + 0.180D3 * t76 - 0.30D2 * t78 + 0.45D2 
     #* t80) * t4
      t86 = x3 * t9
      t89 = log(0.4D1 * t86 * t12)
      t94 = log(-0.4D1 * t86 * t12 / t30)
      t97 = t89 + t94 * z * t37
      t104 = -0.1D1 - z * t37
      t109 = (0.90D2 * t4 * (-t5 + t15 * t16) + t23) * t25 / 0.5760D4 - 
     #t40 * t42 / 0.64D2 - t45 * t47 / 0.32D2 - t40 * t50 / 0.64D2 + (0.
     #90D2 * t4 * (-t5 + t57 * t16) + t23) * t46 / 0.5760D4 - t71 * t5 /
     # 0.11520D5 - t83 * t16 / 0.11520D5 + (0.90D2 * t45 * t97 + (0.90D2
     # * t4 * t5 - t23) * t104) * t41 / 0.11520D5
      t110 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t109)
      t112 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t109)
      t114 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t116 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t121 = 0.180D3 * t21 * t114
      t128 = t4 * (z * t114 * t37 + t114)
      t131 = t4 * t114
      t156 = (0.90D2 * t4 * (t15 * t114 - t116) + t121) * t25 / 0.5760D4
     # - t128 * t42 / 0.64D2 - t131 * t47 / 0.32D2 - t128 * t50 / 0.64D2
     # + (0.90D2 * t4 * (t57 * t114 - t116) + t121) * t46 / 0.5760D4 - t
     #71 * t116 / 0.11520D5 - t83 * t114 / 0.11520D5 + (0.90D2 * t131 * 
     #t97 + (0.90D2 * t4 * t116 - t121) * t104) * t41 / 0.11520D5
      t157 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t156)
      t159 = t2 * x1
      t160 = -0.1D1 + x1
      t161 = x1 * z
      t162 = 0.1D1 - x1 + t161
      t163 = 0.1D1 / t162
      t165 = t2 * t160 * t163
      t166 = t1 ** 2
      t167 = s * t166
      t169 = x1 * t160 * t163
      t170 = t167 * t169
      t172 = t160 ** 2
      t176 = log(0.4D1 * t10 * t12 * t163 * t172)
      t177 = -t160
      t178 = bbggh51J1(s, XB1, XB2, z, lh, wd, t177, 0.10D1, 0.10D1, x4)
      t180 = bbggh51J2(s, XB1, XB2, z, lh, wd, t177, 0.10D1, 0.10D1, x4)
      t190 = t6 * x3
      t191 = x1 * x3
      t195 = Sqrt(-x3 * t162 * t30)
      t207 = -t190 - z + 0.2D1 * t191 + 0.2D1 * t29 * t195 * z - x3 + t1
     #61 - x1 * t11 + x3 * t11 * x1 + 0.2D1 * t190 * z - t190 * t11 - 0.
     #3D1 * t191 * z
      t209 = t162 / t207
      t218 = (0.90D2 * t4 * (-t176 * t178 + t180) - 0.180D3 * t21 * t178
     #) * t25 / 0.5760D4 - t4 * (-t178 - z * t178 * t209) * t42 / 0.64D2
     # + t4 * t178 * t47 / 0.32D2
      t219 = FJET(XB1, XB2, s, 0.0D0, t159, -t165, 0.0D0, -t170, t218)
      t221 = x2 * s
      t222 = t221 * t1
      t223 = -0.1D1 + x2
      t224 = t223 * s
      t225 = t224 * t1
      t226 = -t223
      t227 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t226, 0.10D1, x4)
      t228 = t4 * t227
      t234 = log(-0.4D1 * t54 * t12 * t223)
      t236 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t226, 0.10D1, x4)
      t247 = t228 * t50 / 0.64D2 + (0.90D2 * t4 * (-t234 * t227 + t236) 
     #- 0.180D3 * t21 * t227) * t46 / 0.5760D4 + t228 * t47 / 0.32D2
      t248 = FJET(XB1, XB2, s, 0.0D0, t222, 0.0D0, -t225, 0.0D0, t247)
      t250 = x2 * x3
      t251 = t53 * x3
      t254 = Sqrt(x3 * t223 * t30)
      t255 = t29 * t254
      t257 = 0.2D1 * t255 * x2
      t260 = 0.1D1 / (0.1D1 - x3 + t250)
      t262 = t2 * (0.1D1 - x3 - x2 + t250 + t251 + t257) * t260
      t267 = t2 * x2 * (-0.1D1 + t250 + 0.2D1 * t255) * t260
      t268 = x2 * z
      t269 = t268 - x2 - z
      t270 = t4 * t269
      t271 = t30 * t260
      t272 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t226, -t271, x4)
      t281 = 0.1D1 / (-t268 - t250 * z + t251 * z + x2 + z + x3 - t251 -
     # 0.2D1 * t255 * z + 0.2D1 * t255 * t268 - t257)
      t283 = t281 * t41 * t46
      t285 = t270 * t272 * t283 / 0.64D2
      t286 = FJET(XB1, XB2, s, 0.0D0, t262, 0.0D0, -t267, 0.0D0, t285)
      t290 = t272 * t281 * t50
      t293 = t1 * t160
      t295 = t224 * t293 * t163
      t296 = t221 * t293
      t298 = t167 * t223 * t169
      t299 = bbggh52J1(s, XB1, XB2, z, lh, wd, t177, t226, 0.10D1, x4)
      t302 = t4 * t299 * t47 / 0.32D2
      t303 = FJET(XB1, XB2, s, 0.0D0, t295, t159, -t296, t298, -t302)
      t306 = t299 * t46 * t25
      t309 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t226, 0.10D1, x4)
      t310 = t4 * t309
      t315 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t226, 0.10D1, x4)
      t325 = t310 * t47 / 0.32D2 + t310 * t50 / 0.64D2 + (0.90D2 * t4 * 
     #(t315 - t234 * t309) - 0.180D3 * t21 * t309) * t46 / 0.5760D4
      t326 = FJET(XB1, XB2, s, 0.0D0, -t225, 0.0D0, t222, 0.0D0, t325)
      t328 = bbggh52J2(s, XB1, XB2, z, lh, wd, t177, 0.10D1, 0.10D1, x4)
      t329 = bbggh52J1(s, XB1, XB2, z, lh, wd, t177, 0.10D1, 0.10D1, x4)
      t348 = (0.90D2 * t4 * (t328 - t176 * t329) - 0.180D3 * t21 * t329)
     # * t25 / 0.5760D4 - t4 * (-z * t329 * t209 - t329) * t42 / 0.64D2 
     #+ t4 * t329 * t47 / 0.32D2
      t349 = FJET(XB1, XB2, s, 0.0D0, -t165, t159, 0.0D0, -t170, t348)
      t351 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t226, -t271, x4)
      t354 = t270 * t351 * t283 / 0.64D2
      t355 = FJET(XB1, XB2, s, 0.0D0, -t267, 0.0D0, t262, 0.0D0, t354)
      t359 = t351 * t281 * t50
      t362 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t156)
      t364 = FJET(XB1, XB2, s, t159, 0.0D0, 0.0D0, -t165, -t170, t218)
      t366 = bbggh51J1(s, XB1, XB2, z, lh, wd, t177, t226, 0.10D1, x4)
      t369 = t4 * t366 * t47 / 0.32D2
      t370 = FJET(XB1, XB2, s, t159, -t296, 0.0D0, t295, t298, -t369)
      t373 = t366 * t46 * t25
      t376 = FJET(XB1, XB2, s, t222, 0.0D0, -t225, 0.0D0, 0.0D0, t247)
      t378 = FJET(XB1, XB2, s, t262, 0.0D0, -t267, 0.0D0, 0.0D0, t285)
      t383 = FJET(XB1, XB2, s, t295, 0.0D0, -t296, t159, t298, -t302)
      t387 = FJET(XB1, XB2, s, -t225, 0.0D0, t222, 0.0D0, 0.0D0, t325)
      t389 = FJET(XB1, XB2, s, -t165, 0.0D0, 0.0D0, t159, -t170, t348)
      t391 = FJET(XB1, XB2, s, -t296, t159, t295, 0.0D0, t298, -t369)
      t395 = FJET(XB1, XB2, s, -t267, 0.0D0, t262, 0.0D0, 0.0D0, t354)
      bbggh5n1em1 = t110 * t109 + t112 * t109 + t157 * t156 + t219 * t21
     #8 + t248 * t247 + t286 * t4 * t269 * t290 / 0.64D2 - t303 * t4 * t
     #306 / 0.32D2 + t326 * t325 + t349 * t348 + t355 * t4 * t269 * t359
     # / 0.64D2 + t362 * t156 + t364 * t218 - t370 * t4 * t373 / 0.32D2 
     #+ t376 * t247 + t378 * t4 * t269 * t290 / 0.64D2 - t383 * t4 * t30
     #6 / 0.32D2 + t387 * t325 + t389 * t348 - t391 * t4 * t373 / 0.32D2
     # + t395 * t4 * t269 * t359 / 0.64D2

      end function



      doubleprecision function bbggh5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t17 = z ** 2
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t17 * t21)
      t27 = (-0.180D3 * lh - 0.90D2 * t24) * t4
      t30 = cos(t19)
      t33 = Sqrt(-x3 * (-0.1D1 + x3))
      t42 = (-0.1D1 - z / (-z - x3 + 0.2D1 * t30 * t33 * z)) / x3
      t45 = -t6 * t7 / 0.64D2 - t6 * t10 / 0.64D2 - t4 * t13 / 0.128D3 -
     # t27 * t5 / 0.11520D5 + t6 * t42 / 0.128D3
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t45)
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t45)
      t50 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t51 = t4 * t50
      t56 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t63 = -t51 * t7 / 0.64D2 - t51 * t10 / 0.64D2 - t4 * t56 / 0.128D3
     # - t27 * t50 / 0.11520D5 + t51 * t42 / 0.128D3
      t64 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t63)
      t66 = t2 * x1
      t67 = -0.1D1 + x1
      t70 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t72 = t2 * t67 * t70
      t73 = t1 ** 2
      t77 = s * t73 * x1 * t67 * t70
      t78 = -t67
      t79 = bbggh51J1(s, XB1, XB2, z, lh, wd, t78, 0.10D1, 0.10D1, x4)
      t82 = t4 * t79 * t7 / 0.64D2
      t83 = FJET(XB1, XB2, s, 0.0D0, t66, -t72, 0.0D0, -t77, t82)
      t85 = t79 * t7
      t89 = x2 * s * t1
      t90 = -0.1D1 + x2
      t92 = t90 * s * t1
      t93 = -t90
      t94 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t93, 0.10D1, x4)
      t97 = t4 * t94 * t10 / 0.64D2
      t98 = FJET(XB1, XB2, s, 0.0D0, t89, 0.0D0, -t92, 0.0D0, t97)
      t100 = t94 * t10
      t103 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t93, 0.10D1, x4)
      t106 = t4 * t103 * t10 / 0.64D2
      t107 = FJET(XB1, XB2, s, 0.0D0, -t92, 0.0D0, t89, 0.0D0, t106)
      t109 = t103 * t10
      t112 = bbggh52J1(s, XB1, XB2, z, lh, wd, t78, 0.10D1, 0.10D1, x4)
      t115 = t4 * t112 * t7 / 0.64D2
      t116 = FJET(XB1, XB2, s, 0.0D0, -t72, t66, 0.0D0, -t77, t115)
      t118 = t112 * t7
      t121 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t63)
      t123 = FJET(XB1, XB2, s, t66, 0.0D0, 0.0D0, -t72, -t77, t82)
      t127 = FJET(XB1, XB2, s, t89, 0.0D0, -t92, 0.0D0, 0.0D0, t97)
      t131 = FJET(XB1, XB2, s, -t72, 0.0D0, 0.0D0, t66, -t77, t115)
      t135 = FJET(XB1, XB2, s, -t92, 0.0D0, t89, 0.0D0, 0.0D0, t106)
      bbggh5n1em2 = t46 * t45 + t48 * t45 + t64 * t63 + t83 * t4 * t85 /
     # 0.64D2 + t98 * t4 * t100 / 0.64D2 + t107 * t4 * t109 / 0.64D2 + t
     #116 * t4 * t118 / 0.64D2 + t121 * t63 + t123 * t4 * t85 / 0.64D2 +
     # t127 * t4 * t100 / 0.64D2 + t131 * t4 * t118 / 0.64D2 + t135 * t4
     # * t109 / 0.64D2

      end function



      doubleprecision function bbggh5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t7 = t4 * t5 / 0.128D3
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t14 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t16 = t4 * t14 / 0.128D3
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t16)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t16)
      bbggh5n1em3 = -t8 * t4 * t5 / 0.128D3 - t11 * t4 * t5 / 0.128D3 - 
     #t17 * t4 * t14 / 0.128D3 - t20 * t4 * t14 / 0.128D3

      end function



      doubleprecision function bbggh5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      bbggh5n1em4 = 0.0D0

      end function


      doubleprecision function bbggh5n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = lh * t4
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t15 = log(0.4D1 * t13)
      t16 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t18 = t15 ** 2
      t19 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t25 = lh ** 2
      t27 = 0.3141592653589793D1 ** 2
      t29 = 0.180D3 * t25 - 0.30D2 * t27
      t30 = t29 * t4
      t36 = t18 * t15
      t43 = 0.120D3 * t25 * lh
      t45 = 0.60D2 * lh * t27
      t46 = -0.2884936567583026D3 - t43 + t45
      t47 = t46 * t4
      t48 = t47 * t19
      t50 = 0.1D1 / x1
      t53 = x3 * t6
      t54 = t12 * t9
      t57 = log(0.4D1 * t53 * t54)
      t63 = t57 ** 2
      t69 = t30 * t19
      t71 = 0.1D1 / x3
      t75 = x2 * x3
      t76 = t75 * t13
      t78 = log(0.4D1 * t76)
      t87 = 0.1D1 / x2
      t88 = t87 * t50
      t91 = x2 * t6
      t94 = log(0.4D1 * t91 * t54)
      t100 = t94 ** 2
      t110 = x3 * t9
      t113 = log(0.4D1 * t110 * t12)
      t115 = t113 ** 2
      t126 = t115 * t113
      t135 = x2 * t9
      t138 = log(0.4D1 * t135 * t12)
      t140 = t138 ** 2
      t151 = t140 * t138
      t162 = log(0.4D1 * t75 * t54)
      t168 = t162 ** 2
      t179 = log(0.4D1 * t54)
      t181 = t179 ** 2
      t184 = t181 * t179
      t187 = (-t179 * t29 - 0.90D2 * t181 * lh - 0.2884936567583026D3 - 
     #t43 + t45 - 0.15D2 * t184) * t4
      t190 = t181 ** 2
      t194 = t27 ** 2
      t195 = t25 ** 2
      t204 = (0.15D2 / 0.4D1 * t190 - t179 * t46 + 0.5769873135166051D3 
     #* lh + t194 + 0.60D2 * t195 - 0.60D2 * t25 * t27 + t181 * t29 / 0.
     #2D1 + 0.30D2 * t184 * lh) * t4
      t207 = (-0.180D3 * t5 * (t15 * t16 - t18 * t19 / 0.2D1) + t30 * (t
     #15 * t19 - t16) + 0.90D2 * t4 * (-t18 * t16 / 0.2D1 + t36 * t19 / 
     #0.6D1) - t48) * t50 / 0.2880D4 - (-0.180D3 * t5 * (-t57 * t19 + t1
     #6) + 0.90D2 * t4 * (-t57 * t16 + t63 * t19 / 0.2D1) + t69) * t71 *
     # t50 / 0.2880D4 + (0.90D2 * t4 * (t78 * t19 - t16) + 0.180D3 * t5 
     #* t19) * t71 * t88 / 0.2880D4 + (-0.180D3 * t5 * (t94 * t19 - t16)
     # + 0.90D2 * t4 * (t94 * t16 - t100 * t19 / 0.2D1) - t69) * t87 * t
     #50 / 0.2880D4 + (-0.180D3 * t5 * (t113 * t16 - t115 * t19 / 0.2D1)
     # + t30 * (-t16 + t113 * t19) + 0.90D2 * t4 * (-t115 * t16 / 0.2D1 
     #+ t126 * t19 / 0.6D1) - t48) * t71 / 0.5760D4 + (-0.180D3 * t5 * (
     #t138 * t16 - t140 * t19 / 0.2D1) + t30 * (-t16 + t138 * t19) + 0.9
     #0D2 * t4 * (-t140 * t16 / 0.2D1 + t151 * t19 / 0.6D1) - t48) * t87
     # / 0.5760D4 + (-0.180D3 * t5 * (-t16 + t162 * t19) + 0.90D2 * t4 *
     # (t162 * t16 - t168 * t19 / 0.2D1) - t69) * t71 * t87 / 0.5760D4 -
     # t187 * t16 / 0.5760D4 - t204 * t19 / 0.5760D4
      t208 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = -0.1D1 + x2
      t211 = cos(t7)
      t212 = -0.1D1 + x3
      t214 = Sqrt(-t75 * t212)
      t215 = t211 * t214
      t216 = 0.2D1 * t215
      t219 = t75 - 0.1D1
      t220 = 0.1D1 / t219
      t222 = t2 * t210 * (-t75 - 0.1D1 + x3 + t216) * t220
      t223 = 0.3D1 * t75
      t224 = x2 ** 2
      t225 = t224 * x3
      t227 = 0.2D1 * t215 * x2
      t230 = t2 * (-x2 - x3 + t223 - t225 - t216 + t227) * t220
      t231 = x2 * z
      t232 = 0.1D1 + t231 - x2
      t233 = t212 * t220
      t234 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t233, x4)
      t235 = t232 * t234
      t236 = t75 * t10
      t237 = t210 ** 2
      t238 = t12 * t237
      t239 = t219 ** 2
      t240 = 0.1D1 / t239
      t242 = t238 * t212 * t240
      t245 = log(-0.4D1 * t236 * t242)
      t246 = t245 * t232
      t247 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t233, x4)
      t251 = t225 * z
      t252 = t75 * z
      t253 = 0.2D1 * t75
      t257 = 0.1D1 / (-t251 + t225 - t231 + t252 + x2 - t253 + 0.2D1 * t
     #215 * t231 - t227 - 0.1D1 + t216)
      t261 = t232 * t247 * t257
      t271 = log(-0.4D1 * t75 * t9 * t242)
      t272 = t271 * t232
      t279 = t271 ** 2
      t280 = t279 * t232
      t292 = (0.90D2 * t4 * (t235 - t246 * t247) * t257 - 0.180D3 * t5 *
     # t261) * t71 * t88 / 0.2880D4 + (-0.180D3 * t5 * (t235 - t272 * t2
     #47) * t257 + 0.90D2 * t4 * (-t272 * t234 + t280 * t247 / 0.2D1) * 
     #t257 + t30 * t261) * t71 * t87 / 0.5760D4
      t293 = FJET(XB1, XB2, s, -t222, 0.0D0, t230, 0.0D0, 0.0D0, t292)
      t295 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t207)
      t297 = -0.1D1 + x1
      t298 = t2 * t297
      t299 = x1 * x3
      t300 = x1 * z
      t301 = t299 * z
      t302 = 0.1D1 - x1 + t300
      t306 = Sqrt(-x3 * t302 * x2 * t212)
      t307 = t211 * t306
      t308 = 0.2D1 * t307
      t311 = 0.1D1 / t302
      t314 = t298 * t210 * (-t75 - 0.1D1 + x3 + x1 - t299 - t300 + t301 
     #+ t308) * t311 * t220
      t315 = t212 * s
      t316 = t1 * x1
      t318 = t315 * t316 * t220
      t320 = 0.2D1 * t307 * x2
      t321 = t225 * x1
      t322 = t75 * x1
      t324 = t225 * t300
      t325 = t75 * t300
      t327 = -t225 + t299 + t223 + t320 - t308 - x2 - x3 + t321 - t301 -
     # 0.2D1 * t322 - t324 + 0.2D1 * t325
      t330 = t298 * t327 * t311 * t220
      t331 = t2 * x1
      t334 = t331 * x3 * t210 * t220
      t335 = t1 ** 2
      t340 = s * t335 * x2 * x1 * t297 * t311
      t341 = x2 * x1
      t342 = t341 * z
      t343 = -0.1D1 - t231 - t341 + x1 - t300 + t342 + x2
      t344 = t302 * t343
      t345 = -t297
      t346 = bbggh51J2(s, XB1, XB2, z, lh, wd, t345, x2, t233, x4)
      t348 = t297 ** 2
      t349 = t311 * t348
      t355 = log(-0.4D1 * t76 * t349 * t237 * t212 * t240)
      t356 = t355 * t302
      t357 = bbggh51J1(s, XB1, XB2, z, lh, wd, t345, x2, t233, x4)
      t358 = t343 * t357
      t367 = t53 * x2
      t378 = 0.1D1 - 0.2D1 * x1 + t320 + t321 - 0.3D1 * t322 - 0.3D1 * t
     #342 + 0.2D1 * t307 * x1 + t367 + t341 * t11 + 0.2D1 * t91 * z - t9
     #1 * t11 + 0.2D1 * t300 + 0.2D1 * t307 * t342 - t308 + 0.2D1 * t341
     # - 0.2D1 * t6 * z
      t393 = t6 * t11 - t91 + t253 - t225 + t231 + t251 - t252 + t6 - x2
     # - t324 + 0.4D1 * t325 - 0.2D1 * t307 * t231 - 0.2D1 * t307 * t341
     # - 0.2D1 * t307 * t300 - x3 * t11 * t341 - 0.2D1 * t53 * t231 + t5
     #3 * t11 * x2
      t395 = 0.1D1 / (t378 + t393)
      t398 = t5 * t302
      t402 = -0.90D2 * t4 * (t344 * t346 - t356 * t358) * t395 + 0.180D3
     # * t398 * t358 * t395
      t405 = t402 * t71 * t88 / 0.2880D4
      t406 = FJET(XB1, XB2, s, t314, t318, -t330, t334, -t340, t405)
      t409 = t71 * t87 * t50
      t412 = t2 * x3
      t413 = t2 * t212
      t414 = -t212
      t415 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t414, x4)
      t416 = t54 * t212
      t419 = log(-0.4D1 * t53 * t416)
      t420 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t414, x4)
      t425 = t419 ** 2
      t432 = t30 * t420
      t439 = log(-0.4D1 * t367 * t416)
      t452 = log(-0.4D1 * t75 * t416)
      t458 = t452 ** 2
      t471 = log(-0.4D1 * t110 * t12 * t212)
      t472 = t471 ** 2
      t482 = t472 * t471
      t494 = -(-0.180D3 * t5 * (-t415 + t419 * t420) + 0.90D2 * t4 * (-t
     #425 * t420 / 0.2D1 + t419 * t415) - t432) * t71 * t50 / 0.2880D4 +
     # (0.90D2 * t4 * (t415 - t439 * t420) - 0.180D3 * t5 * t420) * t71 
     #* t88 / 0.2880D4 + (-0.180D3 * t5 * (-t452 * t420 + t415) + 0.90D2
     # * t4 * (-t452 * t415 + t458 * t420 / 0.2D1) + t432) * t71 * t87 /
     # 0.5760D4 + (-0.180D3 * t5 * (t472 * t420 / 0.2D1 - t471 * t415) +
     # t30 * (t415 - t471 * t420) + 0.90D2 * t4 * (-t482 * t420 / 0.6D1 
     #+ t472 * t415 / 0.2D1) + t47 * t420) * t71 / 0.5760D4
      t495 = FJET(XB1, XB2, s, t412, 0.0D0, -t413, 0.0D0, 0.0D0, t494)
      t499 = t2 * t297 * x2 * t311
      t500 = t210 * s
      t501 = t1 * t297
      t502 = t500 * t501
      t503 = bbggh52J2(s, XB1, XB2, z, lh, wd, t345, x2, 0.10D1, x4)
      t504 = t12 * t311
      t506 = t504 * t348 * t237
      t509 = log(0.4D1 * t236 * t506)
      t510 = bbggh52J1(s, XB1, XB2, z, lh, wd, t345, x2, 0.10D1, x4)
      t520 = t91 * t9
      t523 = log(0.4D1 * t520 * t506)
      t528 = t523 ** 2
      t540 = (0.90D2 * t4 * (-t503 + t509 * t510) + 0.180D3 * t5 * t510)
     # * t71 * t88 / 0.2880D4 + (-0.180D3 * t5 * (-t503 + t523 * t510) +
     # 0.90D2 * t4 * (-t528 * t510 / 0.2D1 + t523 * t503) - t30 * t510) 
     #* t87 * t50 / 0.2880D4
      t541 = FJET(XB1, XB2, s, -t499, 0.0D0, t502, t331, -t340, t540)
      t543 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t414, x4)
      t545 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t414, x4)
      t555 = t30 * t543
      t604 = -(-0.180D3 * t5 * (t419 * t543 - t545) + 0.90D2 * t4 * (-t4
     #25 * t543 / 0.2D1 + t419 * t545) - t555) * t71 * t50 / 0.2880D4 + 
     #(0.90D2 * t4 * (-t439 * t543 + t545) - 0.180D3 * t5 * t543) * t71 
     #* t88 / 0.2880D4 + (-0.180D3 * t5 * (t545 - t452 * t543) + 0.90D2 
     #* t4 * (-t452 * t545 + t458 * t543 / 0.2D1) + t555) * t71 * t87 / 
     #0.5760D4 + (-0.180D3 * t5 * (t472 * t543 / 0.2D1 - t471 * t545) + 
     #t30 * (-t471 * t543 + t545) + 0.90D2 * t4 * (-t482 * t543 / 0.6D1 
     #+ t472 * t545 / 0.2D1) + t47 * t543) * t71 / 0.5760D4
      t605 = FJET(XB1, XB2, s, 0.0D0, -t413, 0.0D0, t412, 0.0D0, t604)
      t607 = FJET(XB1, XB2, s, -t413, 0.0D0, t412, 0.0D0, 0.0D0, t604)
      t609 = t504 * t348
      t612 = log(0.4D1 * t10 * t609)
      t613 = bbggh51J2(s, XB1, XB2, z, lh, wd, t345, 0.0D0, 0.10D1, x4)
      t615 = t612 ** 2
      t616 = bbggh51J1(s, XB1, XB2, z, lh, wd, t345, 0.0D0, 0.10D1, x4)
      t627 = t615 * t612
      t636 = t53 * t9
      t639 = log(0.4D1 * t636 * t609)
      t645 = t639 ** 2
      t651 = t30 * t616
      t658 = log(0.4D1 * t367 * t54 * t349)
      t670 = log(0.4D1 * t520 * t609)
      t676 = t670 ** 2
      t686 = (-0.180D3 * t5 * (-t612 * t613 + t615 * t616 / 0.2D1) + t30
     # * (t613 - t612 * t616) + 0.90D2 * t4 * (t615 * t613 / 0.2D1 - t62
     #7 * t616 / 0.6D1) + t47 * t616) * t50 / 0.2880D4 - (-0.180D3 * t5 
     #* (-t613 + t639 * t616) + 0.90D2 * t4 * (t639 * t613 - t645 * t616
     # / 0.2D1) - t651) * t71 * t50 / 0.2880D4 + (0.90D2 * t4 * (t613 - 
     #t658 * t616) - 0.180D3 * t5 * t616) * t71 * t88 / 0.2880D4 + (-0.1
     #80D3 * t5 * (-t670 * t616 + t613) + 0.90D2 * t4 * (-t670 * t613 + 
     #t676 * t616 / 0.2D1) + t651) * t87 * t50 / 0.2880D4
      t687 = FJET(XB1, XB2, s, t331, -t298, 0.0D0, 0.0D0, 0.0D0, t686)
      t689 = t2 * t299
      t691 = t2 * t297 * x3
      t692 = t315 * t316
      t693 = t315 * t501
      t694 = bbggh52J2(s, XB1, XB2, z, lh, wd, t345, 0.0D0, t414, x4)
      t696 = t504 * t348 * t212
      t699 = log(-0.4D1 * t636 * t696)
      t700 = bbggh52J1(s, XB1, XB2, z, lh, wd, t345, 0.0D0, t414, x4)
      t705 = t699 ** 2
      t718 = log(-0.4D1 * t236 * t696)
      t729 = -(-0.180D3 * t5 * (t694 - t699 * t700) + 0.90D2 * t4 * (t70
     #5 * t700 / 0.2D1 - t699 * t694) + t30 * t700) * t71 * t50 / 0.2880
     #D4 + (0.90D2 * t4 * (t718 * t700 - t694) + 0.180D3 * t5 * t700) * 
     #t71 * t88 / 0.2880D4
      t730 = FJET(XB1, XB2, s, t689, -t691, -t692, t693, 0.0D0, t729)
      t732 = t500 * t1
      t734 = x2 * s * t1
      t737 = log(0.4D1 * t135 * t238)
      t738 = t737 ** 2
      t739 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t742 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t750 = t738 * t737
      t762 = t54 * t237
      t765 = log(0.4D1 * t75 * t762)
      t770 = t765 ** 2
      t777 = t30 * t739
      t784 = log(0.4D1 * t367 * t762)
      t797 = log(0.4D1 * t91 * t762)
      t802 = t797 ** 2
      t813 = (-0.180D3 * t5 * (t738 * t739 / 0.2D1 - t737 * t742) + t30 
     #* (-t737 * t739 + t742) + 0.90D2 * t4 * (-t750 * t739 / 0.6D1 + t7
     #38 * t742 / 0.2D1) + t47 * t739) * t87 / 0.5760D4 + (-0.180D3 * t5
     # * (t742 - t765 * t739) + 0.90D2 * t4 * (t770 * t739 / 0.2D1 - t76
     #5 * t742) + t777) * t71 * t87 / 0.5760D4 + (0.90D2 * t4 * (-t784 *
     # t739 + t742) - 0.180D3 * t5 * t739) * t71 * t88 / 0.2880D4 + (-0.
     #180D3 * t5 * (-t797 * t739 + t742) + 0.90D2 * t4 * (t802 * t739 / 
     #0.2D1 - t797 * t742) + t777) * t87 * t50 / 0.2880D4
      t814 = FJET(XB1, XB2, s, 0.0D0, -t732, 0.0D0, t734, 0.0D0, t813)
      t816 = FJET(XB1, XB2, s, -t732, 0.0D0, t734, 0.0D0, 0.0D0, t813)
      t818 = bbggh52J2(s, XB1, XB2, z, lh, wd, t345, 0.0D0, 0.10D1, x4)
      t820 = bbggh52J1(s, XB1, XB2, z, lh, wd, t345, 0.0D0, 0.10D1, x4)
      t849 = t30 * t820
      t876 = (-0.180D3 * t5 * (-t612 * t818 + t615 * t820 / 0.2D1) + t30
     # * (-t612 * t820 + t818) + 0.90D2 * t4 * (t615 * t818 / 0.2D1 - t6
     #27 * t820 / 0.6D1) + t47 * t820) * t50 / 0.2880D4 - (-0.180D3 * t5
     # * (-t818 + t639 * t820) + 0.90D2 * t4 * (-t645 * t820 / 0.2D1 + t
     #639 * t818) - t849) * t71 * t50 / 0.2880D4 + (0.90D2 * t4 * (t818 
     #- t658 * t820) - 0.180D3 * t5 * t820) * t71 * t88 / 0.2880D4 + (-0
     #.180D3 * t5 * (t818 - t670 * t820) + 0.90D2 * t4 * (t676 * t820 / 
     #0.2D1 - t670 * t818) + t849) * t87 * t50 / 0.2880D4
      t877 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t331, -t298, 0.0D0, t876)
      t879 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t233, x4)
      t880 = t232 * t879
      t881 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t233, x4)
      t888 = t232 * t881 * t257
      t912 = (0.90D2 * t4 * (t880 - t246 * t881) * t257 - 0.180D3 * t5 *
     # t888) * t71 * t88 / 0.2880D4 + (-0.180D3 * t5 * (t880 - t272 * t8
     #81) * t257 + 0.90D2 * t4 * (-t272 * t879 + t280 * t881 / 0.2D1) * 
     #t257 + t30 * t888) * t71 * t87 / 0.5760D4
      t913 = FJET(XB1, XB2, s, t230, 0.0D0, -t222, 0.0D0, 0.0D0, t912)
      t915 = bbggh51J1(s, XB1, XB2, z, lh, wd, t345, 0.0D0, t414, x4)
      t917 = bbggh51J2(s, XB1, XB2, z, lh, wd, t345, 0.0D0, t414, x4)
      t941 = -(-0.180D3 * t5 * (-t699 * t915 + t917) + 0.90D2 * t4 * (t7
     #05 * t915 / 0.2D1 - t699 * t917) + t30 * t915) * t71 * t50 / 0.288
     #0D4 + (0.90D2 * t4 * (-t917 + t718 * t915) + 0.180D3 * t5 * t915) 
     #* t71 * t88 / 0.2880D4
      t942 = FJET(XB1, XB2, s, -t692, t693, t689, -t691, 0.0D0, t941)
      t944 = FJET(XB1, XB2, s, 0.0D0, t412, 0.0D0, -t413, 0.0D0, t494)
      t946 = t208 * t207 + t293 * t292 + t295 * t207 + t406 * t402 * t40
     #9 / 0.2880D4 + t495 * t494 + t541 * t540 + t605 * t604 + t607 * t6
     #04 + t687 * t686 + t730 * t729 + t814 * t813 + t816 * t813 + t877 
     #* t876 + t913 * t912 + t942 * t941 + t944 * t494
      t947 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t948 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t969 = t30 * t948
      t1008 = (0.90D2 * t4 * (t947 - t784 * t948) - 0.180D3 * t5 * t948)
     # * t71 * t88 / 0.2880D4 + (-0.180D3 * t5 * (t947 - t797 * t948) + 
     #0.90D2 * t4 * (t802 * t948 / 0.2D1 - t797 * t947) + t969) * t87 * 
     #t50 / 0.2880D4 + (-0.180D3 * t5 * (t738 * t948 / 0.2D1 - t737 * t9
     #47) + t30 * (t947 - t737 * t948) + 0.90D2 * t4 * (-t750 * t948 / 0
     #.6D1 + t738 * t947 / 0.2D1) + t47 * t948) * t87 / 0.5760D4 + (-0.1
     #80D3 * t5 * (t947 - t765 * t948) + 0.90D2 * t4 * (-t765 * t947 + t
     #770 * t948 / 0.2D1) + t969) * t71 * t87 / 0.5760D4
      t1009 = FJET(XB1, XB2, s, t734, 0.0D0, -t732, 0.0D0, 0.0D0, t1008)
      t1011 = FJET(XB1, XB2, s, 0.0D0, t734, 0.0D0, -t732, 0.0D0, t1008)
      t1013 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1016 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1031 = t47 * t1013
      t1045 = t30 * t1013
      t1130 = (-0.180D3 * t5 * (-t18 * t1013 / 0.2D1 + t15 * t1016) + t3
     #0 * (-t1016 + t15 * t1013) + 0.90D2 * t4 * (t36 * t1013 / 0.6D1 - 
     #t18 * t1016 / 0.2D1) - t1031) * t50 / 0.2880D4 - (-0.180D3 * t5 * 
     #(-t57 * t1013 + t1016) + 0.90D2 * t4 * (-t57 * t1016 + t63 * t1013
     # / 0.2D1) + t1045) * t71 * t50 / 0.2880D4 + (0.90D2 * t4 * (-t1016
     # + t78 * t1013) + 0.180D3 * t5 * t1013) * t71 * t88 / 0.2880D4 + (
     #-0.180D3 * t5 * (t94 * t1013 - t1016) + 0.90D2 * t4 * (t94 * t1016
     # - t100 * t1013 / 0.2D1) - t1045) * t87 * t50 / 0.2880D4 - t187 * 
     #t1016 / 0.5760D4 - t204 * t1013 / 0.5760D4 + (-0.180D3 * t5 * (t11
     #3 * t1016 - t115 * t1013 / 0.2D1) + t30 * (t113 * t1013 - t1016) +
     # 0.90D2 * t4 * (-t115 * t1016 / 0.2D1 + t126 * t1013 / 0.6D1) - t1
     #031) * t71 / 0.5760D4 + (-0.180D3 * t5 * (t138 * t1016 - t140 * t1
     #013 / 0.2D1) + t30 * (t138 * t1013 - t1016) + 0.90D2 * t4 * (-t140
     # * t1016 / 0.2D1 + t151 * t1013 / 0.6D1) - t1031) * t87 / 0.5760D4
     # + (-0.180D3 * t5 * (t162 * t1013 - t1016) + 0.90D2 * t4 * (t162 *
     # t1016 - t168 * t1013 / 0.2D1) - t1045) * t71 * t87 / 0.5760D4
      t1131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1130)
      t1133 = FJET(XB1, XB2, s, 0.0D0, -t222, 0.0D0, t230, 0.0D0, t292)
      t1135 = FJET(XB1, XB2, s, 0.0D0, -t499, t331, t502, -t340, t540)
      t1137 = FJET(XB1, XB2, s, 0.0D0, t230, 0.0D0, -t222, 0.0D0, t912)
      t1139 = FJET(XB1, XB2, s, t318, t314, t334, -t330, -t340, t405)
      t1143 = bbggh52J2(s, XB1, XB2, z, lh, wd, t345, x2, t233, x4)
      t1145 = bbggh52J1(s, XB1, XB2, z, lh, wd, t345, x2, t233, x4)
      t1146 = t343 * t1145
      t1155 = -0.90D2 * t4 * (t344 * t1143 - t356 * t1146) * t395 + 0.18
     #0D3 * t398 * t1146 * t395
      t1158 = t1155 * t71 * t88 / 0.2880D4
      t1159 = FJET(XB1, XB2, s, t334, -t330, t318, t314, -t340, t1158)
      t1163 = FJET(XB1, XB2, s, -t330, t334, t314, t318, -t340, t1158)
      t1167 = FJET(XB1, XB2, s, -t298, t331, 0.0D0, 0.0D0, 0.0D0, t686)
      t1169 = FJET(XB1, XB2, s, t693, -t692, -t691, t689, 0.0D0, t941)
      t1171 = bbggh51J1(s, XB1, XB2, z, lh, wd, t345, x2, 0.10D1, x4)
      t1173 = bbggh51J2(s, XB1, XB2, z, lh, wd, t345, x2, 0.10D1, x4)
      t1197 = (0.90D2 * t4 * (t509 * t1171 - t1173) + 0.180D3 * t5 * t11
     #71) * t71 * t88 / 0.2880D4 + (-0.180D3 * t5 * (-t1173 + t523 * t11
     #71) + 0.90D2 * t4 * (t523 * t1173 - t528 * t1171 / 0.2D1) - t30 * 
     #t1171) * t87 * t50 / 0.2880D4
      t1198 = FJET(XB1, XB2, s, t502, t331, -t499, 0.0D0, -t340, t1197)
      t1200 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1130)
      t1202 = FJET(XB1, XB2, s, t331, t502, 0.0D0, -t499, -t340, t1197)
      t1204 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t298, t331, 0.0D0, t876)
      t1206 = FJET(XB1, XB2, s, -t691, t689, t693, -t692, 0.0D0, t729)
      t1208 = t1009 * t1008 + t1011 * t1008 + t1131 * t1130 + t1133 * t2
     #92 + t1135 * t540 + t1137 * t912 + t1139 * t402 * t409 / 0.2880D4 
     #+ t1159 * t1155 * t409 / 0.2880D4 + t1163 * t1155 * t409 / 0.2880D
     #4 + t1167 * t686 + t1169 * t941 + t1198 * t1197 + t1200 * t1130 + 
     #t1202 * t1197 + t1204 * t876 + t1206 * t729
      bbggh5n2e1 = t946 + t1208

      end function



      doubleprecision function bbggh5n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = x3 * t5
      t7 = z ** 2
      t8 = 0.1D1 / t7
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t15 = log(0.4D1 * t6 * t12)
      t16 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t18 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t22 = lh * t4
      t24 = 0.180D3 * t22 * t16
      t26 = 0.1D1 / x3
      t28 = 0.1D1 / x1
      t32 = 0.1D1 / x2
      t34 = t26 * t32 * t28
      t37 = x2 * t5
      t40 = log(0.4D1 * t37 * t12)
      t49 = t5 * t11
      t52 = log(0.4D1 * t49 * t8)
      t57 = t52 ** 2
      t64 = lh ** 2
      t65 = 0.180D3 * t64
      t66 = 0.3141592653589793D1 ** 2
      t67 = 0.30D2 * t66
      t68 = t65 - t67
      t69 = t68 * t4
      t70 = t69 * t16
      t74 = x3 * t11
      t77 = log(0.4D1 * t74 * t8)
      t83 = t77 ** 2
      t92 = x2 * x3
      t95 = log(0.4D1 * t92 * t12)
      t104 = x2 * t11
      t107 = log(0.4D1 * t104 * t8)
      t113 = t107 ** 2
      t123 = log(0.4D1 * t12)
      t126 = t123 ** 2
      t129 = (0.180D3 * t123 * lh + t65 - t67 + 0.45D2 * t126) * t4
      t142 = (-t123 * t68 - 0.90D2 * t126 * lh - 0.2884936567583026D3 - 
     #0.120D3 * t64 * lh + 0.60D2 * lh * t66 - 0.15D2 * t126 * t123) * t
     #4
      t145 = -(0.90D2 * t4 * (-t15 * t16 + t18) - t24) * t26 * t28 / 0.2
     #880D4 - t4 * t16 * t34 / 0.32D2 + (0.90D2 * t4 * (t40 * t16 - t18)
     # + t24) * t32 * t28 / 0.2880D4 + (-0.180D3 * t22 * (-t18 + t52 * t
     #16) + 0.90D2 * t4 * (-t57 * t16 / 0.2D1 + t52 * t18) - t70) * t28 
     #/ 0.2880D4 + (-0.180D3 * t22 * (t77 * t16 - t18) + 0.90D2 * t4 * (
     #t77 * t18 - t83 * t16 / 0.2D1) - t70) * t26 / 0.5760D4 + (0.90D2 *
     # t4 * (t95 * t16 - t18) + t24) * t26 * t32 / 0.5760D4 + (-0.180D3 
     #* t22 * (t107 * t16 - t18) + 0.90D2 * t4 * (t107 * t18 - t113 * t1
     #6 / 0.2D1) - t70) * t32 / 0.5760D4 - t129 * t18 / 0.5760D4 - t142 
     #* t16 / 0.5760D4
      t146 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t145)
      t148 = -0.1D1 + x1
      t150 = t2 * t148 * x3
      t151 = x1 * x3
      t152 = t2 * t151
      t153 = -0.1D1 + x3
      t154 = t153 * s
      t155 = t1 * t148
      t156 = t154 * t155
      t157 = t1 * x1
      t158 = t154 * t157
      t159 = -t148
      t160 = -t153
      t161 = bbggh52J2(s, XB1, XB2, z, lh, wd, t159, 0.0D0, t160, x4)
      t162 = t6 * t11
      t163 = x1 * z
      t164 = 0.1D1 - x1 + t163
      t165 = 0.1D1 / t164
      t166 = t8 * t165
      t167 = t148 ** 2
      t172 = log(-0.4D1 * t162 * t166 * t167 * t153)
      t173 = bbggh52J1(s, XB1, XB2, z, lh, wd, t159, 0.0D0, t160, x4)
      t187 = -(0.90D2 * t4 * (t161 - t172 * t173) - 0.180D3 * t22 * t173
     #) * t26 * t28 / 0.2880D4 - t4 * t173 * t34 / 0.32D2
      t188 = FJET(XB1, XB2, s, -t150, t152, t156, -t158, 0.0D0, t187)
      t190 = -0.1D1 + x2
      t191 = t190 * s
      t192 = t191 * t155
      t193 = t2 * x1
      t196 = t2 * t148 * x2 * t165
      t197 = t1 ** 2
      t202 = s * t197 * x2 * x1 * t148 * t165
      t203 = bbggh51J1(s, XB1, XB2, z, lh, wd, t159, x2, 0.10D1, x4)
      t207 = bbggh51J2(s, XB1, XB2, z, lh, wd, t159, x2, 0.10D1, x4)
      t208 = t37 * t11
      t209 = t190 ** 2
      t214 = log(0.4D1 * t208 * t166 * t167 * t209)
      t225 = -t4 * t203 * t34 / 0.32D2 + (0.90D2 * t4 * (-t207 + t214 * 
     #t203) + 0.180D3 * t22 * t203) * t32 * t28 / 0.2880D4
      t226 = FJET(XB1, XB2, s, t192, t193, -t196, 0.0D0, -t202, t225)
      t228 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t230 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t235 = 0.180D3 * t22 * t228
      t261 = t69 * t228
      t303 = -(0.90D2 * t4 * (-t15 * t228 + t230) - t235) * t26 * t28 / 
     #0.2880D4 - t4 * t228 * t34 / 0.32D2 + (0.90D2 * t4 * (t40 * t228 -
     # t230) + t235) * t32 * t28 / 0.2880D4 + (-0.180D3 * t22 * (t52 * t
     #228 - t230) + 0.90D2 * t4 * (t52 * t230 - t57 * t228 / 0.2D1) - t2
     #61) * t28 / 0.2880D4 - t129 * t230 / 0.5760D4 - t142 * t228 / 0.57
     #60D4 + (-0.180D3 * t22 * (-t230 + t77 * t228) + 0.90D2 * t4 * (t77
     # * t230 - t83 * t228 / 0.2D1) - t261) * t26 / 0.5760D4 + (0.90D2 *
     # t4 * (-t230 + t95 * t228) + t235) * t26 * t32 / 0.5760D4 + (-0.18
     #0D3 * t22 * (-t230 + t107 * t228) + 0.90D2 * t4 * (t107 * t230 - t
     #113 * t228 / 0.2D1) - t261) * t32 / 0.5760D4
      t304 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t303)
      t306 = bbggh51J1(s, XB1, XB2, z, lh, wd, t159, 0.0D0, t160, x4)
      t308 = bbggh51J2(s, XB1, XB2, z, lh, wd, t159, 0.0D0, t160, x4)
      t321 = -(0.90D2 * t4 * (-t172 * t306 + t308) - 0.180D3 * t22 * t30
     #6) * t26 * t28 / 0.2880D4 - t4 * t306 * t34 / 0.32D2
      t322 = FJET(XB1, XB2, s, t156, -t158, -t150, t152, 0.0D0, t321)
      t324 = t2 * t153
      t325 = t2 * x3
      t326 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t160, x4)
      t327 = t12 * t153
      t330 = log(-0.4D1 * t92 * t327)
      t331 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t160, x4)
      t337 = 0.180D3 * t22 * t331
      t344 = log(-0.4D1 * t6 * t327)
      t359 = log(-0.4D1 * t74 * t8 * t153)
      t364 = t359 ** 2
      t375 = (0.90D2 * t4 * (t326 - t330 * t331) - t337) * t26 * t32 / 0
     #.5760D4 - (0.90D2 * t4 * (t344 * t331 - t326) + t337) * t26 * t28 
     #/ 0.2880D4 + t4 * t331 * t34 / 0.32D2 + (-0.180D3 * t22 * (-t359 *
     # t331 + t326) + 0.90D2 * t4 * (t364 * t331 / 0.2D1 - t359 * t326) 
     #+ t69 * t331) * t26 / 0.5760D4
      t376 = FJET(XB1, XB2, s, -t324, 0.0D0, t325, 0.0D0, 0.0D0, t375)
      t378 = t2 * t148
      t379 = bbggh51J2(s, XB1, XB2, z, lh, wd, t159, 0.0D0, 0.10D1, x4)
      t380 = t166 * t167
      t383 = log(0.4D1 * t162 * t380)
      t384 = bbggh51J1(s, XB1, XB2, z, lh, wd, t159, 0.0D0, 0.10D1, x4)
      t390 = 0.180D3 * t22 * t384
      t400 = log(0.4D1 * t208 * t380)
      t411 = log(0.4D1 * t49 * t380)
      t417 = t411 ** 2
      t427 = -(0.90D2 * t4 * (-t379 + t383 * t384) + t390) * t26 * t28 /
     # 0.2880D4 + t4 * t384 * t34 / 0.32D2 + (0.90D2 * t4 * (-t400 * t38
     #4 + t379) - t390) * t32 * t28 / 0.2880D4 + (-0.180D3 * t22 * (t379
     # - t411 * t384) + 0.90D2 * t4 * (-t411 * t379 + t417 * t384 / 0.2D
     #1) + t69 * t384) * t28 / 0.2880D4
      t428 = FJET(XB1, XB2, s, -t378, t193, 0.0D0, 0.0D0, 0.0D0, t427)
      t430 = FJET(XB1, XB2, s, -t158, t156, t152, -t150, 0.0D0, t321)
      t432 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t160, x4)
      t433 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t160, x4)
      t439 = 0.180D3 * t22 * t433
      t469 = -(0.90D2 * t4 * (-t432 + t344 * t433) + t439) * t26 * t28 /
     # 0.2880D4 + t4 * t433 * t34 / 0.32D2 + (-0.180D3 * t22 * (t432 - t
     #359 * t433) + 0.90D2 * t4 * (t364 * t433 / 0.2D1 - t359 * t432) + 
     #t69 * t433) * t26 / 0.5760D4 + (0.90D2 * t4 * (-t330 * t433 + t432
     #) - t439) * t26 * t32 / 0.5760D4
      t470 = FJET(XB1, XB2, s, 0.0D0, t325, 0.0D0, -t324, 0.0D0, t469)
      t473 = x2 * s * t1
      t474 = t191 * t1
      t475 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t479 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t480 = t12 * t209
      t483 = log(0.4D1 * t37 * t480)
      t489 = 0.180D3 * t22 * t475
      t496 = log(0.4D1 * t92 * t480)
      t505 = t8 * t209
      t508 = log(0.4D1 * t104 * t505)
      t513 = t508 ** 2
      t524 = t4 * t475 * t34 / 0.32D2 + (0.90D2 * t4 * (t479 - t483 * t4
     #75) - t489) * t32 * t28 / 0.2880D4 + (0.90D2 * t4 * (t479 - t496 *
     # t475) - t489) * t26 * t32 / 0.5760D4 + (-0.180D3 * t22 * (t479 - 
     #t508 * t475) + 0.90D2 * t4 * (t513 * t475 / 0.2D1 - t508 * t479) +
     # t69 * t475) * t32 / 0.5760D4
      t525 = FJET(XB1, XB2, s, t473, 0.0D0, -t474, 0.0D0, 0.0D0, t524)
      t527 = bbggh52J2(s, XB1, XB2, z, lh, wd, t159, 0.0D0, 0.10D1, x4)
      t528 = bbggh52J1(s, XB1, XB2, z, lh, wd, t159, 0.0D0, 0.10D1, x4)
      t534 = 0.180D3 * t22 * t528
      t564 = -(0.90D2 * t4 * (-t527 + t383 * t528) + t534) * t26 * t28 /
     # 0.2880D4 + t4 * t528 * t34 / 0.32D2 + (0.90D2 * t4 * (t527 - t400
     # * t528) - t534) * t32 * t28 / 0.2880D4 + (-0.180D3 * t22 * (-t411
     # * t528 + t527) + 0.90D2 * t4 * (-t411 * t527 + t417 * t528 / 0.2D
     #1) + t69 * t528) * t28 / 0.2880D4
      t565 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t378, t193, 0.0D0, t564)
      t567 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t303)
      t569 = t92 - 0.1D1
      t570 = 0.1D1 / t569
      t572 = t154 * t157 * t570
      t573 = t151 * z
      t574 = cos(t9)
      t578 = Sqrt(-x3 * t164 * x2 * t153)
      t579 = t574 * t578
      t580 = 0.2D1 * t579
      t585 = t378 * t190 * (-t92 - 0.1D1 + x3 + x1 - t151 - t163 + t573 
     #+ t580) * t165 * t570
      t588 = t193 * x3 * t190 * t570
      t589 = x2 ** 2
      t590 = t589 * x3
      t591 = 0.3D1 * t92
      t593 = 0.2D1 * t579 * x2
      t594 = t590 * x1
      t595 = t92 * x1
      t597 = t590 * t163
      t598 = t92 * t163
      t600 = -t590 + t151 + t591 + t593 - t580 - x2 - x3 + t594 - t573 -
     # 0.2D1 * t595 - t597 + 0.2D1 * t598
      t603 = t378 * t600 * t165 * t570
      t604 = t4 * t164
      t605 = x2 * z
      t606 = x2 * x1
      t607 = t606 * z
      t608 = -0.1D1 - t605 - t606 + x1 - t163 + t607 + x2
      t609 = t153 * t570
      t610 = bbggh51J1(s, XB1, XB2, z, lh, wd, t159, x2, t609, x4)
      t613 = t590 * z
      t614 = t92 * z
      t632 = 0.1D1 + t613 - t614 + t593 + t594 - 0.3D1 * t595 - 0.3D1 * 
     #t607 + 0.2D1 * t579 * x1 + 0.2D1 * t579 * t607 - 0.2D1 * x1 - 0.2D
     #1 * t579 * t606 - 0.2D1 * t579 * t163 - x3 * t7 * t606 - 0.2D1 * t
     #6 * t605 + t6 * t7 * x2 - t597
      t641 = 0.2D1 * t92
      t647 = 0.4D1 * t598 - 0.2D1 * t579 * t605 - x2 + 0.2D1 * t606 - 0.
     #2D1 * t5 * z + t5 * t7 - t37 - t580 + 0.2D1 * t163 + t605 + t641 -
     # t590 + x2 * t6 + t606 * t7 + 0.2D1 * t37 * z - t37 * t7 + t5
      t649 = 0.1D1 / (t632 + t647)
      t651 = t32 * t28
      t652 = t649 * t26 * t651
      t654 = t604 * t608 * t610 * t652 / 0.32D2
      t655 = FJET(XB1, XB2, s, t572, t585, t588, -t603, -t202, -t654)
      t657 = t164 * t608
      t660 = t610 * t649 * t34
      t663 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t668 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t673 = 0.180D3 * t22 * t663
      t700 = t4 * t663 * t34 / 0.32D2 + (0.90D2 * t4 * (-t483 * t663 + t
     #668) - t673) * t32 * t28 / 0.2880D4 + (0.90D2 * t4 * (t668 - t496 
     #* t663) - t673) * t26 * t32 / 0.5760D4 + (-0.180D3 * t22 * (-t508 
     #* t663 + t668) + 0.90D2 * t4 * (t513 * t663 / 0.2D1 - t508 * t668)
     # + t69 * t663) * t32 / 0.5760D4
      t701 = FJET(XB1, XB2, s, -t474, 0.0D0, t473, 0.0D0, 0.0D0, t700)
      t703 = FJET(XB1, XB2, s, 0.0D0, -t474, 0.0D0, t473, 0.0D0, t700)
      t705 = FJET(XB1, XB2, s, 0.0D0, t473, 0.0D0, -t474, 0.0D0, t524)
      t707 = t146 * t145 + t188 * t187 + t226 * t225 + t304 * t303 + t32
     #2 * t321 + t376 * t375 + t428 * t427 + t430 * t321 + t470 * t469 +
     # t525 * t524 + t565 * t564 + t567 * t303 - t655 * t4 * t657 * t660
     # / 0.32D2 + t701 * t700 + t703 * t700 + t705 * t524
      t708 = FJET(XB1, XB2, s, t193, -t378, 0.0D0, 0.0D0, 0.0D0, t427)
      t711 = Sqrt(-t92 * t153)
      t712 = t574 * t711
      t713 = 0.2D1 * t712
      t717 = t2 * t190 * (-t92 - 0.1D1 + x3 + t713) * t570
      t719 = 0.2D1 * t712 * x2
      t722 = t2 * (-x2 - x3 + t591 - t590 - t713 + t719) * t570
      t723 = 0.1D1 + t605 - x2
      t724 = t4 * t723
      t725 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t609, x4)
      t730 = 0.1D1 / (-t613 + t590 - t605 + t614 + x2 - t641 + 0.2D1 * t
     #712 * t605 - t719 - 0.1D1 + t713)
      t732 = t730 * t26 * t651
      t735 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t609, x4)
      t738 = t569 ** 2
      t744 = log(-0.4D1 * t92 * t11 * t505 * t153 / t738)
      t745 = t744 * t723
      t759 = t724 * t725 * t732 / 0.32D2 + (0.90D2 * t4 * (t723 * t735 -
     # t745 * t725) * t730 - 0.180D3 * t22 * t723 * t725 * t730) * t26 *
     # t32 / 0.5760D4
      t760 = FJET(XB1, XB2, s, 0.0D0, -t717, 0.0D0, t722, 0.0D0, t759)
      t762 = FJET(XB1, XB2, s, -t717, 0.0D0, t722, 0.0D0, 0.0D0, t759)
      t764 = bbggh52J1(s, XB1, XB2, z, lh, wd, t159, x2, 0.10D1, x4)
      t768 = bbggh52J2(s, XB1, XB2, z, lh, wd, t159, x2, 0.10D1, x4)
      t779 = -t4 * t764 * t34 / 0.32D2 + (0.90D2 * t4 * (-t768 + t214 * 
     #t764) + 0.180D3 * t22 * t764) * t32 * t28 / 0.2880D4
      t780 = FJET(XB1, XB2, s, -t196, 0.0D0, t192, t193, -t202, t779)
      t782 = FJET(XB1, XB2, s, 0.0D0, -t324, 0.0D0, t325, 0.0D0, t375)
      t784 = FJET(XB1, XB2, s, t152, -t150, -t158, t156, 0.0D0, t187)
      t786 = FJET(XB1, XB2, s, t193, t192, 0.0D0, -t196, -t202, t225)
      t788 = FJET(XB1, XB2, s, t325, 0.0D0, -t324, 0.0D0, 0.0D0, t469)
      t790 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t609, x4)
      t792 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t609, x4)
      t809 = (0.90D2 * t4 * (t723 * t790 - t745 * t792) * t730 - 0.180D3
     # * t22 * t723 * t792 * t730) * t26 * t32 / 0.5760D4 + t724 * t792 
     #* t732 / 0.32D2
      t810 = FJET(XB1, XB2, s, t722, 0.0D0, -t717, 0.0D0, 0.0D0, t809)
      t812 = bbggh52J1(s, XB1, XB2, z, lh, wd, t159, x2, t609, x4)
      t816 = t604 * t608 * t812 * t652 / 0.32D2
      t817 = FJET(XB1, XB2, s, t588, -t603, t572, t585, -t202, -t816)
      t821 = t812 * t649 * t34
      t824 = FJET(XB1, XB2, s, 0.0D0, t722, 0.0D0, -t717, 0.0D0, t809)
      t826 = FJET(XB1, XB2, s, 0.0D0, -t196, t193, t192, -t202, t779)
      t828 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t193, -t378, 0.0D0, t564)
      t830 = FJET(XB1, XB2, s, -t603, t588, t585, t572, -t202, -t816)
      t835 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t145)
      t837 = FJET(XB1, XB2, s, t585, t572, -t603, t588, -t202, -t654)
      t842 = t708 * t427 + t760 * t759 + t762 * t759 + t780 * t779 + t78
     #2 * t375 + t784 * t187 + t786 * t225 + t788 * t469 + t810 * t809 -
     # t817 * t4 * t657 * t821 / 0.32D2 + t824 * t809 + t826 * t779 + t8
     #28 * t564 - t830 * t4 * t657 * t821 / 0.32D2 + t835 * t145 - t837 
     #* t4 * t657 * t660 / 0.32D2
      bbggh5n2e0 = t707 + t842

      end function



      doubleprecision function bbggh5n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t2 = -0.1D1 + z
      t3 = x2 * s * t2
      t4 = -0.1D1 + x2
      t5 = t4 * s
      t6 = t5 * t2
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t10 = t8 * t9
      t11 = 0.1D1 / x3
      t12 = 0.1D1 / x2
      t13 = t11 * t12
      t16 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t17 = x4 * 0.3141592653589793D1
      t18 = Sin(t17)
      t19 = t18 ** 2
      t20 = x2 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t23 = t4 ** 2
      t27 = log(0.4D1 * t20 * t22 * t23)
      t32 = lh * t8
      t38 = 0.1D1 / x1
      t39 = t12 * t38
      t42 = t10 * t13 / 0.64D2 + (0.90D2 * t8 * (t16 - t27 * t9) - 0.180
     #D3 * t32 * t9) * t12 / 0.5760D4 + t10 * t39 / 0.32D2
      t43 = FJET(XB1, XB2, s, t3, 0.0D0, -t6, 0.0D0, 0.0D0, t42)
      t45 = s * t2
      t46 = x1 ** 2
      t47 = t46 * t19
      t50 = log(0.4D1 * t47 * t22)
      t51 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t53 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t58 = 0.180D3 * t32 * t51
      t62 = t8 * t51
      t63 = t11 * t38
      t70 = log(0.4D1 * t22 * t19)
      t73 = lh ** 2
      t75 = 0.3141592653589793D1 ** 2
      t77 = t70 ** 2
      t80 = (0.180D3 * t70 * lh + 0.180D3 * t73 - 0.30D2 * t75 + 0.45D2 
     #* t77) * t8
      t83 = x3 * t19
      t86 = log(0.4D1 * t83 * t22)
      t98 = log(0.4D1 * t20 * t22)
      t109 = (-0.180D3 * lh - 0.90D2 * t70) * t8
      t112 = (0.90D2 * t8 * (t50 * t51 - t53) + t58) * t38 / 0.2880D4 - 
     #t62 * t63 / 0.32D2 - t62 * t39 / 0.32D2 - t80 * t51 / 0.5760D4 + (
     #0.90D2 * t8 * (-t53 + t86 * t51) + t58) * t11 / 0.5760D4 - t62 * t
     #13 / 0.64D2 + (0.90D2 * t8 * (-t53 + t98 * t51) + t58) * t12 / 0.5
     #760D4 - t109 * t53 / 0.5760D4
      t113 = FJET(XB1, XB2, s, 0.0D0, t45, 0.0D0, 0.0D0, 0.0D0, t112)
      t115 = -0.1D1 + x3
      t116 = t45 * t115
      t117 = t45 * x3
      t118 = -t115
      t119 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t120 = t8 * t119
      t128 = log(-0.4D1 * t83 * t22 * t115)
      t130 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t139 = t120 * t63 / 0.32D2 + t120 * t13 / 0.64D2 + (0.90D2 * t8 * 
     #(-t128 * t119 + t130) - 0.180D3 * t32 * t119) * t11 / 0.5760D4
      t140 = FJET(XB1, XB2, s, 0.0D0, -t116, 0.0D0, t117, 0.0D0, t139)
      t142 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t143 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t153 = t8 * t143
      t158 = (0.90D2 * t8 * (t142 - t128 * t143) - 0.180D3 * t32 * t143)
     # * t11 / 0.5760D4 + t153 * t63 / 0.32D2 + t153 * t13 / 0.64D2
      t159 = FJET(XB1, XB2, s, t117, 0.0D0, -t116, 0.0D0, 0.0D0, t158)
      t161 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t162 = t8 * t161
      t168 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t177 = t162 * t39 / 0.32D2 + t162 * t13 / 0.64D2 + (0.90D2 * t8 * 
     #(-t27 * t161 + t168) - 0.180D3 * t32 * t161) * t12 / 0.5760D4
      t178 = FJET(XB1, XB2, s, 0.0D0, -t6, 0.0D0, t3, 0.0D0, t177)
      t180 = FJET(XB1, XB2, s, -t116, 0.0D0, t117, 0.0D0, 0.0D0, t139)
      t182 = FJET(XB1, XB2, s, -t6, 0.0D0, t3, 0.0D0, 0.0D0, t177)
      t184 = -0.1D1 + x1
      t188 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t190 = t45 * t184 * x2 * t188
      t191 = t45 * x1
      t192 = t2 * t184
      t193 = t5 * t192
      t194 = t2 ** 2
      t199 = s * t194 * x2 * x1 * t184 * t188
      t200 = -t184
      t201 = bbggh52J1(s, XB1, XB2, z, lh, wd, t200, x2, 0.10D1, x4)
      t204 = t8 * t201 * t39 / 0.32D2
      t205 = FJET(XB1, XB2, s, 0.0D0, -t190, t191, t193, -t199, -t204)
      t208 = t201 * t12 * t38
      t211 = bbggh51J1(s, XB1, XB2, z, lh, wd, t200, x2, 0.10D1, x4)
      t214 = t8 * t211 * t39 / 0.32D2
      t215 = FJET(XB1, XB2, s, t193, t191, -t190, 0.0D0, -t199, -t214)
      t218 = t211 * t12 * t38
      t221 = FJET(XB1, XB2, s, t191, t193, 0.0D0, -t190, -t199, -t214)
      t226 = t45 * x1 * x3
      t228 = t45 * t184 * x3
      t229 = t115 * s
      t231 = t229 * t2 * x1
      t232 = t229 * t192
      t233 = bbggh52J1(s, XB1, XB2, z, lh, wd, t200, 0.0D0, t118, x4)
      t236 = t8 * t233 * t63 / 0.32D2
      t237 = FJET(XB1, XB2, s, t226, -t228, -t231, t232, 0.0D0, -t236)
      t240 = t233 * t11 * t38
      t243 = bbggh51J1(s, XB1, XB2, z, lh, wd, t200, 0.0D0, t118, x4)
      t246 = t8 * t243 * t63 / 0.32D2
      t247 = FJET(XB1, XB2, s, t232, -t231, -t228, t226, 0.0D0, -t246)
      t250 = t243 * t11 * t38
      t253 = FJET(XB1, XB2, s, -t190, 0.0D0, t193, t191, -t199, -t204)
      t257 = FJET(XB1, XB2, s, -t228, t226, t232, -t231, 0.0D0, -t236)
      t261 = t43 * t42 + t113 * t112 + t140 * t139 + t159 * t158 + t178 
     #* t177 + t180 * t139 + t182 * t177 - t205 * t8 * t208 / 0.32D2 - t
     #215 * t8 * t218 / 0.32D2 - t221 * t8 * t218 / 0.32D2 - t237 * t8 *
     # t240 / 0.32D2 - t247 * t8 * t250 / 0.32D2 - t253 * t8 * t208 / 0.
     #32D2 - t257 * t8 * t240 / 0.32D2
      t262 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t6, 0.0D0, t42)
      t264 = t45 * t184
      t266 = t184 ** 2
      t270 = log(0.4D1 * t47 * t22 * t188 * t266)
      t271 = bbggh52J1(s, XB1, XB2, z, lh, wd, t200, 0.0D0, 0.10D1, x4)
      t273 = bbggh52J2(s, XB1, XB2, z, lh, wd, t200, 0.0D0, 0.10D1, x4)
      t282 = t8 * t271
      t287 = (0.90D2 * t8 * (-t270 * t271 + t273) - 0.180D3 * t32 * t271
     #) * t38 / 0.2880D4 + t282 * t63 / 0.32D2 + t282 * t39 / 0.32D2
      t288 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t191, -t264, 0.0D0, t287)
      t290 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t264, t191, 0.0D0, t287)
      t292 = FJET(XB1, XB2, s, 0.0D0, t117, 0.0D0, -t116, 0.0D0, t158)
      t294 = bbggh51J2(s, XB1, XB2, z, lh, wd, t200, 0.0D0, 0.10D1, x4)
      t295 = bbggh51J1(s, XB1, XB2, z, lh, wd, t200, 0.0D0, 0.10D1, x4)
      t305 = t8 * t295
      t310 = (0.90D2 * t8 * (t294 - t270 * t295) - 0.180D3 * t32 * t295)
     # * t38 / 0.2880D4 + t305 * t63 / 0.32D2 + t305 * t39 / 0.32D2
      t311 = FJET(XB1, XB2, s, -t264, t191, 0.0D0, 0.0D0, 0.0D0, t310)
      t313 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t314 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t320 = 0.180D3 * t32 * t314
      t324 = t8 * t314
      t349 = (0.90D2 * t8 * (-t313 + t50 * t314) + t320) * t38 / 0.2880D
     #4 - t324 * t63 / 0.32D2 - t324 * t39 / 0.32D2 - t324 * t13 / 0.64D
     #2 + (0.90D2 * t8 * (t98 * t314 - t313) + t320) * t12 / 0.5760D4 - 
     #t109 * t313 / 0.5760D4 - t80 * t314 / 0.5760D4 + (0.90D2 * t8 * (t
     #86 * t314 - t313) + t320) * t11 / 0.5760D4
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t45, 0.0D0, t349)
      t352 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t45, 0.0D0, 0.0D0, t349)
      t354 = FJET(XB1, XB2, s, t191, -t264, 0.0D0, 0.0D0, 0.0D0, t310)
      t356 = FJET(XB1, XB2, s, t45, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t112)
      t358 = FJET(XB1, XB2, s, -t231, t232, t226, -t228, 0.0D0, -t246)
      t362 = x2 * x3
      t363 = cos(t17)
      t365 = Sqrt(-t362 * t115)
      t366 = t363 * t365
      t367 = 0.2D1 * t366
      t371 = 0.1D1 / (t362 - 0.1D1)
      t373 = t45 * t4 * (-t362 - 0.1D1 + x3 + t367) * t371
      t375 = x2 ** 2
      t376 = t375 * x3
      t378 = 0.2D1 * t366 * x2
      t381 = t45 * (-x2 - x3 + 0.3D1 * t362 - t376 - t367 + t378) * t371
      t382 = x2 * z
      t383 = 0.1D1 + t382 - x2
      t384 = t8 * t383
      t385 = t115 * t371
      t386 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t385, x4)
      t394 = 0.1D1 / (-t376 * z + t376 - t382 + t362 * z + x2 - 0.2D1 * 
     #t362 + 0.2D1 * t366 * t382 - t378 - 0.1D1 + t367)
      t396 = t394 * t11 * t12
      t398 = t384 * t386 * t396 / 0.64D2
      t399 = FJET(XB1, XB2, s, 0.0D0, -t373, 0.0D0, t381, 0.0D0, t398)
      t403 = t386 * t394 * t13
      t406 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t385, x4)
      t409 = t384 * t406 * t396 / 0.64D2
      t410 = FJET(XB1, XB2, s, 0.0D0, t381, 0.0D0, -t373, 0.0D0, t409)
      t414 = t406 * t394 * t13
      t417 = FJET(XB1, XB2, s, -t373, 0.0D0, t381, 0.0D0, 0.0D0, t398)
      t422 = FJET(XB1, XB2, s, t381, 0.0D0, -t373, 0.0D0, 0.0D0, t409)
      t427 = t262 * t42 + t288 * t287 + t290 * t287 + t292 * t158 + t311
     # * t310 + t350 * t349 + t352 * t349 + t354 * t310 + t356 * t112 - 
     #t358 * t8 * t250 / 0.32D2 + t399 * t8 * t383 * t403 / 0.64D2 + t41
     #0 * t8 * t383 * t414 / 0.64D2 + t417 * t8 * t383 * t403 / 0.64D2 +
     # t422 * t8 * t383 * t414 / 0.64D2
      bbggh5n2em1 = t261 + t427

      end function



      doubleprecision function bbggh5n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t17 = z ** 2
      t20 = Sin(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t17 * t21)
      t27 = (-0.180D3 * lh - 0.90D2 * t24) * t4
      t30 = 0.1D1 / x3
      t33 = -t6 * t7 / 0.32D2 - t6 * t10 / 0.64D2 - t4 * t13 / 0.64D2 - 
     #t27 * t5 / 0.5760D4 - t6 * t30 / 0.64D2
      t34 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t33)
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t33)
      t38 = t2 * x1
      t39 = -0.1D1 + x1
      t40 = t2 * t39
      t41 = -t39
      t42 = bbggh52J1(s, XB1, XB2, z, lh, wd, t41, 0.0D0, 0.10D1, x4)
      t45 = t4 * t42 * t7 / 0.32D2
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t38, -t40, 0.0D0, t45)
      t48 = t42 * t7
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t40, t38, 0.0D0, t45)
      t55 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t56 = t4 * t55
      t65 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t68 = -t56 * t7 / 0.32D2 - t27 * t55 / 0.5760D4 - t56 * t30 / 0.64
     #D2 - t56 * t10 / 0.64D2 - t4 * t65 / 0.64D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t68)
      t71 = t2 * x3
      t72 = -0.1D1 + x3
      t73 = t2 * t72
      t74 = -t72
      t75 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t74, x4)
      t78 = t4 * t75 * t30 / 0.64D2
      t79 = FJET(XB1, XB2, s, 0.0D0, t71, 0.0D0, -t73, 0.0D0, t78)
      t81 = t75 * t30
      t85 = x2 * s * t1
      t88 = (-0.1D1 + x2) * s * t1
      t89 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t92 = t4 * t89 * t10 / 0.64D2
      t93 = FJET(XB1, XB2, s, 0.0D0, t85, 0.0D0, -t88, 0.0D0, t92)
      t95 = t89 * t10
      t98 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t74, x4)
      t101 = t4 * t98 * t30 / 0.64D2
      t102 = FJET(XB1, XB2, s, 0.0D0, -t73, 0.0D0, t71, 0.0D0, t101)
      t104 = t98 * t30
      t107 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t110 = t4 * t107 * t10 / 0.64D2
      t111 = FJET(XB1, XB2, s, 0.0D0, -t88, 0.0D0, t85, 0.0D0, t110)
      t113 = t107 * t10
      t116 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t68)
      t118 = bbggh51J1(s, XB1, XB2, z, lh, wd, t41, 0.0D0, 0.10D1, x4)
      t121 = t4 * t118 * t7 / 0.32D2
      t122 = FJET(XB1, XB2, s, t38, -t40, 0.0D0, 0.0D0, 0.0D0, t121)
      t124 = t118 * t7
      t127 = FJET(XB1, XB2, s, t71, 0.0D0, -t73, 0.0D0, 0.0D0, t78)
      t131 = FJET(XB1, XB2, s, t85, 0.0D0, -t88, 0.0D0, 0.0D0, t92)
      t135 = FJET(XB1, XB2, s, -t40, t38, 0.0D0, 0.0D0, 0.0D0, t121)
      t139 = FJET(XB1, XB2, s, -t88, 0.0D0, t85, 0.0D0, 0.0D0, t110)
      t143 = FJET(XB1, XB2, s, -t73, 0.0D0, t71, 0.0D0, 0.0D0, t101)
      bbggh5n2em2 = t34 * t33 + t36 * t33 + t46 * t4 * t48 / 0.32D2 + t5
     #1 * t4 * t48 / 0.32D2 + t69 * t68 + t79 * t4 * t81 / 0.64D2 + t93 
     #* t4 * t95 / 0.64D2 + t102 * t4 * t104 / 0.64D2 + t111 * t4 * t113
     # / 0.64D2 + t116 * t68 + t122 * t4 * t124 / 0.32D2 + t127 * t4 * t
     #81 / 0.64D2 + t131 * t4 * t95 / 0.64D2 + t135 * t4 * t124 / 0.32D2
     # + t139 * t4 * t113 / 0.64D2 + t143 * t4 * t104 / 0.64D2

      end function



      doubleprecision function bbggh5n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t14 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t16 = t4 * t14 / 0.64D2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t16)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t16)
      bbggh5n2em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2 - t1
     #7 * t4 * t14 / 0.64D2 - t20 * t4 * t14 / 0.64D2

      end function



      doubleprecision function bbggh5n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      bbggh5n2em4 = 0.0D0

      end function


      doubleprecision function bbggh5n3e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = lh * t4
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = t10 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 ** 2
      t18 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t21 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = t30 * t4
      t35 = t17 * t16
      t44 = 0.120D3 * t26 * lh
      t46 = 0.60D2 * lh * t28
      t47 = -0.2884936567583026D3 - t44 + t46
      t48 = t47 * t4
      t49 = t48 * t18
      t51 = 0.1D1 / x1
      t54 = t6 * x3
      t55 = t9 * t13
      t56 = -0.1D1 + x3
      t57 = 0.1D1 / t56
      t58 = t55 * t57
      t61 = log(-0.4D1 * t54 * t58)
      t62 = t61 * z
      t63 = cos(t7)
      t64 = x3 * z
      t66 = Sqrt(-t64 * t56)
      t70 = 0.1D1 / (-z - x3 + 0.2D1 * t63 * t66)
      t71 = t70 * t18
      t73 = z * t70
      t74 = t73 * t21
      t77 = log(0.4D1 * t54 * t55)
      t82 = t61 ** 2
      t83 = t82 * z
      t86 = t77 ** 2
      t89 = t70 * t21
      t95 = t73 * t18
      t99 = 0.1D1 / x3
      t103 = 0.1D1 - x2
      t104 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t103, 0.10D1, x4)
      t105 = x2 ** 2
      t106 = x3 * t105
      t109 = log(0.4D1 * t106 * t14)
      t111 = t106 * t6
      t112 = -t103
      t113 = t55 * t112
      t116 = log(-0.4D1 * t111 * t113)
      t117 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t103, 0.10D1, x4)
      t121 = log(-0.4D1 * t111 * t58)
      t122 = t121 * z
      t127 = -t95 + t117 - t18
      t132 = 0.1D1 / x2
      t133 = t132 * t51
      t136 = t105 * t6
      t139 = log(0.4D1 * t136 * t55)
      t143 = log(-0.4D1 * t136 * t113)
      t149 = t139 ** 2
      t153 = t143 ** 2
      t159 = -t18 + t117
      t166 = log(0.4D1 * t55)
      t168 = t166 ** 2
      t171 = t168 * t166
      t174 = (-t166 * t30 - 0.90D2 * t168 * lh - 0.2884936567583026D3 - 
     #t44 + t46 - 0.15D2 * t171) * t4
      t177 = t168 ** 2
      t181 = t28 ** 2
      t182 = t26 ** 2
      t191 = (0.15D2 / 0.4D1 * t177 - t166 * t47 + 0.5769873135166051D3 
     #* lh + t181 + 0.60D2 * t182 - 0.60D2 * t26 * t28 + t168 * t30 / 0.
     #2D1 + 0.30D2 * t171 * lh) * t4
      t199 = x3 * t13
      t202 = log(0.4D1 * t199 * t9)
      t203 = t202 ** 2
      t207 = log(-0.4D1 * t199 * t9 * t57)
      t208 = t207 ** 2
      t212 = t203 / 0.2D1 + t208 * z * t70 / 0.2D1
      t220 = -t207 * z * t70 - t202
      t228 = -t203 * t202 / 0.6D1 - t208 * t207 * z * t70 / 0.6D1
      t233 = t73 + 0.1D1
      t238 = t13 * t105
      t241 = log(0.4D1 * t238 * t9)
      t243 = t241 ** 2
      t246 = t9 * t112
      t249 = log(-0.4D1 * t238 * t246)
      t251 = t249 ** 2
      t263 = t243 * t241
      t268 = t251 * t249
      t280 = log(0.4D1 * t106 * t55)
      t284 = log(-0.4D1 * t106 * t113)
      t288 = log(-0.4D1 * t106 * t58)
      t289 = t288 * z
      t295 = t288 ** 2
      t296 = t295 * z
      t301 = t280 ** 2
      t304 = t284 ** 2
      t315 = (-0.180D3 * t5 * (-t17 * t18 / 0.2D1 + t16 * t21) + t31 * (
     #-t21 + t16 * t18) + 0.90D2 * t4 * (t35 * t18 / 0.6D1 - t17 * t21 /
     # 0.2D1) - t49) * t51 / 0.5760D4 + (-0.180D3 * t5 * (t62 * t71 - t2
     #1 - t74 + t77 * t18) + 0.90D2 * t4 * (-t83 * t71 / 0.2D1 - t86 * t
     #18 / 0.2D1 + t62 * t89 + t77 * t21) + t31 * (-t95 - t18)) * t99 * 
     #t51 / 0.5760D4 + (0.90D2 * t4 * (t104 - t21 + t109 * t18 - t116 * 
     #t117 + t122 * t71 - t74) - 0.180D3 * t5 * t127) * t99 * t133 / 0.2
     #880D4 + (-0.180D3 * t5 * (t139 * t18 + t104 - t143 * t117 - t21) +
     # 0.90D2 * t4 * (t139 * t21 - t149 * t18 / 0.2D1 - t143 * t104 + t1
     #53 * t117 / 0.2D1) + t31 * t159) * t132 * t51 / 0.2880D4 - t174 * 
     #t21 / 0.11520D5 - t191 * t18 / 0.11520D5 - ((0.90D2 * t4 * t21 - 0
     #.180D3 * t5 * t18) * t212 + (-0.180D3 * t5 * t21 + t31 * t18) * t2
     #20 + 0.90D2 * t4 * t18 * t228 + (t31 * t21 + t49) * t233) * t99 / 
     #0.11520D5 + (-0.180D3 * t5 * (t241 * t21 - t243 * t18 / 0.2D1 - t2
     #49 * t104 + t251 * t117 / 0.2D1) + t31 * (-t249 * t117 + t104 - t2
     #1 + t241 * t18) + 0.90D2 * t4 * (-t243 * t21 / 0.2D1 + t263 * t18 
     #/ 0.6D1 + t251 * t104 / 0.2D1 - t268 * t117 / 0.6D1) + t48 * t159)
     # * t132 / 0.5760D4 + (-0.180D3 * t5 * (t280 * t18 - t21 + t104 - t
     #284 * t117 - t74 + t289 * t71) + 0.90D2 * t4 * (t289 * t89 - t296 
     #* t71 / 0.2D1 + t280 * t21 - t284 * t104 - t301 * t18 / 0.2D1 + t3
     #04 * t117 / 0.2D1) + t31 * t127) * t99 * t132 / 0.5760D4
      t316 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t315)
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t315)
      t320 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t322 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t338 = t48 * t322
      t342 = t73 * t320
      t343 = t70 * t322
      t352 = t70 * t320
      t359 = t73 * t322
      t367 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t103, 0.10D1, x4)
      t368 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t103, 0.10D1, x4)
      t374 = -t322 - t359 + t368
      t395 = -t322 + t368
      t476 = (-0.180D3 * t5 * (t16 * t320 - t17 * t322 / 0.2D1) + t31 * 
     #(-t320 + t16 * t322) + 0.90D2 * t4 * (-t17 * t320 / 0.2D1 + t35 * 
     #t322 / 0.6D1) - t338) * t51 / 0.5760D4 + (-0.180D3 * t5 * (-t342 +
     # t62 * t343 - t320 + t77 * t322) + 0.90D2 * t4 * (t77 * t320 - t86
     # * t322 / 0.2D1 + t62 * t352 - t83 * t343 / 0.2D1) + t31 * (-t359 
     #- t322)) * t99 * t51 / 0.5760D4 + (0.90D2 * t4 * (t122 * t343 - t3
     #42 + t367 - t320 - t116 * t368 + t109 * t322) - 0.180D3 * t5 * t37
     #4) * t99 * t133 / 0.2880D4 + (-0.180D3 * t5 * (t139 * t322 - t320 
     #+ t367 - t143 * t368) + 0.90D2 * t4 * (t139 * t320 - t143 * t367 -
     # t149 * t322 / 0.2D1 + t153 * t368 / 0.2D1) + t31 * t395) * t132 *
     # t51 / 0.2880D4 - t174 * t320 / 0.11520D5 - t191 * t322 / 0.11520D
     #5 - ((0.90D2 * t4 * t320 - 0.180D3 * t5 * t322) * t212 + (-0.180D3
     # * t5 * t320 + t31 * t322) * t220 + 0.90D2 * t4 * t322 * t228 + (t
     #31 * t320 + t338) * t233) * t99 / 0.11520D5 + (-0.180D3 * t5 * (t2
     #41 * t320 - t243 * t322 / 0.2D1 - t249 * t367 + t251 * t368 / 0.2D
     #1) + t31 * (t367 - t249 * t368 - t320 + t241 * t322) + 0.90D2 * t4
     # * (-t243 * t320 / 0.2D1 + t263 * t322 / 0.6D1 + t251 * t367 / 0.2
     #D1 - t268 * t368 / 0.6D1) + t48 * t395) * t132 / 0.5760D4 + (-0.18
     #0D3 * t5 * (t280 * t322 - t342 - t320 - t284 * t368 + t367 + t289 
     #* t343) + 0.90D2 * t4 * (t280 * t320 - t284 * t367 + t289 * t352 +
     # t304 * t368 / 0.2D1 - t301 * t322 / 0.2D1 - t296 * t343 / 0.2D1) 
     #+ t31 * t374) * t99 * t132 / 0.5760D4
      t477 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t476)
      t479 = x2 * x3
      t480 = 0.1D1 - x3 + t479
      t481 = 0.1D1 / t480
      t482 = t479 * t481
      t483 = t2 * t482
      t484 = t56 * t481
      t485 = t2 * t484
      t486 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t103, -t484, x4)
      t487 = z * t486
      t488 = t106 * t10
      t490 = t480 ** 2
      t491 = 0.1D1 / t490
      t492 = t56 * t491
      t496 = log(0.4D1 * t488 * t13 * t112 * t492)
      t497 = t496 * z
      t498 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t103, -t484, x4)
      t502 = t112 * t56
      t504 = Sqrt(t64 * t502)
      t508 = 0.1D1 / (-z - x3 + t479 + 0.2D1 * t63 * t504)
      t512 = z * t498 * t508
      t523 = log(0.4D1 * t106 * t13 * t246 * t492)
      t524 = t523 * z
      t531 = t523 ** 2
      t532 = t531 * z
      t544 = (0.90D2 * t4 * (t487 - t497 * t498) * t508 - 0.180D3 * t5 *
     # t512) * t99 * t133 / 0.2880D4 + (-0.180D3 * t5 * (t487 - t524 * t
     #498) * t508 + 0.90D2 * t4 * (-t524 * t486 + t532 * t498 / 0.2D1) *
     # t508 + t31 * t512) * t99 * t132 / 0.5760D4
      t545 = FJET(XB1, XB2, s, 0.0D0, t483, 0.0D0, -t485, 0.0D0, t544)
      t548 = t1 * x1
      t549 = x1 * z
      t550 = -z - x1 + t549
      t551 = 0.1D1 / t550
      t553 = t112 * s * t548 * t551
      t554 = -0.1D1 + x1
      t555 = t2 * t554
      t557 = x2 * s * t548
      t558 = t1 ** 2
      t559 = s * t558
      t562 = x1 * t554 * t551
      t563 = t559 * t112 * t562
      t564 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, t103, 0.10D1, x4)
      t565 = 0.1D1 / t11
      t566 = t565 * t551
      t567 = t554 ** 2
      t569 = t566 * t567 * t112
      t572 = log(0.4D1 * t488 * t569)
      t573 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, t103, 0.10D1, x4)
      t583 = t136 * t9
      t586 = log(0.4D1 * t583 * t569)
      t591 = t586 ** 2
      t603 = (0.90D2 * t4 * (-t564 + t572 * t573) + 0.180D3 * t5 * t573)
     # * t99 * t133 / 0.2880D4 + (-0.180D3 * t5 * (t586 * t573 - t564) +
     # 0.90D2 * t4 * (-t591 * t573 / 0.2D1 + t586 * t564) - t31 * t573) 
     #* t132 * t51 / 0.2880D4
      t604 = FJET(XB1, XB2, s, 0.0D0, t553, -t555, t557, -t563, t603)
      t607 = t2 * x1 * t551
      t608 = t559 * t562
      t609 = t566 * t567
      t612 = log(-0.4D1 * t10 * t609)
      t613 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t615 = t612 ** 2
      t616 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t627 = t615 * t612
      t637 = z * t550
      t638 = t637 * t613
      t639 = t54 * t9
      t641 = t566 * t567 * t57
      t644 = log(0.4D1 * t639 * t641)
      t645 = t644 * z
      t646 = t550 * t616
      t649 = x1 * x3
      t650 = t649 * z
      t652 = 0.2D1 * t54 * z
      t653 = x1 * t11
      t654 = x3 * t11
      t655 = t654 * x1
      t656 = t54 * t11
      t657 = x3 * t550
      t659 = Sqrt(t657 * t56)
      t664 = 0.1D1 / (-t549 - t650 - t64 - t54 + t652 + t653 + t655 - t6
     #56 + 0.2D1 * t63 * t659 * z - t11)
      t668 = log(-0.4D1 * t639 * t609)
      t675 = t644 ** 2
      t676 = t675 * z
      t682 = t668 ** 2
      t690 = t616 - t637 * t616 * t664
      t698 = log(0.4D1 * t488 * t641)
      t699 = t698 * z
      t704 = t551 * t567
      t708 = log(-0.4D1 * t111 * t9 * t565 * t704)
      t721 = log(-0.4D1 * t583 * t609)
      t727 = t721 ** 2
      t738 = (-0.180D3 * t5 * (-t612 * t613 + t615 * t616 / 0.2D1) + t31
     # * (t613 - t612 * t616) + 0.90D2 * t4 * (t615 * t613 / 0.2D1 - t62
     #7 * t616 / 0.6D1) + t48 * t616) * t51 / 0.5760D4 + (-0.180D3 * t5 
     #* (t613 - (t638 - t645 * t646) * t664 - t668 * t616) + 0.90D2 * t4
     # * (-(-t645 * t550 * t613 + t676 * t646 / 0.2D1) * t664 - t668 * t
     #613 + t682 * t616 / 0.2D1) + t31 * t690) * t99 * t51 / 0.5760D4 + 
     #(0.90D2 * t4 * (t613 - (t638 - t699 * t646) * t664 - t708 * t616) 
     #- 0.180D3 * t5 * t690) * t99 * t133 / 0.2880D4 + (-0.180D3 * t5 * 
     #(-t721 * t616 + t613) + 0.90D2 * t4 * (-t721 * t613 + t727 * t616 
     #/ 0.2D1) + t31 * t616) * t132 * t51 / 0.2880D4
      t739 = FJET(XB1, XB2, s, 0.0D0, -t555, -t607, 0.0D0, t608, t738)
      t741 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t743 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t764 = t637 * t741
      t765 = t550 * t743
      t786 = t743 - t637 * t743 * t664
      t820 = (-0.180D3 * t5 * (-t612 * t741 + t615 * t743 / 0.2D1) + t31
     # * (t741 - t612 * t743) + 0.90D2 * t4 * (t615 * t741 / 0.2D1 - t62
     #7 * t743 / 0.6D1) + t48 * t743) * t51 / 0.5760D4 + (-0.180D3 * t5 
     #* (-t668 * t743 + t741 - (t764 - t645 * t765) * t664) + 0.90D2 * t
     #4 * (-t668 * t741 - (-t645 * t550 * t741 + t676 * t765 / 0.2D1) * 
     #t664 + t682 * t743 / 0.2D1) + t31 * t786) * t99 * t51 / 0.5760D4 +
     # (0.90D2 * t4 * (t741 - t708 * t743 - (t764 - t699 * t765) * t664)
     # - 0.180D3 * t5 * t786) * t99 * t133 / 0.2880D4 + (-0.180D3 * t5 *
     # (-t721 * t743 + t741) + 0.90D2 * t4 * (-t721 * t741 + t727 * t743
     # / 0.2D1) + t31 * t743) * t132 * t51 / 0.2880D4
      t821 = FJET(XB1, XB2, s, 0.0D0, -t607, -t555, 0.0D0, t608, t820)
      t823 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t103, -t484, x4)
      t824 = z * t823
      t825 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t103, -t484, x4)
      t832 = z * t825 * t508
      t856 = (0.90D2 * t4 * (t824 - t497 * t825) * t508 - 0.180D3 * t5 *
     # t832) * t99 * t133 / 0.2880D4 + (-0.180D3 * t5 * (t824 - t524 * t
     #825) * t508 + 0.90D2 * t4 * (-t524 * t823 + t532 * t825 / 0.2D1) *
     # t508 + t31 * t832) * t99 * t132 / 0.5760D4
      t857 = FJET(XB1, XB2, s, 0.0D0, -t485, 0.0D0, t483, 0.0D0, t856)
      t859 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t476)
      t861 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, t103, 0.10D1, x4)
      t863 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, t103, 0.10D1, x4)
      t887 = (0.90D2 * t4 * (t572 * t861 - t863) + 0.180D3 * t5 * t861) 
     #* t99 * t133 / 0.2880D4 + (-0.180D3 * t5 * (-t863 + t586 * t861) +
     # 0.90D2 * t4 * (t586 * t863 - t591 * t861 / 0.2D1) - t31 * t861) *
     # t132 * t51 / 0.2880D4
      t888 = FJET(XB1, XB2, s, t557, -t555, t553, 0.0D0, -t563, t887)
      t890 = FJET(XB1, XB2, s, t483, 0.0D0, -t485, 0.0D0, 0.0D0, t544)
      t892 = FJET(XB1, XB2, s, t553, 0.0D0, t557, -t555, -t563, t603)
      t897 = t56 * s * t1 * t554 * t481
      t898 = t2 * x1
      t900 = Sqrt(-t657 * t502)
      t901 = t63 * t900
      t907 = t898 * x2 * (-x3 + t479 - z + t64 - x1 + t649 + t549 - t650
     # + 0.2D1 * t901) * t551 * t481
      t908 = t555 * t482
      t911 = t106 * x1
      t913 = t106 * t549
      t917 = t898 * (-x2 + t479 + 0.2D1 * t901 * x2 + 0.1D1 - x3 + t911 
     #+ t106 * z - t913) * t551 * t481
      t918 = x2 * x1
      t919 = t918 * z
      t920 = -z - t918 + t919
      t921 = t550 * t920
      t922 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, t103, -t484, x4)
      t930 = log(-0.4D1 * t106 * t10 * t565 * t704 * t502 * t491)
      t931 = t930 * t550
      t932 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, t103, -t484, x4)
      t933 = t920 * t932
      t945 = 0.2D1 * t901 * t919 + t549 + t650 - t652 - t655 + t656 - t9
     #11 + t919 - 0.2D1 * t901 * z - t54 * x2 - t479 * z + t479 * x1 - t
     #918 * t11
      t946 = x2 * t6
      t960 = -0.2D1 * t946 * z + t946 * t11 + t913 - 0.2D1 * t901 * t918
     # + t654 * t918 + 0.2D1 * t54 * x2 * z - t54 * t11 * x2 - 0.2D1 * t
     #479 * t549 + t946 + t64 + t54 - t653 + t11
      t962 = 0.1D1 / (t945 + t960)
      t965 = t5 * t550
      t969 = 0.90D2 * t4 * (t921 * t922 - t931 * t933) * t962 - 0.180D3 
     #* t965 * t933 * t962
      t972 = t969 * t99 * t133 / 0.2880D4
      t973 = FJET(XB1, XB2, s, t897, t907, -t908, -t917, -t563, t972)
      t976 = t99 * t132 * t51
      t979 = FJET(XB1, XB2, s, t907, t897, -t917, -t908, -t563, t972)
      t983 = FJET(XB1, XB2, s, -t555, 0.0D0, 0.0D0, -t607, t608, t738)
      t985 = FJET(XB1, XB2, s, -t555, t557, 0.0D0, t553, -t563, t887)
      t987 = FJET(XB1, XB2, s, -t607, 0.0D0, 0.0D0, -t555, t608, t820)
      t989 = FJET(XB1, XB2, s, -t485, 0.0D0, t483, 0.0D0, 0.0D0, t856)
      t991 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, t103, -t484, x4)
      t993 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, t103, -t484, x4)
      t994 = t920 * t993
      t1003 = 0.90D2 * t4 * (t921 * t991 - t931 * t994) * t962 - 0.180D3
     # * t965 * t994 * t962
      t1006 = t1003 * t99 * t133 / 0.2880D4
      t1007 = FJET(XB1, XB2, s, -t917, -t908, t907, t897, -t563, t1006)
      t1011 = FJET(XB1, XB2, s, -t908, -t917, t897, t907, -t563, t1006)
      bbggh5n3e1 = t316 * t315 + t318 * t315 + t477 * t476 + t545 * t544
     # + t604 * t603 + t739 * t738 + t821 * t820 + t857 * t856 + t859 * 
     #t476 + t888 * t887 + t890 * t544 + t892 * t603 + t973 * t969 * t97
     #6 / 0.2880D4 + t979 * t969 * t976 / 0.2880D4 + t983 * t738 + t985 
     #* t887 + t987 * t820 + t989 * t856 + t1007 * t1003 * t976 / 0.2880
     #D4 + t1011 * t1003 * t976 / 0.2880D4

      end function



      doubleprecision function bbggh5n3e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = t5 * x3
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = t9 * t12
      t14 = -0.1D1 + x3
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t19 = log(-0.4D1 * t6 * t16)
      t20 = t19 * z
      t21 = cos(t7)
      t22 = x3 * z
      t24 = Sqrt(-t22 * t14)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t24)
      t29 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t30 = t28 * t29
      t32 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t33 = z * t28
      t34 = t33 * t32
      t37 = log(0.4D1 * t6 * t13)
      t42 = lh * t4
      t43 = t33 * t29
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t53 = 0.1D1 - x2
      t54 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t53, 0.10D1, x4)
      t55 = -t43 + t54 - t29
      t57 = 0.1D1 / x2
      t59 = t48 * t57 * t50
      t62 = x2 ** 2
      t63 = t62 * t5
      t66 = log(0.4D1 * t63 * t13)
      t68 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t53, 0.10D1, x4)
      t69 = -t53
      t70 = t13 * t69
      t73 = log(-0.4D1 * t63 * t70)
      t78 = -t29 + t54
      t85 = t5 * t9
      t88 = log(0.4D1 * t85 * t12)
      t93 = t88 ** 2
      t100 = lh ** 2
      t101 = 0.180D3 * t100
      t102 = 0.3141592653589793D1 ** 2
      t103 = 0.30D2 * t102
      t104 = t101 - t103
      t105 = t104 * t4
      t106 = t105 * t29
      t115 = x3 * t12
      t119 = log(-0.4D1 * t115 * t9 * t15)
      t124 = log(0.4D1 * t115 * t9)
      t125 = -t119 * z * t28 - t124
      t128 = t124 ** 2
      t129 = t119 ** 2
      t133 = t128 / 0.2D1 + t129 * z * t28 / 0.2D1
      t139 = t33 + 0.1D1
      t144 = x3 * t62
      t147 = log(0.4D1 * t144 * t13)
      t151 = log(-0.4D1 * t144 * t70)
      t155 = log(-0.4D1 * t144 * t16)
      t156 = t155 * z
      t167 = t12 * t62
      t168 = t9 * t69
      t171 = log(-0.4D1 * t167 * t168)
      t175 = log(0.4D1 * t167 * t9)
      t181 = t175 ** 2
      t185 = t171 ** 2
      t196 = log(0.4D1 * t13)
      t199 = t196 ** 2
      t202 = (0.180D3 * t196 * lh + t101 - t103 + 0.45D2 * t199) * t4
      t215 = (-t196 * t104 - 0.90D2 * t199 * lh - 0.2884936567583026D3 -
     # 0.120D3 * t100 * lh + 0.60D2 * lh * t102 - 0.15D2 * t199 * t196) 
     #* t4
      t218 = (0.90D2 * t4 * (t20 * t30 - t32 - t34 + t37 * t29) - 0.180D
     #3 * t42 * (-t43 - t29)) * t48 * t50 / 0.5760D4 + t4 * t55 * t59 / 
     #0.32D2 + (0.90D2 * t4 * (t66 * t29 + t68 - t73 * t54 - t32) - 0.18
     #0D3 * t42 * t78) * t57 * t50 / 0.2880D4 + (-0.180D3 * t42 * (-t32 
     #+ t88 * t29) + 0.90D2 * t4 * (-t93 * t29 / 0.2D1 + t88 * t32) - t1
     #06) * t50 / 0.5760D4 - ((0.90D2 * t4 * t32 - 0.180D3 * t42 * t29) 
     #* t125 + 0.90D2 * t4 * t29 * t133 + (-0.180D3 * t42 * t32 + t106) 
     #* t139) * t48 / 0.11520D5 + (0.90D2 * t4 * (t147 * t29 - t32 + t68
     # - t151 * t54 - t34 + t156 * t30) - 0.180D3 * t42 * t55) * t48 * t
     #57 / 0.5760D4 + (-0.180D3 * t42 * (-t171 * t54 + t68 - t32 + t175 
     #* t29) + 0.90D2 * t4 * (t175 * t32 - t181 * t29 / 0.2D1 - t171 * t
     #68 + t185 * t54 / 0.2D1) + t105 * t78) * t57 / 0.5760D4 - t202 * t
     #32 / 0.11520D5 - t215 * t29 / 0.11520D5
      t219 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t218)
      t221 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t218)
      t223 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t224 = t33 * t223
      t225 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t226 = t28 * t225
      t232 = t33 * t225
      t240 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t53, 0.10D1, x4)
      t241 = -t225 - t232 + t240
      t246 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t53, 0.10D1, x4)
      t251 = -t225 + t240
      t268 = t105 * t225
      t322 = (0.90D2 * t4 * (-t224 + t20 * t226 - t223 + t37 * t225) - 0
     #.180D3 * t42 * (-t232 - t225)) * t48 * t50 / 0.5760D4 + t4 * t241 
     #* t59 / 0.32D2 + (0.90D2 * t4 * (t66 * t225 - t223 + t246 - t73 * 
     #t240) - 0.180D3 * t42 * t251) * t57 * t50 / 0.2880D4 + (-0.180D3 *
     # t42 * (-t223 + t88 * t225) + 0.90D2 * t4 * (t88 * t223 - t93 * t2
     #25 / 0.2D1) - t268) * t50 / 0.5760D4 - ((0.90D2 * t4 * t223 - 0.18
     #0D3 * t42 * t225) * t125 + 0.90D2 * t4 * t225 * t133 + (-0.180D3 *
     # t42 * t223 + t268) * t139) * t48 / 0.11520D5 + (0.90D2 * t4 * (t1
     #47 * t225 - t224 - t223 - t151 * t240 + t246 + t156 * t226) - 0.18
     #0D3 * t42 * t241) * t48 * t57 / 0.5760D4 + (-0.180D3 * t42 * (t246
     # - t171 * t240 - t223 + t175 * t225) + 0.90D2 * t4 * (t175 * t223 
     #- t181 * t225 / 0.2D1 - t171 * t246 + t185 * t240 / 0.2D1) + t105 
     #* t251) * t57 / 0.5760D4 - t202 * t223 / 0.11520D5 - t215 * t225 /
     # 0.11520D5
      t323 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t322)
      t325 = x2 * x3
      t326 = 0.1D1 - x3 + t325
      t327 = 0.1D1 / t326
      t328 = t325 * t327
      t329 = t2 * t328
      t330 = t14 * t327
      t331 = t2 * t330
      t332 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t53, -t330, x4)
      t335 = t326 ** 2
      t341 = log(0.4D1 * t144 * t12 * t168 * t14 / t335)
      t342 = t341 * z
      t343 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t53, -t330, x4)
      t347 = t69 * t14
      t349 = Sqrt(t22 * t347)
      t353 = 0.1D1 / (-z - x3 + t325 + 0.2D1 * t21 * t349)
      t364 = t4 * z
      t367 = t57 * t50
      t368 = t353 * t48 * t367
      t371 = (0.90D2 * t4 * (z * t332 - t342 * t343) * t353 - 0.180D3 * 
     #t42 * z * t343 * t353) * t48 * t57 / 0.5760D4 + t364 * t343 * t368
     # / 0.32D2
      t372 = FJET(XB1, XB2, s, 0.0D0, t329, 0.0D0, -t331, 0.0D0, t371)
      t375 = t1 * x1
      t376 = x1 * z
      t377 = -z - x1 + t376
      t378 = 0.1D1 / t377
      t380 = t69 * s * t375 * t378
      t381 = -0.1D1 + x1
      t382 = t2 * t381
      t384 = x2 * s * t375
      t385 = t1 ** 2
      t386 = s * t385
      t389 = x1 * t381 * t378
      t390 = t386 * t69 * t389
      t391 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, t53, 0.10D1, x4)
      t395 = t63 * t9
      t397 = 0.1D1 / t10 * t378
      t398 = t381 ** 2
      t403 = log(0.4D1 * t395 * t397 * t398 * t69)
      t405 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, t53, 0.10D1, x4)
      t415 = -t4 * t391 * t59 / 0.32D2 + (0.90D2 * t4 * (t403 * t391 - t
     #405) + 0.180D3 * t42 * t391) * t57 * t50 / 0.2880D4
      t416 = FJET(XB1, XB2, s, 0.0D0, t380, -t382, t384, -t390, t415)
      t419 = t2 * x1 * t378
      t420 = t386 * t389
      t421 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t422 = z * t377
      t424 = t6 * t9
      t429 = log(0.4D1 * t424 * t397 * t398 * t15)
      t430 = t429 * z
      t431 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t435 = x1 * x3
      t436 = t435 * z
      t438 = 0.2D1 * t6 * z
      t439 = x1 * t10
      t440 = x3 * t10
      t441 = t440 * x1
      t442 = t6 * t10
      t443 = x3 * t377
      t445 = Sqrt(t443 * t14)
      t450 = 0.1D1 / (-t376 - t436 - t22 - t6 + t438 + t439 + t441 - t44
     #2 + 0.2D1 * t21 * t445 * z - t10)
      t452 = t397 * t398
      t455 = log(-0.4D1 * t424 * t452)
      t462 = t431 - t422 * t431 * t450
      t474 = log(-0.4D1 * t395 * t452)
      t487 = log(-0.4D1 * t85 * t452)
      t493 = t487 ** 2
      t503 = (0.90D2 * t4 * (t421 - (t422 * t421 - t430 * t377 * t431) *
     # t450 - t455 * t431) - 0.180D3 * t42 * t462) * t48 * t50 / 0.5760D
     #4 + t4 * t462 * t59 / 0.32D2 + (0.90D2 * t4 * (-t474 * t431 + t421
     #) - 0.180D3 * t42 * t431) * t57 * t50 / 0.2880D4 + (-0.180D3 * t42
     # * (t421 - t487 * t431) + 0.90D2 * t4 * (-t487 * t421 + t493 * t43
     #1 / 0.2D1) + t105 * t431) * t50 / 0.5760D4
      t504 = FJET(XB1, XB2, s, 0.0D0, -t382, -t419, 0.0D0, t420, t503)
      t506 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t508 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t519 = t506 - t422 * t506 * t450
      t553 = (0.90D2 * t4 * (-t455 * t506 + t508 - (t422 * t508 - t430 *
     # t377 * t506) * t450) - 0.180D3 * t42 * t519) * t48 * t50 / 0.5760
     #D4 + t4 * t519 * t59 / 0.32D2 + (0.90D2 * t4 * (-t474 * t506 + t50
     #8) - 0.180D3 * t42 * t506) * t57 * t50 / 0.2880D4 + (-0.180D3 * t4
     #2 * (t508 - t487 * t506) + 0.90D2 * t4 * (-t487 * t508 + t493 * t5
     #06 / 0.2D1) + t105 * t506) * t50 / 0.5760D4
      t554 = FJET(XB1, XB2, s, 0.0D0, -t419, -t382, 0.0D0, t420, t553)
      t556 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t53, -t330, x4)
      t558 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t53, -t330, x4)
      t575 = (0.90D2 * t4 * (z * t556 - t342 * t558) * t353 - 0.180D3 * 
     #t42 * z * t558 * t353) * t48 * t57 / 0.5760D4 + t364 * t558 * t368
     # / 0.32D2
      t576 = FJET(XB1, XB2, s, 0.0D0, -t331, 0.0D0, t329, 0.0D0, t575)
      t578 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t322)
      t580 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, t53, 0.10D1, x4)
      t584 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, t53, 0.10D1, x4)
      t595 = -t4 * t580 * t59 / 0.32D2 + (0.90D2 * t4 * (-t584 + t403 * 
     #t580) + 0.180D3 * t42 * t580) * t57 * t50 / 0.2880D4
      t596 = FJET(XB1, XB2, s, t384, -t382, t380, 0.0D0, -t390, t595)
      t598 = FJET(XB1, XB2, s, t329, 0.0D0, -t331, 0.0D0, 0.0D0, t371)
      t600 = FJET(XB1, XB2, s, t380, 0.0D0, t384, -t382, -t390, t415)
      t605 = t14 * s * t1 * t381 * t327
      t606 = t2 * x1
      t608 = Sqrt(-t443 * t347)
      t609 = t21 * t608
      t615 = t606 * x2 * (-x3 + t325 - z + t22 - x1 + t435 + t376 - t436
     # + 0.2D1 * t609) * t378 * t327
      t616 = t382 * t328
      t619 = t144 * x1
      t621 = t144 * t376
      t625 = t606 * (-x2 + t325 + 0.2D1 * t609 * x2 + 0.1D1 - x3 + t619 
     #+ t144 * z - t621) * t378 * t327
      t626 = t4 * t377
      t627 = x2 * x1
      t628 = t627 * z
      t629 = -z - t627 + t628
      t630 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, t53, -t330, x4)
      t641 = x2 * t5
      t645 = 0.2D1 * t609 * t628 + t10 - t619 + t628 - 0.2D1 * t609 * z 
     #- x2 * t6 - t325 * z + t325 * x1 - t627 * t10 - 0.2D1 * t641 * z +
     # t641 * t10 + t436 - t438
      t656 = -t441 + t442 + t376 + t22 + t6 - t439 + t641 + t621 - 0.2D1
     # * t609 * t627 + t440 * t627 + 0.2D1 * t6 * x2 * z - t6 * t10 * x2
     # - 0.2D1 * t325 * t376
      t658 = 0.1D1 / (t645 + t656)
      t660 = t658 * t48 * t367
      t662 = t626 * t629 * t630 * t660 / 0.32D2
      t663 = FJET(XB1, XB2, s, t605, t615, -t616, -t625, -t390, t662)
      t665 = t377 * t629
      t668 = t630 * t658 * t59
      t671 = FJET(XB1, XB2, s, t615, t605, -t625, -t616, -t390, t662)
      t676 = FJET(XB1, XB2, s, -t382, 0.0D0, 0.0D0, -t419, t420, t503)
      t678 = FJET(XB1, XB2, s, -t382, t384, 0.0D0, t380, -t390, t595)
      t680 = FJET(XB1, XB2, s, -t419, 0.0D0, 0.0D0, -t382, t420, t553)
      t682 = FJET(XB1, XB2, s, -t331, 0.0D0, t329, 0.0D0, 0.0D0, t575)
      t684 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, t53, -t330, x4)
      t688 = t626 * t629 * t684 * t660 / 0.32D2
      t689 = FJET(XB1, XB2, s, -t625, -t616, t615, t605, -t390, t688)
      t693 = t684 * t658 * t59
      t696 = FJET(XB1, XB2, s, -t616, -t625, t605, t615, -t390, t688)
      bbggh5n3e0 = t219 * t218 + t221 * t218 + t323 * t322 + t372 * t371
     # + t416 * t415 + t504 * t503 + t554 * t553 + t576 * t575 + t578 * 
     #t322 + t596 * t595 + t598 * t371 + t600 * t415 + t663 * t4 * t665 
     #* t668 / 0.32D2 + t671 * t4 * t665 * t668 / 0.32D2 + t676 * t503 +
     # t678 * t595 + t680 * t553 + t682 * t575 + t689 * t4 * t665 * t693
     # / 0.32D2 + t696 * t4 * t665 * t693 / 0.32D2

      end function



      doubleprecision function bbggh5n3em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t22 = lh * t4
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = cos(t7)
      t30 = x3 * z
      t31 = -0.1D1 + x3
      t33 = Sqrt(-t30 * t31)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t33 * t29)
      t38 = z * t37
      t39 = t38 * t17
      t42 = 0.1D1 / x3
      t43 = t42 * t26
      t46 = 0.1D1 - x2
      t47 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t48 = -t17 + t47
      t50 = 0.1D1 / x2
      t51 = t50 * t26
      t56 = t42 * t50
      t59 = x2 ** 2
      t60 = t13 * t59
      t61 = -t46
      t65 = log(-0.4D1 * t60 * t9 * t61)
      t67 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t70 = log(0.4D1 * t60 * t9)
      t83 = log(0.4D1 * t13 * t9)
      t86 = (-0.180D3 * lh - 0.90D2 * t83) * t4
      t91 = lh ** 2
      t93 = 0.3141592653589793D1 ** 2
      t95 = t83 ** 2
      t98 = (0.180D3 * t83 * lh + 0.180D3 * t91 - 0.30D2 * t93 + 0.45D2 
     #* t95) * t4
      t102 = x3 * t13
      t107 = log(-0.4D1 * t102 * t9 / t31)
      t112 = log(0.4D1 * t102 * t9)
      t113 = -t107 * z * t37 - t112
      t119 = t38 + 0.1D1
      t124 = (0.90D2 * t4 * (-t5 + t16 * t17) + t24) * t26 / 0.5760D4 + 
     #t4 * (-t39 - t17) * t43 / 0.64D2 + t4 * t48 * t51 / 0.32D2 + t4 * 
     #(-t39 + t47 - t17) * t56 / 0.64D2 + (0.90D2 * t4 * (-t65 * t47 + t
     #67 - t5 + t70 * t17) - 0.180D3 * t22 * t48) * t50 / 0.5760D4 - t86
     # * t5 / 0.11520D5 - t98 * t17 / 0.11520D5 - (0.90D2 * t4 * t17 * t
     #113 + (0.90D2 * t4 * t5 - t24) * t119) * t42 / 0.11520D5
      t125 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t124)
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t124)
      t129 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t130 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t136 = 0.180D3 * t22 * t130
      t140 = t38 * t130
      t145 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t146 = -t130 + t145
      t154 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t179 = (0.90D2 * t4 * (-t129 + t16 * t130) + t136) * t26 / 0.5760D
     #4 + t4 * (-t140 - t130) * t43 / 0.64D2 + t4 * t146 * t51 / 0.32D2 
     #+ t4 * (-t130 - t140 + t145) * t56 / 0.64D2 + (0.90D2 * t4 * (t154
     # - t65 * t145 - t129 + t70 * t130) - 0.180D3 * t22 * t146) * t50 /
     # 0.5760D4 - t86 * t129 / 0.11520D5 - t98 * t130 / 0.11520D5 - (0.9
     #0D2 * t4 * t130 * t113 + (0.90D2 * t4 * t129 - t136) * t119) * t42
     # / 0.11520D5
      t180 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = x2 * x3
      t184 = 0.1D1 / (0.1D1 - x3 + t182)
      t186 = t2 * t182 * t184
      t187 = t31 * t184
      t188 = t2 * t187
      t189 = t4 * z
      t190 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t187, x4)
      t194 = Sqrt(t30 * t61 * t31)
      t198 = 0.1D1 / (-z - x3 + t182 + 0.2D1 * t29 * t194)
      t200 = t198 * t42 * t50
      t202 = t189 * t190 * t200 / 0.64D2
      t203 = FJET(XB1, XB2, s, 0.0D0, t186, 0.0D0, -t188, 0.0D0, t202)
      t207 = t190 * t198 * t56
      t211 = t1 * x1
      t212 = x1 * z
      t213 = -z - x1 + t212
      t214 = 0.1D1 / t213
      t216 = t61 * s * t211 * t214
      t217 = -0.1D1 + x1
      t218 = t2 * t217
      t220 = x2 * s * t211
      t221 = t1 ** 2
      t222 = s * t221
      t225 = x1 * t217 * t214
      t226 = t222 * t61 * t225
      t227 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t230 = t4 * t227 * t51 / 0.32D2
      t231 = FJET(XB1, XB2, s, 0.0D0, t216, -t218, t220, -t226, -t230)
      t234 = t227 * t50 * t26
      t238 = t2 * x1 * t214
      t239 = t222 * t225
      t240 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t243 = t217 ** 2
      t247 = log(-0.4D1 * t10 / t11 * t214 * t243)
      t248 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t258 = z * t213
      t261 = t6 * x3
      t270 = Sqrt(x3 * t213 * t31)
      t275 = 0.1D1 / (-t212 - x3 * x1 * z - t30 - t261 + 0.2D1 * t261 * 
     #z + x1 * t11 + x3 * t11 * x1 - t261 * t11 + 0.2D1 * t29 * t270 * z
     # - t11)
      t285 = (0.90D2 * t4 * (t240 - t247 * t248) - 0.180D3 * t22 * t248)
     # * t26 / 0.5760D4 + t4 * (t248 - t258 * t248 * t275) * t43 / 0.64D
     #2 + t4 * t248 * t51 / 0.32D2
      t286 = FJET(XB1, XB2, s, 0.0D0, -t218, -t238, 0.0D0, t239, t285)
      t288 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t289 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t308 = (0.90D2 * t4 * (t288 - t247 * t289) - 0.180D3 * t22 * t289)
     # * t26 / 0.5760D4 + t4 * (t289 - t258 * t289 * t275) * t43 / 0.64D
     #2 + t4 * t289 * t51 / 0.32D2
      t309 = FJET(XB1, XB2, s, 0.0D0, -t238, -t218, 0.0D0, t239, t308)
      t311 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t187, x4)
      t314 = t189 * t311 * t200 / 0.64D2
      t315 = FJET(XB1, XB2, s, 0.0D0, -t188, 0.0D0, t186, 0.0D0, t314)
      t319 = t311 * t198 * t56
      t322 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t179)
      t324 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t327 = t4 * t324 * t51 / 0.32D2
      t328 = FJET(XB1, XB2, s, t220, -t218, t216, 0.0D0, -t226, -t327)
      t331 = t324 * t50 * t26
      t334 = FJET(XB1, XB2, s, t186, 0.0D0, -t188, 0.0D0, 0.0D0, t202)
      t339 = FJET(XB1, XB2, s, t216, 0.0D0, t220, -t218, -t226, -t230)
      t343 = FJET(XB1, XB2, s, -t218, 0.0D0, 0.0D0, -t238, t239, t285)
      t345 = FJET(XB1, XB2, s, -t218, t220, 0.0D0, t216, -t226, -t327)
      t349 = FJET(XB1, XB2, s, -t238, 0.0D0, 0.0D0, -t218, t239, t308)
      t351 = FJET(XB1, XB2, s, -t188, 0.0D0, t186, 0.0D0, 0.0D0, t314)
      bbggh5n3em1 = t125 * t124 + t127 * t124 + t180 * t179 + t203 * t4 
     #* z * t207 / 0.64D2 - t231 * t4 * t234 / 0.32D2 + t286 * t285 + t3
     #09 * t308 + t315 * t4 * z * t319 / 0.64D2 + t322 * t179 - t328 * t
     #4 * t331 / 0.32D2 + t334 * t4 * z * t207 / 0.64D2 - t339 * t4 * t2
     #34 / 0.32D2 + t343 * t285 - t345 * t4 * t331 / 0.32D2 + t349 * t30
     #8 + t351 * t4 * z * t319 / 0.64D2

      end function



      doubleprecision function bbggh5n3em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 - x2
      t11 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t14 = 0.1D1 / x2
      t17 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t21 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t32 = (-0.180D3 * lh - 0.90D2 * t29) * t4
      t35 = cos(t24)
      t39 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t47 = (z / (-z - x3 + 0.2D1 * t35 * t39) + 0.1D1) / x3
      t50 = -t6 * t7 / 0.64D2 + t4 * (-t5 + t11) * t14 / 0.64D2 - t4 * t
     #17 / 0.128D3 - t32 * t5 / 0.11520D5 - t6 * t47 / 0.128D3
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t50)
      t55 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t56 = t4 * t55
      t59 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t64 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t71 = -t56 * t7 / 0.64D2 + t4 * (-t55 + t59) * t14 / 0.64D2 - t4 *
     # t64 / 0.128D3 - t32 * t55 / 0.11520D5 - t56 * t47 / 0.128D3
      t72 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t71)
      t74 = -0.1D1 + x1
      t75 = t2 * t74
      t78 = 0.1D1 / (-z - x1 + x1 * z)
      t80 = t2 * x1 * t78
      t81 = t1 ** 2
      t85 = s * t81 * x1 * t74 * t78
      t86 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t89 = t4 * t86 * t7 / 0.64D2
      t90 = FJET(XB1, XB2, s, 0.0D0, -t75, -t80, 0.0D0, t85, t89)
      t92 = t86 * t7
      t95 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t98 = t4 * t95 * t7 / 0.64D2
      t99 = FJET(XB1, XB2, s, 0.0D0, -t80, -t75, 0.0D0, t85, t98)
      t101 = t95 * t7
      t104 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t71)
      t106 = FJET(XB1, XB2, s, -t75, 0.0D0, 0.0D0, -t80, t85, t89)
      t110 = FJET(XB1, XB2, s, -t80, 0.0D0, 0.0D0, -t75, t85, t98)
      bbggh5n3em2 = t51 * t50 + t53 * t50 + t72 * t71 + t90 * t4 * t92 /
     # 0.64D2 + t99 * t4 * t101 / 0.64D2 + t104 * t71 + t106 * t4 * t92 
     #/ 0.64D2 + t110 * t4 * t101 / 0.64D2

      end function



      doubleprecision function bbggh5n3em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t7 = t4 * t5 / 0.128D3
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t14 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t16 = t4 * t14 / 0.128D3
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t16)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t16)
      bbggh5n3em3 = -t8 * t4 * t5 / 0.128D3 - t11 * t4 * t5 / 0.128D3 - 
     #t17 * t4 * t14 / 0.128D3 - t20 * t4 * t14 / 0.128D3

      end function



      doubleprecision function bbggh5n3em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      bbggh5n3em4 = 0.0D0

      end function


      doubleprecision function bbggh5n4e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = lh * t4
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = t10 * t13
      t16 = log(0.4D1 * t14)
      t17 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t19 = t16 ** 2
      t20 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = t30 * t4
      t37 = t19 * t16
      t44 = 0.120D3 * t26 * lh
      t46 = 0.60D2 * lh * t28
      t47 = -0.2884936567583026D3 - t44 + t46
      t48 = t47 * t4
      t49 = t48 * t20
      t51 = 0.1D1 / x1
      t54 = x3 * t6
      t55 = t13 * t9
      t58 = log(0.4D1 * t54 * t55)
      t64 = t58 ** 2
      t72 = 0.1D1 / x3
      t76 = x2 * x3
      t79 = log(0.4D1 * t76 * t14)
      t81 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t82 = t54 * x2
      t83 = -0.1D1 + x2
      t84 = t83 ** 2
      t85 = t55 * t84
      t88 = log(0.4D1 * t82 * t85)
      t89 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t94 = t20 - t89
      t99 = 0.1D1 / x2
      t100 = t99 * t51
      t103 = x2 * t6
      t106 = log(0.4D1 * t103 * t85)
      t110 = log(0.4D1 * t103 * t55)
      t115 = t110 ** 2
      t119 = t106 ** 2
      t126 = t31 * t94
      t134 = log(0.4D1 * x3 * t13 * t9)
      t136 = t134 ** 2
      t147 = t136 * t134
      t156 = x2 * t13
      t159 = log(0.4D1 * t156 * t9)
      t161 = t159 ** 2
      t164 = t9 * t84
      t167 = log(0.4D1 * t156 * t164)
      t169 = t167 ** 2
      t181 = t161 * t159
      t186 = t169 * t167
      t198 = log(0.4D1 * t76 * t55)
      t202 = log(0.4D1 * t76 * t85)
      t208 = t198 ** 2
      t211 = t202 ** 2
      t223 = log(0.4D1 * t55)
      t225 = t223 ** 2
      t228 = t225 * t223
      t231 = (-t223 * t30 - 0.90D2 * t225 * lh - 0.2884936567583026D3 - 
     #t44 + t46 - 0.15D2 * t228) * t4
      t234 = t225 ** 2
      t238 = t28 ** 2
      t239 = t26 ** 2
      t248 = (0.15D2 / 0.4D1 * t234 - t223 * t47 + 0.5769873135166051D3 
     #* lh + t238 + 0.60D2 * t239 - 0.60D2 * t26 * t28 + t225 * t30 / 0.
     #2D1 + 0.30D2 * t228 * lh) * t4
      t251 = -(-0.180D3 * t5 * (-t16 * t17 + t19 * t20 / 0.2D1) + t31 * 
     #(t17 - t16 * t20) + 0.90D2 * t4 * (t19 * t17 / 0.2D1 - t37 * t20 /
     # 0.6D1) + t49) * t51 / 0.2880D4 - (-0.180D3 * t5 * (t17 - t58 * t2
     #0) + 0.90D2 * t4 * (-t58 * t17 + t64 * t20 / 0.2D1) + t31 * t20) *
     # t72 * t51 / 0.2880D4 - (0.90D2 * t4 * (-t79 * t20 - t81 + t17 + t
     #88 * t89) - 0.180D3 * t5 * t94) * t72 * t100 / 0.2880D4 - (-0.180D
     #3 * t5 * (t17 - t81 + t106 * t89 - t110 * t20) + 0.90D2 * t4 * (t1
     #15 * t20 / 0.2D1 - t110 * t17 - t119 * t89 / 0.2D1 + t106 * t81) +
     # t126) * t99 * t51 / 0.2880D4 - (-0.180D3 * t5 * (-t134 * t17 + t1
     #36 * t20 / 0.2D1) + t31 * (-t134 * t20 + t17) + 0.90D2 * t4 * (t13
     #6 * t17 / 0.2D1 - t147 * t20 / 0.6D1) + t49) * t72 / 0.5760D4 - (-
     #0.180D3 * t5 * (-t159 * t17 + t161 * t20 / 0.2D1 + t167 * t81 - t1
     #69 * t89 / 0.2D1) + t31 * (-t81 + t167 * t89 + t17 - t159 * t20) +
     # 0.90D2 * t4 * (t161 * t17 / 0.2D1 - t181 * t20 / 0.6D1 - t169 * t
     #81 / 0.2D1 + t186 * t89 / 0.6D1) + t48 * t94) * t99 / 0.5760D4 - (
     #-0.180D3 * t5 * (-t198 * t20 - t81 + t17 + t202 * t89) + 0.90D2 * 
     #t4 * (-t198 * t17 + t208 * t20 / 0.2D1 - t211 * t89 / 0.2D1 + t202
     # * t81) + t126) * t72 * t99 / 0.5760D4 - t231 * t17 / 0.5760D4 - t
     #248 * t20 / 0.5760D4
      t252 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t251)
      t254 = t2 * x1
      t255 = x3 * z
      t256 = x1 * x3
      t257 = x1 * z
      t258 = t256 * z
      t259 = cos(t7)
      t260 = -z - x1 + t257
      t262 = -0.1D1 + x3
      t263 = x2 * t262
      t265 = Sqrt(x3 * t260 * t263)
      t266 = t259 * t265
      t267 = 0.2D1 * t266
      t270 = 0.1D1 / t260
      t271 = t76 - 0.1D1
      t272 = 0.1D1 / t271
      t275 = t254 * t83 * (-t76 - z + t255 - x1 + t256 + t257 - t258 + t
     #267) * t270 * t272
      t276 = t262 * s
      t277 = -0.1D1 + x1
      t278 = t1 * t277
      t280 = t276 * t278 * t272
      t281 = x2 ** 2
      t282 = t281 * x3
      t283 = t282 * x1
      t285 = t76 * z
      t287 = t76 * x1
      t291 = t282 * t257
      t294 = -t255 - t256 + t76 - t267 - x2 - t283 - t282 * z + t258 + 0
     #.2D1 * t285 + 0.2D1 * t287 + 0.2D1 * t266 * x2 + t291 - 0.2D1 * t7
     #6 * t257
      t297 = t254 * t294 * t270 * t272
      t298 = t2 * t277
      t300 = x3 * t83 * t272
      t301 = t298 * t300
      t302 = t1 ** 2
      t307 = s * t302 * x2 * x1 * t277 * t270
      t308 = x2 * x1
      t309 = t308 * z
      t310 = z + x1 - t257 - t308 + t309
      t311 = t260 * t310
      t312 = t262 * t272
      t313 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, x2, t312, x4)
      t315 = 0.1D1 / t11
      t318 = t277 ** 2
      t319 = t318 * t270
      t321 = t271 ** 2
      t322 = 0.1D1 / t321
      t327 = log(0.4D1 * t76 * t10 * t315 * t319 * t84 * t262 * t322)
      t328 = t327 * t260
      t329 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, x2, t312, x4)
      t330 = t310 * t329
      t344 = 0.2D1 * t266 * z + 0.2D1 * t266 * x1 - t6 - 0.2D1 * t257 - 
     #t82 + t283 + t309 - t285 - t287 + 0.2D1 * t266 * t309 - t308 * t11
     # - 0.2D1 * t103 * z
      t362 = t103 * t11 - 0.2D1 * t266 * t308 - 0.2D1 * t266 * t257 + 0.
     #2D1 * x1 * t11 + 0.2D1 * t6 * z - t6 * t11 + t103 - t11 + x3 * t11
     # * t308 + 0.2D1 * t54 * x2 * z - t54 * t11 * x2 - t291
      t364 = 0.1D1 / (t344 + t362)
      t367 = t5 * t260
      t371 = 0.90D2 * t4 * (-t311 * t313 + t328 * t330) * t364 + 0.180D3
     # * t367 * t330 * t364
      t374 = t371 * t72 * t100 / 0.2880D4
      t375 = FJET(XB1, XB2, s, t275, -t280, -t297, -t301, t307, -t374)
      t378 = t72 * t99 * t51
      t381 = t2 * t300
      t382 = t2 * t312
      t383 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t312, x4)
      t384 = z * t383
      t385 = t76 * t10
      t387 = t262 * t322
      t391 = log(-0.4D1 * t385 * t13 * t84 * t387)
      t392 = t391 * z
      t393 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t312, x4)
      t398 = Sqrt(-t255 * t263)
      t402 = 0.1D1 / (-z - t76 + 0.2D1 * t259 * t398)
      t406 = z * t393 * t402
      t417 = log(-0.4D1 * t76 * t13 * t164 * t387)
      t418 = t417 * z
      t425 = t417 ** 2
      t426 = t425 * z
      t438 = -(-0.90D2 * t4 * (t384 - t392 * t393) * t402 + 0.180D3 * t5
     # * t406) * t72 * t100 / 0.2880D4 - (0.180D3 * t5 * (t384 - t418 * 
     #t393) * t402 - 0.90D2 * t4 * (-t418 * t383 + t426 * t393 / 0.2D1) 
     #* t402 - t31 * t406) * t72 * t99 / 0.5760D4
      t439 = FJET(XB1, XB2, s, 0.0D0, t381, 0.0D0, t382, 0.0D0, t438)
      t441 = t1 * x1
      t442 = t276 * t441
      t443 = t276 * t278
      t444 = t2 * t256
      t446 = t2 * t277 * x3
      t447 = -t262
      t448 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t447, x4)
      t449 = t54 * t9
      t450 = t315 * t270
      t452 = t450 * t318 * t262
      t455 = log(0.4D1 * t449 * t452)
      t456 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t447, x4)
      t462 = t455 ** 2
      t474 = log(0.4D1 * t385 * t452)
      t485 = -(-0.180D3 * t5 * (t448 - t455 * t456) + 0.90D2 * t4 * (-t4
     #55 * t448 + t462 * t456 / 0.2D1) + t31 * t456) * t72 * t51 / 0.288
     #0D4 - (0.90D2 * t4 * (-t474 * t456 + t448) - 0.180D3 * t5 * t456) 
     #* t72 * t100 / 0.2880D4
      t486 = FJET(XB1, XB2, s, -t442, t443, t444, -t446, 0.0D0, t485)
      t489 = t2 * t308 * t270
      t491 = t83 * s * t441
      t492 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t495 = t315 * t318 * t270 * t84
      t498 = log(-0.4D1 * t385 * t495)
      t499 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t509 = t103 * t9
      t512 = log(-0.4D1 * t509 * t495)
      t517 = t512 ** 2
      t529 = -(0.90D2 * t4 * (t492 - t498 * t499) - 0.180D3 * t5 * t499)
     # * t72 * t100 / 0.2880D4 - (-0.180D3 * t5 * (-t512 * t499 + t492) 
     #+ 0.90D2 * t4 * (t517 * t499 / 0.2D1 - t512 * t492) + t31 * t499) 
     #* t99 * t51 / 0.2880D4
      t530 = FJET(XB1, XB2, s, 0.0D0, -t489, -t298, -t491, t307, t529)
      t532 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t533 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t558 = -(0.90D2 * t4 * (t532 - t498 * t533) - 0.180D3 * t5 * t533)
     # * t72 * t100 / 0.2880D4 - (-0.180D3 * t5 * (t532 - t512 * t533) +
     # 0.90D2 * t4 * (-t512 * t532 + t517 * t533 / 0.2D1) + t31 * t533) 
     #* t99 * t51 / 0.2880D4
      t559 = FJET(XB1, XB2, s, -t298, -t491, 0.0D0, -t489, t307, t558)
      t561 = t450 * t318
      t564 = log(-0.4D1 * t10 * t561)
      t565 = t564 ** 2
      t566 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t569 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t577 = t565 * t564
      t590 = log(-0.4D1 * t449 * t561)
      t595 = t590 ** 2
      t602 = t31 * t566
      t610 = log(-0.4D1 * t82 * t9 * t315 * t319)
      t622 = log(-0.4D1 * t509 * t561)
      t628 = t622 ** 2
      t638 = -(-0.180D3 * t5 * (-t565 * t566 / 0.2D1 + t564 * t569) + t3
     #1 * (-t569 + t564 * t566) + 0.90D2 * t4 * (t577 * t566 / 0.6D1 - t
     #565 * t569 / 0.2D1) - t48 * t566) * t51 / 0.2880D4 - (-0.180D3 * t
     #5 * (-t569 + t590 * t566) + 0.90D2 * t4 * (-t595 * t566 / 0.2D1 + 
     #t590 * t569) - t602) * t72 * t51 / 0.2880D4 - (0.90D2 * t4 * (t610
     # * t566 - t569) + 0.180D3 * t5 * t566) * t72 * t100 / 0.2880D4 - (
     #-0.180D3 * t5 * (-t569 + t622 * t566) + 0.90D2 * t4 * (t622 * t569
     # - t628 * t566 / 0.2D1) - t602) * t99 * t51 / 0.2880D4
      t639 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t298, t254, 0.0D0, t638)
      t641 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t312, x4)
      t642 = z * t641
      t643 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t312, x4)
      t650 = z * t643 * t402
      t674 = -(-0.90D2 * t4 * (t642 - t392 * t643) * t402 + 0.180D3 * t5
     # * t650) * t72 * t100 / 0.2880D4 - (0.180D3 * t5 * (t642 - t418 * 
     #t643) * t402 - 0.90D2 * t4 * (-t418 * t641 + t426 * t643 / 0.2D1) 
     #* t402 - t31 * t650) * t72 * t99 / 0.5760D4
      t675 = FJET(XB1, XB2, s, t382, 0.0D0, t381, 0.0D0, 0.0D0, t674)
      t677 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t447, x4)
      t678 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t447, x4)
      t703 = -(-0.180D3 * t5 * (t677 - t455 * t678) + 0.90D2 * t4 * (-t4
     #55 * t677 + t462 * t678 / 0.2D1) + t31 * t678) * t72 * t51 / 0.288
     #0D4 - (0.90D2 * t4 * (-t474 * t678 + t677) - 0.180D3 * t5 * t678) 
     #* t72 * t100 / 0.2880D4
      t704 = FJET(XB1, XB2, s, t444, -t446, -t442, t443, 0.0D0, t703)
      t706 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t708 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t737 = t31 * t708
      t764 = -(-0.180D3 * t5 * (t564 * t706 - t565 * t708 / 0.2D1) + t31
     # * (-t706 + t564 * t708) + 0.90D2 * t4 * (-t565 * t706 / 0.2D1 + t
     #577 * t708 / 0.6D1) - t48 * t708) * t51 / 0.2880D4 - (-0.180D3 * t
     #5 * (t590 * t708 - t706) + 0.90D2 * t4 * (t590 * t706 - t595 * t70
     #8 / 0.2D1) - t737) * t72 * t51 / 0.2880D4 - (0.90D2 * t4 * (-t706 
     #+ t610 * t708) + 0.180D3 * t5 * t708) * t72 * t100 / 0.2880D4 - (-
     #0.180D3 * t5 * (-t706 + t622 * t708) + 0.90D2 * t4 * (t622 * t706 
     #- t628 * t708 / 0.2D1) - t737) * t99 * t51 / 0.2880D4
      t765 = FJET(XB1, XB2, s, t254, -t298, 0.0D0, 0.0D0, 0.0D0, t764)
      t767 = t2 * x3
      t768 = t2 * t262
      t769 = t55 * t262
      t772 = log(-0.4D1 * t54 * t769)
      t773 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t447, x4)
      t775 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t447, x4)
      t779 = t772 ** 2
      t786 = t31 * t773
      t793 = log(-0.4D1 * t82 * t769)
      t806 = log(-0.4D1 * t76 * t769)
      t811 = t806 ** 2
      t825 = log(-0.4D1 * t55 * x3 * t262)
      t826 = t825 ** 2
      t836 = t826 * t825
      t848 = -(-0.180D3 * t5 * (t772 * t773 - t775) + 0.90D2 * t4 * (-t7
     #79 * t773 / 0.2D1 + t772 * t775) - t786) * t72 * t51 / 0.2880D4 - 
     #(0.90D2 * t4 * (t793 * t773 - t775) + 0.180D3 * t5 * t773) * t72 *
     # t100 / 0.2880D4 - (-0.180D3 * t5 * (-t775 + t806 * t773) + 0.90D2
     # * t4 * (-t811 * t773 / 0.2D1 + t806 * t775) - t786) * t72 * t99 /
     # 0.5760D4 - (-0.180D3 * t5 * (-t826 * t773 / 0.2D1 + t825 * t775) 
     #+ t31 * (-t775 + t825 * t773) + 0.90D2 * t4 * (t836 * t773 / 0.6D1
     # - t826 * t775 / 0.2D1) - t48 * t773) * t72 / 0.5760D4
      t849 = FJET(XB1, XB2, s, 0.0D0, t767, 0.0D0, -t768, 0.0D0, t848)
      t851 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t254, -t298, 0.0D0, t638)
      t853 = FJET(XB1, XB2, s, -t489, 0.0D0, -t491, -t298, t307, t529)
      t855 = FJET(XB1, XB2, s, -t298, t254, 0.0D0, 0.0D0, 0.0D0, t764)
      t857 = t252 * t251 - t375 * t371 * t378 / 0.2880D4 + t439 * t438 +
     # t486 * t485 + t530 * t529 + t559 * t558 + t639 * t638 + t675 * t6
     #74 + t704 * t703 + t765 * t764 + t849 * t848 + t851 * t638 + t853 
     #* t529 + t855 * t764
      t858 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t447, x4)
      t860 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t447, x4)
      t870 = t31 * t858
      t919 = -(-0.180D3 * t5 * (t772 * t858 - t860) + 0.90D2 * t4 * (-t7
     #79 * t858 / 0.2D1 + t772 * t860) - t870) * t72 * t51 / 0.2880D4 - 
     #(0.90D2 * t4 * (-t860 + t793 * t858) + 0.180D3 * t5 * t858) * t72 
     #* t100 / 0.2880D4 - (-0.180D3 * t5 * (t806 * t858 - t860) + 0.90D2
     # * t4 * (t806 * t860 - t811 * t858 / 0.2D1) - t870) * t72 * t99 / 
     #0.5760D4 - (-0.180D3 * t5 * (-t826 * t858 / 0.2D1 + t825 * t860) +
     # t31 * (t825 * t858 - t860) + 0.90D2 * t4 * (t836 * t858 / 0.6D1 -
     # t826 * t860 / 0.2D1) - t48 * t858) * t72 / 0.5760D4
      t920 = FJET(XB1, XB2, s, 0.0D0, -t768, 0.0D0, t767, 0.0D0, t919)
      t922 = FJET(XB1, XB2, s, 0.0D0, t382, 0.0D0, t381, 0.0D0, t674)
      t924 = FJET(XB1, XB2, s, t443, -t442, -t446, t444, 0.0D0, t485)
      t926 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t928 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t944 = t48 * t928
      t964 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t966 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t970 = t928 - t964
      t991 = t31 * t970
      t1065 = -(-0.180D3 * t5 * (-t16 * t926 + t19 * t928 / 0.2D1) + t31
     # * (t926 - t16 * t928) + 0.90D2 * t4 * (t19 * t926 / 0.2D1 - t37 *
     # t928 / 0.6D1) + t944) * t51 / 0.2880D4 - (-0.180D3 * t5 * (t926 -
     # t58 * t928) + 0.90D2 * t4 * (t64 * t928 / 0.2D1 - t58 * t926) + t
     #31 * t928) * t72 * t51 / 0.2880D4 - (0.90D2 * t4 * (-t79 * t928 + 
     #t88 * t964 - t966 + t926) - 0.180D3 * t5 * t970) * t72 * t100 / 0.
     #2880D4 - (-0.180D3 * t5 * (t106 * t964 + t926 - t966 - t110 * t928
     #) + 0.90D2 * t4 * (-t119 * t964 / 0.2D1 + t106 * t966 + t115 * t92
     #8 / 0.2D1 - t110 * t926) + t991) * t99 * t51 / 0.2880D4 - t231 * t
     #926 / 0.5760D4 - t248 * t928 / 0.5760D4 - (-0.180D3 * t5 * (-t134 
     #* t926 + t136 * t928 / 0.2D1) + t31 * (t926 - t134 * t928) + 0.90D
     #2 * t4 * (t136 * t926 / 0.2D1 - t147 * t928 / 0.6D1) + t944) * t72
     # / 0.5760D4 - (-0.180D3 * t5 * (-t159 * t926 + t161 * t928 / 0.2D1
     # + t167 * t966 - t169 * t964 / 0.2D1) + t31 * (t167 * t964 - t966 
     #+ t926 - t159 * t928) + 0.90D2 * t4 * (t161 * t926 / 0.2D1 - t181 
     #* t928 / 0.6D1 - t169 * t966 / 0.2D1 + t186 * t964 / 0.6D1) + t48 
     #* t970) * t99 / 0.5760D4 - (-0.180D3 * t5 * (t202 * t964 - t966 + 
     #t926 - t198 * t928) + 0.90D2 * t4 * (t208 * t928 / 0.2D1 - t198 * 
     #t926 + t202 * t966 - t211 * t964 / 0.2D1) + t991) * t72 * t99 / 0.
     #5760D4
      t1066 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1065)
      t1068 = FJET(XB1, XB2, s, t767, 0.0D0, -t768, 0.0D0, 0.0D0, t848)
      t1070 = FJET(XB1, XB2, s, -t446, t444, t443, -t442, 0.0D0, t703)
      t1072 = FJET(XB1, XB2, s, -t491, -t298, -t489, 0.0D0, t307, t558)
      t1074 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, x2, t312, x4)
      t1076 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, x2, t312, x4)
      t1077 = t310 * t1076
      t1086 = 0.90D2 * t4 * (-t311 * t1074 + t328 * t1077) * t364 + 0.18
     #0D3 * t367 * t1077 * t364
      t1089 = t1086 * t72 * t100 / 0.2880D4
      t1090 = FJET(XB1, XB2, s, -t297, -t301, t275, -t280, t307, -t1089)
      t1094 = FJET(XB1, XB2, s, -t280, t275, -t301, -t297, t307, -t374)
      t1098 = FJET(XB1, XB2, s, -t301, -t297, -t280, t275, t307, -t1089)
      t1102 = FJET(XB1, XB2, s, t381, 0.0D0, t382, 0.0D0, 0.0D0, t438)
      t1104 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1065)
      t1106 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t251)
      t1108 = FJET(XB1, XB2, s, -t768, 0.0D0, t767, 0.0D0, 0.0D0, t919)
      t1110 = t920 * t919 + t922 * t674 + t924 * t485 + t1066 * t1065 + 
     #t1068 * t848 + t1070 * t703 + t1072 * t558 - t1090 * t1086 * t378 
     #/ 0.2880D4 - t1094 * t371 * t378 / 0.2880D4 - t1098 * t1086 * t378
     # / 0.2880D4 + t1102 * t438 + t1104 * t1065 + t1106 * t251 + t1108 
     #* t919
      bbggh5n4e1 = t857 + t1110

      end function



      doubleprecision function bbggh5n4e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x2
      t5 = x2 * x3
      t6 = t5 - 0.1D1
      t7 = 0.1D1 / t6
      t8 = x3 * t3 * t7
      t9 = t2 * t8
      t10 = -0.1D1 + x3
      t11 = t10 * t7
      t12 = t2 * t11
      t13 = s ** 2
      t14 = 0.1D1 / t13
      t15 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t11, x4)
      t17 = z ** 2
      t19 = 0.1D1 / t17 / z
      t21 = x4 * 0.3141592653589793D1
      t22 = Sin(t21)
      t23 = t22 ** 2
      t24 = t3 ** 2
      t25 = t23 * t24
      t26 = t6 ** 2
      t32 = log(-0.4D1 * t5 * t19 * t25 * t10 / t26)
      t33 = t32 * z
      t34 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t11, x4)
      t38 = cos(t21)
      t39 = x3 * z
      t40 = x2 * t10
      t42 = Sqrt(-t39 * t40)
      t46 = 0.1D1 / (-z - t5 + 0.2D1 * t38 * t42)
      t49 = lh * t14
      t55 = 0.1D1 / x3
      t57 = 0.1D1 / x2
      t60 = t14 * z
      t63 = 0.1D1 / x1
      t64 = t57 * t63
      t65 = t46 * t55 * t64
      t68 = -(-0.90D2 * t14 * (z * t15 - t33 * t34) * t46 + 0.180D3 * t4
     #9 * z * t34 * t46) * t55 * t57 / 0.5760D4 + t60 * t34 * t65 / 0.32
     #D2
      t69 = FJET(XB1, XB2, s, t9, 0.0D0, t12, 0.0D0, 0.0D0, t68)
      t71 = t2 * x1
      t72 = -0.1D1 + x1
      t73 = t2 * t72
      t74 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t75 = x1 ** 2
      t76 = x3 * t75
      t77 = t76 * t23
      t79 = x1 * z
      t80 = -z - x1 + t79
      t81 = 0.1D1 / t80
      t82 = 0.1D1 / t17 * t81
      t83 = t72 ** 2
      t84 = t82 * t83
      t87 = log(-0.4D1 * t77 * t84)
      t88 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t94 = 0.180D3 * t49 * t88
      t101 = t55 * t57 * t63
      t104 = x2 * t75
      t105 = t104 * t23
      t108 = log(-0.4D1 * t105 * t84)
      t117 = t75 * t23
      t120 = log(-0.4D1 * t117 * t84)
      t125 = t120 ** 2
      t132 = lh ** 2
      t133 = 0.180D3 * t132
      t134 = 0.3141592653589793D1 ** 2
      t135 = 0.30D2 * t134
      t136 = t133 - t135
      t137 = t136 * t14
      t142 = -(0.90D2 * t14 * (-t74 + t87 * t88) + t94) * t55 * t63 / 0.
     #2880D4 + t14 * t88 * t101 / 0.32D2 - (0.90D2 * t14 * (-t74 + t108 
     #* t88) + t94) * t57 * t63 / 0.2880D4 - (-0.180D3 * t49 * (-t74 + t
     #120 * t88) + 0.90D2 * t14 * (-t125 * t88 / 0.2D1 + t120 * t74) - t
     #137 * t88) * t63 / 0.2880D4
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t71, -t73, 0.0D0, t142)
      t145 = t10 * s
      t146 = t1 * t72
      t148 = t145 * t146 * t7
      t149 = x1 * x3
      t150 = t149 * z
      t153 = Sqrt(x3 * t80 * t40)
      t154 = t38 * t153
      t155 = 0.2D1 * t154
      t160 = t71 * t3 * (-t5 - z + t39 - x1 + t149 + t79 - t150 + t155) 
     #* t81 * t7
      t161 = t73 * t8
      t162 = x2 ** 2
      t163 = t162 * x3
      t164 = t163 * x1
      t166 = t5 * z
      t168 = t5 * x1
      t172 = t163 * t79
      t175 = -t39 - t149 + t5 - t155 - x2 - t164 - t163 * z + t150 + 0.2
     #D1 * t166 + 0.2D1 * t168 + 0.2D1 * t154 * x2 + t172 - 0.2D1 * t5 *
     # t79
      t178 = t71 * t175 * t81 * t7
      t179 = t1 ** 2
      t184 = s * t179 * x2 * x1 * t72 * t81
      t185 = t14 * t80
      t186 = x2 * x1
      t187 = t186 * z
      t188 = z + x1 - t79 - t186 + t187
      t189 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, x2, t11, x4)
      t203 = 0.2D1 * t154 * z + 0.2D1 * t154 * x1 - t75 - 0.2D1 * t79 - 
     #t76 * x2 + t164 + t187 - t166 - t168 + 0.2D1 * t154 * t187 - t186 
     #* t17 - 0.2D1 * t104 * z
      t221 = t104 * t17 - 0.2D1 * t154 * t186 - 0.2D1 * t154 * t79 + 0.2
     #D1 * x1 * t17 + 0.2D1 * t75 * z - t75 * t17 + t104 - t17 + x3 * t1
     #7 * t186 + 0.2D1 * t76 * x2 * z - t76 * t17 * x2 - t172
      t223 = 0.1D1 / (t203 + t221)
      t225 = t223 * t55 * t64
      t227 = t185 * t188 * t189 * t225 / 0.32D2
      t228 = FJET(XB1, XB2, s, -t148, t160, -t161, -t178, t184, t227)
      t230 = t80 * t188
      t233 = t189 * t223 * t101
      t236 = FJET(XB1, XB2, s, t160, -t148, -t178, -t161, t184, t227)
      t242 = t2 * t72 * x3
      t243 = t2 * t149
      t244 = t145 * t146
      t245 = t1 * x1
      t246 = t145 * t245
      t247 = -t10
      t248 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t247, x4)
      t253 = log(0.4D1 * t77 * t82 * t83 * t10)
      t254 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t247, x4)
      t268 = -(0.90D2 * t14 * (t248 - t253 * t254) - 0.180D3 * t49 * t25
     #4) * t55 * t63 / 0.2880D4 - t14 * t254 * t101 / 0.32D2
      t269 = FJET(XB1, XB2, s, -t242, t243, t244, -t246, 0.0D0, t268)
      t271 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t272 = t19 * t23
      t275 = log(0.4D1 * t76 * t272)
      t276 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t287 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t288 = t276 - t287
      t292 = t272 * t24
      t295 = log(0.4D1 * t104 * t292)
      t297 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t300 = log(0.4D1 * t104 * t272)
      t306 = 0.180D3 * t49 * t288
      t313 = log(0.4D1 * t117 * t19)
      t319 = t313 ** 2
      t325 = t137 * t276
      t332 = log(0.4D1 * x3 * t19 * t23)
      t338 = t332 ** 2
      t349 = log(0.4D1 * t5 * t292)
      t353 = log(0.4D1 * t5 * t272)
      t362 = x2 * t19
      t365 = log(0.4D1 * t362 * t25)
      t369 = log(0.4D1 * t362 * t23)
      t375 = t369 ** 2
      t379 = t365 ** 2
      t390 = log(0.4D1 * t272)
      t393 = t390 ** 2
      t396 = (0.180D3 * t390 * lh + t133 - t135 + 0.45D2 * t393) * t14
      t409 = (-t390 * t136 - 0.90D2 * t393 * lh - 0.2884936567583026D3 -
     # 0.120D3 * t132 * lh + 0.60D2 * lh * t134 - 0.15D2 * t393 * t390) 
     #* t14
      t412 = -(0.90D2 * t14 * (t271 - t275 * t276) - 0.180D3 * t49 * t27
     #6) * t55 * t63 / 0.2880D4 - t14 * t288 * t101 / 0.32D2 - (0.90D2 *
     # t14 * (t295 * t287 + t271 - t297 - t300 * t276) - t306) * t57 * t
     #63 / 0.2880D4 - (-0.180D3 * t49 * (t271 - t313 * t276) + 0.90D2 * 
     #t14 * (-t313 * t271 + t319 * t276 / 0.2D1) + t325) * t63 / 0.2880D
     #4 - (-0.180D3 * t49 * (t271 - t332 * t276) + 0.90D2 * t14 * (-t332
     # * t271 + t338 * t276 / 0.2D1) + t325) * t55 / 0.5760D4 - (0.90D2 
     #* t14 * (t349 * t287 - t297 + t271 - t353 * t276) - t306) * t55 * 
     #t57 / 0.5760D4 - (-0.180D3 * t49 * (t365 * t287 - t297 + t271 - t3
     #69 * t276) + 0.90D2 * t14 * (-t369 * t271 + t375 * t276 / 0.2D1 + 
     #t365 * t297 - t379 * t287 / 0.2D1) + t137 * t288) * t57 / 0.5760D4
     # - t396 * t271 / 0.5760D4 - t409 * t276 / 0.5760D4
      t413 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t412)
      t415 = FJET(XB1, XB2, s, 0.0D0, t9, 0.0D0, t12, 0.0D0, t68)
      t417 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, x2, t11, x4)
      t421 = t185 * t188 * t417 * t225 / 0.32D2
      t422 = FJET(XB1, XB2, s, -t178, -t161, t160, -t148, t184, t421)
      t426 = t417 * t223 * t101
      t429 = t2 * t10
      t430 = t2 * x3
      t431 = t272 * t10
      t434 = log(-0.4D1 * t5 * t431)
      t435 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t247, x4)
      t437 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t247, x4)
      t442 = 0.180D3 * t49 * t435
      t449 = log(-0.4D1 * t76 * t431)
      t464 = log(-0.4D1 * t272 * x3 * t10)
      t469 = t464 ** 2
      t480 = -(0.90D2 * t14 * (t434 * t435 - t437) + t442) * t55 * t57 /
     # 0.5760D4 - (0.90D2 * t14 * (t449 * t435 - t437) + t442) * t55 * t
     #63 / 0.2880D4 + t14 * t435 * t101 / 0.32D2 - (-0.180D3 * t49 * (t4
     #64 * t435 - t437) + 0.90D2 * t14 * (-t469 * t435 / 0.2D1 + t464 * 
     #t437) - t137 * t435) * t55 / 0.5760D4
      t481 = FJET(XB1, XB2, s, -t429, 0.0D0, t430, 0.0D0, 0.0D0, t480)
      t483 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t485 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t490 = 0.180D3 * t49 * t483
      t520 = -(0.90D2 * t14 * (t87 * t483 - t485) + t490) * t55 * t63 / 
     #0.2880D4 + t14 * t483 * t101 / 0.32D2 - (0.90D2 * t14 * (-t485 + t
     #108 * t483) + t490) * t57 * t63 / 0.2880D4 - (-0.180D3 * t49 * (-t
     #485 + t120 * t483) + 0.90D2 * t14 * (t120 * t485 - t125 * t483 / 0
     #.2D1) - t137 * t483) * t63 / 0.2880D4
      t521 = FJET(XB1, XB2, s, -t73, t71, 0.0D0, 0.0D0, 0.0D0, t520)
      t523 = FJET(XB1, XB2, s, -t161, -t178, -t148, t160, t184, t421)
      t528 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t11, x4)
      t532 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t11, x4)
      t547 = t60 * t528 * t65 / 0.32D2 - (-0.90D2 * t14 * (z * t532 - t3
     #3 * t528) * t46 + 0.180D3 * t49 * z * t528 * t46) * t55 * t57 / 0.
     #5760D4
      t548 = FJET(XB1, XB2, s, 0.0D0, t12, 0.0D0, t9, 0.0D0, t547)
      t550 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t412)
      t552 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t247, x4)
      t554 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t247, x4)
      t559 = 0.180D3 * t49 * t552
      t589 = -(0.90D2 * t14 * (t449 * t552 - t554) + t559) * t55 * t63 /
     # 0.2880D4 + t14 * t552 * t101 / 0.32D2 - (-0.180D3 * t49 * (-t554 
     #+ t464 * t552) + 0.90D2 * t14 * (-t469 * t552 / 0.2D1 + t464 * t55
     #4) - t137 * t552) * t55 / 0.5760D4 - (0.90D2 * t14 * (-t554 + t434
     # * t552) + t559) * t55 * t57 / 0.5760D4
      t590 = FJET(XB1, XB2, s, 0.0D0, t430, 0.0D0, -t429, 0.0D0, t589)
      t592 = t69 * t68 + t143 * t142 + t228 * t14 * t230 * t233 / 0.32D2
     # + t236 * t14 * t230 * t233 / 0.32D2 + t269 * t268 + t413 * t412 +
     # t415 * t68 + t422 * t14 * t230 * t426 / 0.32D2 + t481 * t480 + t5
     #21 * t520 + t523 * t14 * t230 * t426 / 0.32D2 + t548 * t547 + t550
     # * t412 + t590 * t589
      t593 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t594 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t605 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t606 = t594 - t605
      t610 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t617 = 0.180D3 * t49 * t606
      t632 = t137 * t594
      t680 = -(0.90D2 * t14 * (t593 - t275 * t594) - 0.180D3 * t49 * t59
     #4) * t55 * t63 / 0.2880D4 - t14 * t606 * t101 / 0.32D2 - (0.90D2 *
     # t14 * (t593 - t610 + t295 * t605 - t300 * t594) - t617) * t57 * t
     #63 / 0.2880D4 - (-0.180D3 * t49 * (t593 - t313 * t594) + 0.90D2 * 
     #t14 * (-t313 * t593 + t319 * t594 / 0.2D1) + t632) * t63 / 0.2880D
     #4 - t396 * t593 / 0.5760D4 - t409 * t594 / 0.5760D4 - (-0.180D3 * 
     #t49 * (-t332 * t594 + t593) + 0.90D2 * t14 * (-t332 * t593 + t338 
     #* t594 / 0.2D1) + t632) * t55 / 0.5760D4 - (0.90D2 * t14 * (-t353 
     #* t594 - t610 + t593 + t349 * t605) - t617) * t55 * t57 / 0.5760D4
     # - (-0.180D3 * t49 * (-t610 + t365 * t605 + t593 - t369 * t594) + 
     #0.90D2 * t14 * (-t369 * t593 + t375 * t594 / 0.2D1 + t365 * t610 -
     # t379 * t605 / 0.2D1) + t137 * t606) * t57 / 0.5760D4
      t681 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t680)
      t683 = FJET(XB1, XB2, s, 0.0D0, -t429, 0.0D0, t430, 0.0D0, t480)
      t685 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t680)
      t688 = t3 * s * t245
      t690 = t2 * t186 * t81
      t691 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t695 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t700 = log(-0.4D1 * t105 * t82 * t83 * t24)
      t711 = -t14 * t691 * t101 / 0.32D2 - (0.90D2 * t14 * (t695 - t700 
     #* t691) - 0.180D3 * t49 * t691) * t57 * t63 / 0.2880D4
      t712 = FJET(XB1, XB2, s, -t73, -t688, 0.0D0, -t690, t184, t711)
      t714 = FJET(XB1, XB2, s, t71, -t73, 0.0D0, 0.0D0, 0.0D0, t520)
      t716 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t721 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t731 = -t14 * t716 * t101 / 0.32D2 - (0.90D2 * t14 * (-t700 * t716
     # + t721) - 0.180D3 * t49 * t716) * t57 * t63 / 0.2880D4
      t732 = FJET(XB1, XB2, s, -t690, 0.0D0, -t688, -t73, t184, t731)
      t734 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t73, t71, 0.0D0, t142)
      t736 = FJET(XB1, XB2, s, 0.0D0, -t690, -t73, -t688, t184, t731)
      t738 = FJET(XB1, XB2, s, t12, 0.0D0, t9, 0.0D0, 0.0D0, t547)
      t740 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t247, x4)
      t741 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t247, x4)
      t755 = -(0.90D2 * t14 * (t740 - t253 * t741) - 0.180D3 * t49 * t74
     #1) * t55 * t63 / 0.2880D4 - t14 * t741 * t101 / 0.32D2
      t756 = FJET(XB1, XB2, s, -t246, t244, t243, -t242, 0.0D0, t755)
      t758 = FJET(XB1, XB2, s, t244, -t246, -t242, t243, 0.0D0, t755)
      t760 = FJET(XB1, XB2, s, -t688, -t73, -t690, 0.0D0, t184, t711)
      t762 = FJET(XB1, XB2, s, t243, -t242, -t246, t244, 0.0D0, t268)
      t764 = FJET(XB1, XB2, s, t430, 0.0D0, -t429, 0.0D0, 0.0D0, t589)
      t766 = t681 * t680 + t683 * t480 + t685 * t680 + t712 * t711 + t71
     #4 * t520 + t732 * t731 + t734 * t142 + t736 * t731 + t738 * t547 +
     # t756 * t755 + t758 * t755 + t760 * t711 + t762 * t268 + t764 * t5
     #89
      bbggh5n4e0 = t592 + t766

      end function



      doubleprecision function bbggh5n4em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t22 = lh * t4
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t35 = t17 - t34
      t36 = t4 * t35
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t41 = t30 * t37
      t44 = x2 * t13
      t45 = -0.1D1 + x2
      t46 = t45 ** 2
      t50 = log(0.4D1 * t44 * t9 * t46)
      t52 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t55 = log(0.4D1 * t44 * t9)
      t66 = t13 * t9
      t68 = log(0.4D1 * t66)
      t71 = (-0.180D3 * lh - 0.90D2 * t68) * t4
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t80 = t68 ** 2
      t83 = (0.180D3 * t68 * lh + 0.180D3 * t76 - 0.30D2 * t78 + 0.45D2 
     #* t80) * t4
      t89 = log(0.4D1 * x3 * t13 * t9)
      t97 = -(0.90D2 * t4 * (t5 - t16 * t17) - t24) * t26 / 0.2880D4 - t
     #4 * t17 * t31 / 0.32D2 - t36 * t38 / 0.32D2 - t36 * t41 / 0.64D2 -
     # (0.90D2 * t4 * (t50 * t34 - t52 + t5 - t55 * t17) - 0.180D3 * t22
     # * t35) * t37 / 0.5760D4 - t71 * t5 / 0.5760D4 - t83 * t17 / 0.576
     #0D4 - (0.90D2 * t4 * (t5 - t89 * t17) - t24) * t30 / 0.5760D4
      t98 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t97)
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t97)
      t102 = t2 * x1
      t103 = -0.1D1 + x1
      t104 = t2 * t103
      t105 = bbggh52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t109 = 0.1D1 / (-z - x1 + x1 * z)
      t111 = t103 ** 2
      t115 = log(-0.4D1 * t10 / t11 * t109 * t111)
      t116 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t126 = t4 * t116
      t131 = -(0.90D2 * t4 * (-t105 + t115 * t116) + 0.180D3 * t22 * t11
     #6) * t26 / 0.2880D4 + t126 * t31 / 0.32D2 + t126 * t38 / 0.32D2
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t102, -t104, 0.0D0, t131)
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t104, t102, 0.0D0, t131)
      t136 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t137 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t143 = 0.180D3 * t22 * t137
      t150 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t151 = t137 - t150
      t152 = t4 * t151
      t166 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t179 = -(0.90D2 * t4 * (t136 - t16 * t137) - t143) * t26 / 0.2880D
     #4 - t4 * t137 * t31 / 0.32D2 - t152 * t38 / 0.32D2 - t83 * t137 / 
     #0.5760D4 - (0.90D2 * t4 * (-t89 * t137 + t136) - t143) * t30 / 0.5
     #760D4 - t152 * t41 / 0.64D2 - (0.90D2 * t4 * (-t166 + t50 * t150 +
     # t136 - t55 * t137) - 0.180D3 * t22 * t151) * t37 / 0.5760D4 - t71
     # * t136 / 0.5760D4
      t180 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = t2 * x3
      t183 = -0.1D1 + x3
      t184 = t2 * t183
      t185 = -t183
      t186 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t185, x4)
      t190 = log(-0.4D1 * t66 * x3 * t183)
      t191 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t185, x4)
      t201 = t4 * t191
      t206 = -(0.90D2 * t4 * (-t186 + t190 * t191) + 0.180D3 * t22 * t19
     #1) * t30 / 0.5760D4 + t201 * t31 / 0.32D2 + t201 * t41 / 0.64D2
      t207 = FJET(XB1, XB2, s, 0.0D0, t182, 0.0D0, -t184, 0.0D0, t206)
      t209 = x2 * x3
      t211 = 0.1D1 / (t209 - 0.1D1)
      t212 = t183 * t211
      t213 = t2 * t212
      t216 = t2 * x3 * t45 * t211
      t217 = t4 * z
      t218 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t212, x4)
      t220 = cos(t7)
      t224 = Sqrt(-x3 * z * x2 * t183)
      t228 = 0.1D1 / (-z - t209 + 0.2D1 * t220 * t224)
      t230 = t228 * t30 * t37
      t232 = t217 * t218 * t230 / 0.64D2
      t233 = FJET(XB1, XB2, s, 0.0D0, t213, 0.0D0, t216, 0.0D0, t232)
      t237 = t218 * t228 * t41
      t240 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t212, x4)
      t243 = t217 * t240 * t230 / 0.64D2
      t244 = FJET(XB1, XB2, s, 0.0D0, t216, 0.0D0, t213, 0.0D0, t243)
      t248 = t240 * t228 * t41
      t251 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t185, x4)
      t252 = t4 * t251
      t256 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t185, x4)
      t267 = t252 * t31 / 0.32D2 - (0.90D2 * t4 * (t190 * t251 - t256) +
     # 0.180D3 * t22 * t251) * t30 / 0.5760D4 + t252 * t41 / 0.64D2
      t268 = FJET(XB1, XB2, s, 0.0D0, -t184, 0.0D0, t182, 0.0D0, t267)
      t272 = t2 * x1 * x2 * t109
      t274 = t1 * x1
      t275 = t45 * s * t274
      t276 = t1 ** 2
      t281 = s * t276 * x2 * x1 * t103 * t109
      t282 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t285 = t4 * t282 * t38 / 0.32D2
      t286 = FJET(XB1, XB2, s, 0.0D0, -t272, -t104, -t275, t281, -t285)
      t289 = t282 * t37 * t26
      t292 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t179)
      t294 = bbggh51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t295 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t305 = t4 * t295
      t310 = -(0.90D2 * t4 * (-t294 + t115 * t295) + 0.180D3 * t22 * t29
     #5) * t26 / 0.2880D4 + t305 * t31 / 0.32D2 + t305 * t38 / 0.32D2
      t311 = FJET(XB1, XB2, s, t102, -t104, 0.0D0, 0.0D0, 0.0D0, t310)
      t313 = t98 * t97 + t100 * t97 + t132 * t131 + t134 * t131 + t180 *
     # t179 + t207 * t206 + t233 * t4 * z * t237 / 0.64D2 + t244 * t4 * 
     #z * t248 / 0.64D2 + t268 * t267 - t286 * t4 * t289 / 0.32D2 + t292
     # * t179 + t311 * t310
      t314 = FJET(XB1, XB2, s, t182, 0.0D0, -t184, 0.0D0, 0.0D0, t206)
      t317 = t2 * x1 * x3
      t319 = t2 * t103 * x3
      t320 = t183 * s
      t321 = t320 * t274
      t323 = t320 * t1 * t103
      t324 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t185, x4)
      t327 = t4 * t324 * t31 / 0.32D2
      t328 = FJET(XB1, XB2, s, t317, -t319, -t321, t323, 0.0D0, -t327)
      t331 = t324 * t30 * t26
      t334 = FJET(XB1, XB2, s, t213, 0.0D0, t216, 0.0D0, 0.0D0, t232)
      t339 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t185, x4)
      t342 = t4 * t339 * t31 / 0.32D2
      t343 = FJET(XB1, XB2, s, t323, -t321, -t319, t317, 0.0D0, -t342)
      t346 = t339 * t30 * t26
      t349 = FJET(XB1, XB2, s, t216, 0.0D0, t213, 0.0D0, 0.0D0, t243)
      t354 = FJET(XB1, XB2, s, -t104, t102, 0.0D0, 0.0D0, 0.0D0, t310)
      t356 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t359 = t4 * t356 * t38 / 0.32D2
      t360 = FJET(XB1, XB2, s, -t104, -t275, 0.0D0, -t272, t281, -t359)
      t363 = t356 * t37 * t26
      t366 = FJET(XB1, XB2, s, -t184, 0.0D0, t182, 0.0D0, 0.0D0, t267)
      t368 = FJET(XB1, XB2, s, -t319, t317, t323, -t321, 0.0D0, -t327)
      t372 = FJET(XB1, XB2, s, -t275, -t104, -t272, 0.0D0, t281, -t359)
      t376 = FJET(XB1, XB2, s, -t321, t323, t317, -t319, 0.0D0, -t342)
      t380 = FJET(XB1, XB2, s, -t272, 0.0D0, -t275, -t104, t281, -t285)
      t384 = t314 * t206 - t328 * t4 * t331 / 0.32D2 + t334 * t4 * z * t
     #237 / 0.64D2 - t343 * t4 * t346 / 0.32D2 + t349 * t4 * z * t248 / 
     #0.64D2 + t354 * t310 - t360 * t4 * t363 / 0.32D2 + t366 * t267 - t
     #368 * t4 * t331 / 0.32D2 - t372 * t4 * t363 / 0.32D2 - t376 * t4 *
     # t346 / 0.32D2 - t380 * t4 * t289 / 0.32D2
      bbggh5n4em1 = t313 + t384

      end function



      doubleprecision function bbggh5n4em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t13 = 0.1D1 / x2
      t16 = bbggh52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t20 = z ** 2
      t24 = Sin(x4 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t31 = (-0.180D3 * lh - 0.90D2 * t28) * t4
      t34 = 0.1D1 / x3
      t37 = -t6 * t7 / 0.32D2 - t4 * (t5 - t10) * t13 / 0.64D2 - t4 * t1
     #6 / 0.64D2 - t31 * t5 / 0.5760D4 - t6 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t42 = t2 * x1
      t44 = t2 * (-0.1D1 + x1)
      t45 = bbggh52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t48 = t4 * t45 * t7 / 0.32D2
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t42, -t44, 0.0D0, t48)
      t51 = t45 * t7
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t44, t42, 0.0D0, t48)
      t58 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t59 = t4 * t58
      t66 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t71 = bbggh51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t74 = -t59 * t7 / 0.32D2 - t31 * t58 / 0.5760D4 - t59 * t34 / 0.64
     #D2 - t4 * (t58 - t66) * t13 / 0.64D2 - t4 * t71 / 0.64D2
      t75 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t74)
      t77 = t2 * x3
      t78 = -0.1D1 + x3
      t79 = t2 * t78
      t80 = -t78
      t81 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t80, x4)
      t84 = t4 * t81 * t34 / 0.64D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t77, 0.0D0, -t79, 0.0D0, t84)
      t87 = t81 * t34
      t90 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t80, x4)
      t93 = t4 * t90 * t34 / 0.64D2
      t94 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t77, 0.0D0, t93)
      t96 = t90 * t34
      t99 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t74)
      t101 = bbggh51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t104 = t4 * t101 * t7 / 0.32D2
      t105 = FJET(XB1, XB2, s, t42, -t44, 0.0D0, 0.0D0, 0.0D0, t104)
      t107 = t101 * t7
      t110 = FJET(XB1, XB2, s, t77, 0.0D0, -t79, 0.0D0, 0.0D0, t84)
      t114 = FJET(XB1, XB2, s, -t44, t42, 0.0D0, 0.0D0, 0.0D0, t104)
      t118 = FJET(XB1, XB2, s, -t79, 0.0D0, t77, 0.0D0, 0.0D0, t93)
      bbggh5n4em2 = t38 * t37 + t40 * t37 + t49 * t4 * t51 / 0.32D2 + t5
     #4 * t4 * t51 / 0.32D2 + t75 * t74 + t85 * t4 * t87 / 0.64D2 + t94 
     #* t4 * t96 / 0.64D2 + t99 * t74 + t105 * t4 * t107 / 0.32D2 + t110
     # * t4 * t87 / 0.64D2 + t114 * t4 * t107 / 0.32D2 + t118 * t4 * t96
     # / 0.64D2

      end function



      doubleprecision function bbggh5n4em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t14 = bbggh51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t16 = t4 * t14 / 0.64D2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t16)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t16)
      bbggh5n4em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2 - t1
     #7 * t4 * t14 / 0.64D2 - t20 * t4 * t14 / 0.64D2

      end function



      doubleprecision function bbggh5n4em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh51J1
      doubleprecision bbggh51J2
      doubleprecision bbggh52J1
      doubleprecision bbggh52J2
      bbggh5n4em4 = 0.0D0

      end function
  
 

      doubleprecision function bbggh51J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t8 = z + x1 * t2
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + x2 * x3 + t24
      t29 = t18 * t8 + x2 * t11 - t24
      t32 = t5 * t10
      t33 = t29 ** 2
      t37 = t1 * z
      t38 = t37 * t2
      t39 = 0.1D1 / t8
      t40 = x1 * t39
      t44 = z ** 2
      t47 = 0.1D1 - x1
      t48 = t47 ** 2
      t53 = t4 * x1
      t55 = t47 * x3
      t59 = t25 ** 2
      t64 = t39 * t25
      t68 = t11 ** 2
      t77 = t2 * t47
      t81 = x3 ** 2
      t87 = -t4 * t5 * t10 * t25 * t29 - 0.2D1 * t4 * t32 * t33 - 0.4D1 
     #* t38 * t40 * t29 - 0.2D1 * t1 * t44 - 0.2D1 * t4 * t48 * t11 * x3
     # - 0.4D1 * t53 * t39 * t29 * t55 - t4 * t32 * t59 - 0.2D1 * t4 * t
     #47 * t11 * x1 * t64 - t4 * t48 * t68 - 0.2D1 * t38 * t40 * t25 - 0
     #.2D1 * t53 * t64 * t55 - 0.2D1 * t37 * t77 * t11 - t4 * t48 * t81 
     #- 0.2D1 * t37 * t77 * x3
      bbggh51J1 = -0.48D2 * wd * t87

      end function
  
   
 

      doubleprecision function bbggh51J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t8 = z + x1 * t2
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + x2 * x3 + t24
      t29 = t18 * t8 + x2 * t11 - t24
      t32 = 0.1D1 - x1
      t33 = t32 ** 2
      t38 = t4 * x1
      t39 = 0.1D1 / t8
      t41 = t32 * x3
      t47 = t39 * t25
      t50 = x3 ** 2
      t53 = t5 * t10
      t54 = t29 ** 2
      t58 = t25 ** 2
      t64 = t11 ** 2
      bbggh51J2 = -0.48D2 * wd * (t4 * t5 * t10 * t25 * t29 + 0.2D1 * t4
     # * t33 * t11 * x3 + 0.4D1 * t38 * t39 * t29 * t41 + t4 * t32 * t11
     # * x1 * t47 + t4 * t33 * t50 + 0.2D1 * t4 * t53 * t54 + t4 * t53 *
     # t58 + 0.3D1 * t38 * t47 * t41 + t4 * t33 * t64)

      end function
  
   
 

      doubleprecision function bbggh52J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t11 = t7 ** 2
      t15 = t1 * z
      t16 = t2 * t5
      t20 = z ** 2
      t23 = x1 ** 2
      t26 = z + x1 * t2
      t27 = t26 ** 2
      t28 = 0.1D1 / t27
      t29 = 0.1D1 - x2
      t34 = cos(x4 * 0.3141592653589793D1)
      t35 = x3 * t29
      t39 = Sqrt(t35 * t26 * x2 * t7)
      t41 = 0.2D1 * t34 * t39
      t42 = t7 * t29 * t26 + x2 * x3 + t41
      t46 = t35 * t26 + x2 * t7 - t41
      t52 = 0.1D1 / t26
      t53 = t52 * t42
      t57 = x3 ** 2
      t60 = t4 * x1
      t62 = t5 * x3
      t66 = t23 * t28
      t67 = t46 ** 2
      t76 = t15 * t2
      t77 = x1 * t52
      t81 = t42 ** 2
      t87 = -t4 * t6 * t7 * x3 - 0.2D1 * t4 * t6 * t11 - 0.4D1 * t15 * t
     #16 * t7 - 0.2D1 * t1 * t20 - 0.2D1 * t4 * t23 * t28 * t42 * t46 - 
     #0.4D1 * t4 * t5 * t7 * x1 * t53 - t4 * t6 * t57 - 0.2D1 * t60 * t5
     #2 * t46 * t62 - t4 * t66 * t67 - 0.2D1 * t15 * t16 * x3 - 0.2D1 * 
     #t60 * t53 * t62 - 0.2D1 * t76 * t77 * t46 - t4 * t66 * t81 - 0.2D1
     # * t76 * t77 * t42
      bbggh52J1 = -0.48D2 * wd * t87

      end function
  
   
 

      doubleprecision function bbggh52J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t11 = x1 ** 2
      t14 = z + x1 * t2
      t15 = t14 ** 2
      t16 = 0.1D1 / t15
      t17 = 0.1D1 - x2
      t22 = cos(x4 * 0.3141592653589793D1)
      t23 = x3 * t17
      t27 = Sqrt(t23 * t14 * x2 * t7)
      t29 = 0.2D1 * t22 * t27
      t30 = t7 * t17 * t14 + x2 * x3 + t29
      t34 = t23 * t14 + x2 * t7 - t29
      t40 = 0.1D1 / t14
      t41 = t40 * t30
      t45 = t4 * x1
      t47 = t5 * x3
      t50 = t11 * t16
      t51 = t30 ** 2
      t54 = t7 ** 2
      t58 = x3 ** 2
      t64 = t34 ** 2
      bbggh52J2 = -0.48D2 * wd * (t4 * t6 * t7 * x3 + 0.2D1 * t4 * t11 *
     # t16 * t30 * t34 + 0.4D1 * t4 * t5 * t7 * x1 * t41 + t45 * t40 * t
     #34 * t47 + t4 * t50 * t51 + 0.2D1 * t4 * t6 * t54 + t4 * t6 * t58 
     #+ 0.3D1 * t45 * t41 * t47 + t4 * t50 * t64)

      end function
  
 