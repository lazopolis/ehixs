  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t10 = 0.3141592653589793D1 * lh
      t11 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t12 = t5 * t11
      t15 = lh ** 2
      t17 = 0.3141592653589793D1 ** 2
      t19 = 0.180D3 * t15 - 0.30D2 * t17
      t20 = 0.3141592653589793D1 * t19
      t21 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t22 = t5 * t21
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
      t44 = 0.1D1 / (-x3 - z + 0.2D1 * t37 * t39 * z)
      t48 = log(0.4D1 * t28 * t30)
      t49 = 0.1D1 / z
      t53 = t48 ** 2
      t56 = t36 ** 2
      t65 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t72 = 0.60D2 * lh * t17 - 0.2884936567583026D3 - 0.120D3 * t15 * l
     #h
      t73 = 0.3141592653589793D1 * t72
      t92 = 0.1D1 / x3
      t95 = 0.3141592653589793D1 * t49
      t96 = rrgg2qqbarh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t100 = t30 * t27
      t102 = log(0.4D1 * t100)
      t103 = t102 ** 2
      t104 = t103 * 0.3141592653589793D1
      t105 = t49 * lh
      t110 = t103 * t102 * 0.3141592653589793D1
      t113 = t102 * 0.3141592653589793D1
      t114 = t49 * t19
      t144 = t17 ** 2
      t145 = t15 ** 2
      t151 = t103 ** 2
      t159 = t49 * t7
      t160 = x1 ** 2
      t161 = x3 * t160
      t164 = log(0.4D1 * t161 * t100)
      t165 = t164 * t49
      t167 = t164 ** 2
      t171 = t44 * t7
      t172 = t100 * t32
      t175 = log(-0.4D1 * t161 * t172)
      t176 = t175 * t44
      t178 = t175 ** 2
      t185 = t49 * t11
      t187 = t44 * t11
      t196 = t5 * (-t49 * t21 - t44 * t21)
      t197 = t20 * t196
      t200 = 0.1D1 / x1
      t203 = t19 * t5
      t204 = t160 * t27
      t205 = t204 * t30
      t207 = log(0.4D1 * t205)
      t212 = t207 ** 2
      t223 = t72 * t5
      t225 = t95 * t223 * t21
      t226 = lh * t5
      t237 = x2 ** 2
      t238 = t237 * x3
      t239 = t238 * t205
      t241 = log(0.4D1 * t239)
      t244 = t238 * t160
      t247 = log(-0.4D1 * t244 * t172)
      t257 = 0.1D1 / x2
      t258 = t257 * t200
      t261 = t237 * t160
      t264 = log(0.4D1 * t261 * t100)
      t265 = t264 * t49
      t267 = t264 ** 2
      t279 = t5 * t49
      t288 = log(-0.4D1 * t238 * t172)
      t289 = t288 * t44
      t291 = t288 ** 2
      t297 = log(0.4D1 * t238 * t100)
      t298 = t297 * t49
      t300 = t297 ** 2
      t317 = t237 * t27
      t320 = log(0.4D1 * t317 * t30)
      t321 = t320 * t49
      t326 = t320 ** 2
      t327 = t326 * t49
      t349 = ((0.90D2 * t6 * t7 - 0.180D3 * t10 * t12 + t20 * t22) * (t3
     #6 * t44 + t48 * t49) + 0.90D2 * t6 * t21 * (t53 * t48 * t49 / 0.6D
     #1 + t56 * t36 * t44 / 0.6D1) + (t20 * t12 + 0.90D2 * t6 * t65 + t7
     #3 * t22 - 0.180D3 * t10 * t5 * t7) * (-t44 - t49) + (0.90D2 * t6 *
     # t11 - 0.180D3 * t10 * t22) * (-t56 * t44 / 0.2D1 - t53 * t49 / 0.
     #2D1)) * t92 / 0.2880D4 - t95 * t5 * t96 / 0.32D2 - (-0.90D2 * t104
     # * t105 + t95 * t72 - 0.15D2 * t110 * t49 - t113 * t114) * t5 * t1
     #1 / 0.2880D4 - (0.180D3 * t113 * t105 + 0.45D2 * t104 * t49 + t95 
     #* t19) * t5 * t7 / 0.2880D4 - (-0.180D3 * t95 * lh - 0.90D2 * t113
     # * t49) * t5 * t65 / 0.2880D4 - (0.30D2 * t110 * t105 + t104 * t11
     #4 / 0.2D1 - t113 * t49 * t72 + t95 * (0.5769873135166051D3 * lh + 
     #t144 + 0.60D2 * t145 - 0.60D2 * t15 * t17) + 0.15D2 / 0.4D1 * t151
     # * 0.3141592653589793D1 * t49) * t5 * t21 / 0.2880D4 + (0.90D2 * t
     #6 * (-t159 + t165 * t11 - t167 * t49 * t21 / 0.2D1 - t171 + t176 *
     # t11 - t178 * t44 * t21 / 0.2D1) - 0.180D3 * t10 * t5 * (-t185 + t
     #165 * t21 - t187 + t176 * t21) + t197) * t92 * t200 / 0.1440D4 + (
     #t95 * t203 * (-t11 + t207 * t21) + 0.90D2 * t95 * t5 * (-t212 * t1
     #1 / 0.2D1 - t65 + t212 * t207 * t21 / 0.6D1 + t207 * t7) - t225 - 
     #0.180D3 * t95 * t226 * (-t7 + t207 * t11 - t212 * t21 / 0.2D1)) * 
     #t200 / 0.1440D4 + (0.90D2 * t6 * (-t185 + t241 * t49 * t21 + t247 
     #* t44 * t21 - t187) - 0.180D3 * t10 * t196) * t92 * t258 / 0.720D3
     # + (0.90D2 * t6 * (-t159 + t265 * t11 - t267 * t49 * t21 / 0.2D1) 
     #- 0.180D3 * t10 * t5 * (-t185 + t265 * t21) - t20 * t279 * t21) * 
     #t257 * t200 / 0.720D3 + (0.90D2 * t6 * (-t171 + t289 * t11 - t291 
     #* t44 * t21 / 0.2D1 - t159 + t298 * t11 - t300 * t49 * t21 / 0.2D1
     #) - 0.180D3 * t10 * t5 * (-t187 + t289 * t21 - t185 + t298 * t21) 
     #+ t197) * t92 * t257 / 0.1440D4 - (t20 * t5 * (t185 - t321 * t21) 
     #+ 0.90D2 * t6 * (t327 * t11 / 0.2D1 + t49 * t65 - t326 * t320 * t4
     #9 * t21 / 0.6D1 - t321 * t7) + t225 - 0.180D3 * t10 * t5 * (t159 -
     # t321 * t11 + t327 * t21 / 0.2D1)) * t257 / 0.1440D4
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t349)
      t352 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t349)
      t354 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t349)
      t356 = t2 * x1
      t357 = -0.1D1 + x1
      t358 = x1 * z
      t359 = 0.1D1 - x1 + t358
      t360 = 0.1D1 / t359
      t362 = t2 * t357 * t360
      t363 = t1 ** 2
      t364 = s * t363
      t366 = x1 * t357 * t360
      t367 = t364 * t366
      t368 = -t357
      t369 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, t368, 0.10D1, 0
     #.10D1, x4)
      t370 = t49 * t369
      t371 = t161 * t27
      t372 = t30 * t360
      t373 = t357 ** 2
      t374 = t372 * t373
      t377 = log(0.4D1 * t371 * t374)
      t378 = t377 * t49
      t379 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, t368, 0.10D1, 0
     #.10D1, x4)
      t381 = t377 ** 2
      t383 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t368, 0.10D1, 0
     #.10D1, x4)
      t388 = t372 * t373 * t32
      t391 = log(-0.4D1 * t371 * t388)
      t392 = t391 * t359
      t394 = t391 ** 2
      t399 = x3 * t359
      t401 = Sqrt(-t399 * t31)
      t405 = x3 * x1
      t406 = t405 * z
      t407 = 0.3D1 * t406
      t408 = x1 * t29
      t409 = x3 * t29
      t410 = t409 * x1
      t412 = 0.2D1 * t161 * z
      t413 = t161 * t29
      t414 = 0.2D1 * t405
      t415 = -z + 0.2D1 * t37 * t401 * z + t358 - t407 - t408 + t410 + t
     #412 - t413 - x3 + t414 - t161
      t416 = 0.1D1 / t415
      t421 = t49 * t379
      t423 = t359 * t379
      t435 = t5 * (t49 * t383 + t359 * t383 * t416)
      t443 = log(0.4D1 * t204 * t374)
      t448 = t443 ** 2
      t451 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, t368, 0.10D1, 0
     #.10D1, x4)
      t472 = t360 * t373
      t476 = log(0.4D1 * t244 * t100 * t472)
      t479 = t238 * t204
      t482 = log(-0.4D1 * t479 * t388)
      t496 = t261 * t27
      t499 = log(0.4D1 * t496 * t374)
      t500 = t499 * t49
      t502 = t499 ** 2
      t520 = (0.90D2 * t6 * (t370 - t378 * t379 + t381 * t49 * t383 / 0.
     #2D1 + (t359 * t369 - t392 * t379 + t394 * t359 * t383 / 0.2D1) * t
     #416) - 0.180D3 * t10 * t5 * (t421 - t378 * t383 + (t423 - t392 * t
     #383) * t416) + t20 * t435) * t92 * t200 / 0.1440D4 + (t95 * t203 *
     # (t379 - t443 * t383) + 0.90D2 * t95 * t5 * (t448 * t379 / 0.2D1 +
     # t451 - t448 * t443 * t383 / 0.6D1 - t443 * t369) + t95 * t223 * t
     #383 - 0.180D3 * t95 * t226 * (t369 - t443 * t379 + t448 * t383 / 0
     #.2D1)) * t200 / 0.1440D4 + (0.90D2 * t6 * (t421 - t476 * t49 * t38
     #3 + (t423 - t482 * t359 * t383) * t416) - 0.180D3 * t10 * t435) * 
     #t92 * t258 / 0.720D3 + (0.90D2 * t6 * (t370 - t500 * t379 + t502 *
     # t49 * t383 / 0.2D1) - 0.180D3 * t10 * t5 * (t421 - t500 * t383) +
     # t20 * t279 * t383) * t257 * t200 / 0.720D3
      t521 = FJET(XB1, XB2, s, 0.0D0, t356, -t362, 0.0D0, -t367, t520)
      t524 = x2 * t1 * s
      t525 = -0.1D1 + x2
      t527 = t525 * t1 * s
      t528 = -t525
      t529 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, 0
     #.10D1, x4)
      t530 = t100 * t525
      t533 = log(-0.4D1 * t244 * t530)
      t534 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, 0
     #.10D1, x4)
      t537 = x2 * z
      t539 = 0.1D1 / (t537 - z - x2)
      t544 = t5 * t534 * t539
      t551 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, 0
     #.10D1, x4)
      t554 = log(-0.4D1 * t261 * t530)
      t556 = t554 ** 2
      t569 = t20 * t544
      t576 = log(-0.4D1 * t238 * t530)
      t578 = t576 ** 2
      t595 = t30 * t525
      t598 = log(-0.4D1 * t317 * t595)
      t604 = t598 ** 2
      t607 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, 0
     #.10D1, x4)
      t628 = (-0.90D2 * t6 * (t529 - t533 * t534) * t539 + 0.180D3 * t10
     # * t544) * t92 * t258 / 0.720D3 + (-0.90D2 * t6 * (t551 - t554 * t
     #529 + t556 * t534 / 0.2D1) * t539 + 0.180D3 * t10 * t5 * (t529 - t
     #554 * t534) * t539 - t569) * t257 * t200 / 0.720D3 + (-0.90D2 * t6
     # * (t551 - t576 * t529 + t578 * t534 / 0.2D1) * t539 + 0.180D3 * t
     #10 * t5 * (t529 - t576 * t534) * t539 - t569) * t92 * t257 / 0.144
     #0D4 - (t20 * t5 * (t529 - t598 * t534) * t539 + 0.90D2 * t6 * (t60
     #4 * t529 / 0.2D1 + t607 - t604 * t598 * t534 / 0.6D1 - t598 * t551
     #) * t539 + t73 * t544 - 0.180D3 * t10 * t5 * (t551 - t598 * t529 +
     # t604 * t534 / 0.2D1) * t539) * t257 / 0.1440D4
      t629 = FJET(XB1, XB2, s, 0.0D0, t524, 0.0D0, -t527, 0.0D0, t628)
      t631 = x2 * x3
      t634 = Sqrt(x3 * t525 * t31)
      t635 = t37 * t634
      t637 = 0.2D1 * t635 * x2
      t639 = 0.1D1 - x3 + t631
      t640 = 0.1D1 / t639
      t642 = t2 * (0.1D1 - x3 - x2 + t631 + t238 + t637) * t640
      t647 = t2 * x2 * (-0.1D1 + t631 + 0.2D1 * t635) * t640
      t648 = t631 * z
      t649 = t238 * z
      t655 = 0.1D1 / (-t537 - t648 + t649 + x3 - t238 + z + x2 - t637 - 
     #0.2D1 * t635 * z + 0.2D1 * t635 * t537)
      t656 = t31 * t640
      t657 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, -
     #t656, x4)
      t658 = t655 * t657
      t659 = t639 ** 2
      t660 = 0.1D1 / t659
      t662 = t595 * t31 * t660
      t665 = log(0.4D1 * t479 * t662)
      t667 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, -
     #t656, x4)
      t673 = t5 * t655 * t667
      t679 = (0.90D2 * t6 * (-t658 + t665 * t655 * t667) + 0.180D3 * t10
     # * t673) * t92 * t258 / 0.720D3
      t680 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t528, -
     #t656, x4)
      t685 = log(0.4D1 * t238 * t27 * t662)
      t686 = t685 * t655
      t688 = t685 ** 2
      t692 = -t655 * t680 + t686 * t657 - t688 * t655 * t667 / 0.2D1
      t696 = -t658 + t686 * t667
      t700 = t20 * t673
      t705 = t679 + (0.90D2 * t6 * t692 - 0.180D3 * t10 * t5 * t696 - t7
     #00) * t92 * t257 / 0.1440D4
      t706 = FJET(XB1, XB2, s, 0.0D0, t642, 0.0D0, -t647, 0.0D0, t705)
      t709 = t1 * t357
      t711 = t525 * s * t709 * t360
      t713 = x2 * s * t709
      t715 = t364 * t525 * t366
      t716 = x2 * x1
      t717 = t716 * z
      t719 = 0.1D1 / (-t716 + z - t537 + t717 + x2)
      t720 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, t368, t528, 0.1
     #0D1, x4)
      t721 = t719 * t720
      t723 = t372 * t373 * t525
      t726 = log(-0.4D1 * t479 * t723)
      t728 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t368, t528, 0.1
     #0D1, x4)
      t734 = t5 * t719 * t728
      t739 = (0.90D2 * t6 * (-t721 + t726 * t719 * t728) + 0.180D3 * t10
     # * t734) * t92 * t258
      t740 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, t368, t528, 0.1
     #0D1, x4)
      t744 = log(-0.4D1 * t496 * t723)
      t745 = t744 * t719
      t747 = t744 ** 2
      t751 = -t719 * t740 + t745 * t720 - t747 * t719 * t728 / 0.2D1
      t755 = -t721 + t745 * t728
      t759 = t20 * t734
      t764 = t739 / 0.720D3 + (0.90D2 * t6 * t751 - 0.180D3 * t10 * t5 *
     # t755 - t759) * t257 * t200 / 0.720D3
      t765 = FJET(XB1, XB2, s, 0.0D0, t711, t356, -t713, t715, t764)
      t767 = FJET(XB1, XB2, s, 0.0D0, -t527, 0.0D0, t524, 0.0D0, t628)
      t769 = FJET(XB1, XB2, s, 0.0D0, -t362, t356, 0.0D0, -t367, t520)
      t771 = FJET(XB1, XB2, s, 0.0D0, -t647, 0.0D0, t642, 0.0D0, t705)
      t773 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t349)
      t775 = FJET(XB1, XB2, s, t356, 0.0D0, 0.0D0, -t362, -t367, t520)
      t777 = t350 * t349 + t352 * t349 + t354 * t349 + t521 * t520 + t62
     #9 * t628 + t706 * t705 + t765 * t764 + t767 * t628 + t769 * t520 +
     # t771 * t705 + t773 * t349 + t775 * t520
      t778 = FJET(XB1, XB2, s, t356, -t713, 0.0D0, t711, t715, t764)
      t780 = FJET(XB1, XB2, s, t524, 0.0D0, -t527, 0.0D0, 0.0D0, t628)
      t782 = FJET(XB1, XB2, s, t642, 0.0D0, -t647, 0.0D0, 0.0D0, t705)
      t784 = FJET(XB1, XB2, s, t711, 0.0D0, -t713, t356, t715, t764)
      t787 = t356 * t631 * t640
      t788 = t2 * t357
      t789 = t525 * t31
      t791 = Sqrt(t399 * t789)
      t792 = t37 * t791
      t794 = 0.2D1 * t792 * x2
      t795 = t238 * t358
      t796 = t238 * x1
      t800 = t788 * (t794 + t795 - t796 + 0.1D1 - x3 + t238 - x2 + t631)
     # * t360 * t640
      t804 = t31 * s * t1 * x1 * t640
      t810 = t788 * x2 * (-0.1D1 + t631 + x1 - t405 - t358 + t406 + 0.2D
     #1 * t792) * t360 * t640
      t825 = t537 + t238 + 0.2D1 * t792 * t717 + 0.2D1 * t716 - x2 + t79
     #5 - 0.2D1 * t792 * t716 - 0.2D1 * t792 * t537 + 0.2D1 * t631 * t35
     #8 - t409 * t716 - 0.2D1 * t161 * t537 + t161 * t29 * x2 + t358 - t
     #408 + t414 - t161 - z
      t826 = x2 * t160
      t836 = -t826 - x3 + t794 - t796 + 0.2D1 * t792 * z - t631 * x1 + t
     #161 * x2 + t716 * t29 + 0.2D1 * t826 * z - t826 * t29 - t407 + t41
     #0 + t412 - t413 - 0.3D1 * t717 + t648 - t649
      t838 = 0.1D1 / (t825 + t836)
      t839 = t359 * t838
      t840 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, t368, t528, -t6
     #56, x4)
      t846 = log(0.4D1 * t239 * t472 * t789 * t660)
      t848 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t368, t528, -t6
     #56, x4)
      t851 = -t839 * t840 + t846 * t359 * t838 * t848
      t857 = 0.180D3 * t10 * t5 * t839 * t848
      t858 = 0.90D2 * t6 * t851 + t857
      t861 = t858 * t92 * t258 / 0.720D3
      t862 = FJET(XB1, XB2, s, t787, -t800, -t804, t810, t715, t861)
      t865 = t92 * t257 * t200
      t868 = FJET(XB1, XB2, s, t810, -t804, -t800, t787, t715, t861)
      t872 = FJET(XB1, XB2, s, -t527, 0.0D0, t524, 0.0D0, 0.0D0, t628)
      t874 = FJET(XB1, XB2, s, -t362, 0.0D0, 0.0D0, t356, -t367, t520)
      t887 = t739 / 0.720D3 + (0.90D2 * t6 * t751 - 0.180D3 * t10 * t5 *
     # t755 - t759) * t257 * t200 / 0.720D3
      t888 = FJET(XB1, XB2, s, -t713, t356, t711, 0.0D0, t715, t887)
      t901 = t679 + (0.90D2 * t6 * t692 - 0.180D3 * t10 * t5 * t696 - t7
     #00) * t92 * t257 / 0.1440D4
      t902 = FJET(XB1, XB2, s, -t647, 0.0D0, t642, 0.0D0, 0.0D0, t901)
      t907 = 0.90D2 * t6 * t851 + t857
      t910 = t907 * t92 * t258 / 0.720D3
      t911 = FJET(XB1, XB2, s, -t804, t810, t787, -t800, t715, t910)
      t915 = FJET(XB1, XB2, s, -t800, t787, t810, -t804, t715, t910)
      t919 = t778 * t764 + t780 * t628 + t782 * t705 + t784 * t764 + t86
     #2 * t858 * t865 / 0.720D3 + t868 * t858 * t865 / 0.720D3 + t872 * 
     #t628 + t874 * t520 + t888 * t887 + t902 * t901 + t911 * t907 * t86
     #5 / 0.720D3 + t915 * t907 * t865 / 0.720D3
      rrgg2qqbarht5s1e1 = t777 + t919

      end function



      doubleprecision function rrgg2qqbarht5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
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
      t28 = 0.1D1 / (-x3 - z + 0.2D1 * t21 * t23 * z)
      t32 = log(0.4D1 * t11 * t13)
      t33 = t32 ** 2
      t34 = 0.1D1 / z
      t41 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t44 = 0.3141592653589793D1 * lh
      t45 = t5 * t7
      t53 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t59 = lh ** 2
      t61 = 0.3141592653589793D1 ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t64 = 0.3141592653589793D1 * t63
      t70 = 0.1D1 / x3
      t73 = t13 * t10
      t75 = log(0.4D1 * t73)
      t76 = t75 * 0.3141592653589793D1
      t77 = t34 * lh
      t80 = t75 ** 2
      t81 = t80 * 0.3141592653589793D1
      t84 = 0.3141592653589793D1 * t34
      t90 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t120 = t28 * t41
      t121 = x2 ** 2
      t122 = t121 * x3
      t123 = t73 * t15
      t126 = log(-0.4D1 * t122 * t123)
      t129 = t34 * t41
      t132 = log(0.4D1 * t122 * t73)
      t140 = -t34 * t7 - t28 * t7
      t143 = 0.180D3 * t44 * t5 * t140
      t146 = 0.1D1 / x2
      t150 = t121 * t10
      t153 = log(0.4D1 * t150 * t13)
      t154 = t153 * t34
      t156 = t153 ** 2
      t168 = t5 * t34
      t169 = t168 * t7
      t170 = t64 * t169
      t174 = x1 ** 2
      t175 = x3 * t174
      t178 = log(0.4D1 * t175 * t73)
      t183 = log(-0.4D1 * t175 * t123)
      t191 = 0.1D1 / x1
      t196 = t70 * t146 * t191
      t199 = t121 * t174
      t202 = log(0.4D1 * t199 * t73)
      t214 = t174 * t10
      t217 = log(0.4D1 * t214 * t13)
      t219 = t217 ** 2
      t226 = lh * t5
      t235 = (0.90D2 * t6 * t7 * (-t20 * t28 / 0.2D1 - t33 * t34 / 0.2D1
     #) + (0.90D2 * t6 * t41 - 0.180D3 * t44 * t45) * (t19 * t28 + t32 *
     # t34) + (0.90D2 * t6 * t53 - 0.180D3 * t44 * t5 * t41 + t64 * t45)
     # * (-t28 - t34)) * t70 / 0.2880D4 - (0.180D3 * t76 * t77 + 0.45D2 
     #* t81 * t34 + t84 * t63) * t5 * t41 / 0.2880D4 - t84 * t5 * t90 / 
     #0.32D2 - (-0.90D2 * t81 * t77 + t84 * (0.60D2 * lh * t61 - 0.28849
     #36567583026D3 - 0.120D3 * t59 * lh) - 0.15D2 * t80 * t75 * 0.31415
     #92653589793D1 * t34 - t76 * t34 * t63) * t5 * t7 / 0.2880D4 - (-0.
     #180D3 * t84 * lh - 0.90D2 * t76 * t34) * t5 * t53 / 0.2880D4 + (0.
     #90D2 * t6 * (-t120 + t126 * t28 * t7 - t129 + t132 * t34 * t7) - t
     #143) * t70 * t146 / 0.1440D4 - (0.90D2 * t6 * (t34 * t53 - t154 * 
     #t41 + t156 * t34 * t7 / 0.2D1) - 0.180D3 * t44 * t5 * (t129 - t154
     # * t7) + t170) * t146 / 0.1440D4 + (0.90D2 * t6 * (-t129 + t178 * 
     #t34 * t7 - t120 + t183 * t28 * t7) - t143) * t70 * t191 / 0.1440D4
     # + t6 * t140 * t196 / 0.8D1 + (0.90D2 * t6 * (-t129 + t202 * t34 *
     # t7) + 0.180D3 * t44 * t169) * t146 * t191 / 0.720D3 + (0.90D2 * t
     #84 * t5 * (-t53 + t217 * t41 - t219 * t7 / 0.2D1) - 0.180D3 * t84 
     #* t226 * (-t41 + t217 * t7) - t170) * t191 / 0.1440D4
      t236 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t235)
      t238 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t235)
      t240 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t235)
      t242 = t2 * x1
      t243 = -0.1D1 + x1
      t244 = x1 * z
      t245 = 0.1D1 - x1 + t244
      t246 = 0.1D1 / t245
      t248 = t2 * t243 * t246
      t249 = t1 ** 2
      t250 = s * t249
      t252 = x1 * t243 * t246
      t253 = t250 * t252
      t254 = -t243
      t255 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, t254, 0.10D1, 0
     #.10D1, x4)
      t256 = t34 * t255
      t257 = t175 * t10
      t258 = t13 * t246
      t259 = t243 ** 2
      t260 = t258 * t259
      t263 = log(0.4D1 * t257 * t260)
      t265 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t254, 0.10D1, 0
     #.10D1, x4)
      t272 = log(-0.4D1 * t257 * t258 * t259 * t15)
      t276 = x3 * t245
      t278 = Sqrt(-t276 * t14)
      t282 = x3 * x1
      t283 = t282 * z
      t284 = 0.3D1 * t283
      t285 = x1 * t12
      t286 = x3 * t12
      t287 = t286 * x1
      t289 = 0.2D1 * t175 * z
      t290 = t175 * t12
      t291 = 0.2D1 * t282
      t292 = -z + 0.2D1 * t21 * t278 * z + t244 - t284 - t285 + t287 + t
     #289 - t290 - x3 + t291 - t175
      t293 = 0.1D1 / t292
      t301 = t34 * t265 + t245 * t265 * t293
      t312 = t199 * t10
      t315 = log(0.4D1 * t312 * t260)
      t321 = t168 * t265
      t328 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, t254, 0.10D1, 0
     #.10D1, x4)
      t331 = log(0.4D1 * t214 * t260)
      t333 = t331 ** 2
      t349 = (0.90D2 * t6 * (t256 - t263 * t34 * t265 + (t245 * t255 - t
     #272 * t245 * t265) * t293) - 0.180D3 * t44 * t5 * t301) * t70 * t1
     #91 / 0.1440D4 + t6 * t301 * t196 / 0.8D1 + (0.90D2 * t6 * (t256 - 
     #t315 * t34 * t265) - 0.180D3 * t44 * t321) * t146 * t191 / 0.720D3
     # + (0.90D2 * t84 * t5 * (t328 - t331 * t255 + t333 * t265 / 0.2D1)
     # - 0.180D3 * t84 * t226 * (t255 - t331 * t265) + t64 * t321) * t19
     #1 / 0.1440D4
      t350 = FJET(XB1, XB2, s, 0.0D0, t242, -t248, 0.0D0, -t253, t349)
      t353 = x2 * t1 * s
      t354 = -0.1D1 + x2
      t356 = t354 * t1 * s
      t357 = -t354
      t358 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t357, 0
     #.10D1, x4)
      t359 = t73 * t354
      t362 = log(-0.4D1 * t122 * t359)
      t363 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t357, 0
     #.10D1, x4)
      t366 = x2 * z
      t368 = 0.1D1 / (t366 - z - x2)
      t373 = t5 * t363 * t368
      t375 = 0.180D3 * t44 * t373
      t380 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t357, 0
     #.10D1, x4)
      t381 = t13 * t354
      t384 = log(-0.4D1 * t150 * t381)
      t386 = t384 ** 2
      t405 = t146 * t191
      t411 = log(-0.4D1 * t199 * t359)
      t421 = (-0.90D2 * t6 * (t358 - t362 * t363) * t368 + t375) * t70 *
     # t146 / 0.1440D4 - (0.90D2 * t6 * (t380 - t384 * t358 + t386 * t36
     #3 / 0.2D1) * t368 - 0.180D3 * t44 * t5 * (t358 - t384 * t363) * t3
     #68 + t64 * t373) * t146 / 0.1440D4 - t6 * t363 * t368 * t70 * t405
     # / 0.8D1 + (-0.90D2 * t6 * (t358 - t411 * t363) * t368 + t375) * t
     #146 * t191 / 0.720D3
      t422 = FJET(XB1, XB2, s, 0.0D0, t353, 0.0D0, -t356, 0.0D0, t421)
      t424 = x2 * x3
      t427 = Sqrt(x3 * t354 * t14)
      t428 = t21 * t427
      t430 = 0.2D1 * t428 * x2
      t432 = 0.1D1 - x3 + t424
      t433 = 0.1D1 / t432
      t435 = t2 * (0.1D1 - x3 - x2 + t424 + t122 + t430) * t433
      t440 = t2 * x2 * (-0.1D1 + t424 + 0.2D1 * t428) * t433
      t441 = t424 * z
      t442 = t122 * z
      t448 = 0.1D1 / (-t366 - t441 + t442 + x3 - t122 + z + x2 - t430 - 
     #0.2D1 * t428 * z + 0.2D1 * t428 * t366)
      t449 = t14 * t433
      t450 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t357, -
     #t449, x4)
      t453 = t432 ** 2
      t459 = log(0.4D1 * t122 * t10 * t381 * t14 / t453)
      t461 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t357, -
     #t449, x4)
      t463 = -t448 * t450 + t459 * t448 * t461
      t469 = 0.180D3 * t44 * t5 * t448 * t461
      t478 = t6 * t448 * t461 * t70 * t405 / 0.8D1
      t479 = (0.90D2 * t6 * t463 + t469) * t70 * t146 / 0.1440D4 - t478
      t480 = FJET(XB1, XB2, s, 0.0D0, t435, 0.0D0, -t440, 0.0D0, t479)
      t483 = t1 * t243
      t485 = t354 * s * t483 * t246
      t487 = x2 * s * t483
      t489 = t250 * t354 * t252
      t490 = x2 * x1
      t491 = t490 * z
      t493 = 0.1D1 / (-t490 + z - t366 + t491 + x2)
      t495 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t254, t357, 0.1
     #0D1, x4)
      t499 = t6 * t493 * t495 * t70 * t405 / 0.8D1
      t500 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, t254, t357, 0.1
     #0D1, x4)
      t506 = log(-0.4D1 * t312 * t258 * t259 * t354)
      t509 = -t493 * t500 + t506 * t493 * t495
      t515 = 0.180D3 * t44 * t5 * t493 * t495
      t520 = -t499 + (0.90D2 * t6 * t509 + t515) * t146 * t191 / 0.720D3
      t521 = FJET(XB1, XB2, s, 0.0D0, t485, t242, -t487, t489, t520)
      t523 = FJET(XB1, XB2, s, 0.0D0, -t356, 0.0D0, t353, 0.0D0, t421)
      t525 = FJET(XB1, XB2, s, 0.0D0, -t248, t242, 0.0D0, -t253, t349)
      t527 = FJET(XB1, XB2, s, 0.0D0, -t440, 0.0D0, t435, 0.0D0, t479)
      t529 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t235)
      t531 = FJET(XB1, XB2, s, t242, 0.0D0, 0.0D0, -t248, -t253, t349)
      t533 = t236 * t235 + t238 * t235 + t240 * t235 + t350 * t349 + t42
     #2 * t421 + t480 * t479 + t521 * t520 + t523 * t421 + t525 * t349 +
     # t527 * t479 + t529 * t235 + t531 * t349
      t534 = FJET(XB1, XB2, s, t242, -t487, 0.0D0, t485, t489, t520)
      t536 = FJET(XB1, XB2, s, t353, 0.0D0, -t356, 0.0D0, 0.0D0, t421)
      t538 = FJET(XB1, XB2, s, t435, 0.0D0, -t440, 0.0D0, 0.0D0, t479)
      t540 = FJET(XB1, XB2, s, t485, 0.0D0, -t487, t242, t489, t520)
      t543 = t242 * t424 * t433
      t544 = t2 * t243
      t547 = Sqrt(t276 * t354 * t14)
      t548 = t21 * t547
      t550 = 0.2D1 * t548 * x2
      t551 = t122 * t244
      t552 = t122 * x1
      t556 = t544 * (t550 + t551 - t552 + 0.1D1 - x3 + t122 - x2 + t424)
     # * t246 * t433
      t560 = t14 * s * t1 * x1 * t433
      t566 = t544 * x2 * (-0.1D1 + t424 + x1 - t282 - t244 + t283 + 0.2D
     #1 * t548) * t246 * t433
      t579 = x2 * t174
      t582 = -x2 - z - x3 + t551 - 0.2D1 * t548 * t490 - 0.2D1 * t548 * 
     #t366 + 0.2D1 * t424 * t244 - t286 * t490 - 0.2D1 * t175 * t366 + t
     #175 * t12 * x2 + t441 - t442 - 0.3D1 * t491 - t579 + t550 - t552 +
     # 0.2D1 * t548 * z
      t592 = -t424 * x1 + t175 * x2 + t490 * t12 + 0.2D1 * t579 * z - t5
     #79 * t12 + 0.2D1 * t548 * t491 + t244 - t285 + t291 - t175 + t366 
     #+ t122 + 0.2D1 * t490 - t284 + t287 + t289 - t290
      t594 = 0.1D1 / (t582 + t592)
      t597 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t254, t357, -t4
     #49, x4)
      t601 = t6 * t245 * t594 * t597 * t70 * t405 / 0.8D1
      t602 = FJET(XB1, XB2, s, t543, -t556, -t560, t566, t489, -t601)
      t604 = t5 * t245
      t607 = t594 * t597 * t196
      t610 = FJET(XB1, XB2, s, t566, -t560, -t556, t543, t489, -t601)
      t615 = FJET(XB1, XB2, s, -t356, 0.0D0, t353, 0.0D0, 0.0D0, t421)
      t617 = FJET(XB1, XB2, s, -t248, 0.0D0, 0.0D0, t242, -t253, t349)
      t626 = -t499 + (0.90D2 * t6 * t509 + t515) * t146 * t191 / 0.720D3
      t627 = FJET(XB1, XB2, s, -t487, t242, t485, 0.0D0, t489, t626)
      t636 = (0.90D2 * t6 * t463 + t469) * t70 * t146 / 0.1440D4 - t478
      t637 = FJET(XB1, XB2, s, -t440, 0.0D0, t435, 0.0D0, 0.0D0, t636)
      t639 = FJET(XB1, XB2, s, -t560, t566, t543, -t556, t489, -t601)
      t644 = FJET(XB1, XB2, s, -t556, t543, t566, -t560, t489, -t601)
      t649 = t534 * t520 + t536 * t421 + t538 * t479 + t540 * t520 - t60
     #2 * 0.3141592653589793D1 * t604 * t607 / 0.8D1 - t610 * 0.31415926
     #53589793D1 * t604 * t607 / 0.8D1 + t615 * t421 + t617 * t349 + t62
     #7 * t626 + t637 * t636 - t639 * 0.3141592653589793D1 * t604 * t607
     # / 0.8D1 - t644 * 0.3141592653589793D1 * t604 * t607 / 0.8D1
      rrgg2qqbarht5s1e0 = t533 + t649

      end function



      doubleprecision function rrgg2qqbarht5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
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
      t27 = 0.1D1 / (-x3 - z + 0.2D1 * t20 * t22 * z)
      t31 = log(0.4D1 * t11 * t13)
      t32 = 0.1D1 / z
      t38 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t41 = 0.3141592653589793D1 * lh
      t49 = 0.1D1 / x3
      t52 = 0.3141592653589793D1 * t32
      t53 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t61 = log(0.4D1 * t13 * t10)
      t62 = t61 * 0.3141592653589793D1
      t72 = t61 ** 2
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t89 = (-t32 * t7 - t27 * t7) * t49
      t90 = 0.1D1 / x2
      t95 = x2 ** 2
      t96 = t95 * t10
      t99 = log(0.4D1 * t96 * t13)
      t105 = t5 * t32
      t108 = 0.180D3 * t41 * t105 * t7
      t112 = t6 * t32
      t114 = 0.1D1 / x1
      t118 = x1 ** 2
      t119 = t118 * t10
      t122 = log(0.4D1 * t119 * t13)
      t134 = (0.90D2 * t6 * t7 * (t19 * t27 + t31 * t32) + (0.90D2 * t6 
     #* t38 - 0.180D3 * t41 * t5 * t7) * (-t27 - t32)) * t49 / 0.2880D4 
     #- t52 * t5 * t53 / 0.32D2 - (-0.180D3 * t52 * lh - 0.90D2 * t62 * 
     #t32) * t5 * t38 / 0.2880D4 - (0.180D3 * t62 * t32 * lh + 0.45D2 * 
     #t72 * 0.3141592653589793D1 * t32 + t52 * (0.180D3 * t76 - 0.30D2 *
     # t78)) * t5 * t7 / 0.2880D4 + t6 * t89 * t90 / 0.16D2 - (0.90D2 * 
     #t6 * (t32 * t38 - t99 * t32 * t7) - t108) * t90 / 0.1440D4 - t112 
     #* t7 * t90 * t114 / 0.8D1 + (0.90D2 * t52 * t5 * (-t38 + t122 * t7
     #) + t108) * t114 / 0.1440D4 + t6 * t89 * t114 / 0.16D2
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t134)
      t137 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t134)
      t139 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t134)
      t141 = t2 * x1
      t142 = -0.1D1 + x1
      t143 = x1 * z
      t144 = 0.1D1 - x1 + t143
      t145 = 0.1D1 / t144
      t147 = t2 * t142 * t145
      t148 = t1 ** 2
      t149 = s * t148
      t151 = x1 * t142 * t145
      t152 = t149 * t151
      t153 = -t142
      t154 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t153, 0.10D1, 0
     #.10D1, x4)
      t159 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, t153, 0.10D1, 0
     #.10D1, x4)
      t161 = t142 ** 2
      t165 = log(0.4D1 * t119 * t13 * t145 * t161)
      t181 = Sqrt(-x3 * t144 * t14)
      t185 = x3 * x1
      t191 = x3 * t118
      t196 = -z + 0.2D1 * t20 * t181 * z + t143 - 0.3D1 * t185 * z - x1 
     #* t12 + x3 * t12 * x1 + 0.2D1 * t191 * z - t191 * t12 - x3 + 0.2D1
     # * t185 - t191
      t204 = t112 * t154 * t90 * t114 / 0.8D1 + (0.90D2 * t52 * t5 * (t1
     #59 - t165 * t154) - 0.180D3 * t41 * t105 * t154) * t114 / 0.1440D4
     # + t6 * (t32 * t154 + t144 * t154 / t196) * t49 * t114 / 0.16D2
      t205 = FJET(XB1, XB2, s, 0.0D0, t141, -t147, 0.0D0, -t152, t204)
      t208 = x2 * t1 * s
      t209 = -0.1D1 + x2
      t211 = t209 * t1 * s
      t212 = -t209
      t213 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t212, 0
     #.10D1, x4)
      t214 = t6 * t213
      t215 = x2 * z
      t217 = 0.1D1 / (t215 - z - x2)
      t222 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t212, 0
     #.10D1, x4)
      t226 = log(-0.4D1 * t96 * t13 * t209)
      t243 = -t214 * t217 * t49 * t90 / 0.16D2 - (0.90D2 * t6 * (t222 - 
     #t226 * t213) * t217 - 0.180D3 * t41 * t5 * t213 * t217) * t90 / 0.
     #1440D4 - t214 * t217 * t90 * t114 / 0.8D1
      t244 = FJET(XB1, XB2, s, 0.0D0, t208, 0.0D0, -t211, 0.0D0, t243)
      t246 = x2 * x3
      t247 = t95 * x3
      t250 = Sqrt(x3 * t209 * t14)
      t251 = t20 * t250
      t253 = 0.2D1 * t251 * x2
      t256 = 0.1D1 / (0.1D1 - x3 + t246)
      t258 = t2 * (0.1D1 - x3 - x2 + t246 + t247 + t253) * t256
      t263 = t2 * x2 * (-0.1D1 + t246 + 0.2D1 * t251) * t256
      t271 = 0.1D1 / (-t215 - t246 * z + t247 * z + x3 - t247 + z + x2 -
     # t253 - 0.2D1 * t251 * z + 0.2D1 * t251 * t215)
      t274 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t212, -
     #t14 * t256, x4)
      t278 = t6 * t271 * t274 * t49 * t90 / 0.16D2
      t279 = FJET(XB1, XB2, s, 0.0D0, t258, 0.0D0, -t263, 0.0D0, -t278)
      t284 = t271 * t274 * t49 * t90
      t288 = t1 * t142
      t290 = t209 * s * t288 * t145
      t292 = x2 * s * t288
      t294 = t149 * t209 * t151
      t295 = x2 * x1
      t298 = 0.1D1 / (-t295 + z - t215 + t295 * z + x2)
      t300 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, t153, t212, 0.1
     #0D1, x4)
      t304 = t6 * t298 * t300 * t90 * t114 / 0.8D1
      t305 = FJET(XB1, XB2, s, 0.0D0, t290, t141, -t292, t294, -t304)
      t310 = t298 * t300 * t90 * t114
      t313 = FJET(XB1, XB2, s, 0.0D0, -t211, 0.0D0, t208, 0.0D0, t243)
      t315 = FJET(XB1, XB2, s, 0.0D0, -t147, t141, 0.0D0, -t152, t204)
      t317 = FJET(XB1, XB2, s, 0.0D0, -t263, 0.0D0, t258, 0.0D0, -t278)
      t322 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t134)
      t324 = FJET(XB1, XB2, s, t141, 0.0D0, 0.0D0, -t147, -t152, t204)
      t326 = FJET(XB1, XB2, s, t141, -t292, 0.0D0, t290, t294, -t304)
      t331 = FJET(XB1, XB2, s, t208, 0.0D0, -t211, 0.0D0, 0.0D0, t243)
      t333 = FJET(XB1, XB2, s, t258, 0.0D0, -t263, 0.0D0, 0.0D0, -t278)
      t338 = FJET(XB1, XB2, s, t290, 0.0D0, -t292, t141, t294, -t304)
      t343 = FJET(XB1, XB2, s, -t211, 0.0D0, t208, 0.0D0, 0.0D0, t243)
      t345 = FJET(XB1, XB2, s, -t147, 0.0D0, 0.0D0, t141, -t152, t204)
      t347 = FJET(XB1, XB2, s, -t292, t141, t290, 0.0D0, t294, -t304)
      t352 = FJET(XB1, XB2, s, -t263, 0.0D0, t258, 0.0D0, 0.0D0, -t278)
      rrgg2qqbarht5s1em1 = t135 * t134 + t137 * t134 + t139 * t134 + t20
     #5 * t204 + t244 * t243 - t279 * 0.3141592653589793D1 * t5 * t284 /
     # 0.16D2 - t305 * 0.3141592653589793D1 * t5 * t310 / 0.8D1 + t313 *
     # t243 + t315 * t204 - t317 * 0.3141592653589793D1 * t5 * t284 / 0.
     #16D2 + t322 * t134 + t324 * t204 - t326 * 0.3141592653589793D1 * t
     #5 * t310 / 0.8D1 + t331 * t243 - t333 * 0.3141592653589793D1 * t5 
     #* t284 / 0.16D2 - t338 * 0.3141592653589793D1 * t5 * t310 / 0.8D1 
     #+ t343 * t243 + t345 * t204 - t347 * 0.3141592653589793D1 * t5 * t
     #310 / 0.8D1 - t352 * 0.3141592653589793D1 * t5 * t284 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t18 = 0.1D1 / z
      t25 = t18 * t7
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t34 = 0.3141592653589793D1 * t18
      t35 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t41 = z ** 2
      t43 = Sin(t8)
      t44 = t43 ** 2
      t47 = log(0.4D1 / t41 * t44)
      t55 = t6 * t7 * (-0.1D1 / (-x3 - z + 0.2D1 * t9 * t12 * z) - t18) 
     #/ x3 / 0.32D2 - t6 * t25 * t26 / 0.16D2 - t6 * t25 * t30 / 0.16D2 
     #- t34 * t5 * t35 / 0.32D2 - (-0.180D3 * t34 * lh - 0.90D2 * t47 * 
     #0.3141592653589793D1 * t18) * t5 * t7 / 0.2880D4
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t55)
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t55)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t55)
      t62 = t2 * x1
      t63 = -0.1D1 + x1
      t66 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t68 = t2 * t63 * t66
      t69 = t1 ** 2
      t73 = s * t69 * x1 * t63 * t66
      t75 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, -t63, 0.10D1, 0.
     #10D1, x4)
      t77 = t18 * t75 * t30
      t79 = t6 * t77 / 0.16D2
      t80 = FJET(XB1, XB2, s, 0.0D0, t62, -t68, 0.0D0, -t73, t79)
      t86 = x2 * t1 * s
      t87 = -0.1D1 + x2
      t89 = t87 * t1 * s
      t91 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, -t87, 0.
     #10D1, x4)
      t96 = t91 / (x2 * z - z - x2) * t26
      t98 = t6 * t96 / 0.16D2
      t99 = FJET(XB1, XB2, s, 0.0D0, t86, 0.0D0, -t89, 0.0D0, -t98)
      t104 = FJET(XB1, XB2, s, 0.0D0, -t89, 0.0D0, t86, 0.0D0, -t98)
      t109 = FJET(XB1, XB2, s, 0.0D0, -t68, t62, 0.0D0, -t73, t79)
      t114 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t55)
      t116 = FJET(XB1, XB2, s, t62, 0.0D0, 0.0D0, -t68, -t73, t79)
      t121 = FJET(XB1, XB2, s, t86, 0.0D0, -t89, 0.0D0, 0.0D0, -t98)
      t126 = FJET(XB1, XB2, s, -t89, 0.0D0, t86, 0.0D0, 0.0D0, -t98)
      t131 = FJET(XB1, XB2, s, -t68, 0.0D0, 0.0D0, t62, -t73, t79)
      rrgg2qqbarht5s1em2 = t56 * t55 + t58 * t55 + t60 * t55 + t80 * 0.3
     #141592653589793D1 * t5 * t77 / 0.16D2 - t99 * 0.3141592653589793D1
     # * t5 * t96 / 0.16D2 - t104 * 0.3141592653589793D1 * t5 * t96 / 0.
     #16D2 + t109 * 0.3141592653589793D1 * t5 * t77 / 0.16D2 + t114 * t5
     #5 + t116 * 0.3141592653589793D1 * t5 * t77 / 0.16D2 - t121 * 0.314
     #1592653589793D1 * t5 * t96 / 0.16D2 - t126 * 0.3141592653589793D1 
     #* t5 * t96 / 0.16D2 + t131 * 0.3141592653589793D1 * t5 * t77 / 0.1
     #6D2

      end function



      doubleprecision function rrgg2qqbarht5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = 0.1D1 / z
      t8 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t11 = 0.3141592653589793D1 * t5 * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t5 * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2qqbarht5s1em3 = -t12 * 0.3141592653589793D1 * t15 / 0.32D2 - 
     #t17 * 0.3141592653589793D1 * t15 / 0.32D2 - t20 * 0.31415926535897
     #93D1 * t15 / 0.32D2 - t23 * 0.3141592653589793D1 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2qqbarht5s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht5s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / z
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgg2qqbarh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t20 = log(0.4D1 * t18)
      t21 = t20 ** 2
      t22 = t21 * t3
      t23 = 0.3141592653589793D1 * lh
      t26 = 0.3141592653589793D1 ** 2
      t29 = lh ** 2
      t32 = 0.60D2 * lh * t26 - 0.2884936567583026D3 - 0.120D3 * t29 * l
     #h
      t35 = t21 * t20 * t3
      t38 = t20 * t3
      t41 = 0.180D3 * t29 - 0.30D2 * t26
      t42 = 0.3141592653589793D1 * t41
      t46 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t56 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t65 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t72 = 0.3141592653589793D1 * t32
      t75 = t26 ** 2
      t76 = t29 ** 2
      t82 = t21 ** 2
      t88 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t91 = 0.3141592653589793D1 * t7
      t92 = x1 ** 2
      t93 = x3 * t92
      t94 = -0.1D1 + x3
      t95 = 0.1D1 / t94
      t96 = t18 * t95
      t99 = log(-0.4D1 * t93 * t96)
      t101 = t99 ** 2
      t105 = cos(t15)
      t106 = x3 * z
      t108 = Sqrt(-t106 * t94)
      t112 = 0.1D1 / (-z - x3 + 0.2D1 * t105 * t108)
      t114 = t3 * t56
      t117 = log(0.4D1 * t93 * t18)
      t118 = t117 * t3
      t120 = t117 ** 2
      t130 = t3 * t46
      t136 = t88 * t112
      t137 = t3 * t88
      t142 = 0.1D1 / x3
      t144 = 0.1D1 / x1
      t147 = t41 * t7
      t148 = t92 * t17
      t149 = t148 * t14
      t151 = log(0.4D1 * t149)
      t156 = t151 ** 2
      t167 = t32 * t7
      t170 = lh * t7
      t181 = x2 ** 2
      t182 = x3 * t181
      t185 = log(0.4D1 * t182 * t149)
      t188 = 0.1D1 - x2
      t189 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, 0.
     #10D1, x4)
      t190 = t3 * t189
      t191 = t182 * t92
      t192 = -t188
      t193 = t18 * t192
      t196 = log(-0.4D1 * t191 * t193)
      t198 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, 0.
     #10D1, x4)
      t202 = log(-0.4D1 * t191 * t96)
      t209 = t3 * t198
      t211 = t7 * (t136 - t209 + t137)
      t216 = 0.1D1 / x2
      t217 = t216 * t144
      t220 = t181 * t92
      t223 = log(0.4D1 * t220 * t18)
      t224 = t223 * t3
      t226 = t223 ** 2
      t230 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, 0.
     #10D1, x4)
      t231 = t3 * t230
      t234 = log(-0.4D1 * t220 * t193)
      t235 = t234 * t3
      t237 = t234 ** 2
      t259 = t7 * t46
      t262 = t7 * t88
      t265 = x3 * t14
      t268 = log(0.4D1 * t265 * t17)
      t273 = log(-0.4D1 * t265 * t17 * t95)
      t277 = t273 ** 2
      t280 = t268 ** 2
      t313 = log(0.4D1 * t182 * t18)
      t314 = t313 * t3
      t318 = log(-0.4D1 * t182 * t96)
      t320 = t318 ** 2
      t325 = t313 ** 2
      t331 = log(-0.4D1 * t182 * t193)
      t332 = t331 * t3
      t334 = t331 ** 2
      t355 = t14 * t181
      t358 = log(0.4D1 * t355 * t17)
      t360 = t17 * t192
      t363 = log(-0.4D1 * t355 * t360)
      t368 = t363 ** 2
      t371 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, 0.
     #10D1, x4)
      t376 = t358 ** 2
      t403 = -t4 * t7 * t8 / 0.32D2 - (-0.90D2 * t22 * t23 + t4 * t32 - 
     #0.15D2 * t35 * 0.3141592653589793D1 - t38 * t42) * t7 * t46 / 0.28
     #80D4 - (0.180D3 * t38 * t23 + 0.45D2 * t22 * 0.3141592653589793D1 
     #+ t4 * t41) * t7 * t56 / 0.2880D4 - (-0.180D3 * t4 * lh - 0.90D2 *
     # t38 * 0.3141592653589793D1) * t7 * t65 / 0.2880D4 - (0.30D2 * t35
     # * t23 + t22 * t42 / 0.2D1 - t38 * t72 + t4 * (0.5769873135166051D
     #3 * lh + t75 + 0.60D2 * t76 - 0.60D2 * t29 * t26) + 0.15D2 / 0.4D1
     # * t82 * t3 * 0.3141592653589793D1) * t7 * t88 / 0.2880D4 - (0.90D
     #2 * t91 * ((t56 - t99 * t46 + t101 * t88 / 0.2D1) * t112 + t114 - 
     #t118 * t46 + t120 * t3 * t88 / 0.2D1) - 0.180D3 * t23 * t7 * ((t46
     # - t99 * t88) * t112 + t130 - t118 * t88) + t42 * t7 * (t136 + t13
     #7)) * t142 * t144 / 0.1440D4 + (t4 * t147 * (-t46 + t151 * t88) + 
     #0.90D2 * t4 * t7 * (-t156 * t46 / 0.2D1 - t65 + t156 * t151 * t88 
     #/ 0.6D1 + t151 * t56) - t4 * t167 * t88 - 0.180D3 * t4 * t170 * (-
     #t56 + t151 * t46 - t156 * t88 / 0.2D1)) * t144 / 0.1440D4 - (0.90D
     #2 * t91 * (t130 - t185 * t3 * t88 - t190 + t196 * t3 * t198 + (t46
     # - t202 * t88) * t112) - 0.180D3 * t23 * t211) * t142 * t217 / 0.7
     #20D3 - (0.90D2 * t91 * (t114 - t224 * t46 + t226 * t3 * t88 / 0.2D
     #1 - t231 + t235 * t189 - t237 * t3 * t198 / 0.2D1) - 0.180D3 * t23
     # * t7 * (t130 - t224 * t88 - t190 + t235 * t198) + t42 * t7 * (-t2
     #09 + t137)) * t216 * t144 / 0.720D3 + ((0.90D2 * t91 * t56 - 0.180
     #D3 * t23 * t259 + t42 * t262) * (t268 * t3 + t273 * t112) + 0.90D2
     # * t91 * t88 * (t277 * t273 * t112 / 0.6D1 + t280 * t268 * t3 / 0.
     #6D1) + (t42 * t259 + 0.90D2 * t91 * t65 + t72 * t262 - 0.180D3 * t
     #23 * t7 * t56) * (-t3 - t112) + (0.90D2 * t91 * t46 - 0.180D3 * t2
     #3 * t262) * (-t280 * t3 / 0.2D1 - t277 * t112 / 0.2D1)) * t142 / 0
     #.2880D4 - (0.90D2 * t91 * (t114 - t314 * t46 + (t56 - t318 * t46 +
     # t320 * t88 / 0.2D1) * t112 + t325 * t3 * t88 / 0.2D1 - t231 + t33
     #2 * t189 - t334 * t3 * t198 / 0.2D1) - 0.180D3 * t23 * t7 * (t130 
     #+ (t46 - t318 * t88) * t112 - t314 * t88 - t190 + t332 * t198) + t
     #42 * t211) * t142 * t216 / 0.1440D4 - (t4 * t147 * (t46 - t358 * t
     #88 - t189 + t363 * t198) + 0.90D2 * t4 * t7 * (-t368 * t189 / 0.2D
     #1 - t371 + t368 * t363 * t198 / 0.6D1 + t363 * t230 + t376 * t46 /
     # 0.2D1 + t65 - t376 * t358 * t88 / 0.6D1 - t358 * t56) + t4 * t167
     # * (t88 - t198) - 0.180D3 * t4 * t170 * (t56 - t358 * t46 + t376 *
     # t88 / 0.2D1 - t230 + t363 * t189 - t368 * t198 / 0.2D1)) * t216 /
     # 0.1440D4
      t404 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t403)
      t406 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t403)
      t408 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t403)
      t410 = x2 * x3
      t411 = 0.1D1 - x3 + t410
      t412 = 0.1D1 / t411
      t413 = t410 * t412
      t414 = t2 * t413
      t415 = t94 * t412
      t416 = t2 * t415
      t417 = t192 * t94
      t419 = Sqrt(t106 * t417)
      t423 = 0.1D1 / (-z - x3 + t410 + 0.2D1 * t105 * t419)
      t424 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, -t
     #415, x4)
      t425 = t423 * t424
      t426 = t182 * t148
      t428 = t411 ** 2
      t429 = 0.1D1 / t428
      t430 = t94 * t429
      t434 = log(0.4D1 * t426 * t14 * t192 * t430)
      t436 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, -t
     #415, x4)
      t442 = t7 * t423 * t436
      t448 = (0.90D2 * t91 * (-t425 + t434 * t423 * t436) + 0.180D3 * t2
     #3 * t442) * t142 * t217 / 0.720D3
      t449 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t188, -t
     #415, x4)
      t455 = log(0.4D1 * t182 * t14 * t360 * t430)
      t456 = t455 * t423
      t458 = t455 ** 2
      t462 = -t423 * t449 + t456 * t424 - t458 * t423 * t436 / 0.2D1
      t466 = -t425 + t456 * t436
      t470 = t42 * t442
      t475 = -t448 - (0.90D2 * t91 * t462 - 0.180D3 * t23 * t7 * t466 - 
     #t470) * t142 * t216 / 0.1440D4
      t476 = FJET(XB1, XB2, s, 0.0D0, t414, 0.0D0, -t416, 0.0D0, t475)
      t479 = t1 * x1
      t480 = x1 * z
      t481 = -z - x1 + t480
      t482 = 0.1D1 / t481
      t484 = t192 * s * t479 * t482
      t485 = -0.1D1 + x1
      t486 = t2 * t485
      t488 = x2 * s * t479
      t489 = t1 ** 2
      t490 = s * t489
      t493 = x1 * t485 * t482
      t494 = t490 * t192 * t493
      t495 = x2 * x1
      t496 = t495 * z
      t498 = 0.1D1 / (t496 - t495 - z)
      t499 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, x1, t188, 0.10D
     #1, x4)
      t500 = t498 * t499
      t501 = 0.1D1 / t12
      t502 = t501 * t482
      t503 = t485 ** 2
      t505 = t502 * t503 * t192
      t508 = log(0.4D1 * t426 * t505)
      t510 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t188, 0.10D
     #1, x4)
      t516 = t7 * t498 * t510
      t522 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, x1, t188, 0.10D
     #1, x4)
      t524 = t220 * t17
      t527 = log(0.4D1 * t524 * t505)
      t528 = t527 * t498
      t530 = t527 ** 2
      t547 = -(0.90D2 * t91 * (-t500 + t508 * t498 * t510) + 0.180D3 * t
     #23 * t516) * t142 * t217 / 0.720D3 - (0.90D2 * t91 * (-t498 * t522
     # + t528 * t499 - t530 * t498 * t510 / 0.2D1) - 0.180D3 * t23 * t7 
     #* (-t500 + t528 * t510) - t42 * t516) * t216 * t144 / 0.720D3
      t548 = FJET(XB1, XB2, s, 0.0D0, t484, -t486, t488, -t494, t547)
      t551 = t2 * x1 * t482
      t552 = t490 * t493
      t553 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t554 = t3 * t553
      t555 = t93 * t17
      t556 = t502 * t503
      t559 = log(-0.4D1 * t555 * t556)
      t560 = t559 * t3
      t561 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t563 = t559 ** 2
      t565 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t570 = t502 * t503 * t95
      t573 = log(0.4D1 * t555 * t570)
      t574 = t573 * t481
      t576 = t573 ** 2
      t581 = x3 * x1
      t582 = t581 * z
      t584 = 0.2D1 * t93 * z
      t585 = x1 * t12
      t586 = x3 * t12
      t587 = t586 * x1
      t588 = t93 * t12
      t589 = x3 * t481
      t591 = Sqrt(t589 * t94)
      t596 = 0.1D1 / (-t480 - t582 + t584 + t585 + t587 - t588 - t93 - t
     #106 + 0.2D1 * t105 * t591 * z - t12)
      t601 = t3 * t561
      t603 = t481 * t561
      t615 = t7 * (-t3 * t565 + t481 * t565 * t596)
      t620 = (0.90D2 * t91 * (-t554 + t560 * t561 - t563 * t3 * t565 / 0
     #.2D1 + (t481 * t553 - t574 * t561 + t576 * t481 * t565 / 0.2D1) * 
     #t596) - 0.180D3 * t23 * t7 * (-t601 + t560 * t565 + (t603 - t574 *
     # t565) * t596) + t42 * t615) * t142 * t144 / 0.1440D4
      t623 = log(-0.4D1 * t148 * t556)
      t628 = t623 ** 2
      t631 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t651 = (t4 * t147 * (t561 - t623 * t565) + 0.90D2 * t4 * t7 * (t62
     #8 * t561 / 0.2D1 + t631 - t628 * t623 * t565 / 0.6D1 - t623 * t553
     #) + t4 * t167 * t565 - 0.180D3 * t4 * t170 * (t553 - t623 * t561 +
     # t628 * t565 / 0.2D1)) * t144 / 0.1440D4
      t654 = log(0.4D1 * t426 * t570)
      t660 = t482 * t503
      t664 = log(-0.4D1 * t191 * t17 * t501 * t660)
      t675 = (0.90D2 * t91 * ((t603 - t654 * t481 * t565) * t596 - t601 
     #+ t664 * t3 * t565) - 0.180D3 * t23 * t615) * t142 * t217 / 0.720D
     #3
      t678 = log(-0.4D1 * t524 * t556)
      t679 = t678 * t3
      t681 = t678 ** 2
      t685 = -t554 + t679 * t561 - t681 * t3 * t565 / 0.2D1
      t689 = -t601 + t679 * t565
      t695 = t42 * t7 * t3 * t565
      t700 = -t620 + t651 - t675 - (0.90D2 * t91 * t685 - 0.180D3 * t23 
     #* t7 * t689 - t695) * t216 * t144 / 0.720D3
      t701 = FJET(XB1, XB2, s, 0.0D0, -t486, -t551, 0.0D0, t552, t700)
      t703 = FJET(XB1, XB2, s, 0.0D0, -t551, -t486, 0.0D0, t552, t700)
      t705 = FJET(XB1, XB2, s, 0.0D0, -t416, 0.0D0, t414, 0.0D0, t475)
      t707 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t403)
      t709 = FJET(XB1, XB2, s, t488, -t486, t484, 0.0D0, -t494, t547)
      t722 = -t448 - (0.90D2 * t91 * t462 - 0.180D3 * t23 * t7 * t466 - 
     #t470) * t142 * t216 / 0.1440D4
      t723 = FJET(XB1, XB2, s, t414, 0.0D0, -t416, 0.0D0, 0.0D0, t722)
      t725 = FJET(XB1, XB2, s, t484, 0.0D0, t488, -t486, -t494, t547)
      t730 = t94 * s * t1 * t485 * t412
      t731 = t2 * x1
      t733 = Sqrt(-t589 * t417)
      t734 = t105 * t733
      t740 = t731 * x2 * (-x3 + t410 - z + t106 - x1 + t581 + t480 - t58
     #2 + 0.2D1 * t734) * t482 * t412
      t741 = t486 * t413
      t744 = t182 * t480
      t745 = t182 * x1
      t750 = t731 * (0.2D1 * t734 * x2 - t744 + t745 + t182 * z + 0.1D1 
     #- x3 - x2 + t410) * t482 * t412
      t762 = x2 * t92
      t765 = t496 - t585 + t93 + t106 - 0.2D1 * t734 * t495 - 0.2D1 * t4
     #10 * t480 + t586 * t495 + 0.2D1 * t93 * x2 * z - t93 * t12 * x2 + 
     #t744 + t480 - t495 * t12 - 0.2D1 * t762 * z
      t774 = t762 * t12 + t12 - t745 - 0.2D1 * t734 * z - t410 * z + t41
     #0 * x1 - t93 * x2 + 0.2D1 * t734 * t496 + t762 + t582 - t584 - t58
     #7 + t588
      t776 = 0.1D1 / (t765 + t774)
      t777 = t481 * t776
      t778 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, x1, t188, -t415
     #, x4)
      t786 = log(-0.4D1 * t182 * t148 * t501 * t660 * t417 * t429)
      t788 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t188, -t415
     #, x4)
      t798 = 0.90D2 * t91 * (t777 * t778 - t786 * t481 * t776 * t788) - 
     #0.180D3 * t23 * t7 * t777 * t788
      t801 = t798 * t142 * t217 / 0.720D3
      t802 = FJET(XB1, XB2, s, t730, t740, -t741, -t750, -t494, -t801)
      t805 = t142 * t216 * t144
      t808 = FJET(XB1, XB2, s, t740, t730, -t750, -t741, -t494, -t801)
      t812 = FJET(XB1, XB2, s, -t486, 0.0D0, 0.0D0, -t551, t552, t700)
      t814 = FJET(XB1, XB2, s, -t486, t488, 0.0D0, t484, -t494, t547)
      t827 = -t620 + t651 - t675 - (0.90D2 * t91 * t685 - 0.180D3 * t23 
     #* t7 * t689 - t695) * t216 * t144 / 0.720D3
      t828 = FJET(XB1, XB2, s, -t551, 0.0D0, 0.0D0, -t486, t552, t827)
      t830 = FJET(XB1, XB2, s, -t416, 0.0D0, t414, 0.0D0, 0.0D0, t722)
      t832 = FJET(XB1, XB2, s, -t750, -t741, t740, t730, -t494, -t801)
      t836 = FJET(XB1, XB2, s, -t741, -t750, t730, t740, -t494, -t801)
      rrgg2qqbarht5s2e1 = t404 * t403 + t406 * t403 + t408 * t403 + t476
     # * t475 + t548 * t547 + t701 * t700 + t703 * t700 + t705 * t475 + 
     #t707 * t403 + t709 * t547 + t723 * t722 + t725 * t547 - t802 * t79
     #8 * t805 / 0.720D3 - t808 * t798 * t805 / 0.720D3 + t812 * t700 + 
     #t814 * t547 + t828 * t827 + t830 * t722 - t832 * t798 * t805 / 0.7
     #20D3 - t836 * t798 * t805 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht5s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t17 = log(0.4D1 * t11 * t14)
      t18 = t17 ** 2
      t19 = 0.1D1 / z
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t26 = log(-0.4D1 * t11 * t14 * t22)
      t27 = t26 ** 2
      t28 = cos(t12)
      t29 = x3 * z
      t31 = Sqrt(-t29 * t21)
      t35 = 0.1D1 / (-z - x3 + 0.2D1 * t28 * t31)
      t42 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t45 = 0.3141592653589793D1 * lh
      t46 = t5 * t7
      t54 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t60 = lh ** 2
      t62 = 0.3141592653589793D1 ** 2
      t64 = 0.180D3 * t60 - 0.30D2 * t62
      t65 = 0.3141592653589793D1 * t64
      t71 = 0.1D1 / x3
      t74 = t10 * t14
      t76 = log(0.4D1 * t74)
      t77 = t76 * t19
      t80 = t76 ** 2
      t81 = t80 * t19
      t84 = 0.3141592653589793D1 * t19
      t90 = rrgg2qqbarh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t119 = t19 * t42
      t120 = x2 ** 2
      t121 = x3 * t120
      t122 = t74 * t22
      t125 = log(-0.4D1 * t121 * t122)
      t131 = log(0.4D1 * t121 * t74)
      t134 = 0.1D1 - x2
      t135 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.
     #10D1, x4)
      t136 = t19 * t135
      t137 = -t134
      t138 = t74 * t137
      t141 = log(-0.4D1 * t121 * t138)
      t143 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.
     #10D1, x4)
      t148 = t7 * t35
      t149 = t19 * t143
      t150 = t19 * t7
      t151 = t148 - t149 + t150
      t157 = 0.1D1 / x2
      t160 = t10 * t120
      t163 = log(0.4D1 * t160 * t14)
      t165 = t163 ** 2
      t168 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.
     #10D1, x4)
      t169 = t14 * t137
      t172 = log(-0.4D1 * t160 * t169)
      t174 = t172 ** 2
      t181 = lh * t5
      t188 = t64 * t5
      t195 = x1 ** 2
      t196 = x3 * t195
      t199 = log(-0.4D1 * t196 * t122)
      t205 = log(0.4D1 * t196 * t74)
      t217 = 0.1D1 / x1
      t222 = t71 * t157 * t217
      t225 = t120 * t195
      t228 = log(0.4D1 * t225 * t74)
      t233 = log(-0.4D1 * t225 * t138)
      t247 = t195 * t14
      t250 = log(0.4D1 * t247 * t10)
      t252 = t250 ** 2
      t269 = (0.90D2 * t6 * t7 * (-t18 * t19 / 0.2D1 - t27 * t35 / 0.2D1
     #) + (0.90D2 * t6 * t42 - 0.180D3 * t45 * t46) * (t17 * t19 + t26 *
     # t35) + (0.90D2 * t6 * t54 - 0.180D3 * t45 * t5 * t42 + t65 * t46)
     # * (-t19 - t35)) * t71 / 0.2880D4 - (0.180D3 * t77 * t45 + 0.45D2 
     #* t81 * 0.3141592653589793D1 + t84 * t64) * t5 * t42 / 0.2880D4 - 
     #t84 * t5 * t90 / 0.32D2 - (-0.90D2 * t81 * t45 + t84 * (0.60D2 * l
     #h * t62 - 0.2884936567583026D3 - 0.120D3 * t60 * lh) - 0.15D2 * t8
     #0 * t76 * t19 * 0.3141592653589793D1 - t77 * t65) * t5 * t7 / 0.28
     #80D4 - (-0.180D3 * t84 * lh - 0.90D2 * t77 * 0.3141592653589793D1)
     # * t5 * t54 / 0.2880D4 - (0.90D2 * t6 * (t119 + (t42 - t125 * t7) 
     #* t35 - t131 * t19 * t7 - t136 + t141 * t19 * t143) - 0.180D3 * t4
     #5 * t5 * t151) * t71 * t157 / 0.1440D4 - (0.90D2 * t84 * t5 * (t54
     # - t163 * t42 + t165 * t7 / 0.2D1 - t168 + t172 * t135 - t174 * t1
     #43 / 0.2D1) - 0.180D3 * t84 * t181 * (t42 - t163 * t7 - t135 + t17
     #2 * t143) + t84 * t188 * (t7 - t143)) * t157 / 0.1440D4 - (0.90D2 
     #* t6 * ((t42 - t199 * t7) * t35 + t119 - t205 * t19 * t7) - 0.180D
     #3 * t45 * t5 * (t148 + t150)) * t71 * t217 / 0.1440D4 - t6 * t151 
     #* t222 / 0.8D1 - (0.90D2 * t6 * (t119 - t228 * t19 * t7 - t136 + t
     #233 * t19 * t143) - 0.180D3 * t45 * t5 * (-t149 + t150)) * t157 * 
     #t217 / 0.720D3 + (0.90D2 * t84 * t5 * (-t54 + t250 * t42 - t252 * 
     #t7 / 0.2D1) - 0.180D3 * t84 * t181 * (-t42 + t250 * t7) - t84 * t1
     #88 * t7) * t217 / 0.1440D4
      t270 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t269)
      t272 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t269)
      t274 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t269)
      t276 = x2 * x3
      t277 = 0.1D1 - x3 + t276
      t278 = 0.1D1 / t277
      t279 = t276 * t278
      t280 = t2 * t279
      t281 = t21 * t278
      t282 = t2 * t281
      t283 = t137 * t21
      t285 = Sqrt(t29 * t283)
      t289 = 0.1D1 / (-z - x3 + t276 + 0.2D1 * t28 * t285)
      t290 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t
     #281, x4)
      t293 = t277 ** 2
      t299 = log(0.4D1 * t121 * t10 * t169 * t21 / t293)
      t301 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t
     #281, x4)
      t303 = -t289 * t290 + t299 * t289 * t301
      t309 = 0.180D3 * t45 * t5 * t289 * t301
      t316 = t157 * t217
      t319 = t6 * t289 * t301 * t71 * t316 / 0.8D1
      t320 = -(0.90D2 * t6 * t303 + t309) * t71 * t157 / 0.1440D4 + t319
      t321 = FJET(XB1, XB2, s, 0.0D0, t280, 0.0D0, -t282, 0.0D0, t320)
      t324 = t1 * x1
      t325 = x1 * z
      t326 = -z - x1 + t325
      t327 = 0.1D1 / t326
      t329 = t137 * s * t324 * t327
      t330 = -0.1D1 + x1
      t331 = t2 * t330
      t333 = x2 * s * t324
      t334 = t1 ** 2
      t335 = s * t334
      t338 = x1 * t330 * t327
      t339 = t335 * t137 * t338
      t340 = x2 * x1
      t341 = t340 * z
      t343 = 0.1D1 / (t341 - t340 - z)
      t345 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D
     #1, x4)
      t350 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D
     #1, x4)
      t352 = t225 * t14
      t354 = 0.1D1 / t8 * t327
      t355 = t330 ** 2
      t360 = log(0.4D1 * t352 * t354 * t355 * t137)
      t374 = t6 * t343 * t345 * t71 * t316 / 0.8D1 - (0.90D2 * t6 * (-t3
     #43 * t350 + t360 * t343 * t345) + 0.180D3 * t45 * t5 * t343 * t345
     #) * t157 * t217 / 0.720D3
      t375 = FJET(XB1, XB2, s, 0.0D0, t329, -t331, t333, -t339, t374)
      t378 = t2 * x1 * t327
      t379 = t335 * t338
      t380 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t381 = t19 * t380
      t382 = t196 * t14
      t383 = t354 * t355
      t386 = log(-0.4D1 * t382 * t383)
      t388 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t395 = log(0.4D1 * t382 * t354 * t355 * t22)
      t399 = x3 * x1
      t400 = t399 * z
      t402 = 0.2D1 * t196 * z
      t403 = x1 * t8
      t404 = x3 * t8
      t405 = t404 * x1
      t406 = t196 * t8
      t407 = x3 * t326
      t409 = Sqrt(t407 * t21)
      t414 = 0.1D1 / (-t325 - t400 + t402 + t403 + t405 - t406 - t196 - 
     #t29 + 0.2D1 * t28 * t409 * z - t8)
      t422 = -t19 * t388 + t326 * t388 * t414
      t429 = (0.90D2 * t6 * (-t381 + t386 * t19 * t388 + (t326 * t380 - 
     #t395 * t326 * t388) * t414) - 0.180D3 * t45 * t5 * t422) * t71 * t
     #217 / 0.1440D4
      t432 = t6 * t422 * t222 / 0.8D1
      t435 = log(-0.4D1 * t352 * t383)
      t438 = -t381 + t435 * t19 * t388
      t442 = t5 * t19 * t388
      t444 = 0.180D3 * t45 * t442
      t449 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t452 = log(-0.4D1 * t247 * t383)
      t454 = t452 ** 2
      t469 = (0.90D2 * t84 * t5 * (t449 - t452 * t380 + t454 * t388 / 0.
     #2D1) - 0.180D3 * t84 * t181 * (t380 - t452 * t388) + t65 * t442) *
     # t217 / 0.1440D4
      t470 = -t429 - t432 - (0.90D2 * t6 * t438 + t444) * t157 * t217 / 
     #0.720D3 + t469
      t471 = FJET(XB1, XB2, s, 0.0D0, -t331, -t378, 0.0D0, t379, t470)
      t473 = FJET(XB1, XB2, s, 0.0D0, -t378, -t331, 0.0D0, t379, t470)
      t475 = FJET(XB1, XB2, s, 0.0D0, -t282, 0.0D0, t280, 0.0D0, t320)
      t477 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t269)
      t479 = FJET(XB1, XB2, s, t333, -t331, t329, 0.0D0, -t339, t374)
      t488 = -(0.90D2 * t6 * t303 + t309) * t71 * t157 / 0.1440D4 + t319
      t489 = FJET(XB1, XB2, s, t280, 0.0D0, -t282, 0.0D0, 0.0D0, t488)
      t491 = FJET(XB1, XB2, s, t329, 0.0D0, t333, -t331, -t339, t374)
      t496 = t21 * s * t1 * t330 * t278
      t497 = t2 * x1
      t499 = Sqrt(-t407 * t283)
      t500 = t28 * t499
      t506 = t497 * x2 * (-x3 + t276 - z + t29 - x1 + t399 + t325 - t400
     # + 0.2D1 * t500) * t327 * t278
      t507 = t331 * t279
      t510 = t121 * t325
      t511 = t121 * x1
      t516 = t497 * (0.2D1 * t500 * x2 - t510 + t511 + t121 * z + 0.1D1 
     #- x3 - x2 + t276) * t327 * t278
      t523 = x2 * t195
      t527 = t341 + t400 - t402 - t405 + t406 - t511 - 0.2D1 * t500 * z 
     #- t276 * z + t276 * x1 - t196 * x2 - t340 * t8 - 0.2D1 * t523 * z 
     #+ t523 * t8
      t540 = t523 + t510 - 0.2D1 * t500 * t340 - 0.2D1 * t276 * t325 + t
     #404 * t340 + 0.2D1 * t196 * x2 * z - t196 * t8 * x2 + 0.2D1 * t500
     # * t341 + t8 + t325 - t403 + t196 + t29
      t542 = 0.1D1 / (t527 + t540)
      t545 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, -t281
     #, x4)
      t549 = t6 * t326 * t542 * t545 * t71 * t316 / 0.8D1
      t550 = FJET(XB1, XB2, s, t496, t506, -t507, -t516, -t339, -t549)
      t552 = t5 * t326
      t555 = t542 * t545 * t222
      t558 = FJET(XB1, XB2, s, t506, t496, -t516, -t507, -t339, -t549)
      t563 = FJET(XB1, XB2, s, -t331, 0.0D0, 0.0D0, -t378, t379, t470)
      t565 = FJET(XB1, XB2, s, -t331, t333, 0.0D0, t329, -t339, t374)
      t574 = -t429 - t432 - (0.90D2 * t6 * t438 + t444) * t157 * t217 / 
     #0.720D3 + t469
      t575 = FJET(XB1, XB2, s, -t378, 0.0D0, 0.0D0, -t331, t379, t574)
      t577 = FJET(XB1, XB2, s, -t282, 0.0D0, t280, 0.0D0, 0.0D0, t488)
      t579 = FJET(XB1, XB2, s, -t516, -t507, t506, t496, -t339, -t549)
      t584 = FJET(XB1, XB2, s, -t507, -t516, t496, t506, -t339, -t549)
      rrgg2qqbarht5s2e0 = t270 * t269 + t272 * t269 + t274 * t269 + t321
     # * t320 + t375 * t374 + t471 * t470 + t473 * t470 + t475 * t320 + 
     #t477 * t269 + t479 * t374 + t489 * t488 + t491 * t374 - t550 * 0.3
     #141592653589793D1 * t552 * t555 / 0.8D1 - t558 * 0.314159265358979
     #3D1 * t552 * t555 / 0.8D1 + t563 * t470 + t565 * t374 + t575 * t57
     #4 + t577 * t488 - t579 * 0.3141592653589793D1 * t552 * t555 / 0.8D
     #1 - t584 * 0.3141592653589793D1 * t552 * t555 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht5s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t17 = log(0.4D1 * t11 * t14)
      t18 = 0.1D1 / z
      t20 = -0.1D1 + x3
      t25 = log(-0.4D1 * t11 * t14 / t20)
      t26 = cos(t12)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t20)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t26)
      t39 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t42 = 0.3141592653589793D1 * lh
      t50 = 0.1D1 / x3
      t53 = 0.3141592653589793D1 * t18
      t54 = rrgg2qqbarh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t62 = log(0.4D1 * t10 * t14)
      t63 = t62 * t18
      t72 = t62 ** 2
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t86 = t7 * t33
      t87 = 0.1D1 - x2
      t88 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t87, 0.10
     #D1, x4)
      t89 = t18 * t88
      t90 = t18 * t7
      t93 = 0.1D1 / x2
      t97 = x2 ** 2
      t98 = t10 * t97
      t101 = log(0.4D1 * t98 * t14)
      t103 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t87, 0.1
     #0D1, x4)
      t104 = -t87
      t108 = log(-0.4D1 * t98 * t14 * t104)
      t114 = lh * t5
      t124 = 0.1D1 / x1
      t128 = x1 ** 2
      t129 = t128 * t14
      t132 = log(0.4D1 * t129 * t10)
      t149 = (0.90D2 * t6 * t7 * (t17 * t18 + t25 * t33) + (0.90D2 * t6 
     #* t39 - 0.180D3 * t42 * t5 * t7) * (-t18 - t33)) * t50 / 0.2880D4 
     #- t53 * t5 * t54 / 0.32D2 - (-0.180D3 * t53 * lh - 0.90D2 * t63 * 
     #0.3141592653589793D1) * t5 * t39 / 0.2880D4 - (0.180D3 * t63 * t42
     # + 0.45D2 * t72 * t18 * 0.3141592653589793D1 + t53 * (0.180D3 * t7
     #6 - 0.30D2 * t78)) * t5 * t7 / 0.2880D4 - t6 * (t86 - t89 + t90) *
     # t50 * t93 / 0.16D2 - (0.90D2 * t53 * t5 * (t39 - t101 * t7 - t103
     # + t108 * t88) - 0.180D3 * t53 * t114 * (t7 - t88)) * t93 / 0.1440
     #D4 - t6 * (-t89 + t90) * t93 * t124 / 0.8D1 + (0.90D2 * t53 * t5 *
     # (-t39 + t132 * t7) + 0.180D3 * t53 * t114 * t7) * t124 / 0.1440D4
     # - t6 * (t86 + t90) * t50 * t124 / 0.16D2
      t150 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t149)
      t152 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t149)
      t154 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t149)
      t156 = x2 * x3
      t158 = 0.1D1 / (0.1D1 - x3 + t156)
      t160 = t2 * t156 * t158
      t161 = t20 * t158
      t162 = t2 * t161
      t165 = Sqrt(t27 * t104 * t20)
      t169 = 0.1D1 / (-z - x3 + t156 + 0.2D1 * t26 * t165)
      t171 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t87, -t1
     #61, x4)
      t175 = t6 * t169 * t171 * t50 * t93 / 0.16D2
      t176 = FJET(XB1, XB2, s, 0.0D0, t160, 0.0D0, -t162, 0.0D0, t175)
      t181 = t169 * t171 * t50 * t93
      t185 = t1 * x1
      t186 = x1 * z
      t187 = -z - x1 + t186
      t188 = 0.1D1 / t187
      t190 = t104 * s * t185 * t188
      t191 = -0.1D1 + x1
      t192 = t2 * t191
      t194 = x2 * s * t185
      t195 = t1 ** 2
      t196 = s * t195
      t199 = x1 * t191 * t188
      t200 = t196 * t104 * t199
      t201 = x2 * x1
      t204 = 0.1D1 / (t201 * z - t201 - z)
      t206 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t87, 0.10D1
     #, x4)
      t210 = t6 * t204 * t206 * t93 * t124 / 0.8D1
      t211 = FJET(XB1, XB2, s, 0.0D0, t190, -t192, t194, -t200, t210)
      t216 = t204 * t206 * t93 * t124
      t220 = t2 * x1 * t188
      t221 = t196 * t199
      t223 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t228 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t231 = t191 ** 2
      t235 = log(-0.4D1 * t129 / t8 * t188 * t231)
      t252 = x3 * t128
      t261 = Sqrt(x3 * t187 * t20)
      t273 = t6 * t18 * t223 * t93 * t124 / 0.8D1 + (0.90D2 * t53 * t5 *
     # (t228 - t235 * t223) - 0.180D3 * t42 * t5 * t18 * t223) * t124 / 
     #0.1440D4 - t6 * (-t18 * t223 + t187 * t223 / (-t186 - x3 * x1 * z 
     #+ 0.2D1 * t252 * z + x1 * t8 + x3 * t8 * x1 - t252 * t8 - t252 - t
     #27 + 0.2D1 * t26 * t261 * z - t8)) * t50 * t124 / 0.16D2
      t274 = FJET(XB1, XB2, s, 0.0D0, -t192, -t220, 0.0D0, t221, t273)
      t276 = FJET(XB1, XB2, s, 0.0D0, -t220, -t192, 0.0D0, t221, t273)
      t278 = FJET(XB1, XB2, s, 0.0D0, -t162, 0.0D0, t160, 0.0D0, t175)
      t283 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t149)
      t285 = FJET(XB1, XB2, s, t194, -t192, t190, 0.0D0, -t200, t210)
      t290 = FJET(XB1, XB2, s, t160, 0.0D0, -t162, 0.0D0, 0.0D0, t175)
      t295 = FJET(XB1, XB2, s, t190, 0.0D0, t194, -t192, -t200, t210)
      t300 = FJET(XB1, XB2, s, -t192, 0.0D0, 0.0D0, -t220, t221, t273)
      t302 = FJET(XB1, XB2, s, -t192, t194, 0.0D0, t190, -t200, t210)
      t307 = FJET(XB1, XB2, s, -t220, 0.0D0, 0.0D0, -t192, t221, t273)
      t309 = FJET(XB1, XB2, s, -t162, 0.0D0, t160, 0.0D0, 0.0D0, t175)
      rrgg2qqbarht5s2em1 = t150 * t149 + t152 * t149 + t154 * t149 + t17
     #6 * 0.3141592653589793D1 * t5 * t181 / 0.16D2 + t211 * 0.314159265
     #3589793D1 * t5 * t216 / 0.8D1 + t274 * t273 + t276 * t273 + t278 *
     # 0.3141592653589793D1 * t5 * t181 / 0.16D2 + t283 * t149 + t285 * 
     #0.3141592653589793D1 * t5 * t216 / 0.8D1 + t290 * 0.31415926535897
     #93D1 * t5 * t181 / 0.16D2 + t295 * 0.3141592653589793D1 * t5 * t21
     #6 / 0.8D1 + t300 * t273 + t302 * 0.3141592653589793D1 * t5 * t216 
     #/ 0.8D1 + t307 * t273 + t309 * 0.3141592653589793D1 * t5 * t181 / 
     #0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t8 = 0.1D1 / z
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t14 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t25 = 0.3141592653589793D1 * t8
      t27 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.1D1 - x
     #2, 0.10D1, x4)
      t35 = 0.1D1 / x1
      t39 = rrgg2qqbarh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t45 = z ** 2
      t48 = Sin(t9)
      t49 = t48 ** 2
      t52 = log(0.4D1 / t45 / z * t49)
      t60 = t6 * t7 * (-t8 - 0.1D1 / (-z - x3 + 0.2D1 * t10 * t14)) / x3
     # / 0.32D2 - t25 * t5 * (t7 - t27) / x2 / 0.16D2 - t25 * t5 * t7 * 
     #t35 / 0.16D2 - t25 * t5 * t39 / 0.32D2 - (-0.180D3 * t25 * lh - 0.
     #90D2 * t52 * t8 * 0.3141592653589793D1) * t5 * t7 / 0.2880D4
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t60)
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t60)
      t65 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t60)
      t67 = -0.1D1 + x1
      t68 = t2 * t67
      t71 = 0.1D1 / (-z - x1 + x1 * z)
      t73 = t2 * x1 * t71
      t74 = t1 ** 2
      t78 = s * t74 * x1 * t67 * t71
      t79 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10
     #D1, x4)
      t81 = t8 * t79 * t35
      t83 = t6 * t81 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, -t68, -t73, 0.0D0, t78, t83)
      t89 = FJET(XB1, XB2, s, 0.0D0, -t73, -t68, 0.0D0, t78, t83)
      t94 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t60)
      t96 = FJET(XB1, XB2, s, -t68, 0.0D0, 0.0D0, -t73, t78, t83)
      t101 = FJET(XB1, XB2, s, -t73, 0.0D0, 0.0D0, -t68, t78, t83)
      rrgg2qqbarht5s2em2 = t61 * t60 + t63 * t60 + t65 * t60 + t84 * 0.3
     #141592653589793D1 * t5 * t81 / 0.16D2 + t89 * 0.3141592653589793D1
     # * t5 * t81 / 0.16D2 + t94 * t60 + t96 * 0.3141592653589793D1 * t5
     # * t81 / 0.16D2 + t101 * 0.3141592653589793D1 * t5 * t81 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht5s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = (-0.1D1 + z) * s
      t3 = 0.1D1 / z
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgg2qqbarh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t11 = 0.3141592653589793D1 * t3 * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t3 * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2qqbarht5s2em3 = -t12 * 0.3141592653589793D1 * t15 / 0.32D2 - 
     #t17 * 0.3141592653589793D1 * t15 / 0.32D2 - t20 * 0.31415926535897
     #93D1 * t15 / 0.32D2 - t23 * 0.3141592653589793D1 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht5s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2qqbarht5s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh51J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = 0.1D1 - x2
      t12 = z + x1 * t9 * t4
      t13 = t8 * t12
      t16 = z + x1 * t4
      t17 = t16 ** 2
      t19 = 0.1D1 / t17 / t16
      t20 = x1 ** 2
      t21 = t19 * t20
      t22 = x3 * t9
      t24 = 0.1D1 - x3
      t25 = x2 * t24
      t27 = cos(x4 * 0.3141592653589793D1)
      t31 = Sqrt(t22 * t16 * x2 * t24)
      t33 = 0.2D1 * t27 * t31
      t34 = t22 * t16 + t25 - t33
      t37 = x2 * x3
      t38 = t24 * t9 * t16 + t37 + t33
      t39 = t34 * t38
      t43 = t5 ** 2
      t44 = t3 * t43
      t45 = t44 * t13
      t46 = t17 ** 2
      t47 = 0.1D1 / t46
      t48 = t20 * x1
      t49 = t47 * t48
      t50 = t34 ** 2
      t51 = t50 * t38
      t56 = t3 * t43 * t4
      t57 = t20 ** 2
      t58 = t57 * t19
      t59 = t56 * t58
      t60 = t8 * x3
      t64 = t8 ** 2
      t65 = t64 ** 2
      t66 = t24 ** 2
      t67 = t65 * t66
      t68 = t56 * t67
      t69 = 0.1D1 / t16
      t70 = x1 * t69
      t71 = t37 * t70
      t74 = t64 * t8
      t75 = t74 * t24
      t76 = t44 * t75
      t79 = t65 * t24
      t80 = t56 * t79
      t81 = 0.1D1 / t17
      t82 = t12 * t81
      t87 = t44 * t57
      t88 = t47 * t50
      t89 = t38 ** 2
      t93 = t20 * t69
      t100 = t12 * t19
      t106 = t64 * t24
      t120 = x1 * t38
      t125 = 0.584D3 * t7 * t13 * t21 * t39 - 0.328D3 * t45 * t49 * t51 
     #+ 0.288D3 * t59 * t51 * t60 + 0.56D2 * t68 * t71 + 0.112D3 * t76 *
     # t71 - 0.96D2 * t80 * t82 * t37 * x1 + 0.108D3 * t87 * t88 * t89 -
     # 0.128D3 * t7 * t93 * t34 * z * t60 - 0.94D2 * t56 * t75 * t100 * 
     #t20 * t38 * x2 - 0.47D2 * t44 * t106 * t100 * t20 * t89 + 0.288D3 
     #* t56 * t65 * t12 * t81 * x3 * t66 * x1 * t38 - 0.96D2 * t76 * t82
     # * t120 * x3
      t126 = t66 * t24
      t130 = t66 ** 2
      t134 = t3 * t4
      t138 = t3 * t5
      t142 = t34 * t89
      t147 = t3 * t43 * t5
      t148 = t74 * t12
      t150 = x2 ** 2
      t162 = t64 * t12
      t164 = t34 * x2
      t168 = t56 * t162
      t173 = t7 * t106
      t174 = t82 * t120
      t177 = x3 ** 2
      t182 = t147 * t79
      t184 = t20 * t81
      t188 = t48 * t81
      t189 = t44 * t188
      t193 = 0.144D3 * t7 * t74 * t126 + 0.36D2 * t44 * t65 * t130 + 0.1
     #44D3 * t134 * t8 * t24 + 0.216D3 * t138 * t64 * t66 - 0.430D3 * t4
     #5 * t49 * t142 - 0.192D3 * t147 * t148 * t49 * t34 * t150 + 0.144D
     #3 * t56 * t148 * t19 * x3 * t24 * t20 * t89 - 0.400D3 * t44 * t162
     # * t21 * t164 + 0.182D3 * t168 * t49 * t50 * x2 + 0.128D3 * t173 *
     # t174 - 0.28D2 * t80 * t177 * x2 * t70 - 0.36D2 * t182 * x3 * t150
     # * t184 - 0.96D2 * t189 * t39 * t60
      t195 = t57 * t47
      t197 = t150 * t64
      t201 = t56 * t188
      t206 = t7 * t48
      t207 = t19 * t34
      t208 = t207 * t89
      t212 = t56 * t57 * x1
      t214 = t50 * t34
      t215 = t47 * t38 * t214
      t218 = t19 * t50
      t219 = t218 * t38
      t225 = t81 * t34
      t226 = t225 * t38
      t233 = t44 * x1
      t234 = t74 * t66
      t235 = t234 * x3
      t239 = t106 * x3
      t242 = t75 * t177
      t245 = t47 * t34
      t247 = t245 * t89 * t38
      t250 = -0.36D2 * t147 * t195 * t39 * t197 - 0.94D2 * t201 * t34 * 
     #t64 * t37 + 0.144D3 * t206 * t208 + 0.144D3 * t212 * t215 - 0.16D2
     # * t87 * t219 - 0.108D3 * t87 * t215 - 0.216D3 * t138 * t20 * t226
     # - 0.288D3 * t206 * t219 + 0.880D3 * t206 * t226 - 0.328D3 * t233 
     #* t235 + 0.584D3 * t7 * x1 * t239 - 0.430D3 * t233 * t242 - 0.36D2
     # * t87 * t247
      t252 = t24 * t12
      t256 = t7 * t74
      t257 = t66 * t12
      t261 = t44 * t65
      t278 = t177 * x3
      t292 = t50 ** 2
      t296 = -0.312D3 * t138 * t64 * t252 * t69 - 0.240D3 * t256 * t257 
     #* t69 + 0.12D2 * t261 * t126 * t12 * t69 - 0.748D3 * t87 * t208 - 
     #0.10D2 * t44 * x1 * t74 * t126 - 0.56D2 * t138 * x1 * t8 * t24 - 0
     #.288D3 * t7 * t235 - 0.36D2 * t44 * t79 * t278 - 0.216D3 * t138 * 
     #t239 + 0.144D3 * t7 * t242 + 0.216D3 * t138 * t184 * t50 - 0.240D3
     # * t7 * t188 * t50 + 0.36D2 * t44 * t195 * t292
      t299 = t48 * t19
      t347 = 0.144D3 * t7 * t299 * t214 + 0.12D2 * t44 * t58 * t214 - 0.
     #312D3 * t138 * t93 * t34 + 0.328D3 * t7 * x1 * t64 * t66 - 0.192D3
     # * t147 * t48 * t75 * t150 * t81 + 0.144D3 * t56 * t74 * t24 * t15
     #0 * t184 - 0.128D3 * t189 * t164 * t8 * z - 0.128D3 * t173 * t82 *
     # z * x1 * t38 + 0.144D3 * t134 * t70 * t34 - 0.108D3 * t44 * t65 *
     # t126 * x3 + 0.108D3 * t44 * t67 * t177 + 0.36D2 * t147 * t65 * t6
     #6 * t150 * t184
      t353 = x2 * t69
      t357 = t69 * x3
      t375 = t3 * t43 * t6
      t377 = t150 * x2
      t386 = t44 * t48
      t391 = t56 * t57
      t392 = x2 * t8
      t393 = t218 * t392
      t411 = -0.748D3 * t261 * t252 * t69 * t177 + 0.182D3 * t56 * t20 *
     # t234 * t353 + 0.880D3 * t256 * t252 * t357 - 0.16D2 * t261 * t257
     # * t357 - 0.112D3 * t44 * t74 * t66 * x2 * t70 - 0.28D2 * t56 * t6
     #5 * t126 * x2 * t70 - 0.28D2 * t375 * t65 * t24 * t377 * t299 - 0.
     #168D3 * t7 * t64 * t25 * t70 - 0.148D3 * t386 * t81 * t50 * t60 - 
     #0.148D3 * t391 * t393 + 0.328D3 * t7 * t8 * t100 * t20 * t50 + 0.5
     #30D3 * t56 * t20 * t74 * t24 * x3 * t353 - 0.47D2 * t182 * t100 * 
     #t150 * t20
      t413 = x2 * x1
      t418 = t147 * t57
      t419 = t207 * t197
      t427 = t7 * t20
      t428 = t225 * t392
      t436 = t69 * t34
      t455 = t44 * t20
      t456 = t64 * t177
      t465 = -0.128D3 * t76 * t82 * t413 * z - 0.47D2 * t418 * t419 + 0.
     #36D2 * t418 * t88 * t197 - 0.112D3 * t386 * t393 - 0.168D3 * t427 
     #* t428 + 0.144D3 * t56 * t48 * t419 + 0.128D3 * t386 * t428 + 0.12
     #8D3 * t427 * t436 * t60 - 0.28D2 * t391 * t47 * t214 * t392 - 0.56
     #D2 * t138 * t8 * t82 * x1 * t34 - 0.10D2 * t44 * t8 * t12 * t47 * 
     #t48 * t214 - 0.47D2 * t455 * t436 * t456 - 0.28D2 * t375 * t57 * t
     #245 * t377 * t74
      t470 = t56 * t65 * t8
      t471 = t12 * t69
      t485 = t56 * t195
      t490 = t39 * t392
      t501 = t82 * t413
      t510 = -0.400D3 * t455 * t106 * t353 + 0.144D3 * t470 * t471 * x3 
     #* t126 + 0.144D3 * t470 * t471 * t278 * t24 + 0.144D3 * t212 * t24
     #7 + 0.144D3 * t201 * t39 * t456 - 0.28D2 * t485 * t142 * t392 + 0.
     #112D3 * t44 * t299 * t490 + 0.56D2 * t485 * t51 * t392 - 0.96D2 * 
     #t59 * t490 - 0.148D3 * t44 * t234 * t174 + 0.128D3 * t76 * t501 - 
     #0.148D3 * t68 * t501 + 0.530D3 * t168 * t49 * t39 * x2
      rrgg2qqbarh51J1 = -wd * (t125 + t193 + t250 + t296 + t347 + t411 +
     # t465 + t510) * nf / t1 / z / 0.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t8 = t3 * t6 * t4
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 ** 2
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t4
      t17 = t8 * t11 * t15
      t19 = z + x1 * t4
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = t21 * x3
      t23 = 0.1D1 - x3
      t24 = t23 ** 2
      t28 = x2 * x3
      t30 = cos(x4 * 0.3141592653589793D1)
      t31 = x3 * t12
      t35 = Sqrt(t31 * t19 * x2 * t23)
      t37 = 0.2D1 * t30 * t35
      t38 = t23 * t12 * t19 + t28 + t37
      t41 = t17 * t22 * t24 * x1 * t38
      t43 = t3 * t6
      t44 = t10 * t9
      t45 = t44 * t23
      t46 = t43 * t45
      t47 = t15 * t21
      t48 = x1 * t38
      t51 = t46 * t47 * t48 * x3
      t55 = 0.1D1 / t20 / t19
      t56 = t15 * t55
      t57 = x1 ** 2
      t61 = t8 * t45 * t56 * t57 * t38 * x2
      t63 = t57 ** 2
      t64 = t20 ** 2
      t65 = 0.1D1 / t64
      t66 = t63 * t65
      t67 = t8 * t66
      t69 = x2 * t23
      t70 = t31 * t19 + t69 - t37
      t71 = t38 ** 2
      t72 = t70 * t71
      t73 = x2 * t9
      t77 = t57 * x1
      t78 = t77 * t21
      t79 = t43 * t78
      t80 = t70 * t38
      t81 = x3 * t9
      t83 = t79 * t80 * t81
      t85 = t63 * t55
      t86 = t8 * t85
      t87 = t80 * t73
      t88 = t86 * t87
      t90 = t77 * t55
      t92 = t43 * t90 * t87
      t94 = t70 ** 2
      t95 = t94 * t38
      t99 = t9 * t15
      t100 = t43 * t99
      t101 = t65 * t77
      t103 = t100 * t101 * t72
      t106 = t3 * t6 * t5
      t107 = t11 * t23
      t108 = t106 * t107
      t109 = x2 ** 2
      t111 = t57 * t21
      t113 = t108 * x3 * t109 * t111
      t118 = 0.1D1 / t19
      t119 = x2 * t118
      t121 = t8 * t57 * t44 * t23 * x3 * t119
      t123 = t24 * t23
      t128 = 0.288D3 * t41 - 0.96D2 * t51 - 0.94D2 * t61 - 0.28D2 * t67 
     #* t72 * t73 - 0.96D2 * t83 - 0.96D2 * t88 + 0.112D3 * t92 + 0.56D2
     # * t67 * t95 * t73 - 0.430D3 * t103 - 0.36D2 * t113 + 0.530D3 * t1
     #21 - 0.108D3 * t43 * t11 * t123 * x3
      t129 = t11 * t24
      t130 = x3 ** 2
      t134 = t3 * t5
      t136 = t134 * t111 * t94
      t138 = t5 * t4
      t139 = t3 * t138
      t141 = t139 * t78 * t94
      t143 = t94 ** 2
      t147 = t94 * t70
      t152 = t43 * t85 * t147
      t154 = t57 * t118
      t156 = t134 * t154 * t70
      t160 = t139 * x1 * t10 * t24
      t164 = t43 * x1 * t44 * t123
      t166 = x1 * t9
      t168 = t134 * t166 * t23
      t170 = t44 * t24
      t171 = t170 * x3
      t174 = t130 * x3
      t178 = t10 * t23
      t179 = t178 * x3
      t180 = t134 * t179
      t182 = 0.108D3 * t43 * t129 * t130 + 0.216D3 * t136 - 0.240D3 * t1
     #41 + 0.36D2 * t43 * t66 * t143 + 0.144D3 * t139 * t90 * t147 + 0.1
     #2D2 * t152 - 0.312D3 * t156 + 0.328D3 * t160 - 0.10D2 * t164 - 0.5
     #6D2 * t168 - 0.288D3 * t139 * t171 - 0.36D2 * t43 * t107 * t174 - 
     #0.216D3 * t180
      t184 = t45 * t130
      t187 = t44 * t15
      t191 = t106 * t187 * t101 * t70 * t109
      t193 = t8 * t187
      t199 = 0.144D3 * t193 * t55 * x3 * t23 * t57 * t71
      t200 = t10 * t15
      t201 = t43 * t200
      t202 = t55 * t57
      t203 = t70 * x2
      t205 = t201 * t202 * t203
      t207 = t8 * t200
      t210 = t207 * t101 * t94 * x2
      t212 = t8 * t107
      t214 = x1 * t118
      t218 = t8 * t129
      t219 = t28 * t214
      t222 = t46 * t219
      t227 = t139 * t154 * t70 * z * t81
      t231 = t79 * t203 * t9 * z
      t233 = t139 * t178
      t237 = t233 * t47 * z * x1 * t38
      t241 = t212 * t47 * t28 * x1
      t243 = t3 * t4
      t246 = 0.144D3 * t243 * t214 * t70
      t247 = 0.144D3 * t139 * t184 - 0.192D3 * t191 + t199 - 0.400D3 * t
     #205 + 0.182D3 * t210 - 0.28D2 * t212 * t130 * x2 * t214 + 0.56D2 *
     # t218 * t219 + 0.112D3 * t222 - 0.128D3 * t227 - 0.128D3 * t231 - 
     #0.128D3 * t237 - 0.96D2 * t241 + t246
      t250 = t8 * t57 * t170 * t119
      t252 = t43 * t11
      t253 = t23 * t15
      t256 = t252 * t253 * t118 * t130
      t261 = t106 * t11 * t24 * t109 * t111
      t266 = t8 * t44 * t23 * t109 * t111
      t269 = t47 * t48
      t270 = t43 * t170 * t269
      t272 = t8 * t78
      t273 = t10 * t130
      t276 = 0.144D3 * t272 * t80 * t273
      t280 = t10 * t24
      t281 = t134 * t280
      t283 = t9 * t23
      t285 = 0.144D3 * t243 * t283
      t286 = t24 ** 2
      t292 = t139 * t99 * t202 * t80
      t295 = t100 * t101 * t95
      t299 = t272 * t70 * t10 * t28
      t301 = 0.182D3 * t250 - 0.748D3 * t256 + 0.36D2 * t261 + 0.144D3 *
     # t266 - 0.148D3 * t270 + t276 + 0.144D3 * t139 * t44 * t123 + 0.21
     #6D3 * t281 + t285 + 0.36D2 * t43 * t11 * t286 + 0.584D3 * t292 - 0
     #.328D3 * t295 - 0.94D2 * t299
      t306 = t108 * t56 * t109 * t57
      t308 = x2 * x1
      t311 = t46 * t47 * t308 * z
      t313 = t47 * t308
      t314 = t46 * t313
      t316 = t218 * t313
      t320 = t207 * t101 * t80 * x2
      t325 = t43 * t178 * t56 * t57 * t71
      t328 = t86 * t95 * t81
      t330 = t233 * t269
      t332 = t139 * t44
      t333 = t118 * x3
      t335 = t332 * t253 * t333
      t345 = t43 * t44 * t24 * x2 * t214
      t347 = t24 * t15
      t349 = t252 * t347 * t333
      t351 = -0.47D2 * t306 - 0.128D3 * t311 + 0.128D3 * t314 - 0.148D3 
     #* t316 + 0.530D3 * t320 - 0.47D2 * t325 + 0.288D3 * t328 + 0.128D3
     # * t330 + 0.880D3 * t335 - 0.28D2 * t8 * t11 * t123 * x2 * t214 - 
     #0.112D3 * t345 - 0.16D2 * t349
      t352 = t139 * t10
      t354 = t352 * t69 * t214
      t357 = t3 * t6 * t138
      t359 = t109 * x2
      t362 = t357 * t11 * t23 * t359 * t90
      t364 = t43 * t77
      t367 = t364 * t21 * t94 * t81
      t369 = t8 * t63
      t370 = t55 * t94
      t371 = t370 * t73
      t372 = t369 * t371
      t377 = t139 * t9 * t56 * t57 * t94
      t379 = t106 * t63
      t380 = t55 * t70
      t381 = t109 * t10
      t382 = t380 * t381
      t383 = t379 * t382
      t389 = t357 * t63 * t65 * t70 * t359 * t44
      t391 = t43 * t57
      t392 = t118 * t70
      t394 = t391 * t392 * t273
      t400 = t43 * t9 * t15 * t65 * t77 * t147
      t402 = t134 * t9
      t405 = t402 * t47 * x1 * t70
      t407 = t65 * t147
      t411 = t139 * t57
      t413 = t411 * t392 * t81
      t415 = t21 * t70
      t416 = t415 * t73
      t417 = t364 * t416
      t419 = -0.168D3 * t354 - 0.28D2 * t362 - 0.148D3 * t367 - 0.148D3 
     #* t372 + 0.328D3 * t377 - 0.47D2 * t383 - 0.28D2 * t389 - 0.47D2 *
     # t394 - 0.10D2 * t400 - 0.56D2 * t405 - 0.28D2 * t369 * t407 * t73
     # + 0.128D3 * t413 + 0.128D3 * t417
      t422 = t8 * t77 * t382
      t424 = t411 * t416
      t426 = t364 * t371
      t428 = t65 * t94
      t430 = t379 * t428 * t381
      t433 = t391 * t178 * t119
      t436 = t8 * t11 * t9
      t437 = t15 * t118
      t441 = 0.144D3 * t436 * t437 * t174 * t23
      t445 = 0.144D3 * t436 * t437 * x3 * t123
      t448 = t106 * t66 * t80 * t381
      t453 = t106 * t77 * t45 * t109 * t21
      t455 = t43 * t63
      t456 = t428 * t71
      t460 = t8 * t63 * x1
      t463 = t65 * t71 * t38 * t70
      t465 = 0.144D3 * t460 * t463
      t466 = t407 * t38
      t470 = t415 * t38
      t471 = t134 * t57 * t470
      t473 = 0.144D3 * t422 - 0.168D3 * t424 - 0.112D3 * t426 + 0.36D2 *
     # t430 - 0.400D3 * t433 + t441 + t445 - 0.36D2 * t448 - 0.192D3 * t
     #453 + 0.108D3 * t455 * t456 + t465 - 0.108D3 * t455 * t466 - 0.216
     #D3 * t471
      t474 = t139 * t77
      t475 = t370 * t38
      t478 = t474 * t470
      t480 = t43 * x1
      t481 = t480 * t171
      t484 = t139 * x1 * t179
      t486 = t480 * t184
      t492 = t134 * t10 * t253 * t118
      t495 = t332 * t347 * t118
      t499 = t252 * t123 * t15 * t118
      t501 = t380 * t71
      t502 = t455 * t501
      t507 = 0.144D3 * t460 * t466
      t508 = t455 * t475
      t510 = -0.288D3 * t474 * t475 + 0.880D3 * t478 - 0.328D3 * t481 + 
     #0.584D3 * t484 - 0.430D3 * t486 - 0.36D2 * t455 * t463 - 0.312D3 *
     # t492 - 0.240D3 * t495 + 0.12D2 * t499 - 0.748D3 * t502 + 0.144D3 
     #* t474 * t501 + t507 - 0.16D2 * t508
      t528 = t70 * x3
      t535 = -0.432D3 * t41 + 0.640D3 * t51 + 0.108D3 * t61 + 0.640D3 * 
     #t83 + 0.640D3 * t88 - 0.144D3 * t92 + 0.542D3 * t103 + 0.72D2 * t1
     #13 - 0.444D3 * t121 - 0.432D3 * t86 * t72 * t81 + 0.256D3 * t193 *
     # t202 * t528 * x2 - 0.72D2 * t136 - 0.64D2 * t141
      t551 = 0.16D2 * t152 + 0.264D3 * t156 - 0.256D3 * t160 + 0.62D2 * 
     #t164 - 0.24D2 * t168 + 0.72D2 * t180 + 0.64D2 * t134 * t166 * x3 +
     # 0.30D2 * t191 - t199 + 0.288D3 * t205 - 0.108D3 * t210 - 0.144D3 
     #* t222 + 0.256D3 * t227 + 0.256D3 * t231
      t561 = t118 * t38
      t565 = t21 * t71
      t581 = 0.256D3 * t237 + 0.640D3 * t241 - t246 - 0.108D3 * t250 + 0
     #.592D3 * t256 - 0.72D2 * t261 - 0.256D3 * t266 + 0.64D2 * t352 * t
     #313 - 0.128D3 * t411 * t81 * t561 + 0.64D2 * t364 * t81 * t565 + 0
     #.64D2 * t369 * t55 * t71 * t73 - 0.128D3 * t364 * t21 * t38 * t73 
     #- 0.288D3 * t436 * t437 * t130 * t24
      t612 = t43 * t187
      t613 = t21 * t130
      t617 = t139 * t200
      t621 = t21 * x1
      t626 = -0.256D3 * t391 * t280 * t561 - 0.136D3 * t364 * t283 * t56
     #5 + 0.216D3 * t411 * t283 * t561 + 0.64D2 * t402 * t269 + 0.216D3 
     #* t270 - t276 + 0.256D3 * t8 * t77 * t10 * t23 * t21 * t38 * x2 - 
     #0.256D3 * t43 * t57 * t10 * t23 * t118 * t38 * x3 - 0.256D3 * t201
     # * t202 * t80 * x3 + 0.64D2 * t612 * t613 * t48 - 0.128D3 * t617 *
     # t22 * t48 + 0.216D3 * t617 * t621 * t528 - 0.72D2 * t281 - t285
      t653 = -0.128D3 * t612 * t22 * t308 + 0.64D2 * t17 * t613 * t308 -
     # 0.544D3 * t292 + 0.604D3 * t295 - 0.432D3 * t17 * t613 * t23 * x1
     # * t38 + 0.108D3 * t299 - 0.256D3 * t201 * t202 * t94 * x3 + 0.54D
     #2 * t306 + 0.256D3 * t311 - 0.496D3 * t314 + 0.216D3 * t316 - 0.44
     #4D3 * t320 + 0.54D2 * t325
      t671 = -0.136D3 * t612 * t621 * t70 * t130 - 0.432D3 * t328 - 0.49
     #6D3 * t330 - 0.640D3 * t335 + 0.144D3 * t345 + 0.608D3 * t349 + 0.
     #344D3 * t354 + 0.56D2 * t362 + 0.216D3 * t367 + 0.216D3 * t372 - 0
     #.256D3 * t377 + 0.54D2 * t383 + 0.56D2 * t389 + 0.54D2 * t394
      t684 = 0.62D2 * t400 - 0.24D2 * t405 - 0.496D3 * t413 - 0.496D3 * 
     #t417 - 0.256D3 * t422 + 0.344D3 * t424 + 0.144D3 * t426 - 0.72D2 *
     # t430 + 0.288D3 * t433 - t441 - t445 + 0.72D2 * t448 + 0.30D2 * t4
     #53
      t700 = -t465 + 0.72D2 * t471 - 0.640D3 * t478 + 0.604D3 * t481 - 0
     #.544D3 * t484 + 0.542D3 * t486 + 0.264D3 * t492 - 0.64D2 * t495 + 
     #0.16D2 * t499 + 0.592D3 * t502 - t507 + 0.608D3 * t508 + 0.64D2 * 
     #t411 * t73 * t118 - 0.288D3 * t460 * t456
      rrgg2qqbarh51J2 = -(0.2D1 * wd * (t128 + t182 + t247 + t301 + t351
     # + t419 + t473 + t510) + wd * (t535 + t551 + t581 + t626 + t653 + 
     #t671 + t684 + t700)) * nf / t1 / z / 0.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t8 = t3 * t6 * t4
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 ** 2
      t12 = 0.1D1 - x3
      t13 = t11 * t12
      t14 = t8 * t13
      t15 = x3 ** 2
      t18 = z + x1 * t4
      t19 = 0.1D1 / t18
      t20 = x1 * t19
      t25 = t3 * t6 * t5
      t26 = t25 * t13
      t27 = x2 ** 2
      t29 = x1 ** 2
      t30 = t18 ** 2
      t31 = 0.1D1 / t30
      t32 = t29 * t31
      t34 = t26 * x3 * t27 * t32
      t36 = t10 * t9
      t37 = 0.1D1 - x2
      t40 = z + x1 * t37 * t4
      t41 = t36 * t40
      t42 = t8 * t41
      t44 = 0.1D1 / t30 / t18
      t45 = t44 * x3
      t49 = x2 * x3
      t51 = cos(x4 * 0.3141592653589793D1)
      t52 = x3 * t37
      t56 = Sqrt(t52 * t18 * x2 * t12)
      t58 = 0.2D1 * t51 * t56
      t59 = t12 * t37 * t18 + t49 + t58
      t60 = t59 ** 2
      t64 = 0.144D3 * t42 * t45 * t12 * t29 * t60
      t65 = t3 * t6
      t66 = t10 * t40
      t67 = t65 * t66
      t68 = t44 * t29
      t70 = x2 * t12
      t71 = t52 * t18 + t70 - t58
      t72 = t71 * x2
      t74 = t67 * t68 * t72
      t76 = t8 * t66
      t77 = t30 ** 2
      t78 = 0.1D1 / t77
      t79 = t29 * x1
      t80 = t78 * t79
      t81 = t71 ** 2
      t84 = t76 * t80 * t81 * x2
      t86 = t5 * t4
      t87 = t3 * t86
      t88 = t9 * t40
      t90 = t71 * t59
      t92 = t87 * t88 * t68 * t90
      t94 = t65 * t88
      t95 = t81 * t59
      t97 = t94 * t80 * t95
      t99 = t29 * t19
      t102 = x3 * t9
      t104 = t87 * t99 * t71 * z * t102
      t105 = 0.128D3 * t104
      t106 = t79 * t31
      t107 = t65 * t106
      t110 = t107 * t72 * t9 * z
      t112 = t10 * t12
      t113 = t87 * t112
      t114 = t40 * t31
      t115 = x1 * z
      t118 = t113 * t114 * t115 * t59
      t119 = 0.128D3 * t118
      t120 = t29 ** 2
      t121 = t120 * t78
      t122 = t8 * t121
      t123 = t71 * t60
      t124 = x2 * t9
      t128 = t12 ** 2
      t129 = t11 * t128
      t130 = t8 * t129
      t131 = t49 * t20
      t134 = -0.28D2 * t14 * t15 * x2 * t20 - 0.36D2 * t34 + t64 - 0.400
     #D3 * t74 + 0.182D3 * t84 + 0.584D3 * t92 - 0.328D3 * t97 - t105 - 
     #0.128D3 * t110 - t119 - 0.28D2 * t122 * t123 * t124 + 0.56D2 * t13
     #0 * t131
      t135 = t36 * t12
      t136 = t65 * t135
      t137 = t136 * t131
      t141 = t14 * t114 * t49 * x1
      t145 = t76 * t80 * t90 * x2
      t148 = t107 * t90 * t102
      t150 = t120 * t44
      t151 = t8 * t150
      t152 = t90 * t124
      t153 = t151 * t152
      t155 = t36 * t128
      t157 = x1 * t59
      t158 = t114 * t157
      t159 = t65 * t155 * t158
      t161 = t8 * t106
      t162 = t10 * t15
      t165 = 0.144D3 * t161 * t90 * t162
      t166 = t128 * t12
      t170 = t3 * t5
      t171 = t10 * t128
      t172 = t170 * t171
      t174 = t3 * t4
      t175 = t9 * t12
      t177 = 0.144D3 * t174 * t175
      t178 = t128 ** 2
      t182 = t65 * t120
      t183 = t78 * t81
      t184 = t183 * t60
      t188 = t8 * t120 * x1
      t189 = t60 * t59
      t191 = t78 * t189 * t71
      t193 = 0.144D3 * t188 * t191
      t194 = 0.112D3 * t137 - 0.96D2 * t141 + 0.530D3 * t145 - 0.96D2 * 
     #t148 - 0.96D2 * t153 - 0.148D3 * t159 + t165 + 0.144D3 * t87 * t36
     # * t166 + 0.216D3 * t172 + t177 + 0.36D2 * t65 * t11 * t178 + 0.10
     #8D3 * t182 * t184 + t193
      t196 = t81 * t71
      t197 = t78 * t196
      t198 = t197 * t59
      t202 = t31 * t71
      t203 = t202 * t59
      t204 = t170 * t29 * t203
      t206 = t87 * t79
      t207 = t44 * t81
      t208 = t207 * t59
      t211 = t206 * t203
      t213 = t65 * x1
      t214 = t155 * x3
      t215 = t213 * t214
      t218 = t112 * x3
      t219 = t87 * x1 * t218
      t221 = t135 * t15
      t222 = t213 * t221
      t227 = t12 * t40
      t229 = t170 * t10 * t227 * t19
      t231 = t87 * t36
      t232 = t128 * t40
      t234 = t231 * t232 * t19
      t236 = t65 * t11
      t239 = t236 * t166 * t40 * t19
      t241 = t44 * t71
      t242 = t241 * t60
      t243 = t182 * t242
      t247 = -0.108D3 * t182 * t198 - 0.216D3 * t204 - 0.288D3 * t206 * 
     #t208 + 0.880D3 * t211 - 0.328D3 * t215 + 0.584D3 * t219 - 0.430D3 
     #* t222 - 0.36D2 * t182 * t191 - 0.312D3 * t229 - 0.240D3 * t234 + 
     #0.12D2 * t239 - 0.748D3 * t243 + 0.144D3 * t206 * t242
      t249 = 0.144D3 * t188 * t198
      t250 = t182 * t208
      t252 = t11 * t40
      t253 = t8 * t252
      t254 = t31 * x3
      t258 = t253 * t254 * t128 * x1 * t59
      t262 = t136 * t114 * t157 * x3
      t265 = t40 * t44
      t269 = t8 * t135 * t265 * t29 * t59 * x2
      t272 = t29 * t60
      t273 = t265 * t272
      t274 = t65 * t112 * t273
      t277 = t27 * t10
      t279 = t25 * t121 * t90 * t277
      t281 = t79 * t44
      t283 = t65 * t281 * t152
      t289 = t94 * t80 * t123
      t294 = x2 * t19
      t296 = t8 * t29 * t36 * t12 * x3 * t294
      t298 = t27 * t29
      t299 = t265 * t298
      t300 = t26 * t299
      t302 = x2 * x1
      t305 = t136 * t114 * t302 * z
      t307 = t249 - 0.16D2 * t250 + 0.288D3 * t258 - 0.96D2 * t262 - 0.9
     #4D2 * t269 - 0.47D2 * t274 - 0.36D2 * t279 + 0.112D3 * t283 + 0.56
     #D2 * t122 * t95 * t124 - 0.430D3 * t289 + 0.530D3 * t296 - 0.47D2 
     #* t300 - 0.128D3 * t305
      t310 = t114 * t302
      t311 = t136 * t310
      t313 = t130 * t310
      t317 = 0.144D3 * t174 * t20 * t71
      t326 = t170 * t32 * t81
      t329 = t87 * t106 * t81
      t331 = t81 ** 2
      t339 = t65 * t150 * t196
      t342 = t170 * t99 * t71
      t344 = x1 * t10
      t346 = t87 * t344 * t128
      t348 = 0.128D3 * t311 - 0.148D3 * t313 + t317 - 0.108D3 * t65 * t1
     #1 * t166 * x3 + 0.108D3 * t65 * t129 * t15 + 0.216D3 * t326 - 0.24
     #0D3 * t329 + 0.36D2 * t65 * t121 * t331 + 0.144D3 * t87 * t281 * t
     #196 + 0.12D2 * t339 - 0.312D3 * t342 + 0.328D3 * t346
      t349 = x1 * t36
      t351 = t65 * t349 * t166
      t353 = x1 * t9
      t355 = t170 * t353 * t12
      t359 = t15 * x3
      t363 = t170 * t218
      t369 = t161 * t71 * t10 * t49
      t371 = t25 * t41
      t374 = t371 * t80 * t71 * t27
      t376 = t8 * t29
      t378 = t376 * t155 * t294
      t382 = t236 * t227 * t19 * t15
      t387 = t25 * t11 * t128 * t27 * t32
      t389 = t8 * t36
      t392 = t389 * t12 * t27 * t32
      t394 = t25 * t79
      t395 = t27 * t31
      t397 = t394 * t135 * t395
      t399 = -0.10D2 * t351 - 0.56D2 * t355 - 0.288D3 * t87 * t214 - 0.3
     #6D2 * t65 * t13 * t359 - 0.216D3 * t363 + 0.144D3 * t87 * t221 - 0
     #.94D2 * t369 - 0.192D3 * t374 + 0.182D3 * t378 - 0.748D3 * t382 + 
     #0.36D2 * t387 + 0.144D3 * t392 - 0.192D3 * t397
      t401 = t19 * x3
      t403 = t231 * t227 * t401
      t413 = t65 * t36 * t128 * x2 * t20
      t416 = t236 * t232 * t401
      t418 = t87 * t10
      t420 = t418 * t70 * t20
      t423 = t3 * t6 * t86
      t424 = t423 * t11
      t425 = t27 * x2
      t428 = t424 * t12 * t425 * t281
      t430 = t65 * t79
      t433 = t430 * t31 * t81 * t102
      t435 = t8 * t120
      t436 = t207 * t124
      t437 = t435 * t436
      t439 = t87 * t9
      t442 = t439 * t265 * t29 * t81
      t444 = t25 * t120
      t445 = t241 * t277
      t446 = t444 * t445
      t448 = t423 * t120
      t450 = t425 * t36
      t452 = t448 * t78 * t71 * t450
      t454 = t65 * t29
      t455 = t19 * t71
      t457 = t454 * t455 * t162
      t459 = t65 * t9
      t460 = t40 * t78
      t463 = t459 * t460 * t79 * t196
      t465 = 0.880D3 * t403 - 0.28D2 * t8 * t11 * t166 * x2 * t20 - 0.11
     #2D3 * t413 - 0.16D2 * t416 - 0.168D3 * t420 - 0.28D2 * t428 - 0.14
     #8D3 * t433 - 0.148D3 * t437 + 0.328D3 * t442 - 0.47D2 * t446 - 0.2
     #8D2 * t452 - 0.47D2 * t457 - 0.10D2 * t463
      t466 = t170 * t9
      t469 = t466 * t114 * x1 * t71
      t474 = t87 * t29
      t476 = t474 * t455 * t102
      t478 = t202 * t124
      t479 = t430 * t478
      t481 = t8 * t79
      t482 = t481 * t445
      t484 = t474 * t478
      t486 = t430 * t436
      t489 = t444 * t183 * t277
      t492 = t454 * t112 * t294
      t495 = t8 * t11 * t9
      t496 = t40 * t19
      t500 = 0.144D3 * t495 * t496 * t359 * t12
      t504 = 0.144D3 * t495 * t496 * x3 * t166
      t505 = t113 * t158
      t508 = t151 * t95 * t102
      t510 = -0.56D2 * t469 - 0.28D2 * t435 * t197 * t124 + 0.128D3 * t4
     #76 + 0.128D3 * t479 + 0.144D3 * t482 - 0.168D3 * t484 - 0.112D3 * 
     #t486 + 0.36D2 * t489 - 0.400D3 * t492 + t500 + t504 + 0.128D3 * t5
     #05 + 0.288D3 * t508
      t528 = t67 * t68 * t90 * x3
      t530 = t65 * t41
      t531 = t31 * t15
      t535 = 0.72D2 * t34 - t64 + 0.288D3 * t74 - 0.108D3 * t84 - 0.544D
     #3 * t92 + 0.604D3 * t97 + 0.256D3 * t104 + 0.256D3 * t110 + 0.256D
     #3 * t118 - 0.144D3 * t137 + 0.640D3 * t141 - 0.256D3 * t528 + 0.64
     #D2 * t530 * t531 * t157
      t536 = t87 * t66
      t538 = t536 * t254 * t157
      t548 = t71 * x3
      t551 = t42 * t68 * t548 * x2
      t560 = t59 * x2
      t562 = t8 * t79 * t10 * t12 * t31 * t560
      t567 = t59 * x3
      t569 = t65 * t29 * t10 * t12 * t19 * t567
      t572 = -0.128D3 * t538 - 0.432D3 * t253 * t531 * t12 * x1 * t59 - 
     #0.432D3 * t151 * t123 * t102 + 0.256D3 * t551 - 0.444D3 * t145 + 0
     #.640D3 * t148 + 0.640D3 * t153 + 0.216D3 * t159 - t165 + 0.256D3 *
     # t562 - 0.256D3 * t569 - 0.72D2 * t172 - t177 - t193
      t585 = t474 * t124 * t19
      t589 = 0.72D2 * t204 - 0.640D3 * t211 + 0.604D3 * t215 - 0.544D3 *
     # t219 + 0.542D3 * t222 + 0.264D3 * t229 - 0.64D2 * t234 + 0.16D2 *
     # t239 + 0.592D3 * t243 - t249 + 0.608D3 * t250 + 0.64D2 * t585 - 0
     #.288D3 * t188 * t184
      t590 = t418 * t310
      t592 = t19 * t59
      t594 = t474 * t102 * t592
      t596 = t31 * t60
      t602 = t435 * t44 * t60 * t124
      t606 = t430 * t31 * t59 * t124
      t613 = t454 * t171 * t592
      t619 = t474 * t175 * t592
      t621 = t466 * t158
      t627 = 0.64D2 * t590 - 0.128D3 * t594 + 0.64D2 * t430 * t102 * t59
     #6 + 0.64D2 * t602 - 0.128D3 * t606 - 0.288D3 * t495 * t496 * t15 *
     # t128 - 0.256D3 * t613 - 0.136D3 * t430 * t175 * t596 + 0.216D3 * 
     #t619 + 0.64D2 * t621 - 0.432D3 * t258 + 0.640D3 * t262 + 0.108D3 *
     # t269 + 0.54D2 * t274
      t635 = t67 * t68 * t81 * x3
      t645 = 0.72D2 * t279 - 0.144D3 * t283 + 0.542D3 * t289 - 0.256D3 *
     # t635 - 0.444D3 * t296 + 0.54D2 * t300 + 0.256D3 * t305 - 0.496D3 
     #* t311 + 0.216D3 * t313 - t317 - 0.72D2 * t326 - 0.64D2 * t329 + 0
     #.16D2 * t339
      t652 = t170 * t353 * x3
      t662 = 0.264D3 * t342 - 0.256D3 * t346 + 0.62D2 * t351 - 0.24D2 * 
     #t355 + 0.72D2 * t363 + 0.64D2 * t652 + 0.108D3 * t369 + 0.30D2 * t
     #374 - 0.108D3 * t378 + 0.592D3 * t382 - 0.72D2 * t387 - 0.256D3 * 
     #t392 + 0.30D2 * t397 - 0.640D3 * t403
      t677 = 0.144D3 * t413 + 0.608D3 * t416 + 0.344D3 * t420 + 0.56D2 *
     # t428 + 0.216D3 * t433 + 0.216D3 * t437 - 0.256D3 * t442 + 0.54D2 
     #* t446 + 0.56D2 * t452 + 0.54D2 * t457 + 0.62D2 * t463 - 0.24D2 * 
     #t469 - 0.496D3 * t476
      t686 = t31 * x1
      t688 = t536 * t686 * t548
      t695 = t530 * t254 * t302
      t698 = t253 * t531 * t302
      t700 = -0.496D3 * t479 - 0.256D3 * t482 + 0.344D3 * t484 + 0.144D3
     # * t486 - 0.72D2 * t489 + 0.288D3 * t492 - t500 - t504 - 0.496D3 *
     # t505 - 0.432D3 * t508 + 0.216D3 * t688 - 0.136D3 * t530 * t686 * 
     #t71 * t15 - 0.128D3 * t695 + 0.64D2 * t698
      t716 = t31 * x2
      t717 = z ** 2
      t727 = t79 * x2
      t734 = -0.74288D5 * t74 - 0.29128D5 * t84 + 0.74208D5 * t92 - 0.27
     #920D5 * t97 - t105 - 0.512D3 * t110 - t119 + 0.384D3 * t42 * t44 *
     # t27 * t29 * z - 0.384D3 * t536 * t716 * x1 * t717 + 0.288D3 * t25
     # * t252 * t45 * t298 - 0.256D3 * t141 - 0.384D3 * t65 * t727 * t9 
     #* t31 * z * t59
      t748 = -0.25264D5 * t528 - 0.64D2 * t538 + 0.25248D5 * t551 - 0.53
     #6D3 * t145 - 0.256D3 * t153 + 0.25248D5 * t562 - 0.25264D5 * t569 
     #+ 0.128D3 * t211 - 0.27920D5 * t215 + 0.74208D5 * t219 - 0.28208D5
     # * t222 - 0.64D2 * t229 + 0.128D3 * t234
      t766 = -0.64D2 * t239 - 0.64D2 * t243 - 0.128D3 * t250 + 0.128D3 *
     # t585 - 0.96D2 * t481 * t277 * t31 - 0.112D3 * t448 * t450 * t44 +
     # 0.128D3 * t590 - 0.64D2 * t594 - 0.128D3 * t602 - 0.64D2 * t606 -
     # 0.25296D5 * t613 + 0.80D2 * t619
      t813 = 0.128D3 * t621 - 0.96D2 * t389 * t299 - 0.112D3 * t424 * t4
     #60 * t425 * t79 - 0.128D3 * t170 * x2 * t353 * t19 * t717 * z - 0.
     #384D3 * t474 * t124 * t19 * t717 + 0.384D3 * t481 * t277 * t31 * z
     # + 0.48D2 * t376 * t36 * t15 * t294 + 0.48D2 * t394 * t36 * x3 * t
     #395 - 0.192D3 * t454 * t10 * x3 * t294 - 0.96D2 * t439 * t273 + 0.
     #16D2 * t459 * t460 * t79 * t189 + 0.288D3 * t444 * t44 * t59 * t27
     #7 + 0.32D2 * t454 * t162 * t592
      t828 = -0.8D1 * t269 - 0.4D1 * t274 - 0.28208D5 * t289 - 0.25296D5
     # * t635 - 0.536D3 * t296 + 0.252D3 * t300 - 0.512D3 * t305 + 0.8D1
     # * t311 - 0.128D3 * t313 + 0.128D3 * t329 - 0.64D2 * t339 - 0.64D2
     # * t342
      t859 = 0.74320D5 * t346 + 0.304D3 * t351 - 0.32D2 * t355 + 0.128D3
     # * t652 - 0.96D2 * t87 * t344 * t15 + 0.16D2 * t65 * t349 * t359 -
     # 0.8D1 * t369 + 0.28792D5 * t374 - 0.384D3 * t530 * t716 * t115 * 
     #x3 + 0.48D2 * t371 * t80 * t59 * t27 - 0.192D3 * t67 * t68 * t560 
     #+ 0.48D2 * t76 * t80 * t60 * x2 + 0.32D2 * t67 * t45 * t272
      t883 = 0.64D2 * t42 * t44 * x2 * t29 * x3 * t59 + 0.64D2 * t8 * t7
     #27 * t10 * t31 * t567 - 0.29128D5 * t378 - 0.64D2 * t382 + 0.288D3
     # * t392 + 0.28792D5 * t397 + 0.128D3 * t403 - 0.128D3 * t416 - 0.1
     #44D3 * t420 - 0.144D3 * t428 - 0.128D3 * t437 + 0.74320D5 * t442 +
     # 0.252D3 * t446
      t897 = -0.144D3 * t452 - 0.4D1 * t457 + 0.304D3 * t463 - 0.32D2 * 
     #t469 + 0.8D1 * t476 + 0.8D1 * t479 + 0.288D3 * t482 - 0.144D3 * t4
     #84 - 0.74288D5 * t492 + 0.8D1 * t505 + 0.80D2 * t688 - 0.64D2 * t6
     #95 - 0.128D3 * t698
      rrgg2qqbarh51J3 = -(0.3D1 * wd * (t134 + t194 + t247 + t307 + t348
     # + t399 + t465 + t510) + 0.2D1 * wd * (t535 + t572 + t589 + t627 +
     # t645 + t662 + t677 + t700) + wd * (t734 + t748 + t766 + t813 + t8
     #28 + t859 + t883 + t897)) * nf / t1 / z / 0.3141592653589793D1 / 0
     #.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 - x3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t17 = x1 ** 2
      t18 = t7 * t17
      t19 = t9 * t11
      t21 = z + x1 * t4
      t22 = 0.1D1 / t21
      t23 = x2 * t22
      t25 = t18 * t19 * t23
      t28 = t3 * t6 * t4
      t30 = t28 * t10 * t8
      t31 = 0.1D1 - x2
      t34 = z + x1 * t31 * t4
      t35 = t34 * t22
      t36 = x3 ** 2
      t37 = t36 * x3
      t41 = 0.144D3 * t30 * t35 * t37 * t11
      t42 = t11 * t12
      t46 = 0.144D3 * t30 * t35 * x3 * t42
      t48 = t3 * t6 * t5
      t49 = t17 ** 2
      t50 = t21 ** 2
      t51 = t50 ** 2
      t52 = 0.1D1 / t51
      t53 = t49 * t52
      t55 = t31 * x3
      t57 = x2 * t11
      t59 = cos(x4 * 0.3141592653589793D1)
      t63 = Sqrt(t55 * t21 * x2 * t11)
      t65 = 0.2D1 * t59 * t63
      t66 = t55 * t21 + t57 - t65
      t69 = x2 * x3
      t70 = t11 * t31 * t21 + t69 + t65
      t71 = t66 * t70
      t72 = x2 ** 2
      t73 = t72 * t9
      t75 = t48 * t53 * t71 * t73
      t77 = t17 * x1
      t79 = 0.1D1 / t50 / t21
      t80 = t77 * t79
      t82 = x2 * t8
      t83 = t71 * t82
      t84 = t7 * t80 * t83
      t86 = t3 * t4
      t87 = t8 * t11
      t89 = 0.144D3 * t86 * t87
      t90 = t10 * t12
      t91 = t28 * t90
      t92 = x1 * t22
      t93 = t69 * t92
      t96 = t9 * t8
      t97 = t96 * t11
      t98 = t7 * t97
      t99 = t98 * t93
      t101 = t10 * t11
      t102 = t28 * t101
      t103 = 0.1D1 / t50
      t104 = t34 * t103
      t107 = t102 * t104 * t69 * x1
      t109 = t96 * t34
      t110 = t28 * t109
      t111 = t79 * x3
      t113 = t70 ** 2
      t117 = 0.144D3 * t110 * t111 * t11 * t17 * t113
      t118 = t9 * t34
      t119 = t7 * t118
      t120 = t79 * t17
      t121 = t66 * x2
      t123 = t119 * t120 * t121
      t125 = 0.36D2 * t7 * t10 * t13 - 0.400D3 * t25 + t41 + t46 - 0.36D
     #2 * t75 + 0.112D3 * t84 + t89 + 0.56D2 * t91 * t93 + 0.112D3 * t99
     # - 0.96D2 * t107 + t117 - 0.400D3 * t123
      t126 = t28 * t118
      t127 = t52 * t77
      t128 = t66 ** 2
      t131 = t126 * t127 * t128 * x2
      t133 = t5 * t4
      t134 = t3 * t133
      t135 = t134 * t19
      t136 = x1 * t70
      t137 = t104 * t136
      t138 = t135 * t137
      t140 = t49 * t79
      t141 = t28 * t140
      t142 = t70 * t128
      t143 = t8 * x3
      t145 = t141 * t142 * t143
      t151 = t48 * t101
      t153 = t17 * t103
      t155 = t151 * x3 * t72 * t153
      t157 = t8 * t34
      t158 = t7 * t157
      t159 = t66 * t113
      t161 = t158 * t127 * t159
      t165 = t134 * t157 * t120 * t71
      t168 = t158 * t127 * t142
      t170 = t3 * t5
      t171 = t9 * t12
      t172 = t170 * t171
      t174 = t134 * t17
      t175 = t103 * t66
      t176 = t175 * t82
      t177 = t174 * t176
      t179 = t7 * t77
      t180 = t79 * t128
      t181 = t180 * t82
      t182 = t179 * t181
      t184 = t48 * t49
      t185 = t52 * t128
      t187 = t184 * t185 * t73
      t190 = t34 * t79
      t194 = t28 * t97 * t190 * t17 * t70 * x2
      t196 = 0.182D3 * t131 + 0.128D3 * t138 + 0.288D3 * t145 - 0.28D2 *
     # t102 * t36 * x2 * t92 - 0.36D2 * t155 - 0.430D3 * t161 + 0.584D3 
     #* t165 - 0.328D3 * t168 + 0.216D3 * t172 - 0.168D3 * t177 - 0.112D
     #3 * t182 + 0.36D2 * t187 - 0.94D2 * t194
      t199 = t17 * t113
      t200 = t190 * t199
      t201 = t7 * t19 * t200
      t203 = t77 * t103
      t204 = t28 * t203
      t207 = t204 * t66 * t9 * t69
      t209 = t48 * t109
      t212 = t209 * t127 * t66 * t72
      t215 = t3 * t6 * t133
      t216 = t215 * t10
      t217 = t72 * x2
      t220 = t216 * t11 * t217 * t80
      t224 = t179 * t103 * t128 * t143
      t226 = t28 * t49
      t227 = t226 * t181
      t229 = t134 * t8
      t232 = t229 * t190 * t17 * t128
      t234 = t79 * t66
      t235 = t234 * t73
      t236 = t184 * t235
      t238 = t215 * t49
      t240 = t217 * t96
      t242 = t238 * t52 * t66 * t240
      t244 = t22 * t66
      t245 = t9 * t36
      t247 = t18 * t244 * t245
      t249 = t7 * t8
      t250 = t34 * t52
      t251 = t128 * t66
      t254 = t249 * t250 * t77 * t251
      t256 = t170 * t8
      t259 = t256 * t104 * x1 * t66
      t261 = t52 * t251
      t265 = -0.47D2 * t201 - 0.94D2 * t207 - 0.192D3 * t212 - 0.28D2 * 
     #t220 - 0.148D3 * t224 - 0.148D3 * t227 + 0.328D3 * t232 - 0.47D2 *
     # t236 - 0.28D2 * t242 - 0.47D2 * t247 - 0.10D2 * t254 - 0.56D2 * t
     #259 - 0.28D2 * t226 * t261 * t82
      t267 = t174 * t244 * t143
      t269 = t179 * t176
      t271 = t28 * t77
      t272 = t271 * t235
      t274 = t28 * t17
      t275 = t96 * t12
      t277 = t274 * t275 * t23
      t279 = t7 * t10
      t280 = t11 * t34
      t283 = t279 * t280 * t22 * t36
      t288 = t48 * t10 * t12 * t72 * t153
      t290 = t28 * t96
      t293 = t290 * t11 * t72 * t153
      t295 = t48 * t77
      t296 = t72 * t103
      t298 = t295 * t97 * t296
      t300 = t134 * t96
      t301 = t22 * x3
      t303 = t300 * t280 * t301
      t313 = t7 * t96 * t12 * x2 * t92
      t315 = t12 * t34
      t317 = t279 * t315 * t301
      t319 = t134 * t9
      t321 = t319 * t57 * t92
      t323 = 0.128D3 * t267 + 0.128D3 * t269 + 0.144D3 * t272 + 0.182D3 
     #* t277 - 0.748D3 * t283 + 0.36D2 * t288 + 0.144D3 * t293 - 0.192D3
     # * t298 + 0.880D3 * t303 - 0.28D2 * t28 * t10 * t42 * x2 * t92 - 0
     #.112D3 * t313 - 0.16D2 * t317 - 0.168D3 * t321
      t329 = t7 * t49
      t330 = t185 * t113
      t334 = t28 * t49 * x1
      t335 = t113 * t70
      t337 = t52 * t335 * t66
      t339 = 0.144D3 * t334 * t337
      t340 = t261 * t70
      t344 = t175 * t70
      t345 = t170 * t17 * t344
      t347 = t134 * t77
      t348 = t180 * t70
      t351 = t347 * t344
      t353 = t7 * x1
      t354 = t275 * x3
      t355 = t353 * t354
      t358 = t19 * x3
      t359 = t134 * x1 * t358
      t361 = t97 * t36
      t362 = t353 * t361
      t368 = t170 * t9 * t280 * t22
      t370 = 0.144D3 * t134 * t96 * t42 + 0.108D3 * t329 * t330 + t339 -
     # 0.108D3 * t329 * t340 - 0.216D3 * t345 - 0.288D3 * t347 * t348 + 
     #0.880D3 * t351 - 0.328D3 * t355 + 0.584D3 * t359 - 0.430D3 * t362 
     #- 0.36D2 * t329 * t337 - 0.312D3 * t368
      t372 = t300 * t315 * t22
      t376 = t279 * t42 * t34 * t22
      t378 = t234 * t113
      t379 = t329 * t378
      t384 = 0.144D3 * t334 * t340
      t385 = t329 * t348
      t395 = t170 * t153 * t128
      t398 = t134 * t203 * t128
      t400 = t128 ** 2
      t408 = t7 * t140 * t251
      t410 = -0.240D3 * t372 + 0.12D2 * t376 - 0.748D3 * t379 + 0.144D3 
     #* t347 * t378 + t384 - 0.16D2 * t385 - 0.108D3 * t7 * t10 * t42 * 
     #x3 + 0.108D3 * t7 * t90 * t36 + 0.216D3 * t395 - 0.240D3 * t398 + 
     #0.36D2 * t7 * t53 * t400 + 0.144D3 * t134 * t80 * t251 + 0.12D2 * 
     #t408
      t412 = t17 * t22
      t414 = t170 * t412 * t66
      t416 = x1 * t9
      t418 = t134 * t416 * t12
      t420 = x1 * t96
      t422 = t7 * t420 * t42
      t424 = x1 * t8
      t426 = t170 * t424 * t11
      t433 = t170 * t358
      t437 = x2 * x1
      t438 = t104 * t437
      t439 = t91 * t438
      t445 = t28 * t17 * t96 * t11 * x3 * t23
      t447 = t72 * t17
      t448 = t190 * t447
      t449 = t151 * t448
      t453 = t98 * t104 * t437 * z
      t455 = t98 * t438
      t457 = -0.312D3 * t414 + 0.328D3 * t418 - 0.10D2 * t422 - 0.56D2 *
     # t426 - 0.288D3 * t134 * t354 - 0.36D2 * t7 * t101 * t37 - 0.216D3
     # * t433 + 0.144D3 * t134 * t361 - 0.148D3 * t439 + 0.530D3 * t445 
     #- 0.47D2 * t449 - 0.128D3 * t453 + 0.128D3 * t455
      t460 = 0.144D3 * t86 * t92 * t66
      t461 = t10 * t34
      t462 = t28 * t461
      t463 = t103 * x3
      t467 = t462 * t463 * t12 * x1 * t70
      t471 = t98 * t104 * t136 * x3
      t476 = t134 * t412 * t66 * z * t143
      t477 = 0.128D3 * t476
      t478 = t7 * t203
      t481 = t478 * t121 * t8 * z
      t483 = x1 * z
      t486 = t135 * t104 * t483 * t70
      t487 = 0.128D3 * t486
      t488 = t28 * t53
      t494 = t126 * t127 * t71 * x2
      t497 = t478 * t71 * t143
      t499 = t141 * t83
      t502 = t7 * t275 * t137
      t506 = 0.144D3 * t204 * t71 * t245
      t510 = t460 + 0.288D3 * t467 - 0.96D2 * t471 - t477 - 0.128D3 * t4
     #81 - t487 + 0.56D2 * t488 * t142 * t82 + 0.530D3 * t494 - 0.96D2 *
     # t497 - 0.96D2 * t499 - 0.148D3 * t502 + t506 - 0.28D2 * t488 * t1
     #59 * t82
      t525 = 0.288D3 * t25 - t41 - t46 + 0.72D2 * t75 - 0.144D3 * t84 - 
     #t89 - 0.144D3 * t99 + 0.640D3 * t107 - t117 + 0.288D3 * t123 - 0.1
     #08D3 * t131 - 0.496D3 * t138 - 0.432D3 * t145
      t527 = t7 * t109
      t529 = t527 * t463 * t437
      t531 = t103 * t36
      t533 = t462 * t531 * t437
      t535 = t134 * t118
      t536 = t103 * x1
      t537 = t66 * x3
      t539 = t535 * t536 * t537
      t548 = t70 * x2
      t550 = t28 * t77 * t9 * t11 * t103 * t548
      t555 = t70 * x3
      t557 = t7 * t17 * t9 * t11 * t22 * t555
      t566 = 0.72D2 * t155 - 0.128D3 * t529 + 0.64D2 * t533 + 0.216D3 * 
     #t539 - 0.136D3 * t527 * t536 * t66 * t36 + 0.256D3 * t550 - 0.256D
     #3 * t557 + 0.542D3 * t161 - 0.544D3 * t165 + 0.604D3 * t168 - 0.72
     #D2 * t172 + 0.344D3 * t177 + 0.144D3 * t182 - 0.72D2 * t187
      t581 = 0.108D3 * t194 + 0.54D2 * t201 + 0.108D3 * t207 + 0.30D2 * 
     #t212 + 0.56D2 * t220 + 0.216D3 * t224 + 0.216D3 * t227 - 0.256D3 *
     # t232 + 0.54D2 * t236 + 0.56D2 * t242 + 0.54D2 * t247 + 0.62D2 * t
     #254 - 0.24D2 * t259
      t595 = -0.496D3 * t267 - 0.496D3 * t269 - 0.256D3 * t272 - 0.108D3
     # * t277 + 0.592D3 * t283 - 0.72D2 * t288 - 0.256D3 * t293 + 0.30D2
     # * t298 - 0.640D3 * t303 + 0.144D3 * t313 + 0.608D3 * t317 + 0.344
     #D3 * t321 - t339 + 0.72D2 * t345
      t608 = t174 * t82 * t22
      t614 = t119 * t120 * t71 * x3
      t616 = -0.640D3 * t351 + 0.604D3 * t355 - 0.544D3 * t359 + 0.542D3
     # * t362 + 0.264D3 * t368 - 0.64D2 * t372 + 0.16D2 * t376 + 0.592D3
     # * t379 - t384 + 0.608D3 * t385 + 0.64D2 * t608 - 0.288D3 * t334 *
     # t330 - 0.256D3 * t614
      t626 = t170 * t424 * x3
      t633 = -0.72D2 * t395 - 0.64D2 * t398 + 0.16D2 * t408 + 0.264D3 * 
     #t414 - 0.256D3 * t418 + 0.62D2 * t422 - 0.24D2 * t426 + 0.72D2 * t
     #433 + 0.64D2 * t626 + 0.216D3 * t439 - 0.444D3 * t445 + 0.54D2 * t
     #449 + 0.256D3 * t453 - 0.496D3 * t455
      t635 = t319 * t438
      t637 = t22 * t70
      t639 = t174 * t143 * t637
      t641 = t103 * t113
      t647 = t226 * t79 * t113 * t82
      t651 = t179 * t103 * t70 * t82
      t658 = t18 * t171 * t637
      t664 = t174 * t87 * t637
      t666 = t256 * t137
      t670 = -t460 + 0.64D2 * t635 - 0.128D3 * t639 + 0.64D2 * t179 * t1
     #43 * t641 + 0.64D2 * t647 - 0.128D3 * t651 - 0.288D3 * t30 * t35 *
     # t36 * t12 - 0.256D3 * t658 - 0.136D3 * t179 * t87 * t641 + 0.216D
     #3 * t664 + 0.64D2 * t666 - 0.432D3 * t467 + 0.640D3 * t471
      t678 = t535 * t463 * t136
      t690 = t110 * t120 * t537 * x2
      t694 = t119 * t120 * t128 * x3
      t700 = 0.256D3 * t476 + 0.256D3 * t481 + 0.256D3 * t486 + 0.64D2 *
     # t527 * t531 * t136 - 0.128D3 * t678 - 0.432D3 * t462 * t531 * t11
     # * x1 * t70 - 0.432D3 * t141 * t159 * t143 + 0.256D3 * t690 - 0.25
     #6D3 * t694 - 0.444D3 * t494 + 0.640D3 * t497 + 0.640D3 * t499 + 0.
     #216D3 * t502 - t506
      t731 = t77 * x2
      t738 = -0.74288D5 * t25 - 0.256D3 * t107 - 0.74288D5 * t123 - 0.29
     #128D5 * t131 + 0.8D1 * t138 + 0.48D2 * t209 * t127 * t70 * t72 - 0
     #.192D3 * t119 * t120 * t548 + 0.48D2 * t126 * t127 * t113 * x2 + 0
     #.32D2 * t119 * t111 * t199 + 0.64D2 * t110 * t79 * x2 * t17 * x3 *
     # t70 + 0.64D2 * t28 * t731 * t9 * t103 * t555 - 0.64D2 * t529
      t757 = t103 * x2
      t758 = z ** 2
      t766 = -0.128D3 * t533 + 0.80D2 * t539 + 0.25248D5 * t550 - 0.2526
     #4D5 * t557 - 0.28208D5 * t161 + 0.74208D5 * t165 - 0.27920D5 * t16
     #8 - 0.384D3 * t7 * t731 * t8 * t103 * z * t70 + 0.384D3 * t110 * t
     #79 * t72 * t17 * z - 0.384D3 * t535 * t757 * x1 * t758 - 0.144D3 *
     # t177 - 0.8D1 * t194 - 0.4D1 * t201
      t780 = -0.8D1 * t207 + 0.28792D5 * t212 - 0.144D3 * t220 - 0.128D3
     # * t227 + 0.74320D5 * t232 + 0.252D3 * t236 - 0.144D3 * t242 - 0.4
     #D1 * t247 + 0.304D3 * t254 - 0.32D2 * t259 + 0.8D1 * t267 + 0.8D1 
     #* t269
      t809 = 0.288D3 * t272 - 0.29128D5 * t277 - 0.64D2 * t283 + 0.288D3
     # * t293 + 0.28792D5 * t298 + 0.128D3 * t303 - 0.128D3 * t317 - 0.1
     #44D3 * t321 - 0.96D2 * t290 * t448 - 0.112D3 * t216 * t250 * t217 
     #* t77 - 0.128D3 * t170 * x2 * t424 * t22 * t758 * z - 0.384D3 * t1
     #74 * t82 * t22 * t758 + 0.384D3 * t271 * t73 * t103 * z
      t842 = 0.48D2 * t274 * t96 * t36 * t23 + 0.48D2 * t295 * t96 * x3 
     #* t296 - 0.192D3 * t18 * t9 * x3 * t23 - 0.96D2 * t229 * t200 + 0.
     #16D2 * t249 * t250 * t77 * t335 + 0.288D3 * t184 * t79 * t70 * t73
     # + 0.32D2 * t18 * t245 * t637 + 0.128D3 * t351 - 0.27920D5 * t355 
     #+ 0.74208D5 * t359 - 0.28208D5 * t362 - 0.64D2 * t368
      t866 = 0.128D3 * t372 - 0.64D2 * t376 - 0.64D2 * t379 - 0.128D3 * 
     #t385 + 0.128D3 * t608 - 0.96D2 * t271 * t73 * t103 - 0.112D3 * t23
     #8 * t240 * t79 - 0.25264D5 * t614 + 0.288D3 * t48 * t461 * t111 * 
     #t447 - 0.384D3 * t527 * t757 * t483 * x3 + 0.128D3 * t398 - 0.64D2
     # * t408 - 0.64D2 * t414
      t885 = 0.74320D5 * t418 + 0.304D3 * t422 - 0.32D2 * t426 + 0.128D3
     # * t626 - 0.96D2 * t134 * t416 * t36 + 0.16D2 * t7 * t420 * t37 - 
     #0.128D3 * t439 - 0.536D3 * t445 + 0.252D3 * t449 - 0.512D3 * t453 
     #+ 0.8D1 * t455 + 0.128D3 * t635 - 0.64D2 * t639
      t897 = -0.128D3 * t647 - 0.64D2 * t651 - 0.25296D5 * t658 + 0.80D2
     # * t664 + 0.128D3 * t666 - t477 - 0.512D3 * t481 - t487 - 0.64D2 *
     # t678 + 0.25248D5 * t690 - 0.25296D5 * t694 - 0.536D3 * t494 - 0.2
     #56D3 * t499
      rrgg2qqbarh51J4 = -(0.4D1 * wd * (t125 + t196 + t265 + t323 + t370
     # + t410 + t457 + t510) + 0.3D1 * wd * (t525 + t566 + t581 + t595 +
     # t616 + t633 + t670 + t700) + 0.2D1 * wd * (t738 + t766 + t780 + t
     #809 + t842 + t866 + t885 + t897)) * nf / t1 / z / 0.31415926535897
     #93D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t8 = t3 * t6 * t4
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 ** 2
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t4
      t16 = t11 * t15
      t17 = t8 * t16
      t19 = z + x1 * t4
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = t21 * x3
      t23 = 0.1D1 - x3
      t24 = t23 ** 2
      t28 = x2 * x3
      t30 = cos(x4 * 0.3141592653589793D1)
      t31 = x3 * t12
      t35 = Sqrt(t31 * t19 * x2 * t23)
      t37 = 0.2D1 * t30 * t35
      t38 = t23 * t12 * t19 + t28 + t37
      t41 = t17 * t22 * t24 * x1 * t38
      t43 = t5 * t4
      t44 = t3 * t43
      t45 = t10 * t9
      t46 = t24 * t23
      t50 = t3 * t5
      t51 = t10 * t24
      t52 = t50 * t51
      t54 = t3 * t4
      t55 = t9 * t23
      t57 = 0.144D3 * t54 * t55
      t58 = t3 * t6
      t59 = t24 ** 2
      t63 = x1 ** 2
      t64 = t63 ** 2
      t65 = t58 * t64
      t66 = t20 ** 2
      t67 = 0.1D1 / t66
      t69 = x2 * t23
      t70 = t31 * t19 + t69 - t37
      t71 = t70 ** 2
      t72 = t67 * t71
      t73 = t38 ** 2
      t74 = t72 * t73
      t78 = t8 * t64 * x1
      t79 = t73 * t38
      t81 = t67 * t79 * t70
      t83 = 0.144D3 * t78 * t81
      t84 = t71 * t70
      t85 = t67 * t84
      t86 = t85 * t38
      t90 = t21 * t70
      t91 = t90 * t38
      t92 = t50 * t63 * t91
      t94 = t63 * x1
      t95 = t44 * t94
      t97 = 0.1D1 / t20 / t19
      t98 = t97 * t71
      t99 = t98 * t38
      t102 = t95 * t91
      t104 = t58 * x1
      t105 = t45 * t24
      t106 = t105 * x3
      t107 = t104 * t106
      t109 = 0.288D3 * t41 + 0.144D3 * t44 * t45 * t46 + 0.216D3 * t52 +
     # t57 + 0.36D2 * t58 * t11 * t59 + 0.108D3 * t65 * t74 + t83 - 0.10
     #8D3 * t65 * t86 - 0.216D3 * t92 - 0.288D3 * t95 * t99 + 0.880D3 * 
     #t102 - 0.328D3 * t107
      t111 = t10 * t23
      t112 = t111 * x3
      t113 = t44 * x1 * t112
      t115 = t45 * t23
      t116 = x3 ** 2
      t117 = t115 * t116
      t118 = t104 * t117
      t123 = t23 * t15
      t124 = 0.1D1 / t19
      t126 = t50 * t10 * t123 * t124
      t128 = t44 * t45
      t129 = t24 * t15
      t131 = t128 * t129 * t124
      t133 = t58 * t11
      t136 = t133 * t46 * t15 * t124
      t138 = t97 * t70
      t139 = t138 * t73
      t140 = t65 * t139
      t145 = 0.144D3 * t78 * t86
      t146 = t65 * t99
      t148 = t8 * t63
      t149 = x2 * t124
      t151 = t148 * t105 * t149
      t155 = t133 * t123 * t124 * t116
      t158 = t3 * t6 * t5
      t160 = x2 ** 2
      t162 = t63 * t21
      t164 = t158 * t11 * t24 * t160 * t162
      t166 = 0.584D3 * t113 - 0.430D3 * t118 - 0.36D2 * t65 * t81 - 0.31
     #2D3 * t126 - 0.240D3 * t131 + 0.12D2 * t136 - 0.748D3 * t140 + 0.1
     #44D3 * t95 * t139 + t145 - 0.16D2 * t146 + 0.182D3 * t151 - 0.748D
     #3 * t155 + 0.36D2 * t164
      t168 = t8 * t45
      t171 = t168 * t23 * t160 * t162
      t173 = t158 * t94
      t174 = t160 * t21
      t176 = t173 * t115 * t174
      t178 = t124 * x3
      t180 = t128 * t123 * t178
      t184 = x1 * t124
      t191 = t58 * t45 * t24 * x2 * t184
      t194 = t133 * t129 * t178
      t196 = t44 * t10
      t198 = t196 * t69 * t184
      t201 = t3 * t6 * t43
      t202 = t201 * t11
      t203 = t160 * x2
      t205 = t94 * t97
      t207 = t202 * t23 * t203 * t205
      t209 = t58 * t94
      t211 = x3 * t9
      t213 = t209 * t21 * t71 * t211
      t215 = t8 * t64
      t216 = x2 * t9
      t217 = t98 * t216
      t218 = t215 * t217
      t220 = t44 * t9
      t221 = t15 * t97
      t224 = t220 * t221 * t63 * t71
      t226 = t158 * t64
      t227 = t160 * t10
      t228 = t138 * t227
      t229 = t226 * t228
      t231 = t201 * t64
      t233 = t203 * t45
      t235 = t231 * t67 * t70 * t233
      t237 = 0.144D3 * t171 - 0.192D3 * t176 + 0.880D3 * t180 - 0.28D2 *
     # t8 * t11 * t46 * x2 * t184 - 0.112D3 * t191 - 0.16D2 * t194 - 0.1
     #68D3 * t198 - 0.28D2 * t207 - 0.148D3 * t213 - 0.148D3 * t218 + 0.
     #328D3 * t224 - 0.47D2 * t229 - 0.28D2 * t235
      t238 = t58 * t63
      t239 = t124 * t70
      t240 = t10 * t116
      t242 = t238 * t239 * t240
      t244 = t58 * t9
      t245 = t15 * t67
      t248 = t244 * t245 * t94 * t84
      t250 = t50 * t9
      t251 = t15 * t21
      t254 = t250 * t251 * x1 * t70
      t259 = t44 * t63
      t261 = t259 * t239 * t211
      t263 = t90 * t216
      t264 = t209 * t263
      t266 = t8 * t94
      t267 = t266 * t228
      t269 = t259 * t263
      t271 = t209 * t217
      t274 = t226 * t72 * t227
      t277 = t238 * t111 * t149
      t280 = t8 * t11 * t9
      t281 = t15 * t124
      t282 = t116 * x3
      t286 = 0.144D3 * t280 * t281 * t282 * t23
      t290 = 0.144D3 * t280 * t281 * x3 * t46
      t291 = -0.47D2 * t242 - 0.10D2 * t248 - 0.56D2 * t254 - 0.28D2 * t
     #215 * t85 * t216 + 0.128D3 * t261 + 0.128D3 * t264 + 0.144D3 * t26
     #7 - 0.168D3 * t269 - 0.112D3 * t271 + 0.36D2 * t274 - 0.400D3 * t2
     #77 + t286 + t290
      t294 = t64 * t67
      t296 = t70 * t38
      t298 = t158 * t294 * t296 * t227
      t301 = t296 * t216
      t302 = t58 * t205 * t301
      t304 = t8 * t294
      t305 = t71 * t38
      t309 = t9 * t15
      t310 = t58 * t309
      t311 = t67 * t94
      t312 = t70 * t73
      t314 = t310 * t311 * t312
      t316 = t44 * t111
      t317 = x1 * z
      t320 = t316 * t251 * t317 * t38
      t321 = 0.128D3 * t320
      t323 = t97 * t63
      t325 = t44 * t309 * t323 * t296
      t328 = t310 * t311 * t305
      t330 = t63 * t124
      t334 = t44 * t330 * t70 * z * t211
      t335 = 0.128D3 * t334
      t336 = t11 * t24
      t337 = t8 * t336
      t338 = t28 * t184
      t341 = t58 * t115
      t342 = t341 * t338
      t344 = t11 * t23
      t345 = t8 * t344
      t348 = t345 * t251 * t28 * x1
      t350 = t64 * t97
      t351 = t8 * t350
      t353 = t351 * t305 * t211
      t355 = -0.36D2 * t298 + 0.112D3 * t302 + 0.56D2 * t304 * t305 * t2
     #16 - 0.430D3 * t314 - t321 + 0.584D3 * t325 - 0.328D3 * t328 - t33
     #5 + 0.56D2 * t337 * t338 + 0.112D3 * t342 - 0.96D2 * t348 + 0.288D
     #3 * t353
      t360 = t8 * t63 * t45 * t23 * x3 * t149
      t362 = t158 * t344
      t363 = t160 * t63
      t364 = t221 * t363
      t365 = t362 * t364
      t367 = x2 * x1
      t370 = t341 * t251 * t367 * z
      t372 = t251 * t367
      t373 = t341 * t372
      t375 = t337 * t372
      t377 = t10 * t15
      t378 = t8 * t377
      t381 = t378 * t311 * t296 * x2
      t383 = t94 * t21
      t384 = t58 * t383
      t385 = t70 * x2
      t388 = t384 * t385 * t9 * z
      t396 = t362 * x3 * t160 * t162
      t398 = x1 * t38
      t401 = t341 * t251 * t398 * x3
      t403 = t251 * t398
      t404 = t316 * t403
      t407 = t384 * t296 * t211
      t409 = t351 * t301
      t411 = 0.530D3 * t360 - 0.47D2 * t365 - 0.128D3 * t370 + 0.128D3 *
     # t373 - 0.148D3 * t375 + 0.530D3 * t381 - 0.128D3 * t388 - 0.28D2 
     #* t345 * t116 * x2 * t184 - 0.36D2 * t396 - 0.96D2 * t401 + 0.128D
     #3 * t404 - 0.96D2 * t407 - 0.96D2 * t409
      t415 = 0.144D3 * t54 * t184 * t70
      t424 = t50 * t162 * t71
      t427 = t44 * t383 * t71
      t429 = t71 ** 2
      t437 = t58 * t350 * t84
      t440 = t50 * t330 * t70
      t442 = x1 * t10
      t444 = t44 * t442 * t24
      t446 = x1 * t45
      t448 = t58 * t446 * t46
      t450 = x1 * t9
      t452 = t50 * t450 * t23
      t456 = t415 - 0.108D3 * t58 * t11 * t46 * x3 + 0.108D3 * t58 * t33
     #6 * t116 + 0.216D3 * t424 - 0.240D3 * t427 + 0.36D2 * t58 * t294 *
     # t429 + 0.144D3 * t44 * t205 * t84 + 0.12D2 * t437 - 0.312D3 * t44
     #0 + 0.328D3 * t444 - 0.10D2 * t448 - 0.56D2 * t452 - 0.288D3 * t44
     # * t106
      t460 = t50 * t112
      t465 = t58 * t105 * t403
      t467 = t8 * t383
      t470 = 0.144D3 * t467 * t296 * t240
      t478 = t8 * t115 * t221 * t63 * t38 * x2
      t481 = t63 * t73
      t482 = t221 * t481
      t483 = t58 * t111 * t482
      t487 = t467 * t70 * t10 * t28
      t489 = t45 * t15
      t490 = t158 * t489
      t493 = t490 * t311 * t70 * t160
      t495 = t8 * t489
      t496 = t97 * x3
      t501 = 0.144D3 * t495 * t496 * t23 * t63 * t73
      t502 = t58 * t377
      t504 = t502 * t323 * t385
      t508 = t378 * t311 * t71 * x2
      t510 = -0.36D2 * t58 * t344 * t282 - 0.216D3 * t460 + 0.144D3 * t4
     #4 * t117 - 0.148D3 * t465 + t470 - 0.28D2 * t304 * t312 * t216 - 0
     #.94D2 * t478 - 0.47D2 * t483 - 0.94D2 * t487 - 0.192D3 * t493 + t5
     #01 - 0.400D3 * t504 + 0.182D3 * t508
      t518 = t58 * t489
      t520 = t518 * t22 * t367
      t522 = t21 * t116
      t524 = t17 * t522 * t367
      t533 = -0.432D3 * t41 - 0.72D2 * t52 - t57 - 0.128D3 * t520 + 0.64
     #D2 * t524 - t83 + 0.72D2 * t92 - 0.640D3 * t102 + 0.604D3 * t107 -
     # 0.544D3 * t113 + 0.542D3 * t118 + 0.264D3 * t126 - 0.64D2 * t131
      t538 = t259 * t216 * t124
      t550 = 0.16D2 * t136 + 0.592D3 * t140 - t145 + 0.608D3 * t146 + 0.
     #64D2 * t538 - 0.288D3 * t78 * t74 - 0.108D3 * t151 + 0.592D3 * t15
     #5 - 0.72D2 * t164 - 0.256D3 * t171 + 0.30D2 * t176 - 0.640D3 * t18
     #0 + 0.144D3 * t191 + 0.608D3 * t194
      t565 = 0.344D3 * t198 + 0.56D2 * t207 + 0.216D3 * t213 + 0.216D3 *
     # t218 - 0.256D3 * t224 + 0.54D2 * t229 + 0.56D2 * t235 + 0.54D2 * 
     #t242 + 0.62D2 * t248 - 0.24D2 * t254 - 0.496D3 * t261 - 0.496D3 * 
     #t264 - 0.256D3 * t267
      t570 = t196 * t372
      t572 = t124 * t38
      t574 = t259 * t211 * t572
      t576 = t21 * t73
      t582 = t215 * t97 * t73 * t216
      t586 = t209 * t21 * t38 * t216
      t593 = t238 * t51 * t572
      t598 = 0.344D3 * t269 + 0.144D3 * t271 - 0.72D2 * t274 + 0.288D3 *
     # t277 - t286 - t290 + 0.64D2 * t570 - 0.128D3 * t574 + 0.64D2 * t2
     #09 * t211 * t576 + 0.64D2 * t582 - 0.128D3 * t586 - 0.288D3 * t280
     # * t281 * t116 * t24 - 0.256D3 * t593 - 0.136D3 * t209 * t55 * t57
     #6
      t602 = t259 * t55 * t572
      t604 = t250 * t403
      t617 = 0.216D3 * t602 + 0.64D2 * t604 + 0.72D2 * t298 - 0.144D3 * 
     #t302 + 0.542D3 * t314 + 0.256D3 * t320 - 0.544D3 * t325 + 0.604D3 
     #* t328 + 0.256D3 * t334 - 0.144D3 * t342 + 0.640D3 * t348 - 0.432D
     #3 * t353 - 0.444D3 * t360
      t627 = t38 * x3
      t629 = t58 * t63 * t10 * t23 * t124 * t627
      t633 = t502 * t323 * t296 * x3
      t638 = t44 * t377
      t640 = t638 * t22 * t398
      t650 = 0.54D2 * t365 + 0.256D3 * t370 - 0.496D3 * t373 + 0.216D3 *
     # t375 - 0.444D3 * t381 + 0.256D3 * t388 - 0.256D3 * t629 - 0.256D3
     # * t633 + 0.64D2 * t518 * t522 * t398 - 0.128D3 * t640 - 0.432D3 *
     # t17 * t522 * t23 * x1 * t38 + 0.72D2 * t396 + 0.640D3 * t401 - 0.
     #496D3 * t404
      t654 = t21 * x1
      t655 = t70 * x3
      t657 = t638 * t654 * t655
      t666 = t38 * x2
      t668 = t8 * t94 * t10 * t23 * t21 * t666
      t675 = t495 * t323 * t655 * x2
      t679 = t502 * t323 * t71 * x3
      t685 = 0.640D3 * t407 + 0.640D3 * t409 + 0.216D3 * t657 - 0.136D3 
     #* t518 * t654 * t70 * t116 + 0.256D3 * t668 - 0.432D3 * t351 * t31
     #2 * t211 + 0.256D3 * t675 - 0.256D3 * t679 - t415 - 0.72D2 * t424 
     #- 0.64D2 * t427 + 0.16D2 * t437 + 0.264D3 * t440
      t691 = t50 * t450 * x3
      t700 = -0.256D3 * t444 + 0.62D2 * t448 - 0.24D2 * t452 + 0.72D2 * 
     #t460 + 0.64D2 * t691 + 0.216D3 * t465 - t470 + 0.108D3 * t478 + 0.
     #54D2 * t483 + 0.108D3 * t487 + 0.30D2 * t493 - t501 + 0.288D3 * t5
     #04 - 0.108D3 * t508
      t721 = t94 * x2
      t738 = -0.64D2 * t520 - 0.128D3 * t524 + 0.48D2 * t378 * t311 * t7
     #3 * x2 + 0.32D2 * t502 * t496 * t481 + 0.64D2 * t495 * t97 * x2 * 
     #t63 * x3 * t38 + 0.64D2 * t8 * t721 * t10 * t21 * t627 - 0.384D3 *
     # t58 * t721 * t9 * t21 * z * t38 + 0.128D3 * t102 - 0.27920D5 * t1
     #07 + 0.74208D5 * t113 - 0.28208D5 * t118 - 0.64D2 * t126
      t747 = t21 * x2
      t748 = z ** 2
      t770 = 0.128D3 * t131 - 0.64D2 * t136 - 0.64D2 * t140 - 0.128D3 * 
     #t146 + 0.128D3 * t538 - 0.96D2 * t266 * t227 * t21 - 0.384D3 * t63
     #8 * t747 * x1 * t748 + 0.288D3 * t158 * t16 * t496 * t363 - 0.384D
     #3 * t518 * t747 * t317 * x3 + 0.48D2 * t490 * t311 * t38 * t160 - 
     #0.192D3 * t502 * t323 * t666 - 0.29128D5 * t151 - 0.64D2 * t155
      t784 = 0.288D3 * t171 + 0.28792D5 * t176 + 0.128D3 * t180 - 0.128D
     #3 * t194 - 0.144D3 * t198 - 0.144D3 * t207 - 0.128D3 * t218 + 0.74
     #320D5 * t224 + 0.252D3 * t229 - 0.144D3 * t235 - 0.4D1 * t242 + 0.
     #304D3 * t248
      t798 = -0.32D2 * t254 + 0.8D1 * t261 + 0.8D1 * t264 + 0.288D3 * t2
     #67 - 0.144D3 * t269 - 0.74288D5 * t277 + 0.128D3 * t570 - 0.64D2 *
     # t574 - 0.128D3 * t582 - 0.64D2 * t586 - 0.25296D5 * t593 + 0.80D2
     # * t602 + 0.128D3 * t604
      t846 = -0.96D2 * t168 * t364 - 0.112D3 * t202 * t245 * t203 * t94 
     #- 0.128D3 * t50 * x2 * t450 * t124 * t748 * z - 0.384D3 * t259 * t
     #216 * t124 * t748 + 0.384D3 * t266 * t227 * t21 * z + 0.48D2 * t14
     #8 * t45 * t116 * t149 + 0.48D2 * t173 * t45 * x3 * t174 - 0.192D3 
     #* t238 * t10 * x3 * t149 - 0.96D2 * t220 * t482 + 0.16D2 * t244 * 
     #t245 * t94 * t79 + 0.288D3 * t226 * t97 * t38 * t227 + 0.32D2 * t2
     #38 * t240 * t572
      t862 = -0.28208D5 * t314 + 0.384D3 * t495 * t97 * t160 * t63 * z -
     # t321 + 0.74208D5 * t325 - 0.27920D5 * t328 - t335 - 0.256D3 * t34
     #8 - 0.536D3 * t360 + 0.252D3 * t365 - 0.512D3 * t370 + 0.8D1 * t37
     #3 - 0.128D3 * t375 - 0.536D3 * t381
      t877 = -0.512D3 * t388 - 0.25264D5 * t629 - 0.25264D5 * t633 - 0.6
     #4D2 * t640 + 0.8D1 * t404 - 0.256D3 * t409 + 0.80D2 * t657 + 0.252
     #48D5 * t668 + 0.25248D5 * t675 - 0.25296D5 * t679 + 0.128D3 * t427
     # - 0.64D2 * t437 - 0.64D2 * t440
      t897 = 0.74320D5 * t444 + 0.304D3 * t448 - 0.32D2 * t452 + 0.128D3
     # * t691 - 0.96D2 * t44 * t442 * t116 + 0.16D2 * t58 * t446 * t282 
     #- 0.112D3 * t231 * t233 * t97 - 0.8D1 * t478 - 0.4D1 * t483 - 0.8D
     #1 * t487 + 0.28792D5 * t493 - 0.74288D5 * t504 - 0.29128D5 * t508
      rrgg2qqbarh51J5 = -(0.5D1 * wd * (t109 + t166 + t237 + t291 + t355
     # + t411 + t456 + t510) + 0.4D1 * wd * (t533 + t550 + t565 + t598 +
     # t617 + t650 + t685 + t700) + 0.3D1 * wd * (t738 + t770 + t784 + t
     #798 + t846 + t862 + t877 + t897)) * nf / t1 / z / 0.31415926535897
     #93D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = x1 ** 2
      t9 = t8 * x1
      t11 = z + x1 * t4
      t12 = t11 ** 2
      t14 = 0.1D1 / t12 / t11
      t15 = t9 * t14
      t17 = 0.1D1 - x2
      t18 = x3 * t17
      t20 = 0.1D1 - x3
      t21 = x2 * t20
      t23 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(t18 * t11 * x2 * t20)
      t29 = 0.2D1 * t23 * t27
      t30 = t18 * t11 + t21 - t29
      t33 = x2 * x3
      t34 = t20 * t17 * t11 + t33 + t29
      t35 = t30 * t34
      t36 = 0.1D1 - x1
      t37 = x2 * t36
      t38 = t35 * t37
      t43 = z + x1 * t17 * t4
      t44 = t36 * t43
      t45 = t7 * t44
      t46 = t12 ** 2
      t47 = 0.1D1 / t46
      t48 = t47 * t9
      t49 = t34 ** 2
      t50 = t30 * t49
      t52 = t45 * t48 * t50
      t54 = t5 * t4
      t55 = t3 * t54
      t57 = t14 * t8
      t59 = t55 * t44 * t57 * t35
      t61 = t30 ** 2
      t62 = t61 * t34
      t64 = t45 * t48 * t62
      t67 = t3 * t6 * t4
      t68 = t36 ** 2
      t69 = t68 ** 2
      t70 = t69 * t43
      t71 = t67 * t70
      t72 = 0.1D1 / t12
      t73 = x3 ** 2
      t74 = t72 * t73
      t75 = x2 * x1
      t77 = t71 * t74 * t75
      t79 = t55 * t8
      t80 = 0.1D1 / t11
      t82 = t79 * t37 * t80
      t84 = t68 * t43
      t85 = t7 * t84
      t88 = t85 * t57 * t35 * x3
      t90 = t9 * t72
      t91 = t67 * t90
      t92 = t68 * t73
      t97 = t3 * t6 * t5
      t98 = t69 * t20
      t99 = t97 * t98
      t100 = t43 * t14
      t101 = x2 ** 2
      t102 = t101 * t8
      t103 = t100 * t102
      t104 = t99 * t103
      t106 = t68 * t36
      t107 = t106 * t20
      t108 = t7 * t107
      t109 = t43 * t72
      t112 = t108 * t109 * t75 * z
      t114 = t109 * t75
      t115 = t108 * t114
      t117 = t20 ** 2
      t120 = t67 * t69 * t117 * t114
      t122 = t106 * t43
      t123 = t7 * t122
      t124 = t72 * x3
      t126 = t123 * t124 * t75
      t128 = -0.144D3 * t7 * t15 * t38 + 0.542D3 * t52 - 0.544D3 * t59 +
     # 0.604D3 * t64 + 0.64D2 * t77 + 0.64D2 * t82 - 0.256D3 * t88 - 0.1
     #44D3 * t91 * t35 * t92 + 0.54D2 * t104 + 0.256D3 * t112 - 0.496D3 
     #* t115 + 0.216D3 * t120 - 0.128D3 * t126
      t129 = x1 * t34
      t138 = t67 * t107 * t100 * t8 * t34 * x2
      t140 = t68 * t20
      t142 = t8 * t49
      t143 = t100 * t142
      t144 = t7 * t140 * t143
      t148 = t91 * t30 * t68 * t33
      t150 = t97 * t122
      t153 = t150 * t48 * t30 * t101
      t155 = t67 * t122
      t156 = t14 * x3
      t162 = t7 * t90
      t163 = t36 * x3
      t169 = t85 * t57 * t61 * x3
      t177 = t8 * t72
      t184 = x2 * t80
      t186 = t67 * t8 * t106 * t20 * x3 * t184
      t188 = t55 * t84
      t190 = t188 * t124 * t129
      t197 = t8 ** 2
      t198 = t197 * t14
      t199 = t67 * t198
      t203 = 0.640D3 * t108 * t109 * t129 * x3 + 0.108D3 * t138 + 0.54D2
     # * t144 + 0.108D3 * t148 + 0.30D2 * t153 - 0.144D3 * t155 * t156 *
     # t20 * t8 * t49 + 0.640D3 * t162 * t35 * t163 - 0.256D3 * t169 - 0
     #.432D3 * t71 * t124 * t117 * x1 * t34 + 0.72D2 * t99 * x3 * t101 *
     # t177 - 0.444D3 * t186 - 0.128D3 * t190 - 0.432D3 * t71 * t74 * t2
     #0 * x1 * t34 - 0.432D3 * t199 * t50 * t163
      t205 = t30 * x3
      t208 = t155 * t57 * t205 * x2
      t210 = t67 * t84
      t213 = t210 * t48 * t61 * x2
      t215 = t55 * t140
      t216 = t109 * t129
      t217 = t215 * t216
      t222 = t72 * x1
      t224 = t188 * t222 * t205
      t228 = t210 * t48 * t35 * x2
      t230 = t55 * t106
      t231 = t117 * t43
      t233 = t230 * t231 * t80
      t235 = t7 * t69
      t236 = t117 * t20
      t239 = t235 * t236 * t43 * t80
      t241 = t7 * t197
      t242 = t14 * t30
      t244 = t241 * t242 * t49
      t247 = t67 * t197 * x1
      t249 = t61 * t30
      t253 = t14 * t61
      t255 = t241 * t253 * t34
      t257 = t49 * t34
      t262 = t3 * t5
      t264 = t72 * t30
      t265 = t264 * t34
      t268 = 0.256D3 * t208 - 0.108D3 * t213 - 0.496D3 * t217 - 0.432D3 
     #* t199 * t62 * t163 + 0.216D3 * t224 - 0.444D3 * t228 - 0.64D2 * t
     #233 + 0.16D2 * t239 + 0.592D3 * t244 - 0.144D3 * t247 * t47 * t34 
     #* t249 + 0.608D3 * t255 - 0.144D3 * t247 * t47 * t257 * t30 + 0.72
     #D2 * t262 * t8 * t265
      t270 = t55 * t9 * t265
      t272 = t7 * x1
      t273 = t106 * t117
      t275 = t272 * t273 * x3
      t278 = t140 * x3
      t279 = t55 * x1 * t278
      t282 = t272 * t107 * t73
      t285 = t20 * t43
      t287 = t262 * t68 * t285 * t80
      t289 = x1 * t80
      t293 = t8 * t80
      t297 = t55 * t293 * t30 * z * t163
      t299 = t30 * x2
      t302 = t162 * t299 * t36 * z
      t304 = x1 * z
      t307 = t215 * t109 * t304 * t34
      t309 = t7 * t8
      t310 = t68 * t117
      t311 = t80 * t34
      t313 = t309 * t310 * t311
      t315 = t7 * t9
      t316 = t36 * t20
      t317 = t72 * t49
      t322 = t79 * t316 * t311
      t324 = t262 * t36
      t325 = t324 * t216
      t328 = t309 * t140 * t184
      t330 = -0.640D3 * t270 + 0.604D3 * t275 - 0.544D3 * t279 + 0.542D3
     # * t282 + 0.264D3 * t287 - 0.144D3 * t108 * t33 * t289 + 0.256D3 *
     # t297 + 0.256D3 * t302 + 0.256D3 * t307 - 0.256D3 * t313 - 0.136D3
     # * t315 * t316 * t317 + 0.216D3 * t322 + 0.64D2 * t325 + 0.288D3 *
     # t328
      t334 = t67 * t69 * t36
      t335 = t43 * t80
      t336 = t73 * x3
      t345 = t199 * t38
      t354 = t264 * t37
      t355 = t315 * t354
      t357 = t67 * t9
      t358 = t101 * t68
      t359 = t242 * t358
      t360 = t357 * t359
      t362 = t79 * t354
      t364 = t253 * t37
      t367 = t97 * t197
      t372 = t55 * t68
      t373 = t372 * t114
      t376 = t79 * t163 * t311
      t381 = -0.144D3 * t334 * t335 * t336 * t20 - 0.144D3 * t334 * t335
     # * x3 * t236 + 0.640D3 * t345 + 0.216D3 * t7 * t273 * t216 - 0.288
     #D3 * t334 * t335 * t73 * t117 - 0.496D3 * t355 - 0.256D3 * t360 + 
     #0.344D3 * t362 + 0.144D3 * t315 * t364 - 0.72D2 * t367 * t47 * t61
     # * t358 + 0.64D2 * t373 - 0.128D3 * t376 + 0.64D2 * t315 * t163 * 
     #t317
      t382 = t67 * t197
      t385 = t382 * t14 * t49 * t37
      t389 = t315 * t72 * t34 * t37
      t392 = t3 * t6 * t54
      t393 = t392 * t69
      t394 = t101 * x2
      t397 = t393 * t20 * t394 * t15
      t403 = t382 * t364
      t405 = t55 * t36
      t408 = t405 * t100 * t8 * t61
      t410 = t367 * t359
      t412 = t392 * t197
      t414 = t394 * t106
      t416 = t412 * t47 * t30 * t414
      t418 = t80 * t30
      t420 = t309 * t418 * t92
      t422 = t7 * t36
      t423 = t43 * t47
      t426 = t422 * t423 * t9 * t249
      t430 = t324 * t109 * x1 * t30
      t433 = t79 * t418 * t163
      t435 = t67 * t8
      t437 = t435 * t273 * t184
      t441 = t235 * t285 * t80 * t73
      t443 = 0.64D2 * t385 - 0.128D3 * t389 + 0.56D2 * t397 + 0.216D3 * 
     #t315 * t72 * t61 * t163 + 0.216D3 * t403 - 0.256D3 * t408 + 0.54D2
     # * t410 + 0.56D2 * t416 + 0.54D2 * t420 + 0.62D2 * t426 - 0.24D2 *
     # t430 - 0.496D3 * t433 - 0.108D3 * t437 + 0.592D3 * t441
      t450 = t67 * t106
      t453 = t450 * t20 * t101 * t177
      t455 = t97 * t9
      t456 = t101 * t72
      t458 = t455 * t107 * t456
      t460 = t80 * x3
      t462 = t230 * t285 * t460
      t470 = t235 * t231 * t460
      t473 = t372 * t21 * t289
      t481 = t3 * t4
      t485 = t85 * t57 * t299
      t494 = t34 * x2
      t496 = t67 * t9 * t68 * t20 * t72 * t494
      t498 = -0.72D2 * t97 * t69 * t117 * t101 * t177 - 0.256D3 * t453 +
     # 0.30D2 * t458 - 0.640D3 * t462 + 0.144D3 * t7 * t106 * t117 * x2 
     #* t289 + 0.608D3 * t470 + 0.344D3 * t473 - 0.288D3 * t247 * t47 * 
     #t49 * t61 - 0.72D2 * t262 * t310 - 0.144D3 * t481 * t316 + 0.288D3
     # * t485 - 0.136D3 * t123 * t222 * t30 * t73 + 0.256D3 * t496
      t502 = t34 * x3
      t504 = t7 * t8 * t68 * t20 * t80 * t502
      t506 = x1 * t36
      t508 = t262 * t506 * t20
      t519 = t55 * t90 * t61
      t522 = t7 * t198 * t249
      t525 = t262 * t293 * t30
      t527 = x1 * t68
      t529 = t55 * t527 * t117
      t531 = x1 * t106
      t533 = t7 * t531 * t236
      t538 = t67 * t98 * t109 * t33 * x1
      t541 = t262 * t506 * x3
      t551 = -0.256D3 * t504 - 0.24D2 * t508 + 0.72D2 * t262 * t278 - 0.
     #144D3 * t481 * t289 * t30 - 0.72D2 * t262 * t177 * t61 - 0.64D2 * 
     #t519 + 0.16D2 * t522 + 0.264D3 * t525 - 0.256D3 * t529 + 0.62D2 * 
     #t533 + 0.640D3 * t538 + 0.64D2 * t541 + 0.64D2 * t123 * t74 * t129
     # + 0.72D2 * t97 * t197 * t47 * t35 * t358
      t572 = -0.28208D5 * t52 + 0.74208D5 * t59 - 0.27920D5 * t64 - 0.12
     #8D3 * t77 + 0.128D3 * t82 - 0.25264D5 * t88 + 0.252D3 * t104 - 0.5
     #12D3 * t112 + 0.8D1 * t115 - 0.128D3 * t120 - 0.64D2 * t126 + 0.48
     #D2 * t210 * t48 * t49 * x2
      t593 = 0.32D2 * t85 * t156 * t142 + 0.64D2 * t155 * t14 * x2 * t8 
     #* x3 * t34 - 0.8D1 * t138 - 0.4D1 * t144 - 0.8D1 * t148 + 0.28792D
     #5 * t153 - 0.25296D5 * t169 - 0.536D3 * t186 - 0.64D2 * t190 + 0.2
     #5248D5 * t208 - 0.29128D5 * t213 + 0.8D1 * t217 + 0.80D2 * t224
      t597 = z ** 2
      t637 = -0.536D3 * t228 - 0.128D3 * t262 * x2 * t506 * t80 * t597 *
     # z - 0.384D3 * t79 * t37 * t80 * t597 + 0.384D3 * t357 * t358 * t7
     #2 * z + 0.48D2 * t435 * t106 * t73 * t184 + 0.48D2 * t455 * t106 *
     # x3 * t456 - 0.192D3 * t309 * t68 * x3 * t184 - 0.96D2 * t405 * t1
     #43 + 0.16D2 * t422 * t423 * t9 * t257 + 0.288D3 * t367 * t14 * t34
     # * t358 + 0.32D2 * t309 * t92 * t311 + 0.128D3 * t233
      t659 = -0.64D2 * t239 - 0.64D2 * t244 - 0.128D3 * t255 - 0.96D2 * 
     #t357 * t358 * t72 - 0.112D3 * t412 * t414 * t14 + 0.128D3 * t270 -
     # 0.27920D5 * t275 + 0.74208D5 * t279 - 0.28208D5 * t282 - 0.64D2 *
     # t287 + 0.384D3 * t155 * t14 * t101 * t8 * z - 0.128D3 * t297 - 0.
     #512D3 * t302
      t663 = x2 * t9
      t693 = -0.128D3 * t307 + 0.64D2 * t67 * t663 * t68 * t72 * t502 - 
     #0.384D3 * t7 * t663 * t36 * t72 * z * t34 - 0.96D2 * t450 * t103 -
     # 0.112D3 * t393 * t423 * t394 * t9 - 0.25296D5 * t313 + 0.80D2 * t
     #322 + 0.128D3 * t325 - 0.74288D5 * t328 - 0.256D3 * t345 + 0.48D2 
     #* t150 * t48 * t34 * t101 - 0.192D3 * t85 * t57 * t494
      t707 = 0.8D1 * t355 + 0.288D3 * t360 - 0.144D3 * t362 + 0.128D3 * 
     #t373 - 0.64D2 * t376 - 0.128D3 * t385 - 0.64D2 * t389 - 0.144D3 * 
     #t397 - 0.128D3 * t403 + 0.74320D5 * t408 + 0.252D3 * t410 - 0.144D
     #3 * t416 - 0.4D1 * t420
      t719 = t72 * x2
      t726 = 0.304D3 * t426 - 0.32D2 * t430 + 0.8D1 * t433 - 0.29128D5 *
     # t437 - 0.64D2 * t441 + 0.288D3 * t453 + 0.28792D5 * t458 + 0.128D
     #3 * t462 - 0.128D3 * t470 - 0.144D3 * t473 - 0.384D3 * t188 * t719
     # * x1 * t597 - 0.74288D5 * t485 + 0.25248D5 * t496
      t750 = -0.25264D5 * t504 + 0.288D3 * t97 * t70 * t156 * t102 - 0.3
     #84D3 * t123 * t719 * t304 * x3 - 0.32D2 * t508 - 0.96D2 * t55 * t5
     #27 * t73 + 0.16D2 * t7 * t531 * t336 + 0.128D3 * t519 - 0.64D2 * t
     #522 - 0.64D2 * t525 + 0.74320D5 * t529 + 0.304D3 * t533 - 0.256D3 
     #* t538 + 0.128D3 * t541
      rrgg2qqbarh51J6 = -(0.5D1 * wd * (t128 + t203 + t268 + t330 + t381
     # + t443 + t498 + t551) + 0.4D1 * wd * (t572 + t593 + t637 + t659 +
     # t693 + t707 + t726 + t750)) * nf / t1 / z / 0.3141592653589793D1 
     #/ 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh51J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t8 = t3 * t6 * t4
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = t10 * t9
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t4
      t16 = t11 * t15
      t17 = t8 * t16
      t19 = z + x1 * t4
      t20 = t19 ** 2
      t22 = 0.1D1 / t20 / t19
      t23 = x1 ** 2
      t24 = t22 * t23
      t25 = x3 * t12
      t27 = 0.1D1 - x3
      t28 = x2 * t27
      t30 = cos(x4 * 0.3141592653589793D1)
      t34 = Sqrt(t25 * t19 * x2 * t27)
      t36 = 0.2D1 * t30 * t34
      t37 = t25 * t19 + t28 - t36
      t38 = t37 * x3
      t43 = t3 * t6
      t44 = t10 * t15
      t45 = t43 * t44
      t46 = t37 ** 2
      t51 = t9 * t15
      t52 = t43 * t51
      t53 = t20 ** 2
      t54 = 0.1D1 / t53
      t55 = t23 * x1
      t56 = t54 * t55
      t59 = x2 * x3
      t60 = t27 * t12 * t19 + t59 + t36
      t61 = t60 ** 2
      t66 = t5 * t4
      t67 = t3 * t66
      t69 = t37 * t60
      t81 = t67 * t44
      t82 = 0.1D1 / t20
      t83 = t82 * x3
      t84 = x1 * t60
      t88 = t11 * t27
      t89 = t43 * t88
      t90 = t15 * t82
      t91 = x2 * x1
      t92 = t90 * t91
      t95 = t10 ** 2
      t96 = t27 ** 2
      t101 = t10 * t27
      t102 = t67 * t101
      t103 = x1 * z
      t108 = t23 ** 2
      t109 = t108 * t22
      t111 = x2 * t9
      t115 = t8 * t44
      t120 = 0.25248D5 * t17 * t24 * t38 * x2 - 0.25296D5 * t45 * t24 * 
     #t46 * x3 - 0.28208D5 * t52 * t56 * t37 * t61 + 0.74208D5 * t67 * t
     #51 * t24 * t69 - 0.27920D5 * t52 * t56 * t46 * t60 - 0.25264D5 * t
     #45 * t24 * t69 * x3 - 0.64D2 * t81 * t83 * t84 + 0.8D1 * t89 * t92
     # - 0.128D3 * t8 * t95 * t96 * t92 - 0.128D3 * t102 * t90 * t103 * 
     #t60 - 0.256D3 * t8 * t109 * t69 * t111 - 0.29128D5 * t115 * t56 * 
     #t46 * x2
      t121 = t90 * t84
      t131 = t60 * x2
      t137 = 0.1D1 / t19
      t139 = t60 * x3
      t143 = t55 * x2
      t150 = t8 * t23
      t151 = t11 * t96
      t152 = x2 * t137
      t156 = t43 * t95
      t157 = t27 * t15
      t158 = x3 ** 2
      t163 = t8 * t11
      t164 = x2 ** 2
      t171 = t3 * t6 * t5
      t172 = t171 * t55
      t173 = t164 * t82
      t177 = t67 * t11
      t178 = t137 * x3
      t182 = t96 * t15
      t186 = t67 * t10
      t192 = t3 * t6 * t66
      t193 = t192 * t95
      t194 = t164 * x2
      t200 = 0.8D1 * t102 * t121 + 0.80D2 * t81 * t82 * x1 * t38 + 0.252
     #48D5 * t8 * t55 * t10 * t27 * t82 * t131 - 0.25264D5 * t43 * t23 *
     # t10 * t27 * t137 * t139 - 0.384D3 * t43 * t143 * t9 * t82 * z * t
     #60 - 0.29128D5 * t150 * t151 * t152 - 0.64D2 * t156 * t157 * t137 
     #* t158 + 0.288D3 * t163 * t27 * t164 * t23 * t82 + 0.28792D5 * t17
     #2 * t88 * t173 + 0.128D3 * t177 * t157 * t178 - 0.128D3 * t156 * t
     #182 * t178 - 0.144D3 * t186 * t28 * x1 * t137 - 0.144D3 * t193 * t
     #27 * t194 * t55 * t22
      t202 = t8 * t108
      t203 = t22 * t46
      t207 = t67 * t9
      t208 = t15 * t22
      t213 = t171 * t108
      t214 = t22 * t37
      t215 = t164 * t10
      t216 = t214 * t215
      t219 = t192 * t108
      t221 = t194 * t11
      t225 = t43 * t23
      t226 = t137 * t37
      t227 = t10 * t158
      t231 = t43 * t9
      t232 = t15 * t54
      t233 = t46 * t37
      t238 = t3 * t5
      t239 = t238 * t9
      t244 = t67 * t23
      t245 = x3 * t9
      t249 = t164 * t23
      t250 = t208 * t249
      t258 = x1 * t9
      t259 = z ** 2
      t269 = -0.128D3 * t202 * t203 * t111 + 0.74320D5 * t207 * t208 * t
     #23 * t46 + 0.252D3 * t213 * t216 - 0.144D3 * t219 * t54 * t37 * t2
     #21 - 0.4D1 * t225 * t226 * t227 + 0.304D3 * t231 * t232 * t55 * t2
     #33 - 0.32D2 * t239 * t90 * x1 * t37 + 0.8D1 * t244 * t226 * t245 -
     # 0.96D2 * t163 * t250 - 0.112D3 * t193 * t232 * t194 * t55 - 0.128
     #D3 * t238 * x2 * t258 * t137 * t259 * z - 0.384D3 * t244 * t111 * 
     #t137 * t259
      t270 = t55 * t8
      t287 = t23 * t61
      t288 = t208 * t287
      t300 = t137 * t60
      t304 = t43 * t55
      t305 = t82 * t37
      t306 = t305 * t111
      t318 = 0.384D3 * t270 * t215 * t82 * z + 0.48D2 * t150 * t11 * t15
     #8 * t152 + 0.48D2 * t172 * t11 * x3 * t173 - 0.192D3 * t225 * t10 
     #* x3 * t152 - 0.96D2 * t207 * t288 + 0.16D2 * t231 * t232 * t55 * 
     #t61 * t60 + 0.288D3 * t213 * t22 * t60 * t215 + 0.32D2 * t225 * t2
     #27 * t300 + 0.8D1 * t304 * t306 + 0.288D3 * t270 * t216 - 0.144D3 
     #* t244 * t306 + 0.128D3 * t186 * t92 - 0.64D2 * t244 * t245 * t300
      t355 = t43 * x1
      t363 = -0.128D3 * t202 * t22 * t61 * t111 - 0.64D2 * t304 * t82 * 
     #t60 * t111 - 0.25296D5 * t225 * t10 * t96 * t300 + 0.80D2 * t244 *
     # t9 * t27 * t300 + 0.128D3 * t239 * t121 - 0.74288D5 * t225 * t101
     # * t152 + 0.128D3 * t244 * t111 * t137 - 0.96D2 * t270 * t215 * t8
     #2 - 0.112D3 * t219 * t221 * t22 + 0.128D3 * t67 * t55 * t305 * t60
     # - 0.27920D5 * t355 * t151 * x3 + 0.74208D5 * t67 * x1 * t101 * x3
      t374 = t96 * t27
      t379 = t43 * t108
      t386 = t55 * t82
      t388 = t37 * x2
      t399 = t95 * t27
      t421 = -0.28208D5 * t355 * t88 * t158 - 0.64D2 * t238 * t10 * t157
     # * t137 + 0.128D3 * t177 * t182 * t137 - 0.64D2 * t156 * t374 * t1
     #5 * t137 - 0.64D2 * t379 * t214 * t61 - 0.128D3 * t379 * t203 * t6
     #0 - 0.512D3 * t43 * t386 * t388 * t9 * z - 0.536D3 * t8 * t23 * t1
     #1 * t27 * x3 * t152 + 0.252D3 * t171 * t399 * t250 - 0.512D3 * t89
     # * t90 * t91 * z - 0.8D1 * t8 * t88 * t208 * t23 * t60 * x2 - 0.4D
     #1 * t43 * t101 * t288 - 0.256D3 * t8 * t399 * t90 * t59 * x1
      t429 = t23 * t137
      t433 = x1 * t10
      t437 = x1 * t11
      t462 = t82 * x2
      t467 = t95 * t15
      t469 = t22 * x3
      t473 = 0.128D3 * t67 * t386 * t46 - 0.64D2 * t43 * t109 * t233 - 0
     #.64D2 * t238 * t429 * t37 + 0.74320D5 * t67 * t433 * t96 + 0.304D3
     # * t43 * t437 * t374 - 0.32D2 * t238 * t258 * t27 - 0.96D2 * t67 *
     # t433 * t158 + 0.16D2 * t43 * t437 * t158 * x3 + 0.128D3 * t238 * 
     #t258 * x3 - 0.192D3 * t45 * t24 * t131 + 0.384D3 * t17 * t22 * t16
     #4 * t23 * z - 0.384D3 * t81 * t462 * x1 * t259 + 0.288D3 * t171 * 
     #t467 * t469 * t249
      t474 = t43 * t16
      t479 = t171 * t16
      t531 = -0.384D3 * t474 * t462 * t103 * x3 + 0.48D2 * t479 * t56 * 
     #t60 * t164 - 0.128D3 * t67 * t429 * t37 * z * t245 - 0.536D3 * t11
     #5 * t56 * t69 * x2 - 0.64D2 * t474 * t83 * t91 - 0.128D3 * t8 * t4
     #67 * t82 * t158 * t91 + 0.48D2 * t115 * t56 * t61 * x2 + 0.32D2 * 
     #t45 * t469 * t287 + 0.64D2 * t17 * t22 * x2 * t23 * x3 * t60 + 0.6
     #4D2 * t8 * t143 * t10 * t82 * t139 - 0.8D1 * t8 * t386 * t37 * t10
     # * t59 + 0.28792D5 * t479 * t56 * t37 * t164 - 0.74288D5 * t45 * t
     #24 * t388
      rrgg2qqbarh51J7 = -0.5D1 / 0.96D2 * wd * (t120 + t200 + t269 + t31
     #8 + t363 + t421 + t473 + t531) * nf / t1 / z / 0.3141592653589793D
     #1

      end function
  
 