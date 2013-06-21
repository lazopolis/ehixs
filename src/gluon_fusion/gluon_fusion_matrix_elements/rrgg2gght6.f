  
      subroutine rrgg2gght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh61J1  
      doubleprecision rrgg2ggh61J2  
      doubleprecision rrgg2ggh61J3  
      doubleprecision rrgg2ggh61J4  
      doubleprecision rrgg2ggh61J5  
      doubleprecision rrgg2ggh61J6  
      doubleprecision rrgg2ggh61J7  
      doubleprecision rrgg2gght6s1e1  
      doubleprecision rrgg2gght6s1e0  
      doubleprecision rrgg2gght6s1em1  
      doubleprecision rrgg2gght6s1em2  
      doubleprecision rrgg2gght6s1em3  
      doubleprecision rrgg2gght6s1em4  
      doubleprecision rrgg2gght6s2e1  
      doubleprecision rrgg2gght6s2e0  
      doubleprecision rrgg2gght6s2em1  
      doubleprecision rrgg2gght6s2em2  
      doubleprecision rrgg2gght6s2em3  
      doubleprecision rrgg2gght6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * 0.3141592653589793D1
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t10 = log(0.4D1 * t8)
      t11 = t10 ** 2
      t12 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t15 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t16 = t11 * t10
      t17 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t20 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t23 = s ** 2
      t25 = 0.1D1 / t23 / s
      t27 = 0.1D1 / z
      t28 = t27 * 0.3141592653589793D1
      t37 = lh ** 2
      t39 = 0.3141592653589793D1 ** 2
      t41 = 0.180D3 * t37 - 0.30D2 * t39
      t52 = 0.60D2 * lh * t39 - 0.2884936567583026D3 - 0.120D3 * t37 * l
     #h
      t57 = t39 ** 2
      t58 = t37 ** 2
      t67 = rrgg2ggh61J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t73 = t11 ** 2
      t80 = t25 * 0.3141592653589793D1
      t81 = x1 ** 2
      t82 = x3 * t81
      t85 = log(0.4D1 * t82 * t8)
      t87 = t85 ** 2
      t92 = -0.1D1 + x3
      t93 = 0.1D1 / t92
      t94 = t8 * t93
      t97 = log(-0.4D1 * t82 * t94)
      t99 = t97 ** 2
      t103 = cos(t5)
      t104 = z * t103
      t106 = Sqrt(-x3 * t92)
      t110 = 0.1D1 / (-z - x3 + 0.2D1 * t104 * t106)
      t127 = t17 * t110 + t17 * t27
      t129 = t80 * t41 * t127
      t131 = 0.1D1 / x3
      t133 = 0.1D1 / x1
      t136 = t27 * t25
      t137 = 0.3141592653589793D1 * t41
      t138 = t81 * t7
      t139 = t138 * t4
      t141 = log(0.4D1 * t139)
      t146 = t141 ** 2
      t157 = 0.3141592653589793D1 * t52
      t159 = t136 * t157 * t17
      t160 = 0.3141592653589793D1 * lh
      t171 = x2 ** 2
      t172 = t171 * x3
      t173 = t172 * t139
      t175 = log(0.4D1 * t173)
      t179 = t172 * t81
      t182 = log(-0.4D1 * t179 * t94)
      t194 = 0.1D1 / x2
      t195 = t133 * t194
      t198 = t171 * t81
      t201 = log(0.4D1 * t198 * t8)
      t203 = t201 ** 2
      t216 = t41 * t17
      t230 = x3 * t7
      t234 = log(-0.4D1 * t230 * t4 * t93)
      t238 = log(0.4D1 * t230 * t4)
      t242 = t238 ** 2
      t245 = t234 ** 2
      t281 = log(0.4D1 * t172 * t8)
      t283 = t281 ** 2
      t290 = log(-0.4D1 * t172 * t94)
      t292 = t290 ** 2
      t314 = t171 * t7
      t317 = log(0.4D1 * t314 * t4)
      t323 = t317 ** 2
      t345 = (t11 * t12 / 0.2D1 + t15 - t16 * t17 / 0.6D1 - t10 * t20) *
     # t25 * t28 * lh / 0.16D2 - (t20 - t10 * t12 + t11 * t17 / 0.2D1) *
     # t25 * t28 * t41 / 0.2880D4 - (t12 - t10 * t17) * t25 * t28 * t52 
     #/ 0.2880D4 - t17 * t25 * t28 * (t57 + 0.60D2 * t58 + 0.57698731351
     #66051D3 * lh - 0.60D2 * t37 * t39) / 0.2880D4 - (t67 - t16 * t12 /
     # 0.6D1 + t11 * t20 / 0.2D1 - t10 * t15 + t73 * t17 / 0.24D2) * t25
     # * t28 / 0.32D2 - (0.90D2 * t80 * ((t20 - t85 * t12 + t87 * t17 / 
     #0.2D1) * t27 + (t20 - t97 * t12 + t99 * t17 / 0.2D1) * t110) - 0.1
     #80D3 * t80 * lh * ((t12 - t85 * t17) * t27 + (t12 - t97 * t17) * t
     #110) + t129) * t131 * t133 / 0.1440D4 - (t136 * t137 * (t12 - t141
     # * t17) + 0.90D2 * t136 * 0.3141592653589793D1 * (t146 * t12 / 0.2
     #D1 + t15 - t146 * t141 * t17 / 0.6D1 - t141 * t20) + t159 - 0.180D
     #3 * t136 * t160 * (t20 - t141 * t12 + t146 * t17 / 0.2D1)) * t133 
     #/ 0.1440D4 - (0.90D2 * t80 * ((t12 - t175 * t17) * t27 + (t12 - t1
     #82 * t17) * t110) - 0.180D3 * t80 * lh * t127) * t131 * t195 / 0.7
     #20D3 + (-0.90D2 * t80 * (t20 - t201 * t12 + t203 * t17 / 0.2D1) * 
     #t27 + 0.180D3 * t80 * lh * (t12 - t201 * t17) * t27 - t80 * t216 *
     # t27) * t133 * t194 / 0.720D3 + ((0.90D2 * t80 * t20 - 0.180D3 * t
     #80 * lh * t12 + t80 * t216) * (t234 * t110 + t238 * t27) + 0.90D2 
     #* t80 * t17 * (t242 * t238 * t27 / 0.6D1 + t245 * t234 * t110 / 0.
     #6D1) + (t80 * t41 * t12 + 0.90D2 * t80 * t15 + t80 * t52 * t17 - 0
     #.180D3 * t80 * lh * t20) * (-t110 - t27) + (0.90D2 * t80 * t12 - 0
     #.180D3 * t80 * lh * t17) * (-t245 * t110 / 0.2D1 - t242 * t27 / 0.
     #2D1)) * t131 / 0.2880D4 - (0.90D2 * t80 * ((t20 - t281 * t12 + t28
     #3 * t17 / 0.2D1) * t27 + (t20 - t290 * t12 + t292 * t17 / 0.2D1) *
     # t110) - 0.180D3 * t80 * lh * ((t12 - t281 * t17) * t27 + (t12 - t
     #290 * t17) * t110) + t129) * t131 * t194 / 0.1440D4 + (-t80 * t41 
     #* (t12 - t317 * t17) * t27 - 0.90D2 * t80 * (t323 * t12 / 0.2D1 + 
     #t15 - t323 * t317 * t17 / 0.6D1 - t317 * t20) * t27 - t159 + 0.180
     #D3 * t80 * lh * (t20 - t317 * t12 + t323 * t17 / 0.2D1) * t27) * t
     #194 / 0.1440D4
      t346 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t345)
      t348 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t345)
      t350 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t345)
      t352 = t2 * x1
      t353 = -0.1D1 + x1
      t354 = x1 * z
      t355 = 0.1D1 - x1 + t354
      t356 = 0.1D1 / t355
      t358 = t2 * t353 * t356
      t359 = t1 ** 2
      t360 = s * t359
      t362 = x1 * t353 * t356
      t363 = t360 * t362
      t364 = -t353
      t365 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, t364, 0.10D1, 0.10
     #D1, x4)
      t366 = t27 * t365
      t367 = t82 * t7
      t368 = t353 ** 2
      t369 = t4 * t368
      t370 = t369 * t356
      t373 = log(0.4D1 * t367 * t370)
      t374 = t373 * t27
      t375 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, t364, 0.10D1, 0.10
     #D1, x4)
      t377 = t373 ** 2
      t379 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t364, 0.10D1, 0.10
     #D1, x4)
      t384 = t369 * t356 * t93
      t387 = log(-0.4D1 * t367 * t384)
      t388 = t387 * t355
      t390 = t387 ** 2
      t395 = x3 * x1
      t396 = t395 * z
      t397 = 0.3D1 * t396
      t398 = 0.2D1 * t395
      t399 = x3 * t355
      t401 = Sqrt(-t399 * t92)
      t404 = x1 * t3
      t405 = t395 * t3
      t407 = 0.2D1 * t82 * z
      t408 = t81 * t3
      t409 = t408 * x3
      t410 = t354 - t397 - z - t82 + t398 - x3 + 0.2D1 * t104 * t401 - t
     #404 + t405 + t407 - t409
      t411 = 0.1D1 / t410
      t416 = t27 * t375
      t418 = t355 * t375
      t429 = -t27 * t379 - t355 * t379 * t411
      t435 = (0.90D2 * t80 * (-t366 + t374 * t375 - t377 * t27 * t379 / 
     #0.2D1 - (t355 * t365 - t388 * t375 + t390 * t355 * t379 / 0.2D1) *
     # t411) - 0.180D3 * t80 * lh * (-t416 + t374 * t379 - (t418 - t388 
     #* t379) * t411) + t80 * t41 * t429) * t131 * t133 / 0.1440D4
      t438 = log(0.4D1 * t138 * t370)
      t440 = -t375 + t438 * t379
      t443 = t438 ** 2
      t446 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, t364, 0.10D1, 0.10
     #D1, x4)
      t451 = -t443 * t375 / 0.2D1 - t446 + t443 * t438 * t379 / 0.6D1 + 
     #t438 * t365
      t456 = t136 * t157 * t379
      t460 = -t365 + t438 * t375 - t443 * t379 / 0.2D1
      t467 = t172 * t138
      t470 = log(-0.4D1 * t467 * t384)
      t475 = t368 * t356
      t479 = log(0.4D1 * t179 * t8 * t475)
      t491 = (0.90D2 * t80 * (-(t418 - t470 * t355 * t379) * t411 - t416
     # + t479 * t27 * t379) - 0.180D3 * t80 * lh * t429) * t131 * t195 /
     # 0.720D3
      t492 = t198 * t7
      t495 = log(0.4D1 * t492 * t370)
      t496 = t495 * t27
      t498 = t495 ** 2
      t516 = (0.90D2 * t80 * (t366 - t496 * t375 + t498 * t27 * t379 / 0
     #.2D1) - 0.180D3 * t80 * lh * (t416 - t496 * t379) + t80 * t41 * t2
     #7 * t379) * t133 * t194 / 0.720D3
      t517 = -t435 - (t136 * t137 * t440 + 0.90D2 * t136 * 0.31415926535
     #89793D1 * t451 - t456 - 0.180D3 * t136 * t160 * t460) * t133 / 0.1
     #440D4 - t491 + t516
      t518 = FJET(XB1, XB2, s, 0.0D0, t352, -t358, 0.0D0, -t363, t517)
      t520 = x2 * s
      t521 = t520 * t1
      t522 = -0.1D1 + x2
      t523 = t522 * s
      t524 = t523 * t1
      t525 = -t522
      t526 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, 0.10
     #D1, x4)
      t527 = t8 * t522
      t530 = log(-0.4D1 * t172 * t527)
      t531 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, 0.10
     #D1, x4)
      t533 = t530 ** 2
      t534 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, 0.10
     #D1, x4)
      t538 = x2 * z
      t540 = 0.1D1 / (-x2 - z + t538)
      t552 = t80 * t41 * t534 * t540
      t557 = t4 * t522
      t560 = log(-0.4D1 * t314 * t557)
      t566 = t560 ** 2
      t569 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, 0.10
     #D1, x4)
      t594 = log(-0.4D1 * t179 * t527)
      t610 = log(-0.4D1 * t198 * t527)
      t612 = t610 ** 2
      t629 = -(0.90D2 * t80 * (t526 - t530 * t531 + t533 * t534 / 0.2D1)
     # * t540 - 0.180D3 * t80 * lh * (t531 - t530 * t534) * t540 + t552)
     # * t131 * t194 / 0.1440D4 + (-t80 * t41 * (t531 - t560 * t534) * t
     #540 - 0.90D2 * t80 * (t566 * t531 / 0.2D1 + t569 - t566 * t560 * t
     #534 / 0.6D1 - t560 * t526) * t540 - t80 * t52 * t534 * t540 + 0.18
     #0D3 * t80 * lh * (t526 - t560 * t531 + t566 * t534 / 0.2D1) * t540
     #) * t194 / 0.1440D4 - (0.90D2 * t80 * (t531 - t594 * t534) * t540 
     #- 0.180D3 * t80 * lh * t534 * t540) * t131 * t195 / 0.720D3 + (-0.
     #90D2 * t80 * (t526 - t610 * t531 + t612 * t534 / 0.2D1) * t540 + 0
     #.180D3 * t80 * lh * (t531 - t610 * t534) * t540 - t552) * t133 * t
     #194 / 0.720D3
      t630 = FJET(XB1, XB2, s, 0.0D0, t521, 0.0D0, -t524, 0.0D0, t629)
      t632 = x2 * x3
      t635 = Sqrt(x3 * t522 * t92)
      t636 = t103 * t635
      t638 = 0.2D1 * t636 * x2
      t640 = 0.1D1 - x3 + t632
      t641 = 0.1D1 / t640
      t643 = t2 * (0.1D1 - x3 - x2 + t632 + t172 + t638) * t641
      t648 = t2 * x2 * (-0.1D1 + t632 + 0.2D1 * t636) * t641
      t649 = t92 * t641
      t650 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, -t64
     #9, x4)
      t651 = t640 ** 2
      t652 = 0.1D1 / t651
      t654 = t557 * t92 * t652
      t657 = log(0.4D1 * t467 * t654)
      t658 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, -t64
     #9, x4)
      t662 = z * t171 * x3
      t663 = t632 * z
      t670 = 0.1D1 / (x2 + z - t538 + t662 - t663 + x3 - t172 - t638 - 0
     #.2D1 * t104 * t635 + 0.2D1 * t104 * t635 * x2)
      t682 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t525, -t64
     #9, x4)
      t686 = log(0.4D1 * t172 * t7 * t654)
      t688 = t686 ** 2
      t708 = -(0.90D2 * t80 * (t650 - t657 * t658) * t670 - 0.180D3 * t8
     #0 * lh * t658 * t670) * t131 * t195 / 0.720D3 - (0.90D2 * t80 * (t
     #682 - t686 * t650 + t688 * t658 / 0.2D1) * t670 - 0.180D3 * t80 * 
     #lh * (t650 - t686 * t658) * t670 + t80 * t41 * t658 * t670) * t131
     # * t194 / 0.1440D4
      t709 = FJET(XB1, XB2, s, 0.0D0, t643, 0.0D0, -t648, 0.0D0, t708)
      t711 = t1 * t353
      t713 = t523 * t711 * t356
      t714 = t520 * t711
      t716 = t360 * t522 * t362
      t717 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, t364, t525, 0.10D1
     #, x4)
      t719 = t369 * t356 * t522
      t722 = log(-0.4D1 * t467 * t719)
      t723 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t364, t525, 0.10D1
     #, x4)
      t726 = x1 * x2
      t727 = t726 * z
      t729 = 0.1D1 / (x2 + z - t538 - t726 + t727)
      t740 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, t364, t525, 0.10D1
     #, x4)
      t743 = log(-0.4D1 * t492 * t719)
      t745 = t743 ** 2
      t765 = -(0.90D2 * t80 * (t717 - t722 * t723) * t729 - 0.180D3 * t8
     #0 * lh * t723 * t729) * t131 * t195 / 0.720D3 + (-0.90D2 * t80 * (
     #t740 - t743 * t717 + t745 * t723 / 0.2D1) * t729 + 0.180D3 * t80 *
     # lh * (t717 - t743 * t723) * t729 - t80 * t41 * t723 * t729) * t13
     #3 * t194 / 0.720D3
      t766 = FJET(XB1, XB2, s, 0.0D0, t713, t352, -t714, t716, t765)
      t768 = FJET(XB1, XB2, s, 0.0D0, -t524, 0.0D0, t521, 0.0D0, t629)
      t770 = FJET(XB1, XB2, s, 0.0D0, -t358, t352, 0.0D0, -t363, t517)
      t772 = FJET(XB1, XB2, s, 0.0D0, -t648, 0.0D0, t643, 0.0D0, t708)
      t774 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t345)
      t790 = -t435 - (t136 * t137 * t440 + 0.90D2 * t136 * 0.31415926535
     #89793D1 * t451 - t456 - 0.180D3 * t136 * t160 * t460) * t133 / 0.1
     #440D4 - t491 + t516
      t791 = FJET(XB1, XB2, s, t352, 0.0D0, 0.0D0, -t358, -t363, t790)
      t793 = t346 * t345 + t348 * t345 + t350 * t345 + t518 * t517 + t63
     #0 * t629 + t709 * t708 + t766 * t765 + t768 * t629 + t770 * t517 +
     # t772 * t708 + t774 * t345 + t791 * t790
      t794 = FJET(XB1, XB2, s, t352, -t714, 0.0D0, t713, t716, t765)
      t796 = FJET(XB1, XB2, s, t521, 0.0D0, -t524, 0.0D0, 0.0D0, t629)
      t798 = FJET(XB1, XB2, s, t643, 0.0D0, -t648, 0.0D0, 0.0D0, t708)
      t800 = FJET(XB1, XB2, s, t713, 0.0D0, -t714, t352, t716, t765)
      t803 = t352 * t632 * t641
      t804 = t2 * t353
      t805 = t354 * t172
      t806 = t522 * t92
      t808 = Sqrt(t399 * t806)
      t809 = t103 * t808
      t811 = 0.2D1 * t809 * x2
      t813 = x1 * t171 * x3
      t817 = t804 * (t805 + t811 - x2 + t632 + t172 + 0.1D1 - x3 - t813)
     # * t356 * t641
      t821 = t92 * s * t1 * x1 * t641
      t827 = t804 * x2 * (-0.1D1 + t632 + x1 - t395 - t354 + t396 + 0.2D
     #1 * t809) * t356 * t641
      t828 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, t364, t525, -t649,
     # x4)
      t834 = log(0.4D1 * t173 * t475 * t806 * t652)
      t836 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t364, t525, -t649,
     # x4)
      t844 = x2 * t81
      t849 = t811 - t813 - t395 * x2 + 0.2D1 * t104 * t808 + t82 * x2 + 
     #t726 * t3 + 0.2D1 * t844 * z - t844 * t3 - z - x2 + t354 - t82 + t
     #398 - t404 + t538 + t172 + 0.2D1 * t726
      t867 = t805 + 0.2D1 * t395 * t538 + t408 * t632 - 0.2D1 * t82 * t5
     #38 - t395 * x2 * t3 - 0.2D1 * t104 * t808 * x2 - 0.2D1 * t809 * t7
     #26 - t397 + t405 + t407 - t409 - 0.3D1 * t727 + 0.2D1 * t104 * t80
     #8 * x1 * x2 - t662 + t663 - t844 - x3
      t869 = 0.1D1 / (t849 + t867)
      t878 = 0.90D2 * t80 * (t355 * t828 - t834 * t355 * t836) * t869 - 
     #0.180D3 * t80 * lh * t355 * t836 * t869
      t881 = t878 * t131 * t195 / 0.720D3
      t882 = FJET(XB1, XB2, s, t803, -t817, -t821, t827, t716, -t881)
      t885 = t131 * t133 * t194
      t888 = FJET(XB1, XB2, s, t827, -t821, -t817, t803, t716, -t881)
      t892 = FJET(XB1, XB2, s, -t524, 0.0D0, t521, 0.0D0, 0.0D0, t629)
      t894 = FJET(XB1, XB2, s, -t358, 0.0D0, 0.0D0, t352, -t363, t790)
      t896 = FJET(XB1, XB2, s, -t714, t352, t713, 0.0D0, t716, t765)
      t898 = FJET(XB1, XB2, s, -t648, 0.0D0, t643, 0.0D0, 0.0D0, t708)
      t900 = FJET(XB1, XB2, s, -t821, t827, t803, -t817, t716, -t881)
      t904 = FJET(XB1, XB2, s, -t817, t803, t827, -t821, t716, -t881)
      t908 = t794 * t765 + t796 * t629 + t798 * t708 + t800 * t765 - t88
     #2 * t878 * t885 / 0.720D3 - t888 * t878 * t885 / 0.720D3 + t892 * 
     #t629 + t894 * t790 + t896 * t765 + t898 * t708 - t900 * t878 * t88
     #5 / 0.720D3 - t904 * t878 * t885 / 0.720D3
      rrgg2gght6s1e1 = t793 + t908

      end function



      doubleprecision function rrgg2gght6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
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
      t22 = z * t21
      t24 = Sqrt(-x3 * t14)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t22 * t24)
      t32 = log(0.4D1 * t11 * t13)
      t33 = t32 ** 2
      t34 = 0.1D1 / z
      t41 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t44 = lh * t7
      t52 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t58 = lh ** 2
      t60 = 0.3141592653589793D1 ** 2
      t62 = 0.180D3 * t58 - 0.30D2 * t60
      t63 = t62 * t7
      t69 = 0.1D1 / x3
      t72 = t13 * t10
      t74 = log(0.4D1 * t72)
      t76 = t74 ** 2
      t81 = t34 * 0.3141592653589793D1
      t85 = t34 * t5
      t97 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t112 = x2 ** 2
      t113 = t112 * x3
      t116 = log(0.4D1 * t113 * t72)
      t120 = t72 * t15
      t123 = log(-0.4D1 * t113 * t120)
      t132 = t7 * t28 + t7 * t34
      t135 = 0.180D3 * t6 * lh * t132
      t138 = 0.1D1 / x2
      t141 = t112 * t10
      t144 = log(0.4D1 * t141 * t13)
      t146 = t144 ** 2
      t160 = t6 * t63 * t34
      t164 = x1 ** 2
      t165 = x3 * t164
      t168 = log(0.4D1 * t165 * t72)
      t174 = log(-0.4D1 * t165 * t120)
      t183 = 0.1D1 / x1
      t188 = t69 * t183 * t138
      t191 = t112 * t164
      t194 = log(0.4D1 * t191 * t72)
      t207 = t164 * t10
      t210 = log(0.4D1 * t207 * t13)
      t212 = t210 ** 2
      t219 = 0.3141592653589793D1 * lh
      t228 = (0.90D2 * t6 * t7 * (-t20 * t28 / 0.2D1 - t33 * t34 / 0.2D1
     #) + (0.90D2 * t6 * t41 - 0.180D3 * t6 * t44) * (t19 * t28 + t32 * 
     #t34) + (0.90D2 * t6 * t52 - 0.180D3 * t6 * lh * t41 + t6 * t63) * 
     #(-t28 - t34)) * t69 / 0.2880D4 + (t52 - t74 * t41 + t76 * t7 / 0.2
     #D1) * t5 * t81 * lh / 0.16D2 - t85 * 0.3141592653589793D1 * (0.60D
     #2 * lh * t60 - 0.2884936567583026D3 - 0.120D3 * t58 * lh) * t7 / 0
     #.2880D4 - (t76 * t41 / 0.2D1 + t97 - t76 * t74 * t7 / 0.6D1 - t74 
     #* t52) * t5 * t81 / 0.32D2 - (t41 - t74 * t7) * t5 * t81 * t62 / 0
     #.2880D4 - (0.90D2 * t6 * ((t41 - t116 * t7) * t34 + (t41 - t123 * 
     #t7) * t28) - t135) * t69 * t138 / 0.1440D4 + (-0.90D2 * t6 * (t52 
     #- t144 * t41 + t146 * t7 / 0.2D1) * t34 + 0.180D3 * t6 * lh * (t41
     # - t144 * t7) * t34 - t160) * t138 / 0.1440D4 - (0.90D2 * t6 * ((t
     #41 - t168 * t7) * t34 + (t41 - t174 * t7) * t28) - t135) * t69 * t
     #183 / 0.1440D4 - t6 * t132 * t188 / 0.8D1 + (-0.90D2 * t6 * (t41 -
     # t194 * t7) * t34 + 0.180D3 * t6 * t44 * t34) * t183 * t138 / 0.72
     #0D3 - (0.90D2 * t85 * 0.3141592653589793D1 * (t52 - t210 * t41 + t
     #212 * t7 / 0.2D1) - 0.180D3 * t85 * t219 * (t41 - t210 * t7) + t16
     #0) * t183 / 0.1440D4
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t228)
      t231 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t228)
      t233 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t228)
      t235 = t2 * x1
      t236 = -0.1D1 + x1
      t237 = x1 * z
      t238 = 0.1D1 - x1 + t237
      t239 = 0.1D1 / t238
      t241 = t2 * t236 * t239
      t242 = t1 ** 2
      t243 = s * t242
      t245 = x1 * t236 * t239
      t246 = t243 * t245
      t247 = -t236
      t248 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, t247, 0.10D1, 0.10
     #D1, x4)
      t249 = t34 * t248
      t250 = t165 * t10
      t251 = t236 ** 2
      t252 = t13 * t251
      t253 = t252 * t239
      t256 = log(0.4D1 * t250 * t253)
      t258 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t247, 0.10D1, 0.10
     #D1, x4)
      t265 = log(-0.4D1 * t250 * t252 * t239 * t15)
      t269 = x3 * x1
      t270 = t269 * z
      t271 = 0.3D1 * t270
      t272 = 0.2D1 * t269
      t273 = x3 * t238
      t275 = Sqrt(-t273 * t14)
      t278 = x1 * t12
      t279 = t269 * t12
      t281 = 0.2D1 * t165 * z
      t282 = t164 * t12
      t283 = t282 * x3
      t284 = t237 - t271 - z - t165 + t272 - x3 + 0.2D1 * t22 * t275 - t
     #278 + t279 + t281 - t283
      t285 = 0.1D1 / t284
      t293 = -t34 * t258 - t238 * t258 * t285
      t300 = (0.90D2 * t6 * (-t249 + t256 * t34 * t258 - (t238 * t248 - 
     #t265 * t238 * t258) * t285) - 0.180D3 * t6 * lh * t293) * t69 * t1
     #83 / 0.1440D4
      t303 = t6 * t293 * t188 / 0.8D1
      t304 = t191 * t10
      t307 = log(0.4D1 * t304 * t253)
      t320 = (0.90D2 * t6 * (t249 - t307 * t34 * t258) - 0.180D3 * t6 * 
     #lh * t34 * t258) * t183 * t138 / 0.720D3
      t321 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, t247, 0.10D1, 0.10
     #D1, x4)
      t324 = log(0.4D1 * t207 * t253)
      t326 = t324 ** 2
      t329 = -t321 + t324 * t248 - t326 * t258 / 0.2D1
      t334 = -t248 + t324 * t258
      t340 = t6 * t62 * t34 * t258
      t344 = -t300 - t303 + t320 - (0.90D2 * t85 * 0.3141592653589793D1 
     #* t329 - 0.180D3 * t85 * t219 * t334 - t340) * t183 / 0.1440D4
      t345 = FJET(XB1, XB2, s, 0.0D0, t235, -t241, 0.0D0, -t246, t344)
      t347 = x2 * s
      t348 = t347 * t1
      t349 = -0.1D1 + x2
      t350 = t349 * s
      t351 = t350 * t1
      t352 = -t349
      t353 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t352, 0.10
     #D1, x4)
      t354 = t72 * t349
      t357 = log(-0.4D1 * t113 * t354)
      t358 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t352, 0.10
     #D1, x4)
      t361 = x2 * z
      t363 = 0.1D1 / (-x2 - z + t361)
      t370 = 0.180D3 * t6 * lh * t358 * t363
      t375 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t352, 0.10
     #D1, x4)
      t376 = t13 * t349
      t379 = log(-0.4D1 * t141 * t376)
      t381 = t379 ** 2
      t402 = t183 * t138
      t408 = log(-0.4D1 * t191 * t354)
      t418 = -(0.90D2 * t6 * (t353 - t357 * t358) * t363 - t370) * t69 *
     # t138 / 0.1440D4 + (-0.90D2 * t6 * (t375 - t379 * t353 + t381 * t3
     #58 / 0.2D1) * t363 + 0.180D3 * t6 * lh * (t353 - t379 * t358) * t3
     #63 - t6 * t62 * t358 * t363) * t138 / 0.1440D4 - t6 * t358 * t363 
     #* t69 * t402 / 0.8D1 + (-0.90D2 * t6 * (t353 - t408 * t358) * t363
     # + t370) * t183 * t138 / 0.720D3
      t419 = FJET(XB1, XB2, s, 0.0D0, t348, 0.0D0, -t351, 0.0D0, t418)
      t421 = x2 * x3
      t424 = Sqrt(x3 * t349 * t14)
      t425 = t21 * t424
      t427 = 0.2D1 * t425 * x2
      t429 = 0.1D1 - x3 + t421
      t430 = 0.1D1 / t429
      t432 = t2 * (0.1D1 - x3 - x2 + t421 + t113 + t427) * t430
      t437 = t2 * x2 * (-0.1D1 + t421 + 0.2D1 * t425) * t430
      t438 = t14 * t430
      t439 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t352, -t43
     #8, x4)
      t441 = t429 ** 2
      t447 = log(0.4D1 * t113 * t10 * t376 * t14 / t441)
      t448 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t352, -t43
     #8, x4)
      t452 = z * t112 * x3
      t453 = t421 * z
      t460 = 0.1D1 / (x2 + z - t361 + t452 - t453 + x3 - t113 - t427 - 0
     #.2D1 * t22 * t424 + 0.2D1 * t22 * t424 * x2)
      t477 = -(0.90D2 * t6 * (t439 - t447 * t448) * t460 - 0.180D3 * t6 
     #* lh * t448 * t460) * t69 * t138 / 0.1440D4 - t6 * t448 * t460 * t
     #69 * t402 / 0.8D1
      t478 = FJET(XB1, XB2, s, 0.0D0, t432, 0.0D0, -t437, 0.0D0, t477)
      t480 = t1 * t236
      t482 = t350 * t480 * t239
      t483 = t347 * t480
      t485 = t243 * t349 * t245
      t486 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t247, t352, 0.10D1
     #, x4)
      t488 = x1 * x2
      t489 = t488 * z
      t491 = 0.1D1 / (x2 + z - t361 - t488 + t489)
      t496 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, t247, t352, 0.10D1
     #, x4)
      t502 = log(-0.4D1 * t304 * t13 * t239 * t251 * t349)
      t516 = -t6 * t486 * t491 * t69 * t402 / 0.8D1 + (-0.90D2 * t6 * (t
     #496 - t502 * t486) * t491 + 0.180D3 * t6 * lh * t486 * t491) * t18
     #3 * t138 / 0.720D3
      t517 = FJET(XB1, XB2, s, 0.0D0, t482, t235, -t483, t485, t516)
      t519 = FJET(XB1, XB2, s, 0.0D0, -t351, 0.0D0, t348, 0.0D0, t418)
      t521 = FJET(XB1, XB2, s, 0.0D0, -t241, t235, 0.0D0, -t246, t344)
      t523 = FJET(XB1, XB2, s, 0.0D0, -t437, 0.0D0, t432, 0.0D0, t477)
      t525 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t228)
      t538 = -t300 - t303 + t320 - (0.90D2 * t85 * 0.3141592653589793D1 
     #* t329 - 0.180D3 * t85 * t219 * t334 - t340) * t183 / 0.1440D4
      t539 = FJET(XB1, XB2, s, t235, 0.0D0, 0.0D0, -t241, -t246, t538)
      t541 = t229 * t228 + t231 * t228 + t233 * t228 + t345 * t344 + t41
     #9 * t418 + t478 * t477 + t517 * t516 + t519 * t418 + t521 * t344 +
     # t523 * t477 + t525 * t228 + t539 * t538
      t542 = FJET(XB1, XB2, s, t235, -t483, 0.0D0, t482, t485, t516)
      t544 = FJET(XB1, XB2, s, t348, 0.0D0, -t351, 0.0D0, 0.0D0, t418)
      t546 = FJET(XB1, XB2, s, t432, 0.0D0, -t437, 0.0D0, 0.0D0, t477)
      t548 = FJET(XB1, XB2, s, t482, 0.0D0, -t483, t235, t485, t516)
      t551 = t235 * t421 * t430
      t552 = t2 * t236
      t553 = t237 * t113
      t556 = Sqrt(t273 * t349 * t14)
      t557 = t21 * t556
      t559 = 0.2D1 * t557 * x2
      t561 = x1 * t112 * x3
      t565 = t552 * (t553 + t559 - x2 + t421 + t113 + 0.1D1 - x3 - t561)
     # * t239 * t430
      t569 = t14 * s * t1 * x1 * t430
      t575 = t552 * x2 * (-0.1D1 + t421 + x1 - t269 - t237 + t270 + 0.2D
     #1 * t557) * t239 * t430
      t576 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t247, t352, -t438,
     # x4)
      t592 = t237 - t165 + t272 - t278 - t271 + t279 + t281 - t283 + t55
     #3 + 0.2D1 * t269 * t361 + t282 * t421 - 0.2D1 * t165 * t361 - t269
     # * x2 * t12 - 0.2D1 * t22 * t556 * x2 - 0.2D1 * t557 * t488 + t113
     # + 0.2D1 * t488
      t593 = x2 * t164
      t607 = -z - x3 - t593 + 0.2D1 * t22 * t556 * x1 * x2 - 0.3D1 * t48
     #9 + t361 - t452 + t453 + t559 - t561 - t269 * x2 + 0.2D1 * t22 * t
     #556 + t165 * x2 + t488 * t12 + 0.2D1 * t593 * z - t593 * t12 - x2
      t609 = 0.1D1 / (t592 + t607)
      t613 = t6 * t238 * t576 * t609 * t69 * t402 / 0.8D1
      t614 = FJET(XB1, XB2, s, t551, -t565, -t569, t575, t485, -t613)
      t616 = 0.3141592653589793D1 * t238
      t619 = t576 * t609 * t188
      t622 = FJET(XB1, XB2, s, t575, -t569, -t565, t551, t485, -t613)
      t627 = FJET(XB1, XB2, s, -t351, 0.0D0, t348, 0.0D0, 0.0D0, t418)
      t629 = FJET(XB1, XB2, s, -t241, 0.0D0, 0.0D0, t235, -t246, t538)
      t631 = FJET(XB1, XB2, s, -t483, t235, t482, 0.0D0, t485, t516)
      t633 = FJET(XB1, XB2, s, -t437, 0.0D0, t432, 0.0D0, 0.0D0, t477)
      t635 = FJET(XB1, XB2, s, -t569, t575, t551, -t565, t485, -t613)
      t640 = FJET(XB1, XB2, s, -t565, t551, t575, -t569, t485, -t613)
      t645 = t542 * t516 + t544 * t418 + t546 * t477 + t548 * t516 - t61
     #4 * t5 * t616 * t619 / 0.8D1 - t622 * t5 * t616 * t619 / 0.8D1 + t
     #627 * t418 + t629 * t538 + t631 * t516 + t633 * t477 - t635 * t5 *
     # t616 * t619 / 0.8D1 - t640 * t5 * t616 * t619 / 0.8D1
      rrgg2gght6s1e0 = t541 + t645

      end function



      doubleprecision function rrgg2gght6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = -0.1D1 + x3
      t19 = log(-0.4D1 * t11 * t13 / t14)
      t20 = cos(t8)
      t21 = z * t20
      t23 = Sqrt(-x3 * t14)
      t27 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t23)
      t31 = log(0.4D1 * t11 * t13)
      t32 = 0.1D1 / z
      t38 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t41 = lh * t7
      t48 = 0.1D1 / x3
      t53 = log(0.4D1 * t13 * t10)
      t57 = t32 * 0.3141592653589793D1
      t61 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t63 = t53 ** 2
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t82 = (t7 * t27 + t7 * t32) * t48
      t83 = 0.1D1 / x2
      t87 = x2 ** 2
      t88 = t87 * t10
      t91 = log(0.4D1 * t88 * t13)
      t99 = 0.180D3 * t6 * t41 * t32
      t105 = 0.1D1 / x1
      t110 = t32 * t5
      t111 = x1 ** 2
      t112 = t111 * t10
      t115 = log(0.4D1 * t112 * t13)
      t127 = (0.90D2 * t6 * t7 * (t19 * t27 + t31 * t32) + (0.90D2 * t6 
     #* t38 - 0.180D3 * t6 * t41) * (-t27 - t32)) * t48 / 0.2880D4 + (t3
     #8 - t53 * t7) * t5 * t57 * lh / 0.16D2 - (t61 - t53 * t38 + t63 * 
     #t7 / 0.2D1) * t5 * t57 / 0.32D2 - t6 * (0.180D3 * t70 - 0.30D2 * t
     #72) * t7 * t32 / 0.2880D4 - t6 * t82 * t83 / 0.16D2 + (-0.90D2 * t
     #6 * (t38 - t91 * t7) * t32 + t99) * t83 / 0.1440D4 - t7 * t5 * t32
     # * 0.3141592653589793D1 * t105 * t83 / 0.8D1 - (0.90D2 * t110 * 0.
     #3141592653589793D1 * (t38 - t115 * t7) - t99) * t105 / 0.1440D4 - 
     #t6 * t82 * t105 / 0.16D2
      t128 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t127)
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t127)
      t132 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t127)
      t134 = t2 * x1
      t135 = -0.1D1 + x1
      t136 = x1 * z
      t137 = 0.1D1 - x1 + t136
      t138 = 0.1D1 / t137
      t140 = t2 * t135 * t138
      t141 = t1 ** 2
      t142 = s * t141
      t144 = x1 * t135 * t138
      t145 = t142 * t144
      t147 = -t135
      t148 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t147, 0.10D1, 0.10
     #D1, x4)
      t152 = t6 * t32 * t148 * t105 * t83 / 0.8D1
      t153 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, t147, 0.10D1, 0.10
     #D1, x4)
      t155 = t135 ** 2
      t159 = log(0.4D1 * t112 * t13 * t138 * t155)
      t161 = -t153 + t159 * t148
      t168 = 0.180D3 * t6 * lh * t32 * t148
      t174 = x3 * x1
      t177 = x3 * t111
      t181 = Sqrt(-x3 * t137 * t14)
      t190 = t136 - 0.3D1 * t174 * z - z - t177 + 0.2D1 * t174 - x3 + 0.
     #2D1 * t21 * t181 - x1 * t12 + t174 * t12 + 0.2D1 * t177 * z - t111
     # * t12 * x3
      t197 = t6 * (-t32 * t148 - t137 * t148 / t190) * t48 * t105 / 0.16
     #D2
      t198 = t152 - (0.90D2 * t110 * 0.3141592653589793D1 * t161 + t168)
     # * t105 / 0.1440D4 - t197
      t199 = FJET(XB1, XB2, s, 0.0D0, t134, -t140, 0.0D0, -t145, t198)
      t201 = x2 * s
      t202 = t201 * t1
      t203 = -0.1D1 + x2
      t204 = t203 * s
      t205 = t204 * t1
      t206 = -t203
      t207 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t206, 0.10
     #D1, x4)
      t208 = t6 * t207
      t209 = x2 * z
      t211 = 0.1D1 / (-x2 - z + t209)
      t216 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t206, 0.10
     #D1, x4)
      t220 = log(-0.4D1 * t88 * t13 * t203)
      t237 = -t208 * t211 * t48 * t83 / 0.16D2 + (-0.90D2 * t6 * (t216 -
     # t220 * t207) * t211 + 0.180D3 * t6 * lh * t207 * t211) * t83 / 0.
     #1440D4 - t208 * t211 * t105 * t83 / 0.8D1
      t238 = FJET(XB1, XB2, s, 0.0D0, t202, 0.0D0, -t205, 0.0D0, t237)
      t240 = x2 * x3
      t241 = t87 * x3
      t244 = Sqrt(x3 * t203 * t14)
      t245 = t20 * t244
      t247 = 0.2D1 * t245 * x2
      t250 = 0.1D1 / (0.1D1 - x3 + t240)
      t252 = t2 * (0.1D1 - x3 - x2 + t240 + t241 + t247) * t250
      t257 = t2 * x2 * (-0.1D1 + t240 + 0.2D1 * t245) * t250
      t259 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t206, -t14
     # * t250, x4)
      t270 = 0.1D1 / (x2 + z - t209 + z * t87 * x3 - t240 * z + x3 - t24
     #1 - t247 - 0.2D1 * t21 * t244 + 0.2D1 * t21 * t244 * x2)
      t274 = t6 * t259 * t270 * t48 * t83 / 0.16D2
      t275 = FJET(XB1, XB2, s, 0.0D0, t252, 0.0D0, -t257, 0.0D0, -t274)
      t280 = t259 * t270 * t48 * t83
      t283 = t1 * t135
      t285 = t204 * t283 * t138
      t286 = t201 * t283
      t288 = t142 * t203 * t144
      t289 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, t147, t206, 0.10D1
     #, x4)
      t291 = x1 * x2
      t294 = 0.1D1 / (x2 + z - t209 - t291 + t291 * z)
      t298 = t6 * t289 * t294 * t105 * t83 / 0.8D1
      t299 = FJET(XB1, XB2, s, 0.0D0, t285, t134, -t286, t288, -t298)
      t304 = t289 * t294 * t105 * t83
      t307 = FJET(XB1, XB2, s, 0.0D0, -t205, 0.0D0, t202, 0.0D0, t237)
      t309 = FJET(XB1, XB2, s, 0.0D0, -t140, t134, 0.0D0, -t145, t198)
      t311 = FJET(XB1, XB2, s, 0.0D0, -t257, 0.0D0, t252, 0.0D0, -t274)
      t316 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t127)
      t325 = t152 - (0.90D2 * t110 * 0.3141592653589793D1 * t161 + t168)
     # * t105 / 0.1440D4 - t197
      t326 = FJET(XB1, XB2, s, t134, 0.0D0, 0.0D0, -t140, -t145, t325)
      t328 = FJET(XB1, XB2, s, t134, -t286, 0.0D0, t285, t288, -t298)
      t333 = FJET(XB1, XB2, s, t202, 0.0D0, -t205, 0.0D0, 0.0D0, t237)
      t335 = FJET(XB1, XB2, s, t252, 0.0D0, -t257, 0.0D0, 0.0D0, -t274)
      t340 = FJET(XB1, XB2, s, t285, 0.0D0, -t286, t134, t288, -t298)
      t345 = FJET(XB1, XB2, s, -t205, 0.0D0, t202, 0.0D0, 0.0D0, t237)
      t347 = FJET(XB1, XB2, s, -t140, 0.0D0, 0.0D0, t134, -t145, t325)
      t349 = FJET(XB1, XB2, s, -t257, 0.0D0, t252, 0.0D0, 0.0D0, -t274)
      t354 = FJET(XB1, XB2, s, -t286, t134, t285, 0.0D0, t288, -t298)
      rrgg2gght6s1em1 = t128 * t127 + t130 * t127 + t132 * t127 + t199 *
     # t198 + t238 * t237 - t275 * t5 * 0.3141592653589793D1 * t280 / 0.
     #16D2 - t299 * t5 * 0.3141592653589793D1 * t304 / 0.8D1 + t307 * t2
     #37 + t309 * t198 - t311 * t5 * 0.3141592653589793D1 * t280 / 0.16D
     #2 + t316 * t127 + t326 * t325 - t328 * t5 * 0.3141592653589793D1 *
     # t304 / 0.8D1 + t333 * t237 - t335 * t5 * 0.3141592653589793D1 * t
     #280 / 0.16D2 - t340 * t5 * 0.3141592653589793D1 * t304 / 0.8D1 + t
     #345 * t237 + t347 * t325 - t349 * t5 * 0.3141592653589793D1 * t280
     # / 0.16D2 - t354 * t5 * 0.3141592653589793D1 * t304 / 0.8D1

      end function



      doubleprecision function rrgg2gght6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = cos(t8)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t18 = 0.1D1 / z
      t25 = t7 * t5
      t26 = t18 * 0.3141592653589793D1
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t39 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t40 = z ** 2
      t42 = Sin(t8)
      t43 = t42 ** 2
      t46 = log(0.4D1 / t40 * t43)
      t52 = t6 * t7 * (-0.1D1 / (-z - x3 + 0.2D1 * z * t9 * t13) - t18) 
     #/ x3 / 0.32D2 - t25 * t26 * t27 / 0.16D2 - t25 * t26 * t31 / 0.16D
     #2 + t6 * lh * t7 * t18 / 0.16D2 - (t39 - t46 * t7) * t5 * t26 / 0.
     #32D2
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t52)
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t52)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t52)
      t59 = t2 * x1
      t60 = -0.1D1 + x1
      t63 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t65 = t2 * t60 * t63
      t66 = t1 ** 2
      t70 = s * t66 * x1 * t60 * t63
      t72 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, -t60, 0.10D1, 0.10D
     #1, x4)
      t74 = t18 * t72 * t31
      t76 = t6 * t74 / 0.16D2
      t77 = FJET(XB1, XB2, s, 0.0D0, t59, -t65, 0.0D0, -t70, t76)
      t83 = x2 * s * t1
      t84 = -0.1D1 + x2
      t86 = t84 * s * t1
      t88 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, -t84, 0.10D
     #1, x4)
      t93 = t88 / (-x2 - z + x2 * z) * t27
      t95 = t6 * t93 / 0.16D2
      t96 = FJET(XB1, XB2, s, 0.0D0, t83, 0.0D0, -t86, 0.0D0, -t95)
      t101 = FJET(XB1, XB2, s, 0.0D0, -t86, 0.0D0, t83, 0.0D0, -t95)
      t106 = FJET(XB1, XB2, s, 0.0D0, -t65, t59, 0.0D0, -t70, t76)
      t111 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t52)
      t113 = FJET(XB1, XB2, s, t59, 0.0D0, 0.0D0, -t65, -t70, t76)
      t118 = FJET(XB1, XB2, s, t83, 0.0D0, -t86, 0.0D0, 0.0D0, -t95)
      t123 = FJET(XB1, XB2, s, -t86, 0.0D0, t83, 0.0D0, 0.0D0, -t95)
      t128 = FJET(XB1, XB2, s, -t65, 0.0D0, 0.0D0, t59, -t70, t76)
      rrgg2gght6s1em2 = t53 * t52 + t55 * t52 + t57 * t52 + t77 * t5 * 0
     #.3141592653589793D1 * t74 / 0.16D2 - t96 * t5 * 0.3141592653589793
     #D1 * t93 / 0.16D2 - t101 * t5 * 0.3141592653589793D1 * t93 / 0.16D
     #2 + t106 * t5 * 0.3141592653589793D1 * t74 / 0.16D2 + t111 * t52 +
     # t113 * t5 * 0.3141592653589793D1 * t74 / 0.16D2 - t118 * t5 * 0.3
     #141592653589793D1 * t93 / 0.16D2 - t123 * t5 * 0.3141592653589793D
     #1 * t93 / 0.16D2 + t128 * t5 * 0.3141592653589793D1 * t74 / 0.16D2

      end function



      doubleprecision function rrgg2gght6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t8 = 0.1D1 / z
      t11 = t3 * t6 * t8 * 0.3141592653589793D1 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t6 * t8 * 0.3141592653589793D1
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2gght6s1em3 = -t12 * t3 * t15 / 0.32D2 - t17 * t3 * t15 / 0.32
     #D2 - t20 * t3 * t15 / 0.32D2 - t23 * t3 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2gght6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      rrgg2gght6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght6s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = rrgg2ggh61J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t20 = log(0.4D1 * t18)
      t21 = t20 ** 2
      t22 = t21 * t3
      t23 = t6 * 0.3141592653589793D1
      t24 = t23 * lh
      t27 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t33 = 0.60D2 * lh * t27 - 0.2884936567583026D3 - 0.120D3 * t30 * l
     #h
      t34 = 0.3141592653589793D1 * t33
      t37 = t21 * t20 * t3
      t40 = t20 * t3
      t43 = 0.180D3 * t30 - 0.30D2 * t27
      t44 = t23 * t43
      t47 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t54 = 0.3141592653589793D1 * t43
      t57 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t60 = 0.3141592653589793D1 * lh
      t66 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t75 = t27 ** 2
      t76 = t30 ** 2
      t84 = t21 ** 2
      t89 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t92 = t3 * t57
      t93 = x1 ** 2
      t94 = x3 * t93
      t97 = log(0.4D1 * t94 * t18)
      t98 = t97 ** 2
      t102 = t97 * t3
      t104 = -0.1D1 + x3
      t105 = 0.1D1 / t104
      t106 = t18 * t105
      t109 = log(-0.4D1 * t94 * t106)
      t111 = t109 ** 2
      t115 = cos(t15)
      t116 = x3 * z
      t118 = Sqrt(-t116 * t104)
      t122 = 0.1D1 / (-z - x3 + 0.2D1 * t115 * t118)
      t128 = t3 * t47
      t136 = t89 * t122
      t137 = t3 * t89
      t142 = 0.1D1 / x3
      t144 = 0.1D1 / x1
      t147 = t93 * t17
      t148 = t147 * t14
      t150 = log(0.4D1 * t148)
      t155 = t150 ** 2
      t178 = x2 ** 2
      t179 = x3 * t178
      t180 = t179 * t93
      t183 = log(-0.4D1 * t180 * t106)
      t187 = -0.1D1 + x2
      t188 = t18 * t187
      t191 = log(-0.4D1 * t180 * t188)
      t193 = -t187
      t194 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, 0.10D
     #1, x4)
      t198 = log(0.4D1 * t179 * t148)
      t201 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, 0.10D
     #1, x4)
      t202 = t3 * t201
      t206 = t3 * t194
      t207 = -t136 - t137 + t206
      t213 = 0.1D1 / x2
      t214 = t144 * t213
      t217 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, 0.10D
     #1, x4)
      t218 = t3 * t217
      t219 = t178 * t93
      t222 = log(-0.4D1 * t219 * t188)
      t223 = t222 * t3
      t227 = log(0.4D1 * t219 * t18)
      t228 = t227 * t3
      t230 = t222 ** 2
      t234 = t227 ** 2
      t262 = x3 * t14
      t265 = log(0.4D1 * t262 * t17)
      t270 = log(-0.4D1 * t262 * t17 * t105)
      t274 = t270 ** 2
      t277 = t265 ** 2
      t313 = log(-0.4D1 * t179 * t188)
      t314 = t313 * t3
      t316 = t313 ** 2
      t322 = log(0.4D1 * t179 * t18)
      t323 = t322 * t3
      t325 = t322 ** 2
      t331 = log(-0.4D1 * t179 * t106)
      t333 = t331 ** 2
      t356 = t14 * t178
      t359 = log(0.4D1 * t356 * t17)
      t361 = t17 * t187
      t364 = log(-0.4D1 * t356 * t361)
      t369 = t364 ** 2
      t372 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, 0.10D
     #1, x4)
      t377 = t359 ** 2
      t404 = -t7 * 0.3141592653589793D1 * t8 / 0.32D2 - (-0.90D2 * t22 *
     # t24 + t7 * t34 - 0.15D2 * t37 * t23 - t40 * t44) * t47 / 0.2880D4
     # - (0.180D3 * t40 * t24 + 0.45D2 * t22 * t23 + t7 * t54) * t57 / 0
     #.2880D4 - (-0.180D3 * t7 * t60 - 0.90D2 * t40 * t23) * t66 / 0.288
     #0D4 - (0.30D2 * t37 * t24 + t22 * t44 / 0.2D1 - t40 * t23 * t33 + 
     #t7 * 0.3141592653589793D1 * (t75 + 0.60D2 * t76 + 0.57698731351660
     #51D3 * lh - 0.60D2 * t30 * t27) + 0.15D2 / 0.4D1 * t84 * t3 * t23)
     # * t89 / 0.2880D4 + (0.90D2 * t23 * (-t92 - t98 * t3 * t89 / 0.2D1
     # + t102 * t47 - (t57 - t109 * t47 + t111 * t89 / 0.2D1) * t122) - 
     #0.180D3 * t23 * lh * (t102 * t89 - t128 - (t47 - t109 * t89) * t12
     #2) + t23 * t43 * (-t136 - t137)) * t142 * t144 / 0.1440D4 - (t7 * 
     #t54 * (t47 - t150 * t89) + 0.90D2 * t7 * 0.3141592653589793D1 * (t
     #155 * t47 / 0.2D1 + t66 - t155 * t150 * t89 / 0.6D1 - t150 * t57) 
     #+ t7 * t34 * t89 - 0.180D3 * t7 * t60 * (t57 - t150 * t47 + t155 *
     # t89 / 0.2D1)) * t144 / 0.1440D4 + (0.90D2 * t23 * (-(t47 - t183 *
     # t89) * t122 - t128 - t191 * t3 * t194 + t198 * t3 * t89 + t202) -
     # 0.180D3 * t23 * lh * t207) * t142 * t214 / 0.720D3 + (0.90D2 * t2
     #3 * (t218 - t92 - t223 * t201 + t228 * t47 + t230 * t3 * t194 / 0.
     #2D1 - t234 * t3 * t89 / 0.2D1) - 0.180D3 * t23 * lh * (t202 + t228
     # * t89 - t223 * t194 - t128) + t23 * t43 * (-t137 + t206)) * t144 
     #* t213 / 0.720D3 + ((0.90D2 * t23 * t57 - 0.180D3 * t23 * lh * t47
     # + t23 * t43 * t89) * (t265 * t3 + t270 * t122) + 0.90D2 * t23 * t
     #89 * (t274 * t270 * t122 / 0.6D1 + t277 * t265 * t3 / 0.6D1) + (t2
     #3 * t43 * t47 + 0.90D2 * t23 * t66 + t23 * t33 * t89 - 0.180D3 * t
     #23 * lh * t57) * (-t3 - t122) + (0.90D2 * t23 * t47 - 0.180D3 * t2
     #3 * lh * t89) * (-t277 * t3 / 0.2D1 - t274 * t122 / 0.2D1)) * t142
     # / 0.2880D4 + (0.90D2 * t23 * (t218 - t314 * t201 + t316 * t3 * t1
     #94 / 0.2D1 - t92 + t323 * t47 - t325 * t3 * t89 / 0.2D1 - (t57 - t
     #331 * t47 + t333 * t89 / 0.2D1) * t122) - 0.180D3 * t23 * lh * (-(
     #t47 - t331 * t89) * t122 + t202 - t314 * t194 - t128 + t323 * t89)
     # + t23 * t43 * t207) * t142 * t213 / 0.1440D4 - (t7 * t54 * (t47 -
     # t359 * t89 - t201 + t364 * t194) + 0.90D2 * t7 * 0.31415926535897
     #93D1 * (-t369 * t201 / 0.2D1 - t372 + t369 * t364 * t194 / 0.6D1 +
     # t364 * t217 + t377 * t47 / 0.2D1 + t66 - t377 * t359 * t89 / 0.6D
     #1 - t359 * t57) + t7 * t34 * (t89 - t194) - 0.180D3 * t7 * t60 * (
     #t57 - t359 * t47 + t377 * t89 / 0.2D1 - t217 + t364 * t201 - t369 
     #* t194 / 0.2D1)) * t213 / 0.1440D4
      t405 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t404)
      t407 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t404)
      t409 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t404)
      t411 = x2 * x3
      t412 = 0.1D1 - x3 + t411
      t413 = 0.1D1 / t412
      t414 = t411 * t413
      t415 = t2 * t414
      t416 = t104 * t413
      t417 = t2 * t416
      t418 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, -t416
     #, x4)
      t419 = t179 * t147
      t421 = t412 ** 2
      t422 = 0.1D1 / t421
      t423 = t104 * t422
      t427 = log(0.4D1 * t419 * t14 * t187 * t423)
      t428 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, -t416
     #, x4)
      t431 = t187 * t104
      t433 = Sqrt(t116 * t431)
      t437 = 0.1D1 / (-z - x3 + t411 + 0.2D1 * t115 * t433)
      t449 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t193, -t416
     #, x4)
      t454 = log(0.4D1 * t179 * t14 * t361 * t423)
      t456 = t454 ** 2
      t476 = (0.90D2 * t23 * (t418 - t427 * t428) * t437 - 0.180D3 * t23
     # * lh * t428 * t437) * t142 * t214 / 0.720D3 + (0.90D2 * t23 * (t4
     #49 - t454 * t418 + t456 * t428 / 0.2D1) * t437 - 0.180D3 * t23 * l
     #h * (t418 - t454 * t428) * t437 + t23 * t43 * t428 * t437) * t142 
     #* t213 / 0.1440D4
      t477 = FJET(XB1, XB2, s, 0.0D0, t415, 0.0D0, -t417, 0.0D0, t476)
      t480 = t1 * x1
      t481 = x1 * z
      t482 = -z - x1 + t481
      t483 = 0.1D1 / t482
      t485 = t187 * s * t480 * t483
      t486 = -0.1D1 + x1
      t487 = t2 * t486
      t489 = x2 * s * t480
      t490 = t1 ** 2
      t491 = s * t490
      t494 = x1 * t486 * t483
      t495 = t491 * t187 * t494
      t496 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t193, 0.10D1, 
     #x4)
      t497 = 0.1D1 / t12
      t498 = t486 ** 2
      t499 = t497 * t498
      t501 = t499 * t483 * t187
      t504 = log(0.4D1 * t419 * t501)
      t505 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t193, 0.10D1, 
     #x4)
      t508 = x1 * x2
      t509 = t508 * z
      t511 = 0.1D1 / (-z - t508 + t509)
      t522 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, x1, t193, 0.10D1, 
     #x4)
      t523 = t219 * t17
      t526 = log(0.4D1 * t523 * t501)
      t528 = t526 ** 2
      t548 = (0.90D2 * t23 * (t496 - t504 * t505) * t511 - 0.180D3 * t23
     # * lh * t505 * t511) * t142 * t214 / 0.720D3 + (0.90D2 * t23 * (t5
     #22 - t526 * t496 + t528 * t505 / 0.2D1) * t511 - 0.180D3 * t23 * l
     #h * (t496 - t526 * t505) * t511 + t23 * t43 * t505 * t511) * t144 
     #* t213 / 0.720D3
      t549 = FJET(XB1, XB2, s, 0.0D0, t485, -t487, t489, -t495, t548)
      t552 = t2 * x1 * t483
      t553 = t491 * t494
      t554 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t556 = t94 * t17
      t558 = t499 * t483 * t105
      t561 = log(0.4D1 * t556 * t558)
      t562 = t561 * t482
      t563 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t565 = t561 ** 2
      t567 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t571 = x3 * x1
      t572 = t571 * z
      t574 = 0.2D1 * t94 * z
      t575 = x1 * t12
      t576 = t571 * t12
      t577 = t93 * t12
      t578 = t577 * x3
      t579 = z * t115
      t580 = x3 * t482
      t582 = Sqrt(t580 * t104)
      t586 = 0.1D1 / (-t481 - t572 + t574 + t575 + t576 - t94 - t116 - t
     #578 - t12 + 0.2D1 * t579 * t582)
      t588 = t3 * t554
      t589 = t499 * t483
      t592 = log(-0.4D1 * t556 * t589)
      t593 = t592 * t3
      t595 = t592 ** 2
      t602 = t482 * t563
      t606 = t3 * t563
      t615 = -t482 * t567 * t586 + t3 * t567
      t621 = (0.90D2 * t23 * (-(t482 * t554 - t562 * t563 + t565 * t482 
     #* t567 / 0.2D1) * t586 + t588 - t593 * t563 + t595 * t3 * t567 / 0
     #.2D1) - 0.180D3 * t23 * lh * (-(t602 - t562 * t567) * t586 + t606 
     #- t593 * t567) + t23 * t43 * t615) * t142 * t144 / 0.1440D4
      t624 = log(-0.4D1 * t147 * t589)
      t626 = -t563 + t624 * t567
      t629 = t624 ** 2
      t632 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t637 = -t629 * t563 / 0.2D1 - t632 + t629 * t624 * t567 / 0.6D1 + 
     #t624 * t554
      t642 = t7 * t34 * t567
      t646 = -t554 + t624 * t563 - t629 * t567 / 0.2D1
      t654 = t498 * t483
      t658 = log(-0.4D1 * t180 * t17 * t497 * t654)
      t663 = log(0.4D1 * t419 * t558)
      t677 = (0.90D2 * t23 * (t606 - t658 * t3 * t567 - (t602 - t663 * t
     #482 * t567) * t586) - 0.180D3 * t23 * lh * t615) * t142 * t214 / 0
     #.720D3
      t680 = log(-0.4D1 * t523 * t589)
      t681 = t680 * t3
      t683 = t680 ** 2
      t701 = (0.90D2 * t23 * (t588 - t681 * t563 + t683 * t3 * t567 / 0.
     #2D1) - 0.180D3 * t23 * lh * (t606 - t681 * t567) + t23 * t43 * t3 
     #* t567) * t144 * t213 / 0.720D3
      t702 = t621 - (t7 * t54 * t626 + 0.90D2 * t7 * 0.3141592653589793D
     #1 * t637 - t642 - 0.180D3 * t7 * t60 * t646) * t144 / 0.1440D4 + t
     #677 + t701
      t703 = FJET(XB1, XB2, s, 0.0D0, -t487, -t552, 0.0D0, t553, t702)
      t705 = FJET(XB1, XB2, s, 0.0D0, -t552, -t487, 0.0D0, t553, t702)
      t707 = FJET(XB1, XB2, s, 0.0D0, -t417, 0.0D0, t415, 0.0D0, t476)
      t709 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t404)
      t711 = FJET(XB1, XB2, s, t489, -t487, t485, 0.0D0, -t495, t548)
      t713 = FJET(XB1, XB2, s, t415, 0.0D0, -t417, 0.0D0, 0.0D0, t476)
      t715 = FJET(XB1, XB2, s, t485, 0.0D0, t489, -t487, -t495, t548)
      t720 = t104 * s * t1 * t486 * t413
      t721 = t2 * x1
      t723 = Sqrt(-t580 * t431)
      t724 = t115 * t723
      t730 = t721 * x2 * (-x3 + t411 - z + t116 - x1 + t571 + t481 - t57
     #2 + 0.2D1 * t724) * t483 * t413
      t731 = t487 * t414
      t732 = t481 * t179
      t733 = t724 * x2
      t738 = x1 * t178 * x3
      t742 = t721 * (-t732 - x2 + t411 + 0.2D1 * t733 + 0.1D1 - x3 + z *
     # t178 * x3 + t738) * t483 * t413
      t743 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t193, -t416, x
     #4)
      t751 = log(-0.4D1 * t179 * t147 * t497 * t654 * t431 * t422)
      t753 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t193, -t416, x
     #4)
      t762 = x2 * t93
      t766 = x2 * z
      t774 = -t738 - t411 * z + t571 * x2 - 0.2D1 * t579 * t723 - t94 * 
     #x2 - t508 * t12 - 0.2D1 * t762 * z + t762 * t12 + t732 - 0.2D1 * t
     #571 * t766 - t577 * t411 + 0.2D1 * t94 * t766 + t571 * x2 * t12
      t781 = -0.2D1 * x1 * t115 * t723 * x2 + t509 + t572 - t574 - t576 
     #+ t578 + t12 + t762 + t481 - t575 + t94 + t116 + 0.2D1 * t481 * t7
     #33
      t783 = 0.1D1 / (t774 + t781)
      t791 = -0.90D2 * t23 * (t482 * t743 - t751 * t482 * t753) * t783 +
     # 0.180D3 * t24 * t482 * t753 * t783
      t794 = t791 * t142 * t214 / 0.720D3
      t795 = FJET(XB1, XB2, s, t720, t730, -t731, -t742, -t495, t794)
      t798 = t142 * t144 * t213
      t801 = FJET(XB1, XB2, s, t730, t720, -t742, -t731, -t495, t794)
      t819 = t621 - (t7 * t54 * t626 + 0.90D2 * t7 * 0.3141592653589793D
     #1 * t637 - t642 - 0.180D3 * t7 * t60 * t646) * t144 / 0.1440D4 + t
     #677 + t701
      t820 = FJET(XB1, XB2, s, -t487, 0.0D0, 0.0D0, -t552, t553, t819)
      t822 = FJET(XB1, XB2, s, -t487, t489, 0.0D0, t485, -t495, t548)
      t824 = FJET(XB1, XB2, s, -t552, 0.0D0, 0.0D0, -t487, t553, t819)
      t826 = FJET(XB1, XB2, s, -t417, 0.0D0, t415, 0.0D0, 0.0D0, t476)
      t828 = FJET(XB1, XB2, s, -t742, -t731, t730, t720, -t495, t794)
      t832 = FJET(XB1, XB2, s, -t731, -t742, t720, t730, -t495, t794)
      rrgg2gght6s2e1 = t405 * t404 + t407 * t404 + t409 * t404 + t477 * 
     #t476 + t549 * t548 + t703 * t702 + t705 * t702 + t707 * t476 + t70
     #9 * t404 + t711 * t548 + t713 * t476 + t715 * t548 + t795 * t791 *
     # t798 / 0.720D3 + t801 * t791 * t798 / 0.720D3 + t820 * t819 + t82
     #2 * t548 + t824 * t819 + t826 * t476 + t828 * t791 * t798 / 0.720D
     #3 + t832 * t791 * t798 / 0.720D3

      end function



      doubleprecision function rrgg2gght6s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
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
      t42 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t53 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t59 = lh ** 2
      t61 = 0.3141592653589793D1 ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t70 = 0.1D1 / x3
      t73 = t10 * t14
      t75 = log(0.4D1 * t73)
      t76 = t75 * t19
      t77 = t6 * lh
      t80 = t75 ** 2
      t81 = t80 * t19
      t84 = t19 * t5
      t85 = 0.3141592653589793D1 * t63
      t90 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t112 = 0.3141592653589793D1 * lh
      t120 = x2 ** 2
      t121 = x3 * t120
      t122 = t73 * t22
      t125 = log(-0.4D1 * t121 * t122)
      t129 = 0.1D1 - x2
      t130 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t129, 0.10D
     #1, x4)
      t131 = t19 * t130
      t132 = -t129
      t133 = t73 * t132
      t136 = log(-0.4D1 * t121 * t133)
      t138 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t129, 0.10D
     #1, x4)
      t140 = t19 * t42
      t143 = log(0.4D1 * t121 * t73)
      t149 = t7 * t35
      t150 = t19 * t7
      t151 = t19 * t138
      t152 = -t149 - t150 + t151
      t158 = 0.1D1 / x2
      t161 = t10 * t120
      t164 = log(0.4D1 * t161 * t14)
      t166 = t164 ** 2
      t169 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t129, 0.10D
     #1, x4)
      t170 = t14 * t132
      t173 = log(-0.4D1 * t161 * t170)
      t175 = t173 ** 2
      t194 = x1 ** 2
      t195 = x3 * t194
      t198 = log(0.4D1 * t195 * t73)
      t203 = log(-0.4D1 * t195 * t122)
      t216 = 0.1D1 / x1
      t221 = t70 * t216 * t158
      t224 = t120 * t194
      t227 = log(0.4D1 * t224 * t73)
      t232 = log(-0.4D1 * t224 * t133)
      t246 = t194 * t14
      t249 = log(0.4D1 * t246 * t10)
      t251 = t249 ** 2
      t268 = (0.90D2 * t6 * t7 * (-t18 * t19 / 0.2D1 - t27 * t35 / 0.2D1
     #) + (0.90D2 * t6 * t42 - 0.180D3 * t6 * lh * t7) * (t17 * t19 + t2
     #6 * t35) + (0.90D2 * t6 * t53 - 0.180D3 * t6 * lh * t42 + t6 * t63
     # * t7) * (-t19 - t35)) * t70 / 0.2880D4 - (0.180D3 * t76 * t77 + 0
     #.45D2 * t81 * t6 + t84 * t85) * t42 / 0.2880D4 - t84 * 0.314159265
     #3589793D1 * t90 / 0.32D2 - (-0.90D2 * t81 * t77 + t84 * 0.31415926
     #53589793D1 * (0.60D2 * lh * t61 - 0.2884936567583026D3 - 0.120D3 *
     # t59 * lh) - 0.15D2 * t80 * t75 * t19 * t6 - t76 * t6 * t63) * t7 
     #/ 0.2880D4 - (-0.180D3 * t84 * t112 - 0.90D2 * t76 * t6) * t53 / 0
     #.2880D4 + (0.90D2 * t6 * (-(t42 - t125 * t7) * t35 + t131 - t136 *
     # t19 * t138 - t140 + t143 * t19 * t7) - 0.180D3 * t6 * lh * t152) 
     #* t70 * t158 / 0.1440D4 - (0.90D2 * t84 * 0.3141592653589793D1 * (
     #t53 - t164 * t42 + t166 * t7 / 0.2D1 - t169 + t173 * t130 - t175 *
     # t138 / 0.2D1) - 0.180D3 * t84 * t112 * (t42 - t164 * t7 - t130 + 
     #t173 * t138) + t84 * t85 * (t7 - t138)) * t158 / 0.1440D4 + (0.90D
     #2 * t6 * (t198 * t19 * t7 - t140 - (t42 - t203 * t7) * t35) - 0.18
     #0D3 * t6 * lh * (-t149 - t150)) * t70 * t216 / 0.1440D4 + t6 * t15
     #2 * t221 / 0.8D1 + (0.90D2 * t6 * (t131 + t227 * t19 * t7 - t232 *
     # t19 * t138 - t140) - 0.180D3 * t6 * lh * (-t150 + t151)) * t216 *
     # t158 / 0.720D3 - (0.90D2 * t84 * 0.3141592653589793D1 * (t53 - t2
     #49 * t42 + t251 * t7 / 0.2D1) - 0.180D3 * t84 * t112 * (t42 - t249
     # * t7) + t84 * t85 * t7) * t216 / 0.1440D4
      t269 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t268)
      t271 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t268)
      t273 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t268)
      t275 = x2 * x3
      t276 = 0.1D1 - x3 + t275
      t277 = 0.1D1 / t276
      t278 = t275 * t277
      t279 = t2 * t278
      t280 = t21 * t277
      t281 = t2 * t280
      t282 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t129, -t280
     #, x4)
      t284 = t276 ** 2
      t290 = log(0.4D1 * t121 * t10 * t170 * t21 / t284)
      t291 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t129, -t280
     #, x4)
      t294 = t132 * t21
      t296 = Sqrt(t29 * t294)
      t300 = 0.1D1 / (-z - x3 + t275 + 0.2D1 * t28 * t296)
      t314 = t216 * t158
      t318 = (0.90D2 * t6 * (t282 - t290 * t291) * t300 - 0.180D3 * t6 *
     # lh * t291 * t300) * t70 * t158 / 0.1440D4 + t6 * t291 * t300 * t7
     #0 * t314 / 0.8D1
      t319 = FJET(XB1, XB2, s, 0.0D0, t279, 0.0D0, -t281, 0.0D0, t318)
      t322 = t1 * x1
      t323 = x1 * z
      t324 = -z - x1 + t323
      t325 = 0.1D1 / t324
      t327 = t132 * s * t322 * t325
      t328 = -0.1D1 + x1
      t329 = t2 * t328
      t331 = x2 * s * t322
      t332 = t1 ** 2
      t333 = s * t332
      t336 = x1 * t328 * t325
      t337 = t333 * t132 * t336
      t338 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t129, 0.10D1, 
     #x4)
      t340 = x1 * x2
      t341 = t340 * z
      t343 = 0.1D1 / (-z - t340 + t341)
      t348 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, x1, t129, 0.10D1, 
     #x4)
      t349 = t224 * t14
      t350 = 0.1D1 / t8
      t352 = t328 ** 2
      t357 = log(0.4D1 * t349 * t350 * t325 * t352 * t132)
      t371 = t6 * t338 * t343 * t70 * t314 / 0.8D1 + (0.90D2 * t6 * (t34
     #8 - t357 * t338) * t343 - 0.180D3 * t6 * lh * t338 * t343) * t216 
     #* t158 / 0.720D3
      t372 = FJET(XB1, XB2, s, 0.0D0, t327, -t329, t331, -t337, t371)
      t375 = t2 * x1 * t325
      t376 = t333 * t336
      t377 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t379 = t195 * t14
      t380 = t350 * t352
      t385 = log(0.4D1 * t379 * t380 * t325 * t22)
      t387 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t390 = x3 * x1
      t391 = t390 * z
      t393 = 0.2D1 * t195 * z
      t394 = x1 * t8
      t395 = t390 * t8
      t396 = t194 * t8
      t397 = t396 * x3
      t398 = z * t28
      t399 = x3 * t324
      t401 = Sqrt(t399 * t21)
      t405 = 0.1D1 / (-t323 - t391 + t393 + t394 + t395 - t195 - t29 - t
     #397 - t8 + 0.2D1 * t398 * t401)
      t407 = t19 * t377
      t408 = t380 * t325
      t411 = log(-0.4D1 * t379 * t408)
      t420 = -t324 * t387 * t405 + t19 * t387
      t427 = (0.90D2 * t6 * (-(t324 * t377 - t385 * t324 * t387) * t405 
     #+ t407 - t411 * t19 * t387) - 0.180D3 * t6 * lh * t420) * t70 * t2
     #16 / 0.1440D4
      t430 = t6 * t420 * t221 / 0.8D1
      t433 = log(-0.4D1 * t349 * t408)
      t446 = (0.90D2 * t6 * (t407 - t433 * t19 * t387) - 0.180D3 * t6 * 
     #lh * t19 * t387) * t216 * t158 / 0.720D3
      t447 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t450 = log(-0.4D1 * t246 * t408)
      t452 = t450 ** 2
      t455 = -t447 + t450 * t377 - t452 * t387 / 0.2D1
      t460 = -t377 + t450 * t387
      t466 = t6 * t63 * t19 * t387
      t470 = t427 + t430 + t446 - (0.90D2 * t84 * 0.3141592653589793D1 *
     # t455 - 0.180D3 * t84 * t112 * t460 - t466) * t216 / 0.1440D4
      t471 = FJET(XB1, XB2, s, 0.0D0, -t329, -t375, 0.0D0, t376, t470)
      t473 = FJET(XB1, XB2, s, 0.0D0, -t375, -t329, 0.0D0, t376, t470)
      t475 = FJET(XB1, XB2, s, 0.0D0, -t281, 0.0D0, t279, 0.0D0, t318)
      t477 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t268)
      t479 = FJET(XB1, XB2, s, t331, -t329, t327, 0.0D0, -t337, t371)
      t481 = FJET(XB1, XB2, s, t279, 0.0D0, -t281, 0.0D0, 0.0D0, t318)
      t483 = FJET(XB1, XB2, s, t327, 0.0D0, t331, -t329, -t337, t371)
      t488 = t21 * s * t1 * t328 * t277
      t489 = t2 * x1
      t491 = Sqrt(-t399 * t294)
      t492 = t28 * t491
      t498 = t489 * x2 * (-x3 + t275 - z + t29 - x1 + t390 + t323 - t391
     # + 0.2D1 * t492) * t325 * t277
      t499 = t329 * t278
      t500 = t323 * t121
      t501 = t492 * x2
      t506 = x1 * t120 * x3
      t510 = t489 * (-t500 - x2 + t275 + 0.2D1 * t501 + 0.1D1 - x3 + z *
     # t120 * x3 + t506) * t325 * t277
      t511 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t129, -t280, x
     #4)
      t514 = x2 * z
      t526 = x2 * t194
      t529 = t500 - 0.2D1 * t390 * t514 - t396 * t275 + 0.2D1 * t195 * t
     #514 + t390 * x2 * t8 - 0.2D1 * x1 * t28 * t491 * x2 + t323 - t394 
     #+ t195 + t29 + t526 + 0.2D1 * t323 * t501 - t506
      t539 = -t275 * z + t390 * x2 - 0.2D1 * t398 * t491 - t195 * x2 - t
     #340 * t8 - 0.2D1 * t526 * z + t526 * t8 + t8 + t341 + t391 - t393 
     #- t395 + t397
      t541 = 0.1D1 / (t529 + t539)
      t545 = t6 * t324 * t511 * t541 * t70 * t314 / 0.8D1
      t546 = FJET(XB1, XB2, s, t488, t498, -t499, -t510, -t337, -t545)
      t548 = 0.3141592653589793D1 * t324
      t551 = t511 * t541 * t221
      t554 = FJET(XB1, XB2, s, t498, t488, -t510, -t499, -t337, -t545)
      t570 = t427 + t430 + t446 - (0.90D2 * t84 * 0.3141592653589793D1 *
     # t455 - 0.180D3 * t84 * t112 * t460 - t466) * t216 / 0.1440D4
      t571 = FJET(XB1, XB2, s, -t329, 0.0D0, 0.0D0, -t375, t376, t570)
      t573 = FJET(XB1, XB2, s, -t329, t331, 0.0D0, t327, -t337, t371)
      t575 = FJET(XB1, XB2, s, -t375, 0.0D0, 0.0D0, -t329, t376, t570)
      t577 = FJET(XB1, XB2, s, -t281, 0.0D0, t279, 0.0D0, 0.0D0, t318)
      t579 = FJET(XB1, XB2, s, -t510, -t499, t498, t488, -t337, -t545)
      t584 = FJET(XB1, XB2, s, -t499, -t510, t488, t498, -t337, -t545)
      rrgg2gght6s2e0 = t269 * t268 + t271 * t268 + t273 * t268 + t319 * 
     #t318 + t372 * t371 + t471 * t470 + t473 * t470 + t475 * t318 + t47
     #7 * t268 + t479 * t371 + t481 * t318 + t483 * t371 - t546 * t5 * t
     #548 * t551 / 0.8D1 - t554 * t5 * t548 * t551 / 0.8D1 + t571 * t570
     # + t573 * t371 + t575 * t570 + t577 * t318 - t579 * t5 * t548 * t5
     #51 / 0.8D1 - t584 * t5 * t548 * t551 / 0.8D1

      end function



      doubleprecision function rrgg2gght6s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
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
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t39 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t49 = 0.1D1 / x3
      t52 = t18 * t5
      t53 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t57 = 0.3141592653589793D1 * lh
      t62 = log(0.4D1 * t10 * t14)
      t63 = t62 * t18
      t72 = t62 ** 2
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t86 = t7 * t33
      t87 = t18 * t7
      t88 = 0.1D1 - x2
      t89 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t88, 0.10D1,
     # x4)
      t90 = t18 * t89
      t93 = 0.1D1 / x2
      t97 = x2 ** 2
      t98 = t10 * t97
      t101 = log(0.4D1 * t98 * t14)
      t103 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t88, 0.10D1
     #, x4)
      t104 = -t88
      t108 = log(-0.4D1 * t98 * t14 * t104)
      t122 = 0.1D1 / x1
      t127 = x1 ** 2
      t128 = t127 * t14
      t131 = log(0.4D1 * t128 * t10)
      t148 = (0.90D2 * t6 * t7 * (t17 * t18 + t25 * t33) + (0.90D2 * t6 
     #* t39 - 0.180D3 * t6 * lh * t7) * (-t18 - t33)) * t49 / 0.2880D4 -
     # t52 * 0.3141592653589793D1 * t53 / 0.32D2 - (-0.180D3 * t52 * t57
     # - 0.90D2 * t63 * t6) * t39 / 0.2880D4 - (0.180D3 * t63 * t6 * lh 
     #+ 0.45D2 * t72 * t18 * t6 + t52 * 0.3141592653589793D1 * (0.180D3 
     #* t76 - 0.30D2 * t78)) * t7 / 0.2880D4 + t6 * (-t86 - t87 + t90) *
     # t49 * t93 / 0.16D2 - (0.90D2 * t52 * 0.3141592653589793D1 * (t39 
     #- t101 * t7 - t103 + t108 * t89) - 0.180D3 * t52 * t57 * (t7 - t89
     #)) * t93 / 0.1440D4 + t6 * (-t87 + t90) * t122 * t93 / 0.8D1 - (0.
     #90D2 * t52 * 0.3141592653589793D1 * (t39 - t131 * t7) - 0.180D3 * 
     #t52 * t57 * t7) * t122 / 0.1440D4 + t6 * (-t86 - t87) * t49 * t122
     # / 0.16D2
      t149 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t148)
      t151 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t148)
      t153 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t148)
      t155 = x2 * x3
      t157 = 0.1D1 / (0.1D1 - x3 + t155)
      t159 = t2 * t155 * t157
      t160 = t20 * t157
      t161 = t2 * t160
      t162 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t88, -t160,
     # x4)
      t166 = Sqrt(t27 * t104 * t20)
      t170 = 0.1D1 / (-z - x3 + t155 + 0.2D1 * t26 * t166)
      t174 = t6 * t162 * t170 * t49 * t93 / 0.16D2
      t175 = FJET(XB1, XB2, s, 0.0D0, t159, 0.0D0, -t161, 0.0D0, t174)
      t180 = t162 * t170 * t49 * t93
      t184 = t1 * x1
      t185 = x1 * z
      t186 = -z - x1 + t185
      t187 = 0.1D1 / t186
      t189 = t104 * s * t184 * t187
      t190 = -0.1D1 + x1
      t191 = t2 * t190
      t193 = x2 * s * t184
      t194 = t1 ** 2
      t195 = s * t194
      t198 = x1 * t190 * t187
      t199 = t195 * t104 * t198
      t200 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, t88, 0.10D1, x
     #4)
      t202 = x1 * x2
      t205 = 0.1D1 / (-z - t202 + t202 * z)
      t209 = t6 * t200 * t205 * t122 * t93 / 0.8D1
      t210 = FJET(XB1, XB2, s, 0.0D0, t189, -t191, t193, -t199, t209)
      t215 = t200 * t205 * t122 * t93
      t219 = t2 * x1 * t187
      t220 = t195 * t198
      t222 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t226 = t6 * t18 * t222 * t122 * t93 / 0.8D1
      t227 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t230 = t190 ** 2
      t234 = log(-0.4D1 * t128 / t8 * t187 * t230)
      t236 = -t227 + t234 * t222
      t243 = 0.180D3 * t6 * lh * t18 * t222
      t248 = x3 * x1
      t250 = x3 * t127
      t260 = Sqrt(x3 * t186 * t20)
      t271 = t6 * (-t186 * t222 / (-t185 - t248 * z + 0.2D1 * t250 * z +
     # x1 * t8 + t248 * t8 - t250 - t27 - t127 * t8 * x3 - t8 + 0.2D1 * 
     #z * t26 * t260) + t18 * t222) * t49 * t122 / 0.16D2
      t272 = t226 - (0.90D2 * t52 * 0.3141592653589793D1 * t236 + t243) 
     #* t122 / 0.1440D4 + t271
      t273 = FJET(XB1, XB2, s, 0.0D0, -t191, -t219, 0.0D0, t220, t272)
      t275 = FJET(XB1, XB2, s, 0.0D0, -t219, -t191, 0.0D0, t220, t272)
      t277 = FJET(XB1, XB2, s, 0.0D0, -t161, 0.0D0, t159, 0.0D0, t174)
      t282 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t148)
      t284 = FJET(XB1, XB2, s, t193, -t191, t189, 0.0D0, -t199, t209)
      t289 = FJET(XB1, XB2, s, t159, 0.0D0, -t161, 0.0D0, 0.0D0, t174)
      t294 = FJET(XB1, XB2, s, t189, 0.0D0, t193, -t191, -t199, t209)
      t306 = t226 - (0.90D2 * t52 * 0.3141592653589793D1 * t236 + t243) 
     #* t122 / 0.1440D4 + t271
      t307 = FJET(XB1, XB2, s, -t191, 0.0D0, 0.0D0, -t219, t220, t306)
      t309 = FJET(XB1, XB2, s, -t191, t193, 0.0D0, t189, -t199, t209)
      t314 = FJET(XB1, XB2, s, -t219, 0.0D0, 0.0D0, -t191, t220, t306)
      t316 = FJET(XB1, XB2, s, -t161, 0.0D0, t159, 0.0D0, 0.0D0, t174)
      rrgg2gght6s2em1 = t149 * t148 + t151 * t148 + t153 * t148 + t175 *
     # t5 * 0.3141592653589793D1 * t180 / 0.16D2 + t210 * t5 * 0.3141592
     #653589793D1 * t215 / 0.8D1 + t273 * t272 + t275 * t272 + t277 * t5
     # * 0.3141592653589793D1 * t180 / 0.16D2 + t282 * t148 + t284 * t5 
     #* 0.3141592653589793D1 * t215 / 0.8D1 + t289 * t5 * 0.314159265358
     #9793D1 * t180 / 0.16D2 + t294 * t5 * 0.3141592653589793D1 * t215 /
     # 0.8D1 + t307 * t306 + t309 * t5 * 0.3141592653589793D1 * t215 / 0
     #.8D1 + t314 * t306 + t316 * t5 * 0.3141592653589793D1 * t180 / 0.1
     #6D2

      end function



      doubleprecision function rrgg2gght6s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * 0.3141592653589793D1
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t8 = 0.1D1 / z
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t14 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t25 = t8 * t5
      t27 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.1D1 - x2, 
     #0.10D1, x4)
      t35 = 0.1D1 / x1
      t39 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t46 = z ** 2
      t49 = Sin(t9)
      t50 = t49 ** 2
      t53 = log(0.4D1 / t46 / z * t50)
      t60 = t6 * t7 * (-t8 - 0.1D1 / (-z - x3 + 0.2D1 * t10 * t14)) / x3
     # / 0.32D2 - t25 * 0.3141592653589793D1 * (t7 - t27) / x2 / 0.16D2 
     #- t25 * 0.3141592653589793D1 * t7 * t35 / 0.16D2 - t25 * 0.3141592
     #653589793D1 * t39 / 0.32D2 - (-0.180D3 * t25 * 0.3141592653589793D
     #1 * lh - 0.90D2 * t53 * t8 * t6) * t7 / 0.2880D4
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t60)
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t60)
      t65 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t60)
      t67 = -0.1D1 + x1
      t68 = t2 * t67
      t71 = 0.1D1 / (-z - x1 + x1 * z)
      t73 = t2 * x1 * t71
      t74 = t1 ** 2
      t78 = s * t74 * x1 * t67 * t71
      t79 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1,
     # x4)
      t81 = t8 * t79 * t35
      t83 = t81 * t6 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, -t68, -t73, 0.0D0, t78, t83)
      t89 = FJET(XB1, XB2, s, 0.0D0, -t73, -t68, 0.0D0, t78, t83)
      t94 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t60)
      t96 = FJET(XB1, XB2, s, -t68, 0.0D0, 0.0D0, -t73, t78, t83)
      t101 = FJET(XB1, XB2, s, -t73, 0.0D0, 0.0D0, -t68, t78, t83)
      rrgg2gght6s2em2 = t61 * t60 + t63 * t60 + t65 * t60 + t84 * t5 * 0
     #.3141592653589793D1 * t81 / 0.16D2 + t89 * t5 * 0.3141592653589793
     #D1 * t81 / 0.16D2 + t94 * t60 + t96 * t5 * 0.3141592653589793D1 * 
     #t81 / 0.16D2 + t101 * t5 * 0.3141592653589793D1 * t81 / 0.16D2

      end function



      doubleprecision function rrgg2gght6s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t8 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t11 = t3 * t6 * 0.3141592653589793D1 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t6 * 0.3141592653589793D1 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2gght6s2em3 = -t12 * t3 * t15 / 0.32D2 - t17 * t3 * t15 / 0.32
     #D2 - t20 * t3 * t15 / 0.32D2 - t23 * t3 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2gght6s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7
      rrgg2gght6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh61J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t11 = x2 * x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + t11 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t32 = t1 ** 2
      t33 = t32 ** 2
      t34 = t33 * t24
      t37 = z + x1 * t8 * t1
      t39 = t31 * t34 * t37
      t40 = t4 ** 2
      t41 = t40 ** 2
      t42 = 0.1D1 / t41
      t44 = x2 * t7
      t45 = t14 * t4 + t44 - t20
      t48 = t24 * x3
      t50 = s - t2 * t6 * t45 - t2 * t48
      t51 = t42 * t50
      t52 = x1 ** 2
      t53 = t52 * x1
      t54 = t53 * t45
      t55 = t21 ** 2
      t60 = t50 * t27
      t61 = t32 * t1
      t63 = t60 * t30 * t61
      t64 = t24 ** 2
      t65 = t64 * t7
      t66 = x1 * x2
      t67 = t66 * t5
      t71 = t29 * t28
      t72 = t50 * t71
      t73 = t33 * t1
      t75 = t72 * t73 * t24
      t77 = 0.1D1 / t41 / t4
      t78 = t37 * t77
      t79 = t52 ** 2
      t80 = t45 ** 2
      t81 = t79 * t80
      t86 = 0.1D1 / t40
      t87 = t53 * t86
      t88 = t45 * t21
      t89 = t87 * t88
      t92 = t61 * t64
      t93 = t92 * t37
      t94 = t31 * t93
      t95 = t86 * t50
      t96 = x1 * t45
      t101 = t30 * t73
      t103 = t60 * t101 * t79
      t104 = t42 * t45
      t110 = t60 * t101
      t112 = 0.1D1 / t40 / t4
      t113 = t53 * t112
      t114 = x2 ** 2
      t115 = t45 * t114
      t116 = t115 * t64
      t120 = t71 * z
      t121 = t33 * t32
      t123 = t64 ** 2
      t126 = t7 * t37
      t132 = t50 * t33
      t133 = t120 * t132
      t135 = t45 * x2 * t24
      t136 = t87 * t135
      t139 = t121 * t123
      t140 = t139 * t37
      t142 = t112 * t50
      t143 = t114 * t52
      t148 = t53 * t114
      t153 = t71 * t73
      t155 = t153 * t79 * t27
      t156 = t112 * t80
      t158 = t21 * t24 * x3
      t162 = t27 * t71
      t164 = t162 * t73 * t53
      t165 = t86 * t55
      t170 = t64 * t24
      t171 = t121 * t170
      t172 = t171 * t37
      t173 = t72 * t172
      t174 = t42 * t53
      t180 = t33 ** 2
      t183 = t79 * t45
      t184 = t114 * x2
      t189 = t30 * z
      t190 = t189 * t60
      t191 = t33 * t79
      t196 = z ** 2
      t198 = t30 * t196 * t60
      t205 = -0.74D2 * t39 * t51 * t54 * t55 - 0.120D3 * t63 * t65 * t67
     # - 0.14D2 * t75 * t78 * t81 * t55 - 0.16D2 * t63 * t89 - 0.96D2 * 
     #t94 * t95 * t96 * x3 - 0.6D1 * t103 * t104 * t55 * x2 * t24 + 0.10
     #4D3 * t110 * t113 * t116 - 0.32D2 * t120 * t27 * t121 * t123 * t12
     #6 * t112 * t114 * t52 + 0.64D2 * t133 * t136 + 0.32D2 * t31 * t140
     # * t142 * t143 * x3 + 0.64D2 * t110 * t148 * t64 * t86 + 0.48D2 * 
     #t155 * t156 * t158 - 0.120D3 * t164 * t65 * t165 * x3 + 0.16D2 * t
     #173 * t174 * t80 * x3 * x2 - 0.2D1 * t72 * t180 * t123 * t78 * t18
     #3 * t184 + 0.16D2 * t190 * t191 * t156 * t21 - 0.64D2 * t198 * t32
     # * x2 * x1 * t24 * t5
      t206 = t123 * t37
      t208 = t7 ** 2
      t209 = t208 * t7
      t215 = t30 * t121
      t218 = t7 * t114
      t219 = t52 * t86
      t225 = t60 * t101 * t123
      t226 = x3 ** 2
      t227 = t7 * t226
      t231 = t33 * t64
      t232 = t231 * t37
      t233 = t31 * t232
      t234 = t52 * t45
      t240 = t162 * t121 * t52
      t241 = t5 * t45
      t242 = t226 ** 2
      t248 = t189 * t60 * t33
      t249 = t87 * t45
      t253 = t123 * t24
      t257 = t86 * t7
      t258 = t226 * x3
      t264 = t71 * t33
      t266 = t264 * t53 * t27
      t267 = t86 * t80
      t271 = t72 * t140
      t278 = t33 * t61
      t280 = t162 * t278 * t53
      t281 = t123 * t208
      t282 = t114 * t86
      t286 = t123 * t209
      t293 = t123 * t7
      t298 = t31 * t132
      t299 = t52 * t5
      t301 = t21 * t64 * t226
      t305 = t86 * t45
      t309 = t305 * t158
      t312 = t189 * t50
      t313 = t73 * t170
      t314 = t313 * t37
      t316 = t112 * t27
      t318 = t52 * t21
      t323 = t50 * t61
      t324 = t120 * t323
      t327 = t299 * t45 * t24 * x3
      t330 = t53 * z
      t334 = -0.6D1 * t153 * t206 * t95 * t209 * x1 * t21 - 0.14D2 * t60
     # * t215 * t123 * t218 * t219 * x3 - 0.6D1 * t225 * t227 * t67 + 0.
     #133D3 * t233 * t142 * t234 * x2 + 0.16D2 * t240 * t241 * t123 * t2
     #42 - 0.24D2 * t248 * t249 * t158 - 0.32D2 * t72 * t121 * t253 * t3
     #7 * t257 * t258 * x1 * t21 - 0.12D2 * t266 * t267 * t48 + 0.48D2 *
     # t271 * t112 * t7 * t226 * t52 * t55 - 0.6D1 * t280 * t281 * t282 
     #- 0.6D1 * t240 * t286 * x2 * t5 - 0.2D1 * t162 * t180 * t79 * t293
     # * t184 * t112 - 0.64D2 * t298 * t299 * t301 + 0.104D3 * t164 * t3
     #05 * t301 + 0.48D2 * t266 * t309 + 0.16D2 * t312 * t314 * t316 * x
     #2 * t318 * t7 + 0.64D2 * t324 * t327 - 0.32D2 * t63 * t330 * t165
      t336 = t64 * t208
      t340 = t278 * t170
      t346 = t64 * t37
      t349 = t55 * t21
      t354 = t61 * t24
      t356 = t31 * t354 * t37
      t361 = t1 * t24
      t363 = t37 * t5
      t364 = t50 * z
      t369 = t71 * t196 * z
      t370 = t27 * t32
      t372 = t65 * t363
      t378 = t50 * t30
      t380 = t316 * z
      t382 = t7 * t52 * t55
      t386 = t33 * t123
      t388 = t378 * t386 * t37
      t389 = t5 * t27
      t395 = t170 * t37
      t403 = t153 * t206 * t86
      t404 = t50 * t208
      t405 = x1 * t21
      t406 = t405 * x3
      t410 = t80 ** 2
      t415 = t32 * t64
      t416 = t31 * t415
      t417 = t5 * t50
      t418 = t126 * t417
      t421 = t162 * t73
      t422 = x1 * t123
      t423 = t208 ** 2
      t427 = t71 * t61
      t428 = t427 * t53
      t429 = t27 * t86
      t433 = t60 * t30
      t440 = t264 * t79
      t441 = t80 * t45
      t445 = -0.24D2 * t164 * t336 * t165 - 0.6D1 * t72 * t340 * t78 * t
     #81 * t114 - 0.42D2 * t153 * t346 * t51 * t7 * t53 * t349 - 0.133D3
     # * t356 * t142 * t234 * t21 - 0.32D2 * t31 * t361 * t363 * t364 - 
     #0.128D3 * t369 * t370 * t372 - 0.32D2 * t120 * t370 * t372 + 0.8D1
     # * t378 * t232 * t380 * t382 + 0.32D2 * t388 * t389 * z * t7 * t22
     #6 - 0.14D2 * t153 * t395 * t142 * t208 * t52 * t55 + 0.48D2 * t403
     # * t404 * t406 - 0.2D1 * t75 * t78 * t79 * t410 - 0.38D2 * t416 * 
     #t418 - 0.2D1 * t421 * t422 * t423 - 0.6D1 * t428 * t429 * t80 + 0.
     #2D1 * t433 * t386 * t423 + 0.24D2 * t433 * t415 * t208 - 0.6D1 * t
     #440 * t316 * t441
      t446 = t61 * t170
      t453 = t71 * t32
      t461 = t79 * x1
      t462 = t153 * t461
      t463 = t27 * t42
      t468 = t7 * t50
      t478 = t73 * t64
      t479 = t478 * t37
      t480 = t31 * t479
      t481 = t53 * t80
      t486 = t45 * t55
      t491 = t60 * t30 * t32
      t495 = t60 * t215
      t496 = t143 * t86
      t500 = t31 * t313
      t501 = t52 * x2
      t502 = t501 * t5
      t506 = t71 * t196
      t509 = t363 * t50 * t226
      t516 = t112 * t45
      t518 = t21 * x2 * t24
      t519 = t516 * t518
      t524 = t21 * x3 * x2
      t539 = 0.8D1 * t433 * t446 * t209 + 0.32D2 * t433 * t361 * t7 - 0.
     #2D1 * t453 * t52 * t389 * t45 - 0.32D2 * t433 * t3 * z - 0.2D1 * t
     #462 * t463 * t410 - 0.74D2 * t31 * t171 * t468 * t148 * t86 - 0.74
     #D2 * t31 * t172 * t51 * t54 * t114 + 0.67D2 * t480 * t51 * t481 * 
     #x2 + 0.8D1 * t63 * t113 * t486 - 0.24D2 * t491 * t219 * t88 + 0.14
     #D2 * t495 * t281 * t496 + 0.67D2 * t500 * t404 * t502 - 0.32D2 * t
     #506 * t446 * t509 + 0.384D3 * t63 * t87 * t88 * z + 0.80D2 * t103 
     #* t519 + 0.48D2 * t173 * t174 * t45 * t524 - 0.6D1 * t427 * t346 *
     # t95 * t7 * x1 * t21 + 0.8D1 * t378 * t140 * t380 * t218 * t52
      t545 = t55 ** 2
      t553 = t7 * x3
      t557 = t32 * t52
      t558 = t5 * t21
      t565 = t557 * t241
      t571 = t45 * t349
      t576 = t50 * x1
      t580 = t7 * t258
      t587 = t208 * t226
      t592 = t170 * t209
      t596 = t80 * t55
      t607 = t61 * t53
      t608 = t112 * t441
      t612 = t80 * t21
      t616 = 0.16D2 * t162 * t121 * t79 * t52 * t77 * t45 * t545 + 0.8D1
     # * t433 * t446 * t227 - 0.24D2 * t433 * t415 * t553 + 0.32D2 * t43
     #3 * t557 * t558 + 0.32D2 * t433 * t3 * t241 - 0.38D2 * t433 * t565
     # + 0.8D1 * t428 * t429 * t88 + 0.32D2 * t462 * t463 * t571 + 0.48D
     #2 * t31 * t32 * t25 * t576 - 0.2D1 * t433 * t386 * t580 - 0.24D2 *
     # t440 * t316 * t486 + 0.6D1 * t433 * t386 * t587 + 0.7D1 * t31 * t
     #33 * t592 * t576 - 0.24D2 * t462 * t463 * t596 + 0.24D2 * t433 * t
     #557 * t267 - 0.205D3 * t31 * t61 * t336 * t576 + 0.8D1 * t433 * t6
     #07 * t608 + 0.16D2 * t440 * t316 * t612
      t621 = t208 * x3
      t625 = t441 * t21
      t630 = t363 * t404
      t633 = t209 * x3
      t653 = t363 * t50 * t209
      t656 = t191 * t608
      t660 = t64 * t226
      t661 = t61 * x1 * t660
      t664 = t506 * t27
      t670 = t607 * t165
      t674 = t170 * t258
      t684 = 0.2D1 * t433 * t191 * t42 * t410 - 0.16D2 * t433 * t446 * t
     #621 + 0.8D1 * t462 * t463 * t625 - 0.6D1 * t427 * t170 * t630 - 0.
     #6D1 * t421 * t422 * t633 - 0.6D1 * t433 * t386 * t633 - 0.2D1 * t4
     #53 * t64 * t418 - 0.2D1 * t153 * t253 * t363 * t50 * t423 + 0.176D
     #3 * t433 * t607 * t267 - 0.6D1 * t264 * t123 * t653 - 0.260D3 * t4
     #33 * t656 - 0.32D2 * t433 * t661 - 0.32D2 * t664 * t661 - 0.32D2 *
     # t120 * t50 * t565 - 0.32D2 * t433 * t670 + 0.32D2 * t433 * t33 * 
     #x1 * t674 - 0.42D2 * t421 * t422 * t580 - 0.128D3 * t369 * t50 * t
     #565
      t691 = t50 * t73
      t696 = t31 * t92
      t697 = t37 * t86
      t700 = t697 * t50 * x2 * x1
      t703 = t73 * t123
      t707 = x3 * x1
      t712 = t691 * t53
      t715 = t64 * x3 * x2
      t716 = t305 * t715
      t721 = t50 * t53
      t723 = t86 * t21 * x2
      t727 = t32 * t24
      t733 = t31 * t354
      t734 = t37 * t112
      t735 = t50 * t52
      t741 = t189 * t60 * t73
      t749 = t31 * t231
      t750 = t299 * t21
      t758 = z * t52
      t770 = t52 * t55 * x3
      t774 = t405 * t226
      t778 = t79 * t42
      t782 = -0.32D2 * t664 * t670 - 0.14D2 * t421 * t422 * t587 + 0.64D
     #2 * t120 * t691 * t79 * t519 - 0.32D2 * t696 * t700 - 0.24D2 * t31
     #2 * t703 * t37 * t429 * x2 * t707 * t7 - 0.74D2 * t31 * t712 * t71
     #6 - 0.133D3 * t31 * t478 * t7 * t721 * t723 + 0.48D2 * t31 * t727 
     #* t697 * t576 * t45 - 0.205D3 * t733 * t734 * t735 * t80 + 0.16D2 
     #* t741 * t249 * t715 - 0.67D2 * t39 * t51 * t481 * t21 + 0.205D3 *
     # t749 * t404 * t750 + 0.384D3 * t491 * t299 * t45 * t196 + 0.32D2 
     #* t356 * t142 * t758 * t55 - 0.384D3 * t416 * t126 * t417 * z + 0.
     #104D3 * t153 * t395 * t112 * t468 * t770 - 0.120D3 * t403 * t468 *
     # t774 - 0.6D1 * t298 * t778 * t625
      t785 = t31 * t34
      t786 = t37 * t42
      t791 = t79 * t441
      t796 = t52 * t80
      t802 = t55 * t24 * x3
      t814 = t27 * t61
      t816 = t299 * t158
      t821 = t299 * t45 * t64 * t226
      t831 = t27 * t33
      t834 = t112 * t52
      t845 = t33 * t170
      t846 = t845 * t37
      t847 = t31 * t846
      t848 = t66 * x3
      t858 = t31 * t231 * t7
      t859 = t558 * x3
      t873 = 0.16D2 * t190 * t656 + 0.32D2 * t785 * t786 * t721 * t349 -
     # 0.6D1 * t75 * t78 * t791 * t21 + 0.205D3 * t233 * t142 * t796 * x
     #3 - 0.120D3 * t155 * t516 * t802 + 0.7D1 * t785 * t786 * t721 * t4
     #41 + 0.64D2 * t120 * t132 * t53 * t309 + 0.64D2 * t506 * t814 * t8
     #16 + 0.8D1 * t248 * t821 + 0.64D2 * t120 * t814 * t64 * t126 * t86
     # * x1 * t21 - 0.32D2 * t120 * t831 * t64 * t126 * t834 * t55 - 0.3
     #2D2 * t162 * t121 * t461 * t104 * t48 * t349 - 0.64D2 * t847 * t95
     # * t848 + 0.32D2 * t110 * t330 * t114 * t64 * t86 + 0.133D3 * t858
     # * t735 * t859 - 0.32D2 * t133 * t821 - 0.14D2 * t164 * t267 * t66
     #0 + 0.148D3 * t480 * t51 * t53 * t88 * x2
      t877 = t121 * t64
      t878 = t72 * t877
      t890 = t60 * t30 * t278
      t896 = t27 * t73
      t900 = t318 * x2
      t906 = t126 * t86
      t910 = t278 * t123
      t920 = t72 * t479
      t925 = t170 * t208
      t931 = t734 * t50 * t114 * t52
      t936 = t208 * x1 * t21
      t940 = t30 * t33
      t950 = t79 * t112
      t952 = t21 * t114 * t64
      t957 = t80 * x2 * t24
      t969 = t395 * t86
      t971 = t27 * z
      t976 = 0.16D2 * t878 * t78 * t7 * t79 * t545 + 0.32D2 * t198 + 0.3
     #84D3 * t416 * t126 * t417 * t196 - 0.42D2 * t890 * t293 * t184 * t
     #53 * t112 - 0.64D2 * t120 * t896 * t170 * t126 * t112 * t900 + 0.6
     #4D2 * t120 * t896 * t123 * t906 * t848 - 0.32D2 * t31 * t910 * t78
     #6 * t50 * t184 * t53 - 0.133D3 * t696 * t468 * t707 + 0.48D2 * t92
     #0 * t174 * t612 * x3 - 0.48D2 * t298 * t925 * t67 + 0.64D2 * t500 
     #* t931 - 0.12D2 * t264 * t395 * t95 * t936 + 0.48D2 * t60 * t940 *
     # t170 * t44 * t6 * x3 + 0.12D2 * t225 * t621 * t67 + 0.32D2 * t495
     # * t950 * t952 + 0.8D1 * t741 * t950 * t957 + 0.8D1 * t189 * t691 
     #* t123 * t697 * t27 * t66 * t208 - 0.24D2 * t378 * t33 * t969 * t9
     #71 * t7 * t406
      t977 = t264 * t206
      t987 = t60 * t940 * t53
      t990 = t31 * t446
      t994 = t31 * t313 * t7
      t995 = t11 * t5
      t1016 = t950 * t486
      t1022 = t31 * t845
      t1028 = t950 * t116
      t1037 = t72 * t877 * t37
      t1038 = t77 * t79
      t1044 = t734 * t735 * t55
      t1052 = 0.16D2 * t977 * t417 * t621 - 0.64D2 * t63 * t758 * x2 * t
     #24 * t5 + 0.48D2 * t987 * t519 - 0.32D2 * t990 * t509 + 0.148D3 * 
     #t994 * t735 * t995 + 0.64D2 * t987 * z * t86 * t518 - 0.64D2 * t31
     # * t132 * t52 * z * t64 * t995 + 0.32D2 * t190 * t191 * t516 * t55
     # - 0.64D2 * t120 * t712 * t716 - 0.38D2 * t298 * t1016 - 0.176D3 *
     # t298 * t950 * t612 - 0.67D2 * t1022 * t404 * t707 - 0.32D2 * t120
     # * t50 * t121 * t1028 + 0.16D2 * t388 * t389 * z * t208 * x3 - 0.1
     #2D2 * t1037 * t1038 * t612 * x2 - 0.5D1 * t858 * t1044 - 0.64D2 * 
     #t324 * t89 + 0.64D2 * t491 * t758 * t558
      t1059 = t162 * t73 * t52
      t1124 = t52 * t27
      t1136 = t162 * t121 * t79
      t1141 = -0.14D2 * t264 * t346 * t142 * t382 + 0.48D2 * t1059 * t92
     #5 * t859 - 0.64D2 * t298 * t87 * t518 - 0.5D1 * t495 * t1028 + 0.1
     #6D2 * t72 * t121 * t123 * t64 * t363 * t7 * t242 - 0.42D2 * t890 *
     # t778 * t45 * t184 * t170 + 0.48D2 * t264 * t969 * t468 * t406 + 0
     #.8D1 * t72 * t910 * t37 * t174 * t115 * x3 + 0.8D1 * t189 * t60 * 
     #t121 * t1028 - 0.64D2 * t31 * t323 * t52 * z * t5 * t158 - 0.16D2 
     #* t63 * t113 * t612 + 0.16D2 * t378 * t386 * t363 * t971 * t209 + 
     #0.32D2 * t72 * t703 * t697 * t96 * t258 + 0.8D1 * t72 * t478 * t78
     #6 * t53 * t441 * x3 - 0.24D2 * t72 * t313 * t734 * t796 * t226 - 0
     #.14D2 * t264 * t1124 * t241 * t660 - 0.32D2 * t173 * t42 * t7 * x3
     # * t53 * t349 + 0.48D2 * t1136 * t516 * t660 * t55
      t1142 = t121 * t53
      t1143 = t162 * t1142
      t1150 = t170 * t7
      t1181 = t697 * t50
      t1198 = t417 * x3
      t1208 = t363 * t364 * x3
      t1211 = t95 * z
      t1223 = -0.32D2 * t1143 * t305 * t674 * t21 + 0.8D1 * t162 * t278 
     #* t79 * t1150 * t112 * t21 * t114 - 0.6D1 * t878 * t78 * t791 * x2
     # + 0.6D1 * t298 * t778 * t596 + 0.8D1 * t248 * t87 * t80 * t24 * x
     #3 - 0.42D2 * t1059 * t241 * t674 - 0.6D1 * t110 * t778 * t441 * x2
     # * t24 - 0.120D3 * t63 * t219 * t135 + 0.80D2 * t31 * t703 * t7 * 
     #t1181 * t848 - 0.32D2 * t120 * t831 * t293 * t363 * t226 + 0.8D1 *
     # t378 * t846 * t429 * z * t936 - 0.48D2 * t298 * t113 * t957 - 0.1
     #6D2 * t990 * t126 * t1198 - 0.24D2 * t271 * t834 * t45 * t226 * x2
     # + 0.64D2 * t416 * t1208 - 0.64D2 * t94 * t1211 * t406 - 0.14D2 * 
     #t240 * t293 * t226 * x2 * t5 - 0.12D2 * t240 * t281 * t995
      t1227 = t120 * t831 * t170
      t1245 = t95 * t406
      t1285 = t31 * t386
      t1300 = 0.64D2 * t1227 * t126 * t86 * x2 * x1 + 0.8D1 * t427 * t39
     #5 * t417 * t553 + 0.104D3 * t110 * t1150 * t496 - 0.42D2 * t75 * t
     #78 * t183 * t349 + 0.64D2 * t506 * t93 * t1245 + 0.16D2 * t987 * t
     #309 + 0.32D2 * t416 * t363 * t50 * x3 + 0.64D2 * t94 * t1245 + 0.4
     #8D2 * t847 * t95 * t96 * t226 - 0.24D2 * t741 * t950 * t45 * t518 
     #- 0.14D2 * t1037 * t1038 * t486 * x2 - 0.6D1 * t72 * t340 * t37 * 
     #t1038 * t88 * t114 - 0.64D2 * t233 * t142 * t770 - 0.6D1 * t110 * 
     #t286 * t67 - 0.14D2 * t60 * t215 * t79 * t104 * t952 - 0.38D2 * t1
     #285 * t126 * t417 * t226 - 0.32D2 * t990 * t363 * t364 * t226 + 0.
     #32D2 * t198 * t33 * t114 * t52 * t64 * t86
      t1326 = x1 * z
      t1347 = t153 * t253 * t37
      t1372 = -0.64D2 * t233 * t142 * z * t900 + 0.8D1 * t1059 * t592 * 
     #t558 - 0.24D2 * t1136 * t65 * t112 * t55 * x2 - 0.32D2 * t506 * t3
     #54 * t1044 - 0.2D1 * t298 * t778 * t571 + 0.133D3 * t749 * t468 * 
     #t502 + 0.16D2 * t298 * t136 + 0.64D2 * t31 * t727 * t37 * t95 * t1
     #326 * t21 - 0.64D2 * t378 * t93 * t429 * t66 * z - 0.32D2 * t733 *
     # t1044 - 0.5D1 * t31 * t139 * t7 * t931 + 0.12D2 * t103 * t42 * t8
     #0 * t518 + 0.32D2 * t1347 * t417 * t580 - 0.6D1 * t427 * t1124 * t
     #241 * t48 + 0.384D3 * t31 * t446 * t7 * t1208 + 0.80D2 * t31 * t92
     # * t7 * t697 * t576 * t21 - 0.176D3 * t1285 * t208 * t37 * t1198 +
     # 0.32D2 * t298 * t87 * t802
      t1412 = t142 * t52
      t1440 = -0.74D2 * t994 * t734 * t50 * t900 + 0.64D2 * t1227 * t906
     # * t406 - 0.96D2 * t733 * t468 * t750 + 0.32D2 * t63 * t1326 * t66
     #0 + 0.64D2 * t491 * t1326 * t48 + 0.14D2 * t495 * t778 * t80 * t11
     #4 * t64 + 0.32D2 * t155 * t25 * t112 * t349 - 0.24D2 * t977 * t417
     # * t227 - 0.24D2 * t1347 * t417 * t587 + 0.8D1 * t1347 * t417 * t6
     #33 + 0.48D2 * t785 * t468 * t87 * t55 + 0.133D3 * t233 * t1412 * t
     #88 * x3 - 0.64D2 * t120 * t814 * t1150 * t363 * x3 - 0.32D2 * t890
     # * t79 * t184 * t170 * t112 + 0.32D2 * t847 * t95 * t774 - 0.32D2 
     #* t133 * t1016 - 0.74D2 * t1022 * t468 * x1 * t226 + 0.104D3 * t92
     #0 * t174 * t486 * x3
      t1446 = t31 * t314
      t1486 = t31 * t845 * t7
      t1504 = -0.120D3 * t72 * t314 * t834 * t88 * t226 + 0.32D2 * t1446
     # * t142 * z * t114 * t52 - 0.6D1 * t155 * t608 * t48 + 0.48D2 * t1
     #62 * t1142 * t170 * t257 * t524 + 0.176D3 * t990 * t630 - 0.6D1 * 
     #t280 * t293 * t282 * x3 + 0.80D2 * t63 * t327 + 0.64D2 * t63 * t81
     #6 - 0.5D1 * t298 * t821 + 0.104D3 * t1059 * t1150 * t558 * t226 + 
     #0.16D2 * t1143 * t925 * t723 - 0.260D3 * t1285 * t653 - 0.32D2 * t
     #63 * t501 * t24 * t5 + 0.16D2 * t1486 * t700 + 0.64D2 * t847 * t12
     #11 * t848 - 0.133D3 * t1446 * t1412 * t45 * x3 * x2 - 0.384D3 * t4
     #91 * t299 * t45 * z + 0.16D2 * t1486 * t1181 * t406
      rrgg2ggh61J1 = 0.9D1 / 0.16D2 * wd * (t205 + t334 + t445 + t539 + 
     #t616 + t684 + t782 + t873 + t976 + t1052 + t1141 + t1223 + t1300 +
     # t1372 + t1440 + t1504) / t27 / t28 / t50 / z / 0.3141592653589793
     #D1

      end function
  
   
 

      doubleprecision function rrgg2ggh61J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t11 = x2 * t10
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + t11 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = s ** 2
      t28 = t27 ** 2
      t29 = t28 * s
      t30 = t26 * t29
      t31 = t1 ** 2
      t32 = t31 ** 2
      t33 = t23 ** 2
      t34 = t33 ** 2
      t35 = t32 * t34
      t38 = z + x1 * t7 * t1
      t40 = t30 * t35 * t38
      t43 = x2 * x3
      t44 = t10 * t7 * t4 + t43 + t19
      t47 = t23 * t10
      t49 = s - t2 * t6 * t44 - t2 * t47
      t50 = t5 * t49
      t51 = t10 ** 2
      t56 = 0.16D2 * t40 * t50 * z * t51 * x3
      t57 = t49 * t29
      t58 = t33 * t23
      t59 = t32 * t58
      t60 = t59 * t38
      t61 = t57 * t60
      t62 = t4 ** 2
      t63 = 0.1D1 / t62
      t64 = t63 * t26
      t65 = t64 * z
      t66 = t43 * x1
      t68 = t61 * t65 * t66
      t70 = t31 * t1
      t71 = t70 * t33
      t72 = t71 * t38
      t73 = t57 * t72
      t74 = x1 * t44
      t75 = t74 * x3
      t76 = t64 * t75
      t78 = 0.64D2 * t73 * t76
      t79 = t32 * t23
      t80 = t57 * t79
      t81 = t62 ** 2
      t82 = 0.1D1 / t81
      t83 = t38 * t82
      t84 = x1 ** 2
      t85 = t84 * x1
      t86 = t26 * t85
      t87 = t44 ** 2
      t88 = t87 * t44
      t92 = 0.32D2 * t80 * t83 * t86 * t88
      t93 = t70 * t23
      t94 = t57 * t93
      t96 = 0.1D1 / t62 / t4
      t97 = t38 * t96
      t98 = t26 * t84
      t100 = t97 * t98 * t87
      t102 = 0.32D2 * t94 * t100
      t103 = t28 * t27
      t104 = t49 * t103
      t105 = t32 * t31
      t106 = t105 * t85
      t109 = t10 * t63
      t111 = t44 * x3 * x2
      t114 = 0.48D2 * t104 * t106 * t58 * t109 * t111
      t115 = t26 * t32
      t116 = t115 * t85
      t117 = t57 * t116
      t120 = t44 * x2 * t23
      t122 = t117 * z * t63 * t120
      t127 = t43 * t5
      t129 = t57 * t115 * t84 * z * t33 * t127
      t131 = t103 * t32
      t133 = t131 * t85 * t49
      t134 = t20 ** 2
      t135 = t63 * t134
      t138 = 0.12D2 * t133 * t135 * t24
      t139 = t26 * t103
      t140 = t105 * t34
      t141 = t140 * t38
      t142 = t139 * t141
      t144 = x3 ** 2
      t149 = 0.48D2 * t142 * t96 * t10 * t144 * t84 * t87
      t150 = t105 * t58
      t151 = t150 * t38
      t152 = t139 * t151
      t158 = 0.32D2 * t152 * t82 * t10 * x3 * t85 * t88
      t159 = t96 * t20
      t160 = t159 * t120
      t162 = 0.48D2 * t117 * t160
      t163 = t26 * t49
      t164 = t29 * t105
      t167 = x2 ** 2
      t168 = t10 * t167
      t169 = t84 * t63
      t173 = 0.14D2 * t163 * t164 * t34 * t168 * t169 * x3
      t174 = t58 * t38
      t175 = t174 * t63
      t177 = t26 * t10
      t180 = 0.48D2 * t131 * t175 * t177 * t75
      t181 = t32 * t70
      t182 = t181 * t34
      t185 = t82 * t85
      t186 = t20 * t167
      t190 = 0.8D1 * t139 * t182 * t38 * t185 * t186 * x3
      t191 = t32 * t1
      t192 = t191 * t58
      t193 = t57 * t192
      t194 = t51 * t26
      t195 = t84 * x2
      t196 = t195 * t5
      t198 = t193 * t194 * t196
      t200 = t84 ** 2
      t202 = t104 * t191 * t200
      t206 = 0.32D2 * t202 * t47 * t96 * t88
      t207 = t56 + 0.64D2 * t68 + t78 + t92 - t102 + t114 + 0.64D2 * t12
     #2 - 0.64D2 * t129 - t138 + t149 - t158 + t162 - t173 + t180 + t190
     # + 0.67D2 * t198 + t206
      t208 = t191 * t34
      t211 = t38 * t63
      t212 = t211 * t26
      t215 = 0.80D2 * t57 * t208 * t10 * t212 * t66
      t216 = t84 * t5
      t217 = t216 * t44
      t219 = t94 * t177 * t217
      t221 = t31 * t33
      t222 = t57 * t221
      t223 = t38 * t5
      t227 = 0.32D2 * t222 * t223 * t26 * x3
      t228 = t10 * t38
      t229 = t5 * t26
      t230 = z ** 2
      t234 = 0.384D3 * t222 * t228 * t229 * t230
      t236 = t139 * t191 * t23
      t238 = 0.1D1 / t81 / t4
      t239 = t38 * t238
      t240 = t200 * t20
      t244 = 0.42D2 * t236 * t239 * t240 * t88
      t245 = t26 * t70
      t250 = t44 * t23 * x3
      t252 = t57 * t245 * t84 * z * t5 * t250
      t254 = t57 * t245
      t255 = t84 * z
      t259 = t254 * t255 * x2 * t23 * t5
      t261 = t63 * t20
      t262 = t261 * t250
      t264 = 0.48D2 * t133 * t262
      t265 = t96 * t134
      t268 = 0.48D2 * t202 * t265 * t250
      t271 = t26 * x1
      t275 = 0.80D2 * t57 * t71 * t10 * t211 * t271 * t44
      t279 = t87 ** 2
      t283 = 0.16D2 * t104 * t105 * t200 * t84 * t238 * t20 * t279
      t286 = t51 * x1 * t44
      t289 = 0.12D2 * t131 * t174 * t64 * t286
      t290 = t103 * z
      t291 = t290 * t245
      t294 = t216 * t20 * t23 * x3
      t296 = 0.64D2 * t291 * t294
      t297 = t290 * t115
      t300 = t216 * t20 * t33 * t144
      t302 = 0.32D2 * t297 * t300
      t303 = t26 * t191
      t307 = 0.64D2 * t290 * t303 * t200 * t160
      t308 = t74 * t144
      t311 = 0.32D2 * t61 * t64 * t308
      t312 = t70 * t58
      t313 = t57 * t312
      t314 = t229 * x3
      t317 = 0.16D2 * t313 * t228 * t314
      t318 = t34 * t23
      t322 = t144 * x3
      t327 = 0.32D2 * t139 * t105 * t318 * t38 * t109 * t322 * x1 * t44
      t328 = t215 - 0.96D2 * t219 + t227 + t234 - t244 - 0.64D2 * t252 -
     # 0.64D2 * t259 + t264 + t268 + t275 + t283 - t289 + t296 - t302 + 
     #t307 + t311 - t317 - t327
      t330 = t57 * t115
      t332 = t44 * t33 * t144
      t335 = 0.64D2 * t330 * t216 * t332
      t336 = t51 * t38
      t337 = t336 * t229
      t339 = 0.176D3 * t313 * t337
      t340 = t200 * t82
      t341 = t134 * t87
      t344 = 0.6D1 * t330 * t340 * t341
      t347 = t97 * t26 * t167 * t84
      t349 = 0.64D2 * t193 * t347
      t353 = 0.384D3 * t222 * t228 * t229 * z
      t354 = x1 * z
      t355 = t33 * t144
      t357 = t254 * t354 * t355
      t359 = t29 * z
      t361 = t359 * t163 * t32
      t363 = 0.8D1 * t361 * t300
      t364 = t49 * t32
      t366 = t290 * t364 * t58
      t367 = t228 * t63
      t370 = 0.64D2 * t366 * t367 * t75
      t374 = t144 ** 2
      t378 = 0.16D2 * t139 * t105 * t34 * t33 * t223 * t10 * t374
      t380 = t57 * t93 * t38
      t381 = t96 * t26
      t382 = t84 * t20
      t385 = t380 * t381 * t382 * t44
      t387 = t359 * t26
      t388 = t192 * t38
      t390 = t96 * t49
      t392 = t84 * t44
      t396 = 0.16D2 * t387 * t388 * t390 * x2 * t392 * t10
      t397 = t32 * t33
      t398 = t397 * t38
      t399 = t57 * t398
      t400 = t84 * t134
      t403 = t399 * t381 * t400 * x3
      t405 = t85 * t63
      t406 = t405 * t20
      t408 = t361 * t406 * t250
      t411 = t57 * t59 * t10
      t414 = 0.16D2 * t411 * t212 * t75
      t415 = t57 * t397
      t417 = t415 * t177 * t196
      t421 = 0.64D2 * t61 * t64 * t66
      t423 = t82 * t26
      t424 = t85 * t20
      t427 = t57 * t151 * t423 * t424 * t167
      t430 = t163 * t29 * t31
      t431 = t20 * t44
      t434 = 0.24D2 * t430 * t169 * t431
      t435 = -t335 + t339 + t344 + t349 - t353 + 0.32D2 * t357 + t363 + 
     #t370 + t378 - 0.133D3 * t385 + t396 + 0.205D3 * t403 - 0.24D2 * t4
     #08 + t414 + 0.133D3 * t417 - t421 - 0.74D2 * t427 - t434
      t436 = t163 * t29
      t437 = t10 * t144
      t440 = 0.8D1 * t436 * t312 * t437
      t441 = t103 * t230
      t444 = 0.32D2 * t441 * t93 * t100
      t445 = t31 * t84
      t446 = t5 * t44
      t449 = 0.32D2 * t436 * t445 * t446
      t450 = t200 * t96
      t451 = t134 * t44
      t454 = 0.176D3 * t330 * t450 * t451
      t455 = t405 * t431
      t457 = 0.16D2 * t254 * t455
      t458 = t10 * x3
      t461 = 0.24D2 * t436 * t221 * t458
      t462 = t103 * t191
      t468 = 0.14D2 * t462 * t174 * t381 * t51 * t84 * t87
      t469 = t5 * t20
      t470 = t445 * t469
      t472 = 0.38D2 * t436 * t470
      t475 = 0.32D2 * t436 * t3 * t469
      t477 = t430 * t354 * t24
      t479 = t103 * t70
      t480 = t479 * t85
      t481 = t49 * t63
      t484 = 0.8D1 * t480 * t481 * t431
      t487 = 0.64D2 * t330 * t405 * t120
      t488 = t26 * t105
      t489 = t57 * t488
      t490 = t186 * t33
      t491 = t450 * t490
      t493 = 0.5D1 * t489 * t491
      t494 = t200 * x1
      t495 = t462 * t494
      t496 = t49 * t82
      t497 = t20 * t88
      t500 = 0.32D2 * t495 * t496 * t497
      t503 = t57 * t31 * t47 * t271
      t505 = t10 * t322
      t508 = 0.2D1 * t436 * t35 * t505
      t509 = t131 * t200
      t510 = t20 * t87
      t513 = 0.24D2 * t509 * t390 * t510
      t514 = t51 * t144
      t517 = 0.6D1 * t436 * t35 * t514
      t518 = t440 - t444 + t449 - t454 - t457 - t461 - t468 - t472 + t47
     #5 + 0.64D2 * t477 + t484 - t487 - t493 + t500 + 0.48D2 * t503 - t5
     #08 - t513 + t517
      t522 = t51 * t10
      t523 = t58 * t522
      t525 = t57 * t32 * t523 * t271
      t529 = 0.24D2 * t436 * t445 * t135
      t532 = 0.24D2 * t495 * t496 * t341
      t534 = t163 * t29 * t181
      t535 = t34 * t10
      t536 = t167 * x2
      t541 = 0.42D2 * t534 * t535 * t536 * t85 * t96
      t542 = t58 * t51
      t543 = x1 * x2
      t544 = t543 * t5
      t547 = 0.48D2 * t330 * t542 * t544
      t548 = t34 * t38
      t554 = 0.6D1 * t462 * t548 * t64 * t522 * x1 * t44
      t556 = t57 * t397 * t10
      t558 = 0.5D1 * t556 * t100
      t560 = 0.64D2 * t291 * t455
      t561 = t33 * t38
      t567 = 0.6D1 * t479 * t561 * t64 * t10 * x1 * t44
      t568 = t134 * t20
      t569 = t200 * t568
      t573 = 0.6D1 * t236 * t239 * t569 * t44
      t574 = t200 * t134
      t578 = 0.14D2 * t236 * t239 * t574 * t87
      t580 = t33 * t51
      t582 = t57 * t70 * t580 * t271
      t584 = t131 * t548
      t585 = t51 * x3
      t588 = 0.16D2 * t584 * t229 * t585
      t589 = t70 * t85
      t590 = t96 * t568
      t593 = 0.8D1 * t436 * t589 * t590
      t598 = 0.8D1 * t30 * t60 * t481 * z * t286
      t600 = t390 * z
      t602 = t10 * t84 * t87
      t605 = 0.8D1 * t30 * t398 * t600 * t602
      t606 = t85 * t96
      t609 = 0.16D2 * t254 * t606 * t451
      t610 = t33 * t10
      t613 = 0.120D3 * t254 * t610 * t544
      t614 = 0.7D1 * t525 + t529 - t532 - t541 - t547 - t554 - t558 - t5
     #60 - t567 - t573 - t578 - 0.205D3 * t582 + t588 + t593 + t598 + t6
     #05 - t609 - t613
      t617 = 0.16D2 * t509 * t390 * t451
      t618 = t446 * x3
      t620 = t556 * t98 * t618
      t622 = t85 * z
      t623 = t63 * t87
      t626 = 0.32D2 * t254 * t622 * t623
      t629 = 0.6D1 * t479 * t58 * t337
      t630 = t134 ** 2
      t634 = 0.2D1 * t236 * t239 * t200 * t630
      t639 = 0.64D2 * t366 * t228 * t63 * x2 * x1
      t641 = t104 * t191 * t85
      t644 = 0.24D2 * t641 * t580 * t623
      t645 = t104 * t191
      t646 = x1 * t34
      t647 = t522 * x3
      t650 = 0.6D1 * t645 * t646 * t647
      t653 = 0.64D2 * t430 * t255 * t446
      t656 = 0.6D1 * t436 * t35 * t647
      t657 = t57 * t388
      t661 = t657 * t381 * z * t167 * t84
      t665 = 0.6D1 * t202 * t590 * t24
      t670 = 0.42D2 * t534 * t340 * t20 * t536 * t58
      t671 = t181 * t58
      t674 = t238 * t200
      t678 = 0.6D1 * t139 * t671 * t38 * t674 * t431 * t167
      t679 = t57 * t303
      t680 = t85 * t167
      t684 = 0.64D2 * t679 * t680 * t33 * t63
      t690 = 0.42D2 * t462 * t561 * t423 * t10 * t85 * t88
      t691 = t103 * t31
      t693 = t223 * t177
      t695 = 0.2D1 * t691 * t33 * t693
      t698 = t82 * t20
      t702 = 0.32D2 * t104 * t105 * t494 * t698 * t24 * t88
      t703 = t617 + 0.133D3 * t620 - t626 - t629 - t634 + t639 - t644 - 
     #t650 + t653 - t656 + 0.32D2 * t661 - t665 - t670 - t678 + t684 - t
     #690 - t695 - t702
      t709 = 0.14D2 * t489 * t340 * t134 * t167 * t33
      t711 = t57 * t192 * t10
      t713 = t392 * x2
      t716 = 0.74D2 * t711 * t97 * t26 * t713
      t717 = x1 * t20
      t720 = t73 * t64 * t717 * x3
      t725 = t84 * t87 * x3
      t728 = 0.104D3 * t462 * t174 * t96 * t177 * t725
      t729 = t49 * t70
      t731 = t58 * t10
      t735 = 0.64D2 * t290 * t729 * t731 * t223 * x3
      t740 = 0.32D2 * t290 * t364 * t535 * t223 * t144
      t745 = 0.8D1 * t361 * t405 * t134 * t23 * x3
      t746 = t381 * t84
      t750 = t657 * t746 * t20 * x3 * x2
      t752 = t57 * t59
      t753 = x3 * x1
      t755 = t752 * t194 * t753
      t757 = t1 * t23
      t759 = t26 * z
      t762 = 0.32D2 * t57 * t757 * t223 * t759
      t764 = t51 ** 2
      t768 = 0.2D1 * t462 * t318 * t223 * t26 * t764
      t769 = t57 * t35
      t771 = t522 * t38 * t229
      t773 = 0.260D3 * t769 * t771
      t775 = t57 * t79 * t38
      t778 = t775 * t423 * t424 * t87
      t781 = t359 * t163 * t191
      t784 = t781 * t450 * t20 * t120
      t786 = t34 * t51
      t787 = t167 * t84
      t788 = t787 * t63
      t791 = 0.14D2 * t489 * t786 * t788
      t793 = t104 * t105 * t84
      t797 = 0.16D2 * t793 * t469 * t34 * t374
      t800 = 0.104D3 * t679 * t606 * t490
      t804 = 0.120D3 * t641 * t610 * t623 * x3
      t805 = t709 - t716 - 0.96D2 * t720 + t728 - t735 - t740 + t745 - 0
     #.133D3 * t750 - 0.67D2 * t755 - t762 - t768 - t773 - 0.74D2 * t778
     # - 0.24D2 * t784 + t791 + t797 + t800 - t804
      t810 = 0.32D2 * t139 * t208 * t211 * t717 * t322
      t811 = t49 * t191
      t817 = 0.64D2 * t290 * t811 * t58 * t228 * t96 * t713
      t821 = t44 * t167 * t33
      t824 = 0.14D2 * t163 * t164 * t200 * t698 * t821
      t827 = 0.176D3 * t436 * t589 * t135
      t830 = 0.6D1 * t131 * t34 * t771
      t831 = t191 * t33
      t835 = t63 * t44 * x2
      t837 = t57 * t831 * t10 * t86 * t835
      t840 = t711 * t98 * t127
      t844 = t96 * t84
      t848 = 0.32D2 * t290 * t364 * t33 * t228 * t844 * t87
      t849 = t57 * t71
      t852 = t211 * t26 * x2 * x1
      t854 = 0.32D2 * t849 * t852
      t857 = 0.104D3 * t679 * t731 * t788
      t858 = t32 * t200
      t859 = t858 * t590
      t861 = 0.260D3 * t436 * t859
      t863 = t70 * x1 * t355
      t865 = 0.32D2 * t436 * t863
      t868 = t80 * t83 * t86 * t568
      t872 = t399 * t746 * t431 * x3
      t874 = t441 * t49
      t876 = 0.32D2 * t874 * t863
      t879 = 0.176D3 * t769 * t336 * t314
      t882 = t399 * t381 * z * t713
      t884 = t359 * t163
      t888 = 0.16D2 * t884 * t858 * t265 * t44
      t889 = t810 - t817 - t824 + t827 - t830 - 0.133D3 * t837 + 0.148D3
     # * t840 - t848 - t854 + t857 - t861 - t865 + 0.7D1 * t868 + 0.133D
     #3 * t872 - t876 - t879 - 0.64D2 * t882 + t888
      t893 = t589 * t623
      t900 = t104 * t105 * t200
      t905 = t104 * t106
      t906 = t58 * t322
      t919 = t104 * t191 * t84
      t928 = t104 * t181 * t85
      t929 = t167 * t63
      t935 = t450 * t510
      t939 = t103 * t230 * z
      t940 = t49 * t31
      t942 = t610 * t223
      t946 = t87 * t23 * x3
      t950 = t84 * t49
      t955 = t831 * t38
      t956 = t139 * t955
      t974 = -0.32D2 * t436 * t893 - 0.32D2 * t290 * t26 * t470 + 0.48D2
     # * t900 * t159 * t355 * t87 - 0.32D2 * t905 * t261 * t906 * t44 - 
     #0.14D2 * t641 * t135 * t355 + 0.32D2 * t436 * t32 * x1 * t906 + 0.
     #104D3 * t919 * t731 * t446 * t144 + 0.16D2 * t905 * t542 * t835 - 
     #0.6D1 * t928 * t786 * t929 - 0.5D1 * t330 * t300 - 0.38D2 * t330 *
     # t935 - 0.128D3 * t939 * t940 * t942 - 0.120D3 * t202 * t159 * t94
     #6 - 0.14D2 * t131 * t950 * t469 * t355 + 0.104D3 * t956 * t185 * t
     #510 * x3 - 0.6D1 * t139 * t671 * t239 * t574 * t167 - 0.42D2 * t64
     #5 * t646 * t505 + 0.8D1 * t30 * t141 * t600 * t168 * t84
      t977 = 0.32D2 * t489 * t450 * t821
      t980 = 0.128D3 * t939 * t26 * t470
      t983 = 0.14D2 * t645 * t646 * t514
      t985 = 0.32D2 * t874 * t893
      t988 = t223 * t26 * t144
      t990 = 0.32D2 * t441 * t312 * t988
      t993 = 0.8D1 * t919 * t523 * t446
      t998 = 0.24D2 * t900 * t610 * t96 * t87 * x2
      t1000 = t134 * x2 * t23
      t1003 = 0.48D2 * t330 * t606 * t1000
      t1007 = 0.32D2 * t884 * t858 * t159 * t87
      t1011 = 0.384D3 * t430 * t216 * t20 * t230
      t1015 = 0.384D3 * t254 * t405 * t431 * z
      t1016 = t57 * t955
      t1020 = t1016 * t423 * t85 * t431 * x2
      t1022 = t105 * t33
      t1023 = t139 * t1022
      t1027 = 0.6D1 * t1023 * t239 * t569 * x2
      t1032 = 0.24D2 * t142 * t844 * t20 * t144 * x2
      t1035 = 0.64D2 * t399 * t381 * t725
      t1037 = t29 * t230 * t163
      t1038 = 0.32D2 * t1037
      t1043 = 0.120D3 * t139 * t388 * t844 * t431 * t144
      t1047 = 0.8D1 * t359 * t163 * t105 * t491
      t1048 = t977 - t980 - t983 - t985 - t990 + t993 - t998 - t1003 + t
     #1007 + t1011 + t1015 + 0.148D3 * t1020 - t1027 - t1032 - t1035 + t
     #1038 - t1043 + t1047
      t1052 = t94 * t97 * t98 * t134
      t1058 = 0.32D2 * t40 * t50 * z * t10 * t144
      t1059 = t568 * t44
      t1062 = 0.6D1 * t330 * t340 * t1059
      t1063 = t34 * t522
      t1067 = 0.6D1 * t793 * t1063 * x2 * t5
      t1068 = t32 ** 2
      t1074 = 0.2D1 * t104 * t1068 * t200 * t535 * t536 * t96
      t1076 = t415 * t194 * t217
      t1081 = 0.32D2 * t313 * t223 * t759 * t144
      t1083 = t33 * x3 * x2
      t1086 = 0.16D2 * t781 * t406 * t1083
      t1091 = 0.16D2 * t152 * t185 * t134 * x3 * x2
      t1097 = 0.2D1 * t139 * t1068 * t34 * t239 * t240 * t536
      t1100 = t752 * t177 * x1 * t144
      t1102 = t29 * t191
      t1104 = t163 * t1102 * t34
      t1107 = 0.12D2 * t1104 * t585 * t544
      t1111 = 0.48D2 * t152 * t185 * t20 * t111
      t1112 = t31 * t23
      t1116 = t57 * t1112 * t211 * t271 * t20
      t1120 = 0.64D2 * t290 * t116 * t262
      t1122 = t163 * t1102 * t200
      t1127 = 0.6D1 * t1122 * t698 * t87 * x2 * t23
      t1132 = 0.32D2 * t57 * t141 * t381 * t787 * x3
      t1136 = 0.2D1 * t691 * t84 * t50 * t20
      t1137 = -0.205D3 * t1052 + t1058 - t1062 - t1067 - t1074 + 0.205D3
     # * t1076 - t1081 + t1086 + t1091 - t1097 - 0.74D2 * t1100 + t1107 
     #+ t1111 + 0.48D2 * t1116 + t1120 - t1127 + t1132 - t1136
      t1140 = 0.32D2 * t436 * t3 * z
      t1143 = 0.2D1 * t495 * t496 * t630
      t1145 = t216 * t250
      t1147 = 0.64D2 * t441 * t729 * t1145
      t1148 = t85 * t134
      t1151 = t1016 * t423 * t1148 * x2
      t1154 = t20 * x2 * t23
      t1155 = t405 * t1154
      t1157 = 0.16D2 * t330 * t1155
      t1159 = t139 * t1022 * t38
      t1163 = 0.14D2 * t1159 * t674 * t510 * x2
      t1166 = 0.2D1 * t330 * t340 * t497
      t1171 = 0.16D2 * t1023 * t239 * t10 * t200 * t279
      t1175 = 0.32D2 * t254 * t195 * t23 * t5
      t1180 = 0.24D2 * t139 * t192 * t97 * t400 * t144
      t1182 = 0.38D2 * t222 * t693
      t1189 = 0.8D1 * t359 * t303 * t34 * t211 * t49 * t543 * t51
      t1193 = t30 * t72 * t481 * t543 * z
      t1196 = 0.16D2 * t884 * t859
      t1202 = 0.32D2 * t1037 * t32 * t167 * t84 * t33 * t63
      t1206 = 0.38D2 * t769 * t228 * t229 * t144
      t1210 = 0.12D2 * t1159 * t674 * t451 * x2
      t1213 = 0.6D1 * t1104 * t437 * t544
      t1214 = -t1140 - t1143 + t1147 + 0.67D2 * t1151 + t1157 - t1163 - 
     #t1166 + t1171 - t1175 - t1180 - t1182 + t1189 - 0.64D2 * t1193 + t
     #1196 + t1202 - t1206 - t1210 - t1213
      t1223 = 0.48D2 * t163 * t29 * t32 * t58 * t11 * t6 * x3
      t1225 = t462 * t318 * t38
      t1228 = 0.32D2 * t1225 * t229 * t505
      t1229 = t303 * t85
      t1231 = t261 * t1083
      t1233 = 0.64D2 * t290 * t1229 * t1231
      t1236 = 0.64D2 * t441 * t72 * t76
      t1238 = t462 * t548 * t63
      t1241 = 0.120D3 * t1238 * t177 * t308
      t1244 = t399 * t381 * t382 * x2
      t1248 = t61 * t64 * t717 * t144
      t1253 = 0.6D1 * t479 * t950 * t469 * t24
      t1256 = 0.42D2 * t919 * t469 * t906
      t1261 = 0.32D2 * t534 * t200 * t536 * t58 * t96
      t1263 = 0.32D2 * t313 * t988
      t1267 = 0.5D1 * t57 * t140 * t10 * t347
      t1272 = 0.6D1 * t679 * t340 * t568 * x2 * t23
      t1275 = 0.120D3 * t254 * t169 * t1154
      t1277 = 0.80D2 * t1122 * t160
      t1280 = 0.74D2 * t57 * t1229 * t1231
      t1283 = 0.8D1 * t254 * t606 * t510
      t1287 = 0.48D2 * t956 * t185 * t451 * x3
      t1288 = t1223 + t1228 - t1233 + t1236 - t1241 + 0.133D3 * t1244 + 
     #0.48D2 * t1248 - t1253 - t1256 - t1261 - t1263 - t1267 - t1272 - t
     #1275 + t1277 - t1280 + t1283 + t1287
      t1291 = 0.6D1 * t480 * t481 * t134
      t1294 = 0.2D1 * t645 * t646 * t764
      t1297 = 0.6D1 * t679 * t1063 * t544
      t1301 = t57 * t150 * t177 * t680 * t63
      t1306 = 0.8D1 * t479 * t174 * t229 * t458
      t1309 = t49 * z
      t1312 = t30 * t32 * t175 * t1309 * t10 * t75
      t1319 = t387 * t208 * t38 * t481 * x2 * t753 * t10
      t1322 = 0.64D2 * t297 * t1155
      t1325 = 0.32D2 * t290 * t488 * t491
      t1332 = 0.64D2 * t290 * t729 * t33 * t228 * t63 * x1 * t44
      t1336 = 0.12D2 * t1122 * t82 * t134 * t120
      t1341 = 0.64D2 * t290 * t811 * t34 * t367 * t66
      t1349 = 0.32D2 * t290 * t49 * t105 * t34 * t228 * t96 * t167 * t84
      t1351 = 0.16D2 * t411 * t852
      t1354 = 0.32D2 * t330 * t405 * t946
      t1360 = 0.32D2 * t57 * t182 * t83 * t26 * t536 * t85
      t1364 = 0.6D1 * t928 * t535 * t929 * x3
      t1368 = t223 * t759 * x3
      t1370 = 0.384D3 * t57 * t312 * t10 * t1368
      t1371 = -t1291 - t1294 - t1297 - 0.74D2 * t1301 + t1306 - 0.24D2 *
     # t1312 - 0.24D2 * t1319 + t1322 - t1325 + t1332 + t1336 + t1341 - 
     #t1349 + t1351 + t1354 - t1360 - t1364 + t1370
      t1375 = 0.6D1 * t509 * t390 * t568
      t1378 = 0.8D1 * t436 * t312 * t522
      t1381 = 0.32D2 * t436 * t757 * t10
      t1386 = 0.16D2 * t30 * t35 * t223 * t1309 * t522
      t1388 = 0.64D2 * t222 * t1368
      t1393 = t57 * t1112 * t38 * t64 * t354 * t44
      t1397 = t775 * t423 * t1148 * t44
      t1401 = t80 * t177 * t405 * t87
      t1408 = 0.64D2 * t1037 * t31 * x2 * x1 * t23 * t5
      t1411 = 0.2D1 * t436 * t35 * t764
      t1413 = 0.64D2 * t254 * t1145
      t1416 = 0.48D2 * t1238 * t194 * t75
      t1419 = t380 * t381 * t255 * t87
      t1427 = 0.8D1 * t104 * t181 * t200 * t731 * t96 * t44 * t167
      t1429 = 0.16D2 * t117 * t262
      t1432 = 0.24D2 * t436 * t221 * t51
      t1435 = 0.24D2 * t584 * t229 * t437
      t1438 = 0.24D2 * t1225 * t229 * t514
      t1439 = -t1375 + t1378 + t1381 + t1386 + t1388 + 0.64D2 * t1393 - 
     #0.67D2 * t1397 + 0.48D2 * t1401 - t1408 + t1411 + t1413 + t1416 + 
     #0.32D2 * t1419 + t1427 + t1429 + t1432 - t1435 - t1438
      t1442 = 0.8D1 * t1225 * t229 * t647
      t1448 = 0.8D1 * t139 * t831 * t83 * t85 * t568 * x3
      t1450 = 0.32D2 * t297 * t935
      t1452 = t73 * t65 * t75
      t1458 = 0.14D2 * t793 * t535 * t144 * x2 * t5
      t1461 = 0.12D2 * t793 * t786 * t127
      t1464 = 0.104D3 * t641 * t261 * t332
      t1467 = 0.32D2 * t290 * t940 * t942
      t1470 = 0.8D1 * t495 * t496 * t1059
      t1473 = 0.16D2 * t436 * t312 * t585
      t1477 = 0.384D3 * t430 * t216 * t20 * z
      t1481 = 0.2D1 * t436 * t858 * t82 * t630
      t1483 = t849 * t177 * t753
      t1486 = 0.80D2 * t254 * t294
      t1490 = t679 * t622 * t167 * t33 * t63
      t1494 = 0.8D1 * t781 * t450 * t1000
      t1498 = 0.14D2 * t131 * t561 * t381 * t602
      t1501 = 0.48D2 * t919 * t542 * t618
      t1502 = t1442 + t1448 - t1450 - 0.64D2 * t1452 - t1458 - t1461 + t
     #1464 - t1467 + t1470 - t1473 - t1477 + t1481 - 0.133D3 * t1483 + t
     #1486 + 0.32D2 * t1490 + t1494 - t1498 + t1501
      t1513 = -t56 - 0.80D2 * t68 - t78 - t92 + t102 - t114 - 0.80D2 * t
     #122 + 0.32D2 * t129 + t138 - t149 + t158 - t162 + t173 - t180 - t1
     #90 + 0.12D2 * t198 - t206
      t1517 = -t215 + 0.240D3 * t219 - t227 - t234 + t244 + 0.48D2 * t25
     #2 + 0.48D2 * t259 - t264 - t268 - t275 - t283 + t289 - t296 + t302
     # - t307 - t311 + t317 + t327
      t1525 = t335 - t339 - t344 - t349 + t353 - 0.48D2 * t357 - t363 - 
     #t370 - t378 + 0.30D2 * t385 - t396 - 0.102D3 * t403 + 0.48D2 * t40
     #8 - t414 - 0.30D2 * t417 + t421 + 0.40D2 * t427 + t434
      t1528 = -t440 + t444 - t449 + t454 + t457 + t461 + t468 + t472 - t
     #475 - 0.80D2 * t477 - t484 + t487 + t493 - t500 - 0.120D3 * t503 +
     # t508 + t513 - t517
      t1533 = -0.52D2 * t525 - t529 + t532 + t541 + t547 + t554 + t558 +
     # t560 + t567 + t573 + t578 + 0.102D3 * t582 - t588 - t593 - t598 -
     # t605 + t609 + t613
      t1536 = -t617 - 0.30D2 * t620 + t626 + t629 + t634 - t639 + t644 +
     # t650 - t653 + t656 - 0.48D2 * t661 + t665 + t670 + t678 - t684 + 
     #t690 + t695 + t702
      t1543 = -t709 + t716 + 0.240D3 * t720 - t728 + t735 + t740 - t745 
     #+ 0.30D2 * t750 - 0.12D2 * t755 + t762 + t768 + t773 + 0.40D2 * t7
     #78 + 0.48D2 * t784 - t791 - t797 - t800 + t804
      t1549 = -t810 + t817 + t824 - t827 + t830 + 0.30D2 * t837 - 0.80D2
     # * t840 + t848 + t854 - t857 + t861 + t865 - 0.52D2 * t868 - 0.30D
     #2 * t872 + t876 + t879 + 0.32D2 * t882 - t888
      t1555 = -t977 + t980 + t983 + t985 + t990 - t993 + t998 + t1003 - 
     #t1007 - t1011 - t1015 - 0.80D2 * t1020 + t1027 + t1032 + t1035 - t
     #1038 + t1043 - t1047
      t1561 = 0.102D3 * t1052 - t1058 + t1062 + t1067 + t1074 - 0.102D3 
     #* t1076 + t1081 - t1086 - t1091 + t1097 + 0.40D2 * t1100 - t1107 -
     # t1111 - 0.120D3 * t1116 - t1120 + t1127 - t1132 + t1136
      t1564 = t1140 + t1143 - t1147 + 0.12D2 * t1151 - t1157 + t1163 + t
     #1166 - t1171 + t1175 + t1180 + t1182 - t1189 + 0.48D2 * t1193 - t1
     #196 - t1202 + t1206 + t1210 + t1213
      t1569 = -t1223 - t1228 + t1233 - t1236 + t1241 - 0.30D2 * t1244 - 
     #0.120D3 * t1248 + t1253 + t1256 + t1261 + t1263 + t1267 + t1272 + 
     #t1275 - t1277 + t1280 - t1283 - t1287
      t1573 = t1291 + t1294 + t1297 + 0.40D2 * t1301 - t1306 + 0.48D2 * 
     #t1312 + 0.48D2 * t1319 - t1322 + t1325 - t1332 - t1336 - t1341 + t
     #1349 - t1351 - t1354 + t1360 + t1364 - t1370
      t1579 = t1375 - t1378 - t1381 - t1386 - t1388 - 0.80D2 * t1393 - 0
     #.12D2 * t1397 - 0.120D3 * t1401 + t1408 - t1411 - t1413 - t1416 - 
     #0.48D2 * t1419 - t1427 - t1429 - t1432 + t1435 + t1438
      t1583 = -t1442 - t1448 + t1450 + 0.48D2 * t1452 + t1458 + t1461 - 
     #t1464 + t1467 - t1470 + t1473 + t1477 - t1481 + 0.30D2 * t1483 - t
     #1486 - 0.48D2 * t1490 - t1494 + t1498 - t1501
      rrgg2ggh61J2 = 0.9D1 / 0.16D2 * (0.2D1 * wd * (t207 + t328 + t435 
     #+ t518 + t614 + t703 + t805 + t889 + t974 + t1048 + t1137 + t1214 
     #+ t1288 + t1371 + t1439 + t1502) + wd * (t1513 + t1517 + t1525 + t
     #1528 + t1533 + t1536 + t1543 + t1549 - t974 + t1555 + t1561 + t156
     #4 + t1569 + t1573 + t1579 + t1583)) / t49 / t27 / t26 / z / 0.3141
     #592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh61J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * t1
      t4 = z ** 2
      t5 = t3 * t4
      t6 = 0.1D1 - z
      t7 = s * t6
      t8 = x1 * t6
      t9 = z + t8
      t10 = 0.1D1 / t9
      t11 = x1 * t10
      t12 = 0.1D1 - x3
      t13 = 0.1D1 - x2
      t16 = x2 * x3
      t18 = cos(x4 * 0.3141592653589793D1)
      t19 = x3 * t13
      t23 = Sqrt(t19 * t9 * x2 * t12)
      t25 = 0.2D1 * t18 * t23
      t26 = t12 * t13 * t9 + t16 + t25
      t29 = 0.1D1 - x1
      t30 = t29 * t12
      t32 = s - t7 * t11 * t26 - t7 * t30
      t33 = t6 ** 2
      t34 = t33 * t6
      t35 = t32 * t34
      t37 = x1 ** 2
      t38 = t37 * t10
      t40 = t26 * t29 * x3
      t41 = t38 * t40
      t44 = t33 ** 2
      t45 = t44 * t6
      t46 = t3 * t45
      t47 = t37 * x1
      t48 = t47 * t32
      t49 = t46 * t48
      t50 = t9 ** 2
      t51 = 0.1D1 / t50
      t53 = x2 * t12
      t54 = t19 * t9 + t53 - t25
      t55 = t54 ** 2
      t56 = t51 * t55
      t57 = t29 ** 2
      t58 = x3 ** 2
      t59 = t57 * t58
      t65 = t29 * x3
      t67 = s - t7 * t11 * t54 - t7 * t65
      t68 = t67 * t3
      t69 = t44 * t33
      t70 = t57 * t29
      t71 = t69 * t70
      t74 = z + x1 * t13 * t6
      t75 = t71 * t74
      t76 = t68 * t75
      t77 = t50 ** 2
      t78 = 0.1D1 / t77
      t81 = t26 ** 2
      t82 = t81 * t26
      t87 = t57 ** 2
      t88 = t69 * t87
      t89 = t88 * t74
      t90 = t68 * t89
      t92 = 0.1D1 / t50 / t9
      t99 = t67 * t32
      t100 = t2 * s
      t101 = t100 * t44
      t102 = t99 * t101
      t103 = t12 ** 2
      t104 = t70 * t103
      t105 = x1 * x2
      t106 = t105 * t10
      t110 = t32 * t3
      t111 = t110 * t45
      t112 = x1 * t87
      t113 = t103 * t58
      t117 = t5 * t32
      t118 = t34 * t47
      t119 = t51 * t81
      t120 = t118 * t119
      t124 = t3 * t4 * z
      t126 = t33 * t37
      t127 = t10 * t54
      t128 = t126 * t127
      t131 = t32 * t100
      t132 = t131 * t67
      t134 = t58 * x3
      t135 = t70 * t134
      t137 = t132 * t44 * x1 * t135
      t141 = t3 * z
      t146 = t34 * x1 * t59
      t147 = t117 * t146
      t149 = t132 * t146
      t151 = t37 ** 2
      t152 = t44 * t151
      t153 = t55 * t54
      t154 = t92 * t153
      t155 = t152 * t154
      t158 = t3 * t44
      t160 = t74 * t10
      t161 = t103 * t12
      t163 = t160 * t67 * t161
      t169 = t87 * t29
      t171 = t103 ** 2
      t176 = 0.64D2 * t5 * t35 * t41 - 0.14D2 * t49 * t56 * t59 - 0.32D2
     # * t76 * t78 * t12 * x3 * t47 * t82 + 0.48D2 * t90 * t92 * t12 * t
     #58 * t37 * t81 - 0.48D2 * t102 * t104 * t106 - 0.14D2 * t111 * t11
     #2 * t113 - 0.32D2 * t117 * t120 - 0.128D3 * t124 * t67 * t128 + 0.
     #32D2 * t137 - 0.32D2 * t132 * t120 - 0.32D2 * t141 * t67 * t128 - 
     #0.32D2 * t147 - 0.32D2 * t149 - 0.260D3 * t132 * t155 - 0.6D1 * t1
     #58 * t87 * t163 + 0.176D3 * t132 * t118 * t56 - 0.2D1 * t46 * t169
     # * t160 * t67 * t171
      t177 = t3 * t33
      t179 = t67 * t12
      t180 = t160 * t179
      t182 = 0.2D1 * t177 * t57 * t180
      t183 = t44 * t87
      t184 = t161 * x3
      t187 = 0.6D1 * t132 * t183 * t184
      t190 = 0.6D1 * t111 * t112 * t184
      t191 = t3 * t34
      t193 = t67 * t103
      t194 = t160 * t193
      t196 = 0.6D1 * t191 * t70 * t194
      t197 = t151 * x1
      t198 = t46 * t197
      t199 = t32 * t78
      t200 = t153 * t26
      t203 = 0.8D1 * t198 * t199 * t200
      t204 = t34 * t70
      t205 = t103 * x3
      t208 = 0.16D2 * t132 * t204 * t205
      t209 = t158 * t151
      t210 = t32 * t92
      t211 = t55 * t26
      t214 = 0.16D2 * t209 * t210 * t211
      t217 = 0.8D1 * t132 * t118 * t154
      t219 = t57 * t103
      t220 = t67 * x1
      t222 = t131 * t34 * t219 * t220
      t224 = t55 * t81
      t227 = 0.24D2 * t198 * t199 * t224
      t229 = t70 * t161
      t231 = t131 * t44 * t229 * t220
      t235 = 0.6D1 * t132 * t183 * t113
      t236 = t54 * t81
      t239 = 0.24D2 * t209 * t210 * t236
      t240 = t12 * t134
      t243 = 0.2D1 * t132 * t183 * t240
      t246 = t131 * t33 * t30 * t220
      t248 = t54 * t82
      t251 = 0.32D2 * t198 * t199 * t248
      t252 = t191 * t47
      t253 = t32 * t51
      t254 = t54 * t26
      t257 = 0.8D1 * t252 * t253 * t254
      t259 = 0.38D2 * t132 * t128
      t260 = -t182 - t187 - t190 - t196 + t203 - t208 + t214 + t217 - 0.
     #205D3 * t222 - t227 + 0.7D1 * t231 + t235 - t239 - t243 + 0.48D2 *
     # t246 + t251 + t257 - t259
      t264 = 0.32D2 * t132 * t8 * t127
      t265 = t10 * t26
      t268 = 0.32D2 * t132 * t126 * t265
      t269 = t33 * t57
      t270 = t12 * x3
      t273 = 0.24D2 * t132 * t269 * t270
      t274 = t100 * z
      t275 = t274 * t99
      t276 = t92 * t54
      t280 = 0.32D2 * t275 * t152 * t276 * t81
      t281 = t44 ** 2
      t285 = 0.1D1 / t77 / t9
      t286 = t74 * t285
      t287 = t151 * t54
      t288 = x2 ** 2
      t289 = t288 * x2
      t293 = 0.2D1 * t68 * t281 * t87 * t286 * t287 * t289
      t294 = t44 * t70
      t295 = t131 * t294
      t298 = t295 * t179 * x1 * t58
      t301 = t70 * t12
      t305 = 0.64D2 * t141 * t35 * t301 * t160 * x3
      t306 = t32 * t44
      t308 = t87 * t12
      t312 = 0.32D2 * t141 * t306 * t308 * t160 * t58
      t313 = t69 * t47
      t314 = t110 * t313
      t315 = t51 * t54
      t319 = 0.32D2 * t314 * t315 * t135 * t26
      t321 = t110 * t69 * t151
      t325 = 0.48D2 * t321 * t276 * t59 * t81
      t326 = t34 * t57
      t327 = t131 * t326
      t328 = t74 * t51
      t331 = t328 * t67 * x2 * x1
      t333 = 0.32D2 * t327 * t331
      t334 = t45 * t70
      t335 = t131 * t334
      t336 = t74 * t92
      t339 = t336 * t67 * t288 * t37
      t341 = 0.64D2 * t335 * t339
      t342 = t67 * t34
      t343 = t131 * t342
      t344 = t47 * z
      t347 = 0.32D2 * t343 * t344 * t119
      t348 = t294 * t74
      t349 = t131 * t348
      t350 = t51 * t67
      t351 = x1 * t26
      t352 = t351 * t58
      t355 = 0.32D2 * t349 * t350 * t352
      t356 = t44 * t57
      t357 = t131 * t356
      t358 = t37 * x2
      t359 = t358 * t10
      t361 = t357 * t179 * t359
      t363 = t87 * t74
      t369 = 0.6D1 * t46 * t363 * t350 * t161 * x1 * t26
      t370 = t67 * t45
      t374 = t26 * x2 * t29
      t375 = t276 * t374
      t377 = 0.64D2 * t141 * t370 * t151 * t375
      t378 = t6 * t29
      t380 = t67 * z
      t383 = 0.32D2 * t131 * t378 * t160 * t380
      t384 = t264 + t268 - t273 + t280 - t293 - 0.74D2 * t298 - t305 - t
     #312 - t319 + t325 - t333 + t341 - t347 + t355 + 0.133D3 * t361 - t
     #369 + t377 - t383
      t385 = t32 * t33
      t387 = t57 * t12
      t388 = t387 * t160
      t390 = 0.128D3 * t124 * t385 * t388
      t393 = 0.32D2 * t141 * t385 * t388
      t397 = t81 ** 2
      t401 = 0.16D2 * t110 * t69 * t151 * t37 * t285 * t54 * t397
      t403 = t100 * t4 * t99
      t408 = t403 * t44 * t288 * t37 * t57 * t51
      t409 = 0.32D2 * t408
      t411 = t274 * t99 * t45
      t412 = t151 * t92
      t414 = t55 * x2 * t29
      t417 = 0.8D1 * t411 * t412 * t414
      t418 = t158 * t48
      t421 = 0.12D2 * t418 * t56 * t65
      t422 = t67 * t44
      t423 = t422 * t47
      t424 = t131 * t423
      t425 = t315 * t40
      t427 = 0.16D2 * t424 * t425
      t428 = t44 * t29
      t429 = t131 * t428
      t430 = t74 * t78
      t431 = t67 * t47
      t434 = t429 * t430 * t431 * t82
      t435 = 0.32D2 * t434
      t440 = 0.24D2 * t321 * t387 * t92 * t81 * x2
      t441 = t356 * t74
      t442 = t131 * t441
      t443 = t92 * t67
      t444 = t37 * t55
      t447 = t442 * t443 * t444 * x3
      t449 = t334 * t74
      t450 = t131 * t449
      t454 = t450 * t443 * z * t288 * t37
      t456 = t37 * t54
      t459 = t442 * t443 * t456 * x2
      t461 = t47 * t51
      t464 = t429 * t179 * t461 * t81
      t466 = t443 * t37
      t469 = t442 * t466 * t254 * x3
      t472 = t131 * t294 * t12
      t473 = t328 * t67
      t474 = t351 * x3
      t477 = 0.16D2 * t472 * t473 * t474
      t482 = t131 * t342 * t37 * z * t10 * t40
      t485 = t274 * t99 * t44
      t490 = 0.8D1 * t485 * t461 * t55 * t29 * x3
      t491 = t67 * t100
      t493 = t32 * z
      t497 = 0.16D2 * t491 * t183 * t160 * t493 * t161
      t498 = -t390 - t393 + t401 + t409 + t417 - t421 + t427 + t435 - t4
     #40 + 0.205D3 * t447 + 0.32D2 * t454 + 0.133D3 * t459 + 0.48D2 * t4
     #64 + 0.133D3 * t469 + t477 - 0.64D2 * t482 + t490 + t497
      t501 = t326 * t74
      t505 = t491 * t501 * t253 * t105 * z
      t507 = t37 * t32
      t508 = t46 * t507
      t510 = t508 * t127 * t135
      t511 = 0.42D2 * t510
      t512 = t78 * t47
      t515 = t26 * x3 * x2
      t518 = 0.48D2 * t76 * t512 * t54 * t515
      t519 = t45 * t57
      t520 = t519 * t74
      t521 = t68 * t520
      t524 = t521 * t512 * t236 * x3
      t525 = 0.104D3 * t524
      t526 = t70 * t74
      t530 = t37 * t81 * x3
      t532 = t46 * t526 * t92 * t179 * t530
      t533 = 0.104D3 * t532
      t534 = t44 * t34
      t535 = t534 * t87
      t540 = t131 * t535 * t430 * t67 * t289 * t47
      t541 = 0.32D2 * t540
      t542 = x3 * x1
      t544 = t327 * t179 * t542
      t548 = t38 * t54 * t29 * x3
      t549 = t343 * t548
      t550 = 0.80D2 * t549
      t551 = t38 * t26
      t553 = t357 * t193 * t551
      t556 = t46 * t169 * t74
      t557 = t10 * t67
      t560 = 0.32D2 * t556 * t557 * t240
      t562 = t110 * t45 * t151
      t566 = 0.32D2 * t562 * t30 * t92 * t82
      t569 = t12 * t51
      t572 = 0.48D2 * t110 * t313 * t70 * t569 * t515
      t575 = t12 * t74
      t580 = 0.64D2 * t141 * t35 * t57 * t575 * t51 * x1 * t26
      t583 = t92 * t37
      t587 = 0.32D2 * t141 * t306 * t57 * t575 * t583 * t81
      t590 = t54 * t288
      t591 = t590 * t57
      t592 = t412 * t591
      t594 = 0.8D1 * t274 * t99 * t69 * t592
      t595 = t131 * t204
      t599 = 0.32D2 * t595 * t160 * t380 * t58
      t600 = t461 * t254
      t602 = 0.16D2 * t343 * t600
      t606 = 0.48D2 * t521 * t512 * t211 * x3
      t607 = -0.64D2 * t505 - t511 + t518 + t525 + t533 - t541 - 0.133D3
     # * t544 + t550 + 0.205D3 * t553 + t560 + t566 + t572 + t580 - t587
     # + t594 - t599 - t602 + t606
      t610 = t411 * t412 * t54 * t374
      t613 = t131 * t356 * t12
      t614 = t67 * t37
      t615 = t265 * x3
      t617 = t613 * t614 * t615
      t619 = t100 * t69
      t622 = t12 * t288
      t623 = t37 * t51
      t627 = 0.14D2 * t99 * t619 * t87 * t622 * t623 * x3
      t634 = 0.8D1 * t110 * t534 * t151 * t301 * t92 * t26 * t288
      t636 = t81 * t29 * x3
      t638 = t562 * t276 * t636
      t639 = 0.120D3 * t638
      t640 = t100 * t45
      t642 = t99 * t640 * t87
      t643 = t12 * t58
      t646 = 0.6D1 * t642 * t643 * t106
      t647 = t33 * t29
      t650 = x1 * z
      t653 = t131 * t647 * t74 * t350 * t650 * t26
      t655 = t67 * t69
      t656 = t131 * t655
      t657 = t656 * t592
      t658 = 0.5D1 * t657
      t659 = t131 * t370
      t660 = t47 * t288
      t664 = 0.64D2 * t659 * t660 * t57 * t51
      t665 = t34 * t29
      t666 = t131 * t665
      t668 = t336 * t614 * t81
      t669 = t666 * t668
      t670 = 0.32D2 * t669
      t673 = t131 * t88 * t12 * t339
      t674 = 0.5D1 * t673
      t675 = t141 * t422
      t676 = t412 * t236
      t678 = 0.32D2 * t675 * t676
      t679 = t131 * t501
      t680 = t350 * z
      t682 = t679 * t680 * t474
      t686 = 0.64D2 * t141 * t423 * t425
      t688 = t99 * t640 * t151
      t689 = t78 * t54
      t694 = 0.6D1 * t688 * t689 * t81 * x2 * t29
      t695 = t131 * t183
      t697 = t557 * x3
      t700 = 0.176D3 * t695 * t103 * t74 * t697
      t701 = t141 * t342
      t703 = 0.64D2 * t701 * t600
      t704 = t47 * t92
      t707 = 0.16D2 * t343 * t704 * t211
      t708 = -0.24D2 * t610 + 0.133D3 * t617 - t627 + t634 - t639 - t646
     # + 0.64D2 * t653 - t658 + t664 - t670 - t674 - t678 - 0.64D2 * t68
     #2 + t686 - t694 - t700 - t703 - t707
      t711 = t26 * t288 * t57
      t713 = t656 * t412 * t711
      t714 = 0.32D2 * t713
      t717 = 0.24D2 * t132 * t269 * t103
      t720 = 0.2D1 * t132 * t183 * t171
      t723 = 0.6D1 * t252 * t253 * t55
      t726 = 0.2D1 * t111 * t112 * t171
      t728 = t343 * t387 * t106
      t729 = 0.120D3 * t728
      t732 = 0.8D1 * t132 * t204 * t161
      t733 = t151 * t78
      t736 = 0.6D1 * t102 * t733 * t224
      t739 = 0.6D1 * t209 * t210 * t153
      t741 = t37 * t26
      t742 = t741 * x2
      t744 = t442 * t443 * z * t742
      t745 = 0.64D2 * t744
      t748 = 0.32D2 * t132 * t378 * t12
      t753 = 0.14D2 * t99 * t619 * t151 * t689 * t711
      t757 = 0.38D2 * t695 * t575 * t557 * t58
      t761 = t131 * t71 * t179 * t660 * t51
      t764 = t131 * t67 * t534
      t768 = t764 * t151 * t289 * t70 * t92
      t769 = 0.32D2 * t768
      t770 = t69 * t57
      t772 = t68 * t770 * t74
      t773 = t285 * t151
      t777 = 0.14D2 * t772 * t773 * t236 * x2
      t778 = t45 * t87
      t780 = x1 * t54
      t784 = 0.32D2 * t68 * t778 * t328 * t780 * t134
      t786 = t32 * t10
      t789 = 0.2D1 * t177 * t37 * t786 * t54
      t790 = t714 + t717 + t720 - t723 - t726 - t729 + t732 + t736 - t73
     #9 - t745 + t748 - t753 - t757 - 0.74D2 * t761 - t769 - t777 + t784
     # - t789
      t791 = t37 * z
      t795 = t343 * t791 * x2 * t29 * t10
      t797 = t461 * t54
      t799 = t57 * x3 * x2
      t802 = 0.16D2 * t411 * t797 * t799
      t806 = t68 * t449 * t583 * t254 * t58
      t807 = 0.120D3 * t806
      t809 = t141 * t306 * t70
      t814 = 0.64D2 * t809 * t575 * t51 * x2 * x1
      t815 = t575 * t51
      t818 = 0.64D2 * t809 * t815 * t474
      t820 = t5 * t665 * t668
      t821 = 0.32D2 * t820
      t825 = t58 ** 2
      t829 = 0.16D2 * t68 * t69 * t87 * t57 * t160 * t12 * t825
      t832 = 0.32D2 * t132 * t8 * z
      t833 = t131 * t269
      t835 = 0.38D2 * t833 * t180
      t837 = t54 * x2 * t29
      t838 = t461 * t837
      t840 = 0.64D2 * t675 * t838
      t843 = 0.16D2 * t595 * t575 * t697
      t849 = 0.8D1 * t68 * t519 * t430 * t47 * t153 * x3
      t854 = 0.24D2 * t68 * t334 * t336 * t444 * t58
      t857 = 0.24D2 * t49 * t219 * t119
      t858 = t370 * t47
      t860 = t315 * t799
      t862 = 0.64D2 * t141 * t858 * t860
      t864 = t350 * t474
      t866 = 0.64D2 * t5 * t501 * t864
      t868 = 0.16D2 * t275 * t155
      t871 = t38 * t54 * t57 * t58
      t872 = t102 * t871
      t873 = 0.5D1 * t872
      t874 = -0.64D2 * t795 + t802 - t807 + t814 + t818 - t821 + t829 - 
     #t832 - t835 + t840 - t843 + t849 - t854 - t857 - t862 + t866 + t86
     #8 - t873
      t879 = t131 * t428 * t74
      t880 = t78 * t67
      t881 = t47 * t54
      t884 = t879 * t880 * t881 * t81
      t886 = t87 * t161
      t889 = 0.6D1 * t659 * t886 * t106
      t891 = 0.16D2 * t102 * t838
      t893 = t295 * t193 * t542
      t895 = t526 * t51
      t899 = 0.48D2 * t158 * t895 * t179 * t474
      t905 = 0.8D1 * t68 * t535 * t74 * t512 * t590 * x3
      t907 = t110 * t69 * t37
      t912 = 0.14D2 * t907 * t308 * t58 * x2 * t10
      t913 = t87 * t103
      t914 = t16 * t10
      t917 = 0.12D2 * t907 * t913 * t914
      t918 = t16 * x1
      t920 = t349 * t680 * t918
      t926 = t491 * t44 * t895 * t493 * t12 * t474
      t929 = 0.8D1 * t485 * t871
      t932 = 0.2D1 * t102 * t733 * t248
      t933 = t57 * t74
      t938 = t46 * t933 * t880 * t12 * t47 * t82
      t939 = 0.42D2 * t938
      t945 = 0.14D2 * t46 * t526 * t443 * t103 * t37 * t81
      t947 = t46 * t363 * t51
      t950 = 0.48D2 * t947 * t193 * t474
      t952 = t210 * z
      t954 = t12 * t37 * t81
      t957 = 0.8D1 * t491 * t441 * t952 * t954
      t959 = t491 * t183 * t74
      t964 = 0.32D2 * t959 * t786 * z * t12 * t58
      t965 = t47 * t55
      t968 = t879 * t880 * t965 * t26
      t970 = -0.74D2 * t884 - t889 + t891 - 0.67D2 * t893 + t899 + t905 
     #- t912 - t917 + 0.64D2 * t920 - 0.24D2 * t926 + t929 - t932 - t939
     # - t945 + t950 + t957 + t964 - 0.67D2 * t968
      t973 = t429 * t430 * t431 * t153
      t977 = t103 * x1 * t26
      t980 = 0.12D2 * t158 * t526 * t350 * t977
      t988 = 0.32D2 * t68 * t69 * t169 * t74 * t569 * t134 * x1 * t26
      t992 = t131 * t647 * t328 * t220 * t54
      t995 = t68 * t45 * t29
      t996 = t151 * t153
      t1000 = 0.6D1 * t995 * t286 * t996 * t26
      t1003 = t349 * t350 * t780 * t58
      t1006 = t288 * t37
      t1009 = t131 * t89 * t443 * t1006 * x3
      t1010 = 0.32D2 * t1009
      t1011 = t1006 * t51
      t1013 = t659 * t301 * t1011
      t1014 = 0.104D3 * t1013
      t1017 = 0.64D2 * t349 * t350 * t918
      t1019 = t26 * t57 * t58
      t1022 = 0.64D2 * t102 * t38 * t1019
      t1025 = 0.24D2 * t132 * t126 * t56
      t1027 = t131 * t665 * t74
      t1030 = t1027 * t443 * t456 * t26
      t1035 = 0.12D2 * t688 * t78 * t55 * t374
      t1037 = 0.64D2 * t343 * t41
      t1040 = 0.64D2 * t442 * t443 * t530
      t1046 = 0.6D1 * t191 * t933 * t350 * t12 * x1 * t26
      t1051 = 0.16D2 * t76 * t512 * t55 * x3 * x2
      t1053 = t131 * t67 * t33
      t1057 = 0.384D3 * t1053 * t38 * t54 * z
      t1058 = 0.7D1 * t973 - t980 - t988 + 0.48D2 * t992 - t1000 + 0.48D
     #2 * t1003 + t1010 + t1014 - t1017 - t1022 + t1025 - 0.133D3 * t103
     #0 + t1035 + t1037 - t1040 - t1046 + t1051 - t1057
      t1070 = t68 * t770
      t1091 = t49 * t315 * t1019
      t1097 = t55 ** 2
      t1103 = t111 * t112 * t240
      t1112 = t158 * t363
      t1118 = t49 * t387 * t119 * x3
      t1124 = 0.32D2 * t403 + 0.384D3 * t343 * t461 * t254 * z + 0.8D1 *
     # t491 * t89 * t952 * t622 * t37 - 0.6D1 * t1070 * t286 * t996 * x2
     # + 0.14D2 * t656 * t913 * t1011 - 0.64D2 * t102 * t461 * t374 - 0.
     #6D1 * t562 * t154 * t65 - 0.2D1 * t110 * t281 * t151 * t308 * t289
     # * t92 + 0.104D3 * t1091 + 0.48D2 * t418 * t425 - 0.38D2 * t102 * 
     #t676 - 0.2D1 * t995 * t286 * t151 * t1097 - 0.42D2 * t1103 - 0.176
     #D3 * t102 * t412 * t211 + 0.8D1 * t191 * t526 * t557 * t270 - 0.24
     #D2 * t1112 * t557 * t643 - 0.120D3 * t1118 - 0.12D2 * t772 * t773 
     #* t211 * x2
      t1125 = t613 * t668
      t1126 = 0.5D1 * t1125
      t1129 = 0.8D1 * t508 * t229 * t265
      t1136 = 0.8D1 * t274 * t370 * t87 * t328 * t32 * t105 * t103
      t1139 = 0.8D1 * t343 * t704 * t236
      t1143 = 0.6D1 * t191 * t507 * t127 * t65
      t1145 = t160 * t67 * t58
      t1147 = 0.32D2 * t595 * t1145
      t1148 = t151 * t55
      t1152 = 0.14D2 * t995 * t286 * t1148 * t81
      t1153 = t131 * t520
      t1157 = t1153 * t880 * t47 * t254 * x2
      t1164 = 0.48D2 * t99 * t101 * t70 * t53 * t11 * x3
      t1167 = 0.12D2 * t642 * t205 * t106
      t1170 = t679 * t350 * t780 * x3
      t1174 = 0.24D2 * t1053 * t623 * t254
      t1176 = t485 * t797 * t40
      t1178 = t92 * t55
      t1182 = 0.16D2 * t275 * t152 * t1178 * t26
      t1188 = 0.64D2 * t403 * t33 * x2 * x1 * t29 * t10
      t1190 = t160 * t380 * x3
      t1192 = 0.64D2 * t833 * t1190
      t1196 = 0.384D3 * t833 * t575 * t557 * t4
      t1198 = 0.260D3 * t695 * t163
      t1199 = -t1126 + t1129 + t1136 + t1139 - t1143 - t1147 - t1152 + 0
     #.148D3 * t1157 + t1164 + t1167 - 0.96D2 * t1170 - t1174 - 0.24D2 *
     # t1176 + t1182 - t1188 + t1192 + t1196 - t1198
      t1203 = t659 * t704 * t591
      t1205 = t688 * t375
      t1208 = t131 * t858 * t860
      t1226 = t110 * t534 * t47
      t1227 = t288 * t51
      t1240 = t508 * t301 * t265 * t58
      t1243 = t51 * t26 * x2
      t1252 = t947 * t179 * t352
      t1256 = t995 * t286 * t287 * t82
      t1261 = t274 * t67
      t1270 = 0.104D3 * t1203 + 0.80D2 * t1205 - 0.74D2 * t1208 + 0.16D2
     # * t907 * t127 * t87 * t825 - 0.24D2 * t556 * t557 * t113 + 0.8D1 
     #* t556 * t557 * t184 + 0.14D2 * t656 * t733 * t55 * t288 * t57 - 0
     #.6D1 * t1226 * t913 * t1227 - 0.6D1 * t907 * t886 * x2 * t10 + 0.4
     #8D2 * t562 * t1178 * t40 + 0.104D3 * t1240 + 0.16D2 * t314 * t104 
     #* t1243 + 0.384D3 * t131 * t204 * t12 * t1190 - 0.120D3 * t1252 - 
     #0.42D2 * t1256 - 0.32D2 * t5 * t204 * t1145 + 0.16D2 * t1261 * t44
     #9 * t210 * x2 * t741 * t12 + 0.64D2 * t701 * t548
      t1272 = 0.32D2 * t675 * t871
      t1276 = 0.32D2 * t833 * t160 * t67 * x3
      t1278 = t1053 * t650 * t65
      t1282 = 0.64D2 * t1053 * t791 * t265
      t1286 = 0.384D3 * t1053 * t38 * t54 * t4
      t1289 = t1027 * t443 * t791 * t81
      t1294 = 0.384D3 * t833 * t575 * t557 * z
      t1296 = t343 * t650 * t59
      t1300 = t424 * z * t51 * t374
      t1306 = t131 * t422 * t37 * z * t57 * t914
      t1307 = 0.64D2 * t1306
      t1312 = 0.16D2 * t1070 * t286 * t12 * t151 * t397
      t1315 = 0.16D2 * t1112 * t557 * t205
      t1319 = 0.14D2 * t158 * t933 * t443 * t954
      t1325 = t1261 * t778 * t74 * t253 * x2 * t542 * t12
      t1328 = 0.48D2 * t424 * t375
      t1333 = t131 * t326 * t12 * t328 * t220 * t26
      t1334 = 0.80D2 * t1333
      t1337 = 0.48D2 * t102 * t704 * t414
      t1340 = 0.6D1 * t102 * t733 * t200
      t1341 = -t1272 + t1276 + 0.64D2 * t1278 + t1282 + t1286 + 0.32D2 *
     # t1289 - t1294 + 0.32D2 * t1296 + 0.64D2 * t1300 - t1307 + t1312 +
     # t1315 - t1319 - 0.24D2 * t1325 + t1328 + t1334 - t1337 - t1340
      t1343 = t534 * t70
      t1349 = 0.6D1 * t68 * t1343 * t74 * t773 * t254 * t288
      t1353 = t764 * t308 * t289 * t47 * t92
      t1354 = 0.42D2 * t1353
      t1359 = 0.6D1 * t659 * t733 * t153 * x2 * t29
      t1361 = t343 * t623 * t837
      t1362 = 0.120D3 * t1361
      t1364 = 0.64D2 * t679 * t864
      t1369 = 0.24D2 * t90 * t583 * t54 * t58 * x2
      t1373 = 0.2D1 * t132 * t152 * t78 * t1097
      t1375 = t131 * t334 * t12
      t1377 = t1375 * t614 * t914
      t1383 = 0.6D1 * t68 * t1343 * t286 * t1148 * t288
      t1386 = 0.32D2 * t102 * t461 * t636
      t1389 = 0.48D2 * t508 * t104 * t615
      t1393 = 0.14D2 * t158 * t507 * t127 * t59
      t1397 = t131 * t519 * t12 * t431 * t1243
      t1402 = t450 * t466 * t54 * x3 * x2
      t1406 = t1375 * t336 * t67 * t742
      t1407 = 0.74D2 * t1406
      t1411 = t131 * t778 * t12 * t473 * t918
      t1412 = 0.80D2 * t1411
      t1414 = t335 * t193 * t359
      t1420 = 0.8D1 * t491 * t348 * t253 * z * t977
      t1421 = -t1349 - t1354 - t1359 - t1362 + t1364 - t1369 + t1373 + 0
     #.148D3 * t1377 - t1383 + t1386 + t1389 - t1393 - 0.133D3 * t1397 -
     # 0.133D3 * t1402 - t1407 + t1412 + 0.67D2 * t1414 + t1420
      t1423 = t666 * t179 * t551
      t1432 = 0.32D2 * t141 * t32 * t69 * t87 * t575 * t92 * t288 * t37
      t1438 = 0.32D2 * t110 * t69 * t197 * t689 * t65 * t82
      t1442 = t659 * t344 * t288 * t57 * t51
      t1445 = 0.16D2 * t472 * t331
      t1449 = 0.32D2 * t343 * t358 * t29 * t10
      t1450 = t32 * t45
      t1456 = 0.64D2 * t141 * t1450 * t70 * t575 * t92 * t742
      t1461 = 0.64D2 * t141 * t1450 * t87 * t815 * t918
      t1465 = 0.6D1 * t1226 * t308 * t1227 * x3
      t1469 = t764 * t733 * t54 * t289 * t70
      t1470 = 0.42D2 * t1469
      t1473 = 0.32D2 * t141 * t655 * t592
      t1478 = 0.16D2 * t959 * t786 * z * t103 * x3
      t1480 = 0.176D3 * t595 * t194
      t1483 = 0.8D1 * t132 * t204 * t643
      t1486 = t666 * t336 * t614 * t55
      t1491 = t131 * t75 * t880 * t881 * t288
      t1495 = t1153 * t880 * t965 * x2
      t1499 = 0.2D1 * t198 * t199 * t1097
      t1500 = -0.96D2 * t1423 - t1432 - t1438 + 0.32D2 * t1442 + t1445 -
     # t1449 - t1456 + t1461 - t1465 - t1470 - t1473 + t1478 + t1480 + t
     #1483 - 0.205D3 * t1486 - 0.74D2 * t1491 + 0.67D2 * t1495 - t1499
      t1511 = t182 + t187 + t190 + t196 - t203 + t208 - t214 - t217 + 0.
     #102D3 * t222 + t227 - 0.52D2 * t231 - t235 + t239 + t243 - 0.120D3
     # * t246 - t251 - t257 + t259
      t1515 = -t264 - t268 + t273 - t280 + t293 + 0.40D2 * t298 + t305 +
     # t312 + t319 - t325 + t333 - t341 + t347 - t355 - 0.30D2 * t361 + 
     #t369 - t377 + t383
      t1522 = t390 + t393 - t401 - t409 - t417 + t421 - t427 - t435 + t4
     #40 - 0.102D3 * t447 - 0.48D2 * t454 - 0.30D2 * t459 - 0.120D3 * t4
     #64 - 0.30D2 * t469 - t477 + 0.48D2 * t482 - t490 - t497
      t1528 = 0.48D2 * t505 + t511 - t518 - t525 - t533 + t541 + 0.30D2 
     #* t544 - t550 - 0.102D3 * t553 - t560 - t566 - t572 - t580 + t587 
     #- t594 + t599 + t602 - t606
      t1533 = 0.48D2 * t610 - 0.30D2 * t617 + t627 - t634 + t639 + t646 
     #- 0.80D2 * t653 + t658 - t664 + t670 + t674 + t678 + 0.48D2 * t682
     # - t686 + t694 + t700 + t703 + t707
      t1537 = -t714 - t717 - t720 + t723 + t726 + t729 - t732 - t736 + t
     #739 + 0.32D2 * t744 - t748 + t753 + t757 + 0.40D2 * t761 + t769 + 
     #t777 - t784 + t789
      t1539 = 0.48D2 * t795 - t802 + t807 - t814 - t818 + t821 - t829 + 
     #t832 + t835 - t840 + t843 - t849 + t854 + t857 + t862 - t866 - t86
     #8 + t873
      t1548 = 0.40D2 * t884 + t889 - t891 - 0.12D2 * t893 - t899 - t905 
     #+ t912 + t917 - 0.80D2 * t920 + 0.48D2 * t926 - t929 + t932 + t939
     # + t945 - t950 - t957 - t964 - 0.12D2 * t968
      t1553 = -0.52D2 * t973 + t980 + t988 - 0.120D3 * t992 + t1000 - 0.
     #120D3 * t1003 - t1010 - t1014 + t1017 + t1022 - t1025 + 0.30D2 * t
     #1030 - t1035 - t1037 + t1040 + t1046 - t1051 + t1057
      t1559 = t1126 - t1129 - t1136 - t1139 + t1143 + t1147 + t1152 - 0.
     #80D2 * t1157 - t1164 - t1167 + 0.240D3 * t1170 + t1174 + 0.48D2 * 
     #t1176 - t1182 + t1188 - t1192 - t1196 + t1198
      t1569 = t1272 - t1276 - 0.80D2 * t1278 - t1282 - t1286 - 0.48D2 * 
     #t1289 + t1294 - 0.48D2 * t1296 - 0.80D2 * t1300 + 0.32D2 * t1306 -
     # t1312 - t1315 + t1319 + 0.48D2 * t1325 - t1328 - t1334 + t1337 + 
     #t1340
      t1575 = t1349 + t1354 + t1359 + t1362 - t1364 + t1369 - t1373 - 0.
     #80D2 * t1377 + t1383 - t1386 - t1389 + t1393 + 0.30D2 * t1397 + 0.
     #30D2 * t1402 + t1407 - t1412 + 0.12D2 * t1414 - t1420
      t1581 = 0.240D3 * t1423 + t1432 + t1438 - 0.48D2 * t1442 - t1445 +
     # t1449 + t1456 - t1461 + t1465 + t1470 + t1473 - t1478 - t1480 - t
     #1483 + 0.102D3 * t1486 + 0.40D2 * t1491 + 0.12D2 * t1495 + t1499
      t1606 = 0.64D2 * t137 - 0.64D2 * t147 - 0.64D2 * t149 - 0.48D2 * t
     #222 + 0.80D2 * t231 - 0.64D2 * t298 + 0.32D2 * t361 + 0.64D2 * t40
     #8 + t427 + 0.64D2 * t434 + 0.48D2 * t447 + 0.96D2 * t454 + 0.32D2 
     #* t459 + 0.32D2 * t469 + t477 + 0.16D2 * t482 + 0.16D2 * t505 - 0.
     #32D2 * t510 + 0.64D2 * t524 + 0.64D2 * t532
      t1626 = -0.64D2 * t540 - 0.64D2 * t544 + 0.16D2 * t549 + 0.48D2 * 
     #t553 + 0.32D2 * t617 - 0.32D2 * t638 + 0.16D2 * t653 + 0.32D2 * t6
     #57 - 0.64D2 * t669 + 0.32D2 * t673 + 0.16D2 * t682 + 0.64D2 * t713
     # - 0.32D2 * t728 - t745 - 0.64D2 * t761 - 0.64D2 * t768 + 0.16D2 *
     # t795 - 0.32D2 * t806 - 0.64D2 * t820 + 0.32D2 * t872
      t1647 = -0.64D2 * t884 + t891 - 0.16D2 * t893 + 0.16D2 * t920 - 0.
     #32D2 * t938 - 0.16D2 * t968 + 0.80D2 * t973 + 0.64D2 * t1009 + 0.6
     #4D2 * t1013 - 0.64D2 * t1030 + 0.64D2 * t1091 - 0.32D2 * t1103 - 0
     #.32D2 * t1118 + 0.32D2 * t1125 + 0.256D3 * t1157 + 0.64D2 * t1203 
     #+ 0.16D2 * t1205 - 0.64D2 * t1208 + 0.64D2 * t1240 - 0.32D2 * t125
     #2
      t1668 = -0.64D2 * t1397 - 0.64D2 * t1402 - 0.64D2 * t1406 + 0.16D2
     # * t1411 + 0.16D2 * t1414 + 0.96D2 * t1442 + t1445 - 0.32D2 * t146
     #9 - 0.48D2 * t1486 - 0.64D2 * t1491 + 0.16D2 * t1495
      rrgg2ggh61J3 = 0.9D1 / 0.16D2 * (0.3D1 * wd * (t176 + t260 + t384 
     #+ t498 + t607 + t708 + t790 + t874 + t970 + t1058 + t1124 + t1199 
     #+ t1270 + t1341 + t1421 + t1500) + 0.2D1 * wd * (-t176 + t1511 + t
     #1515 + t1522 + t1528 + t1533 + t1537 + t1539 + t1548 + t1553 - t11
     #24 + t1559 - t1270 + t1569 + t1575 + t1581) + wd * (t1606 + t1626 
     #+ t1647 - 0.32D2 * t1256 + 0.16D2 * t1278 + 0.96D2 * t1289 + 0.96D
     #2 * t1296 + 0.16D2 * t1300 - t1307 + 0.16D2 * t1333 - 0.32D2 * t13
     #53 - 0.32D2 * t1361 + 0.256D3 * t1377 + t1668)) / t32 / t1 / t67 /
     # z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh61J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t11 = x2 * x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + t11 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t32 = t1 ** 2
      t33 = t32 ** 2
      t34 = t33 * t24
      t35 = t31 * t34
      t38 = z + x1 * t8 * t1
      t39 = t4 ** 2
      t40 = t39 ** 2
      t41 = 0.1D1 / t40
      t42 = t38 * t41
      t44 = x2 * t7
      t45 = t14 * t4 + t44 - t20
      t48 = t24 * x3
      t50 = s - t2 * t6 * t45 - t2 * t48
      t51 = x1 ** 2
      t52 = t51 * x1
      t53 = t50 * t52
      t54 = t21 ** 2
      t55 = t54 * t21
      t58 = t35 * t42 * t53 * t55
      t59 = 0.32D2 * t58
      t60 = t50 * t27
      t61 = t30 * t33
      t62 = t60 * t61
      t63 = t51 ** 2
      t64 = t63 * t41
      t65 = t45 ** 2
      t66 = t65 * t45
      t67 = t66 * t21
      t70 = 0.6D1 * t62 * t64 * t67
      t71 = t50 * t33
      t72 = t71 * t52
      t73 = t31 * t72
      t74 = 0.1D1 / t39
      t75 = t74 * t45
      t77 = t21 * t24 * x3
      t78 = t75 * t77
      t80 = 0.16D2 * t73 * t78
      t81 = t29 * t28
      t82 = t50 * t81
      t83 = t32 * t1
      t84 = t33 * t83
      t85 = t24 ** 2
      t86 = t85 ** 2
      t87 = t84 * t86
      t90 = t41 * t52
      t91 = x2 ** 2
      t92 = t45 * t91
      t96 = 0.8D1 * t82 * t87 * t38 * t90 * t92 * x3
      t97 = t81 * t33
      t98 = t85 * t24
      t99 = t98 * t38
      t100 = t99 * t74
      t102 = t50 * t7
      t103 = x1 * t21
      t104 = t103 * x3
      t107 = 0.48D2 * t97 * t100 * t102 * t104
      t108 = t33 * t1
      t109 = t81 * t108
      t110 = t86 * t38
      t112 = t109 * t110 * t74
      t113 = x3 ** 2
      t114 = t103 * t113
      t116 = t112 * t102 * t114
      t117 = 0.120D3 * t116
      t119 = 0.1D1 / t39 / t4
      t123 = t51 * t54 * x3
      t125 = t109 * t99 * t119 * t102 * t123
      t126 = 0.104D3 * t125
      t127 = t83 * t85
      t128 = t127 * t38
      t129 = t31 * t128
      t130 = t74 * t50
      t131 = x1 * t45
      t134 = t129 * t130 * t131 * x3
      t136 = t108 * t85
      t142 = 0.8D1 * t82 * t136 * t42 * t52 * t66 * x3
      t143 = t108 * t98
      t145 = t38 * t119
      t146 = t51 * t65
      t150 = 0.24D2 * t82 * t143 * t145 * t146 * t113
      t151 = t81 * t83
      t153 = t5 * t50
      t154 = t7 * x3
      t157 = 0.8D1 * t151 * t99 * t153 * t154
      t158 = t83 * t98
      t159 = t31 * t158
      t160 = t7 ** 2
      t161 = t160 * t38
      t162 = t161 * t153
      t164 = 0.176D3 * t159 * t162
      t165 = t27 * t81
      t167 = t165 * t84 * t52
      t168 = t86 * t7
      t169 = t91 * t74
      t173 = 0.6D1 * t167 * t168 * t169 * x3
      t174 = t33 * t32
      t175 = t174 * t86
      t180 = t145 * t50 * t91 * t51
      t181 = t31 * t175 * t7 * t180
      t182 = 0.5D1 * t181
      t183 = t81 * z
      t186 = 0.64D2 * t183 * t72 * t78
      t187 = z ** 2
      t188 = t81 * t187
      t189 = t27 * t83
      t191 = t51 * t5
      t192 = t191 * t77
      t194 = 0.64D2 * t188 * t189 * t192
      t195 = t30 * t174
      t198 = t41 * t45
      t200 = t21 * t91 * t85
      t203 = 0.14D2 * t60 * t195 * t63 * t198 * t200
      t204 = t59 - t70 + t80 + t96 + t107 - t117 + t126 - 0.96D2 * t134 
     #+ t142 - t150 + t157 + t164 - t173 - t182 + t186 + t194 - t203
      t205 = t33 * t86
      t206 = t31 * t205
      t207 = t7 * t38
      t211 = 0.38D2 * t206 * t207 * t153 * t113
      t212 = t52 * t74
      t215 = t35 * t102 * t212 * t54
      t217 = t165 * t108
      t218 = x1 * t86
      t219 = t160 ** 2
      t222 = 0.2D1 * t217 * t218 * t219
      t223 = t151 * t52
      t224 = t27 * t74
      t227 = 0.6D1 * t223 * t224 * t65
      t228 = t60 * t30
      t231 = 0.2D1 * t228 * t205 * t219
      t232 = t32 * t85
      t235 = 0.24D2 * t228 * t232 * t160
      t236 = t97 * t63
      t237 = t27 * t119
      t240 = 0.6D1 * t236 * t237 * t66
      t241 = t160 * t7
      t244 = 0.8D1 * t228 * t158 * t241
      t245 = t1 * t24
      t248 = 0.32D2 * t228 * t245 * t7
      t249 = t81 * t32
      t251 = t27 * t5
      t254 = 0.2D1 * t249 * t51 * t251 * t45
      t257 = 0.32D2 * t228 * t3 * z
      t258 = t63 * x1
      t259 = t109 * t258
      t260 = t27 * t41
      t261 = t65 ** 2
      t264 = 0.2D1 * t259 * t260 * t261
      t265 = t50 * t30
      t266 = t33 * t85
      t267 = t266 * t38
      t269 = t237 * z
      t271 = t7 * t51 * t54
      t274 = 0.8D1 * t265 * t267 * t269 * t271
      t275 = t51 * t27
      t277 = t5 * t45
      t278 = t85 * t113
      t281 = 0.14D2 * t97 * t275 * t277 * t278
      t282 = t33 * t98
      t284 = t31 * t282 * t7
      t285 = t38 * t74
      t288 = t285 * t50 * x2 * x1
      t290 = 0.16D2 * t284 * t288
      t291 = t50 * t174
      t292 = t31 * t291
      t293 = t63 * t119
      t294 = t92 * t85
      t295 = t293 * t294
      t296 = t292 * t295
      t297 = 0.5D1 * t296
      t298 = t50 * t108
      t299 = t31 * t298
      t300 = t52 * z
      t304 = t299 * t300 * t91 * t85 * t74
      t306 = t30 * z
      t307 = t306 * t50
      t308 = t143 * t38
      t311 = t51 * t21
      t315 = 0.16D2 * t307 * t308 * t237 * x2 * t311 * t7
      t316 = -t211 + 0.48D2 * t215 - t222 - t227 + t231 + t235 - t240 + 
     #t244 + t248 - t254 - t257 - t264 + t274 - t281 + t290 - t297 + 0.3
     #2D2 * t304 + t315
      t318 = t50 * t83
      t319 = t183 * t318
      t322 = t191 * t45 * t24 * x3
      t324 = 0.64D2 * t319 * t322
      t325 = t183 * t71
      t328 = t191 * t45 * t85 * t113
      t330 = 0.32D2 * t325 * t328
      t332 = t38 * t5
      t333 = t50 * z
      t336 = 0.32D2 * t31 * t245 * t332 * t333
      t338 = t81 * t187 * z
      t339 = t27 * t32
      t341 = t85 * t7
      t342 = t341 * t332
      t344 = 0.128D3 * t338 * t339 * t342
      t345 = t109 * t275
      t346 = t113 * x3
      t347 = t98 * t346
      t349 = t345 * t277 * t347
      t350 = 0.42D2 * t349
      t352 = t292 * t293 * t200
      t353 = 0.32D2 * t352
      t354 = t174 * t85
      t356 = t82 * t354 * t38
      t358 = 0.1D1 / t40 / t4
      t359 = t358 * t63
      t360 = t65 * t21
      t364 = 0.12D2 * t356 * t359 * t360 * x2
      t365 = t31 * t318
      t366 = t74 * t54
      t369 = 0.32D2 * t365 * t300 * t366
      t370 = t306 * t60
      t371 = t33 * t63
      t372 = t119 * t66
      t373 = t371 * t372
      t375 = 0.16D2 * t370 * t373
      t377 = t306 * t60 * t33
      t378 = t212 * t45
      t380 = t377 * t378 * t77
      t383 = t109 * t63 * t27
      t386 = 0.6D1 * t383 * t372 * t48
      t391 = 0.8D1 * t377 * t212 * t65 * t24 * x3
      t393 = t82 * t108 * t24
      t394 = t38 * t358
      t398 = 0.2D1 * t393 * t394 * t63 * t261
      t399 = t50 * t160
      t402 = 0.48D2 * t112 * t399 * t104
      t403 = t83 * t24
      t404 = t31 * t403
      t405 = t191 * t21
      t407 = t404 * t102 * t405
      t409 = t32 * t24
      t412 = x1 * z
      t415 = t31 * t409 * t38 * t130 * t412 * t21
      t417 = t51 * t74
      t419 = t45 * x2 * t24
      t421 = t365 * t417 * t419
      t422 = 0.120D3 * t421
      t427 = 0.14D2 * t292 * t64 * t65 * t91 * t85
      t428 = t324 - t330 - t336 - t344 - t350 + t353 - t364 - t369 + t37
     #5 - 0.24D2 * t380 - t386 + t391 - t398 + t402 - 0.96D2 * t407 + 0.
     #64D2 * t415 - t422 + t427
      t430 = 0.8D1 * t377 * t328
      t431 = t365 * t322
      t432 = 0.80D2 * t431
      t433 = t85 * t38
      t435 = t119 * t50
      t438 = 0.14D2 * t97 * t433 * t435 * t271
      t442 = x1 * x2
      t446 = 0.8D1 * t306 * t298 * t86 * t285 * t27 * t442 * t160
      t447 = t50 * t51
      t449 = t145 * t447 * t54
      t450 = t404 * t449
      t451 = 0.32D2 * t450
      t453 = t98 * t7
      t457 = 0.64D2 * t183 * t189 * t453 * t332 * x3
      t458 = t45 * t54
      t459 = t293 * t458
      t461 = 0.38D2 * t62 * t459
      t464 = 0.176D3 * t62 * t293 * t360
      t468 = t265 * t128 * t224 * t442 * z
      t470 = t174 * t52
      t471 = t165 * t470
      t475 = 0.32D2 * t471 * t75 * t347 * t21
      t476 = t52 * t27
      t477 = t109 * t476
      t478 = t74 * t65
      t481 = 0.14D2 * t477 * t478 * t278
      t482 = t5 * t21
      t485 = t345 * t453 * t482 * t113
      t486 = 0.104D3 * t485
      t488 = t31 * t266 * t7
      t489 = t488 * t449
      t490 = 0.5D1 * t489
      t491 = t45 * t21
      t495 = 0.384D3 * t365 * t212 * t491 * z
      t496 = t298 * t63
      t497 = t31 * t496
      t498 = t119 * t45
      t500 = t21 * x2 * t24
      t501 = t498 * t500
      t502 = t497 * t501
      t503 = 0.80D2 * t502
      t505 = t21 * t85 * t113
      t507 = t477 * t75 * t505
      t508 = 0.104D3 * t507
      t510 = t91 * x2
      t514 = t31 * t87 * t42 * t50 * t510 * t52
      t515 = 0.32D2 * t514
      t518 = t477 * t341 * t366 * x3
      t519 = 0.120D3 * t518
      t520 = t430 + t432 - t438 + t446 - t451 - t457 - t461 - t464 - 0.6
     #4D2 * t468 - t475 - t481 + t486 - t490 + t495 + t503 + t508 - t515
     # - t519
      t524 = t60 * t30 * t84
      t528 = t524 * t64 * t45 * t510 * t98
      t529 = 0.42D2 * t528
      t530 = t52 * t119
      t532 = t299 * t530 * t294
      t533 = 0.104D3 * t532
      t534 = t31 * t143
      t536 = 0.64D2 * t534 * t180
      t537 = t31 * t127
      t539 = 0.32D2 * t537 * t288
      t542 = 0.8D1 * t365 * t530 * t458
      t544 = t60 * t30 * t32
      t547 = 0.24D2 * t544 * t417 * t491
      t550 = t50 * x1
      t553 = t31 * t127 * t7 * t285 * t550 * t21
      t554 = 0.80D2 * t553
      t555 = t153 * x3
      t558 = 0.176D3 * t206 * t161 * t555
      t560 = t31 * t403 * t38
      t561 = t51 * t45
      t564 = t560 * t435 * t561 * t21
      t566 = t31 * t267
      t569 = t566 * t435 * t146 * x3
      t574 = t524 * t168 * t510 * t52 * t119
      t575 = 0.42D2 * t574
      t576 = t175 * t38
      t577 = t82 * t576
      t578 = t119 * t51
      t583 = 0.24D2 * t577 * t578 * t45 * t113 * x2
      t584 = t31 * t232
      t586 = t332 * t333 * x3
      t588 = 0.64D2 * t584 * t586
      t589 = t212 * t491
      t591 = 0.16D2 * t365 * t589
      t593 = t311 * x2
      t595 = t566 * t435 * z * t593
      t596 = 0.64D2 * t595
      t598 = t27 * z
      t602 = 0.16D2 * t265 * t205 * t332 * t598 * t241
      t607 = t31 * t318 * t51 * z * t5 * t77
      t609 = t97 * t476
      t611 = 0.48D2 * t609 * t78
      t612 = -t529 + t533 + t536 - t539 + t542 - t547 + t554 - t558 - 0.
     #133D3 * t564 + 0.205D3 * t569 - t575 - t583 + t588 - t591 - t596 +
     # t602 - 0.64D2 * t607 + t611
      t613 = t119 * t65
      t616 = 0.48D2 * t383 * t613 * t77
      t618 = t7 * t91
      t622 = 0.8D1 * t265 * t576 * t269 * t618 * t51
      t626 = 0.384D3 * t544 * t191 * t45 * z
      t627 = t174 * t98
      t628 = t627 * t38
      t629 = t82 * t628
      t634 = 0.16D2 * t629 * t90 * t65 * x3 * x2
      t635 = t63 * t66
      t639 = 0.6D1 * t393 * t394 * t635 * t21
      t640 = t63 * t65
      t644 = 0.14D2 * t393 * t394 * t640 * t54
      t646 = t306 * t60 * t108
      t649 = t646 * t293 * t45 * t500
      t651 = t31 * t282
      t652 = x3 * x1
      t654 = t651 * t399 * t652
      t656 = z * t51
      t659 = t560 * t435 * t656 * t54
      t664 = 0.384D3 * t584 * t207 * t153 * z
      t665 = t442 * t5
      t667 = t365 * t341 * t665
      t668 = 0.120D3 * t667
      t669 = t31 * t308
      t673 = t669 * t435 * z * t91 * t51
      t678 = 0.8D1 * t306 * t60 * t174 * t295
      t682 = 0.32D2 * t159 * t332 * t333 * t113
      t686 = t54 ** 2
      t690 = 0.16D2 * t165 * t174 * t63 * t51 * t358 * t45 * t686
      t691 = t435 * t51
      t694 = t566 * t691 * t491 * x3
      t696 = t285 * t50
      t699 = 0.16D2 * t284 * t696 * t104
      t701 = 0.64D2 * t319 * t589
      t702 = t616 + t622 - t626 + t634 - t639 - t644 - 0.24D2 * t649 - 0
     #.67D2 * t654 + 0.32D2 * t659 - t664 - t668 + 0.32D2 * t673 + t678 
     #- t682 + t690 + 0.133D3 * t694 + t699 - t701
      t705 = 0.32D2 * t325 * t459
      t707 = t31 * t143 * t7
      t708 = t11 * t5
      t710 = t707 * t447 * t708
      t712 = t7 * t113
      t715 = 0.8D1 * t228 * t158 * t712
      t716 = t97 * t110
      t719 = 0.24D2 * t716 * t153 * t712
      t722 = t21 * x3 * x2
      t725 = 0.48D2 * t629 * t90 * t45 * t722
      t728 = 0.24D2 * t228 * t232 * t154
      t729 = t86 * t24
      t731 = t109 * t729 * t38
      t732 = t241 * x3
      t735 = 0.8D1 * t731 * t153 * t732
      t736 = t160 * t113
      t739 = 0.24D2 * t731 * t153 * t736
      t745 = 0.14D2 * t60 * t195 * t86 * t618 * t417 * x3
      t748 = t60 * t30 * t108 * t86
      t751 = 0.6D1 * t748 * t712 * t665
      t757 = 0.48D2 * t60 * t61 * t98 * t44 * t6 * x3
      t760 = 0.64D2 * t62 * t212 * t500
      t761 = t160 * x3
      t764 = 0.16D2 * t716 * t153 * t761
      t767 = t7 * t74
      t770 = 0.48D2 * t165 * t470 * t98 * t767 * t722
      t772 = t537 * t102 * t652
      t778 = t31 * t71 * t51 * z * t85 * t708
      t779 = 0.64D2 * t778
      t782 = t332 * t50 * t113
      t784 = 0.32D2 * t188 * t158 * t782
      t789 = 0.6D1 * t497 * t198 * t54 * x2 * t24
      t790 = -t705 + 0.148D3 * t710 + t715 - t719 + t725 - t728 + t735 -
     # t739 - t745 - t751 + t757 - t760 + t764 + t770 - 0.133D3 * t772 -
     # t779 - t784 - t789
      t791 = t51 * x2
      t795 = 0.32D2 * t365 * t791 * t24 * t5
      t799 = t31 * t409 * t285 * t550 * t45
      t803 = t404 * t145 * t447 * t65
      t805 = t32 * t51
      t808 = 0.32D2 * t228 * t805 * t482
      t810 = t54 * t24 * x3
      t812 = t383 * t498 * t810
      t813 = 0.120D3 * t812
      t816 = t35 * t42 * t53 * t66
      t820 = 0.16D2 * t365 * t530 * t360
      t821 = t108 * t86
      t827 = t307 * t821 * t38 * t224 * x2 * t652 * t7
      t829 = t136 * t38
      t830 = t82 * t829
      t833 = t830 * t90 * t458 * x3
      t834 = 0.104D3 * t833
      t838 = t82 * t308 * t578 * t491 * t113
      t839 = 0.120D3 * t838
      t840 = t98 * t160
      t841 = t482 * x3
      t844 = 0.48D2 * t345 * t840 * t841
      t847 = 0.32D2 * t228 * t3 * t277
      t853 = 0.32D2 * t165 * t174 * t258 * t198 * t48 * t55
      t854 = t52 * t91
      t858 = 0.64D2 * t299 * t854 * t85 * t74
      t861 = t651 * t102 * x1 * t113
      t863 = t805 * t277
      t865 = 0.38D2 * t228 * t863
      t867 = t365 * t412 * t278
      t871 = 0.8D1 * t223 * t224 * t491
      t872 = -t795 + 0.48D2 * t799 - 0.205D3 * t803 + t808 - t813 + 0.7D
     #1 * t816 - t820 - 0.24D2 * t827 + t834 - t839 + t844 + t847 - t853
     # + t858 - 0.74D2 * t861 - t865 + 0.32D2 * t867 + t871
      t878 = 0.64D2 * t544 * t656 * t482
      t879 = t45 * t55
      t882 = 0.32D2 * t259 * t260 * t879
      t885 = 0.12D2 * t748 * t761 * t665
      t886 = t282 * t38
      t890 = t160 * x1 * t21
      t893 = 0.8D1 * t265 * t886 * t224 * z * t890
      t896 = 0.24D2 * t236 * t237 * t458
      t897 = t7 * t346
      t900 = 0.2D1 * t228 * t205 * t897
      t903 = t31 * t32 * t25 * t550
      t908 = 0.48D2 * t830 * t90 * t360 * x3
      t911 = 0.6D1 * t228 * t205 * t736
      t913 = t98 * t241
      t915 = t31 * t33 * t913 * t550
      t918 = t41 * t50
      t919 = t52 * t45
      t922 = t31 * t628 * t918 * t919 * t91
      t924 = t31 * t829
      t925 = t52 * t65
      t928 = t924 * t918 * t925 * x2
      t932 = t566 * t435 * t561 * x2
      t934 = t31 * t886
      t937 = t934 * t130 * t131 * t113
      t940 = t265 * t205 * t38
      t945 = 0.32D2 * t940 * t251 * z * t7 * t113
      t946 = t65 * t54
      t949 = 0.24D2 * t259 * t260 * t946
      t953 = t924 * t918 * t52 * t491 * x2
      t955 = t82 * t354
      t960 = 0.16D2 * t955 * t394 * t7 * t63 * t686
      t961 = t878 + t882 + t885 + t893 - t896 - t900 + 0.48D2 * t903 + t
     #908 + t911 + 0.7D1 * t915 - 0.74D2 * t922 + 0.67D2 * t928 + 0.133D
     #3 * t932 + 0.48D2 * t937 + t945 - t949 + 0.148D3 * t953 + t960
      t964 = 0.32D2 * t183 * t339 * t342
      t966 = t165 * t174 * t63
      t971 = 0.24D2 * t966 * t341 * t119 * t54 * x2
      t978 = 0.8D1 * t165 * t84 * t63 * t453 * t119 * t21 * t91
      t980 = 0.48D2 * t73 * t501
      t982 = t188 * t403 * t449
      t983 = 0.32D2 * t982
      t985 = t85 * t160
      t987 = t31 * t83 * t985 * t550
      t991 = 0.24D2 * t228 * t805 * t478
      t994 = 0.16D2 * t236 * t237 * t360
      t995 = t83 * t52
      t998 = 0.8D1 * t228 * t995 * t372
      t1002 = 0.2D1 * t228 * t371 * t41 * t261
      t1005 = 0.6D1 * t217 * t218 * t732
      t1008 = 0.6D1 * t151 * t98 * t162
      t1011 = 0.8D1 * t259 * t260 * t67
      t1013 = t65 * x2 * t24
      t1016 = 0.8D1 * t646 * t293 * t1013
      t1019 = 0.16D2 * t228 * t158 * t761
      t1022 = 0.6D1 * t228 * t205 * t732
      t1026 = 0.12D2 * t497 * t41 * t65 * t500
      t1028 = t332 * t102
      t1030 = 0.2D1 * t249 * t85 * t1028
      t1031 = -t964 - t971 + t978 + t980 - t983 - 0.205D3 * t987 + t991 
     #+ t994 + t998 + t1002 - t1005 - t1008 + t1011 + t1016 - t1019 - t1
     #022 + t1026 - t1030
      t1035 = 0.176D3 * t228 * t995 * t478
      t1040 = 0.2D1 * t109 * t729 * t332 * t50 * t219
      t1043 = t332 * t50 * t241
      t1045 = 0.6D1 * t97 * t86 * t1043
      t1047 = 0.260D3 * t228 * t373
      t1052 = 0.32D2 * t82 * t821 * t285 * t131 * t346
      t1054 = t83 * x1 * t278
      t1055 = t228 * t1054
      t1056 = 0.32D2 * t1055
      t1060 = 0.16D2 * t370 * t371 * t613 * t21
      t1062 = t30 * t187 * t60
      t1068 = 0.64D2 * t1062 * t32 * x2 * x1 * t24 * t5
      t1069 = t188 * t27
      t1070 = t1069 * t1054
      t1071 = 0.32D2 * t1070
      t1074 = 0.32D2 * t183 * t50 * t863
      t1076 = t217 * t218 * t897
      t1077 = 0.42D2 * t1076
      t1080 = t228 * t33 * x1 * t347
      t1081 = 0.32D2 * t1080
      t1082 = t995 * t366
      t1084 = 0.32D2 * t228 * t1082
      t1089 = t265 * t33 * t100 * t598 * t7 * t104
      t1093 = 0.128D3 * t338 * t50 * t863
      t1099 = 0.32D2 * t629 * t41 * t7 * x3 * t52 * t55
      t1103 = 0.48D2 * t966 * t498 * t278 * t54
      t1106 = 0.14D2 * t217 * t218 * t736
      t1107 = t1035 - t1040 - t1045 - t1047 + t1052 - t1056 + t1060 - t1
     #068 - t1071 - t1074 - t1077 + t1081 - t1084 - 0.24D2 * t1089 - t10
     #93 - t1099 + t1103 - t1106
      t1109 = 0.32D2 * t1069 * t1082
      t1117 = 0.32D2 * t82 * t174 * t729 * t38 * t767 * t346 * x1 * t21
      t1120 = 0.12D2 * t609 * t478 * t48
      t1126 = 0.48D2 * t577 * t119 * t7 * t113 * t51 * t54
      t1127 = t130 * z
      t1128 = t11 * x1
      t1130 = t934 * t1127 * t1128
      t1133 = t165 * t174 * t51
      t1134 = t86 * t160
      t1137 = 0.12D2 * t1133 * t1134 * t708
      t1142 = 0.6D1 * t299 * t64 * t66 * x2 * t24
      t1145 = 0.6D1 * t62 * t64 * t946
      t1150 = 0.14D2 * t1133 * t168 * t113 * x2 * t5
      t1154 = 0.32D2 * t370 * t371 * t498 * t54
      t1155 = t298 * t52
      t1158 = t85 * x3 * x2
      t1159 = t75 * t1158
      t1161 = 0.64D2 * t183 * t1155 * t1159
      t1163 = t130 * t104
      t1165 = 0.64D2 * t188 * t128 * t1163
      t1166 = t212 * t419
      t1168 = 0.64D2 * t325 * t1166
      t1171 = 0.32D2 * t183 * t291 * t295
      t1176 = 0.16D2 * t940 * t251 * z * t160 * x3
      t1180 = 0.384D3 * t544 * t191 * t45 * t187
      t1182 = 0.38D2 * t584 * t1028
      t1186 = t365 * t656 * x2 * t24 * t5
      t1188 = -t1109 - t1117 - t1120 + t1126 + 0.64D2 * t1130 - t1137 - 
     #t1142 + t1145 - t1150 + t1154 - t1161 + t1165 + t1168 - t1171 + t1
     #176 + t1180 - t1182 - 0.64D2 * t1186
      t1193 = 0.16D2 * t159 * t207 * t555
      t1195 = t488 * t447 * t841
      t1199 = 0.32D2 * t62 * t212 * t810
      t1203 = 0.32D2 * t383 * t25 * t119 * t55
      t1206 = 0.8D1 * t345 * t913 * t482
      t1207 = t33 ** 2
      t1210 = t63 * t45
      t1214 = 0.2D1 * t82 * t1207 * t86 * t394 * t1210 * t510
      t1217 = t393 * t394 * t1210 * t55
      t1218 = 0.42D2 * t1217
      t1220 = t31 * t1155 * t1159
      t1221 = 0.74D2 * t1220
      t1225 = t74 * t21 * x2
      t1227 = t31 * t136 * t7 * t53 * t1225
      t1229 = t84 * t98
      t1234 = 0.6D1 * t82 * t1229 * t394 * t640 * t91
      t1239 = t1062 * t33 * t91 * t51 * t85 * t74
      t1240 = 0.32D2 * t1239
      t1243 = 0.24D2 * t477 * t985 * t366
      t1246 = 0.64D2 * t566 * t435 * t123
      t1252 = 0.6D1 * t151 * t433 * t130 * t7 * x1 * t21
      t1256 = 0.12D2 * t97 * t99 * t130 * t890
      t1258 = t91 * t51
      t1261 = t31 * t576 * t435 * t1258 * x3
      t1262 = 0.32D2 * t1261
      t1265 = 0.64D2 * t934 * t130 * t1128
      t1267 = t31 * t34 * t38
      t1270 = t1267 * t918 * t919 * t54
      t1272 = -t1193 + 0.133D3 * t1195 + t1199 + t1203 + t1206 - t1214 -
     # t1218 - t1221 - 0.133D3 * t1227 - t1234 + t1240 - t1243 - t1246 -
     # t1252 - t1256 + t1262 - t1265 - 0.74D2 * t1270
      t1275 = t1267 * t918 * t925 * t21
      t1283 = 0.64D2 * t183 * t189 * t85 * t207 * t74 * x1 * t21
      t1284 = t27 * t33
      t1290 = 0.32D2 * t183 * t1284 * t85 * t207 * t578 * t54
      t1292 = 0.260D3 * t206 * t1043
      t1293 = t31 * t266
      t1295 = t1293 * t399 * t405
      t1299 = 0.32D2 * t731 * t153 * t897
      t1303 = 0.6D1 * t151 * t275 * t277 * t48
      t1304 = t86 * t241
      t1307 = 0.6D1 * t299 * t1304 * t665
      t1308 = t1258 * t74
      t1311 = 0.14D2 * t292 * t1134 * t1308
      t1314 = 0.2D1 * t62 * t64 * t879
      t1316 = 0.16D2 * t62 * t1166
      t1318 = t129 * t1127 * t104
      t1322 = t707 * t145 * t50 * t593
      t1323 = 0.74D2 * t1322
      t1327 = t31 * t821 * t7 * t696 * t1128
      t1328 = 0.80D2 * t1327
      t1329 = t791 * t5
      t1331 = t534 * t399 * t1329
      t1334 = 0.32D2 * t159 * t782
      t1337 = 0.48D2 * t62 * t530 * t1013
      t1339 = 0.64D2 * t129 * t1163
      t1340 = -0.67D2 * t1275 + t1283 - t1290 - t1292 + 0.205D3 * t1295 
     #+ t1299 - t1303 - t1307 + t1311 - t1314 + t1316 - 0.64D2 * t1318 -
     # t1323 + t1328 + 0.67D2 * t1331 - t1334 - t1337 + t1339
      t1347 = 0.6D1 * t82 * t1229 * t38 * t359 * t491 * t91
      t1351 = t31 * t627 * t102 * t854 * t74
      t1356 = t524 * t63 * t510 * t98 * t119
      t1357 = 0.32D2 * t1356
      t1360 = 0.64D2 * t183 * t496 * t501
      t1362 = t183 * t1284 * t98
      t1367 = 0.64D2 * t1362 * t207 * t74 * x2 * x1
      t1368 = t207 * t74
      t1371 = 0.64D2 * t1362 * t1368 * t104
      t1375 = t113 ** 2
      t1379 = 0.16D2 * t82 * t174 * t86 * t85 * t332 * t7 * t1375
      t1383 = 0.384D3 * t584 * t207 * t153 * t187
      t1387 = 0.16D2 * t1133 * t277 * t86 * t1375
      t1391 = t669 * t691 * t45 * x3 * x2
      t1395 = 0.16D2 * t471 * t840 * t1225
      t1398 = 0.64D2 * t62 * t191 * t505
      t1400 = 0.64D2 * t365 * t192
      t1401 = t62 * t328
      t1402 = 0.5D1 * t1401
      t1405 = 0.48D2 * t62 * t840 * t665
      t1411 = 0.6D1 * t109 * t110 * t130 * t241 * x1 * t21
      t1414 = 0.32D2 * t934 * t130 * t114
      t1415 = t27 * t108
      t1421 = 0.64D2 * t183 * t1415 * t98 * t207 * t119 * t593
      t1422 = -t1347 - 0.74D2 * t1351 - t1357 + t1360 + t1367 + t1371 + 
     #t1379 + t1383 + t1387 - 0.133D3 * t1391 + t1395 - t1398 + t1400 - 
     #t1402 - t1405 - t1411 + t1414 - t1421
      t1427 = 0.64D2 * t183 * t1415 * t86 * t1368 * t1128
      t1435 = 0.32D2 * t183 * t27 * t174 * t86 * t207 * t119 * t91 * t51
      t1439 = 0.32D2 * t584 * t332 * t50 * x3
      t1442 = 0.16D2 * t646 * t378 * t1158
      t1446 = 0.384D3 * t31 * t158 * t7 * t586
      t1448 = t544 * t412 * t48
      t1451 = t299 * t453 * t1308
      t1452 = 0.104D3 * t1451
      t1457 = t109 * t433 * t918 * t7 * t52 * t55
      t1458 = 0.42D2 * t1457
      t1464 = 0.14D2 * t109 * t99 * t435 * t160 * t51 * t54
      t1467 = t73 * z * t74 * t500
      t1471 = 0.6D1 * t167 * t1134 * t169
      t1475 = 0.6D1 * t1133 * t1304 * x2 * t5
      t1481 = 0.2D1 * t165 * t1207 * t63 * t168 * t510 * t119
      t1482 = 0.32D2 * t1062
      t1486 = 0.14D2 * t356 * t359 * t458 * x2
      t1488 = t1293 * t102 * t1329
      t1494 = 0.32D2 * t183 * t1284 * t168 * t332 * t113
      t1498 = 0.6D1 * t955 * t394 * t635 * x2
      t1499 = t1427 - t1435 + t1439 + t1442 + t1446 + 0.64D2 * t1448 + t
     #1452 - t1458 - t1464 + 0.64D2 * t1467 - t1471 - t1475 - t1481 + t1
     #482 - t1486 + 0.133D3 * t1488 - t1494 - t1498
      t1507 = -t59 + t70 - t80 - t96 - t107 + t117 - t126 + 0.240D3 * t1
     #34 - t142 + t150 - t157 - t164 + t173 + t182 - t186 - t194 + t203
      t1510 = t211 - 0.120D3 * t215 + t222 + t227 - t231 - t235 + t240 -
     # t244 - t248 + t254 + t257 + t264 - t274 + t281 - t290 + t297 - 0.
     #48D2 * t304 - t315
      t1515 = -t324 + t330 + t336 + t344 + t350 - t353 + t364 + t369 - t
     #375 + 0.48D2 * t380 + t386 - t391 + t398 - t402 + 0.240D3 * t407 -
     # 0.80D2 * t415 + t422 - t427
      t1517 = -t430 - t432 + t438 - t446 + t451 + t457 + t461 + t464 + 0
     #.48D2 * t468 + t475 + t481 - t486 + t490 - t495 - t503 - t508 + t5
     #15 + t519
      t1524 = t529 - t533 - t536 + t539 - t542 + t547 - t554 + t558 + 0.
     #30D2 * t564 - 0.102D3 * t569 + t575 + t583 - t588 + t591 + 0.32D2 
     #* t595 - t602 + 0.48D2 * t607 - t611
      t1530 = -t616 - t622 + t626 - t634 + t639 + t644 + 0.48D2 * t649 -
     # 0.12D2 * t654 - 0.48D2 * t659 + t664 + t668 - 0.48D2 * t673 - t67
     #8 + t682 - t690 - 0.30D2 * t694 - t699 + t701
      t1535 = t705 - 0.80D2 * t710 - t715 + t719 - t725 + t728 - t735 + 
     #t739 + t745 + t751 - t757 + t760 - t764 - t770 + 0.30D2 * t772 + 0
     #.32D2 * t778 + t784 + t789
      t1542 = t795 - 0.120D3 * t799 + 0.102D3 * t803 - t808 + t813 - 0.5
     #2D2 * t816 + t820 + 0.48D2 * t827 - t834 + t839 - t844 - t847 + t8
     #53 - t858 + 0.40D2 * t861 + t865 - 0.48D2 * t867 - t871
      t1553 = -t878 - t882 - t885 - t893 + t896 + t900 - 0.120D3 * t903 
     #- t908 - t911 - 0.52D2 * t915 + 0.40D2 * t922 + 0.12D2 * t928 - 0.
     #30D2 * t932 - 0.120D3 * t937 - t945 + t949 - 0.80D2 * t953 - t960
      t1555 = t964 + t971 - t978 - t980 + t983 + 0.102D3 * t987 - t991 -
     # t994 - t998 - t1002 + t1005 + t1008 - t1011 - t1016 + t1019 + t10
     #22 - t1026 + t1030
      t1558 = -t1035 + t1040 + t1045 + t1047 - t1052 + t1056 - t1060 + t
     #1068 + t1071 + t1074 + t1077 - t1081 + t1084 + 0.48D2 * t1089 + t1
     #093 + t1099 - t1103 + t1106
      t1561 = t1109 + t1117 + t1120 - t1126 - 0.80D2 * t1130 + t1137 + t
     #1142 - t1145 + t1150 - t1154 + t1161 - t1165 - t1168 + t1171 - t11
     #76 - t1180 + t1182 + 0.48D2 * t1186
      t1567 = t1193 - 0.30D2 * t1195 - t1199 - t1203 - t1206 + t1214 + t
     #1218 + t1221 + 0.30D2 * t1227 + t1234 - t1240 + t1243 + t1246 + t1
     #252 + t1256 - t1262 + t1265 + 0.40D2 * t1270
      t1572 = -0.12D2 * t1275 - t1283 + t1290 + t1292 - 0.102D3 * t1295 
     #- t1299 + t1303 + t1307 - t1311 + t1314 - t1316 + 0.48D2 * t1318 +
     # t1323 - t1328 + 0.12D2 * t1331 + t1334 + t1337 - t1339
      t1576 = t1347 + 0.40D2 * t1351 + t1357 - t1360 - t1367 - t1371 - t
     #1379 - t1383 - t1387 + 0.30D2 * t1391 - t1395 + t1398 - t1400 + t1
     #402 + t1405 + t1411 - t1414 + t1421
      t1580 = -t1427 + t1435 - t1439 - t1442 - t1446 - 0.80D2 * t1448 - 
     #t1452 + t1458 + t1464 - 0.80D2 * t1467 + t1471 + t1475 + t1481 - t
     #1482 + t1486 - 0.30D2 * t1488 + t1494 + t1498
      t1605 = 0.64D2 * t58 + t80 - 0.32D2 * t116 + 0.64D2 * t125 + 0.32D
     #2 * t181 + t290 + 0.32D2 * t296 + 0.96D2 * t304 - 0.32D2 * t349 + 
     #0.64D2 * t352 + 0.16D2 * t415 - 0.32D2 * t421 + 0.16D2 * t431 - 0.
     #64D2 * t450 + 0.16D2 * t468 + 0.64D2 * t485 + 0.32D2 * t489 + 0.16
     #D2 * t502 + 0.64D2 * t507 - 0.64D2 * t514
      t1623 = -0.32D2 * t518 - 0.32D2 * t528 + 0.64D2 * t532 + 0.16D2 * 
     #t553 - 0.64D2 * t564 + 0.48D2 * t569 - 0.32D2 * t574 - t596 + 0.16
     #D2 * t607 - 0.16D2 * t654 + 0.96D2 * t659 - 0.32D2 * t667 + 0.96D2
     # * t673 + 0.32D2 * t694 + t699 + 0.256D3 * t710 - 0.64D2 * t772 - 
     #t779 - 0.48D2 * t803 - 0.32D2 * t812
      t1645 = 0.80D2 * t816 + 0.64D2 * t833 - 0.32D2 * t838 - 0.64D2 * t
     #861 + 0.96D2 * t867 + 0.80D2 * t915 - 0.64D2 * t922 + 0.16D2 * t92
     #8 + 0.32D2 * t932 + 0.256D3 * t953 - 0.64D2 * t982 - 0.48D2 * t987
     # - 0.64D2 * t1055 - 0.64D2 * t1070 - 0.32D2 * t1076 + 0.64D2 * t10
     #80 + 0.16D2 * t1130 + 0.16D2 * t1186 + 0.32D2 * t1195 - 0.32D2 * t
     #1217
      t1667 = 0.16D2 * t1327 + 0.16D2 * t1331 - 0.64D2 * t1351 - 0.64D2 
     #* t1356 - 0.64D2 * t1391 + 0.32D2 * t1401 + 0.16D2 * t1448 + 0.64D
     #2 * t1451 - 0.32D2 * t1457 + 0.16D2 * t1467 + 0.32D2 * t1488
      rrgg2ggh61J4 = 0.9D1 / 0.16D2 * (0.4D1 * wd * (t204 + t316 + t428 
     #+ t520 + t612 + t702 + t790 + t872 + t961 + t1031 + t1107 + t1188 
     #+ t1272 + t1340 + t1422 + t1499) + 0.3D1 * wd * (t1507 + t1510 + t
     #1515 + t1517 + t1524 + t1530 + t1535 + t1542 + t1553 + t1555 + t15
     #58 + t1561 + t1567 + t1572 + t1576 + t1580) + 0.2D1 * wd * (t1605 
     #+ t1623 + t1645 - 0.64D2 * t1220 - 0.64D2 * t1227 + 0.64D2 * t1239
     # + 0.64D2 * t1261 - 0.64D2 * t1270 - 0.16D2 * t1275 + 0.48D2 * t12
     #95 + t1316 + 0.16D2 * t1318 - 0.64D2 * t1322 + t1667)) / t27 / t28
     # / t50 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh61J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t11 = x2 * x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + t11 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t32 = t1 * t24
      t36 = z + x1 * t8 * t1
      t37 = t36 * t5
      t39 = x2 * t7
      t40 = t14 * t4 + t39 - t20
      t43 = t24 * x3
      t45 = s - t2 * t6 * t40 - t2 * t43
      t46 = t45 * z
      t49 = 0.32D2 * t31 * t32 * t37 * t46
      t50 = t29 * t28
      t51 = z ** 2
      t53 = t50 * t51 * z
      t54 = t1 ** 2
      t55 = t27 * t54
      t57 = t24 ** 2
      t58 = t57 * t7
      t59 = t58 * t37
      t61 = 0.128D3 * t53 * t55 * t59
      t62 = t50 * z
      t65 = 0.32D2 * t62 * t55 * t59
      t66 = t45 * t50
      t67 = t54 ** 2
      t68 = t67 * t54
      t69 = t57 ** 2
      t73 = x3 ** 2
      t74 = t73 ** 2
      t78 = 0.16D2 * t66 * t68 * t69 * t57 * t37 * t7 * t74
      t79 = t27 * t50
      t80 = t67 * t1
      t81 = x1 ** 2
      t83 = t79 * t80 * t81
      t84 = t57 * t24
      t85 = t7 ** 2
      t86 = t85 * t7
      t87 = t84 * t86
      t88 = t5 * t21
      t91 = 0.8D1 * t83 * t87 * t88
      t92 = t81 ** 2
      t94 = t79 * t68 * t92
      t95 = t4 ** 2
      t97 = 0.1D1 / t95 / t4
      t98 = t21 ** 2
      t103 = 0.24D2 * t94 * t58 * t97 * t98 * x2
      t104 = t45 * t27
      t105 = t30 * t68
      t108 = t95 ** 2
      t109 = 0.1D1 / t108
      t110 = t109 * t40
      t111 = x2 ** 2
      t113 = t21 * t111 * t57
      t116 = 0.14D2 * t104 * t105 * t92 * t110 * t113
      t117 = t30 * z
      t119 = t117 * t104 * t80
      t120 = t81 * x1
      t121 = 0.1D1 / t95
      t122 = t120 * t121
      t123 = t122 * t40
      t125 = t57 * x3 * x2
      t128 = 0.16D2 * t119 * t123 * t125
      t129 = t67 * t84
      t130 = t129 * t36
      t131 = t31 * t130
      t132 = t121 * t45
      t133 = x1 * x2
      t134 = t133 * x3
      t137 = 0.64D2 * t131 * t132 * t134
      t138 = t30 * t67
      t140 = t104 * t138 * t120
      t141 = t97 * t40
      t143 = t21 * x2 * t24
      t144 = t141 * t143
      t146 = 0.48D2 * t140 * t144
      t147 = t50 * t80
      t148 = t69 * t36
      t154 = 0.6D1 * t147 * t148 * t132 * t86 * x1 * t21
      t155 = x1 * t21
      t156 = t155 * t73
      t159 = 0.32D2 * t131 * t132 * t156
      t160 = t45 * t67
      t161 = t62 * t160
      t162 = t92 * t97
      t163 = t40 * t98
      t164 = t162 * t163
      t166 = 0.32D2 * t161 * t164
      t167 = t54 * t1
      t168 = t167 * t57
      t169 = t168 * t36
      t170 = t31 * t169
      t171 = t132 * z
      t172 = t155 * x3
      t174 = t170 * t171 * t172
      t176 = t121 * t40
      t178 = t21 * t24 * x3
      t179 = t176 * t178
      t181 = 0.16D2 * t140 * t179
      t182 = t45 * t80
      t183 = t182 * t120
      t185 = t176 * t125
      t187 = 0.64D2 * t62 * t183 * t185
      t188 = t50 * t51
      t190 = t132 * t172
      t192 = 0.64D2 * t188 * t169 * t190
      t193 = -t49 - t61 - t65 + t78 + t91 - t103 - t116 + t128 - t137 + 
     #t146 - t154 + t159 - t166 - 0.64D2 * t174 + t181 - t187 + t192
      t194 = t45 * t167
      t195 = t62 * t194
      t196 = t81 * t5
      t199 = t196 * t40 * t24 * x3
      t201 = 0.64D2 * t195 * t199
      t203 = t104 * t30 * t167
      t204 = t81 * t121
      t206 = t40 * x2 * t24
      t208 = t203 * t204 * t206
      t209 = 0.120D3 * t208
      t210 = t31 * t160
      t213 = 0.64D2 * t210 * t122 * t143
      t214 = t45 * t68
      t215 = t31 * t214
      t216 = t40 * t111
      t217 = t216 * t57
      t218 = t162 * t217
      t219 = t215 * t218
      t220 = 0.5D1 * t219
      t221 = t31 * t168
      t222 = t36 * t121
      t225 = t222 * t45 * x2 * x1
      t227 = 0.32D2 * t221 * t225
      t228 = t30 * t80
      t229 = t104 * t228
      t230 = t84 * t7
      t231 = t111 * t81
      t232 = t231 * t121
      t234 = t229 * t230 * t232
      t235 = 0.104D3 * t234
      t236 = t5 * t40
      t237 = t73 * x3
      t238 = t84 * t237
      t240 = t83 * t236 * t238
      t241 = 0.42D2 * t240
      t242 = t68 * t84
      t243 = t242 * t36
      t245 = t109 * t45
      t246 = t120 * t40
      t249 = t31 * t243 * t245 * t246 * t111
      t251 = t45 * t30
      t253 = t84 * t36
      t254 = t253 * t121
      t256 = t27 * z
      t259 = t251 * t67 * t254 * t256 * t7 * t172
      t262 = t31 * t45 * t54
      t266 = 0.384D3 * t262 * t196 * t40 * t51
      t267 = t40 * t21
      t271 = 0.384D3 * t203 * t122 * t267 * z
      t272 = t120 * z
      t273 = t121 * t98
      t276 = 0.32D2 * t203 * t272 * t273
      t277 = x1 * t40
      t280 = t131 * t132 * t277 * t73
      t282 = t80 * t84
      t283 = t282 * t36
      t284 = t31 * t283
      t285 = t97 * t45
      t286 = t285 * t81
      t290 = t284 * t286 * t40 * x3 * x2
      t293 = t66 * t80 * t24
      t295 = 0.1D1 / t108 / t4
      t296 = t36 * t295
      t297 = t40 ** 2
      t298 = t92 * t297
      t302 = 0.14D2 * t293 * t296 * t298 * t98
      t305 = t83 * t230 * t88 * t73
      t306 = 0.104D3 * t305
      t307 = t68 * t120
      t308 = t79 * t307
      t309 = t84 * t85
      t311 = t121 * t21 * x2
      t314 = 0.16D2 * t308 * t309 * t311
      t315 = t67 * t167
      t317 = t79 * t315 * t120
      t318 = t69 * t85
      t319 = t111 * t121
      t322 = 0.6D1 * t317 * t318 * t319
      t323 = t201 - t209 - t213 - t220 - t227 + t235 - t241 - 0.74D2 * t
     #249 - 0.24D2 * t259 + t266 + t271 - t276 + 0.48D2 * t280 - 0.133D3
     # * t290 - t302 + t306 + t314 - t322
      t325 = t50 * t67
      t328 = t85 * x1 * t21
      t331 = 0.12D2 * t325 * t253 * t132 * t328
      t332 = t69 * t24
      t336 = t121 * t7
      t341 = 0.32D2 * t66 * t68 * t332 * t36 * t336 * t237 * x1 * t21
      t344 = t140 * z * t121 * t143
      t349 = t11 * t5
      t351 = t31 * t160 * t81 * z * t57 * t349
      t352 = 0.64D2 * t351
      t353 = t167 * t84
      t356 = t37 * t45 * t73
      t358 = 0.32D2 * t188 * t353 * t356
      t360 = t325 * t120 * t27
      t361 = t121 * t297
      t364 = 0.12D2 * t360 * t361 * t43
      t365 = t68 * t69
      t366 = t365 * t36
      t367 = t66 * t366
      t373 = 0.48D2 * t367 * t97 * t7 * t73 * t81 * t98
      t375 = t131 * t171 * t134
      t377 = t54 * t57
      t378 = t31 * t377
      t382 = 0.32D2 * t378 * t37 * t45 * x3
      t383 = t79 * t80
      t384 = x1 * t69
      t385 = t85 ** 2
      t388 = 0.2D1 * t383 * t384 * t385
      t389 = t50 * t167
      t390 = t389 * t120
      t391 = t27 * t121
      t394 = 0.6D1 * t390 * t391 * t297
      t395 = t104 * t30
      t396 = t67 * t69
      t399 = 0.2D1 * t395 * t396 * t385
      t402 = 0.24D2 * t395 * t377 * t85
      t403 = t325 * t92
      t404 = t27 * t97
      t405 = t297 * t40
      t408 = 0.6D1 * t403 * t404 * t405
      t411 = 0.8D1 * t395 * t353 * t86
      t414 = 0.32D2 * t395 * t32 * t7
      t415 = t50 * t54
      t417 = t27 * t5
      t420 = 0.2D1 * t415 * t81 * t417 * t40
      t423 = 0.32D2 * t395 * t3 * z
      t424 = -t331 - t341 + 0.64D2 * t344 - t352 - t358 - t364 + t373 + 
     #0.64D2 * t375 + t382 - t388 - t394 + t399 + t402 - t408 + t411 + t
     #414 - t420 - t423
      t425 = t92 * x1
      t426 = t147 * t425
      t427 = t27 * t109
      t428 = t297 ** 2
      t431 = 0.2D1 * t426 * t427 * t428
      t432 = t182 * t92
      t433 = t31 * t432
      t434 = t433 * t144
      t435 = 0.80D2 * t434
      t436 = t31 * t282
      t437 = t36 * t97
      t440 = t437 * t45 * t111 * t81
      t442 = 0.64D2 * t436 * t440
      t443 = t325 * t148
      t444 = t5 * t45
      t445 = t85 * x3
      t448 = 0.16D2 * t443 * t444 * t445
      t449 = t57 * t73
      t453 = 0.48D2 * t94 * t141 * t449 * t98
      t454 = t80 * t69
      t459 = 0.32D2 * t66 * t454 * t222 * t277 * t237
      t460 = t57 * t36
      t466 = 0.6D1 * t389 * t460 * t132 * t7 * x1 * t21
      t467 = t80 * t57
      t468 = t467 * t36
      t469 = t31 * t468
      t473 = t469 * t245 * t120 * t267 * x2
      t475 = t68 * t57
      t476 = t66 * t475
      t478 = t98 ** 2
      t482 = 0.16D2 * t476 * t296 * t7 * t92 * t478
      t489 = 0.16D2 * t79 * t68 * t92 * t81 * t295 * t40 * t478
      t490 = t7 * t73
      t493 = 0.8D1 * t395 * t353 * t490
      t494 = t7 * x3
      t497 = 0.24D2 * t395 * t377 * t494
      t499 = t147 * t92 * t27
      t500 = t97 * t297
      t503 = 0.48D2 * t499 * t500 * t178
      t507 = 0.64D2 * t62 * t160 * t120 * t179
      t508 = t27 * t167
      t510 = t196 * t178
      t512 = 0.64D2 * t188 * t508 * t510
      t513 = t27 * t67
      t515 = t62 * t513 * t84
      t516 = t7 * t36
      t521 = 0.64D2 * t515 * t516 * t121 * x2 * x1
      t522 = t516 * t121
      t525 = 0.64D2 * t515 * t522 * t172
      t526 = t54 * t81
      t529 = 0.32D2 * t395 * t526 * t88
      t530 = -t431 + t435 + t442 + t448 + t453 + t459 - t466 + 0.148D3 *
     # t473 + t482 + t489 + t493 - t497 + t503 + t507 + t512 + t521 + t5
     #25 + t529
      t535 = 0.32D2 * t395 * t3 * t236
      t537 = t30 * t51 * t104
      t543 = 0.64D2 * t537 * t54 * x2 * x1 * t24 * t5
      t550 = 0.8D1 * t79 * t315 * t92 * t230 * t97 * t21 * t111
      t551 = t92 * t405
      t555 = 0.6D1 * t476 * t296 * t551 * x2
      t556 = t526 * t236
      t558 = 0.38D2 * t395 * t556
      t560 = t31 * t183 * t185
      t561 = 0.74D2 * t560
      t562 = t31 * t396
      t566 = 0.38D2 * t562 * t516 * t444 * t73
      t568 = t45 * x1
      t570 = t31 * t54 * t25 * t568
      t572 = t98 * t21
      t573 = t40 * t572
      t576 = 0.32D2 * t426 * t427 * t573
      t579 = 0.8D1 * t390 * t391 * t267
      t582 = t7 * t81 * t98
      t585 = 0.14D2 * t325 * t460 * t285 * t582
      t586 = t88 * x3
      t589 = 0.48D2 * t83 * t309 * t586
      t591 = t444 * x3
      t594 = 0.176D3 * t562 * t85 * t36 * t591
      t597 = 0.24D2 * t403 * t404 * t163
      t598 = t92 * t109
      t603 = 0.6D1 * t229 * t598 * t405 * x2 * t24
      t604 = t31 * t353
      t607 = 0.16D2 * t604 * t516 * t591
      t608 = t67 * t57
      t610 = t31 * t608 * t7
      t611 = t45 * t81
      t613 = t610 * t611 * t586
      t616 = t98 * t24 * x3
      t619 = 0.32D2 * t210 * t122 * t616
      t620 = t535 - t543 + t550 - t555 - t558 - t561 - t566 + 0.48D2 * t
     #570 + t576 + t579 - t585 + t589 - t594 - t597 - t603 - t607 + 0.13
     #3D3 * t613 + t619
      t622 = t104 * t30 * t315
      t623 = t111 * x2
      t627 = t622 * t598 * t40 * t623 * t84
      t628 = 0.42D2 * t627
      t629 = t120 * t97
      t631 = t229 * t629 * t217
      t632 = 0.104D3 * t631
      t634 = t104 * t228 * t69
      t635 = t133 * t5
      t638 = 0.12D2 * t634 * t445 * t635
      t639 = t81 * t27
      t643 = 0.6D1 * t389 * t639 * t236 * t43
      t648 = 0.14D2 * t215 * t598 * t297 * t111 * t57
      t650 = t31 * t282 * t7
      t652 = t81 * t21
      t653 = t652 * x2
      t655 = t650 * t437 * t45 * t653
      t656 = 0.74D2 * t655
      t657 = t608 * t36
      t658 = t31 * t657
      t661 = t658 * t285 * z * t653
      t662 = 0.64D2 * t661
      t665 = 0.24D2 * t395 * t526 * t361
      t669 = t622 * t92 * t623 * t84 * t97
      t670 = 0.32D2 * t669
      t672 = 0.32D2 * t604 * t356
      t674 = t57 * t85
      t676 = t31 * t167 * t674 * t568
      t680 = t119 * t162 * t40 * t143
      t685 = 0.32D2 * t604 * t37 * t46 * t73
      t686 = t167 * t120
      t687 = t97 * t405
      t690 = 0.8D1 * t395 * t686 * t687
      t693 = t196 * t40 * t57 * t73
      t694 = t210 * t693
      t695 = 0.5D1 * t694
      t697 = 0.38D2 * t210 * t164
      t698 = t81 * x2
      t702 = 0.32D2 * t203 * t698 * t24 * t5
      t704 = t31 * t129 * t7
      t706 = 0.16D2 * t704 * t225
      t707 = -t628 + t632 + t638 - t643 + t648 - t656 - t662 + t665 - t6
     #70 - t672 - 0.205D3 * t676 - 0.24D2 * t680 - t685 + t690 - t695 - 
     #t697 - t702 + t706
      t710 = t79 * t80 * t120
      t713 = 0.24D2 * t710 * t674 * t273
      t714 = t67 * t24
      t716 = t31 * t714 * t36
      t717 = t120 * t297
      t720 = t716 * t245 * t717 * t21
      t722 = t167 * t24
      t724 = t31 * t722 * t36
      t725 = t81 * t40
      t728 = t724 * t285 * t725 * t21
      t732 = t45 * t120
      t734 = t31 * t467 * t7 * t732 * t311
      t739 = 0.32D2 * t308 * t176 * t238 * t21
      t741 = 0.48D2 * t360 * t179
      t743 = t37 * t46 * x3
      t745 = 0.64D2 * t378 * t743
      t746 = t54 * t24
      t749 = x1 * z
      t752 = t31 * t746 * t36 * t132 * t749 * t21
      t754 = t297 * t21
      t757 = 0.16D2 * t403 * t404 * t754
      t758 = t117 * t104
      t759 = t67 * t92
      t760 = t759 * t687
      t762 = 0.16D2 * t758 * t760
      t767 = t537 * t67 * t111 * t81 * t57 * t121
      t768 = 0.32D2 * t767
      t774 = 0.32D2 * t79 * t68 * t425 * t110 * t43 * t572
      t778 = t229 * t272 * t111 * t57 * t121
      t780 = t97 * t81
      t785 = 0.24D2 * t367 * t780 * t40 * t73 * x2
      t786 = z * t81
      t789 = t724 * t285 * t786 * t98
      t793 = 0.48D2 * t210 * t309 * t635
      t794 = t85 * t45
      t795 = t698 * t5
      t797 = t436 * t794 * t795
      t801 = t716 * t245 * t246 * t98
      t803 = -t713 - 0.67D2 * t720 - 0.133D3 * t728 - 0.133D3 * t734 - t
     #739 + t741 + t745 + 0.64D2 * t752 + t757 + t762 + t768 - t774 + 0.
     #32D2 * t778 - t785 + 0.32D2 * t789 - t793 + 0.67D2 * t797 - 0.74D2
     # * t801
      t810 = t437 * t611 * t98
      t811 = t188 * t722 * t810
      t813 = t27 * t80
      t821 = t404 * z
      t826 = t251 * t396 * t36
      t837 = t69 * t86
      t847 = t79 * t68 * t81
      t852 = t67 ** 2
      t855 = t69 * t7
      t860 = t86 * x3
      t865 = t37 * t794
      t868 = t405 * t21
      t881 = t45 * t7
      t885 = t315 * t69
      t888 = t109 * t120
      t893 = 0.2D1 * t395 * t759 * t109 * t428 - 0.32D2 * t811 - 0.64D2 
     #* t62 * t813 * t84 * t516 * t97 * t653 + 0.8D1 * t251 * t657 * t82
     #1 * t582 + 0.32D2 * t826 * t417 * z * t7 * t73 + 0.16D2 * t251 * t
     #396 * t37 * t256 * t86 - 0.6D1 * t229 * t837 * t635 - 0.16D2 * t39
     #5 * t353 * t445 + 0.64D2 * t170 * t190 - 0.6D1 * t847 * t837 * x2 
     #* t5 - 0.2D1 * t79 * t852 * t92 * t855 * t623 * t97 - 0.6D1 * t383
     # * t384 * t860 - 0.6D1 * t389 * t84 * t865 + 0.8D1 * t426 * t427 *
     # t868 - 0.6D1 * t395 * t396 * t860 + 0.8D1 * t251 * t130 * t391 * 
     #z * t328 + 0.48D2 * t325 * t254 * t881 * t172 + 0.8D1 * t66 * t885
     # * t36 * t888 * t216 * x3
      t898 = t37 * t881
      t909 = t710 * t58 * t273 * x3
      t917 = t147 * t332 * t36
      t918 = t7 * t237
      t922 = t66 * t243
      t930 = t215 * t162 * t113
      t944 = t92 * t40
      t958 = t21 * x3 * x2
      t967 = t222 * t45
      t969 = t31 * t454 * t7 * t967 * t134
      t972 = t21 * t57 * t73
      t978 = -0.2D1 * t415 * t57 * t898 + 0.176D3 * t604 * t865 - 0.6D1 
     #* t317 * t855 * t319 * x3 - 0.120D3 * t909 - 0.2D1 * t147 * t332 *
     # t37 * t45 * t385 + 0.32D2 * t917 * t444 * t918 - 0.32D2 * t922 * 
     #t109 * t7 * x3 * t120 * t572 + 0.32D2 * t930 - 0.6D1 * t433 * t110
     # * t98 * x2 * t24 + 0.16D2 * t922 * t888 * t297 * x3 * x2 - 0.2D1 
     #* t66 * t852 * t69 * t296 * t944 * t623 - 0.14D2 * t710 * t361 * t
     #449 - 0.6D1 * t499 * t687 * t43 + 0.48D2 * t79 * t307 * t84 * t336
     # * t958 + 0.176D3 * t395 * t686 * t361 + 0.80D2 * t969 - 0.64D2 * 
     #t210 * t196 * t972 - 0.32D2 * t161 * t693
      t981 = 0.64D2 * t62 * t432 * t144
      t984 = t37 * t45 * t86
      t986 = 0.6D1 * t325 * t69 * t984
      t994 = 0.32D2 * t62 * t27 * t68 * t69 * t516 * t97 * t111 * t81
      t999 = 0.32D2 * t62 * t513 * t855 * t37 * t73
      t1001 = t66 * t475 * t36
      t1002 = t295 * t92
      t1006 = 0.12D2 * t1001 * t1002 * t754 * x2
      t1010 = 0.14D2 * t1001 * t1002 * t163 * x2
      t1011 = 0.32D2 * t537
      t1017 = 0.48D2 * t104 * t138 * t84 * t39 * t6 * x3
      t1021 = 0.32D2 * t758 * t759 * t141 * t98
      t1026 = t147 * t460 * t245 * t7 * t120 * t572
      t1027 = 0.42D2 * t1026
      t1030 = 0.14D2 * t215 * t318 * t232
      t1034 = 0.16D2 * t847 * t236 * t69 * t74
      t1037 = 0.64D2 * t262 * t786 * t88
      t1039 = t117 * t104 * t67
      t1041 = t1039 * t123 * t178
      t1046 = 0.384D3 * t378 * t516 * t444 * t51
      t1048 = 0.64D2 * t203 * t510
      t1050 = t167 * x1 * t449
      t1051 = t395 * t1050
      t1052 = 0.32D2 * t1051
      t1054 = 0.260D3 * t395 * t760
      t1055 = t981 - t986 - t994 - t999 - t1006 - t1010 + t1011 + t1017 
     #+ t1021 - t1027 + t1030 + t1034 + t1037 - 0.24D2 * t1041 + t1046 +
     # t1048 - t1052 - t1054
      t1059 = 0.32D2 * t62 * t45 * t556
      t1060 = t188 * t27
      t1061 = t1060 * t1050
      t1062 = 0.32D2 * t1061
      t1063 = t686 * t273
      t1065 = 0.32D2 * t395 * t1063
      t1066 = t610 * t810
      t1067 = 0.5D1 * t1066
      t1068 = t122 * t267
      t1070 = 0.64D2 * t195 * t1068
      t1074 = 0.2D1 * t293 * t296 * t92 * t428
      t1076 = 0.32D2 * t1060 * t1063
      t1079 = 0.128D3 * t53 * t45 * t556
      t1081 = t383 * t384 * t918
      t1082 = 0.42D2 * t1081
      t1085 = t395 * t67 * x1 * t238
      t1086 = 0.32D2 * t1085
      t1087 = t85 * t73
      t1090 = 0.14D2 * t383 * t384 * t1087
      t1094 = t66 * t283 * t780 * t267 * t73
      t1095 = 0.120D3 * t1094
      t1096 = t31 * t714
      t1097 = t36 * t109
      t1100 = t1096 * t1097 * t732 * t572
      t1101 = 0.32D2 * t1100
      t1104 = t293 * t296 * t944 * t572
      t1105 = 0.42D2 * t1104
      t1110 = 0.64D2 * t62 * t508 * t230 * t37 * x3
      t1111 = t117 * t45
      t1115 = x3 * x1
      t1118 = t1111 * t454 * t36 * t391 * x2 * t1115 * t7
      t1125 = 0.16D2 * t1111 * t283 * t404 * x2 * t652 * t7
      t1127 = 0.38D2 * t378 * t898
      t1128 = -t1059 - t1062 - t1065 - t1067 - t1070 - t1074 - t1076 - t
     #1079 - t1082 + t1086 - t1090 - t1095 + t1101 - t1105 - t1110 - 0.2
     #4D2 * t1118 + t1125 - t1127
      t1129 = t122 * t206
      t1131 = 0.64D2 * t161 * t1129
      t1134 = 0.24D2 * t443 * t444 * t490
      t1137 = 0.24D2 * t917 * t444 * t1087
      t1140 = 0.8D1 * t917 * t444 * t860
      t1144 = t81 * t98 * x3
      t1146 = t147 * t253 * t97 * t881 * t1144
      t1147 = 0.104D3 * t1146
      t1149 = t147 * t148 * t121
      t1151 = t1149 * t881 * t156
      t1152 = 0.120D3 * t1151
      t1155 = 0.6D1 * t210 * t598 * t868
      t1156 = t31 * t608
      t1158 = t1156 * t881 * t795
      t1161 = 0.16D2 * t210 * t1129
      t1162 = t31 * t129
      t1165 = t1162 * t881 * x1 * t73
      t1169 = 0.16D2 * t203 * t629 * t754
      t1171 = t203 * t58 * t635
      t1172 = 0.120D3 * t1171
      t1173 = t315 * t84
      t1178 = 0.6D1 * t66 * t1173 * t296 * t298 * t111
      t1182 = 0.14D2 * t325 * t639 * t236 * t449
      t1183 = t66 * t468
      t1186 = t1183 * t888 * t163 * x3
      t1187 = 0.104D3 * t1186
      t1193 = 0.6D1 * t66 * t1173 * t36 * t1002 * t267 * t111
      t1195 = t120 * t111
      t1198 = t31 * t242 * t881 * t1195 * t121
      t1203 = 0.12D2 * t433 * t109 * t297 * t143
      t1204 = t1131 - t1134 - t1137 + t1140 + t1147 - t1152 - t1155 + 0.
     #133D3 * t1158 + t1161 - 0.74D2 * t1165 - t1169 - t1172 - t1178 - t
     #1182 + t1187 - t1193 - 0.74D2 * t1198 + t1203
      t1207 = t31 * t722
      t1208 = t1207 * t810
      t1209 = 0.32D2 * t1208
      t1215 = 0.14D2 * t147 * t253 * t285 * t85 * t81 * t98
      t1216 = t196 * t21
      t1218 = t1156 * t794 * t1216
      t1223 = 0.6D1 * t293 * t296 * t551 * t21
      t1225 = t1162 * t794 * t1115
      t1228 = t1207 * t881 * t1216
      t1236 = 0.64D2 * t62 * t508 * t57 * t516 * t121 * x1 * t21
      t1239 = 0.176D3 * t210 * t162 * t754
      t1241 = 0.16D2 * t203 * t1068
      t1244 = t170 * t132 * t277 * x3
      t1248 = 0.64D2 * t658 * t285 * t1144
      t1252 = 0.384D3 * t378 * t516 * t444 * z
      t1254 = 0.260D3 * t562 * t984
      t1256 = t499 * t141 * t616
      t1257 = 0.120D3 * t1256
      t1260 = t1096 * t1097 * t732 * t405
      t1264 = t7 * t111
      t1268 = 0.14D2 * t104 * t105 * t69 * t1264 * t204 * x3
      t1271 = 0.6D1 * t634 * t490 * t635
      t1274 = 0.16D2 * t704 * t967 * t172
      t1275 = -t1209 - t1215 + 0.205D3 * t1218 - t1223 - 0.67D2 * t1225 
     #- 0.96D2 * t1228 + t1236 - t1239 - t1241 - 0.96D2 * t1244 - t1248 
     #- t1252 - t1254 - t1257 + 0.7D1 * t1260 - t1268 - t1271 + t1274
      t1279 = t284 * t285 * z * t111 * t81
      t1285 = t31 * t168 * t7 * t222 * t568 * t21
      t1286 = 0.80D2 * t1285
      t1290 = 0.32D2 * t499 * t25 * t97 * t572
      t1296 = 0.32D2 * t62 * t513 * t57 * t516 * t780 * t98
      t1298 = 0.8D1 * t1039 * t693
      t1300 = t297 * x2 * t24
      t1303 = 0.48D2 * t210 * t629 * t1300
      t1308 = 0.14D2 * t847 * t855 * t73 * x2 * t5
      t1311 = 0.12D2 * t847 * t318 * t349
      t1313 = t710 * t176 * t972
      t1314 = 0.104D3 * t1313
      t1317 = t1096 * t881 * t122 * t98
      t1321 = t658 * t286 * t267 * x3
      t1325 = 0.48D2 * t1149 * t794 * t172
      t1328 = t469 * t245 * t717 * x2
      t1331 = t650 * t611 * t349
      t1335 = 0.32D2 * t62 * t214 * t218
      t1340 = 0.16D2 * t826 * t417 * z * t85 * x3
      t1343 = 0.8D1 * t203 * t629 * t163
      t1346 = 0.24D2 * t262 * t204 * t267
      t1347 = 0.32D2 * t1279 + t1286 + t1290 - t1296 + t1298 - t1303 - t
     #1308 - t1311 + t1314 + 0.48D2 * t1317 + 0.133D3 * t1321 + t1325 + 
     #0.67D2 * t1328 + 0.148D3 * t1331 - t1335 + t1340 + t1343 - t1346
      t1351 = 0.8D1 * t119 * t162 * t1300
      t1358 = 0.8D1 * t117 * t182 * t69 * t222 * t27 * t133 * t85
      t1362 = 0.384D3 * t262 * t196 * t40 * z
      t1366 = 0.64D2 * t229 * t1195 * t57 * t121
      t1371 = t31 * t885 * t1097 * t45 * t623 * t120
      t1372 = 0.32D2 * t1371
      t1374 = t221 * t881 * t1115
      t1381 = 0.8D1 * t66 * t467 * t1097 * t120 * t405 * x3
      t1383 = t81 * t297
      t1387 = 0.24D2 * t66 * t282 * t437 * t1383 * t73
      t1391 = 0.8D1 * t389 * t253 * t444 * t494
      t1395 = 0.8D1 * t117 * t104 * t68 * t218
      t1399 = t31 * t746 * t222 * t568 * t40
      t1403 = t1207 * t437 * t611 * t297
      t1409 = t31 * t194 * t81 * z * t5 * t178
      t1414 = t203 * t786 * x2 * t24 * t5
      t1418 = t658 * t285 * t1383 * x3
      t1423 = 0.48D2 * t922 * t888 * t40 * t958
      t1424 = t297 * t98
      t1427 = 0.6D1 * t210 * t598 * t1424
      t1430 = 0.24D2 * t426 * t427 * t1424
      t1431 = t1351 + t1358 - t1362 + t1366 - t1372 - 0.133D3 * t1374 + 
     #t1381 - t1387 + t1391 + t1395 + 0.48D2 * t1399 - 0.205D3 * t1403 -
     # 0.64D2 * t1409 - 0.64D2 * t1414 + 0.205D3 * t1418 + t1423 + t1427
     # - t1430
      t1434 = t31 * t365 * t7 * t440
      t1435 = 0.5D1 * t1434
      t1436 = t203 * t199
      t1437 = 0.80D2 * t1436
      t1440 = 0.2D1 * t210 * t598 * t573
      t1443 = t31 * t67 * t87 * t568
      t1447 = 0.6D1 * t395 * t396 * t1087
      t1449 = t203 * t749 * t449
      t1452 = t262 * t749 * t43
      t1457 = 0.384D3 * t31 * t353 * t7 * t743
      t1460 = t658 * t285 * t725 * x2
      t1466 = 0.8D1 * t251 * t366 * t821 * t1264 * t81
      t1471 = 0.8D1 * t1039 * t122 * t297 * t24 * x3
      t1475 = 0.16D2 * t758 * t759 * t500 * t21
      t1480 = 0.64D2 * t62 * t813 * t69 * t522 * t134
      t1484 = 0.48D2 * t1183 * t888 * t754 * x3
      t1488 = t251 * t169 * t391 * t133 * z
      t1493 = t622 * t855 * t623 * t120 * t97
      t1494 = 0.42D2 * t1493
      t1497 = 0.2D1 * t395 * t396 * t918
      t1501 = t31 * t366 * t285 * t231 * x3
      t1502 = 0.32D2 * t1501
      t1503 = -t1435 + t1437 - t1440 + 0.7D1 * t1443 + t1447 + 0.32D2 * 
     #t1449 + 0.64D2 * t1452 + t1457 + 0.133D3 * t1460 + t1466 + t1471 +
     # t1475 + t1480 + t1484 - 0.64D2 * t1488 - t1494 - t1497 + t1502
      t1511 = t49 + t61 + t65 - t78 - t91 + t103 + t116 - t128 + t137 - 
     #t146 + t154 - t159 + t166 + 0.48D2 * t174 - t181 + t187 - t192
      t1516 = -t201 + t209 + t213 + t220 + t227 - t235 + t241 + 0.40D2 *
     # t249 + 0.48D2 * t259 - t266 - t271 + t276 - 0.120D3 * t280 + 0.30
     #D2 * t290 + t302 - t306 - t314 + t322
      t1521 = t331 + t341 - 0.80D2 * t344 + 0.32D2 * t351 + t358 + t364 
     #- t373 - 0.80D2 * t375 - t382 + t388 + t394 - t399 - t402 + t408 -
     # t411 - t414 + t420 + t423
      t1523 = t431 - t435 - t442 - t448 - t453 - t459 + t466 - 0.80D2 * 
     #t473 - t482 - t489 - t493 + t497 - t503 - t507 - t512 - t521 - t52
     #5 - t529
      t1528 = -t535 + t543 - t550 + t555 + t558 + t561 + t566 - 0.120D3 
     #* t570 - t576 - t579 + t585 - t589 + t594 + t597 + t603 + t607 - 0
     #.30D2 * t613 - t619
      t1532 = t628 - t632 - t638 + t643 - t648 + t656 + 0.32D2 * t661 - 
     #t665 + t670 + t672 + 0.102D3 * t676 + 0.48D2 * t680 + t685 - t690 
     #+ t695 + t697 + t702 - t706
      t1542 = t713 - 0.12D2 * t720 + 0.30D2 * t728 + 0.30D2 * t734 + t73
     #9 - t741 - t745 - 0.80D2 * t752 - t757 - t762 - t768 + t774 - 0.48
     #D2 * t778 + t785 - 0.48D2 * t789 + t793 + 0.12D2 * t797 + 0.40D2 *
     # t801
      t1549 = -t981 + t986 + t994 + t999 + t1006 + t1010 - t1011 - t1017
     # - t1021 + t1027 - t1030 - t1034 - t1037 + 0.48D2 * t1041 - t1046 
     #- t1048 + t1052 + t1054
      t1552 = t1059 + t1062 + t1065 + t1067 + t1070 + t1074 + t1076 + t1
     #079 + t1082 - t1086 + t1090 + t1095 - t1101 + t1105 + t1110 + 0.48
     #D2 * t1118 - t1125 + t1127
      t1556 = -t1131 + t1134 + t1137 - t1140 - t1147 + t1152 + t1155 - 0
     #.30D2 * t1158 - t1161 + 0.40D2 * t1165 + t1169 + t1172 + t1178 + t
     #1182 - t1187 + t1193 + 0.40D2 * t1198 - t1203
      t1564 = t1209 + t1215 - 0.102D3 * t1218 + t1223 - 0.12D2 * t1225 +
     # 0.240D3 * t1228 - t1236 + t1239 + t1241 + 0.240D3 * t1244 + t1248
     # + t1252 + t1254 + t1257 - 0.52D2 * t1260 + t1268 + t1271 - t1274
      t1570 = -0.48D2 * t1279 - t1286 - t1290 + t1296 - t1298 + t1303 + 
     #t1308 + t1311 - t1314 - 0.120D3 * t1317 - 0.30D2 * t1321 - t1325 +
     # 0.12D2 * t1328 - 0.80D2 * t1331 + t1335 - t1340 - t1343 + t1346
      t1578 = -t1351 - t1358 + t1362 - t1366 + t1372 + 0.30D2 * t1374 - 
     #t1381 + t1387 - t1391 - t1395 - 0.120D3 * t1399 + 0.102D3 * t1403 
     #+ 0.48D2 * t1409 + 0.48D2 * t1414 - 0.102D3 * t1418 - t1423 - t142
     #7 + t1430
      t1584 = t1435 - t1437 + t1440 - 0.52D2 * t1443 - t1447 - 0.48D2 * 
     #t1449 - 0.80D2 * t1452 - t1457 - 0.30D2 * t1460 - t1466 - t1471 - 
     #t1475 - t1480 - t1484 + 0.48D2 * t1488 + t1494 + t1497 - t1502
      t1608 = 0.16D2 * t174 + t181 - 0.32D2 * t208 + 0.32D2 * t219 + 0.6
     #4D2 * t234 - 0.32D2 * t240 - 0.64D2 * t249 - 0.64D2 * t290 + 0.64D
     #2 * t305 + 0.16D2 * t344 - t352 + 0.16D2 * t375 + 0.16D2 * t434 + 
     #0.256D3 * t473 - 0.64D2 * t560 + 0.32D2 * t613 - 0.32D2 * t627 + 0
     #.64D2 * t631 - 0.64D2 * t655 - t662
      t1628 = -0.64D2 * t669 - 0.48D2 * t676 + 0.32D2 * t694 + t706 - 0.
     #16D2 * t720 - 0.64D2 * t728 - 0.64D2 * t734 + 0.16D2 * t752 + 0.64
     #D2 * t767 + 0.96D2 * t778 + 0.96D2 * t789 + 0.16D2 * t797 - 0.64D2
     # * t801 - 0.64D2 * t811 - 0.32D2 * t909 + 0.64D2 * t930 + 0.16D2 *
     # t969 - 0.32D2 * t1026 - 0.64D2 * t1051 - 0.64D2 * t1061
      t1648 = 0.32D2 * t1066 - 0.32D2 * t1081 + 0.64D2 * t1085 - 0.32D2 
     #* t1094 + 0.64D2 * t1100 - 0.32D2 * t1104 + 0.64D2 * t1146 - 0.32D
     #2 * t1151 + 0.32D2 * t1158 + t1161 - 0.64D2 * t1165 - 0.32D2 * t11
     #71 + 0.64D2 * t1186 - 0.64D2 * t1198 - 0.64D2 * t1208 + 0.48D2 * t
     #1218 - 0.16D2 * t1225 - 0.32D2 * t1256 + 0.80D2 * t1260 + t1274
      t1671 = 0.16D2 * t1414 + 0.48D2 * t1418 + 0.32D2 * t1434 + 0.16D2 
     #* t1436 + 0.80D2 * t1443 + 0.96D2 * t1449 + 0.16D2 * t1452 + 0.32D
     #2 * t1460 + 0.16D2 * t1488 - 0.32D2 * t1493 + 0.64D2 * t1501
      rrgg2ggh61J5 = 0.9D1 / 0.16D2 * (0.5D1 * wd * (t193 + t323 + t424 
     #+ t530 + t620 + t707 + t803 + t893 + t978 + t1055 + t1128 + t1204 
     #+ t1275 + t1347 + t1431 + t1503) + 0.4D1 * wd * (t1511 + t1516 + t
     #1521 + t1523 + t1528 + t1532 + t1542 - t893 - t978 + t1549 + t1552
     # + t1556 + t1564 + t1570 + t1578 + t1584) + 0.3D1 * wd * (t1608 + 
     #t1628 + t1648 + 0.96D2 * t1279 + 0.16D2 * t1285 + 0.64D2 * t1313 +
     # 0.32D2 * t1321 + 0.16D2 * t1328 + 0.256D3 * t1331 - 0.64D2 * t137
     #1 - 0.64D2 * t1374 - 0.48D2 * t1403 + 0.16D2 * t1409 + t1671)) / t
     #27 / t28 / t45 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh61J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t11 = x2 * x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + t11 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t33 = x2 * t7
      t34 = t14 * t4 + t33 - t20
      t37 = t24 * x3
      t39 = s - t2 * t6 * t34 - t2 * t37
      t40 = t31 * t39
      t41 = t1 ** 2
      t42 = x1 ** 2
      t43 = t41 * t42
      t44 = t5 * t21
      t48 = t24 ** 2
      t49 = t41 * t48
      t50 = t7 * x3
      t54 = t41 * t1
      t55 = t48 * t24
      t56 = t54 * t55
      t57 = x3 ** 2
      t58 = t7 * t57
      t62 = t29 * t28
      t63 = t27 * t62
      t64 = t41 ** 2
      t65 = t64 * t41
      t67 = t42 ** 2
      t69 = t4 ** 2
      t70 = t69 ** 2
      t72 = 0.1D1 / t70 / t4
      t74 = t21 ** 2
      t75 = t74 ** 2
      t80 = t5 * t34
      t84 = t43 * t80
      t87 = t62 * t54
      t91 = z + x1 * t8 * t1
      t92 = t91 * t5
      t93 = t7 ** 2
      t94 = t39 * t93
      t95 = t92 * t94
      t98 = t64 * t1
      t99 = t62 * t98
      t100 = t67 * x1
      t101 = t99 * t100
      t102 = 0.1D1 / t70
      t103 = t27 * t102
      t104 = t34 ** 2
      t105 = t104 * t34
      t106 = t105 * t21
      t110 = t93 * x3
      t114 = t64 * t67
      t115 = t104 ** 2
      t120 = t62 * t64
      t121 = t120 * t67
      t123 = 0.1D1 / t69 / t4
      t124 = t27 * t123
      t125 = t104 * t21
      t129 = t42 * x1
      t130 = t54 * t129
      t131 = t123 * t105
      t136 = t48 * t93
      t137 = t39 * x1
      t139 = t31 * t54 * t136 * t137
      t141 = 0.1D1 / t69
      t142 = t141 * t104
      t146 = t104 * t74
      t151 = t93 * t7
      t152 = t55 * t151
      t154 = t31 * t64 * t152 * t137
      t156 = t48 ** 2
      t157 = t64 * t156
      t158 = t93 * t57
      t162 = -0.32D2 * t40 * t43 * t44 + 0.24D2 * t40 * t49 * t50 - 0.8D
     #1 * t40 * t56 * t58 - 0.16D2 * t63 * t65 * t67 * t42 * t72 * t34 *
     # t75 - 0.32D2 * t40 * t3 * t80 + 0.38D2 * t40 * t84 + 0.6D1 * t87 
     #* t55 * t95 - 0.8D1 * t101 * t103 * t106 + 0.16D2 * t40 * t56 * t1
     #10 - 0.2D1 * t40 * t114 * t102 * t115 - 0.16D2 * t121 * t124 * t12
     #5 - 0.8D1 * t40 * t130 * t131 + 0.102D3 * t139 - 0.24D2 * t40 * t4
     #3 * t142 + 0.24D2 * t101 * t103 * t146 - 0.52D2 * t154 - 0.6D1 * t
     #40 * t157 * t158
      t163 = t34 * t74
      t167 = t57 * x3
      t168 = t7 * t167
      t176 = t74 * t21
      t177 = t34 * t176
      t181 = t87 * t129
      t182 = t27 * t141
      t183 = t34 * t21
      t190 = t156 * t24
      t192 = t93 ** 2
      t197 = t62 * t41
      t199 = t39 * t7
      t200 = t92 * t199
      t203 = t151 * x3
      t207 = t63 * t98
      t208 = x1 * t156
      t215 = z ** 2
      t216 = t62 * t215
      t217 = t216 * t27
      t218 = t141 * t74
      t219 = t130 * t218
      t223 = t62 * t215 * z
      t228 = t207 * t208 * t168
      t231 = t55 * t167
      t233 = t40 * t64 * x1 * t231
      t237 = t62 * z
      t242 = t48 * t57
      t243 = t54 * x1 * t242
      t244 = t217 * t243
      t246 = 0.24D2 * t121 * t124 * t163 + 0.2D1 * t40 * t157 * t168 - 0
     #.120D3 * t31 * t41 * t25 * t137 - 0.32D2 * t101 * t103 * t177 - 0.
     #8D1 * t181 * t182 * t183 - 0.176D3 * t40 * t130 * t142 + 0.2D1 * t
     #99 * t190 * t92 * t39 * t192 + 0.2D1 * t197 * t48 * t200 + 0.6D1 *
     # t40 * t157 * t203 + 0.6D1 * t207 * t208 * t203 + 0.14D2 * t207 * 
     #t208 * t158 + 0.32D2 * t217 * t219 + 0.128D3 * t223 * t39 * t84 + 
     #0.42D2 * t228 - 0.32D2 * t233 + 0.32D2 * t40 * t219 + 0.32D2 * t23
     #7 * t39 * t84 + 0.32D2 * t244
      t248 = t40 * t243
      t250 = t114 * t131
      t255 = t92 * t39 * t151
      t258 = t30 * z
      t259 = t27 * t39
      t261 = t258 * t259 * t64
      t262 = t129 * t141
      t263 = t262 * t34
      t265 = t21 * t24 * x3
      t269 = t31 * t49
      t270 = t7 * t91
      t271 = t5 * t39
      t277 = t63 * t98 * t67
      t283 = t63 * t98 * t42
      t288 = t63 * t65 * t67
      t289 = t48 * t7
      t296 = t63 * t98 * t129
      t300 = t48 * t91
      t302 = t102 * t39
      t306 = t99 * t300 * t302 * t7 * t129 * t176
      t308 = t39 * t62
      t309 = t65 * t55
      t310 = t309 * t91
      t311 = t308 * t310
      t312 = t102 * t129
      t318 = t64 ** 2
      t321 = t91 * t72
      t322 = t67 * t34
      t323 = x2 ** 2
      t324 = t323 * x2
      t329 = t64 * t54
      t332 = t55 * t7
      t345 = t259 * t30 * t54
      t346 = t129 * t123
      t351 = t259 * t30 * t41
      t352 = t42 * t141
      t356 = t65 * t156
      t357 = t356 * t91
      t358 = t308 * t357
      t359 = t123 * t42
      t365 = 0.32D2 * t248 + 0.260D3 * t40 * t250 + 0.6D1 * t120 * t156 
     #* t255 + 0.48D2 * t261 * t263 * t265 - 0.384D3 * t269 * t270 * t27
     #1 * t215 - 0.32D2 * t277 * t25 * t123 * t176 - 0.8D1 * t283 * t152
     # * t44 + 0.24D2 * t288 * t289 * t123 * t74 * x2 + 0.24D2 * t296 * 
     #t136 * t218 + 0.42D2 * t306 - 0.16D2 * t311 * t312 * t104 * x3 * x
     #2 + 0.2D1 * t308 * t318 * t156 * t321 * t322 * t324 - 0.8D1 * t63 
     #* t329 * t67 * t332 * t123 * t21 * t323 + 0.32D2 * t40 * t3 * z + 
     #0.2D1 * t101 * t103 * t115 - 0.8D1 * t345 * t346 * t163 + 0.24D2 *
     # t351 * t352 * t183 + 0.24D2 * t358 * t359 * t34 * t57 * x2
      t384 = t1 * t24
      t389 = t27 * t5
      t393 = t98 * t48
      t394 = t393 * t91
      t395 = t31 * t394
      t396 = t129 * t104
      t399 = t395 * t302 * t396 * x2
      t401 = t64 * t48
      t402 = t401 * t91
      t403 = t31 * t402
      t404 = t123 * t39
      t405 = t42 * t34
      t408 = t403 * t404 * t405 * x2
      t412 = t21 * x3 * x2
      t416 = t30 * t98
      t417 = t259 * t416
      t418 = t34 * t323
      t419 = t418 * t48
      t421 = t417 * t346 * t419
      t423 = t30 * t65
      t424 = t259 * t423
      t425 = t67 * t102
      t431 = t98 * t55
      t433 = t31 * t431 * t7
      t434 = t91 * t123
      t436 = t42 * t21
      t437 = t436 * x2
      t439 = t433 * t434 * t39 * t437
      t443 = t7 * t42 * t74
      t447 = t55 * t93
      t448 = t44 * x3
      t452 = t27 * t98
      t455 = t270 * t141
      t456 = t11 * x1
      t468 = 0.2D1 * t207 * t208 * t192 + 0.6D1 * t181 * t182 * t104 - 0
     #.2D1 * t40 * t157 * t192 - 0.24D2 * t40 * t49 * t93 + 0.6D1 * t121
     # * t124 * t105 - 0.8D1 * t40 * t56 * t151 - 0.32D2 * t40 * t384 * 
     #t7 + 0.2D1 * t197 * t42 * t389 * t34 + 0.12D2 * t399 - 0.30D2 * t4
     #08 - 0.48D2 * t311 * t312 * t34 * t412 - 0.104D3 * t421 - 0.14D2 *
     # t424 * t425 * t104 * t323 * t48 + 0.74D2 * t439 + 0.14D2 * t120 *
     # t300 * t404 * t443 - 0.48D2 * t283 * t447 * t448 - 0.64D2 * t237 
     #* t452 * t156 * t455 * t456 + 0.32D2 * t237 * t27 * t65 * t156 * t
     #270 * t123 * t323 * t42
      t471 = t39 * t64
      t472 = t237 * t471
      t474 = t34 * x2 * t24
      t475 = t262 * t474
      t478 = t42 * t5
      t487 = t64 * t55
      t488 = t487 * t91
      t489 = t31 * t488
      t490 = t141 * t39
      t495 = t129 * t34
      t498 = t31 * t310 * t302 * t495 * t323
      t500 = t129 * z
      t504 = t417 * t500 * t323 * t48 * t141
      t508 = t478 * t34 * t24 * x3
      t509 = t345 * t508
      t511 = t30 * t64
      t512 = t259 * t511
      t516 = t39 * t30
      t517 = t54 * t48
      t518 = t517 * t91
      t520 = x1 * x2
      t523 = t516 * t518 * t182 * t520 * z
      t525 = t471 * t129
      t526 = t31 * t525
      t527 = t141 * t34
      t528 = t527 * t265
      t530 = 0.16D2 * t526 * t528
      t531 = t27 * t54
      t539 = t27 * t64
      t547 = t124 * z
      t548 = t7 * t323
      t553 = t39 * t54
      t558 = t31 * t553 * t42 * z * t5 * t265
      t560 = t42 * z
      t564 = t345 * t560 * x2 * t24 * t5
      t569 = t258 * t259
      t573 = t30 * t215 * t259
      t578 = t573 * t64 * t323 * t42 * t48 * t141
      t580 = -0.64D2 * t472 * t475 - 0.384D3 * t351 * t478 * t34 * t215 
     #- 0.32D2 * t269 * t92 * t39 * x3 + 0.64D2 * t489 * t490 * t456 + 0
     #.40D2 * t498 - 0.48D2 * t504 - 0.80D2 * t509 + 0.2D1 * t512 * t425
     # * t177 + 0.48D2 * t523 - t530 - 0.64D2 * t237 * t531 * t48 * t270
     # * t141 * x1 * t21 + 0.32D2 * t237 * t539 * t48 * t270 * t359 * t7
     #4 - 0.8D1 * t516 * t357 * t547 * t548 * t42 + 0.48D2 * t558 + 0.48
     #D2 * t564 + 0.32D2 * t345 * t500 * t218 - 0.16D2 * t569 * t250 - 0
     #.32D2 * t578
      t582 = t21 * t48 * t57
      t586 = t55 * t91
      t593 = t156 * t91
      t595 = t99 * t593 * t141
      t596 = x1 * t21
      t597 = t596 * x3
      t601 = t39 * t98
      t602 = t601 * t67
      t603 = t31 * t602
      t604 = t123 * t34
      t606 = t21 * x2 * t24
      t607 = t604 * t606
      t608 = t603 * t607
      t610 = t601 * t129
      t613 = t48 * x3 * x2
      t614 = t527 * t613
      t615 = t31 * t610 * t614
      t620 = t329 * t156
      t622 = t91 * t102
      t626 = t31 * t620 * t622 * t39 * t324 * t129
      t628 = t65 * t48
      t629 = t308 * t628
      t638 = t99 * t190 * t91
      t646 = t64 * t24
      t647 = t31 * t646
      t662 = t516 * t157 * t91
      t670 = t296 * t289 * t218 * x3
      t674 = t92 * t39 * t57
      t678 = t74 * t24 * x3
      t680 = t277 * t604 * t678
      t682 = 0.64D2 * t512 * t478 * t582 + 0.14D2 * t99 * t586 * t404 * 
     #t93 * t42 * t74 - 0.48D2 * t595 * t94 * t597 - 0.80D2 * t608 + 0.7
     #4D2 * t615 - 0.6D1 * t512 * t425 * t146 + 0.32D2 * t626 - 0.16D2 *
     # t629 * t321 * t7 * t67 * t75 + 0.38D2 * t269 * t200 - 0.32D2 * t6
     #38 * t271 * t168 + 0.384D3 * t351 * t478 * t34 * z - 0.120D3 * t64
     #7 * t199 * t262 * t74 + 0.6D1 * t417 * t425 * t105 * x2 * t24 - 0.
     #8D1 * t516 * t402 * t547 * t443 - 0.32D2 * t662 * t389 * z * t7 * 
     #t57 + 0.120D3 * t670 + 0.32D2 * t216 * t56 * t674 + 0.120D3 * t680
      t685 = t237 * t539 * t55
      t692 = t57 ** 2
      t699 = t478 * t34 * t48 * t57
      t700 = t512 * t699
      t702 = t67 * t123
      t703 = t702 * t163
      t707 = t21 * t323 * t48
      t709 = t424 * t702 * t707
      t717 = t54 * t24
      t719 = t31 * t717 * t91
      t722 = t719 * t404 * t405 * t21
      t732 = t329 * t55
      t735 = t72 * t67
      t740 = t31 * t157
      t742 = t271 * x3
      t746 = t156 * t93
      t747 = t323 * t42
      t748 = t747 * t141
      t753 = t63 * t65 * t42
      t760 = t526 * z * t141 * t606
      t765 = t11 * t5
      t767 = t31 * t471 * t42 * z * t48 * t765
      t770 = t258 * t259 * t98
      t775 = t104 * x2 * t24
      t781 = t91 * t141
      t787 = -0.64D2 * t685 * t455 * t597 - 0.16D2 * t308 * t65 * t156 *
     # t48 * t92 * t7 * t692 + 0.5D1 * t700 + 0.38D2 * t512 * t703 - 0.3
     #2D2 * t709 + 0.14D2 * t259 * t423 * t156 * t548 * t352 * x3 + 0.30
     #D2 * t722 - 0.384D3 * t345 * t262 * t183 * z + 0.384D3 * t269 * t2
     #70 * t271 * z + 0.6D1 * t308 * t732 * t91 * t735 * t183 * t323 + 0
     #.176D3 * t740 * t93 * t91 * t742 - 0.14D2 * t424 * t746 * t748 - 0
     #.16D2 * t753 * t80 * t156 * t692 - 0.80D2 * t760 + 0.32D2 * t767 -
     # 0.16D2 * t770 * t263 * t613 - 0.8D1 * t770 * t702 * t775 - 0.8D1 
     #* t258 * t601 * t156 * t781 * t27 * t520 * t93
      t788 = t308 * t394
      t794 = t27 * z
      t800 = t586 * t141
      t807 = t31 * t487 * t7
      t808 = t781 * t39
      t811 = 0.16D2 * t807 * t808 * t597
      t813 = t42 * t74 * x3
      t819 = t719 * t404 * t560 * t74
      t824 = t93 * x1 * t21
      t836 = t702 * t419
      t839 = t31 * t56
      t840 = t39 * z
      t849 = t478 * t265
      t853 = t308 * t98 * t24
      t858 = t98 * t156
      t862 = t31 * t858 * t7 * t808 * t456
      t864 = t31 * t431
      t865 = t42 * x2
      t866 = t865 * t5
      t868 = t864 * t94 * t866
      t870 = x1 * t34
      t875 = x1 * z
      t877 = t345 * t875 * t242
      t880 = t351 * t875 * t37
      t882 = -0.48D2 * t788 * t312 * t125 * x3 - 0.16D2 * t516 * t157 * 
     #t92 * t794 * t151 + 0.48D2 * t516 * t64 * t800 * t794 * t7 * t597 
     #- t811 + 0.64D2 * t403 * t404 * t813 - 0.48D2 * t819 - 0.8D1 * t51
     #6 * t488 * t182 * z * t824 + 0.6D1 * t99 * t593 * t490 * t151 * x1
     # * t21 - 0.8D1 * t258 * t259 * t65 * t836 + 0.32D2 * t839 * t92 * 
     #t840 * t57 - 0.64D2 * t237 * t525 * t528 - 0.64D2 * t216 * t531 * 
     #t849 + 0.2D1 * t853 * t321 * t67 * t115 - 0.80D2 * t862 + 0.12D2 *
     # t868 - 0.120D3 * t489 * t490 * t870 * t57 - 0.48D2 * t877 - 0.80D
     #2 * t880
      t886 = t156 * t151
      t893 = t156 * t7
      t901 = t296 * t527 * t582
      t903 = t258 * t39
      t907 = x3 * x1
      t912 = t431 * t91
      t921 = t31 * t717
      t922 = t39 * t42
      t924 = t434 * t922 * t74
      t925 = t921 * t924
      t936 = t41 * t24
      t941 = t31 * t936 * t91 * t490 * t875 * t21
      t950 = t921 * t434 * t922 * t104
      t961 = t39 * t129
      t964 = t647 * t622 * t961 * t176
      t967 = t92 * t840 * x3
      t974 = 0.6D1 * t753 * t886 * x2 * t5 + 0.2D1 * t63 * t318 * t67 * 
     #t893 * t324 * t123 - 0.64D2 * t345 * t849 - 0.104D3 * t901 + 0.48D
     #2 * t903 * t858 * t91 * t182 * x2 * t907 * t7 - 0.16D2 * t903 * t9
     #12 * t124 * x2 * t436 * t7 + 0.260D3 * t740 * t255 + 0.32D2 * t925
     # + 0.64D2 * t237 * t610 * t614 + 0.64D2 * t573 * t41 * x2 * x1 * t
     #24 * t5 - 0.80D2 * t941 - 0.120D3 * t31 * t936 * t781 * t137 * t34
     # + 0.102D3 * t950 - 0.8D1 * t261 * t262 * t104 * t24 * x3 - 0.32D2
     # * t569 * t114 * t604 * t74 - 0.32D2 * t964 - 0.64D2 * t269 * t967
     # + 0.48D2 * t770 * t702 * t34 * t606
      t975 = t31 * t401
      t977 = t975 * t199 * t866
      t986 = 0.16D2 * t512 * t475
      t988 = t308 * t628 * t91
      t995 = t102 * t34
      t1003 = t395 * t302 * t129 * t183 * x2
      t1008 = t99 * t586 * t123 * t199 * t813
      t1020 = t434 * t39 * t323 * t42
      t1021 = t31 * t356 * t7 * t1020
      t1029 = t259 * t416 * t156
      t1030 = t520 * t5
      t1034 = t129 * t323
      t1039 = t67 * t105
      t1047 = t424 * t836
      t1050 = t345 * t352 * t474
      t1052 = -0.30D2 * t977 - 0.12D2 * t603 * t102 * t104 * t606 - 0.48
     #D2 * t526 * t607 - t986 + 0.14D2 * t988 * t735 * t163 * x2 + 0.32D
     #2 * t63 * t65 * t100 * t995 * t37 * t176 - 0.80D2 * t1003 - 0.104D
     #3 * t1008 + 0.24D2 * t638 * t271 * t158 - 0.8D1 * t638 * t271 * t2
     #03 + 0.5D1 * t1021 - 0.32D2 * t308 * t858 * t781 * t870 * t167 - 0
     #.12D2 * t1029 * t110 * t1030 - 0.64D2 * t417 * t1034 * t48 * t141 
     #+ 0.6D1 * t629 * t321 * t1039 * x2 + 0.64D2 * t512 * t262 * t606 +
     # 0.5D1 * t1047 + 0.120D3 * t1050
      t1058 = t31 * t518
      t1066 = t237 * t553
      t1074 = t31 * t912
      t1078 = t1074 * t404 * z * t323 * t42
      t1080 = t262 * t183
      t1084 = t259 * t30 * t329
      t1088 = t1084 * t893 * t324 * t129 * t123
      t1091 = t120 * t129 * t27
      t1108 = t31 * t517 * t7 * t781 * t137 * t21
      t1116 = t65 * t129
      t1119 = t7 * t141
      t1131 = -0.48D2 * t288 * t604 * t242 * t74 + 0.240D3 * t1058 * t49
     #0 * t870 * x3 + 0.6D1 * t417 * t886 * t1030 - 0.64D2 * t1066 * t50
     #8 + 0.32D2 * t472 * t699 - 0.64D2 * t237 * t602 * t607 - 0.48D2 * 
     #t1078 + 0.64D2 * t1066 * t1080 + 0.42D2 * t1088 + 0.12D2 * t1091 *
     # t142 * t37 - 0.48D2 * t358 * t123 * t7 * t57 * t42 * t74 + 0.16D2
     # * t345 * t346 * t125 - 0.80D2 * t1108 + 0.14D2 * t296 * t142 * t2
     #42 + 0.6D1 * t277 * t131 * t37 - 0.48D2 * t63 * t1116 * t55 * t111
     #9 * t412 + 0.32D2 * t311 * t102 * t7 * x3 * t129 * t176 - 0.64D2 *
     # t864 * t1020
      t1135 = t141 * t21 * x2
      t1137 = t31 * t393 * t7 * t961 * t1135
      t1140 = t433 * t922 * t765
      t1143 = t31 * t646 * t91
      t1146 = t1143 * t302 * t396 * t21
      t1152 = t67 * t104
      t1181 = t478 * t21
      t1185 = t596 * t57
      t1187 = t595 * t199 * t1185
      t1192 = t63 * t1116
      t1197 = t404 * t42
      t1200 = t403 * t1197 * t183 * x3
      t1203 = t345 * t289 * t1030
      t1207 = t853 * t321 * t322 * t176
      t1214 = 0.30D2 * t1137 - 0.80D2 * t1140 - 0.12D2 * t1146 + 0.6D1 *
     # t853 * t321 * t1039 * t21 + 0.14D2 * t853 * t321 * t1152 * t74 + 
     #0.32D2 * t237 * t39 * t65 * t836 + 0.6D1 * t87 * t300 * t490 * t7 
     #* x1 * t21 + 0.12D2 * t120 * t586 * t490 * t824 + 0.32D2 * t308 * 
     #t65 * t190 * t91 * t1119 * t167 * x1 * t21 - 0.8D1 * t261 * t699 +
     # 0.240D3 * t921 * t199 * t1181 + 0.120D3 * t1187 + 0.6D1 * t512 * 
     #t425 * t106 + 0.32D2 * t1192 * t527 * t231 * t21 - 0.30D2 * t1200 
     #+ 0.120D3 * t1203 + 0.42D2 * t1207 + 0.64D2 * t237 * t531 * t332 *
     # t92 * x3
      t1238 = t27 * t41
      t1240 = t289 * t92
      t1248 = t42 * t27
      t1255 = t788 * t312 * t163 * x3
      t1260 = t1074 * t1197 * t34 * x3 * x2
      t1262 = t31 * t487
      t1264 = t1262 * t94 * t907
      t1267 = t31 * t401 * t7
      t1269 = t1267 * t922 * t448
      t1276 = t647 * t622 * t961 * t105
      t1280 = t975 * t94 * t1181
      t1292 = 0.32D2 * t237 * t539 * t893 * t92 * t57 - 0.32D2 * t512 * 
     #t262 * t678 + 0.6D1 * t1029 * t58 * t1030 - 0.48D2 * t259 * t511 *
     # t55 * t33 * t6 * x3 + 0.32D2 * t31 * t384 * t92 * t840 + 0.128D3 
     #* t223 * t1238 * t1240 + 0.6D1 * t308 * t732 * t321 * t1152 * t323
     # + 0.14D2 * t120 * t1248 * t80 * t242 - 0.104D3 * t1255 + 0.30D2 *
     # t1260 - 0.12D2 * t1264 - 0.30D2 * t1269 + 0.48D2 * t512 * t447 * 
     #t1030 - 0.52D2 * t1276 - 0.32D2 * t573 - 0.102D3 * t1280 - 0.48D2 
     #* t120 * t800 * t199 * t597 - 0.8D1 * t308 * t620 * t91 * t312 * t
     #418 * x3
      t1296 = t490 * t597
      t1305 = t1262 * t199 * x1 * t57
      t1310 = t31 * t357 * t404 * t747 * x3
      t1315 = t63 * t329 * t129
      t1316 = t323 * t141
      t1321 = t120 * t593
      t1327 = t1143 * t302 * t495 * t74
      t1331 = t781 * t39 * x2 * x1
      t1333 = 0.16D2 * t807 * t1331
      t1339 = t490 * z
      t1341 = t1058 * t1339 * t597
      t1345 = t403 * t404 * z * t437
      t1347 = t123 * t104
      t1357 = t216 * t717 * t924
      t1365 = 0.48D2 * t512 * t346 * t775 - 0.64D2 * t1058 * t1296 + 0.3
     #2D2 * t345 * t865 * t24 * t5 + 0.40D2 * t1305 - 0.32D2 * t1310 - 0
     #.176D3 * t839 * t95 + 0.6D1 * t1315 * t893 * t1316 * x3 - 0.16D2 *
     # t1321 * t271 * t110 + 0.40D2 * t1327 - t1333 + 0.32D2 * t237 * t1
     #238 * t1240 + 0.32D2 * t472 * t703 + 0.48D2 * t1341 + 0.32D2 * t13
     #45 - 0.16D2 * t569 * t114 * t1347 * t21 - 0.384D3 * t31 * t56 * t7
     # * t967 + 0.32D2 * t1357 + 0.64D2 * t237 * t452 * t55 * t270 * t12
     #3 * t437
      t1370 = t1084 * t425 * t34 * t324 * t55
      t1375 = t1084 * t67 * t324 * t55 * t123
      t1384 = t31 * t517
      t1388 = t417 * t332 * t748
      t1397 = t42 * t104
      t1407 = t283 * t80 * t231
      t1415 = t1384 * t199 * t907
      t1417 = t1267 * t924
      t1421 = t283 * t332 * t44 * t57
      t1432 = 0.42D2 * t1370 + 0.32D2 * t1375 + 0.32D2 * t839 * t674 - 0
     #.64D2 * t685 * t270 * t141 * x2 * x1 + 0.32D2 * t1384 * t1331 - 0.
     #104D3 * t1388 - 0.8D1 * t308 * t393 * t622 * t129 * t105 * x3 + 0.
     #24D2 * t308 * t431 * t434 * t1397 * t57 + 0.6D1 * t87 * t1248 * t8
     #0 * t37 + 0.42D2 * t1407 - 0.48D2 * t1091 * t528 - 0.48D2 * t277 *
     # t1347 * t265 + 0.30D2 * t1415 + 0.5D1 * t1417 - 0.104D3 * t1421 -
     # 0.16D2 * t1192 * t447 * t1135 + 0.6D1 * t1315 * t746 * t1316 - 0.
     #32D2 * t489 * t490 * t1185
      t1458 = t308 * t912 * t359 * t183 * t57
      t1466 = t489 * t1339 * t456
      t1471 = t31 * t309 * t199 * t1034 * t141
      t1481 = t403 * t404 * t1397 * x3
      t1502 = 0.16D2 * t839 * t270 * t742 - 0.64D2 * t216 * t518 * t1296
     # + 0.38D2 * t740 * t270 * t271 * t57 + 0.12D2 * t988 * t735 * t125
     # * x2 + 0.14D2 * t753 * t893 * t57 * x2 * t5 + 0.12D2 * t753 * t74
     #6 * t765 + 0.120D3 * t1458 - 0.16D2 * t662 * t389 * z * t93 * x3 -
     # 0.80D2 * t1466 + 0.40D2 * t1471 + 0.24D2 * t1321 * t271 * t58 - 0
     #.64D2 * t351 * t560 * t44 - 0.102D3 * t1481 + 0.176D3 * t512 * t70
     #2 * t125 + 0.16D2 * t345 * t1080 + 0.6D1 * t603 * t995 * t74 * x2 
     #* t24 + 0.14D2 * t259 * t423 * t67 * t995 * t707 - 0.8D1 * t87 * t
     #586 * t271 * t50
      t1528 = -0.48D2 * t139 + 0.80D2 * t154 - 0.32D2 * t228 + 0.64D2 * 
     #t233 - 0.64D2 * t244 - 0.64D2 * t248 - 0.32D2 * t306 + 0.16D2 * t3
     #99 + 0.32D2 * t408 + 0.64D2 * t421 - 0.64D2 * t439 - 0.64D2 * t498
     # + 0.96D2 * t504 + 0.16D2 * t509 + 0.16D2 * t523 + t530 + 0.16D2 *
     # t558 + 0.16D2 * t564 + 0.64D2 * t578 + 0.16D2 * t608
      t1548 = -0.64D2 * t615 - 0.64D2 * t626 - 0.32D2 * t670 - 0.32D2 * 
     #t680 + 0.32D2 * t700 + 0.64D2 * t709 - 0.64D2 * t722 + 0.16D2 * t7
     #60 - 0.64D2 * t767 + t811 + 0.96D2 * t819 + 0.16D2 * t862 + 0.16D2
     # * t868 + 0.96D2 * t877 + 0.16D2 * t880 + 0.64D2 * t901 - 0.64D2 *
     # t925 + 0.16D2 * t941 - 0.48D2 * t950 + 0.64D2 * t964
      t1569 = 0.32D2 * t977 + t986 + 0.256D3 * t1003 + 0.64D2 * t1008 + 
     #0.32D2 * t1021 + 0.32D2 * t1047 - 0.32D2 * t1050 + 0.96D2 * t1078 
     #- 0.32D2 * t1088 + 0.16D2 * t1108 - 0.64D2 * t1137 + 0.256D3 * t11
     #40 - 0.16D2 * t1146 - 0.32D2 * t1187 + 0.32D2 * t1200 - 0.32D2 * t
     #1203 - 0.32D2 * t1207 + 0.64D2 * t1255 - 0.64D2 * t1260 - 0.16D2 *
     # t1264
      t1591 = -0.32D2 * t1370 - 0.64D2 * t1375 + 0.64D2 * t1388 - 0.32D2
     # * t1407 - 0.64D2 * t1415 + 0.32D2 * t1417 + 0.64D2 * t1421 - 0.32
     #D2 * t1458 + 0.16D2 * t1466 - 0.64D2 * t1471 + 0.48D2 * t1481
      rrgg2ggh61J6 = 0.9D1 / 0.16D2 * (0.5D1 * wd * (t162 + t246 + t365 
     #+ t468 + t580 + t682 + t787 + t882 + t974 + t1052 + t1131 + t1214 
     #+ t1292 + t1365 + t1432 + t1502) + 0.4D1 * wd * (t1528 + t1548 + t
     #1569 + 0.32D2 * t1269 + 0.80D2 * t1276 + 0.48D2 * t1280 - 0.64D2 *
     # t1305 + 0.64D2 * t1310 - 0.64D2 * t1327 + t1333 + 0.16D2 * t1341 
     #- 0.64D2 * t1345 - 0.64D2 * t1357 + t1591)) / t27 / t28 / t39 / z 
     #/ 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh61J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t11 = x2 * x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + t11 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * t6 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t34 = t14 * t4 + x2 * t7 - t20
      t37 = t24 * x3
      t39 = s - t2 * t6 * t34 - t2 * t37
      t40 = t31 * t39
      t41 = t1 ** 2
      t42 = t41 * t1
      t44 = t24 ** 2
      t45 = x3 ** 2
      t46 = t44 * t45
      t47 = t42 * x1 * t46
      t51 = t7 ** 2
      t53 = t39 * x1
      t57 = t39 * t42
      t58 = t31 * t57
      t59 = x1 * z
      t69 = t42 * t44
      t72 = z + x1 * t8 * t1
      t73 = t69 * t72
      t75 = t4 ** 2
      t76 = 0.1D1 / t75
      t78 = x1 * x2
      t83 = x1 ** 2
      t88 = t21 * t24 * x3
      t92 = t41 ** 2
      t93 = t92 * t1
      t94 = t93 * t44
      t97 = t83 * x1
      t98 = t39 * t97
      t104 = t39 * t27
      t105 = t92 * t42
      t107 = t104 * t30 * t105
      t108 = t83 ** 2
      t109 = t75 ** 2
      t110 = 0.1D1 / t109
      t112 = x2 ** 2
      t113 = t112 * x2
      t115 = t44 * t24
      t120 = t92 * t115
      t122 = t31 * t120 * t7
      t123 = t72 * t76
      t124 = t123 * t39
      t125 = x1 * t21
      t126 = t125 * x3
      t132 = 0.1D1 / t75 / t4
      t137 = t92 * t24
      t139 = t31 * t137 * t72
      t140 = t110 * t39
      t141 = t97 * t34
      t142 = t21 ** 2
      t150 = t76 * t39
      t155 = t29 * t28
      t156 = t155 * t93
      t158 = t156 * t83 * t27
      t160 = t45 * x3
      t161 = t115 * t160
      t166 = t156 * t97 * t27
      t167 = t76 * t34
      t173 = t31 * t137
      t174 = t72 * t110
      t175 = t142 * t21
      t180 = t34 ** 2
      t181 = t97 * t180
      t186 = t39 * t92
      t190 = t11 * t5
      t194 = t93 * t115
      t196 = t31 * t194 * t7
      t197 = t39 * t83
      t201 = t39 * t93
      t202 = t31 * t201
      t209 = t115 * t7
      t210 = t112 * t83
      t215 = -0.64D2 * t40 * t47 - 0.48D2 * t31 * t42 * t44 * t51 * t53 
     #+ 0.96D2 * t58 * t59 * t46 + 0.16D2 * t31 * t39 * t41 * t59 * t37 
     #+ 0.16D2 * t39 * t30 * t73 * t76 * t27 * t78 * z + 0.16D2 * t31 * 
     #t57 * t83 * z * t5 * t88 - 0.64D2 * t31 * t94 * t7 * t98 * t76 * t
     #21 * x2 - 0.32D2 * t107 * t108 * t110 * t34 * t113 * t115 + 0.16D2
     # * t122 * t124 * t126 - 0.64D2 * t107 * t108 * t113 * t115 * t132 
     #- 0.64D2 * t139 * t140 * t141 * t142 + 0.16D2 * t31 * t41 * t24 * 
     #t72 * t150 * t59 * t21 - 0.32D2 * t158 * t5 * t34 * t161 + 0.64D2 
     #* t166 * t167 * t21 * t44 * t45 + 0.64D2 * t173 * t174 * t98 * t17
     #5 - 0.16D2 * t139 * t140 * t181 * t21 - 0.64D2 * t31 * t186 * t83 
     #* z * t44 * t190 + 0.256D3 * t196 * t197 * t190 + 0.96D2 * t202 * 
     #t97 * z * t112 * t44 * t76 + 0.64D2 * t202 * t209 * t210 * t76
      t216 = t194 * t72
      t217 = t31 * t216
      t218 = t132 * t39
      t219 = t218 * t83
      t225 = t39 * t155
      t236 = t31 * t120
      t237 = t51 * t39
      t238 = x3 * x1
      t242 = t44 * t7
      t248 = t31 * t186 * t97
      t251 = t21 * x2 * t24
      t257 = t83 * x2 * t5
      t266 = t44 ** 2
      t273 = t92 * t41
      t274 = t273 * t115
      t281 = t94 * t72
      t282 = t31 * t281
      t308 = t92 * t44
      t310 = t31 * t308 * t72
      t313 = t83 * t21 * x2
      t322 = z ** 2
      t332 = t31 * t308 * t7
      t333 = t72 * t132
      t335 = t333 * t197 * t142
      t341 = t42 * t24
      t343 = t31 * t341 * t72
      t344 = t83 * t34
      t351 = t150 * z
      t352 = t11 * x1
      t356 = -0.64D2 * t217 * t219 * t34 * x3 * x2 - 0.32D2 * t225 * t93
     # * t24 * t72 / t109 / t4 * t108 * t34 * t175 - 0.16D2 * t236 * t23
     #7 * t238 - 0.32D2 * t58 * t242 * t78 * t5 + 0.16D2 * t248 * z * t7
     #6 * t251 + 0.16D2 * t31 * t194 * t237 * t257 + 0.80D2 * t173 * t17
     #4 * t98 * t180 * t34 - 0.32D2 * t107 * t266 * t7 * t113 * t97 * t1
     #32 - 0.64D2 * t31 * t274 * t72 * t140 * t141 * t112 + 0.16D2 * t28
     #2 * t140 * t181 * x2 - 0.64D2 * t31 * t105 * t266 * t174 * t39 * t
     #113 * t97 + 0.64D2 * t225 * t281 * t110 * t97 * t34 * t142 * x3 - 
     #0.64D2 * t31 * t201 * t97 * t167 * t44 * x3 * x2 - 0.64D2 * t310 *
     # t218 * z * t313 - 0.32D2 * t166 * t242 * t76 * t142 * x3 + 0.64D2
     # * t30 * t322 * t104 * t92 * t112 * t83 * t44 * t76 + 0.32D2 * t33
     #2 * t335 + 0.16D2 * t248 * t167 * t88 - 0.64D2 * t343 * t218 * t34
     #4 * t21 + 0.16D2 * t31 * t120 * t72 * t351 * t352
      t359 = t31 * t39 * t273
      t360 = t108 * t132
      t366 = t155 * t322
      t373 = t39 * t7
      t388 = t132 * t34
      t407 = t31 * t308
      t408 = t83 * t5
      t433 = t31 * t341
      t452 = t5 * t21
      t459 = t34 * t21
      t464 = 0.64D2 * t359 * t360 * t21 * t112 * t44 - 0.64D2 * t366 * t
     #341 * t335 + 0.64D2 * t156 * t115 * t72 * t132 * t373 * t83 * t142
     # * x3 - 0.32D2 * t156 * t266 * t72 * t76 * t373 * t125 * t45 + 0.1
     #6D2 * t31 * t201 * t108 * t388 * t251 + 0.96D2 * t217 * t218 * z *
     # t112 * t83 - 0.64D2 * t196 * t333 * t39 * t313 + 0.16D2 * t31 * t
     #93 * t266 * t7 * t124 * t352 + 0.48D2 * t407 * t237 * t408 * t21 +
     # 0.32D2 * t310 * t218 * t344 * x2 + 0.32D2 * t407 * t373 * t257 + 
     #0.16D2 * t31 * t73 * t351 * t126 - 0.64D2 * t31 * t69 * t373 * t23
     #8 + 0.16D2 * t58 * t408 * t34 * t24 * x3 - 0.64D2 * t433 * t335 - 
     #0.32D2 * t156 * t44 * t72 * t140 * t7 * t97 * t175 - 0.48D2 * t433
     # * t333 * t197 * t180 + 0.48D2 * t310 * t218 * t83 * t180 * x3 + 0
     #.32D2 * t332 * t197 * t452 * x3 - 0.32D2 * t225 * t216 * t132 * t8
     #3 * t459 * t45
      t465 = t31 * t186
      t484 = z * t83
      t499 = t34 * t112 * t44
      t518 = t273 * t266
      t532 = t34 * x2 * t24
      t576 = 0.32D2 * t31 * t518 * t7 * t333 * t39 * t112 * t83 + 0.64D2
     # * t202 * t97 * t132 * t499 - 0.32D2 * t58 * t83 * t76 * t532 + 0.
     #64D2 * t158 * t209 * t452 * t45 - 0.64D2 * t31 * t274 * t373 * t97
     # * t112 * t76 + 0.80D2 * t31 * t92 * t115 * t51 * t7 * t53 + 0.16D
     #2 * t465 * t97 * t76 * t532 + 0.64D2 * t31 * t518 * t72 * t218 * t
     #210 * x3 - 0.32D2 * t27 * t155 * t93 * x1 * t266 * t7 * t160 + 0.6
     #4D2 * t40 * t92 * x1 * t161 - 0.64D2 * t366 * t27 * t47
      rrgg2ggh61J7 = 0.45D2 / 0.16D2 * wd * (t215 + t356 + t464 + 0.32D2
     # * t465 * t408 * t34 * t44 * t45 - 0.32D2 * t156 * t108 * t27 * t3
     #88 * t142 * t24 * x3 + 0.16D2 * t31 * t69 * t7 * t123 * t53 * t21 
     #+ 0.96D2 * t343 * t218 * t484 * t142 + 0.32D2 * t310 * t219 * t459
     # * x3 + 0.16D2 * t58 * t484 * x2 * t24 * t5 + 0.32D2 * t359 * t360
     # * t499 - 0.64D2 * t236 * t373 * x1 * t45 + 0.256D3 * t282 * t140 
     #* t97 * t459 * x2 + 0.16D2 * t122 * t123 * t39 * x2 * x1 + t576) /
     # t27 / t28 / t39 / z / 0.3141592653589793D1

      end function
  
 