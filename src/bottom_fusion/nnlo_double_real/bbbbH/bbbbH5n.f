  
      subroutine bbbbH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH51J1  
      doubleprecision bbbbH51J2  
      doubleprecision bbbbH51J3  
      doubleprecision bbbbH52J1  
      doubleprecision bbbbH52J2  
      doubleprecision bbbbH52J3  
      doubleprecision bbbbH5n1e1  
      doubleprecision bbbbH5n1e0  
      doubleprecision bbbbH5n1em1  
      doubleprecision bbbbH5n1em2  
      doubleprecision bbbbH5n1em3  
      doubleprecision bbbbH5n1em4  
      doubleprecision bbbbH5n2e1  
      doubleprecision bbbbH5n2e0  
      doubleprecision bbbbH5n2em1  
      doubleprecision bbbbH5n2em2  
      doubleprecision bbbbH5n2em3  
      doubleprecision bbbbH5n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH5n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH5n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH5n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH5n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH5n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH5n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = t17 ** 2
      t19 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t22 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t28 - t30
      t32 = t31 * t5
      t37 = t18 * t17
      t46 = 0.60D2 * lh * t29
      t48 = 0.120D3 * t27 * lh
      t49 = t46 - 0.2884936567583026D3 - t48
      t50 = t49 * t5
      t51 = t50 * t19
      t53 = 0.1D1 / x1
      t56 = x3 * t8
      t57 = t14 * t11
      t60 = log(0.4D1 * t56 * t57)
      t62 = -0.1D1 + x3
      t63 = 0.1D1 / t62
      t64 = t57 * t63
      t67 = log(-0.4D1 * t56 * t64)
      t70 = cos(t9)
      t72 = Sqrt(-x3 * t62)
      t76 = 0.1D1 / (-0.1D1 + 0.2D1 * t70 * t72 - x3)
      t82 = t60 ** 2
      t86 = t67 ** 2
      t95 = -t19 - t19 * t76
      t98 = 0.1D1 / x3
      t102 = x2 ** 2
      t103 = x3 * t102
      t104 = t103 * t8
      t107 = log(-0.4D1 * t104 * t64)
      t111 = t103 * t15
      t113 = log(0.4D1 * t111)
      t118 = -t95
      t123 = 0.1D1 / x2
      t124 = t123 * t53
      t127 = t102 * t8
      t130 = log(0.4D1 * t127 * t57)
      t136 = t130 ** 2
      t142 = t32 * t19
      t148 = log(0.4D1 * t57)
      t151 = t148 ** 2
      t154 = (0.180D3 * t148 * lh + t28 - t30 + 0.45D2 * t151) * t5
      t160 = t151 * t148
      t163 = (-t148 * t31 - 0.90D2 * t151 * lh + t46 - 0.288493656758302
     #6D3 - t48 - 0.15D2 * t160) * t5
      t166 = t151 ** 2
      t170 = t29 ** 2
      t171 = t27 ** 2
      t180 = (0.15D2 / 0.4D1 * t166 - t148 * t49 + 0.5769873135166051D3 
     #* lh + t170 + 0.60D2 * t171 - 0.60D2 * t27 * t29 + t151 * t31 / 0.
     #2D1 + 0.30D2 * t160 * lh) * t5
      t188 = x3 * t11
      t192 = log(-0.4D1 * t188 * t14 * t63)
      t193 = t192 ** 2
      t197 = log(0.4D1 * t188 * t14)
      t198 = t197 ** 2
      t200 = -t193 * t76 / 0.2D1 - t198 / 0.2D1
      t208 = t197 + t192 * t76
      t215 = t193 * t192 * t76 / 0.6D1 + t198 * t197 / 0.6D1
      t222 = -0.1D1 - t76
      t227 = t102 * t11
      t230 = log(0.4D1 * t227 * t14)
      t232 = t230 ** 2
      t244 = t232 * t230
      t255 = log(0.4D1 * t103 * t57)
      t259 = log(-0.4D1 * t103 * t64)
      t268 = t259 ** 2
      t273 = t255 ** 2
      t284 = (-0.180D3 * t6 * (-t7 - t18 * t19 / 0.2D1 + t17 * t22) + t3
     #2 * (-t22 + t17 * t19) + 0.90D2 * t5 * (t17 * t7 + t37 * t19 / 0.6
     #D1 - t18 * t22 / 0.2D1) - t51) * t53 / 0.5760D4 + (-0.180D3 * t6 *
     # (-t22 + t60 * t19 - (t22 - t67 * t19) * t76) + 0.90D2 * t5 * (-t7
     # + t60 * t22 - t82 * t19 / 0.2D1 - (t7 - t67 * t22 + t86 * t19 / 0
     #.2D1) * t76) + t32 * t95) * t98 * t53 / 0.5760D4 - (0.90D2 * t5 * 
     #(t22 + (t22 - t107 * t19) * t76 - t113 * t19) - 0.180D3 * t6 * t11
     #8) * t98 * t124 / 0.2880D4 + (-0.180D3 * t6 * (-t22 + t130 * t19) 
     #+ 0.90D2 * t5 * (-t7 + t130 * t22 - t136 * t19 / 0.2D1) - t142) * 
     #t123 * t53 / 0.2880D4 - t154 * t7 / 0.11520D5 - t163 * t22 / 0.115
     #20D5 - t180 * t19 / 0.11520D5 + ((0.90D2 * t5 * t22 - 0.180D3 * t6
     # * t19) * t200 + (0.90D2 * t5 * t7 - 0.180D3 * t6 * t22 + t142) * 
     #t208 + 0.90D2 * t5 * t19 * t215 + (-0.180D3 * t6 * t7 + t32 * t22 
     #+ t51) * t222) * t98 / 0.11520D5 - (-0.180D3 * t6 * (t7 - t230 * t
     #22 + t232 * t19 / 0.2D1) + t32 * (t22 - t230 * t19) + 0.90D2 * t5 
     #* (-t230 * t7 + t232 * t22 / 0.2D1 - t244 * t19 / 0.6D1) + t51) * 
     #t123 / 0.5760D4 - (-0.180D3 * t6 * (t22 - t255 * t19 + (t22 - t259
     # * t19) * t76) + 0.90D2 * t5 * (t7 - t255 * t22 + (t7 - t259 * t22
     # + t268 * t19 / 0.2D1) * t76 + t273 * t19 / 0.2D1) + t32 * t118) *
     # t98 * t123 / 0.5760D4
      t285 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t284)
      t287 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t288 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t290 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t307 = t50 * t290
      t330 = -t290 - t290 * t76
      t343 = -t330
      t360 = t32 * t290
      t437 = (-0.180D3 * t6 * (-t287 + t17 * t288 - t18 * t290 / 0.2D1) 
     #+ t32 * (-t288 + t17 * t290) + 0.90D2 * t5 * (t17 * t287 + t37 * t
     #290 / 0.6D1 - t18 * t288 / 0.2D1) - t307) * t53 / 0.5760D4 + (-0.1
     #80D3 * t6 * (t60 * t290 - t288 - (t288 - t67 * t290) * t76) + 0.90
     #D2 * t5 * (t60 * t288 - t82 * t290 / 0.2D1 - t287 - (t287 - t67 * 
     #t288 + t86 * t290 / 0.2D1) * t76) + t32 * t330) * t98 * t53 / 0.57
     #60D4 - (0.90D2 * t5 * (t288 + (t288 - t107 * t290) * t76 - t113 * 
     #t290) - 0.180D3 * t6 * t343) * t98 * t124 / 0.2880D4 + (-0.180D3 *
     # t6 * (-t288 + t130 * t290) + 0.90D2 * t5 * (-t287 + t130 * t288 -
     # t136 * t290 / 0.2D1) - t360) * t123 * t53 / 0.2880D4 - t154 * t28
     #7 / 0.11520D5 - t163 * t288 / 0.11520D5 - t180 * t290 / 0.11520D5 
     #+ ((0.90D2 * t5 * t288 - 0.180D3 * t6 * t290) * t200 + (0.90D2 * t
     #5 * t287 - 0.180D3 * t6 * t288 + t360) * t208 + 0.90D2 * t5 * t290
     # * t215 + (-0.180D3 * t6 * t287 + t32 * t288 + t307) * t222) * t98
     # / 0.11520D5 - (-0.180D3 * t6 * (t287 - t230 * t288 + t232 * t290 
     #/ 0.2D1) + t32 * (-t230 * t290 + t288) + 0.90D2 * t5 * (-t230 * t2
     #87 + t232 * t288 / 0.2D1 - t244 * t290 / 0.6D1) + t307) * t123 / 0
     #.5760D4 - (-0.180D3 * t6 * ((t288 - t259 * t290) * t76 - t255 * t2
     #90 + t288) + 0.90D2 * t5 * (t287 + t273 * t290 / 0.2D1 + (t287 - t
     #259 * t288 + t268 * t290 / 0.2D1) * t76 - t255 * t288) + t32 * t34
     #3) * t98 * t123 / 0.5760D4
      t438 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t437)
      t440 = t2 * x1
      t441 = -0.1D1 + x1
      t442 = t2 * t441
      t443 = x1 * z
      t444 = 0.1D1 - x1 + t443
      t445 = 0.1D1 / t444
      t446 = t14 * t445
      t447 = t441 ** 2
      t448 = t446 * t447
      t451 = log(0.4D1 * t12 * t448)
      t452 = -t441
      t453 = bbbbH52J2(s, XB1, XB2, z, lh, wd, t452, 0.0D0, 0.0D0, x4)
      t455 = t451 ** 2
      t456 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t452, 0.0D0, 0.0D0, x4)
      t459 = bbbbH52J3(s, XB1, XB2, z, lh, wd, t452, 0.0D0, 0.0D0, x4)
      t469 = t455 * t451
      t479 = t56 * t11
      t481 = t446 * t447 * t63
      t484 = log(-0.4D1 * t479 * t481)
      t487 = x3 * x1
      t488 = t487 * z
      t491 = x3 * t444
      t493 = Sqrt(-t491 * t62)
      t497 = 0.1D1 / (-0.2D1 * t488 + 0.2D1 * t487 - 0.1D1 + 0.2D1 * t70
     # * t493 - x3)
      t501 = log(0.4D1 * t479 * t448)
      t507 = t484 ** 2
      t513 = t501 ** 2
      t520 = t456 + t456 * t497
      t526 = t103 * t12
      t529 = log(-0.4D1 * t526 * t481)
      t533 = t445 * t447
      t537 = log(0.4D1 * t104 * t57 * t533)
      t549 = t127 * t11
      t552 = log(0.4D1 * t549 * t448)
      t558 = t552 ** 2
      t569 = (-0.180D3 * t6 * (-t451 * t453 + t455 * t456 / 0.2D1 + t459
     #) + t32 * (t453 - t451 * t456) + 0.90D2 * t5 * (-t451 * t459 + t45
     #5 * t453 / 0.2D1 - t469 * t456 / 0.6D1) + t50 * t456) * t53 / 0.57
     #60D4 + (-0.180D3 * t6 * ((t453 - t484 * t456) * t497 - t501 * t456
     # + t453) + 0.90D2 * t5 * (t459 + (t459 - t484 * t453 + t507 * t456
     # / 0.2D1) * t497 - t501 * t453 + t513 * t456 / 0.2D1) + t32 * t520
     #) * t98 * t53 / 0.5760D4 - (0.90D2 * t5 * (-(t453 - t529 * t456) *
     # t497 - t453 + t537 * t456) + 0.180D3 * t6 * t520) * t98 * t124 / 
     #0.2880D4 + (-0.180D3 * t6 * (t453 - t552 * t456) + 0.90D2 * t5 * (
     #t459 - t552 * t453 + t558 * t456 / 0.2D1) + t32 * t456) * t123 * t
     #53 / 0.2880D4
      t570 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t440, -t442, 0.0D0, t569)
      t572 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t452, 0.0D0, 0.0D0, x4)
      t575 = bbbbH51J3(s, XB1, XB2, z, lh, wd, t452, 0.0D0, 0.0D0, x4)
      t576 = bbbbH51J2(s, XB1, XB2, z, lh, wd, t452, 0.0D0, 0.0D0, x4)
      t615 = t572 + t572 * t497
      t650 = (-0.180D3 * t6 * (t455 * t572 / 0.2D1 + t575 - t451 * t576)
     # + t32 * (-t451 * t572 + t576) + 0.90D2 * t5 * (-t451 * t575 + t45
     #5 * t576 / 0.2D1 - t469 * t572 / 0.6D1) + t50 * t572) * t53 / 0.57
     #60D4 + (-0.180D3 * t6 * (-t501 * t572 + (t576 - t484 * t572) * t49
     #7 + t576) + 0.90D2 * t5 * (-t501 * t576 + t575 + t513 * t572 / 0.2
     #D1 + (t575 - t484 * t576 + t507 * t572 / 0.2D1) * t497) + t32 * t6
     #15) * t98 * t53 / 0.5760D4 - (0.90D2 * t5 * (-(t576 - t529 * t572)
     # * t497 + t537 * t572 - t576) + 0.180D3 * t6 * t615) * t98 * t124 
     #/ 0.2880D4 + (-0.180D3 * t6 * (-t552 * t572 + t576) + 0.90D2 * t5 
     #* (-t552 * t576 + t558 * t572 / 0.2D1 + t575) + t32 * t572) * t123
     # * t53 / 0.2880D4
      t651 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t442, t440, 0.0D0, t650)
      t653 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t437)
      t656 = x2 * t1 * s
      t657 = -0.1D1 + x2
      t659 = t657 * t1 * s
      t660 = t57 * t657
      t663 = log(-0.4D1 * t104 * t660)
      t664 = x2 * z
      t666 = 0.1D1 / (0.1D1 + t664 - x2)
      t667 = t663 * t666
      t668 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t670 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t671 = t666 * t670
      t675 = t666 * t668
      t684 = log(-0.4D1 * t127 * t660)
      t685 = t684 * t666
      t691 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t692 = t666 * t691
      t693 = t684 ** 2
      t694 = t693 * t666
      t700 = t32 * t675
      t705 = t14 * t657
      t708 = log(-0.4D1 * t227 * t705)
      t709 = t708 ** 2
      t710 = t709 * t666
      t713 = t708 * t666
      t723 = t709 * t708 * t666
      t737 = log(-0.4D1 * t103 * t660)
      t738 = t737 * t666
      t743 = t737 ** 2
      t744 = t743 * t666
      t755 = -(0.90D2 * t5 * (t667 * t668 - t671) + 0.180D3 * t6 * t675)
     # * t98 * t124 / 0.2880D4 + (-0.180D3 * t6 * (-t685 * t668 + t671) 
     #+ 0.90D2 * t5 * (-t685 * t670 + t692 + t694 * t668 / 0.2D1) + t700
     #) * t123 * t53 / 0.2880D4 - (-0.180D3 * t6 * (-t692 - t710 * t668 
     #/ 0.2D1 + t713 * t670) + t32 * (-t671 + t713 * t668) + 0.90D2 * t5
     # * (t713 * t691 + t723 * t668 / 0.6D1 - t710 * t670 / 0.2D1) - t50
     # * t675) * t123 / 0.5760D4 - (-0.180D3 * t6 * (-t671 + t738 * t668
     #) + 0.90D2 * t5 * (-t692 - t744 * t668 / 0.2D1 + t738 * t670) - t7
     #00) * t98 * t123 / 0.5760D4
      t756 = FJET(XB1, XB2, s, 0.0D0, t656, 0.0D0, -t659, 0.0D0, t755)
      t758 = x2 * x3
      t761 = Sqrt(x3 * t657 * t62)
      t762 = t70 * t761
      t764 = 0.2D1 * t762 * x2
      t766 = 0.1D1 - x3 + t758
      t767 = 0.1D1 / t766
      t769 = t2 * (0.1D1 - x3 - x2 + t758 + t103 + t764) * t767
      t770 = 0.2D1 * t762
      t774 = t2 * x2 * (-0.1D1 + t758 + t770) * t767
      t777 = t103 * z
      t779 = 0.1D1 / (-0.1D1 + t770 - t764 + 0.2D1 * t762 * t664 - t103 
     #- t664 + x2 - x3 + t758 + t777)
      t780 = t758 * t767
      t781 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t780, x4)
      t782 = t779 * t781
      t783 = t766 ** 2
      t784 = 0.1D1 / t783
      t786 = t705 * t62 * t784
      t789 = log(0.4D1 * t526 * t786)
      t790 = t789 * t779
      t791 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t780, x4)
      t796 = t779 * t791
      t806 = log(0.4D1 * t103 * t11 * t786)
      t807 = t806 * t779
      t812 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t780, x4)
      t815 = t806 ** 2
      t816 = t815 * t779
      t827 = -(0.90D2 * t5 * (-t782 + t790 * t791) + 0.180D3 * t6 * t796
     #) * t98 * t124 / 0.2880D4 - (-0.180D3 * t6 * (-t782 + t807 * t791)
     # + 0.90D2 * t5 * (-t779 * t812 + t807 * t781 - t816 * t791 / 0.2D1
     #) - t32 * t796) * t98 * t123 / 0.5760D4
      t828 = FJET(XB1, XB2, s, 0.0D0, t769, 0.0D0, -t774, 0.0D0, t827)
      t830 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t832 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t833 = t666 * t832
      t837 = t666 * t830
      t849 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t850 = t666 * t849
      t856 = t32 * t837
      t896 = -(0.90D2 * t5 * (t667 * t830 - t833) + 0.180D3 * t6 * t837)
     # * t98 * t124 / 0.2880D4 + (-0.180D3 * t6 * (-t685 * t830 + t833) 
     #+ 0.90D2 * t5 * (-t685 * t832 + t850 + t694 * t830 / 0.2D1) + t856
     #) * t123 * t53 / 0.2880D4 - (-0.180D3 * t6 * (-t850 - t710 * t830 
     #/ 0.2D1 + t713 * t832) + t32 * (-t833 + t713 * t830) + 0.90D2 * t5
     # * (t713 * t849 + t723 * t830 / 0.6D1 - t710 * t832 / 0.2D1) - t50
     # * t837) * t123 / 0.5760D4 - (-0.180D3 * t6 * (-t833 + t738 * t830
     #) + 0.90D2 * t5 * (t738 * t832 - t744 * t830 / 0.2D1 - t850) - t85
     #6) * t98 * t123 / 0.5760D4
      t897 = FJET(XB1, XB2, s, 0.0D0, -t659, 0.0D0, t656, 0.0D0, t896)
      t899 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t780, x4)
      t901 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t780, x4)
      t902 = t779 * t901
      t906 = t779 * t899
      t917 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t780, x4)
      t930 = -(0.90D2 * t5 * (t790 * t899 - t902) + 0.180D3 * t6 * t906)
     # * t98 * t124 / 0.2880D4 - (-0.180D3 * t6 * (-t902 + t807 * t899) 
     #+ 0.90D2 * t5 * (-t779 * t917 - t816 * t899 / 0.2D1 + t807 * t901)
     # - t32 * t906) * t98 * t123 / 0.5760D4
      t931 = FJET(XB1, XB2, s, 0.0D0, -t774, 0.0D0, t769, 0.0D0, t930)
      t935 = t2 * t441 * x2 * t445
      t938 = t657 * s * t1 * t441
      t939 = t1 ** 2
      t944 = s * t939 * x2 * x1 * t441 * t445
      t945 = x2 * x1
      t946 = t945 * z
      t948 = 0.1D1 / (-0.1D1 - t664 - t945 + x1 + x2 - t443 + t946)
      t949 = bbbbH52J2(s, XB1, XB2, z, lh, wd, t452, x2, 0.0D0, x4)
      t950 = t948 * t949
      t952 = t446 * t447 * t657
      t955 = log(-0.4D1 * t526 * t952)
      t956 = t955 * t948
      t957 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t452, x2, 0.0D0, x4)
      t964 = t948 * t957 * t444
      t972 = log(-0.4D1 * t549 * t952)
      t973 = t972 * t948
      t979 = bbbbH52J3(s, XB1, XB2, z, lh, wd, t452, x2, 0.0D0, x4)
      t982 = t972 ** 2
      t983 = t982 * t948
      t995 = -(0.90D2 * t5 * (-t950 + t956 * t957) * t444 + 0.180D3 * t6
     # * t964) * t98 * t124 / 0.2880D4 + (0.180D3 * t6 * (-t950 + t973 *
     # t957) * t444 - 0.90D2 * t5 * (-t948 * t979 + t973 * t949 - t983 *
     # t957 / 0.2D1) * t444 + t32 * t964) * t123 * t53 / 0.2880D4
      t996 = FJET(XB1, XB2, s, 0.0D0, -t935, t440, t938, -t944, t995)
      t998 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t284)
      t1000 = bbbbH51J2(s, XB1, XB2, z, lh, wd, t452, x2, 0.0D0, x4)
      t1001 = t948 * t1000
      t1002 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t452, x2, 0.0D0, x4)
      t1009 = t948 * t1002 * t444
      t1020 = bbbbH51J3(s, XB1, XB2, z, lh, wd, t452, x2, 0.0D0, x4)
      t1034 = -(0.90D2 * t5 * (-t1001 + t956 * t1002) * t444 + 0.180D3 *
     # t6 * t1009) * t98 * t124 / 0.2880D4 + (0.180D3 * t6 * (-t1001 + t
     #973 * t1002) * t444 - 0.90D2 * t5 * (-t948 * t1020 + t973 * t1000 
     #- t983 * t1002 / 0.2D1) * t444 + t32 * t1009) * t123 * t53 / 0.288
     #0D4
      t1035 = FJET(XB1, XB2, s, t440, t938, 0.0D0, -t935, -t944, t1034)
      t1037 = t285 * t284 + t438 * t437 + t570 * t569 + t651 * t650 + t6
     #53 * t437 + t756 * t755 + t828 * t827 + t897 * t896 + t931 * t930 
     #+ t996 * t995 + t998 * t284 + t1035 * t1034
      t1038 = FJET(XB1, XB2, s, t440, -t442, 0.0D0, 0.0D0, 0.0D0, t650)
      t1040 = FJET(XB1, XB2, s, t656, 0.0D0, -t659, 0.0D0, 0.0D0, t896)
      t1042 = FJET(XB1, XB2, s, t769, 0.0D0, -t774, 0.0D0, 0.0D0, t930)
      t1044 = FJET(XB1, XB2, s, t938, t440, -t935, 0.0D0, -t944, t995)
      t1046 = t440 * t780
      t1047 = t758 * x1
      t1048 = t758 * t443
      t1049 = t657 * t62
      t1051 = Sqrt(t491 * t1049)
      t1052 = t70 * t1051
      t1053 = 0.2D1 * t1052
      t1058 = t442 * x2 * (t487 - t488 + t758 - t1047 + t1048 - 0.1D1 + 
     #t1053) * t445 * t767
      t1062 = t62 * s * t1 * x1 * t767
      t1064 = 0.2D1 * t1052 * x2
      t1065 = 0.1D1 - x1 + t443 - x2 + t945 - t946 - x3 + t487 - t488 + 
     #t758 - t1047 + t1048 + t103 + t1064
      t1068 = t442 * t1065 * t445 * t767
      t1083 = 0.1D1 - t758 + t103 - t946 - t777 + t664 + 0.2D1 * t1052 *
     # t946 + 0.3D1 * t1047 + t1064 + t104 - 0.4D1 * t56 * z - 0.2D1 * t
     #56 * x2 + 0.2D1 * t56 * t13 - 0.2D1 * t103 * x1 + 0.2D1 * t1052 * 
     #x1 + 0.3D1 * t488 - x2
      t1109 = x3 - 0.4D1 * t1048 - 0.2D1 * t1052 * t443 - 0.2D1 * t1052 
     #* t945 - 0.2D1 * t1052 * t664 + x3 * t13 * t945 + 0.4D1 * t56 * t6
     #64 - 0.2D1 * t56 * t13 * x2 + 0.3D1 * t103 * t443 - t103 * t13 * x
     #1 - 0.2D1 * t103 * t8 * z + t103 * t8 * t13 + t945 + t443 - 0.3D1 
     #* t487 - x1 - t1053 + 0.2D1 * t56
      t1111 = 0.1D1 / (t1083 + t1109)
      t1112 = t444 * t1111
      t1113 = bbbbH52J2(s, XB1, XB2, z, lh, wd, t452, x2, t780, x4)
      t1119 = log(0.4D1 * t111 * t533 * t1049 * t784)
      t1120 = t1119 * t444
      t1121 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t452, x2, t780, x4)
      t1130 = 0.90D2 * t5 * (-t1112 * t1113 + t1120 * t1111 * t1121) + 0
     #.180D3 * t6 * t1112 * t1121
      t1133 = t1130 * t98 * t124 / 0.2880D4
      t1134 = FJET(XB1, XB2, s, t1046, t1058, -t1062, -t1068, -t944, -t1
     #133)
      t1137 = t98 * t123 * t53
      t1140 = bbbbH51J2(s, XB1, XB2, z, lh, wd, t452, x2, t780, x4)
      t1142 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t452, x2, t780, x4)
      t1151 = 0.90D2 * t5 * (-t1112 * t1140 + t1120 * t1111 * t1142) + 0
     #.180D3 * t6 * t1112 * t1142
      t1154 = t1151 * t98 * t124 / 0.2880D4
      t1155 = FJET(XB1, XB2, s, t1058, t1046, -t1068, -t1062, -t944, -t1
     #154)
      t1159 = FJET(XB1, XB2, s, -t442, t440, 0.0D0, 0.0D0, 0.0D0, t569)
      t1161 = FJET(XB1, XB2, s, -t659, 0.0D0, t656, 0.0D0, 0.0D0, t755)
      t1163 = FJET(XB1, XB2, s, -t774, 0.0D0, t769, 0.0D0, 0.0D0, t827)
      t1165 = FJET(XB1, XB2, s, -t935, 0.0D0, t938, t440, -t944, t1034)
      t1167 = FJET(XB1, XB2, s, -t1062, -t1068, t1046, t1058, -t944, -t1
     #154)
      t1171 = FJET(XB1, XB2, s, -t1068, -t1062, t1058, t1046, -t944, -t1
     #133)
      t1175 = t1038 * t650 + t1040 * t896 + t1042 * t930 + t1044 * t995 
     #- t1134 * t1130 * t1137 / 0.2880D4 - t1155 * t1151 * t1137 / 0.288
     #0D4 + t1159 * t569 + t1161 * t755 + t1163 * t827 + t1165 * t1034 -
     # t1167 * t1151 * t1137 / 0.2880D4 - t1171 * t1130 * t1137 / 0.2880
     #D4
      bbbbH5n1e1 = t1037 + t1175

      end function



      doubleprecision function bbbbH5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t8 * t14)
      t18 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t20 = -0.1D1 + x3
      t21 = 0.1D1 / t20
      t22 = t14 * t21
      t25 = log(-0.4D1 * t8 * t22)
      t28 = cos(t11)
      t30 = Sqrt(-x3 * t20)
      t34 = 0.1D1 / (-0.1D1 + 0.2D1 * t28 * t30 - x3)
      t39 = lh * t5
      t41 = -t18 - t18 * t34
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t50 = -t41
      t52 = 0.1D1 / x2
      t54 = t45 * t52 * t47
      t57 = x2 ** 2
      t58 = t57 * t7
      t61 = log(0.4D1 * t58 * t14)
      t67 = 0.180D3 * t39 * t18
      t72 = t7 * t13
      t75 = log(0.4D1 * t72 * t10)
      t80 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t81 = t75 ** 2
      t88 = lh ** 2
      t89 = 0.180D3 * t88
      t90 = 0.3141592653589793D1 ** 2
      t91 = 0.30D2 * t90
      t92 = t89 - t91
      t93 = t92 * t5
      t94 = t93 * t18
      t101 = x3 * t13
      t104 = log(0.4D1 * t101 * t10)
      t108 = log(-0.4D1 * t101 * t10 * t21)
      t110 = t104 + t108 * t34
      t113 = t108 ** 2
      t115 = t104 ** 2
      t117 = -t113 * t34 / 0.2D1 - t115 / 0.2D1
      t125 = -0.1D1 - t34
      t130 = t57 * x3
      t133 = log(0.4D1 * t130 * t14)
      t137 = log(-0.4D1 * t130 * t22)
      t150 = t57 * t13
      t153 = log(0.4D1 * t150 * t10)
      t159 = t153 ** 2
      t170 = log(0.4D1 * t14)
      t173 = (-0.180D3 * lh - 0.90D2 * t170) * t5
      t178 = t170 ** 2
      t181 = (0.180D3 * t170 * lh + t89 - t91 + 0.45D2 * t178) * t5
      t194 = (-t170 * t92 - 0.90D2 * t178 * lh + 0.60D2 * lh * t90 - 0.2
     #884936567583026D3 - 0.120D3 * t88 * lh - 0.15D2 * t178 * t170) * t
     #5
      t197 = (0.90D2 * t5 * (-t6 + t17 * t18 - (t6 - t25 * t18) * t34) -
     # 0.180D3 * t39 * t41) * t45 * t47 / 0.5760D4 - t5 * t50 * t54 / 0.
     #32D2 + (0.90D2 * t5 * (-t6 + t61 * t18) + t67) * t52 * t47 / 0.288
     #0D4 + (-0.180D3 * t39 * (-t6 + t75 * t18) + 0.90D2 * t5 * (-t80 - 
     #t81 * t18 / 0.2D1 + t75 * t6) - t94) * t47 / 0.5760D4 + ((0.90D2 *
     # t5 * t6 - t67) * t110 + 0.90D2 * t5 * t18 * t117 + (0.90D2 * t5 *
     # t80 - 0.180D3 * t39 * t6 + t94) * t125) * t45 / 0.11520D5 - (0.90
     #D2 * t5 * (t6 - t133 * t18 + (t6 - t137 * t18) * t34) - 0.180D3 * 
     #t39 * t50) * t45 * t52 / 0.5760D4 - (-0.180D3 * t39 * (t6 - t153 *
     # t18) + 0.90D2 * t5 * (t80 - t153 * t6 + t159 * t18 / 0.2D1) + t94
     #) * t52 / 0.5760D4 - t173 * t80 / 0.11520D5 - t181 * t6 / 0.11520D
     #5 - t194 * t18 / 0.11520D5
      t198 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t197)
      t200 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t202 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t210 = -t200 - t200 * t34
      t217 = -t210
      t226 = 0.180D3 * t39 * t200
      t235 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t242 = t93 * t200
      t294 = (0.90D2 * t5 * (t17 * t200 - t202 - (t202 - t25 * t200) * t
     #34) - 0.180D3 * t39 * t210) * t45 * t47 / 0.5760D4 - t5 * t217 * t
     #54 / 0.32D2 + (0.90D2 * t5 * (-t202 + t61 * t200) + t226) * t52 * 
     #t47 / 0.2880D4 + (-0.180D3 * t39 * (-t202 + t75 * t200) + 0.90D2 *
     # t5 * (-t235 + t75 * t202 - t81 * t200 / 0.2D1) - t242) * t47 / 0.
     #5760D4 + ((0.90D2 * t5 * t202 - t226) * t110 + 0.90D2 * t5 * t200 
     #* t117 + (0.90D2 * t5 * t235 - 0.180D3 * t39 * t202 + t242) * t125
     #) * t45 / 0.11520D5 - (0.90D2 * t5 * ((t202 - t137 * t200) * t34 -
     # t133 * t200 + t202) - 0.180D3 * t39 * t217) * t45 * t52 / 0.5760D
     #4 - (-0.180D3 * t39 * (-t153 * t200 + t202) + 0.90D2 * t5 * (t235 
     #- t153 * t202 + t159 * t200 / 0.2D1) + t242) * t52 / 0.5760D4 - t1
     #73 * t235 / 0.11520D5 - t181 * t202 / 0.11520D5 - t194 * t200 / 0.
     #11520D5
      t295 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t294)
      t297 = t2 * x1
      t298 = -0.1D1 + x1
      t299 = t2 * t298
      t300 = -t298
      t301 = bbbbH52J2(s, XB1, XB2, z, lh, wd, t300, 0.0D0, 0.0D0, x4)
      t302 = t8 * t13
      t303 = x1 * z
      t304 = 0.1D1 - x1 + t303
      t305 = 0.1D1 / t304
      t306 = t10 * t305
      t307 = t298 ** 2
      t312 = log(-0.4D1 * t302 * t306 * t307 * t21)
      t313 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t300, 0.0D0, 0.0D0, x4)
      t316 = x3 * x1
      t317 = t316 * z
      t320 = x3 * t304
      t322 = Sqrt(-t320 * t20)
      t326 = 0.1D1 / (-0.2D1 * t317 + 0.2D1 * t316 - 0.1D1 + 0.2D1 * t28
     # * t322 - x3)
      t328 = t306 * t307
      t331 = log(0.4D1 * t302 * t328)
      t337 = t313 + t313 * t326
      t348 = t58 * t13
      t351 = log(0.4D1 * t348 * t328)
      t364 = log(0.4D1 * t72 * t328)
      t370 = t364 ** 2
      t373 = bbbbH52J3(s, XB1, XB2, z, lh, wd, t300, 0.0D0, 0.0D0, x4)
      t381 = (0.90D2 * t5 * ((t301 - t312 * t313) * t326 - t331 * t313 +
     # t301) - 0.180D3 * t39 * t337) * t45 * t47 / 0.5760D4 + t5 * t337 
     #* t54 / 0.32D2 + (0.90D2 * t5 * (t301 - t351 * t313) - 0.180D3 * t
     #39 * t313) * t52 * t47 / 0.2880D4 + (-0.180D3 * t39 * (t301 - t364
     # * t313) + 0.90D2 * t5 * (-t364 * t301 + t370 * t313 / 0.2D1 + t37
     #3) + t93 * t313) * t47 / 0.5760D4
      t382 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t297, -t299, 0.0D0, t381)
      t384 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t300, 0.0D0, 0.0D0, x4)
      t386 = bbbbH51J2(s, XB1, XB2, z, lh, wd, t300, 0.0D0, 0.0D0, x4)
      t394 = t384 + t384 * t326
      t421 = bbbbH51J3(s, XB1, XB2, z, lh, wd, t300, 0.0D0, 0.0D0, x4)
      t430 = (0.90D2 * t5 * (-t331 * t384 + (t386 - t312 * t384) * t326 
     #+ t386) - 0.180D3 * t39 * t394) * t45 * t47 / 0.5760D4 + t5 * t394
     # * t54 / 0.32D2 + (0.90D2 * t5 * (-t351 * t384 + t386) - 0.180D3 *
     # t39 * t384) * t52 * t47 / 0.2880D4 + (-0.180D3 * t39 * (-t364 * t
     #384 + t386) + 0.90D2 * t5 * (t370 * t384 / 0.2D1 + t421 - t364 * t
     #386) + t93 * t384) * t47 / 0.5760D4
      t431 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t299, t297, 0.0D0, t430)
      t433 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t294)
      t436 = x2 * t1 * s
      t437 = -0.1D1 + x2
      t439 = t437 * t1 * s
      t440 = x2 * z
      t442 = 0.1D1 / (0.1D1 + t440 - x2)
      t443 = t5 * t442
      t444 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t448 = t14 * t437
      t451 = log(-0.4D1 * t58 * t448)
      t452 = t451 * t442
      t454 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t455 = t442 * t454
      t459 = t442 * t444
      t461 = 0.180D3 * t39 * t459
      t468 = log(-0.4D1 * t130 * t448)
      t469 = t468 * t442
      t478 = t10 * t437
      t481 = log(-0.4D1 * t150 * t478)
      t482 = t481 * t442
      t487 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t489 = t481 ** 2
      t490 = t489 * t442
      t501 = t443 * t444 * t54 / 0.32D2 + (0.90D2 * t5 * (-t452 * t444 +
     # t455) - t461) * t52 * t47 / 0.2880D4 - (0.90D2 * t5 * (-t455 + t4
     #69 * t444) + t461) * t45 * t52 / 0.5760D4 - (-0.180D3 * t39 * (-t4
     #55 + t482 * t444) + 0.90D2 * t5 * (-t442 * t487 - t490 * t444 / 0.
     #2D1 + t482 * t454) - t93 * t459) * t52 / 0.5760D4
      t502 = FJET(XB1, XB2, s, 0.0D0, t436, 0.0D0, -t439, 0.0D0, t501)
      t504 = x2 * x3
      t507 = Sqrt(x3 * t437 * t20)
      t508 = t28 * t507
      t510 = 0.2D1 * t508 * x2
      t512 = 0.1D1 - x3 + t504
      t513 = 0.1D1 / t512
      t515 = t2 * (0.1D1 - x3 - x2 + t504 + t130 + t510) * t513
      t516 = 0.2D1 * t508
      t520 = t2 * x2 * (-0.1D1 + t504 + t516) * t513
      t523 = t130 * z
      t525 = 0.1D1 / (-0.1D1 + t516 - t510 + 0.2D1 * t508 * t440 - t130 
     #- t440 + x2 - x3 + t504 + t523)
      t526 = t5 * t525
      t527 = t504 * t513
      t528 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t527, x4)
      t532 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t527, x4)
      t535 = t512 ** 2
      t541 = log(0.4D1 * t130 * t13 * t478 * t20 / t535)
      t542 = t541 * t525
      t554 = t526 * t528 * t54 / 0.32D2 - (0.90D2 * t5 * (-t525 * t532 +
     # t542 * t528) + 0.180D3 * t39 * t525 * t528) * t45 * t52 / 0.5760D
     #4
      t555 = FJET(XB1, XB2, s, 0.0D0, t515, 0.0D0, -t520, 0.0D0, t554)
      t557 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t558 = t442 * t557
      t559 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t564 = t442 * t559
      t566 = 0.180D3 * t39 * t564
      t575 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t598 = -(0.90D2 * t5 * (-t558 + t469 * t559) + t566) * t45 * t52 /
     # 0.5760D4 - (-0.180D3 * t39 * (-t558 + t482 * t559) + 0.90D2 * t5 
     #* (-t442 * t575 - t490 * t559 / 0.2D1 + t482 * t557) - t93 * t564)
     # * t52 / 0.5760D4 + t443 * t559 * t54 / 0.32D2 + (0.90D2 * t5 * (-
     #t452 * t559 + t558) - t566) * t52 * t47 / 0.2880D4
      t599 = FJET(XB1, XB2, s, 0.0D0, -t439, 0.0D0, t436, 0.0D0, t598)
      t601 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t527, x4)
      t605 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t527, x4)
      t618 = t526 * t601 * t54 / 0.32D2 - (0.90D2 * t5 * (-t525 * t605 +
     # t542 * t601) + 0.180D3 * t39 * t525 * t601) * t45 * t52 / 0.5760D
     #4
      t619 = FJET(XB1, XB2, s, 0.0D0, -t520, 0.0D0, t515, 0.0D0, t618)
      t623 = t2 * t298 * x2 * t305
      t626 = t437 * s * t1 * t298
      t627 = t1 ** 2
      t632 = s * t627 * x2 * x1 * t298 * t305
      t633 = x2 * x1
      t634 = t633 * z
      t636 = 0.1D1 / (-0.1D1 - t440 - t633 + x1 + x2 - t303 + t634)
      t637 = t5 * t636
      t638 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t300, x2, 0.0D0, x4)
      t641 = t52 * t47
      t642 = t304 * t45 * t641
      t645 = bbbbH52J2(s, XB1, XB2, z, lh, wd, t300, x2, 0.0D0, x4)
      t651 = log(-0.4D1 * t348 * t306 * t307 * t437)
      t652 = t651 * t636
      t666 = t637 * t638 * t642 / 0.32D2 + (-0.90D2 * t5 * (-t636 * t645
     # + t652 * t638) * t304 - 0.180D3 * t39 * t636 * t638 * t304) * t52
     # * t47 / 0.2880D4
      t667 = FJET(XB1, XB2, s, 0.0D0, -t623, t297, t626, -t632, t666)
      t669 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t197)
      t671 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t300, x2, 0.0D0, x4)
      t675 = bbbbH51J2(s, XB1, XB2, z, lh, wd, t300, x2, 0.0D0, x4)
      t690 = t637 * t671 * t642 / 0.32D2 + (-0.90D2 * t5 * (-t636 * t675
     # + t652 * t671) * t304 - 0.180D3 * t39 * t636 * t671 * t304) * t52
     # * t47 / 0.2880D4
      t691 = FJET(XB1, XB2, s, t297, t626, 0.0D0, -t623, -t632, t690)
      t693 = t198 * t197 + t295 * t294 + t382 * t381 + t431 * t430 + t43
     #3 * t294 + t502 * t501 + t555 * t554 + t599 * t598 + t619 * t618 +
     # t667 * t666 + t669 * t197 + t691 * t690
      t694 = FJET(XB1, XB2, s, t297, -t299, 0.0D0, 0.0D0, 0.0D0, t430)
      t696 = FJET(XB1, XB2, s, t436, 0.0D0, -t439, 0.0D0, 0.0D0, t598)
      t698 = FJET(XB1, XB2, s, t515, 0.0D0, -t520, 0.0D0, 0.0D0, t618)
      t700 = FJET(XB1, XB2, s, t626, t297, -t623, 0.0D0, -t632, t666)
      t702 = t297 * t527
      t703 = t504 * x1
      t704 = t504 * t303
      t707 = Sqrt(t320 * t437 * t20)
      t708 = t28 * t707
      t709 = 0.2D1 * t708
      t714 = t299 * x2 * (t316 - t317 + t504 - t703 + t704 - 0.1D1 + t70
     #9) * t305 * t513
      t718 = t20 * s * t1 * x1 * t513
      t720 = 0.2D1 * t708 * x2
      t721 = 0.1D1 - x1 + t303 - x2 + t633 - t634 - x3 + t316 - t317 + t
     #504 - t703 + t704 + t130 + t720
      t724 = t299 * t721 * t305 * t513
      t738 = 0.1D1 - t634 + t440 + 0.3D1 * t317 + t633 - x1 - t523 + x3 
     #+ t303 - 0.3D1 * t316 + 0.3D1 * t703 + t720 + t130 * t7 - 0.4D1 * 
     #t8 * z - 0.2D1 * t8 * x2 + 0.2D1 * t8 * t9 - 0.2D1 * t130 * x1
      t767 = 0.2D1 * t708 * x1 - t504 + t130 - 0.4D1 * t704 - 0.2D1 * t7
     #08 * t633 - 0.2D1 * t708 * t303 - 0.2D1 * t708 * t440 - 0.2D1 * t8
     # * t9 * x2 + 0.4D1 * t8 * t440 + x3 * t9 * t633 + 0.3D1 * t130 * t
     #303 - 0.2D1 * t130 * t7 * z - t130 * t9 * x1 + t130 * t7 * t9 - t7
     #09 + 0.2D1 * t8 - x2 + 0.2D1 * t708 * t634
      t769 = 0.1D1 / (t738 + t767)
      t770 = t5 * t304 * t769
      t771 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t300, x2, t527, x4)
      t773 = t771 * t45 * t641
      t775 = t770 * t773 / 0.32D2
      t776 = FJET(XB1, XB2, s, t702, t714, -t718, -t724, -t632, t775)
      t778 = t304 * t769
      t782 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t300, x2, t527, x4)
      t784 = t782 * t45 * t641
      t786 = t770 * t784 / 0.32D2
      t787 = FJET(XB1, XB2, s, t714, t702, -t724, -t718, -t632, t786)
      t792 = FJET(XB1, XB2, s, -t299, t297, 0.0D0, 0.0D0, 0.0D0, t381)
      t794 = FJET(XB1, XB2, s, -t439, 0.0D0, t436, 0.0D0, 0.0D0, t501)
      t796 = FJET(XB1, XB2, s, -t520, 0.0D0, t515, 0.0D0, 0.0D0, t554)
      t798 = FJET(XB1, XB2, s, -t623, 0.0D0, t626, t297, -t632, t690)
      t800 = FJET(XB1, XB2, s, -t718, -t724, t702, t714, -t632, t786)
      t805 = FJET(XB1, XB2, s, -t724, -t718, t714, t702, -t632, t775)
      t810 = t694 * t430 + t696 * t598 + t698 * t618 + t700 * t666 + t77
     #6 * t5 * t778 * t773 / 0.32D2 + t787 * t5 * t778 * t784 / 0.32D2 +
     # t792 * t381 + t794 * t501 + t796 * t554 + t798 * t690 + t800 * t5
     # * t778 * t784 / 0.32D2 + t805 * t5 * t778 * t773 / 0.32D2
      bbbbH5n1e0 = t693 + t810

      end function



      doubleprecision function bbbbH5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = cos(t8)
      t30 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t30)
      t36 = 0.1D1 / (-0.1D1 + 0.2D1 * t29 * t32 - x3)
      t38 = -t17 - t17 * t36
      t40 = 0.1D1 / x3
      t41 = t40 * t26
      t44 = t5 * t17
      t45 = 0.1D1 / x2
      t46 = t45 * t26
      t51 = t40 * t45
      t54 = x2 ** 2
      t55 = t54 * t10
      t58 = log(0.4D1 * t55 * t13)
      t66 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t72 = log(0.4D1 * t10 * t13)
      t75 = (-0.180D3 * lh - 0.90D2 * t72) * t5
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t84 = t72 ** 2
      t87 = (0.180D3 * t72 * lh + 0.180D3 * t80 - 0.30D2 * t82 + 0.45D2 
     #* t84) * t5
      t90 = x3 * t10
      t93 = log(0.4D1 * t90 * t13)
      t98 = log(-0.4D1 * t90 * t13 / t30)
      t100 = t93 + t98 * t36
      t106 = -0.1D1 - t36
      t111 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.5760D4 + 
     #t5 * t38 * t41 / 0.64D2 - t44 * t46 / 0.32D2 + t5 * t38 * t51 / 0.
     #64D2 - (0.90D2 * t5 * (t6 - t58 * t17) - t24) * t45 / 0.5760D4 - t
     #5 * t66 / 0.128D3 - t75 * t6 / 0.11520D5 - t87 * t17 / 0.11520D5 +
     # (0.90D2 * t44 * t100 + (0.90D2 * t5 * t6 - t24) * t106) * t40 / 0
     #.11520D5
      t112 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t111)
      t114 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t115 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t121 = 0.180D3 * t22 * t115
      t126 = -t115 - t115 * t36
      t130 = t5 * t115
      t144 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t160 = (0.90D2 * t5 * (-t114 + t16 * t115) + t121) * t26 / 0.5760D
     #4 + t5 * t126 * t41 / 0.64D2 - t130 * t46 / 0.32D2 + t5 * t126 * t
     #51 / 0.64D2 - (0.90D2 * t5 * (-t58 * t115 + t114) - t121) * t45 / 
     #0.5760D4 - t5 * t144 / 0.128D3 - t75 * t114 / 0.11520D5 - t87 * t1
     #15 / 0.11520D5 + (0.90D2 * t130 * t100 + (0.90D2 * t5 * t114 - t12
     #1) * t106) * t40 / 0.11520D5
      t161 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t160)
      t163 = t2 * x1
      t164 = -0.1D1 + x1
      t165 = t2 * t164
      t166 = -t164
      t167 = bbbbH52J2(s, XB1, XB2, z, lh, wd, t166, 0.0D0, 0.0D0, x4)
      t168 = x1 * z
      t169 = 0.1D1 - x1 + t168
      t170 = 0.1D1 / t169
      t172 = t164 ** 2
      t176 = log(0.4D1 * t11 * t13 * t170 * t172)
      t177 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t166, 0.0D0, 0.0D0, x4)
      t187 = x3 * x1
      t193 = Sqrt(-x3 * t169 * t30)
      t197 = 0.1D1 / (-0.2D1 * t187 * z + 0.2D1 * t187 - 0.1D1 + 0.2D1 *
     # t29 * t193 - x3)
      t206 = (0.90D2 * t5 * (t167 - t176 * t177) - 0.180D3 * t22 * t177)
     # * t26 / 0.5760D4 + t5 * (t177 + t177 * t197) * t41 / 0.64D2 + t5 
     #* t177 * t46 / 0.32D2
      t207 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t163, -t165, 0.0D0, t206)
      t209 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t166, 0.0D0, 0.0D0, x4)
      t211 = bbbbH51J2(s, XB1, XB2, z, lh, wd, t166, 0.0D0, 0.0D0, x4)
      t228 = (0.90D2 * t5 * (-t176 * t209 + t211) - 0.180D3 * t22 * t209
     #) * t26 / 0.5760D4 + t5 * (t209 + t209 * t197) * t41 / 0.64D2 + t5
     # * t209 * t46 / 0.32D2
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t165, t163, 0.0D0, t228)
      t231 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t160)
      t234 = x2 * t1 * s
      t235 = -0.1D1 + x2
      t237 = t235 * t1 * s
      t238 = x2 * z
      t240 = 0.1D1 / (0.1D1 + t238 - x2)
      t241 = t5 * t240
      t242 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t251 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t256 = log(-0.4D1 * t55 * t13 * t235)
      t257 = t256 * t240
      t268 = t241 * t242 * t45 * t26 / 0.32D2 + t241 * t242 * t40 * t45 
     #/ 0.64D2 - (0.90D2 * t5 * (-t240 * t251 + t257 * t242) + 0.180D3 *
     # t22 * t240 * t242) * t45 / 0.5760D4
      t269 = FJET(XB1, XB2, s, 0.0D0, t234, 0.0D0, -t237, 0.0D0, t268)
      t271 = x2 * x3
      t272 = t54 * x3
      t275 = Sqrt(x3 * t235 * t30)
      t276 = t29 * t275
      t278 = 0.2D1 * t276 * x2
      t281 = 0.1D1 / (0.1D1 - x3 + t271)
      t283 = t2 * (0.1D1 - x3 - x2 + t271 + t272 + t278) * t281
      t284 = 0.2D1 * t276
      t288 = t2 * x2 * (-0.1D1 + t271 + t284) * t281
      t293 = 0.1D1 / (-0.1D1 + t284 - t278 + 0.2D1 * t276 * t238 - t272 
     #- t238 + x2 - x3 + t271 + t272 * z)
      t294 = t5 * t293
      t295 = t271 * t281
      t296 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t295, x4)
      t298 = t296 * t40 * t45
      t300 = t294 * t298 / 0.64D2
      t301 = FJET(XB1, XB2, s, 0.0D0, t283, 0.0D0, -t288, 0.0D0, t300)
      t306 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t315 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t327 = t241 * t306 * t45 * t26 / 0.32D2 + t241 * t306 * t40 * t45 
     #/ 0.64D2 - (0.90D2 * t5 * (-t240 * t315 + t257 * t306) + 0.180D3 *
     # t22 * t240 * t306) * t45 / 0.5760D4
      t328 = FJET(XB1, XB2, s, 0.0D0, -t237, 0.0D0, t234, 0.0D0, t327)
      t330 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t295, x4)
      t332 = t330 * t40 * t45
      t334 = t294 * t332 / 0.64D2
      t335 = FJET(XB1, XB2, s, 0.0D0, -t288, 0.0D0, t283, 0.0D0, t334)
      t342 = t2 * t164 * x2 * t170
      t345 = t235 * s * t1 * t164
      t346 = t1 ** 2
      t351 = s * t346 * x2 * x1 * t164 * t170
      t352 = x2 * x1
      t355 = 0.1D1 / (-0.1D1 - t238 - t352 + x1 + x2 - t168 + t352 * z)
      t356 = t5 * t355
      t357 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t166, x2, 0.0D0, x4)
      t360 = t169 * t45 * t26
      t362 = t356 * t357 * t360 / 0.32D2
      t363 = FJET(XB1, XB2, s, 0.0D0, -t342, t163, t345, -t351, t362)
      t367 = t357 * t169 * t46
      t370 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t111)
      t372 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t166, x2, 0.0D0, x4)
      t375 = t356 * t372 * t360 / 0.32D2
      t376 = FJET(XB1, XB2, s, t163, t345, 0.0D0, -t342, -t351, t375)
      t380 = t372 * t169 * t46
      t383 = FJET(XB1, XB2, s, t163, -t165, 0.0D0, 0.0D0, 0.0D0, t228)
      t385 = FJET(XB1, XB2, s, t234, 0.0D0, -t237, 0.0D0, 0.0D0, t327)
      t387 = FJET(XB1, XB2, s, t283, 0.0D0, -t288, 0.0D0, 0.0D0, t334)
      t392 = FJET(XB1, XB2, s, t345, t163, -t342, 0.0D0, -t351, t362)
      t397 = FJET(XB1, XB2, s, -t165, t163, 0.0D0, 0.0D0, 0.0D0, t206)
      t399 = FJET(XB1, XB2, s, -t237, 0.0D0, t234, 0.0D0, 0.0D0, t268)
      t401 = FJET(XB1, XB2, s, -t342, 0.0D0, t345, t163, -t351, t375)
      t406 = FJET(XB1, XB2, s, -t288, 0.0D0, t283, 0.0D0, 0.0D0, t300)
      bbbbH5n1em1 = t112 * t111 + t161 * t160 + t207 * t206 + t229 * t22
     #8 + t231 * t160 + t269 * t268 + t301 * t5 * t293 * t298 / 0.64D2 +
     # t328 * t327 + t335 * t5 * t293 * t332 / 0.64D2 + t363 * t5 * t355
     # * t367 / 0.32D2 + t370 * t111 + t376 * t5 * t355 * t380 / 0.32D2 
     #+ t383 * t228 + t385 * t327 + t387 * t5 * t293 * t332 / 0.64D2 + t
     #392 * t5 * t355 * t367 / 0.32D2 + t397 * t206 + t399 * t268 + t401
     # * t5 * t355 * t380 / 0.32D2 + t406 * t5 * t293 * t298 / 0.64D2

      end function



      doubleprecision function bbbbH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x2
      t14 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t18 = z ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t28 = (-0.180D3 * lh - 0.90D2 * t25) * t5
      t31 = cos(t20)
      t34 = Sqrt(-x3 * (-0.1D1 + x3))
      t41 = (-0.1D1 - 0.1D1 / (-0.1D1 + 0.2D1 * t31 * t34 - x3)) / x3
      t44 = -t7 * t8 / 0.64D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.128D3 -
     # t28 * t6 / 0.11520D5 + t7 * t41 / 0.128D3
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t44)
      t47 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t48 = t5 * t47
      t53 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t60 = -t48 * t8 / 0.64D2 - t48 * t11 / 0.64D2 - t5 * t53 / 0.128D3
     # - t28 * t47 / 0.11520D5 + t48 * t41 / 0.128D3
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t60)
      t63 = t2 * x1
      t64 = -0.1D1 + x1
      t65 = t2 * t64
      t66 = -t64
      t67 = bbbbH52J1(s, XB1, XB2, z, lh, wd, t66, 0.0D0, 0.0D0, x4)
      t70 = t5 * t67 * t8 / 0.64D2
      t71 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t63, -t65, 0.0D0, t70)
      t73 = t67 * t8
      t76 = bbbbH51J1(s, XB1, XB2, z, lh, wd, t66, 0.0D0, 0.0D0, x4)
      t79 = t5 * t76 * t8 / 0.64D2
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t65, t63, 0.0D0, t79)
      t82 = t76 * t8
      t85 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t60)
      t88 = x2 * t1 * s
      t91 = (-0.1D1 + x2) * t1 * s
      t94 = 0.1D1 / (0.1D1 + x2 * z - x2)
      t95 = t5 * t94
      t96 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t99 = t95 * t96 * t11 / 0.64D2
      t100 = FJET(XB1, XB2, s, 0.0D0, t88, 0.0D0, -t91, 0.0D0, t99)
      t103 = t94 * t96 * t11
      t106 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t109 = t95 * t106 * t11 / 0.64D2
      t110 = FJET(XB1, XB2, s, 0.0D0, -t91, 0.0D0, t88, 0.0D0, t109)
      t113 = t94 * t106 * t11
      t116 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t44)
      t118 = FJET(XB1, XB2, s, t63, -t65, 0.0D0, 0.0D0, 0.0D0, t79)
      t122 = FJET(XB1, XB2, s, t88, 0.0D0, -t91, 0.0D0, 0.0D0, t109)
      t126 = FJET(XB1, XB2, s, -t65, t63, 0.0D0, 0.0D0, 0.0D0, t70)
      t130 = FJET(XB1, XB2, s, -t91, 0.0D0, t88, 0.0D0, 0.0D0, t99)
      bbbbH5n1em2 = t45 * t44 + t61 * t60 + t71 * t5 * t73 / 0.64D2 + t8
     #0 * t5 * t82 / 0.64D2 + t85 * t60 + t100 * t5 * t103 / 0.64D2 + t1
     #10 * t5 * t113 / 0.64D2 + t116 * t44 + t118 * t5 * t82 / 0.64D2 + 
     #t122 * t5 * t113 / 0.64D2 + t126 * t5 * t73 / 0.64D2 + t130 * t5 *
     # t103 / 0.64D2

      end function



      doubleprecision function bbbbH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t8 = t5 * t6 / 0.128D3
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t12 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t14 = t5 * t12 / 0.128D3
      t15 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t14)
      t18 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t14)
      t21 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      bbbbH5n1em3 = -t9 * t5 * t6 / 0.128D3 - t15 * t5 * t12 / 0.128D3 -
     # t18 * t5 * t12 / 0.128D3 - t21 * t5 * t6 / 0.128D3

      end function



      doubleprecision function bbbbH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      bbbbH5n1em4 = 0.0D0

      end function


      doubleprecision function bbbbH5n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t18 = log(0.4D1 * t16)
      t19 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = t18 ** 2
      t22 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t28 = lh ** 2
      t29 = 0.180D3 * t28
      t30 = 0.3141592653589793D1 ** 2
      t31 = 0.30D2 * t30
      t32 = t29 - t31
      t33 = t32 * t5
      t40 = t21 * t18
      t47 = 0.60D2 * lh * t30
      t49 = 0.120D3 * t28 * lh
      t50 = t47 - 0.2884936567583026D3 - t49
      t51 = t50 * t5
      t52 = t51 * t22
      t54 = 0.1D1 / x1
      t57 = x3 * t8
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
      t79 = 0.1D1 / (-0.1D1 - t72 + 0.2D1 * t73 * t75 + x3)
      t85 = t68 ** 2
      t91 = t61 ** 2
      t97 = t22 * t79
      t101 = 0.1D1 / x3
      t105 = x2 ** 2
      t106 = x3 * t105
      t107 = t106 * t8
      t108 = -0.1D1 + x2
      t109 = t58 * t108
      t112 = log(-0.4D1 * t107 * t109)
      t113 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t117 = log(0.4D1 * t106 * t16)
      t121 = log(-0.4D1 * t107 * t65)
      t125 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t129 = t113 - t97 - t22
      t134 = 0.1D1 / x2
      t135 = t134 * t54
      t138 = t105 * t8
      t141 = log(0.4D1 * t138 * t58)
      t145 = log(-0.4D1 * t138 * t109)
      t150 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t152 = t145 ** 2
      t156 = t141 ** 2
      t162 = -t22 + t113
      t169 = log(0.4D1 * t58)
      t172 = t169 ** 2
      t175 = (0.180D3 * t169 * lh + t29 - t31 + 0.45D2 * t172) * t5
      t181 = t172 * t169
      t184 = (-t169 * t32 - 0.90D2 * t172 * lh + t47 - 0.288493656758302
     #6D3 - t49 - 0.15D2 * t181) * t5
      t187 = t172 ** 2
      t191 = t30 ** 2
      t192 = t28 ** 2
      t201 = (0.15D2 / 0.4D1 * t187 - t169 * t50 + 0.5769873135166051D3 
     #* lh + t191 + 0.60D2 * t192 - 0.60D2 * t28 * t30 + t172 * t32 / 0.
     #2D1 + 0.30D2 * t181 * lh) * t5
      t209 = x3 * t15
      t213 = log(-0.4D1 * t209 * t11 * t64)
      t214 = t213 ** 2
      t218 = log(0.4D1 * t209 * t11)
      t219 = t218 ** 2
      t221 = -t214 * t79 / 0.2D1 - t219 / 0.2D1
      t230 = t218 + t213 * t79
      t237 = t214 * t213 * t79 / 0.6D1 + t219 * t218 / 0.6D1
      t244 = -0.1D1 - t79
      t249 = t15 * t105
      t252 = log(0.4D1 * t249 * t11)
      t254 = t252 ** 2
      t257 = t11 * t108
      t260 = log(-0.4D1 * t249 * t257)
      t262 = t260 ** 2
      t275 = t254 * t252
      t281 = t262 * t260
      t293 = log(0.4D1 * t106 * t58)
      t297 = log(-0.4D1 * t106 * t65)
      t303 = log(-0.4D1 * t106 * t109)
      t308 = t293 ** 2
      t312 = t297 ** 2
      t319 = t303 ** 2
      t330 = (-0.180D3 * t6 * (-t7 + t18 * t19 - t21 * t22 / 0.2D1) + t3
     #3 * (t18 * t22 - t19) + 0.90D2 * t5 * (t18 * t7 - t21 * t19 / 0.2D
     #1 + t40 * t22 / 0.6D1) - t52) * t54 / 0.5760D4 + (-0.180D3 * t6 * 
     #(t61 * t22 - t19 - (t19 - t68 * t22) * t79) + 0.90D2 * t5 * (-(t7 
     #- t68 * t19 + t85 * t22 / 0.2D1) * t79 - t7 + t61 * t19 - t91 * t2
     #2 / 0.2D1) + t33 * (-t22 - t97)) * t101 * t54 / 0.5760D4 + (0.90D2
     # * t5 * (-t112 * t113 + t117 * t22 - t19 - (t19 - t121 * t22) * t7
     #9 + t125) - 0.180D3 * t6 * t129) * t101 * t135 / 0.2880D4 + (-0.18
     #0D3 * t6 * (t125 + t141 * t22 - t19 - t145 * t113) + 0.90D2 * t5 *
     # (t150 - t145 * t125 + t152 * t113 / 0.2D1 - t7 + t141 * t19 - t15
     #6 * t22 / 0.2D1) + t33 * t162) * t134 * t54 / 0.2880D4 - t175 * t7
     # / 0.11520D5 - t184 * t19 / 0.11520D5 - t201 * t22 / 0.11520D5 + (
     #(0.90D2 * t5 * t19 - 0.180D3 * t6 * t22) * t221 + (0.90D2 * t5 * t
     #7 - 0.180D3 * t6 * t19 + t33 * t22) * t230 + 0.90D2 * t5 * t22 * t
     #237 + (-0.180D3 * t6 * t7 + t33 * t19 + t52) * t244) * t101 / 0.11
     #520D5 + (-0.180D3 * t6 * (-t7 + t252 * t19 - t254 * t22 / 0.2D1 + 
     #t150 - t260 * t125 + t262 * t113 / 0.2D1) + t33 * (-t19 - t260 * t
     #113 + t125 + t252 * t22) + 0.90D2 * t5 * (t252 * t7 - t254 * t19 /
     # 0.2D1 + t275 * t22 / 0.6D1 - t260 * t150 + t262 * t125 / 0.2D1 - 
     #t281 * t113 / 0.6D1) + t51 * t162) * t134 / 0.5760D4 + (-0.180D3 *
     # t6 * (t293 * t22 - (t19 - t297 * t22) * t79 - t303 * t113 + t125 
     #- t19) + 0.90D2 * t5 * (-t308 * t22 / 0.2D1 - (t7 - t297 * t19 + t
     #312 * t22 / 0.2D1) * t79 + t293 * t19 + t150 - t303 * t125 - t7 + 
     #t319 * t113 / 0.2D1) + t33 * t129) * t101 * t134 / 0.5760D4
      t331 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t330)
      t333 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t334 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t336 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t353 = t51 * t336
      t375 = t336 * t79
      t386 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t388 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t392 = -t336 + t386 - t375
      t408 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t414 = -t336 + t386
      t507 = (-0.180D3 * t6 * (-t333 + t18 * t334 - t21 * t336 / 0.2D1) 
     #+ t33 * (-t334 + t18 * t336) + 0.90D2 * t5 * (t18 * t333 - t21 * t
     #334 / 0.2D1 + t40 * t336 / 0.6D1) - t353) * t54 / 0.5760D4 + (-0.1
     #80D3 * t6 * (-(t334 - t68 * t336) * t79 - t334 + t61 * t336) + 0.9
     #0D2 * t5 * (t61 * t334 - (t333 - t68 * t334 + t85 * t336 / 0.2D1) 
     #* t79 - t333 - t91 * t336 / 0.2D1) + t33 * (-t375 - t336)) * t101 
     #* t54 / 0.5760D4 + (0.90D2 * t5 * (-t334 - (t334 - t121 * t336) * 
     #t79 + t117 * t336 - t112 * t386 + t388) - 0.180D3 * t6 * t392) * t
     #101 * t135 / 0.2880D4 + (-0.180D3 * t6 * (-t334 + t141 * t336 - t1
     #45 * t386 + t388) + 0.90D2 * t5 * (-t145 * t388 + t141 * t334 + t1
     #52 * t386 / 0.2D1 - t333 + t408 - t156 * t336 / 0.2D1) + t33 * t41
     #4) * t134 * t54 / 0.2880D4 - t175 * t333 / 0.11520D5 - t184 * t334
     # / 0.11520D5 - t201 * t336 / 0.11520D5 + ((0.90D2 * t5 * t334 - 0.
     #180D3 * t6 * t336) * t221 + (0.90D2 * t5 * t333 - 0.180D3 * t6 * t
     #334 + t33 * t336) * t230 + 0.90D2 * t5 * t336 * t237 + (-0.180D3 *
     # t6 * t333 + t33 * t334 + t353) * t244) * t101 / 0.11520D5 + (-0.1
     #80D3 * t6 * (t252 * t334 - t260 * t388 - t254 * t336 / 0.2D1 + t40
     #8 - t333 + t262 * t386 / 0.2D1) + t33 * (t388 - t260 * t386 + t252
     # * t336 - t334) + 0.90D2 * t5 * (t252 * t333 - t254 * t334 / 0.2D1
     # + t275 * t336 / 0.6D1 - t260 * t408 + t262 * t388 / 0.2D1 - t281 
     #* t386 / 0.6D1) + t51 * t414) * t134 / 0.5760D4 + (-0.180D3 * t6 *
     # (-t334 + t293 * t336 + t388 - (t334 - t297 * t336) * t79 - t303 *
     # t386) + 0.90D2 * t5 * (-t333 + t293 * t334 - (t333 - t297 * t334 
     #+ t312 * t336 / 0.2D1) * t79 - t308 * t336 / 0.2D1 + t319 * t386 /
     # 0.2D1 - t303 * t388 + t408) + t33 * t392) * t101 * t134 / 0.5760D
     #4
      t508 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t507)
      t510 = t2 * x1
      t511 = -0.1D1 + x1
      t512 = t2 * t511
      t513 = bbbbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t514 = 0.1D1 / t13
      t515 = x1 * z
      t516 = -z - x1 + t515
      t517 = 0.1D1 / t516
      t518 = t514 * t517
      t519 = t511 ** 2
      t520 = t518 * t519
      t523 = log(-0.4D1 * t12 * t520)
      t524 = bbbbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t526 = t523 ** 2
      t527 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t537 = t526 * t523
      t549 = t57 * t11
      t552 = log(-0.4D1 * t549 * t520)
      t555 = t518 * t519 * t64
      t558 = log(0.4D1 * t549 * t555)
      t561 = x3 * x1
      t562 = t561 * z
      t565 = x3 * t516
      t567 = Sqrt(t565 * t63)
      t571 = 0.1D1 / (0.2D1 * t562 - 0.2D1 * t561 - t72 - 0.1D1 + 0.2D1 
     #* t73 * t567 + x3)
      t577 = t552 ** 2
      t581 = t558 ** 2
      t590 = t527 * t571 + t527
      t596 = t106 * t12
      t599 = log(0.4D1 * t596 * t555)
      t604 = t517 * t519
      t608 = log(-0.4D1 * t107 * t11 * t514 * t604)
      t619 = t138 * t11
      t622 = log(-0.4D1 * t619 * t520)
      t628 = t622 ** 2
      t639 = (-0.180D3 * t6 * (t513 - t523 * t524 + t526 * t527 / 0.2D1)
     # + t33 * (t524 - t523 * t527) + 0.90D2 * t5 * (-t523 * t513 - t537
     # * t527 / 0.6D1 + t526 * t524 / 0.2D1) + t51 * t527) * t54 / 0.576
     #0D4 + (-0.180D3 * t6 * (-t552 * t527 + t524 + (t524 - t558 * t527)
     # * t571) + 0.90D2 * t5 * (t513 - t552 * t524 + t577 * t527 / 0.2D1
     # + (t513 - t558 * t524 + t581 * t527 / 0.2D1) * t571) + t33 * t590
     #) * t101 * t54 / 0.5760D4 + (0.90D2 * t5 * ((t524 - t599 * t527) *
     # t571 + t524 - t608 * t527) - 0.180D3 * t6 * t590) * t101 * t135 /
     # 0.2880D4 + (-0.180D3 * t6 * (t524 - t622 * t527) + 0.90D2 * t5 * 
     #(-t622 * t524 + t513 + t628 * t527 / 0.2D1) + t33 * t527) * t134 *
     # t54 / 0.2880D4
      t640 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t510, -t512, 0.0D0, t639)
      t642 = bbbbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t643 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t646 = bbbbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t685 = t643 * t571 + t643
      t719 = (-0.180D3 * t6 * (t642 + t526 * t643 / 0.2D1 - t523 * t646)
     # + t33 * (-t523 * t643 + t646) + 0.90D2 * t5 * (-t523 * t642 - t53
     #7 * t643 / 0.6D1 + t526 * t646 / 0.2D1) + t51 * t643) * t54 / 0.57
     #60D4 + (-0.180D3 * t6 * ((t646 - t558 * t643) * t571 + t646 - t552
     # * t643) + 0.90D2 * t5 * (t642 + t577 * t643 / 0.2D1 - t552 * t646
     # + (t642 - t558 * t646 + t581 * t643 / 0.2D1) * t571) + t33 * t685
     #) * t101 * t54 / 0.5760D4 + (0.90D2 * t5 * ((t646 - t599 * t643) *
     # t571 - t608 * t643 + t646) - 0.180D3 * t6 * t685) * t101 * t135 /
     # 0.2880D4 + (-0.180D3 * t6 * (t646 - t622 * t643) + 0.90D2 * t5 * 
     #(t642 + t628 * t643 / 0.2D1 - t622 * t646) + t33 * t643) * t134 * 
     #t54 / 0.2880D4
      t720 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t512, t510, 0.0D0, t719)
      t722 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t507)
      t724 = x2 * x3
      t725 = 0.1D1 - x3 + t724
      t726 = 0.1D1 / t725
      t727 = t724 * t726
      t728 = t2 * t727
      t731 = t63 * t1 * s * t726
      t733 = t725 ** 2
      t734 = 0.1D1 / t733
      t735 = t63 * t734
      t739 = log(0.4D1 * t596 * t15 * t108 * t735)
      t740 = t724 * z
      t741 = t108 * t63
      t743 = Sqrt(t71 * t741)
      t747 = 0.1D1 / (t740 - t72 - 0.1D1 + 0.2D1 * t73 * t743 + x3)
      t748 = t739 * t747
      t749 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t727, x4)
      t751 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t727, x4)
      t752 = t747 * t751
      t756 = t747 * t749
      t767 = log(0.4D1 * t106 * t15 * t257 * t735)
      t768 = t767 * t747
      t773 = t767 ** 2
      t774 = t773 * t747
      t777 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t727, x4)
      t788 = (0.90D2 * t5 * (-t748 * t749 + t752) - 0.180D3 * t6 * t756)
     # * t101 * t135 / 0.2880D4 + (-0.180D3 * t6 * (-t768 * t749 + t752)
     # + 0.90D2 * t5 * (t774 * t749 / 0.2D1 + t747 * t777 - t768 * t751)
     # + t33 * t756) * t101 * t134 / 0.5760D4
      t789 = FJET(XB1, XB2, s, 0.0D0, t728, 0.0D0, -t731, 0.0D0, t788)
      t791 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t727, x4)
      t793 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t727, x4)
      t794 = t747 * t793
      t798 = t747 * t791
      t809 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t727, x4)
      t822 = (0.90D2 * t5 * (-t748 * t791 + t794) - 0.180D3 * t6 * t798)
     # * t101 * t135 / 0.2880D4 + (-0.180D3 * t6 * (t794 - t768 * t791) 
     #+ 0.90D2 * t5 * (t747 * t809 - t768 * t793 + t774 * t791 / 0.2D1) 
     #+ t33 * t798) * t101 * t134 / 0.5760D4
      t823 = FJET(XB1, XB2, s, 0.0D0, -t731, 0.0D0, t728, 0.0D0, t822)
      t825 = x2 * x1
      t827 = t2 * t825 * t517
      t830 = t108 * s * t1 * x1
      t831 = t1 ** 2
      t836 = s * t831 * x2 * x1 * t511 * t517
      t837 = t825 * z
      t839 = 0.1D1 / (z - t825 + x1 - t515 + t837)
      t840 = t516 * t839
      t841 = bbbbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t842 = t840 * t841
      t844 = t518 * t519 * t108
      t847 = log(0.4D1 * t596 * t844)
      t848 = t847 * t516
      t849 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t850 = t839 * t849
      t855 = t840 * t849
      t863 = log(0.4D1 * t619 * t844)
      t864 = t863 * t516
      t869 = t863 ** 2
      t870 = t869 * t516
      t873 = bbbbH52J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t885 = (0.90D2 * t5 * (t842 - t848 * t850) - 0.180D3 * t6 * t855) 
     #* t101 * t135 / 0.2880D4 + (-0.180D3 * t6 * (-t864 * t850 + t842) 
     #+ 0.90D2 * t5 * (t870 * t850 / 0.2D1 + t840 * t873 - t864 * t839 *
     # t841) + t33 * t855) * t134 * t54 / 0.2880D4
      t886 = FJET(XB1, XB2, s, 0.0D0, -t827, -t512, -t830, t836, t885)
      t888 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t330)
      t890 = FJET(XB1, XB2, s, t510, -t512, 0.0D0, 0.0D0, 0.0D0, t719)
      t892 = FJET(XB1, XB2, s, t728, 0.0D0, -t731, 0.0D0, 0.0D0, t822)
      t897 = t63 * s * t1 * t511 * t726
      t898 = x2 * z
      t899 = t724 * x1
      t900 = t724 * t515
      t902 = Sqrt(-t565 * t741)
      t903 = t73 * t902
      t906 = z + x1 - t515 - t898 - t825 + t837 - t71 - t561 + t562 + t7
     #40 + t899 - t900 + t106 + 0.2D1 * t903 * x2
      t909 = t510 * t906 * t517 * t726
      t910 = t512 * t727
      t916 = t510 * x2 * (-t71 - t561 + t562 + t740 + t899 - t900 - 0.1D
     #1 + x3 + 0.2D1 * t903) * t517 * t726
      t924 = x3 * t13
      t937 = 0.2D1 * t903 * t837 + 0.2D1 * t903 * x1 + t724 * t13 - t107
     # + 0.2D1 * t903 * z + 0.4D1 * t924 * x1 + 0.4D1 * t57 * z + 0.2D1 
     #* t57 * x2 - 0.2D1 * t57 * t13 - 0.5D1 * t562 - t899 - t837 - z + 
     #0.4D1 * t900 - 0.2D1 * t903 * t515
      t957 = -0.3D1 * t924 * t825 - 0.4D1 * t57 * t898 + 0.2D1 * t57 * t
     #13 * x2 - t106 * t515 - 0.2D1 * t903 * t825 + t106 * t13 * x1 + 0.
     #2D1 * t106 * t8 * z - t106 * t8 * t13 + t561 + t71 + t825 + t515 -
     # 0.2D1 * t57 - 0.2D1 * t924 - x1
      t959 = 0.1D1 / (t937 + t957)
      t960 = t516 * t959
      t961 = bbbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t727, x4)
      t969 = log(-0.4D1 * t106 * t12 * t514 * t604 * t741 * t734)
      t970 = t969 * t516
      t971 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t727, x4)
      t980 = 0.90D2 * t5 * (t960 * t961 - t970 * t959 * t971) - 0.180D3 
     #* t6 * t960 * t971
      t983 = t980 * t101 * t135 / 0.2880D4
      t984 = FJET(XB1, XB2, s, t897, -t909, -t910, t916, t836, t983)
      t987 = t101 * t134 * t54
      t990 = FJET(XB1, XB2, s, t916, -t910, -t909, t897, t836, t983)
      t994 = FJET(XB1, XB2, s, -t512, t510, 0.0D0, 0.0D0, 0.0D0, t639)
      t996 = bbbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t997 = t840 * t996
      t998 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t999 = t839 * t998
      t1004 = t840 * t998
      t1014 = bbbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t1028 = (0.90D2 * t5 * (t997 - t848 * t999) - 0.180D3 * t6 * t1004
     #) * t101 * t135 / 0.2880D4 + (-0.180D3 * t6 * (t997 - t864 * t999)
     # + 0.90D2 * t5 * (t840 * t1014 + t870 * t999 / 0.2D1 - t864 * t839
     # * t996) + t33 * t1004) * t134 * t54 / 0.2880D4
      t1029 = FJET(XB1, XB2, s, -t512, -t830, 0.0D0, -t827, t836, t1028)
      t1031 = FJET(XB1, XB2, s, -t830, -t512, -t827, 0.0D0, t836, t885)
      t1033 = FJET(XB1, XB2, s, -t731, 0.0D0, t728, 0.0D0, 0.0D0, t788)
      t1035 = FJET(XB1, XB2, s, -t827, 0.0D0, -t830, -t512, t836, t1028)
      t1037 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, t727, x4)
      t1040 = bbbbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, t727, x4)
      t1048 = 0.90D2 * t5 * (-t970 * t959 * t1037 + t960 * t1040) - 0.18
     #0D3 * t6 * t960 * t1037
      t1051 = t1048 * t101 * t135 / 0.2880D4
      t1052 = FJET(XB1, XB2, s, -t909, t897, t916, -t910, t836, t1051)
      t1056 = FJET(XB1, XB2, s, -t910, t916, t897, -t909, t836, t1051)
      bbbbH5n2e1 = t331 * t330 + t508 * t507 + t640 * t639 + t720 * t719
     # + t722 * t507 + t789 * t788 + t823 * t822 + t886 * t885 + t888 * 
     #t330 + t890 * t719 + t892 * t822 + t984 * t980 * t987 / 0.2880D4 +
     # t990 * t980 * t987 / 0.2880D4 + t994 * t639 + t1029 * t1028 + t10
     #31 * t885 + t1033 * t788 + t1035 * t1028 + t1052 * t1048 * t987 / 
     #0.2880D4 + t1056 * t1048 * t987 / 0.2880D4

      end function



      doubleprecision function bbbbH5n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t20 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t23 = t14 * t22
      t26 = log(-0.4D1 * t7 * t23)
      t29 = x3 * z
      t30 = 0.2D1 * t29
      t31 = cos(t11)
      t33 = Sqrt(-t29 * t21)
      t37 = 0.1D1 / (-0.1D1 - t30 + 0.2D1 * t31 * t33 + x3)
      t42 = lh * t5
      t43 = t18 * t37
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t53 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t54 = t53 - t43 - t18
      t56 = 0.1D1 / x2
      t58 = t48 * t56 * t50
      t61 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t62 = x2 ** 2
      t63 = t62 * t6
      t66 = log(0.4D1 * t63 * t14)
      t68 = -0.1D1 + x2
      t69 = t14 * t68
      t72 = log(-0.4D1 * t63 * t69)
      t77 = -t18 + t53
      t84 = t6 * t13
      t87 = log(0.4D1 * t84 * t10)
      t92 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t94 = t87 ** 2
      t100 = lh ** 2
      t101 = 0.180D3 * t100
      t102 = 0.3141592653589793D1 ** 2
      t103 = 0.30D2 * t102
      t104 = t101 - t103
      t105 = t104 * t5
      t106 = t105 * t18
      t115 = x3 * t10
      t118 = log(0.4D1 * t115 * t13)
      t122 = log(-0.4D1 * t115 * t13 * t22)
      t124 = t118 + t122 * t37
      t127 = t122 ** 2
      t129 = t118 ** 2
      t131 = -t127 * t37 / 0.2D1 - t129 / 0.2D1
      t139 = -0.1D1 - t37
      t144 = t62 * x3
      t147 = log(0.4D1 * t144 * t14)
      t151 = log(-0.4D1 * t144 * t23)
      t157 = log(-0.4D1 * t144 * t69)
      t168 = t10 * t62
      t169 = t13 * t68
      t172 = log(-0.4D1 * t168 * t169)
      t176 = log(0.4D1 * t168 * t13)
      t182 = t176 ** 2
      t185 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t187 = t172 ** 2
      t199 = log(0.4D1 * t14)
      t202 = (-0.180D3 * lh - 0.90D2 * t199) * t5
      t207 = t199 ** 2
      t210 = (0.180D3 * t199 * lh + t101 - t103 + 0.45D2 * t207) * t5
      t223 = (-t199 * t104 - 0.90D2 * t207 * lh + 0.60D2 * lh * t102 - 0
     #.2884936567583026D3 - 0.120D3 * t100 * lh - 0.15D2 * t207 * t199) 
     #* t5
      t226 = (0.90D2 * t5 * (t17 * t18 - t20 - (t20 - t26 * t18) * t37) 
     #- 0.180D3 * t42 * (-t18 - t43)) * t48 * t50 / 0.5760D4 + t5 * t54 
     #* t58 / 0.32D2 + (0.90D2 * t5 * (t61 + t66 * t18 - t20 - t72 * t53
     #) - 0.180D3 * t42 * t77) * t56 * t50 / 0.2880D4 + (-0.180D3 * t42 
     #* (t87 * t18 - t20) + 0.90D2 * t5 * (-t92 + t87 * t20 - t94 * t18 
     #/ 0.2D1) - t106) * t50 / 0.5760D4 + ((0.90D2 * t5 * t20 - 0.180D3 
     #* t42 * t18) * t124 + 0.90D2 * t5 * t18 * t131 + (0.90D2 * t5 * t9
     #2 - 0.180D3 * t42 * t20 + t106) * t139) * t48 / 0.11520D5 + (0.90D
     #2 * t5 * (t147 * t18 - (t20 - t151 * t18) * t37 - t157 * t53 + t61
     # - t20) - 0.180D3 * t42 * t54) * t48 * t56 / 0.5760D4 + (-0.180D3 
     #* t42 * (-t20 - t172 * t53 + t61 + t176 * t18) + 0.90D2 * t5 * (-t
     #92 + t176 * t20 - t182 * t18 / 0.2D1 + t185 - t172 * t61 + t187 * 
     #t53 / 0.2D1) + t105 * t77) * t56 / 0.5760D4 - t202 * t92 / 0.11520
     #D5 - t210 * t20 / 0.11520D5 - t223 * t18 / 0.11520D5
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t226)
      t229 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t230 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t238 = t230 * t37
      t246 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t247 = -t230 + t246 - t238
      t253 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t257 = -t230 + t246
      t268 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t275 = t105 * t230
      t320 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t336 = (0.90D2 * t5 * (-(t229 - t26 * t230) * t37 - t229 + t17 * t
     #230) - 0.180D3 * t42 * (-t238 - t230)) * t48 * t50 / 0.5760D4 + t5
     # * t247 * t58 / 0.32D2 + (0.90D2 * t5 * (-t229 + t66 * t230 - t72 
     #* t246 + t253) - 0.180D3 * t42 * t257) * t56 * t50 / 0.2880D4 + (-
     #0.180D3 * t42 * (-t229 + t87 * t230) + 0.90D2 * t5 * (-t268 + t87 
     #* t229 - t94 * t230 / 0.2D1) - t275) * t50 / 0.5760D4 + ((0.90D2 *
     # t5 * t229 - 0.180D3 * t42 * t230) * t124 + 0.90D2 * t5 * t230 * t
     #131 + (0.90D2 * t5 * t268 - 0.180D3 * t42 * t229 + t275) * t139) *
     # t48 / 0.11520D5 + (0.90D2 * t5 * (-t229 + t147 * t230 + t253 - (t
     #229 - t151 * t230) * t37 - t157 * t246) - 0.180D3 * t42 * t247) * 
     #t48 * t56 / 0.5760D4 + (-0.180D3 * t42 * (t253 - t172 * t246 + t17
     #6 * t230 - t229) + 0.90D2 * t5 * (t176 * t229 - t172 * t253 - t182
     # * t230 / 0.2D1 + t320 - t268 + t187 * t246 / 0.2D1) + t105 * t257
     #) * t56 / 0.5760D4 - t202 * t268 / 0.11520D5 - t210 * t229 / 0.115
     #20D5 - t223 * t230 / 0.11520D5
      t337 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t336)
      t339 = t2 * x1
      t340 = -0.1D1 + x1
      t341 = t2 * t340
      t342 = t7 * t13
      t344 = x1 * z
      t345 = -z - x1 + t344
      t346 = 0.1D1 / t345
      t347 = 0.1D1 / t8 * t346
      t348 = t340 ** 2
      t349 = t347 * t348
      t352 = log(-0.4D1 * t342 * t349)
      t353 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t355 = bbbbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t360 = log(0.4D1 * t342 * t347 * t348 * t22)
      t363 = x3 * x1
      t364 = t363 * z
      t367 = x3 * t345
      t369 = Sqrt(t367 * t21)
      t373 = 0.1D1 / (0.2D1 * t364 - 0.2D1 * t363 - t30 - 0.1D1 + 0.2D1 
     #* t31 * t369 + x3)
      t379 = t353 * t373 + t353
      t389 = t63 * t13
      t392 = log(-0.4D1 * t389 * t349)
      t405 = log(-0.4D1 * t84 * t349)
      t410 = bbbbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t412 = t405 ** 2
      t422 = (0.90D2 * t5 * (-t352 * t353 + t355 + (t355 - t360 * t353) 
     #* t373) - 0.180D3 * t42 * t379) * t48 * t50 / 0.5760D4 + t5 * t379
     # * t58 / 0.32D2 + (0.90D2 * t5 * (t355 - t392 * t353) - 0.180D3 * 
     #t42 * t353) * t56 * t50 / 0.2880D4 + (-0.180D3 * t42 * (t355 - t40
     #5 * t353) + 0.90D2 * t5 * (t410 - t405 * t355 + t412 * t353 / 0.2D
     #1) + t105 * t353) * t50 / 0.5760D4
      t423 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t339, -t341, 0.0D0, t422)
      t425 = bbbbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t426 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t435 = t426 * t373 + t426
      t459 = bbbbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t470 = (0.90D2 * t5 * ((t425 - t360 * t426) * t373 + t425 - t352 *
     # t426) - 0.180D3 * t42 * t435) * t48 * t50 / 0.5760D4 + t5 * t435 
     #* t58 / 0.32D2 + (0.90D2 * t5 * (t425 - t392 * t426) - 0.180D3 * t
     #42 * t426) * t56 * t50 / 0.2880D4 + (-0.180D3 * t42 * (-t405 * t42
     #6 + t425) + 0.90D2 * t5 * (t459 + t412 * t426 / 0.2D1 - t405 * t42
     #5) + t105 * t426) * t50 / 0.5760D4
      t471 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t341, t339, 0.0D0, t470)
      t473 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t336)
      t475 = x2 * x3
      t476 = 0.1D1 - x3 + t475
      t477 = 0.1D1 / t476
      t478 = t475 * t477
      t479 = t2 * t478
      t482 = t21 * t1 * s * t477
      t483 = t475 * z
      t484 = t68 * t21
      t486 = Sqrt(t29 * t484)
      t490 = 0.1D1 / (t483 - t30 - 0.1D1 + 0.2D1 * t31 * t486 + x3)
      t491 = t5 * t490
      t492 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t478, x4)
      t497 = t476 ** 2
      t503 = log(0.4D1 * t144 * t10 * t169 * t21 / t497)
      t504 = t503 * t490
      t506 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t478, x4)
      t518 = t491 * t492 * t58 / 0.32D2 + (0.90D2 * t5 * (-t504 * t492 +
     # t490 * t506) - 0.180D3 * t42 * t490 * t492) * t48 * t56 / 0.5760D
     #4
      t519 = FJET(XB1, XB2, s, 0.0D0, t479, 0.0D0, -t482, 0.0D0, t518)
      t521 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t478, x4)
      t525 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t478, x4)
      t538 = t491 * t521 * t58 / 0.32D2 + (0.90D2 * t5 * (t490 * t525 - 
     #t504 * t521) - 0.180D3 * t42 * t490 * t521) * t48 * t56 / 0.5760D4
      t539 = FJET(XB1, XB2, s, 0.0D0, -t482, 0.0D0, t479, 0.0D0, t538)
      t541 = x2 * x1
      t543 = t2 * t541 * t346
      t546 = t68 * s * t1 * x1
      t547 = t1 ** 2
      t552 = s * t547 * x2 * x1 * t340 * t346
      t553 = t5 * t345
      t554 = t541 * z
      t556 = 0.1D1 / (z - t541 + x1 - t344 + t554)
      t557 = t553 * t556
      t558 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t560 = t56 * t50
      t568 = log(0.4D1 * t389 * t347 * t348 * t68)
      t569 = t568 * t345
      t572 = t345 * t556
      t573 = bbbbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t585 = t557 * t558 * t48 * t560 / 0.32D2 + (0.90D2 * t5 * (-t569 *
     # t556 * t558 + t572 * t573) - 0.180D3 * t42 * t572 * t558) * t56 *
     # t50 / 0.2880D4
      t586 = FJET(XB1, XB2, s, 0.0D0, -t543, -t341, -t546, t552, t585)
      t588 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t226)
      t590 = FJET(XB1, XB2, s, t339, -t341, 0.0D0, 0.0D0, 0.0D0, t470)
      t592 = FJET(XB1, XB2, s, t479, 0.0D0, -t482, 0.0D0, 0.0D0, t538)
      t597 = t21 * s * t1 * t340 * t477
      t598 = x2 * z
      t599 = t475 * x1
      t600 = t475 * t344
      t602 = Sqrt(-t367 * t484)
      t603 = t31 * t602
      t606 = z + x1 - t344 - t598 - t541 + t554 - t29 - t363 + t364 + t4
     #83 + t599 - t600 + t144 + 0.2D1 * t603 * x2
      t609 = t339 * t606 * t346 * t477
      t610 = t341 * t478
      t616 = t339 * x2 * (-t29 - t363 + t364 + t483 + t599 - t600 - 0.1D
     #1 + x3 + 0.2D1 * t603) * t346 * t477
      t636 = x3 * t8
      t640 = -0.5D1 * t364 - 0.4D1 * t7 * t598 + 0.2D1 * t7 * t8 * x2 - 
     #t144 * t344 - 0.2D1 * t603 * t541 + t144 * t8 * x1 + 0.2D1 * t144 
     #* t6 * z - t144 * t6 * t8 + 0.4D1 * t600 - 0.2D1 * t603 * t344 - 0
     #.3D1 * t636 * t541 + t344 + t363 + t541 - 0.2D1 * t7
      t658 = -0.2D1 * t636 - t554 + 0.2D1 * t603 * t554 + t29 - z - x1 -
     # t599 + 0.2D1 * t603 * x1 + t475 * t8 - t144 * t6 + 0.2D1 * t603 *
     # z + 0.4D1 * t636 * x1 + 0.4D1 * t7 * z + 0.2D1 * t7 * x2 - 0.2D1 
     #* t7 * t8
      t660 = 0.1D1 / (t640 + t658)
      t661 = t553 * t660
      t662 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t478, x4)
      t664 = t662 * t48 * t560
      t666 = t661 * t664 / 0.32D2
      t667 = FJET(XB1, XB2, s, t597, -t609, -t610, t616, t552, t666)
      t669 = t345 * t660
      t673 = FJET(XB1, XB2, s, t616, -t610, -t609, t597, t552, t666)
      t678 = FJET(XB1, XB2, s, -t341, t339, 0.0D0, 0.0D0, 0.0D0, t422)
      t680 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t685 = bbbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t699 = t557 * t680 * t48 * t560 / 0.32D2 + (0.90D2 * t5 * (t572 * 
     #t685 - t569 * t556 * t680) - 0.180D3 * t42 * t572 * t680) * t56 * 
     #t50 / 0.2880D4
      t700 = FJET(XB1, XB2, s, -t341, -t546, 0.0D0, -t543, t552, t699)
      t702 = FJET(XB1, XB2, s, -t546, -t341, -t543, 0.0D0, t552, t585)
      t704 = FJET(XB1, XB2, s, -t482, 0.0D0, t479, 0.0D0, 0.0D0, t518)
      t706 = FJET(XB1, XB2, s, -t543, 0.0D0, -t546, -t341, t552, t699)
      t708 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, t478, x4)
      t710 = t708 * t48 * t560
      t712 = t661 * t710 / 0.32D2
      t713 = FJET(XB1, XB2, s, -t609, t597, t616, -t610, t552, t712)
      t718 = FJET(XB1, XB2, s, -t610, t616, t597, -t609, t552, t712)
      bbbbH5n2e0 = t227 * t226 + t337 * t336 + t423 * t422 + t471 * t470
     # + t473 * t336 + t518 * t519 + t539 * t538 + t586 * t585 + t588 * 
     #t226 + t590 * t470 + t592 * t538 + t667 * t5 * t669 * t664 / 0.32D
     #2 + t673 * t5 * t669 * t664 / 0.32D2 + t678 * t422 + t700 * t699 +
     # t702 * t585 + t704 * t518 + t706 * t699 + t713 * t5 * t669 * t710
     # / 0.32D2 + t718 * t5 * t669 * t710 / 0.32D2

      end function



      doubleprecision function bbbbH5n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t19 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t23 = lh * t5
      t25 = 0.180D3 * t23 * t17
      t27 = 0.1D1 / x1
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t7)
      t33 = -0.1D1 + x3
      t35 = Sqrt(-t30 * t33)
      t39 = 0.1D1 / (-0.1D1 - t31 + 0.2D1 * t32 * t35 + x3)
      t40 = t17 * t39
      t43 = 0.1D1 / x3
      t44 = t43 * t27
      t47 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t48 = -t17 + t47
      t50 = 0.1D1 / x2
      t51 = t50 * t27
      t56 = t43 * t50
      t59 = x2 ** 2
      t60 = t13 * t59
      t61 = -0.1D1 + x2
      t65 = log(-0.4D1 * t60 * t9 * t61)
      t67 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t70 = log(0.4D1 * t60 * t9)
      t80 = bbbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t86 = log(0.4D1 * t13 * t9)
      t89 = (-0.180D3 * lh - 0.90D2 * t86) * t5
      t94 = lh ** 2
      t96 = 0.3141592653589793D1 ** 2
      t98 = t86 ** 2
      t101 = (0.180D3 * t86 * lh + 0.180D3 * t94 - 0.30D2 * t96 + 0.45D2
     # * t98) * t5
      t105 = x3 * t13
      t108 = log(0.4D1 * t105 * t9)
      t113 = log(-0.4D1 * t105 * t9 / t33)
      t115 = t108 + t113 * t39
      t121 = -0.1D1 - t39
      t126 = (0.90D2 * t5 * (t16 * t17 - t19) + t25) * t27 / 0.5760D4 + 
     #t5 * (-t17 - t40) * t44 / 0.64D2 + t5 * t48 * t51 / 0.32D2 + t5 * 
     #(t47 - t40 - t17) * t56 / 0.64D2 + (0.90D2 * t5 * (-t19 - t65 * t4
     #7 + t67 + t70 * t17) - 0.180D3 * t23 * t48) * t50 / 0.5760D4 - t5 
     #* t80 / 0.128D3 - t89 * t19 / 0.11520D5 - t101 * t17 / 0.11520D5 +
     # (0.90D2 * t5 * t17 * t115 + (0.90D2 * t5 * t19 - t25) * t121) * t
     #43 / 0.11520D5
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t126)
      t129 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t130 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t136 = 0.180D3 * t23 * t130
      t140 = t130 * t39
      t145 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t146 = -t130 + t145
      t154 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t165 = bbbbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t182 = (0.90D2 * t5 * (-t129 + t16 * t130) + t136) * t27 / 0.5760D
     #4 + t5 * (-t140 - t130) * t44 / 0.64D2 + t5 * t146 * t51 / 0.32D2 
     #+ t5 * (-t130 + t145 - t140) * t56 / 0.64D2 + (0.90D2 * t5 * (t154
     # - t65 * t145 + t70 * t130 - t129) - 0.180D3 * t23 * t146) * t50 /
     # 0.5760D4 - t5 * t165 / 0.128D3 - t89 * t129 / 0.11520D5 - t101 * 
     #t130 / 0.11520D5 + (0.90D2 * t5 * t130 * t115 + (0.90D2 * t5 * t12
     #9 - t136) * t121) * t43 / 0.11520D5
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t182)
      t185 = t2 * x1
      t186 = -0.1D1 + x1
      t187 = t2 * t186
      t188 = bbbbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t190 = x1 * z
      t191 = -z - x1 + t190
      t192 = 0.1D1 / t191
      t194 = t186 ** 2
      t198 = log(-0.4D1 * t10 / t11 * t192 * t194)
      t199 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t209 = x3 * x1
      t215 = Sqrt(x3 * t191 * t33)
      t219 = 0.1D1 / (0.2D1 * t209 * z - 0.2D1 * t209 - t31 - 0.1D1 + 0.
     #2D1 * t32 * t215 + x3)
      t228 = (0.90D2 * t5 * (t188 - t198 * t199) - 0.180D3 * t23 * t199)
     # * t27 / 0.5760D4 + t5 * (t199 * t219 + t199) * t44 / 0.64D2 + t5 
     #* t199 * t51 / 0.32D2
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t185, -t187, 0.0D0, t228)
      t231 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t233 = bbbbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t250 = (0.90D2 * t5 * (-t198 * t231 + t233) - 0.180D3 * t23 * t231
     #) * t27 / 0.5760D4 + t5 * (t231 * t219 + t231) * t44 / 0.64D2 + t5
     # * t231 * t51 / 0.32D2
      t251 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t187, t185, 0.0D0, t250)
      t253 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t182)
      t255 = x2 * x3
      t257 = 0.1D1 / (0.1D1 - x3 + t255)
      t258 = t255 * t257
      t259 = t2 * t258
      t262 = t33 * t1 * s * t257
      t266 = Sqrt(t30 * t61 * t33)
      t270 = 0.1D1 / (t255 * z - t31 - 0.1D1 + 0.2D1 * t32 * t266 + x3)
      t271 = t5 * t270
      t272 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t258, x4)
      t274 = t272 * t43 * t50
      t276 = t271 * t274 / 0.64D2
      t277 = FJET(XB1, XB2, s, 0.0D0, t259, 0.0D0, -t262, 0.0D0, t276)
      t282 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t258, x4)
      t284 = t282 * t43 * t50
      t286 = t271 * t284 / 0.64D2
      t287 = FJET(XB1, XB2, s, 0.0D0, -t262, 0.0D0, t259, 0.0D0, t286)
      t292 = x2 * x1
      t294 = t2 * t292 * t192
      t297 = t61 * s * t1 * x1
      t298 = t1 ** 2
      t303 = s * t298 * x2 * x1 * t186 * t192
      t307 = 0.1D1 / (z - t292 + x1 - t190 + t292 * z)
      t308 = t5 * t191 * t307
      t309 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t313 = t308 * t309 * t50 * t27 / 0.32D2
      t314 = FJET(XB1, XB2, s, 0.0D0, -t294, -t187, -t297, t303, t313)
      t318 = t307 * t309 * t51
      t321 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t126)
      t323 = FJET(XB1, XB2, s, t185, -t187, 0.0D0, 0.0D0, 0.0D0, t250)
      t325 = FJET(XB1, XB2, s, t259, 0.0D0, -t262, 0.0D0, 0.0D0, t286)
      t330 = FJET(XB1, XB2, s, -t187, t185, 0.0D0, 0.0D0, 0.0D0, t228)
      t332 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t336 = t308 * t332 * t50 * t27 / 0.32D2
      t337 = FJET(XB1, XB2, s, -t187, -t297, 0.0D0, -t294, t303, t336)
      t341 = t307 * t332 * t51
      t344 = FJET(XB1, XB2, s, -t297, -t187, -t294, 0.0D0, t303, t313)
      t349 = FJET(XB1, XB2, s, -t294, 0.0D0, -t297, -t187, t303, t336)
      t354 = FJET(XB1, XB2, s, -t262, 0.0D0, t259, 0.0D0, 0.0D0, t276)
      bbbbH5n2em1 = t127 * t126 + t183 * t182 + t229 * t228 + t251 * t25
     #0 + t253 * t182 + t277 * t5 * t270 * t274 / 0.64D2 + t287 * t5 * t
     #270 * t284 / 0.64D2 + t314 * t5 * t191 * t318 / 0.32D2 + t321 * t1
     #26 + t323 * t250 + t325 * t5 * t270 * t284 / 0.64D2 + t330 * t228 
     #+ t337 * t5 * t191 * t341 / 0.32D2 + t344 * t5 * t191 * t318 / 0.3
     #2D2 + t349 * t5 * t191 * t341 / 0.32D2 + t354 * t5 * t270 * t274 /
     # 0.64D2

      end function



      doubleprecision function bbbbH5n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t14 = 0.1D1 / x2
      t17 = bbbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t32 = (-0.180D3 * lh - 0.90D2 * t29) * t5
      t35 = x3 * z
      t37 = cos(t24)
      t40 = Sqrt(-t35 * (-0.1D1 + x3))
      t47 = (-0.1D1 - 0.1D1 / (-0.1D1 - 0.2D1 * t35 + 0.2D1 * t37 * t40 
     #+ x3)) / x3
      t50 = -t7 * t8 / 0.64D2 + t5 * (-t6 + t11) * t14 / 0.64D2 - t5 * t
     #17 / 0.128D3 - t32 * t6 / 0.11520D5 + t7 * t47 / 0.128D3
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t54 = t5 * t53
      t57 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t62 = bbbbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t69 = -t54 * t8 / 0.64D2 + t5 * (-t53 + t57) * t14 / 0.64D2 - t5 *
     # t62 / 0.128D3 - t32 * t53 / 0.11520D5 + t54 * t47 / 0.128D3
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t69)
      t72 = t2 * x1
      t74 = t2 * (-0.1D1 + x1)
      t75 = bbbbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t78 = t5 * t75 * t8 / 0.64D2
      t79 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t72, -t74, 0.0D0, t78)
      t81 = t75 * t8
      t84 = bbbbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t87 = t5 * t84 * t8 / 0.64D2
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t74, t72, 0.0D0, t87)
      t90 = t84 * t8
      t93 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t69)
      t95 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t50)
      t97 = FJET(XB1, XB2, s, t72, -t74, 0.0D0, 0.0D0, 0.0D0, t87)
      t101 = FJET(XB1, XB2, s, -t74, t72, 0.0D0, 0.0D0, 0.0D0, t78)
      bbbbH5n2em2 = t51 * t50 + t70 * t69 + t79 * t5 * t81 / 0.64D2 + t8
     #8 * t5 * t90 / 0.64D2 + t93 * t69 + t95 * t50 + t97 * t5 * t90 / 0
     #.64D2 + t101 * t5 * t81 / 0.64D2

      end function



      doubleprecision function bbbbH5n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t8 = t5 * t6 / 0.128D3
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t12 = bbbbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t14 = t5 * t12 / 0.128D3
      t15 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t14)
      t18 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t14)
      t21 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      bbbbH5n2em3 = -t9 * t5 * t6 / 0.128D3 - t15 * t5 * t12 / 0.128D3 -
     # t18 * t5 * t12 / 0.128D3 - t21 * t5 * t6 / 0.128D3

      end function



      doubleprecision function bbbbH5n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH51J1
      doubleprecision bbbbH51J2
      doubleprecision bbbbH51J3
      doubleprecision bbbbH52J1
      doubleprecision bbbbH52J2
      doubleprecision bbbbH52J3
      bbbbH5n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbbbH51J1
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
      t6 = t2 * t4 * t3
      t7 = x1 ** 2
      t8 = t7 * x1
      t10 = z + x1 * t3
      t11 = t10 ** 2
      t13 = 0.1D1 / t11 / t10
      t15 = 0.1D1 - x3
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(x3 * t16 * t10 * x2 * t15)
      t29 = t15 * t16 * t10 + x2 * x3 + 0.2D1 * t21 * t26
      t30 = t29 ** 2
      t35 = 0.1D1 - x1
      t36 = t35 ** 2
      t39 = z + x1 * t16 * t3
      t42 = 0.1D1 / t11
      t44 = x1 * t29
      t47 = t6 * t7
      t49 = 0.1D1 / t10
      t54 = t15 ** 2
      t57 = t2 * t3
      t66 = t2 * t4
      t71 = t4 ** 2
      t75 = x2 ** 2
      t89 = x2 * t35
      t108 = -0.2D1 * t6 * t8 * t13 * t30 * t29 - t6 * t36 * t39 * t42 *
     # t15 * t44 - t47 * t35 * t15 * t49 * t29 - t6 * x1 * t36 * t54 - t
     #57 * x1 * t49 * t29 - t6 * t35 * t39 * t13 * t7 * t30 + 0.2D1 * t6
     #6 * t7 * t42 * t30 + 0.2D1 * t2 * t71 * t3 * t8 * t75 * t36 * t42 
     #+ t57 * x1 + 0.2D1 * t6 * t8 * t42 * t30 - 0.2D1 * t66 * t7 * t49 
     #* t29 - 0.2D1 * t47 * t89 * t49 + 0.2D1 * t66 * x1 * t35 * t15 + 0
     #.2D1 * t2 * t71 * t8 * t42 * t29 * t89 + 0.2D1 * t66 * t35 * t39 *
     # t42 * t44
      bbbbH51J1 = -0.16D2 / 0.3D1 * wd * t108

      end function
  
   
 

      doubleprecision function bbbbH51J2
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
      t6 = t2 * t4 * t3
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = 0.1D1 - x2
      t12 = z + x1 * t9 * t3
      t16 = z + x1 * t3
      t17 = t16 ** 2
      t18 = 0.1D1 / t17
      t19 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(x3 * t9 * t16 * x2 * t19)
      t33 = t19 * t9 * t16 + x2 * x3 + 0.2D1 * t25 * t30
      t34 = x1 * t33
      t37 = x1 ** 2
      t40 = 0.1D1 / t16
      t44 = t2 * t4
      t58 = t33 ** 2
      t62 = t2 * t3
      t68 = t19 ** 2
      bbbbH51J2 = -0.16D2 / 0.3D1 * wd * (t6 * t8 * t12 * t18 * t19 * t3
     #4 + t6 * t37 * t7 * t19 * t40 * t33 - 0.2D1 * t44 * t7 * t12 * t18
     # * t34 - 0.2D1 * t44 * x1 * t7 * t19 - t6 * t7 * t12 / t17 / t16 *
     # t37 * t58 + t62 * x1 * t40 * t33 - t62 * x1 - t6 * x1 * t8 * t68)

      end function
  
   
 

      doubleprecision function bbbbH51J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t2 * t3
      t6 = t3 ** 2
      t7 = t2 * t6
      t8 = 0.1D1 - x1
      t10 = 0.1D1 - x2
      t15 = z + x1 * t3
      t16 = t15 ** 2
      t19 = 0.1D1 - x3
      t24 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(x3 * t10 * t15 * x2 * t19)
      t32 = t19 * t10 * t15 + x2 * x3 + 0.2D1 * t24 * t29
      bbbbH51J3 = -0.16D2 / 0.3D1 * wd * (-t4 * x1 - t7 * t8 * (z + x1 *
     # t10 * t3) / t16 * x1 * t32 + t4 * x1 / t15 * t32 - t7 * x1 * t8 *
     # t19)

      end function
  
   
 

      doubleprecision function bbbbH52J1
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
      t6 = t2 * t4 * t3
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = 0.1D1 - x3
      t11 = t10 ** 2
      t16 = 0.1D1 - x2
      t19 = z + x1 * t16 * t3
      t23 = z + x1 * t3
      t24 = t23 ** 2
      t25 = 0.1D1 / t24
      t26 = t25 * t10
      t31 = cos(x4 * 0.3141592653589793D1)
      t36 = Sqrt(x3 * t16 * t23 * x2 * t10)
      t39 = t10 * t16 * t23 + x2 * x3 + 0.2D1 * t31 * t36
      t40 = x1 * t39
      t43 = x1 ** 2
      t45 = t7 * t10
      t46 = 0.1D1 / t23
      t53 = t2 * t3
      t58 = t19 / t24 / t23
      t59 = t39 ** 2
      t63 = t2 * t4
      t68 = t19 * t46
      t72 = t4 ** 2
      t76 = x2 ** 2
      t93 = t19 * t25
      t94 = x2 * x1
      t108 = -0.2D1 * t6 * t9 * t11 * t10 - t6 * t8 * t19 * t26 * t40 - 
     #t6 * t43 * t45 * t46 * t39 - t6 * x1 * t8 * t11 - t53 * t45 - t6 *
     # t7 * t58 * t43 * t59 + 0.2D1 * t63 * t8 * t11 + 0.2D1 * t6 * t9 *
     # t68 * t11 + 0.2D1 * t2 * t72 * t3 * t9 * t58 * t76 * t43 + t53 * 
     #t7 * t19 * t46 + 0.2D1 * t63 * x1 * t7 * t10 - 0.2D1 * t63 * t8 * 
     #t68 * t10 - 0.2D1 * t6 * t8 * t93 * t94 + 0.2D1 * t63 * t7 * t93 *
     # t40 + 0.2D1 * t2 * t72 * t9 * t19 * t26 * t94
      bbbbH52J1 = -0.16D2 / 0.3D1 * wd * t108

      end function
  
   
 

      doubleprecision function bbbbH52J2
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
      t6 = 0.1D1 - x1
      t8 = 0.1D1 - x2
      t11 = z + x1 * t8 * t3
      t13 = z + x1 * t3
      t14 = t13 ** 2
      t15 = 0.1D1 / t14
      t17 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t8 * t13 * x2 * t17)
      t30 = t17 * t8 * t13 + x2 * x3 + 0.2D1 * t22 * t27
      t31 = x1 * t30
      t36 = t2 * t4 * t3
      t37 = x1 ** 2
      t39 = t6 * t17
      t40 = 0.1D1 / t13
      t48 = t30 ** 2
      t56 = t6 ** 2
      t63 = t17 ** 2
      t66 = t2 * t3
      bbbbH52J2 = -0.16D2 / 0.3D1 * wd * (-0.2D1 * t5 * t6 * t11 * t15 *
     # t31 + t36 * t37 * t39 * t40 * t30 - t36 * t6 * t11 / t14 / t13 * 
     #t37 * t48 - 0.2D1 * t5 * x1 * t6 * t17 + t36 * t56 * t11 * t15 * t
     #17 * t31 - t36 * x1 * t56 * t63 + t66 * t39 - t66 * t6 * t11 * t40
     #)

      end function
  
   
 

      doubleprecision function bbbbH52J3
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
      t6 = 0.1D1 - x1
      t8 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t15 = z + x1 * t12 * t3
      t17 = z + x1 * t3
      t18 = t17 ** 2
      t25 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(x3 * t12 * t17 * x2 * t8)
      t37 = t2 * t3
      bbbbH52J3 = -0.16D2 / 0.3D1 * wd * (-t5 * x1 * t6 * t8 - t5 * t6 *
     # t15 / t18 * x1 * (t8 * t12 * t17 + x2 * x3 + 0.2D1 * t25 * t30) -
     # t37 * t6 * t15 / t17 + t37 * t6 * t8)

      end function
  
 