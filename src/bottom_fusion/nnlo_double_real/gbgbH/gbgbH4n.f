  
      subroutine gbgbH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH41J1  
      doubleprecision gbgbH41J2  
      doubleprecision gbgbH42J1  
      doubleprecision gbgbH42J2  
      doubleprecision gbgbH4n1e1  
      doubleprecision gbgbH4n1e0  
      doubleprecision gbgbH4n1em1  
      doubleprecision gbgbH4n1em2  
      doubleprecision gbgbH4n1em3  
      doubleprecision gbgbH4n1em4  
      doubleprecision gbgbH4n2e1  
      doubleprecision gbgbH4n2e0  
      doubleprecision gbgbH4n2em1  
      doubleprecision gbgbH4n2em2  
      doubleprecision gbgbH4n2em3  
      doubleprecision gbgbH4n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH4n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH4n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH4n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH4n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH4n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH4n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 ** 2
      t18 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t21 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = t30 * t5
      t35 = t17 * t16
      t44 = 0.120D3 * t26 * lh
      t46 = 0.60D2 * lh * t28
      t47 = -0.2884936567583026D3 - t44 + t46
      t48 = t47 * t5
      t49 = t48 * t18
      t51 = 0.1D1 / x1
      t54 = t7 * x3
      t55 = t10 * t13
      t56 = -0.1D1 + x3
      t57 = 0.1D1 / t56
      t58 = t55 * t57
      t61 = log(-0.4D1 * t54 * t58)
      t64 = cos(t8)
      t66 = Sqrt(-x3 * t56)
      t70 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t64 * t66)
      t74 = log(0.4D1 * t54 * t55)
      t80 = t61 ** 2
      t86 = t74 ** 2
      t93 = t18 * t70 + t18
      t96 = 0.1D1 / x3
      t100 = x2 ** 2
      t101 = x3 * t100
      t102 = t101 * t7
      t105 = log(-0.4D1 * t102 * t58)
      t109 = t101 * t14
      t111 = log(0.4D1 * t109)
      t120 = 0.1D1 / x2
      t121 = t120 * t51
      t124 = t100 * t7
      t127 = log(0.4D1 * t124 * t55)
      t132 = t127 ** 2
      t145 = log(0.4D1 * t55)
      t147 = t145 ** 2
      t150 = t147 * t145
      t153 = (-t145 * t30 - 0.90D2 * t147 * lh - 0.2884936567583026D3 - 
     #t44 + t46 - 0.15D2 * t150) * t5
      t156 = t147 ** 2
      t160 = t28 ** 2
      t161 = t26 ** 2
      t170 = (0.15D2 / 0.4D1 * t156 - t145 * t47 + 0.5769873135166051D3 
     #* lh + t160 + 0.60D2 * t161 - 0.60D2 * t26 * t28 + t147 * t30 / 0.
     #2D1 + 0.30D2 * t150 * lh) * t5
      t178 = x3 * t10
      t182 = log(-0.4D1 * t178 * t13 * t57)
      t183 = t182 ** 2
      t187 = log(0.4D1 * t178 * t13)
      t188 = t187 ** 2
      t190 = -t183 * t70 / 0.2D1 - t188 / 0.2D1
      t198 = t187 + t182 * t70
      t205 = t183 * t182 * t70 / 0.6D1 + t188 * t187 / 0.6D1
      t212 = -0.1D1 - t70
      t220 = log(0.4D1 * t100 * t13 * t10)
      t221 = t220 ** 2
      t231 = t221 * t220
      t244 = log(-0.4D1 * t101 * t58)
      t250 = log(0.4D1 * t101 * t55)
      t256 = t244 ** 2
      t261 = t250 ** 2
      t274 = (-0.180D3 * t6 * (-t17 * t18 / 0.2D1 + t16 * t21) + t31 * (
     #-t21 + t16 * t18) + 0.90D2 * t5 * (t35 * t18 / 0.6D1 - t17 * t21 /
     # 0.2D1) - t49) * t51 / 0.5760D4 - (-0.180D3 * t6 * ((t21 - t61 * t
     #18) * t70 + t21 - t74 * t18) + 0.90D2 * t5 * ((-t61 * t21 + t80 * 
     #t18 / 0.2D1) * t70 - t74 * t21 + t86 * t18 / 0.2D1) + t31 * t93) *
     # t96 * t51 / 0.5760D4 - (0.90D2 * t5 * ((t21 - t105 * t18) * t70 +
     # t21 - t111 * t18) - 0.180D3 * t6 * t93) * t96 * t121 / 0.2880D4 -
     # (-0.180D3 * t6 * (-t127 * t18 + t21) + 0.90D2 * t5 * (t132 * t18 
     #/ 0.2D1 - t127 * t21) + t31 * t18) * t120 * t51 / 0.2880D4 - t153 
     #* t21 / 0.11520D5 - t170 * t18 / 0.11520D5 + ((-0.180D3 * t18 * lh
     # + 0.90D2 * t21) * t5 * t190 + (-0.180D3 * t21 * lh + t18 * t30) *
     # t5 * t198 + 0.90D2 * t18 * t5 * t205 + (t21 * t30 + t18 * t47) * 
     #t5 * t212) * t96 / 0.11520D5 + (-0.180D3 * t6 * (-t221 * t18 / 0.2
     #D1 + t220 * t21) + t31 * (-t21 + t220 * t18) + 0.90D2 * t5 * (t231
     # * t18 / 0.6D1 - t221 * t21 / 0.2D1) - t49) * t120 / 0.5760D4 + (-
     #0.180D3 * t6 * (-(t21 - t244 * t18) * t70 - t21 + t250 * t18) + 0.
     #90D2 * t5 * (-(-t244 * t21 + t256 * t18 / 0.2D1) * t70 - t261 * t1
     #8 / 0.2D1 + t250 * t21) - t31 * t93) * t96 * t120 / 0.5760D4
      t275 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t274)
      t277 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t280 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t322 = t48 * t280
      t345 = t280 * t70 + t280
      t422 = -t153 * t277 / 0.11520D5 - t170 * t280 / 0.11520D5 + ((-0.1
     #80D3 * t280 * lh + 0.90D2 * t277) * t5 * t190 + (-0.180D3 * t277 *
     # lh + t280 * t30) * t5 * t198 + 0.90D2 * t280 * t5 * t205 + (t277 
     #* t30 + t280 * t47) * t5 * t212) * t96 / 0.11520D5 + (-0.180D3 * t
     #6 * (t16 * t277 - t17 * t280 / 0.2D1) + t31 * (-t277 + t16 * t280)
     # + 0.90D2 * t5 * (-t17 * t277 / 0.2D1 + t35 * t280 / 0.6D1) - t322
     #) * t51 / 0.5760D4 - (-0.180D3 * t6 * ((t277 - t61 * t280) * t70 +
     # t277 - t74 * t280) + 0.90D2 * t5 * ((-t61 * t277 + t80 * t280 / 0
     #.2D1) * t70 - t74 * t277 + t86 * t280 / 0.2D1) + t31 * t345) * t96
     # * t51 / 0.5760D4 - (0.90D2 * t5 * (t277 - t111 * t280 + (t277 - t
     #105 * t280) * t70) - 0.180D3 * t6 * t345) * t96 * t121 / 0.2880D4 
     #- (-0.180D3 * t6 * (t277 - t127 * t280) + 0.90D2 * t5 * (-t127 * t
     #277 + t132 * t280 / 0.2D1) + t31 * t280) * t120 * t51 / 0.2880D4 +
     # (-0.180D3 * t6 * (t220 * t277 - t221 * t280 / 0.2D1) + t31 * (-t2
     #77 + t220 * t280) + 0.90D2 * t5 * (-t221 * t277 / 0.2D1 + t231 * t
     #280 / 0.6D1) - t322) * t120 / 0.5760D4 + (-0.180D3 * t6 * (-(t277 
     #- t244 * t280) * t70 - t277 + t250 * t280) + 0.90D2 * t5 * (t250 *
     # t277 - t261 * t280 / 0.2D1 - (-t244 * t277 + t256 * t280 / 0.2D1)
     # * t70) - t31 * t345) * t96 * t120 / 0.5760D4
      t423 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t422)
      t425 = t2 * x1
      t426 = -0.1D1 + x1
      t427 = t2 * t426
      t428 = x1 * z
      t429 = 0.1D1 - x1 + t428
      t430 = 0.1D1 / t429
      t431 = t13 * t430
      t432 = t426 ** 2
      t433 = t431 * t432
      t436 = log(0.4D1 * t11 * t433)
      t437 = t436 ** 2
      t438 = -t426
      t439 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t438, 0.0D0, 0.0D0, x4)
      t442 = gbgbH41J2(s, XB1, XB2, z, lh, wd, t438, 0.0D0, 0.0D0, x4)
      t450 = t437 * t436
      t465 = t10 * t432 * t430 * t57
      t468 = log(-0.4D1 * t54 * t13 * t465)
      t471 = x1 * x3
      t472 = t471 * z
      t475 = x3 * t429
      t477 = Sqrt(-t475 * t56)
      t481 = 0.1D1 / (-0.2D1 * t472 + 0.2D1 * t471 - 0.1D1 - x3 + 0.2D1 
     #* t64 * t477)
      t486 = log(0.4D1 * t54 * t10 * t433)
      t492 = t486 ** 2
      t496 = t468 ** 2
      t505 = -t439 - t439 * t481
      t512 = t101 * t7 * t13
      t515 = log(-0.4D1 * t512 * t465)
      t519 = t430 * t432
      t523 = log(0.4D1 * t102 * t55 * t519)
      t534 = t124 * t10
      t537 = log(0.4D1 * t534 * t433)
      t542 = t537 ** 2
      t554 = (-0.180D3 * t6 * (t437 * t439 / 0.2D1 - t436 * t442) + t31 
     #* (t442 - t436 * t439) + 0.90D2 * t5 * (-t450 * t439 / 0.6D1 + t43
     #7 * t442 / 0.2D1) + t48 * t439) * t51 / 0.5760D4 - (-0.180D3 * t6 
     #* (-(t442 - t468 * t439) * t481 - t442 + t486 * t439) + 0.90D2 * t
     #5 * (t486 * t442 - t492 * t439 / 0.2D1 - (-t468 * t442 + t496 * t4
     #39 / 0.2D1) * t481) + t31 * t505) * t96 * t51 / 0.5760D4 - (0.90D2
     # * t5 * (-(t442 - t515 * t439) * t481 - t442 + t523 * t439) - 0.18
     #0D3 * t6 * t505) * t96 * t121 / 0.2880D4 - (-0.180D3 * t6 * (-t442
     # + t537 * t439) + 0.90D2 * t5 * (-t542 * t439 / 0.2D1 + t537 * t44
     #2) - t31 * t439) * t120 * t51 / 0.2880D4
      t555 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t425, -t427, 0.0D0, t554)
      t557 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t438, 0.0D0, 0.0D0, x4)
      t560 = gbgbH42J2(s, XB1, XB2, z, lh, wd, t438, 0.0D0, 0.0D0, x4)
      t598 = -t557 - t557 * t481
      t632 = (-0.180D3 * t6 * (t437 * t557 / 0.2D1 - t436 * t560) + t31 
     #* (t560 - t436 * t557) + 0.90D2 * t5 * (-t450 * t557 / 0.6D1 + t43
     #7 * t560 / 0.2D1) + t48 * t557) * t51 / 0.5760D4 - (-0.180D3 * t6 
     #* (t486 * t557 - t560 - (t560 - t468 * t557) * t481) + 0.90D2 * t5
     # * (t486 * t560 - t492 * t557 / 0.2D1 - (-t468 * t560 + t496 * t55
     #7 / 0.2D1) * t481) + t31 * t598) * t96 * t51 / 0.5760D4 - (0.90D2 
     #* t5 * (-(t560 - t515 * t557) * t481 + t523 * t557 - t560) - 0.180
     #D3 * t6 * t598) * t96 * t121 / 0.2880D4 - (-0.180D3 * t6 * (-t560 
     #+ t537 * t557) + 0.90D2 * t5 * (t537 * t560 - t542 * t557 / 0.2D1)
     # - t31 * t557) * t120 * t51 / 0.2880D4
      t633 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t427, t425, 0.0D0, t632)
      t636 = x2 * s * t1
      t637 = -0.1D1 + x2
      t638 = t637 * s
      t639 = t638 * t1
      t640 = t55 * t637
      t643 = log(-0.4D1 * t102 * t640)
      t644 = x2 * z
      t646 = 0.1D1 / (0.1D1 + t644 - x2)
      t647 = t643 * t646
      t648 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t650 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t651 = t646 * t650
      t655 = t646 * t648
      t664 = log(-0.4D1 * t124 * t640)
      t665 = t664 * t646
      t671 = t664 ** 2
      t672 = t671 * t646
      t678 = t31 * t655
      t687 = log(-0.4D1 * t100 * t10 * t13 * t637)
      t688 = t687 ** 2
      t689 = t688 * t646
      t692 = t687 * t646
      t701 = t688 * t687 * t646
      t715 = log(-0.4D1 * t101 * t640)
      t716 = t715 * t646
      t722 = t715 ** 2
      t723 = t722 * t646
      t733 = -(0.90D2 * t5 * (t647 * t648 - t651) + 0.180D3 * t6 * t655)
     # * t96 * t121 / 0.2880D4 - (-0.180D3 * t6 * (-t651 + t665 * t648) 
     #+ 0.90D2 * t5 * (t665 * t650 - t672 * t648 / 0.2D1) - t678) * t120
     # * t51 / 0.2880D4 + (-0.180D3 * t6 * (t689 * t648 / 0.2D1 - t692 *
     # t650) + t31 * (t651 - t692 * t648) + 0.90D2 * t5 * (-t701 * t648 
     #/ 0.6D1 + t689 * t650 / 0.2D1) + t48 * t655) * t120 / 0.5760D4 + (
     #-0.180D3 * t6 * (-t716 * t648 + t651) + 0.90D2 * t5 * (-t716 * t65
     #0 + t723 * t648 / 0.2D1) + t678) * t96 * t120 / 0.5760D4
      t734 = FJET(XB1, XB2, s, 0.0D0, t636, 0.0D0, -t639, 0.0D0, t733)
      t736 = x2 * x3
      t739 = Sqrt(x3 * t637 * t56)
      t740 = t64 * t739
      t741 = 0.2D1 * t740
      t744 = 0.1D1 - x3 + t736
      t745 = 0.1D1 / t744
      t747 = t2 * x2 * (-0.1D1 + t736 + t741) * t745
      t749 = 0.2D1 * t740 * x2
      t752 = t2 * (0.1D1 - x3 - x2 + t736 + t101 + t749) * t745
      t754 = t744 ** 2
      t755 = 0.1D1 / t754
      t757 = t10 * t637 * t56 * t755
      t760 = log(0.4D1 * t512 * t757)
      t761 = t101 * z
      t765 = 0.1D1 / (-0.1D1 - t101 + t736 - t644 + x2 - x3 - t749 + t76
     #1 + t741 + 0.2D1 * t740 * t644)
      t766 = t760 * t765
      t767 = t736 * t745
      t768 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t767, x4)
      t770 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t767, x4)
      t771 = t765 * t770
      t775 = t765 * t768
      t785 = log(0.4D1 * t101 * t13 * t757)
      t786 = t785 * t765
      t791 = t785 ** 2
      t792 = t791 * t765
      t804 = -(0.90D2 * t5 * (t766 * t768 - t771) + 0.180D3 * t6 * t775)
     # * t96 * t121 / 0.2880D4 + (-0.180D3 * t6 * (-t786 * t768 + t771) 
     #+ 0.90D2 * t5 * (t792 * t768 / 0.2D1 - t786 * t770) + t31 * t775) 
     #* t96 * t120 / 0.5760D4
      t805 = FJET(XB1, XB2, s, 0.0D0, -t747, 0.0D0, t752, 0.0D0, t804)
      t809 = t2 * t426 * x2 * t430
      t811 = t638 * t1 * t426
      t812 = t1 ** 2
      t817 = s * t812 * x2 * t426 * x1 * t430
      t818 = x2 * x1
      t819 = t818 * z
      t821 = 0.1D1 / (-0.1D1 + x2 - t818 - t644 + t819 - t428 + x1)
      t822 = t429 * t821
      t823 = gbgbH41J2(s, XB1, XB2, z, lh, wd, t438, x2, 0.0D0, x4)
      t824 = t822 * t823
      t827 = t431 * t432 * t637
      t830 = log(-0.4D1 * t101 * t11 * t827)
      t831 = t830 * t429
      t832 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t438, x2, 0.0D0, x4)
      t833 = t821 * t832
      t838 = t822 * t832
      t846 = log(-0.4D1 * t534 * t827)
      t847 = t846 * t429
      t852 = t846 ** 2
      t853 = t852 * t429
      t866 = -(0.90D2 * t5 * (-t824 + t831 * t833) + 0.180D3 * t6 * t838
     #) * t96 * t121 / 0.2880D4 - (-0.180D3 * t6 * (t847 * t833 - t824) 
     #+ 0.90D2 * t5 * (-t853 * t833 / 0.2D1 + t847 * t821 * t823) - t31 
     #* t838) * t120 * t51 / 0.2880D4
      t867 = FJET(XB1, XB2, s, 0.0D0, -t809, t425, t811, -t817, t866)
      t869 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t871 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t872 = t646 * t871
      t876 = t646 * t869
      t893 = t31 * t876
      t932 = -(0.90D2 * t5 * (t647 * t869 - t872) + 0.180D3 * t6 * t876)
     # * t96 * t121 / 0.2880D4 - (-0.180D3 * t6 * (t665 * t869 - t872) +
     # 0.90D2 * t5 * (-t672 * t869 / 0.2D1 + t665 * t871) - t893) * t120
     # * t51 / 0.2880D4 + (-0.180D3 * t6 * (t689 * t869 / 0.2D1 - t692 *
     # t871) + t31 * (t872 - t692 * t869) + 0.90D2 * t5 * (-t701 * t869 
     #/ 0.6D1 + t689 * t871 / 0.2D1) + t48 * t876) * t120 / 0.5760D4 + (
     #-0.180D3 * t6 * (t872 - t716 * t869) + 0.90D2 * t5 * (t723 * t869 
     #/ 0.2D1 - t716 * t871) + t893) * t96 * t120 / 0.5760D4
      t933 = FJET(XB1, XB2, s, t636, 0.0D0, -t639, 0.0D0, 0.0D0, t932)
      t935 = t425 * t767
      t936 = t736 * x1
      t937 = t736 * t428
      t938 = t637 * t56
      t940 = Sqrt(t475 * t938)
      t941 = t64 * t940
      t942 = 0.2D1 * t941
      t947 = t427 * x2 * (t471 - t472 + t736 - t936 + t937 - 0.1D1 + t94
     #2) * t430 * t745
      t951 = t56 * s * t1 * x1 * t745
      t953 = 0.2D1 * t941 * x2
      t954 = 0.1D1 - x1 + t428 - x2 + t818 - t819 - x3 + t471 - t472 + t
     #736 - t936 + t937 + t101 + t953
      t957 = t427 * t954 * t430 * t745
      t968 = 0.1D1 - x1 + 0.2D1 * t941 * t819 - t942 + 0.2D1 * t54 - t73
     #6 + t101 + t818 - t761 - t819 + t644 + 0.3D1 * t936 + t953 + t102 
     #- 0.4D1 * t54 * z - 0.2D1 * t54 * x2 + 0.2D1 * t54 * t12
      t998 = -0.2D1 * t101 * x1 + 0.2D1 * t941 * x1 - 0.4D1 * t937 - 0.2
     #D1 * t941 * t428 - 0.2D1 * t941 * t644 - 0.2D1 * t941 * t818 + x3 
     #* t12 * t818 + 0.4D1 * t54 * t644 - 0.2D1 * t54 * t12 * x2 + 0.3D1
     # * t101 * t428 - t101 * t12 * x1 - 0.2D1 * t101 * t7 * z + t101 * 
     #t7 * t12 + t428 - 0.3D1 * t471 - x2 + x3 + 0.3D1 * t472
      t1000 = 0.1D1 / (t968 + t998)
      t1001 = t429 * t1000
      t1002 = gbgbH41J2(s, XB1, XB2, z, lh, wd, t438, x2, t767, x4)
      t1008 = log(0.4D1 * t109 * t519 * t938 * t755)
      t1009 = t1008 * t429
      t1010 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t438, x2, t767, x4)
      t1019 = 0.90D2 * t5 * (-t1001 * t1002 + t1009 * t1000 * t1010) + 0
     #.180D3 * t6 * t1001 * t1010
      t1023 = FJET(XB1, XB2, s, t935, t947, -t951, -t957, -t817, -t1019 
     #* t96 * t121 / 0.2880D4)
      t1026 = t96 * t120 * t51
      t1029 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t438, x2, t767, x4)
      t1032 = gbgbH42J2(s, XB1, XB2, z, lh, wd, t438, x2, t767, x4)
      t1040 = 0.90D2 * t5 * (t1009 * t1000 * t1029 - t1001 * t1032) + 0.
     #180D3 * t6 * t1001 * t1029
      t1044 = FJET(XB1, XB2, s, t947, t935, -t957, -t951, -t817, -t1040 
     #* t96 * t121 / 0.2880D4)
      t1048 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t767, x4)
      t1049 = t765 * t1048
      t1050 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t767, x4)
      t1055 = t765 * t1050
      t1077 = -(0.90D2 * t5 * (-t1049 + t766 * t1050) + 0.180D3 * t6 * t
     #1055) * t96 * t121 / 0.2880D4 + (-0.180D3 * t6 * (-t786 * t1050 + 
     #t1049) + 0.90D2 * t5 * (-t786 * t1048 + t792 * t1050 / 0.2D1) + t3
     #1 * t1055) * t96 * t120 / 0.5760D4
      t1078 = FJET(XB1, XB2, s, -t747, 0.0D0, t752, 0.0D0, 0.0D0, t1077)
      t1080 = gbgbH42J2(s, XB1, XB2, z, lh, wd, t438, x2, 0.0D0, x4)
      t1081 = t822 * t1080
      t1082 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t438, x2, 0.0D0, x4)
      t1083 = t821 * t1082
      t1088 = t822 * t1082
      t1110 = -(0.90D2 * t5 * (-t1081 + t831 * t1083) + 0.180D3 * t6 * t
     #1088) * t96 * t121 / 0.2880D4 - (-0.180D3 * t6 * (-t1081 + t847 * 
     #t1083) + 0.90D2 * t5 * (t847 * t821 * t1080 - t853 * t1083 / 0.2D1
     #) - t31 * t1088) * t120 * t51 / 0.2880D4
      t1111 = FJET(XB1, XB2, s, -t809, 0.0D0, t811, t425, -t817, t1110)
      gbgbH4n1e1 = t275 * t274 + t423 * t422 + t555 * t554 + t633 * t632
     # + t734 * t733 + t805 * t804 + t867 * t866 + t933 * t932 - t1023 *
     # t1019 * t1026 / 0.2880D4 - t1044 * t1040 * t1026 / 0.2880D4 + t10
     #78 * t1077 + t1111 * t1110

      end function



      doubleprecision function gbgbH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
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
      t21 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t24 = cos(t9)
      t26 = Sqrt(-x3 * t15)
      t30 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t24 * t26)
      t34 = log(0.4D1 * t8 * t14)
      t39 = lh * t5
      t41 = t21 * t30 + t21
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t51 = 0.1D1 / x2
      t53 = t45 * t51 * t47
      t56 = x2 ** 2
      t57 = t56 * t7
      t60 = log(0.4D1 * t57 * t14)
      t71 = t7 * t11
      t74 = log(0.4D1 * t71 * t13)
      t79 = t74 ** 2
      t86 = lh ** 2
      t87 = 0.180D3 * t86
      t88 = 0.3141592653589793D1 ** 2
      t89 = 0.30D2 * t88
      t90 = t87 - t89
      t91 = t90 * t5
      t92 = t91 * t21
      t101 = x3 * t11
      t104 = log(0.4D1 * t101 * t13)
      t108 = log(-0.4D1 * t101 * t13 * t16)
      t110 = t104 + t108 * t30
      t113 = t108 ** 2
      t115 = t104 ** 2
      t117 = -t113 * t30 / 0.2D1 - t115 / 0.2D1
      t125 = -0.1D1 - t30
      t130 = x3 * t56
      t133 = log(-0.4D1 * t130 * t17)
      t139 = log(0.4D1 * t130 * t14)
      t154 = log(0.4D1 * t56 * t13 * t11)
      t159 = t154 ** 2
      t170 = log(0.4D1 * t14)
      t173 = t170 ** 2
      t176 = (0.180D3 * t170 * lh + t87 - t89 + 0.45D2 * t173) * t5
      t189 = (-t170 * t90 - 0.90D2 * t173 * lh - 0.2884936567583026D3 - 
     #0.120D3 * t86 * lh + 0.60D2 * lh * t88 - 0.15D2 * t173 * t170) * t
     #5
      t192 = -(0.90D2 * t5 * ((t6 - t20 * t21) * t30 + t6 - t34 * t21) -
     # 0.180D3 * t39 * t41) * t45 * t47 / 0.5760D4 - t5 * t41 * t53 / 0.
     #32D2 - (0.90D2 * t5 * (-t60 * t21 + t6) - 0.180D3 * t39 * t21) * t
     #51 * t47 / 0.2880D4 + (-0.180D3 * t39 * (-t6 + t74 * t21) + 0.90D2
     # * t5 * (-t79 * t21 / 0.2D1 + t74 * t6) - t92) * t47 / 0.5760D4 + 
     #((-0.180D3 * t21 * lh + 0.90D2 * t6) * t5 * t110 + 0.90D2 * t21 * 
     #t5 * t117 + (-0.180D3 * t6 * lh + t21 * t90) * t5 * t125) * t45 / 
     #0.11520D5 + (0.90D2 * t5 * (-(t6 - t133 * t21) * t30 - t6 + t139 *
     # t21) + 0.180D3 * t39 * t41) * t45 * t51 / 0.5760D4 + (-0.180D3 * 
     #t39 * (-t6 + t154 * t21) + 0.90D2 * t5 * (-t159 * t21 / 0.2D1 + t1
     #54 * t6) - t92) * t51 / 0.5760D4 - t176 * t6 / 0.11520D5 - t189 * 
     #t21 / 0.11520D5
      t193 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t192)
      t195 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t196 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t205 = t196 * t30 + t196
      t235 = t91 * t196
      t288 = -(0.90D2 * t5 * ((t195 - t20 * t196) * t30 + t195 - t34 * t
     #196) - 0.180D3 * t39 * t205) * t45 * t47 / 0.5760D4 - t5 * t205 * 
     #t53 / 0.32D2 - (0.90D2 * t5 * (t195 - t60 * t196) - 0.180D3 * t39 
     #* t196) * t51 * t47 / 0.2880D4 + (-0.180D3 * t39 * (-t195 + t74 * 
     #t196) + 0.90D2 * t5 * (t74 * t195 - t79 * t196 / 0.2D1) - t235) * 
     #t47 / 0.5760D4 + ((-0.180D3 * t196 * lh + 0.90D2 * t195) * t5 * t1
     #10 + 0.90D2 * t196 * t5 * t117 + (-0.180D3 * t195 * lh + t196 * t9
     #0) * t5 * t125) * t45 / 0.11520D5 + (0.90D2 * t5 * (-(t195 - t133 
     #* t196) * t30 - t195 + t139 * t196) + 0.180D3 * t39 * t205) * t45 
     #* t51 / 0.5760D4 + (-0.180D3 * t39 * (-t195 + t154 * t196) + 0.90D
     #2 * t5 * (t154 * t195 - t159 * t196 / 0.2D1) - t235) * t51 / 0.576
     #0D4 - t176 * t195 / 0.11520D5 - t189 * t196 / 0.11520D5
      t289 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t288)
      t291 = t2 * x1
      t292 = -0.1D1 + x1
      t293 = t2 * t292
      t294 = -t292
      t295 = gbgbH41J2(s, XB1, XB2, z, lh, wd, t294, 0.0D0, 0.0D0, x4)
      t297 = t292 ** 2
      t299 = x1 * z
      t300 = 0.1D1 - x1 + t299
      t301 = 0.1D1 / t300
      t306 = log(-0.4D1 * t8 * t13 * t11 * t297 * t301 * t16)
      t307 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t294, 0.0D0, 0.0D0, x4)
      t310 = x1 * x3
      t311 = t310 * z
      t314 = x3 * t300
      t316 = Sqrt(-t314 * t15)
      t320 = 0.1D1 / (-0.2D1 * t311 + 0.2D1 * t310 - 0.1D1 - x3 + 0.2D1 
     #* t24 * t316)
      t323 = t13 * t301
      t324 = t323 * t297
      t327 = log(0.4D1 * t8 * t11 * t324)
      t333 = -t307 - t307 * t320
      t343 = t57 * t11
      t346 = log(0.4D1 * t343 * t324)
      t359 = log(0.4D1 * t71 * t324)
      t364 = t359 ** 2
      t375 = -(0.90D2 * t5 * (-(t295 - t306 * t307) * t320 - t295 + t327
     # * t307) - 0.180D3 * t39 * t333) * t45 * t47 / 0.5760D4 - t5 * t33
     #3 * t53 / 0.32D2 - (0.90D2 * t5 * (-t295 + t346 * t307) + 0.180D3 
     #* t39 * t307) * t51 * t47 / 0.2880D4 + (-0.180D3 * t39 * (t295 - t
     #359 * t307) + 0.90D2 * t5 * (t364 * t307 / 0.2D1 - t359 * t295) + 
     #t91 * t307) * t47 / 0.5760D4
      t376 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t291, -t293, 0.0D0, t375)
      t378 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t294, 0.0D0, 0.0D0, x4)
      t380 = gbgbH42J2(s, XB1, XB2, z, lh, wd, t294, 0.0D0, 0.0D0, x4)
      t388 = -t378 - t378 * t320
      t422 = -(0.90D2 * t5 * (t327 * t378 - t380 - (t380 - t306 * t378) 
     #* t320) - 0.180D3 * t39 * t388) * t45 * t47 / 0.5760D4 - t5 * t388
     # * t53 / 0.32D2 - (0.90D2 * t5 * (-t380 + t346 * t378) + 0.180D3 *
     # t39 * t378) * t51 * t47 / 0.2880D4 + (-0.180D3 * t39 * (t380 - t3
     #59 * t378) + 0.90D2 * t5 * (t364 * t378 / 0.2D1 - t359 * t380) + t
     #91 * t378) * t47 / 0.5760D4
      t423 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t293, t291, 0.0D0, t422)
      t426 = x2 * s * t1
      t427 = -0.1D1 + x2
      t428 = t427 * s
      t429 = t428 * t1
      t430 = x2 * z
      t432 = 0.1D1 / (0.1D1 + t430 - x2)
      t433 = t5 * t432
      t434 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t438 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t439 = t432 * t438
      t440 = t14 * t427
      t443 = log(-0.4D1 * t57 * t440)
      t444 = t443 * t432
      t449 = t432 * t434
      t451 = 0.180D3 * t39 * t449
      t458 = log(-0.4D1 * t130 * t440)
      t459 = t458 * t432
      t472 = log(-0.4D1 * t56 * t11 * t13 * t427)
      t473 = t472 * t432
      t478 = t472 ** 2
      t479 = t478 * t432
      t490 = t433 * t434 * t53 / 0.32D2 - (0.90D2 * t5 * (-t439 + t444 *
     # t434) + t451) * t51 * t47 / 0.2880D4 + (0.90D2 * t5 * (-t459 * t4
     #34 + t439) - t451) * t45 * t51 / 0.5760D4 + (-0.180D3 * t39 * (t43
     #9 - t473 * t434) + 0.90D2 * t5 * (t479 * t434 / 0.2D1 - t473 * t43
     #8) + t91 * t449) * t51 / 0.5760D4
      t491 = FJET(XB1, XB2, s, 0.0D0, t426, 0.0D0, -t429, 0.0D0, t490)
      t493 = x2 * x3
      t496 = Sqrt(x3 * t427 * t15)
      t497 = t24 * t496
      t498 = 0.2D1 * t497
      t501 = 0.1D1 - x3 + t493
      t502 = 0.1D1 / t501
      t504 = t2 * x2 * (-0.1D1 + t493 + t498) * t502
      t506 = 0.2D1 * t497 * x2
      t509 = t2 * (0.1D1 - x3 - x2 + t493 + t130 + t506) * t502
      t510 = t130 * z
      t514 = 0.1D1 / (-0.1D1 - t130 + t493 - t430 + x2 - x3 - t506 + t51
     #0 + t498 + 0.2D1 * t497 * t430)
      t515 = t5 * t514
      t516 = t493 * t502
      t517 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t516, x4)
      t523 = t501 ** 2
      t529 = log(0.4D1 * t130 * t13 * t11 * t427 * t15 / t523)
      t530 = t529 * t514
      t532 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t516, x4)
      t544 = t515 * t517 * t53 / 0.32D2 + (0.90D2 * t5 * (-t530 * t517 +
     # t514 * t532) - 0.180D3 * t39 * t514 * t517) * t45 * t51 / 0.5760D
     #4
      t545 = FJET(XB1, XB2, s, 0.0D0, -t504, 0.0D0, t509, 0.0D0, t544)
      t549 = t2 * t292 * x2 * t301
      t551 = t428 * t1 * t292
      t552 = t1 ** 2
      t557 = s * t552 * x2 * t292 * x1 * t301
      t558 = t5 * t300
      t559 = x2 * x1
      t560 = t559 * z
      t562 = 0.1D1 / (-0.1D1 + x2 - t559 - t430 + t560 - t299 + x1)
      t563 = t558 * t562
      t564 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t294, x2, 0.0D0, x4)
      t566 = t51 * t47
      t574 = log(-0.4D1 * t343 * t323 * t297 * t427)
      t575 = t574 * t300
      t578 = t300 * t562
      t579 = gbgbH41J2(s, XB1, XB2, z, lh, wd, t294, x2, 0.0D0, x4)
      t591 = t563 * t564 * t45 * t566 / 0.32D2 - (0.90D2 * t5 * (t575 * 
     #t562 * t564 - t578 * t579) + 0.180D3 * t39 * t578 * t564) * t51 * 
     #t47 / 0.2880D4
      t592 = FJET(XB1, XB2, s, 0.0D0, -t549, t291, t551, -t557, t591)
      t594 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t599 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t600 = t432 * t599
      t604 = t432 * t594
      t606 = 0.180D3 * t39 * t604
      t633 = t433 * t594 * t53 / 0.32D2 - (0.90D2 * t5 * (t444 * t594 - 
     #t600) + t606) * t51 * t47 / 0.2880D4 + (0.90D2 * t5 * (t600 - t459
     # * t594) - t606) * t45 * t51 / 0.5760D4 + (-0.180D3 * t39 * (t600 
     #- t473 * t594) + 0.90D2 * t5 * (t479 * t594 / 0.2D1 - t473 * t599)
     # + t91 * t604) * t51 / 0.5760D4
      t634 = FJET(XB1, XB2, s, t426, 0.0D0, -t429, 0.0D0, 0.0D0, t633)
      t636 = t291 * t516
      t637 = t493 * x1
      t638 = t493 * t299
      t641 = Sqrt(t314 * t427 * t15)
      t642 = t24 * t641
      t643 = 0.2D1 * t642
      t648 = t293 * x2 * (t310 - t311 + t493 - t637 + t638 - 0.1D1 + t64
     #3) * t301 * t502
      t652 = t15 * s * t1 * x1 * t502
      t654 = 0.2D1 * t642 * x2
      t655 = 0.1D1 - x1 + t299 - x2 + t559 - t560 - x3 + t310 - t311 + t
     #493 - t637 + t638 + t130 + t654
      t658 = t293 * t655 * t301 * t502
      t672 = 0.1D1 + 0.3D1 * t311 + t130 * t7 - 0.4D1 * t8 * z - 0.2D1 *
     # t8 * x2 + 0.2D1 * t8 * t12 - 0.2D1 * t130 * x1 + 0.2D1 * t642 * x
     #1 + 0.3D1 * t637 + t654 - t560 + x3 - x1 - t510 - x2 + t559 + t130
      t700 = -t493 - t643 + 0.2D1 * t8 - 0.2D1 * t642 * t299 + x3 * t12 
     #* t559 - 0.2D1 * t642 * t559 - 0.2D1 * t642 * t430 + 0.4D1 * t8 * 
     #t430 - t130 * t12 * x1 + 0.3D1 * t130 * t299 - 0.2D1 * t8 * t12 * 
     #x2 + t130 * t7 * t12 - 0.2D1 * t130 * t7 * z + t430 + 0.2D1 * t642
     # * t560 - 0.3D1 * t310 + t299 - 0.4D1 * t638
      t702 = 0.1D1 / (t672 + t700)
      t703 = t558 * t702
      t704 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t294, x2, t516, x4)
      t706 = t704 * t45 * t566
      t709 = FJET(XB1, XB2, s, t636, t648, -t652, -t658, -t557, t703 * t
     #706 / 0.32D2)
      t711 = t300 * t702
      t715 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t294, x2, t516, x4)
      t717 = t715 * t45 * t566
      t720 = FJET(XB1, XB2, s, t648, t636, -t658, -t652, -t557, t703 * t
     #717 / 0.32D2)
      t725 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t516, x4)
      t730 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t516, x4)
      t742 = t515 * t725 * t53 / 0.32D2 + (0.90D2 * t5 * (-t530 * t725 +
     # t514 * t730) - 0.180D3 * t39 * t514 * t725) * t45 * t51 / 0.5760D
     #4
      t743 = FJET(XB1, XB2, s, -t504, 0.0D0, t509, 0.0D0, 0.0D0, t742)
      t745 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t294, x2, 0.0D0, x4)
      t750 = gbgbH42J2(s, XB1, XB2, z, lh, wd, t294, x2, 0.0D0, x4)
      t764 = t563 * t745 * t45 * t566 / 0.32D2 - (0.90D2 * t5 * (-t578 *
     # t750 + t575 * t562 * t745) + 0.180D3 * t39 * t578 * t745) * t51 *
     # t47 / 0.2880D4
      t765 = FJET(XB1, XB2, s, -t549, 0.0D0, t551, t291, -t557, t764)
      gbgbH4n1e0 = t193 * t192 + t289 * t288 + t376 * t375 + t423 * t422
     # + t491 * t490 + t545 * t544 + t592 * t591 + t634 * t633 + t709 * 
     #t5 * t711 * t706 / 0.32D2 + t720 * t5 * t711 * t717 / 0.32D2 + t74
     #3 * t742 + t765 * t764

      end function



      doubleprecision function gbgbH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = cos(t8)
      t30 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t30)
      t36 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t29 * t32)
      t38 = t17 * t36 + t17
      t40 = 0.1D1 / x3
      t41 = t40 * t26
      t44 = t17 * t5
      t45 = 0.1D1 / x2
      t46 = t45 * t26
      t51 = t40 * t45
      t54 = x2 ** 2
      t58 = log(0.4D1 * t54 * t13 * t10)
      t69 = log(0.4D1 * t10 * t13)
      t72 = (-0.180D3 * lh - 0.90D2 * t69) * t5
      t77 = lh ** 2
      t79 = 0.3141592653589793D1 ** 2
      t81 = t69 ** 2
      t84 = (0.180D3 * t69 * lh + 0.180D3 * t77 - 0.30D2 * t79 + 0.45D2 
     #* t81) * t5
      t87 = x3 * t10
      t90 = log(0.4D1 * t87 * t13)
      t95 = log(-0.4D1 * t87 * t13 / t30)
      t97 = t90 + t95 * t36
      t105 = -0.1D1 - t36
      t110 = (0.90D2 * t5 * (-t6 + t17 * t16) + t24) * t26 / 0.5760D4 - 
     #t5 * t38 * t41 / 0.64D2 - t44 * t46 / 0.32D2 - t5 * t38 * t51 / 0.
     #64D2 + (0.90D2 * t5 * (-t6 + t58 * t17) + t24) * t45 / 0.5760D4 - 
     #t72 * t6 / 0.11520D5 - t84 * t17 / 0.11520D5 + (0.90D2 * t44 * t97
     # + (-0.180D3 * t17 * lh + 0.90D2 * t6) * t5 * t105) * t40 / 0.1152
     #0D5
      t111 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t110)
      t113 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t114 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t120 = 0.180D3 * t22 * t114
      t125 = t114 * t36 + t114
      t129 = t5 * t114
      t158 = (0.90D2 * t5 * (-t113 + t16 * t114) + t120) * t26 / 0.5760D
     #4 - t5 * t125 * t41 / 0.64D2 - t129 * t46 / 0.32D2 - t5 * t125 * t
     #51 / 0.64D2 + (0.90D2 * t5 * (-t113 + t58 * t114) + t120) * t45 / 
     #0.5760D4 - t72 * t113 / 0.11520D5 - t84 * t114 / 0.11520D5 + (0.90
     #D2 * t129 * t97 + (-0.180D3 * t114 * lh + 0.90D2 * t113) * t5 * t1
     #05) * t40 / 0.11520D5
      t159 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t158)
      t161 = t2 * x1
      t162 = -0.1D1 + x1
      t163 = t2 * t162
      t164 = -t162
      t165 = gbgbH41J2(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.0D0, x4)
      t166 = x1 * z
      t167 = 0.1D1 - x1 + t166
      t168 = 0.1D1 / t167
      t170 = t162 ** 2
      t174 = log(0.4D1 * t11 * t13 * t168 * t170)
      t175 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.0D0, x4)
      t185 = x1 * x3
      t191 = Sqrt(-x3 * t167 * t30)
      t195 = 0.1D1 / (-0.2D1 * t185 * z + 0.2D1 * t185 - 0.1D1 - x3 + 0.
     #2D1 * t29 * t191)
      t204 = (0.90D2 * t5 * (t165 - t174 * t175) - 0.180D3 * t22 * t175)
     # * t26 / 0.5760D4 - t5 * (-t175 - t175 * t195) * t41 / 0.64D2 + t5
     # * t175 * t46 / 0.32D2
      t205 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t161, -t163, 0.0D0, t204)
      t207 = gbgbH42J2(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.0D0, x4)
      t208 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.0D0, x4)
      t226 = (0.90D2 * t5 * (t207 - t174 * t208) - 0.180D3 * t22 * t208)
     # * t26 / 0.5760D4 - t5 * (-t208 - t208 * t195) * t41 / 0.64D2 + t5
     # * t208 * t46 / 0.32D2
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t163, t161, 0.0D0, t226)
      t230 = x2 * s * t1
      t231 = -0.1D1 + x2
      t232 = t231 * s
      t233 = t232 * t1
      t234 = x2 * z
      t236 = 0.1D1 / (0.1D1 + t234 - x2)
      t237 = t5 * t236
      t238 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t247 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t253 = log(-0.4D1 * t54 * t10 * t13 * t231)
      t254 = t253 * t236
      t265 = t237 * t238 * t45 * t26 / 0.32D2 + t237 * t238 * t40 * t45 
     #/ 0.64D2 + (0.90D2 * t5 * (t236 * t247 - t254 * t238) - 0.180D3 * 
     #t22 * t236 * t238) * t45 / 0.5760D4
      t266 = FJET(XB1, XB2, s, 0.0D0, t230, 0.0D0, -t233, 0.0D0, t265)
      t268 = x2 * x3
      t271 = Sqrt(x3 * t231 * t30)
      t272 = t29 * t271
      t273 = 0.2D1 * t272
      t277 = 0.1D1 / (0.1D1 - x3 + t268)
      t279 = t2 * x2 * (-0.1D1 + t268 + t273) * t277
      t280 = t54 * x3
      t282 = 0.2D1 * t272 * x2
      t285 = t2 * (0.1D1 - x3 - x2 + t268 + t280 + t282) * t277
      t290 = 0.1D1 / (-0.1D1 - t280 + t268 - t234 + x2 - x3 - t282 + t28
     #0 * z + t273 + 0.2D1 * t272 * t234)
      t291 = t5 * t290
      t292 = t268 * t277
      t293 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t292, x4)
      t295 = t293 * t40 * t45
      t298 = FJET(XB1, XB2, s, 0.0D0, -t279, 0.0D0, t285, 0.0D0, t291 * 
     #t295 / 0.64D2)
      t305 = t2 * t162 * x2 * t168
      t307 = t232 * t1 * t162
      t308 = t1 ** 2
      t313 = s * t308 * x2 * t162 * x1 * t168
      t315 = x2 * x1
      t318 = 0.1D1 / (-0.1D1 + x2 - t315 - t234 + t315 * z - t166 + x1)
      t319 = t5 * t167 * t318
      t320 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t164, x2, 0.0D0, x4)
      t325 = FJET(XB1, XB2, s, 0.0D0, -t305, t161, t307, -t313, t319 * t
     #320 * t45 * t26 / 0.32D2)
      t332 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t341 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t353 = t237 * t332 * t45 * t26 / 0.32D2 + t237 * t332 * t40 * t45 
     #/ 0.64D2 + (0.90D2 * t5 * (t236 * t341 - t254 * t332) - 0.180D3 * 
     #t22 * t236 * t332) * t45 / 0.5760D4
      t354 = FJET(XB1, XB2, s, t230, 0.0D0, -t233, 0.0D0, 0.0D0, t353)
      t356 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t164, x2, 0.0D0, x4)
      t361 = FJET(XB1, XB2, s, -t305, 0.0D0, t307, t161, -t313, t319 * t
     #356 * t45 * t26 / 0.32D2)
      t368 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t292, x4)
      t370 = t368 * t40 * t45
      t373 = FJET(XB1, XB2, s, -t279, 0.0D0, t285, 0.0D0, 0.0D0, t291 * 
     #t370 / 0.64D2)
      gbgbH4n1em1 = t111 * t110 + t159 * t158 + t205 * t204 + t227 * t22
     #6 + t266 * t265 + t298 * t5 * t290 * t295 / 0.64D2 + t325 * t5 * t
     #167 * t318 * t320 * t46 / 0.32D2 + t354 * t353 + t361 * t5 * t167 
     #* t318 * t356 * t46 / 0.32D2 + t373 * t5 * t290 * t370 / 0.64D2

      end function



      doubleprecision function gbgbH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x2
      t14 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t18 = z ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t28 = (-0.180D3 * lh - 0.90D2 * t25) * t6
      t31 = cos(t20)
      t34 = Sqrt(-x3 * (-0.1D1 + x3))
      t41 = (-0.1D1 - 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t31 * t34)) / x3
      t44 = -t7 * t8 / 0.64D2 - t7 * t11 / 0.64D2 - t6 * t14 / 0.128D3 -
     # t28 * t3 / 0.11520D5 + t7 * t41 / 0.128D3
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t44)
      t47 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t48 = t6 * t47
      t53 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t60 = -t48 * t8 / 0.64D2 - t48 * t11 / 0.64D2 - t6 * t53 / 0.128D3
     # - t28 * t47 / 0.11520D5 + t48 * t41 / 0.128D3
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t60)
      t63 = t2 * x1
      t64 = -0.1D1 + x1
      t65 = t2 * t64
      t66 = -t64
      t67 = gbgbH41J1(s, XB1, XB2, z, lh, wd, t66, 0.0D0, 0.0D0, x4)
      t71 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t63, -t65, 0.0D0, t6 * t67 *
     # t8 / 0.64D2)
      t76 = gbgbH42J1(s, XB1, XB2, z, lh, wd, t66, 0.0D0, 0.0D0, x4)
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t65, t63, 0.0D0, t6 * t76 *
     # t8 / 0.64D2)
      t86 = x2 * s * t1
      t89 = (-0.1D1 + x2) * s * t1
      t92 = 0.1D1 / (0.1D1 + x2 * z - x2)
      t93 = t6 * t92
      t94 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t98 = FJET(XB1, XB2, s, 0.0D0, t86, 0.0D0, -t89, 0.0D0, t93 * t94 
     #* t11 / 0.64D2)
      t104 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t108 = FJET(XB1, XB2, s, t86, 0.0D0, -t89, 0.0D0, 0.0D0, t93 * t10
     #4 * t11 / 0.64D2)
      gbgbH4n1em2 = t45 * t44 + t61 * t60 + t71 * t6 * t67 * t8 / 0.64D2
     # + t80 * t6 * t76 * t8 / 0.64D2 + t98 * t6 * t92 * t94 * t11 / 0.6
     #4D2 + t108 * t6 * t92 * t104 * t11 / 0.64D2

      end function



      doubleprecision function gbgbH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t2 = s * (-0.1D1 + z)
      t3 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t3 * t6 / 
     #0.128D3)
      t12 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t15 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t6 * t12 
     #/ 0.128D3)
      gbgbH4n1em3 = -t9 * t3 * t6 / 0.128D3 - t15 * t6 * t12 / 0.128D3

      end function



      doubleprecision function gbgbH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      gbgbH4n1em4 = 0.0D0

      end function


      doubleprecision function gbgbH4n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t17 = log(0.4D1 * t15)
      t18 = t17 ** 2
      t19 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t22 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t27 = lh ** 2
      t29 = 0.3141592653589793D1 ** 2
      t31 = 0.180D3 * t27 - 0.30D2 * t29
      t32 = t31 * t5
      t36 = t18 * t17
      t45 = 0.120D3 * t27 * lh
      t47 = 0.60D2 * lh * t29
      t48 = -0.2884936567583026D3 - t45 + t47
      t49 = t48 * t5
      t50 = t49 * t19
      t52 = 0.1D1 / x1
      t55 = t7 * x3
      t56 = t14 * t10
      t59 = log(0.4D1 * t55 * t56)
      t61 = -0.1D1 + x3
      t62 = 0.1D1 / t61
      t63 = t56 * t62
      t66 = log(-0.4D1 * t55 * t63)
      t69 = x3 * z
      t70 = 0.2D1 * t69
      t71 = cos(t8)
      t73 = Sqrt(-t69 * t61)
      t77 = 0.1D1 / (-0.1D1 - t70 + 0.2D1 * t71 * t73 + x3)
      t83 = t66 ** 2
      t89 = t59 ** 2
      t95 = t19 * t77
      t99 = 0.1D1 / x3
      t103 = x2 ** 2
      t104 = t103 * x3
      t107 = log(0.4D1 * t104 * t15)
      t109 = t104 * t7
      t112 = log(-0.4D1 * t109 * t63)
      t116 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t117 = -0.1D1 + x2
      t118 = t56 * t117
      t121 = log(-0.4D1 * t109 * t118)
      t122 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t127 = t95 - t122 + t19
      t132 = 0.1D1 / x2
      t133 = t132 * t52
      t136 = t103 * t7
      t139 = log(0.4D1 * t136 * t56)
      t143 = log(-0.4D1 * t136 * t118)
      t149 = t143 ** 2
      t153 = t139 ** 2
      t159 = t19 - t122
      t166 = log(0.4D1 * t56)
      t168 = t166 ** 2
      t171 = t168 * t166
      t174 = (-t166 * t31 - 0.90D2 * t168 * lh - 0.2884936567583026D3 - 
     #t45 + t47 - 0.15D2 * t171) * t5
      t177 = t168 ** 2
      t181 = t29 ** 2
      t182 = t27 ** 2
      t191 = (0.15D2 / 0.4D1 * t177 - t166 * t48 + 0.5769873135166051D3 
     #* lh + t181 + 0.60D2 * t182 - 0.60D2 * t27 * t29 + t168 * t31 / 0.
     #2D1 + 0.30D2 * t171 * lh) * t5
      t199 = x3 * t14
      t203 = log(-0.4D1 * t199 * t10 * t62)
      t204 = t203 ** 2
      t208 = log(0.4D1 * t199 * t10)
      t209 = t208 ** 2
      t211 = t204 * t77 / 0.2D1 + t209 / 0.2D1
      t218 = -t208 - t203 * t77
      t225 = -t204 * t203 * t77 / 0.6D1 - t209 * t208 / 0.6D1
      t230 = 0.1D1 + t77
      t235 = t14 * t103
      t238 = log(0.4D1 * t235 * t10)
      t240 = t238 ** 2
      t243 = t10 * t117
      t246 = log(-0.4D1 * t235 * t243)
      t248 = t246 ** 2
      t260 = t240 * t238
      t265 = t248 * t246
      t277 = log(0.4D1 * t104 * t56)
      t281 = log(-0.4D1 * t104 * t118)
      t285 = log(-0.4D1 * t104 * t63)
      t294 = t281 ** 2
      t298 = t285 ** 2
      t303 = t277 ** 2
      t314 = -(-0.180D3 * t6 * (t18 * t19 / 0.2D1 - t17 * t22) + t32 * (
     #t22 - t17 * t19) + 0.90D2 * t5 * (-t36 * t19 / 0.6D1 + t18 * t22 /
     # 0.2D1) + t50) * t52 / 0.5760D4 - (-0.180D3 * t6 * (t22 - t59 * t1
     #9 + (t22 - t66 * t19) * t77) + 0.90D2 * t5 * ((-t66 * t22 + t83 * 
     #t19 / 0.2D1) * t77 - t59 * t22 + t89 * t19 / 0.2D1) + t32 * (t19 +
     # t95)) * t99 * t52 / 0.5760D4 - (0.90D2 * t5 * (t22 - t107 * t19 +
     # (t22 - t112 * t19) * t77 - t116 + t121 * t122) - 0.180D3 * t6 * t
     #127) * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (t22 - t139 * t19 
     #- t116 + t143 * t122) + 0.90D2 * t5 * (t143 * t116 - t149 * t122 /
     # 0.2D1 - t139 * t22 + t153 * t19 / 0.2D1) + t32 * t159) * t132 * t
     #52 / 0.2880D4 - t174 * t22 / 0.11520D5 - t191 * t19 / 0.11520D5 - 
     #((0.90D2 * t5 * t22 - 0.180D3 * t6 * t19) * t211 + (-0.180D3 * t6 
     #* t22 + t32 * t19) * t218 + 0.90D2 * t5 * t19 * t225 + (t32 * t22 
     #+ t50) * t230) * t99 / 0.11520D5 - (-0.180D3 * t6 * (-t238 * t22 +
     # t240 * t19 / 0.2D1 + t246 * t116 - t248 * t122 / 0.2D1) + t32 * (
     #-t116 + t246 * t122 + t22 - t238 * t19) + 0.90D2 * t5 * (t240 * t2
     #2 / 0.2D1 - t260 * t19 / 0.6D1 - t248 * t116 / 0.2D1 + t265 * t122
     # / 0.6D1) + t49 * t159) * t132 / 0.5760D4 - (-0.180D3 * t6 * (-t27
     #7 * t19 + t22 + t281 * t122 - t116 + (t22 - t285 * t19) * t77) + 0
     #.90D2 * t5 * (t281 * t116 - t277 * t22 - t294 * t122 / 0.2D1 + (-t
     #285 * t22 + t298 * t19 / 0.2D1) * t77 + t303 * t19 / 0.2D1) + t32 
     #* t127) * t99 * t132 / 0.5760D4
      t315 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t314)
      t317 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t320 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t338 = t49 * t320
      t381 = t320 * t77
      t391 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t392 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t398 = -t392 + t320 + t381
      t419 = t320 - t392
      t480 = -t174 * t317 / 0.11520D5 - t191 * t320 / 0.11520D5 - ((0.90
     #D2 * t5 * t317 - 0.180D3 * t6 * t320) * t211 + (-0.180D3 * t6 * t3
     #17 + t32 * t320) * t218 + 0.90D2 * t5 * t320 * t225 + (t32 * t317 
     #+ t338) * t230) * t99 / 0.11520D5 - (-0.180D3 * t6 * (-t17 * t317 
     #+ t18 * t320 / 0.2D1) + t32 * (t317 - t17 * t320) + 0.90D2 * t5 * 
     #(t18 * t317 / 0.2D1 - t36 * t320 / 0.6D1) + t338) * t52 / 0.5760D4
     # - (-0.180D3 * t6 * (t317 - t59 * t320 + (t317 - t66 * t320) * t77
     #) + 0.90D2 * t5 * ((-t66 * t317 + t83 * t320 / 0.2D1) * t77 - t59 
     #* t317 + t89 * t320 / 0.2D1) + t32 * (t320 + t381)) * t99 * t52 / 
     #0.5760D4 - (0.90D2 * t5 * ((t317 - t112 * t320) * t77 + t317 - t39
     #1 + t121 * t392 - t107 * t320) - 0.180D3 * t6 * t398) * t99 * t133
     # / 0.2880D4 - (-0.180D3 * t6 * (t317 - t139 * t320 - t391 + t143 *
     # t392) + 0.90D2 * t5 * (t143 * t391 - t149 * t392 / 0.2D1 - t139 *
     # t317 + t153 * t320 / 0.2D1) + t32 * t419) * t132 * t52 / 0.2880D4
     # - (-0.180D3 * t6 * (-t238 * t317 + t240 * t320 / 0.2D1 + t246 * t
     #391 - t248 * t392 / 0.2D1) + t32 * (-t391 + t246 * t392 + t317 - t
     #238 * t320) + 0.90D2 * t5 * (t240 * t317 / 0.2D1 - t260 * t320 / 0
     #.6D1 - t248 * t391 / 0.2D1 + t265 * t392 / 0.6D1) + t49 * t419) * 
     #t132 / 0.5760D4 - (-0.180D3 * t6 * (t317 - t277 * t320 - t391 + t2
     #81 * t392 + (t317 - t285 * t320) * t77) + 0.90D2 * t5 * (t281 * t3
     #91 - t294 * t392 / 0.2D1 - t277 * t317 + t303 * t320 / 0.2D1 + (-t
     #285 * t317 + t298 * t320 / 0.2D1) * t77) + t32 * t398) * t99 * t13
     #2 / 0.5760D4
      t481 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t480)
      t483 = t2 * x1
      t484 = -0.1D1 + x1
      t485 = t2 * t484
      t486 = 0.1D1 / t12
      t487 = x1 * z
      t488 = -z - x1 + t487
      t489 = 0.1D1 / t488
      t490 = t486 * t489
      t491 = t484 ** 2
      t492 = t490 * t491
      t495 = log(-0.4D1 * t11 * t492)
      t496 = t495 ** 2
      t497 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t500 = gbgbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t508 = t496 * t495
      t520 = t55 * t10
      t523 = log(-0.4D1 * t520 * t492)
      t526 = t490 * t491 * t62
      t529 = log(0.4D1 * t520 * t526)
      t532 = x1 * x3
      t533 = t532 * z
      t536 = x3 * t488
      t538 = Sqrt(t536 * t61)
      t542 = 0.1D1 / (0.2D1 * t533 - 0.2D1 * t532 - t70 + 0.2D1 * t71 * 
     #t538 - 0.1D1 + x3)
      t548 = t529 ** 2
      t554 = t523 ** 2
      t561 = -t497 * t542 - t497
      t568 = t489 * t491
      t572 = log(-0.4D1 * t109 * t10 * t486 * t568)
      t574 = t104 * t11
      t577 = log(0.4D1 * t574 * t526)
      t590 = t136 * t10
      t593 = log(-0.4D1 * t590 * t492)
      t599 = t593 ** 2
      t610 = -(-0.180D3 * t6 * (-t496 * t497 / 0.2D1 + t495 * t500) + t3
     #2 * (-t500 + t495 * t497) + 0.90D2 * t5 * (t508 * t497 / 0.6D1 - t
     #496 * t500 / 0.2D1) - t49 * t497) * t52 / 0.5760D4 - (-0.180D3 * t
     #6 * (-t500 + t523 * t497 - (t500 - t529 * t497) * t542) + 0.90D2 *
     # t5 * (-(-t529 * t500 + t548 * t497 / 0.2D1) * t542 + t523 * t500 
     #- t554 * t497 / 0.2D1) + t32 * t561) * t99 * t52 / 0.5760D4 - (0.9
     #0D2 * t5 * (-t500 + t572 * t497 - (t500 - t577 * t497) * t542) - 0
     #.180D3 * t6 * t561) * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (t5
     #93 * t497 - t500) + 0.90D2 * t5 * (t593 * t500 - t599 * t497 / 0.2
     #D1) - t32 * t497) * t132 * t52 / 0.2880D4
      t611 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t483, -t485, 0.0D0, t610)
      t613 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t616 = gbgbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t654 = -t613 * t542 - t613
      t688 = -(-0.180D3 * t6 * (-t496 * t613 / 0.2D1 + t495 * t616) + t3
     #2 * (-t616 + t495 * t613) + 0.90D2 * t5 * (t508 * t613 / 0.6D1 - t
     #496 * t616 / 0.2D1) - t49 * t613) * t52 / 0.5760D4 - (-0.180D3 * t
     #6 * (-t616 + t523 * t613 - (t616 - t529 * t613) * t542) + 0.90D2 *
     # t5 * (-(-t529 * t616 + t548 * t613 / 0.2D1) * t542 + t523 * t616 
     #- t554 * t613 / 0.2D1) + t32 * t654) * t99 * t52 / 0.5760D4 - (0.9
     #0D2 * t5 * (-(t616 - t577 * t613) * t542 - t616 + t572 * t613) - 0
     #.180D3 * t6 * t654) * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (-t
     #616 + t593 * t613) + 0.90D2 * t5 * (t593 * t616 - t599 * t613 / 0.
     #2D1) - t32 * t613) * t132 * t52 / 0.2880D4
      t689 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t485, t483, 0.0D0, t688)
      t691 = x2 * x3
      t692 = 0.1D1 - x3 + t691
      t693 = 0.1D1 / t692
      t694 = t691 * t693
      t695 = t2 * t694
      t697 = t2 * t61 * t693
      t699 = t692 ** 2
      t700 = 0.1D1 / t699
      t701 = t61 * t700
      t705 = log(0.4D1 * t574 * t14 * t117 * t701)
      t706 = t691 * z
      t707 = t117 * t61
      t709 = Sqrt(t69 * t707)
      t713 = 0.1D1 / (t706 - t70 + 0.2D1 * t71 * t709 - 0.1D1 + x3)
      t714 = t705 * t713
      t715 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t694, x4)
      t717 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t694, x4)
      t718 = t713 * t717
      t722 = t713 * t715
      t733 = log(0.4D1 * t104 * t14 * t243 * t701)
      t734 = t733 * t713
      t739 = t733 ** 2
      t740 = t739 * t713
      t752 = -(0.90D2 * t5 * (t714 * t715 - t718) + 0.180D3 * t6 * t722)
     # * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (t734 * t715 - t718) +
     # 0.90D2 * t5 * (-t740 * t715 / 0.2D1 + t734 * t717) - t32 * t722) 
     #* t99 * t132 / 0.5760D4
      t753 = FJET(XB1, XB2, s, 0.0D0, t695, 0.0D0, -t697, 0.0D0, t752)
      t755 = x2 * x1
      t757 = t2 * t755 * t489
      t760 = t117 * s * t1 * x1
      t761 = t1 ** 2
      t766 = s * t761 * x2 * x1 * t484 * t489
      t767 = t755 * z
      t769 = 0.1D1 / (z + x1 - t487 - t755 + t767)
      t770 = t488 * t769
      t771 = gbgbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t772 = t770 * t771
      t774 = t490 * t491 * t117
      t777 = log(0.4D1 * t574 * t774)
      t778 = t777 * t488
      t779 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t780 = t769 * t779
      t785 = t770 * t779
      t793 = log(0.4D1 * t590 * t774)
      t794 = t793 * t488
      t801 = t793 ** 2
      t802 = t801 * t488
      t813 = -(0.90D2 * t5 * (-t772 + t778 * t780) + 0.180D3 * t6 * t785
     #) * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (t794 * t780 - t772) 
     #+ 0.90D2 * t5 * (t794 * t769 * t771 - t802 * t780 / 0.2D1) - t32 *
     # t785) * t132 * t52 / 0.2880D4
      t814 = FJET(XB1, XB2, s, 0.0D0, -t757, -t485, -t760, t766, t813)
      t816 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t694, x4)
      t817 = t713 * t816
      t818 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t694, x4)
      t823 = t713 * t818
      t845 = -(0.90D2 * t5 * (-t817 + t714 * t818) + 0.180D3 * t6 * t823
     #) * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (t734 * t818 - t817) 
     #+ 0.90D2 * t5 * (-t740 * t818 / 0.2D1 + t734 * t816) - t32 * t823)
     # * t99 * t132 / 0.5760D4
      t846 = FJET(XB1, XB2, s, t695, 0.0D0, -t697, 0.0D0, 0.0D0, t845)
      t848 = t691 * x1
      t849 = t691 * t487
      t851 = Sqrt(-t536 * t707)
      t852 = t71 * t851
      t858 = t483 * x2 * (-t69 - t532 + t533 + t706 + t848 - t849 - 0.1D
     #1 + x3 + 0.2D1 * t852) * t489 * t693
      t859 = t485 * t694
      t860 = x2 * z
      t863 = z + x1 - t487 - t860 - t755 + t767 - t69 - t532 + t533 + t7
     #06 + t848 - t849 + t104 + 0.2D1 * t852 * x2
      t866 = t483 * t863 * t489 * t693
      t870 = t61 * s * t1 * t484 * t693
      t877 = log(-0.4D1 * t104 * t11 * t486 * t568 * t707 * t700)
      t878 = t877 * t488
      t889 = x3 * t12
      t902 = 0.2D1 * t852 * t767 + 0.4D1 * t849 - 0.2D1 * t852 * t755 + 
     #0.2D1 * t55 * t12 * x2 - 0.4D1 * t55 * t860 - 0.3D1 * t889 * t755 
     #- t104 * t487 - 0.2D1 * t852 * t487 - t104 * t7 * t12 + 0.2D1 * t1
     #04 * t7 * z + t104 * t12 * x1 + t487 + t532 - t767 - t848
      t919 = 0.2D1 * t852 * z - t109 + t691 * t12 + 0.2D1 * t852 * x1 + 
     #0.4D1 * t889 * x1 + 0.4D1 * t55 * z + 0.2D1 * t55 * x2 - 0.2D1 * t
     #55 * t12 - z - x1 + t755 - 0.5D1 * t533 + t69 - 0.2D1 * t55 - 0.2D
     #1 * t889
      t921 = 0.1D1 / (t902 + t919)
      t922 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, x2, t694, x4)
      t925 = t488 * t921
      t926 = gbgbH42J2(s, XB1, XB2, z, lh, wd, x1, x2, t694, x4)
      t934 = 0.90D2 * t5 * (t878 * t921 * t922 - t925 * t926) + 0.180D3 
     #* t6 * t925 * t922
      t938 = FJET(XB1, XB2, s, t858, -t859, -t866, t870, t766, -t934 * t
     #99 * t133 / 0.2880D4)
      t941 = t99 * t132 * t52
      t944 = gbgbH42J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t945 = t770 * t944
      t946 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t947 = t769 * t946
      t952 = t770 * t946
      t974 = -(0.90D2 * t5 * (-t945 + t778 * t947) + 0.180D3 * t6 * t952
     #) * t99 * t133 / 0.2880D4 - (-0.180D3 * t6 * (t794 * t947 - t945) 
     #+ 0.90D2 * t5 * (t794 * t769 * t944 - t802 * t947 / 0.2D1) - t32 *
     # t952) * t132 * t52 / 0.2880D4
      t975 = FJET(XB1, XB2, s, -t757, 0.0D0, -t760, -t485, t766, t974)
      t977 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, t694, x4)
      t980 = gbgbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, t694, x4)
      t988 = 0.90D2 * t5 * (t878 * t921 * t977 - t925 * t980) + 0.180D3 
     #* t6 * t925 * t977
      t992 = FJET(XB1, XB2, s, -t859, t858, t870, -t866, t766, -t988 * t
     #99 * t133 / 0.2880D4)
      gbgbH4n2e1 = t315 * t314 + t481 * t480 + t611 * t610 + t689 * t688
     # + t753 * t752 + t814 * t813 + t846 * t845 - t938 * t934 * t941 / 
     #0.2880D4 + t975 * t974 - t992 * t988 * t941 / 0.2880D4

      end function



      doubleprecision function gbgbH4n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t23 = t15 * t22
      t26 = log(-0.4D1 * t8 * t23)
      t29 = x3 * z
      t30 = 0.2D1 * t29
      t31 = cos(t12)
      t33 = Sqrt(-t29 * t21)
      t37 = 0.1D1 / (-0.1D1 - t30 + 0.2D1 * t31 * t33 + x3)
      t42 = lh * t5
      t43 = t19 * t37
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t53 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t54 = t43 - t53 + t19
      t56 = 0.1D1 / x2
      t58 = t48 * t56 * t50
      t61 = x2 ** 2
      t62 = t61 * t7
      t65 = log(0.4D1 * t62 * t15)
      t67 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t68 = -0.1D1 + x2
      t69 = t15 * t68
      t72 = log(-0.4D1 * t62 * t69)
      t77 = t19 - t53
      t84 = t7 * t14
      t87 = log(0.4D1 * t84 * t11)
      t92 = t87 ** 2
      t99 = lh ** 2
      t100 = 0.180D3 * t99
      t101 = 0.3141592653589793D1 ** 2
      t102 = 0.30D2 * t101
      t103 = t100 - t102
      t104 = t103 * t5
      t105 = t104 * t19
      t114 = x3 * t11
      t117 = log(0.4D1 * t114 * t14)
      t121 = log(-0.4D1 * t114 * t14 * t22)
      t123 = -t117 - t121 * t37
      t126 = t121 ** 2
      t128 = t117 ** 2
      t130 = t126 * t37 / 0.2D1 + t128 / 0.2D1
      t136 = 0.1D1 + t37
      t141 = t61 * x3
      t144 = log(0.4D1 * t141 * t15)
      t148 = log(-0.4D1 * t141 * t69)
      t152 = log(-0.4D1 * t141 * t23)
      t165 = t11 * t61
      t166 = t14 * t68
      t169 = log(-0.4D1 * t165 * t166)
      t173 = log(0.4D1 * t165 * t14)
      t179 = t173 ** 2
      t183 = t169 ** 2
      t194 = log(0.4D1 * t15)
      t197 = t194 ** 2
      t200 = (0.180D3 * t194 * lh + t100 - t102 + 0.45D2 * t197) * t5
      t213 = (-t194 * t103 - 0.90D2 * t197 * lh - 0.2884936567583026D3 -
     # 0.120D3 * t99 * lh + 0.60D2 * lh * t101 - 0.15D2 * t197 * t194) *
     # t5
      t216 = -(0.90D2 * t5 * (t6 - t18 * t19 + (t6 - t26 * t19) * t37) -
     # 0.180D3 * t42 * (t19 + t43)) * t48 * t50 / 0.5760D4 - t5 * t54 * 
     #t58 / 0.32D2 - (0.90D2 * t5 * (t6 - t65 * t19 - t67 + t72 * t53) -
     # 0.180D3 * t42 * t77) * t56 * t50 / 0.2880D4 - (-0.180D3 * t42 * (
     #t6 - t87 * t19) + 0.90D2 * t5 * (t92 * t19 / 0.2D1 - t87 * t6) + t
     #105) * t50 / 0.5760D4 - ((0.90D2 * t5 * t6 - 0.180D3 * t42 * t19) 
     #* t123 + 0.90D2 * t5 * t19 * t130 + (-0.180D3 * t42 * t6 + t105) *
     # t136) * t48 / 0.11520D5 - (0.90D2 * t5 * (-t144 * t19 + t6 + t148
     # * t53 - t67 + (t6 - t152 * t19) * t37) - 0.180D3 * t42 * t54) * t
     #48 * t56 / 0.5760D4 - (-0.180D3 * t42 * (-t67 + t169 * t53 + t6 - 
     #t173 * t19) + 0.90D2 * t5 * (-t173 * t6 + t179 * t19 / 0.2D1 + t16
     #9 * t67 - t183 * t53 / 0.2D1) + t104 * t77) * t56 / 0.5760D4 - t20
     #0 * t6 / 0.11520D5 - t213 * t19 / 0.11520D5
      t217 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t216)
      t219 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t220 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t228 = t220 * t37
      t236 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t237 = -t236 + t220 + t228
      t242 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t247 = t220 - t236
      t264 = t104 * t220
      t320 = -(0.90D2 * t5 * (t219 - t18 * t220 + (t219 - t26 * t220) * 
     #t37) - 0.180D3 * t42 * (t220 + t228)) * t48 * t50 / 0.5760D4 - t5 
     #* t237 * t58 / 0.32D2 - (0.90D2 * t5 * (t219 - t65 * t220 - t242 +
     # t72 * t236) - 0.180D3 * t42 * t247) * t56 * t50 / 0.2880D4 - (-0.
     #180D3 * t42 * (t219 - t87 * t220) + 0.90D2 * t5 * (-t87 * t219 + t
     #92 * t220 / 0.2D1) + t264) * t50 / 0.5760D4 - ((0.90D2 * t5 * t219
     # - 0.180D3 * t42 * t220) * t123 + 0.90D2 * t5 * t220 * t130 + (-0.
     #180D3 * t42 * t219 + t264) * t136) * t48 / 0.11520D5 - (0.90D2 * t
     #5 * (t219 - t144 * t220 - t242 + t148 * t236 + (t219 - t152 * t220
     #) * t37) - 0.180D3 * t42 * t237) * t48 * t56 / 0.5760D4 - (-0.180D
     #3 * t42 * (-t242 + t169 * t236 + t219 - t173 * t220) + 0.90D2 * t5
     # * (-t173 * t219 + t179 * t220 / 0.2D1 + t169 * t242 - t183 * t236
     # / 0.2D1) + t104 * t247) * t56 / 0.5760D4 - t200 * t219 / 0.11520D
     #5 - t213 * t220 / 0.11520D5
      t321 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t320)
      t323 = t2 * x1
      t324 = -0.1D1 + x1
      t325 = t2 * t324
      t326 = gbgbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t327 = t8 * t14
      t329 = x1 * z
      t330 = -z - x1 + t329
      t331 = 0.1D1 / t330
      t332 = 0.1D1 / t9 * t331
      t333 = t324 ** 2
      t334 = t332 * t333
      t337 = log(-0.4D1 * t327 * t334)
      t338 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t344 = log(0.4D1 * t327 * t332 * t333 * t22)
      t347 = x1 * x3
      t348 = t347 * z
      t351 = x3 * t330
      t353 = Sqrt(t351 * t21)
      t357 = 0.1D1 / (0.2D1 * t348 - 0.2D1 * t347 - t30 + 0.2D1 * t31 * 
     #t353 - 0.1D1 + x3)
      t363 = -t338 * t357 - t338
      t373 = t62 * t14
      t376 = log(-0.4D1 * t373 * t334)
      t389 = log(-0.4D1 * t84 * t334)
      t394 = t389 ** 2
      t405 = -(0.90D2 * t5 * (-t326 + t337 * t338 - (t326 - t344 * t338)
     # * t357) - 0.180D3 * t42 * t363) * t48 * t50 / 0.5760D4 - t5 * t36
     #3 * t58 / 0.32D2 - (0.90D2 * t5 * (t376 * t338 - t326) + 0.180D3 *
     # t42 * t338) * t56 * t50 / 0.2880D4 - (-0.180D3 * t42 * (-t326 + t
     #389 * t338) + 0.90D2 * t5 * (-t394 * t338 / 0.2D1 + t389 * t326) -
     # t104 * t338) * t50 / 0.5760D4
      t406 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t323, -t325, 0.0D0, t405)
      t408 = gbgbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t409 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t418 = -t409 * t357 - t409
      t452 = -(0.90D2 * t5 * (-t408 + t337 * t409 - (t408 - t344 * t409)
     # * t357) - 0.180D3 * t42 * t418) * t48 * t50 / 0.5760D4 - t5 * t41
     #8 * t58 / 0.32D2 - (0.90D2 * t5 * (-t408 + t376 * t409) + 0.180D3 
     #* t42 * t409) * t56 * t50 / 0.2880D4 - (-0.180D3 * t42 * (-t408 + 
     #t389 * t409) + 0.90D2 * t5 * (-t394 * t409 / 0.2D1 + t389 * t408) 
     #- t104 * t409) * t50 / 0.5760D4
      t453 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t325, t323, 0.0D0, t452)
      t455 = x2 * x3
      t456 = 0.1D1 - x3 + t455
      t457 = 0.1D1 / t456
      t458 = t455 * t457
      t459 = t2 * t458
      t461 = t2 * t21 * t457
      t462 = t455 * z
      t463 = t68 * t21
      t465 = Sqrt(t29 * t463)
      t469 = 0.1D1 / (t462 - t30 + 0.2D1 * t31 * t465 - 0.1D1 + x3)
      t470 = t5 * t469
      t471 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t458, x4)
      t476 = t456 ** 2
      t482 = log(0.4D1 * t141 * t11 * t166 * t21 / t476)
      t483 = t482 * t469
      t485 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t458, x4)
      t497 = t470 * t471 * t58 / 0.32D2 - (0.90D2 * t5 * (t483 * t471 - 
     #t469 * t485) + 0.180D3 * t42 * t469 * t471) * t48 * t56 / 0.5760D4
      t498 = FJET(XB1, XB2, s, 0.0D0, t459, 0.0D0, -t461, 0.0D0, t497)
      t500 = x2 * x1
      t502 = t2 * t500 * t331
      t505 = t68 * s * t1 * x1
      t506 = t1 ** 2
      t511 = s * t506 * x2 * x1 * t324 * t331
      t512 = t5 * t330
      t513 = t500 * z
      t515 = 0.1D1 / (z + x1 - t329 - t500 + t513)
      t516 = t512 * t515
      t517 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t519 = t56 * t50
      t527 = log(0.4D1 * t373 * t332 * t333 * t68)
      t528 = t527 * t330
      t531 = t330 * t515
      t532 = gbgbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t544 = t516 * t517 * t48 * t519 / 0.32D2 - (0.90D2 * t5 * (t528 * 
     #t515 * t517 - t531 * t532) + 0.180D3 * t42 * t531 * t517) * t56 * 
     #t50 / 0.2880D4
      t545 = FJET(XB1, XB2, s, 0.0D0, -t502, -t325, -t505, t511, t544)
      t547 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t458, x4)
      t552 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t458, x4)
      t564 = t470 * t547 * t58 / 0.32D2 - (0.90D2 * t5 * (t483 * t547 - 
     #t469 * t552) + 0.180D3 * t42 * t469 * t547) * t48 * t56 / 0.5760D4
      t565 = FJET(XB1, XB2, s, t459, 0.0D0, -t461, 0.0D0, 0.0D0, t564)
      t567 = t455 * x1
      t568 = t455 * t329
      t570 = Sqrt(-t351 * t463)
      t571 = t31 * t570
      t577 = t323 * x2 * (-t29 - t347 + t348 + t462 + t567 - t568 - 0.1D
     #1 + x3 + 0.2D1 * t571) * t331 * t457
      t578 = t325 * t458
      t579 = x2 * z
      t582 = z + x1 - t329 - t579 - t500 + t513 - t29 - t347 + t348 + t4
     #62 + t567 - t568 + t141 + 0.2D1 * t571 * x2
      t585 = t323 * t582 * t331 * t457
      t589 = t21 * s * t1 * t324 * t457
      t597 = x3 * t9
      t603 = -t567 + 0.2D1 * t571 * z + t455 * t9 - t141 * t7 + 0.2D1 * 
     #t571 * x1 - t513 - 0.2D1 * t8 - 0.2D1 * t597 + t500 - 0.5D1 * t348
     # + t329 + t347 - x1 + 0.4D1 * t568 - 0.2D1 * t571 * t500
      t631 = -0.3D1 * t597 * t500 - 0.4D1 * t8 * t579 + 0.2D1 * t8 * t9 
     #* x2 - t141 * t329 + t141 * t9 * x1 + 0.2D1 * t141 * t7 * z - t141
     # * t7 * t9 - 0.2D1 * t571 * t329 - z + t29 + 0.2D1 * t571 * t513 +
     # 0.4D1 * t597 * x1 + 0.4D1 * t8 * z + 0.2D1 * t8 * x2 - 0.2D1 * t8
     # * t9
      t633 = 0.1D1 / (t603 + t631)
      t634 = t512 * t633
      t635 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, x2, t458, x4)
      t637 = t635 * t48 * t519
      t640 = FJET(XB1, XB2, s, t577, -t578, -t585, t589, t511, t634 * t6
     #37 / 0.32D2)
      t642 = t330 * t633
      t646 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t653 = gbgbH42J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t665 = t516 * t646 * t48 * t519 / 0.32D2 - (0.90D2 * t5 * (t528 * 
     #t515 * t646 - t531 * t653) + 0.180D3 * t42 * t531 * t646) * t56 * 
     #t50 / 0.2880D4
      t666 = FJET(XB1, XB2, s, -t502, 0.0D0, -t505, -t325, t511, t665)
      t668 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, t458, x4)
      t670 = t668 * t48 * t519
      t673 = FJET(XB1, XB2, s, -t578, t577, t589, -t585, t511, t634 * t6
     #70 / 0.32D2)
      gbgbH4n2e0 = t217 * t216 + t321 * t320 + t406 * t405 + t453 * t452
     # + t498 * t497 + t545 * t544 + t565 * t564 + t640 * t5 * t642 * t6
     #37 / 0.32D2 + t666 * t665 + t673 * t5 * t642 * t670 / 0.32D2

      end function



      doubleprecision function gbgbH4n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t17 = log(0.4D1 * t11 * t14)
      t18 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t23 = lh * t5
      t25 = 0.180D3 * t23 * t18
      t27 = 0.1D1 / x1
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t8)
      t33 = -0.1D1 + x3
      t35 = Sqrt(-t30 * t33)
      t39 = 0.1D1 / (-0.1D1 - t31 + 0.2D1 * t32 * t35 + x3)
      t40 = t18 * t39
      t43 = 0.1D1 / x3
      t44 = t43 * t27
      t47 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t48 = t18 - t47
      t50 = 0.1D1 / x2
      t51 = t50 * t27
      t56 = t43 * t50
      t59 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t60 = x2 ** 2
      t61 = t14 * t60
      t62 = -0.1D1 + x2
      t66 = log(-0.4D1 * t61 * t10 * t62)
      t70 = log(0.4D1 * t61 * t10)
      t83 = log(0.4D1 * t14 * t10)
      t86 = (-0.180D3 * lh - 0.90D2 * t83) * t5
      t91 = lh ** 2
      t93 = 0.3141592653589793D1 ** 2
      t95 = t83 ** 2
      t98 = (0.180D3 * t83 * lh + 0.180D3 * t91 - 0.30D2 * t93 + 0.45D2 
     #* t95) * t5
      t102 = x3 * t14
      t105 = log(0.4D1 * t102 * t10)
      t110 = log(-0.4D1 * t102 * t10 / t33)
      t112 = -t105 - t110 * t39
      t118 = 0.1D1 + t39
      t123 = -(0.90D2 * t5 * (t6 - t18 * t17) - t25) * t27 / 0.5760D4 - 
     #t5 * (t18 + t40) * t44 / 0.64D2 - t5 * t48 * t51 / 0.32D2 - t5 * (
     #t40 - t47 + t18) * t56 / 0.64D2 - (0.90D2 * t5 * (-t59 + t66 * t47
     # + t6 - t70 * t18) - 0.180D3 * t23 * t48) * t50 / 0.5760D4 - t86 *
     # t6 / 0.11520D5 - t98 * t18 / 0.11520D5 - (0.90D2 * t5 * t18 * t11
     #2 + (0.90D2 * t5 * t6 - t25) * t118) * t43 / 0.11520D5
      t124 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t123)
      t126 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t127 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t133 = 0.180D3 * t23 * t127
      t137 = t127 * t39
      t142 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t143 = t127 - t142
      t151 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t176 = -(0.90D2 * t5 * (t126 - t17 * t127) - t133) * t27 / 0.5760D
     #4 - t5 * (t127 + t137) * t44 / 0.64D2 - t5 * t143 * t51 / 0.32D2 -
     # t5 * (-t142 + t127 + t137) * t56 / 0.64D2 - (0.90D2 * t5 * (-t151
     # + t66 * t142 + t126 - t70 * t127) - 0.180D3 * t23 * t143) * t50 /
     # 0.5760D4 - t86 * t126 / 0.11520D5 - t98 * t127 / 0.11520D5 - (0.9
     #0D2 * t5 * t127 * t112 + (0.90D2 * t5 * t126 - t133) * t118) * t43
     # / 0.11520D5
      t177 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t176)
      t179 = t2 * x1
      t180 = -0.1D1 + x1
      t181 = t2 * t180
      t182 = gbgbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t184 = x1 * z
      t185 = -z - x1 + t184
      t186 = 0.1D1 / t185
      t188 = t180 ** 2
      t192 = log(-0.4D1 * t11 / t12 * t186 * t188)
      t193 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t203 = x1 * x3
      t209 = Sqrt(x3 * t185 * t33)
      t213 = 0.1D1 / (0.2D1 * t203 * z - 0.2D1 * t203 - t31 + 0.2D1 * t3
     #2 * t209 - 0.1D1 + x3)
      t222 = -(0.90D2 * t5 * (-t182 + t192 * t193) + 0.180D3 * t23 * t19
     #3) * t27 / 0.5760D4 - t5 * (-t193 * t213 - t193) * t44 / 0.64D2 + 
     #t5 * t193 * t51 / 0.32D2
      t223 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t179, -t181, 0.0D0, t222)
      t225 = gbgbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t226 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t244 = -(0.90D2 * t5 * (-t225 + t192 * t226) + 0.180D3 * t23 * t22
     #6) * t27 / 0.5760D4 - t5 * (-t226 * t213 - t226) * t44 / 0.64D2 + 
     #t5 * t226 * t51 / 0.32D2
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t181, t179, 0.0D0, t244)
      t247 = x2 * x3
      t249 = 0.1D1 / (0.1D1 - x3 + t247)
      t250 = t247 * t249
      t251 = t2 * t250
      t253 = t2 * t33 * t249
      t257 = Sqrt(t30 * t62 * t33)
      t261 = 0.1D1 / (t247 * z - t31 + 0.2D1 * t32 * t257 - 0.1D1 + x3)
      t262 = t5 * t261
      t263 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t250, x4)
      t265 = t263 * t43 * t50
      t268 = FJET(XB1, XB2, s, 0.0D0, t251, 0.0D0, -t253, 0.0D0, t262 * 
     #t265 / 0.64D2)
      t273 = x2 * x1
      t275 = t2 * t273 * t186
      t278 = t62 * s * t1 * x1
      t279 = t1 ** 2
      t284 = s * t279 * x2 * x1 * t180 * t186
      t288 = 0.1D1 / (z + x1 - t184 - t273 + t273 * z)
      t289 = t5 * t185 * t288
      t290 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t295 = FJET(XB1, XB2, s, 0.0D0, -t275, -t181, -t278, t284, t289 * 
     #t290 * t50 * t27 / 0.32D2)
      t302 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t307 = FJET(XB1, XB2, s, -t275, 0.0D0, -t278, -t181, t284, t289 * 
     #t302 * t50 * t27 / 0.32D2)
      t314 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t250, x4)
      t316 = t314 * t43 * t50
      t319 = FJET(XB1, XB2, s, t251, 0.0D0, -t253, 0.0D0, 0.0D0, t262 * 
     #t316 / 0.64D2)
      gbgbH4n2em1 = t124 * t123 + t177 * t176 + t223 * t222 + t245 * t24
     #4 + t268 * t5 * t261 * t265 / 0.64D2 + t295 * t5 * t185 * t288 * t
     #290 * t51 / 0.32D2 + t307 * t5 * t185 * t288 * t302 * t51 / 0.32D2
     # + t319 * t5 * t261 * t316 / 0.64D2

      end function



      doubleprecision function gbgbH4n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t14 = 0.1D1 / x2
      t17 = gbgbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t32 = (-0.180D3 * lh - 0.90D2 * t29) * t5
      t35 = x3 * z
      t37 = cos(t24)
      t40 = Sqrt(-t35 * (-0.1D1 + x3))
      t47 = (0.1D1 + 0.1D1 / (-0.1D1 - 0.2D1 * t35 + 0.2D1 * t37 * t40 +
     # x3)) / x3
      t50 = -t7 * t8 / 0.64D2 - t5 * (t6 - t11) * t14 / 0.64D2 - t5 * t1
     #7 / 0.128D3 - t32 * t6 / 0.11520D5 - t7 * t47 / 0.128D3
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t54 = t5 * t53
      t57 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t62 = gbgbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t69 = -t54 * t8 / 0.64D2 - t5 * (t53 - t57) * t14 / 0.64D2 - t5 * 
     #t62 / 0.128D3 - t32 * t53 / 0.11520D5 - t54 * t47 / 0.128D3
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t69)
      t72 = t2 * x1
      t74 = t2 * (-0.1D1 + x1)
      t75 = gbgbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t79 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t72, -t74, 0.0D0, t5 * t75 *
     # t8 / 0.64D2)
      t84 = gbgbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t74, t72, 0.0D0, t5 * t84 *
     # t8 / 0.64D2)
      gbgbH4n2em2 = t51 * t50 + t70 * t69 + t79 * t5 * t75 * t8 / 0.64D2
     # + t88 * t5 * t84 * t8 / 0.64D2

      end function



      doubleprecision function gbgbH4n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = gbgbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t5 * t6 / 
     #0.128D3)
      t12 = gbgbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t15 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t5 * t12 
     #/ 0.128D3)
      gbgbH4n2em3 = -t9 * t5 * t6 / 0.128D3 - t15 * t5 * t12 / 0.128D3

      end function



      doubleprecision function gbgbH4n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH41J1
      doubleprecision gbgbH41J2
      doubleprecision gbgbH42J1
      doubleprecision gbgbH42J2
      gbgbH4n2em4 = 0.0D0

      end function
  
 

      doubleprecision function gbgbH41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = s ** 2
      t4 = t1 ** 2
      t6 = 0.1D1 - x1
      t7 = t6 ** 2
      t8 = 0.1D1 - x3
      t9 = t8 ** 2
      t12 = t3 * z
      t17 = z ** 2
      gbgbH41J1 = 0.48D2 * s * t1 * x1 * (-t3 * t4 * t7 * t9 - 0.2D1 * t
     #12 * t1 * t6 * t8 - 0.2D1 * t3 * t17 - t3 + 0.2D1 * t12 + 0.2D1 * 
     #t3 * t1 * t6 * t8) * wd

      end function
  
   
 

      doubleprecision function gbgbH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = s ** 2
      t4 = t1 ** 2
      t6 = 0.1D1 - x1
      t7 = t6 ** 2
      t8 = 0.1D1 - x3
      t9 = t8 ** 2
      gbgbH41J2 = 0.48D2 * s * t1 * x1 * (t3 * t4 * t7 * t9 + t3 - 0.3D1
     # * t3 * t1 * t6 * t8) * wd

      end function
  
   
 

      doubleprecision function gbgbH42J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t5 = 0.1D1 - x2
      t10 = z + x1 * t1
      t11 = 0.1D1 / t10
      t13 = s ** 2
      t14 = t1 ** 2
      t16 = x1 ** 2
      t17 = t10 ** 2
      t20 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(x3 * t5 * t10 * x2 * t20)
      t33 = t20 * t5 * t10 + x2 * x3 + 0.2D1 * t25 * t30
      t34 = t33 ** 2
      t37 = t13 * z
      t40 = x1 * t11 * t33
      t43 = z ** 2
      gbgbH42J1 = 0.48D2 * s * t1 * (0.1D1 - x1) * (z + x1 * t5 * t1) * 
     #t11 * (-t13 * t14 * t16 / t17 * t34 - 0.2D1 * t37 * t1 * t40 - 0.2
     #D1 * t13 * t43 - t13 + 0.2D1 * t37 + 0.2D1 * t13 * t1 * t40) * wd

      end function
  
   
 

      doubleprecision function gbgbH42J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t5 = 0.1D1 - x2
      t10 = z + x1 * t1
      t11 = 0.1D1 / t10
      t13 = s ** 2
      t14 = t1 ** 2
      t16 = x1 ** 2
      t17 = t10 ** 2
      t20 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(x3 * t5 * t10 * x2 * t20)
      t33 = t20 * t5 * t10 + x2 * x3 + 0.2D1 * t25 * t30
      t34 = t33 ** 2
      gbgbH42J2 = 0.48D2 * s * t1 * (0.1D1 - x1) * (z + x1 * t5 * t1) * 
     #t11 * (t13 * t14 * t16 / t17 * t34 + t13 - 0.3D1 * t13 * t1 * x1 *
     # t11 * t33) * wd

      end function
  
 