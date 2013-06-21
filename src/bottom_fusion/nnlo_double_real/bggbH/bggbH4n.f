  
      subroutine bggbH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH41J1  
      doubleprecision bggbH41J2  
      doubleprecision bggbH41J3  
      doubleprecision bggbH42J1  
      doubleprecision bggbH42J2  
      doubleprecision bggbH42J3  
      doubleprecision bggbH4n1e1  
      doubleprecision bggbH4n1e0  
      doubleprecision bggbH4n1em1  
      doubleprecision bggbH4n1em2  
      doubleprecision bggbH4n1em3  
      doubleprecision bggbH4n1em4  
      doubleprecision bggbH4n2e1  
      doubleprecision bggbH4n2e0  
      doubleprecision bggbH4n2em1  
      doubleprecision bggbH4n2em2  
      doubleprecision bggbH4n2em3  
      doubleprecision bggbH4n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH4n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH4n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH4n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH4n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH4n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH4n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * lh
      t7 = 0.1D1 / z
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t20 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t21 = t17 ** 2
      t22 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t29 = lh ** 2
      t31 = 0.3141592653589793D1 ** 2
      t33 = 0.180D3 * t29 - 0.30D2 * t31
      t34 = t5 * t33
      t39 = t5 * t7
      t43 = t21 * t17
      t53 = -0.2884936567583026D3 - 0.120D3 * t29 * lh + 0.60D2 * lh * t
     #31
      t54 = t5 * t53
      t55 = t7 * t22
      t56 = t54 * t55
      t58 = 0.1D1 / x1
      t61 = t7 * t18
      t62 = t8 * x3
      t63 = t14 * t11
      t66 = log(0.4D1 * t62 * t63)
      t67 = t66 * t7
      t69 = -0.1D1 + x3
      t70 = 0.1D1 / t69
      t71 = t63 * t70
      t74 = log(-0.4D1 * t62 * t71)
      t77 = cos(t9)
      t79 = Sqrt(-x3 * t69)
      t84 = 0.1D1 / (-z - x3 + 0.2D1 * t77 * t79 * z)
      t90 = t7 * t20
      t92 = t74 ** 2
      t97 = t66 ** 2
      t98 = t97 * t7
      t105 = t55 + t22 * t84
      t108 = 0.1D1 / x3
      t112 = x2 ** 2
      t113 = x3 * t112
      t114 = t113 * t8
      t117 = log(-0.4D1 * t114 * t71)
      t121 = t113 * t15
      t123 = log(0.4D1 * t121)
      t124 = t123 * t7
      t129 = -t105
      t134 = 0.1D1 / x2
      t135 = t134 * t58
      t138 = t112 * t8
      t141 = log(0.4D1 * t138 * t63)
      t142 = t141 * t7
      t148 = t141 ** 2
      t149 = t148 * t7
      t161 = log(0.4D1 * t63)
      t162 = t161 * t5
      t165 = t161 ** 2
      t166 = t165 * t5
      t169 = (0.180D3 * t162 * lh + t34 + 0.45D2 * t166) * t7
      t176 = t165 * t161 * t5
      t179 = (-t162 * t33 - 0.90D2 * t166 * lh + t54 - 0.15D2 * t176) * 
     #t7
      t182 = t165 ** 2
      t187 = t31 ** 2
      t188 = t29 ** 2
      t199 = (0.15D2 / 0.4D1 * t182 * t5 - t162 * t53 + t5 * (0.57698731
     #35166051D3 * lh + t187 + 0.60D2 * t188 - 0.60D2 * t29 * t31) + t16
     #6 * t33 / 0.2D1 + 0.30D2 * t176 * lh) * t7
      t207 = x3 * t11
      t211 = log(-0.4D1 * t207 * t14 * t70)
      t212 = t211 ** 2
      t216 = log(0.4D1 * t207 * t14)
      t217 = t216 ** 2
      t220 = t212 * t84 / 0.2D1 + t217 * t7 / 0.2D1
      t230 = -t216 * t7 - t211 * t84
      t238 = -t212 * t211 * t84 / 0.6D1 - t217 * t216 * t7 / 0.6D1
      t246 = t7 + t84
      t251 = t112 * t11
      t254 = log(0.4D1 * t251 * t14)
      t255 = t254 * t7
      t257 = t254 ** 2
      t258 = t257 * t7
      t271 = t257 * t254 * t7
      t282 = log(0.4D1 * t113 * t63)
      t283 = t282 * t7
      t287 = log(-0.4D1 * t113 * t71)
      t295 = t282 ** 2
      t296 = t295 * t7
      t300 = t287 ** 2
      t313 = -(-0.180D3 * t6 * t7 * (-t17 * t18 + t20 + t21 * t22 / 0.2D
     #1) + t34 * t7 * (-t17 * t22 + t18) + 0.90D2 * t39 * (-t17 * t20 + 
     #t21 * t18 / 0.2D1 - t43 * t22 / 0.6D1) + t56) * t58 / 0.5760D4 - (
     #-0.180D3 * t6 * (t61 - t67 * t22 + (t18 - t74 * t22) * t84) + 0.90
     #D2 * t5 * (-t67 * t18 + t90 + (-t74 * t18 + t20 + t92 * t22 / 0.2D
     #1) * t84 + t98 * t22 / 0.2D1) + t34 * t105) * t108 * t58 / 0.5760D
     #4 + (0.90D2 * t5 * (-(t18 - t117 * t22) * t84 + t124 * t22 - t61) 
     #- 0.180D3 * t6 * t129) * t108 * t135 / 0.2880D4 + (-0.180D3 * t6 *
     # (t142 * t22 - t61) + 0.90D2 * t5 * (-t90 + t142 * t18 - t149 * t2
     #2 / 0.2D1) - t34 * t55) * t134 * t58 / 0.2880D4 - t169 * t20 / 0.1
     #1520D5 - t179 * t18 / 0.11520D5 - t199 * t22 / 0.11520D5 - ((0.90D
     #2 * t5 * t18 - 0.180D3 * t6 * t22) * t220 + (0.90D2 * t5 * t20 - 0
     #.180D3 * t6 * t18 + t34 * t22) * t230 + 0.90D2 * t5 * t22 * t238 +
     # (-0.180D3 * t6 * t20 + t34 * t18 + t54 * t22) * t246) * t108 / 0.
     #11520D5 + (-0.180D3 * t6 * (-t90 + t255 * t18 - t258 * t22 / 0.2D1
     #) + t34 * (-t61 + t255 * t22) + 0.90D2 * t5 * (t255 * t20 - t258 *
     # t18 / 0.2D1 + t271 * t22 / 0.6D1) - t56) * t134 / 0.5760D4 + (-0.
     #180D3 * t6 * (t283 * t22 - t61 - (t18 - t287 * t22) * t84) + 0.90D
     #2 * t5 * (-t90 + t283 * t18 - t296 * t22 / 0.2D1 - (-t287 * t18 + 
     #t20 + t300 * t22 / 0.2D1) * t84) + t34 * t129) * t108 * t134 / 0.5
     #760D4
      t314 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t313)
      t316 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t319 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t320 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t338 = t7 * t316
      t339 = t54 * t338
      t347 = t7 * t320
      t351 = t7 * t319
      t364 = t338 + t316 * t84
      t377 = -t364
      t473 = -(-0.180D3 * t6 * t7 * (t21 * t316 / 0.2D1 + t319 - t17 * t
     #320) + t34 * t7 * (t320 - t17 * t316) + 0.90D2 * t39 * (-t17 * t31
     #9 + t21 * t320 / 0.2D1 - t43 * t316 / 0.6D1) + t339) * t58 / 0.576
     #0D4 - (-0.180D3 * t6 * (-t67 * t316 + (t320 - t74 * t316) * t84 + 
     #t347) + 0.90D2 * t5 * (t351 + t98 * t316 / 0.2D1 + (t92 * t316 / 0
     #.2D1 + t319 - t74 * t320) * t84 - t67 * t320) + t34 * t364) * t108
     # * t58 / 0.5760D4 + (0.90D2 * t5 * (-(t320 - t117 * t316) * t84 - 
     #t347 + t124 * t316) - 0.180D3 * t6 * t377) * t108 * t135 / 0.2880D
     #4 + (-0.180D3 * t6 * (-t347 + t142 * t316) + 0.90D2 * t5 * (-t351 
     #- t149 * t316 / 0.2D1 + t142 * t320) - t34 * t338) * t134 * t58 / 
     #0.2880D4 - t179 * t320 / 0.11520D5 - t199 * t316 / 0.11520D5 - ((0
     #.90D2 * t5 * t320 - 0.180D3 * t6 * t316) * t220 + (0.90D2 * t5 * t
     #319 - 0.180D3 * t6 * t320 + t34 * t316) * t230 + 0.90D2 * t5 * t31
     #6 * t238 + (-0.180D3 * t6 * t319 + t34 * t320 + t54 * t316) * t246
     #) * t108 / 0.11520D5 - t169 * t319 / 0.11520D5 + (-0.180D3 * t6 * 
     #(-t351 + t255 * t320 - t258 * t316 / 0.2D1) + t34 * (-t347 + t255 
     #* t316) + 0.90D2 * t5 * (t255 * t319 - t258 * t320 / 0.2D1 + t271 
     #* t316 / 0.6D1) - t339) * t134 / 0.5760D4 + (-0.180D3 * t6 * (-t34
     #7 + t283 * t316 - (t320 - t287 * t316) * t84) + 0.90D2 * t5 * (t28
     #3 * t320 - t296 * t316 / 0.2D1 - (t300 * t316 / 0.2D1 + t319 - t28
     #7 * t320) * t84 - t351) + t34 * t377) * t108 * t134 / 0.5760D4
      t474 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t473)
      t476 = t2 * x1
      t477 = -0.1D1 + x1
      t478 = x1 * z
      t479 = 0.1D1 - x1 + t478
      t480 = 0.1D1 / t479
      t482 = t2 * t477 * t480
      t483 = t1 ** 2
      t484 = s * t483
      t486 = x1 * t477 * t480
      t487 = t484 * t486
      t488 = t14 * t480
      t489 = t477 ** 2
      t490 = t488 * t489
      t493 = log(0.4D1 * t12 * t490)
      t494 = -t477
      t495 = bggbH41J2(s, XB1, XB2, z, lh, wd, t494, 0.10D1, 0.10D1, x4)
      t497 = bggbH41J3(s, XB1, XB2, z, lh, wd, t494, 0.10D1, 0.10D1, x4)
      t498 = t493 ** 2
      t499 = bggbH41J1(s, XB1, XB2, z, lh, wd, t494, 0.10D1, 0.10D1, x4)
      t513 = t498 * t493
      t519 = t7 * t499
      t524 = t62 * t11
      t526 = t488 * t489 * t70
      t529 = log(-0.4D1 * t524 * t526)
      t533 = x1 * x3
      t534 = 0.2D1 * t533
      t535 = x3 * t479
      t537 = Sqrt(-t535 * t69)
      t541 = x1 * t13
      t542 = x3 * t13
      t543 = t542 * x1
      t545 = 0.2D1 * t62 * z
      t546 = t62 * t13
      t547 = t533 * z
      t548 = 0.3D1 * t547
      t549 = -t62 - z + t534 + 0.2D1 * t77 * t537 * z - x3 + t478 - t541
     # + t543 + t545 - t546 - t548
      t550 = 0.1D1 / t549
      t552 = t7 * t495
      t555 = log(0.4D1 * t524 * t490)
      t556 = t555 * t7
      t562 = t529 ** 2
      t569 = t555 ** 2
      t570 = t569 * t7
      t573 = t7 * t497
      t579 = -t499 * t479 * t550 - t519
      t585 = t113 * t12
      t588 = log(-0.4D1 * t585 * t526)
      t593 = t480 * t489
      t597 = log(0.4D1 * t114 * t63 * t593)
      t598 = t597 * t7
      t610 = t138 * t11
      t613 = log(0.4D1 * t610 * t490)
      t614 = t613 * t7
      t619 = t613 ** 2
      t620 = t619 * t7
      t632 = -(-0.180D3 * t6 * t7 * (t493 * t495 - t497 - t498 * t499 / 
     #0.2D1) + t34 * t7 * (-t495 + t493 * t499) + 0.90D2 * t39 * (t493 *
     # t497 - t498 * t495 / 0.2D1 + t513 * t499 / 0.6D1) - t54 * t519) *
     # t58 / 0.5760D4 - (-0.180D3 * t6 * (-(t495 - t529 * t499) * t479 *
     # t550 - t552 + t556 * t499) + 0.90D2 * t5 * (-(-t529 * t495 + t497
     # + t562 * t499 / 0.2D1) * t479 * t550 + t556 * t495 - t570 * t499 
     #/ 0.2D1 - t573) + t34 * t579) * t108 * t58 / 0.5760D4 + (0.90D2 * 
     #t5 * ((t495 - t588 * t499) * t479 * t550 + t552 - t598 * t499) + 0
     #.180D3 * t6 * t579) * t108 * t135 / 0.2880D4 + (-0.180D3 * t6 * (-
     #t614 * t499 + t552) + 0.90D2 * t5 * (t620 * t499 / 0.2D1 - t614 * 
     #t495 + t573) + t34 * t519) * t134 * t58 / 0.2880D4
      t633 = FJET(XB1, XB2, s, 0.0D0, t476, -t482, 0.0D0, -t487, t632)
      t635 = x2 * x3
      t636 = -0.1D1 + x2
      t639 = Sqrt(x3 * t636 * t69)
      t640 = t77 * t639
      t642 = 0.2D1 * t640 * x2
      t644 = 0.1D1 - x3 + t635
      t645 = 0.1D1 / t644
      t647 = t2 * (0.1D1 - x2 - x3 + t635 + t113 + t642) * t645
      t652 = t2 * x2 * (t635 - 0.1D1 + 0.2D1 * t640) * t645
      t653 = -t636
      t654 = t69 * t645
      t655 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t653, -t654, x4)
      t656 = t14 * t636
      t657 = t644 ** 2
      t658 = 0.1D1 / t657
      t660 = t656 * t69 * t658
      t663 = log(0.4D1 * t585 * t660)
      t664 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t653, -t654, x4)
      t668 = x2 * z
      t669 = t113 * z
      t670 = t635 * z
      t676 = 0.1D1 / (x2 + z - t668 + t669 - t670 + x3 - t113 - t642 - 0
     #.2D1 * t640 * z + 0.2D1 * t640 * t668)
      t679 = t664 * t676
      t689 = log(0.4D1 * t113 * t11 * t660)
      t695 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, t653, -t654, x4)
      t697 = t689 ** 2
      t709 = (-0.90D2 * t5 * (t655 - t663 * t664) * t676 + 0.180D3 * t6 
     #* t679) * t108 * t135 / 0.2880D4 + (0.180D3 * t6 * (t655 - t689 * 
     #t664) * t676 - 0.90D2 * t5 * (t695 - t689 * t655 + t697 * t664 / 0
     #.2D1) * t676 - t34 * t679) * t108 * t134 / 0.5760D4
      t710 = FJET(XB1, XB2, s, 0.0D0, t647, 0.0D0, -t652, 0.0D0, t709)
      t713 = t1 * t477
      t715 = t636 * s * t713 * t480
      t716 = x2 * s
      t717 = t716 * t713
      t719 = t484 * t636 * t486
      t720 = bggbH42J2(s, XB1, XB2, z, lh, wd, t494, t653, 0.10D1, x4)
      t722 = t488 * t489 * t636
      t725 = log(-0.4D1 * t585 * t722)
      t726 = bggbH42J1(s, XB1, XB2, z, lh, wd, t494, t653, 0.10D1, x4)
      t730 = x2 * x1
      t731 = t730 * z
      t733 = 0.1D1 / (z - t668 - t730 + t731 + x2)
      t736 = t726 * t733
      t744 = log(-0.4D1 * t610 * t722)
      t750 = bggbH42J3(s, XB1, XB2, z, lh, wd, t494, t653, 0.10D1, x4)
      t752 = t744 ** 2
      t764 = (-0.90D2 * t5 * (t720 - t725 * t726) * t733 + 0.180D3 * t6 
     #* t736) * t108 * t135 / 0.2880D4 + (0.180D3 * t6 * (t720 - t744 * 
     #t726) * t733 - 0.90D2 * t5 * (t750 - t744 * t720 + t752 * t726 / 0
     #.2D1) * t733 - t34 * t736) * t134 * t58 / 0.2880D4
      t765 = FJET(XB1, XB2, s, 0.0D0, t715, t476, -t717, t719, t764)
      t767 = t2 * t636
      t768 = t716 * t1
      t769 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t653, 0.10D1, x4)
      t770 = t63 * t636
      t773 = log(-0.4D1 * t114 * t770)
      t774 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t653, 0.10D1, x4)
      t779 = 0.1D1 / (-x2 - z + t668)
      t782 = t774 * t779
      t791 = log(-0.4D1 * t138 * t770)
      t797 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, t653, 0.10D1, x4)
      t799 = t791 ** 2
      t806 = t34 * t782
      t813 = log(-0.4D1 * t251 * t656)
      t815 = t813 ** 2
      t829 = t815 * t813
      t842 = log(-0.4D1 * t113 * t770)
      t849 = t842 ** 2
      t860 = (-0.90D2 * t5 * (t769 - t773 * t774) * t779 + 0.180D3 * t6 
     #* t782) * t108 * t135 / 0.2880D4 + (0.180D3 * t6 * (t769 - t791 * 
     #t774) * t779 - 0.90D2 * t5 * (t797 - t791 * t769 + t799 * t774 / 0
     #.2D1) * t779 - t806) * t134 * t58 / 0.2880D4 + (0.180D3 * t6 * (t7
     #97 - t813 * t769 + t815 * t774 / 0.2D1) * t779 - t34 * (t769 - t81
     #3 * t774) * t779 - 0.90D2 * t5 * (-t813 * t797 + t815 * t769 / 0.2
     #D1 - t829 * t774 / 0.6D1) * t779 - t54 * t782) * t134 / 0.5760D4 +
     # (0.180D3 * t6 * (t769 - t842 * t774) * t779 - 0.90D2 * t5 * (t797
     # - t842 * t769 + t849 * t774 / 0.2D1) * t779 - t806) * t108 * t134
     # / 0.5760D4
      t861 = FJET(XB1, XB2, s, 0.0D0, -t767, 0.0D0, t768, 0.0D0, t860)
      t863 = bggbH42J1(s, XB1, XB2, z, lh, wd, t494, 0.10D1, 0.10D1, x4)
      t866 = bggbH42J3(s, XB1, XB2, z, lh, wd, t494, 0.10D1, 0.10D1, x4)
      t867 = bggbH42J2(s, XB1, XB2, z, lh, wd, t494, 0.10D1, 0.10D1, x4)
      t885 = t7 * t863
      t894 = t7 * t867
      t908 = t7 * t866
      t914 = -t863 * t479 * t550 - t885
      t950 = -(-0.180D3 * t6 * t7 * (-t498 * t863 / 0.2D1 - t866 + t493 
     #* t867) + t34 * t7 * (-t867 + t493 * t863) + 0.90D2 * t39 * (t493 
     #* t866 - t498 * t867 / 0.2D1 + t513 * t863 / 0.6D1) - t54 * t885) 
     #* t58 / 0.5760D4 - (-0.180D3 * t6 * (-(t867 - t529 * t863) * t479 
     #* t550 - t894 + t556 * t863) + 0.90D2 * t5 * (-(t562 * t863 / 0.2D
     #1 + t866 - t529 * t867) * t479 * t550 + t556 * t867 - t570 * t863 
     #/ 0.2D1 - t908) + t34 * t914) * t108 * t58 / 0.5760D4 + (0.90D2 * 
     #t5 * (-t598 * t863 + (t867 - t588 * t863) * t479 * t550 + t894) + 
     #0.180D3 * t6 * t914) * t108 * t135 / 0.2880D4 + (-0.180D3 * t6 * (
     #t894 - t614 * t863) + 0.90D2 * t5 * (t620 * t863 / 0.2D1 - t614 * 
     #t867 + t908) + t34 * t885) * t134 * t58 / 0.2880D4
      t951 = FJET(XB1, XB2, s, 0.0D0, -t482, t476, 0.0D0, -t487, t950)
      t953 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t653, 0.10D1, x4)
      t954 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t653, 0.10D1, x4)
      t960 = t954 * t779
      t972 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t653, 0.10D1, x4)
      t980 = t34 * t960
      t1025 = (-0.90D2 * t5 * (t953 - t773 * t954) * t779 + 0.180D3 * t6
     # * t960) * t108 * t135 / 0.2880D4 + (0.180D3 * t6 * (t953 - t791 *
     # t954) * t779 - 0.90D2 * t5 * (t972 - t791 * t953 + t799 * t954 / 
     #0.2D1) * t779 - t980) * t134 * t58 / 0.2880D4 + (0.180D3 * t6 * (t
     #972 - t813 * t953 + t815 * t954 / 0.2D1) * t779 - t34 * (t953 - t8
     #13 * t954) * t779 - 0.90D2 * t5 * (-t813 * t972 + t815 * t953 / 0.
     #2D1 - t829 * t954 / 0.6D1) * t779 - t54 * t960) * t134 / 0.5760D4 
     #+ (0.180D3 * t6 * (t953 - t842 * t954) * t779 - 0.90D2 * t5 * (t97
     #2 - t842 * t953 + t849 * t954 / 0.2D1) * t779 - t980) * t108 * t13
     #4 / 0.5760D4
      t1026 = FJET(XB1, XB2, s, t768, 0.0D0, -t767, 0.0D0, 0.0D0, t1025)
      t1029 = t476 * t635 * t645
      t1030 = t2 * t477
      t1031 = t113 * x1
      t1032 = t113 * t478
      t1033 = t636 * t69
      t1035 = Sqrt(t535 * t1033)
      t1036 = t77 * t1035
      t1038 = 0.2D1 * t1036 * x2
      t1042 = t1030 * (t113 - x2 + t635 + 0.1D1 - x3 - t1031 + t1032 + t
     #1038) * t480 * t645
      t1046 = t69 * s * t1 * x1 * t645
      t1052 = t1030 * x2 * (-0.1D1 + t635 + x1 - t533 - t478 + t547 + 0.
     #2D1 * t1036) * t480 * t645
      t1053 = bggbH42J2(s, XB1, XB2, z, lh, wd, t494, t653, -t654, x4)
      t1059 = log(0.4D1 * t121 * t593 * t1033 * t658)
      t1060 = t1059 * t479
      t1061 = bggbH42J1(s, XB1, XB2, z, lh, wd, t494, t653, -t654, x4)
      t1069 = x2 * t8
      t1080 = t1032 - 0.2D1 * t1036 * t668 - 0.2D1 * t1036 * t730 - t106
     #9 - x3 - x2 - t635 * x1 + t730 * t13 + 0.2D1 * t1069 * z - t1069 *
     # t13 + 0.2D1 * t1036 * z + t62 * x2 - t1031 + t1038 - 0.3D1 * t731
     # + 0.2D1 * t730 - t669
      t1090 = t670 + t668 + t113 - t62 + t534 - t541 + t478 + t543 + t54
     #5 - t546 - t548 + 0.2D1 * t1036 * t731 - t542 * t730 - 0.2D1 * t62
     # * t668 + t62 * t13 * x2 + 0.2D1 * t635 * t478 - z
      t1092 = 0.1D1 / (t1080 + t1090)
      t1099 = -0.90D2 * t5 * (t479 * t1053 - t1060 * t1061) * t1092 + 0.
     #180D3 * t6 * t479 * t1061 * t1092
      t1103 = FJET(XB1, XB2, s, t1029, -t1042, -t1046, t1052, t719, t109
     #9 * t108 * t135 / 0.2880D4)
      t1106 = t108 * t134 * t58
      t1109 = bggbH41J2(s, XB1, XB2, z, lh, wd, t494, t653, -t654, x4)
      t1111 = bggbH41J1(s, XB1, XB2, z, lh, wd, t494, t653, -t654, x4)
      t1121 = -0.90D2 * t5 * (t479 * t1109 - t1060 * t1111) * t1092 + 0.
     #180D3 * t6 * t479 * t1111 * t1092
      t1125 = FJET(XB1, XB2, s, t1052, -t1046, -t1042, t1029, t719, t112
     #1 * t108 * t135 / 0.2880D4)
      t1129 = bggbH41J2(s, XB1, XB2, z, lh, wd, t494, t653, 0.10D1, x4)
      t1130 = bggbH41J1(s, XB1, XB2, z, lh, wd, t494, t653, 0.10D1, x4)
      t1136 = t1130 * t733
      t1147 = bggbH41J3(s, XB1, XB2, z, lh, wd, t494, t653, 0.10D1, x4)
      t1160 = (-0.90D2 * t5 * (t1129 - t725 * t1130) * t733 + 0.180D3 * 
     #t6 * t1136) * t108 * t135 / 0.2880D4 + (0.180D3 * t6 * (t1129 - t7
     #44 * t1130) * t733 - 0.90D2 * t5 * (t1147 - t744 * t1129 + t752 * 
     #t1130 / 0.2D1) * t733 - t34 * t1136) * t134 * t58 / 0.2880D4
      t1161 = FJET(XB1, XB2, s, -t717, t476, t715, 0.0D0, t719, t1160)
      t1163 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t653, -t654, x4)
      t1164 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t653, -t654, x4)
      t1170 = t1164 * t676
      t1182 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t653, -t654, x4)
      t1195 = (-0.90D2 * t5 * (t1163 - t663 * t1164) * t676 + 0.180D3 * 
     #t6 * t1170) * t108 * t135 / 0.2880D4 + (0.180D3 * t6 * (t1163 - t6
     #89 * t1164) * t676 - 0.90D2 * t5 * (t1182 - t689 * t1163 + t697 * 
     #t1164 / 0.2D1) * t676 - t34 * t1170) * t108 * t134 / 0.5760D4
      t1196 = FJET(XB1, XB2, s, -t652, 0.0D0, t647, 0.0D0, 0.0D0, t1195)
      bggbH4n1e1 = t314 * t313 + t474 * t473 + t633 * t632 + t710 * t709
     # + t765 * t764 + t861 * t860 + t951 * t950 + t1026 * t1025 + t1103
     # * t1099 * t1106 / 0.2880D4 + t1125 * t1121 * t1106 / 0.2880D4 + t
     #1161 * t1160 + t1196 * t1195

      end function



      doubleprecision function bggbH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t8 = t6 * t7
      t9 = x1 ** 2
      t10 = t9 * x3
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = t19 * t6
      t21 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t23 = -0.1D1 + x3
      t24 = 0.1D1 / t23
      t25 = t16 * t24
      t28 = log(-0.4D1 * t10 * t25)
      t31 = cos(t13)
      t33 = Sqrt(-x3 * t23)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t33 * z)
      t43 = t5 * lh
      t44 = t6 * t21
      t46 = t44 + t21 * t38
      t50 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t55 = -t46
      t57 = 0.1D1 / x2
      t59 = t50 * t57 * t52
      t62 = x2 ** 2
      t63 = t62 * t9
      t66 = log(0.4D1 * t63 * t16)
      t67 = t66 * t6
      t78 = t9 * t15
      t81 = log(0.4D1 * t78 * t12)
      t87 = t5 * t6
      t89 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t90 = t81 ** 2
      t96 = lh ** 2
      t98 = 0.3141592653589793D1 ** 2
      t100 = 0.180D3 * t96 - 0.30D2 * t98
      t101 = t5 * t100
      t102 = t101 * t44
      t111 = x3 * t15
      t114 = log(0.4D1 * t111 * t12)
      t119 = log(-0.4D1 * t111 * t12 * t24)
      t121 = -t114 * t6 - t119 * t38
      t124 = t119 ** 2
      t126 = t114 ** 2
      t129 = t124 * t38 / 0.2D1 + t126 * t6 / 0.2D1
      t138 = t6 + t38
      t143 = x3 * t62
      t146 = log(0.4D1 * t143 * t16)
      t147 = t146 * t6
      t151 = log(-0.4D1 * t143 * t25)
      t164 = t62 * t15
      t167 = log(0.4D1 * t164 * t12)
      t168 = t167 * t6
      t175 = t167 ** 2
      t176 = t175 * t6
      t187 = log(0.4D1 * t16)
      t188 = t187 * t5
      t191 = (-0.180D3 * t43 - 0.90D2 * t188) * t6
      t196 = t187 ** 2
      t197 = t196 * t5
      t200 = (0.180D3 * t188 * lh + t101 + 0.45D2 * t197) * t6
      t216 = (-t188 * t100 - 0.90D2 * t197 * lh + t5 * (-0.2884936567583
     #026D3 - 0.120D3 * t96 * lh + 0.60D2 * lh * t98) - 0.15D2 * t196 * 
     #t187 * t5) * t6
      t219 = -(0.90D2 * t5 * (t8 - t20 * t21 + (t7 - t28 * t21) * t38) -
     # 0.180D3 * t43 * t46) * t50 * t52 / 0.5760D4 + t5 * t55 * t59 / 0.
     #32D2 + (0.90D2 * t5 * (t67 * t21 - t8) + 0.180D3 * t43 * t44) * t5
     #7 * t52 / 0.2880D4 - (-0.180D3 * t43 * t6 * (-t81 * t21 + t7) + 0.
     #90D2 * t87 * (-t81 * t7 + t89 + t90 * t21 / 0.2D1) + t102) * t52 /
     # 0.5760D4 - ((0.90D2 * t5 * t7 - 0.180D3 * t43 * t21) * t121 + 0.9
     #0D2 * t5 * t21 * t129 + (0.90D2 * t5 * t89 - 0.180D3 * t43 * t7 + 
     #t101 * t21) * t138) * t50 / 0.11520D5 + (0.90D2 * t5 * (t147 * t21
     # - t8 - (t7 - t151 * t21) * t38) - 0.180D3 * t43 * t55) * t50 * t5
     #7 / 0.5760D4 + (-0.180D3 * t43 * (-t8 + t168 * t21) + 0.90D2 * t5 
     #* (-t6 * t89 + t168 * t7 - t176 * t21 / 0.2D1) - t102) * t57 / 0.5
     #760D4 - t191 * t89 / 0.11520D5 - t200 * t7 / 0.11520D5 - t216 * t2
     #1 / 0.11520D5
      t220 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t219)
      t222 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t224 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t228 = t6 * t224
      t232 = t6 * t222
      t234 = t232 + t222 * t38
      t241 = -t234
      t262 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t267 = t101 * t232
      t323 = -(0.90D2 * t5 * (-t20 * t222 + (t224 - t28 * t222) * t38 + 
     #t228) - 0.180D3 * t43 * t234) * t50 * t52 / 0.5760D4 + t5 * t241 *
     # t59 / 0.32D2 + (0.90D2 * t5 * (-t228 + t67 * t222) + 0.180D3 * t4
     #3 * t232) * t57 * t52 / 0.2880D4 - (-0.180D3 * t43 * t6 * (t224 - 
     #t81 * t222) + 0.90D2 * t87 * (t90 * t222 / 0.2D1 + t262 - t81 * t2
     #24) + t267) * t52 / 0.5760D4 - ((0.90D2 * t5 * t224 - 0.180D3 * t4
     #3 * t222) * t121 + 0.90D2 * t5 * t222 * t129 + (0.90D2 * t5 * t262
     # - 0.180D3 * t43 * t224 + t101 * t222) * t138) * t50 / 0.11520D5 +
     # (0.90D2 * t5 * (-t228 + t147 * t222 - (t224 - t151 * t222) * t38)
     # - 0.180D3 * t43 * t241) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 
     #* (-t228 + t168 * t222) + 0.90D2 * t5 * (-t6 * t262 + t168 * t224 
     #- t176 * t222 / 0.2D1) - t267) * t57 / 0.5760D4 - t191 * t262 / 0.
     #11520D5 - t200 * t224 / 0.11520D5 - t216 * t222 / 0.11520D5
      t324 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t323)
      t326 = t2 * x1
      t327 = -0.1D1 + x1
      t328 = x1 * z
      t329 = 0.1D1 - x1 + t328
      t330 = 0.1D1 / t329
      t332 = t2 * t327 * t330
      t333 = t1 ** 2
      t334 = s * t333
      t336 = x1 * t327 * t330
      t337 = t334 * t336
      t338 = -t327
      t339 = bggbH41J2(s, XB1, XB2, z, lh, wd, t338, 0.10D1, 0.10D1, x4)
      t340 = t10 * t15
      t341 = t12 * t330
      t342 = t327 ** 2
      t347 = log(-0.4D1 * t340 * t341 * t342 * t24)
      t348 = bggbH41J1(s, XB1, XB2, z, lh, wd, t338, 0.10D1, 0.10D1, x4)
      t352 = x1 * x3
      t353 = 0.2D1 * t352
      t354 = x3 * t329
      t356 = Sqrt(-t354 * t23)
      t360 = x1 * t11
      t361 = x3 * t11
      t362 = t361 * x1
      t364 = 0.2D1 * t10 * z
      t365 = t10 * t11
      t366 = t352 * z
      t367 = 0.3D1 * t366
      t368 = -t10 - z + t353 + 0.2D1 * t31 * t356 * z - x3 + t328 - t360
     # + t362 + t364 - t365 - t367
      t369 = 0.1D1 / t368
      t371 = t6 * t339
      t372 = t341 * t342
      t375 = log(0.4D1 * t340 * t372)
      t376 = t375 * t6
      t383 = t6 * t348
      t384 = -t348 * t329 * t369 - t383
      t395 = t63 * t15
      t398 = log(0.4D1 * t395 * t372)
      t399 = t398 * t6
      t412 = log(0.4D1 * t78 * t372)
      t419 = bggbH41J3(s, XB1, XB2, z, lh, wd, t338, 0.10D1, 0.10D1, x4)
      t420 = t412 ** 2
      t430 = -(0.90D2 * t5 * (-(t339 - t347 * t348) * t329 * t369 - t371
     # + t376 * t348) - 0.180D3 * t43 * t384) * t50 * t52 / 0.5760D4 - t
     #5 * t384 * t59 / 0.32D2 + (0.90D2 * t5 * (-t399 * t348 + t371) - 0
     #.180D3 * t43 * t383) * t57 * t52 / 0.2880D4 - (-0.180D3 * t43 * t6
     # * (-t339 + t412 * t348) + 0.90D2 * t87 * (t412 * t339 - t419 - t4
     #20 * t348 / 0.2D1) - t101 * t383) * t52 / 0.5760D4
      t431 = FJET(XB1, XB2, s, 0.0D0, t326, -t332, 0.0D0, -t337, t430)
      t433 = x2 * x3
      t434 = -0.1D1 + x2
      t437 = Sqrt(x3 * t434 * t23)
      t438 = t31 * t437
      t440 = 0.2D1 * t438 * x2
      t442 = 0.1D1 - x3 + t433
      t443 = 0.1D1 / t442
      t445 = t2 * (0.1D1 - x2 - x3 + t433 + t143 + t440) * t443
      t450 = t2 * x2 * (t433 - 0.1D1 + 0.2D1 * t438) * t443
      t451 = -t434
      t452 = t23 * t443
      t453 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t451, -t452, x4)
      t455 = x2 * z
      t456 = t143 * z
      t457 = t433 * z
      t463 = 0.1D1 / (x2 + z - t455 + t456 - t457 + x3 - t143 - t440 - 0
     #.2D1 * t438 * z + 0.2D1 * t438 * t455)
      t467 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t451, -t452, x4)
      t469 = t12 * t434
      t470 = t442 ** 2
      t476 = log(0.4D1 * t143 * t15 * t469 * t23 / t470)
      t489 = -t5 * t453 * t463 * t59 / 0.32D2 + (-0.90D2 * t5 * (t467 - 
     #t476 * t453) * t463 + 0.180D3 * t43 * t453 * t463) * t50 * t57 / 0
     #.5760D4
      t490 = FJET(XB1, XB2, s, 0.0D0, t445, 0.0D0, -t450, 0.0D0, t489)
      t493 = t1 * t327
      t495 = t434 * s * t493 * t330
      t496 = x2 * s
      t497 = t496 * t493
      t499 = t334 * t434 * t336
      t500 = bggbH42J1(s, XB1, XB2, z, lh, wd, t338, t451, 0.10D1, x4)
      t502 = x2 * x1
      t503 = t502 * z
      t505 = 0.1D1 / (z - t455 - t502 + t503 + x2)
      t509 = bggbH42J2(s, XB1, XB2, z, lh, wd, t338, t451, 0.10D1, x4)
      t514 = log(-0.4D1 * t395 * t341 * t342 * t434)
      t527 = -t5 * t500 * t505 * t59 / 0.32D2 + (-0.90D2 * t5 * (t509 - 
     #t514 * t500) * t505 + 0.180D3 * t43 * t500 * t505) * t57 * t52 / 0
     #.2880D4
      t528 = FJET(XB1, XB2, s, 0.0D0, t495, t326, -t497, t499, t527)
      t530 = t2 * t434
      t531 = t496 * t1
      t532 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t451, 0.10D1, x4)
      t535 = 0.1D1 / (-x2 - z + t455)
      t539 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t451, 0.10D1, x4)
      t540 = t16 * t434
      t543 = log(-0.4D1 * t63 * t540)
      t549 = t532 * t535
      t551 = 0.180D3 * t43 * t549
      t558 = log(-0.4D1 * t143 * t540)
      t570 = log(-0.4D1 * t164 * t469)
      t576 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, t451, 0.10D1, x4)
      t578 = t570 ** 2
      t589 = -t5 * t532 * t535 * t59 / 0.32D2 + (-0.90D2 * t5 * (t539 - 
     #t543 * t532) * t535 + t551) * t57 * t52 / 0.2880D4 + (-0.90D2 * t5
     # * (t539 - t558 * t532) * t535 + t551) * t50 * t57 / 0.5760D4 + (0
     #.180D3 * t43 * (t539 - t570 * t532) * t535 - 0.90D2 * t5 * (t576 -
     # t570 * t539 + t578 * t532 / 0.2D1) * t535 - t101 * t549) * t57 / 
     #0.5760D4
      t590 = FJET(XB1, XB2, s, 0.0D0, -t530, 0.0D0, t531, 0.0D0, t589)
      t592 = bggbH42J2(s, XB1, XB2, z, lh, wd, t338, 0.10D1, 0.10D1, x4)
      t593 = bggbH42J1(s, XB1, XB2, z, lh, wd, t338, 0.10D1, 0.10D1, x4)
      t598 = t6 * t592
      t605 = t6 * t593
      t606 = -t593 * t329 * t369 - t605
      t634 = bggbH42J3(s, XB1, XB2, z, lh, wd, t338, 0.10D1, 0.10D1, x4)
      t643 = -(0.90D2 * t5 * (-(t592 - t347 * t593) * t329 * t369 - t598
     # + t376 * t593) - 0.180D3 * t43 * t606) * t50 * t52 / 0.5760D4 - t
     #5 * t606 * t59 / 0.32D2 + (0.90D2 * t5 * (t598 - t399 * t593) - 0.
     #180D3 * t43 * t605) * t57 * t52 / 0.2880D4 - (-0.180D3 * t43 * t6 
     #* (-t592 + t412 * t593) + 0.90D2 * t87 * (-t420 * t593 / 0.2D1 - t
     #634 + t412 * t592) - t101 * t605) * t52 / 0.5760D4
      t644 = FJET(XB1, XB2, s, 0.0D0, -t332, t326, 0.0D0, -t337, t643)
      t646 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t451, 0.10D1, x4)
      t651 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t451, 0.10D1, x4)
      t657 = t646 * t535
      t659 = 0.180D3 * t43 * t657
      t678 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t451, 0.10D1, x4)
      t690 = -t5 * t646 * t535 * t59 / 0.32D2 + (-0.90D2 * t5 * (t651 - 
     #t543 * t646) * t535 + t659) * t57 * t52 / 0.2880D4 + (-0.90D2 * t5
     # * (t651 - t558 * t646) * t535 + t659) * t50 * t57 / 0.5760D4 + (0
     #.180D3 * t43 * (t651 - t570 * t646) * t535 - 0.90D2 * t5 * (t678 -
     # t570 * t651 + t578 * t646 / 0.2D1) * t535 - t101 * t657) * t57 / 
     #0.5760D4
      t691 = FJET(XB1, XB2, s, t531, 0.0D0, -t530, 0.0D0, 0.0D0, t690)
      t694 = t326 * t433 * t443
      t695 = t2 * t327
      t696 = t143 * x1
      t697 = t143 * t328
      t700 = Sqrt(t354 * t434 * t23)
      t701 = t31 * t700
      t703 = 0.2D1 * t701 * x2
      t707 = t695 * (t143 - x2 + t433 + 0.1D1 - x3 - t696 + t697 + t703)
     # * t330 * t443
      t711 = t23 * s * t1 * x1 * t443
      t717 = t695 * x2 * (-0.1D1 + t433 + x1 - t352 - t328 + t366 + 0.2D
     #1 * t701) * t330 * t443
      t718 = t5 * t329
      t719 = bggbH42J1(s, XB1, XB2, z, lh, wd, t338, t451, -t452, x4)
      t721 = x2 * t9
      t743 = -t721 + 0.2D1 * t701 * t503 + 0.2D1 * t433 * t328 + t697 - 
     #0.2D1 * t701 * t455 - 0.2D1 * t701 * t502 - t361 * t502 - 0.2D1 * 
     #t10 * t455 + t10 * t11 * x2 + t703 + t10 * x2 - t433 * x1 + t502 *
     # t11 + 0.2D1 * t721 * z - t721 * t11 + 0.2D1 * t701 * z + t328
      t746 = -t10 + t353 - t360 - 0.3D1 * t503 - t456 + t457 + t362 + t3
     #64 - t365 - t367 + t143 + t455 + 0.2D1 * t502 - x3 - t696 - x2 - z
      t751 = 0.1D1 / (t743 + t746) * t50 * t57 * t52
      t754 = FJET(XB1, XB2, s, t694, -t707, -t711, t717, t499, -t718 * t
     #719 * t751 / 0.32D2)
      t760 = bggbH41J1(s, XB1, XB2, z, lh, wd, t338, t451, -t452, x4)
      t764 = FJET(XB1, XB2, s, t717, -t711, -t707, t694, t499, -t718 * t
     #760 * t751 / 0.32D2)
      t770 = bggbH41J1(s, XB1, XB2, z, lh, wd, t338, t451, 0.10D1, x4)
      t775 = bggbH41J2(s, XB1, XB2, z, lh, wd, t338, t451, 0.10D1, x4)
      t788 = -t5 * t770 * t505 * t59 / 0.32D2 + (-0.90D2 * t5 * (t775 - 
     #t514 * t770) * t505 + 0.180D3 * t43 * t770 * t505) * t57 * t52 / 0
     #.2880D4
      t789 = FJET(XB1, XB2, s, -t497, t326, t495, 0.0D0, t499, t788)
      t791 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t451, -t452, x4)
      t796 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t451, -t452, x4)
      t809 = -t5 * t791 * t463 * t59 / 0.32D2 + (-0.90D2 * t5 * (t796 - 
     #t476 * t791) * t463 + 0.180D3 * t43 * t791 * t463) * t50 * t57 / 0
     #.5760D4
      t810 = FJET(XB1, XB2, s, -t450, 0.0D0, t445, 0.0D0, 0.0D0, t809)
      bggbH4n1e0 = t220 * t219 + t324 * t323 + t431 * t430 + t490 * t489
     # + t528 * t527 + t590 * t589 + t644 * t643 + t691 * t690 - t754 * 
     #t5 * t329 * t719 * t751 / 0.32D2 - t764 * t5 * t329 * t760 * t751 
     #/ 0.32D2 + t789 * t788 + t810 * t809

      end function



      doubleprecision function bggbH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t5 * t6
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t17 = log(0.4D1 * t12 * t14)
      t18 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t20 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t24 = t5 * lh
      t25 = t6 * t18
      t27 = 0.180D3 * t24 * t25
      t29 = 0.1D1 / x1
      t32 = cos(t9)
      t33 = -0.1D1 + x3
      t35 = Sqrt(-x3 * t33)
      t40 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t35 * z)
      t42 = t25 + t18 * t40
      t44 = 0.1D1 / x3
      t45 = t44 * t29
      t48 = 0.1D1 / x2
      t55 = t44 * t48
      t59 = x2 ** 2
      t60 = t59 * t11
      t63 = log(0.4D1 * t60 * t14)
      t64 = t63 * t6
      t72 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t78 = log(0.4D1 * t14 * t11)
      t79 = t78 * t5
      t82 = (-0.180D3 * t24 - 0.90D2 * t79) * t6
      t87 = lh ** 2
      t89 = 0.3141592653589793D1 ** 2
      t93 = t78 ** 2
      t97 = (0.180D3 * t79 * lh + t5 * (0.180D3 * t87 - 0.30D2 * t89) + 
     #0.45D2 * t93 * t5) * t6
      t101 = x3 * t11
      t104 = log(0.4D1 * t101 * t14)
      t110 = log(-0.4D1 * t101 * t14 / t33)
      t112 = -t104 * t6 - t110 * t40
      t120 = t6 + t40
      t125 = -(0.90D2 * t7 * (-t17 * t18 + t20) - t27) * t29 / 0.5760D4 
     #- t5 * t42 * t45 / 0.64D2 - t7 * t18 * t48 * t29 / 0.32D2 - t5 * t
     #42 * t55 / 0.64D2 + (0.90D2 * t5 * (-t6 * t20 + t64 * t18) + t27) 
     #* t48 / 0.5760D4 - t7 * t72 / 0.128D3 - t82 * t20 / 0.11520D5 - t9
     #7 * t18 / 0.11520D5 - (0.90D2 * t5 * t18 * t112 + (0.90D2 * t5 * t
     #20 - 0.180D3 * t24 * t18) * t120) * t44 / 0.11520D5
      t126 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t125)
      t128 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t129 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t134 = t6 * t129
      t136 = 0.180D3 * t24 * t134
      t141 = t134 + t129 * t40
      t161 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t180 = -(0.90D2 * t7 * (t128 - t17 * t129) - t136) * t29 / 0.5760D
     #4 - t5 * t141 * t45 / 0.64D2 - t7 * t129 * t48 * t29 / 0.32D2 - t5
     # * t141 * t55 / 0.64D2 + (0.90D2 * t5 * (-t6 * t128 + t64 * t129) 
     #+ t136) * t48 / 0.5760D4 - t7 * t161 / 0.128D3 - t82 * t128 / 0.11
     #520D5 - t97 * t129 / 0.11520D5 - (0.90D2 * t5 * t129 * t112 + (0.9
     #0D2 * t5 * t128 - 0.180D3 * t24 * t129) * t120) * t44 / 0.11520D5
      t181 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t180)
      t183 = t2 * x1
      t184 = -0.1D1 + x1
      t185 = x1 * z
      t186 = 0.1D1 - x1 + t185
      t187 = 0.1D1 / t186
      t189 = t2 * t184 * t187
      t190 = t1 ** 2
      t191 = s * t190
      t193 = x1 * t184 * t187
      t194 = t191 * t193
      t195 = -t184
      t196 = bggbH41J2(s, XB1, XB2, z, lh, wd, t195, 0.10D1, 0.10D1, x4)
      t198 = t184 ** 2
      t202 = log(0.4D1 * t12 * t14 * t187 * t198)
      t203 = bggbH41J1(s, XB1, XB2, z, lh, wd, t195, 0.10D1, 0.10D1, x4)
      t208 = t6 * t203
      t215 = t8 * x3
      t216 = x1 * x3
      t220 = Sqrt(-x3 * t186 * t33)
      t232 = -t215 - z + 0.2D1 * t216 + 0.2D1 * t32 * t220 * z - x3 + t1
     #85 - x1 * t13 + x3 * t13 * x1 + 0.2D1 * t215 * z - t215 * t13 - 0.
     #3D1 * t216 * z
      t233 = 0.1D1 / t232
      t243 = -(0.90D2 * t7 * (-t196 + t202 * t203) + 0.180D3 * t24 * t20
     #8) * t29 / 0.5760D4 - t5 * (-t203 * t186 * t233 - t208) * t45 / 0.
     #64D2 + t7 * t203 * t48 * t29 / 0.32D2
      t244 = FJET(XB1, XB2, s, 0.0D0, t183, -t189, 0.0D0, -t194, t243)
      t246 = x2 * x3
      t247 = x3 * t59
      t248 = -0.1D1 + x2
      t251 = Sqrt(x3 * t248 * t33)
      t252 = t32 * t251
      t254 = 0.2D1 * t252 * x2
      t257 = 0.1D1 / (0.1D1 - x3 + t246)
      t259 = t2 * (0.1D1 - x2 - x3 + t246 + t247 + t254) * t257
      t264 = t2 * x2 * (t246 - 0.1D1 + 0.2D1 * t252) * t257
      t265 = -t248
      t266 = t33 * t257
      t267 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t265, -t266, x4)
      t269 = x2 * z
      t279 = 0.1D1 / (x2 + z - t269 + t247 * z - t246 * z + x3 - t247 - 
     #t254 - 0.2D1 * t252 * z + 0.2D1 * t252 * t269) * t44 * t48
      t282 = FJET(XB1, XB2, s, 0.0D0, t259, 0.0D0, -t264, 0.0D0, -t5 * t
     #267 * t279 / 0.64D2)
      t288 = t1 * t184
      t290 = t248 * s * t288 * t187
      t291 = x2 * s
      t292 = t291 * t288
      t294 = t191 * t248 * t193
      t295 = bggbH42J1(s, XB1, XB2, z, lh, wd, t195, t265, 0.10D1, x4)
      t297 = x2 * x1
      t302 = 0.1D1 / (z - t269 - t297 + t297 * z + x2) * t48 * t29
      t305 = FJET(XB1, XB2, s, 0.0D0, t290, t183, -t292, t294, -t5 * t29
     #5 * t302 / 0.32D2)
      t310 = t2 * t248
      t311 = t291 * t1
      t312 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t265, 0.10D1, x4)
      t313 = t5 * t312
      t315 = 0.1D1 / (-x2 - z + t269)
      t317 = t315 * t48 * t29
      t321 = t315 * t44 * t48
      t324 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t265, 0.10D1, x4)
      t328 = log(-0.4D1 * t60 * t14 * t248)
      t340 = -t313 * t317 / 0.32D2 - t313 * t321 / 0.64D2 + (-0.90D2 * t
     #5 * (t324 - t328 * t312) * t315 + 0.180D3 * t24 * t312 * t315) * t
     #48 / 0.5760D4
      t341 = FJET(XB1, XB2, s, 0.0D0, -t310, 0.0D0, t311, 0.0D0, t340)
      t343 = bggbH42J2(s, XB1, XB2, z, lh, wd, t195, 0.10D1, 0.10D1, x4)
      t344 = bggbH42J1(s, XB1, XB2, z, lh, wd, t195, 0.10D1, 0.10D1, x4)
      t349 = t6 * t344
      t365 = -(0.90D2 * t7 * (-t343 + t202 * t344) + 0.180D3 * t24 * t34
     #9) * t29 / 0.5760D4 - t5 * (-t344 * t186 * t233 - t349) * t45 / 0.
     #64D2 + t7 * t344 * t48 * t29 / 0.32D2
      t366 = FJET(XB1, XB2, s, 0.0D0, -t189, t183, 0.0D0, -t194, t365)
      t368 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t265, 0.10D1, x4)
      t369 = t5 * t368
      t374 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t265, 0.10D1, x4)
      t386 = -t369 * t317 / 0.32D2 - t369 * t321 / 0.64D2 + (-0.90D2 * t
     #5 * (t374 - t328 * t368) * t315 + 0.180D3 * t24 * t368 * t315) * t
     #48 / 0.5760D4
      t387 = FJET(XB1, XB2, s, t311, 0.0D0, -t310, 0.0D0, 0.0D0, t386)
      t389 = bggbH41J1(s, XB1, XB2, z, lh, wd, t195, t265, 0.10D1, x4)
      t393 = FJET(XB1, XB2, s, -t292, t183, t290, 0.0D0, t294, -t5 * t38
     #9 * t302 / 0.32D2)
      t398 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t265, -t266, x4)
      t402 = FJET(XB1, XB2, s, -t264, 0.0D0, t259, 0.0D0, 0.0D0, -t5 * t
     #398 * t279 / 0.64D2)
      bggbH4n1em1 = t126 * t125 + t181 * t180 + t244 * t243 - t282 * t5 
     #* t267 * t279 / 0.64D2 - t305 * t5 * t295 * t302 / 0.32D2 + t341 *
     # t340 + t366 * t365 + t387 * t386 - t393 * t5 * t389 * t302 / 0.32
     #D2 - t402 * t5 * t398 * t279 / 0.64D2

      end function



      doubleprecision function bggbH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t5 * t6
      t8 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x2
      t17 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t22 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t22 * t26)
      t33 = (-0.180D3 * t5 * lh - 0.90D2 * t29 * t5) * t6
      t37 = cos(t24)
      t40 = Sqrt(-x3 * (-0.1D1 + x3))
      t48 = (t6 + 0.1D1 / (-z - x3 + 0.2D1 * t37 * t40 * z)) / x3
      t51 = -t7 * t8 * t9 / 0.64D2 - t7 * t8 * t13 / 0.64D2 - t7 * t17 /
     # 0.128D3 - t33 * t8 / 0.11520D5 - t5 * t8 * t48 / 0.128D3
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t51)
      t54 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t61 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t69 = -t7 * t54 * t9 / 0.64D2 - t7 * t54 * t13 / 0.64D2 - t7 * t61
     # / 0.128D3 - t33 * t54 / 0.11520D5 - t5 * t54 * t48 / 0.128D3
      t70 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t69)
      t72 = t2 * x1
      t73 = -0.1D1 + x1
      t76 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t78 = t2 * t73 * t76
      t79 = t1 ** 2
      t83 = s * t79 * x1 * t73 * t76
      t84 = -t73
      t85 = bggbH41J1(s, XB1, XB2, z, lh, wd, t84, 0.10D1, 0.10D1, x4)
      t89 = FJET(XB1, XB2, s, 0.0D0, t72, -t78, 0.0D0, -t83, t7 * t85 * 
     #t9 / 0.64D2)
      t95 = -0.1D1 + x2
      t96 = t2 * t95
      t98 = x2 * s * t1
      t99 = -t95
      t100 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t99, 0.10D1, x4)
      t104 = 0.1D1 / (-x2 - z + x2 * z)
      t105 = t104 * t13
      t108 = FJET(XB1, XB2, s, 0.0D0, -t96, 0.0D0, t98, 0.0D0, -t5 * t10
     #0 * t105 / 0.64D2)
      t114 = bggbH42J1(s, XB1, XB2, z, lh, wd, t84, 0.10D1, 0.10D1, x4)
      t118 = FJET(XB1, XB2, s, 0.0D0, -t78, t72, 0.0D0, -t83, t7 * t114 
     #* t9 / 0.64D2)
      t124 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t99, 0.10D1, x4)
      t128 = FJET(XB1, XB2, s, t98, 0.0D0, -t96, 0.0D0, 0.0D0, -t5 * t12
     #4 * t105 / 0.64D2)
      bggbH4n1em2 = t52 * t51 + t70 * t69 + t89 * t5 * t6 * t85 * t9 / 0
     #.64D2 - t108 * t5 * t100 * t104 * t13 / 0.64D2 + t118 * t5 * t6 * 
     #t114 * t9 / 0.64D2 - t128 * t5 * t124 * t104 * t13 / 0.64D2

      end function



      doubleprecision function bggbH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t5 * t6
      t8 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7 * t8 /
     # 0.128D3)
      t15 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t18 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7 * t15 
     #/ 0.128D3)
      bggbH4n1em3 = -t11 * t5 * t6 * t8 / 0.128D3 - t18 * t5 * t6 * t15 
     #/ 0.128D3

      end function



      doubleprecision function bggbH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      bggbH4n1em4 = 0.0D0

      end function


      doubleprecision function bggbH4n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = 0.1D1 / t3 / z
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = log(0.4D1 * t9)
      t12 = t11 ** 2
      t13 = 0.1D1 / z
      t14 = t12 * t13
      t15 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t19 = t12 * t11 * t13
      t20 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t23 = t12 ** 2
      t24 = t23 * t13
      t25 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t34 = t13 * t20
      t35 = t11 * t13
      t39 = lh ** 2
      t42 = 0.3141592653589793D1 ** 2
      t45 = -0.2884936567583026D3 - 0.120D3 * t39 * lh + 0.60D2 * lh * t
     #42
      t48 = t13 * t25
      t50 = t42 ** 2
      t51 = t39 ** 2
      t56 = t31 * (0.5769873135166051D3 * lh + t50 + 0.60D2 * t51 - 0.60
     #D2 * t39 * t42)
      t59 = t13 * t15
      t67 = 0.180D3 * t39 - 0.30D2 * t42
      t81 = t31 * lh
      t85 = x3 * t5
      t88 = log(0.4D1 * t85 * t8)
      t89 = t88 ** 2
      t91 = -0.1D1 + x3
      t92 = 0.1D1 / t91
      t96 = log(-0.4D1 * t85 * t8 * t92)
      t97 = t96 ** 2
      t98 = cos(t6)
      t99 = x3 * z
      t101 = Sqrt(-t99 * t91)
      t105 = 0.1D1 / (-z - x3 + 0.2D1 * t98 * t101)
      t108 = -t89 * t13 / 0.2D1 - t97 * t105 / 0.2D1
      t114 = t31 * t67
      t119 = t96 * t105 + t88 * t13
      t127 = t89 * t88 * t13 / 0.6D1 + t97 * t96 * t105 / 0.6D1
      t133 = t31 * t45
      t136 = -t105 - t13
      t139 = 0.1D1 / x3
      t142 = x1 ** 2
      t143 = t142 * t8
      t144 = t143 * t5
      t146 = log(0.4D1 * t144)
      t148 = t146 ** 2
      t159 = t31 * t13
      t161 = t148 * t146
      t171 = 0.1D1 / x1
      t174 = t142 * x3
      t177 = log(0.4D1 * t174 * t9)
      t178 = t177 * t13
      t180 = t9 * t92
      t183 = log(-0.4D1 * t174 * t180)
      t191 = t183 ** 2
      t197 = t177 ** 2
      t198 = t197 * t13
      t204 = t25 * t105
      t211 = x2 ** 2
      t212 = x3 * t211
      t213 = t212 * t142
      t214 = -0.1D1 + x2
      t215 = t9 * t214
      t218 = log(-0.4D1 * t213 * t215)
      t219 = t218 * t13
      t220 = -t214
      t221 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t220, 0.10D1, x4)
      t223 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t220, 0.10D1, x4)
      t224 = t13 * t223
      t227 = log(-0.4D1 * t213 * t180)
      t233 = log(0.4D1 * t212 * t144)
      t234 = t233 * t13
      t239 = t13 * t221
      t240 = t48 + t204 - t239
      t245 = 0.1D1 / x2
      t246 = t245 * t171
      t249 = t211 * t142
      t252 = log(0.4D1 * t249 * t9)
      t253 = t252 * t13
      t257 = log(-0.4D1 * t249 * t215)
      t258 = t257 * t13
      t264 = t257 ** 2
      t265 = t264 * t13
      t269 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, t220, 0.10D1, x4)
      t270 = t13 * t269
      t271 = t252 ** 2
      t272 = t271 * t13
      t284 = t5 * t211
      t285 = t8 * t214
      t288 = log(-0.4D1 * t284 * t285)
      t290 = t288 ** 2
      t295 = log(0.4D1 * t284 * t8)
      t297 = t295 ** 2
      t312 = t290 * t288
      t318 = t297 * t295
      t332 = log(-0.4D1 * t212 * t215)
      t333 = t332 * t13
      t337 = log(0.4D1 * t212 * t9)
      t338 = t337 * t13
      t342 = log(-0.4D1 * t212 * t180)
      t350 = t332 ** 2
      t351 = t350 * t13
      t355 = t342 ** 2
      t361 = t337 ** 2
      t362 = t361 * t13
      t374 = -(t14 * t15 / 0.2D1 - t19 * t20 / 0.6D1 + t24 * t25 / 0.24D
     #2) * t31 / 0.128D3 - (t34 - t35 * t25) * t31 * t45 / 0.11520D5 - t
     #48 * t56 / 0.11520D5 - (t59 - t35 * t20 + t14 * t25 / 0.2D1) * t31
     # * t67 / 0.11520D5 + (-t35 * t15 + t14 * t20 / 0.2D1 - t19 * t25 /
     # 0.6D1) * t31 * lh / 0.64D2 + ((0.90D2 * t31 * t20 - 0.180D3 * t81
     # * t25) * t108 + (0.90D2 * t31 * t15 - 0.180D3 * t81 * t20 + t114 
     #* t25) * t119 + 0.90D2 * t31 * t25 * t127 + (-0.180D3 * t81 * t15 
     #+ t114 * t20 + t133 * t25) * t136) * t139 / 0.11520D5 - (-0.180D3 
     #* t81 * t13 * (t15 - t146 * t20 + t148 * t25 / 0.2D1) + t114 * t13
     # * (t20 - t146 * t25) + 0.90D2 * t159 * (-t146 * t15 - t161 * t25 
     #/ 0.6D1 + t148 * t20 / 0.2D1) + t133 * t48) * t171 / 0.5760D4 - (-
     #0.180D3 * t81 * (t34 - t178 * t25 + (t20 - t183 * t25) * t105) + 0
     #.90D2 * t31 * ((t15 - t183 * t20 + t191 * t25 / 0.2D1) * t105 + t5
     #9 - t178 * t20 + t198 * t25 / 0.2D1) + t114 * (t48 + t204)) * t139
     # * t171 / 0.5760D4 - (0.90D2 * t31 * (t219 * t221 - t224 + (t20 - 
     #t227 * t25) * t105 + t34 - t234 * t25) - 0.180D3 * t81 * t240) * t
     #139 * t246 / 0.2880D4 - (-0.180D3 * t81 * (-t253 * t25 + t34 + t25
     #8 * t221 - t224) + 0.90D2 * t31 * (t258 * t223 - t265 * t221 / 0.2
     #D1 + t59 - t253 * t20 - t270 + t272 * t25 / 0.2D1) + t114 * (t48 -
     # t239)) * t245 * t171 / 0.2880D4 - (-0.180D3 * t81 * t13 * (-t269 
     #+ t288 * t223 - t290 * t221 / 0.2D1 + t15 - t295 * t20 + t297 * t2
     #5 / 0.2D1) + t114 * t13 * (t20 - t295 * t25 - t223 + t288 * t221) 
     #+ 0.90D2 * t159 * (t288 * t269 - t290 * t223 / 0.2D1 + t312 * t221
     # / 0.6D1 - t295 * t15 + t297 * t20 / 0.2D1 - t318 * t25 / 0.6D1) +
     # t133 * t13 * (t25 - t221)) * t245 / 0.5760D4 + (-0.180D3 * t81 * 
     #(-t34 - t333 * t221 + t338 * t25 + t224 - (t20 - t342 * t25) * t10
     #5) + 0.90D2 * t31 * (t270 - t333 * t223 + t351 * t221 / 0.2D1 - (t
     #15 - t342 * t20 + t355 * t25 / 0.2D1) * t105 - t59 + t338 * t20 - 
     #t362 * t25 / 0.2D1) - t114 * t240) * t139 * t245 / 0.5760D4
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t374)
      t377 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t378 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t381 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t399 = t13 * t378
      t404 = t13 * t381
      t417 = t13 * t377
      t424 = t378 * t105
      t431 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t220, 0.10D1, x4)
      t432 = t13 * t431
      t433 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t220, 0.10D1, x4)
      t442 = t13 * t433
      t443 = t424 + t399 - t442
      t455 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t220, 0.10D1, x4)
      t456 = t13 * t455
      t591 = -(-0.180D3 * t81 * t13 * (t377 + t148 * t378 / 0.2D1 - t146
     # * t381) + t114 * t13 * (t381 - t146 * t378) + 0.90D2 * t159 * (-t
     #161 * t378 / 0.6D1 - t146 * t377 + t148 * t381 / 0.2D1) + t133 * t
     #399) * t171 / 0.5760D4 - (-0.180D3 * t81 * (t404 - t178 * t378 + (
     #t381 - t183 * t378) * t105) + 0.90D2 * t31 * ((t377 - t183 * t381 
     #+ t191 * t378 / 0.2D1) * t105 + t417 - t178 * t381 + t198 * t378 /
     # 0.2D1) + t114 * (t399 + t424)) * t139 * t171 / 0.5760D4 - (0.90D2
     # * t31 * (-t432 + t219 * t433 + (t381 - t227 * t378) * t105 + t404
     # - t234 * t378) - 0.180D3 * t81 * t443) * t139 * t246 / 0.2880D4 -
     # (-0.180D3 * t81 * (t404 - t432 + t258 * t433 - t253 * t378) + 0.9
     #0D2 * t31 * (-t456 + t258 * t431 - t265 * t433 / 0.2D1 + t417 - t2
     #53 * t381 + t272 * t378 / 0.2D1) + t114 * (-t442 + t399)) * t245 *
     # t171 / 0.2880D4 - (-0.180D3 * t81 * t13 * (-t455 + t288 * t431 - 
     #t290 * t433 / 0.2D1 + t377 - t295 * t381 + t297 * t378 / 0.2D1) + 
     #t114 * t13 * (t381 - t295 * t378 - t431 + t288 * t433) + 0.90D2 * 
     #t159 * (t288 * t455 - t290 * t431 / 0.2D1 + t312 * t433 / 0.6D1 - 
     #t295 * t377 + t297 * t381 / 0.2D1 - t318 * t378 / 0.6D1) + t133 * 
     #t13 * (t378 - t433)) * t245 / 0.5760D4 + (-0.180D3 * t81 * (-t404 
     #- t333 * t433 + t338 * t378 + t432 - (t381 - t342 * t378) * t105) 
     #+ 0.90D2 * t31 * (t456 - t333 * t431 + t351 * t433 / 0.2D1 - (t377
     # - t342 * t381 + t355 * t378 / 0.2D1) * t105 - t417 + t338 * t381 
     #- t362 * t378 / 0.2D1) - t114 * t443) * t139 * t245 / 0.5760D4 - (
     #t14 * t377 / 0.2D1 - t19 * t381 / 0.6D1 + t24 * t378 / 0.24D2) * t
     #31 / 0.128D3 - (t404 - t35 * t378) * t31 * t45 / 0.11520D5 - t399 
     #* t56 / 0.11520D5 - (t417 - t35 * t381 + t14 * t378 / 0.2D1) * t31
     # * t67 / 0.11520D5 + (-t35 * t377 + t14 * t381 / 0.2D1 - t19 * t37
     #8 / 0.6D1) * t31 * lh / 0.64D2 + ((0.90D2 * t31 * t381 - 0.180D3 *
     # t81 * t378) * t108 + (0.90D2 * t31 * t377 - 0.180D3 * t81 * t381 
     #+ t114 * t378) * t119 + 0.90D2 * t31 * t378 * t127 + (-0.180D3 * t
     #81 * t377 + t114 * t381 + t133 * t378) * t136) * t139 / 0.11520D5
      t592 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t591)
      t595 = t1 * x1
      t596 = x1 * z
      t597 = -z - x1 + t596
      t598 = 0.1D1 / t597
      t600 = t214 * s * t595 * t598
      t601 = -0.1D1 + x1
      t602 = t2 * t601
      t604 = x2 * s * t595
      t605 = t1 ** 2
      t606 = s * t605
      t609 = x1 * t601 * t598
      t610 = t606 * t214 * t609
      t611 = bggbH42J2(s, XB1, XB2, z, lh, wd, x1, t220, 0.10D1, x4)
      t612 = t212 * t143
      t613 = 0.1D1 / t3
      t614 = t613 * t598
      t615 = t601 ** 2
      t617 = t614 * t615 * t214
      t620 = log(0.4D1 * t612 * t617)
      t621 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, t220, 0.10D1, x4)
      t625 = x2 * x1
      t626 = t625 * z
      t628 = 0.1D1 / (-z - t625 + t626)
      t631 = t621 * t628
      t637 = t249 * t8
      t640 = log(0.4D1 * t637 * t617)
      t646 = bggbH42J3(s, XB1, XB2, z, lh, wd, x1, t220, 0.10D1, x4)
      t648 = t640 ** 2
      t660 = -(-0.90D2 * t31 * (t611 - t620 * t621) * t628 + 0.180D3 * t
     #81 * t631) * t139 * t246 / 0.2880D4 - (0.180D3 * t81 * (t611 - t64
     #0 * t621) * t628 - 0.90D2 * t31 * (t646 - t640 * t611 + t648 * t62
     #1 / 0.2D1) * t628 - t114 * t631) * t245 * t171 / 0.2880D4
      t661 = FJET(XB1, XB2, s, 0.0D0, t600, -t602, t604, -t610, t660)
      t664 = t2 * x1 * t598
      t665 = t606 * t609
      t666 = t614 * t615
      t669 = log(-0.4D1 * t143 * t666)
      t670 = t669 ** 2
      t671 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t674 = bggbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t676 = bggbH41J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t686 = t670 * t669
      t694 = t13 * t671
      t699 = t13 * t674
      t700 = t174 * t8
      t703 = log(-0.4D1 * t700 * t666)
      t704 = t703 * t13
      t706 = t597 * t674
      t708 = t614 * t615 * t92
      t711 = log(0.4D1 * t700 * t708)
      t712 = t711 * t597
      t715 = x1 * x3
      t716 = t715 * z
      t718 = 0.2D1 * t174 * z
      t719 = x1 * t3
      t720 = x3 * t3
      t721 = t720 * x1
      t722 = t174 * t3
      t723 = x3 * t597
      t725 = Sqrt(t723 * t91)
      t730 = 0.1D1 / (-t596 - t716 - t99 - t174 + t718 + t719 + t721 - t
     #722 + 0.2D1 * t98 * t725 * z - t3)
      t737 = t711 ** 2
      t738 = t737 * t597
      t743 = t13 * t676
      t745 = t703 ** 2
      t746 = t745 * t13
      t754 = t597 * t671 * t730 - t694
      t762 = log(0.4D1 * t612 * t708)
      t763 = t762 * t597
      t768 = t598 * t615
      t772 = log(-0.4D1 * t213 * t8 * t613 * t768)
      t773 = t772 * t13
      t786 = log(-0.4D1 * t637 * t666)
      t787 = t786 * t13
      t793 = t786 ** 2
      t794 = t793 * t13
      t805 = -(-0.180D3 * t81 * t13 * (-t670 * t671 / 0.2D1 + t669 * t67
     #4 - t676) + t114 * t13 * (-t674 + t669 * t671) + 0.90D2 * t159 * (
     #t669 * t676 + t686 * t671 / 0.6D1 - t670 * t674 / 0.2D1) - t133 * 
     #t694) * t171 / 0.5760D4 - (-0.180D3 * t81 * (-t699 + t704 * t671 -
     # (-t706 + t712 * t671) * t730) + 0.90D2 * t31 * (-(-t597 * t676 + 
     #t712 * t674 - t738 * t671 / 0.2D1) * t730 - t743 + t704 * t674 - t
     #746 * t671 / 0.2D1) + t114 * t754) * t139 * t171 / 0.5760D4 - (0.9
     #0D2 * t31 * (-(-t706 + t763 * t671) * t730 - t699 + t773 * t671) -
     # 0.180D3 * t81 * t754) * t139 * t246 / 0.2880D4 - (-0.180D3 * t81 
     #* (t787 * t671 - t699) + 0.90D2 * t31 * (t787 * t674 - t794 * t671
     # / 0.2D1 - t743) - t114 * t694) * t245 * t171 / 0.2880D4
      t806 = FJET(XB1, XB2, s, 0.0D0, -t602, -t664, 0.0D0, t665, t805)
      t808 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t811 = bggbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t813 = bggbH42J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t830 = t13 * t808
      t835 = t13 * t811
      t837 = t597 * t811
      t850 = t13 * t813
      t859 = t597 * t808 * t730 - t830
      t893 = -(-0.180D3 * t81 * t13 * (-t670 * t808 / 0.2D1 + t669 * t81
     #1 - t813) + t114 * t13 * (-t811 + t669 * t808) + 0.90D2 * t159 * (
     #t669 * t813 + t686 * t808 / 0.6D1 - t670 * t811 / 0.2D1) - t133 * 
     #t830) * t171 / 0.5760D4 - (-0.180D3 * t81 * (-t835 + t704 * t808 -
     # (-t837 + t712 * t808) * t730) + 0.90D2 * t31 * (-(-t597 * t813 + 
     #t712 * t811 - t738 * t808 / 0.2D1) * t730 - t850 + t704 * t811 - t
     #746 * t808 / 0.2D1) + t114 * t859) * t139 * t171 / 0.5760D4 - (0.9
     #0D2 * t31 * (-t835 + t773 * t808 - (-t837 + t763 * t808) * t730) -
     # 0.180D3 * t81 * t859) * t139 * t246 / 0.2880D4 - (-0.180D3 * t81 
     #* (t787 * t808 - t835) + 0.90D2 * t31 * (t787 * t811 - t794 * t808
     # / 0.2D1 - t850) - t114 * t830) * t245 * t171 / 0.2880D4
      t894 = FJET(XB1, XB2, s, 0.0D0, -t664, -t602, 0.0D0, t665, t893)
      t896 = x2 * x3
      t897 = 0.1D1 - x3 + t896
      t898 = 0.1D1 / t897
      t899 = t91 * t898
      t900 = t2 * t899
      t901 = t896 * t898
      t902 = t2 * t901
      t903 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t220, -t899, x4)
      t905 = t897 ** 2
      t906 = 0.1D1 / t905
      t907 = t91 * t906
      t911 = log(0.4D1 * t612 * t5 * t214 * t907)
      t912 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t220, -t899, x4)
      t916 = t214 * t91
      t918 = Sqrt(t99 * t916)
      t922 = 0.1D1 / (-z - x3 + t896 + 0.2D1 * t98 * t918)
      t925 = t912 * t922
      t936 = log(0.4D1 * t212 * t5 * t285 * t907)
      t943 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t220, -t899, x4)
      t944 = t936 ** 2
      t956 = -(-0.90D2 * t31 * (t903 - t911 * t912) * t922 + 0.180D3 * t
     #81 * t925) * t139 * t246 / 0.2880D4 + (-0.180D3 * t81 * (t903 - t9
     #36 * t912) * t922 + 0.90D2 * t31 * (-t936 * t903 + t943 + t944 * t
     #912 / 0.2D1) * t922 + t114 * t925) * t139 * t245 / 0.5760D4
      t957 = FJET(XB1, XB2, s, 0.0D0, -t900, 0.0D0, t902, 0.0D0, t956)
      t959 = bggbH41J2(s, XB1, XB2, z, lh, wd, x1, t220, 0.10D1, x4)
      t960 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, t220, 0.10D1, x4)
      t966 = t960 * t628
      t977 = bggbH41J3(s, XB1, XB2, z, lh, wd, x1, t220, 0.10D1, x4)
      t990 = -(-0.90D2 * t31 * (t959 - t620 * t960) * t628 + 0.180D3 * t
     #81 * t966) * t139 * t246 / 0.2880D4 - (0.180D3 * t81 * (t959 - t64
     #0 * t960) * t628 - 0.90D2 * t31 * (t977 - t640 * t959 + t648 * t96
     #0 / 0.2D1) * t628 - t114 * t966) * t245 * t171 / 0.2880D4
      t991 = FJET(XB1, XB2, s, t604, -t602, t600, 0.0D0, -t610, t990)
      t993 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t220, -t899, x4)
      t994 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t220, -t899, x4)
      t1000 = t994 * t922
      t1014 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, t220, -t899, x4)
      t1025 = -(-0.90D2 * t31 * (t993 - t911 * t994) * t922 + 0.180D3 * 
     #t81 * t1000) * t139 * t246 / 0.2880D4 + (-0.180D3 * t81 * (t993 - 
     #t936 * t994) * t922 + 0.90D2 * t31 * (t944 * t994 / 0.2D1 + t1014 
     #- t936 * t993) * t922 + t114 * t1000) * t139 * t245 / 0.5760D4
      t1026 = FJET(XB1, XB2, s, t902, 0.0D0, -t900, 0.0D0, 0.0D0, t1025)
      t1028 = t2 * x1
      t1030 = Sqrt(-t723 * t916)
      t1031 = t98 * t1030
      t1037 = t1028 * x2 * (-x3 + t896 - z + t99 - x1 + t715 + t596 - t7
     #16 + 0.2D1 * t1031) * t598 * t898
      t1041 = t91 * s * t1 * t601 * t898
      t1044 = t212 * x1
      t1046 = t212 * t596
      t1050 = t1028 * (-x2 + t896 + 0.2D1 * t1031 * x2 + 0.1D1 - x3 + t1
     #044 + t212 * z - t1046) * t598 * t898
      t1051 = t602 * t901
      t1052 = bggbH41J2(s, XB1, XB2, z, lh, wd, x1, t220, -t899, x4)
      t1060 = log(-0.4D1 * t212 * t143 * t613 * t768 * t916 * t906)
      t1061 = t1060 * t597
      t1062 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, t220, -t899, x4)
      t1066 = x2 * t142
      t1076 = t1066 + t99 + t174 - t719 + t596 - 0.2D1 * t1031 * z - t17
     #4 * x2 - t896 * z + t896 * x1 - t625 * t3 - 0.2D1 * t1066 * z + t1
     #066 * t3 + t3
      t1089 = -t1044 + 0.2D1 * t1031 * t626 + t1046 - 0.2D1 * t1031 * t6
     #25 + t720 * t625 + 0.2D1 * t174 * x2 * z - t174 * t3 * x2 - 0.2D1 
     #* t896 * t596 - t718 - t721 + t722 + t716 + t626
      t1091 = 0.1D1 / (t1076 + t1089)
      t1098 = 0.90D2 * t31 * (t597 * t1052 - t1061 * t1062) * t1091 - 0.
     #180D3 * t81 * t597 * t1062 * t1091
      t1102 = FJET(XB1, XB2, s, t1037, t1041, -t1050, -t1051, -t610, -t1
     #098 * t139 * t246 / 0.2880D4)
      t1105 = t139 * t245 * t171
      t1108 = bggbH42J2(s, XB1, XB2, z, lh, wd, x1, t220, -t899, x4)
      t1110 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, t220, -t899, x4)
      t1120 = 0.90D2 * t31 * (t597 * t1108 - t1061 * t1110) * t1091 - 0.
     #180D3 * t81 * t597 * t1110 * t1091
      t1124 = FJET(XB1, XB2, s, -t1051, -t1050, t1041, t1037, -t610, -t1
     #120 * t139 * t246 / 0.2880D4)
      bggbH4n2e1 = t375 * t374 + t592 * t591 + t661 * t660 + t806 * t805
     # + t894 * t893 + t957 * t956 + t991 * t990 + t1026 * t1025 - t1102
     # * t1098 * t1105 / 0.2880D4 - t1124 * t1120 * t1105 / 0.2880D4

      end function



      doubleprecision function bggbH4n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t8 = t6 * t7
      t9 = x1 ** 2
      t10 = t9 * x3
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t20 = log(0.4D1 * t10 * t17)
      t21 = t6 * t20
      t22 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t24 = -0.1D1 + x3
      t25 = 0.1D1 / t24
      t26 = t17 * t25
      t29 = log(-0.4D1 * t10 * t26)
      t32 = cos(t14)
      t33 = x3 * z
      t35 = Sqrt(-t33 * t24)
      t39 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t35)
      t44 = t5 * lh
      t45 = t6 * t22
      t46 = t22 * t39
      t51 = 0.1D1 / x3
      t53 = 0.1D1 / x1
      t56 = 0.1D1 - x2
      t57 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t58 = t6 * t57
      t59 = t45 + t46 - t58
      t61 = 0.1D1 / x2
      t63 = t51 * t61 * t53
      t66 = x2 ** 2
      t67 = t66 * t9
      t70 = log(0.4D1 * t67 * t17)
      t71 = t70 * t6
      t73 = -t56
      t74 = t17 * t73
      t77 = log(-0.4D1 * t67 * t74)
      t78 = t77 * t6
      t80 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t81 = t6 * t80
      t92 = t9 * t16
      t95 = log(0.4D1 * t92 * t13)
      t101 = t5 * t6
      t102 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t104 = t95 ** 2
      t110 = lh ** 2
      t112 = 0.3141592653589793D1 ** 2
      t114 = 0.180D3 * t110 - 0.30D2 * t112
      t115 = t5 * t114
      t125 = x3 * t13
      t129 = log(-0.4D1 * t125 * t16 * t25)
      t133 = log(0.4D1 * t125 * t16)
      t135 = t129 * t39 + t133 * t6
      t138 = t133 ** 2
      t140 = t129 ** 2
      t143 = -t138 * t6 / 0.2D1 - t140 * t39 / 0.2D1
      t152 = -t39 - t6
      t157 = x3 * t66
      t160 = log(-0.4D1 * t157 * t74)
      t161 = t160 * t6
      t165 = log(0.4D1 * t157 * t17)
      t166 = t165 * t6
      t170 = log(-0.4D1 * t157 * t26)
      t184 = t13 * t66
      t187 = log(0.4D1 * t184 * t16)
      t189 = t16 * t73
      t192 = log(-0.4D1 * t184 * t189)
      t198 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t200 = t192 ** 2
      t204 = t187 ** 2
      t217 = log(0.4D1 * t17)
      t218 = t217 * t6
      t226 = t217 ** 2
      t227 = t226 * t6
      t239 = t5 * (-0.2884936567583026D3 - 0.120D3 * t110 * lh + 0.60D2 
     #* lh * t112)
      t246 = t226 * t217 * t6
      t252 = -(0.90D2 * t5 * (t8 - t21 * t22 + (t7 - t29 * t22) * t39) -
     # 0.180D3 * t44 * (t45 + t46)) * t51 * t53 / 0.5760D4 - t5 * t59 * 
     #t63 / 0.32D2 - (0.90D2 * t5 * (-t71 * t22 + t8 + t78 * t57 - t81) 
     #- 0.180D3 * t44 * (t45 - t58)) * t61 * t53 / 0.2880D4 - (-0.180D3 
     #* t44 * t6 * (t7 - t95 * t22) + 0.90D2 * t101 * (t102 - t95 * t7 +
     # t104 * t22 / 0.2D1) + t115 * t45) * t53 / 0.5760D4 + ((0.90D2 * t
     #5 * t7 - 0.180D3 * t44 * t22) * t135 + 0.90D2 * t5 * t22 * t143 + 
     #(0.90D2 * t5 * t102 - 0.180D3 * t44 * t7 + t115 * t22) * t152) * t
     #51 / 0.11520D5 + (0.90D2 * t5 * (-t8 - t161 * t57 + t166 * t22 + t
     #81 - (t7 - t170 * t22) * t39) + 0.180D3 * t44 * t59) * t51 * t61 /
     # 0.5760D4 - (-0.180D3 * t44 * t6 * (t7 - t187 * t22 - t80 + t192 *
     # t57) + 0.90D2 * t101 * (-t198 + t192 * t80 - t200 * t57 / 0.2D1 +
     # t102 - t187 * t7 + t204 * t22 / 0.2D1) + t115 * t6 * (t22 - t57))
     # * t61 / 0.5760D4 - (t8 - t218 * t22) * t5 * t114 / 0.11520D5 + (t
     #6 * t102 - t218 * t7 + t227 * t22 / 0.2D1) * t5 * lh / 0.64D2 - t2
     #39 * t45 / 0.11520D5 - (-t218 * t102 + t227 * t7 / 0.2D1 - t246 * 
     #t22 / 0.6D1) * t5 / 0.128D3
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t252)
      t255 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t256 = t6 * t255
      t257 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t265 = t6 * t257
      t266 = t257 * t39
      t274 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t275 = t6 * t274
      t276 = t266 + t265 - t275
      t280 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t281 = t6 * t280
      t299 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t350 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t389 = -(0.90D2 * t5 * (t256 - t21 * t257 + (t255 - t29 * t257) * 
     #t39) - 0.180D3 * t44 * (t265 + t266)) * t51 * t53 / 0.5760D4 - t5 
     #* t276 * t63 / 0.32D2 - (0.90D2 * t5 * (t256 - t281 + t78 * t274 -
     # t71 * t257) - 0.180D3 * t44 * (-t275 + t265)) * t61 * t53 / 0.288
     #0D4 - (-0.180D3 * t44 * t6 * (t255 - t95 * t257) + 0.90D2 * t101 *
     # (t299 + t104 * t257 / 0.2D1 - t95 * t255) + t115 * t265) * t53 / 
     #0.5760D4 + ((0.90D2 * t5 * t255 - 0.180D3 * t44 * t257) * t135 + 0
     #.90D2 * t5 * t257 * t143 + (0.90D2 * t5 * t299 - 0.180D3 * t44 * t
     #255 + t115 * t257) * t152) * t51 / 0.11520D5 + (0.90D2 * t5 * (-t2
     #56 - t161 * t274 + t166 * t257 + t281 - (t255 - t170 * t257) * t39
     #) + 0.180D3 * t44 * t276) * t51 * t61 / 0.5760D4 - (-0.180D3 * t44
     # * t6 * (t255 - t187 * t257 - t280 + t192 * t274) + 0.90D2 * t101 
     #* (-t350 + t192 * t280 - t200 * t274 / 0.2D1 + t299 - t187 * t255 
     #+ t204 * t257 / 0.2D1) + t115 * t6 * (t257 - t274)) * t61 / 0.5760
     #D4 - (t256 - t218 * t257) * t5 * t114 / 0.11520D5 + (t6 * t299 - t
     #218 * t255 + t227 * t257 / 0.2D1) * t5 * lh / 0.64D2 - t239 * t265
     # / 0.11520D5 - (-t218 * t299 + t227 * t255 / 0.2D1 - t246 * t257 /
     # 0.6D1) * t5 / 0.128D3
      t390 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t389)
      t393 = t1 * x1
      t394 = x1 * z
      t395 = -z - x1 + t394
      t396 = 0.1D1 / t395
      t398 = t73 * s * t393 * t396
      t399 = -0.1D1 + x1
      t400 = t2 * t399
      t402 = x2 * s * t393
      t403 = t1 ** 2
      t404 = s * t403
      t407 = x1 * t399 * t396
      t408 = t404 * t73 * t407
      t409 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t411 = x2 * x1
      t412 = t411 * z
      t414 = 0.1D1 / (-z - t411 + t412)
      t418 = bggbH42J2(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t419 = t67 * t16
      t421 = 0.1D1 / t11 * t396
      t422 = t399 ** 2
      t427 = log(0.4D1 * t419 * t421 * t422 * t73)
      t440 = t5 * t409 * t414 * t63 / 0.32D2 - (-0.90D2 * t5 * (t418 - t
     #427 * t409) * t414 + 0.180D3 * t44 * t409 * t414) * t61 * t53 / 0.
     #2880D4
      t441 = FJET(XB1, XB2, s, 0.0D0, t398, -t400, t402, -t408, t440)
      t444 = t2 * x1 * t396
      t445 = t404 * t407
      t446 = bggbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t447 = t6 * t446
      t448 = t10 * t16
      t449 = t421 * t422
      t452 = log(-0.4D1 * t448 * t449)
      t453 = t452 * t6
      t454 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t461 = log(0.4D1 * t448 * t421 * t422 * t25)
      t462 = t461 * t395
      t465 = x1 * x3
      t466 = t465 * z
      t468 = 0.2D1 * t10 * z
      t469 = x1 * t11
      t470 = x3 * t11
      t471 = t470 * x1
      t472 = t10 * t11
      t473 = x3 * t395
      t475 = Sqrt(t473 * t24)
      t480 = 0.1D1 / (-t394 - t466 - t33 - t10 + t468 + t469 + t471 - t4
     #72 + 0.2D1 * t32 * t475 * z - t11)
      t487 = t6 * t454
      t488 = t395 * t454 * t480 - t487
      t500 = log(-0.4D1 * t419 * t449)
      t501 = t500 * t6
      t514 = log(-0.4D1 * t92 * t449)
      t520 = t514 ** 2
      t524 = bggbH41J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t532 = -(0.90D2 * t5 * (-t447 + t453 * t454 - (-t395 * t446 + t462
     # * t454) * t480) - 0.180D3 * t44 * t488) * t51 * t53 / 0.5760D4 - 
     #t5 * t488 * t63 / 0.32D2 - (0.90D2 * t5 * (t501 * t454 - t447) + 0
     #.180D3 * t44 * t487) * t61 * t53 / 0.2880D4 - (-0.180D3 * t44 * t6
     # * (-t446 + t514 * t454) + 0.90D2 * t101 * (-t520 * t454 / 0.2D1 +
     # t514 * t446 - t524) - t115 * t487) * t53 / 0.5760D4
      t533 = FJET(XB1, XB2, s, 0.0D0, -t400, -t444, 0.0D0, t445, t532)
      t535 = bggbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t536 = t6 * t535
      t537 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t548 = t6 * t537
      t549 = t395 * t537 * t480 - t548
      t577 = bggbH42J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t585 = -(0.90D2 * t5 * (-t536 + t453 * t537 - (-t395 * t535 + t462
     # * t537) * t480) - 0.180D3 * t44 * t549) * t51 * t53 / 0.5760D4 - 
     #t5 * t549 * t63 / 0.32D2 - (0.90D2 * t5 * (t501 * t537 - t536) + 0
     #.180D3 * t44 * t548) * t61 * t53 / 0.2880D4 - (-0.180D3 * t44 * t6
     # * (-t535 + t514 * t537) + 0.90D2 * t101 * (-t520 * t537 / 0.2D1 +
     # t514 * t535 - t577) - t115 * t548) * t53 / 0.5760D4
      t586 = FJET(XB1, XB2, s, 0.0D0, -t444, -t400, 0.0D0, t445, t585)
      t588 = x2 * x3
      t589 = 0.1D1 - x3 + t588
      t590 = 0.1D1 / t589
      t591 = t24 * t590
      t592 = t2 * t591
      t593 = t588 * t590
      t594 = t2 * t593
      t595 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t591, x4)
      t597 = t73 * t24
      t599 = Sqrt(t33 * t597)
      t603 = 0.1D1 / (-z - x3 + t588 + 0.2D1 * t32 * t599)
      t607 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t591, x4)
      t609 = t589 ** 2
      t615 = log(0.4D1 * t157 * t13 * t189 * t24 / t609)
      t628 = t5 * t595 * t603 * t63 / 0.32D2 + (0.90D2 * t5 * (t607 - t6
     #15 * t595) * t603 - 0.180D3 * t44 * t595 * t603) * t51 * t61 / 0.5
     #760D4
      t629 = FJET(XB1, XB2, s, 0.0D0, -t592, 0.0D0, t594, 0.0D0, t628)
      t631 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t636 = bggbH41J2(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t649 = t5 * t631 * t414 * t63 / 0.32D2 - (-0.90D2 * t5 * (t636 - t
     #427 * t631) * t414 + 0.180D3 * t44 * t631 * t414) * t61 * t53 / 0.
     #2880D4
      t650 = FJET(XB1, XB2, s, t402, -t400, t398, 0.0D0, -t408, t649)
      t652 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t591, x4)
      t657 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t591, x4)
      t670 = t5 * t652 * t603 * t63 / 0.32D2 + (0.90D2 * t5 * (t657 - t6
     #15 * t652) * t603 - 0.180D3 * t44 * t652 * t603) * t51 * t61 / 0.5
     #760D4
      t671 = FJET(XB1, XB2, s, t594, 0.0D0, -t592, 0.0D0, 0.0D0, t670)
      t673 = t2 * x1
      t675 = Sqrt(-t473 * t597)
      t676 = t32 * t675
      t682 = t673 * x2 * (-x3 + t588 - z + t33 - x1 + t465 + t394 - t466
     # + 0.2D1 * t676) * t396 * t590
      t686 = t24 * s * t1 * t399 * t590
      t689 = t157 * x1
      t691 = t157 * t394
      t695 = t673 * (-x2 + t588 + 0.2D1 * t676 * x2 + 0.1D1 - x3 + t689 
     #+ t157 * z - t691) * t396 * t590
      t696 = t400 * t593
      t697 = t5 * t395
      t698 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, t56, -t591, x4)
      t718 = t691 - 0.2D1 * t676 * t411 + t470 * t411 + 0.2D1 * t10 * x2
     # * z - t10 * t11 * x2 - 0.2D1 * t588 * t394 + 0.2D1 * t676 * t412 
     #- t689 - 0.2D1 * t676 * z - t10 * x2 - t588 * z + t588 * x1 - t411
     # * t11
      t719 = x2 * t9
      t723 = -0.2D1 * t719 * z + t719 * t11 + t466 - t468 - t471 + t472 
     #+ t719 + t11 + t394 + t33 + t10 - t469 + t412
      t728 = 0.1D1 / (t718 + t723) * t51 * t61 * t53
      t731 = FJET(XB1, XB2, s, t682, t686, -t695, -t696, -t408, -t697 * 
     #t698 * t728 / 0.32D2)
      t737 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, t56, -t591, x4)
      t741 = FJET(XB1, XB2, s, -t696, -t695, t686, t682, -t408, -t697 * 
     #t737 * t728 / 0.32D2)
      bggbH4n2e0 = t253 * t252 + t390 * t389 + t441 * t440 + t533 * t532
     # + t586 * t585 + t629 * t628 + t650 * t649 + t670 * t671 - t731 * 
     #t5 * t395 * t698 * t728 / 0.32D2 - t741 * t5 * t395 * t737 * t728 
     #/ 0.32D2

      end function



      doubleprecision function bggbH4n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t5 * t6
      t8 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t19 = log(0.4D1 * t13 * t16)
      t20 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t25 = t5 * lh
      t26 = t6 * t20
      t30 = 0.1D1 / x1
      t33 = cos(t10)
      t34 = x3 * z
      t35 = -0.1D1 + x3
      t37 = Sqrt(-t34 * t35)
      t41 = 0.1D1 / (-z - x3 + 0.2D1 * t33 * t37)
      t42 = t20 * t41
      t45 = 0.1D1 / x3
      t46 = t45 * t30
      t49 = 0.1D1 - x2
      t50 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t49, 0.10D1, x4)
      t51 = t6 * t50
      t54 = 0.1D1 / x2
      t55 = t54 * t30
      t60 = t45 * t54
      t63 = x2 ** 2
      t64 = t16 * t63
      t67 = log(0.4D1 * t64 * t12)
      t69 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t49, 0.10D1, x4)
      t70 = -t49
      t74 = log(-0.4D1 * t64 * t12 * t70)
      t89 = log(0.4D1 * t16 * t12)
      t90 = t89 * t6
      t96 = lh ** 2
      t98 = 0.3141592653589793D1 ** 2
      t101 = t5 * (0.180D3 * t96 - 0.30D2 * t98)
      t104 = bggbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t107 = t89 ** 2
      t108 = t107 * t6
      t115 = x3 * t16
      t120 = log(-0.4D1 * t115 * t12 / t35)
      t124 = log(0.4D1 * t115 * t12)
      t126 = t120 * t41 + t124 * t6
      t134 = -t41 - t6
      t139 = -(0.90D2 * t7 * (t8 - t19 * t20) - 0.180D3 * t25 * t26) * t
     #30 / 0.5760D4 - t5 * (t26 + t42) * t46 / 0.64D2 - t5 * (t26 - t51)
     # * t55 / 0.32D2 + t5 * (-t26 + t51 - t42) * t60 / 0.64D2 - (0.90D2
     # * t7 * (t8 - t67 * t20 - t69 + t74 * t50) - 0.180D3 * t25 * t6 * 
     #(t20 - t50)) * t54 / 0.5760D4 + (t6 * t8 - t90 * t20) * t5 * lh / 
     #0.64D2 - t101 * t26 / 0.11520D5 - (t6 * t104 - t90 * t8 + t108 * t
     #20 / 0.2D1) * t5 / 0.128D3 + (0.90D2 * t5 * t20 * t126 + (0.90D2 *
     # t5 * t8 - 0.180D3 * t25 * t20) * t134) * t45 / 0.11520D5
      t140 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t139)
      t142 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t143 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t148 = t6 * t143
      t154 = t143 * t41
      t159 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t49, 0.10D1, x4)
      t160 = t6 * t159
      t170 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t49, 0.10D1, x4)
      t190 = bggbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t210 = -(0.90D2 * t7 * (t142 - t19 * t143) - 0.180D3 * t25 * t148)
     # * t30 / 0.5760D4 - t5 * (t148 + t154) * t46 / 0.64D2 - t5 * (-t16
     #0 + t148) * t55 / 0.32D2 + t5 * (-t148 + t160 - t154) * t60 / 0.64
     #D2 - (0.90D2 * t7 * (t142 - t67 * t143 - t170 + t74 * t159) - 0.18
     #0D3 * t25 * t6 * (t143 - t159)) * t54 / 0.5760D4 + (t6 * t142 - t9
     #0 * t143) * t5 * lh / 0.64D2 - t101 * t148 / 0.11520D5 - (t6 * t19
     #0 - t90 * t142 + t108 * t143 / 0.2D1) * t5 / 0.128D3 + (0.90D2 * t
     #5 * t143 * t126 + (0.90D2 * t5 * t142 - 0.180D3 * t25 * t143) * t1
     #34) * t45 / 0.11520D5
      t211 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t210)
      t214 = t1 * x1
      t215 = x1 * z
      t216 = -z - x1 + t215
      t217 = 0.1D1 / t216
      t219 = t70 * s * t214 * t217
      t220 = -0.1D1 + x1
      t221 = t2 * t220
      t223 = x2 * s * t214
      t224 = t1 ** 2
      t225 = s * t224
      t228 = x1 * t220 * t217
      t229 = t225 * t70 * t228
      t230 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, t49, 0.10D1, x4)
      t232 = x2 * x1
      t237 = 0.1D1 / (-z - t232 + t232 * z) * t54 * t30
      t240 = FJET(XB1, XB2, s, 0.0D0, t219, -t221, t223, -t229, t5 * t23
     #0 * t237 / 0.32D2)
      t246 = t2 * x1 * t217
      t247 = t225 * t228
      t248 = bggbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t251 = t220 ** 2
      t255 = log(-0.4D1 * t13 / t14 * t217 * t251)
      t256 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t261 = t6 * t256
      t270 = t9 * x3
      t279 = Sqrt(x3 * t216 * t35)
      t284 = 0.1D1 / (-t215 - x3 * x1 * z - t34 - t270 + 0.2D1 * t270 * 
     #z + x1 * t14 + x3 * t14 * x1 - t270 * t14 + 0.2D1 * t33 * t279 * z
     # - t14)
      t294 = -(0.90D2 * t7 * (-t248 + t255 * t256) + 0.180D3 * t25 * t26
     #1) * t30 / 0.5760D4 - t5 * (t216 * t256 * t284 - t261) * t46 / 0.6
     #4D2 + t7 * t256 * t54 * t30 / 0.32D2
      t295 = FJET(XB1, XB2, s, 0.0D0, -t221, -t246, 0.0D0, t247, t294)
      t297 = bggbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t298 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t303 = t6 * t298
      t319 = -(0.90D2 * t7 * (-t297 + t255 * t298) + 0.180D3 * t25 * t30
     #3) * t30 / 0.5760D4 - t5 * (t216 * t298 * t284 - t303) * t46 / 0.6
     #4D2 + t7 * t298 * t54 * t30 / 0.32D2
      t320 = FJET(XB1, XB2, s, 0.0D0, -t246, -t221, 0.0D0, t247, t319)
      t322 = x2 * x3
      t324 = 0.1D1 / (0.1D1 - x3 + t322)
      t325 = t35 * t324
      t326 = t2 * t325
      t328 = t2 * t322 * t324
      t329 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t49, -t325, x4)
      t333 = Sqrt(t34 * t70 * t35)
      t339 = 0.1D1 / (-z - x3 + t322 + 0.2D1 * t33 * t333) * t45 * t54
      t342 = FJET(XB1, XB2, s, 0.0D0, -t326, 0.0D0, t328, 0.0D0, t5 * t3
     #29 * t339 / 0.64D2)
      t347 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, t49, 0.10D1, x4)
      t351 = FJET(XB1, XB2, s, t223, -t221, t219, 0.0D0, -t229, t5 * t34
     #7 * t237 / 0.32D2)
      t356 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t49, -t325, x4)
      t360 = FJET(XB1, XB2, s, t328, 0.0D0, -t326, 0.0D0, 0.0D0, t5 * t3
     #56 * t339 / 0.64D2)
      bggbH4n2em1 = t140 * t139 + t211 * t210 + t240 * t5 * t230 * t237 
     #/ 0.32D2 + t295 * t294 + t320 * t319 + t342 * t5 * t329 * t339 / 0
     #.64D2 + t351 * t5 * t347 * t237 / 0.32D2 + t360 * t5 * t356 * t339
     # / 0.64D2

      end function



      doubleprecision function bggbH4n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t5 = t3 * t4
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = 0.1D1 / x1
      t13 = t8 * t3
      t14 = 0.1D1 - x2
      t15 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t14, 0.10D1, x4)
      t17 = 0.1D1 / x2
      t21 = t8 * lh
      t24 = bggbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = z ** 2
      t29 = x4 * 0.3141592653589793D1
      t30 = Sin(t29)
      t31 = t30 ** 2
      t34 = log(0.4D1 / t26 / z * t31)
      t35 = t34 * t3
      t41 = cos(t29)
      t45 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t52 = (-0.1D1 / (-z - x3 + 0.2D1 * t41 * t45) - t3) / x3
      t55 = -t5 * t8 * t9 / 0.64D2 - t13 * (t4 - t15) * t17 / 0.64D2 + t
     #21 * t5 / 0.64D2 - (t3 * t24 - t35 * t4) * t8 / 0.128D3 + t8 * t4 
     #* t52 / 0.128D3
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t55)
      t58 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t62 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t14, 0.10D1, x4)
      t70 = bggbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t79 = -t13 * t58 * t9 / 0.64D2 - t13 * (t58 - t62) * t17 / 0.64D2 
     #+ t21 * t3 * t58 / 0.64D2 - (t3 * t70 - t35 * t58) * t8 / 0.128D3 
     #+ t8 * t58 * t52 / 0.128D3
      t80 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t79)
      t82 = -0.1D1 + x1
      t83 = t2 * t82
      t86 = 0.1D1 / (-z - x1 + x1 * z)
      t88 = t2 * x1 * t86
      t89 = t1 ** 2
      t93 = s * t89 * x1 * t82 * t86
      t94 = bggbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t98 = FJET(XB1, XB2, s, 0.0D0, -t83, -t88, 0.0D0, t93, t13 * t94 *
     # t9 / 0.64D2)
      t104 = bggbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t108 = FJET(XB1, XB2, s, 0.0D0, -t88, -t83, 0.0D0, t93, t13 * t104
     # * t9 / 0.64D2)
      bggbH4n2em2 = t56 * t55 + t80 * t79 + t98 * t8 * t3 * t94 * t9 / 0
     #.64D2 + t108 * t8 * t3 * t104 * t9 / 0.64D2

      end function



      doubleprecision function bggbH4n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = bggbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t3 * t4 *
     # t8 / 0.128D3)
      t16 = bggbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8 * t3 *
     # t16 / 0.128D3)
      bggbH4n2em3 = -t11 * t3 * t4 * t8 / 0.128D3 - t19 * t8 * t3 * t16 
     #/ 0.128D3

      end function



      doubleprecision function bggbH4n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH41J1
      doubleprecision bggbH41J2
      doubleprecision bggbH41J3
      doubleprecision bggbH42J1
      doubleprecision bggbH42J2
      doubleprecision bggbH42J3
      bggbH4n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bggbH41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t6 = s * t2
      t8 = z + t2 * x1
      t12 = x3 * (0.1D1 - x2)
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t8 * x2 * t14)
      t27 = 0.1D1 - x1
      t30 = s - t6 * x1 / t8 * (t12 * t8 + x2 * t14 - 0.2D1 * t17 * t21)
     # - t6 * t27 * x3
      t31 = t30 * s
      t37 = z ** 2
      bggbH41J1 = 0.128D3 / 0.3D1 * t1 * t2 * x1 * z * wd * (0.2D1 * t31
     # * z + t31 * t2 * t27 * x3 - t31 - 0.2D1 * t1 * t37) / t30

      end function
  
   
 

      doubleprecision function bggbH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t6 = s * t2
      t8 = z + t2 * x1
      t12 = x3 * (0.1D1 - x2)
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t8 * x2 * t14)
      t27 = 0.1D1 - x1
      t30 = s - t6 * x1 / t8 * (t12 * t8 + x2 * t14 - 0.2D1 * t17 * t21)
     # - t6 * t27 * x3
      t31 = t30 * s
      bggbH41J2 = 0.128D3 / 0.3D1 * t1 * t2 * x1 * z * wd * (-0.2D1 * t3
     #1 * t2 * t27 * x3 + t31) / t30

      end function
  
   
 

      doubleprecision function bggbH41J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t4 = (0.1D1 - z) ** 2
      bggbH41J3 = 0.128D3 / 0.3D1 * t1 * s * t4 * x1 * z * wd * (0.1D1 -
     # x1) * x3

      end function
  
   
 

      doubleprecision function bggbH42J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = 0.1D1 - x2
      t12 = z + t2 * x1
      t13 = 0.1D1 / t12
      t15 = s * t2
      t17 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t5 * t12 * x2 * t17)
      t31 = x1 * t13 * (t17 * t5 * t12 + x2 * x3 + 0.2D1 * t22 * t27)
      t35 = s - t15 * t31 - t15 * t4 * t17
      t36 = t35 * s
      t41 = z ** 2
      bggbH42J1 = 0.128D3 / 0.3D1 * t1 * t2 * t4 * (z + x1 * t5 * t2) * 
     #t13 * z * wd * (0.2D1 * t36 * z + t36 * t2 * t31 - t36 - 0.2D1 * t
     #1 * t41) / t35

      end function
  
   
 

      doubleprecision function bggbH42J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = 0.1D1 - x2
      t12 = z + t2 * x1
      t13 = 0.1D1 / t12
      t15 = s * t2
      t17 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t5 * t12 * x2 * t17)
      t31 = x1 * t13 * (t17 * t5 * t12 + x2 * x3 + 0.2D1 * t22 * t27)
      t35 = s - t15 * t31 - t15 * t4 * t17
      t36 = t35 * s
      bggbH42J2 = 0.128D3 / 0.3D1 * t1 * t2 * t4 * (z + x1 * t5 * t2) * 
     #t13 * z * wd * (-0.2D1 * t36 * t2 * t31 + t36) / t35

      end function
  
   
 

      doubleprecision function bggbH42J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t7 = 0.1D1 - x2
      t14 = z + x1 * t3
      t15 = t14 ** 2
      t19 = 0.1D1 - x3
      t24 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(x3 * t7 * t14 * x2 * t19)
      bggbH42J3 = 0.128D3 / 0.3D1 * t1 * s * t4 * (0.1D1 - x1) * (z + x1
     # * t7 * t3) / t15 * z * wd * x1 * (t19 * t7 * t14 + x2 * x3 + 0.2D
     #1 * t24 * t29)

      end function
  
 