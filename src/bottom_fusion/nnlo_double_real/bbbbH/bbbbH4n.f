  
      subroutine bbbbH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH41J1  
      doubleprecision bbbbH41J2  
      doubleprecision bbbbH41J3  
      doubleprecision bbbbH42J1  
      doubleprecision bbbbH42J2  
      doubleprecision bbbbH42J3  
      doubleprecision bbbbH4n1e1  
      doubleprecision bbbbH4n1e0  
      doubleprecision bbbbH4n1em1  
      doubleprecision bbbbH4n1em2  
      doubleprecision bbbbH4n1em3  
      doubleprecision bbbbH4n1em4  
      doubleprecision bbbbH4n2e1  
      doubleprecision bbbbH4n2e0  
      doubleprecision bbbbH4n2em1  
      doubleprecision bbbbH4n2em2  
      doubleprecision bbbbH4n2em3  
      doubleprecision bbbbH4n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH4n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH4n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH4n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH4n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH4n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH4n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / z
      t4 = t3 * lh
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = t18 ** 2
      t20 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t23 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t29 = lh ** 2
      t31 = 0.3141592653589793D1 ** 2
      t33 = 0.180D3 * t29 - 0.30D2 * t31
      t34 = t3 * t33
      t39 = t3 * t7
      t41 = t19 * t18
      t53 = 0.60D2 * lh * t31 - 0.2884936567583026D3 - 0.120D3 * t29 * l
     #h
      t54 = t3 * t53
      t55 = t7 * t20
      t56 = t54 * t55
      t58 = 0.1D1 / x1
      t61 = lh * t7
      t62 = t3 * t23
      t63 = x3 * t9
      t64 = t15 * t12
      t67 = log(0.4D1 * t63 * t64)
      t68 = t67 * t3
      t70 = -0.1D1 + x3
      t71 = 0.1D1 / t70
      t72 = t64 * t71
      t75 = log(-0.4D1 * t63 * t72)
      t78 = cos(t10)
      t80 = Sqrt(-x3 * t70)
      t85 = 0.1D1 / (-x3 - z + 0.2D1 * t78 * t80 * z)
      t91 = t75 ** 2
      t96 = t3 * t8
      t98 = t67 ** 2
      t99 = t98 * t3
      t105 = t33 * t7
      t106 = t3 * t20
      t108 = t106 + t20 * t85
      t111 = 0.1D1 / x3
      t115 = x2 ** 2
      t116 = t115 * x3
      t117 = t116 * t16
      t119 = log(0.4D1 * t117)
      t120 = t119 * t3
      t122 = t116 * t9
      t125 = log(-0.4D1 * t122 * t72)
      t132 = -t108
      t137 = 0.1D1 / x2
      t138 = t137 * t58
      t141 = t115 * t9
      t144 = log(0.4D1 * t141 * t64)
      t145 = t144 * t3
      t151 = t144 ** 2
      t152 = t151 * t3
      t164 = log(0.4D1 * t64)
      t165 = t164 * t3
      t168 = t164 ** 2
      t169 = t168 * t3
      t172 = (0.180D3 * t165 * lh + t34 + 0.45D2 * t169) * t7
      t179 = t168 * t164 * t3
      t182 = (-t165 * t33 - 0.90D2 * t169 * lh + t54 - 0.15D2 * t179) * 
     #t7
      t185 = t168 ** 2
      t190 = t31 ** 2
      t191 = t29 ** 2
      t202 = (0.15D2 / 0.4D1 * t185 * t3 - t165 * t53 + t3 * (0.57698731
     #35166051D3 * lh + t190 + 0.60D2 * t191 - 0.60D2 * t29 * t31) + t16
     #9 * t33 / 0.2D1 + 0.30D2 * t179 * lh) * t7
      t210 = x3 * t12
      t213 = log(0.4D1 * t210 * t15)
      t214 = t213 ** 2
      t219 = log(-0.4D1 * t210 * t15 * t71)
      t220 = t219 ** 2
      t223 = t214 * t3 / 0.2D1 + t220 * t85 / 0.2D1
      t233 = -t219 * t85 - t213 * t3
      t240 = -t214 * t213 * t3 / 0.6D1 - t220 * t219 * t85 / 0.6D1
      t246 = t53 * t7
      t249 = t85 + t3
      t254 = t115 * t12
      t257 = log(0.4D1 * t254 * t15)
      t258 = t257 * t3
      t260 = t257 ** 2
      t261 = t260 * t3
      t274 = t260 * t257 * t3
      t285 = log(0.4D1 * t116 * t64)
      t286 = t285 * t3
      t290 = log(-0.4D1 * t116 * t72)
      t299 = t290 ** 2
      t304 = t285 ** 2
      t305 = t304 * t3
      t316 = -(-0.180D3 * t4 * t7 * (t8 + t19 * t20 / 0.2D1 - t18 * t23)
     # + t34 * t7 * (t23 - t18 * t20) + 0.90D2 * t39 * (-t18 * t8 - t41 
     #* t20 / 0.6D1 + t19 * t23 / 0.2D1) + t56) * t58 / 0.5760D4 - (-0.1
     #80D3 * t61 * (t62 - t68 * t20 + (t23 - t75 * t20) * t85) + 0.90D2 
     #* t7 * ((t8 - t75 * t23 + t91 * t20 / 0.2D1) * t85 + t96 - t68 * t
     #23 + t99 * t20 / 0.2D1) + t105 * t108) * t111 * t58 / 0.5760D4 + (
     #0.90D2 * t7 * (-t62 + t120 * t20 - (t23 - t125 * t20) * t85) - 0.1
     #80D3 * t61 * t132) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t
     #145 * t20 - t62) + 0.90D2 * t7 * (-t96 + t145 * t23 - t152 * t20 /
     # 0.2D1) - t105 * t106) * t137 * t58 / 0.2880D4 - t172 * t8 / 0.115
     #20D5 - t182 * t23 / 0.11520D5 - t202 * t20 / 0.11520D5 - ((0.90D2 
     #* t7 * t23 - 0.180D3 * t61 * t20) * t223 + (0.90D2 * t7 * t8 - 0.1
     #80D3 * t61 * t23 + t105 * t20) * t233 + 0.90D2 * t55 * t240 + (-0.
     #180D3 * t61 * t8 + t105 * t23 + t246 * t20) * t249) * t111 / 0.115
     #20D5 + (-0.180D3 * t61 * (-t96 + t258 * t23 - t261 * t20 / 0.2D1) 
     #+ t105 * (t258 * t20 - t62) + 0.90D2 * t7 * (t258 * t8 - t261 * t2
     #3 / 0.2D1 + t274 * t20 / 0.6D1) - t56) * t137 / 0.5760D4 + (-0.180
     #D3 * t61 * (-t62 + t286 * t20 - (t23 - t290 * t20) * t85) + 0.90D2
     # * t7 * (t286 * t23 - t96 - (t8 - t290 * t23 + t299 * t20 / 0.2D1)
     # * t85 - t305 * t20 / 0.2D1) + t105 * t132) * t111 * t137 / 0.5760
     #D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t316)
      t319 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t321 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t322 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t341 = t7 * t322
      t342 = t54 * t341
      t346 = t3 * t319
      t359 = t3 * t321
      t366 = t3 * t322
      t368 = t366 + t322 * t85
      t381 = -t368
      t476 = -(-0.180D3 * t4 * t7 * (-t18 * t319 + t321 + t19 * t322 / 0
     #.2D1) + t34 * t7 * (t319 - t18 * t322) + 0.90D2 * t39 * (-t18 * t3
     #21 - t41 * t322 / 0.6D1 + t19 * t319 / 0.2D1) + t342) * t58 / 0.57
     #60D4 - (-0.180D3 * t61 * (t346 - t68 * t322 + (t319 - t75 * t322) 
     #* t85) + 0.90D2 * t7 * ((-t75 * t319 + t321 + t91 * t322 / 0.2D1) 
     #* t85 + t359 - t68 * t319 + t99 * t322 / 0.2D1) + t105 * t368) * t
     #111 * t58 / 0.5760D4 + (0.90D2 * t7 * (-t346 - (t319 - t125 * t322
     #) * t85 + t120 * t322) - 0.180D3 * t61 * t381) * t111 * t138 / 0.2
     #880D4 + (-0.180D3 * t61 * (t145 * t322 - t346) + 0.90D2 * t7 * (-t
     #359 + t145 * t319 - t152 * t322 / 0.2D1) - t105 * t366) * t137 * t
     #58 / 0.2880D4 - t172 * t321 / 0.11520D5 - t182 * t319 / 0.11520D5 
     #- t202 * t322 / 0.11520D5 - ((0.90D2 * t7 * t319 - 0.180D3 * t61 *
     # t322) * t223 + (0.90D2 * t7 * t321 - 0.180D3 * t61 * t319 + t105 
     #* t322) * t233 + 0.90D2 * t341 * t240 + (-0.180D3 * t61 * t321 + t
     #105 * t319 + t246 * t322) * t249) * t111 / 0.11520D5 + (-0.180D3 *
     # t61 * (-t359 + t258 * t319 - t261 * t322 / 0.2D1) + t105 * (t258 
     #* t322 - t346) + 0.90D2 * t7 * (t258 * t321 - t261 * t319 / 0.2D1 
     #+ t274 * t322 / 0.6D1) - t342) * t137 / 0.5760D4 + (-0.180D3 * t61
     # * (-t346 + t286 * t322 - (t319 - t290 * t322) * t85) + 0.90D2 * t
     #7 * (t286 * t319 - (-t290 * t319 + t321 + t299 * t322 / 0.2D1) * t
     #85 - t359 - t305 * t322 / 0.2D1) + t105 * t381) * t111 * t137 / 0.
     #5760D4
      t477 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t476)
      t479 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t476)
      t481 = t2 * x1
      t482 = -0.1D1 + x1
      t483 = x1 * z
      t484 = 0.1D1 - x1 + t483
      t485 = 0.1D1 / t484
      t487 = t2 * t482 * t485
      t488 = t1 ** 2
      t489 = s * t488
      t491 = x1 * t482 * t485
      t492 = t489 * t491
      t493 = t15 * t485
      t494 = t482 ** 2
      t495 = t493 * t494
      t498 = log(0.4D1 * t13 * t495)
      t499 = t498 ** 2
      t500 = -t482
      t501 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t500, 0.10D1, 0.10D1, x4)
      t504 = bbbbH42J3(s, XB1, XB2, z, lh, wd, t500, 0.10D1, 0.10D1, x4)
      t505 = bbbbH42J2(s, XB1, XB2, z, lh, wd, t500, 0.10D1, 0.10D1, x4)
      t518 = t499 * t498
      t529 = t484 * t505
      t530 = t63 * t12
      t532 = t493 * t494 * t71
      t535 = log(-0.4D1 * t530 * t532)
      t536 = t535 * t484
      t539 = x3 * x1
      t540 = 0.2D1 * t539
      t541 = x3 * t484
      t543 = Sqrt(-t541 * t70)
      t547 = t539 * z
      t548 = 0.3D1 * t547
      t549 = x1 * t14
      t550 = x3 * t14
      t551 = t550 * x1
      t553 = 0.2D1 * t63 * z
      t554 = t63 * t14
      t555 = -z + t540 - t63 - x3 + t483 + 0.2D1 * t78 * t543 * z - t548
     # - t549 + t551 + t553 - t554
      t556 = 0.1D1 / t555
      t560 = log(0.4D1 * t530 * t495)
      t561 = t560 * t3
      t563 = t3 * t505
      t567 = t3 * t504
      t569 = t560 ** 2
      t570 = t569 * t3
      t575 = t535 ** 2
      t576 = t575 * t484
      t586 = t3 * t501
      t587 = -t484 * t501 * t556 - t586
      t593 = t485 * t494
      t597 = log(0.4D1 * t122 * t64 * t593)
      t598 = t597 * t3
      t600 = t116 * t13
      t603 = log(-0.4D1 * t600 * t532)
      t604 = t603 * t484
      t618 = t141 * t12
      t621 = log(0.4D1 * t618 * t495)
      t622 = t621 * t3
      t627 = t621 ** 2
      t628 = t627 * t3
      t640 = -(-0.180D3 * t4 * t7 * (-t499 * t501 / 0.2D1 - t504 + t498 
     #* t505) + t34 * t7 * (-t505 + t498 * t501) + 0.90D2 * t39 * (t498 
     #* t504 - t499 * t505 / 0.2D1 + t518 * t501 / 0.6D1) - t54 * t7 * t
     #501) * t58 / 0.5760D4 - (-0.180D3 * t61 * (-(t529 - t536 * t501) *
     # t556 + t561 * t501 - t563) + 0.90D2 * t7 * (-t567 + t561 * t505 -
     # t570 * t501 / 0.2D1 - (t484 * t504 - t536 * t505 + t576 * t501 / 
     #0.2D1) * t556) + t105 * t587) * t111 * t58 / 0.5760D4 + (0.90D2 * 
     #t7 * (-t598 * t501 + t563 + (t529 - t604 * t501) * t556) + 0.180D3
     # * t61 * t587) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (-t622
     # * t501 + t563) + 0.90D2 * t7 * (t567 + t628 * t501 / 0.2D1 - t622
     # * t505) + t105 * t586) * t137 * t58 / 0.2880D4
      t641 = FJET(XB1, XB2, s, 0.0D0, t481, -t487, 0.0D0, -t492, t640)
      t644 = x2 * t1 * s
      t645 = -0.1D1 + x2
      t647 = t645 * t1 * s
      t648 = t64 * t645
      t651 = log(-0.4D1 * t122 * t648)
      t652 = x2 * z
      t654 = 0.1D1 / (-x2 - z + t652)
      t655 = t651 * t654
      t656 = -t645
      t657 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t656, 0.10D1, x4)
      t659 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t656, 0.10D1, x4)
      t660 = t654 * t659
      t664 = t654 * t657
      t673 = log(-0.4D1 * t141 * t648)
      t674 = t673 * t654
      t680 = t673 ** 2
      t681 = t680 * t654
      t684 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t656, 0.10D1, x4)
      t685 = t654 * t684
      t689 = t105 * t664
      t694 = t15 * t645
      t697 = log(-0.4D1 * t254 * t694)
      t698 = t697 ** 2
      t699 = t698 * t654
      t702 = t697 * t654
      t712 = t698 * t697 * t654
      t726 = log(-0.4D1 * t116 * t648)
      t727 = t726 * t654
      t732 = t726 ** 2
      t733 = t732 * t654
      t744 = (0.90D2 * t7 * (t655 * t657 - t660) + 0.180D3 * t61 * t664)
     # * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t674 * t657 - t660)
     # + 0.90D2 * t7 * (t674 * t659 - t681 * t657 / 0.2D1 - t685) - t689
     #) * t137 * t58 / 0.2880D4 + (-0.180D3 * t61 * (-t685 - t699 * t657
     # / 0.2D1 + t702 * t659) + t105 * (-t660 + t702 * t657) + 0.90D2 * 
     #t7 * (t702 * t684 + t712 * t657 / 0.6D1 - t699 * t659 / 0.2D1) - t
     #246 * t664) * t137 / 0.5760D4 + (-0.180D3 * t61 * (t727 * t657 - t
     #660) + 0.90D2 * t7 * (-t685 - t733 * t657 / 0.2D1 + t727 * t659) -
     # t689) * t111 * t137 / 0.5760D4
      t745 = FJET(XB1, XB2, s, 0.0D0, t644, 0.0D0, -t647, 0.0D0, t744)
      t747 = x2 * x3
      t750 = Sqrt(x3 * t645 * t70)
      t751 = t78 * t750
      t753 = 0.2D1 * t751 * x2
      t755 = 0.1D1 - x3 + t747
      t756 = 0.1D1 / t755
      t758 = t2 * (0.1D1 - x3 - x2 + t747 + t116 + t753) * t756
      t763 = t2 * x2 * (-0.1D1 + t747 + 0.2D1 * t751) * t756
      t764 = t755 ** 2
      t765 = 0.1D1 / t764
      t767 = t694 * t70 * t765
      t770 = log(0.4D1 * t600 * t767)
      t771 = t747 * z
      t772 = t116 * z
      t778 = 0.1D1 / (x2 + x3 - t116 + z - t652 - t771 + t772 - t753 - 0
     #.2D1 * t751 * z + 0.2D1 * t751 * t652)
      t779 = t770 * t778
      t780 = t70 * t756
      t781 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t656, -t780, x4)
      t783 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t656, -t780, x4)
      t784 = t778 * t783
      t788 = t778 * t781
      t798 = log(0.4D1 * t116 * t12 * t767)
      t799 = t798 * t778
      t804 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, t656, -t780, x4)
      t807 = t798 ** 2
      t808 = t807 * t778
      t819 = (0.90D2 * t7 * (t779 * t781 - t784) + 0.180D3 * t61 * t788)
     # * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (-t784 + t799 * t781
     #) + 0.90D2 * t7 * (-t778 * t804 + t799 * t783 - t808 * t781 / 0.2D
     #1) - t105 * t788) * t111 * t137 / 0.5760D4
      t820 = FJET(XB1, XB2, s, 0.0D0, t758, 0.0D0, -t763, 0.0D0, t819)
      t823 = t1 * t482
      t825 = t645 * s * t823 * t485
      t827 = x2 * s * t823
      t829 = t489 * t645 * t491
      t830 = x2 * x1
      t831 = t830 * z
      t833 = 0.1D1 / (x2 - t830 + z - t652 + t831)
      t834 = bbbbH42J2(s, XB1, XB2, z, lh, wd, t500, t656, 0.10D1, x4)
      t835 = t833 * t834
      t837 = t493 * t494 * t645
      t840 = log(-0.4D1 * t600 * t837)
      t841 = t840 * t833
      t842 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t500, t656, 0.10D1, x4)
      t847 = t833 * t842
      t855 = log(-0.4D1 * t618 * t837)
      t856 = t855 * t833
      t861 = t855 ** 2
      t862 = t861 * t833
      t865 = bbbbH42J3(s, XB1, XB2, z, lh, wd, t500, t656, 0.10D1, x4)
      t876 = (0.90D2 * t7 * (-t835 + t841 * t842) + 0.180D3 * t61 * t847
     #) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (-t835 + t856 * t84
     #2) + 0.90D2 * t7 * (-t862 * t842 / 0.2D1 - t833 * t865 + t856 * t8
     #34) - t105 * t847) * t137 * t58 / 0.2880D4
      t877 = FJET(XB1, XB2, s, 0.0D0, t825, t481, -t827, t829, t876)
      t879 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t656, 0.10D1, x4)
      t881 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t656, 0.10D1, x4)
      t882 = t654 * t881
      t886 = t654 * t879
      t900 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, t656, 0.10D1, x4)
      t901 = t654 * t900
      t905 = t105 * t886
      t945 = (0.90D2 * t7 * (t655 * t879 - t882) + 0.180D3 * t61 * t886)
     # * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t674 * t879 - t882)
     # + 0.90D2 * t7 * (-t681 * t879 / 0.2D1 + t674 * t881 - t901) - t90
     #5) * t137 * t58 / 0.2880D4 + (-0.180D3 * t61 * (-t901 - t699 * t87
     #9 / 0.2D1 + t702 * t881) + t105 * (-t882 + t702 * t879) + 0.90D2 *
     # t7 * (t702 * t900 + t712 * t879 / 0.6D1 - t699 * t881 / 0.2D1) - 
     #t246 * t886) * t137 / 0.5760D4 + (-0.180D3 * t61 * (-t882 + t727 *
     # t879) + 0.90D2 * t7 * (-t901 + t727 * t881 - t733 * t879 / 0.2D1)
     # - t905) * t111 * t137 / 0.5760D4
      t946 = FJET(XB1, XB2, s, 0.0D0, -t647, 0.0D0, t644, 0.0D0, t945)
      t948 = FJET(XB1, XB2, s, 0.0D0, -t487, t481, 0.0D0, -t492, t640)
      t950 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t656, -t780, x4)
      t951 = t778 * t950
      t952 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t656, -t780, x4)
      t957 = t778 * t952
      t970 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t656, -t780, x4)
      t981 = (0.90D2 * t7 * (-t951 + t779 * t952) + 0.180D3 * t61 * t957
     #) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t799 * t952 - t951
     #) + 0.90D2 * t7 * (-t808 * t952 / 0.2D1 - t778 * t970 + t799 * t95
     #0) - t105 * t957) * t111 * t137 / 0.5760D4
      t982 = FJET(XB1, XB2, s, 0.0D0, -t763, 0.0D0, t758, 0.0D0, t981)
      t984 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t316)
      t986 = bbbbH41J3(s, XB1, XB2, z, lh, wd, t500, 0.10D1, 0.10D1, x4)
      t987 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t500, 0.10D1, 0.10D1, x4)
      t990 = bbbbH41J2(s, XB1, XB2, z, lh, wd, t500, 0.10D1, 0.10D1, x4)
      t1013 = t484 * t990
      t1018 = t3 * t990
      t1022 = t3 * t986
      t1035 = t3 * t987
      t1038 = -t1035 - t484 * t987 * t556
      t1073 = -(-0.180D3 * t4 * t7 * (-t986 - t499 * t987 / 0.2D1 + t498
     # * t990) + t34 * t7 * (t498 * t987 - t990) + 0.90D2 * t39 * (t518 
     #* t987 / 0.6D1 - t499 * t990 / 0.2D1 + t498 * t986) - t54 * t7 * t
     #987) * t58 / 0.5760D4 - (-0.180D3 * t61 * (-(t1013 - t536 * t987) 
     #* t556 + t561 * t987 - t1018) + 0.90D2 * t7 * (-t1022 - t570 * t98
     #7 / 0.2D1 + t561 * t990 - (t484 * t986 - t536 * t990 + t576 * t987
     # / 0.2D1) * t556) + t105 * t1038) * t111 * t58 / 0.5760D4 + (0.90D
     #2 * t7 * ((t1013 - t604 * t987) * t556 - t598 * t987 + t1018) + 0.
     #180D3 * t61 * t1038) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * 
     #(t1018 - t622 * t987) + 0.90D2 * t7 * (t1022 - t622 * t990 + t628 
     #* t987 / 0.2D1) + t105 * t1035) * t137 * t58 / 0.2880D4
      t1074 = FJET(XB1, XB2, s, t481, 0.0D0, 0.0D0, -t487, -t492, t1073)
      t1076 = t317 * t316 + t477 * t476 + t479 * t476 + t641 * t640 + t7
     #45 * t744 + t820 * t819 + t877 * t876 + t946 * t945 + t948 * t640 
     #+ t982 * t981 + t984 * t316 + t1074 * t1073
      t1077 = bbbbH41J2(s, XB1, XB2, z, lh, wd, t500, t656, 0.10D1, x4)
      t1078 = t833 * t1077
      t1079 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t500, t656, 0.10D1, x4)
      t1084 = t833 * t1079
      t1097 = bbbbH41J3(s, XB1, XB2, z, lh, wd, t500, t656, 0.10D1, x4)
      t1107 = (0.90D2 * t7 * (-t1078 + t841 * t1079) + 0.180D3 * t61 * t
     #1084) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (-t1078 + t856 
     #* t1079) + 0.90D2 * t7 * (-t862 * t1079 / 0.2D1 + t856 * t1077 - t
     #833 * t1097) - t105 * t1084) * t137 * t58 / 0.2880D4
      t1108 = FJET(XB1, XB2, s, t481, -t827, 0.0D0, t825, t829, t1107)
      t1110 = FJET(XB1, XB2, s, t644, 0.0D0, -t647, 0.0D0, 0.0D0, t945)
      t1112 = FJET(XB1, XB2, s, t758, 0.0D0, -t763, 0.0D0, 0.0D0, t981)
      t1114 = FJET(XB1, XB2, s, t825, 0.0D0, -t827, t481, t829, t1107)
      t1117 = t481 * t747 * t756
      t1118 = t2 * t482
      t1119 = t645 * t70
      t1121 = Sqrt(t541 * t1119)
      t1122 = t78 * t1121
      t1124 = 0.2D1 * t1122 * x2
      t1125 = t116 * t483
      t1126 = t116 * x1
      t1130 = t1118 * (t1124 + t116 + 0.1D1 - x3 - x2 + t747 + t1125 - t
     #1126) * t485 * t756
      t1134 = t70 * s * t1 * x1 * t756
      t1140 = t1118 * x2 * (-0.1D1 + t747 + x1 - t539 - t483 + t547 + 0.
     #2D1 * t1122) * t485 * t756
      t1144 = 0.2D1 * t830 - z - x2 + t483 - t548 + t551 + t553 - t554 +
     # t771 - t772 + t540 - t63 - t549 + t652 + t1124 - t1126 + 0.2D1 * 
     #t1122 * z
      t1148 = x2 * t9
      t1166 = -t747 * x1 + t63 * x2 + t830 * t14 + 0.2D1 * t1148 * z - t
     #1148 * t14 + t116 + 0.2D1 * t1122 * t831 - 0.3D1 * t831 - x3 - t11
     #48 + t1125 - 0.2D1 * t1122 * t830 - 0.2D1 * t1122 * t652 + 0.2D1 *
     # t747 * t483 - t550 * t830 - 0.2D1 * t63 * t652 + t63 * t14 * x2
      t1168 = 0.1D1 / (t1144 + t1166)
      t1169 = t484 * t1168
      t1170 = bbbbH42J2(s, XB1, XB2, z, lh, wd, t500, t656, -t780, x4)
      t1176 = log(0.4D1 * t117 * t593 * t1119 * t765)
      t1177 = t1176 * t484
      t1178 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t500, t656, -t780, x4)
      t1187 = 0.90D2 * t7 * (-t1169 * t1170 + t1177 * t1168 * t1178) + 0
     #.180D3 * t61 * t1169 * t1178
      t1190 = t1187 * t111 * t138 / 0.2880D4
      t1191 = FJET(XB1, XB2, s, t1117, -t1130, -t1134, t1140, t829, t119
     #0)
      t1194 = t111 * t137 * t58
      t1197 = FJET(XB1, XB2, s, t1140, -t1134, -t1130, t1117, t829, t119
     #0)
      t1201 = FJET(XB1, XB2, s, -t647, 0.0D0, t644, 0.0D0, 0.0D0, t744)
      t1203 = FJET(XB1, XB2, s, -t487, 0.0D0, 0.0D0, t481, -t492, t1073)
      t1205 = FJET(XB1, XB2, s, -t827, t481, t825, 0.0D0, t829, t876)
      t1207 = FJET(XB1, XB2, s, -t763, 0.0D0, t758, 0.0D0, 0.0D0, t819)
      t1209 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t500, t656, -t780, x4)
      t1212 = bbbbH41J2(s, XB1, XB2, z, lh, wd, t500, t656, -t780, x4)
      t1220 = 0.90D2 * t7 * (t1177 * t1168 * t1209 - t1169 * t1212) + 0.
     #180D3 * t61 * t1169 * t1209
      t1223 = t1220 * t111 * t138 / 0.2880D4
      t1224 = FJET(XB1, XB2, s, -t1134, t1140, t1117, -t1130, t829, t122
     #3)
      t1228 = FJET(XB1, XB2, s, -t1130, t1117, t1140, -t1134, t829, t122
     #3)
      t1232 = t1108 * t1107 + t1110 * t945 + t1112 * t981 + t1114 * t110
     #7 + t1191 * t1187 * t1194 / 0.2880D4 + t1197 * t1187 * t1194 / 0.2
     #880D4 + t1201 * t744 + t1203 * t1073 + t1205 * t876 + t1207 * t819
     # + t1224 * t1220 * t1194 / 0.2880D4 + t1228 * t1220 * t1194 / 0.28
     #80D4
      bbbbH4n1e1 = t1076 + t1232

      end function



      doubleprecision function bbbbH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t8 = t6 * t7
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = t19 * t6
      t21 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t23 = -0.1D1 + x3
      t24 = 0.1D1 / t23
      t25 = t16 * t24
      t28 = log(-0.4D1 * t10 * t25)
      t31 = cos(t13)
      t33 = Sqrt(-x3 * t23)
      t38 = 0.1D1 / (-x3 - z + 0.2D1 * t31 * t33 * z)
      t43 = lh * t5
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
      t78 = t6 * lh
      t79 = t9 * t15
      t82 = log(0.4D1 * t79 * t12)
      t88 = t6 * t5
      t89 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t90 = t82 ** 2
      t97 = lh ** 2
      t99 = 0.3141592653589793D1 ** 2
      t101 = 0.180D3 * t97 - 0.30D2 * t99
      t102 = t101 * t5
      t103 = t102 * t44
      t112 = x3 * t15
      t116 = log(-0.4D1 * t112 * t12 * t24)
      t120 = log(0.4D1 * t112 * t12)
      t122 = -t116 * t38 - t120 * t6
      t125 = t120 ** 2
      t127 = t116 ** 2
      t130 = t125 * t6 / 0.2D1 + t127 * t38 / 0.2D1
      t139 = t38 + t6
      t144 = t62 * x3
      t147 = log(0.4D1 * t144 * t16)
      t148 = t147 * t6
      t152 = log(-0.4D1 * t144 * t25)
      t165 = t62 * t15
      t168 = log(0.4D1 * t165 * t12)
      t169 = t168 * t6
      t176 = t168 ** 2
      t177 = t176 * t6
      t188 = log(0.4D1 * t16)
      t189 = t188 * t6
      t192 = (-0.180D3 * t78 - 0.90D2 * t189) * t5
      t198 = t188 ** 2
      t199 = t198 * t6
      t202 = (0.180D3 * t189 * lh + t6 * t101 + 0.45D2 * t199) * t5
      t218 = (-t189 * t101 - 0.90D2 * t199 * lh + t6 * (0.60D2 * lh * t9
     #9 - 0.2884936567583026D3 - 0.120D3 * t97 * lh) - 0.15D2 * t198 * t
     #188 * t6) * t5
      t221 = -(0.90D2 * t5 * (t8 - t20 * t21 + (t7 - t28 * t21) * t38) -
     # 0.180D3 * t43 * t46) * t50 * t52 / 0.5760D4 + t5 * t55 * t59 / 0.
     #32D2 + (0.90D2 * t5 * (t67 * t21 - t8) + 0.180D3 * t43 * t44) * t5
     #7 * t52 / 0.2880D4 - (-0.180D3 * t78 * t5 * (t7 - t82 * t21) + 0.9
     #0D2 * t88 * (t89 + t90 * t21 / 0.2D1 - t82 * t7) + t103) * t52 / 0
     #.5760D4 - ((0.90D2 * t5 * t7 - 0.180D3 * t43 * t21) * t122 + 0.90D
     #2 * t5 * t21 * t130 + (0.90D2 * t5 * t89 - 0.180D3 * t43 * t7 + t1
     #02 * t21) * t139) * t50 / 0.11520D5 + (0.90D2 * t5 * (-t8 + t148 *
     # t21 - (t7 - t152 * t21) * t38) - 0.180D3 * t43 * t55) * t50 * t57
     # / 0.5760D4 + (-0.180D3 * t43 * (t169 * t21 - t8) + 0.90D2 * t5 * 
     #(-t6 * t89 + t169 * t7 - t177 * t21 / 0.2D1) - t103) * t57 / 0.576
     #0D4 - t192 * t89 / 0.11520D5 - t202 * t7 / 0.11520D5 - t218 * t21 
     #/ 0.11520D5
      t222 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t221)
      t224 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t225 = t6 * t224
      t226 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t234 = t6 * t226
      t236 = t234 + t226 * t38
      t243 = -t236
      t263 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t269 = t102 * t234
      t325 = -(0.90D2 * t5 * (t225 - t20 * t226 + (t224 - t28 * t226) * 
     #t38) - 0.180D3 * t43 * t236) * t50 * t52 / 0.5760D4 + t5 * t243 * 
     #t59 / 0.32D2 + (0.90D2 * t5 * (t67 * t226 - t225) + 0.180D3 * t43 
     #* t234) * t57 * t52 / 0.2880D4 - (-0.180D3 * t78 * t5 * (t224 - t8
     #2 * t226) + 0.90D2 * t88 * (-t82 * t224 + t263 + t90 * t226 / 0.2D
     #1) + t269) * t52 / 0.5760D4 - ((0.90D2 * t5 * t224 - 0.180D3 * t43
     # * t226) * t122 + 0.90D2 * t5 * t226 * t130 + (0.90D2 * t5 * t263 
     #- 0.180D3 * t43 * t224 + t102 * t226) * t139) * t50 / 0.11520D5 + 
     #(0.90D2 * t5 * (-t225 + t148 * t226 - (t224 - t152 * t226) * t38) 
     #- 0.180D3 * t43 * t243) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 *
     # (t169 * t226 - t225) + 0.90D2 * t5 * (-t6 * t263 + t169 * t224 - 
     #t177 * t226 / 0.2D1) - t269) * t57 / 0.5760D4 - t192 * t263 / 0.11
     #520D5 - t202 * t224 / 0.11520D5 - t218 * t226 / 0.11520D5
      t326 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t325)
      t328 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t325)
      t330 = t2 * x1
      t331 = -0.1D1 + x1
      t332 = x1 * z
      t333 = 0.1D1 - x1 + t332
      t334 = 0.1D1 / t333
      t336 = t2 * t331 * t334
      t337 = t1 ** 2
      t338 = s * t337
      t340 = x1 * t331 * t334
      t341 = t338 * t340
      t342 = -t331
      t343 = bbbbH42J2(s, XB1, XB2, z, lh, wd, t342, 0.10D1, 0.10D1, x4)
      t345 = t10 * t15
      t346 = t12 * t334
      t347 = t331 ** 2
      t352 = log(-0.4D1 * t345 * t346 * t347 * t24)
      t353 = t352 * t333
      t354 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t342, 0.10D1, 0.10D1, x4)
      t357 = x3 * x1
      t358 = 0.2D1 * t357
      t359 = x3 * t333
      t361 = Sqrt(-t359 * t23)
      t365 = t357 * z
      t366 = 0.3D1 * t365
      t367 = x1 * t11
      t368 = x3 * t11
      t369 = t368 * x1
      t371 = 0.2D1 * t10 * z
      t372 = t10 * t11
      t373 = -z + t358 - t10 - x3 + t332 + 0.2D1 * t31 * t361 * z - t366
     # - t367 + t369 + t371 - t372
      t374 = 0.1D1 / t373
      t376 = t346 * t347
      t379 = log(0.4D1 * t345 * t376)
      t380 = t379 * t6
      t382 = t6 * t343
      t388 = t6 * t354
      t389 = -t333 * t354 * t374 - t388
      t400 = t63 * t15
      t403 = log(0.4D1 * t400 * t376)
      t404 = t403 * t6
      t417 = log(0.4D1 * t79 * t376)
      t423 = t417 ** 2
      t426 = bbbbH42J3(s, XB1, XB2, z, lh, wd, t342, 0.10D1, 0.10D1, x4)
      t435 = -(0.90D2 * t5 * (-(t333 * t343 - t353 * t354) * t374 + t380
     # * t354 - t382) - 0.180D3 * t43 * t389) * t50 * t52 / 0.5760D4 - t
     #5 * t389 * t59 / 0.32D2 + (0.90D2 * t5 * (-t404 * t354 + t382) - 0
     #.180D3 * t43 * t388) * t57 * t52 / 0.2880D4 - (-0.180D3 * t78 * t5
     # * (-t343 + t417 * t354) + 0.90D2 * t88 * (-t423 * t354 / 0.2D1 - 
     #t426 + t417 * t343) - t102 * t388) * t52 / 0.5760D4
      t436 = FJET(XB1, XB2, s, 0.0D0, t330, -t336, 0.0D0, -t341, t435)
      t439 = x2 * t1 * s
      t440 = -0.1D1 + x2
      t442 = t440 * t1 * s
      t443 = x2 * z
      t445 = 0.1D1 / (-x2 - z + t443)
      t446 = t5 * t445
      t447 = -t440
      t448 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t447, 0.10D1, x4)
      t452 = t16 * t440
      t455 = log(-0.4D1 * t63 * t452)
      t456 = t455 * t445
      t458 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t447, 0.10D1, x4)
      t459 = t445 * t458
      t463 = t445 * t448
      t465 = 0.180D3 * t43 * t463
      t472 = log(-0.4D1 * t144 * t452)
      t473 = t472 * t445
      t482 = t12 * t440
      t485 = log(-0.4D1 * t165 * t482)
      t486 = t485 * t445
      t491 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t447, 0.10D1, x4)
      t493 = t485 ** 2
      t494 = t493 * t445
      t505 = -t446 * t448 * t59 / 0.32D2 + (0.90D2 * t5 * (t456 * t448 -
     # t459) + t465) * t57 * t52 / 0.2880D4 + (0.90D2 * t5 * (t473 * t44
     #8 - t459) + t465) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 * (-t45
     #9 + t486 * t448) + 0.90D2 * t5 * (-t445 * t491 - t494 * t448 / 0.2
     #D1 + t486 * t458) - t102 * t463) * t57 / 0.5760D4
      t506 = FJET(XB1, XB2, s, 0.0D0, t439, 0.0D0, -t442, 0.0D0, t505)
      t508 = x2 * x3
      t511 = Sqrt(x3 * t440 * t23)
      t512 = t31 * t511
      t514 = 0.2D1 * t512 * x2
      t516 = 0.1D1 - x3 + t508
      t517 = 0.1D1 / t516
      t519 = t2 * (0.1D1 - x3 - x2 + t508 + t144 + t514) * t517
      t524 = t2 * x2 * (-0.1D1 + t508 + 0.2D1 * t512) * t517
      t525 = t508 * z
      t526 = t144 * z
      t532 = 0.1D1 / (x2 + x3 - t144 + z - t443 - t525 + t526 - t514 - 0
     #.2D1 * t512 * z + 0.2D1 * t512 * t443)
      t533 = t5 * t532
      t534 = t23 * t517
      t535 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t447, -t534, x4)
      t539 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t447, -t534, x4)
      t542 = t516 ** 2
      t548 = log(0.4D1 * t144 * t15 * t482 * t23 / t542)
      t549 = t548 * t532
      t561 = -t533 * t535 * t59 / 0.32D2 + (0.90D2 * t5 * (-t532 * t539 
     #+ t549 * t535) + 0.180D3 * t43 * t532 * t535) * t50 * t57 / 0.5760
     #D4
      t562 = FJET(XB1, XB2, s, 0.0D0, t519, 0.0D0, -t524, 0.0D0, t561)
      t565 = t1 * t331
      t567 = t440 * s * t565 * t334
      t569 = x2 * s * t565
      t571 = t338 * t440 * t340
      t572 = x2 * x1
      t573 = t572 * z
      t575 = 0.1D1 / (x2 - t572 + z - t443 + t573)
      t576 = t5 * t575
      t577 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t342, t447, 0.10D1, x4)
      t581 = bbbbH42J2(s, XB1, XB2, z, lh, wd, t342, t447, 0.10D1, x4)
      t587 = log(-0.4D1 * t400 * t346 * t347 * t440)
      t588 = t587 * t575
      t600 = -t576 * t577 * t59 / 0.32D2 + (0.90D2 * t5 * (-t575 * t581 
     #+ t588 * t577) + 0.180D3 * t43 * t575 * t577) * t57 * t52 / 0.2880
     #D4
      t601 = FJET(XB1, XB2, s, 0.0D0, t567, t330, -t569, t571, t600)
      t603 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t447, 0.10D1, x4)
      t608 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t447, 0.10D1, x4)
      t609 = t445 * t608
      t613 = t445 * t603
      t615 = 0.180D3 * t43 * t613
      t632 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, t447, 0.10D1, x4)
      t644 = -t446 * t603 * t59 / 0.32D2 + (0.90D2 * t5 * (t456 * t603 -
     # t609) + t615) * t57 * t52 / 0.2880D4 + (0.90D2 * t5 * (-t609 + t4
     #73 * t603) + t615) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 * (-t6
     #09 + t486 * t603) + 0.90D2 * t5 * (-t445 * t632 - t494 * t603 / 0.
     #2D1 + t486 * t608) - t102 * t613) * t57 / 0.5760D4
      t645 = FJET(XB1, XB2, s, 0.0D0, -t442, 0.0D0, t439, 0.0D0, t644)
      t647 = FJET(XB1, XB2, s, 0.0D0, -t336, t330, 0.0D0, -t341, t435)
      t649 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t447, -t534, x4)
      t651 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t447, -t534, x4)
      t666 = (0.90D2 * t5 * (t549 * t649 - t532 * t651) + 0.180D3 * t43 
     #* t532 * t649) * t50 * t57 / 0.5760D4 - t533 * t649 * t59 / 0.32D2
      t667 = FJET(XB1, XB2, s, 0.0D0, -t524, 0.0D0, t519, 0.0D0, t666)
      t669 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t221)
      t671 = bbbbH41J2(s, XB1, XB2, z, lh, wd, t342, 0.10D1, 0.10D1, x4)
      t673 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t342, 0.10D1, 0.10D1, x4)
      t678 = t6 * t671
      t682 = t6 * t673
      t685 = -t682 - t333 * t673 * t374
      t711 = bbbbH41J3(s, XB1, XB2, z, lh, wd, t342, 0.10D1, 0.10D1, x4)
      t722 = -(0.90D2 * t5 * (-(t333 * t671 - t353 * t673) * t374 + t380
     # * t673 - t678) - 0.180D3 * t43 * t685) * t50 * t52 / 0.5760D4 - t
     #5 * t685 * t59 / 0.32D2 + (0.90D2 * t5 * (t678 - t404 * t673) - 0.
     #180D3 * t43 * t682) * t57 * t52 / 0.2880D4 - (-0.180D3 * t78 * t5 
     #* (t417 * t673 - t671) + 0.90D2 * t88 * (-t711 - t423 * t673 / 0.2
     #D1 + t417 * t671) - t102 * t682) * t52 / 0.5760D4
      t723 = FJET(XB1, XB2, s, t330, 0.0D0, 0.0D0, -t336, -t341, t722)
      t725 = t222 * t221 + t326 * t325 + t328 * t325 + t436 * t435 + t50
     #6 * t505 + t562 * t561 + t601 * t600 + t645 * t644 + t647 * t435 +
     # t667 * t666 + t669 * t221 + t723 * t722
      t726 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t342, t447, 0.10D1, x4)
      t730 = bbbbH41J2(s, XB1, XB2, z, lh, wd, t342, t447, 0.10D1, x4)
      t743 = -t576 * t726 * t59 / 0.32D2 + (0.90D2 * t5 * (-t575 * t730 
     #+ t588 * t726) + 0.180D3 * t43 * t575 * t726) * t57 * t52 / 0.2880
     #D4
      t744 = FJET(XB1, XB2, s, t330, -t569, 0.0D0, t567, t571, t743)
      t746 = FJET(XB1, XB2, s, t439, 0.0D0, -t442, 0.0D0, 0.0D0, t644)
      t748 = FJET(XB1, XB2, s, t519, 0.0D0, -t524, 0.0D0, 0.0D0, t666)
      t750 = FJET(XB1, XB2, s, t567, 0.0D0, -t569, t330, t571, t743)
      t753 = t330 * t508 * t517
      t754 = t2 * t331
      t757 = Sqrt(t359 * t440 * t23)
      t758 = t31 * t757
      t760 = 0.2D1 * t758 * x2
      t761 = t144 * t332
      t762 = t144 * x1
      t766 = t754 * (t760 + t144 + 0.1D1 - x3 - x2 + t508 + t761 - t762)
     # * t334 * t517
      t770 = t23 * s * t1 * x1 * t517
      t776 = t754 * x2 * (-0.1D1 + t508 + x1 - t357 - t332 + t365 + 0.2D
     #1 * t758) * t334 * t517
      t781 = x2 * t9
      t793 = -t526 + 0.2D1 * t572 - x2 + 0.2D1 * t758 * t573 - t781 - x3
     # - z + t144 + t443 + t761 - 0.2D1 * t758 * t572 - 0.2D1 * t758 * t
     #443 + 0.2D1 * t508 * t332 - t368 * t572 - 0.2D1 * t10 * t443 + t10
     # * t11 * x2 + t332
      t803 = t358 - t10 - t367 - 0.3D1 * t573 - t508 * x1 + t10 * x2 + t
     #572 * t11 + 0.2D1 * t781 * z - t781 * t11 + t760 - t762 + 0.2D1 * 
     #t758 * z - t366 + t369 + t371 - t372 + t525
      t805 = 0.1D1 / (t793 + t803)
      t806 = t5 * t333 * t805
      t807 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t342, t447, -t534, x4)
      t809 = t57 * t52
      t810 = t807 * t50 * t809
      t812 = t806 * t810 / 0.32D2
      t813 = FJET(XB1, XB2, s, t753, -t766, -t770, t776, t571, -t812)
      t815 = t333 * t805
      t819 = FJET(XB1, XB2, s, t776, -t770, -t766, t753, t571, -t812)
      t824 = FJET(XB1, XB2, s, -t442, 0.0D0, t439, 0.0D0, 0.0D0, t505)
      t826 = FJET(XB1, XB2, s, -t336, 0.0D0, 0.0D0, t330, -t341, t722)
      t828 = FJET(XB1, XB2, s, -t569, t330, t567, 0.0D0, t571, t600)
      t830 = FJET(XB1, XB2, s, -t524, 0.0D0, t519, 0.0D0, 0.0D0, t561)
      t832 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t342, t447, -t534, x4)
      t834 = t832 * t50 * t809
      t836 = t806 * t834 / 0.32D2
      t837 = FJET(XB1, XB2, s, -t770, t776, t753, -t766, t571, -t836)
      t842 = FJET(XB1, XB2, s, -t766, t753, t776, -t770, t571, -t836)
      t847 = t744 * t743 + t746 * t644 + t748 * t666 + t750 * t743 - t81
     #3 * t5 * t815 * t810 / 0.32D2 - t819 * t5 * t815 * t810 / 0.32D2 +
     # t824 * t505 + t826 * t722 + t828 * t600 + t830 * t561 - t837 * t5
     # * t815 * t834 / 0.32D2 - t842 * t5 * t815 * t834 / 0.32D2
      bbbbH4n1e0 = t725 + t847

      end function



      doubleprecision function bbbbH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t18 = log(0.4D1 * t13 * t15)
      t19 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t24 = t6 * lh
      t25 = t3 * t19
      t27 = 0.180D3 * t24 * t25
      t29 = 0.1D1 / x1
      t32 = cos(t10)
      t33 = -0.1D1 + x3
      t35 = Sqrt(-x3 * t33)
      t40 = 0.1D1 / (-x3 - z + 0.2D1 * t32 * t35 * z)
      t42 = t25 + t19 * t40
      t44 = 0.1D1 / x3
      t45 = t44 * t29
      t48 = 0.1D1 / x2
      t55 = t44 * t48
      t58 = x2 ** 2
      t59 = t58 * t12
      t62 = log(0.4D1 * t59 * t15)
      t63 = t62 * t3
      t72 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t79 = log(0.4D1 * t12 * t15)
      t80 = t79 * t3
      t83 = (-0.180D3 * t3 * lh - 0.90D2 * t80) * t6
      t88 = lh ** 2
      t90 = 0.3141592653589793D1 ** 2
      t94 = t79 ** 2
      t98 = (0.180D3 * t80 * lh + t3 * (0.180D3 * t88 - 0.30D2 * t90) + 
     #0.45D2 * t94 * t3) * t6
      t102 = x3 * t12
      t107 = log(-0.4D1 * t102 * t15 / t33)
      t111 = log(0.4D1 * t102 * t15)
      t113 = -t107 * t40 - t111 * t3
      t121 = t40 + t3
      t126 = -(0.90D2 * t7 * (t8 - t18 * t19) - t27) * t29 / 0.5760D4 - 
     #t6 * t42 * t45 / 0.64D2 - t7 * t19 * t48 * t29 / 0.32D2 - t6 * t42
     # * t55 / 0.64D2 + (0.90D2 * t6 * (t63 * t19 - t3 * t8) + t27) * t4
     #8 / 0.5760D4 - t7 * t72 / 0.128D3 - t83 * t8 / 0.11520D5 - t98 * t
     #19 / 0.11520D5 - (0.90D2 * t6 * t19 * t113 + (0.90D2 * t6 * t8 - 0
     #.180D3 * t24 * t19) * t121) * t44 / 0.11520D5
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t126)
      t129 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t130 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t135 = t3 * t130
      t137 = 0.180D3 * t24 * t135
      t142 = t135 + t130 * t40
      t162 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t181 = -(0.90D2 * t7 * (t129 - t18 * t130) - t137) * t29 / 0.5760D
     #4 - t6 * t142 * t45 / 0.64D2 - t7 * t130 * t48 * t29 / 0.32D2 - t6
     # * t142 * t55 / 0.64D2 + (0.90D2 * t6 * (t63 * t130 - t3 * t129) +
     # t137) * t48 / 0.5760D4 - t7 * t162 / 0.128D3 - t83 * t129 / 0.115
     #20D5 - t98 * t130 / 0.11520D5 - (0.90D2 * t6 * t130 * t113 + (0.90
     #D2 * t6 * t129 - 0.180D3 * t24 * t130) * t121) * t44 / 0.11520D5
      t182 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t181)
      t184 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t181)
      t186 = t2 * x1
      t187 = -0.1D1 + x1
      t188 = x1 * z
      t189 = 0.1D1 - x1 + t188
      t190 = 0.1D1 / t189
      t192 = t2 * t187 * t190
      t193 = t1 ** 2
      t194 = s * t193
      t196 = x1 * t187 * t190
      t197 = t194 * t196
      t198 = -t187
      t199 = bbbbH42J2(s, XB1, XB2, z, lh, wd, t198, 0.10D1, 0.10D1, x4)
      t201 = t187 ** 2
      t205 = log(0.4D1 * t13 * t15 * t190 * t201)
      t206 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t198, 0.10D1, 0.10D1, x4)
      t211 = t3 * t206
      t218 = x3 * x1
      t220 = x3 * t9
      t223 = Sqrt(-x3 * t189 * t33)
      t235 = -z + 0.2D1 * t218 - t220 - x3 + t188 + 0.2D1 * t32 * t223 *
     # z - 0.3D1 * t218 * z - x1 * t14 + x3 * t14 * x1 + 0.2D1 * t220 * 
     #z - t220 * t14
      t236 = 0.1D1 / t235
      t246 = -(0.90D2 * t7 * (-t199 + t205 * t206) + 0.180D3 * t24 * t21
     #1) * t29 / 0.5760D4 - t6 * (-t189 * t206 * t236 - t211) * t45 / 0.
     #64D2 + t7 * t206 * t48 * t29 / 0.32D2
      t247 = FJET(XB1, XB2, s, 0.0D0, t186, -t192, 0.0D0, -t197, t246)
      t250 = x2 * t1 * s
      t251 = -0.1D1 + x2
      t253 = t251 * t1 * s
      t254 = x2 * z
      t256 = 0.1D1 / (-x2 - z + t254)
      t257 = t6 * t256
      t258 = -t251
      t259 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t258, 0.10D1, x4)
      t264 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t258, 0.10D1, x4)
      t269 = log(-0.4D1 * t59 * t15 * t251)
      t270 = t269 * t256
      t285 = -t257 * t259 * t44 * t48 / 0.64D2 + (0.90D2 * t6 * (-t256 *
     # t264 + t270 * t259) + 0.180D3 * t24 * t256 * t259) * t48 / 0.5760
     #D4 - t257 * t259 * t48 * t29 / 0.32D2
      t286 = FJET(XB1, XB2, s, 0.0D0, t250, 0.0D0, -t253, 0.0D0, t285)
      t288 = x2 * x3
      t289 = t58 * x3
      t292 = Sqrt(x3 * t251 * t33)
      t293 = t32 * t292
      t295 = 0.2D1 * t293 * x2
      t298 = 0.1D1 / (0.1D1 - x3 + t288)
      t300 = t2 * (0.1D1 - x3 - x2 + t288 + t289 + t295) * t298
      t305 = t2 * x2 * (-0.1D1 + t288 + 0.2D1 * t293) * t298
      t313 = 0.1D1 / (x2 + x3 - t289 + z - t254 - t288 * z + t289 * z - 
     #t295 - 0.2D1 * t293 * z + 0.2D1 * t293 * t254)
      t314 = t6 * t313
      t315 = t33 * t298
      t316 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t258, -t315, x4)
      t318 = t316 * t44 * t48
      t320 = t314 * t318 / 0.64D2
      t321 = FJET(XB1, XB2, s, 0.0D0, t300, 0.0D0, -t305, 0.0D0, -t320)
      t327 = t1 * t187
      t329 = t251 * s * t327 * t190
      t331 = x2 * s * t327
      t333 = t194 * t251 * t196
      t334 = x2 * x1
      t337 = 0.1D1 / (x2 - t334 + z - t254 + t334 * z)
      t338 = t6 * t337
      t339 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t198, t258, 0.10D1, x4)
      t341 = t339 * t48 * t29
      t343 = t338 * t341 / 0.32D2
      t344 = FJET(XB1, XB2, s, 0.0D0, t329, t186, -t331, t333, -t343)
      t349 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t258, 0.10D1, x4)
      t358 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, t258, 0.10D1, x4)
      t370 = -t257 * t349 * t48 * t29 / 0.32D2 - t257 * t349 * t44 * t48
     # / 0.64D2 + (0.90D2 * t6 * (-t256 * t358 + t270 * t349) + 0.180D3 
     #* t24 * t256 * t349) * t48 / 0.5760D4
      t371 = FJET(XB1, XB2, s, 0.0D0, -t253, 0.0D0, t250, 0.0D0, t370)
      t373 = FJET(XB1, XB2, s, 0.0D0, -t192, t186, 0.0D0, -t197, t246)
      t375 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t258, -t315, x4)
      t377 = t375 * t44 * t48
      t379 = t314 * t377 / 0.64D2
      t380 = FJET(XB1, XB2, s, 0.0D0, -t305, 0.0D0, t300, 0.0D0, -t379)
      t385 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t126)
      t387 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t198, 0.10D1, 0.10D1, x4)
      t389 = bbbbH41J2(s, XB1, XB2, z, lh, wd, t198, 0.10D1, 0.10D1, x4)
      t393 = t3 * t387
      t409 = -(0.90D2 * t7 * (t205 * t387 - t389) + 0.180D3 * t24 * t393
     #) * t29 / 0.5760D4 - t6 * (-t393 - t189 * t387 * t236) * t45 / 0.6
     #4D2 + t7 * t387 * t48 * t29 / 0.32D2
      t410 = FJET(XB1, XB2, s, t186, 0.0D0, 0.0D0, -t192, -t197, t409)
      t412 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t198, t258, 0.10D1, x4)
      t414 = t412 * t48 * t29
      t416 = t338 * t414 / 0.32D2
      t417 = FJET(XB1, XB2, s, t186, -t331, 0.0D0, t329, t333, -t416)
      t422 = FJET(XB1, XB2, s, t250, 0.0D0, -t253, 0.0D0, 0.0D0, t370)
      t424 = FJET(XB1, XB2, s, t300, 0.0D0, -t305, 0.0D0, 0.0D0, -t379)
      t429 = FJET(XB1, XB2, s, t329, 0.0D0, -t331, t186, t333, -t416)
      t434 = FJET(XB1, XB2, s, -t253, 0.0D0, t250, 0.0D0, 0.0D0, t285)
      t436 = FJET(XB1, XB2, s, -t192, 0.0D0, 0.0D0, t186, -t197, t409)
      t438 = FJET(XB1, XB2, s, -t331, t186, t329, 0.0D0, t333, -t343)
      t443 = FJET(XB1, XB2, s, -t305, 0.0D0, t300, 0.0D0, 0.0D0, -t320)
      bbbbH4n1em1 = t127 * t126 + t182 * t181 + t184 * t181 + t247 * t24
     #6 + t286 * t285 - t321 * t6 * t313 * t318 / 0.64D2 - t344 * t6 * t
     #337 * t341 / 0.32D2 + t371 * t370 + t373 * t246 - t380 * t6 * t313
     # * t377 / 0.64D2 + t385 * t126 + t410 * t409 - t417 * t6 * t337 * 
     #t414 / 0.32D2 + t422 * t370 - t424 * t6 * t313 * t377 / 0.64D2 - t
     #429 * t6 * t337 * t414 / 0.32D2 + t434 * t285 + t436 * t409 - t438
     # * t6 * t337 * t341 / 0.32D2 - t443 * t6 * t313 * t318 / 0.64D2

      end function



      doubleprecision function bbbbH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t6 * t5
      t8 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x2
      t17 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t22 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t22 * t26)
      t33 = (-0.180D3 * t6 * lh - 0.90D2 * t29 * t6) * t5
      t37 = cos(t24)
      t40 = Sqrt(-x3 * (-0.1D1 + x3))
      t48 = (0.1D1 / (-x3 - z + 0.2D1 * t37 * t40 * z) + t6) / x3
      t51 = -t7 * t8 * t9 / 0.64D2 - t7 * t8 * t13 / 0.64D2 - t7 * t17 /
     # 0.128D3 - t33 * t8 / 0.11520D5 - t5 * t8 * t48 / 0.128D3
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t51)
      t54 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t61 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t69 = -t7 * t54 * t9 / 0.64D2 - t7 * t54 * t13 / 0.64D2 - t7 * t61
     # / 0.128D3 - t33 * t54 / 0.11520D5 - t5 * t54 * t48 / 0.128D3
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t69)
      t72 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t69)
      t74 = t2 * x1
      t75 = -0.1D1 + x1
      t78 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t80 = t2 * t75 * t78
      t81 = t1 ** 2
      t85 = s * t81 * x1 * t75 * t78
      t86 = -t75
      t87 = bbbbH42J1(s, XB1, XB2, z, lh, wd, t86, 0.10D1, 0.10D1, x4)
      t90 = t7 * t87 * t9 / 0.64D2
      t91 = FJET(XB1, XB2, s, 0.0D0, t74, -t80, 0.0D0, -t85, t90)
      t94 = t6 * t87 * t9
      t98 = x2 * t1 * s
      t99 = -0.1D1 + x2
      t101 = t99 * t1 * s
      t104 = 0.1D1 / (-x2 - z + x2 * z)
      t105 = t5 * t104
      t106 = -t99
      t107 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t106, 0.10D1, x4)
      t110 = t105 * t107 * t13 / 0.64D2
      t111 = FJET(XB1, XB2, s, 0.0D0, t98, 0.0D0, -t101, 0.0D0, -t110)
      t114 = t104 * t107 * t13
      t117 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, t106, 0.10D1, x4)
      t120 = t105 * t117 * t13 / 0.64D2
      t121 = FJET(XB1, XB2, s, 0.0D0, -t101, 0.0D0, t98, 0.0D0, -t120)
      t124 = t104 * t117 * t13
      t127 = FJET(XB1, XB2, s, 0.0D0, -t80, t74, 0.0D0, -t85, t90)
      t131 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t51)
      t133 = bbbbH41J1(s, XB1, XB2, z, lh, wd, t86, 0.10D1, 0.10D1, x4)
      t136 = t7 * t133 * t9 / 0.64D2
      t137 = FJET(XB1, XB2, s, t74, 0.0D0, 0.0D0, -t80, -t85, t136)
      t140 = t6 * t133 * t9
      t143 = FJET(XB1, XB2, s, t98, 0.0D0, -t101, 0.0D0, 0.0D0, -t120)
      t147 = FJET(XB1, XB2, s, -t80, 0.0D0, 0.0D0, t74, -t85, t136)
      t151 = FJET(XB1, XB2, s, -t101, 0.0D0, t98, 0.0D0, 0.0D0, -t110)
      bbbbH4n1em2 = t52 * t51 + t70 * t69 + t72 * t69 + t91 * t5 * t94 /
     # 0.64D2 - t111 * t5 * t114 / 0.64D2 - t121 * t5 * t124 / 0.64D2 + 
     #t127 * t5 * t94 / 0.64D2 + t131 * t51 + t137 * t5 * t140 / 0.64D2 
     #- t143 * t5 * t124 / 0.64D2 + t147 * t5 * t140 / 0.64D2 - t151 * t
     #5 * t114 / 0.64D2

      end function



      doubleprecision function bbbbH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t6 * t5
      t8 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t10 = t7 * t8 / 0.128D3
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t10)
      t13 = t6 * t8
      t15 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t17 = t7 * t15 / 0.128D3
      t18 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t17)
      t20 = t6 * t15
      t22 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t17)
      t25 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t10)
      bbbbH4n1em3 = -t11 * t5 * t13 / 0.128D3 - t18 * t5 * t20 / 0.128D3
     # - t22 * t5 * t20 / 0.128D3 - t25 * t5 * t13 / 0.128D3

      end function



      doubleprecision function bbbbH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      bbbbH4n1em4 = 0.0D0

      end function


      doubleprecision function bbbbH4n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / z
      t4 = t3 * lh
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t18 = log(0.4D1 * t16)
      t19 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t21 = t18 ** 2
      t22 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t25 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t30 = lh ** 2
      t32 = 0.3141592653589793D1 ** 2
      t34 = 0.180D3 * t30 - 0.30D2 * t32
      t35 = t3 * t34
      t40 = t3 * t7
      t42 = t21 * t18
      t54 = 0.60D2 * lh * t32 - 0.2884936567583026D3 - 0.120D3 * t30 * l
     #h
      t55 = t3 * t54
      t56 = t7 * t22
      t59 = 0.1D1 / x1
      t62 = lh * t7
      t63 = t3 * t19
      t64 = x3 * t8
      t65 = t15 * t11
      t68 = log(0.4D1 * t64 * t65)
      t69 = t68 * t3
      t71 = -0.1D1 + x3
      t72 = 0.1D1 / t71
      t73 = t65 * t72
      t76 = log(-0.4D1 * t64 * t73)
      t79 = cos(t9)
      t80 = x3 * z
      t82 = Sqrt(-t80 * t71)
      t86 = 0.1D1 / (-z - x3 + 0.2D1 * t79 * t82)
      t92 = t76 ** 2
      t97 = t3 * t25
      t99 = t68 ** 2
      t100 = t99 * t3
      t106 = t34 * t7
      t107 = t3 * t22
      t108 = t22 * t86
      t112 = 0.1D1 / x3
      t116 = x2 ** 2
      t117 = x3 * t116
      t118 = t117 * t8
      t121 = log(-0.4D1 * t118 * t73)
      t127 = log(0.4D1 * t117 * t16)
      t128 = t127 * t3
      t130 = -0.1D1 + x2
      t131 = t65 * t130
      t134 = log(-0.4D1 * t118 * t131)
      t135 = t134 * t3
      t136 = -t130
      t137 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t136, 0.10D1, x4)
      t139 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t136, 0.10D1, x4)
      t140 = t3 * t139
      t144 = t3 * t137
      t145 = t107 - t144 + t108
      t150 = 0.1D1 / x2
      t151 = t150 * t59
      t154 = t116 * t8
      t157 = log(0.4D1 * t154 * t65)
      t158 = t157 * t3
      t162 = log(-0.4D1 * t154 * t131)
      t163 = t162 * t3
      t169 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t136, 0.10D1, x4)
      t170 = t3 * t169
      t171 = t157 ** 2
      t172 = t171 * t3
      t175 = t162 ** 2
      t176 = t175 * t3
      t190 = log(0.4D1 * t65)
      t191 = t190 ** 2
      t192 = t191 * t3
      t196 = t191 * t190 * t3
      t199 = t191 ** 2
      t200 = t199 * t3
      t203 = t190 * t3
      t208 = t32 ** 2
      t209 = t30 ** 2
      t213 = 0.5769873135166051D3 * lh + t208 + 0.60D2 * t209 - 0.60D2 *
     # t30 * t32
      t236 = x3 * t15
      t240 = log(-0.4D1 * t236 * t11 * t72)
      t241 = t240 ** 2
      t245 = log(0.4D1 * t236 * t11)
      t246 = t245 ** 2
      t249 = t241 * t86 / 0.2D1 + t246 * t3 / 0.2D1
      t259 = -t245 * t3 - t240 * t86
      t266 = -t241 * t240 * t86 / 0.6D1 - t246 * t245 * t3 / 0.6D1
      t275 = t3 + t86
      t280 = t15 * t116
      t283 = log(0.4D1 * t280 * t11)
      t285 = t283 ** 2
      t288 = t11 * t130
      t291 = log(-0.4D1 * t280 * t288)
      t293 = t291 ** 2
      t308 = t285 * t283
      t314 = t293 * t291
      t328 = log(-0.4D1 * t117 * t73)
      t334 = log(0.4D1 * t117 * t65)
      t335 = t334 * t3
      t339 = log(-0.4D1 * t117 * t131)
      t340 = t339 * t3
      t345 = t334 ** 2
      t346 = t345 * t3
      t350 = t328 ** 2
      t355 = t339 ** 2
      t356 = t355 * t3
      t369 = -(-0.180D3 * t4 * t7 * (-t18 * t19 + t21 * t22 / 0.2D1 + t2
     #5) + t35 * t7 * (t19 - t18 * t22) + 0.90D2 * t40 * (-t18 * t25 - t
     #42 * t22 / 0.6D1 + t21 * t19 / 0.2D1) + t55 * t56) * t59 / 0.5760D
     #4 - (-0.180D3 * t62 * (t63 - t69 * t22 + (t19 - t76 * t22) * t86) 
     #+ 0.90D2 * t7 * ((t25 - t76 * t19 + t92 * t22 / 0.2D1) * t86 + t97
     # - t69 * t19 + t100 * t22 / 0.2D1) + t106 * (t107 + t108)) * t112 
     #* t59 / 0.5760D4 - (0.90D2 * t7 * (t63 + (t19 - t121 * t22) * t86 
     #- t128 * t22 + t135 * t137 - t140) - 0.180D3 * t62 * t145) * t112 
     #* t151 / 0.2880D4 + (-0.180D3 * t62 * (-t63 + t158 * t22 + t140 - 
     #t163 * t137) + 0.90D2 * t7 * (t158 * t19 + t170 - t172 * t22 / 0.2
     #D1 + t176 * t137 / 0.2D1 - t163 * t139 - t97) + t106 * (-t107 + t1
     #44)) * t150 * t59 / 0.2880D4 - (0.45D2 * t192 * t25 - 0.15D2 * t19
     #6 * t19 + 0.15D2 / 0.4D1 * t200 * t22 + (t63 - t203 * t22) * t54 +
     # t107 * t213 + (t97 - t203 * t19 + t192 * t22 / 0.2D1) * t34 - 0.1
     #80D3 * (-t203 * t25 + t192 * t19 / 0.2D1 - t196 * t22 / 0.6D1) * l
     #h) * t7 / 0.11520D5 - ((0.90D2 * t19 - 0.180D3 * t22 * lh) * t7 * 
     #t249 + (0.90D2 * t25 - 0.180D3 * t19 * lh + t22 * t34) * t7 * t259
     # + 0.90D2 * t56 * t266 + (t19 * t34 - 0.180D3 * t25 * lh + t22 * t
     #54) * t7 * t275) * t112 / 0.11520D5 - (-0.180D3 * t4 * t7 * (t25 -
     # t283 * t19 + t285 * t22 / 0.2D1 - t169 + t291 * t139 - t293 * t13
     #7 / 0.2D1) + t35 * t7 * (t19 + t291 * t137 - t139 - t283 * t22) + 
     #0.90D2 * t40 * (-t283 * t25 + t285 * t19 / 0.2D1 - t308 * t22 / 0.
     #6D1 + t291 * t169 - t293 * t139 / 0.2D1 + t314 * t137 / 0.6D1) + t
     #55 * t7 * (-t137 + t22)) * t150 / 0.5760D4 - (-0.180D3 * t62 * ((t
     #19 - t328 * t22) * t86 - t335 * t22 + t63 - t140 + t340 * t137) + 
     #0.90D2 * t7 * (t346 * t22 / 0.2D1 - t170 + (t25 - t328 * t19 + t35
     #0 * t22 / 0.2D1) * t86 - t356 * t137 / 0.2D1 + t97 + t340 * t139 -
     # t335 * t19) + t106 * t145) * t112 * t150 / 0.5760D4
      t370 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t369)
      t372 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t374 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t375 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t394 = t7 * t375
      t399 = t3 * t372
      t412 = t3 * t374
      t419 = t3 * t375
      t420 = t375 * t86
      t430 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t136, 0.10D1, x4)
      t433 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t136, 0.10D1, x4)
      t434 = t3 * t433
      t438 = t3 * t430
      t439 = -t438 + t419 + t420
      t451 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, t136, 0.10D1, x4)
      t452 = t3 * t451
      t580 = -(-0.180D3 * t4 * t7 * (-t18 * t372 + t374 + t21 * t375 / 0
     #.2D1) + t35 * t7 * (t372 - t18 * t375) + 0.90D2 * t40 * (-t18 * t3
     #74 - t42 * t375 / 0.6D1 + t21 * t372 / 0.2D1) + t55 * t394) * t59 
     #/ 0.5760D4 - (-0.180D3 * t62 * (t399 - t69 * t375 + (t372 - t76 * 
     #t375) * t86) + 0.90D2 * t7 * ((t374 - t76 * t372 + t92 * t375 / 0.
     #2D1) * t86 + t412 - t69 * t372 + t100 * t375 / 0.2D1) + t106 * (t4
     #19 + t420)) * t112 * t59 / 0.5760D4 - (0.90D2 * t7 * ((t372 - t121
     # * t375) * t86 + t399 + t135 * t430 - t128 * t375 - t434) - 0.180D
     #3 * t62 * t439) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (-t39
     #9 + t158 * t375 + t434 - t163 * t430) + 0.90D2 * t7 * (t452 - t172
     # * t375 / 0.2D1 - t163 * t433 + t158 * t372 + t176 * t430 / 0.2D1 
     #- t412) + t106 * (-t419 + t438)) * t150 * t59 / 0.2880D4 - (0.45D2
     # * t192 * t374 - 0.15D2 * t196 * t372 + 0.15D2 / 0.4D1 * t200 * t3
     #75 + (t399 - t203 * t375) * t54 + t419 * t213 + (t412 - t203 * t37
     #2 + t192 * t375 / 0.2D1) * t34 - 0.180D3 * (-t203 * t374 + t192 * 
     #t372 / 0.2D1 - t196 * t375 / 0.6D1) * lh) * t7 / 0.11520D5 - ((-0.
     #180D3 * t375 * lh + 0.90D2 * t372) * t7 * t249 + (-0.180D3 * t372 
     #* lh + t375 * t34 + 0.90D2 * t374) * t7 * t259 + 0.90D2 * t394 * t
     #266 + (t372 * t34 - 0.180D3 * t374 * lh + t375 * t54) * t7 * t275)
     # * t112 / 0.11520D5 - (-0.180D3 * t4 * t7 * (-t283 * t372 + t291 *
     # t433 + t285 * t375 / 0.2D1 - t451 + t374 - t293 * t430 / 0.2D1) +
     # t35 * t7 * (-t433 + t291 * t430 - t283 * t375 + t372) + 0.90D2 * 
     #t40 * (-t283 * t374 + t285 * t372 / 0.2D1 - t308 * t375 / 0.6D1 + 
     #t291 * t451 - t293 * t433 / 0.2D1 + t314 * t430 / 0.6D1) + t55 * t
     #7 * (-t430 + t375)) * t150 / 0.5760D4 - (-0.180D3 * t62 * ((t372 -
     # t328 * t375) * t86 + t399 - t434 - t335 * t375 + t340 * t430) + 0
     #.90D2 * t7 * (-t335 * t372 + t412 - t452 + (t374 - t328 * t372 + t
     #350 * t375 / 0.2D1) * t86 + t340 * t433 + t346 * t375 / 0.2D1 - t3
     #56 * t430 / 0.2D1) + t106 * t439) * t112 * t150 / 0.5760D4
      t581 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t580)
      t583 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t580)
      t586 = x1 * z
      t587 = -z - x1 + t586
      t588 = 0.1D1 / t587
      t590 = t2 * x1 * t130 * t588
      t591 = -0.1D1 + x1
      t592 = t2 * t591
      t595 = x2 * s * t1 * x1
      t596 = t1 ** 2
      t597 = s * t596
      t600 = x1 * t591 * t588
      t601 = t597 * t130 * t600
      t602 = t117 * t12
      t603 = 0.1D1 / t13
      t604 = t603 * t588
      t605 = t591 ** 2
      t607 = t604 * t605 * t130
      t610 = log(0.4D1 * t602 * t607)
      t611 = x2 * x1
      t612 = t611 * z
      t614 = 0.1D1 / (t612 - z - t611)
      t615 = t610 * t614
      t616 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, t136, 0.10D1, x4)
      t618 = bbbbH42J2(s, XB1, XB2, z, lh, wd, x1, t136, 0.10D1, x4)
      t619 = t614 * t618
      t623 = t614 * t616
      t629 = t154 * t11
      t632 = log(0.4D1 * t629 * t607)
      t633 = t632 * t614
      t638 = bbbbH42J3(s, XB1, XB2, z, lh, wd, x1, t136, 0.10D1, x4)
      t640 = t632 ** 2
      t641 = t640 * t614
      t653 = -(0.90D2 * t7 * (t615 * t616 - t619) + 0.180D3 * t62 * t623
     #) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (t619 - t633 * t616
     #) + 0.90D2 * t7 * (t614 * t638 + t641 * t616 / 0.2D1 - t633 * t618
     #) + t106 * t623) * t150 * t59 / 0.2880D4
      t654 = FJET(XB1, XB2, s, 0.0D0, t590, -t592, t595, -t601, t653)
      t656 = x2 * x3
      t657 = 0.1D1 - x3 + t656
      t658 = 0.1D1 / t657
      t659 = t656 * t658
      t660 = t2 * t659
      t663 = t71 * t1 * s * t658
      t665 = t657 ** 2
      t666 = 0.1D1 / t665
      t667 = t71 * t666
      t671 = log(0.4D1 * t602 * t15 * t130 * t667)
      t672 = t130 * t71
      t674 = Sqrt(t80 * t672)
      t678 = 0.1D1 / (-z - x3 + t656 + 0.2D1 * t79 * t674)
      t679 = t671 * t678
      t680 = t71 * t658
      t681 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t136, -t680, x4)
      t683 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t136, -t680, x4)
      t684 = t678 * t683
      t688 = t678 * t681
      t699 = log(0.4D1 * t117 * t15 * t288 * t667)
      t700 = t699 * t678
      t706 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t136, -t680, x4)
      t708 = t699 ** 2
      t709 = t708 * t678
      t720 = -(0.90D2 * t7 * (t679 * t681 - t684) + 0.180D3 * t62 * t688
     #) * t112 * t151 / 0.2880D4 - (-0.180D3 * t62 * (-t684 + t700 * t68
     #1) + 0.90D2 * t7 * (t700 * t683 - t678 * t706 - t709 * t681 / 0.2D
     #1) - t106 * t688) * t112 * t150 / 0.5760D4
      t721 = FJET(XB1, XB2, s, 0.0D0, t660, 0.0D0, -t663, 0.0D0, t720)
      t724 = t2 * x1 * t588
      t725 = t597 * t600
      t726 = t604 * t605
      t729 = log(-0.4D1 * t12 * t726)
      t730 = t729 ** 2
      t731 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t734 = bbbbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t736 = bbbbH42J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t748 = t730 * t729
      t759 = t3 * t734
      t760 = t587 * t734
      t761 = t64 * t11
      t763 = t604 * t605 * t72
      t766 = log(0.4D1 * t761 * t763)
      t767 = t766 * t587
      t770 = x3 * x1
      t771 = t770 * z
      t772 = x1 * t13
      t773 = x3 * t13
      t774 = t773 * x1
      t776 = 0.2D1 * t64 * z
      t777 = t64 * t13
      t778 = x3 * t587
      t780 = Sqrt(t778 * t71)
      t785 = 0.1D1 / (-t586 - t771 - t64 + t772 + t774 - t80 + t776 - t7
     #77 - t13 + 0.2D1 * t79 * t780 * z)
      t789 = log(-0.4D1 * t761 * t726)
      t790 = t789 * t3
      t797 = t766 ** 2
      t798 = t797 * t587
      t803 = t3 * t736
      t805 = t789 ** 2
      t806 = t805 * t3
      t812 = t3 * t731
      t815 = -t812 + t587 * t731 * t785
      t823 = log(0.4D1 * t602 * t763)
      t824 = t823 * t587
      t829 = t588 * t605
      t833 = log(-0.4D1 * t118 * t11 * t603 * t829)
      t834 = t833 * t3
      t847 = log(-0.4D1 * t629 * t726)
      t848 = t847 * t3
      t854 = t847 ** 2
      t855 = t854 * t3
      t866 = -(-0.180D3 * t4 * t7 * (-t730 * t731 / 0.2D1 + t729 * t734 
     #- t736) + t35 * t7 * (-t734 + t729 * t731) + 0.90D2 * t40 * (t729 
     #* t736 - t730 * t734 / 0.2D1 + t748 * t731 / 0.6D1) - t55 * t7 * t
     #731) * t59 / 0.5760D4 - (-0.180D3 * t62 * (-t759 + (t760 - t767 * 
     #t731) * t785 + t790 * t731) + 0.90D2 * t7 * ((t587 * t736 - t767 *
     # t734 + t798 * t731 / 0.2D1) * t785 - t803 + t790 * t734 - t806 * 
     #t731 / 0.2D1) + t106 * t815) * t112 * t59 / 0.5760D4 - (0.90D2 * t
     #7 * (-t759 + (t760 - t824 * t731) * t785 + t834 * t731) - 0.180D3 
     #* t62 * t815) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (-t848 
     #* t731 + t759) + 0.90D2 * t7 * (-t848 * t734 + t855 * t731 / 0.2D1
     # + t803) + t106 * t812) * t150 * t59 / 0.2880D4
      t867 = FJET(XB1, XB2, s, 0.0D0, -t592, -t724, 0.0D0, t725, t866)
      t869 = FJET(XB1, XB2, s, 0.0D0, -t724, -t592, 0.0D0, t725, t866)
      t871 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t136, -t680, x4)
      t873 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t136, -t680, x4)
      t874 = t678 * t873
      t878 = t678 * t871
      t892 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, t136, -t680, x4)
      t902 = -(0.90D2 * t7 * (t679 * t871 - t874) + 0.180D3 * t62 * t878
     #) * t112 * t151 / 0.2880D4 - (-0.180D3 * t62 * (t700 * t871 - t874
     #) + 0.90D2 * t7 * (-t709 * t871 / 0.2D1 + t700 * t873 - t678 * t89
     #2) - t106 * t878) * t112 * t150 / 0.5760D4
      t903 = FJET(XB1, XB2, s, 0.0D0, -t663, 0.0D0, t660, 0.0D0, t902)
      t905 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t369)
      t907 = FJET(XB1, XB2, s, t595, -t592, t590, 0.0D0, -t601, t653)
      t909 = bbbbH41J2(s, XB1, XB2, z, lh, wd, x1, t136, 0.10D1, x4)
      t910 = t614 * t909
      t911 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, t136, 0.10D1, x4)
      t916 = t614 * t911
      t927 = bbbbH41J3(s, XB1, XB2, z, lh, wd, x1, t136, 0.10D1, x4)
      t939 = -(0.90D2 * t7 * (-t910 + t615 * t911) + 0.180D3 * t62 * t91
     #6) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (t910 - t633 * t91
     #1) + 0.90D2 * t7 * (-t633 * t909 + t614 * t927 + t641 * t911 / 0.2
     #D1) + t106 * t916) * t150 * t59 / 0.2880D4
      t940 = FJET(XB1, XB2, s, t590, 0.0D0, t595, -t592, -t601, t939)
      t942 = FJET(XB1, XB2, s, t660, 0.0D0, -t663, 0.0D0, 0.0D0, t902)
      t947 = t71 * s * t1 * t591 * t658
      t948 = t2 * x1
      t950 = Sqrt(-t778 * t672)
      t951 = t79 * t950
      t957 = t948 * x2 * (-x3 + t656 - z + t80 - x1 + t770 + t586 - t771
     # + 0.2D1 * t951) * t588 * t658
      t958 = t592 * t659
      t961 = t117 * t586
      t962 = t117 * x1
      t967 = t948 * (0.1D1 - x3 - x2 + t656 + 0.2D1 * t951 * x2 - t961 +
     # t962 + t117 * z) * t588 * t658
      t974 = log(-0.4D1 * t117 * t12 * t603 * t829 * t672 * t666)
      t975 = t974 * t587
      t982 = x2 * t8
      t986 = -t962 - 0.2D1 * t951 * z - t656 * z + t656 * x1 - t64 * x2 
     #- t611 * t13 - 0.2D1 * t982 * z + t982 * t13 + t586 + t13 + t64 - 
     #t772 + t80
      t999 = t961 - 0.2D1 * t656 * t586 + t773 * t611 + 0.2D1 * t64 * x2
     # * z - t64 * t13 * x2 - 0.2D1 * t951 * t611 + t771 - t774 - t776 +
     # t777 + t612 + 0.2D1 * t951 * t612 + t982
      t1001 = 0.1D1 / (t986 + t999)
      t1002 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, t136, -t680, x4)
      t1005 = t587 * t1001
      t1006 = bbbbH41J2(s, XB1, XB2, z, lh, wd, x1, t136, -t680, x4)
      t1014 = 0.90D2 * t7 * (-t975 * t1001 * t1002 + t1005 * t1006) - 0.
     #180D3 * t62 * t1005 * t1002
      t1017 = t1014 * t112 * t151 / 0.2880D4
      t1018 = FJET(XB1, XB2, s, t947, t957, -t958, -t967, -t601, -t1017)
      t1021 = t112 * t150 * t59
      t1024 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, t136, -t680, x4)
      t1027 = bbbbH42J2(s, XB1, XB2, z, lh, wd, x1, t136, -t680, x4)
      t1035 = 0.90D2 * t7 * (-t975 * t1001 * t1024 + t1005 * t1027) - 0.
     #180D3 * t62 * t1005 * t1024
      t1038 = t1035 * t112 * t151 / 0.2880D4
      t1039 = FJET(XB1, XB2, s, t957, t947, -t967, -t958, -t601, -t1038)
      t1043 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1046 = bbbbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1048 = bbbbH41J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1070 = t3 * t1046
      t1071 = t587 * t1046
      t1086 = t3 * t1048
      t1094 = t3 * t1043
      t1095 = t587 * t1043 * t785 - t1094
      t1129 = -(-0.180D3 * t4 * t7 * (-t730 * t1043 / 0.2D1 + t729 * t10
     #46 - t1048) + t35 * t7 * (t729 * t1043 - t1046) + 0.90D2 * t40 * (
     #t748 * t1043 / 0.6D1 - t730 * t1046 / 0.2D1 + t729 * t1048) - t55 
     #* t7 * t1043) * t59 / 0.5760D4 - (-0.180D3 * t62 * (-t1070 + (t107
     #1 - t767 * t1043) * t785 + t790 * t1043) + 0.90D2 * t7 * ((t587 * 
     #t1048 - t767 * t1046 + t798 * t1043 / 0.2D1) * t785 + t790 * t1046
     # - t1086 - t806 * t1043 / 0.2D1) + t106 * t1095) * t112 * t59 / 0.
     #5760D4 - (0.90D2 * t7 * ((t1071 - t824 * t1043) * t785 + t834 * t1
     #043 - t1070) - 0.180D3 * t62 * t1095) * t112 * t151 / 0.2880D4 + (
     #-0.180D3 * t62 * (t1070 - t848 * t1043) + 0.90D2 * t7 * (t1086 + t
     #855 * t1043 / 0.2D1 - t848 * t1046) + t106 * t1094) * t150 * t59 /
     # 0.2880D4
      t1130 = FJET(XB1, XB2, s, -t592, 0.0D0, 0.0D0, -t724, t725, t1129)
      t1132 = FJET(XB1, XB2, s, -t592, t595, 0.0D0, t590, -t601, t939)
      t1134 = FJET(XB1, XB2, s, -t724, 0.0D0, 0.0D0, -t592, t725, t1129)
      t1136 = FJET(XB1, XB2, s, -t663, 0.0D0, t660, 0.0D0, 0.0D0, t720)
      t1138 = FJET(XB1, XB2, s, -t967, -t958, t957, t947, -t601, -t1017)
      t1142 = FJET(XB1, XB2, s, -t958, -t967, t947, t957, -t601, -t1038)
      bbbbH4n2e1 = t370 * t369 + t581 * t580 + t583 * t580 + t654 * t653
     # + t721 * t720 + t867 * t866 + t869 * t866 + t903 * t902 + t905 * 
     #t369 + t907 * t653 + t940 * t939 + t942 * t902 - t1018 * t1014 * t
     #1021 / 0.2880D4 - t1039 * t1035 * t1021 / 0.2880D4 + t1130 * t1129
     # + t1132 * t939 + t1134 * t1129 + t1136 * t720 - t1138 * t1014 * t
     #1021 / 0.2880D4 - t1142 * t1035 * t1021 / 0.2880D4

      end function



      doubleprecision function bbbbH4n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t8 = t6 * t7
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t20 = log(0.4D1 * t10 * t17)
      t21 = t20 * t6
      t22 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t24 = -0.1D1 + x3
      t25 = 0.1D1 / t24
      t26 = t17 * t25
      t29 = log(-0.4D1 * t10 * t26)
      t32 = cos(t14)
      t33 = x3 * z
      t35 = Sqrt(-t33 * t24)
      t39 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t35)
      t44 = lh * t5
      t45 = t6 * t22
      t46 = t22 * t39
      t51 = 0.1D1 / x3
      t53 = 0.1D1 / x1
      t56 = 0.1D1 - x2
      t57 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t58 = t6 * t57
      t59 = t45 - t58 + t46
      t61 = 0.1D1 / x2
      t63 = t51 * t61 * t53
      t66 = x2 ** 2
      t67 = t66 * t9
      t70 = log(0.4D1 * t67 * t17)
      t71 = t70 * t6
      t73 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t74 = t6 * t73
      t75 = -t56
      t76 = t17 * t75
      t79 = log(-0.4D1 * t67 * t76)
      t80 = t79 * t6
      t92 = t6 * lh
      t93 = t9 * t16
      t96 = log(0.4D1 * t93 * t13)
      t102 = t6 * t5
      t104 = t96 ** 2
      t107 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t111 = lh ** 2
      t113 = 0.3141592653589793D1 ** 2
      t115 = 0.180D3 * t111 - 0.30D2 * t113
      t116 = t6 * t115
      t117 = t5 * t22
      t127 = x3 * t13
      t130 = log(0.4D1 * t127 * t16)
      t135 = log(-0.4D1 * t127 * t16 * t25)
      t137 = -t130 * t6 - t135 * t39
      t139 = t135 ** 2
      t141 = t130 ** 2
      t144 = t139 * t39 / 0.2D1 + t141 * t6 / 0.2D1
      t153 = t6 + t39
      t158 = x3 * t66
      t161 = log(-0.4D1 * t158 * t26)
      t167 = log(0.4D1 * t158 * t17)
      t168 = t167 * t6
      t172 = log(-0.4D1 * t158 * t76)
      t173 = t172 * t6
      t184 = t13 * t66
      t185 = t16 * t75
      t188 = log(-0.4D1 * t184 * t185)
      t192 = log(0.4D1 * t184 * t16)
      t199 = t192 ** 2
      t202 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t204 = t188 ** 2
      t217 = log(0.4D1 * t17)
      t218 = t217 * t6
      t224 = t217 ** 2
      t225 = t224 * t6
      t235 = 0.60D2 * lh * t113 - 0.2884936567583026D3 - 0.120D3 * t111 
     #* lh
      t242 = t224 * t217 * t6
      t248 = -(0.90D2 * t5 * (t8 - t21 * t22 + (t7 - t29 * t22) * t39) -
     # 0.180D3 * t44 * (t45 + t46)) * t51 * t53 / 0.5760D4 - t5 * t59 * 
     #t63 / 0.32D2 + (0.90D2 * t5 * (-t8 + t71 * t22 + t74 - t80 * t57) 
     #- 0.180D3 * t44 * (-t45 + t58)) * t61 * t53 / 0.2880D4 - (-0.180D3
     # * t92 * t5 * (t7 - t96 * t22) + 0.90D2 * t102 * (-t96 * t7 + t104
     # * t22 / 0.2D1 + t107) + t116 * t117) * t53 / 0.5760D4 - ((0.90D2 
     #* t7 - 0.180D3 * t22 * lh) * t5 * t137 + 0.90D2 * t117 * t144 + (0
     #.90D2 * t107 - 0.180D3 * t7 * lh + t22 * t115) * t5 * t153) * t51 
     #/ 0.11520D5 - (0.90D2 * t5 * ((t7 - t161 * t22) * t39 - t168 * t22
     # + t8 - t74 + t173 * t57) - 0.180D3 * t44 * t59) * t51 * t61 / 0.5
     #760D4 - (-0.180D3 * t92 * t5 * (t7 + t188 * t57 - t73 - t192 * t22
     #) + 0.90D2 * t102 * (t107 - t192 * t7 + t199 * t22 / 0.2D1 - t202 
     #+ t188 * t73 - t204 * t57 / 0.2D1) + t116 * t5 * (-t57 + t22)) * t
     #61 / 0.5760D4 - ((t8 - t218 * t22) * t115 - 0.180D3 * (t6 * t107 -
     # t218 * t7 + t225 * t22 / 0.2D1) * lh + t45 * t235 - 0.90D2 * t218
     # * t107 + 0.45D2 * t225 * t7 - 0.15D2 * t242 * t22) * t5 / 0.11520
     #D5
      t249 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t248)
      t251 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t252 = t6 * t251
      t253 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t261 = t6 * t253
      t262 = t253 * t39
      t270 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t271 = t6 * t270
      t272 = -t271 + t261 + t262
      t277 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t278 = t6 * t277
      t296 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t302 = t5 * t253
      t349 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4)
      t381 = -(0.90D2 * t5 * (t252 - t21 * t253 + (t251 - t29 * t253) * 
     #t39) - 0.180D3 * t44 * (t261 + t262)) * t51 * t53 / 0.5760D4 - t5 
     #* t272 * t63 / 0.32D2 + (0.90D2 * t5 * (-t252 + t71 * t253 + t278 
     #- t80 * t270) - 0.180D3 * t44 * (-t261 + t271)) * t61 * t53 / 0.28
     #80D4 - (-0.180D3 * t92 * t5 * (t251 - t96 * t253) + 0.90D2 * t102 
     #* (-t96 * t251 + t296 + t104 * t253 / 0.2D1) + t116 * t302) * t53 
     #/ 0.5760D4 - ((-0.180D3 * t253 * lh + 0.90D2 * t251) * t5 * t137 +
     # 0.90D2 * t302 * t144 + (-0.180D3 * t251 * lh + t253 * t115 + 0.90
     #D2 * t296) * t5 * t153) * t51 / 0.11520D5 - (0.90D2 * t5 * ((t251 
     #- t161 * t253) * t39 + t252 - t278 - t168 * t253 + t173 * t270) - 
     #0.180D3 * t44 * t272) * t51 * t61 / 0.5760D4 - (-0.180D3 * t92 * t
     #5 * (-t277 + t188 * t270 - t192 * t253 + t251) + 0.90D2 * t102 * (
     #-t192 * t251 + t188 * t277 + t199 * t253 / 0.2D1 - t349 + t296 - t
     #204 * t270 / 0.2D1) + t116 * t5 * (-t270 + t253)) * t61 / 0.5760D4
     # - ((t252 - t218 * t253) * t115 - 0.180D3 * (t6 * t296 - t218 * t2
     #51 + t225 * t253 / 0.2D1) * lh + t261 * t235 - 0.90D2 * t218 * t29
     #6 + 0.45D2 * t225 * t251 - 0.15D2 * t242 * t253) * t5 / 0.11520D5
      t382 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t381)
      t384 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t381)
      t387 = x1 * z
      t388 = -z - x1 + t387
      t389 = 0.1D1 / t388
      t391 = t2 * x1 * t75 * t389
      t392 = -0.1D1 + x1
      t393 = t2 * t392
      t396 = x2 * s * t1 * x1
      t397 = t1 ** 2
      t398 = s * t397
      t401 = x1 * t392 * t389
      t402 = t398 * t75 * t401
      t403 = x2 * x1
      t404 = t403 * z
      t406 = 0.1D1 / (t404 - z - t403)
      t407 = t5 * t406
      t408 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t412 = bbbbH42J2(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t414 = t67 * t16
      t416 = 0.1D1 / t11 * t389
      t417 = t392 ** 2
      t422 = log(0.4D1 * t414 * t416 * t417 * t75)
      t423 = t422 * t406
      t435 = t407 * t408 * t63 / 0.32D2 + (0.90D2 * t5 * (t406 * t412 - 
     #t423 * t408) - 0.180D3 * t44 * t406 * t408) * t61 * t53 / 0.2880D4
      t436 = FJET(XB1, XB2, s, 0.0D0, t391, -t393, t396, -t402, t435)
      t438 = x2 * x3
      t439 = 0.1D1 - x3 + t438
      t440 = 0.1D1 / t439
      t441 = t438 * t440
      t442 = t2 * t441
      t445 = t24 * t1 * s * t440
      t446 = t75 * t24
      t448 = Sqrt(t33 * t446)
      t452 = 0.1D1 / (-z - x3 + t438 + 0.2D1 * t32 * t448)
      t453 = t24 * t440
      t454 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t453, x4)
      t457 = t439 ** 2
      t463 = log(0.4D1 * t158 * t13 * t185 * t24 / t457)
      t464 = t463 * t452
      t465 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t453, x4)
      t477 = t5 * t452
      t481 = -(0.90D2 * t5 * (-t452 * t454 + t464 * t465) + 0.180D3 * t4
     #4 * t452 * t465) * t51 * t61 / 0.5760D4 + t477 * t465 * t63 / 0.32
     #D2
      t482 = FJET(XB1, XB2, s, 0.0D0, t442, 0.0D0, -t445, 0.0D0, t481)
      t485 = t2 * x1 * t389
      t486 = t398 * t401
      t487 = bbbbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t488 = t6 * t487
      t490 = t10 * t16
      t495 = log(0.4D1 * t490 * t416 * t417 * t25)
      t496 = t495 * t388
      t497 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t500 = x3 * x1
      t501 = t500 * z
      t502 = x1 * t11
      t503 = x3 * t11
      t504 = t503 * x1
      t506 = 0.2D1 * t10 * z
      t507 = t10 * t11
      t508 = x3 * t388
      t510 = Sqrt(t508 * t24)
      t515 = 0.1D1 / (-t387 - t501 - t10 + t502 + t504 - t33 + t506 - t5
     #07 - t11 + 0.2D1 * t32 * t510 * z)
      t517 = t416 * t417
      t520 = log(-0.4D1 * t490 * t517)
      t521 = t520 * t6
      t526 = t6 * t497
      t529 = -t526 + t388 * t497 * t515
      t541 = log(-0.4D1 * t414 * t517)
      t542 = t541 * t6
      t555 = log(-0.4D1 * t93 * t517)
      t561 = t555 ** 2
      t565 = bbbbH42J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t569 = t115 * t5
      t574 = -(0.90D2 * t5 * (-t488 + (t388 * t487 - t496 * t497) * t515
     # + t521 * t497) - 0.180D3 * t44 * t529) * t51 * t53 / 0.5760D4 - t
     #5 * t529 * t63 / 0.32D2 + (0.90D2 * t5 * (-t542 * t497 + t488) - 0
     #.180D3 * t44 * t526) * t61 * t53 / 0.2880D4 - (-0.180D3 * t92 * t5
     # * (-t487 + t555 * t497) + 0.90D2 * t102 * (-t561 * t497 / 0.2D1 +
     # t555 * t487 - t565) - t569 * t526) * t53 / 0.5760D4
      t575 = FJET(XB1, XB2, s, 0.0D0, -t393, -t485, 0.0D0, t486, t574)
      t577 = FJET(XB1, XB2, s, 0.0D0, -t485, -t393, 0.0D0, t486, t574)
      t579 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t453, x4)
      t584 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t453, x4)
      t596 = t477 * t579 * t63 / 0.32D2 - (0.90D2 * t5 * (t464 * t579 - 
     #t452 * t584) + 0.180D3 * t44 * t452 * t579) * t51 * t61 / 0.5760D4
      t597 = FJET(XB1, XB2, s, 0.0D0, -t445, 0.0D0, t442, 0.0D0, t596)
      t599 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t248)
      t601 = FJET(XB1, XB2, s, t396, -t393, t391, 0.0D0, -t402, t435)
      t603 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t607 = bbbbH41J2(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t620 = t407 * t603 * t63 / 0.32D2 + (0.90D2 * t5 * (t406 * t607 - 
     #t423 * t603) - 0.180D3 * t44 * t406 * t603) * t61 * t53 / 0.2880D4
      t621 = FJET(XB1, XB2, s, t391, 0.0D0, t396, -t393, -t402, t620)
      t623 = FJET(XB1, XB2, s, t442, 0.0D0, -t445, 0.0D0, 0.0D0, t596)
      t628 = t24 * s * t1 * t392 * t440
      t629 = t2 * x1
      t631 = Sqrt(-t508 * t446)
      t632 = t32 * t631
      t638 = t629 * x2 * (-x3 + t438 - z + t33 - x1 + t500 + t387 - t501
     # + 0.2D1 * t632) * t389 * t440
      t639 = t393 * t441
      t642 = t158 * t387
      t643 = t158 * x1
      t648 = t629 * (0.1D1 - x3 - x2 + t438 + 0.2D1 * t632 * x2 - t642 +
     # t643 + t158 * z) * t389 * t440
      t662 = x2 * t9
      t663 = t11 + t642 + 0.2D1 * t10 * x2 * z + t503 * t403 - 0.2D1 * t
     #438 * t387 - 0.2D1 * t632 * t403 - t10 * t11 * x2 + 0.2D1 * t632 *
     # t404 + t404 - t502 + t33 + t10 + t662
      t673 = -t438 * z + t438 * x1 - t10 * x2 - t403 * t11 - 0.2D1 * t66
     #2 * z + t662 * t11 - t643 - 0.2D1 * t632 * z + t387 + t507 - t506 
     #- t504 + t501
      t675 = 0.1D1 / (t663 + t673)
      t676 = t5 * t388 * t675
      t677 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, t56, -t453, x4)
      t679 = t61 * t53
      t680 = t677 * t51 * t679
      t682 = t676 * t680 / 0.32D2
      t683 = FJET(XB1, XB2, s, t628, t638, -t639, -t648, -t402, -t682)
      t685 = t388 * t675
      t689 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, t56, -t453, x4)
      t691 = t689 * t51 * t679
      t693 = t676 * t691 / 0.32D2
      t694 = FJET(XB1, XB2, s, t638, t628, -t648, -t639, -t402, -t693)
      t699 = bbbbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t700 = t6 * t699
      t702 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t712 = t6 * t702
      t713 = t388 * t702 * t515 - t712
      t741 = bbbbH41J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t749 = -(0.90D2 * t5 * (-t700 + (t388 * t699 - t496 * t702) * t515
     # + t521 * t702) - 0.180D3 * t44 * t713) * t51 * t53 / 0.5760D4 - t
     #5 * t713 * t63 / 0.32D2 + (0.90D2 * t5 * (t700 - t542 * t702) - 0.
     #180D3 * t44 * t712) * t61 * t53 / 0.2880D4 - (-0.180D3 * t92 * t5 
     #* (t555 * t702 - t699) + 0.90D2 * t102 * (-t561 * t702 / 0.2D1 + t
     #555 * t699 - t741) - t569 * t712) * t53 / 0.5760D4
      t750 = FJET(XB1, XB2, s, -t393, 0.0D0, 0.0D0, -t485, t486, t749)
      t752 = FJET(XB1, XB2, s, -t393, t396, 0.0D0, t391, -t402, t620)
      t754 = FJET(XB1, XB2, s, -t485, 0.0D0, 0.0D0, -t393, t486, t749)
      t756 = FJET(XB1, XB2, s, -t445, 0.0D0, t442, 0.0D0, 0.0D0, t481)
      t758 = FJET(XB1, XB2, s, -t648, -t639, t638, t628, -t402, -t682)
      t763 = FJET(XB1, XB2, s, -t639, -t648, t628, t638, -t402, -t693)
      bbbbH4n2e0 = t249 * t248 + t382 * t381 + t384 * t381 + t436 * t435
     # + t482 * t481 + t575 * t574 + t577 * t574 + t597 * t596 + t599 * 
     #t248 + t601 * t435 + t621 * t620 + t623 * t596 - t683 * t5 * t685 
     #* t680 / 0.32D2 - t694 * t5 * t685 * t691 / 0.32D2 + t750 * t749 +
     # t752 * t620 + t754 * t749 + t756 * t481 - t758 * t5 * t685 * t680
     # / 0.32D2 - t763 * t5 * t685 * t691 / 0.32D2

      end function



      doubleprecision function bbbbH4n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t19 = log(0.4D1 * t13 * t16)
      t20 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t25 = t3 * lh
      t26 = t20 * t6
      t30 = 0.1D1 / x1
      t33 = t3 * t20
      t34 = cos(t10)
      t35 = x3 * z
      t36 = -0.1D1 + x3
      t38 = Sqrt(-t35 * t36)
      t42 = 0.1D1 / (-z - x3 + 0.2D1 * t34 * t38)
      t43 = t20 * t42
      t46 = 0.1D1 / x3
      t47 = t46 * t30
      t50 = 0.1D1 - x2
      t51 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x4)
      t52 = t3 * t51
      t55 = 0.1D1 / x2
      t56 = t55 * t30
      t61 = t46 * t55
      t64 = x2 ** 2
      t65 = t16 * t64
      t66 = -t50
      t70 = log(-0.4D1 * t65 * t12 * t66)
      t72 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x4)
      t75 = log(0.4D1 * t65 * t12)
      t90 = log(0.4D1 * t16 * t12)
      t91 = t90 * t3
      t96 = lh ** 2
      t98 = 0.3141592653589793D1 ** 2
      t100 = 0.180D3 * t96 - 0.30D2 * t98
      t102 = bbbbH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t107 = t90 ** 2
      t108 = t107 * t3
      t114 = x3 * t16
      t117 = log(0.4D1 * t114 * t12)
      t123 = log(-0.4D1 * t114 * t12 / t36)
      t125 = -t117 * t3 - t123 * t42
      t133 = t3 + t42
      t138 = -(0.90D2 * t7 * (t8 - t19 * t20) - 0.180D3 * t25 * t26) * t
     #30 / 0.5760D4 - t6 * (t33 + t43) * t47 / 0.64D2 + t6 * (-t33 + t52
     #) * t56 / 0.32D2 - t6 * (t33 - t52 + t43) * t61 / 0.64D2 - (0.90D2
     # * t7 * (t8 + t70 * t51 - t72 - t75 * t20) - 0.180D3 * t25 * t6 * 
     #(-t51 + t20)) * t55 / 0.5760D4 - (-0.180D3 * (t3 * t8 - t91 * t20)
     # * lh + t33 * t100 + 0.90D2 * t3 * t102 - 0.90D2 * t91 * t8 + 0.45
     #D2 * t108 * t20) * t6 / 0.11520D5 - (0.90D2 * t26 * t125 + (0.90D2
     # * t8 - 0.180D3 * t20 * lh) * t6 * t133) * t46 / 0.11520D5
      t139 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t138)
      t141 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t142 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t147 = t6 * t142
      t153 = t3 * t142
      t154 = t142 * t42
      t159 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x4)
      t160 = t3 * t159
      t169 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x4)
      t188 = bbbbH42J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t209 = -(0.90D2 * t7 * (t141 - t19 * t142) - 0.180D3 * t25 * t147)
     # * t30 / 0.5760D4 - t6 * (t153 + t154) * t47 / 0.64D2 + t6 * (-t15
     #3 + t160) * t56 / 0.32D2 - t6 * (-t160 + t153 + t154) * t61 / 0.64
     #D2 - (0.90D2 * t7 * (-t169 + t70 * t159 - t75 * t142 + t141) - 0.1
     #80D3 * t25 * t6 * (-t159 + t142)) * t55 / 0.5760D4 - (-0.180D3 * (
     #t3 * t141 - t91 * t142) * lh + t153 * t100 + 0.90D2 * t3 * t188 - 
     #0.90D2 * t91 * t141 + 0.45D2 * t108 * t142) * t6 / 0.11520D5 - (0.
     #90D2 * t147 * t125 + (-0.180D3 * t142 * lh + 0.90D2 * t141) * t6 *
     # t133) * t46 / 0.11520D5
      t210 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t209)
      t212 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t209)
      t215 = x1 * z
      t216 = -z - x1 + t215
      t217 = 0.1D1 / t216
      t219 = t2 * x1 * t66 * t217
      t220 = -0.1D1 + x1
      t221 = t2 * t220
      t224 = x2 * s * t1 * x1
      t225 = t1 ** 2
      t226 = s * t225
      t229 = x1 * t220 * t217
      t230 = t226 * t66 * t229
      t231 = x2 * x1
      t234 = 0.1D1 / (t231 * z - z - t231)
      t235 = t6 * t234
      t236 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, t50, 0.10D1, x4)
      t238 = t236 * t55 * t30
      t240 = t235 * t238 / 0.32D2
      t241 = FJET(XB1, XB2, s, 0.0D0, t219, -t221, t224, -t230, t240)
      t246 = x2 * x3
      t248 = 0.1D1 / (0.1D1 - x3 + t246)
      t250 = t2 * t246 * t248
      t253 = t36 * t1 * s * t248
      t256 = Sqrt(t35 * t66 * t36)
      t260 = 0.1D1 / (-z - x3 + t246 + 0.2D1 * t34 * t256)
      t261 = t6 * t260
      t262 = t36 * t248
      t263 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, -t262, x4)
      t265 = t263 * t46 * t55
      t267 = t261 * t265 / 0.64D2
      t268 = FJET(XB1, XB2, s, 0.0D0, t250, 0.0D0, -t253, 0.0D0, t267)
      t274 = t2 * x1 * t217
      t275 = t226 * t229
      t276 = bbbbH42J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t279 = t220 ** 2
      t283 = log(-0.4D1 * t13 / t14 * t217 * t279)
      t284 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t289 = t6 * lh
      t290 = t3 * t284
      t299 = x3 * t9
      t308 = Sqrt(x3 * t216 * t36)
      t313 = 0.1D1 / (-t215 - x3 * x1 * z - t299 + x1 * t14 + x3 * t14 *
     # x1 - t35 + 0.2D1 * t299 * z - t299 * t14 - t14 + 0.2D1 * t34 * t3
     #08 * z)
      t323 = -(0.90D2 * t7 * (-t276 + t283 * t284) + 0.180D3 * t289 * t2
     #90) * t30 / 0.5760D4 - t6 * (-t290 + t216 * t284 * t313) * t47 / 0
     #.64D2 + t7 * t284 * t55 * t30 / 0.32D2
      t324 = FJET(XB1, XB2, s, 0.0D0, -t221, -t274, 0.0D0, t275, t323)
      t326 = FJET(XB1, XB2, s, 0.0D0, -t274, -t221, 0.0D0, t275, t323)
      t328 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, -t262, x4)
      t330 = t328 * t46 * t55
      t332 = t261 * t330 / 0.64D2
      t333 = FJET(XB1, XB2, s, 0.0D0, -t253, 0.0D0, t250, 0.0D0, t332)
      t338 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t138)
      t340 = FJET(XB1, XB2, s, t224, -t221, t219, 0.0D0, -t230, t240)
      t345 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, t50, 0.10D1, x4)
      t347 = t345 * t55 * t30
      t349 = t235 * t347 / 0.32D2
      t350 = FJET(XB1, XB2, s, t219, 0.0D0, t224, -t221, -t230, t349)
      t355 = FJET(XB1, XB2, s, t250, 0.0D0, -t253, 0.0D0, 0.0D0, t332)
      t360 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t362 = bbbbH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t366 = t3 * t360
      t382 = -(0.90D2 * t7 * (t283 * t360 - t362) + 0.180D3 * t289 * t36
     #6) * t30 / 0.5760D4 - t6 * (t216 * t360 * t313 - t366) * t47 / 0.6
     #4D2 + t7 * t360 * t55 * t30 / 0.32D2
      t383 = FJET(XB1, XB2, s, -t221, 0.0D0, 0.0D0, -t274, t275, t382)
      t385 = FJET(XB1, XB2, s, -t221, t224, 0.0D0, t219, -t230, t349)
      t390 = FJET(XB1, XB2, s, -t274, 0.0D0, 0.0D0, -t221, t275, t382)
      t392 = FJET(XB1, XB2, s, -t253, 0.0D0, t250, 0.0D0, 0.0D0, t267)
      bbbbH4n2em1 = t139 * t138 + t210 * t209 + t212 * t209 + t241 * t6 
     #* t234 * t238 / 0.32D2 + t268 * t6 * t260 * t265 / 0.64D2 + t324 *
     # t323 + t326 * t323 + t333 * t6 * t260 * t330 / 0.64D2 + t338 * t1
     #38 + t340 * t6 * t234 * t238 / 0.32D2 + t350 * t6 * t234 * t347 / 
     #0.32D2 + t355 * t6 * t260 * t330 / 0.64D2 + t383 * t382 + t385 * t
     #6 * t234 * t347 / 0.32D2 + t390 * t382 + t392 * t6 * t260 * t265 /
     # 0.64D2

      end function



      doubleprecision function bbbbH4n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t9 = 0.1D1 / x1
      t13 = 0.1D1 - x2
      t14 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t13, 0.10D1, x4)
      t16 = 0.1D1 / x2
      t23 = bbbbH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = z ** 2
      t29 = x4 * 0.3141592653589793D1
      t30 = Sin(t29)
      t31 = t30 ** 2
      t34 = log(0.4D1 / t26 / z * t31)
      t35 = t34 * t3
      t42 = cos(t29)
      t46 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t53 = (t3 + 0.1D1 / (-z - x3 + 0.2D1 * t42 * t46)) / x3
      t56 = -t7 * t8 * t9 / 0.64D2 - t7 * (-t14 + t8) * t16 / 0.64D2 - (
     #-0.180D3 * t3 * t8 * lh + 0.90D2 * t3 * t23 - 0.90D2 * t35 * t8) *
     # t6 / 0.11520D5 - t8 * t6 * t53 / 0.128D3
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t56)
      t59 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t63 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, t13, 0.10D1, x4)
      t71 = bbbbH42J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t82 = -t7 * t59 * t9 / 0.64D2 - t7 * (-t63 + t59) * t16 / 0.64D2 -
     # (-0.180D3 * t3 * t59 * lh + 0.90D2 * t3 * t71 - 0.90D2 * t35 * t5
     #9) * t6 / 0.11520D5 - t59 * t6 * t53 / 0.128D3
      t83 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t82)
      t85 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t82)
      t87 = -0.1D1 + x1
      t88 = t2 * t87
      t91 = 0.1D1 / (-z - x1 + x1 * z)
      t93 = t2 * x1 * t91
      t94 = t1 ** 2
      t98 = s * t94 * x1 * t87 * t91
      t99 = bbbbH42J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t102 = t7 * t99 * t9 / 0.64D2
      t103 = FJET(XB1, XB2, s, 0.0D0, -t88, -t93, 0.0D0, t98, t102)
      t106 = t3 * t99 * t9
      t109 = FJET(XB1, XB2, s, 0.0D0, -t93, -t88, 0.0D0, t98, t102)
      t113 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t56)
      t115 = bbbbH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t118 = t7 * t115 * t9 / 0.64D2
      t119 = FJET(XB1, XB2, s, -t88, 0.0D0, 0.0D0, -t93, t98, t118)
      t122 = t3 * t115 * t9
      t125 = FJET(XB1, XB2, s, -t93, 0.0D0, 0.0D0, -t88, t98, t118)
      bbbbH4n2em2 = t57 * t56 + t83 * t82 + t85 * t82 + t103 * t6 * t106
     # / 0.64D2 + t109 * t6 * t106 / 0.64D2 + t113 * t56 + t119 * t6 * t
     #122 / 0.64D2 + t125 * t6 * t122 / 0.64D2

      end function



      doubleprecision function bbbbH4n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      t2 = (-0.1D1 + z) * s
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbbbH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t10 = t7 * t8 / 0.128D3
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t10)
      t13 = t6 * t8
      t15 = bbbbH42J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t17 = t7 * t15 / 0.128D3
      t18 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t17)
      t20 = t6 * t15
      t22 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t17)
      t25 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t10)
      bbbbH4n2em3 = -t11 * t3 * t13 / 0.128D3 - t18 * t3 * t20 / 0.128D3
     # - t22 * t3 * t20 / 0.128D3 - t25 * t3 * t13 / 0.128D3

      end function



      doubleprecision function bbbbH4n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH41J1
      doubleprecision bbbbH41J2
      doubleprecision bbbbH41J3
      doubleprecision bbbbH42J1
      doubleprecision bbbbH42J2
      doubleprecision bbbbH42J3
      bbbbH4n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbbbH41J1
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
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = x1 ** 2
      t9 = z + x1 * t3
      t10 = t9 ** 2
      t12 = 0.1D1 / t10 / t9
      t13 = t7 * t12
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t9 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t9 * t16 + x2 * t18 - t27
      t29 = t28 ** 2
      t30 = s * t3
      t31 = 0.1D1 / t9
      t32 = x1 * t31
      t35 = 0.1D1 - x1
      t36 = t35 * x3
      t38 = s - t30 * t32 * t28 - t30 * t36
      t39 = t29 * t38
      t42 = x2 * x3
      t43 = t18 * t15 * t9 + t42 + t27
      t46 = t35 * t18
      t48 = s - t30 * t32 * t43 - t46 * t30
      t52 = z + x1 * t15 * t3
      t53 = t48 * t35 * t52
      t57 = t4 ** 2
      t59 = t7 * x1
      t60 = 0.1D1 / t10
      t61 = t59 * t60
      t63 = t28 * t38
      t69 = t1 ** 2
      t70 = t69 * t57
      t71 = t35 ** 2
      t72 = t71 * t35
      t73 = t70 * t72
      t74 = t18 ** 2
      t75 = t74 * t18
      t79 = t38 * t48
      t80 = t79 * t2
      t81 = t5 * t72
      t85 = t7 ** 2
      t86 = t70 * t85
      t87 = t29 * t28
      t88 = t12 * t87
      t94 = t4 * t71
      t98 = t38 * t2
      t99 = t98 * t94
      t100 = t18 * t48
      t101 = t52 * t31
      t107 = t48 * t52
      t108 = t60 * x1
      t120 = t2 * t4
      t123 = t48 * t43
      t124 = t63 * t123
      t132 = t71 * t52
      t143 = t43 ** 2
      t147 = 0.2D1 * t6 * t13 * t39 * t53 - 0.9D1 * t2 * t57 * t61 * t63
     # * t48 * x2 * t35 - t73 * t75 * x1 * t48 + 0.4D1 * t80 * t81 * t75
     # - t86 * t88 * t48 + t80 * t3 * t35 * t18 - 0.4D1 * t80 * t94 * t7
     #4 + 0.3D1 * t99 * t100 * t101 + 0.2D1 * t70 * t72 * t74 * t107 * t
     #108 * t43 + 0.3D1 * t6 * t7 * t38 * t46 * t48 * t31 * t43 - 0.2D1 
     #* t120 * t7 * t60 * t124 + 0.2D1 * t6 * t61 * t124 + 0.3D1 * t6 * 
     #t108 * t28 * t79 * t132 * x3 - 0.3D1 * t120 * t108 * t63 * t53 - t
     #70 * t46 * t59 * t48 * t60 * t143
      t148 = t71 * t18
      t149 = t70 * t148
      t156 = t72 * t18
      t159 = t52 * t60
      t160 = x2 * z
      t165 = t69 * t5
      t172 = t10 ** 2
      t175 = t70 * t59 / t172
      t177 = t52 * t38
      t180 = t7 * t31
      t186 = t70 * t61
      t191 = t71 ** 2
      t192 = t191 * t18
      t193 = t70 * t192
      t194 = x3 ** 2
      t195 = t31 * t194
      t198 = t28 * t48
      t212 = t69 * t57 * t3
      t213 = t85 * t12
      t221 = t38 * t43
      t229 = t12 * t28
      t238 = t120 * t7
      t240 = t60 * t29 * t79
      t243 = 0.2D1 * t149 * t7 * t48 * t31 * t43 * x3 + 0.6D1 * t70 * t1
     #56 * t48 * t159 * t160 * x1 - 0.6D1 * t165 * t156 * t107 * t31 * z
     # * x3 - t175 * t87 * t35 * t177 - 0.6D1 * t120 * t180 * t63 * t48 
     #* z + 0.6D1 * t186 * t63 * t160 * t35 - t193 * t177 * t195 + 0.2D1
     # * t186 * t198 * t43 * t35 * x3 + 0.2D1 * t70 * t156 * t38 * t159 
     #* x1 * t43 * x3 - 0.12D2 * t212 * t213 * t63 * t43 * x2 * t35 + 0.
     #2D1 * t86 * t12 * t29 * t221 + 0.3D1 * t80 * t81 * t74 * x3 + 0.6D
     #1 * t86 * t229 * t38 * t143 - 0.2D1 * t80 * t94 * t18 * x3 - 0.4D1
     # * t238 * t240
      t250 = t31 * t28
      t251 = t250 * t79
      t257 = t6 * t59
      t263 = t74 * t48
      t267 = t69 * t4
      t269 = z ** 2
      t287 = t48 * t143
      t291 = t69 * t57 * t4
      t293 = x2 ** 2
      t312 = -0.3D1 * t120 * x1 * t38 * t35 * t100 + 0.3D1 * t238 * t251
     # + t2 * t3 * x1 * t251 + 0.4D1 * t257 * t88 * t79 + 0.2D1 * t6 * x
     #1 * t38 * t71 * t263 + 0.6D1 * t267 * t7 * t250 * t38 * t269 - t70
     # * t191 * t75 * t38 * t101 - t73 * t18 * x1 * t48 * t194 + t80 * t
     #81 * t18 * t194 - 0.2D1 * t257 * t240 - t86 * t229 * t287 + 0.6D1 
     #* t291 * t192 * t107 * t12 * t293 * t7 - 0.6D1 * t165 * t61 * t63 
     #* z * t43 + 0.2D1 * t186 * t39 * t36 - t70 * t108 * t28 * t72 * t1
     #77 * t194
      t342 = t98 * t81
      t362 = t6 * t59 * t12
      t382 = 0.6D1 * t291 * t213 * t63 * t293 * t71 - 0.6D1 * t99 * t100
     # * t101 * z + 0.6D1 * t267 * t148 * t107 * t31 * t269 - 0.12D2 * t
     #212 * t192 * t48 * t159 * t42 * x1 + 0.2D1 * t70 * t191 * t74 * t1
     #07 * t31 * x3 + 0.6D1 * t193 * t107 * t195 - 0.2D1 * t342 * t263 *
     # t101 - t149 * t177 * t13 * t143 - 0.9D1 * t98 * t57 * t72 * t18 *
     # t107 * t60 * x2 * x1 + 0.2D1 * t342 * t100 * t101 * x3 + 0.3D1 * 
     #t362 * t39 * t123 + t362 * t63 * t287 - t70 * t180 * t198 * t71 * 
     #t194 + 0.2D1 * t70 * t13 * t28 * t132 * t221 * x3 - t175 * t28 * t
     #35 * t177 * t143
      bbbbH41J1 = 0.16D2 / 0.3D1 * wd * (t147 + t243 + t312 + t382) / t3
     #8 / t48

      end function
  
   
 

      doubleprecision function bbbbH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = 0.1D1 - x3
      t10 = t8 * t9
      t11 = t6 * t10
      t12 = s * t3
      t14 = z + x1 * t3
      t15 = 0.1D1 / t14
      t16 = x1 * t15
      t17 = 0.1D1 - x2
      t18 = x3 * t17
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t18 * t14 * x2 * t9)
      t28 = 0.2D1 * t22 * t26
      t29 = t18 * t14 + x2 * t9 - t28
      t32 = t7 * x3
      t34 = s - t12 * t16 * t29 - t12 * t32
      t37 = z + x1 * t17 * t3
      t38 = t34 * t37
      t39 = t14 ** 2
      t41 = 0.1D1 / t39 / t14
      t42 = x1 ** 2
      t43 = t41 * t42
      t47 = t9 * t17 * t14 + x2 * x3 + t28
      t48 = t47 ** 2
      t53 = t1 * s
      t54 = t4 * t3
      t55 = t53 * t54
      t56 = t43 * t29
      t60 = t7 * t9
      t62 = s - t12 * t16 * t47 - t12 * t60
      t63 = t34 * t62
      t76 = t42 * x1
      t77 = 0.1D1 / t39
      t78 = t76 * t77
      t79 = t6 * t78
      t80 = t29 * t62
      t86 = t7 * t8
      t96 = t34 * t53
      t97 = t54 * t86
      t98 = t96 * t97
      t99 = t9 ** 2
      t100 = t99 * t62
      t101 = t37 * t15
      t105 = t6 * t86
      t107 = t62 * x3
      t111 = t42 ** 2
      t112 = t6 * t111
      t113 = t29 ** 2
      t114 = t41 * t113
      t115 = t62 * t47
      t119 = t34 * t47
      t123 = t63 * t53
      t128 = t53 * t4
      t131 = t9 * t62
      t140 = t55 * t76
      t141 = t113 * t29
      t142 = t41 * t141
      t146 = 0.3D1 * t11 * t38 * t43 * t48 + 0.4D1 * t55 * t56 * t63 * t
     #7 * t37 * t47 - 0.3D1 * t55 * t42 * t34 * t60 * t62 * t15 * t47 - 
     #0.4D1 * t79 * t80 * t47 * t7 * x3 - 0.4D1 * t6 * t86 * t9 * t34 * 
     #t37 * t77 * x1 * t47 * x3 - 0.2D1 * t98 * t100 * t101 + 0.2D1 * t1
     #05 * t99 * x1 * t107 - 0.2D1 * t112 * t114 * t115 - 0.2D1 * t112 *
     # t114 * t119 + 0.3D1 * t123 * t97 * t99 * x3 + 0.3D1 * t128 * x1 *
     # t34 * t7 * t131 - t53 * t3 * x1 * t15 * t29 * t63 + 0.2D1 * t140 
     #* t142 * t63
      t152 = t8 ** 2
      t154 = t99 * t9
      t159 = x3 ** 2
      t172 = t62 * t48
      t181 = t62 * t37
      t182 = t77 * x1
      t183 = t182 * t47
      t184 = t181 * t183
      t188 = t6 * t152 * t99
      t189 = x3 * t15
      t195 = t42 * t62
      t196 = t15 * t47
      t200 = t39 ** 2
      t203 = t6 * t76 / t200
      t210 = t6 * t86 * t99
      t218 = 0.4D1 * t55 * x1 * t34 * t8 * t100 + t6 * t152 * t154 * t34
     # * t101 + 0.3D1 * t105 * t9 * x1 * t62 * t159 + t123 * t97 * t9 * 
     #t159 - 0.2D1 * t140 * t77 * t113 * t63 + t112 * t41 * t29 * t172 +
     # t98 * t131 * t101 * x3 + 0.2D1 * t96 * t54 * t8 * t9 * t184 - 0.2
     #D1 * t188 * t38 * t189 - 0.2D1 * t6 * t8 * t99 * t195 * t196 + 0.2
     #D1 * t203 * t113 * t7 * t38 * t47 + 0.2D1 * t210 * t38 * t183 + 0.
     #2D1 * t79 * t113 * t62 * t32
      t220 = t42 * t15
      t227 = t29 * t34
      t249 = t62 * t7
      t250 = t249 * t37
      t257 = t113 * t34
      t275 = t55 * t76 * t41
      t281 = 0.3D1 * t6 * t220 * t80 * t8 * t159 + t55 * t78 * t227 * t1
     #15 - 0.4D1 * t11 * t195 * t196 * x3 - 0.2D1 * t188 * t181 * t189 +
     # t6 * t152 * t9 * t38 * t15 * t159 - 0.2D1 * t6 * t43 * t113 * t8 
     #* t38 * x3 + 0.3D1 * t128 * t182 * t227 * t250 - 0.2D1 * t210 * t1
     #84 + 0.4D1 * t55 * t43 * t257 * t250 + t53 * t5 * t78 * t227 * t62
     # * x2 * t7 + t96 * t5 * t86 * t9 * t181 * t77 * x2 * x1 + 0.3D1 * 
     #t275 * t257 * t115 + t275 * t227 * t172
      t298 = t8 * t37
      t336 = 0.2D1 * t55 * t220 * t227 * t249 * x3 + 0.4D1 * t55 * x1 * 
     #t34 * t10 * t107 + t6 * t60 * t76 * t62 * t77 * t48 - 0.4D1 * t6 *
     # t56 * t298 * t119 * x3 + t105 * t154 * x1 * t62 + 0.2D1 * t123 * 
     #t97 * t154 + t112 * t142 * t62 - t123 * t3 * t7 * t9 - 0.2D1 * t79
     # * t257 * t32 + t6 * t182 * t29 * t86 * t38 * t159 - 0.3D1 * t55 *
     # t182 * t29 * t63 * t298 * x3 + 0.3D1 * t203 * t29 * t7 * t38 * t4
     #8 + t203 * t141 * t7 * t38
      bbbbH41J2 = 0.16D2 / 0.3D1 * wd * (t146 + t218 + t281 + t336) / t3
     #4 / t62

      end function
  
   
 

      doubleprecision function bbbbH41J3
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
      t6 = x1 ** 2
      t7 = t5 * t6
      t9 = z + x1 * t3
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t9 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t9 + x2 * t14 - t23
      t26 = s * t3
      t27 = x1 * t10
      t30 = 0.1D1 - x1
      t33 = s - t26 * t27 * t24 - t26 * t30 * x3
      t37 = t14 * t11 * t9 + x2 * x3 + t23
      t40 = t30 * t14
      t42 = s - t26 * t27 * t37 - t26 * t40
      t43 = t33 * t42
      t44 = t10 * t24 * t43
      t46 = t9 ** 2
      t47 = 0.1D1 / t46
      t50 = t24 * t33
      t51 = t42 * t37
      t55 = t43 * t2
      t56 = t4 * t3
      t57 = t30 ** 2
      t59 = t56 * t57 * t30
      t60 = x3 ** 2
      t64 = t4 * t57
      t65 = t14 ** 2
      t69 = t2 * t56
      t72 = 0.1D1 / t46 / t9
      t74 = t69 * t6 * x1 * t72
      t75 = t24 ** 2
      t83 = x1 * t47
      t85 = t42 * t30
      t88 = z + x1 * t11 * t3
      t93 = t37 ** 2
      t97 = t33 * t2
      t130 = t14 * t42
      t158 = -t7 * t44 + 0.2D1 * t5 * t6 * t47 * t50 * t51 - t55 * t59 *
     # t14 * t60 + 0.2D1 * t55 * t64 * t65 - t74 * t75 * t33 * t51 + 0.2
     #D1 * t7 * t47 * t75 * t43 + 0.2D1 * t5 * t83 * t50 * t85 * t88 - t
     #74 * t50 * t42 * t93 - t97 * t56 * t57 * t14 * t42 * t88 * t83 * t
     #37 - t55 * t3 * t30 * t14 - t69 * t83 * t24 * t43 * t57 * t88 * x3
     # - t69 * t6 * t10 * t50 * t85 * x3 - t69 * t6 * t33 * t40 * t42 * 
     #t10 * t37 + 0.2D1 * t55 * t64 * t14 * x3 - t97 * t64 * t130 * t88 
     #* t10 - t69 * x1 * t33 * t57 * t14 * t42 * x3 - t69 * t6 * t72 * t
     #24 * t43 * t30 * t88 * t37 - t55 * t59 * t65 * x3 - t2 * t3 * x1 *
     # t44 + 0.2D1 * t5 * x1 * t33 * t30 * t130
      bbbbH41J3 = 0.16D2 / 0.3D1 * wd * t158 / t33 / t42

      end function
  
   
 

      doubleprecision function bbbbH42J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t14 = 0.1D1 / t12 / t11
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t30 = t29 * t28
      t32 = s * t3
      t33 = 0.1D1 / t11
      t34 = x1 * t33
      t37 = x2 * x3
      t38 = t18 * t15 * t11 + t37 + t27
      t41 = 0.1D1 - x1
      t42 = t41 * t18
      t44 = s - t32 * t34 * t38 - t32 * t42
      t48 = t4 * t3
      t49 = t2 * t48
      t50 = t7 * x1
      t51 = t49 * t50
      t52 = 0.1D1 / t12
      t53 = t52 * t29
      t57 = t41 ** 2
      t58 = t57 * t41
      t60 = t18 ** 2
      t61 = t60 * t18
      t68 = t41 * x3
      t70 = s - t32 * t34 * t28 - t32 * t68
      t71 = t1 * s
      t72 = t70 * t71
      t74 = t72 * t4 * t57
      t75 = t18 * t44
      t78 = z + x1 * t15 * t3
      t79 = t78 * t33
      t84 = t2 * t4
      t85 = t57 * t18
      t87 = t44 * t78
      t88 = z ** 2
      t94 = t2 * t5 * t4
      t95 = t12 ** 2
      t97 = t50 / t95
      t99 = t28 * t58
      t100 = t78 * t70
      t101 = x2 ** 2
      t105 = t5 * t3
      t106 = t2 * t105
      t107 = t58 * t60
      t109 = t7 * t44
      t114 = t70 * t44
      t115 = t71 * t48
      t118 = t38 ** 2
      t124 = t72 * t48 * t58
      t125 = t60 * t44
      t128 = t7 * t14
      t130 = t29 * t70
      t132 = t44 * t41 * t78
      t136 = x1 * t52
      t139 = t57 * t78
      t144 = t71 * t4
      t146 = t28 * t70
      t151 = t50 * t44
      t162 = t57 ** 2
      t164 = t6 * t162 * t60
      t165 = t33 * x3
      t166 = t100 * t165
      t175 = -0.2D1 * t9 * t14 * t30 * t44 + 0.2D1 * t51 * t53 * t44 - 0
     #.2D1 * t6 * t58 * t61 * x1 * t44 - 0.6D1 * t74 * t75 * t79 * z + 0
     #.6D1 * t84 * t85 * t87 * t33 * t88 - t94 * t97 * t99 * t100 * t101
     # + 0.2D1 * t106 * t107 * t109 * x2 * t33 - 0.2D1 * t114 * t115 * t
     #42 * t7 * t52 * t118 - t124 * t125 * t79 + 0.2D1 * t115 * t128 * t
     #130 * t132 + 0.5D1 * t115 * t136 * t28 * t114 * t139 * x3 - 0.3D1 
     #* t144 * t136 * t146 * t132 + 0.2D1 * t106 * t85 * t151 * t52 * t3
     #8 * x2 + 0.2D1 * t6 * t107 * t87 * t136 * t38 - 0.2D1 * t164 * t16
     #6 - 0.2D1 * t6 * t57 * t60 * t109 * t33 * t38
      t183 = t58 * t18
      t186 = t78 * t52
      t187 = x2 * z
      t192 = t49 * t183
      t198 = t7 * t70
      t205 = t71 * t5
      t206 = t50 * t52
      t208 = t44 * x2
      t209 = t208 * t41
      t222 = t44 * t38
      t225 = t6 * t206
      t231 = x3 ** 2
      t237 = t29 * t57
      t242 = t84 * t7
      t243 = t33 * t28
      t265 = -t6 * t42 * t151 * t52 * t118 + t124 * t75 * t79 * x3 + 0.6
     #D1 * t6 * t183 * t44 * t186 * t187 * x1 - 0.6D1 * t192 * t87 * t33
     # * z * x3 + 0.5D1 * t115 * t198 * t42 * t44 * t33 * t38 - 0.7D1 * 
     #t205 * t206 * t146 * t209 - 0.7D1 * t72 * t5 * t58 * t18 * t87 * t
     #52 * x2 * x1 + t115 * t206 * t146 * t222 + 0.2D1 * t225 * t130 * t
     #68 - 0.2D1 * t115 * t34 * t146 * t44 * t57 * t231 + 0.2D1 * t106 *
     # t97 * t237 * t100 * x2 - t242 * t243 * t44 - t114 * t205 * t107 *
     # x2 * x1 * t33 - t84 * t57 * t18 * t70 * t79 - 0.6D1 * t144 * t7 *
     # t33 * t146 * t44 * z + 0.6D1 * t225 * t146 * t187 * t41
      t267 = t162 * t18
      t268 = t6 * t267
      t269 = t33 * t231
      t279 = t71 * t105
      t280 = t50 * t14
      t289 = t8 * t14
      t307 = t14 * t28
      t342 = -t268 * t100 * t269 + 0.2D1 * t192 * t166 + 0.2D1 * t205 * 
     #t198 * t85 * t208 * t33 + t279 * t280 * t146 * t44 * t101 * t57 - 
     #t205 * t280 * t130 * t209 + 0.6D1 * t94 * t289 * t146 * t101 * t57
     # + 0.6D1 * t242 * t243 * t70 * t88 - 0.2D1 * t6 * t162 * t61 * t70
     # * t79 - t115 * t50 * t53 * t114 - t9 * t307 * t44 * t118 + 0.2D1 
     #* t49 * t58 * t60 * t70 * t79 + 0.2D1 * t51 * t52 * t28 * t222 - 0
     #.12D2 * t106 * t289 * t146 * t38 * x2 * t41 - 0.2D1 * t6 * t97 * t
     #30 * t41 * t100 - 0.6D1 * t49 * t206 * t146 * z * t38 + 0.6D1 * t9
     #4 * t267 * t87 * t14 * t101 * t7
      t355 = t128 * t28
      t390 = t14 * t29
      t415 = 0.2D1 * t164 * t87 * t165 + 0.6D1 * t268 * t87 * t269 - 0.1
     #2D2 * t106 * t267 * t44 * t186 * t37 * x1 + 0.2D1 * t106 * t355 * 
     #t58 * t78 * t70 * x3 * x2 - t6 * t136 * t99 * t100 * t231 + 0.2D1 
     #* t205 * t355 * t114 * t139 * x2 + t114 * t279 * t183 * t101 * t7 
     #* t52 + 0.2D1 * t144 * t7 * t243 * t114 - 0.3D1 * t144 * x1 * t70 
     #* t41 * t75 + 0.6D1 * t9 * t307 * t70 * t118 + 0.2D1 * t9 * t390 *
     # t70 * t38 - 0.2D1 * t9 * t390 * t222 - t94 * t183 * t151 * t101 *
     # t52 + 0.2D1 * t115 * x1 * t70 * t57 * t125 + 0.2D1 * t74 * t75 * 
     #t79 - 0.2D1 * t6 * t128 * t237 * t100 * x3
      bbbbH42J1 = 0.16D2 / 0.3D1 * wd * (t175 + t265 + t342 + t415) / t7
     #0 / t44

      end function
  
   
 

      doubleprecision function bbbbH42J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = x1 ** 2
      t9 = z + x1 * t3
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t9 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t9 + x2 * t14 - t23
      t25 = t10 * t24
      t26 = s * t3
      t27 = x1 * t10
      t31 = t14 * t11 * t9 + x2 * x3 + t23
      t34 = 0.1D1 - x1
      t35 = t34 * t14
      t37 = s - t26 * t27 * t31 - t26 * t35
      t41 = t4 ** 2
      t43 = t2 * t41 * t4
      t44 = t34 ** 2
      t45 = t44 * t34
      t46 = t45 * t14
      t48 = t6 * x1
      t49 = t48 * t37
      t50 = x2 ** 2
      t51 = t9 ** 2
      t52 = 0.1D1 / t51
      t57 = t1 * s
      t58 = t4 * t3
      t59 = t57 * t58
      t62 = t34 * x3
      t64 = s - t26 * t27 * t24 - t26 * t62
      t65 = t6 * t64
      t72 = t64 * t57
      t74 = t72 * t58 * t45
      t75 = t14 ** 2
      t79 = z + x1 * t11 * t3
      t80 = t79 * t10
      t83 = t41 * t3
      t84 = t57 * t83
      t86 = 0.1D1 / t51 / t9
      t87 = t48 * t86
      t89 = t24 * t64
      t94 = t64 * t37
      t95 = t57 * t41
      t97 = t45 * t75
      t105 = t37 * t79
      t106 = x1 * t52
      t108 = t105 * t106 * t31
      t111 = t2 * t41
      t112 = t6 * t86
      t114 = t24 ** 2
      t115 = t114 * t44
      t116 = t79 * t64
      t121 = t44 ** 2
      t123 = t111 * t121 * t75
      t124 = t10 * x3
      t125 = t116 * t124
      t128 = t51 ** 2
      t130 = t48 / t128
      t132 = t114 * t24
      t137 = t2 * t83
      t138 = t112 * t24
      t148 = t44 * t79
      t153 = 0.3D1 * t5 * t6 * t25 * t37 + 0.3D1 * t43 * t46 * t49 * t50
     # * t52 - 0.5D1 * t59 * t65 * t35 * t37 * t10 * t31 + t74 * t75 * t
     #37 * t80 - t84 * t87 * t89 * t37 * t50 * t44 + t94 * t95 * t97 * x
     #2 * x1 * t10 - 0.2D1 * t72 * t58 * t44 * t14 * t108 + 0.2D1 * t111
     # * t112 * t115 * t116 * x3 + 0.2D1 * t123 * t125 + 0.2D1 * t111 * 
     #t130 * t132 * t34 * t116 - 0.4D1 * t137 * t138 * t45 * t79 * t64 *
     # x3 * x2 - 0.5D1 * t59 * t106 * t24 * t94 * t148 * x3
      t154 = t4 * t57
      t156 = t34 * t37
      t169 = x3 ** 2
      t174 = t31 ** 2
      t179 = t75 * t14
      t184 = t6 ** 2
      t185 = t111 * t184
      t190 = t2 * t58
      t191 = t190 * t48
      t192 = t52 * t114
      t206 = t114 * t64
      t207 = t37 * x2
      t208 = t207 * t34
      t212 = t24 * t45
      t218 = t6 * t37
      t223 = 0.3D1 * t154 * t106 * t89 * t156 * t79 - 0.2D1 * t111 * t97
     # * t108 - 0.2D1 * t123 * t105 * t124 + t111 * t121 * t14 * t116 * 
     #t10 * t169 + t111 * t35 * t49 * t52 * t174 + 0.2D1 * t111 * t45 * 
     #t179 * x1 * t37 + 0.2D1 * t185 * t86 * t132 * t37 - 0.4D1 * t191 *
     # t192 * t37 - 0.4D1 * t190 * t46 * t125 + 0.2D1 * t59 * t27 * t89 
     #* t37 * t44 * t169 + t95 * t87 * t206 * t208 + 0.3D1 * t43 * t130 
     #* t212 * t116 * t50 - 0.4D1 * t137 * t97 * t218 * x2 * t10
      t235 = t14 * t37
      t251 = t44 * t14
      t266 = t48 * t52
      t268 = t37 * t31
      t291 = -t94 * t84 * t46 * t50 * t6 * t52 - 0.4D1 * t137 * t130 * t
     #115 * t116 * x2 + 0.6D1 * t74 * t235 * t80 * x3 - 0.3D1 * t72 * t4
     # * t44 * t235 * t80 + 0.2D1 * t94 * t59 * t35 * t6 * t52 * t174 - 
     #0.4D1 * t137 * t251 * t49 * t52 * t31 * x2 + 0.3D1 * t72 * t41 * t
     #45 * t14 * t105 * t52 * x2 * x1 + 0.6D1 * t59 * t266 * t89 * t268 
     #+ 0.2D1 * t95 * t65 * t251 * t207 * t10 - 0.2D1 * t111 * t266 * t2
     #06 * t62 + t111 * t106 * t212 * t116 * t169 + 0.2D1 * t111 * t44 *
     # t75 * t218 * t10 * t31
      t311 = t86 * t114
      t350 = -0.4D1 * t191 * t52 * t24 * t268 - 0.2D1 * t59 * t6 * t10 *
     # t89 * t156 * x3 + 0.3D1 * t95 * t266 * t89 * t208 + 0.2D1 * t95 *
     # t138 * t94 * t148 * x2 + 0.2D1 * t185 * t311 * t268 + 0.3D1 * t5 
     #* t44 * t14 * t64 * t80 - 0.2D1 * t185 * t311 * t64 * t31 + 0.3D1 
     #* t154 * x1 * t64 * t34 * t235 - 0.3D1 * t154 * t6 * t25 * t94 + 0
     #.2D1 * t111 * t121 * t179 * t64 * t80 + t59 * t48 * t192 * t94 + t
     #185 * t86 * t24 * t37 * t174 - 0.4D1 * t190 * t45 * t75 * t64 * t8
     #0
      bbbbH42J2 = 0.16D2 / 0.3D1 * wd * (t153 + t223 + t291 + t350) / t6
     #4 / t37

      end function
  
   
 

      doubleprecision function bbbbH42J3
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
      t7 = x1 ** 2
      t8 = t7 * x1
      t10 = z + x1 * t3
      t11 = t10 ** 2
      t13 = 0.1D1 / t11 / t10
      t14 = t8 * t13
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t10 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t10 + x2 * t19 - t28
      t30 = t29 ** 2
      t31 = s * t3
      t32 = 0.1D1 / t10
      t33 = x1 * t32
      t36 = 0.1D1 - x1
      t39 = s - t31 * t33 * t29 - t31 * t36 * x3
      t40 = t30 * t39
      t44 = t19 * t16 * t10 + x2 * x3 + t28
      t45 = t33 * t44
      t47 = t36 * t19
      t49 = s - t31 * t45 - t31 * t47
      t50 = t49 * x2
      t51 = t50 * t36
      t54 = t39 * t2
      t55 = t36 ** 2
      t56 = t55 * t36
      t62 = z + x1 * t16 * t3
      t63 = t49 * t62
      t64 = 0.1D1 / t11
      t69 = t4 * t3
      t73 = t64 * x1
      t78 = t2 * t5 * t3
      t80 = t29 * t39
      t81 = x2 ** 2
      t86 = t2 * t69
      t89 = t19 ** 2
      t93 = t2 * t4
      t96 = t39 * t49
      t101 = t19 * t49
      t102 = t62 * t32
      t106 = t7 * t13
      t109 = t55 * t62
      t115 = t49 * t36
      t117 = t80 * t115 * x3
      t119 = t7 * t39
      t141 = t8 * t64
      t170 = t6 * t14 * t40 * t51 - t54 * t5 * t56 * t19 * t63 * t64 * x
     #2 * x1 - t54 * t69 * t55 * t19 * t63 * t73 * t44 - t78 * t14 * t80
     # * t49 * t81 * t55 + t86 * x1 * t39 * t55 * t89 * t49 + t93 * t7 *
     # t32 * t29 * t96 - t54 * t69 * t56 * t101 * t102 * x3 - t6 * t106 
     #* t29 * t96 * t109 * x2 - t86 * t7 * t32 * t117 - t86 * t119 * t47
     # * t49 * t32 * t44 + t86 * t106 * t40 * t115 * t62 - t6 * t119 * t
     #55 * t19 * t50 * t32 + t54 * t4 * t55 * t101 * t102 + 0.2D1 * t93 
     #* t33 * t117 - t6 * t141 * t80 * t51 - t86 * t141 * t80 * t49 * t4
     #4 - t96 * t78 * t56 * t19 * t81 * t7 * t64 - t86 * t73 * t29 * t96
     # * t109 * x3 + t96 * t6 * t56 * t89 * x2 * x1 * t32 + 0.2D1 * t96 
     #* t93 * t47 * t45
      bbbbH42J3 = 0.16D2 / 0.3D1 * wd * t170 / t39 / t49

      end function
  
 