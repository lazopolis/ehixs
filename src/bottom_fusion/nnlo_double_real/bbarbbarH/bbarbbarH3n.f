  
      subroutine bbarbbarH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH31J1  
      doubleprecision bbarbbarH31J2  
      doubleprecision bbarbbarH31J3  
      doubleprecision bbarbbarH32J1  
      doubleprecision bbarbbarH32J2  
      doubleprecision bbarbbarH32J3  
      doubleprecision bbarbbarH3n1e1  
      doubleprecision bbarbbarH3n1e0  
      doubleprecision bbarbbarH3n1em1  
      doubleprecision bbarbbarH3n1em2  
      doubleprecision bbarbbarH3n1em3  
      doubleprecision bbarbbarH3n1em4  
      doubleprecision bbarbbarH3n2e1  
      doubleprecision bbarbbarH3n2e0  
      doubleprecision bbarbbarH3n2em1  
      doubleprecision bbarbbarH3n2em2  
      doubleprecision bbarbbarH3n2em3  
      doubleprecision bbarbbarH3n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH3n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH3n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH3n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH3n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH3n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH3n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH3n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH3n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH3n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH3n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH3n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH3n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH3n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
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
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t20 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t21 = t17 ** 2
      t22 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t29 = lh ** 2
      t31 = 0.3141592653589793D1 ** 2
      t33 = 0.180D3 * t29 - 0.30D2 * t31
      t34 = t3 * t33
      t39 = t3 * t7
      t41 = t21 * t17
      t53 = -0.2884936567583026D3 - 0.120D3 * t29 * lh + 0.60D2 * lh * t
     #31
      t54 = t3 * t53
      t55 = t7 * t22
      t56 = t54 * t55
      t58 = 0.1D1 / x1
      t61 = lh * t7
      t62 = t3 * t18
      t63 = t8 * x3
      t64 = t14 * t11
      t67 = log(0.4D1 * t63 * t64)
      t68 = t67 * t3
      t70 = -0.1D1 + x3
      t71 = 0.1D1 / t70
      t72 = t64 * t71
      t75 = log(-0.4D1 * t63 * t72)
      t78 = cos(t9)
      t80 = Sqrt(-x3 * t70)
      t85 = 0.1D1 / (-z - x3 + 0.2D1 * t78 * t80 * z)
      t91 = t75 ** 2
      t96 = t3 * t20
      t98 = t67 ** 2
      t99 = t98 * t3
      t105 = t33 * t7
      t106 = t3 * t22
      t108 = -t106 - t22 * t85
      t111 = 0.1D1 / x3
      t115 = x2 ** 2
      t116 = t115 * x3
      t117 = t116 * t15
      t119 = log(0.4D1 * t117)
      t120 = t119 * t3
      t122 = t116 * t8
      t125 = log(-0.4D1 * t122 * t72)
      t132 = -t108
      t137 = 0.1D1 / x2
      t138 = t137 * t58
      t141 = t115 * t8
      t144 = log(0.4D1 * t141 * t64)
      t145 = t144 * t3
      t150 = t144 ** 2
      t151 = t150 * t3
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
      t210 = x3 * t11
      t213 = log(0.4D1 * t210 * t14)
      t214 = t213 ** 2
      t219 = log(-0.4D1 * t210 * t14 * t71)
      t220 = t219 ** 2
      t223 = -t214 * t3 / 0.2D1 - t220 * t85 / 0.2D1
      t233 = t219 * t85 + t213 * t3
      t240 = t214 * t213 * t3 / 0.6D1 + t220 * t219 * t85 / 0.6D1
      t246 = t53 * t7
      t249 = -t85 - t3
      t254 = t115 * t11
      t257 = log(0.4D1 * t254 * t14)
      t258 = t257 * t3
      t260 = t257 ** 2
      t261 = t260 * t3
      t274 = t260 * t257 * t3
      t285 = log(-0.4D1 * t116 * t72)
      t291 = log(0.4D1 * t116 * t64)
      t292 = t291 * t3
      t298 = t285 ** 2
      t303 = t291 ** 2
      t304 = t303 * t3
      t316 = (-0.180D3 * t4 * t7 * (t17 * t18 - t20 - t21 * t22 / 0.2D1)
     # + t34 * t7 * (-t18 + t17 * t22) + 0.90D2 * t39 * (t17 * t20 + t41
     # * t22 / 0.6D1 - t21 * t18 / 0.2D1) - t56) * t58 / 0.5760D4 + (-0.
     #180D3 * t61 * (-t62 + t68 * t22 - (t18 - t75 * t22) * t85) + 0.90D
     #2 * t7 * (-(-t75 * t18 + t20 + t91 * t22 / 0.2D1) * t85 - t96 + t6
     #8 * t18 - t99 * t22 / 0.2D1) + t105 * t108) * t111 * t58 / 0.5760D
     #4 - (0.90D2 * t7 * (-t120 * t22 + (t18 - t125 * t22) * t85 + t62) 
     #- 0.180D3 * t61 * t132) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61
     # * (-t62 + t145 * t22) + 0.90D2 * t7 * (-t96 - t151 * t22 / 0.2D1 
     #+ t145 * t18) - t105 * t106) * t137 * t58 / 0.2880D4 - t172 * t20 
     #/ 0.11520D5 - t182 * t18 / 0.11520D5 - t202 * t22 / 0.11520D5 + ((
     #0.90D2 * t7 * t18 - 0.180D3 * t61 * t22) * t223 + (0.90D2 * t7 * t
     #20 - 0.180D3 * t61 * t18 + t105 * t22) * t233 + 0.90D2 * t55 * t24
     #0 + (-0.180D3 * t61 * t20 + t105 * t18 + t246 * t22) * t249) * t11
     #1 / 0.11520D5 + (-0.180D3 * t61 * (-t96 + t258 * t18 - t261 * t22 
     #/ 0.2D1) + t105 * (t258 * t22 - t62) + 0.90D2 * t7 * (t258 * t20 -
     # t261 * t18 / 0.2D1 + t274 * t22 / 0.6D1) - t56) * t137 / 0.5760D4
     # - (-0.180D3 * t61 * ((t18 - t285 * t22) * t85 + t62 - t292 * t22)
     # + 0.90D2 * t7 * (t96 + (-t285 * t18 + t20 + t298 * t22 / 0.2D1) *
     # t85 + t304 * t22 / 0.2D1 - t292 * t18) + t105 * t132) * t111 * t1
     #37 / 0.5760D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t316)
      t319 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t322 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t323 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t341 = t7 * t319
      t342 = t54 * t341
      t346 = t3 * t323
      t359 = t3 * t322
      t366 = t3 * t319
      t368 = -t366 - t319 * t85
      t381 = -t368
      t476 = (-0.180D3 * t4 * t7 * (-t21 * t319 / 0.2D1 - t322 + t17 * t
     #323) + t34 * t7 * (-t323 + t17 * t319) + 0.90D2 * t39 * (t41 * t31
     #9 / 0.6D1 + t17 * t322 - t21 * t323 / 0.2D1) - t342) * t58 / 0.576
     #0D4 + (-0.180D3 * t61 * (-t346 + t68 * t319 - (t323 - t75 * t319) 
     #* t85) + 0.90D2 * t7 * (-(t91 * t319 / 0.2D1 + t322 - t75 * t323) 
     #* t85 - t359 + t68 * t323 - t99 * t319 / 0.2D1) + t105 * t368) * t
     #111 * t58 / 0.5760D4 - (0.90D2 * t7 * (-t120 * t319 + t346 + (t323
     # - t125 * t319) * t85) - 0.180D3 * t61 * t381) * t111 * t138 / 0.2
     #880D4 + (-0.180D3 * t61 * (-t346 + t145 * t319) + 0.90D2 * t7 * (-
     #t151 * t319 / 0.2D1 - t359 + t145 * t323) - t105 * t366) * t137 * 
     #t58 / 0.2880D4 - t182 * t323 / 0.11520D5 - t202 * t319 / 0.11520D5
     # + ((0.90D2 * t7 * t323 - 0.180D3 * t61 * t319) * t223 + (0.90D2 *
     # t7 * t322 - 0.180D3 * t61 * t323 + t105 * t319) * t233 + 0.90D2 *
     # t341 * t240 + (-0.180D3 * t61 * t322 + t105 * t323 + t246 * t319)
     # * t249) * t111 / 0.11520D5 - t172 * t322 / 0.11520D5 + (-0.180D3 
     #* t61 * (-t359 + t258 * t323 - t261 * t319 / 0.2D1) + t105 * (-t34
     #6 + t258 * t319) + 0.90D2 * t7 * (t258 * t322 - t261 * t323 / 0.2D
     #1 + t274 * t319 / 0.6D1) - t342) * t137 / 0.5760D4 - (-0.180D3 * t
     #61 * ((t323 - t285 * t319) * t85 + t346 - t292 * t319) + 0.90D2 * 
     #t7 * ((t298 * t319 / 0.2D1 + t322 - t285 * t323) * t85 - t292 * t3
     #23 + t304 * t319 / 0.2D1 + t359) + t105 * t381) * t111 * t137 / 0.
     #5760D4
      t477 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t476)
      t479 = t2 * x1
      t480 = -0.1D1 + x1
      t481 = x1 * z
      t482 = 0.1D1 - x1 + t481
      t483 = 0.1D1 / t482
      t485 = t2 * t480 * t483
      t486 = t1 ** 2
      t487 = s * t486
      t489 = x1 * t480 * t483
      t490 = t487 * t489
      t491 = t14 * t483
      t492 = t480 ** 2
      t493 = t491 * t492
      t496 = log(0.4D1 * t12 * t493)
      t497 = t496 ** 2
      t498 = -t480
      t499 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t498, 0.10D1, 0.10D1,
     # x4)
      t502 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, t498, 0.10D1, 0.10D1,
     # x4)
      t503 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, t498, 0.10D1, 0.10D1,
     # x4)
      t514 = t497 * t496
      t527 = t3 * t503
      t528 = t63 * t11
      t531 = log(0.4D1 * t528 * t493)
      t532 = t531 * t3
      t535 = t491 * t492 * t71
      t538 = log(-0.4D1 * t528 * t535)
      t542 = x1 * x3
      t543 = 0.2D1 * t542
      t544 = x1 * t13
      t545 = x3 * t13
      t546 = t545 * x1
      t548 = 0.2D1 * t63 * z
      t549 = t63 * t13
      t550 = t542 * z
      t551 = 0.3D1 * t550
      t552 = x3 * t482
      t554 = Sqrt(-t552 * t70)
      t558 = -t63 - z + t543 - x3 + t481 - t544 + t546 + t548 - t549 - t
     #551 + 0.2D1 * t78 * t554 * z
      t559 = 0.1D1 / t558
      t565 = t538 ** 2
      t571 = t3 * t502
      t573 = t531 ** 2
      t574 = t573 * t3
      t582 = t3 * t499
      t583 = t499 * t482 * t559 + t582
      t589 = t483 * t492
      t593 = log(0.4D1 * t122 * t64 * t589)
      t594 = t593 * t3
      t596 = t116 * t12
      t599 = log(-0.4D1 * t596 * t535)
      t614 = t141 * t11
      t617 = log(0.4D1 * t614 * t493)
      t618 = t617 * t3
      t623 = t617 ** 2
      t624 = t623 * t3
      t636 = (-0.180D3 * t4 * t7 * (t497 * t499 / 0.2D1 + t502 - t496 * 
     #t503) + t34 * t7 * (t503 - t496 * t499) + 0.90D2 * t39 * (-t496 * 
     #t502 - t514 * t499 / 0.6D1 + t497 * t503 / 0.2D1) + t54 * t7 * t49
     #9) * t58 / 0.5760D4 + (-0.180D3 * t61 * (t527 - t532 * t499 + (t50
     #3 - t538 * t499) * t482 * t559) + 0.90D2 * t7 * ((-t538 * t503 + t
     #502 + t565 * t499 / 0.2D1) * t482 * t559 + t571 - t532 * t503 + t5
     #74 * t499 / 0.2D1) + t105 * t583) * t111 * t58 / 0.5760D4 - (0.90D
     #2 * t7 * (t594 * t499 - (t503 - t599 * t499) * t482 * t559 - t527)
     # + 0.180D3 * t61 * t583) * t111 * t138 / 0.2880D4 + (-0.180D3 * t6
     #1 * (t527 - t618 * t499) + 0.90D2 * t7 * (t571 + t624 * t499 / 0.2
     #D1 - t618 * t503) + t105 * t582) * t137 * t58 / 0.2880D4
      t637 = FJET(XB1, XB2, s, 0.0D0, t479, -t485, 0.0D0, -t490, t636)
      t639 = x2 * x3
      t640 = -0.1D1 + x2
      t643 = Sqrt(x3 * t640 * t70)
      t644 = t78 * t643
      t646 = 0.2D1 * t644 * x2
      t648 = 0.1D1 - x3 + t639
      t649 = 0.1D1 / t648
      t651 = t2 * (0.1D1 - x3 - x2 + t639 + t116 + t646) * t649
      t656 = t2 * x2 * (-0.1D1 + t639 + 0.2D1 * t644) * t649
      t657 = x2 * z
      t658 = t639 * z
      t659 = t116 * z
      t665 = 0.1D1 / (z - t657 + x2 + x3 - t116 - t658 + t659 - t646 - 0
     #.2D1 * t644 * z + 0.2D1 * t644 * t657)
      t666 = -t640
      t667 = t70 * t649
      t668 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, t666, -t667, 
     #x4)
      t669 = t665 * t668
      t670 = t14 * t640
      t671 = t648 ** 2
      t672 = 0.1D1 / t671
      t673 = t70 * t672
      t677 = log(0.4D1 * t596 * t670 * t673)
      t678 = t677 * t665
      t679 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t666, -t667, 
     #x4)
      t684 = t665 * t679
      t696 = log(0.4D1 * t116 * t14 * t11 * t640 * t673)
      t697 = t696 * t665
      t702 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.10D1, t666, -t667, 
     #x4)
      t704 = t696 ** 2
      t705 = t704 * t665
      t717 = -(0.90D2 * t7 * (t669 - t678 * t679) - 0.180D3 * t61 * t684
     #) * t111 * t138 / 0.2880D4 - (-0.180D3 * t61 * (t669 - t697 * t679
     #) + 0.90D2 * t7 * (t665 * t702 + t705 * t679 / 0.2D1 - t697 * t668
     #) + t105 * t684) * t111 * t137 / 0.5760D4
      t718 = FJET(XB1, XB2, s, 0.0D0, t651, 0.0D0, -t656, 0.0D0, t717)
      t720 = t640 * s
      t721 = t1 * t480
      t723 = t720 * t721 * t483
      t724 = x2 * s
      t725 = t724 * t721
      t727 = t487 * t640 * t489
      t729 = t491 * t492 * t640
      t732 = log(-0.4D1 * t596 * t729)
      t733 = x2 * x1
      t734 = t733 * z
      t736 = 0.1D1 / (z - t657 - t733 + x2 + t734)
      t737 = t732 * t736
      t738 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t498, t666, 0.10D1, x
     #4)
      t740 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, t498, t666, 0.10D1, x
     #4)
      t741 = t736 * t740
      t745 = t736 * t738
      t753 = log(-0.4D1 * t614 * t729)
      t754 = t753 * t736
      t759 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, t498, t666, 0.10D1, x
     #4)
      t761 = t753 ** 2
      t762 = t761 * t736
      t774 = -(0.90D2 * t7 * (-t737 * t738 + t741) - 0.180D3 * t61 * t74
     #5) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t754 * t738 - t74
     #1) + 0.90D2 * t7 * (-t736 * t759 - t762 * t738 / 0.2D1 + t754 * t7
     #40) - t105 * t745) * t137 * t58 / 0.2880D4
      t775 = FJET(XB1, XB2, s, 0.0D0, t723, t479, -t725, t727, t774)
      t777 = t720 * t1
      t778 = t724 * t1
      t779 = t64 * t640
      t782 = log(-0.4D1 * t122 * t779)
      t784 = 0.1D1 / (-z + t657 - x2)
      t785 = t782 * t784
      t786 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t666, 0.10D1,
     # x4)
      t788 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, t666, 0.10D1,
     # x4)
      t789 = t784 * t788
      t793 = t784 * t786
      t802 = log(-0.4D1 * t141 * t779)
      t803 = t802 * t784
      t808 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.10D1, t666, 0.10D1,
     # x4)
      t809 = t784 * t808
      t810 = t802 ** 2
      t811 = t810 * t784
      t818 = t105 * t793
      t825 = log(-0.4D1 * t254 * t670)
      t826 = t825 * t784
      t828 = t825 ** 2
      t829 = t828 * t784
      t842 = t828 * t825 * t784
      t854 = log(-0.4D1 * t116 * t779)
      t855 = t854 * t784
      t861 = t854 ** 2
      t862 = t861 * t784
      t872 = -(0.90D2 * t7 * (-t785 * t786 + t789) - 0.180D3 * t61 * t79
     #3) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t803 * t786 - t78
     #9) + 0.90D2 * t7 * (-t809 - t811 * t786 / 0.2D1 + t803 * t788) - t
     #818) * t137 * t58 / 0.2880D4 + (-0.180D3 * t61 * (-t809 + t826 * t
     #788 - t829 * t786 / 0.2D1) + t105 * (t826 * t786 - t789) + 0.90D2 
     #* t7 * (t826 * t808 - t829 * t788 / 0.2D1 + t842 * t786 / 0.6D1) -
     # t246 * t793) * t137 / 0.5760D4 - (-0.180D3 * t61 * (-t855 * t786 
     #+ t789) + 0.90D2 * t7 * (-t855 * t788 + t862 * t786 / 0.2D1 + t809
     #) + t818) * t111 * t137 / 0.5760D4
      t873 = FJET(XB1, XB2, s, 0.0D0, -t777, 0.0D0, t778, 0.0D0, t872)
      t875 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, t498, 0.10D1, 0.10D1,
     # x4)
      t877 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, t498, 0.10D1, 0.10D1,
     # x4)
      t878 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t498, 0.10D1, 0.10D1,
     # x4)
      t902 = t3 * t875
      t917 = t3 * t877
      t926 = t3 * t878
      t927 = t878 * t482 * t559 + t926
      t963 = (-0.180D3 * t4 * t7 * (-t496 * t875 + t877 + t497 * t878 / 
     #0.2D1) + t34 * t7 * (t875 - t496 * t878) + 0.90D2 * t39 * (-t496 *
     # t877 - t514 * t878 / 0.6D1 + t497 * t875 / 0.2D1) + t54 * t7 * t8
     #78) * t58 / 0.5760D4 + (-0.180D3 * t61 * (t902 - t532 * t878 + (t8
     #75 - t538 * t878) * t482 * t559) + 0.90D2 * t7 * ((t565 * t878 / 0
     #.2D1 + t877 - t538 * t875) * t482 * t559 + t917 - t532 * t875 + t5
     #74 * t878 / 0.2D1) + t105 * t927) * t111 * t58 / 0.5760D4 - (0.90D
     #2 * t7 * (t594 * t878 - t902 - (t875 - t599 * t878) * t482 * t559)
     # + 0.180D3 * t61 * t927) * t111 * t138 / 0.2880D4 + (-0.180D3 * t6
     #1 * (-t618 * t878 + t902) + 0.90D2 * t7 * (t917 - t618 * t875 + t6
     #24 * t878 / 0.2D1) + t105 * t926) * t137 * t58 / 0.2880D4
      t964 = FJET(XB1, XB2, s, 0.0D0, -t485, t479, 0.0D0, -t490, t963)
      t966 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t666, 0.10D1,
     # x4)
      t968 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, t666, 0.10D1,
     # x4)
      t969 = t784 * t968
      t973 = t784 * t966
      t984 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.10D1, t666, 0.10D1,
     # x4)
      t985 = t784 * t984
      t992 = t105 * t973
      t1032 = -(0.90D2 * t7 * (-t785 * t966 + t969) - 0.180D3 * t61 * t9
     #73) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (-t969 + t803 * t
     #966) + 0.90D2 * t7 * (-t985 + t803 * t968 - t811 * t966 / 0.2D1) -
     # t992) * t137 * t58 / 0.2880D4 + (-0.180D3 * t61 * (-t985 + t826 *
     # t968 - t829 * t966 / 0.2D1) + t105 * (t826 * t966 - t969) + 0.90D
     #2 * t7 * (t826 * t984 - t829 * t968 / 0.2D1 + t842 * t966 / 0.6D1)
     # - t246 * t973) * t137 / 0.5760D4 - (-0.180D3 * t61 * (t969 - t855
     # * t966) + 0.90D2 * t7 * (t862 * t966 / 0.2D1 + t985 - t855 * t968
     #) + t992) * t111 * t137 / 0.5760D4
      t1033 = FJET(XB1, XB2, s, t778, 0.0D0, -t777, 0.0D0, 0.0D0, t1032)
      t1036 = t479 * t639 * t649
      t1037 = t2 * t480
      t1038 = t116 * x1
      t1039 = t640 * t70
      t1041 = Sqrt(t552 * t1039)
      t1042 = t78 * t1041
      t1044 = 0.2D1 * t1042 * x2
      t1045 = t116 * t481
      t1049 = t1037 * (t116 - x2 + t639 + 0.1D1 - x3 - t1038 + t1044 + t
     #1045) * t483 * t649
      t1053 = t70 * s * t1 * x1 * t649
      t1059 = t1037 * x2 * (-0.1D1 + t639 + x1 - t542 - t481 + t550 + 0.
     #2D1 * t1042) * t483 * t649
      t1067 = x2 * t8
      t1073 = -x2 - t63 + t543 - t544 + t481 + 0.2D1 * t1042 * t734 - 0.
     #3D1 * t734 - z + t658 - t659 + 0.2D1 * t733 + t63 * x2 - t639 * x1
     # + t733 * t13 + 0.2D1 * t1067 * z - t1067 * t13 + 0.2D1 * t1042 * 
     #z
      t1085 = t116 + t657 - t1038 + t1044 + t546 + t548 - t549 - t551 - 
     #t1067 + t1045 - 0.2D1 * t1042 * t657 - 0.2D1 * t1042 * t733 - t545
     # * t733 - 0.2D1 * t63 * t657 + t63 * t13 * x2 + 0.2D1 * t639 * t48
     #1 - x3
      t1087 = 0.1D1 / (t1073 + t1085)
      t1088 = t482 * t1087
      t1089 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, t498, t666, -t667, x
     #4)
      t1095 = log(0.4D1 * t117 * t589 * t1039 * t672)
      t1096 = t1095 * t482
      t1097 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t498, t666, -t667, x
     #4)
      t1106 = 0.90D2 * t7 * (t1088 * t1089 - t1096 * t1087 * t1097) - 0.
     #180D3 * t61 * t1088 * t1097
      t1110 = FJET(XB1, XB2, s, t1036, -t1049, -t1053, t1059, t727, -t11
     #06 * t111 * t138 / 0.2880D4)
      t1113 = t111 * t137 * t58
      t1116 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, t498, t666, -t667, x
     #4)
      t1118 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t498, t666, -t667, x
     #4)
      t1127 = 0.90D2 * t7 * (t1088 * t1116 - t1096 * t1087 * t1118) - 0.
     #180D3 * t61 * t1088 * t1118
      t1131 = FJET(XB1, XB2, s, t1059, -t1053, -t1049, t1036, t727, -t11
     #27 * t111 * t138 / 0.2880D4)
      t1135 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, t498, t666, 0.10D1, 
     #x4)
      t1136 = t736 * t1135
      t1137 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t498, t666, 0.10D1, 
     #x4)
      t1142 = t736 * t1137
      t1155 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, t498, t666, 0.10D1, 
     #x4)
      t1165 = -(0.90D2 * t7 * (t1136 - t737 * t1137) - 0.180D3 * t61 * t
     #1142) * t111 * t138 / 0.2880D4 + (-0.180D3 * t61 * (t754 * t1137 -
     # t1136) + 0.90D2 * t7 * (t754 * t1135 - t762 * t1137 / 0.2D1 - t73
     #6 * t1155) - t105 * t1142) * t137 * t58 / 0.2880D4
      t1166 = FJET(XB1, XB2, s, -t725, t479, t723, 0.0D0, t727, t1165)
      t1168 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t666, -t667,
     # x4)
      t1170 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, t666, -t667,
     # x4)
      t1171 = t665 * t1170
      t1175 = t665 * t1168
      t1186 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.10D1, t666, -t667,
     # x4)
      t1199 = -(0.90D2 * t7 * (-t678 * t1168 + t1171) - 0.180D3 * t61 * 
     #t1175) * t111 * t138 / 0.2880D4 - (-0.180D3 * t61 * (-t697 * t1168
     # + t1171) + 0.90D2 * t7 * (t665 * t1186 - t697 * t1170 + t705 * t1
     #168 / 0.2D1) + t105 * t1175) * t111 * t137 / 0.5760D4
      t1200 = FJET(XB1, XB2, s, -t656, 0.0D0, t651, 0.0D0, 0.0D0, t1199)
      bbarbbarH3n1e1 = t317 * t316 + t477 * t476 + t637 * t636 + t718 * 
     #t717 + t775 * t774 + t873 * t872 + t964 * t963 + t1033 * t1032 - t
     #1110 * t1106 * t1113 / 0.2880D4 - t1131 * t1127 * t1113 / 0.2880D4
     # + t1166 * t1165 + t1200 * t1199

      end function



      doubleprecision function bbarbbarH3n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
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
      t21 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t23 = -0.1D1 + x3
      t24 = 0.1D1 / t23
      t25 = t16 * t24
      t28 = log(-0.4D1 * t10 * t25)
      t31 = cos(t13)
      t33 = Sqrt(-x3 * t23)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t33 * z)
      t43 = lh * t5
      t44 = t6 * t21
      t46 = -t44 - t21 * t38
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
      t90 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t91 = t82 ** 2
      t97 = lh ** 2
      t99 = 0.3141592653589793D1 ** 2
      t101 = 0.180D3 * t97 - 0.30D2 * t99
      t102 = t101 * t5
      t103 = t102 * t44
      t112 = x3 * t15
      t116 = log(-0.4D1 * t112 * t12 * t24)
      t120 = log(0.4D1 * t112 * t12)
      t122 = t116 * t38 + t120 * t6
      t125 = t120 ** 2
      t127 = t116 ** 2
      t130 = -t125 * t6 / 0.2D1 - t127 * t38 / 0.2D1
      t139 = -t38 - t6
      t144 = t62 * x3
      t147 = log(-0.4D1 * t144 * t25)
      t153 = log(0.4D1 * t144 * t16)
      t154 = t153 * t6
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
      t218 = (-t189 * t101 - 0.90D2 * t199 * lh + t6 * (-0.2884936567583
     #026D3 - 0.120D3 * t97 * lh + 0.60D2 * lh * t99) - 0.15D2 * t198 * 
     #t188 * t6) * t5
      t221 = (0.90D2 * t5 * (-t8 + t20 * t21 - (t7 - t28 * t21) * t38) -
     # 0.180D3 * t43 * t46) * t50 * t52 / 0.5760D4 - t5 * t55 * t59 / 0.
     #32D2 + (0.90D2 * t5 * (-t8 + t67 * t21) + 0.180D3 * t43 * t44) * t
     #57 * t52 / 0.2880D4 + (-0.180D3 * t78 * t5 * (-t7 + t82 * t21) + 0
     #.90D2 * t88 * (t82 * t7 - t90 - t91 * t21 / 0.2D1) - t103) * t52 /
     # 0.5760D4 + ((0.90D2 * t5 * t7 - 0.180D3 * t43 * t21) * t122 + 0.9
     #0D2 * t5 * t21 * t130 + (0.90D2 * t5 * t90 - 0.180D3 * t43 * t7 + 
     #t102 * t21) * t139) * t50 / 0.11520D5 - (0.90D2 * t5 * ((t7 - t147
     # * t21) * t38 + t8 - t154 * t21) - 0.180D3 * t43 * t55) * t50 * t5
     #7 / 0.5760D4 + (-0.180D3 * t43 * (t169 * t21 - t8) + 0.90D2 * t5 *
     # (-t6 * t90 + t169 * t7 - t177 * t21 / 0.2D1) - t103) * t57 / 0.57
     #60D4 - t192 * t90 / 0.11520D5 - t202 * t7 / 0.11520D5 - t218 * t21
     # / 0.11520D5
      t222 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t221)
      t224 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t225 = t6 * t224
      t226 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t234 = t6 * t226
      t236 = -t234 - t226 * t38
      t243 = -t236
      t264 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t269 = t102 * t234
      t325 = (0.90D2 * t5 * (-t225 + t20 * t226 - (t224 - t28 * t226) * 
     #t38) - 0.180D3 * t43 * t236) * t50 * t52 / 0.5760D4 - t5 * t243 * 
     #t59 / 0.32D2 + (0.90D2 * t5 * (-t225 + t67 * t226) + 0.180D3 * t43
     # * t234) * t57 * t52 / 0.2880D4 + (-0.180D3 * t78 * t5 * (-t224 + 
     #t82 * t226) + 0.90D2 * t88 * (-t91 * t226 / 0.2D1 - t264 + t82 * t
     #224) - t269) * t52 / 0.5760D4 + ((0.90D2 * t5 * t224 - 0.180D3 * t
     #43 * t226) * t122 + 0.90D2 * t5 * t226 * t130 + (0.90D2 * t5 * t26
     #4 - 0.180D3 * t43 * t224 + t102 * t226) * t139) * t50 / 0.11520D5 
     #- (0.90D2 * t5 * ((t224 - t147 * t226) * t38 + t225 - t154 * t226)
     # - 0.180D3 * t43 * t243) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 
     #* (-t225 + t169 * t226) + 0.90D2 * t5 * (-t6 * t264 + t169 * t224 
     #- t177 * t226 / 0.2D1) - t269) * t57 / 0.5760D4 - t192 * t264 / 0.
     #11520D5 - t202 * t224 / 0.11520D5 - t218 * t226 / 0.11520D5
      t326 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t325)
      t328 = t2 * x1
      t329 = -0.1D1 + x1
      t330 = x1 * z
      t331 = 0.1D1 - x1 + t330
      t332 = 0.1D1 / t331
      t334 = t2 * t329 * t332
      t335 = t1 ** 2
      t336 = s * t335
      t338 = x1 * t329 * t332
      t339 = t336 * t338
      t340 = -t329
      t341 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, t340, 0.10D1, 0.10D1,
     # x4)
      t342 = t6 * t341
      t343 = t10 * t15
      t344 = t12 * t332
      t345 = t329 ** 2
      t346 = t344 * t345
      t349 = log(0.4D1 * t343 * t346)
      t350 = t349 * t6
      t351 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t340, 0.10D1, 0.10D1,
     # x4)
      t357 = log(-0.4D1 * t343 * t344 * t345 * t24)
      t361 = x1 * x3
      t362 = 0.2D1 * t361
      t363 = x1 * t11
      t364 = x3 * t11
      t365 = t364 * x1
      t367 = 0.2D1 * t10 * z
      t368 = t10 * t11
      t369 = t361 * z
      t370 = 0.3D1 * t369
      t371 = x3 * t331
      t373 = Sqrt(-t371 * t23)
      t377 = -t10 - z + t362 - x3 + t330 - t363 + t365 + t367 - t368 - t
     #370 + 0.2D1 * t31 * t373 * z
      t378 = 0.1D1 / t377
      t385 = t6 * t351
      t386 = t351 * t331 * t378 + t385
      t397 = t63 * t15
      t400 = log(0.4D1 * t397 * t346)
      t401 = t400 * t6
      t414 = log(0.4D1 * t79 * t346)
      t420 = t414 ** 2
      t423 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, t340, 0.10D1, 0.10D1,
     # x4)
      t432 = (0.90D2 * t5 * (t342 - t350 * t351 + (t341 - t357 * t351) *
     # t331 * t378) - 0.180D3 * t43 * t386) * t50 * t52 / 0.5760D4 + t5 
     #* t386 * t59 / 0.32D2 + (0.90D2 * t5 * (t342 - t401 * t351) - 0.18
     #0D3 * t43 * t385) * t57 * t52 / 0.2880D4 + (-0.180D3 * t78 * t5 * 
     #(t341 - t414 * t351) + 0.90D2 * t88 * (t420 * t351 / 0.2D1 + t423 
     #- t414 * t341) + t102 * t385) * t52 / 0.5760D4
      t433 = FJET(XB1, XB2, s, 0.0D0, t328, -t334, 0.0D0, -t339, t432)
      t435 = x2 * x3
      t436 = -0.1D1 + x2
      t439 = Sqrt(x3 * t436 * t23)
      t440 = t31 * t439
      t442 = 0.2D1 * t440 * x2
      t444 = 0.1D1 - x3 + t435
      t445 = 0.1D1 / t444
      t447 = t2 * (0.1D1 - x3 - x2 + t435 + t144 + t442) * t445
      t452 = t2 * x2 * (-0.1D1 + t435 + 0.2D1 * t440) * t445
      t453 = x2 * z
      t454 = t435 * z
      t455 = t144 * z
      t461 = 0.1D1 / (z - t453 + x2 + x3 - t144 - t454 + t455 - t442 - 0
     #.2D1 * t440 * z + 0.2D1 * t440 * t453)
      t462 = t5 * t461
      t463 = -t436
      t464 = t23 * t445
      t465 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t463, -t464, 
     #x4)
      t469 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, t463, -t464, 
     #x4)
      t473 = t444 ** 2
      t479 = log(0.4D1 * t144 * t12 * t15 * t436 * t23 / t473)
      t480 = t479 * t461
      t492 = -t462 * t465 * t59 / 0.32D2 - (0.90D2 * t5 * (t461 * t469 -
     # t480 * t465) - 0.180D3 * t43 * t461 * t465) * t50 * t57 / 0.5760D
     #4
      t493 = FJET(XB1, XB2, s, 0.0D0, t447, 0.0D0, -t452, 0.0D0, t492)
      t495 = t436 * s
      t496 = t1 * t329
      t498 = t495 * t496 * t332
      t499 = x2 * s
      t500 = t496 * t499
      t502 = t336 * t436 * t338
      t503 = x2 * x1
      t504 = t503 * z
      t506 = 0.1D1 / (z - t453 - t503 + x2 + t504)
      t507 = t5 * t506
      t508 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t340, t463, 0.10D1, x
     #4)
      t516 = log(-0.4D1 * t397 * t344 * t345 * t436)
      t517 = t516 * t506
      t519 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, t340, t463, 0.10D1, x
     #4)
      t531 = -t507 * t508 * t59 / 0.32D2 + (0.90D2 * t5 * (t517 * t508 -
     # t506 * t519) + 0.180D3 * t43 * t506 * t508) * t57 * t52 / 0.2880D
     #4
      t532 = FJET(XB1, XB2, s, 0.0D0, t498, t328, -t500, t502, t531)
      t534 = t495 * t1
      t535 = t499 * t1
      t537 = 0.1D1 / (-z + t453 - x2)
      t538 = t5 * t537
      t539 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t463, 0.10D1,
     # x4)
      t543 = t16 * t436
      t546 = log(-0.4D1 * t63 * t543)
      t547 = t546 * t537
      t549 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, t463, 0.10D1,
     # x4)
      t550 = t537 * t549
      t554 = t537 * t539
      t556 = 0.180D3 * t43 * t554
      t563 = log(-0.4D1 * t144 * t543)
      t564 = t563 * t537
      t576 = log(-0.4D1 * t165 * t12 * t436)
      t577 = t576 * t537
      t582 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.10D1, t463, 0.10D1,
     # x4)
      t585 = t576 ** 2
      t586 = t585 * t537
      t596 = -t538 * t539 * t59 / 0.32D2 + (0.90D2 * t5 * (t547 * t539 -
     # t550) + t556) * t57 * t52 / 0.2880D4 - (0.90D2 * t5 * (-t564 * t5
     #39 + t550) - t556) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 * (t57
     #7 * t539 - t550) + 0.90D2 * t5 * (-t537 * t582 + t577 * t549 - t58
     #6 * t539 / 0.2D1) - t102 * t554) * t57 / 0.5760D4
      t597 = FJET(XB1, XB2, s, 0.0D0, -t534, 0.0D0, t535, 0.0D0, t596)
      t599 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, t340, 0.10D1, 0.10D1,
     # x4)
      t600 = t6 * t599
      t601 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t340, 0.10D1, 0.10D1,
     # x4)
      t612 = t6 * t601
      t613 = t601 * t331 * t378 + t612
      t640 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, t340, 0.10D1, 0.10D1,
     # x4)
      t650 = (0.90D2 * t5 * (t600 - t350 * t601 + (t599 - t357 * t601) *
     # t331 * t378) - 0.180D3 * t43 * t613) * t50 * t52 / 0.5760D4 + t5 
     #* t613 * t59 / 0.32D2 + (0.90D2 * t5 * (-t401 * t601 + t600) - 0.1
     #80D3 * t43 * t612) * t57 * t52 / 0.2880D4 + (-0.180D3 * t78 * t5 *
     # (t599 - t414 * t601) + 0.90D2 * t88 * (-t414 * t599 + t640 + t420
     # * t601 / 0.2D1) + t102 * t612) * t52 / 0.5760D4
      t651 = FJET(XB1, XB2, s, 0.0D0, -t334, t328, 0.0D0, -t339, t650)
      t653 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t463, 0.10D1,
     # x4)
      t657 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, t463, 0.10D1,
     # x4)
      t658 = t537 * t657
      t663 = t537 * t653
      t665 = 0.180D3 * t43 * t663
      t682 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.10D1, t463, 0.10D1,
     # x4)
      t694 = -t538 * t653 * t59 / 0.32D2 + (0.90D2 * t5 * (-t658 + t547 
     #* t653) + t665) * t57 * t52 / 0.2880D4 - (0.90D2 * t5 * (t658 - t5
     #64 * t653) - t665) * t50 * t57 / 0.5760D4 + (-0.180D3 * t43 * (t57
     #7 * t653 - t658) + 0.90D2 * t5 * (-t537 * t682 + t577 * t657 - t58
     #6 * t653 / 0.2D1) - t102 * t663) * t57 / 0.5760D4
      t695 = FJET(XB1, XB2, s, t535, 0.0D0, -t534, 0.0D0, 0.0D0, t694)
      t698 = t328 * t435 * t445
      t699 = t2 * t329
      t700 = t144 * x1
      t703 = Sqrt(t371 * t436 * t23)
      t704 = t31 * t703
      t706 = 0.2D1 * t704 * x2
      t707 = t144 * t330
      t711 = t699 * (t144 - x2 + t435 + 0.1D1 - x3 - t700 + t706 + t707)
     # * t332 * t445
      t715 = t23 * s * t1 * x1 * t445
      t721 = t699 * x2 * (-0.1D1 + t435 + x1 - t361 - t330 + t369 + 0.2D
     #1 * t704) * t332 * t445
      t727 = 0.2D1 * t503 - t10 + t362 - t363 + t144 + t453 + t330 + t36
     #5 + t367 - t368 - t370 - x2 - t700 + t706 + t10 * x2 - t435 * x1 +
     # t503 * t11
      t728 = x2 * t9
      t748 = 0.2D1 * t728 * z - t728 * t11 + 0.2D1 * t704 * z + t707 - 0
     #.2D1 * t704 * t453 - 0.2D1 * t704 * t503 - t364 * t503 - 0.2D1 * t
     #10 * t453 + t10 * t11 * x2 + 0.2D1 * t435 * t330 + 0.2D1 * t704 * 
     #t504 - x3 - z + t454 - t455 - 0.3D1 * t504 - t728
      t750 = 0.1D1 / (t727 + t748)
      t751 = t5 * t331 * t750
      t752 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t340, t463, -t464, x4
     #)
      t754 = t57 * t52
      t755 = t752 * t50 * t754
      t758 = FJET(XB1, XB2, s, t698, -t711, -t715, t721, t502, -t751 * t
     #755 / 0.32D2)
      t760 = t331 * t750
      t764 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t340, t463, -t464, x4
     #)
      t766 = t764 * t50 * t754
      t769 = FJET(XB1, XB2, s, t721, -t715, -t711, t698, t502, -t751 * t
     #766 / 0.32D2)
      t774 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t340, t463, 0.10D1, x
     #4)
      t779 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, t340, t463, 0.10D1, x
     #4)
      t791 = -t507 * t774 * t59 / 0.32D2 + (0.90D2 * t5 * (t517 * t774 -
     # t506 * t779) + 0.180D3 * t43 * t506 * t774) * t57 * t52 / 0.2880D
     #4
      t792 = FJET(XB1, XB2, s, -t500, t328, t498, 0.0D0, t502, t791)
      t794 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t463, -t464, 
     #x4)
      t799 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, t463, -t464, 
     #x4)
      t811 = -t462 * t794 * t59 / 0.32D2 - (0.90D2 * t5 * (-t480 * t794 
     #+ t461 * t799) - 0.180D3 * t43 * t461 * t794) * t50 * t57 / 0.5760
     #D4
      t812 = FJET(XB1, XB2, s, -t452, 0.0D0, t447, 0.0D0, 0.0D0, t811)
      bbarbbarH3n1e0 = t222 * t221 + t326 * t325 + t433 * t432 + t493 * 
     #t492 + t532 * t531 + t597 * t596 + t651 * t650 + t695 * t694 - t75
     #8 * t5 * t760 * t755 / 0.32D2 - t769 * t5 * t760 * t766 / 0.32D2 +
     # t792 * t791 + t812 * t811

      end function



      doubleprecision function bbarbbarH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t18 = log(0.4D1 * t13 * t15)
      t19 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t24 = t6 * lh
      t25 = t3 * t19
      t27 = 0.180D3 * t24 * t25
      t29 = 0.1D1 / x1
      t32 = cos(t10)
      t33 = -0.1D1 + x3
      t35 = Sqrt(-x3 * t33)
      t40 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t35 * z)
      t42 = -t25 - t19 * t40
      t44 = 0.1D1 / x3
      t45 = t44 * t29
      t48 = 0.1D1 / x2
      t55 = t44 * t48
      t58 = x2 ** 2
      t59 = t58 * t12
      t62 = log(0.4D1 * t59 * t15)
      t63 = t62 * t3
      t72 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
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
      t113 = t107 * t40 + t111 * t3
      t121 = -t40 - t3
      t126 = (0.90D2 * t7 * (-t8 + t18 * t19) + t27) * t29 / 0.5760D4 + 
     #t6 * t42 * t45 / 0.64D2 - t7 * t19 * t48 * t29 / 0.32D2 + t6 * t42
     # * t55 / 0.64D2 + (0.90D2 * t6 * (t63 * t19 - t3 * t8) + t27) * t4
     #8 / 0.5760D4 - t7 * t72 / 0.128D3 - t83 * t8 / 0.11520D5 - t98 * t
     #19 / 0.11520D5 + (0.90D2 * t6 * t19 * t113 + (0.90D2 * t6 * t8 - 0
     #.180D3 * t24 * t19) * t121) * t44 / 0.11520D5
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t126)
      t129 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t130 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t135 = t3 * t130
      t137 = 0.180D3 * t24 * t135
      t142 = -t135 - t130 * t40
      t162 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D
     #1, x4)
      t181 = (0.90D2 * t7 * (-t129 + t18 * t130) + t137) * t29 / 0.5760D
     #4 + t6 * t142 * t45 / 0.64D2 - t7 * t130 * t48 * t29 / 0.32D2 + t6
     # * t142 * t55 / 0.64D2 + (0.90D2 * t6 * (-t3 * t129 + t63 * t130) 
     #+ t137) * t48 / 0.5760D4 - t7 * t162 / 0.128D3 - t83 * t129 / 0.11
     #520D5 - t98 * t130 / 0.11520D5 + (0.90D2 * t6 * t130 * t113 + (0.9
     #0D2 * t6 * t129 - 0.180D3 * t24 * t130) * t121) * t44 / 0.11520D5
      t182 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t181)
      t184 = t2 * x1
      t185 = -0.1D1 + x1
      t186 = x1 * z
      t187 = 0.1D1 - x1 + t186
      t188 = 0.1D1 / t187
      t190 = t2 * t185 * t188
      t191 = t1 ** 2
      t192 = s * t191
      t194 = x1 * t185 * t188
      t195 = t192 * t194
      t196 = -t185
      t197 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, t196, 0.10D1, 0.10D1,
     # x4)
      t199 = t185 ** 2
      t203 = log(0.4D1 * t13 * t15 * t188 * t199)
      t204 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t196, 0.10D1, 0.10D1,
     # x4)
      t209 = t3 * t204
      t216 = t9 * x3
      t217 = x1 * x3
      t229 = Sqrt(-x3 * t187 * t33)
      t233 = -t216 - z + 0.2D1 * t217 - x3 + t186 - x1 * t14 + x3 * t14 
     #* x1 + 0.2D1 * t216 * z - t216 * t14 - 0.3D1 * t217 * z + 0.2D1 * 
     #t32 * t229 * z
      t234 = 0.1D1 / t233
      t244 = (0.90D2 * t7 * (t197 - t203 * t204) - 0.180D3 * t24 * t209)
     # * t29 / 0.5760D4 + t6 * (t204 * t187 * t234 + t209) * t45 / 0.64D
     #2 + t7 * t204 * t48 * t29 / 0.32D2
      t245 = FJET(XB1, XB2, s, 0.0D0, t184, -t190, 0.0D0, -t195, t244)
      t247 = x2 * x3
      t248 = t58 * x3
      t249 = -0.1D1 + x2
      t252 = Sqrt(x3 * t249 * t33)
      t253 = t32 * t252
      t255 = 0.2D1 * t253 * x2
      t258 = 0.1D1 / (0.1D1 - x3 + t247)
      t260 = t2 * (0.1D1 - x3 - x2 + t247 + t248 + t255) * t258
      t265 = t2 * x2 * (-0.1D1 + t247 + 0.2D1 * t253) * t258
      t266 = x2 * z
      t274 = 0.1D1 / (z - t266 + x2 + x3 - t248 - t247 * z + t248 * z - 
     #t255 - 0.2D1 * t253 * z + 0.2D1 * t253 * t266)
      t275 = t6 * t274
      t276 = -t249
      t277 = t33 * t258
      t278 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, -t277, 
     #x4)
      t280 = t278 * t44 * t48
      t283 = FJET(XB1, XB2, s, 0.0D0, t260, 0.0D0, -t265, 0.0D0, -t275 *
     # t280 / 0.64D2)
      t288 = t249 * s
      t289 = t1 * t185
      t291 = t288 * t289 * t188
      t292 = x2 * s
      t293 = t292 * t289
      t295 = t192 * t249 * t194
      t296 = x2 * x1
      t299 = 0.1D1 / (z - t266 - t296 + x2 + t296 * z)
      t300 = t6 * t299
      t301 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t196, t276, 0.10D1, x
     #4)
      t303 = t301 * t48 * t29
      t306 = FJET(XB1, XB2, s, 0.0D0, t291, t184, -t293, t295, -t300 * t
     #303 / 0.32D2)
      t311 = t288 * t1
      t312 = t292 * t1
      t314 = 0.1D1 / (-z + t266 - x2)
      t315 = t6 * t314
      t316 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1,
     # x4)
      t328 = log(-0.4D1 * t59 * t15 * t249)
      t329 = t328 * t314
      t331 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1,
     # x4)
      t342 = -t315 * t316 * t48 * t29 / 0.32D2 - t315 * t316 * t44 * t48
     # / 0.64D2 + (0.90D2 * t6 * (t329 * t316 - t314 * t331) + 0.180D3 *
     # t24 * t314 * t316) * t48 / 0.5760D4
      t343 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t312, 0.0D0, t342)
      t345 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, t196, 0.10D1, 0.10D1,
     # x4)
      t346 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t196, 0.10D1, 0.10D1,
     # x4)
      t351 = t3 * t346
      t367 = (0.90D2 * t7 * (t345 - t203 * t346) - 0.180D3 * t24 * t351)
     # * t29 / 0.5760D4 + t6 * (t346 * t187 * t234 + t351) * t45 / 0.64D
     #2 + t7 * t346 * t48 * t29 / 0.32D2
      t368 = FJET(XB1, XB2, s, 0.0D0, -t190, t184, 0.0D0, -t195, t367)
      t370 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1,
     # x4)
      t380 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1,
     # x4)
      t391 = -t315 * t370 * t48 * t29 / 0.32D2 - t315 * t370 * t44 * t48
     # / 0.64D2 + (0.90D2 * t6 * (t329 * t370 - t314 * t380) + 0.180D3 *
     # t24 * t314 * t370) * t48 / 0.5760D4
      t392 = FJET(XB1, XB2, s, t312, 0.0D0, -t311, 0.0D0, 0.0D0, t391)
      t394 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t196, t276, 0.10D1, x
     #4)
      t396 = t394 * t48 * t29
      t399 = FJET(XB1, XB2, s, -t293, t184, t291, 0.0D0, t295, -t300 * t
     #396 / 0.32D2)
      t404 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, -t277, 
     #x4)
      t406 = t404 * t44 * t48
      t409 = FJET(XB1, XB2, s, -t265, 0.0D0, t260, 0.0D0, 0.0D0, -t275 *
     # t406 / 0.64D2)
      bbarbbarH3n1em1 = t127 * t126 + t182 * t181 + t245 * t244 - t283 *
     # t6 * t274 * t280 / 0.64D2 - t306 * t6 * t299 * t303 / 0.32D2 + t3
     #43 * t342 + t368 * t367 + t392 * t391 - t399 * t6 * t299 * t396 / 
     #0.32D2 - t409 * t6 * t274 * t406 / 0.64D2

      end function



      doubleprecision function bbarbbarH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t6 * t5
      t8 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x2
      t17 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t22 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t22 * t26)
      t33 = (-0.180D3 * t6 * lh - 0.90D2 * t29 * t6) * t5
      t37 = cos(t24)
      t40 = Sqrt(-x3 * (-0.1D1 + x3))
      t48 = (-0.1D1 / (-z - x3 + 0.2D1 * t37 * t40 * z) - t6) / x3
      t51 = -t7 * t8 * t9 / 0.64D2 - t7 * t8 * t13 / 0.64D2 - t7 * t17 /
     # 0.128D3 - t33 * t8 / 0.11520D5 + t5 * t8 * t48 / 0.128D3
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t51)
      t54 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t61 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t69 = -t7 * t54 * t9 / 0.64D2 - t7 * t54 * t13 / 0.64D2 - t7 * t61
     # / 0.128D3 - t33 * t54 / 0.11520D5 + t5 * t54 * t48 / 0.128D3
      t70 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t69)
      t72 = t2 * x1
      t73 = -0.1D1 + x1
      t76 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t78 = t2 * t73 * t76
      t79 = t1 ** 2
      t83 = s * t79 * x1 * t73 * t76
      t84 = -t73
      t85 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, t84, 0.10D1, 0.10D1, x
     #4)
      t89 = FJET(XB1, XB2, s, 0.0D0, t72, -t78, 0.0D0, -t83, t7 * t85 * 
     #t9 / 0.64D2)
      t95 = -0.1D1 + x2
      t97 = t95 * s * t1
      t99 = x2 * s * t1
      t102 = 0.1D1 / (-z + x2 * z - x2)
      t103 = t5 * t102
      t104 = -t95
      t105 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, t104, 0.10D1,
     # x4)
      t109 = FJET(XB1, XB2, s, 0.0D0, -t97, 0.0D0, t99, 0.0D0, -t103 * t
     #105 * t13 / 0.64D2)
      t115 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, t84, 0.10D1, 0.10D1, 
     #x4)
      t119 = FJET(XB1, XB2, s, 0.0D0, -t78, t72, 0.0D0, -t83, t7 * t115 
     #* t9 / 0.64D2)
      t125 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, t104, 0.10D1,
     # x4)
      t129 = FJET(XB1, XB2, s, t99, 0.0D0, -t97, 0.0D0, 0.0D0, -t103 * t
     #125 * t13 / 0.64D2)
      bbarbbarH3n1em2 = t52 * t51 + t70 * t69 + t89 * t5 * t6 * t85 * t9
     # / 0.64D2 - t109 * t5 * t102 * t105 * t13 / 0.64D2 + t119 * t6 * t
     #5 * t115 * t9 / 0.64D2 - t129 * t5 * t102 * t125 * t13 / 0.64D2

      end function



      doubleprecision function bbarbbarH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = t6 * t5
      t8 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7 * t8 /
     # 0.128D3)
      t15 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t18 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7 * t15 
     #/ 0.128D3)
      bbarbbarH3n1em3 = -t11 * t5 * t6 * t8 / 0.128D3 - t18 * t5 * t6 * 
     #t15 / 0.128D3

      end function



      doubleprecision function bbarbbarH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      bbarbbarH3n1em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH3n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = t3 * lh
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = t13 * t16
      t19 = log(0.4D1 * t17)
      t20 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t22 = t19 ** 2
      t23 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t30 = lh ** 2
      t32 = 0.3141592653589793D1 ** 2
      t34 = 0.180D3 * t30 - 0.30D2 * t32
      t35 = t3 * t34
      t40 = t3 * t7
      t44 = t22 * t19
      t54 = -0.2884936567583026D3 - 0.120D3 * t30 * lh + 0.60D2 * lh * t
     #32
      t55 = t3 * t54
      t56 = t7 * t23
      t59 = 0.1D1 / x1
      t62 = lh * t7
      t63 = t3 * t20
      t64 = t9 * x3
      t65 = t12 * t16
      t66 = -0.1D1 + x3
      t67 = 0.1D1 / t66
      t68 = t65 * t67
      t71 = log(-0.4D1 * t64 * t68)
      t74 = cos(t10)
      t75 = x3 * z
      t77 = Sqrt(-t75 * t66)
      t81 = 0.1D1 / (-z - x3 + 0.2D1 * t74 * t77)
      t85 = log(0.4D1 * t64 * t65)
      t86 = t85 * t3
      t92 = t71 ** 2
      t97 = t3 * t8
      t99 = t85 ** 2
      t100 = t99 * t3
      t106 = t34 * t7
      t107 = t3 * t23
      t108 = t23 * t81
      t112 = 0.1D1 / x3
      t116 = x2 ** 2
      t117 = x3 * t116
      t118 = t117 * t9
      t121 = log(-0.4D1 * t118 * t68)
      t125 = 0.1D1 - x2
      t126 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, t125, 0.10D1, 
     #x4)
      t127 = t3 * t126
      t128 = -t125
      t129 = t65 * t128
      t132 = log(-0.4D1 * t118 * t129)
      t133 = t132 * t3
      t134 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t125, 0.10D1, 
     #x4)
      t138 = log(0.4D1 * t117 * t17)
      t139 = t138 * t3
      t144 = t3 * t134
      t145 = -t107 - t108 + t144
      t150 = 0.1D1 / x2
      t151 = t150 * t59
      t154 = t116 * t9
      t157 = log(-0.4D1 * t154 * t129)
      t158 = t157 * t3
      t162 = log(0.4D1 * t154 * t65)
      t163 = t162 * t3
      t168 = t157 ** 2
      t169 = t168 * t3
      t172 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.0D0, t125, 0.10D1, 
     #x4)
      t173 = t3 * t172
      t176 = t162 ** 2
      t177 = t176 * t3
      t190 = log(0.4D1 * t65)
      t191 = t190 * t3
      t194 = t190 ** 2
      t195 = t194 * t3
      t198 = (0.180D3 * t191 * lh + t35 + 0.45D2 * t195) * t7
      t205 = t194 * t190 * t3
      t208 = (-t191 * t34 - 0.90D2 * t195 * lh + t55 - 0.15D2 * t205) * 
     #t7
      t211 = t194 ** 2
      t216 = t32 ** 2
      t217 = t30 ** 2
      t228 = (0.15D2 / 0.4D1 * t211 * t3 - t191 * t54 + t3 * (0.57698731
     #35166051D3 * lh + t216 + 0.60D2 * t217 - 0.60D2 * t30 * t32) + t19
     #5 * t34 / 0.2D1 + 0.30D2 * t205 * lh) * t7
      t236 = x3 * t16
      t239 = log(0.4D1 * t236 * t12)
      t240 = t239 ** 2
      t245 = log(-0.4D1 * t236 * t12 * t67)
      t246 = t245 ** 2
      t249 = t240 * t3 / 0.2D1 + t246 * t81 / 0.2D1
      t259 = -t245 * t81 - t239 * t3
      t266 = -t240 * t239 * t3 / 0.6D1 - t246 * t245 * t81 / 0.6D1
      t272 = t54 * t7
      t275 = t81 + t3
      t280 = t16 * t116
      t281 = t12 * t128
      t284 = log(-0.4D1 * t280 * t281)
      t286 = t284 ** 2
      t291 = log(0.4D1 * t280 * t12)
      t293 = t291 ** 2
      t308 = t286 * t284
      t314 = t293 * t291
      t328 = log(-0.4D1 * t117 * t68)
      t334 = log(0.4D1 * t117 * t65)
      t335 = t334 * t3
      t339 = log(-0.4D1 * t117 * t129)
      t340 = t339 * t3
      t346 = t328 ** 2
      t353 = t334 ** 2
      t354 = t353 * t3
      t357 = t339 ** 2
      t358 = t357 * t3
      t370 = -(-0.180D3 * t4 * t7 * (t8 - t19 * t20 + t22 * t23 / 0.2D1)
     # + t35 * t7 * (-t19 * t23 + t20) + 0.90D2 * t40 * (-t19 * t8 + t22
     # * t20 / 0.2D1 - t44 * t23 / 0.6D1) + t55 * t56) * t59 / 0.5760D4 
     #- (-0.180D3 * t62 * (t63 + (t20 - t71 * t23) * t81 - t86 * t23) + 
     #0.90D2 * t7 * ((t8 - t71 * t20 + t92 * t23 / 0.2D1) * t81 + t97 - 
     #t86 * t20 + t100 * t23 / 0.2D1) + t106 * (t107 + t108)) * t112 * t
     #59 / 0.5760D4 + (0.90D2 * t7 * (-(t20 - t121 * t23) * t81 + t127 -
     # t63 - t133 * t134 + t139 * t23) - 0.180D3 * t62 * t145) * t112 * 
     #t151 / 0.2880D4 + (-0.180D3 * t62 * (-t158 * t134 + t127 + t163 * 
     #t23 - t63) + 0.90D2 * t7 * (t169 * t134 / 0.2D1 - t97 + t173 + t16
     #3 * t20 - t158 * t126 - t177 * t23 / 0.2D1) + t106 * (-t107 + t144
     #)) * t150 * t59 / 0.2880D4 - t198 * t8 / 0.11520D5 - t208 * t20 / 
     #0.11520D5 - t228 * t23 / 0.11520D5 - ((0.90D2 * t7 * t20 - 0.180D3
     # * t62 * t23) * t249 + (0.90D2 * t7 * t8 - 0.180D3 * t62 * t20 + t
     #106 * t23) * t259 + 0.90D2 * t56 * t266 + (-0.180D3 * t62 * t8 + t
     #106 * t20 + t272 * t23) * t275) * t112 / 0.11520D5 - (-0.180D3 * t
     #4 * t7 * (-t172 + t284 * t126 - t286 * t134 / 0.2D1 + t8 - t291 * 
     #t20 + t293 * t23 / 0.2D1) + t35 * t7 * (t20 - t291 * t23 - t126 + 
     #t284 * t134) + 0.90D2 * t40 * (t284 * t172 - t286 * t126 / 0.2D1 +
     # t308 * t134 / 0.6D1 - t291 * t8 + t293 * t20 / 0.2D1 - t314 * t23
     # / 0.6D1) + t55 * t7 * (t23 - t134)) * t150 / 0.5760D4 - (-0.180D3
     # * t62 * (t63 + (t20 - t328 * t23) * t81 - t335 * t23 + t340 * t13
     #4 - t127) + 0.90D2 * t7 * ((t8 - t328 * t20 + t346 * t23 / 0.2D1) 
     #* t81 + t97 - t173 - t335 * t20 + t340 * t126 + t354 * t23 / 0.2D1
     # - t358 * t134 / 0.2D1) - t106 * t145) * t112 * t150 / 0.5760D4
      t371 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t370)
      t373 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t374 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t376 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t395 = t7 * t376
      t403 = t3 * t374
      t416 = t3 * t373
      t420 = t376 * t81
      t421 = t3 * t376
      t431 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t125, 0.10D1, 
     #x4)
      t434 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, t125, 0.10D1, 
     #x4)
      t435 = t3 * t434
      t439 = t3 * t431
      t440 = t439 - t421 - t420
      t452 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.0D0, t125, 0.10D1, 
     #x4)
      t453 = t3 * t452
      t561 = -(-0.180D3 * t4 * t7 * (t373 - t19 * t374 + t22 * t376 / 0.
     #2D1) + t35 * t7 * (-t19 * t376 + t374) + 0.90D2 * t40 * (-t19 * t3
     #73 + t22 * t374 / 0.2D1 - t44 * t376 / 0.6D1) + t55 * t395) * t59 
     #/ 0.5760D4 - (-0.180D3 * t62 * ((t374 - t71 * t376) * t81 + t403 -
     # t86 * t376) + 0.90D2 * t7 * ((t373 - t71 * t374 + t92 * t376 / 0.
     #2D1) * t81 - t86 * t374 + t100 * t376 / 0.2D1 + t416) + t106 * (t4
     #20 + t421)) * t112 * t59 / 0.5760D4 + (0.90D2 * t7 * (-t403 - (t37
     #4 - t121 * t376) * t81 - t133 * t431 + t139 * t376 + t435) - 0.180
     #D3 * t62 * t440) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (t43
     #5 - t158 * t431 - t403 + t163 * t376) + 0.90D2 * t7 * (t453 - t177
     # * t376 / 0.2D1 + t169 * t431 / 0.2D1 + t163 * t374 - t158 * t434 
     #- t416) + t106 * (t439 - t421)) * t150 * t59 / 0.2880D4 - t208 * t
     #374 / 0.11520D5 - t228 * t376 / 0.11520D5 - ((0.90D2 * t7 * t374 -
     # 0.180D3 * t62 * t376) * t249 + (0.90D2 * t7 * t373 - 0.180D3 * t6
     #2 * t374 + t106 * t376) * t259 + 0.90D2 * t395 * t266 + (-0.180D3 
     #* t62 * t373 + t106 * t374 + t272 * t376) * t275) * t112 / 0.11520
     #D5 - t198 * t373 / 0.11520D5 - (-0.180D3 * t4 * t7 * (-t452 + t284
     # * t434 - t286 * t431 / 0.2D1 + t373 - t291 * t374 + t293 * t376 /
     # 0.2D1) + t35 * t7 * (t374 - t291 * t376 - t434 + t284 * t431) + 0
     #.90D2 * t40 * (t284 * t452 - t286 * t434 / 0.2D1 + t308 * t431 / 0
     #.6D1 - t291 * t373 + t293 * t374 / 0.2D1 - t314 * t376 / 0.6D1) + 
     #t55 * t7 * (t376 - t431)) * t150 / 0.5760D4 - (-0.180D3 * t62 * (t
     #403 - t335 * t376 + (t374 - t328 * t376) * t81 - t435 + t340 * t43
     #1) + 0.90D2 * t7 * (t416 - t453 + (t373 - t328 * t374 + t346 * t37
     #6 / 0.2D1) * t81 + t354 * t376 / 0.2D1 - t358 * t431 / 0.2D1 - t33
     #5 * t374 + t340 * t434) - t106 * t440) * t112 * t150 / 0.5760D4
      t562 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t561)
      t565 = t1 * x1
      t566 = x1 * z
      t567 = -z - x1 + t566
      t568 = 0.1D1 / t567
      t570 = t128 * s * t565 * t568
      t571 = -0.1D1 + x1
      t572 = t2 * t571
      t574 = x2 * s * t565
      t575 = t1 ** 2
      t576 = s * t575
      t579 = x1 * t571 * t568
      t580 = t576 * t128 * t579
      t581 = x2 * x1
      t582 = t581 * z
      t584 = 0.1D1 / (-z - t581 + t582)
      t585 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, x1, t125, 0.10D1, x4)
      t586 = t584 * t585
      t587 = t117 * t13
      t588 = 0.1D1 / t14
      t589 = t588 * t568
      t590 = t571 ** 2
      t592 = t589 * t590 * t128
      t595 = log(0.4D1 * t587 * t592)
      t596 = t595 * t584
      t597 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, t125, 0.10D1, x4)
      t602 = t584 * t597
      t608 = t154 * t12
      t611 = log(0.4D1 * t608 * t592)
      t612 = t611 * t584
      t617 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, x1, t125, 0.10D1, x4)
      t620 = t611 ** 2
      t621 = t620 * t584
      t632 = (0.90D2 * t7 * (t586 - t597 * t596) - 0.180D3 * t62 * t602)
     # * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (-t612 * t597 + t586
     #) + 0.90D2 * t7 * (t584 * t617 - t612 * t585 + t621 * t597 / 0.2D1
     #) + t106 * t602) * t150 * t59 / 0.2880D4
      t633 = FJET(XB1, XB2, s, 0.0D0, t570, -t572, t574, -t580, t632)
      t636 = t2 * x1 * t568
      t637 = t576 * t579
      t638 = t589 * t590
      t641 = log(-0.4D1 * t13 * t638)
      t642 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t644 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t645 = t641 ** 2
      t646 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t660 = t645 * t641
      t671 = t3 * t642
      t672 = t567 * t642
      t673 = t64 * t12
      t675 = t589 * t590 * t67
      t678 = log(0.4D1 * t673 * t675)
      t679 = t678 * t567
      t682 = x1 * x3
      t683 = t682 * z
      t685 = 0.2D1 * t64 * z
      t686 = x1 * t14
      t687 = x3 * t14
      t688 = t687 * x1
      t689 = t64 * t14
      t690 = x3 * t567
      t692 = Sqrt(t690 * t66)
      t697 = 0.1D1 / (-t566 - t683 - t75 - t64 + t685 + t686 + t688 - t6
     #89 + 0.2D1 * t74 * t692 * z - t14)
      t701 = log(-0.4D1 * t673 * t638)
      t702 = t701 * t3
      t707 = t3 * t644
      t710 = t678 ** 2
      t711 = t710 * t567
      t716 = t701 ** 2
      t717 = t716 * t3
      t726 = t3 * t646
      t727 = t567 * t646 * t697 - t726
      t735 = log(0.4D1 * t587 * t675)
      t736 = t735 * t567
      t741 = t568 * t590
      t745 = log(-0.4D1 * t118 * t12 * t588 * t741)
      t746 = t745 * t3
      t760 = log(-0.4D1 * t608 * t638)
      t761 = t760 * t3
      t766 = t760 ** 2
      t767 = t766 * t3
      t779 = -(-0.180D3 * t4 * t7 * (t641 * t642 - t644 - t645 * t646 / 
     #0.2D1) + t35 * t7 * (t641 * t646 - t642) + 0.90D2 * t40 * (t641 * 
     #t644 - t645 * t642 / 0.2D1 + t660 * t646 / 0.6D1) - t55 * t7 * t64
     #6) * t59 / 0.5760D4 - (-0.180D3 * t62 * (-t671 + (t672 - t679 * t6
     #46) * t697 + t702 * t646) + 0.90D2 * t7 * (-t707 + (t567 * t644 - 
     #t679 * t642 + t711 * t646 / 0.2D1) * t697 - t717 * t646 / 0.2D1 + 
     #t702 * t642) + t106 * t727) * t112 * t59 / 0.5760D4 + (0.90D2 * t7
     # * (-(t672 - t736 * t646) * t697 + t671 - t746 * t646) + 0.180D3 *
     # t62 * t727) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (-t761 *
     # t646 + t671) + 0.90D2 * t7 * (t767 * t646 / 0.2D1 - t761 * t642 +
     # t707) + t106 * t726) * t150 * t59 / 0.2880D4
      t780 = FJET(XB1, XB2, s, 0.0D0, -t572, -t636, 0.0D0, t637, t779)
      t782 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t785 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t786 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t809 = t3 * t786
      t810 = t567 * t786
      t818 = t3 * t785
      t833 = t3 * t782
      t834 = t567 * t782 * t697 - t833
      t869 = -(-0.180D3 * t4 * t7 * (-t645 * t782 / 0.2D1 - t785 + t641 
     #* t786) + t35 * t7 * (t641 * t782 - t786) + 0.90D2 * t40 * (t641 *
     # t785 - t645 * t786 / 0.2D1 + t660 * t782 / 0.6D1) - t55 * t7 * t7
     #82) * t59 / 0.5760D4 - (-0.180D3 * t62 * (-t809 + (t810 - t679 * t
     #782) * t697 + t702 * t782) + 0.90D2 * t7 * (-t818 + (t567 * t785 -
     # t679 * t786 + t711 * t782 / 0.2D1) * t697 - t717 * t782 / 0.2D1 +
     # t702 * t786) + t106 * t834) * t112 * t59 / 0.5760D4 + (0.90D2 * t
     #7 * (t809 - (t810 - t736 * t782) * t697 - t746 * t782) + 0.180D3 *
     # t62 * t834) * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (t809 - 
     #t761 * t782) + 0.90D2 * t7 * (t818 + t767 * t782 / 0.2D1 - t761 * 
     #t786) + t106 * t833) * t150 * t59 / 0.2880D4
      t870 = FJET(XB1, XB2, s, 0.0D0, -t636, -t572, 0.0D0, t637, t869)
      t872 = x2 * x3
      t873 = 0.1D1 - x3 + t872
      t874 = 0.1D1 / t873
      t875 = t66 * t874
      t876 = t2 * t875
      t877 = t872 * t874
      t878 = t2 * t877
      t879 = t128 * t66
      t881 = Sqrt(t75 * t879)
      t885 = 0.1D1 / (-z - x3 + t872 + 0.2D1 * t74 * t881)
      t886 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, t125, -t875, x
     #4)
      t887 = t885 * t886
      t889 = t873 ** 2
      t890 = 0.1D1 / t889
      t891 = t66 * t890
      t895 = log(0.4D1 * t587 * t16 * t128 * t891)
      t896 = t895 * t885
      t897 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t125, -t875, x
     #4)
      t902 = t885 * t897
      t913 = log(0.4D1 * t117 * t16 * t281 * t891)
      t914 = t913 * t885
      t919 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.0D0, t125, -t875, x
     #4)
      t922 = t913 ** 2
      t923 = t922 * t885
      t934 = (0.90D2 * t7 * (t887 - t896 * t897) - 0.180D3 * t62 * t902)
     # * t112 * t151 / 0.2880D4 - (-0.180D3 * t62 * (t914 * t897 - t887)
     # + 0.90D2 * t7 * (-t885 * t919 + t914 * t886 - t923 * t897 / 0.2D1
     #) - t106 * t902) * t112 * t150 / 0.5760D4
      t935 = FJET(XB1, XB2, s, 0.0D0, -t876, 0.0D0, t878, 0.0D0, t934)
      t937 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, x1, t125, 0.10D1, x4)
      t938 = t584 * t937
      t939 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, t125, 0.10D1, x4)
      t944 = t584 * t939
      t957 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, x1, t125, 0.10D1, x4)
      t967 = (0.90D2 * t7 * (t938 - t596 * t939) - 0.180D3 * t62 * t944)
     # * t112 * t151 / 0.2880D4 + (-0.180D3 * t62 * (t938 - t612 * t939)
     # + 0.90D2 * t7 * (t621 * t939 / 0.2D1 - t612 * t937 + t584 * t957)
     # + t106 * t944) * t150 * t59 / 0.2880D4
      t968 = FJET(XB1, XB2, s, t574, -t572, t570, 0.0D0, -t580, t967)
      t970 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, t125, -t875, x
     #4)
      t971 = t885 * t970
      t972 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t125, -t875, x
     #4)
      t977 = t885 * t972
      t988 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.0D0, t125, -t875, x
     #4)
      t1001 = (0.90D2 * t7 * (t971 - t896 * t972) - 0.180D3 * t62 * t977
     #) * t112 * t151 / 0.2880D4 - (-0.180D3 * t62 * (t914 * t972 - t971
     #) + 0.90D2 * t7 * (-t885 * t988 + t914 * t970 - t923 * t972 / 0.2D
     #1) - t106 * t977) * t112 * t150 / 0.5760D4
      t1002 = FJET(XB1, XB2, s, t878, 0.0D0, -t876, 0.0D0, 0.0D0, t1001)
      t1004 = t2 * x1
      t1006 = Sqrt(-t690 * t879)
      t1007 = t74 * t1006
      t1013 = t1004 * x2 * (-x3 + t872 - z + t75 - x1 + t682 + t566 - t6
     #83 + 0.2D1 * t1007) * t568 * t874
      t1017 = t66 * s * t1 * t571 * t874
      t1020 = t117 * x1
      t1022 = t117 * t566
      t1026 = t1004 * (-x2 + t872 + 0.2D1 * t1007 * x2 + 0.1D1 - x3 + t1
     #020 + t117 * z - t1022) * t568 * t874
      t1027 = t572 * t877
      t1040 = 0.2D1 * t1007 * t582 + t14 + t582 - 0.2D1 * t1007 * t581 +
     # t687 * t581 + 0.2D1 * t64 * x2 * z - t64 * t14 * x2 - 0.2D1 * t87
     #2 * t566 + t75 + t64 - t686 - t688 + t689
      t1041 = x2 * t9
      t1051 = t566 + t683 - t685 + t1041 - t1020 - 0.2D1 * t1007 * z - t
     #64 * x2 - t872 * z + t872 * x1 - t581 * t14 - 0.2D1 * t1041 * z + 
     #t1041 * t14 + t1022
      t1053 = 0.1D1 / (t1040 + t1051)
      t1054 = t567 * t1053
      t1055 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, x1, t125, -t875, x4)
      t1063 = log(-0.4D1 * t117 * t13 * t588 * t741 * t879 * t890)
      t1064 = t1063 * t567
      t1065 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, t125, -t875, x4)
      t1074 = 0.90D2 * t7 * (-t1054 * t1055 + t1064 * t1053 * t1065) + 0
     #.180D3 * t62 * t1054 * t1065
      t1078 = FJET(XB1, XB2, s, t1013, t1017, -t1026, -t1027, -t580, t10
     #74 * t112 * t151 / 0.2880D4)
      t1081 = t112 * t150 * t59
      t1084 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, x1, t125, -t875, x4)
      t1086 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, t125, -t875, x4)
      t1095 = 0.90D2 * t7 * (-t1054 * t1084 + t1064 * t1053 * t1086) + 0
     #.180D3 * t62 * t1054 * t1086
      t1099 = FJET(XB1, XB2, s, -t1027, -t1026, t1017, t1013, -t580, t10
     #95 * t112 * t151 / 0.2880D4)
      bbarbbarH3n2e1 = t371 * t370 + t562 * t561 + t633 * t632 + t780 * 
     #t779 + t870 * t869 + t935 * t934 + t968 * t967 + t1002 * t1001 + t
     #1078 * t1074 * t1081 / 0.2880D4 + t1099 * t1095 * t1081 / 0.2880D4

      end function



      doubleprecision function bbarbbarH3n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 / z
      t7 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t8 = t6 * t7
      t9 = x1 ** 2
      t10 = t9 * x3
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = t13 * t16
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t23 = log(-0.4D1 * t10 * t20)
      t24 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t27 = cos(t11)
      t28 = x3 * z
      t30 = Sqrt(-t28 * t18)
      t34 = 0.1D1 / (-z - x3 + 0.2D1 * t27 * t30)
      t38 = log(0.4D1 * t10 * t17)
      t39 = t38 * t6
      t44 = lh * t5
      t45 = t6 * t24
      t46 = t24 * t34
      t51 = 0.1D1 / x3
      t53 = 0.1D1 / x1
      t56 = 0.1D1 - x2
      t57 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4
     #)
      t58 = t6 * t57
      t59 = -t45 - t46 + t58
      t61 = 0.1D1 / x2
      t63 = t51 * t61 * t53
      t66 = x2 ** 2
      t67 = t66 * t9
      t68 = -t56
      t69 = t17 * t68
      t72 = log(-0.4D1 * t67 * t69)
      t73 = t72 * t6
      t75 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x4
     #)
      t76 = t6 * t75
      t79 = log(0.4D1 * t67 * t17)
      t80 = t79 * t6
      t92 = t6 * lh
      t93 = t9 * t13
      t96 = log(0.4D1 * t93 * t16)
      t102 = t6 * t5
      t103 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t105 = t96 ** 2
      t111 = lh ** 2
      t113 = 0.3141592653589793D1 ** 2
      t115 = 0.180D3 * t111 - 0.30D2 * t113
      t116 = t6 * t115
      t117 = t5 * t24
      t127 = x3 * t16
      t131 = log(-0.4D1 * t127 * t13 * t19)
      t135 = log(0.4D1 * t127 * t13)
      t137 = -t131 * t34 - t135 * t6
      t139 = t135 ** 2
      t141 = t131 ** 2
      t144 = t139 * t6 / 0.2D1 + t141 * t34 / 0.2D1
      t151 = t115 * t5
      t154 = t34 + t6
      t159 = x3 * t66
      t162 = log(-0.4D1 * t159 * t20)
      t168 = log(0.4D1 * t159 * t17)
      t169 = t168 * t6
      t173 = log(-0.4D1 * t159 * t69)
      t174 = t173 * t6
      t186 = t16 * t66
      t189 = log(0.4D1 * t186 * t13)
      t191 = t13 * t68
      t194 = log(-0.4D1 * t186 * t191)
      t200 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x
     #4)
      t202 = t194 ** 2
      t206 = t189 ** 2
      t220 = log(0.4D1 * t17)
      t221 = t220 * t6
      t224 = (-0.180D3 * t92 - 0.90D2 * t221) * t5
      t229 = t220 ** 2
      t230 = t229 * t6
      t233 = (0.180D3 * t221 * lh + t116 + 0.45D2 * t230) * t5
      t249 = (-t221 * t115 - 0.90D2 * t230 * lh + t6 * (-0.2884936567583
     #026D3 - 0.120D3 * t111 * lh + 0.60D2 * lh * t113) - 0.15D2 * t229 
     #* t220 * t6) * t5
      t252 = -(0.90D2 * t5 * (t8 + (t7 - t23 * t24) * t34 - t39 * t24) -
     # 0.180D3 * t44 * (t45 + t46)) * t51 * t53 / 0.5760D4 + t5 * t59 * 
     #t63 / 0.32D2 + (0.90D2 * t5 * (-t73 * t57 + t76 + t80 * t24 - t8) 
     #- 0.180D3 * t44 * (-t45 + t58)) * t61 * t53 / 0.2880D4 - (-0.180D3
     # * t92 * t5 * (-t96 * t24 + t7) + 0.90D2 * t102 * (t103 - t96 * t7
     # + t105 * t24 / 0.2D1) + t116 * t117) * t53 / 0.5760D4 - ((0.90D2 
     #* t5 * t7 - 0.180D3 * t44 * t24) * t137 + 0.90D2 * t117 * t144 + (
     #0.90D2 * t5 * t103 - 0.180D3 * t44 * t7 + t151 * t24) * t154) * t5
     #1 / 0.11520D5 - (0.90D2 * t5 * (t8 + (t7 - t162 * t24) * t34 - t16
     #9 * t24 + t174 * t57 - t76) + 0.180D3 * t44 * t59) * t51 * t61 / 0
     #.5760D4 - (-0.180D3 * t92 * t5 * (t7 - t189 * t24 - t75 + t194 * t
     #57) + 0.90D2 * t102 * (-t200 + t194 * t75 - t202 * t57 / 0.2D1 + t
     #103 - t189 * t7 + t206 * t24 / 0.2D1) + t116 * t5 * (t24 - t57)) *
     # t61 / 0.5760D4 - t224 * t103 / 0.11520D5 - t233 * t7 / 0.11520D5 
     #- t249 * t24 / 0.11520D5
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t252)
      t255 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t256 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t260 = t6 * t255
      t265 = t256 * t34
      t266 = t6 * t256
      t274 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x
     #4)
      t275 = t6 * t274
      t276 = t275 - t266 - t265
      t280 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x
     #4)
      t281 = t6 * t280
      t299 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t306 = t5 * t256
      t350 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.0D0, t56, 0.10D1, x
     #4)
      t372 = -(0.90D2 * t5 * ((t255 - t23 * t256) * t34 + t260 - t39 * t
     #256) - 0.180D3 * t44 * (t265 + t266)) * t51 * t53 / 0.5760D4 + t5 
     #* t276 * t63 / 0.32D2 + (0.90D2 * t5 * (t281 - t73 * t274 - t260 +
     # t80 * t256) - 0.180D3 * t44 * (t275 - t266)) * t61 * t53 / 0.2880
     #D4 - (-0.180D3 * t92 * t5 * (-t96 * t256 + t255) + 0.90D2 * t102 *
     # (t299 - t96 * t255 + t105 * t256 / 0.2D1) + t116 * t306) * t53 / 
     #0.5760D4 - ((0.90D2 * t5 * t255 - 0.180D3 * t44 * t256) * t137 + 0
     #.90D2 * t306 * t144 + (0.90D2 * t5 * t299 - 0.180D3 * t44 * t255 +
     # t151 * t256) * t154) * t51 / 0.11520D5 - (0.90D2 * t5 * (t260 - t
     #169 * t256 + (t255 - t162 * t256) * t34 - t281 + t174 * t274) + 0.
     #180D3 * t44 * t276) * t51 * t61 / 0.5760D4 - (-0.180D3 * t92 * t5 
     #* (t255 - t189 * t256 - t280 + t194 * t274) + 0.90D2 * t102 * (-t3
     #50 + t194 * t280 - t202 * t274 / 0.2D1 + t299 - t189 * t255 + t206
     # * t256 / 0.2D1) + t116 * t5 * (t256 - t274)) * t61 / 0.5760D4 - t
     #224 * t299 / 0.11520D5 - t233 * t255 / 0.11520D5 - t249 * t256 / 0
     #.11520D5
      t373 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t372)
      t376 = t1 * x1
      t377 = x1 * z
      t378 = -z - x1 + t377
      t379 = 0.1D1 / t378
      t381 = t68 * s * t376 * t379
      t382 = -0.1D1 + x1
      t383 = t2 * t382
      t385 = x2 * s * t376
      t386 = t1 ** 2
      t387 = s * t386
      t390 = x1 * t382 * t379
      t391 = t387 * t68 * t390
      t392 = x2 * x1
      t393 = t392 * z
      t395 = 0.1D1 / (-z - t392 + t393)
      t396 = t5 * t395
      t397 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t401 = t67 * t13
      t403 = 0.1D1 / t14 * t379
      t404 = t382 ** 2
      t409 = log(0.4D1 * t401 * t403 * t404 * t68)
      t410 = t409 * t395
      t412 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t424 = t396 * t397 * t63 / 0.32D2 + (0.90D2 * t5 * (-t410 * t397 +
     # t395 * t412) - 0.180D3 * t44 * t395 * t397) * t61 * t53 / 0.2880D
     #4
      t425 = FJET(XB1, XB2, s, 0.0D0, t381, -t383, t385, -t391, t424)
      t428 = t2 * x1 * t379
      t429 = t387 * t390
      t430 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t431 = t6 * t430
      t433 = t10 * t13
      t438 = log(0.4D1 * t433 * t403 * t404 * t19)
      t439 = t438 * t378
      t440 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t443 = x1 * x3
      t444 = t443 * z
      t446 = 0.2D1 * t10 * z
      t447 = x1 * t14
      t448 = x3 * t14
      t449 = t448 * x1
      t450 = t10 * t14
      t451 = x3 * t378
      t453 = Sqrt(t451 * t18)
      t458 = 0.1D1 / (-t377 - t444 - t28 - t10 + t446 + t447 + t449 - t4
     #50 + 0.2D1 * t27 * t453 * z - t14)
      t460 = t403 * t404
      t463 = log(-0.4D1 * t433 * t460)
      t464 = t463 * t6
      t471 = t6 * t440
      t472 = t378 * t440 * t458 - t471
      t485 = log(-0.4D1 * t401 * t460)
      t486 = t485 * t6
      t499 = log(-0.4D1 * t93 * t460)
      t506 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t507 = t499 ** 2
      t517 = -(0.90D2 * t5 * (-t431 + (t378 * t430 - t439 * t440) * t458
     # + t464 * t440) - 0.180D3 * t44 * t472) * t51 * t53 / 0.5760D4 - t
     #5 * t472 * t63 / 0.32D2 + (0.90D2 * t5 * (-t486 * t440 + t431) - 0
     #.180D3 * t44 * t471) * t61 * t53 / 0.2880D4 - (-0.180D3 * t92 * t5
     # * (t499 * t440 - t430) + 0.90D2 * t102 * (t499 * t430 - t506 - t5
     #07 * t440 / 0.2D1) - t151 * t471) * t53 / 0.5760D4
      t518 = FJET(XB1, XB2, s, 0.0D0, -t383, -t428, 0.0D0, t429, t517)
      t520 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t521 = t6 * t520
      t523 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t533 = t6 * t523
      t534 = t378 * t523 * t458 - t533
      t562 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t571 = -(0.90D2 * t5 * (-t521 + (t378 * t520 - t439 * t523) * t458
     # + t464 * t523) - 0.180D3 * t44 * t534) * t51 * t53 / 0.5760D4 - t
     #5 * t534 * t63 / 0.32D2 + (0.90D2 * t5 * (t521 - t486 * t523) - 0.
     #180D3 * t44 * t533) * t61 * t53 / 0.2880D4 - (-0.180D3 * t92 * t5 
     #* (t499 * t523 - t520) + 0.90D2 * t102 * (-t507 * t523 / 0.2D1 - t
     #562 + t499 * t520) - t151 * t533) * t53 / 0.5760D4
      t572 = FJET(XB1, XB2, s, 0.0D0, -t428, -t383, 0.0D0, t429, t571)
      t574 = x2 * x3
      t575 = 0.1D1 - x3 + t574
      t576 = 0.1D1 / t575
      t577 = t18 * t576
      t578 = t2 * t577
      t579 = t574 * t576
      t580 = t2 * t579
      t581 = t68 * t18
      t583 = Sqrt(t28 * t581)
      t587 = 0.1D1 / (-z - x3 + t574 + 0.2D1 * t27 * t583)
      t588 = t5 * t587
      t589 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t577, x4
     #)
      t594 = t575 ** 2
      t600 = log(0.4D1 * t159 * t16 * t191 * t18 / t594)
      t601 = t600 * t587
      t603 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t577, x4
     #)
      t615 = t588 * t589 * t63 / 0.32D2 - (0.90D2 * t5 * (t601 * t589 - 
     #t587 * t603) + 0.180D3 * t44 * t587 * t589) * t51 * t61 / 0.5760D4
      t616 = FJET(XB1, XB2, s, 0.0D0, -t578, 0.0D0, t580, 0.0D0, t615)
      t618 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t622 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, x1, t56, 0.10D1, x4)
      t635 = t396 * t618 * t63 / 0.32D2 + (0.90D2 * t5 * (t395 * t622 - 
     #t410 * t618) - 0.180D3 * t44 * t395 * t618) * t61 * t53 / 0.2880D4
      t636 = FJET(XB1, XB2, s, t385, -t383, t381, 0.0D0, -t391, t635)
      t638 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t577, x4
     #)
      t643 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, t56, -t577, x4
     #)
      t655 = t588 * t638 * t63 / 0.32D2 - (0.90D2 * t5 * (t601 * t638 - 
     #t587 * t643) + 0.180D3 * t44 * t587 * t638) * t51 * t61 / 0.5760D4
      t656 = FJET(XB1, XB2, s, t580, 0.0D0, -t578, 0.0D0, 0.0D0, t655)
      t658 = t2 * x1
      t660 = Sqrt(-t451 * t581)
      t661 = t27 * t660
      t667 = t658 * x2 * (-x3 + t574 - z + t28 - x1 + t443 + t377 - t444
     # + 0.2D1 * t661) * t379 * t576
      t671 = t18 * s * t1 * t382 * t576
      t674 = t159 * x1
      t676 = t159 * t377
      t680 = t658 * (-x2 + t574 + 0.2D1 * t661 * x2 + 0.1D1 - x3 + t674 
     #+ t159 * z - t676) * t379 * t576
      t681 = t383 * t579
      t683 = x2 * t9
      t695 = t683 + t28 + t10 - t447 + t377 + t676 - 0.2D1 * t661 * t392
     # + t448 * t392 + 0.2D1 * t10 * x2 * z - t10 * t14 * x2 - 0.2D1 * t
     #574 * t377 + t14 + t683 * t14
      t706 = -0.2D1 * t661 * z - t10 * x2 - t574 * z + t574 * x1 - t392 
     #* t14 - 0.2D1 * t683 * z + 0.2D1 * t661 * t393 - t674 + t444 - t44
     #6 - t449 + t450 + t393
      t708 = 0.1D1 / (t695 + t706)
      t709 = t5 * t378 * t708
      t710 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, t56, -t577, x4)
      t712 = t61 * t53
      t713 = t710 * t51 * t712
      t716 = FJET(XB1, XB2, s, t667, t671, -t680, -t681, -t391, -t709 * 
     #t713 / 0.32D2)
      t718 = t378 * t708
      t722 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, t56, -t577, x4)
      t724 = t722 * t51 * t712
      t727 = FJET(XB1, XB2, s, -t681, -t680, t671, t667, -t391, -t709 * 
     #t724 / 0.32D2)
      bbarbbarH3n2e0 = t253 * t252 + t373 * t372 + t425 * t424 + t518 * 
     #t517 + t572 * t571 + t616 * t615 + t636 * t635 + t656 * t655 - t71
     #6 * t5 * t718 * t713 / 0.32D2 - t727 * t5 * t718 * t724 / 0.32D2

      end function



      doubleprecision function bbarbbarH3n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = x1 ** 2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t18 = log(0.4D1 * t12 * t15)
      t19 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t21 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t25 = t3 * lh
      t26 = t6 * t19
      t30 = 0.1D1 / x1
      t33 = t3 * t19
      t34 = cos(t9)
      t35 = x3 * z
      t36 = -0.1D1 + x3
      t38 = Sqrt(-t35 * t36)
      t42 = 0.1D1 / (-z - x3 + 0.2D1 * t34 * t38)
      t43 = t19 * t42
      t46 = 0.1D1 / x3
      t47 = t46 * t30
      t50 = 0.1D1 - x2
      t51 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x4
     #)
      t52 = t3 * t51
      t55 = 0.1D1 / x2
      t56 = t55 * t30
      t61 = t46 * t55
      t64 = x2 ** 2
      t65 = t15 * t64
      t68 = log(0.4D1 * t65 * t11)
      t70 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x4
     #)
      t71 = -t50
      t75 = log(-0.4D1 * t65 * t11 * t71)
      t87 = bbarbbarH32J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t93 = log(0.4D1 * t15 * t11)
      t94 = t93 * t3
      t97 = (-0.180D3 * t25 - 0.90D2 * t94) * t6
      t102 = lh ** 2
      t104 = 0.3141592653589793D1 ** 2
      t108 = t93 ** 2
      t112 = (0.180D3 * t94 * lh + t3 * (0.180D3 * t102 - 0.30D2 * t104)
     # + 0.45D2 * t108 * t3) * t6
      t115 = x3 * t15
      t120 = log(-0.4D1 * t115 * t11 / t36)
      t124 = log(0.4D1 * t115 * t11)
      t126 = -t120 * t42 - t124 * t3
      t131 = t6 * lh
      t135 = t42 + t3
      t140 = -(0.90D2 * t7 * (-t18 * t19 + t21) - 0.180D3 * t25 * t26) *
     # t30 / 0.5760D4 - t6 * (t33 + t43) * t47 / 0.64D2 + t6 * (-t33 + t
     #52) * t56 / 0.32D2 - t6 * (t43 - t52 + t33) * t61 / 0.64D2 - (0.90
     #D2 * t7 * (t21 - t68 * t19 - t70 + t75 * t51) - 0.180D3 * t25 * t6
     # * (t19 - t51)) * t55 / 0.5760D4 - t7 * t87 / 0.128D3 - t97 * t21 
     #/ 0.11520D5 - t112 * t19 / 0.11520D5 - (0.90D2 * t26 * t126 + (0.9
     #0D2 * t6 * t21 - 0.180D3 * t131 * t19) * t135) * t46 / 0.11520D5
      t141 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t140)
      t143 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t145 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t149 = t6 * t143
      t155 = t143 * t42
      t156 = t3 * t143
      t161 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x
     #4)
      t162 = t3 * t161
      t172 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, t50, 0.10D1, x
     #4)
      t184 = bbarbbarH31J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1
     #, x4)
      t202 = -(0.90D2 * t7 * (-t18 * t143 + t145) - 0.180D3 * t25 * t149
     #) * t30 / 0.5760D4 - t6 * (t155 + t156) * t47 / 0.64D2 + t6 * (t16
     #2 - t156) * t56 / 0.32D2 - t6 * (t155 - t162 + t156) * t61 / 0.64D
     #2 - (0.90D2 * t7 * (t145 - t68 * t143 - t172 + t75 * t161) - 0.180
     #D3 * t25 * t6 * (t143 - t161)) * t55 / 0.5760D4 - t7 * t184 / 0.12
     #8D3 - t97 * t145 / 0.11520D5 - t112 * t143 / 0.11520D5 - (0.90D2 *
     # t149 * t126 + (0.90D2 * t6 * t145 - 0.180D3 * t131 * t143) * t135
     #) * t46 / 0.11520D5
      t203 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t202)
      t206 = t1 * x1
      t207 = x1 * z
      t208 = -z - x1 + t207
      t209 = 0.1D1 / t208
      t211 = t71 * s * t206 * t209
      t212 = -0.1D1 + x1
      t213 = t2 * t212
      t215 = x2 * s * t206
      t216 = t1 ** 2
      t217 = s * t216
      t220 = x1 * t212 * t209
      t221 = t217 * t71 * t220
      t222 = x2 * x1
      t225 = 0.1D1 / (-z - t222 + t222 * z)
      t226 = t6 * t225
      t227 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, t50, 0.10D1, x4)
      t229 = t227 * t55 * t30
      t232 = FJET(XB1, XB2, s, 0.0D0, t211, -t213, t215, -t221, t226 * t
     #229 / 0.32D2)
      t238 = t2 * x1 * t209
      t239 = t217 * t220
      t242 = t212 ** 2
      t246 = log(-0.4D1 * t12 / t13 * t209 * t242)
      t247 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t249 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t253 = t3 * t247
      t262 = t8 * x3
      t271 = Sqrt(x3 * t208 * t36)
      t276 = 0.1D1 / (-t207 - x3 * x1 * z - t35 - t262 + 0.2D1 * t262 * 
     #z + x1 * t13 + x3 * t13 * x1 - t262 * t13 + 0.2D1 * t34 * t271 * z
     # - t13)
      t286 = -(0.90D2 * t7 * (t246 * t247 - t249) + 0.180D3 * t131 * t25
     #3) * t30 / 0.5760D4 - t6 * (t208 * t247 * t276 - t253) * t47 / 0.6
     #4D2 + t7 * t247 * t55 * t30 / 0.32D2
      t287 = FJET(XB1, XB2, s, 0.0D0, -t213, -t238, 0.0D0, t239, t286)
      t289 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t291 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t295 = t3 * t289
      t311 = -(0.90D2 * t7 * (t246 * t289 - t291) + 0.180D3 * t131 * t29
     #5) * t30 / 0.5760D4 - t6 * (t208 * t289 * t276 - t295) * t47 / 0.6
     #4D2 + t7 * t289 * t55 * t30 / 0.32D2
      t312 = FJET(XB1, XB2, s, 0.0D0, -t238, -t213, 0.0D0, t239, t311)
      t314 = x2 * x3
      t316 = 0.1D1 / (0.1D1 - x3 + t314)
      t317 = t36 * t316
      t318 = t2 * t317
      t320 = t2 * t314 * t316
      t323 = Sqrt(t35 * t71 * t36)
      t327 = 0.1D1 / (-z - x3 + t314 + 0.2D1 * t34 * t323)
      t328 = t6 * t327
      t329 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, -t317, x4
     #)
      t331 = t329 * t46 * t55
      t334 = FJET(XB1, XB2, s, 0.0D0, -t318, 0.0D0, t320, 0.0D0, t328 * 
     #t331 / 0.64D2)
      t339 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, t50, 0.10D1, x4)
      t341 = t339 * t55 * t30
      t344 = FJET(XB1, XB2, s, t215, -t213, t211, 0.0D0, -t221, t226 * t
     #341 / 0.32D2)
      t349 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t50, -t317, x4
     #)
      t351 = t349 * t46 * t55
      t354 = FJET(XB1, XB2, s, t320, 0.0D0, -t318, 0.0D0, 0.0D0, t328 * 
     #t351 / 0.64D2)
      bbarbbarH3n2em1 = t141 * t140 + t203 * t202 + t232 * t6 * t225 * t
     #229 / 0.32D2 + t287 * t286 + t312 * t311 + t334 * t6 * t327 * t331
     # / 0.64D2 + t344 * t6 * t225 * t341 / 0.32D2 + t354 * t6 * t327 * 
     #t351 / 0.64D2

      end function



      doubleprecision function bbarbbarH3n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t9 = 0.1D1 / x1
      t13 = 0.1D1 - x2
      t14 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, t13, 0.10D1, x4
     #)
      t16 = 0.1D1 / x2
      t20 = bbarbbarH32J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t25 = z ** 2
      t28 = x4 * 0.3141592653589793D1
      t29 = Sin(t28)
      t30 = t29 ** 2
      t33 = log(0.4D1 / t25 / z * t30)
      t37 = (-0.180D3 * t3 * lh - 0.90D2 * t33 * t3) * t6
      t41 = cos(t28)
      t45 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t52 = (0.1D1 / (-z - x3 + 0.2D1 * t41 * t45) + t3) / x3
      t55 = -t7 * t8 * t9 / 0.64D2 - t7 * (t8 - t14) * t16 / 0.64D2 - t7
     # * t20 / 0.128D3 - t37 * t8 / 0.11520D5 - t6 * t8 * t52 / 0.128D3
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t55)
      t58 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t62 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, t13, 0.10D1, x4
     #)
      t67 = bbarbbarH31J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t75 = -t7 * t58 * t9 / 0.64D2 - t7 * (t58 - t62) * t16 / 0.64D2 - 
     #t7 * t67 / 0.128D3 - t37 * t58 / 0.11520D5 - t6 * t58 * t52 / 0.12
     #8D3
      t76 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t75)
      t78 = -0.1D1 + x1
      t79 = t2 * t78
      t82 = 0.1D1 / (-z - x1 + x1 * z)
      t84 = t2 * x1 * t82
      t85 = t1 ** 2
      t89 = s * t85 * x1 * t78 * t82
      t90 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4
     #)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t79, -t84, 0.0D0, t89, t7 * t90 * 
     #t9 / 0.64D2)
      t100 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t104 = FJET(XB1, XB2, s, 0.0D0, -t84, -t79, 0.0D0, t89, t7 * t100 
     #* t9 / 0.64D2)
      bbarbbarH3n2em2 = t56 * t55 + t76 * t75 + t94 * t6 * t3 * t90 * t9
     # / 0.64D2 + t104 * t3 * t6 * t100 * t9 / 0.64D2

      end function



      doubleprecision function bbarbbarH3n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = bbarbbarH32J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7 * t8 /
     # 0.128D3)
      t15 = bbarbbarH31J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t18 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7 * t15 
     #/ 0.128D3)
      bbarbbarH3n2em3 = -t11 * t3 * t6 * t8 / 0.128D3 - t18 * t3 * t6 * 
     #t15 / 0.128D3

      end function



      doubleprecision function bbarbbarH3n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH31J1
      doubleprecision bbarbbarH31J2
      doubleprecision bbarbbarH31J3
      doubleprecision bbarbbarH32J1
      doubleprecision bbarbbarH32J2
      doubleprecision bbarbbarH32J3
      bbarbbarH3n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbarbbarH31J1
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
      t8 = z + x1 * t3
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t13 = 0.1D1 - x2
      t14 = x3 * t13
      t16 = 0.1D1 - x3
      t19 = cos(x4 * 0.3141592653589793D1)
      t23 = Sqrt(t14 * t8 * x2 * t16)
      t25 = 0.2D1 * t19 * t23
      t26 = t14 * t8 + x2 * t16 - t25
      t27 = s * t3
      t28 = 0.1D1 / t8
      t29 = x1 * t28
      t32 = 0.1D1 - x1
      t33 = t32 * x3
      t35 = s - t27 * t29 * t26 - t27 * t33
      t36 = t26 * t35
      t40 = t16 * t13 * t8 + x2 * x3 + t25
      t43 = t32 * t16
      t45 = s - t27 * t29 * t40 - t27 * t43
      t46 = t45 * t40
      t47 = t36 * t46
      t50 = t2 * s
      t51 = t4 ** 2
      t52 = t50 * t51
      t53 = t6 * x1
      t54 = t9 ** 2
      t55 = 0.1D1 / t54
      t57 = t52 * t53 * t55
      t58 = t26 ** 2
      t59 = t58 * t32
      t62 = z + x1 * t13 * t3
      t63 = t62 * t35
      t67 = t4 * t3
      t68 = t50 * t67
      t70 = t35 * t10
      t74 = t50 * t4
      t75 = t32 ** 2
      t76 = t75 * t16
      t77 = t74 * t76
      t78 = t62 * t28
      t79 = z ** 2
      t84 = t75 * t32
      t87 = t16 ** 2
      t92 = t2 * t67
      t93 = x1 * t10
      t96 = t75 * t62
      t97 = t35 * t45
      t103 = t26 * t32
      t104 = t63 * t45
      t109 = 0.1D1 / t9 / t8
      t110 = t6 * t109
      t120 = t35 * t2 * t45
      t121 = t67 * t84
      t126 = t4 * t75
      t133 = t45 * t2
      t134 = t3 * t51
      t135 = t75 ** 2
      t138 = t133 * t134 * t135 * t62
      t140 = x1 * t16
      t145 = t133 * t126
      t146 = t35 * t16
      t149 = t50 * t134
      t150 = t6 ** 2
      t151 = t150 * t35
      t152 = t149 * t151
      t153 = t109 * t26
      t159 = t53 * t35
      t161 = t10 * t26
      t168 = t50 * t51 * t67
      t169 = x2 ** 2
      t170 = t150 * x1
      t174 = t26 * t45
      t179 = -0.2D1 * t5 * t6 * t10 * t47 - t57 * t59 * t63 * t40 - 0.2D
     #1 * t68 * t53 * t70 * t58 + 0.6D1 * t77 * t78 * t45 * t79 - t52 * 
     #t84 * t62 * t70 * t87 * x1 * t40 + 0.3D1 * t92 * t93 * t26 * t96 *
     # t97 * x3 - 0.5D1 * t5 * t93 * t103 * t104 + 0.2D1 * t92 * t110 * 
     #t59 * t104 + 0.2D1 * t74 * x1 * t43 * t45 + 0.2D1 * t120 * t121 * 
     #t87 * t16 - 0.2D1 * t120 * t126 * t87 + t120 * t3 * t32 * t16 + 0.
     #2D1 * t138 * t70 * x2 * t140 * x3 - t145 * t78 * t146 - 0.12D2 * t
     #152 * t153 * t40 * x2 * t32 + 0.6D1 * t52 * t159 * t161 * z * x2 *
     # t32 + 0.2D1 * t168 * t169 * t170 * t75 * t55 * t174 * t40
      t180 = t51 * t4
      t184 = t109 * t35
      t185 = t169 * t6
      t187 = t184 * t185 * t16
      t189 = x2 * x1
      t193 = t6 * t28
      t195 = x3 ** 2
      t202 = t40 ** 2
      t207 = t50 * t180
      t222 = t16 * t62
      t223 = t28 * t45
      t227 = t52 * x1
      t233 = t45 * x3
      t236 = t92 * t53
      t238 = t10 * t58 * t97
      t266 = t28 * t26 * t97
      t272 = -t133 * t180 * t135 * t62 * t187 + t138 * t70 * t189 * t87 
     #- t52 * t193 * t174 * t75 * t195 + 0.6D1 * t52 * t76 * t62 * t109 
     #* t45 * t6 * t202 + 0.2D1 * t207 * t135 * t62 * t187 - 0.6D1 * t68
     # * t159 * t161 * z * t40 - 0.6D1 * t145 * t78 * t146 * z + 0.6D1 *
     # t74 * t75 * t222 * t223 - t227 * t84 * t16 * t45 * t195 - t227 * 
     #t84 * t87 * t233 - t236 * t238 + 0.2D1 * t92 * x1 * t75 * t87 * t9
     #7 - 0.2D1 * t120 * t126 * t16 * x3 - 0.5D1 * t5 * x1 * t43 * t97 +
     # 0.2D1 * t236 * t109 * t58 * t26 * t97 + 0.6D1 * t52 * t150 * t184
     # * t26 * t202 + t2 * t3 * x1 * t266 + 0.2D1 * t74 * t93 * t103 * t
     #63
      t274 = t6 * t5
      t291 = t135 * t32
      t305 = t51 ** 2
      t306 = t50 * t305
      t307 = t169 * x2
      t330 = t45 * z
      t342 = t169 * t75
      t346 = t133 * t121
      t351 = t92 * t53 * t109
      t352 = t58 * t35
      t357 = t62 * t10
      t365 = t45 * x2
      t370 = t53 * t10
      t375 = -0.2D1 * t274 * t238 + 0.5D1 * t274 * t266 + 0.6D1 * t74 * 
     #t6 * t35 * t28 * t26 * t79 + 0.3D1 * t92 * t6 * t32 * t146 * t223 
     #* t40 + 0.2D1 * t168 * t185 * t291 * t109 * t16 * t63 * x3 - t207 
     #* t189 * t291 * t10 * t16 * t63 * t195 - 0.2D1 * t306 * t307 * t53
     # * t291 * t55 * t222 * t35 - t57 * t103 * t63 * t202 + 0.7D1 * t13
     #3 * t67 * t75 * t62 * t70 * t140 * t40 - t52 * t96 * t184 * t16 * 
     #t6 * t202 + 0.6D1 * t77 * t78 * t330 - 0.2D1 * t306 * t307 * t170 
     #* t84 * t55 * t174 + 0.2D1 * t207 * t150 * t109 * t174 * t342 - t3
     #46 * t78 * t35 * t87 + 0.2D1 * t351 * t352 * t46 - 0.12D2 * t68 * 
     #t76 * t357 * t45 * x1 * t40 - 0.2D1 * t149 * t135 * t87 * t357 * t
     #365 * x1 - t52 * t370 * t58 * t45 * t33
      t413 = t2 * t134
      t414 = x2 * t150
      t417 = t97 * t40
      t467 = 0.4D1 * t92 * t370 * t47 - 0.6D1 * t5 * t193 * t36 * t330 +
     # t351 * t36 * t45 * t202 + t92 * t193 * t36 * t45 * t32 * x3 + 0.6
     #D1 * t207 * t151 * t153 * t342 - t207 * x2 * t170 * t32 * t55 * t1
     #74 * t202 - 0.2D1 * t92 * x1 * t75 * t146 * t233 - t2 * t180 * t16
     #9 * t150 * t75 * t109 * t36 * t45 + 0.2D1 * t413 * t414 * t32 * t1
     #53 * t417 - 0.6D1 * t2 * t51 * t370 * t36 * t365 * t32 - 0.2D1 * t
     #346 * t78 * t146 * x3 - 0.2D1 * t92 * t110 * t26 * t32 * t62 * t41
     #7 - 0.2D1 * t152 * t109 * t58 * x2 * t32 + 0.2D1 * t120 * t121 * t
     #87 * x3 + t120 * t121 * t16 * t195 - 0.2D1 * t68 * t84 * t87 * t62
     # * t223 - 0.6D1 * t68 * t76 * t62 * t10 * t45 * z * x1 * t40 + t41
     #3 * t414 * t32 * t109 * t352 * t45
      bbarbbarH31J1 = 0.16D2 / 0.3D1 * wd * (t179 + t272 + t375 + t467) 
     #/ t35 / t45 / s

      end function
  
   
 

      doubleprecision function bbarbbarH31J2
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
      t6 = t5 * t3
      t7 = t2 * t6
      t8 = x1 ** 2
      t9 = t8 ** 2
      t10 = x2 * t9
      t11 = 0.1D1 - x1
      t15 = z + x1 * t3
      t16 = t15 ** 2
      t18 = 0.1D1 / t16 / t15
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t22 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t20 * t15 * x2 * t22)
      t31 = 0.2D1 * t25 * t29
      t32 = t20 * t15 + x2 * t22 - t31
      t34 = s * t3
      t35 = 0.1D1 / t15
      t36 = x1 * t35
      t39 = t11 * x3
      t41 = s - t34 * t36 * t32 - t34 * t39
      t45 = t22 * t19 * t15 + x2 * x3 + t31
      t48 = t11 * t22
      t50 = s - t34 * t36 * t45 - t34 * t48
      t51 = t41 * t50
      t52 = t51 * t45
      t56 = t4 * t3
      t57 = t2 * t56
      t58 = 0.1D1 / t16
      t59 = x1 * t58
      t62 = t11 ** 2
      t65 = z + x1 * t19 * t3
      t66 = t62 * t65
      t71 = t2 * t5
      t72 = t62 * t11
      t73 = t72 * t22
      t82 = t50 * t2
      t83 = t5 * t4
      t84 = t62 ** 2
      t88 = t18 * t41
      t89 = x2 ** 2
      t92 = t88 * t89 * t8 * t22
      t94 = t56 * t72
      t95 = t82 * t94
      t96 = t65 * t35
      t97 = t41 * t22
      t102 = t8 * x1
      t103 = t102 * t58
      t105 = t32 * t41
      t106 = t50 * x2
      t111 = t2 * s
      t112 = t111 * t83
      t118 = t57 * t102 * t18
      t119 = t32 ** 2
      t120 = t119 * t41
      t121 = t50 * t45
      t125 = t2 * t4
      t128 = t105 * t121
      t131 = t111 * t5
      t132 = t16 ** 2
      t133 = 0.1D1 / t132
      t135 = t131 * t102 * t133
      t137 = t65 * t41
      t141 = t8 * t35
      t157 = t50 * t35
      t162 = -0.2D1 * t7 * t10 * t11 * t18 * t32 * t52 - 0.3D1 * t57 * t
     #59 * t32 * t66 * t51 * x3 + 0.2D1 * t71 * t73 * t65 * t58 * t50 * 
     #t41 * x2 * x1 - t82 * t83 * t84 * t65 * t92 + 0.3D1 * t95 * t96 * 
     #t97 * x3 + 0.2D1 * t71 * t103 * t105 * t106 * t11 - 0.2D1 * t112 *
     # t84 * t65 * t92 - 0.4D1 * t118 * t120 * t121 + 0.4D1 * t125 * t8 
     #* t58 * t128 + t135 * t119 * t11 * t137 * t45 - 0.3D1 * t57 * t141
     # * t105 * t50 * t11 * x3 - 0.2D1 * t57 * t8 * t18 * t32 * t11 * t6
     #5 * t52 - 0.3D1 * t57 * t8 * t11 * t97 * t157 * t45
      t166 = t58 * t41
      t167 = t22 * x1
      t174 = t45 ** 2
      t180 = t22 ** 2
      t192 = t32 * t50
      t204 = t57 * t102
      t206 = t58 * t119 * t51
      t209 = t41 * t2 * t50
      t210 = t4 * t62
      t215 = t131 * x1
      t217 = t50 * x3
      t229 = t125 * t8
      t233 = t35 * t32 * t51
      t236 = -0.3D1 * t82 * t56 * t62 * t65 * t166 * t167 * t45 + t131 *
     # t66 * t88 * t22 * t8 * t174 + t131 * t72 * t65 * t166 * t180 * x1
     # * t45 - t7 * t10 * t11 * t18 * t120 * t50 - 0.2D1 * t112 * t9 * t
     #18 * t192 * t89 * t62 + t112 * x2 * t9 * x1 * t11 * t133 * t192 * 
     #t174 + t204 * t206 + 0.4D1 * t209 * t210 * t22 * x3 + t215 * t72 *
     # t180 * t217 - 0.2D1 * t204 * t18 * t119 * t32 * t51 + 0.5D1 * t12
     #5 * x1 * t48 * t51 + 0.2D1 * t229 * t206 - 0.6D1 * t229 * t233
      t241 = t111 * t6
      t249 = t32 * t11
      t257 = t111 * t4
      t293 = -t2 * t3 * x1 * t233 + 0.2D1 * t241 * t84 * t180 * t65 * t5
     #8 * t106 * x1 + t135 * t249 * t137 * t174 - 0.3D1 * t118 * t105 * 
     #t50 * t174 - 0.2D1 * t257 * x1 * t48 * t50 - 0.2D1 * t209 * t94 * 
     #t180 * t22 + 0.2D1 * t209 * t210 * t180 - t209 * t3 * t11 * t22 + 
     #0.5D1 * t125 * t59 * t249 * t137 * t50 - 0.2D1 * t57 * x1 * t62 * 
     #t97 * t217 - 0.6D1 * t82 * t210 * t96 * t97 + t95 * t96 * t41 * t1
     #80 - 0.2D1 * t257 * t59 * t249 * t137
      t294 = x2 * x1
      t299 = x3 ** 2
      t305 = t82 * t6 * t84 * t65
      t311 = t111 * t56
      t360 = t112 * t294 * t84 * t11 * t58 * t22 * t137 * t299 - 0.2D1 *
     # t305 * t166 * x2 * t167 * x3 + 0.2D1 * t311 * t102 * t166 * t119 
     #+ 0.2D1 * t241 * t9 * t41 * t18 * t119 * x2 * t11 - t2 * t83 * t89
     # * t9 * t62 * t18 * t105 * t50 + 0.3D1 * t57 * t103 * t128 - t305 
     #* t166 * t294 * t180 + t131 * t141 * t192 * t62 * t299 + t215 * t7
     #3 * t50 * t299 + t131 * t103 * t119 * t50 * t39 - 0.4D1 * t209 * t
     #94 * t180 * x3 - 0.3D1 * t209 * t94 * t22 * t299 + 0.2D1 * t311 * 
     #t72 * t180 * t65 * t157
      bbarbbarH31J2 = 0.16D2 / 0.3D1 * wd * (t162 + t236 + t293 + t360) 
     #/ t41 / t50 / s

      end function
  
   
 

      doubleprecision function bbarbbarH31J3
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
      t7 = z + x1 * t3
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = x1 * t9
      t12 = 0.1D1 - x2
      t13 = x3 * t12
      t15 = 0.1D1 - x3
      t18 = cos(x4 * 0.3141592653589793D1)
      t22 = Sqrt(t13 * t7 * x2 * t15)
      t24 = 0.2D1 * t18 * t22
      t25 = t13 * t7 + x2 * t15 - t24
      t26 = 0.1D1 - x1
      t27 = t25 * t26
      t30 = z + x1 * t12 * t3
      t31 = s * t3
      t32 = 0.1D1 / t7
      t33 = x1 * t32
      t36 = t26 * x3
      t38 = s - t31 * t33 * t25 - t31 * t36
      t39 = t30 * t38
      t43 = t15 * t12 * t7 + x2 * x3 + t24
      t46 = t26 * t15
      t48 = s - t31 * t33 * t43 - t31 * t46
      t49 = t39 * t48
      t52 = t4 ** 2
      t53 = t2 * t52
      t54 = x1 ** 2
      t55 = t54 * x1
      t56 = t55 * t9
      t58 = t25 * t38
      t63 = t52 * t3
      t65 = t54 ** 2
      t70 = 0.1D1 / t8 / t7
      t72 = t38 * t48
      t73 = t72 * t43
      t76 = t2 * s
      t77 = t76 * t52
      t78 = t77 * x1
      t79 = t26 ** 2
      t80 = t79 * t26
      t81 = t15 ** 2
      t83 = t48 * x3
      t89 = t4 * t3
      t90 = t2 * t89
      t101 = t15 * t38
      t104 = t48 * t2
      t105 = t79 ** 2
      t109 = t9 * t38
      t111 = x1 * t15
      t115 = t8 ** 2
      t116 = 0.1D1 / t115
      t118 = t77 * t55 * t116
      t119 = t25 ** 2
      t120 = t119 * t26
      t124 = t43 ** 2
      t129 = t76 * t52 * t4
      t130 = x2 * x1
      t135 = x3 ** 2
      t145 = t80 * t15
      t156 = t5 * t10 * t27 * t49 + t53 * t56 * t58 * t48 * x2 * t26 - t
     #2 * t63 * x2 * t65 * t26 * t70 * t25 * t73 + t78 * t80 * t81 * t83
     # + t5 * x1 * t46 * t72 + t90 * x1 * t79 * t81 * t72 + t5 * t54 * t
     #32 * t25 * t72 + t90 * x1 * t79 * t101 * t83 - t104 * t63 * t105 *
     # t30 * t109 * x2 * t111 * x3 + t118 * t120 * t39 * t43 + t118 * t2
     #7 * t39 * t124 + t129 * t130 * t105 * t26 * t9 * t15 * t39 * t135 
     #+ t104 * t89 * t79 * t30 * t109 * t111 * t43 + t78 * t145 * t48 * 
     #t135 + t53 * t145 * t30 * t9 * t48 * t38 * x2 * x1
      t159 = t30 * t32
      t162 = t54 * t70
      t173 = t76 * t63
      t191 = t54 * t32
      t193 = t25 * t48
      t197 = t76 * t89
      t244 = t104 * t4 * t79 * t159 * t101 + t90 * t162 * t120 * t49 + t
     #77 * t79 * t30 * t70 * t38 * t15 * t54 * t124 - 0.2D1 * t173 * t10
     #5 * t30 * t9 * t101 * t130 * x3 + t77 * t80 * t30 * t109 * t81 * x
     #1 * t43 + t77 * t56 * t119 * t48 * t36 + t77 * t191 * t193 * t79 *
     # t135 - 0.2D1 * t197 * t10 * t25 * t79 * t39 * x3 - 0.2D1 * t173 *
     # t65 * t70 * t193 * t43 * x2 * t26 + t129 * x2 * t65 * x1 * t26 * 
     #t116 * t193 * t124 - t104 * t89 * t80 * t159 * t101 * x3 + t90 * t
     #162 * t25 * t26 * t30 * t73 - 0.2D1 * t197 * t54 * t26 * t15 * t48
     # * t32 * t43 + t90 * t191 * t58 * t48 * t26 * x3 - t90 * t56 * t58
     # * t48 * t43
      bbarbbarH31J3 = 0.16D2 / 0.3D1 * wd * (t156 + t244) / t38 / t48 / 
     #s

      end function
  
   
 

      doubleprecision function bbarbbarH32J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = x1 ** 2
      t10 = t9 ** 2
      t11 = s * t4
      t13 = z + x1 * t4
      t14 = 0.1D1 / t13
      t15 = x1 * t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t13 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t13 + x2 * t19 - t28
      t32 = 0.1D1 - x1
      t33 = t32 * x3
      t35 = s - t11 * t15 * t29 - t11 * t33
      t38 = t13 ** 2
      t40 = 0.1D1 / t38 / t13
      t41 = t29 ** 2
      t47 = t5 * t4
      t48 = t3 * t47
      t49 = t9 * t14
      t50 = t48 * t49
      t51 = t29 * t35
      t55 = t5 * t3
      t56 = t55 * t9
      t57 = t14 * t29
      t62 = t32 * t19
      t65 = x2 * x3
      t66 = t19 * t16 * t13 + t65 + t28
      t70 = s - t11 * t15 * t66 - t62 * t11
      t75 = t35 * t2 * t70
      t76 = t32 ** 2
      t77 = t76 * t32
      t78 = t47 * t77
      t79 = t19 ** 2
      t84 = t5 * t76
      t91 = x1 * t9
      t93 = 0.1D1 / t38
      t94 = t35 * t93
      t98 = t3 * t6
      t99 = t76 ** 2
      t102 = z + x1 * t16 * t4
      t103 = t99 * t102
      t105 = t14 * t70
      t106 = x3 ** 2
      t107 = t19 * t106
      t111 = t77 * t102
      t118 = t2 * t47
      t121 = t19 * t35
      t122 = t70 * x3
      t126 = x1 * t93
      t128 = t29 * t32
      t129 = t102 * t35
      t133 = t6 * t5
      t134 = t3 * t133
      t135 = t10 * x1
      t138 = t38 ** 2
      t139 = 0.1D1 / t138
      t141 = t29 * t70
      t142 = t66 ** 2
      t146 = t70 * t2
      t147 = t146 * t78
      t148 = t102 * t14
      t153 = t98 * t49
      t154 = t76 * t106
      t157 = t91 * t93
      t168 = -0.2D1 * t8 * t10 * t35 * t40 * t41 * x2 * t32 - 0.12D2 * t
     #50 * t51 * t33 + 0.6D1 * t56 * t57 * t35 + 0.2D1 * t55 * x1 * t62 
     #* t70 + 0.2D1 * t75 * t78 * t79 * t19 - 0.2D1 * t75 * t84 * t79 + 
     #t75 * t4 * t32 * t19 - 0.2D1 * t48 * t91 * t94 * t41 + 0.6D1 * t98
     # * t103 * t105 * t107 - 0.6D1 * t48 * t111 * t105 * t19 * z * x3 -
     # 0.2D1 * t118 * x1 * t76 * t121 * t122 + 0.2D1 * t55 * t126 * t128
     # * t129 - t134 * x2 * t135 * t32 * t139 * t141 * t142 + 0.4D1 * t1
     #47 * t148 * t121 * x3 - t153 * t141 * t154 - t98 * t157 * t41 * t7
     #0 * t33 + 0.3D1 * t118 * t9 * t32 * t121 * t105 * t66
      t171 = t146 * t7 * t99 * t102
      t173 = x1 * t19
      t179 = t118 * t91 * t40
      t180 = t41 * t35
      t181 = t70 * t66
      t185 = t146 * t84
      t193 = t2 * t5
      t216 = t51 * t181
      t220 = t98 * t91 * t139
      t221 = t32 * t41
      t234 = t70 * t19
      t244 = t98 * x1
      t245 = t77 * t19
      t258 = t35 * t70
      t262 = 0.2D1 * t171 * t94 * x2 * t173 * x3 + 0.2D1 * t179 * t180 *
     # t181 + 0.5D1 * t185 * t148 * t121 - 0.6D1 * t185 * t148 * t121 * 
     #z - 0.6D1 * t193 * t49 * t51 * t70 * z + 0.7D1 * t118 * t49 * t51 
     #* t70 * t32 * x3 + t146 * t47 * t76 * t102 * t94 * t173 * t66 + 0.
     #6D1 * t153 * t51 * t154 - 0.2D1 * t193 * t9 * t93 * t216 - t220 * 
     #t221 * t129 * t66 - 0.2D1 * t48 * t77 * t79 * t102 * t105 + t75 * 
     #t78 * t107 + 0.6D1 * t98 * t111 * t93 * t234 * z * x2 * x1 + 0.2D1
     # * t75 * t78 * t79 * x3 - t244 * t245 * t70 * t106 - t244 * t77 * 
     #t79 * t122 - 0.2D1 * t75 * t84 * t19 * x3 + 0.2D1 * t118 * x1 * t7
     #6 * t79 * t258
      t264 = t118 * t91
      t266 = t93 * t41 * t258
      t279 = t57 * t258
      t282 = z ** 2
      t287 = t193 * t9
      t304 = x2 * x1
      t305 = t99 * t32
      t312 = t6 ** 2
      t313 = t3 * t312
      t314 = x2 ** 2
      t315 = t314 * x2
      t324 = t134 * t103
      t325 = t40 * t35
      t327 = t19 * t314 * t9
      t328 = t325 * t327
      t331 = t9 * t40
      t333 = t129 * t70
      t344 = t2 * t7
      t345 = x2 * t10
      t349 = t258 * t66
      t363 = -t264 * t266 - 0.5D1 * t193 * x1 * t62 * t258 + 0.2D1 * t26
     #4 * t40 * t41 * t29 * t258 + t2 * t4 * x1 * t279 + 0.6D1 * t56 * t
     #35 * t14 * t29 * t282 - t287 * t279 - 0.2D1 * t287 * t266 - 0.6D1 
     #* t2 * t6 * t245 * t102 * t93 * t70 * t35 * x2 * x1 + 0.6D1 * t56 
     #* t57 * t35 * z - t134 * t304 * t305 * t93 * t19 * t129 * t106 - 0
     #.2D1 * t313 * t315 * t91 * t305 * t139 * t19 * t102 * t35 + 0.2D1 
     #* t324 * t328 + 0.2D1 * t118 * t331 * t221 * t333 - t147 * t148 * 
     #t35 * t79 - 0.5D1 * t193 * t126 * t128 * t333 + 0.2D1 * t344 * t34
     #5 * t32 * t40 * t29 * t349 - t2 * t133 * t314 * t10 * t76 * t40 * 
     #t51 * t70 - 0.2D1 * t118 * t157 * t216
      t364 = t76 * t102
      t393 = t3 * t6 * t47
      t463 = -t98 * t364 * t325 * t19 * t9 * t142 - 0.2D1 * t8 * t99 * t
     #79 * t102 * t93 * t70 * x2 * x1 - t220 * t128 * t129 * t142 - 0.2D
     #1 * t118 * t331 * t29 * t32 * t102 * t349 - t98 * t111 * t94 * t79
     # * x1 * t66 + 0.2D1 * t393 * t314 * t135 * t76 * t139 * t141 * t66
     # - 0.2D1 * t313 * t315 * t135 * t77 * t139 * t141 + 0.2D1 * t134 *
     # t10 * t40 * t141 * t314 * t76 + 0.3D1 * t118 * t126 * t29 * t364 
     #* t258 * x3 - t146 * t133 * t99 * t102 * t328 + t171 * t94 * t304 
     #* t79 - 0.12D2 * t8 * t103 * t93 * t234 * t65 * x1 - 0.6D1 * t50 *
     # t51 * z * t32 * x3 + 0.6D1 * t324 * t40 * t70 * t327 + t344 * t34
     #5 * t32 * t40 * t180 * t70 + 0.6D1 * t55 * t76 * t19 * t148 * t70 
     #* t282 + t179 * t51 * t70 * t142 + 0.2D1 * t393 * t314 * t9 * t305
     # * t40 * t19 * t129 * x3
      bbarbbarH32J1 = 0.16D2 / 0.3D1 * wd * (t168 + t262 + t363 + t463) 
     #/ t70 / t35 / s

      end function
  
   
 

      doubleprecision function bbarbbarH32J2
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
      t5 = t4 * t3
      t6 = t2 * t5
      t8 = z + x1 * t3
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t11 = t10 * x1
      t12 = 0.1D1 - x2
      t13 = x3 * t12
      t15 = 0.1D1 - x3
      t18 = cos(x4 * 0.3141592653589793D1)
      t22 = Sqrt(t13 * t8 * x2 * t15)
      t24 = 0.2D1 * t18 * t22
      t25 = t13 * t8 + x2 * t15 - t24
      t28 = 0.1D1 - x1
      t29 = t28 ** 2
      t32 = z + x1 * t12 * t3
      t33 = t29 * t32
      t34 = s * t3
      t35 = 0.1D1 / t8
      t36 = x1 * t35
      t39 = t28 * x3
      t41 = s - t34 * t36 * t25 - t34 * t39
      t45 = t15 * t12 * t8 + x2 * x3 + t24
      t48 = t28 * t15
      t50 = s - t34 * t36 * t45 - t34 * t48
      t51 = t41 * t50
      t56 = t4 ** 2
      t57 = t56 * t3
      t58 = t2 * t57
      t59 = x1 ** 2
      t60 = t59 ** 2
      t61 = x2 * t60
      t65 = 0.1D1 / t9 / t8
      t67 = t51 * t45
      t71 = t2 * t56
      t72 = t59 * x1
      t73 = t72 * t10
      t75 = t25 * t41
      t76 = t50 * x2
      t81 = t2 * s
      t82 = t81 * t4
      t88 = t41 * t2 * t50
      t89 = t29 * t28
      t90 = t5 * t89
      t91 = t15 ** 2
      t96 = t4 * t29
      t103 = t81 * t5
      t105 = t41 * t10
      t106 = t25 ** 2
      t110 = t56 * t4
      t112 = x2 ** 2
      t120 = t50 * t45
      t121 = t75 * t120
      t126 = t106 * t41
      t130 = t81 * t57
      t140 = t15 * t41
      t141 = t50 * x3
      t145 = -0.3D1 * t6 * t11 * t25 * t33 * t51 * x3 - 0.2D1 * t58 * t6
     #1 * t28 * t65 * t25 * t67 + 0.2D1 * t71 * t73 * t75 * t76 * t28 - 
     #0.2D1 * t82 * x1 * t48 * t50 - 0.2D1 * t88 * t90 * t91 * t15 + 0.2
     #D1 * t88 * t96 * t91 - t88 * t3 * t28 * t15 + 0.2D1 * t103 * t72 *
     # t105 * t106 - t2 * t110 * t112 * t60 * t29 * t65 * t75 * t50 + 0.
     #3D1 * t6 * t73 * t121 - t58 * t61 * t28 * t65 * t126 * t50 + 0.2D1
     # * t130 * t60 * t41 * t65 * t106 * x2 * t28 - 0.2D1 * t6 * x1 * t2
     #9 * t140 * t141
      t146 = t50 * t2
      t147 = t29 ** 2
      t150 = t146 * t57 * t147 * t32
      t152 = x1 * t15
      t158 = t25 * t28
      t159 = t32 * t41
      t163 = t81 * t110
      t164 = x2 * x1
      t169 = x3 ** 2
      t173 = t2 * t4
      t178 = t81 * t56
      t179 = t9 ** 2
      t180 = 0.1D1 / t179
      t182 = t178 * t72 * t180
      t188 = t32 * t35
      t192 = t146 * t90
      t204 = t65 * t41
      t206 = t45 ** 2
      t230 = -0.2D1 * t150 * t105 * x2 * t152 * x3 - 0.2D1 * t82 * t11 *
     # t158 * t159 + t163 * t164 * t147 * t28 * t10 * t15 * t159 * t169 
     #+ 0.4D1 * t173 * t59 * t10 * t121 + t182 * t106 * t28 * t159 * t45
     # - 0.6D1 * t146 * t96 * t188 * t140 + t192 * t188 * t41 * t91 - 0.
     #3D1 * t146 * t5 * t29 * t32 * t105 * t152 * t45 + t178 * t33 * t20
     #4 * t15 * t59 * t206 + t178 * t89 * t32 * t105 * t91 * x1 * t45 + 
     #0.2D1 * t130 * t147 * t91 * t32 * t10 * t76 * x1 + t182 * t158 * t
     #159 * t206 + t178 * t73 * t106 * t50 * t39
      t233 = t6 * t72 * t65
      t238 = t59 * t35
      t259 = t204 * t112 * t59 * t15
      t265 = t25 * t50
      t271 = t50 * t35
      t275 = t178 * x1
      t276 = t89 * t15
      t295 = -0.3D1 * t233 * t75 * t50 * t206 - 0.3D1 * t6 * t238 * t75 
     #* t50 * t28 * x3 + 0.3D1 * t192 * t188 * t140 * x3 + 0.5D1 * t173 
     #* t11 * t158 * t159 * t50 - t146 * t110 * t147 * t32 * t259 - t150
     # * t105 * t164 * t91 + t178 * t238 * t265 * t29 * t169 + 0.2D1 * t
     #103 * t89 * t91 * t32 * t271 + t275 * t276 * t50 * t169 - 0.4D1 * 
     #t88 * t90 * t91 * x3 - 0.3D1 * t88 * t90 * t15 * t169 + t275 * t89
     # * t91 * t141 + 0.4D1 * t88 * t96 * t15 * x3
      t296 = t6 * t72
      t303 = t10 * t106 * t51
      t305 = t173 * t59
      t307 = t35 * t25 * t51
      t360 = -0.2D1 * t296 * t65 * t106 * t25 * t51 + t296 * t303 - 0.6D
     #1 * t305 * t307 - t2 * t3 * x1 * t307 + 0.5D1 * t173 * x1 * t48 * 
     #t51 + 0.2D1 * t305 * t303 - 0.3D1 * t6 * t59 * t28 * t140 * t271 *
     # t45 - 0.4D1 * t233 * t126 * t120 - 0.2D1 * t6 * t59 * t65 * t25 *
     # t28 * t32 * t67 + 0.2D1 * t71 * t276 * t32 * t10 * t50 * t41 * x2
     # * x1 - 0.2D1 * t163 * t147 * t32 * t259 - 0.2D1 * t163 * t60 * t6
     #5 * t265 * t112 * t29 + t163 * x2 * t60 * x1 * t28 * t180 * t265 *
     # t206
      bbarbbarH32J2 = 0.16D2 / 0.3D1 * wd * (t145 + t230 + t295 + t360) 
     #/ t41 / t50 / s

      end function
  
   
 

      doubleprecision function bbarbbarH32J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t27 * t29
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t24 ** 2
      t37 = z + x1 * t8 * t1
      t40 = t4 ** 2
      t41 = 0.1D1 / t40
      t44 = t14 * t4 + x2 * t7 - t20
      t47 = t24 * x3
      t49 = s - t2 * t6 * t44 - t2 * t47
      t50 = t41 * t49
      t51 = t7 * x1
      t55 = t29 * t32
      t56 = x1 ** 2
      t58 = 0.1D1 / t40 / t4
      t59 = t56 * t58
      t61 = t44 ** 2
      t62 = t61 * t24
      t63 = t37 * t49
      t64 = t63 * t27
      t67 = t31 ** 2
      t68 = t67 * t1
      t70 = t56 ** 2
      t75 = t49 * t27
      t76 = t75 * t21
      t79 = t29 * t67
      t80 = t56 * x1
      t81 = t80 * t41
      t83 = t44 * t49
      t88 = t29 * s
      t90 = t88 * t67 * t31
      t91 = x2 * x1
      t92 = t33 ** 2
      t97 = x3 ** 2
      t101 = t88 * t67
      t102 = t40 ** 2
      t103 = 0.1D1 / t102
      t105 = t101 * t80 * t103
      t106 = t44 * t24
      t107 = t21 ** 2
      t111 = t29 * t31
      t112 = x1 * t41
      t118 = t7 * t49
      t119 = t27 * x3
      t122 = t88 * t32
      t137 = t56 * t5
      t139 = t44 * t27
      t149 = t37 * t5
      t162 = t30 * t32 * t33 * t37 * t50 * t51 * t21 + t55 * t59 * t62 *
     # t64 - t29 * t68 * x2 * t70 * t24 * t58 * t44 * t76 + t79 * t81 * 
     #t83 * t27 * x2 * t24 + t90 * t91 * t92 * t24 * t41 * t7 * t63 * t9
     #7 + t105 * t106 * t63 * t107 + t111 * t112 * t106 * t64 + t55 * x1
     # * t33 * t118 * t119 - 0.2D1 * t122 * t56 * t24 * t7 * t27 * t5 * 
     #t21 + t101 * t33 * t37 * t58 * t49 * t7 * t56 * t107 + t101 * t137
     # * t139 * t33 * t97 + t101 * t81 * t61 * t27 * t47 + t30 * t31 * t
     #33 * t149 * t118 - t30 * t68 * t92 * t37 * t50 * x2 * t51 * x3 + t
     #105 * t62 * t63 * t21
      t163 = t101 * x1
      t164 = t33 * t24
      t165 = t164 * t7
      t178 = t7 ** 2
      t199 = t88 * t68
      t244 = t163 * t165 * t27 * t97 + t90 * x2 * t70 * x1 * t24 * t103 
     #* t139 * t107 + t101 * t164 * t37 * t50 * t178 * x1 * t21 + t111 *
     # x1 * t25 * t75 + t55 * x1 * t33 * t178 * t75 + t163 * t164 * t178
     # * t119 - 0.2D1 * t122 * t112 * t44 * t33 * t63 * x3 - 0.2D1 * t19
     #9 * t70 * t58 * t139 * t21 * x2 * t24 + t111 * t56 * t5 * t44 * t7
     #5 - t30 * t32 * t164 * t149 * t118 * x3 + t55 * t59 * t44 * t24 * 
     #t37 * t76 - t55 * t81 * t83 * t27 * t21 + t55 * t137 * t83 * t27 *
     # t24 * x3 + t79 * t165 * t37 * t41 * t27 * t49 * x2 * x1 - 0.2D1 *
     # t199 * t92 * t37 * t41 * t118 * t91 * x3
      bbarbbarH32J3 = 0.16D2 / 0.3D1 * wd * (t162 + t244) / t49 / t27 / 
     #s

      end function
  
 