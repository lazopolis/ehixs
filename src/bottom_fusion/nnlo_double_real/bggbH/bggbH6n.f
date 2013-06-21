  
      subroutine bggbH6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH61J1  
      doubleprecision bggbH61J2  
      doubleprecision bggbH61J3  
      doubleprecision bggbH6n1e1  
      doubleprecision bggbH6n1e0  
      doubleprecision bggbH6n1em1  
      doubleprecision bggbH6n1em2  
      doubleprecision bggbH6n1em3  
      doubleprecision bggbH6n1em4  
      doubleprecision bggbH6n2e1  
      doubleprecision bggbH6n2e0  
      doubleprecision bggbH6n2em1  
      doubleprecision bggbH6n2em2  
      doubleprecision bggbH6n2em3  
      doubleprecision bggbH6n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH6n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH6n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH6n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH6n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH6n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH6n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH6n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * 0.3141592653589793D1
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t10 = log(0.4D1 * t8)
      t13 = lh ** 2
      t14 = 0.180D3 * t13
      t15 = 0.3141592653589793D1 ** 2
      t16 = 0.30D2 * t15
      t17 = t10 ** 2
      t20 = s ** 2
      t21 = 0.1D1 / t20
      t23 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t26 = t14 - t16
      t31 = 0.120D3 * t13 * lh
      t33 = 0.60D2 * lh * t15
      t34 = t17 * t10
      t38 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t41 = t17 ** 2
      t43 = -0.2884936567583026D3 - t31 + t33
      t46 = t15 ** 2
      t47 = t13 ** 2
      t57 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t60 = t21 * lh
      t61 = x3 * t7
      t64 = log(0.4D1 * t61 * t4)
      t66 = t64 ** 2
      t72 = t21 * t26
      t85 = t21 * t43
      t86 = t85 * t57
      t88 = 0.1D1 / x3
      t91 = x1 ** 2
      t92 = t91 * t7
      t93 = t92 * t4
      t95 = log(0.4D1 * t93)
      t97 = t95 ** 2
      t116 = 0.1D1 / x1
      t119 = x3 * t91
      t122 = log(0.4D1 * t119 * t8)
      t128 = t122 ** 2
      t134 = t72 * t57
      t139 = x2 * x3
      t142 = log(0.4D1 * t139 * t93)
      t151 = 0.1D1 / x2
      t152 = t151 * t116
      t155 = x2 * t91
      t158 = log(0.4D1 * t155 * t8)
      t164 = t158 ** 2
      t174 = x2 * t7
      t177 = log(0.4D1 * t174 * t4)
      t179 = t177 ** 2
      t202 = log(0.4D1 * t139 * t8)
      t208 = t202 ** 2
      t218 = -(0.180D3 * t10 * lh + t14 - t16 + 0.45D2 * t17) * t21 * t2
     #3 / 0.5760D4 - (-t10 * t26 - 0.90D2 * t17 * lh - 0.288493656758302
     #6D3 - t31 + t33 - 0.15D2 * t34) * t21 * t38 / 0.5760D4 - (0.15D2 /
     # 0.4D1 * t41 - t10 * t43 + 0.5769873135166051D3 * lh + t46 + 0.60D
     #2 * t47 - 0.60D2 * t13 * t15 + t17 * t26 / 0.2D1 + 0.30D2 * t34 * 
     #lh) * t21 * t57 / 0.5760D4 + (-0.180D3 * t60 * (-t23 + t64 * t38 -
     # t66 * t57 / 0.2D1) + t72 * (-t38 + t64 * t57) + 0.90D2 * t21 * (t
     #64 * t23 - t66 * t38 / 0.2D1 + t66 * t64 * t57 / 0.6D1) - t86) * t
     #88 / 0.5760D4 + (-0.180D3 * t60 * (-t23 + t95 * t38 - t97 * t57 / 
     #0.2D1) + t72 * (-t38 + t95 * t57) + 0.90D2 * t21 * (t95 * t23 - t9
     #7 * t38 / 0.2D1 + t97 * t95 * t57 / 0.6D1) - t86) * t116 / 0.2880D
     #4 + (-0.180D3 * t60 * (-t38 + t122 * t57) + 0.90D2 * t21 * (-t23 +
     # t122 * t38 - t128 * t57 / 0.2D1) - t134) * t88 * t116 / 0.2880D4 
     #- (0.90D2 * t21 * (t38 - t142 * t57) - 0.180D3 * t60 * t57) * t88 
     #* t152 / 0.2880D4 - (-0.180D3 * t60 * (t38 - t158 * t57) + 0.90D2 
     #* t21 * (t23 - t158 * t38 + t164 * t57 / 0.2D1) + t134) * t151 * t
     #116 / 0.2880D4 + (-0.180D3 * t60 * (-t23 + t177 * t38 - t179 * t57
     # / 0.2D1) + t72 * (-t38 + t177 * t57) + 0.90D2 * t21 * (t177 * t23
     # - t179 * t38 / 0.2D1 + t179 * t177 * t57 / 0.6D1) - t86) * t151 /
     # 0.5760D4 - (-0.180D3 * t60 * (t38 - t202 * t57) + 0.90D2 * t21 * 
     #(t23 - t202 * t38 + t208 * t57 / 0.2D1) + t134) * t88 * t151 / 0.5
     #760D4
      t219 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t218)
      t221 = t2 * x1
      t222 = -0.1D1 + x1
      t224 = -t222
      t225 = bggbH61J3(s, XB1, XB2, z, lh, wd, t224, 0.0D0, 0.0D0, x4)
      t226 = x1 * z
      t227 = 0.1D1 - x1 + t226
      t228 = 0.1D1 / t227
      t229 = t4 * t228
      t230 = t222 ** 2
      t231 = t229 * t230
      t234 = log(0.4D1 * t92 * t231)
      t235 = bggbH61J2(s, XB1, XB2, z, lh, wd, t224, 0.0D0, 0.0D0, x4)
      t237 = t234 ** 2
      t238 = bggbH61J1(s, XB1, XB2, z, lh, wd, t224, 0.0D0, 0.0D0, x4)
      t259 = t119 * t7
      t262 = log(0.4D1 * t259 * t231)
      t268 = t262 ** 2
      t274 = t72 * t238
      t278 = t139 * t91
      t283 = log(0.4D1 * t278 * t8 * t228 * t230)
      t293 = t155 * t7
      t296 = log(0.4D1 * t293 * t231)
      t302 = t296 ** 2
      t312 = (-0.180D3 * t60 * (t225 - t234 * t235 + t237 * t238 / 0.2D1
     #) + t72 * (t235 - t234 * t238) + 0.90D2 * t21 * (-t234 * t225 + t2
     #37 * t235 / 0.2D1 - t237 * t234 * t238 / 0.6D1) + t85 * t238) * t1
     #16 / 0.2880D4 + (-0.180D3 * t60 * (t235 - t262 * t238) + 0.90D2 * 
     #t21 * (t225 - t262 * t235 + t268 * t238 / 0.2D1) + t274) * t88 * t
     #116 / 0.2880D4 - (0.90D2 * t21 * (-t235 + t283 * t238) + 0.180D3 *
     # t60 * t238) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (-t235 + 
     #t296 * t238) + 0.90D2 * t21 * (-t225 + t296 * t235 - t302 * t238 /
     # 0.2D1) - t274) * t151 * t116 / 0.2880D4
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t221, -t2 * t222, 0.0D0, t3
     #12)
      t316 = -0.1D1 + x3
      t318 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t319 = t8 * t316
      t322 = log(-0.4D1 * t119 * t319)
      t323 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t328 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t330 = t322 ** 2
      t336 = t72 * t323
      t343 = log(-0.4D1 * t278 * t319)
      t356 = log(-0.4D1 * t139 * t319)
      t362 = t356 ** 2
      t375 = log(-0.4D1 * t61 * t4 * t316)
      t377 = t375 ** 2
      t399 = (-0.180D3 * t60 * (t318 - t322 * t323) + 0.90D2 * t21 * (t3
     #28 - t322 * t318 + t330 * t323 / 0.2D1) + t336) * t88 * t116 / 0.2
     #880D4 - (0.90D2 * t21 * (-t318 + t343 * t323) + 0.180D3 * t60 * t3
     #23) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (-t318 + t356 * t3
     #23) + 0.90D2 * t21 * (-t328 + t356 * t318 - t362 * t323 / 0.2D1) -
     # t336) * t88 * t151 / 0.5760D4 + (-0.180D3 * t60 * (t328 - t375 * 
     #t318 + t377 * t323 / 0.2D1) + t72 * (t318 - t375 * t323) + 0.90D2 
     #* t21 * (-t375 * t328 + t377 * t318 / 0.2D1 - t377 * t375 * t323 /
     # 0.6D1) + t85 * t323) * t88 / 0.5760D4
      t400 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t316, 0.0D0,
     # t399)
      t404 = -0.1D1 + x2
      t406 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t407 = t8 * t404
      t410 = log(-0.4D1 * t278 * t407)
      t411 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t424 = log(-0.4D1 * t155 * t407)
      t429 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t431 = t424 ** 2
      t437 = t72 * t411
      t442 = t4 * t404
      t445 = log(-0.4D1 * t174 * t442)
      t447 = t445 ** 2
      t471 = log(-0.4D1 * t139 * t407)
      t477 = t471 ** 2
      t487 = -(0.90D2 * t21 * (-t406 + t410 * t411) + 0.180D3 * t60 * t4
     #11) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (-t406 + t424 * t4
     #11) + 0.90D2 * t21 * (-t429 + t424 * t406 - t431 * t411 / 0.2D1) -
     # t437) * t151 * t116 / 0.2880D4 + (-0.180D3 * t60 * (t429 - t445 *
     # t406 + t447 * t411 / 0.2D1) + t72 * (t406 - t445 * t411) + 0.90D2
     # * t21 * (-t445 * t429 + t447 * t406 / 0.2D1 - t447 * t445 * t411 
     #/ 0.6D1) + t85 * t411) * t151 / 0.5760D4 - (-0.180D3 * t60 * (-t40
     #6 + t471 * t411) + 0.90D2 * t21 * (-t429 + t471 * t406 - t477 * t4
     #11 / 0.2D1) - t437) * t88 * t151 / 0.5760D4
      t488 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t1, 0.0D0, -t2 * t404, 0.
     #0D0, t487)
      t490 = 0.2D1 * t139
      t491 = cos(t5)
      t495 = Sqrt(x2 * t404 * x3 * t316)
      t497 = 0.2D1 * t491 * t495
      t502 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t507 = log(0.4D1 * t278 * t8 * t404 * t316)
      t508 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t523 = log(0.4D1 * t139 * t7 * t442 * t316)
      t528 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t530 = t523 ** 2
      t541 = -(0.90D2 * t21 * (t502 - t507 * t508) - 0.180D3 * t60 * t50
     #8) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (t502 - t523 * t508
     #) + 0.90D2 * t21 * (t528 - t523 * t502 + t530 * t508 / 0.2D1) + t7
     #2 * t508) * t88 * t151 / 0.5760D4
      t542 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t490 - x2 + t497), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t490 + t497), 0.0D0, t541)
      t548 = t1 * t222
      t550 = t1 ** 2
      t555 = s * t550 * x2 * t222 * x1 * t228
      t556 = bggbH61J2(s, XB1, XB2, z, lh, wd, t224, x2, 0.0D0, x4)
      t557 = t139 * t92
      t558 = t230 * t404
      t559 = t229 * t558
      t562 = log(-0.4D1 * t557 * t559)
      t563 = bggbH61J1(s, XB1, XB2, z, lh, wd, t224, x2, 0.0D0, x4)
      t575 = log(-0.4D1 * t293 * t559)
      t580 = bggbH61J3(s, XB1, XB2, z, lh, wd, t224, x2, 0.0D0, x4)
      t582 = t575 ** 2
      t593 = -(0.90D2 * t21 * (t556 - t562 * t563) - 0.180D3 * t60 * t56
     #3) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (t556 - t575 * t563
     #) + 0.90D2 * t21 * (t580 - t575 * t556 + t582 * t563 / 0.2D1) + t7
     #2 * t563) * t151 * t116 / 0.2880D4
      t594 = FJET(XB1, XB2, s, 0.0D0, -t2 * t222 * x2 * t228, t221, t404
     # * s * t548, -t555, t593)
      t596 = x1 * x3
      t597 = t2 * t596
      t598 = t596 * z
      t599 = t139 * x1
      t600 = t139 * t226
      t605 = Sqrt(x3 * t404 * t227 * x2 * t316)
      t607 = 0.2D1 * t491 * t605
      t612 = t316 * s
      t614 = t612 * t1 * x1
      t615 = x2 * x1
      t617 = 0.1D1 - x1 + t226 - x2 + t615 - t615 * z - x3 + t596 - t598
     # + t490 - t599 + t600 + t607
      t621 = bggbH61J2(s, XB1, XB2, z, lh, wd, t224, x2, x3, x4)
      t626 = log(0.4D1 * t557 * t229 * t558 * t316)
      t627 = bggbH61J1(s, XB1, XB2, z, lh, wd, t224, x2, x3, x4)
      t634 = 0.90D2 * t21 * (-t621 + t626 * t627) + 0.180D3 * t60 * t627
      t638 = FJET(XB1, XB2, s, t597, t2 * t222 * (-x3 + t596 - t598 + t4
     #90 - t599 + t600 - x2 + t607) * t228, -t614, -t2 * t222 * t617 * t
     #228, -t555, -t634 * t88 * t152 / 0.2880D4)
      t647 = bggbH61J2(s, XB1, XB2, z, lh, wd, t224, 0.0D0, x3, x4)
      t649 = t229 * t230 * t316
      t652 = log(-0.4D1 * t259 * t649)
      t653 = bggbH61J1(s, XB1, XB2, z, lh, wd, t224, 0.0D0, x3, x4)
      t658 = bggbH61J3(s, XB1, XB2, z, lh, wd, t224, 0.0D0, x3, x4)
      t660 = t652 ** 2
      t672 = log(-0.4D1 * t557 * t649)
      t683 = (0.180D3 * t60 * (t647 - t652 * t653) - 0.90D2 * t21 * (t65
     #8 - t652 * t647 + t660 * t653 / 0.2D1) - t72 * t653) * t88 * t116 
     #/ 0.2880D4 - (0.90D2 * t21 * (t647 - t672 * t653) - 0.180D3 * t60 
     #* t653) * t88 * t152 / 0.2880D4
      t684 = FJET(XB1, XB2, s, t597, -t2 * t222 * x3, -t614, t612 * t548
     #, 0.0D0, t683)
      bggbH6n1e1 = t219 * t218 + t313 * t312 + t400 * t399 + t488 * t487
     # + t542 * t541 + t594 * t593 - t638 * t634 * t88 * t151 * t116 / 0
     #.2880D4 + t684 * t683

      end function



      doubleprecision function bggbH6n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t7 * t13)
      t17 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t22 = t4 * lh
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x3
      t28 = 0.1D1 / x1
      t32 = 0.1D1 / x2
      t34 = t26 * t32 * t28
      t37 = x2 * t6
      t40 = log(0.4D1 * t37 * t13)
      t49 = t6 * t12
      t52 = log(0.4D1 * t49 * t9)
      t57 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t59 = t52 ** 2
      t65 = lh ** 2
      t66 = 0.180D3 * t65
      t67 = 0.3141592653589793D1 ** 2
      t68 = 0.30D2 * t67
      t69 = t66 - t68
      t70 = t4 * t69
      t71 = t70 * t17
      t75 = x3 * t12
      t78 = log(0.4D1 * t75 * t9)
      t84 = t78 ** 2
      t93 = x2 * x3
      t96 = log(0.4D1 * t93 * t13)
      t105 = x2 * t12
      t108 = log(0.4D1 * t105 * t9)
      t114 = t108 ** 2
      t125 = log(0.4D1 * t13)
      t133 = t125 ** 2
      t152 = (0.90D2 * t4 * (-t5 + t16 * t17) + t24) * t26 * t28 / 0.288
     #0D4 - t4 * t17 * t34 / 0.32D2 - (0.90D2 * t4 * (t5 - t40 * t17) - 
     #t24) * t32 * t28 / 0.2880D4 + (-0.180D3 * t22 * (-t5 + t52 * t17) 
     #+ 0.90D2 * t4 * (-t57 + t52 * t5 - t59 * t17 / 0.2D1) - t71) * t28
     # / 0.2880D4 + (-0.180D3 * t22 * (-t5 + t78 * t17) + 0.90D2 * t4 * 
     #(-t57 + t78 * t5 - t84 * t17 / 0.2D1) - t71) * t26 / 0.5760D4 - (0
     #.90D2 * t4 * (t5 - t96 * t17) - t24) * t26 * t32 / 0.5760D4 + (-0.
     #180D3 * t22 * (-t5 + t108 * t17) + 0.90D2 * t4 * (-t57 + t108 * t5
     # - t114 * t17 / 0.2D1) - t71) * t32 / 0.5760D4 - (-0.180D3 * lh - 
     #0.90D2 * t125) * t4 * t57 / 0.5760D4 - (0.180D3 * t125 * lh + t66 
     #- t68 + 0.45D2 * t133) * t4 * t5 / 0.5760D4 - (-t125 * t69 - 0.90D
     #2 * t133 * lh - 0.2884936567583026D3 - 0.120D3 * t65 * lh + 0.60D2
     # * lh * t67 - 0.15D2 * t133 * t125) * t4 * t17 / 0.5760D4
      t153 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t152)
      t155 = t2 * x1
      t156 = -0.1D1 + x1
      t158 = -t156
      t159 = bggbH61J2(s, XB1, XB2, z, lh, wd, t158, 0.0D0, 0.0D0, x4)
      t160 = t7 * t12
      t161 = x1 * z
      t162 = 0.1D1 - x1 + t161
      t163 = 0.1D1 / t162
      t164 = t9 * t163
      t165 = t156 ** 2
      t166 = t164 * t165
      t169 = log(0.4D1 * t160 * t166)
      t170 = bggbH61J1(s, XB1, XB2, z, lh, wd, t158, 0.0D0, 0.0D0, x4)
      t176 = 0.180D3 * t22 * t170
      t184 = t37 * t12
      t187 = log(0.4D1 * t184 * t166)
      t198 = log(0.4D1 * t49 * t166)
      t203 = bggbH61J3(s, XB1, XB2, z, lh, wd, t158, 0.0D0, 0.0D0, x4)
      t205 = t198 ** 2
      t215 = (0.90D2 * t4 * (t159 - t169 * t170) - t176) * t26 * t28 / 0
     #.2880D4 + t4 * t170 * t34 / 0.32D2 - (0.90D2 * t4 * (-t159 + t187 
     #* t170) + t176) * t32 * t28 / 0.2880D4 + (-0.180D3 * t22 * (t159 -
     # t198 * t170) + 0.90D2 * t4 * (t203 - t198 * t159 + t205 * t170 / 
     #0.2D1) + t70 * t170) * t28 / 0.2880D4
      t216 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t155, -t2 * t156, 0.0D0, t2
     #15)
      t219 = -0.1D1 + x3
      t221 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t222 = t13 * t219
      t225 = log(-0.4D1 * t7 * t222)
      t226 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t232 = 0.180D3 * t22 * t226
      t243 = log(-0.4D1 * t75 * t9 * t219)
      t248 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t250 = t243 ** 2
      t262 = log(-0.4D1 * t93 * t222)
      t271 = (0.90D2 * t4 * (t221 - t225 * t226) - t232) * t26 * t28 / 0
     #.2880D4 + t4 * t226 * t34 / 0.32D2 + (-0.180D3 * t22 * (t221 - t24
     #3 * t226) + 0.90D2 * t4 * (t248 - t243 * t221 + t250 * t226 / 0.2D
     #1) + t70 * t226) * t26 / 0.5760D4 - (0.90D2 * t4 * (-t221 + t262 *
     # t226) + t232) * t26 * t32 / 0.5760D4
      t272 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t219, 0.0D0,
     # t271)
      t276 = -0.1D1 + x2
      t278 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t282 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t283 = t13 * t276
      t286 = log(-0.4D1 * t37 * t283)
      t292 = 0.180D3 * t22 * t278
      t299 = log(-0.4D1 * t93 * t283)
      t308 = t9 * t276
      t311 = log(-0.4D1 * t105 * t308)
      t316 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t318 = t311 ** 2
      t328 = t4 * t278 * t34 / 0.32D2 - (0.90D2 * t4 * (-t282 + t286 * t
     #278) + t292) * t32 * t28 / 0.2880D4 - (0.90D2 * t4 * (-t282 + t299
     # * t278) + t292) * t26 * t32 / 0.5760D4 + (-0.180D3 * t22 * (t282 
     #- t311 * t278) + 0.90D2 * t4 * (t316 - t311 * t282 + t318 * t278 /
     # 0.2D1) + t70 * t278) * t32 / 0.5760D4
      t329 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t1, 0.0D0, -t2 * t276, 0.
     #0D0, t328)
      t331 = 0.2D1 * t93
      t332 = cos(t10)
      t336 = Sqrt(x2 * t276 * x3 * t219)
      t338 = 0.2D1 * t332 * t336
      t343 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t347 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t352 = log(0.4D1 * t93 * t12 * t308 * t219)
      t363 = -t4 * t343 * t34 / 0.32D2 - (0.90D2 * t4 * (t347 - t352 * t
     #343) - 0.180D3 * t22 * t343) * t26 * t32 / 0.5760D4
      t364 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t331 - x2 + t338), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t331 + t338), 0.0D0, t363)
      t370 = t1 * t156
      t372 = t1 ** 2
      t377 = s * t372 * x2 * t156 * x1 * t163
      t378 = bggbH61J1(s, XB1, XB2, z, lh, wd, t158, x2, 0.0D0, x4)
      t382 = bggbH61J2(s, XB1, XB2, z, lh, wd, t158, x2, 0.0D0, x4)
      t387 = log(-0.4D1 * t184 * t164 * t165 * t276)
      t398 = -t4 * t378 * t34 / 0.32D2 - (0.90D2 * t4 * (t382 - t387 * t
     #378) - 0.180D3 * t22 * t378) * t32 * t28 / 0.2880D4
      t399 = FJET(XB1, XB2, s, 0.0D0, -t2 * t156 * x2 * t163, t155, t276
     # * s * t370, -t377, t398)
      t401 = x1 * x3
      t402 = t2 * t401
      t403 = t401 * z
      t404 = t93 * x1
      t405 = t93 * t161
      t410 = Sqrt(x3 * t276 * t162 * x2 * t219)
      t412 = 0.2D1 * t332 * t410
      t417 = t219 * s
      t419 = t417 * t1 * x1
      t420 = x2 * x1
      t422 = 0.1D1 - x1 + t161 - x2 + t420 - t420 * z - x3 + t401 - t403
     # + t331 - t404 + t405 + t412
      t426 = bggbH61J1(s, XB1, XB2, z, lh, wd, t158, x2, x3, x4)
      t430 = FJET(XB1, XB2, s, t402, t2 * t156 * (-x3 + t401 - t403 + t3
     #31 - t404 + t405 - x2 + t412) * t163, -t419, -t2 * t156 * t422 * t
     #163, -t377, t4 * t426 * t34 / 0.32D2)
      t438 = bggbH61J2(s, XB1, XB2, z, lh, wd, t158, 0.0D0, x3, x4)
      t443 = log(-0.4D1 * t160 * t164 * t165 * t219)
      t444 = bggbH61J1(s, XB1, XB2, z, lh, wd, t158, 0.0D0, x3, x4)
      t458 = (-0.90D2 * t4 * (t438 - t443 * t444) + 0.180D3 * t22 * t444
     #) * t26 * t28 / 0.2880D4 - t4 * t444 * t34 / 0.32D2
      t459 = FJET(XB1, XB2, s, t402, -t2 * t156 * x3, -t419, t417 * t370
     #, 0.0D0, t458)
      bggbH6n1e0 = t153 * t152 + t216 * t215 + t272 * t271 + t329 * t328
     # + t364 * t363 + t399 * t398 + t430 * t4 * t426 * t34 / 0.32D2 + t
     #459 * t458

      end function



      doubleprecision function bggbH6n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x3
      t8 = 0.1D1 / x2
      t9 = t7 * t8
      t12 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x2 * t15
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t21 = log(0.4D1 * t16 * t18)
      t26 = t4 * lh
      t28 = 0.180D3 * t26 * t5
      t32 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t38 = log(0.4D1 * t18 * t15)
      t46 = lh ** 2
      t48 = 0.3141592653589793D1 ** 2
      t50 = t38 ** 2
      t56 = x1 ** 2
      t57 = t56 * t15
      t60 = log(0.4D1 * t57 * t18)
      t66 = 0.1D1 / x1
      t69 = t7 * t66
      t72 = t8 * t66
      t75 = x3 * t15
      t78 = log(0.4D1 * t75 * t18)
      t86 = -t6 * t9 / 0.64D2 + (0.90D2 * t4 * (-t12 + t21 * t5) + t28) 
     #* t8 / 0.5760D4 - t4 * t32 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t3
     #8) * t4 * t12 / 0.5760D4 - (0.180D3 * t38 * lh + 0.180D3 * t46 - 0
     #.30D2 * t48 + 0.45D2 * t50) * t4 * t5 / 0.5760D4 + (0.90D2 * t4 * 
     #(-t12 + t60 * t5) + t28) * t66 / 0.2880D4 - t6 * t69 / 0.32D2 - t6
     # * t72 / 0.32D2 + (0.90D2 * t4 * (-t12 + t78 * t5) + t28) * t7 / 0
     #.5760D4
      t87 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t86)
      t89 = t2 * x1
      t90 = -0.1D1 + x1
      t92 = -t90
      t93 = bggbH61J2(s, XB1, XB2, z, lh, wd, t92, 0.0D0, 0.0D0, x4)
      t96 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t98 = t90 ** 2
      t102 = log(0.4D1 * t57 * t18 * t96 * t98)
      t103 = bggbH61J1(s, XB1, XB2, z, lh, wd, t92, 0.0D0, 0.0D0, x4)
      t113 = t4 * t103
      t118 = (0.90D2 * t4 * (t93 - t102 * t103) - 0.180D3 * t26 * t103) 
     #* t66 / 0.2880D4 + t113 * t69 / 0.32D2 + t113 * t72 / 0.32D2
      t119 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t89, -t2 * t90, 0.0D0, t118
     #)
      t122 = -0.1D1 + x3
      t124 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t125 = t4 * t124
      t128 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t132 = log(-0.4D1 * t75 * t18 * t122)
      t144 = t125 * t69 / 0.32D2 + (0.90D2 * t4 * (t128 - t132 * t124) -
     # 0.180D3 * t26 * t124) * t7 / 0.5760D4 + t125 * t9 / 0.64D2
      t145 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t122, 0.0D0,
     # t144)
      t149 = -0.1D1 + x2
      t151 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t152 = t4 * t151
      t157 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t161 = log(-0.4D1 * t16 * t18 * t149)
      t171 = t152 * t72 / 0.32D2 + t152 * t9 / 0.64D2 + (0.90D2 * t4 * (
     #t157 - t161 * t151) - 0.180D3 * t26 * t151) * t8 / 0.5760D4
      t172 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t1, 0.0D0, -t2 * t149, 0.
     #0D0, t171)
      t175 = 0.2D1 * x2 * x3
      t176 = cos(t13)
      t180 = Sqrt(x2 * t149 * x3 * t122)
      t182 = 0.2D1 * t176 * t180
      t187 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t191 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t175 - x2 + t182), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t175 + t182), 0.0D0, -t4 * t187 * t9 
     #/ 0.64D2)
      t201 = t1 * t90
      t203 = t1 ** 2
      t209 = bggbH61J1(s, XB1, XB2, z, lh, wd, t92, x2, 0.0D0, x4)
      t213 = FJET(XB1, XB2, s, 0.0D0, -t2 * t90 * x2 * t96, t89, t149 * 
     #s * t201, -s * t203 * x2 * t90 * x1 * t96, -t4 * t209 * t72 / 0.32
     #D2)
      t223 = t122 * s
      t227 = bggbH61J1(s, XB1, XB2, z, lh, wd, t92, 0.0D0, x3, x4)
      t231 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t90 * x3, -t223 * t1 
     #* x1, t223 * t201, 0.0D0, -t4 * t227 * t69 / 0.32D2)
      bggbH6n1em1 = t87 * t86 + t119 * t118 + t145 * t144 + t172 * t171 
     #- t191 * t4 * t187 * t7 * t8 / 0.64D2 - t213 * t4 * t209 * t8 * t6
     #6 / 0.32D2 - t231 * t4 * t227 * t7 * t66 / 0.32D2

      end function



      doubleprecision function bggbH6n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x2
      t10 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t14 = z ** 2
      t17 = Sin(x4 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t21 = log(0.4D1 / t14 * t18)
      t27 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t33 = -t6 * t7 / 0.64D2 - t4 * t10 / 0.64D2 - (-0.180D3 * lh - 0.9
     #0D2 * t21) * t4 * t5 / 0.5760D4 - t6 * t27 / 0.32D2 - t6 * t30 / 0
     #.64D2
      t34 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t33)
      t37 = -0.1D1 + x1
      t40 = bggbH61J1(s, XB1, XB2, z, lh, wd, -t37, 0.0D0, 0.0D0, x4)
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x1, -t2 * t37, 0.0D0, t
     #4 * t40 * t27 / 0.32D2)
      t52 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t56 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, t4 * t52 * t30 / 0.64D2)
      t65 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t69 = FJET(XB1, XB2, s, 0.0D0, x2 * s * t1, 0.0D0, -t2 * (-0.1D1 +
     # x2), 0.0D0, t4 * t65 * t7 / 0.64D2)
      bggbH6n1em2 = t34 * t33 + t44 * t4 * t40 * t27 / 0.32D2 + t56 * t4
     # * t52 * t30 / 0.64D2 + t69 * t4 * t65 * t7 / 0.64D2

      end function



      doubleprecision function bggbH6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D
     #0, -t4 * t5 / 0.64D2)
      bggbH6n1em3 = -t8 * t4 * t5 / 0.64D2

      end function



      doubleprecision function bggbH6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      bggbH6n1em4 = 0.0D0

      end function


      doubleprecision function bggbH6n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = 0.1D1 / t3 / z
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = log(0.4D1 * t9)
      t14 = lh ** 2
      t15 = 0.180D3 * t14
      t16 = 0.3141592653589793D1 ** 2
      t17 = 0.30D2 * t16
      t18 = t11 ** 2
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t24 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t27 = t18 ** 2
      t30 = 0.120D3 * t14 * lh
      t32 = 0.60D2 * lh * t16
      t33 = -0.2884936567583026D3 - t30 + t32
      t36 = t16 ** 2
      t37 = t14 ** 2
      t41 = t15 - t17
      t44 = t18 * t11
      t49 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t52 = t22 * lh
      t53 = x1 ** 2
      t54 = t53 * t8
      t55 = t54 * t5
      t57 = log(0.4D1 * t55)
      t58 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t60 = t57 ** 2
      t66 = t22 * t41
      t79 = t22 * t33
      t80 = t79 * t49
      t82 = 0.1D1 / x1
      t85 = x3 * t53
      t88 = log(0.4D1 * t85 * t9)
      t94 = t88 ** 2
      t102 = 0.1D1 / x3
      t106 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t107 = x2 * x3
      t110 = log(0.4D1 * t107 * t55)
      t112 = t107 * t53
      t113 = -0.1D1 + x2
      t114 = t9 * t113
      t117 = log(-0.4D1 * t112 * t114)
      t118 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t123 = t118 - t49
      t128 = 0.1D1 / x2
      t129 = t128 * t82
      t132 = x2 * t53
      t135 = log(0.4D1 * t132 * t9)
      t139 = log(-0.4D1 * t132 * t114)
      t144 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t146 = t139 ** 2
      t150 = t135 ** 2
      t165 = log(0.4D1 * x3 * t5 * t8)
      t167 = t165 ** 2
      t196 = x2 * t5
      t199 = log(0.4D1 * t196 * t8)
      t201 = t199 ** 2
      t204 = t8 * t113
      t207 = log(-0.4D1 * t196 * t204)
      t209 = t207 ** 2
      t240 = log(-0.4D1 * t107 * t114)
      t244 = log(0.4D1 * t107 * t9)
      t249 = t240 ** 2
      t252 = t244 ** 2
      t265 = -(0.180D3 * t11 * lh + t15 - t17 + 0.45D2 * t18) * t22 * t2
     #4 / 0.5760D4 - (0.15D2 / 0.4D1 * t27 - t11 * t33 + 0.5769873135166
     #051D3 * lh + t36 + 0.60D2 * t37 - 0.60D2 * t14 * t16 + t18 * t41 /
     # 0.2D1 + 0.30D2 * t44 * lh) * t22 * t49 / 0.5760D4 + (-0.180D3 * t
     #52 * (-t24 + t57 * t58 - t60 * t49 / 0.2D1) + t66 * (-t58 + t57 * 
     #t49) + 0.90D2 * t22 * (t57 * t24 - t60 * t58 / 0.2D1 + t60 * t57 *
     # t49 / 0.6D1) - t80) * t82 / 0.2880D4 + (-0.180D3 * t52 * (-t58 + 
     #t88 * t49) + 0.90D2 * t22 * (-t24 + t88 * t58 - t94 * t49 / 0.2D1)
     # - t66 * t49) * t102 * t82 / 0.2880D4 + (0.90D2 * t22 * (t106 - t5
     #8 + t110 * t49 - t117 * t118) - 0.180D3 * t52 * t123) * t102 * t12
     #9 / 0.2880D4 - (-0.180D3 * t52 * (-t106 + t58 - t135 * t49 + t139 
     #* t118) + 0.90D2 * t22 * (-t144 + t139 * t106 - t146 * t118 / 0.2D
     #1 + t24 - t135 * t58 + t150 * t49 / 0.2D1) - t66 * t123) * t128 * 
     #t82 / 0.2880D4 + (-0.180D3 * t52 * (-t24 + t165 * t58 - t167 * t49
     # / 0.2D1) + t66 * (-t58 + t165 * t49) + 0.90D2 * t22 * (t165 * t24
     # - t167 * t58 / 0.2D1 + t167 * t165 * t49 / 0.6D1) - t80) * t102 /
     # 0.5760D4 - (-t11 * t41 - 0.90D2 * t18 * lh - 0.2884936567583026D3
     # - t30 + t32 - 0.15D2 * t44) * t22 * t58 / 0.5760D4 + (-0.180D3 * 
     #t52 * (-t24 + t199 * t58 - t201 * t49 / 0.2D1 + t144 - t207 * t106
     # + t209 * t118 / 0.2D1) + t66 * (t106 - t207 * t118 - t58 + t199 *
     # t49) + 0.90D2 * t22 * (t199 * t24 - t201 * t58 / 0.2D1 + t201 * t
     #199 * t49 / 0.6D1 - t207 * t144 + t209 * t106 / 0.2D1 - t209 * t20
     #7 * t118 / 0.6D1) + t79 * t123) * t128 / 0.5760D4 + (-0.180D3 * t5
     #2 * (t106 - t240 * t118 - t58 + t244 * t49) + 0.90D2 * t22 * (t144
     # + t249 * t118 / 0.2D1 - t252 * t49 / 0.2D1 - t240 * t106 - t24 + 
     #t244 * t58) + t66 * t123) * t102 * t128 / 0.5760D4
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t265)
      t268 = -0.1D1 + x1
      t269 = t2 * t268
      t271 = bggbH61J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t272 = 0.1D1 / t3
      t273 = x1 * z
      t274 = -z - x1 + t273
      t275 = 0.1D1 / t274
      t276 = t272 * t275
      t277 = t268 ** 2
      t278 = t276 * t277
      t281 = log(-0.4D1 * t54 * t278)
      t282 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t284 = t281 ** 2
      t285 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t306 = t85 * t8
      t309 = log(-0.4D1 * t306 * t278)
      t315 = t309 ** 2
      t321 = t66 * t285
      t330 = log(-0.4D1 * t112 * t8 * t272 * t277 * t275)
      t340 = t132 * t8
      t343 = log(-0.4D1 * t340 * t278)
      t349 = t343 ** 2
      t359 = (-0.180D3 * t52 * (t271 - t281 * t282 + t284 * t285 / 0.2D1
     #) + t66 * (t282 - t281 * t285) + 0.90D2 * t22 * (-t281 * t271 + t2
     #84 * t282 / 0.2D1 - t284 * t281 * t285 / 0.6D1) + t79 * t285) * t8
     #2 / 0.2880D4 + (-0.180D3 * t52 * (t282 - t309 * t285) + 0.90D2 * t
     #22 * (t271 - t309 * t282 + t315 * t285 / 0.2D1) + t321) * t102 * t
     #82 / 0.2880D4 + (0.90D2 * t22 * (t282 - t330 * t285) - 0.180D3 * t
     #52 * t285) * t102 * t129 / 0.2880D4 - (-0.180D3 * t52 * (-t282 + t
     #343 * t285) + 0.90D2 * t22 * (-t271 + t343 * t282 - t349 * t285 / 
     #0.2D1) - t321) * t128 * t82 / 0.2880D4
      t360 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t269, t2 * x1, 0.0D0, t359
     #)
      t362 = x2 * x1
      t366 = t1 * x1
      t368 = t1 ** 2
      t373 = s * t368 * x2 * x1 * t268 * t275
      t374 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t375 = t107 * t54
      t376 = t277 * t113
      t377 = t276 * t376
      t380 = log(0.4D1 * t375 * t377)
      t381 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t393 = log(0.4D1 * t340 * t377)
      t398 = bggbH61J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t400 = t393 ** 2
      t411 = (0.90D2 * t22 * (-t374 + t380 * t381) + 0.180D3 * t52 * t38
     #1) * t102 * t129 / 0.2880D4 - (-0.180D3 * t52 * (t374 - t393 * t38
     #1) + 0.90D2 * t22 * (t398 - t393 * t374 + t400 * t381 / 0.2D1) + t
     #66 * t381) * t128 * t82 / 0.2880D4
      t412 = FJET(XB1, XB2, s, 0.0D0, -t2 * t362 * t275, -t269, -t113 * 
     #s * t366, t373, t411)
      t415 = -0.1D1 + x3
      t417 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t418 = t9 * t415
      t421 = log(-0.4D1 * t85 * t418)
      t422 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t427 = t421 ** 2
      t430 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t444 = log(0.4D1 * t112 * t9 * t113 * t415)
      t445 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t449 = log(-0.4D1 * t112 * t418)
      t451 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t455 = -t445 + t422
      t465 = log(-0.4D1 * t9 * x3 * t415)
      t467 = t465 ** 2
      t491 = log(-0.4D1 * t107 * t418)
      t497 = log(0.4D1 * t107 * t5 * t204 * t415)
      t503 = t491 ** 2
      t507 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t508 = t497 ** 2
      t519 = (-0.180D3 * t52 * (t417 - t421 * t422) + 0.90D2 * t22 * (t4
     #27 * t422 / 0.2D1 + t430 - t421 * t417) + t66 * t422) * t102 * t82
     # / 0.2880D4 + (0.90D2 * t22 * (t444 * t445 + t417 - t449 * t422 - 
     #t451) - 0.180D3 * t52 * t455) * t102 * t129 / 0.2880D4 + (-0.180D3
     # * t52 * (t430 - t465 * t417 + t467 * t422 / 0.2D1) + t66 * (t417 
     #- t465 * t422) + 0.90D2 * t22 * (-t465 * t430 + t467 * t417 / 0.2D
     #1 - t467 * t465 * t422 / 0.6D1) + t79 * t422) * t102 / 0.5760D4 + 
     #(-0.180D3 * t52 * (t417 - t491 * t422 - t451 + t497 * t445) + 0.90
     #D2 * t22 * (-t491 * t417 + t503 * t422 / 0.2D1 + t497 * t451 + t43
     #0 - t507 - t508 * t445 / 0.2D1) + t66 * t455) * t102 * t128 / 0.57
     #60D4
      t520 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t415, 0.0D0, 0.0D0,
     # t519)
      t523 = t2 * t268 * x3
      t524 = x1 * x3
      t526 = t415 * s
      t528 = t526 * t1 * t268
      t530 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t532 = t276 * t277 * t415
      t535 = log(0.4D1 * t306 * t532)
      t536 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t541 = bggbH61J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t543 = t535 ** 2
      t555 = log(0.4D1 * t375 * t532)
      t566 = (0.180D3 * t52 * (t530 - t535 * t536) - 0.90D2 * t22 * (t54
     #1 - t535 * t530 + t543 * t536 / 0.2D1) - t66 * t536) * t102 * t82 
     #/ 0.2880D4 + (0.90D2 * t22 * (-t530 + t555 * t536) + 0.180D3 * t52
     # * t536) * t102 * t129 / 0.2880D4
      t567 = FJET(XB1, XB2, s, -t523, t2 * t524, t528, -t526 * t366, 0.0
     #D0, t566)
      t569 = x3 * z
      t570 = t524 * z
      t571 = t107 * z
      t572 = t107 * x1
      t573 = t107 * t273
      t574 = cos(t6)
      t579 = Sqrt(-x3 * t113 * t274 * x2 * t415)
      t581 = 0.2D1 * t574 * t579
      t588 = z + x1 - t273 - x2 * z - t362 + t362 * z - t569 - t524 + t5
     #70 + t571 + t572 - t573 + t107 + t581
      t592 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t597 = log(-0.4D1 * t375 * t276 * t376 * t415)
      t598 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t605 = 0.90D2 * t22 * (t592 - t597 * t598) - 0.180D3 * t52 * t598
      t609 = FJET(XB1, XB2, s, -t523, t2 * x1 * (-t569 - t524 + t570 + t
     #571 + t572 - t573 - x2 + t107 + t581) * t275, t528, -t2 * x1 * t58
     #8 * t275, t373, t605 * t102 * t129 / 0.2880D4)
      bggbH6n2e1 = t266 * t265 + t360 * t359 + t412 * t411 + t520 * t519
     # + t567 * t566 + t609 * t605 * t102 * t128 * t82 / 0.2880D4

      end function



      doubleprecision function bggbH6n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t23 = t4 * lh
      t27 = 0.1D1 / x3
      t29 = 0.1D1 / x1
      t32 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t33 = t32 - t18
      t35 = 0.1D1 / x2
      t37 = t27 * t35 * t29
      t40 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t41 = x2 * t6
      t44 = log(0.4D1 * t41 * t14)
      t46 = -0.1D1 + x2
      t47 = t14 * t46
      t50 = log(-0.4D1 * t41 * t47)
      t62 = t6 * t13
      t65 = log(0.4D1 * t62 * t10)
      t70 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t4 * t82
      t84 = t83 * t18
      t91 = log(0.4D1 * x3 * t10 * t13)
      t97 = t91 ** 2
      t106 = x2 * x3
      t109 = log(-0.4D1 * t106 * t47)
      t113 = log(0.4D1 * t106 * t14)
      t124 = x2 * t10
      t125 = t13 * t46
      t128 = log(-0.4D1 * t124 * t125)
      t132 = log(0.4D1 * t124 * t13)
      t138 = t132 ** 2
      t141 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t143 = t128 ** 2
      t155 = log(0.4D1 * t14)
      t163 = t155 ** 2
      t182 = (0.90D2 * t4 * (-t5 + t17 * t18) + 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 + t4 * t33 * t37 / 0.32D2 - (0.90D2 * t4 * (-t
     #40 + t5 - t44 * t18 + t50 * t32) + 0.180D3 * t23 * t33) * t35 * t2
     #9 / 0.2880D4 + (-0.180D3 * t23 * (-t5 + t65 * t18) + 0.90D2 * t4 *
     # (-t70 + t65 * t5 - t72 * t18 / 0.2D1) - t84) * t29 / 0.2880D4 + (
     #-0.180D3 * t23 * (-t5 + t91 * t18) + 0.90D2 * t4 * (-t70 + t91 * t
     #5 - t97 * t18 / 0.2D1) - t84) * t27 / 0.5760D4 + (0.90D2 * t4 * (t
     #40 - t109 * t32 - t5 + t113 * t18) - 0.180D3 * t23 * t33) * t27 * 
     #t35 / 0.5760D4 + (-0.180D3 * t23 * (t40 - t128 * t32 - t5 + t132 *
     # t18) + 0.90D2 * t4 * (-t70 + t132 * t5 - t138 * t18 / 0.2D1 + t14
     #1 - t128 * t40 + t143 * t32 / 0.2D1) + t83 * t33) * t35 / 0.5760D4
     # - (-0.180D3 * lh - 0.90D2 * t155) * t4 * t70 / 0.5760D4 - (0.180D
     #3 * t155 * lh + t79 - t81 + 0.45D2 * t163) * t4 * t5 / 0.5760D4 - 
     #(-t155 * t82 - 0.90D2 * t163 * lh - 0.2884936567583026D3 - 0.120D3
     # * t78 * lh + 0.60D2 * lh * t80 - 0.15D2 * t163 * t155) * t4 * t18
     # / 0.5760D4
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t182)
      t185 = -0.1D1 + x1
      t186 = t2 * t185
      t188 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t189 = t7 * t13
      t191 = x1 * z
      t192 = -z - x1 + t191
      t193 = 0.1D1 / t192
      t194 = 0.1D1 / t8 * t193
      t195 = t185 ** 2
      t196 = t194 * t195
      t199 = log(-0.4D1 * t189 * t196)
      t200 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t206 = 0.180D3 * t23 * t200
      t214 = t41 * t13
      t217 = log(-0.4D1 * t214 * t196)
      t228 = log(-0.4D1 * t62 * t196)
      t233 = bggbH61J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t235 = t228 ** 2
      t245 = (0.90D2 * t4 * (t188 - t199 * t200) - t206) * t27 * t29 / 0
     #.2880D4 + t4 * t200 * t37 / 0.32D2 - (0.90D2 * t4 * (-t188 + t217 
     #* t200) + t206) * t35 * t29 / 0.2880D4 + (-0.180D3 * t23 * (t188 -
     # t228 * t200) + 0.90D2 * t4 * (t233 - t228 * t188 + t235 * t200 / 
     #0.2D1) + t83 * t200) * t29 / 0.2880D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t186, t2 * x1, 0.0D0, t245
     #)
      t248 = x2 * x1
      t252 = t1 * x1
      t254 = t1 ** 2
      t259 = s * t254 * x2 * x1 * t185 * t193
      t260 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t264 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t269 = log(0.4D1 * t214 * t194 * t195 * t46)
      t280 = -t4 * t260 * t37 / 0.32D2 - (0.90D2 * t4 * (t264 - t269 * t
     #260) - 0.180D3 * t23 * t260) * t35 * t29 / 0.2880D4
      t281 = FJET(XB1, XB2, s, 0.0D0, -t2 * t248 * t193, -t186, -t46 * s
     # * t252, t259, t280)
      t284 = -0.1D1 + x3
      t286 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t287 = t14 * t284
      t290 = log(-0.4D1 * t7 * t287)
      t291 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t302 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t303 = -t302 + t291
      t310 = log(-0.4D1 * t14 * x3 * t284)
      t315 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t317 = t310 ** 2
      t329 = log(-0.4D1 * t106 * t287)
      t331 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t336 = log(0.4D1 * t106 * t10 * t125 * t284)
      t347 = (0.90D2 * t4 * (t286 - t290 * t291) - 0.180D3 * t23 * t291)
     # * t27 * t29 / 0.2880D4 + t4 * t303 * t37 / 0.32D2 + (-0.180D3 * t
     #23 * (t286 - t310 * t291) + 0.90D2 * t4 * (t315 - t310 * t286 + t3
     #17 * t291 / 0.2D1) + t83 * t291) * t27 / 0.5760D4 + (0.90D2 * t4 *
     # (t286 - t329 * t291 - t331 + t336 * t302) - 0.180D3 * t23 * t303)
     # * t27 * t35 / 0.5760D4
      t348 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t284, 0.0D0, 0.0D0,
     # t347)
      t351 = t2 * t185 * x3
      t352 = x1 * x3
      t354 = t284 * s
      t356 = t354 * t1 * t185
      t358 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t363 = log(0.4D1 * t189 * t194 * t195 * t284)
      t364 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t378 = (-0.90D2 * t4 * (t358 - t363 * t364) + 0.180D3 * t23 * t364
     #) * t27 * t29 / 0.2880D4 - t4 * t364 * t37 / 0.32D2
      t379 = FJET(XB1, XB2, s, -t351, t2 * t352, t356, -t354 * t252, 0.0
     #D0, t378)
      t381 = x3 * z
      t382 = t352 * z
      t383 = t106 * z
      t384 = t106 * x1
      t385 = t106 * t191
      t386 = cos(t11)
      t391 = Sqrt(-x3 * t46 * t192 * x2 * t284)
      t393 = 0.2D1 * t386 * t391
      t400 = z + x1 - t191 - x2 * z - t248 + t248 * z - t381 - t352 + t3
     #82 + t383 + t384 - t385 + t106 + t393
      t404 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t408 = FJET(XB1, XB2, s, -t351, t2 * x1 * (-t381 - t352 + t382 + t
     #383 + t384 - t385 - x2 + t106 + t393) * t193, t356, -t2 * x1 * t40
     #0 * t193, t259, t4 * t404 * t37 / 0.32D2)
      bggbH6n2e0 = t183 * t182 + t246 * t245 + t281 * t280 + t348 * t347
     # + t379 * t378 + t408 * t4 * t404 * t37 / 0.32D2

      end function



      doubleprecision function bggbH6n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t8 = Sin(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t22 = t4 * lh
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t35 = t17 - t34
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t41 = bggbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t45 = t13 * t9
      t47 = log(0.4D1 * t45)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = t47 ** 2
      t65 = -t35
      t67 = t30 * t37
      t70 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t71 = x2 * t13
      t72 = -0.1D1 + x2
      t76 = log(-0.4D1 * t71 * t9 * t72)
      t80 = log(0.4D1 * t71 * t9)
      t93 = log(0.4D1 * x3 * t13 * t9)
      t101 = (0.90D2 * t4 * (-t5 + t16 * t17) + t24) * t26 / 0.2880D4 - 
     #t4 * t17 * t31 / 0.32D2 - t4 * t35 * t38 / 0.32D2 - t4 * t41 / 0.6
     #4D2 - (-0.180D3 * lh - 0.90D2 * t47) * t4 * t5 / 0.5760D4 - (0.180
     #D3 * t47 * lh + 0.180D3 * t55 - 0.30D2 * t57 + 0.45D2 * t59) * t4 
     #* t17 / 0.5760D4 + t4 * t65 * t67 / 0.64D2 + (0.90D2 * t4 * (t70 -
     # t76 * t34 - t5 + t80 * t17) - 0.180D3 * t22 * t65) * t37 / 0.5760
     #D4 + (0.90D2 * t4 * (-t5 + t93 * t17) + t24) * t30 / 0.5760D4
      t102 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t101)
      t104 = -0.1D1 + x1
      t105 = t2 * t104
      t107 = bggbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t111 = 0.1D1 / (-z - x1 + x1 * z)
      t113 = t104 ** 2
      t117 = log(-0.4D1 * t10 / t11 * t111 * t113)
      t118 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t128 = t4 * t118
      t133 = (0.90D2 * t4 * (t107 - t117 * t118) - 0.180D3 * t22 * t118)
     # * t26 / 0.2880D4 + t128 * t31 / 0.32D2 + t128 * t38 / 0.32D2
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t105, t2 * x1, 0.0D0, t133
     #)
      t140 = t1 * x1
      t142 = t1 ** 2
      t148 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t152 = FJET(XB1, XB2, s, 0.0D0, -t2 * x1 * x2 * t111, -t105, -t72 
     #* s * t140, s * t142 * x2 * x1 * t104 * t111, -t4 * t148 * t38 / 0
     #.32D2)
      t159 = -0.1D1 + x3
      t161 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t165 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t170 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t174 = log(-0.4D1 * t45 * x3 * t159)
      t184 = t4 * t161 * t31 / 0.32D2 + t4 * (-t165 + t161) * t67 / 0.64
     #D2 + (0.90D2 * t4 * (t170 - t174 * t161) - 0.180D3 * t22 * t161) *
     # t30 / 0.5760D4
      t185 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t159, 0.0D0, 0.0D0,
     # t184)
      t191 = t159 * s
      t195 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t199 = FJET(XB1, XB2, s, -t2 * t104 * x3, t2 * x1 * x3, t191 * t1 
     #* t104, -t191 * t140, 0.0D0, -t4 * t195 * t31 / 0.32D2)
      bggbH6n2em1 = t102 * t101 + t134 * t133 - t152 * t4 * t148 * t37 *
     # t26 / 0.32D2 + t185 * t184 - t199 * t4 * t195 * t30 * t26 / 0.32D
     #2

      end function



      doubleprecision function bggbH6n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t6 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t12 = bggbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t16 = z ** 2
      t20 = Sin(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t16 / z * t21)
      t30 = t4 * t6
      t31 = 0.1D1 / x1
      t34 = 0.1D1 / x3
      t37 = t4 * (t5 - t6) / x2 / 0.64D2 - t4 * t12 / 0.64D2 - (-0.180D3
     # * lh - 0.90D2 * t24) * t4 * t6 / 0.5760D4 - t30 * t31 / 0.32D2 - 
     #t30 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t43 = bggbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * (-0.1D1 + x1), t2 * x1
     #, 0.0D0, t4 * t43 * t31 / 0.32D2)
      t55 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t59 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, t4 * t55 * t34 / 0.64D2)
      bggbH6n2em2 = t38 * t37 + t47 * t4 * t43 * t31 / 0.32D2 + t59 * t4
     # * t55 * t34 / 0.64D2

      end function



      doubleprecision function bggbH6n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D
     #0, -t4 * t5 / 0.64D2)
      bggbH6n2em3 = -t8 * t4 * t5 / 0.64D2

      end function



      doubleprecision function bggbH6n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH61J1
      doubleprecision bggbH61J2
      doubleprecision bggbH61J3
      bggbH6n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bggbH61J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17
      t26 = s - t2 * x1 * t5 * t20 - t2 * (0.1D1 - x1) * x3
      t27 = s ** 2
      t28 = t26 * t27
      t32 = t27 * t1 * x1
      t33 = t5 * t20
      t40 = t27 * s
      t43 = z ** 2
      t49 = t1 ** 2
      t51 = x1 ** 2
      t52 = t4 ** 2
      t55 = t20 ** 2
      t56 = t51 / t52 * t55
      bggbH61J1 = -0.16D2 / 0.3D1 * wd * (0.3D1 * t28 * z - t28 - t32 * 
     #t33 * t26 + 0.3D1 * t32 * t33 * t26 * z - 0.4D1 * t40 * t1 * x1 * 
     #t33 * t43 - 0.4D1 * t28 * t43 - t28 * t49 * t56 - t40 * t49 * t1 *
     # t51 * x1 / t52 / t4 * t55 * t20 + 0.2D1 * t40 * t43 * z + 0.3D1 *
     # t40 * z * t49 * t56) / t26

      end function
  
   
 

      doubleprecision function bggbH61J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t4 = t1 * t2 * x1
      t6 = z + t2 * x1
      t7 = 0.1D1 / t6
      t9 = x3 * (0.1D1 - x2)
      t11 = 0.1D1 - x3
      t14 = cos(x4 * 0.3141592653589793D1)
      t18 = Sqrt(t9 * t6 * x2 * t11)
      t21 = t6 * t9 + x2 * t11 - 0.2D1 * t14 * t18
      t22 = t7 * t21
      t23 = s * t2
      t30 = s - t23 * x1 * t7 * t21 - t23 * (0.1D1 - x1) * x3
      t34 = t30 * t1
      t38 = s * t1
      t40 = t2 ** 2
      t42 = x1 ** 2
      t43 = t6 ** 2
      t46 = t21 ** 2
      t47 = t42 / t43 * t46
      bggbH61J2 = -0.16D2 / 0.3D1 * wd * (-t4 * t22 * t30 * z - t34 * z 
     #+ t4 * t22 * t30 - t38 * z * t40 * t47 + t38 * t40 * t2 * t42 * x1
     # / t43 / t6 * t46 * t21 + t34 + t34 * t40 * t47) / t30

      end function
  
   
 

      doubleprecision function bggbH61J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t4 = t1 * t2 * x1
      t6 = z + t2 * x1
      t7 = 0.1D1 / t6
      t9 = x3 * (0.1D1 - x2)
      t11 = 0.1D1 - x3
      t14 = cos(x4 * 0.3141592653589793D1)
      t18 = Sqrt(t9 * t6 * x2 * t11)
      t21 = t6 * t9 + x2 * t11 - 0.2D1 * t14 * t18
      t22 = t7 * t21
      t23 = s * t2
      t30 = s - t23 * x1 * t7 * t21 - t23 * (0.1D1 - x1) * x3
      t37 = t2 ** 2
      t39 = x1 ** 2
      t40 = t6 ** 2
      t43 = t21 ** 2
      t44 = t39 / t40 * t43
      t46 = s * t1
      bggbH61J3 = -0.16D2 / 0.3D1 * wd * (t4 * t22 * t30 - t4 * t22 * t3
     #0 * z + t30 * t1 * t37 * t44 - t46 * z * t37 * t44 + t46 * t37 * t
     #2 * t39 * x1 / t40 / t6 * t43 * t21) / t30

      end function
  
 