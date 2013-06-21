  
      subroutine gbgbH6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH61J1  
      doubleprecision gbgbH61J2  
      doubleprecision gbgbH61J3  
      doubleprecision gbgbH6n1e1  
      doubleprecision gbgbH6n1e0  
      doubleprecision gbgbH6n1em1  
      doubleprecision gbgbH6n1em2  
      doubleprecision gbgbH6n1em3  
      doubleprecision gbgbH6n1em4  
      doubleprecision gbgbH6n2e1  
      doubleprecision gbgbH6n2e0  
      doubleprecision gbgbH6n2em1  
      doubleprecision gbgbH6n2em2  
      doubleprecision gbgbH6n2em3  
      doubleprecision gbgbH6n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH6n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH6n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH6n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH6n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH6n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH6n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH6n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
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
      t23 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t26 = t14 - t16
      t31 = 0.120D3 * t13 * lh
      t33 = 0.60D2 * lh * t15
      t34 = t17 * t10
      t38 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t41 = t17 ** 2
      t43 = -0.2884936567583026D3 - t31 + t33
      t46 = t15 ** 2
      t47 = t13 ** 2
      t57 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t60 = lh * t21
      t61 = x3 * t7
      t64 = log(0.4D1 * t61 * t4)
      t66 = t64 ** 2
      t72 = t26 * t21
      t85 = t43 * t21
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
     #88 / 0.5760D4 - (-0.180D3 * t60 * (t23 - t95 * t38 + t97 * t57 / 0
     #.2D1) + t72 * (t38 - t95 * t57) + 0.90D2 * t21 * (-t95 * t23 + t97
     # * t38 / 0.2D1 - t97 * t95 * t57 / 0.6D1) + t86) * t116 / 0.2880D4
     # - (-0.180D3 * t60 * (t38 - t122 * t57) + 0.90D2 * t21 * (t23 - t1
     #22 * t38 + t128 * t57 / 0.2D1) + t134) * t88 * t116 / 0.2880D4 - (
     #0.90D2 * t21 * (t38 - t142 * t57) - 0.180D3 * t60 * t57) * t88 * t
     #152 / 0.2880D4 - (-0.180D3 * t60 * (t38 - t158 * t57) + 0.90D2 * t
     #21 * (t23 - t158 * t38 + t164 * t57 / 0.2D1) + t134) * t151 * t116
     # / 0.2880D4 - (-0.180D3 * t60 * (t23 - t177 * t38 + t179 * t57 / 0
     #.2D1) + t72 * (t38 - t177 * t57) + 0.90D2 * t21 * (-t177 * t23 + t
     #179 * t38 / 0.2D1 - t179 * t177 * t57 / 0.6D1) + t86) * t151 / 0.5
     #760D4 + (-0.180D3 * t60 * (-t38 + t202 * t57) + 0.90D2 * t21 * (-t
     #23 + t202 * t38 - t208 * t57 / 0.2D1) - t134) * t88 * t151 / 0.576
     #0D4
      t219 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t218)
      t221 = -0.1D1 + x1
      t223 = t2 * x1
      t224 = -t221
      t225 = gbgbH61J3(s, XB1, XB2, z, lh, wd, t224, 0.0D0, 0.0D0, x4)
      t226 = x1 * z
      t227 = 0.1D1 - x1 + t226
      t228 = 0.1D1 / t227
      t229 = t4 * t228
      t230 = t221 ** 2
      t231 = t229 * t230
      t234 = log(0.4D1 * t92 * t231)
      t235 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t224, 0.0D0, 0.0D0, x4)
      t237 = t234 ** 2
      t238 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t224, 0.0D0, 0.0D0, x4)
      t259 = t119 * t7
      t262 = log(0.4D1 * t259 * t231)
      t268 = t262 ** 2
      t274 = t72 * t238
      t278 = t139 * t91
      t283 = log(0.4D1 * t278 * t8 * t228 * t230)
      t293 = t155 * t7
      t296 = log(0.4D1 * t293 * t231)
      t302 = t296 ** 2
      t312 = -(0.180D3 * t60 * (t225 - t234 * t235 + t237 * t238 / 0.2D1
     #) - t72 * (t235 - t234 * t238) - 0.90D2 * t21 * (-t234 * t225 + t2
     #37 * t235 / 0.2D1 - t237 * t234 * t238 / 0.6D1) - t85 * t238) * t1
     #16 / 0.2880D4 - (-0.180D3 * t60 * (-t235 + t262 * t238) + 0.90D2 *
     # t21 * (-t225 + t262 * t235 - t268 * t238 / 0.2D1) - t274) * t88 *
     # t116 / 0.2880D4 - (0.90D2 * t21 * (-t235 + t283 * t238) + 0.180D3
     # * t60 * t238) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (-t235 
     #+ t296 * t238) + 0.90D2 * t21 * (-t225 + t296 * t235 - t302 * t238
     # / 0.2D1) - t274) * t151 * t116 / 0.2880D4
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t221, t223, 0.0D0, t3
     #12)
      t316 = -0.1D1 + x3
      t318 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t319 = t8 * t316
      t322 = log(-0.4D1 * t119 * t319)
      t323 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t328 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t330 = t322 ** 2
      t336 = t72 * t323
      t343 = log(-0.4D1 * t278 * t319)
      t356 = log(-0.4D1 * t139 * t319)
      t362 = t356 ** 2
      t375 = log(-0.4D1 * t61 * t4 * t316)
      t377 = t375 ** 2
      t399 = -(-0.180D3 * t60 * (-t318 + t322 * t323) + 0.90D2 * t21 * (
     #-t328 + t322 * t318 - t330 * t323 / 0.2D1) - t336) * t88 * t116 / 
     #0.2880D4 - (0.90D2 * t21 * (-t318 + t343 * t323) + 0.180D3 * t60 *
     # t323) * t88 * t152 / 0.2880D4 + (-0.180D3 * t60 * (t318 - t356 * 
     #t323) + 0.90D2 * t21 * (t328 - t356 * t318 + t362 * t323 / 0.2D1) 
     #+ t336) * t88 * t151 / 0.5760D4 + (-0.180D3 * t60 * (t328 - t375 *
     # t318 + t377 * t323 / 0.2D1) + t72 * (t318 - t375 * t323) + 0.90D2
     # * t21 * (-t375 * t328 + t377 * t318 / 0.2D1 - t377 * t375 * t323 
     #/ 0.6D1) + t85 * t323) * t88 / 0.5760D4
      t400 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t316, 0.0D0, 0.0D0,
     # t399)
      t404 = -0.1D1 + x2
      t405 = t404 * s
      t407 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t408 = t8 * t404
      t411 = log(-0.4D1 * t278 * t408)
      t412 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t425 = log(-0.4D1 * t155 * t408)
      t430 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t432 = t425 ** 2
      t438 = t72 * t412
      t443 = t4 * t404
      t446 = log(-0.4D1 * t174 * t443)
      t448 = t446 ** 2
      t472 = log(-0.4D1 * t139 * t408)
      t478 = t472 ** 2
      t488 = -(0.90D2 * t21 * (-t407 + t411 * t412) + 0.180D3 * t60 * t4
     #12) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (-t407 + t425 * t4
     #12) + 0.90D2 * t21 * (-t430 + t425 * t407 - t432 * t412 / 0.2D1) -
     # t438) * t151 * t116 / 0.2880D4 - (0.180D3 * t60 * (t430 - t446 * 
     #t407 + t448 * t412 / 0.2D1) - t72 * (t407 - t446 * t412) - 0.90D2 
     #* t21 * (-t446 * t430 + t448 * t407 / 0.2D1 - t448 * t446 * t412 /
     # 0.6D1) - t85 * t412) * t151 / 0.5760D4 + (-0.180D3 * t60 * (t407 
     #- t472 * t412) + 0.90D2 * t21 * (t430 - t472 * t407 + t478 * t412 
     #/ 0.2D1) + t438) * t88 * t151 / 0.5760D4
      t489 = FJET(XB1, XB2, s, x2 * s * t1, 0.0D0, -t405 * t1, 0.0D0, 0.
     #0D0, t488)
      t491 = x1 * x3
      t492 = t491 * z
      t493 = 0.2D1 * t139
      t494 = t139 * x1
      t495 = t139 * t226
      t496 = cos(t5)
      t501 = Sqrt(x3 * t404 * t227 * x2 * t316)
      t503 = 0.2D1 * t496 * t501
      t508 = t2 * t491
      t509 = x2 * x1
      t511 = 0.1D1 - x1 + t226 - x2 + t509 - t509 * z - x3 + t491 - t492
     # + t493 - t494 + t495 + t503
      t515 = t316 * s
      t517 = t515 * t1 * x1
      t518 = t1 ** 2
      t523 = s * t518 * x2 * t221 * x1 * t228
      t524 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t224, x2, x3, x4)
      t525 = t139 * t92
      t526 = t230 * t404
      t531 = log(0.4D1 * t525 * t229 * t526 * t316)
      t532 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t224, x2, x3, x4)
      t539 = 0.90D2 * t21 * (-t524 + t531 * t532) + 0.180D3 * t60 * t532
      t543 = FJET(XB1, XB2, s, t2 * t221 * (-x3 + t491 - t492 + t493 - t
     #494 + t495 - x2 + t503) * t228, t508, -t2 * t221 * t511 * t228, -t
     #517, -t523, -t539 * t88 * t152 / 0.2880D4)
      t552 = Sqrt(x2 * t404 * x3 * t316)
      t554 = 0.2D1 * t496 * t552
      t559 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t564 = log(0.4D1 * t278 * t8 * t404 * t316)
      t565 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t580 = log(0.4D1 * t139 * t7 * t443 * t316)
      t585 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t587 = t580 ** 2
      t598 = -(0.90D2 * t21 * (t559 - t564 * t565) - 0.180D3 * t60 * t56
     #5) * t88 * t152 / 0.2880D4 + (0.180D3 * t60 * (t559 - t580 * t565)
     # - 0.90D2 * t21 * (t585 - t580 * t559 + t587 * t565 / 0.2D1) - t72
     # * t565) * t88 * t151 / 0.5760D4
      t599 = FJET(XB1, XB2, s, -t2 * (-x3 + t493 - x2 + t554), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t493 + t554), 0.0D0, 0.0D0, t598)
      t603 = t1 * t221
      t605 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t224, 0.0D0, x3, x4)
      t607 = t229 * t230 * t316
      t610 = log(-0.4D1 * t259 * t607)
      t611 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t224, 0.0D0, x3, x4)
      t616 = gbgbH61J3(s, XB1, XB2, z, lh, wd, t224, 0.0D0, x3, x4)
      t618 = t610 ** 2
      t630 = log(-0.4D1 * t525 * t607)
      t641 = -(-0.180D3 * t60 * (t605 - t610 * t611) + 0.90D2 * t21 * (t
     #616 - t610 * t605 + t618 * t611 / 0.2D1) + t72 * t611) * t88 * t11
     #6 / 0.2880D4 - (0.90D2 * t21 * (t605 - t630 * t611) - 0.180D3 * t6
     #0 * t611) * t88 * t152 / 0.2880D4
      t642 = FJET(XB1, XB2, s, -t2 * t221 * x3, t508, t515 * t603, -t517
     #, 0.0D0, t641)
      t648 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t224, x2, 0.0D0, x4)
      t649 = t229 * t526
      t652 = log(-0.4D1 * t525 * t649)
      t653 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t224, x2, 0.0D0, x4)
      t665 = log(-0.4D1 * t293 * t649)
      t670 = gbgbH61J3(s, XB1, XB2, z, lh, wd, t224, x2, 0.0D0, x4)
      t672 = t665 ** 2
      t683 = -(0.90D2 * t21 * (t648 - t652 * t653) - 0.180D3 * t60 * t65
     #3) * t88 * t152 / 0.2880D4 - (-0.180D3 * t60 * (t648 - t665 * t653
     #) + 0.90D2 * t21 * (t670 - t665 * t648 + t672 * t653 / 0.2D1) + t7
     #2 * t653) * t151 * t116 / 0.2880D4
      t684 = FJET(XB1, XB2, s, -t2 * t221 * x2 * t228, 0.0D0, t405 * t60
     #3, t223, -t523, t683)
      gbgbH6n1e1 = t219 * t218 + t313 * t312 + t400 * t399 + t489 * t488
     # - t543 * t539 * t88 * t151 * t116 / 0.2880D4 + t599 * t598 + t642
     # * t641 + t684 * t683

      end function



      doubleprecision function gbgbH6n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t7 * t13)
      t17 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t22 = lh * t4
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x3
      t28 = 0.1D1 / x1
      t32 = 0.1D1 / x2
      t34 = t26 * t32 * t28
      t37 = x2 * t6
      t40 = log(0.4D1 * t37 * t13)
      t49 = t6 * t12
      t52 = log(0.4D1 * t49 * t9)
      t57 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t59 = t52 ** 2
      t65 = lh ** 2
      t66 = 0.180D3 * t65
      t67 = 0.3141592653589793D1 ** 2
      t68 = 0.30D2 * t67
      t69 = t66 - t68
      t70 = t69 * t4
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
      t152 = -(0.90D2 * t4 * (t5 - t16 * t17) - t24) * t26 * t28 / 0.288
     #0D4 - t4 * t17 * t34 / 0.32D2 - (0.90D2 * t4 * (t5 - t40 * t17) - 
     #t24) * t32 * t28 / 0.2880D4 - (-0.180D3 * t22 * (t5 - t52 * t17) +
     # 0.90D2 * t4 * (t57 - t52 * t5 + t59 * t17 / 0.2D1) + t71) * t28 /
     # 0.2880D4 + (-0.180D3 * t22 * (-t5 + t78 * t17) + 0.90D2 * t4 * (-
     #t57 + t78 * t5 - t84 * t17 / 0.2D1) - t71) * t26 / 0.5760D4 + (0.9
     #0D2 * t4 * (-t5 + t96 * t17) + t24) * t26 * t32 / 0.5760D4 - (-0.1
     #80D3 * t22 * (t5 - t108 * t17) + 0.90D2 * t4 * (t57 - t108 * t5 + 
     #t114 * t17 / 0.2D1) + t71) * t32 / 0.5760D4 - (-0.180D3 * lh - 0.9
     #0D2 * t125) * t4 * t57 / 0.5760D4 - (0.180D3 * t125 * lh + t66 - t
     #68 + 0.45D2 * t133) * t4 * t5 / 0.5760D4 - (-t125 * t69 - 0.90D2 *
     # t133 * lh - 0.2884936567583026D3 - 0.120D3 * t65 * lh + 0.60D2 * 
     #lh * t67 - 0.15D2 * t133 * t125) * t4 * t17 / 0.5760D4
      t153 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t152)
      t155 = -0.1D1 + x1
      t157 = t2 * x1
      t158 = -t155
      t159 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t158, 0.0D0, 0.0D0, x4)
      t160 = t7 * t12
      t161 = x1 * z
      t162 = 0.1D1 - x1 + t161
      t163 = 0.1D1 / t162
      t164 = t9 * t163
      t165 = t155 ** 2
      t166 = t164 * t165
      t169 = log(0.4D1 * t160 * t166)
      t170 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t158, 0.0D0, 0.0D0, x4)
      t176 = 0.180D3 * t22 * t170
      t184 = t37 * t12
      t187 = log(0.4D1 * t184 * t166)
      t198 = log(0.4D1 * t49 * t166)
      t203 = gbgbH61J3(s, XB1, XB2, z, lh, wd, t158, 0.0D0, 0.0D0, x4)
      t205 = t198 ** 2
      t215 = -(0.90D2 * t4 * (-t159 + t169 * t170) + t176) * t26 * t28 /
     # 0.2880D4 + t4 * t170 * t34 / 0.32D2 - (0.90D2 * t4 * (-t159 + t18
     #7 * t170) + t176) * t32 * t28 / 0.2880D4 - (0.180D3 * t22 * (t159 
     #- t198 * t170) - 0.90D2 * t4 * (t203 - t198 * t159 + t205 * t170 /
     # 0.2D1) - t70 * t170) * t28 / 0.2880D4
      t216 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t155, t157, 0.0D0, t2
     #15)
      t219 = -0.1D1 + x3
      t221 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t222 = t13 * t219
      t225 = log(-0.4D1 * t7 * t222)
      t226 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t232 = 0.180D3 * t22 * t226
      t243 = log(-0.4D1 * t75 * t9 * t219)
      t248 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t250 = t243 ** 2
      t262 = log(-0.4D1 * t93 * t222)
      t271 = -(0.90D2 * t4 * (-t221 + t225 * t226) + t232) * t26 * t28 /
     # 0.2880D4 + t4 * t226 * t34 / 0.32D2 + (-0.180D3 * t22 * (t221 - t
     #243 * t226) + 0.90D2 * t4 * (t248 - t243 * t221 + t250 * t226 / 0.
     #2D1) + t70 * t226) * t26 / 0.5760D4 + (0.90D2 * t4 * (t221 - t262 
     #* t226) - t232) * t26 * t32 / 0.5760D4
      t272 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t219, 0.0D0, 0.0D0,
     # t271)
      t276 = -0.1D1 + x2
      t277 = t276 * s
      t279 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t283 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t284 = t13 * t276
      t287 = log(-0.4D1 * t37 * t284)
      t293 = 0.180D3 * t22 * t279
      t300 = log(-0.4D1 * t93 * t284)
      t309 = t9 * t276
      t312 = log(-0.4D1 * t105 * t309)
      t317 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t319 = t312 ** 2
      t329 = t4 * t279 * t34 / 0.32D2 - (0.90D2 * t4 * (-t283 + t287 * t
     #279) + t293) * t32 * t28 / 0.2880D4 + (0.90D2 * t4 * (t283 - t300 
     #* t279) - t293) * t26 * t32 / 0.5760D4 - (0.180D3 * t22 * (t283 - 
     #t312 * t279) - 0.90D2 * t4 * (t317 - t312 * t283 + t319 * t279 / 0
     #.2D1) - t70 * t279) * t32 / 0.5760D4
      t330 = FJET(XB1, XB2, s, x2 * s * t1, 0.0D0, -t277 * t1, 0.0D0, 0.
     #0D0, t329)
      t332 = x1 * x3
      t333 = t332 * z
      t334 = 0.2D1 * t93
      t335 = t93 * x1
      t336 = t93 * t161
      t337 = cos(t10)
      t342 = Sqrt(x3 * t276 * t162 * x2 * t219)
      t344 = 0.2D1 * t337 * t342
      t349 = t2 * t332
      t350 = x2 * x1
      t352 = 0.1D1 - x1 + t161 - x2 + t350 - t350 * z - x3 + t332 - t333
     # + t334 - t335 + t336 + t344
      t356 = t219 * s
      t358 = t356 * t1 * x1
      t359 = t1 ** 2
      t364 = s * t359 * x2 * t155 * x1 * t163
      t365 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t158, x2, x3, x4)
      t369 = FJET(XB1, XB2, s, t2 * t155 * (-x3 + t332 - t333 + t334 - t
     #335 + t336 - x2 + t344) * t163, t349, -t2 * t155 * t352 * t163, -t
     #358, -t364, t4 * t365 * t34 / 0.32D2)
      t377 = Sqrt(x2 * t276 * x3 * t219)
      t379 = 0.2D1 * t337 * t377
      t384 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t388 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t393 = log(0.4D1 * t93 * t12 * t309 * t219)
      t404 = -t4 * t384 * t34 / 0.32D2 + (-0.90D2 * t4 * (t388 - t393 * 
     #t384) + 0.180D3 * t22 * t384) * t26 * t32 / 0.5760D4
      t405 = FJET(XB1, XB2, s, -t2 * (-x3 + t334 - x2 + t379), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t334 + t379), 0.0D0, 0.0D0, t404)
      t409 = t1 * t155
      t411 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t158, 0.0D0, x3, x4)
      t416 = log(-0.4D1 * t160 * t164 * t165 * t219)
      t417 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t158, 0.0D0, x3, x4)
      t431 = -(0.90D2 * t4 * (t411 - t416 * t417) - 0.180D3 * t22 * t417
     #) * t26 * t28 / 0.2880D4 - t4 * t417 * t34 / 0.32D2
      t432 = FJET(XB1, XB2, s, -t2 * t155 * x3, t349, t356 * t409, -t358
     #, 0.0D0, t431)
      t438 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t158, x2, 0.0D0, x4)
      t442 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t158, x2, 0.0D0, x4)
      t447 = log(-0.4D1 * t184 * t164 * t165 * t276)
      t458 = -t4 * t438 * t34 / 0.32D2 - (0.90D2 * t4 * (t442 - t447 * t
     #438) - 0.180D3 * t22 * t438) * t32 * t28 / 0.2880D4
      t459 = FJET(XB1, XB2, s, -t2 * t155 * x2 * t163, 0.0D0, t277 * t40
     #9, t157, -t364, t458)
      gbgbH6n1e0 = t153 * t152 + t216 * t215 + t272 * t271 + t330 * t329
     # + t369 * t4 * t365 * t34 / 0.32D2 + t405 * t404 + t432 * t431 + t
     #459 * t458

      end function



      doubleprecision function gbgbH6n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x3
      t8 = 0.1D1 / x2
      t9 = t7 * t8
      t12 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x2 * t15
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t21 = log(0.4D1 * t16 * t18)
      t26 = lh * t4
      t28 = 0.180D3 * t26 * t5
      t32 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
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
      t86 = -t6 * t9 / 0.64D2 - (0.90D2 * t4 * (t12 - t21 * t5) - t28) *
     # t8 / 0.5760D4 - t4 * t32 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t38
     #) * t4 * t12 / 0.5760D4 - (0.180D3 * t38 * lh + 0.180D3 * t46 - 0.
     #30D2 * t48 + 0.45D2 * t50) * t4 * t5 / 0.5760D4 - (0.90D2 * t4 * (
     #t12 - t60 * t5) - t28) * t66 / 0.2880D4 - t6 * t69 / 0.32D2 - t6 *
     # t72 / 0.32D2 + (0.90D2 * t4 * (-t12 + t78 * t5) + t28) * t7 / 0.5
     #760D4
      t87 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t86)
      t89 = -0.1D1 + x1
      t91 = t2 * x1
      t92 = -t89
      t93 = gbgbH61J2(s, XB1, XB2, z, lh, wd, t92, 0.0D0, 0.0D0, x4)
      t96 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t98 = t89 ** 2
      t102 = log(0.4D1 * t57 * t18 * t96 * t98)
      t103 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t92, 0.0D0, 0.0D0, x4)
      t113 = t4 * t103
      t118 = -(-0.90D2 * t4 * (t93 - t102 * t103) + 0.180D3 * t26 * t103
     #) * t66 / 0.2880D4 + t113 * t69 / 0.32D2 + t113 * t72 / 0.32D2
      t119 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t89, t91, 0.0D0, t118
     #)
      t122 = -0.1D1 + x3
      t124 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t125 = t4 * t124
      t128 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t132 = log(-0.4D1 * t75 * t18 * t122)
      t144 = t125 * t69 / 0.32D2 + (0.90D2 * t4 * (t128 - t132 * t124) -
     # 0.180D3 * t26 * t124) * t7 / 0.5760D4 + t125 * t9 / 0.64D2
      t145 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t122, 0.0D0, 0.0D0,
     # t144)
      t149 = -0.1D1 + x2
      t150 = t149 * s
      t152 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t153 = t4 * t152
      t158 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t162 = log(-0.4D1 * t16 * t18 * t149)
      t172 = t153 * t72 / 0.32D2 + t153 * t9 / 0.64D2 - (-0.90D2 * t4 * 
     #(t158 - t162 * t152) + 0.180D3 * t26 * t152) * t8 / 0.5760D4
      t173 = FJET(XB1, XB2, s, x2 * s * t1, 0.0D0, -t150 * t1, 0.0D0, 0.
     #0D0, t172)
      t176 = 0.2D1 * x2 * x3
      t177 = cos(t13)
      t181 = Sqrt(x2 * t149 * x3 * t122)
      t183 = 0.2D1 * t177 * t181
      t188 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t192 = FJET(XB1, XB2, s, -t2 * (-x3 + t176 - x2 + t183), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t176 + t183), 0.0D0, 0.0D0, -t4 * t188 * t9 
     #/ 0.64D2)
      t202 = t122 * s
      t203 = t1 * t89
      t207 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t92, 0.0D0, x3, x4)
      t211 = FJET(XB1, XB2, s, -t2 * t89 * x3, t2 * x1 * x3, t202 * t203
     #, -t202 * t1 * x1, 0.0D0, -t4 * t207 * t69 / 0.32D2)
      t221 = t1 ** 2
      t227 = gbgbH61J1(s, XB1, XB2, z, lh, wd, t92, x2, 0.0D0, x4)
      t231 = FJET(XB1, XB2, s, -t2 * t89 * x2 * t96, 0.0D0, t150 * t203,
     # t91, -s * t221 * x2 * t89 * x1 * t96, -t4 * t227 * t72 / 0.32D2)
      gbgbH6n1em1 = t87 * t86 + t119 * t118 + t145 * t144 + t173 * t172 
     #- t192 * t4 * t188 * t7 * t8 / 0.64D2 - t211 * t4 * t207 * t7 * t6
     #6 / 0.32D2 - t231 * t4 * t227 * t8 * t66 / 0.32D2

      end function



      doubleprecision function gbgbH6n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x2
      t10 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t14 = z ** 2
      t17 = Sin(x4 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t21 = log(0.4D1 / t14 * t18)
      t27 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t33 = -t6 * t7 / 0.64D2 - t4 * t10 / 0.64D2 - (-0.180D3 * lh - 0.9
     #0D2 * t21) * t4 * t5 / 0.5760D4 - t6 * t27 / 0.32D2 - t6 * t30 / 0
     #.64D2
      t34 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t33)
      t36 = -0.1D1 + x1
      t40 = gbgbH61J1(s, XB1, XB2, z, lh, wd, -t36, 0.0D0, 0.0D0, x4)
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t36, t2 * x1, 0.0D0, t
     #4 * t40 * t27 / 0.32D2)
      t52 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t56 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, t4 * t52 * t30 / 0.64D2)
      t66 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t70 = FJET(XB1, XB2, s, x2 * s * t1, 0.0D0, -(-0.1D1 + x2) * s * t
     #1, 0.0D0, 0.0D0, t4 * t66 * t7 / 0.64D2)
      gbgbH6n1em2 = t34 * t33 + t44 * t4 * t40 * t27 / 0.32D2 + t56 * t4
     # * t52 * t30 / 0.64D2 + t70 * t4 * t66 * t7 / 0.64D2

      end function



      doubleprecision function gbgbH6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D
     #0, -t4 * t5 / 0.64D2)
      gbgbH6n1em3 = -t8 * t4 * t5 / 0.64D2

      end function



      doubleprecision function gbgbH6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      gbgbH6n1em4 = 0.0D0

      end function


      doubleprecision function gbgbH6n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
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
      t24 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t27 = t18 ** 2
      t30 = 0.120D3 * t14 * lh
      t32 = 0.60D2 * lh * t16
      t33 = -0.2884936567583026D3 - t30 + t32
      t36 = t16 ** 2
      t37 = t14 ** 2
      t41 = t15 - t17
      t44 = t18 * t11
      t49 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t52 = lh * t22
      t53 = x1 ** 2
      t54 = t53 * t8
      t55 = t54 * t5
      t57 = log(0.4D1 * t55)
      t58 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t60 = t57 ** 2
      t66 = t41 * t22
      t79 = t33 * t22
      t80 = t79 * t49
      t82 = 0.1D1 / x1
      t85 = x3 * t53
      t88 = log(0.4D1 * t85 * t9)
      t94 = t88 ** 2
      t102 = 0.1D1 / x3
      t106 = x2 * x3
      t107 = t106 * t53
      t108 = -0.1D1 + x2
      t109 = t9 * t108
      t112 = log(-0.4D1 * t107 * t109)
      t113 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t117 = log(0.4D1 * t106 * t55)
      t119 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t123 = t49 - t113
      t128 = 0.1D1 / x2
      t129 = t128 * t82
      t132 = x2 * t53
      t135 = log(-0.4D1 * t132 * t109)
      t139 = log(0.4D1 * t132 * t9)
      t145 = t139 ** 2
      t148 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t150 = t135 ** 2
      t164 = log(0.4D1 * x3 * t5 * t8)
      t166 = t164 ** 2
      t195 = x2 * t5
      t196 = t8 * t108
      t199 = log(-0.4D1 * t195 * t196)
      t201 = t199 ** 2
      t206 = log(0.4D1 * t195 * t8)
      t208 = t206 ** 2
      t233 = -t123
      t240 = log(-0.4D1 * t106 * t109)
      t244 = log(0.4D1 * t106 * t9)
      t249 = t240 ** 2
      t254 = t244 ** 2
      t265 = -(0.180D3 * t11 * lh + t15 - t17 + 0.45D2 * t18) * t22 * t2
     #4 / 0.5760D4 - (0.15D2 / 0.4D1 * t27 - t11 * t33 + 0.5769873135166
     #051D3 * lh + t36 + 0.60D2 * t37 - 0.60D2 * t14 * t16 + t18 * t41 /
     # 0.2D1 + 0.30D2 * t44 * lh) * t22 * t49 / 0.5760D4 - (-0.180D3 * t
     #52 * (t24 - t57 * t58 + t60 * t49 / 0.2D1) + t66 * (t58 - t57 * t4
     #9) + 0.90D2 * t22 * (-t57 * t24 + t60 * t58 / 0.2D1 - t60 * t57 * 
     #t49 / 0.6D1) + t80) * t82 / 0.2880D4 - (-0.180D3 * t52 * (t58 - t8
     #8 * t49) + 0.90D2 * t22 * (t24 - t88 * t58 + t94 * t49 / 0.2D1) + 
     #t66 * t49) * t102 * t82 / 0.2880D4 - (0.90D2 * t22 * (t112 * t113 
     #+ t58 - t117 * t49 - t119) - 0.180D3 * t52 * t123) * t102 * t129 /
     # 0.2880D4 - (-0.180D3 * t52 * (t58 - t119 + t135 * t113 - t139 * t
     #49) + 0.90D2 * t22 * (t24 - t139 * t58 + t145 * t49 / 0.2D1 - t148
     # + t135 * t119 - t150 * t113 / 0.2D1) + t66 * t123) * t128 * t82 /
     # 0.2880D4 + (-0.180D3 * t52 * (-t24 + t164 * t58 - t166 * t49 / 0.
     #2D1) + t66 * (-t58 + t164 * t49) + 0.90D2 * t22 * (t164 * t24 - t1
     #66 * t58 / 0.2D1 + t166 * t164 * t49 / 0.6D1) - t80) * t102 / 0.57
     #60D4 - (-t11 * t41 - 0.90D2 * t18 * lh - 0.2884936567583026D3 - t3
     #0 + t32 - 0.15D2 * t44) * t22 * t58 / 0.5760D4 + (-0.180D3 * t52 *
     # (t148 - t199 * t119 + t201 * t113 / 0.2D1 - t24 + t206 * t58 - t2
     #08 * t49 / 0.2D1) + t66 * (-t58 + t206 * t49 + t119 - t199 * t113)
     # + 0.90D2 * t22 * (-t199 * t148 + t201 * t119 / 0.2D1 - t201 * t19
     #9 * t113 / 0.6D1 + t206 * t24 - t208 * t58 / 0.2D1 + t208 * t206 *
     # t49 / 0.6D1) + t79 * t233) * t128 / 0.5760D4 + (-0.180D3 * t52 * 
     #(t119 - t240 * t113 - t58 + t244 * t49) + 0.90D2 * t22 * (t249 * t
     #113 / 0.2D1 + t148 - t240 * t119 - t24 + t244 * t58 - t254 * t49 /
     # 0.2D1) + t66 * t233) * t102 * t128 / 0.5760D4
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t265)
      t269 = -0.1D1 + x1
      t270 = t2 * t269
      t271 = gbgbH61J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t272 = 0.1D1 / t3
      t273 = x1 * z
      t274 = -z - x1 + t273
      t275 = 0.1D1 / t274
      t276 = t272 * t275
      t277 = t269 ** 2
      t278 = t276 * t277
      t281 = log(-0.4D1 * t54 * t278)
      t282 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t284 = t281 ** 2
      t285 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t306 = t85 * t8
      t309 = log(-0.4D1 * t306 * t278)
      t315 = t309 ** 2
      t321 = t66 * t285
      t330 = log(-0.4D1 * t107 * t8 * t272 * t277 * t275)
      t340 = t132 * t8
      t343 = log(-0.4D1 * t340 * t278)
      t349 = t343 ** 2
      t359 = -(0.180D3 * t52 * (t271 - t281 * t282 + t284 * t285 / 0.2D1
     #) - t66 * (t282 - t281 * t285) - 0.90D2 * t22 * (-t281 * t271 + t2
     #84 * t282 / 0.2D1 - t284 * t281 * t285 / 0.6D1) - t79 * t285) * t8
     #2 / 0.2880D4 - (-0.180D3 * t52 * (-t282 + t309 * t285) + 0.90D2 * 
     #t22 * (-t271 + t309 * t282 - t315 * t285 / 0.2D1) - t321) * t102 *
     # t82 / 0.2880D4 - (0.90D2 * t22 * (-t282 + t330 * t285) + 0.180D3 
     #* t52 * t285) * t102 * t129 / 0.2880D4 - (-0.180D3 * t52 * (-t282 
     #+ t343 * t285) + 0.90D2 * t22 * (-t271 + t343 * t282 - t349 * t285
     # / 0.2D1) - t321) * t128 * t82 / 0.2880D4
      t360 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x1, -t270, 0.0D0, t359
     #)
      t363 = -0.1D1 + x3
      t365 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t366 = t9 * t363
      t369 = log(-0.4D1 * t85 * t366)
      t370 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t375 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t377 = t369 ** 2
      t388 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t391 = log(-0.4D1 * t107 * t366)
      t397 = log(0.4D1 * t107 * t9 * t108 * t363)
      t398 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t403 = -t370 + t398
      t413 = log(-0.4D1 * t9 * x3 * t363)
      t415 = t413 ** 2
      t439 = log(-0.4D1 * t106 * t366)
      t445 = log(0.4D1 * t106 * t5 * t196 * t363)
      t451 = t439 ** 2
      t455 = t445 ** 2
      t458 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t468 = -(-0.180D3 * t52 * (-t365 + t369 * t370) + 0.90D2 * t22 * (
     #-t375 + t369 * t365 - t377 * t370 / 0.2D1) - t66 * t370) * t102 * 
     #t82 / 0.2880D4 - (0.90D2 * t22 * (t388 - t365 + t391 * t370 - t397
     # * t398) - 0.180D3 * t52 * t403) * t102 * t129 / 0.2880D4 + (-0.18
     #0D3 * t52 * (t375 - t413 * t365 + t415 * t370 / 0.2D1) + t66 * (t3
     #65 - t413 * t370) + 0.90D2 * t22 * (-t413 * t375 + t415 * t365 / 0
     #.2D1 - t415 * t413 * t370 / 0.6D1) + t79 * t370) * t102 / 0.5760D4
     # + (-0.180D3 * t52 * (t365 - t439 * t370 - t388 + t445 * t398) + 0
     #.90D2 * t22 * (t375 - t439 * t365 + t451 * t370 / 0.2D1 + t445 * t
     #388 - t455 * t398 / 0.2D1 - t458) - t66 * t403) * t102 * t128 / 0.
     #5760D4
      t469 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t363, 0.0D0,
     # t468)
      t471 = x1 * x3
      t474 = t2 * t269 * x3
      t475 = t363 * s
      t476 = t1 * x1
      t479 = t475 * t1 * t269
      t480 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t482 = t276 * t277 * t363
      t485 = log(0.4D1 * t306 * t482)
      t486 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t491 = gbgbH61J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t493 = t485 ** 2
      t503 = t106 * t54
      t506 = log(0.4D1 * t503 * t482)
      t517 = -(-0.180D3 * t52 * (t480 - t485 * t486) + 0.90D2 * t22 * (t
     #491 - t485 * t480 + t493 * t486 / 0.2D1) + t66 * t486) * t102 * t8
     #2 / 0.2880D4 - (0.90D2 * t22 * (t480 - t506 * t486) - 0.180D3 * t5
     #2 * t486) * t102 * t129 / 0.2880D4
      t518 = FJET(XB1, XB2, s, t2 * t471, -t474, -t475 * t476, t479, 0.0
     #D0, t517)
      t520 = x3 * z
      t521 = t471 * z
      t522 = t106 * z
      t523 = t106 * x1
      t524 = t106 * t273
      t525 = cos(t6)
      t530 = Sqrt(-x3 * t108 * t274 * x2 * t363)
      t532 = 0.2D1 * t525 * t530
      t538 = x2 * x1
      t540 = z + x1 - t273 - x2 * z - t538 + t538 * z - t520 - t471 + t5
     #21 + t522 + t523 - t524 + t106 + t532
      t544 = t1 ** 2
      t549 = s * t544 * x2 * x1 * t269 * t275
      t550 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t551 = t277 * t108
      t556 = log(-0.4D1 * t503 * t276 * t551 * t363)
      t557 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t564 = 0.90D2 * t22 * (-t550 + t556 * t557) + 0.180D3 * t52 * t557
      t568 = FJET(XB1, XB2, s, t2 * x1 * (-t520 - t471 + t521 + t522 + t
     #523 - t524 - x2 + t106 + t532) * t275, -t474, -t2 * x1 * t540 * t2
     #75, t479, t549, -t564 * t102 * t129 / 0.2880D4)
      t578 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t579 = t276 * t551
      t582 = log(0.4D1 * t503 * t579)
      t583 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t595 = log(0.4D1 * t340 * t579)
      t600 = gbgbH61J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t602 = t595 ** 2
      t613 = -(0.90D2 * t22 * (t578 - t582 * t583) - 0.180D3 * t52 * t58
     #3) * t102 * t129 / 0.2880D4 - (-0.180D3 * t52 * (t578 - t595 * t58
     #3) + 0.90D2 * t22 * (t600 - t595 * t578 + t602 * t583 / 0.2D1) + t
     #66 * t583) * t128 * t82 / 0.2880D4
      t614 = FJET(XB1, XB2, s, -t2 * t538 * t275, 0.0D0, -t108 * s * t47
     #6, -t270, t549, t613)
      gbgbH6n2e1 = t266 * t265 + t360 * t359 + t469 * t468 + t518 * t517
     # - t568 * t564 * t102 * t128 * t82 / 0.2880D4 + t614 * t613

      end function



      doubleprecision function gbgbH6n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t23 = lh * t4
      t27 = 0.1D1 / x3
      t29 = 0.1D1 / x1
      t32 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t33 = t18 - t32
      t35 = 0.1D1 / x2
      t37 = t27 * t35 * t29
      t40 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t41 = x2 * t6
      t42 = -0.1D1 + x2
      t43 = t14 * t42
      t46 = log(-0.4D1 * t41 * t43)
      t50 = log(0.4D1 * t41 * t14)
      t61 = t6 * t13
      t64 = log(0.4D1 * t61 * t10)
      t69 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t71 = t64 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t4
      t83 = t82 * t18
      t90 = log(0.4D1 * x3 * t10 * t13)
      t96 = t90 ** 2
      t105 = x2 * x3
      t108 = log(-0.4D1 * t105 * t43)
      t112 = log(0.4D1 * t105 * t14)
      t117 = -t33
      t124 = x2 * t10
      t127 = log(0.4D1 * t124 * t13)
      t129 = t13 * t42
      t132 = log(-0.4D1 * t124 * t129)
      t137 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t139 = t132 ** 2
      t143 = t127 ** 2
      t155 = log(0.4D1 * t14)
      t163 = t155 ** 2
      t182 = -(0.90D2 * t4 * (t5 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t4 * t33 * t37 / 0.32D2 - (0.90D2 * t4 * (t5
     # - t40 + t46 * t32 - t50 * t18) - 0.180D3 * t23 * t33) * t35 * t29
     # / 0.2880D4 - (-0.180D3 * t23 * (t5 - t64 * t18) + 0.90D2 * t4 * (
     #t69 - t64 * t5 + t71 * t18 / 0.2D1) + t83) * t29 / 0.2880D4 + (-0.
     #180D3 * t23 * (-t5 + t90 * t18) + 0.90D2 * t4 * (-t69 + t90 * t5 -
     # t96 * t18 / 0.2D1) - t83) * t27 / 0.5760D4 + (0.90D2 * t4 * (t40 
     #- t108 * t32 - t5 + t112 * t18) - 0.180D3 * t23 * t117) * t27 * t3
     #5 / 0.5760D4 + (-0.180D3 * t23 * (-t5 + t127 * t18 + t40 - t132 * 
     #t32) + 0.90D2 * t4 * (t137 - t132 * t40 + t139 * t32 / 0.2D1 - t69
     # + t127 * t5 - t143 * t18 / 0.2D1) + t82 * t117) * t35 / 0.5760D4 
     #- (-0.180D3 * lh - 0.90D2 * t155) * t4 * t69 / 0.5760D4 - (0.180D3
     # * t155 * lh + t78 - t80 + 0.45D2 * t163) * t4 * t5 / 0.5760D4 - (
     #-t155 * t81 - 0.90D2 * t163 * lh - 0.2884936567583026D3 - 0.120D3 
     #* t77 * lh + 0.60D2 * lh * t79 - 0.15D2 * t163 * t155) * t4 * t18 
     #/ 0.5760D4
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t182)
      t186 = -0.1D1 + x1
      t187 = t2 * t186
      t188 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t189 = t7 * t13
      t190 = 0.1D1 / t8
      t191 = x1 * z
      t192 = -z - x1 + t191
      t193 = 0.1D1 / t192
      t194 = t190 * t193
      t195 = t186 ** 2
      t196 = t194 * t195
      t199 = log(-0.4D1 * t189 * t196)
      t200 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t206 = 0.180D3 * t23 * t200
      t214 = t41 * t13
      t217 = log(-0.4D1 * t214 * t196)
      t228 = log(-0.4D1 * t61 * t196)
      t233 = gbgbH61J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t235 = t228 ** 2
      t245 = -(0.90D2 * t4 * (-t188 + t199 * t200) + t206) * t27 * t29 /
     # 0.2880D4 + t4 * t200 * t37 / 0.32D2 - (0.90D2 * t4 * (-t188 + t21
     #7 * t200) + t206) * t35 * t29 / 0.2880D4 - (0.180D3 * t23 * (t188 
     #- t228 * t200) - 0.90D2 * t4 * (t233 - t228 * t188 + t235 * t200 /
     # 0.2D1) - t82 * t200) * t29 / 0.2880D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x1, -t187, 0.0D0, t245
     #)
      t249 = -0.1D1 + x3
      t251 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t252 = t14 * t249
      t255 = log(-0.4D1 * t7 * t252)
      t256 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t267 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t268 = -t256 + t267
      t275 = log(-0.4D1 * t14 * x3 * t249)
      t280 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t282 = t275 ** 2
      t294 = log(-0.4D1 * t105 * t252)
      t296 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t301 = log(0.4D1 * t105 * t10 * t129 * t249)
      t313 = -(0.90D2 * t4 * (-t251 + t255 * t256) + 0.180D3 * t23 * t25
     #6) * t27 * t29 / 0.2880D4 - t4 * t268 * t37 / 0.32D2 + (-0.180D3 *
     # t23 * (t251 - t275 * t256) + 0.90D2 * t4 * (t280 - t275 * t251 + 
     #t282 * t256 / 0.2D1) + t82 * t256) * t27 / 0.5760D4 + (0.90D2 * t4
     # * (t251 - t294 * t256 - t296 + t301 * t267) + 0.180D3 * t23 * t26
     #8) * t27 * t35 / 0.5760D4
      t314 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t249, 0.0D0,
     # t313)
      t316 = x1 * x3
      t319 = t2 * t186 * x3
      t320 = t249 * s
      t321 = t1 * x1
      t324 = t320 * t1 * t186
      t325 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t330 = log(0.4D1 * t189 * t194 * t195 * t249)
      t331 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t345 = -(0.90D2 * t4 * (t325 - t330 * t331) - 0.180D3 * t23 * t331
     #) * t27 * t29 / 0.2880D4 - t4 * t331 * t37 / 0.32D2
      t346 = FJET(XB1, XB2, s, t2 * t316, -t319, -t320 * t321, t324, 0.0
     #D0, t345)
      t348 = x3 * z
      t349 = t316 * z
      t350 = t105 * z
      t351 = t105 * x1
      t352 = t105 * t191
      t353 = cos(t11)
      t358 = Sqrt(-x3 * t42 * t192 * x2 * t249)
      t360 = 0.2D1 * t353 * t358
      t366 = x2 * x1
      t368 = z + x1 - t191 - x2 * z - t366 + t366 * z - t348 - t316 + t3
     #49 + t350 + t351 - t352 + t105 + t360
      t372 = t1 ** 2
      t377 = s * t372 * x2 * x1 * t186 * t193
      t378 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t382 = FJET(XB1, XB2, s, t2 * x1 * (-t348 - t316 + t349 + t350 + t
     #351 - t352 - x2 + t105 + t360) * t193, -t319, -t2 * x1 * t368 * t1
     #93, t324, t377, t4 * t378 * t37 / 0.32D2)
      t391 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t395 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t401 = log(0.4D1 * t214 * t190 * t42 * t195 * t193)
      t412 = -t4 * t391 * t37 / 0.32D2 - (0.90D2 * t4 * (t395 - t401 * t
     #391) - 0.180D3 * t23 * t391) * t35 * t29 / 0.2880D4
      t413 = FJET(XB1, XB2, s, -t2 * t366 * t193, 0.0D0, -t42 * s * t321
     #, -t187, t377, t412)
      gbgbH6n2e0 = t183 * t182 + t246 * t245 + t314 * t313 + t346 * t345
     # + t382 * t4 * t378 * t37 / 0.32D2 + t413 * t412

      end function



      doubleprecision function gbgbH6n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t8 = Sin(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t22 = lh * t4
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t35 = t17 - t34
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t41 = gbgbH61J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t45 = t13 * t9
      t47 = log(0.4D1 * t45)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = t47 ** 2
      t65 = -t35
      t67 = t30 * t37
      t70 = x2 * t13
      t73 = log(0.4D1 * t70 * t9)
      t75 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t76 = -0.1D1 + x2
      t80 = log(-0.4D1 * t70 * t9 * t76)
      t93 = log(0.4D1 * x3 * t13 * t9)
      t101 = -(0.90D2 * t4 * (t5 - t16 * t17) - t24) * t26 / 0.2880D4 - 
     #t4 * t17 * t31 / 0.32D2 - t4 * t35 * t38 / 0.32D2 - t4 * t41 / 0.6
     #4D2 - (-0.180D3 * lh - 0.90D2 * t47) * t4 * t5 / 0.5760D4 - (0.180
     #D3 * t47 * lh + 0.180D3 * t55 - 0.30D2 * t57 + 0.45D2 * t59) * t4 
     #* t17 / 0.5760D4 + t4 * t65 * t67 / 0.64D2 + (0.90D2 * t4 * (-t5 +
     # t73 * t17 + t75 - t80 * t34) - 0.180D3 * t22 * t65) * t37 / 0.576
     #0D4 + (0.90D2 * t4 * (-t5 + t93 * t17) + t24) * t30 / 0.5760D4
      t102 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t101)
      t105 = -0.1D1 + x1
      t106 = t2 * t105
      t107 = gbgbH61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t111 = 0.1D1 / (-z - x1 + x1 * z)
      t113 = t105 ** 2
      t117 = log(-0.4D1 * t10 / t11 * t111 * t113)
      t118 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t128 = t4 * t118
      t133 = -(-0.90D2 * t4 * (t107 - t117 * t118) + 0.180D3 * t22 * t11
     #8) * t26 / 0.2880D4 + t128 * t31 / 0.32D2 + t128 * t38 / 0.32D2
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x1, -t106, 0.0D0, t133
     #)
      t137 = -0.1D1 + x3
      t139 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t143 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t148 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t152 = log(-0.4D1 * t45 * x3 * t137)
      t162 = t4 * t139 * t31 / 0.32D2 + t4 * (t139 - t143) * t67 / 0.64D
     #2 + (0.90D2 * t4 * (t148 - t152 * t139) - 0.180D3 * t22 * t139) * 
     #t30 / 0.5760D4
      t163 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t137, 0.0D0,
     # t162)
      t169 = t137 * s
      t170 = t1 * x1
      t174 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t178 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t105 * x3, -t169 * t1
     #70, t169 * t1 * t105, 0.0D0, -t4 * t174 * t31 / 0.32D2)
      t189 = t1 ** 2
      t195 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t199 = FJET(XB1, XB2, s, -t2 * x1 * x2 * t111, 0.0D0, -t76 * s * t
     #170, -t106, s * t189 * x2 * x1 * t105 * t111, -t4 * t195 * t38 / 0
     #.32D2)
      gbgbH6n2em1 = t102 * t101 + t134 * t133 + t163 * t162 - t178 * t4 
     #* t174 * t30 * t26 / 0.32D2 - t199 * t4 * t195 * t37 * t26 / 0.32D
     #2

      end function



      doubleprecision function gbgbH6n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t12 = gbgbH61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t16 = z ** 2
      t20 = Sin(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t16 / z * t21)
      t30 = t4 * t5
      t31 = 0.1D1 / x1
      t34 = 0.1D1 / x3
      t37 = t4 * (-t5 + t6) / x2 / 0.64D2 - t4 * t12 / 0.64D2 - (-0.180D
     #3 * lh - 0.90D2 * t24) * t4 * t5 / 0.5760D4 - t30 * t31 / 0.32D2 -
     # t30 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t43 = gbgbH61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x1, -t2 * (-0.1D1 + x1)
     #, 0.0D0, t4 * t43 * t31 / 0.32D2)
      t55 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, t4 * t55 * t34 / 0.64D2)
      gbgbH6n2em2 = t38 * t37 + t47 * t4 * t43 * t31 / 0.32D2 + t59 * t4
     # * t55 * t34 / 0.64D2

      end function



      doubleprecision function gbgbH6n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D
     #0, -t4 * t5 / 0.64D2)
      gbgbH6n2em3 = -t8 * t4 * t5 / 0.64D2

      end function



      doubleprecision function gbgbH6n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH61J1
      doubleprecision gbgbH61J2
      doubleprecision gbgbH61J3
      gbgbH6n2em4 = 0.0D0

      end function
  
 

      doubleprecision function gbgbH61J1
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
      gbgbH61J1 = -0.16D2 / 0.3D1 * wd * (0.3D1 * t28 * z - t28 - t32 * 
     #t33 * t26 + 0.3D1 * t32 * t33 * t26 * z - 0.4D1 * t40 * t1 * x1 * 
     #t33 * t43 - 0.4D1 * t28 * t43 - t28 * t49 * t56 - t40 * t49 * t1 *
     # t51 * x1 / t52 / t4 * t55 * t20 + 0.2D1 * t40 * t43 * z + 0.3D1 *
     # t40 * z * t49 * t56) / t26

      end function
  
   
 

      doubleprecision function gbgbH61J2
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
      gbgbH61J2 = -0.16D2 / 0.3D1 * wd * (-t4 * t22 * t30 * z - t34 * z 
     #+ t4 * t22 * t30 - t38 * z * t40 * t47 + t38 * t40 * t2 * t42 * x1
     # / t43 / t6 * t46 * t21 + t34 + t34 * t40 * t47) / t30

      end function
  
   
 

      doubleprecision function gbgbH61J3
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
      gbgbH61J3 = -0.16D2 / 0.3D1 * wd * (t4 * t22 * t30 - t4 * t22 * t3
     #0 * z + t30 * t1 * t37 * t44 - t46 * z * t37 * t44 + t46 * t37 * t
     #2 * t39 * x1 / t40 / t6 * t43 * t21) / t30

      end function
  
 