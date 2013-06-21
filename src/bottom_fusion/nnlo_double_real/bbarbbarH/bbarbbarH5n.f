  
      subroutine bbarbbarH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH51J1  
      doubleprecision bbarbbarH51J2  
      doubleprecision bbarbbarH51J3  
      doubleprecision bbarbbarH5n1e1  
      doubleprecision bbarbbarH5n1e0  
      doubleprecision bbarbbarH5n1em1  
      doubleprecision bbarbbarH5n1em2  
      doubleprecision bbarbbarH5n1em3  
      doubleprecision bbarbbarH5n1em4  
      doubleprecision bbarbbarH5n2e1  
      doubleprecision bbarbbarH5n2e0  
      doubleprecision bbarbbarH5n2em1  
      doubleprecision bbarbbarH5n2em2  
      doubleprecision bbarbbarH5n2em3  
      doubleprecision bbarbbarH5n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH5n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH5n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH5n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH5n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH5n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH5n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x
     #4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
      t19 = t16 ** 2
      t20 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = t4 * t30
      t48 = -0.2884936567583026D3 - 0.120D3 * t26 * lh + 0.60D2 * lh * t
     #28
      t49 = t4 * t48
      t50 = t49 * t20
      t52 = 0.1D1 / x1
      t55 = x3 * t7
      t56 = t13 * t10
      t59 = log(0.4D1 * t55 * t56)
      t65 = t59 ** 2
      t71 = t31 * t20
      t73 = 0.1D1 / x3
      t77 = x2 * x3
      t80 = log(0.4D1 * t77 * t14)
      t89 = 0.1D1 / x2
      t90 = t89 * t52
      t93 = x2 * t7
      t96 = log(0.4D1 * t93 * t56)
      t102 = t96 ** 2
      t113 = log(0.4D1 * t56)
      t114 = t113 ** 2
      t117 = t114 * t113
      t120 = t114 ** 2
      t127 = t28 ** 2
      t128 = t26 ** 2
      t150 = x3 * t10
      t153 = log(0.4D1 * t150 * t13)
      t155 = t153 ** 2
      t176 = x2 * t10
      t179 = log(0.4D1 * t176 * t13)
      t181 = t179 ** 2
      t204 = log(0.4D1 * t77 * t56)
      t210 = t204 ** 2
      t220 = -(-0.180D3 * t5 * (t6 - t16 * t17 + t19 * t20 / 0.2D1) + t3
     #1 * (t17 - t16 * t20) + 0.90D2 * t4 * (-t16 * t6 + t19 * t17 / 0.2
     #D1 - t19 * t16 * t20 / 0.6D1) + t50) * t52 / 0.2880D4 + (-0.180D3 
     #* t5 * (-t17 + t59 * t20) + 0.90D2 * t4 * (-t6 + t59 * t17 - t65 *
     # t20 / 0.2D1) - t71) * t73 * t52 / 0.2880D4 - (0.90D2 * t4 * (t17 
     #- t80 * t20) - 0.180D3 * t5 * t20) * t73 * t90 / 0.2880D4 + (-0.18
     #0D3 * t5 * (-t17 + t96 * t20) + 0.90D2 * t4 * (-t6 + t96 * t17 - t
     #102 * t20 / 0.2D1) - t71) * t89 * t52 / 0.2880D4 - (0.45D2 * t114 
     #* t6 - 0.15D2 * t117 * t17 + 0.15D2 / 0.4D1 * t120 * t20 + (t17 - 
     #t113 * t20) * t48 + t20 * (0.5769873135166051D3 * lh + t127 + 0.60
     #D2 * t128 - 0.60D2 * t26 * t28) + (t6 - t113 * t17 + t114 * t20 / 
     #0.2D1) * t30 - 0.180D3 * (-t113 * t6 + t114 * t17 / 0.2D1 - t117 *
     # t20 / 0.6D1) * lh) * t4 / 0.5760D4 + (-0.180D3 * t5 * (-t6 + t153
     # * t17 - t155 * t20 / 0.2D1) + t31 * (-t17 + t153 * t20) + 0.90D2 
     #* t4 * (t153 * t6 - t155 * t17 / 0.2D1 + t155 * t153 * t20 / 0.6D1
     #) - t50) * t73 / 0.5760D4 - (-0.180D3 * t5 * (t6 - t179 * t17 + t1
     #81 * t20 / 0.2D1) + t31 * (t17 - t179 * t20) + 0.90D2 * t4 * (-t17
     #9 * t6 + t181 * t17 / 0.2D1 - t181 * t179 * t20 / 0.6D1) + t50) * 
     #t89 / 0.5760D4 + (-0.180D3 * t5 * (-t17 + t204 * t20) + 0.90D2 * t
     #4 * (-t6 + t204 * t17 - t210 * t20 / 0.2D1) - t71) * t73 * t89 / 0
     #.5760D4
      t221 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t220)
      t223 = t2 * x1
      t224 = -0.1D1 + x1
      t225 = t2 * t224
      t226 = -t224
      t227 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, t226, 0.0D0, 0.0D0, x
     #4)
      t228 = x1 * z
      t229 = 0.1D1 - x1 + t228
      t230 = 0.1D1 / t229
      t231 = t13 * t230
      t232 = t224 ** 2
      t233 = t231 * t232
      t236 = log(0.4D1 * t11 * t233)
      t237 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t226, 0.0D0, 0.0D0, x
     #4)
      t239 = t236 ** 2
      t240 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t226, 0.0D0, 0.0D0, x
     #4)
      t243 = -t227 + t236 * t237 - t239 * t240 / 0.2D1
      t247 = -t237 + t236 * t240
      t255 = t236 * t227 - t239 * t237 / 0.2D1 + t239 * t236 * t240 / 0.
     #6D1
      t258 = t49 * t240
      t261 = t55 * t10
      t264 = log(0.4D1 * t261 * t233)
      t270 = t264 ** 2
      t276 = t31 * t240
      t279 = (-0.180D3 * t5 * (t237 - t264 * t240) + 0.90D2 * t4 * (t227
     # - t264 * t237 + t270 * t240 / 0.2D1) + t276) * t73 * t52
      t280 = t77 * t7
      t285 = log(0.4D1 * t280 * t56 * t230 * t232)
      t294 = (0.90D2 * t4 * (-t237 + t285 * t240) + 0.180D3 * t5 * t240)
     # * t73 * t90
      t295 = t93 * t10
      t298 = log(0.4D1 * t295 * t233)
      t304 = t298 ** 2
      t312 = (-0.180D3 * t5 * (t237 - t298 * t240) + 0.90D2 * t4 * (t227
     # - t298 * t237 + t304 * t240 / 0.2D1) + t276) * t89 * t52
      t314 = -(-0.180D3 * t5 * t243 + t31 * t247 + 0.90D2 * t4 * t255 - 
     #t258) * t52 / 0.2880D4 + t279 / 0.2880D4 - t294 / 0.2880D4 + t312 
     #/ 0.2880D4
      t315 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t223, -t225, 0.0D0, t314)
      t317 = t2 * x3
      t318 = -0.1D1 + x3
      t319 = t2 * t318
      t320 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t324 = log(-0.4D1 * t150 * t13 * t318)
      t325 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t327 = t324 ** 2
      t328 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t350 = t56 * t318
      t353 = log(-0.4D1 * t55 * t350)
      t359 = t353 ** 2
      t365 = t31 * t328
      t372 = log(-0.4D1 * t280 * t350)
      t385 = log(-0.4D1 * t77 * t350)
      t391 = t385 ** 2
      t401 = (-0.180D3 * t5 * (t320 - t324 * t325 + t327 * t328 / 0.2D1)
     # + t31 * (t325 - t324 * t328) + 0.90D2 * t4 * (-t324 * t320 + t327
     # * t325 / 0.2D1 - t327 * t324 * t328 / 0.6D1) + t49 * t328) * t73 
     #/ 0.5760D4 + (-0.180D3 * t5 * (t325 - t353 * t328) + 0.90D2 * t4 *
     # (t320 - t353 * t325 + t359 * t328 / 0.2D1) + t365) * t73 * t52 / 
     #0.2880D4 - (0.90D2 * t4 * (-t325 + t372 * t328) + 0.180D3 * t5 * t
     #328) * t73 * t90 / 0.2880D4 + (-0.180D3 * t5 * (t325 - t385 * t328
     #) + 0.90D2 * t4 * (t320 - t385 * t325 + t391 * t328 / 0.2D1) + t36
     #5) * t73 * t89 / 0.5760D4
      t402 = FJET(XB1, XB2, s, 0.0D0, t317, 0.0D0, -t319, 0.0D0, t401)
      t405 = x2 * s * t1
      t406 = -0.1D1 + x2
      t407 = t406 * s
      t408 = t407 * t1
      t409 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t410 = t13 * t406
      t413 = log(-0.4D1 * t176 * t410)
      t414 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t416 = t413 ** 2
      t417 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t438 = (-0.180D3 * t5 * (-t409 + t413 * t414 - t416 * t417 / 0.2D1
     #) + t31 * (-t414 + t413 * t417) + 0.90D2 * t4 * (t413 * t409 - t41
     #6 * t414 / 0.2D1 + t416 * t413 * t417 / 0.6D1) - t49 * t417) * t89
     # / 0.5760D4
      t439 = t56 * t406
      t442 = log(-0.4D1 * t77 * t439)
      t444 = t414 - t442 * t417
      t448 = t442 ** 2
      t451 = t409 - t442 * t414 + t448 * t417 / 0.2D1
      t454 = t31 * t417
      t461 = log(-0.4D1 * t280 * t439)
      t471 = (0.90D2 * t4 * (-t414 + t461 * t417) + 0.180D3 * t5 * t417)
     # * t73 * t90 / 0.2880D4
      t474 = log(-0.4D1 * t93 * t439)
      t476 = t414 - t474 * t417
      t480 = t474 ** 2
      t483 = t409 - t474 * t414 + t480 * t417 / 0.2D1
      t490 = -t438 + (-0.180D3 * t5 * t444 + 0.90D2 * t4 * t451 + t454) 
     #* t73 * t89 / 0.5760D4 - t471 + (-0.180D3 * t5 * t476 + 0.90D2 * t
     #4 * t483 + t454) * t89 * t52 / 0.2880D4
      t491 = FJET(XB1, XB2, s, 0.0D0, t405, 0.0D0, -t408, 0.0D0, t490)
      t493 = 0.2D1 * t77
      t494 = cos(t8)
      t495 = x3 * t406
      t498 = Sqrt(t495 * x2 * t318)
      t500 = 0.2D1 * t494 * t498
      t502 = t2 * (-x3 + t493 - x2 + t500)
      t504 = t2 * (0.1D1 - x2 - x3 + t493 + t500)
      t505 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t510 = log(0.4D1 * t77 * t10 * t410 * t318)
      t511 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t516 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t518 = t510 ** 2
      t533 = log(0.4D1 * t280 * t56 * t406 * t318)
      t544 = (-0.180D3 * t5 * (-t505 + t510 * t511) + 0.90D2 * t4 * (-t5
     #16 + t510 * t505 - t518 * t511 / 0.2D1) - t31 * t511) * t73 * t89 
     #/ 0.5760D4 - (0.90D2 * t4 * (t505 - t533 * t511) - 0.180D3 * t5 * 
     #t511) * t73 * t90 / 0.2880D4
      t545 = FJET(XB1, XB2, s, 0.0D0, -t502, 0.0D0, t504, 0.0D0, t544)
      t549 = t2 * t224 * x2 * t230
      t550 = t1 * t224
      t551 = t407 * t550
      t552 = t1 ** 2
      t557 = s * t552 * x2 * t224 * x1 * t230
      t558 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t226, x2, 0.0D0, x4)
      t559 = t77 * t11
      t560 = t232 * t406
      t561 = t231 * t560
      t564 = log(-0.4D1 * t559 * t561)
      t565 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t226, x2, 0.0D0, x4)
      t577 = log(-0.4D1 * t295 * t561)
      t582 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, t226, x2, 0.0D0, x4)
      t584 = t577 ** 2
      t595 = -(0.90D2 * t4 * (t558 - t564 * t565) - 0.180D3 * t5 * t565)
     # * t73 * t90 / 0.2880D4 + (-0.180D3 * t5 * (-t558 + t577 * t565) +
     # 0.90D2 * t4 * (-t582 + t577 * t558 - t584 * t565 / 0.2D1) - t31 *
     # t565) * t89 * t52 / 0.2880D4
      t596 = FJET(XB1, XB2, s, 0.0D0, -t549, t223, t551, -t557, t595)
      t598 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t220)
      t600 = FJET(XB1, XB2, s, t504, 0.0D0, -t502, 0.0D0, 0.0D0, t544)
      t602 = x1 * x3
      t603 = t2 * t602
      t604 = t602 * z
      t605 = t77 * x1
      t606 = t77 * t228
      t610 = Sqrt(t495 * t229 * x2 * t318)
      t612 = 0.2D1 * t494 * t610
      t616 = t2 * t224 * (-x3 + t602 - t604 + t493 - t605 + t606 - x2 + 
     #t612) * t230
      t617 = t318 * s
      t619 = t617 * t1 * x1
      t620 = x2 * x1
      t622 = 0.1D1 - x1 + t228 - x2 + t620 - t620 * z - x3 + t602 - t604
     # + t493 - t605 + t606 + t612
      t625 = t2 * t224 * t622 * t230
      t626 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t226, x2, x3, x4)
      t631 = log(0.4D1 * t559 * t231 * t560 * t318)
      t632 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t226, x2, x3, x4)
      t634 = -t626 + t631 * t632
      t638 = 0.180D3 * t5 * t632
      t639 = 0.90D2 * t4 * t634 + t638
      t643 = FJET(XB1, XB2, s, t603, t616, -t619, -t625, -t557, -t639 * 
     #t73 * t90 / 0.2880D4)
      t646 = t73 * t89 * t52
      t650 = t2 * t224 * x3
      t651 = t617 * t550
      t652 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t226, 0.0D0, x3, x4)
      t654 = t231 * t232 * t318
      t657 = log(-0.4D1 * t261 * t654)
      t658 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t226, 0.0D0, x3, x4)
      t663 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, t226, 0.0D0, x3, x4)
      t665 = t657 ** 2
      t677 = log(-0.4D1 * t559 * t654)
      t688 = (-0.180D3 * t5 * (-t652 + t657 * t658) + 0.90D2 * t4 * (-t6
     #63 + t657 * t652 - t665 * t658 / 0.2D1) - t31 * t658) * t73 * t52 
     #/ 0.2880D4 - (0.90D2 * t4 * (t652 - t677 * t658) - 0.180D3 * t5 * 
     #t658) * t73 * t90 / 0.2880D4
      t689 = FJET(XB1, XB2, s, t603, -t650, -t619, t651, 0.0D0, t688)
      t691 = FJET(XB1, XB2, s, t551, t223, -t549, 0.0D0, -t557, t595)
      t693 = FJET(XB1, XB2, s, t651, -t619, -t650, t603, 0.0D0, t688)
      t706 = -(-0.180D3 * t5 * t243 + t31 * t247 + 0.90D2 * t4 * t255 - 
     #t258) * t52 / 0.2880D4 + t279 / 0.2880D4 - t294 / 0.2880D4 + t312 
     #/ 0.2880D4
      t707 = FJET(XB1, XB2, s, -t225, t223, 0.0D0, 0.0D0, 0.0D0, t706)
      t709 = FJET(XB1, XB2, s, -t319, 0.0D0, t317, 0.0D0, 0.0D0, t401)
      t731 = -t438 + (-0.180D3 * t5 * t444 + 0.90D2 * t4 * t451 + t454) 
     #* t73 * t89 / 0.5760D4 - t471 + (-0.180D3 * t5 * t476 + 0.90D2 * t
     #4 * t483 + t454) * t89 * t52 / 0.2880D4
      t732 = FJET(XB1, XB2, s, -t408, 0.0D0, t405, 0.0D0, 0.0D0, t731)
      t737 = 0.90D2 * t4 * t634 + t638
      t741 = FJET(XB1, XB2, s, -t625, -t619, t616, t603, -t557, -t737 * 
     #t73 * t90 / 0.2880D4)
      bbarbbarH5n1e1 = t221 * t220 + t315 * t314 + t402 * t401 + t491 * 
     #t490 + t545 * t544 + t596 * t595 + t598 * t220 + t600 * t544 - t64
     #3 * t639 * t646 / 0.2880D4 + t689 * t688 + t691 * t595 + t693 * t6
     #88 + t707 * t706 + t709 * t401 + t732 * t731 - t741 * t737 * t646 
     #/ 0.2880D4

      end function



      doubleprecision function bbarbbarH5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x
     #4)
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t7 * t13)
      t17 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
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
      t57 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
      t59 = t52 ** 2
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t69 = 0.180D3 * t65 - 0.30D2 * t67
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
      t124 = log(0.4D1 * t13)
      t129 = t124 ** 2
      t151 = (0.90D2 * t4 * (-t5 + t16 * t17) + t24) * t26 * t28 / 0.288
     #0D4 - t4 * t17 * t34 / 0.32D2 + (0.90D2 * t4 * (-t5 + t40 * t17) +
     # t24) * t32 * t28 / 0.2880D4 - (-0.180D3 * t22 * (t5 - t52 * t17) 
     #+ 0.90D2 * t4 * (t57 - t52 * t5 + t59 * t17 / 0.2D1) + t71) * t28 
     #/ 0.2880D4 + (-0.180D3 * t22 * (-t5 + t78 * t17) + 0.90D2 * t4 * (
     #-t57 + t78 * t5 - t84 * t17 / 0.2D1) - t71) * t26 / 0.5760D4 + (0.
     #90D2 * t4 * (-t5 + t96 * t17) + t24) * t26 * t32 / 0.5760D4 - (-0.
     #180D3 * t22 * (t5 - t108 * t17) + 0.90D2 * t4 * (t57 - t108 * t5 +
     # t114 * t17 / 0.2D1) + t71) * t32 / 0.5760D4 - ((t5 - t124 * t17) 
     #* t69 - 0.180D3 * (t57 - t124 * t5 + t129 * t17 / 0.2D1) * lh + t1
     #7 * (-0.2884936567583026D3 - 0.120D3 * t65 * lh + 0.60D2 * lh * t6
     #7) - 0.90D2 * t124 * t57 + 0.45D2 * t129 * t5 - 0.15D2 * t129 * t1
     #24 * t17) * t4 / 0.5760D4
      t152 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t151)
      t154 = t2 * x1
      t155 = -0.1D1 + x1
      t156 = t2 * t155
      t157 = -t155
      t158 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t157, 0.0D0, 0.0D0, x
     #4)
      t159 = t7 * t12
      t160 = x1 * z
      t161 = 0.1D1 - x1 + t160
      t162 = 0.1D1 / t161
      t163 = t9 * t162
      t164 = t155 ** 2
      t165 = t163 * t164
      t168 = log(0.4D1 * t159 * t165)
      t169 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t157, 0.0D0, 0.0D0, x
     #4)
      t175 = 0.180D3 * t22 * t169
      t179 = (0.90D2 * t4 * (t158 - t168 * t169) - t175) * t26 * t28 / 0
     #.2880D4
      t182 = t4 * t169 * t34 / 0.32D2
      t183 = t37 * t12
      t186 = log(0.4D1 * t183 * t165)
      t194 = (0.90D2 * t4 * (t158 - t186 * t169) - t175) * t32 * t28 / 0
     #.2880D4
      t197 = log(0.4D1 * t49 * t165)
      t199 = -t158 + t197 * t169
      t202 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, t157, 0.0D0, 0.0D0, x
     #4)
      t204 = t197 ** 2
      t207 = -t202 + t197 * t158 - t204 * t169 / 0.2D1
      t210 = t70 * t169
      t214 = t179 + t182 + t194 - (-0.180D3 * t22 * t199 + 0.90D2 * t4 *
     # t207 - t210) * t28 / 0.2880D4
      t215 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t154, -t156, 0.0D0, t214)
      t217 = t2 * x3
      t218 = -0.1D1 + x3
      t219 = t2 * t218
      t220 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t221 = t13 * t218
      t224 = log(-0.4D1 * t7 * t221)
      t225 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t231 = 0.180D3 * t22 * t225
      t242 = log(-0.4D1 * t75 * t9 * t218)
      t247 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t249 = t242 ** 2
      t261 = log(-0.4D1 * t93 * t221)
      t270 = (0.90D2 * t4 * (t220 - t224 * t225) - t231) * t26 * t28 / 0
     #.2880D4 + t4 * t225 * t34 / 0.32D2 + (-0.180D3 * t22 * (t220 - t24
     #2 * t225) + 0.90D2 * t4 * (t247 - t242 * t220 + t249 * t225 / 0.2D
     #1) + t70 * t225) * t26 / 0.5760D4 + (0.90D2 * t4 * (t220 - t261 * 
     #t225) - t231) * t26 * t32 / 0.5760D4
      t271 = FJET(XB1, XB2, s, 0.0D0, t217, 0.0D0, -t219, 0.0D0, t270)
      t274 = x2 * s * t1
      t275 = -0.1D1 + x2
      t276 = t275 * s
      t277 = t276 * t1
      t278 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t279 = t13 * t275
      t282 = log(-0.4D1 * t93 * t279)
      t283 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t285 = t278 - t282 * t283
      t289 = 0.180D3 * t22 * t283
      t294 = t9 * t275
      t297 = log(-0.4D1 * t105 * t294)
      t302 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t304 = t297 ** 2
      t313 = (-0.180D3 * t22 * (-t278 + t297 * t283) + 0.90D2 * t4 * (-t
     #302 + t297 * t278 - t304 * t283 / 0.2D1) - t70 * t283) * t32 / 0.5
     #760D4
      t316 = t4 * t283 * t34 / 0.32D2
      t319 = log(-0.4D1 * t37 * t279)
      t321 = t278 - t319 * t283
      t328 = (0.90D2 * t4 * t285 - t289) * t26 * t32 / 0.5760D4 - t313 +
     # t316 + (0.90D2 * t4 * t321 - t289) * t32 * t28 / 0.2880D4
      t329 = FJET(XB1, XB2, s, 0.0D0, t274, 0.0D0, -t277, 0.0D0, t328)
      t331 = 0.2D1 * t93
      t332 = cos(t10)
      t333 = x3 * t275
      t336 = Sqrt(t333 * x2 * t218)
      t338 = 0.2D1 * t332 * t336
      t340 = t2 * (-x3 + t331 - x2 + t338)
      t342 = t2 * (0.1D1 - x2 - x3 + t331 + t338)
      t343 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t347 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t352 = log(0.4D1 * t93 * t12 * t294 * t218)
      t363 = -t4 * t343 * t34 / 0.32D2 + (0.90D2 * t4 * (-t347 + t352 * 
     #t343) + 0.180D3 * t22 * t343) * t26 * t32 / 0.5760D4
      t364 = FJET(XB1, XB2, s, 0.0D0, -t340, 0.0D0, t342, 0.0D0, t363)
      t368 = t2 * t155 * x2 * t162
      t369 = t1 * t155
      t370 = t276 * t369
      t371 = t1 ** 2
      t376 = s * t371 * x2 * t155 * x1 * t162
      t377 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t157, x2, 0.0D0, x4)
      t381 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t157, x2, 0.0D0, x4)
      t386 = log(-0.4D1 * t183 * t163 * t164 * t275)
      t397 = -t4 * t377 * t34 / 0.32D2 + (0.90D2 * t4 * (-t381 + t386 * 
     #t377) + 0.180D3 * t22 * t377) * t32 * t28 / 0.2880D4
      t398 = FJET(XB1, XB2, s, 0.0D0, -t368, t154, t370, -t376, t397)
      t400 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t151)
      t402 = FJET(XB1, XB2, s, t342, 0.0D0, -t340, 0.0D0, 0.0D0, t363)
      t404 = x1 * x3
      t405 = t2 * t404
      t406 = t404 * z
      t407 = t93 * x1
      t408 = t93 * t160
      t412 = Sqrt(t333 * t161 * x2 * t218)
      t414 = 0.2D1 * t332 * t412
      t418 = t2 * t155 * (-x3 + t404 - t406 + t331 - t407 + t408 - x2 + 
     #t414) * t162
      t419 = t218 * s
      t421 = t419 * t1 * x1
      t422 = x2 * x1
      t424 = 0.1D1 - x1 + t160 - x2 + t422 - t422 * z - x3 + t404 - t406
     # + t331 - t407 + t408 + t414
      t427 = t2 * t155 * t424 * t162
      t428 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t157, x2, x3, x4)
      t431 = t4 * t428 * t34 / 0.32D2
      t432 = FJET(XB1, XB2, s, t405, t418, -t421, -t427, -t376, t431)
      t438 = t2 * t155 * x3
      t439 = t419 * t369
      t440 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t157, 0.0D0, x3, x4)
      t445 = log(-0.4D1 * t159 * t163 * t164 * t218)
      t446 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t157, 0.0D0, x3, x4)
      t460 = (0.90D2 * t4 * (-t440 + t445 * t446) + 0.180D3 * t22 * t446
     #) * t26 * t28 / 0.2880D4 - t4 * t446 * t34 / 0.32D2
      t461 = FJET(XB1, XB2, s, t405, -t438, -t421, t439, 0.0D0, t460)
      t463 = FJET(XB1, XB2, s, t370, t154, -t368, 0.0D0, -t376, t397)
      t465 = FJET(XB1, XB2, s, t439, -t421, -t438, t405, 0.0D0, t460)
      t476 = t179 + t182 + t194 - (-0.180D3 * t22 * t199 + 0.90D2 * t4 *
     # t207 - t210) * t28 / 0.2880D4
      t477 = FJET(XB1, XB2, s, -t156, t154, 0.0D0, 0.0D0, 0.0D0, t476)
      t479 = FJET(XB1, XB2, s, -t219, 0.0D0, t217, 0.0D0, 0.0D0, t270)
      t495 = (0.90D2 * t4 * t285 - t289) * t26 * t32 / 0.5760D4 - t313 +
     # t316 + (0.90D2 * t4 * t321 - t289) * t32 * t28 / 0.2880D4
      t496 = FJET(XB1, XB2, s, -t277, 0.0D0, t274, 0.0D0, 0.0D0, t495)
      t498 = FJET(XB1, XB2, s, -t427, -t421, t418, t405, -t376, t431)
      bbarbbarH5n1e0 = t152 * t151 + t215 * t214 + t271 * t270 + t329 * 
     #t328 + t364 * t363 + t398 * t397 + t400 * t151 + t402 * t363 + t43
     #2 * t4 * t428 * t34 / 0.32D2 + t461 * t460 + t463 * t397 + t465 * 
     #t460 + t477 * t476 + t479 * t270 + t496 * t495 + t498 * t4 * t428 
     #* t34 / 0.32D2

      end function



      doubleprecision function bbarbbarH5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x
     #4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
      t21 = t4 * lh
      t23 = 0.180D3 * t21 * t16
      t25 = 0.1D1 / x1
      t28 = t4 * t16
      t29 = 0.1D1 / x3
      t30 = t29 * t25
      t33 = 0.1D1 / x2
      t34 = t33 * t25
      t37 = t29 * t33
      t40 = x2 * t9
      t43 = log(0.4D1 * t40 * t12)
      t53 = log(0.4D1 * t9 * t12)
      t58 = lh ** 2
      t60 = 0.3141592653589793D1 ** 2
      t64 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
      t68 = t53 ** 2
      t74 = x3 * t9
      t77 = log(0.4D1 * t74 * t12)
      t85 = -(0.90D2 * t4 * (t5 - t15 * t16) - t23) * t25 / 0.2880D4 - t
     #28 * t30 / 0.32D2 - t28 * t34 / 0.32D2 - t28 * t37 / 0.64D2 - (0.9
     #0D2 * t4 * (t5 - t43 * t16) - t23) * t33 / 0.5760D4 - (-0.180D3 * 
     #(t5 - t53 * t16) * lh + t16 * (0.180D3 * t58 - 0.30D2 * t60) + 0.9
     #0D2 * t64 - 0.90D2 * t53 * t5 + 0.45D2 * t68 * t16) * t4 / 0.5760D
     #4 + (0.90D2 * t4 * (-t5 + t77 * t16) + t23) * t29 / 0.5760D4
      t86 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t85)
      t88 = t2 * x1
      t89 = -0.1D1 + x1
      t90 = t2 * t89
      t91 = -t89
      t92 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, t91, 0.0D0, 0.0D0, x4)
      t95 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t97 = t89 ** 2
      t101 = log(0.4D1 * t10 * t12 * t95 * t97)
      t102 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t91, 0.0D0, 0.0D0, x4
     #)
      t104 = -t92 + t101 * t102
      t108 = 0.180D3 * t21 * t102
      t112 = t4 * t102
      t114 = t112 * t30 / 0.32D2
      t116 = t112 * t34 / 0.32D2
      t117 = -(0.90D2 * t4 * t104 + t108) * t25 / 0.2880D4 + t114 + t116
      t118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t88, -t90, 0.0D0, t117)
      t120 = t2 * x3
      t121 = -0.1D1 + x3
      t122 = t2 * t121
      t123 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t124 = t4 * t123
      t129 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t133 = log(-0.4D1 * t74 * t12 * t121)
      t143 = t124 * t37 / 0.64D2 + t124 * t30 / 0.32D2 + (0.90D2 * t4 * 
     #(t129 - t133 * t123) - 0.180D3 * t21 * t123) * t29 / 0.5760D4
      t144 = FJET(XB1, XB2, s, 0.0D0, t120, 0.0D0, -t122, 0.0D0, t143)
      t147 = x2 * s * t1
      t148 = -0.1D1 + x2
      t149 = t148 * s
      t150 = t149 * t1
      t151 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t152 = t4 * t151
      t155 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4
     #)
      t159 = log(-0.4D1 * t40 * t12 * t148)
      t171 = t152 * t37 / 0.64D2 - (0.90D2 * t4 * (-t155 + t159 * t151) 
     #+ 0.180D3 * t21 * t151) * t33 / 0.5760D4 + t152 * t34 / 0.32D2
      t172 = FJET(XB1, XB2, s, 0.0D0, t147, 0.0D0, -t150, 0.0D0, t171)
      t175 = 0.2D1 * x2 * x3
      t176 = cos(t7)
      t180 = Sqrt(x3 * t148 * x2 * t121)
      t182 = 0.2D1 * t176 * t180
      t184 = t2 * (-x3 + t175 - x2 + t182)
      t186 = t2 * (0.1D1 - x2 - x3 + t175 + t182)
      t187 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t190 = t4 * t187 * t37 / 0.64D2
      t191 = FJET(XB1, XB2, s, 0.0D0, -t184, 0.0D0, t186, 0.0D0, -t190)
      t194 = t187 * t29 * t33
      t199 = t2 * t89 * x2 * t95
      t200 = t1 * t89
      t201 = t149 * t200
      t202 = t1 ** 2
      t207 = s * t202 * x2 * t89 * x1 * t95
      t208 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t91, x2, 0.0D0, x4)
      t211 = t4 * t208 * t34 / 0.32D2
      t212 = FJET(XB1, XB2, s, 0.0D0, -t199, t88, t201, -t207, -t211)
      t215 = t208 * t33 * t25
      t218 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t85)
      t220 = FJET(XB1, XB2, s, t186, 0.0D0, -t184, 0.0D0, 0.0D0, -t190)
      t225 = t2 * x1 * x3
      t227 = t2 * t89 * x3
      t228 = t121 * s
      t230 = t228 * t1 * x1
      t231 = t228 * t200
      t232 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, t91, 0.0D0, x3, x4)
      t235 = t4 * t232 * t30 / 0.32D2
      t236 = FJET(XB1, XB2, s, t225, -t227, -t230, t231, 0.0D0, -t235)
      t239 = t232 * t29 * t25
      t242 = FJET(XB1, XB2, s, t201, t88, -t199, 0.0D0, -t207, -t211)
      t246 = FJET(XB1, XB2, s, t231, -t230, -t227, t225, 0.0D0, -t235)
      t256 = -(0.90D2 * t4 * t104 + t108) * t25 / 0.2880D4 + t114 + t116
      t257 = FJET(XB1, XB2, s, -t90, t88, 0.0D0, 0.0D0, 0.0D0, t256)
      t259 = FJET(XB1, XB2, s, -t122, 0.0D0, t120, 0.0D0, 0.0D0, t143)
      t261 = FJET(XB1, XB2, s, -t150, 0.0D0, t147, 0.0D0, 0.0D0, t171)
      bbarbbarH5n1em1 = t86 * t85 + t118 * t117 + t144 * t143 + t172 * t
     #171 - t191 * t4 * t194 / 0.64D2 - t212 * t4 * t215 / 0.32D2 + t218
     # * t85 - t220 * t4 * t194 / 0.64D2 - t236 * t4 * t239 / 0.32D2 - t
     #242 * t4 * t215 / 0.32D2 - t246 * t4 * t239 / 0.32D2 + t257 * t256
     # + t259 * t143 + t261 * t171

      end function



      doubleprecision function bbarbbarH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x
     #4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t15 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, 
     #x4)
      t17 = z ** 2
      t20 = Sin(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t17 * t21)
      t30 = 0.1D1 / x3
      t33 = -t6 * t7 / 0.32D2 - t6 * t10 / 0.64D2 - (-0.180D3 * t5 * lh 
     #+ 0.90D2 * t15 - 0.90D2 * t24 * t5) * t4 / 0.5760D4 - t6 * t30 / 0
     #.64D2
      t34 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t33)
      t36 = t2 * x1
      t37 = -0.1D1 + x1
      t38 = t2 * t37
      t40 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, -t37, 0.0D0, 0.0D0, x4
     #)
      t43 = t4 * t40 * t7 / 0.32D2
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t36, -t38, 0.0D0, t43)
      t46 = t40 * t7
      t49 = t2 * x3
      t51 = t2 * (-0.1D1 + x3)
      t52 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t55 = t4 * t52 * t30 / 0.64D2
      t56 = FJET(XB1, XB2, s, 0.0D0, t49, 0.0D0, -t51, 0.0D0, t55)
      t58 = t52 * t30
      t62 = x2 * s * t1
      t65 = (-0.1D1 + x2) * s * t1
      t66 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t69 = t4 * t66 * t10 / 0.64D2
      t70 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t65, 0.0D0, t69)
      t72 = t66 * t10
      t75 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t33)
      t77 = FJET(XB1, XB2, s, -t38, t36, 0.0D0, 0.0D0, 0.0D0, t43)
      t81 = FJET(XB1, XB2, s, -t51, 0.0D0, t49, 0.0D0, 0.0D0, t55)
      t85 = FJET(XB1, XB2, s, -t65, 0.0D0, t62, 0.0D0, 0.0D0, t69)
      bbarbbarH5n1em2 = t34 * t33 + t44 * t4 * t46 / 0.32D2 + t56 * t4 *
     # t58 / 0.64D2 + t70 * t4 * t72 / 0.64D2 + t75 * t33 + t77 * t4 * t
     #46 / 0.32D2 + t81 * t4 * t58 / 0.64D2 + t85 * t4 * t72 / 0.64D2

      end function



      doubleprecision function bbarbbarH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x
     #4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t7)
      bbarbbarH5n1em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2

      end function



      doubleprecision function bbarbbarH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      bbarbbarH5n1em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH5n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4
     #)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t20 = t17 ** 2
      t21 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t28 - t30
      t32 = t4 * t31
      t46 = 0.120D3 * t27 * lh
      t48 = 0.60D2 * lh * t29
      t49 = -0.2884936567583026D3 - t46 + t48
      t50 = t4 * t49
      t51 = t50 * t21
      t53 = 0.1D1 / x1
      t56 = x3 * t7
      t57 = t14 * t10
      t60 = log(0.4D1 * t56 * t57)
      t66 = t60 ** 2
      t74 = 0.1D1 / x3
      t78 = x2 * x3
      t81 = log(0.4D1 * t78 * t15)
      t83 = t78 * t7
      t84 = -0.1D1 + x2
      t85 = t57 * t84
      t88 = log(-0.4D1 * t83 * t85)
      t89 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t91 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t95 = -t89 + t21
      t100 = 0.1D1 / x2
      t101 = t100 * t53
      t104 = x2 * t7
      t107 = log(0.4D1 * t104 * t57)
      t111 = log(-0.4D1 * t104 * t85)
      t116 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t117 = t107 ** 2
      t122 = t111 ** 2
      t128 = -t95
      t129 = t32 * t128
      t135 = log(0.4D1 * t57)
      t138 = t135 ** 2
      t147 = t138 * t135
      t153 = t138 ** 2
      t157 = t29 ** 2
      t158 = t27 ** 2
      t173 = log(0.4D1 * x3 * t14 * t10)
      t175 = t173 ** 2
      t196 = x2 * t14
      t197 = t10 * t84
      t200 = log(-0.4D1 * t196 * t197)
      t202 = t200 ** 2
      t207 = log(0.4D1 * t196 * t10)
      t209 = t207 ** 2
      t240 = log(0.4D1 * t78 * t57)
      t244 = log(-0.4D1 * t78 * t85)
      t251 = t240 ** 2
      t254 = t244 ** 2
      t264 = -(-0.180D3 * t5 * (t6 - t17 * t18 + t20 * t21 / 0.2D1) + t3
     #2 * (t18 - t17 * t21) + 0.90D2 * t4 * (-t17 * t6 + t20 * t18 / 0.2
     #D1 - t20 * t17 * t21 / 0.6D1) + t51) * t53 / 0.2880D4 + (-0.180D3 
     #* t5 * (t60 * t21 - t18) + 0.90D2 * t4 * (-t6 + t60 * t18 - t66 * 
     #t21 / 0.2D1) - t32 * t21) * t74 * t53 / 0.2880D4 - (0.90D2 * t4 * 
     #(-t81 * t21 + t18 + t88 * t89 - t91) - 0.180D3 * t5 * t95) * t74 *
     # t101 / 0.2880D4 + (-0.180D3 * t5 * (t91 + t107 * t21 - t111 * t89
     # - t18) + 0.90D2 * t4 * (-t6 + t116 - t117 * t21 / 0.2D1 - t111 * 
     #t91 + t107 * t18 + t122 * t89 / 0.2D1) + t129) * t100 * t53 / 0.28
     #80D4 - (0.180D3 * t135 * lh + t28 - t30 + 0.45D2 * t138) * t4 * t6
     # / 0.5760D4 - (-t135 * t31 - 0.90D2 * t138 * lh - 0.28849365675830
     #26D3 - t46 + t48 - 0.15D2 * t147) * t4 * t18 / 0.5760D4 - (0.15D2 
     #/ 0.4D1 * t153 - t135 * t49 + 0.5769873135166051D3 * lh + t157 + 0
     #.60D2 * t158 - 0.60D2 * t27 * t29 + t138 * t31 / 0.2D1 + 0.30D2 * 
     #t147 * lh) * t4 * t21 / 0.5760D4 + (-0.180D3 * t5 * (-t6 + t173 * 
     #t18 - t175 * t21 / 0.2D1) + t32 * (t173 * t21 - t18) + 0.90D2 * t4
     # * (t173 * t6 - t175 * t18 / 0.2D1 + t175 * t173 * t21 / 0.6D1) - 
     #t51) * t74 / 0.5760D4 + (-0.180D3 * t5 * (t116 - t200 * t91 + t202
     # * t89 / 0.2D1 - t6 + t207 * t18 - t209 * t21 / 0.2D1) + t32 * (-t
     #18 + t207 * t21 + t91 - t200 * t89) + 0.90D2 * t4 * (-t200 * t116 
     #+ t202 * t91 / 0.2D1 - t202 * t200 * t89 / 0.6D1 + t207 * t6 - t20
     #9 * t18 / 0.2D1 + t209 * t207 * t21 / 0.6D1) + t50 * t128) * t100 
     #/ 0.5760D4 + (-0.180D3 * t5 * (-t18 + t240 * t21 - t244 * t89 + t9
     #1) + 0.90D2 * t4 * (-t6 + t116 + t240 * t18 - t244 * t91 - t251 * 
     #t21 / 0.2D1 + t254 * t89 / 0.2D1) + t129) * t74 * t100 / 0.5760D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t264)
      t267 = -0.1D1 + x1
      t268 = t2 * t267
      t269 = t2 * x1
      t270 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t271 = 0.1D1 / t12
      t272 = x1 * z
      t273 = -z - x1 + t272
      t274 = 0.1D1 / t273
      t275 = t271 * t274
      t276 = t267 ** 2
      t277 = t275 * t276
      t280 = log(-0.4D1 * t11 * t277)
      t281 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t283 = t280 ** 2
      t284 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t287 = -t270 + t280 * t281 - t283 * t284 / 0.2D1
      t291 = -t281 + t280 * t284
      t299 = t280 * t270 - t283 * t281 / 0.2D1 + t283 * t280 * t284 / 0.
     #6D1
      t302 = t50 * t284
      t305 = t56 * t10
      t308 = log(-0.4D1 * t305 * t277)
      t314 = t308 ** 2
      t320 = t32 * t284
      t323 = (-0.180D3 * t5 * (t281 - t308 * t284) + 0.90D2 * t4 * (t270
     # - t308 * t281 + t314 * t284 / 0.2D1) + t320) * t74 * t53
      t329 = log(-0.4D1 * t83 * t10 * t271 * t276 * t274)
      t338 = (0.90D2 * t4 * (-t281 + t329 * t284) + 0.180D3 * t5 * t284)
     # * t74 * t101
      t339 = t104 * t10
      t342 = log(-0.4D1 * t339 * t277)
      t348 = t342 ** 2
      t356 = (-0.180D3 * t5 * (t281 - t342 * t284) + 0.90D2 * t4 * (t270
     # - t342 * t281 + t348 * t284 / 0.2D1) + t320) * t100 * t53
      t358 = -(-0.180D3 * t5 * t287 + t32 * t291 + 0.90D2 * t4 * t299 - 
     #t302) * t53 / 0.2880D4 + t323 / 0.2880D4 - t338 / 0.2880D4 + t356 
     #/ 0.2880D4
      t359 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t268, t269, 0.0D0, t358)
      t361 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t264)
      t363 = -0.1D1 + x3
      t364 = t2 * t363
      t365 = t2 * x3
      t366 = t57 * t363
      t369 = log(-0.4D1 * t56 * t366)
      t370 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t372 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t376 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t378 = t369 ** 2
      t391 = log(-0.4D1 * t83 * t366)
      t397 = log(0.4D1 * t83 * t57 * t84 * t363)
      t398 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t400 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t404 = -t370 + t398
      t414 = log(-0.4D1 * t57 * x3 * t363)
      t416 = t414 ** 2
      t442 = log(0.4D1 * t78 * t14 * t197 * t363)
      t446 = log(-0.4D1 * t78 * t366)
      t451 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t453 = t442 ** 2
      t456 = t446 ** 2
      t469 = (-0.180D3 * t5 * (-t369 * t370 + t372) + 0.90D2 * t4 * (t37
     #6 - t369 * t372 + t378 * t370 / 0.2D1) + t32 * t370) * t74 * t53 /
     # 0.2880D4 - (0.90D2 * t4 * (-t372 + t391 * t370 - t398 * t397 + t4
     #00) - 0.180D3 * t5 * t404) * t74 * t101 / 0.2880D4 + (-0.180D3 * t
     #5 * (t376 - t414 * t372 + t416 * t370 / 0.2D1) + t32 * (-t414 * t3
     #70 + t372) + 0.90D2 * t4 * (-t414 * t376 + t416 * t372 / 0.2D1 - t
     #416 * t414 * t370 / 0.6D1) + t50 * t370) * t74 / 0.5760D4 + (-0.18
     #0D3 * t5 * (-t400 + t442 * t398 - t446 * t370 + t372) + 0.90D2 * t
     #4 * (-t451 + t442 * t400 - t453 * t398 / 0.2D1 + t456 * t370 / 0.2
     #D1 + t376 - t446 * t372) - t32 * t404) * t74 * t100 / 0.5760D4
      t470 = FJET(XB1, XB2, s, 0.0D0, -t364, 0.0D0, t365, 0.0D0, t469)
      t472 = x2 * x1
      t474 = t2 * t472 * t274
      t476 = t1 * x1
      t477 = t84 * s * t476
      t478 = t1 ** 2
      t483 = s * t478 * x2 * x1 * t267 * t274
      t484 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t485 = t78 * t11
      t486 = t276 * t84
      t487 = t275 * t486
      t490 = log(0.4D1 * t485 * t487)
      t491 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t500 = (0.90D2 * t4 * (t484 - t491 * t490) - 0.180D3 * t5 * t491) 
     #* t74 * t101
      t503 = log(0.4D1 * t339 * t487)
      t505 = -t484 + t503 * t491
      t508 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t510 = t503 ** 2
      t513 = -t508 + t503 * t484 - t510 * t491 / 0.2D1
      t516 = t32 * t491
      t521 = -t500 / 0.2880D4 + (-0.180D3 * t5 * t505 + 0.90D2 * t4 * t5
     #13 - t516) * t100 * t53 / 0.2880D4
      t522 = FJET(XB1, XB2, s, 0.0D0, -t474, -t268, -t477, t483, t521)
      t535 = -(-0.180D3 * t5 * t287 + t32 * t291 + 0.90D2 * t4 * t299 - 
     #t302) * t53 / 0.2880D4 + t323 / 0.2880D4 - t338 / 0.2880D4 + t356 
     #/ 0.2880D4
      t536 = FJET(XB1, XB2, s, t269, -t268, 0.0D0, 0.0D0, 0.0D0, t535)
      t538 = FJET(XB1, XB2, s, t365, 0.0D0, -t364, 0.0D0, 0.0D0, t469)
      t541 = t2 * t267 * x3
      t542 = x1 * x3
      t543 = t2 * t542
      t544 = t363 * s
      t546 = t544 * t1 * t267
      t547 = t544 * t476
      t548 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t550 = t275 * t276 * t363
      t553 = log(0.4D1 * t305 * t550)
      t554 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t559 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t561 = t553 ** 2
      t573 = log(0.4D1 * t485 * t550)
      t584 = (0.180D3 * t5 * (t548 - t553 * t554) - 0.90D2 * t4 * (t559 
     #- t553 * t548 + t561 * t554 / 0.2D1) - t32 * t554) * t74 * t53 / 0
     #.2880D4 - (0.90D2 * t4 * (t548 - t573 * t554) - 0.180D3 * t5 * t55
     #4) * t74 * t101 / 0.2880D4
      t585 = FJET(XB1, XB2, s, -t541, t543, t546, -t547, 0.0D0, t584)
      t587 = x3 * z
      t588 = t542 * z
      t589 = t78 * z
      t590 = t78 * x1
      t591 = t78 * t272
      t592 = cos(t8)
      t597 = Sqrt(-x3 * t84 * t273 * x2 * t363)
      t599 = 0.2D1 * t592 * t597
      t603 = t2 * x1 * (-t587 - t542 + t588 + t589 + t590 - t591 - x2 + 
     #t78 + t599) * t274
      t606 = z + x1 - t272 - x2 * z - t472 + t472 * z - t587 - t542 + t5
     #88 + t589 + t590 - t591 + t78 + t599
      t609 = t2 * x1 * t606 * t274
      t610 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t615 = log(-0.4D1 * t485 * t275 * t486 * t363)
      t616 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t618 = -t610 + t615 * t616
      t622 = 0.180D3 * t5 * t616
      t623 = 0.90D2 * t4 * t618 + t622
      t627 = FJET(XB1, XB2, s, -t541, t603, t546, -t609, t483, -t623 * t
     #74 * t101 / 0.2880D4)
      t630 = t74 * t100 * t53
      t643 = -t500 / 0.2880D4 + (-0.180D3 * t5 * t505 + 0.90D2 * t4 * t5
     #13 - t516) * t100 * t53 / 0.2880D4
      t644 = FJET(XB1, XB2, s, -t477, -t268, -t474, 0.0D0, t483, t643)
      t646 = FJET(XB1, XB2, s, -t547, t546, t543, -t541, 0.0D0, t584)
      t651 = 0.90D2 * t4 * t618 + t622
      t655 = FJET(XB1, XB2, s, -t609, t546, t603, -t541, t483, -t651 * t
     #74 * t101 / 0.2880D4)
      bbarbbarH5n2e1 = t265 * t264 + t359 * t358 + t361 * t264 + t470 * 
     #t469 + t522 * t521 + t536 * t535 + t538 * t469 + t585 * t584 - t62
     #7 * t623 * t630 / 0.2880D4 + t644 * t643 + t646 * t584 - t655 * t6
     #51 * t630 / 0.2880D4

      end function



      doubleprecision function bbarbbarH5n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = x3 * t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t6 * t13)
      t17 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t19 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t23 = t4 * lh
      t27 = 0.1D1 / x3
      t29 = 0.1D1 / x1
      t32 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t33 = -t32 + t17
      t35 = 0.1D1 / x2
      t37 = t27 * t35 * t29
      t40 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t41 = x2 * t5
      t44 = log(0.4D1 * t41 * t13)
      t46 = -0.1D1 + x2
      t47 = t13 * t46
      t50 = log(-0.4D1 * t41 * t47)
      t55 = -t33
      t57 = 0.180D3 * t23 * t55
      t62 = t5 * t12
      t65 = log(0.4D1 * t62 * t9)
      t70 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t4 * t82
      t84 = t83 * t17
      t91 = log(0.4D1 * x3 * t9 * t12)
      t97 = t91 ** 2
      t106 = x2 * x3
      t109 = log(0.4D1 * t106 * t13)
      t113 = log(-0.4D1 * t106 * t47)
      t122 = x2 * t9
      t125 = log(0.4D1 * t122 * t12)
      t127 = t12 * t46
      t130 = log(-0.4D1 * t122 * t127)
      t135 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t137 = t130 ** 2
      t141 = t125 ** 2
      t153 = log(0.4D1 * t13)
      t161 = t153 ** 2
      t180 = (0.90D2 * t4 * (t16 * t17 - t19) + 0.180D3 * t23 * t17) * t
     #27 * t29 / 0.2880D4 - t4 * t33 * t37 / 0.32D2 + (0.90D2 * t4 * (t4
     #0 + t44 * t17 - t50 * t32 - t19) - t57) * t35 * t29 / 0.2880D4 - (
     #-0.180D3 * t23 * (t19 - t65 * t17) + 0.90D2 * t4 * (t70 - t65 * t1
     #9 + t72 * t17 / 0.2D1) + t84) * t29 / 0.2880D4 + (-0.180D3 * t23 *
     # (t91 * t17 - t19) + 0.90D2 * t4 * (-t70 + t91 * t19 - t97 * t17 /
     # 0.2D1) - t84) * t27 / 0.5760D4 + (0.90D2 * t4 * (-t19 + t109 * t1
     #7 - t113 * t32 + t40) - t57) * t27 * t35 / 0.5760D4 + (-0.180D3 * 
     #t23 * (-t19 + t125 * t17 + t40 - t130 * t32) + 0.90D2 * t4 * (t135
     # - t130 * t40 + t137 * t32 / 0.2D1 - t70 + t125 * t19 - t141 * t17
     # / 0.2D1) + t83 * t55) * t35 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 
     #* t153) * t4 * t70 / 0.5760D4 - (0.180D3 * t153 * lh + t79 - t81 +
     # 0.45D2 * t161) * t4 * t19 / 0.5760D4 - (-t153 * t82 - 0.90D2 * t1
     #61 * lh - 0.2884936567583026D3 - 0.120D3 * t78 * lh + 0.60D2 * lh 
     #* t80 - 0.15D2 * t161 * t153) * t4 * t17 / 0.5760D4
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t180)
      t183 = -0.1D1 + x1
      t184 = t2 * t183
      t185 = t2 * x1
      t186 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t187 = t6 * t12
      t189 = x1 * z
      t190 = -z - x1 + t189
      t191 = 0.1D1 / t190
      t192 = 0.1D1 / t7 * t191
      t193 = t183 ** 2
      t194 = t192 * t193
      t197 = log(-0.4D1 * t187 * t194)
      t198 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t204 = 0.180D3 * t23 * t198
      t208 = (0.90D2 * t4 * (t186 - t197 * t198) - t204) * t27 * t29 / 0
     #.2880D4
      t211 = t4 * t198 * t37 / 0.32D2
      t212 = t41 * t12
      t215 = log(-0.4D1 * t212 * t194)
      t223 = (0.90D2 * t4 * (t186 - t215 * t198) - t204) * t35 * t29 / 0
     #.2880D4
      t226 = log(-0.4D1 * t62 * t194)
      t228 = -t186 + t226 * t198
      t231 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t233 = t226 ** 2
      t236 = -t231 + t226 * t186 - t233 * t198 / 0.2D1
      t239 = t83 * t198
      t243 = t208 + t211 + t223 - (-0.180D3 * t23 * t228 + 0.90D2 * t4 *
     # t236 - t239) * t29 / 0.2880D4
      t244 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t184, t185, 0.0D0, t243)
      t246 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t180)
      t248 = -0.1D1 + x3
      t249 = t2 * t248
      t250 = t2 * x3
      t251 = t13 * t248
      t254 = log(-0.4D1 * t6 * t251)
      t255 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t257 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t267 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t268 = -t255 + t267
      t272 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t277 = log(0.4D1 * t106 * t9 * t127 * t248)
      t281 = log(-0.4D1 * t106 * t251)
      t296 = log(-0.4D1 * t13 * x3 * t248)
      t301 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t303 = t296 ** 2
      t313 = (0.90D2 * t4 * (-t254 * t255 + t257) - 0.180D3 * t23 * t255
     #) * t27 * t29 / 0.2880D4 - t4 * t268 * t37 / 0.32D2 + (0.90D2 * t4
     # * (-t272 + t277 * t267 - t281 * t255 + t257) + 0.180D3 * t23 * t2
     #68) * t27 * t35 / 0.5760D4 + (-0.180D3 * t23 * (-t296 * t255 + t25
     #7) + 0.90D2 * t4 * (t301 - t296 * t257 + t303 * t255 / 0.2D1) + t8
     #3 * t255) * t27 / 0.5760D4
      t314 = FJET(XB1, XB2, s, 0.0D0, -t249, 0.0D0, t250, 0.0D0, t313)
      t316 = x2 * x1
      t318 = t2 * t316 * t191
      t320 = t1 * x1
      t321 = t46 * s * t320
      t322 = t1 ** 2
      t327 = s * t322 * x2 * x1 * t183 * t191
      t328 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t331 = t4 * t328 * t37 / 0.32D2
      t332 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t337 = log(0.4D1 * t212 * t192 * t193 * t46)
      t339 = -t332 + t337 * t328
      t343 = 0.180D3 * t23 * t328
      t348 = -t331 + (0.90D2 * t4 * t339 + t343) * t35 * t29 / 0.2880D4
      t349 = FJET(XB1, XB2, s, 0.0D0, -t318, -t184, -t321, t327, t348)
      t360 = t208 + t211 + t223 - (-0.180D3 * t23 * t228 + 0.90D2 * t4 *
     # t236 - t239) * t29 / 0.2880D4
      t361 = FJET(XB1, XB2, s, t185, -t184, 0.0D0, 0.0D0, 0.0D0, t360)
      t363 = FJET(XB1, XB2, s, t250, 0.0D0, -t249, 0.0D0, 0.0D0, t313)
      t366 = t2 * t183 * x3
      t367 = x1 * x3
      t368 = t2 * t367
      t369 = t248 * s
      t371 = t369 * t1 * t183
      t372 = t369 * t320
      t373 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t378 = log(0.4D1 * t187 * t192 * t193 * t248)
      t379 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t393 = (-0.90D2 * t4 * (t373 - t378 * t379) + 0.180D3 * t23 * t379
     #) * t27 * t29 / 0.2880D4 - t4 * t379 * t37 / 0.32D2
      t394 = FJET(XB1, XB2, s, -t366, t368, t371, -t372, 0.0D0, t393)
      t396 = x3 * z
      t397 = t367 * z
      t398 = t106 * z
      t399 = t106 * x1
      t400 = t106 * t189
      t401 = cos(t10)
      t406 = Sqrt(-x3 * t46 * t190 * x2 * t248)
      t408 = 0.2D1 * t401 * t406
      t412 = t2 * x1 * (-t396 - t367 + t397 + t398 + t399 - t400 - x2 + 
     #t106 + t408) * t191
      t415 = z + x1 - t189 - x2 * z - t316 + t316 * z - t396 - t367 + t3
     #97 + t398 + t399 - t400 + t106 + t408
      t418 = t2 * x1 * t415 * t191
      t419 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t422 = t4 * t419 * t37 / 0.32D2
      t423 = FJET(XB1, XB2, s, -t366, t412, t371, -t418, t327, t422)
      t435 = -t331 + (0.90D2 * t4 * t339 + t343) * t35 * t29 / 0.2880D4
      t436 = FJET(XB1, XB2, s, -t321, -t184, -t318, 0.0D0, t327, t435)
      t438 = FJET(XB1, XB2, s, -t372, t371, t368, -t366, 0.0D0, t393)
      t440 = FJET(XB1, XB2, s, -t418, t371, t412, -t366, t327, t422)
      bbarbbarH5n2e0 = t181 * t180 + t244 * t243 + t246 * t180 + t314 * 
     #t313 + t349 * t348 + t361 * t360 + t363 * t313 + t394 * t393 + t42
     #3 * t4 * t419 * t37 / 0.32D2 + t436 * t435 + t438 * t393 + t440 * 
     #t4 * t419 * t37 / 0.32D2

      end function



      doubleprecision function bbarbbarH5n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4
     #)
      t6 = x1 ** 2
      t8 = Sin(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t22 = t4 * lh
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t35 = -t17 + t34
      t36 = t4 * t35
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t41 = t30 * t37
      t44 = x2 * t13
      t47 = log(0.4D1 * t44 * t9)
      t49 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t50 = -0.1D1 + x2
      t54 = log(-0.4D1 * t44 * t9 * t50)
      t64 = bbarbbarH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t68 = t13 * t9
      t70 = log(0.4D1 * t68)
      t78 = lh ** 2
      t80 = 0.3141592653589793D1 ** 2
      t82 = t70 ** 2
      t91 = log(0.4D1 * x3 * t13 * t9)
      t99 = -(0.90D2 * t4 * (t5 - t16 * t17) - t24) * t26 / 0.2880D4 - t
     #4 * t17 * t31 / 0.32D2 + t36 * t38 / 0.32D2 + t36 * t41 / 0.64D2 +
     # (0.90D2 * t4 * (-t5 + t47 * t17 + t49 - t54 * t34) - 0.180D3 * t2
     #2 * t35) * t37 / 0.5760D4 - t4 * t64 / 0.64D2 - (-0.180D3 * lh - 0
     #.90D2 * t70) * t4 * t5 / 0.5760D4 - (0.180D3 * t70 * lh + 0.180D3 
     #* t78 - 0.30D2 * t80 + 0.45D2 * t82) * t4 * t17 / 0.5760D4 + (0.90
     #D2 * t4 * (t91 * t17 - t5) + t24) * t30 / 0.5760D4
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t99)
      t102 = -0.1D1 + x1
      t103 = t2 * t102
      t104 = t2 * x1
      t105 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t109 = 0.1D1 / (-z - x1 + x1 * z)
      t111 = t102 ** 2
      t115 = log(-0.4D1 * t10 / t11 * t109 * t111)
      t116 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t118 = -t105 + t115 * t116
      t122 = 0.180D3 * t22 * t116
      t126 = t4 * t116
      t128 = t126 * t31 / 0.32D2
      t130 = t126 * t38 / 0.32D2
      t131 = -(0.90D2 * t4 * t118 + t122) * t26 / 0.2880D4 + t128 + t130
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t103, t104, 0.0D0, t131)
      t134 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t99)
      t136 = -0.1D1 + x3
      t137 = t2 * t136
      t138 = t2 * x3
      t139 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t143 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t151 = log(-0.4D1 * t68 * x3 * t136)
      t153 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t162 = t4 * t139 * t31 / 0.32D2 + t4 * (-t143 + t139) * t41 / 0.64
     #D2 + (0.90D2 * t4 * (-t151 * t139 + t153) - 0.180D3 * t22 * t139) 
     #* t30 / 0.5760D4
      t163 = FJET(XB1, XB2, s, 0.0D0, -t137, 0.0D0, t138, 0.0D0, t162)
      t167 = t2 * x1 * x2 * t109
      t169 = t1 * x1
      t170 = t50 * s * t169
      t171 = t1 ** 2
      t176 = s * t171 * x2 * x1 * t102 * t109
      t177 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t180 = t4 * t177 * t38 / 0.32D2
      t181 = FJET(XB1, XB2, s, 0.0D0, -t167, -t103, -t170, t176, -t180)
      t184 = t177 * t37 * t26
      t193 = -(0.90D2 * t4 * t118 + t122) * t26 / 0.2880D4 + t128 + t130
      t194 = FJET(XB1, XB2, s, t104, -t103, 0.0D0, 0.0D0, 0.0D0, t193)
      t196 = FJET(XB1, XB2, s, t138, 0.0D0, -t137, 0.0D0, 0.0D0, t162)
      t199 = t2 * t102 * x3
      t201 = t2 * x1 * x3
      t202 = t136 * s
      t204 = t202 * t1 * t102
      t205 = t202 * t169
      t206 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t209 = t4 * t206 * t31 / 0.32D2
      t210 = FJET(XB1, XB2, s, -t199, t201, t204, -t205, 0.0D0, -t209)
      t213 = t206 * t30 * t26
      t216 = FJET(XB1, XB2, s, -t170, -t103, -t167, 0.0D0, t176, -t180)
      t220 = FJET(XB1, XB2, s, -t205, t204, t201, -t199, 0.0D0, -t209)
      bbarbbarH5n2em1 = t100 * t99 + t132 * t131 + t134 * t99 + t163 * t
     #162 - t181 * t4 * t184 / 0.32D2 + t194 * t193 + t196 * t162 - t210
     # * t4 * t213 / 0.32D2 - t216 * t4 * t184 / 0.32D2 - t220 * t4 * t2
     #13 / 0.32D2

      end function



      doubleprecision function bbarbbarH5n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4
     #)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t16 = bbarbbarH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x
     #4)
      t20 = z ** 2
      t24 = Sin(x4 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t34 = 0.1D1 / x3
      t37 = -t6 * t7 / 0.32D2 + t4 * (-t5 + t10) / x2 / 0.64D2 - t4 * t1
     #6 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t28) * t4 * t5 / 0.5760D4 -
     # t6 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t41 = t2 * (-0.1D1 + x1)
      t42 = t2 * x1
      t43 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t46 = t4 * t43 * t7 / 0.32D2
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t41, t42, 0.0D0, t46)
      t49 = t43 * t7
      t52 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t37)
      t55 = t2 * (-0.1D1 + x3)
      t56 = t2 * x3
      t57 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t60 = t4 * t57 * t34 / 0.64D2
      t61 = FJET(XB1, XB2, s, 0.0D0, -t55, 0.0D0, t56, 0.0D0, t60)
      t63 = t57 * t34
      t66 = FJET(XB1, XB2, s, t42, -t41, 0.0D0, 0.0D0, 0.0D0, t46)
      t70 = FJET(XB1, XB2, s, t56, 0.0D0, -t55, 0.0D0, 0.0D0, t60)
      bbarbbarH5n2em2 = t38 * t37 + t47 * t4 * t49 / 0.32D2 + t52 * t37 
     #+ t61 * t4 * t63 / 0.64D2 + t66 * t4 * t49 / 0.32D2 + t70 * t4 * t
     #63 / 0.64D2

      end function



      doubleprecision function bbarbbarH5n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4
     #)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7)
      bbarbbarH5n2em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2

      end function



      doubleprecision function bbarbbarH5n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH51J1
      doubleprecision bbarbbarH51J2
      doubleprecision bbarbbarH51J3
      bbarbbarH5n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbarbbarH51J1
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
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t26 = s - t2 * t6 * t20 - t2 * t23 * x3
      t27 = s ** 2
      t28 = t26 * t27
      t29 = t1 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t4 ** 2
      t34 = t31 / t32
      t38 = t10 * t7 * t4 + x2 * x3 + t19
      t39 = t38 ** 2
      t45 = t27 * s
      t47 = t23 ** 2
      t48 = t10 ** 2
      t52 = t28 * t29 * t23
      t53 = t10 * x1
      t70 = t5 * t20
      t74 = t20 ** 2
      t84 = t30 * t34 * t39 - t28 * t1 * t6 * t38 + t45 * t29 * t47 * t4
     #8 + 0.2D1 * t52 * t53 * t5 * t38 - 0.2D1 * t28 - 0.2D1 * t45 * t1 
     #* t23 * t10 - t28 * t29 * t47 * t48 + 0.2D1 * t28 * t1 * t23 * t10
     # + 0.2D1 * t52 * t53 * t70 - 0.2D1 * t30 * t34 * t74 - 0.2D1 * t27
     # * t1 * x1 * t70 * t26 + 0.2D1 * t45
      bbarbbarH51J1 = -0.16D2 / 0.3D1 * wd * t84 / t26

      end function
  
   
 

      doubleprecision function bbarbbarH51J2
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
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * t6 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x3
      t27 = s ** 2
      t28 = t26 * t27
      t29 = t1 ** 2
      t36 = t10 * t7 * t4 + x2 * x3 + t19
      t41 = t23 ** 2
      t43 = t10 ** 2
      t47 = x1 ** 2
      t48 = t4 ** 2
      t51 = t36 ** 2
      bbarbbarH51J2 = -0.16D2 / 0.3D1 * wd * (-0.2D1 * t28 * t29 * t23 *
     # t10 * x1 * t5 * t36 + t28 * t29 * t41 * t43 + t28 * t29 * t47 / t
     #48 * t51 + t28 * t1 * t6 * t36 - t27 * s * t29 * t41 * t43) / t26

      end function
  
   
 

      doubleprecision function bbarbbarH51J3
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
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 * t5 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x
     #3
      t27 = s ** 2
      t28 = t26 * t27
      t29 = t1 ** 2
      t30 = t23 ** 2
      t32 = t10 ** 2
      bbarbbarH51J3 = -0.16D2 / 0.3D1 * wd * (t28 * t29 * t30 * t32 - t2
     #8 * t29 * t23 * t10 * x1 * t5 * (t10 * t7 * t4 + x2 * x3 + t19) - 
     #t27 * s * t29 * t30 * t32) / t26

      end function
  
 