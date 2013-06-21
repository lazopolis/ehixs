  
      subroutine rrgg2qqbarht6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh61J1  
      doubleprecision rrgg2qqbarh61J2  
      doubleprecision rrgg2qqbarh61J3  
      doubleprecision rrgg2qqbarh61J4  
      doubleprecision rrgg2qqbarh61J5  
      doubleprecision rrgg2qqbarh61J6  
      doubleprecision rrgg2qqbarh61J7  
      doubleprecision rrgg2qqbarht6s1e1  
      doubleprecision rrgg2qqbarht6s1e0  
      doubleprecision rrgg2qqbarht6s1em1  
      doubleprecision rrgg2qqbarht6s1em2  
      doubleprecision rrgg2qqbarht6s1em3  
      doubleprecision rrgg2qqbarht6s1em4  
      doubleprecision rrgg2qqbarht6s2e1  
      doubleprecision rrgg2qqbarht6s2e0  
      doubleprecision rrgg2qqbarht6s2em1  
      doubleprecision rrgg2qqbarht6s2em2  
      doubleprecision rrgg2qqbarht6s2em3  
      doubleprecision rrgg2qqbarht6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t10 = 0.3141592653589793D1 * lh
      t11 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t12 = t5 * t11
      t15 = lh ** 2
      t17 = 0.3141592653589793D1 ** 2
      t19 = -0.180D3 * t15 + 0.30D2 * t17
      t20 = 0.3141592653589793D1 * t19
      t21 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t22 = t5 * t21
      t23 = t20 * t22
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
      t43 = 0.1D1 / (-0.1D1 + 0.2D1 * t37 * t39 - x3)
      t47 = log(0.4D1 * t28 * t30)
      t50 = t47 ** 2
      t52 = t36 ** 2
      t61 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t68 = -0.60D2 * lh * t17 + 0.2884936567583026D3 + 0.120D3 * t15 * 
     #lh
      t69 = 0.3141592653589793D1 * t68
      t70 = t69 * t22
      t87 = 0.1D1 / x3
      t90 = rrgg2qqbarh61J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t93 = t30 * t27
      t95 = log(0.4D1 * t93)
      t96 = t95 ** 2
      t97 = t96 * 0.3141592653589793D1
      t101 = t96 * t95 * 0.3141592653589793D1
      t103 = t95 * 0.3141592653589793D1
      t128 = t17 ** 2
      t129 = t15 ** 2
      t135 = t96 ** 2
      t142 = x1 ** 2
      t143 = x3 * t142
      t146 = log(0.4D1 * t143 * t93)
      t148 = t146 ** 2
      t151 = t93 * t32
      t154 = log(-0.4D1 * t143 * t151)
      t156 = t154 ** 2
      t174 = t5 * (t21 + t21 * t43)
      t175 = t20 * t174
      t178 = 0.1D1 / x1
      t181 = t142 * t27
      t182 = t181 * t30
      t184 = log(0.4D1 * t182)
      t189 = t184 ** 2
      t209 = x2 ** 2
      t210 = t209 * x3
      t211 = t210 * t182
      t213 = log(0.4D1 * t211)
      t215 = t210 * t142
      t218 = log(-0.4D1 * t215 * t151)
      t229 = 0.1D1 / x2
      t230 = t229 * t178
      t233 = t209 * t142
      t236 = log(0.4D1 * t233 * t93)
      t238 = t236 ** 2
      t255 = log(0.4D1 * t210 * t93)
      t257 = t255 ** 2
      t262 = log(-0.4D1 * t210 * t151)
      t264 = t262 ** 2
      t284 = t209 * t27
      t287 = log(0.4D1 * t284 * t30)
      t292 = t287 ** 2
      t312 = ((-0.90D2 * t6 * t7 + 0.180D3 * t10 * t12 + t23) * (-t36 * 
     #t43 - t47) - 0.90D2 * t6 * t21 * (-t50 * t47 / 0.6D1 - t52 * t36 *
     # t43 / 0.6D1) + (t20 * t12 - 0.90D2 * t6 * t61 + t70 + 0.180D3 * t
     #10 * t5 * t7) * (t43 + 0.1D1) + (-0.90D2 * t6 * t11 + 0.180D3 * t1
     #0 * t22) * (t52 * t43 / 0.2D1 + t50 / 0.2D1)) * t87 / 0.2880D4 - t
     #6 * t90 / 0.32D2 + (0.90D2 * t97 * lh + t69 + 0.15D2 * t101 - t103
     # * t19) * t5 * t11 / 0.2880D4 + (-0.180D3 * t103 * lh - 0.45D2 * t
     #97 + t20) * t5 * t7 / 0.2880D4 + (0.180D3 * t10 + 0.90D2 * t103) *
     # t5 * t61 / 0.2880D4 + (-0.30D2 * t101 * lh + t97 * t19 / 0.2D1 - 
     #t103 * t68 + 0.3141592653589793D1 * (-0.5769873135166051D3 * lh - 
     #t128 - 0.60D2 * t129 + 0.60D2 * t15 * t17) - 0.15D2 / 0.4D1 * t135
     # * 0.3141592653589793D1) * t5 * t21 / 0.2880D4 + (-0.90D2 * t6 * (
     #t7 - t146 * t11 + t148 * t21 / 0.2D1 + (t7 - t154 * t11 + t156 * t
     #21 / 0.2D1) * t43) + 0.180D3 * t10 * t5 * (t11 - t146 * t21 + (t11
     # - t154 * t21) * t43) + t175) * t87 * t178 / 0.1440D4 + (t20 * t5 
     #* (t11 - t184 * t21) - 0.90D2 * t6 * (t189 * t11 / 0.2D1 + t61 - t
     #189 * t184 * t21 / 0.6D1 - t184 * t7) + t70 + 0.180D3 * t10 * t5 *
     # (t7 - t184 * t11 + t189 * t21 / 0.2D1)) * t178 / 0.1440D4 + (-0.9
     #0D2 * t6 * (t11 - t213 * t21 + (t11 - t218 * t21) * t43) + 0.180D3
     # * t10 * t174) * t87 * t230 / 0.720D3 + (-0.90D2 * t6 * (t7 - t236
     # * t11 + t238 * t21 / 0.2D1) + 0.180D3 * t10 * t5 * (t11 - t236 * 
     #t21) + t23) * t229 * t178 / 0.720D3 + (-0.90D2 * t6 * (t7 - t255 *
     # t11 + t257 * t21 / 0.2D1 + (t7 - t262 * t11 + t264 * t21 / 0.2D1)
     # * t43) + 0.180D3 * t10 * t5 * (t11 - t255 * t21 + (t11 - t262 * t
     #21) * t43) + t175) * t87 * t229 / 0.1440D4 + (t20 * t5 * (t11 - t2
     #87 * t21) - 0.90D2 * t6 * (t292 * t11 / 0.2D1 + t61 - t292 * t287 
     #* t21 / 0.6D1 - t287 * t7) + t70 + 0.180D3 * t10 * t5 * (t7 - t287
     # * t11 + t292 * t21 / 0.2D1)) * t229 / 0.1440D4
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t312)
      t315 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t312)
      t317 = t2 * x1
      t318 = -0.1D1 + x1
      t319 = t2 * t318
      t320 = -t318
      t321 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, t320, 0.0D0, 0.
     #0D0, x4)
      t322 = t143 * t27
      t323 = x1 * z
      t324 = 0.1D1 - x1 + t323
      t325 = 0.1D1 / t324
      t326 = t30 * t325
      t327 = t318 ** 2
      t329 = t326 * t327 * t32
      t332 = log(-0.4D1 * t322 * t329)
      t333 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, t320, 0.0D0, 0.
     #0D0, x4)
      t335 = t332 ** 2
      t336 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t320, 0.0D0, 0.
     #0D0, x4)
      t340 = x3 * x1
      t341 = t340 * z
      t344 = x3 * t324
      t346 = Sqrt(-t344 * t31)
      t350 = 0.1D1 / (-0.2D1 * t341 + 0.2D1 * t340 - 0.1D1 - x3 + 0.2D1 
     #* t37 * t346)
      t352 = t326 * t327
      t355 = log(0.4D1 * t322 * t352)
      t357 = t355 ** 2
      t373 = t5 * (-t336 - t336 * t350)
      t381 = log(0.4D1 * t181 * t352)
      t386 = t381 ** 2
      t389 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, t320, 0.0D0, 0.
     #0D0, x4)
      t397 = t5 * t336
      t409 = t210 * t181
      t412 = log(-0.4D1 * t409 * t329)
      t416 = t325 * t327
      t420 = log(0.4D1 * t215 * t93 * t416)
      t431 = t233 * t27
      t434 = log(0.4D1 * t431 * t352)
      t436 = t434 ** 2
      t452 = (-0.90D2 * t6 * ((-t321 + t332 * t333 - t335 * t336 / 0.2D1
     #) * t350 - t321 + t355 * t333 - t357 * t336 / 0.2D1) + 0.180D3 * t
     #10 * t5 * ((-t333 + t332 * t336) * t350 - t333 + t355 * t336) + t2
     #0 * t373) * t87 * t178 / 0.1440D4 + (t20 * t5 * (-t333 + t381 * t3
     #36) - 0.90D2 * t6 * (-t386 * t333 / 0.2D1 - t389 + t386 * t381 * t
     #336 / 0.6D1 + t381 * t321) - t69 * t397 + 0.180D3 * t10 * t5 * (-t
     #321 + t381 * t333 - t386 * t336 / 0.2D1)) * t178 / 0.1440D4 + (-0.
     #90D2 * t6 * ((-t333 + t412 * t336) * t350 - t333 + t420 * t336) + 
     #0.180D3 * t10 * t373) * t87 * t230 / 0.720D3 + (-0.90D2 * t6 * (-t
     #321 + t434 * t333 - t436 * t336 / 0.2D1) + 0.180D3 * t10 * t5 * (-
     #t333 + t434 * t336) - t20 * t397) * t229 * t178 / 0.720D3
      t453 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t317, -t319, 0.0D0, t452)
      t455 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t319, t317, 0.0D0, t452)
      t457 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t312)
      t460 = x2 * t1 * s
      t461 = -0.1D1 + x2
      t463 = t461 * t1 * s
      t464 = x2 * z
      t466 = 0.1D1 / (0.1D1 - x2 + t464)
      t467 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t468 = t466 * t467
      t469 = t93 * t461
      t472 = log(-0.4D1 * t215 * t469)
      t474 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t480 = t5 * t466 * t474
      t487 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t488 = t466 * t487
      t491 = log(-0.4D1 * t233 * t469)
      t492 = t491 * t466
      t494 = t491 ** 2
      t506 = t20 * t480
      t513 = log(-0.4D1 * t210 * t469)
      t514 = t513 * t466
      t516 = t513 ** 2
      t532 = t30 * t461
      t535 = log(-0.4D1 * t284 * t532)
      t536 = t535 * t466
      t541 = t535 ** 2
      t542 = t541 * t466
      t545 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t566 = (-0.90D2 * t6 * (-t468 + t472 * t466 * t474) - 0.180D3 * t1
     #0 * t480) * t87 * t230 / 0.720D3 + (-0.90D2 * t6 * (-t488 + t492 *
     # t467 - t494 * t466 * t474 / 0.2D1) + 0.180D3 * t10 * t5 * (-t468 
     #+ t492 * t474) - t506) * t229 * t178 / 0.720D3 + (-0.90D2 * t6 * (
     #-t488 + t514 * t467 - t516 * t466 * t474 / 0.2D1) + 0.180D3 * t10 
     #* t5 * (-t468 + t514 * t474) - t506) * t87 * t229 / 0.1440D4 + (t2
     #0 * t5 * (-t468 + t536 * t474) - 0.90D2 * t6 * (-t542 * t467 / 0.2
     #D1 - t466 * t545 + t541 * t535 * t466 * t474 / 0.6D1 + t536 * t487
     #) - t69 * t480 + 0.180D3 * t10 * t5 * (-t488 + t536 * t467 - t542 
     #* t474 / 0.2D1)) * t229 / 0.1440D4
      t567 = FJET(XB1, XB2, s, 0.0D0, t460, 0.0D0, -t463, 0.0D0, t566)
      t569 = x2 * x3
      t572 = Sqrt(x3 * t461 * t31)
      t573 = t37 * t572
      t575 = 0.2D1 * t573 * x2
      t577 = 0.1D1 - x3 + t569
      t578 = 0.1D1 / t577
      t580 = t2 * (0.1D1 - x3 - x2 + t569 + t210 + t575) * t578
      t581 = 0.2D1 * t573
      t585 = t2 * x2 * (-0.1D1 + t569 + t581) * t578
      t588 = t210 * z
      t590 = 0.1D1 / (-0.1D1 + t581 + 0.2D1 * t573 * t464 - t575 + t588 
     #+ x2 - x3 - t464 - t210 + t569)
      t591 = t569 * t578
      t592 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t59
     #1, x4)
      t593 = t590 * t592
      t594 = t577 ** 2
      t595 = 0.1D1 / t594
      t597 = t532 * t31 * t595
      t600 = log(0.4D1 * t409 * t597)
      t602 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t59
     #1, x4)
      t608 = t5 * t590 * t602
      t614 = (-0.90D2 * t6 * (-t593 + t600 * t590 * t602) - 0.180D3 * t1
     #0 * t608) * t87 * t230 / 0.720D3
      t615 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t59
     #1, x4)
      t620 = log(0.4D1 * t210 * t27 * t597)
      t621 = t620 * t590
      t623 = t620 ** 2
      t627 = -t590 * t615 + t621 * t592 - t623 * t590 * t602 / 0.2D1
      t631 = -t593 + t621 * t602
      t635 = t20 * t608
      t640 = t614 + (-0.90D2 * t6 * t627 + 0.180D3 * t10 * t5 * t631 - t
     #635) * t87 * t229 / 0.1440D4
      t641 = FJET(XB1, XB2, s, 0.0D0, t580, 0.0D0, -t585, 0.0D0, t640)
      t643 = FJET(XB1, XB2, s, 0.0D0, -t463, 0.0D0, t460, 0.0D0, t566)
      t645 = FJET(XB1, XB2, s, 0.0D0, -t585, 0.0D0, t580, 0.0D0, t640)
      t649 = t2 * t318 * x2 * t325
      t652 = t461 * s * t1 * t318
      t653 = t1 ** 2
      t658 = s * t653 * x2 * x1 * t318 * t325
      t659 = x2 * x1
      t660 = t659 * z
      t662 = 0.1D1 / (-0.1D1 + x1 - t659 - t323 - t464 + x2 + t660)
      t663 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, t320, x2, 0.0D0
     #, x4)
      t664 = t662 * t663
      t666 = t326 * t327 * t461
      t669 = log(-0.4D1 * t409 * t666)
      t671 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t320, x2, 0.0D0
     #, x4)
      t677 = t10 * t5
      t679 = t662 * t671 * t324
      t685 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, t320, x2, 0.0D0
     #, x4)
      t689 = log(-0.4D1 * t431 * t666)
      t690 = t689 * t662
      t692 = t689 ** 2
      t712 = (0.90D2 * t6 * (t664 - t669 * t662 * t671) * t324 - 0.180D3
     # * t677 * t679) * t87 * t230 / 0.720D3 + (0.90D2 * t6 * (t662 * t6
     #85 - t690 * t663 + t692 * t662 * t671 / 0.2D1) * t324 - 0.180D3 * 
     #t10 * t5 * (t664 - t690 * t671) * t324 - t20 * t5 * t679) * t229 *
     # t178 / 0.720D3
      t713 = FJET(XB1, XB2, s, 0.0D0, -t649, t317, t652, -t658, t712)
      t715 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t312)
      t717 = FJET(XB1, XB2, s, t317, t652, 0.0D0, -t649, -t658, t712)
      t719 = t313 * t312 + t315 * t312 + t453 * t452 + t455 * t452 + t45
     #7 * t312 + t567 * t566 + t641 * t640 + t643 * t566 + t645 * t640 +
     # t713 * t712 + t715 * t312 + t717 * t712
      t720 = FJET(XB1, XB2, s, t317, -t319, 0.0D0, 0.0D0, 0.0D0, t452)
      t722 = FJET(XB1, XB2, s, t460, 0.0D0, -t463, 0.0D0, 0.0D0, t566)
      t724 = FJET(XB1, XB2, s, t580, 0.0D0, -t585, 0.0D0, 0.0D0, t640)
      t726 = FJET(XB1, XB2, s, t652, t317, -t649, 0.0D0, -t658, t712)
      t728 = t317 * t591
      t729 = t569 * x1
      t730 = t569 * t323
      t731 = t461 * t31
      t733 = Sqrt(t344 * t731)
      t734 = t37 * t733
      t735 = 0.2D1 * t734
      t740 = t319 * x2 * (t340 - t341 + t569 - t729 + t730 - 0.1D1 + t73
     #5) * t325 * t578
      t744 = t31 * s * t1 * x1 * t578
      t746 = 0.2D1 * t734 * x2
      t747 = 0.1D1 - x1 + t323 - x2 + t659 - t660 - x3 + t340 - t341 + t
     #569 - t729 + t730 + t210 + t746
      t750 = t319 * t747 * t325 * t578
      t776 = 0.1D1 + 0.3D1 * t341 + x3 - 0.4D1 * t730 - 0.2D1 * t734 * t
     #659 - 0.2D1 * t734 * t464 + x3 * t29 * t659 + 0.4D1 * t143 * t464 
     #- 0.2D1 * t143 * t29 * x2 + 0.3D1 * t210 * t323 - 0.2D1 * t734 * t
     #323 - t210 * t29 * x1 - 0.2D1 * t210 * t142 * z + t210 * t142 * t2
     #9 - t588 + t323 - 0.3D1 * t340
      t791 = -x2 + t659 + 0.2D1 * t143 - t660 - t735 + 0.2D1 * t734 * t6
     #60 - t569 + t210 + t464 - x1 + 0.3D1 * t729 + t746 - 0.4D1 * t143 
     #* z - 0.2D1 * t143 * x2 + 0.2D1 * t143 * t29 - 0.2D1 * t210 * x1 +
     # t215 + 0.2D1 * t734 * x1
      t793 = 0.1D1 / (t776 + t791)
      t794 = t324 * t793
      t795 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, t320, x2, t591,
     # x4)
      t801 = log(0.4D1 * t211 * t416 * t731 * t595)
      t803 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t320, x2, t591,
     # x4)
      t806 = -t794 * t795 + t801 * t324 * t793 * t803
      t811 = 0.180D3 * t677 * t794 * t803
      t812 = -0.90D2 * t6 * t806 - t811
      t815 = t812 * t87 * t230 / 0.720D3
      t816 = FJET(XB1, XB2, s, t728, t740, -t744, -t750, -t658, t815)
      t819 = t87 * t229 * t178
      t822 = FJET(XB1, XB2, s, t740, t728, -t750, -t744, -t658, t815)
      t826 = FJET(XB1, XB2, s, -t319, t317, 0.0D0, 0.0D0, 0.0D0, t452)
      t828 = FJET(XB1, XB2, s, -t463, 0.0D0, t460, 0.0D0, 0.0D0, t566)
      t841 = t614 + (-0.90D2 * t6 * t627 + 0.180D3 * t10 * t5 * t631 - t
     #635) * t87 * t229 / 0.1440D4
      t842 = FJET(XB1, XB2, s, -t585, 0.0D0, t580, 0.0D0, 0.0D0, t841)
      t844 = FJET(XB1, XB2, s, -t649, 0.0D0, t652, t317, -t658, t712)
      t849 = -0.90D2 * t6 * t806 - t811
      t852 = t849 * t87 * t230 / 0.720D3
      t853 = FJET(XB1, XB2, s, -t744, -t750, t728, t740, -t658, t852)
      t857 = FJET(XB1, XB2, s, -t750, -t744, t740, t728, -t658, t852)
      t861 = t720 * t452 + t722 * t566 + t724 * t640 + t726 * t712 + t81
     #6 * t812 * t819 / 0.720D3 + t822 * t812 * t819 / 0.720D3 + t826 * 
     #t452 + t828 * t566 + t842 * t841 + t844 * t712 + t853 * t849 * t81
     #9 / 0.720D3 + t857 * t849 * t819 / 0.720D3
      rrgg2qqbarht6s1e1 = t719 + t861

      end function



      doubleprecision function rrgg2qqbarht6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
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
      t27 = 0.1D1 / (-0.1D1 + 0.2D1 * t21 * t23 - x3)
      t31 = log(0.4D1 * t11 * t13)
      t32 = t31 ** 2
      t38 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t41 = 0.3141592653589793D1 * lh
      t42 = t5 * t7
      t44 = 0.180D3 * t41 * t42
      t49 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = -0.180D3 * t55 + 0.30D2 * t57
      t60 = 0.3141592653589793D1 * t59
      t61 = t60 * t42
      t66 = 0.1D1 / x3
      t69 = t13 * t10
      t71 = log(0.4D1 * t69)
      t72 = t71 * 0.3141592653589793D1
      t75 = t71 ** 2
      t76 = t75 * 0.3141592653589793D1
      t82 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t107 = x2 ** 2
      t108 = t107 * x3
      t111 = log(0.4D1 * t108 * t69)
      t113 = t69 * t15
      t116 = log(-0.4D1 * t108 * t113)
      t124 = t7 + t7 * t27
      t127 = 0.180D3 * t41 * t5 * t124
      t130 = 0.1D1 / x2
      t133 = t107 * t10
      t136 = log(0.4D1 * t133 * t13)
      t138 = t136 ** 2
      t152 = x1 ** 2
      t153 = x3 * t152
      t156 = log(0.4D1 * t153 * t69)
      t160 = log(-0.4D1 * t153 * t113)
      t169 = 0.1D1 / x1
      t174 = t66 * t130 * t169
      t177 = t107 * t152
      t180 = log(0.4D1 * t177 * t69)
      t189 = t152 * t10
      t192 = log(0.4D1 * t189 * t13)
      t194 = t192 ** 2
      t208 = (-0.90D2 * t6 * t7 * (t20 * t27 / 0.2D1 + t32 / 0.2D1) + (-
     #0.90D2 * t6 * t38 + t44) * (-t19 * t27 - t31) + (-0.90D2 * t6 * t4
     #9 + 0.180D3 * t41 * t5 * t38 + t61) * (t27 + 0.1D1)) * t66 / 0.288
     #0D4 + (-0.180D3 * t72 * lh - 0.45D2 * t76 + t60) * t5 * t38 / 0.28
     #80D4 - t6 * t82 / 0.32D2 + (0.90D2 * t76 * lh + 0.3141592653589793
     #D1 * (-0.60D2 * lh * t57 + 0.2884936567583026D3 + 0.120D3 * t55 * 
     #lh) + 0.15D2 * t75 * t71 * 0.3141592653589793D1 - t72 * t59) * t5 
     #* t7 / 0.2880D4 + (0.180D3 * t41 + 0.90D2 * t72) * t5 * t49 / 0.28
     #80D4 + (-0.90D2 * t6 * (t38 - t111 * t7 + (t38 - t116 * t7) * t27)
     # + t127) * t66 * t130 / 0.1440D4 + (-0.90D2 * t6 * (t49 - t136 * t
     #38 + t138 * t7 / 0.2D1) + 0.180D3 * t41 * t5 * (t38 - t136 * t7) +
     # t61) * t130 / 0.1440D4 + (-0.90D2 * t6 * (t38 - t156 * t7 + (t38 
     #- t160 * t7) * t27) + t127) * t66 * t169 / 0.1440D4 - t6 * t124 * 
     #t174 / 0.8D1 + (-0.90D2 * t6 * (t38 - t180 * t7) + t44) * t130 * t
     #169 / 0.720D3 + (-0.90D2 * t6 * (t49 - t192 * t38 + t194 * t7 / 0.
     #2D1) + 0.180D3 * t41 * t5 * (t38 - t192 * t7) + t61) * t169 / 0.14
     #40D4
      t209 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t208)
      t211 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t208)
      t213 = t2 * x1
      t214 = -0.1D1 + x1
      t215 = t2 * t214
      t216 = -t214
      t217 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, t216, 0.0D0, 0.
     #0D0, x4)
      t218 = t153 * t10
      t219 = x1 * z
      t220 = 0.1D1 - x1 + t219
      t221 = 0.1D1 / t220
      t222 = t13 * t221
      t223 = t214 ** 2
      t228 = log(-0.4D1 * t218 * t222 * t223 * t15)
      t229 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t216, 0.0D0, 0.
     #0D0, x4)
      t232 = x3 * x1
      t233 = t232 * z
      t236 = x3 * t220
      t238 = Sqrt(-t236 * t14)
      t242 = 0.1D1 / (-0.2D1 * t233 + 0.2D1 * t232 - 0.1D1 - x3 + 0.2D1 
     #* t238 * t21)
      t244 = t222 * t223
      t247 = log(0.4D1 * t218 * t244)
      t253 = -t229 - t229 * t242
      t264 = t177 * t10
      t267 = log(0.4D1 * t264 * t244)
      t272 = t5 * t229
      t279 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, t216, 0.0D0, 0.
     #0D0, x4)
      t282 = log(0.4D1 * t189 * t244)
      t284 = t282 ** 2
      t299 = (-0.90D2 * t6 * ((-t217 + t228 * t229) * t242 - t217 + t247
     # * t229) + 0.180D3 * t41 * t5 * t253) * t66 * t169 / 0.1440D4 - t6
     # * t253 * t174 / 0.8D1 + (-0.90D2 * t6 * (-t217 + t267 * t229) - 0
     #.180D3 * t41 * t272) * t130 * t169 / 0.720D3 + (-0.90D2 * t6 * (-t
     #279 + t282 * t217 - t284 * t229 / 0.2D1) + 0.180D3 * t41 * t5 * (-
     #t217 + t282 * t229) - t60 * t272) * t169 / 0.1440D4
      t300 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t213, -t215, 0.0D0, t299)
      t302 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t215, t213, 0.0D0, t299)
      t304 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t208)
      t307 = x2 * t1 * s
      t308 = -0.1D1 + x2
      t310 = t308 * t1 * s
      t311 = x2 * z
      t313 = 0.1D1 / (0.1D1 - x2 + t311)
      t314 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t315 = t313 * t314
      t316 = t69 * t308
      t319 = log(-0.4D1 * t108 * t316)
      t321 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t327 = t5 * t313 * t321
      t329 = 0.180D3 * t41 * t327
      t334 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t336 = t13 * t308
      t339 = log(-0.4D1 * t133 * t336)
      t340 = t339 * t313
      t342 = t339 ** 2
      t360 = t130 * t169
      t366 = log(-0.4D1 * t177 * t316)
      t376 = (-0.90D2 * t6 * (-t315 + t319 * t313 * t321) - t329) * t66 
     #* t130 / 0.1440D4 + (-0.90D2 * t6 * (-t313 * t334 + t340 * t314 - 
     #t342 * t313 * t321 / 0.2D1) + 0.180D3 * t41 * t5 * (-t315 + t340 *
     # t321) - t60 * t327) * t130 / 0.1440D4 + t6 * t313 * t321 * t66 * 
     #t360 / 0.8D1 + (-0.90D2 * t6 * (-t315 + t366 * t313 * t321) - t329
     #) * t130 * t169 / 0.720D3
      t377 = FJET(XB1, XB2, s, 0.0D0, t307, 0.0D0, -t310, 0.0D0, t376)
      t379 = x2 * x3
      t382 = Sqrt(x3 * t308 * t14)
      t383 = t21 * t382
      t385 = 0.2D1 * t383 * x2
      t387 = 0.1D1 - x3 + t379
      t388 = 0.1D1 / t387
      t390 = t2 * (0.1D1 - x3 - x2 + t379 + t108 + t385) * t388
      t391 = 0.2D1 * t383
      t395 = t2 * x2 * (-0.1D1 + t379 + t391) * t388
      t398 = t108 * z
      t400 = 0.1D1 / (-0.1D1 + t391 + 0.2D1 * t383 * t311 - t385 + t398 
     #+ x2 - x3 - t311 - t108 + t379)
      t401 = t379 * t388
      t402 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t40
     #1, x4)
      t405 = t387 ** 2
      t411 = log(0.4D1 * t108 * t10 * t336 * t14 / t405)
      t413 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t40
     #1, x4)
      t415 = -t400 * t402 + t411 * t400 * t413
      t421 = 0.180D3 * t41 * t5 * t400 * t413
      t430 = t6 * t400 * t413 * t66 * t360 / 0.8D1
      t431 = (-0.90D2 * t6 * t415 - t421) * t66 * t130 / 0.1440D4 + t430
      t432 = FJET(XB1, XB2, s, 0.0D0, t390, 0.0D0, -t395, 0.0D0, t431)
      t434 = FJET(XB1, XB2, s, 0.0D0, -t310, 0.0D0, t307, 0.0D0, t376)
      t436 = FJET(XB1, XB2, s, 0.0D0, -t395, 0.0D0, t390, 0.0D0, t431)
      t440 = t2 * t214 * x2 * t221
      t443 = t308 * s * t1 * t214
      t444 = t1 ** 2
      t449 = s * t444 * x2 * x1 * t214 * t221
      t450 = x2 * x1
      t451 = t450 * z
      t453 = 0.1D1 / (-0.1D1 + x1 - t450 - t219 - t311 + x2 + t451)
      t454 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t216, x2, 0.0D0
     #, x4)
      t455 = t453 * t454
      t461 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, t216, x2, 0.0D0
     #, x4)
      t467 = log(-0.4D1 * t264 * t222 * t223 * t308)
      t482 = t6 * t455 * t220 * t66 * t360 / 0.8D1 + (0.90D2 * t6 * (t45
     #3 * t461 - t467 * t453 * t454) * t220 - 0.180D3 * t41 * t5 * t455 
     #* t220) * t130 * t169 / 0.720D3
      t483 = FJET(XB1, XB2, s, 0.0D0, -t440, t213, t443, -t449, t482)
      t485 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t208)
      t487 = FJET(XB1, XB2, s, t213, t443, 0.0D0, -t440, -t449, t482)
      t489 = t209 * t208 + t211 * t208 + t300 * t299 + t302 * t299 + t30
     #4 * t208 + t377 * t376 + t432 * t431 + t434 * t376 + t436 * t431 +
     # t483 * t482 + t485 * t208 + t487 * t482
      t490 = FJET(XB1, XB2, s, t213, -t215, 0.0D0, 0.0D0, 0.0D0, t299)
      t492 = FJET(XB1, XB2, s, t307, 0.0D0, -t310, 0.0D0, 0.0D0, t376)
      t494 = FJET(XB1, XB2, s, t390, 0.0D0, -t395, 0.0D0, 0.0D0, t431)
      t496 = FJET(XB1, XB2, s, t443, t213, -t440, 0.0D0, -t449, t482)
      t498 = t213 * t401
      t499 = t379 * x1
      t500 = t379 * t219
      t503 = Sqrt(t236 * t308 * t14)
      t504 = t21 * t503
      t505 = 0.2D1 * t504
      t510 = t215 * x2 * (t232 - t233 + t379 - t499 + t500 - 0.1D1 + t50
     #5) * t221 * t388
      t514 = t14 * s * t1 * x1 * t388
      t516 = 0.2D1 * t504 * x2
      t517 = 0.1D1 - x1 + t219 - x2 + t450 - t451 - x3 + t232 - t233 + t
     #379 - t499 + t500 + t108 + t516
      t520 = t215 * t517 * t221 * t388
      t534 = 0.1D1 + t219 - 0.3D1 * t232 + t311 - t379 + t108 + x3 + t45
     #0 - t505 + 0.3D1 * t499 + t516 - 0.4D1 * t153 * z - 0.2D1 * t153 *
     # x2 + 0.2D1 * t153 * t12 - 0.2D1 * t108 * x1 + t108 * t152 + 0.2D1
     # * t504 * x1
      t562 = -t451 + 0.3D1 * t233 - t398 - 0.4D1 * t500 - 0.2D1 * t504 *
     # t450 - 0.2D1 * t504 * t311 + x3 * t12 * t450 + 0.4D1 * t153 * t31
     #1 - 0.2D1 * t153 * t12 * x2 + 0.3D1 * t108 * t219 - 0.2D1 * t504 *
     # t219 - t108 * t12 * x1 - 0.2D1 * t108 * t152 * z + t108 * t152 * 
     #t12 - x2 - x1 + 0.2D1 * t153 + 0.2D1 * t504 * t451
      t564 = 0.1D1 / (t534 + t562)
      t567 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t216, x2, t401,
     # x4)
      t571 = t6 * t220 * t564 * t567 * t66 * t360 / 0.8D1
      t572 = FJET(XB1, XB2, s, t498, t510, -t514, -t520, -t449, t571)
      t574 = t5 * t220
      t577 = t564 * t567 * t174
      t580 = FJET(XB1, XB2, s, t510, t498, -t520, -t514, -t449, t571)
      t585 = FJET(XB1, XB2, s, -t215, t213, 0.0D0, 0.0D0, 0.0D0, t299)
      t587 = FJET(XB1, XB2, s, -t310, 0.0D0, t307, 0.0D0, 0.0D0, t376)
      t596 = (-0.90D2 * t6 * t415 - t421) * t66 * t130 / 0.1440D4 + t430
      t597 = FJET(XB1, XB2, s, -t395, 0.0D0, t390, 0.0D0, 0.0D0, t596)
      t599 = FJET(XB1, XB2, s, -t440, 0.0D0, t443, t213, -t449, t482)
      t601 = FJET(XB1, XB2, s, -t514, -t520, t498, t510, -t449, t571)
      t606 = FJET(XB1, XB2, s, -t520, -t514, t510, t498, -t449, t571)
      t611 = t490 * t299 + t492 * t376 + t494 * t431 + t496 * t482 + t57
     #2 * 0.3141592653589793D1 * t574 * t577 / 0.8D1 + t580 * 0.31415926
     #53589793D1 * t574 * t577 / 0.8D1 + t585 * t299 + t587 * t376 + t59
     #7 * t596 + t599 * t482 + t601 * 0.3141592653589793D1 * t574 * t577
     # / 0.8D1 + t606 * 0.3141592653589793D1 * t574 * t577 / 0.8D1
      rrgg2qqbarht6s1e0 = t489 + t611

      end function



      doubleprecision function rrgg2qqbarht6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
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
      t26 = 0.1D1 / (-0.1D1 + 0.2D1 * t20 * t22 - x3)
      t30 = log(0.4D1 * t11 * t13)
      t35 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t38 = 0.3141592653589793D1 * lh
      t41 = 0.180D3 * t38 * t5 * t7
      t46 = 0.1D1 / x3
      t49 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t55 = log(0.4D1 * t13 * t10)
      t56 = t55 * 0.3141592653589793D1
      t64 = t55 ** 2
      t67 = lh ** 2
      t69 = 0.3141592653589793D1 ** 2
      t79 = (t7 + t7 * t26) * t46
      t80 = 0.1D1 / x2
      t84 = x2 ** 2
      t85 = t84 * t10
      t88 = log(0.4D1 * t85 * t13)
      t97 = 0.1D1 / x1
      t101 = x1 ** 2
      t102 = t101 * t10
      t105 = log(0.4D1 * t102 * t13)
      t116 = (-0.90D2 * t6 * t7 * (-t19 * t26 - t30) + (-0.90D2 * t6 * t
     #35 + t41) * (t26 + 0.1D1)) * t46 / 0.2880D4 - t6 * t49 / 0.32D2 + 
     #(0.180D3 * t38 + 0.90D2 * t56) * t5 * t35 / 0.2880D4 + (-0.180D3 *
     # t56 * lh - 0.45D2 * t64 * 0.3141592653589793D1 + 0.31415926535897
     #93D1 * (-0.180D3 * t67 + 0.30D2 * t69)) * t5 * t7 / 0.2880D4 - t6 
     #* t79 * t80 / 0.16D2 + (-0.90D2 * t6 * (t35 - t88 * t7) + t41) * t
     #80 / 0.1440D4 - t6 * t7 * t80 * t97 / 0.8D1 + (-0.90D2 * t6 * (t35
     # - t105 * t7) + t41) * t97 / 0.1440D4 - t6 * t79 * t97 / 0.16D2
      t117 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t116)
      t119 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t116)
      t121 = t2 * x1
      t122 = -0.1D1 + x1
      t123 = t2 * t122
      t124 = -t122
      t125 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t124, 0.0D0, 0.
     #0D0, x4)
      t130 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, t124, 0.0D0, 0.
     #0D0, x4)
      t131 = x1 * z
      t132 = 0.1D1 - x1 + t131
      t133 = 0.1D1 / t132
      t135 = t122 ** 2
      t139 = log(0.4D1 * t102 * t13 * t133 * t135)
      t150 = x3 * x1
      t156 = Sqrt(-x3 * t132 * t14)
      t167 = t6 * t125 * t80 * t97 / 0.8D1 + (-0.90D2 * t6 * (-t130 + t1
     #39 * t125) - 0.180D3 * t38 * t5 * t125) * t97 / 0.1440D4 - t6 * (-
     #t125 - t125 / (-0.2D1 * t150 * z + 0.2D1 * t150 - 0.1D1 - x3 + 0.2
     #D1 * t20 * t156)) * t46 * t97 / 0.16D2
      t168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t121, -t123, 0.0D0, t167)
      t170 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t123, t121, 0.0D0, t167)
      t172 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t116)
      t175 = x2 * t1 * s
      t176 = -0.1D1 + x2
      t178 = t176 * t1 * s
      t179 = x2 * z
      t181 = 0.1D1 / (0.1D1 - x2 + t179)
      t182 = t6 * t181
      t183 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t188 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0
     #D0, x4)
      t193 = log(-0.4D1 * t85 * t13 * t176)
      t210 = t182 * t183 * t46 * t80 / 0.16D2 + (-0.90D2 * t6 * (-t181 *
     # t188 + t193 * t181 * t183) - 0.180D3 * t38 * t5 * t181 * t183) * 
     #t80 / 0.1440D4 + t182 * t183 * t80 * t97 / 0.8D1
      t211 = FJET(XB1, XB2, s, 0.0D0, t175, 0.0D0, -t178, 0.0D0, t210)
      t213 = x2 * x3
      t214 = t84 * x3
      t217 = Sqrt(x3 * t176 * t14)
      t218 = t20 * t217
      t220 = 0.2D1 * t218 * x2
      t223 = 0.1D1 / (0.1D1 - x3 + t213)
      t225 = t2 * (0.1D1 - x3 - x2 + t213 + t214 + t220) * t223
      t226 = 0.2D1 * t218
      t230 = t2 * x2 * (-0.1D1 + t213 + t226) * t223
      t235 = 0.1D1 / (-0.1D1 + t226 + 0.2D1 * t218 * t179 - t220 + t214 
     #* z + x2 - x3 - t179 - t214 + t213)
      t238 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t21
     #3 * t223, x4)
      t242 = t6 * t235 * t238 * t46 * t80 / 0.16D2
      t243 = FJET(XB1, XB2, s, 0.0D0, t225, 0.0D0, -t230, 0.0D0, t242)
      t248 = t235 * t238 * t46 * t80
      t251 = FJET(XB1, XB2, s, 0.0D0, -t178, 0.0D0, t175, 0.0D0, t210)
      t253 = FJET(XB1, XB2, s, 0.0D0, -t230, 0.0D0, t225, 0.0D0, t242)
      t260 = t2 * t122 * x2 * t133
      t263 = t176 * s * t1 * t122
      t264 = t1 ** 2
      t269 = s * t264 * x2 * x1 * t122 * t133
      t270 = x2 * x1
      t273 = 0.1D1 / (-0.1D1 + x1 - t270 - t131 - t179 + x2 + t270 * z)
      t275 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.0D0
     #, x4)
      t278 = t275 * t132 * t80 * t97
      t280 = t6 * t273 * t278 / 0.8D1
      t281 = FJET(XB1, XB2, s, 0.0D0, -t260, t121, t263, -t269, t280)
      t283 = t5 * t273
      t287 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t116)
      t289 = FJET(XB1, XB2, s, t121, t263, 0.0D0, -t260, -t269, t280)
      t294 = FJET(XB1, XB2, s, t121, -t123, 0.0D0, 0.0D0, 0.0D0, t167)
      t296 = FJET(XB1, XB2, s, t175, 0.0D0, -t178, 0.0D0, 0.0D0, t210)
      t298 = FJET(XB1, XB2, s, t225, 0.0D0, -t230, 0.0D0, 0.0D0, t242)
      t303 = FJET(XB1, XB2, s, t263, t121, -t260, 0.0D0, -t269, t280)
      t308 = FJET(XB1, XB2, s, -t123, t121, 0.0D0, 0.0D0, 0.0D0, t167)
      t310 = FJET(XB1, XB2, s, -t178, 0.0D0, t175, 0.0D0, 0.0D0, t210)
      t312 = FJET(XB1, XB2, s, -t260, 0.0D0, t263, t121, -t269, t280)
      t317 = FJET(XB1, XB2, s, -t230, 0.0D0, t225, 0.0D0, 0.0D0, t242)
      rrgg2qqbarht6s1em1 = t117 * t116 + t119 * t116 + t168 * t167 + t17
     #0 * t167 + t172 * t116 + t211 * t210 + t243 * 0.3141592653589793D1
     # * t5 * t248 / 0.16D2 + t251 * t210 + t253 * 0.3141592653589793D1 
     #* t5 * t248 / 0.16D2 + t281 * 0.3141592653589793D1 * t283 * t278 /
     # 0.8D1 + t287 * t116 + t289 * 0.3141592653589793D1 * t283 * t278 /
     # 0.8D1 + t294 * t167 + t296 * t210 + t298 * 0.3141592653589793D1 *
     # t5 * t248 / 0.16D2 + t303 * 0.3141592653589793D1 * t283 * t278 / 
     #0.8D1 + t308 * t167 + t310 * t210 + t312 * 0.3141592653589793D1 * 
     #t283 * t278 / 0.8D1 + t317 * 0.3141592653589793D1 * t5 * t248 / 0.
     #16D2

      end function



      doubleprecision function rrgg2qqbarht6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t23 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t31 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.0D0, x4)
      t36 = z ** 2
      t38 = Sin(t8)
      t39 = t38 ** 2
      t42 = log(0.4D1 / t36 * t39)
      t49 = -t6 * t7 * (0.1D1 / (-0.1D1 + 0.2D1 * t9 * t12 - x3) + 0.1D1
     #) / x3 / 0.32D2 - t6 * t7 * t23 / 0.16D2 - t6 * t7 * t27 / 0.16D2 
     #- t6 * t31 / 0.32D2 + (0.180D3 * 0.3141592653589793D1 * lh + 0.90D
     #2 * t42 * 0.3141592653589793D1) * t5 * t7 / 0.2880D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t54 = t2 * x1
      t55 = -0.1D1 + x1
      t56 = t2 * t55
      t58 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, -t55, 0.0D0, 0.0
     #D0, x4)
      t61 = t6 * t58 * t27 / 0.16D2
      t62 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t54, -t56, 0.0D0, t61)
      t65 = t5 * t58 * t27
      t68 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t56, t54, 0.0D0, t61)
      t72 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t75 = x2 * t1 * s
      t78 = (-0.1D1 + x2) * t1 * s
      t82 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D
     #0, x4)
      t84 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t82 * t23
      t86 = t6 * t84 / 0.16D2
      t87 = FJET(XB1, XB2, s, 0.0D0, t75, 0.0D0, -t78, 0.0D0, t86)
      t92 = FJET(XB1, XB2, s, 0.0D0, -t78, 0.0D0, t75, 0.0D0, t86)
      t97 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t99 = FJET(XB1, XB2, s, t54, -t56, 0.0D0, 0.0D0, 0.0D0, t61)
      t103 = FJET(XB1, XB2, s, t75, 0.0D0, -t78, 0.0D0, 0.0D0, t86)
      t108 = FJET(XB1, XB2, s, -t78, 0.0D0, t75, 0.0D0, 0.0D0, t86)
      t113 = FJET(XB1, XB2, s, -t56, t54, 0.0D0, 0.0D0, 0.0D0, t61)
      rrgg2qqbarht6s1em2 = t50 * t49 + t52 * t49 + t62 * 0.3141592653589
     #793D1 * t65 / 0.16D2 + t68 * 0.3141592653589793D1 * t65 / 0.16D2 +
     # t72 * t49 + t87 * 0.3141592653589793D1 * t5 * t84 / 0.16D2 + t92 
     #* 0.3141592653589793D1 * t5 * t84 / 0.16D2 + t97 * t49 + t99 * 0.3
     #141592653589793D1 * t65 / 0.16D2 + t103 * 0.3141592653589793D1 * t
     #5 * t84 / 0.16D2 + t108 * 0.3141592653589793D1 * t5 * t84 / 0.16D2
     # + t113 * 0.3141592653589793D1 * t65 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t9 = 0.3141592653589793D1 * t5 * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t5 * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2qqbarht6s1em3 = -t10 * 0.3141592653589793D1 * t12 / 0.32D2 - 
     #t14 * 0.3141592653589793D1 * t12 / 0.32D2 - t17 * 0.31415926535897
     #93D1 * t12 / 0.32D2 - t20 * 0.3141592653589793D1 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      rrgg2qqbarht6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht6s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t10 = 0.3141592653589793D1 * lh
      t11 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t12 = t5 * t11
      t15 = lh ** 2
      t17 = 0.3141592653589793D1 ** 2
      t19 = -0.180D3 * t15 + 0.30D2 * t17
      t20 = 0.3141592653589793D1 * t19
      t21 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t22 = t5 * t21
      t25 = z ** 2
      t27 = 0.1D1 / t25 / z
      t28 = x3 * t27
      t29 = x4 * 0.3141592653589793D1
      t30 = Sin(t29)
      t31 = t30 ** 2
      t32 = -0.1D1 + x3
      t33 = 0.1D1 / t32
      t37 = log(-0.4D1 * t28 * t31 * t33)
      t38 = x3 * z
      t39 = 0.2D1 * t38
      t40 = cos(t29)
      t42 = Sqrt(-t38 * t32)
      t46 = 0.1D1 / (-t39 - 0.1D1 + x3 + 0.2D1 * t40 * t42)
      t50 = log(0.4D1 * t28 * t31)
      t53 = t50 ** 2
      t55 = t37 ** 2
      t64 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t71 = -0.60D2 * lh * t17 + 0.2884936567583026D3 + 0.120D3 * t15 * 
     #lh
      t72 = 0.3141592653589793D1 * t71
      t73 = t72 * t22
      t90 = 0.1D1 / x3
      t93 = t27 * t31
      t95 = log(0.4D1 * t93)
      t96 = t95 ** 2
      t99 = t96 * t95
      t119 = t17 ** 2
      t120 = t15 ** 2
      t126 = rrgg2qqbarh61J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.0D0, x4)
      t132 = t96 ** 2
      t141 = x1 ** 2
      t142 = x3 * t141
      t143 = t93 * t33
      t146 = log(-0.4D1 * t142 * t143)
      t148 = t146 ** 2
      t155 = log(0.4D1 * t142 * t93)
      t157 = t155 ** 2
      t171 = t21 * t46
      t177 = 0.1D1 / x1
      t180 = t141 * t31
      t181 = t180 * t27
      t183 = log(0.4D1 * t181)
      t188 = t183 ** 2
      t208 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t209 = x2 ** 2
      t210 = x3 * t209
      t211 = t210 * t141
      t212 = -0.1D1 + x2
      t213 = t93 * t212
      t216 = log(-0.4D1 * t211 * t213)
      t217 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t221 = log(-0.4D1 * t211 * t143)
      t227 = log(0.4D1 * t210 * t181)
      t233 = t5 * (-t217 + t21 + t171)
      t238 = 0.1D1 / x2
      t239 = t238 * t177
      t242 = t209 * t141
      t245 = log(0.4D1 * t242 * t93)
      t247 = t245 ** 2
      t250 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t253 = log(-0.4D1 * t242 * t213)
      t255 = t253 ** 2
      t268 = t5 * (-t217 + t21)
      t276 = log(0.4D1 * t210 * t93)
      t278 = t276 ** 2
      t283 = log(-0.4D1 * t210 * t143)
      t285 = t283 ** 2
      t292 = log(-0.4D1 * t210 * t213)
      t294 = t292 ** 2
      t314 = t27 * t209
      t315 = t31 * t212
      t318 = log(-0.4D1 * t314 * t315)
      t322 = log(0.4D1 * t314 * t31)
      t327 = t322 ** 2
      t334 = t318 ** 2
      t337 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t359 = ((-0.90D2 * t6 * t7 + 0.180D3 * t10 * t12 + t20 * t22) * (-
     #t37 * t46 - t50) - 0.90D2 * t6 * t21 * (-t53 * t50 / 0.6D1 - t55 *
     # t37 * t46 / 0.6D1) + (t20 * t12 - 0.90D2 * t6 * t64 + t73 + 0.180
     #D3 * t10 * t5 * t7) * (t46 + 0.1D1) + (-0.90D2 * t6 * t11 + 0.180D
     #3 * t10 * t22) * (t55 * t46 / 0.2D1 + t53 / 0.2D1)) * t90 / 0.2880
     #D4 + (0.180D3 * (t96 * t11 / 0.2D1 + t64 - t99 * t21 / 0.6D1 - t95
     # * t7) * 0.3141592653589793D1 * lh + (t7 - t95 * t11 + t96 * t21 /
     # 0.2D1) * 0.3141592653589793D1 * t19 + (t11 - t95 * t21) * 0.31415
     #92653589793D1 * t71 + t21 * 0.3141592653589793D1 * (-0.57698731351
     #66051D3 * lh - t119 - 0.60D2 * t120 + 0.60D2 * t15 * t17) - 0.90D2
     # * (t126 - t99 * t11 / 0.6D1 + t96 * t7 / 0.2D1 - t95 * t64 + t132
     # * t21 / 0.24D2) * 0.3141592653589793D1) * t5 / 0.2880D4 - (-0.90D
     #2 * t6 * (-(t7 - t146 * t11 + t148 * t21 / 0.2D1) * t46 - t7 + t15
     #5 * t11 - t157 * t21 / 0.2D1) + 0.180D3 * t10 * t5 * (-(t11 - t146
     # * t21) * t46 - t11 + t155 * t21) + t20 * t5 * (-t171 - t21)) * t9
     #0 * t177 / 0.1440D4 + (t20 * t5 * (t11 - t183 * t21) - 0.90D2 * t6
     # * (t188 * t11 / 0.2D1 + t64 - t188 * t183 * t21 / 0.6D1 - t183 * 
     #t7) + t73 + 0.180D3 * t10 * t5 * (t7 - t183 * t11 + t188 * t21 / 0
     #.2D1)) * t177 / 0.1440D4 + (-0.90D2 * t6 * (-t208 + t216 * t217 + 
     #(t11 - t221 * t21) * t46 + t11 - t227 * t21) + 0.180D3 * t10 * t23
     #3) * t90 * t239 / 0.720D3 + (-0.90D2 * t6 * (t7 - t245 * t11 + t24
     #7 * t21 / 0.2D1 - t250 + t253 * t208 - t255 * t217 / 0.2D1) + 0.18
     #0D3 * t10 * t5 * (t11 - t245 * t21 - t208 + t253 * t217) + t20 * t
     #268) * t238 * t177 / 0.720D3 + (-0.90D2 * t6 * (t7 - t276 * t11 + 
     #t278 * t21 / 0.2D1 + (t7 - t283 * t11 + t285 * t21 / 0.2D1) * t46 
     #- t250 + t292 * t208 - t294 * t217 / 0.2D1) + 0.180D3 * t10 * t5 *
     # (t11 - t276 * t21 + (t11 - t283 * t21) * t46 - t208 + t292 * t217
     #) + t20 * t233) * t90 * t238 / 0.1440D4 + (t20 * t5 * (-t208 + t31
     #8 * t217 + t11 - t322 * t21) - 0.90D2 * t6 * (t327 * t11 / 0.2D1 +
     # t64 - t327 * t322 * t21 / 0.6D1 - t322 * t7 - t334 * t208 / 0.2D1
     # - t337 + t334 * t318 * t217 / 0.6D1 + t318 * t250) + t72 * t268 +
     # 0.180D3 * t10 * t5 * (-t250 + t318 * t208 - t334 * t217 / 0.2D1 +
     # t7 - t322 * t11 + t327 * t21 / 0.2D1)) * t238 / 0.1440D4
      t360 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t359)
      t362 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t359)
      t364 = t2 * x1
      t365 = -0.1D1 + x1
      t366 = t2 * t365
      t367 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t368 = t142 * t31
      t369 = 0.1D1 / t25
      t370 = x1 * z
      t371 = -z - x1 + t370
      t372 = 0.1D1 / t371
      t373 = t369 * t372
      t374 = t365 ** 2
      t375 = t373 * t374
      t378 = log(-0.4D1 * t368 * t375)
      t379 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t381 = t378 ** 2
      t382 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t386 = t373 * t374 * t33
      t389 = log(0.4D1 * t368 * t386)
      t391 = t389 ** 2
      t395 = x3 * x1
      t396 = t395 * z
      t399 = x3 * t371
      t401 = Sqrt(t399 * t32)
      t405 = 0.1D1 / (0.2D1 * t396 - 0.2D1 * t395 - t39 + x3 + 0.2D1 * t
     #40 * t401 - 0.1D1)
      t419 = t382 * t405 + t382
      t425 = (-0.90D2 * t6 * (t367 - t378 * t379 + t381 * t382 / 0.2D1 +
     # (t367 - t389 * t379 + t391 * t382 / 0.2D1) * t405) + 0.180D3 * t1
     #0 * t5 * (t379 - t378 * t382 + (t379 - t389 * t382) * t405) + t20 
     #* t5 * t419) * t90 * t177 / 0.1440D4
      t428 = log(-0.4D1 * t180 * t375)
      t430 = -t379 + t428 * t382
      t433 = t428 ** 2
      t436 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t441 = -t433 * t379 / 0.2D1 - t436 + t433 * t428 * t382 / 0.6D1 + 
     #t428 * t367
      t444 = t5 * t382
      t445 = t72 * t444
      t449 = -t367 + t428 * t379 - t433 * t382 / 0.2D1
      t457 = t372 * t374
      t461 = log(-0.4D1 * t211 * t31 * t369 * t457)
      t463 = t210 * t180
      t466 = log(0.4D1 * t463 * t386)
      t480 = (-0.90D2 * t6 * (-t379 + t461 * t382 - (t379 - t466 * t382)
     # * t405) - 0.180D3 * t10 * t5 * t419) * t90 * t239 / 0.720D3
      t481 = t242 * t31
      t484 = log(-0.4D1 * t481 * t375)
      t486 = t484 ** 2
      t501 = (-0.90D2 * t6 * (-t367 + t484 * t379 - t486 * t382 / 0.2D1)
     # + 0.180D3 * t10 * t5 * (-t379 + t484 * t382) - t20 * t444) * t238
     # * t177 / 0.720D3
      t502 = -t425 + (t20 * t5 * t430 - 0.90D2 * t6 * t441 - t445 + 0.18
     #0D3 * t10 * t5 * t449) * t177 / 0.1440D4 + t480 + t501
      t503 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t364, -t366, 0.0D0, t502)
      t505 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t366, t364, 0.0D0, t502)
      t507 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t359)
      t509 = x2 * x3
      t510 = 0.1D1 - x3 + t509
      t511 = 0.1D1 / t510
      t512 = t509 * t511
      t513 = t2 * t512
      t515 = t2 * t32 * t511
      t516 = t509 * z
      t517 = t212 * t32
      t519 = Sqrt(t38 * t517)
      t523 = 0.1D1 / (-t39 + t516 - 0.1D1 + x3 + 0.2D1 * t40 * t519)
      t524 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t512
     #, x4)
      t525 = t523 * t524
      t527 = t510 ** 2
      t528 = 0.1D1 / t527
      t529 = t32 * t528
      t533 = log(0.4D1 * t463 * t27 * t212 * t529)
      t535 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t512
     #, x4)
      t541 = t5 * t523 * t535
      t547 = (-0.90D2 * t6 * (-t525 + t533 * t523 * t535) - 0.180D3 * t1
     #0 * t541) * t90 * t239 / 0.720D3
      t548 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t512
     #, x4)
      t554 = log(0.4D1 * t210 * t27 * t315 * t529)
      t555 = t554 * t523
      t557 = t554 ** 2
      t561 = -t523 * t548 + t555 * t524 - t557 * t523 * t535 / 0.2D1
      t565 = -t525 + t555 * t535
      t569 = t20 * t541
      t574 = t547 + (-0.90D2 * t6 * t561 + 0.180D3 * t10 * t5 * t565 - t
     #569) * t90 * t238 / 0.1440D4
      t575 = FJET(XB1, XB2, s, 0.0D0, t513, 0.0D0, -t515, 0.0D0, t574)
      t577 = FJET(XB1, XB2, s, 0.0D0, -t515, 0.0D0, t513, 0.0D0, t574)
      t579 = x2 * x1
      t581 = t2 * t579 * t372
      t584 = t212 * s * t1 * x1
      t585 = t1 ** 2
      t590 = s * t585 * x2 * x1 * t365 * t372
      t591 = t579 * z
      t593 = 0.1D1 / (z + x1 + t591 - t370 - t579)
      t594 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t595 = t593 * t594
      t597 = t373 * t374 * t212
      t600 = log(0.4D1 * t463 * t597)
      t602 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t608 = t10 * t5
      t610 = t593 * t602 * t371
      t616 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t620 = log(0.4D1 * t481 * t597)
      t621 = t620 * t593
      t623 = t620 ** 2
      t643 = (0.90D2 * t6 * (t595 - t600 * t593 * t602) * t371 - 0.180D3
     # * t608 * t610) * t90 * t239 / 0.720D3 + (0.90D2 * t6 * (t593 * t6
     #16 - t621 * t594 + t623 * t593 * t602 / 0.2D1) * t371 - 0.180D3 * 
     #t10 * t5 * (t595 - t621 * t602) * t371 - t20 * t5 * t610) * t238 *
     # t177 / 0.720D3
      t644 = FJET(XB1, XB2, s, 0.0D0, -t581, -t366, -t584, t590, t643)
      t646 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t359)
      t661 = -t425 + (t20 * t5 * t430 - 0.90D2 * t6 * t441 - t445 + 0.18
     #0D3 * t10 * t5 * t449) * t177 / 0.1440D4 + t480 + t501
      t662 = FJET(XB1, XB2, s, t364, -t366, 0.0D0, 0.0D0, 0.0D0, t661)
      t675 = t547 + (-0.90D2 * t6 * t561 + 0.180D3 * t10 * t5 * t565 - t
     #569) * t90 * t238 / 0.1440D4
      t676 = FJET(XB1, XB2, s, t513, 0.0D0, -t515, 0.0D0, 0.0D0, t675)
      t681 = t32 * s * t1 * t365 * t511
      t682 = x2 * z
      t683 = t509 * x1
      t684 = t509 * t370
      t686 = Sqrt(-t399 * t517)
      t687 = t40 * t686
      t690 = z + x1 - t370 - t682 - t579 + t591 - t38 - t395 + t396 + t5
     #16 + t683 - t684 + t210 + 0.2D1 * t687 * x2
      t693 = t364 * t690 * t372 * t511
      t694 = t366 * t512
      t700 = t364 * x2 * (-t38 - t395 + t396 + t516 + t683 - t684 - 0.1D
     #1 + x3 + 0.2D1 * t687) * t372 * t511
      t701 = x3 * t25
      t726 = -0.2D1 * t701 - 0.2D1 * t142 + 0.4D1 * t684 - 0.2D1 * t687 
     #* t579 - 0.3D1 * t701 * t579 - 0.4D1 * t142 * t682 + 0.2D1 * t142 
     #* t25 * x2 - t210 * t370 + t210 * t25 * x1 + 0.2D1 * t210 * t141 *
     # z - t210 * t141 * t25 - 0.2D1 * t687 * t370 + t579 - x1 + 0.2D1 *
     # t687 * t591
      t741 = -t591 - 0.5D1 * t396 - t683 + 0.2D1 * t687 * z + 0.4D1 * t7
     #01 * x1 + 0.4D1 * t142 * z + 0.2D1 * t142 * x2 - 0.2D1 * t142 * t2
     #5 + t509 * t25 - t211 + 0.2D1 * t687 * x1 + t38 + t370 + t395 - z
      t743 = 0.1D1 / (t726 + t741)
      t744 = t371 * t743
      t745 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t512, x
     #4)
      t753 = log(-0.4D1 * t210 * t180 * t369 * t457 * t517 * t528)
      t755 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t512, x
     #4)
      t758 = -t744 * t745 + t753 * t371 * t743 * t755
      t763 = 0.180D3 * t608 * t744 * t755
      t764 = -0.90D2 * t6 * t758 - t763
      t767 = t764 * t90 * t239 / 0.720D3
      t768 = FJET(XB1, XB2, s, t681, -t693, -t694, t700, t590, t767)
      t771 = t90 * t238 * t177
      t774 = FJET(XB1, XB2, s, t700, -t694, -t693, t681, t590, t767)
      t778 = FJET(XB1, XB2, s, -t366, t364, 0.0D0, 0.0D0, 0.0D0, t661)
      t780 = FJET(XB1, XB2, s, -t366, -t584, 0.0D0, -t581, t590, t643)
      t782 = FJET(XB1, XB2, s, -t584, -t366, -t581, 0.0D0, t590, t643)
      t784 = FJET(XB1, XB2, s, -t515, 0.0D0, t513, 0.0D0, 0.0D0, t675)
      t786 = FJET(XB1, XB2, s, -t581, 0.0D0, -t584, -t366, t590, t643)
      t791 = -0.90D2 * t6 * t758 - t763
      t794 = t791 * t90 * t239 / 0.720D3
      t795 = FJET(XB1, XB2, s, -t693, t681, t700, -t694, t590, t794)
      t799 = FJET(XB1, XB2, s, -t694, t700, t681, -t693, t590, t794)
      rrgg2qqbarht6s2e1 = t360 * t359 + t362 * t359 + t503 * t502 + t505
     # * t502 + t507 * t359 + t575 * t574 + t577 * t574 + t644 * t643 + 
     #t646 * t359 + t662 * t661 + t676 * t675 + t768 * t764 * t771 / 0.7
     #20D3 + t774 * t764 * t771 / 0.720D3 + t778 * t661 + t780 * t643 + 
     #t782 * t643 + t784 * t675 + t786 * t643 + t795 * t791 * t771 / 0.7
     #20D3 + t799 * t791 * t771 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht6s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t16 = 0.1D1 / t15
      t20 = log(-0.4D1 * t11 * t14 * t16)
      t21 = t20 ** 2
      t22 = x3 * z
      t23 = 0.2D1 * t22
      t24 = cos(t12)
      t26 = Sqrt(-t22 * t15)
      t30 = 0.1D1 / (-t23 - 0.1D1 + x3 + 0.2D1 * t24 * t26)
      t34 = log(0.4D1 * t11 * t14)
      t35 = t34 ** 2
      t41 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t44 = 0.3141592653589793D1 * lh
      t45 = t5 * t7
      t52 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t58 = lh ** 2
      t60 = 0.3141592653589793D1 ** 2
      t62 = -0.180D3 * t58 + 0.30D2 * t60
      t63 = 0.3141592653589793D1 * t62
      t64 = t63 * t45
      t69 = 0.1D1 / x3
      t72 = t10 * t14
      t74 = log(0.4D1 * t72)
      t76 = t74 ** 2
      t92 = rrgg2qqbarh61J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t107 = x2 ** 2
      t108 = t107 * x3
      t111 = log(0.4D1 * t108 * t72)
      t113 = t72 * t16
      t116 = log(-0.4D1 * t108 * t113)
      t120 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t121 = -0.1D1 + x2
      t122 = t72 * t121
      t125 = log(-0.4D1 * t108 * t122)
      t126 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t131 = t7 * t30
      t132 = -t126 + t7 + t131
      t138 = 0.1D1 / x2
      t141 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D
     #0, x4)
      t142 = t10 * t107
      t143 = t14 * t121
      t146 = log(-0.4D1 * t142 * t143)
      t148 = t146 ** 2
      t153 = log(0.4D1 * t142 * t14)
      t155 = t153 ** 2
      t168 = t5 * (-t126 + t7)
      t173 = x1 ** 2
      t174 = x3 * t173
      t177 = log(-0.4D1 * t174 * t113)
      t183 = log(0.4D1 * t174 * t72)
      t194 = 0.1D1 / x1
      t199 = t69 * t138 * t194
      t202 = t107 * t173
      t205 = log(0.4D1 * t202 * t72)
      t209 = log(-0.4D1 * t202 * t122)
      t220 = t173 * t14
      t223 = log(0.4D1 * t220 * t10)
      t225 = t223 ** 2
      t239 = (-0.90D2 * t6 * t7 * (t21 * t30 / 0.2D1 + t35 / 0.2D1) + (-
     #0.90D2 * t6 * t41 + 0.180D3 * t44 * t45) * (-t20 * t30 - t34) + (-
     #0.90D2 * t6 * t52 + 0.180D3 * t44 * t5 * t41 + t64) * (t30 + 0.1D1
     #)) * t69 / 0.2880D4 + (0.180D3 * (t52 - t74 * t41 + t76 * t7 / 0.2
     #D1) * 0.3141592653589793D1 * lh + t7 * 0.3141592653589793D1 * (-0.
     #60D2 * lh * t60 + 0.2884936567583026D3 + 0.120D3 * t58 * lh) - 0.9
     #0D2 * (t76 * t41 / 0.2D1 + t92 - t76 * t74 * t7 / 0.6D1 - t74 * t5
     #2) * 0.3141592653589793D1 + (t41 - t74 * t7) * 0.3141592653589793D
     #1 * t62) * t5 / 0.2880D4 + (-0.90D2 * t6 * (t41 - t111 * t7 + (t41
     # - t116 * t7) * t30 - t120 + t125 * t126) + 0.180D3 * t44 * t5 * t
     #132) * t69 * t138 / 0.1440D4 + (-0.90D2 * t6 * (-t141 + t146 * t12
     #0 - t148 * t126 / 0.2D1 + t52 - t153 * t41 + t155 * t7 / 0.2D1) + 
     #0.180D3 * t44 * t5 * (-t120 + t146 * t126 + t41 - t153 * t7) + t63
     # * t168) * t138 / 0.1440D4 - (-0.90D2 * t6 * (-(t41 - t177 * t7) *
     # t30 - t41 + t183 * t7) + 0.180D3 * t44 * t5 * (-t131 - t7)) * t69
     # * t194 / 0.1440D4 - t6 * t132 * t199 / 0.8D1 + (-0.90D2 * t6 * (t
     #41 - t205 * t7 - t120 + t209 * t126) + 0.180D3 * t44 * t168) * t13
     #8 * t194 / 0.720D3 + (-0.90D2 * t6 * (t52 - t223 * t41 + t225 * t7
     # / 0.2D1) + 0.180D3 * t44 * t5 * (t41 - t223 * t7) + t64) * t194 /
     # 0.1440D4
      t240 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t239)
      t242 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t239)
      t244 = t2 * x1
      t245 = -0.1D1 + x1
      t246 = t2 * t245
      t247 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t248 = t174 * t14
      t250 = x1 * z
      t251 = -z - x1 + t250
      t252 = 0.1D1 / t251
      t253 = 0.1D1 / t8 * t252
      t254 = t245 ** 2
      t255 = t253 * t254
      t258 = log(-0.4D1 * t248 * t255)
      t259 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t265 = log(0.4D1 * t248 * t253 * t254 * t16)
      t268 = x3 * x1
      t269 = t268 * z
      t272 = x3 * t251
      t274 = Sqrt(t272 * t15)
      t278 = 0.1D1 / (0.2D1 * t269 - 0.2D1 * t268 - t23 + x3 + 0.2D1 * t
     #24 * t274 - 0.1D1)
      t284 = t259 * t278 + t259
      t291 = (-0.90D2 * t6 * (t247 - t258 * t259 + (t247 - t265 * t259) 
     #* t278) + 0.180D3 * t44 * t5 * t284) * t69 * t194 / 0.1440D4
      t295 = -t6 * t284 * t199 / 0.8D1
      t296 = t202 * t14
      t299 = log(-0.4D1 * t296 * t255)
      t304 = t5 * t259
      t310 = (-0.90D2 * t6 * (-t247 + t299 * t259) - 0.180D3 * t44 * t30
     #4) * t138 * t194 / 0.720D3
      t311 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t314 = log(-0.4D1 * t220 * t255)
      t316 = t314 ** 2
      t319 = -t311 + t314 * t247 - t316 * t259 / 0.2D1
      t323 = -t247 + t314 * t259
      t327 = t63 * t304
      t331 = -t291 - t295 + t310 + (-0.90D2 * t6 * t319 + 0.180D3 * t44 
     #* t5 * t323 - t327) * t194 / 0.1440D4
      t332 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t244, -t246, 0.0D0, t331)
      t334 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t246, t244, 0.0D0, t331)
      t336 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t239)
      t338 = x2 * x3
      t339 = 0.1D1 - x3 + t338
      t340 = 0.1D1 / t339
      t341 = t338 * t340
      t342 = t2 * t341
      t344 = t2 * t15 * t340
      t345 = t338 * z
      t346 = t121 * t15
      t348 = Sqrt(t22 * t346)
      t352 = 0.1D1 / (-t23 + t345 - 0.1D1 + x3 + 0.2D1 * t24 * t348)
      t353 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t341
     #, x4)
      t356 = t339 ** 2
      t362 = log(0.4D1 * t108 * t10 * t143 * t15 / t356)
      t364 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t341
     #, x4)
      t366 = -t352 * t353 + t362 * t352 * t364
      t372 = 0.180D3 * t44 * t5 * t352 * t364
      t379 = t138 * t194
      t382 = t6 * t352 * t364 * t69 * t379 / 0.8D1
      t383 = (-0.90D2 * t6 * t366 - t372) * t69 * t138 / 0.1440D4 + t382
      t384 = FJET(XB1, XB2, s, 0.0D0, t342, 0.0D0, -t344, 0.0D0, t383)
      t386 = FJET(XB1, XB2, s, 0.0D0, -t344, 0.0D0, t342, 0.0D0, t383)
      t388 = x2 * x1
      t390 = t2 * t388 * t252
      t393 = t121 * s * t1 * x1
      t394 = t1 ** 2
      t399 = s * t394 * x2 * x1 * t245 * t252
      t400 = t388 * z
      t402 = 0.1D1 / (z + x1 + t400 - t250 - t388)
      t403 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t404 = t402 * t403
      t410 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t416 = log(0.4D1 * t296 * t253 * t254 * t121)
      t431 = t6 * t404 * t251 * t69 * t379 / 0.8D1 + (0.90D2 * t6 * (t40
     #2 * t410 - t416 * t402 * t403) * t251 - 0.180D3 * t44 * t5 * t404 
     #* t251) * t138 * t194 / 0.720D3
      t432 = FJET(XB1, XB2, s, 0.0D0, -t390, -t246, -t393, t399, t431)
      t434 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t239)
      t446 = -t291 - t295 + t310 + (-0.90D2 * t6 * t319 + 0.180D3 * t44 
     #* t5 * t323 - t327) * t194 / 0.1440D4
      t447 = FJET(XB1, XB2, s, t244, -t246, 0.0D0, 0.0D0, 0.0D0, t446)
      t456 = (-0.90D2 * t6 * t366 - t372) * t69 * t138 / 0.1440D4 + t382
      t457 = FJET(XB1, XB2, s, t342, 0.0D0, -t344, 0.0D0, 0.0D0, t456)
      t462 = t15 * s * t1 * t245 * t340
      t463 = x2 * z
      t464 = t338 * x1
      t465 = t338 * t250
      t467 = Sqrt(-t272 * t346)
      t468 = t24 * t467
      t471 = z + x1 - t250 - t463 - t388 + t400 - t22 - t268 + t269 + t3
     #45 + t464 - t465 + t108 + 0.2D1 * t468 * x2
      t474 = t244 * t471 * t252 * t340
      t475 = t246 * t341
      t481 = t244 * x2 * (-t22 - t268 + t269 + t345 + t464 - t465 - 0.1D
     #1 + x3 + 0.2D1 * t468) * t252 * t340
      t485 = x3 * t8
      t498 = t250 + t268 - 0.5D1 * t269 - t464 + 0.2D1 * t468 * z + 0.4D
     #1 * t485 * x1 + 0.4D1 * t174 * z + 0.2D1 * t174 * x2 - 0.2D1 * t17
     #4 * t8 + t338 * t8 - t108 * t173 + 0.2D1 * t468 * x1 + t388 - t400
     # + t22
      t523 = -z + 0.4D1 * t465 - 0.2D1 * t468 * t388 - 0.3D1 * t485 * t3
     #88 - 0.4D1 * t174 * t463 + 0.2D1 * t174 * t8 * x2 - t108 * t250 + 
     #t108 * t8 * x1 + 0.2D1 * t108 * t173 * z - t108 * t173 * t8 - 0.2D
     #1 * t468 * t250 + 0.2D1 * t468 * t400 - x1 - 0.2D1 * t485 - 0.2D1 
     #* t174
      t525 = 0.1D1 / (t498 + t523)
      t528 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t341, x
     #4)
      t532 = t6 * t251 * t525 * t528 * t69 * t379 / 0.8D1
      t533 = FJET(XB1, XB2, s, t462, -t474, -t475, t481, t399, t532)
      t535 = t5 * t251
      t538 = t525 * t528 * t199
      t541 = FJET(XB1, XB2, s, t481, -t475, -t474, t462, t399, t532)
      t546 = FJET(XB1, XB2, s, -t246, t244, 0.0D0, 0.0D0, 0.0D0, t446)
      t548 = FJET(XB1, XB2, s, -t246, -t393, 0.0D0, -t390, t399, t431)
      t550 = FJET(XB1, XB2, s, -t393, -t246, -t390, 0.0D0, t399, t431)
      t552 = FJET(XB1, XB2, s, -t344, 0.0D0, t342, 0.0D0, 0.0D0, t456)
      t554 = FJET(XB1, XB2, s, -t390, 0.0D0, -t393, -t246, t399, t431)
      t556 = FJET(XB1, XB2, s, -t474, t462, t481, -t475, t399, t532)
      t561 = FJET(XB1, XB2, s, -t475, t481, t462, -t474, t399, t532)
      rrgg2qqbarht6s2e0 = t240 * t239 + t242 * t239 + t332 * t331 + t334
     # * t331 + t336 * t239 + t384 * t383 + t386 * t383 + t432 * t431 + 
     #t434 * t239 + t447 * t446 + t457 * t456 + t533 * 0.314159265358979
     #3D1 * t535 * t538 / 0.8D1 + t541 * 0.3141592653589793D1 * t535 * t
     #538 / 0.8D1 + t546 * t446 + t548 * t431 + t550 * t431 + t552 * t45
     #6 + t554 * t431 + t556 * 0.3141592653589793D1 * t535 * t538 / 0.8D
     #1 + t561 * 0.3141592653589793D1 * t535 * t538 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht6s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t20 = log(-0.4D1 * t11 * t14 / t15)
      t21 = x3 * z
      t22 = 0.2D1 * t21
      t23 = cos(t12)
      t25 = Sqrt(-t21 * t15)
      t29 = 0.1D1 / (-t22 - 0.1D1 + x3 + 0.2D1 * t23 * t25)
      t33 = log(0.4D1 * t11 * t14)
      t38 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t41 = 0.3141592653589793D1 * lh
      t44 = 0.180D3 * t41 * t5 * t7
      t49 = 0.1D1 / x3
      t54 = log(0.4D1 * t10 * t14)
      t60 = rrgg2qqbarh61J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t62 = t54 ** 2
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t78 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t79 = t7 * t29
      t82 = 0.1D1 / x2
      t86 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t87 = x2 ** 2
      t88 = t10 * t87
      t89 = -0.1D1 + x2
      t93 = log(-0.4D1 * t88 * t14 * t89)
      t97 = log(0.4D1 * t88 * t14)
      t102 = -t78 + t7
      t110 = 0.1D1 / x1
      t114 = x1 ** 2
      t115 = t114 * t14
      t118 = log(0.4D1 * t115 * t10)
      t131 = (-0.90D2 * t6 * t7 * (-t20 * t29 - t33) + (-0.90D2 * t6 * t
     #38 + t44) * (t29 + 0.1D1)) * t49 / 0.2880D4 + (0.180D3 * (t38 - t5
     #4 * t7) * 0.3141592653589793D1 * lh - 0.90D2 * (t60 - t54 * t38 + 
     #t62 * t7 / 0.2D1) * 0.3141592653589793D1 + t7 * 0.3141592653589793
     #D1 * (-0.180D3 * t69 + 0.30D2 * t71)) * t5 / 0.2880D4 - t6 * (-t78
     # + t7 + t79) * t49 * t82 / 0.16D2 + (-0.90D2 * t6 * (-t86 + t93 * 
     #t78 + t38 - t97 * t7) + 0.180D3 * t41 * t5 * t102) * t82 / 0.1440D
     #4 - t6 * t102 * t82 * t110 / 0.8D1 + (-0.90D2 * t6 * (t38 - t118 *
     # t7) + t44) * t110 / 0.1440D4 + t6 * (-t79 - t7) * t49 * t110 / 0.
     #16D2
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t131)
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t131)
      t136 = t2 * x1
      t137 = -0.1D1 + x1
      t138 = t2 * t137
      t139 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t143 = t6 * t139 * t82 * t110 / 0.8D1
      t144 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D
     #0, x4)
      t146 = x1 * z
      t147 = -z - x1 + t146
      t148 = 0.1D1 / t147
      t150 = t137 ** 2
      t154 = log(-0.4D1 * t115 / t8 * t148 * t150)
      t156 = -t144 + t154 * t139
      t161 = 0.180D3 * t41 * t5 * t139
      t165 = x3 * x1
      t171 = Sqrt(x3 * t147 * t15)
      t181 = t6 * (t139 / (0.2D1 * t165 * z - 0.2D1 * t165 - t22 + x3 + 
     #0.2D1 * t23 * t171 - 0.1D1) + t139) * t49 * t110 / 0.16D2
      t182 = t143 + (-0.90D2 * t6 * t156 - t161) * t110 / 0.1440D4 + t18
     #1
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t136, -t138, 0.0D0, t182)
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t138, t136, 0.0D0, t182)
      t187 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t131)
      t189 = x2 * x3
      t191 = 0.1D1 / (0.1D1 - x3 + t189)
      t192 = t189 * t191
      t193 = t2 * t192
      t195 = t2 * t15 * t191
      t199 = Sqrt(t21 * t89 * t15)
      t203 = 0.1D1 / (-t22 + t189 * z - 0.1D1 + x3 + 0.2D1 * t23 * t199)
      t205 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t192
     #, x4)
      t209 = t6 * t203 * t205 * t49 * t82 / 0.16D2
      t210 = FJET(XB1, XB2, s, 0.0D0, t193, 0.0D0, -t195, 0.0D0, t209)
      t215 = t203 * t205 * t49 * t82
      t218 = FJET(XB1, XB2, s, 0.0D0, -t195, 0.0D0, t193, 0.0D0, t209)
      t223 = x2 * x1
      t225 = t2 * t223 * t148
      t228 = t89 * s * t1 * x1
      t229 = t1 ** 2
      t234 = s * t229 * x2 * x1 * t137 * t148
      t237 = 0.1D1 / (z + x1 + t223 * z - t146 - t223)
      t239 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t242 = t239 * t147 * t82 * t110
      t244 = t6 * t237 * t242 / 0.8D1
      t245 = FJET(XB1, XB2, s, 0.0D0, -t225, -t138, -t228, t234, t244)
      t247 = t5 * t237
      t251 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t131)
      t259 = t143 + (-0.90D2 * t6 * t156 - t161) * t110 / 0.1440D4 + t18
     #1
      t260 = FJET(XB1, XB2, s, t136, -t138, 0.0D0, 0.0D0, 0.0D0, t259)
      t262 = FJET(XB1, XB2, s, t193, 0.0D0, -t195, 0.0D0, 0.0D0, t209)
      t267 = FJET(XB1, XB2, s, -t138, t136, 0.0D0, 0.0D0, 0.0D0, t259)
      t269 = FJET(XB1, XB2, s, -t138, -t228, 0.0D0, -t225, t234, t244)
      t274 = FJET(XB1, XB2, s, -t228, -t138, -t225, 0.0D0, t234, t244)
      t279 = FJET(XB1, XB2, s, -t225, 0.0D0, -t228, -t138, t234, t244)
      t284 = FJET(XB1, XB2, s, -t195, 0.0D0, t193, 0.0D0, 0.0D0, t209)
      rrgg2qqbarht6s2em1 = t132 * t131 + t134 * t131 + t183 * t182 + t18
     #5 * t182 + t187 * t131 + t210 * 0.3141592653589793D1 * t5 * t215 /
     # 0.16D2 + t218 * 0.3141592653589793D1 * t5 * t215 / 0.16D2 + t245 
     #* 0.3141592653589793D1 * t247 * t242 / 0.8D1 + t251 * t131 + t260 
     #* t259 + t262 * 0.3141592653589793D1 * t5 * t215 / 0.16D2 + t267 *
     # t259 + t269 * 0.3141592653589793D1 * t247 * t242 / 0.8D1 + t274 *
     # 0.3141592653589793D1 * t247 * t242 / 0.8D1 + t279 * 0.31415926535
     #89793D1 * t247 * t242 / 0.8D1 + t284 * 0.3141592653589793D1 * t5 *
     # t215 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht6s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t8 = x3 * z
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t14 = Sqrt(-t8 * (-0.1D1 + x3))
      t25 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t31 = 0.1D1 / x1
      t38 = rrgg2qqbarh61J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #0D0, x4)
      t39 = z ** 2
      t42 = Sin(t10)
      t43 = t42 ** 2
      t46 = log(0.4D1 / t39 / z * t43)
      t54 = -t6 * t7 * (0.1D1 / (-0.2D1 * t8 - 0.1D1 + x3 + 0.2D1 * t11 
     #* t14) + 0.1D1) / x3 / 0.32D2 - t6 * (-t25 + t7) / x2 / 0.16D2 - t
     #6 * t7 * t31 / 0.16D2 + (0.180D3 * t7 * 0.3141592653589793D1 * lh 
     #- 0.90D2 * (t38 - t46 * t7) * 0.3141592653589793D1) * t5 / 0.2880D
     #4
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t54)
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t54)
      t59 = t2 * x1
      t61 = t2 * (-0.1D1 + x1)
      t62 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0
     #, x4)
      t65 = t6 * t62 * t31 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t59, -t61, 0.0D0, t65)
      t69 = t5 * t62 * t31
      t72 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t61, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t54)
      t78 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t54)
      t80 = FJET(XB1, XB2, s, t59, -t61, 0.0D0, 0.0D0, 0.0D0, t65)
      t84 = FJET(XB1, XB2, s, -t61, t59, 0.0D0, 0.0D0, 0.0D0, t65)
      rrgg2qqbarht6s2em2 = t55 * t54 + t57 * t54 + t66 * 0.3141592653589
     #793D1 * t69 / 0.16D2 + t72 * 0.3141592653589793D1 * t69 / 0.16D2 +
     # t76 * t54 + t78 * t54 + t80 * 0.3141592653589793D1 * t69 / 0.16D2
     # + t84 * 0.3141592653589793D1 * t69 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht6s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2qqbarh61J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t9 = 0.3141592653589793D1 * t5 * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t5 * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2qqbarht6s2em3 = -t10 * 0.3141592653589793D1 * t12 / 0.32D2 - 
     #t14 * 0.3141592653589793D1 * t12 / 0.32D2 - t17 * 0.31415926535897
     #93D1 * t12 / 0.32D2 - t20 * 0.3141592653589793D1 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht6s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh61J1
      doubleprecision rrgg2qqbarh61J2
      doubleprecision rrgg2qqbarh61J3
      doubleprecision rrgg2qqbarh61J4
      doubleprecision rrgg2qqbarh61J5
      doubleprecision rrgg2qqbarh61J6
      doubleprecision rrgg2qqbarh61J7
      rrgg2qqbarht6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh61J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t14 = t4 ** 2
      t17 = x2 ** 2
      t20 = x1 ** 2
      t22 = t7 ** 2
      t25 = t10 ** 2
      rrgg2qqbarh61J1 = -wd * (-t2 * t4 * x2 * x1 * t7 / t10 - t2 * t14 
     #* t4 * t17 * x2 * t20 * x1 * t22 * t7 / t25 / t10) * nf / z / 0.31
     #41592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t13 = t2 * t4 * x2 * x1 * t7 / t10
      t14 = t4 ** 2
      t17 = x2 ** 2
      t20 = x1 ** 2
      t22 = t7 ** 2
      t25 = t10 ** 2
      t29 = t2 * t14 * t4 * t17 * x2 * t20 * x1 * t22 * t7 / t25 / t10
      rrgg2qqbarh61J2 = -(0.2D1 * wd * (-t13 - t29) + wd * (t29 - 0.2D1 
     #* t2 * t14 * t17 * t20 * t22 / t25 + t13)) * nf / z / 0.3141592653
     #589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t13 = t5 * x2 * x1 * t7 / t10
      t14 = t4 ** 2
      t17 = x2 ** 2
      t20 = x1 ** 2
      t22 = t7 ** 2
      t25 = t10 ** 2
      t27 = 0.1D1 / t25 / t10
      t29 = t2 * t14 * t4 * t17 * x2 * t20 * x1 * t22 * t7 * t27
      t36 = 0.1D1 / t25
      t39 = 0.2D1 * t2 * t14 * t17 * t20 * t22 * t36
      t44 = 0.1D1 - x3
      t45 = 0.1D1 - x2
      t50 = cos(x4 * 0.3141592653589793D1)
      t55 = Sqrt(x3 * t45 * t10 * x2 * t44)
      t58 = t44 * t45 * t10 + x2 * x3 + 0.2D1 * t50 * t55
      t63 = t7 * (z + x1 * t45 * t3)
      t70 = t2 * t4 * t3
      t72 = t58 ** 2
      t76 = t44 ** 2
      rrgg2qqbarh61J3 = -(0.3D1 * wd * (-t13 - t29) + 0.2D1 * wd * (t29 
     #- t39 + t13) + wd * (-t5 * x1 * t36 * t58 * t63 - t39 - t5 * t7 * 
     #t44 * x1 + t70 * t20 * t27 * t72 * t63 + t29 + t70 * t22 * t76 * x
     #1 + t13)) * nf / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t13 = t5 * x2 * x1 * t7 / t10
      t14 = t4 ** 2
      t17 = x2 ** 2
      t20 = x1 ** 2
      t22 = t7 ** 2
      t25 = t10 ** 2
      t27 = 0.1D1 / t25 / t10
      t29 = t2 * t14 * t4 * t17 * x2 * t20 * x1 * t22 * t7 * t27
      t36 = 0.1D1 / t25
      t39 = 0.2D1 * t2 * t14 * t17 * t20 * t22 * t36
      t44 = 0.1D1 - x3
      t45 = 0.1D1 - x2
      t50 = cos(x4 * 0.3141592653589793D1)
      t55 = Sqrt(x3 * t45 * t10 * x2 * t44)
      t58 = t44 * t45 * t10 + x2 * x3 + 0.2D1 * t50 * t55
      t63 = t7 * (z + x1 * t45 * t3)
      t70 = t2 * t4 * t3
      t72 = t58 ** 2
      t76 = t44 ** 2
      rrgg2qqbarh61J4 = -(0.4D1 * wd * (-t13 - t29) + 0.3D1 * wd * (t29 
     #- t39 + t13) + 0.2D1 * wd * (-t5 * x1 * t36 * t58 * t63 - t39 - t5
     # * t7 * t44 * x1 + t70 * t20 * t27 * t72 * t63 + t29 + t70 * t22 *
     # t76 * x1 + t13)) * nf / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t13 = t5 * x2 * x1 * t7 / t10
      t14 = t4 ** 2
      t17 = x2 ** 2
      t20 = x1 ** 2
      t22 = t7 ** 2
      t25 = t10 ** 2
      t27 = 0.1D1 / t25 / t10
      t29 = t2 * t14 * t4 * t17 * x2 * t20 * x1 * t22 * t7 * t27
      t36 = 0.1D1 / t25
      t39 = 0.2D1 * t2 * t14 * t17 * t20 * t22 * t36
      t44 = 0.1D1 - x3
      t45 = 0.1D1 - x2
      t50 = cos(x4 * 0.3141592653589793D1)
      t55 = Sqrt(x3 * t45 * t10 * x2 * t44)
      t58 = t44 * t45 * t10 + x2 * x3 + 0.2D1 * t50 * t55
      t63 = t7 * (z + x1 * t45 * t3)
      t70 = t2 * t4 * t3
      t72 = t58 ** 2
      t76 = t44 ** 2
      rrgg2qqbarh61J5 = -(0.5D1 * wd * (-t13 - t29) + 0.4D1 * wd * (t29 
     #- t39 + t13) + 0.3D1 * wd * (-t5 * x1 * t36 * t58 * t63 - t39 - t5
     # * t7 * t44 * x1 + t70 * t20 * t27 * t72 * t63 + t29 + t70 * t22 *
     # t76 * x1 + t13)) * nf / z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t8 = x2 ** 2
      t11 = x1 ** 2
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t18 = z + x1 * t3
      t19 = t18 ** 2
      t21 = 0.1D1 / t19 / t18
      t23 = t2 * t5 * t4 * t8 * x2 * t11 * x1 * t14 * t13 * t21
      t27 = 0.1D1 / t19
      t30 = 0.2D1 * t2 * t5 * t8 * t11 * t14 * t27
      t31 = t2 * t4
      t36 = t31 * x2 * x1 * t13 / t18
      t41 = 0.1D1 - x3
      t42 = 0.1D1 - x2
      t47 = cos(x4 * 0.3141592653589793D1)
      t52 = Sqrt(x3 * t42 * t18 * x2 * t41)
      t55 = t41 * t42 * t18 + x2 * x3 + 0.2D1 * t47 * t52
      t60 = t13 * (z + x1 * t42 * t3)
      t67 = t2 * t4 * t3
      t69 = t55 ** 2
      t73 = t41 ** 2
      rrgg2qqbarh61J6 = -(0.5D1 * wd * (t23 - t30 + t36) + 0.4D1 * wd * 
     #(-t31 * x1 * t27 * t55 * t60 - t30 - t31 * t13 * t41 * x1 + t67 * 
     #t11 * t21 * t69 * t60 + t23 + t67 * t14 * t73 * x1 + t36)) * nf / 
     #z / 0.3141592653589793D1 / 0.6D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh61J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t8 = z + x1 * t3
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t17 = cos(x4 * 0.3141592653589793D1)
      t22 = Sqrt(x3 * t12 * t8 * x2 * t11)
      t25 = t11 * t12 * t8 + x2 * x3 + 0.2D1 * t17 * t22
      t27 = 0.1D1 - x1
      t31 = t27 * (z + x1 * t12 * t3)
      t34 = t4 ** 2
      t36 = x2 ** 2
      t38 = x1 ** 2
      t39 = t27 ** 2
      t48 = t2 * t4 * t3
      t51 = 0.1D1 / t9 / t8
      t52 = t25 ** 2
      t65 = t11 ** 2
      rrgg2qqbarh61J7 = -0.5D1 / 0.6D1 * wd * (-t5 * x1 * t10 * t25 * t3
     #1 - 0.2D1 * t2 * t34 * t36 * t38 * t39 * t10 - t5 * t27 * t11 * x1
     # + t48 * t38 * t51 * t52 * t31 + t2 * t34 * t4 * t36 * x2 * t38 * 
     #x1 * t39 * t27 * t51 + t48 * t39 * t65 * x1 + t5 * x2 * x1 * t27 /
     # t8) * nf / z / 0.3141592653589793D1

      end function
  
 