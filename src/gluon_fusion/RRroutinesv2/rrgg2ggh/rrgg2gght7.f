      subroutine rrgg2gght7
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt7
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt7
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhard71J1  
      doubleprecision rrgg2gghhard71J2  
      doubleprecision rrgg2gghhard71J3  
      doubleprecision rrgg2gghhard71J4  
      doubleprecision rrgg2gghhard71J5  
      doubleprecision rrgg2gghhard71J6  
      doubleprecision rrgg2gghhardt7s1e1  
      doubleprecision rrgg2gghhardt7s1e0  
      doubleprecision rrgg2gghhardt7s1em1  
      doubleprecision rrgg2gghhardt7s1em2  
      doubleprecision rrgg2gghhardt7s1em3  
      doubleprecision rrgg2gghhardt7s1em4  
      doubleprecision rrgg2gghhardt7s2e1  
      doubleprecision rrgg2gghhardt7s2e0  
      doubleprecision rrgg2gghhardt7s2em1  
      doubleprecision rrgg2gghhardt7s2em2  
      doubleprecision rrgg2gghhardt7s2em3  
      doubleprecision rrgg2gghhardt7s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt7s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t10 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t11 = t10 * t6
      t13 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t14 = t13 * t6
      t15 = pi ** 2
      t17 = lh ** 2
      t19 = -0.30D2 * t15 + 0.180D3 * t17
      t23 = x4 * pi
      t24 = Sin(t23)
      t25 = t24 ** 2
      t26 = x3 * t25
      t27 = z ** 2
      t28 = 0.1D1 / t27
      t31 = log(0.4D1 * t26 * t28)
      t32 = -0.1D1 + x3
      t33 = 0.1D1 / t32
      t37 = log(-0.4D1 * t26 * t28 * t33)
      t38 = cos(t23)
      t40 = Sqrt(-x3 * t32)
      t44 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t38 * t40)
      t48 = t37 ** 2
      t51 = t31 ** 2
      t65 = -0.240D3 * zeta3 - 0.120D3 * t17 * lh + 0.60D2 * lh * t15
      t67 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t85 = 0.1D1 / x3
      t88 = t28 * t25
      t90 = log(0.4D1 * t88)
      t91 = t90 ** 2
      t94 = t91 * t90
      t112 = t15 ** 2
      t113 = t17 ** 2
      t126 = rrgg2gghhard71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 
     #0.0D0, 0.0D0, 0.0D0)
      t127 = t91 ** 2
      t136 = t6 * pi
      t137 = x1 ** 2
      t138 = x3 * t137
      t141 = log(0.4D1 * t138 * t88)
      t143 = t141 ** 2
      t146 = t88 * t33
      t149 = log(-0.4D1 * t138 * t146)
      t151 = t149 ** 2
      t159 = t6 * lh
      t168 = t6 * t19
      t170 = t13 * t44 + t13
      t172 = t168 * pi * t170
      t175 = 0.1D1 / x1
      t178 = t137 * t25
      t179 = t178 * t28
      t181 = log(0.4D1 * t179)
      t186 = t181 ** 2
      t196 = t6 * t65
      t197 = pi * t13
      t198 = t196 * t197
      t209 = x2 ** 2
      t210 = t137 * t209
      t211 = t210 * x3
      t214 = log(-0.4D1 * t211 * t146)
      t218 = t209 * x3
      t219 = t218 * t179
      t221 = log(0.4D1 * t219)
      t232 = 0.1D1 / x2
      t233 = t175 * t232
      t238 = log(0.4D1 * t210 * t88)
      t240 = t238 ** 2
      t258 = log(0.4D1 * t218 * t88)
      t260 = t258 ** 2
      t265 = log(-0.4D1 * t218 * t146)
      t267 = t265 ** 2
      t287 = t209 * t25
      t290 = log(0.4D1 * t287 * t28)
      t295 = t290 ** 2
      t315 = ((-0.180D3 * t7 * lh + 0.90D2 * t11 + t14 * t19) * pi * (t3
     #1 + t37 * t44) + 0.90D2 * t14 * pi * (t48 * t37 * t44 / 0.6D1 + t5
     #1 * t31 / 0.6D1) + (-0.180D3 * t11 * lh + t14 * t65 + 0.90D2 * t67
     # * t6 + t7 * t19) * pi * (-0.1D1 - t44) + (-0.180D3 * t14 * lh + 0
     #.90D2 * t7) * pi * (-t51 / 0.2D1 - t48 * t44 / 0.2D1)) * t85 / 0.2
     #880D4 - (-0.180D3 * (t91 * t3 / 0.2D1 + t67 - t94 * t13 / 0.6D1 - 
     #t90 * t10) * t6 * lh + (-t90 * t3 + t91 * t13 / 0.2D1 + t10) * t6 
     #* t19 + (t3 - t90 * t13) * t6 * t65 + t14 * (t112 + 0.60D2 * t113 
     #+ 0.480D3 * lh * zeta3 - 0.60D2 * t17 * t15) + 0.90D2 * (-t94 * t3
     # / 0.6D1 + t91 * t10 / 0.2D1 - t90 * t67 + t126 + t127 * t13 / 0.2
     #4D2) * t6) * pi / 0.2880D4 - (0.90D2 * t136 * (-t141 * t3 + t143 *
     # t13 / 0.2D1 + t10 + (-t149 * t3 + t151 * t13 / 0.2D1 + t10) * t44
     #) - 0.180D3 * t159 * pi * (t3 - t141 * t13 + (t3 - t149 * t13) * t
     #44) + t172) * t85 * t175 / 0.1440D4 + (t168 * pi * (-t3 + t181 * t
     #13) + 0.90D2 * t136 * (-t186 * t3 / 0.2D1 - t67 + t186 * t181 * t1
     #3 / 0.6D1 + t181 * t10) - t198 - 0.180D3 * t159 * pi * (t181 * t3 
     #- t186 * t13 / 0.2D1 - t10)) * t175 / 0.1440D4 + (0.90D2 * t136 * 
     #(-(t3 - t214 * t13) * t44 - t3 + t221 * t13) + 0.180D3 * t159 * pi
     # * t170) * t85 * t233 / 0.720D3 - (0.90D2 * t136 * (-t238 * t3 + t
     #240 * t13 / 0.2D1 + t10) - 0.180D3 * t159 * pi * (t3 - t238 * t13)
     # + t168 * t197) * t175 * t232 / 0.720D3 - (0.90D2 * t136 * (-t258 
     #* t3 + t260 * t13 / 0.2D1 + t10 + (-t265 * t3 + t267 * t13 / 0.2D1
     # + t10) * t44) - 0.180D3 * t159 * pi * (t3 - t258 * t13 + (t3 - t2
     #65 * t13) * t44) + t172) * t85 * t232 / 0.1440D4 - (t168 * pi * (t
     #3 - t290 * t13) + 0.90D2 * t136 * (t295 * t3 / 0.2D1 + t67 - t295 
     #* t290 * t13 / 0.6D1 - t290 * t10) + t198 - 0.180D3 * t159 * pi * 
     #(-t290 * t3 + t295 * t13 / 0.2D1 + t10)) * t232 / 0.1440D4
      t316 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t315)
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t315)
      t320 = t2 * x1
      t321 = -0.1D1 + x1
      t322 = t2 * t321
      t323 = t138 * t25
      t324 = x1 * z
      t325 = 0.1D1 - x1 + t324
      t326 = 0.1D1 / t325
      t327 = t28 * t326
      t328 = t321 ** 2
      t330 = t327 * t328 * t33
      t333 = log(-0.4D1 * t323 * t330)
      t334 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t32
     #2, 0.0D0, t320, 0.0D0)
      t336 = t333 ** 2
      t337 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t32
     #2, 0.0D0, t320, 0.0D0)
      t340 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t32
     #2, 0.0D0, t320, 0.0D0)
      t342 = x3 * x1
      t343 = t342 * z
      t346 = x3 * t325
      t348 = Sqrt(-t346 * t32)
      t352 = 0.1D1 / (-0.2D1 * t343 + 0.2D1 * t342 - 0.1D1 - x3 + 0.2D1 
     #* t38 * t348)
      t354 = t327 * t328
      t357 = log(0.4D1 * t323 * t354)
      t359 = t357 ** 2
      t374 = -t337 - t337 * t352
      t383 = log(0.4D1 * t178 * t354)
      t388 = t383 ** 2
      t391 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t32
     #2, 0.0D0, t320, 0.0D0)
      t399 = pi * t337
      t411 = t326 * t328
      t415 = log(0.4D1 * t211 * t88 * t411)
      t417 = t218 * t178
      t420 = log(-0.4D1 * t417 * t330)
      t435 = t210 * t25
      t438 = log(0.4D1 * t435 * t354)
      t440 = t438 ** 2
      t456 = -(0.90D2 * t136 * (-(-t333 * t334 + t336 * t337 / 0.2D1 + t
     #340) * t352 + t357 * t334 - t359 * t337 / 0.2D1 - t340) - 0.180D3 
     #* t159 * pi * (-(t334 - t333 * t337) * t352 - t334 + t357 * t337) 
     #+ t168 * pi * t374) * t85 * t175 / 0.1440D4 + (t168 * pi * (t334 -
     # t383 * t337) + 0.90D2 * t136 * (t388 * t334 / 0.2D1 + t391 - t388
     # * t383 * t337 / 0.6D1 - t383 * t340) + t196 * t399 - 0.180D3 * t1
     #59 * pi * (-t383 * t334 + t388 * t337 / 0.2D1 + t340)) * t175 / 0.
     #1440D4 + (0.90D2 * t136 * (t334 - t415 * t337 + (t334 - t420 * t33
     #7) * t352) + 0.180D3 * t159 * pi * t374) * t85 * t233 / 0.720D3 - 
     #(0.90D2 * t136 * (t438 * t334 - t440 * t337 / 0.2D1 - t340) - 0.18
     #0D3 * t159 * pi * (-t334 + t438 * t337) - t168 * t399) * t175 * t2
     #32 / 0.720D3
      t457 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t320, -t322, 0.0D0, t456)
      t459 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t322, t320, 0.0D0, t456)
      t461 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t315)
      t464 = x2 * s * t1
      t465 = -0.1D1 + x2
      t466 = t465 * s
      t467 = t466 * t1
      t468 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t464, -t467
     #, 0.0D0, 0.0D0, 0.0D0)
      t469 = t88 * t465
      t472 = log(-0.4D1 * t211 * t469)
      t473 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t464, -t467
     #, 0.0D0, 0.0D0, 0.0D0)
      t476 = x2 * z
      t478 = 0.1D1 / (0.1D1 - x2 + t476)
      t483 = pi * t473 * t478
      t492 = log(-0.4D1 * t210 * t469)
      t494 = t492 ** 2
      t497 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t464, -t467
     #, 0.0D0, 0.0D0, 0.0D0)
      t508 = t168 * t483
      t515 = log(-0.4D1 * t218 * t469)
      t517 = t515 ** 2
      t534 = t28 * t465
      t537 = log(-0.4D1 * t287 * t534)
      t543 = t537 ** 2
      t546 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, t464, -t467
     #, 0.0D0, 0.0D0, 0.0D0)
      t567 = (0.90D2 * t136 * (t468 - t472 * t473) * t478 - 0.180D3 * t1
     #59 * t483) * t85 * t233 / 0.720D3 - (-0.90D2 * t136 * (-t492 * t46
     #8 + t494 * t473 / 0.2D1 + t497) * t478 + 0.180D3 * t159 * pi * (t4
     #68 - t492 * t473) * t478 - t508) * t175 * t232 / 0.720D3 - (-0.90D
     #2 * t136 * (-t515 * t468 + t517 * t473 / 0.2D1 + t497) * t478 + 0.
     #180D3 * t159 * pi * (t468 - t515 * t473) * t478 - t508) * t85 * t2
     #32 / 0.1440D4 - (-t168 * pi * (t468 - t537 * t473) * t478 - 0.90D2
     # * t136 * (t543 * t468 / 0.2D1 + t546 - t543 * t537 * t473 / 0.6D1
     # - t537 * t497) * t478 - t196 * t483 + 0.180D3 * t159 * pi * (-t53
     #7 * t468 + t543 * t473 / 0.2D1 + t497) * t478) * t232 / 0.1440D4
      t568 = FJET(XB1, XB2, s, 0.0D0, t464, 0.0D0, -t467, 0.0D0, t567)
      t570 = x2 * x3
      t573 = Sqrt(x3 * t465 * t32)
      t574 = t38 * t573
      t576 = 0.2D1 * t574 * x2
      t578 = 0.1D1 - x3 + t570
      t579 = 0.1D1 / t578
      t581 = t2 * (0.1D1 - x3 - x2 + t570 + t218 + t576) * t579
      t582 = 0.2D1 * t574
      t586 = t2 * x2 * (-0.1D1 + t570 + t582) * t579
      t587 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t586, t581
     #, 0.0D0, 0.0D0, 0.0D0)
      t588 = t578 ** 2
      t589 = 0.1D1 / t588
      t591 = t534 * t32 * t589
      t594 = log(0.4D1 * t417 * t591)
      t595 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t586, t581
     #, 0.0D0, 0.0D0, 0.0D0)
      t598 = z * t38
      t603 = z * t209 * x3
      t605 = 0.1D1 / (-0.1D1 - t576 + x2 - x3 + t570 + t582 + 0.2D1 * t5
     #98 * t573 * x2 + t603 - t476 - t218)
      t610 = pi * t595 * t605
      t620 = log(0.4D1 * t218 * t25 * t591)
      t622 = t620 ** 2
      t625 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t586, t581
     #, 0.0D0, 0.0D0, 0.0D0)
      t641 = (0.90D2 * t136 * (t587 - t594 * t595) * t605 - 0.180D3 * t1
     #59 * t610) * t85 * t233 / 0.720D3 - (-0.90D2 * t136 * (-t620 * t58
     #7 + t622 * t595 / 0.2D1 + t625) * t605 + 0.180D3 * t159 * pi * (t5
     #87 - t620 * t595) * t605 - t168 * t610) * t85 * t232 / 0.1440D4
      t642 = FJET(XB1, XB2, s, 0.0D0, t581, 0.0D0, -t586, 0.0D0, t641)
      t644 = FJET(XB1, XB2, s, 0.0D0, -t467, 0.0D0, t464, 0.0D0, t567)
      t646 = FJET(XB1, XB2, s, 0.0D0, -t586, 0.0D0, t581, 0.0D0, t641)
      t650 = t2 * t321 * x2 * t326
      t652 = t466 * t1 * t321
      t653 = t1 ** 2
      t658 = s * t653 * x2 * x1 * t321 * t326
      t659 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t650, t652
     #, 0.0D0, t320, -t658)
      t660 = t325 * t659
      t662 = t327 * t328 * t465
      t665 = log(-0.4D1 * t417 * t662)
      t667 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t650, t652
     #, 0.0D0, t320, -t658)
      t670 = x1 * x2
      t671 = t670 * z
      t673 = 0.1D1 / (-0.1D1 + t671 - t476 + x1 + x2 - t324 - t670)
      t677 = t159 * pi
      t679 = t325 * t667 * t673
      t687 = log(-0.4D1 * t435 * t662)
      t688 = t687 * t325
      t690 = t687 ** 2
      t694 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t650, t652
     #, 0.0D0, t320, -t658)
      t712 = (0.90D2 * t136 * (t660 - t665 * t325 * t667) * t673 - 0.180
     #D3 * t677 * t679) * t85 * t233 / 0.720D3 - (-0.90D2 * t136 * (-t68
     #8 * t659 + t690 * t325 * t667 / 0.2D1 + t325 * t694) * t673 + 0.18
     #0D3 * t159 * pi * (t660 - t688 * t667) * t673 - t168 * pi * t679) 
     #* t175 * t232 / 0.720D3
      t713 = FJET(XB1, XB2, s, 0.0D0, -t650, t320, t652, -t658, t712)
      t715 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t315)
      t717 = FJET(XB1, XB2, s, t320, t652, 0.0D0, -t650, -t658, t712)
      t719 = t316 * t315 + t318 * t315 + t457 * t456 + t459 * t456 + t46
     #1 * t315 + t568 * t567 + t642 * t641 + t644 * t567 + t646 * t641 +
     # t713 * t712 + t715 * t315 + t717 * t712
      t720 = FJET(XB1, XB2, s, t320, -t322, 0.0D0, 0.0D0, 0.0D0, t456)
      t722 = FJET(XB1, XB2, s, t464, 0.0D0, -t467, 0.0D0, 0.0D0, t567)
      t724 = FJET(XB1, XB2, s, t581, 0.0D0, -t586, 0.0D0, 0.0D0, t641)
      t726 = FJET(XB1, XB2, s, t652, t320, -t650, 0.0D0, -t658, t712)
      t729 = t320 * t570 * t579
      t730 = t342 * x2
      t731 = t342 * t476
      t732 = t465 * t32
      t734 = Sqrt(t346 * t732)
      t735 = t38 * t734
      t736 = 0.2D1 * t735
      t741 = t322 * x2 * (t342 - t343 + t570 - t730 + t731 - 0.1D1 + t73
     #6) * t326 * t579
      t745 = t32 * s * t1 * x1 * t579
      t747 = 0.2D1 * t735 * x2
      t748 = 0.1D1 - x1 + t324 - x2 + t670 - t671 - x3 + t342 - t343 + t
     #570 - t730 + t731 + t218 + t747
      t751 = t322 * t748 * t326 * t579
      t752 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t741, -t751
     #, t729, -t745, -t658)
      t758 = log(0.4D1 * t219 * t411 * t732 * t589)
      t760 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t741, -t751
     #, t729, -t745, -t658)
      t767 = t734 * x1
      t771 = x1 * t209
      t778 = t137 * t27
      t789 = 0.1D1 + 0.4D1 * t138 * t476 + t342 * x2 * t27 - 0.2D1 * t59
     #8 * t767 + 0.2D1 * t138 - 0.2D1 * t771 * x3 - 0.2D1 * t138 * x2 - 
     #0.4D1 * t138 * z + 0.2D1 * t778 * x3 + 0.3D1 * t324 * t218 - 0.2D1
     # * t778 * t570 + t476 - t570 + t218 + x3 - 0.4D1 * t731 - 0.2D1 * 
     #t210 * x3 * z
      t790 = t27 * x3
      t806 = -t771 * t790 + t210 * t790 - 0.2D1 * t735 * t670 - 0.2D1 * 
     #t598 * t734 * x2 - x1 - t603 + 0.2D1 * t598 * t767 * x2 - x2 + t67
     #0 + 0.3D1 * t343 + t324 - 0.3D1 * t342 - t671 - t736 + 0.3D1 * t73
     #0 + t747 + 0.2D1 * t735 * x1 + t211
      t808 = 0.1D1 / (t789 + t806)
      t816 = 0.90D2 * t136 * (t325 * t752 - t758 * t325 * t760) * t808 -
     # 0.180D3 * t677 * t325 * t760 * t808
      t819 = t816 * t85 * t233 / 0.720D3
      t820 = FJET(XB1, XB2, s, t729, t741, -t745, -t751, -t658, t819)
      t823 = t85 * t175 * t232
      t826 = FJET(XB1, XB2, s, t741, t729, -t751, -t745, -t658, t819)
      t830 = FJET(XB1, XB2, s, -t322, t320, 0.0D0, 0.0D0, 0.0D0, t456)
      t832 = FJET(XB1, XB2, s, -t467, 0.0D0, t464, 0.0D0, 0.0D0, t567)
      t834 = FJET(XB1, XB2, s, -t586, 0.0D0, t581, 0.0D0, 0.0D0, t641)
      t836 = FJET(XB1, XB2, s, -t650, 0.0D0, t652, t320, -t658, t712)
      t838 = FJET(XB1, XB2, s, -t745, -t751, t729, t741, -t658, t819)
      t842 = FJET(XB1, XB2, s, -t751, -t745, t741, t729, -t658, t819)
      t846 = t720 * t456 + t722 * t567 + t724 * t641 + t726 * t712 + t82
     #0 * t816 * t823 / 0.720D3 + t826 * t816 * t823 / 0.720D3 + t830 * 
     #t456 + t832 * t567 + t834 * t641 + t836 * t712 + t838 * t816 * t82
     #3 / 0.720D3 + t842 * t816 * t823 / 0.720D3
      rrgg2gghhardt7s1e1 = t719 + t846

      end function



      doubleprecision function rrgg2gghhardt7s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = t16 ** 2
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t23 = log(-0.4D1 * t11 * t13 * t19)
      t24 = t23 ** 2
      t25 = cos(t8)
      t27 = Sqrt(-x3 * t18)
      t31 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t25 * t27)
      t40 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t41 = t40 * t6
      t50 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t53 = pi ** 2
      t55 = lh ** 2
      t57 = -0.30D2 * t53 + 0.180D3 * t55
      t64 = 0.1D1 / x3
      t67 = t13 * t10
      t69 = log(0.4D1 * t67)
      t71 = t69 ** 2
      t87 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t102 = t6 * pi
      t103 = x2 ** 2
      t104 = t103 * x3
      t107 = log(0.4D1 * t104 * t67)
      t109 = t67 * t19
      t112 = log(-0.4D1 * t104 * t109)
      t119 = t6 * lh
      t121 = t3 * t31 + t3
      t124 = 0.180D3 * t119 * pi * t121
      t127 = 0.1D1 / x2
      t130 = t103 * t10
      t133 = log(0.4D1 * t130 * t13)
      t135 = t133 ** 2
      t146 = t6 * t57
      t147 = pi * t3
      t148 = t146 * t147
      t152 = x1 ** 2
      t153 = x3 * t152
      t156 = log(0.4D1 * t153 * t67)
      t160 = log(-0.4D1 * t153 * t109)
      t169 = 0.1D1 / x1
      t175 = t64 * t169 * t127
      t178 = t103 * t152
      t181 = log(0.4D1 * t178 * t67)
      t192 = t152 * t10
      t195 = log(0.4D1 * t192 * t13)
      t197 = t195 ** 2
      t211 = (0.90D2 * t7 * pi * (-t17 / 0.2D1 - t24 * t31 / 0.2D1) + (-
     #0.180D3 * t7 * lh + 0.90D2 * t41) * pi * (t16 + t23 * t31) + (-0.1
     #80D3 * t41 * lh + 0.90D2 * t50 * t6 + t7 * t57) * pi * (-0.1D1 - t
     #31)) * t64 / 0.2880D4 - (-0.180D3 * (-t69 * t40 + t71 * t3 / 0.2D1
     # + t50) * t6 * lh + t7 * (-0.240D3 * zeta3 - 0.120D3 * t55 * lh + 
     #0.60D2 * lh * t53) + 0.90D2 * (t71 * t40 / 0.2D1 + t87 - t71 * t69
     # * t3 / 0.6D1 - t69 * t50) * t6 + (t40 - t69 * t3) * t6 * t57) * p
     #i / 0.2880D4 - (0.90D2 * t102 * (t40 - t107 * t3 + (t40 - t112 * t
     #3) * t31) - t124) * t64 * t127 / 0.1440D4 - (0.90D2 * t102 * (-t13
     #3 * t40 + t135 * t3 / 0.2D1 + t50) - 0.180D3 * t119 * pi * (t40 - 
     #t133 * t3) + t148) * t127 / 0.1440D4 - (0.90D2 * t102 * (t40 - t15
     #6 * t3 + (t40 - t160 * t3) * t31) - t124) * t64 * t169 / 0.1440D4 
     #- t102 * t121 * t175 / 0.8D1 - (0.90D2 * t102 * (t40 - t181 * t3) 
     #- 0.180D3 * t119 * t147) * t169 * t127 / 0.720D3 + (0.90D2 * t102 
     #* (t195 * t40 - t197 * t3 / 0.2D1 - t50) - 0.180D3 * t119 * pi * (
     #-t40 + t195 * t3) - t148) * t169 / 0.1440D4
      t212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t211)
      t214 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t211)
      t216 = t2 * x1
      t217 = -0.1D1 + x1
      t218 = t2 * t217
      t219 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t21
     #8, 0.0D0, t216, 0.0D0)
      t220 = t153 * t10
      t221 = x1 * z
      t222 = 0.1D1 - x1 + t221
      t223 = 0.1D1 / t222
      t224 = t13 * t223
      t225 = t217 ** 2
      t230 = log(-0.4D1 * t220 * t224 * t225 * t19)
      t231 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t21
     #8, 0.0D0, t216, 0.0D0)
      t234 = x3 * x1
      t235 = t234 * z
      t238 = x3 * t222
      t240 = Sqrt(-t238 * t18)
      t244 = 0.1D1 / (-0.2D1 * t235 + 0.2D1 * t234 - 0.1D1 - x3 + 0.2D1 
     #* t25 * t240)
      t246 = t224 * t225
      t249 = log(0.4D1 * t220 * t246)
      t255 = -t231 - t231 * t244
      t267 = t178 * t10
      t270 = log(0.4D1 * t267 * t246)
      t275 = pi * t231
      t284 = log(0.4D1 * t192 * t246)
      t286 = t284 ** 2
      t289 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t21
     #8, 0.0D0, t216, 0.0D0)
      t302 = -(0.90D2 * t102 * (-(t219 - t230 * t231) * t244 - t219 + t2
     #49 * t231) - 0.180D3 * t119 * pi * t255) * t64 * t169 / 0.1440D4 -
     # t102 * t255 * t175 / 0.8D1 - (0.90D2 * t102 * (-t219 + t270 * t23
     #1) + 0.180D3 * t119 * t275) * t169 * t127 / 0.720D3 + (0.90D2 * t1
     #02 * (-t284 * t219 + t286 * t231 / 0.2D1 + t289) - 0.180D3 * t119 
     #* pi * (t219 - t284 * t231) + t146 * t275) * t169 / 0.1440D4
      t303 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t216, -t218, 0.0D0, t302)
      t305 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t218, t216, 0.0D0, t302)
      t307 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t211)
      t310 = x2 * s * t1
      t311 = -0.1D1 + x2
      t312 = t311 * s
      t313 = t312 * t1
      t314 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t310, -t313
     #, 0.0D0, 0.0D0, 0.0D0)
      t315 = t67 * t311
      t318 = log(-0.4D1 * t104 * t315)
      t319 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t310, -t313
     #, 0.0D0, 0.0D0, 0.0D0)
      t322 = x2 * z
      t324 = 0.1D1 / (0.1D1 - x2 + t322)
      t329 = pi * t319 * t324
      t331 = 0.180D3 * t119 * t329
      t336 = t13 * t311
      t339 = log(-0.4D1 * t130 * t336)
      t341 = t339 ** 2
      t344 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t310, -t313
     #, 0.0D0, 0.0D0, 0.0D0)
      t361 = t169 * t127
      t367 = log(-0.4D1 * t178 * t315)
      t377 = -(-0.90D2 * t102 * (t314 - t318 * t319) * t324 + t331) * t6
     #4 * t127 / 0.1440D4 - (-0.90D2 * t102 * (-t339 * t314 + t341 * t31
     #9 / 0.2D1 + t344) * t324 + 0.180D3 * t119 * pi * (t314 - t339 * t3
     #19) * t324 - t146 * t329) * t127 / 0.1440D4 + t102 * t319 * t324 *
     # t64 * t361 / 0.8D1 - (-0.90D2 * t102 * (t314 - t367 * t319) * t32
     #4 + t331) * t169 * t127 / 0.720D3
      t378 = FJET(XB1, XB2, s, 0.0D0, t310, 0.0D0, -t313, 0.0D0, t377)
      t380 = x2 * x3
      t383 = Sqrt(x3 * t311 * t18)
      t384 = t25 * t383
      t386 = 0.2D1 * t384 * x2
      t388 = 0.1D1 - x3 + t380
      t389 = 0.1D1 / t388
      t391 = t2 * (0.1D1 - x3 - x2 + t380 + t104 + t386) * t389
      t392 = 0.2D1 * t384
      t396 = t2 * x2 * (-0.1D1 + t380 + t392) * t389
      t397 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t396, t391
     #, 0.0D0, 0.0D0, 0.0D0)
      t399 = t388 ** 2
      t405 = log(0.4D1 * t104 * t10 * t336 * t18 / t399)
      t406 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t396, t391
     #, 0.0D0, 0.0D0, 0.0D0)
      t409 = z * t25
      t414 = z * t103 * x3
      t416 = 0.1D1 / (-0.1D1 - t386 + x2 - x3 + t380 + t392 + 0.2D1 * t4
     #09 * t383 * x2 + t414 - t322 - t104)
      t433 = -(-0.90D2 * t102 * (t397 - t405 * t406) * t416 + 0.180D3 * 
     #t119 * pi * t406 * t416) * t64 * t127 / 0.1440D4 + t102 * t406 * t
     #416 * t64 * t361 / 0.8D1
      t434 = FJET(XB1, XB2, s, 0.0D0, t391, 0.0D0, -t396, 0.0D0, t433)
      t436 = FJET(XB1, XB2, s, 0.0D0, -t313, 0.0D0, t310, 0.0D0, t377)
      t438 = FJET(XB1, XB2, s, 0.0D0, -t396, 0.0D0, t391, 0.0D0, t433)
      t442 = t2 * t217 * x2 * t223
      t444 = t312 * t1 * t217
      t445 = t1 ** 2
      t450 = s * t445 * x2 * x1 * t217 * t223
      t451 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t442, t444
     #, 0.0D0, t216, -t450)
      t452 = t222 * t451
      t454 = x1 * x2
      t455 = t454 * z
      t457 = 0.1D1 / (-0.1D1 + t455 - t322 + x1 + x2 - t221 - t454)
      t462 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t442, t444
     #, 0.0D0, t216, -t450)
      t468 = log(-0.4D1 * t267 * t224 * t225 * t311)
      t483 = t102 * t452 * t457 * t64 * t361 / 0.8D1 - (-0.90D2 * t102 *
     # (t222 * t462 - t468 * t222 * t451) * t457 + 0.180D3 * t119 * pi *
     # t452 * t457) * t169 * t127 / 0.720D3
      t484 = FJET(XB1, XB2, s, 0.0D0, -t442, t216, t444, -t450, t483)
      t486 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t211)
      t488 = FJET(XB1, XB2, s, t216, t444, 0.0D0, -t442, -t450, t483)
      t490 = t212 * t211 + t214 * t211 + t303 * t302 + t305 * t302 + t30
     #7 * t211 + t378 * t377 + t434 * t433 + t436 * t377 + t438 * t433 +
     # t484 * t483 + t486 * t211 + t488 * t483
      t491 = FJET(XB1, XB2, s, t216, -t218, 0.0D0, 0.0D0, 0.0D0, t302)
      t493 = FJET(XB1, XB2, s, t310, 0.0D0, -t313, 0.0D0, 0.0D0, t377)
      t495 = FJET(XB1, XB2, s, t391, 0.0D0, -t396, 0.0D0, 0.0D0, t433)
      t497 = FJET(XB1, XB2, s, t444, t216, -t442, 0.0D0, -t450, t483)
      t500 = t216 * t380 * t389
      t501 = t234 * x2
      t502 = t234 * t322
      t505 = Sqrt(t238 * t311 * t18)
      t506 = t25 * t505
      t507 = 0.2D1 * t506
      t512 = t218 * x2 * (t234 - t235 + t380 - t501 + t502 - 0.1D1 + t50
     #7) * t223 * t389
      t516 = t18 * s * t1 * x1 * t389
      t518 = 0.2D1 * t506 * x2
      t519 = 0.1D1 - x1 + t221 - x2 + t454 - t455 - x3 + t234 - t235 + t
     #380 - t501 + t502 + t104 + t518
      t522 = t218 * t519 * t223 * t389
      t523 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t512, -t522
     #, t500, -t516, -t450)
      t534 = x1 * t103
      t537 = t152 * t12
      t543 = t12 * x3
      t551 = t505 * x1
      t558 = 0.1D1 + 0.3D1 * t501 + t518 + 0.2D1 * t506 * x1 + t178 * x3
     # - 0.4D1 * t153 * z - 0.2D1 * t153 * x2 - 0.2D1 * t534 * x3 + 0.2D
     #1 * t537 * x3 - 0.2D1 * t178 * x3 * z - t534 * t543 + t178 * t543 
     #- 0.2D1 * t506 * t454 - 0.2D1 * t409 * t505 * x2 - 0.2D1 * t409 * 
     #t551 + t234 * x2 * t12 + 0.4D1 * t153 * t322
      t570 = -0.2D1 * t537 * t380 + 0.3D1 * t221 * t104 - 0.3D1 * t234 -
     # 0.4D1 * t502 + t454 + t221 + 0.2D1 * t153 - t507 + 0.3D1 * t235 +
     # 0.2D1 * t409 * t551 * x2 + x3 - x1 - t414 - t455 + t322 - t380 + 
     #t104 - x2
      t572 = 0.1D1 / (t558 + t570)
      t576 = t102 * t222 * t523 * t572 * t64 * t361 / 0.8D1
      t577 = FJET(XB1, XB2, s, t500, t512, -t516, -t522, -t450, t576)
      t579 = pi * t222
      t582 = t523 * t572 * t175
      t585 = FJET(XB1, XB2, s, t512, t500, -t522, -t516, -t450, t576)
      t590 = FJET(XB1, XB2, s, -t218, t216, 0.0D0, 0.0D0, 0.0D0, t302)
      t592 = FJET(XB1, XB2, s, -t313, 0.0D0, t310, 0.0D0, 0.0D0, t377)
      t594 = FJET(XB1, XB2, s, -t396, 0.0D0, t391, 0.0D0, 0.0D0, t433)
      t596 = FJET(XB1, XB2, s, -t442, 0.0D0, t444, t216, -t450, t483)
      t598 = FJET(XB1, XB2, s, -t516, -t522, t500, t512, -t450, t576)
      t603 = FJET(XB1, XB2, s, -t522, -t516, t512, t500, -t450, t576)
      t608 = t491 * t302 + t493 * t377 + t495 * t433 + t497 * t483 + t57
     #7 * t6 * t579 * t582 / 0.8D1 + t585 * t6 * t579 * t582 / 0.8D1 + t
     #590 * t302 + t592 * t377 + t594 * t433 + t596 * t483 + t598 * t6 *
     # t579 * t582 / 0.8D1 + t603 * t6 * t579 * t582 / 0.8D1
      rrgg2gghhardt7s1e0 = t490 + t608

      end function



      doubleprecision function rrgg2gghhardt7s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = -0.1D1 + x3
      t22 = log(-0.4D1 * t11 * t13 / t17)
      t23 = cos(t8)
      t25 = Sqrt(-x3 * t17)
      t29 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t23 * t25)
      t37 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t45 = 0.1D1 / x3
      t50 = log(0.4D1 * t13 * t10)
      t57 = t50 ** 2
      t60 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t64 = pi ** 2
      t66 = lh ** 2
      t73 = t6 * pi
      t76 = (t3 * t29 + t3) * t45
      t77 = 0.1D1 / x2
      t81 = x2 ** 2
      t82 = t81 * t10
      t85 = log(0.4D1 * t82 * t13)
      t90 = t6 * lh
      t93 = 0.180D3 * t90 * pi * t3
      t97 = 0.1D1 / x1
      t102 = x1 ** 2
      t103 = t102 * t10
      t106 = log(0.4D1 * t103 * t13)
      t117 = (0.90D2 * t7 * pi * (t16 + t22 * t29) + (-0.180D3 * t7 * lh
     # + 0.90D2 * t37 * t6) * pi * (-0.1D1 - t29)) * t45 / 0.2880D4 - (-
     #0.180D3 * (t37 - t50 * t3) * t6 * lh + 0.90D2 * (-t50 * t37 + t57 
     #* t3 / 0.2D1 + t60) * t6 + t7 * (-0.30D2 * t64 + 0.180D3 * t66)) *
     # pi / 0.2880D4 - t73 * t76 * t77 / 0.16D2 - (0.90D2 * t73 * (t37 -
     # t85 * t3) - t93) * t77 / 0.1440D4 - t7 * pi * t97 * t77 / 0.8D1 +
     # (0.90D2 * t73 * (-t37 + t106 * t3) + t93) * t97 / 0.1440D4 - t73 
     #* t76 * t97 / 0.16D2
      t118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t117)
      t120 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t117)
      t122 = t2 * x1
      t123 = -0.1D1 + x1
      t124 = t2 * t123
      t125 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t12
     #4, 0.0D0, t122, 0.0D0)
      t130 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t12
     #4, 0.0D0, t122, 0.0D0)
      t131 = x1 * z
      t132 = 0.1D1 - x1 + t131
      t133 = 0.1D1 / t132
      t135 = t123 ** 2
      t139 = log(0.4D1 * t103 * t13 * t133 * t135)
      t150 = x3 * x1
      t156 = Sqrt(-x3 * t132 * t17)
      t167 = t73 * t125 * t97 * t77 / 0.8D1 + (0.90D2 * t73 * (t130 - t1
     #39 * t125) - 0.180D3 * t90 * pi * t125) * t97 / 0.1440D4 - t73 * (
     #-t125 - t125 / (-0.2D1 * t150 * z + 0.2D1 * t150 - 0.1D1 - x3 + 0.
     #2D1 * t23 * t156)) * t45 * t97 / 0.16D2
      t168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t122, -t124, 0.0D0, t167)
      t170 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t124, t122, 0.0D0, t167)
      t172 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t117)
      t175 = x2 * s * t1
      t176 = -0.1D1 + x2
      t177 = t176 * s
      t178 = t177 * t1
      t179 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t175, -t178
     #, 0.0D0, 0.0D0, 0.0D0)
      t180 = t73 * t179
      t181 = x2 * z
      t183 = 0.1D1 / (0.1D1 - x2 + t181)
      t188 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t175, -t178
     #, 0.0D0, 0.0D0, 0.0D0)
      t192 = log(-0.4D1 * t82 * t13 * t176)
      t209 = t180 * t183 * t45 * t77 / 0.16D2 - (-0.90D2 * t73 * (t188 -
     # t192 * t179) * t183 + 0.180D3 * t90 * pi * t179 * t183) * t77 / 0
     #.1440D4 + t180 * t183 * t97 * t77 / 0.8D1
      t210 = FJET(XB1, XB2, s, 0.0D0, t175, 0.0D0, -t178, 0.0D0, t209)
      t212 = x2 * x3
      t213 = t81 * x3
      t216 = Sqrt(x3 * t176 * t17)
      t217 = t23 * t216
      t219 = 0.2D1 * t217 * x2
      t222 = 0.1D1 / (0.1D1 - x3 + t212)
      t224 = t2 * (0.1D1 - x3 - x2 + t212 + t213 + t219) * t222
      t225 = 0.2D1 * t217
      t229 = t2 * x2 * (-0.1D1 + t212 + t225) * t222
      t230 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t229, t224
     #, 0.0D0, 0.0D0, 0.0D0)
      t239 = 0.1D1 / (-0.1D1 - t219 + x2 - x3 + t212 + t225 + 0.2D1 * z 
     #* t23 * t216 * x2 + z * t81 * x3 - t181 - t213)
      t243 = t73 * t230 * t239 * t45 * t77 / 0.16D2
      t244 = FJET(XB1, XB2, s, 0.0D0, t224, 0.0D0, -t229, 0.0D0, t243)
      t249 = t230 * t239 * t45 * t77
      t252 = FJET(XB1, XB2, s, 0.0D0, -t178, 0.0D0, t175, 0.0D0, t209)
      t254 = FJET(XB1, XB2, s, 0.0D0, -t229, 0.0D0, t224, 0.0D0, t243)
      t261 = t2 * t123 * x2 * t133
      t263 = t177 * t1 * t123
      t264 = t1 ** 2
      t269 = s * t264 * x2 * x1 * t123 * t133
      t271 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263
     #, 0.0D0, t122, -t269)
      t272 = x1 * x2
      t278 = t271 / (-0.1D1 + t272 * z - t181 + x1 + x2 - t131 - t272) *
     # t97 * t77
      t280 = t73 * t132 * t278 / 0.8D1
      t281 = FJET(XB1, XB2, s, 0.0D0, -t261, t122, t263, -t269, t280)
      t283 = pi * t132
      t287 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t117)
      t289 = FJET(XB1, XB2, s, t122, t263, 0.0D0, -t261, -t269, t280)
      t294 = FJET(XB1, XB2, s, t122, -t124, 0.0D0, 0.0D0, 0.0D0, t167)
      t296 = FJET(XB1, XB2, s, t175, 0.0D0, -t178, 0.0D0, 0.0D0, t209)
      t298 = FJET(XB1, XB2, s, t224, 0.0D0, -t229, 0.0D0, 0.0D0, t243)
      t303 = FJET(XB1, XB2, s, t263, t122, -t261, 0.0D0, -t269, t280)
      t308 = FJET(XB1, XB2, s, -t124, t122, 0.0D0, 0.0D0, 0.0D0, t167)
      t310 = FJET(XB1, XB2, s, -t178, 0.0D0, t175, 0.0D0, 0.0D0, t209)
      t312 = FJET(XB1, XB2, s, -t261, 0.0D0, t263, t122, -t269, t280)
      t317 = FJET(XB1, XB2, s, -t229, 0.0D0, t224, 0.0D0, 0.0D0, t243)
      rrgg2gghhardt7s1em1 = t118 * t117 + t120 * t117 + t168 * t167 + t1
     #70 * t167 + t172 * t117 + t210 * t209 + t244 * t6 * pi * t249 / 0.
     #16D2 + t252 * t209 + t254 * t6 * pi * t249 / 0.16D2 + t281 * t6 * 
     #t283 * t278 / 0.8D1 + t287 * t117 + t289 * t6 * t283 * t278 / 0.8D
     #1 + t294 * t167 + t296 * t209 + t298 * t6 * pi * t249 / 0.16D2 + t
     #303 * t6 * t283 * t278 / 0.8D1 + t308 * t167 + t310 * t209 + t312 
     #* t6 * t283 * t278 / 0.8D1 + t317 * t6 * pi * t249 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt7s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = x4 * pi
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t23 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t33 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t34 = z ** 2
      t36 = Sin(t8)
      t37 = t36 ** 2
      t40 = log(0.4D1 / t34 * t37)
      t48 = t7 * pi * (-0.1D1 - 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t9 * t12)
     #) / x3 / 0.32D2 - t7 * pi * t23 / 0.16D2 - t7 * pi * t27 / 0.16D2 
     #- (-0.180D3 * t7 * lh + 0.90D2 * (t33 - t40 * t3) * t6) * pi / 0.2
     #880D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t53 = t2 * x1
      t55 = t2 * (-0.1D1 + x1)
      t56 = t6 * pi
      t57 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t55,
     # 0.0D0, t53, 0.0D0)
      t60 = t56 * t57 * t27 / 0.16D2
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t53, -t55, 0.0D0, t60)
      t64 = pi * t57 * t27
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t55, t53, 0.0D0, t60)
      t71 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t74 = x2 * s * t1
      t77 = (-0.1D1 + x2) * s * t1
      t78 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t74, -t77, 0
     #.0D0, 0.0D0, 0.0D0)
      t83 = t78 / (0.1D1 - x2 + x2 * z) * t23
      t85 = t56 * t83 / 0.16D2
      t86 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, -t77, 0.0D0, t85)
      t91 = FJET(XB1, XB2, s, 0.0D0, -t77, 0.0D0, t74, 0.0D0, t85)
      t96 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t98 = FJET(XB1, XB2, s, t53, -t55, 0.0D0, 0.0D0, 0.0D0, t60)
      t102 = FJET(XB1, XB2, s, t74, 0.0D0, -t77, 0.0D0, 0.0D0, t85)
      t107 = FJET(XB1, XB2, s, -t77, 0.0D0, t74, 0.0D0, 0.0D0, t85)
      t112 = FJET(XB1, XB2, s, -t55, t53, 0.0D0, 0.0D0, 0.0D0, t60)
      rrgg2gghhardt7s1em2 = t49 * t48 + t51 * t48 + t61 * t6 * t64 / 0.1
     #6D2 + t67 * t6 * t64 / 0.16D2 + t71 * t48 + t86 * t6 * pi * t83 / 
     #0.16D2 + t91 * t6 * pi * t83 / 0.16D2 + t96 * t48 + t98 * t6 * t64
     # / 0.16D2 + t102 * t6 * pi * t83 / 0.16D2 + t107 * t6 * pi * t83 /
     # 0.16D2 + t112 * t6 * t64 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt7s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t9 = t3 * t6 * pi / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t6 * pi
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2gghhardt7s1em3 = -t10 * t3 * t12 / 0.32D2 - t14 * t3 * t12 / 
     #0.32D2 - t17 * t3 * t12 / 0.32D2 - t20 * t3 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2gghhardt7s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt7s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t16 = -0.1D1 + x3
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t21 = log(-0.4D1 * t8 * t18)
      t22 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t28 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t9)
      t34 = Sqrt(-t30 * t16)
      t38 = 0.1D1 / (-0.1D1 - t31 + 0.2D1 * t32 * t34 + x3)
      t42 = log(0.4D1 * t8 * t15)
      t44 = t42 ** 2
      t50 = t5 * lh
      t59 = pi ** 2
      t61 = lh ** 2
      t63 = -0.30D2 * t59 + 0.180D3 * t61
      t64 = t5 * t63
      t70 = 0.1D1 / x3
      t72 = 0.1D1 / x1
      t75 = t7 * t11
      t76 = t75 * t14
      t78 = log(0.4D1 * t76)
      t83 = t78 ** 2
      t86 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t99 = -0.240D3 * zeta3 - 0.120D3 * t61 * lh + 0.60D2 * lh * t59
      t100 = t5 * t99
      t101 = pi * t25
      t113 = x2 ** 2
      t114 = t113 * x3
      t117 = log(0.4D1 * t114 * t76)
      t119 = t7 * t113
      t120 = t119 * x3
      t121 = -0.1D1 + x2
      t122 = t15 * t121
      t125 = log(-0.4D1 * t120 * t122)
      t129 = log(-0.4D1 * t120 * t18)
      t136 = t101 * t38
      t141 = 0.1D1 / x2
      t142 = t72 * t141
      t147 = log(0.4D1 * t119 * t15)
      t149 = t147 ** 2
      t154 = log(-0.4D1 * t119 * t122)
      t156 = t154 ** 2
      t172 = t22 * t5
      t175 = t28 * t5
      t177 = t25 * t5
      t180 = (-0.180D3 * t172 * lh + 0.90D2 * t175 + t177 * t63) * pi
      t181 = x3 * t14
      t184 = log(0.4D1 * t181 * t11)
      t188 = log(-0.4D1 * t181 * t11 * t17)
      t192 = t188 ** 2
      t195 = t184 ** 2
      t216 = (-0.180D3 * t177 * lh + 0.90D2 * t172) * pi
      t226 = log(0.4D1 * t114 * t15)
      t228 = t226 ** 2
      t233 = log(-0.4D1 * t114 * t18)
      t235 = t233 ** 2
      t242 = log(-0.4D1 * t114 * t122)
      t244 = t242 ** 2
      t264 = t14 * t113
      t267 = log(0.4D1 * t264 * t11)
      t268 = t267 ** 2
      t269 = t11 * t121
      t272 = log(-0.4D1 * t264 * t269)
      t273 = t272 ** 2
      t290 = log(0.4D1 * t15)
      t291 = t290 ** 2
      t294 = t291 * t290
      t312 = t59 ** 2
      t313 = t61 ** 2
      t326 = rrgg2gghhard71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, 0.0D0, t2, 0.0D0)
      t327 = t291 ** 2
      t336 = -(0.90D2 * t6 * ((-t21 * t22 + t24 * t25 / 0.2D1 + t28) * t
     #38 - t42 * t22 + t44 * t25 / 0.2D1 + t28) - 0.180D3 * t50 * pi * (
     #(t22 - t21 * t25) * t38 + t22 - t42 * t25) + t64 * pi * (t25 * t38
     # + t25)) * t70 * t72 / 0.1440D4 + (t64 * pi * (-t22 + t78 * t25) +
     # 0.90D2 * t6 * (-t83 * t22 / 0.2D1 - t86 + t83 * t78 * t25 / 0.6D1
     # + t78 * t28) - t100 * t101 - 0.180D3 * t50 * pi * (t78 * t22 - t8
     #3 * t25 / 0.2D1 - t28)) * t72 / 0.1440D4 + (0.90D2 * t6 * (t117 * 
     #t25 - t125 * t25 - (t22 - t129 * t25) * t38) + 0.180D3 * t50 * t13
     #6) * t70 * t142 / 0.720D3 + (0.90D2 * t6 * (t147 * t22 - t149 * t2
     #5 / 0.2D1 - t154 * t22 + t156 * t25 / 0.2D1) - 0.180D3 * t50 * pi 
     #* (t147 * t25 - t154 * t25)) * t72 * t141 / 0.720D3 - (t180 * (-t1
     #84 - t188 * t38) + 0.90D2 * t177 * pi * (-t192 * t188 * t38 / 0.6D
     #1 - t195 * t184 / 0.6D1) + (-0.180D3 * t175 * lh + t177 * t99 + 0.
     #90D2 * t86 * t5 + t172 * t63) * pi * (0.1D1 + t38) + t216 * (t195 
     #/ 0.2D1 + t192 * t38 / 0.2D1)) * t70 / 0.2880D4 - (0.90D2 * t6 * (
     #-t226 * t22 + t228 * t25 / 0.2D1 + (-t233 * t22 + t235 * t25 / 0.2
     #D1 + t28) * t38 + t242 * t22 - t244 * t25 / 0.2D1) - 0.180D3 * t50
     # * pi * (-t226 * t25 + (t22 - t233 * t25) * t38 + t242 * t25) + t6
     #4 * t136) * t70 * t141 / 0.1440D4 - (t216 * (t268 / 0.2D1 - t273 /
     # 0.2D1) + 0.90D2 * t177 * pi * (t273 * t272 / 0.6D1 - t268 * t267 
     #/ 0.6D1) + t180 * (-t267 + t272)) * t141 / 0.1440D4 - (-0.180D3 * 
     #(t291 * t22 / 0.2D1 + t86 - t294 * t25 / 0.6D1 - t290 * t28) * t5 
     #* lh + (-t290 * t22 + t291 * t25 / 0.2D1 + t28) * t5 * t63 + (t22 
     #- t290 * t25) * t5 * t99 + t177 * (t312 + 0.60D2 * t313 + 0.480D3 
     #* lh * zeta3 - 0.60D2 * t61 * t59) + 0.90D2 * (-t294 * t22 / 0.6D1
     # + t291 * t28 / 0.2D1 - t290 * t86 + t326 + t327 * t25 / 0.24D2) *
     # t5) * pi / 0.2880D4
      t337 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t336)
      t339 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t336)
      t341 = t2 * x1
      t342 = -0.1D1 + x1
      t343 = t2 * t342
      t344 = t8 * t11
      t345 = 0.1D1 / t12
      t346 = x1 * z
      t347 = -z - x1 + t346
      t348 = 0.1D1 / t347
      t349 = t345 * t348
      t350 = t342 ** 2
      t352 = t349 * t350 * t17
      t355 = log(0.4D1 * t344 * t352)
      t356 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t341
     #, 0.0D0, -t343, 0.0D0)
      t358 = t355 ** 2
      t359 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t341
     #, 0.0D0, -t343, 0.0D0)
      t362 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t341
     #, 0.0D0, -t343, 0.0D0)
      t364 = x3 * x1
      t365 = t364 * z
      t368 = x3 * t347
      t370 = Sqrt(t368 * t16)
      t374 = 0.1D1 / (0.2D1 * t365 - 0.2D1 * t364 - t31 - 0.1D1 + 0.2D1 
     #* t32 * t370 + x3)
      t376 = t349 * t350
      t379 = log(-0.4D1 * t344 * t376)
      t381 = t379 ** 2
      t396 = -t359 - t359 * t374
      t405 = log(-0.4D1 * t75 * t376)
      t410 = t405 ** 2
      t413 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t341
     #, 0.0D0, -t343, 0.0D0)
      t421 = pi * t359
      t433 = t114 * t75
      t436 = log(0.4D1 * t433 * t352)
      t441 = t348 * t350
      t445 = log(-0.4D1 * t120 * t11 * t345 * t441)
      t458 = t119 * t11
      t461 = log(-0.4D1 * t458 * t376)
      t463 = t461 ** 2
      t479 = -(0.90D2 * t6 * (-(-t355 * t356 + t358 * t359 / 0.2D1 + t36
     #2) * t374 + t379 * t356 - t381 * t359 / 0.2D1 - t362) - 0.180D3 * 
     #t50 * pi * (-(t356 - t355 * t359) * t374 - t356 + t379 * t359) + t
     #64 * pi * t396) * t70 * t72 / 0.1440D4 + (t64 * pi * (t356 - t405 
     #* t359) + 0.90D2 * t6 * (t410 * t356 / 0.2D1 + t413 - t410 * t405 
     #* t359 / 0.6D1 - t405 * t362) + t100 * t421 - 0.180D3 * t50 * pi *
     # (-t405 * t356 + t410 * t359 / 0.2D1 + t362)) * t72 / 0.1440D4 + (
     #0.90D2 * t6 * ((t356 - t436 * t359) * t374 + t356 - t445 * t359) +
     # 0.180D3 * t50 * pi * t396) * t70 * t142 / 0.720D3 + (0.90D2 * t6 
     #* (-t461 * t356 + t463 * t359 / 0.2D1 + t362) - 0.180D3 * t50 * pi
     # * (t356 - t461 * t359) + t64 * t421) * t72 * t141 / 0.720D3
      t480 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t341, -t343, 0.0D0, t479)
      t482 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t343, t341, 0.0D0, t479)
      t484 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t336)
      t486 = x2 * x3
      t487 = 0.1D1 - x3 + t486
      t488 = 0.1D1 / t487
      t489 = t486 * t488
      t490 = t2 * t489
      t492 = t2 * t16 * t488
      t494 = t487 ** 2
      t495 = 0.1D1 / t494
      t496 = t16 * t495
      t500 = log(0.4D1 * t114 * t14 * t269 * t496)
      t501 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t490, -t492, 0.0D0)
      t503 = t500 ** 2
      t504 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t490, -t492, 0.0D0)
      t507 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t490, -t492, 0.0D0)
      t509 = t486 * z
      t510 = t121 * t16
      t512 = Sqrt(t30 * t510)
      t516 = 0.1D1 / (t509 - t31 - 0.1D1 + 0.2D1 * t32 * t512 + x3)
      t527 = pi * t504 * t516
      t537 = log(0.4D1 * t433 * t14 * t121 * t496)
      t549 = -(0.90D2 * t6 * (t500 * t501 - t503 * t504 / 0.2D1 - t507) 
     #* t516 - 0.180D3 * t50 * pi * (-t501 + t500 * t504) * t516 - t64 *
     # t527) * t70 * t141 / 0.1440D4 + (-0.90D2 * t6 * (-t501 + t537 * t
     #504) * t516 - 0.180D3 * t50 * t527) * t70 * t142 / 0.720D3
      t550 = FJET(XB1, XB2, s, 0.0D0, t490, 0.0D0, -t492, 0.0D0, t549)
      t552 = FJET(XB1, XB2, s, 0.0D0, -t492, 0.0D0, t490, 0.0D0, t549)
      t554 = x1 * x2
      t556 = t2 * t554 * t348
      t559 = t121 * s * t1 * x1
      t560 = t1 ** 2
      t565 = s * t560 * x2 * x1 * t342 * t348
      t566 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t556, -t55
     #9, 0.0D0, -t343, t565)
      t568 = t349 * t350 * t121
      t571 = log(0.4D1 * t433 * t568)
      t572 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t556, -t55
     #9, 0.0D0, -t343, t565)
      t575 = t554 * z
      t577 = 0.1D1 / (z + x1 - t346 - t554 + t575)
      t582 = t50 * pi
      t584 = t572 * t577 * t347
      t592 = log(0.4D1 * t458 * t568)
      t594 = t592 ** 2
      t597 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t556, -t55
     #9, 0.0D0, -t343, t565)
      t615 = (0.90D2 * t6 * (t566 - t571 * t572) * t577 * t347 - 0.180D3
     # * t582 * t584) * t70 * t142 / 0.720D3 + (0.90D2 * t6 * (-t592 * t
     #566 + t594 * t572 / 0.2D1 + t597) * t577 * t347 - 0.180D3 * t582 *
     # (t566 - t592 * t572) * t577 * t347 + t64 * pi * t584) * t72 * t14
     #1 / 0.720D3
      t616 = FJET(XB1, XB2, s, 0.0D0, -t556, -t343, -t559, t565, t615)
      t618 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t336)
      t620 = FJET(XB1, XB2, s, t341, -t343, 0.0D0, 0.0D0, 0.0D0, t479)
      t622 = FJET(XB1, XB2, s, t490, 0.0D0, -t492, 0.0D0, 0.0D0, t549)
      t627 = t16 * s * t1 * t342 * t488
      t628 = x2 * z
      t629 = t364 * x2
      t630 = t364 * t628
      t632 = Sqrt(-t368 * t510)
      t633 = t32 * t632
      t634 = t633 * x2
      t636 = z + x1 - t346 - t628 - t554 + t575 - t30 - t364 + t365 + t5
     #09 + t629 - t630 + t114 + 0.2D1 * t634
      t639 = t341 * t636 * t348 * t488
      t640 = t343 * t489
      t646 = t341 * x2 * (-t30 - t364 + t365 + t509 + t629 - t630 - 0.1D
     #1 + x3 + 0.2D1 * t633) * t348 * t488
      t647 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t646, -t639
     #, -t640, t627, t565)
      t655 = log(-0.4D1 * t114 * t75 * t345 * t441 * t510 * t495)
      t657 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t646, -t639
     #, -t640, t627, t565)
      t666 = x3 * t12
      t674 = t7 * t12
      t678 = x1 * t32
      t682 = -z - x1 + t346 + t364 + t30 + 0.2D1 * t346 * t634 + 0.4D1 *
     # t630 + 0.2D1 * t119 * t30 + x1 * t113 * t666 - t119 * t666 - 0.3D
     #1 * t364 * x2 * t12 - 0.4D1 * t8 * t628 + 0.2D1 * t674 * t486 - t3
     #46 * t114 - 0.2D1 * t678 * t632 * x2
      t702 = -0.2D1 * t346 * t633 - 0.5D1 * t365 - t120 + t486 * t12 + 0
     #.2D1 * t678 * t632 + 0.2D1 * z * t32 * t632 + 0.4D1 * t364 * t12 +
     # 0.4D1 * t8 * z + 0.2D1 * t8 * x2 - 0.2D1 * t674 * x3 + t554 - 0.2
     #D1 * t666 - 0.2D1 * t8 - t629 - t575
      t704 = 0.1D1 / (t682 + t702)
      t712 = 0.90D2 * t6 * (t347 * t647 - t655 * t347 * t657) * t704 - 0
     #.180D3 * t582 * t347 * t657 * t704
      t715 = t712 * t70 * t142 / 0.720D3
      t716 = FJET(XB1, XB2, s, t627, -t639, -t640, t646, t565, t715)
      t719 = t70 * t72 * t141
      t722 = FJET(XB1, XB2, s, t646, -t640, -t639, t627, t565, t715)
      t726 = FJET(XB1, XB2, s, -t343, t341, 0.0D0, 0.0D0, 0.0D0, t479)
      t728 = FJET(XB1, XB2, s, -t343, -t559, 0.0D0, -t556, t565, t615)
      t730 = FJET(XB1, XB2, s, -t492, 0.0D0, t490, 0.0D0, 0.0D0, t549)
      t732 = FJET(XB1, XB2, s, -t559, -t343, -t556, 0.0D0, t565, t615)
      t734 = FJET(XB1, XB2, s, -t556, 0.0D0, -t559, -t343, t565, t615)
      t736 = FJET(XB1, XB2, s, -t639, t627, t646, -t640, t565, t715)
      t740 = FJET(XB1, XB2, s, -t640, t646, t627, -t639, t565, t715)
      rrgg2gghhardt7s2e1 = t337 * t336 + t339 * t336 + t480 * t479 + t48
     #2 * t479 + t484 * t336 + t550 * t549 + t552 * t549 + t616 * t615 +
     # t618 * t336 + t620 * t479 + t622 * t549 + t716 * t712 * t719 / 0.
     #720D3 + t722 * t712 * t719 / 0.720D3 + t726 * t479 + t728 * t615 +
     # t730 * t549 + t732 * t615 + t734 * t615 + t736 * t712 * t719 / 0.
     #720D3 + t740 * t712 * t719 / 0.720D3

      end function



      doubleprecision function rrgg2gghhardt7s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t23 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t26 = x3 * z
      t27 = 0.2D1 * t26
      t28 = cos(t10)
      t30 = Sqrt(-t26 * t17)
      t34 = 0.1D1 / (-0.1D1 - t27 + 0.2D1 * t28 * t30 + x3)
      t38 = log(0.4D1 * t9 * t16)
      t43 = t5 * lh
      t50 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t57 = 0.1D1 / x2
      t58 = t52 * t57
      t62 = x2 ** 2
      t63 = t62 * t8
      t66 = log(0.4D1 * t63 * t16)
      t68 = -0.1D1 + x2
      t69 = t16 * t68
      t72 = log(-0.4D1 * t63 * t69)
      t79 = t8 * t12
      t82 = log(0.4D1 * t79 * t15)
      t84 = t82 ** 2
      t87 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t96 = pi ** 2
      t98 = lh ** 2
      t100 = -0.30D2 * t96 + 0.180D3 * t98
      t101 = t5 * t100
      t102 = pi * t23
      t107 = t23 * t5
      t108 = x3 * t15
      t111 = log(0.4D1 * t108 * t12)
      t112 = t111 ** 2
      t116 = log(-0.4D1 * t108 * t12 * t18)
      t117 = t116 ** 2
      t126 = t7 * t5
      t129 = (-0.180D3 * t107 * lh + 0.90D2 * t126) * pi
      t146 = log(0.4D1 * t16)
      t148 = t146 ** 2
      t164 = rrgg2gghhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, 0.0D0, t2, 0.0D0)
      t179 = t62 * x3
      t182 = log(0.4D1 * t179 * t16)
      t186 = log(-0.4D1 * t179 * t19)
      t192 = log(-0.4D1 * t179 * t69)
      t204 = t15 * t62
      t207 = log(0.4D1 * t204 * t12)
      t208 = t207 ** 2
      t209 = t12 * t68
      t212 = log(-0.4D1 * t204 * t209)
      t213 = t212 ** 2
      t224 = -(0.90D2 * t6 * ((t7 - t22 * t23) * t34 + t7 - t38 * t23) -
     # 0.180D3 * t43 * pi * (t23 * t34 + t23)) * t50 * t52 / 0.1440D4 - 
     #t6 * t23 * t34 * t50 * t58 / 0.8D1 + t6 * (t66 * t23 - t72 * t23) 
     #* t52 * t57 / 0.8D1 + (0.90D2 * t6 * (t82 * t7 - t84 * t23 / 0.2D1
     # - t87) - 0.180D3 * t43 * pi * (-t7 + t82 * t23) - t101 * t102) * 
     #t52 / 0.1440D4 - (0.90D2 * t107 * pi * (t112 / 0.2D1 + t117 * t34 
     #/ 0.2D1) + t129 * (-t111 - t116 * t34) + (-0.180D3 * t126 * lh + 0
     #.90D2 * t87 * t5 + t107 * t100) * pi * (0.1D1 + t34)) * t50 / 0.28
     #80D4 - (-0.180D3 * (-t146 * t7 + t148 * t23 / 0.2D1 + t87) * t5 * 
     #lh + t107 * (-0.240D3 * zeta3 - 0.120D3 * t98 * lh + 0.60D2 * lh *
     # t96) + 0.90D2 * (t148 * t7 / 0.2D1 + t164 - t148 * t146 * t23 / 0
     #.6D1 - t146 * t87) * t5 + (t7 - t146 * t23) * t5 * t100) * pi / 0.
     #2880D4 - (0.90D2 * t6 * (-t182 * t23 + (t7 - t186 * t23) * t34 + t
     #192 * t23) - 0.180D3 * t43 * t102 * t34) * t50 * t57 / 0.1440D4 - 
     #(0.90D2 * t107 * pi * (t208 / 0.2D1 - t213 / 0.2D1) + t129 * (-t20
     #7 + t212)) * t57 / 0.1440D4
      t225 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t224)
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t224)
      t229 = t2 * x1
      t230 = -0.1D1 + x1
      t231 = t2 * t230
      t232 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t229
     #, 0.0D0, -t231, 0.0D0)
      t233 = t9 * t12
      t235 = x1 * z
      t236 = -z - x1 + t235
      t237 = 0.1D1 / t236
      t238 = 0.1D1 / t13 * t237
      t239 = t230 ** 2
      t244 = log(0.4D1 * t233 * t238 * t239 * t18)
      t245 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t229
     #, 0.0D0, -t231, 0.0D0)
      t248 = x3 * x1
      t249 = t248 * z
      t252 = x3 * t236
      t254 = Sqrt(t252 * t17)
      t258 = 0.1D1 / (0.2D1 * t249 - 0.2D1 * t248 - t27 - 0.1D1 + 0.2D1 
     #* t28 * t254 + x3)
      t260 = t238 * t239
      t263 = log(-0.4D1 * t233 * t260)
      t269 = -t245 - t245 * t258
      t280 = t50 * t52 * t57
      t283 = t63 * t12
      t286 = log(-0.4D1 * t283 * t260)
      t291 = pi * t245
      t300 = log(-0.4D1 * t79 * t260)
      t302 = t300 ** 2
      t305 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t229
     #, 0.0D0, -t231, 0.0D0)
      t318 = -(0.90D2 * t6 * (-(t232 - t244 * t245) * t258 - t232 + t263
     # * t245) - 0.180D3 * t43 * pi * t269) * t50 * t52 / 0.1440D4 - t6 
     #* t269 * t280 / 0.8D1 + (0.90D2 * t6 * (t232 - t286 * t245) - 0.18
     #0D3 * t43 * t291) * t52 * t57 / 0.720D3 + (0.90D2 * t6 * (-t300 * 
     #t232 + t302 * t245 / 0.2D1 + t305) - 0.180D3 * t43 * pi * (t232 - 
     #t300 * t245) + t101 * t291) * t52 / 0.1440D4
      t319 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t229, -t231, 0.0D0, t318)
      t321 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t231, t229, 0.0D0, t318)
      t323 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t224)
      t325 = x2 * x3
      t326 = 0.1D1 - x3 + t325
      t327 = 0.1D1 / t326
      t328 = t325 * t327
      t329 = t2 * t328
      t331 = t2 * t17 * t327
      t332 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t329, -t331, 0.0D0)
      t334 = t325 * z
      t335 = t68 * t17
      t337 = Sqrt(t26 * t335)
      t341 = 0.1D1 / (t334 - t27 - 0.1D1 + 0.2D1 * t28 * t337 + x3)
      t346 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t329, -t331, 0.0D0)
      t348 = t326 ** 2
      t354 = log(0.4D1 * t179 * t15 * t209 * t17 / t348)
      t368 = t6 * t332 * t341 * t50 * t58 / 0.8D1 - (0.90D2 * t6 * (-t34
     #6 + t354 * t332) * t341 + 0.180D3 * t43 * pi * t332 * t341) * t50 
     #* t57 / 0.1440D4
      t369 = FJET(XB1, XB2, s, 0.0D0, t329, 0.0D0, -t331, 0.0D0, t368)
      t371 = FJET(XB1, XB2, s, 0.0D0, -t331, 0.0D0, t329, 0.0D0, t368)
      t373 = x1 * x2
      t375 = t2 * t373 * t237
      t378 = t68 * s * t1 * x1
      t379 = t1 ** 2
      t384 = s * t379 * x2 * x1 * t230 * t237
      t385 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t375, -t37
     #8, 0.0D0, -t231, t384)
      t386 = t373 * z
      t388 = 0.1D1 / (z + x1 - t235 - t373 + t386)
      t389 = t385 * t388
      t395 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t375, -t37
     #8, 0.0D0, -t231, t384)
      t400 = log(0.4D1 * t283 * t238 * t239 * t68)
      t415 = t6 * t389 * t236 * t50 * t58 / 0.8D1 + (0.90D2 * t6 * (t395
     # - t400 * t385) * t388 * t236 - 0.180D3 * t43 * pi * t389 * t236) 
     #* t52 * t57 / 0.720D3
      t416 = FJET(XB1, XB2, s, 0.0D0, -t375, -t231, -t378, t384, t415)
      t418 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t224)
      t420 = FJET(XB1, XB2, s, t229, -t231, 0.0D0, 0.0D0, 0.0D0, t318)
      t422 = FJET(XB1, XB2, s, t329, 0.0D0, -t331, 0.0D0, 0.0D0, t368)
      t427 = t17 * s * t1 * t230 * t327
      t428 = x2 * z
      t429 = t248 * x2
      t430 = t248 * t428
      t432 = Sqrt(-t252 * t335)
      t433 = t28 * t432
      t434 = t433 * x2
      t436 = z + x1 - t235 - t428 - t373 + t386 - t26 - t248 + t249 + t3
     #34 + t429 - t430 + t179 + 0.2D1 * t434
      t439 = t229 * t436 * t237 * t327
      t440 = t231 * t328
      t446 = t229 * x2 * (-t26 - t248 + t249 + t334 + t429 - t430 - 0.1D
     #1 + x3 + 0.2D1 * t433) * t237 * t327
      t447 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t446, -t439
     #, -t440, t427, t384)
      t450 = x3 * t13
      t458 = t8 * t13
      t462 = x1 * t28
      t474 = t373 - t386 - 0.2D1 * t450 - 0.2D1 * t9 - 0.3D1 * t248 * x2
     # * t13 - 0.4D1 * t9 * t428 + 0.2D1 * t458 * t325 - t235 * t179 - 0
     #.2D1 * t462 * t432 * x2 - 0.2D1 * t235 * t433 - x1 + 0.4D1 * t430 
     #+ 0.2D1 * t63 * t26 + x1 * t62 * t450 - t63 * t450
      t493 = t235 + t248 + 0.2D1 * t235 * t434 - z - 0.5D1 * t249 + 0.4D
     #1 * t9 * z + 0.2D1 * t9 * x2 - 0.2D1 * t458 * x3 - t429 - t63 * x3
     # + t325 * t13 + 0.2D1 * t462 * t432 + 0.2D1 * z * t28 * t432 + 0.4
     #D1 * t248 * t13 + t26
      t495 = 0.1D1 / (t474 + t493)
      t499 = t6 * t236 * t447 * t495 * t50 * t58 / 0.8D1
      t500 = FJET(XB1, XB2, s, t427, -t439, -t440, t446, t384, t499)
      t502 = pi * t236
      t505 = t447 * t495 * t280
      t508 = FJET(XB1, XB2, s, t446, -t440, -t439, t427, t384, t499)
      t513 = FJET(XB1, XB2, s, -t231, t229, 0.0D0, 0.0D0, 0.0D0, t318)
      t515 = FJET(XB1, XB2, s, -t231, -t378, 0.0D0, -t375, t384, t415)
      t517 = FJET(XB1, XB2, s, -t331, 0.0D0, t329, 0.0D0, 0.0D0, t368)
      t519 = FJET(XB1, XB2, s, -t378, -t231, -t375, 0.0D0, t384, t415)
      t521 = FJET(XB1, XB2, s, -t375, 0.0D0, -t378, -t231, t384, t415)
      t523 = FJET(XB1, XB2, s, -t439, t427, t446, -t440, t384, t499)
      t528 = FJET(XB1, XB2, s, -t440, t446, t427, -t439, t384, t499)
      rrgg2gghhardt7s2e0 = t225 * t224 + t227 * t224 + t319 * t318 + t32
     #1 * t318 + t323 * t224 + t369 * t368 + t371 * t368 + t416 * t415 +
     # t418 * t224 + t420 * t318 + t422 * t368 + t500 * t5 * t502 * t505
     # / 0.8D1 + t508 * t5 * t502 * t505 / 0.8D1 + t513 * t318 + t515 * 
     #t415 + t517 * t368 + t519 * t415 + t521 * t415 + t523 * t5 * t502 
     #* t505 / 0.8D1 + t528 * t5 * t502 * t505 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt7s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t17 = log(0.4D1 * t11 * t14)
      t18 = -0.1D1 + x3
      t23 = log(-0.4D1 * t11 * t14 / t18)
      t24 = x3 * z
      t25 = 0.2D1 * t24
      t26 = cos(t12)
      t28 = Sqrt(-t24 * t18)
      t32 = 0.1D1 / (-0.1D1 - t25 + 0.2D1 * t26 * t28 + x3)
      t40 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t48 = 0.1D1 / x3
      t51 = t6 * pi
      t52 = x1 ** 2
      t53 = t52 * t14
      t56 = log(0.4D1 * t53 * t10)
      t61 = t6 * lh
      t66 = 0.1D1 / x1
      t77 = 0.1D1 / x2
      t81 = x2 ** 2
      t82 = t10 * t81
      t85 = log(0.4D1 * t82 * t14)
      t86 = -0.1D1 + x2
      t90 = log(-0.4D1 * t82 * t14 * t86)
      t98 = log(0.4D1 * t10 * t14)
      t105 = t98 ** 2
      t108 = rrgg2gghhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, 0.0D0, t2, 0.0D0)
      t112 = pi ** 2
      t114 = lh ** 2
      t121 = -(0.90D2 * t7 * pi * (-t17 - t23 * t32) + (-0.180D3 * t7 * 
     #lh + 0.90D2 * t40 * t6) * pi * (0.1D1 + t32)) * t48 / 0.2880D4 + (
     #0.90D2 * t51 * (-t40 + t56 * t3) + 0.180D3 * t61 * pi * t3) * t66 
     #/ 0.1440D4 - t51 * (t3 * t32 + t3) * t48 * t66 / 0.16D2 - t51 * t3
     # * t32 * t48 * t77 / 0.16D2 - t7 * pi * (-t85 + t90) * t77 / 0.16D
     #2 - (-0.180D3 * (t40 - t98 * t3) * t6 * lh + 0.90D2 * (-t98 * t40 
     #+ t105 * t3 / 0.2D1 + t108) * t6 + t7 * (-0.30D2 * t112 + 0.180D3 
     #* t114)) * pi / 0.2880D4
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t124 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t126 = t2 * x1
      t127 = -0.1D1 + x1
      t128 = t2 * t127
      t129 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t126
     #, 0.0D0, -t128, 0.0D0)
      t134 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t126
     #, 0.0D0, -t128, 0.0D0)
      t136 = x1 * z
      t137 = -z - x1 + t136
      t138 = 0.1D1 / t137
      t140 = t127 ** 2
      t144 = log(-0.4D1 * t53 / t8 * t138 * t140)
      t155 = x3 * x1
      t161 = Sqrt(x3 * t137 * t18)
      t172 = t51 * t129 * t66 * t77 / 0.8D1 + (0.90D2 * t51 * (t134 - t1
     #44 * t129) - 0.180D3 * t61 * pi * t129) * t66 / 0.1440D4 - t51 * (
     #-t129 - t129 / (0.2D1 * t155 * z - 0.2D1 * t155 - t25 - 0.1D1 + 0.
     #2D1 * t26 * t161 + x3)) * t48 * t66 / 0.16D2
      t173 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t126, -t128, 0.0D0, t172)
      t175 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t128, t126, 0.0D0, t172)
      t177 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t179 = x2 * x3
      t181 = 0.1D1 / (0.1D1 - x3 + t179)
      t183 = t2 * t179 * t181
      t185 = t2 * t18 * t181
      t186 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t183, -t185, 0.0D0)
      t191 = Sqrt(t24 * t86 * t18)
      t195 = 0.1D1 / (t179 * z - t25 - 0.1D1 + 0.2D1 * t26 * t191 + x3)
      t199 = t51 * t186 * t195 * t48 * t77 / 0.16D2
      t200 = FJET(XB1, XB2, s, 0.0D0, t183, 0.0D0, -t185, 0.0D0, t199)
      t205 = t186 * t195 * t48 * t77
      t208 = FJET(XB1, XB2, s, 0.0D0, -t185, 0.0D0, t183, 0.0D0, t199)
      t213 = x1 * x2
      t215 = t2 * t213 * t138
      t218 = t86 * s * t1 * x1
      t219 = t1 ** 2
      t224 = s * t219 * x2 * x1 * t127 * t138
      t225 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t215, -t21
     #8, 0.0D0, -t128, t224)
      t232 = 0.1D1 / (z + x1 - t136 - t213 + t213 * z) * t137 * t66 * t7
     #7
      t234 = t51 * t225 * t232 / 0.8D1
      t235 = FJET(XB1, XB2, s, 0.0D0, -t215, -t128, -t218, t224, t234)
      t237 = pi * t225
      t241 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t243 = FJET(XB1, XB2, s, t126, -t128, 0.0D0, 0.0D0, 0.0D0, t172)
      t245 = FJET(XB1, XB2, s, t183, 0.0D0, -t185, 0.0D0, 0.0D0, t199)
      t250 = FJET(XB1, XB2, s, -t128, t126, 0.0D0, 0.0D0, 0.0D0, t172)
      t252 = FJET(XB1, XB2, s, -t128, -t218, 0.0D0, -t215, t224, t234)
      t257 = FJET(XB1, XB2, s, -t185, 0.0D0, t183, 0.0D0, 0.0D0, t199)
      t262 = FJET(XB1, XB2, s, -t218, -t128, -t215, 0.0D0, t224, t234)
      t267 = FJET(XB1, XB2, s, -t215, 0.0D0, -t218, -t128, t224, t234)
      rrgg2gghhardt7s2em1 = t122 * t121 + t124 * t121 + t173 * t172 + t1
     #75 * t172 + t177 * t121 + t200 * t6 * pi * t205 / 0.16D2 + t208 * 
     #t6 * pi * t205 / 0.16D2 + t235 * t6 * t237 * t232 / 0.8D1 + t241 *
     # t121 + t243 * t172 + t245 * t6 * pi * t205 / 0.16D2 + t250 * t172
     # + t252 * t6 * t237 * t232 / 0.8D1 + t257 * t6 * pi * t205 / 0.16D
     #2 + t262 * t6 * t237 * t232 / 0.8D1 + t267 * t6 * t237 * t232 / 0.
     #8D1

      end function



      doubleprecision function rrgg2gghhardt7s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = 0.1D1 / x1
      t12 = x3 * z
      t14 = x4 * pi
      t15 = cos(t14)
      t18 = Sqrt(-t12 * (-0.1D1 + x3))
      t31 = rrgg2gghhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t32 = z ** 2
      t35 = Sin(t14)
      t36 = t35 ** 2
      t39 = log(0.4D1 / t32 / z * t36)
      t47 = -t7 * pi * t8 / 0.16D2 - t7 * pi * (0.1D1 + 0.1D1 / (-0.1D1 
     #- 0.2D1 * t12 + 0.2D1 * t15 * t18 + x3)) / x3 / 0.32D2 - (-0.180D3
     # * t7 * lh + 0.90D2 * (t31 - t39 * t3) * t6) * pi / 0.2880D4
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t47)
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t47)
      t52 = t2 * x1
      t54 = t2 * (-0.1D1 + x1)
      t56 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t52, 
     #0.0D0, -t54, 0.0D0)
      t59 = t6 * pi * t56 * t8 / 0.16D2
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t52, -t54, 0.0D0, t59)
      t63 = pi * t56 * t8
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t54, t52, 0.0D0, t59)
      t70 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t47)
      t72 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t47)
      t74 = FJET(XB1, XB2, s, t52, -t54, 0.0D0, 0.0D0, 0.0D0, t59)
      t78 = FJET(XB1, XB2, s, -t54, t52, 0.0D0, 0.0D0, 0.0D0, t59)
      rrgg2gghhardt7s2em2 = t48 * t47 + t50 * t47 + t60 * t6 * t63 / 0.1
     #6D2 + t66 * t6 * t63 / 0.16D2 + t70 * t47 + t72 * t47 + t74 * t6 *
     # t63 / 0.16D2 + t78 * t6 * t63 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt7s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2gghhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t9 = t3 * t6 * pi / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t6 * pi
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2gghhardt7s2em3 = -t10 * t3 * t12 / 0.32D2 - t14 * t3 * t12 / 
     #0.32D2 - t17 * t3 * t12 / 0.32D2 - t20 * t3 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2gghhardt7s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2gghhard71J1
      doubleprecision rrgg2gghhard71J2
      doubleprecision rrgg2gghhard71J3
      doubleprecision rrgg2gghhard71J4
      doubleprecision rrgg2gghhard71J5
      doubleprecision rrgg2gghhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt7s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2gghhard71J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t9 = 0.27D2 / 0.2D1 * S14
      t12 = 0.27D2 / 0.2D1 * S24
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S24
      t27 = 0.9D1 / 0.2D1 * S14
      t29 = S34 ** 2
      t38 = 0.9D1 / 0.2D1 * S14 * S24
      t39 = S14 ** 2
      t41 = S24 ** 2
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2gghhard71J1 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 +
     # 0.18D2 * S23 + 0.18D2 * S24) * S12 + (-0.27D2 / 0.2D1 * S13 - t9 
     #- 0.27D2 * S34 - 0.27D2 / 0.2D1 * S23 - t12) * S34 + (t15 + t12) *
     # t17 + (t9 + t19) * t21 + ((0.9D1 / 0.2D1 * S23 + t24 + 0.9D1 * S3
     #4 + 0.9D1 / 0.2D1 * S13 + t27) * t29 + ((t27 - t19) * t17 + (-t15 
     #+ t24) * t21) * S34 + (-t38 + 0.9D1 * t39 + 0.9D1 / 0.2D1 * t41) *
     # t17 + (0.9D1 / 0.2D1 * t39 - t38 + 0.9D1 * t41) * t21) * t50) * s
     # * z + 0.9D1 * t55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29
     # - 0.18D2 * t29 * S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard71J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t8 = S13 + S14 + S34
      t11 = S23 + S24 + S34
      t15 = 0.9D1 / 0.2D1 * S24
      t18 = 0.9D1 / 0.2D1 * S14
      t20 = S34 ** 2
      t31 = 0.9D1 / 0.2D1 * S14 * S24
      t32 = S14 ** 2
      t34 = S24 ** 2
      t43 = 0.1D1 / S12
      t48 = S12 ** 2
      t57 = t20 ** 2
      rrgg2gghhard71J2 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 +
     # 0.18D2 * S23 + 0.18D2 * S24) * S12 + 0.9D1 * t8 * S14 + 0.9D1 * t
     #11 * S24 + ((0.9D1 / 0.2D1 * S23 + t15 + 0.9D1 * S34 + 0.9D1 / 0.2
     #D1 * S13 + t18) * t20 + ((t18 - 0.9D1 * S24) * t8 + (-0.9D1 * S14 
     #+ t15) * t11) * S34 + (-t31 + 0.9D1 * t32 + 0.9D1 / 0.2D1 * t34) *
     # t8 + (0.9D1 / 0.2D1 * t32 - t31 + 0.9D1 * t34) * t11) * t43) * s 
     #* z + 0.9D1 * t48 * S12 - 0.18D2 * t48 * S34 + 0.27D2 * S12 * t20 
     #- 0.18D2 * t20 * S34 + 0.9D1 * t57 * t43) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard71J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t9 = 0.27D2 / 0.2D1 * S14
      t12 = 0.27D2 / 0.2D1 * S24
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S24
      t27 = 0.9D1 / 0.2D1 * S14
      t29 = S34 ** 2
      t38 = 0.9D1 / 0.2D1 * S14 * S24
      t39 = S14 ** 2
      t41 = S24 ** 2
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2gghhard71J3 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 +
     # 0.18D2 * S23 + 0.18D2 * S24) * S12 + (0.27D2 / 0.2D1 * S13 + t9 +
     # 0.27D2 * S34 + 0.27D2 / 0.2D1 * S23 + t12) * S34 + (t15 - t12) * 
     #t17 + (-t9 + t19) * t21 + ((0.9D1 / 0.2D1 * S23 + t24 + 0.9D1 * S3
     #4 + 0.9D1 / 0.2D1 * S13 + t27) * t29 + ((t27 - t19) * t17 + (-t15 
     #+ t24) * t21) * S34 + (-t38 + 0.9D1 * t39 + 0.9D1 / 0.2D1 * t41) *
     # t17 + (0.9D1 / 0.2D1 * t39 - t38 + 0.9D1 * t41) * t21) * t50) * s
     # * z + 0.9D1 * t55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29
     # - 0.18D2 * t29 * S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard71J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t9 = 0.27D2 * S14
      t12 = 0.27D2 * S24
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S24
      t27 = 0.9D1 / 0.2D1 * S14
      t29 = S34 ** 2
      t38 = 0.9D1 / 0.2D1 * S14 * S24
      t39 = S14 ** 2
      t41 = S24 ** 2
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2gghhard71J4 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 +
     # 0.18D2 * S23 + 0.18D2 * S24) * S12 + (0.27D2 * S13 + t9 + 0.54D2 
     #* S34 + 0.27D2 * S23 + t12) * S34 + (t15 - t12) * t17 + (-t9 + t19
     #) * t21 + ((0.9D1 / 0.2D1 * S23 + t24 + 0.9D1 * S34 + 0.9D1 / 0.2D
     #1 * S13 + t27) * t29 + ((t27 - t19) * t17 + (-t15 + t24) * t21) * 
     #S34 + (-t38 + 0.9D1 * t39 + 0.9D1 / 0.2D1 * t41) * t17 + (0.9D1 / 
     #0.2D1 * t39 - t38 + 0.9D1 * t41) * t21) * t50) * s * z + 0.9D1 * t
     #55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29 - 0.18D2 * t29 
     #* S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard71J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t9 = 0.81D2 / 0.2D1 * S14
      t12 = 0.81D2 / 0.2D1 * S24
      t15 = 0.9D1 * S14
      t17 = S13 + S14 + S34
      t19 = 0.9D1 * S24
      t21 = S23 + S24 + S34
      t24 = 0.9D1 / 0.2D1 * S24
      t27 = 0.9D1 / 0.2D1 * S14
      t29 = S34 ** 2
      t38 = 0.9D1 / 0.2D1 * S14 * S24
      t39 = S14 ** 2
      t41 = S24 ** 2
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2gghhard71J5 = (((0.18D2 * S13 + 0.18D2 * S14 + 0.36D2 * S34 +
     # 0.18D2 * S23 + 0.18D2 * S24) * S12 + (0.81D2 / 0.2D1 * S13 + t9 +
     # 0.81D2 * S34 + 0.81D2 / 0.2D1 * S23 + t12) * S34 + (t15 - t12) * 
     #t17 + (-t9 + t19) * t21 + ((0.9D1 / 0.2D1 * S23 + t24 + 0.9D1 * S3
     #4 + 0.9D1 / 0.2D1 * S13 + t27) * t29 + ((t27 - t19) * t17 + (-t15 
     #+ t24) * t21) * S34 + (-t38 + 0.9D1 * t39 + 0.9D1 / 0.2D1 * t41) *
     # t17 + (0.9D1 / 0.2D1 * t39 - t38 + 0.9D1 * t41) * t21) * t50) * s
     # * z + 0.9D1 * t55 * S12 - 0.18D2 * t55 * S34 + 0.27D2 * S12 * t29
     # - 0.18D2 * t29 * S34 + 0.9D1 * t64 * t50) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard71J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t9 = 0.135D3 * S14
      t12 = 0.135D3 * S24
      t15 = 0.45D2 * S14
      t17 = S13 + S14 + S34
      t19 = 0.45D2 * S24
      t21 = S23 + S24 + S34
      t24 = 0.45D2 / 0.2D1 * S24
      t27 = 0.45D2 / 0.2D1 * S14
      t29 = S34 ** 2
      t38 = 0.45D2 / 0.2D1 * S14 * S24
      t39 = S14 ** 2
      t41 = S24 ** 2
      t50 = 0.1D1 / S12
      t55 = S12 ** 2
      t64 = t29 ** 2
      rrgg2gghhard71J6 = (((-0.90D2 * S13 - 0.90D2 * S14 - 0.180D3 * S34
     # - 0.90D2 * S23 - 0.90D2 * S24) * S12 + (0.135D3 * S13 + t9 + 0.27
     #0D3 * S34 + 0.135D3 * S23 + t12) * S34 + (-t15 - t12) * t17 + (-t9
     # - t19) * t21 + ((-0.45D2 / 0.2D1 * S23 - t24 - 0.45D2 * S34 - 0.4
     #5D2 / 0.2D1 * S13 - t27) * t29 + ((-t27 + t19) * t17 + (t15 - t24)
     # * t21) * S34 + (t38 - 0.45D2 * t39 - 0.45D2 / 0.2D1 * t41) * t17 
     #+ (-0.45D2 / 0.2D1 * t39 + t38 - 0.45D2 * t41) * t21) * t50) * s *
     # z - 0.45D2 * t55 * S12 + 0.90D2 * t55 * S34 - 0.135D3 * S12 * t29
     # + 0.90D2 * t29 * S34 - 0.45D2 * t64 * t50) / pi * wd / z

      end function
  
   
      subroutine rrgg2gghsoftt7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt7s1e1  
      doubleprecision rrgg2gghsoftt7s1e0  
      doubleprecision rrgg2gghsoftt7s1em1  
      doubleprecision rrgg2gghsoftt7s1em2  
      doubleprecision rrgg2gghsoftt7s1em3  
      doubleprecision rrgg2gghsoftt7s1em4  
      doubleprecision rrgg2gghsoftt7s2e1  
      doubleprecision rrgg2gghsoftt7s2e0  
      doubleprecision rrgg2gghsoftt7s2em1  
      doubleprecision rrgg2gghsoftt7s2em2  
      doubleprecision rrgg2gghsoftt7s2em3  
      doubleprecision rrgg2gghsoftt7s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt7s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t2 = x3 * t1
      t3 = x4 * pi
      t4 = Sin(t3)
      t5 = t4 ** 2
      t6 = t2 * t5
      t8 = log(0.4D1 * t6)
      t9 = t8 ** 2
      t10 = x3 * t5
      t12 = (-0.1D1 + x1) ** 2
      t14 = -0.1D1 + x3
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t10 * t1 * t12 * t15)
      t20 = t19 ** 2
      t21 = cos(t3)
      t23 = Sqrt(-x3 * t14)
      t27 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t21 * t23)
      t32 = log(-0.4D1 * t10 * t1 * t15)
      t33 = t32 ** 2
      t35 = t5 * t12
      t36 = t2 * t35
      t38 = log(0.4D1 * t36)
      t39 = t38 ** 2
      t45 = 0.180D3 * lh
      t47 = (0.90D2 + t45) * wd
      t48 = -0.180D3 * wd + t47
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x1
      t59 = x2 ** 2
      t60 = t1 * t59
      t61 = -0.1D1 + x2
      t65 = log(-0.4D1 * t10 * t60 * t61)
      t66 = x2 * x3
      t68 = (0.1D1 - x3 + t66) ** 2
      t69 = 0.1D1 / t68
      t72 = t59 * t69 * t61 * t14
      t75 = log(0.4D1 * t6 * t72)
      t78 = Sqrt(x3 * t61 * t14)
      t82 = 0.1D1 / (-0.1D1 - x3 + t66 + 0.2D1 * t21 * t78)
      t84 = x3 * t59
      t85 = t1 * t5
      t88 = log(0.4D1 * t84 * t85)
      t92 = log(-0.4D1 * t10 * t60 * t15)
      t94 = t12 * t59
      t95 = t94 * t61
      t98 = log(-0.4D1 * t6 * t95)
      t102 = log(-0.4D1 * t6 * t94 * t15)
      t106 = log(0.4D1 * t36 * t72)
      t108 = t85 * t12
      t111 = log(0.4D1 * t84 * t108)
      t115 = 0.1D1 / x2
      t121 = log(0.4D1 * t60 * t35)
      t122 = t121 ** 2
      t125 = log(0.4D1 * t60 * t5)
      t126 = t125 ** 2
      t127 = t5 * t61
      t130 = log(-0.4D1 * t60 * t127)
      t131 = t130 ** 2
      t134 = log(-0.4D1 * t85 * t95)
      t135 = t134 ** 2
      t147 = log(0.4D1 * t85)
      t148 = t147 ** 2
      t150 = log(0.4D1 * t108)
      t151 = t150 ** 2
      t163 = lh ** 2
      t164 = 0.180D3 * t163
      t165 = pi ** 2
      t166 = 0.30D2 * t165
      t168 = (-t45 - t164 + t166) * wd
      t169 = -0.270D3 * wd + 0.2D1 * t47 + t168
      t176 = log(0.4D1 * t10)
      t179 = log(-0.4D1 * t10 * t15)
      t183 = t179 ** 2
      t186 = t176 ** 2
      t196 = 0.60D2 * lh * t165
      t197 = 0.240D3 * zeta3
      t199 = 0.120D3 * t163 * lh
      t214 = log(-0.4D1 * t84 * t127)
      t215 = t214 ** 2
      t219 = log(-0.4D1 * t10 * t59 * t15)
      t220 = t219 ** 2
      t222 = t84 * t5
      t224 = log(0.4D1 * t222)
      t225 = t224 ** 2
      t230 = log(0.4D1 * t222 * t69 * t61 * t14)
      t231 = t230 ** 2
      t247 = t59 * t5
      t249 = log(0.4D1 * t247)
      t250 = t249 ** 2
      t253 = log(-0.4D1 * t247 * t61)
      t254 = t253 ** 2
      t271 = log(0.4D1 * t5)
      t272 = 0.90D2 * t271
      t277 = 0.180D3 * t271 * lh
      t278 = t271 ** 2
      t279 = 0.45D2 * t278
      t284 = 0.90D2 * t278 * lh
      t285 = t278 * t271
      t286 = 0.15D2 * t285
      t287 = -t164 + t166
      t288 = t271 * t287
      t300 = t165 ** 2
      t301 = t163 ** 2
      t305 = t278 ** 2
      t307 = -t284 + t196 - t197 - t199 - t286 + t288 - 0.30D2 * t285 * 
     #lh + t278 * t287 / 0.2D1 - t271 * (-t196 + t197 + t199) - 0.480D3 
     #* lh * zeta3 - t300 - 0.60D2 * t301 + 0.60D2 * t163 * t165 - 0.15D
     #2 / 0.4D1 * t305
      t310 = -(-0.90D2 * wd * (-t9 / 0.2D1 + t20 * t27 / 0.2D1 - t33 * t
     #27 / 0.2D1 + t39 / 0.2D1) + t48 * (t8 - t19 * t27 + t32 * t27 - t3
     #8)) * t54 * t56 / 0.40D2 - 0.9D1 / 0.2D1 * wd * (t65 + t75 * t82 -
     # t88 - t92 * t27 - t98 + t102 * t27 - t106 * t82 + t111) * t54 * t
     #56 * t115 + (-0.90D2 * wd * (-t122 / 0.2D1 + t126 / 0.2D1 - t131 /
     # 0.2D1 + t135 / 0.2D1) + t48 * (t121 - t125 + t130 - t134)) * t56 
     #* t115 / 0.20D2 - (t48 * (-t148 / 0.2D1 + t151 / 0.2D1) - 0.90D2 *
     # wd * (-t151 * t150 / 0.6D1 + t148 * t147 / 0.6D1) + t169 * (t147 
     #- t150)) * t56 / 0.40D2 - (t169 * (t176 + t179 * t27) - 0.90D2 * w
     #d * (t183 * t179 * t27 / 0.6D1 + t186 * t176 / 0.6D1) + (-0.360D3 
     #* wd + 0.3D1 * t47 + 0.2D1 * t168 + (t164 - t166 - t196 + t197 + t
     #199) * wd) * (-0.1D1 - t27) + t48 * (-t186 / 0.2D1 - t183 * t27 / 
     #0.2D1)) * t54 / 0.80D2 - (-0.90D2 * wd * (t215 / 0.2D1 - t220 * t2
     #7 / 0.2D1 - t225 / 0.2D1 + t231 * t82 / 0.2D1) + t48 * (-t214 + t2
     #19 * t27 + t224 - t230 * t82) + t169 * (t82 - t27)) * t54 * t115 /
     # 0.40D2 + (t48 * (t250 / 0.2D1 - t254 / 0.2D1) - 0.90D2 * wd * (t2
     #54 * t253 / 0.6D1 - t250 * t249 / 0.6D1) + t169 * (-t249 + t253)) 
     #* t115 / 0.40D2 - 0.45D2 / 0.8D1 * wd + (0.90D2 + t45 + t272) * wd
     # / 0.20D2 + 0.3D1 / 0.80D2 * (-t45 - t272 - t277 - t279 - t164 + t
     #166) * wd + (t277 + t279 + t164 - t166 + t284 - t196 + t197 + t199
     # + t286 - t288) * wd / 0.40D2 + t307 * wd / 0.80D2
      t311 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t310)
      rrgg2gghsoftt7s1e1 = t311 * t310

      end function



      doubleprecision function rrgg2gghsoftt7s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t2 = x4 * pi
      t3 = Sin(t2)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = log(0.4D1 * t5)
      t8 = t7 ** 2
      t10 = (-0.1D1 + x1) ** 2
      t13 = log(0.4D1 * t5 * t10)
      t14 = t13 ** 2
      t20 = 0.180D3 * lh
      t22 = (0.90D2 + t20) * wd
      t23 = -0.180D3 * wd + t22
      t27 = 0.1D1 / x1
      t30 = x3 * t1
      t33 = log(0.4D1 * t30 * t4)
      t34 = x3 * t4
      t36 = -0.1D1 + x3
      t37 = 0.1D1 / t36
      t41 = log(-0.4D1 * t34 * t1 * t10 * t37)
      t42 = cos(t2)
      t44 = Sqrt(-x3 * t36)
      t48 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t42 * t44)
      t53 = log(-0.4D1 * t34 * t1 * t37)
      t55 = t4 * t10
      t58 = log(0.4D1 * t30 * t55)
      t61 = 0.1D1 / x3
      t65 = x2 ** 2
      t66 = t65 * t1
      t69 = log(0.4D1 * t66 * t55)
      t72 = log(0.4D1 * t66 * t4)
      t73 = -0.1D1 + x2
      t74 = t4 * t73
      t77 = log(-0.4D1 * t66 * t74)
      t82 = log(-0.4D1 * t5 * t10 * t65 * t73)
      t85 = 0.1D1 / x2
      t90 = log(0.4D1 * t34)
      t91 = t90 ** 2
      t94 = log(-0.4D1 * t34 * t37)
      t95 = t94 ** 2
      t106 = lh ** 2
      t107 = 0.180D3 * t106
      t108 = pi ** 2
      t109 = 0.30D2 * t108
      t120 = log(0.4D1 * t4)
      t121 = 0.90D2 * t120
      t126 = 0.180D3 * t120 * lh
      t127 = t120 ** 2
      t128 = 0.45D2 * t127
      t146 = x3 * t65
      t149 = log(-0.4D1 * t146 * t74)
      t153 = log(-0.4D1 * t34 * t65 * t37)
      t155 = t146 * t4
      t157 = log(0.4D1 * t155)
      t158 = x2 * x3
      t160 = (0.1D1 - x3 + t158) ** 2
      t166 = log(0.4D1 * t155 / t160 * t73 * t36)
      t169 = Sqrt(x3 * t73 * t36)
      t173 = 0.1D1 / (-0.1D1 - x3 + t158 + 0.2D1 * t42 * t169)
      t184 = t65 * t4
      t186 = log(0.4D1 * t184)
      t187 = t186 ** 2
      t190 = log(-0.4D1 * t184 * t73)
      t191 = t190 ** 2
      t201 = -(-0.90D2 * wd * (-t8 / 0.2D1 + t14 / 0.2D1) + t23 * (t7 - 
     #t13)) * t27 / 0.40D2 + 0.9D1 / 0.4D1 * wd * (t33 - t41 * t48 + t53
     # * t48 - t58) * t61 * t27 - 0.9D1 / 0.2D1 * wd * (t69 - t72 + t77 
     #- t82) * t27 * t85 - (-0.90D2 * wd * (-t91 / 0.2D1 - t95 * t48 / 0
     #.2D1) + t23 * (t90 + t94 * t48) + (-0.270D3 * wd + 0.2D1 * t22 + (
     #-t20 - t107 + t109) * wd) * (-0.1D1 - t48)) * t61 / 0.80D2 - 0.9D1
     # / 0.2D1 * wd + 0.3D1 / 0.80D2 * (0.90D2 + t20 + t121) * wd + (-t2
     #0 - t121 - t126 - t128 - t107 + t109) * wd / 0.40D2 + (t126 + t128
     # + t107 - t109 + 0.90D2 * t127 * lh - 0.60D2 * lh * t108 + 0.240D3
     # * zeta3 + 0.120D3 * t106 * lh + 0.15D2 * t127 * t120 - t120 * (-t
     #107 + t109)) * wd / 0.80D2 - (-0.90D2 * wd * (-t149 + t153 * t48 +
     # t157 - t166 * t173) + t23 * (t173 - t48)) * t61 * t85 / 0.40D2 + 
     #(-0.90D2 * wd * (t187 / 0.2D1 - t191 / 0.2D1) + t23 * (-t186 + t19
     #0)) * t85 / 0.40D2
      t202 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t201)
      rrgg2gghsoftt7s1e0 = t202 * t201

      end function



      doubleprecision function rrgg2gghsoftt7s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t4 = x3 * t3
      t6 = log(0.4D1 * t4)
      t7 = -0.1D1 + x3
      t11 = log(-0.4D1 * t4 / t7)
      t12 = cos(t1)
      t14 = Sqrt(-x3 * t7)
      t18 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t12 * t14)
      t24 = 0.180D3 * lh
      t31 = 0.1D1 / x3
      t34 = x1 ** 2
      t35 = t34 * t3
      t37 = log(0.4D1 * t35)
      t39 = (-0.1D1 + x1) ** 2
      t42 = log(0.4D1 * t35 * t39)
      t49 = -0.1D1 + x2
      t52 = Sqrt(x3 * t49 * t7)
      t59 = 0.1D1 / x2
      t63 = x2 ** 2
      t64 = t63 * t3
      t66 = log(0.4D1 * t64)
      t69 = log(-0.4D1 * t64 * t49)
      t76 = log(0.4D1 * t3)
      t77 = 0.90D2 * t76
      t83 = t76 ** 2
      t85 = lh ** 2
      t87 = pi ** 2
      t92 = -(-0.90D2 * wd * (t6 + t11 * t18) + (-0.180D3 * wd + (0.90D2
     # + t24) * wd) * (-0.1D1 - t18)) * t31 / 0.80D2 + 0.9D1 / 0.4D1 * w
     #d * (t37 - t42) / x1 + 0.9D1 / 0.4D1 * wd * (0.1D1 / (-0.1D1 - x3 
     #+ x2 * x3 + 0.2D1 * t12 * t52) - t18) * t31 * t59 - 0.9D1 / 0.4D1 
     #* wd * (-t66 + t69) * t59 - 0.27D2 / 0.8D1 * wd + (0.90D2 + t24 + 
     #t77) * wd / 0.40D2 + (-t24 - t77 - 0.180D3 * t76 * lh - 0.45D2 * t
     #83 - 0.180D3 * t85 + 0.30D2 * t87) * wd / 0.80D2
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t92)
      rrgg2gghsoftt7s1em1 = t93 * t92

      end function



      doubleprecision function rrgg2gghsoftt7s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = cos(t1)
      t5 = Sqrt(-x3 * (-0.1D1 + x3))
      t17 = Sin(t1)
      t18 = t17 ** 2
      t20 = log(0.4D1 * t18)
      t25 = 0.9D1 / 0.8D1 * wd * (-0.1D1 - 0.1D1 / (-0.1D1 - x3 + 0.2D1 
     #* t2 * t5)) / x3 - 0.9D1 / 0.4D1 * wd + (0.90D2 + 0.180D3 * lh + 0
     #.90D2 * t20) * wd / 0.80D2
      t26 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t25)
      rrgg2gghsoftt7s1em2 = t26 * t25

      end function



      doubleprecision function rrgg2gghsoftt7s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.8D1 * wd)
      rrgg2gghsoftt7s1em3 = -0.9D1 / 0.8D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt7s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt7s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t4 = x3 * t3
      t5 = x1 ** 2
      t7 = (-0.1D1 + x1) ** 2
      t9 = -0.1D1 + x3
      t10 = 0.1D1 / t9
      t14 = log(-0.4D1 * t4 * t5 * t7 * t10)
      t15 = t14 ** 2
      t16 = cos(t1)
      t18 = Sqrt(-x3 * t9)
      t22 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t16 * t18)
      t27 = log(-0.4D1 * t4 * t5 * t10)
      t28 = t27 ** 2
      t30 = x3 * t5
      t31 = t3 * t7
      t32 = t30 * t31
      t34 = log(0.4D1 * t32)
      t35 = t34 ** 2
      t36 = t30 * t3
      t38 = log(0.4D1 * t36)
      t39 = t38 ** 2
      t45 = 0.180D3 * lh
      t47 = (0.90D2 + t45) * wd
      t48 = -0.180D3 * wd + t47
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x1
      t59 = x2 ** 2
      t60 = t7 * t59
      t64 = log(-0.4D1 * t36 * t60 * t10)
      t66 = t59 * t5
      t70 = log(-0.4D1 * t4 * t66 * t10)
      t72 = x2 * x3
      t74 = (0.1D1 - x3 + t72) ** 2
      t75 = 0.1D1 / t74
      t77 = -0.1D1 + x2
      t79 = t59 * t75 * t77 * t9
      t82 = log(0.4D1 * t36 * t79)
      t85 = Sqrt(x3 * t77 * t9)
      t89 = 0.1D1 / (-0.1D1 - x3 + t72 + 0.2D1 * t16 * t85)
      t91 = t60 * t77
      t94 = log(-0.4D1 * t36 * t91)
      t97 = log(0.4D1 * t32 * t79)
      t102 = log(-0.4D1 * t4 * t66 * t77)
      t103 = x3 * t59
      t104 = t5 * t3
      t107 = log(0.4D1 * t103 * t104)
      t108 = t104 * t7
      t111 = log(0.4D1 * t103 * t108)
      t115 = 0.1D1 / x2
      t121 = log(-0.4D1 * t104 * t91)
      t122 = t121 ** 2
      t123 = t3 * t77
      t126 = log(-0.4D1 * t66 * t123)
      t127 = t126 ** 2
      t130 = log(0.4D1 * t66 * t31)
      t131 = t130 ** 2
      t134 = log(0.4D1 * t66 * t3)
      t135 = t134 ** 2
      t147 = log(0.4D1 * t104)
      t148 = t147 ** 2
      t150 = log(0.4D1 * t108)
      t151 = t150 ** 2
      t163 = lh ** 2
      t164 = 0.180D3 * t163
      t165 = pi ** 2
      t166 = 0.30D2 * t165
      t168 = (-t45 - t164 + t166) * wd
      t169 = -0.270D3 * wd + 0.2D1 * t47 + t168
      t176 = log(0.4D1 * t4)
      t179 = log(-0.4D1 * t4 * t10)
      t183 = t179 ** 2
      t186 = t176 ** 2
      t196 = 0.60D2 * lh * t165
      t197 = 0.240D3 * zeta3
      t199 = 0.120D3 * t163 * lh
      t212 = t103 * t3
      t214 = log(0.4D1 * t212)
      t215 = t214 ** 2
      t219 = log(-0.4D1 * t4 * t59 * t10)
      t220 = t219 ** 2
      t224 = log(-0.4D1 * t103 * t123)
      t225 = t224 ** 2
      t230 = log(0.4D1 * t212 * t75 * t77 * t9)
      t231 = t230 ** 2
      t247 = t59 * t3
      t249 = log(0.4D1 * t247)
      t250 = t249 ** 2
      t253 = log(-0.4D1 * t247 * t77)
      t254 = t253 ** 2
      t271 = log(0.4D1 * t3)
      t272 = 0.90D2 * t271
      t277 = 0.180D3 * t271 * lh
      t278 = t271 ** 2
      t279 = 0.45D2 * t278
      t284 = 0.90D2 * t278 * lh
      t285 = t278 * t271
      t286 = 0.15D2 * t285
      t287 = -t164 + t166
      t288 = t271 * t287
      t300 = t165 ** 2
      t301 = t163 ** 2
      t305 = t278 ** 2
      t307 = -t284 + t196 - t197 - t199 - t286 + t288 - 0.30D2 * t285 * 
     #lh + t278 * t287 / 0.2D1 - t271 * (-t196 + t197 + t199) - 0.480D3 
     #* lh * zeta3 - t300 - 0.60D2 * t301 + 0.60D2 * t163 * t165 - 0.15D
     #2 / 0.4D1 * t305
      t310 = (-0.90D2 * wd * (-t15 * t22 / 0.2D1 + t28 * t22 / 0.2D1 - t
     #35 / 0.2D1 + t39 / 0.2D1) + t48 * (t14 * t22 - t27 * t22 + t34 - t
     #38)) * t54 * t56 / 0.40D2 + 0.9D1 / 0.2D1 * wd * (-t64 * t22 + t70
     # * t22 - t82 * t89 + t94 + t97 * t89 - t102 + t107 - t111) * t54 *
     # t56 * t115 - (-0.90D2 * wd * (-t122 / 0.2D1 + t127 / 0.2D1 + t131
     # / 0.2D1 - t135 / 0.2D1) + t48 * (t121 - t126 - t130 + t134)) * t5
     #6 * t115 / 0.20D2 - (t48 * (-t148 / 0.2D1 + t151 / 0.2D1) - 0.90D2
     # * wd * (-t151 * t150 / 0.6D1 + t148 * t147 / 0.6D1) + t169 * (t14
     #7 - t150)) * t56 / 0.40D2 + (t169 * (-t176 - t179 * t22) - 0.90D2 
     #* wd * (-t183 * t179 * t22 / 0.6D1 - t186 * t176 / 0.6D1) + (-0.36
     #0D3 * wd + 0.3D1 * t47 + 0.2D1 * t168 + (t164 - t166 - t196 + t197
     # + t199) * wd) * (0.1D1 + t22) + t48 * (t186 / 0.2D1 + t183 * t22 
     #/ 0.2D1)) * t54 / 0.80D2 + (-0.90D2 * wd * (t215 / 0.2D1 + t220 * 
     #t22 / 0.2D1 - t225 / 0.2D1 - t231 * t89 / 0.2D1) + t48 * (-t214 - 
     #t219 * t22 + t224 + t230 * t89) + t169 * (-t89 + t22)) * t54 * t11
     #5 / 0.40D2 - (t48 * (-t250 / 0.2D1 + t254 / 0.2D1) - 0.90D2 * wd *
     # (-t254 * t253 / 0.6D1 + t250 * t249 / 0.6D1) + t169 * (t249 - t25
     #3)) * t115 / 0.40D2 - 0.45D2 / 0.8D1 * wd + (0.90D2 + t45 + t272) 
     #* wd / 0.20D2 + 0.3D1 / 0.80D2 * (-t45 - t272 - t277 - t279 - t164
     # + t166) * wd + (t277 + t279 + t164 - t166 + t284 - t196 + t197 + 
     #t199 + t286 - t288) * wd / 0.40D2 + t307 * wd / 0.80D2
      t311 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t310)
      rrgg2gghsoftt7s2e1 = t311 * t310

      end function



      doubleprecision function rrgg2gghsoftt7s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t2 = x4 * pi
      t3 = Sin(t2)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = log(0.4D1 * t5)
      t8 = t7 ** 2
      t10 = (-0.1D1 + x1) ** 2
      t13 = log(0.4D1 * t5 * t10)
      t14 = t13 ** 2
      t20 = 0.180D3 * lh
      t22 = (0.90D2 + t20) * wd
      t23 = -0.180D3 * wd + t22
      t27 = 0.1D1 / x1
      t30 = x3 * t4
      t32 = -0.1D1 + x3
      t33 = 0.1D1 / t32
      t37 = log(-0.4D1 * t30 * t1 * t10 * t33)
      t38 = cos(t2)
      t40 = Sqrt(-x3 * t32)
      t44 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t38 * t40)
      t49 = log(-0.4D1 * t30 * t1 * t33)
      t51 = x3 * t1
      t52 = t4 * t10
      t55 = log(0.4D1 * t51 * t52)
      t58 = log(0.4D1 * t51 * t4)
      t61 = 0.1D1 / x3
      t65 = x2 ** 2
      t67 = -0.1D1 + x2
      t71 = log(-0.4D1 * t5 * t10 * t65 * t67)
      t72 = t65 * t1
      t73 = t4 * t67
      t76 = log(-0.4D1 * t72 * t73)
      t79 = log(0.4D1 * t72 * t52)
      t82 = log(0.4D1 * t72 * t4)
      t85 = 0.1D1 / x2
      t90 = log(0.4D1 * t30)
      t91 = t90 ** 2
      t94 = log(-0.4D1 * t30 * t33)
      t95 = t94 ** 2
      t106 = lh ** 2
      t107 = 0.180D3 * t106
      t108 = pi ** 2
      t109 = 0.30D2 * t108
      t120 = log(0.4D1 * t4)
      t121 = 0.90D2 * t120
      t126 = 0.180D3 * t120 * lh
      t127 = t120 ** 2
      t128 = 0.45D2 * t127
      t146 = x3 * t65
      t147 = t146 * t4
      t149 = log(0.4D1 * t147)
      t153 = log(-0.4D1 * t30 * t65 * t33)
      t157 = log(-0.4D1 * t146 * t73)
      t158 = x2 * x3
      t160 = (0.1D1 - x3 + t158) ** 2
      t166 = log(0.4D1 * t147 / t160 * t67 * t32)
      t169 = Sqrt(x3 * t67 * t32)
      t173 = 0.1D1 / (-0.1D1 - x3 + t158 + 0.2D1 * t38 * t169)
      t184 = t65 * t4
      t186 = log(0.4D1 * t184)
      t187 = t186 ** 2
      t190 = log(-0.4D1 * t184 * t67)
      t191 = t190 ** 2
      t201 = -(-0.90D2 * wd * (-t8 / 0.2D1 + t14 / 0.2D1) + t23 * (t7 - 
     #t13)) * t27 / 0.40D2 - 0.9D1 / 0.4D1 * wd * (t37 * t44 - t49 * t44
     # + t55 - t58) * t61 * t27 + 0.9D1 / 0.2D1 * wd * (t71 - t76 - t79 
     #+ t82) * t27 * t85 + (-0.90D2 * wd * (t91 / 0.2D1 + t95 * t44 / 0.
     #2D1) + t23 * (-t90 - t94 * t44) + (-0.270D3 * wd + 0.2D1 * t22 + (
     #-t20 - t107 + t109) * wd) * (0.1D1 + t44)) * t61 / 0.80D2 - 0.9D1 
     #/ 0.2D1 * wd + 0.3D1 / 0.80D2 * (0.90D2 + t20 + t121) * wd + (-t20
     # - t121 - t126 - t128 - t107 + t109) * wd / 0.40D2 + (t126 + t128 
     #+ t107 - t109 + 0.90D2 * t127 * lh - 0.60D2 * lh * t108 + 0.240D3 
     #* zeta3 + 0.120D3 * t106 * lh + 0.15D2 * t127 * t120 - t120 * (-t1
     #07 + t109)) * wd / 0.80D2 + (-0.90D2 * wd * (-t149 - t153 * t44 + 
     #t157 + t166 * t173) + t23 * (-t173 + t44)) * t61 * t85 / 0.40D2 - 
     #(-0.90D2 * wd * (-t187 / 0.2D1 + t191 / 0.2D1) + t23 * (t186 - t19
     #0)) * t85 / 0.40D2
      t202 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t201)
      rrgg2gghsoftt7s2e0 = t202 * t201

      end function



      doubleprecision function rrgg2gghsoftt7s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t4 = x3 * t3
      t6 = log(0.4D1 * t4)
      t7 = -0.1D1 + x3
      t11 = log(-0.4D1 * t4 / t7)
      t12 = cos(t1)
      t14 = Sqrt(-x3 * t7)
      t18 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t12 * t14)
      t24 = 0.180D3 * lh
      t31 = 0.1D1 / x3
      t34 = x1 ** 2
      t35 = t34 * t3
      t37 = log(0.4D1 * t35)
      t39 = (-0.1D1 + x1) ** 2
      t42 = log(0.4D1 * t35 * t39)
      t49 = -0.1D1 + x2
      t52 = Sqrt(x3 * t49 * t7)
      t59 = 0.1D1 / x2
      t63 = x2 ** 2
      t64 = t63 * t3
      t66 = log(0.4D1 * t64)
      t69 = log(-0.4D1 * t64 * t49)
      t76 = log(0.4D1 * t3)
      t77 = 0.90D2 * t76
      t83 = t76 ** 2
      t85 = lh ** 2
      t87 = pi ** 2
      t92 = (-0.90D2 * wd * (-t6 - t11 * t18) + (-0.180D3 * wd + (0.90D2
     # + t24) * wd) * (0.1D1 + t18)) * t31 / 0.80D2 + 0.9D1 / 0.4D1 * wd
     # * (t37 - t42) / x1 - 0.9D1 / 0.4D1 * wd * (-0.1D1 / (-0.1D1 - x3 
     #+ x2 * x3 + 0.2D1 * t12 * t52) + t18) * t31 * t59 + 0.9D1 / 0.4D1 
     #* wd * (t66 - t69) * t59 - 0.27D2 / 0.8D1 * wd + (0.90D2 + t24 + t
     #77) * wd / 0.40D2 + (-t24 - t77 - 0.180D3 * t76 * lh - 0.45D2 * t8
     #3 - 0.180D3 * t85 + 0.30D2 * t87) * wd / 0.80D2
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t92)
      rrgg2gghsoftt7s2em1 = t93 * t92

      end function



      doubleprecision function rrgg2gghsoftt7s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x4 * pi
      t2 = cos(t1)
      t5 = Sqrt(-x3 * (-0.1D1 + x3))
      t17 = Sin(t1)
      t18 = t17 ** 2
      t20 = log(0.4D1 * t18)
      t25 = -0.9D1 / 0.8D1 * wd * (0.1D1 + 0.1D1 / (-0.1D1 - x3 + 0.2D1 
     #* t2 * t5)) / x3 - 0.9D1 / 0.4D1 * wd + (0.90D2 + 0.180D3 * lh + 0
     #.90D2 * t20) * wd / 0.80D2
      t26 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t25)
      rrgg2gghsoftt7s2em2 = t26 * t25

      end function



      doubleprecision function rrgg2gghsoftt7s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.8D1 * wd)
      rrgg2gghsoftt7s2em3 = -0.9D1 / 0.8D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt7s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt7s2em4 = 0.0D0

      end function
