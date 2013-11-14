  
      subroutine rrqg2qght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh61J1  
      doubleprecision rrqg2qgh61J2  
      doubleprecision rrqg2qgh61J3  
      doubleprecision rrqg2qgh61J4  
      doubleprecision rrqg2qgh61J5  
      doubleprecision rrqg2qgh61J6  
      doubleprecision rrqg2qgh62J1  
      doubleprecision rrqg2qgh62J2  
      doubleprecision rrqg2qgh62J3  
      doubleprecision rrqg2qgh62J4  
      doubleprecision rrqg2qgh62J5  
      doubleprecision rrqg2qgh62J6  
      doubleprecision rrqg2qgh62J7  
      doubleprecision rrqg2qgh63J1  
      doubleprecision rrqg2qgh63J2  
      doubleprecision rrqg2qgh63J3  
      doubleprecision rrqg2qgh63J4  
      doubleprecision rrqg2qgh63J5  
      doubleprecision rrqg2qgh63J6  
      doubleprecision rrqg2qgh64J1  
      doubleprecision rrqg2qgh64J2  
      doubleprecision rrqg2qgh64J3  
      doubleprecision rrqg2qgh64J4  
      doubleprecision rrqg2qgh64J5  
      doubleprecision rrqg2qgh64J6  
      doubleprecision rrqg2qgh64J7  
      doubleprecision rrqg2qght6s1e1  
      doubleprecision rrqg2qght6s1e0  
      doubleprecision rrqg2qght6s1em1  
      doubleprecision rrqg2qght6s1em2  
      doubleprecision rrqg2qght6s1em3  
      doubleprecision rrqg2qght6s1em4  
      doubleprecision rrqg2qght6s2e1  
      doubleprecision rrqg2qght6s2e0  
      doubleprecision rrqg2qght6s2em1  
      doubleprecision rrqg2qght6s2em2  
      doubleprecision rrqg2qght6s2em3  
      doubleprecision rrqg2qght6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght6s1e1
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = t3 * pi
      t7 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = t7 * pi
      t10 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t11 = t10 * pi
      t12 = lh ** 2
      t14 = pi ** 2
      t16 = 0.180D3 * t12 - 0.30D2 * t14
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = x4 * pi
      t25 = Sin(t24)
      t26 = t25 ** 2
      t27 = x3 * t26
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t35 = log(0.4D1 * t27 * t32)
      t36 = 0.1D1 / z
      t38 = -0.1D1 + x3
      t39 = 0.1D1 / t38
      t40 = t32 * t39
      t43 = log(-0.4D1 * t27 * t40)
      t44 = cos(t24)
      t46 = Sqrt(-x3 * t38)
      t51 = 0.1D1 / (-x3 - z + 0.2D1 * t44 * t46 * z)
      t54 = t23 * (-t35 * t36 - t43 * t51)
      t56 = t19 * t23
      t57 = t43 ** 2
      t60 = t35 ** 2
      t65 = t56 * (-t57 * t43 * t51 / 0.6D1 - t60 * t35 * t36 / 0.6D1)
      t75 = 0.60D2 * lh * t14 - 0.240D3 * zeta3 - 0.120D3 * t12 * lh
      t77 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t84 = t23 * (t36 + t51)
      t95 = t23 * (t60 * t36 / 0.2D1 + t57 * t51 / 0.2D1)
      t98 = 0.1D1 / x3
      t101 = t29 * t26
      t102 = t101 * t31
      t104 = log(0.4D1 * t102)
      t105 = t104 ** 2
      t106 = t105 * t36
      t109 = t36 * t77
      t111 = t105 * t104 * t36
      t114 = t104 * t36
      t120 = t36 * t7
      t127 = t36 * t3
      t132 = t36 * t10
      t133 = t14 ** 2
      t134 = t12 ** 2
      t141 = pi * (t133 + 0.60D2 * t134 + 0.480D3 * lh * zeta3 - 0.60D2 
     #* t12 * t14)
      t143 = rrqg2qgh64J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t150 = t105 ** 2
      t151 = t150 * t36
      t161 = pi * t19
      t162 = x1 ** 2
      t163 = x3 * t162
      t164 = t163 * t26
      t167 = log(-0.4D1 * t164 * t40)
      t169 = t167 ** 2
      t176 = log(0.4D1 * t163 * t102)
      t177 = t176 * t36
      t179 = t176 ** 2
      t180 = t179 * t36
      t187 = pi * lh
      t196 = pi * t16
      t199 = t56 * (t10 * t51 + t132)
      t200 = t196 * t199
      t203 = 0.1D1 / x1
      t206 = t36 * pi
      t207 = t206 * t16
      t208 = t162 * t26
      t211 = log(0.4D1 * t208 * t32)
      t216 = t211 ** 2
      t219 = t216 * t211
      t227 = t206 * t75
      t229 = t227 * t56 * t10
      t230 = t206 * lh
      t241 = x2 ** 2
      t242 = x3 * t241
      t243 = t242 * t162
      t246 = log(0.4D1 * t243 * t102)
      t247 = t246 * t36
      t253 = log(-0.4D1 * t243 * t101 * t31 * t39)
      t265 = 0.1D1 / x2
      t266 = t265 * t203
      t269 = t241 * t162
      t272 = log(0.4D1 * t269 * t102)
      t273 = t272 ** 2
      t274 = t273 * t36
      t277 = t272 * t36
      t288 = t196 * t19
      t289 = t23 * t36
      t296 = t242 * t26
      t299 = log(-0.4D1 * t296 * t40)
      t301 = t299 ** 2
      t308 = log(0.4D1 * t242 * t102)
      t309 = t308 ** 2
      t310 = t309 * t36
      t313 = t308 * t36
      t331 = t241 * t26
      t334 = log(0.4D1 * t331 * t32)
      t335 = t334 * t36
      t340 = t334 ** 2
      t341 = t340 * t36
      t345 = t340 * t334 * t36
      t363 = ((-0.180D3 * t4 * lh + 0.90D2 * t8 + t11 * t16) * t19 * t54
     # + 0.90D2 * t11 * t65 + (-0.180D3 * t8 * lh + t11 * t75 + 0.90D2 *
     # t77 * pi + t4 * t16) * t19 * t84 + (-0.180D3 * t11 * lh + 0.90D2 
     #* t4) * t19 * t95) * t98 / 0.2880D4 + (-0.180D3 * (t106 * t3 / 0.2
     #D1 + t109 - t111 * t10 / 0.6D1 - t114 * t7) * pi * lh + (t120 - t1
     #14 * t3 + t106 * t10 / 0.2D1) * pi * t16 + (t127 - t114 * t10) * p
     #i * t75 + t132 * t141 + 0.90D2 * (t36 * t143 - t111 * t3 / 0.6D1 +
     # t106 * t7 / 0.2D1 - t114 * t77 + t151 * t10 / 0.24D2) * pi) * t19
     # * t23 / 0.2880D4 + (0.90D2 * t161 * t23 * ((t7 - t167 * t3 + t169
     # * t10 / 0.2D1) * t51 + t120 - t177 * t3 + t180 * t10 / 0.2D1) - 0
     #.180D3 * t187 * t56 * ((t3 - t167 * t10) * t51 - t177 * t10 + t127
     #) + t200) * t98 * t203 / 0.1440D4 + (t207 * t56 * (-t211 * t10 + t
     #3) + 0.90D2 * t206 * t56 * (t216 * t3 / 0.2D1 + t77 - t219 * t10 /
     # 0.6D1 - t211 * t7) + t229 - 0.180D3 * t230 * t56 * (t7 + t216 * t
     #10 / 0.2D1 - t211 * t3)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 
     #* (-t247 * t10 + (t3 - t253 * t10) * t51 + t127) - 0.180D3 * t187 
     #* t199) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t120 + t2
     #74 * t10 / 0.2D1 - t277 * t3) - 0.180D3 * t187 * t56 * (-t277 * t1
     #0 + t127) + t288 * t289 * t10) * t265 * t203 / 0.720D3 + (0.90D2 *
     # t161 * t23 * (t120 + (t7 - t299 * t3 + t301 * t10 / 0.2D1) * t51 
     #+ t310 * t10 / 0.2D1 - t313 * t3) - 0.180D3 * t187 * t56 * ((t3 - 
     #t299 * t10) * t51 + t127 - t313 * t10) + t200) * t98 * t265 / 0.14
     #40D4 + (t196 * t56 * (t127 - t335 * t10) + 0.90D2 * t161 * t23 * (
     #t341 * t3 / 0.2D1 + t109 - t345 * t10 / 0.6D1 - t335 * t7) + t229 
     #- 0.180D3 * t187 * t56 * (t120 - t335 * t3 + t341 * t10 / 0.2D1)) 
     #* t265 / 0.1440D4
      t364 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t363)
      t366 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t367 = t366 * pi
      t370 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t371 = t370 * pi
      t373 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t374 = t373 * pi
      t384 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t402 = t36 * t384
      t410 = t36 * t370
      t417 = t36 * t366
      t422 = t36 * t373
      t424 = rrqg2qgh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t462 = t56 * (t373 * t51 + t422)
      t463 = t196 * t462
      t482 = t227 * t56 * t373
      t572 = ((-0.180D3 * t367 * lh + 0.90D2 * t371 + t374 * t16) * t19 
     #* t54 + 0.90D2 * t374 * t65 + (-0.180D3 * t371 * lh + t374 * t75 +
     # 0.90D2 * t384 * pi + t367 * t16) * t19 * t84 + (-0.180D3 * t374 *
     # lh + 0.90D2 * t367) * t19 * t95) * t98 / 0.2880D4 + (-0.180D3 * (
     #t106 * t366 / 0.2D1 + t402 - t111 * t373 / 0.6D1 - t114 * t370) * 
     #pi * lh + (t410 - t114 * t366 + t106 * t373 / 0.2D1) * pi * t16 + 
     #(t417 - t114 * t373) * pi * t75 + t422 * t141 + 0.90D2 * (t36 * t4
     #24 - t111 * t366 / 0.6D1 + t106 * t370 / 0.2D1 - t114 * t384 + t15
     #1 * t373 / 0.24D2) * pi) * t19 * t23 / 0.2880D4 + (0.90D2 * t161 *
     # t23 * ((t370 - t167 * t366 + t169 * t373 / 0.2D1) * t51 + t410 + 
     #t180 * t373 / 0.2D1 - t177 * t366) - 0.180D3 * t187 * t56 * ((t366
     # - t167 * t373) * t51 - t177 * t373 + t417) + t463) * t98 * t203 /
     # 0.1440D4 + (t207 * t56 * (t366 - t211 * t373) + 0.90D2 * t206 * t
     #56 * (t216 * t366 / 0.2D1 - t219 * t373 / 0.6D1 + t384 - t211 * t3
     #70) + t482 - 0.180D3 * t230 * t56 * (t370 - t211 * t366 + t216 * t
     #373 / 0.2D1)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 * ((t366 - 
     #t253 * t373) * t51 - t247 * t373 + t417) - 0.180D3 * t187 * t462) 
     #* t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t410 + t274 * t37
     #3 / 0.2D1 - t277 * t366) - 0.180D3 * t187 * t56 * (-t277 * t373 + 
     #t417) + t288 * t289 * t373) * t265 * t203 / 0.720D3 + (0.90D2 * t1
     #61 * t23 * (t310 * t373 / 0.2D1 - t313 * t366 + t410 + (t370 - t29
     #9 * t366 + t301 * t373 / 0.2D1) * t51) - 0.180D3 * t187 * t56 * (-
     #t313 * t373 + t417 + (t366 - t299 * t373) * t51) + t463) * t98 * t
     #265 / 0.1440D4 + (t196 * t56 * (t417 - t335 * t373) + 0.90D2 * t16
     #1 * t23 * (t341 * t366 / 0.2D1 + t402 - t335 * t370 - t345 * t373 
     #/ 0.6D1) + t482 - 0.180D3 * t187 * t56 * (t410 - t335 * t366 + t34
     #1 * t373 / 0.2D1)) * t265 / 0.1440D4
      t573 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t572)
      t575 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t576 = t575 * pi
      t579 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t580 = t579 * pi
      t582 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t583 = t582 * pi
      t593 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t611 = t36 * t593
      t619 = t36 * t579
      t626 = t36 * t575
      t631 = t36 * t582
      t633 = rrqg2qgh63J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t671 = t56 * (t582 * t51 + t631)
      t672 = t196 * t671
      t691 = t227 * t56 * t582
      t781 = ((-0.180D3 * t576 * lh + 0.90D2 * t580 + t583 * t16) * t19 
     #* t54 + 0.90D2 * t583 * t65 + (-0.180D3 * t580 * lh + t583 * t75 +
     # 0.90D2 * t593 * pi + t576 * t16) * t19 * t84 + (-0.180D3 * t583 *
     # lh + 0.90D2 * t576) * t19 * t95) * t98 / 0.2880D4 + (-0.180D3 * (
     #t106 * t575 / 0.2D1 + t611 - t111 * t582 / 0.6D1 - t114 * t579) * 
     #pi * lh + (t619 - t114 * t575 + t106 * t582 / 0.2D1) * pi * t16 + 
     #(t626 - t114 * t582) * pi * t75 + t631 * t141 + 0.90D2 * (t36 * t6
     #33 - t111 * t575 / 0.6D1 + t106 * t579 / 0.2D1 - t114 * t593 + t15
     #1 * t582 / 0.24D2) * pi) * t19 * t23 / 0.2880D4 + (0.90D2 * t161 *
     # t23 * ((-t167 * t575 + t579 + t169 * t582 / 0.2D1) * t51 + t619 -
     # t177 * t575 + t180 * t582 / 0.2D1) - 0.180D3 * t187 * t56 * ((t57
     #5 - t167 * t582) * t51 + t626 - t177 * t582) + t672) * t98 * t203 
     #/ 0.1440D4 + (t207 * t56 * (-t211 * t582 + t575) + 0.90D2 * t206 *
     # t56 * (t216 * t575 / 0.2D1 - t219 * t582 / 0.6D1 + t593 - t211 * 
     #t579) + t691 - 0.180D3 * t230 * t56 * (-t211 * t575 + t216 * t582 
     #/ 0.2D1 + t579)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 * (-t247
     # * t582 + (t575 - t253 * t582) * t51 + t626) - 0.180D3 * t187 * t6
     #71) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t619 - t277 *
     # t575 + t274 * t582 / 0.2D1) - 0.180D3 * t187 * t56 * (t626 - t277
     # * t582) + t288 * t289 * t582) * t265 * t203 / 0.720D3 + (0.90D2 *
     # t161 * t23 * ((-t299 * t575 + t579 + t301 * t582 / 0.2D1) * t51 -
     # t313 * t575 + t310 * t582 / 0.2D1 + t619) - 0.180D3 * t187 * t56 
     #* ((t575 - t299 * t582) * t51 + t626 - t313 * t582) + t672) * t98 
     #* t265 / 0.1440D4 + (t196 * t56 * (t626 - t335 * t582) + 0.90D2 * 
     #t161 * t23 * (t341 * t575 / 0.2D1 + t611 - t345 * t582 / 0.6D1 - t
     #335 * t579) + t691 - 0.180D3 * t187 * t56 * (t619 - t335 * t575 + 
     #t341 * t582 / 0.2D1)) * t265 / 0.1440D4
      t782 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t781)
      t784 = t2 * x1
      t785 = -0.1D1 + x1
      t786 = x1 * z
      t787 = 0.1D1 - x1 + t786
      t788 = 0.1D1 / t787
      t790 = t2 * t785 * t788
      t791 = s * t30
      t793 = t785 * x1 * t788
      t794 = t791 * t793
      t795 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, t
     #784, 0.0D0, -t794)
      t796 = t36 * t795
      t799 = t31 * t788
      t800 = t785 ** 2
      t805 = log(-0.4D1 * t163 * t101 * t799 * t800 * t39)
      t806 = t805 * t787
      t807 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, t
     #784, 0.0D0, -t794)
      t809 = t805 ** 2
      t810 = t809 * t787
      t811 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, t
     #784, 0.0D0, -t794)
      t815 = x3 * x1
      t816 = t815 * z
      t817 = 0.3D1 * t816
      t818 = x1 * t28
      t819 = x3 * t28
      t820 = t819 * x1
      t822 = 0.2D1 * t163 * z
      t823 = t163 * t28
      t824 = 0.2D1 * t815
      t825 = x3 * t787
      t827 = Sqrt(-t825 * t38)
      t831 = -z + t786 - t817 - t818 + t820 + t822 - t823 - t163 + t824 
     #+ 0.2D1 * t44 * t827 * z - x3
      t832 = 0.1D1 / t831
      t834 = t788 * t800
      t835 = t32 * t834
      t838 = log(0.4D1 * t164 * t835)
      t839 = t838 ** 2
      t840 = t839 * t36
      t843 = t838 * t36
      t849 = t787 * t807
      t853 = t36 * t807
      t863 = t56 * (-t36 * t811 - t787 * t811 * t832)
      t869 = t208 * t29
      t870 = t799 * t800
      t873 = log(0.4D1 * t869 * t870)
      t878 = t873 ** 2
      t881 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, t
     #784, 0.0D0, -t794)
      t882 = t878 * t873
      t902 = t242 * t208
      t907 = log(-0.4D1 * t902 * t32 * t834 * t39)
      t908 = t907 * t787
      t914 = log(0.4D1 * t902 * t835)
      t915 = t914 * t36
      t927 = t269 * t26
      t930 = log(0.4D1 * t927 * t835)
      t931 = t930 * t36
      t933 = t930 ** 2
      t934 = t933 * t36
      t952 = (0.90D2 * t161 * t23 * (-t796 - (t787 * t795 - t806 * t807 
     #+ t810 * t811 / 0.2D1) * t832 - t840 * t811 / 0.2D1 + t843 * t807)
     # - 0.180D3 * t187 * t56 * (-(t849 - t806 * t811) * t832 - t853 + t
     #843 * t811) + t196 * t863) * t98 * t203 / 0.1440D4 + (t207 * t56 *
     # (-t807 + t873 * t811) + 0.90D2 * t206 * t56 * (-t878 * t807 / 0.2
     #D1 - t881 + t882 * t811 / 0.6D1 + t873 * t795) - t227 * t56 * t811
     # - 0.180D3 * t230 * t56 * (-t878 * t811 / 0.2D1 - t795 + t873 * t8
     #07)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 * (-(t849 - t908 * t
     #811) * t832 - t853 + t915 * t811) - 0.180D3 * t187 * t863) * t98 *
     # t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t931 * t807 - t934 * t81
     #1 / 0.2D1 - t796) - 0.180D3 * t187 * t56 * (t931 * t811 - t853) - 
     #t288 * t289 * t811) * t265 * t203 / 0.720D3
      t953 = FJET(XB1, XB2, s, 0.0D0, t784, -t790, 0.0D0, -t794, t952)
      t955 = x2 * s
      t956 = t955 * t1
      t957 = -0.1D1 + x2
      t958 = t957 * s
      t959 = t958 * t1
      t960 = t31 * t957
      t964 = log(-0.4D1 * t243 * t101 * t960)
      t965 = x2 * z
      t967 = 0.1D1 / (-x2 + t965 - z)
      t968 = t964 * t967
      t969 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0.
     #0D0, 0.0D0, 0.0D0)
      t971 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0.
     #0D0, 0.0D0, 0.0D0)
      t972 = t967 * t971
      t977 = t187 * t19
      t978 = t23 * t967
      t979 = t978 * t969
      t986 = t32 * t957
      t989 = log(-0.4D1 * t927 * t986)
      t990 = t989 * t967
      t992 = t989 ** 2
      t993 = t992 * t967
      t996 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0.
     #0D0, 0.0D0, 0.0D0)
      t997 = t967 * t996
      t1007 = t288 * t979
      t1014 = log(-0.4D1 * t296 * t986)
      t1015 = t1014 ** 2
      t1016 = t1015 * t967
      t1019 = t1014 * t967
      t1036 = log(-0.4D1 * t331 * t986)
      t1037 = t1036 * t967
      t1042 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1044 = t1036 ** 2
      t1045 = t1044 * t967
      t1050 = t1044 * t1036 * t967
      t1058 = pi * t75 * t19
      t1070 = (0.90D2 * t161 * t23 * (-t968 * t969 + t972) - 0.180D3 * t
     #977 * t979) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (-t990
     # * t971 + t993 * t969 / 0.2D1 + t997) - 0.180D3 * t187 * t56 * (-t
     #990 * t969 + t972) + t1007) * t265 * t203 / 0.720D3 + (0.90D2 * t1
     #61 * t23 * (t997 + t1016 * t969 / 0.2D1 - t1019 * t971) - 0.180D3 
     #* t187 * t56 * (-t1019 * t969 + t972) + t1007) * t98 * t265 / 0.14
     #40D4 + (t196 * t56 * (-t1037 * t969 + t972) + 0.90D2 * t161 * t23 
     #* (t967 * t1042 + t1045 * t971 / 0.2D1 - t1037 * t996 - t1050 * t9
     #69 / 0.6D1) + t1058 * t979 - 0.180D3 * t187 * t56 * (t997 + t1045 
     #* t969 / 0.2D1 - t1037 * t971)) * t265 / 0.1440D4
      t1071 = FJET(XB1, XB2, s, 0.0D0, t956, 0.0D0, -t959, 0.0D0, t1070)
      t1073 = x2 * x3
      t1076 = Sqrt(x3 * t957 * t38)
      t1077 = t44 * t1076
      t1079 = 0.2D1 * t1077 * x2
      t1081 = 0.1D1 - x3 + t1073
      t1082 = 0.1D1 / t1081
      t1084 = t2 * (0.1D1 - x3 - x2 + t1073 + t242 + t1079) * t1082
      t1089 = t2 * x2 * (-0.1D1 + t1073 + 0.2D1 * t1077) * t1082
      t1090 = t242 * z
      t1091 = t1073 * z
      t1097 = 0.1D1 / (-t242 + x3 + x2 + t1090 - t965 - t1091 + z - t107
     #9 - 0.2D1 * t1077 * z + 0.2D1 * t1077 * t965)
      t1098 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1099 = t1097 * t1098
      t1100 = t957 * t38
      t1101 = t1081 ** 2
      t1102 = 0.1D1 / t1101
      t1103 = t1100 * t1102
      t1107 = log(0.4D1 * t902 * t32 * t1103)
      t1108 = t1107 * t1097
      t1109 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1115 = t23 * t1097
      t1116 = t1115 * t1109
      t1128 = log(0.4D1 * t242 * t101 * t960 * t38 * t1102)
      t1129 = t1128 ** 2
      t1130 = t1129 * t1097
      t1133 = t1128 * t1097
      t1135 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1151 = (0.90D2 * t161 * t23 * (t1099 - t1108 * t1109) - 0.180D3 *
     # t977 * t1116) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t1
     #130 * t1109 / 0.2D1 - t1133 * t1098 + t1097 * t1135) - 0.180D3 * t
     #187 * t56 * (-t1133 * t1109 + t1099) + t288 * t1116) * t98 * t265 
     #/ 0.1440D4
      t1152 = FJET(XB1, XB2, s, 0.0D0, t1084, 0.0D0, -t1089, 0.0D0, t115
     #1)
      t1154 = t1 * t785
      t1156 = t958 * t1154 * t788
      t1157 = t955 * t1154
      t1159 = t791 * t957 * t793
      t1160 = x2 * x1
      t1161 = t1160 * z
      t1163 = 0.1D1 / (t1161 + z - t965 - t1160 + x2)
      t1164 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1165 = t1163 * t1164
      t1170 = log(-0.4D1 * t902 * t32 * t834 * t957)
      t1171 = t1170 * t1163
      t1172 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1178 = t23 * t1163
      t1179 = t1178 * t1172
      t1190 = log(-0.4D1 * t269 * t101 * t799 * t800 * t957)
      t1191 = t1190 ** 2
      t1192 = t1191 * t1163
      t1195 = t1190 * t1163
      t1197 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1213 = (0.90D2 * t161 * t23 * (t1165 - t1171 * t1172) - 0.180D3 *
     # t977 * t1179) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t1
     #192 * t1172 / 0.2D1 - t1195 * t1164 + t1163 * t1197) - 0.180D3 * t
     #187 * t56 * (t1165 - t1195 * t1172) + t288 * t1179) * t265 * t203 
     #/ 0.720D3
      t1214 = FJET(XB1, XB2, s, 0.0D0, t1156, t784, -t1157, t1159, t1213
     #)
      t1216 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1217 = t967 * t1216
      t1218 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1224 = t978 * t1218
      t1232 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1233 = t967 * t1232
      t1245 = t288 * t1224
      t1273 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1292 = (0.90D2 * t161 * t23 * (t1217 - t968 * t1218) - 0.180D3 * 
     #t977 * t1224) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (-t9
     #90 * t1216 + t1233 + t993 * t1218 / 0.2D1) - 0.180D3 * t187 * t56 
     #* (-t990 * t1218 + t1217) + t1245) * t265 * t203 / 0.720D3 + (0.90
     #D2 * t161 * t23 * (t1233 - t1019 * t1216 + t1016 * t1218 / 0.2D1) 
     #- 0.180D3 * t187 * t56 * (-t1019 * t1218 + t1217) + t1245) * t98 *
     # t265 / 0.1440D4 + (t196 * t56 * (-t1037 * t1218 + t1217) + 0.90D2
     # * t161 * t23 * (-t1037 * t1232 - t1050 * t1218 / 0.6D1 + t967 * t
     #1273 + t1045 * t1216 / 0.2D1) + t1058 * t1224 - 0.180D3 * t187 * t
     #56 * (t1233 + t1045 * t1218 / 0.2D1 - t1037 * t1216)) * t265 / 0.1
     #440D4
      t1293 = FJET(XB1, XB2, s, 0.0D0, -t959, 0.0D0, t956, 0.0D0, t1292)
      t1295 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1298 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1300 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1301 = t36 * t1300
      t1313 = t36 * t1298
      t1314 = t787 * t1298
      t1326 = t56 * (-t787 * t1295 * t832 - t36 * t1295)
      t1338 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1390 = (0.90D2 * t161 * t23 * (-t840 * t1295 / 0.2D1 + t843 * t12
     #98 - t1301 - (t787 * t1300 - t806 * t1298 + t810 * t1295 / 0.2D1) 
     #* t832) - 0.180D3 * t187 * t56 * (t843 * t1295 - t1313 - (t1314 - 
     #t806 * t1295) * t832) + t196 * t1326) * t98 * t203 / 0.1440D4 + (t
     #207 * t56 * (t873 * t1295 - t1298) + 0.90D2 * t206 * t56 * (-t878 
     #* t1298 / 0.2D1 - t1338 + t873 * t1300 + t882 * t1295 / 0.6D1) - t
     #227 * t56 * t1295 - 0.180D3 * t230 * t56 * (-t878 * t1295 / 0.2D1 
     #+ t873 * t1298 - t1300)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 
     #* (-(t1314 - t908 * t1295) * t832 + t915 * t1295 - t1313) - 0.180D
     #3 * t187 * t1326) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * 
     #(-t934 * t1295 / 0.2D1 + t931 * t1298 - t1301) - 0.180D3 * t187 * 
     #t56 * (-t1313 + t931 * t1295) - t288 * t289 * t1295) * t265 * t203
     # / 0.720D3
      t1391 = FJET(XB1, XB2, s, 0.0D0, -t790, t784, 0.0D0, -t794, t1390)
      t1393 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1395 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1396 = t1097 * t1395
      t1401 = t1115 * t1393
      t1411 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1427 = (0.90D2 * t161 * t23 * (-t1108 * t1393 + t1396) - 0.180D3 
     #* t977 * t1401) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (-
     #t1133 * t1395 + t1130 * t1393 / 0.2D1 + t1097 * t1411) - 0.180D3 *
     # t187 * t56 * (-t1133 * t1393 + t1396) + t288 * t1401) * t98 * t26
     #5 / 0.1440D4
      t1428 = FJET(XB1, XB2, s, 0.0D0, -t1089, 0.0D0, t1084, 0.0D0, t142
     #7)
      t1430 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1433 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1434 = t36 * t1433
      t1435 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1438 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1444 = t36 * t1438
      t1451 = t36 * t1430
      t1456 = t36 * t1435
      t1458 = rrqg2qgh62J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1496 = t56 * (t1456 + t1435 * t51)
      t1497 = t196 * t1496
      t1516 = t227 * t56 * t1435
      t1559 = t1430 * pi
      t1562 = t1438 * pi
      t1564 = t1435 * pi
      t1636 = (-0.180D3 * (t106 * t1430 / 0.2D1 + t1434 - t111 * t1435 /
     # 0.6D1 - t114 * t1438) * pi * lh + (t1444 - t114 * t1430 + t106 * 
     #t1435 / 0.2D1) * pi * t16 + (t1451 - t114 * t1435) * pi * t75 + t1
     #456 * t141 + 0.90D2 * (t36 * t1458 - t111 * t1430 / 0.6D1 + t106 *
     # t1438 / 0.2D1 - t114 * t1433 + t151 * t1435 / 0.24D2) * pi) * t19
     # * t23 / 0.2880D4 + (0.90D2 * t161 * t23 * (t180 * t1435 / 0.2D1 +
     # (t169 * t1435 / 0.2D1 + t1438 - t167 * t1430) * t51 + t1444 - t17
     #7 * t1430) - 0.180D3 * t187 * t56 * (-t177 * t1435 + (t1430 - t167
     # * t1435) * t51 + t1451) + t1497) * t98 * t203 / 0.1440D4 + (t207 
     #* t56 * (t1430 - t211 * t1435) + 0.90D2 * t206 * t56 * (-t211 * t1
     #438 + t216 * t1430 / 0.2D1 + t1433 - t219 * t1435 / 0.6D1) + t1516
     # - 0.180D3 * t230 * t56 * (t1438 - t211 * t1430 + t216 * t1435 / 0
     #.2D1)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 * (t1451 - t247 * 
     #t1435 + (t1430 - t253 * t1435) * t51) - 0.180D3 * t187 * t1496) * 
     #t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t274 * t1435 / 0.2D
     #1 - t277 * t1430 + t1444) - 0.180D3 * t187 * t56 * (t1451 - t277 *
     # t1435) + t288 * t289 * t1435) * t265 * t203 / 0.720D3 + ((-0.180D
     #3 * t1559 * lh + 0.90D2 * t1562 + t1564 * t16) * t19 * t54 + 0.90D
     #2 * t1564 * t65 + (-0.180D3 * t1562 * lh + t1564 * t75 + 0.90D2 * 
     #t1433 * pi + t1559 * t16) * t19 * t84 + (-0.180D3 * t1564 * lh + 0
     #.90D2 * t1559) * t19 * t95) * t98 / 0.2880D4 + (0.90D2 * t161 * t2
     #3 * ((t301 * t1435 / 0.2D1 + t1438 - t299 * t1430) * t51 + t1444 +
     # t310 * t1435 / 0.2D1 - t313 * t1430) - 0.180D3 * t187 * t56 * (t1
     #451 + (t1430 - t299 * t1435) * t51 - t313 * t1435) + t1497) * t98 
     #* t265 / 0.1440D4 + (t196 * t56 * (t1451 - t335 * t1435) + 0.90D2 
     #* t161 * t23 * (-t335 * t1438 + t341 * t1430 / 0.2D1 + t1434 - t34
     #5 * t1435 / 0.6D1) + t1516 - 0.180D3 * t187 * t56 * (t1444 - t335 
     #* t1430 + t341 * t1435 / 0.2D1)) * t265 / 0.1440D4
      t1637 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1636)
      t1639 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1640 = t36 * t1639
      t1641 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1644 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1657 = t36 * t1644
      t1658 = t787 * t1644
      t1670 = t56 * (-t787 * t1641 * t832 - t36 * t1641)
      t1683 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t1734 = (0.90D2 * t161 * t23 * (-t1640 - t840 * t1641 / 0.2D1 + t8
     #43 * t1644 - (t787 * t1639 - t806 * t1644 + t810 * t1641 / 0.2D1) 
     #* t832) - 0.180D3 * t187 * t56 * (t843 * t1641 - t1657 - (t1658 - 
     #t806 * t1641) * t832) + t196 * t1670) * t98 * t203 / 0.1440D4 + (t
     #207 * t56 * (-t1644 + t873 * t1641) + 0.90D2 * t206 * t56 * (-t878
     # * t1644 / 0.2D1 + t873 * t1639 - t1683 + t882 * t1641 / 0.6D1) - 
     #t227 * t56 * t1641 - 0.180D3 * t230 * t56 * (-t1639 + t873 * t1644
     # - t878 * t1641 / 0.2D1)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23
     # * (t915 * t1641 - t1657 - (t1658 - t908 * t1641) * t832) - 0.180D
     #3 * t187 * t1670) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * 
     #(-t1640 + t931 * t1644 - t934 * t1641 / 0.2D1) - 0.180D3 * t187 * 
     #t56 * (-t1657 + t931 * t1641) - t288 * t289 * t1641) * t265 * t203
     # / 0.720D3
      t1735 = FJET(XB1, XB2, s, t784, 0.0D0, 0.0D0, -t790, -t794, t1734)
      t1737 = t364 * t363 + t573 * t572 + t782 * t781 + t953 * t952 + t1
     #071 * t1070 + t1152 * t1151 + t1214 * t1213 + t1293 * t1292 + t139
     #1 * t1390 + t1428 * t1427 + t1637 * t1636 + t1735 * t1734
      t1738 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1739 = t1163 * t1738
      t1740 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1746 = t1178 * t1740
      t1754 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1771 = (0.90D2 * t161 * t23 * (t1739 - t1171 * t1740) - 0.180D3 *
     # t977 * t1746) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t1
     #192 * t1740 / 0.2D1 + t1163 * t1754 - t1195 * t1738) - 0.180D3 * t
     #187 * t56 * (t1739 - t1195 * t1740) + t288 * t1746) * t265 * t203 
     #/ 0.720D3
      t1772 = FJET(XB1, XB2, s, t784, -t1157, 0.0D0, t1156, t1159, t1771
     #)
      t1774 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1775 = t967 * t1774
      t1776 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1779 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1786 = t967 * t1779
      t1791 = t978 * t1776
      t1792 = t288 * t1791
      t1801 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t1850 = (0.90D2 * t161 * t23 * (t1775 + t1016 * t1776 / 0.2D1 - t1
     #019 * t1779) - 0.180D3 * t187 * t56 * (-t1019 * t1776 + t1786) + t
     #1792) * t98 * t265 / 0.1440D4 + (t196 * t56 * (t1786 - t1037 * t17
     #76) + 0.90D2 * t161 * t23 * (t967 * t1801 - t1037 * t1774 - t1050 
     #* t1776 / 0.6D1 + t1045 * t1779 / 0.2D1) + t1058 * t1791 - 0.180D3
     # * t187 * t56 * (t1775 + t1045 * t1776 / 0.2D1 - t1037 * t1779)) *
     # t265 / 0.1440D4 + (0.90D2 * t161 * t23 * (-t968 * t1776 + t1786) 
     #- 0.180D3 * t977 * t1791) * t98 * t266 / 0.720D3 + (0.90D2 * t161 
     #* t23 * (t993 * t1776 / 0.2D1 - t990 * t1779 + t1775) - 0.180D3 * 
     #t187 * t56 * (-t990 * t1776 + t1786) + t1792) * t265 * t203 / 0.72
     #0D3
      t1851 = FJET(XB1, XB2, s, t956, 0.0D0, -t959, 0.0D0, 0.0D0, t1850)
      t1853 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1855 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1857 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t1865 = t1097 * t1853
      t1870 = t1115 * t1857
      t1887 = (0.90D2 * t161 * t23 * (-t1133 * t1853 + t1097 * t1855 + t
     #1130 * t1857 / 0.2D1) - 0.180D3 * t187 * t56 * (-t1133 * t1857 + t
     #1865) + t288 * t1870) * t98 * t265 / 0.1440D4 + (0.90D2 * t161 * t
     #23 * (t1865 - t1108 * t1857) - 0.180D3 * t977 * t1870) * t98 * t26
     #6 / 0.720D3
      t1888 = FJET(XB1, XB2, s, t1084, 0.0D0, -t1089, 0.0D0, 0.0D0, t188
     #7)
      t1890 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1891 = t1163 * t1890
      t1892 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1898 = t1178 * t1892
      t1906 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t1923 = (0.90D2 * t161 * t23 * (t1891 - t1171 * t1892) - 0.180D3 *
     # t977 * t1898) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t1
     #192 * t1892 / 0.2D1 + t1163 * t1906 - t1195 * t1890) - 0.180D3 * t
     #187 * t56 * (t1891 - t1195 * t1892) + t288 * t1898) * t265 * t203 
     #/ 0.720D3
      t1924 = FJET(XB1, XB2, s, t1156, 0.0D0, -t1157, t784, t1159, t1923
     #)
      t1927 = t784 * t1073 * t1082
      t1928 = t2 * t785
      t1929 = t242 * x1
      t1931 = Sqrt(t825 * t1100)
      t1932 = t44 * t1931
      t1934 = 0.2D1 * t1932 * x2
      t1935 = t242 * t786
      t1939 = t1928 * (-t1929 - x2 + t1073 + 0.1D1 - x3 + t242 + t1934 +
     # t1935) * t788 * t1082
      t1943 = t38 * s * t1 * x1 * t1082
      t1949 = t1928 * x2 * (-0.1D1 + t1073 + x1 - t815 - t786 + t816 + 0
     #.2D1 * t1932) * t788 * t1082
      t1954 = t162 * x2
      t1955 = -t818 + t824 - t1929 - x2 + t786 + 0.2D1 * t1160 - t163 - 
     #0.3D1 * t1161 - x3 - t817 + t820 + t822 - t823 - t1090 + t1091 + 0
     #.2D1 * t1932 * t1161 - t1954
      t1958 = t28 * x2
      t1976 = -z - t162 * t28 * x2 + t1958 * x1 + 0.2D1 * t1954 * z - t1
     #073 * x1 + t163 * x2 + 0.2D1 * t1932 * z + t1934 + t1935 - 0.2D1 *
     # t1932 * t965 - 0.2D1 * t1932 * t1160 + 0.2D1 * t1073 * t786 - t81
     #9 * t1160 - 0.2D1 * t163 * t965 + t163 * t1958 + t965 + t242
      t1978 = 0.1D1 / (t1955 + t1976)
      t1979 = t787 * t1978
      t1980 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t1986 = log(0.4D1 * t242 * t869 * t870 * t1103)
      t1987 = t1986 * t787
      t1988 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t1989 = t1978 * t1988
      t1995 = t23 * t787
      t1999 = 0.90D2 * t161 * t23 * (t1979 * t1980 - t1987 * t1989) - 0.
     #180D3 * t977 * t1995 * t1989
      t2003 = FJET(XB1, XB2, s, t1927, -t1939, -t1943, t1949, t1159, t19
     #99 * t98 * t266 / 0.720D3)
      t2006 = t98 * t265 * t203
      t2009 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t2010 = t1978 * t2009
      t2012 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t2021 = 0.90D2 * t161 * t23 * (-t1987 * t2010 + t1979 * t2012) - 0
     #.180D3 * t977 * t1995 * t2010
      t2025 = FJET(XB1, XB2, s, t1949, -t1943, -t1939, t1927, t1159, t20
     #21 * t98 * t266 / 0.720D3)
      t2029 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t2032 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t2034 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t2035 = t967 * t2034
      t2040 = t967 * t2032
      t2046 = t978 * t2029
      t2047 = t288 * t2046
      t2056 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, t956, -t959, 0
     #.0D0, 0.0D0, 0.0D0)
      t2105 = (0.90D2 * t161 * t23 * (t1016 * t2029 / 0.2D1 - t1019 * t2
     #032 + t2035) - 0.180D3 * t187 * t56 * (t2040 - t1019 * t2029) + t2
     #047) * t98 * t265 / 0.1440D4 + (t196 * t56 * (t2040 - t1037 * t202
     #9) + 0.90D2 * t161 * t23 * (t967 * t2056 - t1037 * t2034 - t1050 *
     # t2029 / 0.6D1 + t1045 * t2032 / 0.2D1) + t1058 * t2046 - 0.180D3 
     #* t187 * t56 * (t2035 + t1045 * t2029 / 0.2D1 - t1037 * t2032)) * 
     #t265 / 0.1440D4 + (0.90D2 * t161 * t23 * (-t968 * t2029 + t2040) -
     # 0.180D3 * t977 * t2046) * t98 * t266 / 0.720D3 + (0.90D2 * t161 *
     # t23 * (t993 * t2029 / 0.2D1 - t990 * t2032 + t2035) - 0.180D3 * t
     #187 * t56 * (-t990 * t2029 + t2040) + t2047) * t265 * t203 / 0.720
     #D3
      t2106 = FJET(XB1, XB2, s, -t959, 0.0D0, t956, 0.0D0, 0.0D0, t2105)
      t2108 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t2109 = t36 * t2108
      t2110 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t2113 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t2126 = t36 * t2113
      t2127 = t787 * t2113
      t2139 = t56 * (-t787 * t2110 * t832 - t36 * t2110)
      t2152 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t790, 
     #t784, 0.0D0, -t794)
      t2203 = (0.90D2 * t161 * t23 * (-t2109 - t840 * t2110 / 0.2D1 + t8
     #43 * t2113 - (t787 * t2108 - t806 * t2113 + t810 * t2110 / 0.2D1) 
     #* t832) - 0.180D3 * t187 * t56 * (t843 * t2110 - t2126 - (t2127 - 
     #t806 * t2110) * t832) + t196 * t2139) * t98 * t203 / 0.1440D4 + (t
     #207 * t56 * (-t2113 + t873 * t2110) + 0.90D2 * t206 * t56 * (-t878
     # * t2113 / 0.2D1 + t873 * t2108 - t2152 + t882 * t2110 / 0.6D1) - 
     #t227 * t56 * t2110 - 0.180D3 * t230 * t56 * (t873 * t2113 - t878 *
     # t2110 / 0.2D1 - t2108)) * t203 / 0.1440D4 + (0.90D2 * t161 * t23 
     #* (t915 * t2110 - t2126 - (t2127 - t908 * t2110) * t832) - 0.180D3
     # * t187 * t2139) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (
     #-t934 * t2110 / 0.2D1 - t2109 + t931 * t2113) - 0.180D3 * t187 * t
     #56 * (-t2126 + t931 * t2110) - t288 * t289 * t2110) * t265 * t203 
     #/ 0.720D3
      t2204 = FJET(XB1, XB2, s, -t790, 0.0D0, 0.0D0, t784, -t794, t2203)
      t2206 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t2207 = t1163 * t2206
      t2208 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t2214 = t1178 * t2208
      t2220 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t1157, t1156,
     # t784, 0.0D0, t1159)
      t2239 = (0.90D2 * t161 * t23 * (t2207 - t1171 * t2208) - 0.180D3 *
     # t977 * t2214) * t98 * t266 / 0.720D3 + (0.90D2 * t161 * t23 * (t1
     #163 * t2220 - t1195 * t2206 + t1192 * t2208 / 0.2D1) - 0.180D3 * t
     #187 * t56 * (t2207 - t1195 * t2208) + t288 * t2214) * t265 * t203 
     #/ 0.720D3
      t2240 = FJET(XB1, XB2, s, -t1157, t784, t1156, 0.0D0, t1159, t2239
     #)
      t2242 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t2244 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t2246 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t1089, t1084,
     # 0.0D0, 0.0D0, 0.0D0)
      t2253 = t1097 * t2242
      t2259 = t1115 * t2246
      t2276 = (0.90D2 * t161 * t23 * (-t1133 * t2242 + t1097 * t2244 + t
     #1130 * t2246 / 0.2D1) - 0.180D3 * t187 * t56 * (t2253 - t1133 * t2
     #246) + t288 * t2259) * t98 * t265 / 0.1440D4 + (0.90D2 * t161 * t2
     #3 * (t2253 - t1108 * t2246) - 0.180D3 * t977 * t2259) * t98 * t266
     # / 0.720D3
      t2277 = FJET(XB1, XB2, s, -t1089, 0.0D0, t1084, 0.0D0, 0.0D0, t227
     #6)
      t2279 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t2280 = t1978 * t2279
      t2282 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t2291 = 0.90D2 * t161 * t23 * (-t1987 * t2280 + t1979 * t2282) - 0
     #.180D3 * t977 * t1995 * t2280
      t2295 = FJET(XB1, XB2, s, -t1943, t1949, t1927, -t1939, t1159, t22
     #91 * t98 * t266 / 0.720D3)
      t2299 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t2300 = t1978 * t2299
      t2302 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t1949, -t1939,
     # -t1943, t1927, t1159)
      t2311 = 0.90D2 * t161 * t23 * (-t1987 * t2300 + t1979 * t2302) - 0
     #.180D3 * t977 * t1995 * t2300
      t2315 = FJET(XB1, XB2, s, -t1939, t1927, t1949, -t1943, t1159, t23
     #11 * t98 * t266 / 0.720D3)
      t2319 = t1772 * t1771 + t1851 * t1850 + t1888 * t1887 + t1924 * t1
     #923 + t2003 * t1999 * t2006 / 0.720D3 + t2025 * t2021 * t2006 / 0.
     #720D3 + t2106 * t2105 + t2204 * t2203 + t2240 * t2239 + t2277 * t2
     #276 + t2295 * t2291 * t2006 / 0.720D3 + t2315 * t2311 * t2006 / 0.
     #720D3
      rrqg2qght6s1e1 = t1737 + t2319

      end function



      doubleprecision function rrqg2qght6s1e0
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = t21 ** 2
      t23 = 0.1D1 / z
      t25 = -0.1D1 + x3
      t26 = 0.1D1 / t25
      t27 = t18 * t26
      t30 = log(-0.4D1 * t13 * t27)
      t31 = t30 ** 2
      t32 = cos(t10)
      t34 = Sqrt(-x3 * t25)
      t39 = 0.1D1 / (-x3 - z + 0.2D1 * t32 * t34 * z)
      t43 = t9 * (t22 * t23 / 0.2D1 + t31 * t39 / 0.2D1)
      t48 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t49 = t48 * pi
      t56 = t8 * (-t21 * t23 - t30 * t39)
      t60 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t63 = lh ** 2
      t65 = pi ** 2
      t67 = 0.180D3 * t63 - 0.30D2 * t65
      t72 = t8 * (t23 + t39)
      t75 = 0.1D1 / x3
      t78 = t23 * t60
      t79 = t15 * t12
      t80 = t79 * t17
      t82 = log(0.4D1 * t80)
      t83 = t82 * t23
      t85 = t82 ** 2
      t86 = t85 * t23
      t93 = t23 * t3
      t100 = pi * (0.60D2 * lh * t65 - 0.240D3 * zeta3 - 0.120D3 * t63 *
     # lh)
      t104 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t107 = t85 * t82 * t23
      t114 = t23 * t48
      t123 = pi * t5
      t124 = x2 ** 2
      t125 = x3 * t124
      t126 = t125 * t12
      t129 = log(-0.4D1 * t126 * t27)
      t135 = log(0.4D1 * t125 * t80)
      t136 = t135 * t23
      t142 = pi * lh
      t144 = t3 * t39 + t93
      t147 = 0.180D3 * t142 * t9 * t144
      t150 = 0.1D1 / x2
      t153 = t124 * t12
      t156 = log(0.4D1 * t153 * t18)
      t157 = t156 * t23
      t159 = t156 ** 2
      t160 = t159 * t23
      t173 = pi * t67 * t5
      t174 = t8 * t23
      t175 = t174 * t3
      t176 = t173 * t175
      t180 = x1 ** 2
      t181 = x3 * t180
      t182 = t181 * t12
      t185 = log(-0.4D1 * t182 * t27)
      t191 = log(0.4D1 * t181 * t80)
      t192 = t191 * t23
      t200 = 0.1D1 / x1
      t203 = t123 * t8
      t205 = t150 * t200
      t209 = t124 * t180
      t212 = log(0.4D1 * t209 * t80)
      t213 = t212 * t23
      t219 = t142 * t5
      t226 = t23 * pi
      t227 = t180 * t12
      t230 = log(0.4D1 * t227 * t18)
      t231 = t230 ** 2
      t239 = t226 * lh
      t248 = (0.90D2 * t4 * t43 + (-0.180D3 * t4 * lh + 0.90D2 * t49) * 
     #t5 * t56 + (-0.180D3 * t49 * lh + 0.90D2 * t60 * pi + t4 * t67) * 
     #t5 * t72) * t75 / 0.2880D4 + (-0.180D3 * (t78 - t83 * t48 + t86 * 
     #t3 / 0.2D1) * pi * lh + t93 * t100 + 0.90D2 * (t86 * t48 / 0.2D1 +
     # t23 * t104 - t107 * t3 / 0.6D1 - t83 * t60) * pi + (t114 - t83 * 
     #t3) * pi * t67) * t5 * t8 / 0.2880D4 + (0.90D2 * t123 * t8 * ((t48
     # - t129 * t3) * t39 + t114 - t136 * t3) - t147) * t75 * t150 / 0.1
     #440D4 + (0.90D2 * t123 * t8 * (t78 - t157 * t48 + t160 * t3 / 0.2D
     #1) - 0.180D3 * t142 * t9 * (t114 - t157 * t3) + t176) * t150 / 0.1
     #440D4 + (0.90D2 * t123 * t8 * ((t48 - t185 * t3) * t39 - t192 * t3
     # + t114) - t147) * t75 * t200 / 0.1440D4 + t203 * t144 * t75 * t20
     #5 / 0.8D1 + (0.90D2 * t123 * t8 * (-t213 * t3 + t114) - 0.180D3 * 
     #t219 * t175) * t150 * t200 / 0.720D3 + (0.90D2 * t226 * t9 * (t60 
     #+ t231 * t3 / 0.2D1 - t230 * t48) - 0.180D3 * t239 * t9 * (-t230 *
     # t3 + t48) + t176) * t200 / 0.1440D4
      t249 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t248)
      t251 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t252 = t251 * pi
      t257 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t258 = t257 * pi
      t265 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t275 = t23 * t265
      t283 = t23 * t251
      t287 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t295 = t23 * t257
      t313 = t251 * t39 + t283
      t316 = 0.180D3 * t142 * t9 * t313
      t333 = t174 * t251
      t334 = t173 * t333
      t380 = (0.90D2 * t252 * t43 + (-0.180D3 * t252 * lh + 0.90D2 * t25
     #8) * t5 * t56 + (-0.180D3 * t258 * lh + 0.90D2 * t265 * pi + t252 
     #* t67) * t5 * t72) * t75 / 0.2880D4 + (-0.180D3 * (t275 - t83 * t2
     #57 + t86 * t251 / 0.2D1) * pi * lh + t283 * t100 + 0.90D2 * (t86 *
     # t257 / 0.2D1 + t23 * t287 - t107 * t251 / 0.6D1 - t83 * t265) * p
     #i + (t295 - t83 * t251) * pi * t67) * t5 * t8 / 0.2880D4 + (0.90D2
     # * t123 * t8 * (-t136 * t251 + t295 + (t257 - t129 * t251) * t39) 
     #- t316) * t75 * t150 / 0.1440D4 + (0.90D2 * t123 * t8 * (t275 - t1
     #57 * t257 + t160 * t251 / 0.2D1) - 0.180D3 * t142 * t9 * (t295 - t
     #157 * t251) + t334) * t150 / 0.1440D4 + (0.90D2 * t123 * t8 * ((t2
     #57 - t185 * t251) * t39 - t192 * t251 + t295) - t316) * t75 * t200
     # / 0.1440D4 + t203 * t313 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * 
     #t8 * (-t213 * t251 + t295) - 0.180D3 * t219 * t333) * t150 * t200 
     #/ 0.720D3 + (0.90D2 * t226 * t9 * (t265 - t230 * t257 + t231 * t25
     #1 / 0.2D1) - 0.180D3 * t239 * t9 * (t257 - t230 * t251) + t334) * 
     #t200 / 0.1440D4
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t380)
      t383 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t384 = t383 * pi
      t389 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t390 = t389 * pi
      t397 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t407 = t23 * t397
      t415 = t23 * t383
      t419 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t427 = t23 * t389
      t445 = t383 * t39 + t415
      t448 = 0.180D3 * t142 * t9 * t445
      t465 = t174 * t383
      t466 = t173 * t465
      t512 = (0.90D2 * t384 * t43 + (-0.180D3 * t384 * lh + 0.90D2 * t39
     #0) * t5 * t56 + (-0.180D3 * t390 * lh + 0.90D2 * t397 * pi + t384 
     #* t67) * t5 * t72) * t75 / 0.2880D4 + (-0.180D3 * (t407 - t83 * t3
     #89 + t86 * t383 / 0.2D1) * pi * lh + t415 * t100 + 0.90D2 * (t86 *
     # t389 / 0.2D1 + t23 * t419 - t107 * t383 / 0.6D1 - t83 * t397) * p
     #i + (t427 - t83 * t383) * pi * t67) * t5 * t8 / 0.2880D4 + (0.90D2
     # * t123 * t8 * ((t389 - t129 * t383) * t39 + t427 - t136 * t383) -
     # t448) * t75 * t150 / 0.1440D4 + (0.90D2 * t123 * t8 * (t407 - t15
     #7 * t389 + t160 * t383 / 0.2D1) - 0.180D3 * t142 * t9 * (t427 - t1
     #57 * t383) + t466) * t150 / 0.1440D4 + (0.90D2 * t123 * t8 * ((t38
     #9 - t185 * t383) * t39 + t427 - t192 * t383) - t448) * t75 * t200 
     #/ 0.1440D4 + t203 * t445 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t
     #8 * (t427 - t213 * t383) - 0.180D3 * t219 * t465) * t150 * t200 / 
     #0.720D3 + (0.90D2 * t226 * t9 * (-t230 * t389 + t231 * t383 / 0.2D
     #1 + t397) - 0.180D3 * t239 * t9 * (-t230 * t383 + t389) + t466) * 
     #t200 / 0.1440D4
      t513 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t512)
      t515 = t2 * x1
      t516 = -0.1D1 + x1
      t517 = x1 * z
      t518 = 0.1D1 - x1 + t517
      t519 = 0.1D1 / t518
      t521 = t2 * t516 * t519
      t522 = s * t16
      t524 = t516 * x1 * t519
      t525 = t522 * t524
      t526 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, t
     #515, 0.0D0, -t525)
      t529 = t17 * t519
      t530 = t516 ** 2
      t535 = log(-0.4D1 * t181 * t79 * t529 * t530 * t26)
      t536 = t535 * t518
      t537 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, t
     #515, 0.0D0, -t525)
      t540 = x3 * x1
      t541 = t540 * z
      t542 = 0.3D1 * t541
      t543 = x1 * t14
      t544 = x3 * t14
      t545 = t544 * x1
      t547 = 0.2D1 * t181 * z
      t548 = t181 * t14
      t549 = 0.2D1 * t540
      t550 = x3 * t518
      t552 = Sqrt(-t550 * t25)
      t556 = -z + t517 - t542 - t543 + t545 + t547 - t548 - t181 + t549 
     #+ 0.2D1 * t32 * t552 * z - x3
      t557 = 0.1D1 / t556
      t559 = t23 * t526
      t561 = t18 * t519 * t530
      t564 = log(0.4D1 * t182 * t561)
      t565 = t564 * t23
      t574 = -t23 * t537 - t518 * t537 * t557
      t586 = t209 * t12
      t589 = log(0.4D1 * t586 * t561)
      t590 = t589 * t23
      t596 = t174 * t537
      t607 = log(0.4D1 * t227 * t15 * t529 * t530)
      t608 = t607 ** 2
      t611 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, t
     #515, 0.0D0, -t525)
      t626 = (0.90D2 * t123 * t8 * (-(t518 * t526 - t536 * t537) * t557 
     #- t559 + t565 * t537) - 0.180D3 * t142 * t9 * t574) * t75 * t200 /
     # 0.1440D4 + t203 * t574 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8
     # * (t590 * t537 - t559) + 0.180D3 * t219 * t596) * t150 * t200 / 0
     #.720D3 + (0.90D2 * t226 * t9 * (-t608 * t537 / 0.2D1 - t611 + t607
     # * t526) - 0.180D3 * t239 * t9 * (-t526 + t607 * t537) - t173 * t5
     #96) * t200 / 0.1440D4
      t627 = FJET(XB1, XB2, s, 0.0D0, t515, -t521, 0.0D0, -t525, t626)
      t629 = x2 * s
      t630 = t629 * t1
      t631 = -0.1D1 + x2
      t632 = t631 * s
      t633 = t632 * t1
      t634 = t18 * t631
      t637 = log(-0.4D1 * t126 * t634)
      t638 = x2 * z
      t640 = 0.1D1 / (-x2 + t638 - z)
      t641 = t637 * t640
      t642 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0.
     #0D0, 0.0D0, 0.0D0)
      t644 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0.
     #0D0, 0.0D0, 0.0D0)
      t645 = t640 * t644
      t650 = t8 * t640
      t651 = t650 * t642
      t653 = 0.180D3 * t219 * t651
      t658 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0.
     #0D0, 0.0D0, 0.0D0)
      t662 = log(-0.4D1 * t153 * t634)
      t663 = t662 ** 2
      t664 = t663 * t640
      t667 = t662 * t640
      t682 = t123 * t650
      t689 = log(-0.4D1 * t586 * t634)
      t690 = t689 * t640
      t700 = (0.90D2 * t123 * t8 * (-t641 * t642 + t645) - t653) * t75 *
     # t150 / 0.1440D4 + (0.90D2 * t123 * t8 * (t640 * t658 + t664 * t64
     #2 / 0.2D1 - t667 * t644) - 0.180D3 * t142 * t9 * (-t667 * t642 + t
     #645) + t173 * t651) * t150 / 0.1440D4 + t682 * t642 * t75 * t205 /
     # 0.8D1 + (0.90D2 * t123 * t8 * (-t690 * t642 + t645) - t653) * t15
     #0 * t200 / 0.720D3
      t701 = FJET(XB1, XB2, s, 0.0D0, t630, 0.0D0, -t633, 0.0D0, t700)
      t703 = x2 * x3
      t706 = Sqrt(x3 * t631 * t25)
      t707 = t32 * t706
      t709 = 0.2D1 * t707 * x2
      t711 = 0.1D1 - x3 + t703
      t712 = 0.1D1 / t711
      t714 = t2 * (0.1D1 - x3 - x2 + t703 + t125 + t709) * t712
      t719 = t2 * x2 * (-0.1D1 + t703 + 0.2D1 * t707) * t712
      t722 = t711 ** 2
      t728 = log(0.4D1 * t125 * t79 * t17 * t631 * t25 / t722)
      t729 = t125 * z
      t730 = t703 * z
      t736 = 0.1D1 / (-t125 + x3 + x2 + t729 - t638 - t730 + z - t709 - 
     #0.2D1 * t707 * z + 0.2D1 * t707 * t638)
      t737 = t728 * t736
      t738 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0.
     #0D0, 0.0D0, 0.0D0)
      t740 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0.
     #0D0, 0.0D0, 0.0D0)
      t746 = t8 * t736
      t754 = t123 * t746
      t759 = (0.90D2 * t123 * t8 * (-t737 * t738 + t736 * t740) - 0.180D
     #3 * t219 * t746 * t738) * t75 * t150 / 0.1440D4 + t754 * t738 * t7
     #5 * t205 / 0.8D1
      t760 = FJET(XB1, XB2, s, 0.0D0, t714, 0.0D0, -t719, 0.0D0, t759)
      t762 = t1 * t516
      t764 = t632 * t762 * t519
      t765 = t629 * t762
      t767 = t522 * t631 * t524
      t768 = x2 * x1
      t769 = t768 * z
      t771 = 0.1D1 / (t769 + z - t638 - t768 + x2)
      t772 = t8 * t771
      t773 = t123 * t772
      t774 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t5
     #15, 0.0D0, t767)
      t779 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t5
     #15, 0.0D0, t767)
      t786 = log(-0.4D1 * t209 * t79 * t529 * t530 * t631)
      t787 = t786 * t771
      t800 = t773 * t774 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * (t
     #771 * t779 - t787 * t774) - 0.180D3 * t219 * t772 * t774) * t150 *
     # t200 / 0.720D3
      t801 = FJET(XB1, XB2, s, 0.0D0, t764, t515, -t765, t767, t800)
      t803 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0.
     #0D0, 0.0D0, 0.0D0)
      t805 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0.
     #0D0, 0.0D0, 0.0D0)
      t806 = t640 * t805
      t811 = t650 * t803
      t813 = 0.180D3 * t219 * t811
      t818 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0.
     #0D0, 0.0D0, 0.0D0)
      t849 = (0.90D2 * t123 * t8 * (-t641 * t803 + t806) - t813) * t75 *
     # t150 / 0.1440D4 + (0.90D2 * t123 * t8 * (t640 * t818 + t664 * t80
     #3 / 0.2D1 - t667 * t805) - 0.180D3 * t142 * t9 * (-t667 * t803 + t
     #806) + t173 * t811) * t150 / 0.1440D4 + t682 * t803 * t75 * t205 /
     # 0.8D1 + (0.90D2 * t123 * t8 * (-t690 * t803 + t806) - t813) * t15
     #0 * t200 / 0.720D3
      t850 = FJET(XB1, XB2, s, 0.0D0, -t633, 0.0D0, t630, 0.0D0, t849)
      t852 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, t
     #515, 0.0D0, -t525)
      t854 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, t
     #515, 0.0D0, -t525)
      t855 = t23 * t854
      t867 = -t518 * t852 * t557 - t23 * t852
      t884 = t174 * t852
      t894 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, t
     #515, 0.0D0, -t525)
      t908 = (0.90D2 * t123 * t8 * (t565 * t852 - t855 - (t518 * t854 - 
     #t536 * t852) * t557) - 0.180D3 * t142 * t9 * t867) * t75 * t200 / 
     #0.1440D4 + t203 * t867 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 
     #* (-t855 + t590 * t852) + 0.180D3 * t219 * t884) * t150 * t200 / 0
     #.720D3 + (0.90D2 * t226 * t9 * (-t608 * t852 / 0.2D1 + t607 * t854
     # - t894) - 0.180D3 * t239 * t9 * (t607 * t852 - t854) - t173 * t88
     #4) * t200 / 0.1440D4
      t909 = FJET(XB1, XB2, s, 0.0D0, -t521, t515, 0.0D0, -t525, t908)
      t911 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0.
     #0D0, 0.0D0, 0.0D0)
      t913 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0.
     #0D0, 0.0D0, 0.0D0)
      t930 = (0.90D2 * t123 * t8 * (-t737 * t911 + t736 * t913) - 0.180D
     #3 * t219 * t746 * t911) * t75 * t150 / 0.1440D4 + t754 * t911 * t7
     #5 * t205 / 0.8D1
      t931 = FJET(XB1, XB2, s, 0.0D0, -t719, 0.0D0, t714, 0.0D0, t930)
      t933 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t934 = t23 * t933
      t935 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t937 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t944 = t23 * t937
      t948 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t956 = t23 * t935
      t974 = t944 + t937 * t39
      t977 = 0.180D3 * t142 * t9 * t974
      t994 = t174 * t937
      t995 = t173 * t994
      t999 = t937 * pi
      t1004 = t935 * pi
      t1062 = (-0.180D3 * (t934 - t83 * t935 + t86 * t937 / 0.2D1) * pi 
     #* lh + t944 * t100 + 0.90D2 * (t86 * t935 / 0.2D1 + t23 * t948 - t
     #107 * t937 / 0.6D1 - t83 * t933) * pi + (t956 - t83 * t937) * pi *
     # t67) * t5 * t8 / 0.2880D4 + (0.90D2 * t123 * t8 * (t956 + (t935 -
     # t129 * t937) * t39 - t136 * t937) - t977) * t75 * t150 / 0.1440D4
     # + (0.90D2 * t123 * t8 * (t934 - t157 * t935 + t160 * t937 / 0.2D1
     #) - 0.180D3 * t142 * t9 * (t956 - t157 * t937) + t995) * t150 / 0.
     #1440D4 + (0.90D2 * t999 * t43 + (-0.180D3 * t999 * lh + 0.90D2 * t
     #1004) * t5 * t56 + (-0.180D3 * t1004 * lh + 0.90D2 * t933 * pi + t
     #999 * t67) * t5 * t72) * t75 / 0.2880D4 + (0.90D2 * t123 * t8 * (-
     #t192 * t937 + (t935 - t185 * t937) * t39 + t956) - t977) * t75 * t
     #200 / 0.1440D4 + t203 * t974 * t75 * t205 / 0.8D1 + (0.90D2 * t123
     # * t8 * (t956 - t213 * t937) - 0.180D3 * t219 * t994) * t150 * t20
     #0 / 0.720D3 + (0.90D2 * t226 * t9 * (t933 - t230 * t935 + t231 * t
     #937 / 0.2D1) - 0.180D3 * t239 * t9 * (t935 - t230 * t937) + t995) 
     #* t200 / 0.1440D4
      t1063 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1062)
      t1065 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, 
     #t515, 0.0D0, -t525)
      t1067 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, 
     #t515, 0.0D0, -t525)
      t1068 = t23 * t1067
      t1080 = -t518 * t1065 * t557 - t23 * t1065
      t1097 = t174 * t1065
      t1104 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, 
     #t515, 0.0D0, -t525)
      t1121 = (0.90D2 * t123 * t8 * (t565 * t1065 - t1068 - (t518 * t106
     #7 - t536 * t1065) * t557) - 0.180D3 * t142 * t9 * t1080) * t75 * t
     #200 / 0.1440D4 + t203 * t1080 * t75 * t205 / 0.8D1 + (0.90D2 * t12
     #3 * t8 * (-t1068 + t590 * t1065) + 0.180D3 * t219 * t1097) * t150 
     #* t200 / 0.720D3 + (0.90D2 * t226 * t9 * (-t1104 + t607 * t1067 - 
     #t608 * t1065 / 0.2D1) - 0.180D3 * t239 * t9 * (-t1067 + t607 * t10
     #65) - t173 * t1097) * t200 / 0.1440D4
      t1122 = FJET(XB1, XB2, s, t515, 0.0D0, 0.0D0, -t521, -t525, t1121)
      t1124 = t249 * t248 + t381 * t380 + t513 * t512 + t627 * t626 + t7
     #01 * t700 + t760 * t759 + t801 * t800 + t850 * t849 + t909 * t908 
     #+ t931 * t930 + t1063 * t1062 + t1122 * t1121
      t1125 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t
     #515, 0.0D0, t767)
      t1130 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t
     #515, 0.0D0, t767)
      t1144 = t773 * t1125 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(t771 * t1130 - t787 * t1125) - 0.180D3 * t219 * t772 * t1125) * t
     #150 * t200 / 0.720D3
      t1145 = FJET(XB1, XB2, s, t515, -t765, 0.0D0, t764, t767, t1144)
      t1147 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0
     #.0D0, 0.0D0, 0.0D0)
      t1153 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0
     #.0D0, 0.0D0, 0.0D0)
      t1154 = t640 * t1153
      t1159 = t650 * t1147
      t1161 = 0.180D3 * t219 * t1159
      t1175 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0
     #.0D0, 0.0D0, 0.0D0)
      t1193 = t682 * t1147 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(-t690 * t1147 + t1154) - t1161) * t150 * t200 / 0.720D3 + (0.90D2
     # * t123 * t8 * (-t641 * t1147 + t1154) - t1161) * t75 * t150 / 0.1
     #440D4 + (0.90D2 * t123 * t8 * (t640 * t1175 + t664 * t1147 / 0.2D1
     # - t667 * t1153) - 0.180D3 * t142 * t9 * (t1154 - t667 * t1147) + 
     #t173 * t1159) * t150 / 0.1440D4
      t1194 = FJET(XB1, XB2, s, t630, 0.0D0, -t633, 0.0D0, 0.0D0, t1193)
      t1196 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0
     #.0D0, 0.0D0, 0.0D0)
      t1202 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0
     #.0D0, 0.0D0, 0.0D0)
      t1215 = t754 * t1196 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(-t737 * t1196 + t736 * t1202) - 0.180D3 * t219 * t746 * t1196) * 
     #t75 * t150 / 0.1440D4
      t1216 = FJET(XB1, XB2, s, t714, 0.0D0, -t719, 0.0D0, 0.0D0, t1215)
      t1218 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t
     #515, 0.0D0, t767)
      t1223 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t
     #515, 0.0D0, t767)
      t1237 = t773 * t1218 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(t771 * t1223 - t787 * t1218) - 0.180D3 * t219 * t772 * t1218) * t
     #150 * t200 / 0.720D3
      t1238 = FJET(XB1, XB2, s, t764, 0.0D0, -t765, t515, t767, t1237)
      t1241 = t515 * t703 * t712
      t1242 = t2 * t516
      t1243 = t125 * x1
      t1246 = Sqrt(t550 * t631 * t25)
      t1247 = t32 * t1246
      t1249 = 0.2D1 * t1247 * x2
      t1250 = t125 * t517
      t1254 = t1242 * (-t1243 - x2 + t703 + 0.1D1 - x3 + t125 + t1249 + 
     #t1250) * t519 * t712
      t1258 = t25 * s * t1 * x1 * t712
      t1264 = t1242 * x2 * (-0.1D1 + t703 + x1 - t540 - t517 + t541 + 0.
     #2D1 * t1247) * t519 * t712
      t1266 = t123 * t8 * t518
      t1267 = t180 * x2
      t1272 = t14 * x2
      t1282 = -t1267 + 0.2D1 * t1247 * t769 - t1243 + t1249 - t180 * t14
     # * x2 + t1272 * x1 + 0.2D1 * t1267 * z - t703 * x1 + t181 * x2 + 0
     #.2D1 * t1247 * z - t548 - x2 - 0.3D1 * t769 - t543 - t181 + t549 +
     # 0.2D1 * t768
      t1293 = t125 + t517 + t1250 - 0.2D1 * t1247 * t638 - 0.2D1 * t1247
     # * t768 + 0.2D1 * t703 * t517 - t544 * t768 - 0.2D1 * t181 * t638 
     #+ t181 * t1272 - t542 + t545 + t547 - x3 - z - t729 + t730 + t638
      t1295 = 0.1D1 / (t1282 + t1293)
      t1296 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1264, -t1254,
     # -t1258, t1241, t767)
      t1299 = t75 * t150 * t200
      t1300 = t1295 * t1296 * t1299
      t1303 = FJET(XB1, XB2, s, t1241, -t1254, -t1258, t1264, t767, t126
     #6 * t1300 / 0.8D1)
      t1305 = t9 * t518
      t1309 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1264, -t1254,
     # -t1258, t1241, t767)
      t1311 = t1295 * t1309 * t1299
      t1314 = FJET(XB1, XB2, s, t1264, -t1258, -t1254, t1241, t767, t126
     #6 * t1311 / 0.8D1)
      t1319 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0
     #.0D0, 0.0D0, 0.0D0)
      t1325 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0
     #.0D0, 0.0D0, 0.0D0)
      t1326 = t640 * t1325
      t1331 = t650 * t1319
      t1333 = 0.180D3 * t219 * t1331
      t1347 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, t630, -t633, 0
     #.0D0, 0.0D0, 0.0D0)
      t1365 = t682 * t1319 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(-t690 * t1319 + t1326) - t1333) * t150 * t200 / 0.720D3 + (0.90D2
     # * t123 * t8 * (t1326 - t641 * t1319) - t1333) * t75 * t150 / 0.14
     #40D4 + (0.90D2 * t123 * t8 * (t640 * t1347 + t664 * t1319 / 0.2D1 
     #- t667 * t1325) - 0.180D3 * t142 * t9 * (t1326 - t667 * t1319) + t
     #173 * t1331) * t150 / 0.1440D4
      t1366 = FJET(XB1, XB2, s, -t633, 0.0D0, t630, 0.0D0, 0.0D0, t1365)
      t1368 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, 
     #t515, 0.0D0, -t525)
      t1370 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, 
     #t515, 0.0D0, -t525)
      t1371 = t23 * t1370
      t1383 = -t518 * t1368 * t557 - t23 * t1368
      t1400 = t174 * t1368
      t1410 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t521, 
     #t515, 0.0D0, -t525)
      t1424 = (0.90D2 * t123 * t8 * (t565 * t1368 - t1371 - (t518 * t137
     #0 - t536 * t1368) * t557) - 0.180D3 * t142 * t9 * t1383) * t75 * t
     #200 / 0.1440D4 + t203 * t1383 * t75 * t205 / 0.8D1 + (0.90D2 * t12
     #3 * t8 * (-t1371 + t590 * t1368) + 0.180D3 * t219 * t1400) * t150 
     #* t200 / 0.720D3 + (0.90D2 * t226 * t9 * (t607 * t1370 - t608 * t1
     #368 / 0.2D1 - t1410) - 0.180D3 * t239 * t9 * (-t1370 + t607 * t136
     #8) - t173 * t1400) * t200 / 0.1440D4
      t1425 = FJET(XB1, XB2, s, -t521, 0.0D0, 0.0D0, t515, -t525, t1424)
      t1427 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t
     #515, 0.0D0, t767)
      t1432 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t765, t764, t
     #515, 0.0D0, t767)
      t1446 = t773 * t1427 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(t771 * t1432 - t787 * t1427) - 0.180D3 * t219 * t772 * t1427) * t
     #150 * t200 / 0.720D3
      t1447 = FJET(XB1, XB2, s, -t765, t515, t764, 0.0D0, t767, t1446)
      t1449 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0
     #.0D0, 0.0D0, 0.0D0)
      t1454 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t719, t714, 0
     #.0D0, 0.0D0, 0.0D0)
      t1468 = t754 * t1449 * t75 * t205 / 0.8D1 + (0.90D2 * t123 * t8 * 
     #(t736 * t1454 - t737 * t1449) - 0.180D3 * t219 * t746 * t1449) * t
     #75 * t150 / 0.1440D4
      t1469 = FJET(XB1, XB2, s, -t719, 0.0D0, t714, 0.0D0, 0.0D0, t1468)
      t1471 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1264, -t1254,
     # -t1258, t1241, t767)
      t1473 = t1295 * t1471 * t1299
      t1476 = FJET(XB1, XB2, s, -t1258, t1264, t1241, -t1254, t767, t126
     #6 * t1473 / 0.8D1)
      t1481 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1264, -t1254,
     # -t1258, t1241, t767)
      t1483 = t1295 * t1481 * t1299
      t1486 = FJET(XB1, XB2, s, -t1254, t1241, t1264, -t1258, t767, t126
     #6 * t1483 / 0.8D1)
      t1491 = t1145 * t1144 + t1194 * t1193 + t1216 * t1215 + t1238 * t1
     #237 + t1303 * pi * t1305 * t1300 / 0.8D1 + t1314 * pi * t1305 * t1
     #311 / 0.8D1 + t1366 * t1365 + t1425 * t1424 + t1447 * t1446 + t146
     #9 * t1468 + t1476 * pi * t1305 * t1473 / 0.8D1 + t1486 * pi * t130
     #5 * t1483 / 0.8D1
      rrqg2qght6s1e0 = t1124 + t1491

      end function



      doubleprecision function rrqg2qght6s1em1
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = 0.1D1 / z
      t24 = -0.1D1 + x3
      t29 = log(-0.4D1 * t13 * t18 / t24)
      t30 = cos(t10)
      t32 = Sqrt(-x3 * t24)
      t37 = 0.1D1 / (-x3 - z + 0.2D1 * t30 * t32 * z)
      t40 = t9 * (-t21 * t22 - t29 * t37)
      t45 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t51 = t8 * (t22 + t37)
      t54 = 0.1D1 / x3
      t57 = t22 * t45
      t61 = log(0.4D1 * t15 * t12 * t17)
      t62 = t61 * t22
      t68 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t71 = t61 ** 2
      t72 = t71 * t22
      t78 = t22 * t3
      t79 = lh ** 2
      t81 = pi ** 2
      t84 = pi * (0.180D3 * t79 - 0.30D2 * t81)
      t90 = pi * t5
      t91 = t90 * t8
      t94 = (t3 * t37 + t78) * t54
      t95 = 0.1D1 / x2
      t99 = x2 ** 2
      t100 = t99 * t12
      t103 = log(0.4D1 * t100 * t18)
      t104 = t103 * t22
      t111 = pi * lh * t5
      t112 = t8 * t22
      t115 = 0.180D3 * t111 * t112 * t3
      t119 = 0.1D1 / x1
      t120 = t95 * t119
      t124 = t22 * pi
      t125 = x1 ** 2
      t126 = t125 * t12
      t129 = log(0.4D1 * t126 * t18)
      t141 = (0.90D2 * t4 * t40 + (-0.180D3 * t4 * lh + 0.90D2 * t45 * p
     #i) * t5 * t51) * t54 / 0.2880D4 + (-0.180D3 * (t57 - t62 * t3) * p
     #i * lh + 0.90D2 * (t22 * t68 - t62 * t45 + t72 * t3 / 0.2D1) * pi 
     #+ t78 * t84) * t5 * t8 / 0.2880D4 + t91 * t94 * t95 / 0.16D2 + (0.
     #90D2 * t90 * t8 * (t57 - t104 * t3) - t115) * t95 / 0.1440D4 + t91
     # * t78 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (-t129 * t3 + t45) -
     # t115) * t119 / 0.1440D4 + t91 * t94 * t119 / 0.16D2
      t142 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t141)
      t144 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t145 = t144 * pi
      t150 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t159 = t22 * t150
      t165 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t173 = t22 * t144
      t181 = (t144 * t37 + t173) * t54
      t192 = 0.180D3 * t111 * t112 * t144
      t210 = (0.90D2 * t145 * t40 + (-0.180D3 * t145 * lh + 0.90D2 * t15
     #0 * pi) * t5 * t51) * t54 / 0.2880D4 + (-0.180D3 * (t159 - t62 * t
     #144) * pi * lh + 0.90D2 * (t22 * t165 - t62 * t150 + t72 * t144 / 
     #0.2D1) * pi + t173 * t84) * t5 * t8 / 0.2880D4 + t91 * t181 * t95 
     #/ 0.16D2 + (0.90D2 * t90 * t8 * (t159 - t104 * t144) - t192) * t95
     # / 0.1440D4 + t91 * t173 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (t
     #150 - t129 * t144) - t192) * t119 / 0.1440D4 + t91 * t181 * t119 /
     # 0.16D2
      t211 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t210)
      t213 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t214 = t213 * pi
      t219 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t228 = t22 * t219
      t234 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t242 = t22 * t213
      t250 = (t213 * t37 + t242) * t54
      t261 = 0.180D3 * t111 * t112 * t213
      t279 = (0.90D2 * t214 * t40 + (-0.180D3 * t214 * lh + 0.90D2 * t21
     #9 * pi) * t5 * t51) * t54 / 0.2880D4 + (-0.180D3 * (t228 - t62 * t
     #213) * pi * lh + 0.90D2 * (t22 * t234 - t62 * t219 + t72 * t213 / 
     #0.2D1) * pi + t242 * t84) * t5 * t8 / 0.2880D4 + t91 * t250 * t95 
     #/ 0.16D2 + (0.90D2 * t90 * t8 * (t228 - t104 * t213) - t261) * t95
     # / 0.1440D4 + t91 * t242 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (-
     #t129 * t213 + t219) - t261) * t119 / 0.1440D4 + t91 * t250 * t119 
     #/ 0.16D2
      t280 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t279)
      t282 = t2 * x1
      t283 = -0.1D1 + x1
      t284 = x1 * z
      t285 = 0.1D1 - x1 + t284
      t286 = 0.1D1 / t285
      t288 = t2 * t283 * t286
      t289 = s * t16
      t291 = t283 * x1 * t286
      t292 = t289 * t291
      t293 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t294 = t22 * t293
      t298 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t301 = t283 ** 2
      t305 = log(0.4D1 * t126 * t15 * t17 * t286 * t301)
      t318 = x3 * x1
      t324 = x3 * t125
      t331 = Sqrt(-x3 * t285 * t24)
      t335 = -z + t284 - 0.3D1 * t318 * z - x1 * t14 + x3 * t14 * x1 + 0
     #.2D1 * t324 * z - t324 * t14 - t324 + 0.2D1 * t318 + 0.2D1 * t30 *
     # t331 * z - x3
      t336 = 0.1D1 / t335
      t343 = -t91 * t294 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (-t298 +
     # t305 * t293) + 0.180D3 * t111 * t112 * t293) * t119 / 0.1440D4 + 
     #t91 * (-t294 - t285 * t293 * t336) * t54 * t119 / 0.16D2
      t344 = FJET(XB1, XB2, s, 0.0D0, t282, -t288, 0.0D0, -t292, t343)
      t346 = x2 * s
      t347 = t346 * t1
      t348 = -0.1D1 + x2
      t349 = t348 * s
      t350 = t349 * t1
      t351 = x2 * z
      t353 = 0.1D1 / (-x2 + t351 - z)
      t354 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t355 = t353 * t354
      t356 = t54 * t95
      t363 = log(-0.4D1 * t100 * t18 * t348)
      t364 = t363 * t353
      t366 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t372 = t8 * t353
      t382 = t91 * t355 * t356 / 0.16D2 + (0.90D2 * t90 * t8 * (-t364 * 
     #t354 + t353 * t366) - 0.180D3 * t111 * t372 * t354) * t95 / 0.1440
     #D4 + t91 * t355 * t120 / 0.8D1
      t383 = FJET(XB1, XB2, s, 0.0D0, t347, 0.0D0, -t350, 0.0D0, t382)
      t385 = x2 * x3
      t386 = t99 * x3
      t389 = Sqrt(x3 * t348 * t24)
      t390 = t30 * t389
      t392 = 0.2D1 * t390 * x2
      t395 = 0.1D1 / (0.1D1 - x3 + t385)
      t397 = t2 * (0.1D1 - x3 - x2 + t385 + t386 + t392) * t395
      t402 = t2 * x2 * (-0.1D1 + t385 + 0.2D1 * t390) * t395
      t410 = 0.1D1 / (-t386 + x3 + x2 + t386 * z - t351 - t385 * z + z -
     # t392 - 0.2D1 * t390 * z + 0.2D1 * t390 * t351)
      t411 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t402, t397, 0.
     #0D0, 0.0D0, 0.0D0)
      t413 = t410 * t411 * t356
      t416 = FJET(XB1, XB2, s, 0.0D0, t397, 0.0D0, -t402, 0.0D0, t91 * t
     #413 / 0.16D2)
      t421 = t1 * t283
      t423 = t349 * t421 * t286
      t424 = t346 * t421
      t426 = t289 * t348 * t291
      t427 = x2 * x1
      t430 = 0.1D1 / (t427 * z + z - t351 - t427 + x2)
      t431 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t424, t423, t2
     #82, 0.0D0, t426)
      t433 = t430 * t431 * t120
      t436 = FJET(XB1, XB2, s, 0.0D0, t423, t282, -t424, t426, t91 * t43
     #3 / 0.8D1)
      t441 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t442 = t353 * t441
      t447 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t462 = t91 * t442 * t356 / 0.16D2 + (0.90D2 * t90 * t8 * (-t364 * 
     #t441 + t353 * t447) - 0.180D3 * t111 * t372 * t441) * t95 / 0.1440
     #D4 + t91 * t442 * t120 / 0.8D1
      t463 = FJET(XB1, XB2, s, 0.0D0, -t350, 0.0D0, t347, 0.0D0, t462)
      t465 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t466 = t22 * t465
      t471 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t489 = -t91 * t466 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (t305 * 
     #t465 - t471) + 0.180D3 * t111 * t112 * t465) * t119 / 0.1440D4 + t
     #91 * (-t285 * t465 * t336 - t466) * t54 * t119 / 0.16D2
      t490 = FJET(XB1, XB2, s, 0.0D0, -t288, t282, 0.0D0, -t292, t489)
      t492 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t402, t397, 0.
     #0D0, 0.0D0, 0.0D0)
      t494 = t410 * t492 * t356
      t497 = FJET(XB1, XB2, s, 0.0D0, -t402, 0.0D0, t397, 0.0D0, t91 * t
     #494 / 0.16D2)
      t502 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t503 = t22 * t502
      t504 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t510 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t518 = t22 * t504
      t526 = (t518 + t504 * t37) * t54
      t537 = 0.180D3 * t111 * t112 * t504
      t541 = t504 * pi
      t568 = (-0.180D3 * (t503 - t62 * t504) * pi * lh + 0.90D2 * (t22 *
     # t510 - t62 * t502 + t72 * t504 / 0.2D1) * pi + t518 * t84) * t5 *
     # t8 / 0.2880D4 + t91 * t526 * t95 / 0.16D2 + (0.90D2 * t90 * t8 * 
     #(t503 - t104 * t504) - t537) * t95 / 0.1440D4 + (0.90D2 * t541 * t
     #40 + (-0.180D3 * t541 * lh + 0.90D2 * t502 * pi) * t5 * t51) * t54
     # / 0.2880D4 + t91 * t518 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (t
     #502 - t129 * t504) - t537) * t119 / 0.1440D4 + t91 * t526 * t119 /
     # 0.16D2
      t569 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t568)
      t571 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t572 = t22 * t571
      t576 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t595 = -t91 * t572 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (-t576 +
     # t305 * t571) + 0.180D3 * t111 * t112 * t571) * t119 / 0.1440D4 + 
     #t91 * (-t285 * t571 * t336 - t572) * t54 * t119 / 0.16D2
      t596 = FJET(XB1, XB2, s, t282, 0.0D0, 0.0D0, -t288, -t292, t595)
      t598 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t424, t423, t2
     #82, 0.0D0, t426)
      t600 = t430 * t598 * t120
      t603 = FJET(XB1, XB2, s, t282, -t424, 0.0D0, t423, t426, t91 * t60
     #0 / 0.8D1)
      t608 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t609 = t353 * t608
      t616 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t629 = t91 * t609 * t120 / 0.8D1 + t91 * t609 * t356 / 0.16D2 + (0
     #.90D2 * t90 * t8 * (t353 * t616 - t364 * t608) - 0.180D3 * t111 * 
     #t372 * t608) * t95 / 0.1440D4
      t630 = FJET(XB1, XB2, s, t347, 0.0D0, -t350, 0.0D0, 0.0D0, t629)
      t632 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t402, t397, 0.
     #0D0, 0.0D0, 0.0D0)
      t634 = t410 * t632 * t356
      t637 = FJET(XB1, XB2, s, t397, 0.0D0, -t402, 0.0D0, 0.0D0, t91 * t
     #634 / 0.16D2)
      t642 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t424, t423, t2
     #82, 0.0D0, t426)
      t644 = t430 * t642 * t120
      t647 = FJET(XB1, XB2, s, t423, 0.0D0, -t424, t282, t426, t91 * t64
     #4 / 0.8D1)
      t652 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t653 = t353 * t652
      t660 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t347, -t350, 0.
     #0D0, 0.0D0, 0.0D0)
      t673 = t91 * t653 * t120 / 0.8D1 + t91 * t653 * t356 / 0.16D2 + (0
     #.90D2 * t90 * t8 * (t353 * t660 - t364 * t652) - 0.180D3 * t111 * 
     #t372 * t652) * t95 / 0.1440D4
      t674 = FJET(XB1, XB2, s, -t350, 0.0D0, t347, 0.0D0, 0.0D0, t673)
      t676 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t677 = t22 * t676
      t681 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t288, t
     #282, 0.0D0, -t292)
      t700 = -t91 * t677 * t120 / 0.8D1 + (0.90D2 * t124 * t9 * (-t681 +
     # t305 * t676) + 0.180D3 * t111 * t112 * t676) * t119 / 0.1440D4 + 
     #t91 * (-t285 * t676 * t336 - t677) * t54 * t119 / 0.16D2
      t701 = FJET(XB1, XB2, s, -t288, 0.0D0, 0.0D0, t282, -t292, t700)
      t703 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t424, t423, t2
     #82, 0.0D0, t426)
      t705 = t430 * t703 * t120
      t708 = FJET(XB1, XB2, s, -t424, t282, t423, 0.0D0, t426, t91 * t70
     #5 / 0.8D1)
      t713 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t402, t397, 0.
     #0D0, 0.0D0, 0.0D0)
      t715 = t410 * t713 * t356
      t718 = FJET(XB1, XB2, s, -t402, 0.0D0, t397, 0.0D0, 0.0D0, t91 * t
     #715 / 0.16D2)
      rrqg2qght6s1em1 = t142 * t141 + t211 * t210 + t280 * t279 + t344 *
     # t343 + t383 * t382 + t416 * pi * t9 * t413 / 0.16D2 + t436 * pi *
     # t9 * t433 / 0.8D1 + t463 * t462 + t490 * t489 + t497 * pi * t9 * 
     #t494 / 0.16D2 + t569 * t568 + t596 * t595 + t603 * pi * t9 * t600 
     #/ 0.8D1 + t630 * t629 + t637 * pi * t9 * t634 / 0.16D2 + t647 * pi
     # * t9 * t644 / 0.8D1 + t674 * t673 + t701 * t700 + t708 * pi * t9 
     #* t705 / 0.8D1 + t718 * pi * t9 * t715 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s1em2
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t5 = 0.1D1 / t1
      t7 = s ** 2
      t9 = 0.1D1 / t7 / s
      t10 = 0.1D1 / z
      t11 = x4 * pi
      t12 = cos(t11)
      t15 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = t9 * (t10 + 0.1D1 / (-x3 - z + 0.2D1 * t12 * t15 * z)) / x3
      t28 = pi * t5 * t9
      t29 = t10 * t3
      t30 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t38 = pi * lh
      t41 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t43 = z ** 2
      t45 = Sin(t11)
      t46 = t45 ** 2
      t48 = t1 ** 2
      t49 = t48 ** 2
      t52 = log(0.4D1 / t43 * t46 * t49)
      t53 = t52 * t10
      t62 = t3 * pi * t5 * t24 / 0.32D2 + t28 * t29 * t30 / 0.16D2 + t28
     # * t29 * t34 / 0.16D2 + (-0.180D3 * t29 * t38 + 0.90D2 * (t10 * t4
     #1 - t53 * t3) * pi) * t5 * t9 / 0.2880D4
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t62)
      t65 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t70 = t10 * t65
      t79 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t89 = t65 * pi * t5 * t24 / 0.32D2 + t28 * t70 * t30 / 0.16D2 + t2
     #8 * t70 * t34 / 0.16D2 + (-0.180D3 * t70 * t38 + 0.90D2 * (t10 * t
     #79 - t53 * t65) * pi) * t5 * t9 / 0.2880D4
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t89)
      t92 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t97 = t10 * t92
      t106 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t116 = t92 * pi * t5 * t24 / 0.32D2 + t28 * t97 * t30 / 0.16D2 + t
     #28 * t97 * t34 / 0.16D2 + (-0.180D3 * t97 * t38 + 0.90D2 * (t10 * 
     #t106 - t53 * t92) * pi) * t5 * t9 / 0.2880D4
      t117 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t116)
      t119 = t2 * x1
      t120 = -0.1D1 + x1
      t123 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t125 = t2 * t120 * t123
      t129 = s * t48 * t120 * x1 * t123
      t130 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t125, t
     #119, 0.0D0, -t129)
      t135 = FJET(XB1, XB2, s, 0.0D0, t119, -t125, 0.0D0, -t129, -t28 * 
     #t10 * t130 * t34 / 0.16D2)
      t138 = t9 * t10
      t144 = x2 * s * t1
      t147 = (-0.1D1 + x2) * s * t1
      t150 = 0.1D1 / (-x2 + x2 * z - z)
      t151 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t147, 0.
     #0D0, 0.0D0, 0.0D0)
      t156 = FJET(XB1, XB2, s, 0.0D0, t144, 0.0D0, -t147, 0.0D0, t28 * t
     #150 * t151 * t30 / 0.16D2)
      t159 = t9 * t150
      t164 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t147, 0.
     #0D0, 0.0D0, 0.0D0)
      t169 = FJET(XB1, XB2, s, 0.0D0, -t147, 0.0D0, t144, 0.0D0, t28 * t
     #150 * t164 * t30 / 0.16D2)
      t176 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t125, t
     #119, 0.0D0, -t129)
      t181 = FJET(XB1, XB2, s, 0.0D0, -t125, t119, 0.0D0, -t129, -t28 * 
     #t10 * t176 * t34 / 0.16D2)
      t188 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t193 = t10 * t188
      t202 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t212 = t188 * pi * t5 * t24 / 0.32D2 + t28 * t193 * t30 / 0.16D2 +
     # t28 * t193 * t34 / 0.16D2 + (-0.180D3 * t193 * t38 + 0.90D2 * (t1
     #0 * t202 - t53 * t188) * pi) * t5 * t9 / 0.2880D4
      t213 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t212)
      t215 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t125, t
     #119, 0.0D0, -t129)
      t220 = FJET(XB1, XB2, s, t119, 0.0D0, 0.0D0, -t125, -t129, -t28 * 
     #t10 * t215 * t34 / 0.16D2)
      t227 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t147, 0.
     #0D0, 0.0D0, 0.0D0)
      t232 = FJET(XB1, XB2, s, t144, 0.0D0, -t147, 0.0D0, 0.0D0, t28 * t
     #150 * t227 * t30 / 0.16D2)
      t239 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t147, 0.
     #0D0, 0.0D0, 0.0D0)
      t244 = FJET(XB1, XB2, s, -t147, 0.0D0, t144, 0.0D0, 0.0D0, t28 * t
     #150 * t239 * t30 / 0.16D2)
      t253 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t125, t
     #119, 0.0D0, -t129)
      t258 = FJET(XB1, XB2, s, -t125, 0.0D0, 0.0D0, t119, -t129, -t10 * 
     #pi * t5 * t9 * t253 * t34 / 0.16D2)
      rrqg2qght6s1em2 = t63 * t62 + t90 * t89 + t117 * t116 - t135 * pi 
     #* t5 * t138 * t130 * t34 / 0.16D2 + t156 * pi * t5 * t159 * t151 *
     # t30 / 0.16D2 + t169 * pi * t5 * t159 * t164 * t30 / 0.16D2 - t181
     # * pi * t5 * t138 * t176 * t34 / 0.16D2 + t213 * t212 - t220 * pi 
     #* t5 * t138 * t215 * t34 / 0.16D2 + t232 * pi * t5 * t159 * t227 *
     # t30 / 0.16D2 + t244 * pi * t5 * t159 * t239 * t30 / 0.16D2 - t258
     # * t10 * pi * t5 * t9 * t253 * t34 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s1em3
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = 0.1D1 / t1
      t4 = t3 * pi
      t5 = s ** 2
      t9 = 0.1D1 / t5 / s / z
      t10 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t11 = t9 * t10
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t34 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t35 = t9 * t34
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t35 /
     # 0.32D2)
      rrqg2qght6s1em3 = t14 * pi * t3 * t11 / 0.32D2 + t22 * pi * t3 * t
     #19 / 0.32D2 + t30 * pi * t3 * t27 / 0.32D2 + t38 * pi * t3 * t35 /
     # 0.32D2

      end function



      doubleprecision function rrqg2qght6s1em4
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght6s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght6s2e1
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t12 = pi * lh
      t13 = t3 * t7
      t14 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t15 = t13 * t14
      t18 = lh ** 2
      t20 = pi ** 2
      t22 = -0.180D3 * t18 + 0.30D2 * t20
      t23 = pi * t22
      t24 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t25 = t13 * t24
      t28 = z ** 2
      t30 = 0.1D1 / t28 / z
      t31 = x3 * t30
      t32 = x4 * pi
      t33 = Sin(t32)
      t34 = t33 ** 2
      t35 = t1 ** 2
      t36 = t35 ** 2
      t37 = t34 * t36
      t40 = log(0.4D1 * t31 * t37)
      t41 = 0.1D1 / z
      t43 = -0.1D1 + x3
      t44 = 0.1D1 / t43
      t45 = t37 * t44
      t48 = log(-0.4D1 * t31 * t45)
      t49 = cos(t32)
      t50 = x3 * z
      t52 = Sqrt(-t50 * t43)
      t56 = 0.1D1 / (-z - x3 + 0.2D1 * t49 * t52)
      t58 = t40 * t41 + t48 * t56
      t60 = t7 * t24
      t61 = t48 ** 2
      t64 = t40 ** 2
      t68 = t61 * t48 * t56 / 0.6D1 + t64 * t40 * t41 / 0.6D1
      t73 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t74 = t7 * t73
      t82 = -0.60D2 * lh * t20 + 0.240D3 * zeta3 + 0.120D3 * t18 * lh
      t83 = pi * t82
      t85 = t13 * t8
      t89 = -t41 - t56
      t91 = t7 * t14
      t100 = -t64 * t41 / 0.2D1 - t61 * t56 / 0.2D1
      t103 = 0.1D1 / x3
      t106 = x2 ** 2
      t107 = x3 * t106
      t108 = t107 * t30
      t109 = -0.1D1 + x2
      t110 = t37 * t109
      t113 = log(-0.4D1 * t108 * t110)
      t114 = t113 * t41
      t116 = t113 ** 2
      t117 = t116 * t41
      t122 = log(-0.4D1 * t108 * t45)
      t124 = t122 ** 2
      t129 = t30 * t34
      t130 = t129 * t36
      t133 = log(0.4D1 * t107 * t130)
      t134 = t133 * t41
      t136 = t133 ** 2
      t137 = t136 * t41
      t153 = t23 * t3
      t154 = t60 * t56
      t158 = 0.1D1 / x2
      t161 = t41 * pi
      t164 = t161 * lh
      t168 = t30 * t106
      t171 = log(0.4D1 * t168 * t37)
      t172 = t171 ** 2
      t175 = log(-0.4D1 * t168 * t110)
      t176 = t175 ** 2
      t178 = t172 / 0.2D1 - t176 / 0.2D1
      t180 = t161 * t3
      t184 = t176 * t175 / 0.6D1 - t172 * t171 / 0.6D1
      t192 = t161 * t22
      t195 = -t171 + t175
      t200 = rrqg2qgh62J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t205 = log(0.4D1 * t130)
      t206 = t205 ** 2
      t207 = t206 * pi
      t208 = t41 * lh
      t211 = t161 * t82
      t213 = t206 * t205 * pi
      t216 = t205 * pi
      t217 = t41 * t22
      t220 = (0.90D2 * t207 * t208 + t211 + 0.15D2 * t213 * t41 - t216 *
     # t217) * t3
      t228 = (-0.180D3 * t216 * t208 - 0.45D2 * t207 * t41 + t192) * t3
      t235 = (0.180D3 * t164 + 0.90D2 * t216 * t41) * t3
      t238 = x1 ** 2
      t239 = x3 * t238
      t240 = t239 * t34
      t241 = t30 * t36
      t245 = log(-0.4D1 * t240 * t241 * t44)
      t247 = t245 ** 2
      t255 = log(0.4D1 * t239 * t130)
      t256 = t255 * t41
      t258 = t255 ** 2
      t259 = t258 * t41
      t282 = 0.1D1 / x1
      t285 = t238 * t34
      t288 = log(0.4D1 * t285 * t241)
      t294 = t288 ** 2
      t297 = t294 * t288
      t315 = t107 * t238
      t320 = log(-0.4D1 * t315 * t129 * t36 * t44)
      t326 = log(0.4D1 * t315 * t130)
      t327 = t326 * t41
      t329 = t36 * t109
      t333 = log(-0.4D1 * t315 * t129 * t329)
      t334 = t333 * t41
      t340 = t12 * t3
      t345 = t158 * t282
      t348 = t106 * t238
      t351 = log(0.4D1 * t348 * t130)
      t352 = t351 ** 2
      t353 = t352 * t41
      t356 = t351 * t41
      t358 = t348 * t34
      t362 = log(-0.4D1 * t358 * t241 * t109)
      t363 = t362 ** 2
      t364 = t363 * t41
      t367 = t362 * t41
      t391 = t20 ** 2
      t392 = t18 ** 2
      t398 = t206 ** 2
      t403 = (-0.30D2 * t213 * t208 + t207 * t217 / 0.2D1 - t216 * t41 *
     # t82 + t161 * (-0.480D3 * lh * zeta3 - t391 - 0.60D2 * t392 + 0.60
     #D2 * t18 * t20) - 0.15D2 / 0.4D1 * t398 * pi * t41) * t3
      t406 = ((-0.90D2 * t4 * t9 + 0.180D3 * t12 * t15 + t23 * t25) * t5
     #8 - 0.90D2 * t4 * t60 * t68 + (t23 * t15 - 0.90D2 * t4 * t74 + t83
     # * t25 + 0.180D3 * t12 * t85) * t89 + (-0.90D2 * t4 * t91 + 0.180D
     #3 * t12 * t25) * t100) * t103 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-
     #t114 * t14 + t117 * t24 / 0.2D1 - (-t122 * t14 + t8 + t124 * t24 /
     # 0.2D1) * t56 + t134 * t14 - t137 * t24 / 0.2D1) + 0.180D3 * t12 *
     # t13 * (-(t14 - t122 * t24) * t56 - t114 * t24 + t134 * t24) - t15
     #3 * t154) * t103 * t158 / 0.1440D4 - ((-0.90D2 * t161 * t15 + 0.18
     #0D3 * t164 * t25) * t178 - 0.90D2 * t180 * t60 * t184 + (-0.90D2 *
     # t161 * t85 + 0.180D3 * t164 * t15 + t192 * t25) * t195) * t158 / 
     #0.1440D4 + t161 * t13 * t200 / 0.32D2 - t220 * t91 / 0.2880D4 - t2
     #28 * t9 / 0.2880D4 - t235 * t74 / 0.2880D4 + (-0.90D2 * t4 * t7 * 
     #(-(-t245 * t14 + t8 + t247 * t24 / 0.2D1) * t56 - t41 * t8 + t256 
     #* t14 - t259 * t24 / 0.2D1) + 0.180D3 * t12 * t13 * (-(t14 - t245 
     #* t24) * t56 - t41 * t14 + t256 * t24) + t23 * t13 * (-t24 * t56 -
     # t41 * t24)) * t103 * t282 / 0.1440D4 + (t192 * t13 * (-t14 + t288
     # * t24) - 0.90D2 * t161 * t13 * (t288 * t8 - t294 * t14 / 0.2D1 + 
     #t297 * t24 / 0.6D1 - t73) - t211 * t25 + 0.180D3 * t164 * t13 * (t
     #288 * t14 - t8 - t294 * t24 / 0.2D1)) * t282 / 0.1440D4 - (-0.90D2
     # * t4 * t7 * ((t14 - t320 * t24) * t56 - t327 * t24 + t334 * t24) 
     #+ 0.180D3 * t340 * t154) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 *
     # t7 * (t353 * t24 / 0.2D1 - t356 * t14 - t364 * t24 / 0.2D1 + t367
     # * t14) + 0.180D3 * t12 * t13 * (-t356 * t24 + t367 * t24)) * t158
     # * t282 / 0.720D3 - t403 * t60 / 0.2880D4
      t407 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t406)
      t409 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t410 = t7 * t409
      t413 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t414 = t7 * t413
      t417 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t418 = t13 * t417
      t421 = t13 * t409
      t429 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t430 = t7 * t429
      t434 = t13 * t413
      t439 = t7 * t417
      t473 = t410 * t56
      t498 = rrqg2qgh63J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t598 = -t403 * t410 / 0.2880D4 + ((-0.90D2 * t4 * t414 + 0.180D3 *
     # t12 * t418 + t23 * t421) * t58 - 0.90D2 * t4 * t410 * t68 + (t23 
     #* t418 - 0.90D2 * t4 * t430 + t83 * t421 + 0.180D3 * t12 * t434) *
     # t89 + (-0.90D2 * t4 * t439 + 0.180D3 * t12 * t421) * t100) * t103
     # / 0.2880D4 + (-0.90D2 * t4 * t7 * (t134 * t417 - (t124 * t409 / 0
     #.2D1 + t413 - t122 * t417) * t56 + t117 * t409 / 0.2D1 - t114 * t4
     #17 - t137 * t409 / 0.2D1) + 0.180D3 * t12 * t13 * (-(t417 - t122 *
     # t409) * t56 - t114 * t409 + t134 * t409) - t153 * t473) * t103 * 
     #t158 / 0.1440D4 - ((-0.90D2 * t161 * t418 + 0.180D3 * t164 * t421)
     # * t178 - 0.90D2 * t180 * t410 * t184 + (-0.90D2 * t161 * t434 + 0
     #.180D3 * t164 * t418 + t192 * t421) * t195) * t158 / 0.1440D4 + t1
     #61 * t13 * t498 / 0.32D2 - t220 * t439 / 0.2880D4 - t228 * t414 / 
     #0.2880D4 + (-0.90D2 * t4 * t7 * (-(t247 * t409 / 0.2D1 + t413 - t2
     #45 * t417) * t56 - t41 * t413 + t256 * t417 - t259 * t409 / 0.2D1)
     # + 0.180D3 * t12 * t13 * (-(t417 - t245 * t409) * t56 - t41 * t417
     # + t256 * t409) + t23 * t13 * (-t409 * t56 - t41 * t409)) * t103 *
     # t282 / 0.1440D4 + (t192 * t13 * (-t417 + t288 * t409) - 0.90D2 * 
     #t161 * t13 * (t297 * t409 / 0.6D1 - t429 - t294 * t417 / 0.2D1 + t
     #288 * t413) - t211 * t421 + 0.180D3 * t164 * t13 * (-t294 * t409 /
     # 0.2D1 - t413 + t288 * t417)) * t282 / 0.1440D4 - (-0.90D2 * t4 * 
     #t7 * ((t417 - t320 * t409) * t56 - t327 * t409 + t334 * t409) + 0.
     #180D3 * t340 * t473) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 * t7 
     #* (-t364 * t409 / 0.2D1 + t367 * t417 + t353 * t409 / 0.2D1 - t356
     # * t417) + 0.180D3 * t12 * t13 * (t367 * t409 - t356 * t409)) * t1
     #58 * t282 / 0.720D3 - t235 * t430 / 0.2880D4
      t599 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t598)
      t601 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t602 = t7 * t601
      t605 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t606 = t7 * t605
      t609 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t610 = t7 * t609
      t613 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t614 = t13 * t613
      t617 = t13 * t605
      t628 = t13 * t609
      t633 = t7 * t613
      t667 = t606 * t56
      t692 = rrqg2qgh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t790 = -t235 * t602 / 0.2880D4 - t403 * t606 / 0.2880D4 + ((-0.90D
     #2 * t4 * t610 + 0.180D3 * t12 * t614 + t23 * t617) * t58 - 0.90D2 
     #* t4 * t606 * t68 + (t23 * t614 - 0.90D2 * t4 * t602 + t83 * t617 
     #+ 0.180D3 * t12 * t628) * t89 + (-0.90D2 * t4 * t633 + 0.180D3 * t
     #12 * t617) * t100) * t103 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-(t12
     #4 * t605 / 0.2D1 + t609 - t122 * t613) * t56 - t114 * t613 + t117 
     #* t605 / 0.2D1 + t134 * t613 - t137 * t605 / 0.2D1) + 0.180D3 * t1
     #2 * t13 * (-(t613 - t122 * t605) * t56 - t114 * t605 + t134 * t605
     #) - t153 * t667) * t103 * t158 / 0.1440D4 - ((-0.90D2 * t161 * t61
     #4 + 0.180D3 * t164 * t617) * t178 - 0.90D2 * t180 * t606 * t184 + 
     #(-0.90D2 * t161 * t628 + 0.180D3 * t164 * t614 + t192 * t617) * t1
     #95) * t158 / 0.1440D4 + t161 * t13 * t692 / 0.32D2 - t220 * t633 /
     # 0.2880D4 + (-0.90D2 * t4 * t7 * (-(t247 * t605 / 0.2D1 + t609 - t
     #245 * t613) * t56 - t41 * t609 + t256 * t613 - t259 * t605 / 0.2D1
     #) + 0.180D3 * t12 * t13 * (-(t613 - t245 * t605) * t56 + t256 * t6
     #05 - t41 * t613) + t23 * t13 * (-t605 * t56 - t41 * t605)) * t103 
     #* t282 / 0.1440D4 + (t192 * t13 * (-t613 + t288 * t605) - 0.90D2 *
     # t161 * t13 * (t297 * t605 / 0.6D1 - t601 + t288 * t609 - t294 * t
     #613 / 0.2D1) - t211 * t617 + 0.180D3 * t164 * t13 * (-t294 * t605 
     #/ 0.2D1 - t609 + t288 * t613)) * t282 / 0.1440D4 - (-0.90D2 * t4 *
     # t7 * ((t613 - t320 * t605) * t56 + t334 * t605 - t327 * t605) + 0
     #.180D3 * t340 * t667) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 * t7
     # * (-t356 * t613 + t367 * t613 - t364 * t605 / 0.2D1 + t353 * t605
     # / 0.2D1) + 0.180D3 * t12 * t13 * (-t356 * t605 + t367 * t605)) * 
     #t158 * t282 / 0.720D3 - t228 * t610 / 0.2880D4
      t791 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t790)
      t794 = x1 * z
      t795 = -z - x1 + t794
      t796 = 0.1D1 / t795
      t798 = t2 * x1 * t109 * t796
      t799 = -0.1D1 + x1
      t800 = t2 * t799
      t803 = x2 * s * t1 * x1
      t804 = s * t35
      t807 = t799 * x1 * t796
      t808 = t804 * t109 * t807
      t809 = x2 * x1
      t810 = t809 * z
      t812 = 0.1D1 / (-z + t810 - t809)
      t813 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t8
     #00, 0.0D0, -t808)
      t814 = t812 * t813
      t815 = t107 * t285
      t816 = 0.1D1 / t28
      t817 = t816 * t36
      t818 = t799 ** 2
      t819 = t796 * t818
      t824 = log(0.4D1 * t815 * t817 * t819 * t109)
      t825 = t824 * t812
      t826 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t8
     #00, 0.0D0, -t808)
      t832 = t7 * t812
      t833 = t832 * t826
      t839 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t8
     #00, 0.0D0, -t808)
      t841 = t34 * t816
      t843 = t36 * t796
      t848 = log(0.4D1 * t348 * t841 * t843 * t818 * t109)
      t849 = t848 ** 2
      t850 = t849 * t812
      t853 = t848 * t812
      t869 = -(-0.90D2 * t4 * t7 * (-t814 + t825 * t826) - 0.180D3 * t34
     #0 * t833) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 * t7 * (-t812 * 
     #t839 - t850 * t826 / 0.2D1 + t853 * t813) + 0.180D3 * t12 * t13 * 
     #(t853 * t826 - t814) - t153 * t833) * t158 * t282 / 0.720D3
      t870 = FJET(XB1, XB2, s, 0.0D0, t798, -t800, t803, -t808, t869)
      t872 = x2 * x3
      t873 = 0.1D1 - x3 + t872
      t874 = 0.1D1 / t873
      t875 = t872 * t874
      t876 = t2 * t875
      t878 = t2 * t43 * t874
      t880 = t873 ** 2
      t881 = 0.1D1 / t880
      t886 = log(0.4D1 * t107 * t129 * t329 * t43 * t881)
      t887 = t109 * t43
      t889 = Sqrt(t50 * t887)
      t893 = 0.1D1 / (-z - x3 + t872 + 0.2D1 * t49 * t889)
      t894 = t886 * t893
      t895 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t878, t876, 0.0D0)
      t897 = t886 ** 2
      t898 = t897 * t893
      t899 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t878, t876, 0.0D0)
      t902 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t878, t876, 0.0D0)
      t909 = t893 * t895
      t914 = t7 * t893
      t915 = t914 * t899
      t921 = t887 * t881
      t925 = log(0.4D1 * t815 * t241 * t921)
      t926 = t925 * t893
      t938 = (-0.90D2 * t4 * t7 * (-t894 * t895 + t898 * t899 / 0.2D1 + 
     #t893 * t902) + 0.180D3 * t12 * t13 * (-t894 * t899 + t909) + t153 
     #* t915) * t103 * t158 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t926 * t8
     #99 - t909) - 0.180D3 * t340 * t915) * t103 * t345 / 0.720D3
      t939 = FJET(XB1, XB2, s, 0.0D0, t876, 0.0D0, -t878, 0.0D0, t938)
      t942 = t2 * x1 * t796
      t943 = t804 * t807
      t944 = t817 * t819
      t947 = log(-0.4D1 * t240 * t944)
      t948 = t947 * t41
      t949 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, -
     #t800, 0.0D0, t943)
      t951 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, -
     #t800, 0.0D0, t943)
      t952 = t41 * t951
      t958 = log(0.4D1 * t239 * t841 * t843 * t818 * t44)
      t960 = t958 ** 2
      t961 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, -
     #t800, 0.0D0, t943)
      t966 = x3 * x1
      t967 = t966 * z
      t968 = x1 * t28
      t969 = x3 * t28
      t970 = t969 * x1
      t971 = t239 * t28
      t973 = 0.2D1 * t239 * z
      t974 = x3 * t795
      t976 = Sqrt(t974 * t43)
      t981 = 0.1D1 / (-t794 - t967 + t968 + t970 - t239 - t50 - t971 + t
     #973 + 0.2D1 * t49 * t976 * z - t28)
      t983 = t947 ** 2
      t984 = t983 * t41
      t992 = t41 * t949
      t1004 = t41 * t961 - t961 * t795 * t981
      t1011 = t285 * t816
      t1012 = t843 * t818
      t1015 = log(-0.4D1 * t1011 * t1012)
      t1020 = t1015 ** 2
      t1021 = t1020 * t1015
      t1025 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1046 = log(-0.4D1 * t815 * t944)
      t1047 = t1046 * t41
      t1053 = log(0.4D1 * t815 * t817 * t819 * t44)
      t1072 = log(-0.4D1 * t358 * t944)
      t1073 = t1072 ** 2
      t1074 = t1073 * t41
      t1077 = t1072 * t41
      t1088 = t7 * t41
      t1095 = (-0.90D2 * t4 * t7 * (-t948 * t949 + t952 + (-t951 + t958 
     #* t949 - t960 * t961 / 0.2D1) * t795 * t981 + t984 * t961 / 0.2D1)
     # + 0.180D3 * t12 * t13 * (-t948 * t961 + t992 + (-t949 + t958 * t9
     #61) * t795 * t981) + t23 * t13 * t1004) * t103 * t282 / 0.1440D4 +
     # (t192 * t13 * (t949 - t1015 * t961) - 0.90D2 * t161 * t13 * (-t10
     #21 * t961 / 0.6D1 - t1015 * t951 + t1025 + t1020 * t949 / 0.2D1) +
     # t211 * t13 * t961 + 0.180D3 * t164 * t13 * (t1020 * t961 / 0.2D1 
     #+ t951 - t1015 * t949)) * t282 / 0.1440D4 - (-0.90D2 * t4 * t7 * (
     #t1047 * t961 - (-t949 + t1053 * t961) * t795 * t981 - t992) - 0.18
     #0D3 * t12 * t13 * t1004) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 *
     # t7 * (-t1074 * t961 / 0.2D1 + t1077 * t949 - t952) + 0.180D3 * t1
     #2 * t13 * (t1077 * t961 - t992) - t153 * t1088 * t961) * t158 * t2
     #82 / 0.720D3
      t1096 = FJET(XB1, XB2, s, 0.0D0, -t800, -t942, 0.0D0, t943, t1095)
      t1098 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1100 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1102 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1108 = t41 * t1100
      t1119 = t41 * t1098
      t1128 = t41 * t1102 - t1102 * t795 * t981
      t1139 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1196 = (-0.90D2 * t4 * t7 * (-t948 * t1098 + (-t1100 + t958 * t10
     #98 - t960 * t1102 / 0.2D1) * t795 * t981 + t1108 + t984 * t1102 / 
     #0.2D1) + 0.180D3 * t12 * t13 * ((t958 * t1102 - t1098) * t795 * t9
     #81 + t1119 - t948 * t1102) + t23 * t13 * t1128) * t103 * t282 / 0.
     #1440D4 + (t192 * t13 * (t1098 - t1015 * t1102) - 0.90D2 * t161 * t
     #13 * (t1139 + t1020 * t1098 / 0.2D1 - t1021 * t1102 / 0.6D1 - t101
     #5 * t1100) + t211 * t13 * t1102 + 0.180D3 * t164 * t13 * (t1100 + 
     #t1020 * t1102 / 0.2D1 - t1015 * t1098)) * t282 / 0.1440D4 - (-0.90
     #D2 * t4 * t7 * (t1047 * t1102 - (t1053 * t1102 - t1098) * t795 * t
     #981 - t1119) - 0.180D3 * t12 * t13 * t1128) * t103 * t345 / 0.720D
     #3 - (-0.90D2 * t4 * t7 * (-t1074 * t1102 / 0.2D1 + t1077 * t1098 -
     # t1108) + 0.180D3 * t12 * t13 * (t1077 * t1102 - t1119) - t153 * t
     #1088 * t1102) * t158 * t282 / 0.720D3
      t1197 = FJET(XB1, XB2, s, 0.0D0, -t942, -t800, 0.0D0, t943, t1196)
      t1199 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1201 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1204 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1211 = t893 * t1199
      t1216 = t914 * t1201
      t1233 = (-0.90D2 * t4 * t7 * (-t894 * t1199 + t898 * t1201 / 0.2D1
     # + t893 * t1204) + 0.180D3 * t12 * t13 * (-t894 * t1201 + t1211) +
     # t153 * t1216) * t103 * t158 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t9
     #26 * t1201 - t1211) - 0.180D3 * t340 * t1216) * t103 * t345 / 0.72
     #0D3
      t1234 = FJET(XB1, XB2, s, 0.0D0, -t878, 0.0D0, t876, 0.0D0, t1233)
      t1236 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1238 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1239 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1278 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1284 = t13 * t1239
      t1305 = t7 * t1239
      t1306 = t1305 * t56
      t1333 = t7 * t1238
      t1336 = t13 * t1236
      t1346 = t7 * t1278
      t1350 = t13 * t1238
      t1355 = t7 * t1236
      t1369 = rrqg2qgh64J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1425 = (-0.90D2 * t4 * t7 * (-(-t245 * t1236 + t1238 + t247 * t12
     #39 / 0.2D1) * t56 - t41 * t1238 - t259 * t1239 / 0.2D1 + t256 * t1
     #236) + 0.180D3 * t12 * t13 * (t256 * t1239 - (t1236 - t245 * t1239
     #) * t56 - t41 * t1236) + t23 * t13 * (-t1239 * t56 - t41 * t1239))
     # * t103 * t282 / 0.1440D4 + (t192 * t13 * (-t1236 + t288 * t1239) 
     #- 0.90D2 * t161 * t13 * (-t294 * t1236 / 0.2D1 + t297 * t1239 / 0.
     #6D1 - t1278 + t288 * t1238) - t211 * t1284 + 0.180D3 * t164 * t13 
     #* (t288 * t1236 - t294 * t1239 / 0.2D1 - t1238)) * t282 / 0.1440D4
     # - (-0.90D2 * t4 * t7 * (t334 * t1239 - t327 * t1239 + (t1236 - t3
     #20 * t1239) * t56) + 0.180D3 * t340 * t1306) * t103 * t345 / 0.720
     #D3 - (-0.90D2 * t4 * t7 * (-t356 * t1236 - t364 * t1239 / 0.2D1 + 
     #t353 * t1239 / 0.2D1 + t367 * t1236) + 0.180D3 * t12 * t13 * (-t35
     #6 * t1239 + t367 * t1239)) * t158 * t282 / 0.720D3 + ((-0.90D2 * t
     #4 * t1333 + 0.180D3 * t12 * t1336 + t23 * t1284) * t58 - 0.90D2 * 
     #t4 * t1305 * t68 + (t23 * t1336 - 0.90D2 * t4 * t1346 + t83 * t128
     #4 + 0.180D3 * t12 * t1350) * t89 + (-0.90D2 * t4 * t1355 + 0.180D3
     # * t12 * t1284) * t100) * t103 / 0.2880D4 - t228 * t1333 / 0.2880D
     #4 - t403 * t1305 / 0.2880D4 + t161 * t13 * t1369 / 0.32D2 - t220 *
     # t1355 / 0.2880D4 - t235 * t1346 / 0.2880D4 + (-0.90D2 * t4 * t7 *
     # (-(-t122 * t1236 + t1238 + t124 * t1239 / 0.2D1) * t56 - t114 * t
     #1236 + t117 * t1239 / 0.2D1 + t134 * t1236 - t137 * t1239 / 0.2D1)
     # + 0.180D3 * t12 * t13 * (-t114 * t1239 + t134 * t1239 - (t1236 - 
     #t122 * t1239) * t56) - t153 * t1306) * t103 * t158 / 0.1440D4 - ((
     #-0.90D2 * t161 * t1336 + 0.180D3 * t164 * t1284) * t178 - 0.90D2 *
     # t180 * t1305 * t184 + (-0.90D2 * t161 * t1350 + 0.180D3 * t164 * 
     #t1336 + t192 * t1284) * t195) * t158 / 0.1440D4
      t1426 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1425)
      t1428 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1429 = t812 * t1428
      t1430 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1436 = t832 * t1430
      t1445 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1461 = -(-0.90D2 * t4 * t7 * (-t1429 + t825 * t1430) - 0.180D3 * 
     #t340 * t1436) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 * t7 * (t853
     # * t1428 - t850 * t1430 / 0.2D1 - t812 * t1445) + 0.180D3 * t12 * 
     #t13 * (-t1429 + t853 * t1430) - t153 * t1436) * t158 * t282 / 0.72
     #0D3
      t1462 = FJET(XB1, XB2, s, t803, -t800, t798, 0.0D0, -t808, t1461)
      t1464 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1465 = t812 * t1464
      t1466 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1472 = t832 * t1466
      t1479 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1497 = -(-0.90D2 * t4 * t7 * (-t1465 + t825 * t1466) - 0.180D3 * 
     #t340 * t1472) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 * t7 * (t853
     # * t1464 - t812 * t1479 - t850 * t1466 / 0.2D1) + 0.180D3 * t12 * 
     #t13 * (-t1465 + t853 * t1466) - t153 * t1472) * t158 * t282 / 0.72
     #0D3
      t1498 = FJET(XB1, XB2, s, t798, 0.0D0, t803, -t800, -t808, t1497)
      t1500 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1502 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1503 = t893 * t1502
      t1508 = t914 * t1500
      t1518 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1534 = -(-0.90D2 * t4 * t7 * (t926 * t1500 - t1503) - 0.180D3 * t
     #340 * t1508) * t103 * t345 / 0.720D3 + (-0.90D2 * t4 * t7 * (t898 
     #* t1500 / 0.2D1 - t894 * t1502 + t893 * t1518) + 0.180D3 * t12 * t
     #13 * (-t894 * t1500 + t1503) + t153 * t1508) * t103 * t158 / 0.144
     #0D4
      t1535 = FJET(XB1, XB2, s, t876, 0.0D0, -t878, 0.0D0, 0.0D0, t1534)
      t1540 = t43 * s * t1 * t799 * t874
      t1541 = t2 * x1
      t1543 = Sqrt(-t974 * t887)
      t1544 = t49 * t1543
      t1550 = t1541 * x2 * (-x3 + t872 - z + t50 - x1 + t966 + t794 - t9
     #67 + 0.2D1 * t1544) * t796 * t874
      t1551 = t800 * t875
      t1552 = t107 * x1
      t1554 = t107 * t794
      t1560 = t1541 * (t1552 + t107 * z - x2 + t872 + 0.1D1 - x3 - t1554
     # + 0.2D1 * t1544 * x2) * t796 * t874
      t1565 = log(-0.4D1 * t107 * t1011 * t1012 * t921)
      t1566 = t1565 * t795
      t1574 = 0.2D1 * t1544 * t810 + t967 - t970 + t971 - t973 - t968 + 
     #t239 + t50 + t794 + t1554 - 0.2D1 * t1544 * t809 - 0.2D1 * t872 * 
     #t794 + t969 * t809
      t1578 = t28 * x2
      t1580 = t238 * x2
      t1591 = 0.2D1 * t239 * x2 * z - t239 * t1578 + t1580 - t1552 + t23
     #8 * t28 * x2 - t1578 * x1 - 0.2D1 * t1580 * z - t872 * z + t872 * 
     #x1 - t239 * x2 - 0.2D1 * t1544 * z + t810 + t28
      t1593 = 0.1D1 / (t1574 + t1591)
      t1594 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1595 = t1593 * t1594
      t1597 = t795 * t1593
      t1598 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1604 = t7 * t795
      t1608 = -0.90D2 * t4 * t7 * (-t1566 * t1595 + t1597 * t1598) + 0.1
     #80D3 * t340 * t1604 * t1595
      t1612 = FJET(XB1, XB2, s, t1540, t1550, -t1551, -t1560, -t808, -t1
     #608 * t103 * t345 / 0.720D3)
      t1615 = t103 * t158 * t282
      t1618 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1619 = t1593 * t1618
      t1621 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1630 = -0.90D2 * t4 * t7 * (-t1566 * t1619 + t1597 * t1621) + 0.1
     #80D3 * t340 * t1604 * t1619
      t1634 = FJET(XB1, XB2, s, t1550, t1540, -t1560, -t1551, -t808, -t1
     #630 * t103 * t345 / 0.720D3)
      t1638 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1640 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1641 = t41 * t1640
      t1642 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1659 = t41 * t1638
      t1668 = t41 * t1642 - t1642 * t795 * t981
      t1679 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1736 = (-0.90D2 * t4 * t7 * (-t948 * t1638 + t1641 + t984 * t1642
     # / 0.2D1 + (t958 * t1638 - t960 * t1642 / 0.2D1 - t1640) * t795 * 
     #t981) + 0.180D3 * t12 * t13 * ((t958 * t1642 - t1638) * t795 * t98
     #1 + t1659 - t948 * t1642) + t23 * t13 * t1668) * t103 * t282 / 0.1
     #440D4 + (t192 * t13 * (-t1015 * t1642 + t1638) - 0.90D2 * t161 * t
     #13 * (t1679 - t1021 * t1642 / 0.6D1 + t1020 * t1638 / 0.2D1 - t101
     #5 * t1640) + t211 * t13 * t1642 + 0.180D3 * t164 * t13 * (-t1015 *
     # t1638 + t1020 * t1642 / 0.2D1 + t1640)) * t282 / 0.1440D4 - (-0.9
     #0D2 * t4 * t7 * (-(t1053 * t1642 - t1638) * t795 * t981 + t1047 * 
     #t1642 - t1659) - 0.180D3 * t12 * t13 * t1668) * t103 * t345 / 0.72
     #0D3 - (-0.90D2 * t4 * t7 * (t1077 * t1638 - t1074 * t1642 / 0.2D1 
     #- t1641) + 0.180D3 * t12 * t13 * (t1077 * t1642 - t1659) - t153 * 
     #t1088 * t1642) * t158 * t282 / 0.720D3
      t1737 = FJET(XB1, XB2, s, -t800, 0.0D0, 0.0D0, -t942, t943, t1736)
      t1739 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1741 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1742 = t812 * t1741
      t1747 = t832 * t1739
      t1754 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, t803, t798, -t
     #800, 0.0D0, -t808)
      t1772 = -(-0.90D2 * t4 * t7 * (t825 * t1739 - t1742) - 0.180D3 * t
     #340 * t1747) * t103 * t345 / 0.720D3 - (-0.90D2 * t4 * t7 * (t853 
     #* t1741 - t812 * t1754 - t850 * t1739 / 0.2D1) + 0.180D3 * t12 * t
     #13 * (t853 * t1739 - t1742) - t153 * t1747) * t158 * t282 / 0.720D
     #3
      t1773 = FJET(XB1, XB2, s, -t800, t803, 0.0D0, t798, -t808, t1772)
      t1775 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1777 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1778 = t41 * t1777
      t1779 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1796 = t41 * t1775
      t1805 = t41 * t1779 - t1779 * t795 * t981
      t1821 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t942, 
     #-t800, 0.0D0, t943)
      t1873 = (-0.90D2 * t4 * t7 * (-t948 * t1775 + t1778 + t984 * t1779
     # / 0.2D1 + (t958 * t1775 - t960 * t1779 / 0.2D1 - t1777) * t795 * 
     #t981) + 0.180D3 * t12 * t13 * ((t958 * t1779 - t1775) * t795 * t98
     #1 + t1796 - t948 * t1779) + t23 * t13 * t1805) * t103 * t282 / 0.1
     #440D4 + (t192 * t13 * (-t1015 * t1779 + t1775) - 0.90D2 * t161 * t
     #13 * (t1020 * t1775 / 0.2D1 - t1015 * t1777 - t1021 * t1779 / 0.6D
     #1 + t1821) + t211 * t13 * t1779 + 0.180D3 * t164 * t13 * (-t1015 *
     # t1775 + t1020 * t1779 / 0.2D1 + t1777)) * t282 / 0.1440D4 - (-0.9
     #0D2 * t4 * t7 * (-t1796 - (t1053 * t1779 - t1775) * t795 * t981 + 
     #t1047 * t1779) - 0.180D3 * t12 * t13 * t1805) * t103 * t345 / 0.72
     #0D3 - (-0.90D2 * t4 * t7 * (t1077 * t1775 - t1074 * t1779 / 0.2D1 
     #- t1778) + 0.180D3 * t12 * t13 * (t1077 * t1779 - t1796) - t153 * 
     #t1088 * t1779) * t158 * t282 / 0.720D3
      t1874 = FJET(XB1, XB2, s, -t942, 0.0D0, 0.0D0, -t800, t943, t1873)
      t1876 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1877 = t893 * t1876
      t1878 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1884 = t914 * t1878
      t1894 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t878, t876, 0.0D0)
      t1910 = -(-0.90D2 * t4 * t7 * (-t1877 + t926 * t1878) - 0.180D3 * 
     #t340 * t1884) * t103 * t345 / 0.720D3 + (-0.90D2 * t4 * t7 * (t898
     # * t1878 / 0.2D1 - t894 * t1876 + t893 * t1894) + 0.180D3 * t12 * 
     #t13 * (-t894 * t1878 + t1877) + t153 * t1884) * t103 * t158 / 0.14
     #40D4
      t1911 = FJET(XB1, XB2, s, -t878, 0.0D0, t876, 0.0D0, 0.0D0, t1910)
      t1913 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1915 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1916 = t1593 * t1915
      t1925 = -0.90D2 * t4 * t7 * (t1597 * t1913 - t1566 * t1916) + 0.18
     #0D3 * t340 * t1604 * t1916
      t1929 = FJET(XB1, XB2, s, -t1560, -t1551, t1550, t1540, -t808, -t1
     #925 * t103 * t345 / 0.720D3)
      t1933 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1935 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1550, -t1560,
     # t1540, -t1551, -t808)
      t1936 = t1593 * t1935
      t1945 = -0.90D2 * t4 * t7 * (t1597 * t1933 - t1566 * t1936) + 0.18
     #0D3 * t340 * t1604 * t1936
      t1949 = FJET(XB1, XB2, s, -t1551, -t1560, t1540, t1550, -t808, -t1
     #945 * t103 * t345 / 0.720D3)
      rrqg2qght6s2e1 = t407 * t406 + t599 * t598 + t791 * t790 + t870 * 
     #t869 + t939 * t938 + t1096 * t1095 + t1197 * t1196 + t1234 * t1233
     # + t1426 * t1425 + t1462 * t1461 + t1498 * t1497 + t1535 * t1534 -
     # t1612 * t1608 * t1615 / 0.720D3 - t1634 * t1630 * t1615 / 0.720D3
     # + t1737 * t1736 + t1773 * t1772 + t1874 * t1873 + t1911 * t1910 -
     # t1929 * t1925 * t1615 / 0.720D3 - t1949 * t1945 * t1615 / 0.720D3

      end function



      doubleprecision function rrqg2qght6s2e0
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t15 = z ** 2
      t17 = 0.1D1 / t15 / z
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t26 = log(-0.4D1 * t14 * t20 * t22)
      t27 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = cos(t11)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t21)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t39 = 0.1D1 / z
      t41 = t17 * t13
      t42 = t41 * t19
      t45 = log(0.4D1 * t10 * t42)
      t46 = t45 * t39
      t52 = pi * lh
      t53 = t3 * t7
      t61 = 0.1D1 / x3
      t63 = 0.1D1 / x1
      t66 = t7 * t27
      t69 = 0.1D1 / x2
      t70 = t69 * t63
      t71 = t37 * t61 * t70
      t74 = t4 * t7
      t75 = x2 ** 2
      t76 = t75 * t9
      t79 = log(0.4D1 * t76 * t42)
      t80 = t79 * t39
      t82 = t76 * t13
      t83 = -0.1D1 + x2
      t87 = log(-0.4D1 * t82 * t20 * t83)
      t88 = t87 * t39
      t95 = t39 * pi
      t96 = t9 * t13
      t99 = log(0.4D1 * t96 * t20)
      t101 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t102 = t99 ** 2
      t109 = t95 * lh
      t115 = lh ** 2
      t117 = pi ** 2
      t119 = -0.180D3 * t115 + 0.30D2 * t117
      t120 = t95 * t119
      t121 = t53 * t27
      t126 = x3 * t17
      t127 = t13 * t19
      t130 = log(0.4D1 * t126 * t127)
      t131 = t130 ** 2
      t133 = t127 * t22
      t136 = log(-0.4D1 * t126 * t133)
      t137 = t136 ** 2
      t140 = -t131 * t39 / 0.2D1 - t137 * t37 / 0.2D1
      t144 = t7 * t8
      t152 = t130 * t39 + t136 * t37
      t154 = t7 * t101
      t157 = t53 * t8
      t160 = pi * t119
      t163 = -t39 - t37
      t169 = log(0.4D1 * t42)
      t170 = t169 * pi
      t171 = t39 * lh
      t174 = t169 ** 2
      t175 = t174 * pi
      t179 = (-0.180D3 * t170 * t171 - 0.45D2 * t175 * t39 + t120) * t3
      t182 = rrqg2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t202 = (0.90D2 * t175 * t171 + t95 * (-0.60D2 * lh * t117 + 0.240D
     #3 * zeta3 + 0.120D3 * t115 * lh) + 0.15D2 * t174 * t169 * pi * t39
     # - t170 * t39 * t119) * t3
      t209 = (0.180D3 * t109 + 0.90D2 * t170 * t39) * t3
      t212 = x3 * t75
      t213 = t212 * t17
      t216 = log(-0.4D1 * t213 * t133)
      t220 = t127 * t83
      t223 = log(-0.4D1 * t213 * t220)
      t224 = t223 * t39
      t228 = log(0.4D1 * t212 * t42)
      t229 = t228 * t39
      t235 = t52 * t3
      t243 = t95 * t3
      t244 = t17 * t75
      t247 = log(0.4D1 * t244 * t127)
      t248 = t247 ** 2
      t251 = log(-0.4D1 * t244 * t220)
      t252 = t251 ** 2
      t254 = t248 / 0.2D1 - t252 / 0.2D1
      t263 = -t247 + t251
      t268 = (-0.90D2 * t4 * t7 * (-(t8 - t26 * t27) * t37 - t39 * t8 + 
     #t46 * t27) + 0.180D3 * t52 * t53 * (-t27 * t37 - t39 * t27)) * t61
     # * t63 / 0.1440D4 + t4 * t66 * t71 / 0.8D1 + t74 * (-t80 * t27 + t
     #88 * t27) * t69 * t63 / 0.8D1 + (-0.90D2 * t95 * t53 * (t99 * t8 -
     # t101 - t102 * t27 / 0.2D1) + 0.180D3 * t109 * t53 * (-t8 + t99 * 
     #t27) - t120 * t121) * t63 / 0.1440D4 + (-0.90D2 * t4 * t66 * t140 
     #+ (-0.90D2 * t4 * t144 + 0.180D3 * t52 * t121) * t152 + (-0.90D2 *
     # t4 * t154 + 0.180D3 * t52 * t157 + t160 * t121) * t163) * t61 / 0
     #.2880D4 - t179 * t144 / 0.2880D4 + t95 * t53 * t182 / 0.32D2 - t20
     #2 * t66 / 0.2880D4 - t209 * t154 / 0.2880D4 + (-0.90D2 * t4 * t7 *
     # (-(t8 - t216 * t27) * t37 - t224 * t27 + t229 * t27) - 0.180D3 * 
     #t235 * t66 * t37) * t61 * t69 / 0.1440D4 - (-0.90D2 * t243 * t66 *
     # t254 + (-0.90D2 * t95 * t157 + 0.180D3 * t109 * t121) * t263) * t
     #69 / 0.1440D4
      t269 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t268)
      t271 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t272 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t292 = t7 * t272
      t305 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t316 = t53 * t272
      t324 = t7 * t271
      t331 = t7 * t305
      t334 = t53 * t271
      t345 = rrqg2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t381 = (-0.90D2 * t4 * t7 * (-(t271 - t26 * t272) * t37 - t39 * t2
     #71 + t46 * t272) + 0.180D3 * t52 * t53 * (-t272 * t37 - t39 * t272
     #)) * t61 * t63 / 0.1440D4 + t4 * t292 * t71 / 0.8D1 + t74 * (t88 *
     # t272 - t80 * t272) * t69 * t63 / 0.8D1 + (-0.90D2 * t95 * t53 * (
     #-t102 * t272 / 0.2D1 - t305 + t99 * t271) + 0.180D3 * t109 * t53 *
     # (-t271 + t99 * t272) - t120 * t316) * t63 / 0.1440D4 + (-0.90D2 *
     # t4 * t292 * t140 + (-0.90D2 * t4 * t324 + 0.180D3 * t52 * t316) *
     # t152 + (-0.90D2 * t4 * t331 + 0.180D3 * t52 * t334 + t160 * t316)
     # * t163) * t61 / 0.2880D4 - t179 * t324 / 0.2880D4 + t95 * t53 * t
     #345 / 0.32D2 - t202 * t292 / 0.2880D4 - t209 * t331 / 0.2880D4 + (
     #-0.90D2 * t4 * t7 * (-(t271 - t216 * t272) * t37 - t224 * t272 + t
     #229 * t272) - 0.180D3 * t235 * t292 * t37) * t61 * t69 / 0.1440D4 
     #- (-0.90D2 * t243 * t292 * t254 + (-0.90D2 * t95 * t334 + 0.180D3 
     #* t109 * t316) * t263) * t69 / 0.1440D4
      t382 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t381)
      t384 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t385 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t405 = t7 * t385
      t418 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t429 = t53 * t385
      t437 = t7 * t384
      t444 = t7 * t418
      t447 = t53 * t384
      t458 = rrqg2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t494 = (-0.90D2 * t4 * t7 * (-(t384 - t26 * t385) * t37 + t46 * t3
     #85 - t39 * t384) + 0.180D3 * t52 * t53 * (-t385 * t37 - t39 * t385
     #)) * t61 * t63 / 0.1440D4 + t4 * t405 * t71 / 0.8D1 + t74 * (-t80 
     #* t385 + t88 * t385) * t69 * t63 / 0.8D1 + (-0.90D2 * t95 * t53 * 
     #(-t102 * t385 / 0.2D1 - t418 + t99 * t384) + 0.180D3 * t109 * t53 
     #* (-t384 + t99 * t385) - t120 * t429) * t63 / 0.1440D4 + (-0.90D2 
     #* t4 * t405 * t140 + (-0.90D2 * t4 * t437 + 0.180D3 * t52 * t429) 
     #* t152 + (-0.90D2 * t4 * t444 + 0.180D3 * t52 * t447 + t160 * t429
     #) * t163) * t61 / 0.2880D4 - t179 * t437 / 0.2880D4 + t95 * t53 * 
     #t458 / 0.32D2 - t202 * t405 / 0.2880D4 - t209 * t444 / 0.2880D4 + 
     #(-0.90D2 * t4 * t7 * (-(t384 - t216 * t385) * t37 - t224 * t385 + 
     #t229 * t385) - 0.180D3 * t235 * t405 * t37) * t61 * t69 / 0.1440D4
     # - (-0.90D2 * t243 * t405 * t254 + (-0.90D2 * t95 * t447 + 0.180D3
     # * t109 * t429) * t263) * t69 / 0.1440D4
      t495 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t494)
      t498 = x1 * z
      t499 = -z - x1 + t498
      t500 = 0.1D1 / t499
      t502 = t2 * x1 * t83 * t500
      t503 = -0.1D1 + x1
      t504 = t2 * t503
      t507 = x2 * s * t1 * x1
      t508 = s * t18
      t511 = t503 * x1 * t500
      t512 = t508 * t83 * t511
      t513 = x2 * x1
      t514 = t513 * z
      t516 = 0.1D1 / (-z + t514 - t513)
      t517 = t7 * t516
      t518 = t4 * t517
      t519 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t5
     #04, 0.0D0, -t512)
      t524 = 0.1D1 / t15
      t525 = t13 * t524
      t527 = t19 * t500
      t528 = t503 ** 2
      t533 = log(0.4D1 * t76 * t525 * t527 * t528 * t83)
      t534 = t533 * t516
      t536 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t5
     #04, 0.0D0, -t512)
      t549 = -t518 * t519 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (t5
     #34 * t519 - t516 * t536) - 0.180D3 * t235 * t517 * t519) * t69 * t
     #63 / 0.720D3
      t550 = FJET(XB1, XB2, s, 0.0D0, t502, -t504, t507, -t512, t549)
      t552 = x2 * x3
      t553 = 0.1D1 - x3 + t552
      t554 = 0.1D1 / t553
      t555 = t552 * t554
      t556 = t2 * t555
      t558 = t2 * t21 * t554
      t559 = t83 * t21
      t561 = Sqrt(t31 * t559)
      t565 = 0.1D1 / (-z - x3 + t552 + 0.2D1 * t30 * t561)
      t566 = t7 * t565
      t567 = t4 * t566
      t568 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t558, t556, 0.0D0)
      t575 = t553 ** 2
      t581 = log(0.4D1 * t212 * t41 * t19 * t83 * t21 / t575)
      t582 = t581 * t565
      t584 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t558, t556, 0.0D0)
      t597 = -t567 * t568 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t
     #582 * t568 + t565 * t584) + 0.180D3 * t235 * t566 * t568) * t61 * 
     #t69 / 0.1440D4
      t598 = FJET(XB1, XB2, s, 0.0D0, t556, 0.0D0, -t558, 0.0D0, t597)
      t601 = t2 * x1 * t500
      t602 = t508 * t511
      t605 = t524 * t19 * t500 * t528
      t608 = log(-0.4D1 * t14 * t605)
      t609 = t608 * t39
      t610 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, -
     #t504, 0.0D0, t602)
      t612 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, -
     #t504, 0.0D0, t602)
      t613 = t39 * t612
      t619 = log(0.4D1 * t10 * t525 * t527 * t528 * t22)
      t623 = x3 * x1
      t624 = t623 * z
      t625 = x1 * t15
      t626 = x3 * t15
      t627 = t626 * x1
      t628 = t10 * t15
      t630 = 0.2D1 * t10 * z
      t631 = x3 * t499
      t633 = Sqrt(t631 * t21)
      t638 = 0.1D1 / (-t498 - t624 + t625 + t627 - t10 - t31 - t628 + t6
     #30 + 0.2D1 * t30 * t633 * z - t15)
      t647 = t39 * t610 - t610 * t499 * t638
      t662 = log(-0.4D1 * t82 * t605)
      t663 = t662 * t39
      t669 = t7 * t39
      t670 = t669 * t610
      t681 = log(-0.4D1 * t96 * t524 * t527 * t528)
      t682 = t681 ** 2
      t685 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, -
     #t504, 0.0D0, t602)
      t696 = t160 * t3
      t701 = (-0.90D2 * t4 * t7 * (-t609 * t610 + t613 + (-t612 + t619 *
     # t610) * t499 * t638) + 0.180D3 * t52 * t53 * t647) * t61 * t63 / 
     #0.1440D4 - t74 * t647 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #t663 * t610 - t613) - 0.180D3 * t235 * t670) * t69 * t63 / 0.720D3
     # + (-0.90D2 * t95 * t53 * (t682 * t610 / 0.2D1 + t685 - t681 * t61
     #2) + 0.180D3 * t109 * t53 * (t612 - t681 * t610) + t696 * t670) * 
     #t63 / 0.1440D4
      t702 = FJET(XB1, XB2, s, 0.0D0, -t504, -t601, 0.0D0, t602, t701)
      t704 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, -
     #t504, 0.0D0, t602)
      t706 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, -
     #t504, 0.0D0, t602)
      t710 = t39 * t706
      t719 = t39 * t704 - t704 * t499 * t638
      t737 = t669 * t704
      t744 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, -
     #t504, 0.0D0, t602)
      t761 = (-0.90D2 * t4 * t7 * ((t619 * t704 - t706) * t499 * t638 + 
     #t710 - t609 * t704) + 0.180D3 * t52 * t53 * t719) * t61 * t63 / 0.
     #1440D4 - t74 * t719 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (t6
     #63 * t704 - t710) - 0.180D3 * t235 * t737) * t69 * t63 / 0.720D3 +
     # (-0.90D2 * t95 * t53 * (t744 + t682 * t704 / 0.2D1 - t681 * t706)
     # + 0.180D3 * t109 * t53 * (t706 - t681 * t704) + t696 * t737) * t6
     #3 / 0.1440D4
      t762 = FJET(XB1, XB2, s, 0.0D0, -t601, -t504, 0.0D0, t602, t761)
      t764 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t558, t556, 0.0D0)
      t770 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t558, t556, 0.0D0)
      t783 = -t567 * t764 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t
     #582 * t764 + t565 * t770) + 0.180D3 * t235 * t566 * t764) * t61 * 
     #t69 / 0.1440D4
      t784 = FJET(XB1, XB2, s, 0.0D0, -t558, 0.0D0, t556, 0.0D0, t783)
      t786 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t788 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t807 = t7 * t786
      t821 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t831 = t53 * t786
      t838 = t7 * t788
      t841 = rrqg2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t845 = t7 * t821
      t859 = t53 * t788
      t896 = (-0.90D2 * t4 * t7 * (t46 * t786 - (t788 - t26 * t786) * t3
     #7 - t39 * t788) + 0.180D3 * t52 * t53 * (-t786 * t37 - t39 * t786)
     #) * t61 * t63 / 0.1440D4 + t4 * t807 * t71 / 0.8D1 + t74 * (-t80 *
     # t786 + t88 * t786) * t69 * t63 / 0.8D1 + (-0.90D2 * t95 * t53 * (
     #t99 * t788 - t102 * t786 / 0.2D1 - t821) + 0.180D3 * t109 * t53 * 
     #(-t788 + t99 * t786) - t120 * t831) * t63 / 0.1440D4 - t202 * t807
     # / 0.2880D4 - t179 * t838 / 0.2880D4 + t95 * t53 * t841 / 0.32D2 -
     # t209 * t845 / 0.2880D4 + (-0.90D2 * t4 * t807 * t140 + (-0.90D2 *
     # t4 * t838 + 0.180D3 * t52 * t831) * t152 + (-0.90D2 * t4 * t845 +
     # 0.180D3 * t52 * t859 + t160 * t831) * t163) * t61 / 0.2880D4 + (-
     #0.90D2 * t4 * t7 * (-t224 * t786 + t229 * t786 - (t788 - t216 * t7
     #86) * t37) - 0.180D3 * t235 * t807 * t37) * t61 * t69 / 0.1440D4 -
     # (-0.90D2 * t243 * t807 * t254 + (-0.90D2 * t95 * t859 + 0.180D3 *
     # t109 * t831) * t263) * t69 / 0.1440D4
      t897 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t896)
      t899 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t5
     #04, 0.0D0, -t512)
      t904 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t5
     #04, 0.0D0, -t512)
      t918 = -t518 * t899 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t
     #516 * t904 + t534 * t899) - 0.180D3 * t235 * t517 * t899) * t69 * 
     #t63 / 0.720D3
      t919 = FJET(XB1, XB2, s, t507, -t504, t502, 0.0D0, -t512, t918)
      t921 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t5
     #04, 0.0D0, -t512)
      t926 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t5
     #04, 0.0D0, -t512)
      t940 = -t518 * t921 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t
     #516 * t926 + t534 * t921) - 0.180D3 * t235 * t517 * t921) * t69 * 
     #t63 / 0.720D3
      t941 = FJET(XB1, XB2, s, t502, 0.0D0, t507, -t504, -t512, t940)
      t943 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t558, t556, 0.0D0)
      t949 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t558, t556, 0.0D0)
      t962 = -t567 * t943 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t
     #582 * t943 + t565 * t949) + 0.180D3 * t235 * t566 * t943) * t61 * 
     #t69 / 0.1440D4
      t963 = FJET(XB1, XB2, s, t556, 0.0D0, -t558, 0.0D0, 0.0D0, t962)
      t968 = t21 * s * t1 * t503 * t554
      t969 = t2 * x1
      t971 = Sqrt(-t631 * t559)
      t972 = t30 * t971
      t978 = t969 * x2 * (-x3 + t552 - z + t31 - x1 + t623 + t498 - t624
     # + 0.2D1 * t972) * t500 * t554
      t979 = t504 * t555
      t980 = t212 * x1
      t982 = t212 * t498
      t988 = t969 * (t980 + t212 * z - x2 + t552 + 0.1D1 - x3 - t982 + 0
     #.2D1 * t972 * x2) * t500 * t554
      t990 = t4 * t7 * t499
      t993 = t15 * x2
      t995 = t9 * x2
      t1005 = t15 - t980 + t9 * t15 * x2 - t993 * x1 - 0.2D1 * t995 * z 
     #- t552 * z + t552 * x1 - t10 * x2 - 0.2D1 * t972 * z + 0.2D1 * t97
     #2 * t514 + t514 + t624 - t627
      t1015 = t628 - t630 + t995 - t625 + t10 + t31 + t982 - 0.2D1 * t97
     #2 * t513 - 0.2D1 * t552 * t498 + t626 * t513 + 0.2D1 * t10 * x2 * 
     #z - t10 * t993 + t498
      t1017 = 0.1D1 / (t1005 + t1015)
      t1018 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t978, -t988, t
     #968, -t979, -t512)
      t1021 = t61 * t69 * t63
      t1022 = t1017 * t1018 * t1021
      t1025 = FJET(XB1, XB2, s, t968, t978, -t979, -t988, -t512, t990 * 
     #t1022 / 0.8D1)
      t1027 = t53 * t499
      t1031 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t978, -t988, t
     #968, -t979, -t512)
      t1033 = t1017 * t1031 * t1021
      t1036 = FJET(XB1, XB2, s, t978, t968, -t988, -t979, -t512, t990 * 
     #t1033 / 0.8D1)
      t1041 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, 
     #-t504, 0.0D0, t602)
      t1043 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, 
     #-t504, 0.0D0, t602)
      t1047 = t39 * t1043
      t1056 = t39 * t1041 - t1041 * t499 * t638
      t1074 = t669 * t1041
      t1084 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, 
     #-t504, 0.0D0, t602)
      t1098 = (-0.90D2 * t4 * t7 * ((t619 * t1041 - t1043) * t499 * t638
     # + t1047 - t609 * t1041) + 0.180D3 * t52 * t53 * t1056) * t61 * t6
     #3 / 0.1440D4 - t74 * t1056 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t
     #7 * (t663 * t1041 - t1047) - 0.180D3 * t235 * t1074) * t69 * t63 /
     # 0.720D3 + (-0.90D2 * t95 * t53 * (-t681 * t1043 + t682 * t1041 / 
     #0.2D1 + t1084) + 0.180D3 * t109 * t53 * (-t681 * t1041 + t1043) + 
     #t696 * t1074) * t63 / 0.1440D4
      t1099 = FJET(XB1, XB2, s, -t504, 0.0D0, 0.0D0, -t601, t602, t1098)
      t1101 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t
     #504, 0.0D0, -t512)
      t1107 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t507, t502, -t
     #504, 0.0D0, -t512)
      t1120 = -t518 * t1101 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #t534 * t1101 - t516 * t1107) - 0.180D3 * t235 * t517 * t1101) * t6
     #9 * t63 / 0.720D3
      t1121 = FJET(XB1, XB2, s, -t504, t507, 0.0D0, t502, -t512, t1120)
      t1123 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, 
     #-t504, 0.0D0, t602)
      t1125 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, 
     #-t504, 0.0D0, t602)
      t1129 = t39 * t1125
      t1138 = t39 * t1123 - t1123 * t499 * t638
      t1156 = t669 * t1123
      t1166 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t601, 
     #-t504, 0.0D0, t602)
      t1180 = (-0.90D2 * t4 * t7 * ((t619 * t1123 - t1125) * t499 * t638
     # + t1129 - t609 * t1123) + 0.180D3 * t52 * t53 * t1138) * t61 * t6
     #3 / 0.1440D4 - t74 * t1138 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t
     #7 * (t663 * t1123 - t1129) - 0.180D3 * t235 * t1156) * t69 * t63 /
     # 0.720D3 + (-0.90D2 * t95 * t53 * (-t681 * t1125 + t682 * t1123 / 
     #0.2D1 + t1166) + 0.180D3 * t109 * t53 * (-t681 * t1123 + t1125) + 
     #t696 * t1156) * t63 / 0.1440D4
      t1181 = FJET(XB1, XB2, s, -t601, 0.0D0, 0.0D0, -t504, t602, t1180)
      t1183 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t558, t556, 0.0D0)
      t1189 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t558, t556, 0.0D0)
      t1202 = -t567 * t1183 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #-t582 * t1183 + t565 * t1189) + 0.180D3 * t235 * t566 * t1183) * t
     #61 * t69 / 0.1440D4
      t1203 = FJET(XB1, XB2, s, -t558, 0.0D0, t556, 0.0D0, 0.0D0, t1202)
      t1205 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t978, -t988, t
     #968, -t979, -t512)
      t1207 = t1017 * t1205 * t1021
      t1210 = FJET(XB1, XB2, s, -t988, -t979, t978, t968, -t512, t990 * 
     #t1207 / 0.8D1)
      t1215 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t978, -t988, t
     #968, -t979, -t512)
      t1217 = t1017 * t1215 * t1021
      t1220 = FJET(XB1, XB2, s, -t979, -t988, t968, t978, -t512, t990 * 
     #t1217 / 0.8D1)
      rrqg2qght6s2e0 = t269 * t268 + t382 * t381 + t495 * t494 + t550 * 
     #t549 + t598 * t597 + t702 * t701 + t762 * t761 + t784 * t783 + t89
     #7 * t896 + t919 * t918 + t941 * t940 + t963 * t962 + t1025 * pi * 
     #t1027 * t1022 / 0.8D1 + t1036 * pi * t1027 * t1033 / 0.8D1 + t1099
     # * t1098 + t1121 * t1120 + t1181 * t1180 + t1203 * t1202 + t1210 *
     # pi * t1027 * t1207 / 0.8D1 + t1220 * pi * t1027 * t1217 / 0.8D1

      end function



      doubleprecision function rrqg2qght6s2em1
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * pi
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t22 = log(0.4D1 * t13 * t19)
      t23 = 0.1D1 / z
      t25 = -0.1D1 + x3
      t30 = log(-0.4D1 * t13 * t19 / t25)
      t31 = cos(t14)
      t32 = x3 * z
      t34 = Sqrt(-t32 * t25)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t34)
      t40 = t22 * t23 + t30 * t38
      t44 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t45 = t7 * t44
      t48 = pi * lh
      t49 = t3 * t7
      t50 = t49 * t8
      t54 = -t23 - t38
      t57 = 0.1D1 / x3
      t60 = t23 * pi
      t61 = x1 ** 2
      t62 = t61 * t16
      t66 = log(0.4D1 * t62 * t12 * t18)
      t72 = t60 * lh
      t76 = 0.1D1 / x1
      t79 = t4 * t7
      t80 = t8 * t38
      t87 = 0.1D1 / x2
      t88 = t57 * t87
      t92 = t60 * t3
      t93 = x2 ** 2
      t94 = t12 * t93
      t97 = log(0.4D1 * t94 * t19)
      t98 = -0.1D1 + x2
      t102 = log(-0.4D1 * t94 * t19 * t98)
      t104 = (-t97 + t102) * t87
      t108 = rrqg2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t116 = log(0.4D1 * t12 * t16 * t18)
      t117 = t116 * pi
      t121 = (0.180D3 * t72 + 0.90D2 * t117 * t23) * t3
      t127 = t116 ** 2
      t131 = lh ** 2
      t133 = pi ** 2
      t138 = (-0.180D3 * t117 * t23 * lh - 0.45D2 * t127 * pi * t23 + t6
     #0 * (-0.180D3 * t131 + 0.30D2 * t133)) * t3
      t141 = (-0.90D2 * t4 * t9 * t40 + (-0.90D2 * t4 * t45 + 0.180D3 * 
     #t48 * t50) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 * t49 * (-t44 
     #+ t66 * t8) - 0.180D3 * t72 * t50) * t76 / 0.1440D4 - t79 * (-t80 
     #- t23 * t8) * t57 * t76 / 0.16D2 + t79 * t80 * t88 / 0.16D2 + t92 
     #* t9 * t104 / 0.16D2 + t60 * t49 * t108 / 0.32D2 - t121 * t45 / 0.
     #2880D4 - t138 * t9 / 0.2880D4
      t142 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t141)
      t144 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t145 = t7 * t144
      t149 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t150 = t7 * t149
      t153 = t49 * t144
      t171 = t144 * t38
      t184 = rrqg2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t192 = (-0.90D2 * t4 * t145 * t40 + (-0.90D2 * t4 * t150 + 0.180D3
     # * t48 * t153) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 * t49 * (-
     #t149 + t66 * t144) - 0.180D3 * t72 * t153) * t76 / 0.1440D4 - t79 
     #* (-t171 - t23 * t144) * t57 * t76 / 0.16D2 + t79 * t171 * t88 / 0
     #.16D2 + t92 * t145 * t104 / 0.16D2 + t60 * t49 * t184 / 0.32D2 - t
     #121 * t150 / 0.2880D4 - t138 * t145 / 0.2880D4
      t193 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t192)
      t195 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t196 = t7 * t195
      t200 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t201 = t7 * t200
      t204 = t49 * t195
      t222 = t195 * t38
      t235 = rrqg2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t243 = (-0.90D2 * t4 * t196 * t40 + (-0.90D2 * t4 * t201 + 0.180D3
     # * t48 * t204) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 * t49 * (-
     #t200 + t66 * t195) - 0.180D3 * t72 * t204) * t76 / 0.1440D4 - t79 
     #* (-t222 - t23 * t195) * t57 * t76 / 0.16D2 + t79 * t222 * t88 / 0
     #.16D2 + t92 * t196 * t104 / 0.16D2 + t60 * t49 * t235 / 0.32D2 - t
     #121 * t201 / 0.2880D4 - t138 * t196 / 0.2880D4
      t244 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t243)
      t247 = x1 * z
      t248 = -z - x1 + t247
      t249 = 0.1D1 / t248
      t251 = t2 * x1 * t98 * t249
      t252 = -0.1D1 + x1
      t253 = t2 * t252
      t256 = x2 * s * t1 * x1
      t257 = s * t17
      t260 = t252 * x1 * t249
      t261 = t257 * t98 * t260
      t262 = x2 * x1
      t265 = 0.1D1 / (-z + t262 * z - t262)
      t266 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t256, t251, -t2
     #53, 0.0D0, -t261)
      t268 = t87 * t76
      t269 = t265 * t266 * t268
      t272 = FJET(XB1, XB2, s, 0.0D0, t251, -t253, t256, -t261, -t79 * t
     #269 / 0.8D1)
      t277 = x2 * x3
      t279 = 0.1D1 / (0.1D1 - x3 + t277)
      t281 = t2 * t277 * t279
      t283 = t2 * t25 * t279
      t286 = Sqrt(t32 * t98 * t25)
      t290 = 0.1D1 / (-z - x3 + t277 + 0.2D1 * t31 * t286)
      t291 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t283, t281, 0.0D0)
      t293 = t290 * t291 * t88
      t296 = FJET(XB1, XB2, s, 0.0D0, t281, 0.0D0, -t283, 0.0D0, -t79 * 
     #t293 / 0.16D2)
      t302 = t2 * x1 * t249
      t303 = t257 * t260
      t304 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t305 = t23 * t304
      t309 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t313 = t252 ** 2
      t317 = log(-0.4D1 * t62 / t10 * t18 * t249 * t313)
      t323 = t48 * t3
      t324 = t7 * t23
      t337 = x3 * t61
      t343 = Sqrt(x3 * t248 * t25)
      t348 = 0.1D1 / (-t247 - x3 * x1 * z + x1 * t10 + x3 * t10 * x1 - t
     #337 - t32 - t337 * t10 + 0.2D1 * t337 * z + 0.2D1 * t31 * t343 * z
     # - t10)
      t355 = -t79 * t305 * t268 / 0.8D1 + (-0.90D2 * t60 * t49 * (t309 -
     # t317 * t304) + 0.180D3 * t323 * t324 * t304) * t76 / 0.1440D4 - t
     #79 * (t305 - t304 * t248 * t348) * t57 * t76 / 0.16D2
      t356 = FJET(XB1, XB2, s, 0.0D0, -t253, -t302, 0.0D0, t303, t355)
      t358 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t359 = t23 * t358
      t363 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t382 = -t79 * t359 * t268 / 0.8D1 + (-0.90D2 * t60 * t49 * (t363 -
     # t317 * t358) + 0.180D3 * t323 * t324 * t358) * t76 / 0.1440D4 - t
     #79 * (t359 - t358 * t248 * t348) * t57 * t76 / 0.16D2
      t383 = FJET(XB1, XB2, s, 0.0D0, -t302, -t253, 0.0D0, t303, t382)
      t385 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t283, t281, 0.0D0)
      t387 = t290 * t385 * t88
      t390 = FJET(XB1, XB2, s, 0.0D0, -t283, 0.0D0, t281, 0.0D0, -t79 * 
     #t387 / 0.16D2)
      t395 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t396 = t7 * t395
      t400 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t401 = t7 * t400
      t404 = t49 * t395
      t412 = rrqg2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t430 = t395 * t38
      t443 = (-0.90D2 * t4 * t396 * t40 + (-0.90D2 * t4 * t401 + 0.180D3
     # * t48 * t404) * t54) * t57 / 0.2880D4 + t60 * t49 * t412 / 0.32D2
     # - t121 * t401 / 0.2880D4 - t138 * t396 / 0.2880D4 + (-0.90D2 * t6
     #0 * t49 * (-t400 + t66 * t395) - 0.180D3 * t72 * t404) * t76 / 0.1
     #440D4 - t79 * (-t430 - t23 * t395) * t57 * t76 / 0.16D2 + t79 * t4
     #30 * t88 / 0.16D2 + t92 * t396 * t104 / 0.16D2
      t444 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t443)
      t446 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t256, t251, -t2
     #53, 0.0D0, -t261)
      t448 = t265 * t446 * t268
      t451 = FJET(XB1, XB2, s, t256, -t253, t251, 0.0D0, -t261, -t79 * t
     #448 / 0.8D1)
      t456 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t256, t251, -t2
     #53, 0.0D0, -t261)
      t458 = t265 * t456 * t268
      t461 = FJET(XB1, XB2, s, t251, 0.0D0, t256, -t253, -t261, -t79 * t
     #458 / 0.8D1)
      t466 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t283, t281, 0.0D0)
      t468 = t290 * t466 * t88
      t471 = FJET(XB1, XB2, s, t281, 0.0D0, -t283, 0.0D0, 0.0D0, -t79 * 
     #t468 / 0.16D2)
      t476 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t477 = t23 * t476
      t482 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t500 = -t79 * t477 * t268 / 0.8D1 + (-0.90D2 * t60 * t49 * (-t317 
     #* t476 + t482) + 0.180D3 * t323 * t324 * t476) * t76 / 0.1440D4 - 
     #t79 * (t477 - t476 * t248 * t348) * t57 * t76 / 0.16D2
      t501 = FJET(XB1, XB2, s, -t253, 0.0D0, 0.0D0, -t302, t303, t500)
      t503 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t256, t251, -t2
     #53, 0.0D0, -t261)
      t505 = t265 * t503 * t268
      t508 = FJET(XB1, XB2, s, -t253, t256, 0.0D0, t251, -t261, -t79 * t
     #505 / 0.8D1)
      t513 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t514 = t23 * t513
      t519 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t302, -
     #t253, 0.0D0, t303)
      t537 = -t79 * t514 * t268 / 0.8D1 + (-0.90D2 * t60 * t49 * (-t317 
     #* t513 + t519) + 0.180D3 * t323 * t324 * t513) * t76 / 0.1440D4 - 
     #t79 * (t514 - t513 * t248 * t348) * t57 * t76 / 0.16D2
      t538 = FJET(XB1, XB2, s, -t302, 0.0D0, 0.0D0, -t253, t303, t537)
      t540 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t283, t281, 0.0D0)
      t542 = t290 * t540 * t88
      t545 = FJET(XB1, XB2, s, -t283, 0.0D0, t281, 0.0D0, 0.0D0, -t79 * 
     #t542 / 0.16D2)
      rrqg2qght6s2em1 = t142 * t141 + t193 * t192 + t244 * t243 - t272 *
     # pi * t49 * t269 / 0.8D1 - t296 * pi * t49 * t293 / 0.16D2 + t356 
     #* t355 + t383 * t382 - t390 * pi * t49 * t387 / 0.16D2 + t444 * t4
     #43 - t451 * pi * t49 * t448 / 0.8D1 - t461 * pi * t49 * t458 / 0.8
     #D1 - t471 * pi * t49 * t468 / 0.16D2 + t501 * t500 - t508 * pi * t
     #49 * t505 / 0.8D1 + t538 * t537 - t545 * pi * t49 * t542 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s2em2
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = 0.1D1 / z
      t4 = pi * t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = s ** 2
      t9 = 0.1D1 / t7 / s
      t10 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t11 = t9 * t10
      t12 = 0.1D1 / x1
      t17 = pi * t5 * t9
      t18 = x4 * pi
      t19 = cos(t18)
      t23 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t28 = -t3 - 0.1D1 / (-z - x3 + 0.2D1 * t19 * t23)
      t30 = 0.1D1 / x3
      t34 = t5 * t9
      t35 = rrqg2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t41 = z ** 2
      t44 = Sin(t18)
      t45 = t44 ** 2
      t47 = t1 ** 2
      t48 = t47 ** 2
      t51 = log(0.4D1 / t41 / z * t45 * t48)
      t56 = (0.180D3 * t4 * lh + 0.90D2 * t51 * pi * t3) * t5
      t59 = t6 * t11 * t12 / 0.16D2 - t17 * t10 * t28 * t30 / 0.32D2 + t
     #4 * t34 * t35 / 0.32D2 - t56 * t11 / 0.2880D4
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t59)
      t62 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t63 = t9 * t62
      t71 = rrqg2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t77 = t6 * t63 * t12 / 0.16D2 - t17 * t62 * t28 * t30 / 0.32D2 + t
     #4 * t34 * t71 / 0.32D2 - t56 * t63 / 0.2880D4
      t78 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t77)
      t80 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t81 = t9 * t80
      t89 = rrqg2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t95 = t6 * t81 * t12 / 0.16D2 - t17 * t80 * t28 * t30 / 0.32D2 + t
     #4 * t34 * t89 / 0.32D2 - t56 * t81 / 0.2880D4
      t96 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t95)
      t98 = -0.1D1 + x1
      t99 = t2 * t98
      t102 = 0.1D1 / (-z - x1 + x1 * z)
      t104 = t2 * x1 * t102
      t108 = s * t47 * x1 * t98 * t102
      t109 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t104, -
     #t99, 0.0D0, t108)
      t114 = FJET(XB1, XB2, s, 0.0D0, -t99, -t104, 0.0D0, t108, -t17 * t
     #3 * t109 * t12 / 0.16D2)
      t117 = t9 * t3
      t122 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t104, -
     #t99, 0.0D0, t108)
      t127 = FJET(XB1, XB2, s, 0.0D0, -t104, -t99, 0.0D0, t108, -t17 * t
     #3 * t122 * t12 / 0.16D2)
      t134 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t135 = t9 * t134
      t143 = rrqg2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t149 = t6 * t135 * t12 / 0.16D2 - t17 * t134 * t28 * t30 / 0.32D2 
     #+ t4 * t34 * t143 / 0.32D2 - t56 * t135 / 0.2880D4
      t150 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t149)
      t152 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t104, -
     #t99, 0.0D0, t108)
      t157 = FJET(XB1, XB2, s, -t99, 0.0D0, 0.0D0, -t104, t108, -t17 * t
     #3 * t152 * t12 / 0.16D2)
      t164 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t104, -
     #t99, 0.0D0, t108)
      t169 = FJET(XB1, XB2, s, -t104, 0.0D0, 0.0D0, -t99, t108, -t17 * t
     #3 * t164 * t12 / 0.16D2)
      rrqg2qght6s2em2 = t60 * t59 + t78 * t77 + t96 * t95 - t114 * pi * 
     #t5 * t117 * t109 * t12 / 0.16D2 - t127 * pi * t5 * t117 * t122 * t
     #12 / 0.16D2 + t150 * t149 - t157 * pi * t5 * t117 * t152 * t12 / 0
     #.16D2 - t169 * pi * t5 * t117 * t164 * t12 / 0.16D2

      end function



      doubleprecision function rrqg2qght6s2em3
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

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
      t3 = 0.1D1 / z
      t4 = pi * t3
      t6 = s ** 2
      t9 = 0.1D1 / t1 / t6 / s
      t10 = rrqg2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t11 = t9 * t10
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrqg2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrqg2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t34 = rrqg2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t35 = t9 * t34
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t35 /
     # 0.32D2)
      rrqg2qght6s2em3 = t14 * t3 * pi * t11 / 0.32D2 + t22 * t3 * pi * t
     #19 / 0.32D2 + t30 * t3 * pi * t27 / 0.32D2 + t38 * t3 * pi * t35 /
     # 0.32D2

      end function



      doubleprecision function rrqg2qght6s2em4
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
      doubleprecision rrqg2qgh61J1
      doubleprecision rrqg2qgh61J2
      doubleprecision rrqg2qgh61J3
      doubleprecision rrqg2qgh61J4
      doubleprecision rrqg2qgh61J5
      doubleprecision rrqg2qgh61J6
      doubleprecision rrqg2qgh62J1
      doubleprecision rrqg2qgh62J2
      doubleprecision rrqg2qgh62J3
      doubleprecision rrqg2qgh62J4
      doubleprecision rrqg2qgh62J5
      doubleprecision rrqg2qgh62J6
      doubleprecision rrqg2qgh62J7
      doubleprecision rrqg2qgh63J1
      doubleprecision rrqg2qgh63J2
      doubleprecision rrqg2qgh63J3
      doubleprecision rrqg2qgh63J4
      doubleprecision rrqg2qgh63J5
      doubleprecision rrqg2qgh63J6
      doubleprecision rrqg2qgh64J1
      doubleprecision rrqg2qgh64J2
      doubleprecision rrqg2qgh64J3
      doubleprecision rrqg2qgh64J4
      doubleprecision rrqg2qgh64J5
      doubleprecision rrqg2qgh64J6
      doubleprecision rrqg2qgh64J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh61J1
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
      t1 = 0.1D1 / S12
      t2 = t1 * S24
      t3 = S23 + S24 + S34
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t19 = S24 * t6
      t20 = 0.16D2 * t19
      t29 = 0.8D1 * S14
      t30 = S23 * S24
      t31 = S24 * S14
      t33 = 0.32D2 * t30 - 0.32D2 * t31
      t39 = S34 ** 2
      t48 = S23 ** 2
      t50 = S14 ** 2
      t56 = S24 * t50
      t58 = t30 * S14
      t74 = S13 * S14
      t76 = S23 * S13
      t78 = S13 ** 2
      t81 = S24 ** 2
      t108 = t81 * S24
      t140 = S12 ** 2
      rrqg2qgh61J1 = (-0.16D2 * t2 * t3 * t6 * t7 * s * t10 * z + 0.48D2
     # * t2 * t3 * t7 * t10 + ((-0.8D1 - t20) * t3 * S12 + (0.8D1 + 0.32
     #D2 * t19) * t3 * S34 + (-0.48D2 * S24 - t29 + t33 * t6) * t3 + ((-
     #0.4D1 - t20) * t3 * t39 + (-0.8D1 * S23 + t29 - t33 * t6) * t3 * S
     #34 + (-0.48D2 * t30 - 0.8D1 * t48 - 0.4D1 * t50 + 0.8D1 * S14 * S2
     #3 + (-0.16D2 * S24 * t48 - 0.16D2 * t56 + 0.32D2 * t58) * t6) * t3
     #) * t1) * s * z + (0.416D3 * S13 - 0.4D1 * S24) * t3 * S34 + (0.41
     #2D3 * t74 + 0.16D2 * t76 + 0.412D3 * t78 + 0.4D1 * t31 - 0.40D2 * 
     #t81) * t3 + ((0.4D1 * S13 + 0.8D1 * S24) * t3 * t39 + (0.4D1 * t78
     # + 0.4D1 * t30 - 0.16D2 * t31 + 0.412D3 * t76 + 0.8D1 * t74) * t3 
     #* S34 + (0.4D1 * t50 * S13 + 0.416D3 * t76 * S14 - 0.40D2 * t81 * 
     #S23 + 0.4D1 * S14 * t78 + 0.412D3 * S23 * t78 - 0.40D2 * t108 - 0.
     #4D1 * t58 + 0.8D1 * t56) * t3) * t1 + (-S24 * t3 * t39 * S34 + (0.
     #2D1 * t81 + 0.3D1 * t31) * t3 * t39 + (-0.4D1 * t81 * S14 - 0.2D1 
     #* t108 - 0.3D1 * t56) * t3 * S34 + (S24 * t50 * S14 + 0.2D1 * t81 
     #* t50 + 0.2D1 * t108 * S14) * t3) / t140) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh61J2
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
      t1 = S23 + S24 + S34
      t2 = t1 * S24
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t13 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = S24 * t35
      t42 = t15 * S14
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S24
      t64 = S13 * S14
      t66 = S23 * S13
      t70 = S24 ** 2
      t72 = S13 ** 2
      t99 = t70 * S23
      t135 = S12 ** 2
      rrqg2qgh61J2 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (t13 - 0.16D2 * S14 + t18 * t4) * t1 + ((0.4D1 - 0.16
     #D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t13 - t18 * t4) * t1 * S34 +
     # (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 - 0.16D2 * t40
     # + 0.32D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S13)
     # * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (-0.24D2 
     #* t64 - 0.8D1 * t66 + 0.2D1 * t16 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S13 + t54) * t1 * t25 + (0.60D2 * t
     #64 - 0.56D2 * t72 - 0.16D2 * t16 - 0.24D2 * t66 + 0.2D1 * t15) * t
     #1 * S34 + (-0.12D2 * t37 * S13 - 0.2D1 * t42 + 0.30D2 * t35 * S13 
     #+ 0.8D1 * t38 - 0.20D2 * S23 * t72 - 0.16D2 * t66 * S14 + 0.8D1 * 
     #t40 + 0.8D1 * t99 - 0.56D2 * S14 * t72 - 0.86D2 * t72 * S13) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t70 + 0.3D1 * t16) * t1 * t25
     # + (-0.2D1 * t99 - 0.4D1 * t70 * S14 - 0.3D1 * t40 - 0.2D1 * t38) 
     #* t1 * S34 + (S24 * t35 * S14 + 0.2D1 * t99 * S14 + 0.2D1 * t38 * 
     #S14 + 0.2D1 * t70 * t35) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh61J3
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
      t1 = S23 + S24 + S34
      t2 = t1 * S24
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t13 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = S24 * t35
      t42 = t15 * S14
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S24
      t64 = S13 * S14
      t66 = S23 * S13
      t70 = S24 ** 2
      t72 = S13 ** 2
      t99 = t70 * S23
      t135 = S12 ** 2
      rrqg2qgh61J3 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (t13 - 0.16D2 * S14 + t18 * t4) * t1 + ((0.4D1 - 0.16
     #D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t13 - t18 * t4) * t1 * S34 +
     # (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 - 0.16D2 * t40
     # + 0.32D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S13)
     # * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (-0.24D2 
     #* t64 - 0.8D1 * t66 + 0.2D1 * t16 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S13 + t54) * t1 * t25 + (0.60D2 * t
     #64 - 0.56D2 * t72 - 0.16D2 * t16 - 0.24D2 * t66 + 0.2D1 * t15) * t
     #1 * S34 + (-0.12D2 * t37 * S13 - 0.2D1 * t42 + 0.30D2 * t35 * S13 
     #+ 0.8D1 * t38 - 0.20D2 * S23 * t72 - 0.16D2 * t66 * S14 + 0.8D1 * 
     #t40 + 0.8D1 * t99 - 0.56D2 * S14 * t72 - 0.86D2 * t72 * S13) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t70 + 0.3D1 * t16) * t1 * t25
     # + (-0.2D1 * t99 - 0.4D1 * t70 * S14 - 0.3D1 * t40 - 0.2D1 * t38) 
     #* t1 * S34 + (S24 * t35 * S14 + 0.2D1 * t99 * S14 + 0.2D1 * t38 * 
     #S14 + 0.2D1 * t70 * t35) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh61J4
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
      t1 = S23 + S24 + S34
      t2 = t1 * S24
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t13 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = S24 * t35
      t42 = t15 * S14
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S24
      t64 = S13 * S14
      t66 = S23 * S13
      t70 = S24 ** 2
      t72 = S13 ** 2
      t99 = t70 * S23
      t135 = S12 ** 2
      rrqg2qgh61J4 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (t13 - 0.16D2 * S14 + t18 * t4) * t1 + ((0.4D1 - 0.16
     #D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t13 - t18 * t4) * t1 * S34 +
     # (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 - 0.16D2 * t40
     # + 0.32D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S13)
     # * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (-0.24D2 
     #* t64 - 0.8D1 * t66 + 0.2D1 * t16 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S13 + t54) * t1 * t25 + (0.60D2 * t
     #64 - 0.56D2 * t72 - 0.16D2 * t16 - 0.24D2 * t66 + 0.2D1 * t15) * t
     #1 * S34 + (-0.12D2 * t37 * S13 - 0.2D1 * t42 + 0.30D2 * t35 * S13 
     #+ 0.8D1 * t38 - 0.20D2 * S23 * t72 - 0.16D2 * t66 * S14 + 0.8D1 * 
     #t40 + 0.8D1 * t99 - 0.56D2 * S14 * t72 - 0.86D2 * t72 * S13) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t70 + 0.3D1 * t16) * t1 * t25
     # + (-0.2D1 * t99 - 0.4D1 * t70 * S14 - 0.3D1 * t40 - 0.2D1 * t38) 
     #* t1 * S34 + (S24 * t35 * S14 + 0.2D1 * t99 * S14 + 0.2D1 * t38 * 
     #S14 + 0.2D1 * t70 * t35) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh61J5
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
      t1 = S23 + S24 + S34
      t2 = t1 * S24
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t13 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = S24 * t35
      t42 = t15 * S14
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S24
      t64 = S13 * S14
      t66 = S23 * S13
      t70 = S24 ** 2
      t72 = S13 ** 2
      t99 = t70 * S23
      t135 = S12 ** 2
      rrqg2qgh61J5 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (t13 - 0.16D2 * S14 + t18 * t4) * t1 + ((0.4D1 - 0.16
     #D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t13 - t18 * t4) * t1 * S34 +
     # (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 - 0.16D2 * t40
     # + 0.32D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S13)
     # * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (-0.24D2 
     #* t64 - 0.8D1 * t66 + 0.2D1 * t16 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S13 + t54) * t1 * t25 + (0.60D2 * t
     #64 - 0.56D2 * t72 - 0.16D2 * t16 - 0.24D2 * t66 + 0.2D1 * t15) * t
     #1 * S34 + (-0.12D2 * t37 * S13 - 0.2D1 * t42 + 0.30D2 * t35 * S13 
     #+ 0.8D1 * t38 - 0.20D2 * S23 * t72 - 0.16D2 * t66 * S14 + 0.8D1 * 
     #t40 + 0.8D1 * t99 - 0.56D2 * S14 * t72 - 0.86D2 * t72 * S13) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t70 + 0.3D1 * t16) * t1 * t25
     # + (-0.2D1 * t99 - 0.4D1 * t70 * S14 - 0.3D1 * t40 - 0.2D1 * t38) 
     #* t1 * S34 + (S24 * t35 * S14 + 0.2D1 * t99 * S14 + 0.2D1 * t38 * 
     #S14 + 0.2D1 * t70 * t35) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh61J6
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
      t1 = 0.1D1 / S12
      t2 = t1 * S24
      t3 = S23 + S24 + S34
      t7 = s ** 2
      t10 = z ** 2
      t28 = S34 ** 2
      t36 = S23 * S24
      t38 = S23 ** 2
      t42 = S14 ** 2
      t61 = S13 * S14
      t63 = S23 * S13
      t68 = S13 ** 2
      t70 = S24 ** 2
      t84 = t70 * S24
      t88 = t70 * S23
      t98 = S24 * t38
      t119 = S12 ** 2
      rrqg2qgh61J6 = (0.16D2 * t2 * t3 / (S12 + S13 + S23) * t7 * s * t1
     #0 * z - 0.48D2 * t2 * t3 * t7 * t10 + (0.8D1 * t3 * S12 + 0.8D1 * 
     #t3 * S34 + (0.16D2 * S23 - 0.8D1 * S14 + 0.48D2 * S24) * t3 + (0.8
     #D1 * t3 * t28 + (-0.16D2 * S14 - 0.8D1 * S23) * t3 * S34 + (0.48D2
     # * t36 + 0.8D1 * t38 + 0.8D1 * S14 * S23 + 0.8D1 * t42) * t3) * t1
     #) * s * z + (0.8D1 * S24 - 0.12D2 * S13) * t3 * S12 + (0.2D1 * S24
     # - 0.432D3 * S13) * t3 * S34 + (-0.436D3 * t61 - 0.24D2 * t63 - 0.
     #2D1 * S24 * S14 + 0.16D2 * t36 - 0.432D3 * t68 + 0.48D2 * t70) * t
     #3 + (0.26D2 * S13 * t3 * t28 + (0.52D2 * t61 - 0.2D1 * t36 - 0.60D
     #2 * t68 - 0.436D3 * t63) * t3 * S34 + (0.40D2 * t84 - 0.60D2 * S14
     # * t68 + 0.48D2 * t88 - 0.12D2 * t38 * S13 + 0.26D2 * t42 * S13 + 
     #0.2D1 * t36 * S14 - 0.432D3 * S23 * t68 + 0.8D1 * t98 - 0.86D2 * t
     #68 * S13 - 0.432D3 * t63 * S14) * t3) * t1 + ((-0.2D1 * t88 - 0.2D
     #1 * t98 + 0.2D1 * t84) * t3 * S34 + (0.2D1 * t88 * S14 + 0.2D1 * t
     #98 * S14 - 0.2D1 * t84 * S14) * t3) / t119) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J1
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
      t1 = S23 + S24 + S34
      t2 = t1 * S24
      t3 = t2 * S34
      t7 = 0.1D1 / S12
      t12 = 0.1D1 / (S12 + S14 + S24)
      t13 = S12 ** 2
      t17 = S23 * S24
      t19 = S24 ** 2
      t21 = S24 * S14
      t32 = t19 * S23
      t34 = t17 * S14
      t36 = S14 ** 2
      t37 = S24 * t36
      t38 = S23 ** 2
      t39 = S24 * t38
      t41 = t19 * S24
      t43 = t19 * S14
      t50 = S34 ** 2
      t56 = S23 * S13
      t58 = S13 * S14
      t60 = S13 ** 2
      t73 = t36 * S13
      t74 = t38 * S13
      t76 = t60 * S13
      t78 = t56 * S14
      t80 = S23 * t60
      t82 = S14 * t60
      t109 = t60 ** 2
      t114 = t36 * S14
      t127 = t38 * S23
      t132 = t19 ** 2
      t139 = -0.4D1 * t73 * S23 + 0.14D2 / 0.3D1 * t58 * t38 - 0.28D2 / 
     #0.9D1 * t32 * S14 + 0.14D2 / 0.3D1 * t39 * S14 + t109 - 0.4D1 * t7
     #6 * S23 + 0.6D1 * t60 * t38 + 0.7D1 / 0.9D1 * S13 * t114 + t60 * t
     #36 - 0.4D1 * t41 * S23 + 0.6D1 * t19 * t38 + 0.7D1 / 0.9D1 * t76 *
     # S14 - 0.4D1 * t37 * S23 + t19 * t36 - 0.4D1 * S13 * t127 - 0.28D2
     # / 0.9D1 * t82 * S23 + t132 + 0.7D1 / 0.9D1 * S24 * t114 + 0.7D1 /
     # 0.9D1 * t41 * S14 - 0.4D1 * S24 * t127
      t141 = -0.91D2 / 0.18D2 * t82 - 0.26D2 / 0.3D1 * t39 + t41 / 0.3D1
     # + 0.20D2 / 0.3D1 * t32 - 0.82D2 / 0.9D1 * t80 - 0.47D2 / 0.36D2 *
     # t37 - 0.100D3 / 0.9D1 * t78 + 0.32D2 / 0.9D1 * t34 + 0.37D2 / 0.9
     #D1 * t43 - 0.5D1 / 0.18D2 * t76 - 0.16D2 / 0.3D1 * t73 - 0.14D2 / 
     #0.9D1 * t74 + t139 * t12
      rrqg2qgh62J1 = ((0.32D2 / 0.9D1 * t3 - 0.32D2 / 0.9D1 * t2 * S14) 
     #* t7 * s * z - t2 * t12 * t13 + (-0.187D3 / 0.9D1 * S24 + (-0.4D1 
     #* t17 + 0.3D1 * t19 + 0.7D1 / 0.9D1 * t21) * t12) * t1 * S12 - 0.8
     #D1 / 0.3D1 * t3 + (-0.4D1 / 0.9D1 * t19 - 0.220D3 / 0.9D1 * t17 + 
     #0.8D1 / 0.3D1 * t21 + (0.8D1 * t32 + 0.28D2 / 0.9D1 * t34 - t37 - 
     #0.6D1 * t39 - 0.3D1 * t41 - 0.14D2 / 0.9D1 * t43) * t12) * t1 + (-
     #S13 * t1 * t12 * t50 * S34 + (-0.47D2 / 0.36D2 * S24 - 0.215D3 / 0
     #.18D2 * S13 + (-0.4D1 * t56 + 0.7D1 / 0.9D1 * t58 + 0.3D1 * t60) *
     # t12) * t1 * t50 + (-0.37D2 / 0.9D1 * t19 - 0.146D3 / 0.9D1 * t56 
     #+ 0.47D2 / 0.18D2 * t21 - 0.265D3 / 0.18D2 * t58 - 0.82D2 / 0.9D1 
     #* t60 - 0.32D2 / 0.9D1 * t17 + (-t73 - 0.6D1 * t74 - 0.3D1 * t76 +
     # 0.28D2 / 0.9D1 * t78 + 0.8D1 * t80 - 0.14D2 / 0.9D1 * t82) * t12)
     # * t1 * S34 + t141 * t1) * t7 + (-0.4D1 / 0.9D1 * t114 * S23 * S24
     # - 0.4D1 / 0.9D1 * S14 * t127 * S24) * t12 * t1 / t13) / pi * wd /
     # z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J2
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
      t1 = S23 + S24 + S34
      t2 = t1 * S24
      t7 = 0.1D1 / S12
      t12 = 0.1D1 / (S12 + S14 + S24)
      t13 = S12 ** 2
      t21 = S23 * S24
      t23 = S24 ** 2
      t25 = S24 * S14
      t39 = S13 ** 2
      t43 = S13 * S14
      t46 = S23 * S13
      t50 = t23 * S23
      t52 = t21 * S14
      t54 = S23 ** 2
      t55 = S24 * t54
      t57 = t23 * S24
      t59 = S14 ** 2
      t60 = S24 * t59
      t61 = t23 * S14
      t68 = S34 ** 2
      t89 = t59 * S13
      t90 = S23 * t39
      t92 = t39 * S13
      t94 = t39 * S14
      t96 = t46 * S14
      t98 = t54 * S13
      t127 = t39 ** 2
      t132 = t59 * S14
      t150 = t23 ** 2
      t152 = 0.28D2 / 0.9D1 * t89 * S23 - 0.44D2 / 0.9D1 * t43 * t54 + 0
     #.8D1 / 0.9D1 * t50 * S14 - 0.44D2 / 0.9D1 * t55 * S14 + t127 - 0.4
     #D1 * t92 * S23 + 0.4D1 * t39 * t54 - 0.7D1 / 0.9D1 * S13 * t132 - 
     #t39 * t59 - 0.4D1 * t57 * S23 + 0.4D1 * t23 * t54 + 0.7D1 / 0.9D1 
     #* t92 * S14 + 0.28D2 / 0.9D1 * S23 * t60 + 0.7D1 / 0.9D1 * t57 * S
     #14 - 0.7D1 / 0.9D1 * S24 * t132 + 0.8D1 / 0.9D1 * t94 * S23 + t150
     # - t23 * t59
      t154 = -0.4D1 / 0.3D1 * t55 - 0.20D2 / 0.9D1 * t98 - 0.37D2 / 0.18
     #D2 * t94 - 0.9D1 / 0.2D1 * t89 + 0.7D1 / 0.9D1 * t57 + 0.76D2 / 0.
     #9D1 * t50 - 0.2D1 * t90 + 0.7D1 / 0.36D2 * t60 - 0.28D2 / 0.9D1 * 
     #t96 - 0.92D2 / 0.9D1 * t52 - 0.17D2 / 0.9D1 * t61 + 0.13D2 / 0.9D1
     # * t92 - 0.16D2 / 0.9D1 * t54 * S14 + t152 * t12
      rrqg2qgh62J2 = ((-0.32D2 / 0.9D1 * t2 * S34 + 0.32D2 / 0.9D1 * t2 
     #* S14) * t7 * s * z - t2 * t12 * t13 + (0.16D2 / 0.9D1 * t1 * S34 
     #+ (-0.34D2 / 0.9D1 * S13 - 0.16D2 / 0.9D1 * S14 - 0.13D2 / 0.3D1 *
     # S24 + (-0.4D1 * t21 + 0.3D1 * t23 + 0.7D1 / 0.9D1 * t25) * t12) *
     # t1) * S12 + (-0.64D2 / 0.9D1 * S13 + 0.32D2 / 0.9D1 * S23 + 0.136
     #D3 / 0.9D1 * S24) * t1 * S34 + (-0.64D2 / 0.9D1 * t39 - 0.20D2 / 0
     #.3D1 * t21 - 0.136D3 / 0.9D1 * t25 - 0.64D2 / 0.9D1 * t43 + 0.148D
     #3 / 0.9D1 * t23 - 0.6D1 * t46 - 0.32D2 / 0.9D1 * S14 * S23 + (0.8D
     #1 * t50 - 0.8D1 / 0.9D1 * t52 - 0.4D1 * t55 - 0.3D1 * t57 + t60 - 
     #0.14D2 / 0.9D1 * t61) * t12) * t1 + (-S13 * t1 * t12 * t68 * S34 +
     # (0.7D1 / 0.36D2 * S24 + 0.28D2 / 0.9D1 * S13 + (-0.4D1 * t46 + 0.
     #7D1 / 0.9D1 * t43 + 0.3D1 * t39) * t12) * t1 * t68 + (0.17D2 / 0.9
     #D1 * t23 + 0.16D2 / 0.9D1 * t54 - 0.10D2 / 0.9D1 * t46 - 0.7D1 / 0
     #.18D2 * t25 - 0.43D2 / 0.18D2 * t43 + 0.23D2 / 0.3D1 * t39 + 0.92D
     #2 / 0.9D1 * t21 + (t89 + 0.8D1 * t90 - 0.3D1 * t92 - 0.14D2 / 0.9D
     #1 * t94 - 0.8D1 / 0.9D1 * t96 - 0.4D1 * t98) * t12) * t1 * S34 + t
     #154 * t1) * t7 - 0.8D1 / 0.9D1 * t59 * t54 * t1 * S24 * t12 / t13)
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J3
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
      t1 = S23 + S24 + S34
      t2 = S14 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t20 = S24 * S14
      t21 = S14 ** 2
      t29 = t1 * S24
      t30 = S12 ** 2
      t36 = 0.16D2 / 0.9D1 * S14
      t38 = S23 * S24
      t40 = S24 ** 2
      t49 = S34 ** 2
      t59 = 0.16D2 / 0.3D1 * S14 * S23
      t62 = S13 * S14
      t66 = S23 * S13
      t68 = S13 ** 2
      t70 = t40 * S23
      t72 = t38 * S14
      t74 = S23 ** 2
      t75 = S24 * t74
      t77 = t40 * S24
      t79 = S24 * t21
      t80 = t40 * S14
      t111 = t21 * S13
      t112 = S23 * t68
      t114 = t68 * S13
      t116 = S14 * t68
      t118 = t66 * S14
      t120 = t74 * S13
      t130 = t21 * S14
      t153 = t40 ** 2
      t154 = t68 ** 2
      t176 = -0.44D2 / 0.9D1 * t79 * S23 - 0.44D2 / 0.9D1 * t111 * S23 +
     # 0.8D1 / 0.9D1 * t70 * S14 - 0.8D1 / 0.9D1 * t75 * S14 - t40 * t21
     # + t153 + t154 - 0.4D1 * t114 * S23 + 0.4D1 * t68 * t74 + 0.29D2 /
     # 0.9D1 * S13 * t130 - t68 * t21 - 0.8D1 / 0.9D1 * t62 * t74 + 0.4D
     #1 * t40 * t74 + 0.7D1 / 0.9D1 * t114 * S14 + 0.7D1 / 0.9D1 * t77 *
     # S14 + 0.8D1 / 0.9D1 * t116 * S23 + 0.29D2 / 0.9D1 * S24 * t130 - 
     #0.4D1 * t77 * S23
      t178 = 0.44D2 / 0.9D1 * t70 - 0.10D2 * t72 - 0.28D2 / 0.9D1 * t120
     # + 0.28D2 / 0.9D1 * t130 + 0.259D3 / 0.36D2 * t79 - 0.28D2 / 0.9D1
     # * t75 + 0.14527D5 / 0.18D2 * t116 - 0.18598D5 / 0.9D1 * t112 - 0.
     #6200D4 / 0.3D1 * t118 + 0.89D2 / 0.9D1 * t114 - t77 + 0.5D1 / 0.3D
     #1 * t80 + 0.8D1 / 0.3D1 * S23 * t21 + 0.14315D5 / 0.18D2 * t111 - 
     #0.16D2 / 0.3D1 * t74 * S14 + t176 * t6
      rrqg2qgh62J3 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.3D1 * t20 - 0.32D2 / 0.3D1 * t21) * t1 * t3) * s * z - 
     #t29 * t6 * t30 + (0.16D2 / 0.9D1 * t1 * S34 + (-0.55D2 / 0.9D1 * S
     #24 + t36 - 0.34D2 / 0.9D1 * S13 + (-0.4D1 * t38 + 0.3D1 * t40 + 0.
     #7D1 / 0.9D1 * t20) * t6) * t1) * S12 + 0.8D1 / 0.9D1 * t1 * t49 + 
     #(-t36 + 0.136D3 / 0.9D1 * S24 - 0.6380D4 / 0.9D1 * S13 + 0.16D2 / 
     #0.3D1 * S23) * t1 * S34 + (-t59 + 0.8D1 * t21 - 0.8D1 * t20 - 0.63
     #76D4 / 0.9D1 * t62 - 0.92D2 / 0.9D1 * t38 + 0.116D3 / 0.9D1 * t40 
     #- 0.74D2 / 0.9D1 * t66 - 0.6388D4 / 0.9D1 * t68 + (0.8D1 * t70 - 0
     #.8D1 / 0.9D1 * t72 - 0.4D1 * t75 - 0.3D1 * t77 + t79 - 0.14D2 / 0.
     #9D1 * t80) * t6) * t1 + ((0.4D1 / 0.9D1 - S13 * t6) * t1 * t49 * S
     #34 + (S24 / 0.12D2 + 0.8D1 / 0.3D1 * S23 - 0.4D1 / 0.3D1 * S14 - 0
     #.7024D4 / 0.9D1 * S13 + (-0.4D1 * t66 + 0.7D1 / 0.9D1 * t62 + 0.3D
     #1 * t68) * t6) * t1 * t49 + (-t59 + 0.25D2 / 0.2D1 * t62 + 0.17D2 
     #/ 0.9D1 * t40 + 0.4D1 / 0.3D1 * t21 + 0.16D2 / 0.3D1 * t74 - 0.691
     #1D4 / 0.9D1 * t68 - 0.18562D5 / 0.9D1 * t66 - t20 / 0.6D1 + 0.10D2
     # * t38 + (t111 + 0.8D1 * t112 - 0.3D1 * t114 - 0.14D2 / 0.9D1 * t1
     #16 - 0.8D1 / 0.9D1 * t118 - 0.4D1 * t120) * t6) * t1 * S34 + t178 
     #* t1) * t3 + (-0.4D1 / 0.9D1 * t29 * S23 * t49 - 0.4D1 / 0.9D1 * t
     #29 * t74 * S34 + (0.4D1 / 0.9D1 * t130 * S23 * S24 + 0.4D1 / 0.9D1
     # * S14 * t74 * S23 * S24 - 0.16D2 / 0.9D1 * t21 * t74 * S24) * t6 
     #* t1) / t30) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J4
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
      t1 = S23 + S24 + S34
      t2 = S14 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t20 = S24 * S14
      t21 = S14 ** 2
      t29 = t1 * S24
      t30 = S12 ** 2
      t36 = 0.16D2 / 0.9D1 * S14
      t38 = S23 * S24
      t40 = S24 ** 2
      t49 = S34 ** 2
      t59 = 0.16D2 / 0.3D1 * S14 * S23
      t62 = S13 * S14
      t66 = S23 * S13
      t68 = S13 ** 2
      t70 = t40 * S23
      t72 = t38 * S14
      t74 = S23 ** 2
      t75 = S24 * t74
      t77 = t40 * S24
      t79 = S24 * t21
      t80 = t40 * S14
      t111 = t21 * S13
      t112 = S23 * t68
      t114 = t68 * S13
      t116 = S14 * t68
      t118 = t66 * S14
      t120 = t74 * S13
      t130 = t21 * S14
      t153 = t40 ** 2
      t154 = t68 ** 2
      t176 = -0.44D2 / 0.9D1 * t79 * S23 - 0.44D2 / 0.9D1 * t111 * S23 +
     # 0.8D1 / 0.9D1 * t70 * S14 - 0.8D1 / 0.9D1 * t75 * S14 - t40 * t21
     # + t153 + t154 - 0.4D1 * t114 * S23 + 0.4D1 * t68 * t74 + 0.29D2 /
     # 0.9D1 * S13 * t130 - t68 * t21 - 0.8D1 / 0.9D1 * t62 * t74 + 0.4D
     #1 * t40 * t74 + 0.7D1 / 0.9D1 * t114 * S14 + 0.7D1 / 0.9D1 * t77 *
     # S14 + 0.8D1 / 0.9D1 * t116 * S23 + 0.29D2 / 0.9D1 * S24 * t130 - 
     #0.4D1 * t77 * S23
      t178 = 0.44D2 / 0.9D1 * t70 - 0.10D2 * t72 - 0.28D2 / 0.9D1 * t120
     # + 0.28D2 / 0.9D1 * t130 + 0.259D3 / 0.36D2 * t79 - 0.28D2 / 0.9D1
     # * t75 + 0.14527D5 / 0.18D2 * t116 - 0.18598D5 / 0.9D1 * t112 - 0.
     #6200D4 / 0.3D1 * t118 + 0.89D2 / 0.9D1 * t114 - t77 + 0.5D1 / 0.3D
     #1 * t80 + 0.8D1 / 0.3D1 * S23 * t21 + 0.14315D5 / 0.18D2 * t111 - 
     #0.16D2 / 0.3D1 * t74 * S14 + t176 * t6
      rrqg2qgh62J4 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.3D1 * t20 - 0.32D2 / 0.3D1 * t21) * t1 * t3) * s * z - 
     #t29 * t6 * t30 + (0.16D2 / 0.9D1 * t1 * S34 + (-0.55D2 / 0.9D1 * S
     #24 + t36 - 0.34D2 / 0.9D1 * S13 + (-0.4D1 * t38 + 0.3D1 * t40 + 0.
     #7D1 / 0.9D1 * t20) * t6) * t1) * S12 + 0.8D1 / 0.9D1 * t1 * t49 + 
     #(-t36 + 0.136D3 / 0.9D1 * S24 - 0.6380D4 / 0.9D1 * S13 + 0.16D2 / 
     #0.3D1 * S23) * t1 * S34 + (-t59 + 0.8D1 * t21 - 0.8D1 * t20 - 0.63
     #76D4 / 0.9D1 * t62 - 0.92D2 / 0.9D1 * t38 + 0.116D3 / 0.9D1 * t40 
     #- 0.74D2 / 0.9D1 * t66 - 0.6388D4 / 0.9D1 * t68 + (0.8D1 * t70 - 0
     #.8D1 / 0.9D1 * t72 - 0.4D1 * t75 - 0.3D1 * t77 + t79 - 0.14D2 / 0.
     #9D1 * t80) * t6) * t1 + ((0.4D1 / 0.9D1 - S13 * t6) * t1 * t49 * S
     #34 + (S24 / 0.12D2 + 0.8D1 / 0.3D1 * S23 - 0.4D1 / 0.3D1 * S14 - 0
     #.7024D4 / 0.9D1 * S13 + (-0.4D1 * t66 + 0.7D1 / 0.9D1 * t62 + 0.3D
     #1 * t68) * t6) * t1 * t49 + (-t59 + 0.25D2 / 0.2D1 * t62 + 0.17D2 
     #/ 0.9D1 * t40 + 0.4D1 / 0.3D1 * t21 + 0.16D2 / 0.3D1 * t74 - 0.691
     #1D4 / 0.9D1 * t68 - 0.18562D5 / 0.9D1 * t66 - t20 / 0.6D1 + 0.10D2
     # * t38 + (t111 + 0.8D1 * t112 - 0.3D1 * t114 - 0.14D2 / 0.9D1 * t1
     #16 - 0.8D1 / 0.9D1 * t118 - 0.4D1 * t120) * t6) * t1 * S34 + t178 
     #* t1) * t3 + (-0.4D1 / 0.9D1 * t29 * S23 * t49 - 0.4D1 / 0.9D1 * t
     #29 * t74 * S34 + (0.4D1 / 0.9D1 * t130 * S23 * S24 + 0.4D1 / 0.9D1
     # * S14 * t74 * S23 * S24 - 0.16D2 / 0.9D1 * t21 * t74 * S24) * t6 
     #* t1) / t30) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J5
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
      t1 = S23 + S24 + S34
      t2 = S14 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t20 = S24 * S14
      t21 = S14 ** 2
      t29 = t1 * S24
      t30 = S12 ** 2
      t36 = 0.16D2 / 0.9D1 * S14
      t38 = S23 * S24
      t40 = S24 ** 2
      t49 = S34 ** 2
      t59 = 0.16D2 / 0.3D1 * S14 * S23
      t62 = S13 * S14
      t66 = S23 * S13
      t68 = S13 ** 2
      t70 = t40 * S23
      t72 = t38 * S14
      t74 = S23 ** 2
      t75 = S24 * t74
      t77 = t40 * S24
      t79 = S24 * t21
      t80 = t40 * S14
      t111 = t21 * S13
      t112 = S23 * t68
      t114 = t68 * S13
      t116 = S14 * t68
      t118 = t66 * S14
      t120 = t74 * S13
      t130 = t21 * S14
      t153 = t40 ** 2
      t154 = t68 ** 2
      t176 = -0.44D2 / 0.9D1 * t79 * S23 - 0.44D2 / 0.9D1 * t111 * S23 +
     # 0.8D1 / 0.9D1 * t70 * S14 - 0.8D1 / 0.9D1 * t75 * S14 - t40 * t21
     # + t153 + t154 - 0.4D1 * t114 * S23 + 0.4D1 * t68 * t74 + 0.29D2 /
     # 0.9D1 * S13 * t130 - t68 * t21 - 0.8D1 / 0.9D1 * t62 * t74 + 0.4D
     #1 * t40 * t74 + 0.7D1 / 0.9D1 * t114 * S14 + 0.7D1 / 0.9D1 * t77 *
     # S14 + 0.8D1 / 0.9D1 * t116 * S23 + 0.29D2 / 0.9D1 * S24 * t130 - 
     #0.4D1 * t77 * S23
      t178 = 0.44D2 / 0.9D1 * t70 - 0.10D2 * t72 - 0.28D2 / 0.9D1 * t120
     # + 0.28D2 / 0.9D1 * t130 + 0.259D3 / 0.36D2 * t79 - 0.28D2 / 0.9D1
     # * t75 + 0.14527D5 / 0.18D2 * t116 - 0.18598D5 / 0.9D1 * t112 - 0.
     #6200D4 / 0.3D1 * t118 + 0.89D2 / 0.9D1 * t114 - t77 + 0.5D1 / 0.3D
     #1 * t80 + 0.8D1 / 0.3D1 * S23 * t21 + 0.14315D5 / 0.18D2 * t111 - 
     #0.16D2 / 0.3D1 * t74 * S14 + t176 * t6
      rrqg2qgh62J5 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.3D1 * t20 - 0.32D2 / 0.3D1 * t21) * t1 * t3) * s * z - 
     #t29 * t6 * t30 + (0.16D2 / 0.9D1 * t1 * S34 + (-0.55D2 / 0.9D1 * S
     #24 + t36 - 0.34D2 / 0.9D1 * S13 + (-0.4D1 * t38 + 0.3D1 * t40 + 0.
     #7D1 / 0.9D1 * t20) * t6) * t1) * S12 + 0.8D1 / 0.9D1 * t1 * t49 + 
     #(-t36 + 0.136D3 / 0.9D1 * S24 - 0.6380D4 / 0.9D1 * S13 + 0.16D2 / 
     #0.3D1 * S23) * t1 * S34 + (-t59 + 0.8D1 * t21 - 0.8D1 * t20 - 0.63
     #76D4 / 0.9D1 * t62 - 0.92D2 / 0.9D1 * t38 + 0.116D3 / 0.9D1 * t40 
     #- 0.74D2 / 0.9D1 * t66 - 0.6388D4 / 0.9D1 * t68 + (0.8D1 * t70 - 0
     #.8D1 / 0.9D1 * t72 - 0.4D1 * t75 - 0.3D1 * t77 + t79 - 0.14D2 / 0.
     #9D1 * t80) * t6) * t1 + ((0.4D1 / 0.9D1 - S13 * t6) * t1 * t49 * S
     #34 + (S24 / 0.12D2 + 0.8D1 / 0.3D1 * S23 - 0.4D1 / 0.3D1 * S14 - 0
     #.7024D4 / 0.9D1 * S13 + (-0.4D1 * t66 + 0.7D1 / 0.9D1 * t62 + 0.3D
     #1 * t68) * t6) * t1 * t49 + (-t59 + 0.25D2 / 0.2D1 * t62 + 0.17D2 
     #/ 0.9D1 * t40 + 0.4D1 / 0.3D1 * t21 + 0.16D2 / 0.3D1 * t74 - 0.691
     #1D4 / 0.9D1 * t68 - 0.18562D5 / 0.9D1 * t66 - t20 / 0.6D1 + 0.10D2
     # * t38 + (t111 + 0.8D1 * t112 - 0.3D1 * t114 - 0.14D2 / 0.9D1 * t1
     #16 - 0.8D1 / 0.9D1 * t118 - 0.4D1 * t120) * t6) * t1 * S34 + t178 
     #* t1) * t3 + (-0.4D1 / 0.9D1 * t29 * S23 * t49 - 0.4D1 / 0.9D1 * t
     #29 * t74 * S34 + (0.4D1 / 0.9D1 * t130 * S23 * S24 + 0.4D1 / 0.9D1
     # * S14 * t74 * S23 * S24 - 0.16D2 / 0.9D1 * t21 * t74 * S24) * t6 
     #* t1) / t30) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J6
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
      t1 = S23 + S24 + S34
      t2 = S14 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t20 = t1 * S24
      t23 = S14 ** 2
      t25 = S24 * S14
      t38 = 0.16D2 / 0.9D1 * S14
      t43 = S34 ** 2
      t52 = S23 * S24
      t54 = S13 * S14
      t57 = 0.16D2 / 0.3D1 * S14 * S23
      t58 = S13 ** 2
      t60 = S23 * S13
      t64 = S24 ** 2
      t66 = S24 * t23
      t68 = S23 ** 2
      t69 = S24 * t68
      t71 = t52 * S14
      t95 = t23 * S13
      t97 = t68 * S13
      t99 = t60 * S14
      t117 = t23 * S14
      t126 = S14 * t58
      t128 = t64 * S23
      t140 = S23 * t68
      t159 = -0.2D1 * t64 * t68 - 0.8D1 / 0.9D1 * t66 * S23 - 0.8D1 / 0.
     #9D1 * t95 * S23 - 0.50D2 / 0.9D1 * t54 * t68 + 0.4D1 * t126 * S23 
     #+ 0.4D1 * S13 * t140 + 0.4D1 * S24 * t140 - 0.50D2 / 0.9D1 * t69 *
     # S14 - 0.2D1 * t58 * t23 + 0.4D1 * t128 * S14 - 0.2D1 * t64 * t23 
     #+ 0.22D2 / 0.9D1 * S13 * t117 + 0.22D2 / 0.9D1 * S24 * t117 - 0.2D
     #1 * t58 * t68
      t161 = 0.8D1 / 0.3D1 * S23 * t23 - 0.22D2 / 0.9D1 * t64 * S14 + 0.
     #17D2 / 0.2D1 * t66 + 0.50D2 / 0.9D1 * t69 - 0.16D2 / 0.3D1 * S14 *
     # t68 + 0.14411D5 / 0.18D2 * t95 - 0.6172D4 / 0.3D1 * S23 * t58 + 0
     #.28D2 / 0.9D1 * t117 + 0.61D2 / 0.6D1 * t58 * S13 - 0.14D2 / 0.9D1
     # * t97 - 0.4D1 / 0.3D1 * t64 * S24 - 0.122D3 / 0.9D1 * t71 - 0.185
     #00D5 / 0.9D1 * t99 + 0.7309D4 / 0.9D1 * t126 - 0.16D2 / 0.9D1 * t1
     #28 + t159 * t6
      t184 = S12 ** 2
      rrqg2qgh62J6 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.9D1 * t20 * S34 + (-0.32D2 / 0.3D1 * t23 - 0.64D2 / 0.9
     #D1 * t25) * t1) * t3) * s * z + (0.16D2 / 0.9D1 * t1 * S34 + (0.44
     #D2 / 0.3D1 * S24 - 0.34D2 / 0.9D1 * S13 + t38) * t1) * S12 + 0.8D1
     # / 0.9D1 * t1 * t43 + (0.160D3 / 0.9D1 * S24 + 0.16D2 / 0.3D1 * S2
     #3 - 0.6380D4 / 0.9D1 * S13 - t38) * t1 * S34 + (0.128D3 / 0.9D1 * 
     #t52 - 0.6376D4 / 0.9D1 * t54 - t57 - 0.6388D4 / 0.9D1 * t58 - 0.74
     #D2 / 0.9D1 * t60 + 0.8D1 * t23 - 0.32D2 / 0.3D1 * t25 + 0.40D2 / 0
     #.3D1 * t64 + (0.2D1 * t66 + 0.2D1 * t69 - 0.4D1 * t71) * t6) * t1 
     #+ (0.4D1 / 0.9D1 * t1 * t43 * S34 + (-0.4D1 / 0.3D1 * S14 + 0.25D2
     # / 0.18D2 * S24 + 0.8D1 / 0.3D1 * S23 - 0.1537D4 / 0.2D1 * S13) * 
     #t1 * t43 + (0.16D2 / 0.3D1 * t68 - 0.6829D4 / 0.9D1 * t58 + 0.4D1 
     #/ 0.3D1 * t23 - 0.18416D5 / 0.9D1 * t60 - 0.25D2 / 0.9D1 * t25 + 0
     #.6D1 * t64 + 0.245D3 / 0.9D1 * t54 - t57 + 0.122D3 / 0.9D1 * t52 +
     # (0.2D1 * t95 + 0.2D1 * t97 - 0.4D1 * t99) * t6) * t1 * S34 + t161
     # * t1) * t3 + (-0.4D1 / 0.9D1 * t20 * S23 * t43 - 0.4D1 / 0.9D1 * 
     #t20 * t68 * S34 + (-0.16D2 / 0.9D1 * t23 * t68 * S24 + 0.8D1 / 0.9
     #D1 * t117 * S23 * S24 + 0.8D1 / 0.9D1 * S14 * t140 * S24) * t6 * t
     #1) / t184) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh62J7
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
      t1 = S23 + S24 + S34
      t2 = S14 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t20 = t1 * S24
      t23 = S24 * S14
      t25 = S14 ** 2
      t39 = S34 ** 2
      t48 = S13 * S14
      t51 = S23 * S13
      t54 = S13 ** 2
      t56 = S24 ** 2
      t58 = S23 * S24
      t60 = S14 * S23
      t81 = S23 ** 2
      t90 = S24 * t81
      t104 = S24 * t25
      t106 = t25 * S14
      t110 = t25 * S13
      t130 = 0.8D1 / 0.3D1 * S23 * t25 - 0.16D2 / 0.9D1 * t56 * S24 - 0.
     #16D2 / 0.9D1 * t90 + 0.76D2 / 0.9D1 * t54 * S13 - 0.32D2 / 0.9D1 *
     # t56 * S23 + 0.32D2 / 0.9D1 * t56 * S14 + 0.7282D4 / 0.9D1 * S14 *
     # t54 - 0.18572D5 / 0.9D1 * t51 * S14 - 0.18580D5 / 0.9D1 * S23 * t
     #54 + 0.7D1 * t104 + 0.28D2 / 0.9D1 * t106 - 0.32D2 / 0.9D1 * t81 *
     # S14 + 0.7198D4 / 0.9D1 * t110 - 0.8D1 / 0.9D1 * t81 * S13 + 0.2D1
     # / 0.9D1 * S14 * t58 + (0.4D1 * S13 * t106 - 0.8D1 * t110 * S23 + 
     #0.4D1 * S24 * t106 + 0.4D1 * t48 * t81 + 0.4D1 * t90 * S14 - 0.8D1
     # * t104 * S23) * t6
      t154 = S12 ** 2
      rrqg2qgh62J7 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (0
     #.32D2 / 0.9D1 * t20 * S34 + (-0.128D3 / 0.9D1 * t23 - 0.32D2 / 0.3
     #D1 * t25) * t1) * t3) * s * z + (-0.16D2 / 0.9D1 * S24 + 0.32D2 / 
     #0.9D1 * S14) * t1 * S12 + 0.8D1 / 0.9D1 * t1 * t39 + (0.16D2 / 0.9
     #D1 * S23 - 0.6316D4 / 0.9D1 * S13 - 0.16D2 / 0.9D1 * S14) * t1 * S
     #34 + (-0.2104D4 / 0.3D1 * t48 + 0.64D2 / 0.9D1 * t23 - 0.20D2 / 0.
     #9D1 * t51 + 0.8D1 * t25 - 0.2108D4 / 0.3D1 * t54 - 0.32D2 / 0.9D1 
     #* t56 - 0.32D2 / 0.9D1 * t58 - 0.16D2 / 0.9D1 * t60) * t1 + (0.4D1
     # / 0.9D1 * t1 * t39 * S34 + (-S24 / 0.9D1 - 0.4D1 / 0.3D1 * S14 + 
     #0.8D1 / 0.3D1 * S23 - 0.7052D4 / 0.9D1 * S13) * t1 * t39 + (-0.16D
     #2 / 0.3D1 * t60 - 0.6980D4 / 0.9D1 * t54 + 0.4D1 / 0.3D1 * t25 - 0
     #.2D1 / 0.9D1 * t58 + 0.134D3 / 0.9D1 * t48 + 0.2D1 / 0.9D1 * t23 -
     # 0.6184D4 / 0.3D1 * t51 + 0.32D2 / 0.9D1 * t81) * t1 * S34 + t130 
     #* t1) * t3 + (-0.4D1 / 0.9D1 * t20 * S23 * t39 - 0.4D1 / 0.9D1 * t
     #20 * t81 * S34 + (0.4D1 / 0.9D1 * S14 * t81 * S23 * S24 + 0.4D1 / 
     #0.9D1 * t106 * S23 * S24 - 0.8D1 / 0.9D1 * t25 * t81 * S24) * t6 *
     # t1) / t154) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh63J1
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
      t1 = 0.1D1 / S12
      t2 = t1 * S13
      t3 = S13 + S14 + S34
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t19 = S13 * t6
      t20 = 0.16D2 * t19
      t28 = 0.8D1 * S23
      t30 = S23 * S13
      t31 = S13 * S14
      t33 = -0.32D2 * t30 + 0.32D2 * t31
      t39 = S34 ** 2
      t47 = S14 ** 2
      t51 = S23 ** 2
      t54 = t30 * S14
      t56 = t51 * S13
      t74 = S23 * S24
      t77 = S13 ** 2
      t79 = S24 * S14
      t81 = S24 ** 2
      t110 = t77 * S13
      t140 = S12 ** 2
      rrqg2qgh63J1 = (-0.16D2 * t2 * t3 * t6 * t7 * s * t10 * z + 0.48D2
     # * t2 * t3 * t7 * t10 + ((-0.8D1 - t20) * t3 * S12 + (0.8D1 + 0.32
     #D2 * t19) * t3 * S34 + (-t28 - 0.48D2 * S13 + t33 * t6) * t3 + ((-
     #0.4D1 - t20) * t3 * t39 + (t28 - 0.8D1 * S14 - t33 * t6) * t3 * S3
     #4 + (-0.8D1 * t47 + 0.8D1 * S14 * S23 - 0.4D1 * t51 - 0.48D2 * t31
     # + (0.32D2 * t54 - 0.16D2 * t56 - 0.16D2 * t47 * S13) * t6) * t3) 
     #* t1) * s * z + (0.416D3 * S24 - 0.4D1 * S13) * t3 * S34 + (0.412D
     #3 * t74 + 0.4D1 * t30 - 0.40D2 * t77 + 0.16D2 * t79 + 0.412D3 * t8
     #1) * t3 + ((0.8D1 * S13 + 0.4D1 * S24) * t3 * t39 + (0.4D1 * t81 +
     # 0.412D3 * t79 + 0.4D1 * t31 + 0.8D1 * t74 - 0.16D2 * t30) * t3 * 
     #S34 + (0.4D1 * S24 * t51 + 0.4D1 * t81 * S23 + 0.416D3 * t74 * S14
     # - 0.4D1 * t54 + 0.412D3 * t81 * S14 - 0.40D2 * t77 * S14 + 0.8D1 
     #* t56 - 0.40D2 * t110) * t3) * t1 + (-S13 * t3 * t39 * S34 + (0.3D
     #1 * t30 + 0.2D1 * t77) * t3 * t39 + (-0.3D1 * t56 - 0.4D1 * t77 * 
     #S23 - 0.2D1 * t110) * t3 * S34 + (S13 * t51 * S23 + 0.2D1 * t77 * 
     #t51 + 0.2D1 * t110 * S23) * t3) / t140) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh63J2
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
      t1 = S13 + S14 + S34
      t2 = t1 * S13
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S23 * S13
      t16 = S13 * S14
      t18 = -0.32D2 * t15 + 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = t15 * S14
      t39 = t33 * S13
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t69 = S13 ** 2
      t72 = S24 ** 2
      t96 = t69 * S14
      t135 = S12 ** 2
      rrqg2qgh63J2 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (0.32D2 * t37 - 0.16D2 * t39
     # - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24)
     # * t1 * S12 + (-0.16D2 * S24 - 0.2D1 * S13) * t1 * S34 + (0.16D2 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.8D1 * t69 + 0.2D1 * t15 - 0.
     #20D2 * t72) * t1 + ((t54 + 0.30D2 * S24) * t1 * t25 + (0.60D2 * t6
     #7 - 0.24D2 * t65 - 0.16D2 * t15 - 0.56D2 * t72 + 0.2D1 * t16) * t1
     # * S34 + (-0.2D1 * t37 - 0.86D2 * t72 * S24 - 0.20D2 * t72 * S14 -
     # 0.12D2 * S24 * t41 + 0.8D1 * t39 + 0.8D1 * t96 + 0.8D1 * t42 - 0.
     #16D2 * t67 * S14 - 0.56D2 * t72 * S23 + 0.30D2 * S24 * t33) * t1) 
     #* t49 + (-t2 * t25 * S34 + (0.3D1 * t15 + 0.2D1 * t69) * t1 * t25 
     #+ (-0.2D1 * t96 - 0.4D1 * S23 * t69 - 0.3D1 * t39 - 0.2D1 * t42) *
     # t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t42 * S23 + 0.2D1 * t69 * t
     #33 + 0.2D1 * t96 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh63J3
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
      t1 = S13 + S14 + S34
      t2 = t1 * S13
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S23 * S13
      t16 = S13 * S14
      t18 = -0.32D2 * t15 + 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = t15 * S14
      t39 = t33 * S13
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t69 = S13 ** 2
      t72 = S24 ** 2
      t96 = t69 * S14
      t135 = S12 ** 2
      rrqg2qgh63J3 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (0.32D2 * t37 - 0.16D2 * t39
     # - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24)
     # * t1 * S12 + (-0.16D2 * S24 - 0.2D1 * S13) * t1 * S34 + (0.16D2 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.8D1 * t69 + 0.2D1 * t15 - 0.
     #20D2 * t72) * t1 + ((t54 + 0.30D2 * S24) * t1 * t25 + (0.60D2 * t6
     #7 - 0.24D2 * t65 - 0.16D2 * t15 - 0.56D2 * t72 + 0.2D1 * t16) * t1
     # * S34 + (-0.2D1 * t37 - 0.86D2 * t72 * S24 - 0.20D2 * t72 * S14 -
     # 0.12D2 * S24 * t41 + 0.8D1 * t39 + 0.8D1 * t96 + 0.8D1 * t42 - 0.
     #16D2 * t67 * S14 - 0.56D2 * t72 * S23 + 0.30D2 * S24 * t33) * t1) 
     #* t49 + (-t2 * t25 * S34 + (0.3D1 * t15 + 0.2D1 * t69) * t1 * t25 
     #+ (-0.2D1 * t96 - 0.4D1 * S23 * t69 - 0.3D1 * t39 - 0.2D1 * t42) *
     # t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t42 * S23 + 0.2D1 * t69 * t
     #33 + 0.2D1 * t96 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh63J4
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
      t1 = S13 + S14 + S34
      t2 = t1 * S13
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S23 * S13
      t16 = S13 * S14
      t18 = -0.32D2 * t15 + 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = t15 * S14
      t39 = t33 * S13
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t69 = S13 ** 2
      t72 = S24 ** 2
      t96 = t69 * S14
      t135 = S12 ** 2
      rrqg2qgh63J4 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (0.32D2 * t37 - 0.16D2 * t39
     # - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24)
     # * t1 * S12 + (-0.16D2 * S24 - 0.2D1 * S13) * t1 * S34 + (0.16D2 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.8D1 * t69 + 0.2D1 * t15 - 0.
     #20D2 * t72) * t1 + ((t54 + 0.30D2 * S24) * t1 * t25 + (0.60D2 * t6
     #7 - 0.24D2 * t65 - 0.16D2 * t15 - 0.56D2 * t72 + 0.2D1 * t16) * t1
     # * S34 + (-0.2D1 * t37 - 0.86D2 * t72 * S24 - 0.20D2 * t72 * S14 -
     # 0.12D2 * S24 * t41 + 0.8D1 * t39 + 0.8D1 * t96 + 0.8D1 * t42 - 0.
     #16D2 * t67 * S14 - 0.56D2 * t72 * S23 + 0.30D2 * S24 * t33) * t1) 
     #* t49 + (-t2 * t25 * S34 + (0.3D1 * t15 + 0.2D1 * t69) * t1 * t25 
     #+ (-0.2D1 * t96 - 0.4D1 * S23 * t69 - 0.3D1 * t39 - 0.2D1 * t42) *
     # t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t42 * S23 + 0.2D1 * t69 * t
     #33 + 0.2D1 * t96 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh63J5
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
      t1 = S13 + S14 + S34
      t2 = t1 * S13
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S23 * S13
      t16 = S13 * S14
      t18 = -0.32D2 * t15 + 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = t15 * S14
      t39 = t33 * S13
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t69 = S13 ** 2
      t72 = S24 ** 2
      t96 = t69 * S14
      t135 = S12 ** 2
      rrqg2qgh63J5 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (0.32D2 * t37 - 0.16D2 * t39
     # - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24)
     # * t1 * S12 + (-0.16D2 * S24 - 0.2D1 * S13) * t1 * S34 + (0.16D2 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.8D1 * t69 + 0.2D1 * t15 - 0.
     #20D2 * t72) * t1 + ((t54 + 0.30D2 * S24) * t1 * t25 + (0.60D2 * t6
     #7 - 0.24D2 * t65 - 0.16D2 * t15 - 0.56D2 * t72 + 0.2D1 * t16) * t1
     # * S34 + (-0.2D1 * t37 - 0.86D2 * t72 * S24 - 0.20D2 * t72 * S14 -
     # 0.12D2 * S24 * t41 + 0.8D1 * t39 + 0.8D1 * t96 + 0.8D1 * t42 - 0.
     #16D2 * t67 * S14 - 0.56D2 * t72 * S23 + 0.30D2 * S24 * t33) * t1) 
     #* t49 + (-t2 * t25 * S34 + (0.3D1 * t15 + 0.2D1 * t69) * t1 * t25 
     #+ (-0.2D1 * t96 - 0.4D1 * S23 * t69 - 0.3D1 * t39 - 0.2D1 * t42) *
     # t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t42 * S23 + 0.2D1 * t69 * t
     #33 + 0.2D1 * t96 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh63J6
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
      t1 = 0.1D1 / S12
      t2 = t1 * S13
      t3 = S13 + S14 + S34
      t7 = s ** 2
      t10 = z ** 2
      t28 = S34 ** 2
      t36 = S14 ** 2
      t40 = S13 * S14
      t42 = S23 ** 2
      t61 = S24 * S14
      t63 = S13 ** 2
      t66 = S23 * S13
      t68 = S23 * S24
      t70 = S24 ** 2
      t92 = t36 * S13
      t98 = t63 * S13
      t100 = S14 * t63
      t119 = S12 ** 2
      rrqg2qgh63J6 = (0.16D2 * t2 * t3 / (S12 + S14 + S24) * t7 * s * t1
     #0 * z - 0.48D2 * t2 * t3 * t7 * t10 + (0.8D1 * t3 * S12 + 0.8D1 * 
     #t3 * S34 + (0.48D2 * S13 + 0.16D2 * S14 - 0.8D1 * S23) * t3 + (0.8
     #D1 * t3 * t28 + (-0.16D2 * S23 - 0.8D1 * S14) * t3 * S34 + (0.8D1 
     #* t36 + 0.8D1 * S14 * S23 + 0.48D2 * t40 + 0.8D1 * t42) * t3) * t1
     #) * s * z + (0.8D1 * S13 - 0.12D2 * S24) * t3 * S12 + (0.2D1 * S13
     # - 0.432D3 * S24) * t3 * S34 + (-0.24D2 * t61 + 0.48D2 * t63 + 0.1
     #6D2 * t40 - 0.2D1 * t66 - 0.436D3 * t68 - 0.432D3 * t70) * t3 + (0
     #.26D2 * t3 * S24 * t28 + (0.52D2 * t68 - 0.436D3 * t61 - 0.60D2 * 
     #t70 - 0.2D1 * t40) * t3 * S34 + (0.2D1 * t66 * S14 - 0.432D3 * t70
     # * S14 - 0.86D2 * t70 * S24 - 0.60D2 * t70 * S23 + 0.8D1 * t92 - 0
     #.12D2 * S24 * t36 - 0.432D3 * S14 * t68 + 0.40D2 * t98 + 0.48D2 * 
     #t100 + 0.26D2 * S24 * t42) * t3) * t1 + ((-0.2D1 * t100 + 0.2D1 * 
     #t98 - 0.2D1 * t92) * t3 * S34 + (-0.2D1 * t98 * S23 + 0.2D1 * t92 
     #* S23 + 0.2D1 * t100 * S23) * t3) / t119) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J1
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
      t1 = S13 + S14 + S34
      t3 = S34 * t1 * S13
      t4 = t1 * S13
      t8 = 0.1D1 / S12
      t13 = 0.1D1 / (S12 + S13 + S23)
      t14 = S12 ** 2
      t18 = S23 * S13
      t20 = S13 ** 2
      t22 = S13 * S14
      t33 = t20 * S13
      t35 = S14 ** 2
      t36 = t35 * S13
      t38 = t18 * S14
      t40 = S23 ** 2
      t41 = t40 * S13
      t42 = S23 * t20
      t44 = S14 * t20
      t51 = S34 ** 2
      t57 = S24 * S14
      t59 = S23 * S24
      t61 = S24 ** 2
      t74 = t59 * S14
      t76 = S24 * t35
      t78 = t61 * S23
      t80 = t61 * S14
      t82 = t40 * S24
      t83 = t61 * S24
      t114 = t20 ** 2
      t115 = t40 * S23
      t123 = t35 * S14
      t137 = t61 ** 2
      t140 = -0.28D2 / 0.9D1 * t78 * S14 - 0.4D1 * t82 * S14 + 0.14D2 / 
     #0.3D1 * t76 * S23 + 0.14D2 / 0.3D1 * t36 * S23 - 0.4D1 * t22 * t40
     # - 0.28D2 / 0.9D1 * t44 * S23 + t114 + 0.7D1 / 0.9D1 * S13 * t115 
     #+ 0.7D1 / 0.9D1 * S24 * t115 + 0.7D1 / 0.9D1 * t33 * S23 + t20 * t
     #40 - 0.4D1 * S13 * t123 + 0.6D1 * t20 * t35 + t61 * t40 - 0.4D1 * 
     #t33 * S14 + 0.7D1 / 0.9D1 * t83 * S23 - 0.4D1 * S24 * t123 + 0.6D1
     # * t61 * t35 + t137 - 0.4D1 * t83 * S14
      t142 = -0.82D2 / 0.9D1 * t80 - 0.47D2 / 0.36D2 * t41 - 0.26D2 / 0.
     #3D1 * t36 - 0.91D2 / 0.18D2 * t78 + 0.37D2 / 0.9D1 * t42 - 0.16D2 
     #/ 0.3D1 * t82 - 0.14D2 / 0.9D1 * t76 + 0.20D2 / 0.3D1 * t44 + 0.32
     #D2 / 0.9D1 * t38 + t33 / 0.3D1 - 0.5D1 / 0.18D2 * t83 - 0.100D3 / 
     #0.9D1 * t74 + t140 * t13
      rrqg2qgh64J1 = ((0.32D2 / 0.9D1 * t3 - 0.32D2 / 0.9D1 * t4 * S23) 
     #* t8 * s * z - t4 * t13 * t14 + (-0.187D3 / 0.9D1 * S13 + (0.7D1 /
     # 0.9D1 * t18 + 0.3D1 * t20 - 0.4D1 * t22) * t13) * t1 * S12 - 0.8D
     #1 / 0.3D1 * t3 + (-0.4D1 / 0.9D1 * t20 - 0.220D3 / 0.9D1 * t22 + 0
     #.8D1 / 0.3D1 * t18 + (-0.3D1 * t33 - 0.6D1 * t36 + 0.28D2 / 0.9D1 
     #* t38 - t41 - 0.14D2 / 0.9D1 * t42 + 0.8D1 * t44) * t13) * t1 + (-
     #t1 * S24 * t13 * t51 * S34 + (-0.215D3 / 0.18D2 * S24 - 0.47D2 / 0
     #.36D2 * S13 + (-0.4D1 * t57 + 0.7D1 / 0.9D1 * t59 + 0.3D1 * t61) *
     # t13) * t1 * t51 + (-0.146D3 / 0.9D1 * t57 - 0.32D2 / 0.9D1 * t22 
     #+ 0.47D2 / 0.18D2 * t18 - 0.265D3 / 0.18D2 * t59 - 0.37D2 / 0.9D1 
     #* t20 - 0.82D2 / 0.9D1 * t61 + (0.28D2 / 0.9D1 * t74 - 0.6D1 * t76
     # - 0.14D2 / 0.9D1 * t78 + 0.8D1 * t80 - t82 - 0.3D1 * t83) * t13) 
     #* t1 * S34 + t142 * t1) * t8 + (-0.4D1 / 0.9D1 * S23 * t123 * S13 
     #- 0.4D1 / 0.9D1 * t115 * S14 * S13) * t13 * t1 / t14) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J2
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
      t1 = S13 + S14 + S34
      t2 = t1 * S34
      t4 = t1 * S13
      t8 = 0.1D1 / S12
      t13 = 0.1D1 / (S12 + S13 + S23)
      t14 = S12 ** 2
      t21 = S23 * S13
      t23 = S13 ** 2
      t25 = S13 * S14
      t40 = S24 * S14
      t43 = S23 * S24
      t48 = S24 ** 2
      t50 = S14 ** 2
      t51 = t50 * S13
      t53 = S23 * t23
      t55 = t23 * S13
      t57 = t21 * S14
      t59 = S23 ** 2
      t60 = t59 * S13
      t61 = t23 * S14
      t68 = S34 ** 2
      t89 = t43 * S14
      t91 = t48 * S24
      t93 = S24 * t59
      t94 = S24 * t50
      t96 = t48 * S23
      t98 = t48 * S14
      t133 = t23 ** 2
      t134 = t59 * S23
      t147 = t48 ** 2
      t152 = 0.8D1 / 0.9D1 * t96 * S14 + 0.28D2 / 0.9D1 * t93 * S14 + 0.
     #7D1 / 0.9D1 * t91 * S23 - 0.44D2 / 0.9D1 * t94 * S23 - 0.44D2 / 0.
     #9D1 * t51 * S23 + 0.28D2 / 0.9D1 * t25 * t59 + 0.8D1 / 0.9D1 * t61
     # * S23 + t133 - 0.7D1 / 0.9D1 * S13 * t134 - 0.7D1 / 0.9D1 * S24 *
     # t134 + 0.7D1 / 0.9D1 * t55 * S23 - t23 * t59 + 0.4D1 * t23 * t50 
     #- t48 * t59 - 0.4D1 * t55 * S14 + t147 + 0.4D1 * t48 * t50 - 0.4D1
     # * t91 * S14
      t154 = 0.13D2 / 0.9D1 * t91 - 0.16D2 / 0.9D1 * S23 * t50 - 0.37D2 
     #/ 0.18D2 * t96 - 0.2D1 * t98 + 0.7D1 / 0.36D2 * t60 - 0.4D1 / 0.3D
     #1 * t51 + 0.76D2 / 0.9D1 * t61 - 0.17D2 / 0.9D1 * t53 - 0.9D1 / 0.
     #2D1 * t93 - 0.20D2 / 0.9D1 * t94 - 0.28D2 / 0.9D1 * t89 - 0.92D2 /
     # 0.9D1 * t57 + 0.7D1 / 0.9D1 * t55 + t152 * t13
      rrqg2qgh64J2 = ((-0.32D2 / 0.9D1 * t2 * S13 + 0.32D2 / 0.9D1 * t4 
     #* S23) * t8 * s * z - t4 * t13 * t14 + (0.16D2 / 0.9D1 * t2 + (-0.
     #16D2 / 0.9D1 * S23 - 0.13D2 / 0.3D1 * S13 - 0.34D2 / 0.9D1 * S24 +
     # (0.7D1 / 0.9D1 * t21 + 0.3D1 * t23 - 0.4D1 * t25) * t13) * t1) * 
     #S12 + (0.136D3 / 0.9D1 * S13 + 0.32D2 / 0.9D1 * S14 - 0.64D2 / 0.9
     #D1 * S24) * t1 * S34 + (-0.20D2 / 0.3D1 * t25 - 0.6D1 * t40 + 0.14
     #8D3 / 0.9D1 * t23 - 0.64D2 / 0.9D1 * t43 - 0.32D2 / 0.9D1 * S14 * 
     #S23 - 0.136D3 / 0.9D1 * t21 - 0.64D2 / 0.9D1 * t48 + (-0.4D1 * t51
     # - 0.14D2 / 0.9D1 * t53 - 0.3D1 * t55 - 0.8D1 / 0.9D1 * t57 + t60 
     #+ 0.8D1 * t61) * t13) * t1 + (-t1 * S24 * t13 * t68 * S34 + (0.7D1
     # / 0.36D2 * S13 + 0.28D2 / 0.9D1 * S24 + (-0.4D1 * t40 + 0.7D1 / 0
     #.9D1 * t43 + 0.3D1 * t48) * t13) * t1 * t68 + (0.23D2 / 0.3D1 * t4
     #8 + 0.17D2 / 0.9D1 * t23 - 0.7D1 / 0.18D2 * t21 + 0.16D2 / 0.9D1 *
     # t50 - 0.10D2 / 0.9D1 * t40 - 0.43D2 / 0.18D2 * t43 + 0.92D2 / 0.9
     #D1 * t25 + (-0.8D1 / 0.9D1 * t89 - 0.3D1 * t91 + t93 - 0.4D1 * t94
     # - 0.14D2 / 0.9D1 * t96 + 0.8D1 * t98) * t13) * t1 * S34 + t154 * 
     #t1) * t8 - 0.8D1 / 0.9D1 * t59 * t50 * t1 * S13 * t13 / t14) / pi 
     #* wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J3
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
      t1 = S13 + S14 + S34
      t2 = S23 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t20 = S23 ** 2
      t21 = S23 * S13
      t29 = t1 * S13
      t30 = S12 ** 2
      t36 = 0.16D2 / 0.9D1 * S23
      t39 = S13 ** 2
      t41 = S13 * S14
      t49 = S34 ** 2
      t58 = S23 * S24
      t61 = S24 * S14
      t66 = 0.16D2 / 0.3D1 * S14 * S23
      t67 = S24 ** 2
      t70 = S14 ** 2
      t71 = t70 * S13
      t73 = S23 * t39
      t75 = t39 * S13
      t77 = t21 * S14
      t79 = t20 * S13
      t80 = S14 * t39
      t111 = S14 * t58
      t113 = t67 * S24
      t115 = S24 * t20
      t116 = t70 * S24
      t118 = t67 * S23
      t120 = t67 * S14
      t127 = S23 * t20
      t161 = t39 ** 2
      t168 = t67 ** 2
      t176 = -t39 * t20 + 0.4D1 * t39 * t70 + 0.8D1 / 0.9D1 * t118 * S14
     # - 0.44D2 / 0.9D1 * t115 * S14 + 0.7D1 / 0.9D1 * t113 * S23 - 0.8D
     #1 / 0.9D1 * t116 * S23 - 0.8D1 / 0.9D1 * t71 * S23 - 0.44D2 / 0.9D
     #1 * t41 * t20 + 0.8D1 / 0.9D1 * t80 * S23 + t161 + 0.29D2 / 0.9D1 
     #* S13 * t127 + 0.29D2 / 0.9D1 * S24 * t127 + 0.7D1 / 0.9D1 * t75 *
     # S23 + t168 + 0.4D1 * t67 * t70 - t67 * t20 - 0.4D1 * t75 * S14 - 
     #0.4D1 * t113 * S14
      t178 = 0.28D2 / 0.9D1 * t127 + 0.8D1 / 0.3D1 * S14 * t20 + 0.89D2 
     #/ 0.9D1 * t113 - 0.16D2 / 0.3D1 * t70 * S23 + 0.14527D5 / 0.18D2 *
     # t118 - 0.18598D5 / 0.9D1 * t120 + 0.259D3 / 0.36D2 * t79 - 0.28D2
     # / 0.9D1 * t71 + 0.44D2 / 0.9D1 * t80 + 0.5D1 / 0.3D1 * t73 + 0.14
     #315D5 / 0.18D2 * t115 - 0.28D2 / 0.9D1 * t116 - 0.6200D4 / 0.3D1 *
     # t111 - 0.10D2 * t77 - t75 + t176 * t6
      rrqg2qgh64J3 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.3D1 * t20 - 0.32D2 / 0.3D1 * t21) * t1 * t3) * s * z - 
     #t29 * t6 * t30 + (0.16D2 / 0.9D1 * t1 * S34 + (-0.55D2 / 0.9D1 * S
     #13 + t36 - 0.34D2 / 0.9D1 * S24 + (0.7D1 / 0.9D1 * t21 + 0.3D1 * t
     #39 - 0.4D1 * t41) * t6) * t1) * S12 + 0.8D1 / 0.9D1 * t1 * t49 + (
     #0.136D3 / 0.9D1 * S13 - t36 + 0.16D2 / 0.3D1 * S14 - 0.6380D4 / 0.
     #9D1 * S24) * t1 * S34 + (-0.6376D4 / 0.9D1 * t58 + 0.116D3 / 0.9D1
     # * t39 - 0.74D2 / 0.9D1 * t61 + 0.8D1 * t20 - 0.92D2 / 0.9D1 * t41
     # - t66 - 0.6388D4 / 0.9D1 * t67 - 0.8D1 * t21 + (-0.4D1 * t71 - 0.
     #14D2 / 0.9D1 * t73 - 0.3D1 * t75 - 0.8D1 / 0.9D1 * t77 + t79 + 0.8
     #D1 * t80) * t6) * t1 + ((0.4D1 / 0.9D1 - S24 * t6) * t1 * t49 * S3
     #4 + (S13 / 0.12D2 - 0.7024D4 / 0.9D1 * S24 - 0.4D1 / 0.3D1 * S23 +
     # 0.8D1 / 0.3D1 * S14 + (-0.4D1 * t61 + 0.7D1 / 0.9D1 * t58 + 0.3D1
     # * t67) * t6) * t1 * t49 + (0.16D2 / 0.3D1 * t70 + 0.17D2 / 0.9D1 
     #* t39 - t21 / 0.6D1 + 0.25D2 / 0.2D1 * t58 - 0.6911D4 / 0.9D1 * t6
     #7 - t66 - 0.18562D5 / 0.9D1 * t61 + 0.4D1 / 0.3D1 * t20 + 0.10D2 *
     # t41 + (-0.8D1 / 0.9D1 * t111 - 0.3D1 * t113 + t115 - 0.4D1 * t116
     # - 0.14D2 / 0.9D1 * t118 + 0.8D1 * t120) * t6) * t1 * S34 + t178 *
     # t1) * t3 + (-0.4D1 / 0.9D1 * t29 * S14 * t49 - 0.4D1 / 0.9D1 * t2
     #9 * t70 * S34 + (0.4D1 / 0.9D1 * t127 * S14 * S13 - 0.16D2 / 0.9D1
     # * t20 * t70 * S13 + 0.4D1 / 0.9D1 * S23 * t70 * S14 * S13) * t6 *
     # t1) / t30) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J4
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
      t1 = S13 + S14 + S34
      t2 = S23 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t20 = S23 ** 2
      t21 = S23 * S13
      t29 = t1 * S13
      t30 = S12 ** 2
      t36 = 0.16D2 / 0.9D1 * S23
      t39 = S13 ** 2
      t41 = S13 * S14
      t49 = S34 ** 2
      t58 = S23 * S24
      t61 = S24 * S14
      t66 = 0.16D2 / 0.3D1 * S14 * S23
      t67 = S24 ** 2
      t70 = S14 ** 2
      t71 = t70 * S13
      t73 = S23 * t39
      t75 = t39 * S13
      t77 = t21 * S14
      t79 = t20 * S13
      t80 = S14 * t39
      t111 = S14 * t58
      t113 = t67 * S24
      t115 = S24 * t20
      t116 = t70 * S24
      t118 = t67 * S23
      t120 = t67 * S14
      t127 = S23 * t20
      t161 = t39 ** 2
      t168 = t67 ** 2
      t176 = -t39 * t20 + 0.4D1 * t39 * t70 + 0.8D1 / 0.9D1 * t118 * S14
     # - 0.44D2 / 0.9D1 * t115 * S14 + 0.7D1 / 0.9D1 * t113 * S23 - 0.8D
     #1 / 0.9D1 * t116 * S23 - 0.8D1 / 0.9D1 * t71 * S23 - 0.44D2 / 0.9D
     #1 * t41 * t20 + 0.8D1 / 0.9D1 * t80 * S23 + t161 + 0.29D2 / 0.9D1 
     #* S13 * t127 + 0.29D2 / 0.9D1 * S24 * t127 + 0.7D1 / 0.9D1 * t75 *
     # S23 + t168 + 0.4D1 * t67 * t70 - t67 * t20 - 0.4D1 * t75 * S14 - 
     #0.4D1 * t113 * S14
      t178 = 0.28D2 / 0.9D1 * t127 + 0.8D1 / 0.3D1 * S14 * t20 + 0.89D2 
     #/ 0.9D1 * t113 - 0.16D2 / 0.3D1 * t70 * S23 + 0.14527D5 / 0.18D2 *
     # t118 - 0.18598D5 / 0.9D1 * t120 + 0.259D3 / 0.36D2 * t79 - 0.28D2
     # / 0.9D1 * t71 + 0.44D2 / 0.9D1 * t80 + 0.5D1 / 0.3D1 * t73 + 0.14
     #315D5 / 0.18D2 * t115 - 0.28D2 / 0.9D1 * t116 - 0.6200D4 / 0.3D1 *
     # t111 - 0.10D2 * t77 - t75 + t176 * t6
      rrqg2qgh64J4 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.3D1 * t20 - 0.32D2 / 0.3D1 * t21) * t1 * t3) * s * z - 
     #t29 * t6 * t30 + (0.16D2 / 0.9D1 * t1 * S34 + (-0.55D2 / 0.9D1 * S
     #13 + t36 - 0.34D2 / 0.9D1 * S24 + (0.7D1 / 0.9D1 * t21 + 0.3D1 * t
     #39 - 0.4D1 * t41) * t6) * t1) * S12 + 0.8D1 / 0.9D1 * t1 * t49 + (
     #0.136D3 / 0.9D1 * S13 - t36 + 0.16D2 / 0.3D1 * S14 - 0.6380D4 / 0.
     #9D1 * S24) * t1 * S34 + (-0.6376D4 / 0.9D1 * t58 + 0.116D3 / 0.9D1
     # * t39 - 0.74D2 / 0.9D1 * t61 + 0.8D1 * t20 - 0.92D2 / 0.9D1 * t41
     # - t66 - 0.6388D4 / 0.9D1 * t67 - 0.8D1 * t21 + (-0.4D1 * t71 - 0.
     #14D2 / 0.9D1 * t73 - 0.3D1 * t75 - 0.8D1 / 0.9D1 * t77 + t79 + 0.8
     #D1 * t80) * t6) * t1 + ((0.4D1 / 0.9D1 - S24 * t6) * t1 * t49 * S3
     #4 + (S13 / 0.12D2 - 0.7024D4 / 0.9D1 * S24 - 0.4D1 / 0.3D1 * S23 +
     # 0.8D1 / 0.3D1 * S14 + (-0.4D1 * t61 + 0.7D1 / 0.9D1 * t58 + 0.3D1
     # * t67) * t6) * t1 * t49 + (0.16D2 / 0.3D1 * t70 + 0.17D2 / 0.9D1 
     #* t39 - t21 / 0.6D1 + 0.25D2 / 0.2D1 * t58 - 0.6911D4 / 0.9D1 * t6
     #7 - t66 - 0.18562D5 / 0.9D1 * t61 + 0.4D1 / 0.3D1 * t20 + 0.10D2 *
     # t41 + (-0.8D1 / 0.9D1 * t111 - 0.3D1 * t113 + t115 - 0.4D1 * t116
     # - 0.14D2 / 0.9D1 * t118 + 0.8D1 * t120) * t6) * t1 * S34 + t178 *
     # t1) * t3 + (-0.4D1 / 0.9D1 * t29 * S14 * t49 - 0.4D1 / 0.9D1 * t2
     #9 * t70 * S34 + (0.4D1 / 0.9D1 * t127 * S14 * S13 - 0.16D2 / 0.9D1
     # * t20 * t70 * S13 + 0.4D1 / 0.9D1 * S23 * t70 * S14 * S13) * t6 *
     # t1) / t30) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J5
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
      t1 = S13 + S14 + S34
      t2 = S23 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t20 = S23 ** 2
      t21 = S23 * S13
      t29 = t1 * S13
      t30 = S12 ** 2
      t36 = 0.16D2 / 0.9D1 * S23
      t39 = S13 ** 2
      t41 = S13 * S14
      t49 = S34 ** 2
      t58 = S23 * S24
      t61 = S24 * S14
      t66 = 0.16D2 / 0.3D1 * S14 * S23
      t67 = S24 ** 2
      t70 = S14 ** 2
      t71 = t70 * S13
      t73 = S23 * t39
      t75 = t39 * S13
      t77 = t21 * S14
      t79 = t20 * S13
      t80 = S14 * t39
      t111 = S14 * t58
      t113 = t67 * S24
      t115 = S24 * t20
      t116 = t70 * S24
      t118 = t67 * S23
      t120 = t67 * S14
      t127 = S23 * t20
      t161 = t39 ** 2
      t168 = t67 ** 2
      t176 = -t39 * t20 + 0.4D1 * t39 * t70 + 0.8D1 / 0.9D1 * t118 * S14
     # - 0.44D2 / 0.9D1 * t115 * S14 + 0.7D1 / 0.9D1 * t113 * S23 - 0.8D
     #1 / 0.9D1 * t116 * S23 - 0.8D1 / 0.9D1 * t71 * S23 - 0.44D2 / 0.9D
     #1 * t41 * t20 + 0.8D1 / 0.9D1 * t80 * S23 + t161 + 0.29D2 / 0.9D1 
     #* S13 * t127 + 0.29D2 / 0.9D1 * S24 * t127 + 0.7D1 / 0.9D1 * t75 *
     # S23 + t168 + 0.4D1 * t67 * t70 - t67 * t20 - 0.4D1 * t75 * S14 - 
     #0.4D1 * t113 * S14
      t178 = 0.28D2 / 0.9D1 * t127 + 0.8D1 / 0.3D1 * S14 * t20 + 0.89D2 
     #/ 0.9D1 * t113 - 0.16D2 / 0.3D1 * t70 * S23 + 0.14527D5 / 0.18D2 *
     # t118 - 0.18598D5 / 0.9D1 * t120 + 0.259D3 / 0.36D2 * t79 - 0.28D2
     # / 0.9D1 * t71 + 0.44D2 / 0.9D1 * t80 + 0.5D1 / 0.3D1 * t73 + 0.14
     #315D5 / 0.18D2 * t115 - 0.28D2 / 0.9D1 * t116 - 0.6200D4 / 0.3D1 *
     # t111 - 0.10D2 * t77 - t75 + t176 * t6
      rrqg2qgh64J5 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.3D1 * t20 - 0.32D2 / 0.3D1 * t21) * t1 * t3) * s * z - 
     #t29 * t6 * t30 + (0.16D2 / 0.9D1 * t1 * S34 + (-0.55D2 / 0.9D1 * S
     #13 + t36 - 0.34D2 / 0.9D1 * S24 + (0.7D1 / 0.9D1 * t21 + 0.3D1 * t
     #39 - 0.4D1 * t41) * t6) * t1) * S12 + 0.8D1 / 0.9D1 * t1 * t49 + (
     #0.136D3 / 0.9D1 * S13 - t36 + 0.16D2 / 0.3D1 * S14 - 0.6380D4 / 0.
     #9D1 * S24) * t1 * S34 + (-0.6376D4 / 0.9D1 * t58 + 0.116D3 / 0.9D1
     # * t39 - 0.74D2 / 0.9D1 * t61 + 0.8D1 * t20 - 0.92D2 / 0.9D1 * t41
     # - t66 - 0.6388D4 / 0.9D1 * t67 - 0.8D1 * t21 + (-0.4D1 * t71 - 0.
     #14D2 / 0.9D1 * t73 - 0.3D1 * t75 - 0.8D1 / 0.9D1 * t77 + t79 + 0.8
     #D1 * t80) * t6) * t1 + ((0.4D1 / 0.9D1 - S24 * t6) * t1 * t49 * S3
     #4 + (S13 / 0.12D2 - 0.7024D4 / 0.9D1 * S24 - 0.4D1 / 0.3D1 * S23 +
     # 0.8D1 / 0.3D1 * S14 + (-0.4D1 * t61 + 0.7D1 / 0.9D1 * t58 + 0.3D1
     # * t67) * t6) * t1 * t49 + (0.16D2 / 0.3D1 * t70 + 0.17D2 / 0.9D1 
     #* t39 - t21 / 0.6D1 + 0.25D2 / 0.2D1 * t58 - 0.6911D4 / 0.9D1 * t6
     #7 - t66 - 0.18562D5 / 0.9D1 * t61 + 0.4D1 / 0.3D1 * t20 + 0.10D2 *
     # t41 + (-0.8D1 / 0.9D1 * t111 - 0.3D1 * t113 + t115 - 0.4D1 * t116
     # - 0.14D2 / 0.9D1 * t118 + 0.8D1 * t120) * t6) * t1 * S34 + t178 *
     # t1) * t3 + (-0.4D1 / 0.9D1 * t29 * S14 * t49 - 0.4D1 / 0.9D1 * t2
     #9 * t70 * S34 + (0.4D1 / 0.9D1 * t127 * S14 * S13 - 0.16D2 / 0.9D1
     # * t20 * t70 * S13 + 0.4D1 / 0.9D1 * S23 * t70 * S14 * S13) * t6 *
     # t1) / t30) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J6
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
      t1 = S13 + S14 + S34
      t2 = S23 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t20 = t1 * S34
      t23 = S23 * S13
      t25 = S23 ** 2
      t36 = 0.16D2 / 0.9D1 * S23
      t42 = S34 ** 2
      t51 = S24 * S14
      t54 = S23 * S24
      t57 = S13 ** 2
      t59 = S13 * S14
      t62 = 0.16D2 / 0.3D1 * S14 * S23
      t63 = S24 ** 2
      t65 = S14 ** 2
      t66 = t65 * S13
      t68 = t25 * S13
      t70 = t23 * S14
      t94 = S24 * t65
      t96 = S24 * t25
      t98 = S14 * t54
      t110 = S23 * t25
      t118 = t63 * S23
      t124 = S14 * t57
      t131 = t65 * S14
      t158 = -0.2D1 * t63 * t25 + 0.4D1 * S24 * t131 + 0.4D1 * t118 * S1
     #4 - 0.8D1 / 0.9D1 * t96 * S14 + 0.4D1 * t124 * S23 - 0.50D2 / 0.9D
     #1 * t94 * S23 - 0.50D2 / 0.9D1 * t66 * S23 - 0.8D1 / 0.9D1 * t25 *
     # t59 - 0.2D1 * t57 * t65 + 0.22D2 / 0.9D1 * S24 * t110 - 0.2D1 * t
     #57 * t25 + 0.4D1 * S13 * t131 + 0.22D2 / 0.9D1 * S13 * t110 - 0.2D
     #1 * t63 * t65
      t160 = -0.16D2 / 0.3D1 * S23 * t65 + 0.8D1 / 0.3D1 * t25 * S14 - 0
     #.14D2 / 0.9D1 * t94 + 0.28D2 / 0.9D1 * t110 - 0.122D3 / 0.9D1 * t7
     #0 - 0.4D1 / 0.3D1 * t57 * S13 + 0.61D2 / 0.6D1 * t63 * S24 - 0.185
     #00D5 / 0.9D1 * t98 + 0.7309D4 / 0.9D1 * t118 - 0.6172D4 / 0.3D1 * 
     #S14 * t63 + 0.17D2 / 0.2D1 * t68 + 0.50D2 / 0.9D1 * t66 - 0.16D2 /
     # 0.9D1 * t124 - 0.22D2 / 0.9D1 * S23 * t57 + 0.14411D5 / 0.18D2 * 
     #t96 + t158 * t6
      t164 = t1 * S13
      t184 = S12 ** 2
      rrqg2qgh64J6 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (-
     #0.32D2 / 0.9D1 * t20 * S13 + (-0.64D2 / 0.9D1 * t23 - 0.32D2 / 0.3
     #D1 * t25) * t1) * t3) * s * z + (0.16D2 / 0.9D1 * t20 + (0.44D2 / 
     #0.3D1 * S13 + t36 - 0.34D2 / 0.9D1 * S24) * t1) * S12 + 0.8D1 / 0.
     #9D1 * t1 * t42 + (-0.6380D4 / 0.9D1 * S24 - t36 + 0.160D3 / 0.9D1 
     #* S13 + 0.16D2 / 0.3D1 * S14) * t1 * S34 + (-0.74D2 / 0.9D1 * t51 
     #- 0.32D2 / 0.3D1 * t23 - 0.6376D4 / 0.9D1 * t54 + 0.8D1 * t25 + 0.
     #40D2 / 0.3D1 * t57 + 0.128D3 / 0.9D1 * t59 - t62 - 0.6388D4 / 0.9D
     #1 * t63 + (0.2D1 * t66 + 0.2D1 * t68 - 0.4D1 * t70) * t6) * t1 + (
     #0.4D1 / 0.9D1 * t1 * t42 * S34 + (0.8D1 / 0.3D1 * S14 - 0.1537D4 /
     # 0.2D1 * S24 - 0.4D1 / 0.3D1 * S23 + 0.25D2 / 0.18D2 * S13) * t1 *
     # t42 + (0.6D1 * t57 + 0.122D3 / 0.9D1 * t59 + 0.4D1 / 0.3D1 * t25 
     #- t62 + 0.16D2 / 0.3D1 * t65 - 0.6829D4 / 0.9D1 * t63 - 0.25D2 / 0
     #.9D1 * t23 + 0.245D3 / 0.9D1 * t54 - 0.18416D5 / 0.9D1 * t51 + (0.
     #2D1 * t94 + 0.2D1 * t96 - 0.4D1 * t98) * t6) * t1 * S34 + t160 * t
     #1) * t3 + (-0.4D1 / 0.9D1 * t164 * S14 * t42 - 0.4D1 / 0.9D1 * t16
     #4 * t65 * S34 + (-0.16D2 / 0.9D1 * t25 * t65 * S13 + 0.8D1 / 0.9D1
     # * S23 * t131 * S13 + 0.8D1 / 0.9D1 * t110 * S14 * S13) * t6 * t1)
     # / t184) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh64J7
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
      t1 = S13 + S14 + S34
      t2 = S23 * t1
      t3 = 0.1D1 / S12
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t23 = S23 ** 2
      t25 = S23 * S13
      t39 = S34 ** 2
      t49 = S24 * S14
      t52 = S13 * S14
      t54 = S23 * S24
      t56 = S24 ** 2
      t58 = S13 ** 2
      t60 = S14 * S23
      t74 = S14 ** 2
      t88 = t74 * S13
      t96 = S24 * t74
      t100 = S23 * t23
      t102 = S24 * t23
      t130 = -0.18580D5 / 0.9D1 * t56 * S14 - 0.16D2 / 0.9D1 * t88 + 0.7
     #282D4 / 0.9D1 * t56 * S23 + 0.32D2 / 0.9D1 * S23 * t58 + 0.7D1 * t
     #23 * S13 - 0.8D1 / 0.9D1 * t96 - 0.32D2 / 0.9D1 * S14 * t58 + 0.28
     #D2 / 0.9D1 * t100 + 0.7198D4 / 0.9D1 * t102 - 0.32D2 / 0.9D1 * S23
     # * t74 - 0.18572D5 / 0.9D1 * S14 * t54 + 0.2D1 / 0.9D1 * t25 * S14
     # + 0.8D1 / 0.3D1 * t23 * S14 - 0.16D2 / 0.9D1 * t58 * S13 + 0.76D2
     # / 0.9D1 * t56 * S24 + (-0.8D1 * t102 * S14 + 0.4D1 * t96 * S23 - 
     #0.8D1 * t52 * t23 + 0.4D1 * t88 * S23 + 0.4D1 * S24 * t100 + 0.4D1
     # * S13 * t100) * t6
      t134 = t1 * S13
      t155 = S12 ** 2
      rrqg2qgh64J7 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t2 + (0
     #.32D2 / 0.9D1 * S34 * t1 * S13 + (-0.32D2 / 0.3D1 * t23 - 0.128D3 
     #/ 0.9D1 * t25) * t1) * t3) * s * z + (-0.16D2 / 0.9D1 * S13 + 0.32
     #D2 / 0.9D1 * S23) * t1 * S12 + 0.8D1 / 0.9D1 * t1 * t39 + (0.16D2 
     #/ 0.9D1 * S14 - 0.6316D4 / 0.9D1 * S24 - 0.16D2 / 0.9D1 * S23) * t
     #1 * S34 + (0.8D1 * t23 - 0.20D2 / 0.9D1 * t49 + 0.64D2 / 0.9D1 * t
     #25 - 0.32D2 / 0.9D1 * t52 - 0.2104D4 / 0.3D1 * t54 - 0.2108D4 / 0.
     #3D1 * t56 - 0.32D2 / 0.9D1 * t58 - 0.16D2 / 0.9D1 * t60) * t1 + (0
     #.4D1 / 0.9D1 * t1 * t39 * S34 + (-0.4D1 / 0.3D1 * S23 + 0.8D1 / 0.
     #3D1 * S14 - 0.7052D4 / 0.9D1 * S24 - S13 / 0.9D1) * t1 * t39 + (0.
     #32D2 / 0.9D1 * t74 - 0.6980D4 / 0.9D1 * t56 - 0.2D1 / 0.9D1 * t52 
     #- 0.16D2 / 0.3D1 * t60 + 0.2D1 / 0.9D1 * t25 + 0.134D3 / 0.9D1 * t
     #54 + 0.4D1 / 0.3D1 * t23 - 0.6184D4 / 0.3D1 * t49) * t1 * S34 + t1
     #30 * t1) * t3 + (-0.4D1 / 0.9D1 * t134 * S14 * t39 - 0.4D1 / 0.9D1
     # * t134 * t74 * S34 + (-0.8D1 / 0.9D1 * t23 * t74 * S13 + 0.4D1 / 
     #0.9D1 * S23 * t74 * S14 * S13 + 0.4D1 / 0.9D1 * t100 * S14 * S13) 
     #* t6 * t1) / t155) / pi * wd / z

      end function
  
 