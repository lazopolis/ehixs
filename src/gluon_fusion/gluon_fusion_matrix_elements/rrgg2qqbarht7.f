  
      subroutine rrgg2qqbarht7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh71J1  
      doubleprecision rrgg2qqbarh71J2  
      doubleprecision rrgg2qqbarh71J3  
      doubleprecision rrgg2qqbarh71J4  
      doubleprecision rrgg2qqbarh71J5  
      doubleprecision rrgg2qqbarh71J6  
      doubleprecision rrgg2qqbarht7s1e1  
      doubleprecision rrgg2qqbarht7s1e0  
      doubleprecision rrgg2qqbarht7s1em1  
      doubleprecision rrgg2qqbarht7s1em2  
      doubleprecision rrgg2qqbarht7s1em3  
      doubleprecision rrgg2qqbarht7s1em4  
      doubleprecision rrgg2qqbarht7s2e1  
      doubleprecision rrgg2qqbarht7s2e0  
      doubleprecision rrgg2qqbarht7s2em1  
      doubleprecision rrgg2qqbarht7s2em2  
      doubleprecision rrgg2qqbarht7s2em3  
      doubleprecision rrgg2qqbarht7s2em4  
      doubleprecision rrgg2qqbarht7s3e1  
      doubleprecision rrgg2qqbarht7s3e0  
      doubleprecision rrgg2qqbarht7s3em1  
      doubleprecision rrgg2qqbarht7s3em2  
      doubleprecision rrgg2qqbarht7s3em3  
      doubleprecision rrgg2qqbarht7s3em4  
      doubleprecision rrgg2qqbarht7s4e1  
      doubleprecision rrgg2qqbarht7s4e0  
      doubleprecision rrgg2qqbarht7s4em1  
      doubleprecision rrgg2qqbarht7s4em2  
      doubleprecision rrgg2qqbarht7s4em3  
      doubleprecision rrgg2qqbarht7s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarht7s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarht7s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarht7s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarht7s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarht7s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarht7s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarht7s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarht7s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarht7s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarht7s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarht7s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarht7s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht7s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * 0.3141592653589793D1
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t10 = log(0.4D1 * t8)
      t11 = t10 ** 2
      t14 = 0.3141592653589793D1 ** 2
      t16 = 0.60D2 * lh * t14
      t17 = lh ** 2
      t19 = 0.120D3 * t17 * lh
      t20 = t11 * t10
      t22 = 0.180D3 * t17
      t23 = 0.30D2 * t14
      t24 = -t22 + t23
      t27 = s ** 2
      t28 = 0.1D1 / t27
      t30 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t31 = 0.3141592653589793D1 * t30
      t39 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t40 = 0.3141592653589793D1 * t39
      t47 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t51 = t28 * 0.3141592653589793D1
      t52 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t59 = -t16 + 0.2884936567583026D3 + t19
      t62 = t14 ** 2
      t63 = t17 ** 2
      t67 = t11 ** 2
      t71 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t72 = 0.3141592653589793D1 * t71
      t75 = x1 ** 2
      t76 = x3 * t75
      t79 = log(0.4D1 * t76 * t8)
      t81 = t79 ** 2
      t84 = -0.1D1 + x3
      t85 = 0.1D1 / t84
      t86 = t8 * t85
      t89 = log(-0.4D1 * t76 * t86)
      t90 = t89 * z
      t92 = t89 ** 2
      t96 = z * t39
      t98 = cos(t5)
      t100 = Sqrt(-x3 * t84)
      t105 = 0.1D1 / (-z - x3 + 0.2D1 * t98 * t100 * z)
      t110 = lh * t28
      t112 = z * t30
      t120 = t24 * t28
      t124 = 0.3141592653589793D1 * (-z * t71 * t105 - t71)
      t125 = t120 * t124
      t127 = 0.1D1 / x3
      t129 = 0.1D1 / x1
      t132 = t75 * t7
      t133 = t132 * t4
      t135 = log(0.4D1 * t133)
      t140 = t135 ** 2
      t150 = t59 * t28
      t151 = t150 * t72
      t162 = x2 ** 2
      t163 = t162 * x3
      t164 = t163 * t133
      t166 = log(0.4D1 * t164)
      t168 = t163 * t75
      t171 = log(-0.4D1 * t168 * t86)
      t183 = 0.1D1 / x2
      t184 = t183 * t129
      t187 = t162 * t75
      t190 = log(0.4D1 * t187 * t8)
      t192 = t190 ** 2
      t203 = t120 * t72
      t213 = x3 * t7
      t216 = log(0.4D1 * t213 * t4)
      t220 = log(-0.4D1 * t213 * t4 * t85)
      t225 = t220 ** 2
      t229 = t216 ** 2
      t260 = log(0.4D1 * t163 * t8)
      t261 = t260 ** 2
      t267 = log(-0.4D1 * t163 * t86)
      t268 = t267 * z
      t270 = t267 ** 2
      t291 = t162 * t7
      t294 = log(0.4D1 * t291 * t4)
      t299 = t294 ** 2
      t319 = (0.90D2 * t11 * lh - t16 + 0.2884936567583026D3 + t19 + 0.1
     #5D2 * t20 - t10 * t24) * t28 * t31 / 0.2880D4 + (-0.180D3 * t10 * 
     #lh - 0.45D2 * t11 - t22 + t23) * t28 * t40 / 0.2880D4 + (0.180D3 *
     # lh + 0.90D2 * t10) * t28 * 0.3141592653589793D1 * t47 / 0.2880D4 
     #- t51 * t52 / 0.32D2 + (-0.30D2 * t20 * lh + t11 * t24 / 0.2D1 - t
     #10 * t59 - 0.5769873135166051D3 * lh - t62 - 0.60D2 * t63 + 0.60D2
     # * t17 * t14 - 0.15D2 / 0.4D1 * t67) * t28 * t72 / 0.2880D4 - (-0.
     #90D2 * t51 * (t79 * t30 - t81 * t71 / 0.2D1 - t39 - (-t90 * t30 + 
     #t92 * z * t71 / 0.2D1 + t96) * t105) + 0.180D3 * t110 * 0.31415926
     #53589793D1 * (-t30 + t79 * t71 - (t112 - t90 * t71) * t105) + t125
     #) * t127 * t129 / 0.1440D4 + (t120 * 0.3141592653589793D1 * (t30 -
     # t135 * t71) - 0.90D2 * t51 * (t140 * t30 / 0.2D1 + t47 - t140 * t
     #135 * t71 / 0.6D1 - t135 * t39) + t151 + 0.180D3 * t110 * 0.314159
     #2653589793D1 * (-t135 * t30 + t140 * t71 / 0.2D1 + t39)) * t129 / 
     #0.1440D4 - (-0.90D2 * t51 * (t166 * t71 - (t112 - t171 * z * t71) 
     #* t105 - t30) + 0.180D3 * t110 * t124) * t127 * t184 / 0.720D3 + (
     #-0.90D2 * t51 * (-t190 * t30 + t39 + t192 * t71 / 0.2D1) + 0.180D3
     # * t110 * 0.3141592653589793D1 * (-t190 * t71 + t30) + t203) * t18
     #3 * t129 / 0.720D3 + ((0.180D3 * t110 * t31 + t203 - 0.90D2 * t51 
     #* t39) * (-t216 - t220 * z * t105) - 0.90D2 * t51 * t71 * (-t225 *
     # t220 * z * t105 / 0.6D1 - t229 * t216 / 0.6D1) + (t120 * t31 - 0.
     #90D2 * t51 * t47 + t151 + 0.180D3 * t110 * t40) * (0.1D1 + z * t10
     #5) + (-0.90D2 * t51 * t30 + 0.180D3 * t110 * t72) * (t229 / 0.2D1 
     #+ t225 * z * t105 / 0.2D1)) * t127 / 0.2880D4 - (-0.90D2 * t51 * (
     #-t261 * t71 / 0.2D1 + t260 * t30 - (-t268 * t30 + t270 * z * t71 /
     # 0.2D1 + t96) * t105 - t39) + 0.180D3 * t110 * 0.3141592653589793D
     #1 * (-t30 - (t112 - t268 * t71) * t105 + t260 * t71) + t125) * t12
     #7 * t183 / 0.1440D4 + (t120 * 0.3141592653589793D1 * (t30 - t294 *
     # t71) - 0.90D2 * t51 * (t299 * t30 / 0.2D1 + t47 - t299 * t294 * t
     #71 / 0.6D1 - t294 * t39) + t151 + 0.180D3 * t110 * 0.3141592653589
     #793D1 * (-t294 * t30 + t299 * t71 / 0.2D1 + t39)) * t183 / 0.1440D
     #4
      t320 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t319)
      t322 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t319)
      t324 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t319)
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
      t338 = t76 * t7
      t339 = t4 * t330
      t340 = t327 ** 2
      t341 = t339 * t340
      t344 = log(0.4D1 * t338 * t341)
      t345 = -t327
      t346 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t345, 0.10D1, 0
     #.10D1, x4)
      t348 = t344 ** 2
      t349 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t345, 0.10D1, 0
     #.10D1, x4)
      t352 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t345, 0.10D1, 0
     #.10D1, x4)
      t354 = t339 * t340 * t85
      t357 = log(-0.4D1 * t338 * t354)
      t358 = t357 * z
      t360 = t357 ** 2
      t367 = x3 * t329
      t369 = Sqrt(-t367 * t84)
      t373 = x3 * x1
      t374 = t373 * z
      t375 = 0.3D1 * t374
      t376 = x1 * t3
      t377 = x3 * t3
      t378 = t377 * x1
      t380 = 0.2D1 * t76 * z
      t381 = t76 * t3
      t382 = 0.2D1 * t373
      t383 = -z + 0.2D1 * t98 * t369 * z + t328 - t375 - t376 + t378 + t
     #380 - t381 - x3 + t382 - t76
      t384 = 0.1D1 / t383
      t390 = z * t346
      t403 = 0.3141592653589793D1 * (t349 + z * t349 * t329 * t384)
      t408 = (-0.90D2 * t51 * (-t344 * t346 + t348 * t349 / 0.2D1 + t352
     # + (-t358 * t346 + t360 * z * t349 / 0.2D1 + z * t352) * t329 * t3
     #84) + 0.180D3 * t110 * 0.3141592653589793D1 * (t346 - t344 * t349 
     #+ (t390 - t358 * t349) * t329 * t384) + t120 * t403) * t127 * t129
     # / 0.1440D4
      t411 = log(0.4D1 * t132 * t341)
      t413 = -t346 + t411 * t349
      t416 = t411 ** 2
      t419 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, t345, 0.10D1, 0
     #.10D1, x4)
      t424 = -t416 * t346 / 0.2D1 - t419 + t416 * t411 * t349 / 0.6D1 + 
     #t411 * t352
      t427 = 0.3141592653589793D1 * t349
      t428 = t150 * t427
      t432 = t411 * t346 - t416 * t349 / 0.2D1 - t352
      t439 = t163 * t132
      t442 = log(-0.4D1 * t439 * t354)
      t448 = t330 * t340
      t452 = log(0.4D1 * t168 * t8 * t448)
      t462 = (-0.90D2 * t51 * ((t390 - t442 * z * t349) * t329 * t384 + 
     #t346 - t452 * t349) + 0.180D3 * t110 * t403) * t127 * t184 / 0.720
     #D3
      t463 = t187 * t7
      t466 = log(0.4D1 * t463 * t341)
      t468 = t466 ** 2
      t483 = (-0.90D2 * t51 * (t466 * t346 - t468 * t349 / 0.2D1 - t352)
     # + 0.180D3 * t110 * 0.3141592653589793D1 * (-t346 + t466 * t349) -
     # t120 * t427) * t183 * t129 / 0.720D3
      t484 = -t408 + (t120 * 0.3141592653589793D1 * t413 - 0.90D2 * t51 
     #* t424 - t428 + 0.180D3 * t110 * 0.3141592653589793D1 * t432) * t1
     #29 / 0.1440D4 - t462 + t483
      t485 = FJET(XB1, XB2, s, 0.0D0, t326, -t332, 0.0D0, -t337, t484)
      t488 = x2 * t1 * s
      t489 = -0.1D1 + x2
      t491 = t489 * t1 * s
      t492 = t8 * t489
      t495 = log(-0.4D1 * t163 * t492)
      t496 = -t489
      t497 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, 0
     #.10D1, x4)
      t499 = t495 ** 2
      t500 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, 0
     #.10D1, x4)
      t503 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, 0
     #.10D1, x4)
      t512 = 0.3141592653589793D1 * t500
      t513 = t120 * t512
      t517 = (-0.90D2 * t51 * (-t495 * t497 + t499 * t500 / 0.2D1 + t503
     #) + 0.180D3 * t110 * 0.3141592653589793D1 * (t497 - t495 * t500) +
     # t513) * t127 * t183 / 0.1440D4
      t518 = t4 * t489
      t521 = log(-0.4D1 * t291 * t518)
      t523 = -t497 + t521 * t500
      t526 = t521 ** 2
      t529 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, 0
     #.10D1, x4)
      t534 = -t526 * t497 / 0.2D1 - t529 + t526 * t521 * t500 / 0.6D1 + 
     #t521 * t503
      t537 = t150 * t512
      t541 = t521 * t497 - t526 * t500 / 0.2D1 - t503
      t550 = log(-0.4D1 * t168 * t492)
      t560 = (-0.90D2 * t51 * (t497 - t550 * t500) + 0.180D3 * t110 * t5
     #12) * t127 * t184 / 0.720D3
      t563 = log(-0.4D1 * t187 * t492)
      t565 = t563 ** 2
      t579 = (-0.90D2 * t51 * (t563 * t497 - t565 * t500 / 0.2D1 - t503)
     # + 0.180D3 * t110 * 0.3141592653589793D1 * (-t497 + t563 * t500) -
     # t513) * t183 * t129 / 0.720D3
      t580 = -t517 + (t120 * 0.3141592653589793D1 * t523 - 0.90D2 * t51 
     #* t534 - t537 + 0.180D3 * t110 * 0.3141592653589793D1 * t541) * t1
     #83 / 0.1440D4 - t560 + t579
      t581 = FJET(XB1, XB2, s, 0.0D0, t488, 0.0D0, -t491, 0.0D0, t580)
      t583 = x2 * x3
      t586 = Sqrt(x3 * t489 * t84)
      t587 = t98 * t586
      t589 = 0.2D1 * t587 * x2
      t591 = 0.1D1 - x3 + t583
      t592 = 0.1D1 / t591
      t594 = t2 * (0.1D1 - x3 - x2 + t583 + t163 + t589) * t592
      t599 = t2 * x2 * (-0.1D1 + t583 + 0.2D1 * t587) * t592
      t600 = x2 * z
      t601 = t600 - z - x2
      t602 = t84 * t592
      t603 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, -
     #t602, x4)
      t604 = t601 * t603
      t605 = t591 ** 2
      t606 = 0.1D1 / t605
      t608 = t518 * t84 * t606
      t611 = log(0.4D1 * t439 * t608)
      t613 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, -
     #t602, x4)
      t616 = t583 * z
      t617 = t163 * z
      t623 = 0.1D1 / (-t600 - t616 + t617 + z + x3 - t163 + x2 - t589 - 
     #0.2D1 * t587 * z + 0.2D1 * t587 * t600)
      t627 = t110 * 0.3141592653589793D1
      t629 = t601 * t613 * t623
      t639 = log(0.4D1 * t163 * t7 * t608)
      t640 = t639 * t601
      t642 = t639 ** 2
      t646 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t496, -
     #t602, x4)
      t664 = -(-0.90D2 * t51 * (t604 - t611 * t601 * t613) * t623 + 0.18
     #0D3 * t627 * t629) * t127 * t184 / 0.720D3 - (-0.90D2 * t51 * (-t6
     #40 * t603 + t642 * t601 * t613 / 0.2D1 + t601 * t646) * t623 + 0.1
     #80D3 * t110 * 0.3141592653589793D1 * (t604 - t640 * t613) * t623 +
     # t120 * 0.3141592653589793D1 * t629) * t127 * t183 / 0.1440D4
      t665 = FJET(XB1, XB2, s, 0.0D0, t594, 0.0D0, -t599, 0.0D0, t664)
      t668 = t1 * t327
      t670 = t489 * s * t668 * t330
      t672 = x2 * s * t668
      t674 = t334 * t489 * t336
      t675 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t345, t496, 0.1
     #0D1, x4)
      t677 = t339 * t340 * t489
      t680 = log(-0.4D1 * t439 * t677)
      t681 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t345, t496, 0.1
     #0D1, x4)
      t686 = 0.3141592653589793D1 * t681
      t694 = log(-0.4D1 * t463 * t677)
      t696 = t694 ** 2
      t699 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t345, t496, 0.1
     #0D1, x4)
      t713 = -(-0.90D2 * t51 * (-t675 + t680 * t681) - 0.180D3 * t110 * 
     #t686) * t127 * t184 / 0.720D3 + (-0.90D2 * t51 * (-t694 * t675 + t
     #696 * t681 / 0.2D1 + t699) + 0.180D3 * t110 * 0.3141592653589793D1
     # * (t675 - t694 * t681) + t120 * t686) * t183 * t129 / 0.720D3
      t714 = FJET(XB1, XB2, s, 0.0D0, t670, t326, -t672, t674, t713)
      t716 = FJET(XB1, XB2, s, 0.0D0, -t491, 0.0D0, t488, 0.0D0, t580)
      t718 = FJET(XB1, XB2, s, 0.0D0, -t332, t326, 0.0D0, -t337, t484)
      t720 = FJET(XB1, XB2, s, 0.0D0, -t599, 0.0D0, t594, 0.0D0, t664)
      t722 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t319)
      t737 = -t408 + (t120 * 0.3141592653589793D1 * t413 - 0.90D2 * t51 
     #* t424 - t428 + 0.180D3 * t110 * 0.3141592653589793D1 * t432) * t1
     #29 / 0.1440D4 - t462 + t483
      t738 = FJET(XB1, XB2, s, t326, 0.0D0, 0.0D0, -t332, -t337, t737)
      t740 = t320 * t319 + t322 * t319 + t324 * t319 + t485 * t484 + t58
     #1 * t580 + t665 * t664 + t714 * t713 + t716 * t580 + t718 * t484 +
     # t720 * t664 + t722 * t319 + t738 * t737
      t741 = FJET(XB1, XB2, s, t326, -t672, 0.0D0, t670, t674, t713)
      t756 = -t517 + (t120 * 0.3141592653589793D1 * t523 - 0.90D2 * t51 
     #* t534 - t537 + 0.180D3 * t110 * 0.3141592653589793D1 * t541) * t1
     #83 / 0.1440D4 - t560 + t579
      t757 = FJET(XB1, XB2, s, t488, 0.0D0, -t491, 0.0D0, 0.0D0, t756)
      t759 = FJET(XB1, XB2, s, t594, 0.0D0, -t599, 0.0D0, 0.0D0, t664)
      t761 = FJET(XB1, XB2, s, t670, 0.0D0, -t672, t326, t674, t713)
      t764 = t326 * t583 * t592
      t765 = t2 * t327
      t766 = t489 * t84
      t768 = Sqrt(t367 * t766)
      t769 = t98 * t768
      t771 = 0.2D1 * t769 * x2
      t772 = t163 * t328
      t773 = t163 * x1
      t777 = t765 * (t771 + t772 - t773 + 0.1D1 - x3 + t163 - x2 + t583)
     # * t330 * t592
      t781 = t84 * s * t1 * x1 * t592
      t787 = t765 * x2 * (-0.1D1 + t583 + x1 - t373 - t328 + t374 + 0.2D
     #1 * t769) * t330 * t592
      t790 = x2 * x1
      t791 = t790 * z
      t795 = -t375 + t378 + t380 - t381 - z - x3 + t328 - t376 + t382 - 
     #t76 - x2 + t771 - t773 + 0.2D1 * t769 * z - 0.3D1 * t791 - t583 * 
     #x1 + t76 * x2
      t797 = x2 * t75
      t815 = t790 * t3 + 0.2D1 * t797 * z - t797 * t3 + t163 + t600 - t7
     #97 + 0.2D1 * t790 + 0.2D1 * t769 * t791 + t616 - t617 + t772 - 0.2
     #D1 * t769 * t790 - 0.2D1 * t769 * t600 + 0.2D1 * t583 * t328 - t37
     #7 * t790 - 0.2D1 * t76 * t600 + t76 * t3 * x2
      t817 = 0.1D1 / (t795 + t815)
      t818 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t345, t496, -t6
     #02, x4)
      t824 = log(0.4D1 * t164 * t448 * t766 * t606)
      t826 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t345, t496, -t6
     #02, x4)
      t829 = z - t600 + t791 + x2 - t790
      t839 = 0.90D2 * t51 * (t817 * t818 - t824 * t817 * t826) * t829 * 
     #t329 - 0.180D3 * t627 * t817 * t826 * t829 * t329
      t842 = t839 * t127 * t184 / 0.720D3
      t843 = FJET(XB1, XB2, s, t764, -t777, -t781, t787, t674, -t842)
      t846 = t127 * t183 * t129
      t849 = FJET(XB1, XB2, s, t787, -t781, -t777, t764, t674, -t842)
      t853 = FJET(XB1, XB2, s, -t491, 0.0D0, t488, 0.0D0, 0.0D0, t756)
      t855 = FJET(XB1, XB2, s, -t332, 0.0D0, 0.0D0, t326, -t337, t737)
      t857 = FJET(XB1, XB2, s, -t672, t326, t670, 0.0D0, t674, t713)
      t859 = FJET(XB1, XB2, s, -t599, 0.0D0, t594, 0.0D0, 0.0D0, t664)
      t861 = FJET(XB1, XB2, s, -t781, t787, t764, -t777, t674, -t842)
      t865 = FJET(XB1, XB2, s, -t777, t764, t787, -t781, t674, -t842)
      t869 = t741 * t713 + t757 * t756 + t759 * t664 + t761 * t713 - t84
     #3 * t839 * t846 / 0.720D3 - t849 * t839 * t846 / 0.720D3 + t853 * 
     #t756 + t855 * t737 + t857 * t713 + t859 * t664 - t861 * t839 * t84
     #6 / 0.720D3 - t865 * t839 * t846 / 0.720D3
      rrgg2qqbarht7s1e1 = t740 + t869

      end function



      doubleprecision function rrgg2qqbarht7s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 ** 2
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t22 = log(-0.4D1 * t10 * t12 * t18)
      t23 = t22 ** 2
      t25 = cos(t7)
      t27 = Sqrt(-x3 * t17)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t39 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t42 = lh * t4
      t43 = 0.3141592653589793D1 * t6
      t45 = 0.180D3 * t42 * t43
      t51 = 0.3141592653589793D1 * t39
      t54 = lh ** 2
      t55 = 0.180D3 * t54
      t56 = 0.3141592653589793D1 ** 2
      t57 = 0.30D2 * t56
      t58 = -t55 + t57
      t59 = t58 * t4
      t60 = t59 * t43
      t61 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t69 = 0.1D1 / x3
      t72 = t12 * t9
      t74 = log(0.4D1 * t72)
      t77 = t74 ** 2
      t83 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t106 = z * t39
      t107 = x2 ** 2
      t108 = t107 * x3
      t109 = t72 * t18
      t112 = log(-0.4D1 * t108 * t109)
      t119 = log(0.4D1 * t108 * t72)
      t126 = -z * t6 * t32 - t6
      t129 = 0.180D3 * t42 * 0.3141592653589793D1 * t126
      t132 = 0.1D1 / x2
      t135 = t107 * t9
      t138 = log(0.4D1 * t135 * t12)
      t140 = t138 ** 2
      t154 = x1 ** 2
      t155 = x3 * t154
      t158 = log(0.4D1 * t155 * t72)
      t162 = log(-0.4D1 * t155 * t109)
      t172 = 0.1D1 / x1
      t177 = t69 * t132 * t172
      t180 = t107 * t154
      t183 = log(0.4D1 * t180 * t72)
      t192 = t154 * t9
      t195 = log(0.4D1 * t192 * t12)
      t197 = t195 ** 2
      t211 = (-0.90D2 * t5 * t6 * (t16 / 0.2D1 + t23 * z * t32 / 0.2D1) 
     #+ (-0.90D2 * t5 * t39 + t45) * (-t15 - t22 * z * t32) + (0.180D3 *
     # t42 * t51 + t60 - 0.90D2 * t5 * t61) * (0.1D1 + z * t32)) * t69 /
     # 0.2880D4 + (-0.180D3 * t74 * lh - 0.45D2 * t77 - t55 + t57) * t4 
     #* t51 / 0.2880D4 - t5 * t83 / 0.32D2 + (0.90D2 * t77 * lh - 0.60D2
     # * lh * t56 + 0.2884936567583026D3 + 0.120D3 * t54 * lh + 0.15D2 *
     # t77 * t74 - t74 * t58) * t4 * t43 / 0.2880D4 + (0.180D3 * lh + 0.
     #90D2 * t74) * t4 * 0.3141592653589793D1 * t61 / 0.2880D4 - (-0.90D
     #2 * t5 * (-t39 - (t106 - t112 * z * t6) * t32 + t119 * t6) + t129)
     # * t69 * t132 / 0.1440D4 + (-0.90D2 * t5 * (-t138 * t39 + t140 * t
     #6 / 0.2D1 + t61) + 0.180D3 * t42 * 0.3141592653589793D1 * (t39 - t
     #138 * t6) + t60) * t132 / 0.1440D4 - (-0.90D2 * t5 * (-t39 + t158 
     #* t6 - (t106 - t162 * z * t6) * t32) + t129) * t69 * t172 / 0.1440
     #D4 + t5 * t126 * t177 / 0.8D1 + (-0.90D2 * t5 * (-t183 * t6 + t39)
     # + t45) * t132 * t172 / 0.720D3 + (-0.90D2 * t5 * (-t195 * t39 + t
     #197 * t6 / 0.2D1 + t61) + 0.180D3 * t42 * 0.3141592653589793D1 * (
     #t39 - t195 * t6) + t60) * t172 / 0.1440D4
      t212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t211)
      t214 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t211)
      t216 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t211)
      t218 = t2 * x1
      t219 = -0.1D1 + x1
      t220 = x1 * z
      t221 = 0.1D1 - x1 + t220
      t222 = 0.1D1 / t221
      t224 = t2 * t219 * t222
      t225 = t1 ** 2
      t226 = s * t225
      t228 = x1 * t219 * t222
      t229 = t226 * t228
      t230 = -t219
      t231 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t230, 0.10D1, 0
     #.10D1, x4)
      t232 = t155 * t9
      t233 = t12 * t222
      t234 = t219 ** 2
      t235 = t233 * t234
      t238 = log(0.4D1 * t232 * t235)
      t239 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t230, 0.10D1, 0
     #.10D1, x4)
      t246 = log(-0.4D1 * t232 * t233 * t234 * t18)
      t251 = x3 * t221
      t253 = Sqrt(-t251 * t17)
      t257 = x3 * x1
      t258 = t257 * z
      t259 = 0.3D1 * t258
      t260 = x1 * t11
      t261 = x3 * t11
      t262 = t261 * x1
      t264 = 0.2D1 * t155 * z
      t265 = t155 * t11
      t266 = 0.2D1 * t257
      t267 = -z + 0.2D1 * t25 * t253 * z + t220 - t259 - t260 + t262 + t
     #264 - t265 - x3 + t266 - t155
      t268 = 0.1D1 / t267
      t276 = t239 + z * t239 * t221 * t268
      t283 = (-0.90D2 * t5 * (t231 - t238 * t239 + (z * t231 - t246 * z 
     #* t239) * t221 * t268) + 0.180D3 * t42 * 0.3141592653589793D1 * t2
     #76) * t69 * t172 / 0.1440D4
      t286 = t5 * t276 * t177 / 0.8D1
      t287 = t180 * t9
      t290 = log(0.4D1 * t287 * t235)
      t295 = 0.3141592653589793D1 * t239
      t301 = (-0.90D2 * t5 * (-t231 + t290 * t239) - 0.180D3 * t42 * t29
     #5) * t132 * t172 / 0.720D3
      t304 = log(0.4D1 * t192 * t235)
      t306 = t304 ** 2
      t309 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t230, 0.10D1, 0
     #.10D1, x4)
      t310 = t304 * t231 - t306 * t239 / 0.2D1 - t309
      t314 = -t231 + t304 * t239
      t318 = t59 * t295
      t322 = -t283 + t286 + t301 + (-0.90D2 * t5 * t310 + 0.180D3 * t42 
     #* 0.3141592653589793D1 * t314 - t318) * t172 / 0.1440D4
      t323 = FJET(XB1, XB2, s, 0.0D0, t218, -t224, 0.0D0, -t229, t322)
      t326 = x2 * t1 * s
      t327 = -0.1D1 + x2
      t329 = t327 * t1 * s
      t330 = -t327
      t331 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t330, 0
     #.10D1, x4)
      t332 = t72 * t327
      t335 = log(-0.4D1 * t108 * t332)
      t336 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t330, 0
     #.10D1, x4)
      t341 = 0.3141592653589793D1 * t336
      t343 = 0.180D3 * t42 * t341
      t347 = (-0.90D2 * t5 * (t331 - t335 * t336) + t343) * t69 * t132 /
     # 0.1440D4
      t348 = t12 * t327
      t351 = log(-0.4D1 * t135 * t348)
      t353 = t351 ** 2
      t356 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t330, 0
     #.10D1, x4)
      t357 = t351 * t331 - t353 * t336 / 0.2D1 - t356
      t361 = -t331 + t351 * t336
      t365 = t59 * t341
      t371 = t5 * t336 * t177 / 0.8D1
      t374 = log(-0.4D1 * t180 * t332)
      t382 = (-0.90D2 * t5 * (-t331 + t374 * t336) - t343) * t132 * t172
     # / 0.720D3
      t383 = -t347 + (-0.90D2 * t5 * t357 + 0.180D3 * t42 * 0.3141592653
     #589793D1 * t361 - t365) * t132 / 0.1440D4 + t371 + t382
      t384 = FJET(XB1, XB2, s, 0.0D0, t326, 0.0D0, -t329, 0.0D0, t383)
      t386 = x2 * x3
      t389 = Sqrt(x3 * t327 * t17)
      t390 = t25 * t389
      t392 = 0.2D1 * t390 * x2
      t394 = 0.1D1 - x3 + t386
      t395 = 0.1D1 / t394
      t397 = t2 * (0.1D1 - x3 - x2 + t386 + t108 + t392) * t395
      t402 = t2 * x2 * (-0.1D1 + t386 + 0.2D1 * t390) * t395
      t403 = x2 * z
      t404 = t403 - z - x2
      t405 = t17 * t395
      t406 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t330, -
     #t405, x4)
      t409 = t394 ** 2
      t415 = log(0.4D1 * t108 * t9 * t348 * t17 / t409)
      t417 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t330, -
     #t405, x4)
      t420 = t386 * z
      t421 = t108 * z
      t427 = 0.1D1 / (-t403 - t420 + t421 + z + x3 - t108 + x2 - t392 - 
     #0.2D1 * t390 * z + 0.2D1 * t390 * t403)
      t432 = t404 * t417
      t446 = -(-0.90D2 * t5 * (t404 * t406 - t415 * t404 * t417) * t427 
     #+ 0.180D3 * t42 * 0.3141592653589793D1 * t432 * t427) * t69 * t132
     # / 0.1440D4 + t5 * t432 * t427 * t69 * t132 * t172 / 0.8D1
      t447 = FJET(XB1, XB2, s, 0.0D0, t397, 0.0D0, -t402, 0.0D0, t446)
      t450 = t1 * t219
      t452 = t327 * s * t450 * t222
      t454 = x2 * s * t450
      t456 = t226 * t327 * t228
      t457 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t230, t330, 0.1
     #0D1, x4)
      t461 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t230, t330, 0.1
     #0D1, x4)
      t466 = log(-0.4D1 * t287 * t233 * t234 * t327)
      t478 = -t5 * t457 * t177 / 0.8D1 + (-0.90D2 * t5 * (t461 - t466 * 
     #t457) + 0.180D3 * t42 * 0.3141592653589793D1 * t457) * t132 * t172
     # / 0.720D3
      t479 = FJET(XB1, XB2, s, 0.0D0, t452, t218, -t454, t456, t478)
      t481 = FJET(XB1, XB2, s, 0.0D0, -t329, 0.0D0, t326, 0.0D0, t383)
      t483 = FJET(XB1, XB2, s, 0.0D0, -t224, t218, 0.0D0, -t229, t322)
      t485 = FJET(XB1, XB2, s, 0.0D0, -t402, 0.0D0, t397, 0.0D0, t446)
      t487 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t211)
      t499 = -t283 + t286 + t301 + (-0.90D2 * t5 * t310 + 0.180D3 * t42 
     #* 0.3141592653589793D1 * t314 - t318) * t172 / 0.1440D4
      t500 = FJET(XB1, XB2, s, t218, 0.0D0, 0.0D0, -t224, -t229, t499)
      t502 = t212 * t211 + t214 * t211 + t216 * t211 + t323 * t322 + t38
     #4 * t383 + t447 * t446 + t479 * t478 + t481 * t383 + t483 * t322 +
     # t485 * t446 + t487 * t211 + t499 * t500
      t503 = FJET(XB1, XB2, s, t218, -t454, 0.0D0, t452, t456, t478)
      t515 = -t347 + (-0.90D2 * t5 * t357 + 0.180D3 * t42 * 0.3141592653
     #589793D1 * t361 - t365) * t132 / 0.1440D4 + t371 + t382
      t516 = FJET(XB1, XB2, s, t326, 0.0D0, -t329, 0.0D0, 0.0D0, t515)
      t518 = FJET(XB1, XB2, s, t397, 0.0D0, -t402, 0.0D0, 0.0D0, t446)
      t520 = FJET(XB1, XB2, s, t452, 0.0D0, -t454, t218, t456, t478)
      t523 = t218 * t386 * t395
      t524 = t2 * t219
      t527 = Sqrt(t251 * t327 * t17)
      t528 = t25 * t527
      t530 = 0.2D1 * t528 * x2
      t531 = t108 * t220
      t532 = t108 * x1
      t536 = t524 * (t530 + t531 - t532 + 0.1D1 - x3 + t108 - x2 + t386)
     # * t222 * t395
      t540 = t17 * s * t1 * x1 * t395
      t546 = t524 * x2 * (-0.1D1 + t386 + x1 - t257 - t220 + t258 + 0.2D
     #1 * t528) * t222 * t395
      t547 = x2 * t154
      t548 = x2 * x1
      t552 = -x3 - z + t108 + t403 - t547 + 0.2D1 * t548 - t155 + t220 -
     # t260 + t266 - t259 + t262 + t264 - t265 + t530 - t532 + 0.2D1 * t
     #528 * z
      t553 = t548 * z
      t574 = -0.3D1 * t553 - t386 * x1 + t155 * x2 + t548 * t11 + 0.2D1 
     #* t547 * z - t547 * t11 - x2 + t531 - 0.2D1 * t528 * t548 - 0.2D1 
     #* t528 * t403 + 0.2D1 * t386 * t220 - t261 * t548 - 0.2D1 * t155 *
     # t403 + t155 * t11 * x2 + 0.2D1 * t528 * t553 + t420 - t421
      t576 = 0.1D1 / (t552 + t574)
      t577 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t230, t330, -t4
     #05, x4)
      t582 = (z - t403 + t553 + x2 - t548) * t221 * t177
      t584 = t5 * t576 * t577 * t582 / 0.8D1
      t585 = FJET(XB1, XB2, s, t523, -t536, -t540, t546, t456, -t584)
      t588 = 0.3141592653589793D1 * t576 * t577
      t592 = FJET(XB1, XB2, s, t546, -t540, -t536, t523, t456, -t584)
      t597 = FJET(XB1, XB2, s, -t329, 0.0D0, t326, 0.0D0, 0.0D0, t515)
      t599 = FJET(XB1, XB2, s, -t224, 0.0D0, 0.0D0, t218, -t229, t499)
      t601 = FJET(XB1, XB2, s, -t454, t218, t452, 0.0D0, t456, t478)
      t603 = FJET(XB1, XB2, s, -t402, 0.0D0, t397, 0.0D0, 0.0D0, t446)
      t605 = FJET(XB1, XB2, s, -t540, t546, t523, -t536, t456, -t584)
      t610 = FJET(XB1, XB2, s, -t536, t523, t546, -t540, t456, -t584)
      t615 = t503 * t478 + t516 * t515 + t518 * t446 + t520 * t478 - t58
     #5 * t4 * t588 * t582 / 0.8D1 - t592 * t4 * t588 * t582 / 0.8D1 + t
     #597 * t515 + t599 * t499 + t601 * t478 + t603 * t446 - t605 * t4 *
     # t588 * t582 / 0.8D1 - t610 * t4 * t588 * t582 / 0.8D1
      rrgg2qqbarht7s1e0 = t502 + t615

      end function



      doubleprecision function rrgg2qqbarht7s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = -0.1D1 + x3
      t21 = log(-0.4D1 * t10 * t12 / t16)
      t23 = cos(t7)
      t25 = Sqrt(-x3 * t16)
      t30 = 0.1D1 / (-z - x3 + 0.2D1 * t23 * t25 * z)
      t36 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t39 = lh * t4
      t40 = 0.3141592653589793D1 * t6
      t42 = 0.180D3 * t39 * t40
      t48 = 0.1D1 / x3
      t54 = log(0.4D1 * t12 * t9)
      t63 = t54 ** 2
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t73 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t79 = (-z * t6 * t30 - t6) * t48
      t80 = 0.1D1 / x2
      t84 = x2 ** 2
      t85 = t84 * t9
      t88 = log(0.4D1 * t85 * t12)
      t97 = 0.1D1 / x1
      t101 = x1 ** 2
      t102 = t101 * t9
      t105 = log(0.4D1 * t102 * t12)
      t116 = (-0.90D2 * t5 * t6 * (-t15 - t21 * z * t30) + (-0.90D2 * t5
     # * t36 + t42) * (0.1D1 + z * t30)) * t48 / 0.2880D4 + (0.180D3 * l
     #h + 0.90D2 * t54) * t4 * 0.3141592653589793D1 * t36 / 0.2880D4 + (
     #-0.180D3 * t54 * lh - 0.45D2 * t63 - 0.180D3 * t65 + 0.30D2 * t67)
     # * t4 * t40 / 0.2880D4 - t5 * t73 / 0.32D2 + t5 * t79 * t80 / 0.16
     #D2 + (-0.90D2 * t5 * (t36 - t88 * t6) + t42) * t80 / 0.1440D4 - t5
     # * t6 * t80 * t97 / 0.8D1 + (-0.90D2 * t5 * (t36 - t105 * t6) + t4
     #2) * t97 / 0.1440D4 + t5 * t79 * t97 / 0.16D2
      t117 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t116)
      t119 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t116)
      t121 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t116)
      t123 = t2 * x1
      t124 = -0.1D1 + x1
      t125 = x1 * z
      t126 = 0.1D1 - x1 + t125
      t127 = 0.1D1 / t126
      t129 = t2 * t124 * t127
      t130 = t1 ** 2
      t131 = s * t130
      t133 = x1 * t124 * t127
      t134 = t131 * t133
      t135 = -t124
      t136 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t135, 0.10D1, 0
     #.10D1, x4)
      t140 = t5 * t136 * t80 * t97 / 0.8D1
      t141 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t135, 0.10D1, 0
     #.10D1, x4)
      t143 = t124 ** 2
      t147 = log(0.4D1 * t102 * t12 * t127 * t143)
      t149 = -t141 + t147 * t136
      t154 = 0.180D3 * t39 * 0.3141592653589793D1 * t136
      t161 = Sqrt(-x3 * t126 * t16)
      t165 = x3 * x1
      t171 = x3 * t101
      t176 = -z + 0.2D1 * t23 * t161 * z + t125 - 0.3D1 * t165 * z - x1 
     #* t11 + x3 * t11 * x1 + 0.2D1 * t171 * z - t171 * t11 - x3 + 0.2D1
     # * t165 - t171
      t184 = t5 * (t136 + z * t136 * t126 / t176) * t48 * t97 / 0.16D2
      t185 = t140 + (-0.90D2 * t5 * t149 - t154) * t97 / 0.1440D4 + t184
      t186 = FJET(XB1, XB2, s, 0.0D0, t123, -t129, 0.0D0, -t134, t185)
      t189 = x2 * t1 * s
      t190 = -0.1D1 + x2
      t192 = t190 * t1 * s
      t193 = -t190
      t194 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t193, 0
     #.10D1, x4)
      t198 = t5 * t194 * t48 * t80 / 0.16D2
      t199 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t193, 0
     #.10D1, x4)
      t203 = log(-0.4D1 * t85 * t12 * t190)
      t205 = -t199 + t203 * t194
      t210 = 0.180D3 * t39 * 0.3141592653589793D1 * t194
      t217 = t5 * t194 * t80 * t97 / 0.8D1
      t218 = t198 + (-0.90D2 * t5 * t205 - t210) * t80 / 0.1440D4 + t217
      t219 = FJET(XB1, XB2, s, 0.0D0, t189, 0.0D0, -t192, 0.0D0, t218)
      t221 = x2 * x3
      t222 = x3 * t84
      t225 = Sqrt(x3 * t190 * t16)
      t226 = t23 * t225
      t228 = 0.2D1 * t226 * x2
      t231 = 0.1D1 / (0.1D1 - x3 + t221)
      t233 = t2 * (0.1D1 - x3 - x2 + t221 + t222 + t228) * t231
      t238 = t2 * x2 * (-0.1D1 + t221 + 0.2D1 * t226) * t231
      t239 = x2 * z
      t240 = t239 - z - x2
      t243 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t193, -
     #t16 * t231, x4)
      t254 = t243 / (-t239 - t221 * z + t222 * z + z + x3 - t222 + x2 - 
     #t228 - 0.2D1 * t226 * z + 0.2D1 * t226 * t239) * t48 * t80
      t256 = t5 * t240 * t254 / 0.16D2
      t257 = FJET(XB1, XB2, s, 0.0D0, t233, 0.0D0, -t238, 0.0D0, t256)
      t259 = 0.3141592653589793D1 * t240
      t264 = t1 * t124
      t266 = t190 * s * t264 * t127
      t268 = x2 * s * t264
      t270 = t131 * t190 * t133
      t271 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t135, t193, 0.1
     #0D1, x4)
      t273 = t271 * t80 * t97
      t275 = t5 * t273 / 0.8D1
      t276 = FJET(XB1, XB2, s, 0.0D0, t266, t123, -t268, t270, -t275)
      t281 = FJET(XB1, XB2, s, 0.0D0, -t192, 0.0D0, t189, 0.0D0, t218)
      t283 = FJET(XB1, XB2, s, 0.0D0, -t129, t123, 0.0D0, -t134, t185)
      t285 = FJET(XB1, XB2, s, 0.0D0, -t238, 0.0D0, t233, 0.0D0, t256)
      t290 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t116)
      t298 = t140 + (-0.90D2 * t5 * t149 - t154) * t97 / 0.1440D4 + t184
      t299 = FJET(XB1, XB2, s, t123, 0.0D0, 0.0D0, -t129, -t134, t298)
      t301 = FJET(XB1, XB2, s, t123, -t268, 0.0D0, t266, t270, -t275)
      t312 = t198 + (-0.90D2 * t5 * t205 - t210) * t80 / 0.1440D4 + t217
      t313 = FJET(XB1, XB2, s, t189, 0.0D0, -t192, 0.0D0, 0.0D0, t312)
      t315 = FJET(XB1, XB2, s, t233, 0.0D0, -t238, 0.0D0, 0.0D0, t256)
      t320 = FJET(XB1, XB2, s, t266, 0.0D0, -t268, t123, t270, -t275)
      t325 = FJET(XB1, XB2, s, -t192, 0.0D0, t189, 0.0D0, 0.0D0, t312)
      t327 = FJET(XB1, XB2, s, -t129, 0.0D0, 0.0D0, t123, -t134, t298)
      t329 = FJET(XB1, XB2, s, -t238, 0.0D0, t233, 0.0D0, 0.0D0, t256)
      t334 = FJET(XB1, XB2, s, -t268, t123, t266, 0.0D0, t270, -t275)
      rrgg2qqbarht7s1em1 = t117 * t116 + t119 * t116 + t121 * t116 + t18
     #6 * t185 + t219 * t218 + t257 * t4 * t259 * t254 / 0.16D2 - t276 *
     # t4 * 0.3141592653589793D1 * t273 / 0.8D1 + t281 * t218 + t283 * t
     #185 + t285 * t4 * t259 * t254 / 0.16D2 + t290 * t116 + t299 * t298
     # - t301 * t4 * 0.3141592653589793D1 * t273 / 0.8D1 + t313 * t312 +
     # t315 * t4 * t259 * t254 / 0.16D2 - t320 * t4 * 0.3141592653589793
     #D1 * t273 / 0.8D1 + t325 * t312 + t327 * t298 + t329 * t4 * t259 *
     # t254 / 0.16D2 - t334 * t4 * 0.3141592653589793D1 * t273 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht7s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t11 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t32 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t36 = z ** 2
      t38 = Sin(t7)
      t39 = t38 ** 2
      t42 = log(0.4D1 / t36 * t39)
      t49 = -t5 * t6 * (0.1D1 + z / (-z - x3 + 0.2D1 * t8 * t11 * z)) / 
     #x3 / 0.32D2 - t5 * t6 * t24 / 0.16D2 - t5 * t6 * t28 / 0.16D2 - t5
     # * t32 / 0.32D2 + (0.180D3 * lh + 0.90D2 * t42) * t4 * 0.314159265
     #3589793D1 * t6 / 0.2880D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t54 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t56 = t2 * x1
      t57 = -0.1D1 + x1
      t60 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t62 = t2 * t57 * t60
      t63 = t1 ** 2
      t67 = s * t63 * x1 * t57 * t60
      t69 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, -t57, 0.10D1, 0.
     #10D1, x4)
      t72 = t5 * t69 * t28 / 0.16D2
      t73 = FJET(XB1, XB2, s, 0.0D0, t56, -t62, 0.0D0, -t67, t72)
      t76 = 0.3141592653589793D1 * t69 * t28
      t80 = x2 * t1 * s
      t81 = -0.1D1 + x2
      t83 = t81 * t1 * s
      t85 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, -t81, 0.
     #10D1, x4)
      t88 = t5 * t85 * t24 / 0.16D2
      t89 = FJET(XB1, XB2, s, 0.0D0, t80, 0.0D0, -t83, 0.0D0, t88)
      t92 = 0.3141592653589793D1 * t85 * t24
      t95 = FJET(XB1, XB2, s, 0.0D0, -t83, 0.0D0, t80, 0.0D0, t88)
      t99 = FJET(XB1, XB2, s, 0.0D0, -t62, t56, 0.0D0, -t67, t72)
      t103 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t105 = FJET(XB1, XB2, s, t56, 0.0D0, 0.0D0, -t62, -t67, t72)
      t109 = FJET(XB1, XB2, s, t80, 0.0D0, -t83, 0.0D0, 0.0D0, t88)
      t113 = FJET(XB1, XB2, s, -t83, 0.0D0, t80, 0.0D0, 0.0D0, t88)
      t117 = FJET(XB1, XB2, s, -t62, 0.0D0, 0.0D0, t56, -t67, t72)
      rrgg2qqbarht7s1em2 = t50 * t49 + t52 * t49 + t54 * t49 + t73 * t4 
     #* t76 / 0.16D2 + t89 * t4 * t92 / 0.16D2 + t95 * t4 * t92 / 0.16D2
     # + t99 * t4 * t76 / 0.16D2 + t103 * t49 + t105 * t4 * t76 / 0.16D2
     # + t109 * t4 * t92 / 0.16D2 + t113 * t4 * t92 / 0.16D2 + t117 * t4
     # * t76 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t8 = t4 * 0.3141592653589793D1 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = 0.3141592653589793D1 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s1em3 = -t9 * t4 * t11 / 0.32D2 - t13 * t4 * t11 / 0.
     #32D2 - t16 * t4 * t11 / 0.32D2 - t19 * t4 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht7s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      rrgg2qqbarht7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht7s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = -0.1D1 + x1
      t5 = x1 * z
      t6 = 0.1D1 - x1 + t5
      t7 = 0.1D1 / t6
      t9 = t2 * t3 * x2 * t7
      t10 = -0.1D1 + x2
      t12 = t1 * t3
      t13 = t10 * s * t12
      t14 = t2 * x1
      t15 = t1 ** 2
      t20 = s * t15 * x2 * x1 * t3 * t7
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = t22 * 0.3141592653589793D1
      t24 = -t3
      t25 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.10D1,
     # x4)
      t26 = x2 * x3
      t27 = x1 ** 2
      t28 = x4 * 0.3141592653589793D1
      t29 = Sin(t28)
      t30 = t29 ** 2
      t31 = t27 * t30
      t32 = t26 * t31
      t33 = z ** 2
      t34 = 0.1D1 / t33
      t35 = t3 ** 2
      t37 = t10 ** 2
      t39 = t34 * t35 * t7 * t37
      t42 = log(0.4D1 * t32 * t39)
      t43 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.10D1,
     # x4)
      t48 = lh * t22
      t49 = 0.3141592653589793D1 * t43
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x2
      t56 = 0.1D1 / x1
      t57 = t55 * t56
      t59 = x2 * t27
      t60 = t59 * t30
      t63 = log(0.4D1 * t60 * t39)
      t65 = t63 ** 2
      t68 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.10D1,
     # x4)
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = -t78 + t80
      t82 = t81 * t22
      t88 = (-0.90D2 * t23 * (t25 - t42 * t43) + 0.180D3 * t48 * t49) * 
     #t53 * t57 / 0.720D3 + (-0.90D2 * t23 * (-t63 * t25 + t65 * t43 / 0
     #.2D1 + t68) + 0.180D3 * t48 * 0.3141592653589793D1 * (t25 - t63 * 
     #t43) + t82 * t49) * t55 * t56 / 0.720D3
      t89 = FJET(XB1, XB2, s, -t9, 0.0D0, t13, t14, -t20, t88)
      t94 = log(0.4D1 * x3 * t30 * t34)
      t97 = t94 ** 2
      t100 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t102 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t103 = 0.90D2 * t102
      t107 = 0.60D2 * lh * t79
      t109 = 0.120D3 * t77 * lh
      t112 = -t81
      t115 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t117 = 0.180D3 * lh
      t120 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t126 = t34 * t30
      t128 = log(0.4D1 * t126)
      t129 = t128 ** 2
      t132 = t129 * t128
      t154 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t161 = t107 - 0.2884936567583026D3 - t109
      t164 = t79 ** 2
      t165 = t77 ** 2
      t169 = t129 ** 2
      t173 = 0.3141592653589793D1 * t115
      t176 = x3 * t27
      t179 = log(0.4D1 * t176 * t126)
      t181 = t179 ** 2
      t192 = t82 * t173
      t197 = t31 * t34
      t199 = log(0.4D1 * t197)
      t202 = t199 ** 2
      t222 = log(0.4D1 * t26 * t197)
      t235 = log(0.4D1 * t59 * t126)
      t237 = t235 ** 2
      t254 = log(0.4D1 * t26 * t126)
      t256 = t254 ** 2
      t271 = x2 * t30
      t274 = log(0.4D1 * t271 * t34)
      t279 = t274 ** 2
      t290 = -t161 * t22
      t302 = t23 * (-(0.180D3 * t94 * lh + 0.45D2 * t97 + t78 - t80) * t
     #100 - t103 - (-0.90D2 * t97 * lh + t107 - 0.2884936567583026D3 - t
     #109 - 0.15D2 * t97 * t94 - t94 * t112) * t115 - (-t117 - 0.90D2 * 
     #t94) * t120) * t53 / 0.1440D4 - (-0.90D2 * t129 * lh + t107 - 0.28
     #84936567583026D3 - t109 - 0.15D2 * t132 - t128 * t112) * t22 * 0.3
     #141592653589793D1 * t100 / 0.1440D4 - (0.180D3 * t128 * lh + 0.45D
     #2 * t129 + t78 - t80) * t22 * 0.3141592653589793D1 * t120 / 0.1440
     #D4 - (-t117 - 0.90D2 * t128) * t22 * 0.3141592653589793D1 * t102 /
     # 0.1440D4 - t23 * t154 / 0.16D2 - (0.30D2 * t132 * lh + t129 * t11
     #2 / 0.2D1 - t128 * t161 + 0.5769873135166051D3 * lh + t164 + 0.60D
     #2 * t165 - 0.60D2 * t77 * t79 + 0.15D2 / 0.4D1 * t169) * t22 * t17
     #3 / 0.1440D4 - (-0.90D2 * t23 * (t179 * t100 - t181 * t115 / 0.2D1
     # - t120) + 0.180D3 * t48 * 0.3141592653589793D1 * (-t100 + t179 * 
     #t115) - t192) * t53 * t56 / 0.720D3 - t23 * ((0.180D3 * t199 * lh 
     #+ 0.45D2 * t202 + t78 - t80) * t100 + t103 + (-0.90D2 * t202 * lh 
     #+ t107 - 0.2884936567583026D3 - t109 - 0.15D2 * t202 * t199 - t199
     # * t112) * t115 + (-t117 - 0.90D2 * t199) * t120) * t56 / 0.720D3 
     #+ (-0.90D2 * t23 * (t100 - t222 * t115) + 0.180D3 * t48 * t173) * 
     #t53 * t57 / 0.720D3 + (-0.90D2 * t23 * (-t235 * t100 + t237 * t115
     # / 0.2D1 + t120) + 0.180D3 * t48 * 0.3141592653589793D1 * (t100 - 
     #t235 * t115) + t192) * t55 * t56 / 0.720D3 - (-0.90D2 * t23 * (t25
     #4 * t100 - t256 * t115 / 0.2D1 - t120) + 0.180D3 * t48 * 0.3141592
     #653589793D1 * (-t100 + t254 * t115) - t192) * t53 * t55 / 0.1440D4
     # + (t82 * 0.3141592653589793D1 * (t100 - t274 * t115) - 0.90D2 * t
     #23 * (t279 * t100 / 0.2D1 + t102 - t279 * t274 * t115 / 0.6D1 - t2
     #74 * t120) + t290 * t173 + 0.180D3 * t48 * 0.3141592653589793D1 * 
     #(-t274 * t100 + t279 * t115 / 0.2D1 + t120)) * t55 / 0.1440D4
      t303 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t302)
      t305 = -0.1D1 + x3
      t306 = t305 * s
      t307 = t1 * x1
      t308 = t26 - 0.1D1
      t309 = 0.1D1 / t308
      t311 = t306 * t307 * t309
      t312 = t2 * t3
      t313 = x3 * x1
      t314 = t313 * z
      t315 = cos(t28)
      t319 = Sqrt(-x3 * t6 * x2 * t305)
      t320 = t315 * t319
      t321 = 0.2D1 * t320
      t326 = t312 * t10 * (-t26 - 0.1D1 + x3 + x1 - t313 - t5 + t314 + t
     #321) * t7 * t309
      t329 = t14 * x3 * t10 * t309
      t330 = t26 * t5
      t332 = x2 ** 2
      t333 = t332 * x3
      t334 = t333 * t5
      t335 = t26 * x1
      t337 = t333 * x1
      t338 = 0.3D1 * t26
      t340 = 0.2D1 * t320 * x2
      t341 = 0.2D1 * t330 - t334 - t314 - 0.2D1 * t335 + t337 - x2 - x3 
     #- t321 + t313 - t333 + t338 + t340
      t344 = t312 * t341 * t7 * t309
      t346 = 0.2D1 * t26
      t347 = x2 * z
      t348 = x2 * x1
      t349 = t348 * z
      t354 = t176 * x2
      t359 = 0.1D1 - 0.2D1 * x1 + t346 - t333 + t347 - x2 + t27 + 0.2D1 
     #* t320 * t349 - 0.3D1 * t335 + t337 + t340 - 0.3D1 * t349 + t354 +
     # t348 * t33 + 0.2D1 * t59 * z - t59 * t33
      t376 = t333 * z
      t377 = t26 * z
      t382 = 0.2D1 * t320 * x1 + 0.4D1 * t330 - t334 - x3 * t33 * t348 -
     # 0.2D1 * t176 * t347 + t176 * t33 * x2 - 0.2D1 * t320 * t348 - 0.2
     #D1 * t320 * t5 - 0.2D1 * t320 * t347 + 0.2D1 * t5 + t376 - t377 - 
     #t321 - 0.2D1 * t27 * z + t27 * t33 - t59 + 0.2D1 * t348
      t384 = 0.1D1 / (t359 + t382)
      t385 = -0.1D1 + x1 - t348 - t5 - t347 + x2 + t349
      t386 = t384 * t385
      t387 = t305 * t309
      t388 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t24, x2, t387, 
     #x4)
      t393 = t308 ** 2
      t394 = 0.1D1 / t393
      t396 = t35 * t27
      t401 = log(-0.4D1 * t26 * t305 * t37 * t30 * t394 * t34 * t396 * t
     #7)
      t403 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t24, x2, t387, 
     #x4)
      t410 = t48 * 0.3141592653589793D1
      t415 = -0.90D2 * t23 * (t386 * t388 - t401 * t384 * t385 * t403) *
     # t6 + 0.180D3 * t410 * t386 * t403 * t6
      t418 = t415 * t53 * t57 / 0.720D3
      t419 = FJET(XB1, XB2, s, t311, t326, t329, -t344, -t20, t418)
      t422 = t53 * t55 * t56
      t426 = x2 * t1 * s
      t428 = t10 * t1 * s
      t429 = t126 * t37
      t432 = log(0.4D1 * t26 * t429)
      t433 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t435 = t432 ** 2
      t436 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t439 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t448 = 0.3141592653589793D1 * t436
      t449 = t82 * t448
      t453 = (-0.90D2 * t23 * (-t432 * t433 + t435 * t436 / 0.2D1 + t439
     #) + 0.180D3 * t48 * 0.3141592653589793D1 * (t433 - t432 * t436) + 
     #t449) * t53 * t55 / 0.1440D4
      t454 = t34 * t37
      t457 = log(0.4D1 * t271 * t454)
      t459 = -t433 + t457 * t436
      t462 = t457 ** 2
      t465 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t470 = -t462 * t433 / 0.2D1 - t465 + t462 * t457 * t436 / 0.6D1 + 
     #t457 * t439
      t473 = t290 * t448
      t477 = t457 * t433 - t462 * t436 / 0.2D1 - t439
      t486 = log(0.4D1 * t354 * t429)
      t496 = (-0.90D2 * t23 * (-t433 + t486 * t436) - 0.180D3 * t48 * t4
     #48) * t53 * t57 / 0.720D3
      t499 = log(0.4D1 * t59 * t429)
      t501 = t499 ** 2
      t515 = (-0.90D2 * t23 * (t499 * t433 - t501 * t436 / 0.2D1 - t439)
     # + 0.180D3 * t48 * 0.3141592653589793D1 * (-t433 + t499 * t436) - 
     #t449) * t55 * t56 / 0.720D3
      t516 = -t453 + (t82 * 0.3141592653589793D1 * t459 - 0.90D2 * t23 *
     # t470 - t473 + 0.180D3 * t48 * 0.3141592653589793D1 * t477) * t55 
     #/ 0.1440D4 + t496 + t515
      t517 = FJET(XB1, XB2, s, 0.0D0, t426, 0.0D0, -t428, 0.0D0, t516)
      t519 = t2 * t305
      t520 = t2 * x3
      t521 = x3 * t305
      t524 = log(-0.4D1 * t126 * t521)
      t527 = t524 ** 2
      t530 = -t305
      t531 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t530, x4)
      t533 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t530, x4)
      t541 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t530, x4)
      t545 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t530, x4)
      t547 = (-0.180D3 * t524 * lh - 0.45D2 * t527 - t78 + t80) * t531 -
     # 0.90D2 * t533 + (0.90D2 * t527 * lh - t107 + 0.2884936567583026D3
     # + t109 + 0.15D2 * t527 * t524 - t524 * t81) * t541 + (t117 + 0.90
     #D2 * t524) * t545
      t552 = t34 * x3 * t305
      t555 = log(-0.4D1 * t31 * t552)
      t557 = t555 ** 2
      t568 = 0.3141592653589793D1 * t541
      t569 = t82 * t568
      t573 = (-0.90D2 * t23 * (-t555 * t531 + t557 * t541 / 0.2D1 + t545
     #) + 0.180D3 * t48 * 0.3141592653589793D1 * (t531 - t555 * t541) + 
     #t569) * t53 * t56 / 0.720D3
      t576 = log(-0.4D1 * t60 * t552)
      t586 = (-0.90D2 * t23 * (-t531 + t576 * t541) - 0.180D3 * t48 * t5
     #68) * t53 * t57 / 0.720D3
      t589 = log(-0.4D1 * t271 * t552)
      t591 = t589 ** 2
      t605 = (-0.90D2 * t23 * (-t589 * t531 + t591 * t541 / 0.2D1 + t545
     #) + 0.180D3 * t48 * 0.3141592653589793D1 * (t531 - t589 * t541) + 
     #t569) * t53 * t55 / 0.1440D4
      t606 = -t23 * t547 * t53 / 0.1440D4 - t573 + t586 - t605
      t607 = FJET(XB1, XB2, s, -t519, 0.0D0, t520, 0.0D0, 0.0D0, t606)
      t610 = Sqrt(-t26 * t305)
      t611 = t315 * t610
      t612 = 0.2D1 * t611
      t616 = t2 * t10 * (-t26 - 0.1D1 + x3 + t612) * t309
      t618 = 0.2D1 * t611 * x2
      t621 = t2 * (-x2 - x3 + t338 - t333 - t612 + t618) * t309
      t624 = t454 * t305 * t394
      t627 = log(-0.4D1 * t26 * t30 * t624)
      t628 = 0.1D1 - x2 + t347
      t629 = t627 * t628
      t630 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t38
     #7, x4)
      t632 = t627 ** 2
      t634 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t38
     #7, x4)
      t637 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t38
     #7, x4)
      t643 = 0.1D1 / (-t376 + t333 + x2 - t346 - t618 + 0.2D1 * t611 * t
     #347 - t347 + t377 + t612 - 0.1D1)
      t647 = t628 * t630
      t656 = t628 * t634 * t643
      t664 = log(-0.4D1 * t32 * t624)
      t677 = -(0.90D2 * t23 * (t629 * t630 - t632 * t628 * t634 / 0.2D1 
     #- t628 * t637) * t643 - 0.180D3 * t48 * 0.3141592653589793D1 * (-t
     #647 + t629 * t634) * t643 + t82 * 0.3141592653589793D1 * t656) * t
     #53 * t55 / 0.1440D4 + (-0.90D2 * t23 * (-t647 + t664 * t628 * t634
     #) * t643 - 0.180D3 * t410 * t656) * t53 * t57 / 0.720D3
      t678 = FJET(XB1, XB2, s, 0.0D0, -t616, 0.0D0, t621, 0.0D0, t677)
      t680 = FJET(XB1, XB2, s, -t616, 0.0D0, t621, 0.0D0, 0.0D0, t677)
      t682 = FJET(XB1, XB2, s, t329, -t344, t311, t326, -t20, t418)
      t690 = -t23 * t547 * t53 / 0.1440D4 - t573 + t586 - t605
      t691 = FJET(XB1, XB2, s, 0.0D0, t520, 0.0D0, -t519, 0.0D0, t690)
      t693 = t306 * t307
      t694 = t306 * t12
      t695 = t2 * t313
      t697 = t2 * t3 * x3
      t698 = t7 * t35
      t702 = log(-0.4D1 * t197 * t521 * t698)
      t703 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, t53
     #0, x4)
      t705 = t702 ** 2
      t706 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, t53
     #0, x4)
      t709 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, t53
     #0, x4)
      t710 = -t702 * t703 + t705 * t706 / 0.2D1 + t709
      t714 = t703 - t702 * t706
      t718 = 0.3141592653589793D1 * t706
      t719 = t82 * t718
      t728 = log(-0.4D1 * t26 * t305 * t7 * t126 * t396)
      t737 = (-0.90D2 * t23 * (t703 - t728 * t706) + 0.180D3 * t48 * t71
     #8) * t53 * t57
      t739 = -(0.90D2 * t23 * t710 - 0.180D3 * t48 * 0.3141592653589793D
     #1 * t714 - t719) * t53 * t56 / 0.720D3 + t737 / 0.720D3
      t740 = FJET(XB1, XB2, s, -t693, t694, t695, -t697, 0.0D0, t739)
      t744 = t34 * t7 * t35
      t747 = log(0.4D1 * t176 * t30 * t744)
      t748 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.1
     #0D1, x4)
      t750 = t747 ** 2
      t751 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.1
     #0D1, x4)
      t754 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.1
     #0D1, x4)
      t763 = 0.3141592653589793D1 * t751
      t764 = t82 * t763
      t767 = (-0.90D2 * t23 * (-t747 * t748 + t750 * t751 / 0.2D1 + t754
     #) + 0.180D3 * t48 * 0.3141592653589793D1 * (t748 - t747 * t751) + 
     #t764) * t53 * t56
      t770 = log(0.4D1 * t31 * t744)
      t773 = t770 ** 2
      t777 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.1
     #0D1, x4)
      t789 = -(-0.180D3 * t770 * lh - 0.45D2 * t773 - t78 + t80) * t748 
     #+ 0.90D2 * t777 - (0.90D2 * t773 * lh - t107 + 0.2884936567583026D
     #3 + t109 + 0.15D2 * t773 * t770 - t770 * t81) * t751 - (t117 + 0.9
     #0D2 * t770) * t754
      t795 = log(0.4D1 * t354 * t126 * t698)
      t804 = (-0.90D2 * t23 * (-t748 + t795 * t751) - 0.180D3 * t48 * t7
     #63) * t53 * t57
      t807 = log(0.4D1 * t60 * t744)
      t809 = t807 ** 2
      t822 = (-0.90D2 * t23 * (t807 * t748 - t809 * t751 / 0.2D1 - t754)
     # + 0.180D3 * t48 * 0.3141592653589793D1 * (-t748 + t807 * t751) - 
     #t764) * t55 * t56
      t824 = -t767 / 0.720D3 + t23 * t789 * t56 / 0.720D3 + t804 / 0.720
     #D3 + t822 / 0.720D3
      t825 = FJET(XB1, XB2, s, -t312, t14, 0.0D0, 0.0D0, 0.0D0, t824)
      t827 = FJET(XB1, XB2, s, 0.0D0, t621, 0.0D0, -t616, 0.0D0, t677)
      t829 = FJET(XB1, XB2, s, -t344, t329, t326, t311, -t20, t418)
      t833 = FJET(XB1, XB2, s, 0.0D0, -t428, 0.0D0, t426, 0.0D0, t516)
      t835 = FJET(XB1, XB2, s, -t697, t695, t694, -t693, 0.0D0, t739)
      t837 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t302)
      t839 = t89 * t88 + t303 * t302 + t419 * t415 * t422 / 0.720D3 + t5
     #17 * t516 + t607 * t606 + t678 * t677 + t680 * t677 + t682 * t415 
     #* t422 / 0.720D3 + t691 * t690 + t740 * t739 + t825 * t824 + t827 
     #* t677 + t829 * t415 * t422 / 0.720D3 + t833 * t516 + t835 * t739 
     #+ t837 * t302
      t844 = -t767 / 0.720D3 + t23 * t789 * t56 / 0.720D3 + t804 / 0.720
     #D3 + t822 / 0.720D3
      t845 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t312, t14, 0.0D0, t844)
      t847 = FJET(XB1, XB2, s, 0.0D0, -t519, 0.0D0, t520, 0.0D0, t690)
      t862 = -t453 + (t82 * 0.3141592653589793D1 * t459 - 0.90D2 * t23 *
     # t470 - t473 + 0.180D3 * t48 * 0.3141592653589793D1 * t477) * t55 
     #/ 0.1440D4 + t496 + t515
      t863 = FJET(XB1, XB2, s, -t428, 0.0D0, t426, 0.0D0, 0.0D0, t862)
      t876 = -(0.90D2 * t23 * t710 - 0.180D3 * t48 * 0.3141592653589793D
     #1 * t714 - t719) * t53 * t56 / 0.720D3 + t737 / 0.720D3
      t877 = FJET(XB1, XB2, s, t695, -t697, -t693, t694, 0.0D0, t876)
      t879 = FJET(XB1, XB2, s, t14, -t312, 0.0D0, 0.0D0, 0.0D0, t824)
      t881 = FJET(XB1, XB2, s, 0.0D0, -t9, t14, t13, -t20, t88)
      t883 = FJET(XB1, XB2, s, t621, 0.0D0, -t616, 0.0D0, 0.0D0, t677)
      t885 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t14, -t312, 0.0D0, t844)
      t887 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t302)
      t889 = FJET(XB1, XB2, s, t326, t311, -t344, t329, -t20, t418)
      t893 = FJET(XB1, XB2, s, t520, 0.0D0, -t519, 0.0D0, 0.0D0, t606)
      t895 = FJET(XB1, XB2, s, t426, 0.0D0, -t428, 0.0D0, 0.0D0, t862)
      t897 = FJET(XB1, XB2, s, t14, t13, 0.0D0, -t9, -t20, t88)
      t899 = FJET(XB1, XB2, s, t13, t14, -t9, 0.0D0, -t20, t88)
      t901 = FJET(XB1, XB2, s, t694, -t693, -t697, t695, 0.0D0, t876)
      t903 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t302)
      t905 = t845 * t844 + t847 * t690 + t863 * t862 + t877 * t876 + t87
     #9 * t824 + t881 * t88 + t883 * t677 + t885 * t844 + t887 * t302 + 
     #t889 * t415 * t422 / 0.720D3 + t893 * t606 + t895 * t862 + t897 * 
     #t88 + t899 * t88 + t901 * t876 + t903 * t302
      rrgg2qqbarht7s2e1 = t839 + t905

      end function



      doubleprecision function rrgg2qqbarht7s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = 0.180D3 * lh
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * x3 * t9 * t12)
      t18 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.10D1, x4)
      t22 = t15 ** 2
      t24 = lh ** 2
      t25 = 0.180D3 * t24
      t26 = 0.3141592653589793D1 ** 2
      t27 = 0.30D2 * t26
      t29 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.10D1, x4)
      t31 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.10D1, x4)
      t32 = 0.90D2 * t31
      t34 = 0.1D1 / x3
      t38 = t12 * t9
      t40 = log(0.4D1 * t38)
      t43 = t40 ** 2
      t50 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.10D1, x4)
      t61 = t25 - t27
      t65 = 0.3141592653589793D1 * t29
      t74 = x2 * x3
      t77 = log(0.4D1 * t74 * t38)
      t82 = lh * t4
      t84 = 0.180D3 * t82 * t65
      t87 = 0.1D1 / x2
      t90 = x2 * t9
      t93 = log(0.4D1 * t90 * t12)
      t95 = t93 ** 2
      t107 = -t61 * t4
      t112 = x1 ** 2
      t113 = x3 * t112
      t116 = log(0.4D1 * t113 * t38)
      t123 = 0.1D1 / x1
      t128 = t34 * t87 * t123
      t131 = x2 * t112
      t134 = log(0.4D1 * t131 * t38)
      t143 = t112 * t9
      t144 = t143 * t12
      t146 = log(0.4D1 * t144)
      t152 = t146 ** 2
      t160 = t5 * (-(-t6 - 0.90D2 * t15) * t18 - (0.180D3 * t15 * lh + 0
     #.45D2 * t22 + t25 - t27) * t29 - t32) * t34 / 0.1440D4 - (0.180D3 
     #* t40 * lh + 0.45D2 * t43 + t25 - t27) * t4 * 0.3141592653589793D1
     # * t18 / 0.1440D4 - t5 * t50 / 0.16D2 - (-0.90D2 * t43 * lh + 0.60
     #D2 * lh * t26 - 0.2884936567583026D3 - 0.120D3 * t24 * lh - 0.15D2
     # * t43 * t40 - t40 * t61) * t4 * t65 / 0.1440D4 - (-t6 - 0.90D2 * 
     #t40) * t4 * 0.3141592653589793D1 * t31 / 0.1440D4 - (-0.90D2 * t5 
     #* (-t18 + t77 * t29) - t84) * t34 * t87 / 0.1440D4 + (-0.90D2 * t5
     # * (-t93 * t18 + t95 * t29 / 0.2D1 + t31) + 0.180D3 * t82 * 0.3141
     #592653589793D1 * (t18 - t93 * t29) + t107 * t65) * t87 / 0.1440D4 
     #- (-0.90D2 * t5 * (-t18 + t116 * t29) - t84) * t34 * t123 / 0.720D
     #3 - t5 * t29 * t128 / 0.8D1 + (-0.90D2 * t5 * (t18 - t134 * t29) +
     # t84) * t87 * t123 / 0.720D3 - t5 * ((-t6 - 0.90D2 * t146) * t18 +
     # (0.180D3 * t146 * lh + 0.45D2 * t152 + t25 - t27) * t29 + t32) * 
     #t123 / 0.720D3
      t161 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t160)
      t163 = -0.1D1 + x3
      t164 = t163 * s
      t165 = t1 * x1
      t166 = t164 * t165
      t167 = -0.1D1 + x1
      t168 = t1 * t167
      t169 = t164 * t168
      t170 = x3 * x1
      t171 = t2 * t170
      t173 = t2 * t167 * x3
      t174 = -t167
      t175 = -t163
      t176 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t174, 0.0D0, t1
     #75, x4)
      t177 = x3 * t163
      t178 = x1 * z
      t179 = 0.1D1 - x1 + t178
      t180 = 0.1D1 / t179
      t181 = t167 ** 2
      t186 = log(-0.4D1 * t144 * t177 * t180 * t181)
      t187 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t174, 0.0D0, t1
     #75, x4)
      t189 = t176 - t186 * t187
      t194 = 0.180D3 * t82 * 0.3141592653589793D1 * t187
      t201 = t5 * t187 * t128 / 0.8D1
      t202 = -(0.90D2 * t5 * t189 - t194) * t34 * t123 / 0.720D3 - t201
      t203 = FJET(XB1, XB2, s, -t166, t169, t171, -t173, 0.0D0, t202)
      t212 = -(0.90D2 * t5 * t189 - t194) * t34 * t123 / 0.720D3 - t201
      t213 = FJET(XB1, XB2, s, t171, -t173, -t166, t169, 0.0D0, t212)
      t215 = t2 * t167
      t216 = t2 * x1
      t217 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t174, 0.0D0, 0.
     #10D1, x4)
      t219 = t12 * t180
      t220 = t219 * t181
      t223 = log(0.4D1 * t113 * t9 * t220)
      t224 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t174, 0.0D0, 0.
     #10D1, x4)
      t231 = 0.180D3 * t82 * 0.3141592653589793D1 * t224
      t235 = (-0.90D2 * t5 * (t217 - t223 * t224) + t231) * t34 * t123 /
     # 0.720D3
      t238 = t5 * t224 * t128 / 0.8D1
      t239 = t131 * t9
      t242 = log(0.4D1 * t239 * t220)
      t250 = (-0.90D2 * t5 * (-t217 + t242 * t224) - t231) * t87 * t123 
     #/ 0.720D3
      t253 = log(0.4D1 * t143 * t220)
      t259 = t253 ** 2
      t263 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, t174, 0.0D0, 0.
     #10D1, x4)
      t265 = (t6 + 0.90D2 * t253) * t217 + (-0.180D3 * t253 * lh - 0.45D
     #2 * t259 - t25 + t27) * t224 - 0.90D2 * t263
      t269 = -t235 + t238 + t250 - t5 * t265 * t123 / 0.720D3
      t270 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t215, t216, 0.0D0, t269)
      t272 = t2 * x3
      t273 = t2 * t163
      t274 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t175, x4)
      t276 = t12 * x3 * t163
      t279 = log(-0.4D1 * t90 * t276)
      t280 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t175, x4)
      t287 = 0.180D3 * t82 * 0.3141592653589793D1 * t280
      t291 = (-0.90D2 * t5 * (t274 - t279 * t280) + t287) * t34 * t87 / 
     #0.1440D4
      t294 = log(-0.4D1 * t38 * t177)
      t300 = t294 ** 2
      t304 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t175, x4)
      t306 = -(t6 + 0.90D2 * t294) * t274 - (-0.180D3 * t294 * lh - 0.45
     #D2 * t300 - t25 + t27) * t280 + 0.90D2 * t304
      t312 = log(-0.4D1 * t143 * t276)
      t320 = (-0.90D2 * t5 * (t274 - t312 * t280) + t287) * t34 * t123 /
     # 0.720D3
      t323 = t5 * t280 * t128 / 0.8D1
      t324 = -t291 + t5 * t306 * t34 / 0.1440D4 - t320 + t323
      t325 = FJET(XB1, XB2, s, 0.0D0, t272, 0.0D0, -t273, 0.0D0, t324)
      t331 = -t291 + t5 * t306 * t34 / 0.1440D4 - t320 + t323
      t332 = FJET(XB1, XB2, s, t272, 0.0D0, -t273, 0.0D0, 0.0D0, t331)
      t334 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t160)
      t336 = 0.3D1 * t74
      t337 = x2 ** 2
      t338 = t337 * x3
      t339 = cos(t7)
      t341 = Sqrt(-t74 * t163)
      t342 = t339 * t341
      t343 = 0.2D1 * t342
      t345 = 0.2D1 * t342 * x2
      t347 = t74 - 0.1D1
      t348 = 0.1D1 / t347
      t350 = t2 * (-x2 - x3 + t336 - t338 - t343 + t345) * t348
      t351 = -0.1D1 + x2
      t355 = t2 * t351 * (-t74 - 0.1D1 + x3 + t343) * t348
      t356 = x2 * z
      t357 = 0.1D1 - x2 + t356
      t358 = t163 * t348
      t359 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t35
     #8, x4)
      t360 = t357 * t359
      t362 = t338 * z
      t363 = 0.2D1 * t74
      t366 = t74 * z
      t368 = 0.1D1 / (-t362 + t338 + x2 - t363 - t345 + 0.2D1 * t342 * t
     #356 - t356 + t366 + t343 - 0.1D1)
      t374 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t35
     #8, x4)
      t377 = t351 ** 2
      t378 = t12 * t377
      t379 = t347 ** 2
      t385 = log(-0.4D1 * t74 * t9 * t378 * t163 / t379)
      t400 = t5 * t360 * t368 * t34 * t87 * t123 / 0.8D1 - (0.90D2 * t5 
     #* (-t357 * t374 + t385 * t357 * t359) * t368 + 0.180D3 * t82 * 0.3
     #141592653589793D1 * t360 * t368) * t34 * t87 / 0.1440D4
      t401 = FJET(XB1, XB2, s, t350, 0.0D0, -t355, 0.0D0, 0.0D0, t400)
      t403 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t160)
      t406 = t351 * s * t168
      t409 = t2 * t167 * x2 * t180
      t410 = t1 ** 2
      t415 = s * t410 * x2 * x1 * t167 * t180
      t416 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t174, x2, 0.10D
     #1, x4)
      t420 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t174, x2, 0.10D
     #1, x4)
      t425 = log(0.4D1 * t239 * t219 * t181 * t377)
      t437 = -t5 * t416 * t128 / 0.8D1 + (-0.90D2 * t5 * (t420 - t425 * 
     #t416) + 0.180D3 * t82 * 0.3141592653589793D1 * t416) * t87 * t123 
     #/ 0.720D3
      t438 = FJET(XB1, XB2, s, t406, t216, -t409, 0.0D0, -t415, t437)
      t440 = FJET(XB1, XB2, s, 0.0D0, -t409, t216, t406, -t415, t437)
      t442 = FJET(XB1, XB2, s, t169, -t166, -t173, t171, 0.0D0, t212)
      t444 = FJET(XB1, XB2, s, -t173, t171, t169, -t166, 0.0D0, t202)
      t447 = t351 * t1 * s
      t449 = x2 * t1 * s
      t450 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t451 = t38 * t377
      t454 = log(0.4D1 * t74 * t451)
      t455 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t460 = 0.3141592653589793D1 * t455
      t462 = 0.180D3 * t82 * t460
      t466 = (-0.90D2 * t5 * (t450 - t454 * t455) + t462) * t34 * t87 / 
     #0.1440D4
      t469 = log(0.4D1 * t90 * t378)
      t471 = t469 ** 2
      t474 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t475 = -t469 * t450 + t471 * t455 / 0.2D1 + t474
      t479 = t450 - t469 * t455
      t483 = t107 * t460
      t489 = t5 * t455 * t128 / 0.8D1
      t492 = log(0.4D1 * t131 * t451)
      t500 = (-0.90D2 * t5 * (-t450 + t492 * t455) - t462) * t87 * t123 
     #/ 0.720D3
      t501 = -t466 + (0.90D2 * t5 * t475 - 0.180D3 * t82 * 0.31415926535
     #89793D1 * t479 - t483) * t87 / 0.1440D4 + t489 + t500
      t502 = FJET(XB1, XB2, s, -t447, 0.0D0, t449, 0.0D0, 0.0D0, t501)
      t504 = FJET(XB1, XB2, s, -t409, 0.0D0, t406, t216, -t415, t437)
      t506 = FJET(XB1, XB2, s, -t273, 0.0D0, t272, 0.0D0, 0.0D0, t331)
      t508 = t161 * t160 + t203 * t202 + t213 * t212 + t270 * t269 + t32
     #5 * t324 + t332 * t331 + t334 * t160 + t401 * t400 + t403 * t160 +
     # t438 * t437 + t440 * t437 + t442 * t212 + t444 * t202 + t502 * t5
     #01 + t504 * t437 + t506 * t331
      t509 = FJET(XB1, XB2, s, t216, t406, 0.0D0, -t409, -t415, t437)
      t515 = -t235 + t238 + t250 - t5 * t265 * t123 / 0.720D3
      t516 = FJET(XB1, XB2, s, -t215, t216, 0.0D0, 0.0D0, 0.0D0, t515)
      t519 = t164 * t165 * t348
      t520 = t170 * z
      t524 = Sqrt(-x3 * t179 * x2 * t163)
      t525 = t339 * t524
      t526 = 0.2D1 * t525
      t531 = t215 * t351 * (-t74 - 0.1D1 + x3 + x1 - t170 - t178 + t520 
     #+ t526) * t180 * t348
      t534 = t216 * x3 * t351 * t348
      t535 = t74 * t178
      t537 = t338 * t178
      t538 = t74 * x1
      t540 = t338 * x1
      t542 = 0.2D1 * t525 * x2
      t543 = 0.2D1 * t535 - t537 - t520 - 0.2D1 * t538 + t540 - x2 - x3 
     #- t526 + t170 - t338 + t336 + t542
      t546 = t215 * t543 * t180 * t348
      t551 = x2 * x1
      t563 = 0.1D1 + 0.2D1 * t178 - x2 - 0.2D1 * x1 + t363 - t338 + t356
     # + 0.4D1 * t535 - t537 - x3 * t11 * t551 - 0.2D1 * t113 * t356 + t
     #113 * t11 * x2 - 0.2D1 * t525 * t551 - 0.2D1 * t525 * t178 - 0.2D1
     # * t525 * t356 - t526
      t569 = t551 * z
      t580 = -0.2D1 * t112 * z + t112 * t11 - t131 + 0.2D1 * t551 + t362
     # - t366 - 0.3D1 * t538 + t540 + t542 - 0.3D1 * t569 + t113 * x2 + 
     #t551 * t11 + 0.2D1 * t131 * z - t131 * t11 + 0.2D1 * t525 * x1 + 0
     #.2D1 * t525 * t569 + t112
      t582 = 0.1D1 / (t563 + t580)
      t583 = -0.1D1 + x1 - t551 - t178 - t356 + x2 + t569
      t586 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t174, x2, t358,
     # x4)
      t588 = t586 * t179 * t128
      t590 = t5 * t582 * t583 * t588 / 0.8D1
      t591 = FJET(XB1, XB2, s, t519, t531, t534, -t546, -t415, -t590)
      t594 = 0.3141592653589793D1 * t582 * t583
      t598 = FJET(XB1, XB2, s, t534, -t546, t519, t531, -t415, -t590)
      t603 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t216, -t215, 0.0D0, t269)
      t605 = FJET(XB1, XB2, s, 0.0D0, t350, 0.0D0, -t355, 0.0D0, t400)
      t607 = FJET(XB1, XB2, s, t216, -t215, 0.0D0, 0.0D0, 0.0D0, t515)
      t619 = -t466 + (0.90D2 * t5 * t475 - 0.180D3 * t82 * 0.31415926535
     #89793D1 * t479 - t483) * t87 / 0.1440D4 + t489 + t500
      t620 = FJET(XB1, XB2, s, 0.0D0, t449, 0.0D0, -t447, 0.0D0, t619)
      t622 = FJET(XB1, XB2, s, 0.0D0, -t273, 0.0D0, t272, 0.0D0, t324)
      t624 = FJET(XB1, XB2, s, 0.0D0, -t447, 0.0D0, t449, 0.0D0, t619)
      t626 = FJET(XB1, XB2, s, t449, 0.0D0, -t447, 0.0D0, 0.0D0, t501)
      t628 = FJET(XB1, XB2, s, 0.0D0, -t355, 0.0D0, t350, 0.0D0, t400)
      t630 = FJET(XB1, XB2, s, -t355, 0.0D0, t350, 0.0D0, 0.0D0, t400)
      t632 = FJET(XB1, XB2, s, -t546, t534, t531, t519, -t415, -t590)
      t637 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t160)
      t639 = FJET(XB1, XB2, s, t531, t519, -t546, t534, -t415, -t590)
      t644 = t509 * t437 + t516 * t515 - t591 * t4 * t594 * t588 / 0.8D1
     # - t598 * t4 * t594 * t588 / 0.8D1 + t603 * t269 + t605 * t400 + t
     #607 * t515 + t620 * t619 + t622 * t324 + t624 * t619 + t626 * t501
     # + t628 * t400 + t630 * t400 - t632 * t4 * t594 * t588 / 0.8D1 + t
     #637 * t160 - t639 * t4 * t594 * t588 / 0.8D1
      rrgg2qqbarht7s2e0 = t508 + t644

      end function



      doubleprecision function rrgg2qqbarht7s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t3 * s
      t11 = t9 * x1 * x3
      t13 = t9 * t6 * x3
      t14 = s ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * 0.3141592653589793D1
      t17 = -t6
      t18 = -t1
      t19 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t17, 0.0D0, t18,
     # x4)
      t20 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t23 = t19 * t20 * t22
      t25 = t16 * t23 / 0.8D1
      t26 = FJET(XB1, XB2, s, -t5, t8, t11, -t13, 0.0D0, -t25)
      t31 = t9 * x1
      t32 = -0.1D1 + x2
      t34 = t32 * s * t7
      t38 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t40 = t9 * t6 * x2 * t38
      t41 = t3 ** 2
      t46 = s * t41 * x2 * x1 * t6 * t38
      t47 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t17, x2, 0.10D1,
     # x4)
      t48 = 0.1D1 / x2
      t50 = t47 * t48 * t22
      t52 = t16 * t50 / 0.8D1
      t53 = FJET(XB1, XB2, s, t31, t34, 0.0D0, -t40, -t46, -t52)
      t58 = FJET(XB1, XB2, s, t8, -t5, -t13, t11, 0.0D0, -t25)
      t63 = FJET(XB1, XB2, s, -t40, 0.0D0, t34, t31, -t46, -t52)
      t69 = x2 * t3 * s
      t71 = t32 * t3 * s
      t72 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t76 = t16 * t72 * t48 * t22 / 0.8D1
      t80 = t16 * t72 * t20 * t48 / 0.16D2
      t81 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t82 = x4 * 0.3141592653589793D1
      t83 = Sin(t82)
      t84 = t83 ** 2
      t85 = x2 * t84
      t86 = z ** 2
      t87 = 0.1D1 / t86
      t88 = t32 ** 2
      t92 = log(0.4D1 * t85 * t87 * t88)
      t94 = -t81 + t92 * t72
      t97 = t15 * lh
      t100 = 0.180D3 * t97 * 0.3141592653589793D1 * t72
      t104 = t76 + t80 + (-0.90D2 * t16 * t94 - t100) * t48 / 0.1440D4
      t105 = FJET(XB1, XB2, s, 0.0D0, t69, 0.0D0, -t71, 0.0D0, t104)
      t107 = FJET(XB1, XB2, s, -t13, t11, t8, -t5, 0.0D0, -t25)
      t112 = t9 * x3
      t113 = t9 * t1
      t114 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t18, x4)
      t115 = t114 * t20
      t118 = t16 * t115 * t48 / 0.16D2
      t119 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t18, x4)
      t121 = 0.180D3 * lh
      t122 = t87 * t84
      t126 = log(-0.4D1 * t122 * x3 * t1)
      t130 = 0.90D2 * t119 - (t121 + 0.90D2 * t126) * t114
      t136 = t16 * t115 * t22 / 0.8D1
      t137 = t118 + t16 * t130 * t20 / 0.1440D4 + t136
      t138 = FJET(XB1, XB2, s, 0.0D0, t112, 0.0D0, -t113, 0.0D0, t137)
      t144 = t118 + t16 * t130 * t20 / 0.1440D4 + t136
      t145 = FJET(XB1, XB2, s, -t113, 0.0D0, t112, 0.0D0, 0.0D0, t144)
      t147 = FJET(XB1, XB2, s, 0.0D0, -t71, 0.0D0, t69, 0.0D0, t104)
      t149 = t9 * t6
      t150 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, t17, 0.0D0, 0.1
     #0D1, x4)
      t154 = t16 * t150 * t48 * t22 / 0.8D1
      t155 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, t17, 0.0D0, 0.1
     #0D1, x4)
      t157 = x1 ** 2
      t158 = t157 * t84
      t160 = t6 ** 2
      t164 = log(0.4D1 * t158 * t87 * t38 * t160)
      t168 = 0.90D2 * t155 - (t121 + 0.90D2 * t164) * t150
      t175 = t16 * t150 * t20 * t22 / 0.8D1
      t176 = t154 + t16 * t168 * t22 / 0.720D3 + t175
      t177 = FJET(XB1, XB2, s, t31, -t149, 0.0D0, 0.0D0, 0.0D0, t176)
      t185 = t76 + t80 + (-0.90D2 * t16 * t94 - t100) * t48 / 0.1440D4
      t186 = FJET(XB1, XB2, s, -t71, 0.0D0, t69, 0.0D0, 0.0D0, t185)
      t188 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t189 = 0.90D2 * t188
      t193 = log(0.4D1 * x3 * t84 * t87)
      t196 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t203 = log(0.4D1 * t122)
      t212 = t203 ** 2
      t214 = lh ** 2
      t216 = 0.3141592653589793D1 ** 2
      t220 = 0.3141592653589793D1 * t196
      t223 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t226 = t196 * t20
      t232 = log(0.4D1 * t85 * t87)
      t248 = log(0.4D1 * t158 * t87)
      t259 = t16 * (-t189 - (-t121 - 0.90D2 * t193) * t196) * t20 / 0.14
     #40D4 - (-t121 - 0.90D2 * t203) * t15 * 0.3141592653589793D1 * t188
     # / 0.1440D4 - (0.180D3 * t203 * lh + 0.45D2 * t212 + 0.180D3 * t21
     #4 - 0.30D2 * t216) * t15 * t220 / 0.1440D4 - t16 * t223 / 0.16D2 -
     # t16 * t226 * t48 / 0.16D2 + (-0.90D2 * t16 * (t188 - t232 * t196)
     # + 0.180D3 * t97 * t220) * t48 / 0.1440D4 - t16 * t196 * t48 * t22
     # / 0.8D1 - t16 * (t189 + (-t121 - 0.90D2 * t248) * t196) * t22 / 0
     #.720D3 - t16 * t226 * t22 / 0.8D1
      t260 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t9, 0.0D0, t259)
      t262 = FJET(XB1, XB2, s, 0.0D0, t9, 0.0D0, 0.0D0, 0.0D0, t259)
      t264 = FJET(XB1, XB2, s, 0.0D0, -t113, 0.0D0, t112, 0.0D0, t137)
      t266 = -t26 * t15 * 0.3141592653589793D1 * t23 / 0.8D1 - t53 * t15
     # * 0.3141592653589793D1 * t50 / 0.8D1 - t58 * t15 * 0.314159265358
     #9793D1 * t23 / 0.8D1 - t63 * t15 * 0.3141592653589793D1 * t50 / 0.
     #8D1 + t105 * t104 - t107 * t15 * 0.3141592653589793D1 * t23 / 0.8D
     #1 + t138 * t137 + t145 * t144 + t147 * t104 + t177 * t176 + t186 *
     # t185 + t260 * t259 + t262 * t259 + t264 * t137
      t267 = FJET(XB1, XB2, s, t112, 0.0D0, -t113, 0.0D0, 0.0D0, t144)
      t269 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t9, 0.0D0, 0.0D0, t259)
      t271 = x2 * x3
      t272 = cos(t82)
      t274 = Sqrt(-t271 * t1)
      t275 = t272 * t274
      t276 = 0.2D1 * t275
      t280 = 0.1D1 / (t271 - 0.1D1)
      t282 = t9 * t32 * (-t271 - 0.1D1 + x3 + t276) * t280
      t284 = x2 ** 2
      t285 = t284 * x3
      t287 = 0.2D1 * t275 * x2
      t290 = t9 * (-x2 - x3 + 0.3D1 * t271 - t285 - t276 + t287) * t280
      t291 = x2 * z
      t292 = 0.1D1 - x2 + t291
      t295 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1 
     #* t280, x4)
      t305 = t295 / (-t285 * z + t285 + x2 - 0.2D1 * t271 - t287 + 0.2D1
     # * t275 * t291 - t291 + t271 * z + t276 - 0.1D1) * t20 * t48
      t307 = t16 * t292 * t305 / 0.16D2
      t308 = FJET(XB1, XB2, s, 0.0D0, -t282, 0.0D0, t290, 0.0D0, t307)
      t310 = 0.3141592653589793D1 * t292
      t314 = FJET(XB1, XB2, s, t290, 0.0D0, -t282, 0.0D0, 0.0D0, t307)
      t323 = t154 + t16 * t168 * t22 / 0.720D3 + t175
      t324 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t31, -t149, 0.0D0, t323)
      t326 = FJET(XB1, XB2, s, -t149, t31, 0.0D0, 0.0D0, 0.0D0, t176)
      t328 = FJET(XB1, XB2, s, t69, 0.0D0, -t71, 0.0D0, 0.0D0, t185)
      t330 = FJET(XB1, XB2, s, t9, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t259)
      t332 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t149, t31, 0.0D0, t323)
      t334 = FJET(XB1, XB2, s, -t282, 0.0D0, t290, 0.0D0, 0.0D0, t307)
      t339 = FJET(XB1, XB2, s, 0.0D0, t290, 0.0D0, -t282, 0.0D0, t307)
      t344 = FJET(XB1, XB2, s, t11, -t13, -t5, t8, 0.0D0, -t25)
      t349 = FJET(XB1, XB2, s, t34, t31, -t40, 0.0D0, -t46, -t52)
      t354 = FJET(XB1, XB2, s, 0.0D0, -t40, t31, t34, -t46, -t52)
      t359 = t267 * t144 + t269 * t259 + t308 * t15 * t310 * t305 / 0.16
     #D2 + t314 * t15 * t310 * t305 / 0.16D2 + t324 * t323 + t326 * t176
     # + t328 * t185 + t330 * t259 + t332 * t323 + t334 * t15 * t310 * t
     #305 / 0.16D2 + t339 * t15 * t310 * t305 / 0.16D2 - t344 * t15 * 0.
     #3141592653589793D1 * t23 / 0.8D1 - t349 * t15 * 0.3141592653589793
     #D1 * t50 / 0.8D1 - t354 * t15 * 0.3141592653589793D1 * t50 / 0.8D1
      rrgg2qqbarht7s2em1 = t266 + t359

      end function



      doubleprecision function rrgg2qqbarht7s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.10D1, x4)
      t23 = z ** 2
      t26 = Sin(x4 * 0.3141592653589793D1)
      t27 = t26 ** 2
      t30 = log(0.4D1 / t23 * t27)
      t37 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 - (-0.180D3 * lh - 0.90D2 * t30) *
     # t4 * 0.3141592653589793D1 * t6 / 0.1440D4
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t42 = t2 * x1
      t43 = -0.1D1 + x1
      t44 = t2 * t43
      t46 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, -t43, 0.0D0, 0.1
     #0D1, x4)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t42, -t44, 0.0D0, t49)
      t53 = 0.3141592653589793D1 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t44, t42, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t37)
      t62 = t2 * x3
      t63 = -0.1D1 + x3
      t64 = t2 * t63
      t66 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, -
     #t63, x4)
      t69 = t5 * t66 * t7 / 0.16D2
      t70 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t69)
      t73 = 0.3141592653589793D1 * t66 * t7
      t77 = x2 * t1 * s
      t80 = (-0.1D1 + x2) * t1 * s
      t81 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t84 = t5 * t81 * t11 / 0.16D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t77, 0.0D0, -t80, 0.0D0, t84)
      t88 = 0.3141592653589793D1 * t81 * t11
      t91 = FJET(XB1, XB2, s, 0.0D0, -t80, 0.0D0, t77, 0.0D0, t84)
      t95 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t69)
      t99 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t37)
      t101 = FJET(XB1, XB2, s, t42, -t44, 0.0D0, 0.0D0, 0.0D0, t49)
      t105 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t69)
      t109 = FJET(XB1, XB2, s, t77, 0.0D0, -t80, 0.0D0, 0.0D0, t84)
      t113 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t69)
      t117 = FJET(XB1, XB2, s, -t80, 0.0D0, t77, 0.0D0, 0.0D0, t84)
      t121 = FJET(XB1, XB2, s, -t44, t42, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2qqbarht7s2em2 = t38 * t37 + t40 * t37 + t50 * t4 * t53 / 0.8D
     #1 + t56 * t4 * t53 / 0.8D1 + t60 * t37 + t70 * t4 * t73 / 0.16D2 +
     # t85 * t4 * t88 / 0.16D2 + t91 * t4 * t88 / 0.16D2 + t95 * t4 * t7
     #3 / 0.16D2 + t99 * t37 + t101 * t4 * t53 / 0.8D1 + t105 * t4 * t73
     # / 0.16D2 + t109 * t4 * t88 / 0.16D2 + t113 * t4 * t73 / 0.16D2 + 
     #t117 * t4 * t88 / 0.16D2 + t121 * t4 * t53 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht7s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t8 = t4 * 0.3141592653589793D1 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = 0.3141592653589793D1 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s2em3 = -t9 * t4 * t11 / 0.16D2 - t13 * t4 * t11 / 0.
     #16D2 - t16 * t4 * t11 / 0.16D2 - t19 * t4 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      rrgg2qqbarht7s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht7s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = lh * t4
      t6 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t7 = 0.3141592653589793D1 * t6
      t10 = lh ** 2
      t11 = 0.180D3 * t10
      t12 = 0.3141592653589793D1 ** 2
      t13 = 0.30D2 * t12
      t14 = t11 - t13
      t15 = t14 * t4
      t16 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t17 = 0.3141592653589793D1 * t16
      t19 = t4 * 0.3141592653589793D1
      t20 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t24 = z ** 2
      t26 = 0.1D1 / t24 / z
      t27 = x3 * t26
      t28 = x4 * 0.3141592653589793D1
      t29 = Sin(t28)
      t30 = t29 ** 2
      t33 = log(0.4D1 * t27 * t30)
      t34 = -0.1D1 + x3
      t35 = 0.1D1 / t34
      t39 = log(-0.4D1 * t27 * t30 * t35)
      t40 = cos(t28)
      t41 = x3 * z
      t43 = Sqrt(-t41 * t34)
      t47 = 0.1D1 / (-z - x3 + 0.2D1 * t40 * t43)
      t52 = t39 ** 2
      t56 = t33 ** 2
      t64 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t68 = 0.60D2 * lh * t12
      t70 = 0.120D3 * t10 * lh
      t71 = t68 - 0.2884936567583026D3 - t70
      t72 = t71 * t4
      t73 = t72 * t17
      t74 = 0.3141592653589793D1 * t20
      t92 = 0.1D1 / x3
      t95 = t26 * t30
      t97 = log(0.4D1 * t95)
      t98 = t97 ** 2
      t101 = t98 * t97
      t122 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 
     #0.10D1, x4)
      t131 = t12 ** 2
      t132 = t10 ** 2
      t136 = t98 ** 2
      t142 = x1 ** 2
      t143 = x3 * t142
      t146 = log(0.4D1 * t143 * t95)
      t148 = t146 ** 2
      t151 = t95 * t35
      t154 = log(-0.4D1 * t143 * t151)
      t155 = t154 * t47
      t157 = t154 ** 2
      t161 = t47 * t20
      t168 = t47 * t6
      t177 = t47 * t16 * z
      t183 = 0.1D1 / x1
      t186 = t142 * t30
      t187 = t186 * t26
      t189 = log(0.4D1 * t187)
      t194 = t189 ** 2
      t214 = x2 ** 2
      t215 = x3 * t214
      t216 = t215 * t142
      t219 = log(-0.4D1 * t216 * t151)
      t224 = 0.1D1 - x2
      t225 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, 0.
     #10D1, x4)
      t226 = -t224
      t227 = t95 * t226
      t230 = log(-0.4D1 * t216 * t227)
      t231 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, 0.
     #10D1, x4)
      t235 = log(0.4D1 * t215 * t187)
      t241 = 0.3141592653589793D1 * (-t177 + t231 - t16)
      t246 = 0.1D1 / x2
      t247 = t246 * t183
      t250 = t214 * t142
      t253 = log(0.4D1 * t250 * t95)
      t255 = t253 ** 2
      t260 = log(-0.4D1 * t250 * t227)
      t262 = t260 ** 2
      t265 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, 0.
     #10D1, x4)
      t276 = 0.3141592653589793D1 * (t16 - t231)
      t284 = log(0.4D1 * t215 * t95)
      t286 = t284 ** 2
      t291 = log(-0.4D1 * t215 * t151)
      t292 = t291 * t47
      t294 = t291 ** 2
      t302 = log(-0.4D1 * t215 * t227)
      t304 = t302 ** 2
      t324 = t26 * t214
      t327 = log(0.4D1 * t324 * t30)
      t329 = t30 * t226
      t332 = log(-0.4D1 * t324 * t329)
      t337 = t332 ** 2
      t340 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, 0.
     #10D1, x4)
      t345 = t327 ** 2
      t369 = -((0.180D3 * t5 * t7 - t15 * t17 - 0.90D2 * t19 * t20) * (t
     #33 + t39 * t47 * z) - 0.90D2 * t19 * t16 * (t52 * t39 * t47 * z / 
     #0.6D1 + t56 * t33 / 0.6D1) + (-t15 * t7 - 0.90D2 * t19 * t64 - t73
     # + 0.180D3 * t5 * t74) * (-0.1D1 - t47 * z) + (-0.90D2 * t19 * t6 
     #+ 0.180D3 * t5 * t17) * (-t56 / 0.2D1 - t52 * t47 * z / 0.2D1)) * 
     #t92 / 0.2880D4 - (-0.90D2 * t98 * lh + t68 - 0.2884936567583026D3 
     #- t70 - 0.15D2 * t101 - t97 * t14) * t4 * t7 / 0.2880D4 - (0.180D3
     # * t97 * lh + 0.45D2 * t98 + t11 - t13) * t4 * t74 / 0.2880D4 - (-
     #0.180D3 * lh - 0.90D2 * t97) * t4 * 0.3141592653589793D1 * t64 / 0
     #.2880D4 - t19 * t122 / 0.32D2 - (0.30D2 * t101 * lh + t98 * t14 / 
     #0.2D1 - t97 * t71 + 0.5769873135166051D3 * lh + t131 + 0.60D2 * t1
     #32 - 0.60D2 * t10 * t12 + 0.15D2 / 0.4D1 * t136) * t4 * t17 / 0.28
     #80D4 + (0.90D2 * t19 * (t146 * t6 - t148 * t16 / 0.2D1 - t20 + (t1
     #55 * t6 - t157 * t47 * t16 / 0.2D1 - t161) * z) - 0.180D3 * t5 * 0
     #.3141592653589793D1 * (-t6 + t146 * t16 + (-t168 + t155 * t16) * z
     #) + t15 * 0.3141592653589793D1 * (-t177 - t16)) * t92 * t183 / 0.1
     #440D4 - (t15 * 0.3141592653589793D1 * (t6 - t189 * t16) + 0.90D2 *
     # t19 * (t194 * t6 / 0.2D1 + t64 - t194 * t189 * t16 / 0.6D1 - t189
     # * t20) + t73 - 0.180D3 * t5 * 0.3141592653589793D1 * (-t189 * t6 
     #+ t194 * t16 / 0.2D1 + t20)) * t183 / 0.1440D4 + (0.90D2 * t19 * (
     #(-t168 + t219 * t47 * t16) * z + t225 - t230 * t231 - t6 + t235 * 
     #t16) - 0.180D3 * t5 * t241) * t92 * t247 / 0.720D3 - (0.90D2 * t19
     # * (-t253 * t6 + t255 * t16 / 0.2D1 + t20 + t260 * t225 - t262 * t
     #231 / 0.2D1 - t265) - 0.180D3 * t5 * 0.3141592653589793D1 * (t6 - 
     #t253 * t16 - t225 + t260 * t231) + t15 * t276) * t246 * t183 / 0.7
     #20D3 + (0.90D2 * t19 * (t284 * t6 - t286 * t16 / 0.2D1 - t20 + (t2
     #92 * t6 - t294 * t47 * t16 / 0.2D1 - t161) * z - t302 * t225 + t30
     #4 * t231 / 0.2D1 + t265) - 0.180D3 * t5 * 0.3141592653589793D1 * (
     #-t6 + t284 * t16 + (-t168 + t292 * t16) * z + t225 - t302 * t231) 
     #+ t15 * t241) * t92 * t246 / 0.1440D4 - (t15 * 0.3141592653589793D
     #1 * (t6 - t327 * t16 - t225 + t332 * t231) + 0.90D2 * t19 * (-t337
     # * t225 / 0.2D1 - t340 + t337 * t332 * t231 / 0.6D1 + t332 * t265 
     #+ t345 * t6 / 0.2D1 + t64 - t345 * t327 * t16 / 0.6D1 - t327 * t20
     #) + t72 * t276 - 0.180D3 * t5 * 0.3141592653589793D1 * (-t327 * t6
     # + t345 * t16 / 0.2D1 + t20 + t332 * t225 - t337 * t231 / 0.2D1 - 
     #t265)) * t246 / 0.1440D4
      t370 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t369)
      t372 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t369)
      t374 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t369)
      t376 = x2 * x3
      t377 = 0.1D1 - x3 + t376
      t378 = 0.1D1 / t377
      t379 = t376 * t378
      t380 = t2 * t379
      t381 = t34 * t378
      t382 = t2 * t381
      t383 = t215 * t186
      t385 = t377 ** 2
      t386 = 0.1D1 / t385
      t387 = t34 * t386
      t391 = log(0.4D1 * t383 * t26 * t226 * t387)
      t392 = t226 * t34
      t394 = Sqrt(t41 * t392)
      t398 = 0.1D1 / (-z - x3 + t376 + 0.2D1 * t40 * t394)
      t400 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, -t
     #381, x4)
      t401 = z * t400
      t403 = t398 * z
      t404 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, -t
     #381, x4)
      t405 = t403 * t404
      t409 = t5 * 0.3141592653589793D1
      t410 = t403 * t400
      t417 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t224, -t
     #381, x4)
      t423 = log(0.4D1 * t215 * t26 * t329 * t387)
      t424 = t423 ** 2
      t428 = t423 * t398
      t445 = (0.90D2 * t19 * (-t391 * t398 * t401 + t405) - 0.180D3 * t4
     #09 * t410) * t92 * t247 / 0.720D3 + (0.90D2 * t19 * (t403 * t417 +
     # t424 * t398 * t401 / 0.2D1 - t428 * z * t404) - 0.180D3 * t5 * 0.
     #3141592653589793D1 * (-t428 * t401 + t405) + t15 * 0.3141592653589
     #793D1 * t410) * t92 * t246 / 0.1440D4
      t446 = FJET(XB1, XB2, s, 0.0D0, t380, 0.0D0, -t382, 0.0D0, t445)
      t449 = t1 * x1
      t450 = x1 * z
      t451 = -z - x1 + t450
      t452 = 0.1D1 / t451
      t454 = t226 * s * t449 * t452
      t455 = -0.1D1 + x1
      t456 = t2 * t455
      t458 = x2 * s * t449
      t459 = t1 ** 2
      t460 = s * t459
      t463 = x1 * t455 * t452
      t464 = t460 * t226 * t463
      t465 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, t224, 0.10D
     #1, x4)
      t466 = 0.1D1 / t24
      t467 = t466 * t452
      t468 = t455 ** 2
      t470 = t467 * t468 * t226
      t473 = log(0.4D1 * t383 * t470)
      t474 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, t224, 0.10D
     #1, x4)
      t479 = 0.3141592653589793D1 * t474
      t485 = t250 * t30
      t488 = log(0.4D1 * t485 * t470)
      t490 = t488 ** 2
      t493 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, t224, 0.10D
     #1, x4)
      t507 = (0.90D2 * t19 * (-t465 + t473 * t474) + 0.180D3 * t5 * t479
     #) * t92 * t247 / 0.720D3 - (0.90D2 * t19 * (-t488 * t465 + t490 * 
     #t474 / 0.2D1 + t493) - 0.180D3 * t5 * 0.3141592653589793D1 * (t465
     # - t488 * t474) + t15 * t479) * t246 * t183 / 0.720D3
      t508 = FJET(XB1, XB2, s, 0.0D0, t454, -t456, t458, -t464, t507)
      t511 = t2 * x1 * t452
      t512 = t460 * t463
      t513 = t143 * t30
      t515 = t467 * t468 * t35
      t518 = log(0.4D1 * t513 * t515)
      t519 = t518 * z
      t520 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t523 = t518 ** 2
      t525 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t526 = t451 * t525
      t529 = z * t451
      t530 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t533 = x3 * x1
      t534 = t533 * z
      t535 = t143 * t24
      t536 = x1 * t24
      t537 = x3 * t24
      t538 = t537 * x1
      t540 = 0.2D1 * t143 * z
      t541 = x3 * t451
      t543 = Sqrt(t541 * t34)
      t548 = 0.1D1 / (-t450 - t534 - t143 - t535 - t41 + t536 + t538 + t
     #540 + 0.2D1 * t40 * t543 * z - t24)
      t550 = t467 * t468
      t553 = log(-0.4D1 * t513 * t550)
      t555 = t553 ** 2
      t561 = t529 * t520
      t573 = 0.3141592653589793D1 * (-t529 * t525 * t548 + t525)
      t578 = (0.90D2 * t19 * (-(-t519 * t451 * t520 + t523 * z * t526 / 
     #0.2D1 + t529 * t530) * t548 - t553 * t520 + t555 * t525 / 0.2D1 + 
     #t530) - 0.180D3 * t5 * 0.3141592653589793D1 * (-(t561 - t519 * t52
     #6) * t548 + t520 - t553 * t525) + t15 * t573) * t92 * t183 / 0.144
     #0D4
      t581 = log(-0.4D1 * t186 * t550)
      t583 = -t520 + t581 * t525
      t586 = t581 ** 2
      t589 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t594 = -t586 * t520 / 0.2D1 - t589 + t586 * t581 * t525 / 0.6D1 + 
     #t581 * t530
      t597 = 0.3141592653589793D1 * t525
      t598 = t72 * t597
      t602 = t581 * t520 - t586 * t525 / 0.2D1 - t530
      t611 = log(0.4D1 * t383 * t515)
      t617 = t452 * t468
      t621 = log(-0.4D1 * t216 * t30 * t466 * t617)
      t631 = (0.90D2 * t19 * (-(t561 - t611 * z * t526) * t548 + t520 - 
     #t621 * t525) - 0.180D3 * t5 * t573) * t92 * t247 / 0.720D3
      t634 = log(-0.4D1 * t485 * t550)
      t636 = t634 ** 2
      t639 = t634 * t520 - t636 * t525 / 0.2D1 - t530
      t643 = -t520 + t634 * t525
      t647 = t15 * t597
      t651 = (0.90D2 * t19 * t639 - 0.180D3 * t5 * 0.3141592653589793D1 
     #* t643 - t647) * t246 * t183 / 0.720D3
      t652 = t578 - (t15 * 0.3141592653589793D1 * t583 + 0.90D2 * t19 * 
     #t594 - t598 - 0.180D3 * t5 * 0.3141592653589793D1 * t602) * t183 /
     # 0.1440D4 + t631 - t651
      t653 = FJET(XB1, XB2, s, 0.0D0, -t456, -t511, 0.0D0, t512, t652)
      t655 = FJET(XB1, XB2, s, 0.0D0, -t511, -t456, 0.0D0, t512, t652)
      t657 = FJET(XB1, XB2, s, 0.0D0, -t382, 0.0D0, t380, 0.0D0, t445)
      t659 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t369)
      t661 = FJET(XB1, XB2, s, t458, -t456, t454, 0.0D0, -t464, t507)
      t663 = FJET(XB1, XB2, s, t380, 0.0D0, -t382, 0.0D0, 0.0D0, t445)
      t665 = FJET(XB1, XB2, s, t454, 0.0D0, t458, -t456, -t464, t507)
      t670 = t34 * s * t1 * t455 * t378
      t671 = t2 * x1
      t673 = Sqrt(-t541 * t392)
      t674 = t40 * t673
      t680 = t671 * x2 * (-x3 + t376 - z + t41 - x1 + t533 + t450 - t534
     # + 0.2D1 * t674) * t452 * t378
      t681 = t456 * t379
      t682 = t215 * t450
      t683 = t215 * x1
      t690 = t671 * (-t682 + t683 + t215 * z + 0.2D1 * t674 * x2 + 0.1D1
     # - x3 - x2 + t376) * t452 * t378
      t691 = x2 * x1
      t692 = t691 * z
      t693 = -z + t692 - t691
      t702 = x2 * t142
      t706 = 0.2D1 * t674 * t692 - 0.2D1 * t674 * z - t376 * z + t376 * 
     #x1 - t143 * x2 - t691 * t24 - 0.2D1 * t702 * z + t702 * t24 - t683
     # + t692 - t540 + t534 + t535
      t717 = -t538 + t702 + t24 + t143 + t41 - t536 + t682 - 0.2D1 * t67
     #4 * t691 - 0.2D1 * t376 * t450 + t537 * t691 + 0.2D1 * t143 * x2 *
     # z - t143 * t24 * x2 + t450
      t719 = 0.1D1 / (t706 + t717)
      t720 = t693 * t719
      t721 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, t224, -t381
     #, x4)
      t729 = log(-0.4D1 * t215 * t186 * t466 * t617 * t392 * t386)
      t731 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, t224, -t381
     #, x4)
      t742 = 0.90D2 * t19 * (t720 * t721 - t729 * t693 * t719 * t731) * 
     #t451 - 0.180D3 * t409 * t720 * t731 * t451
      t745 = t742 * t92 * t247 / 0.720D3
      t746 = FJET(XB1, XB2, s, t670, t680, -t681, -t690, -t464, t745)
      t749 = t92 * t246 * t183
      t752 = FJET(XB1, XB2, s, t680, t670, -t690, -t681, -t464, t745)
      t768 = (t15 * 0.3141592653589793D1 * t583 + 0.90D2 * t19 * t594 - 
     #t598 - 0.180D3 * t5 * 0.3141592653589793D1 * t602) * t183 / 0.1440
     #D4
      t769 = t578 - t768 + t631 - t651
      t770 = FJET(XB1, XB2, s, -t456, 0.0D0, 0.0D0, -t511, t512, t769)
      t772 = FJET(XB1, XB2, s, -t456, t458, 0.0D0, t454, -t464, t507)
      t785 = t578 - t768 + t631 - (0.90D2 * t19 * t639 - 0.180D3 * t5 * 
     #0.3141592653589793D1 * t643 - t647) * t246 * t183 / 0.720D3
      t786 = FJET(XB1, XB2, s, -t511, 0.0D0, 0.0D0, -t456, t512, t785)
      t788 = FJET(XB1, XB2, s, -t382, 0.0D0, t380, 0.0D0, 0.0D0, t445)
      t790 = FJET(XB1, XB2, s, -t690, -t681, t680, t670, -t464, t745)
      t794 = FJET(XB1, XB2, s, -t681, -t690, t670, t680, -t464, t745)
      rrgg2qqbarht7s3e1 = t370 * t369 + t372 * t369 + t374 * t369 + t446
     # * t445 + t508 * t507 + t653 * t652 + t655 * t652 + t657 * t445 + 
     #t659 * t369 + t661 * t507 + t663 * t445 + t665 * t507 + t746 * t74
     #2 * t749 / 0.720D3 + t752 * t742 * t749 / 0.720D3 + t770 * t769 + 
     #t772 * t507 + t786 * t785 + t788 * t445 + t790 * t742 * t749 / 0.7
     #20D3 + t794 * t742 * t749 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht7s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = t16 ** 2
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t23 = log(-0.4D1 * t10 * t13 * t19)
      t24 = t23 ** 2
      t25 = cos(t11)
      t26 = x3 * z
      t28 = Sqrt(-t26 * t18)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t28)
      t40 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t43 = lh * t4
      t44 = 0.3141592653589793D1 * t6
      t52 = 0.3141592653589793D1 * t40
      t55 = lh ** 2
      t56 = 0.180D3 * t55
      t57 = 0.3141592653589793D1 ** 2
      t58 = 0.30D2 * t57
      t59 = t56 - t58
      t60 = t59 * t4
      t61 = t60 * t44
      t62 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t70 = 0.1D1 / x3
      t73 = t9 * t13
      t75 = log(0.4D1 * t73)
      t78 = t75 ** 2
      t84 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t107 = x2 ** 2
      t108 = x3 * t107
      t111 = log(0.4D1 * t108 * t73)
      t113 = t32 * t40
      t114 = t73 * t19
      t117 = log(-0.4D1 * t108 * t114)
      t122 = 0.1D1 - x2
      t123 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t122, 0.
     #10D1, x4)
      t124 = -t122
      t125 = t73 * t124
      t128 = log(-0.4D1 * t108 * t125)
      t129 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t122, 0.
     #10D1, x4)
      t135 = t32 * t6 * z
      t136 = -t135 + t129 - t6
      t142 = 0.1D1 / x2
      t145 = t9 * t107
      t148 = log(0.4D1 * t145 * t13)
      t150 = t148 ** 2
      t153 = t13 * t124
      t156 = log(-0.4D1 * t145 * t153)
      t158 = t156 ** 2
      t161 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t122, 0.
     #10D1, x4)
      t172 = 0.3141592653589793D1 * (t6 - t129)
      t177 = x1 ** 2
      t178 = x3 * t177
      t181 = log(0.4D1 * t178 * t73)
      t185 = log(-0.4D1 * t178 * t114)
      t199 = 0.1D1 / x1
      t204 = t70 * t142 * t199
      t207 = t107 * t177
      t210 = log(0.4D1 * t207 * t73)
      t214 = log(-0.4D1 * t207 * t125)
      t225 = t177 * t13
      t228 = log(0.4D1 * t225 * t9)
      t230 = t228 ** 2
      t244 = -(-0.90D2 * t5 * t6 * (-t17 / 0.2D1 - t24 * t32 * z / 0.2D1
     #) + (-0.90D2 * t5 * t40 + 0.180D3 * t43 * t44) * (t16 + t23 * t32 
     #* z) + (0.180D3 * t43 * t52 - t61 - 0.90D2 * t5 * t62) * (-0.1D1 -
     # t32 * z)) * t70 / 0.2880D4 - (0.180D3 * t75 * lh + 0.45D2 * t78 +
     # t56 - t58) * t4 * t52 / 0.2880D4 - t5 * t84 / 0.32D2 - (-0.90D2 *
     # t78 * lh + 0.60D2 * lh * t57 - 0.2884936567583026D3 - 0.120D3 * t
     #55 * lh - 0.15D2 * t78 * t75 - t75 * t59) * t4 * t44 / 0.2880D4 - 
     #(-0.180D3 * lh - 0.90D2 * t75) * t4 * 0.3141592653589793D1 * t62 /
     # 0.2880D4 + (0.90D2 * t5 * (-t40 + t111 * t6 + (-t113 + t117 * t32
     # * t6) * z + t123 - t128 * t129) - 0.180D3 * t43 * 0.3141592653589
     #793D1 * t136) * t70 * t142 / 0.1440D4 - (0.90D2 * t5 * (-t148 * t4
     #0 + t150 * t6 / 0.2D1 + t62 + t156 * t123 - t158 * t129 / 0.2D1 - 
     #t161) - 0.180D3 * t43 * 0.3141592653589793D1 * (t40 - t148 * t6 - 
     #t123 + t156 * t129) + t60 * t172) * t142 / 0.1440D4 + (0.90D2 * t5
     # * (-t40 + t181 * t6 + (-t113 + t185 * t32 * t6) * z) - 0.180D3 * 
     #t43 * 0.3141592653589793D1 * (-t135 - t6)) * t70 * t199 / 0.1440D4
     # + t5 * t136 * t204 / 0.8D1 - (0.90D2 * t5 * (t40 - t210 * t6 - t1
     #23 + t214 * t129) - 0.180D3 * t43 * t172) * t142 * t199 / 0.720D3 
     #- (0.90D2 * t5 * (-t228 * t40 + t230 * t6 / 0.2D1 + t62) - 0.180D3
     # * t43 * 0.3141592653589793D1 * (t40 - t228 * t6) + t61) * t199 / 
     #0.1440D4
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t244)
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t244)
      t249 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t244)
      t251 = x2 * x3
      t252 = 0.1D1 - x3 + t251
      t253 = 0.1D1 / t252
      t254 = t251 * t253
      t255 = t2 * t254
      t256 = t18 * t253
      t257 = t2 * t256
      t259 = t252 ** 2
      t265 = log(0.4D1 * t108 * t9 * t153 * t18 / t259)
      t266 = t124 * t18
      t268 = Sqrt(t26 * t266)
      t272 = 0.1D1 / (-z - x3 + t251 + 0.2D1 * t25 * t268)
      t274 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t122, -t
     #256, x4)
      t277 = t272 * z
      t278 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t122, -t
     #256, x4)
      t297 = (0.90D2 * t5 * (-t265 * t272 * z * t274 + t277 * t278) - 0.
     #180D3 * t43 * 0.3141592653589793D1 * t277 * t274) * t70 * t142 / 0
     #.1440D4 + t5 * t277 * t274 * t70 * t142 * t199 / 0.8D1
      t298 = FJET(XB1, XB2, s, 0.0D0, t255, 0.0D0, -t257, 0.0D0, t297)
      t301 = t1 * x1
      t302 = x1 * z
      t303 = -z - x1 + t302
      t304 = 0.1D1 / t303
      t306 = t124 * s * t301 * t304
      t307 = -0.1D1 + x1
      t308 = t2 * t307
      t310 = x2 * s * t301
      t311 = t1 ** 2
      t312 = s * t311
      t315 = x1 * t307 * t304
      t316 = t312 * t124 * t315
      t317 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, t122, 0.10D
     #1, x4)
      t321 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, t122, 0.10D
     #1, x4)
      t322 = t207 * t13
      t324 = 0.1D1 / t7 * t304
      t325 = t307 ** 2
      t330 = log(0.4D1 * t322 * t324 * t325 * t124)
      t342 = -t5 * t317 * t204 / 0.8D1 - (0.90D2 * t5 * (t321 - t330 * t
     #317) - 0.180D3 * t43 * 0.3141592653589793D1 * t317) * t142 * t199 
     #/ 0.720D3
      t343 = FJET(XB1, XB2, s, 0.0D0, t306, -t308, t310, -t316, t342)
      t346 = t2 * x1 * t304
      t347 = t312 * t315
      t348 = z * t303
      t349 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t351 = t178 * t13
      t356 = log(0.4D1 * t351 * t324 * t325 * t19)
      t358 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t362 = x3 * x1
      t363 = t362 * z
      t364 = t178 * t7
      t365 = x1 * t7
      t366 = x3 * t7
      t367 = t366 * x1
      t369 = 0.2D1 * t178 * z
      t370 = x3 * t303
      t372 = Sqrt(t370 * t18)
      t377 = 0.1D1 / (-t302 - t363 - t178 - t364 - t26 + t365 + t367 + t
     #369 + 0.2D1 * t25 * t372 * z - t7)
      t379 = t324 * t325
      t382 = log(-0.4D1 * t351 * t379)
      t389 = -t348 * t358 * t377 + t358
      t396 = (0.90D2 * t5 * (-(t348 * t349 - t356 * z * t303 * t358) * t
     #377 + t349 - t382 * t358) - 0.180D3 * t43 * 0.3141592653589793D1 *
     # t389) * t70 * t199 / 0.1440D4
      t399 = t5 * t389 * t204 / 0.8D1
      t402 = log(-0.4D1 * t322 * t379)
      t404 = -t349 + t402 * t358
      t407 = 0.3141592653589793D1 * t358
      t409 = 0.180D3 * t43 * t407
      t413 = (0.90D2 * t5 * t404 + t409) * t142 * t199 / 0.720D3
      t416 = log(-0.4D1 * t225 * t379)
      t418 = t416 ** 2
      t421 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t422 = t416 * t349 - t418 * t358 / 0.2D1 - t421
      t426 = -t349 + t416 * t358
      t430 = t60 * t407
      t434 = t396 + t399 - t413 - (0.90D2 * t5 * t422 - 0.180D3 * t43 * 
     #0.3141592653589793D1 * t426 - t430) * t199 / 0.1440D4
      t435 = FJET(XB1, XB2, s, 0.0D0, -t308, -t346, 0.0D0, t347, t434)
      t437 = FJET(XB1, XB2, s, 0.0D0, -t346, -t308, 0.0D0, t347, t434)
      t439 = FJET(XB1, XB2, s, 0.0D0, -t257, 0.0D0, t255, 0.0D0, t297)
      t441 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t244)
      t443 = FJET(XB1, XB2, s, t310, -t308, t306, 0.0D0, -t316, t342)
      t445 = FJET(XB1, XB2, s, t255, 0.0D0, -t257, 0.0D0, 0.0D0, t297)
      t447 = FJET(XB1, XB2, s, t306, 0.0D0, t310, -t308, -t316, t342)
      t452 = t18 * s * t1 * t307 * t253
      t453 = t2 * x1
      t455 = Sqrt(-t370 * t266)
      t456 = t25 * t455
      t462 = t453 * x2 * (-x3 + t251 - z + t26 - x1 + t362 + t302 - t363
     # + 0.2D1 * t456) * t304 * t253
      t463 = t308 * t254
      t464 = t108 * t302
      t465 = t108 * x1
      t472 = t453 * (-t464 + t465 + t108 * z + 0.2D1 * t456 * x2 + 0.1D1
     # - x3 - x2 + t251) * t304 * t253
      t473 = x2 * x1
      t474 = t473 * z
      t475 = -z + t474 - t473
      t488 = x2 * t177
      t489 = t464 - 0.2D1 * t456 * t473 - 0.2D1 * t251 * t302 + t366 * t
     #473 + 0.2D1 * t178 * x2 * z - t178 * t7 * x2 + t302 + t178 + t26 -
     # t365 + 0.2D1 * t456 * t474 + t488 + t7
      t499 = t363 + t364 - t367 - t369 - t465 + t474 - 0.2D1 * t456 * z 
     #- t251 * z + t251 * x1 - t178 * x2 - t473 * t7 - 0.2D1 * t488 * z 
     #+ t488 * t7
      t501 = 0.1D1 / (t489 + t499)
      t504 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, t122, -t256
     #, x4)
      t506 = t504 * t303 * t204
      t508 = t5 * t475 * t501 * t506 / 0.8D1
      t509 = FJET(XB1, XB2, s, t452, t462, -t463, -t472, -t316, t508)
      t512 = 0.3141592653589793D1 * t475 * t501
      t516 = FJET(XB1, XB2, s, t462, t452, -t472, -t463, -t316, t508)
      t530 = (0.90D2 * t5 * t422 - 0.180D3 * t43 * 0.3141592653589793D1 
     #* t426 - t430) * t199 / 0.1440D4
      t531 = t396 + t399 - t413 - t530
      t532 = FJET(XB1, XB2, s, -t308, 0.0D0, 0.0D0, -t346, t347, t531)
      t534 = FJET(XB1, XB2, s, -t308, t310, 0.0D0, t306, -t316, t342)
      t543 = t396 + t399 - (0.90D2 * t5 * t404 + t409) * t142 * t199 / 0
     #.720D3 - t530
      t544 = FJET(XB1, XB2, s, -t346, 0.0D0, 0.0D0, -t308, t347, t543)
      t546 = FJET(XB1, XB2, s, -t257, 0.0D0, t255, 0.0D0, 0.0D0, t297)
      t548 = FJET(XB1, XB2, s, -t472, -t463, t462, t452, -t316, t508)
      t553 = FJET(XB1, XB2, s, -t463, -t472, t452, t462, -t316, t508)
      rrgg2qqbarht7s3e0 = t245 * t244 + t247 * t244 + t249 * t244 + t298
     # * t297 + t343 * t342 + t435 * t434 + t437 * t434 + t439 * t297 + 
     #t441 * t244 + t443 * t342 + t445 * t297 + t447 * t342 + t509 * t4 
     #* t512 * t506 / 0.8D1 + t516 * t4 * t512 * t506 / 0.8D1 + t532 * t
     #531 + t534 * t342 + t544 * t543 + t546 * t297 + t548 * t4 * t512 *
     # t506 / 0.8D1 + t553 * t4 * t512 * t506 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht7s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = -0.1D1 + x3
      t22 = log(-0.4D1 * t10 * t13 / t17)
      t23 = cos(t11)
      t24 = x3 * z
      t26 = Sqrt(-t24 * t17)
      t30 = 0.1D1 / (-z - x3 + 0.2D1 * t23 * t26)
      t37 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t40 = lh * t4
      t41 = 0.3141592653589793D1 * t6
      t43 = 0.180D3 * t40 * t41
      t49 = 0.1D1 / x3
      t55 = log(0.4D1 * t9 * t13)
      t64 = t55 ** 2
      t66 = lh ** 2
      t68 = 0.3141592653589793D1 ** 2
      t74 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t78 = t30 * t6 * z
      t79 = 0.1D1 - x2
      t80 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t79, 0.10
     #D1, x4)
      t83 = 0.1D1 / x2
      t87 = x2 ** 2
      t88 = t9 * t87
      t91 = log(0.4D1 * t88 * t13)
      t93 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t79, 0.10
     #D1, x4)
      t94 = -t79
      t98 = log(-0.4D1 * t88 * t13 * t94)
      t103 = t6 - t80
      t111 = 0.1D1 / x1
      t115 = x1 ** 2
      t116 = t115 * t13
      t119 = log(0.4D1 * t116 * t9)
      t132 = -(-0.90D2 * t5 * t6 * (t16 + t22 * t30 * z) + (-0.90D2 * t5
     # * t37 + t43) * (-0.1D1 - t30 * z)) * t49 / 0.2880D4 - (-0.180D3 *
     # lh - 0.90D2 * t55) * t4 * 0.3141592653589793D1 * t37 / 0.2880D4 -
     # (0.180D3 * t55 * lh + 0.45D2 * t64 + 0.180D3 * t66 - 0.30D2 * t68
     #) * t4 * t41 / 0.2880D4 - t5 * t74 / 0.32D2 + t5 * (-t78 + t80 - t
     #6) * t49 * t83 / 0.16D2 - (0.90D2 * t5 * (t37 - t91 * t6 - t93 + t
     #98 * t80) - 0.180D3 * t40 * 0.3141592653589793D1 * t103) * t83 / 0
     #.1440D4 - t5 * t103 * t83 * t111 / 0.8D1 - (0.90D2 * t5 * (t37 - t
     #119 * t6) - t43) * t111 / 0.1440D4 + t5 * (-t78 - t6) * t49 * t111
     # / 0.16D2
      t133 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t132)
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t132)
      t137 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t132)
      t139 = x2 * x3
      t141 = 0.1D1 / (0.1D1 - x3 + t139)
      t143 = t2 * t139 * t141
      t144 = t17 * t141
      t145 = t2 * t144
      t148 = Sqrt(t24 * t94 * t17)
      t152 = 0.1D1 / (-z - x3 + t139 + 0.2D1 * t23 * t148)
      t154 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t79, -t1
     #44, x4)
      t157 = z * t154 * t49 * t83
      t159 = t5 * t152 * t157 / 0.16D2
      t160 = FJET(XB1, XB2, s, 0.0D0, t143, 0.0D0, -t145, 0.0D0, t159)
      t162 = 0.3141592653589793D1 * t152
      t167 = t1 * x1
      t168 = x1 * z
      t169 = -z - x1 + t168
      t170 = 0.1D1 / t169
      t172 = t94 * s * t167 * t170
      t173 = -0.1D1 + x1
      t174 = t2 * t173
      t176 = x2 * s * t167
      t177 = t1 ** 2
      t178 = s * t177
      t181 = x1 * t173 * t170
      t182 = t178 * t94 * t181
      t183 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, t79, 0.10D1
     #, x4)
      t185 = t183 * t83 * t111
      t187 = t5 * t185 / 0.8D1
      t188 = FJET(XB1, XB2, s, 0.0D0, t172, -t174, t176, -t182, -t187)
      t194 = t2 * x1 * t170
      t195 = t178 * t181
      t196 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t200 = t5 * t196 * t83 * t111 / 0.8D1
      t201 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t204 = t173 ** 2
      t208 = log(-0.4D1 * t116 / t7 * t170 * t204)
      t210 = -t201 + t208 * t196
      t215 = 0.180D3 * t40 * 0.3141592653589793D1 * t196
      t222 = x3 * t115
      t231 = Sqrt(x3 * t169 * t17)
      t243 = t5 * (-z * t169 * t196 / (-t168 - x3 * x1 * z - t222 - t222
     # * t7 - t24 + x1 * t7 + x3 * t7 * x1 + 0.2D1 * t222 * z + 0.2D1 * 
     #t23 * t231 * z - t7) + t196) * t49 * t111 / 0.16D2
      t244 = t200 - (0.90D2 * t5 * t210 + t215) * t111 / 0.1440D4 + t243
      t245 = FJET(XB1, XB2, s, 0.0D0, -t174, -t194, 0.0D0, t195, t244)
      t247 = FJET(XB1, XB2, s, 0.0D0, -t194, -t174, 0.0D0, t195, t244)
      t249 = FJET(XB1, XB2, s, 0.0D0, -t145, 0.0D0, t143, 0.0D0, t159)
      t254 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t256 = FJET(XB1, XB2, s, t176, -t174, t172, 0.0D0, -t182, -t187)
      t261 = FJET(XB1, XB2, s, t143, 0.0D0, -t145, 0.0D0, 0.0D0, t159)
      t266 = FJET(XB1, XB2, s, t172, 0.0D0, t176, -t174, -t182, -t187)
      t277 = t200 - (0.90D2 * t5 * t210 + t215) * t111 / 0.1440D4 + t243
      t278 = FJET(XB1, XB2, s, -t174, 0.0D0, 0.0D0, -t194, t195, t277)
      t280 = FJET(XB1, XB2, s, -t174, t176, 0.0D0, t172, -t182, -t187)
      t285 = FJET(XB1, XB2, s, -t194, 0.0D0, 0.0D0, -t174, t195, t277)
      t287 = FJET(XB1, XB2, s, -t145, 0.0D0, t143, 0.0D0, 0.0D0, t159)
      rrgg2qqbarht7s3em1 = t133 * t132 + t135 * t132 + t137 * t132 + t16
     #0 * t4 * t162 * t157 / 0.16D2 - t188 * t4 * 0.3141592653589793D1 *
     # t185 / 0.8D1 + t245 * t244 + t247 * t244 + t249 * t4 * t162 * t15
     #7 / 0.16D2 + t254 * t132 - t256 * t4 * 0.3141592653589793D1 * t185
     # / 0.8D1 + t261 * t4 * t162 * t157 / 0.16D2 - t266 * t4 * 0.314159
     #2653589793D1 * t185 / 0.8D1 + t277 * t278 - t280 * t4 * 0.31415926
     #53589793D1 * t185 / 0.8D1 + t285 * t277 + t287 * t4 * t162 * t157 
     #/ 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t12 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t25 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.1D1 - x
     #2, 0.10D1, x4)
      t31 = 0.1D1 / x1
      t35 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t39 = z ** 2
      t42 = Sin(t7)
      t43 = t42 ** 2
      t46 = log(0.4D1 / t39 / z * t43)
      t53 = t5 * t6 * (-0.1D1 - 0.1D1 / (-z - x3 + 0.2D1 * t8 * t12) * z
     #) / x3 / 0.32D2 - t5 * (t6 - t25) / x2 / 0.16D2 - t5 * t6 * t31 / 
     #0.16D2 - t5 * t35 / 0.32D2 - (-0.180D3 * lh - 0.90D2 * t46) * t4 *
     # 0.3141592653589793D1 * t6 / 0.2880D4
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t53)
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t53)
      t58 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t53)
      t60 = -0.1D1 + x1
      t61 = t2 * t60
      t64 = 0.1D1 / (-z - x1 + x1 * z)
      t66 = t2 * x1 * t64
      t67 = t1 ** 2
      t71 = s * t67 * x1 * t60 * t64
      t72 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10
     #D1, x4)
      t75 = t5 * t72 * t31 / 0.16D2
      t76 = FJET(XB1, XB2, s, 0.0D0, -t61, -t66, 0.0D0, t71, t75)
      t79 = 0.3141592653589793D1 * t72 * t31
      t82 = FJET(XB1, XB2, s, 0.0D0, -t66, -t61, 0.0D0, t71, t75)
      t86 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t53)
      t88 = FJET(XB1, XB2, s, -t61, 0.0D0, 0.0D0, -t66, t71, t75)
      t92 = FJET(XB1, XB2, s, -t66, 0.0D0, 0.0D0, -t61, t71, t75)
      rrgg2qqbarht7s3em2 = t54 * t53 + t56 * t53 + t58 * t53 + t76 * t4 
     #* t79 / 0.16D2 + t82 * t4 * t79 / 0.16D2 + t86 * t53 + t88 * t4 * 
     #t79 / 0.16D2 + t92 * t4 * t79 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t8 = t4 * 0.3141592653589793D1 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = 0.3141592653589793D1 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s3em3 = -t9 * t4 * t11 / 0.32D2 - t13 * t4 * t11 / 0.
     #32D2 - t16 * t4 * t11 / 0.32D2 - t19 * t4 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht7s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      rrgg2qqbarht7s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht7s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * 0.3141592653589793D1
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t17 = x1 * z
      t18 = -z - x1 + t17
      t19 = 0.1D1 / t18
      t21 = t4 ** 2
      t22 = t16 * t19 * t21
      t25 = log(-0.4D1 * t10 * t13 * t22)
      t26 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t28 = t25 ** 2
      t29 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t32 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t36 = lh * t7
      t42 = lh ** 2
      t43 = 0.180D3 * t42
      t44 = 0.3141592653589793D1 ** 2
      t45 = 0.30D2 * t44
      t46 = t43 - t45
      t47 = t46 * t7
      t48 = 0.3141592653589793D1 * t29
      t49 = t47 * t48
      t51 = 0.1D1 / x3
      t53 = 0.1D1 / x1
      t54 = (0.90D2 * t8 * (-t25 * t26 + t28 * t29 / 0.2D1 + t32) - 0.18
     #0D3 * t36 * 0.3141592653589793D1 * (t26 - t25 * t29) + t49) * t51 
     #* t53
      t55 = t9 * t13
      t58 = log(-0.4D1 * t55 * t22)
      t60 = t26 - t58 * t29
      t63 = t58 ** 2
      t66 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t71 = t63 * t26 / 0.2D1 + t66 - t63 * t58 * t29 / 0.6D1 - t58 * t3
     #2
      t75 = 0.60D2 * lh * t44
      t77 = 0.120D3 * t42 * lh
      t78 = t75 - 0.2884936567583026D3 - t77
      t79 = t78 * t7
      t80 = t79 * t48
      t84 = -t58 * t26 + t63 * t29 / 0.2D1 + t32
      t90 = t10 * x2
      t91 = t13 * t16
      t92 = t21 * t19
      t96 = log(-0.4D1 * t90 * t91 * t92)
      t105 = 0.1D1 / x2
      t106 = t105 * t53
      t107 = (0.90D2 * t8 * (-t26 + t96 * t29) + 0.180D3 * t36 * t48) * 
     #t51 * t106
      t108 = x2 * t9
      t109 = t108 * t13
      t112 = log(-0.4D1 * t109 * t22)
      t114 = t112 ** 2
      t127 = (0.90D2 * t8 * (t112 * t26 - t114 * t29 / 0.2D1 - t32) - 0.
     #180D3 * t36 * 0.3141592653589793D1 * (-t26 + t112 * t29) - t49) * 
     #t105 * t53
      t129 = t54 / 0.720D3 + (t47 * 0.3141592653589793D1 * t60 + 0.90D2 
     #* t8 * t71 + t80 - 0.180D3 * t36 * 0.3141592653589793D1 * t84) * t
     #53 / 0.720D3 - t107 / 0.720D3 - t127 / 0.720D3
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t129)
      t132 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t134 = 0.1D1 / t15 / z
      t135 = x3 * t134
      t138 = log(0.4D1 * t135 * t13)
      t139 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t144 = t138 ** 2
      t147 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t151 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t156 = 0.3141592653589793D1 * t139
      t157 = t79 * t156
      t168 = t134 * t13
      t170 = log(0.4D1 * t168)
      t171 = t170 ** 2
      t174 = t171 * t170
      t197 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t206 = t44 ** 2
      t207 = t42 ** 2
      t211 = t171 ** 2
      t219 = log(0.4D1 * t10 * t168)
      t221 = t219 ** 2
      t237 = t55 * t134
      t239 = log(0.4D1 * t237)
      t244 = t239 ** 2
      t264 = x2 * x3
      t267 = log(0.4D1 * t264 * t237)
      t269 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t270 = -0.1D1 + x2
      t271 = t270 ** 2
      t272 = t168 * t271
      t275 = log(0.4D1 * t90 * t272)
      t276 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t282 = 0.3141592653589793D1 * (-t276 + t139)
      t291 = log(0.4D1 * t108 * t168)
      t293 = t291 ** 2
      t298 = log(0.4D1 * t108 * t272)
      t300 = t298 ** 2
      t303 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t313 = t47 * t282
      t320 = log(0.4D1 * t264 * t168)
      t322 = t320 ** 2
      t327 = log(0.4D1 * t264 * t272)
      t329 = t327 ** 2
      t345 = x2 * t134
      t346 = t13 * t271
      t349 = log(0.4D1 * t345 * t346)
      t353 = log(0.4D1 * t345 * t13)
      t358 = t353 ** 2
      t365 = t349 ** 2
      t368 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t390 = (t47 * 0.3141592653589793D1 * (-t132 + t138 * t139) + 0.90D
     #2 * t8 * (-t144 * t132 / 0.2D1 - t147 + t144 * t138 * t139 / 0.6D1
     # + t138 * t151) - t157 - 0.180D3 * t36 * 0.3141592653589793D1 * (t
     #138 * t132 - t144 * t139 / 0.2D1 - t151)) * t51 / 0.1440D4 - (-0.9
     #0D2 * t171 * lh + t75 - 0.2884936567583026D3 - t77 - 0.15D2 * t174
     # - t170 * t46) * t7 * 0.3141592653589793D1 * t132 / 0.1440D4 - (0.
     #180D3 * t170 * lh + 0.45D2 * t171 + t43 - t45) * t7 * 0.3141592653
     #589793D1 * t151 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t170) * t7 
     #* 0.3141592653589793D1 * t147 / 0.1440D4 - t8 * t197 / 0.16D2 - (0
     #.30D2 * t174 * lh + t171 * t46 / 0.2D1 - t170 * t78 + 0.5769873135
     #166051D3 * lh + t206 + 0.60D2 * t207 - 0.60D2 * t42 * t44 + 0.15D2
     # / 0.4D1 * t211) * t7 * t156 / 0.1440D4 + (0.90D2 * t8 * (t219 * t
     #132 - t221 * t139 / 0.2D1 - t151) - 0.180D3 * t36 * 0.314159265358
     #9793D1 * (-t132 + t219 * t139) - t47 * t156) * t51 * t53 / 0.720D3
     # + (t47 * 0.3141592653589793D1 * (-t132 + t239 * t139) + 0.90D2 * 
     #t8 * (-t244 * t132 / 0.2D1 - t147 + t244 * t239 * t139 / 0.6D1 + t
     #239 * t151) - t157 - 0.180D3 * t36 * 0.3141592653589793D1 * (t239 
     #* t132 - t244 * t139 / 0.2D1 - t151)) * t53 / 0.720D3 - (0.90D2 * 
     #t8 * (t132 - t267 * t139 - t269 + t275 * t276) - 0.180D3 * t36 * t
     #282) * t51 * t106 / 0.720D3 - (0.90D2 * t8 * (-t291 * t132 + t293 
     #* t139 / 0.2D1 + t151 + t298 * t269 - t300 * t276 / 0.2D1 - t303) 
     #- 0.180D3 * t36 * 0.3141592653589793D1 * (t132 - t291 * t139 - t26
     #9 + t298 * t276) + t313) * t105 * t53 / 0.720D3 - (0.90D2 * t8 * (
     #-t320 * t132 + t322 * t139 / 0.2D1 + t151 + t327 * t269 - t329 * t
     #276 / 0.2D1 - t303) - 0.180D3 * t36 * 0.3141592653589793D1 * (t132
     # - t320 * t139 - t269 + t327 * t276) + t313) * t51 * t105 / 0.1440
     #D4 - (t47 * 0.3141592653589793D1 * (-t269 + t349 * t276 + t132 - t
     #353 * t139) + 0.90D2 * t8 * (t358 * t132 / 0.2D1 + t147 - t358 * t
     #353 * t139 / 0.6D1 - t353 * t151 - t365 * t269 / 0.2D1 - t368 + t3
     #65 * t349 * t276 / 0.6D1 + t349 * t303) + t79 * t282 - 0.180D3 * t
     #36 * 0.3141592653589793D1 * (t349 * t269 - t365 * t276 / 0.2D1 - t
     #303 - t353 * t132 + t358 * t139 / 0.2D1 + t151)) * t105 / 0.1440D4
      t391 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t390)
      t394 = t264 - 0.1D1
      t395 = 0.1D1 / t394
      t396 = x3 * t270 * t395
      t397 = t5 * t396
      t398 = cos(t11)
      t400 = -0.1D1 + x3
      t401 = x2 * t400
      t403 = Sqrt(x3 * t18 * t401)
      t404 = t398 * t403
      t409 = x2 ** 2
      t410 = x3 * t409
      t411 = t410 * t17
      t412 = x3 * x1
      t413 = t412 * z
      t414 = t264 * z
      t416 = t264 * x1
      t418 = t410 * x1
      t420 = x3 * z
      t421 = 0.2D1 * t404
      t422 = 0.2D1 * t404 * x2 - 0.2D1 * t264 * t17 + t411 + t413 + 0.2D
     #1 * t414 + 0.2D1 * t416 - t418 - t410 * z - x2 - t420 - t412 - t42
     #1 + t264
      t425 = t3 * t422 * t19 * t395
      t426 = t400 * s
      t427 = t1 * t4
      t429 = t426 * t427 * t395
      t434 = t3 * t270 * (-t264 - z + t420 - x1 + t412 + t17 - t413 + t4
     #21) * t19 * t395
      t435 = t1 ** 2
      t440 = s * t435 * x2 * x1 * t4 * t19
      t441 = t400 * t395
      t442 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t441, x
     #4)
      t446 = t394 ** 2
      t447 = 0.1D1 / t446
      t454 = log(0.4D1 * t264 * t400 * t271 * t13 * t447 * t16 * t21 * t
     #9 * t19)
      t455 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t441, x
     #4)
      t464 = x2 * x1
      t480 = 0.2D1 * x1 * t15 + 0.2D1 * t9 * z - t9 * t15 + t108 - 0.2D1
     # * t404 * t464 - 0.2D1 * t404 * t17 + x3 * t15 * t464 + 0.2D1 * t1
     #0 * x2 * z - t10 * t15 * x2 - t411 + 0.2D1 * t404 * z + 0.2D1 * t4
     #04 * x1
      t481 = t464 * z
      t489 = t481 - t414 - t416 - t90 + t418 - t464 * t15 - 0.2D1 * t108
     # * z + t108 * t15 + 0.2D1 * t404 * t481 - 0.2D1 * t17 - t15 - t9
      t491 = 0.1D1 / (t480 + t489)
      t492 = z + x1 + t481 - t17 - t464
      t497 = t36 * 0.3141592653589793D1
      t503 = -0.90D2 * t8 * (t442 - t454 * t455) * t491 * t492 * t18 + 0
     #.180D3 * t497 * t455 * t491 * t492 * t18
      t506 = t503 * t51 * t106 / 0.720D3
      t507 = FJET(XB1, XB2, s, -t397, -t425, -t429, t434, t440, -t506)
      t510 = t51 * t105 * t53
      t513 = t426 * t427
      t514 = t1 * x1
      t515 = t426 * t514
      t517 = t2 * t4 * x3
      t518 = t2 * t412
      t521 = x3 * t400 * t92
      t524 = log(0.4D1 * t55 * t16 * t521)
      t525 = -t400
      t526 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t525
     #, x4)
      t528 = t524 ** 2
      t529 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t525
     #, x4)
      t532 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t525
     #, x4)
      t533 = t524 * t526 - t528 * t529 / 0.2D1 - t532
      t537 = -t526 + t524 * t529
      t541 = 0.3141592653589793D1 * t529
      t542 = t47 * t541
      t549 = log(0.4D1 * t108 * t91 * t521)
      t558 = (0.90D2 * t8 * (t526 - t549 * t529) - 0.180D3 * t36 * t541)
     # * t51 * t106
      t560 = (0.90D2 * t8 * t533 - 0.180D3 * t36 * 0.3141592653589793D1 
     #* t537 - t542) * t51 * t53 / 0.720D3 - t558 / 0.720D3
      t561 = FJET(XB1, XB2, s, t513, -t515, -t517, t518, 0.0D0, t560)
      t563 = t2 * x3
      t564 = t2 * t400
      t568 = log(-0.4D1 * t10 * t168 * t400)
      t569 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #525, x4)
      t571 = t568 ** 2
      t572 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #525, x4)
      t575 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #525, x4)
      t584 = 0.3141592653589793D1 * t572
      t585 = t47 * t584
      t589 = (0.90D2 * t8 * (-t568 * t569 + t571 * t572 / 0.2D1 + t575) 
     #- 0.180D3 * t36 * 0.3141592653589793D1 * (t569 - t568 * t572) + t5
     #85) * t51 * t53 / 0.720D3
      t593 = log(-0.4D1 * t109 * t135 * t400)
      t603 = (0.90D2 * t8 * (-t569 + t593 * t572) + 0.180D3 * t36 * t584
     #) * t51 * t106 / 0.720D3
      t607 = log(-0.4D1 * t135 * t13 * t400)
      t612 = t607 ** 2
      t615 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #525, x4)
      t633 = (t47 * 0.3141592653589793D1 * (t569 - t607 * t572) + 0.90D2
     # * t8 * (t612 * t569 / 0.2D1 + t615 - t612 * t607 * t572 / 0.6D1 -
     # t607 * t575) + t79 * t584 - 0.180D3 * t36 * 0.3141592653589793D1 
     #* (-t607 * t569 + t612 * t572 / 0.2D1 + t575)) * t51 / 0.1440D4
      t638 = log(-0.4D1 * t345 * t13 * x3 * t400)
      t640 = t638 ** 2
      t643 = t638 * t569 - t640 * t572 / 0.2D1 - t575
      t647 = -t569 + t638 * t572
      t655 = t589 - t603 + t633 - (0.90D2 * t8 * t643 - 0.180D3 * t36 * 
     #0.3141592653589793D1 * t647 - t585) * t51 * t105 / 0.1440D4
      t656 = FJET(XB1, XB2, s, 0.0D0, t563, 0.0D0, -t564, 0.0D0, t655)
      t671 = t54 / 0.720D3 + (t47 * 0.3141592653589793D1 * t60 + 0.90D2 
     #* t8 * t71 + t80 - 0.180D3 * t36 * 0.3141592653589793D1 * t84) * t
     #53 / 0.720D3 - t107 / 0.720D3 - t127 / 0.720D3
      t672 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t671)
      t675 = t270 * s * t514
      t677 = t2 * t464 * t19
      t678 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t682 = t16 * t21 * t19 * t271
      t685 = log(-0.4D1 * t264 * t55 * t682)
      t686 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t691 = 0.3141592653589793D1 * t686
      t699 = log(-0.4D1 * t109 * t682)
      t701 = t699 ** 2
      t704 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t718 = -(0.90D2 * t8 * (t678 - t685 * t686) - 0.180D3 * t36 * t691
     #) * t51 * t106 / 0.720D3 - (0.90D2 * t8 * (-t699 * t678 + t701 * t
     #686 / 0.2D1 + t704) - 0.180D3 * t36 * 0.3141592653589793D1 * (t678
     # - t699 * t686) + t47 * t691) * t105 * t53 / 0.720D3
      t719 = FJET(XB1, XB2, s, -t5, -t675, 0.0D0, -t677, t440, t718)
      t721 = t2 * t441
      t722 = t2 * t396
      t729 = log(-0.4D1 * t264 * t400 * t134 * t271 * t13 * t447)
      t730 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t441
     #, x4)
      t732 = t729 ** 2
      t733 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t441
     #, x4)
      t736 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t441
     #, x4)
      t739 = Sqrt(-t420 * t401)
      t743 = 0.1D1 / (-z - t264 + 0.2D1 * t398 * t739)
      t756 = t733 * t743 * z
      t768 = log(-0.4D1 * t264 * t400 * t134 * t346 * t447 * t9)
      t781 = -(-0.90D2 * t8 * (-t729 * t730 + t732 * t733 / 0.2D1 + t736
     #) * t743 * z + 0.180D3 * t497 * (t730 - t729 * t733) * t743 * z - 
     #t47 * 0.3141592653589793D1 * t756) * t51 * t105 / 0.1440D4 - (-0.9
     #0D2 * t8 * (t730 - t768 * t733) * t743 * z + 0.180D3 * t497 * t756
     #) * t51 * t106 / 0.720D3
      t782 = FJET(XB1, XB2, s, t721, 0.0D0, t722, 0.0D0, 0.0D0, t781)
      t784 = FJET(XB1, XB2, s, 0.0D0, t722, 0.0D0, t721, 0.0D0, t781)
      t797 = t589 - t603 + t633 - (0.90D2 * t8 * t643 - 0.180D3 * t36 * 
     #0.3141592653589793D1 * t647 - t585) * t51 * t105 / 0.1440D4
      t798 = FJET(XB1, XB2, s, -t564, 0.0D0, t563, 0.0D0, 0.0D0, t797)
      t811 = (0.90D2 * t8 * t533 - 0.180D3 * t36 * 0.3141592653589793D1 
     #* t537 - t542) * t51 * t53 / 0.720D3 - t558 / 0.720D3
      t812 = FJET(XB1, XB2, s, -t517, t518, t513, -t515, 0.0D0, t811)
      t814 = FJET(XB1, XB2, s, 0.0D0, t721, 0.0D0, t722, 0.0D0, t781)
      t816 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t671)
      t818 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t390)
      t820 = t130 * t129 + t391 * t390 - t507 * t503 * t510 / 0.720D3 + 
     #t561 * t560 + t656 * t655 + t672 * t671 + t719 * t718 + t782 * t78
     #1 + t784 * t781 + t798 * t797 + t812 * t811 + t814 * t781 + t816 *
     # t671 + t818 * t390
      t821 = FJET(XB1, XB2, s, -t429, t434, -t397, -t425, t440, -t506)
      t825 = FJET(XB1, XB2, s, -t425, -t397, t434, -t429, t440, -t506)
      t829 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t129)
      t831 = FJET(XB1, XB2, s, t518, -t517, -t515, t513, 0.0D0, t560)
      t833 = FJET(XB1, XB2, s, 0.0D0, -t564, 0.0D0, t563, 0.0D0, t655)
      t835 = FJET(XB1, XB2, s, 0.0D0, -t677, -t5, -t675, t440, t718)
      t837 = FJET(XB1, XB2, s, -t675, -t5, -t677, 0.0D0, t440, t718)
      t839 = FJET(XB1, XB2, s, -t677, 0.0D0, -t675, -t5, t440, t718)
      t841 = FJET(XB1, XB2, s, -t515, t513, t518, -t517, 0.0D0, t811)
      t843 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t390)
      t845 = FJET(XB1, XB2, s, t563, 0.0D0, -t564, 0.0D0, 0.0D0, t655)
      t847 = FJET(XB1, XB2, s, t722, 0.0D0, t721, 0.0D0, 0.0D0, t781)
      t849 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t390)
      t851 = FJET(XB1, XB2, s, t434, -t429, -t425, -t397, t440, -t506)
      t855 = -t821 * t503 * t510 / 0.720D3 - t825 * t503 * t510 / 0.720D
     #3 + t829 * t129 + t831 * t560 + t833 * t655 + t835 * t718 + t837 *
     # t718 + t839 * t718 + t841 * t811 + t843 * t390 + t845 * t655 + t8
     #47 * t781 + t849 * t390 - t851 * t503 * t510 / 0.720D3
      rrgg2qqbarht7s4e1 = t820 + t855

      end function



      doubleprecision function rrgg2qqbarht7s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * 0.3141592653589793D1
      t9 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = x1 * z
      t19 = -z - x1 + t18
      t20 = 0.1D1 / t19
      t21 = t17 * t20
      t22 = t4 ** 2
      t23 = t21 * t22
      t26 = log(-0.4D1 * t11 * t14 * t23)
      t27 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t32 = lh * t7
      t33 = 0.3141592653589793D1 * t27
      t35 = 0.180D3 * t32 * t33
      t37 = 0.1D1 / x3
      t39 = 0.1D1 / x1
      t41 = (0.90D2 * t8 * (t9 - t26 * t27) - t35) * t37 * t39 / 0.720D3
      t43 = 0.1D1 / x2
      t45 = t37 * t43 * t39
      t47 = t8 * t27 * t45 / 0.8D1
      t48 = x2 * t10
      t49 = t48 * t14
      t52 = log(-0.4D1 * t49 * t23)
      t60 = (0.90D2 * t8 * (-t9 + t52 * t27) + t35) * t43 * t39 / 0.720D
     #3
      t61 = t10 * t14
      t64 = log(-0.4D1 * t61 * t23)
      t66 = t64 ** 2
      t69 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t70 = t64 * t9 - t66 * t27 / 0.2D1 - t69
      t74 = -t9 + t64 * t27
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t7
      t84 = t83 * t33
      t88 = t41 + t47 - t60 + (-0.90D2 * t8 * t70 + 0.180D3 * t32 * 0.31
     #41592653589793D1 * t74 + t84) * t39 / 0.720D3
      t89 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t88)
      t91 = -0.1D1 + x3
      t92 = t91 * s
      t93 = t1 * x1
      t94 = t92 * t93
      t95 = t1 * t4
      t96 = t92 * t95
      t97 = x3 * x1
      t98 = t2 * t97
      t100 = t2 * t4 * x3
      t101 = -t91
      t102 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t101
     #, x4)
      t109 = log(0.4D1 * t61 * t17 * x3 * t91 * t20 * t22)
      t110 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t101
     #, x4)
      t112 = t102 - t109 * t110
      t117 = 0.180D3 * t32 * 0.3141592653589793D1 * t110
      t124 = t8 * t110 * t45 / 0.8D1
      t125 = (-0.90D2 * t8 * t112 + t117) * t37 * t39 / 0.720D3 - t124
      t126 = FJET(XB1, XB2, s, -t94, t96, t98, -t100, 0.0D0, t125)
      t138 = t41 + t47 - t60 + (-0.90D2 * t8 * t70 + 0.180D3 * t32 * 0.3
     #141592653589793D1 * t74 + t84) * t39 / 0.720D3
      t139 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t138)
      t141 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t88)
      t144 = 0.1D1 / t16 / z
      t145 = x3 * t144
      t148 = log(0.4D1 * t145 * t14)
      t149 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t151 = t148 ** 2
      t152 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t155 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t164 = 0.3141592653589793D1 * t152
      t165 = t83 * t164
      t169 = t144 * t14
      t171 = log(0.4D1 * t169)
      t174 = t171 ** 2
      t181 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t204 = x2 * x3
      t207 = log(0.4D1 * t204 * t169)
      t209 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t210 = -0.1D1 + x2
      t211 = t210 ** 2
      t212 = t169 * t211
      t215 = log(0.4D1 * t204 * t212)
      t216 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t221 = -t216 + t152
      t222 = 0.3141592653589793D1 * t221
      t224 = 0.180D3 * t32 * t222
      t229 = x2 * t144
      t233 = log(0.4D1 * t229 * t14 * t211)
      t235 = t233 ** 2
      t238 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t241 = log(0.4D1 * t229 * t14)
      t243 = t241 ** 2
      t261 = log(0.4D1 * t11 * t169)
      t277 = log(0.4D1 * t48 * t169)
      t281 = log(0.4D1 * t48 * t212)
      t292 = log(0.4D1 * t61 * t144)
      t294 = t292 ** 2
      t308 = (0.90D2 * t8 * (t148 * t149 - t151 * t152 / 0.2D1 - t155) -
     # 0.180D3 * t32 * 0.3141592653589793D1 * (-t149 + t148 * t152) - t1
     #65) * t37 / 0.1440D4 - (0.180D3 * t171 * lh + 0.45D2 * t174 + t79 
     #- t81) * t7 * 0.3141592653589793D1 * t149 / 0.1440D4 - t8 * t181 /
     # 0.16D2 - (-0.90D2 * t174 * lh + 0.60D2 * lh * t80 - 0.28849365675
     #83026D3 - 0.120D3 * t78 * lh - 0.15D2 * t174 * t171 - t171 * t82) 
     #* t7 * t164 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t171) * t7 * 0.
     #3141592653589793D1 * t155 / 0.1440D4 - (0.90D2 * t8 * (t149 - t207
     # * t152 - t209 + t215 * t216) - t224) * t37 * t43 / 0.1440D4 - (0.
     #90D2 * t8 * (t233 * t209 - t235 * t216 / 0.2D1 - t238 - t241 * t14
     #9 + t243 * t152 / 0.2D1 + t155) - 0.180D3 * t32 * 0.31415926535897
     #93D1 * (-t209 + t233 * t216 + t149 - t241 * t152) + t83 * t222) * 
     #t43 / 0.1440D4 + (0.90D2 * t8 * (-t149 + t261 * t152) + 0.180D3 * 
     #t32 * t164) * t37 * t39 / 0.720D3 - t8 * t221 * t45 / 0.8D1 - (0.9
     #0D2 * t8 * (t149 - t277 * t152 - t209 + t281 * t216) - t224) * t43
     # * t39 / 0.720D3 + (0.90D2 * t8 * (t292 * t149 - t294 * t152 / 0.2
     #D1 - t155) - 0.180D3 * t32 * 0.3141592653589793D1 * (-t149 + t292 
     #* t152) - t165) * t39 / 0.720D3
      t309 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t308)
      t311 = t2 * t91
      t312 = t2 * x3
      t316 = log(-0.4D1 * t145 * t14 * t91)
      t317 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #101, x4)
      t319 = t316 ** 2
      t320 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #101, x4)
      t323 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #101, x4)
      t332 = 0.3141592653589793D1 * t320
      t336 = (0.90D2 * t8 * (-t316 * t317 + t319 * t320 / 0.2D1 + t323) 
     #- 0.180D3 * t32 * 0.3141592653589793D1 * (t317 - t316 * t320) + t8
     #3 * t332) * t37 / 0.1440D4
      t341 = log(-0.4D1 * t229 * t14 * x3 * t91)
      t343 = t317 - t341 * t320
      t347 = 0.180D3 * t32 * t332
      t355 = log(-0.4D1 * t11 * t169 * t91)
      t363 = (0.90D2 * t8 * (t317 - t355 * t320) - t347) * t37 * t39 / 0
     #.720D3
      t366 = t8 * t320 * t45 / 0.8D1
      t367 = t336 - (-0.90D2 * t8 * t343 + t347) * t37 * t43 / 0.1440D4 
     #+ t363 + t366
      t368 = FJET(XB1, XB2, s, -t311, 0.0D0, t312, 0.0D0, 0.0D0, t367)
      t370 = t204 - 0.1D1
      t371 = 0.1D1 / t370
      t372 = t91 * t371
      t373 = t2 * t372
      t375 = x3 * t210 * t371
      t376 = t2 * t375
      t377 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t372
     #, x4)
      t378 = cos(t12)
      t379 = x3 * z
      t380 = x2 * t91
      t382 = Sqrt(-t379 * t380)
      t386 = 0.1D1 / (-z - t204 + 0.2D1 * t378 * t382)
      t387 = t377 * t386
      t394 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t372
     #, x4)
      t397 = t370 ** 2
      t403 = log(-0.4D1 * t204 * t91 * t144 * t211 * t14 / t397)
      t418 = t8 * t387 * z * t37 * t43 * t39 / 0.8D1 - (-0.90D2 * t8 * (
     #t394 - t403 * t377) * t386 * z + 0.180D3 * t32 * 0.314159265358979
     #3D1 * t387 * z) * t37 * t43 / 0.1440D4
      t419 = FJET(XB1, XB2, s, t373, 0.0D0, t376, 0.0D0, 0.0D0, t418)
      t421 = FJET(XB1, XB2, s, 0.0D0, t373, 0.0D0, t376, 0.0D0, t418)
      t423 = FJET(XB1, XB2, s, 0.0D0, t376, 0.0D0, t373, 0.0D0, t418)
      t432 = (-0.90D2 * t8 * t112 + t117) * t37 * t39 / 0.720D3 - t124
      t433 = FJET(XB1, XB2, s, t98, -t100, -t94, t96, 0.0D0, t432)
      t435 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t308)
      t438 = t210 * s * t93
      t439 = x2 * x1
      t441 = t2 * t439 * t20
      t442 = t1 ** 2
      t447 = s * t442 * x2 * x1 * t4 * t20
      t448 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t452 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t457 = log(-0.4D1 * t49 * t21 * t22 * t211)
      t469 = -t8 * t448 * t45 / 0.8D1 - (0.90D2 * t8 * (t452 - t457 * t4
     #48) - 0.180D3 * t32 * 0.3141592653589793D1 * t448) * t43 * t39 / 0
     #.720D3
      t470 = FJET(XB1, XB2, s, -t5, -t438, 0.0D0, -t441, t447, t469)
      t474 = Sqrt(x3 * t19 * t380)
      t475 = t378 * t474
      t480 = x2 ** 2
      t481 = x3 * t480
      t482 = t481 * t18
      t483 = t97 * z
      t484 = t204 * z
      t486 = t204 * x1
      t488 = t481 * x1
      t490 = 0.2D1 * t475
      t491 = 0.2D1 * t475 * x2 - 0.2D1 * t204 * t18 + t482 + t483 + 0.2D
     #1 * t484 + 0.2D1 * t486 - t488 - t481 * z - x2 - t379 - t97 - t490
     # + t204
      t494 = t3 * t491 * t20 * t371
      t495 = t5 * t375
      t500 = t3 * t210 * (-t204 - z + t379 - x1 + t97 + t18 - t483 + t49
     #0) * t20 * t371
      t502 = t92 * t95 * t371
      t503 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t372, x
     #4)
      t524 = 0.2D1 * x1 * t16 + 0.2D1 * t10 * z - t10 * t16 + t48 - 0.2D
     #1 * t475 * t439 - 0.2D1 * t475 * t18 + x3 * t16 * t439 + 0.2D1 * t
     #11 * x2 * z - t11 * t16 * x2 - t482 + 0.2D1 * t475 * z + 0.2D1 * t
     #475 * x1
      t525 = t439 * z
      t534 = t525 - t484 - t486 - t11 * x2 + t488 - t439 * t16 - 0.2D1 *
     # t48 * z + t48 * t16 + 0.2D1 * t475 * t525 - 0.2D1 * t18 - t16 - t
     #10
      t536 = 0.1D1 / (t524 + t534)
      t541 = (z + x1 + t525 - t18 - t439) * t19 * t45
      t543 = t8 * t503 * t536 * t541 / 0.8D1
      t544 = FJET(XB1, XB2, s, -t494, -t495, t500, -t502, t447, t543)
      t547 = 0.3141592653589793D1 * t503 * t536
      t551 = FJET(XB1, XB2, s, -t502, t500, -t495, -t494, t447, t543)
      t556 = t89 * t88 + t126 * t125 + t139 * t138 + t141 * t88 + t309 *
     # t308 + t368 * t367 + t419 * t418 + t421 * t418 + t423 * t418 + t4
     #33 * t432 + t435 * t308 + t470 * t469 + t544 * t7 * t547 * t541 / 
     #0.8D1 + t551 * t7 * t547 * t541 / 0.8D1
      t557 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t308)
      t559 = FJET(XB1, XB2, s, -t100, t98, t96, -t94, 0.0D0, t125)
      t561 = FJET(XB1, XB2, s, t500, -t502, -t494, -t495, t447, t543)
      t566 = FJET(XB1, XB2, s, -t495, -t494, -t502, t500, t447, t543)
      t578 = t336 - (-0.90D2 * t8 * t343 + t347) * t37 * t43 / 0.1440D4 
     #+ t363 + t366
      t579 = FJET(XB1, XB2, s, t312, 0.0D0, -t311, 0.0D0, 0.0D0, t578)
      t581 = FJET(XB1, XB2, s, 0.0D0, -t441, -t5, -t438, t447, t469)
      t583 = FJET(XB1, XB2, s, t376, 0.0D0, t373, 0.0D0, 0.0D0, t418)
      t585 = FJET(XB1, XB2, s, -t438, -t5, -t441, 0.0D0, t447, t469)
      t587 = FJET(XB1, XB2, s, t96, -t94, -t100, t98, 0.0D0, t432)
      t589 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t138)
      t591 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t308)
      t593 = FJET(XB1, XB2, s, -t441, 0.0D0, -t438, -t5, t447, t469)
      t595 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t312, 0.0D0, t578)
      t597 = FJET(XB1, XB2, s, 0.0D0, t312, 0.0D0, -t311, 0.0D0, t578)
      t599 = t557 * t308 + t559 * t125 + t561 * t7 * t547 * t541 / 0.8D1
     # + t566 * t7 * t547 * t541 / 0.8D1 + t579 * t578 + t581 * t469 + t
     #583 * t418 + t585 * t469 + t587 * t432 + t589 * t138 + t591 * t308
     # + t593 * t469 + t595 * t578 + t597 * t578
      rrgg2qqbarht7s4e0 = t556 + t599

      end function



      doubleprecision function rrgg2qqbarht7s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #10D1, x4)
      t22 = lh * t4
      t23 = 0.3141592653589793D1 * t17
      t25 = 0.180D3 * t22 * t23
      t27 = 0.1D1 / x3
      t33 = log(0.4D1 * t9 * t13)
      t42 = t33 ** 2
      t44 = lh ** 2
      t46 = 0.3141592653589793D1 ** 2
      t52 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #10D1, x4)
      t55 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t56 = -t55 + t17
      t58 = 0.1D1 / x2
      t62 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t63 = x2 * t9
      t64 = -0.1D1 + x2
      t65 = t64 ** 2
      t69 = log(0.4D1 * t63 * t13 * t65)
      t73 = log(0.4D1 * t63 * t13)
      t85 = 0.1D1 / x1
      t89 = x1 ** 2
      t90 = t89 * t13
      t93 = log(0.4D1 * t90 * t9)
      t105 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 - 
     #(-0.180D3 * lh - 0.90D2 * t33) * t4 * 0.3141592653589793D1 * t6 / 
     #0.1440D4 - (0.180D3 * t33 * lh + 0.45D2 * t42 + 0.180D3 * t44 - 0.
     #30D2 * t46) * t4 * t23 / 0.1440D4 - t5 * t52 / 0.16D2 - t5 * t56 *
     # t27 * t58 / 0.16D2 - (0.90D2 * t5 * (-t62 + t69 * t55 + t6 - t73 
     #* t17) - 0.180D3 * t22 * 0.3141592653589793D1 * t56) * t58 / 0.144
     #0D4 - t5 * t56 * t58 * t85 / 0.8D1 + (0.90D2 * t5 * (-t6 + t93 * t
     #17) + t25) * t85 / 0.720D3 - t5 * t17 * t27 * t85 / 0.8D1
      t106 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t105)
      t108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t105)
      t110 = t2 * x1
      t111 = -0.1D1 + x1
      t112 = t2 * t111
      t113 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10
     #D1, x4)
      t117 = t5 * t113 * t58 * t85 / 0.8D1
      t118 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10
     #D1, x4)
      t122 = 0.1D1 / (-z - x1 + x1 * z)
      t124 = t111 ** 2
      t128 = log(-0.4D1 * t90 / t7 * t122 * t124)
      t130 = t118 - t128 * t113
      t135 = 0.180D3 * t22 * 0.3141592653589793D1 * t113
      t142 = t5 * t113 * t27 * t85 / 0.8D1
      t143 = t117 + (0.90D2 * t5 * t130 - t135) * t85 / 0.720D3 + t142
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t110, -t112, 0.0D0, t143)
      t146 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t112, t110, 0.0D0, t143)
      t148 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t105)
      t150 = t2 * x3
      t151 = -0.1D1 + x3
      t152 = t2 * t151
      t153 = -t151
      t154 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #153, x4)
      t155 = t154 * t27
      t159 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #153, x4)
      t163 = log(-0.4D1 * t10 * t13 * t151)
      t177 = t5 * t155 * t58 / 0.16D2 + (0.90D2 * t5 * (t159 - t163 * t1
     #54) - 0.180D3 * t22 * 0.3141592653589793D1 * t154) * t27 / 0.1440D
     #4 + t5 * t155 * t85 / 0.8D1
      t178 = FJET(XB1, XB2, s, 0.0D0, t150, 0.0D0, -t152, 0.0D0, t177)
      t180 = x2 * x3
      t182 = 0.1D1 / (t180 - 0.1D1)
      t183 = t151 * t182
      t184 = t2 * t183
      t187 = t2 * x3 * t64 * t182
      t188 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t183
     #, x4)
      t190 = cos(t11)
      t194 = Sqrt(-x3 * z * x2 * t151)
      t201 = 0.1D1 / (-z - t180 + 0.2D1 * t190 * t194) * z * t27 * t58
      t203 = t5 * t188 * t201 / 0.16D2
      t204 = FJET(XB1, XB2, s, 0.0D0, t184, 0.0D0, t187, 0.0D0, t203)
      t206 = 0.3141592653589793D1 * t188
      t210 = FJET(XB1, XB2, s, 0.0D0, t187, 0.0D0, t184, 0.0D0, t203)
      t215 = FJET(XB1, XB2, s, 0.0D0, -t152, 0.0D0, t150, 0.0D0, t177)
      t219 = t2 * x1 * x2 * t122
      t221 = t1 * x1
      t222 = t64 * s * t221
      t223 = t1 ** 2
      t228 = s * t223 * x2 * x1 * t111 * t122
      t229 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t231 = t229 * t58 * t85
      t233 = t5 * t231 / 0.8D1
      t234 = FJET(XB1, XB2, s, 0.0D0, -t219, -t112, -t222, t228, -t233)
      t239 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t105)
      t247 = t117 + (0.90D2 * t5 * t130 - t135) * t85 / 0.720D3 + t142
      t248 = FJET(XB1, XB2, s, t110, -t112, 0.0D0, 0.0D0, 0.0D0, t247)
      t250 = t106 * t105 + t108 * t105 + t144 * t143 + t146 * t143 + t14
     #8 * t105 + t178 * t177 + t204 * t4 * t206 * t201 / 0.16D2 + t210 *
     # t4 * t206 * t201 / 0.16D2 + t215 * t177 - t234 * t4 * 0.314159265
     #3589793D1 * t231 / 0.8D1 + t239 * t105 + t248 * t247
      t251 = FJET(XB1, XB2, s, t150, 0.0D0, -t152, 0.0D0, 0.0D0, t177)
      t254 = t2 * x1 * x3
      t256 = t2 * t111 * x3
      t257 = t151 * s
      t258 = t257 * t221
      t260 = t257 * t1 * t111
      t261 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t153
     #, x4)
      t263 = t261 * t27 * t85
      t265 = t5 * t263 / 0.8D1
      t266 = FJET(XB1, XB2, s, t254, -t256, -t258, t260, 0.0D0, -t265)
      t271 = FJET(XB1, XB2, s, t260, -t258, -t256, t254, 0.0D0, -t265)
      t276 = FJET(XB1, XB2, s, t184, 0.0D0, t187, 0.0D0, 0.0D0, t203)
      t281 = FJET(XB1, XB2, s, t187, 0.0D0, t184, 0.0D0, 0.0D0, t203)
      t286 = FJET(XB1, XB2, s, -t112, t110, 0.0D0, 0.0D0, 0.0D0, t247)
      t288 = FJET(XB1, XB2, s, -t112, -t222, 0.0D0, -t219, t228, -t233)
      t293 = FJET(XB1, XB2, s, -t152, 0.0D0, t150, 0.0D0, 0.0D0, t177)
      t295 = FJET(XB1, XB2, s, -t256, t254, t260, -t258, 0.0D0, -t265)
      t300 = FJET(XB1, XB2, s, -t222, -t112, -t219, 0.0D0, t228, -t233)
      t305 = FJET(XB1, XB2, s, -t258, t260, t254, -t256, 0.0D0, -t265)
      t310 = FJET(XB1, XB2, s, -t219, 0.0D0, -t222, -t112, t228, -t233)
      t315 = t251 * t177 - t266 * t4 * 0.3141592653589793D1 * t263 / 0.8
     #D1 - t271 * t4 * 0.3141592653589793D1 * t263 / 0.8D1 + t276 * t4 *
     # t206 * t201 / 0.16D2 + t281 * t4 * t206 * t201 / 0.16D2 + t286 * 
     #t247 - t288 * t4 * 0.3141592653589793D1 * t231 / 0.8D1 + t293 * t1
     #77 - t295 * t4 * 0.3141592653589793D1 * t263 / 0.8D1 - t300 * t4 *
     # 0.3141592653589793D1 * t231 / 0.8D1 - t305 * t4 * 0.3141592653589
     #793D1 * t263 / 0.8D1 - t310 * t4 * 0.3141592653589793D1 * t231 / 0
     #.8D1
      rrgg2qqbarht7s4em1 = t250 + t315

      end function



      doubleprecision function rrgg2qqbarht7s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * 0.3141592653589793D1
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t7 = 0.1D1 / x3
      t11 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t17 = 0.1D1 / x1
      t21 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #10D1, x4)
      t25 = z ** 2
      t29 = Sin(x4 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t33 = log(0.4D1 / t25 / z * t30)
      t40 = -t5 * t6 * t7 / 0.16D2 - t5 * (-t11 + t6) / x2 / 0.16D2 - t5
     # * t6 * t17 / 0.8D1 - t5 * t21 / 0.16D2 - (-0.180D3 * lh - 0.90D2 
     #* t33) * t4 * 0.3141592653589793D1 * t6 / 0.1440D4
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t40)
      t43 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t40)
      t45 = t2 * x1
      t47 = t2 * (-0.1D1 + x1)
      t48 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t51 = t5 * t48 * t17 / 0.8D1
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t45, -t47, 0.0D0, t51)
      t55 = 0.3141592653589793D1 * t48 * t17
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t47, t45, 0.0D0, t51)
      t62 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t40)
      t64 = t2 * x3
      t65 = -0.1D1 + x3
      t66 = t2 * t65
      t68 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, -t
     #65, x4)
      t71 = t5 * t68 * t7 / 0.16D2
      t72 = FJET(XB1, XB2, s, 0.0D0, t64, 0.0D0, -t66, 0.0D0, t71)
      t75 = 0.3141592653589793D1 * t68 * t7
      t78 = FJET(XB1, XB2, s, 0.0D0, -t66, 0.0D0, t64, 0.0D0, t71)
      t82 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t40)
      t84 = FJET(XB1, XB2, s, t45, -t47, 0.0D0, 0.0D0, 0.0D0, t51)
      t88 = FJET(XB1, XB2, s, t64, 0.0D0, -t66, 0.0D0, 0.0D0, t71)
      t92 = FJET(XB1, XB2, s, -t66, 0.0D0, t64, 0.0D0, 0.0D0, t71)
      t96 = FJET(XB1, XB2, s, -t47, t45, 0.0D0, 0.0D0, 0.0D0, t51)
      rrgg2qqbarht7s4em2 = t41 * t40 + t43 * t40 + t52 * t4 * t55 / 0.8D
     #1 + t58 * t4 * t55 / 0.8D1 + t62 * t40 + t72 * t4 * t75 / 0.16D2 +
     # t78 * t4 * t75 / 0.16D2 + t82 * t40 + t84 * t4 * t55 / 0.8D1 + t8
     #8 * t4 * t75 / 0.16D2 + t92 * t4 * t75 / 0.16D2 + t96 * t4 * t55 /
     # 0.8D1

      end function



      doubleprecision function rrgg2qqbarht7s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t8 = t4 * 0.3141592653589793D1 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = 0.3141592653589793D1 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s4em3 = -t9 * t4 * t11 / 0.16D2 - t13 * t4 * t11 / 0.
     #16D2 - t16 * t4 * t11 / 0.16D2 - t19 * t4 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6
      rrgg2qqbarht7s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh71J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x3 ** 2
      t13 = 0.1D1 - x3
      t18 = t13 ** 2
      t19 = t18 * t13
      t23 = x1 ** 2
      t24 = t23 ** 2
      t25 = t6 * t24
      t27 = z + x1 * t3
      t28 = t27 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / t29
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t37 = x3 * t31
      t41 = Sqrt(t37 * t27 * x2 * t13)
      t43 = 0.2D1 * t36 * t41
      t44 = t13 * t31 * t27 + x2 * x3 + t43
      t45 = t44 ** 2
      t50 = t37 * t27 + x2 * t13 - t43
      t55 = t50 ** 2
      t56 = t55 * t50
      t61 = t2 * t4 * t3
      t62 = t23 * x1
      t63 = t61 * t62
      t65 = 0.1D1 / t28 / t27
      t70 = t2 * t4
      t72 = 0.1D1 / t28
      t81 = t2 * t3
      t92 = t8 * t7
      t115 = 0.4D1 * t6 * t9 * t10 * x3 * t13 + 0.4D1 * t6 * t9 * x3 * t
     #19 + 0.4D1 * t25 * t30 * t45 * t44 * t50 + 0.4D1 * t25 * t30 * t44
     # * t56 + 0.7D1 * t63 * t65 * t50 * t45 - 0.6D1 * t70 * t23 * t72 *
     # t50 * t44 - 0.4D1 * t63 * t65 * t55 * t44 + 0.2D1 * t81 * t7 * t1
     #3 - 0.6D1 * t70 * t8 * x3 * t13 + 0.2D1 * t70 * t8 * t18 + t61 * t
     #92 * t19 + t61 * t62 * t65 * t56 + 0.2D1 * t81 * x1 / t27 * t50 + 
     #0.2D1 * t70 * t23 * t72 * t55 + 0.7D1 * t61 * t92 * t13 * t10 - 0.
     #4D1 * t61 * t92 * t18 * x3
      rrgg2qqbarh71J1 = 0.3D1 / 0.8D1 * wd * t115 * nf / t1 / z / 0.3141
     #592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x3 ** 2
      t13 = 0.1D1 - x3
      t16 = 0.4D1 * t6 * t9 * t10 * x3 * t13
      t18 = t13 ** 2
      t19 = t18 * t13
      t22 = 0.4D1 * t6 * t9 * x3 * t19
      t23 = x1 ** 2
      t24 = t23 ** 2
      t25 = t6 * t24
      t27 = z + x1 * t3
      t28 = t27 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / t29
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t37 = x3 * t31
      t41 = Sqrt(t37 * t27 * x2 * t13)
      t43 = 0.2D1 * t36 * t41
      t44 = t13 * t31 * t27 + x2 * x3 + t43
      t45 = t44 ** 2
      t50 = t37 * t27 + x2 * t13 - t43
      t53 = 0.4D1 * t25 * t30 * t45 * t44 * t50
      t55 = t50 ** 2
      t56 = t55 * t50
      t59 = 0.4D1 * t25 * t30 * t44 * t56
      t61 = t2 * t4 * t3
      t62 = t23 * x1
      t63 = t61 * t62
      t65 = 0.1D1 / t28 / t27
      t70 = t2 * t4
      t72 = 0.1D1 / t28
      t81 = t2 * t3
      t84 = 0.2D1 * t81 * t7 * t13
      t85 = t8 * x3
      t92 = t8 * t7
      t95 = t62 * t65
      t98 = 0.1D1 / t27
      t102 = 0.2D1 * t81 * x1 * t98 * t50
      t103 = t23 * t72
      t115 = t16 + t22 + t53 + t59 + 0.7D1 * t63 * t65 * t50 * t45 - 0.6
     #D1 * t70 * t23 * t72 * t50 * t44 - 0.4D1 * t63 * t65 * t55 * t44 +
     # t84 - 0.6D1 * t70 * t85 * t13 + 0.2D1 * t70 * t8 * t18 + t61 * t9
     #2 * t19 + t61 * t95 * t56 + t102 + 0.2D1 * t70 * t103 * t55 + 0.7D
     #1 * t61 * t92 * t13 * t10 - 0.4D1 * t61 * t92 * t18 * x3
      t125 = t98 * t44
      t132 = t13 * t23 * t72 * t45
      t138 = t13 * x1 * t125
      t152 = t98 * t50
      t153 = t7 * x3
      t158 = t8 * t10
      t162 = t6 * t95
      t176 = -0.8D1 * t6 * t9 * t10 * t18 + 0.4D1 * t6 * t92 * x3 * t18 
     #* x1 * t125 - t16 - t22 + 0.4D1 * t6 * t85 * t132 + 0.4D1 * t6 * t
     #92 * t10 * t138 - t59 - 0.8D1 * t25 * t30 * t45 * t55 - t102 - t53
     # - t84 - 0.2D1 * t70 * t7 * t138 + 0.2D1 * t61 * t7 * t132 - 0.2D1
     # * t70 * x1 * t152 * t153 + 0.2D1 * t61 * x1 * t152 * t158 + 0.4D1
     # * t162 * t44 * t55 * t153 + 0.4D1 * t6 * t103 * t44 * t50 * t158 
     #+ 0.4D1 * t162 * t45 * t50 * t153
      rrgg2qqbarh71J2 = 0.3D1 / 0.8D1 * (0.2D1 * wd * t115 + wd * t176) 
     #* nf / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x3 ** 2
      t13 = 0.1D1 - x3
      t16 = 0.4D1 * t6 * t9 * t10 * x3 * t13
      t18 = t13 ** 2
      t19 = t18 * t13
      t22 = 0.4D1 * t6 * t9 * x3 * t19
      t23 = x1 ** 2
      t24 = t23 ** 2
      t25 = t6 * t24
      t27 = z + x1 * t3
      t28 = t27 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / t29
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t37 = x3 * t31
      t41 = Sqrt(t37 * t27 * x2 * t13)
      t43 = 0.2D1 * t36 * t41
      t44 = t13 * t31 * t27 + x2 * x3 + t43
      t45 = t44 ** 2
      t50 = t37 * t27 + x2 * t13 - t43
      t53 = 0.4D1 * t25 * t30 * t45 * t44 * t50
      t55 = t50 ** 2
      t56 = t55 * t50
      t59 = 0.4D1 * t25 * t30 * t44 * t56
      t61 = t2 * t4 * t3
      t62 = t23 * x1
      t63 = t61 * t62
      t65 = 0.1D1 / t28 / t27
      t70 = t2 * t4
      t72 = 0.1D1 / t28
      t81 = t2 * t3
      t84 = 0.2D1 * t81 * t7 * t13
      t85 = t8 * x3
      t92 = t8 * t7
      t95 = t62 * t65
      t98 = 0.1D1 / t27
      t102 = 0.2D1 * t81 * x1 * t98 * t50
      t103 = t23 * t72
      t115 = t16 + t22 + t53 + t59 + 0.7D1 * t63 * t65 * t50 * t45 - 0.6
     #D1 * t70 * t23 * t72 * t50 * t44 - 0.4D1 * t63 * t65 * t55 * t44 +
     # t84 - 0.6D1 * t70 * t85 * t13 + 0.2D1 * t70 * t8 * t18 + t61 * t9
     #2 * t19 + t61 * t95 * t56 + t102 + 0.2D1 * t70 * t103 * t55 + 0.7D
     #1 * t61 * t92 * t13 * t10 - 0.4D1 * t61 * t92 * t18 * x3
      t125 = t98 * t44
      t132 = t13 * t23 * t72 * t45
      t138 = t13 * x1 * t125
      t152 = t98 * t50
      t153 = t7 * x3
      t158 = t8 * t10
      t162 = t6 * t95
      t176 = -0.8D1 * t6 * t9 * t10 * t18 + 0.4D1 * t6 * t92 * x3 * t18 
     #* x1 * t125 - t16 - t22 + 0.4D1 * t6 * t85 * t132 + 0.4D1 * t6 * t
     #92 * t10 * t138 - t59 - 0.8D1 * t25 * t30 * t45 * t55 - t102 - t53
     # - t84 - 0.2D1 * t70 * t7 * t138 + 0.2D1 * t61 * t7 * t132 - 0.2D1
     # * t70 * x1 * t152 * t153 + 0.2D1 * t61 * x1 * t152 * t158 + 0.4D1
     # * t162 * t44 * t55 * t153 + 0.4D1 * t6 * t103 * t44 * t50 * t158 
     #+ 0.4D1 * t162 * t45 * t50 * t153
      rrgg2qqbarh71J3 = 0.3D1 / 0.8D1 * (0.3D1 * wd * t115 + 0.2D1 * wd 
     #* t176) * nf / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x3 ** 2
      t13 = 0.1D1 - x3
      t16 = 0.4D1 * t6 * t9 * t10 * x3 * t13
      t18 = t13 ** 2
      t19 = t18 * t13
      t22 = 0.4D1 * t6 * t9 * x3 * t19
      t23 = x1 ** 2
      t24 = t23 ** 2
      t25 = t6 * t24
      t27 = z + x1 * t3
      t28 = t27 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / t29
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t37 = x3 * t31
      t41 = Sqrt(t37 * t27 * x2 * t13)
      t43 = 0.2D1 * t36 * t41
      t44 = t13 * t31 * t27 + x2 * x3 + t43
      t45 = t44 ** 2
      t50 = t37 * t27 + x2 * t13 - t43
      t53 = 0.4D1 * t25 * t30 * t45 * t44 * t50
      t55 = t50 ** 2
      t56 = t55 * t50
      t59 = 0.4D1 * t25 * t30 * t44 * t56
      t61 = t2 * t4 * t3
      t62 = t23 * x1
      t63 = t61 * t62
      t65 = 0.1D1 / t28 / t27
      t70 = t2 * t4
      t72 = 0.1D1 / t28
      t81 = t2 * t3
      t84 = 0.2D1 * t81 * t7 * t13
      t85 = t8 * x3
      t92 = t8 * t7
      t95 = t62 * t65
      t98 = 0.1D1 / t27
      t102 = 0.2D1 * t81 * x1 * t98 * t50
      t103 = t23 * t72
      t115 = t16 + t22 + t53 + t59 + 0.7D1 * t63 * t65 * t50 * t45 - 0.6
     #D1 * t70 * t23 * t72 * t50 * t44 - 0.4D1 * t63 * t65 * t55 * t44 +
     # t84 - 0.6D1 * t70 * t85 * t13 + 0.2D1 * t70 * t8 * t18 + t61 * t9
     #2 * t19 + t61 * t95 * t56 + t102 + 0.2D1 * t70 * t103 * t55 + 0.7D
     #1 * t61 * t92 * t13 * t10 - 0.4D1 * t61 * t92 * t18 * x3
      t125 = t98 * t44
      t132 = t13 * t23 * t72 * t45
      t138 = t13 * x1 * t125
      t152 = t98 * t50
      t153 = t7 * x3
      t158 = t8 * t10
      t162 = t6 * t95
      t176 = -0.8D1 * t6 * t9 * t10 * t18 + 0.4D1 * t6 * t92 * x3 * t18 
     #* x1 * t125 - t16 - t22 + 0.4D1 * t6 * t85 * t132 + 0.4D1 * t6 * t
     #92 * t10 * t138 - t59 - 0.8D1 * t25 * t30 * t45 * t55 - t102 - t53
     # - t84 - 0.2D1 * t70 * t7 * t138 + 0.2D1 * t61 * t7 * t132 - 0.2D1
     # * t70 * x1 * t152 * t153 + 0.2D1 * t61 * x1 * t152 * t158 + 0.4D1
     # * t162 * t44 * t55 * t153 + 0.4D1 * t6 * t103 * t44 * t50 * t158 
     #+ 0.4D1 * t162 * t45 * t50 * t153
      rrgg2qqbarh71J4 = 0.3D1 / 0.8D1 * (0.4D1 * wd * t115 + 0.3D1 * wd 
     #* t176) * nf / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x3 ** 2
      t13 = 0.1D1 - x3
      t16 = 0.4D1 * t6 * t9 * t10 * x3 * t13
      t18 = t13 ** 2
      t19 = t18 * t13
      t22 = 0.4D1 * t6 * t9 * x3 * t19
      t23 = x1 ** 2
      t24 = t23 ** 2
      t25 = t6 * t24
      t27 = z + x1 * t3
      t28 = t27 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / t29
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t37 = x3 * t31
      t41 = Sqrt(t37 * t27 * x2 * t13)
      t43 = 0.2D1 * t36 * t41
      t44 = t13 * t31 * t27 + x2 * x3 + t43
      t45 = t44 ** 2
      t50 = t37 * t27 + x2 * t13 - t43
      t53 = 0.4D1 * t25 * t30 * t45 * t44 * t50
      t55 = t50 ** 2
      t56 = t55 * t50
      t59 = 0.4D1 * t25 * t30 * t44 * t56
      t61 = t2 * t4 * t3
      t62 = t23 * x1
      t63 = t61 * t62
      t65 = 0.1D1 / t28 / t27
      t70 = t2 * t4
      t72 = 0.1D1 / t28
      t81 = t2 * t3
      t84 = 0.2D1 * t81 * t7 * t13
      t85 = t8 * x3
      t92 = t8 * t7
      t95 = t62 * t65
      t98 = 0.1D1 / t27
      t102 = 0.2D1 * t81 * x1 * t98 * t50
      t103 = t23 * t72
      t115 = t16 + t22 + t53 + t59 + 0.7D1 * t63 * t65 * t50 * t45 - 0.6
     #D1 * t70 * t23 * t72 * t50 * t44 - 0.4D1 * t63 * t65 * t55 * t44 +
     # t84 - 0.6D1 * t70 * t85 * t13 + 0.2D1 * t70 * t8 * t18 + t61 * t9
     #2 * t19 + t61 * t95 * t56 + t102 + 0.2D1 * t70 * t103 * t55 + 0.7D
     #1 * t61 * t92 * t13 * t10 - 0.4D1 * t61 * t92 * t18 * x3
      t125 = t98 * t44
      t132 = t13 * t23 * t72 * t45
      t138 = t13 * x1 * t125
      t152 = t98 * t50
      t153 = t7 * x3
      t158 = t8 * t10
      t162 = t6 * t95
      t176 = -0.8D1 * t6 * t9 * t10 * t18 + 0.4D1 * t6 * t92 * x3 * t18 
     #* x1 * t125 - t16 - t22 + 0.4D1 * t6 * t85 * t132 + 0.4D1 * t6 * t
     #92 * t10 * t138 - t59 - 0.8D1 * t25 * t30 * t45 * t55 - t102 - t53
     # - t84 - 0.2D1 * t70 * t7 * t138 + 0.2D1 * t61 * t7 * t132 - 0.2D1
     # * t70 * x1 * t152 * t153 + 0.2D1 * t61 * x1 * t152 * t158 + 0.4D1
     # * t162 * t44 * t55 * t153 + 0.4D1 * t6 * t103 * t44 * t50 * t158 
     #+ 0.4D1 * t162 * t45 * t50 * t153
      rrgg2qqbarh71J5 = 0.3D1 / 0.8D1 * (0.5D1 * wd * t115 + 0.4D1 * wd 
     #* t176) * nf / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x3 ** 2
      t12 = 0.1D1 - x3
      t13 = t12 ** 2
      t17 = t8 * t7
      t22 = z + x1 * t3
      t23 = 0.1D1 / t22
      t24 = 0.1D1 - x2
      t29 = cos(x4 * 0.3141592653589793D1)
      t30 = x3 * t24
      t34 = Sqrt(t30 * t22 * x2 * t12)
      t36 = 0.2D1 * t29 * t34
      t37 = t12 * t24 * t22 + x2 * x3 + t36
      t38 = t23 * t37
      t54 = x1 ** 2
      t56 = t22 ** 2
      t57 = 0.1D1 / t56
      t58 = t37 ** 2
      t60 = t12 * t54 * t57 * t58
      t66 = t12 * x1 * t38
      t69 = t54 ** 2
      t70 = t6 * t69
      t71 = t56 ** 2
      t72 = 0.1D1 / t71
      t76 = t30 * t22 + x2 * t12 - t36
      t77 = t76 ** 2
      t86 = t2 * t3
      t99 = t2 * t4
      t104 = t2 * t4 * t3
      t109 = t23 * t76
      t110 = t7 * x3
      t115 = t8 * t10
      t123 = t6 * t54 * x1 / t56 / t22
      t138 = -0.8D1 * t6 * t9 * t10 * t13 + 0.4D1 * t6 * t17 * x3 * t13 
     #* x1 * t38 - 0.4D1 * t6 * t9 * t10 * x3 * t12 - 0.4D1 * t6 * t9 * 
     #x3 * t13 * t12 + 0.4D1 * t6 * t8 * x3 * t60 + 0.4D1 * t6 * t17 * t
     #10 * t66 - 0.4D1 * t70 * t72 * t37 * t77 * t76 - 0.8D1 * t70 * t72
     # * t58 * t77 - 0.2D1 * t86 * x1 * t23 * t76 - 0.4D1 * t70 * t72 * 
     #t58 * t37 * t76 - 0.2D1 * t86 * t7 * t12 - 0.2D1 * t99 * t7 * t66 
     #+ 0.2D1 * t104 * t7 * t60 - 0.2D1 * t99 * x1 * t109 * t110 + 0.2D1
     # * t104 * x1 * t109 * t115 + 0.4D1 * t123 * t37 * t77 * t110 + 0.4
     #D1 * t6 * t54 * t57 * t37 * t76 * t115 + 0.4D1 * t123 * t58 * t76 
     #* t110
      rrgg2qqbarh71J6 = 0.15D2 / 0.8D1 * wd * t138 * nf / t1 / z / 0.314
     #1592653589793D1

      end function
  
 