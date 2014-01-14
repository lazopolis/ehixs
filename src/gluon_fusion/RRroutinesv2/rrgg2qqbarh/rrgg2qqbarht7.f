      subroutine rrgg2qqbarht7
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt7
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt7
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhard71J1  
      doubleprecision rrgg2qqbarhhard71J2  
      doubleprecision rrgg2qqbarhhard71J3  
      doubleprecision rrgg2qqbarhhard71J4  
      doubleprecision rrgg2qqbarhhard71J5  
      doubleprecision rrgg2qqbarhhard71J6  
      doubleprecision rrgg2qqbarhhardt7s1e1  
      doubleprecision rrgg2qqbarhhardt7s1e0  
      doubleprecision rrgg2qqbarhhardt7s1em1  
      doubleprecision rrgg2qqbarhhardt7s1em2  
      doubleprecision rrgg2qqbarhhardt7s1em3  
      doubleprecision rrgg2qqbarhhardt7s1em4  
      doubleprecision rrgg2qqbarhhardt7s2e1  
      doubleprecision rrgg2qqbarhhardt7s2e0  
      doubleprecision rrgg2qqbarhhardt7s2em1  
      doubleprecision rrgg2qqbarhhardt7s2em2  
      doubleprecision rrgg2qqbarhhardt7s2em3  
      doubleprecision rrgg2qqbarhhardt7s2em4  
      doubleprecision rrgg2qqbarhhardt7s3e1  
      doubleprecision rrgg2qqbarhhardt7s3e0  
      doubleprecision rrgg2qqbarhhardt7s3em1  
      doubleprecision rrgg2qqbarhhardt7s3em2  
      doubleprecision rrgg2qqbarhhardt7s3em3  
      doubleprecision rrgg2qqbarhhardt7s3em4  
      doubleprecision rrgg2qqbarhhardt7s4e1  
      doubleprecision rrgg2qqbarhhardt7s4e0  
      doubleprecision rrgg2qqbarhhardt7s4em1  
      doubleprecision rrgg2qqbarhhardt7s4em2  
      doubleprecision rrgg2qqbarhhardt7s4em3  
      doubleprecision rrgg2qqbarhhardt7s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarhhardt7s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarhhardt7s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarhhardt7s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarhhardt7s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarhhardt7s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarhhardt7s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarhhardt7s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarhhardt7s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarhhardt7s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarhhardt7s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2qqbarhhardt7s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2qqbarhhardt7s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt7s1e1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t6 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t7 = 0.90D2 * t6
      t8 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t9 = lh ** 2
      t10 = 0.180D3 * t9
      t11 = pi ** 2
      t12 = 0.30D2 * t11
      t13 = t10 - t12
      t17 = s ** 2
      t18 = 0.1D1 / t17
      t19 = x4 * pi
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = x3 * t21
      t23 = z ** 2
      t24 = 0.1D1 / t23
      t25 = -0.1D1 + x3
      t26 = 0.1D1 / t25
      t30 = log(-0.4D1 * t22 * t24 * t26)
      t32 = cos(t19)
      t34 = Sqrt(-x3 * t25)
      t39 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t34 * z)
      t43 = log(0.4D1 * t22 * t24)
      t48 = t43 ** 2
      t50 = t30 ** 2
      t66 = 0.60D2 * lh * t11 - 0.240D3 * zeta3 - 0.120D3 * t9 * lh
      t68 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t89 = 0.1D1 / x3
      t92 = t24 * t21
      t94 = log(0.4D1 * t92)
      t95 = t94 ** 2
      t98 = t95 * t94
      t113 = t11 ** 2
      t114 = t9 ** 2
      t128 = rrgg2qqbarhhard71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t130 = t95 ** 2
      t137 = pi * t18
      t138 = x1 ** 2
      t139 = x3 * t138
      t142 = log(0.4D1 * t139 * t92)
      t144 = t142 ** 2
      t147 = t92 * t26
      t150 = log(-0.4D1 * t139 * t147)
      t151 = t150 * z
      t153 = t150 ** 2
      t157 = z * t6
      t163 = lh * pi
      t165 = z * t3
      t173 = t13 * pi
      t177 = t18 * (z * t8 * t39 + t8)
      t181 = 0.1D1 / x1
      t184 = t138 * t21
      t185 = t184 * t24
      t187 = log(0.4D1 * t185)
      t192 = t187 ** 2
      t202 = t66 * pi
      t203 = t18 * t8
      t215 = x2 ** 2
      t216 = x3 * t215
      t217 = t216 * t138
      t220 = log(-0.4D1 * t217 * t147)
      t225 = t216 * t185
      t227 = log(0.4D1 * t225)
      t236 = 0.1D1 / x2
      t237 = t236 * t181
      t240 = t215 * t138
      t243 = log(0.4D1 * t240 * t92)
      t245 = t243 ** 2
      t261 = 0.180D3 * lh
      t264 = log(0.4D1 * t216 * t92)
      t270 = t264 ** 2
      t276 = log(-0.4D1 * t216 * t147)
      t283 = t276 ** 2
      t297 = -t13 * pi
      t298 = t215 * t21
      t301 = log(0.4D1 * t298 * t24)
      t306 = t301 ** 2
      t317 = -t66 * pi
      t329 = ((0.180D3 * t3 * lh - t7 - t8 * t13) * pi * t18 * (-t30 * z
     # * t39 - t43) - 0.90D2 * t8 * pi * t18 * (-t48 * t43 / 0.6D1 - t50
     # * t30 * z * t39 / 0.6D1) + (0.180D3 * t6 * lh - t8 * t66 - 0.90D2
     # * t68 - t3 * t13) * pi * t18 * (z * t39 + 0.1D1) + (0.180D3 * t8 
     #* lh - 0.90D2 * t3) * pi * t18 * (t50 * z * t39 / 0.2D1 + t48 / 0.
     #2D1)) * t89 / 0.2880D4 + (-0.180D3 * (-t95 * t3 / 0.2D1 - t68 + t9
     #8 * t8 / 0.6D1 + t94 * t6) * lh + (t94 * t3 - t95 * t8 / 0.2D1 - t
     #6) * t13 + (-t3 + t94 * t8) * t66 - t8 * (t113 + 0.60D2 * t114 + 0
     #.480D3 * lh * zeta3 - 0.60D2 * t9 * t11) + 0.15D2 * t98 * t3 - 0.4
     #5D2 * t95 * t6 + 0.90D2 * t94 * t68 - 0.90D2 * t128 - 0.15D2 / 0.4
     #D1 * t130 * t8) * pi * t18 / 0.2880D4 - (0.90D2 * t137 * (-t142 * 
     #t3 + t144 * t8 / 0.2D1 + t6 - (t151 * t3 - t153 * z * t8 / 0.2D1 -
     # t157) * t39) - 0.180D3 * t163 * t18 * (t3 - t142 * t8 - (-t165 + 
     #t151 * t8) * t39) + t173 * t177) * t89 * t181 / 0.1440D4 + (t173 *
     # t18 * (-t3 + t187 * t8) + 0.90D2 * t137 * (-t192 * t3 / 0.2D1 - t
     #68 + t192 * t187 * t8 / 0.6D1 + t187 * t6) - t202 * t203 - 0.180D3
     # * t163 * t18 * (t187 * t3 - t192 * t8 / 0.2D1 - t6)) * t181 / 0.1
     #440D4 - (0.90D2 * t137 * (-(-t165 + t220 * z * t8) * t39 + t3 - t2
     #27 * t8) - 0.180D3 * t163 * t177) * t89 * t237 / 0.720D3 + (0.90D2
     # * t137 * (t243 * t3 - t245 * t8 / 0.2D1 - t6) - 0.180D3 * t163 * 
     #t18 * (-t3 + t243 * t8) - t173 * t203) * t236 * t181 / 0.720D3 - t
     #137 * (-(t261 + 0.90D2 * t264) * t3 - (-0.180D3 * t264 * lh - 0.45
     #D2 * t270 - t10 + t12) * t8 + t7 - ((t261 + 0.90D2 * t276) * z * t
     #3 + (-0.180D3 * t276 * lh - 0.45D2 * t283 - t10 + t12) * z * t8 - 
     #0.90D2 * t157) * t39) * t89 * t236 / 0.1440D4 + (t297 * t18 * (t3 
     #- t301 * t8) - 0.90D2 * t137 * (t306 * t3 / 0.2D1 + t68 - t306 * t
     #301 * t8 / 0.6D1 - t301 * t6) + t317 * t203 + 0.180D3 * t163 * t18
     # * (-t301 * t3 + t306 * t8 / 0.2D1 + t6)) * t236 / 0.1440D4
      t330 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t329)
      t332 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t329)
      t334 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t329)
      t336 = t2 * x1
      t337 = -0.1D1 + x1
      t338 = x1 * z
      t339 = 0.1D1 - x1 + t338
      t340 = 0.1D1 / t339
      t342 = t2 * t337 * t340
      t343 = t1 ** 2
      t344 = s * t343
      t346 = x1 * t337 * t340
      t347 = t344 * t346
      t348 = t139 * t21
      t349 = t24 * t340
      t350 = t337 ** 2
      t352 = t349 * t350 * t26
      t355 = log(-0.4D1 * t348 * t352)
      t356 = t355 * t339
      t357 = x3 * x1
      t358 = t357 * z
      t359 = 0.3D1 * t358
      t360 = 0.2D1 * t357
      t361 = x1 * t23
      t362 = x3 * t23
      t363 = t362 * x1
      t365 = 0.2D1 * t139 * z
      t366 = t139 * t23
      t367 = x3 * t339
      t369 = Sqrt(-t367 * t25)
      t373 = -z + t338 - t359 + t360 - x3 - t361 + t363 + t365 - t366 + 
     #0.2D1 * t32 * t369 * z - t139
      t374 = 0.1D1 / t373
      t375 = z * t374
      t376 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t342, t336, 0.0D0, -t347)
      t379 = t355 ** 2
      t381 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t342, t336, 0.0D0, -t347)
      t382 = t375 * t381
      t385 = t339 * z
      t386 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t342, t336, 0.0D0, -t347)
      t389 = t349 * t350
      t392 = log(0.4D1 * t348 * t389)
      t394 = t392 ** 2
      t401 = t385 * t374 * t376
      t411 = t18 * (-t385 * t374 * t381 - t381)
      t419 = log(0.4D1 * t184 * t389)
      t424 = t419 ** 2
      t427 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t342, t336, 0.0D0, -t347)
      t435 = t18 * t381
      t447 = t216 * t184
      t450 = log(-0.4D1 * t447 * t352)
      t453 = t340 * t350
      t457 = log(0.4D1 * t217 * t92 * t453)
      t468 = t240 * t21
      t471 = log(0.4D1 * t468 * t389)
      t473 = t471 ** 2
      t489 = -(0.90D2 * t137 * (t356 * t375 * t376 - t379 * t339 * t382 
     #/ 0.2D1 - t385 * t374 * t386 + t392 * t376 - t394 * t381 / 0.2D1 -
     # t386) - 0.180D3 * t163 * t18 * (-t401 + t356 * t382 - t376 + t392
     # * t381) + t173 * t411) * t89 * t181 / 0.1440D4 + (t173 * t18 * (t
     #376 - t419 * t381) + 0.90D2 * t137 * (t424 * t376 / 0.2D1 + t427 -
     # t424 * t419 * t381 / 0.6D1 - t419 * t386) + t202 * t435 - 0.180D3
     # * t163 * t18 * (-t419 * t376 + t424 * t381 / 0.2D1 + t386)) * t18
     #1 / 0.1440D4 - (0.90D2 * t137 * (-t401 + t450 * t339 * t382 - t376
     # + t457 * t381) - 0.180D3 * t163 * t411) * t89 * t237 / 0.720D3 + 
     #(0.90D2 * t137 * (-t471 * t376 + t473 * t381 / 0.2D1 + t386) - 0.1
     #80D3 * t163 * t18 * (t376 - t471 * t381) + t173 * t435) * t236 * t
     #181 / 0.720D3
      t490 = FJET(XB1, XB2, s, 0.0D0, t336, -t342, 0.0D0, -t347, t489)
      t493 = x2 * t1 * s
      t494 = -0.1D1 + x2
      t496 = t494 * t1 * s
      t497 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t493, -t
     #496, 0.0D0, 0.0D0, 0.0D0)
      t498 = t92 * t494
      t501 = log(-0.4D1 * t217 * t498)
      t502 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t493, -t
     #496, 0.0D0, 0.0D0, 0.0D0)
      t507 = t18 * t502
      t513 = (0.90D2 * t137 * (-t497 + t501 * t502) + 0.180D3 * t163 * t
     #507) * t89 * t237 / 0.720D3
      t516 = log(-0.4D1 * t240 * t498)
      t518 = t516 ** 2
      t521 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t493, -t
     #496, 0.0D0, 0.0D0, 0.0D0)
      t534 = (0.90D2 * t137 * (-t516 * t497 + t518 * t502 / 0.2D1 + t521
     #) - 0.180D3 * t163 * t18 * (t497 - t516 * t502) + t173 * t507) * t
     #236 * t181 / 0.720D3
      t537 = log(-0.4D1 * t216 * t498)
      t543 = t537 ** 2
      t552 = t137 * (-(-t261 - 0.90D2 * t537) * t497 - (0.180D3 * t537 *
     # lh + 0.45D2 * t543 + t10 - t12) * t502 - 0.90D2 * t521) * t89 * t
     #236 / 0.1440D4
      t553 = t24 * t494
      t556 = log(-0.4D1 * t298 * t553)
      t558 = -t497 + t556 * t502
      t561 = t556 ** 2
      t564 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, t493, -t
     #496, 0.0D0, 0.0D0, 0.0D0)
      t569 = -t561 * t497 / 0.2D1 - t564 + t561 * t556 * t502 / 0.6D1 + 
     #t556 * t521
      t572 = t317 * t507
      t576 = t556 * t497 - t561 * t502 / 0.2D1 - t521
      t583 = -t513 + t534 - t552 + (t297 * t18 * t558 - 0.90D2 * t137 * 
     #t569 - t572 + 0.180D3 * t163 * t18 * t576) * t236 / 0.1440D4
      t584 = FJET(XB1, XB2, s, 0.0D0, t493, 0.0D0, -t496, 0.0D0, t583)
      t586 = x2 * x3
      t589 = Sqrt(x3 * t494 * t25)
      t590 = t32 * t589
      t592 = 0.2D1 * t590 * x2
      t594 = 0.1D1 - x3 + t586
      t595 = 0.1D1 / t594
      t597 = t2 * (0.1D1 - x3 - x2 + t586 + t216 + t592) * t595
      t602 = t2 * x2 * (-0.1D1 + t586 + 0.2D1 * t590) * t595
      t603 = x2 * z
      t604 = t586 * z
      t605 = t216 * z
      t611 = 0.1D1 / (-t603 + z - t216 + x3 + x2 - t604 + t605 - 0.2D1 *
     # t590 * z + 0.2D1 * t590 * t603 - t592)
      t612 = t603 - z - x2
      t613 = t611 * t612
      t614 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t602, t
     #597, 0.0D0, 0.0D0, 0.0D0)
      t616 = t594 ** 2
      t617 = 0.1D1 / t616
      t619 = t553 * t25 * t617
      t622 = log(0.4D1 * t447 * t619)
      t624 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t602, t
     #597, 0.0D0, 0.0D0, 0.0D0)
      t630 = t163 * t18
      t643 = log(0.4D1 * t216 * t21 * t619)
      t644 = t643 * t611
      t649 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t602, t
     #597, 0.0D0, 0.0D0, 0.0D0)
      t654 = t643 ** 2
      t666 = -(0.90D2 * t137 * (-t613 * t614 + t622 * t611 * t612 * t624
     #) + 0.180D3 * t630 * t613 * t624) * t89 * t237 / 0.720D3 - t137 * 
     #(-(-0.180D3 * t611 * lh - 0.90D2 * t644) * t612 * t614 - 0.90D2 * 
     #t613 * t649 - (0.180D3 * t644 * lh + 0.45D2 * t654 * t611 + t611 *
     # t13) * t612 * t624) * t89 * t236 / 0.1440D4
      t667 = FJET(XB1, XB2, s, 0.0D0, t597, 0.0D0, -t602, 0.0D0, t666)
      t670 = t1 * t337
      t672 = t494 * s * t670 * t340
      t674 = x2 * s * t670
      t676 = t344 * t494 * t346
      t677 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t674, t
     #672, t336, 0.0D0, t676)
      t679 = t349 * t350 * t494
      t682 = log(-0.4D1 * t447 * t679)
      t683 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t674, t
     #672, t336, 0.0D0, t676)
      t688 = t18 * t683
      t693 = (0.90D2 * t137 * (t677 - t682 * t683) - 0.180D3 * t163 * t6
     #88) * t89 * t237
      t696 = log(-0.4D1 * t468 * t679)
      t698 = t696 ** 2
      t701 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t674, t
     #672, t336, 0.0D0, t676)
      t702 = t696 * t677 - t698 * t683 / 0.2D1 - t701
      t706 = -t677 + t696 * t683
      t710 = t173 * t688
      t715 = -t693 / 0.720D3 + (0.90D2 * t137 * t702 - 0.180D3 * t163 * 
     #t18 * t706 - t710) * t236 * t181 / 0.720D3
      t716 = FJET(XB1, XB2, s, 0.0D0, t672, t336, -t674, t676, t715)
      t718 = FJET(XB1, XB2, s, 0.0D0, -t496, 0.0D0, t493, 0.0D0, t583)
      t720 = FJET(XB1, XB2, s, 0.0D0, -t342, t336, 0.0D0, -t347, t489)
      t722 = FJET(XB1, XB2, s, 0.0D0, -t602, 0.0D0, t597, 0.0D0, t666)
      t724 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t329)
      t726 = FJET(XB1, XB2, s, t336, 0.0D0, 0.0D0, -t342, -t347, t489)
      t728 = t330 * t329 + t332 * t329 + t334 * t329 + t490 * t489 + t58
     #4 * t583 + t667 * t666 + t716 * t715 + t718 * t583 + t720 * t489 +
     # t722 * t666 + t724 * t329 + t726 * t489
      t729 = FJET(XB1, XB2, s, t336, -t674, 0.0D0, t672, t676, t715)
      t744 = -t513 + t534 - t552 + (t297 * t18 * t558 - 0.90D2 * t137 * 
     #t569 - t572 + 0.180D3 * t163 * t18 * t576) * t236 / 0.1440D4
      t745 = FJET(XB1, XB2, s, t493, 0.0D0, -t496, 0.0D0, 0.0D0, t744)
      t747 = FJET(XB1, XB2, s, t597, 0.0D0, -t602, 0.0D0, 0.0D0, t666)
      t749 = FJET(XB1, XB2, s, t672, 0.0D0, -t674, t336, t676, t715)
      t752 = t336 * t586 * t595
      t753 = t2 * t337
      t754 = t216 * x1
      t755 = t216 * t338
      t756 = t494 * t25
      t758 = Sqrt(t367 * t756)
      t759 = t32 * t758
      t761 = 0.2D1 * t759 * x2
      t765 = t753 * (-t754 + t216 - x2 + t586 + t755 + t761 + 0.1D1 - x3
     #) * t340 * t595
      t769 = t25 * s * t1 * x1 * t595
      t775 = t753 * x2 * (-0.1D1 + t586 + x1 - t357 - t338 + t358 + 0.2D
     #1 * t759) * t340 * t595
      t776 = x2 * x1
      t778 = x2 * t138
      t779 = t776 * z
      t786 = t338 + t360 - t361 - t139 + 0.2D1 * t776 - t778 - z - x3 + 
     #t604 - t605 - t754 + t761 - 0.3D1 * t779 - t586 * x1 + t139 * x2 +
     # t776 * t23 + 0.2D1 * t778 * z
      t803 = -t778 * t23 + 0.2D1 * t759 * z + 0.2D1 * t759 * t779 - t359
     # + t363 + t365 - t366 + t216 + t603 + t755 + 0.2D1 * t586 * t338 -
     # t362 * t776 - 0.2D1 * t139 * t603 + t139 * t23 * x2 - 0.2D1 * t75
     #9 * t603 - 0.2D1 * t759 * t776 - x2
      t805 = 0.1D1 / (t786 + t803)
      t806 = t339 * t805
      t807 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t775, -t
     #765, -t769, t752, t676)
      t813 = log(0.4D1 * t225 * t453 * t756 * t617)
      t815 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t775, -t
     #765, -t769, t752, t676)
      t819 = z - t603 + t779 + x2 - t776
      t827 = 0.90D2 * t137 * (t806 * t807 - t813 * t339 * t805 * t815) *
     # t819 - 0.180D3 * t630 * t806 * t815 * t819
      t830 = t827 * t89 * t237 / 0.720D3
      t831 = FJET(XB1, XB2, s, t752, -t765, -t769, t775, t676, -t830)
      t834 = t89 * t236 * t181
      t837 = FJET(XB1, XB2, s, t775, -t769, -t765, t752, t676, -t830)
      t841 = FJET(XB1, XB2, s, -t496, 0.0D0, t493, 0.0D0, 0.0D0, t744)
      t843 = FJET(XB1, XB2, s, -t342, 0.0D0, 0.0D0, t336, -t347, t489)
      t856 = -t693 / 0.720D3 + (0.90D2 * t137 * t702 - 0.180D3 * t163 * 
     #t18 * t706 - t710) * t236 * t181 / 0.720D3
      t857 = FJET(XB1, XB2, s, -t674, t336, t672, 0.0D0, t676, t856)
      t859 = FJET(XB1, XB2, s, -t602, 0.0D0, t597, 0.0D0, 0.0D0, t666)
      t861 = FJET(XB1, XB2, s, -t769, t775, t752, -t765, t676, -t830)
      t865 = FJET(XB1, XB2, s, -t765, t752, t775, -t769, t676, -t830)
      t869 = t729 * t715 + t745 * t744 + t747 * t666 + t749 * t715 - t83
     #1 * t827 * t834 / 0.720D3 - t837 * t827 * t834 / 0.720D3 + t841 * 
     #t744 + t843 * t489 + t857 * t856 + t859 * t666 - t861 * t827 * t83
     #4 / 0.720D3 - t865 * t827 * t834 / 0.720D3
      rrgg2qqbarhhardt7s1e1 = t728 + t869

      end function



      doubleprecision function rrgg2qqbarhhardt7s1e0
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = -0.1D1 + x3
      t14 = 0.1D1 / t13
      t18 = log(-0.4D1 * t10 * t12 * t14)
      t19 = t18 ** 2
      t21 = cos(t7)
      t23 = Sqrt(-x3 * t13)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t23 * z)
      t32 = log(0.4D1 * t10 * t12)
      t33 = t32 ** 2
      t41 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t42 = 0.90D2 * t41
      t52 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t54 = lh ** 2
      t56 = pi ** 2
      t58 = 0.180D3 * t54 - 0.30D2 * t56
      t67 = 0.1D1 / x3
      t70 = t12 * t9
      t72 = log(0.4D1 * t70)
      t74 = t72 ** 2
      t89 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t103 = pi * t6
      t104 = 0.180D3 * lh
      t105 = x2 ** 2
      t106 = t105 * x3
      t109 = log(0.4D1 * t106 * t70)
      t113 = z * t41
      t115 = t70 * t14
      t118 = log(-0.4D1 * t106 * t115)
      t127 = 0.1D1 / x2
      t131 = t105 * t9
      t134 = log(0.4D1 * t131 * t12)
      t136 = t134 ** 2
      t142 = lh * pi
      t149 = -t58 * pi
      t150 = t6 * t3
      t155 = x1 ** 2
      t156 = x3 * t155
      t159 = log(0.4D1 * t156 * t70)
      t163 = log(-0.4D1 * t156 * t115)
      t173 = z * t3 * t28 + t3
      t179 = 0.1D1 / x1
      t184 = t67 * t127 * t179
      t187 = t105 * t155
      t190 = log(0.4D1 * t187 * t70)
      t201 = t155 * t9
      t204 = log(0.4D1 * t201 * t12)
      t206 = t204 ** 2
      t217 = t58 * pi
      t222 = (-0.90D2 * t3 * pi * t6 * (t19 * z * t28 / 0.2D1 + t33 / 0.
     #2D1) + (0.180D3 * t3 * lh - t42) * pi * t6 * (-t18 * z * t28 - t32
     #) + (0.180D3 * t41 * lh - 0.90D2 * t52 - t3 * t58) * pi * t6 * (z 
     #* t28 + 0.1D1)) * t67 / 0.2880D4 + (-0.180D3 * (t72 * t41 - t74 * 
     #t3 / 0.2D1 - t52) * lh - t3 * (0.60D2 * lh * t56 - 0.240D3 * zeta3
     # - 0.120D3 * t54 * lh) - 0.45D2 * t74 * t41 - 0.90D2 * t89 + 0.15D
     #2 * t74 * t72 * t3 + 0.90D2 * t72 * t52 + (-t41 + t72 * t3) * t58)
     # * pi * t6 / 0.2880D4 - t103 * (t42 - (t104 + 0.90D2 * t109) * t3 
     #- (-0.90D2 * t113 + (t104 + 0.90D2 * t118) * z * t3) * t28) * t67 
     #* t127 / 0.1440D4 + (-0.90D2 * t103 * (-t134 * t41 + t136 * t3 / 0
     #.2D1 + t52) + 0.180D3 * t142 * t6 * (t41 - t134 * t3) + t149 * t15
     #0) * t127 / 0.1440D4 - (0.90D2 * t103 * (t41 - t159 * t3 - (-t113 
     #+ t163 * z * t3) * t28) - 0.180D3 * t142 * t6 * t173) * t67 * t179
     # / 0.1440D4 - t103 * t173 * t184 / 0.8D1 + (0.90D2 * t103 * (-t41 
     #+ t190 * t3) + 0.180D3 * t142 * t150) * t127 * t179 / 0.720D3 + (0
     #.90D2 * t103 * (t204 * t41 - t206 * t3 / 0.2D1 - t52) - 0.180D3 * 
     #t142 * t6 * (-t41 + t204 * t3) - t217 * t150) * t179 / 0.1440D4
      t223 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t222)
      t225 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t222)
      t227 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t222)
      t229 = t2 * x1
      t230 = -0.1D1 + x1
      t231 = x1 * z
      t232 = 0.1D1 - x1 + t231
      t233 = 0.1D1 / t232
      t235 = t2 * t230 * t233
      t236 = t1 ** 2
      t237 = s * t236
      t239 = x1 * t230 * t233
      t240 = t237 * t239
      t241 = t232 * z
      t242 = x3 * x1
      t243 = t242 * z
      t244 = 0.3D1 * t243
      t245 = 0.2D1 * t242
      t246 = x1 * t11
      t247 = x3 * t11
      t248 = t247 * x1
      t250 = 0.2D1 * t156 * z
      t251 = t156 * t11
      t252 = x3 * t232
      t254 = Sqrt(-t252 * t13)
      t258 = -z + t231 - t244 + t245 - x3 - t246 + t248 + t250 - t251 + 
     #0.2D1 * t21 * t254 * z - t156
      t259 = 0.1D1 / t258
      t260 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t235, t229, 0.0D0, -t240)
      t263 = t156 * t9
      t264 = t12 * t233
      t265 = t230 ** 2
      t270 = log(-0.4D1 * t263 * t264 * t265 * t14)
      t273 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t235, t229, 0.0D0, -t240)
      t276 = t264 * t265
      t279 = log(0.4D1 * t263 * t276)
      t286 = -t241 * t259 * t273 - t273
      t297 = t187 * t9
      t300 = log(0.4D1 * t297 * t276)
      t305 = t6 * t273
      t314 = log(0.4D1 * t201 * t276)
      t316 = t314 ** 2
      t319 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t235, t229, 0.0D0, -t240)
      t332 = -(0.90D2 * t103 * (-t241 * t259 * t260 + t270 * t232 * z * 
     #t259 * t273 - t260 + t279 * t273) - 0.180D3 * t142 * t6 * t286) * 
     #t67 * t179 / 0.1440D4 - t103 * t286 * t184 / 0.8D1 + (0.90D2 * t10
     #3 * (t260 - t300 * t273) - 0.180D3 * t142 * t305) * t127 * t179 / 
     #0.720D3 + (0.90D2 * t103 * (-t314 * t260 + t316 * t273 / 0.2D1 + t
     #319) - 0.180D3 * t142 * t6 * (t260 - t314 * t273) + t217 * t305) *
     # t179 / 0.1440D4
      t333 = FJET(XB1, XB2, s, 0.0D0, t229, -t235, 0.0D0, -t240, t332)
      t336 = x2 * t1 * s
      t337 = -0.1D1 + x2
      t339 = t337 * t1 * s
      t340 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t336, -t
     #339, 0.0D0, 0.0D0, 0.0D0)
      t342 = t70 * t337
      t345 = log(-0.4D1 * t106 * t342)
      t348 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t336, -t
     #339, 0.0D0, 0.0D0, 0.0D0)
      t354 = t103 * (-0.90D2 * t340 - (-t104 - 0.90D2 * t345) * t348) * 
     #t67 * t127 / 0.1440D4
      t355 = t12 * t337
      t358 = log(-0.4D1 * t131 * t355)
      t360 = t358 ** 2
      t363 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t336, -t
     #339, 0.0D0, 0.0D0, 0.0D0)
      t364 = t358 * t340 - t360 * t348 / 0.2D1 - t363
      t368 = -t340 + t358 * t348
      t372 = t6 * t348
      t373 = t149 * t372
      t379 = t103 * t348 * t184 / 0.8D1
      t382 = log(-0.4D1 * t187 * t342)
      t392 = (0.90D2 * t103 * (t340 - t382 * t348) - 0.180D3 * t142 * t3
     #72) * t127 * t179 / 0.720D3
      t393 = -t354 + (-0.90D2 * t103 * t364 + 0.180D3 * t142 * t6 * t368
     # - t373) * t127 / 0.1440D4 + t379 + t392
      t394 = FJET(XB1, XB2, s, 0.0D0, t336, 0.0D0, -t339, 0.0D0, t393)
      t396 = x2 * x3
      t399 = Sqrt(x3 * t337 * t13)
      t400 = t21 * t399
      t402 = 0.2D1 * t400 * x2
      t404 = 0.1D1 - x3 + t396
      t405 = 0.1D1 / t404
      t407 = t2 * (0.1D1 - x3 - x2 + t396 + t106 + t402) * t405
      t412 = t2 * x2 * (-0.1D1 + t396 + 0.2D1 * t400) * t405
      t413 = x2 * z
      t414 = t396 * z
      t415 = t106 * z
      t421 = 0.1D1 / (-t413 + z - t106 + x3 + x2 - t414 + t415 - 0.2D1 *
     # t400 * z + 0.2D1 * t400 * t413 - t402)
      t425 = t404 ** 2
      t431 = log(0.4D1 * t106 * t9 * t355 * t13 / t425)
      t435 = t413 - z - x2
      t437 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t412, t
     #407, 0.0D0, 0.0D0, 0.0D0)
      t439 = t421 * t435
      t440 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t412, t
     #407, 0.0D0, 0.0D0, 0.0D0)
      t454 = -t103 * (-(-0.180D3 * t421 * lh - 0.90D2 * t431 * t421) * t
     #435 * t437 - 0.90D2 * t439 * t440) * t67 * t127 / 0.1440D4 + t103 
     #* t439 * t437 * t67 * t127 * t179 / 0.8D1
      t455 = FJET(XB1, XB2, s, 0.0D0, t407, 0.0D0, -t412, 0.0D0, t454)
      t458 = t1 * t230
      t460 = t337 * s * t458 * t233
      t462 = x2 * s * t458
      t464 = t237 * t337 * t239
      t465 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t462, t
     #460, t229, 0.0D0, t464)
      t468 = t103 * t465 * t184 / 0.8D1
      t469 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t462, t
     #460, t229, 0.0D0, t464)
      t474 = log(-0.4D1 * t297 * t264 * t265 * t337)
      t476 = -t469 + t474 * t465
      t481 = 0.180D3 * t142 * t6 * t465
      t486 = -t468 + (0.90D2 * t103 * t476 + t481) * t127 * t179 / 0.720
     #D3
      t487 = FJET(XB1, XB2, s, 0.0D0, t460, t229, -t462, t464, t486)
      t489 = FJET(XB1, XB2, s, 0.0D0, -t339, 0.0D0, t336, 0.0D0, t393)
      t491 = FJET(XB1, XB2, s, 0.0D0, -t235, t229, 0.0D0, -t240, t332)
      t493 = FJET(XB1, XB2, s, 0.0D0, -t412, 0.0D0, t407, 0.0D0, t454)
      t495 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t222)
      t497 = FJET(XB1, XB2, s, t229, 0.0D0, 0.0D0, -t235, -t240, t332)
      t499 = t223 * t222 + t225 * t222 + t227 * t222 + t333 * t332 + t39
     #4 * t393 + t455 * t454 + t487 * t486 + t489 * t393 + t491 * t332 +
     # t493 * t454 + t495 * t222 + t497 * t332
      t500 = FJET(XB1, XB2, s, t229, -t462, 0.0D0, t460, t464, t486)
      t512 = -t354 + (-0.90D2 * t103 * t364 + 0.180D3 * t142 * t6 * t368
     # - t373) * t127 / 0.1440D4 + t379 + t392
      t513 = FJET(XB1, XB2, s, t336, 0.0D0, -t339, 0.0D0, 0.0D0, t512)
      t515 = FJET(XB1, XB2, s, t407, 0.0D0, -t412, 0.0D0, 0.0D0, t454)
      t517 = FJET(XB1, XB2, s, t460, 0.0D0, -t462, t229, t464, t486)
      t520 = t229 * t396 * t405
      t521 = t2 * t230
      t522 = t106 * x1
      t523 = t106 * t231
      t526 = Sqrt(t252 * t337 * t13)
      t527 = t21 * t526
      t529 = 0.2D1 * t527 * x2
      t533 = t521 * (-t522 + t106 - x2 + t396 + t523 + t529 + 0.1D1 - x3
     #) * t233 * t405
      t537 = t13 * s * t1 * x1 * t405
      t543 = t521 * x2 * (-0.1D1 + t396 + x1 - t242 - t231 + t243 + 0.2D
     #1 * t527) * t233 * t405
      t544 = x2 * x1
      t545 = t544 * z
      t552 = x2 * t155
      t560 = t414 - t415 - t244 + t248 + t250 - t251 + 0.2D1 * t527 * t5
     #45 + t529 - 0.3D1 * t545 - t396 * x1 + t156 * x2 + t544 * t11 + 0.
     #2D1 * t552 * z - t552 * t11 + 0.2D1 * t527 * z + t523 + 0.2D1 * t3
     #96 * t231
      t571 = -t247 * t544 - 0.2D1 * t156 * t413 + t156 * t11 * x2 - 0.2D
     #1 * t527 * t413 - 0.2D1 * t527 * t544 - t552 + t106 + t413 - t522 
     #+ 0.2D1 * t544 - x3 + t231 + t245 - t246 - t156 - z - x2
      t573 = 0.1D1 / (t560 + t571)
      t576 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t543, -t
     #533, -t537, t520, t464)
      t579 = t576 * (z - t413 + t545 + x2 - t544) * t184
      t581 = t103 * t232 * t573 * t579 / 0.8D1
      t582 = FJET(XB1, XB2, s, t520, -t533, -t537, t543, t464, -t581)
      t585 = t6 * t232 * t573
      t589 = FJET(XB1, XB2, s, t543, -t537, -t533, t520, t464, -t581)
      t594 = FJET(XB1, XB2, s, -t339, 0.0D0, t336, 0.0D0, 0.0D0, t512)
      t596 = FJET(XB1, XB2, s, -t235, 0.0D0, 0.0D0, t229, -t240, t332)
      t605 = -t468 + (0.90D2 * t103 * t476 + t481) * t127 * t179 / 0.720
     #D3
      t606 = FJET(XB1, XB2, s, -t462, t229, t460, 0.0D0, t464, t605)
      t608 = FJET(XB1, XB2, s, -t412, 0.0D0, t407, 0.0D0, 0.0D0, t454)
      t610 = FJET(XB1, XB2, s, -t537, t543, t520, -t533, t464, -t581)
      t615 = FJET(XB1, XB2, s, -t533, t520, t543, -t537, t464, -t581)
      t620 = t500 * t486 + t513 * t512 + t515 * t454 + t517 * t486 - t58
     #2 * pi * t585 * t579 / 0.8D1 - t589 * pi * t585 * t579 / 0.8D1 + t
     #594 * t512 + t596 * t332 + t606 * t605 + t608 * t454 - t610 * pi *
     # t585 * t579 / 0.8D1 - t615 * pi * t585 * t579 / 0.8D1
      rrgg2qqbarhhardt7s1e0 = t499 + t620

      end function



      doubleprecision function rrgg2qqbarhhardt7s1em1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = -0.1D1 + x3
      t18 = log(-0.4D1 * t10 * t12 / t13)
      t20 = cos(t7)
      t22 = Sqrt(-x3 * t13)
      t27 = 0.1D1 / (-z - x3 + 0.2D1 * t20 * t22 * z)
      t31 = log(0.4D1 * t10 * t12)
      t38 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t47 = 0.1D1 / x3
      t52 = log(0.4D1 * t12 * t9)
      t59 = t52 ** 2
      t62 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t64 = lh ** 2
      t66 = pi ** 2
      t74 = pi * t6
      t77 = z * t3 * t27 + t3
      t80 = 0.1D1 / x2
      t84 = x2 ** 2
      t85 = t84 * t9
      t88 = log(0.4D1 * t85 * t12)
      t93 = lh * pi
      t96 = 0.180D3 * t93 * t6 * t3
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t9
      t109 = log(0.4D1 * t106 * t12)
      t121 = (-0.90D2 * t4 * t6 * (-t18 * z * t27 - t31) + (0.180D3 * t3
     # * lh - 0.90D2 * t38) * pi * t6 * (z * t27 + 0.1D1)) * t47 / 0.288
     #0D4 + (-0.180D3 * (-t38 + t52 * t3) * lh + 0.90D2 * t52 * t38 - 0.
     #45D2 * t59 * t3 - 0.90D2 * t62 - t3 * (0.180D3 * t64 - 0.30D2 * t6
     #6)) * pi * t6 / 0.2880D4 - t74 * t77 * t47 * t80 / 0.16D2 + (-0.90
     #D2 * t74 * (t38 - t88 * t3) + t96) * t80 / 0.1440D4 - t4 * t6 * t8
     #0 * t101 / 0.8D1 + (0.90D2 * t74 * (-t38 + t109 * t3) + t96) * t10
     #1 / 0.1440D4 - t74 * t77 * t47 * t101 / 0.16D2
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t124 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t126 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t128 = t2 * x1
      t129 = -0.1D1 + x1
      t130 = x1 * z
      t131 = 0.1D1 - x1 + t130
      t132 = 0.1D1 / t131
      t134 = t2 * t129 * t132
      t135 = t1 ** 2
      t136 = s * t135
      t138 = x1 * t129 * t132
      t139 = t136 * t138
      t140 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t134, t128, 0.0D0, -t139)
      t145 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t134, t128, 0.0D0, -t139)
      t147 = t129 ** 2
      t151 = log(0.4D1 * t106 * t12 * t132 * t147)
      t163 = x3 * x1
      t170 = t105 * x3
      t176 = Sqrt(-x3 * t131 * t13)
      t180 = -z + t130 - 0.3D1 * t163 * z + 0.2D1 * t163 - x3 - x1 * t11
     # + x3 * t11 * x1 + 0.2D1 * t170 * z - t170 * t11 + 0.2D1 * t20 * t
     #176 * z - t170
      t189 = t74 * t140 * t80 * t101 / 0.8D1 + (0.90D2 * t74 * (t145 - t
     #151 * t140) - 0.180D3 * t93 * t6 * t140) * t101 / 0.1440D4 - t74 *
     # (-z * t131 / t180 * t140 - t140) * t47 * t101 / 0.16D2
      t190 = FJET(XB1, XB2, s, 0.0D0, t128, -t134, 0.0D0, -t139, t189)
      t193 = x2 * t1 * s
      t194 = -0.1D1 + x2
      t196 = t194 * t1 * s
      t197 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t193, -t
     #196, 0.0D0, 0.0D0, 0.0D0)
      t201 = t74 * t197 * t47 * t80 / 0.16D2
      t202 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t193, -t
     #196, 0.0D0, 0.0D0, 0.0D0)
      t206 = log(-0.4D1 * t85 * t12 * t194)
      t208 = -t202 + t206 * t197
      t213 = 0.180D3 * t93 * t6 * t197
      t220 = t74 * t197 * t80 * t101 / 0.8D1
      t221 = t201 + (-0.90D2 * t74 * t208 - t213) * t80 / 0.1440D4 + t22
     #0
      t222 = FJET(XB1, XB2, s, 0.0D0, t193, 0.0D0, -t196, 0.0D0, t221)
      t224 = x2 * x3
      t225 = t84 * x3
      t228 = Sqrt(x3 * t194 * t13)
      t229 = t20 * t228
      t231 = 0.2D1 * t229 * x2
      t234 = 0.1D1 / (0.1D1 - x3 + t224)
      t236 = t2 * (0.1D1 - x3 - x2 + t224 + t225 + t231) * t234
      t241 = t2 * x2 * (-0.1D1 + t224 + 0.2D1 * t229) * t234
      t242 = x2 * z
      t250 = 0.1D1 / (-t242 + z - t225 + x3 + x2 - t224 * z + t225 * z -
     # 0.2D1 * t229 * z + 0.2D1 * t229 * t242 - t231)
      t253 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t241, t
     #236, 0.0D0, 0.0D0, 0.0D0)
      t256 = (t242 - z - x2) * t253 * t47 * t80
      t258 = t74 * t250 * t256 / 0.16D2
      t259 = FJET(XB1, XB2, s, 0.0D0, t236, 0.0D0, -t241, 0.0D0, t258)
      t261 = t6 * t250
      t266 = t1 * t129
      t268 = t194 * s * t266 * t132
      t270 = x2 * s * t266
      t272 = t136 * t194 * t138
      t273 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t270, t
     #268, t128, 0.0D0, t272)
      t275 = t273 * t80 * t101
      t277 = t74 * t275 / 0.8D1
      t278 = FJET(XB1, XB2, s, 0.0D0, t268, t128, -t270, t272, -t277)
      t283 = FJET(XB1, XB2, s, 0.0D0, -t196, 0.0D0, t193, 0.0D0, t221)
      t285 = FJET(XB1, XB2, s, 0.0D0, -t134, t128, 0.0D0, -t139, t189)
      t287 = FJET(XB1, XB2, s, 0.0D0, -t241, 0.0D0, t236, 0.0D0, t258)
      t292 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t294 = FJET(XB1, XB2, s, t128, 0.0D0, 0.0D0, -t134, -t139, t189)
      t296 = FJET(XB1, XB2, s, t128, -t270, 0.0D0, t268, t272, -t277)
      t307 = t201 + (-0.90D2 * t74 * t208 - t213) * t80 / 0.1440D4 + t22
     #0
      t308 = FJET(XB1, XB2, s, t193, 0.0D0, -t196, 0.0D0, 0.0D0, t307)
      t310 = FJET(XB1, XB2, s, t236, 0.0D0, -t241, 0.0D0, 0.0D0, t258)
      t315 = FJET(XB1, XB2, s, t268, 0.0D0, -t270, t128, t272, -t277)
      t320 = FJET(XB1, XB2, s, -t196, 0.0D0, t193, 0.0D0, 0.0D0, t307)
      t322 = FJET(XB1, XB2, s, -t134, 0.0D0, 0.0D0, t128, -t139, t189)
      t324 = FJET(XB1, XB2, s, -t270, t128, t268, 0.0D0, t272, -t277)
      t329 = FJET(XB1, XB2, s, -t241, 0.0D0, t236, 0.0D0, 0.0D0, t258)
      rrgg2qqbarhhardt7s1em1 = t122 * t121 + t124 * t121 + t126 * t121 +
     # t190 * t189 + t222 * t221 + t259 * pi * t261 * t256 / 0.16D2 - t2
     #78 * pi * t6 * t275 / 0.8D1 + t283 * t221 + t285 * t189 + t287 * p
     #i * t261 * t256 / 0.16D2 + t292 * t121 + t294 * t189 - t296 * pi *
     # t6 * t275 / 0.8D1 + t308 * t307 + t310 * pi * t261 * t256 / 0.16D
     #2 - t315 * pi * t6 * t275 / 0.8D1 + t320 * t307 + t322 * t189 - t3
     #24 * pi * t6 * t275 / 0.8D1 + t329 * pi * t261 * t256 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s1em2
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = cos(t7)
      t11 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t34 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t36 = z ** 2
      t38 = Sin(t7)
      t39 = t38 ** 2
      t42 = log(0.4D1 / t36 * t39)
      t49 = -t4 * t6 * (z / (-z - x3 + 0.2D1 * t8 * t11 * z) + 0.1D1) / 
     #x3 / 0.32D2 - t4 * t6 * t24 / 0.16D2 - t4 * t6 * t28 / 0.16D2 + (0
     #.180D3 * t3 * lh - 0.90D2 * t34 + 0.90D2 * t42 * t3) * pi * t6 / 0
     #.2880D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t54 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t56 = t2 * x1
      t57 = -0.1D1 + x1
      t60 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t62 = t2 * t57 * t60
      t63 = t1 ** 2
      t67 = s * t63 * x1 * t57 * t60
      t68 = pi * t6
      t69 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #62, t56, 0.0D0, -t67)
      t72 = t68 * t69 * t28 / 0.16D2
      t73 = FJET(XB1, XB2, s, 0.0D0, t56, -t62, 0.0D0, -t67, t72)
      t76 = t6 * t69 * t28
      t80 = x2 * t1 * s
      t83 = (-0.1D1 + x2) * t1 * s
      t84 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t83
     #, 0.0D0, 0.0D0, 0.0D0)
      t87 = t68 * t84 * t24 / 0.16D2
      t88 = FJET(XB1, XB2, s, 0.0D0, t80, 0.0D0, -t83, 0.0D0, t87)
      t91 = t6 * t84 * t24
      t94 = FJET(XB1, XB2, s, 0.0D0, -t83, 0.0D0, t80, 0.0D0, t87)
      t98 = FJET(XB1, XB2, s, 0.0D0, -t62, t56, 0.0D0, -t67, t72)
      t102 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t56, 0.0D0, 0.0D0, -t62, -t67, t72)
      t108 = FJET(XB1, XB2, s, t80, 0.0D0, -t83, 0.0D0, 0.0D0, t87)
      t112 = FJET(XB1, XB2, s, -t83, 0.0D0, t80, 0.0D0, 0.0D0, t87)
      t116 = FJET(XB1, XB2, s, -t62, 0.0D0, 0.0D0, t56, -t67, t72)
      rrgg2qqbarhhardt7s1em2 = t50 * t49 + t52 * t49 + t54 * t49 + t73 *
     # pi * t76 / 0.16D2 + t88 * pi * t91 / 0.16D2 + t94 * pi * t91 / 0.
     #16D2 + t98 * pi * t76 / 0.16D2 + t102 * t49 + t104 * pi * t76 / 0.
     #16D2 + t108 * pi * t91 / 0.16D2 + t112 * pi * t91 / 0.16D2 + t116 
     #* pi * t76 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s1em3
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * pi * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = pi * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarhhardt7s1em3 = -t9 * t3 * t11 / 0.32D2 - t13 * t3 * t11 
     #/ 0.32D2 - t16 * t3 * t11 / 0.32D2 - t19 * t3 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s1em4
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt7s2e1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = x2 * x3
      t6 = -0.1D1 + t5
      t7 = 0.1D1 / t6
      t9 = t2 * t4 * t7
      t10 = s * t3
      t11 = -0.1D1 + x1
      t12 = t10 * t11
      t13 = -0.1D1 + x2
      t14 = x3 * x1
      t15 = x1 * z
      t16 = t14 * z
      t17 = x4 * pi
      t18 = cos(t17)
      t19 = 0.1D1 - x1 + t15
      t23 = Sqrt(-x3 * t19 * x2 * t1)
      t24 = t18 * t23
      t25 = 0.2D1 * t24
      t28 = 0.1D1 / t19
      t31 = t12 * t13 * (-t5 - 0.1D1 + x3 + x1 - t14 - t15 + t16 + t25) 
     #* t28 * t7
      t32 = t10 * x1
      t35 = t32 * x3 * t13 * t7
      t36 = t5 * x1
      t38 = x2 ** 2
      t39 = t38 * x3
      t40 = t39 * x1
      t41 = 0.3D1 * t5
      t42 = t5 * t15
      t44 = t39 * t15
      t46 = 0.2D1 * t24 * x2
      t47 = -t16 - 0.2D1 * t36 + t40 - t39 + t41 + t14 + 0.2D1 * t42 - t
     #44 - t25 - x2 - x3 + t46
      t50 = t12 * t47 * t28 * t7
      t51 = t3 ** 2
      t56 = s * t51 * x2 * x1 * t11 * t28
      t57 = s ** 2
      t58 = 0.1D1 / t57
      t59 = pi * t58
      t60 = x2 * x1
      t61 = t60 * z
      t62 = x2 * z
      t63 = -0.1D1 + t61 - t62 - t60 + x1 + x2 - t15
      t64 = t19 * t63
      t67 = x1 ** 2
      t68 = x3 * t67
      t69 = t68 * x2
      t70 = z ** 2
      t72 = x2 * t67
      t90 = 0.1D1 - 0.3D1 * t36 + t40 + t46 - 0.3D1 * t61 + t69 + t60 * 
     #t70 + 0.2D1 * t72 * z - t72 * t70 + 0.2D1 * t24 * x1 - 0.2D1 * t24
     # * t62 - 0.2D1 * t24 * t15 - 0.2D1 * t24 * t60 - x3 * t70 * t60 - 
     #0.2D1 * t68 * t62 + t68 * t70 * x2
      t92 = t39 * z
      t93 = t5 * z
      t96 = 0.2D1 * t5
      t103 = 0.4D1 * t42 - t44 + t92 - t93 + 0.2D1 * t24 * t61 + t96 - t
     #39 + t62 + t67 + 0.2D1 * t15 - t25 + 0.2D1 * t60 - 0.2D1 * t67 * z
     # + t67 * t70 - t72 - 0.2D1 * x1 - x2
      t105 = 0.1D1 / (t90 + t103)
      t106 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t31, -t5
     #0, t9, t35, -t56)
      t109 = t67 * t28
      t110 = Sin(t17)
      t111 = t110 ** 2
      t112 = 0.1D1 / t70
      t113 = t111 * t112
      t114 = t6 ** 2
      t115 = 0.1D1 / t114
      t118 = t13 ** 2
      t119 = t11 ** 2
      t120 = t118 * t119
      t121 = t5 * t1
      t125 = log(-0.4D1 * t109 * t113 * t115 * t120 * t121)
      t128 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t31, -t5
     #0, t9, t35, -t56)
      t131 = -t64 * t105 * t106 + t125 * t19 * t63 * t105 * t128
      t134 = lh * pi
      t135 = t134 * t58
      t139 = 0.180D3 * t135 * t64 * t105 * t128
      t140 = 0.90D2 * t59 * t131 + t139
      t141 = 0.1D1 / x3
      t143 = 0.1D1 / x2
      t144 = 0.1D1 / x1
      t145 = t143 * t144
      t147 = t140 * t141 * t145 / 0.720D3
      t148 = FJET(XB1, XB2, s, t9, t31, t35, -t50, -t56, t147)
      t151 = t141 * t143 * t144
      t154 = FJET(XB1, XB2, s, t35, -t50, t9, t31, -t56, t147)
      t159 = t3 * t11
      t160 = t13 * s * t159
      t163 = t10 * t11 * x2 * t28
      t164 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t160, -t
     #163, t32, 0.0D0, -t56)
      t165 = t109 * t113
      t169 = log(0.4D1 * t165 * t120 * t5)
      t170 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t160, -t
     #163, t32, 0.0D0, -t56)
      t175 = t58 * t170
      t180 = (0.90D2 * t59 * (-t164 + t169 * t170) + 0.180D3 * t134 * t1
     #75) * t141 * t145
      t181 = t72 * t111
      t182 = t112 * t28
      t186 = log(0.4D1 * t181 * t182 * t120)
      t188 = t186 ** 2
      t191 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t160, -t
     #163, t32, 0.0D0, -t56)
      t192 = -t186 * t164 + t188 * t170 / 0.2D1 + t191
      t196 = t164 - t186 * t170
      t200 = lh ** 2
      t201 = 0.180D3 * t200
      t202 = pi ** 2
      t203 = 0.30D2 * t202
      t204 = t201 - t203
      t205 = t204 * pi
      t206 = t205 * t175
      t211 = t180 / 0.720D3 - (0.90D2 * t59 * t192 - 0.180D3 * t134 * t5
     #8 * t196 + t206) * t143 * t144 / 0.720D3
      t212 = FJET(XB1, XB2, s, t32, t160, 0.0D0, -t163, -t56, t211)
      t215 = x3 * s * t159
      t216 = t10 * t14
      t217 = t2 * t159
      t218 = t2 * t4
      t221 = x3 * t1
      t225 = log(-0.4D1 * t109 * t111 * t112 * t119 * t221)
      t226 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t217, -t
     #215, -t218, t216, 0.0D0)
      t228 = t225 ** 2
      t229 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t217, -t
     #215, -t218, t216, 0.0D0)
      t232 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t217, -t
     #215, -t218, t216, 0.0D0)
      t241 = t58 * t229
      t250 = log(-0.4D1 * t165 * t119 * x2 * t221)
      t261 = (0.90D2 * t59 * (t225 * t226 - t228 * t229 / 0.2D1 - t232) 
     #- 0.180D3 * t134 * t58 * (-t226 + t225 * t229) - t205 * t241) * t1
     #41 * t144 / 0.720D3 + (0.90D2 * t59 * (-t226 + t250 * t229) + 0.18
     #0D3 * t134 * t241) * t141 * t145 / 0.720D3
      t262 = FJET(XB1, XB2, s, -t215, t216, t217, -t218, 0.0D0, t261)
      t265 = x2 * t3 * s
      t267 = t13 * t3 * s
      t268 = t113 * t118
      t271 = log(0.4D1 * t5 * t268)
      t272 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t267, t
     #265, 0.0D0, 0.0D0, 0.0D0)
      t274 = t271 ** 2
      t275 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t267, t
     #265, 0.0D0, 0.0D0, 0.0D0)
      t278 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t267, t
     #265, 0.0D0, 0.0D0, 0.0D0)
      t287 = t58 * t275
      t288 = t205 * t287
      t293 = x2 * t111
      t294 = t112 * t118
      t297 = log(0.4D1 * t293 * t294)
      t302 = t297 ** 2
      t305 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, -t267, t
     #265, 0.0D0, 0.0D0, 0.0D0)
      t314 = 0.60D2 * lh * t202
      t315 = 0.240D3 * zeta3
      t317 = 0.120D3 * t200 * lh
      t318 = t314 - t315 - t317
      t319 = t318 * pi
      t333 = log(0.4D1 * t69 * t268)
      t346 = log(0.4D1 * t72 * t268)
      t348 = t346 ** 2
      t363 = (0.90D2 * t59 * (-t271 * t272 + t274 * t275 / 0.2D1 + t278)
     # - 0.180D3 * t134 * t58 * (t272 - t271 * t275) + t288) * t141 * t1
     #43 / 0.1440D4 + (t205 * t58 * (t272 - t297 * t275) + 0.90D2 * t59 
     #* (t302 * t272 / 0.2D1 + t305 - t302 * t297 * t275 / 0.6D1 - t297 
     #* t278) + t319 * t287 - 0.180D3 * t134 * t58 * (-t297 * t272 + t30
     #2 * t275 / 0.2D1 + t278)) * t143 / 0.1440D4 + (0.90D2 * t59 * (t27
     #2 - t333 * t275) - 0.180D3 * t134 * t287) * t141 * t145 / 0.720D3 
     #- (0.90D2 * t59 * (t346 * t272 - t348 * t275 / 0.2D1 - t278) - 0.1
     #80D3 * t134 * t58 * (-t272 + t346 * t275) - t288) * t143 * t144 / 
     #0.720D3
      t364 = FJET(XB1, XB2, s, 0.0D0, t265, 0.0D0, -t267, 0.0D0, t363)
      t366 = t10 * t1
      t367 = t10 * x3
      t368 = t67 * t111
      t370 = t112 * x3 * t1
      t373 = log(-0.4D1 * t368 * t370)
      t374 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t366, t
     #367, 0.0D0, 0.0D0, 0.0D0)
      t376 = t373 ** 2
      t377 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t366, t
     #367, 0.0D0, 0.0D0, 0.0D0)
      t380 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t366, t
     #367, 0.0D0, 0.0D0, 0.0D0)
      t389 = t58 * t377
      t390 = t205 * t389
      t397 = log(-0.4D1 * t181 * t370)
      t410 = log(-0.4D1 * t113 * t221)
      t415 = t410 ** 2
      t418 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, -t366, t
     #367, 0.0D0, 0.0D0, 0.0D0)
      t440 = log(-0.4D1 * t5 * t113 * t1)
      t442 = t440 ** 2
      t457 = (0.90D2 * t59 * (-t373 * t374 + t376 * t377 / 0.2D1 + t380)
     # - 0.180D3 * t134 * t58 * (t374 - t373 * t377) + t390) * t141 * t1
     #44 / 0.720D3 + (0.90D2 * t59 * (t374 - t397 * t377) - 0.180D3 * t1
     #34 * t389) * t141 * t145 / 0.720D3 + (t205 * t58 * (t374 - t410 * 
     #t377) + 0.90D2 * t59 * (t415 * t374 / 0.2D1 + t418 - t415 * t410 *
     # t377 / 0.6D1 - t410 * t380) + t319 * t389 - 0.180D3 * t134 * t58 
     #* (-t410 * t374 + t415 * t377 / 0.2D1 + t380)) * t141 / 0.1440D4 +
     # (0.90D2 * t59 * (-t440 * t374 + t442 * t377 / 0.2D1 + t380) - 0.1
     #80D3 * t134 * t58 * (t374 - t440 * t377) + t390) * t141 * t143 / 0
     #.1440D4
      t458 = FJET(XB1, XB2, s, -t366, 0.0D0, t367, 0.0D0, 0.0D0, t457)
      t461 = t182 * t119
      t464 = log(0.4D1 * t68 * t111 * t461)
      t465 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.
     #0D0, t32, 0.0D0, 0.0D0)
      t467 = t464 ** 2
      t468 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.
     #0D0, t32, 0.0D0, 0.0D0)
      t471 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.
     #0D0, t32, 0.0D0, 0.0D0)
      t480 = t58 * t468
      t481 = t205 * t480
      t487 = log(0.4D1 * t368 * t461)
      t492 = t487 ** 2
      t495 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.
     #0D0, t32, 0.0D0, 0.0D0)
      t517 = log(0.4D1 * t69 * t113 * t28 * t119)
      t529 = log(0.4D1 * t181 * t461)
      t531 = t529 ** 2
      t546 = (0.90D2 * t59 * (-t464 * t465 + t467 * t468 / 0.2D1 + t471)
     # - 0.180D3 * t134 * t58 * (t465 - t464 * t468) + t481) * t141 * t1
     #44 / 0.720D3 + (t205 * t58 * (t465 - t487 * t468) + 0.90D2 * t59 *
     # (t492 * t465 / 0.2D1 + t495 - t492 * t487 * t468 / 0.6D1 - t487 *
     # t471) + t319 * t480 - 0.180D3 * t134 * t58 * (-t487 * t465 + t492
     # * t468 / 0.2D1 + t471)) * t144 / 0.720D3 + (0.90D2 * t59 * (t465 
     #- t517 * t468) - 0.180D3 * t134 * t480) * t141 * t145 / 0.720D3 - 
     #(0.90D2 * t59 * (t529 * t465 - t531 * t468 / 0.2D1 - t471) - 0.180
     #D3 * t134 * t58 * (-t465 + t529 * t468) - t481) * t143 * t144 / 0.
     #720D3
      t547 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t12, t32, 0.0D0, t546)
      t549 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0
     #D0, 0.0D0, 0.0D0, 0.0D0)
      t553 = log(0.4D1 * x3 * t111 * t112)
      t554 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0
     #D0, 0.0D0, 0.0D0, 0.0D0)
      t559 = t553 ** 2
      t562 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0
     #D0, 0.0D0, 0.0D0, 0.0D0)
      t566 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0
     #D0, 0.0D0, 0.0D0, 0.0D0)
      t571 = t58 * t554
      t572 = t319 * t571
      t584 = log(0.4D1 * t113)
      t585 = t584 ** 2
      t588 = t585 * t584
      t611 = rrgg2qqbarhhard71J5(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0
     #D0, 0.0D0, 0.0D0, 0.0D0)
      t619 = t202 ** 2
      t620 = t200 ** 2
      t626 = t585 ** 2
      t634 = log(0.4D1 * t68 * t113)
      t636 = t634 ** 2
      t647 = t205 * t571
      t652 = t368 * t112
      t654 = log(0.4D1 * t652)
      t659 = t654 ** 2
      t681 = log(0.4D1 * t5 * t652)
      t694 = log(0.4D1 * t72 * t113)
      t696 = t694 ** 2
      t713 = log(0.4D1 * t5 * t113)
      t715 = t713 ** 2
      t732 = log(0.4D1 * t293 * t112)
      t737 = t732 ** 2
      t757 = (t205 * t58 * (-t549 + t553 * t554) + 0.90D2 * t59 * (-t559
     # * t549 / 0.2D1 - t562 + t559 * t553 * t554 / 0.6D1 + t553 * t566)
     # - t572 - 0.180D3 * t134 * t58 * (t553 * t549 - t559 * t554 / 0.2D
     #1 - t566)) * t141 / 0.1440D4 - (-0.90D2 * t585 * lh + t314 - t315 
     #- t317 - 0.15D2 * t588 - t584 * t204) * pi * t58 * t549 / 0.1440D4
     # - (0.180D3 * t584 * lh + 0.45D2 * t585 + t201 - t203) * pi * t58 
     #* t566 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t584) * pi * t58 * t
     #562 / 0.1440D4 - t59 * t611 / 0.16D2 - (0.30D2 * t588 * lh + t585 
     #* t204 / 0.2D1 - t584 * t318 + t619 + 0.60D2 * t620 + 0.480D3 * lh
     # * zeta3 - 0.60D2 * t200 * t202 + 0.15D2 / 0.4D1 * t626) * pi * t5
     #71 / 0.1440D4 + (0.90D2 * t59 * (t634 * t549 - t636 * t554 / 0.2D1
     # - t566) - 0.180D3 * t134 * t58 * (-t549 + t634 * t554) - t647) * 
     #t141 * t144 / 0.720D3 + (t205 * t58 * (-t549 + t654 * t554) + 0.90
     #D2 * t59 * (-t659 * t549 / 0.2D1 - t562 + t659 * t654 * t554 / 0.6
     #D1 + t654 * t566) - t572 - 0.180D3 * t134 * t58 * (t654 * t549 - t
     #659 * t554 / 0.2D1 - t566)) * t144 / 0.720D3 + (0.90D2 * t59 * (-t
     #549 + t681 * t554) + 0.180D3 * t134 * t571) * t141 * t145 / 0.720D
     #3 - (0.90D2 * t59 * (-t694 * t549 + t696 * t554 / 0.2D1 + t566) - 
     #0.180D3 * t134 * t58 * (t549 - t694 * t554) + t647) * t143 * t144 
     #/ 0.720D3 + (0.90D2 * t59 * (t713 * t549 - t715 * t554 / 0.2D1 - t
     #566) - 0.180D3 * t134 * t58 * (-t549 + t713 * t554) - t647) * t141
     # * t143 / 0.1440D4 + (t205 * t58 * (-t549 + t732 * t554) + 0.90D2 
     #* t59 * (-t737 * t549 / 0.2D1 - t562 + t737 * t732 * t554 / 0.6D1 
     #+ t732 * t566) - t572 - 0.180D3 * t134 * t58 * (t732 * t549 - t737
     # * t554 / 0.2D1 - t566)) * t143 / 0.1440D4
      t758 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t10, 0.0D0, 0.0D0, t757)
      t760 = FJET(XB1, XB2, s, 0.0D0, t10, 0.0D0, 0.0D0, 0.0D0, t757)
      t762 = FJET(XB1, XB2, s, -t218, t217, t216, -t215, 0.0D0, t261)
      t764 = FJET(XB1, XB2, s, t367, 0.0D0, -t366, 0.0D0, 0.0D0, t457)
      t766 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t10, 0.0D0, t757)
      t768 = Sqrt(-t121)
      t769 = t18 * t768
      t770 = 0.2D1 * t769
      t772 = 0.2D1 * t769 * x2
      t775 = t10 * (-x2 - x3 + t41 - t39 - t770 + t772) * t7
      t779 = t10 * t13 * (-t5 - 0.1D1 + x3 + t770) * t7
      t782 = t294 * t1 * t115
      t785 = log(-0.4D1 * t5 * t111 * t782)
      t786 = 0.1D1 + t62 - x2
      t787 = t785 * t786
      t791 = 0.1D1 / (t39 - t92 + x2 - t96 - t62 + t93 + 0.2D1 * t769 * 
     #t62 - t772 + t770 - 0.1D1)
      t792 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t779, t
     #775, 0.0D0, 0.0D0, 0.0D0)
      t795 = t785 ** 2
      t797 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t779, t
     #775, 0.0D0, 0.0D0, 0.0D0)
      t798 = t791 * t797
      t801 = t786 * t791
      t802 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t779, t
     #775, 0.0D0, 0.0D0, 0.0D0)
      t807 = t801 * t792
      t814 = t801 * t797
      t823 = log(-0.4D1 * t5 * t368 * t782)
      t835 = (0.90D2 * t59 * (-t787 * t791 * t792 + t795 * t786 * t798 /
     # 0.2D1 + t801 * t802) - 0.180D3 * t134 * t58 * (t807 - t787 * t798
     #) + t205 * t58 * t814) * t141 * t143 / 0.1440D4 + (0.90D2 * t59 * 
     #(t807 - t823 * t786 * t798) - 0.180D3 * t135 * t814) * t141 * t145
     # / 0.720D3
      t836 = FJET(XB1, XB2, s, 0.0D0, t775, 0.0D0, -t779, 0.0D0, t835)
      t838 = FJET(XB1, XB2, s, t32, -t12, 0.0D0, 0.0D0, 0.0D0, t546)
      t840 = FJET(XB1, XB2, s, -t267, 0.0D0, t265, 0.0D0, 0.0D0, t363)
      t842 = FJET(XB1, XB2, s, t216, -t215, -t218, t217, 0.0D0, t261)
      t844 = t148 * t140 * t151 / 0.720D3 + t154 * t140 * t151 / 0.720D3
     # + t212 * t211 + t262 * t261 + t364 * t363 + t458 * t457 + t547 * 
     #t546 + t758 * t757 + t760 * t757 + t762 * t261 + t764 * t457 + t76
     #6 * t757 + t836 * t835 + t838 * t546 + t840 * t363 + t842 * t261
      t845 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t32, -t12, 0.0D0, t546)
      t847 = FJET(XB1, XB2, s, t265, 0.0D0, -t267, 0.0D0, 0.0D0, t363)
      t849 = FJET(XB1, XB2, s, 0.0D0, -t163, t32, t160, -t56, t211)
      t851 = FJET(XB1, XB2, s, 0.0D0, t367, 0.0D0, -t366, 0.0D0, t457)
      t853 = FJET(XB1, XB2, s, 0.0D0, -t366, 0.0D0, t367, 0.0D0, t457)
      t866 = t180 / 0.720D3 - (0.90D2 * t59 * t192 - 0.180D3 * t134 * t5
     #8 * t196 + t206) * t143 * t144 / 0.720D3
      t867 = FJET(XB1, XB2, s, -t163, 0.0D0, t160, t32, -t56, t866)
      t869 = FJET(XB1, XB2, s, 0.0D0, -t779, 0.0D0, t775, 0.0D0, t835)
      t871 = FJET(XB1, XB2, s, t31, t9, -t50, t35, -t56, t147)
      t878 = 0.90D2 * t59 * t131 + t139
      t882 = FJET(XB1, XB2, s, -t50, t35, t31, t9, -t56, t878 * t141 * t
     #145 / 0.720D3)
      t886 = FJET(XB1, XB2, s, -t12, t32, 0.0D0, 0.0D0, 0.0D0, t546)
      t888 = FJET(XB1, XB2, s, t775, 0.0D0, -t779, 0.0D0, 0.0D0, t835)
      t890 = FJET(XB1, XB2, s, t10, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t757)
      t892 = FJET(XB1, XB2, s, t217, -t218, -t215, t216, 0.0D0, t261)
      t894 = FJET(XB1, XB2, s, t160, t32, -t163, 0.0D0, -t56, t211)
      t896 = FJET(XB1, XB2, s, 0.0D0, -t267, 0.0D0, t265, 0.0D0, t363)
      t898 = FJET(XB1, XB2, s, -t779, 0.0D0, t775, 0.0D0, 0.0D0, t835)
      t900 = t845 * t546 + t847 * t363 + t849 * t211 + t851 * t457 + t85
     #3 * t457 + t867 * t866 + t869 * t835 + t871 * t140 * t151 / 0.720D
     #3 + t882 * t878 * t151 / 0.720D3 + t886 * t546 + t888 * t835 + t89
     #0 * t757 + t892 * t261 + t894 * t211 + t896 * t363 + t898 * t835
      rrgg2qqbarhhardt7s2e1 = t844 + t900

      end function



      doubleprecision function rrgg2qqbarhhardt7s2e0
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0
     #, t5, 0.0D0, 0.0D0)
      t10 = x1 ** 2
      t11 = t10 * x3
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = x1 * z
      t19 = 0.1D1 - x1 + t18
      t20 = 0.1D1 / t19
      t21 = t17 * t20
      t22 = t3 ** 2
      t23 = t21 * t22
      t26 = log(0.4D1 * t11 * t14 * t23)
      t27 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D
     #0, t5, 0.0D0, 0.0D0)
      t32 = lh * pi
      t33 = t7 * t27
      t35 = 0.180D3 * t32 * t33
      t37 = 0.1D1 / x3
      t39 = 0.1D1 / x1
      t43 = 0.1D1 / x2
      t45 = t37 * t43 * t39
      t48 = x2 * t10
      t49 = t48 * t14
      t52 = log(0.4D1 * t49 * t23)
      t61 = t10 * t14
      t64 = log(0.4D1 * t61 * t23)
      t66 = t64 ** 2
      t69 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D
     #0, t5, 0.0D0, 0.0D0)
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = pi ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * pi
      t88 = (0.90D2 * t8 * (t9 - t26 * t27) - t35) * t37 * t39 / 0.720D3
     # + t8 * t27 * t45 / 0.8D1 - (0.90D2 * t8 * (-t9 + t52 * t27) + t35
     #) * t43 * t39 / 0.720D3 + (0.90D2 * t8 * (-t64 * t9 + t66 * t27 / 
     #0.2D1 + t69) - 0.180D3 * t32 * t7 * (t9 - t64 * t27) + t83 * t33) 
     #* t39 / 0.720D3
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t88)
      t92 = x2 * t1 * s
      t93 = -0.1D1 + x2
      t95 = t93 * t1 * s
      t96 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t95, t92
     #, 0.0D0, 0.0D0, 0.0D0)
      t100 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t95, t9
     #2, 0.0D0, 0.0D0, 0.0D0)
      t101 = t14 * t17
      t102 = t93 ** 2
      t103 = t101 * t102
      t106 = log(0.4D1 * t48 * t103)
      t111 = t7 * t96
      t113 = 0.180D3 * t32 * t111
      t118 = x2 * x3
      t121 = log(0.4D1 * t118 * t103)
      t130 = x2 * t14
      t131 = t17 * t102
      t134 = log(0.4D1 * t130 * t131)
      t136 = t134 ** 2
      t139 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t95, t9
     #2, 0.0D0, 0.0D0, 0.0D0)
      t152 = t8 * t96 * t45 / 0.8D1 - (0.90D2 * t8 * (-t100 + t106 * t96
     #) + t113) * t43 * t39 / 0.720D3 + (0.90D2 * t8 * (t100 - t121 * t9
     #6) - t113) * t37 * t43 / 0.1440D4 + (0.90D2 * t8 * (-t134 * t100 +
     # t136 * t96 / 0.2D1 + t139) - 0.180D3 * t32 * t7 * (t100 - t134 * 
     #t96) + t83 * t111) * t43 / 0.1440D4
      t153 = FJET(XB1, XB2, s, 0.0D0, t92, 0.0D0, -t95, 0.0D0, t152)
      t157 = t2 * t3 * x2 * t20
      t159 = t1 * t3
      t160 = t93 * s * t159
      t161 = t1 ** 2
      t166 = s * t161 * x2 * x1 * t3 * t20
      t167 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t160, -t
     #157, t5, 0.0D0, -t166)
      t170 = t8 * t167 * t45 / 0.8D1
      t171 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t160, -t
     #157, t5, 0.0D0, -t166)
      t176 = log(0.4D1 * t49 * t21 * t22 * t102)
      t178 = -t171 + t176 * t167
      t183 = 0.180D3 * t32 * t7 * t167
      t188 = -t170 - (-0.90D2 * t8 * t178 - t183) * t43 * t39 / 0.720D3
      t189 = FJET(XB1, XB2, s, -t157, 0.0D0, t160, t5, -t166, t188)
      t191 = -0.1D1 + x3
      t192 = t2 * t191
      t193 = t2 * x3
      t194 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t192, t
     #193, 0.0D0, 0.0D0, 0.0D0)
      t198 = log(-0.4D1 * t118 * t101 * t191)
      t199 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t192, t
     #193, 0.0D0, 0.0D0, 0.0D0)
      t204 = t7 * t199
      t206 = 0.180D3 * t32 * t204
      t211 = x3 * t191
      t214 = log(-0.4D1 * t101 * t211)
      t216 = t214 ** 2
      t219 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t192, t
     #193, 0.0D0, 0.0D0, 0.0D0)
      t236 = log(-0.4D1 * t61 * t17 * x3 * t191)
      t248 = (0.90D2 * t8 * (t194 - t198 * t199) - t206) * t37 * t43 / 0
     #.1440D4 + (0.90D2 * t8 * (-t214 * t194 + t216 * t199 / 0.2D1 + t21
     #9) - 0.180D3 * t32 * t7 * (t194 - t214 * t199) + t83 * t204) * t37
     # / 0.1440D4 + (0.90D2 * t8 * (t194 - t236 * t199) - t206) * t37 * 
     #t39 / 0.720D3 + t8 * t199 * t45 / 0.8D1
      t249 = FJET(XB1, XB2, s, 0.0D0, -t192, 0.0D0, t193, 0.0D0, t248)
      t254 = log(0.4D1 * x3 * t14 * t17)
      t255 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D
     #0, 0.0D0, 0.0D0, 0.0D0)
      t257 = t254 ** 2
      t258 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D
     #0, 0.0D0, 0.0D0, 0.0D0)
      t261 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D
     #0, 0.0D0, 0.0D0, 0.0D0)
      t270 = t7 * t258
      t271 = t83 * t270
      t276 = log(0.4D1 * t101)
      t279 = t276 ** 2
      t286 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D
     #0, 0.0D0, 0.0D0, 0.0D0)
      t312 = log(0.4D1 * t118 * t101)
      t318 = 0.180D3 * t32 * t270
      t325 = log(0.4D1 * t130 * t17)
      t327 = t325 ** 2
      t343 = log(0.4D1 * t11 * t101)
      t357 = log(0.4D1 * t48 * t101)
      t368 = log(0.4D1 * t61 * t17)
      t370 = t368 ** 2
      t384 = (0.90D2 * t8 * (t254 * t255 - t257 * t258 / 0.2D1 - t261) -
     # 0.180D3 * t32 * t7 * (-t255 + t254 * t258) - t271) * t37 / 0.1440
     #D4 - (0.180D3 * t276 * lh + 0.45D2 * t279 + t79 - t81) * pi * t7 *
     # t255 / 0.1440D4 - t8 * t286 / 0.16D2 - (-0.90D2 * t279 * lh + 0.6
     #0D2 * lh * t80 - 0.240D3 * zeta3 - 0.120D3 * t78 * lh - 0.15D2 * t
     #279 * t276 - t276 * t82) * pi * t270 / 0.1440D4 - (-0.180D3 * lh -
     # 0.90D2 * t276) * pi * t7 * t261 / 0.1440D4 + (0.90D2 * t8 * (-t25
     #5 + t312 * t258) + t318) * t37 * t43 / 0.1440D4 + (0.90D2 * t8 * (
     #t325 * t255 - t327 * t258 / 0.2D1 - t261) - 0.180D3 * t32 * t7 * (
     #-t255 + t325 * t258) - t271) * t43 / 0.1440D4 + (0.90D2 * t8 * (-t
     #255 + t343 * t258) + t318) * t37 * t39 / 0.720D3 - t8 * t258 * t45
     # / 0.8D1 - (0.90D2 * t8 * (t255 - t357 * t258) - t318) * t43 * t39
     # / 0.720D3 + (0.90D2 * t8 * (t368 * t255 - t370 * t258 / 0.2D1 - t
     #261) - 0.180D3 * t32 * t7 * (-t255 + t368 * t258) - t271) * t39 / 
     #0.720D3
      t385 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t384)
      t388 = x3 * s * t159
      t389 = x3 * x1
      t390 = t2 * t389
      t391 = t191 * s
      t392 = t391 * t159
      t393 = t1 * x1
      t394 = t391 * t393
      t395 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t392, -t
     #388, -t394, t390, 0.0D0)
      t402 = log(-0.4D1 * t10 * t20 * t14 * t17 * t22 * t211)
      t403 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t392, -t
     #388, -t394, t390, 0.0D0)
      t418 = (0.90D2 * t8 * (-t395 + t402 * t403) + 0.180D3 * t32 * t7 *
     # t403) * t37 * t39 / 0.720D3 - t8 * t403 * t45 / 0.8D1
      t419 = FJET(XB1, XB2, s, -t388, t390, t392, -t394, 0.0D0, t418)
      t421 = FJET(XB1, XB2, s, t390, -t388, -t394, t392, 0.0D0, t418)
      t423 = FJET(XB1, XB2, s, 0.0D0, -t95, 0.0D0, t92, 0.0D0, t152)
      t425 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t384)
      t427 = 0.3D1 * t118
      t428 = x2 ** 2
      t429 = t428 * x3
      t430 = cos(t12)
      t432 = Sqrt(-t118 * t191)
      t433 = t430 * t432
      t434 = 0.2D1 * t433
      t436 = 0.2D1 * t433 * x2
      t438 = -0.1D1 + t118
      t439 = 0.1D1 / t438
      t441 = t2 * (-x2 - x3 + t427 - t429 - t434 + t436) * t439
      t445 = t2 * t93 * (-t118 - 0.1D1 + x3 + t434) * t439
      t446 = x2 * z
      t447 = 0.1D1 + t446 - x2
      t448 = t429 * z
      t449 = 0.2D1 * t118
      t450 = t118 * z
      t454 = 0.1D1 / (t429 - t448 + x2 - t449 - t446 + t450 + 0.2D1 * t4
     #33 * t446 - t436 + t434 - 0.1D1)
      t455 = t447 * t454
      t457 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t445, t
     #441, 0.0D0, 0.0D0, 0.0D0)
      t463 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t445, t
     #441, 0.0D0, 0.0D0, 0.0D0)
      t466 = t438 ** 2
      t472 = log(-0.4D1 * t118 * t14 * t131 * t191 / t466)
      t487 = t8 * t455 * t457 * t37 * t43 * t39 / 0.8D1 + (0.90D2 * t8 *
     # (t455 * t463 - t472 * t447 * t454 * t457) - 0.180D3 * t32 * t7 * 
     #t455 * t457) * t37 * t43 / 0.1440D4
      t488 = FJET(XB1, XB2, s, t441, 0.0D0, -t445, 0.0D0, 0.0D0, t487)
      t490 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t384)
      t499 = -t170 - (-0.90D2 * t8 * t178 - t183) * t43 * t39 / 0.720D3
      t500 = FJET(XB1, XB2, s, t5, t160, 0.0D0, -t157, -t166, t499)
      t502 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t88)
      t504 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t384)
      t506 = t389 * z
      t510 = Sqrt(-x3 * t19 * x2 * t191)
      t511 = t430 * t510
      t512 = 0.2D1 * t511
      t517 = t4 * t93 * (-t118 - 0.1D1 + x3 + x1 - t389 - t18 + t506 + t
     #512) * t20 * t439
      t519 = t391 * t393 * t439
      t520 = t118 * x1
      t522 = t429 * x1
      t523 = t118 * t18
      t525 = t429 * t18
      t527 = 0.2D1 * t511 * x2
      t528 = -t506 - 0.2D1 * t520 + t522 - t429 + t427 + t389 + 0.2D1 * 
     #t523 - t525 - t512 - x2 - x3 + t527
      t531 = t4 * t528 * t20 * t439
      t534 = t5 * x3 * t93 * t439
      t535 = x2 * x1
      t536 = t535 * z
      t537 = -0.1D1 + t536 - t446 - t535 + x1 + x2 - t18
      t551 = 0.1D1 - t512 + 0.2D1 * t535 - 0.2D1 * t10 * z + t10 * t16 -
     # t48 + t448 - t450 - 0.3D1 * t520 + t522 + t527 - 0.3D1 * t536 + t
     #11 * x2 + t535 * t16 + 0.2D1 * t48 * z - t48 * t16
      t571 = 0.2D1 * t511 * x1 + t10 - 0.2D1 * x1 - x2 - 0.2D1 * t511 * 
     #t446 - 0.2D1 * t511 * t18 - 0.2D1 * t511 * t535 - x3 * t16 * t535 
     #- 0.2D1 * t11 * t446 + t11 * t16 * x2 + 0.4D1 * t523 - t525 + 0.2D
     #1 * t511 * t536 + 0.2D1 * t18 + t449 - t429 + t446
      t574 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t517, -t
     #531, t519, t534, -t166)
      t576 = 0.1D1 / (t551 + t571) * t574 * t45
      t578 = t8 * t19 * t537 * t576 / 0.8D1
      t579 = FJET(XB1, XB2, s, t517, t519, -t531, t534, -t166, -t578)
      t582 = t7 * t19 * t537
      t586 = FJET(XB1, XB2, s, t392, -t394, -t388, t390, 0.0D0, t418)
      t588 = t89 * t88 + t153 * t152 + t189 * t188 + t249 * t248 + t385 
     #* t384 + t419 * t418 + t421 * t418 + t423 * t152 + t425 * t384 + t
     #488 * t487 + t490 * t384 + t500 * t499 + t502 * t88 + t504 * t384 
     #- t579 * pi * t582 * t576 / 0.8D1 + t586 * t418
      t589 = FJET(XB1, XB2, s, t160, t5, -t157, 0.0D0, -t166, t499)
      t591 = FJET(XB1, XB2, s, 0.0D0, -t157, t5, t160, -t166, t499)
      t593 = FJET(XB1, XB2, s, 0.0D0, t441, 0.0D0, -t445, 0.0D0, t487)
      t595 = FJET(XB1, XB2, s, 0.0D0, t193, 0.0D0, -t192, 0.0D0, t248)
      t597 = FJET(XB1, XB2, s, -t531, t534, t517, t519, -t166, -t578)
      t602 = FJET(XB1, XB2, s, t519, t517, t534, -t531, -t166, -t578)
      t607 = FJET(XB1, XB2, s, 0.0D0, -t445, 0.0D0, t441, 0.0D0, t487)
      t609 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t88)
      t611 = FJET(XB1, XB2, s, t92, 0.0D0, -t95, 0.0D0, 0.0D0, t152)
      t613 = FJET(XB1, XB2, s, -t95, 0.0D0, t92, 0.0D0, 0.0D0, t152)
      t615 = FJET(XB1, XB2, s, t534, -t531, t519, t517, -t166, -t578)
      t620 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t88)
      t622 = FJET(XB1, XB2, s, -t445, 0.0D0, t441, 0.0D0, 0.0D0, t487)
      t624 = FJET(XB1, XB2, s, -t394, t392, t390, -t388, 0.0D0, t418)
      t626 = FJET(XB1, XB2, s, -t192, 0.0D0, t193, 0.0D0, 0.0D0, t248)
      t628 = FJET(XB1, XB2, s, t193, 0.0D0, -t192, 0.0D0, 0.0D0, t248)
      t630 = t589 * t499 + t591 * t499 + t593 * t487 + t595 * t248 - t59
     #7 * pi * t582 * t576 / 0.8D1 - t602 * pi * t582 * t576 / 0.8D1 + t
     #607 * t487 + t609 * t88 + t611 * t152 + t613 * t152 - t615 * pi * 
     #t582 * t576 / 0.8D1 + t620 * t88 + t622 * t487 + t624 * t418 + t62
     #6 * t248 + t628 * t248
      rrgg2qqbarhhardt7s2e0 = t588 + t630

      end function



      doubleprecision function rrgg2qqbarhhardt7s2em1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * x3 * t9 * t12)
      t16 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0
     #, 0.0D0, 0.0D0, 0.0D0)
      t21 = lh * pi
      t22 = t4 * t16
      t24 = 0.180D3 * t21 * t22
      t26 = 0.1D1 / x3
      t30 = t12 * t9
      t32 = log(0.4D1 * t30)
      t41 = t32 ** 2
      t43 = lh ** 2
      t45 = pi ** 2
      t51 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0
     #, 0.0D0, 0.0D0, 0.0D0)
      t54 = t16 * t26
      t55 = 0.1D1 / x2
      t59 = x2 * t9
      t62 = log(0.4D1 * t59 * t12)
      t71 = 0.1D1 / x1
      t75 = x1 ** 2
      t76 = t75 * t9
      t79 = log(0.4D1 * t76 * t12)
      t90 = (0.90D2 * t5 * (-t6 + t15 * t16) + t24) * t26 / 0.1440D4 - (
     #-0.180D3 * lh - 0.90D2 * t32) * pi * t4 * t6 / 0.1440D4 - (0.180D3
     # * t32 * lh + 0.45D2 * t41 + 0.180D3 * t43 - 0.30D2 * t45) * pi * 
     #t22 / 0.1440D4 - t5 * t51 / 0.16D2 - t5 * t54 * t55 / 0.16D2 + (0.
     #90D2 * t5 * (-t6 + t62 * t16) + t24) * t55 / 0.1440D4 - t5 * t16 *
     # t55 * t71 / 0.8D1 + (0.90D2 * t5 * (-t6 + t79 * t16) + t24) * t71
     # / 0.720D3 - t5 * t54 * t71 / 0.8D1
      t91 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t90)
      t93 = -0.1D1 + x3
      t94 = t2 * t93
      t95 = t2 * x3
      t96 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t94, t95
     #, 0.0D0, 0.0D0, 0.0D0)
      t97 = t96 * t26
      t101 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t94, t9
     #5, 0.0D0, 0.0D0, 0.0D0)
      t105 = log(-0.4D1 * t30 * x3 * t93)
      t119 = t5 * t97 * t55 / 0.16D2 + (0.90D2 * t5 * (t101 - t105 * t96
     #) - 0.180D3 * t21 * t4 * t96) * t26 / 0.1440D4 + t5 * t97 * t71 / 
     #0.8D1
      t120 = FJET(XB1, XB2, s, 0.0D0, -t94, 0.0D0, t95, 0.0D0, t119)
      t123 = x2 * t1 * s
      t124 = -0.1D1 + x2
      t126 = t124 * t1 * s
      t127 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t126, t
     #123, 0.0D0, 0.0D0, 0.0D0)
      t136 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t126, t
     #123, 0.0D0, 0.0D0, 0.0D0)
      t137 = t124 ** 2
      t141 = log(0.4D1 * t59 * t12 * t137)
      t152 = t5 * t127 * t55 * t71 / 0.8D1 + t5 * t127 * t26 * t55 / 0.1
     #6D2 + (0.90D2 * t5 * (t136 - t141 * t127) - 0.180D3 * t21 * t4 * t
     #127) * t55 / 0.1440D4
      t153 = FJET(XB1, XB2, s, 0.0D0, t123, 0.0D0, -t126, 0.0D0, t152)
      t155 = FJET(XB1, XB2, s, 0.0D0, -t126, 0.0D0, t123, 0.0D0, t152)
      t157 = FJET(XB1, XB2, s, -t126, 0.0D0, t123, 0.0D0, 0.0D0, t152)
      t159 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t90)
      t161 = t2 * x1
      t162 = -0.1D1 + x1
      t163 = t2 * t162
      t164 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t163, 0
     #.0D0, t161, 0.0D0, 0.0D0)
      t169 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t163, 0
     #.0D0, t161, 0.0D0, 0.0D0)
      t172 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t174 = t162 ** 2
      t178 = log(0.4D1 * t76 * t12 * t172 * t174)
      t193 = t5 * t164 * t55 * t71 / 0.8D1 + (0.90D2 * t5 * (t169 - t178
     # * t164) - 0.180D3 * t21 * t4 * t164) * t71 / 0.720D3 + t5 * t164 
     #* t26 * t71 / 0.8D1
      t194 = FJET(XB1, XB2, s, t161, -t163, 0.0D0, 0.0D0, 0.0D0, t193)
      t196 = t93 * s
      t197 = t1 * t162
      t198 = t196 * t197
      t200 = t196 * t1 * x1
      t202 = x3 * s * t197
      t204 = t2 * x1 * x3
      t205 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t198, -t
     #202, -t200, t204, 0.0D0)
      t207 = t205 * t26 * t71
      t209 = t5 * t207 / 0.8D1
      t210 = FJET(XB1, XB2, s, t198, -t200, -t202, t204, 0.0D0, -t209)
      t217 = t2 * t162 * x2 * t172
      t219 = t124 * s * t197
      t220 = t1 ** 2
      t225 = s * t220 * x2 * x1 * t162 * t172
      t226 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t219, -t
     #217, t161, 0.0D0, -t225)
      t228 = t226 * t55 * t71
      t230 = t5 * t228 / 0.8D1
      t231 = FJET(XB1, XB2, s, -t217, 0.0D0, t219, t161, -t225, -t230)
      t236 = FJET(XB1, XB2, s, t161, t219, 0.0D0, -t217, -t225, -t230)
      t241 = FJET(XB1, XB2, s, -t202, t204, t198, -t200, 0.0D0, -t209)
      t246 = FJET(XB1, XB2, s, t204, -t202, -t200, t198, 0.0D0, -t209)
      t251 = FJET(XB1, XB2, s, t219, t161, -t217, 0.0D0, -t225, -t230)
      t256 = FJET(XB1, XB2, s, -t200, t198, t204, -t202, 0.0D0, -t209)
      t261 = t91 * t90 + t120 * t119 + t153 * t152 + t155 * t152 + t157 
     #* t152 + t159 * t90 + t194 * t193 - t210 * pi * t4 * t207 / 0.8D1 
     #- t231 * pi * t4 * t228 / 0.8D1 - t236 * pi * t4 * t228 / 0.8D1 - 
     #t241 * pi * t4 * t207 / 0.8D1 - t246 * pi * t4 * t207 / 0.8D1 - t2
     #51 * pi * t4 * t228 / 0.8D1 - t256 * pi * t4 * t207 / 0.8D1
      t262 = FJET(XB1, XB2, s, 0.0D0, -t217, t161, t219, -t225, -t230)
      t267 = x2 * x3
      t268 = cos(t7)
      t270 = Sqrt(-t267 * t93)
      t271 = t268 * t270
      t272 = 0.2D1 * t271
      t276 = 0.1D1 / (-0.1D1 + t267)
      t278 = t2 * t124 * (-t267 - 0.1D1 + x3 + t272) * t276
      t280 = x2 ** 2
      t281 = t280 * x3
      t283 = 0.2D1 * t271 * x2
      t286 = t2 * (-x2 - x3 + 0.3D1 * t267 - t281 - t272 + t283) * t276
      t287 = x2 * z
      t288 = 0.1D1 + t287 - x2
      t297 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t278, t
     #286, 0.0D0, 0.0D0, 0.0D0)
      t300 = 0.1D1 / (t281 - t281 * z + x2 - 0.2D1 * t267 - t287 + t267 
     #* z + 0.2D1 * t271 * t287 - t283 + t272 - 0.1D1) * t297 * t26 * t5
     #5
      t302 = t5 * t288 * t300 / 0.16D2
      t303 = FJET(XB1, XB2, s, 0.0D0, -t278, 0.0D0, t286, 0.0D0, t302)
      t305 = t4 * t288
      t309 = FJET(XB1, XB2, s, 0.0D0, t286, 0.0D0, -t278, 0.0D0, t302)
      t314 = FJET(XB1, XB2, s, -t278, 0.0D0, t286, 0.0D0, 0.0D0, t302)
      t319 = FJET(XB1, XB2, s, t286, 0.0D0, -t278, 0.0D0, 0.0D0, t302)
      t324 = FJET(XB1, XB2, s, -t94, 0.0D0, t95, 0.0D0, 0.0D0, t119)
      t326 = FJET(XB1, XB2, s, t123, 0.0D0, -t126, 0.0D0, 0.0D0, t152)
      t328 = FJET(XB1, XB2, s, t95, 0.0D0, -t94, 0.0D0, 0.0D0, t119)
      t330 = FJET(XB1, XB2, s, 0.0D0, t95, 0.0D0, -t94, 0.0D0, t119)
      t332 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t90)
      t334 = FJET(XB1, XB2, s, -t163, t161, 0.0D0, 0.0D0, 0.0D0, t193)
      t336 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t90)
      t338 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t161, -t163, 0.0D0, t193)
      t340 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t163, t161, 0.0D0, t193)
      t342 = -t262 * pi * t4 * t228 / 0.8D1 + t303 * pi * t305 * t300 / 
     #0.16D2 + t309 * pi * t305 * t300 / 0.16D2 + t314 * pi * t305 * t30
     #0 / 0.16D2 + t319 * pi * t305 * t300 / 0.16D2 + t324 * t119 + t326
     # * t152 + t328 * t119 + t330 * t119 + t332 * t90 + t334 * t193 + t
     #336 * t90 + t338 * t193 + t340 * t193
      rrgg2qqbarhhardt7s2em1 = t261 + t342

      end function



      doubleprecision function rrgg2qqbarhhardt7s2em2
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0
     #, 0.0D0, 0.0D0, 0.0D0)
      t23 = z ** 2
      t26 = Sin(x4 * pi)
      t27 = t26 ** 2
      t30 = log(0.4D1 / t23 * t27)
      t37 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 - (-0.180D3 * lh - 0.90D2 * t30) *
     # pi * t4 * t6 / 0.1440D4
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t42 = t2 * x1
      t44 = t2 * (-0.1D1 + x1)
      t45 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t44, 0.0
     #D0, t42, 0.0D0, 0.0D0)
      t48 = t5 * t45 * t15 / 0.8D1
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t42, -t44, 0.0D0, t48)
      t52 = t4 * t45 * t15
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t44, t42, 0.0D0, t48)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t37)
      t61 = t2 * x3
      t63 = t2 * (-0.1D1 + x3)
      t64 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t63, t61
     #, 0.0D0, 0.0D0, 0.0D0)
      t67 = t5 * t64 * t7 / 0.16D2
      t68 = FJET(XB1, XB2, s, 0.0D0, t61, 0.0D0, -t63, 0.0D0, t67)
      t71 = t4 * t64 * t7
      t75 = x2 * t1 * s
      t78 = (-0.1D1 + x2) * t1 * s
      t79 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t78, t75
     #, 0.0D0, 0.0D0, 0.0D0)
      t82 = t5 * t79 * t11 / 0.16D2
      t83 = FJET(XB1, XB2, s, 0.0D0, t75, 0.0D0, -t78, 0.0D0, t82)
      t86 = t4 * t79 * t11
      t89 = FJET(XB1, XB2, s, 0.0D0, -t63, 0.0D0, t61, 0.0D0, t67)
      t93 = FJET(XB1, XB2, s, 0.0D0, -t78, 0.0D0, t75, 0.0D0, t82)
      t97 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t37)
      t99 = FJET(XB1, XB2, s, t42, -t44, 0.0D0, 0.0D0, 0.0D0, t48)
      t103 = FJET(XB1, XB2, s, t61, 0.0D0, -t63, 0.0D0, 0.0D0, t67)
      t107 = FJET(XB1, XB2, s, t75, 0.0D0, -t78, 0.0D0, 0.0D0, t82)
      t111 = FJET(XB1, XB2, s, -t63, 0.0D0, t61, 0.0D0, 0.0D0, t67)
      t115 = FJET(XB1, XB2, s, -t78, 0.0D0, t75, 0.0D0, 0.0D0, t82)
      t119 = FJET(XB1, XB2, s, -t44, t42, 0.0D0, 0.0D0, 0.0D0, t48)
      rrgg2qqbarhhardt7s2em2 = t38 * t37 + t40 * t37 + t49 * pi * t52 / 
     #0.8D1 + t55 * pi * t52 / 0.8D1 + t59 * t37 + t68 * pi * t71 / 0.16
     #D2 + t83 * pi * t86 / 0.16D2 + t89 * pi * t71 / 0.16D2 + t93 * pi 
     #* t86 / 0.16D2 + t97 * t37 + t99 * pi * t52 / 0.8D1 + t103 * pi * 
     #t71 / 0.16D2 + t107 * pi * t86 / 0.16D2 + t111 * pi * t71 / 0.16D2
     # + t115 * pi * t86 / 0.16D2 + t119 * pi * t52 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt7s2em3
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarhhardt7s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 
     #/ 0.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s2em4
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt7s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt7s3e1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t24 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t25 = -0.1D1 + x3
      t26 = 0.1D1 / t25
      t27 = t14 * t26
      t30 = log(-0.4D1 * t7 * t27)
      t31 = t30 * z
      t33 = t30 ** 2
      t37 = z * t24
      t39 = cos(t11)
      t40 = x3 * z
      t42 = Sqrt(-t40 * t25)
      t46 = 0.1D1 / (-z - x3 + 0.2D1 * t39 * t42)
      t51 = lh * pi
      t53 = z * t18
      t61 = lh ** 2
      t62 = 0.180D3 * t61
      t63 = pi ** 2
      t64 = 0.30D2 * t63
      t65 = t62 - t64
      t66 = t65 * pi
      t68 = z * t21 * t46
      t73 = 0.1D1 / x3
      t75 = 0.1D1 / x1
      t78 = t6 * t13
      t79 = t78 * t10
      t81 = log(0.4D1 * t79)
      t86 = t81 ** 2
      t89 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t98 = 0.60D2 * lh * t63
      t99 = 0.240D3 * zeta3
      t101 = 0.120D3 * t61 * lh
      t102 = t98 - t99 - t101
      t103 = t102 * pi
      t104 = t4 * t21
      t116 = x2 ** 2
      t117 = x3 * t116
      t120 = log(0.4D1 * t117 * t79)
      t122 = t117 * t6
      t123 = -0.1D1 + x2
      t124 = t14 * t123
      t127 = log(-0.4D1 * t122 * t124)
      t131 = log(-0.4D1 * t122 * t27)
      t139 = t51 * t4
      t144 = 0.1D1 / x2
      t145 = t144 * t75
      t148 = t116 * t6
      t151 = log(0.4D1 * t148 * t14)
      t153 = t151 ** 2
      t158 = log(-0.4D1 * t148 * t124)
      t160 = t158 ** 2
      t176 = pi * t18
      t179 = pi * t24
      t181 = pi * t21
      t185 = x3 * t10
      t189 = log(-0.4D1 * t185 * t13 * t26)
      t194 = log(0.4D1 * t185 * t13)
      t197 = t194 ** 2
      t199 = t189 ** 2
      t234 = log(-0.4D1 * t117 * t124)
      t236 = t234 ** 2
      t241 = log(-0.4D1 * t117 * t27)
      t242 = t241 * z
      t244 = t241 ** 2
      t252 = log(0.4D1 * t117 * t14)
      t254 = t252 ** 2
      t269 = t66 * t4
      t280 = t10 * t116
      t281 = t13 * t123
      t284 = log(-0.4D1 * t280 * t281)
      t285 = t284 ** 2
      t288 = log(0.4D1 * t280 * t13)
      t289 = t288 ** 2
      t300 = t4 * t18
      t313 = log(0.4D1 * t14)
      t314 = t313 ** 2
      t317 = t314 * t313
      t339 = rrgg2qqbarhhard71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t347 = t63 ** 2
      t348 = t61 ** 2
      t354 = t314 ** 2
      t360 = (0.90D2 * t5 * (t17 * t18 - t20 * t21 / 0.2D1 - t24 - (-t31
     # * t18 + t33 * z * t21 / 0.2D1 + t37) * t46) - 0.180D3 * t51 * t4 
     #* (-t18 + t17 * t21 - (t53 - t31 * t21) * t46) + t66 * t4 * (-t68 
     #- t21)) * t73 * t75 / 0.1440D4 - (t66 * t4 * (t18 - t81 * t21) + 0
     #.90D2 * t5 * (t86 * t18 / 0.2D1 + t89 - t86 * t81 * t21 / 0.6D1 - 
     #t81 * t24) + t103 * t104 - 0.180D3 * t51 * t4 * (-t81 * t18 + t86 
     #* t21 / 0.2D1 + t24)) * t75 / 0.1440D4 - (0.90D2 * t5 * (-t120 * t
     #21 + t127 * t21 + (t53 - t131 * z * t21) * t46) - 0.180D3 * t139 *
     # t68) * t73 * t145 / 0.720D3 - (0.90D2 * t5 * (-t151 * t18 + t153 
     #* t21 / 0.2D1 + t158 * t18 - t160 * t21 / 0.2D1) - 0.180D3 * t51 *
     # t4 * (t158 * t21 - t151 * t21)) * t144 * t75 / 0.720D3 + ((-0.180
     #D3 * t176 * lh + 0.90D2 * t179 + t181 * t65) * t4 * (t189 * z * t4
     #6 + t194) + 0.90D2 * t5 * t21 * (t197 * t194 / 0.6D1 + t199 * t189
     # * z * t46 / 0.6D1) + (-0.180D3 * t179 * lh + t181 * t102 + 0.90D2
     # * pi * t89 + t176 * t65) * t4 * (-z * t46 - 0.1D1) + (-0.180D3 * 
     #t181 * lh + 0.90D2 * t176) * t4 * (-t199 * z * t46 / 0.2D1 - t197 
     #/ 0.2D1)) * t73 / 0.2880D4 + (0.90D2 * t5 * (-t234 * t18 + t236 * 
     #t21 / 0.2D1 - (-t242 * t18 + t244 * z * t21 / 0.2D1 + t37) * t46 +
     # t252 * t18 - t254 * t21 / 0.2D1) - 0.180D3 * t51 * t4 * (-t234 * 
     #t21 - (t53 - t242 * t21) * t46 + t252 * t21) - t269 * t68) * t73 *
     # t144 / 0.1440D4 - ((0.90D2 * t5 * t18 - 0.180D3 * t51 * t104) * (
     #-t285 / 0.2D1 + t289 / 0.2D1) + 0.90D2 * t5 * t21 * (-t289 * t288 
     #/ 0.6D1 + t285 * t284 / 0.6D1) + (-0.180D3 * t51 * t300 + t66 * t1
     #04 + 0.90D2 * t5 * t24) * (t284 - t288)) * t144 / 0.1440D4 - (-0.9
     #0D2 * t314 * lh + t98 - t99 - t101 - 0.15D2 * t317 - t313 * t65) *
     # pi * t300 / 0.2880D4 - (0.180D3 * t313 * lh + 0.45D2 * t314 + t62
     # - t64) * pi * t4 * t24 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t31
     #3) * pi * t4 * t89 / 0.2880D4 - t5 * t339 / 0.32D2 - (0.30D2 * t31
     #7 * lh + t314 * t65 / 0.2D1 - t313 * t102 + t347 + 0.60D2 * t348 +
     # 0.480D3 * lh * zeta3 - 0.60D2 * t61 * t63 + 0.15D2 / 0.4D1 * t354
     #) * pi * t104 / 0.2880D4
      t361 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t360)
      t363 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t360)
      t365 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t360)
      t367 = x2 * x3
      t368 = 0.1D1 - x3 + t367
      t369 = 0.1D1 / t368
      t370 = t367 * t369
      t371 = t2 * t370
      t373 = t2 * t25 * t369
      t375 = t368 ** 2
      t376 = 0.1D1 / t375
      t377 = t25 * t376
      t381 = log(0.4D1 * t117 * t10 * t281 * t377)
      t382 = t381 * z
      t383 = t123 * t25
      t385 = Sqrt(t40 * t383)
      t389 = 0.1D1 / (-z - x3 + t367 + 0.2D1 * t39 * t385)
      t390 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t373, t371, 0.0D0)
      t393 = t381 ** 2
      t395 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t373, t371, 0.0D0)
      t396 = t389 * t395
      t399 = z * t389
      t400 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t373, t371, 0.0D0)
      t405 = t399 * t390
      t411 = t399 * t395
      t417 = t117 * t78
      t422 = log(0.4D1 * t417 * t10 * t123 * t377)
      t434 = (0.90D2 * t5 * (-t382 * t389 * t390 + t393 * z * t396 / 0.2
     #D1 + t399 * t400) - 0.180D3 * t51 * t4 * (t405 - t382 * t396) + t2
     #69 * t411) * t73 * t144 / 0.1440D4 - (0.90D2 * t5 * (-t405 + t422 
     #* z * t396) + 0.180D3 * t139 * t411) * t73 * t145 / 0.720D3
      t435 = FJET(XB1, XB2, s, 0.0D0, t371, 0.0D0, -t373, 0.0D0, t434)
      t438 = t1 * x1
      t439 = x1 * z
      t440 = -z - x1 + t439
      t441 = 0.1D1 / t440
      t443 = t123 * s * t438 * t441
      t444 = -0.1D1 + x1
      t445 = t2 * t444
      t447 = x2 * s * t438
      t448 = t1 ** 2
      t449 = s * t448
      t452 = x1 * t444 * t441
      t453 = t449 * t123 * t452
      t454 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t447, t4
     #43, -t445, 0.0D0, -t453)
      t455 = 0.1D1 / t8
      t456 = t455 * t441
      t457 = t444 ** 2
      t459 = t456 * t457 * t123
      t462 = log(0.4D1 * t417 * t459)
      t463 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t447, t4
     #43, -t445, 0.0D0, -t453)
      t468 = t4 * t463
      t474 = t148 * t13
      t477 = log(0.4D1 * t474 * t459)
      t479 = t477 ** 2
      t482 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t447, t4
     #43, -t445, 0.0D0, -t453)
      t496 = -(0.90D2 * t5 * (t454 - t462 * t463) - 0.180D3 * t51 * t468
     #) * t73 * t145 / 0.720D3 - (0.90D2 * t5 * (-t477 * t454 + t479 * t
     #463 / 0.2D1 + t482) - 0.180D3 * t51 * t4 * (t454 - t477 * t463) + 
     #t66 * t468) * t144 * t75 / 0.720D3
      t497 = FJET(XB1, XB2, s, 0.0D0, t443, -t445, t447, -t453, t496)
      t500 = t2 * x1 * t441
      t501 = t449 * t452
      t502 = t7 * t13
      t503 = t456 * t457
      t506 = log(-0.4D1 * t502 * t503)
      t507 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t500, -t445, 0.0D0, t501)
      t510 = t456 * t457 * t26
      t513 = log(0.4D1 * t502 * t510)
      t514 = t513 * z
      t517 = t513 ** 2
      t519 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t500, -t445, 0.0D0, t501)
      t520 = t440 * t519
      t523 = z * t440
      t524 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t500, -t445, 0.0D0, t501)
      t527 = x3 * x1
      t528 = t527 * z
      t529 = x1 * t8
      t530 = x3 * t8
      t531 = t530 * x1
      t533 = 0.2D1 * t7 * z
      t534 = t7 * t8
      t535 = x3 * t440
      t537 = Sqrt(t535 * t25)
      t542 = 0.1D1 / (-t439 - t528 - t40 - t7 + t529 + t531 + t533 - t53
     #4 - t8 + 0.2D1 * t39 * t537 * z)
      t544 = t506 ** 2
      t550 = t523 * t507
      t561 = t519 - t523 * t519 * t542
      t570 = log(-0.4D1 * t78 * t503)
      t575 = t570 ** 2
      t578 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t500, -t445, 0.0D0, t501)
      t586 = t4 * t519
      t599 = t441 * t457
      t603 = log(-0.4D1 * t122 * t13 * t455 * t599)
      t607 = log(0.4D1 * t417 * t510)
      t625 = log(-0.4D1 * t474 * t503)
      t626 = t625 ** 2
      t643 = (0.90D2 * t5 * (-t506 * t507 - (-t514 * t440 * t507 + t517 
     #* z * t520 / 0.2D1 + t523 * t524) * t542 + t524 + t544 * t519 / 0.
     #2D1) - 0.180D3 * t51 * t4 * (-(t550 - t514 * t520) * t542 - t506 *
     # t519 + t507) + t66 * t4 * t561) * t73 * t75 / 0.1440D4 - (t66 * t
     #4 * (-t507 + t570 * t519) + 0.90D2 * t5 * (-t575 * t507 / 0.2D1 - 
     #t578 + t575 * t570 * t519 / 0.6D1 + t570 * t524) - t103 * t586 - 0
     #.180D3 * t51 * t4 * (t570 * t507 - t575 * t519 / 0.2D1 - t524)) * 
     #t75 / 0.1440D4 - (0.90D2 * t5 * (-t507 + t603 * t519 + (t550 - t60
     #7 * z * t520) * t542) + 0.180D3 * t51 * t4 * t561) * t73 * t145 / 
     #0.720D3 - (0.90D2 * t5 * (-t626 * t519 / 0.2D1 - t524 + t625 * t50
     #7) - 0.180D3 * t51 * t4 * (t625 * t519 - t507) - t66 * t586) * t14
     #4 * t75 / 0.720D3
      t644 = FJET(XB1, XB2, s, 0.0D0, -t445, -t500, 0.0D0, t501, t643)
      t646 = FJET(XB1, XB2, s, 0.0D0, -t500, -t445, 0.0D0, t501, t643)
      t648 = FJET(XB1, XB2, s, 0.0D0, -t373, 0.0D0, t371, 0.0D0, t434)
      t650 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t360)
      t652 = FJET(XB1, XB2, s, t447, -t445, t443, 0.0D0, -t453, t496)
      t654 = FJET(XB1, XB2, s, t371, 0.0D0, -t373, 0.0D0, 0.0D0, t434)
      t656 = FJET(XB1, XB2, s, t443, 0.0D0, t447, -t445, -t453, t496)
      t661 = t25 * s * t1 * t444 * t369
      t662 = t2 * x1
      t664 = Sqrt(-t535 * t383)
      t665 = t39 * t664
      t671 = t662 * x2 * (-x3 + t367 - z + t40 - x1 + t527 + t439 - t528
     # + 0.2D1 * t665) * t441 * t369
      t672 = t445 * t370
      t675 = t117 * x1
      t677 = t117 * t439
      t681 = t662 * (0.2D1 * t665 * x2 + t675 + t117 * z - x2 + t367 - t
     #677 + 0.1D1 - x3) * t441 * t369
      t682 = x2 * x1
      t683 = t682 * z
      t688 = x2 * t6
      t696 = t8 - t675 + t683 - t367 * z + t367 * x1 - t7 * x2 - t682 * 
     #t8 - 0.2D1 * t688 * z + t688 * t8 - 0.2D1 * t665 * z + t688 + 0.2D
     #1 * t665 * t683 + t439
      t707 = t40 + t7 - t529 + t528 - t531 - t533 + t534 + t677 - 0.2D1 
     #* t367 * t439 + t530 * t682 + 0.2D1 * t7 * x2 * z - t7 * t8 * x2 -
     # 0.2D1 * t665 * t682
      t709 = 0.1D1 / (t696 + t707)
      t710 = t709 * t440
      t711 = -z + t683 - t682
      t712 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t671, -t
     #681, t661, -t672, -t453)
      t721 = log(-0.4D1 * t117 * t78 * t455 * t599 * t383 * t376)
      t724 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t671, -t
     #681, t661, -t672, -t453)
      t734 = 0.90D2 * t5 * (-t710 * t711 * t712 + t721 * t709 * t440 * t
     #711 * t724) + 0.180D3 * t139 * t710 * t711 * t724
      t737 = t734 * t73 * t145 / 0.720D3
      t738 = FJET(XB1, XB2, s, t661, t671, -t672, -t681, -t453, -t737)
      t741 = t73 * t144 * t75
      t744 = FJET(XB1, XB2, s, t671, t661, -t681, -t672, -t453, -t737)
      t748 = FJET(XB1, XB2, s, -t445, 0.0D0, 0.0D0, -t500, t501, t643)
      t750 = FJET(XB1, XB2, s, -t445, t447, 0.0D0, t443, -t453, t496)
      t752 = FJET(XB1, XB2, s, -t500, 0.0D0, 0.0D0, -t445, t501, t643)
      t754 = FJET(XB1, XB2, s, -t373, 0.0D0, t371, 0.0D0, 0.0D0, t434)
      t756 = FJET(XB1, XB2, s, -t681, -t672, t671, t661, -t453, -t737)
      t760 = FJET(XB1, XB2, s, -t672, -t681, t661, t671, -t453, -t737)
      rrgg2qqbarhhardt7s3e1 = t361 * t360 + t363 * t360 + t365 * t360 + 
     #t435 * t434 + t497 * t496 + t644 * t643 + t646 * t643 + t648 * t43
     #4 + t650 * t360 + t652 * t496 + t654 * t434 + t656 * t496 - t738 *
     # t734 * t741 / 0.720D3 - t744 * t734 * t741 / 0.720D3 + t748 * t64
     #3 + t750 * t496 + t752 * t643 + t754 * t434 - t756 * t734 * t741 /
     # 0.720D3 - t760 * t734 * t741 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarhhardt7s3e0
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t21 = z * t6
      t22 = -0.1D1 + x3
      t23 = 0.1D1 / t22
      t24 = t15 * t23
      t27 = log(-0.4D1 * t8 * t24)
      t31 = cos(t12)
      t32 = x3 * z
      t34 = Sqrt(-t32 * t22)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t34)
      t43 = lh * pi
      t44 = z * t19
      t45 = t44 * t38
      t51 = 0.1D1 / x3
      t53 = 0.1D1 / x1
      t58 = 0.1D1 / x2
      t59 = t58 * t53
      t63 = x2 ** 2
      t64 = t63 * t7
      t65 = -0.1D1 + x2
      t66 = t15 * t65
      t69 = log(-0.4D1 * t64 * t66)
      t73 = log(0.4D1 * t64 * t15)
      t80 = t7 * t14
      t83 = log(0.4D1 * t80 * t11)
      t85 = t83 ** 2
      t88 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t97 = lh ** 2
      t98 = 0.180D3 * t97
      t99 = pi ** 2
      t100 = 0.30D2 * t99
      t101 = t98 - t100
      t102 = t101 * pi
      t103 = t4 * t19
      t108 = x3 * t11
      t112 = log(-0.4D1 * t108 * t14 * t23)
      t113 = t112 ** 2
      t118 = log(0.4D1 * t108 * t14)
      t119 = t118 ** 2
      t125 = pi * t19
      t128 = pi * t6
      t150 = log(0.4D1 * t15)
      t153 = t150 ** 2
      t160 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t184 = x3 * t63
      t187 = log(-0.4D1 * t184 * t66)
      t191 = log(-0.4D1 * t184 * t24)
      t198 = log(0.4D1 * t184 * t15)
      t203 = t43 * t4
      t210 = t11 * t63
      t211 = t14 * t65
      t214 = log(-0.4D1 * t210 * t211)
      t215 = t214 ** 2
      t218 = log(0.4D1 * t210 * t14)
      t219 = t218 ** 2
      t235 = (0.90D2 * t5 * (-t6 + t18 * t19 - (t21 - t27 * z * t19) * t
     #38) - 0.180D3 * t43 * t4 * (-t45 - t19)) * t51 * t53 / 0.1440D4 - 
     #t5 * t44 * t38 * t51 * t59 / 0.8D1 - t5 * (t69 * t19 - t73 * t19) 
     #* t58 * t53 / 0.8D1 - (0.90D2 * t5 * (-t83 * t6 + t85 * t19 / 0.2D
     #1 + t88) - 0.180D3 * t43 * t4 * (t6 - t83 * t19) + t102 * t103) * 
     #t53 / 0.1440D4 + (0.90D2 * t5 * t19 * (-t113 * z * t38 / 0.2D1 - t
     #119 / 0.2D1) + (-0.180D3 * t125 * lh + 0.90D2 * t128) * t4 * (t112
     # * z * t38 + t118) + (-0.180D3 * t128 * lh + 0.90D2 * pi * t88 + t
     #125 * t101) * t4 * (-z * t38 - 0.1D1)) * t51 / 0.2880D4 - (0.180D3
     # * t150 * lh + 0.45D2 * t153 + t98 - t100) * pi * t4 * t6 / 0.2880
     #D4 - t5 * t160 / 0.32D2 - (-0.90D2 * t153 * lh + 0.60D2 * lh * t99
     # - 0.240D3 * zeta3 - 0.120D3 * t97 * lh - 0.15D2 * t153 * t150 - t
     #150 * t101) * pi * t103 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t15
     #0) * pi * t4 * t88 / 0.2880D4 + (0.90D2 * t5 * (-t187 * t19 - (t21
     # - t191 * z * t19) * t38 + t198 * t19) + 0.180D3 * t203 * t45) * t
     #51 * t58 / 0.1440D4 - (0.90D2 * t5 * t19 * (-t215 / 0.2D1 + t219 /
     # 0.2D1) + (0.90D2 * t5 * t6 - 0.180D3 * t43 * t103) * (t214 - t218
     #)) * t58 / 0.1440D4
      t236 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t235)
      t238 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t235)
      t240 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t235)
      t242 = x2 * x3
      t243 = 0.1D1 - x3 + t242
      t244 = 0.1D1 / t243
      t245 = t242 * t244
      t246 = t2 * t245
      t248 = t2 * t22 * t244
      t249 = t65 * t22
      t251 = Sqrt(t32 * t249)
      t255 = 0.1D1 / (-z - x3 + t242 + 0.2D1 * t31 * t251)
      t256 = z * t255
      t258 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t248, t246, 0.0D0)
      t263 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t248, t246, 0.0D0)
      t266 = t243 ** 2
      t272 = log(0.4D1 * t184 * t11 * t211 * t22 / t266)
      t286 = t5 * t256 * t258 * t51 * t59 / 0.8D1 + (0.90D2 * t5 * (t256
     # * t263 - t272 * z * t255 * t258) - 0.180D3 * t203 * t256 * t258) 
     #* t51 * t58 / 0.1440D4
      t287 = FJET(XB1, XB2, s, 0.0D0, t246, 0.0D0, -t248, 0.0D0, t286)
      t290 = t1 * x1
      t291 = x1 * z
      t292 = -z - x1 + t291
      t293 = 0.1D1 / t292
      t295 = t65 * s * t290 * t293
      t296 = -0.1D1 + x1
      t297 = t2 * t296
      t299 = x2 * s * t290
      t300 = t1 ** 2
      t301 = s * t300
      t304 = x1 * t296 * t293
      t305 = t301 * t65 * t304
      t306 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t299, t2
     #95, -t297, 0.0D0, -t305)
      t309 = t51 * t58 * t53
      t312 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t299, t2
     #95, -t297, 0.0D0, -t305)
      t313 = t64 * t14
      t315 = 0.1D1 / t9 * t293
      t316 = t296 ** 2
      t321 = log(0.4D1 * t313 * t315 * t316 * t65)
      t333 = -t5 * t306 * t309 / 0.8D1 - (0.90D2 * t5 * (t312 - t321 * t
     #306) - 0.180D3 * t43 * t4 * t306) * t58 * t53 / 0.720D3
      t334 = FJET(XB1, XB2, s, 0.0D0, t295, -t297, t299, -t305, t333)
      t337 = t2 * x1 * t293
      t338 = t301 * t304
      t339 = z * t292
      t340 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t337, -t297, 0.0D0, t338)
      t342 = t8 * t14
      t347 = log(0.4D1 * t342 * t315 * t316 * t23)
      t349 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t337, -t297, 0.0D0, t338)
      t353 = x3 * x1
      t354 = t353 * z
      t355 = x1 * t9
      t356 = x3 * t9
      t357 = t356 * x1
      t359 = 0.2D1 * t8 * z
      t360 = t8 * t9
      t361 = x3 * t292
      t363 = Sqrt(t361 * t22)
      t368 = 0.1D1 / (-t291 - t354 - t32 - t8 + t355 + t357 + t359 - t36
     #0 - t9 + 0.2D1 * t31 * t363 * z)
      t370 = t315 * t316
      t373 = log(-0.4D1 * t342 * t370)
      t380 = t349 - t339 * t349 * t368
      t394 = log(-0.4D1 * t313 * t370)
      t399 = t4 * t349
      t408 = log(-0.4D1 * t80 * t370)
      t410 = t408 ** 2
      t413 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t337, -t297, 0.0D0, t338)
      t426 = (0.90D2 * t5 * (-(t339 * t340 - t347 * z * t292 * t349) * t
     #368 - t373 * t349 + t340) - 0.180D3 * t43 * t4 * t380) * t51 * t53
     # / 0.1440D4 + t5 * t380 * t309 / 0.8D1 - (0.90D2 * t5 * (t394 * t3
     #49 - t340) + 0.180D3 * t43 * t399) * t58 * t53 / 0.720D3 - (0.90D2
     # * t5 * (t408 * t340 - t410 * t349 / 0.2D1 - t413) - 0.180D3 * t43
     # * t4 * (-t340 + t408 * t349) - t102 * t399) * t53 / 0.1440D4
      t427 = FJET(XB1, XB2, s, 0.0D0, -t297, -t337, 0.0D0, t338, t426)
      t429 = FJET(XB1, XB2, s, 0.0D0, -t337, -t297, 0.0D0, t338, t426)
      t431 = FJET(XB1, XB2, s, 0.0D0, -t248, 0.0D0, t246, 0.0D0, t286)
      t433 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t235)
      t435 = FJET(XB1, XB2, s, t299, -t297, t295, 0.0D0, -t305, t333)
      t437 = FJET(XB1, XB2, s, t246, 0.0D0, -t248, 0.0D0, 0.0D0, t286)
      t439 = FJET(XB1, XB2, s, t295, 0.0D0, t299, -t297, -t305, t333)
      t444 = t22 * s * t1 * t296 * t244
      t445 = t2 * x1
      t447 = Sqrt(-t361 * t249)
      t448 = t31 * t447
      t454 = t445 * x2 * (-x3 + t242 - z + t32 - x1 + t353 + t291 - t354
     # + 0.2D1 * t448) * t293 * t244
      t455 = t297 * t245
      t458 = t184 * x1
      t460 = t184 * t291
      t464 = t445 * (0.2D1 * t448 * x2 + t458 + t184 * z - x2 + t242 - t
     #460 + 0.1D1 - x3) * t293 * t244
      t467 = x2 * x1
      t476 = x2 * t7
      t477 = t467 * z
      t480 = t291 + t354 + t460 - 0.2D1 * t242 * t291 + t356 * t467 + 0.
     #2D1 * t8 * x2 * z - t8 * t9 * x2 - 0.2D1 * t448 * t467 + t476 + 0.
     #2D1 * t448 * t477 + t9 + t32 + t8
      t490 = -t355 - t458 + t477 - t242 * z + t242 * x1 - t8 * x2 - t467
     # * t9 - 0.2D1 * t476 * z + t476 * t9 - 0.2D1 * t448 * z - t357 - t
     #359 + t360
      t492 = 0.1D1 / (t480 + t490)
      t496 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t454, -t
     #464, t444, -t455, -t305)
      t498 = (-z + t477 - t467) * t496 * t309
      t500 = t5 * t492 * t292 * t498 / 0.8D1
      t501 = FJET(XB1, XB2, s, t444, t454, -t455, -t464, -t305, t500)
      t504 = t4 * t492 * t292
      t508 = FJET(XB1, XB2, s, t454, t444, -t464, -t455, -t305, t500)
      t513 = FJET(XB1, XB2, s, -t297, 0.0D0, 0.0D0, -t337, t338, t426)
      t515 = FJET(XB1, XB2, s, -t297, t299, 0.0D0, t295, -t305, t333)
      t517 = FJET(XB1, XB2, s, -t337, 0.0D0, 0.0D0, -t297, t338, t426)
      t519 = FJET(XB1, XB2, s, -t248, 0.0D0, t246, 0.0D0, 0.0D0, t286)
      t521 = FJET(XB1, XB2, s, -t464, -t455, t454, t444, -t305, t500)
      t526 = FJET(XB1, XB2, s, -t455, -t464, t444, t454, -t305, t500)
      rrgg2qqbarhhardt7s3e0 = t236 * t235 + t238 * t235 + t240 * t235 + 
     #t287 * t286 + t334 * t333 + t427 * t426 + t429 * t426 + t431 * t28
     #6 + t433 * t235 + t435 * t333 + t437 * t286 + t439 * t333 + t501 *
     # pi * t504 * t498 / 0.8D1 + t508 * pi * t504 * t498 / 0.8D1 + t513
     # * t426 + t515 * t333 + t517 * t426 + t519 * t286 + t521 * pi * t5
     #04 * t498 / 0.8D1 + t526 * pi * t504 * t498 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt7s3em1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = -0.1D1 + x3
      t19 = log(-0.4D1 * t10 * t13 / t14)
      t21 = cos(t11)
      t22 = x3 * z
      t24 = Sqrt(-t22 * t14)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t24)
      t32 = log(0.4D1 * t10 * t13)
      t40 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t49 = 0.1D1 / x3
      t52 = x1 ** 2
      t53 = t52 * t13
      t56 = log(0.4D1 * t53 * t9)
      t61 = lh * pi
      t62 = t4 * t6
      t66 = 0.1D1 / x1
      t76 = t5 * z
      t78 = 0.1D1 / x2
      t79 = t49 * t78
      t83 = x2 ** 2
      t84 = t9 * t83
      t85 = -0.1D1 + x2
      t89 = log(-0.4D1 * t84 * t13 * t85)
      t92 = log(0.4D1 * t84 * t13)
      t101 = log(0.4D1 * t9 * t13)
      t110 = t101 ** 2
      t112 = lh ** 2
      t114 = pi ** 2
      t120 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t123 = (0.90D2 * t5 * t6 * (t19 * z * t28 + t32) + (-0.180D3 * pi 
     #* t6 * lh + 0.90D2 * pi * t40) * t4 * (-z * t28 - 0.1D1)) * t49 / 
     #0.2880D4 - (0.90D2 * t5 * (t40 - t56 * t6) - 0.180D3 * t61 * t62) 
     #* t66 / 0.1440D4 + t5 * (-z * t6 * t28 - t6) * t49 * t66 / 0.16D2 
     #- t76 * t6 * t28 * t79 / 0.16D2 - t5 * t6 * (t89 - t92) * t78 / 0.
     #16D2 - (-0.180D3 * lh - 0.90D2 * t101) * pi * t4 * t40 / 0.2880D4 
     #- (0.180D3 * t101 * lh + 0.45D2 * t110 + 0.180D3 * t112 - 0.30D2 *
     # t114) * pi * t62 / 0.2880D4 - t5 * t120 / 0.32D2
      t124 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t123)
      t126 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t123)
      t128 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t123)
      t130 = x2 * x3
      t132 = 0.1D1 / (0.1D1 - x3 + t130)
      t134 = t2 * t130 * t132
      t136 = t2 * t14 * t132
      t139 = Sqrt(t22 * t85 * t14)
      t144 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t136, t134, 0.0D0)
      t146 = 0.1D1 / (-z - x3 + t130 + 0.2D1 * t21 * t139) * t144 * t79
      t148 = t76 * t146 / 0.16D2
      t149 = FJET(XB1, XB2, s, 0.0D0, t134, 0.0D0, -t136, 0.0D0, t148)
      t151 = t4 * z
      t156 = t1 * x1
      t157 = x1 * z
      t158 = -z - x1 + t157
      t159 = 0.1D1 / t158
      t161 = t85 * s * t156 * t159
      t162 = -0.1D1 + x1
      t163 = t2 * t162
      t165 = x2 * s * t156
      t166 = t1 ** 2
      t167 = s * t166
      t170 = x1 * t162 * t159
      t171 = t167 * t85 * t170
      t172 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t165, t1
     #61, -t163, 0.0D0, -t171)
      t174 = t172 * t78 * t66
      t176 = t5 * t174 / 0.8D1
      t177 = FJET(XB1, XB2, s, 0.0D0, t161, -t163, t165, -t171, -t176)
      t183 = t2 * x1 * t159
      t184 = t167 * t170
      t185 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t183, -t163, 0.0D0, t184)
      t190 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t183, -t163, 0.0D0, t184)
      t193 = t162 ** 2
      t197 = log(-0.4D1 * t53 / t7 * t159 * t193)
      t211 = x3 * t52
      t220 = Sqrt(x3 * t158 * t14)
      t233 = t5 * t185 * t78 * t66 / 0.8D1 - (0.90D2 * t5 * (-t190 + t19
     #7 * t185) + 0.180D3 * t61 * t4 * t185) * t66 / 0.1440D4 + t5 * (t1
     #85 - z * t158 * t185 / (-t157 - x3 * x1 * z - t22 - t211 + x1 * t7
     # + x3 * t7 * x1 + 0.2D1 * t211 * z - t211 * t7 - t7 + 0.2D1 * t21 
     #* t220 * z)) * t49 * t66 / 0.16D2
      t234 = FJET(XB1, XB2, s, 0.0D0, -t163, -t183, 0.0D0, t184, t233)
      t236 = FJET(XB1, XB2, s, 0.0D0, -t183, -t163, 0.0D0, t184, t233)
      t238 = FJET(XB1, XB2, s, 0.0D0, -t136, 0.0D0, t134, 0.0D0, t148)
      t243 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t123)
      t245 = FJET(XB1, XB2, s, t165, -t163, t161, 0.0D0, -t171, -t176)
      t250 = FJET(XB1, XB2, s, t134, 0.0D0, -t136, 0.0D0, 0.0D0, t148)
      t255 = FJET(XB1, XB2, s, t161, 0.0D0, t165, -t163, -t171, -t176)
      t260 = FJET(XB1, XB2, s, -t163, 0.0D0, 0.0D0, -t183, t184, t233)
      t262 = FJET(XB1, XB2, s, -t163, t165, 0.0D0, t161, -t171, -t176)
      t267 = FJET(XB1, XB2, s, -t183, 0.0D0, 0.0D0, -t163, t184, t233)
      t269 = FJET(XB1, XB2, s, -t136, 0.0D0, t134, 0.0D0, 0.0D0, t148)
      rrgg2qqbarhhardt7s3em1 = t124 * t123 + t126 * t123 + t128 * t123 +
     # t149 * pi * t151 * t146 / 0.16D2 - t177 * pi * t4 * t174 / 0.8D1 
     #+ t234 * t233 + t236 * t233 + t238 * pi * t151 * t146 / 0.16D2 + t
     #243 * t123 - t245 * pi * t4 * t174 / 0.8D1 + t250 * pi * t151 * t1
     #46 / 0.16D2 - t255 * pi * t4 * t174 / 0.8D1 + t260 * t233 - t262 *
     # pi * t4 * t174 / 0.8D1 + t267 * t233 + t269 * pi * t151 * t146 / 
     #0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s3em2
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = x4 * pi
      t12 = cos(t11)
      t16 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t28 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t32 = z ** 2
      t35 = Sin(t11)
      t36 = t35 ** 2
      t39 = log(0.4D1 / t32 / z * t36)
      t46 = -t5 * t6 * t7 / 0.16D2 + t5 * t6 * (-z / (-z - x3 + 0.2D1 * 
     #t12 * t16) - 0.1D1) / x3 / 0.32D2 - t5 * t28 / 0.32D2 - (-0.180D3 
     #* lh - 0.90D2 * t39) * pi * t4 * t6 / 0.2880D4
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t46)
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t46)
      t51 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t46)
      t53 = -0.1D1 + x1
      t54 = t2 * t53
      t57 = 0.1D1 / (-z - x1 + x1 * z)
      t59 = t2 * x1 * t57
      t60 = t1 ** 2
      t64 = s * t60 * x1 * t53 * t57
      t65 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #59, -t54, 0.0D0, t64)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, -t54, -t59, 0.0D0, t64, t68)
      t72 = t4 * t65 * t7
      t75 = FJET(XB1, XB2, s, 0.0D0, -t59, -t54, 0.0D0, t64, t68)
      t79 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t46)
      t81 = FJET(XB1, XB2, s, -t54, 0.0D0, 0.0D0, -t59, t64, t68)
      t85 = FJET(XB1, XB2, s, -t59, 0.0D0, 0.0D0, -t54, t64, t68)
      rrgg2qqbarhhardt7s3em2 = t47 * t46 + t49 * t46 + t51 * t46 + t69 *
     # pi * t72 / 0.16D2 + t75 * pi * t72 / 0.16D2 + t79 * t46 + t81 * p
     #i * t72 / 0.16D2 + t85 * pi * t72 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s3em3
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarhhardt7s3em3 = -t9 * pi * t11 / 0.32D2 - t13 * pi * t11 
     #/ 0.32D2 - t16 * pi * t11 / 0.32D2 - t19 * pi * t11 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s3em4
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt7s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt7s4e1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t4 = -0.1D1 + x1
      t5 = t3 * t4
      t6 = x2 * x3
      t7 = -0.1D1 + t6
      t8 = 0.1D1 / t7
      t10 = t2 * t5 * t8
      t11 = s * t3
      t12 = t11 * x1
      t13 = -0.1D1 + x2
      t14 = x3 * z
      t15 = x3 * x1
      t16 = x1 * z
      t17 = t15 * z
      t18 = x4 * pi
      t19 = cos(t18)
      t20 = -z - x1 + t16
      t22 = x2 * t1
      t24 = Sqrt(x3 * t20 * t22)
      t25 = t19 * t24
      t26 = 0.2D1 * t25
      t29 = 0.1D1 / t20
      t32 = t12 * t13 * (-t6 - z + t14 - x1 + t15 + t16 - t17 + t26) * t
     #29 * t8
      t33 = t11 * t4
      t35 = x3 * t13 * t8
      t36 = t33 * t35
      t37 = t6 * z
      t39 = t6 * x1
      t41 = x2 ** 2
      t42 = t41 * x3
      t43 = t42 * x1
      t47 = t42 * t16
      t50 = t17 + 0.2D1 * t37 + 0.2D1 * t39 - t43 - t42 * z + t6 - t14 -
     # t15 - 0.2D1 * t6 * t16 + t47 - x2 + 0.2D1 * t25 * x2 - t26
      t53 = t12 * t50 * t29 * t8
      t54 = t3 ** 2
      t59 = s * t54 * x2 * x1 * t4 * t29
      t60 = s ** 2
      t61 = 0.1D1 / t60
      t62 = pi * t61
      t63 = z ** 2
      t64 = x2 * x1
      t65 = t64 * z
      t66 = x1 ** 2
      t67 = x3 * t66
      t68 = t67 * x2
      t70 = x2 * t66
      t81 = -t63 + t65 - t37 - t39 - t68 + t43 - t64 * t63 - 0.2D1 * t70
     # * z + t70 * t63 + x3 * t63 * t64 + 0.2D1 * t67 * x2 * z - t67 * t
     #63 * x2
      t98 = -t47 - 0.2D1 * t25 * t64 - 0.2D1 * t25 * t16 - 0.2D1 * t16 +
     # 0.2D1 * t25 * z + 0.2D1 * t25 * x1 + 0.2D1 * t25 * t65 + 0.2D1 * 
     #x1 * t63 + 0.2D1 * t66 * z - t66 * t63 + t70 - t66
      t100 = 0.1D1 / (t81 + t98)
      t101 = t20 * t100
      t102 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t32, -t5
     #3, -t10, -t36, t59)
      t105 = Sin(t18)
      t106 = t105 ** 2
      t107 = 0.1D1 / t63
      t108 = t106 * t107
      t111 = t7 ** 2
      t113 = t13 ** 2
      t114 = 0.1D1 / t111 * t113
      t115 = t4 ** 2
      t121 = log(0.4D1 * t66 * t29 * t108 * t1 * t114 * t115 * x2 * x3)
      t123 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t32, -t5
     #3, -t10, -t36, t59)
      t127 = z + t65 - t64 + x1 - t16
      t131 = lh * pi
      t132 = t131 * t61
      t137 = -0.90D2 * t62 * (t101 * t102 - t121 * t20 * t100 * t123) * 
     #t127 + 0.180D3 * t132 * t101 * t123 * t127
      t138 = 0.1D1 / x3
      t140 = 0.1D1 / x2
      t141 = 0.1D1 / x1
      t142 = t140 * t141
      t144 = t137 * t138 * t142 / 0.720D3
      t145 = FJET(XB1, XB2, s, -t10, t32, -t36, -t53, t59, -t144)
      t148 = t138 * t140 * t141
      t151 = FJET(XB1, XB2, s, -t53, -t36, t32, -t10, t59, -t144)
      t156 = 0.1D1 / t63 / z
      t157 = t156 * t106
      t160 = log(0.4D1 * t67 * t157)
      t161 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t11, 0.0D0, 0.0D0)
      t163 = t160 ** 2
      t164 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t11, 0.0D0, 0.0D0)
      t167 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t11, 0.0D0, 0.0D0)
      t176 = lh ** 2
      t177 = 0.180D3 * t176
      t178 = pi ** 2
      t179 = 0.30D2 * t178
      t180 = t177 - t179
      t181 = t180 * pi
      t182 = t61 * t164
      t183 = t181 * t182
      t188 = t66 * t106
      t189 = t188 * t156
      t191 = log(0.4D1 * t189)
      t196 = t191 ** 2
      t199 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t11, 0.0D0, 0.0D0)
      t208 = 0.60D2 * lh * t178
      t209 = 0.240D3 * zeta3
      t211 = 0.120D3 * t176 * lh
      t212 = t208 - t209 - t211
      t213 = t212 * pi
      t214 = t213 * t182
      t227 = log(0.4D1 * t6 * t189)
      t229 = t157 * t113
      t232 = log(0.4D1 * t68 * t229)
      t240 = log(0.4D1 * t70 * t229)
      t242 = t240 ** 2
      t247 = log(0.4D1 * t70 * t157)
      t249 = t247 ** 2
      t265 = x3 * t156
      t268 = log(0.4D1 * t265 * t106)
      t273 = t268 ** 2
      t295 = log(0.4D1 * t6 * t157)
      t297 = t295 ** 2
      t302 = log(0.4D1 * t6 * t229)
      t304 = t302 ** 2
      t325 = x2 * t156
      t329 = log(0.4D1 * t325 * t106 * t113)
      t330 = t329 ** 2
      t333 = log(0.4D1 * t325 * t106)
      t334 = t333 ** 2
      t345 = t61 * t161
      t357 = log(0.4D1 * t157)
      t358 = t357 ** 2
      t361 = t358 * t357
      t383 = rrgg2qqbarhhard71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t11, 0.0D0, 0.0D0)
      t391 = t178 ** 2
      t392 = t176 ** 2
      t398 = t358 ** 2
      t404 = -(0.90D2 * t62 * (-t160 * t161 + t163 * t164 / 0.2D1 + t167
     #) - 0.180D3 * t131 * t61 * (t161 - t160 * t164) + t183) * t138 * t
     #141 / 0.720D3 + (t181 * t61 * (-t161 + t191 * t164) + 0.90D2 * t62
     # * (-t196 * t161 / 0.2D1 - t199 + t196 * t191 * t164 / 0.6D1 + t19
     #1 * t167) - t214 - 0.180D3 * t131 * t61 * (t191 * t161 - t196 * t1
     #64 / 0.2D1 - t167)) * t141 / 0.720D3 - t62 * (-t227 * t164 + t232 
     #* t164) * t148 / 0.8D1 - (0.90D2 * t62 * (t240 * t161 - t242 * t16
     #4 / 0.2D1 - t247 * t161 + t249 * t164 / 0.2D1) - 0.180D3 * t131 * 
     #t61 * (t240 * t164 - t247 * t164)) * t140 * t141 / 0.720D3 + (t181
     # * t61 * (-t161 + t268 * t164) + 0.90D2 * t62 * (-t273 * t161 / 0.
     #2D1 - t199 + t273 * t268 * t164 / 0.6D1 + t268 * t167) - t214 - 0.
     #180D3 * t131 * t61 * (t268 * t161 - t273 * t164 / 0.2D1 - t167)) *
     # t138 / 0.1440D4 - (0.90D2 * t62 * (-t295 * t161 + t297 * t164 / 0
     #.2D1 + t302 * t161 - t304 * t164 / 0.2D1) - 0.180D3 * t131 * t61 *
     # (-t295 * t164 + t302 * t164)) * t138 * t140 / 0.1440D4 - ((0.90D2
     # * t62 * t161 - 0.180D3 * t131 * t182) * (-t330 / 0.2D1 + t334 / 0
     #.2D1) + 0.90D2 * t62 * t164 * (-t334 * t333 / 0.6D1 + t330 * t329 
     #/ 0.6D1) + (-0.180D3 * t131 * t345 + t183 + 0.90D2 * t62 * t167) *
     # (t329 - t333)) * t140 / 0.1440D4 - (-0.90D2 * t358 * lh + t208 - 
     #t209 - t211 - 0.15D2 * t361 - t357 * t180) * pi * t345 / 0.1440D4 
     #- (0.180D3 * t357 * lh + 0.45D2 * t358 + t177 - t179) * pi * t61 *
     # t167 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t357) * pi * t61 * t1
     #99 / 0.1440D4 - t62 * t383 / 0.16D2 - (0.30D2 * t361 * lh + t358 *
     # t180 / 0.2D1 - t357 * t212 + t391 + 0.60D2 * t392 + 0.480D3 * lh 
     #* zeta3 - 0.60D2 * t176 * t178 + 0.15D2 / 0.4D1 * t398) * pi * t18
     #2 / 0.1440D4
      t405 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t11, 0.0D0, t404)
      t407 = FJET(XB1, XB2, s, t32, -t10, -t53, -t36, t59, -t144)
      t411 = FJET(XB1, XB2, s, -t36, -t53, -t10, t32, t59, -t144)
      t416 = t107 * t29
      t417 = t416 * t115
      t420 = log(-0.4D1 * t67 * t106 * t417)
      t421 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t12, 0.0
     #D0, -t33, 0.0D0, 0.0D0)
      t423 = t420 ** 2
      t424 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t12, 0.0
     #D0, -t33, 0.0D0, 0.0D0)
      t427 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t12, 0.0
     #D0, -t33, 0.0D0, 0.0D0)
      t436 = t61 * t424
      t437 = t181 * t436
      t443 = log(-0.4D1 * t188 * t417)
      t448 = t443 ** 2
      t451 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, t12, 0.0
     #D0, -t33, 0.0D0, 0.0D0)
      t469 = t29 * t115
      t473 = log(-0.4D1 * t68 * t108 * t469)
      t483 = t70 * t106
      t486 = log(-0.4D1 * t483 * t417)
      t488 = t486 ** 2
      t503 = -(0.90D2 * t62 * (t420 * t421 - t423 * t424 / 0.2D1 - t427)
     # - 0.180D3 * t131 * t61 * (-t421 + t420 * t424) - t437) * t138 * t
     #141 / 0.720D3 + (t181 * t61 * (t421 - t443 * t424) + 0.90D2 * t62 
     #* (t448 * t421 / 0.2D1 + t451 - t448 * t443 * t424 / 0.6D1 - t443 
     #* t427) + t213 * t436 - 0.180D3 * t131 * t61 * (-t443 * t421 + t44
     #8 * t424 / 0.2D1 + t427)) * t141 / 0.720D3 - (0.90D2 * t62 * (-t42
     #1 + t473 * t424) + 0.180D3 * t131 * t436) * t138 * t142 / 0.720D3 
     #- (0.90D2 * t62 * (t486 * t421 - t488 * t424 / 0.2D1 - t427) - 0.1
     #80D3 * t131 * t61 * (-t421 + t486 * t424) - t437) * t140 * t141 / 
     #0.720D3
      t504 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t33, t12, 0.0D0, t503)
      t507 = t3 * x1
      t508 = t13 * s * t507
      t510 = t11 * t64 * t29
      t511 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t508, -
     #t510, -t33, 0.0D0, t59)
      t514 = t416 * t115 * t113
      t517 = log(-0.4D1 * t6 * t188 * t514)
      t518 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t508, -
     #t510, -t33, 0.0D0, t59)
      t523 = t61 * t518
      t531 = log(-0.4D1 * t483 * t514)
      t533 = t531 ** 2
      t536 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t508, -
     #t510, -t33, 0.0D0, t59)
      t550 = -(0.90D2 * t62 * (t511 - t517 * t518) - 0.180D3 * t131 * t5
     #23) * t138 * t142 / 0.720D3 - (0.90D2 * t62 * (-t531 * t511 + t533
     # * t518 / 0.2D1 + t536) - 0.180D3 * t131 * t61 * (t511 - t531 * t5
     #18) + t181 * t523) * t140 * t141 / 0.720D3
      t551 = FJET(XB1, XB2, s, -t508, -t33, -t510, 0.0D0, t59, t550)
      t554 = t11 * t1 * t8
      t555 = t11 * t35
      t556 = t157 * t1
      t557 = t114 * t6
      t560 = log(-0.4D1 * t556 * t557)
      t561 = t560 * z
      t562 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t554, t555, 0.0D0)
      t564 = t560 ** 2
      t566 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t554, t555, 0.0D0)
      t569 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t554, t555, 0.0D0)
      t573 = Sqrt(-t14 * t22)
      t577 = 0.1D1 / (-z - t6 + 0.2D1 * t19 * t573)
      t581 = z * t562
      t590 = z * t566 * t577
      t600 = log(-0.4D1 * t188 * t156 * t1 * t557)
      t613 = -(-0.90D2 * t62 * (-t561 * t562 + t564 * z * t566 / 0.2D1 +
     # z * t569) * t577 + 0.180D3 * t131 * t61 * (t581 - t561 * t566) * 
     #t577 - t181 * t61 * t590) * t138 * t140 / 0.1440D4 - (-0.90D2 * t6
     #2 * (t581 - t600 * z * t566) * t577 + 0.180D3 * t132 * t590) * t13
     #8 * t142 / 0.720D3
      t614 = FJET(XB1, XB2, s, t554, 0.0D0, t555, 0.0D0, 0.0D0, t613)
      t616 = FJET(XB1, XB2, s, -t33, t12, 0.0D0, 0.0D0, 0.0D0, t503)
      t618 = FJET(XB1, XB2, s, 0.0D0, t554, 0.0D0, t555, 0.0D0, t613)
      t620 = t2 * t507
      t621 = t2 * t5
      t622 = t11 * t15
      t624 = x3 * s * t5
      t627 = x3 * t1 * t469
      t630 = log(0.4D1 * t188 * t107 * t627)
      t631 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t620, t
     #622, t621, -t624, 0.0D0)
      t633 = t630 ** 2
      t634 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t620, t
     #622, t621, -t624, 0.0D0)
      t637 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, -t620, t
     #622, t621, -t624, 0.0D0)
      t646 = t61 * t634
      t654 = log(0.4D1 * t70 * t108 * t627)
      t665 = -(0.90D2 * t62 * (-t630 * t631 + t633 * t634 / 0.2D1 + t637
     #) - 0.180D3 * t131 * t61 * (t631 - t630 * t634) + t181 * t646) * t
     #138 * t141 / 0.720D3 - (0.90D2 * t62 * (t631 - t654 * t634) - 0.18
     #0D3 * t131 * t646) * t138 * t142 / 0.720D3
      t666 = FJET(XB1, XB2, s, -t620, t621, t622, -t624, 0.0D0, t665)
      t668 = FJET(XB1, XB2, s, t555, 0.0D0, t554, 0.0D0, 0.0D0, t613)
      t670 = t11 * x3
      t671 = t11 * t1
      t674 = log(-0.4D1 * t67 * t556)
      t675 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t671, t670, 0.0D0)
      t677 = t674 ** 2
      t678 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t671, t670, 0.0D0)
      t681 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t671, t670, 0.0D0)
      t690 = t61 * t678
      t691 = t181 * t690
      t695 = (0.90D2 * t62 * (t674 * t675 - t677 * t678 / 0.2D1 - t681) 
     #- 0.180D3 * t131 * t61 * (-t675 + t674 * t678) - t691) * t138 * t1
     #41 / 0.720D3
      t699 = log(-0.4D1 * t483 * t265 * t1)
      t709 = (0.90D2 * t62 * (-t675 + t699 * t678) + 0.180D3 * t131 * t6
     #90) * t138 * t142 / 0.720D3
      t713 = log(-0.4D1 * t265 * t106 * t1)
      t715 = t675 - t713 * t678
      t718 = t713 ** 2
      t721 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t671, t670, 0.0D0)
      t726 = t718 * t675 / 0.2D1 + t721 - t718 * t713 * t678 / 0.6D1 - t
     #713 * t681
      t729 = t213 * t690
      t733 = -t713 * t675 + t718 * t678 / 0.2D1 + t681
      t744 = log(-0.4D1 * t325 * t106 * x3 * t1)
      t746 = t744 ** 2
      t760 = (0.90D2 * t62 * (t744 * t675 - t746 * t678 / 0.2D1 - t681) 
     #- 0.180D3 * t131 * t61 * (-t675 + t744 * t678) - t691) * t138 * t1
     #40 / 0.1440D4
      t761 = -t695 - t709 + (t181 * t61 * t715 + 0.90D2 * t62 * t726 + t
     #729 - 0.180D3 * t131 * t61 * t733) * t138 / 0.1440D4 - t760
      t762 = FJET(XB1, XB2, s, 0.0D0, t670, 0.0D0, -t671, 0.0D0, t761)
      t764 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t12, -t33, 0.0D0, t503)
      t766 = -t145 * t137 * t148 / 0.720D3 - t151 * t137 * t148 / 0.720D
     #3 + t405 * t404 - t407 * t137 * t148 / 0.720D3 - t411 * t137 * t14
     #8 / 0.720D3 + t504 * t503 + t551 * t550 + t614 * t613 + t616 * t50
     #3 + t618 * t613 + t666 * t665 + t668 * t613 + t762 * t761 + t764 *
     # t503
      t767 = FJET(XB1, XB2, s, 0.0D0, t555, 0.0D0, t554, 0.0D0, t613)
      t782 = -t695 - t709 + (t181 * t61 * t715 + 0.90D2 * t62 * t726 + t
     #729 - 0.180D3 * t131 * t61 * t733) * t138 / 0.1440D4 - t760
      t783 = FJET(XB1, XB2, s, -t671, 0.0D0, t670, 0.0D0, 0.0D0, t782)
      t785 = FJET(XB1, XB2, s, t621, -t620, -t624, t622, 0.0D0, t665)
      t787 = FJET(XB1, XB2, s, t12, -t33, 0.0D0, 0.0D0, 0.0D0, t503)
      t789 = FJET(XB1, XB2, s, 0.0D0, -t510, -t33, -t508, t59, t550)
      t791 = FJET(XB1, XB2, s, 0.0D0, t11, 0.0D0, 0.0D0, 0.0D0, t404)
      t793 = FJET(XB1, XB2, s, t670, 0.0D0, -t671, 0.0D0, 0.0D0, t782)
      t795 = FJET(XB1, XB2, s, t622, -t624, -t620, t621, 0.0D0, t665)
      t797 = FJET(XB1, XB2, s, -t510, 0.0D0, -t508, -t33, t59, t550)
      t799 = FJET(XB1, XB2, s, -t624, t622, t621, -t620, 0.0D0, t665)
      t801 = FJET(XB1, XB2, s, 0.0D0, -t671, 0.0D0, t670, 0.0D0, t761)
      t803 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t11, 0.0D0, 0.0D0, t404)
      t805 = FJET(XB1, XB2, s, -t33, -t508, 0.0D0, -t510, t59, t550)
      t807 = FJET(XB1, XB2, s, t11, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t404)
      t809 = t767 * t613 + t783 * t782 + t785 * t665 + t787 * t503 + t78
     #9 * t550 + t791 * t404 + t793 * t782 + t795 * t665 + t797 * t550 +
     # t799 * t665 + t801 * t761 + t803 * t404 + t805 * t550 + t807 * t4
     #04
      rrgg2qqbarhhardt7s4e1 = t766 + t809

      end function



      doubleprecision function rrgg2qqbarhhardt7s4e0
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, -t5, t3, 0.0D0)
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = z ** 2
      t17 = 0.1D1 / t15 / z
      t18 = t14 * t17
      t19 = t18 * t4
      t22 = log(-0.4D1 * t11 * t19)
      t23 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, -t5, t3, 0.0D0)
      t28 = lh * pi
      t29 = t7 * t23
      t31 = 0.180D3 * t28 * t29
      t33 = 0.1D1 / x3
      t35 = 0.1D1 / x1
      t37 = (0.90D2 * t8 * (-t9 + t22 * t23) + t31) * t33 * t35 / 0.720D
     #3
      t39 = 0.1D1 / x2
      t41 = t33 * t39 * t35
      t43 = t8 * t23 * t41 / 0.8D1
      t44 = x2 * t17
      t49 = log(-0.4D1 * t44 * t14 * x3 * t4)
      t57 = (0.90D2 * t8 * (-t9 + t49 * t23) + t31) * t33 * t39 / 0.1440
     #D4
      t58 = x3 * t17
      t62 = log(-0.4D1 * t58 * t14 * t4)
      t64 = t62 ** 2
      t67 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, -t5, t3, 0.0D0)
      t68 = t62 * t9 - t64 * t23 / 0.2D1 - t67
      t72 = -t9 + t62 * t23
      t76 = lh ** 2
      t77 = 0.180D3 * t76
      t78 = pi ** 2
      t79 = 0.30D2 * t78
      t80 = t77 - t79
      t81 = t80 * pi
      t82 = t81 * t29
      t86 = -t37 + t43 - t57 + (-0.90D2 * t8 * t68 + 0.180D3 * t28 * t7 
     #* t72 + t82) * t33 / 0.1440D4
      t87 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t86)
      t89 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t86)
      t91 = -0.1D1 + x2
      t93 = x2 * x3
      t94 = -0.1D1 + t93
      t95 = 0.1D1 / t94
      t96 = x3 * t91 * t95
      t97 = t2 * t96
      t99 = t2 * t4 * t95
      t100 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t99, t97, 0.0D0)
      t102 = t94 ** 2
      t104 = t91 ** 2
      t109 = log(-0.4D1 * t19 / t102 * t104 * t93)
      t111 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t99, t97, 0.0D0)
      t114 = cos(t12)
      t115 = x3 * z
      t116 = x2 * t4
      t118 = Sqrt(-t115 * t116)
      t122 = 0.1D1 / (-z - t93 + 0.2D1 * t114 * t118)
      t127 = z * t111
      t141 = -(-0.90D2 * t8 * (z * t100 - t109 * z * t111) * t122 + 0.18
     #0D3 * t28 * t7 * t127 * t122) * t33 * t39 / 0.1440D4 + t8 * t127 *
     # t122 * t33 * t39 * t35 / 0.8D1
      t142 = FJET(XB1, XB2, s, 0.0D0, t97, 0.0D0, t99, 0.0D0, t141)
      t144 = t2 * x1
      t145 = -0.1D1 + x1
      t146 = t2 * t145
      t147 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t144, 0.
     #0D0, -t146, 0.0D0, 0.0D0)
      t149 = 0.1D1 / t15
      t150 = x1 * z
      t151 = -z - x1 + t150
      t152 = 0.1D1 / t151
      t153 = t149 * t152
      t154 = t145 ** 2
      t155 = t153 * t154
      t158 = log(-0.4D1 * t11 * t14 * t155)
      t159 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t144, 0.
     #0D0, -t146, 0.0D0, 0.0D0)
      t164 = t7 * t159
      t166 = 0.180D3 * t28 * t164
      t174 = x2 * t10
      t175 = t174 * t14
      t178 = log(-0.4D1 * t175 * t155)
      t187 = t10 * t14
      t190 = log(-0.4D1 * t187 * t155)
      t192 = t190 ** 2
      t195 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, t144, 0.
     #0D0, -t146, 0.0D0, 0.0D0)
      t208 = -(0.90D2 * t8 * (-t147 + t158 * t159) + t166) * t33 * t35 /
     # 0.720D3 + t8 * t159 * t41 / 0.8D1 - (0.90D2 * t8 * (-t147 + t178 
     #* t159) + t166) * t39 * t35 / 0.720D3 + (0.90D2 * t8 * (-t190 * t1
     #47 + t192 * t159 / 0.2D1 + t195) - 0.180D3 * t28 * t7 * (t147 - t1
     #90 * t159) + t81 * t164) * t35 / 0.720D3
      t209 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t144, -t146, 0.0D0, t208)
      t211 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t214 = log(0.4D1 * t11 * t18)
      t215 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t220 = t7 * t215
      t222 = 0.180D3 * t28 * t220
      t227 = t18 * t104
      t230 = log(0.4D1 * t174 * t227)
      t234 = log(0.4D1 * t174 * t18)
      t243 = log(0.4D1 * t187 * t17)
      t245 = t243 ** 2
      t248 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t257 = t81 * t220
      t263 = log(0.4D1 * t58 * t14)
      t265 = t263 ** 2
      t280 = log(0.4D1 * t18)
      t283 = t280 ** 2
      t290 = rrgg2qqbarhhard71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t316 = log(0.4D1 * t93 * t18)
      t320 = log(0.4D1 * t93 * t227)
      t330 = log(0.4D1 * t44 * t14 * t104)
      t331 = t330 ** 2
      t334 = log(0.4D1 * t44 * t14)
      t335 = t334 ** 2
      t349 = -(0.90D2 * t8 * (t211 - t214 * t215) - t222) * t33 * t35 / 
     #0.720D3 - t8 * (t230 * t215 - t234 * t215) * t39 * t35 / 0.8D1 + (
     #0.90D2 * t8 * (t243 * t211 - t245 * t215 / 0.2D1 - t248) - 0.180D3
     # * t28 * t7 * (-t211 + t243 * t215) - t257) * t35 / 0.720D3 + (0.9
     #0D2 * t8 * (t263 * t211 - t265 * t215 / 0.2D1 - t248) - 0.180D3 * 
     #t28 * t7 * (-t211 + t263 * t215) - t257) * t33 / 0.1440D4 - (0.180
     #D3 * t280 * lh + 0.45D2 * t283 + t77 - t79) * pi * t7 * t211 / 0.1
     #440D4 - t8 * t290 / 0.16D2 - (-0.90D2 * t283 * lh + 0.60D2 * lh * 
     #t78 - 0.240D3 * zeta3 - 0.120D3 * t76 * lh - 0.15D2 * t283 * t280 
     #- t280 * t80) * pi * t220 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t
     #280) * pi * t7 * t248 / 0.1440D4 - t8 * (-t316 * t215 + t320 * t21
     #5) * t33 * t39 / 0.16D2 - (0.90D2 * t8 * t215 * (-t331 / 0.2D1 + t
     #335 / 0.2D1) + (0.90D2 * t8 * t211 - t222) * (t330 - t334)) * t39 
     #/ 0.1440D4
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t349)
      t352 = x3 * x1
      t353 = t352 * z
      t354 = t93 * z
      t356 = t93 * x1
      t358 = x2 ** 2
      t359 = t358 * x3
      t360 = t359 * x1
      t364 = t359 * t150
      t367 = Sqrt(x3 * t151 * t116)
      t368 = t114 * t367
      t371 = 0.2D1 * t368
      t372 = t353 + 0.2D1 * t354 + 0.2D1 * t356 - t360 - t359 * z + t93 
     #- t115 - t352 - 0.2D1 * t93 * t150 + t364 - x2 + 0.2D1 * t368 * x2
     # - t371
      t375 = t144 * t372 * t152 * t95
      t376 = t146 * t96
      t381 = t144 * t91 * (-t93 - z + t115 - x1 + t352 + t150 - t353 + t
     #371) * t152 * t95
      t382 = t4 * s
      t383 = t1 * t145
      t385 = t382 * t383 * t95
      t386 = t1 ** 2
      t391 = s * t386 * x2 * x1 * t145 * t152
      t392 = x2 * x1
      t393 = t392 * z
      t406 = -t15 + t393 - t354 - t356 - t11 * x2 + t360 - t392 * t15 - 
     #0.2D1 * t174 * z + t174 * t15 + x3 * t15 * t392 + 0.2D1 * t11 * x2
     # * z - t11 * t15 * x2
      t423 = -t364 - 0.2D1 * t368 * t392 - 0.2D1 * t368 * t150 - 0.2D1 *
     # t150 + 0.2D1 * t368 * z + 0.2D1 * t368 * x1 + 0.2D1 * t368 * t393
     # + 0.2D1 * x1 * t15 + 0.2D1 * t10 * z - t10 * t15 + t174 - t10
      t425 = 0.1D1 / (t406 + t423)
      t428 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t381, -t
     #375, -t385, -t376, t391)
      t431 = t428 * (z + t393 - t392 + x1 - t150) * t41
      t433 = t8 * t151 * t425 * t431 / 0.8D1
      t434 = FJET(XB1, XB2, s, -t375, -t376, t381, -t385, t391, t433)
      t437 = t7 * t151 * t425
      t441 = FJET(XB1, XB2, s, -t376, -t375, -t385, t381, t391, t433)
      t446 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t146, t144, 0.0D0, t208)
      t458 = -t37 + t43 - t57 + (-0.90D2 * t8 * t68 + 0.180D3 * t28 * t7
     # * t72 + t82) * t33 / 0.1440D4
      t459 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t458)
      t462 = t2 * t392 * t152
      t464 = t1 * x1
      t465 = t91 * s * t464
      t466 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t465, -
     #t462, -t146, 0.0D0, t391)
      t470 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t465, -
     #t462, -t146, 0.0D0, t391)
      t475 = log(-0.4D1 * t175 * t153 * t154 * t104)
      t487 = -t8 * t466 * t41 / 0.8D1 - (0.90D2 * t8 * (t470 - t475 * t4
     #66) - 0.180D3 * t28 * t7 * t466) * t39 * t35 / 0.720D3
      t488 = FJET(XB1, XB2, s, 0.0D0, -t462, -t146, -t465, t391, t487)
      t490 = FJET(XB1, XB2, s, t144, -t146, 0.0D0, 0.0D0, 0.0D0, t208)
      t492 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t349)
      t494 = FJET(XB1, XB2, s, t99, 0.0D0, t97, 0.0D0, 0.0D0, t141)
      t496 = t2 * t352
      t498 = x3 * s * t383
      t499 = t382 * t464
      t500 = t382 * t383
      t501 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, -t499, t
     #496, t500, -t498, 0.0D0)
      t508 = log(0.4D1 * t187 * t149 * x3 * t4 * t152 * t154)
      t509 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t499, t
     #496, t500, -t498, 0.0D0)
      t524 = -(0.90D2 * t8 * (t501 - t508 * t509) - 0.180D3 * t28 * t7 *
     # t509) * t33 * t35 / 0.720D3 - t8 * t509 * t41 / 0.8D1
      t525 = FJET(XB1, XB2, s, t496, -t498, -t499, t500, 0.0D0, t524)
      t527 = t87 * t86 + t89 * t86 + t142 * t141 + t209 * t208 + t350 * 
     #t349 + t434 * pi * t437 * t431 / 0.8D1 + t441 * pi * t437 * t431 /
     # 0.8D1 + t446 * t208 + t459 * t458 + t488 * t487 + t490 * t208 + t
     #492 * t349 + t494 * t141 + t525 * t524
      t528 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t349)
      t530 = FJET(XB1, XB2, s, -t146, t144, 0.0D0, 0.0D0, 0.0D0, t208)
      t532 = FJET(XB1, XB2, s, t381, -t385, -t375, -t376, t391, t433)
      t537 = FJET(XB1, XB2, s, 0.0D0, t99, 0.0D0, t97, 0.0D0, t141)
      t539 = FJET(XB1, XB2, s, -t462, 0.0D0, -t465, -t146, t391, t487)
      t541 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t349)
      t543 = FJET(XB1, XB2, s, -t465, -t146, -t462, 0.0D0, t391, t487)
      t545 = FJET(XB1, XB2, s, -t146, -t465, 0.0D0, -t462, t391, t487)
      t547 = FJET(XB1, XB2, s, t500, -t499, -t498, t496, 0.0D0, t524)
      t549 = FJET(XB1, XB2, s, -t499, t500, t496, -t498, 0.0D0, t524)
      t551 = FJET(XB1, XB2, s, -t498, t496, t500, -t499, 0.0D0, t524)
      t553 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t458)
      t555 = FJET(XB1, XB2, s, t97, 0.0D0, t99, 0.0D0, 0.0D0, t141)
      t557 = FJET(XB1, XB2, s, -t385, t381, -t376, -t375, t391, t433)
      t562 = t528 * t349 + t530 * t208 + t532 * pi * t437 * t431 / 0.8D1
     # + t537 * t141 + t539 * t487 + t541 * t349 + t543 * t487 + t545 * 
     #t487 + t547 * t524 + t549 * t524 + t551 * t524 + t553 * t458 + t55
     #5 * t141 + t557 * pi * t437 * t431 / 0.8D1
      rrgg2qqbarhhardt7s4e0 = t527 + t562

      end function



      doubleprecision function rrgg2qqbarhhardt7s4em1
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t22 = lh * pi
      t23 = t4 * t17
      t25 = 0.180D3 * t22 * t23
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t31 = t30 * t13
      t34 = log(0.4D1 * t31 * t9)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t48 = -0.1D1 + x2
      t49 = t48 ** 2
      t53 = log(0.4D1 * t47 * t13 * t49)
      t56 = log(0.4D1 * t47 * t13)
      t59 = 0.1D1 / x2
      t66 = log(0.4D1 * t9 * t13)
      t75 = t66 ** 2
      t77 = lh ** 2
      t79 = pi ** 2
      t85 = rrgg2qqbarhhard71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t88 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 + (
     #0.90D2 * t5 * (-t6 + t34 * t17) + t25) * t40 / 0.720D3 - t5 * t17 
     #* t27 * t40 / 0.8D1 - t5 * t17 * (t53 - t56) * t59 / 0.16D2 - (-0.
     #180D3 * lh - 0.90D2 * t66) * pi * t4 * t6 / 0.1440D4 - (0.180D3 * 
     #t66 * lh + 0.45D2 * t75 + 0.180D3 * t77 - 0.30D2 * t79) * pi * t23
     # / 0.1440D4 - t5 * t85 / 0.16D2
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t88)
      t91 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t88)
      t93 = t2 * x1
      t94 = -0.1D1 + x1
      t95 = t2 * t94
      t96 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t93, 0.0D
     #0, -t95, 0.0D0, 0.0D0)
      t101 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, t93, 0.0
     #D0, -t95, 0.0D0, 0.0D0)
      t105 = 0.1D1 / (-z - x1 + x1 * z)
      t107 = t94 ** 2
      t111 = log(-0.4D1 * t31 / t7 * t105 * t107)
      t126 = t5 * t96 * t59 * t40 / 0.8D1 + (0.90D2 * t5 * (t101 - t111 
     #* t96) - 0.180D3 * t22 * t4 * t96) * t40 / 0.720D3 + t5 * t96 * t2
     #7 * t40 / 0.8D1
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t93, -t95, 0.0D0, t126)
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t95, t93, 0.0D0, t126)
      t131 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t88)
      t133 = t2 * x3
      t134 = -0.1D1 + x3
      t135 = t2 * t134
      t136 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t135, t133, 0.0D0)
      t140 = log(-0.4D1 * t10 * t13 * t134)
      t141 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t135, t133, 0.0D0)
      t143 = t136 - t140 * t141
      t148 = 0.180D3 * t22 * t4 * t141
      t152 = t141 * t27
      t155 = t5 * t152 * t40 / 0.8D1
      t158 = t5 * t152 * t59 / 0.16D2
      t159 = (0.90D2 * t5 * t143 - t148) * t27 / 0.1440D4 + t155 + t158
      t160 = FJET(XB1, XB2, s, 0.0D0, t133, 0.0D0, -t135, 0.0D0, t159)
      t162 = x2 * x3
      t164 = 0.1D1 / (-0.1D1 + t162)
      t166 = t2 * t134 * t164
      t169 = t2 * x3 * t48 * t164
      t171 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t166, t169, 0.0D0)
      t172 = cos(t11)
      t176 = Sqrt(-x3 * z * x2 * t134)
      t183 = t171 / (-z - t162 + 0.2D1 * t172 * t176) * t27 * t59
      t185 = t5 * z * t183 / 0.16D2
      t186 = FJET(XB1, XB2, s, 0.0D0, t166, 0.0D0, t169, 0.0D0, t185)
      t188 = t4 * z
      t192 = FJET(XB1, XB2, s, 0.0D0, t169, 0.0D0, t166, 0.0D0, t185)
      t197 = FJET(XB1, XB2, s, 0.0D0, -t135, 0.0D0, t133, 0.0D0, t159)
      t201 = t2 * x1 * x2 * t105
      t203 = t1 * x1
      t204 = t48 * s * t203
      t205 = t1 ** 2
      t210 = s * t205 * x2 * x1 * t94 * t105
      t211 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t204, -
     #t201, -t95, 0.0D0, t210)
      t213 = t211 * t59 * t40
      t215 = t5 * t213 / 0.8D1
      t216 = FJET(XB1, XB2, s, 0.0D0, -t201, -t95, -t204, t210, -t215)
      t221 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t88)
      t223 = FJET(XB1, XB2, s, t93, -t95, 0.0D0, 0.0D0, 0.0D0, t126)
      t225 = t89 * t88 + t91 * t88 + t127 * t126 + t129 * t126 + t131 * 
     #t88 + t160 * t159 + t186 * pi * t188 * t183 / 0.16D2 + t192 * pi *
     # t188 * t183 / 0.16D2 + t197 * t159 - t216 * pi * t4 * t213 / 0.8D
     #1 + t221 * t88 + t223 * t126
      t232 = (0.90D2 * t5 * t143 - t148) * t27 / 0.1440D4 + t155 + t158
      t233 = FJET(XB1, XB2, s, t133, 0.0D0, -t135, 0.0D0, 0.0D0, t232)
      t236 = t2 * x1 * x3
      t238 = t1 * t94
      t239 = x3 * s * t238
      t240 = t134 * s
      t241 = t240 * t203
      t242 = t240 * t238
      t243 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, -t241, t
     #236, t242, -t239, 0.0D0)
      t245 = t243 * t27 * t40
      t247 = t5 * t245 / 0.8D1
      t248 = FJET(XB1, XB2, s, t236, -t239, -t241, t242, 0.0D0, -t247)
      t253 = FJET(XB1, XB2, s, t166, 0.0D0, t169, 0.0D0, 0.0D0, t185)
      t258 = FJET(XB1, XB2, s, t242, -t241, -t239, t236, 0.0D0, -t247)
      t263 = FJET(XB1, XB2, s, t169, 0.0D0, t166, 0.0D0, 0.0D0, t185)
      t268 = FJET(XB1, XB2, s, -t95, t93, 0.0D0, 0.0D0, 0.0D0, t126)
      t270 = FJET(XB1, XB2, s, -t95, -t204, 0.0D0, -t201, t210, -t215)
      t275 = FJET(XB1, XB2, s, -t135, 0.0D0, t133, 0.0D0, 0.0D0, t232)
      t277 = FJET(XB1, XB2, s, -t239, t236, t242, -t241, 0.0D0, -t247)
      t282 = FJET(XB1, XB2, s, -t204, -t95, -t201, 0.0D0, t210, -t215)
      t287 = FJET(XB1, XB2, s, -t241, t242, t236, -t239, 0.0D0, -t247)
      t292 = FJET(XB1, XB2, s, -t201, 0.0D0, -t204, -t95, t210, -t215)
      t297 = t233 * t232 - t248 * pi * t4 * t245 / 0.8D1 + t253 * pi * t
     #188 * t183 / 0.16D2 - t258 * pi * t4 * t245 / 0.8D1 + t263 * pi * 
     #t188 * t183 / 0.16D2 + t268 * t126 - t270 * pi * t4 * t213 / 0.8D1
     # + t275 * t232 - t277 * pi * t4 * t245 / 0.8D1 - t282 * pi * t4 * 
     #t213 / 0.8D1 - t287 * pi * t4 * t245 / 0.8D1 - t292 * pi * t4 * t2
     #13 / 0.8D1
      rrgg2qqbarhhardt7s4em1 = t225 + t297

      end function



      doubleprecision function rrgg2qqbarhhardt7s4em2
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t15 = rrgg2qqbarhhard71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t19 = z ** 2
      t23 = Sin(x4 * pi)
      t24 = t23 ** 2
      t27 = log(0.4D1 / t19 / z * t24)
      t34 = -t5 * t6 * t7 / 0.8D1 - t5 * t6 * t11 / 0.16D2 - t5 * t15 / 
     #0.16D2 - (-0.180D3 * lh - 0.90D2 * t27) * pi * t4 * t6 / 0.1440D4
      t35 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t34)
      t37 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t34)
      t39 = t2 * x1
      t41 = t2 * (-0.1D1 + x1)
      t42 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, t39, 0.0D
     #0, -t41, 0.0D0, 0.0D0)
      t45 = t5 * t42 * t7 / 0.8D1
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t39, -t41, 0.0D0, t45)
      t49 = t4 * t42 * t7
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t41, t39, 0.0D0, t45)
      t56 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t34)
      t58 = t2 * x3
      t60 = t2 * (-0.1D1 + x3)
      t61 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, -t60, t58, 0.0D0)
      t64 = t5 * t61 * t11 / 0.16D2
      t65 = FJET(XB1, XB2, s, 0.0D0, t58, 0.0D0, -t60, 0.0D0, t64)
      t68 = t4 * t61 * t11
      t71 = FJET(XB1, XB2, s, 0.0D0, -t60, 0.0D0, t58, 0.0D0, t64)
      t75 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t34)
      t77 = FJET(XB1, XB2, s, t39, -t41, 0.0D0, 0.0D0, 0.0D0, t45)
      t81 = FJET(XB1, XB2, s, t58, 0.0D0, -t60, 0.0D0, 0.0D0, t64)
      t85 = FJET(XB1, XB2, s, -t41, t39, 0.0D0, 0.0D0, 0.0D0, t45)
      t89 = FJET(XB1, XB2, s, -t60, 0.0D0, t58, 0.0D0, 0.0D0, t64)
      rrgg2qqbarhhardt7s4em2 = t35 * t34 + t37 * t34 + t46 * pi * t49 / 
     #0.8D1 + t52 * pi * t49 / 0.8D1 + t56 * t34 + t65 * pi * t68 / 0.16
     #D2 + t71 * pi * t68 / 0.16D2 + t75 * t34 + t77 * pi * t49 / 0.8D1 
     #+ t81 * pi * t68 / 0.16D2 + t85 * pi * t49 / 0.8D1 + t89 * pi * t6
     #8 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s4em3
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarhhard71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarhhardt7s4em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 
     #/ 0.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt7s4em4
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
      doubleprecision rrgg2qqbarhhard71J1
      doubleprecision rrgg2qqbarhhard71J2
      doubleprecision rrgg2qqbarhhard71J3
      doubleprecision rrgg2qqbarhhard71J4
      doubleprecision rrgg2qqbarhhard71J5
      doubleprecision rrgg2qqbarhhard71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt7s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarhhard71J1
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
      t5 = S24 ** 2
      t11 = S13 ** 2
      t17 = t5 * S24
      t20 = S14 ** 2
      t23 = S23 ** 2
      t26 = t11 * S13
      t40 = S12 ** 2
      rrgg2qqbarhhard71J1 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24
     #) * nf * S12 + 0.3D1 / 0.8D1 * (0.2D1 * t5 - 0.6D1 * S23 * S24 - 0
     #.6D1 * S13 * S14 + 0.2D1 * t11) * nf + 0.3D1 / 0.8D1 * (0.4D1 * S2
     #3 * t5 - t17 + 0.4D1 * t11 * S14 - 0.7D1 * S13 * t20 - 0.7D1 * t23
     # * S24 - t26) * nf / S12 + 0.3D1 / 0.8D1 * (0.4D1 * t23 * S23 * S2
     #4 + 0.4D1 * S23 * t17 + 0.4D1 * t20 * S14 * S13 + 0.4D1 * S14 * t2
     #6) * nf / t40) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard71J2
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
      t5 = S13 * S14
      t7 = S24 ** 2
      t13 = S13 ** 2
      t15 = S23 * S24
      t19 = t13 * S13
      t21 = S23 ** 2
      t24 = S14 ** 2
      t25 = S13 * t24
      t27 = t13 * S14
      t29 = t21 * S24
      t31 = t7 * S24
      t35 = S23 * t7
      t67 = 0.4D1 * t21 * S23 * S24 + 0.4D1 * S23 * t31 - 0.8D1 * t21 * 
     #t7 + 0.4D1 * t35 * S14 + 0.4D1 * t15 * t24 + 0.4D1 * t29 * S14 + 0
     #.4D1 * S14 * t19 + 0.4D1 * t5 * t21 + 0.4D1 * t25 * S23 + 0.4D1 * 
     #t24 * S14 * S13 + 0.4D1 * t27 * S23 - 0.8D1 * t24 * t13
      t69 = S12 ** 2
      rrgg2qqbarhhard71J2 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24
     #) * nf * S12 + 0.3D1 / 0.8D1 * (-0.12D2 * t5 + 0.4D1 * t7 - 0.2D1 
     #* S23 * S13 - 0.2D1 * S24 * S14 + 0.4D1 * t13 - 0.12D2 * t15) * nf
     # + 0.3D1 / 0.8D1 * (-0.2D1 * t19 - 0.2D1 * S13 * t21 - 0.14D2 * t2
     #5 + 0.8D1 * t27 - 0.14D2 * t29 - 0.2D1 * t31 - 0.2D1 * S24 * t24 +
     # 0.8D1 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi * 
     #wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard71J3
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
      t5 = S13 * S14
      t7 = S24 ** 2
      t13 = S13 ** 2
      t15 = S23 * S24
      t19 = t13 * S13
      t21 = S23 ** 2
      t24 = S14 ** 2
      t25 = S13 * t24
      t27 = t13 * S14
      t29 = t21 * S24
      t31 = t7 * S24
      t35 = S23 * t7
      t67 = 0.4D1 * t21 * S23 * S24 + 0.4D1 * S23 * t31 - 0.16D2 * t21 *
     # t7 + 0.8D1 * t35 * S14 + 0.8D1 * t15 * t24 + 0.8D1 * t29 * S14 + 
     #0.4D1 * S14 * t19 + 0.8D1 * t5 * t21 + 0.8D1 * t25 * S23 + 0.4D1 *
     # t24 * S14 * S13 + 0.8D1 * t27 * S23 - 0.16D2 * t24 * t13
      t69 = S12 ** 2
      rrgg2qqbarhhard71J3 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24
     #) * nf * S12 + 0.3D1 / 0.8D1 * (-0.18D2 * t5 + 0.6D1 * t7 - 0.4D1 
     #* S23 * S13 - 0.4D1 * S24 * S14 + 0.6D1 * t13 - 0.18D2 * t15) * nf
     # + 0.3D1 / 0.8D1 * (-0.3D1 * t19 - 0.4D1 * S13 * t21 - 0.21D2 * t2
     #5 + 0.12D2 * t27 - 0.21D2 * t29 - 0.3D1 * t31 - 0.4D1 * S24 * t24 
     #+ 0.12D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi 
     #* wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard71J4
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
      t5 = S13 * S14
      t7 = S24 ** 2
      t13 = S13 ** 2
      t15 = S23 * S24
      t19 = t13 * S13
      t21 = S23 ** 2
      t24 = S14 ** 2
      t25 = S13 * t24
      t27 = t13 * S14
      t29 = t21 * S24
      t31 = t7 * S24
      t35 = S23 * t7
      t67 = 0.4D1 * t21 * S23 * S24 + 0.4D1 * S23 * t31 - 0.24D2 * t21 *
     # t7 + 0.12D2 * t35 * S14 + 0.12D2 * t15 * t24 + 0.12D2 * t29 * S14
     # + 0.4D1 * S14 * t19 + 0.12D2 * t5 * t21 + 0.12D2 * t25 * S23 + 0.
     #4D1 * t24 * S14 * S13 + 0.12D2 * t27 * S23 - 0.24D2 * t24 * t13
      t69 = S12 ** 2
      rrgg2qqbarhhard71J4 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24
     #) * nf * S12 + 0.3D1 / 0.8D1 * (-0.24D2 * t5 + 0.8D1 * t7 - 0.6D1 
     #* S23 * S13 - 0.6D1 * S24 * S14 + 0.8D1 * t13 - 0.24D2 * t15) * nf
     # + 0.3D1 / 0.8D1 * (-0.4D1 * t19 - 0.6D1 * S13 * t21 - 0.28D2 * t2
     #5 + 0.16D2 * t27 - 0.28D2 * t29 - 0.4D1 * t31 - 0.6D1 * S24 * t24 
     #+ 0.16D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi 
     #* wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard71J5
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
      t5 = S13 * S14
      t7 = S24 ** 2
      t13 = S13 ** 2
      t15 = S23 * S24
      t19 = t13 * S13
      t21 = S23 ** 2
      t24 = S14 ** 2
      t25 = S13 * t24
      t27 = t13 * S14
      t29 = t21 * S24
      t31 = t7 * S24
      t35 = S23 * t7
      t67 = 0.4D1 * t21 * S23 * S24 + 0.4D1 * S23 * t31 - 0.32D2 * t21 *
     # t7 + 0.16D2 * t35 * S14 + 0.16D2 * t15 * t24 + 0.16D2 * t29 * S14
     # + 0.4D1 * S14 * t19 + 0.16D2 * t5 * t21 + 0.16D2 * t25 * S23 + 0.
     #4D1 * t24 * S14 * S13 + 0.16D2 * t27 * S23 - 0.32D2 * t24 * t13
      t69 = S12 ** 2
      rrgg2qqbarhhard71J5 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24
     #) * nf * S12 + 0.3D1 / 0.8D1 * (-0.30D2 * t5 + 0.10D2 * t7 - 0.8D1
     # * S23 * S13 - 0.8D1 * S24 * S14 + 0.10D2 * t13 - 0.30D2 * t15) * 
     #nf + 0.3D1 / 0.8D1 * (-0.5D1 * t19 - 0.8D1 * S13 * t21 - 0.35D2 * 
     #t25 + 0.20D2 * t27 - 0.35D2 * t29 - 0.5D1 * t31 - 0.8D1 * S24 * t2
     #4 + 0.20D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / p
     #i * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard71J6
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
      t10 = S23 ** 2
      t12 = S14 ** 2
      t25 = S13 ** 2
      t28 = S24 ** 2
      t55 = 0.4D1 * S23 * S24 * t12 + 0.4D1 * t10 * S24 * S14 - 0.8D1 * 
     #t12 * t25 + 0.4D1 * S23 * t28 * S14 - 0.8D1 * t10 * t28 - 0.4D1 * 
     #t12 * S14 * S13 - 0.4D1 * S14 * t25 * S13 + 0.4D1 * S14 * t25 * S2
     #3 + 0.4D1 * S14 * S13 * t10 + 0.4D1 * t12 * S13 * S23 - 0.4D1 * t1
     #0 * S23 * S24 - 0.4D1 * S23 * t28 * S24
      t57 = S12 ** 2
      rrgg2qqbarhhard71J6 = (0.15D2 / 0.8D1 * (0.2D1 * S24 + 0.2D1 * S13
     #) * nf * S12 + 0.15D2 / 0.8D1 * (-0.2D1 * S23 * S13 - 0.2D1 * S24 
     #* S14) * nf + 0.15D2 / 0.8D1 * (-0.2D1 * S13 * t10 - 0.2D1 * S24 *
     # t12) * nf / S12 + 0.15D2 / 0.8D1 * t55 * nf / t57) / pi * wd / z

      end function
  
   
      subroutine rrgg2qqbarhsoftt7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt7s1e1  
      doubleprecision rrgg2qqbarhsoftt7s1e0  
      doubleprecision rrgg2qqbarhsoftt7s1em1  
      doubleprecision rrgg2qqbarhsoftt7s1em2  
      doubleprecision rrgg2qqbarhsoftt7s1em3  
      doubleprecision rrgg2qqbarhsoftt7s1em4  
      doubleprecision rrgg2qqbarhsoftt7s2e1  
      doubleprecision rrgg2qqbarhsoftt7s2e0  
      doubleprecision rrgg2qqbarhsoftt7s2em1  
      doubleprecision rrgg2qqbarhsoftt7s2em2  
      doubleprecision rrgg2qqbarhsoftt7s2em3  
      doubleprecision rrgg2qqbarhsoftt7s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt7s1e1
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
      rrgg2qqbarhsoftt7s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s1e0
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
      rrgg2qqbarhsoftt7s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s1em1
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
      rrgg2qqbarhsoftt7s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s1em2
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
      rrgg2qqbarhsoftt7s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s1em3
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
      rrgg2qqbarhsoftt7s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s1em4
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
      rrgg2qqbarhsoftt7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhsoftt7s2e1
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
      rrgg2qqbarhsoftt7s2e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s2e0
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
      rrgg2qqbarhsoftt7s2e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s2em1
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
      rrgg2qqbarhsoftt7s2em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s2em2
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
      rrgg2qqbarhsoftt7s2em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s2em3
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
      rrgg2qqbarhsoftt7s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt7s2em4
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
      rrgg2qqbarhsoftt7s2em4 = 0.0D0

      end function
