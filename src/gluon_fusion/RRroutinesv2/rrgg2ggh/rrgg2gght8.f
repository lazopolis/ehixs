      subroutine rrgg2gght8
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt8
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt8
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhard81J1  
      doubleprecision rrgg2gghhard81J2  
      doubleprecision rrgg2gghhard81J3  
      doubleprecision rrgg2gghhard81J4  
      doubleprecision rrgg2gghhard81J5  
      doubleprecision rrgg2gghhard81J6  
      doubleprecision rrgg2gghhardt8s1e1  
      doubleprecision rrgg2gghhardt8s1e0  
      doubleprecision rrgg2gghhardt8s1em1  
      doubleprecision rrgg2gghhardt8s1em2  
      doubleprecision rrgg2gghhardt8s1em3  
      doubleprecision rrgg2gghhardt8s1em4  
      doubleprecision rrgg2gghhardt8s2e1  
      doubleprecision rrgg2gghhardt8s2e0  
      doubleprecision rrgg2gghhardt8s2em1  
      doubleprecision rrgg2gghhardt8s2em2  
      doubleprecision rrgg2gghhardt8s2em3  
      doubleprecision rrgg2gghhardt8s2em4  
      doubleprecision rrgg2gghhardt8s3e1  
      doubleprecision rrgg2gghhardt8s3e0  
      doubleprecision rrgg2gghhardt8s3em1  
      doubleprecision rrgg2gghhardt8s3em2  
      doubleprecision rrgg2gghhardt8s3em3  
      doubleprecision rrgg2gghhardt8s3em4  
      doubleprecision rrgg2gghhardt8s4e1  
      doubleprecision rrgg2gghhardt8s4e0  
      doubleprecision rrgg2gghhardt8s4em1  
      doubleprecision rrgg2gghhardt8s4em2  
      doubleprecision rrgg2gghhardt8s4em3  
      doubleprecision rrgg2gghhardt8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghhardt8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghhardt8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghhardt8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghhardt8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghhardt8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghhardt8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghhardt8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghhardt8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghhardt8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghhardt8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghhardt8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghhardt8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt8s1e1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t6 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t8 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t9 = pi ** 2
      t11 = lh ** 2
      t13 = -0.30D2 * t9 + 0.180D3 * t11
      t17 = s ** 2
      t18 = 0.1D1 / t17
      t19 = x4 * pi
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = x3 * t21
      t23 = z ** 2
      t24 = 0.1D1 / t23
      t27 = log(0.4D1 * t22 * t24)
      t28 = -0.1D1 + x3
      t29 = 0.1D1 / t28
      t33 = log(-0.4D1 * t22 * t24 * t29)
      t35 = cos(t19)
      t36 = z * t35
      t38 = Sqrt(-x3 * t28)
      t42 = 0.1D1 / (-z - x3 + 0.2D1 * t36 * t38)
      t48 = t33 ** 2
      t52 = t27 ** 2
      t66 = -0.240D3 * zeta3 - 0.120D3 * t11 * lh + 0.60D2 * lh * t9
      t68 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t89 = 0.1D1 / x3
      t92 = t24 * t21
      t94 = log(0.4D1 * t92)
      t95 = t94 ** 2
      t98 = t95 * t94
      t113 = t9 ** 2
      t114 = t11 ** 2
      t128 = rrgg2gghhard81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 
     #0.0D0, 0.0D0, 0.0D0)
      t130 = t95 ** 2
      t137 = pi * t18
      t138 = x1 ** 2
      t139 = x3 * t138
      t142 = log(0.4D1 * t139 * t92)
      t144 = t142 ** 2
      t147 = t92 * t29
      t150 = log(-0.4D1 * t139 * t147)
      t151 = t150 * z
      t153 = t150 ** 2
      t157 = z * t6
      t163 = pi * lh
      t165 = z * t3
      t173 = pi * t13
      t177 = t18 * (z * t8 * t42 + t8)
      t178 = t173 * t177
      t181 = 0.1D1 / x1
      t184 = t138 * t21
      t185 = t184 * t24
      t187 = log(0.4D1 * t185)
      t192 = t187 ** 2
      t202 = pi * t66
      t203 = t18 * t8
      t204 = t202 * t203
      t215 = x2 ** 2
      t216 = t215 * x3
      t217 = t216 * t185
      t219 = log(0.4D1 * t217)
      t221 = t216 * t138
      t224 = log(-0.4D1 * t221 * t147)
      t236 = 0.1D1 / x2
      t237 = t181 * t236
      t240 = t215 * t138
      t243 = log(0.4D1 * t240 * t92)
      t245 = t243 ** 2
      t263 = log(-0.4D1 * t216 * t147)
      t264 = t263 * z
      t266 = t263 ** 2
      t274 = log(0.4D1 * t216 * t92)
      t276 = t274 ** 2
      t294 = t215 * t21
      t297 = log(0.4D1 * t294 * t24)
      t302 = t297 ** 2
      t322 = ((-0.180D3 * t3 * lh + 0.90D2 * t6 + t8 * t13) * pi * t18 *
     # (t27 + t33 * z * t42) + 0.90D2 * t8 * pi * t18 * (t48 * t33 * z *
     # t42 / 0.6D1 + t52 * t27 / 0.6D1) + (-0.180D3 * t6 * lh + t8 * t66
     # + 0.90D2 * t68 + t3 * t13) * pi * t18 * (-0.1D1 - z * t42) + (-0.
     #180D3 * t8 * lh + 0.90D2 * t3) * pi * t18 * (-t52 / 0.2D1 - t48 * 
     #z * t42 / 0.2D1)) * t89 / 0.2880D4 - (-0.180D3 * (t95 * t3 / 0.2D1
     # + t68 - t98 * t8 / 0.6D1 - t94 * t6) * lh + (-t94 * t3 + t95 * t8
     # / 0.2D1 + t6) * t13 + (t3 - t94 * t8) * t66 + t8 * (t113 + 0.60D2
     # * t114 + 0.480D3 * lh * zeta3 - 0.60D2 * t11 * t9) - 0.15D2 * t98
     # * t3 + 0.45D2 * t95 * t6 - 0.90D2 * t94 * t68 + 0.90D2 * t128 + 0
     #.15D2 / 0.4D1 * t130 * t8) * pi * t18 / 0.2880D4 - (0.90D2 * t137 
     #* (-t142 * t3 + t144 * t8 / 0.2D1 + t6 + (-t151 * t3 + t153 * z * 
     #t8 / 0.2D1 + t157) * t42) - 0.180D3 * t163 * t18 * (t3 - t142 * t8
     # + (t165 - t151 * t8) * t42) + t178) * t89 * t181 / 0.1440D4 + (t1
     #73 * t18 * (-t3 + t187 * t8) + 0.90D2 * t137 * (-t192 * t3 / 0.2D1
     # - t68 + t192 * t187 * t8 / 0.6D1 + t187 * t6) - t204 - 0.180D3 * 
     #t163 * t18 * (t187 * t3 - t192 * t8 / 0.2D1 - t6)) * t181 / 0.1440
     #D4 - (0.90D2 * t137 * (t3 - t219 * t8 + (t165 - t224 * z * t8) * t
     #42) - 0.180D3 * t163 * t177) * t89 * t237 / 0.720D3 + (0.90D2 * t1
     #37 * (t243 * t3 - t245 * t8 / 0.2D1 - t6) - 0.180D3 * t163 * t18 *
     # (-t3 + t243 * t8) - t173 * t203) * t181 * t236 / 0.720D3 - (0.90D
     #2 * t137 * ((-t264 * t3 + t266 * z * t8 / 0.2D1 + t157) * t42 - t2
     #74 * t3 + t276 * t8 / 0.2D1 + t6) - 0.180D3 * t163 * t18 * ((t165 
     #- t264 * t8) * t42 + t3 - t274 * t8) + t178) * t89 * t236 / 0.1440
     #D4 + (t173 * t18 * (-t3 + t297 * t8) + 0.90D2 * t137 * (-t302 * t3
     # / 0.2D1 - t68 + t302 * t297 * t8 / 0.6D1 + t297 * t6) - t204 - 0.
     #180D3 * t163 * t18 * (t297 * t3 - t302 * t8 / 0.2D1 - t6)) * t236 
     #/ 0.1440D4
      t323 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t322)
      t325 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t322)
      t327 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t322)
      t329 = t2 * x1
      t330 = -0.1D1 + x1
      t331 = x1 * z
      t332 = 0.1D1 - x1 + t331
      t333 = 0.1D1 / t332
      t335 = t2 * t330 * t333
      t336 = t1 ** 2
      t337 = s * t336
      t339 = x1 * t330 * t333
      t340 = t337 * t339
      t341 = t139 * t21
      t342 = t24 * t333
      t343 = t330 ** 2
      t344 = t342 * t343
      t347 = log(0.4D1 * t341 * t344)
      t348 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, t329, 0.0D0, -t340)
      t350 = t347 ** 2
      t351 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, t329, 0.0D0, -t340)
      t354 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, t329, 0.0D0, -t340)
      t356 = t342 * t343 * t29
      t359 = log(-0.4D1 * t341 * t356)
      t360 = t359 * z
      t362 = t359 ** 2
      t369 = x3 * x1
      t370 = t369 * z
      t371 = 0.3D1 * t370
      t372 = x3 * t332
      t374 = Sqrt(-t372 * t28)
      t377 = x1 * t23
      t378 = t369 * t23
      t380 = 0.2D1 * t139 * z
      t381 = t138 * t23
      t382 = t381 * x3
      t383 = 0.2D1 * t369
      t384 = -z - x3 + t331 - t371 + 0.2D1 * t36 * t374 - t377 + t378 + 
     #t380 - t382 + t383 - t139
      t385 = 0.1D1 / t384
      t391 = z * t348
      t404 = t18 * (-t351 - z * t351 * t332 * t385)
      t412 = log(0.4D1 * t184 * t344)
      t417 = t412 ** 2
      t420 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, t329, 0.0D0, -t340)
      t428 = t18 * t351
      t440 = t216 * t184
      t443 = log(-0.4D1 * t440 * t356)
      t449 = t333 * t343
      t453 = log(0.4D1 * t221 * t92 * t449)
      t464 = t240 * t21
      t467 = log(0.4D1 * t464 * t344)
      t469 = t467 ** 2
      t485 = -(0.90D2 * t137 * (t347 * t348 - t350 * t351 / 0.2D1 - t354
     # - (-t360 * t348 + t362 * z * t351 / 0.2D1 + z * t354) * t332 * t3
     #85) - 0.180D3 * t163 * t18 * (-t348 + t347 * t351 - (t391 - t360 *
     # t351) * t332 * t385) + t173 * t404) * t89 * t181 / 0.1440D4 + (t1
     #73 * t18 * (t348 - t412 * t351) + 0.90D2 * t137 * (t417 * t348 / 0
     #.2D1 + t420 - t417 * t412 * t351 / 0.6D1 - t412 * t354) + t202 * t
     #428 - 0.180D3 * t163 * t18 * (-t412 * t348 + t417 * t351 / 0.2D1 +
     # t354)) * t181 / 0.1440D4 - (0.90D2 * t137 * (-(t391 - t443 * z * 
     #t351) * t332 * t385 - t348 + t453 * t351) - 0.180D3 * t163 * t404)
     # * t89 * t237 / 0.720D3 + (0.90D2 * t137 * (-t467 * t348 + t469 * 
     #t351 / 0.2D1 + t354) - 0.180D3 * t163 * t18 * (t348 - t467 * t351)
     # + t173 * t428) * t181 * t236 / 0.720D3
      t486 = FJET(XB1, XB2, s, 0.0D0, t329, -t335, 0.0D0, -t340, t485)
      t488 = x2 * s
      t489 = t488 * t1
      t490 = -0.1D1 + x2
      t491 = t490 * s
      t492 = t491 * t1
      t493 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t489, -t492
     #, 0.0D0, 0.0D0, 0.0D0)
      t494 = t92 * t490
      t497 = log(-0.4D1 * t221 * t494)
      t498 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t489, -t492
     #, 0.0D0, 0.0D0, 0.0D0)
      t503 = t18 * t498
      t512 = log(-0.4D1 * t240 * t494)
      t514 = t512 ** 2
      t517 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t489, -t492
     #, 0.0D0, 0.0D0, 0.0D0)
      t526 = t173 * t503
      t533 = log(-0.4D1 * t216 * t494)
      t535 = t533 ** 2
      t550 = t24 * t490
      t553 = log(-0.4D1 * t294 * t550)
      t558 = t553 ** 2
      t561 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, t489, -t492
     #, 0.0D0, 0.0D0, 0.0D0)
      t580 = -(0.90D2 * t137 * (-t493 + t497 * t498) + 0.180D3 * t163 * 
     #t503) * t89 * t237 / 0.720D3 + (0.90D2 * t137 * (-t512 * t493 + t5
     #14 * t498 / 0.2D1 + t517) - 0.180D3 * t163 * t18 * (t493 - t512 * 
     #t498) + t526) * t181 * t236 / 0.720D3 - (0.90D2 * t137 * (t533 * t
     #493 - t535 * t498 / 0.2D1 - t517) - 0.180D3 * t163 * t18 * (-t493 
     #+ t533 * t498) - t526) * t89 * t236 / 0.1440D4 + (t173 * t18 * (t4
     #93 - t553 * t498) + 0.90D2 * t137 * (t558 * t493 / 0.2D1 + t561 - 
     #t558 * t553 * t498 / 0.6D1 - t553 * t517) + t202 * t503 - 0.180D3 
     #* t163 * t18 * (-t553 * t493 + t558 * t498 / 0.2D1 + t517)) * t236
     # / 0.1440D4
      t581 = FJET(XB1, XB2, s, 0.0D0, t489, 0.0D0, -t492, 0.0D0, t580)
      t583 = x2 * x3
      t586 = Sqrt(x3 * t490 * t28)
      t587 = t35 * t586
      t589 = 0.2D1 * t587 * x2
      t591 = 0.1D1 - x3 + t583
      t592 = 0.1D1 / t591
      t594 = t2 * (0.1D1 - x3 - x2 + t583 + t216 + t589) * t592
      t599 = t2 * x2 * (-0.1D1 + t583 + 0.2D1 * t587) * t592
      t600 = x2 * z
      t601 = t600 - z - x2
      t602 = t583 * z
      t604 = z * t215 * x3
      t611 = 0.1D1 / (-t600 - t602 + t604 + z + x2 + x3 - t216 - t589 - 
     #0.2D1 * t36 * t586 + 0.2D1 * t36 * t586 * x2)
      t612 = t601 * t611
      t613 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t599, t594
     #, 0.0D0, 0.0D0, 0.0D0)
      t614 = t612 * t613
      t615 = t591 ** 2
      t616 = 0.1D1 / t615
      t618 = t550 * t28 * t616
      t621 = log(0.4D1 * t440 * t618)
      t623 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t599, t594
     #, 0.0D0, 0.0D0, 0.0D0)
      t624 = t611 * t623
      t629 = t163 * t18
      t630 = t612 * t623
      t636 = (0.90D2 * t137 * (-t614 + t621 * t601 * t624) + 0.180D3 * t
     #629 * t630) * t89 * t237 / 0.720D3
      t640 = log(0.4D1 * t216 * t21 * t618)
      t641 = t640 * t601
      t644 = t640 ** 2
      t648 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t599, t594
     #, 0.0D0, 0.0D0, 0.0D0)
      t650 = t641 * t611 * t613 - t644 * t601 * t624 / 0.2D1 - t612 * t6
     #48
      t654 = -t614 + t641 * t624
      t659 = t173 * t18 * t630
      t664 = -t636 - (0.90D2 * t137 * t650 - 0.180D3 * t163 * t18 * t654
     # - t659) * t89 * t236 / 0.1440D4
      t665 = FJET(XB1, XB2, s, 0.0D0, t594, 0.0D0, -t599, 0.0D0, t664)
      t667 = t1 * t330
      t669 = t491 * t667 * t333
      t670 = t488 * t667
      t672 = t337 * t490 * t339
      t673 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t670, t669
     #, t329, 0.0D0, t672)
      t675 = t342 * t343 * t490
      t678 = log(-0.4D1 * t440 * t675)
      t679 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t670, t669
     #, t329, 0.0D0, t672)
      t684 = t18 * t679
      t689 = (0.90D2 * t137 * (t673 - t678 * t679) - 0.180D3 * t163 * t6
     #84) * t89 * t237
      t692 = log(-0.4D1 * t464 * t675)
      t694 = t692 ** 2
      t697 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t670, t669
     #, t329, 0.0D0, t672)
      t698 = t692 * t673 - t694 * t679 / 0.2D1 - t697
      t702 = -t673 + t692 * t679
      t706 = t173 * t684
      t711 = -t689 / 0.720D3 + (0.90D2 * t137 * t698 - 0.180D3 * t163 * 
     #t18 * t702 - t706) * t181 * t236 / 0.720D3
      t712 = FJET(XB1, XB2, s, 0.0D0, t669, t329, -t670, t672, t711)
      t714 = FJET(XB1, XB2, s, 0.0D0, -t492, 0.0D0, t489, 0.0D0, t580)
      t716 = FJET(XB1, XB2, s, 0.0D0, -t335, t329, 0.0D0, -t340, t485)
      t718 = FJET(XB1, XB2, s, 0.0D0, -t599, 0.0D0, t594, 0.0D0, t664)
      t720 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t322)
      t722 = FJET(XB1, XB2, s, t329, 0.0D0, 0.0D0, -t335, -t340, t485)
      t724 = t323 * t322 + t325 * t322 + t327 * t322 + t486 * t485 + t58
     #1 * t580 + t665 * t664 + t712 * t711 + t714 * t580 + t716 * t485 +
     # t718 * t664 + t720 * t322 + t722 * t485
      t725 = FJET(XB1, XB2, s, t329, -t670, 0.0D0, t669, t672, t711)
      t727 = FJET(XB1, XB2, s, t489, 0.0D0, -t492, 0.0D0, 0.0D0, t580)
      t729 = FJET(XB1, XB2, s, t594, 0.0D0, -t599, 0.0D0, 0.0D0, t664)
      t731 = FJET(XB1, XB2, s, t669, 0.0D0, -t670, t329, t672, t711)
      t734 = t329 * t583 * t592
      t735 = t2 * t330
      t736 = t490 * t28
      t738 = Sqrt(t372 * t736)
      t739 = t35 * t738
      t741 = 0.2D1 * t739 * x2
      t742 = t331 * t216
      t744 = x1 * t215 * x3
      t748 = t735 * (0.1D1 - x3 - x2 + t583 + t741 + t742 - t744 + t216)
     # * t333 * t592
      t752 = t28 * s * t1 * x1 * t592
      t758 = t735 * x2 * (-0.1D1 + t583 + x1 - t369 - t331 + t370 + 0.2D
     #1 * t739) * t333 * t592
      t759 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t758, -t748
     #, -t752, t734, t672)
      t765 = log(0.4D1 * t217 * t449 * t736 * t616)
      t767 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t758, -t748
     #, -t752, t734, t672)
      t770 = x1 * x2
      t771 = t770 * z
      t772 = x2 - t770 + z - t600 + t771
      t786 = t331 - t377 + t383 - t139 + t742 - 0.2D1 * t739 * t770 - 0.
     #2D1 * t36 * t738 * x2 + 0.2D1 * t369 * t600 - t369 * x2 * t23 - 0.
     #2D1 * t139 * t600 + t381 * t583 - t371 + t378 + t380 - t382 + t741
     # - t744
      t788 = t138 * x2
      t802 = -0.3D1 * t771 + 0.2D1 * t788 * z + t770 * t23 - t381 * x2 -
     # t369 * x2 + 0.2D1 * t36 * t738 + t139 * x2 + t216 + t600 + t602 -
     # t604 - z - x3 + 0.2D1 * t770 - t788 + 0.2D1 * t36 * t738 * x1 * x
     #2 - x2
      t804 = 0.1D1 / (t786 + t802)
      t813 = 0.90D2 * t137 * (t332 * t759 - t765 * t332 * t767) * t772 *
     # t804 - 0.180D3 * t629 * t332 * t767 * t772 * t804
      t816 = t813 * t89 * t237 / 0.720D3
      t817 = FJET(XB1, XB2, s, t734, -t748, -t752, t758, t672, -t816)
      t820 = t89 * t181 * t236
      t823 = FJET(XB1, XB2, s, t758, -t752, -t748, t734, t672, -t816)
      t827 = FJET(XB1, XB2, s, -t492, 0.0D0, t489, 0.0D0, 0.0D0, t580)
      t829 = FJET(XB1, XB2, s, -t335, 0.0D0, 0.0D0, t329, -t340, t485)
      t842 = -t689 / 0.720D3 + (0.90D2 * t137 * t698 - 0.180D3 * t163 * 
     #t18 * t702 - t706) * t181 * t236 / 0.720D3
      t843 = FJET(XB1, XB2, s, -t670, t329, t669, 0.0D0, t672, t842)
      t856 = -t636 - (0.90D2 * t137 * t650 - 0.180D3 * t163 * t18 * t654
     # - t659) * t89 * t236 / 0.1440D4
      t857 = FJET(XB1, XB2, s, -t599, 0.0D0, t594, 0.0D0, 0.0D0, t856)
      t859 = FJET(XB1, XB2, s, -t752, t758, t734, -t748, t672, -t816)
      t863 = FJET(XB1, XB2, s, -t748, t734, t758, -t752, t672, -t816)
      t867 = t725 * t711 + t727 * t580 + t729 * t664 + t731 * t711 - t81
     #7 * t813 * t820 / 0.720D3 - t823 * t813 * t820 / 0.720D3 + t827 * 
     #t580 + t829 * t485 + t843 * t842 + t857 * t856 - t859 * t813 * t82
     #0 / 0.720D3 - t863 * t813 * t820 / 0.720D3
      rrgg2gghhardt8s1e1 = t724 + t867

      end function



      doubleprecision function rrgg2gghhardt8s1e0
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
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
      t26 = z * t25
      t28 = Sqrt(-x3 * t17)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t28)
      t41 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t52 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t54 = pi ** 2
      t56 = lh ** 2
      t58 = -0.30D2 * t54 + 0.180D3 * t56
      t67 = 0.1D1 / x3
      t70 = t12 * t9
      t72 = log(0.4D1 * t70)
      t74 = t72 ** 2
      t89 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t103 = pi * t6
      t104 = z * t41
      t105 = x2 ** 2
      t106 = t105 * x3
      t107 = t70 * t18
      t110 = log(-0.4D1 * t106 * t107)
      t117 = log(0.4D1 * t106 * t70)
      t122 = pi * lh
      t125 = z * t3 * t32 + t3
      t128 = 0.180D3 * t122 * t6 * t125
      t131 = 0.1D1 / x2
      t134 = t105 * t9
      t137 = log(0.4D1 * t134 * t12)
      t139 = t137 ** 2
      t150 = pi * t58
      t151 = t6 * t3
      t152 = t150 * t151
      t156 = x1 ** 2
      t157 = x3 * t156
      t160 = log(0.4D1 * t157 * t70)
      t164 = log(-0.4D1 * t157 * t107)
      t174 = 0.1D1 / x1
      t179 = t67 * t174 * t131
      t182 = t105 * t156
      t185 = log(0.4D1 * t182 * t70)
      t196 = t156 * t9
      t199 = log(0.4D1 * t196 * t12)
      t201 = t199 ** 2
      t215 = (0.90D2 * t3 * pi * t6 * (-t16 / 0.2D1 - t23 * z * t32 / 0.
     #2D1) + (-0.180D3 * t3 * lh + 0.90D2 * t41) * pi * t6 * (t15 + t22 
     #* z * t32) + (-0.180D3 * t41 * lh + 0.90D2 * t52 + t3 * t58) * pi 
     #* t6 * (-0.1D1 - z * t32)) * t67 / 0.2880D4 - (-0.180D3 * (-t72 * 
     #t41 + t74 * t3 / 0.2D1 + t52) * lh + t3 * (-0.240D3 * zeta3 - 0.12
     #0D3 * t56 * lh + 0.60D2 * lh * t54) + 0.45D2 * t74 * t41 + 0.90D2 
     #* t89 - 0.15D2 * t74 * t72 * t3 - 0.90D2 * t72 * t52 + (t41 - t72 
     #* t3) * t58) * pi * t6 / 0.2880D4 - (0.90D2 * t103 * ((t104 - t110
     # * z * t3) * t32 + t41 - t117 * t3) - t128) * t67 * t131 / 0.1440D
     #4 + (0.90D2 * t103 * (t137 * t41 - t139 * t3 / 0.2D1 - t52) - 0.18
     #0D3 * t122 * t6 * (-t41 + t137 * t3) - t152) * t131 / 0.1440D4 - (
     #0.90D2 * t103 * (t41 - t160 * t3 + (t104 - t164 * z * t3) * t32) -
     # t128) * t67 * t174 / 0.1440D4 - t103 * t125 * t179 / 0.8D1 + (0.9
     #0D2 * t103 * (-t41 + t185 * t3) + 0.180D3 * t122 * t151) * t174 * 
     #t131 / 0.720D3 + (0.90D2 * t103 * (t199 * t41 - t201 * t3 / 0.2D1 
     #- t52) - 0.180D3 * t122 * t6 * (-t41 + t199 * t3) - t152) * t174 /
     # 0.1440D4
      t216 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t215)
      t218 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t215)
      t220 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t215)
      t222 = t2 * x1
      t223 = -0.1D1 + x1
      t224 = x1 * z
      t225 = 0.1D1 - x1 + t224
      t226 = 0.1D1 / t225
      t228 = t2 * t223 * t226
      t229 = t1 ** 2
      t230 = s * t229
      t232 = x1 * t223 * t226
      t233 = t230 * t232
      t234 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t22
     #8, t222, 0.0D0, -t233)
      t235 = t157 * t9
      t236 = t12 * t226
      t237 = t223 ** 2
      t238 = t236 * t237
      t241 = log(0.4D1 * t235 * t238)
      t242 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t22
     #8, t222, 0.0D0, -t233)
      t249 = log(-0.4D1 * t235 * t236 * t237 * t18)
      t254 = x3 * x1
      t255 = t254 * z
      t256 = 0.3D1 * t255
      t257 = x3 * t225
      t259 = Sqrt(-t257 * t17)
      t262 = x1 * t11
      t263 = t254 * t11
      t265 = 0.2D1 * t157 * z
      t266 = t156 * t11
      t267 = t266 * x3
      t268 = 0.2D1 * t254
      t269 = -z - x3 + t224 - t256 + 0.2D1 * t26 * t259 - t262 + t263 + 
     #t265 - t267 + t268 - t157
      t270 = 0.1D1 / t269
      t278 = -t242 - z * t242 * t225 * t270
      t289 = t182 * t9
      t292 = log(0.4D1 * t289 * t238)
      t297 = t6 * t242
      t306 = log(0.4D1 * t196 * t238)
      t308 = t306 ** 2
      t311 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t22
     #8, t222, 0.0D0, -t233)
      t324 = -(0.90D2 * t103 * (-t234 + t241 * t242 - (z * t234 - t249 *
     # z * t242) * t225 * t270) - 0.180D3 * t122 * t6 * t278) * t67 * t1
     #74 / 0.1440D4 - t103 * t278 * t179 / 0.8D1 + (0.90D2 * t103 * (t23
     #4 - t292 * t242) - 0.180D3 * t122 * t297) * t174 * t131 / 0.720D3 
     #+ (0.90D2 * t103 * (-t306 * t234 + t308 * t242 / 0.2D1 + t311) - 0
     #.180D3 * t122 * t6 * (t234 - t306 * t242) + t150 * t297) * t174 / 
     #0.1440D4
      t325 = FJET(XB1, XB2, s, 0.0D0, t222, -t228, 0.0D0, -t233, t324)
      t327 = x2 * s
      t328 = t327 * t1
      t329 = -0.1D1 + x2
      t330 = t329 * s
      t331 = t1 * t330
      t332 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331
     #, 0.0D0, 0.0D0, 0.0D0)
      t333 = t70 * t329
      t336 = log(-0.4D1 * t106 * t333)
      t337 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331
     #, 0.0D0, 0.0D0, 0.0D0)
      t342 = t6 * t337
      t344 = 0.180D3 * t122 * t342
      t349 = t12 * t329
      t352 = log(-0.4D1 * t134 * t349)
      t354 = t352 ** 2
      t357 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331
     #, 0.0D0, 0.0D0, 0.0D0)
      t375 = log(-0.4D1 * t182 * t333)
      t384 = -(0.90D2 * t103 * (-t332 + t336 * t337) + t344) * t67 * t13
     #1 / 0.1440D4 + (0.90D2 * t103 * (-t352 * t332 + t354 * t337 / 0.2D
     #1 + t357) - 0.180D3 * t122 * t6 * (t332 - t352 * t337) + t150 * t3
     #42) * t131 / 0.1440D4 + t103 * t337 * t179 / 0.8D1 + (0.90D2 * t10
     #3 * (t332 - t375 * t337) - t344) * t174 * t131 / 0.720D3
      t385 = FJET(XB1, XB2, s, 0.0D0, t328, 0.0D0, -t331, 0.0D0, t384)
      t387 = x2 * x3
      t390 = Sqrt(x3 * t329 * t17)
      t391 = t25 * t390
      t393 = 0.2D1 * t391 * x2
      t395 = 0.1D1 - x3 + t387
      t396 = 0.1D1 / t395
      t398 = t2 * (0.1D1 - x3 - x2 + t387 + t106 + t393) * t396
      t403 = t2 * x2 * (-0.1D1 + t387 + 0.2D1 * t391) * t396
      t404 = x2 * z
      t405 = t404 - z - x2
      t406 = t387 * z
      t408 = z * t105 * x3
      t415 = 0.1D1 / (-t404 - t406 + t408 + z + x2 + x3 - t106 - t393 - 
     #0.2D1 * t26 * t390 + 0.2D1 * t26 * t390 * x2)
      t416 = t405 * t415
      t417 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t403, t398
     #, 0.0D0, 0.0D0, 0.0D0)
      t420 = t395 ** 2
      t426 = log(0.4D1 * t106 * t9 * t349 * t17 / t420)
      t428 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t403, t398
     #, 0.0D0, 0.0D0, 0.0D0)
      t431 = -t416 * t417 + t426 * t405 * t415 * t428
      t437 = 0.180D3 * t122 * t6 * t416 * t428
      t447 = t103 * t416 * t428 * t67 * t174 * t131 / 0.8D1
      t448 = -(0.90D2 * t103 * t431 + t437) * t67 * t131 / 0.1440D4 + t4
     #47
      t449 = FJET(XB1, XB2, s, 0.0D0, t398, 0.0D0, -t403, 0.0D0, t448)
      t451 = t1 * t223
      t453 = t330 * t451 * t226
      t454 = t327 * t451
      t456 = t230 * t329 * t232
      t457 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t454, t453
     #, t222, 0.0D0, t456)
      t460 = t103 * t457 * t179 / 0.8D1
      t461 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t454, t453
     #, t222, 0.0D0, t456)
      t466 = log(-0.4D1 * t289 * t236 * t237 * t329)
      t468 = -t461 + t466 * t457
      t473 = 0.180D3 * t122 * t6 * t457
      t478 = -t460 + (0.90D2 * t103 * t468 + t473) * t174 * t131 / 0.720
     #D3
      t479 = FJET(XB1, XB2, s, 0.0D0, t453, t222, -t454, t456, t478)
      t481 = FJET(XB1, XB2, s, 0.0D0, -t331, 0.0D0, t328, 0.0D0, t384)
      t483 = FJET(XB1, XB2, s, 0.0D0, -t228, t222, 0.0D0, -t233, t324)
      t485 = FJET(XB1, XB2, s, 0.0D0, -t403, 0.0D0, t398, 0.0D0, t448)
      t487 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t215)
      t489 = FJET(XB1, XB2, s, t222, 0.0D0, 0.0D0, -t228, -t233, t324)
      t491 = t216 * t215 + t218 * t215 + t220 * t215 + t325 * t324 + t38
     #5 * t384 + t449 * t448 + t479 * t478 + t481 * t384 + t483 * t324 +
     # t485 * t448 + t487 * t215 + t489 * t324
      t492 = FJET(XB1, XB2, s, t222, -t454, 0.0D0, t453, t456, t478)
      t494 = FJET(XB1, XB2, s, t328, 0.0D0, -t331, 0.0D0, 0.0D0, t384)
      t496 = FJET(XB1, XB2, s, t398, 0.0D0, -t403, 0.0D0, 0.0D0, t448)
      t498 = FJET(XB1, XB2, s, t453, 0.0D0, -t454, t222, t456, t478)
      t501 = t222 * t387 * t396
      t502 = t2 * t223
      t505 = Sqrt(t257 * t329 * t17)
      t506 = t25 * t505
      t508 = 0.2D1 * t506 * x2
      t509 = t224 * t106
      t511 = x1 * t105 * x3
      t515 = t502 * (0.1D1 - x3 - x2 + t387 + t508 + t509 - t511 + t106)
     # * t226 * t396
      t519 = t17 * s * t1 * x1 * t396
      t525 = t502 * x2 * (-0.1D1 + t387 + x1 - t254 - t224 + t255 + 0.2D
     #1 * t506) * t226 * t396
      t526 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t525, -t515
     #, -t519, t501, t456)
      t529 = x1 * x2
      t530 = t529 * z
      t533 = t156 * x2
      t554 = t508 - t511 - 0.3D1 * t530 + 0.2D1 * t533 * z + t529 * t11 
     #- t266 * x2 - t254 * x2 + 0.2D1 * t26 * t505 + t157 * x2 - x3 + t5
     #09 - 0.2D1 * t506 * t529 - 0.2D1 * t26 * t505 * x2 + 0.2D1 * t254 
     #* t404 - t254 * x2 * t11 - 0.2D1 * t157 * t404 + t266 * t387
      t560 = -x2 - z + 0.2D1 * t26 * t505 * x1 * x2 + 0.2D1 * t529 - t53
     #3 + t224 - t262 + t268 - t157 + t106 + t404 - t256 + t263 + t265 -
     # t267 + t406 - t408
      t564 = (x2 - t529 + z - t404 + t530) / (t554 + t560) * t179
      t566 = t103 * t225 * t526 * t564 / 0.8D1
      t567 = FJET(XB1, XB2, s, t501, -t515, -t519, t525, t456, -t566)
      t570 = t6 * t225 * t526
      t574 = FJET(XB1, XB2, s, t525, -t519, -t515, t501, t456, -t566)
      t579 = FJET(XB1, XB2, s, -t331, 0.0D0, t328, 0.0D0, 0.0D0, t384)
      t581 = FJET(XB1, XB2, s, -t228, 0.0D0, 0.0D0, t222, -t233, t324)
      t590 = -t460 + (0.90D2 * t103 * t468 + t473) * t174 * t131 / 0.720
     #D3
      t591 = FJET(XB1, XB2, s, -t454, t222, t453, 0.0D0, t456, t590)
      t600 = -(0.90D2 * t103 * t431 + t437) * t67 * t131 / 0.1440D4 + t4
     #47
      t601 = FJET(XB1, XB2, s, -t403, 0.0D0, t398, 0.0D0, 0.0D0, t600)
      t603 = FJET(XB1, XB2, s, -t519, t525, t501, -t515, t456, -t566)
      t608 = FJET(XB1, XB2, s, -t515, t501, t525, -t519, t456, -t566)
      t613 = t492 * t478 + t494 * t384 + t496 * t448 + t498 * t478 - t56
     #7 * pi * t570 * t564 / 0.8D1 - t574 * pi * t570 * t564 / 0.8D1 + t
     #579 * t384 + t581 * t324 + t591 * t590 + t601 * t600 - t603 * pi *
     # t570 * t564 / 0.8D1 - t608 * pi * t570 * t564 / 0.8D1
      rrgg2gghhardt8s1e0 = t491 + t613

      end function



      doubleprecision function rrgg2gghhardt8s1em1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = -0.1D1 + x3
      t21 = log(-0.4D1 * t10 * t12 / t16)
      t23 = cos(t7)
      t24 = z * t23
      t26 = Sqrt(-x3 * t16)
      t30 = 0.1D1 / (-z - x3 + 0.2D1 * t24 * t26)
      t38 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t47 = 0.1D1 / x3
      t52 = log(0.4D1 * t12 * t9)
      t59 = t52 ** 2
      t62 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t64 = pi ** 2
      t66 = lh ** 2
      t74 = pi * t6
      t78 = (z * t3 * t30 + t3) * t47
      t79 = 0.1D1 / x2
      t83 = x2 ** 2
      t84 = t83 * t9
      t87 = log(0.4D1 * t84 * t12)
      t92 = pi * lh
      t95 = 0.180D3 * t92 * t6 * t3
      t99 = 0.1D1 / x1
      t104 = x1 ** 2
      t105 = t104 * t9
      t108 = log(0.4D1 * t105 * t12)
      t119 = (0.90D2 * t4 * t6 * (t15 + t21 * z * t30) + (-0.180D3 * t3 
     #* lh + 0.90D2 * t38) * pi * t6 * (-0.1D1 - z * t30)) * t47 / 0.288
     #0D4 - (-0.180D3 * (t38 - t52 * t3) * lh - 0.90D2 * t52 * t38 + 0.4
     #5D2 * t59 * t3 + 0.90D2 * t62 + t3 * (-0.30D2 * t64 + 0.180D3 * t6
     #6)) * pi * t6 / 0.2880D4 - t74 * t78 * t79 / 0.16D2 + (0.90D2 * t7
     #4 * (-t38 + t87 * t3) + t95) * t79 / 0.1440D4 - t4 * t6 * t99 * t7
     #9 / 0.8D1 + (0.90D2 * t74 * (-t38 + t108 * t3) + t95) * t99 / 0.14
     #40D4 - t74 * t78 * t99 / 0.16D2
      t120 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t119)
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t119)
      t124 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t119)
      t126 = t2 * x1
      t127 = -0.1D1 + x1
      t128 = x1 * z
      t129 = 0.1D1 - x1 + t128
      t130 = 0.1D1 / t129
      t132 = t2 * t127 * t130
      t133 = t1 ** 2
      t134 = s * t133
      t136 = x1 * t127 * t130
      t137 = t134 * t136
      t138 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t13
     #2, t126, 0.0D0, -t137)
      t143 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t13
     #2, t126, 0.0D0, -t137)
      t145 = t127 ** 2
      t149 = log(0.4D1 * t105 * t12 * t130 * t145)
      t161 = x3 * x1
      t166 = Sqrt(-x3 * t129 * t16)
      t171 = x3 * t104
      t177 = -z - x3 + t128 - 0.3D1 * t161 * z + 0.2D1 * t24 * t166 - x1
     # * t11 + t161 * t11 + 0.2D1 * t171 * z - t104 * t11 * x3 + 0.2D1 *
     # t161 - t171
      t186 = t74 * t138 * t99 * t79 / 0.8D1 + (0.90D2 * t74 * (t143 - t1
     #49 * t138) - 0.180D3 * t92 * t6 * t138) * t99 / 0.1440D4 - t74 * (
     #-t138 - z * t138 * t129 / t177) * t47 * t99 / 0.16D2
      t187 = FJET(XB1, XB2, s, 0.0D0, t126, -t132, 0.0D0, -t137, t186)
      t189 = x2 * s
      t190 = t189 * t1
      t191 = -0.1D1 + x2
      t192 = t191 * s
      t193 = t192 * t1
      t194 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t190, -t193
     #, 0.0D0, 0.0D0, 0.0D0)
      t199 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t190, -t193
     #, 0.0D0, 0.0D0, 0.0D0)
      t203 = log(-0.4D1 * t84 * t12 * t191)
      t218 = t74 * t194 * t47 * t79 / 0.16D2 + (0.90D2 * t74 * (t199 - t
     #203 * t194) - 0.180D3 * t92 * t6 * t194) * t79 / 0.1440D4 + t74 * 
     #t194 * t99 * t79 / 0.8D1
      t219 = FJET(XB1, XB2, s, 0.0D0, t190, 0.0D0, -t193, 0.0D0, t218)
      t221 = x2 * x3
      t222 = t83 * x3
      t225 = Sqrt(x3 * t191 * t16)
      t226 = t23 * t225
      t228 = 0.2D1 * t226 * x2
      t231 = 0.1D1 / (0.1D1 - x3 + t221)
      t233 = t2 * (0.1D1 - x3 - x2 + t221 + t222 + t228) * t231
      t238 = t2 * x2 * (-0.1D1 + t221 + 0.2D1 * t226) * t231
      t239 = x2 * z
      t240 = t239 - z - x2
      t252 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t238, t233
     #, 0.0D0, 0.0D0, 0.0D0)
      t255 = 0.1D1 / (-t239 - t221 * z + z * t83 * x3 + z + x2 + x3 - t2
     #22 - t228 - 0.2D1 * t24 * t225 + 0.2D1 * t24 * t225 * x2) * t252 *
     # t47 * t79
      t257 = t74 * t240 * t255 / 0.16D2
      t258 = FJET(XB1, XB2, s, 0.0D0, t233, 0.0D0, -t238, 0.0D0, t257)
      t260 = t6 * t240
      t264 = t1 * t127
      t266 = t192 * t264 * t130
      t267 = t189 * t264
      t269 = t134 * t191 * t136
      t270 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t267, t266
     #, t126, 0.0D0, t269)
      t272 = t270 * t99 * t79
      t274 = t74 * t272 / 0.8D1
      t275 = FJET(XB1, XB2, s, 0.0D0, t266, t126, -t267, t269, -t274)
      t280 = FJET(XB1, XB2, s, 0.0D0, -t193, 0.0D0, t190, 0.0D0, t218)
      t282 = FJET(XB1, XB2, s, 0.0D0, -t132, t126, 0.0D0, -t137, t186)
      t284 = FJET(XB1, XB2, s, 0.0D0, -t238, 0.0D0, t233, 0.0D0, t257)
      t289 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t119)
      t291 = FJET(XB1, XB2, s, t126, 0.0D0, 0.0D0, -t132, -t137, t186)
      t293 = FJET(XB1, XB2, s, t126, -t267, 0.0D0, t266, t269, -t274)
      t298 = FJET(XB1, XB2, s, t190, 0.0D0, -t193, 0.0D0, 0.0D0, t218)
      t300 = FJET(XB1, XB2, s, t233, 0.0D0, -t238, 0.0D0, 0.0D0, t257)
      t305 = FJET(XB1, XB2, s, t266, 0.0D0, -t267, t126, t269, -t274)
      t310 = FJET(XB1, XB2, s, -t193, 0.0D0, t190, 0.0D0, 0.0D0, t218)
      t312 = FJET(XB1, XB2, s, -t132, 0.0D0, 0.0D0, t126, -t137, t186)
      t314 = FJET(XB1, XB2, s, -t267, t126, t266, 0.0D0, t269, -t274)
      t319 = FJET(XB1, XB2, s, -t238, 0.0D0, t233, 0.0D0, 0.0D0, t257)
      rrgg2gghhardt8s1em1 = t120 * t119 + t122 * t119 + t124 * t119 + t1
     #87 * t186 + t219 * t218 + t258 * pi * t260 * t255 / 0.16D2 - t275 
     #* pi * t6 * t272 / 0.8D1 + t280 * t218 + t282 * t186 + t284 * pi *
     # t260 * t255 / 0.16D2 + t289 * t119 + t291 * t186 - t293 * pi * t6
     # * t272 / 0.8D1 + t298 * t218 + t300 * pi * t260 * t255 / 0.16D2 -
     # t305 * pi * t6 * t272 / 0.8D1 + t310 * t218 + t312 * t186 - t314 
     #* pi * t6 * t272 / 0.8D1 + t319 * pi * t260 * t255 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt8s1em2
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = cos(t7)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t34 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t36 = z ** 2
      t38 = Sin(t7)
      t39 = t38 ** 2
      t42 = log(0.4D1 / t36 * t39)
      t49 = t4 * t6 * (-0.1D1 - z / (-z - x3 + 0.2D1 * z * t8 * t12)) / 
     #x3 / 0.32D2 - t4 * t6 * t24 / 0.16D2 - t4 * t6 * t28 / 0.16D2 - (-
     #0.180D3 * t3 * lh + 0.90D2 * t34 - 0.90D2 * t42 * t3) * pi * t6 / 
     #0.2880D4
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
      t69 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t62,
     # t56, 0.0D0, -t67)
      t72 = t68 * t69 * t28 / 0.16D2
      t73 = FJET(XB1, XB2, s, 0.0D0, t56, -t62, 0.0D0, -t67, t72)
      t76 = t6 * t69 * t28
      t80 = x2 * s * t1
      t83 = (-0.1D1 + x2) * s * t1
      t84 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t83, 0
     #.0D0, 0.0D0, 0.0D0)
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
      rrgg2gghhardt8s1em2 = t50 * t49 + t52 * t49 + t54 * t49 + t73 * pi
     # * t76 / 0.16D2 + t88 * pi * t91 / 0.16D2 + t94 * pi * t91 / 0.16D
     #2 + t98 * pi * t76 / 0.16D2 + t102 * t49 + t104 * pi * t76 / 0.16D
     #2 + t108 * pi * t91 / 0.16D2 + t112 * pi * t91 / 0.16D2 + t116 * p
     #i * t76 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt8s1em3
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * pi * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = pi * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gghhardt8s1em3 = -t9 * t3 * t11 / 0.32D2 - t13 * t3 * t11 / 0
     #.32D2 - t16 * t3 * t11 / 0.32D2 - t19 * t3 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2gghhardt8s1em4
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt8s2e1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t16 * t4
      t20 = log(-0.4D1 * t10 * t17)
      t21 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0
     #D0, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0
     #D0, 0.0D0, 0.0D0)
      t27 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0
     #D0, 0.0D0, 0.0D0)
      t31 = pi * lh
      t37 = pi ** 2
      t39 = lh ** 2
      t41 = -0.30D2 * t37 + 0.180D3 * t39
      t42 = pi * t41
      t43 = t7 * t24
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t48 = 0.1D1 / x1
      t50 = (0.90D2 * t8 * (-t20 * t21 + t23 * t24 / 0.2D1 + t27) - 0.18
     #0D3 * t31 * t7 * (t21 - t20 * t24) + t44) * t46 * t48 / 0.720D3
      t51 = t10 * x2
      t54 = log(-0.4D1 * t51 * t17)
      t63 = 0.1D1 / x2
      t64 = t48 * t63
      t66 = (0.90D2 * t8 * (-t21 + t54 * t24) + 0.180D3 * t31 * t43) * t
     #46 * t64 / 0.720D3
      t67 = x3 * t13
      t71 = log(-0.4D1 * t67 * t15 * t4)
      t73 = -t21 + t71 * t24
      t76 = t71 ** 2
      t79 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0
     #D0, 0.0D0, 0.0D0)
      t84 = -t76 * t21 / 0.2D1 - t79 + t76 * t71 * t24 / 0.6D1 + t71 * t
     #27
      t92 = -0.240D3 * zeta3 - 0.120D3 * t39 * lh + 0.60D2 * lh * t37
      t93 = pi * t92
      t94 = t93 * t43
      t98 = t71 * t21 - t76 * t24 / 0.2D1 - t27
      t105 = x2 * t13
      t110 = log(-0.4D1 * t105 * t15 * x3 * t4)
      t112 = t110 ** 2
      t126 = (0.90D2 * t8 * (t110 * t21 - t112 * t24 / 0.2D1 - t27) - 0.
     #180D3 * t31 * t7 * (-t21 + t110 * t24) - t44) * t46 * t63 / 0.1440
     #D4
      t127 = t50 - t66 - (t42 * t7 * t73 + 0.90D2 * t8 * t84 - t94 - 0.1
     #80D3 * t31 * t7 * t98) * t46 / 0.1440D4 - t126
      t128 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t127)
      t130 = -0.1D1 + x2
      t131 = x2 * x3
      t132 = cos(t11)
      t134 = Sqrt(-t131 * t4)
      t135 = t132 * t134
      t136 = 0.2D1 * t135
      t139 = -0.1D1 + t131
      t140 = 0.1D1 / t139
      t142 = t2 * t130 * (-t131 - 0.1D1 + x3 + t136) * t140
      t143 = 0.3D1 * t131
      t144 = x2 ** 2
      t145 = t144 * x3
      t147 = 0.2D1 * t135 * x2
      t150 = t2 * (-x2 - x3 + t143 - t145 - t136 + t147) * t140
      t151 = x2 * z
      t152 = 0.1D1 - x2 + t151
      t154 = z * t144 * x3
      t155 = z * t132
      t159 = t131 * z
      t160 = 0.2D1 * t131
      t162 = 0.1D1 / (-t154 + t145 + x2 - t147 + 0.2D1 * t155 * t134 * x
     #2 - t151 + t159 - t160 - 0.1D1 + t136)
      t163 = t152 * t162
      t164 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t142, t150
     #, 0.0D0, 0.0D0, 0.0D0)
      t165 = t163 * t164
      t166 = t9 * t13
      t167 = t131 * t166
      t168 = t130 ** 2
      t169 = t15 * t168
      t170 = t139 ** 2
      t171 = 0.1D1 / t170
      t173 = t169 * t4 * t171
      t176 = log(-0.4D1 * t167 * t173)
      t178 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t142, t150
     #, 0.0D0, 0.0D0, 0.0D0)
      t179 = t162 * t178
      t184 = t31 * t7
      t185 = t163 * t178
      t191 = (0.90D2 * t8 * (-t165 + t176 * t152 * t179) + 0.180D3 * t18
     #4 * t185) * t46 * t64 / 0.720D3
      t195 = log(-0.4D1 * t131 * t13 * t173)
      t196 = t195 * t152
      t199 = t195 ** 2
      t203 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t142, t150
     #, 0.0D0, 0.0D0, 0.0D0)
      t205 = -t196 * t162 * t164 + t199 * t152 * t179 / 0.2D1 + t163 * t
     #203
      t209 = t165 - t196 * t179
      t214 = t42 * t7 * t185
      t219 = -t191 - (-0.90D2 * t8 * t205 + 0.180D3 * t31 * t7 * t209 - 
     #t214) * t46 * t63 / 0.1440D4
      t220 = FJET(XB1, XB2, s, -t142, 0.0D0, t150, 0.0D0, 0.0D0, t219)
      t222 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t127)
      t224 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 
     #0.0D0, 0.0D0, 0.0D0)
      t227 = log(0.4D1 * t67 * t15)
      t228 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 
     #0.0D0, 0.0D0, 0.0D0)
      t233 = t227 ** 2
      t236 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 
     #0.0D0, 0.0D0, 0.0D0)
      t240 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 
     #0.0D0, 0.0D0, 0.0D0)
      t245 = t7 * t228
      t246 = t93 * t245
      t258 = log(0.4D1 * t16)
      t259 = t258 ** 2
      t260 = t259 * pi
      t264 = t259 * t258 * pi
      t266 = t258 * pi
      t285 = rrgg2gghhard81J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 
     #0.0D0, 0.0D0, 0.0D0)
      t293 = t37 ** 2
      t294 = t39 ** 2
      t302 = t259 ** 2
      t311 = log(0.4D1 * t10 * t16)
      t313 = t311 ** 2
      t324 = t42 * t245
      t329 = t166 * t15
      t331 = log(0.4D1 * t329)
      t336 = t331 ** 2
      t358 = log(0.4D1 * t131 * t329)
      t369 = t9 * x2
      t372 = log(0.4D1 * t369 * t16)
      t374 = t372 ** 2
      t391 = log(0.4D1 * t131 * t16)
      t393 = t391 ** 2
      t410 = log(0.4D1 * t105 * t15)
      t415 = t410 ** 2
      t435 = -(t42 * t7 * (t224 - t227 * t228) + 0.90D2 * t8 * (t233 * t
     #224 / 0.2D1 + t236 - t233 * t227 * t228 / 0.6D1 - t227 * t240) + t
     #246 - 0.180D3 * t31 * t7 * (-t227 * t224 + t233 * t228 / 0.2D1 + t
     #240)) * t46 / 0.1440D4 - (-0.90D2 * t260 * lh + t93 - 0.15D2 * t26
     #4 - t266 * t41) * t7 * t224 / 0.1440D4 - (0.180D3 * t266 * lh + 0.
     #45D2 * t260 + t42) * t7 * t240 / 0.1440D4 - (-0.180D3 * t31 - 0.90
     #D2 * t266) * t7 * t236 / 0.1440D4 - t8 * t285 / 0.16D2 - (0.30D2 *
     # t264 * lh + t260 * t41 / 0.2D1 - t266 * t92 + pi * (t293 + 0.60D2
     # * t294 + 0.480D3 * lh * zeta3 - 0.60D2 * t39 * t37) + 0.15D2 / 0.
     #4D1 * t302 * pi) * t7 * t228 / 0.1440D4 + (0.90D2 * t8 * (t311 * t
     #224 - t313 * t228 / 0.2D1 - t240) - 0.180D3 * t31 * t7 * (-t224 + 
     #t311 * t228) - t324) * t46 * t48 / 0.720D3 - (t42 * t7 * (t224 - t
     #331 * t228) + 0.90D2 * t8 * (t336 * t224 / 0.2D1 + t236 - t336 * t
     #331 * t228 / 0.6D1 - t331 * t240) + t246 - 0.180D3 * t31 * t7 * (-
     #t331 * t224 + t336 * t228 / 0.2D1 + t240)) * t48 / 0.720D3 - (0.90
     #D2 * t8 * (t224 - t358 * t228) - 0.180D3 * t31 * t245) * t46 * t64
     # / 0.720D3 - (0.90D2 * t8 * (-t372 * t224 + t374 * t228 / 0.2D1 + 
     #t240) - 0.180D3 * t31 * t7 * (t224 - t372 * t228) + t324) * t48 * 
     #t63 / 0.720D3 - (0.90D2 * t8 * (-t391 * t224 + t393 * t228 / 0.2D1
     # + t240) - 0.180D3 * t31 * t7 * (t224 - t391 * t228) + t324) * t46
     # * t63 / 0.1440D4 + (t42 * t7 * (-t224 + t410 * t228) + 0.90D2 * t
     #8 * (-t415 * t224 / 0.2D1 - t236 + t415 * t410 * t228 / 0.6D1 + t4
     #10 * t240) - t246 - 0.180D3 * t31 * t7 * (t410 * t224 - t415 * t22
     #8 / 0.2D1 - t240)) * t63 / 0.1440D4
      t436 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t435)
      t449 = -t191 - (-0.90D2 * t8 * t205 + 0.180D3 * t31 * t7 * t209 - 
     #t214) * t46 * t63 / 0.1440D4
      t450 = FJET(XB1, XB2, s, 0.0D0, -t142, 0.0D0, t150, 0.0D0, t449)
      t452 = -0.1D1 + x1
      t453 = t2 * t452
      t454 = t2 * x1
      t456 = x1 * z
      t457 = 0.1D1 - x1 + t456
      t458 = 0.1D1 / t457
      t460 = t452 ** 2
      t461 = t15 * t458 * t460
      t464 = log(0.4D1 * t10 * t13 * t461)
      t465 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t453, 0.0D
     #0, t454, 0.0D0, 0.0D0)
      t467 = t464 ** 2
      t468 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t453, 0.0D
     #0, t454, 0.0D0, 0.0D0)
      t471 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t453, 0.0D
     #0, t454, 0.0D0, 0.0D0)
      t480 = t7 * t468
      t481 = t42 * t480
      t484 = (0.90D2 * t8 * (-t464 * t465 + t467 * t468 / 0.2D1 + t471) 
     #- 0.180D3 * t31 * t7 * (t465 - t464 * t468) + t481) * t46 * t48
      t487 = log(0.4D1 * t166 * t461)
      t489 = -t465 + t487 * t468
      t492 = t487 ** 2
      t495 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, -t453, 0.0D
     #0, t454, 0.0D0, 0.0D0)
      t500 = -t492 * t465 / 0.2D1 - t495 + t492 * t487 * t468 / 0.6D1 + 
     #t487 * t471
      t503 = t93 * t480
      t507 = t487 * t465 - t492 * t468 / 0.2D1 - t471
      t517 = log(0.4D1 * t51 * t16 * t460 * t458)
      t526 = (0.90D2 * t8 * (-t465 + t517 * t468) + 0.180D3 * t31 * t480
     #) * t46 * t64
      t527 = t369 * t13
      t530 = log(0.4D1 * t527 * t461)
      t532 = t530 ** 2
      t545 = (0.90D2 * t8 * (t530 * t465 - t532 * t468 / 0.2D1 - t471) -
     # 0.180D3 * t31 * t7 * (-t465 + t530 * t468) - t481) * t48 * t63
      t547 = t484 / 0.720D3 - (t42 * t7 * t489 + 0.90D2 * t8 * t500 - t5
     #03 - 0.180D3 * t31 * t7 * t507) * t48 / 0.720D3 - t526 / 0.720D3 -
     # t545 / 0.720D3
      t548 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t453, t454, 0.0D0, t547)
      t550 = t130 * s
      t551 = t1 * t452
      t552 = t550 * t551
      t555 = t2 * t452 * x2 * t458
      t556 = t1 ** 2
      t561 = s * t556 * x2 * x1 * t452 * t458
      t562 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t552, -t555
     #, t454, 0.0D0, -t561)
      t565 = t15 * t460 * t458 * t168
      t568 = log(0.4D1 * t167 * t565)
      t569 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t552, -t555
     #, t454, 0.0D0, -t561)
      t574 = t7 * t569
      t582 = log(0.4D1 * t527 * t565)
      t584 = t582 ** 2
      t587 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t552, -t555
     #, t454, 0.0D0, -t561)
      t601 = -(0.90D2 * t8 * (t562 - t568 * t569) - 0.180D3 * t31 * t574
     #) * t46 * t64 / 0.720D3 - (0.90D2 * t8 * (-t582 * t562 + t584 * t5
     #69 / 0.2D1 + t587) - 0.180D3 * t31 * t7 * (t562 - t582 * t569) + t
     #42 * t574) * t48 * t63 / 0.720D3
      t602 = FJET(XB1, XB2, s, t552, t454, -t555, 0.0D0, -t561, t601)
      t604 = t4 * s
      t605 = t1 * x1
      t606 = t604 * t605
      t607 = t604 * t551
      t608 = x3 * x1
      t609 = t2 * t608
      t611 = x3 * s * t551
      t612 = t9 * t458
      t614 = x3 * t460
      t618 = log(-0.4D1 * t612 * t4 * t16 * t614)
      t619 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t607, -t611
     #, -t606, t609, 0.0D0)
      t621 = t618 ** 2
      t622 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t607, -t611
     #, -t606, t609, 0.0D0)
      t625 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t607, -t611
     #, -t606, t609, 0.0D0)
      t634 = t7 * t622
      t645 = log(-0.4D1 * t612 * t4 * t13 * t15 * x2 * t614)
      t656 = (0.90D2 * t8 * (t618 * t619 - t621 * t622 / 0.2D1 - t625) -
     # 0.180D3 * t31 * t7 * (-t619 + t618 * t622) - t42 * t634) * t46 * 
     #t48 / 0.720D3 - (0.90D2 * t8 * (t619 - t645 * t622) - 0.180D3 * t3
     #1 * t634) * t46 * t64 / 0.720D3
      t657 = FJET(XB1, XB2, s, -t606, t607, t609, -t611, 0.0D0, t656)
      t660 = x2 * s * t1
      t661 = t550 * t1
      t662 = t16 * t168
      t665 = log(0.4D1 * t131 * t662)
      t666 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t661, t660
     #, 0.0D0, 0.0D0, 0.0D0)
      t668 = t665 ** 2
      t669 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t661, t660
     #, 0.0D0, 0.0D0, 0.0D0)
      t672 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t661, t660
     #, 0.0D0, 0.0D0, 0.0D0)
      t681 = t7 * t669
      t682 = t42 * t681
      t689 = log(0.4D1 * t105 * t169)
      t694 = t689 ** 2
      t697 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, -t661, t660
     #, 0.0D0, 0.0D0, 0.0D0)
      t718 = log(0.4D1 * t51 * t662)
      t731 = log(0.4D1 * t369 * t662)
      t733 = t731 ** 2
      t748 = -(0.90D2 * t8 * (t665 * t666 - t668 * t669 / 0.2D1 - t672) 
     #- 0.180D3 * t31 * t7 * (-t666 + t665 * t669) - t682) * t46 * t63 /
     # 0.1440D4 + (t42 * t7 * (t666 - t689 * t669) + 0.90D2 * t8 * (t694
     # * t666 / 0.2D1 + t697 - t694 * t689 * t669 / 0.6D1 - t689 * t672)
     # + t93 * t681 - 0.180D3 * t31 * t7 * (-t689 * t666 + t694 * t669 /
     # 0.2D1 + t672)) * t63 / 0.1440D4 - (0.90D2 * t8 * (-t666 + t718 * 
     #t669) + 0.180D3 * t31 * t681) * t46 * t64 / 0.720D3 - (0.90D2 * t8
     # * (t731 * t666 - t733 * t669 / 0.2D1 - t672) - 0.180D3 * t31 * t7
     # * (-t666 + t731 * t669) - t682) * t48 * t63 / 0.720D3
      t749 = FJET(XB1, XB2, s, 0.0D0, t660, 0.0D0, -t661, 0.0D0, t748)
      t751 = FJET(XB1, XB2, s, t607, -t606, -t611, t609, 0.0D0, t656)
      t753 = FJET(XB1, XB2, s, t660, 0.0D0, -t661, 0.0D0, 0.0D0, t748)
      t768 = t50 - t66 - (t42 * t7 * t73 + 0.90D2 * t8 * t84 - t94 - 0.1
     #80D3 * t31 * t7 * t98) * t46 / 0.1440D4 - t126
      t769 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t768)
      t771 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t768)
      t773 = FJET(XB1, XB2, s, 0.0D0, t150, 0.0D0, -t142, 0.0D0, t449)
      t775 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t435)
      t790 = t484 / 0.720D3 - (t42 * t7 * t489 + 0.90D2 * t8 * t500 - t5
     #03 - 0.180D3 * t31 * t7 * t507) * t48 / 0.720D3 - t526 / 0.720D3 -
     # t545 / 0.720D3
      t791 = FJET(XB1, XB2, s, -t453, t454, 0.0D0, 0.0D0, 0.0D0, t790)
      t793 = t128 * t127 + t220 * t219 + t222 * t127 + t436 * t435 + t45
     #0 * t449 + t548 * t547 + t602 * t601 + t657 * t656 + t749 * t748 +
     # t751 * t656 + t753 * t748 + t769 * t768 + t771 * t768 + t773 * t4
     #49 + t775 * t435 + t791 * t790
      t794 = FJET(XB1, XB2, s, 0.0D0, -t555, t454, t552, -t561, t601)
      t796 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t435)
      t798 = FJET(XB1, XB2, s, -t611, t609, t607, -t606, 0.0D0, t656)
      t800 = FJET(XB1, XB2, s, t454, t552, 0.0D0, -t555, -t561, t601)
      t802 = FJET(XB1, XB2, s, t454, -t453, 0.0D0, 0.0D0, 0.0D0, t790)
      t804 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t454, -t453, 0.0D0, t547)
      t806 = FJET(XB1, XB2, s, t150, 0.0D0, -t142, 0.0D0, 0.0D0, t449)
      t808 = FJET(XB1, XB2, s, 0.0D0, -t661, 0.0D0, t660, 0.0D0, t748)
      t811 = t604 * t605 * t140
      t812 = t608 * z
      t816 = Sqrt(-x3 * t457 * x2 * t4)
      t817 = t132 * t816
      t818 = 0.2D1 * t817
      t823 = t453 * t130 * (-t131 - 0.1D1 + x3 + x1 - t608 - t456 + t812
     # + t818) * t458 * t140
      t826 = t454 * x3 * t130 * t140
      t827 = t608 * x2
      t829 = t608 * t151
      t831 = t456 * t145
      t833 = 0.2D1 * t817 * x2
      t835 = x1 * t144 * x3
      t836 = -x2 - x3 - t812 - 0.2D1 * t827 + t143 + 0.2D1 * t829 - t831
     # - t818 + t833 + t835 + t608 - t145
      t839 = t453 * t836 * t458 * t140
      t840 = x1 * x2
      t841 = t840 * z
      t842 = -0.1D1 + t841 - t151 + x1 + x2 - t456 - t840
      t843 = t457 * t842
      t847 = t9 * t14
      t858 = t816 * x2
      t864 = 0.1D1 - t145 + 0.2D1 * t456 - t818 + t160 - x2 - 0.2D1 * x1
     # - t831 + 0.4D1 * t829 + t847 * t131 - 0.2D1 * t10 * t151 - t608 *
     # x2 * t14 - 0.2D1 * t155 * t816 * x1 - 0.2D1 * t817 * t840 - 0.2D1
     # * t155 * t858 + 0.2D1 * t155 * t858 * x1
      t876 = t9 + t51 + t154 + 0.2D1 * t817 * x1 - t159 - t847 * x2 + t8
     #40 * t14 + 0.2D1 * t369 * z - 0.3D1 * t841 + t835 + t833 - 0.3D1 *
     # t827 - t369 - 0.2D1 * t9 * z + t847 + t151 + 0.2D1 * t840
      t878 = 0.1D1 / (t864 + t876)
      t879 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t823, -t839
     #, t811, t826, -t561)
      t891 = log(-0.4D1 * t9 * t4 * t458 * t13 * t15 * t131 * t460 * t17
     #1 * t168)
      t894 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t823, -t839
     #, t811, t826, -t561)
      t904 = 0.90D2 * t8 * (t843 * t878 * t879 - t891 * t457 * t842 * t8
     #78 * t894) - 0.180D3 * t184 * t843 * t878 * t894
      t907 = t904 * t46 * t64 / 0.720D3
      t908 = FJET(XB1, XB2, s, t811, t823, t826, -t839, -t561, -t907)
      t911 = t46 * t48 * t63
      t914 = FJET(XB1, XB2, s, t826, -t839, t811, t823, -t561, -t907)
      t918 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t435)
      t920 = FJET(XB1, XB2, s, t823, t811, -t839, t826, -t561, -t907)
      t924 = FJET(XB1, XB2, s, t609, -t611, -t606, t607, 0.0D0, t656)
      t926 = FJET(XB1, XB2, s, -t661, 0.0D0, t660, 0.0D0, 0.0D0, t748)
      t928 = FJET(XB1, XB2, s, -t839, t826, t823, t811, -t561, -t907)
      t932 = FJET(XB1, XB2, s, -t555, 0.0D0, t552, t454, -t561, t601)
      t934 = t794 * t601 + t796 * t435 + t798 * t656 + t800 * t601 + t80
     #2 * t790 + t804 * t547 + t806 * t449 + t808 * t748 - t908 * t904 *
     # t911 / 0.720D3 - t914 * t904 * t911 / 0.720D3 + t918 * t435 - t92
     #0 * t904 * t911 / 0.720D3 + t924 * t656 + t926 * t748 - t928 * t90
     #4 * t911 / 0.720D3 + t932 * t601
      rrgg2gghhardt8s2e1 = t793 + t934

      end function



      doubleprecision function rrgg2gghhardt8s2e0
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x3
      t2 = s * t1
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
      t36 = t14 * x2
      t38 = 0.3D1 * t5
      t39 = x2 * z
      t40 = t14 * t39
      t42 = x2 ** 2
      t43 = t42 * x3
      t44 = t15 * t43
      t46 = 0.2D1 * t24 * x2
      t48 = x1 * t42 * x3
      t49 = -x2 - x3 - t16 - 0.2D1 * t36 + t38 + 0.2D1 * t40 - t44 - t25
     # + t46 + t48 + t14 - t43
      t52 = t12 * t49 * t28 * t7
      t53 = t3 ** 2
      t58 = s * t53 * x2 * x1 * t11 * t28
      t59 = s ** 2
      t60 = 0.1D1 / t59
      t61 = pi * t60
      t62 = x1 * x2
      t63 = t62 * z
      t64 = -0.1D1 + t63 - t39 + x1 + x2 - t15 - t62
      t68 = x1 ** 2
      t69 = t68 * x2
      t72 = z ** 2
      t73 = t68 * t72
      t75 = 0.2D1 * t5
      t79 = 0.1D1 - x2 - t25 + 0.2D1 * t62 - t69 - 0.2D1 * t68 * z + t73
     # + 0.2D1 * t15 + t75 - t43 + t39 - 0.2D1 * x1 - 0.3D1 * t36 + t46 
     #+ t48 - 0.3D1 * t63
      t86 = x3 * t68
      t89 = z * t18
      t90 = t23 * x2
      t104 = z * t42 * x3
      t105 = t5 * z
      t109 = 0.2D1 * t69 * z + t62 * t72 - t73 * x2 + 0.2D1 * t24 * x1 +
     # t86 * x2 + 0.4D1 * t40 - t44 - 0.2D1 * t89 * t90 - 0.2D1 * t24 * 
     #t62 - 0.2D1 * t89 * t23 * x1 - t14 * x2 * t72 - 0.2D1 * t86 * t39 
     #+ t73 * t5 + t68 + t104 - t105 + 0.2D1 * t89 * t90 * x1
      t112 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t31, -t52, 
     #t9, t35, -t58)
      t114 = 0.1D1 / x3
      t115 = 0.1D1 / x1
      t117 = 0.1D1 / x2
      t118 = t114 * t115 * t117
      t119 = 0.1D1 / (t79 + t109) * t112 * t118
      t121 = t61 * t19 * t64 * t119 / 0.8D1
      t122 = FJET(XB1, XB2, s, t9, t31, t35, -t52, -t58, -t121)
      t125 = t60 * t19 * t64
      t129 = Sin(t17)
      t130 = t129 ** 2
      t131 = x3 * t130
      t132 = 0.1D1 / t72
      t135 = log(0.4D1 * t131 * t132)
      t136 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t138 = t135 ** 2
      t139 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t142 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t146 = pi * lh
      t152 = pi ** 2
      t154 = lh ** 2
      t156 = -0.30D2 * t152 + 0.180D3 * t154
      t157 = pi * t156
      t158 = t60 * t139
      t159 = t157 * t158
      t163 = t132 * t130
      t165 = log(0.4D1 * t163)
      t166 = t165 * pi
      t169 = t165 ** 2
      t170 = t169 * pi
      t176 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, t10, 0.0D0,
     # 0.0D0, 0.0D0, 0.0D0)
      t204 = log(0.4D1 * t5 * t163)
      t210 = 0.180D3 * t146 * t158
      t215 = x2 * t130
      t218 = log(0.4D1 * t215 * t132)
      t220 = t218 ** 2
      t236 = log(0.4D1 * t86 * t163)
      t250 = log(0.4D1 * t69 * t163)
      t259 = t68 * t130
      t262 = log(0.4D1 * t259 * t132)
      t264 = t262 ** 2
      t278 = -(0.90D2 * t61 * (-t135 * t136 + t138 * t139 / 0.2D1 + t142
     #) - 0.180D3 * t146 * t60 * (t136 - t135 * t139) + t159) * t114 / 0
     #.1440D4 - (0.180D3 * t166 * lh + 0.45D2 * t170 + t157) * t60 * t13
     #6 / 0.1440D4 - t61 * t176 / 0.16D2 - (-0.90D2 * t170 * lh + pi * (
     #-0.240D3 * zeta3 - 0.120D3 * t154 * lh + 0.60D2 * lh * t152) - 0.1
     #5D2 * t169 * t165 * pi - t166 * t156) * t60 * t139 / 0.1440D4 - (-
     #0.180D3 * t146 - 0.90D2 * t166) * t60 * t142 / 0.1440D4 - (0.90D2 
     #* t61 * (t136 - t204 * t139) - t210) * t114 * t117 / 0.1440D4 + (0
     #.90D2 * t61 * (t218 * t136 - t220 * t139 / 0.2D1 - t142) - 0.180D3
     # * t146 * t60 * (-t136 + t218 * t139) - t159) * t117 / 0.1440D4 + 
     #(0.90D2 * t61 * (-t136 + t236 * t139) + t210) * t114 * t115 / 0.72
     #0D3 - t61 * t139 * t118 / 0.8D1 - (0.90D2 * t61 * (t136 - t250 * t
     #139) - t210) * t115 * t117 / 0.720D3 - (0.90D2 * t61 * (-t262 * t1
     #36 + t264 * t139 / 0.2D1 + t142) - 0.180D3 * t146 * t60 * (t136 - 
     #t262 * t139) + t159) * t115 / 0.720D3
      t279 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t10, 0.0D0, 0.0D0, t278)
      t281 = t10 * t1
      t282 = t10 * x3
      t286 = log(-0.4D1 * t131 * t132 * t1)
      t287 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t281, t282
     #, 0.0D0, 0.0D0, 0.0D0)
      t289 = t286 ** 2
      t290 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t281, t282
     #, 0.0D0, 0.0D0, 0.0D0)
      t293 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t281, t282
     #, 0.0D0, 0.0D0, 0.0D0)
      t294 = -t286 * t287 + t289 * t290 / 0.2D1 + t293
      t298 = t287 - t286 * t290
      t302 = t60 * t290
      t303 = t157 * t302
      t311 = log(-0.4D1 * t215 * t132 * x3 * t1)
      t317 = 0.180D3 * t146 * t302
      t321 = (0.90D2 * t61 * (-t287 + t311 * t290) + t317) * t114 * t117
     # / 0.1440D4
      t325 = log(-0.4D1 * t86 * t163 * t1)
      t333 = (0.90D2 * t61 * (t287 - t325 * t290) - t317) * t114 * t115 
     #/ 0.720D3
      t336 = t61 * t290 * t118 / 0.8D1
      t337 = -(-0.90D2 * t61 * t294 + 0.180D3 * t146 * t60 * t298 - t303
     #) * t114 / 0.1440D4 - t321 + t333 + t336
      t338 = FJET(XB1, XB2, s, -t281, 0.0D0, t282, 0.0D0, 0.0D0, t337)
      t340 = t13 * s
      t341 = t3 * t11
      t342 = t340 * t341
      t345 = t10 * t11 * x2 * t28
      t346 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t342, -t345
     #, t32, 0.0D0, -t58)
      t350 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t342, -t345
     #, t32, 0.0D0, -t58)
      t351 = t69 * t130
      t352 = t132 * t28
      t353 = t11 ** 2
      t354 = t13 ** 2
      t359 = log(0.4D1 * t351 * t352 * t353 * t354)
      t371 = -t61 * t346 * t118 / 0.8D1 - (0.90D2 * t61 * (t350 - t359 *
     # t346) - 0.180D3 * t146 * t60 * t346) * t115 * t117 / 0.720D3
      t372 = FJET(XB1, XB2, s, t32, t342, 0.0D0, -t345, -t58, t371)
      t374 = t2 * t4
      t375 = t2 * t341
      t376 = t10 * t14
      t378 = x3 * s * t341
      t379 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t375, -t378
     #, -t374, t376, 0.0D0)
      t386 = log(-0.4D1 * t68 * t28 * t1 * t163 * x3 * t353)
      t387 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t375, -t378
     #, -t374, t376, 0.0D0)
      t402 = (0.90D2 * t61 * (-t379 + t386 * t387) + 0.180D3 * t146 * t6
     #0 * t387) * t114 * t115 / 0.720D3 - t61 * t387 * t118 / 0.8D1
      t403 = FJET(XB1, XB2, s, -t374, t375, t376, -t378, 0.0D0, t402)
      t405 = FJET(XB1, XB2, s, t376, -t378, -t374, t375, 0.0D0, t402)
      t408 = Sqrt(-t5 * t1)
      t409 = t18 * t408
      t410 = 0.2D1 * t409
      t414 = t10 * t13 * (-t5 - 0.1D1 + x3 + t410) * t7
      t416 = 0.2D1 * t409 * x2
      t419 = t10 * (-x2 - x3 + t38 - t43 - t410 + t416) * t7
      t420 = 0.1D1 - x2 + t39
      t425 = 0.1D1 / (-t104 + t43 + x2 - t416 + 0.2D1 * t89 * t408 * x2 
     #- t39 + t105 - t75 - 0.1D1 + t410)
      t426 = t420 * t425
      t428 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t414, t419
     #, 0.0D0, 0.0D0, 0.0D0)
      t433 = t61 * t426 * t428 * t114 * t115 * t117 / 0.8D1
      t434 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t414, t419
     #, 0.0D0, 0.0D0, 0.0D0)
      t437 = t132 * t354
      t438 = t6 ** 2
      t444 = log(-0.4D1 * t5 * t130 * t437 * t1 / t438)
      t448 = -t426 * t434 + t444 * t420 * t425 * t428
      t454 = 0.180D3 * t146 * t60 * t426 * t428
      t459 = t433 - (0.90D2 * t61 * t448 + t454) * t114 * t117 / 0.1440D
     #4
      t460 = FJET(XB1, XB2, s, 0.0D0, -t414, 0.0D0, t419, 0.0D0, t459)
      t462 = t340 * t3
      t464 = x2 * s * t3
      t465 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t462, t464
     #, 0.0D0, 0.0D0, 0.0D0)
      t469 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t462, t464
     #, 0.0D0, 0.0D0, 0.0D0)
      t470 = t163 * t354
      t473 = log(0.4D1 * t69 * t470)
      t478 = t60 * t465
      t480 = 0.180D3 * t146 * t478
      t487 = log(0.4D1 * t5 * t470)
      t498 = log(0.4D1 * t215 * t437)
      t500 = t498 ** 2
      t503 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t462, t464
     #, 0.0D0, 0.0D0, 0.0D0)
      t516 = t61 * t465 * t118 / 0.8D1 - (0.90D2 * t61 * (-t469 + t473 *
     # t465) + t480) * t115 * t117 / 0.720D3 - (0.90D2 * t61 * (-t469 + 
     #t487 * t465) + t480) * t114 * t117 / 0.1440D4 + (0.90D2 * t61 * (-
     #t498 * t469 + t500 * t465 / 0.2D1 + t503) - 0.180D3 * t146 * t60 *
     # (t469 - t498 * t465) + t157 * t478) * t117 / 0.1440D4
      t517 = FJET(XB1, XB2, s, -t462, 0.0D0, t464, 0.0D0, 0.0D0, t516)
      t519 = FJET(XB1, XB2, s, 0.0D0, -t345, t32, t342, -t58, t371)
      t521 = FJET(XB1, XB2, s, t10, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t278)
      t523 = FJET(XB1, XB2, s, -t52, t35, t31, t9, -t58, -t121)
      t528 = FJET(XB1, XB2, s, t375, -t374, -t378, t376, 0.0D0, t402)
      t530 = FJET(XB1, XB2, s, t419, 0.0D0, -t414, 0.0D0, 0.0D0, t459)
      t532 = FJET(XB1, XB2, s, t282, 0.0D0, -t281, 0.0D0, 0.0D0, t337)
      t534 = FJET(XB1, XB2, s, 0.0D0, t419, 0.0D0, -t414, 0.0D0, t459)
      t546 = -(-0.90D2 * t61 * t294 + 0.180D3 * t146 * t60 * t298 - t303
     #) * t114 / 0.1440D4 - t321 + t333 + t336
      t547 = FJET(XB1, XB2, s, 0.0D0, t282, 0.0D0, -t281, 0.0D0, t546)
      t549 = -t122 * pi * t125 * t119 / 0.8D1 + t279 * t278 + t338 * t33
     #7 + t372 * t371 + t403 * t402 + t405 * t402 + t460 * t459 + t517 *
     # t516 + t519 * t371 + t521 * t278 - t523 * pi * t125 * t119 / 0.8D
     #1 + t528 * t402 + t530 * t459 + t532 * t337 + t534 * t459 + t547 *
     # t546
      t550 = FJET(XB1, XB2, s, 0.0D0, t464, 0.0D0, -t462, 0.0D0, t516)
      t552 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0
     #, t32, 0.0D0, 0.0D0)
      t554 = t352 * t353
      t557 = log(0.4D1 * t86 * t130 * t554)
      t558 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0
     #, t32, 0.0D0, 0.0D0)
      t563 = t60 * t558
      t565 = 0.180D3 * t146 * t563
      t569 = (0.90D2 * t61 * (t552 - t557 * t558) - t565) * t114 * t115 
     #/ 0.720D3
      t572 = t61 * t558 * t118 / 0.8D1
      t575 = log(0.4D1 * t351 * t554)
      t583 = (0.90D2 * t61 * (-t552 + t575 * t558) + t565) * t115 * t117
     # / 0.720D3
      t586 = log(0.4D1 * t259 * t554)
      t588 = t586 ** 2
      t591 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0
     #, t32, 0.0D0, 0.0D0)
      t592 = -t586 * t552 + t588 * t558 / 0.2D1 + t591
      t596 = t552 - t586 * t558
      t600 = t157 * t563
      t604 = t569 + t572 - t583 - (-0.90D2 * t61 * t592 + 0.180D3 * t146
     # * t60 * t596 - t600) * t115 / 0.720D3
      t605 = FJET(XB1, XB2, s, -t12, t32, 0.0D0, 0.0D0, 0.0D0, t604)
      t607 = FJET(XB1, XB2, s, 0.0D0, -t462, 0.0D0, t464, 0.0D0, t516)
      t609 = FJET(XB1, XB2, s, -t378, t376, t375, -t374, 0.0D0, t402)
      t611 = FJET(XB1, XB2, s, t35, -t52, t9, t31, -t58, -t121)
      t616 = FJET(XB1, XB2, s, t342, t32, -t345, 0.0D0, -t58, t371)
      t618 = FJET(XB1, XB2, s, 0.0D0, t10, 0.0D0, 0.0D0, 0.0D0, t278)
      t630 = t569 + t572 - t583 - (-0.90D2 * t61 * t592 + 0.180D3 * t146
     # * t60 * t596 - t600) * t115 / 0.720D3
      t631 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t32, -t12, 0.0D0, t630)
      t633 = FJET(XB1, XB2, s, t464, 0.0D0, -t462, 0.0D0, 0.0D0, t516)
      t635 = FJET(XB1, XB2, s, t32, -t12, 0.0D0, 0.0D0, 0.0D0, t604)
      t637 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t10, 0.0D0, t278)
      t639 = FJET(XB1, XB2, s, t31, t9, -t52, t35, -t58, -t121)
      t644 = FJET(XB1, XB2, s, -t345, 0.0D0, t342, t32, -t58, t371)
      t653 = t433 - (0.90D2 * t61 * t448 + t454) * t114 * t117 / 0.1440D
     #4
      t654 = FJET(XB1, XB2, s, -t414, 0.0D0, t419, 0.0D0, 0.0D0, t653)
      t656 = FJET(XB1, XB2, s, 0.0D0, -t281, 0.0D0, t282, 0.0D0, t546)
      t658 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t12, t32, 0.0D0, t630)
      t660 = t550 * t516 + t604 * t605 + t607 * t516 + t609 * t402 - t61
     #1 * pi * t125 * t119 / 0.8D1 + t616 * t371 + t618 * t278 + t631 * 
     #t630 + t633 * t516 + t635 * t604 + t637 * t278 - t639 * pi * t125 
     #* t119 / 0.8D1 + t644 * t371 + t654 * t653 + t656 * t546 + t658 * 
     #t630
      rrgg2gghhardt8s2e0 = t549 + t660

      end function



      doubleprecision function rrgg2gghhardt8s2em1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D
     #0, 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t13 * t15 * t4)
      t20 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0
     #D0, 0.0D0, 0.0D0)
      t22 = t9 - t19 * t20
      t25 = pi * lh
      t28 = 0.180D3 * t25 * t7 * t20
      t30 = 0.1D1 / x3
      t33 = t20 * t30
      t34 = 0.1D1 / x2
      t37 = t8 * t33 * t34 / 0.16D2
      t38 = 0.1D1 / x1
      t41 = t8 * t33 * t38 / 0.8D1
      t42 = -(-0.90D2 * t8 * t22 + t28) * t30 / 0.1440D4 + t37 + t41
      t43 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t42)
      t45 = x2 * x3
      t47 = x2 ** 2
      t48 = t47 * x3
      t49 = cos(t10)
      t51 = Sqrt(-t45 * t4)
      t52 = t49 * t51
      t53 = 0.2D1 * t52
      t55 = 0.2D1 * t52 * x2
      t58 = 0.1D1 / (-0.1D1 + t45)
      t60 = t2 * (-x2 - x3 + 0.3D1 * t45 - t48 - t53 + t55) * t58
      t61 = -0.1D1 + x2
      t65 = t2 * t61 * (-t45 - 0.1D1 + x3 + t53) * t58
      t66 = x2 * z
      t67 = 0.1D1 - x2 + t66
      t79 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t65, t60, 0
     #.0D0, 0.0D0, 0.0D0)
      t82 = 0.1D1 / (-z * t47 * x3 + t48 + x2 - t55 + 0.2D1 * z * t49 * 
     #t51 * x2 - t66 + t45 * z - 0.2D1 * t45 - 0.1D1 + t53) * t79 * t30 
     #* t34
      t84 = t8 * t67 * t82 / 0.16D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t60, 0.0D0, -t65, 0.0D0, t84)
      t87 = t7 * t67
      t91 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t94 = log(0.4D1 * t13 * t15)
      t95 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t102 = 0.180D3 * t25 * t7 * t95
      t109 = log(0.4D1 * t15 * t12)
      t110 = t109 * pi
      t118 = t109 ** 2
      t121 = pi ** 2
      t123 = lh ** 2
      t131 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 
     #0.0D0, 0.0D0, 0.0D0)
      t134 = t95 * t30
      t138 = x2 * t12
      t141 = log(0.4D1 * t138 * t15)
      t153 = x1 ** 2
      t154 = t153 * t12
      t157 = log(0.4D1 * t154 * t15)
      t168 = -(0.90D2 * t8 * (t91 - t94 * t95) - t102) * t30 / 0.1440D4 
     #- (-0.180D3 * t25 - 0.90D2 * t110) * t7 * t91 / 0.1440D4 - (0.180D
     #3 * t110 * lh + 0.45D2 * t118 * pi + pi * (-0.30D2 * t121 + 0.180D
     #3 * t123)) * t7 * t95 / 0.1440D4 - t8 * t131 / 0.16D2 - t8 * t134 
     #* t34 / 0.16D2 + (0.90D2 * t8 * (-t91 + t141 * t95) + t102) * t34 
     #/ 0.1440D4 - t8 * t95 * t38 * t34 / 0.8D1 - (0.90D2 * t8 * (t91 - 
     #t157 * t95) - t102) * t38 / 0.720D3 - t8 * t134 * t38 / 0.8D1
      t169 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t168)
      t171 = FJET(XB1, XB2, s, 0.0D0, -t65, 0.0D0, t60, 0.0D0, t84)
      t176 = FJET(XB1, XB2, s, -t65, 0.0D0, t60, 0.0D0, 0.0D0, t84)
      t181 = t61 * s
      t182 = t181 * t1
      t184 = x2 * s * t1
      t185 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t182, t184
     #, 0.0D0, 0.0D0, 0.0D0)
      t194 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t182, t184
     #, 0.0D0, 0.0D0, 0.0D0)
      t195 = t61 ** 2
      t199 = log(0.4D1 * t138 * t15 * t195)
      t210 = t8 * t185 * t38 * t34 / 0.8D1 + t8 * t185 * t30 * t34 / 0.1
     #6D2 + (0.90D2 * t8 * (t194 - t199 * t185) - 0.180D3 * t25 * t7 * t
     #185) * t34 / 0.1440D4
      t211 = FJET(XB1, XB2, s, 0.0D0, -t182, 0.0D0, t184, 0.0D0, t210)
      t213 = -0.1D1 + x1
      t214 = t2 * t213
      t215 = t2 * x1
      t216 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t214, 0.0D
     #0, t215, 0.0D0, 0.0D0)
      t220 = t8 * t216 * t38 * t34 / 0.8D1
      t221 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t214, 0.0D
     #0, t215, 0.0D0, 0.0D0)
      t224 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t226 = t213 ** 2
      t230 = log(0.4D1 * t154 * t15 * t224 * t226)
      t232 = -t221 + t230 * t216
      t237 = 0.180D3 * t25 * t7 * t216
      t244 = t8 * t216 * t30 * t38 / 0.8D1
      t245 = t220 - (0.90D2 * t8 * t232 + t237) * t38 / 0.720D3 + t244
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t214, t215, 0.0D0, t245)
      t249 = t2 * x1 * x3
      t251 = t1 * t213
      t252 = x3 * s * t251
      t253 = t4 * s
      t255 = t253 * t1 * x1
      t256 = t253 * t251
      t257 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t256, -t252
     #, -t255, t249, 0.0D0)
      t259 = t257 * t30 * t38
      t261 = t8 * t259 / 0.8D1
      t262 = FJET(XB1, XB2, s, t249, -t252, -t255, t256, 0.0D0, -t261)
      t269 = t2 * t213 * x2 * t224
      t270 = t181 * t251
      t271 = t1 ** 2
      t276 = s * t271 * x2 * x1 * t213 * t224
      t277 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t270, -t269
     #, t215, 0.0D0, -t276)
      t279 = t277 * t38 * t34
      t281 = t8 * t279 / 0.8D1
      t282 = FJET(XB1, XB2, s, 0.0D0, -t269, t215, t270, -t276, -t281)
      t287 = FJET(XB1, XB2, s, -t255, t256, t249, -t252, 0.0D0, -t261)
      t292 = FJET(XB1, XB2, s, -t269, 0.0D0, t270, t215, -t276, -t281)
      t297 = FJET(XB1, XB2, s, -t252, t249, t256, -t255, 0.0D0, -t261)
      t302 = FJET(XB1, XB2, s, t215, t270, 0.0D0, -t269, -t276, -t281)
      t307 = FJET(XB1, XB2, s, t256, -t255, -t252, t249, 0.0D0, -t261)
      t312 = t43 * t42 + t85 * pi * t87 * t82 / 0.16D2 + t169 * t168 + t
     #171 * pi * t87 * t82 / 0.16D2 + t176 * pi * t87 * t82 / 0.16D2 + t
     #211 * t210 + t246 * t245 - t262 * pi * t7 * t259 / 0.8D1 - t282 * 
     #pi * t7 * t279 / 0.8D1 - t287 * pi * t7 * t259 / 0.8D1 - t292 * pi
     # * t7 * t279 / 0.8D1 - t297 * pi * t7 * t259 / 0.8D1 - t302 * pi *
     # t7 * t279 / 0.8D1 - t307 * pi * t7 * t259 / 0.8D1
      t313 = FJET(XB1, XB2, s, t270, t215, -t269, 0.0D0, -t276, -t281)
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t168)
      t326 = -(-0.90D2 * t8 * t22 + t28) * t30 / 0.1440D4 + t37 + t41
      t327 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t326)
      t335 = t220 - (0.90D2 * t8 * t232 + t237) * t38 / 0.720D3 + t244
      t336 = FJET(XB1, XB2, s, -t214, t215, 0.0D0, 0.0D0, 0.0D0, t335)
      t338 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t215, -t214, 0.0D0, t245)
      t340 = FJET(XB1, XB2, s, t215, -t214, 0.0D0, 0.0D0, 0.0D0, t335)
      t342 = FJET(XB1, XB2, s, t184, 0.0D0, -t182, 0.0D0, 0.0D0, t210)
      t344 = FJET(XB1, XB2, s, t60, 0.0D0, -t65, 0.0D0, 0.0D0, t84)
      t349 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t168)
      t351 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t42)
      t353 = FJET(XB1, XB2, s, 0.0D0, t184, 0.0D0, -t182, 0.0D0, t210)
      t355 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t326)
      t357 = FJET(XB1, XB2, s, -t182, 0.0D0, t184, 0.0D0, 0.0D0, t210)
      t359 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t168)
      t361 = -t313 * pi * t7 * t279 / 0.8D1 + t318 * t168 + t327 * t326 
     #+ t336 * t335 + t338 * t245 + t340 * t335 + t342 * t210 + t344 * p
     #i * t87 * t82 / 0.16D2 + t349 * t168 + t351 * t42 + t353 * t210 + 
     #t355 * t326 + t357 * t210 + t359 * t168
      rrgg2gghhardt8s2em1 = t312 + t361

      end function



      doubleprecision function rrgg2gghhardt8s2em2
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.
     #0D0, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t24 = z ** 2
      t27 = Sin(x4 * pi)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t
     #31 * pi) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t45 = t2 * (-0.1D1 + x1)
      t46 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t45, 0.0D0,
     # t43, 0.0D0, 0.0D0)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t49)
      t53 = t4 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t62 = t2 * x3
      t64 = t2 * (-0.1D1 + x3)
      t65 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t64, t62, 0
     #.0D0, 0.0D0, 0.0D0)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t68)
      t72 = t4 * t65 * t7
      t76 = x2 * s * t1
      t79 = (-0.1D1 + x2) * s * t1
      t80 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t79, t76, 0
     #.0D0, 0.0D0, 0.0D0)
      t83 = t5 * t80 * t11 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t76, 0.0D0, -t79, 0.0D0, t83)
      t87 = t4 * t80 * t11
      t90 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t68)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t76, 0.0D0, t83)
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t100 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t68)
      t108 = FJET(XB1, XB2, s, t76, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t112 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t68)
      t116 = FJET(XB1, XB2, s, -t79, 0.0D0, t76, 0.0D0, 0.0D0, t83)
      t120 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2gghhardt8s2em2 = t39 * t38 + t41 * t38 + t50 * pi * t53 / 0.8
     #D1 + t56 * pi * t53 / 0.8D1 + t60 * t38 + t69 * pi * t72 / 0.16D2 
     #+ t84 * pi * t87 / 0.16D2 + t90 * pi * t72 / 0.16D2 + t94 * pi * t
     #87 / 0.16D2 + t98 * t38 + t100 * pi * t53 / 0.8D1 + t104 * pi * t7
     #2 / 0.16D2 + t108 * pi * t87 / 0.16D2 + t112 * pi * t72 / 0.16D2 +
     # t116 * pi * t87 / 0.16D2 + t120 * pi * t53 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt8s2em3
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.
     #0D0, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gghhardt8s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0
     #.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt8s2em4
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt8s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt8s3e1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = t10 * t13
      t15 = -0.1D1 + x3
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t20 = log(-0.4D1 * t7 * t17)
      t21 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t27 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t30 = cos(t8)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t15)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t41 = log(0.4D1 * t7 * t14)
      t43 = t41 ** 2
      t49 = pi * lh
      t59 = pi ** 2
      t60 = 0.30D2 * t59
      t61 = lh ** 2
      t62 = 0.180D3 * t61
      t63 = -t60 + t62
      t64 = pi * t63
      t66 = t24 * z * t37
      t71 = 0.1D1 / x3
      t73 = 0.1D1 / x1
      t76 = t6 * t10
      t77 = t76 * t13
      t79 = log(0.4D1 * t77)
      t84 = t79 ** 2
      t87 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t95 = 0.240D3 * zeta3
      t97 = 0.120D3 * t61 * lh
      t99 = 0.60D2 * lh * t59
      t100 = -t95 - t97 + t99
      t101 = pi * t100
      t102 = t4 * t24
      t114 = x2 ** 2
      t115 = x3 * t114
      t116 = t115 * t6
      t117 = -0.1D1 + x2
      t118 = t14 * t117
      t121 = log(-0.4D1 * t116 * t118)
      t125 = log(-0.4D1 * t116 * t17)
      t132 = log(0.4D1 * t115 * t77)
      t137 = t49 * t4
      t142 = 0.1D1 / x2
      t143 = t73 * t142
      t146 = t114 * t6
      t149 = log(0.4D1 * t146 * t14)
      t151 = t149 ** 2
      t156 = log(-0.4D1 * t146 * t118)
      t158 = t156 ** 2
      t180 = x3 * t13
      t184 = log(-0.4D1 * t180 * t10 * t16)
      t189 = log(0.4D1 * t180 * t10)
      t193 = t189 ** 2
      t195 = t184 ** 2
      t231 = log(-0.4D1 * t115 * t118)
      t233 = t231 ** 2
      t238 = log(0.4D1 * t115 * t14)
      t240 = t238 ** 2
      t245 = log(-0.4D1 * t115 * t17)
      t247 = t245 ** 2
      t266 = t64 * t4
      t277 = t13 * t114
      t280 = log(0.4D1 * t277 * t10)
      t281 = t280 ** 2
      t282 = t10 * t117
      t285 = log(-0.4D1 * t277 * t282)
      t286 = t285 ** 2
      t297 = t4 * t21
      t310 = log(0.4D1 * t14)
      t311 = t310 ** 2
      t314 = t311 * t310
      t336 = rrgg2gghhard81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t344 = t59 ** 2
      t345 = t61 ** 2
      t351 = t311 ** 2
      t357 = (0.90D2 * t5 * (-(-t20 * t21 + t23 * t24 / 0.2D1 + t27) * z
     # * t37 + t41 * t21 - t43 * t24 / 0.2D1 - t27) - 0.180D3 * t49 * t4
     # * (-(t21 - t20 * t24) * z * t37 - t21 + t41 * t24) + t64 * t4 * (
     #-t24 - t66)) * t71 * t73 / 0.1440D4 - (t64 * t4 * (t21 - t79 * t24
     #) + 0.90D2 * t5 * (t84 * t21 / 0.2D1 + t87 - t84 * t79 * t24 / 0.6
     #D1 - t79 * t27) + t101 * t102 - 0.180D3 * t49 * t4 * (-t79 * t21 +
     # t84 * t24 / 0.2D1 + t27)) * t73 / 0.1440D4 + (0.90D2 * t5 * (-t12
     #1 * t24 - (t21 - t125 * t24) * z * t37 + t132 * t24) + 0.180D3 * t
     #137 * t66) * t71 * t143 / 0.720D3 - (0.90D2 * t5 * (-t149 * t21 + 
     #t151 * t24 / 0.2D1 + t156 * t21 - t158 * t24 / 0.2D1) - 0.180D3 * 
     #t49 * t4 * (-t149 * t24 + t156 * t24)) * t73 * t142 / 0.720D3 - ((
     #-0.180D3 * t21 * lh + 0.90D2 * t27 + t24 * t63) * pi * t4 * (-t184
     # * z * t37 - t189) + 0.90D2 * t5 * t24 * (-t193 * t189 / 0.6D1 - t
     #195 * t184 * z * t37 / 0.6D1) + (-0.180D3 * t27 * lh + t24 * t100 
     #+ 0.90D2 * t87 + t21 * t63) * pi * t4 * (z * t37 + 0.1D1) + (-0.18
     #0D3 * t24 * lh + 0.90D2 * t21) * pi * t4 * (t195 * z * t37 / 0.2D1
     # + t193 / 0.2D1)) * t71 / 0.2880D4 + (0.90D2 * t5 * (-t231 * t21 +
     # t233 * t24 / 0.2D1 + t238 * t21 - t240 * t24 / 0.2D1 - (-t245 * t
     #21 + t247 * t24 / 0.2D1 + t27) * z * t37) - 0.180D3 * t49 * t4 * (
     #-t231 * t24 + t238 * t24 - (t21 - t245 * t24) * z * t37) - t266 * 
     #t66) * t71 * t142 / 0.1440D4 - ((0.90D2 * t5 * t21 - 0.180D3 * t49
     # * t102) * (t281 / 0.2D1 - t286 / 0.2D1) + 0.90D2 * t5 * t24 * (t2
     #86 * t285 / 0.6D1 - t281 * t280 / 0.6D1) + (-0.180D3 * t49 * t297 
     #+ t64 * t102 + 0.90D2 * t5 * t27) * (-t280 + t285)) * t142 / 0.144
     #0D4 - (-0.90D2 * t311 * lh - t95 - t97 + t99 - 0.15D2 * t314 - t31
     #0 * t63) * pi * t297 / 0.2880D4 - (0.180D3 * t310 * lh + 0.45D2 * 
     #t311 - t60 + t62) * pi * t4 * t27 / 0.2880D4 - (-0.180D3 * lh - 0.
     #90D2 * t310) * pi * t4 * t87 / 0.2880D4 - t5 * t336 / 0.32D2 - (0.
     #30D2 * t314 * lh + t311 * t63 / 0.2D1 - t310 * t100 + t344 + 0.60D
     #2 * t345 + 0.480D3 * lh * zeta3 - 0.60D2 * t61 * t59 + 0.15D2 / 0.
     #4D1 * t351) * pi * t102 / 0.2880D4
      t358 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t357)
      t360 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t357)
      t362 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t357)
      t364 = x2 * x3
      t365 = 0.1D1 - x3 + t364
      t366 = 0.1D1 / t365
      t367 = t364 * t366
      t368 = t2 * t367
      t370 = t2 * t15 * t366
      t372 = t365 ** 2
      t373 = 0.1D1 / t372
      t374 = t15 * t373
      t378 = log(0.4D1 * t115 * t13 * t282 * t374)
      t379 = t378 * z
      t380 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t370, t368, 0.0D0)
      t382 = t378 ** 2
      t384 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t370, t368, 0.0D0)
      t387 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t370, t368, 0.0D0)
      t390 = t117 * t15
      t392 = Sqrt(t31 * t390)
      t396 = 0.1D1 / (-z - x3 + t364 + 0.2D1 * t30 * t392)
      t400 = z * t380
      t408 = z * t384 * t396
      t414 = t115 * t76
      t419 = log(0.4D1 * t414 * t13 * t117 * t374)
      t432 = (-0.90D2 * t5 * (t379 * t380 - t382 * z * t384 / 0.2D1 - z 
     #* t387) * t396 + 0.180D3 * t49 * t4 * (-t400 + t379 * t384) * t396
     # + t266 * t408) * t71 * t142 / 0.1440D4 + (-0.90D2 * t5 * (-t400 +
     # t419 * z * t384) * t396 - 0.180D3 * t137 * t408) * t71 * t143 / 0
     #.720D3
      t433 = FJET(XB1, XB2, s, 0.0D0, t368, 0.0D0, -t370, 0.0D0, t432)
      t436 = t1 * x1
      t437 = x1 * z
      t438 = -z - x1 + t437
      t439 = 0.1D1 / t438
      t441 = t117 * s * t436 * t439
      t442 = -0.1D1 + x1
      t443 = t2 * t442
      t445 = x2 * s * t436
      t446 = t1 ** 2
      t447 = s * t446
      t450 = x1 * t442 * t439
      t451 = t447 * t117 * t450
      t452 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t445, t441,
     # -t443, 0.0D0, -t451)
      t453 = 0.1D1 / t11
      t454 = t453 * t439
      t455 = t442 ** 2
      t457 = t454 * t455 * t117
      t460 = log(0.4D1 * t414 * t457)
      t461 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t445, t441,
     # -t443, 0.0D0, -t451)
      t466 = t4 * t461
      t472 = t146 * t10
      t475 = log(0.4D1 * t472 * t457)
      t477 = t475 ** 2
      t480 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t445, t441,
     # -t443, 0.0D0, -t451)
      t494 = (0.90D2 * t5 * (-t452 + t460 * t461) + 0.180D3 * t49 * t466
     #) * t71 * t143 / 0.720D3 - (0.90D2 * t5 * (-t475 * t452 + t477 * t
     #461 / 0.2D1 + t480) - 0.180D3 * t49 * t4 * (t452 - t475 * t461) + 
     #t64 * t466) * t73 * t142 / 0.720D3
      t495 = FJET(XB1, XB2, s, 0.0D0, t441, -t443, t445, -t451, t494)
      t498 = t2 * x1 * t439
      t499 = t447 * t450
      t500 = t7 * t10
      t502 = t454 * t455 * t16
      t505 = log(0.4D1 * t500 * t502)
      t506 = t505 * z
      t507 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49
     #8, -t443, 0.0D0, t499)
      t510 = t505 ** 2
      t512 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49
     #8, -t443, 0.0D0, t499)
      t513 = t438 * t512
      t516 = z * t438
      t517 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49
     #8, -t443, 0.0D0, t499)
      t520 = x3 * x1
      t521 = t520 * z
      t523 = 0.2D1 * t7 * z
      t524 = t6 * t11
      t525 = t524 * x3
      t526 = x1 * t11
      t527 = t520 * t11
      t528 = z * t30
      t529 = x3 * t438
      t531 = Sqrt(t529 * t15)
      t535 = 0.1D1 / (-t437 - t521 - t31 + t523 - t7 - t525 + t526 + t52
     #7 - t11 + 0.2D1 * t528 * t531)
      t537 = t454 * t455
      t540 = log(-0.4D1 * t500 * t537)
      t542 = t540 ** 2
      t548 = t516 * t507
      t560 = t4 * (-t516 * t512 * t535 + t512)
      t565 = (0.90D2 * t5 * (-(-t506 * t438 * t507 + t510 * z * t513 / 0
     #.2D1 + t516 * t517) * t535 - t540 * t507 + t542 * t512 / 0.2D1 + t
     #517) - 0.180D3 * t49 * t4 * (-(t548 - t506 * t513) * t535 + t507 -
     # t540 * t512) + t64 * t560) * t71 * t73 / 0.1440D4
      t568 = log(-0.4D1 * t76 * t537)
      t570 = -t507 + t568 * t512
      t573 = t568 ** 2
      t576 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49
     #8, -t443, 0.0D0, t499)
      t581 = -t573 * t507 / 0.2D1 - t576 + t573 * t568 * t512 / 0.6D1 + 
     #t568 * t517
      t584 = t4 * t512
      t585 = t101 * t584
      t589 = t568 * t507 - t573 * t512 / 0.2D1 - t517
      t598 = log(0.4D1 * t414 * t502)
      t604 = t439 * t455
      t608 = log(-0.4D1 * t116 * t10 * t453 * t604)
      t618 = (0.90D2 * t5 * (-(t548 - t598 * z * t513) * t535 + t507 - t
     #608 * t512) - 0.180D3 * t49 * t560) * t71 * t143 / 0.720D3
      t621 = log(-0.4D1 * t472 * t537)
      t623 = t621 ** 2
      t626 = t621 * t507 - t623 * t512 / 0.2D1 - t517
      t630 = -t507 + t621 * t512
      t634 = t64 * t584
      t638 = (0.90D2 * t5 * t626 - 0.180D3 * t49 * t4 * t630 - t634) * t
     #73 * t142 / 0.720D3
      t639 = t565 - (t64 * t4 * t570 + 0.90D2 * t5 * t581 - t585 - 0.180
     #D3 * t49 * t4 * t589) * t73 / 0.1440D4 + t618 - t638
      t640 = FJET(XB1, XB2, s, 0.0D0, -t443, -t498, 0.0D0, t499, t639)
      t642 = FJET(XB1, XB2, s, 0.0D0, -t498, -t443, 0.0D0, t499, t639)
      t644 = FJET(XB1, XB2, s, 0.0D0, -t370, 0.0D0, t368, 0.0D0, t432)
      t646 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t357)
      t648 = FJET(XB1, XB2, s, t445, -t443, t441, 0.0D0, -t451, t494)
      t650 = FJET(XB1, XB2, s, t368, 0.0D0, -t370, 0.0D0, 0.0D0, t432)
      t652 = FJET(XB1, XB2, s, t441, 0.0D0, t445, -t443, -t451, t494)
      t657 = t15 * s * t1 * t442 * t366
      t658 = t2 * x1
      t660 = Sqrt(-t529 * t390)
      t661 = t30 * t660
      t667 = t658 * x2 * (-x3 + t364 - z + t31 - x1 + t520 + t437 - t521
     # + 0.2D1 * t661) * t439 * t366
      t668 = t443 * t367
      t669 = t661 * x2
      t671 = t437 * t115
      t675 = x1 * t114 * x3
      t679 = t658 * (0.1D1 - x3 - x2 + t364 + 0.2D1 * t669 - t671 + z * 
     #t114 * x3 + t675) * t439 * t366
      t686 = log(-0.4D1 * t115 * t76 * t453 * t604 * t390 * t373)
      t687 = x1 * x2
      t688 = t687 * z
      t689 = -z - t687 + t688
      t693 = t6 * x2
      t701 = t31 + t7 - t526 + t437 - 0.2D1 * t528 * t660 + t693 + t688 
     #- 0.2D1 * t693 * z - t687 * t11 + t524 * x2 - t364 * z + t520 * x2
     # - t7 * x2
      t706 = x2 * z
      t716 = 0.2D1 * t437 * t669 - t675 + t521 - t523 - t527 + t525 + t1
     #1 + t520 * x2 * t11 - 0.2D1 * t520 * t706 - 0.2D1 * x1 * t30 * t66
     #0 * x2 + t671 - t524 * t364 + 0.2D1 * t7 * t706
      t718 = 0.1D1 / (t701 + t716)
      t720 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t667, -t679
     #, t657, -t668, -t451)
      t723 = t689 * t718
      t724 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t667, -t679
     #, t657, -t668, -t451)
      t734 = 0.90D2 * t5 * (-t686 * t689 * t718 * t438 * t720 + t723 * t
     #438 * t724) - 0.180D3 * t137 * t723 * t438 * t720
      t737 = t734 * t71 * t143 / 0.720D3
      t738 = FJET(XB1, XB2, s, t657, t667, -t668, -t679, -t451, t737)
      t741 = t71 * t73 * t142
      t744 = FJET(XB1, XB2, s, t667, t657, -t679, -t668, -t451, t737)
      t760 = (t64 * t4 * t570 + 0.90D2 * t5 * t581 - t585 - 0.180D3 * t4
     #9 * t4 * t589) * t73 / 0.1440D4
      t761 = t565 - t760 + t618 - t638
      t762 = FJET(XB1, XB2, s, -t443, 0.0D0, 0.0D0, -t498, t499, t761)
      t764 = FJET(XB1, XB2, s, -t443, t445, 0.0D0, t441, -t451, t494)
      t777 = t565 - t760 + t618 - (0.90D2 * t5 * t626 - 0.180D3 * t49 * 
     #t4 * t630 - t634) * t73 * t142 / 0.720D3
      t778 = FJET(XB1, XB2, s, -t498, 0.0D0, 0.0D0, -t443, t499, t777)
      t780 = FJET(XB1, XB2, s, -t370, 0.0D0, t368, 0.0D0, 0.0D0, t432)
      t782 = FJET(XB1, XB2, s, -t679, -t668, t667, t657, -t451, t737)
      t786 = FJET(XB1, XB2, s, -t668, -t679, t657, t667, -t451, t737)
      rrgg2gghhardt8s3e1 = t358 * t357 + t360 * t357 + t362 * t357 + t43
     #3 * t432 + t495 * t494 + t640 * t639 + t642 * t639 + t644 * t432 +
     # t646 * t357 + t648 * t494 + t650 * t432 + t652 * t494 + t738 * t7
     #34 * t741 / 0.720D3 + t744 * t734 * t741 / 0.720D3 + t762 * t761 +
     # t764 * t494 + t778 * t777 + t780 * t432 + t782 * t734 * t741 / 0.
     #720D3 + t786 * t734 * t741 / 0.720D3

      end function



      doubleprecision function rrgg2gghhardt8s3e0
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
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
      t22 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t26 = cos(t9)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t16)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t8 * t15)
      t42 = pi * lh
      t43 = t22 * z
      t44 = t43 * t33
      t50 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t57 = 0.1D1 / x2
      t58 = t52 * t57
      t62 = x2 ** 2
      t63 = t62 * t7
      t66 = log(0.4D1 * t63 * t15)
      t68 = -0.1D1 + x2
      t69 = t15 * t68
      t72 = log(-0.4D1 * t63 * t69)
      t79 = t7 * t11
      t82 = log(0.4D1 * t79 * t14)
      t84 = t82 ** 2
      t87 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t96 = pi ** 2
      t97 = 0.30D2 * t96
      t98 = lh ** 2
      t99 = 0.180D3 * t98
      t100 = -t97 + t99
      t101 = pi * t100
      t102 = t4 * t22
      t107 = x3 * t14
      t111 = log(-0.4D1 * t107 * t11 * t17)
      t112 = t111 ** 2
      t117 = log(0.4D1 * t107 * t11)
      t118 = t117 ** 2
      t148 = log(0.4D1 * t15)
      t151 = t148 ** 2
      t158 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t182 = x3 * t62
      t185 = log(-0.4D1 * t182 * t69)
      t189 = log(0.4D1 * t182 * t15)
      t193 = log(-0.4D1 * t182 * t18)
      t201 = t42 * t4
      t208 = t14 * t62
      t211 = log(0.4D1 * t208 * t11)
      t212 = t211 ** 2
      t213 = t11 * t68
      t216 = log(-0.4D1 * t208 * t213)
      t217 = t216 ** 2
      t233 = (0.90D2 * t5 * (-(t6 - t21 * t22) * z * t33 - t6 + t37 * t2
     #2) - 0.180D3 * t42 * t4 * (-t22 - t44)) * t50 * t52 / 0.1440D4 - t
     #5 * t43 * t33 * t50 * t58 / 0.8D1 - t5 * (-t66 * t22 + t72 * t22) 
     #* t52 * t57 / 0.8D1 - (0.90D2 * t5 * (-t82 * t6 + t84 * t22 / 0.2D
     #1 + t87) - 0.180D3 * t42 * t4 * (t6 - t82 * t22) + t101 * t102) * 
     #t52 / 0.1440D4 - (0.90D2 * t5 * t22 * (t112 * z * t33 / 0.2D1 + t1
     #18 / 0.2D1) + (-0.180D3 * t22 * lh + 0.90D2 * t6) * pi * t4 * (-t1
     #11 * z * t33 - t117) + (-0.180D3 * t6 * lh + 0.90D2 * t87 + t22 * 
     #t100) * pi * t4 * (z * t33 + 0.1D1)) * t50 / 0.2880D4 - (0.180D3 *
     # t148 * lh + 0.45D2 * t151 - t97 + t99) * pi * t4 * t6 / 0.2880D4 
     #- t5 * t158 / 0.32D2 - (-0.90D2 * t151 * lh - 0.240D3 * zeta3 - 0.
     #120D3 * t98 * lh + 0.60D2 * lh * t96 - 0.15D2 * t151 * t148 - t148
     # * t100) * pi * t102 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t148) 
     #* pi * t4 * t87 / 0.2880D4 + (0.90D2 * t5 * (-t185 * t22 + t189 * 
     #t22 - (t6 - t193 * t22) * z * t33) + 0.180D3 * t201 * t44) * t50 *
     # t57 / 0.1440D4 - (0.90D2 * t5 * t22 * (t212 / 0.2D1 - t217 / 0.2D
     #1) + (0.90D2 * t5 * t6 - 0.180D3 * t42 * t102) * (-t211 + t216)) *
     # t57 / 0.1440D4
      t234 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t233)
      t236 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t233)
      t238 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t233)
      t240 = x2 * x3
      t241 = 0.1D1 - x3 + t240
      t242 = 0.1D1 / t241
      t243 = t240 * t242
      t244 = t2 * t243
      t246 = t2 * t16 * t242
      t247 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t246, t244, 0.0D0)
      t248 = z * t247
      t250 = t68 * t16
      t252 = Sqrt(t27 * t250)
      t256 = 0.1D1 / (-z - x3 + t240 + 0.2D1 * t26 * t252)
      t261 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t246, t244, 0.0D0)
      t264 = t241 ** 2
      t270 = log(0.4D1 * t182 * t14 * t213 * t16 / t264)
      t284 = t5 * t248 * t256 * t50 * t58 / 0.8D1 + (-0.90D2 * t5 * (-z 
     #* t261 + t270 * z * t247) * t256 - 0.180D3 * t201 * t248 * t256) *
     # t50 * t57 / 0.1440D4
      t285 = FJET(XB1, XB2, s, 0.0D0, t244, 0.0D0, -t246, 0.0D0, t284)
      t288 = t1 * x1
      t289 = x1 * z
      t290 = -z - x1 + t289
      t291 = 0.1D1 / t290
      t293 = t68 * s * t288 * t291
      t294 = -0.1D1 + x1
      t295 = t2 * t294
      t297 = x2 * s * t288
      t298 = t1 ** 2
      t299 = s * t298
      t302 = x1 * t294 * t291
      t303 = t299 * t68 * t302
      t304 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t297, t293,
     # -t295, 0.0D0, -t303)
      t307 = t50 * t52 * t57
      t310 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t297, t293,
     # -t295, 0.0D0, -t303)
      t311 = t63 * t11
      t313 = 0.1D1 / t12 * t291
      t314 = t294 ** 2
      t319 = log(0.4D1 * t311 * t313 * t314 * t68)
      t331 = -t5 * t304 * t307 / 0.8D1 - (0.90D2 * t5 * (t310 - t319 * t
     #304) - 0.180D3 * t42 * t4 * t304) * t52 * t57 / 0.720D3
      t332 = FJET(XB1, XB2, s, 0.0D0, t293, -t295, t297, -t303, t331)
      t335 = t2 * x1 * t291
      t336 = t299 * t302
      t337 = z * t290
      t338 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, -t295, 0.0D0, t336)
      t340 = t8 * t11
      t345 = log(0.4D1 * t340 * t313 * t314 * t17)
      t347 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, -t295, 0.0D0, t336)
      t351 = x3 * x1
      t352 = t351 * z
      t354 = 0.2D1 * t8 * z
      t355 = t7 * t12
      t356 = t355 * x3
      t357 = x1 * t12
      t358 = t351 * t12
      t359 = z * t26
      t360 = x3 * t290
      t362 = Sqrt(t360 * t16)
      t366 = 0.1D1 / (-t289 - t352 - t27 + t354 - t8 - t356 + t357 + t35
     #8 - t12 + 0.2D1 * t359 * t362)
      t368 = t313 * t314
      t371 = log(-0.4D1 * t340 * t368)
      t378 = -t337 * t347 * t366 + t347
      t385 = (0.90D2 * t5 * (-(t337 * t338 - t345 * z * t290 * t347) * t
     #366 + t338 - t371 * t347) - 0.180D3 * t42 * t4 * t378) * t50 * t52
     # / 0.1440D4
      t388 = t5 * t378 * t307 / 0.8D1
      t391 = log(-0.4D1 * t311 * t368)
      t393 = -t338 + t391 * t347
      t396 = t4 * t347
      t398 = 0.180D3 * t42 * t396
      t402 = (0.90D2 * t5 * t393 + t398) * t52 * t57 / 0.720D3
      t405 = log(-0.4D1 * t79 * t368)
      t407 = t405 ** 2
      t410 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t33
     #5, -t295, 0.0D0, t336)
      t411 = t405 * t338 - t407 * t347 / 0.2D1 - t410
      t415 = -t338 + t405 * t347
      t419 = t101 * t396
      t423 = t385 + t388 - t402 - (0.90D2 * t5 * t411 - 0.180D3 * t42 * 
     #t4 * t415 - t419) * t52 / 0.1440D4
      t424 = FJET(XB1, XB2, s, 0.0D0, -t295, -t335, 0.0D0, t336, t423)
      t426 = FJET(XB1, XB2, s, 0.0D0, -t335, -t295, 0.0D0, t336, t423)
      t428 = FJET(XB1, XB2, s, 0.0D0, -t246, 0.0D0, t244, 0.0D0, t284)
      t430 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t233)
      t432 = FJET(XB1, XB2, s, t297, -t295, t293, 0.0D0, -t303, t331)
      t434 = FJET(XB1, XB2, s, t244, 0.0D0, -t246, 0.0D0, 0.0D0, t284)
      t436 = FJET(XB1, XB2, s, t293, 0.0D0, t297, -t295, -t303, t331)
      t441 = t16 * s * t1 * t294 * t242
      t442 = t2 * x1
      t444 = Sqrt(-t360 * t250)
      t445 = t26 * t444
      t451 = t442 * x2 * (-x3 + t240 - z + t27 - x1 + t351 + t289 - t352
     # + 0.2D1 * t445) * t291 * t242
      t452 = t295 * t243
      t453 = t445 * x2
      t455 = t289 * t182
      t459 = x1 * t62 * x3
      t463 = t442 * (0.1D1 - x3 - x2 + t240 + 0.2D1 * t453 - t455 + z * 
     #t62 * x3 + t459) * t291 * t242
      t464 = x1 * x2
      t465 = t464 * z
      t466 = -z - t464 + t465
      t471 = t7 * x2
      t479 = 0.2D1 * t289 * t453 + t352 - t354 + t356 - t358 + t465 - 0.
     #2D1 * t359 * t444 - 0.2D1 * t471 * z - t464 * t12 + t355 * x2 - t2
     #40 * z + t351 * x2 - t8 * x2
      t484 = x2 * z
      t492 = t455 - 0.2D1 * x1 * t26 * t444 * x2 - 0.2D1 * t351 * t484 +
     # t351 * x2 * t12 + 0.2D1 * t8 * t484 - t355 * t240 + t289 + t27 + 
     #t8 - t357 + t471 + t12 - t459
      t494 = 0.1D1 / (t479 + t492)
      t497 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t451, -t463
     #, t441, -t452, -t303)
      t499 = t290 * t497 * t307
      t501 = t5 * t466 * t494 * t499 / 0.8D1
      t502 = FJET(XB1, XB2, s, t441, t451, -t452, -t463, -t303, t501)
      t505 = t4 * t466 * t494
      t509 = FJET(XB1, XB2, s, t451, t441, -t463, -t452, -t303, t501)
      t523 = (0.90D2 * t5 * t411 - 0.180D3 * t42 * t4 * t415 - t419) * t
     #52 / 0.1440D4
      t524 = t385 + t388 - t402 - t523
      t525 = FJET(XB1, XB2, s, -t295, 0.0D0, 0.0D0, -t335, t336, t524)
      t527 = FJET(XB1, XB2, s, -t295, t297, 0.0D0, t293, -t303, t331)
      t536 = t385 + t388 - (0.90D2 * t5 * t393 + t398) * t52 * t57 / 0.7
     #20D3 - t523
      t537 = FJET(XB1, XB2, s, -t335, 0.0D0, 0.0D0, -t295, t336, t536)
      t539 = FJET(XB1, XB2, s, -t246, 0.0D0, t244, 0.0D0, 0.0D0, t284)
      t541 = FJET(XB1, XB2, s, -t463, -t452, t451, t441, -t303, t501)
      t546 = FJET(XB1, XB2, s, -t452, -t463, t441, t451, -t303, t501)
      rrgg2gghhardt8s3e0 = t234 * t233 + t236 * t233 + t238 * t233 + t28
     #5 * t284 + t332 * t331 + t424 * t423 + t426 * t423 + t428 * t284 +
     # t430 * t233 + t432 * t331 + t434 * t284 + t436 * t331 + t502 * pi
     # * t505 * t499 / 0.8D1 + t509 * pi * t505 * t499 / 0.8D1 + t525 * 
     #t524 + t527 * t331 + t537 * t536 + t539 * t284 + t541 * pi * t505 
     #* t499 / 0.8D1 + t546 * pi * t505 * t499 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt8s3em1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
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
      t39 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t43 = z * t28
      t48 = 0.1D1 / x3
      t51 = x1 ** 2
      t52 = t51 * t13
      t55 = log(0.4D1 * t52 * t9)
      t60 = pi * lh
      t61 = t4 * t6
      t65 = 0.1D1 / x1
      t76 = 0.1D1 / x2
      t77 = t48 * t76
      t81 = x2 ** 2
      t82 = t9 * t81
      t85 = log(0.4D1 * t82 * t13)
      t86 = -0.1D1 + x2
      t90 = log(-0.4D1 * t82 * t13 * t86)
      t99 = log(0.4D1 * t9 * t13)
      t108 = t99 ** 2
      t110 = pi ** 2
      t112 = lh ** 2
      t118 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t121 = -(0.90D2 * t5 * t6 * (-t19 * z * t28 - t32) + (-0.180D3 * t
     #6 * lh + 0.90D2 * t39) * pi * t4 * (t43 + 0.1D1)) * t48 / 0.2880D4
     # - (0.90D2 * t5 * (t39 - t55 * t6) - 0.180D3 * t60 * t61) * t65 / 
     #0.1440D4 + t5 * (-t6 - t6 * z * t28) * t48 * t65 / 0.16D2 - t5 * t
     #6 * t43 * t77 / 0.16D2 - t5 * t6 * (-t85 + t90) * t76 / 0.16D2 - (
     #-0.180D3 * lh - 0.90D2 * t99) * pi * t4 * t39 / 0.2880D4 - (0.180D
     #3 * t99 * lh + 0.45D2 * t108 - 0.30D2 * t110 + 0.180D3 * t112) * p
     #i * t61 / 0.2880D4 - t5 * t118 / 0.32D2
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t124 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t126 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t128 = x2 * x3
      t130 = 0.1D1 / (0.1D1 - x3 + t128)
      t132 = t2 * t128 * t130
      t134 = t2 * t14 * t130
      t136 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t134, t132, 0.0D0)
      t139 = Sqrt(t22 * t86 * t14)
      t145 = t136 / (-z - x3 + t128 + 0.2D1 * t21 * t139) * t77
      t147 = t5 * z * t145 / 0.16D2
      t148 = FJET(XB1, XB2, s, 0.0D0, t132, 0.0D0, -t134, 0.0D0, t147)
      t150 = t4 * z
      t155 = t1 * x1
      t156 = x1 * z
      t157 = -z - x1 + t156
      t158 = 0.1D1 / t157
      t160 = t86 * s * t155 * t158
      t161 = -0.1D1 + x1
      t162 = t2 * t161
      t164 = x2 * s * t155
      t165 = t1 ** 2
      t166 = s * t165
      t169 = x1 * t161 * t158
      t170 = t166 * t86 * t169
      t171 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t164, t160,
     # -t162, 0.0D0, -t170)
      t173 = t171 * t65 * t76
      t175 = t5 * t173 / 0.8D1
      t176 = FJET(XB1, XB2, s, 0.0D0, t160, -t162, t164, -t170, -t175)
      t182 = t2 * x1 * t158
      t183 = t166 * t169
      t184 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t18
     #2, -t162, 0.0D0, t183)
      t188 = t5 * t184 * t65 * t76 / 0.8D1
      t189 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t18
     #2, -t162, 0.0D0, t183)
      t192 = t161 ** 2
      t196 = log(-0.4D1 * t52 / t7 * t158 * t192)
      t198 = -t189 + t196 * t184
      t203 = 0.180D3 * t60 * t4 * t184
      t208 = x3 * x1
      t210 = x3 * t51
      t220 = Sqrt(x3 * t157 * t14)
      t231 = t5 * (-z * t157 * t184 / (-t156 - t208 * z - t22 + 0.2D1 * 
     #t210 * z - t210 - t51 * t7 * x3 + x1 * t7 + t208 * t7 - t7 + 0.2D1
     # * z * t21 * t220) + t184) * t48 * t65 / 0.16D2
      t232 = t188 - (0.90D2 * t5 * t198 + t203) * t65 / 0.1440D4 + t231
      t233 = FJET(XB1, XB2, s, 0.0D0, -t162, -t182, 0.0D0, t183, t232)
      t235 = FJET(XB1, XB2, s, 0.0D0, -t182, -t162, 0.0D0, t183, t232)
      t237 = FJET(XB1, XB2, s, 0.0D0, -t134, 0.0D0, t132, 0.0D0, t147)
      t242 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t244 = FJET(XB1, XB2, s, t164, -t162, t160, 0.0D0, -t170, -t175)
      t249 = FJET(XB1, XB2, s, t132, 0.0D0, -t134, 0.0D0, 0.0D0, t147)
      t254 = FJET(XB1, XB2, s, t160, 0.0D0, t164, -t162, -t170, -t175)
      t265 = t188 - (0.90D2 * t5 * t198 + t203) * t65 / 0.1440D4 + t231
      t266 = FJET(XB1, XB2, s, -t162, 0.0D0, 0.0D0, -t182, t183, t265)
      t268 = FJET(XB1, XB2, s, -t162, t164, 0.0D0, t160, -t170, -t175)
      t273 = FJET(XB1, XB2, s, -t182, 0.0D0, 0.0D0, -t162, t183, t265)
      t275 = FJET(XB1, XB2, s, -t134, 0.0D0, t132, 0.0D0, 0.0D0, t147)
      rrgg2gghhardt8s3em1 = t122 * t121 + t124 * t121 + t126 * t121 + t1
     #48 * pi * t150 * t145 / 0.16D2 - t176 * pi * t4 * t173 / 0.8D1 + t
     #233 * t232 + t235 * t232 + t237 * pi * t150 * t145 / 0.16D2 + t242
     # * t121 - t244 * pi * t4 * t173 / 0.8D1 + t249 * pi * t150 * t145 
     #/ 0.16D2 - t254 * pi * t4 * t173 / 0.8D1 + t266 * t265 - t268 * pi
     # * t4 * t173 / 0.8D1 + t273 * t265 + t275 * pi * t150 * t145 / 0.1
     #6D2

      end function



      doubleprecision function rrgg2gghhardt8s3em2
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = x4 * pi
      t12 = cos(t11)
      t16 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t28 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t32 = z ** 2
      t35 = Sin(t11)
      t36 = t35 ** 2
      t39 = log(0.4D1 / t32 / z * t36)
      t46 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * (z / (-z - x3 + 0.2D1 * t
     #12 * t16) + 0.1D1) / x3 / 0.32D2 - t5 * t28 / 0.32D2 - (-0.180D3 *
     # lh - 0.90D2 * t39) * pi * t4 * t6 / 0.2880D4
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t46)
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t46)
      t51 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t46)
      t53 = -0.1D1 + x1
      t54 = t2 * t53
      t57 = 0.1D1 / (-z - x1 + x1 * z)
      t59 = t2 * x1 * t57
      t60 = t1 ** 2
      t64 = s * t60 * x1 * t53 * t57
      t65 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t59,
     # -t54, 0.0D0, t64)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, -t54, -t59, 0.0D0, t64, t68)
      t72 = t4 * t65 * t7
      t75 = FJET(XB1, XB2, s, 0.0D0, -t59, -t54, 0.0D0, t64, t68)
      t79 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t46)
      t81 = FJET(XB1, XB2, s, -t54, 0.0D0, 0.0D0, -t59, t64, t68)
      t85 = FJET(XB1, XB2, s, -t59, 0.0D0, 0.0D0, -t54, t64, t68)
      rrgg2gghhardt8s3em2 = t47 * t46 + t49 * t46 + t51 * t46 + t69 * pi
     # * t72 / 0.16D2 + t75 * pi * t72 / 0.16D2 + t79 * t46 + t81 * pi *
     # t72 / 0.16D2 + t85 * pi * t72 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt8s3em3
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gghhardt8s3em3 = -t9 * pi * t11 / 0.32D2 - t13 * pi * t11 / 0
     #.32D2 - t16 * pi * t11 / 0.32D2 - t19 * pi * t11 / 0.32D2

      end function



      doubleprecision function rrgg2gghhardt8s3em4
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt8s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt8s4e1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t18 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t24 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t35 = 0.30D2 * t34
      t36 = lh ** 2
      t37 = 0.180D3 * t36
      t38 = -t35 + t37
      t39 = pi * t38
      t40 = t4 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t6 * t13
      t49 = t48 * t10
      t51 = log(0.4D1 * t49)
      t56 = t51 ** 2
      t59 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t67 = 0.240D3 * zeta3
      t69 = 0.120D3 * t36 * lh
      t71 = 0.60D2 * lh * t34
      t72 = -t67 - t69 + t71
      t73 = pi * t72
      t74 = t73 * t40
      t85 = t7 * x2
      t86 = -0.1D1 + x2
      t87 = t86 ** 2
      t88 = t14 * t87
      t91 = log(0.4D1 * t85 * t88)
      t93 = x2 * x3
      t96 = log(0.4D1 * t93 * t49)
      t101 = 0.1D1 / x2
      t102 = t43 * t45 * t101
      t105 = t6 * x2
      t108 = log(0.4D1 * t105 * t88)
      t110 = t108 ** 2
      t113 = t105 * t14
      t115 = log(0.4D1 * t113)
      t117 = t115 ** 2
      t133 = x3 * t10
      t136 = log(0.4D1 * t133 * t13)
      t141 = t136 ** 2
      t163 = log(0.4D1 * t93 * t14)
      t165 = t163 ** 2
      t170 = log(0.4D1 * t93 * t88)
      t172 = t170 ** 2
      t193 = x2 * t10
      t194 = t193 * t13
      t196 = log(0.4D1 * t194)
      t197 = t196 ** 2
      t201 = log(0.4D1 * t193 * t13 * t87)
      t202 = t201 ** 2
      t213 = t4 * t18
      t225 = log(0.4D1 * t14)
      t226 = t225 ** 2
      t229 = t226 * t225
      t251 = rrgg2gghhard81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t259 = t34 ** 2
      t260 = t36 ** 2
      t266 = t226 ** 2
      t272 = (0.90D2 * t5 * (t17 * t18 - t20 * t21 / 0.2D1 - t24) - 0.18
     #0D3 * t28 * t4 * (-t18 + t17 * t21) - t41) * t43 * t45 / 0.720D3 +
     # (t39 * t4 * (-t18 + t51 * t21) + 0.90D2 * t5 * (-t56 * t18 / 0.2D
     #1 - t59 + t56 * t51 * t21 / 0.6D1 + t51 * t24) - t74 - 0.180D3 * t
     #28 * t4 * (t51 * t18 - t56 * t21 / 0.2D1 - t24)) * t45 / 0.720D3 -
     # t5 * (t91 * t21 - t96 * t21) * t102 / 0.8D1 - (0.90D2 * t5 * (t10
     #8 * t18 - t110 * t21 / 0.2D1 - t115 * t18 + t117 * t21 / 0.2D1) - 
     #0.180D3 * t28 * t4 * (t108 * t21 - t115 * t21)) * t45 * t101 / 0.7
     #20D3 + (t39 * t4 * (-t18 + t136 * t21) + 0.90D2 * t5 * (-t141 * t1
     #8 / 0.2D1 - t59 + t141 * t136 * t21 / 0.6D1 + t136 * t24) - t74 - 
     #0.180D3 * t28 * t4 * (t136 * t18 - t141 * t21 / 0.2D1 - t24)) * t4
     #3 / 0.1440D4 + (0.90D2 * t5 * (t163 * t18 - t165 * t21 / 0.2D1 - t
     #170 * t18 + t172 * t21 / 0.2D1) - 0.180D3 * t28 * t4 * (t163 * t21
     # - t170 * t21)) * t43 * t101 / 0.1440D4 - ((0.90D2 * t5 * t18 - 0.
     #180D3 * t28 * t40) * (t197 / 0.2D1 - t202 / 0.2D1) + 0.90D2 * t5 *
     # t21 * (t202 * t201 / 0.6D1 - t197 * t196 / 0.6D1) + (-0.180D3 * t
     #28 * t213 + t41 + 0.90D2 * t5 * t24) * (-t196 + t201)) * t101 / 0.
     #1440D4 - (-0.90D2 * t226 * lh - t67 - t69 + t71 - 0.15D2 * t229 - 
     #t225 * t38) * pi * t213 / 0.1440D4 - (0.180D3 * t225 * lh + 0.45D2
     # * t226 - t35 + t37) * pi * t4 * t24 / 0.1440D4 - (-0.180D3 * lh -
     # 0.90D2 * t225) * pi * t4 * t59 / 0.1440D4 - t5 * t251 / 0.16D2 - 
     #(0.30D2 * t229 * lh + t226 * t38 / 0.2D1 - t225 * t72 + t259 + 0.6
     #0D2 * t260 + 0.480D3 * lh * zeta3 - 0.60D2 * t36 * t34 + 0.15D2 / 
     #0.4D1 * t266) * pi * t40 / 0.1440D4
      t273 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t272)
      t275 = x1 * x2
      t276 = x1 * z
      t277 = -z - x1 + t276
      t278 = 0.1D1 / t277
      t280 = t2 * t275 * t278
      t282 = t1 * x1
      t283 = t86 * s * t282
      t284 = -0.1D1 + x1
      t285 = t2 * t284
      t286 = t1 ** 2
      t291 = s * t286 * x2 * x1 * t284 * t278
      t292 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t283, -t28
     #0, -t285, 0.0D0, t291)
      t294 = 0.1D1 / t8
      t295 = t294 * t278
      t296 = t284 ** 2
      t298 = t295 * t296 * t87
      t301 = log(-0.4D1 * t93 * t48 * t298)
      t302 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t283, -t28
     #0, -t285, 0.0D0, t291)
      t307 = t4 * t302
      t312 = t45 * t101
      t314 = t105 * t13
      t317 = log(-0.4D1 * t314 * t298)
      t319 = t317 ** 2
      t322 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t283, -t28
     #0, -t285, 0.0D0, t291)
      t336 = -(0.90D2 * t5 * (t292 - t301 * t302) - 0.180D3 * t28 * t307
     #) * t43 * t312 / 0.720D3 - (0.90D2 * t5 * (-t317 * t292 + t319 * t
     #302 / 0.2D1 + t322) - 0.180D3 * t28 * t4 * (t292 - t317 * t302) + 
     #t39 * t307) * t45 * t101 / 0.720D3
      t337 = FJET(XB1, XB2, s, -t280, 0.0D0, -t283, -t285, t291, t336)
      t339 = FJET(XB1, XB2, s, 0.0D0, -t280, -t285, -t283, t291, t336)
      t341 = -0.1D1 + x3
      t342 = t341 * s
      t343 = t1 * t284
      t344 = t342 * t343
      t345 = t342 * t282
      t347 = x3 * s * t343
      t348 = x3 * x1
      t349 = t2 * t348
      t350 = t7 * t13
      t351 = t296 * t341
      t355 = log(0.4D1 * t350 * t295 * t351)
      t356 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t345, t349
     #, t344, -t347, 0.0D0)
      t358 = t355 ** 2
      t359 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t345, t349
     #, t344, -t347, 0.0D0)
      t362 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, -t345, t349
     #, t344, -t347, 0.0D0)
      t371 = t4 * t359
      t382 = log(0.4D1 * t48 * t294 * x2 * x3 * t278 * t351)
      t393 = (0.90D2 * t5 * (t355 * t356 - t358 * t359 / 0.2D1 - t362) -
     # 0.180D3 * t28 * t4 * (-t356 + t355 * t359) - t39 * t371) * t43 * 
     #t45 / 0.720D3 - (0.90D2 * t5 * (t356 - t382 * t359) - 0.180D3 * t2
     #8 * t371) * t43 * t312 / 0.720D3
      t394 = FJET(XB1, XB2, s, t344, -t345, -t347, t349, 0.0D0, t393)
      t396 = FJET(XB1, XB2, s, t349, -t347, -t345, t344, 0.0D0, t393)
      t398 = -0.1D1 + t93
      t399 = 0.1D1 / t398
      t401 = t2 * t341 * t399
      t403 = x3 * t86 * t399
      t404 = t2 * t403
      t406 = t398 ** 2
      t407 = 0.1D1 / t406
      t409 = x3 * t341 * t407 * t87
      t412 = log(-0.4D1 * t194 * t409)
      t413 = t412 * z
      t414 = cos(t11)
      t415 = x3 * z
      t416 = x2 * t341
      t418 = Sqrt(-t415 * t416)
      t422 = 0.1D1 / (-z - t93 + 0.2D1 * t414 * t418)
      t423 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t401, t404, 0.0D0)
      t426 = t412 ** 2
      t428 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t401, t404, 0.0D0)
      t429 = t422 * t428
      t432 = z * t422
      t433 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t401, t404, 0.0D0)
      t438 = t432 * t423
      t445 = t432 * t428
      t453 = log(-0.4D1 * t113 * t409)
      t459 = t28 * t4
      t466 = (0.90D2 * t5 * (-t413 * t422 * t423 + t426 * z * t429 / 0.2
     #D1 + t432 * t433) - 0.180D3 * t28 * t4 * (t438 - t413 * t429) + t3
     #9 * t4 * t445) * t43 * t101 / 0.1440D4 - (0.90D2 * t5 * (-t438 + t
     #453 * z * t429) + 0.180D3 * t459 * t445) * t43 * t312 / 0.720D3
      t467 = FJET(XB1, XB2, s, 0.0D0, t401, 0.0D0, t404, 0.0D0, t466)
      t469 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t272)
      t471 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t272)
      t473 = t2 * x1
      t474 = t295 * t296
      t477 = log(-0.4D1 * t350 * t474)
      t478 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t473, 0.0D0
     #, -t285, 0.0D0, 0.0D0)
      t480 = t477 ** 2
      t481 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t473, 0.0D0
     #, -t285, 0.0D0, 0.0D0)
      t484 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t473, 0.0D0
     #, -t285, 0.0D0, 0.0D0)
      t493 = t4 * t481
      t494 = t39 * t493
      t500 = log(-0.4D1 * t48 * t474)
      t505 = t500 ** 2
      t508 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, t473, 0.0D0
     #, -t285, 0.0D0, 0.0D0)
      t527 = t278 * t296
      t531 = log(-0.4D1 * t85 * t13 * t294 * t527)
      t543 = log(-0.4D1 * t314 * t474)
      t545 = t543 ** 2
      t560 = (0.90D2 * t5 * (-t477 * t478 + t480 * t481 / 0.2D1 + t484) 
     #- 0.180D3 * t28 * t4 * (t478 - t477 * t481) + t494) * t43 * t45 / 
     #0.720D3 + (t39 * t4 * (t478 - t500 * t481) + 0.90D2 * t5 * (t505 *
     # t478 / 0.2D1 + t508 - t505 * t500 * t481 / 0.6D1 - t500 * t484) +
     # t73 * t493 - 0.180D3 * t28 * t4 * (-t500 * t478 + t505 * t481 / 0
     #.2D1 + t484)) * t45 / 0.720D3 - (0.90D2 * t5 * (-t478 + t531 * t48
     #1) + 0.180D3 * t28 * t493) * t43 * t312 / 0.720D3 - (0.90D2 * t5 *
     # (t543 * t478 - t545 * t481 / 0.2D1 - t484) - 0.180D3 * t28 * t4 *
     # (-t478 + t543 * t481) - t494) * t45 * t101 / 0.720D3
      t561 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t285, t473, 0.0D0, t560)
      t563 = FJET(XB1, XB2, s, 0.0D0, t404, 0.0D0, t401, 0.0D0, t466)
      t565 = FJET(XB1, XB2, s, -t345, t344, t349, -t347, 0.0D0, t393)
      t567 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t272)
      t569 = t2 * x3
      t570 = t2 * t341
      t571 = t14 * t341
      t574 = log(-0.4D1 * t7 * t571)
      t575 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t570, t569, 0.0D0)
      t577 = t574 ** 2
      t578 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t570, t569, 0.0D0)
      t581 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t570, t569, 0.0D0)
      t590 = t4 * t578
      t591 = t39 * t590
      t598 = log(-0.4D1 * t85 * t571)
      t612 = log(-0.4D1 * t133 * t13 * t341)
      t617 = t612 ** 2
      t620 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t570, t569, 0.0D0)
      t641 = log(-0.4D1 * t93 * t571)
      t643 = t641 ** 2
      t658 = (0.90D2 * t5 * (-t574 * t575 + t577 * t578 / 0.2D1 + t581) 
     #- 0.180D3 * t28 * t4 * (t575 - t574 * t578) + t591) * t43 * t45 / 
     #0.720D3 - (0.90D2 * t5 * (-t575 + t598 * t578) + 0.180D3 * t28 * t
     #590) * t43 * t312 / 0.720D3 + (t39 * t4 * (t575 - t612 * t578) + 0
     #.90D2 * t5 * (t617 * t575 / 0.2D1 + t620 - t617 * t612 * t578 / 0.
     #6D1 - t612 * t581) + t73 * t590 - 0.180D3 * t28 * t4 * (-t612 * t5
     #75 + t617 * t578 / 0.2D1 + t581)) * t43 / 0.1440D4 + (0.90D2 * t5 
     #* (-t641 * t575 + t643 * t578 / 0.2D1 + t581) - 0.180D3 * t28 * t4
     # * (t575 - t641 * t578) + t591) * t43 * t101 / 0.1440D4
      t659 = FJET(XB1, XB2, s, 0.0D0, t569, 0.0D0, -t570, 0.0D0, t658)
      t661 = FJET(XB1, XB2, s, -t285, -t283, 0.0D0, -t280, t291, t336)
      t663 = t273 * t272 + t337 * t336 + t339 * t336 + t394 * t393 + t39
     #6 * t393 + t467 * t466 + t469 * t272 + t471 * t272 + t561 * t560 +
     # t563 * t466 + t565 * t393 + t567 * t272 + t659 * t658 + t661 * t3
     #36
      t664 = FJET(XB1, XB2, s, -t347, t349, t344, -t345, 0.0D0, t393)
      t666 = FJET(XB1, XB2, s, t404, 0.0D0, t401, 0.0D0, 0.0D0, t466)
      t668 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t473, -t285, 0.0D0, t560)
      t670 = FJET(XB1, XB2, s, t569, 0.0D0, -t570, 0.0D0, 0.0D0, t658)
      t672 = FJET(XB1, XB2, s, -t285, t473, 0.0D0, 0.0D0, 0.0D0, t560)
      t674 = t285 * t403
      t677 = Sqrt(x3 * t277 * t416)
      t678 = t414 * t677
      t679 = 0.2D1 * t678
      t680 = t348 * z
      t681 = t93 * z
      t683 = t348 * x2
      t685 = x2 * z
      t688 = x2 ** 2
      t690 = t276 * t688 * x3
      t691 = t678 * x2
      t696 = x1 * t688 * x3
      t697 = -x2 - t679 + t680 + 0.2D1 * t681 + 0.2D1 * t683 + t93 - 0.2
     #D1 * t348 * t685 + t690 + 0.2D1 * t691 - z * t688 * x3 - t696 - t4
     #15 - t348
      t700 = t473 * t697 * t278 * t399
      t702 = t342 * t343 * t399
      t707 = t473 * t86 * (-t93 - z + t415 - x1 + t348 + t276 - t680 + t
     #679) * t278 * t399
      t708 = t275 * z
      t709 = z + x1 - t276 - t275 + t708
      t710 = t277 * t709
      t714 = t6 * t8
      t717 = x1 * t414
      t723 = t105 - 0.2D1 * t105 * z - t275 * t8 + t714 * x2 - t8 + t708
     # - t681 - t683 - 0.2D1 * t276 - 0.2D1 * t717 * t677 * x2 - 0.2D1 *
     # t276 * t678 - t6
      t740 = t348 * x2 * t8 + 0.2D1 * t7 * t685 - t714 * t93 - t690 + 0.
     #2D1 * z * t414 * t677 + 0.2D1 * t717 * t677 + 0.2D1 * x1 * t8 + 0.
     #2D1 * t6 * z - t714 - t85 + t696 + 0.2D1 * t276 * t691
      t742 = 0.1D1 / (t723 + t740)
      t743 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t707, -t700
     #, -t702, -t674, t291)
      t753 = log(0.4D1 * t93 * t48 * t294 * t527 * t87 * t341 * t407)
      t756 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t707, -t700
     #, -t702, -t674, t291)
      t759 = t710 * t742 * t743 - t753 * t277 * t709 * t742 * t756
      t765 = 0.180D3 * t459 * t710 * t742 * t756
      t766 = -0.90D2 * t5 * t759 + t765
      t769 = t766 * t43 * t312 / 0.720D3
      t770 = FJET(XB1, XB2, s, -t674, -t700, -t702, t707, t291, -t769)
      t774 = FJET(XB1, XB2, s, 0.0D0, -t570, 0.0D0, t569, 0.0D0, t658)
      t776 = FJET(XB1, XB2, s, -t570, 0.0D0, t569, 0.0D0, 0.0D0, t658)
      t778 = FJET(XB1, XB2, s, t473, -t285, 0.0D0, 0.0D0, 0.0D0, t560)
      t780 = FJET(XB1, XB2, s, t401, 0.0D0, t404, 0.0D0, 0.0D0, t466)
      t782 = FJET(XB1, XB2, s, -t283, -t285, -t280, 0.0D0, t291, t336)
      t787 = -0.90D2 * t5 * t759 + t765
      t791 = FJET(XB1, XB2, s, t707, -t702, -t700, -t674, t291, -t787 * 
     #t43 * t312 / 0.720D3)
      t795 = FJET(XB1, XB2, s, -t700, -t674, t707, -t702, t291, -t769)
      t799 = FJET(XB1, XB2, s, -t702, t707, -t674, -t700, t291, -t769)
      t803 = t664 * t393 + t666 * t466 + t668 * t560 + t670 * t658 + t67
     #2 * t560 - t770 * t766 * t102 / 0.720D3 + t774 * t658 + t776 * t65
     #8 + t778 * t560 + t780 * t466 + t782 * t336 - t791 * t787 * t102 /
     # 0.720D3 - t795 * t766 * t102 / 0.720D3 - t799 * t766 * t102 / 0.7
     #20D3
      rrgg2gghhardt8s4e1 = t663 + t803

      end function



      doubleprecision function rrgg2gghhardt8s4e0
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t
     #4, 0.0D0, 0.0D0)
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t16 = z ** 2
      t18 = x1 * z
      t19 = -z - x1 + t18
      t20 = 0.1D1 / t19
      t21 = 0.1D1 / t16 * t20
      t22 = t3 ** 2
      t23 = t21 * t22
      t26 = log(-0.4D1 * t15 * t23)
      t27 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -
     #t4, 0.0D0, 0.0D0)
      t32 = pi * lh
      t33 = t7 * t27
      t35 = 0.180D3 * t32 * t33
      t37 = 0.1D1 / x3
      t39 = 0.1D1 / x1
      t44 = 0.1D1 / x2
      t45 = t37 * t39 * t44
      t48 = x2 * t10
      t49 = t48 * t14
      t52 = log(-0.4D1 * t49 * t23)
      t61 = t10 * t14
      t64 = log(-0.4D1 * t61 * t23)
      t66 = t64 ** 2
      t69 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -
     #t4, 0.0D0, 0.0D0)
      t78 = pi ** 2
      t79 = 0.30D2 * t78
      t80 = lh ** 2
      t81 = 0.180D3 * t80
      t82 = -t79 + t81
      t83 = pi * t82
      t88 = (0.90D2 * t8 * (t9 - t26 * t27) - t35) * t37 * t39 / 0.720D3
     # + t8 * t27 * t45 / 0.8D1 - (0.90D2 * t8 * (-t9 + t52 * t27) + t35
     #) * t39 * t44 / 0.720D3 + (0.90D2 * t8 * (-t64 * t9 + t66 * t27 / 
     #0.2D1 + t69) - 0.180D3 * t32 * t7 * (t9 - t64 * t27) + t83 * t33) 
     #* t39 / 0.720D3
      t89 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t88)
      t92 = t1 * t3
      t93 = x3 * s * t92
      t94 = x3 * x1
      t95 = t2 * t94
      t96 = -0.1D1 + x3
      t97 = t96 * s
      t98 = t97 * t92
      t99 = t1 * x1
      t100 = t97 * t99
      t101 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t100, t95,
     # t98, -t93, 0.0D0)
      t106 = log(0.4D1 * t15 * t21 * t22 * t96)
      t107 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t100, t95,
     # t98, -t93, 0.0D0)
      t122 = (0.90D2 * t8 * (-t101 + t106 * t107) + 0.180D3 * t32 * t7 *
     # t107) * t37 * t39 / 0.720D3 - t8 * t107 * t45 / 0.8D1
      t123 = FJET(XB1, XB2, s, -t93, t95, t98, -t100, 0.0D0, t122)
      t125 = cos(t12)
      t127 = x2 * t96
      t129 = Sqrt(x3 * t19 * t127)
      t130 = t125 * t129
      t131 = 0.2D1 * t130
      t132 = t94 * z
      t133 = x2 * x3
      t134 = t133 * z
      t136 = t94 * x2
      t138 = x2 * z
      t141 = x2 ** 2
      t143 = t18 * t141 * x3
      t144 = t130 * x2
      t149 = x1 * t141 * x3
      t150 = x3 * z
      t151 = -x2 - t131 + t132 + 0.2D1 * t134 + 0.2D1 * t136 + t133 - 0.
     #2D1 * t94 * t138 + t143 + 0.2D1 * t144 - z * t141 * x3 - t149 - t1
     #50 - t94
      t153 = -0.1D1 + t133
      t154 = 0.1D1 / t153
      t156 = t5 * t151 * t20 * t154
      t157 = -0.1D1 + x2
      t159 = x3 * t157 * t154
      t160 = t4 * t159
      t165 = t5 * t157 * (-t133 - z + t150 - x1 + t94 + t18 - t132 + t13
     #1) * t20 * t154
      t167 = t97 * t92 * t154
      t168 = t1 ** 2
      t173 = s * t168 * x2 * x1 * t3 * t20
      t174 = x1 * x2
      t175 = t174 * z
      t176 = z + x1 - t18 - t174 + t175
      t182 = t10 * t16
      t185 = x1 * t125
      t191 = t48 - 0.2D1 * t48 * z - t174 * t16 + t182 * x2 - t16 + t175
     # - t134 - t136 - 0.2D1 * t18 - 0.2D1 * t185 * t129 * x2 - 0.2D1 * 
     #t18 * t130 - t10
      t209 = t94 * x2 * t16 + 0.2D1 * t11 * t138 - t182 * t133 - t143 + 
     #0.2D1 * z * t125 * t129 + 0.2D1 * t185 * t129 + 0.2D1 * x1 * t16 +
     # 0.2D1 * t10 * z - t182 - t11 * x2 + t149 + 0.2D1 * t18 * t144
      t212 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t165, -t156
     #, -t167, -t160, t173)
      t214 = 0.1D1 / (t191 + t209) * t212 * t45
      t216 = t8 * t19 * t176 * t214 / 0.8D1
      t217 = FJET(XB1, XB2, s, -t156, -t160, t165, -t167, t173, t216)
      t220 = t7 * t19 * t176
      t225 = t157 * s * t99
      t227 = t2 * t174 * t20
      t228 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t225, -t22
     #7, -t4, 0.0D0, t173)
      t232 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, -t225, -t22
     #7, -t4, 0.0D0, t173)
      t233 = t157 ** 2
      t238 = log(-0.4D1 * t49 * t21 * t22 * t233)
      t250 = -t8 * t228 * t45 / 0.8D1 - (0.90D2 * t8 * (t232 - t238 * t2
     #28) - 0.180D3 * t32 * t7 * t228) * t39 * t44 / 0.720D3
      t251 = FJET(XB1, XB2, s, -t4, -t225, 0.0D0, -t227, t173, t250)
      t253 = FJET(XB1, XB2, s, -t100, t98, t95, -t93, 0.0D0, t122)
      t255 = t2 * t159
      t257 = t2 * t96 * t154
      t259 = Sqrt(-t150 * t127)
      t263 = 0.1D1 / (-z - t133 + 0.2D1 * t125 * t259)
      t264 = z * t263
      t265 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t257, t255, 0.0D0)
      t268 = 0.1D1 / t16 / z
      t269 = x2 * t268
      t270 = t269 * t14
      t272 = t153 ** 2
      t278 = log(-0.4D1 * t270 * x3 * t96 / t272 * t233)
      t280 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t257, t255, 0.0D0)
      t300 = (0.90D2 * t8 * (t264 * t265 - t278 * z * t263 * t280) - 0.1
     #80D3 * t32 * t7 * t264 * t280) * t37 * t44 / 0.1440D4 + t8 * t264 
     #* t280 * t37 * t39 * t44 / 0.8D1
      t301 = FJET(XB1, XB2, s, t255, 0.0D0, t257, 0.0D0, 0.0D0, t300)
      t303 = FJET(XB1, XB2, s, t95, -t93, -t100, t98, 0.0D0, t122)
      t305 = t2 * t96
      t306 = t2 * x3
      t307 = x3 * t268
      t311 = log(-0.4D1 * t307 * t14 * t96)
      t312 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t305, t306, 0.0D0)
      t314 = t311 ** 2
      t315 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t305, t306, 0.0D0)
      t318 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t305, t306, 0.0D0)
      t327 = t7 * t315
      t332 = t14 * t268
      t333 = t332 * t96
      t336 = log(-0.4D1 * t11 * t333)
      t342 = 0.180D3 * t32 * t327
      t352 = log(-0.4D1 * t133 * t333)
      t361 = (0.90D2 * t8 * (-t311 * t312 + t314 * t315 / 0.2D1 + t318) 
     #- 0.180D3 * t32 * t7 * (t312 - t311 * t315) + t83 * t327) * t37 / 
     #0.1440D4 + (0.90D2 * t8 * (t312 - t336 * t315) - t342) * t37 * t39
     # / 0.720D3 + t8 * t315 * t45 / 0.8D1 + (0.90D2 * t8 * (t312 - t352
     # * t315) - t342) * t37 * t44 / 0.1440D4
      t362 = FJET(XB1, XB2, s, 0.0D0, -t305, 0.0D0, t306, 0.0D0, t361)
      t364 = FJET(XB1, XB2, s, 0.0D0, -t227, -t4, -t225, t173, t250)
      t366 = FJET(XB1, XB2, s, -t305, 0.0D0, t306, 0.0D0, 0.0D0, t361)
      t368 = FJET(XB1, XB2, s, 0.0D0, t257, 0.0D0, t255, 0.0D0, t300)
      t370 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t373 = log(0.4D1 * t11 * t332)
      t374 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t379 = t7 * t374
      t381 = 0.180D3 * t32 * t379
      t386 = t332 * t233
      t389 = log(0.4D1 * t48 * t386)
      t393 = log(0.4D1 * t48 * t332)
      t402 = log(0.4D1 * t61 * t268)
      t404 = t402 ** 2
      t407 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t416 = t83 * t379
      t422 = log(0.4D1 * t307 * t14)
      t424 = t422 ** 2
      t439 = log(0.4D1 * t332)
      t442 = t439 ** 2
      t449 = rrgg2gghhard81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t2, 0.0D0, 0.0D0)
      t475 = log(0.4D1 * t133 * t332)
      t479 = log(0.4D1 * t133 * t386)
      t487 = log(0.4D1 * t270)
      t488 = t487 ** 2
      t492 = log(0.4D1 * t269 * t14 * t233)
      t493 = t492 ** 2
      t507 = (0.90D2 * t8 * (-t370 + t373 * t374) + t381) * t37 * t39 / 
     #0.720D3 - t8 * (t389 * t374 - t393 * t374) * t39 * t44 / 0.8D1 + (
     #0.90D2 * t8 * (t402 * t370 - t404 * t374 / 0.2D1 - t407) - 0.180D3
     # * t32 * t7 * (-t370 + t402 * t374) - t416) * t39 / 0.720D3 + (0.9
     #0D2 * t8 * (t422 * t370 - t424 * t374 / 0.2D1 - t407) - 0.180D3 * 
     #t32 * t7 * (-t370 + t422 * t374) - t416) * t37 / 0.1440D4 - (0.180
     #D3 * t439 * lh + 0.45D2 * t442 - t79 + t81) * pi * t7 * t370 / 0.1
     #440D4 - t8 * t449 / 0.16D2 - (-0.90D2 * t442 * lh - 0.240D3 * zeta
     #3 - 0.120D3 * t80 * lh + 0.60D2 * lh * t78 - 0.15D2 * t442 * t439 
     #- t439 * t82) * pi * t379 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t
     #439) * pi * t7 * t407 / 0.1440D4 + t8 * (t475 * t374 - t479 * t374
     #) * t37 * t44 / 0.16D2 - (0.90D2 * t8 * t374 * (t488 / 0.2D1 - t49
     #3 / 0.2D1) + (0.90D2 * t8 * t370 - t381) * (-t487 + t492)) * t44 /
     # 0.1440D4
      t508 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t507)
      t510 = FJET(XB1, XB2, s, t98, -t100, -t93, t95, 0.0D0, t122)
      t512 = FJET(XB1, XB2, s, t257, 0.0D0, t255, 0.0D0, 0.0D0, t300)
      t514 = t89 * t88 + t123 * t122 + t217 * pi * t220 * t214 / 0.8D1 +
     # t251 * t250 + t253 * t122 + t301 * t300 + t303 * t122 + t362 * t3
     #61 + t364 * t250 + t366 * t361 + t368 * t300 + t508 * t507 + t510 
     #* t122 + t512 * t300
      t515 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t88)
      t517 = FJET(XB1, XB2, s, t306, 0.0D0, -t305, 0.0D0, 0.0D0, t361)
      t519 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t88)
      t521 = FJET(XB1, XB2, s, t165, -t167, -t156, -t160, t173, t216)
      t526 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t88)
      t528 = FJET(XB1, XB2, s, -t225, -t4, -t227, 0.0D0, t173, t250)
      t530 = FJET(XB1, XB2, s, -t227, 0.0D0, -t225, -t4, t173, t250)
      t532 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t507)
      t534 = FJET(XB1, XB2, s, 0.0D0, t255, 0.0D0, t257, 0.0D0, t300)
      t536 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t507)
      t538 = FJET(XB1, XB2, s, 0.0D0, t306, 0.0D0, -t305, 0.0D0, t361)
      t540 = FJET(XB1, XB2, s, -t160, -t156, -t167, t165, t173, t216)
      t545 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t507)
      t547 = FJET(XB1, XB2, s, -t167, t165, -t160, -t156, t173, t216)
      t552 = t515 * t88 + t517 * t361 + t519 * t88 + t521 * pi * t220 * 
     #t214 / 0.8D1 + t526 * t88 + t528 * t250 + t530 * t250 + t532 * t50
     #7 + t534 * t300 + t536 * t507 + t538 * t361 + t540 * pi * t220 * t
     #214 / 0.8D1 + t545 * t507 + t547 * pi * t220 * t214 / 0.8D1
      rrgg2gghhardt8s4e0 = t514 + t552

      end function



      doubleprecision function rrgg2gghhardt8s4em1
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t22 = pi * lh
      t23 = t4 * t17
      t25 = 0.180D3 * t22 * t23
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t31 = t30 * t13
      t34 = log(0.4D1 * t31 * t9)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t50 = log(0.4D1 * t47 * t13)
      t51 = -0.1D1 + x2
      t52 = t51 ** 2
      t56 = log(0.4D1 * t47 * t13 * t52)
      t59 = 0.1D1 / x2
      t66 = log(0.4D1 * t9 * t13)
      t75 = t66 ** 2
      t77 = pi ** 2
      t79 = lh ** 2
      t85 = rrgg2gghhard81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t88 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 + (
     #0.90D2 * t5 * (-t6 + t34 * t17) + t25) * t40 / 0.720D3 - t5 * t17 
     #* t27 * t40 / 0.8D1 - t5 * t17 * (-t50 + t56) * t59 / 0.16D2 - (-0
     #.180D3 * lh - 0.90D2 * t66) * pi * t4 * t6 / 0.1440D4 - (0.180D3 *
     # t66 * lh + 0.45D2 * t75 - 0.30D2 * t77 + 0.180D3 * t79) * pi * t2
     #3 / 0.1440D4 - t5 * t85 / 0.16D2
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t88)
      t91 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t88)
      t93 = t2 * x1
      t94 = -0.1D1 + x1
      t95 = t2 * t94
      t96 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t93, 0.0D0, 
     #-t95, 0.0D0, 0.0D0)
      t101 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, t93, 0.0D0,
     # -t95, 0.0D0, 0.0D0)
      t105 = 0.1D1 / (-z - x1 + x1 * z)
      t107 = t94 ** 2
      t111 = log(-0.4D1 * t31 / t7 * t105 * t107)
      t126 = t5 * t96 * t40 * t59 / 0.8D1 + (0.90D2 * t5 * (t101 - t111 
     #* t96) - 0.180D3 * t22 * t4 * t96) * t40 / 0.720D3 + t5 * t96 * t2
     #7 * t40 / 0.8D1
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t93, -t95, 0.0D0, t126)
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t95, t93, 0.0D0, t126)
      t131 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t88)
      t133 = t2 * x3
      t134 = -0.1D1 + x3
      t135 = t2 * t134
      t136 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t135, t133, 0.0D0)
      t140 = log(-0.4D1 * t10 * t13 * t134)
      t141 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, -t135, t133, 0.0D0)
      t152 = t141 * t27
      t159 = (0.90D2 * t5 * (t136 - t140 * t141) - 0.180D3 * t22 * t4 * 
     #t141) * t27 / 0.1440D4 + t5 * t152 * t40 / 0.8D1 + t5 * t152 * t59
     # / 0.16D2
      t160 = FJET(XB1, XB2, s, 0.0D0, t133, 0.0D0, -t135, 0.0D0, t159)
      t162 = x2 * x3
      t164 = 0.1D1 / (-0.1D1 + t162)
      t166 = t2 * t134 * t164
      t169 = t2 * x3 * t51 * t164
      t171 = cos(t11)
      t175 = Sqrt(-x3 * z * x2 * t134)
      t180 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t166, t169, 0.0D0)
      t183 = 0.1D1 / (-z - t162 + 0.2D1 * t171 * t175) * t180 * t27 * t5
     #9
      t185 = t5 * z * t183 / 0.16D2
      t186 = FJET(XB1, XB2, s, 0.0D0, t166, 0.0D0, t169, 0.0D0, t185)
      t188 = t4 * z
      t192 = FJET(XB1, XB2, s, 0.0D0, t169, 0.0D0, t166, 0.0D0, t185)
      t197 = FJET(XB1, XB2, s, 0.0D0, -t135, 0.0D0, t133, 0.0D0, t159)
      t201 = t2 * x1 * x2 * t105
      t203 = t1 * x1
      t204 = t51 * s * t203
      t205 = t1 ** 2
      t210 = s * t205 * x2 * x1 * t94 * t105
      t211 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t204, -t20
     #1, -t95, 0.0D0, t210)
      t213 = t211 * t40 * t59
      t215 = t5 * t213 / 0.8D1
      t216 = FJET(XB1, XB2, s, 0.0D0, -t201, -t95, -t204, t210, -t215)
      t221 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t88)
      t223 = FJET(XB1, XB2, s, t93, -t95, 0.0D0, 0.0D0, 0.0D0, t126)
      t225 = t89 * t88 + t91 * t88 + t127 * t126 + t129 * t126 + t131 * 
     #t88 + t160 * t159 + t186 * pi * t188 * t183 / 0.16D2 + t192 * pi *
     # t188 * t183 / 0.16D2 + t197 * t159 - t216 * pi * t4 * t213 / 0.8D
     #1 + t221 * t88 + t223 * t126
      t226 = FJET(XB1, XB2, s, t133, 0.0D0, -t135, 0.0D0, 0.0D0, t159)
      t229 = t2 * x1 * x3
      t231 = t1 * t94
      t232 = x3 * s * t231
      t233 = t134 * s
      t234 = t233 * t203
      t235 = t233 * t231
      t236 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, -t234, t229
     #, t235, -t232, 0.0D0)
      t238 = t236 * t27 * t40
      t240 = t5 * t238 / 0.8D1
      t241 = FJET(XB1, XB2, s, t229, -t232, -t234, t235, 0.0D0, -t240)
      t246 = FJET(XB1, XB2, s, t166, 0.0D0, t169, 0.0D0, 0.0D0, t185)
      t251 = FJET(XB1, XB2, s, t235, -t234, -t232, t229, 0.0D0, -t240)
      t256 = FJET(XB1, XB2, s, t169, 0.0D0, t166, 0.0D0, 0.0D0, t185)
      t261 = FJET(XB1, XB2, s, -t95, t93, 0.0D0, 0.0D0, 0.0D0, t126)
      t263 = FJET(XB1, XB2, s, -t95, -t204, 0.0D0, -t201, t210, -t215)
      t268 = FJET(XB1, XB2, s, -t135, 0.0D0, t133, 0.0D0, 0.0D0, t159)
      t270 = FJET(XB1, XB2, s, -t232, t229, t235, -t234, 0.0D0, -t240)
      t275 = FJET(XB1, XB2, s, -t204, -t95, -t201, 0.0D0, t210, -t215)
      t280 = FJET(XB1, XB2, s, -t234, t235, t229, -t232, 0.0D0, -t240)
      t285 = FJET(XB1, XB2, s, -t201, 0.0D0, -t204, -t95, t210, -t215)
      t290 = t226 * t159 - t241 * pi * t4 * t238 / 0.8D1 + t246 * pi * t
     #188 * t183 / 0.16D2 - t251 * pi * t4 * t238 / 0.8D1 + t256 * pi * 
     #t188 * t183 / 0.16D2 + t261 * t126 - t263 * pi * t4 * t213 / 0.8D1
     # + t268 * t159 - t270 * pi * t4 * t238 / 0.8D1 - t275 * pi * t4 * 
     #t213 / 0.8D1 - t280 * pi * t4 * t238 / 0.8D1 - t285 * pi * t4 * t2
     #13 / 0.8D1
      rrgg2gghhardt8s4em1 = t225 + t290

      end function



      doubleprecision function rrgg2gghhardt8s4em2
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t15 = rrgg2gghhard81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
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
      t42 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, t39, 0.0D0, 
     #-t41, 0.0D0, 0.0D0)
      t45 = t5 * t42 * t7 / 0.8D1
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t39, -t41, 0.0D0, t45)
      t49 = t4 * t42 * t7
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t41, t39, 0.0D0, t45)
      t56 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t34)
      t58 = t2 * x3
      t60 = t2 * (-0.1D1 + x3)
      t61 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t60, t58, 0.0D0)
      t64 = t5 * t61 * t11 / 0.16D2
      t65 = FJET(XB1, XB2, s, 0.0D0, t58, 0.0D0, -t60, 0.0D0, t64)
      t68 = t4 * t61 * t11
      t71 = FJET(XB1, XB2, s, 0.0D0, -t60, 0.0D0, t58, 0.0D0, t64)
      t75 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t34)
      t77 = FJET(XB1, XB2, s, t39, -t41, 0.0D0, 0.0D0, 0.0D0, t45)
      t81 = FJET(XB1, XB2, s, t58, 0.0D0, -t60, 0.0D0, 0.0D0, t64)
      t85 = FJET(XB1, XB2, s, -t41, t39, 0.0D0, 0.0D0, 0.0D0, t45)
      t89 = FJET(XB1, XB2, s, -t60, 0.0D0, t58, 0.0D0, 0.0D0, t64)
      rrgg2gghhardt8s4em2 = t35 * t34 + t37 * t34 + t46 * pi * t49 / 0.8
     #D1 + t52 * pi * t49 / 0.8D1 + t56 * t34 + t65 * pi * t68 / 0.16D2 
     #+ t71 * pi * t68 / 0.16D2 + t75 * t34 + t77 * pi * t49 / 0.8D1 + t
     #81 * pi * t68 / 0.16D2 + t85 * pi * t49 / 0.8D1 + t89 * pi * t68 /
     # 0.16D2

      end function



      doubleprecision function rrgg2gghhardt8s4em3
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2gghhard81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gghhardt8s4em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0
     #.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt8s4em4
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
      doubleprecision rrgg2gghhard81J1
      doubleprecision rrgg2gghhard81J2
      doubleprecision rrgg2gghhard81J3
      doubleprecision rrgg2gghhard81J4
      doubleprecision rrgg2gghhard81J5
      doubleprecision rrgg2gghhard81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2gghhard81J1
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t6 = 0.1D1 / (S12 + S13 + S23)
      t13 = S13 * S23
      t15 = S13 ** 2
      t17 = S13 * S14
      t21 = S14 * S24
      t23 = S23 * S24
      t25 = S24 ** 2
      t29 = S14 * S23
      t31 = S23 ** 2
      t33 = S14 ** 2
      t35 = t15 * S14
      t37 = S13 * t33
      t39 = t15 * S23
      t40 = 0.9D1 * t39
      t41 = S13 * t31
      t42 = 0.18D2 * t41
      t43 = t17 * S23
      t45 = t15 * S13
      t46 = 0.9D1 * t45
      t49 = t25 * S24
      t50 = 0.9D1 * t49
      t51 = t33 * S24
      t52 = 0.18D2 * t51
      t53 = S14 * t25
      t54 = 0.9D1 * t53
      t55 = t29 * S24
      t57 = t25 * S23
      t59 = t31 * S24
      t64 = 0.1D1 / S12
      t66 = 0.18D2 * t37
      t68 = 0.18D2 * t59
      t72 = t46 + t52 + t54 + t42 + t40 + t66 + 0.9D1 * t57 + t68 + 0.9D
     #1 * t35 + t50 + 0.36D2 * t55 + 0.36D2 * t43
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t98 = 0.18D2 * t49 + 0.90D2 * t43 + 0.72D2 * t57 + t68 + 0.90D2 * 
     #t55 + 0.9D1 * t51 + 0.72D2 * t35 + 0.18D2 * t45 + 0.63D2 * t53 + 0
     #.9D1 * t41 + t66 + 0.63D2 * t39
      t108 = t31 ** 2
      t110 = t33 ** 2
      rrgg2gghhard81J1 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0
     #.2D1 * S24 * t6) * S12 + 0.9D1 * S23 + 0.9D1 * S14 + (0.27D2 / 0.2
     #D1 * t13 - 0.9D1 / 0.2D1 * t15 - 0.9D1 * t17) * t2 + (0.27D2 / 0.2
     #D1 * t21 - 0.9D1 * t23 - 0.9D1 / 0.2D1 * t25) * t6 + (0.36D2 * t29
     # + 0.18D2 * t31 + 0.18D2 * t33 + (0.9D1 / 0.2D1 * t35 + 0.9D1 / 0.
     #2D1 * t37 + t40 + t42 - 0.27D2 / 0.2D1 * t43 + t46) * t2 + (t50 + 
     #t52 + t54 - 0.27D2 / 0.2D1 * t55 + 0.9D1 / 0.2D1 * t57 + 0.9D1 / 0
     #.2D1 * t59) * t6) * t64 + t72 * t74) * s * z + (0.18D2 * S24 + 0.1
     #8D2 * S13) * S12 + 0.54D2 * t25 + 0.54D2 * t15 + 0.72D2 * t23 + 0.
     #63D2 * t13 + 0.63D2 * t21 + 0.72D2 * t17 + t98 * t64 + (0.18D2 * t
     #31 * S23 * S14 + 0.27D2 * t31 * t33 + 0.18D2 * S23 * t33 * S14 + 0
     #.9D1 * t108 + 0.9D1 * t110) * t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard81J2
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t6 = 0.1D1 / (S12 + S13 + S23)
      t13 = S13 * S14
      t15 = S13 ** 2
      t19 = S23 * S24
      t21 = S24 ** 2
      t25 = S14 ** 2
      t27 = S14 * S23
      t29 = S23 ** 2
      t31 = t15 * S23
      t32 = 0.9D1 * t31
      t33 = t15 * S14
      t35 = t15 * S13
      t36 = 0.9D1 * t35
      t38 = 0.18D2 * S13 * t29
      t39 = S13 * t25
      t44 = 0.18D2 * t25 * S24
      t45 = S14 * t21
      t46 = 0.9D1 * t45
      t47 = t21 * S23
      t49 = t29 * S24
      t51 = t21 * S24
      t52 = 0.9D1 * t51
      t56 = 0.1D1 / S12
      t59 = 0.18D2 * t39
      t60 = t27 * S24
      t62 = 0.18D2 * t49
      t63 = t13 * S23
      t66 = t44 + t46 + t38 + t32 + 0.9D1 * t33 + t59 + 0.9D1 * t60 + t6
     #2 + 0.9D1 * t63 + 0.9D1 * t47 + t36 + t52
      t67 = S12 ** 2
      t68 = 0.1D1 / t67
      t102 = t29 ** 2
      t104 = t25 ** 2
      rrgg2gghhard81J2 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0
     #.2D1 * S24 * t6) * S12 + 0.9D1 * S23 + 0.9D1 * S14 + (-0.9D1 * t13
     # - 0.9D1 / 0.2D1 * t15) * t2 + (-0.9D1 * t19 - 0.9D1 / 0.2D1 * t21
     #) * t6 + (0.18D2 * t25 + 0.9D1 * t27 + 0.18D2 * t29 + (t32 + 0.9D1
     # / 0.2D1 * t33 + t36 + t38 + 0.9D1 / 0.2D1 * t39) * t2 + (t44 + t4
     #6 + 0.9D1 / 0.2D1 * t47 + 0.9D1 / 0.2D1 * t49 + t52) * t6) * t56 +
     # t66 * t68) * s * z + (0.18D2 * S24 + 0.18D2 * S13) * S12 + 0.54D2
     # * S14 * S24 + 0.54D2 * t21 + 0.54D2 * S13 * S23 + 0.72D2 * t13 + 
     #0.72D2 * t19 + 0.54D2 * t15 + (t62 + 0.18D2 * t35 + 0.18D2 * t51 +
     # t59 + 0.72D2 * t33 + 0.72D2 * t47 + 0.81D2 * t63 + 0.54D2 * t45 +
     # 0.54D2 * t31 + 0.81D2 * t60) * t56 + (0.18D2 * t29 * S23 * S14 + 
     #0.27D2 * t29 * t25 + 0.18D2 * S23 * t25 * S14 + 0.9D1 * t102 + 0.9
     #D1 * t104) * t68) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard81J3
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t6 = 0.1D1 / (S12 + S13 + S23)
      t13 = S13 * S23
      t15 = S13 ** 2
      t17 = S13 * S14
      t21 = S14 * S24
      t23 = S23 * S24
      t25 = S24 ** 2
      t29 = S14 * S23
      t31 = S23 ** 2
      t33 = S14 ** 2
      t35 = t15 * S14
      t37 = S13 * t33
      t39 = t15 * S23
      t40 = 0.9D1 * t39
      t41 = S13 * t31
      t42 = 0.18D2 * t41
      t43 = t17 * S23
      t45 = t15 * S13
      t46 = 0.9D1 * t45
      t49 = t25 * S24
      t50 = 0.9D1 * t49
      t51 = t33 * S24
      t52 = 0.18D2 * t51
      t53 = S14 * t25
      t54 = 0.9D1 * t53
      t55 = t29 * S24
      t57 = t25 * S23
      t59 = t31 * S24
      t64 = 0.1D1 / S12
      t66 = 0.18D2 * t37
      t68 = 0.18D2 * t59
      t72 = t46 + t52 + t54 + t42 + t40 + t66 + 0.9D1 * t57 + t68 + 0.9D
     #1 * t35 + t50 - 0.18D2 * t55 - 0.18D2 * t43
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t98 = 0.18D2 * t49 + 0.72D2 * t43 + 0.72D2 * t57 + t68 + 0.72D2 * 
     #t55 - 0.9D1 * t51 + 0.72D2 * t35 + 0.18D2 * t45 + 0.45D2 * t53 - 0
     #.9D1 * t41 + t66 + 0.45D2 * t39
      t108 = t31 ** 2
      t110 = t33 ** 2
      rrgg2gghhard81J3 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0
     #.2D1 * S24 * t6) * S12 + 0.9D1 * S23 + 0.9D1 * S14 + (-0.27D2 / 0.
     #2D1 * t13 - 0.9D1 / 0.2D1 * t15 - 0.9D1 * t17) * t2 + (-0.27D2 / 0
     #.2D1 * t21 - 0.9D1 * t23 - 0.9D1 / 0.2D1 * t25) * t6 + (-0.18D2 * 
     #t29 + 0.18D2 * t31 + 0.18D2 * t33 + (0.9D1 / 0.2D1 * t35 + 0.9D1 /
     # 0.2D1 * t37 + t40 + t42 + 0.27D2 / 0.2D1 * t43 + t46) * t2 + (t50
     # + t52 + t54 + 0.27D2 / 0.2D1 * t55 + 0.9D1 / 0.2D1 * t57 + 0.9D1 
     #/ 0.2D1 * t59) * t6) * t64 + t72 * t74) * s * z + (0.18D2 * S24 + 
     #0.18D2 * S13) * S12 + 0.54D2 * t25 + 0.54D2 * t15 + 0.72D2 * t23 +
     # 0.45D2 * t13 + 0.45D2 * t21 + 0.72D2 * t17 + t98 * t64 + (0.18D2 
     #* t31 * S23 * S14 + 0.27D2 * t31 * t33 + 0.18D2 * S23 * t33 * S14 
     #+ 0.9D1 * t108 + 0.9D1 * t110) * t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard81J4
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t6 = 0.1D1 / (S12 + S13 + S23)
      t13 = S13 * S23
      t15 = S13 ** 2
      t17 = S13 * S14
      t21 = S14 * S24
      t23 = S23 * S24
      t25 = S24 ** 2
      t29 = S14 * S23
      t31 = S23 ** 2
      t33 = S14 ** 2
      t35 = t15 * S14
      t37 = S13 * t33
      t39 = t15 * S23
      t40 = 0.9D1 * t39
      t42 = 0.18D2 * S13 * t31
      t43 = t17 * S23
      t45 = t15 * S13
      t46 = 0.9D1 * t45
      t49 = t25 * S24
      t50 = 0.9D1 * t49
      t52 = 0.18D2 * t33 * S24
      t53 = S14 * t25
      t54 = 0.9D1 * t53
      t55 = t29 * S24
      t57 = t25 * S23
      t59 = t31 * S24
      t64 = 0.1D1 / S12
      t66 = 0.18D2 * t37
      t68 = 0.18D2 * t59
      t72 = t46 + t52 + t54 + t42 + t40 + t66 + 0.9D1 * t57 + t68 + 0.9D
     #1 * t35 + t50 - 0.45D2 * t55 - 0.45D2 * t43
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t96 = 0.18D2 * t49 + 0.63D2 * t43 + 0.72D2 * t57 + t68 + 0.63D2 * 
     #t55 - t52 + 0.72D2 * t35 + 0.18D2 * t45 + 0.36D2 * t53 - t42 + t66
     # + 0.36D2 * t39
      t106 = t31 ** 2
      t108 = t33 ** 2
      rrgg2gghhard81J4 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0
     #.2D1 * S24 * t6) * S12 + 0.9D1 * S23 + 0.9D1 * S14 + (-0.27D2 * t1
     #3 - 0.9D1 / 0.2D1 * t15 - 0.9D1 * t17) * t2 + (-0.27D2 * t21 - 0.9
     #D1 * t23 - 0.9D1 / 0.2D1 * t25) * t6 + (-0.45D2 * t29 + 0.18D2 * t
     #31 + 0.18D2 * t33 + (0.9D1 / 0.2D1 * t35 + 0.9D1 / 0.2D1 * t37 + t
     #40 + t42 + 0.27D2 * t43 + t46) * t2 + (t50 + t52 + t54 + 0.27D2 * 
     #t55 + 0.9D1 / 0.2D1 * t57 + 0.9D1 / 0.2D1 * t59) * t6) * t64 + t72
     # * t74) * s * z + (0.18D2 * S24 + 0.18D2 * S13) * S12 + 0.54D2 * t
     #25 + 0.54D2 * t15 + 0.72D2 * t23 + 0.36D2 * t13 + 0.36D2 * t21 + 0
     #.72D2 * t17 + t96 * t64 + (0.18D2 * t31 * S23 * S14 + 0.27D2 * t31
     # * t33 + 0.18D2 * S23 * t33 * S14 + 0.9D1 * t106 + 0.9D1 * t108) *
     # t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard81J5
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t6 = 0.1D1 / (S12 + S13 + S23)
      t13 = S13 * S23
      t15 = S13 ** 2
      t17 = S13 * S14
      t21 = S14 * S24
      t23 = S23 * S24
      t25 = S24 ** 2
      t29 = S14 * S23
      t31 = S23 ** 2
      t33 = S14 ** 2
      t35 = t15 * S14
      t37 = S13 * t33
      t39 = t15 * S23
      t40 = 0.9D1 * t39
      t41 = S13 * t31
      t42 = 0.18D2 * t41
      t43 = t17 * S23
      t45 = t15 * S13
      t46 = 0.9D1 * t45
      t49 = t25 * S24
      t50 = 0.9D1 * t49
      t51 = t33 * S24
      t52 = 0.18D2 * t51
      t53 = S14 * t25
      t54 = 0.9D1 * t53
      t55 = t29 * S24
      t57 = t25 * S23
      t59 = t31 * S24
      t64 = 0.1D1 / S12
      t66 = 0.18D2 * t37
      t68 = 0.18D2 * t59
      t72 = t46 + t52 + t54 + t42 + t40 + t66 + 0.9D1 * t57 + t68 + 0.9D
     #1 * t35 + t50 - 0.72D2 * t55 - 0.72D2 * t43
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t98 = 0.18D2 * t49 + 0.54D2 * t43 + 0.72D2 * t57 + t68 + 0.54D2 * 
     #t55 - 0.27D2 * t51 + 0.72D2 * t35 + 0.18D2 * t45 + 0.27D2 * t53 - 
     #0.27D2 * t41 + t66 + 0.27D2 * t39
      t108 = t31 ** 2
      t110 = t33 ** 2
      rrgg2gghhard81J5 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0
     #.2D1 * S24 * t6) * S12 + 0.9D1 * S23 + 0.9D1 * S14 + (-0.81D2 / 0.
     #2D1 * t13 - 0.9D1 / 0.2D1 * t15 - 0.9D1 * t17) * t2 + (-0.81D2 / 0
     #.2D1 * t21 - 0.9D1 * t23 - 0.9D1 / 0.2D1 * t25) * t6 + (-0.72D2 * 
     #t29 + 0.18D2 * t31 + 0.18D2 * t33 + (0.9D1 / 0.2D1 * t35 + 0.9D1 /
     # 0.2D1 * t37 + t40 + t42 + 0.81D2 / 0.2D1 * t43 + t46) * t2 + (t50
     # + t52 + t54 + 0.81D2 / 0.2D1 * t55 + 0.9D1 / 0.2D1 * t57 + 0.9D1 
     #/ 0.2D1 * t59) * t6) * t64 + t72 * t74) * s * z + (0.18D2 * S24 + 
     #0.18D2 * S13) * S12 + 0.54D2 * t25 + 0.54D2 * t15 + 0.72D2 * t23 +
     # 0.27D2 * t13 + 0.27D2 * t21 + 0.72D2 * t17 + t98 * t64 + (0.18D2 
     #* t31 * S23 * S14 + 0.27D2 * t31 * t33 + 0.18D2 * S23 * t33 * S14 
     #+ 0.9D1 * t108 + 0.9D1 * t110) * t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard81J6
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t6 = 0.1D1 / (S12 + S13 + S23)
      t13 = S13 * S23
      t15 = S13 ** 2
      t17 = S13 * S14
      t21 = S14 * S24
      t23 = S23 * S24
      t25 = S24 ** 2
      t29 = S14 * S23
      t31 = S23 ** 2
      t33 = S14 ** 2
      t35 = t15 * S14
      t37 = S13 * t33
      t39 = t15 * S23
      t40 = 0.45D2 * t39
      t42 = 0.90D2 * S13 * t31
      t43 = t17 * S23
      t45 = t15 * S13
      t46 = 0.45D2 * t45
      t49 = t25 * S24
      t50 = 0.45D2 * t49
      t52 = 0.90D2 * t33 * S24
      t53 = S14 * t25
      t54 = 0.45D2 * t53
      t55 = t29 * S24
      t57 = t25 * S23
      t59 = t31 * S24
      t64 = 0.1D1 / S12
      t66 = 0.90D2 * t37
      t68 = 0.90D2 * t59
      t72 = -t46 - t52 - t54 - t42 - t40 - t66 - 0.45D2 * t57 - t68 - 0.
     #45D2 * t35 - t50 - 0.315D3 * t55 - 0.315D3 * t43
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t96 = -0.90D2 * t49 - 0.495D3 * t43 - 0.360D3 * t57 - t68 - 0.495D
     #3 * t55 - t52 - 0.360D3 * t35 - 0.90D2 * t45 - 0.360D3 * t53 - t42
     # - t66 - 0.360D3 * t39
      t106 = t31 ** 2
      t108 = t33 ** 2
      rrgg2gghhard81J6 = (((-0.45D2 - 0.45D2 / 0.2D1 * S13 * t2 - 0.45D2
     # / 0.2D1 * S24 * t6) * S12 - 0.45D2 * S23 - 0.45D2 * S14 + (-0.135
     #D3 * t13 + 0.45D2 / 0.2D1 * t15 + 0.45D2 * t17) * t2 + (-0.135D3 *
     # t21 + 0.45D2 * t23 + 0.45D2 / 0.2D1 * t25) * t6 + (-0.315D3 * t29
     # - 0.90D2 * t31 - 0.90D2 * t33 + (-0.45D2 / 0.2D1 * t35 - 0.45D2 /
     # 0.2D1 * t37 - t40 - t42 + 0.135D3 * t43 - t46) * t2 + (-t50 - t52
     # - t54 + 0.135D3 * t55 - 0.45D2 / 0.2D1 * t57 - 0.45D2 / 0.2D1 * t
     #59) * t6) * t64 + t72 * t74) * s * z + (-0.90D2 * S24 - 0.90D2 * S
     #13) * S12 - 0.270D3 * t25 - 0.270D3 * t15 - 0.360D3 * t23 - 0.360D
     #3 * t13 - 0.360D3 * t21 - 0.360D3 * t17 + t96 * t64 + (-0.90D2 * t
     #31 * S23 * S14 - 0.135D3 * t31 * t33 - 0.90D2 * S23 * t33 * S14 - 
     #0.45D2 * t106 - 0.45D2 * t108) * t74) / pi * wd / z

      end function
  
   
      subroutine rrgg2gghsoftt8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt8s1e1  
      doubleprecision rrgg2gghsoftt8s1e0  
      doubleprecision rrgg2gghsoftt8s1em1  
      doubleprecision rrgg2gghsoftt8s1em2  
      doubleprecision rrgg2gghsoftt8s1em3  
      doubleprecision rrgg2gghsoftt8s1em4  
      doubleprecision rrgg2gghsoftt8s2e1  
      doubleprecision rrgg2gghsoftt8s2e0  
      doubleprecision rrgg2gghsoftt8s2em1  
      doubleprecision rrgg2gghsoftt8s2em2  
      doubleprecision rrgg2gghsoftt8s2em3  
      doubleprecision rrgg2gghsoftt8s2em4  
      doubleprecision rrgg2gghsoftt8s3e1  
      doubleprecision rrgg2gghsoftt8s3e0  
      doubleprecision rrgg2gghsoftt8s3em1  
      doubleprecision rrgg2gghsoftt8s3em2  
      doubleprecision rrgg2gghsoftt8s3em3  
      doubleprecision rrgg2gghsoftt8s3em4  
      doubleprecision rrgg2gghsoftt8s4e1  
      doubleprecision rrgg2gghsoftt8s4e0  
      doubleprecision rrgg2gghsoftt8s4em1  
      doubleprecision rrgg2gghsoftt8s4em2  
      doubleprecision rrgg2gghsoftt8s4em3  
      doubleprecision rrgg2gghsoftt8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghsoftt8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghsoftt8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghsoftt8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghsoftt8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghsoftt8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghsoftt8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghsoftt8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghsoftt8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghsoftt8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghsoftt8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gghsoftt8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gghsoftt8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt8s1e1
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
      t45 = 0.180D3 * wd * lh
      t46 = 0.90D2 * wd
      t47 = -t45 + t46
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t58 = x2 ** 2
      t59 = t5 * t58
      t60 = -0.1D1 + x2
      t64 = log(-0.4D1 * t4 * t59 * t60)
      t65 = x2 * x3
      t67 = (0.1D1 - x3 + t65) ** 2
      t68 = 0.1D1 / t67
      t71 = t58 * t68 * t60 * t9
      t74 = log(0.4D1 * t36 * t71)
      t77 = Sqrt(x3 * t60 * t9)
      t81 = 0.1D1 / (-0.1D1 - x3 + t65 + 0.2D1 * t16 * t77)
      t83 = x3 * t58
      t84 = t5 * t3
      t87 = log(0.4D1 * t83 * t84)
      t91 = log(-0.4D1 * t4 * t59 * t10)
      t93 = t7 * t58
      t94 = t93 * t60
      t97 = log(-0.4D1 * t36 * t94)
      t101 = log(-0.4D1 * t36 * t93 * t10)
      t105 = log(0.4D1 * t32 * t71)
      t107 = t84 * t7
      t110 = log(0.4D1 * t83 * t107)
      t114 = 0.1D1 / x2
      t120 = log(-0.4D1 * t84 * t94)
      t121 = t120 ** 2
      t122 = t3 * t60
      t125 = log(-0.4D1 * t59 * t122)
      t126 = t125 ** 2
      t129 = log(0.4D1 * t59 * t31)
      t130 = t129 ** 2
      t133 = log(0.4D1 * t59 * t3)
      t134 = t133 ** 2
      t146 = log(0.4D1 * t107)
      t147 = t146 ** 2
      t149 = log(0.4D1 * t84)
      t150 = t149 ** 2
      t160 = pi ** 2
      t162 = lh ** 2
      t164 = -0.30D2 * t160 + 0.180D3 * t162
      t165 = wd * t164
      t166 = t165 - t45 + t46
      t173 = log(0.4D1 * t4)
      t176 = log(-0.4D1 * t4 * t10)
      t180 = t176 ** 2
      t183 = t173 ** 2
      t194 = -0.240D3 * zeta3 - 0.120D3 * t162 * lh + 0.60D2 * lh * t160
      t195 = wd * t194
      t206 = t83 * t3
      t208 = log(0.4D1 * t206)
      t209 = t208 ** 2
      t213 = log(-0.4D1 * t4 * t58 * t10)
      t214 = t213 ** 2
      t218 = log(-0.4D1 * t83 * t122)
      t219 = t218 ** 2
      t224 = log(0.4D1 * t206 * t68 * t60 * t9)
      t225 = t224 ** 2
      t241 = t58 * t3
      t243 = log(0.4D1 * t241)
      t244 = t243 ** 2
      t247 = log(-0.4D1 * t241 * t60)
      t248 = t247 ** 2
      t265 = log(0.4D1 * t3)
      t266 = t265 * wd
      t268 = t265 ** 2
      t269 = t268 * wd
      t271 = 0.3D1 * wd - 0.2D1 * t266 + t269 / 0.2D1
      t279 = t268 * t265 * wd
      t282 = 0.2D1 * wd - t266
      t295 = t160 ** 2
      t296 = t162 ** 2
      t305 = t268 ** 2
      t308 = -(0.90D2 * wd * (-t15 * t22 / 0.2D1 + t28 * t22 / 0.2D1 - t
     #35 / 0.2D1 + t39 / 0.2D1) + t47 * (t14 * t22 - t27 * t22 + t34 - t
     #38)) * t53 * t55 / 0.40D2 - 0.9D1 / 0.2D1 * wd * (t64 + t74 * t81 
     #- t87 - t91 * t22 - t97 + t101 * t22 - t105 * t81 + t110) * t53 * 
     #t55 * t114 + (0.90D2 * wd * (-t121 / 0.2D1 + t126 / 0.2D1 + t130 /
     # 0.2D1 - t134 / 0.2D1) + t47 * (t120 - t125 - t129 + t133)) * t55 
     #* t114 / 0.20D2 - (t47 * (-t147 / 0.2D1 + t150 / 0.2D1) + 0.90D2 *
     # wd * (-t150 * t149 / 0.6D1 + t147 * t146 / 0.6D1) + t166 * (t146 
     #- t149)) * t55 / 0.40D2 + (t166 * (t173 + t176 * t22) + 0.90D2 * w
     #d * (t180 * t176 * t22 / 0.6D1 + t183 * t173 / 0.6D1) + (-t45 + t4
     #6 + t165 + t195) * (-0.1D1 - t22) + t47 * (-t183 / 0.2D1 - t180 * 
     #t22 / 0.2D1)) * t53 / 0.80D2 - (0.90D2 * wd * (t209 / 0.2D1 + t214
     # * t22 / 0.2D1 - t219 / 0.2D1 - t225 * t81 / 0.2D1) + t47 * (-t208
     # - t213 * t22 + t218 + t224 * t81) + t166 * (-t81 + t22)) * t53 * 
     #t114 / 0.40D2 - (t47 * (t244 / 0.2D1 - t248 / 0.2D1) + 0.90D2 * wd
     # * (t248 * t247 / 0.6D1 - t244 * t243 / 0.6D1) + t166 * (-t243 + t
     #247)) * t114 / 0.40D2 - 0.9D1 / 0.4D1 * t271 * lh + t195 / 0.80D2 
     #- 0.9D1 / 0.8D1 * wd + 0.9D1 / 0.8D1 * t266 - 0.9D1 / 0.16D2 * t26
     #9 + 0.3D1 / 0.16D2 * t279 + t282 * t164 / 0.80D2 + 0.9D1 / 0.4D1 *
     # (0.4D1 * wd - 0.3D1 * t266 + t269 - t279 / 0.6D1) * lh - t271 * t
     #164 / 0.80D2 - t282 * t194 / 0.80D2 - wd * (t295 + 0.60D2 * t296 +
     # 0.480D3 * lh * zeta3 - 0.60D2 * t162 * t160) / 0.80D2 - 0.3D1 / 0
     #.64D2 * t305 * wd
      t309 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t308)
      rrgg2gghsoftt8s1e1 = t309 * t308

      end function



      doubleprecision function rrgg2gghsoftt8s1e0
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
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t11 = t10 ** 2
      t13 = log(0.4D1 * t5)
      t14 = t13 ** 2
      t20 = 0.180D3 * wd * lh
      t21 = 0.90D2 * wd
      t22 = -t20 + t21
      t26 = 0.1D1 / x1
      t29 = x3 * t4
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t29 * t1 * t7 * t32)
      t37 = cos(t2)
      t39 = Sqrt(-x3 * t31)
      t43 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t37 * t39)
      t48 = log(-0.4D1 * t29 * t1 * t32)
      t50 = x3 * t1
      t51 = t4 * t7
      t54 = log(0.4D1 * t50 * t51)
      t57 = log(0.4D1 * t50 * t4)
      t60 = 0.1D1 / x3
      t64 = x2 ** 2
      t66 = -0.1D1 + x2
      t70 = log(-0.4D1 * t5 * t7 * t64 * t66)
      t71 = t64 * t1
      t72 = t4 * t66
      t75 = log(-0.4D1 * t71 * t72)
      t78 = log(0.4D1 * t71 * t51)
      t81 = log(0.4D1 * t71 * t4)
      t84 = 0.1D1 / x2
      t89 = log(0.4D1 * t29)
      t90 = t89 ** 2
      t93 = log(-0.4D1 * t29 * t32)
      t94 = t93 ** 2
      t103 = pi ** 2
      t105 = lh ** 2
      t107 = -0.30D2 * t103 + 0.180D3 * t105
      t108 = wd * t107
      t117 = log(0.4D1 * t4)
      t118 = t117 * wd
      t119 = 0.2D1 * wd - t118
      t124 = t117 ** 2
      t125 = t124 * wd
      t147 = x3 * t64
      t148 = t147 * t4
      t150 = log(0.4D1 * t148)
      t154 = log(-0.4D1 * t29 * t64 * t32)
      t158 = log(-0.4D1 * t147 * t72)
      t159 = x2 * x3
      t161 = (0.1D1 - x3 + t159) ** 2
      t167 = log(0.4D1 * t148 / t161 * t66 * t31)
      t170 = Sqrt(x3 * t66 * t31)
      t174 = 0.1D1 / (-0.1D1 - x3 + t159 + 0.2D1 * t37 * t170)
      t185 = t64 * t4
      t187 = log(0.4D1 * t185)
      t188 = t187 ** 2
      t191 = log(-0.4D1 * t185 * t66)
      t192 = t191 ** 2
      t202 = -(0.90D2 * wd * (-t11 / 0.2D1 + t14 / 0.2D1) + t22 * (t10 -
     # t13)) * t26 / 0.40D2 - 0.9D1 / 0.4D1 * wd * (t36 * t43 - t48 * t4
     #3 + t54 - t57) * t60 * t26 + 0.9D1 / 0.2D1 * wd * (t70 - t75 - t78
     # + t81) * t26 * t84 + (0.90D2 * wd * (-t90 / 0.2D1 - t94 * t43 / 0
     #.2D1) + t22 * (t89 + t93 * t43) + (t108 - t20 + t21) * (-0.1D1 - t
     #43)) * t60 / 0.80D2 - 0.9D1 / 0.4D1 * t119 * lh - 0.9D1 / 0.8D1 * 
     #wd + 0.9D1 / 0.8D1 * t118 - 0.9D1 / 0.16D2 * t125 + t108 / 0.80D2 
     #+ 0.9D1 / 0.4D1 * (0.3D1 * wd - 0.2D1 * t118 + t125 / 0.2D1) * lh 
     #- wd * (-0.240D3 * zeta3 - 0.120D3 * t105 * lh + 0.60D2 * lh * t10
     #3) / 0.80D2 + 0.3D1 / 0.16D2 * t124 * t117 * wd - t119 * t107 / 0.
     #80D2 - (0.90D2 * wd * (-t150 - t154 * t43 + t158 + t167 * t174) + 
     #t22 * (-t174 + t43)) * t60 * t84 / 0.40D2 - (0.90D2 * wd * (t188 /
     # 0.2D1 - t192 / 0.2D1) + t22 * (-t187 + t191)) * t84 / 0.40D2
      t203 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t202)
      rrgg2gghsoftt8s1e0 = t203 * t202

      end function



      doubleprecision function rrgg2gghsoftt8s1em1
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
      t23 = wd * lh
      t30 = 0.1D1 / x3
      t33 = x1 ** 2
      t34 = t33 * t3
      t36 = (-0.1D1 + x1) ** 2
      t39 = log(0.4D1 * t34 * t36)
      t41 = log(0.4D1 * t34)
      t48 = -0.1D1 + x2
      t51 = Sqrt(x3 * t48 * t7)
      t58 = 0.1D1 / x2
      t62 = x2 ** 2
      t63 = t62 * t3
      t65 = log(0.4D1 * t63)
      t68 = log(-0.4D1 * t63 * t48)
      t76 = log(0.4D1 * t3)
      t77 = t76 * wd
      t83 = t76 ** 2
      t86 = pi ** 2
      t88 = lh ** 2
      t93 = (0.90D2 * wd * (t6 + t11 * t18) + (-0.180D3 * t23 + 0.90D2 *
     # wd) * (-0.1D1 - t18)) * t30 / 0.80D2 - 0.9D1 / 0.4D1 * wd * (t39 
     #- t41) / x1 - 0.9D1 / 0.4D1 * wd * (-0.1D1 / (-0.1D1 - x3 + x2 * x
     #3 + 0.2D1 * t12 * t51) + t18) * t30 * t58 - 0.9D1 / 0.4D1 * wd * (
     #-t65 + t68) * t58 - 0.9D1 / 0.4D1 * t23 - 0.9D1 / 0.8D1 * wd + 0.9
     #D1 / 0.8D1 * t77 + 0.9D1 / 0.4D1 * (0.2D1 * wd - t77) * lh - 0.9D1
     # / 0.16D2 * t83 * wd - wd * (-0.30D2 * t86 + 0.180D3 * t88) / 0.80
     #D2
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      rrgg2gghsoftt8s1em1 = t94 * t93

      end function



      doubleprecision function rrgg2gghsoftt8s1em2
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
      t18 = Sin(t1)
      t19 = t18 ** 2
      t21 = log(0.4D1 * t19)
      t24 = 0.9D1 / 0.8D1 * wd * (-0.1D1 - 0.1D1 / (-0.1D1 - x3 + 0.2D1 
     #* t2 * t5)) / x3 - 0.9D1 / 0.8D1 * wd + 0.9D1 / 0.4D1 * wd * lh + 
     #0.9D1 / 0.8D1 * t21 * wd
      t25 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t24)
      rrgg2gghsoftt8s1em2 = t25 * t24

      end function



      doubleprecision function rrgg2gghsoftt8s1em3
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
      rrgg2gghsoftt8s1em3 = -0.9D1 / 0.8D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt8s1em4
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
      rrgg2gghsoftt8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt8s2e1
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
      t11 = (-0.1D1 + x1) ** 2
      t12 = t5 * t11
      t13 = t2 * t12
      t15 = log(0.4D1 * t13)
      t16 = t15 ** 2
      t17 = -0.1D1 + x3
      t18 = t5 * t17
      t21 = log(-0.4D1 * t2 * t18)
      t22 = t21 ** 2
      t23 = x3 * t5
      t28 = log(-0.4D1 * t23 * t1 * t11 * t17)
      t29 = t28 ** 2
      t35 = 0.180D3 * wd * lh
      t36 = 0.90D2 * wd
      t37 = -t35 + t36
      t41 = 0.1D1 / x3
      t43 = 0.1D1 / x1
      t50 = log(-0.4D1 * t6 * t11 * x2 * t17)
      t51 = x2 * x3
      t52 = t1 * t5
      t54 = (-0.1D1 + x2) ** 2
      t58 = log(0.4D1 * t51 * t52 * t54)
      t62 = log(-0.4D1 * t51 * t52 * t17)
      t65 = (-0.1D1 + t51) ** 2
      t66 = 0.1D1 / t65
      t68 = x2 * t54 * t17 * t66
      t71 = log(-0.4D1 * t13 * t68)
      t72 = cos(t3)
      t74 = Sqrt(-t51 * t17)
      t78 = 0.1D1 / (-t51 - 0.1D1 + 0.2D1 * t72 * t74)
      t82 = log(-0.4D1 * t6 * t68)
      t84 = t52 * t11
      t87 = log(0.4D1 * t51 * t84)
      t90 = log(0.4D1 * t51 * t52)
      t92 = t5 * t54
      t93 = t92 * t11
      t96 = log(0.4D1 * t51 * t1 * t93)
      t100 = 0.1D1 / x2
      t104 = x2 * t1
      t107 = log(0.4D1 * t104 * t12)
      t108 = t107 ** 2
      t111 = log(0.4D1 * t104 * t93)
      t112 = t111 ** 2
      t115 = log(0.4D1 * t104 * t92)
      t116 = t115 ** 2
      t119 = log(0.4D1 * t104 * t5)
      t120 = t119 ** 2
      t132 = log(0.4D1 * t84)
      t133 = t132 ** 2
      t135 = log(0.4D1 * t52)
      t136 = t135 ** 2
      t146 = pi ** 2
      t148 = lh ** 2
      t150 = -0.30D2 * t146 + 0.180D3 * t148
      t152 = wd * t150 - t35 + t36
      t159 = log(0.4D1 * t23)
      t160 = t159 ** 2
      t163 = log(-0.4D1 * t23 * t17)
      t164 = t163 ** 2
      t181 = log(0.4D1 * t5)
      t182 = t181 * wd
      t184 = t181 ** 2
      t185 = t184 * wd
      t187 = 0.3D1 * wd - 0.2D1 * t182 + t185 / 0.2D1
      t195 = -0.240D3 * zeta3 - 0.120D3 * t148 * lh + 0.60D2 * lh * t146
      t202 = t184 * t181 * wd
      t205 = 0.2D1 * wd - t182
      t218 = t146 ** 2
      t219 = t148 ** 2
      t228 = t184 ** 2
      t233 = log(0.4D1 * t51 * t92)
      t234 = t233 ** 2
      t237 = log(-0.4D1 * t51 * t18)
      t238 = t237 ** 2
      t239 = t51 * t5
      t241 = log(0.4D1 * t239)
      t242 = t241 ** 2
      t247 = log(-0.4D1 * t239 * t54 * t17 * t66)
      t248 = t247 ** 2
      t263 = x2 * t5
      t265 = log(0.4D1 * t263)
      t266 = t265 ** 2
      t269 = log(0.4D1 * t263 * t54)
      t270 = t269 ** 2
      t285 = (0.90D2 * wd * (-t9 / 0.2D1 + t16 / 0.2D1 + t22 / 0.2D1 - t
     #29 / 0.2D1) + t37 * (t8 - t15 - t21 + t28)) * t41 * t43 / 0.20D2 -
     # 0.9D1 / 0.2D1 * wd * (-t50 + t58 + t62 - t71 * t78 + t82 * t78 + 
     #t87 - t90 - t96) * t41 * t43 * t100 - (0.90D2 * wd * (-t108 / 0.2D
     #1 + t112 / 0.2D1 - t116 / 0.2D1 + t120 / 0.2D1) + t37 * (t107 - t1
     #11 + t115 - t119)) * t43 * t100 / 0.20D2 - (t37 * (-t133 / 0.2D1 +
     # t136 / 0.2D1) + 0.90D2 * wd * (-t136 * t135 / 0.6D1 + t133 * t132
     # / 0.6D1) + t152 * (t132 - t135)) * t43 / 0.20D2 - (t37 * (t160 / 
     #0.2D1 - t164 / 0.2D1) + 0.90D2 * wd * (t164 * t163 / 0.6D1 - t160 
     #* t159 / 0.6D1) + t152 * (-t159 + t163)) * t41 / 0.40D2 - 0.9D1 / 
     #0.2D1 * t187 * lh + wd * t195 / 0.40D2 - 0.9D1 / 0.4D1 * wd + 0.9D
     #1 / 0.4D1 * t182 - 0.9D1 / 0.8D1 * t185 + 0.3D1 / 0.8D1 * t202 + t
     #205 * t150 / 0.40D2 + 0.9D1 / 0.2D1 * (0.4D1 * wd - 0.3D1 * t182 +
     # t185 - t202 / 0.6D1) * lh - t187 * t150 / 0.40D2 - t205 * t195 / 
     #0.40D2 - wd * (t218 + 0.60D2 * t219 + 0.480D3 * lh * zeta3 - 0.60D
     #2 * t148 * t146) / 0.40D2 - 0.3D1 / 0.32D2 * t228 * wd + (0.90D2 *
     # wd * (t234 / 0.2D1 + t238 / 0.2D1 - t242 / 0.2D1 + t248 * t78 / 0
     #.2D1) + t37 * (-t233 - t237 + t241 - t247 * t78) + t152 * (0.1D1 +
     # t78)) * t41 * t100 / 0.40D2 + (t37 * (-t266 / 0.2D1 + t270 / 0.2D
     #1) + 0.90D2 * wd * (-t270 * t269 / 0.6D1 + t266 * t265 / 0.6D1) + 
     #t152 * (t265 - t269)) * t100 / 0.40D2
      t286 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t285)
      rrgg2gghsoftt8s2e1 = t286 * t285

      end function



      doubleprecision function rrgg2gghsoftt8s2e0
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
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t11 = t10 ** 2
      t13 = log(0.4D1 * t5)
      t14 = t13 ** 2
      t22 = -0.180D3 * wd * lh + 0.90D2 * wd
      t26 = 0.1D1 / x1
      t29 = x3 * t1
      t32 = log(0.4D1 * t29 * t4)
      t33 = t4 * t7
      t36 = log(0.4D1 * t29 * t33)
      t37 = -0.1D1 + x3
      t38 = t4 * t37
      t41 = log(-0.4D1 * t29 * t38)
      t42 = x3 * t4
      t47 = log(-0.4D1 * t42 * t1 * t7 * t37)
      t50 = 0.1D1 / x3
      t54 = x2 * t1
      t57 = log(0.4D1 * t54 * t33)
      t59 = (-0.1D1 + x2) ** 2
      t60 = t4 * t59
      t64 = log(0.4D1 * t54 * t60 * t7)
      t67 = log(0.4D1 * t54 * t60)
      t70 = log(0.4D1 * t54 * t4)
      t73 = 0.1D1 / x2
      t79 = log(0.4D1 * t4)
      t80 = t79 * wd
      t81 = 0.2D1 * wd - t80
      t86 = t79 ** 2
      t87 = t86 * wd
      t89 = pi ** 2
      t91 = lh ** 2
      t93 = -0.30D2 * t89 + 0.180D3 * t91
      t116 = log(0.4D1 * t42)
      t117 = t116 ** 2
      t120 = log(-0.4D1 * t42 * t37)
      t121 = t120 ** 2
      t131 = x2 * x3
      t134 = log(0.4D1 * t131 * t60)
      t137 = log(-0.4D1 * t131 * t38)
      t138 = t131 * t4
      t140 = log(0.4D1 * t138)
      t143 = (-0.1D1 + t131) ** 2
      t148 = log(-0.4D1 * t138 * t59 * t37 / t143)
      t149 = cos(t2)
      t151 = Sqrt(-t131 * t37)
      t155 = 0.1D1 / (-t131 - 0.1D1 + 0.2D1 * t149 * t151)
      t166 = x2 * t4
      t168 = log(0.4D1 * t166)
      t169 = t168 ** 2
      t172 = log(0.4D1 * t166 * t59)
      t173 = t172 ** 2
      t183 = -(0.90D2 * wd * (-t11 / 0.2D1 + t14 / 0.2D1) + t22 * (t10 -
     # t13)) * t26 / 0.20D2 + 0.9D1 / 0.2D1 * wd * (t32 - t36 - t41 + t4
     #7) * t50 * t26 - 0.9D1 / 0.2D1 * wd * (t57 - t64 + t67 - t70) * t2
     #6 * t73 - 0.9D1 / 0.2D1 * t81 * lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 
     #0.4D1 * t80 - 0.9D1 / 0.8D1 * t87 + wd * t93 / 0.40D2 + 0.9D1 / 0.
     #2D1 * (0.3D1 * wd - 0.2D1 * t80 + t87 / 0.2D1) * lh - wd * (-0.240
     #D3 * zeta3 - 0.120D3 * t91 * lh + 0.60D2 * lh * t89) / 0.40D2 + 0.
     #3D1 / 0.8D1 * t86 * t79 * wd - t81 * t93 / 0.40D2 - (0.90D2 * wd *
     # (t117 / 0.2D1 - t121 / 0.2D1) + t22 * (-t116 + t120)) * t50 / 0.4
     #0D2 + (0.90D2 * wd * (-t134 - t137 + t140 - t148 * t155) + t22 * (
     #0.1D1 + t155)) * t50 * t73 / 0.40D2 + (0.90D2 * wd * (-t169 / 0.2D
     #1 + t173 / 0.2D1) + t22 * (t168 - t172)) * t73 / 0.40D2
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t183)
      rrgg2gghsoftt8s2e0 = t184 * t183

      end function



      doubleprecision function rrgg2gghsoftt8s2em1
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
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t12 = log(0.4D1 * t5)
      t22 = log(0.4D1 * t4)
      t23 = t22 * wd
      t29 = t22 ** 2
      t32 = pi ** 2
      t34 = lh ** 2
      t39 = x3 * t4
      t41 = log(0.4D1 * t39)
      t42 = -0.1D1 + x3
      t45 = log(-0.4D1 * t39 * t42)
      t48 = 0.1D1 / x3
      t51 = x2 * x3
      t52 = cos(t2)
      t54 = Sqrt(-t51 * t42)
      t61 = 0.1D1 / x2
      t65 = x2 * t4
      t67 = log(0.4D1 * t65)
      t69 = (-0.1D1 + x2) ** 2
      t72 = log(0.4D1 * t65 * t69)
      t77 = -0.9D1 / 0.2D1 * wd * (t10 - t12) / x1 - 0.9D1 / 0.2D1 * wd 
     #* lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.4D1 * t23 + 0.9D1 / 0.2D1 * 
     #(0.2D1 * wd - t23) * lh - 0.9D1 / 0.8D1 * t29 * wd - wd * (-0.30D2
     # * t32 + 0.180D3 * t34) / 0.40D2 - 0.9D1 / 0.4D1 * wd * (-t41 + t4
     #5) * t48 + 0.9D1 / 0.4D1 * wd * (0.1D1 + 0.1D1 / (-t51 - 0.1D1 + 0
     #.2D1 * t52 * t54)) * t48 * t61 + 0.9D1 / 0.4D1 * wd * (t67 - t72) 
     #* t61
      t78 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t77)
      rrgg2gghsoftt8s2em1 = t78 * t77

      end function



      doubleprecision function rrgg2gghsoftt8s2em2
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
      t5 = Sin(x4 * pi)
      t6 = t5 ** 2
      t8 = log(0.4D1 * t6)
      t11 = -0.9D1 / 0.4D1 * wd + 0.9D1 / 0.2D1 * wd * lh + 0.9D1 / 0.4D
     #1 * t8 * wd
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t11)
      rrgg2gghsoftt8s2em2 = t12 * t11

      end function



      doubleprecision function rrgg2gghsoftt8s2em3
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
     # 0.4D1 * wd)
      rrgg2gghsoftt8s2em3 = -0.9D1 / 0.4D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt8s2em4
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
      rrgg2gghsoftt8s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt8s3e1
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
      t45 = 0.180D3 * wd * lh
      t46 = 0.90D2 * wd
      t47 = -t45 + t46
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t58 = x2 ** 2
      t59 = t12 * t58
      t63 = log(-0.4D1 * t6 * t59 * t15)
      t65 = t1 * t58
      t69 = log(-0.4D1 * t10 * t65 * t15)
      t71 = x2 * x3
      t73 = (0.1D1 - x3 + t71) ** 2
      t74 = 0.1D1 / t73
      t76 = -0.1D1 + x2
      t78 = t58 * t74 * t76 * t14
      t81 = log(0.4D1 * t6 * t78)
      t84 = Sqrt(x3 * t76 * t14)
      t88 = 0.1D1 / (-0.1D1 - x3 + t71 + 0.2D1 * t21 * t84)
      t90 = t59 * t76
      t93 = log(-0.4D1 * t6 * t90)
      t96 = log(0.4D1 * t36 * t78)
      t101 = log(-0.4D1 * t10 * t65 * t76)
      t102 = x3 * t58
      t103 = t1 * t5
      t106 = log(0.4D1 * t102 * t103)
      t107 = t103 * t12
      t110 = log(0.4D1 * t102 * t107)
      t114 = 0.1D1 / x2
      t120 = log(-0.4D1 * t103 * t90)
      t121 = t120 ** 2
      t122 = t5 * t76
      t125 = log(-0.4D1 * t65 * t122)
      t126 = t125 ** 2
      t129 = log(0.4D1 * t65 * t35)
      t130 = t129 ** 2
      t133 = log(0.4D1 * t65 * t5)
      t134 = t133 ** 2
      t146 = log(0.4D1 * t107)
      t147 = t146 ** 2
      t149 = log(0.4D1 * t103)
      t150 = t149 ** 2
      t160 = pi ** 2
      t162 = lh ** 2
      t164 = -0.30D2 * t160 + 0.180D3 * t162
      t165 = wd * t164
      t166 = t165 - t45 + t46
      t173 = log(0.4D1 * t10)
      t176 = log(-0.4D1 * t10 * t15)
      t180 = t176 ** 2
      t183 = t173 ** 2
      t194 = -0.240D3 * zeta3 - 0.120D3 * t162 * lh + 0.60D2 * lh * t160
      t195 = wd * t194
      t208 = log(-0.4D1 * t102 * t122)
      t209 = t208 ** 2
      t213 = log(-0.4D1 * t10 * t58 * t15)
      t214 = t213 ** 2
      t216 = t102 * t5
      t218 = log(0.4D1 * t216)
      t219 = t218 ** 2
      t224 = log(0.4D1 * t216 * t74 * t76 * t14)
      t225 = t224 ** 2
      t241 = t5 * t58
      t243 = log(0.4D1 * t241)
      t244 = t243 ** 2
      t247 = log(-0.4D1 * t241 * t76)
      t248 = t247 ** 2
      t265 = log(0.4D1 * t5)
      t266 = t265 * wd
      t268 = t265 ** 2
      t269 = t268 * wd
      t271 = 0.3D1 * wd - 0.2D1 * t266 + t269 / 0.2D1
      t279 = t268 * t265 * wd
      t282 = 0.2D1 * wd - t266
      t295 = t160 ** 2
      t296 = t162 ** 2
      t305 = t268 ** 2
      t308 = (0.90D2 * wd * (-t9 / 0.2D1 + t20 * t27 / 0.2D1 - t33 * t27
     # / 0.2D1 + t39 / 0.2D1) + t47 * (t8 - t19 * t27 + t32 * t27 - t38)
     #) * t53 * t55 / 0.40D2 + 0.9D1 / 0.2D1 * wd * (-t63 * t27 + t69 * 
     #t27 - t81 * t88 + t93 + t96 * t88 - t101 + t106 - t110) * t53 * t5
     #5 * t114 + (0.90D2 * wd * (-t121 / 0.2D1 + t126 / 0.2D1 + t130 / 0
     #.2D1 - t134 / 0.2D1) + t47 * (t120 - t125 - t129 + t133)) * t55 * 
     #t114 / 0.20D2 - (t47 * (-t147 / 0.2D1 + t150 / 0.2D1) + 0.90D2 * w
     #d * (-t150 * t149 / 0.6D1 + t147 * t146 / 0.6D1) + t166 * (t146 - 
     #t149)) * t55 / 0.40D2 - (t166 * (-t173 - t176 * t27) + 0.90D2 * wd
     # * (-t180 * t176 * t27 / 0.6D1 - t183 * t173 / 0.6D1) + (-t45 + t4
     #6 + t165 + t195) * (0.1D1 + t27) + t47 * (t183 / 0.2D1 + t180 * t2
     #7 / 0.2D1)) * t53 / 0.80D2 + (0.90D2 * wd * (t209 / 0.2D1 - t214 *
     # t27 / 0.2D1 - t219 / 0.2D1 + t225 * t88 / 0.2D1) + t47 * (-t208 +
     # t213 * t27 + t218 - t224 * t88) + t166 * (t88 - t27)) * t53 * t11
     #4 / 0.40D2 - (t47 * (t244 / 0.2D1 - t248 / 0.2D1) + 0.90D2 * wd * 
     #(t248 * t247 / 0.6D1 - t244 * t243 / 0.6D1) + t166 * (-t243 + t247
     #)) * t114 / 0.40D2 - 0.9D1 / 0.4D1 * t271 * lh + t195 / 0.80D2 - 0
     #.9D1 / 0.8D1 * wd + 0.9D1 / 0.8D1 * t266 - 0.9D1 / 0.16D2 * t269 +
     # 0.3D1 / 0.16D2 * t279 + t282 * t164 / 0.80D2 + 0.9D1 / 0.4D1 * (0
     #.4D1 * wd - 0.3D1 * t266 + t269 - t279 / 0.6D1) * lh - t271 * t164
     # / 0.80D2 - t282 * t194 / 0.80D2 - wd * (t295 + 0.60D2 * t296 + 0.
     #480D3 * lh * zeta3 - 0.60D2 * t162 * t160) / 0.80D2 - 0.3D1 / 0.64
     #D2 * t305 * wd
      t309 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t308)
      rrgg2gghsoftt8s3e1 = t309 * t308

      end function



      doubleprecision function rrgg2gghsoftt8s3e0
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
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t11 = t10 ** 2
      t13 = log(0.4D1 * t5)
      t14 = t13 ** 2
      t20 = 0.180D3 * wd * lh
      t21 = 0.90D2 * wd
      t22 = -t20 + t21
      t26 = 0.1D1 / x1
      t29 = x3 * t1
      t32 = log(0.4D1 * t29 * t4)
      t33 = x3 * t4
      t35 = -0.1D1 + x3
      t36 = 0.1D1 / t35
      t40 = log(-0.4D1 * t33 * t1 * t7 * t36)
      t41 = cos(t2)
      t43 = Sqrt(-x3 * t35)
      t47 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t41 * t43)
      t52 = log(-0.4D1 * t33 * t1 * t36)
      t54 = t4 * t7
      t57 = log(0.4D1 * t29 * t54)
      t60 = 0.1D1 / x3
      t64 = x2 ** 2
      t66 = -0.1D1 + x2
      t70 = log(-0.4D1 * t5 * t7 * t64 * t66)
      t71 = t64 * t1
      t72 = t4 * t66
      t75 = log(-0.4D1 * t71 * t72)
      t78 = log(0.4D1 * t71 * t54)
      t81 = log(0.4D1 * t71 * t4)
      t84 = 0.1D1 / x2
      t89 = log(0.4D1 * t33)
      t90 = t89 ** 2
      t93 = log(-0.4D1 * t33 * t36)
      t94 = t93 ** 2
      t103 = pi ** 2
      t105 = lh ** 2
      t107 = -0.30D2 * t103 + 0.180D3 * t105
      t108 = wd * t107
      t117 = log(0.4D1 * t4)
      t118 = t117 * wd
      t119 = 0.2D1 * wd - t118
      t124 = t117 ** 2
      t125 = t124 * wd
      t147 = x3 * t64
      t150 = log(-0.4D1 * t147 * t72)
      t154 = log(-0.4D1 * t33 * t64 * t36)
      t156 = t147 * t4
      t158 = log(0.4D1 * t156)
      t159 = x2 * x3
      t161 = (0.1D1 - x3 + t159) ** 2
      t167 = log(0.4D1 * t156 / t161 * t66 * t35)
      t170 = Sqrt(x3 * t66 * t35)
      t174 = 0.1D1 / (-0.1D1 - x3 + t159 + 0.2D1 * t41 * t170)
      t185 = t64 * t4
      t187 = log(0.4D1 * t185)
      t188 = t187 ** 2
      t191 = log(-0.4D1 * t185 * t66)
      t192 = t191 ** 2
      t202 = -(0.90D2 * wd * (-t11 / 0.2D1 + t14 / 0.2D1) + t22 * (t10 -
     # t13)) * t26 / 0.40D2 + 0.9D1 / 0.4D1 * wd * (t32 - t40 * t47 + t5
     #2 * t47 - t57) * t60 * t26 + 0.9D1 / 0.2D1 * wd * (t70 - t75 - t78
     # + t81) * t26 * t84 - (0.90D2 * wd * (t90 / 0.2D1 + t94 * t47 / 0.
     #2D1) + t22 * (-t89 - t93 * t47) + (t108 - t20 + t21) * (0.1D1 + t4
     #7)) * t60 / 0.80D2 - 0.9D1 / 0.4D1 * t119 * lh - 0.9D1 / 0.8D1 * w
     #d + 0.9D1 / 0.8D1 * t118 - 0.9D1 / 0.16D2 * t125 + t108 / 0.80D2 +
     # 0.9D1 / 0.4D1 * (0.3D1 * wd - 0.2D1 * t118 + t125 / 0.2D1) * lh -
     # wd * (-0.240D3 * zeta3 - 0.120D3 * t105 * lh + 0.60D2 * lh * t103
     #) / 0.80D2 + 0.3D1 / 0.16D2 * t124 * t117 * wd - t119 * t107 / 0.8
     #0D2 + (0.90D2 * wd * (-t150 + t154 * t47 + t158 - t167 * t174) + t
     #22 * (t174 - t47)) * t60 * t84 / 0.40D2 - (0.90D2 * wd * (t188 / 0
     #.2D1 - t192 / 0.2D1) + t22 * (-t187 + t191)) * t84 / 0.40D2
      t203 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t202)
      rrgg2gghsoftt8s3e0 = t203 * t202

      end function



      doubleprecision function rrgg2gghsoftt8s3em1
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
      t23 = wd * lh
      t30 = 0.1D1 / x3
      t33 = x1 ** 2
      t34 = t33 * t3
      t36 = (-0.1D1 + x1) ** 2
      t39 = log(0.4D1 * t34 * t36)
      t41 = log(0.4D1 * t34)
      t48 = -0.1D1 + x2
      t51 = Sqrt(x3 * t48 * t7)
      t58 = 0.1D1 / x2
      t62 = x2 ** 2
      t63 = t62 * t3
      t65 = log(0.4D1 * t63)
      t68 = log(-0.4D1 * t63 * t48)
      t76 = log(0.4D1 * t3)
      t77 = t76 * wd
      t83 = t76 ** 2
      t86 = pi ** 2
      t88 = lh ** 2
      t93 = -(0.90D2 * wd * (-t6 - t11 * t18) + (-0.180D3 * t23 + 0.90D2
     # * wd) * (0.1D1 + t18)) * t30 / 0.80D2 - 0.9D1 / 0.4D1 * wd * (t39
     # - t41) / x1 + 0.9D1 / 0.4D1 * wd * (0.1D1 / (-0.1D1 - x3 + x2 * x
     #3 + 0.2D1 * t12 * t51) - t18) * t30 * t58 - 0.9D1 / 0.4D1 * wd * (
     #-t65 + t68) * t58 - 0.9D1 / 0.4D1 * t23 - 0.9D1 / 0.8D1 * wd + 0.9
     #D1 / 0.8D1 * t77 + 0.9D1 / 0.4D1 * (0.2D1 * wd - t77) * lh - 0.9D1
     # / 0.16D2 * t83 * wd - wd * (-0.30D2 * t86 + 0.180D3 * t88) / 0.80
     #D2
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      rrgg2gghsoftt8s3em1 = t94 * t93

      end function



      doubleprecision function rrgg2gghsoftt8s3em2
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
      t18 = Sin(t1)
      t19 = t18 ** 2
      t21 = log(0.4D1 * t19)
      t24 = -0.9D1 / 0.8D1 * wd * (0.1D1 + 0.1D1 / (-0.1D1 - x3 + 0.2D1 
     #* t2 * t5)) / x3 - 0.9D1 / 0.8D1 * wd + 0.9D1 / 0.4D1 * wd * lh + 
     #0.9D1 / 0.8D1 * t21 * wd
      t25 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t24)
      rrgg2gghsoftt8s3em2 = t25 * t24

      end function



      doubleprecision function rrgg2gghsoftt8s3em3
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
      rrgg2gghsoftt8s3em3 = -0.9D1 / 0.8D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt8s3em4
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
      rrgg2gghsoftt8s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt8s4e1
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
      t11 = (-0.1D1 + x1) ** 2
      t12 = t5 * t11
      t13 = t2 * t12
      t15 = log(0.4D1 * t13)
      t16 = t15 ** 2
      t17 = -0.1D1 + x3
      t18 = t5 * t17
      t21 = log(-0.4D1 * t2 * t18)
      t22 = t21 ** 2
      t23 = x3 * t5
      t28 = log(-0.4D1 * t23 * t1 * t11 * t17)
      t29 = t28 ** 2
      t35 = 0.180D3 * wd * lh
      t36 = 0.90D2 * wd
      t37 = -t35 + t36
      t41 = 0.1D1 / x3
      t43 = 0.1D1 / x1
      t46 = x2 * x3
      t47 = t1 * t5
      t48 = t47 * t11
      t51 = log(0.4D1 * t46 * t48)
      t55 = log(-0.4D1 * t46 * t47 * t17)
      t57 = (-0.1D1 + x2) ** 2
      t60 = (-0.1D1 + t46) ** 2
      t61 = 0.1D1 / t60
      t63 = x2 * t57 * t17 * t61
      t66 = log(-0.4D1 * t6 * t63)
      t67 = cos(t3)
      t69 = Sqrt(-t46 * t17)
      t73 = 0.1D1 / (-t46 - 0.1D1 + 0.2D1 * t67 * t69)
      t78 = log(0.4D1 * t46 * t47 * t57)
      t81 = log(0.4D1 * t46 * t47)
      t84 = log(-0.4D1 * t13 * t63)
      t90 = log(-0.4D1 * t6 * t11 * x2 * t17)
      t92 = t5 * t57
      t93 = t92 * t11
      t96 = log(0.4D1 * t46 * t1 * t93)
      t100 = 0.1D1 / x2
      t104 = x2 * t1
      t107 = log(0.4D1 * t104 * t12)
      t108 = t107 ** 2
      t111 = log(0.4D1 * t104 * t93)
      t112 = t111 ** 2
      t115 = log(0.4D1 * t104 * t92)
      t116 = t115 ** 2
      t119 = log(0.4D1 * t104 * t5)
      t120 = t119 ** 2
      t132 = log(0.4D1 * t48)
      t133 = t132 ** 2
      t135 = log(0.4D1 * t47)
      t136 = t135 ** 2
      t146 = pi ** 2
      t148 = lh ** 2
      t150 = -0.30D2 * t146 + 0.180D3 * t148
      t152 = wd * t150 - t35 + t36
      t159 = log(0.4D1 * t23)
      t160 = t159 ** 2
      t163 = log(-0.4D1 * t23 * t17)
      t164 = t163 ** 2
      t181 = log(0.4D1 * t5)
      t182 = t181 * wd
      t184 = t181 ** 2
      t185 = t184 * wd
      t187 = 0.3D1 * wd - 0.2D1 * t182 + t185 / 0.2D1
      t195 = -0.240D3 * zeta3 - 0.120D3 * t148 * lh + 0.60D2 * lh * t146
      t202 = t184 * t181 * wd
      t205 = 0.2D1 * wd - t182
      t218 = t146 ** 2
      t219 = t148 ** 2
      t228 = t184 ** 2
      t231 = t46 * t5
      t236 = log(-0.4D1 * t231 * t57 * t17 * t61)
      t237 = t236 ** 2
      t241 = log(-0.4D1 * t46 * t18)
      t242 = t241 ** 2
      t245 = log(0.4D1 * t46 * t92)
      t246 = t245 ** 2
      t248 = log(0.4D1 * t231)
      t249 = t248 ** 2
      t263 = x2 * t5
      t265 = log(0.4D1 * t263)
      t266 = t265 ** 2
      t269 = log(0.4D1 * t263 * t57)
      t270 = t269 ** 2
      t285 = (0.90D2 * wd * (-t9 / 0.2D1 + t16 / 0.2D1 + t22 / 0.2D1 - t
     #29 / 0.2D1) + t37 * (t8 - t15 - t21 + t28)) * t41 * t43 / 0.20D2 +
     # 0.9D1 / 0.2D1 * wd * (-t51 - t55 - t66 * t73 - t78 + t81 + t84 * 
     #t73 + t90 + t96) * t41 * t43 * t100 - (0.90D2 * wd * (-t108 / 0.2D
     #1 + t112 / 0.2D1 - t116 / 0.2D1 + t120 / 0.2D1) + t37 * (t107 - t1
     #11 + t115 - t119)) * t43 * t100 / 0.20D2 - (t37 * (-t133 / 0.2D1 +
     # t136 / 0.2D1) + 0.90D2 * wd * (-t136 * t135 / 0.6D1 + t133 * t132
     # / 0.6D1) + t152 * (t132 - t135)) * t43 / 0.20D2 - (t37 * (t160 / 
     #0.2D1 - t164 / 0.2D1) + 0.90D2 * wd * (t164 * t163 / 0.6D1 - t160 
     #* t159 / 0.6D1) + t152 * (-t159 + t163)) * t41 / 0.40D2 - 0.9D1 / 
     #0.2D1 * t187 * lh + wd * t195 / 0.40D2 - 0.9D1 / 0.4D1 * wd + 0.9D
     #1 / 0.4D1 * t182 - 0.9D1 / 0.8D1 * t185 + 0.3D1 / 0.8D1 * t202 + t
     #205 * t150 / 0.40D2 + 0.9D1 / 0.2D1 * (0.4D1 * wd - 0.3D1 * t182 +
     # t185 - t202 / 0.6D1) * lh - t187 * t150 / 0.40D2 - t205 * t195 / 
     #0.40D2 - wd * (t218 + 0.60D2 * t219 + 0.480D3 * lh * zeta3 - 0.60D
     #2 * t148 * t146) / 0.40D2 - 0.3D1 / 0.32D2 * t228 * wd - (0.90D2 *
     # wd * (-t237 * t73 / 0.2D1 - t242 / 0.2D1 - t246 / 0.2D1 + t249 / 
     #0.2D1) + t37 * (t236 * t73 + t241 + t245 - t248) + t152 * (-0.1D1 
     #- t73)) * t41 * t100 / 0.40D2 + (t37 * (-t266 / 0.2D1 + t270 / 0.2
     #D1) + 0.90D2 * wd * (-t270 * t269 / 0.6D1 + t266 * t265 / 0.6D1) +
     # t152 * (t265 - t269)) * t100 / 0.40D2
      t286 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t285)
      rrgg2gghsoftt8s4e1 = t286 * t285

      end function



      doubleprecision function rrgg2gghsoftt8s4e0
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
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t11 = t10 ** 2
      t13 = log(0.4D1 * t5)
      t14 = t13 ** 2
      t22 = -0.180D3 * wd * lh + 0.90D2 * wd
      t26 = 0.1D1 / x1
      t29 = x3 * t1
      t32 = log(0.4D1 * t29 * t4)
      t33 = t4 * t7
      t36 = log(0.4D1 * t29 * t33)
      t37 = -0.1D1 + x3
      t38 = t4 * t37
      t41 = log(-0.4D1 * t29 * t38)
      t42 = x3 * t4
      t47 = log(-0.4D1 * t42 * t1 * t7 * t37)
      t50 = 0.1D1 / x3
      t54 = x2 * t1
      t57 = log(0.4D1 * t54 * t33)
      t59 = (-0.1D1 + x2) ** 2
      t60 = t4 * t59
      t64 = log(0.4D1 * t54 * t60 * t7)
      t67 = log(0.4D1 * t54 * t60)
      t70 = log(0.4D1 * t54 * t4)
      t73 = 0.1D1 / x2
      t79 = log(0.4D1 * t4)
      t80 = t79 * wd
      t81 = 0.2D1 * wd - t80
      t86 = t79 ** 2
      t87 = t86 * wd
      t89 = pi ** 2
      t91 = lh ** 2
      t93 = -0.30D2 * t89 + 0.180D3 * t91
      t116 = log(0.4D1 * t42)
      t117 = t116 ** 2
      t120 = log(-0.4D1 * t42 * t37)
      t121 = t120 ** 2
      t131 = x2 * x3
      t132 = t131 * t4
      t135 = (-0.1D1 + t131) ** 2
      t140 = log(-0.4D1 * t132 * t59 * t37 / t135)
      t141 = cos(t2)
      t143 = Sqrt(-t131 * t37)
      t147 = 0.1D1 / (-t131 - 0.1D1 + 0.2D1 * t141 * t143)
      t151 = log(-0.4D1 * t131 * t38)
      t154 = log(0.4D1 * t131 * t60)
      t156 = log(0.4D1 * t132)
      t166 = x2 * t4
      t168 = log(0.4D1 * t166)
      t169 = t168 ** 2
      t172 = log(0.4D1 * t166 * t59)
      t173 = t172 ** 2
      t183 = -(0.90D2 * wd * (-t11 / 0.2D1 + t14 / 0.2D1) + t22 * (t10 -
     # t13)) * t26 / 0.20D2 + 0.9D1 / 0.2D1 * wd * (t32 - t36 - t41 + t4
     #7) * t50 * t26 - 0.9D1 / 0.2D1 * wd * (t57 - t64 + t67 - t70) * t2
     #6 * t73 - 0.9D1 / 0.2D1 * t81 * lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 
     #0.4D1 * t80 - 0.9D1 / 0.8D1 * t87 + wd * t93 / 0.40D2 + 0.9D1 / 0.
     #2D1 * (0.3D1 * wd - 0.2D1 * t80 + t87 / 0.2D1) * lh - wd * (-0.240
     #D3 * zeta3 - 0.120D3 * t91 * lh + 0.60D2 * lh * t89) / 0.40D2 + 0.
     #3D1 / 0.8D1 * t86 * t79 * wd - t81 * t93 / 0.40D2 - (0.90D2 * wd *
     # (t117 / 0.2D1 - t121 / 0.2D1) + t22 * (-t116 + t120)) * t50 / 0.4
     #0D2 - (0.90D2 * wd * (t140 * t147 + t151 + t154 - t156) + t22 * (-
     #0.1D1 - t147)) * t50 * t73 / 0.40D2 + (0.90D2 * wd * (-t169 / 0.2D
     #1 + t173 / 0.2D1) + t22 * (t168 - t172)) * t73 / 0.40D2
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t183)
      rrgg2gghsoftt8s4e0 = t184 * t183

      end function



      doubleprecision function rrgg2gghsoftt8s4em1
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
      t7 = (-0.1D1 + x1) ** 2
      t10 = log(0.4D1 * t5 * t7)
      t12 = log(0.4D1 * t5)
      t22 = log(0.4D1 * t4)
      t23 = t22 * wd
      t29 = t22 ** 2
      t32 = pi ** 2
      t34 = lh ** 2
      t39 = x3 * t4
      t41 = log(0.4D1 * t39)
      t42 = -0.1D1 + x3
      t45 = log(-0.4D1 * t39 * t42)
      t48 = 0.1D1 / x3
      t51 = x2 * x3
      t52 = cos(t2)
      t54 = Sqrt(-t51 * t42)
      t61 = 0.1D1 / x2
      t65 = x2 * t4
      t67 = log(0.4D1 * t65)
      t69 = (-0.1D1 + x2) ** 2
      t72 = log(0.4D1 * t65 * t69)
      t77 = -0.9D1 / 0.2D1 * wd * (t10 - t12) / x1 - 0.9D1 / 0.2D1 * wd 
     #* lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.4D1 * t23 + 0.9D1 / 0.2D1 * 
     #(0.2D1 * wd - t23) * lh - 0.9D1 / 0.8D1 * t29 * wd - wd * (-0.30D2
     # * t32 + 0.180D3 * t34) / 0.40D2 - 0.9D1 / 0.4D1 * wd * (-t41 + t4
     #5) * t48 - 0.9D1 / 0.4D1 * wd * (-0.1D1 - 0.1D1 / (-t51 - 0.1D1 + 
     #0.2D1 * t52 * t54)) * t48 * t61 + 0.9D1 / 0.4D1 * wd * (t67 - t72)
     # * t61
      t78 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t77)
      rrgg2gghsoftt8s4em1 = t78 * t77

      end function



      doubleprecision function rrgg2gghsoftt8s4em2
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
      t5 = Sin(x4 * pi)
      t6 = t5 ** 2
      t8 = log(0.4D1 * t6)
      t11 = -0.9D1 / 0.4D1 * wd + 0.9D1 / 0.2D1 * wd * lh + 0.9D1 / 0.4D
     #1 * t8 * wd
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t11)
      rrgg2gghsoftt8s4em2 = t12 * t11

      end function



      doubleprecision function rrgg2gghsoftt8s4em3
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
     # 0.4D1 * wd)
      rrgg2gghsoftt8s4em3 = -0.9D1 / 0.4D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt8s4em4
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
      rrgg2gghsoftt8s4em4 = 0.0D0

      end function
