  
      subroutine rrgg2gght8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh81J1  
      doubleprecision rrgg2ggh81J2  
      doubleprecision rrgg2ggh81J3  
      doubleprecision rrgg2ggh81J4  
      doubleprecision rrgg2ggh81J5  
      doubleprecision rrgg2ggh81J6  
      doubleprecision rrgg2gght8s1e1  
      doubleprecision rrgg2gght8s1e0  
      doubleprecision rrgg2gght8s1em1  
      doubleprecision rrgg2gght8s1em2  
      doubleprecision rrgg2gght8s1em3  
      doubleprecision rrgg2gght8s1em4  
      doubleprecision rrgg2gght8s2e1  
      doubleprecision rrgg2gght8s2e0  
      doubleprecision rrgg2gght8s2em1  
      doubleprecision rrgg2gght8s2em2  
      doubleprecision rrgg2gght8s2em3  
      doubleprecision rrgg2gght8s2em4  
      doubleprecision rrgg2gght8s3e1  
      doubleprecision rrgg2gght8s3e0  
      doubleprecision rrgg2gght8s3em1  
      doubleprecision rrgg2gght8s3em2  
      doubleprecision rrgg2gght8s3em3  
      doubleprecision rrgg2gght8s3em4  
      doubleprecision rrgg2gght8s4e1  
      doubleprecision rrgg2gght8s4e0  
      doubleprecision rrgg2gght8s4em1  
      doubleprecision rrgg2gght8s4em2  
      doubleprecision rrgg2gght8s4em3  
      doubleprecision rrgg2gght8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght8s1e1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t6 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
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
      t68 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t89 = 0.1D1 / x3
      t92 = t24 * t21
      t94 = log(0.4D1 * t92)
      t95 = t94 ** 2
      t98 = t95 * t94
      t113 = t9 ** 2
      t114 = t11 ** 2
      t128 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
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
      t176 = z * t8 * t42 + t8
      t177 = t18 * t176
      t181 = 0.1D1 / x1
      t184 = t138 * t21
      t185 = t184 * t24
      t187 = log(0.4D1 * t185)
      t192 = t187 ** 2
      t202 = pi * t66
      t203 = t18 * t8
      t204 = t202 * t203
      t215 = x2 ** 2
      t216 = x3 * t215
      t217 = t216 * t138
      t220 = log(-0.4D1 * t217 * t147)
      t225 = t216 * t185
      t227 = log(0.4D1 * t225)
      t236 = 0.1D1 / x2
      t237 = t181 * t236
      t240 = t215 * t138
      t243 = log(0.4D1 * t240 * t92)
      t245 = t243 ** 2
      t263 = log(0.4D1 * t216 * t92)
      t265 = t263 ** 2
      t270 = log(-0.4D1 * t216 * t147)
      t271 = t270 * z
      t273 = t270 ** 2
      t297 = t215 * t21
      t300 = log(0.4D1 * t297 * t24)
      t305 = t300 ** 2
      t325 = ((-0.180D3 * t3 * lh + 0.90D2 * t6 + t8 * t13) * pi * t18 *
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
     # + (t165 - t151 * t8) * t42) + t173 * t177) * t89 * t181 / 0.1440D
     #4 + (t173 * t18 * (-t3 + t187 * t8) + 0.90D2 * t137 * (-t192 * t3 
     #/ 0.2D1 - t68 + t192 * t187 * t8 / 0.6D1 + t187 * t6) - t204 - 0.1
     #80D3 * t163 * t18 * (t187 * t3 - t192 * t8 / 0.2D1 - t6)) * t181 /
     # 0.1440D4 - (0.90D2 * t137 * ((t165 - t220 * z * t8) * t42 + t3 - 
     #t227 * t8) - 0.180D3 * t163 * t177) * t89 * t237 / 0.720D3 + (0.90
     #D2 * t137 * (t243 * t3 - t245 * t8 / 0.2D1 - t6) - 0.180D3 * t163 
     #* t18 * (-t3 + t243 * t8) - t173 * t203) * t181 * t236 / 0.720D3 +
     # (0.90D2 * t137 * (t263 * t3 - t265 * t8 / 0.2D1 - t6 - (-t271 * t
     #3 + t273 * z * t8 / 0.2D1 + t157) * t42) - 0.180D3 * t163 * t18 * 
     #(-t3 + t263 * t8 - (t165 - t271 * t8) * t42) - t173 * t18 * t176) 
     #* t89 * t236 / 0.1440D4 - (t173 * t18 * (t3 - t300 * t8) + 0.90D2 
     #* t137 * (t305 * t3 / 0.2D1 + t68 - t305 * t300 * t8 / 0.6D1 - t30
     #0 * t6) + t204 - 0.180D3 * t163 * t18 * (-t300 * t3 + t305 * t8 / 
     #0.2D1 + t6)) * t236 / 0.1440D4
      t326 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t325)
      t328 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t325)
      t330 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t325)
      t332 = t2 * x1
      t333 = -0.1D1 + x1
      t334 = x1 * z
      t335 = 0.1D1 - x1 + t334
      t336 = 0.1D1 / t335
      t338 = t2 * t333 * t336
      t339 = t1 ** 2
      t340 = s * t339
      t342 = x1 * t333 * t336
      t343 = t340 * t342
      t344 = t139 * t21
      t345 = t333 ** 2
      t346 = t24 * t345
      t348 = t346 * t336 * t29
      t351 = log(-0.4D1 * t344 * t348)
      t352 = t351 * t335
      t353 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338, t
     #332, 0.0D0, -t343)
      t356 = t351 ** 2
      t358 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338, t
     #332, 0.0D0, -t343)
      t359 = z * t358
      t362 = z * t335
      t363 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338, t
     #332, 0.0D0, -t343)
      t366 = x3 * x1
      t367 = 0.2D1 * t366
      t368 = x3 * t335
      t370 = Sqrt(-t368 * t28)
      t373 = x1 * t23
      t374 = t366 * t23
      t376 = 0.2D1 * t139 * z
      t377 = t138 * t23
      t378 = t377 * x3
      t379 = t366 * z
      t380 = 0.3D1 * t379
      t381 = -z - t139 + t367 - x3 + t334 + 0.2D1 * t36 * t370 - t373 + 
     #t374 + t376 - t378 - t380
      t382 = 0.1D1 / t381
      t384 = t346 * t336
      t387 = log(0.4D1 * t344 * t384)
      t389 = t387 ** 2
      t395 = t362 * t353
      t407 = t18 * (-t358 - t362 * t358 * t382)
      t415 = log(0.4D1 * t184 * t384)
      t420 = t415 ** 2
      t423 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338, t
     #332, 0.0D0, -t343)
      t431 = t18 * t358
      t443 = t216 * t184
      t446 = log(-0.4D1 * t443 * t348)
      t451 = t345 * t336
      t455 = log(0.4D1 * t217 * t92 * t451)
      t466 = t240 * t21
      t469 = log(0.4D1 * t466 * t384)
      t471 = t469 ** 2
      t487 = -(0.90D2 * t137 * (-(-t352 * z * t353 + t356 * t335 * t359 
     #/ 0.2D1 + t362 * t363) * t382 + t387 * t353 - t389 * t358 / 0.2D1 
     #- t363) - 0.180D3 * t163 * t18 * (-(t395 - t352 * t359) * t382 - t
     #353 + t387 * t358) + t173 * t407) * t89 * t181 / 0.1440D4 + (t173 
     #* t18 * (t353 - t415 * t358) + 0.90D2 * t137 * (t420 * t353 / 0.2D
     #1 + t423 - t420 * t415 * t358 / 0.6D1 - t415 * t363) + t202 * t431
     # - 0.180D3 * t163 * t18 * (-t415 * t353 + t420 * t358 / 0.2D1 + t3
     #63)) * t181 / 0.1440D4 - (0.90D2 * t137 * (-(t395 - t446 * t335 * 
     #t359) * t382 - t353 + t455 * t358) - 0.180D3 * t163 * t407) * t89 
     #* t237 / 0.720D3 + (0.90D2 * t137 * (-t469 * t353 + t471 * t358 / 
     #0.2D1 + t363) - 0.180D3 * t163 * t18 * (t353 - t469 * t358) + t173
     # * t431) * t181 * t236 / 0.720D3
      t488 = FJET(XB1, XB2, s, 0.0D0, t332, -t338, 0.0D0, -t343, t487)
      t490 = x2 * s
      t491 = t490 * t1
      t492 = -0.1D1 + x2
      t493 = t492 * s
      t494 = t493 * t1
      t495 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t491, -t494, 0.
     #0D0, 0.0D0, 0.0D0)
      t496 = t92 * t492
      t499 = log(-0.4D1 * t217 * t496)
      t500 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t491, -t494, 0.
     #0D0, 0.0D0, 0.0D0)
      t505 = t18 * t500
      t511 = (0.90D2 * t137 * (-t495 + t499 * t500) + 0.180D3 * t163 * t
     #505) * t89 * t237 / 0.720D3
      t514 = log(-0.4D1 * t240 * t496)
      t516 = t514 ** 2
      t519 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t491, -t494, 0.
     #0D0, 0.0D0, 0.0D0)
      t528 = t173 * t505
      t532 = (0.90D2 * t137 * (-t514 * t495 + t516 * t500 / 0.2D1 + t519
     #) - 0.180D3 * t163 * t18 * (t495 - t514 * t500) + t528) * t181 * t
     #236 / 0.720D3
      t535 = log(-0.4D1 * t216 * t496)
      t537 = t535 ** 2
      t551 = (0.90D2 * t137 * (-t535 * t495 + t537 * t500 / 0.2D1 + t519
     #) - 0.180D3 * t163 * t18 * (t495 - t535 * t500) + t528) * t89 * t2
     #36 / 0.1440D4
      t552 = t24 * t492
      t555 = log(-0.4D1 * t297 * t552)
      t557 = -t495 + t555 * t500
      t560 = t555 ** 2
      t563 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, t491, -t494, 0.
     #0D0, 0.0D0, 0.0D0)
      t568 = -t560 * t495 / 0.2D1 - t563 + t560 * t555 * t500 / 0.6D1 + 
     #t555 * t519
      t571 = t202 * t505
      t575 = t555 * t495 - t560 * t500 / 0.2D1 - t519
      t582 = -t511 + t532 + t551 - (t173 * t18 * t557 + 0.90D2 * t137 * 
     #t568 - t571 - 0.180D3 * t163 * t18 * t575) * t236 / 0.1440D4
      t583 = FJET(XB1, XB2, s, 0.0D0, t491, 0.0D0, -t494, 0.0D0, t582)
      t585 = x2 * x3
      t588 = Sqrt(x3 * t492 * t28)
      t589 = t35 * t588
      t591 = 0.2D1 * t589 * x2
      t593 = 0.1D1 - x3 + t585
      t594 = 0.1D1 / t593
      t596 = t2 * (0.1D1 - x3 - x2 + t585 + t216 + t591) * t594
      t601 = t2 * x2 * (-0.1D1 + t585 + 0.2D1 * t589) * t594
      t602 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t601, t596, 0.
     #0D0, 0.0D0, 0.0D0)
      t603 = t593 ** 2
      t604 = 0.1D1 / t603
      t606 = t552 * t28 * t604
      t609 = log(0.4D1 * t443 * t606)
      t610 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t601, t596, 0.
     #0D0, 0.0D0, 0.0D0)
      t613 = x2 * z
      t614 = t613 - x2 - z
      t616 = t585 * z
      t618 = z * t215 * x3
      t625 = 0.1D1 / (-t613 - t616 + t618 + x2 + z + x3 - t216 - t591 - 
     #0.2D1 * t36 * t588 + 0.2D1 * t36 * t588 * x2)
      t629 = t163 * t18
      t631 = t610 * t614 * t625
      t641 = log(0.4D1 * t216 * t21 * t606)
      t643 = t641 ** 2
      t646 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t601, t596, 0.
     #0D0, 0.0D0, 0.0D0)
      t664 = -(-0.90D2 * t137 * (t602 - t609 * t610) * t614 * t625 + 0.1
     #80D3 * t629 * t631) * t89 * t237 / 0.720D3 + (0.90D2 * t137 * (-t6
     #41 * t602 + t643 * t610 / 0.2D1 + t646) * t614 * t625 - 0.180D3 * 
     #t629 * (t602 - t641 * t610) * t614 * t625 + t173 * t18 * t631) * t
     #89 * t236 / 0.1440D4
      t665 = FJET(XB1, XB2, s, 0.0D0, t596, 0.0D0, -t601, 0.0D0, t664)
      t667 = t1 * t333
      t669 = t493 * t667 * t336
      t670 = t490 * t667
      t672 = t340 * t492 * t342
      t673 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t670, t669, t3
     #32, 0.0D0, t672)
      t675 = t346 * t336 * t492
      t678 = log(-0.4D1 * t443 * t675)
      t679 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t670, t669, t3
     #32, 0.0D0, t672)
      t684 = t18 * t679
      t689 = (0.90D2 * t137 * (t673 - t678 * t679) - 0.180D3 * t163 * t6
     #84) * t89 * t237
      t692 = log(-0.4D1 * t466 * t675)
      t694 = t692 ** 2
      t697 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t670, t669, t3
     #32, 0.0D0, t672)
      t698 = t692 * t673 - t694 * t679 / 0.2D1 - t697
      t702 = -t673 + t692 * t679
      t706 = t173 * t684
      t711 = -t689 / 0.720D3 + (0.90D2 * t137 * t698 - 0.180D3 * t163 * 
     #t18 * t702 - t706) * t181 * t236 / 0.720D3
      t712 = FJET(XB1, XB2, s, 0.0D0, t669, t332, -t670, t672, t711)
      t714 = FJET(XB1, XB2, s, 0.0D0, -t494, 0.0D0, t491, 0.0D0, t582)
      t716 = FJET(XB1, XB2, s, 0.0D0, -t338, t332, 0.0D0, -t343, t487)
      t718 = FJET(XB1, XB2, s, 0.0D0, -t601, 0.0D0, t596, 0.0D0, t664)
      t720 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t325)
      t722 = FJET(XB1, XB2, s, t332, 0.0D0, 0.0D0, -t338, -t343, t487)
      t724 = t326 * t325 + t328 * t325 + t330 * t325 + t488 * t487 + t58
     #3 * t582 + t665 * t664 + t712 * t711 + t714 * t582 + t716 * t487 +
     # t718 * t664 + t720 * t325 + t722 * t487
      t725 = FJET(XB1, XB2, s, t332, -t670, 0.0D0, t669, t672, t711)
      t740 = -t511 + t532 + t551 - (t173 * t18 * t557 + 0.90D2 * t137 * 
     #t568 - t571 - 0.180D3 * t163 * t18 * t575) * t236 / 0.1440D4
      t741 = FJET(XB1, XB2, s, t491, 0.0D0, -t494, 0.0D0, 0.0D0, t740)
      t743 = FJET(XB1, XB2, s, t596, 0.0D0, -t601, 0.0D0, 0.0D0, t664)
      t745 = FJET(XB1, XB2, s, t669, 0.0D0, -t670, t332, t672, t711)
      t748 = t332 * t585 * t594
      t749 = t2 * t333
      t750 = t334 * t216
      t751 = t492 * t28
      t753 = Sqrt(t368 * t751)
      t754 = t35 * t753
      t756 = 0.2D1 * t754 * x2
      t758 = x1 * t215 * x3
      t762 = t749 * (-x2 + t585 + t750 + t216 + t756 + 0.1D1 - x3 - t758
     #) * t336 * t594
      t766 = t28 * s * t1 * x1 * t594
      t772 = t749 * x2 * (-0.1D1 + t585 + x1 - t366 - t334 + t379 + 0.2D
     #1 * t754) * t336 * t594
      t773 = x1 * x2
      t774 = t773 * z
      t775 = x2 - t773 + z - t613 + t774
      t776 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t772, -t762, -t
     #766, t748, t672)
      t782 = log(0.4D1 * t225 * t451 * t751 * t604)
      t784 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t772, -t762, -t
     #766, t748, t672)
      t788 = x2 * t138
      t798 = -x3 - t788 + 0.2D1 * t773 - x2 + t376 - t378 - t380 - z + t
     #773 * t23 + 0.2D1 * t788 * z - t788 * t23 + t139 * x2 + 0.2D1 * t3
     #6 * t753 - t366 * x2 - t139 + t367 - t373
      t816 = -t758 - 0.3D1 * t774 + t334 + t756 + t750 + 0.2D1 * t366 * 
     #t613 - t366 * x2 * t23 - 0.2D1 * t139 * t613 + t377 * t585 - 0.2D1
     # * t754 * t773 - 0.2D1 * t36 * t753 * x2 + t216 + t613 + t374 + t6
     #16 - t618 + 0.2D1 * t36 * t753 * x1 * x2
      t818 = 0.1D1 / (t798 + t816)
      t827 = 0.90D2 * t137 * (t775 * t776 - t782 * t775 * t784) * t335 *
     # t818 - 0.180D3 * t629 * t775 * t784 * t335 * t818
      t830 = t827 * t89 * t237 / 0.720D3
      t831 = FJET(XB1, XB2, s, t748, -t762, -t766, t772, t672, -t830)
      t834 = t89 * t181 * t236
      t837 = FJET(XB1, XB2, s, t772, -t766, -t762, t748, t672, -t830)
      t841 = FJET(XB1, XB2, s, -t494, 0.0D0, t491, 0.0D0, 0.0D0, t740)
      t843 = FJET(XB1, XB2, s, -t338, 0.0D0, 0.0D0, t332, -t343, t487)
      t856 = -t689 / 0.720D3 + (0.90D2 * t137 * t698 - 0.180D3 * t163 * 
     #t18 * t702 - t706) * t181 * t236 / 0.720D3
      t857 = FJET(XB1, XB2, s, -t670, t332, t669, 0.0D0, t672, t856)
      t859 = FJET(XB1, XB2, s, -t601, 0.0D0, t596, 0.0D0, 0.0D0, t664)
      t861 = FJET(XB1, XB2, s, -t766, t772, t748, -t762, t672, -t830)
      t865 = FJET(XB1, XB2, s, -t762, t748, t772, -t766, t672, -t830)
      t869 = t725 * t711 + t741 * t740 + t743 * t664 + t745 * t711 - t83
     #1 * t827 * t834 / 0.720D3 - t837 * t827 * t834 / 0.720D3 + t841 * 
     #t740 + t843 * t487 + t857 * t856 + t859 * t664 - t861 * t827 * t83
     #4 / 0.720D3 - t865 * t827 * t834 / 0.720D3
      rrgg2gght8s1e1 = t724 + t869

      end function



      doubleprecision function rrgg2gght8s1e0
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
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
      t41 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t52 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t54 = pi ** 2
      t56 = lh ** 2
      t58 = -0.30D2 * t54 + 0.180D3 * t56
      t67 = 0.1D1 / x3
      t70 = t12 * t9
      t72 = log(0.4D1 * t70)
      t74 = t72 ** 2
      t89 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t103 = pi * t6
      t104 = x2 ** 2
      t105 = t104 * x3
      t108 = log(0.4D1 * t105 * t70)
      t110 = z * t41
      t111 = t70 * t18
      t114 = log(-0.4D1 * t105 * t111)
      t122 = pi * lh
      t125 = -z * t3 * t32 - t3
      t131 = 0.1D1 / x2
      t134 = t104 * t9
      t137 = log(0.4D1 * t134 * t12)
      t139 = t137 ** 2
      t150 = pi * t58
      t151 = t6 * t3
      t152 = t150 * t151
      t156 = x1 ** 2
      t157 = x3 * t156
      t160 = log(0.4D1 * t157 * t70)
      t164 = log(-0.4D1 * t157 * t111)
      t172 = -t125
      t178 = 0.1D1 / x1
      t183 = t67 * t178 * t131
      t186 = t104 * t156
      t189 = log(0.4D1 * t186 * t70)
      t200 = t156 * t9
      t203 = log(0.4D1 * t200 * t12)
      t205 = t203 ** 2
      t219 = (0.90D2 * t3 * pi * t6 * (-t16 / 0.2D1 - t23 * z * t32 / 0.
     #2D1) + (-0.180D3 * t3 * lh + 0.90D2 * t41) * pi * t6 * (t15 + t22 
     #* z * t32) + (-0.180D3 * t41 * lh + 0.90D2 * t52 + t3 * t58) * pi 
     #* t6 * (-0.1D1 - z * t32)) * t67 / 0.2880D4 - (-0.180D3 * (-t72 * 
     #t41 + t74 * t3 / 0.2D1 + t52) * lh + t3 * (-0.240D3 * zeta3 - 0.12
     #0D3 * t56 * lh + 0.60D2 * lh * t54) + 0.45D2 * t74 * t41 + 0.90D2 
     #* t89 - 0.15D2 * t74 * t72 * t3 - 0.90D2 * t72 * t52 + (t41 - t72 
     #* t3) * t58) * pi * t6 / 0.2880D4 + (0.90D2 * t103 * (-t41 + t108 
     #* t3 - (t110 - t114 * z * t3) * t32) - 0.180D3 * t122 * t6 * t125)
     # * t67 * t131 / 0.1440D4 - (0.90D2 * t103 * (-t137 * t41 + t139 * 
     #t3 / 0.2D1 + t52) - 0.180D3 * t122 * t6 * (t41 - t137 * t3) + t152
     #) * t131 / 0.1440D4 - (0.90D2 * t103 * (t41 - t160 * t3 + (t110 - 
     #t164 * z * t3) * t32) - 0.180D3 * t122 * t6 * t172) * t67 * t178 /
     # 0.1440D4 - t103 * t172 * t183 / 0.8D1 + (0.90D2 * t103 * (-t41 + 
     #t189 * t3) + 0.180D3 * t122 * t151) * t178 * t131 / 0.720D3 + (0.9
     #0D2 * t103 * (t203 * t41 - t205 * t3 / 0.2D1 - t52) - 0.180D3 * t1
     #22 * t6 * (-t41 + t203 * t3) - t152) * t178 / 0.1440D4
      t220 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t219)
      t222 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t219)
      t224 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t219)
      t226 = t2 * x1
      t227 = -0.1D1 + x1
      t228 = x1 * z
      t229 = 0.1D1 - x1 + t228
      t230 = 0.1D1 / t229
      t232 = t2 * t227 * t230
      t233 = t1 ** 2
      t234 = s * t233
      t236 = x1 * t227 * t230
      t237 = t234 * t236
      t238 = z * t229
      t239 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t232, t
     #226, 0.0D0, -t237)
      t241 = t157 * t9
      t242 = t227 ** 2
      t243 = t12 * t242
      t248 = log(-0.4D1 * t241 * t243 * t230 * t18)
      t250 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t232, t
     #226, 0.0D0, -t237)
      t254 = x3 * x1
      t255 = 0.2D1 * t254
      t256 = x3 * t229
      t258 = Sqrt(-t256 * t17)
      t261 = x1 * t11
      t262 = t254 * t11
      t264 = 0.2D1 * t157 * z
      t265 = t156 * t11
      t266 = t265 * x3
      t267 = t254 * z
      t268 = 0.3D1 * t267
      t269 = -z - t157 + t255 - x3 + t228 + 0.2D1 * t26 * t258 - t261 + 
     #t262 + t264 - t266 - t268
      t270 = 0.1D1 / t269
      t272 = t243 * t230
      t275 = log(0.4D1 * t241 * t272)
      t282 = -t250 - t238 * t250 * t270
      t293 = t186 * t9
      t296 = log(0.4D1 * t293 * t272)
      t301 = t6 * t250
      t310 = log(0.4D1 * t200 * t272)
      t312 = t310 ** 2
      t315 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t232, t
     #226, 0.0D0, -t237)
      t328 = -(0.90D2 * t103 * (-(t238 * t239 - t248 * t229 * z * t250) 
     #* t270 - t239 + t275 * t250) - 0.180D3 * t122 * t6 * t282) * t67 *
     # t178 / 0.1440D4 - t103 * t282 * t183 / 0.8D1 + (0.90D2 * t103 * (
     #t239 - t296 * t250) - 0.180D3 * t122 * t301) * t178 * t131 / 0.720
     #D3 + (0.90D2 * t103 * (-t310 * t239 + t312 * t250 / 0.2D1 + t315) 
     #- 0.180D3 * t122 * t6 * (t239 - t310 * t250) + t150 * t301) * t178
     # / 0.1440D4
      t329 = FJET(XB1, XB2, s, 0.0D0, t226, -t232, 0.0D0, -t237, t328)
      t331 = x2 * s
      t332 = t331 * t1
      t333 = -0.1D1 + x2
      t334 = t333 * s
      t335 = t334 * t1
      t336 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t332, -t335, 0.
     #0D0, 0.0D0, 0.0D0)
      t337 = t70 * t333
      t340 = log(-0.4D1 * t105 * t337)
      t341 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t332, -t335, 0.
     #0D0, 0.0D0, 0.0D0)
      t346 = t6 * t341
      t348 = 0.180D3 * t122 * t346
      t352 = (0.90D2 * t103 * (t336 - t340 * t341) - t348) * t67 * t131 
     #/ 0.1440D4
      t353 = t12 * t333
      t356 = log(-0.4D1 * t134 * t353)
      t358 = t356 ** 2
      t361 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t332, -t335, 0.
     #0D0, 0.0D0, 0.0D0)
      t362 = t356 * t336 - t358 * t341 / 0.2D1 - t361
      t366 = -t336 + t356 * t341
      t370 = t150 * t346
      t376 = t103 * t341 * t183 / 0.8D1
      t379 = log(-0.4D1 * t186 * t337)
      t387 = (0.90D2 * t103 * (t336 - t379 * t341) - t348) * t178 * t131
     # / 0.720D3
      t388 = t352 - (0.90D2 * t103 * t362 - 0.180D3 * t122 * t6 * t366 -
     # t370) * t131 / 0.1440D4 + t376 + t387
      t389 = FJET(XB1, XB2, s, 0.0D0, t332, 0.0D0, -t335, 0.0D0, t388)
      t391 = x2 * x3
      t394 = Sqrt(x3 * t333 * t17)
      t395 = t25 * t394
      t397 = 0.2D1 * t395 * x2
      t399 = 0.1D1 - x3 + t391
      t400 = 0.1D1 / t399
      t402 = t2 * (0.1D1 - x3 - x2 + t391 + t105 + t397) * t400
      t407 = t2 * x2 * (-0.1D1 + t391 + 0.2D1 * t395) * t400
      t408 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t407, t402, 0.
     #0D0, 0.0D0, 0.0D0)
      t410 = t399 ** 2
      t416 = log(0.4D1 * t105 * t9 * t353 * t17 / t410)
      t417 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t407, t402, 0.
     #0D0, 0.0D0, 0.0D0)
      t420 = x2 * z
      t421 = t420 - x2 - z
      t423 = t391 * z
      t425 = z * t104 * x3
      t432 = 0.1D1 / (-t420 - t423 + t425 + x2 + z + x3 - t105 - t397 - 
     #0.2D1 * t26 * t394 + 0.2D1 * t26 * t394 * x2)
      t437 = t417 * t421
      t451 = (0.90D2 * t103 * (t408 - t416 * t417) * t421 * t432 - 0.180
     #D3 * t122 * t6 * t437 * t432) * t67 * t131 / 0.1440D4 + t103 * t43
     #7 * t432 * t67 * t178 * t131 / 0.8D1
      t452 = FJET(XB1, XB2, s, 0.0D0, t402, 0.0D0, -t407, 0.0D0, t451)
      t454 = t1 * t227
      t456 = t334 * t454 * t230
      t457 = t331 * t454
      t459 = t234 * t333 * t236
      t460 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t457, t456, t2
     #26, 0.0D0, t459)
      t463 = t103 * t460 * t183 / 0.8D1
      t464 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t457, t456, t2
     #26, 0.0D0, t459)
      t470 = log(-0.4D1 * t293 * t12 * t230 * t242 * t333)
      t472 = -t464 + t470 * t460
      t477 = 0.180D3 * t122 * t6 * t460
      t482 = -t463 + (0.90D2 * t103 * t472 + t477) * t178 * t131 / 0.720
     #D3
      t483 = FJET(XB1, XB2, s, 0.0D0, t456, t226, -t457, t459, t482)
      t485 = FJET(XB1, XB2, s, 0.0D0, -t335, 0.0D0, t332, 0.0D0, t388)
      t487 = FJET(XB1, XB2, s, 0.0D0, -t232, t226, 0.0D0, -t237, t328)
      t489 = FJET(XB1, XB2, s, 0.0D0, -t407, 0.0D0, t402, 0.0D0, t451)
      t491 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t219)
      t493 = FJET(XB1, XB2, s, t226, 0.0D0, 0.0D0, -t232, -t237, t328)
      t495 = t220 * t219 + t222 * t219 + t224 * t219 + t329 * t328 + t38
     #9 * t388 + t452 * t451 + t483 * t482 + t485 * t388 + t487 * t328 +
     # t489 * t451 + t491 * t219 + t493 * t328
      t496 = FJET(XB1, XB2, s, t226, -t457, 0.0D0, t456, t459, t482)
      t508 = t352 - (0.90D2 * t103 * t362 - 0.180D3 * t122 * t6 * t366 -
     # t370) * t131 / 0.1440D4 + t376 + t387
      t509 = FJET(XB1, XB2, s, t332, 0.0D0, -t335, 0.0D0, 0.0D0, t508)
      t511 = FJET(XB1, XB2, s, t402, 0.0D0, -t407, 0.0D0, 0.0D0, t451)
      t513 = FJET(XB1, XB2, s, t456, 0.0D0, -t457, t226, t459, t482)
      t516 = t226 * t391 * t400
      t517 = t2 * t227
      t518 = t228 * t105
      t521 = Sqrt(t256 * t333 * t17)
      t522 = t25 * t521
      t524 = 0.2D1 * t522 * x2
      t526 = x1 * t104 * x3
      t530 = t517 * (-x2 + t391 + t518 + t105 + t524 + 0.1D1 - x3 - t526
     #) * t230 * t400
      t534 = t17 * s * t1 * x1 * t400
      t540 = t517 * x2 * (-0.1D1 + t391 + x1 - t254 - t228 + t267 + 0.2D
     #1 * t522) * t230 * t400
      t541 = x1 * x2
      t542 = t541 * z
      t543 = x2 - t541 + z - t420 + t542
      t544 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t540, -t530, -t
     #534, t516, t459)
      t564 = x2 * t156
      t565 = -x2 + 0.2D1 * t26 * t521 * x1 * x2 + t518 + 0.2D1 * t254 * 
     #t420 - t254 * x2 * t11 - 0.2D1 * t157 * t420 + t265 * t391 - 0.2D1
     # * t522 * t541 - 0.2D1 * t26 * t521 * x2 + t228 - t157 + t255 - t2
     #61 + t105 + t420 + 0.2D1 * t541 - t564
      t575 = t524 - t526 - 0.3D1 * t542 + t541 * t11 + 0.2D1 * t564 * z 
     #- t564 * t11 + t157 * x2 + 0.2D1 * t26 * t521 - t254 * x2 + t262 +
     # t264 - t266 - t268 + t423 - t425 - z - x3
      t579 = t229 / (t565 + t575) * t183
      t581 = t103 * t543 * t544 * t579 / 0.8D1
      t582 = FJET(XB1, XB2, s, t516, -t530, -t534, t540, t459, -t581)
      t585 = t6 * t543 * t544
      t589 = FJET(XB1, XB2, s, t540, -t534, -t530, t516, t459, -t581)
      t594 = FJET(XB1, XB2, s, -t335, 0.0D0, t332, 0.0D0, 0.0D0, t508)
      t596 = FJET(XB1, XB2, s, -t232, 0.0D0, 0.0D0, t226, -t237, t328)
      t605 = -t463 + (0.90D2 * t103 * t472 + t477) * t178 * t131 / 0.720
     #D3
      t606 = FJET(XB1, XB2, s, -t457, t226, t456, 0.0D0, t459, t605)
      t608 = FJET(XB1, XB2, s, -t407, 0.0D0, t402, 0.0D0, 0.0D0, t451)
      t610 = FJET(XB1, XB2, s, -t534, t540, t516, -t530, t459, -t581)
      t615 = FJET(XB1, XB2, s, -t530, t516, t540, -t534, t459, -t581)
      t620 = t496 * t482 + t509 * t508 + t511 * t451 + t513 * t482 - t58
     #2 * pi * t585 * t579 / 0.8D1 - t589 * pi * t585 * t579 / 0.8D1 + t
     #594 * t508 + t596 * t328 + t606 * t605 + t608 * t451 - t610 * pi *
     # t585 * t579 / 0.8D1 - t615 * pi * t585 * t579 / 0.8D1
      rrgg2gght8s1e0 = t495 + t620

      end function



      doubleprecision function rrgg2gght8s1em1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
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
      t38 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t47 = 0.1D1 / x3
      t52 = log(0.4D1 * t12 * t9)
      t59 = t52 ** 2
      t62 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t64 = pi ** 2
      t66 = lh ** 2
      t74 = pi * t6
      t77 = -z * t3 * t30 - t3
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
      t121 = (0.90D2 * t4 * t6 * (t15 + t21 * z * t30) + (-0.180D3 * t3 
     #* lh + 0.90D2 * t38) * pi * t6 * (-0.1D1 - z * t30)) * t47 / 0.288
     #0D4 - (-0.180D3 * (t38 - t52 * t3) * lh - 0.90D2 * t52 * t38 + 0.4
     #5D2 * t59 * t3 + 0.90D2 * t62 + t3 * (-0.30D2 * t64 + 0.180D3 * t6
     #6)) * pi * t6 / 0.2880D4 + t74 * t77 * t47 * t79 / 0.16D2 - (0.90D
     #2 * t74 * (t38 - t87 * t3) - t95) * t79 / 0.1440D4 - t4 * t6 * t99
     # * t79 / 0.8D1 + (0.90D2 * t74 * (-t38 + t108 * t3) + t95) * t99 /
     # 0.1440D4 + t74 * t77 * t47 * t99 / 0.16D2
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
      t140 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t134, t
     #128, 0.0D0, -t139)
      t145 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t134, t
     #128, 0.0D0, -t139)
      t147 = t129 ** 2
      t151 = log(0.4D1 * t105 * t12 * t132 * t147)
      t163 = t104 * x3
      t164 = x3 * x1
      t168 = Sqrt(-x3 * t131 * t16)
      t179 = -z - t163 + 0.2D1 * t164 - x3 + t130 + 0.2D1 * t24 * t168 -
     # x1 * t11 + t164 * t11 + 0.2D1 * t163 * z - t104 * t11 * x3 - 0.3D
     #1 * t164 * z
      t188 = t74 * t140 * t99 * t79 / 0.8D1 + (0.90D2 * t74 * (t145 - t1
     #51 * t140) - 0.180D3 * t92 * t6 * t140) * t99 / 0.1440D4 - t74 * (
     #-t140 - z * t131 * t140 / t179) * t47 * t99 / 0.16D2
      t189 = FJET(XB1, XB2, s, 0.0D0, t128, -t134, 0.0D0, -t139, t188)
      t191 = x2 * s
      t192 = t191 * t1
      t193 = -0.1D1 + x2
      t194 = t193 * s
      t195 = t194 * t1
      t196 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t192, -t195, 0.
     #0D0, 0.0D0, 0.0D0)
      t200 = t74 * t196 * t47 * t79 / 0.16D2
      t201 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t192, -t195, 0.
     #0D0, 0.0D0, 0.0D0)
      t205 = log(-0.4D1 * t84 * t12 * t193)
      t207 = -t201 + t205 * t196
      t212 = 0.180D3 * t92 * t6 * t196
      t219 = t74 * t196 * t99 * t79 / 0.8D1
      t220 = t200 - (0.90D2 * t74 * t207 + t212) * t79 / 0.1440D4 + t219
      t221 = FJET(XB1, XB2, s, 0.0D0, t192, 0.0D0, -t195, 0.0D0, t220)
      t223 = x2 * x3
      t224 = t83 * x3
      t227 = Sqrt(x3 * t193 * t16)
      t228 = t23 * t227
      t230 = 0.2D1 * t228 * x2
      t233 = 0.1D1 / (0.1D1 - x3 + t223)
      t235 = t2 * (0.1D1 - x3 - x2 + t223 + t224 + t230) * t233
      t240 = t2 * x2 * (-0.1D1 + t223 + 0.2D1 * t228) * t233
      t241 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t240, t235, 0.
     #0D0, 0.0D0, 0.0D0)
      t243 = x2 * z
      t257 = (t243 - x2 - z) / (-t243 - t223 * z + z * t83 * x3 + x2 + z
     # + x3 - t224 - t230 - 0.2D1 * t24 * t227 + 0.2D1 * t24 * t227 * x2
     #) * t47 * t79
      t259 = t74 * t241 * t257 / 0.16D2
      t260 = FJET(XB1, XB2, s, 0.0D0, t235, 0.0D0, -t240, 0.0D0, t259)
      t262 = t6 * t241
      t266 = t1 * t129
      t268 = t194 * t266 * t132
      t269 = t191 * t266
      t271 = t136 * t193 * t138
      t272 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t269, t268, t1
     #28, 0.0D0, t271)
      t274 = t272 * t99 * t79
      t276 = t74 * t274 / 0.8D1
      t277 = FJET(XB1, XB2, s, 0.0D0, t268, t128, -t269, t271, -t276)
      t282 = FJET(XB1, XB2, s, 0.0D0, -t195, 0.0D0, t192, 0.0D0, t220)
      t284 = FJET(XB1, XB2, s, 0.0D0, -t134, t128, 0.0D0, -t139, t188)
      t286 = FJET(XB1, XB2, s, 0.0D0, -t240, 0.0D0, t235, 0.0D0, t259)
      t291 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t293 = FJET(XB1, XB2, s, t128, 0.0D0, 0.0D0, -t134, -t139, t188)
      t295 = FJET(XB1, XB2, s, t128, -t269, 0.0D0, t268, t271, -t276)
      t306 = t200 - (0.90D2 * t74 * t207 + t212) * t79 / 0.1440D4 + t219
      t307 = FJET(XB1, XB2, s, t192, 0.0D0, -t195, 0.0D0, 0.0D0, t306)
      t309 = FJET(XB1, XB2, s, t235, 0.0D0, -t240, 0.0D0, 0.0D0, t259)
      t314 = FJET(XB1, XB2, s, t268, 0.0D0, -t269, t128, t271, -t276)
      t319 = FJET(XB1, XB2, s, -t195, 0.0D0, t192, 0.0D0, 0.0D0, t306)
      t321 = FJET(XB1, XB2, s, -t134, 0.0D0, 0.0D0, t128, -t139, t188)
      t323 = FJET(XB1, XB2, s, -t269, t128, t268, 0.0D0, t271, -t276)
      t328 = FJET(XB1, XB2, s, -t240, 0.0D0, t235, 0.0D0, 0.0D0, t259)
      rrgg2gght8s1em1 = t122 * t121 + t124 * t121 + t126 * t121 + t189 *
     # t188 + t221 * t220 + t260 * pi * t262 * t257 / 0.16D2 - t277 * pi
     # * t6 * t274 / 0.8D1 + t282 * t220 + t284 * t188 + t286 * pi * t26
     #2 * t257 / 0.16D2 + t291 * t121 + t293 * t188 - t295 * pi * t6 * t
     #274 / 0.8D1 + t307 * t306 + t309 * pi * t262 * t257 / 0.16D2 - t31
     #4 * pi * t6 * t274 / 0.8D1 + t319 * t306 + t321 * t188 - t323 * pi
     # * t6 * t274 / 0.8D1 + t328 * pi * t262 * t257 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s1em2
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = cos(t7)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t34 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
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
      t69 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t62, t56
     #, 0.0D0, -t67)
      t72 = t68 * t69 * t28 / 0.16D2
      t73 = FJET(XB1, XB2, s, 0.0D0, t56, -t62, 0.0D0, -t67, t72)
      t76 = t6 * t69 * t28
      t80 = x2 * s * t1
      t83 = (-0.1D1 + x2) * s * t1
      t84 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t83, 0.0D0
     #, 0.0D0, 0.0D0)
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
      rrgg2gght8s1em2 = t50 * t49 + t52 * t49 + t54 * t49 + t73 * pi * t
     #76 / 0.16D2 + t88 * pi * t91 / 0.16D2 + t94 * pi * t91 / 0.16D2 + 
     #t98 * pi * t76 / 0.16D2 + t102 * t49 + t104 * pi * t76 / 0.16D2 + 
     #t108 * pi * t91 / 0.16D2 + t112 * pi * t91 / 0.16D2 + t116 * pi * 
     #t76 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s1em3
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * pi * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = pi * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s1em3 = -t9 * t3 * t11 / 0.32D2 - t13 * t3 * t11 / 0.32D
     #2 - t16 * t3 * t11 / 0.32D2 - t19 * t3 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2gght8s1em4
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght8s2e1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = -0.1D1 + x2
      t4 = x2 * x3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x3
      t9 = Sqrt(-t4 * t7)
      t10 = t6 * t9
      t11 = 0.2D1 * t10
      t14 = t4 - 0.1D1
      t15 = 0.1D1 / t14
      t17 = t2 * t3 * (-t4 - 0.1D1 + x3 + t11) * t15
      t18 = 0.3D1 * t4
      t19 = x2 ** 2
      t20 = t19 * x3
      t22 = 0.2D1 * t10 * x2
      t25 = t2 * (-x2 - x3 + t18 - t20 - t11 + t22) * t15
      t26 = s ** 2
      t27 = 0.1D1 / t26
      t28 = pi * t27
      t29 = Sin(t5)
      t30 = t29 ** 2
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t34 = t3 ** 2
      t35 = t33 * t34
      t36 = t14 ** 2
      t37 = 0.1D1 / t36
      t39 = t35 * t7 * t37
      t42 = log(-0.4D1 * t4 * t30 * t39)
      t43 = x2 * z
      t44 = 0.1D1 - x2 + t43
      t45 = t42 * t44
      t46 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D0
     #, 0.0D0, 0.0D0)
      t48 = t42 ** 2
      t50 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D0
     #, 0.0D0, 0.0D0)
      t53 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D0
     #, 0.0D0, 0.0D0)
      t57 = z * t19 * x3
      t60 = t4 * z
      t61 = 0.2D1 * t4
      t63 = 0.1D1 / (-t57 + t20 - t22 + 0.2D1 * t43 * t10 - t43 + t60 + 
     #x2 - t61 + t11 - 0.1D1)
      t67 = pi * lh
      t68 = t44 * t46
      t75 = pi ** 2
      t76 = 0.30D2 * t75
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = -t76 + t78
      t80 = pi * t79
      t83 = t44 * t50 * t63
      t86 = 0.1D1 / x3
      t88 = 0.1D1 / x2
      t91 = x1 ** 2
      t92 = t91 * t30
      t93 = t4 * t92
      t96 = log(-0.4D1 * t93 * t39)
      t103 = t67 * t27
      t108 = 0.1D1 / x1
      t109 = t108 * t88
      t112 = -(-0.90D2 * t28 * (-t45 * t46 + t48 * t44 * t50 / 0.2D1 + t
     #44 * t53) * t63 + 0.180D3 * t67 * t27 * (t68 - t45 * t50) * t63 - 
     #t80 * t27 * t83) * t86 * t88 / 0.1440D4 - (-0.90D2 * t28 * (t68 - 
     #t96 * t44 * t50) * t63 + 0.180D3 * t103 * t83) * t86 * t109 / 0.72
     #0D3
      t113 = FJET(XB1, XB2, s, -t17, 0.0D0, t25, 0.0D0, 0.0D0, t112)
      t115 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t116 = x3 * t30
      t119 = log(0.4D1 * t116 * t33)
      t120 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t125 = t119 ** 2
      t128 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t132 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t137 = 0.240D3 * zeta3
      t139 = 0.120D3 * t77 * lh
      t141 = 0.60D2 * lh * t75
      t142 = -t137 - t139 + t141
      t143 = pi * t142
      t144 = t27 * t120
      t145 = t143 * t144
      t156 = t33 * t30
      t158 = log(0.4D1 * t156)
      t159 = t158 ** 2
      t162 = t159 * t158
      t185 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t193 = t75 ** 2
      t194 = t77 ** 2
      t200 = t159 ** 2
      t206 = x3 * t91
      t209 = log(0.4D1 * t206 * t156)
      t211 = t209 ** 2
      t222 = t80 * t144
      t227 = t92 * t33
      t229 = log(0.4D1 * t227)
      t234 = t229 ** 2
      t256 = log(0.4D1 * t4 * t227)
      t267 = x2 * t91
      t270 = log(0.4D1 * t267 * t156)
      t272 = t270 ** 2
      t287 = t4 * t156
      t289 = log(0.4D1 * t287)
      t291 = t289 ** 2
      t306 = x2 * t30
      t309 = log(0.4D1 * t306 * t33)
      t314 = t309 ** 2
      t334 = -(t80 * t27 * (t115 - t119 * t120) + 0.90D2 * t28 * (t125 *
     # t115 / 0.2D1 + t128 - t125 * t119 * t120 / 0.6D1 - t119 * t132) +
     # t145 - 0.180D3 * t67 * t27 * (-t119 * t115 + t125 * t120 / 0.2D1 
     #+ t132)) * t86 / 0.1440D4 - (-0.90D2 * t159 * lh - t137 - t139 + t
     #141 - 0.15D2 * t162 - t158 * t79) * pi * t27 * t115 / 0.1440D4 - (
     #0.180D3 * t158 * lh + 0.45D2 * t159 - t76 + t78) * pi * t27 * t132
     # / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t158) * pi * t27 * t128 / 
     #0.1440D4 - t28 * t185 / 0.16D2 - (0.30D2 * t162 * lh + t159 * t79 
     #/ 0.2D1 - t158 * t142 + t193 + 0.60D2 * t194 + 0.480D3 * lh * zeta
     #3 - 0.60D2 * t77 * t75 + 0.15D2 / 0.4D1 * t200) * pi * t144 / 0.14
     #40D4 - (0.90D2 * t28 * (-t209 * t115 + t211 * t120 / 0.2D1 + t132)
     # - 0.180D3 * t67 * t27 * (t115 - t209 * t120) + t222) * t86 * t108
     # / 0.720D3 - (t80 * t27 * (t115 - t229 * t120) + 0.90D2 * t28 * (t
     #234 * t115 / 0.2D1 + t128 - t234 * t229 * t120 / 0.6D1 - t229 * t1
     #32) + t145 - 0.180D3 * t67 * t27 * (-t229 * t115 + t234 * t120 / 0
     #.2D1 + t132)) * t108 / 0.720D3 - (0.90D2 * t28 * (t115 - t256 * t1
     #20) - 0.180D3 * t67 * t144) * t86 * t109 / 0.720D3 + (0.90D2 * t28
     # * (t270 * t115 - t272 * t120 / 0.2D1 - t132) - 0.180D3 * t67 * t2
     #7 * (-t115 + t270 * t120) - t222) * t108 * t88 / 0.720D3 - (0.90D2
     # * t28 * (-t289 * t115 + t291 * t120 / 0.2D1 + t132) - 0.180D3 * t
     #67 * t27 * (t115 - t289 * t120) + t222) * t86 * t88 / 0.1440D4 - (
     #t80 * t27 * (t115 - t309 * t120) + 0.90D2 * t28 * (t314 * t115 / 0
     #.2D1 + t128 - t314 * t309 * t120 / 0.6D1 - t309 * t132) + t145 - 0
     #.180D3 * t67 * t27 * (-t309 * t115 + t314 * t120 / 0.2D1 + t132)) 
     #* t88 / 0.1440D4
      t335 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t334)
      t337 = t3 * s
      t338 = t337 * t1
      t340 = x2 * s * t1
      t341 = t156 * t34
      t342 = t4 * t341
      t344 = log(0.4D1 * t342)
      t345 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t338, t340, 0.
     #0D0, 0.0D0, 0.0D0)
      t347 = t344 ** 2
      t348 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t338, t340, 0.
     #0D0, 0.0D0, 0.0D0)
      t351 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t338, t340, 0.
     #0D0, 0.0D0, 0.0D0)
      t360 = t27 * t348
      t361 = t80 * t360
      t368 = log(0.4D1 * t306 * t35)
      t373 = t368 ** 2
      t376 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t338, t340, 0.
     #0D0, 0.0D0, 0.0D0)
      t395 = t206 * x2
      t398 = log(0.4D1 * t395 * t341)
      t411 = log(0.4D1 * t267 * t341)
      t413 = t411 ** 2
      t428 = -(0.90D2 * t28 * (t344 * t345 - t347 * t348 / 0.2D1 - t351)
     # - 0.180D3 * t67 * t27 * (-t345 + t344 * t348) - t361) * t86 * t88
     # / 0.1440D4 - (t80 * t27 * (-t345 + t368 * t348) + 0.90D2 * t28 * 
     #(-t373 * t345 / 0.2D1 - t376 + t373 * t368 * t348 / 0.6D1 + t368 *
     # t351) - t143 * t360 - 0.180D3 * t67 * t27 * (t368 * t345 - t373 *
     # t348 / 0.2D1 - t351)) * t88 / 0.1440D4 - (0.90D2 * t28 * (-t345 +
     # t398 * t348) + 0.180D3 * t67 * t360) * t86 * t109 / 0.720D3 + (0.
     #90D2 * t28 * (-t411 * t345 + t413 * t348 / 0.2D1 + t351) - 0.180D3
     # * t67 * t27 * (t345 - t411 * t348) + t361) * t108 * t88 / 0.720D3
      t429 = FJET(XB1, XB2, s, -t338, 0.0D0, t340, 0.0D0, 0.0D0, t428)
      t431 = FJET(XB1, XB2, s, 0.0D0, -t338, 0.0D0, t340, 0.0D0, t428)
      t433 = t7 * s
      t434 = -0.1D1 + x1
      t435 = t1 * t434
      t436 = t433 * t435
      t437 = t1 * x1
      t438 = t433 * t437
      t440 = t2 * t434 * x3
      t441 = x3 * x1
      t442 = t2 * t441
      t443 = t206 * t30
      t444 = x1 * z
      t445 = 0.1D1 - x1 + t444
      t446 = 0.1D1 / t445
      t448 = t434 ** 2
      t453 = log(-0.4D1 * t443 * t33 * t446 * t448 * t7)
      t454 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t436, -t440, -t
     #438, t442, 0.0D0)
      t456 = t453 ** 2
      t457 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t436, -t440, -t
     #438, t442, 0.0D0)
      t460 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t436, -t440, -t
     #438, t442, 0.0D0)
      t469 = t27 * t457
      t474 = t446 * t448
      t479 = log(-0.4D1 * t474 * t7 * t91 * t287)
      t490 = -(0.90D2 * t28 * (-t453 * t454 + t456 * t457 / 0.2D1 + t460
     #) - 0.180D3 * t67 * t27 * (t454 - t453 * t457) + t80 * t469) * t86
     # * t108 / 0.720D3 - (0.90D2 * t28 * (t454 - t479 * t457) - 0.180D3
     # * t67 * t469) * t86 * t109 / 0.720D3
      t491 = FJET(XB1, XB2, s, t436, -t438, -t440, t442, 0.0D0, t490)
      t493 = FJET(XB1, XB2, s, -t438, t436, t442, -t440, 0.0D0, t490)
      t495 = t337 * t435
      t496 = t2 * x1
      t499 = t2 * t434 * x2 * t446
      t500 = t1 ** 2
      t505 = s * t500 * x2 * x1 * t434 * t446
      t506 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t495, -t499, t4
     #96, 0.0D0, -t505)
      t507 = t33 * t448
      t509 = t507 * t446 * t34
      t512 = log(0.4D1 * t93 * t509)
      t513 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t495, -t499, t4
     #96, 0.0D0, -t505)
      t518 = t27 * t513
      t523 = (0.90D2 * t28 * (t506 - t512 * t513) - 0.180D3 * t67 * t518
     #) * t86 * t109
      t524 = t267 * t30
      t527 = log(0.4D1 * t524 * t509)
      t529 = t527 ** 2
      t532 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t495, -t499, t4
     #96, 0.0D0, -t505)
      t533 = t527 * t506 - t529 * t513 / 0.2D1 - t532
      t537 = -t506 + t527 * t513
      t541 = t80 * t518
      t546 = -t523 / 0.720D3 + (0.90D2 * t28 * t533 - 0.180D3 * t67 * t2
     #7 * t537 - t541) * t108 * t88 / 0.720D3
      t547 = FJET(XB1, XB2, s, t495, t496, -t499, 0.0D0, -t505, t546)
      t549 = FJET(XB1, XB2, s, 0.0D0, -t17, 0.0D0, t25, 0.0D0, t112)
      t551 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t334)
      t553 = t2 * t434
      t554 = t507 * t446
      t557 = log(0.4D1 * t443 * t554)
      t558 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t553, 0.0D0, t
     #496, 0.0D0, 0.0D0)
      t560 = t557 ** 2
      t561 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t553, 0.0D0, t
     #496, 0.0D0, 0.0D0)
      t564 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t553, 0.0D0, t
     #496, 0.0D0, 0.0D0)
      t573 = t27 * t561
      t574 = t80 * t573
      t580 = log(0.4D1 * t92 * t554)
      t585 = t580 ** 2
      t588 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t553, 0.0D0, t
     #496, 0.0D0, 0.0D0)
      t609 = log(0.4D1 * t395 * t156 * t474)
      t621 = log(0.4D1 * t524 * t554)
      t623 = t621 ** 2
      t638 = -(0.90D2 * t28 * (t557 * t558 - t560 * t561 / 0.2D1 - t564)
     # - 0.180D3 * t67 * t27 * (-t558 + t557 * t561) - t574) * t86 * t10
     #8 / 0.720D3 - (t80 * t27 * (-t558 + t580 * t561) + 0.90D2 * t28 * 
     #(-t585 * t558 / 0.2D1 - t588 + t585 * t580 * t561 / 0.6D1 + t580 *
     # t564) - t143 * t573 - 0.180D3 * t67 * t27 * (t580 * t558 - t585 *
     # t561 / 0.2D1 - t564)) * t108 / 0.720D3 - (0.90D2 * t28 * (-t558 +
     # t609 * t561) + 0.180D3 * t67 * t573) * t86 * t109 / 0.720D3 + (0.
     #90D2 * t28 * (-t621 * t558 + t623 * t561 / 0.2D1 + t564) - 0.180D3
     # * t67 * t27 * (t558 - t621 * t561) + t574) * t108 * t88 / 0.720D3
      t639 = FJET(XB1, XB2, s, -t553, t496, 0.0D0, 0.0D0, 0.0D0, t638)
      t641 = FJET(XB1, XB2, s, t496, -t553, 0.0D0, 0.0D0, 0.0D0, t638)
      t643 = FJET(XB1, XB2, s, t25, 0.0D0, -t17, 0.0D0, 0.0D0, t112)
      t645 = FJET(XB1, XB2, s, 0.0D0, -t499, t496, t495, -t505, t546)
      t658 = -t523 / 0.720D3 + (0.90D2 * t28 * t533 - 0.180D3 * t67 * t2
     #7 * t537 - t541) * t108 * t88 / 0.720D3
      t659 = FJET(XB1, XB2, s, -t499, 0.0D0, t495, t496, -t505, t658)
      t661 = FJET(XB1, XB2, s, t442, -t440, -t438, t436, 0.0D0, t490)
      t663 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t496, -t553, 0.0D0, t638)
      t665 = t113 * t112 + t335 * t334 + t429 * t428 + t431 * t428 + t49
     #1 * t490 + t493 * t490 + t547 * t546 + t549 * t112 + t551 * t334 +
     # t639 * t638 + t641 * t638 + t643 * t112 + t645 * t546 + t659 * t6
     #58 + t661 * t490 + t663 * t638
      t666 = FJET(XB1, XB2, s, -t440, t442, t436, -t438, 0.0D0, t490)
      t668 = t2 * t7
      t669 = t2 * x3
      t670 = t156 * t7
      t673 = log(-0.4D1 * t206 * t670)
      t674 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t668, t669, 0.
     #0D0, 0.0D0, 0.0D0)
      t676 = t673 ** 2
      t677 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t668, t669, 0.
     #0D0, 0.0D0, 0.0D0)
      t680 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t668, t669, 0.
     #0D0, 0.0D0, 0.0D0)
      t689 = t27 * t677
      t690 = t80 * t689
      t694 = (0.90D2 * t28 * (t673 * t674 - t676 * t677 / 0.2D1 - t680) 
     #- 0.180D3 * t67 * t27 * (-t674 + t673 * t677) - t690) * t86 * t108
     # / 0.720D3
      t697 = log(-0.4D1 * t395 * t670)
      t707 = (0.90D2 * t28 * (-t674 + t697 * t677) + 0.180D3 * t67 * t68
     #9) * t86 * t109 / 0.720D3
      t711 = log(-0.4D1 * t116 * t33 * t7)
      t713 = -t674 + t711 * t677
      t716 = t711 ** 2
      t719 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t668, t669, 0.
     #0D0, 0.0D0, 0.0D0)
      t724 = -t716 * t674 / 0.2D1 - t719 + t716 * t711 * t677 / 0.6D1 + 
     #t711 * t680
      t727 = t143 * t689
      t731 = t711 * t674 - t716 * t677 / 0.2D1 - t680
      t740 = log(-0.4D1 * t4 * t670)
      t742 = t740 ** 2
      t756 = (0.90D2 * t28 * (t740 * t674 - t742 * t677 / 0.2D1 - t680) 
     #- 0.180D3 * t67 * t27 * (-t674 + t740 * t677) - t690) * t86 * t88 
     #/ 0.1440D4
      t757 = -t694 - t707 - (t80 * t27 * t713 + 0.90D2 * t28 * t724 - t7
     #27 - 0.180D3 * t67 * t27 * t731) * t86 / 0.1440D4 - t756
      t758 = FJET(XB1, XB2, s, 0.0D0, -t668, 0.0D0, t669, 0.0D0, t757)
      t760 = t441 * z
      t764 = Sqrt(-x3 * t445 * x2 * t7)
      t765 = t6 * t764
      t766 = 0.2D1 * t765
      t771 = t553 * t3 * (-t4 - 0.1D1 + x3 + x1 - t441 - t444 + t760 + t
     #766) * t446 * t15
      t773 = t433 * t437 * t15
      t774 = t441 * t43
      t776 = t444 * t20
      t778 = 0.2D1 * t765 * x2
      t780 = x1 * t19 * x3
      t781 = t441 * x2
      t783 = t18 + 0.2D1 * t774 - t776 - t20 + t441 - t766 - x2 - x3 + t
     #778 + t780 - 0.2D1 * t781 - t760
      t786 = t553 * t783 * t446 * t15
      t789 = t496 * x3 * t3 * t15
      t797 = t91 * t32
      t801 = x1 * x2
      t808 = 0.1D1 + t91 - t60 + t57 + 0.2D1 * t444 - 0.2D1 * x1 + 0.4D1
     # * t774 - t776 - t441 * x2 * t32 - 0.2D1 * t206 * t43 + t797 * t4 
     #- 0.2D1 * t43 * t765 - 0.2D1 * t765 * t801 - 0.2D1 * z * t6 * t764
     # * x1 - x2 + t43
      t809 = t765 * x1
      t814 = t801 * z
      t823 = 0.2D1 * t43 * t809 + t778 + t780 - 0.3D1 * t781 + 0.2D1 * t
     #809 - 0.3D1 * t814 + t801 * t32 + 0.2D1 * t267 * z - t267 * t32 + 
     #t395 + t61 - t20 - t766 + 0.2D1 * t801 - 0.2D1 * t91 * z + t797 - 
     #t267
      t825 = 0.1D1 / (t808 + t823)
      t826 = t825 * t445
      t827 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t771, -t786, t7
     #73, t789, -t505)
      t835 = log(-0.4D1 * t37 * t448 * t7 * t446 * t91 * t342)
      t837 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t771, -t786, t7
     #73, t789, -t505)
      t841 = -0.1D1 - t801 + x1 + t814 + x2 - t444 - t43
      t849 = 0.90D2 * t28 * (t826 * t827 - t835 * t825 * t445 * t837) * 
     #t841 - 0.180D3 * t103 * t826 * t837 * t841
      t852 = t849 * t86 * t109 / 0.720D3
      t853 = FJET(XB1, XB2, s, t771, t773, -t786, t789, -t505, -t852)
      t856 = t86 * t108 * t88
      t859 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t334)
      t861 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t334)
      t863 = FJET(XB1, XB2, s, t789, -t786, t773, t771, -t505, -t852)
      t867 = FJET(XB1, XB2, s, -t786, t789, t771, t773, -t505, -t852)
      t871 = FJET(XB1, XB2, s, t773, t771, t789, -t786, -t505, -t852)
      t888 = -t694 - t707 - (t80 * t27 * t713 + 0.90D2 * t28 * t724 - t7
     #27 - 0.180D3 * t67 * t27 * t731) * t86 / 0.1440D4 - t756
      t889 = FJET(XB1, XB2, s, t669, 0.0D0, -t668, 0.0D0, 0.0D0, t888)
      t891 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t553, t496, 0.0D0, t638)
      t893 = FJET(XB1, XB2, s, t340, 0.0D0, -t338, 0.0D0, 0.0D0, t428)
      t895 = FJET(XB1, XB2, s, 0.0D0, t25, 0.0D0, -t17, 0.0D0, t112)
      t897 = FJET(XB1, XB2, s, t496, t495, 0.0D0, -t499, -t505, t546)
      t899 = FJET(XB1, XB2, s, 0.0D0, t340, 0.0D0, -t338, 0.0D0, t428)
      t901 = FJET(XB1, XB2, s, -t668, 0.0D0, t669, 0.0D0, 0.0D0, t888)
      t903 = FJET(XB1, XB2, s, 0.0D0, t669, 0.0D0, -t668, 0.0D0, t757)
      t905 = t666 * t490 + t758 * t757 - t853 * t849 * t856 / 0.720D3 + 
     #t859 * t334 + t861 * t334 - t863 * t849 * t856 / 0.720D3 - t867 * 
     #t849 * t856 / 0.720D3 - t871 * t849 * t856 / 0.720D3 + t889 * t888
     # + t891 * t638 + t893 * t428 + t895 * t112 + t897 * t546 + t899 * 
     #t428 + t901 * t888 + t903 * t757
      rrgg2gght8s2e1 = t665 + t905

      end function



      doubleprecision function rrgg2gght8s2e0
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = t2 * x1
      t4 = -0.1D1 + x2
      t6 = x2 * x3
      t7 = t6 - 0.1D1
      t8 = 0.1D1 / t7
      t10 = t3 * x3 * t4 * t8
      t11 = -0.1D1 + x1
      t12 = t2 * t11
      t13 = 0.3D1 * t6
      t14 = x3 * x1
      t15 = x2 * z
      t16 = t14 * t15
      t18 = x1 * z
      t19 = x2 ** 2
      t20 = t19 * x3
      t21 = t18 * t20
      t22 = x4 * pi
      t23 = cos(t22)
      t24 = 0.1D1 - x1 + t18
      t26 = -0.1D1 + x3
      t29 = Sqrt(-x3 * t24 * x2 * t26)
      t30 = t23 * t29
      t31 = 0.2D1 * t30
      t33 = 0.2D1 * t30 * x2
      t35 = x1 * t19 * x3
      t36 = t14 * x2
      t38 = t14 * z
      t39 = t13 + 0.2D1 * t16 - t21 - t20 + t14 - t31 - x2 - x3 + t33 + 
     #t35 - 0.2D1 * t36 - t38
      t40 = 0.1D1 / t24
      t43 = t12 * t39 * t40 * t8
      t44 = t26 * s
      t45 = t1 * x1
      t47 = t44 * t45 * t8
      t52 = t12 * t4 * (-t6 - 0.1D1 + x3 + x1 - t14 - t18 + t38 + t31) *
     # t40 * t8
      t53 = t1 ** 2
      t58 = s * t53 * x2 * x1 * t11 * t40
      t59 = s ** 2
      t60 = 0.1D1 / t59
      t61 = pi * t60
      t62 = z ** 2
      t65 = x1 ** 2
      t66 = x3 * t65
      t68 = x2 * t65
      t70 = x1 * x2
      t71 = t70 * z
      t75 = t30 * x1
      t82 = 0.2D1 * t6
      t83 = 0.1D1 - t14 * x2 * t62 + t66 * x2 - t68 * t62 - 0.3D1 * t71 
     #+ 0.2D1 * t68 * z + 0.2D1 * t75 + t70 * t62 + 0.2D1 * t15 * t75 + 
     #t65 + 0.2D1 * t18 - t31 + t33 + t35 - 0.3D1 * t36 + t82
      t87 = z * t19 * x3
      t88 = t6 * z
      t95 = t65 * t62
      t104 = -t20 + t15 - x2 - 0.2D1 * x1 + 0.4D1 * t16 - t21 + t87 - t8
     #8 - 0.2D1 * z * t23 * t29 * x1 - 0.2D1 * t30 * t70 - t68 + t95 - 0
     #.2D1 * t65 * z + 0.2D1 * t70 - 0.2D1 * t15 * t30 + t95 * t6 - 0.2D
     #1 * t66 * t15
      t106 = 0.1D1 / (t83 + t104)
      t109 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t52, -t43, t47,
     # t10, -t58)
      t112 = 0.1D1 / x3
      t113 = 0.1D1 / x1
      t115 = 0.1D1 / x2
      t116 = t112 * t113 * t115
      t117 = t109 * (-0.1D1 - t70 + x1 + t71 + x2 - t18 - t15) * t116
      t119 = t61 * t106 * t24 * t117 / 0.8D1
      t120 = FJET(XB1, XB2, s, t10, -t43, t47, t52, -t58, -t119)
      t123 = t60 * t106 * t24
      t127 = FJET(XB1, XB2, s, t52, t47, -t43, t10, -t58, -t119)
      t132 = FJET(XB1, XB2, s, -t43, t10, t52, t47, -t58, -t119)
      t137 = t4 * s
      t138 = t137 * t1
      t140 = x2 * s * t1
      t141 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t138, t140, 0.
     #0D0, 0.0D0, 0.0D0)
      t145 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t138, t140, 0.
     #0D0, 0.0D0, 0.0D0)
      t146 = Sin(t22)
      t147 = t146 ** 2
      t148 = 0.1D1 / t62
      t149 = t147 * t148
      t150 = t4 ** 2
      t151 = t149 * t150
      t154 = log(0.4D1 * t68 * t151)
      t159 = pi * lh
      t160 = t60 * t141
      t162 = 0.180D3 * t159 * t160
      t169 = log(0.4D1 * t6 * t151)
      t178 = x2 * t147
      t179 = t148 * t150
      t182 = log(0.4D1 * t178 * t179)
      t184 = t182 ** 2
      t187 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t138, t140, 0.
     #0D0, 0.0D0, 0.0D0)
      t196 = pi ** 2
      t197 = 0.30D2 * t196
      t198 = lh ** 2
      t199 = 0.180D3 * t198
      t200 = -t197 + t199
      t201 = pi * t200
      t206 = t61 * t141 * t116 / 0.8D1 + (0.90D2 * t61 * (t145 - t154 * 
     #t141) - t162) * t113 * t115 / 0.720D3 - (0.90D2 * t61 * (-t145 + t
     #169 * t141) + t162) * t112 * t115 / 0.1440D4 - (0.90D2 * t61 * (t1
     #82 * t145 - t184 * t141 / 0.2D1 - t187) - 0.180D3 * t159 * t60 * (
     #-t145 + t182 * t141) - t201 * t160) * t115 / 0.1440D4
      t207 = FJET(XB1, XB2, s, -t138, 0.0D0, t140, 0.0D0, 0.0D0, t206)
      t209 = FJET(XB1, XB2, s, 0.0D0, t140, 0.0D0, -t138, 0.0D0, t206)
      t211 = FJET(XB1, XB2, s, t140, 0.0D0, -t138, 0.0D0, 0.0D0, t206)
      t213 = t1 * t11
      t214 = t137 * t213
      t217 = t2 * t11 * x2 * t40
      t218 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t214, -t217, t3
     #, 0.0D0, -t58)
      t221 = t61 * t218 * t116 / 0.8D1
      t222 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t214, -t217, t3
     #, 0.0D0, -t58)
      t223 = t68 * t147
      t224 = t148 * t40
      t225 = t11 ** 2
      t230 = log(0.4D1 * t223 * t224 * t225 * t150)
      t232 = -t222 + t230 * t218
      t237 = 0.180D3 * t159 * t60 * t218
      t242 = -t221 + (0.90D2 * t61 * t232 + t237) * t113 * t115 / 0.720D
     #3
      t243 = FJET(XB1, XB2, s, t3, t214, 0.0D0, -t217, -t58, t242)
      t245 = x3 * t147
      t248 = log(0.4D1 * t245 * t148)
      t249 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t251 = t248 ** 2
      t252 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t255 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t264 = t60 * t252
      t265 = t201 * t264
      t270 = log(0.4D1 * t149)
      t273 = t270 ** 2
      t280 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t306 = log(0.4D1 * t6 * t149)
      t312 = 0.180D3 * t159 * t264
      t319 = log(0.4D1 * t178 * t148)
      t321 = t319 ** 2
      t337 = log(0.4D1 * t66 * t149)
      t351 = log(0.4D1 * t68 * t149)
      t360 = t65 * t147
      t363 = log(0.4D1 * t360 * t148)
      t365 = t363 ** 2
      t379 = -(0.90D2 * t61 * (-t248 * t249 + t251 * t252 / 0.2D1 + t255
     #) - 0.180D3 * t159 * t60 * (t249 - t248 * t252) + t265) * t112 / 0
     #.1440D4 - (0.180D3 * t270 * lh + 0.45D2 * t273 - t197 + t199) * pi
     # * t60 * t249 / 0.1440D4 - t61 * t280 / 0.16D2 - (-0.90D2 * t273 *
     # lh - 0.240D3 * zeta3 - 0.120D3 * t198 * lh + 0.60D2 * lh * t196 -
     # 0.15D2 * t273 * t270 - t270 * t200) * pi * t264 / 0.1440D4 - (-0.
     #180D3 * lh - 0.90D2 * t270) * pi * t60 * t255 / 0.1440D4 - (0.90D2
     # * t61 * (t249 - t306 * t252) - t312) * t112 * t115 / 0.1440D4 - (
     #0.90D2 * t61 * (-t319 * t249 + t321 * t252 / 0.2D1 + t255) - 0.180
     #D3 * t159 * t60 * (t249 - t319 * t252) + t265) * t115 / 0.1440D4 -
     # (0.90D2 * t61 * (t249 - t337 * t252) - t312) * t112 * t113 / 0.72
     #0D3 - t61 * t252 * t116 / 0.8D1 + (0.90D2 * t61 * (-t249 + t351 * 
     #t252) + t312) * t113 * t115 / 0.720D3 - (0.90D2 * t61 * (-t363 * t
     #249 + t365 * t252 / 0.2D1 + t255) - 0.180D3 * t159 * t60 * (t249 -
     # t363 * t252) + t265) * t113 / 0.720D3
      t380 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t379)
      t382 = t44 * t213
      t383 = t44 * t45
      t385 = t2 * t11 * x3
      t386 = t2 * t14
      t387 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t382, -t385, -t
     #383, t386, 0.0D0)
      t388 = t66 * t147
      t393 = log(-0.4D1 * t388 * t224 * t225 * t26)
      t394 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t382, -t385, -t
     #383, t386, 0.0D0)
      t409 = -(0.90D2 * t61 * (t387 - t393 * t394) - 0.180D3 * t159 * t6
     #0 * t394) * t112 * t113 / 0.720D3 - t61 * t394 * t116 / 0.8D1
      t410 = FJET(XB1, XB2, s, t382, -t383, -t385, t386, 0.0D0, t409)
      t412 = FJET(XB1, XB2, s, t386, -t385, -t383, t382, 0.0D0, t409)
      t414 = FJET(XB1, XB2, s, -t385, t386, t382, -t383, 0.0D0, t409)
      t416 = FJET(XB1, XB2, s, 0.0D0, -t138, 0.0D0, t140, 0.0D0, t206)
      t418 = FJET(XB1, XB2, s, -t383, t382, t386, -t385, 0.0D0, t409)
      t420 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0, t3
     #, 0.0D0, 0.0D0)
      t422 = t148 * t225 * t40
      t425 = log(0.4D1 * t388 * t422)
      t426 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0, t3
     #, 0.0D0, 0.0D0)
      t431 = t60 * t426
      t433 = 0.180D3 * t159 * t431
      t443 = log(0.4D1 * t223 * t422)
      t454 = log(0.4D1 * t360 * t422)
      t456 = t454 ** 2
      t459 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0, t3
     #, 0.0D0, 0.0D0)
      t472 = -(0.90D2 * t61 * (-t420 + t425 * t426) + t433) * t112 * t11
     #3 / 0.720D3 + t61 * t426 * t116 / 0.8D1 + (0.90D2 * t61 * (t420 - 
     #t443 * t426) - t433) * t113 * t115 / 0.720D3 - (0.90D2 * t61 * (t4
     #54 * t420 - t456 * t426 / 0.2D1 - t459) - 0.180D3 * t159 * t60 * (
     #-t420 + t454 * t426) - t201 * t431) * t113 / 0.720D3
      t473 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t12, 0.0D0, t472)
      t475 = FJET(XB1, XB2, s, t3, -t12, 0.0D0, 0.0D0, 0.0D0, t472)
      t484 = -t221 + (0.90D2 * t61 * t232 + t237) * t113 * t115 / 0.720D
     #3
      t485 = FJET(XB1, XB2, s, -t217, 0.0D0, t214, t3, -t58, t484)
      t487 = -t120 * pi * t123 * t117 / 0.8D1 - t127 * pi * t123 * t117 
     #/ 0.8D1 - t132 * pi * t123 * t117 / 0.8D1 + t207 * t206 + t209 * t
     #206 + t211 * t206 + t243 * t242 + t380 * t379 + t410 * t409 + t412
     # * t409 + t414 * t409 + t416 * t206 + t418 * t409 + t473 * t472 + 
     #t475 * t472 + t485 * t484
      t488 = FJET(XB1, XB2, s, 0.0D0, -t217, t3, t214, -t58, t242)
      t491 = Sqrt(-t6 * t26)
      t492 = t23 * t491
      t493 = 0.2D1 * t492
      t497 = t2 * t4 * (-t6 - 0.1D1 + x3 + t493) * t8
      t499 = 0.2D1 * t492 * x2
      t502 = t2 * (-x2 - x3 + t13 - t20 - t493 + t499) * t8
      t503 = 0.1D1 - x2 + t15
      t504 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t497, t502, 0.
     #0D0, 0.0D0, 0.0D0)
      t505 = t503 * t504
      t510 = 0.1D1 / (-t87 + t20 - t499 + 0.2D1 * t15 * t492 - t15 + t88
     # + x2 - t82 + t493 - 0.1D1)
      t516 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t497, t502, 0.
     #0D0, 0.0D0, 0.0D0)
      t519 = t7 ** 2
      t525 = log(-0.4D1 * t6 * t147 * t179 * t26 / t519)
      t540 = t61 * t505 * t510 * t112 * t113 * t115 / 0.8D1 - (-0.90D2 *
     # t61 * (t503 * t516 - t525 * t503 * t504) * t510 + 0.180D3 * t159 
     #* t60 * t505 * t510) * t112 * t115 / 0.1440D4
      t541 = FJET(XB1, XB2, s, -t497, 0.0D0, t502, 0.0D0, 0.0D0, t540)
      t543 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t12, t3, 0.0D0, t472)
      t545 = FJET(XB1, XB2, s, t214, t3, -t217, 0.0D0, -t58, t242)
      t547 = FJET(XB1, XB2, s, t502, 0.0D0, -t497, 0.0D0, 0.0D0, t540)
      t549 = t2 * x3
      t550 = t2 * t26
      t554 = log(-0.4D1 * t245 * t148 * t26)
      t555 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t550, t549, 0.
     #0D0, 0.0D0, 0.0D0)
      t557 = t554 ** 2
      t558 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t550, t549, 0.
     #0D0, 0.0D0, 0.0D0)
      t561 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t550, t549, 0.
     #0D0, 0.0D0, 0.0D0)
      t562 = -t554 * t555 + t557 * t558 / 0.2D1 + t561
      t566 = t555 - t554 * t558
      t570 = t60 * t558
      t571 = t201 * t570
      t575 = t149 * t26
      t578 = log(-0.4D1 * t6 * t575)
      t584 = 0.180D3 * t159 * t570
      t588 = (0.90D2 * t61 * (-t555 + t578 * t558) + t584) * t112 * t115
     # / 0.1440D4
      t591 = log(-0.4D1 * t66 * t575)
      t599 = (0.90D2 * t61 * (-t555 + t591 * t558) + t584) * t112 * t113
     # / 0.720D3
      t602 = t61 * t558 * t116 / 0.8D1
      t603 = -(-0.90D2 * t61 * t562 + 0.180D3 * t159 * t60 * t566 - t571
     #) * t112 / 0.1440D4 - t588 - t599 + t602
      t604 = FJET(XB1, XB2, s, t549, 0.0D0, -t550, 0.0D0, 0.0D0, t603)
      t606 = FJET(XB1, XB2, s, t47, t52, t10, -t43, -t58, -t119)
      t611 = FJET(XB1, XB2, s, 0.0D0, t502, 0.0D0, -t497, 0.0D0, t540)
      t613 = FJET(XB1, XB2, s, -t12, t3, 0.0D0, 0.0D0, 0.0D0, t472)
      t615 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t379)
      t617 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t379)
      t619 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t379)
      t631 = -(-0.90D2 * t61 * t562 + 0.180D3 * t159 * t60 * t566 - t571
     #) * t112 / 0.1440D4 - t588 - t599 + t602
      t632 = FJET(XB1, XB2, s, 0.0D0, t549, 0.0D0, -t550, 0.0D0, t631)
      t634 = FJET(XB1, XB2, s, 0.0D0, -t497, 0.0D0, t502, 0.0D0, t540)
      t636 = FJET(XB1, XB2, s, 0.0D0, -t550, 0.0D0, t549, 0.0D0, t631)
      t638 = FJET(XB1, XB2, s, -t550, 0.0D0, t549, 0.0D0, 0.0D0, t603)
      t640 = t488 * t242 + t541 * t540 + t543 * t472 + t545 * t242 + t54
     #7 * t540 + t604 * t603 - t606 * pi * t123 * t117 / 0.8D1 + t611 * 
     #t540 + t613 * t472 + t615 * t379 + t617 * t379 + t619 * t379 + t63
     #2 * t631 + t634 * t540 + t636 * t631 + t638 * t603
      rrgg2gght8s2e0 = t487 + t640

      end function



      doubleprecision function rrgg2gght8s2em1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = -0.1D1 + x3
      t4 = t2 * t3
      t5 = t2 * x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t13 * t15 * t3)
      t20 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0, 
     #0.0D0, 0.0D0)
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
      t43 = FJET(XB1, XB2, s, -t4, 0.0D0, t5, 0.0D0, 0.0D0, t42)
      t45 = FJET(XB1, XB2, s, t5, 0.0D0, -t4, 0.0D0, 0.0D0, t42)
      t47 = -0.1D1 + x1
      t48 = t2 * t47
      t49 = t2 * x1
      t50 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t48, 0.0D0, t49
     #, 0.0D0, 0.0D0)
      t55 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t48, 0.0D0, t49
     #, 0.0D0, 0.0D0)
      t56 = x1 ** 2
      t57 = t56 * t12
      t60 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t62 = t47 ** 2
      t66 = log(0.4D1 * t57 * t15 * t60 * t62)
      t81 = t8 * t50 * t38 * t34 / 0.8D1 - (0.90D2 * t8 * (-t55 + t66 * 
     #t50) + 0.180D3 * t25 * t7 * t50) * t38 / 0.720D3 + t8 * t50 * t30 
     #* t38 / 0.8D1
      t82 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t49, 0.0D0, t81)
      t85 = x2 * s * t1
      t86 = -0.1D1 + x2
      t87 = t86 * s
      t88 = t87 * t1
      t89 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t88, t85, 0.0D0
     #, 0.0D0, 0.0D0)
      t98 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t88, t85, 0.0D0
     #, 0.0D0, 0.0D0)
      t99 = x2 * t12
      t100 = t86 ** 2
      t104 = log(0.4D1 * t99 * t15 * t100)
      t115 = t8 * t89 * t38 * t34 / 0.8D1 + t8 * t89 * t30 * t34 / 0.16D
     #2 - (0.90D2 * t8 * (-t98 + t104 * t89) + 0.180D3 * t25 * t7 * t89)
     # * t34 / 0.1440D4
      t116 = FJET(XB1, XB2, s, 0.0D0, t85, 0.0D0, -t88, 0.0D0, t115)
      t118 = FJET(XB1, XB2, s, -t48, t49, 0.0D0, 0.0D0, 0.0D0, t81)
      t120 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t123 = log(0.4D1 * t13 * t15)
      t124 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t129 = t7 * t124
      t131 = 0.180D3 * t25 * t129
      t138 = log(0.4D1 * t15 * t12)
      t147 = t138 ** 2
      t149 = pi ** 2
      t151 = lh ** 2
      t157 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t160 = t124 * t30
      t166 = log(0.4D1 * t99 * t15)
      t180 = log(0.4D1 * t57 * t15)
      t191 = -(0.90D2 * t8 * (t120 - t123 * t124) - t131) * t30 / 0.1440
     #D4 - (-0.180D3 * lh - 0.90D2 * t138) * pi * t7 * t120 / 0.1440D4 -
     # (0.180D3 * t138 * lh + 0.45D2 * t147 - 0.30D2 * t149 + 0.180D3 * 
     #t151) * pi * t129 / 0.1440D4 - t8 * t157 / 0.16D2 - t8 * t160 * t3
     #4 / 0.16D2 - (0.90D2 * t8 * (t120 - t166 * t124) - t131) * t34 / 0
     #.1440D4 - t8 * t124 * t38 * t34 / 0.8D1 - (0.90D2 * t8 * (t120 - t
     #180 * t124) - t131) * t38 / 0.720D3 - t8 * t160 * t38 / 0.8D1
      t192 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t191)
      t194 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t49, -t48, 0.0D0, t81)
      t196 = FJET(XB1, XB2, s, -t88, 0.0D0, t85, 0.0D0, 0.0D0, t115)
      t198 = FJET(XB1, XB2, s, 0.0D0, -t88, 0.0D0, t85, 0.0D0, t115)
      t200 = x2 * x3
      t202 = x2 ** 2
      t203 = t202 * x3
      t204 = cos(t10)
      t206 = Sqrt(-t200 * t3)
      t207 = t204 * t206
      t208 = 0.2D1 * t207
      t210 = 0.2D1 * t207 * x2
      t213 = 0.1D1 / (t200 - 0.1D1)
      t215 = t2 * (-x2 - x3 + 0.3D1 * t200 - t203 - t208 + t210) * t213
      t219 = t2 * t86 * (-t200 - 0.1D1 + x3 + t208) * t213
      t220 = x2 * z
      t221 = 0.1D1 - x2 + t220
      t223 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t219, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t234 = t223 / (-z * t202 * x3 + t203 - t210 + 0.2D1 * t220 * t207 
     #- t220 + t200 * z + x2 - 0.2D1 * t200 + t208 - 0.1D1) * t30 * t34
      t236 = t8 * t221 * t234 / 0.16D2
      t237 = FJET(XB1, XB2, s, t215, 0.0D0, -t219, 0.0D0, 0.0D0, t236)
      t239 = t7 * t221
      t243 = FJET(XB1, XB2, s, -t219, 0.0D0, t215, 0.0D0, 0.0D0, t236)
      t248 = t1 * t47
      t249 = t87 * t248
      t252 = t2 * t47 * x2 * t60
      t253 = t1 ** 2
      t258 = s * t253 * x2 * x1 * t47 * t60
      t259 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t249, -t252, t4
     #9, 0.0D0, -t258)
      t261 = t259 * t38 * t34
      t263 = t8 * t261 / 0.8D1
      t264 = FJET(XB1, XB2, s, t49, t249, 0.0D0, -t252, -t258, -t263)
      t269 = FJET(XB1, XB2, s, t249, t49, -t252, 0.0D0, -t258, -t263)
      t274 = FJET(XB1, XB2, s, 0.0D0, -t252, t49, t249, -t258, -t263)
      t279 = t43 * t42 + t45 * t42 + t82 * t81 + t116 * t115 + t118 * t8
     #1 + t192 * t191 + t194 * t81 + t196 * t115 + t198 * t115 + t237 * 
     #pi * t239 * t234 / 0.16D2 + t243 * pi * t239 * t234 / 0.16D2 - t26
     #4 * pi * t7 * t261 / 0.8D1 - t269 * pi * t7 * t261 / 0.8D1 - t274 
     #* pi * t7 * t261 / 0.8D1
      t280 = FJET(XB1, XB2, s, -t252, 0.0D0, t249, t49, -t258, -t263)
      t285 = t3 * s
      t287 = t285 * t1 * x1
      t288 = t285 * t248
      t290 = t2 * x1 * x3
      t292 = t2 * t47 * x3
      t293 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t288, -t292, -t
     #287, t290, 0.0D0)
      t295 = t293 * t30 * t38
      t297 = t8 * t295 / 0.8D1
      t298 = FJET(XB1, XB2, s, -t287, t288, t290, -t292, 0.0D0, -t297)
      t303 = FJET(XB1, XB2, s, t290, -t292, -t287, t288, 0.0D0, -t297)
      t308 = FJET(XB1, XB2, s, -t292, t290, t288, -t287, 0.0D0, -t297)
      t319 = -(-0.90D2 * t8 * t22 + t28) * t30 / 0.1440D4 + t37 + t41
      t320 = FJET(XB1, XB2, s, 0.0D0, -t4, 0.0D0, t5, 0.0D0, t319)
      t322 = FJET(XB1, XB2, s, t85, 0.0D0, -t88, 0.0D0, 0.0D0, t115)
      t324 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t191)
      t326 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t191)
      t328 = FJET(XB1, XB2, s, 0.0D0, -t219, 0.0D0, t215, 0.0D0, t236)
      t333 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t191)
      t335 = FJET(XB1, XB2, s, 0.0D0, t5, 0.0D0, -t4, 0.0D0, t319)
      t337 = FJET(XB1, XB2, s, t49, -t48, 0.0D0, 0.0D0, 0.0D0, t81)
      t339 = FJET(XB1, XB2, s, t288, -t287, -t292, t290, 0.0D0, -t297)
      t344 = FJET(XB1, XB2, s, 0.0D0, t215, 0.0D0, -t219, 0.0D0, t236)
      t349 = -t280 * pi * t7 * t261 / 0.8D1 - t298 * pi * t7 * t295 / 0.
     #8D1 - t303 * pi * t7 * t295 / 0.8D1 - t308 * pi * t7 * t295 / 0.8D
     #1 + t320 * t319 + t322 * t115 + t324 * t191 + t326 * t191 + t328 *
     # pi * t239 * t234 / 0.16D2 + t333 * t191 + t335 * t319 + t337 * t8
     #1 - t339 * pi * t7 * t295 / 0.8D1 + t344 * pi * t239 * t234 / 0.16
     #D2
      rrgg2gght8s2em1 = t279 + t349

      end function



      doubleprecision function rrgg2gght8s2em2
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
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
      t45 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t44, 0.0D0, t42
     #, 0.0D0, 0.0D0)
      t48 = t5 * t45 * t15 / 0.8D1
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t42, -t44, 0.0D0, t48)
      t52 = t4 * t45 * t15
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t44, t42, 0.0D0, t48)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t37)
      t61 = t2 * x3
      t63 = t2 * (-0.1D1 + x3)
      t64 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t63, t61, 0.0D0
     #, 0.0D0, 0.0D0)
      t67 = t5 * t64 * t7 / 0.16D2
      t68 = FJET(XB1, XB2, s, 0.0D0, t61, 0.0D0, -t63, 0.0D0, t67)
      t71 = t4 * t64 * t7
      t75 = x2 * s * t1
      t78 = (-0.1D1 + x2) * s * t1
      t79 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t78, t75, 0.0D0
     #, 0.0D0, 0.0D0)
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
      rrgg2gght8s2em2 = t38 * t37 + t40 * t37 + t49 * pi * t52 / 0.8D1 +
     # t55 * pi * t52 / 0.8D1 + t59 * t37 + t68 * pi * t71 / 0.16D2 + t8
     #3 * pi * t86 / 0.16D2 + t89 * pi * t71 / 0.16D2 + t93 * pi * t86 /
     # 0.16D2 + t97 * t37 + t99 * pi * t52 / 0.8D1 + t103 * pi * t71 / 0
     #.16D2 + t107 * pi * t86 / 0.16D2 + t111 * pi * t71 / 0.16D2 + t115
     # * pi * t86 / 0.16D2 + t119 * pi * t52 / 0.8D1

      end function



      doubleprecision function rrgg2gght8s2em3
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.16D
     #2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s2em4
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght8s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght8s3e1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t21 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t27 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = cos(t8)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t15)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t41 = log(0.4D1 * t7 * t14)
      t43 = t41 ** 2
      t49 = pi * lh
      t59 = pi ** 2
      t61 = lh ** 2
      t63 = -0.30D2 * t59 + 0.180D3 * t61
      t64 = pi * t63
      t66 = t24 * z * t37
      t71 = 0.1D1 / x3
      t73 = 0.1D1 / x1
      t76 = t6 * t10
      t77 = t76 * t13
      t79 = log(0.4D1 * t77)
      t84 = t79 ** 2
      t87 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t100 = -0.240D3 * zeta3 - 0.120D3 * t61 * lh + 0.60D2 * lh * t59
      t101 = pi * t100
      t114 = x2 ** 2
      t115 = x3 * t114
      t118 = log(0.4D1 * t115 * t77)
      t120 = t115 * t6
      t121 = -0.1D1 + x2
      t122 = t14 * t121
      t125 = log(-0.4D1 * t120 * t122)
      t129 = log(-0.4D1 * t120 * t17)
      t137 = t49 * t4
      t142 = 0.1D1 / x2
      t143 = t73 * t142
      t146 = t114 * t6
      t149 = log(0.4D1 * t146 * t14)
      t151 = t149 ** 2
      t156 = log(-0.4D1 * t146 * t122)
      t158 = t156 ** 2
      t179 = (-0.180D3 * t21 * lh + 0.90D2 * t27 + t24 * t63) * pi
      t180 = x3 * t13
      t183 = log(0.4D1 * t180 * t10)
      t187 = log(-0.4D1 * t180 * t10 * t16)
      t193 = t24 * pi
      t194 = t187 ** 2
      t198 = t183 ** 2
      t220 = (-0.180D3 * t24 * lh + 0.90D2 * t21) * pi
      t232 = log(-0.4D1 * t115 * t122)
      t234 = t232 ** 2
      t239 = log(-0.4D1 * t115 * t17)
      t241 = t239 ** 2
      t249 = log(0.4D1 * t115 * t14)
      t251 = t249 ** 2
      t267 = t64 * t4
      t273 = t13 * t114
      t276 = log(0.4D1 * t273 * t10)
      t277 = t276 ** 2
      t278 = t10 * t121
      t281 = log(-0.4D1 * t273 * t278)
      t282 = t281 ** 2
      t301 = log(0.4D1 * t14)
      t302 = t301 ** 2
      t305 = t302 * t301
      t320 = t59 ** 2
      t321 = t61 ** 2
      t335 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t337 = t302 ** 2
      t344 = -(0.90D2 * t5 * ((-t20 * t21 + t23 * t24 / 0.2D1 + t27) * z
     # * t37 - t41 * t21 + t43 * t24 / 0.2D1 + t27) - 0.180D3 * t49 * t4
     # * ((t21 - t20 * t24) * z * t37 + t21 - t41 * t24) + t64 * t4 * (t
     #24 + t66)) * t71 * t73 / 0.1440D4 - (t64 * t4 * (t21 - t79 * t24) 
     #+ 0.90D2 * t5 * (t84 * t21 / 0.2D1 + t87 - t84 * t79 * t24 / 0.6D1
     # - t79 * t27) + t101 * t4 * t24 - 0.180D3 * t49 * t4 * (-t79 * t21
     # + t84 * t24 / 0.2D1 + t27)) * t73 / 0.1440D4 - (0.90D2 * t5 * (-t
     #118 * t24 + t125 * t24 + (t21 - t129 * t24) * z * t37) - 0.180D3 *
     # t137 * t66) * t71 * t143 / 0.720D3 - (0.90D2 * t5 * (-t149 * t21 
     #+ t151 * t24 / 0.2D1 + t156 * t21 - t158 * t24 / 0.2D1) - 0.180D3 
     #* t49 * t4 * (-t149 * t24 + t156 * t24)) * t73 * t142 / 0.720D3 - 
     #(t179 * t4 * (-t183 - t187 * z * t37) + 0.90D2 * t193 * t4 * (-t19
     #4 * t187 * z * t37 / 0.6D1 - t198 * t183 / 0.6D1) + (-0.180D3 * t2
     #7 * lh + t24 * t100 + 0.90D2 * t87 + t21 * t63) * pi * t4 * (0.1D1
     # + z * t37) + t220 * t4 * (t198 / 0.2D1 + t194 * z * t37 / 0.2D1))
     # * t71 / 0.2880D4 - (0.90D2 * t5 * (t232 * t21 - t234 * t24 / 0.2D
     #1 + (-t239 * t21 + t241 * t24 / 0.2D1 + t27) * z * t37 - t249 * t2
     #1 + t251 * t24 / 0.2D1) - 0.180D3 * t49 * t4 * (t232 * t24 + (t21 
     #- t239 * t24) * z * t37 - t249 * t24) + t267 * t66) * t71 * t142 /
     # 0.1440D4 - (t220 * t4 * (t277 / 0.2D1 - t282 / 0.2D1) + 0.90D2 * 
     #t193 * t4 * (t282 * t281 / 0.6D1 - t277 * t276 / 0.6D1) + t179 * t
     #4 * (-t276 + t281)) * t142 / 0.1440D4 - (-0.180D3 * (t302 * t21 / 
     #0.2D1 + t87 - t305 * t24 / 0.6D1 - t301 * t27) * lh + (-t301 * t21
     # + t302 * t24 / 0.2D1 + t27) * t63 + (t21 - t301 * t24) * t100 + t
     #24 * (t320 + 0.60D2 * t321 + 0.480D3 * lh * zeta3 - 0.60D2 * t61 *
     # t59) - 0.15D2 * t305 * t21 + 0.45D2 * t302 * t27 - 0.90D2 * t301 
     #* t87 + 0.90D2 * t335 + 0.15D2 / 0.4D1 * t337 * t24) * pi * t4 / 0
     #.2880D4
      t345 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t344)
      t347 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t344)
      t349 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t344)
      t351 = x2 * x3
      t352 = 0.1D1 - x3 + t351
      t353 = 0.1D1 / t352
      t354 = t351 * t353
      t355 = t2 * t354
      t357 = t2 * t15 * t353
      t359 = t352 ** 2
      t360 = 0.1D1 / t359
      t361 = t15 * t360
      t365 = log(0.4D1 * t115 * t13 * t278 * t361)
      t366 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t357, t355, 0.0D0)
      t368 = t365 ** 2
      t369 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t357, t355, 0.0D0)
      t372 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t357, t355, 0.0D0)
      t375 = t121 * t15
      t377 = Sqrt(t31 * t375)
      t381 = 0.1D1 / (-z - x3 + t351 + 0.2D1 * t30 * t377)
      t392 = t369 * z * t381
      t398 = t115 * t76
      t403 = log(0.4D1 * t398 * t13 * t121 * t361)
      t416 = -(-0.90D2 * t5 * (-t365 * t366 + t368 * t369 / 0.2D1 + t372
     #) * z * t381 + 0.180D3 * t137 * (t366 - t365 * t369) * z * t381 - 
     #t267 * t392) * t71 * t142 / 0.1440D4 - (-0.90D2 * t5 * (t366 - t40
     #3 * t369) * z * t381 + 0.180D3 * t137 * t392) * t71 * t143 / 0.720
     #D3
      t417 = FJET(XB1, XB2, s, 0.0D0, t355, 0.0D0, -t357, 0.0D0, t416)
      t420 = t1 * x1
      t421 = x1 * z
      t422 = -z - x1 + t421
      t423 = 0.1D1 / t422
      t425 = t121 * s * t420 * t423
      t426 = -0.1D1 + x1
      t427 = t2 * t426
      t429 = x2 * s * t420
      t430 = t1 ** 2
      t431 = s * t430
      t434 = x1 * t426 * t423
      t435 = t431 * t121 * t434
      t436 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t429, t425, -t4
     #27, 0.0D0, -t435)
      t437 = 0.1D1 / t11
      t438 = t426 ** 2
      t439 = t437 * t438
      t441 = t439 * t423 * t121
      t444 = log(0.4D1 * t398 * t441)
      t445 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t429, t425, -t4
     #27, 0.0D0, -t435)
      t450 = t4 * t445
      t456 = t146 * t10
      t459 = log(0.4D1 * t456 * t441)
      t461 = t459 ** 2
      t464 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t429, t425, -t4
     #27, 0.0D0, -t435)
      t478 = -(0.90D2 * t5 * (t436 - t444 * t445) - 0.180D3 * t49 * t450
     #) * t71 * t143 / 0.720D3 - (0.90D2 * t5 * (-t459 * t436 + t461 * t
     #445 / 0.2D1 + t464) - 0.180D3 * t49 * t4 * (t436 - t459 * t445) + 
     #t64 * t450) * t73 * t142 / 0.720D3
      t479 = FJET(XB1, XB2, s, 0.0D0, t425, -t427, t429, -t435, t478)
      t482 = t2 * x1 * t423
      t483 = t431 * t434
      t484 = t7 * t10
      t486 = t439 * t423 * t16
      t489 = log(0.4D1 * t484 * t486)
      t490 = t489 * z
      t491 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t482, -
     #t427, 0.0D0, t483)
      t494 = t489 ** 2
      t496 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t482, -
     #t427, 0.0D0, t483)
      t497 = t422 * t496
      t500 = z * t422
      t501 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t482, -
     #t427, 0.0D0, t483)
      t504 = x3 * x1
      t505 = t504 * z
      t506 = t6 * t11
      t507 = t506 * x3
      t508 = x1 * t11
      t509 = t504 * t11
      t511 = 0.2D1 * t7 * z
      t512 = z * t30
      t513 = x3 * t422
      t515 = Sqrt(t513 * t15)
      t519 = 0.1D1 / (-t421 - t505 - t507 + t508 + t509 + t511 - t31 - t
     #7 + 0.2D1 * t512 * t515 - t11)
      t521 = t439 * t423
      t524 = log(-0.4D1 * t484 * t521)
      t526 = t524 ** 2
      t532 = t500 * t491
      t544 = t4 * (t500 * t496 * t519 - t496)
      t549 = (0.90D2 * t5 * ((-t490 * t422 * t491 + t494 * z * t497 / 0.
     #2D1 + t500 * t501) * t519 + t524 * t491 - t526 * t496 / 0.2D1 - t5
     #01) - 0.180D3 * t49 * t4 * ((t532 - t490 * t497) * t519 - t491 + t
     #524 * t496) + t64 * t544) * t71 * t73 / 0.1440D4
      t552 = log(-0.4D1 * t76 * t521)
      t554 = -t491 + t552 * t496
      t557 = t552 ** 2
      t560 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t482, -
     #t427, 0.0D0, t483)
      t565 = -t557 * t491 / 0.2D1 - t560 + t557 * t552 * t496 / 0.6D1 + 
     #t552 * t501
      t568 = t4 * t496
      t569 = t101 * t568
      t573 = t552 * t491 - t557 * t496 / 0.2D1 - t501
      t582 = log(0.4D1 * t398 * t486)
      t588 = t438 * t423
      t592 = log(-0.4D1 * t120 * t10 * t437 * t588)
      t602 = (0.90D2 * t5 * ((t532 - t582 * z * t497) * t519 - t491 + t5
     #92 * t496) - 0.180D3 * t49 * t544) * t71 * t143 / 0.720D3
      t605 = log(-0.4D1 * t456 * t521)
      t607 = t605 ** 2
      t610 = t605 * t491 - t607 * t496 / 0.2D1 - t501
      t614 = -t491 + t605 * t496
      t618 = t64 * t568
      t622 = (0.90D2 * t5 * t610 - 0.180D3 * t49 * t4 * t614 - t618) * t
     #73 * t142 / 0.720D3
      t623 = -t549 - (t64 * t4 * t554 + 0.90D2 * t5 * t565 - t569 - 0.18
     #0D3 * t49 * t4 * t573) * t73 / 0.1440D4 - t602 - t622
      t624 = FJET(XB1, XB2, s, 0.0D0, -t427, -t482, 0.0D0, t483, t623)
      t626 = FJET(XB1, XB2, s, 0.0D0, -t482, -t427, 0.0D0, t483, t623)
      t628 = FJET(XB1, XB2, s, 0.0D0, -t357, 0.0D0, t355, 0.0D0, t416)
      t630 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t344)
      t632 = FJET(XB1, XB2, s, t429, -t427, t425, 0.0D0, -t435, t478)
      t634 = FJET(XB1, XB2, s, t355, 0.0D0, -t357, 0.0D0, 0.0D0, t416)
      t636 = FJET(XB1, XB2, s, t425, 0.0D0, t429, -t427, -t435, t478)
      t641 = t15 * s * t1 * t426 * t353
      t642 = t2 * x1
      t644 = Sqrt(-t513 * t375)
      t645 = t30 * t644
      t651 = t642 * x2 * (-x3 + t351 - z + t31 - x1 + t504 + t421 - t505
     # + 0.2D1 * t645) * t423 * t353
      t652 = t427 * t354
      t653 = t421 * t115
      t654 = t645 * x2
      t659 = x1 * t114 * x3
      t663 = t642 * (-x2 + t351 - t653 + 0.1D1 - x3 + 0.2D1 * t654 + z *
     # t114 * x3 + t659) * t423 * t353
      t664 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t651, -t663, t6
     #41, -t652, -t435)
      t671 = log(-0.4D1 * t115 * t76 * t437 * t588 * t375 * t360)
      t672 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t651, -t663, t6
     #41, -t652, -t435)
      t680 = x2 * z
      t688 = x2 * t6
      t689 = -0.2D1 * x1 * t30 * t644 * x2 + t653 - 0.2D1 * t504 * t680 
     #+ t504 * x2 * t11 + 0.2D1 * t7 * t680 - t506 * t351 + t688 + t505 
     #+ t507 - t509 - t511 - t508 + t31
      t694 = x1 * x2
      t695 = t694 * z
      t703 = t7 + t11 + 0.2D1 * t421 * t654 - t659 - 0.2D1 * t512 * t644
     # + t695 - t694 * t11 - 0.2D1 * t688 * z + t688 * t11 - t7 * x2 - t
     #351 * z + t504 * x2 + t421
      t705 = 0.1D1 / (t689 + t703)
      t707 = -z - t694 + t695
      t716 = -0.90D2 * t5 * (t664 - t671 * t672) * t705 * t422 * t707 + 
     #0.180D3 * t137 * t672 * t705 * t422 * t707
      t719 = t716 * t71 * t143 / 0.720D3
      t720 = FJET(XB1, XB2, s, t641, t651, -t652, -t663, -t435, -t719)
      t723 = t71 * t73 * t142
      t726 = FJET(XB1, XB2, s, t651, t641, -t663, -t652, -t435, -t719)
      t742 = (t64 * t4 * t554 + 0.90D2 * t5 * t565 - t569 - 0.180D3 * t4
     #9 * t4 * t573) * t73 / 0.1440D4
      t743 = -t549 - t742 - t602 - t622
      t744 = FJET(XB1, XB2, s, -t427, 0.0D0, 0.0D0, -t482, t483, t743)
      t746 = FJET(XB1, XB2, s, -t427, t429, 0.0D0, t425, -t435, t478)
      t759 = -t549 - t742 - t602 - (0.90D2 * t5 * t610 - 0.180D3 * t49 *
     # t4 * t614 - t618) * t73 * t142 / 0.720D3
      t760 = FJET(XB1, XB2, s, -t482, 0.0D0, 0.0D0, -t427, t483, t759)
      t762 = FJET(XB1, XB2, s, -t357, 0.0D0, t355, 0.0D0, 0.0D0, t416)
      t764 = FJET(XB1, XB2, s, -t663, -t652, t651, t641, -t435, -t719)
      t768 = FJET(XB1, XB2, s, -t652, -t663, t641, t651, -t435, -t719)
      rrgg2gght8s3e1 = t345 * t344 + t347 * t344 + t349 * t344 + t417 * 
     #t416 + t479 * t478 + t624 * t623 + t626 * t623 + t628 * t416 + t63
     #0 * t344 + t632 * t478 + t634 * t416 + t636 * t478 - t720 * t716 *
     # t723 / 0.720D3 - t726 * t716 * t723 / 0.720D3 + t744 * t743 + t74
     #6 * t478 + t760 * t759 + t762 * t416 - t764 * t716 * t723 / 0.720D
     #3 - t768 * t716 * t723 / 0.720D3

      end function



      doubleprecision function rrgg2gght8s3e0
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t6 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
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
      t22 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
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
      t87 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t96 = pi ** 2
      t98 = lh ** 2
      t100 = -0.30D2 * t96 + 0.180D3 * t98
      t101 = pi * t100
      t107 = t22 * pi
      t108 = x3 * t14
      t111 = log(0.4D1 * t108 * t11)
      t112 = t111 ** 2
      t116 = log(-0.4D1 * t108 * t11 * t17)
      t117 = t116 ** 2
      t129 = (-0.180D3 * t22 * lh + 0.90D2 * t6) * pi
      t149 = log(0.4D1 * t15)
      t151 = t149 ** 2
      t166 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t180 = x3 * t62
      t183 = log(-0.4D1 * t180 * t69)
      t187 = log(-0.4D1 * t180 * t18)
      t194 = log(0.4D1 * t180 * t15)
      t199 = t42 * t4
      t206 = t14 * t62
      t209 = log(0.4D1 * t206 * t11)
      t210 = t209 ** 2
      t211 = t11 * t68
      t214 = log(-0.4D1 * t206 * t211)
      t215 = t214 ** 2
      t227 = -(0.90D2 * t5 * ((t6 - t21 * t22) * z * t33 + t6 - t37 * t2
     #2) - 0.180D3 * t42 * t4 * (t22 + t44)) * t50 * t52 / 0.1440D4 - t5
     # * t43 * t33 * t50 * t58 / 0.8D1 - t5 * (-t66 * t22 + t72 * t22) *
     # t52 * t57 / 0.8D1 - (0.90D2 * t5 * (-t82 * t6 + t84 * t22 / 0.2D1
     # + t87) - 0.180D3 * t42 * t4 * (t6 - t82 * t22) + t101 * t4 * t22)
     # * t52 / 0.1440D4 - (0.90D2 * t107 * t4 * (t112 / 0.2D1 + t117 * z
     # * t33 / 0.2D1) + t129 * t4 * (-t111 - t116 * z * t33) + (-0.180D3
     # * t6 * lh + 0.90D2 * t87 + t22 * t100) * pi * t4 * (0.1D1 + z * t
     #33)) * t50 / 0.2880D4 - (-0.180D3 * (-t149 * t6 + t151 * t22 / 0.2
     #D1 + t87) * lh + t22 * (-0.240D3 * zeta3 - 0.120D3 * t98 * lh + 0.
     #60D2 * lh * t96) + 0.45D2 * t151 * t6 + 0.90D2 * t166 - 0.15D2 * t
     #151 * t149 * t22 - 0.90D2 * t149 * t87 + (t6 - t149 * t22) * t100)
     # * pi * t4 / 0.2880D4 - (0.90D2 * t5 * (t183 * t22 + (t6 - t187 * 
     #t22) * z * t33 - t194 * t22) - 0.180D3 * t199 * t44) * t50 * t57 /
     # 0.1440D4 - (0.90D2 * t107 * t4 * (t210 / 0.2D1 - t215 / 0.2D1) + 
     #t129 * t4 * (-t209 + t214)) * t57 / 0.1440D4
      t228 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t227)
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t227)
      t232 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t227)
      t234 = x2 * x3
      t235 = 0.1D1 - x3 + t234
      t236 = 0.1D1 / t235
      t237 = t234 * t236
      t238 = t2 * t237
      t240 = t2 * t16 * t236
      t241 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t240, t238, 0.0D0)
      t242 = t241 * z
      t244 = t68 * t16
      t246 = Sqrt(t27 * t244)
      t250 = 0.1D1 / (-z - x3 + t234 + 0.2D1 * t26 * t246)
      t255 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t240, t238, 0.0D0)
      t257 = t235 ** 2
      t263 = log(0.4D1 * t180 * t14 * t211 * t16 / t257)
      t277 = t5 * t242 * t250 * t50 * t58 / 0.8D1 - (-0.90D2 * t5 * (t25
     #5 - t263 * t241) * z * t250 + 0.180D3 * t199 * t242 * t250) * t50 
     #* t57 / 0.1440D4
      t278 = FJET(XB1, XB2, s, 0.0D0, t238, 0.0D0, -t240, 0.0D0, t277)
      t281 = t1 * x1
      t282 = x1 * z
      t283 = -z - x1 + t282
      t284 = 0.1D1 / t283
      t286 = t68 * s * t281 * t284
      t287 = -0.1D1 + x1
      t288 = t2 * t287
      t290 = x2 * s * t281
      t291 = t1 ** 2
      t292 = s * t291
      t295 = x1 * t287 * t284
      t296 = t292 * t68 * t295
      t297 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t290, t286, -t2
     #88, 0.0D0, -t296)
      t300 = t50 * t52 * t57
      t303 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t290, t286, -t2
     #88, 0.0D0, -t296)
      t304 = t63 * t11
      t305 = 0.1D1 / t12
      t307 = t287 ** 2
      t312 = log(0.4D1 * t304 * t305 * t284 * t307 * t68)
      t324 = -t5 * t297 * t300 / 0.8D1 - (0.90D2 * t5 * (t303 - t312 * t
     #297) - 0.180D3 * t42 * t4 * t297) * t52 * t57 / 0.720D3
      t325 = FJET(XB1, XB2, s, 0.0D0, t286, -t288, t290, -t296, t324)
      t328 = t2 * x1 * t284
      t329 = t292 * t295
      t330 = z * t283
      t331 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t328, -
     #t288, 0.0D0, t329)
      t333 = t8 * t11
      t334 = t305 * t307
      t339 = log(0.4D1 * t333 * t334 * t284 * t17)
      t341 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t328, -
     #t288, 0.0D0, t329)
      t345 = x3 * x1
      t346 = t345 * z
      t347 = t7 * t12
      t348 = t347 * x3
      t349 = x1 * t12
      t350 = t345 * t12
      t352 = 0.2D1 * t8 * z
      t353 = z * t26
      t354 = x3 * t283
      t356 = Sqrt(t354 * t16)
      t360 = 0.1D1 / (-t282 - t346 - t348 + t349 + t350 + t352 - t27 - t
     #8 + 0.2D1 * t353 * t356 - t12)
      t362 = t334 * t284
      t365 = log(-0.4D1 * t333 * t362)
      t372 = t330 * t341 * t360 - t341
      t379 = (0.90D2 * t5 * ((t330 * t331 - t339 * z * t283 * t341) * t3
     #60 - t331 + t365 * t341) - 0.180D3 * t42 * t4 * t372) * t50 * t52 
     #/ 0.1440D4
      t382 = t5 * t372 * t300 / 0.8D1
      t385 = log(-0.4D1 * t304 * t362)
      t387 = -t331 + t385 * t341
      t390 = t4 * t341
      t392 = 0.180D3 * t42 * t390
      t396 = (0.90D2 * t5 * t387 + t392) * t52 * t57 / 0.720D3
      t399 = log(-0.4D1 * t79 * t362)
      t401 = t399 ** 2
      t404 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t328, -
     #t288, 0.0D0, t329)
      t405 = t399 * t331 - t401 * t341 / 0.2D1 - t404
      t409 = -t331 + t399 * t341
      t413 = t101 * t390
      t417 = -t379 - t382 - t396 - (0.90D2 * t5 * t405 - 0.180D3 * t42 *
     # t4 * t409 - t413) * t52 / 0.1440D4
      t418 = FJET(XB1, XB2, s, 0.0D0, -t288, -t328, 0.0D0, t329, t417)
      t420 = FJET(XB1, XB2, s, 0.0D0, -t328, -t288, 0.0D0, t329, t417)
      t422 = FJET(XB1, XB2, s, 0.0D0, -t240, 0.0D0, t238, 0.0D0, t277)
      t424 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t227)
      t426 = FJET(XB1, XB2, s, t290, -t288, t286, 0.0D0, -t296, t324)
      t428 = FJET(XB1, XB2, s, t238, 0.0D0, -t240, 0.0D0, 0.0D0, t277)
      t430 = FJET(XB1, XB2, s, t286, 0.0D0, t290, -t288, -t296, t324)
      t435 = t16 * s * t1 * t287 * t236
      t436 = t2 * x1
      t438 = Sqrt(-t354 * t244)
      t439 = t26 * t438
      t445 = t436 * x2 * (-x3 + t234 - z + t27 - x1 + t345 + t282 - t346
     # + 0.2D1 * t439) * t284 * t236
      t446 = t288 * t237
      t447 = t282 * t180
      t448 = t439 * x2
      t453 = x1 * t62 * x3
      t457 = t436 * (-x2 + t234 - t447 + 0.1D1 - x3 + 0.2D1 * t448 + z *
     # t62 * x3 + t453) * t284 * t236
      t458 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t445, -t457, t4
     #35, -t446, -t296)
      t461 = x1 * x2
      t462 = t461 * z
      t464 = t7 * x2
      t471 = t346 + t348 - t350 - t352 - t453 - 0.2D1 * t353 * t438 + t4
     #62 - t461 * t12 - 0.2D1 * t464 * z + t464 * t12 - t8 * x2 - t234 *
     # z + t345 * x2
      t474 = x2 * z
      t486 = t12 + 0.2D1 * t282 * t448 + t447 - 0.2D1 * t345 * t474 + t3
     #45 * x2 * t12 + 0.2D1 * t8 * t474 - t347 * t234 - 0.2D1 * x1 * t26
     # * t438 * x2 + t464 + t282 - t349 + t27 + t8
      t488 = 0.1D1 / (t471 + t486)
      t493 = t283 * (-z - t461 + t462) * t300
      t495 = t5 * t458 * t488 * t493 / 0.8D1
      t496 = FJET(XB1, XB2, s, t435, t445, -t446, -t457, -t296, t495)
      t499 = t4 * t458 * t488
      t503 = FJET(XB1, XB2, s, t445, t435, -t457, -t446, -t296, t495)
      t517 = (0.90D2 * t5 * t405 - 0.180D3 * t42 * t4 * t409 - t413) * t
     #52 / 0.1440D4
      t518 = -t379 - t382 - t396 - t517
      t519 = FJET(XB1, XB2, s, -t288, 0.0D0, 0.0D0, -t328, t329, t518)
      t521 = FJET(XB1, XB2, s, -t288, t290, 0.0D0, t286, -t296, t324)
      t530 = -t379 - t382 - (0.90D2 * t5 * t387 + t392) * t52 * t57 / 0.
     #720D3 - t517
      t531 = FJET(XB1, XB2, s, -t328, 0.0D0, 0.0D0, -t288, t329, t530)
      t533 = FJET(XB1, XB2, s, -t240, 0.0D0, t238, 0.0D0, 0.0D0, t277)
      t535 = FJET(XB1, XB2, s, -t457, -t446, t445, t435, -t296, t495)
      t540 = FJET(XB1, XB2, s, -t446, -t457, t435, t445, -t296, t495)
      rrgg2gght8s3e0 = t228 * t227 + t230 * t227 + t232 * t227 + t278 * 
     #t277 + t325 * t324 + t418 * t417 + t420 * t417 + t422 * t277 + t42
     #4 * t227 + t426 * t324 + t428 * t277 + t430 * t324 + t496 * pi * t
     #499 * t493 / 0.8D1 + t503 * pi * t499 * t493 / 0.8D1 + t519 * t518
     # + t521 * t324 + t531 * t530 + t533 * t277 + t535 * pi * t499 * t4
     #93 / 0.8D1 + t540 * pi * t499 * t493 / 0.8D1

      end function



      doubleprecision function rrgg2gght8s3em1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = -0.1D1 + x3
      t22 = log(-0.4D1 * t10 * t13 / t17)
      t24 = cos(t11)
      t25 = x3 * z
      t27 = Sqrt(-t25 * t17)
      t31 = 0.1D1 / (-z - x3 + 0.2D1 * t24 * t27)
      t39 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t43 = z * t31
      t48 = 0.1D1 / x3
      t51 = pi * t6
      t52 = x1 ** 2
      t53 = t52 * t13
      t56 = log(0.4D1 * t53 * t9)
      t61 = pi * lh
      t66 = 0.1D1 / x1
      t77 = 0.1D1 / x2
      t78 = t48 * t77
      t82 = x2 ** 2
      t83 = t9 * t82
      t86 = log(0.4D1 * t83 * t13)
      t87 = -0.1D1 + x2
      t91 = log(-0.4D1 * t83 * t13 * t87)
      t99 = log(0.4D1 * t9 * t13)
      t106 = t99 ** 2
      t109 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t111 = pi ** 2
      t113 = lh ** 2
      t121 = -(0.90D2 * t4 * t6 * (-t16 - t22 * z * t31) + (-0.180D3 * t
     #3 * lh + 0.90D2 * t39) * pi * t6 * (0.1D1 + t43)) * t48 / 0.2880D4
     # - (0.90D2 * t51 * (t39 - t56 * t3) - 0.180D3 * t61 * t6 * t3) * t
     #66 / 0.1440D4 - t51 * (t3 + t3 * z * t31) * t48 * t66 / 0.16D2 - t
     #51 * t3 * t43 * t78 / 0.16D2 - t4 * t6 * (-t86 + t91) * t77 / 0.16
     #D2 - (-0.180D3 * (t39 - t99 * t3) * lh - 0.90D2 * t99 * t39 + 0.45
     #D2 * t106 * t3 + 0.90D2 * t109 + t3 * (-0.30D2 * t111 + 0.180D3 * 
     #t113)) * pi * t6 / 0.2880D4
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t124 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t126 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t128 = x2 * x3
      t130 = 0.1D1 / (0.1D1 - x3 + t128)
      t132 = t2 * t128 * t130
      t134 = t2 * t17 * t130
      t135 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t134, t132, 0.0D0)
      t139 = Sqrt(t25 * t87 * t17)
      t145 = z / (-z - x3 + t128 + 0.2D1 * t24 * t139) * t78
      t147 = t51 * t135 * t145 / 0.16D2
      t148 = FJET(XB1, XB2, s, 0.0D0, t132, 0.0D0, -t134, 0.0D0, t147)
      t150 = t6 * t135
      t155 = t1 * x1
      t156 = x1 * z
      t157 = -z - x1 + t156
      t158 = 0.1D1 / t157
      t160 = t87 * s * t155 * t158
      t161 = -0.1D1 + x1
      t162 = t2 * t161
      t164 = x2 * s * t155
      t165 = t1 ** 2
      t166 = s * t165
      t169 = x1 * t161 * t158
      t170 = t166 * t87 * t169
      t171 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t164, t160, -t1
     #62, 0.0D0, -t170)
      t173 = t171 * t66 * t77
      t175 = t51 * t173 / 0.8D1
      t176 = FJET(XB1, XB2, s, 0.0D0, t160, -t162, t164, -t170, -t175)
      t182 = t2 * x1 * t158
      t183 = t166 * t169
      t184 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t182, -
     #t162, 0.0D0, t183)
      t188 = t51 * t184 * t66 * t77 / 0.8D1
      t189 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t182, -
     #t162, 0.0D0, t183)
      t192 = t161 ** 2
      t196 = log(-0.4D1 * t53 / t7 * t158 * t192)
      t198 = -t189 + t196 * t184
      t203 = 0.180D3 * t61 * t6 * t184
      t208 = x3 * x1
      t214 = x3 * t52
      t220 = Sqrt(x3 * t157 * t17)
      t231 = t51 * (z * t157 * t184 / (-t156 - t208 * z - t52 * t7 * x3 
     #+ x1 * t7 + t208 * t7 + 0.2D1 * t214 * z - t25 - t214 + 0.2D1 * z 
     #* t24 * t220 - t7) - t184) * t48 * t66 / 0.16D2
      t232 = t188 - (0.90D2 * t51 * t198 + t203) * t66 / 0.1440D4 - t231
      t233 = FJET(XB1, XB2, s, 0.0D0, -t162, -t182, 0.0D0, t183, t232)
      t235 = FJET(XB1, XB2, s, 0.0D0, -t182, -t162, 0.0D0, t183, t232)
      t237 = FJET(XB1, XB2, s, 0.0D0, -t134, 0.0D0, t132, 0.0D0, t147)
      t242 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t244 = FJET(XB1, XB2, s, t164, -t162, t160, 0.0D0, -t170, -t175)
      t249 = FJET(XB1, XB2, s, t132, 0.0D0, -t134, 0.0D0, 0.0D0, t147)
      t254 = FJET(XB1, XB2, s, t160, 0.0D0, t164, -t162, -t170, -t175)
      t265 = t188 - (0.90D2 * t51 * t198 + t203) * t66 / 0.1440D4 - t231
      t266 = FJET(XB1, XB2, s, -t162, 0.0D0, 0.0D0, -t182, t183, t265)
      t268 = FJET(XB1, XB2, s, -t162, t164, 0.0D0, t160, -t170, -t175)
      t273 = FJET(XB1, XB2, s, -t182, 0.0D0, 0.0D0, -t162, t183, t265)
      t275 = FJET(XB1, XB2, s, -t134, 0.0D0, t132, 0.0D0, 0.0D0, t147)
      rrgg2gght8s3em1 = t122 * t121 + t124 * t121 + t126 * t121 + t148 *
     # pi * t150 * t145 / 0.16D2 - t176 * pi * t6 * t173 / 0.8D1 + t233 
     #* t232 + t235 * t232 + t237 * pi * t150 * t145 / 0.16D2 + t242 * t
     #121 - t244 * pi * t6 * t173 / 0.8D1 + t249 * pi * t150 * t145 / 0.
     #16D2 - t254 * pi * t6 * t173 / 0.8D1 + t266 * t265 - t268 * pi * t
     #6 * t173 / 0.8D1 + t273 * t265 + t275 * pi * t150 * t145 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s3em2
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = 0.1D1 / x1
      t11 = x4 * pi
      t12 = cos(t11)
      t16 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t30 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t32 = z ** 2
      t35 = Sin(t11)
      t36 = t35 ** 2
      t39 = log(0.4D1 / t32 / z * t36)
      t46 = -t4 * t6 * t7 / 0.16D2 - t4 * t6 * (0.1D1 + z / (-z - x3 + 0
     #.2D1 * t12 * t16)) / x3 / 0.32D2 - (-0.180D3 * t3 * lh + 0.90D2 * 
     #t30 - 0.90D2 * t39 * t3) * pi * t6 / 0.2880D4
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t46)
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t46)
      t51 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t46)
      t53 = -0.1D1 + x1
      t54 = t2 * t53
      t57 = 0.1D1 / (-z - x1 + x1 * z)
      t59 = t2 * x1 * t57
      t60 = t1 ** 2
      t64 = s * t60 * x1 * t53 * t57
      t66 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t59, -t5
     #4, 0.0D0, t64)
      t69 = pi * t6 * t66 * t7 / 0.16D2
      t70 = FJET(XB1, XB2, s, 0.0D0, -t54, -t59, 0.0D0, t64, t69)
      t73 = t6 * t66 * t7
      t76 = FJET(XB1, XB2, s, 0.0D0, -t59, -t54, 0.0D0, t64, t69)
      t80 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, -t54, 0.0D0, 0.0D0, -t59, t64, t69)
      t86 = FJET(XB1, XB2, s, -t59, 0.0D0, 0.0D0, -t54, t64, t69)
      rrgg2gght8s3em2 = t47 * t46 + t49 * t46 + t51 * t46 + t70 * pi * t
     #73 / 0.16D2 + t76 * pi * t73 / 0.16D2 + t80 * t46 + t82 * pi * t73
     # / 0.16D2 + t86 * pi * t73 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s3em3
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * pi * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = pi * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s3em3 = -t9 * t3 * t11 / 0.32D2 - t13 * t3 * t11 / 0.32D
     #2 - t16 * t3 * t11 / 0.32D2 - t19 * t3 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2gght8s3em4
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght8s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght8s4e1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = t2 * x1
      t4 = x2 * x3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = x1 * z
      t8 = -z - x1 + t7
      t10 = -0.1D1 + x3
      t11 = x2 * t10
      t13 = Sqrt(x3 * t8 * t11)
      t14 = t6 * t13
      t17 = x3 * x1
      t18 = x2 * z
      t21 = x2 ** 2
      t23 = t7 * t21 * x3
      t24 = x3 * z
      t28 = x1 * t21 * x3
      t29 = t4 * z
      t31 = t17 * x2
      t33 = t17 * z
      t34 = 0.2D1 * t14
      t35 = t4 + 0.2D1 * t14 * x2 - 0.2D1 * t17 * t18 + t23 - t24 - t17 
     #- x2 - z * t21 * x3 - t28 + 0.2D1 * t29 + 0.2D1 * t31 + t33 - t34
      t36 = 0.1D1 / t8
      t38 = t4 - 0.1D1
      t39 = 0.1D1 / t38
      t41 = t3 * t35 * t36 * t39
      t42 = -0.1D1 + x1
      t43 = t2 * t42
      t44 = -0.1D1 + x2
      t46 = x3 * t44 * t39
      t47 = t43 * t46
      t52 = t3 * t44 * (-t4 - z + t24 - x1 + t17 + t7 - t33 + t34) * t36
     # * t39
      t53 = t10 * s
      t54 = t1 * t42
      t56 = t53 * t54 * t39
      t57 = t1 ** 2
      t62 = s * t57 * x2 * x1 * t42 * t36
      t63 = s ** 2
      t64 = 0.1D1 / t63
      t65 = pi * t64
      t66 = x1 * x2
      t67 = t66 * z
      t68 = z - t66 + x1 + t67 - t7
      t69 = t8 * t68
      t70 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t52, -t41, -t56,
     # -t47, t62)
      t72 = x1 ** 2
      t73 = Sin(t5)
      t74 = t73 ** 2
      t75 = t72 * t74
      t76 = z ** 2
      t77 = 0.1D1 / t76
      t80 = t42 ** 2
      t81 = t80 * t36
      t82 = t44 ** 2
      t84 = t38 ** 2
      t85 = 0.1D1 / t84
      t90 = log(0.4D1 * t4 * t75 * t77 * t81 * t82 * t10 * t85)
      t92 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t52, -t41, -t56,
     # -t47, t62)
      t98 = x3 * t72
      t101 = t72 * t76
      t105 = x2 * t72
      t113 = t17 * x2 * t76 + 0.2D1 * t98 * t18 - t23 - t101 * t4 + t67 
     #- 0.2D1 * t7 - t66 * t76 - 0.2D1 * t105 * z + t105 * t76 + 0.2D1 *
     # x1 * t76 - t72 + 0.2D1 * t72 * z
      t114 = t98 * x2
      t116 = z * t6 * t13
      t127 = -t101 + t105 - t114 + t28 - t29 - t31 + 0.2D1 * t116 + 0.2D
     #1 * x1 * t6 * t13 - t76 - 0.2D1 * t66 * t14 - 0.2D1 * t7 * t14 + 0
     #.2D1 * t66 * t116
      t129 = 0.1D1 / (t113 + t127)
      t133 = pi * lh
      t134 = t133 * t64
      t139 = -0.90D2 * t65 * (t69 * t70 - t90 * t8 * t68 * t92) * t129 +
     # 0.180D3 * t134 * t69 * t92 * t129
      t140 = 0.1D1 / x3
      t142 = 0.1D1 / x1
      t143 = 0.1D1 / x2
      t144 = t142 * t143
      t146 = t139 * t140 * t144 / 0.720D3
      t147 = FJET(XB1, XB2, s, -t41, -t47, t52, -t56, t62, -t146)
      t150 = t140 * t142 * t143
      t153 = t98 * t74
      t154 = t77 * t80
      t155 = t154 * t36
      t158 = log(-0.4D1 * t153 * t155)
      t159 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t43
     #, 0.0D0, 0.0D0)
      t161 = t158 ** 2
      t162 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t43
     #, 0.0D0, 0.0D0)
      t165 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t43
     #, 0.0D0, 0.0D0)
      t174 = pi ** 2
      t175 = 0.30D2 * t174
      t176 = lh ** 2
      t177 = 0.180D3 * t176
      t178 = -t175 + t177
      t179 = pi * t178
      t180 = t64 * t162
      t181 = t179 * t180
      t184 = (0.90D2 * t65 * (t158 * t159 - t161 * t162 / 0.2D1 - t165) 
     #- 0.180D3 * t133 * t64 * (-t159 + t158 * t162) - t181) * t140 * t1
     #42
      t187 = log(-0.4D1 * t75 * t155)
      t189 = -t159 + t187 * t162
      t192 = t187 ** 2
      t195 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t43
     #, 0.0D0, 0.0D0)
      t200 = -t192 * t159 / 0.2D1 - t195 + t192 * t187 * t162 / 0.6D1 + 
     #t187 * t165
      t203 = 0.240D3 * zeta3
      t205 = 0.120D3 * t176 * lh
      t207 = 0.60D2 * lh * t174
      t208 = -t203 - t205 + t207
      t209 = pi * t208
      t210 = t209 * t180
      t214 = t187 * t159 - t192 * t162 / 0.2D1 - t165
      t224 = log(-0.4D1 * t114 * t74 * t77 * t81)
      t233 = (0.90D2 * t65 * (-t159 + t224 * t162) + 0.180D3 * t133 * t1
     #80) * t140 * t144
      t234 = t105 * t74
      t237 = log(-0.4D1 * t234 * t155)
      t239 = t237 ** 2
      t252 = (0.90D2 * t65 * (-t237 * t159 + t239 * t162 / 0.2D1 + t165)
     # - 0.180D3 * t133 * t64 * (t159 - t237 * t162) + t181) * t142 * t1
     #43
      t254 = -t184 / 0.720D3 - (t179 * t64 * t189 + 0.90D2 * t65 * t200 
     #- t210 - 0.180D3 * t133 * t64 * t214) * t142 / 0.720D3 - t233 / 0.
     #720D3 + t252 / 0.720D3
      t255 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t43, t3, 0.0D0, t254)
      t257 = t2 * t17
      t259 = t2 * t42 * x3
      t260 = t1 * x1
      t261 = t53 * t260
      t262 = t53 * t54
      t265 = t77 * t36 * t80 * t10
      t268 = log(0.4D1 * t153 * t265)
      t269 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t261, t257, t2
     #62, -t259, 0.0D0)
      t271 = t268 ** 2
      t272 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t261, t257, t2
     #62, -t259, 0.0D0)
      t275 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t261, t257, t2
     #62, -t259, 0.0D0)
      t284 = t64 * t272
      t289 = t4 * t75
      t292 = log(0.4D1 * t289 * t265)
      t303 = -(0.90D2 * t65 * (-t268 * t269 + t271 * t272 / 0.2D1 + t275
     #) - 0.180D3 * t133 * t64 * (t269 - t268 * t272) + t179 * t284) * t
     #140 * t142 / 0.720D3 - (0.90D2 * t65 * (t269 - t292 * t272) - 0.18
     #0D3 * t133 * t284) * t140 * t144 / 0.720D3
      t304 = FJET(XB1, XB2, s, t257, -t259, -t261, t262, 0.0D0, t303)
      t306 = t2 * t46
      t308 = t2 * t10 * t39
      t309 = t85 * t10
      t311 = 0.1D1 / t76 / z
      t313 = t82 * t74
      t314 = t313 * t4
      t317 = log(-0.4D1 * t309 * t311 * t314)
      t319 = Sqrt(-t24 * t11)
      t323 = 0.1D1 / (-z - t4 + 0.2D1 * t6 * t319)
      t324 = t317 * t323
      t325 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #308, t306, 0.0D0)
      t328 = t323 * z
      t329 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #308, t306, 0.0D0)
      t331 = t317 ** 2
      t333 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #308, t306, 0.0D0)
      t334 = z * t333
      t341 = t328 * t325
      t347 = t328 * t333
      t357 = log(-0.4D1 * t309 * t311 * t72 * t314)
      t369 = -(0.90D2 * t65 * (t324 * z * t325 - t328 * t329 - t331 * t3
     #23 * t334 / 0.2D1) - 0.180D3 * t133 * t64 * (t324 * t334 - t341) -
     # t179 * t64 * t347) * t140 * t143 / 0.1440D4 - (0.90D2 * t65 * (t3
     #57 * t323 * t334 - t341) + 0.180D3 * t134 * t347) * t140 * t144 / 
     #0.720D3
      t370 = FJET(XB1, XB2, s, t306, 0.0D0, t308, 0.0D0, 0.0D0, t369)
      t372 = t311 * t74
      t375 = log(0.4D1 * t98 * t372)
      t376 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t378 = t375 ** 2
      t379 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t382 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t391 = t64 * t379
      t397 = t75 * t311
      t399 = log(0.4D1 * t397)
      t404 = t399 ** 2
      t407 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t415 = t209 * t391
      t428 = log(0.4D1 * t4 * t397)
      t430 = t372 * t82
      t433 = log(0.4D1 * t114 * t430)
      t441 = log(0.4D1 * t105 * t372)
      t443 = t441 ** 2
      t448 = log(0.4D1 * t105 * t430)
      t450 = t448 ** 2
      t466 = x3 * t311
      t469 = log(0.4D1 * t466 * t74)
      t474 = t469 ** 2
      t496 = log(0.4D1 * t4 * t430)
      t498 = t496 ** 2
      t503 = log(0.4D1 * t4 * t372)
      t505 = t503 ** 2
      t521 = x2 * t311
      t524 = log(0.4D1 * t521 * t313)
      t526 = t524 ** 2
      t531 = log(0.4D1 * t521 * t74)
      t533 = t531 ** 2
      t564 = log(0.4D1 * t372)
      t565 = t564 ** 2
      t568 = t565 * t564
      t591 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t599 = t174 ** 2
      t600 = t176 ** 2
      t606 = t565 ** 2
      t612 = -(0.90D2 * t65 * (-t375 * t376 + t378 * t379 / 0.2D1 + t382
     #) - 0.180D3 * t133 * t64 * (t376 - t375 * t379) + t179 * t391) * t
     #140 * t142 / 0.720D3 - (t179 * t64 * (t376 - t399 * t379) + 0.90D2
     # * t65 * (t404 * t376 / 0.2D1 + t407 - t404 * t399 * t379 / 0.6D1 
     #- t399 * t382) + t415 - 0.180D3 * t133 * t64 * (-t399 * t376 + t40
     #4 * t379 / 0.2D1 + t382)) * t142 / 0.720D3 - t65 * (-t428 * t379 +
     # t433 * t379) * t150 / 0.8D1 + (0.90D2 * t65 * (t441 * t376 - t443
     # * t379 / 0.2D1 - t448 * t376 + t450 * t379 / 0.2D1) - 0.180D3 * t
     #133 * t64 * (t441 * t379 - t448 * t379)) * t142 * t143 / 0.720D3 -
     # (t179 * t64 * (t376 - t469 * t379) + 0.90D2 * t65 * (t474 * t376 
     #/ 0.2D1 + t407 - t474 * t469 * t379 / 0.6D1 - t469 * t382) + t415 
     #- 0.180D3 * t133 * t64 * (-t469 * t376 + t474 * t379 / 0.2D1 + t38
     #2)) * t140 / 0.1440D4 - (0.90D2 * t65 * (t496 * t376 - t498 * t379
     # / 0.2D1 - t503 * t376 + t505 * t379 / 0.2D1) - 0.180D3 * t133 * t
     #64 * (t496 * t379 - t503 * t379)) * t140 * t143 / 0.1440D4 + (-0.1
     #80D3 * t133 * t64 * (-t524 * t376 + t526 * t379 / 0.2D1 + t531 * t
     #376 - t533 * t379 / 0.2D1) + 0.90D2 * t65 * (-t533 * t376 / 0.2D1 
     #+ t533 * t531 * t379 / 0.6D1 + t531 * t382 + t526 * t376 / 0.2D1 -
     # t526 * t524 * t379 / 0.6D1 - t524 * t382) + t179 * t64 * (-t524 *
     # t379 + t531 * t379)) * t143 / 0.1440D4 - (-0.90D2 * t565 * lh - t
     #203 - t205 + t207 - 0.15D2 * t568 - t564 * t178) * pi * t64 * t376
     # / 0.1440D4 - (0.180D3 * t564 * lh + 0.45D2 * t565 - t175 + t177) 
     #* pi * t64 * t382 / 0.1440D4 - (-0.180D3 * lh - 0.90D2 * t564) * p
     #i * t64 * t407 / 0.1440D4 - t65 * t591 / 0.16D2 - (0.30D2 * t568 *
     # lh + t565 * t178 / 0.2D1 - t564 * t208 + t599 + 0.60D2 * t600 + 0
     #.480D3 * lh * zeta3 - 0.60D2 * t176 * t174 + 0.15D2 / 0.4D1 * t606
     #) * pi * t391 / 0.1440D4
      t613 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t612)
      t628 = -t184 / 0.720D3 - (t179 * t64 * t189 + 0.90D2 * t65 * t200 
     #- t210 - 0.180D3 * t133 * t64 * t214) * t142 / 0.720D3 - t233 / 0.
     #720D3 + t252 / 0.720D3
      t629 = FJET(XB1, XB2, s, -t43, t3, 0.0D0, 0.0D0, 0.0D0, t628)
      t631 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t612)
      t633 = FJET(XB1, XB2, s, -t259, t257, t262, -t261, 0.0D0, t303)
      t635 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t612)
      t637 = FJET(XB1, XB2, s, t52, -t56, -t41, -t47, t62, -t146)
      t642 = t2 * t66 * t36
      t644 = t44 * s * t260
      t645 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t644, -t642, -
     #t43, 0.0D0, t62)
      t647 = t154 * t36 * t82
      t650 = log(-0.4D1 * t289 * t647)
      t651 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t644, -t642, -
     #t43, 0.0D0, t62)
      t656 = t64 * t651
      t661 = (0.90D2 * t65 * (t645 - t650 * t651) - 0.180D3 * t133 * t65
     #6) * t140 * t144
      t664 = log(-0.4D1 * t234 * t647)
      t666 = t664 ** 2
      t669 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t644, -t642, -
     #t43, 0.0D0, t62)
      t670 = t664 * t645 - t666 * t651 / 0.2D1 - t669
      t674 = -t645 + t664 * t651
      t678 = t179 * t656
      t683 = -t661 / 0.720D3 + (0.90D2 * t65 * t670 - 0.180D3 * t133 * t
     #64 * t674 - t678) * t142 * t143 / 0.720D3
      t684 = FJET(XB1, XB2, s, 0.0D0, -t642, -t43, -t644, t62, t683)
      t686 = FJET(XB1, XB2, s, -t47, -t41, -t56, t52, t62, -t146)
      t690 = FJET(XB1, XB2, s, -t261, t262, t257, -t259, 0.0D0, t303)
      t692 = FJET(XB1, XB2, s, t3, -t43, 0.0D0, 0.0D0, 0.0D0, t628)
      t694 = -t147 * t139 * t150 / 0.720D3 + t255 * t254 + t304 * t303 +
     # t370 * t369 + t613 * t612 + t629 * t628 + t631 * t612 + t633 * t3
     #03 + t635 * t612 - t637 * t139 * t150 / 0.720D3 + t684 * t683 - t6
     #86 * t139 * t150 / 0.720D3 + t690 * t303 + t692 * t628
      t695 = FJET(XB1, XB2, s, -t56, t52, -t47, -t41, t62, -t146)
      t710 = -t661 / 0.720D3 + (0.90D2 * t65 * t670 - 0.180D3 * t133 * t
     #64 * t674 - t678) * t142 * t143 / 0.720D3
      t711 = FJET(XB1, XB2, s, -t642, 0.0D0, -t644, -t43, t62, t710)
      t713 = FJET(XB1, XB2, s, 0.0D0, t306, 0.0D0, t308, 0.0D0, t369)
      t715 = FJET(XB1, XB2, s, 0.0D0, t308, 0.0D0, t306, 0.0D0, t369)
      t717 = FJET(XB1, XB2, s, t262, -t261, -t259, t257, 0.0D0, t303)
      t719 = FJET(XB1, XB2, s, -t43, -t644, 0.0D0, -t642, t62, t710)
      t721 = FJET(XB1, XB2, s, t308, 0.0D0, t306, 0.0D0, 0.0D0, t369)
      t723 = t2 * t10
      t724 = t2 * x3
      t725 = t372 * t10
      t728 = log(-0.4D1 * t98 * t725)
      t729 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t723, t724, 0.0D0)
      t731 = t728 ** 2
      t732 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t723, t724, 0.0D0)
      t735 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t723, t724, 0.0D0)
      t744 = t64 * t732
      t745 = t179 * t744
      t749 = (0.90D2 * t65 * (t728 * t729 - t731 * t732 / 0.2D1 - t735) 
     #- 0.180D3 * t133 * t64 * (-t729 + t728 * t732) - t745) * t140 * t1
     #42 / 0.720D3
      t752 = log(-0.4D1 * t114 * t725)
      t762 = (0.90D2 * t65 * (-t729 + t752 * t732) + 0.180D3 * t133 * t7
     #44) * t140 * t144 / 0.720D3
      t766 = log(-0.4D1 * t466 * t74 * t10)
      t768 = -t729 + t766 * t732
      t771 = t766 ** 2
      t774 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t723, t724, 0.0D0)
      t779 = -t771 * t729 / 0.2D1 - t774 + t771 * t766 * t732 / 0.6D1 + 
     #t766 * t735
      t782 = t209 * t744
      t786 = t766 * t729 - t771 * t732 / 0.2D1 - t735
      t797 = log(-0.4D1 * t521 * t74 * x3 * t10)
      t799 = t797 ** 2
      t802 = t797 * t729 - t799 * t732 / 0.2D1 - t735
      t806 = -t729 + t797 * t732
      t813 = (0.90D2 * t65 * t802 - 0.180D3 * t133 * t64 * t806 - t745) 
     #* t140 * t143 / 0.1440D4
      t814 = -t749 - t762 - (t179 * t64 * t768 + 0.90D2 * t65 * t779 - t
     #782 - 0.180D3 * t133 * t64 * t786) * t140 / 0.1440D4 - t813
      t815 = FJET(XB1, XB2, s, 0.0D0, -t723, 0.0D0, t724, 0.0D0, t814)
      t817 = FJET(XB1, XB2, s, 0.0D0, t724, 0.0D0, -t723, 0.0D0, t814)
      t831 = (t179 * t64 * t768 + 0.90D2 * t65 * t779 - t782 - 0.180D3 *
     # t133 * t64 * t786) * t140 / 0.1440D4
      t843 = -t749 - t762 - t831 - (0.90D2 * t65 * t802 - 0.180D3 * t133
     # * t64 * t806 - t745) * t140 * t143 / 0.1440D4
      t844 = FJET(XB1, XB2, s, -t723, 0.0D0, t724, 0.0D0, 0.0D0, t843)
      t846 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t43, 0.0D0, t254)
      t848 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t612)
      t850 = -t749 - t762 - t831 - t813
      t851 = FJET(XB1, XB2, s, t724, 0.0D0, -t723, 0.0D0, 0.0D0, t850)
      t853 = FJET(XB1, XB2, s, -t644, -t43, -t642, 0.0D0, t62, t710)
      t855 = -t695 * t139 * t150 / 0.720D3 + t711 * t710 + t713 * t369 +
     # t715 * t369 + t717 * t303 + t719 * t710 + t721 * t369 + t815 * t8
     #14 + t817 * t814 + t844 * t843 + t846 * t254 + t848 * t612 + t851 
     #* t850 + t853 * t710
      rrgg2gght8s4e1 = t694 + t855

      end function



      doubleprecision function rrgg2gght8s4e0
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t3 = t2 * x1
      t4 = -0.1D1 + x2
      t5 = x2 * x3
      t6 = x3 * z
      t7 = x3 * x1
      t8 = x1 * z
      t9 = t7 * z
      t10 = x4 * pi
      t11 = cos(t10)
      t12 = -z - x1 + t8
      t14 = -0.1D1 + x3
      t15 = t14 * x2
      t17 = Sqrt(x3 * t12 * t15)
      t18 = t11 * t17
      t19 = 0.2D1 * t18
      t22 = 0.1D1 / t12
      t23 = t5 - 0.1D1
      t24 = 0.1D1 / t23
      t27 = t3 * t4 * (-t5 - z + t6 - x1 + t7 + t8 - t9 + t19) * t22 * t
     #24
      t28 = t14 * s
      t29 = -0.1D1 + x1
      t30 = t1 * t29
      t32 = t28 * t30 * t24
      t35 = x2 * z
      t38 = x2 ** 2
      t40 = t8 * t38 * x3
      t44 = x1 * t38 * x3
      t45 = t5 * z
      t47 = t7 * x2
      t49 = t5 + 0.2D1 * t18 * x2 - 0.2D1 * t7 * t35 + t40 - t6 - t7 - x
     #2 - z * t38 * x3 - t44 + 0.2D1 * t45 + 0.2D1 * t47 + t9 - t19
      t52 = t3 * t49 * t22 * t24
      t53 = t2 * t29
      t55 = x3 * t4 * t24
      t56 = t53 * t55
      t57 = t1 ** 2
      t62 = s * t57 * x2 * x1 * t29 * t22
      t63 = s ** 2
      t64 = 0.1D1 / t63
      t65 = pi * t64
      t66 = x1 * x2
      t67 = t66 * z
      t68 = z - t66 + x1 + t67 - t8
      t71 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t27, -t52, -t32,
     # -t56, t62)
      t72 = z ** 2
      t75 = x1 ** 2
      t76 = x3 * t75
      t79 = t75 * t72
      t83 = x2 * t75
      t91 = t7 * x2 * t72 + 0.2D1 * t76 * t35 - t40 - t79 * t5 + t67 - 0
     #.2D1 * t8 - t66 * t72 - 0.2D1 * t83 * z + t83 * t72 + 0.2D1 * x1 *
     # t72 - t75 + 0.2D1 * t75 * z
      t94 = z * t11 * t17
      t105 = -t79 + t83 - t76 * x2 + t44 - t45 - t47 + 0.2D1 * t94 + 0.2
     #D1 * x1 * t11 * t17 - t72 - 0.2D1 * t66 * t18 - 0.2D1 * t8 * t18 +
     # 0.2D1 * t66 * t94
      t109 = 0.1D1 / x3
      t110 = 0.1D1 / x1
      t112 = 0.1D1 / x2
      t113 = t109 * t110 * t112
      t114 = t71 / (t91 + t105) * t113
      t116 = t65 * t12 * t68 * t114 / 0.8D1
      t117 = FJET(XB1, XB2, s, t27, -t32, -t52, -t56, t62, t116)
      t120 = t64 * t12 * t68
      t125 = t1 * x1
      t126 = t4 * s * t125
      t128 = t2 * t66 * t22
      t129 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t126, -t128, -
     #t53, 0.0D0, t62)
      t132 = t65 * t129 * t113 / 0.8D1
      t133 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t126, -t128, -
     #t53, 0.0D0, t62)
      t134 = Sin(t10)
      t135 = t134 ** 2
      t136 = t83 * t135
      t137 = 0.1D1 / t72
      t138 = t137 * t22
      t139 = t29 ** 2
      t140 = t4 ** 2
      t145 = log(-0.4D1 * t136 * t138 * t139 * t140)
      t147 = t133 - t145 * t129
      t150 = pi * lh
      t153 = 0.180D3 * t150 * t64 * t129
      t158 = -t132 + (-0.90D2 * t65 * t147 + t153) * t110 * t112 / 0.720
     #D3
      t159 = FJET(XB1, XB2, s, -t126, -t53, -t128, 0.0D0, t62, t158)
      t161 = t2 * t55
      t163 = t2 * t14 * t24
      t164 = t23 ** 2
      t168 = 0.1D1 / t72 / z
      t170 = t140 * t135
      t174 = log(-0.4D1 / t164 * t14 * t168 * t170 * t5)
      t176 = Sqrt(-t6 * t15)
      t180 = 0.1D1 / (-z - t5 + 0.2D1 * t11 * t176)
      t182 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #163, t161, 0.0D0)
      t185 = t180 * z
      t186 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #163, t161, 0.0D0)
      t205 = -(0.90D2 * t65 * (t174 * t180 * z * t182 - t185 * t186) + 0
     #.180D3 * t150 * t64 * t185 * t182) * t109 * t112 / 0.1440D4 + t65 
     #* t185 * t182 * t109 * t110 * t112 / 0.8D1
      t206 = FJET(XB1, XB2, s, 0.0D0, t161, 0.0D0, t163, 0.0D0, t205)
      t208 = FJET(XB1, XB2, s, t163, 0.0D0, t161, 0.0D0, 0.0D0, t205)
      t210 = t2 * x3
      t211 = t2 * t14
      t212 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t211, t210, 0.0D0)
      t213 = t135 * t168
      t217 = log(-0.4D1 * t76 * t213 * t14)
      t218 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t211, t210, 0.0D0)
      t223 = t64 * t218
      t225 = 0.180D3 * t150 * t223
      t229 = (0.90D2 * t65 * (-t212 + t217 * t218) + t225) * t109 * t110
     # / 0.720D3
      t232 = t65 * t218 * t113 / 0.8D1
      t233 = x2 * t168
      t238 = log(-0.4D1 * t233 * t135 * x3 * t14)
      t240 = -t212 + t238 * t218
      t246 = (0.90D2 * t65 * t240 + t225) * t109 * t112 / 0.1440D4
      t247 = x3 * t168
      t251 = log(-0.4D1 * t247 * t135 * t14)
      t253 = t251 ** 2
      t256 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t211, t210, 0.0D0)
      t257 = -t251 * t212 + t253 * t218 / 0.2D1 + t256
      t261 = t212 - t251 * t218
      t265 = pi ** 2
      t266 = 0.30D2 * t265
      t267 = lh ** 2
      t268 = 0.180D3 * t267
      t269 = -t266 + t268
      t270 = pi * t269
      t271 = t270 * t223
      t274 = (-0.90D2 * t65 * t257 + 0.180D3 * t150 * t64 * t261 - t271)
     # * t109 / 0.1440D4
      t275 = -t229 + t232 - t246 - t274
      t276 = FJET(XB1, XB2, s, t210, 0.0D0, -t211, 0.0D0, 0.0D0, t275)
      t278 = FJET(XB1, XB2, s, t161, 0.0D0, t163, 0.0D0, 0.0D0, t205)
      t287 = -t132 + (-0.90D2 * t65 * t147 + t153) * t110 * t112 / 0.720
     #D3
      t288 = FJET(XB1, XB2, s, 0.0D0, -t128, -t53, -t126, t62, t287)
      t290 = FJET(XB1, XB2, s, -t56, -t52, -t32, t27, t62, t116)
      t295 = t28 * t125
      t296 = t28 * t30
      t297 = t2 * t7
      t299 = t2 * t29 * x3
      t300 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t295, t297, t2
     #96, -t299, 0.0D0)
      t301 = t76 * t135
      t306 = log(0.4D1 * t301 * t138 * t139 * t14)
      t307 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t295, t297, t2
     #96, -t299, 0.0D0)
      t322 = -(0.90D2 * t65 * (t300 - t306 * t307) - 0.180D3 * t150 * t6
     #4 * t307) * t109 * t110 / 0.720D3 - t65 * t307 * t113 / 0.8D1
      t323 = FJET(XB1, XB2, s, -t295, t296, t297, -t299, 0.0D0, t322)
      t332 = -t229 + t232 - (0.90D2 * t65 * t240 + t225) * t109 * t112 /
     # 0.1440D4 - t274
      t333 = FJET(XB1, XB2, s, -t211, 0.0D0, t210, 0.0D0, 0.0D0, t332)
      t335 = FJET(XB1, XB2, s, -t32, t27, -t56, -t52, t62, t116)
      t340 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t343 = log(0.4D1 * t76 * t213)
      t344 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t349 = t64 * t344
      t358 = log(0.4D1 * t83 * t213)
      t360 = t213 * t140
      t363 = log(0.4D1 * t83 * t360)
      t370 = t75 * t135
      t373 = log(0.4D1 * t370 * t168)
      t375 = t373 ** 2
      t378 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t387 = t270 * t349
      t393 = log(0.4D1 * t247 * t135)
      t395 = t393 ** 2
      t410 = log(0.4D1 * t213)
      t413 = t410 ** 2
      t420 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t446 = log(0.4D1 * t5 * t360)
      t450 = log(0.4D1 * t5 * t213)
      t459 = log(0.4D1 * t233 * t170)
      t461 = t459 ** 2
      t466 = log(0.4D1 * t233 * t135)
      t468 = t466 ** 2
      t483 = -(0.90D2 * t65 * (t340 - t343 * t344) - 0.180D3 * t150 * t3
     #49) * t109 * t110 / 0.720D3 + t65 * (t358 * t344 - t363 * t344) * 
     #t110 * t112 / 0.8D1 - (0.90D2 * t65 * (-t373 * t340 + t375 * t344 
     #/ 0.2D1 + t378) - 0.180D3 * t150 * t64 * (t340 - t373 * t344) + t3
     #87) * t110 / 0.720D3 - (0.90D2 * t65 * (-t393 * t340 + t395 * t344
     # / 0.2D1 + t378) - 0.180D3 * t150 * t64 * (t340 - t393 * t344) + t
     #387) * t109 / 0.1440D4 - (0.180D3 * t410 * lh + 0.45D2 * t413 - t2
     #66 + t268) * pi * t64 * t340 / 0.1440D4 - t65 * t420 / 0.16D2 - (-
     #0.90D2 * t413 * lh - 0.240D3 * zeta3 - 0.120D3 * t267 * lh + 0.60D
     #2 * lh * t265 - 0.15D2 * t413 * t410 - t410 * t269) * pi * t349 / 
     #0.1440D4 - (-0.180D3 * lh - 0.90D2 * t410) * pi * t64 * t378 / 0.1
     #440D4 - t65 * (t446 * t344 - t450 * t344) * t109 * t112 / 0.16D2 +
     # (0.90D2 * t65 * (-t459 * t340 + t461 * t344 / 0.2D1 + t466 * t340
     # - t468 * t344 / 0.2D1) - 0.180D3 * t150 * t64 * (-t459 * t344 + t
     #466 * t344)) * t112 / 0.1440D4
      t484 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t483)
      t486 = FJET(XB1, XB2, s, -t53, -t126, 0.0D0, -t128, t62, t158)
      t488 = FJET(XB1, XB2, s, t297, -t299, -t295, t296, 0.0D0, t322)
      t490 = t117 * pi * t120 * t114 / 0.8D1 + t158 * t159 + t206 * t205
     # + t208 * t205 + t276 * t275 + t278 * t205 + t288 * t287 + t290 * 
     #pi * t120 * t114 / 0.8D1 + t323 * t322 + t333 * t332 + t335 * pi *
     # t120 * t114 / 0.8D1 + t484 * t483 + t486 * t158 + t488 * t322
      t491 = FJET(XB1, XB2, s, -t128, 0.0D0, -t126, -t53, t62, t158)
      t493 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t483)
      t495 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t53
     #, 0.0D0, 0.0D0)
      t497 = t137 * t139 * t22
      t500 = log(-0.4D1 * t301 * t497)
      t501 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t53
     #, 0.0D0, 0.0D0)
      t506 = t64 * t501
      t508 = 0.180D3 * t150 * t506
      t512 = (0.90D2 * t65 * (-t495 + t500 * t501) + t508) * t109 * t110
     # / 0.720D3
      t515 = t65 * t501 * t113 / 0.8D1
      t518 = log(-0.4D1 * t136 * t497)
      t526 = (0.90D2 * t65 * (t495 - t518 * t501) - t508) * t110 * t112 
     #/ 0.720D3
      t529 = log(-0.4D1 * t370 * t497)
      t531 = t529 ** 2
      t534 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, t3, 0.0D0, -t53
     #, 0.0D0, 0.0D0)
      t535 = -t529 * t495 + t531 * t501 / 0.2D1 + t534
      t539 = t495 - t529 * t501
      t543 = t270 * t506
      t547 = -t512 + t515 + t526 - (-0.90D2 * t65 * t535 + 0.180D3 * t15
     #0 * t64 * t539 - t543) * t110 / 0.720D3
      t548 = FJET(XB1, XB2, s, -t53, t3, 0.0D0, 0.0D0, 0.0D0, t547)
      t550 = FJET(XB1, XB2, s, -t52, -t56, t27, -t32, t62, t116)
      t555 = FJET(XB1, XB2, s, 0.0D0, t163, 0.0D0, t161, 0.0D0, t205)
      t567 = -(-0.90D2 * t65 * t257 + 0.180D3 * t150 * t64 * t261 - t271
     #) * t109 / 0.1440D4 - t229 + t232 - t246
      t568 = FJET(XB1, XB2, s, 0.0D0, -t211, 0.0D0, t210, 0.0D0, t567)
      t570 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t483)
      t582 = -t512 + t515 + t526 - (-0.90D2 * t65 * t535 + 0.180D3 * t15
     #0 * t64 * t539 - t543) * t110 / 0.720D3
      t583 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t53, 0.0D0, t582)
      t585 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t53, t3, 0.0D0, t582)
      t587 = FJET(XB1, XB2, s, t296, -t295, -t299, t297, 0.0D0, t322)
      t589 = FJET(XB1, XB2, s, 0.0D0, t210, 0.0D0, -t211, 0.0D0, t567)
      t591 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t483)
      t593 = FJET(XB1, XB2, s, -t299, t297, t296, -t295, 0.0D0, t322)
      t595 = FJET(XB1, XB2, s, t3, -t53, 0.0D0, 0.0D0, 0.0D0, t547)
      t597 = t491 * t158 + t493 * t483 + t548 * t547 + t550 * pi * t120 
     #* t114 / 0.8D1 + t555 * t205 + t568 * t567 + t570 * t483 + t583 * 
     #t582 + t585 * t582 + t587 * t322 + t589 * t567 + t591 * t483 + t59
     #3 * t322 + t595 * t547
      rrgg2gght8s4e0 = t490 + t597

      end function



      doubleprecision function rrgg2gght8s4em1
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

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
      t6 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t22 = pi * lh
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
      t57 = log(0.4D1 * t47 * t13)
      t60 = 0.1D1 / x2
      t67 = log(0.4D1 * t9 * t13)
      t76 = t67 ** 2
      t78 = pi ** 2
      t80 = lh ** 2
      t86 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t89 = -(0.90D2 * t5 * (t6 - t16 * t17) - t25) * t27 / 0.1440D4 - (
     #0.90D2 * t5 * (t6 - t34 * t17) - t25) * t40 / 0.720D3 - t5 * t17 *
     # t27 * t40 / 0.8D1 + t5 * (-t53 * t17 + t57 * t17) * t60 / 0.16D2 
     #- (-0.180D3 * lh - 0.90D2 * t67) * pi * t4 * t6 / 0.1440D4 - (0.18
     #0D3 * t67 * lh + 0.45D2 * t76 - 0.30D2 * t78 + 0.180D3 * t80) * pi
     # * t23 / 0.1440D4 - t5 * t86 / 0.16D2
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t89)
      t92 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t89)
      t94 = t2 * x1
      t95 = -0.1D1 + x1
      t96 = t2 * t95
      t97 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t94, 0.0D0, -t96
     #, 0.0D0, 0.0D0)
      t101 = t5 * t97 * t40 * t60 / 0.8D1
      t102 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, t94, 0.0D0, -t9
     #6, 0.0D0, 0.0D0)
      t106 = 0.1D1 / (-z - x1 + x1 * z)
      t108 = t95 ** 2
      t112 = log(-0.4D1 * t31 / t7 * t106 * t108)
      t114 = -t102 + t112 * t97
      t119 = 0.180D3 * t22 * t4 * t97
      t126 = t5 * t97 * t27 * t40 / 0.8D1
      t127 = t101 - (0.90D2 * t5 * t114 + t119) * t40 / 0.720D3 + t126
      t128 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t94, -t96, 0.0D0, t127)
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t96, t94, 0.0D0, t127)
      t132 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t89)
      t134 = t2 * x3
      t135 = -0.1D1 + x3
      t136 = t2 * t135
      t137 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t136, t134, 0.0D0)
      t141 = log(-0.4D1 * t10 * t13 * t135)
      t142 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t136, t134, 0.0D0)
      t144 = -t137 + t141 * t142
      t149 = 0.180D3 * t22 * t4 * t142
      t153 = t142 * t27
      t156 = t5 * t153 * t40 / 0.8D1
      t159 = t5 * t153 * t60 / 0.16D2
      t160 = -(0.90D2 * t5 * t144 + t149) * t27 / 0.1440D4 + t156 + t159
      t161 = FJET(XB1, XB2, s, 0.0D0, t134, 0.0D0, -t136, 0.0D0, t160)
      t163 = x2 * x3
      t165 = 0.1D1 / (t163 - 0.1D1)
      t167 = t2 * t135 * t165
      t170 = t2 * x3 * t48 * t165
      t171 = cos(t11)
      t175 = Sqrt(-x3 * z * x2 * t135)
      t179 = 0.1D1 / (-z - t163 + 0.2D1 * t171 * t175)
      t181 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #167, t170, 0.0D0)
      t184 = z * t181 * t27 * t60
      t186 = t5 * t179 * t184 / 0.16D2
      t187 = FJET(XB1, XB2, s, 0.0D0, t167, 0.0D0, t170, 0.0D0, t186)
      t189 = t4 * t179
      t193 = FJET(XB1, XB2, s, 0.0D0, t170, 0.0D0, t167, 0.0D0, t186)
      t198 = FJET(XB1, XB2, s, 0.0D0, -t136, 0.0D0, t134, 0.0D0, t160)
      t202 = t2 * x1 * x2 * t106
      t204 = t1 * x1
      t205 = t48 * s * t204
      t206 = t1 ** 2
      t211 = s * t206 * x2 * x1 * t95 * t106
      t212 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t205, -t202, -
     #t96, 0.0D0, t211)
      t214 = t212 * t40 * t60
      t216 = t5 * t214 / 0.8D1
      t217 = FJET(XB1, XB2, s, 0.0D0, -t202, -t96, -t205, t211, -t216)
      t222 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t89)
      t230 = t101 - (0.90D2 * t5 * t114 + t119) * t40 / 0.720D3 + t126
      t231 = FJET(XB1, XB2, s, t94, -t96, 0.0D0, 0.0D0, 0.0D0, t230)
      t233 = t90 * t89 + t92 * t89 + t128 * t127 + t130 * t127 + t132 * 
     #t89 + t161 * t160 + t187 * pi * t189 * t184 / 0.16D2 + t193 * pi *
     # t189 * t184 / 0.16D2 + t198 * t160 - t217 * pi * t4 * t214 / 0.8D
     #1 + t222 * t89 + t231 * t230
      t240 = -(0.90D2 * t5 * t144 + t149) * t27 / 0.1440D4 + t156 + t159
      t241 = FJET(XB1, XB2, s, t134, 0.0D0, -t136, 0.0D0, 0.0D0, t240)
      t244 = t2 * x1 * x3
      t246 = t2 * t95 * x3
      t247 = t135 * s
      t248 = t247 * t204
      t250 = t247 * t1 * t95
      t251 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t248, t244, t2
     #50, -t246, 0.0D0)
      t253 = t251 * t27 * t40
      t255 = t5 * t253 / 0.8D1
      t256 = FJET(XB1, XB2, s, t244, -t246, -t248, t250, 0.0D0, -t255)
      t261 = FJET(XB1, XB2, s, t167, 0.0D0, t170, 0.0D0, 0.0D0, t186)
      t266 = FJET(XB1, XB2, s, t250, -t248, -t246, t244, 0.0D0, -t255)
      t271 = FJET(XB1, XB2, s, t170, 0.0D0, t167, 0.0D0, 0.0D0, t186)
      t276 = FJET(XB1, XB2, s, -t96, t94, 0.0D0, 0.0D0, 0.0D0, t230)
      t278 = FJET(XB1, XB2, s, -t96, -t205, 0.0D0, -t202, t211, -t216)
      t283 = FJET(XB1, XB2, s, -t136, 0.0D0, t134, 0.0D0, 0.0D0, t240)
      t285 = FJET(XB1, XB2, s, -t246, t244, t250, -t248, 0.0D0, -t255)
      t290 = FJET(XB1, XB2, s, -t205, -t96, -t202, 0.0D0, t211, -t216)
      t295 = FJET(XB1, XB2, s, -t248, t250, t244, -t246, 0.0D0, -t255)
      t300 = FJET(XB1, XB2, s, -t202, 0.0D0, -t205, -t96, t211, -t216)
      t305 = t241 * t240 - t256 * pi * t4 * t253 / 0.8D1 + t261 * pi * t
     #189 * t184 / 0.16D2 - t266 * pi * t4 * t253 / 0.8D1 + t271 * pi * 
     #t189 * t184 / 0.16D2 + t276 * t230 - t278 * pi * t4 * t214 / 0.8D1
     # + t283 * t240 - t285 * pi * t4 * t253 / 0.8D1 - t290 * pi * t4 * 
     #t214 / 0.8D1 - t295 * pi * t4 * t253 / 0.8D1 - t300 * pi * t4 * t2
     #14 / 0.8D1
      rrgg2gght8s4em1 = t233 + t305

      end function



      doubleprecision function rrgg2gght8s4em2
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = 0.1D1 / x1
      t11 = pi * t6
      t12 = 0.1D1 / x3
      t16 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = z ** 2
      t24 = Sin(x4 * pi)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t35 = -t3 * pi * t6 * t7 / 0.8D1 - t11 * t3 * t12 / 0.16D2 - t11 *
     # t16 / 0.16D2 - (-0.180D3 * lh - 0.90D2 * t28) * pi * t6 * t3 / 0.
     #1440D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t40 = t2 * x1
      t42 = t2 * (-0.1D1 + x1)
      t43 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, t40, 0.0D0, -t42
     #, 0.0D0, 0.0D0)
      t46 = t11 * t43 * t7 / 0.8D1
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t40, -t42, 0.0D0, t46)
      t50 = t6 * t43 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t40, 0.0D0, t46)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t59 = t2 * x3
      t61 = t2 * (-0.1D1 + x3)
      t62 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #61, t59, 0.0D0)
      t65 = t11 * t62 * t12 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t61, 0.0D0, t65)
      t69 = t6 * t62 * t12
      t72 = FJET(XB1, XB2, s, 0.0D0, -t61, 0.0D0, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t78 = FJET(XB1, XB2, s, t40, -t42, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t61, 0.0D0, 0.0D0, t65)
      t86 = FJET(XB1, XB2, s, -t42, t40, 0.0D0, 0.0D0, 0.0D0, t46)
      t90 = FJET(XB1, XB2, s, -t61, 0.0D0, t59, 0.0D0, 0.0D0, t65)
      rrgg2gght8s4em2 = t36 * t35 + t38 * t35 + t47 * pi * t50 / 0.8D1 +
     # t53 * pi * t50 / 0.8D1 + t57 * t35 + t66 * pi * t69 / 0.16D2 + t7
     #2 * pi * t69 / 0.16D2 + t76 * t35 + t78 * pi * t50 / 0.8D1 + t82 *
     # pi * t69 / 0.16D2 + t86 * pi * t50 / 0.8D1 + t90 * pi * t69 / 0.1
     #6D2

      end function



      doubleprecision function rrgg2gght8s4em3
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * pi * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = pi * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s4em3 = -t9 * t3 * t11 / 0.16D2 - t13 * t3 * t11 / 0.16D
     #2 - t16 * t3 * t11 / 0.16D2 - t19 * t3 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s4em4
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
      doubleprecision rrgg2ggh81J1
      doubleprecision rrgg2ggh81J2
      doubleprecision rrgg2ggh81J3
      doubleprecision rrgg2ggh81J4
      doubleprecision rrgg2ggh81J5
      doubleprecision rrgg2ggh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh81J1
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
      t17 = S13 * S23
      t21 = S23 * S24
      t23 = S24 ** 2
      t25 = S14 * S24
      t29 = S14 * S23
      t31 = S14 ** 2
      t33 = S23 ** 2
      t35 = t15 * S23
      t36 = 0.9D1 * t35
      t37 = t15 * S13
      t38 = 0.9D1 * t37
      t39 = S13 * t33
      t40 = 0.18D2 * t39
      t41 = S13 * t31
      t43 = t15 * S14
      t45 = t13 * S23
      t49 = t29 * S24
      t51 = t33 * S24
      t53 = S14 * t23
      t54 = 0.9D1 * t53
      t55 = t23 * S23
      t57 = t23 * S24
      t58 = 0.9D1 * t57
      t59 = t31 * S24
      t60 = 0.18D2 * t59
      t64 = 0.1D1 / S12
      t68 = 0.18D2 * t41
      t70 = 0.18D2 * t51
      t72 = 0.36D2 * t45 + t40 + t54 + t60 + t36 + 0.9D1 * t43 + t68 + t
     #38 + t58 + 0.9D1 * t55 + t70 + 0.36D2 * t49
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t98 = 0.18D2 * t37 + 0.72D2 * t43 + 0.18D2 * t57 + 0.9D1 * t39 + t
     #68 + 0.63D2 * t53 + 0.9D1 * t59 + 0.90D2 * t49 + t70 + 0.72D2 * t5
     #5 + 0.90D2 * t45 + 0.63D2 * t35
      t108 = t33 ** 2
      t110 = t31 ** 2
      rrgg2ggh81J1 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0.2D1
     # * S24 * t6) * S12 + 0.9D1 * S14 + 0.9D1 * S23 + (-0.9D1 * t13 - 0
     #.9D1 / 0.2D1 * t15 + 0.27D2 / 0.2D1 * t17) * t2 + (-0.9D1 * t21 - 
     #0.9D1 / 0.2D1 * t23 + 0.27D2 / 0.2D1 * t25) * t6 + (0.36D2 * t29 +
     # 0.18D2 * t31 + 0.18D2 * t33 + (t36 + t38 + t40 + 0.9D1 / 0.2D1 * 
     #t41 + 0.9D1 / 0.2D1 * t43 - 0.27D2 / 0.2D1 * t45) * t2 + (-0.27D2 
     #/ 0.2D1 * t49 + 0.9D1 / 0.2D1 * t51 + t54 + 0.9D1 / 0.2D1 * t55 + 
     #t58 + t60) * t6) * t64 + t72 * t74) * s * z + (0.18D2 * S24 + 0.18
     #D2 * S13) * S12 + 0.63D2 * t17 + 0.54D2 * t23 + 0.72D2 * t21 + 0.7
     #2D2 * t13 + 0.63D2 * t25 + 0.54D2 * t15 + t98 * t64 + (0.18D2 * t3
     #3 * S23 * S14 + 0.27D2 * t33 * t31 + 0.18D2 * S23 * t31 * S14 + 0.
     #9D1 * t108 + 0.9D1 * t110) * t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh81J2
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
      t13 = S13 ** 2
      t15 = S13 * S14
      t19 = S24 ** 2
      t21 = S23 * S24
      t25 = S14 ** 2
      t27 = S23 ** 2
      t29 = S14 * S23
      t31 = S13 * t25
      t33 = t13 * S14
      t35 = t13 * S13
      t36 = 0.9D1 * t35
      t37 = t13 * S23
      t38 = 0.9D1 * t37
      t40 = 0.18D2 * S13 * t27
      t44 = 0.18D2 * t25 * S24
      t45 = t27 * S24
      t47 = t19 * S23
      t49 = S14 * t19
      t50 = 0.9D1 * t49
      t51 = t19 * S24
      t52 = 0.9D1 * t51
      t56 = 0.1D1 / S12
      t58 = t15 * S23
      t60 = 0.18D2 * t45
      t62 = 0.18D2 * t31
      t64 = t29 * S24
      t66 = 0.9D1 * t58 + t50 + t44 + t38 + t40 + t60 + 0.9D1 * t33 + t6
     #2 + 0.9D1 * t47 + t52 + 0.9D1 * t64 + t36
      t67 = S12 ** 2
      t68 = 0.1D1 / t67
      t102 = t27 ** 2
      t104 = t25 ** 2
      rrgg2ggh81J2 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0.2D1
     # * S24 * t6) * S12 + 0.9D1 * S14 + 0.9D1 * S23 + (-0.9D1 / 0.2D1 *
     # t13 - 0.9D1 * t15) * t2 + (-0.9D1 / 0.2D1 * t19 - 0.9D1 * t21) * 
     #t6 + (0.18D2 * t25 + 0.18D2 * t27 + 0.9D1 * t29 + (0.9D1 / 0.2D1 *
     # t31 + 0.9D1 / 0.2D1 * t33 + t36 + t38 + t40) * t2 + (t44 + 0.9D1 
     #/ 0.2D1 * t45 + 0.9D1 / 0.2D1 * t47 + t50 + t52) * t6) * t56 + t66
     # * t68) * s * z + (0.18D2 * S24 + 0.18D2 * S13) * S12 + 0.54D2 * S
     #14 * S24 + 0.54D2 * S13 * S23 + 0.72D2 * t15 + 0.72D2 * t21 + 0.54
     #D2 * t13 + 0.54D2 * t19 + (0.72D2 * t47 + 0.54D2 * t49 + 0.72D2 * 
     #t33 + 0.54D2 * t37 + 0.81D2 * t58 + 0.18D2 * t51 + t60 + 0.18D2 * 
     #t35 + 0.81D2 * t64 + t62) * t56 + (0.18D2 * t27 * S23 * S14 + 0.27
     #D2 * t27 * t25 + 0.18D2 * S23 * t25 * S14 + 0.9D1 * t102 + 0.9D1 *
     # t104) * t68) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh81J3
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
      t17 = S13 * S23
      t21 = S23 * S24
      t23 = S24 ** 2
      t25 = S14 * S24
      t29 = S14 * S23
      t31 = S14 ** 2
      t33 = S23 ** 2
      t35 = t15 * S23
      t36 = 0.9D1 * t35
      t37 = t15 * S13
      t38 = 0.9D1 * t37
      t39 = S13 * t33
      t40 = 0.18D2 * t39
      t41 = S13 * t31
      t43 = t15 * S14
      t45 = t13 * S23
      t49 = t29 * S24
      t51 = t33 * S24
      t53 = S14 * t23
      t54 = 0.9D1 * t53
      t55 = t23 * S23
      t57 = t23 * S24
      t58 = 0.9D1 * t57
      t59 = t31 * S24
      t60 = 0.18D2 * t59
      t64 = 0.1D1 / S12
      t68 = 0.18D2 * t41
      t70 = 0.18D2 * t51
      t72 = -0.18D2 * t45 + t40 + t54 + t60 + t36 + 0.9D1 * t43 + t68 + 
     #t38 + t58 + 0.9D1 * t55 + t70 - 0.18D2 * t49
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t98 = 0.18D2 * t37 + 0.72D2 * t43 + 0.18D2 * t57 - 0.9D1 * t39 + t
     #68 + 0.45D2 * t53 - 0.9D1 * t59 + 0.72D2 * t49 + t70 + 0.72D2 * t5
     #5 + 0.72D2 * t45 + 0.45D2 * t35
      t108 = t33 ** 2
      t110 = t31 ** 2
      rrgg2ggh81J3 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0.2D1
     # * S24 * t6) * S12 + 0.9D1 * S14 + 0.9D1 * S23 + (-0.9D1 * t13 - 0
     #.9D1 / 0.2D1 * t15 - 0.27D2 / 0.2D1 * t17) * t2 + (-0.9D1 * t21 - 
     #0.9D1 / 0.2D1 * t23 - 0.27D2 / 0.2D1 * t25) * t6 + (-0.18D2 * t29 
     #+ 0.18D2 * t31 + 0.18D2 * t33 + (t36 + t38 + t40 + 0.9D1 / 0.2D1 *
     # t41 + 0.9D1 / 0.2D1 * t43 + 0.27D2 / 0.2D1 * t45) * t2 + (0.27D2 
     #/ 0.2D1 * t49 + 0.9D1 / 0.2D1 * t51 + t54 + 0.9D1 / 0.2D1 * t55 + 
     #t58 + t60) * t6) * t64 + t72 * t74) * s * z + (0.18D2 * S24 + 0.18
     #D2 * S13) * S12 + 0.45D2 * t17 + 0.54D2 * t23 + 0.72D2 * t21 + 0.7
     #2D2 * t13 + 0.45D2 * t25 + 0.54D2 * t15 + t98 * t64 + (0.18D2 * t3
     #3 * S23 * S14 + 0.27D2 * t33 * t31 + 0.18D2 * S23 * t31 * S14 + 0.
     #9D1 * t108 + 0.9D1 * t110) * t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh81J4
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
      t17 = S13 * S23
      t21 = S23 * S24
      t23 = S24 ** 2
      t25 = S14 * S24
      t29 = S14 * S23
      t31 = S14 ** 2
      t33 = S23 ** 2
      t35 = t15 * S23
      t36 = 0.9D1 * t35
      t37 = t15 * S13
      t38 = 0.9D1 * t37
      t40 = 0.18D2 * S13 * t33
      t41 = S13 * t31
      t43 = t15 * S14
      t45 = t13 * S23
      t49 = t29 * S24
      t51 = t33 * S24
      t53 = S14 * t23
      t54 = 0.9D1 * t53
      t55 = t23 * S23
      t57 = t23 * S24
      t58 = 0.9D1 * t57
      t60 = 0.18D2 * t31 * S24
      t64 = 0.1D1 / S12
      t68 = 0.18D2 * t41
      t70 = 0.18D2 * t51
      t72 = -0.45D2 * t45 + t40 + t54 + t60 + t36 + 0.9D1 * t43 + t68 + 
     #t38 + t58 + 0.9D1 * t55 + t70 - 0.45D2 * t49
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t96 = 0.18D2 * t37 + 0.72D2 * t43 + 0.18D2 * t57 - t40 + t68 + 0.3
     #6D2 * t53 - t60 + 0.63D2 * t49 + t70 + 0.72D2 * t55 + 0.63D2 * t45
     # + 0.36D2 * t35
      t106 = t33 ** 2
      t108 = t31 ** 2
      rrgg2ggh81J4 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0.2D1
     # * S24 * t6) * S12 + 0.9D1 * S14 + 0.9D1 * S23 + (-0.9D1 * t13 - 0
     #.9D1 / 0.2D1 * t15 - 0.27D2 * t17) * t2 + (-0.9D1 * t21 - 0.9D1 / 
     #0.2D1 * t23 - 0.27D2 * t25) * t6 + (-0.45D2 * t29 + 0.18D2 * t31 +
     # 0.18D2 * t33 + (t36 + t38 + t40 + 0.9D1 / 0.2D1 * t41 + 0.9D1 / 0
     #.2D1 * t43 + 0.27D2 * t45) * t2 + (0.27D2 * t49 + 0.9D1 / 0.2D1 * 
     #t51 + t54 + 0.9D1 / 0.2D1 * t55 + t58 + t60) * t6) * t64 + t72 * t
     #74) * s * z + (0.18D2 * S24 + 0.18D2 * S13) * S12 + 0.36D2 * t17 +
     # 0.54D2 * t23 + 0.72D2 * t21 + 0.72D2 * t13 + 0.36D2 * t25 + 0.54D
     #2 * t15 + t96 * t64 + (0.18D2 * t33 * S23 * S14 + 0.27D2 * t33 * t
     #31 + 0.18D2 * S23 * t31 * S14 + 0.9D1 * t106 + 0.9D1 * t108) * t74
     #) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh81J5
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
      t17 = S13 * S23
      t21 = S23 * S24
      t23 = S24 ** 2
      t25 = S14 * S24
      t29 = S14 * S23
      t31 = S14 ** 2
      t33 = S23 ** 2
      t35 = t15 * S23
      t36 = 0.9D1 * t35
      t37 = t15 * S13
      t38 = 0.9D1 * t37
      t39 = S13 * t33
      t40 = 0.18D2 * t39
      t41 = S13 * t31
      t43 = t15 * S14
      t45 = t13 * S23
      t49 = t29 * S24
      t51 = t33 * S24
      t53 = S14 * t23
      t54 = 0.9D1 * t53
      t55 = t23 * S23
      t57 = t23 * S24
      t58 = 0.9D1 * t57
      t59 = t31 * S24
      t60 = 0.18D2 * t59
      t64 = 0.1D1 / S12
      t68 = 0.18D2 * t41
      t70 = 0.18D2 * t51
      t72 = -0.72D2 * t45 + t40 + t54 + t60 + t36 + 0.9D1 * t43 + t68 + 
     #t38 + t58 + 0.9D1 * t55 + t70 - 0.72D2 * t49
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t98 = 0.18D2 * t37 + 0.72D2 * t43 + 0.18D2 * t57 - 0.27D2 * t39 + 
     #t68 + 0.27D2 * t53 - 0.27D2 * t59 + 0.54D2 * t49 + t70 + 0.72D2 * 
     #t55 + 0.54D2 * t45 + 0.27D2 * t35
      t108 = t33 ** 2
      t110 = t31 ** 2
      rrgg2ggh81J5 = (((0.9D1 + 0.9D1 / 0.2D1 * S13 * t2 + 0.9D1 / 0.2D1
     # * S24 * t6) * S12 + 0.9D1 * S14 + 0.9D1 * S23 + (-0.9D1 * t13 - 0
     #.9D1 / 0.2D1 * t15 - 0.81D2 / 0.2D1 * t17) * t2 + (-0.9D1 * t21 - 
     #0.9D1 / 0.2D1 * t23 - 0.81D2 / 0.2D1 * t25) * t6 + (-0.72D2 * t29 
     #+ 0.18D2 * t31 + 0.18D2 * t33 + (t36 + t38 + t40 + 0.9D1 / 0.2D1 *
     # t41 + 0.9D1 / 0.2D1 * t43 + 0.81D2 / 0.2D1 * t45) * t2 + (0.81D2 
     #/ 0.2D1 * t49 + 0.9D1 / 0.2D1 * t51 + t54 + 0.9D1 / 0.2D1 * t55 + 
     #t58 + t60) * t6) * t64 + t72 * t74) * s * z + (0.18D2 * S24 + 0.18
     #D2 * S13) * S12 + 0.27D2 * t17 + 0.54D2 * t23 + 0.72D2 * t21 + 0.7
     #2D2 * t13 + 0.27D2 * t25 + 0.54D2 * t15 + t98 * t64 + (0.18D2 * t3
     #3 * S23 * S14 + 0.27D2 * t33 * t31 + 0.18D2 * S23 * t31 * S14 + 0.
     #9D1 * t108 + 0.9D1 * t110) * t74) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh81J6
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
      t17 = S13 * S23
      t21 = S23 * S24
      t23 = S24 ** 2
      t25 = S14 * S24
      t29 = S14 * S23
      t31 = S14 ** 2
      t33 = S23 ** 2
      t35 = t15 * S23
      t36 = 0.45D2 * t35
      t37 = t15 * S13
      t38 = 0.45D2 * t37
      t40 = 0.90D2 * S13 * t33
      t41 = S13 * t31
      t43 = t15 * S14
      t45 = t13 * S23
      t49 = t29 * S24
      t51 = t33 * S24
      t53 = S14 * t23
      t54 = 0.45D2 * t53
      t55 = t23 * S23
      t57 = t23 * S24
      t58 = 0.45D2 * t57
      t60 = 0.90D2 * t31 * S24
      t64 = 0.1D1 / S12
      t68 = 0.90D2 * t41
      t70 = 0.90D2 * t51
      t72 = -0.315D3 * t45 - t40 - t54 - t60 - t36 - 0.45D2 * t43 - t68 
     #- t38 - t58 - 0.45D2 * t55 - t70 - 0.315D3 * t49
      t73 = S12 ** 2
      t74 = 0.1D1 / t73
      t96 = -0.90D2 * t37 - 0.360D3 * t43 - 0.90D2 * t57 - t40 - t68 - 0
     #.360D3 * t53 - t60 - 0.495D3 * t49 - t70 - 0.360D3 * t55 - 0.495D3
     # * t45 - 0.360D3 * t35
      t106 = t33 ** 2
      t108 = t31 ** 2
      rrgg2ggh81J6 = (((-0.45D2 - 0.45D2 / 0.2D1 * S13 * t2 - 0.45D2 / 0
     #.2D1 * S24 * t6) * S12 - 0.45D2 * S14 - 0.45D2 * S23 + (0.45D2 * t
     #13 + 0.45D2 / 0.2D1 * t15 - 0.135D3 * t17) * t2 + (0.45D2 * t21 + 
     #0.45D2 / 0.2D1 * t23 - 0.135D3 * t25) * t6 + (-0.315D3 * t29 - 0.9
     #0D2 * t31 - 0.90D2 * t33 + (-t36 - t38 - t40 - 0.45D2 / 0.2D1 * t4
     #1 - 0.45D2 / 0.2D1 * t43 + 0.135D3 * t45) * t2 + (0.135D3 * t49 - 
     #0.45D2 / 0.2D1 * t51 - t54 - 0.45D2 / 0.2D1 * t55 - t58 - t60) * t
     #6) * t64 + t72 * t74) * s * z + (-0.90D2 * S24 - 0.90D2 * S13) * S
     #12 - 0.360D3 * t17 - 0.270D3 * t23 - 0.360D3 * t21 - 0.360D3 * t13
     # - 0.360D3 * t25 - 0.270D3 * t15 + t96 * t64 + (-0.90D2 * t33 * S2
     #3 * S14 - 0.135D3 * t33 * t31 - 0.90D2 * S23 * t31 * S14 - 0.45D2 
     #* t106 - 0.45D2 * t108) * t74) / pi * wd / z

      end function
  
 