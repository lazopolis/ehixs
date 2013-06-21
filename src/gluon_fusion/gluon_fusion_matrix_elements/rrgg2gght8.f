  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = t3 * 0.3141592653589793D1
      t7 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t8 = t7 * 0.3141592653589793D1
      t10 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t11 = t10 * 0.3141592653589793D1
      t12 = lh ** 2
      t14 = 0.3141592653589793D1 ** 2
      t16 = -0.180D3 * t12 + 0.30D2 * t14
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t22 = x4 * 0.3141592653589793D1
      t23 = Sin(t22)
      t24 = t23 ** 2
      t25 = x3 * t24
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t30 = log(0.4D1 * t25 * t27)
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t25 * t27 * t32)
      t38 = cos(t22)
      t39 = z * t38
      t41 = Sqrt(-x3 * t31)
      t45 = 0.1D1 / (-x3 - z + 0.2D1 * t39 * t41)
      t49 = t36 ** 2
      t53 = t30 ** 2
      t66 = -0.60D2 * lh * t14 + 0.2884936567583026D3 + 0.120D3 * t12 * 
     #lh
      t68 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t88 = 0.1D1 / x3
      t91 = t27 * t24
      t93 = log(0.4D1 * t91)
      t94 = t93 ** 2
      t97 = t94 * t93
      t116 = t14 ** 2
      t117 = t12 ** 2
      t128 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t129 = t94 ** 2
      t138 = 0.3141592653589793D1 * t20
      t139 = x1 ** 2
      t140 = x3 * t139
      t141 = t91 * t32
      t144 = log(-0.4D1 * t140 * t141)
      t145 = t144 * z
      t147 = t144 ** 2
      t151 = z * t7
      t156 = log(0.4D1 * t140 * t91)
      t158 = t156 ** 2
      t164 = 0.3141592653589793D1 * lh
      t165 = z * t3
      t174 = 0.3141592653589793D1 * t16
      t177 = t10 + z * t10 * t45
      t179 = t174 * t20 * t177
      t182 = 0.1D1 / x1
      t185 = t139 * t24
      t186 = t185 * t27
      t188 = log(0.4D1 * t186)
      t193 = t188 ** 2
      t203 = 0.3141592653589793D1 * t66
      t204 = t20 * t10
      t205 = t203 * t204
      t216 = x2 ** 2
      t217 = x3 * t216
      t218 = t217 * t139
      t221 = log(-0.4D1 * t218 * t141)
      t226 = t217 * t186
      t228 = log(0.4D1 * t226)
      t239 = 0.1D1 / x2
      t240 = t182 * t239
      t243 = t216 * t139
      t246 = log(0.4D1 * t243 * t91)
      t248 = t246 ** 2
      t266 = log(-0.4D1 * t217 * t141)
      t267 = t266 * z
      t269 = t266 ** 2
      t277 = log(0.4D1 * t217 * t91)
      t279 = t277 ** 2
      t297 = t216 * t24
      t300 = log(0.4D1 * t297 * t27)
      t305 = t300 ** 2
      t325 = ((0.180D3 * t4 * lh - 0.90D2 * t8 + t11 * t16) * t20 * (-t3
     #0 - t36 * z * t45) - 0.90D2 * t11 * t20 * (-t49 * t36 * z * t45 / 
     #0.6D1 - t53 * t30 / 0.6D1) + (0.180D3 * t8 * lh + t11 * t66 - 0.90
     #D2 * t68 * 0.3141592653589793D1 + t4 * t16) * t20 * (0.1D1 + z * t
     #45) + (0.180D3 * t11 * lh - 0.90D2 * t4) * t20 * (t53 / 0.2D1 + t4
     #9 * z * t45 / 0.2D1)) * t88 / 0.2880D4 + (0.180D3 * (t94 * t3 / 0.
     #2D1 + t68 - t97 * t10 / 0.6D1 - t93 * t7) * 0.3141592653589793D1 *
     # lh + (-t93 * t3 + t94 * t10 / 0.2D1 + t7) * 0.3141592653589793D1 
     #* t16 + (t3 - t93 * t10) * 0.3141592653589793D1 * t66 + t11 * (-0.
     #5769873135166051D3 * lh - t116 - 0.60D2 * t117 + 0.60D2 * t12 * t1
     #4) - 0.90D2 * (-t97 * t3 / 0.6D1 + t94 * t7 / 0.2D1 - t93 * t68 + 
     #t128 + t129 * t10 / 0.24D2) * 0.3141592653589793D1) * t20 / 0.2880
     #D4 + (-0.90D2 * t138 * ((-t145 * t3 + t147 * z * t10 / 0.2D1 + t15
     #1) * t45 - t156 * t3 + t158 * t10 / 0.2D1 + t7) + 0.180D3 * t164 *
     # t20 * ((t165 - t145 * t10) * t45 + t3 - t156 * t10) + t179) * t88
     # * t182 / 0.1440D4 + (t174 * t20 * (t3 - t188 * t10) - 0.90D2 * t1
     #38 * (t193 * t3 / 0.2D1 + t68 - t193 * t188 * t10 / 0.6D1 - t188 *
     # t7) + t205 + 0.180D3 * t164 * t20 * (-t188 * t3 + t193 * t10 / 0.
     #2D1 + t7)) * t182 / 0.1440D4 - (-0.90D2 * t138 * (-(t165 - t221 * 
     #z * t10) * t45 - t3 + t228 * t10) - 0.180D3 * t164 * t20 * t177) *
     # t88 * t240 / 0.720D3 - (-0.90D2 * t138 * (t246 * t3 - t248 * t10 
     #/ 0.2D1 - t7) + 0.180D3 * t164 * t20 * (-t3 + t246 * t10) - t174 *
     # t204) * t182 * t239 / 0.720D3 + (-0.90D2 * t138 * ((-t267 * t3 + 
     #t269 * z * t10 / 0.2D1 + t151) * t45 - t277 * t3 + t279 * t10 / 0.
     #2D1 + t7) + 0.180D3 * t164 * t20 * ((t165 - t267 * t10) * t45 + t3
     # - t277 * t10) + t179) * t88 * t239 / 0.1440D4 - (t174 * t20 * (-t
     #3 + t300 * t10) - 0.90D2 * t138 * (-t305 * t3 / 0.2D1 - t68 + t305
     # * t300 * t10 / 0.6D1 + t300 * t7) - t205 + 0.180D3 * t164 * t20 *
     # (t300 * t3 - t305 * t10 / 0.2D1 - t7)) * t239 / 0.1440D4
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
      t344 = t140 * t24
      t345 = t333 ** 2
      t346 = t27 * t345
      t348 = t346 * t336 * t32
      t351 = log(-0.4D1 * t344 * t348)
      t352 = t351 * t335
      t353 = -t333
      t354 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t353, 0.10D1, 0.10
     #D1, x4)
      t357 = t351 ** 2
      t359 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t353, 0.10D1, 0.10
     #D1, x4)
      t360 = z * t359
      t363 = t335 * z
      t364 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t353, 0.10D1, 0.10
     #D1, x4)
      t367 = x3 * x1
      t368 = t367 * z
      t369 = 0.3D1 * t368
      t370 = 0.2D1 * t367
      t371 = x3 * t335
      t373 = Sqrt(-t371 * t31)
      t376 = x1 * t26
      t377 = t367 * t26
      t379 = 0.2D1 * t140 * z
      t380 = t139 * t26
      t381 = t380 * x3
      t382 = t334 - t369 - z - t140 + t370 + 0.2D1 * t39 * t373 - x3 - t
     #376 + t377 + t379 - t381
      t383 = 0.1D1 / t382
      t385 = t346 * t336
      t388 = log(0.4D1 * t344 * t385)
      t390 = t388 ** 2
      t396 = t363 * t354
      t407 = -t363 * t359 * t383 - t359
      t413 = (-0.90D2 * t138 * (-(-t352 * z * t354 + t357 * t335 * t360 
     #/ 0.2D1 + t363 * t364) * t383 + t388 * t354 - t390 * t359 / 0.2D1 
     #- t364) + 0.180D3 * t164 * t20 * (-(t396 - t352 * t360) * t383 - t
     #354 + t388 * t359) + t174 * t20 * t407) * t88 * t182 / 0.1440D4
      t416 = log(0.4D1 * t185 * t385)
      t418 = -t354 + t416 * t359
      t421 = t416 ** 2
      t424 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, t353, 0.10D1, 0.10
     #D1, x4)
      t429 = -t421 * t354 / 0.2D1 - t424 + t421 * t416 * t359 / 0.6D1 + 
     #t416 * t364
      t432 = t20 * t359
      t433 = t203 * t432
      t437 = t416 * t354 - t421 * t359 / 0.2D1 - t364
      t444 = t217 * t185
      t447 = log(-0.4D1 * t444 * t348)
      t452 = t345 * t336
      t456 = log(0.4D1 * t218 * t91 * t452)
      t468 = (-0.90D2 * t138 * ((t396 - t447 * t335 * t360) * t383 + t35
     #4 - t456 * t359) - 0.180D3 * t164 * t20 * t407) * t88 * t240 / 0.7
     #20D3
      t469 = t243 * t24
      t472 = log(0.4D1 * t469 * t385)
      t474 = t472 ** 2
      t489 = (-0.90D2 * t138 * (-t472 * t354 + t474 * t359 / 0.2D1 + t36
     #4) + 0.180D3 * t164 * t20 * (t354 - t472 * t359) + t174 * t432) * 
     #t182 * t239 / 0.720D3
      t490 = t413 + (t174 * t20 * t418 - 0.90D2 * t138 * t429 - t433 + 0
     #.180D3 * t164 * t20 * t437) * t182 / 0.1440D4 - t468 - t489
      t491 = FJET(XB1, XB2, s, 0.0D0, t332, -t338, 0.0D0, -t343, t490)
      t493 = x2 * s
      t494 = t493 * t1
      t495 = -0.1D1 + x2
      t496 = t495 * s
      t497 = t496 * t1
      t498 = -t495
      t499 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, 0.10
     #D1, x4)
      t500 = t91 * t495
      t503 = log(-0.4D1 * t218 * t500)
      t504 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, 0.10
     #D1, x4)
      t509 = t20 * t504
      t518 = log(-0.4D1 * t243 * t500)
      t520 = t518 ** 2
      t523 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, 0.10
     #D1, x4)
      t532 = t174 * t509
      t539 = log(-0.4D1 * t217 * t500)
      t541 = t539 ** 2
      t556 = t27 * t495
      t559 = log(-0.4D1 * t297 * t556)
      t564 = t559 ** 2
      t567 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, 0.10
     #D1, x4)
      t586 = -(-0.90D2 * t138 * (t499 - t503 * t504) + 0.180D3 * t164 * 
     #t509) * t88 * t240 / 0.720D3 - (-0.90D2 * t138 * (-t518 * t499 + t
     #520 * t504 / 0.2D1 + t523) + 0.180D3 * t164 * t20 * (t499 - t518 *
     # t504) + t532) * t182 * t239 / 0.720D3 + (-0.90D2 * t138 * (t539 *
     # t499 - t541 * t504 / 0.2D1 - t523) + 0.180D3 * t164 * t20 * (-t49
     #9 + t539 * t504) - t532) * t88 * t239 / 0.1440D4 - (t174 * t20 * (
     #t499 - t559 * t504) - 0.90D2 * t138 * (t564 * t499 / 0.2D1 + t567 
     #- t564 * t559 * t504 / 0.6D1 - t559 * t523) + t203 * t509 + 0.180D
     #3 * t164 * t20 * (-t559 * t499 + t564 * t504 / 0.2D1 + t523)) * t2
     #39 / 0.1440D4
      t587 = FJET(XB1, XB2, s, 0.0D0, t494, 0.0D0, -t497, 0.0D0, t586)
      t589 = x2 * x3
      t592 = Sqrt(x3 * t495 * t31)
      t593 = t38 * t592
      t595 = 0.2D1 * t593 * x2
      t597 = 0.1D1 - x3 + t589
      t598 = 0.1D1 / t597
      t600 = t2 * (0.1D1 - x3 - x2 + t589 + t217 + t595) * t598
      t605 = t2 * x2 * (-0.1D1 + t589 + 0.2D1 * t593) * t598
      t606 = t31 * t598
      t607 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, -t60
     #6, x4)
      t608 = t597 ** 2
      t609 = 0.1D1 / t608
      t611 = t556 * t31 * t609
      t614 = log(0.4D1 * t444 * t611)
      t615 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, -t60
     #6, x4)
      t619 = z * t216 * x3
      t620 = x2 * z
      t621 = t589 * z
      t628 = 0.1D1 / (t619 - t620 - t621 - t217 + x3 + z + x2 - 0.2D1 * 
     #t39 * t592 + 0.2D1 * t39 * t592 * x2 - t595)
      t630 = t620 - z - x2
      t634 = t164 * t20
      t636 = t615 * t628 * t630
      t646 = log(0.4D1 * t217 * t24 * t611)
      t648 = t646 ** 2
      t651 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t498, -t60
     #6, x4)
      t669 = -(-0.90D2 * t138 * (t607 - t614 * t615) * t628 * t630 + 0.1
     #80D3 * t634 * t636) * t88 * t240 / 0.720D3 + (0.90D2 * t138 * (-t6
     #46 * t607 + t648 * t615 / 0.2D1 + t651) * t628 * t630 - 0.180D3 * 
     #t634 * (t607 - t646 * t615) * t628 * t630 - t174 * t20 * t636) * t
     #88 * t239 / 0.1440D4
      t670 = FJET(XB1, XB2, s, 0.0D0, t600, 0.0D0, -t605, 0.0D0, t669)
      t672 = t1 * t333
      t674 = t496 * t672 * t336
      t675 = t493 * t672
      t677 = t340 * t495 * t342
      t678 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t353, t498, 0.10D1
     #, x4)
      t680 = t346 * t336 * t495
      t683 = log(-0.4D1 * t444 * t680)
      t684 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t353, t498, 0.10D1
     #, x4)
      t689 = t20 * t684
      t694 = (-0.90D2 * t138 * (-t678 + t683 * t684) - 0.180D3 * t164 * 
     #t689) * t88 * t240
      t697 = log(-0.4D1 * t469 * t680)
      t699 = t697 ** 2
      t702 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t353, t498, 0.10D1
     #, x4)
      t703 = t697 * t678 - t699 * t684 / 0.2D1 - t702
      t707 = -t678 + t697 * t684
      t711 = t174 * t689
      t716 = -t694 / 0.720D3 - (-0.90D2 * t138 * t703 + 0.180D3 * t164 *
     # t20 * t707 - t711) * t182 * t239 / 0.720D3
      t717 = FJET(XB1, XB2, s, 0.0D0, t674, t332, -t675, t677, t716)
      t719 = FJET(XB1, XB2, s, 0.0D0, -t497, 0.0D0, t494, 0.0D0, t586)
      t721 = FJET(XB1, XB2, s, 0.0D0, -t338, t332, 0.0D0, -t343, t490)
      t723 = FJET(XB1, XB2, s, 0.0D0, -t605, 0.0D0, t600, 0.0D0, t669)
      t725 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t325)
      t740 = t413 + (t174 * t20 * t418 - 0.90D2 * t138 * t429 - t433 + 0
     #.180D3 * t164 * t20 * t437) * t182 / 0.1440D4 - t468 - t489
      t741 = FJET(XB1, XB2, s, t332, 0.0D0, 0.0D0, -t338, -t343, t740)
      t743 = t326 * t325 + t328 * t325 + t330 * t325 + t491 * t490 + t58
     #7 * t586 + t670 * t669 + t717 * t716 + t719 * t586 + t721 * t490 +
     # t723 * t669 + t725 * t325 + t741 * t740
      t744 = FJET(XB1, XB2, s, t332, -t675, 0.0D0, t674, t677, t716)
      t746 = FJET(XB1, XB2, s, t494, 0.0D0, -t497, 0.0D0, 0.0D0, t586)
      t748 = FJET(XB1, XB2, s, t600, 0.0D0, -t605, 0.0D0, 0.0D0, t669)
      t750 = FJET(XB1, XB2, s, t674, 0.0D0, -t675, t332, t677, t716)
      t753 = t332 * t589 * t598
      t754 = t2 * t333
      t755 = t334 * t217
      t756 = t495 * t31
      t758 = Sqrt(t371 * t756)
      t759 = t38 * t758
      t761 = 0.2D1 * t759 * x2
      t763 = x1 * t216 * x3
      t767 = t754 * (t755 - x2 + t589 + t217 + t761 + 0.1D1 - x3 - t763)
     # * t336 * t598
      t771 = t31 * s * t1 * x1 * t598
      t777 = t754 * x2 * (-0.1D1 + t589 + x1 - t367 - t334 + t368 + 0.2D
     #1 * t759) * t336 * t598
      t782 = log(0.4D1 * t226 * t452 * t756 * t609)
      t784 = x1 * x2
      t785 = t784 * z
      t786 = x2 - t784 + z - t620 + t785
      t789 = t139 * x2
      t802 = -x2 + t761 - t763 - 0.3D1 * t785 - t367 * x2 + 0.2D1 * t789
     # * z - t380 * x2 + t784 * t26 + 0.2D1 * t39 * t758 + t140 * x2 + 0
     #.2D1 * t784 - t789 + 0.2D1 * t39 * t758 * x1 * x2 - t619 + t621 - 
     #t369 + t377
      t815 = t379 - t381 + t334 - t140 + t370 - t376 + t217 + t620 - z -
     # x3 + t755 + 0.2D1 * t367 * t620 + t380 * t589 - 0.2D1 * t140 * t6
     #20 - t367 * x2 * t26 - 0.2D1 * t39 * t758 * x2 - 0.2D1 * t759 * t7
     #84
      t817 = 0.1D1 / (t802 + t815)
      t819 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t353, t498, -t606,
     # x4)
      t822 = t335 * t786
      t823 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t353, t498, -t606,
     # x4)
      t833 = -0.90D2 * t138 * (t782 * t335 * t786 * t817 * t819 - t822 *
     # t817 * t823) - 0.180D3 * t634 * t822 * t817 * t819
      t836 = t833 * t88 * t240 / 0.720D3
      t837 = FJET(XB1, XB2, s, t753, -t767, -t771, t777, t677, -t836)
      t840 = t88 * t182 * t239
      t843 = FJET(XB1, XB2, s, t777, -t771, -t767, t753, t677, -t836)
      t847 = FJET(XB1, XB2, s, -t497, 0.0D0, t494, 0.0D0, 0.0D0, t586)
      t849 = FJET(XB1, XB2, s, -t338, 0.0D0, 0.0D0, t332, -t343, t740)
      t862 = -t694 / 0.720D3 - (-0.90D2 * t138 * t703 + 0.180D3 * t164 *
     # t20 * t707 - t711) * t182 * t239 / 0.720D3
      t863 = FJET(XB1, XB2, s, -t675, t332, t674, 0.0D0, t677, t862)
      t865 = FJET(XB1, XB2, s, -t605, 0.0D0, t600, 0.0D0, 0.0D0, t669)
      t867 = FJET(XB1, XB2, s, -t771, t777, t753, -t767, t677, -t836)
      t871 = FJET(XB1, XB2, s, -t767, t753, t777, -t771, t677, -t836)
      t875 = t744 * t716 + t746 * t586 + t748 * t669 + t750 * t716 - t83
     #7 * t833 * t840 / 0.720D3 - t843 * t833 * t840 / 0.720D3 + t847 * 
     #t586 + t849 * t740 + t863 * t862 + t865 * t669 - t867 * t833 * t84
     #0 / 0.720D3 - t871 * t833 * t840 / 0.720D3
      rrgg2gght8s1e1 = t743 + t875

      end function



      doubleprecision function rrgg2gght8s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
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
      t26 = z * t25
      t28 = Sqrt(-x3 * t17)
      t32 = 0.1D1 / (-x3 - z + 0.2D1 * t26 * t28)
      t41 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t42 = t41 * 0.3141592653589793D1
      t52 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = -0.180D3 * t55 + 0.30D2 * t57
      t67 = 0.1D1 / x3
      t70 = t12 * t9
      t72 = log(0.4D1 * t70)
      t74 = t72 ** 2
      t89 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t104 = 0.3141592653589793D1 * t6
      t105 = z * t41
      t106 = x2 ** 2
      t107 = t106 * x3
      t108 = t70 * t18
      t111 = log(-0.4D1 * t107 * t108)
      t118 = log(0.4D1 * t107 * t70)
      t123 = 0.3141592653589793D1 * lh
      t126 = t3 + z * t3 * t32
      t129 = 0.180D3 * t123 * t6 * t126
      t132 = 0.1D1 / x2
      t135 = t106 * t9
      t138 = log(0.4D1 * t135 * t12)
      t140 = t138 ** 2
      t151 = 0.3141592653589793D1 * t59
      t152 = t6 * t3
      t153 = t151 * t152
      t157 = x1 ** 2
      t158 = x3 * t157
      t161 = log(-0.4D1 * t158 * t108)
      t168 = log(0.4D1 * t158 * t70)
      t175 = 0.1D1 / x1
      t181 = t67 * t175 * t132
      t184 = t106 * t157
      t187 = log(0.4D1 * t184 * t70)
      t198 = t157 * t9
      t201 = log(0.4D1 * t198 * t12)
      t203 = t201 ** 2
      t217 = (-0.90D2 * t4 * t6 * (t16 / 0.2D1 + t23 * z * t32 / 0.2D1) 
     #+ (0.180D3 * t4 * lh - 0.90D2 * t42) * t6 * (-t15 - t22 * z * t32)
     # + (0.180D3 * t42 * lh - 0.90D2 * t52 * 0.3141592653589793D1 + t4 
     #* t59) * t6 * (0.1D1 + z * t32)) * t67 / 0.2880D4 + (0.180D3 * (-t
     #72 * t41 + t74 * t3 / 0.2D1 + t52) * 0.3141592653589793D1 * lh + t
     #4 * (-0.60D2 * lh * t57 + 0.2884936567583026D3 + 0.120D3 * t55 * l
     #h) - 0.90D2 * (t74 * t41 / 0.2D1 + t89 - t74 * t72 * t3 / 0.6D1 - 
     #t72 * t52) * 0.3141592653589793D1 + (t41 - t72 * t3) * 0.314159265
     #3589793D1 * t59) * t6 / 0.2880D4 + (-0.90D2 * t104 * ((t105 - t111
     # * z * t3) * t32 + t41 - t118 * t3) + t129) * t67 * t132 / 0.1440D
     #4 - (-0.90D2 * t104 * (t138 * t41 - t140 * t3 / 0.2D1 - t52) + 0.1
     #80D3 * t123 * t6 * (-t41 + t138 * t3) - t153) * t132 / 0.1440D4 + 
     #(-0.90D2 * t104 * ((t105 - t161 * z * t3) * t32 + t41 - t168 * t3)
     # + t129) * t67 * t175 / 0.1440D4 - t104 * t126 * t181 / 0.8D1 - (-
     #0.90D2 * t104 * (-t41 + t187 * t3) - 0.180D3 * t123 * t152) * t175
     # * t132 / 0.720D3 + (-0.90D2 * t104 * (-t201 * t41 + t203 * t3 / 0
     #.2D1 + t52) + 0.180D3 * t123 * t6 * (t41 - t201 * t3) + t153) * t1
     #75 / 0.1440D4
      t218 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t217)
      t220 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t217)
      t222 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t217)
      t224 = t2 * x1
      t225 = -0.1D1 + x1
      t226 = x1 * z
      t227 = 0.1D1 - x1 + t226
      t228 = 0.1D1 / t227
      t230 = t2 * t225 * t228
      t231 = t1 ** 2
      t232 = s * t231
      t234 = x1 * t225 * t228
      t235 = t232 * t234
      t236 = t227 * z
      t237 = -t225
      t238 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t237, 0.10D1, 0.10
     #D1, x4)
      t240 = t158 * t9
      t241 = t225 ** 2
      t242 = t12 * t241
      t247 = log(-0.4D1 * t240 * t242 * t228 * t18)
      t249 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t237, 0.10D1, 0.10
     #D1, x4)
      t253 = x3 * x1
      t254 = t253 * z
      t255 = 0.3D1 * t254
      t256 = 0.2D1 * t253
      t257 = x3 * t227
      t259 = Sqrt(-t257 * t17)
      t262 = x1 * t11
      t263 = t253 * t11
      t265 = 0.2D1 * t158 * z
      t266 = t157 * t11
      t267 = t266 * x3
      t268 = t226 - t255 - z - t158 + t256 + 0.2D1 * t26 * t259 - x3 - t
     #262 + t263 + t265 - t267
      t269 = 0.1D1 / t268
      t271 = t242 * t228
      t274 = log(0.4D1 * t240 * t271)
      t281 = -t236 * t249 * t269 - t249
      t288 = (-0.90D2 * t104 * (-(t236 * t238 - t247 * t227 * z * t249) 
     #* t269 - t238 + t274 * t249) + 0.180D3 * t123 * t6 * t281) * t67 *
     # t175 / 0.1440D4
      t292 = -t104 * t281 * t181 / 0.8D1
      t293 = t184 * t9
      t296 = log(0.4D1 * t293 * t271)
      t301 = t6 * t249
      t307 = (-0.90D2 * t104 * (t238 - t296 * t249) + 0.180D3 * t123 * t
     #301) * t175 * t132 / 0.720D3
      t310 = log(0.4D1 * t198 * t271)
      t312 = t310 ** 2
      t315 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t237, 0.10D1, 0.10
     #D1, x4)
      t316 = t310 * t238 - t312 * t249 / 0.2D1 - t315
      t320 = -t238 + t310 * t249
      t324 = t151 * t301
      t328 = t288 + t292 - t307 + (-0.90D2 * t104 * t316 + 0.180D3 * t12
     #3 * t6 * t320 - t324) * t175 / 0.1440D4
      t329 = FJET(XB1, XB2, s, 0.0D0, t224, -t230, 0.0D0, -t235, t328)
      t331 = x2 * s
      t332 = t331 * t1
      t333 = -0.1D1 + x2
      t334 = t333 * s
      t335 = t334 * t1
      t336 = -t333
      t337 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t336, 0.10
     #D1, x4)
      t338 = t70 * t333
      t341 = log(-0.4D1 * t107 * t338)
      t342 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t336, 0.10
     #D1, x4)
      t347 = t6 * t342
      t349 = 0.180D3 * t123 * t347
      t354 = t12 * t333
      t357 = log(-0.4D1 * t135 * t354)
      t359 = t357 ** 2
      t362 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t336, 0.10
     #D1, x4)
      t380 = log(-0.4D1 * t184 * t338)
      t389 = (-0.90D2 * t104 * (-t337 + t341 * t342) - t349) * t67 * t13
     #2 / 0.1440D4 - (-0.90D2 * t104 * (-t357 * t337 + t359 * t342 / 0.2
     #D1 + t362) + 0.180D3 * t123 * t6 * (t337 - t357 * t342) + t151 * t
     #347) * t132 / 0.1440D4 + t104 * t342 * t181 / 0.8D1 - (-0.90D2 * t
     #104 * (t337 - t380 * t342) + t349) * t175 * t132 / 0.720D3
      t390 = FJET(XB1, XB2, s, 0.0D0, t332, 0.0D0, -t335, 0.0D0, t389)
      t392 = x2 * x3
      t395 = Sqrt(x3 * t333 * t17)
      t396 = t25 * t395
      t398 = 0.2D1 * t396 * x2
      t400 = 0.1D1 - x3 + t392
      t401 = 0.1D1 / t400
      t403 = t2 * (0.1D1 - x3 - x2 + t392 + t107 + t398) * t401
      t408 = t2 * x2 * (-0.1D1 + t392 + 0.2D1 * t396) * t401
      t409 = t17 * t401
      t410 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t336, -t40
     #9, x4)
      t412 = t400 ** 2
      t418 = log(0.4D1 * t107 * t9 * t354 * t17 / t412)
      t419 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t336, -t40
     #9, x4)
      t423 = z * t106 * x3
      t424 = x2 * z
      t425 = t392 * z
      t432 = 0.1D1 / (t423 - t424 - t425 - t107 + x3 + z + x2 - 0.2D1 * 
     #t26 * t395 + 0.2D1 * t26 * t395 * x2 - t398)
      t434 = t424 - z - x2
      t439 = t419 * t432
      t453 = (0.90D2 * t104 * (t410 - t418 * t419) * t432 * t434 - 0.180
     #D3 * t123 * t6 * t439 * t434) * t67 * t132 / 0.1440D4 + t104 * t43
     #9 * t434 * t67 * t175 * t132 / 0.8D1
      t454 = FJET(XB1, XB2, s, 0.0D0, t403, 0.0D0, -t408, 0.0D0, t453)
      t456 = t1 * t225
      t458 = t334 * t456 * t228
      t459 = t331 * t456
      t461 = t232 * t333 * t234
      t462 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t237, t336, 0.10D1
     #, x4)
      t465 = t104 * t462 * t181 / 0.8D1
      t466 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t237, t336, 0.10D1
     #, x4)
      t472 = log(-0.4D1 * t293 * t12 * t228 * t241 * t333)
      t474 = -t466 + t472 * t462
      t479 = 0.180D3 * t123 * t6 * t462
      t484 = -t465 - (-0.90D2 * t104 * t474 - t479) * t175 * t132 / 0.72
     #0D3
      t485 = FJET(XB1, XB2, s, 0.0D0, t458, t224, -t459, t461, t484)
      t487 = FJET(XB1, XB2, s, 0.0D0, -t335, 0.0D0, t332, 0.0D0, t389)
      t489 = FJET(XB1, XB2, s, 0.0D0, -t230, t224, 0.0D0, -t235, t328)
      t491 = FJET(XB1, XB2, s, 0.0D0, -t408, 0.0D0, t403, 0.0D0, t453)
      t493 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t217)
      t505 = t288 + t292 - t307 + (-0.90D2 * t104 * t316 + 0.180D3 * t12
     #3 * t6 * t320 - t324) * t175 / 0.1440D4
      t506 = FJET(XB1, XB2, s, t224, 0.0D0, 0.0D0, -t230, -t235, t505)
      t508 = t218 * t217 + t220 * t217 + t222 * t217 + t329 * t328 + t39
     #0 * t389 + t454 * t453 + t485 * t484 + t487 * t389 + t489 * t328 +
     # t491 * t453 + t493 * t217 + t506 * t505
      t509 = FJET(XB1, XB2, s, t224, -t459, 0.0D0, t458, t461, t484)
      t511 = FJET(XB1, XB2, s, t332, 0.0D0, -t335, 0.0D0, 0.0D0, t389)
      t513 = FJET(XB1, XB2, s, t403, 0.0D0, -t408, 0.0D0, 0.0D0, t453)
      t515 = FJET(XB1, XB2, s, t458, 0.0D0, -t459, t224, t461, t484)
      t518 = t224 * t392 * t401
      t519 = t2 * t225
      t520 = t226 * t107
      t523 = Sqrt(t257 * t333 * t17)
      t524 = t25 * t523
      t526 = 0.2D1 * t524 * x2
      t528 = x1 * t106 * x3
      t532 = t519 * (t520 - x2 + t392 + t107 + t526 + 0.1D1 - x3 - t528)
     # * t228 * t401
      t536 = t17 * s * t1 * x1 * t401
      t542 = t519 * x2 * (-0.1D1 + t392 + x1 - t253 - t226 + t254 + 0.2D
     #1 * t524) * t228 * t401
      t543 = x1 * x2
      t544 = t543 * z
      t545 = x2 - t543 + z - t424 + t544
      t549 = t157 * x2
      t558 = 0.2D1 * t543 - t549 + t424 - t158 + t256 - t262 + t226 - x3
     # + t107 - t253 * x2 + 0.2D1 * t549 * z - t266 * x2 + t543 * t11 + 
     #0.2D1 * t26 * t523 + t158 * x2 - z + t526
      t576 = -t528 - 0.3D1 * t544 - t423 + t425 + t265 - t267 - t255 + t
     #263 - 0.2D1 * t524 * t543 + 0.2D1 * t253 * t424 + t266 * t392 - 0.
     #2D1 * t158 * t424 - t253 * x2 * t11 - 0.2D1 * t26 * t523 * x2 + t5
     #20 + 0.2D1 * t26 * t523 * x1 * x2 - x2
      t579 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t237, t336, -t409,
     # x4)
      t581 = 0.1D1 / (t558 + t576) * t579 * t181
      t583 = t104 * t227 * t545 * t581 / 0.8D1
      t584 = FJET(XB1, XB2, s, t518, -t532, -t536, t542, t461, -t583)
      t587 = t6 * t227 * t545
      t591 = FJET(XB1, XB2, s, t542, -t536, -t532, t518, t461, -t583)
      t596 = FJET(XB1, XB2, s, -t335, 0.0D0, t332, 0.0D0, 0.0D0, t389)
      t598 = FJET(XB1, XB2, s, -t230, 0.0D0, 0.0D0, t224, -t235, t505)
      t607 = -t465 - (-0.90D2 * t104 * t474 - t479) * t175 * t132 / 0.72
     #0D3
      t608 = FJET(XB1, XB2, s, -t459, t224, t458, 0.0D0, t461, t607)
      t610 = FJET(XB1, XB2, s, -t408, 0.0D0, t403, 0.0D0, 0.0D0, t453)
      t612 = FJET(XB1, XB2, s, -t536, t542, t518, -t532, t461, -t583)
      t617 = FJET(XB1, XB2, s, -t532, t518, t542, -t536, t461, -t583)
      t622 = t509 * t484 + t511 * t389 + t513 * t453 + t515 * t484 - t58
     #4 * 0.3141592653589793D1 * t587 * t581 / 0.8D1 - t591 * 0.31415926
     #53589793D1 * t587 * t581 / 0.8D1 + t596 * t389 + t598 * t505 + t60
     #8 * t607 + t610 * t453 - t612 * 0.3141592653589793D1 * t587 * t581
     # / 0.8D1 - t617 * 0.3141592653589793D1 * t587 * t581 / 0.8D1
      rrgg2gght8s1e0 = t508 + t622

      end function



      doubleprecision function rrgg2gght8s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
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
      t24 = z * t23
      t26 = Sqrt(-x3 * t16)
      t30 = 0.1D1 / (-x3 - z + 0.2D1 * t24 * t26)
      t38 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t47 = 0.1D1 / x3
      t52 = log(0.4D1 * t12 * t9)
      t59 = t52 ** 2
      t62 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t66 = lh ** 2
      t68 = 0.3141592653589793D1 ** 2
      t75 = 0.3141592653589793D1 * t6
      t79 = (t3 + z * t3 * t30) * t47
      t80 = 0.1D1 / x2
      t84 = x2 ** 2
      t85 = t84 * t9
      t88 = log(0.4D1 * t85 * t12)
      t93 = 0.3141592653589793D1 * lh
      t96 = 0.180D3 * t93 * t6 * t3
      t100 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t9
      t109 = log(0.4D1 * t106 * t12)
      t120 = (-0.90D2 * t4 * t6 * (-t15 - t21 * z * t30) + (0.180D3 * t4
     # * lh - 0.90D2 * t38 * 0.3141592653589793D1) * t6 * (0.1D1 + z * t
     #30)) * t47 / 0.2880D4 + (0.180D3 * (t38 - t52 * t3) * 0.3141592653
     #589793D1 * lh - 0.90D2 * (-t52 * t38 + t59 * t3 / 0.2D1 + t62) * 0
     #.3141592653589793D1 + t4 * (-0.180D3 * t66 + 0.30D2 * t68)) * t6 /
     # 0.2880D4 - t75 * t79 * t80 / 0.16D2 - (-0.90D2 * t75 * (-t38 + t8
     #8 * t3) - t96) * t80 / 0.1440D4 - t4 * t6 * t100 * t80 / 0.8D1 + (
     #-0.90D2 * t75 * (t38 - t109 * t3) + t96) * t100 / 0.1440D4 - t75 *
     # t79 * t100 / 0.16D2
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t120)
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t120)
      t125 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t120)
      t127 = t2 * x1
      t128 = -0.1D1 + x1
      t129 = x1 * z
      t130 = 0.1D1 - x1 + t129
      t131 = 0.1D1 / t130
      t133 = t2 * t128 * t131
      t134 = t1 ** 2
      t135 = s * t134
      t137 = x1 * t128 * t131
      t138 = t135 * t137
      t139 = -t128
      t140 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t139, 0.10D1, 0.10
     #D1, x4)
      t144 = t75 * t140 * t100 * t80 / 0.8D1
      t145 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t139, 0.10D1, 0.10
     #D1, x4)
      t147 = t128 ** 2
      t151 = log(0.4D1 * t106 * t12 * t131 * t147)
      t153 = -t145 + t151 * t140
      t158 = 0.180D3 * t93 * t6 * t140
      t163 = x3 * x1
      t166 = x3 * t105
      t170 = Sqrt(-x3 * t130 * t16)
      t179 = t129 - 0.3D1 * t163 * z - z - t166 + 0.2D1 * t163 + 0.2D1 *
     # t24 * t170 - x3 - x1 * t11 + t163 * t11 + 0.2D1 * t166 * z - t105
     # * t11 * x3
      t187 = t75 * (-z * t130 * t140 / t179 - t140) * t47 * t100 / 0.16D
     #2
      t188 = t144 + (-0.90D2 * t75 * t153 - t158) * t100 / 0.1440D4 - t1
     #87
      t189 = FJET(XB1, XB2, s, 0.0D0, t127, -t133, 0.0D0, -t138, t188)
      t191 = x2 * s
      t192 = t191 * t1
      t193 = -0.1D1 + x2
      t194 = t193 * s
      t195 = t194 * t1
      t196 = -t193
      t197 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t196, 0.10
     #D1, x4)
      t202 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t196, 0.10
     #D1, x4)
      t206 = log(-0.4D1 * t85 * t12 * t193)
      t221 = t75 * t197 * t47 * t80 / 0.16D2 - (-0.90D2 * t75 * (t202 - 
     #t206 * t197) + 0.180D3 * t93 * t6 * t197) * t80 / 0.1440D4 + t75 *
     # t197 * t100 * t80 / 0.8D1
      t222 = FJET(XB1, XB2, s, 0.0D0, t192, 0.0D0, -t195, 0.0D0, t221)
      t224 = x2 * x3
      t225 = t84 * x3
      t228 = Sqrt(x3 * t193 * t16)
      t229 = t23 * t228
      t231 = 0.2D1 * t229 * x2
      t234 = 0.1D1 / (0.1D1 - x3 + t224)
      t236 = t2 * (0.1D1 - x3 - x2 + t224 + t225 + t231) * t234
      t241 = t2 * x2 * (-0.1D1 + t224 + 0.2D1 * t229) * t234
      t243 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t196, -t16
     # * t234, x4)
      t247 = x2 * z
      t259 = 0.1D1 / (z * t84 * x3 - t247 - t224 * z - t225 + x3 + z + x
     #2 - 0.2D1 * t24 * t228 + 0.2D1 * t24 * t228 * x2 - t231) * (t247 -
     # z - x2) * t47 * t80
      t261 = t75 * t243 * t259 / 0.16D2
      t262 = FJET(XB1, XB2, s, 0.0D0, t236, 0.0D0, -t241, 0.0D0, t261)
      t264 = t6 * t243
      t268 = t1 * t128
      t270 = t194 * t268 * t131
      t271 = t191 * t268
      t273 = t135 * t193 * t137
      t274 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t139, t196, 0.10D1
     #, x4)
      t276 = t274 * t100 * t80
      t278 = t75 * t276 / 0.8D1
      t279 = FJET(XB1, XB2, s, 0.0D0, t270, t127, -t271, t273, -t278)
      t284 = FJET(XB1, XB2, s, 0.0D0, -t195, 0.0D0, t192, 0.0D0, t221)
      t286 = FJET(XB1, XB2, s, 0.0D0, -t133, t127, 0.0D0, -t138, t188)
      t288 = FJET(XB1, XB2, s, 0.0D0, -t241, 0.0D0, t236, 0.0D0, t261)
      t293 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t120)
      t301 = t144 + (-0.90D2 * t75 * t153 - t158) * t100 / 0.1440D4 - t1
     #87
      t302 = FJET(XB1, XB2, s, t127, 0.0D0, 0.0D0, -t133, -t138, t301)
      t304 = FJET(XB1, XB2, s, t127, -t271, 0.0D0, t270, t273, -t278)
      t309 = FJET(XB1, XB2, s, t192, 0.0D0, -t195, 0.0D0, 0.0D0, t221)
      t311 = FJET(XB1, XB2, s, t236, 0.0D0, -t241, 0.0D0, 0.0D0, t261)
      t316 = FJET(XB1, XB2, s, t270, 0.0D0, -t271, t127, t273, -t278)
      t321 = FJET(XB1, XB2, s, -t195, 0.0D0, t192, 0.0D0, 0.0D0, t221)
      t323 = FJET(XB1, XB2, s, -t133, 0.0D0, 0.0D0, t127, -t138, t301)
      t325 = FJET(XB1, XB2, s, -t271, t127, t270, 0.0D0, t273, -t278)
      t330 = FJET(XB1, XB2, s, -t241, 0.0D0, t236, 0.0D0, 0.0D0, t261)
      rrgg2gght8s1em1 = t121 * t120 + t123 * t120 + t125 * t120 + t189 *
     # t188 + t222 * t221 + t262 * 0.3141592653589793D1 * t264 * t259 / 
     #0.16D2 - t279 * 0.3141592653589793D1 * t6 * t276 / 0.8D1 + t284 * 
     #t221 + t286 * t188 + t288 * 0.3141592653589793D1 * t264 * t259 / 0
     #.16D2 + t293 * t120 + t302 * t301 - t304 * 0.3141592653589793D1 * 
     #t6 * t276 / 0.8D1 + t309 * t221 + t311 * 0.3141592653589793D1 * t2
     #64 * t259 / 0.16D2 - t316 * 0.3141592653589793D1 * t6 * t276 / 0.8
     #D1 + t321 * t221 + t323 * t301 - t325 * 0.3141592653589793D1 * t6 
     #* t276 / 0.8D1 + t330 * 0.3141592653589793D1 * t264 * t259 / 0.16D
     #2

      end function



      doubleprecision function rrgg2gght8s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t34 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t35 = z ** 2
      t37 = Sin(t7)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t35 * t38)
      t49 = -t4 * t6 * (0.1D1 + z / (-x3 - z + 0.2D1 * z * t8 * t12)) / 
     #x3 / 0.32D2 - t4 * t6 * t24 / 0.16D2 - t4 * t6 * t28 / 0.16D2 + (0
     #.180D3 * t4 * lh - 0.90D2 * (t34 - t41 * t3) * 0.3141592653589793D
     #1) * t6 / 0.2880D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t54 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t56 = t2 * x1
      t57 = -0.1D1 + x1
      t60 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t62 = t2 * t57 * t60
      t63 = t1 ** 2
      t67 = s * t63 * x1 * t57 * t60
      t68 = 0.3141592653589793D1 * t6
      t70 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, -t57, 0.10D1, 0.10D
     #1, x4)
      t73 = t68 * t70 * t28 / 0.16D2
      t74 = FJET(XB1, XB2, s, 0.0D0, t56, -t62, 0.0D0, -t67, t73)
      t77 = t6 * t70 * t28
      t81 = x2 * s * t1
      t82 = -0.1D1 + x2
      t84 = t82 * s * t1
      t86 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, -t82, 0.10D
     #1, x4)
      t89 = t68 * t86 * t24 / 0.16D2
      t90 = FJET(XB1, XB2, s, 0.0D0, t81, 0.0D0, -t84, 0.0D0, t89)
      t93 = t6 * t86 * t24
      t96 = FJET(XB1, XB2, s, 0.0D0, -t84, 0.0D0, t81, 0.0D0, t89)
      t100 = FJET(XB1, XB2, s, 0.0D0, -t62, t56, 0.0D0, -t67, t73)
      t104 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t106 = FJET(XB1, XB2, s, t56, 0.0D0, 0.0D0, -t62, -t67, t73)
      t110 = FJET(XB1, XB2, s, t81, 0.0D0, -t84, 0.0D0, 0.0D0, t89)
      t114 = FJET(XB1, XB2, s, -t84, 0.0D0, t81, 0.0D0, 0.0D0, t89)
      t118 = FJET(XB1, XB2, s, -t62, 0.0D0, 0.0D0, t56, -t67, t73)
      rrgg2gght8s1em2 = t50 * t49 + t52 * t49 + t54 * t49 + t74 * 0.3141
     #592653589793D1 * t77 / 0.16D2 + t90 * 0.3141592653589793D1 * t93 /
     # 0.16D2 + t96 * 0.3141592653589793D1 * t93 / 0.16D2 + t100 * 0.314
     #1592653589793D1 * t77 / 0.16D2 + t104 * t49 + t106 * 0.31415926535
     #89793D1 * t77 / 0.16D2 + t110 * 0.3141592653589793D1 * t93 / 0.16D
     #2 + t114 * 0.3141592653589793D1 * t93 / 0.16D2 + t118 * 0.31415926
     #53589793D1 * t77 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = s * (-0.1D1 + z)
      t3 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * 0.3141592653589793D1 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = 0.3141592653589793D1 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s1em3 = -t9 * t3 * t11 / 0.32D2 - t13 * t3 * t11 / 0.32D
     #2 - t16 * t3 * t11 / 0.32D2 - t19 * t3 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2gght8s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght8s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = -0.180D3 * t3 + 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = s ** 2
      t10 = 0.1D1 / t9
      t11 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = x3 * t14
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t20 = log(0.4D1 * t15 * t17)
      t21 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t26 = 0.3141592653589793D1 * t10
      t27 = t20 ** 2
      t30 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t34 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t43 = -0.60D2 * lh * t5 + 0.2884936567583026D3 + 0.120D3 * t3 * lh
      t44 = 0.3141592653589793D1 * t43
      t45 = t10 * t21
      t46 = t44 * t45
      t47 = 0.3141592653589793D1 * lh
      t56 = 0.1D1 / x3
      t59 = t17 * t14
      t61 = log(0.4D1 * t59)
      t62 = t61 ** 2
      t63 = t62 * 0.3141592653589793D1
      t67 = t62 * t61 * 0.3141592653589793D1
      t69 = t61 * 0.3141592653589793D1
      t88 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t97 = t5 ** 2
      t98 = t3 ** 2
      t104 = t62 ** 2
      t111 = x1 ** 2
      t112 = x3 * t111
      t115 = log(0.4D1 * t112 * t59)
      t117 = t115 ** 2
      t128 = t8 * t45
      t131 = 0.1D1 / x1
      t134 = t111 * t14
      t135 = t134 * t17
      t137 = log(0.4D1 * t135)
      t142 = t137 ** 2
      t162 = x2 * x3
      t165 = log(0.4D1 * t162 * t135)
      t174 = 0.1D1 / x2
      t175 = t131 * t174
      t178 = t111 * x2
      t181 = log(0.4D1 * t178 * t59)
      t183 = t181 ** 2
      t200 = log(0.4D1 * t162 * t59)
      t202 = t200 ** 2
      t217 = x2 * t14
      t220 = log(0.4D1 * t217 * t17)
      t225 = t220 ** 2
      t245 = (t8 * t10 * (t11 - t20 * t21) - 0.90D2 * t26 * (t27 * t11 /
     # 0.2D1 + t30 - t27 * t20 * t21 / 0.6D1 - t20 * t34) + t46 + 0.180D
     #3 * t47 * t10 * (-t20 * t11 + t27 * t21 / 0.2D1 + t34)) * t56 / 0.
     #1440D4 + (0.90D2 * t63 * lh + t44 + 0.15D2 * t67 - t69 * t7) * t10
     # * t11 / 0.1440D4 + (-0.180D3 * t69 * lh - 0.45D2 * t63 + t8) * t1
     #0 * t34 / 0.1440D4 + (0.180D3 * t47 + 0.90D2 * t69) * t10 * t30 / 
     #0.1440D4 - t26 * t88 / 0.16D2 + (-0.30D2 * t67 * lh + t63 * t7 / 0
     #.2D1 - t69 * t43 + 0.3141592653589793D1 * (-0.5769873135166051D3 *
     # lh - t97 - 0.60D2 * t98 + 0.60D2 * t3 * t5) - 0.15D2 / 0.4D1 * t1
     #04 * 0.3141592653589793D1) * t10 * t21 / 0.1440D4 - (-0.90D2 * t26
     # * (t115 * t11 - t117 * t21 / 0.2D1 - t34) + 0.180D3 * t47 * t10 *
     # (-t11 + t115 * t21) - t128) * t56 * t131 / 0.720D3 - (t8 * t10 * 
     #(-t11 + t137 * t21) - 0.90D2 * t26 * (-t142 * t11 / 0.2D1 - t30 + 
     #t142 * t137 * t21 / 0.6D1 + t137 * t34) - t46 + 0.180D3 * t47 * t1
     #0 * (t137 * t11 - t142 * t21 / 0.2D1 - t34)) * t131 / 0.720D3 + (-
     #0.90D2 * t26 * (t11 - t165 * t21) + 0.180D3 * t47 * t45) * t56 * t
     #175 / 0.720D3 - (-0.90D2 * t26 * (t181 * t11 - t183 * t21 / 0.2D1 
     #- t34) + 0.180D3 * t47 * t10 * (-t11 + t181 * t21) - t128) * t131 
     #* t174 / 0.720D3 - (-0.90D2 * t26 * (t200 * t11 - t202 * t21 / 0.2
     #D1 - t34) + 0.180D3 * t47 * t10 * (-t11 + t200 * t21) - t128) * t5
     #6 * t174 / 0.1440D4 - (t8 * t10 * (-t11 + t220 * t21) - 0.90D2 * t
     #26 * (-t225 * t11 / 0.2D1 - t30 + t225 * t220 * t21 / 0.6D1 + t220
     # * t34) - t46 + 0.180D3 * t47 * t10 * (t220 * t11 - t225 * t21 / 0
     #.2D1 - t34)) * t174 / 0.1440D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t245)
      t248 = 0.3D1 * t162
      t249 = x2 ** 2
      t250 = t249 * x3
      t251 = cos(t12)
      t252 = -0.1D1 + x3
      t254 = Sqrt(-t162 * t252)
      t255 = t251 * t254
      t256 = 0.2D1 * t255
      t258 = 0.2D1 * t255 * x2
      t260 = -0.1D1 + t162
      t261 = 0.1D1 / t260
      t263 = t2 * (-x2 - x3 + t248 - t250 - t256 + t258) * t261
      t264 = -0.1D1 + x2
      t268 = t2 * t264 * (-t162 - 0.1D1 + x3 + t256) * t261
      t270 = t264 ** 2
      t271 = t17 * t270
      t272 = t260 ** 2
      t273 = 0.1D1 / t272
      t275 = t271 * t252 * t273
      t278 = log(-0.4D1 * t162 * t14 * t275)
      t279 = x2 * z
      t280 = 0.1D1 - x2 + t279
      t281 = t278 * t280
      t282 = t252 * t261
      t283 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t282, 
     #x4)
      t285 = t278 ** 2
      t287 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t282, 
     #x4)
      t290 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t282, 
     #x4)
      t294 = z * t249 * x3
      t295 = 0.2D1 * t162
      t296 = z * t251
      t300 = t162 * z
      t302 = 0.1D1 / (-t294 + t250 + x2 - t295 + 0.2D1 * t296 * t254 * x
     #2 - t279 + t300 - t258 + t256 - 0.1D1)
      t306 = t280 * t283
      t315 = t280 * t287 * t302
      t321 = t162 * t134
      t324 = log(-0.4D1 * t321 * t275)
      t331 = t47 * t10
      t338 = -(-0.90D2 * t26 * (-t281 * t283 + t285 * t280 * t287 / 0.2D
     #1 + t280 * t290) * t302 + 0.180D3 * t47 * t10 * (t306 - t281 * t28
     #7) * t302 + t8 * t10 * t315) * t56 * t174 / 0.1440D4 + (0.90D2 * t
     #26 * (t306 - t324 * t280 * t287) * t302 - 0.180D3 * t331 * t315) *
     # t56 * t175 / 0.720D3
      t339 = FJET(XB1, XB2, s, t263, 0.0D0, -t268, 0.0D0, 0.0D0, t338)
      t342 = x2 * s * t1
      t343 = t264 * s
      t344 = t343 * t1
      t345 = t59 * t270
      t348 = log(0.4D1 * t162 * t345)
      t349 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t351 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t352 = t348 ** 2
      t353 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t364 = t10 * t353
      t365 = t8 * t364
      t372 = log(0.4D1 * t217 * t271)
      t377 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t378 = t372 ** 2
      t399 = t112 * x2
      t402 = log(0.4D1 * t399 * t345)
      t415 = log(0.4D1 * t178 * t345)
      t416 = t415 ** 2
      t432 = -(-0.90D2 * t26 * (-t348 * t349 + t351 + t352 * t353 / 0.2D
     #1) + 0.180D3 * t47 * t10 * (-t348 * t353 + t349) + t365) * t56 * t
     #174 / 0.1440D4 - (t8 * t10 * (t349 - t372 * t353) - 0.90D2 * t26 *
     # (t377 - t378 * t372 * t353 / 0.6D1 - t372 * t351 + t378 * t349 / 
     #0.2D1) + t44 * t364 + 0.180D3 * t47 * t10 * (-t372 * t349 + t378 *
     # t353 / 0.2D1 + t351)) * t174 / 0.1440D4 + (-0.90D2 * t26 * (-t349
     # + t402 * t353) - 0.180D3 * t47 * t364) * t56 * t175 / 0.720D3 - (
     #-0.90D2 * t26 * (t351 + t416 * t353 / 0.2D1 - t415 * t349) + 0.180
     #D3 * t47 * t10 * (t349 - t415 * t353) + t365) * t131 * t174 / 0.72
     #0D3
      t433 = FJET(XB1, XB2, s, 0.0D0, t342, 0.0D0, -t344, 0.0D0, t432)
      t435 = FJET(XB1, XB2, s, -t268, 0.0D0, t263, 0.0D0, 0.0D0, t338)
      t437 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t245)
      t439 = t2 * x3
      t440 = t2 * t252
      t441 = -t252
      t442 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t44
     #1, x4)
      t446 = log(-0.4D1 * t15 * t17 * t252)
      t447 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t44
     #1, x4)
      t449 = t442 - t446 * t447
      t452 = t446 ** 2
      t455 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t44
     #1, x4)
      t459 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t44
     #1, x4)
      t461 = t452 * t442 / 0.2D1 + t455 - t452 * t446 * t447 / 0.6D1 - t
     #446 * t459
      t464 = t10 * t447
      t465 = t44 * t464
      t469 = -t446 * t442 + t452 * t447 / 0.2D1 + t459
      t476 = t59 * t252
      t479 = log(-0.4D1 * t112 * t476)
      t481 = t479 ** 2
      t492 = t8 * t464
      t496 = (-0.90D2 * t26 * (-t479 * t442 + t481 * t447 / 0.2D1 + t459
     #) + 0.180D3 * t47 * t10 * (t442 - t479 * t447) + t492) * t56 * t13
     #1 / 0.720D3
      t499 = log(-0.4D1 * t399 * t476)
      t509 = (-0.90D2 * t26 * (-t442 + t499 * t447) - 0.180D3 * t47 * t4
     #64) * t56 * t175 / 0.720D3
      t512 = log(-0.4D1 * t162 * t476)
      t514 = t512 ** 2
      t528 = (-0.90D2 * t26 * (-t512 * t442 + t514 * t447 / 0.2D1 + t459
     #) + 0.180D3 * t47 * t10 * (t442 - t512 * t447) + t492) * t56 * t17
     #4 / 0.1440D4
      t529 = (-t8 * t10 * t449 + 0.90D2 * t26 * t461 - t465 - 0.180D3 * 
     #t47 * t10 * t469) * t56 / 0.1440D4 - t496 + t509 - t528
      t530 = FJET(XB1, XB2, s, t439, 0.0D0, -t440, 0.0D0, 0.0D0, t529)
      t532 = -0.1D1 + x1
      t534 = x1 * z
      t535 = 0.1D1 - x1 + t534
      t536 = 0.1D1 / t535
      t538 = t2 * t532 * x2 * t536
      t539 = t1 * t532
      t540 = t343 * t539
      t541 = t2 * x1
      t542 = t1 ** 2
      t547 = s * t542 * x2 * x1 * t532 * t536
      t548 = -t532
      t549 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t548, x2, 0.10D1, 
     #x4)
      t550 = t17 * t536
      t551 = t532 ** 2
      t553 = t550 * t551 * t270
      t556 = log(0.4D1 * t321 * t553)
      t557 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t548, x2, 0.10D1, 
     #x4)
      t562 = t10 * t557
      t567 = (-0.90D2 * t26 * (t549 - t556 * t557) + 0.180D3 * t47 * t56
     #2) * t56 * t175
      t568 = t178 * t14
      t571 = log(0.4D1 * t568 * t553)
      t573 = t571 ** 2
      t576 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t548, x2, 0.10D1, 
     #x4)
      t577 = -t571 * t549 + t573 * t557 / 0.2D1 + t576
      t581 = t549 - t571 * t557
      t585 = t8 * t562
      t590 = t567 / 0.720D3 - (0.90D2 * t26 * t577 - 0.180D3 * t47 * t10
     # * t581 - t585) * t131 * t174 / 0.720D3
      t591 = FJET(XB1, XB2, s, -t538, 0.0D0, t540, t541, -t547, t590)
      t593 = t2 * t532
      t594 = t112 * t14
      t596 = t17 * t551 * t536
      t599 = log(0.4D1 * t594 * t596)
      t600 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, 0.10D
     #1, x4)
      t602 = t599 ** 2
      t603 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, 0.10D
     #1, x4)
      t606 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, 0.10D
     #1, x4)
      t615 = t10 * t603
      t616 = t8 * t615
      t619 = (-0.90D2 * t26 * (-t599 * t600 + t602 * t603 / 0.2D1 + t606
     #) + 0.180D3 * t47 * t10 * (t600 - t599 * t603) + t616) * t56 * t13
     #1
      t622 = log(0.4D1 * t134 * t596)
      t624 = -t600 + t622 * t603
      t627 = t622 ** 2
      t630 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, 0.10D
     #1, x4)
      t635 = -t627 * t600 / 0.2D1 - t630 + t627 * t622 * t603 / 0.6D1 + 
     #t622 * t606
      t638 = t44 * t615
      t642 = t622 * t600 - t627 * t603 / 0.2D1 - t606
      t649 = t59 * t536 * t551
      t652 = log(0.4D1 * t399 * t649)
      t661 = (-0.90D2 * t26 * (-t600 + t652 * t603) - 0.180D3 * t47 * t6
     #15) * t56 * t175
      t664 = log(0.4D1 * t568 * t596)
      t666 = t664 ** 2
      t679 = (-0.90D2 * t26 * (-t664 * t600 + t666 * t603 / 0.2D1 + t606
     #) + 0.180D3 * t47 * t10 * (t600 - t664 * t603) + t616) * t131 * t1
     #74
      t681 = -t619 / 0.720D3 - (-t8 * t10 * t624 + 0.90D2 * t26 * t635 +
     # t638 - 0.180D3 * t47 * t10 * t642) * t131 / 0.720D3 + t661 / 0.72
     #0D3 - t679 / 0.720D3
      t682 = FJET(XB1, XB2, s, t541, -t593, 0.0D0, 0.0D0, 0.0D0, t681)
      t697 = -t619 / 0.720D3 - (-t8 * t10 * t624 + 0.90D2 * t26 * t635 +
     # t638 - 0.180D3 * t47 * t10 * t642) * t131 / 0.720D3 + t661 / 0.72
     #0D3 - t679 / 0.720D3
      t698 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t593, t541, 0.0D0, t697)
      t700 = x3 * x1
      t701 = t2 * t700
      t703 = x3 * s * t539
      t704 = t252 * s
      t705 = t1 * x1
      t706 = t704 * t705
      t707 = t704 * t539
      t712 = log(-0.4D1 * t594 * t550 * t551 * t252)
      t713 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, t441,
     # x4)
      t715 = t712 ** 2
      t716 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, t441,
     # x4)
      t719 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t548, 0.0D0, t441,
     # x4)
      t728 = t10 * t716
      t733 = x2 * t252
      t737 = log(-0.4D1 * t649 * t112 * t733)
      t748 = -(-0.90D2 * t26 * (t712 * t713 - t715 * t716 / 0.2D1 - t719
     #) + 0.180D3 * t47 * t10 * (t712 * t716 - t713) - t8 * t728) * t56 
     #* t131 / 0.720D3 + (-0.90D2 * t26 * (t713 - t737 * t716) + 0.180D3
     # * t47 * t728) * t56 * t175 / 0.720D3
      t749 = FJET(XB1, XB2, s, t701, -t703, -t706, t707, 0.0D0, t748)
      t751 = FJET(XB1, XB2, s, -t706, t707, t701, -t703, 0.0D0, t748)
      t753 = FJET(XB1, XB2, s, 0.0D0, -t344, 0.0D0, t342, 0.0D0, t432)
      t768 = (-t8 * t10 * t449 + 0.90D2 * t26 * t461 - t465 - 0.180D3 * 
     #t47 * t10 * t469) * t56 / 0.1440D4 - t496 + t509 - t528
      t769 = FJET(XB1, XB2, s, 0.0D0, -t440, 0.0D0, t439, 0.0D0, t768)
      t771 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t245)
      t773 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t541, -t593, 0.0D0, t697)
      t775 = FJET(XB1, XB2, s, 0.0D0, t263, 0.0D0, -t268, 0.0D0, t338)
      t777 = t246 * t245 + t339 * t338 + t433 * t432 + t435 * t338 + t43
     #7 * t245 + t530 * t529 + t591 * t590 + t682 * t681 + t698 * t697 +
     # t749 * t748 + t751 * t748 + t753 * t432 + t769 * t768 + t771 * t2
     #45 + t773 * t697 + t775 * t338
      t789 = t567 / 0.720D3 - (0.90D2 * t26 * t577 - 0.180D3 * t47 * t10
     # * t581 - t585) * t131 * t174 / 0.720D3
      t790 = FJET(XB1, XB2, s, t541, t540, 0.0D0, -t538, -t547, t789)
      t792 = FJET(XB1, XB2, s, -t344, 0.0D0, t342, 0.0D0, 0.0D0, t432)
      t794 = FJET(XB1, XB2, s, 0.0D0, -t538, t541, t540, -t547, t789)
      t796 = FJET(XB1, XB2, s, -t703, t701, t707, -t706, 0.0D0, t748)
      t798 = FJET(XB1, XB2, s, 0.0D0, -t268, 0.0D0, t263, 0.0D0, t338)
      t800 = FJET(XB1, XB2, s, -t593, t541, 0.0D0, 0.0D0, 0.0D0, t681)
      t802 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t245)
      t804 = t700 * z
      t805 = t700 * x2
      t807 = t700 * t279
      t809 = t534 * t250
      t812 = Sqrt(-x3 * t535 * t733)
      t813 = t251 * t812
      t815 = 0.2D1 * t813 * x2
      t816 = 0.2D1 * t813
      t818 = x1 * t249 * x3
      t819 = -t804 - 0.2D1 * t805 + 0.2D1 * t807 - t809 + t815 + t248 + 
     #t700 - t250 - x2 - x3 - t816 + t818
      t822 = t593 * t819 * t536 * t261
      t825 = t541 * x3 * t264 * t261
      t830 = t593 * t264 * (-t162 - 0.1D1 + x3 + x1 - t700 - t534 + t804
     # + t816) * t536 * t261
      t832 = t704 * t705 * t261
      t833 = x1 * x2
      t834 = t833 * z
      t835 = -0.1D1 - t279 + t834 + x2 - t534 + x1 - t833
      t836 = t535 * t835
      t837 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t548, x2, t282, x4
     #)
      t847 = log(-0.4D1 * t536 * t273 * t59 * t551 * t112 * t270 * x2 * 
     #t252)
      t849 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t548, x2, t282, x4
     #)
      t856 = t111 * t16
      t863 = 0.1D1 + t295 - t250 + t279 + 0.2D1 * t833 - 0.2D1 * t111 * 
     #z + t856 - t178 - 0.2D1 * x1 - t300 + t294 + 0.2D1 * t534 + t818 -
     # 0.3D1 * t834 + 0.2D1 * t178 * z - t856 * x2
      t868 = t812 * x2
      t885 = t833 * t16 + 0.2D1 * t813 * x1 + t399 + t111 - 0.3D1 * t805
     # + t815 + 0.2D1 * t296 * t868 * x1 - t816 - t700 * x2 * t16 - 0.2D
     #1 * t296 * t868 - 0.2D1 * t813 * t833 - 0.2D1 * t296 * t812 * x1 +
     # 0.4D1 * t807 - t809 + t856 * t162 - 0.2D1 * t112 * t279 - x2
      t887 = 0.1D1 / (t863 + t885)
      t895 = -0.90D2 * t26 * (t836 * t837 - t847 * t535 * t835 * t849) *
     # t887 + 0.180D3 * t331 * t836 * t849 * t887
      t898 = t895 * t56 * t175 / 0.720D3
      t899 = FJET(XB1, XB2, s, -t822, t825, t830, t832, -t547, t898)
      t902 = t56 * t131 * t174
      t905 = FJET(XB1, XB2, s, t342, 0.0D0, -t344, 0.0D0, 0.0D0, t432)
      t907 = FJET(XB1, XB2, s, 0.0D0, t439, 0.0D0, -t440, 0.0D0, t768)
      t909 = FJET(XB1, XB2, s, t540, t541, -t538, 0.0D0, -t547, t789)
      t911 = FJET(XB1, XB2, s, t830, t832, -t822, t825, -t547, t898)
      t915 = FJET(XB1, XB2, s, t825, -t822, t832, t830, -t547, t898)
      t919 = FJET(XB1, XB2, s, t832, t830, t825, -t822, -t547, t898)
      t923 = FJET(XB1, XB2, s, -t440, 0.0D0, t439, 0.0D0, 0.0D0, t529)
      t925 = FJET(XB1, XB2, s, t707, -t706, -t703, t701, 0.0D0, t748)
      t927 = t790 * t789 + t792 * t432 + t794 * t789 + t796 * t748 + t79
     #8 * t338 + t800 * t681 + t802 * t245 + t899 * t895 * t902 / 0.720D
     #3 + t905 * t432 + t907 * t768 + t909 * t789 + t911 * t895 * t902 /
     # 0.720D3 + t915 * t895 * t902 / 0.720D3 + t919 * t895 * t902 / 0.7
     #20D3 + t923 * t529 + t925 * t748
      rrgg2gght8s2e1 = t777 + t927

      end function



      doubleprecision function rrgg2gght8s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = -t4
      t10 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t9, 
     #x4)
      t11 = x2 * x3
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t18 = t17 * t4
      t21 = log(-0.4D1 * t11 * t18)
      t22 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t9, 
     #x4)
      t27 = 0.3141592653589793D1 * lh
      t28 = t7 * t22
      t30 = 0.180D3 * t27 * t28
      t32 = 0.1D1 / x3
      t34 = 0.1D1 / x2
      t36 = (-0.90D2 * t8 * (t10 - t21 * t22) + t30) * t32 * t34 / 0.144
     #0D4
      t37 = x3 * t14
      t41 = log(-0.4D1 * t37 * t16 * t4)
      t43 = t41 ** 2
      t46 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t9, 
     #x4)
      t47 = t41 * t10 - t43 * t22 / 0.2D1 - t46
      t51 = -t10 + t41 * t22
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = -0.180D3 * t55 + 0.30D2 * t57
      t60 = 0.3141592653589793D1 * t59
      t61 = t60 * t28
      t65 = x1 ** 2
      t66 = x3 * t65
      t69 = log(-0.4D1 * t66 * t18)
      t76 = 0.1D1 / x1
      t78 = (-0.90D2 * t8 * (t10 - t69 * t22) + t30) * t32 * t76 / 0.720
     #D3
      t81 = t32 * t76 * t34
      t83 = t8 * t22 * t81 / 0.8D1
      t84 = -t36 + (-0.90D2 * t8 * t47 + 0.180D3 * t27 * t7 * t51 - t61)
     # * t32 / 0.1440D4 - t78 + t83
      t85 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t84)
      t87 = t2 * x1
      t88 = -0.1D1 + x2
      t89 = t88 * s
      t90 = -0.1D1 + x1
      t91 = t1 * t90
      t92 = t89 * t91
      t94 = x1 * z
      t95 = 0.1D1 - x1 + t94
      t96 = 0.1D1 / t95
      t98 = t2 * t90 * x2 * t96
      t99 = t1 ** 2
      t104 = s * t99 * x2 * x1 * t90 * t96
      t105 = -t90
      t106 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t105, x2, 0.10D1, 
     #x4)
      t109 = t8 * t106 * t81 / 0.8D1
      t110 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t105, x2, 0.10D1, 
     #x4)
      t111 = x2 * t65
      t112 = t111 * t14
      t113 = t16 * t96
      t114 = t90 ** 2
      t115 = t88 ** 2
      t120 = log(0.4D1 * t112 * t113 * t114 * t115)
      t122 = -t110 + t120 * t106
      t127 = 0.180D3 * t27 * t7 * t106
      t132 = -t109 - (-0.90D2 * t8 * t122 - t127) * t76 * t34 / 0.720D3
      t133 = FJET(XB1, XB2, s, t87, t92, 0.0D0, -t98, -t104, t132)
      t135 = FJET(XB1, XB2, s, t92, t87, -t98, 0.0D0, -t104, t132)
      t147 = -t36 + (-0.90D2 * t8 * t47 + 0.180D3 * t27 * t7 * t51 - t61
     #) * t32 / 0.1440D4 - t78 + t83
      t148 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t147)
      t150 = t2 * t90
      t151 = x3 * x1
      t152 = t151 * z
      t153 = cos(t12)
      t157 = Sqrt(-x3 * t95 * x2 * t4)
      t158 = t153 * t157
      t159 = 0.2D1 * t158
      t162 = -0.1D1 + t11
      t163 = 0.1D1 / t162
      t166 = t150 * t88 * (-t11 - 0.1D1 + x3 + x1 - t151 - t94 + t152 + 
     #t159) * t96 * t163
      t167 = t4 * s
      t168 = t1 * x1
      t170 = t167 * t168 * t163
      t171 = t151 * x2
      t173 = x2 * z
      t174 = t151 * t173
      t176 = x2 ** 2
      t177 = t176 * x3
      t178 = t94 * t177
      t180 = 0.2D1 * t158 * x2
      t181 = 0.3D1 * t11
      t183 = x1 * t176 * x3
      t184 = -t152 - 0.2D1 * t171 + 0.2D1 * t174 - t178 + t180 + t181 + 
     #t151 - t177 - x2 - x3 - t159 + t183
      t187 = t150 * t184 * t96 * t163
      t190 = t87 * x3 * t88 * t163
      t191 = x1 * x2
      t192 = t191 * z
      t193 = -0.1D1 - t173 + t192 + x2 - t94 + x1 - t191
      t196 = t4 * t163
      t197 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t105, x2, t196, x4
     #)
      t201 = t65 * t15
      t212 = 0.1D1 - t159 + 0.2D1 * t191 - 0.2D1 * t65 * z + t201 - t111
     # - 0.3D1 * t171 + t180 + t183 - 0.3D1 * t192 + 0.2D1 * t111 * z - 
     #t201 * x2 + t191 * t15 + 0.2D1 * t158 * x1 + t66 * x2 - 0.2D1 * x1
      t213 = 0.2D1 * t11
      t220 = z * t153
      t221 = t157 * x2
      t230 = z * t176 * x3
      t231 = t11 * z
      t236 = t213 - t177 + t173 + 0.4D1 * t174 - t178 + t201 * t11 - 0.2
     #D1 * t66 * t173 - t151 * x2 * t15 - 0.2D1 * t220 * t221 - 0.2D1 * 
     #t158 * t191 - 0.2D1 * t220 * t157 * x1 + t230 - t231 + 0.2D1 * t94
     # + t65 + 0.2D1 * t220 * t221 * x1 - x2
      t240 = t197 / (t212 + t236) * t81
      t242 = t8 * t95 * t193 * t240 / 0.8D1
      t243 = FJET(XB1, XB2, s, t166, t170, -t187, t190, -t104, -t242)
      t246 = t7 * t95 * t193
      t250 = FJET(XB1, XB2, s, t170, t166, t190, -t187, -t104, -t242)
      t256 = Sqrt(-t11 * t4)
      t257 = t153 * t256
      t258 = 0.2D1 * t257
      t262 = t2 * t88 * (-t11 - 0.1D1 + x3 + t258) * t163
      t264 = 0.2D1 * t257 * x2
      t267 = t2 * (-x2 - x3 + t181 - t177 - t258 + t264) * t163
      t268 = 0.1D1 - x2 + t173
      t269 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t196, 
     #x4)
      t270 = t268 * t269
      t276 = 0.1D1 / (-t230 + t177 + x2 - t213 + 0.2D1 * t220 * t256 * x
     #2 - t173 + t231 - t264 + t258 - 0.1D1)
      t282 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t196, 
     #x4)
      t285 = t16 * t115
      t286 = t162 ** 2
      t292 = log(-0.4D1 * t11 * t14 * t285 * t4 / t286)
      t307 = t8 * t270 * t276 * t32 * t76 * t34 / 0.8D1 - (-0.90D2 * t8 
     #* (t268 * t282 - t292 * t268 * t269) * t276 + 0.180D3 * t27 * t7 *
     # t270 * t276) * t32 * t34 / 0.1440D4
      t308 = FJET(XB1, XB2, s, -t262, 0.0D0, t267, 0.0D0, 0.0D0, t307)
      t310 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t105, 0.0D0, 0.10D
     #1, x4)
      t311 = t66 * t14
      t313 = t16 * t114 * t96
      t316 = log(0.4D1 * t311 * t313)
      t317 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t105, 0.0D0, 0.10D
     #1, x4)
      t322 = t7 * t317
      t324 = 0.180D3 * t27 * t322
      t328 = (-0.90D2 * t8 * (t310 - t316 * t317) + t324) * t32 * t76 / 
     #0.720D3
      t331 = t8 * t317 * t81 / 0.8D1
      t334 = log(0.4D1 * t112 * t313)
      t342 = (-0.90D2 * t8 * (t310 - t334 * t317) + t324) * t76 * t34 / 
     #0.720D3
      t343 = t65 * t14
      t346 = log(0.4D1 * t343 * t313)
      t348 = t346 ** 2
      t351 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, t105, 0.0D0, 0.10D
     #1, x4)
      t352 = t346 * t310 - t348 * t317 / 0.2D1 - t351
      t356 = -t310 + t346 * t317
      t360 = t60 * t322
      t364 = -t328 + t331 - t342 - (0.90D2 * t8 * t352 - 0.180D3 * t27 *
     # t7 * t356 + t360) * t76 / 0.720D3
      t365 = FJET(XB1, XB2, s, t87, -t150, 0.0D0, 0.0D0, 0.0D0, t364)
      t367 = FJET(XB1, XB2, s, 0.0D0, -t98, t87, t92, -t104, t132)
      t370 = x2 * s * t1
      t371 = t89 * t1
      t372 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t376 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t377 = t17 * t115
      t380 = log(0.4D1 * t111 * t377)
      t385 = t7 * t372
      t387 = 0.180D3 * t27 * t385
      t394 = log(0.4D1 * t11 * t377)
      t403 = x2 * t14
      t406 = log(0.4D1 * t403 * t285)
      t408 = t406 ** 2
      t411 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t424 = t8 * t372 * t81 / 0.8D1 - (-0.90D2 * t8 * (t376 - t380 * t3
     #72) + t387) * t76 * t34 / 0.720D3 - (-0.90D2 * t8 * (-t394 * t372 
     #+ t376) + t387) * t32 * t34 / 0.1440D4 - (-0.90D2 * t8 * (-t406 * 
     #t376 + t408 * t372 / 0.2D1 + t411) + 0.180D3 * t27 * t7 * (t376 - 
     #t406 * t372) + t60 * t385) * t34 / 0.1440D4
      t425 = FJET(XB1, XB2, s, 0.0D0, t370, 0.0D0, -t371, 0.0D0, t424)
      t437 = -t328 + t331 - t342 - (0.90D2 * t8 * t352 - 0.180D3 * t27 *
     # t7 * t356 + t360) * t76 / 0.720D3
      t438 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t150, t87, 0.0D0, t437)
      t442 = log(0.4D1 * t37 * t16)
      t443 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t445 = t442 ** 2
      t446 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t449 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t458 = t7 * t446
      t459 = t60 * t458
      t464 = log(0.4D1 * t17)
      t465 = t464 * 0.3141592653589793D1
      t468 = t464 ** 2
      t469 = t468 * 0.3141592653589793D1
      t475 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t502 = log(0.4D1 * t11 * t17)
      t508 = 0.180D3 * t27 * t458
      t515 = log(0.4D1 * t403 * t16)
      t517 = t515 ** 2
      t533 = log(0.4D1 * t66 * t17)
      t547 = log(0.4D1 * t111 * t17)
      t558 = log(0.4D1 * t343 * t16)
      t560 = t558 ** 2
      t574 = (-0.90D2 * t8 * (-t442 * t443 + t445 * t446 / 0.2D1 + t449)
     # + 0.180D3 * t27 * t7 * (t443 - t446 * t442) + t459) * t32 / 0.144
     #0D4 + (-0.180D3 * t465 * lh - 0.45D2 * t469 + t60) * t7 * t443 / 0
     #.1440D4 - t8 * t475 / 0.16D2 + (0.90D2 * t469 * lh + 0.31415926535
     #89793D1 * (-0.60D2 * lh * t57 + 0.2884936567583026D3 + 0.120D3 * t
     #55 * lh) + 0.15D2 * t468 * t464 * 0.3141592653589793D1 - t465 * t5
     #9) * t7 * t446 / 0.1440D4 + (0.180D3 * t27 + 0.90D2 * t465) * t7 *
     # t449 / 0.1440D4 - (-0.90D2 * t8 * (-t443 + t502 * t446) - t508) *
     # t32 * t34 / 0.1440D4 - (-0.90D2 * t8 * (t515 * t443 - t517 * t446
     # / 0.2D1 - t449) + 0.180D3 * t27 * t7 * (-t443 + t515 * t446) - t4
     #59) * t34 / 0.1440D4 - (-0.90D2 * t8 * (-t443 + t533 * t446) - t50
     #8) * t32 * t76 / 0.720D3 - t8 * t446 * t81 / 0.8D1 - (-0.90D2 * t8
     # * (-t443 + t547 * t446) - t508) * t76 * t34 / 0.720D3 - (-0.90D2 
     #* t8 * (t558 * t443 - t560 * t446 / 0.2D1 - t449) + 0.180D3 * t27 
     #* t7 * (-t443 + t558 * t446) - t459) * t76 / 0.720D3
      t575 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t574)
      t577 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t574)
      t579 = FJET(XB1, XB2, s, -t150, t87, 0.0D0, 0.0D0, 0.0D0, t364)
      t581 = FJET(XB1, XB2, s, 0.0D0, t267, 0.0D0, -t262, 0.0D0, t307)
      t583 = FJET(XB1, XB2, s, t370, 0.0D0, -t371, 0.0D0, 0.0D0, t424)
      t585 = t85 * t84 + t133 * t132 + t135 * t132 + t148 * t147 - t243 
     #* 0.3141592653589793D1 * t246 * t240 / 0.8D1 - t250 * 0.3141592653
     #589793D1 * t246 * t240 / 0.8D1 + t308 * t307 + t365 * t364 + t367 
     #* t132 + t425 * t424 + t438 * t437 + t575 * t574 + t577 * t574 + t
     #579 * t364 + t581 * t307 + t583 * t424
      t586 = FJET(XB1, XB2, s, t267, 0.0D0, -t262, 0.0D0, 0.0D0, t307)
      t588 = FJET(XB1, XB2, s, -t187, t190, t166, t170, -t104, -t242)
      t593 = FJET(XB1, XB2, s, t190, -t187, t170, t166, -t104, -t242)
      t598 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t574)
      t600 = FJET(XB1, XB2, s, 0.0D0, -t262, 0.0D0, t267, 0.0D0, t307)
      t609 = -t109 - (-0.90D2 * t8 * t122 - t127) * t76 * t34 / 0.720D3
      t610 = FJET(XB1, XB2, s, -t98, 0.0D0, t92, t87, -t104, t609)
      t612 = FJET(XB1, XB2, s, 0.0D0, -t371, 0.0D0, t370, 0.0D0, t424)
      t614 = t2 * t151
      t616 = x3 * s * t91
      t617 = t167 * t168
      t618 = t167 * t91
      t623 = log(-0.4D1 * t311 * t113 * t114 * t4)
      t624 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t105, 0.0D0, t9, x
     #4)
      t626 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t105, 0.0D0, t9, x
     #4)
      t640 = -(-0.90D2 * t8 * (t623 * t624 - t626) - 0.180D3 * t27 * t7 
     #* t624) * t32 * t76 / 0.720D3 - t8 * t624 * t81 / 0.8D1
      t641 = FJET(XB1, XB2, s, t614, -t616, -t617, t618, 0.0D0, t640)
      t643 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t87, -t150, 0.0D0, t437)
      t645 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t147)
      t647 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t84)
      t649 = FJET(XB1, XB2, s, t618, -t617, -t616, t614, 0.0D0, t640)
      t651 = FJET(XB1, XB2, s, -t616, t614, t618, -t617, 0.0D0, t640)
      t653 = FJET(XB1, XB2, s, -t371, 0.0D0, t370, 0.0D0, 0.0D0, t424)
      t655 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t574)
      t657 = FJET(XB1, XB2, s, -t617, t618, t614, -t616, 0.0D0, t640)
      t659 = t586 * t307 - t588 * 0.3141592653589793D1 * t246 * t240 / 0
     #.8D1 - t593 * 0.3141592653589793D1 * t246 * t240 / 0.8D1 + t598 * 
     #t574 + t600 * t307 + t610 * t609 + t612 * t424 + t641 * t640 + t64
     #3 * t437 + t645 * t147 + t647 * t84 + t649 * t640 + t651 * t640 + 
     #t653 * t424 + t655 * t574 + t657 * t640
      rrgg2gght8s2e0 = t585 + t659

      end function



      doubleprecision function rrgg2gght8s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t21 = 0.3141592653589793D1 * lh
      t24 = 0.180D3 * t21 * t4 * t16
      t26 = 0.1D1 / x3
      t32 = log(0.4D1 * t12 * t9)
      t33 = t32 * 0.3141592653589793D1
      t41 = t32 ** 2
      t44 = lh ** 2
      t46 = 0.3141592653589793D1 ** 2
      t54 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t57 = t16 * t26
      t58 = 0.1D1 / x2
      t62 = x2 * t9
      t65 = log(0.4D1 * t62 * t12)
      t73 = 0.1D1 / x1
      t78 = x1 ** 2
      t79 = t78 * t9
      t82 = log(0.4D1 * t79 * t12)
      t93 = (-0.90D2 * t5 * (t6 - t15 * t16) + t24) * t26 / 0.1440D4 + (
     #0.180D3 * t21 + 0.90D2 * t33) * t4 * t6 / 0.1440D4 + (-0.180D3 * t
     #33 * lh - 0.45D2 * t41 * 0.3141592653589793D1 + 0.3141592653589793
     #D1 * (-0.180D3 * t44 + 0.30D2 * t46)) * t4 * t16 / 0.1440D4 - t5 *
     # t54 / 0.16D2 - t5 * t57 * t58 / 0.16D2 - (-0.90D2 * t5 * (-t6 + t
     #65 * t16) - t24) * t58 / 0.1440D4 - t5 * t16 * t73 * t58 / 0.8D1 -
     # (-0.90D2 * t5 * (-t6 + t82 * t16) - t24) * t73 / 0.720D3 - t5 * t
     #57 * t73 / 0.8D1
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t93)
      t96 = -0.1D1 + x2
      t97 = x2 * x3
      t98 = cos(t7)
      t99 = -0.1D1 + x3
      t101 = Sqrt(-t97 * t99)
      t102 = t98 * t101
      t103 = 0.2D1 * t102
      t107 = 0.1D1 / (-0.1D1 + t97)
      t109 = t2 * t96 * (-t97 - 0.1D1 + x3 + t103) * t107
      t111 = x2 ** 2
      t112 = t111 * x3
      t114 = 0.2D1 * t102 * x2
      t117 = t2 * (-x2 - x3 + 0.3D1 * t97 - t112 - t103 + t114) * t107
      t118 = x2 * z
      t119 = 0.1D1 - x2 + t118
      t122 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t99 * 
     #t107, x4)
      t135 = t122 / (-z * t111 * x3 + t112 + x2 - 0.2D1 * t97 + 0.2D1 * 
     #z * t98 * t101 * x2 - t118 + t97 * z - t114 + t103 - 0.1D1) * t26 
     #* t58
      t137 = t5 * t119 * t135 / 0.16D2
      t138 = FJET(XB1, XB2, s, -t109, 0.0D0, t117, 0.0D0, 0.0D0, t137)
      t140 = t4 * t119
      t144 = t96 * s
      t145 = -0.1D1 + x1
      t146 = t1 * t145
      t147 = t144 * t146
      t148 = t2 * x1
      t152 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t154 = t2 * t145 * x2 * t152
      t155 = t1 ** 2
      t160 = s * t155 * x2 * x1 * t145 * t152
      t161 = -t145
      t162 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t161, x2, 0.10D1, 
     #x4)
      t164 = t162 * t73 * t58
      t166 = t5 * t164 / 0.8D1
      t167 = FJET(XB1, XB2, s, t147, t148, -t154, 0.0D0, -t160, -t166)
      t172 = t99 * s
      t174 = t172 * t1 * x1
      t175 = t172 * t146
      t177 = t2 * x1 * x3
      t179 = x3 * s * t146
      t180 = -t99
      t181 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t161, 0.0D0, t180,
     # x4)
      t183 = t181 * t26 * t73
      t185 = t5 * t183 / 0.8D1
      t186 = FJET(XB1, XB2, s, -t174, t175, t177, -t179, 0.0D0, -t185)
      t191 = FJET(XB1, XB2, s, -t154, 0.0D0, t147, t148, -t160, -t166)
      t196 = FJET(XB1, XB2, s, t148, t147, 0.0D0, -t154, -t160, -t166)
      t201 = FJET(XB1, XB2, s, t177, -t179, -t174, t175, 0.0D0, -t185)
      t206 = FJET(XB1, XB2, s, -t179, t177, t175, -t174, 0.0D0, -t185)
      t211 = FJET(XB1, XB2, s, 0.0D0, t117, 0.0D0, -t109, 0.0D0, t137)
      t216 = t2 * t99
      t217 = t2 * x3
      t218 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t18
     #0, x4)
      t222 = log(-0.4D1 * t10 * t12 * t99)
      t223 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t18
     #0, x4)
      t225 = t218 - t222 * t223
      t230 = 0.180D3 * t21 * t4 * t223
      t234 = t223 * t26
      t237 = t5 * t234 * t58 / 0.16D2
      t240 = t5 * t234 * t73 / 0.8D1
      t241 = (0.90D2 * t5 * t225 - t230) * t26 / 0.1440D4 + t237 + t240
      t242 = FJET(XB1, XB2, s, -t216, 0.0D0, t217, 0.0D0, 0.0D0, t241)
      t244 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      t247 = x2 * s * t1
      t248 = t144 * t1
      t249 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t258 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t259 = t96 ** 2
      t263 = log(0.4D1 * t62 * t12 * t259)
      t274 = t5 * t249 * t73 * t58 / 0.8D1 + t5 * t249 * t26 * t58 / 0.1
     #6D2 - (-0.90D2 * t5 * (t258 - t263 * t249) + 0.180D3 * t21 * t4 * 
     #t249) * t58 / 0.1440D4
      t275 = FJET(XB1, XB2, s, 0.0D0, t247, 0.0D0, -t248, 0.0D0, t274)
      t277 = FJET(XB1, XB2, s, -t248, 0.0D0, t247, 0.0D0, 0.0D0, t274)
      t279 = FJET(XB1, XB2, s, 0.0D0, -t154, t148, t147, -t160, -t166)
      t284 = t94 * t93 + t138 * 0.3141592653589793D1 * t140 * t135 / 0.1
     #6D2 - t167 * 0.3141592653589793D1 * t4 * t164 / 0.8D1 - t186 * 0.3
     #141592653589793D1 * t4 * t183 / 0.8D1 - t191 * 0.3141592653589793D
     #1 * t4 * t164 / 0.8D1 - t196 * 0.3141592653589793D1 * t4 * t164 / 
     #0.8D1 - t201 * 0.3141592653589793D1 * t4 * t183 / 0.8D1 - t206 * 0
     #.3141592653589793D1 * t4 * t183 / 0.8D1 + t211 * 0.314159265358979
     #3D1 * t140 * t135 / 0.16D2 + t242 * t241 + t244 * t93 + t275 * t27
     #4 + t277 * t274 - t279 * 0.3141592653589793D1 * t4 * t164 / 0.8D1
      t285 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t93)
      t287 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t93)
      t295 = (0.90D2 * t5 * t225 - t230) * t26 / 0.1440D4 + t237 + t240
      t296 = FJET(XB1, XB2, s, 0.0D0, t217, 0.0D0, -t216, 0.0D0, t295)
      t298 = FJET(XB1, XB2, s, t217, 0.0D0, -t216, 0.0D0, 0.0D0, t241)
      t300 = FJET(XB1, XB2, s, 0.0D0, -t216, 0.0D0, t217, 0.0D0, t295)
      t302 = FJET(XB1, XB2, s, 0.0D0, -t109, 0.0D0, t117, 0.0D0, t137)
      t307 = t2 * t145
      t308 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, t161, 0.0D0, 0.10D
     #1, x4)
      t312 = t5 * t308 * t73 * t58 / 0.8D1
      t313 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, t161, 0.0D0, 0.10D
     #1, x4)
      t315 = t145 ** 2
      t319 = log(0.4D1 * t79 * t12 * t152 * t315)
      t321 = t313 - t319 * t308
      t326 = 0.180D3 * t21 * t4 * t308
      t333 = t5 * t308 * t26 * t73 / 0.8D1
      t334 = t312 - (-0.90D2 * t5 * t321 + t326) * t73 / 0.720D3 + t333
      t335 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t148, -t307, 0.0D0, t334)
      t343 = t312 - (-0.90D2 * t5 * t321 + t326) * t73 / 0.720D3 + t333
      t344 = FJET(XB1, XB2, s, -t307, t148, 0.0D0, 0.0D0, 0.0D0, t343)
      t346 = FJET(XB1, XB2, s, 0.0D0, -t248, 0.0D0, t247, 0.0D0, t274)
      t348 = FJET(XB1, XB2, s, t247, 0.0D0, -t248, 0.0D0, 0.0D0, t274)
      t350 = FJET(XB1, XB2, s, t117, 0.0D0, -t109, 0.0D0, 0.0D0, t137)
      t355 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t307, t148, 0.0D0, t334)
      t357 = FJET(XB1, XB2, s, t148, -t307, 0.0D0, 0.0D0, 0.0D0, t343)
      t359 = FJET(XB1, XB2, s, t175, -t174, -t179, t177, 0.0D0, -t185)
      t364 = t285 * t93 + t287 * t93 + t296 * t295 + t298 * t241 + t300 
     #* t295 + t302 * 0.3141592653589793D1 * t140 * t135 / 0.16D2 + t335
     # * t334 + t344 * t343 + t346 * t274 + t348 * t274 + t350 * 0.31415
     #92653589793D1 * t140 * t135 / 0.16D2 + t355 * t334 + t357 * t343 -
     # t359 * 0.3141592653589793D1 * t4 * t183 / 0.8D1
      rrgg2gght8s2em1 = t284 + t364

      end function



      doubleprecision function rrgg2gght8s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t24 = z ** 2
      t27 = Sin(x4 * 0.3141592653589793D1)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 + (0.180D3 * 0.3141592653589793D1 
     #* lh + 0.90D2 * t31 * 0.3141592653589793D1) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t44 = -0.1D1 + x1
      t45 = t2 * t44
      t47 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, -t44, 0.0D0, 0.10D1
     #, x4)
      t50 = t5 * t47 * t15 / 0.8D1
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t50)
      t54 = t4 * t47 * t15
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t50)
      t61 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t63 = t2 * x3
      t64 = -0.1D1 + x3
      t65 = t2 * t64
      t67 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, -t64
     #, x4)
      t70 = t5 * t67 * t7 / 0.16D2
      t71 = FJET(XB1, XB2, s, 0.0D0, t63, 0.0D0, -t65, 0.0D0, t70)
      t74 = t4 * t67 * t7
      t78 = x2 * s * t1
      t81 = (-0.1D1 + x2) * s * t1
      t82 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t85 = t5 * t82 * t11 / 0.16D2
      t86 = FJET(XB1, XB2, s, 0.0D0, t78, 0.0D0, -t81, 0.0D0, t85)
      t89 = t4 * t82 * t11
      t92 = FJET(XB1, XB2, s, 0.0D0, -t65, 0.0D0, t63, 0.0D0, t70)
      t96 = FJET(XB1, XB2, s, 0.0D0, -t81, 0.0D0, t78, 0.0D0, t85)
      t100 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t102 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t50)
      t106 = FJET(XB1, XB2, s, t63, 0.0D0, -t65, 0.0D0, 0.0D0, t70)
      t110 = FJET(XB1, XB2, s, t78, 0.0D0, -t81, 0.0D0, 0.0D0, t85)
      t114 = FJET(XB1, XB2, s, -t65, 0.0D0, t63, 0.0D0, 0.0D0, t70)
      t118 = FJET(XB1, XB2, s, -t81, 0.0D0, t78, 0.0D0, 0.0D0, t85)
      t122 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t50)
      rrgg2gght8s2em2 = t39 * t38 + t41 * t38 + t51 * 0.3141592653589793
     #D1 * t54 / 0.8D1 + t57 * 0.3141592653589793D1 * t54 / 0.8D1 + t61 
     #* t38 + t71 * 0.3141592653589793D1 * t74 / 0.16D2 + t86 * 0.314159
     #2653589793D1 * t89 / 0.16D2 + t92 * 0.3141592653589793D1 * t74 / 0
     #.16D2 + t96 * 0.3141592653589793D1 * t89 / 0.16D2 + t100 * t38 + t
     #102 * 0.3141592653589793D1 * t54 / 0.8D1 + t106 * 0.31415926535897
     #93D1 * t74 / 0.16D2 + t110 * 0.3141592653589793D1 * t89 / 0.16D2 +
     # t114 * 0.3141592653589793D1 * t74 / 0.16D2 + t118 * 0.31415926535
     #89793D1 * t89 / 0.16D2 + t122 * 0.3141592653589793D1 * t54 / 0.8D1

      end function



      doubleprecision function rrgg2gght8s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s2em3 = -t9 * 0.3141592653589793D1 * t11 / 0.16D2 - t13 
     #* 0.3141592653589793D1 * t11 / 0.16D2 - t16 * 0.3141592653589793D1
     # * t11 / 0.16D2 - t19 * 0.3141592653589793D1 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght8s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght8s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t7 = t5 * t6
      t10 = lh ** 2
      t12 = 0.3141592653589793D1 ** 2
      t14 = 0.180D3 * t10 - 0.30D2 * t12
      t15 = 0.3141592653589793D1 * t14
      t16 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t17 = t5 * t16
      t19 = 0.3141592653589793D1 * t5
      t20 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
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
      t41 = cos(t28)
      t42 = x3 * z
      t44 = Sqrt(-t42 * t34)
      t48 = 0.1D1 / (-z - x3 + 0.2D1 * t41 * t44)
      t52 = t39 ** 2
      t56 = t33 ** 2
      t64 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t71 = 0.60D2 * lh * t12 - 0.2884936567583026D3 - 0.120D3 * t10 * l
     #h
      t72 = 0.3141592653589793D1 * t71
      t73 = t72 * t17
      t92 = 0.1D1 / x3
      t95 = t26 * t30
      t97 = log(0.4D1 * t95)
      t98 = t97 ** 2
      t101 = t98 * t97
      t120 = t12 ** 2
      t121 = t10 ** 2
      t133 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t134 = t98 ** 2
      t143 = x1 ** 2
      t144 = x3 * t143
      t147 = log(0.4D1 * t144 * t95)
      t149 = t147 ** 2
      t152 = t95 * t35
      t155 = log(-0.4D1 * t144 * t152)
      t156 = t155 * z
      t158 = t155 ** 2
      t162 = z * t20
      t168 = z * t6
      t178 = z * t16 * t48
      t184 = 0.1D1 / x1
      t187 = t143 * t30
      t188 = t187 * t26
      t190 = log(0.4D1 * t188)
      t195 = t190 ** 2
      t215 = 0.1D1 - x2
      t216 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, 0.10D
     #1, x4)
      t217 = x2 ** 2
      t218 = x3 * t217
      t219 = t218 * t143
      t220 = -t215
      t221 = t95 * t220
      t224 = log(-0.4D1 * t219 * t221)
      t225 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, 0.10D
     #1, x4)
      t229 = log(0.4D1 * t218 * t188)
      t233 = log(-0.4D1 * t219 * t152)
      t242 = t5 * (-t178 + t225 - t16)
      t247 = 0.1D1 / x2
      t248 = t184 * t247
      t251 = t217 * t143
      t254 = log(0.4D1 * t251 * t95)
      t256 = t254 ** 2
      t261 = log(-0.4D1 * t251 * t221)
      t263 = t261 ** 2
      t266 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, 0.10D
     #1, x4)
      t276 = t225 - t16
      t285 = log(-0.4D1 * t218 * t221)
      t287 = t285 ** 2
      t292 = log(-0.4D1 * t218 * t152)
      t293 = t292 * z
      t295 = t292 ** 2
      t303 = log(0.4D1 * t218 * t95)
      t305 = t303 ** 2
      t325 = t26 * t217
      t328 = log(0.4D1 * t325 * t30)
      t330 = t30 * t220
      t333 = log(-0.4D1 * t325 * t330)
      t338 = t333 ** 2
      t341 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, 0.10D
     #1, x4)
      t346 = t328 ** 2
      t372 = ((-0.180D3 * t3 * t7 + t15 * t17 + 0.90D2 * t19 * t20) * (t
     #33 + t39 * z * t48) + 0.90D2 * t19 * t16 * (t52 * t39 * z * t48 / 
     #0.6D1 + t56 * t33 / 0.6D1) + (t15 * t7 + 0.90D2 * t19 * t64 + t73 
     #- 0.180D3 * t3 * t5 * t20) * (-0.1D1 - z * t48) + (0.90D2 * t19 * 
     #t6 - 0.180D3 * t3 * t17) * (-t56 / 0.2D1 - t52 * z * t48 / 0.2D1))
     # * t92 / 0.2880D4 - (-0.180D3 * (t98 * t6 / 0.2D1 + t64 - t101 * t
     #16 / 0.6D1 - t97 * t20) * 0.3141592653589793D1 * lh + (-t97 * t6 +
     # t98 * t16 / 0.2D1 + t20) * 0.3141592653589793D1 * t14 + (t6 - t97
     # * t16) * 0.3141592653589793D1 * t71 + t16 * 0.3141592653589793D1 
     #* (t120 + 0.60D2 * t121 + 0.5769873135166051D3 * lh - 0.60D2 * t10
     # * t12) + 0.90D2 * (-t101 * t6 / 0.6D1 + t98 * t20 / 0.2D1 - t97 *
     # t64 + t133 + t134 * t16 / 0.24D2) * 0.3141592653589793D1) * t5 / 
     #0.2880D4 + (0.90D2 * t19 * (t147 * t6 - t149 * t16 / 0.2D1 - (-t15
     #6 * t6 + t158 * z * t16 / 0.2D1 + t162) * t48 - t20) - 0.180D3 * t
     #3 * t5 * (-t6 - (t168 - t156 * t16) * t48 + t147 * t16) + t15 * t5
     # * (-t16 - t178)) * t92 * t184 / 0.1440D4 - (t15 * t5 * (t6 - t190
     # * t16) + 0.90D2 * t19 * (t195 * t6 / 0.2D1 + t64 - t195 * t190 * 
     #t16 / 0.6D1 - t190 * t20) + t73 - 0.180D3 * t3 * t5 * (-t190 * t6 
     #+ t195 * t16 / 0.2D1 + t20)) * t184 / 0.1440D4 + (0.90D2 * t19 * (
     #t216 - t224 * t225 - t6 + t229 * t16 - (t168 - t233 * z * t16) * t
     #48) - 0.180D3 * t3 * t242) * t92 * t248 / 0.720D3 + (0.90D2 * t19 
     #* (t254 * t6 - t256 * t16 / 0.2D1 - t20 - t261 * t216 + t263 * t22
     #5 / 0.2D1 + t266) - 0.180D3 * t3 * t5 * (-t6 + t254 * t16 + t216 -
     # t261 * t225) + t15 * t5 * t276) * t184 * t247 / 0.720D3 + (0.90D2
     # * t19 * (-t285 * t216 + t287 * t225 / 0.2D1 + t266 - (-t293 * t6 
     #+ t295 * z * t16 / 0.2D1 + t162) * t48 + t303 * t6 - t305 * t16 / 
     #0.2D1 - t20) - 0.180D3 * t3 * t5 * (t216 - t285 * t225 - (t168 - t
     #293 * t16) * t48 - t6 + t303 * t16) + t15 * t242) * t92 * t247 / 0
     #.1440D4 - (t15 * t5 * (t6 - t328 * t16 - t216 + t333 * t225) + 0.9
     #0D2 * t19 * (-t338 * t216 / 0.2D1 - t341 + t338 * t333 * t225 / 0.
     #6D1 + t333 * t266 + t346 * t6 / 0.2D1 + t64 - t346 * t328 * t16 / 
     #0.6D1 - t328 * t20) - t72 * t5 * t276 - 0.180D3 * t3 * t5 * (-t328
     # * t6 + t346 * t16 / 0.2D1 + t20 + t333 * t216 - t338 * t225 / 0.2
     #D1 - t266)) * t247 / 0.1440D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t372)
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t372)
      t377 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t372)
      t379 = x2 * x3
      t380 = 0.1D1 - x3 + t379
      t381 = 0.1D1 / t380
      t382 = t379 * t381
      t383 = t2 * t382
      t384 = t34 * t381
      t385 = t2 * t384
      t386 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, -t384
     #, x4)
      t387 = z * t386
      t388 = t218 * t187
      t390 = t380 ** 2
      t391 = 0.1D1 / t390
      t392 = t34 * t391
      t396 = log(0.4D1 * t388 * t26 * t220 * t392)
      t398 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, -t384
     #, x4)
      t401 = t220 * t34
      t403 = Sqrt(t42 * t401)
      t407 = 0.1D1 / (-z - x3 + t379 + 0.2D1 * t41 * t403)
      t411 = t3 * t5
      t413 = z * t398 * t407
      t424 = log(0.4D1 * t218 * t26 * t330 * t392)
      t425 = t424 * z
      t427 = t424 ** 2
      t431 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t215, -t384
     #, x4)
      t449 = (0.90D2 * t19 * (t387 - t396 * z * t398) * t407 - 0.180D3 *
     # t411 * t413) * t92 * t248 / 0.720D3 + (0.90D2 * t19 * (-t425 * t3
     #86 + t427 * z * t398 / 0.2D1 + z * t431) * t407 - 0.180D3 * t3 * t
     #5 * (t387 - t425 * t398) * t407 + t15 * t5 * t413) * t92 * t247 / 
     #0.1440D4
      t450 = FJET(XB1, XB2, s, 0.0D0, t383, 0.0D0, -t385, 0.0D0, t449)
      t453 = t1 * x1
      t454 = x1 * z
      t455 = -z - x1 + t454
      t456 = 0.1D1 / t455
      t458 = t220 * s * t453 * t456
      t459 = -0.1D1 + x1
      t460 = t2 * t459
      t462 = x2 * s * t453
      t463 = t1 ** 2
      t464 = s * t463
      t467 = x1 * t459 * t456
      t468 = t464 * t220 * t467
      t469 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t215, 0.10D1, 
     #x4)
      t470 = 0.1D1 / t24
      t471 = t459 ** 2
      t472 = t470 * t471
      t474 = t472 * t456 * t220
      t477 = log(0.4D1 * t388 * t474)
      t478 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t215, 0.10D1, 
     #x4)
      t483 = t5 * t478
      t489 = t251 * t30
      t492 = log(0.4D1 * t489 * t474)
      t494 = t492 ** 2
      t497 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, t215, 0.10D1, 
     #x4)
      t511 = (0.90D2 * t19 * (-t469 + t477 * t478) + 0.180D3 * t3 * t483
     #) * t92 * t248 / 0.720D3 + (0.90D2 * t19 * (t492 * t469 - t494 * t
     #478 / 0.2D1 - t497) - 0.180D3 * t3 * t5 * (-t469 + t492 * t478) - 
     #t15 * t483) * t184 * t247 / 0.720D3
      t512 = FJET(XB1, XB2, s, 0.0D0, t458, -t460, t462, -t468, t511)
      t515 = t2 * x1 * t456
      t516 = t464 * t467
      t517 = t144 * t30
      t519 = t472 * t456 * t35
      t522 = log(0.4D1 * t517 * t519)
      t523 = t522 * z
      t524 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t526 = t522 ** 2
      t528 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t531 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t535 = x3 * x1
      t536 = t535 * z
      t537 = x1 * t24
      t538 = t535 * t24
      t539 = t143 * t24
      t540 = t539 * x3
      t542 = 0.2D1 * t144 * z
      t543 = z * t41
      t544 = x3 * t455
      t546 = Sqrt(t544 * t34)
      t550 = 0.1D1 / (-t454 - t536 - t144 + t537 + t538 - t540 + t542 - 
     #t42 + 0.2D1 * t543 * t546 - t24)
      t552 = t472 * t456
      t555 = log(-0.4D1 * t517 * t552)
      t557 = t555 ** 2
      t573 = z * t455
      t577 = t5 * (t528 - t573 * t528 * t550)
      t582 = (0.90D2 * t19 * (-(-t523 * t524 + t526 * z * t528 / 0.2D1 +
     # z * t531) * t455 * t550 - t555 * t524 + t557 * t528 / 0.2D1 + t53
     #1) - 0.180D3 * t3 * t5 * (t524 - t555 * t528 - (z * t524 - t523 * 
     #t528) * t455 * t550) + t15 * t577) * t92 * t184 / 0.1440D4
      t585 = log(-0.4D1 * t187 * t552)
      t587 = -t524 + t585 * t528
      t590 = t585 ** 2
      t593 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t598 = -t590 * t524 / 0.2D1 - t593 + t590 * t585 * t528 / 0.6D1 + 
     #t585 * t531
      t601 = t5 * t528
      t602 = t72 * t601
      t606 = t585 * t524 - t590 * t528 / 0.2D1 - t531
      t614 = t471 * t456
      t618 = log(-0.4D1 * t219 * t30 * t470 * t614)
      t623 = log(0.4D1 * t388 * t519)
      t637 = (0.90D2 * t19 * (t524 - t618 * t528 - (t573 * t524 - t623 *
     # z * t455 * t528) * t550) - 0.180D3 * t3 * t577) * t92 * t248 / 0.
     #720D3
      t640 = log(-0.4D1 * t489 * t552)
      t642 = t640 ** 2
      t657 = (0.90D2 * t19 * (-t640 * t524 + t642 * t528 / 0.2D1 + t531)
     # - 0.180D3 * t3 * t5 * (t524 - t640 * t528) + t15 * t601) * t184 *
     # t247 / 0.720D3
      t658 = t582 - (t15 * t5 * t587 + 0.90D2 * t19 * t598 - t602 - 0.18
     #0D3 * t3 * t5 * t606) * t184 / 0.1440D4 + t637 + t657
      t659 = FJET(XB1, XB2, s, 0.0D0, -t460, -t515, 0.0D0, t516, t658)
      t661 = FJET(XB1, XB2, s, 0.0D0, -t515, -t460, 0.0D0, t516, t658)
      t663 = FJET(XB1, XB2, s, 0.0D0, -t385, 0.0D0, t383, 0.0D0, t449)
      t665 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t372)
      t667 = FJET(XB1, XB2, s, t462, -t460, t458, 0.0D0, -t468, t511)
      t669 = FJET(XB1, XB2, s, t383, 0.0D0, -t385, 0.0D0, 0.0D0, t449)
      t671 = FJET(XB1, XB2, s, t458, 0.0D0, t462, -t460, -t468, t511)
      t676 = t34 * s * t1 * t459 * t381
      t677 = t2 * x1
      t679 = Sqrt(-t544 * t401)
      t680 = t41 * t679
      t686 = t677 * x2 * (-x3 + t379 - z + t42 - x1 + t535 + t454 - t536
     # + 0.2D1 * t680) * t456 * t381
      t687 = t460 * t382
      t688 = t454 * t218
      t689 = t680 * x2
      t694 = x1 * t217 * x3
      t698 = t677 * (-t688 + 0.2D1 * t689 - x2 + t379 + 0.1D1 - x3 + z *
     # t217 * x3 + t694) * t456 * t381
      t699 = x1 * x2
      t700 = t699 * z
      t701 = -z - t699 + t700
      t702 = t701 * t455
      t703 = x2 * z
      t711 = t454 + t144 - t537 + t42 + t536 - t538 + t540 - t542 + t688
     # - 0.2D1 * t535 * t703 - t539 * t379 + 0.2D1 * t144 * t703 + t535 
     #* x2 * t24
      t712 = t143 * x2
      t728 = t712 + 0.2D1 * t454 * t689 + t24 - t694 + t700 - t379 * z +
     # t535 * x2 - 0.2D1 * t543 * t679 - 0.2D1 * t712 * z + t539 * x2 - 
     #t699 * t24 - t144 * x2 - 0.2D1 * x1 * t41 * t679 * x2
      t730 = 0.1D1 / (t711 + t728)
      t731 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t215, -t384, x
     #4)
      t740 = log(-0.4D1 * t218 * t187 * t470 * t614 * t401 * t391)
      t743 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t215, -t384, x
     #4)
      t753 = 0.90D2 * t19 * (t702 * t730 * t731 - t740 * t701 * t455 * t
     #730 * t743) - 0.180D3 * t411 * t702 * t730 * t743
      t756 = t753 * t92 * t248 / 0.720D3
      t757 = FJET(XB1, XB2, s, t676, t686, -t687, -t698, -t468, t756)
      t760 = t92 * t184 * t247
      t763 = FJET(XB1, XB2, s, t686, t676, -t698, -t687, -t468, t756)
      t780 = t582 - (t15 * t5 * t587 + 0.90D2 * t19 * t598 - t602 - 0.18
     #0D3 * t3 * t5 * t606) * t184 / 0.1440D4 + t637 + t657
      t781 = FJET(XB1, XB2, s, -t460, 0.0D0, 0.0D0, -t515, t516, t780)
      t783 = FJET(XB1, XB2, s, -t460, t462, 0.0D0, t458, -t468, t511)
      t785 = FJET(XB1, XB2, s, -t515, 0.0D0, 0.0D0, -t460, t516, t780)
      t787 = FJET(XB1, XB2, s, -t385, 0.0D0, t383, 0.0D0, 0.0D0, t449)
      t789 = FJET(XB1, XB2, s, -t698, -t687, t686, t676, -t468, t756)
      t793 = FJET(XB1, XB2, s, -t687, -t698, t676, t686, -t468, t756)
      rrgg2gght8s3e1 = t373 * t372 + t375 * t372 + t377 * t372 + t450 * 
     #t449 + t512 * t511 + t659 * t658 + t661 * t658 + t663 * t449 + t66
     #5 * t372 + t667 * t511 + t669 * t449 + t671 * t511 + t757 * t753 *
     # t760 / 0.720D3 + t763 * t753 * t760 / 0.720D3 + t781 * t780 + t78
     #3 * t511 + t785 * t780 + t787 * t449 + t789 * t753 * t760 / 0.720D
     #3 + t793 * t753 * t760 / 0.720D3

      end function



      doubleprecision function rrgg2gght8s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
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
      t26 = cos(t11)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t18)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t40 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t43 = 0.3141592653589793D1 * lh
      t44 = t4 * t6
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t60 = 0.3141592653589793D1 * t59
      t61 = t60 * t44
      t62 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t70 = 0.1D1 / x3
      t73 = t9 * t13
      t75 = log(0.4D1 * t73)
      t77 = t75 ** 2
      t93 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t108 = 0.1D1 - x2
      t109 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t108, 0.10D
     #1, x4)
      t110 = x2 ** 2
      t111 = x3 * t110
      t112 = -t108
      t113 = t73 * t112
      t116 = log(-0.4D1 * t111 * t113)
      t117 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t108, 0.10D
     #1, x4)
      t119 = z * t40
      t120 = t73 * t19
      t123 = log(-0.4D1 * t111 * t120)
      t130 = log(0.4D1 * t111 * t73)
      t136 = z * t6 * t33
      t137 = -t136 + t117 - t6
      t143 = 0.1D1 / x2
      t146 = t9 * t110
      t149 = log(0.4D1 * t146 * t13)
      t151 = t149 ** 2
      t154 = t13 * t112
      t157 = log(-0.4D1 * t146 * t154)
      t159 = t157 ** 2
      t162 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t108, 0.10D
     #1, x4)
      t172 = t6 - t117
      t178 = x1 ** 2
      t179 = x3 * t178
      t182 = log(-0.4D1 * t179 * t120)
      t189 = log(0.4D1 * t179 * t73)
      t200 = 0.1D1 / x1
      t205 = t70 * t200 * t143
      t208 = t110 * t178
      t211 = log(0.4D1 * t208 * t73)
      t215 = log(-0.4D1 * t208 * t113)
      t228 = t178 * t13
      t231 = log(0.4D1 * t228 * t9)
      t233 = t231 ** 2
      t247 = (0.90D2 * t5 * t6 * (-t17 / 0.2D1 - t24 * z * t33 / 0.2D1) 
     #+ (0.90D2 * t5 * t40 - 0.180D3 * t43 * t44) * (t16 + t23 * z * t33
     #) + (-0.180D3 * t43 * t4 * t40 + t61 + 0.90D2 * t5 * t62) * (-0.1D
     #1 - z * t33)) * t70 / 0.2880D4 - (-0.180D3 * (-t75 * t40 + t77 * t
     #6 / 0.2D1 + t62) * 0.3141592653589793D1 * lh + t6 * 0.314159265358
     #9793D1 * (0.60D2 * lh * t57 - 0.2884936567583026D3 - 0.120D3 * t55
     # * lh) + 0.90D2 * (t77 * t40 / 0.2D1 + t93 - t77 * t75 * t6 / 0.6D
     #1 - t75 * t62) * 0.3141592653589793D1 + (t40 - t75 * t6) * 0.31415
     #92653589793D1 * t59) * t4 / 0.2880D4 + (0.90D2 * t5 * (t109 - t116
     # * t117 - (t119 - t123 * z * t6) * t33 - t40 + t130 * t6) - 0.180D
     #3 * t43 * t4 * t137) * t70 * t143 / 0.1440D4 - (0.90D2 * t5 * (-t1
     #49 * t40 + t151 * t6 / 0.2D1 + t62 + t157 * t109 - t159 * t117 / 0
     #.2D1 - t162) - 0.180D3 * t43 * t4 * (t40 - t149 * t6 - t109 + t157
     # * t117) + t60 * t4 * t172) * t143 / 0.1440D4 + (0.90D2 * t5 * (-t
     #40 - (t119 - t182 * z * t6) * t33 + t189 * t6) - 0.180D3 * t43 * t
     #4 * (-t6 - t136)) * t70 * t200 / 0.1440D4 + t5 * t137 * t205 / 0.8
     #D1 + (0.90D2 * t5 * (-t40 + t211 * t6 + t109 - t215 * t117) + 0.18
     #0D3 * t43 * t4 * t172) * t200 * t143 / 0.720D3 - (0.90D2 * t5 * (-
     #t231 * t40 + t233 * t6 / 0.2D1 + t62) - 0.180D3 * t43 * t4 * (t40 
     #- t231 * t6) + t61) * t200 / 0.1440D4
      t248 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t247)
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t247)
      t252 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t247)
      t254 = x2 * x3
      t255 = 0.1D1 - x3 + t254
      t256 = 0.1D1 / t255
      t257 = t254 * t256
      t258 = t2 * t257
      t259 = t18 * t256
      t260 = t2 * t259
      t261 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t108, -t259
     #, x4)
      t264 = t255 ** 2
      t270 = log(0.4D1 * t111 * t9 * t154 * t18 / t264)
      t272 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t108, -t259
     #, x4)
      t275 = t112 * t18
      t277 = Sqrt(t27 * t275)
      t281 = 0.1D1 / (-z - x3 + t254 + 0.2D1 * t26 * t277)
      t286 = z * t272
      t300 = (0.90D2 * t5 * (z * t261 - t270 * z * t272) * t281 - 0.180D
     #3 * t43 * t4 * t286 * t281) * t70 * t143 / 0.1440D4 + t5 * t286 * 
     #t281 * t70 * t200 * t143 / 0.8D1
      t301 = FJET(XB1, XB2, s, 0.0D0, t258, 0.0D0, -t260, 0.0D0, t300)
      t304 = t1 * x1
      t305 = x1 * z
      t306 = -z - x1 + t305
      t307 = 0.1D1 / t306
      t309 = t112 * s * t304 * t307
      t310 = -0.1D1 + x1
      t311 = t2 * t310
      t313 = x2 * s * t304
      t314 = t1 ** 2
      t315 = s * t314
      t318 = x1 * t310 * t307
      t319 = t315 * t112 * t318
      t320 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t108, 0.10D1, 
     #x4)
      t324 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t108, 0.10D1, 
     #x4)
      t325 = t208 * t13
      t326 = 0.1D1 / t7
      t328 = t310 ** 2
      t333 = log(0.4D1 * t325 * t326 * t307 * t328 * t112)
      t345 = -t5 * t320 * t205 / 0.8D1 + (0.90D2 * t5 * (-t324 + t333 * 
     #t320) + 0.180D3 * t43 * t4 * t320) * t200 * t143 / 0.720D3
      t346 = FJET(XB1, XB2, s, 0.0D0, t309, -t311, t313, -t319, t345)
      t349 = t2 * x1 * t307
      t350 = t315 * t318
      t351 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t352 = t179 * t13
      t353 = t326 * t328
      t354 = t353 * t307
      t357 = log(-0.4D1 * t352 * t354)
      t358 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t365 = log(0.4D1 * t352 * t353 * t307 * t19)
      t370 = x3 * x1
      t371 = t370 * z
      t372 = x1 * t7
      t373 = t370 * t7
      t374 = t178 * t7
      t375 = t374 * x3
      t377 = 0.2D1 * t179 * z
      t378 = z * t26
      t379 = x3 * t306
      t381 = Sqrt(t379 * t18)
      t385 = 0.1D1 / (-t305 - t371 - t179 + t372 + t373 - t375 + t377 - 
     #t27 + 0.2D1 * t378 * t381 - t7)
      t393 = t358 - z * t306 * t358 * t385
      t400 = (0.90D2 * t5 * (t351 - t357 * t358 - (z * t351 - t365 * z *
     # t358) * t306 * t385) - 0.180D3 * t43 * t4 * t393) * t70 * t200 / 
     #0.1440D4
      t403 = t5 * t393 * t205 / 0.8D1
      t406 = log(-0.4D1 * t325 * t354)
      t411 = t4 * t358
      t417 = (0.90D2 * t5 * (t351 - t406 * t358) - 0.180D3 * t43 * t411)
     # * t200 * t143 / 0.720D3
      t420 = log(-0.4D1 * t228 * t354)
      t422 = t420 ** 2
      t425 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t426 = t420 * t351 - t422 * t358 / 0.2D1 - t425
      t430 = -t351 + t420 * t358
      t434 = t60 * t411
      t438 = t400 + t403 + t417 - (0.90D2 * t5 * t426 - 0.180D3 * t43 * 
     #t4 * t430 - t434) * t200 / 0.1440D4
      t439 = FJET(XB1, XB2, s, 0.0D0, -t311, -t349, 0.0D0, t350, t438)
      t441 = FJET(XB1, XB2, s, 0.0D0, -t349, -t311, 0.0D0, t350, t438)
      t443 = FJET(XB1, XB2, s, 0.0D0, -t260, 0.0D0, t258, 0.0D0, t300)
      t445 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t247)
      t447 = FJET(XB1, XB2, s, t313, -t311, t309, 0.0D0, -t319, t345)
      t449 = FJET(XB1, XB2, s, t258, 0.0D0, -t260, 0.0D0, 0.0D0, t300)
      t451 = FJET(XB1, XB2, s, t309, 0.0D0, t313, -t311, -t319, t345)
      t456 = t18 * s * t1 * t310 * t256
      t457 = t2 * x1
      t459 = Sqrt(-t379 * t275)
      t460 = t26 * t459
      t466 = t457 * x2 * (-x3 + t254 - z + t27 - x1 + t370 + t305 - t371
     # + 0.2D1 * t460) * t307 * t256
      t467 = t311 * t257
      t468 = t305 * t111
      t469 = t460 * x2
      t474 = x1 * t110 * x3
      t478 = t457 * (-t468 + 0.2D1 * t469 - x2 + t254 + 0.1D1 - x3 + z *
     # t110 * x3 + t474) * t307 * t256
      t479 = x1 * x2
      t480 = t479 * z
      t481 = -z - t479 + t480
      t486 = t178 * x2
      t494 = -0.2D1 * t378 * t459 - 0.2D1 * t486 * z + t374 * x2 - t479 
     #* t7 - t179 * x2 + t179 - t372 + t27 + t305 - t474 + t480 - t254 *
     # z + t370 * x2
      t499 = x2 * z
      t509 = -0.2D1 * x1 * t26 * t459 * x2 + t371 - t373 + t375 - t377 +
     # t468 - 0.2D1 * t370 * t499 - t374 * t254 + 0.2D1 * t179 * t499 + 
     #t370 * x2 * t7 + 0.2D1 * t305 * t469 + t7 + t486
      t512 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t108, -t259, x
     #4)
      t514 = 0.1D1 / (t494 + t509) * t512 * t205
      t516 = t5 * t481 * t306 * t514 / 0.8D1
      t517 = FJET(XB1, XB2, s, t456, t466, -t467, -t478, -t319, t516)
      t520 = t4 * t481 * t306
      t524 = FJET(XB1, XB2, s, t466, t456, -t478, -t467, -t319, t516)
      t539 = t400 + t403 + t417 - (0.90D2 * t5 * t426 - 0.180D3 * t43 * 
     #t4 * t430 - t434) * t200 / 0.1440D4
      t540 = FJET(XB1, XB2, s, -t311, 0.0D0, 0.0D0, -t349, t350, t539)
      t542 = FJET(XB1, XB2, s, -t311, t313, 0.0D0, t309, -t319, t345)
      t544 = FJET(XB1, XB2, s, -t349, 0.0D0, 0.0D0, -t311, t350, t539)
      t546 = FJET(XB1, XB2, s, -t260, 0.0D0, t258, 0.0D0, 0.0D0, t300)
      t548 = FJET(XB1, XB2, s, -t478, -t467, t466, t456, -t319, t516)
      t553 = FJET(XB1, XB2, s, -t467, -t478, t456, t466, -t319, t516)
      rrgg2gght8s3e0 = t248 * t247 + t250 * t247 + t252 * t247 + t301 * 
     #t300 + t346 * t345 + t439 * t438 + t441 * t438 + t443 * t300 + t44
     #5 * t247 + t447 * t345 + t449 * t300 + t451 * t345 + t517 * 0.3141
     #592653589793D1 * t520 * t514 / 0.8D1 + t524 * 0.3141592653589793D1
     # * t520 * t514 / 0.8D1 + t540 * t539 + t542 * t345 + t544 * t539 +
     # t546 * t300 + t548 * 0.3141592653589793D1 * t520 * t514 / 0.8D1 +
     # t553 * 0.3141592653589793D1 * t520 * t514 / 0.8D1

      end function



      doubleprecision function rrgg2gght8s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = -0.1D1 + x3
      t22 = log(-0.4D1 * t10 * t13 / t17)
      t24 = cos(t11)
      t25 = x3 * z
      t27 = Sqrt(-t25 * t17)
      t31 = 0.1D1 / (-z - x3 + 0.2D1 * t24 * t27)
      t37 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t40 = 0.3141592653589793D1 * lh
      t43 = 0.180D3 * t40 * t4 * t6
      t49 = 0.1D1 / x3
      t54 = log(0.4D1 * t9 * t13)
      t61 = t54 ** 2
      t64 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t79 = z * t6 * t31
      t80 = 0.1D1 - x2
      t81 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t80, 0.10D1,
     # x4)
      t84 = 0.1D1 / x2
      t88 = x2 ** 2
      t89 = t9 * t88
      t92 = log(0.4D1 * t89 * t13)
      t94 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t80, 0.10D1,
     # x4)
      t95 = -t80
      t99 = log(-0.4D1 * t89 * t13 * t95)
      t104 = t6 - t81
      t112 = 0.1D1 / x1
      t117 = x1 ** 2
      t118 = t117 * t13
      t121 = log(0.4D1 * t118 * t9)
      t134 = (0.90D2 * t5 * t6 * (t16 + t22 * z * t31) + (0.90D2 * t5 * 
     #t37 - t43) * (-0.1D1 - z * t31)) * t49 / 0.2880D4 - (-0.180D3 * (t
     #37 - t54 * t6) * 0.3141592653589793D1 * lh + 0.90D2 * (-t54 * t37 
     #+ t61 * t6 / 0.2D1 + t64) * 0.3141592653589793D1 + t6 * 0.31415926
     #53589793D1 * (0.180D3 * t69 - 0.30D2 * t71)) * t4 / 0.2880D4 + t5 
     #* (-t79 + t81 - t6) * t49 * t84 / 0.16D2 - (0.90D2 * t5 * (t37 - t
     #92 * t6 - t94 + t99 * t81) - 0.180D3 * t40 * t4 * t104) * t84 / 0.
     #1440D4 - t5 * t104 * t112 * t84 / 0.8D1 - (0.90D2 * t5 * (t37 - t1
     #21 * t6) - t43) * t112 / 0.1440D4 + t5 * (-t6 - t79) * t49 * t112 
     #/ 0.16D2
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t134)
      t137 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t134)
      t139 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t134)
      t141 = x2 * x3
      t143 = 0.1D1 / (0.1D1 - x3 + t141)
      t145 = t2 * t141 * t143
      t146 = t17 * t143
      t147 = t2 * t146
      t149 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t80, -t146,
     # x4)
      t152 = Sqrt(t25 * t95 * t17)
      t159 = t149 / (-z - x3 + t141 + 0.2D1 * t24 * t152) * t49 * t84
      t161 = t5 * z * t159 / 0.16D2
      t162 = FJET(XB1, XB2, s, 0.0D0, t145, 0.0D0, -t147, 0.0D0, t161)
      t164 = t4 * z
      t169 = t1 * x1
      t170 = x1 * z
      t171 = -z - x1 + t170
      t172 = 0.1D1 / t171
      t174 = t95 * s * t169 * t172
      t175 = -0.1D1 + x1
      t176 = t2 * t175
      t178 = x2 * s * t169
      t179 = t1 ** 2
      t180 = s * t179
      t183 = x1 * t175 * t172
      t184 = t180 * t95 * t183
      t185 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t80, 0.10D1, x
     #4)
      t187 = t185 * t112 * t84
      t189 = t5 * t187 / 0.8D1
      t190 = FJET(XB1, XB2, s, 0.0D0, t174, -t176, t178, -t184, -t189)
      t196 = t2 * x1 * t172
      t197 = t180 * t183
      t198 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t202 = t5 * t198 * t112 * t84 / 0.8D1
      t203 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t206 = t175 ** 2
      t210 = log(-0.4D1 * t118 / t7 * t172 * t206)
      t212 = -t203 + t210 * t198
      t217 = 0.180D3 * t40 * t4 * t198
      t222 = x3 * x1
      t224 = x3 * t117
      t234 = Sqrt(x3 * t171 * t17)
      t245 = t5 * (t198 - z * t171 * t198 / (-t170 - t222 * z - t224 + x
     #1 * t7 + t222 * t7 - t117 * t7 * x3 + 0.2D1 * t224 * z - t25 + 0.2
     #D1 * z * t24 * t234 - t7)) * t49 * t112 / 0.16D2
      t246 = t202 - (0.90D2 * t5 * t212 + t217) * t112 / 0.1440D4 + t245
      t247 = FJET(XB1, XB2, s, 0.0D0, -t176, -t196, 0.0D0, t197, t246)
      t249 = FJET(XB1, XB2, s, 0.0D0, -t196, -t176, 0.0D0, t197, t246)
      t251 = FJET(XB1, XB2, s, 0.0D0, -t147, 0.0D0, t145, 0.0D0, t161)
      t256 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t134)
      t258 = FJET(XB1, XB2, s, t178, -t176, t174, 0.0D0, -t184, -t189)
      t263 = FJET(XB1, XB2, s, t145, 0.0D0, -t147, 0.0D0, 0.0D0, t161)
      t268 = FJET(XB1, XB2, s, t174, 0.0D0, t178, -t176, -t184, -t189)
      t279 = t202 - (0.90D2 * t5 * t212 + t217) * t112 / 0.1440D4 + t245
      t280 = FJET(XB1, XB2, s, -t176, 0.0D0, 0.0D0, -t196, t197, t279)
      t282 = FJET(XB1, XB2, s, -t176, t178, 0.0D0, t174, -t184, -t189)
      t287 = FJET(XB1, XB2, s, -t196, 0.0D0, 0.0D0, -t176, t197, t279)
      t289 = FJET(XB1, XB2, s, -t147, 0.0D0, t145, 0.0D0, 0.0D0, t161)
      rrgg2gght8s3em1 = t135 * t134 + t137 * t134 + t139 * t134 + t162 *
     # 0.3141592653589793D1 * t164 * t159 / 0.16D2 - t190 * 0.3141592653
     #589793D1 * t4 * t187 / 0.8D1 + t247 * t246 + t249 * t246 + t251 * 
     #0.3141592653589793D1 * t164 * t159 / 0.16D2 + t256 * t134 - t258 *
     # 0.3141592653589793D1 * t4 * t187 / 0.8D1 + t263 * 0.3141592653589
     #793D1 * t164 * t159 / 0.16D2 - t268 * 0.3141592653589793D1 * t4 * 
     #t187 / 0.8D1 + t280 * t279 - t282 * 0.3141592653589793D1 * t4 * t1
     #87 / 0.8D1 + t287 * t279 + t289 * 0.3141592653589793D1 * t164 * t1
     #59 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t12 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t25 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.1D1 - x2, 
     #0.10D1, x4)
      t31 = 0.1D1 / x1
      t38 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t39 = z ** 2
      t42 = Sin(t7)
      t43 = t42 ** 2
      t46 = log(0.4D1 / t39 / z * t43)
      t54 = t5 * t6 * (-0.1D1 - z / (-z - x3 + 0.2D1 * t8 * t12)) / x3 /
     # 0.32D2 - t5 * (t6 - t25) / x2 / 0.16D2 - t5 * t6 * t31 / 0.16D2 -
     # (-0.180D3 * t6 * 0.3141592653589793D1 * lh + 0.90D2 * (t38 - t46 
     #* t6) * 0.3141592653589793D1) * t4 / 0.2880D4
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t54)
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t54)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t54)
      t61 = -0.1D1 + x1
      t62 = t2 * t61
      t65 = 0.1D1 / (-z - x1 + x1 * z)
      t67 = t2 * x1 * t65
      t68 = t1 ** 2
      t72 = s * t68 * x1 * t61 * t65
      t73 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1,
     # x4)
      t76 = t5 * t73 * t31 / 0.16D2
      t77 = FJET(XB1, XB2, s, 0.0D0, -t62, -t67, 0.0D0, t72, t76)
      t80 = t4 * t73 * t31
      t83 = FJET(XB1, XB2, s, 0.0D0, -t67, -t62, 0.0D0, t72, t76)
      t87 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t54)
      t89 = FJET(XB1, XB2, s, -t62, 0.0D0, 0.0D0, -t67, t72, t76)
      t93 = FJET(XB1, XB2, s, -t67, 0.0D0, 0.0D0, -t62, t72, t76)
      rrgg2gght8s3em2 = t55 * t54 + t57 * t54 + t59 * t54 + t77 * 0.3141
     #592653589793D1 * t80 / 0.16D2 + t83 * 0.3141592653589793D1 * t80 /
     # 0.16D2 + t87 * t54 + t89 * 0.3141592653589793D1 * t80 / 0.16D2 + 
     #t93 * 0.3141592653589793D1 * t80 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s3em3 = -t9 * 0.3141592653589793D1 * t11 / 0.32D2 - t13 
     #* 0.3141592653589793D1 * t11 / 0.32D2 - t16 * 0.3141592653589793D1
     # * t11 / 0.32D2 - t19 * 0.3141592653589793D1 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2gght8s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght8s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght8s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = s ** 2
      t10 = 0.1D1 / t9
      t11 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = x3 * t14
      t16 = x4 * 0.3141592653589793D1
      t17 = Sin(t16)
      t18 = t17 ** 2
      t21 = log(0.4D1 * t15 * t18)
      t22 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t27 = 0.3141592653589793D1 * t10
      t28 = t21 ** 2
      t31 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t35 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t44 = 0.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * t3 * lh
      t45 = 0.3141592653589793D1 * t44
      t46 = t10 * t22
      t47 = t45 * t46
      t48 = 0.3141592653589793D1 * lh
      t57 = 0.1D1 / x3
      t60 = t14 * t18
      t62 = log(0.4D1 * t60)
      t63 = t62 ** 2
      t66 = t63 * t62
      t85 = t5 ** 2
      t86 = t3 ** 2
      t98 = rrgg2ggh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t99 = t63 ** 2
      t108 = x1 ** 2
      t109 = x3 * t108
      t112 = log(0.4D1 * t109 * t60)
      t114 = t112 ** 2
      t128 = 0.1D1 / x1
      t131 = t108 * t18
      t132 = t131 * t14
      t134 = log(0.4D1 * t132)
      t139 = t134 ** 2
      t159 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t160 = t109 * x2
      t161 = -0.1D1 + x2
      t162 = t161 ** 2
      t163 = t60 * t162
      t166 = log(0.4D1 * t160 * t163)
      t167 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t169 = x2 * x3
      t172 = log(0.4D1 * t169 * t132)
      t177 = t22 - t167
      t178 = t10 * t177
      t183 = 0.1D1 / x2
      t184 = t128 * t183
      t187 = t108 * x2
      t190 = log(0.4D1 * t187 * t60)
      t192 = t190 ** 2
      t197 = log(0.4D1 * t187 * t163)
      t199 = t197 ** 2
      t202 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t221 = log(0.4D1 * t169 * t60)
      t223 = t221 ** 2
      t228 = log(0.4D1 * t169 * t163)
      t230 = t228 ** 2
      t247 = x2 * t14
      t250 = log(0.4D1 * t247 * t18)
      t255 = log(0.4D1 * t247 * t18 * t162)
      t260 = t255 ** 2
      t263 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t268 = t250 ** 2
      t292 = (t8 * t10 * (-t11 + t21 * t22) + 0.90D2 * t27 * (-t28 * t11
     # / 0.2D1 - t31 + t28 * t21 * t22 / 0.6D1 + t21 * t35) - t47 - 0.18
     #0D3 * t48 * t10 * (t21 * t11 - t28 * t22 / 0.2D1 - t35)) * t57 / 0
     #.1440D4 - (-0.180D3 * (t63 * t11 / 0.2D1 + t31 - t66 * t22 / 0.6D1
     # - t62 * t35) * 0.3141592653589793D1 * lh + (-t62 * t11 + t63 * t2
     #2 / 0.2D1 + t35) * 0.3141592653589793D1 * t7 + (t11 - t62 * t22) *
     # 0.3141592653589793D1 * t44 + t22 * 0.3141592653589793D1 * (t85 + 
     #0.60D2 * t86 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5) + 0.9
     #0D2 * (-t66 * t11 / 0.6D1 + t63 * t35 / 0.2D1 - t62 * t31 + t98 + 
     #t99 * t22 / 0.24D2) * 0.3141592653589793D1) * t10 / 0.1440D4 - (0.
     #90D2 * t27 * (-t112 * t11 + t114 * t22 / 0.2D1 + t35) - 0.180D3 * 
     #t48 * t10 * (t11 - t112 * t22) + t8 * t46) * t57 * t128 / 0.720D3 
     #+ (t8 * t10 * (-t11 + t134 * t22) + 0.90D2 * t27 * (-t139 * t11 / 
     #0.2D1 - t31 + t139 * t134 * t22 / 0.6D1 + t134 * t35) - t47 - 0.18
     #0D3 * t48 * t10 * (t134 * t11 - t139 * t22 / 0.2D1 - t35)) * t128 
     #/ 0.720D3 - (0.90D2 * t27 * (-t159 + t166 * t167 + t11 - t172 * t2
     #2) - 0.180D3 * t48 * t178) * t57 * t184 / 0.720D3 + (0.90D2 * t27 
     #* (t190 * t11 - t192 * t22 / 0.2D1 - t35 - t197 * t159 + t199 * t1
     #67 / 0.2D1 + t202) - 0.180D3 * t48 * t10 * (-t11 + t190 * t22 + t1
     #59 - t197 * t167) - t8 * t10 * t177) * t128 * t183 / 0.720D3 - (0.
     #90D2 * t27 * (-t221 * t11 + t223 * t22 / 0.2D1 + t35 + t228 * t159
     # - t230 * t167 / 0.2D1 - t202) - 0.180D3 * t48 * t10 * (t11 - t221
     # * t22 - t159 + t228 * t167) + t8 * t178) * t57 * t183 / 0.1440D4 
     #- (t8 * t10 * (t11 - t250 * t22 - t159 + t255 * t167) + 0.90D2 * t
     #27 * (-t260 * t159 / 0.2D1 - t263 + t260 * t255 * t167 / 0.6D1 + t
     #255 * t202 + t268 * t11 / 0.2D1 + t31 - t268 * t250 * t22 / 0.6D1 
     #- t250 * t35) + t45 * t178 - 0.180D3 * t48 * t10 * (-t250 * t11 + 
     #t268 * t22 / 0.2D1 + t35 + t255 * t159 - t260 * t167 / 0.2D1 - t20
     #2)) * t183 / 0.1440D4
      t293 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t292)
      t296 = -0.1D1 + t169
      t297 = 0.1D1 / t296
      t298 = x3 * t161 * t297
      t299 = t2 * t298
      t300 = -0.1D1 + x3
      t301 = t300 * t297
      t302 = t2 * t301
      t303 = t296 ** 2
      t304 = 0.1D1 / t303
      t307 = x2 * t300
      t308 = x3 * t162 * t307
      t311 = log(-0.4D1 * t60 * t304 * t308)
      t312 = t311 * z
      t313 = cos(t16)
      t314 = x3 * z
      t316 = Sqrt(-t314 * t307)
      t320 = 0.1D1 / (-z - t169 + 0.2D1 * t313 * t316)
      t321 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t301, x
     #4)
      t324 = z * t320
      t325 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t301, x
     #4)
      t327 = t311 ** 2
      t329 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t301, x
     #4)
      t330 = t320 * t329
      t337 = t324 * t321
      t343 = t324 * t329
      t353 = log(-0.4D1 * t60 * t304 * t108 * t308)
      t359 = t48 * t10
      t366 = -(0.90D2 * t27 * (t312 * t320 * t321 - t324 * t325 - t327 *
     # z * t330 / 0.2D1) - 0.180D3 * t48 * t10 * (t312 * t330 - t337) - 
     #t8 * t10 * t343) * t57 * t183 / 0.1440D4 - (0.90D2 * t27 * (t353 *
     # z * t330 - t337) + 0.180D3 * t359 * t343) * t57 * t184 / 0.720D3
      t367 = FJET(XB1, XB2, s, t299, 0.0D0, t302, 0.0D0, 0.0D0, t366)
      t369 = t2 * x1
      t370 = -0.1D1 + x1
      t371 = t2 * t370
      t372 = t109 * t18
      t373 = 0.1D1 / t12
      t374 = t370 ** 2
      t375 = t373 * t374
      t376 = x1 * z
      t377 = -z - x1 + t376
      t378 = 0.1D1 / t377
      t379 = t375 * t378
      t382 = log(-0.4D1 * t372 * t379)
      t383 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t385 = t382 ** 2
      t386 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t389 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t398 = t10 * t386
      t399 = t8 * t398
      t402 = (0.90D2 * t27 * (t382 * t383 - t385 * t386 / 0.2D1 - t389) 
     #- 0.180D3 * t48 * t10 * (-t383 + t382 * t386) - t399) * t57 * t128
      t405 = log(-0.4D1 * t131 * t379)
      t407 = -t383 + t405 * t386
      t410 = t405 ** 2
      t413 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t418 = -t410 * t383 / 0.2D1 - t413 + t410 * t405 * t386 / 0.6D1 + 
     #t405 * t389
      t421 = t45 * t398
      t425 = t405 * t383 - t410 * t386 / 0.2D1 - t389
      t431 = t18 * t373
      t432 = t374 * t378
      t433 = t431 * t432
      t436 = log(-0.4D1 * t160 * t433)
      t445 = (0.90D2 * t27 * (-t383 + t436 * t386) + 0.180D3 * t48 * t39
     #8) * t57 * t184
      t446 = t187 * t18
      t449 = log(-0.4D1 * t446 * t379)
      t451 = t449 ** 2
      t464 = (0.90D2 * t27 * (-t449 * t383 + t451 * t386 / 0.2D1 + t389)
     # - 0.180D3 * t48 * t10 * (t383 - t449 * t386) + t399) * t128 * t18
     #3
      t466 = -t402 / 0.720D3 + (-t8 * t10 * t407 - 0.90D2 * t27 * t418 +
     # t421 + 0.180D3 * t48 * t10 * t425) * t128 / 0.720D3 - t445 / 0.72
     #0D3 + t464 / 0.720D3
      t467 = FJET(XB1, XB2, s, t369, -t371, 0.0D0, 0.0D0, 0.0D0, t466)
      t470 = t1 * x1
      t471 = t161 * s * t470
      t472 = x1 * x2
      t474 = t2 * t472 * t378
      t475 = t1 ** 2
      t480 = s * t475 * x2 * x1 * t370 * t378
      t481 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t484 = t375 * t378 * t162
      t487 = log(-0.4D1 * t169 * t131 * t484)
      t488 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t493 = t10 * t488
      t498 = (0.90D2 * t27 * (t481 - t487 * t488) - 0.180D3 * t48 * t493
     #) * t57 * t184
      t501 = log(-0.4D1 * t446 * t484)
      t503 = t501 ** 2
      t506 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t507 = -t501 * t481 + t503 * t488 / 0.2D1 + t506
      t511 = t481 - t501 * t488
      t515 = t8 * t493
      t520 = -t498 / 0.720D3 + (-0.90D2 * t27 * t507 + 0.180D3 * t48 * t
     #10 * t511 - t515) * t128 * t183 / 0.720D3
      t521 = FJET(XB1, XB2, s, -t371, -t471, 0.0D0, -t474, t480, t520)
      t523 = t2 * t300
      t524 = t2 * x3
      t525 = -t300
      t526 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t525
     #, x4)
      t530 = log(-0.4D1 * t15 * t18 * t300)
      t531 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t525
     #, x4)
      t536 = t530 ** 2
      t539 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t525
     #, x4)
      t543 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t525
     #, x4)
      t548 = t10 * t531
      t559 = (t8 * t10 * (t526 - t530 * t531) + 0.90D2 * t27 * (t536 * t
     #526 / 0.2D1 + t539 - t536 * t530 * t531 / 0.6D1 - t530 * t543) + t
     #45 * t548 - 0.180D3 * t48 * t10 * (-t530 * t526 + t536 * t531 / 0.
     #2D1 + t543)) * t57 / 0.1440D4
      t560 = t60 * t300
      t563 = log(-0.4D1 * t109 * t560)
      t565 = t563 ** 2
      t576 = t8 * t548
      t580 = (0.90D2 * t27 * (t563 * t526 - t565 * t531 / 0.2D1 - t543) 
     #- 0.180D3 * t48 * t10 * (-t526 + t563 * t531) - t576) * t57 * t128
     # / 0.720D3
      t583 = log(-0.4D1 * t160 * t560)
      t593 = (0.90D2 * t27 * (-t526 + t583 * t531) + 0.180D3 * t48 * t54
     #8) * t57 * t184 / 0.720D3
      t596 = log(-0.4D1 * t169 * t560)
      t598 = t596 ** 2
      t601 = -t596 * t526 + t598 * t531 / 0.2D1 + t543
      t605 = t526 - t596 * t531
      t613 = t559 - t580 - t593 - (-0.90D2 * t27 * t601 + 0.180D3 * t48 
     #* t10 * t605 - t576) * t57 * t183 / 0.1440D4
      t614 = FJET(XB1, XB2, s, -t523, 0.0D0, t524, 0.0D0, 0.0D0, t613)
      t616 = FJET(XB1, XB2, s, 0.0D0, t299, 0.0D0, t302, 0.0D0, t366)
      t619 = t1 * t370
      t620 = x3 * s * t619
      t621 = x3 * x1
      t622 = t2 * t621
      t623 = t300 * s
      t624 = t623 * t619
      t625 = t623 * t470
      t626 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t525, x
     #4)
      t632 = log(0.4D1 * t372 * t373 * t378 * t374 * t300)
      t633 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t525, x
     #4)
      t635 = t632 ** 2
      t636 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t525, x
     #4)
      t647 = t10 * t636
      t655 = log(0.4D1 * t433 * t109 * t307)
      t666 = -(0.90D2 * t27 * (t626 - t632 * t633 + t635 * t636 / 0.2D1)
     # - 0.180D3 * t48 * t10 * (t633 - t632 * t636) + t8 * t647) * t57 *
     # t128 / 0.720D3 - (0.90D2 * t27 * (-t655 * t636 + t633) - 0.180D3 
     #* t48 * t647) * t57 * t184 / 0.720D3
      t667 = FJET(XB1, XB2, s, -t620, t622, t624, -t625, 0.0D0, t666)
      t669 = FJET(XB1, XB2, s, -t474, 0.0D0, -t471, -t371, t480, t520)
      t682 = t559 - t580 - t593 - (-0.90D2 * t27 * t601 + 0.180D3 * t48 
     #* t10 * t605 - t576) * t57 * t183 / 0.1440D4
      t683 = FJET(XB1, XB2, s, 0.0D0, t524, 0.0D0, -t523, 0.0D0, t682)
      t685 = FJET(XB1, XB2, s, -t371, t369, 0.0D0, 0.0D0, 0.0D0, t466)
      t687 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t292)
      t700 = -t498 / 0.720D3 + (-0.90D2 * t27 * t507 + 0.180D3 * t48 * t
     #10 * t511 - t515) * t128 * t183 / 0.720D3
      t701 = FJET(XB1, XB2, s, 0.0D0, -t474, -t371, -t471, t480, t700)
      t716 = -t402 / 0.720D3 + (-t8 * t10 * t407 - 0.90D2 * t27 * t418 +
     # t421 + 0.180D3 * t48 * t10 * t425) * t128 / 0.720D3 - t445 / 0.72
     #0D3 + t464 / 0.720D3
      t717 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t369, -t371, 0.0D0, t716)
      t719 = t621 * z
      t722 = Sqrt(x3 * t377 * t307)
      t723 = t313 * t722
      t724 = 0.2D1 * t723
      t729 = t369 * t161 * (-t169 - z + t314 - x1 + t621 + t376 - t719 +
     # t724) * t378 * t297
      t731 = t623 * t619 * t297
      t732 = t169 * z
      t734 = t621 * x2
      t736 = x2 * z
      t739 = x2 ** 2
      t741 = t376 * t739 * x3
      t742 = t723 * x2
      t747 = x1 * t739 * x3
      t748 = t719 + 0.2D1 * t732 + 0.2D1 * t734 - 0.2D1 * t621 * t736 + 
     #t741 + t169 + 0.2D1 * t742 - t724 - t314 - t621 - x2 - z * t739 * 
     #x3 - t747
      t751 = t369 * t748 * t378 * t297
      t752 = t371 * t298
      t755 = t472 * z
      t756 = t108 * t12
      t767 = -t732 - t734 + 0.2D1 * t376 * t742 + t755 - t756 * t169 + 0
     #.2D1 * t109 * t736 + t621 * x2 * t12 - t741 - 0.2D1 * t187 * z + t
     #756 * x2 - t472 * t12 - 0.2D1 * t376
      t771 = x1 * t313
      t783 = 0.2D1 * z * t313 * t722 + 0.2D1 * t771 * t722 - 0.2D1 * t77
     #1 * t722 * x2 - 0.2D1 * t376 * t723 - t108 - t12 + 0.2D1 * x1 * t1
     #2 + 0.2D1 * t108 * z - t756 - t160 + t747 + t187
      t785 = 0.1D1 / (t767 + t783)
      t786 = z + x1 - t376 - t472 + t755
      t787 = t785 * t786
      t788 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t301, x4)
      t798 = log(0.4D1 * t431 * t432 * t304 * t109 * t162 * x2 * t300)
      t801 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t301, x4)
      t811 = 0.90D2 * t27 * (-t787 * t377 * t788 + t798 * t785 * t786 * 
     #t377 * t801) + 0.180D3 * t359 * t787 * t377 * t801
      t814 = t811 * t57 * t184 / 0.720D3
      t815 = FJET(XB1, XB2, s, t729, -t731, -t751, -t752, t480, -t814)
      t818 = t57 * t128 * t183
      t821 = t293 * t292 + t367 * t366 + t467 * t466 + t521 * t520 + t61
     #4 * t613 + t616 * t366 + t667 * t666 + t669 * t520 + t683 * t682 +
     # t685 * t466 + t687 * t292 + t701 * t700 + t717 * t716 - t815 * t8
     #11 * t818 / 0.720D3
      t822 = FJET(XB1, XB2, s, 0.0D0, -t523, 0.0D0, t524, 0.0D0, t682)
      t824 = FJET(XB1, XB2, s, -t731, t729, -t752, -t751, t480, -t814)
      t828 = FJET(XB1, XB2, s, -t752, -t751, -t731, t729, t480, -t814)
      t832 = FJET(XB1, XB2, s, t302, 0.0D0, t299, 0.0D0, 0.0D0, t366)
      t834 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t292)
      t836 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t371, t369, 0.0D0, t716)
      t838 = FJET(XB1, XB2, s, -t751, -t752, t729, -t731, t480, -t814)
      t842 = FJET(XB1, XB2, s, t622, -t620, -t625, t624, 0.0D0, t666)
      t844 = FJET(XB1, XB2, s, -t471, -t371, -t474, 0.0D0, t480, t520)
      t846 = FJET(XB1, XB2, s, 0.0D0, t302, 0.0D0, t299, 0.0D0, t366)
      t848 = FJET(XB1, XB2, s, t624, -t625, -t620, t622, 0.0D0, t666)
      t850 = FJET(XB1, XB2, s, t524, 0.0D0, -t523, 0.0D0, 0.0D0, t682)
      t852 = FJET(XB1, XB2, s, -t625, t624, t622, -t620, 0.0D0, t666)
      t854 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t292)
      t856 = t822 * t682 - t824 * t811 * t818 / 0.720D3 - t828 * t811 * 
     #t818 / 0.720D3 + t832 * t366 + t834 * t292 + t836 * t716 - t838 * 
     #t811 * t818 / 0.720D3 + t842 * t666 + t844 * t520 + t846 * t366 + 
     #t848 * t666 + t850 * t682 + t852 * t666 + t854 * t292
      rrgg2gght8s4e1 = t821 + t856

      end function



      doubleprecision function rrgg2gght8s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = -0.1D1 + x2
      t7 = t1 * x1
      t8 = t5 * s * t7
      t9 = x1 * x2
      t10 = x1 * z
      t11 = -z - x1 + t10
      t12 = 0.1D1 / t11
      t14 = t2 * t9 * t12
      t15 = t1 ** 2
      t20 = s * t15 * x2 * x1 * t3 * t12
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = 0.3141592653589793D1 * t22
      t24 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4)
      t26 = 0.1D1 / x3
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x2
      t30 = t26 * t27 * t29
      t32 = t23 * t24 * t30 / 0.8D1
      t33 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4)
      t34 = x1 ** 2
      t35 = x2 * t34
      t36 = x4 * 0.3141592653589793D1
      t37 = Sin(t36)
      t38 = t37 ** 2
      t39 = t35 * t38
      t40 = z ** 2
      t41 = 0.1D1 / t40
      t42 = t41 * t12
      t43 = t3 ** 2
      t44 = t5 ** 2
      t49 = log(-0.4D1 * t39 * t42 * t43 * t44)
      t51 = t33 - t49 * t24
      t54 = 0.3141592653589793D1 * lh
      t57 = 0.180D3 * t54 * t22 * t24
      t62 = -t32 + (-0.90D2 * t23 * t51 + t57) * t27 * t29 / 0.720D3
      t63 = FJET(XB1, XB2, s, -t4, -t8, 0.0D0, -t14, t20, t62)
      t65 = FJET(XB1, XB2, s, -t8, -t4, -t14, 0.0D0, t20, t62)
      t67 = FJET(XB1, XB2, s, -t14, 0.0D0, -t8, -t4, t20, t62)
      t69 = -0.1D1 + x3
      t70 = t2 * t69
      t71 = t2 * x3
      t73 = 0.1D1 / t40 / z
      t74 = x3 * t73
      t78 = log(-0.4D1 * t74 * t38 * t69)
      t79 = -t69
      t80 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t79, 
     #x4)
      t82 = t78 ** 2
      t83 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t79, 
     #x4)
      t86 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t79, 
     #x4)
      t95 = lh ** 2
      t97 = 0.3141592653589793D1 ** 2
      t99 = 0.180D3 * t95 - 0.30D2 * t97
      t100 = 0.3141592653589793D1 * t99
      t101 = t22 * t83
      t105 = (0.90D2 * t23 * (-t78 * t80 + t82 * t83 / 0.2D1 + t86) - 0.
     #180D3 * t54 * t22 * (t80 - t78 * t83) + t100 * t101) * t26 / 0.144
     #0D4
      t106 = x2 * x3
      t107 = t73 * t38
      t108 = t107 * t69
      t111 = log(-0.4D1 * t106 * t108)
      t113 = t80 - t111 * t83
      t117 = 0.180D3 * t54 * t101
      t122 = x3 * t34
      t125 = log(-0.4D1 * t122 * t108)
      t133 = (0.90D2 * t23 * (-t80 + t125 * t83) + t117) * t26 * t27 / 0
     #.720D3
      t136 = t23 * t83 * t30 / 0.8D1
      t137 = t105 - (-0.90D2 * t23 * t113 + t117) * t26 * t29 / 0.1440D4
     # - t133 + t136
      t138 = FJET(XB1, XB2, s, -t70, 0.0D0, t71, 0.0D0, 0.0D0, t137)
      t147 = t105 - (-0.90D2 * t23 * t113 + t117) * t26 * t29 / 0.1440D4
     # - t133 + t136
      t148 = FJET(XB1, XB2, s, 0.0D0, -t70, 0.0D0, t71, 0.0D0, t147)
      t152 = log(0.4D1 * t74 * t38)
      t153 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t155 = t152 ** 2
      t156 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t159 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t168 = t22 * t156
      t169 = t100 * t168
      t174 = log(0.4D1 * t107)
      t176 = t174 ** 2
      t192 = rrgg2ggh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t209 = log(0.4D1 * t106 * t107)
      t211 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t212 = t107 * t44
      t215 = log(0.4D1 * t106 * t212)
      t216 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t221 = t156 - t216
      t222 = t221 * t22
      t229 = x2 * t73
      t232 = log(0.4D1 * t229 * t38)
      t234 = t232 ** 2
      t240 = log(0.4D1 * t229 * t38 * t44)
      t242 = t240 ** 2
      t245 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t261 = log(0.4D1 * t122 * t107)
      t277 = log(0.4D1 * t35 * t107)
      t281 = log(0.4D1 * t35 * t212)
      t294 = t34 * t38
      t297 = log(0.4D1 * t294 * t73)
      t299 = t297 ** 2
      t313 = (0.90D2 * t23 * (t152 * t153 - t155 * t156 / 0.2D1 - t159) 
     #- 0.180D3 * t54 * t22 * (-t153 + t152 * t156) - t169) * t26 / 0.14
     #40D4 - (-0.180D3 * (-t174 * t153 + t176 * t156 / 0.2D1 + t159) * 0
     #.3141592653589793D1 * lh + t156 * 0.3141592653589793D1 * (0.60D2 *
     # lh * t97 - 0.2884936567583026D3 - 0.120D3 * t95 * lh) + 0.90D2 * 
     #(t176 * t153 / 0.2D1 + t192 - t176 * t174 * t156 / 0.6D1 - t174 * 
     #t159) * 0.3141592653589793D1 + (t153 - t174 * t156) * 0.3141592653
     #589793D1 * t99) * t22 / 0.1440D4 - (0.90D2 * t23 * (t153 - t209 * 
     #t156 - t211 + t215 * t216) - 0.180D3 * t54 * t222) * t26 * t29 / 0
     #.1440D4 - (0.90D2 * t23 * (-t232 * t153 + t234 * t156 / 0.2D1 + t1
     #59 + t240 * t211 - t242 * t216 / 0.2D1 - t245) - 0.180D3 * t54 * t
     #22 * (t153 - t232 * t156 - t211 + t240 * t216) + t100 * t222) * t2
     #9 / 0.1440D4 - (0.90D2 * t23 * (t153 - t261 * t156) - 0.180D3 * t5
     #4 * t168) * t26 * t27 / 0.720D3 - t23 * t221 * t30 / 0.8D1 + (0.90
     #D2 * t23 * (-t153 + t277 * t156 + t211 - t281 * t216) + 0.180D3 * 
     #t54 * t22 * t221) * t27 * t29 / 0.720D3 + (0.90D2 * t23 * (t297 * 
     #t153 - t299 * t156 / 0.2D1 - t159) - 0.180D3 * t54 * t22 * (-t153 
     #+ t297 * t156) - t169) * t27 / 0.720D3
      t314 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t313)
      t317 = -0.1D1 + t106
      t318 = 0.1D1 / t317
      t319 = x3 * t5 * t318
      t320 = t2 * t319
      t321 = t69 * t318
      t322 = t2 * t321
      t323 = cos(t36)
      t324 = x3 * z
      t325 = x2 * t69
      t327 = Sqrt(-t324 * t325)
      t331 = 0.1D1 / (-z - t106 + 0.2D1 * t323 * t327)
      t332 = z * t331
      t334 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t321, x
     #4)
      t340 = t317 ** 2
      t347 = log(-0.4D1 * t107 / t340 * x3 * t44 * t325)
      t351 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t321, x
     #4)
      t364 = t23 * t332 * t334 * t26 * t27 * t29 / 0.8D1 - (0.90D2 * t23
     # * (t347 * z * t331 * t334 - t332 * t351) + 0.180D3 * t54 * t22 * 
     #t332 * t334) * t26 * t29 / 0.1440D4
      t365 = FJET(XB1, XB2, s, t320, 0.0D0, t322, 0.0D0, 0.0D0, t364)
      t367 = t2 * x1
      t368 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t369 = t122 * t38
      t371 = t41 * t43 * t12
      t374 = log(-0.4D1 * t369 * t371)
      t375 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t380 = t22 * t375
      t382 = 0.180D3 * t54 * t380
      t386 = (0.90D2 * t23 * (-t368 + t374 * t375) + t382) * t26 * t27 /
     # 0.720D3
      t389 = t23 * t375 * t30 / 0.8D1
      t392 = log(-0.4D1 * t39 * t371)
      t400 = (0.90D2 * t23 * (t368 - t392 * t375) - t382) * t27 * t29 / 
     #0.720D3
      t403 = log(-0.4D1 * t294 * t371)
      t405 = t403 ** 2
      t408 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t409 = t403 * t368 - t405 * t375 / 0.2D1 - t408
      t413 = -t368 + t403 * t375
      t417 = t100 * t380
      t421 = -t386 + t389 + t400 + (-0.90D2 * t23 * t409 + 0.180D3 * t54
     # * t22 * t413 + t417) * t27 / 0.720D3
      t422 = FJET(XB1, XB2, s, -t4, t367, 0.0D0, 0.0D0, 0.0D0, t421)
      t434 = -t386 + t389 + t400 + (-0.90D2 * t23 * t409 + 0.180D3 * t54
     # * t22 * t413 + t417) * t27 / 0.720D3
      t435 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t367, 0.0D0, t434)
      t437 = FJET(XB1, XB2, s, 0.0D0, t320, 0.0D0, t322, 0.0D0, t364)
      t439 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t367, -t4, 0.0D0, t434)
      t441 = t69 * s
      t442 = t1 * t3
      t443 = t441 * t442
      t444 = t441 * t7
      t446 = x3 * s * t442
      t447 = x3 * x1
      t448 = t2 * t447
      t449 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t79, x4
     #)
      t454 = log(0.4D1 * t369 * t42 * t43 * t69)
      t455 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t79, x4
     #)
      t470 = -(0.90D2 * t23 * (t449 - t454 * t455) - 0.180D3 * t54 * t22
     # * t455) * t26 * t27 / 0.720D3 - t23 * t455 * t30 / 0.8D1
      t471 = FJET(XB1, XB2, s, t443, -t444, -t446, t448, 0.0D0, t470)
      t473 = FJET(XB1, XB2, s, t448, -t446, -t444, t443, 0.0D0, t470)
      t475 = t447 * z
      t476 = t106 * z
      t478 = t447 * x2
      t480 = x2 * z
      t483 = x2 ** 2
      t485 = t10 * t483 * x3
      t488 = Sqrt(x3 * t11 * t325)
      t489 = t323 * t488
      t490 = t489 * x2
      t492 = 0.2D1 * t489
      t496 = x1 * t483 * x3
      t497 = t475 + 0.2D1 * t476 + 0.2D1 * t478 - 0.2D1 * t447 * t480 + 
     #t485 + t106 + 0.2D1 * t490 - t492 - t324 - t447 - x2 - z * t483 * 
     #x3 - t496
      t500 = t367 * t497 * t12 * t318
      t501 = t4 * t319
      t506 = t367 * t5 * (-t106 - z + t324 - x1 + t447 + t10 - t475 + t4
     #92) * t12 * t318
      t508 = t441 * t442 * t318
      t511 = t9 * z
      t512 = t34 * t40
      t523 = -t476 - t478 + 0.2D1 * t10 * t490 + t511 - t512 * t106 + 0.
     #2D1 * t122 * t480 + t447 * x2 * t40 - t485 - 0.2D1 * t35 * z + t51
     #2 * x2 - t9 * t40 - 0.2D1 * t10
      t527 = x1 * t323
      t540 = 0.2D1 * z * t323 * t488 + 0.2D1 * t527 * t488 - 0.2D1 * t52
     #7 * t488 * x2 - 0.2D1 * t10 * t489 - t34 - t40 + 0.2D1 * x1 * t40 
     #+ 0.2D1 * t34 * z - t512 - t122 * x2 + t496 + t35
      t542 = 0.1D1 / (t523 + t540)
      t543 = z + x1 - t10 - t9 + t511
      t546 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t321, x4)
      t548 = t11 * t546 * t30
      t550 = t23 * t542 * t543 * t548 / 0.8D1
      t551 = FJET(XB1, XB2, s, -t500, -t501, t506, -t508, t20, t550)
      t554 = t22 * t542 * t543
      t558 = t63 * t62 + t65 * t62 + t67 * t62 + t138 * t137 + t148 * t1
     #47 + t314 * t313 + t365 * t364 + t422 * t421 + t435 * t434 + t437 
     #* t364 + t439 * t434 + t471 * t470 + t473 * t470 + t551 * 0.314159
     #2653589793D1 * t554 * t548 / 0.8D1
      t559 = FJET(XB1, XB2, s, -t508, t506, -t501, -t500, t20, t550)
      t564 = FJET(XB1, XB2, s, t506, -t508, -t500, -t501, t20, t550)
      t569 = FJET(XB1, XB2, s, -t446, t448, t443, -t444, 0.0D0, t470)
      t571 = FJET(XB1, XB2, s, -t444, t443, t448, -t446, 0.0D0, t470)
      t573 = FJET(XB1, XB2, s, 0.0D0, t71, 0.0D0, -t70, 0.0D0, t147)
      t575 = FJET(XB1, XB2, s, t322, 0.0D0, t320, 0.0D0, 0.0D0, t364)
      t577 = FJET(XB1, XB2, s, 0.0D0, t322, 0.0D0, t320, 0.0D0, t364)
      t586 = -t32 + (-0.90D2 * t23 * t51 + t57) * t27 * t29 / 0.720D3
      t587 = FJET(XB1, XB2, s, 0.0D0, -t14, -t4, -t8, t20, t586)
      t589 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t313)
      t591 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t313)
      t593 = FJET(XB1, XB2, s, t367, -t4, 0.0D0, 0.0D0, 0.0D0, t421)
      t595 = FJET(XB1, XB2, s, -t501, -t500, -t508, t506, t20, t550)
      t600 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t313)
      t602 = FJET(XB1, XB2, s, t71, 0.0D0, -t70, 0.0D0, 0.0D0, t147)
      t604 = t559 * 0.3141592653589793D1 * t554 * t548 / 0.8D1 + t564 * 
     #0.3141592653589793D1 * t554 * t548 / 0.8D1 + t569 * t470 + t571 * 
     #t470 + t573 * t147 + t575 * t364 + t577 * t364 + t587 * t586 + t58
     #9 * t313 + t591 * t313 + t593 * t421 + t595 * 0.3141592653589793D1
     # * t554 * t548 / 0.8D1 + t600 * t313 + t602 * t147
      rrgg2gght8s4e0 = t558 + t604

      end function



      doubleprecision function rrgg2gght8s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t22 = 0.3141592653589793D1 * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t32 = log(0.4D1 * t9 * t13)
      t39 = t32 ** 2
      t42 = rrgg2ggh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t47 = lh ** 2
      t49 = 0.3141592653589793D1 ** 2
      t56 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t57 = t17 - t56
      t59 = 0.1D1 / x2
      t63 = x2 * t9
      t66 = log(0.4D1 * t63 * t13)
      t68 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t69 = -0.1D1 + x2
      t70 = t69 ** 2
      t74 = log(0.4D1 * t63 * t13 * t70)
      t86 = 0.1D1 / x1
      t91 = x1 ** 2
      t92 = t91 * t13
      t95 = log(0.4D1 * t92 * t9)
      t107 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 - 
     #(-0.180D3 * (t6 - t32 * t17) * 0.3141592653589793D1 * lh + 0.90D2 
     #* (-t32 * t6 + t39 * t17 / 0.2D1 + t42) * 0.3141592653589793D1 + t
     #17 * 0.3141592653589793D1 * (0.180D3 * t47 - 0.30D2 * t49)) * t4 /
     # 0.1440D4 - t5 * t57 * t27 * t59 / 0.16D2 - (0.90D2 * t5 * (t6 - t
     #66 * t17 - t68 + t74 * t56) - 0.180D3 * t22 * t4 * t57) * t59 / 0.
     #1440D4 - t5 * t57 * t86 * t59 / 0.8D1 + (0.90D2 * t5 * (-t6 + t95 
     #* t17) + t25) * t86 / 0.720D3 - t5 * t17 * t27 * t86 / 0.8D1
      t108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t107)
      t110 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t107)
      t112 = t2 * x1
      t113 = -0.1D1 + x1
      t114 = t2 * t113
      t115 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t119 = t5 * t115 * t86 * t59 / 0.8D1
      t120 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t124 = 0.1D1 / (-z - x1 + x1 * z)
      t126 = t113 ** 2
      t130 = log(-0.4D1 * t92 / t7 * t124 * t126)
      t132 = t120 - t130 * t115
      t137 = 0.180D3 * t22 * t4 * t115
      t144 = t5 * t115 * t27 * t86 / 0.8D1
      t145 = t119 + (0.90D2 * t5 * t132 - t137) * t86 / 0.720D3 + t144
      t146 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t112, -t114, 0.0D0, t145)
      t148 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t114, t112, 0.0D0, t145)
      t150 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t107)
      t152 = t2 * x3
      t153 = -0.1D1 + x3
      t154 = t2 * t153
      t155 = -t153
      t156 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t155
     #, x4)
      t157 = t156 * t27
      t161 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t155
     #, x4)
      t165 = log(-0.4D1 * t10 * t13 * t153)
      t179 = t5 * t157 * t59 / 0.16D2 + (0.90D2 * t5 * (t161 - t165 * t1
     #56) - 0.180D3 * t22 * t4 * t156) * t27 / 0.1440D4 + t5 * t157 * t8
     #6 / 0.8D1
      t180 = FJET(XB1, XB2, s, 0.0D0, t152, 0.0D0, -t154, 0.0D0, t179)
      t182 = x2 * x3
      t184 = 0.1D1 / (-0.1D1 + t182)
      t185 = t153 * t184
      t186 = t2 * t185
      t189 = t2 * x3 * t69 * t184
      t191 = cos(t11)
      t195 = Sqrt(-x3 * z * x2 * t153)
      t200 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t185, x
     #4)
      t203 = 0.1D1 / (-z - t182 + 0.2D1 * t191 * t195) * t200 * t27 * t5
     #9
      t205 = t5 * z * t203 / 0.16D2
      t206 = FJET(XB1, XB2, s, 0.0D0, t186, 0.0D0, t189, 0.0D0, t205)
      t208 = t4 * z
      t212 = FJET(XB1, XB2, s, 0.0D0, t189, 0.0D0, t186, 0.0D0, t205)
      t217 = FJET(XB1, XB2, s, 0.0D0, -t154, 0.0D0, t152, 0.0D0, t179)
      t221 = t2 * x1 * x2 * t124
      t223 = t1 * x1
      t224 = t69 * s * t223
      t225 = t1 ** 2
      t230 = s * t225 * x2 * x1 * t113 * t124
      t231 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t233 = t231 * t86 * t59
      t235 = t5 * t233 / 0.8D1
      t236 = FJET(XB1, XB2, s, 0.0D0, -t221, -t114, -t224, t230, -t235)
      t241 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t107)
      t249 = t119 + (0.90D2 * t5 * t132 - t137) * t86 / 0.720D3 + t144
      t250 = FJET(XB1, XB2, s, t112, -t114, 0.0D0, 0.0D0, 0.0D0, t249)
      t252 = t108 * t107 + t110 * t107 + t146 * t145 + t148 * t145 + t15
     #0 * t107 + t180 * t179 + t206 * 0.3141592653589793D1 * t208 * t203
     # / 0.16D2 + t212 * 0.3141592653589793D1 * t208 * t203 / 0.16D2 + t
     #217 * t179 - t236 * 0.3141592653589793D1 * t4 * t233 / 0.8D1 + t24
     #1 * t107 + t250 * t249
      t253 = FJET(XB1, XB2, s, t152, 0.0D0, -t154, 0.0D0, 0.0D0, t179)
      t256 = t2 * x1 * x3
      t258 = t1 * t113
      t259 = x3 * s * t258
      t260 = t153 * s
      t261 = t260 * t223
      t262 = t260 * t258
      t263 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t155, x
     #4)
      t265 = t263 * t27 * t86
      t267 = t5 * t265 / 0.8D1
      t268 = FJET(XB1, XB2, s, t256, -t259, -t261, t262, 0.0D0, -t267)
      t273 = FJET(XB1, XB2, s, t186, 0.0D0, t189, 0.0D0, 0.0D0, t205)
      t278 = FJET(XB1, XB2, s, t262, -t261, -t259, t256, 0.0D0, -t267)
      t283 = FJET(XB1, XB2, s, t189, 0.0D0, t186, 0.0D0, 0.0D0, t205)
      t288 = FJET(XB1, XB2, s, -t114, t112, 0.0D0, 0.0D0, 0.0D0, t249)
      t290 = FJET(XB1, XB2, s, -t114, -t224, 0.0D0, -t221, t230, -t235)
      t295 = FJET(XB1, XB2, s, -t154, 0.0D0, t152, 0.0D0, 0.0D0, t179)
      t297 = FJET(XB1, XB2, s, -t259, t256, t262, -t261, 0.0D0, -t267)
      t302 = FJET(XB1, XB2, s, -t224, -t114, -t221, 0.0D0, t230, -t235)
      t307 = FJET(XB1, XB2, s, -t261, t262, t256, -t259, 0.0D0, -t267)
      t312 = FJET(XB1, XB2, s, -t221, 0.0D0, -t224, -t114, t230, -t235)
      t317 = t253 * t179 - t268 * 0.3141592653589793D1 * t4 * t265 / 0.8
     #D1 + t273 * 0.3141592653589793D1 * t208 * t203 / 0.16D2 - t278 * 0
     #.3141592653589793D1 * t4 * t265 / 0.8D1 + t283 * 0.314159265358979
     #3D1 * t208 * t203 / 0.16D2 + t288 * t249 - t290 * 0.31415926535897
     #93D1 * t4 * t233 / 0.8D1 + t295 * t179 - t297 * 0.3141592653589793
     #D1 * t4 * t265 / 0.8D1 - t302 * 0.3141592653589793D1 * t4 * t233 /
     # 0.8D1 - t307 * 0.3141592653589793D1 * t4 * t265 / 0.8D1 - t312 * 
     #0.3141592653589793D1 * t4 * t233 / 0.8D1
      rrgg2gght8s4em1 = t252 + t317

      end function



      doubleprecision function rrgg2gght8s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t7 = 0.1D1 / x3
      t11 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t17 = 0.1D1 / x1
      t24 = rrgg2ggh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t25 = z ** 2
      t29 = Sin(x4 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t33 = log(0.4D1 / t25 / z * t30)
      t41 = -t5 * t6 * t7 / 0.16D2 - t5 * (t6 - t11) / x2 / 0.16D2 - t5 
     #* t6 * t17 / 0.8D1 - (-0.180D3 * t6 * 0.3141592653589793D1 * lh + 
     #0.90D2 * (t24 - t33 * t6) * 0.3141592653589793D1) * t4 / 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t46 = t2 * x1
      t48 = t2 * (-0.1D1 + x1)
      t49 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t52 = t5 * t49 * t17 / 0.8D1
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t46, -t48, 0.0D0, t52)
      t56 = t4 * t49 * t17
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t46, 0.0D0, t52)
      t63 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t65 = t2 * x3
      t66 = -0.1D1 + x3
      t67 = t2 * t66
      t69 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, -t66,
     # x4)
      t72 = t5 * t69 * t7 / 0.16D2
      t73 = FJET(XB1, XB2, s, 0.0D0, t65, 0.0D0, -t67, 0.0D0, t72)
      t76 = t4 * t69 * t7
      t79 = FJET(XB1, XB2, s, 0.0D0, -t67, 0.0D0, t65, 0.0D0, t72)
      t83 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t85 = FJET(XB1, XB2, s, t46, -t48, 0.0D0, 0.0D0, 0.0D0, t52)
      t89 = FJET(XB1, XB2, s, t65, 0.0D0, -t67, 0.0D0, 0.0D0, t72)
      t93 = FJET(XB1, XB2, s, -t67, 0.0D0, t65, 0.0D0, 0.0D0, t72)
      t97 = FJET(XB1, XB2, s, -t48, t46, 0.0D0, 0.0D0, 0.0D0, t52)
      rrgg2gght8s4em2 = t42 * t41 + t44 * t41 + t53 * 0.3141592653589793
     #D1 * t56 / 0.8D1 + t59 * 0.3141592653589793D1 * t56 / 0.8D1 + t63 
     #* t41 + t73 * 0.3141592653589793D1 * t76 / 0.16D2 + t79 * 0.314159
     #2653589793D1 * t76 / 0.16D2 + t83 * t41 + t85 * 0.3141592653589793
     #D1 * t56 / 0.8D1 + t89 * 0.3141592653589793D1 * t76 / 0.16D2 + t93
     # * 0.3141592653589793D1 * t76 / 0.16D2 + t97 * 0.3141592653589793D
     #1 * t56 / 0.8D1

      end function



      doubleprecision function rrgg2gght8s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght8s4em3 = -t9 * 0.3141592653589793D1 * t11 / 0.16D2 - t13 
     #* 0.3141592653589793D1 * t11 / 0.16D2 - t16 * 0.3141592653589793D1
     # * t11 / 0.16D2 - t19 * 0.3141592653589793D1 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght8s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh81J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = x1 ** 2
      t8 = t7 * x1
      t9 = t6 * t8
      t10 = t3 * t9
      t11 = x1 * t4
      t12 = z + t11
      t13 = t12 ** 2
      t15 = 0.1D1 / t13 / t12
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t12 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t12 + x2 * t19 - t28
      t30 = t29 ** 2
      t31 = t15 * t30
      t32 = s * t4
      t33 = 0.1D1 / t12
      t34 = x1 * t33
      t37 = 0.1D1 - x1
      t38 = t37 * x3
      t40 = s - t32 * t34 * t29 - t32 * t38
      t44 = t19 * t16 * t12 + x2 * x3 + t28
      t45 = t34 * t44
      t47 = t37 * t19
      t49 = s - t32 * t45 - t32 * t47
      t50 = t40 * t49
      t58 = 0.1D1 / t13
      t59 = t7 * t58
      t60 = t44 ** 2
      t61 = t59 * t60
      t65 = t50 * t2
      t66 = t37 ** 2
      t67 = t66 * t37
      t68 = t6 * t67
      t69 = t19 ** 2
      t70 = t69 * t19
      t71 = t68 * t70
      t74 = t5 * t66
      t75 = t74 * t69
      t79 = t4 * t37 * t19
      t82 = t2 * s
      t83 = t49 * t82
      t84 = t83 * z
      t91 = t44 * t37 * x3
      t96 = t50 * t2 * t5
      t101 = t2 * t6
      t102 = t50 * t101
      t103 = t47 * t61
      t106 = t82 * z
      t107 = t5 * x1
      t109 = t33 * t29
      t111 = t40 * t37 * x3
      t112 = t109 * t111
      t115 = t8 * t15
      t123 = t33 * t44
      t124 = t123 * x3
      t125 = t19 * x1 * t124
      t128 = t6 * t7
      t129 = t128 * t58
      t131 = t29 * t40
      t135 = t47 * t45
      t138 = 0.2D1 * t10 * t31 * t50 * t44 + 0.4D1 * t3 * t6 * t37 * t19
     # * t50 * t61 + 0.4D1 * t65 * t71 - 0.12D2 * t65 * t75 + 0.4D1 * t6
     #5 * t79 + t84 * t79 + 0.2D1 * t84 * t71 - 0.8D1 * t3 * t50 * t5 * 
     #t34 * t91 - 0.16D2 * t96 * t59 * t29 * t44 + 0.2D1 * t102 * t103 -
     # 0.3D1 * t106 * t107 * t112 + 0.16D2 * t102 * t115 * t30 * t44 + 0
     #.20D2 * t50 * t101 * t66 * t125 - 0.3D1 * t106 * t129 * t131 * t91
     # - 0.14D2 * t96 * t135
      t139 = z * t6
      t140 = t83 * t139
      t142 = t66 * t69 * t45
      t149 = t15 * t29
      t158 = t30 * t29
      t159 = t15 * t158
      t167 = t106 * t9
      t168 = t40 * t44
      t177 = t74 * t19 * x3
      t180 = t5 ** 2
      t181 = t7 ** 2
      t183 = t13 ** 2
      t185 = t60 ** 2
      t190 = x3 ** 2
      t192 = t68 * t19 * t190
      t194 = t5 * t7
      t195 = t58 * t30
      t199 = t3 * t40
      t209 = t68 * t69 * x3
      t212 = 0.2D1 * t140 * t142 - 0.3D1 * t83 * z * t5 * t135 + 0.4D1 *
     # t10 * t149 * t50 * t60 - 0.3D1 * t83 * t139 * t66 * t125 + 0.2D1 
     #* t10 * t159 * t50 + 0.4D1 * t102 * t115 * t29 * t60 + t167 * t31 
     #* t168 + 0.2D1 * t106 * t6 * t115 * t158 * t40 + 0.2D1 * t84 * t17
     #7 - 0.2D1 * t65 * t180 * t181 / t183 * t185 + t84 * t192 - 0.12D2 
     #* t65 * t194 * t195 + 0.2D1 * t199 * t49 * t4 * t38 + 0.4D1 * t65 
     #* t192 - 0.16D2 * t65 * t177 + 0.16D2 * t65 * t209
      t215 = t66 * t190
      t234 = t30 * t40
      t244 = t58 * t29
      t249 = t66 ** 2
      t251 = t190 ** 2
      t257 = t3 * t50
      t263 = t50 * t2 * t180
      t272 = t3 * t129
      t278 = -0.4D1 * t199 * t49 * t5 * t215 + 0.2D1 * t3 * t6 * t67 * t
     #70 * t50 + 0.4D1 * t65 * t11 * t109 + 0.4D1 * t65 * t9 * t159 + t1
     #06 * t4 * t34 * t131 + t106 * t5 * t59 * t234 + t84 * t209 - 0.14D
     #2 * t49 * t2 * t107 * t112 + 0.20D2 * t50 * t101 * t7 * t244 * t91
     # + t84 * t75 - 0.2D1 * t65 * t180 * t249 * t251 + 0.4D1 * t140 * t
     #103 - 0.4D1 * t257 * t194 * t58 * t60 - 0.4D1 * t263 * t67 * t190 
     #* x3 * t45 - 0.6D1 * t263 * t215 * t61 + 0.2D1 * t272 * t234 * t49
     # * t37 * x3
      t279 = t3 * t68
      t285 = t6 * t66
      t319 = t6 * x1
      t352 = 0.2D1 * t279 * t69 * t40 * t49 * x3 + 0.2D1 * t3 * t285 * t
     #69 * t50 * t45 + 0.14D2 * t102 * t142 + 0.4D1 * t279 * t19 * t40 *
     # t49 * t190 + 0.2D1 * t102 * t34 * t29 * t66 * t190 - 0.4D1 * t263
     # * t38 * t115 * t60 * t44 + 0.8D1 * t272 * t131 * t49 * t91 + 0.2D
     #1 * t106 * t194 * t244 * t168 + t167 * t149 * t40 * t60 + 0.4D1 * 
     #t106 * t319 * t109 * t40 * t66 * t190 + 0.4D1 * t3 * t319 * t33 * 
     #t131 * t49 * t66 * t190 + 0.14D2 * t102 * t59 * t30 * t37 * x3 + 0
     #.2D1 * t106 * t128 * t195 * t111 - 0.2D1 * t257 + 0.8D1 * t3 * t28
     #5 * t19 * t50 * x1 * t124 + 0.2D1 * t257 * t11 * t123
      rrgg2ggh81J1 = -0.9D1 / 0.2D1 * wd * (t138 + t212 + t278 + t352) /
     # t1 / t40 / t49 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh81J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = t1 * x1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = t5 * x1
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t30 = t10 * t7 * t4 + x2 * x3 + t19
      t31 = t6 * t30
      t33 = t23 * t10
      t35 = s - t2 * t31 - t2 * t33
      t36 = t26 * t35
      t37 = s ** 2
      t38 = t37 ** 2
      t39 = t1 ** 2
      t40 = t39 * t1
      t41 = t38 * t40
      t42 = x1 ** 2
      t45 = t4 ** 2
      t46 = 0.1D1 / t45
      t47 = t46 * t20
      t49 = t30 * t23 * x3
      t51 = t36 * t41 * t42 * t47 * t49
      t53 = t38 * z
      t54 = x1 * t40
      t57 = t20 * t26
      t58 = t23 ** 2
      t60 = x3 ** 2
      t64 = 0.4D1 * t53 * t54 * t5 * t57 * t35 * t58 * t60
      t66 = t39 * x1
      t68 = t5 * t20
      t69 = t24 * t26
      t70 = t68 * t69
      t71 = t35 * t38 * t66 * t70
      t73 = t36 * t41
      t74 = t42 * t46
      t75 = t20 ** 2
      t79 = t73 * t74 * t75 * t23 * x3
      t81 = t38 * s
      t82 = t81 * z
      t83 = t40 * t42
      t85 = t46 * t75
      t88 = 0.2D1 * t82 * t83 * t85 * t69
      t89 = t42 * x1
      t90 = t40 * t89
      t91 = t53 * t90
      t93 = 0.1D1 / t45 / t4
      t94 = t75 * t20
      t95 = t93 * t94
      t98 = 0.2D1 * t91 * t95 * t36
      t99 = t58 * t23
      t100 = t40 * t99
      t101 = t53 * t100
      t106 = 0.4D1 * t101 * t10 * t26 * t35 * t60
      t107 = t36 * t38
      t108 = t10 ** 2
      t109 = t108 * t10
      t110 = t100 * t109
      t112 = 0.4D1 * t110 * t107
      t113 = t39 * t58
      t114 = t113 * t108
      t116 = 0.12D2 * t107 * t114
      t118 = t1 * t23 * t10
      t120 = 0.4D1 * t107 * t118
      t121 = t35 * t81
      t122 = t121 * z
      t123 = t122 * t118
      t125 = 0.2D1 * t122 * t110
      t126 = t122 * t114
      t127 = t39 ** 2
      t128 = t58 ** 2
      t130 = t60 ** 2
      t133 = 0.2D1 * t107 * t127 * t128 * t130
      t134 = t39 * t42
      t136 = t26 * t30
      t139 = 0.2D1 * t82 * t134 * t47 * t136
      t140 = 0.20D2 * t51 + t64 - 0.14D2 * t71 + 0.14D2 * t79 + t88 + t9
     #8 + t106 + t112 - t116 + t120 + t123 + t125 + t126 - t133 + t139
      t141 = t82 * t90
      t142 = t93 * t20
      t143 = t30 ** 2
      t146 = t141 * t142 * t26 * t143
      t148 = t36 * t38 * t127
      t149 = t89 * t93
      t154 = 0.4D1 * t148 * t24 * t149 * t143 * t30
      t156 = t36 * t38 * t39
      t160 = 0.16D2 * t156 * t74 * t20 * t30
      t163 = t33 * t31
      t164 = t121 * z * t39 * t163
      t166 = z * t40
      t167 = t121 * t166
      t168 = t74 * t143
      t169 = t33 * t168
      t171 = 0.4D1 * t167 * t169
      t173 = t82 * t66 * t70
      t175 = t83 * t46
      t178 = t82 * t175 * t57 * t49
      t183 = 0.16D2 * t73 * t149 * t75 * t30
      t187 = t5 * t30
      t188 = t187 * x3
      t189 = t10 * x1 * t188
      t190 = t36 * t41 * t58 * t189
      t197 = 0.4D1 * t82 * t54 * t68 * t26 * t58 * t60
      t198 = t53 * t36
      t201 = 0.2D1 * t198 * t3 * t187
      t207 = 0.4D1 * t53 * t40 * t23 * t10 * t36 * t168
      t208 = t40 * t58
      t213 = 0.2D1 * t53 * t208 * t108 * t36 * t31
      t216 = t121 * t166 * t58 * t189
      t222 = 0.4D1 * t148 * t99 * t60 * x3 * t31
      t226 = t73 * t6 * t20 * t58 * t60
      t228 = t146 - t154 - t160 - 0.3D1 * t164 + t171 - 0.3D1 * t173 - 0
     #.3D1 * t178 + t183 + 0.20D2 * t190 + t197 + t201 + t207 + t213 - 0
     #.3D1 * t216 - t222 + 0.2D1 * t226
      t231 = t58 * t108 * t31
      t232 = t73 * t231
      t235 = t113 * t10 * x3
      t237 = 0.2D1 * t122 * t235
      t238 = t42 ** 2
      t240 = t45 ** 2
      t242 = t143 ** 2
      t246 = 0.2D1 * t107 * t127 * t238 / t240 * t242
      t248 = t100 * t10 * t60
      t249 = t122 * t248
      t251 = t100 * t108 * x3
      t252 = t122 * t251
      t257 = 0.2D1 * t82 * t40 * t149 * t94 * t26
      t260 = 0.4D1 * t107 * t90 * t95
      t263 = t82 * t1 * t6 * t57
      t265 = t75 * t26
      t267 = t82 * t39 * t74 * t265
      t270 = 0.4D1 * t107 * t3 * t68
      t275 = 0.2D1 * t53 * t40 * t99 * t109 * t36
      t277 = 0.16D2 * t107 * t235
      t279 = 0.16D2 * t107 * t251
      t280 = t53 * t26
      t282 = t58 * t60
      t285 = 0.4D1 * t280 * t35 * t39 * t282
      t286 = t93 * t75
      t290 = 0.2D1 * t91 * t286 * t36 * t30
      t294 = 0.4D1 * t91 * t142 * t36 * t143
      t295 = 0.14D2 * t232 + t237 - t246 + t249 + t252 + t257 + t260 + t
     #263 + t267 + t270 + t275 - t277 + t279 - t285 + t290 + t294
      t298 = 0.12D2 * t107 * t134 * t85
      t302 = 0.2D1 * t280 * t35 * t1 * t24
      t304 = 0.4D1 * t107 * t248
      t305 = t53 * t175
      t310 = 0.2D1 * t305 * t265 * t35 * t23 * x3
      t315 = 0.2D1 * t101 * t108 * t26 * t35 * x3
      t316 = t73 * t169
      t321 = 0.4D1 * t198 * t134 * t46 * t143
      t323 = 0.2D1 * t167 * t231
      t327 = t53 * t36 * t39 * t6 * t49
      t333 = t53 * t208 * t10 * t36 * x1 * t188
      t338 = 0.4D1 * t73 * t149 * t20 * t143
      t340 = t141 * t286 * t136
      t341 = 0.2D1 * t198
      t344 = t305 * t57 * t35 * t49
      t348 = 0.6D1 * t148 * t282 * t168
      t349 = t156 * t163
      t351 = -t298 + t302 + t304 + t310 + t315 + 0.2D1 * t316 - t321 + t
     #323 - 0.8D1 * t327 + 0.8D1 * t333 + t338 + t340 - t341 + 0.8D1 * t
     #344 - t348 - 0.14D2 * t349
      t359 = -0.22D2 * t51 - t64 + 0.16D2 * t71 - 0.16D2 * t79 - t88 - t
     #98 - t106 - t112 + t116 - t120 - t123 - t125 - t126 + t133 - t139
      t366 = -t146 + t154 + t160 + 0.6D1 * t164 - t171 + 0.6D1 * t173 + 
     #0.6D1 * t178 - t183 - 0.22D2 * t190 - t197 - t201 - t207 - t213 + 
     #0.6D1 * t216 + t222 - 0.4D1 * t226
      t369 = -0.16D2 * t232 - t237 + t246 - t249 - t252 - t257 - t260 - 
     #t263 - t267 - t270 - t275 + t277 - t279 + t285 - t290 - t294
      t375 = t298 - t302 - t304 - t310 - t315 - 0.4D1 * t316 + t321 - t3
     #23 + 0.14D2 * t327 - 0.14D2 * t333 - t338 - t340 + t341 - 0.14D2 *
     # t344 + t348 + 0.16D2 * t349
      rrgg2ggh81J2 = -0.9D1 / 0.2D1 * (0.2D1 * wd * (t140 + t228 + t295 
     #+ t351) + wd * (t359 + t366 + t369 + t375)) / t37 / t26 / t35 / z 
     #/ 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh81J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
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
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t30 = t10 * t7 * t4 + x2 * x3 + t19
      t31 = t6 * t30
      t33 = t23 * t10
      t35 = s - t2 * t31 - t2 * t33
      t36 = t26 * t35
      t37 = s ** 2
      t38 = t37 ** 2
      t39 = t36 * t38
      t40 = t1 ** 2
      t41 = x1 ** 2
      t42 = t40 * t41
      t43 = t4 ** 2
      t44 = 0.1D1 / t43
      t45 = t20 ** 2
      t46 = t44 * t45
      t50 = t38 * z
      t51 = t50 * t26
      t56 = t40 * t1
      t57 = t23 ** 2
      t58 = t57 * t23
      t59 = t56 * t58
      t60 = x3 ** 2
      t62 = t59 * t10 * t60
      t65 = t40 * t57
      t67 = t65 * t10 * x3
      t70 = t10 ** 2
      t72 = t59 * t70 * x3
      t76 = t57 * t60
      t81 = t70 * t10
      t86 = t5 * t20
      t90 = t41 * x1
      t91 = t56 * t90
      t93 = 0.1D1 / t43 / t4
      t94 = t45 * t20
      t95 = t93 * t94
      t99 = t38 * s
      t100 = t99 * z
      t102 = t20 * t26
      t106 = t41 * t44
      t107 = t45 * t26
      t110 = t35 * t99
      t111 = t110 * z
      t114 = t90 * t93
      t121 = t40 ** 2
      t122 = t41 ** 2
      t124 = t43 ** 2
      t126 = t30 ** 2
      t127 = t126 ** 2
      t132 = -0.12D2 * t39 * t42 * t46 + 0.2D1 * t51 * t35 * t1 * t24 + 
     #0.4D1 * t39 * t62 - 0.16D2 * t39 * t67 + 0.16D2 * t39 * t72 - 0.4D
     #1 * t51 * t35 * t40 * t76 + 0.2D1 * t50 * t56 * t58 * t81 * t36 + 
     #0.4D1 * t39 * t3 * t86 + 0.4D1 * t39 * t91 * t95 + t100 * t1 * t6 
     #* t102 + t100 * t40 * t106 * t107 + t111 * t72 + 0.2D1 * t100 * t5
     #6 * t114 * t94 * t26 + 0.2D1 * t111 * t67 - 0.2D1 * t39 * t121 * t
     #122 / t124 * t127
      t133 = t111 * t62
      t136 = t33 * t31
      t137 = t110 * z * t40 * t136
      t139 = z * t56
      t140 = t110 * t139
      t141 = t106 * t126
      t142 = t33 * t141
      t144 = 0.4D1 * t140 * t142
      t145 = t50 * t36
      t146 = 0.2D1 * t145
      t147 = t50 * t91
      t148 = t93 * t45
      t152 = 0.2D1 * t147 * t148 * t36 * t30
      t153 = t93 * t20
      t157 = 0.4D1 * t147 * t153 * t36 * t126
      t158 = t38 * t56
      t159 = t36 * t158
      t163 = 0.16D2 * t159 * t114 * t45 * t30
      t165 = t57 * t70 * t31
      t167 = 0.2D1 * t140 * t165
      t171 = t30 * t23 * x3
      t173 = t50 * t36 * t40 * t6 * t171
      t178 = t5 * t30
      t179 = t178 * x3
      t180 = t10 * x1 * t179
      t181 = t36 * t158 * t57 * t180
      t185 = t110 * t139 * t57 * t180
      t187 = t56 * t57
      t192 = t50 * t187 * t10 * t36 * x1 * t179
      t196 = 0.2D1 * t147 * t95 * t36
      t200 = 0.4D1 * t145 * t42 * t44 * t126
      t204 = t159 * t6 * t20 * t57 * t60
      t206 = t50 * t59
      t211 = 0.2D1 * t206 * t70 * t26 * t35 * x3
      t212 = t133 - 0.3D1 * t137 + t144 - t146 + t152 + t157 + t163 + t1
     #67 - 0.8D1 * t173 + 0.20D2 * t181 - 0.3D1 * t185 + 0.8D1 * t192 + 
     #t196 - t200 + 0.2D1 * t204 + t211
      t215 = t36 * t38 * t121
      t220 = 0.4D1 * t215 * t24 * t114 * t126 * t30
      t222 = t36 * t38 * t40
      t226 = 0.16D2 * t222 * t106 * t20 * t30
      t227 = t40 * x1
      t230 = t26 * t23 * x3
      t231 = t86 * t230
      t232 = t100 * t227 * t231
      t236 = t44 * t20
      t238 = t36 * t158 * t41 * t236 * t171
      t240 = t159 * t165
      t242 = t56 * t41
      t243 = t242 * t44
      t244 = t50 * t243
      t247 = t244 * t102 * t35 * t171
      t249 = t59 * t81
      t251 = 0.4D1 * t39 * t249
      t254 = 0.6D1 * t215 * t76 * t141
      t257 = t35 * t38 * t227 * t231
      t259 = t65 * t70
      t261 = 0.12D2 * t39 * t259
      t265 = t159 * t106 * t45 * t23 * x3
      t268 = t1 * t23 * t10
      t270 = 0.4D1 * t39 * t268
      t271 = t111 * t268
      t273 = 0.2D1 * t111 * t249
      t274 = t57 ** 2
      t276 = t60 ** 2
      t279 = 0.2D1 * t39 * t121 * t274 * t276
      t280 = t111 * t259
      t281 = -t220 - t226 - 0.3D1 * t232 + 0.20D2 * t238 + 0.14D2 * t240
     # + 0.8D1 * t247 + t251 - t254 - 0.14D2 * t257 - t261 + 0.14D2 * t2
     #65 + t270 + t271 + t273 - t279 + t280
      t285 = 0.4D1 * t159 * t114 * t20 * t126
      t286 = t100 * t91
      t287 = t26 * t30
      t289 = t286 * t148 * t287
      t294 = 0.2D1 * t50 * t187 * t70 * t36 * t31
      t298 = 0.2D1 * t100 * t42 * t236 * t287
      t301 = t286 * t153 * t26 * t126
      t306 = 0.4D1 * t206 * t10 * t26 * t35 * t60
      t307 = t56 * x1
      t314 = 0.4D1 * t50 * t307 * t5 * t102 * t35 * t57 * t60
      t317 = t100 * t243 * t102 * t171
      t322 = 0.2D1 * t100 * t242 * t46 * t230
      t328 = 0.4D1 * t100 * t307 * t86 * t26 * t57 * t60
      t333 = 0.2D1 * t244 * t107 * t35 * t23 * x3
      t339 = 0.4D1 * t50 * t56 * t23 * t10 * t36 * t141
      t342 = 0.2D1 * t145 * t3 * t178
      t347 = 0.4D1 * t215 * t58 * t60 * x3 * t31
      t348 = t222 * t136
      t350 = t159 * t142
      t352 = t285 + t289 + t294 + t298 + t301 + t306 + t314 - 0.3D1 * t3
     #17 + t322 + t328 + t333 + t339 + t342 - t347 - 0.14D2 * t348 + 0.2
     #D1 * t350
      t364 = -t133 + 0.6D1 * t137 - t144 + t146 - t152 - t157 - t163 - t
     #167 + 0.14D2 * t173 - 0.22D2 * t181 + 0.6D1 * t185 - 0.14D2 * t192
     # - t196 + t200 - 0.4D1 * t204 - t211
      t372 = t220 + t226 + 0.6D1 * t232 - 0.22D2 * t238 - 0.16D2 * t240 
     #- 0.14D2 * t247 - t251 + t254 + 0.16D2 * t257 + t261 - 0.16D2 * t2
     #65 - t270 - t271 - t273 + t279 - t280
      t376 = -t285 - t289 - t294 - t298 - t301 - t306 - t314 + 0.6D1 * t
     #317 - t322 - t328 - t333 - t339 - t342 + t347 + 0.16D2 * t348 - 0.
     #4D1 * t350
      rrgg2ggh81J3 = -0.9D1 / 0.2D1 * (0.3D1 * wd * (t132 + t212 + t281 
     #+ t352) + 0.2D1 * wd * (-t132 + t364 + t372 + t376)) / t37 / t26 /
     # t35 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh81J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = x1 ** 2
      t8 = t6 * t7
      t9 = x1 * t4
      t10 = z + t9
      t11 = t10 ** 2
      t12 = 0.1D1 / t11
      t13 = t8 * t12
      t14 = t3 * t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t10 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t10 + x2 * t18 - t27
      t29 = t28 ** 2
      t30 = s * t4
      t31 = 0.1D1 / t10
      t32 = x1 * t31
      t35 = 0.1D1 - x1
      t36 = t35 * x3
      t38 = s - t30 * t32 * t28 - t30 * t36
      t39 = t29 * t38
      t43 = t18 * t15 * t10 + x2 * x3 + t27
      t44 = t32 * t43
      t46 = t35 * t18
      t48 = s - t30 * t44 - t30 * t46
      t53 = 0.2D1 * t14 * t39 * t48 * t35 * x3
      t57 = t38 * t48
      t58 = t7 * t12
      t59 = t43 ** 2
      t60 = t58 * t59
      t63 = 0.4D1 * t3 * t6 * t35 * t18 * t57 * t60
      t64 = t5 ** 2
      t66 = t57 * t2 * t64
      t67 = t7 * x1
      t69 = 0.1D1 / t11 / t10
      t70 = t67 * t69
      t75 = 0.4D1 * t66 * t36 * t70 * t59 * t43
      t76 = t2 * s
      t77 = t76 * z
      t78 = x1 * t5
      t80 = t31 * t28
      t82 = t38 * t35 * x3
      t83 = t80 * t82
      t84 = t77 * t78 * t83
      t87 = t28 * t38
      t89 = t43 * t35 * x3
      t91 = t77 * t13 * t87 * t89
      t94 = t57 * t2 * t5
      t98 = 0.16D2 * t94 * t58 * t28 * t43
      t99 = t5 * t7
      t101 = t12 * t28
      t102 = t38 * t43
      t105 = 0.2D1 * t77 * t99 * t101 * t102
      t106 = t6 * t67
      t107 = t77 * t106
      t108 = t69 * t28
      t111 = t107 * t108 * t38 * t59
      t112 = t35 ** 2
      t113 = t112 * t35
      t114 = t6 * t113
      t115 = t3 * t114
      t116 = t18 ** 2
      t121 = 0.2D1 * t115 * t116 * t38 * t48 * x3
      t123 = x3 ** 2
      t127 = 0.4D1 * t115 * t18 * t38 * t48 * t123
      t128 = t6 * x1
      t135 = 0.4D1 * t3 * t128 * t31 * t87 * t48 * t112 * t123
      t136 = t2 * t6
      t137 = t57 * t136
      t141 = t137 * t32 * t28 * t112 * t123
      t143 = t3 * t106
      t144 = t29 * t28
      t145 = t69 * t144
      t148 = 0.2D1 * t143 * t145 * t57
      t152 = t137 * t58 * t29 * t35 * x3
      t156 = t77 * t4 * t32 * t87
      t157 = t53 + t63 - t75 - 0.3D1 * t84 - 0.3D1 * t91 - t98 + t105 + 
     #t111 + t121 + t127 + t135 + 0.2D1 * t141 + t148 + 0.14D2 * t152 + 
     #t156
      t160 = t77 * t5 * t58 * t39
      t161 = t48 * t76
      t162 = t161 * z
      t164 = t114 * t116 * x3
      t165 = t162 * t164
      t170 = 0.2D1 * t77 * t6 * t70 * t144 * t38
      t171 = t5 * t112
      t173 = t171 * t18 * x3
      t175 = 0.2D1 * t162 * t173
      t176 = t57 * t2
      t177 = t7 ** 2
      t179 = t11 ** 2
      t181 = t59 ** 2
      t185 = 0.2D1 * t176 * t64 * t177 / t179 * t181
      t187 = t114 * t18 * t123
      t188 = t162 * t187
      t189 = t46 * t44
      t190 = t94 * t189
      t192 = t46 * t60
      t193 = t137 * t192
      t198 = 0.16D2 * t137 * t70 * t29 * t43
      t200 = t116 * t18
      t204 = 0.2D1 * t3 * t6 * t113 * t200 * t57
      t207 = 0.4D1 * t176 * t9 * t80
      t210 = 0.4D1 * t176 * t106 * t145
      t211 = t3 * t38
      t213 = t112 * t123
      t216 = 0.4D1 * t211 * t48 * t5 * t213
      t220 = t3 * t57 * t5 * t32 * t89
      t223 = 0.4D1 * t176 * t187
      t225 = 0.16D2 * t176 * t173
      t226 = t160 + t165 + t170 + t175 - t185 + t188 - 0.14D2 * t190 + 0
     #.2D1 * t193 + t198 + t204 + t207 + t210 - t216 - 0.8D1 * t220 + t2
     #23 - t225
      t231 = 0.2D1 * t211 * t48 * t4 * t36
      t232 = t12 * t29
      t235 = 0.12D2 * t176 * t99 * t232
      t236 = t6 * t112
      t240 = t31 * t43
      t241 = t240 * x3
      t243 = t3 * t236 * t18 * t57 * x1 * t241
      t248 = t18 * x1 * t241
      t249 = t57 * t136 * t112 * t248
      t254 = t57 * t136 * t7 * t101 * t89
      t256 = t69 * t29
      t260 = 0.2D1 * t143 * t256 * t57 * t43
      t264 = 0.4D1 * t143 * t108 * t57 * t59
      t266 = 0.16D2 * t176 * t164
      t269 = t14 * t87 * t48 * t89
      t272 = t112 * t116 * t44
      t273 = t137 * t272
      t277 = t48 * t2 * t78 * t83
      t279 = t3 * t57
      t282 = 0.2D1 * t279 * t9 * t240
      t283 = z * t6
      t286 = t161 * t283 * t112 * t248
      t291 = 0.2D1 * t77 * t8 * t232 * t82
      t297 = 0.4D1 * t77 * t128 * t80 * t38 * t112 * t123
      t298 = t114 * t200
      t300 = 0.4D1 * t176 * t298
      t301 = t231 - t235 + 0.8D1 * t243 + 0.20D2 * t249 + 0.20D2 * t254 
     #+ t260 + t264 + t266 + 0.8D1 * t269 + 0.14D2 * t273 - 0.14D2 * t27
     #7 + t282 - 0.3D1 * t286 + t291 + t297 + t300
      t302 = t171 * t116
      t304 = 0.12D2 * t176 * t302
      t306 = t4 * t35 * t18
      t308 = 0.4D1 * t176 * t306
      t309 = t162 * t306
      t311 = 0.2D1 * t162 * t298
      t312 = t162 * t302
      t313 = t112 ** 2
      t315 = t123 ** 2
      t318 = 0.2D1 * t176 * t64 * t313 * t315
      t321 = t161 * z * t5 * t189
      t323 = t161 * t283
      t325 = 0.4D1 * t323 * t192
      t329 = 0.4D1 * t279 * t99 * t12 * t59
      t330 = 0.2D1 * t279
      t335 = 0.4D1 * t66 * t113 * t123 * x3 * t44
      t338 = 0.6D1 * t66 * t213 * t60
      t343 = 0.2D1 * t3 * t236 * t116 * t57 * t44
      t345 = 0.2D1 * t323 * t272
      t349 = 0.4D1 * t137 * t70 * t28 * t59
      t351 = t107 * t256 * t102
      t352 = -t304 + t308 + t309 + t311 + t312 - t318 - 0.3D1 * t321 + t
     #325 - t329 - t330 - t335 - t338 + t343 + t345 + t349 + t351
      t361 = -t53 - t63 + t75 + 0.6D1 * t84 + 0.6D1 * t91 + t98 - t105 -
     # t111 - t121 - t127 - t135 - 0.4D1 * t141 - t148 - 0.16D2 * t152 -
     # t156
      t365 = -t160 - t165 - t170 - t175 + t185 - t188 + 0.16D2 * t190 - 
     #0.4D1 * t193 - t198 - t204 - t207 - t210 + t216 + 0.14D2 * t220 - 
     #t223 + t225
      t374 = -t231 + t235 - 0.14D2 * t243 - 0.22D2 * t249 - 0.22D2 * t25
     #4 - t260 - t264 - t266 - 0.14D2 * t269 - 0.16D2 * t273 + 0.16D2 * 
     #t277 - t282 + 0.6D1 * t286 - t291 - t297 - t300
      t376 = t304 - t308 - t309 - t311 - t312 + t318 + 0.6D1 * t321 - t3
     #25 + t329 + t330 + t335 + t338 - t343 - t345 - t349 - t351
      rrgg2ggh81J4 = -0.9D1 / 0.2D1 * (0.4D1 * wd * (t157 + t226 + t301 
     #+ t352) + 0.3D1 * wd * (t361 + t365 + t374 + t376)) / t1 / t38 / t
     #48 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh81J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = t3 * z
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = t6 * t5
      t8 = x1 ** 2
      t9 = t7 * t8
      t10 = x1 * t5
      t11 = z + t10
      t12 = t11 ** 2
      t13 = 0.1D1 / t12
      t14 = t9 * t13
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t19 = 0.1D1 - x3
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t17 * t11 * x2 * t19)
      t28 = 0.2D1 * t22 * t26
      t29 = t17 * t11 + x2 * t19 - t28
      t30 = s * t5
      t31 = 0.1D1 / t11
      t32 = x1 * t31
      t35 = 0.1D1 - x1
      t36 = t35 * x3
      t38 = s - t30 * t32 * t29 - t30 * t36
      t39 = t29 * t38
      t43 = t19 * t16 * t11 + x2 * x3 + t28
      t45 = t43 * t35 * x3
      t47 = t4 * t14 * t39 * t45
      t49 = t2 * z
      t50 = t49 * t14
      t51 = t29 ** 2
      t52 = t51 * t38
      t53 = t32 * t43
      t55 = t35 * t19
      t57 = s - t30 * t53 - t30 * t55
      t62 = 0.2D1 * t50 * t52 * t57 * t35 * x3
      t63 = t38 * t57
      t64 = t2 * t7
      t65 = t63 * t64
      t66 = t8 * x1
      t68 = 0.1D1 / t12 / t11
      t69 = t66 * t68
      t70 = t43 ** 2
      t74 = 0.4D1 * t65 * t69 * t29 * t70
      t75 = t7 * t66
      t76 = t4 * t75
      t77 = t68 * t51
      t78 = t38 * t43
      t80 = t76 * t77 * t78
      t81 = t3 * t57
      t82 = t81 * z
      t83 = t35 ** 2
      t84 = t83 * t35
      t85 = t7 * t84
      t86 = t19 ** 2
      t87 = t86 * t19
      t88 = t85 * t87
      t90 = 0.2D1 * t82 * t88
      t91 = t6 * t83
      t92 = t91 * t86
      t93 = t82 * t92
      t94 = t63 * t2
      t95 = t6 ** 2
      t96 = t83 ** 2
      t98 = x3 ** 2
      t99 = t98 ** 2
      t102 = 0.2D1 * t94 * t95 * t96 * t99
      t103 = t6 * t8
      t105 = t13 * t29
      t108 = 0.2D1 * t4 * t103 * t105 * t78
      t109 = t68 * t29
      t112 = t76 * t109 * t38 * t70
      t116 = t8 * t13
      t117 = t116 * t70
      t120 = 0.4D1 * t49 * t7 * t35 * t19 * t63 * t117
      t121 = t7 * t83
      t126 = 0.2D1 * t49 * t121 * t86 * t63 * t53
      t128 = t83 * t86 * t53
      t129 = t65 * t128
      t132 = 0.4D1 * t94 * t88
      t134 = 0.12D2 * t94 * t92
      t136 = t5 * t35 * t19
      t138 = 0.4D1 * t94 * t136
      t139 = -0.3D1 * t47 + t62 + t74 + t80 + t90 + t93 - t102 + t108 + 
     #t112 + t120 + t126 + 0.14D2 * t129 + t132 - t134 + t138
      t140 = t82 * t136
      t144 = t31 * t43
      t145 = t144 * x3
      t146 = t19 * x1 * t145
      t147 = t63 * t64 * t83 * t146
      t149 = t49 * t63
      t153 = 0.4D1 * t149 * t103 * t13 * t70
      t154 = z * t7
      t155 = t81 * t154
      t157 = 0.2D1 * t155 * t128
      t160 = t55 * t53
      t161 = t81 * z * t6 * t160
      t164 = t85 * t19 * t98
      t165 = t82 * t164
      t166 = t8 ** 2
      t168 = t12 ** 2
      t170 = t70 ** 2
      t174 = 0.2D1 * t94 * t95 * t166 / t168 * t170
      t176 = t91 * t19 * x3
      t178 = 0.2D1 * t82 * t176
      t180 = t51 * t29
      t184 = 0.2D1 * t4 * t7 * t69 * t180 * t38
      t186 = t85 * t86 * x3
      t187 = t82 * t186
      t190 = t4 * t6 * t116 * t52
      t193 = t4 * t5 * t32 * t39
      t194 = t68 * t180
      t197 = 0.4D1 * t94 * t75 * t194
      t198 = t31 * t29
      t201 = 0.4D1 * t94 * t10 * t198
      t206 = 0.2D1 * t49 * t7 * t84 * t87 * t63
      t207 = t49 * t38
      t209 = t83 * t98
      t212 = 0.4D1 * t207 * t57 * t6 * t209
      t213 = t140 + 0.20D2 * t147 - t153 + t157 - 0.3D1 * t161 + t165 - 
     #t174 + t178 + t184 + t187 + t190 + t193 + t197 + t201 + t206 - t21
     #2
      t216 = 0.16D2 * t94 * t186
      t218 = 0.16D2 * t94 * t176
      t220 = 0.4D1 * t94 * t164
      t224 = 0.2D1 * t207 * t57 * t5 * t36
      t225 = t13 * t51
      t228 = 0.12D2 * t94 * t103 * t225
      t229 = t55 * t117
      t230 = t65 * t229
      t232 = t6 * x1
      t235 = t38 * t35 * x3
      t236 = t198 * t235
      t237 = t4 * t232 * t236
      t242 = 0.16D2 * t65 * t69 * t51 * t43
      t244 = t63 * t2 * t95
      t247 = 0.6D1 * t244 * t209 * t117
      t251 = t65 * t32 * t29 * t83 * t98
      t253 = t49 * t75
      t257 = 0.2D1 * t253 * t77 * t63 * t43
      t261 = 0.4D1 * t253 * t109 * t63 * t70
      t262 = t7 * x1
      t268 = 0.4D1 * t4 * t262 * t198 * t38 * t83 * t98
      t271 = 0.2D1 * t149 * t10 * t144
      t272 = 0.2D1 * t149
      t279 = 0.4D1 * t49 * t262 * t31 * t39 * t57 * t83 * t98
      t280 = t216 - t218 + t220 + t224 - t228 + 0.2D1 * t230 - 0.3D1 * t
     #237 + t242 - t247 + 0.2D1 * t251 + t257 + t261 + t268 + t271 - t27
     #2 + t279
      t284 = 0.2D1 * t4 * t9 * t225 * t235
      t285 = t49 * t85
      t290 = 0.2D1 * t285 * t86 * t38 * t57 * x3
      t295 = 0.4D1 * t285 * t19 * t38 * t57 * t98
      t300 = 0.4D1 * t244 * t36 * t69 * t70 * t43
      t302 = t63 * t2 * t6
      t306 = 0.16D2 * t302 * t116 * t29 * t43
      t309 = t57 * t2 * t232 * t236
      t314 = t65 * t116 * t51 * t35 * x3
      t318 = t50 * t39 * t57 * t45
      t323 = t63 * t64 * t8 * t105 * t45
      t328 = t49 * t63 * t6 * t32 * t45
      t334 = t49 * t121 * t19 * t63 * x1 * t145
      t340 = 0.4D1 * t244 * t84 * t98 * x3 * t53
      t342 = 0.4D1 * t155 * t229
      t345 = t81 * t154 * t83 * t146
      t349 = 0.2D1 * t253 * t194 * t63
      t350 = t302 * t160
      t352 = t284 + t290 + t295 - t300 - t306 - 0.14D2 * t309 + 0.14D2 *
     # t314 + 0.8D1 * t318 + 0.20D2 * t323 - 0.8D1 * t328 + 0.8D1 * t334
     # - t340 + t342 - 0.3D1 * t345 + t349 - 0.14D2 * t350
      t359 = 0.6D1 * t47 - t62 - t74 - t80 - t90 - t93 + t102 - t108 - t
     #112 - t120 - t126 - 0.16D2 * t129 - t132 + t134 - t138
      t362 = -t140 - 0.22D2 * t147 + t153 - t157 + 0.6D1 * t161 - t165 +
     # t174 - t178 - t184 - t187 - t190 - t193 - t197 - t201 - t206 + t2
     #12
      t367 = -t216 + t218 - t220 - t224 + t228 - 0.4D1 * t230 + 0.6D1 * 
     #t237 - t242 + t247 - 0.4D1 * t251 - t257 - t261 - t268 - t271 + t2
     #72 - t279
      t376 = -t284 - t290 - t295 + t300 + t306 + 0.16D2 * t309 - 0.16D2 
     #* t314 - 0.14D2 * t318 - 0.22D2 * t323 + 0.14D2 * t328 - 0.14D2 * 
     #t334 + t340 - t342 + 0.6D1 * t345 - t349 + 0.16D2 * t350
      rrgg2ggh81J5 = -0.9D1 / 0.2D1 * (0.5D1 * wd * (t139 + t213 + t280 
     #+ t352) + 0.4D1 * wd * (t359 + t362 + t367 + t376)) / t1 / t38 / t
     #57 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh81J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t6 * t8
      t10 = 0.1D1 - x3
      t11 = t10 ** 2
      t14 = s * t4
      t15 = x1 * t4
      t16 = z + t15
      t17 = 0.1D1 / t16
      t18 = x1 * t17
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t24 = cos(x4 * 0.3141592653589793D1)
      t28 = Sqrt(t20 * t16 * x2 * t10)
      t30 = 0.2D1 * t24 * t28
      t31 = t20 * t16 + x2 * t10 - t30
      t34 = t7 * x3
      t36 = s - t14 * t18 * t31 - t14 * t34
      t40 = t10 * t19 * t16 + x2 * x3 + t30
      t41 = t18 * t40
      t43 = t7 * t10
      t45 = s - t14 * t41 - t14 * t43
      t46 = t36 * t45
      t50 = t46 * t2
      t51 = x1 ** 2
      t52 = t5 * t51
      t53 = t16 ** 2
      t54 = 0.1D1 / t53
      t55 = t31 ** 2
      t56 = t54 * t55
      t60 = t3 * t36
      t65 = t8 * t7
      t66 = t6 * t65
      t67 = x3 ** 2
      t69 = t66 * t10 * t67
      t73 = t8 * t67
      t78 = t11 * t10
      t83 = t17 * t31
      t87 = t51 * x1
      t88 = t6 * t87
      t90 = 0.1D1 / t53 / t16
      t91 = t55 * t31
      t92 = t90 * t91
      t96 = t2 * s
      t97 = t96 * z
      t99 = t31 * t36
      t103 = t51 * t54
      t104 = t55 * t36
      t107 = t45 * t96
      t108 = t107 * z
      t110 = t66 * t11 * x3
      t113 = t87 * t90
      t118 = t5 * t8
      t120 = t118 * t10 * x3
      t123 = t5 ** 2
      t124 = t51 ** 2
      t126 = t53 ** 2
      t128 = t40 ** 2
      t129 = t128 ** 2
      t135 = -0.2D1 * t3 * t9 * t11 * t46 * t41 + 0.12D2 * t50 * t52 * t
     #56 - 0.2D1 * t60 * t45 * t4 * t34 - 0.4D1 * t50 * t69 + 0.4D1 * t6
     #0 * t45 * t5 * t73 - 0.2D1 * t3 * t6 * t65 * t78 * t46 - 0.4D1 * t
     #50 * t15 * t83 - 0.4D1 * t50 * t88 * t92 - t97 * t4 * t18 * t99 - 
     #t97 * t5 * t103 * t104 - t108 * t110 - 0.2D1 * t97 * t6 * t113 * t
     #91 * t36 - 0.2D1 * t108 * t120 + 0.2D1 * t50 * t123 * t124 / t126 
     #* t129 - t108 * t69
      t136 = t97 * t88
      t137 = t90 * t55
      t138 = t36 * t40
      t141 = t6 * t51
      t142 = t141 * t54
      t145 = t40 * t7 * x3
      t149 = t3 * t66
      t155 = t6 * x1
      t163 = t2 * t6
      t164 = t46 * t163
      t170 = t5 * x1
      t172 = t34 * t36
      t173 = t83 * t172
      t178 = t3 * t46
      t180 = t17 * t40
      t184 = t103 * t128
      t185 = t43 * t184
      t196 = t3 * t88
      t197 = t90 * t31
      t217 = -t136 * t137 * t138 + 0.6D1 * t97 * t142 * t99 * t145 - 0.4
     #D1 * t149 * t10 * t36 * t45 * t67 - 0.4D1 * t3 * t155 * t17 * t99 
     #* t45 * t8 * t67 - 0.16D2 * t164 * t113 * t55 * t40 + 0.16D2 * t45
     # * t2 * t170 * t173 - 0.16D2 * t50 * t110 + 0.2D1 * t178 - 0.2D1 *
     # t178 * t15 * t180 - 0.4D1 * t164 * t185 + 0.6D1 * t97 * t170 * t1
     #73 + 0.14D2 * t3 * t46 * t5 * t18 * t145 - 0.4D1 * t196 * t197 * t
     #46 * t128 - 0.16D2 * t164 * t103 * t55 * t7 * x3 - 0.4D1 * t97 * t
     #155 * t83 * t36 * t8 * t67 - 0.4D1 * t164 * t113 * t31 * t128
      t220 = t54 * t31
      t227 = t180 * x3
      t231 = t66 * t78
      t235 = t4 * t7 * t10
      t239 = t118 * t11
      t249 = z * t6
      t250 = t107 * t249
      t252 = t8 * t11 * t41
      t255 = t8 ** 2
      t257 = t67 ** 2
      t269 = t46 * t2 * t123
      t278 = t3 * t142
      t284 = -0.2D1 * t97 * t52 * t220 * t138 - 0.14D2 * t3 * t9 * t10 *
     # t46 * x1 * t227 - 0.4D1 * t50 * t231 - t108 * t235 - 0.4D1 * t50 
     #* t235 + 0.12D2 * t50 * t239 - t108 * t239 - 0.2D1 * t108 * t231 +
     # 0.4D1 * t178 * t52 * t54 * t128 - 0.2D1 * t250 * t252 + 0.2D1 * t
     #50 * t123 * t255 * t257 - 0.2D1 * t149 * t11 * t36 * t45 * x3 - 0.
     #16D2 * t164 * t252 + 0.4D1 * t269 * t65 * t67 * x3 * t41 - 0.2D1 *
     # t196 * t92 * t46 - 0.2D1 * t278 * t104 * t45 * t7 * x3
      t297 = t46 * t2 * t5
      t298 = t43 * t41
      t326 = t10 * x1 * t227
      t351 = -0.4D1 * t3 * t6 * t7 * t10 * t46 * t184 + 0.4D1 * t269 * t
     #34 * t113 * t128 * t40 + 0.16D2 * t297 * t298 + 0.16D2 * t297 * t1
     #03 * t31 * t40 + 0.16D2 * t50 * t120 - 0.2D1 * t196 * t137 * t46 *
     # t40 + 0.6D1 * t107 * z * t5 * t298 - 0.4D1 * t164 * t18 * t31 * t
     #8 * t67 + 0.6D1 * t269 * t73 * t184 + 0.6D1 * t107 * t249 * t8 * t
     #326 - 0.4D1 * t250 * t185 - 0.2D1 * t97 * t141 * t56 * t172 - 0.14
     #D2 * t278 * t99 * t45 * t145 - t136 * t197 * t36 * t128 - 0.22D2 *
     # t46 * t163 * t8 * t326 - 0.22D2 * t46 * t163 * t51 * t220 * t145
      rrgg2ggh81J6 = -0.45D2 / 0.2D1 * wd * (t135 + t217 + t284 + t351) 
     #/ t1 / t36 / t45 / z / 0.3141592653589793D1

      end function
  
 