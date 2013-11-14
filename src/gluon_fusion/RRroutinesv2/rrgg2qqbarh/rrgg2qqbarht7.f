  
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t3 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t7 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = t7 * pi
      t10 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t11 = t10 * pi
      t12 = lh ** 2
      t14 = pi ** 2
      t16 = -0.180D3 * t12 + 0.30D2 * t14
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = x3 * t23
      t25 = x4 * pi
      t26 = Sin(t25)
      t27 = t26 ** 2
      t30 = log(0.4D1 * t24 * t27)
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t24 * t27 * t32)
      t38 = cos(t25)
      t40 = Sqrt(-x3 * t31)
      t45 = 0.1D1 / (-z - x3 + 0.2D1 * t38 * t40 * z)
      t49 = t36 ** 2
      t53 = t30 ** 2
      t67 = -0.60D2 * lh * t14 + 0.240D3 * zeta3 + 0.120D3 * t12 * lh
      t69 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t89 = 0.1D1 / x3
      t92 = t23 * t27
      t94 = log(0.4D1 * t92)
      t95 = t94 ** 2
      t98 = t95 * t94
      t118 = t14 ** 2
      t119 = t12 ** 2
      t130 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t131 = t95 ** 2
      t140 = pi * t20
      t141 = x1 ** 2
      t142 = x3 * t141
      t145 = log(0.4D1 * t142 * t92)
      t147 = t145 ** 2
      t150 = t92 * t32
      t153 = log(-0.4D1 * t142 * t150)
      t154 = t153 * z
      t156 = t153 ** 2
      t160 = z * t7
      t166 = pi * lh
      t168 = z * t3
      t176 = pi * t16
      t180 = t20 * (-z * t10 * t45 - t10)
      t181 = t176 * t180
      t184 = 0.1D1 / x1
      t187 = t141 * t23
      t188 = t187 * t27
      t190 = log(0.4D1 * t188)
      t195 = t190 ** 2
      t205 = pi * t67
      t206 = t20 * t10
      t207 = t205 * t206
      t218 = x2 ** 2
      t219 = t218 * x3
      t220 = t219 * t188
      t222 = log(0.4D1 * t220)
      t224 = t219 * t141
      t227 = log(-0.4D1 * t224 * t150)
      t239 = 0.1D1 / x2
      t240 = t239 * t184
      t243 = t218 * t141
      t246 = log(0.4D1 * t243 * t92)
      t248 = t246 ** 2
      t266 = log(0.4D1 * t219 * t92)
      t268 = t266 ** 2
      t273 = log(-0.4D1 * t219 * t150)
      t274 = t273 * z
      t276 = t273 ** 2
      t297 = t218 * t23
      t300 = log(0.4D1 * t297 * t27)
      t305 = t300 ** 2
      t325 = ((0.180D3 * t4 * lh - 0.90D2 * t8 + t11 * t16) * t20 * (-t3
     #0 - t36 * z * t45) - 0.90D2 * t11 * t20 * (-t49 * t36 * z * t45 / 
     #0.6D1 - t53 * t30 / 0.6D1) + (0.180D3 * t8 * lh + t11 * t67 - 0.90
     #D2 * t69 * pi + t4 * t16) * t20 * (0.1D1 + z * t45) + (0.180D3 * t
     #11 * lh - 0.90D2 * t4) * t20 * (t53 / 0.2D1 + t49 * z * t45 / 0.2D
     #1)) * t89 / 0.2880D4 + (0.180D3 * (t95 * t3 / 0.2D1 + t69 - t98 * 
     #t10 / 0.6D1 - t94 * t7) * pi * lh + (-t94 * t3 + t95 * t10 / 0.2D1
     # + t7) * pi * t16 + (t3 - t94 * t10) * pi * t67 + t11 * (-0.480D3 
     #* lh * zeta3 - t118 - 0.60D2 * t119 + 0.60D2 * t12 * t14) - 0.90D2
     # * (-t98 * t3 / 0.6D1 + t95 * t7 / 0.2D1 - t94 * t69 + t130 + t131
     # * t10 / 0.24D2) * pi) * t20 / 0.2880D4 - (-0.90D2 * t140 * (t145 
     #* t3 - t147 * t10 / 0.2D1 - t7 - (-t154 * t3 + t156 * z * t10 / 0.
     #2D1 + t160) * t45) + 0.180D3 * t166 * t20 * (-t3 + t145 * t10 - (t
     #168 - t154 * t10) * t45) + t181) * t89 * t184 / 0.1440D4 - (t176 *
     # t20 * (-t3 + t190 * t10) - 0.90D2 * t140 * (-t195 * t3 / 0.2D1 - 
     #t69 + t195 * t190 * t10 / 0.6D1 + t190 * t7) - t207 + 0.180D3 * t1
     #66 * t20 * (t190 * t3 - t195 * t10 / 0.2D1 - t7)) * t184 / 0.1440D
     #4 - (-0.90D2 * t140 * (-t3 + t222 * t10 - (t168 - t227 * z * t10) 
     #* t45) + 0.180D3 * t166 * t180) * t89 * t240 / 0.720D3 - (-0.90D2 
     #* t140 * (t246 * t3 - t248 * t10 / 0.2D1 - t7) + 0.180D3 * t166 * 
     #t20 * (-t3 + t246 * t10) - t176 * t206) * t239 * t184 / 0.720D3 - 
     #(-0.90D2 * t140 * (t266 * t3 - t268 * t10 / 0.2D1 - t7 - (-t274 * 
     #t3 + t276 * z * t10 / 0.2D1 + t160) * t45) + 0.180D3 * t166 * t20 
     #* (-t3 + t266 * t10 - (t168 - t274 * t10) * t45) + t181) * t89 * t
     #239 / 0.1440D4 + (t176 * t20 * (t3 - t300 * t10) - 0.90D2 * t140 *
     # (t305 * t3 / 0.2D1 + t69 - t305 * t300 * t10 / 0.6D1 - t300 * t7)
     # + t207 + 0.180D3 * t166 * t20 * (-t300 * t3 + t305 * t10 / 0.2D1 
     #+ t7)) * t239 / 0.1440D4
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
      t344 = t142 * t23
      t345 = t333 ** 2
      t346 = t27 * t345
      t348 = t346 * t336 * t32
      t351 = log(-0.4D1 * t344 * t348)
      t352 = t351 * z
      t353 = t142 * t22
      t354 = x1 * t22
      t355 = x3 * t22
      t356 = t355 * x1
      t358 = 0.2D1 * t142 * z
      t359 = x3 * x1
      t360 = t359 * z
      t361 = 0.3D1 * t360
      t362 = x3 * t335
      t364 = Sqrt(-t362 * t31)
      t368 = 0.2D1 * t359
      t369 = -z - t353 - t354 + t356 + t358 + t334 - t361 + 0.2D1 * t38 
     #* t364 * z - x3 - t142 + t368
      t370 = 0.1D1 / t369
      t371 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338
     #, t332, 0.0D0, -t343)
      t374 = t351 ** 2
      t376 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338
     #, t332, 0.0D0, -t343)
      t377 = t370 * t376
      t380 = z * t370
      t381 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338
     #, t332, 0.0D0, -t343)
      t385 = t346 * t336
      t388 = log(0.4D1 * t344 * t385)
      t390 = t388 ** 2
      t396 = t380 * t371
      t408 = t20 * (t380 * t376 * t335 + t376)
      t416 = log(0.4D1 * t187 * t385)
      t421 = t416 ** 2
      t424 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t338
     #, t332, 0.0D0, -t343)
      t432 = t20 * t376
      t444 = t219 * t187
      t447 = log(-0.4D1 * t444 * t348)
      t452 = t345 * t336
      t456 = log(0.4D1 * t224 * t92 * t452)
      t467 = t243 * t23
      t470 = log(0.4D1 * t467 * t385)
      t472 = t470 ** 2
      t488 = -(-0.90D2 * t140 * ((-t352 * t370 * t371 + t374 * z * t377 
     #/ 0.2D1 + t380 * t381) * t335 - t388 * t371 + t390 * t376 / 0.2D1 
     #+ t381) + 0.180D3 * t166 * t20 * ((t396 - t352 * t377) * t335 + t3
     #71 - t388 * t376) + t176 * t408) * t89 * t184 / 0.1440D4 - (t176 *
     # t20 * (t371 - t416 * t376) - 0.90D2 * t140 * (t421 * t371 / 0.2D1
     # + t424 - t421 * t416 * t376 / 0.6D1 - t416 * t381) + t205 * t432 
     #+ 0.180D3 * t166 * t20 * (-t416 * t371 + t421 * t376 / 0.2D1 + t38
     #1)) * t184 / 0.1440D4 - (-0.90D2 * t140 * ((t396 - t447 * z * t377
     #) * t335 + t371 - t456 * t376) + 0.180D3 * t166 * t408) * t89 * t2
     #40 / 0.720D3 - (-0.90D2 * t140 * (-t470 * t371 + t472 * t376 / 0.2
     #D1 + t381) + 0.180D3 * t166 * t20 * (t371 - t470 * t376) + t176 * 
     #t432) * t239 * t184 / 0.720D3
      t489 = FJET(XB1, XB2, s, 0.0D0, t332, -t338, 0.0D0, -t343, t488)
      t492 = x2 * t1 * s
      t493 = -0.1D1 + x2
      t495 = t493 * t1 * s
      t496 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t492, -t495,
     # 0.0D0, 0.0D0, 0.0D0)
      t497 = t92 * t493
      t500 = log(-0.4D1 * t224 * t497)
      t501 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t492, -t495,
     # 0.0D0, 0.0D0, 0.0D0)
      t506 = t20 * t501
      t512 = (-0.90D2 * t140 * (t496 - t500 * t501) + 0.180D3 * t166 * t
     #506) * t89 * t240 / 0.720D3
      t515 = log(-0.4D1 * t243 * t497)
      t517 = t515 ** 2
      t520 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t492, -t495,
     # 0.0D0, 0.0D0, 0.0D0)
      t529 = t176 * t506
      t533 = (-0.90D2 * t140 * (-t515 * t496 + t517 * t501 / 0.2D1 + t52
     #0) + 0.180D3 * t166 * t20 * (t496 - t515 * t501) + t529) * t239 * 
     #t184 / 0.720D3
      t536 = log(-0.4D1 * t219 * t497)
      t538 = t536 ** 2
      t552 = (-0.90D2 * t140 * (-t536 * t496 + t538 * t501 / 0.2D1 + t52
     #0) + 0.180D3 * t166 * t20 * (t496 - t536 * t501) + t529) * t89 * t
     #239 / 0.1440D4
      t553 = t27 * t493
      t556 = log(-0.4D1 * t297 * t553)
      t558 = -t496 + t556 * t501
      t561 = t556 ** 2
      t564 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, t492, -t495,
     # 0.0D0, 0.0D0, 0.0D0)
      t569 = -t561 * t496 / 0.2D1 - t564 + t561 * t556 * t501 / 0.6D1 + 
     #t556 * t520
      t572 = t205 * t506
      t576 = t556 * t496 - t561 * t501 / 0.2D1 - t520
      t583 = -t512 - t533 - t552 + (t176 * t20 * t558 - 0.90D2 * t140 * 
     #t569 - t572 + 0.180D3 * t166 * t20 * t576) * t239 / 0.1440D4
      t584 = FJET(XB1, XB2, s, 0.0D0, t492, 0.0D0, -t495, 0.0D0, t583)
      t586 = x2 * x3
      t589 = Sqrt(x3 * t493 * t31)
      t590 = t38 * t589
      t592 = 0.2D1 * t590 * x2
      t594 = 0.1D1 - x3 + t586
      t595 = 0.1D1 / t594
      t597 = t2 * (0.1D1 - x3 - x2 + t586 + t219 + t592) * t595
      t602 = t2 * x2 * (-0.1D1 + t586 + 0.2D1 * t590) * t595
      t603 = x2 * z
      t604 = -x2 + t603 - z
      t605 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t602, t597,
     # 0.0D0, 0.0D0, 0.0D0)
      t606 = t604 * t605
      t607 = t594 ** 2
      t608 = 0.1D1 / t607
      t610 = t553 * t31 * t608
      t613 = log(0.4D1 * t444 * t610)
      t615 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t602, t597,
     # 0.0D0, 0.0D0, 0.0D0)
      t618 = t586 * z
      t619 = t219 * z
      t625 = 0.1D1 / (x2 - t603 - t618 + t619 + z + x3 - t219 - t592 - 0
     #.2D1 * t590 * z + 0.2D1 * t590 * t603)
      t629 = t166 * t20
      t631 = t604 * t615 * t625
      t641 = log(0.4D1 * t219 * t23 * t610)
      t642 = t641 * t604
      t644 = t641 ** 2
      t648 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t602, t597,
     # 0.0D0, 0.0D0, 0.0D0)
      t666 = -(0.90D2 * t140 * (-t606 + t613 * t604 * t615) * t625 + 0.1
     #80D3 * t629 * t631) * t89 * t240 / 0.720D3 - (0.90D2 * t140 * (t64
     #2 * t605 - t644 * t604 * t615 / 0.2D1 - t604 * t648) * t625 - 0.18
     #0D3 * t166 * t20 * (-t606 + t642 * t615) * t625 + t176 * t20 * t63
     #1) * t89 * t239 / 0.1440D4
      t667 = FJET(XB1, XB2, s, 0.0D0, t597, 0.0D0, -t602, 0.0D0, t666)
      t670 = t1 * t333
      t672 = t493 * s * t670 * t336
      t674 = x2 * s * t670
      t676 = t340 * t493 * t342
      t677 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t674, t672,
     # t332, 0.0D0, t676)
      t679 = t346 * t336 * t493
      t682 = log(-0.4D1 * t444 * t679)
      t683 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t674, t672,
     # t332, 0.0D0, t676)
      t688 = t20 * t683
      t693 = (-0.90D2 * t140 * (-t677 + t682 * t683) - 0.180D3 * t166 * 
     #t688) * t89 * t240
      t696 = log(-0.4D1 * t467 * t679)
      t698 = t696 ** 2
      t701 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t674, t672,
     # t332, 0.0D0, t676)
      t702 = t696 * t677 - t698 * t683 / 0.2D1 - t701
      t706 = -t677 + t696 * t683
      t710 = t176 * t688
      t715 = -t693 / 0.720D3 - (-0.90D2 * t140 * t702 + 0.180D3 * t166 *
     # t20 * t706 - t710) * t239 * t184 / 0.720D3
      t716 = FJET(XB1, XB2, s, 0.0D0, t672, t332, -t674, t676, t715)
      t718 = FJET(XB1, XB2, s, 0.0D0, -t495, 0.0D0, t492, 0.0D0, t583)
      t720 = FJET(XB1, XB2, s, 0.0D0, -t338, t332, 0.0D0, -t343, t488)
      t722 = FJET(XB1, XB2, s, 0.0D0, -t602, 0.0D0, t597, 0.0D0, t666)
      t724 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t325)
      t726 = FJET(XB1, XB2, s, t332, 0.0D0, 0.0D0, -t338, -t343, t488)
      t728 = t326 * t325 + t328 * t325 + t330 * t325 + t489 * t488 + t58
     #4 * t583 + t667 * t666 + t716 * t715 + t718 * t583 + t720 * t488 +
     # t722 * t666 + t724 * t325 + t726 * t488
      t729 = FJET(XB1, XB2, s, t332, -t674, 0.0D0, t672, t676, t715)
      t744 = -t512 - t533 - t552 + (t176 * t20 * t558 - 0.90D2 * t140 * 
     #t569 - t572 + 0.180D3 * t166 * t20 * t576) * t239 / 0.1440D4
      t745 = FJET(XB1, XB2, s, t492, 0.0D0, -t495, 0.0D0, 0.0D0, t744)
      t747 = FJET(XB1, XB2, s, t597, 0.0D0, -t602, 0.0D0, 0.0D0, t666)
      t749 = FJET(XB1, XB2, s, t672, 0.0D0, -t674, t332, t676, t715)
      t752 = t332 * t586 * t595
      t753 = t2 * t333
      t754 = t219 * t334
      t755 = t219 * x1
      t756 = t493 * t31
      t758 = Sqrt(t362 * t756)
      t759 = t38 * t758
      t761 = 0.2D1 * t759 * x2
      t765 = t753 * (t754 - x2 + t586 - t755 + t761 + 0.1D1 - x3 + t219)
     # * t336 * t595
      t769 = t31 * s * t1 * x1 * t595
      t775 = t753 * x2 * (-0.1D1 + t586 + x1 - t359 - t334 + t360 + 0.2D
     #1 * t759) * t336 * t595
      t776 = x2 * x1
      t777 = t776 * z
      t778 = z - t603 + t777 + x2 - t776
      t779 = t778 * t335
      t783 = t22 * x2
      t785 = t141 * x2
      t795 = t618 - t619 - t353 + t356 + t358 - t361 + 0.2D1 * t759 * t7
     #77 - t755 + t761 - 0.3D1 * t777 + t783 * x1 + 0.2D1 * t785 * z - t
     #141 * t22 * x2 + 0.2D1 * t759 * z + t142 * x2 - t586 * x1 + 0.2D1 
     #* t776
      t806 = -t785 + t219 + t603 + t754 + 0.2D1 * t586 * t334 - 0.2D1 * 
     #t142 * t603 - t355 * t776 + t142 * t783 - 0.2D1 * t759 * t603 - 0.
     #2D1 * t759 * t776 - x2 - x3 - z + t334 - t354 - t142 + t368
      t808 = 0.1D1 / (t795 + t806)
      t809 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t775, -t765,
     # -t769, t752, t676)
      t816 = log(0.4D1 * t220 * t452 * t756 * t608)
      t819 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t775, -t765,
     # -t769, t752, t676)
      t829 = -0.90D2 * t140 * (-t779 * t808 * t809 + t816 * t778 * t335 
     #* t808 * t819) - 0.180D3 * t629 * t779 * t808 * t819
      t832 = t829 * t89 * t240 / 0.720D3
      t833 = FJET(XB1, XB2, s, t752, -t765, -t769, t775, t676, -t832)
      t836 = t89 * t239 * t184
      t839 = FJET(XB1, XB2, s, t775, -t769, -t765, t752, t676, -t832)
      t843 = FJET(XB1, XB2, s, -t495, 0.0D0, t492, 0.0D0, 0.0D0, t744)
      t845 = FJET(XB1, XB2, s, -t338, 0.0D0, 0.0D0, t332, -t343, t488)
      t858 = -t693 / 0.720D3 - (-0.90D2 * t140 * t702 + 0.180D3 * t166 *
     # t20 * t706 - t710) * t239 * t184 / 0.720D3
      t859 = FJET(XB1, XB2, s, -t674, t332, t672, 0.0D0, t676, t858)
      t861 = FJET(XB1, XB2, s, -t602, 0.0D0, t597, 0.0D0, 0.0D0, t666)
      t863 = FJET(XB1, XB2, s, -t769, t775, t752, -t765, t676, -t832)
      t867 = FJET(XB1, XB2, s, -t765, t752, t775, -t769, t676, -t832)
      t871 = t729 * t715 + t745 * t744 + t747 * t666 + t749 * t715 - t83
     #3 * t829 * t836 / 0.720D3 - t839 * t829 * t836 / 0.720D3 + t843 * 
     #t744 + t845 * t488 + t859 * t858 + t861 * t666 - t863 * t829 * t83
     #6 / 0.720D3 - t867 * t829 * t836 / 0.720D3
      rrgg2qqbarht7s1e1 = t728 + t871

      end function



      doubleprecision function rrgg2qqbarht7s1e0
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t3 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = z ** 2
      t8 = 0.1D1 / t7
      t9 = x3 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t15 = log(0.4D1 * t9 * t12)
      t16 = t15 ** 2
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t22 = log(-0.4D1 * t9 * t12 * t18)
      t23 = t22 ** 2
      t25 = cos(t10)
      t27 = Sqrt(-x3 * t17)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t41 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t42 = t41 * pi
      t52 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t55 = lh ** 2
      t57 = pi ** 2
      t59 = -0.180D3 * t55 + 0.30D2 * t57
      t67 = 0.1D1 / x3
      t70 = t8 * t12
      t72 = log(0.4D1 * t70)
      t74 = t72 ** 2
      t90 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t105 = pi * t6
      t106 = x2 ** 2
      t107 = t106 * x3
      t110 = log(0.4D1 * t107 * t70)
      t112 = z * t41
      t113 = t70 * t18
      t116 = log(-0.4D1 * t107 * t113)
      t124 = pi * lh
      t127 = -z * t3 * t32 - t3
      t130 = 0.180D3 * t124 * t6 * t127
      t133 = 0.1D1 / x2
      t136 = t106 * t8
      t139 = log(0.4D1 * t136 * t12)
      t141 = t139 ** 2
      t152 = pi * t59
      t153 = t6 * t3
      t154 = t152 * t153
      t158 = x1 ** 2
      t159 = x3 * t158
      t162 = log(0.4D1 * t159 * t70)
      t166 = log(-0.4D1 * t159 * t113)
      t176 = 0.1D1 / x1
      t181 = t67 * t133 * t176
      t184 = t106 * t158
      t187 = log(0.4D1 * t184 * t70)
      t198 = t158 * t8
      t201 = log(0.4D1 * t198 * t12)
      t203 = t201 ** 2
      t217 = (-0.90D2 * t4 * t6 * (t16 / 0.2D1 + t23 * z * t32 / 0.2D1) 
     #+ (0.180D3 * t4 * lh - 0.90D2 * t42) * t6 * (-t15 - t22 * z * t32)
     # + (0.180D3 * t42 * lh - 0.90D2 * t52 * pi + t4 * t59) * t6 * (0.1
     #D1 + z * t32)) * t67 / 0.2880D4 + (0.180D3 * (-t72 * t41 + t74 * t
     #3 / 0.2D1 + t52) * pi * lh + t4 * (-0.60D2 * lh * t57 + 0.240D3 * 
     #zeta3 + 0.120D3 * t55 * lh) - 0.90D2 * (t74 * t41 / 0.2D1 + t90 - 
     #t74 * t72 * t3 / 0.6D1 - t72 * t52) * pi + (t41 - t72 * t3) * pi *
     # t59) * t6 / 0.2880D4 - (-0.90D2 * t105 * (-t41 + t110 * t3 - (t11
     #2 - t116 * z * t3) * t32) + t130) * t67 * t133 / 0.1440D4 + (-0.90
     #D2 * t105 * (-t139 * t41 + t141 * t3 / 0.2D1 + t52) + 0.180D3 * t1
     #24 * t6 * (t41 - t139 * t3) + t154) * t133 / 0.1440D4 - (-0.90D2 *
     # t105 * (-t41 + t162 * t3 - (t112 - t166 * z * t3) * t32) + t130) 
     #* t67 * t176 / 0.1440D4 + t105 * t127 * t181 / 0.8D1 - (-0.90D2 * 
     #t105 * (-t41 + t187 * t3) - 0.180D3 * t124 * t153) * t133 * t176 /
     # 0.720D3 - (-0.90D2 * t105 * (t201 * t41 - t203 * t3 / 0.2D1 - t52
     #) + 0.180D3 * t124 * t6 * (-t41 + t201 * t3) - t154) * t176 / 0.14
     #40D4
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
      t236 = t159 * t7
      t237 = x1 * t7
      t238 = x3 * t7
      t239 = t238 * x1
      t241 = 0.2D1 * t159 * z
      t242 = x3 * x1
      t243 = t242 * z
      t244 = 0.3D1 * t243
      t245 = x3 * t227
      t247 = Sqrt(-t245 * t17)
      t251 = 0.2D1 * t242
      t252 = -z - t236 - t237 + t239 + t241 + t226 - t244 + 0.2D1 * t25 
     #* t247 * z - x3 - t159 + t251
      t253 = 0.1D1 / t252
      t254 = z * t253
      t255 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t230
     #, t224, 0.0D0, -t235)
      t257 = t159 * t8
      t258 = t225 ** 2
      t259 = t12 * t258
      t264 = log(-0.4D1 * t257 * t259 * t228 * t18)
      t266 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t230
     #, t224, 0.0D0, -t235)
      t271 = t259 * t228
      t274 = log(0.4D1 * t257 * t271)
      t281 = t254 * t266 * t227 + t266
      t292 = t184 * t8
      t295 = log(0.4D1 * t292 * t271)
      t300 = t6 * t266
      t309 = log(0.4D1 * t198 * t271)
      t311 = t309 ** 2
      t314 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t230
     #, t224, 0.0D0, -t235)
      t327 = -(-0.90D2 * t105 * ((t254 * t255 - t264 * z * t253 * t266) 
     #* t227 + t255 - t274 * t266) + 0.180D3 * t124 * t6 * t281) * t67 *
     # t176 / 0.1440D4 + t105 * t281 * t181 / 0.8D1 - (-0.90D2 * t105 * 
     #(t255 - t295 * t266) + 0.180D3 * t124 * t300) * t133 * t176 / 0.72
     #0D3 - (-0.90D2 * t105 * (-t309 * t255 + t311 * t266 / 0.2D1 + t314
     #) + 0.180D3 * t124 * t6 * (t255 - t309 * t266) + t152 * t300) * t1
     #76 / 0.1440D4
      t328 = FJET(XB1, XB2, s, 0.0D0, t224, -t230, 0.0D0, -t235, t327)
      t331 = x2 * t1 * s
      t332 = -0.1D1 + x2
      t334 = t332 * t1 * s
      t335 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334,
     # 0.0D0, 0.0D0, 0.0D0)
      t336 = t70 * t332
      t339 = log(-0.4D1 * t107 * t336)
      t340 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334,
     # 0.0D0, 0.0D0, 0.0D0)
      t345 = t6 * t340
      t347 = 0.180D3 * t124 * t345
      t351 = (-0.90D2 * t105 * (t335 - t339 * t340) + t347) * t67 * t133
     # / 0.1440D4
      t352 = t12 * t332
      t355 = log(-0.4D1 * t136 * t352)
      t357 = t355 ** 2
      t360 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334,
     # 0.0D0, 0.0D0, 0.0D0)
      t361 = t355 * t335 - t357 * t340 / 0.2D1 - t360
      t365 = -t335 + t355 * t340
      t369 = t152 * t345
      t375 = t105 * t340 * t181 / 0.8D1
      t378 = log(-0.4D1 * t184 * t336)
      t386 = (-0.90D2 * t105 * (t335 - t378 * t340) + t347) * t133 * t17
     #6 / 0.720D3
      t387 = -t351 + (-0.90D2 * t105 * t361 + 0.180D3 * t124 * t6 * t365
     # - t369) * t133 / 0.1440D4 + t375 - t386
      t388 = FJET(XB1, XB2, s, 0.0D0, t331, 0.0D0, -t334, 0.0D0, t387)
      t390 = x2 * x3
      t393 = Sqrt(x3 * t332 * t17)
      t394 = t25 * t393
      t396 = 0.2D1 * t394 * x2
      t398 = 0.1D1 - x3 + t390
      t399 = 0.1D1 / t398
      t401 = t2 * (0.1D1 - x3 - x2 + t390 + t107 + t396) * t399
      t406 = t2 * x2 * (-0.1D1 + t390 + 0.2D1 * t394) * t399
      t407 = x2 * z
      t408 = -x2 + t407 - z
      t409 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t406, t401,
     # 0.0D0, 0.0D0, 0.0D0)
      t412 = t398 ** 2
      t418 = log(0.4D1 * t107 * t8 * t352 * t17 / t412)
      t420 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t406, t401,
     # 0.0D0, 0.0D0, 0.0D0)
      t423 = t390 * z
      t424 = t107 * z
      t430 = 0.1D1 / (x2 - t407 - t423 + t424 + z + x3 - t107 - t396 - 0
     #.2D1 * t394 * z + 0.2D1 * t394 * t407)
      t435 = t408 * t420
      t449 = -(0.90D2 * t105 * (-t408 * t409 + t418 * t408 * t420) * t43
     #0 + 0.180D3 * t124 * t6 * t435 * t430) * t67 * t133 / 0.1440D4 + t
     #105 * t435 * t430 * t67 * t133 * t176 / 0.8D1
      t450 = FJET(XB1, XB2, s, 0.0D0, t401, 0.0D0, -t406, 0.0D0, t449)
      t453 = t1 * t225
      t455 = t332 * s * t453 * t228
      t457 = x2 * s * t453
      t459 = t232 * t332 * t234
      t460 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t457, t455,
     # t224, 0.0D0, t459)
      t463 = t105 * t460 * t181 / 0.8D1
      t464 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t457, t455,
     # t224, 0.0D0, t459)
      t470 = log(-0.4D1 * t292 * t12 * t228 * t258 * t332)
      t472 = -t464 + t470 * t460
      t477 = 0.180D3 * t124 * t6 * t460
      t482 = -t463 - (-0.90D2 * t105 * t472 - t477) * t133 * t176 / 0.72
     #0D3
      t483 = FJET(XB1, XB2, s, 0.0D0, t455, t224, -t457, t459, t482)
      t485 = FJET(XB1, XB2, s, 0.0D0, -t334, 0.0D0, t331, 0.0D0, t387)
      t487 = FJET(XB1, XB2, s, 0.0D0, -t230, t224, 0.0D0, -t235, t327)
      t489 = FJET(XB1, XB2, s, 0.0D0, -t406, 0.0D0, t401, 0.0D0, t449)
      t491 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t217)
      t493 = FJET(XB1, XB2, s, t224, 0.0D0, 0.0D0, -t230, -t235, t327)
      t495 = t218 * t217 + t220 * t217 + t222 * t217 + t328 * t327 + t38
     #8 * t387 + t450 * t449 + t483 * t482 + t485 * t387 + t487 * t327 +
     # t489 * t449 + t491 * t217 + t493 * t327
      t496 = FJET(XB1, XB2, s, t224, -t457, 0.0D0, t455, t459, t482)
      t508 = -t351 + (-0.90D2 * t105 * t361 + 0.180D3 * t124 * t6 * t365
     # - t369) * t133 / 0.1440D4 + t375 - t386
      t509 = FJET(XB1, XB2, s, t331, 0.0D0, -t334, 0.0D0, 0.0D0, t508)
      t511 = FJET(XB1, XB2, s, t401, 0.0D0, -t406, 0.0D0, 0.0D0, t449)
      t513 = FJET(XB1, XB2, s, t455, 0.0D0, -t457, t224, t459, t482)
      t516 = t224 * t390 * t399
      t517 = t2 * t225
      t518 = t107 * t226
      t519 = t107 * x1
      t522 = Sqrt(t245 * t332 * t17)
      t523 = t25 * t522
      t525 = 0.2D1 * t523 * x2
      t529 = t517 * (t518 - x2 + t390 - t519 + t525 + 0.1D1 - x3 + t107)
     # * t228 * t399
      t533 = t17 * s * t1 * x1 * t399
      t539 = t517 * x2 * (-0.1D1 + t390 + x1 - t242 - t226 + t243 + 0.2D
     #1 * t523) * t228 * t399
      t540 = x2 * x1
      t541 = t540 * z
      t542 = z - t407 + t541 + x2 - t540
      t546 = t158 * x2
      t548 = t7 * x2
      t558 = t423 - t424 - t236 + t239 + t241 - t244 + 0.2D1 * t540 - t5
     #46 - t519 + t525 - 0.3D1 * t541 + t548 * x1 + 0.2D1 * t546 * z - t
     #158 * t7 * x2 + 0.2D1 * t523 * z + t159 * x2 - t390 * x1
      t571 = -x2 - 0.2D1 * t523 * t407 - 0.2D1 * t523 * t540 + t107 + t4
     #07 + 0.2D1 * t523 * t541 + t518 + 0.2D1 * t390 * t226 - 0.2D1 * t1
     #59 * t407 - t238 * t540 + t159 * t548 + t226 - t237 - t159 + t251 
     #- z - x3
      t574 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t539, -t529,
     # -t533, t516, t459)
      t576 = 0.1D1 / (t558 + t571) * t574 * t181
      t578 = t105 * t542 * t227 * t576 / 0.8D1
      t579 = FJET(XB1, XB2, s, t516, -t529, -t533, t539, t459, -t578)
      t582 = t6 * t542 * t227
      t586 = FJET(XB1, XB2, s, t539, -t533, -t529, t516, t459, -t578)
      t591 = FJET(XB1, XB2, s, -t334, 0.0D0, t331, 0.0D0, 0.0D0, t508)
      t593 = FJET(XB1, XB2, s, -t230, 0.0D0, 0.0D0, t224, -t235, t327)
      t602 = -t463 - (-0.90D2 * t105 * t472 - t477) * t133 * t176 / 0.72
     #0D3
      t603 = FJET(XB1, XB2, s, -t457, t224, t455, 0.0D0, t459, t602)
      t605 = FJET(XB1, XB2, s, -t406, 0.0D0, t401, 0.0D0, 0.0D0, t449)
      t607 = FJET(XB1, XB2, s, -t533, t539, t516, -t529, t459, -t578)
      t612 = FJET(XB1, XB2, s, -t529, t516, t539, -t533, t459, -t578)
      t617 = t496 * t482 + t509 * t508 + t511 * t449 + t513 * t482 - t57
     #9 * pi * t582 * t576 / 0.8D1 - t586 * pi * t582 * t576 / 0.8D1 + t
     #591 * t508 + t593 * t327 + t603 * t602 + t605 * t449 - t607 * pi *
     # t582 * t576 / 0.8D1 - t612 * pi * t582 * t576 / 0.8D1
      rrgg2qqbarht7s1e0 = t495 + t617

      end function



      doubleprecision function rrgg2qqbarht7s1em1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t3 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = z ** 2
      t8 = 0.1D1 / t7
      t9 = x3 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t15 = log(0.4D1 * t9 * t12)
      t16 = -0.1D1 + x3
      t21 = log(-0.4D1 * t9 * t12 / t16)
      t23 = cos(t10)
      t25 = Sqrt(-x3 * t16)
      t30 = 0.1D1 / (-z - x3 + 0.2D1 * t23 * t25 * z)
      t38 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t47 = 0.1D1 / x3
      t52 = log(0.4D1 * t8 * t12)
      t59 = t52 ** 2
      t62 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t66 = lh ** 2
      t68 = pi ** 2
      t75 = pi * t6
      t79 = (-z * t3 * t30 - t3) * t47
      t80 = 0.1D1 / x2
      t84 = x2 ** 2
      t85 = t84 * t8
      t88 = log(0.4D1 * t85 * t12)
      t93 = pi * lh
      t96 = 0.180D3 * t93 * t6 * t3
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t8
      t109 = log(0.4D1 * t106 * t12)
      t120 = (-0.90D2 * t4 * t6 * (-t15 - t21 * z * t30) + (0.180D3 * t4
     # * lh - 0.90D2 * t38 * pi) * t6 * (0.1D1 + z * t30)) * t47 / 0.288
     #0D4 + (0.180D3 * (t38 - t52 * t3) * pi * lh - 0.90D2 * (-t52 * t38
     # + t59 * t3 / 0.2D1 + t62) * pi + t4 * (-0.180D3 * t66 + 0.30D2 * 
     #t68)) * t6 / 0.2880D4 + t75 * t79 * t80 / 0.16D2 + (-0.90D2 * t75 
     #* (t38 - t88 * t3) + t96) * t80 / 0.1440D4 - t4 * t6 * t80 * t101 
     #/ 0.8D1 - (-0.90D2 * t75 * (-t38 + t109 * t3) - t96) * t101 / 0.14
     #40D4 + t75 * t79 * t101 / 0.16D2
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
      t139 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t133
     #, t127, 0.0D0, -t138)
      t144 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t133
     #, t127, 0.0D0, -t138)
      t146 = t128 ** 2
      t150 = log(0.4D1 * t106 * t12 * t131 * t146)
      t161 = x3 * t105
      t168 = x3 * x1
      t173 = Sqrt(-x3 * t130 * t16)
      t178 = -z - t161 * t7 - x1 * t7 + x3 * t7 * x1 + 0.2D1 * t161 * z 
     #+ t129 - 0.3D1 * t168 * z + 0.2D1 * t23 * t173 * z - x3 - t161 + 0
     #.2D1 * t168
      t188 = t75 * t139 * t80 * t101 / 0.8D1 - (-0.90D2 * t75 * (t144 - 
     #t150 * t139) + 0.180D3 * t93 * t6 * t139) * t101 / 0.1440D4 + t75 
     #* (z / t178 * t139 * t130 + t139) * t47 * t101 / 0.16D2
      t189 = FJET(XB1, XB2, s, 0.0D0, t127, -t133, 0.0D0, -t138, t188)
      t192 = x2 * t1 * s
      t193 = -0.1D1 + x2
      t195 = t193 * t1 * s
      t196 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t192, -t195,
     # 0.0D0, 0.0D0, 0.0D0)
      t200 = t75 * t196 * t47 * t80 / 0.16D2
      t201 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t192, -t195,
     # 0.0D0, 0.0D0, 0.0D0)
      t205 = log(-0.4D1 * t85 * t12 * t193)
      t207 = -t201 + t205 * t196
      t212 = 0.180D3 * t93 * t6 * t196
      t219 = t75 * t196 * t80 * t101 / 0.8D1
      t220 = t200 + (-0.90D2 * t75 * t207 - t212) * t80 / 0.1440D4 + t21
     #9
      t221 = FJET(XB1, XB2, s, 0.0D0, t192, 0.0D0, -t195, 0.0D0, t220)
      t223 = x2 * x3
      t224 = t84 * x3
      t227 = Sqrt(x3 * t193 * t16)
      t228 = t23 * t227
      t230 = 0.2D1 * t228 * x2
      t233 = 0.1D1 / (0.1D1 - x3 + t223)
      t235 = t2 * (0.1D1 - x3 - x2 + t223 + t224 + t230) * t233
      t240 = t2 * x2 * (-0.1D1 + t223 + 0.2D1 * t228) * t233
      t241 = x2 * z
      t242 = -x2 + t241 - z
      t244 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t240, t235,
     # 0.0D0, 0.0D0, 0.0D0)
      t255 = t244 / (x2 - t241 - t223 * z + t224 * z + z + x3 - t224 - t
     #230 - 0.2D1 * t228 * z + 0.2D1 * t228 * t241) * t47 * t80
      t257 = t75 * t242 * t255 / 0.16D2
      t258 = FJET(XB1, XB2, s, 0.0D0, t235, 0.0D0, -t240, 0.0D0, t257)
      t260 = t6 * t242
      t265 = t1 * t128
      t267 = t193 * s * t265 * t131
      t269 = x2 * s * t265
      t271 = t135 * t193 * t137
      t272 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t269, t267,
     # t127, 0.0D0, t271)
      t274 = t272 * t80 * t101
      t276 = t75 * t274 / 0.8D1
      t277 = FJET(XB1, XB2, s, 0.0D0, t267, t127, -t269, t271, -t276)
      t282 = FJET(XB1, XB2, s, 0.0D0, -t195, 0.0D0, t192, 0.0D0, t220)
      t284 = FJET(XB1, XB2, s, 0.0D0, -t133, t127, 0.0D0, -t138, t188)
      t286 = FJET(XB1, XB2, s, 0.0D0, -t240, 0.0D0, t235, 0.0D0, t257)
      t291 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t120)
      t293 = FJET(XB1, XB2, s, t127, 0.0D0, 0.0D0, -t133, -t138, t188)
      t295 = FJET(XB1, XB2, s, t127, -t269, 0.0D0, t267, t271, -t276)
      t306 = t200 + (-0.90D2 * t75 * t207 - t212) * t80 / 0.1440D4 + t21
     #9
      t307 = FJET(XB1, XB2, s, t192, 0.0D0, -t195, 0.0D0, 0.0D0, t306)
      t309 = FJET(XB1, XB2, s, t235, 0.0D0, -t240, 0.0D0, 0.0D0, t257)
      t314 = FJET(XB1, XB2, s, t267, 0.0D0, -t269, t127, t271, -t276)
      t319 = FJET(XB1, XB2, s, -t195, 0.0D0, t192, 0.0D0, 0.0D0, t306)
      t321 = FJET(XB1, XB2, s, -t133, 0.0D0, 0.0D0, t127, -t138, t188)
      t323 = FJET(XB1, XB2, s, -t269, t127, t267, 0.0D0, t271, -t276)
      t328 = FJET(XB1, XB2, s, -t240, 0.0D0, t235, 0.0D0, 0.0D0, t257)
      rrgg2qqbarht7s1em1 = t121 * t120 + t123 * t120 + t125 * t120 + t18
     #9 * t188 + t221 * t220 + t258 * pi * t260 * t255 / 0.16D2 - t277 *
     # pi * t6 * t274 / 0.8D1 + t282 * t220 + t284 * t188 + t286 * pi * 
     #t260 * t255 / 0.16D2 + t291 * t120 + t293 * t188 - t295 * pi * t6 
     #* t274 / 0.8D1 + t307 * t306 + t309 * pi * t260 * t255 / 0.16D2 - 
     #t314 * pi * t6 * t274 / 0.8D1 + t319 * t306 + t321 * t188 - t323 *
     # pi * t6 * t274 / 0.8D1 + t328 * pi * t260 * t255 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s1em2
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t3 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = cos(t7)
      t11 = Sqrt(-x3 * (-0.1D1 + x3))
      t24 = 0.1D1 / x2
      t28 = 0.1D1 / x1
      t34 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t35 = z ** 2
      t37 = Sin(t7)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t35 * t38)
      t49 = -t4 * t6 * (0.1D1 + z / (-z - x3 + 0.2D1 * t8 * t11 * z)) / 
     #x3 / 0.32D2 - t4 * t6 * t24 / 0.16D2 - t4 * t6 * t28 / 0.16D2 + (0
     #.180D3 * t4 * lh - 0.90D2 * (t34 - t41 * t3) * pi) * t6 / 0.2880D4
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
      t69 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t62, 
     #t56, 0.0D0, -t67)
      t72 = t68 * t69 * t28 / 0.16D2
      t73 = FJET(XB1, XB2, s, 0.0D0, t56, -t62, 0.0D0, -t67, t72)
      t76 = t6 * t69 * t28
      t80 = x2 * t1 * s
      t83 = (-0.1D1 + x2) * t1 * s
      t84 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t83, 0.
     #0D0, 0.0D0, 0.0D0)
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
      rrgg2qqbarht7s1em2 = t50 * t49 + t52 * t49 + t54 * t49 + t73 * pi 
     #* t76 / 0.16D2 + t88 * pi * t91 / 0.16D2 + t94 * pi * t91 / 0.16D2
     # + t98 * pi * t76 / 0.16D2 + t102 * t49 + t104 * pi * t76 / 0.16D2
     # + t108 * pi * t91 / 0.16D2 + t112 * pi * t91 / 0.16D2 + t116 * pi
     # * t76 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s1em3
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = t3 * pi * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = pi * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s1em3 = -t9 * t3 * t11 / 0.32D2 - t13 * t3 * t11 / 0.
     #32D2 - t16 * t3 * t11 / 0.32D2 - t19 * t3 * t11 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht7s1em4
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht7s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht7s2e1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t5 = -0.1D1 + x2
      t6 = x2 * x3
      t7 = x3 * x1
      t8 = x1 * z
      t9 = t7 * z
      t10 = x4 * pi
      t11 = cos(t10)
      t12 = 0.1D1 - x1 + t8
      t14 = -0.1D1 + x3
      t17 = Sqrt(-x3 * t12 * x2 * t14)
      t18 = t11 * t17
      t19 = 0.2D1 * t18
      t22 = 0.1D1 / t12
      t23 = -0.1D1 + t6
      t24 = 0.1D1 / t23
      t27 = t4 * t5 * (-t6 - 0.1D1 + x3 + x1 - t7 - t8 + t9 + t19) * t22
     # * t24
      t28 = t14 * s
      t29 = t1 * x1
      t31 = t28 * t29 * t24
      t32 = t6 * t8
      t34 = x2 ** 2
      t35 = t34 * x3
      t36 = t35 * t8
      t37 = 0.3D1 * t6
      t38 = t35 * x1
      t39 = t6 * x1
      t42 = 0.2D1 * t18 * x2
      t43 = 0.2D1 * t32 - t36 + t37 + t38 - t9 - 0.2D1 * t39 - x2 - x3 +
     # t7 - t35 + t42 - t19
      t46 = t4 * t43 * t22 * t24
      t47 = t2 * x1
      t50 = t47 * x3 * t5 * t24
      t51 = t1 ** 2
      t56 = s * t51 * x2 * x1 * t3 * t22
      t57 = s ** 2
      t58 = 0.1D1 / t57
      t59 = pi * t58
      t60 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t27, -t46, t3
     #1, t50, -t56)
      t61 = x1 ** 2
      t62 = t5 ** 2
      t64 = Sin(t10)
      t65 = t64 ** 2
      t66 = z ** 2
      t67 = 0.1D1 / t66
      t68 = t65 * t67
      t69 = t3 ** 2
      t72 = t23 ** 2
      t73 = 0.1D1 / t72
      t79 = log(-0.4D1 * t61 * t62 * t68 * t69 * t6 * t22 * t73 * t14)
      t80 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t27, -t46, t3
     #1, t50, -t56)
      t84 = t61 * x2
      t87 = t61 * t66
      t88 = x2 * x1
      t95 = x2 * z
      t98 = x3 * t61
      t103 = t66 * x2
      t106 = 0.1D1 - t84 - 0.2D1 * t61 * z + t87 + 0.2D1 * t88 + t42 + t
     #61 + t38 - 0.3D1 * t39 - 0.2D1 * t18 * t8 - 0.2D1 * t18 * t88 - 0.
     #2D1 * t18 * t95 - 0.2D1 * t98 * t95 - x3 * t66 * t88 + t98 * t103 
     #+ 0.4D1 * t32
      t107 = t88 * z
      t116 = t98 * x2
      t120 = t35 * z
      t121 = t6 * z
      t122 = 0.2D1 * t6
      t123 = -t36 - t19 + 0.2D1 * t18 * t107 - 0.2D1 * x1 + 0.2D1 * t8 +
     # t103 * x1 + 0.2D1 * t84 * z - t87 * x2 + t116 - 0.3D1 * t107 + 0.
     #2D1 * t18 * x1 + t120 - t121 - x2 + t122 - t35 + t95
      t125 = 0.1D1 / (t106 + t123)
      t127 = -0.1D1 - t8 + x1 - t95 - t88 + t107 + x2
      t131 = pi * lh
      t132 = t131 * t58
      t138 = -0.90D2 * t59 * (t60 - t79 * t80) * t125 * t12 * t127 + 0.1
     #80D3 * t132 * t80 * t125 * t12 * t127
      t139 = 0.1D1 / x3
      t141 = 0.1D1 / x2
      t142 = 0.1D1 / x1
      t143 = t141 * t142
      t145 = t138 * t139 * t143 / 0.720D3
      t146 = FJET(XB1, XB2, s, t27, t31, -t46, t50, -t56, t145)
      t149 = t139 * t141 * t142
      t153 = Sqrt(-t6 * t14)
      t154 = t11 * t153
      t155 = 0.2D1 * t154
      t157 = 0.2D1 * t154 * x2
      t160 = t2 * (-x2 - x3 + t37 - t35 - t155 + t157) * t24
      t164 = t2 * t5 * (-t6 - 0.1D1 + x3 + t155) * t24
      t166 = t67 * t62
      t168 = t166 * t14 * t73
      t171 = log(-0.4D1 * t6 * t65 * t168)
      t172 = 0.1D1 - x2 + t95
      t173 = t171 * t172
      t174 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t164, t160,
     # 0.0D0, 0.0D0, 0.0D0)
      t176 = t171 ** 2
      t178 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t164, t160,
     # 0.0D0, 0.0D0, 0.0D0)
      t181 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t164, t160,
     # 0.0D0, 0.0D0, 0.0D0)
      t187 = 0.1D1 / (-t120 + t35 + x2 - t122 - t157 - t95 + t121 + 0.2D
     #1 * t154 * t95 + t155 - 0.1D1)
      t191 = t172 * t174
      t198 = lh ** 2
      t200 = pi ** 2
      t202 = -0.180D3 * t198 + 0.30D2 * t200
      t203 = pi * t202
      t206 = t172 * t178 * t187
      t213 = t6 * t61 * t65
      t216 = log(-0.4D1 * t213 * t168)
      t229 = (0.90D2 * t59 * (-t173 * t174 + t176 * t172 * t178 / 0.2D1 
     #+ t172 * t181) * t187 - 0.180D3 * t131 * t58 * (t191 - t173 * t178
     #) * t187 - t203 * t58 * t206) * t139 * t141 / 0.1440D4 + (0.90D2 *
     # t59 * (t191 - t216 * t172 * t178) * t187 - 0.180D3 * t132 * t206)
     # * t139 * t143 / 0.720D3
      t230 = FJET(XB1, XB2, s, t160, 0.0D0, -t164, 0.0D0, 0.0D0, t229)
      t232 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t233 = x3 * t67
      t236 = log(0.4D1 * t233 * t65)
      t237 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t242 = t236 ** 2
      t245 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t249 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t259 = -0.60D2 * lh * t200 + 0.240D3 * zeta3 + 0.120D3 * t198 * lh
      t260 = pi * t259
      t261 = t58 * t237
      t262 = t260 * t261
      t274 = log(0.4D1 * t68)
      t275 = t274 ** 2
      t276 = t275 * pi
      t280 = t275 * t274 * pi
      t282 = t274 * pi
      t301 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t311 = t200 ** 2
      t312 = t198 ** 2
      t318 = t275 ** 2
      t327 = log(0.4D1 * t98 * t68)
      t329 = t327 ** 2
      t340 = t203 * t261
      t345 = t61 * t67
      t346 = t345 * t65
      t348 = log(0.4D1 * t346)
      t353 = t348 ** 2
      t375 = log(0.4D1 * t6 * t346)
      t386 = t84 * t68
      t388 = log(0.4D1 * t386)
      t390 = t388 ** 2
      t407 = log(0.4D1 * t6 * t68)
      t409 = t407 ** 2
      t424 = x2 * t65
      t427 = log(0.4D1 * t424 * t67)
      t432 = t427 ** 2
      t452 = (t203 * t58 * (t232 - t236 * t237) - 0.90D2 * t59 * (t242 *
     # t232 / 0.2D1 + t245 - t242 * t236 * t237 / 0.6D1 - t236 * t249) +
     # t262 + 0.180D3 * t131 * t58 * (-t236 * t232 + t242 * t237 / 0.2D1
     # + t249)) * t139 / 0.1440D4 + (0.90D2 * t276 * lh + t260 + 0.15D2 
     #* t280 - t282 * t202) * t58 * t232 / 0.1440D4 + (-0.180D3 * t282 *
     # lh - 0.45D2 * t276 + t203) * t58 * t249 / 0.1440D4 + (0.180D3 * t
     #131 + 0.90D2 * t282) * t58 * t245 / 0.1440D4 - t59 * t301 / 0.16D2
     # + (-0.30D2 * t280 * lh + t276 * t202 / 0.2D1 - t282 * t259 + pi *
     # (-0.480D3 * lh * zeta3 - t311 - 0.60D2 * t312 + 0.60D2 * t198 * t
     #200) - 0.15D2 / 0.4D1 * t318 * pi) * t58 * t237 / 0.1440D4 + (-0.9
     #0D2 * t59 * (-t327 * t232 + t329 * t237 / 0.2D1 + t249) + 0.180D3 
     #* t131 * t58 * (t232 - t327 * t237) + t340) * t139 * t142 / 0.720D
     #3 + (t203 * t58 * (t232 - t348 * t237) - 0.90D2 * t59 * (t353 * t2
     #32 / 0.2D1 + t245 - t353 * t348 * t237 / 0.6D1 - t348 * t249) + t2
     #62 + 0.180D3 * t131 * t58 * (-t348 * t232 + t353 * t237 / 0.2D1 + 
     #t249)) * t142 / 0.720D3 + (-0.90D2 * t59 * (t232 - t375 * t237) + 
     #0.180D3 * t131 * t261) * t139 * t143 / 0.720D3 - (-0.90D2 * t59 * 
     #(t388 * t232 - t390 * t237 / 0.2D1 - t249) + 0.180D3 * t131 * t58 
     #* (-t232 + t388 * t237) - t340) * t141 * t142 / 0.720D3 + (-0.90D2
     # * t59 * (-t407 * t232 + t409 * t237 / 0.2D1 + t249) + 0.180D3 * t
     #131 * t58 * (t232 - t407 * t237) + t340) * t139 * t141 / 0.1440D4 
     #- (t203 * t58 * (-t232 + t427 * t237) - 0.90D2 * t59 * (-t432 * t2
     #32 / 0.2D1 - t245 + t432 * t427 * t237 / 0.6D1 + t427 * t249) - t2
     #62 + 0.180D3 * t131 * t58 * (t427 * t232 - t432 * t237 / 0.2D1 - t
     #249)) * t141 / 0.1440D4
      t453 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t452)
      t455 = FJET(XB1, XB2, s, -t46, t50, t27, t31, -t56, t145)
      t459 = FJET(XB1, XB2, s, 0.0D0, t160, 0.0D0, -t164, 0.0D0, t229)
      t462 = x2 * t1 * s
      t464 = t5 * t1 * s
      t465 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t464, t462,
     # 0.0D0, 0.0D0, 0.0D0)
      t466 = t68 * t62
      t469 = log(0.4D1 * t116 * t466)
      t470 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t464, t462,
     # 0.0D0, 0.0D0, 0.0D0)
      t475 = t58 * t470
      t481 = (-0.90D2 * t59 * (-t465 + t469 * t470) - 0.180D3 * t131 * t
     #475) * t139 * t143 / 0.720D3
      t484 = log(0.4D1 * t84 * t466)
      t486 = t484 ** 2
      t489 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t464, t462,
     # 0.0D0, 0.0D0, 0.0D0)
      t498 = t203 * t475
      t502 = (-0.90D2 * t59 * (-t484 * t465 + t486 * t470 / 0.2D1 + t489
     #) + 0.180D3 * t131 * t58 * (t465 - t484 * t470) + t498) * t141 * t
     #142 / 0.720D3
      t505 = log(0.4D1 * t6 * t466)
      t507 = t505 ** 2
      t521 = (-0.90D2 * t59 * (t505 * t465 - t507 * t470 / 0.2D1 - t489)
     # + 0.180D3 * t131 * t58 * (-t465 + t505 * t470) - t498) * t139 * t
     #141 / 0.1440D4
      t524 = log(0.4D1 * t424 * t166)
      t526 = -t465 + t524 * t470
      t529 = t524 ** 2
      t532 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, -t464, t462,
     # 0.0D0, 0.0D0, 0.0D0)
      t537 = -t529 * t465 / 0.2D1 - t532 + t529 * t524 * t470 / 0.6D1 + 
     #t524 * t489
      t540 = t260 * t475
      t544 = t524 * t465 - t529 * t470 / 0.2D1 - t489
      t551 = t481 - t502 + t521 - (-t203 * t58 * t526 + 0.90D2 * t59 * t
     #537 + t540 - 0.180D3 * t131 * t58 * t544) * t141 / 0.1440D4
      t552 = FJET(XB1, XB2, s, t462, 0.0D0, -t464, 0.0D0, 0.0D0, t551)
      t567 = t481 - t502 + t521 - (-t203 * t58 * t526 + 0.90D2 * t59 * t
     #537 + t540 - 0.180D3 * t131 * t58 * t544) * t141 / 0.1440D4
      t568 = FJET(XB1, XB2, s, 0.0D0, -t464, 0.0D0, t462, 0.0D0, t567)
      t570 = FJET(XB1, XB2, s, -t464, 0.0D0, t462, 0.0D0, 0.0D0, t551)
      t572 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t452)
      t574 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t452)
      t578 = t65 * t69 * t22
      t581 = log(0.4D1 * t98 * t67 * t578)
      t582 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, 
     #t47, 0.0D0, 0.0D0)
      t584 = t581 ** 2
      t585 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, 
     #t47, 0.0D0, 0.0D0)
      t588 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, 
     #t47, 0.0D0, 0.0D0)
      t597 = t58 * t585
      t598 = t203 * t597
      t601 = (-0.90D2 * t59 * (t581 * t582 - t584 * t585 / 0.2D1 - t588)
     # + 0.180D3 * t131 * t58 * (-t582 + t581 * t585) - t598) * t139 * t
     #142
      t604 = log(0.4D1 * t345 * t578)
      t606 = t582 - t604 * t585
      t609 = t604 ** 2
      t612 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, 
     #t47, 0.0D0, 0.0D0)
      t617 = t609 * t582 / 0.2D1 + t612 - t609 * t604 * t585 / 0.6D1 - t
     #604 * t588
      t620 = t260 * t597
      t624 = -t604 * t582 + t609 * t585 / 0.2D1 + t588
      t630 = t22 * t69
      t634 = log(0.4D1 * t116 * t68 * t630)
      t643 = (-0.90D2 * t59 * (-t582 + t634 * t585) - 0.180D3 * t131 * t
     #597) * t139 * t143
      t644 = t84 * t65
      t645 = t67 * t22
      t649 = log(0.4D1 * t644 * t645 * t69)
      t651 = t649 ** 2
      t664 = (-0.90D2 * t59 * (-t649 * t582 + t651 * t585 / 0.2D1 + t588
     #) + 0.180D3 * t131 * t58 * (t582 - t649 * t585) + t598) * t141 * t
     #142
      t666 = t601 / 0.720D3 + (-t203 * t58 * t606 + 0.90D2 * t59 * t617 
     #- t620 - 0.180D3 * t131 * t58 * t624) * t142 / 0.720D3 + t643 / 0.
     #720D3 - t664 / 0.720D3
      t667 = FJET(XB1, XB2, s, t47, -t4, 0.0D0, 0.0D0, 0.0D0, t666)
      t669 = t2 * t7
      t671 = t1 * t3
      t672 = x3 * s * t671
      t673 = t28 * t29
      t674 = t28 * t671
      t675 = x3 * t14
      t676 = t630 * t675
      t679 = log(-0.4D1 * t346 * t676)
      t680 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t674, -t672,
     # -t673, t669, 0.0D0)
      t682 = t679 ** 2
      t683 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t674, -t672,
     # -t673, t669, 0.0D0)
      t686 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t674, -t672,
     # -t673, t669, 0.0D0)
      t687 = -t679 * t680 + t682 * t683 / 0.2D1 + t686
      t691 = t680 - t679 * t683
      t695 = t58 * t683
      t696 = t203 * t695
      t702 = log(-0.4D1 * t386 * t676)
      t711 = (-0.90D2 * t59 * (t680 - t702 * t683) + 0.180D3 * t131 * t6
     #95) * t139 * t143
      t713 = (-0.90D2 * t59 * t687 + 0.180D3 * t131 * t58 * t691 + t696)
     # * t139 * t142 / 0.720D3 + t711 / 0.720D3
      t714 = FJET(XB1, XB2, s, t669, -t672, -t673, t674, 0.0D0, t713)
      t716 = FJET(XB1, XB2, s, -t4, t47, 0.0D0, 0.0D0, 0.0D0, t666)
      t719 = t5 * s * t671
      t722 = t2 * t3 * x2 * t22
      t723 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t719, -t722,
     # t47, 0.0D0, -t56)
      t725 = t645 * t69 * t62
      t728 = log(0.4D1 * t213 * t725)
      t729 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t719, -t722,
     # t47, 0.0D0, -t56)
      t734 = t58 * t729
      t739 = (-0.90D2 * t59 * (t723 - t728 * t729) + 0.180D3 * t131 * t7
     #34) * t139 * t143
      t742 = log(0.4D1 * t644 * t725)
      t744 = t742 ** 2
      t747 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t719, -t722,
     # t47, 0.0D0, -t56)
      t748 = t742 * t723 - t744 * t729 / 0.2D1 - t747
      t752 = -t723 + t742 * t729
      t756 = t203 * t734
      t761 = t739 / 0.720D3 - (-0.90D2 * t59 * t748 + 0.180D3 * t131 * t
     #58 * t752 - t756) * t141 * t142 / 0.720D3
      t762 = FJET(XB1, XB2, s, t719, t47, -t722, 0.0D0, -t56, t761)
      t764 = FJET(XB1, XB2, s, t674, -t673, -t672, t669, 0.0D0, t713)
      t766 = FJET(XB1, XB2, s, t47, t719, 0.0D0, -t722, -t56, t761)
      t768 = t146 * t138 * t149 / 0.720D3 + t230 * t229 + t453 * t452 + 
     #t455 * t138 * t149 / 0.720D3 + t459 * t229 + t552 * t551 + t568 * 
     #t567 + t570 * t551 + t572 * t452 + t574 * t452 + t667 * t666 + t71
     #4 * t713 + t716 * t666 + t762 * t761 + t764 * t713 + t766 * t761
      t780 = (-0.90D2 * t59 * t687 + 0.180D3 * t131 * t58 * t691 + t696)
     # * t139 * t142 / 0.720D3 + t711 / 0.720D3
      t781 = FJET(XB1, XB2, s, -t673, t674, t669, -t672, 0.0D0, t780)
      t783 = FJET(XB1, XB2, s, 0.0D0, -t722, t47, t719, -t56, t761)
      t785 = t2 * t14
      t786 = t2 * x3
      t790 = log(-0.4D1 * t98 * t68 * t14)
      t791 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t785, t786,
     # 0.0D0, 0.0D0, 0.0D0)
      t793 = t790 ** 2
      t794 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t785, t786,
     # 0.0D0, 0.0D0, 0.0D0)
      t797 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t785, t786,
     # 0.0D0, 0.0D0, 0.0D0)
      t806 = t58 * t794
      t807 = t203 * t806
      t811 = (-0.90D2 * t59 * (t790 * t791 - t793 * t794 / 0.2D1 - t797)
     # + 0.180D3 * t131 * t58 * (-t791 + t790 * t794) - t807) * t139 * t
     #142 / 0.720D3
      t812 = t233 * t14
      t815 = log(-0.4D1 * t644 * t812)
      t825 = (-0.90D2 * t59 * (-t791 + t815 * t794) - 0.180D3 * t131 * t
     #806) * t139 * t143 / 0.720D3
      t828 = log(-0.4D1 * t68 * t675)
      t830 = -t791 + t828 * t794
      t833 = t828 ** 2
      t836 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, -t785, t786,
     # 0.0D0, 0.0D0, 0.0D0)
      t841 = -t833 * t791 / 0.2D1 - t836 + t833 * t828 * t794 / 0.6D1 + 
     #t828 * t797
      t844 = t260 * t806
      t848 = t828 * t791 - t833 * t794 / 0.2D1 - t797
      t857 = log(-0.4D1 * t424 * t812)
      t859 = t857 ** 2
      t873 = (-0.90D2 * t59 * (t857 * t791 - t859 * t794 / 0.2D1 - t797)
     # + 0.180D3 * t131 * t58 * (-t791 + t857 * t794) - t807) * t139 * t
     #141 / 0.1440D4
      t874 = t811 + t825 + (t203 * t58 * t830 - 0.90D2 * t59 * t841 - t8
     #44 + 0.180D3 * t131 * t58 * t848) * t139 / 0.1440D4 + t873
      t875 = FJET(XB1, XB2, s, 0.0D0, -t785, 0.0D0, t786, 0.0D0, t874)
      t890 = t811 + t825 + (t203 * t58 * t830 - 0.90D2 * t59 * t841 - t8
     #44 + 0.180D3 * t131 * t58 * t848) * t139 / 0.1440D4 + t873
      t891 = FJET(XB1, XB2, s, t786, 0.0D0, -t785, 0.0D0, 0.0D0, t890)
      t893 = FJET(XB1, XB2, s, -t164, 0.0D0, t160, 0.0D0, 0.0D0, t229)
      t908 = t601 / 0.720D3 + (-t203 * t58 * t606 + 0.90D2 * t59 * t617 
     #- t620 - 0.180D3 * t131 * t58 * t624) * t142 / 0.720D3 + t643 / 0.
     #720D3 - t664 / 0.720D3
      t909 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t47, -t4, 0.0D0, t908)
      t911 = FJET(XB1, XB2, s, -t785, 0.0D0, t786, 0.0D0, 0.0D0, t890)
      t924 = t739 / 0.720D3 - (-0.90D2 * t59 * t748 + 0.180D3 * t131 * t
     #58 * t752 - t756) * t141 * t142 / 0.720D3
      t925 = FJET(XB1, XB2, s, -t722, 0.0D0, t719, t47, -t56, t924)
      t927 = FJET(XB1, XB2, s, 0.0D0, t786, 0.0D0, -t785, 0.0D0, t874)
      t929 = FJET(XB1, XB2, s, t31, t27, t50, -t46, -t56, t145)
      t933 = FJET(XB1, XB2, s, t50, -t46, t31, t27, -t56, t145)
      t937 = FJET(XB1, XB2, s, -t672, t669, t674, -t673, 0.0D0, t780)
      t939 = FJET(XB1, XB2, s, 0.0D0, t462, 0.0D0, -t464, 0.0D0, t567)
      t941 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t47, 0.0D0, t908)
      t943 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t452)
      t945 = FJET(XB1, XB2, s, 0.0D0, -t164, 0.0D0, t160, 0.0D0, t229)
      t947 = t781 * t780 + t783 * t761 + t875 * t874 + t891 * t890 + t89
     #3 * t229 + t909 * t908 + t911 * t890 + t925 * t924 + t927 * t874 +
     # t929 * t138 * t149 / 0.720D3 + t933 * t138 * t149 / 0.720D3 + t93
     #7 * t780 + t939 * t567 + t941 * t908 + t943 * t452 + t945 * t229
      rrgg2qqbarht7s2e1 = t768 + t947

      end function



      doubleprecision function rrgg2qqbarht7s2e0
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t7 = -0.1D1 + t6
      t8 = 0.1D1 / t7
      t10 = t3 * x3 * t4 * t8
      t11 = -0.1D1 + x1
      t12 = t2 * t11
      t13 = x1 * z
      t14 = t6 * t13
      t16 = x2 ** 2
      t17 = t16 * x3
      t18 = t17 * t13
      t19 = 0.3D1 * t6
      t20 = t17 * x1
      t21 = x3 * x1
      t22 = t21 * z
      t23 = t6 * x1
      t25 = x4 * pi
      t26 = cos(t25)
      t27 = 0.1D1 - x1 + t13
      t29 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t27 * x2 * t29)
      t33 = t26 * t32
      t35 = 0.2D1 * t33 * x2
      t36 = 0.2D1 * t33
      t37 = 0.2D1 * t14 - t18 + t19 + t20 - t22 - 0.2D1 * t23 - x2 - x3 
     #+ t21 - t17 + t35 - t36
      t38 = 0.1D1 / t27
      t41 = t12 * t37 * t38 * t8
      t42 = t29 * s
      t43 = t1 * x1
      t45 = t42 * t43 * t8
      t50 = t12 * t4 * (-t6 - 0.1D1 + x3 + x1 - t21 - t13 + t22 + t36) *
     # t38 * t8
      t51 = t1 ** 2
      t56 = s * t51 * x2 * x1 * t11 * t38
      t57 = s ** 2
      t58 = 0.1D1 / t57
      t59 = pi * t58
      t60 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t50, -t41, t4
     #5, t10, -t56)
      t62 = x2 * x1
      t63 = t62 * z
      t66 = t17 * z
      t67 = t6 * z
      t69 = 0.2D1 * t6
      t70 = x2 * z
      t71 = x1 ** 2
      t72 = t71 * x2
      t75 = z ** 2
      t76 = t71 * t75
      t79 = 0.1D1 - 0.2D1 * x1 + 0.2D1 * t33 * t63 + t66 - t67 + 0.2D1 *
     # t13 + t69 - t17 + t70 - t36 - t72 - 0.2D1 * t71 * z + t76 + 0.2D1
     # * t62 + t20 - 0.3D1 * t23
      t80 = t75 * x2
      t85 = x3 * t71
      t102 = t35 + t80 * x1 + 0.2D1 * t72 * z - t76 * x2 + t85 * x2 - 0.
     #3D1 * t63 + 0.2D1 * t33 * x1 - x2 + 0.4D1 * t14 - t18 - 0.2D1 * t8
     #5 * t70 - x3 * t75 * t62 + t85 * t80 - 0.2D1 * t33 * t13 - 0.2D1 *
     # t33 * t62 - 0.2D1 * t33 * t70 + t71
      t104 = 0.1D1 / (t79 + t102)
      t109 = 0.1D1 / x3
      t110 = 0.1D1 / x2
      t112 = 0.1D1 / x1
      t113 = t109 * t110 * t112
      t114 = t27 * (-0.1D1 - t13 + x1 - t70 - t62 + t63 + x2) * t113
      t116 = t59 * t60 * t104 * t114 / 0.8D1
      t117 = FJET(XB1, XB2, s, t10, -t41, t45, t50, -t56, -t116)
      t120 = t58 * t60 * t104
      t124 = 0.1D1 / t75
      t125 = x3 * t124
      t126 = Sin(t25)
      t127 = t126 ** 2
      t130 = log(0.4D1 * t125 * t127)
      t131 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t133 = t130 ** 2
      t134 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t137 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t141 = pi * lh
      t147 = lh ** 2
      t149 = pi ** 2
      t151 = -0.180D3 * t147 + 0.30D2 * t149
      t152 = pi * t151
      t153 = t58 * t134
      t154 = t152 * t153
      t158 = t124 * t127
      t160 = log(0.4D1 * t158)
      t161 = t160 * pi
      t164 = t160 ** 2
      t165 = t164 * pi
      t171 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t199 = log(0.4D1 * t6 * t158)
      t205 = 0.180D3 * t141 * t153
      t210 = x2 * t127
      t213 = log(0.4D1 * t210 * t124)
      t215 = t213 ** 2
      t231 = log(0.4D1 * t85 * t158)
      t245 = log(0.4D1 * t72 * t158)
      t254 = t71 * t124
      t255 = t254 * t127
      t257 = log(0.4D1 * t255)
      t259 = t257 ** 2
      t273 = (-0.90D2 * t59 * (-t130 * t131 + t133 * t134 / 0.2D1 + t137
     #) + 0.180D3 * t141 * t58 * (t131 - t130 * t134) + t154) * t109 / 0
     #.1440D4 + (-0.180D3 * t161 * lh - 0.45D2 * t165 + t152) * t58 * t1
     #31 / 0.1440D4 - t59 * t171 / 0.16D2 + (0.90D2 * t165 * lh + pi * (
     #-0.60D2 * lh * t149 + 0.240D3 * zeta3 + 0.120D3 * t147 * lh) + 0.1
     #5D2 * t164 * t160 * pi - t161 * t151) * t58 * t134 / 0.1440D4 + (0
     #.180D3 * t141 + 0.90D2 * t161) * t58 * t137 / 0.1440D4 + (-0.90D2 
     #* t59 * (t131 - t199 * t134) + t205) * t109 * t110 / 0.1440D4 - (-
     #0.90D2 * t59 * (t213 * t131 - t215 * t134 / 0.2D1 - t137) + 0.180D
     #3 * t141 * t58 * (-t131 + t213 * t134) - t154) * t110 / 0.1440D4 +
     # (-0.90D2 * t59 * (t131 - t231 * t134) + t205) * t109 * t112 / 0.7
     #20D3 - t59 * t134 * t113 / 0.8D1 - (-0.90D2 * t59 * (-t131 + t245 
     #* t134) - t205) * t110 * t112 / 0.720D3 + (-0.90D2 * t59 * (-t257 
     #* t131 + t259 * t134 / 0.2D1 + t137) + 0.180D3 * t141 * t58 * (t13
     #1 - t257 * t134) + t154) * t112 / 0.720D3
      t274 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t273)
      t276 = t2 * t21
      t278 = t1 * t11
      t279 = x3 * s * t278
      t280 = t42 * t43
      t281 = t42 * t278
      t282 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t281, -t279,
     # -t280, t276, 0.0D0)
      t283 = t11 ** 2
      t285 = x3 * t29
      t289 = log(-0.4D1 * t255 * t38 * t283 * t285)
      t290 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t281, -t279,
     # -t280, t276, 0.0D0)
      t292 = t282 - t289 * t290
      t297 = 0.180D3 * t141 * t58 * t290
      t304 = t59 * t290 * t113 / 0.8D1
      t305 = (-0.90D2 * t59 * t292 + t297) * t109 * t112 / 0.720D3 - t30
     #4
      t306 = FJET(XB1, XB2, s, t276, -t279, -t280, t281, 0.0D0, t305)
      t309 = Sqrt(-t6 * t29)
      t310 = t26 * t309
      t311 = 0.2D1 * t310
      t315 = t2 * t4 * (-t6 - 0.1D1 + x3 + t311) * t8
      t317 = 0.2D1 * t310 * x2
      t320 = t2 * (-x2 - x3 + t19 - t17 - t311 + t317) * t8
      t321 = 0.1D1 - x2 + t70
      t322 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t315, t320,
     # 0.0D0, 0.0D0, 0.0D0)
      t323 = t321 * t322
      t328 = 0.1D1 / (-t66 + t17 + x2 - t69 - t317 - t70 + t67 + 0.2D1 *
     # t310 * t70 + t311 - 0.1D1)
      t334 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t315, t320,
     # 0.0D0, 0.0D0, 0.0D0)
      t337 = t4 ** 2
      t338 = t124 * t337
      t339 = t7 ** 2
      t345 = log(-0.4D1 * t6 * t127 * t338 * t29 / t339)
      t360 = t59 * t323 * t328 * t109 * t110 * t112 / 0.8D1 + (0.90D2 * 
     #t59 * (t321 * t334 - t345 * t321 * t322) * t328 - 0.180D3 * t141 *
     # t58 * t323 * t328) * t109 * t110 / 0.1440D4
      t361 = FJET(XB1, XB2, s, -t315, 0.0D0, t320, 0.0D0, 0.0D0, t360)
      t363 = t2 * t29
      t364 = t2 * x3
      t365 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t363, t364,
     # 0.0D0, 0.0D0, 0.0D0)
      t369 = log(-0.4D1 * t210 * t125 * t29)
      t370 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t363, t364,
     # 0.0D0, 0.0D0, 0.0D0)
      t375 = t58 * t370
      t377 = 0.180D3 * t141 * t375
      t381 = (-0.90D2 * t59 * (-t365 + t369 * t370) - t377) * t109 * t11
     #0 / 0.1440D4
      t384 = log(-0.4D1 * t158 * t285)
      t386 = t384 ** 2
      t389 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t363, t364,
     # 0.0D0, 0.0D0, 0.0D0)
      t390 = t384 * t365 - t386 * t370 / 0.2D1 - t389
      t394 = -t365 + t384 * t370
      t398 = t152 * t375
      t405 = log(-0.4D1 * t85 * t158 * t29)
      t413 = (-0.90D2 * t59 * (-t365 + t405 * t370) - t377) * t109 * t11
     #2 / 0.720D3
      t416 = t59 * t370 * t113 / 0.8D1
      t417 = t381 + (-0.90D2 * t59 * t390 + 0.180D3 * t141 * t58 * t394 
     #- t398) * t109 / 0.1440D4 + t413 + t416
      t418 = FJET(XB1, XB2, s, 0.0D0, -t363, 0.0D0, t364, 0.0D0, t417)
      t421 = t4 * s * t278
      t424 = t2 * t11 * x2 * t38
      t425 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t421, -t424,
     # t3, 0.0D0, -t56)
      t428 = t59 * t425 * t113 / 0.8D1
      t429 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t421, -t424,
     # t3, 0.0D0, -t56)
      t430 = t72 * t127
      t431 = t124 * t38
      t436 = log(0.4D1 * t430 * t431 * t283 * t337)
      t438 = -t429 + t436 * t425
      t443 = 0.180D3 * t141 * t58 * t425
      t448 = -t428 - (-0.90D2 * t59 * t438 - t443) * t110 * t112 / 0.720
     #D3
      t449 = FJET(XB1, XB2, s, t3, t421, 0.0D0, -t424, -t56, t448)
      t451 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0,
     # t3, 0.0D0, 0.0D0)
      t454 = t127 * t283 * t38
      t457 = log(0.4D1 * t85 * t124 * t454)
      t458 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0,
     # t3, 0.0D0, 0.0D0)
      t463 = t58 * t458
      t465 = 0.180D3 * t141 * t463
      t469 = (-0.90D2 * t59 * (-t451 + t457 * t458) - t465) * t109 * t11
     #2 / 0.720D3
      t472 = t59 * t458 * t113 / 0.8D1
      t476 = log(0.4D1 * t430 * t431 * t283)
      t484 = (-0.90D2 * t59 * (t451 - t476 * t458) + t465) * t110 * t112
     # / 0.720D3
      t487 = log(0.4D1 * t254 * t454)
      t489 = t487 ** 2
      t492 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t12, 0.0D0,
     # t3, 0.0D0, 0.0D0)
      t493 = -t487 * t451 + t489 * t458 / 0.2D1 + t492
      t497 = t451 - t487 * t458
      t501 = t152 * t463
      t505 = t469 + t472 - t484 + (0.90D2 * t59 * t493 - 0.180D3 * t141 
     #* t58 * t497 - t501) * t112 / 0.720D3
      t506 = FJET(XB1, XB2, s, t3, -t12, 0.0D0, 0.0D0, 0.0D0, t505)
      t508 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t273)
      t520 = t469 + t472 - t484 + (0.90D2 * t59 * t493 - 0.180D3 * t141 
     #* t58 * t497 - t501) * t112 / 0.720D3
      t521 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t12, t3, 0.0D0, t520)
      t524 = t4 * t1 * s
      t526 = x2 * t1 * s
      t527 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t524, t526,
     # 0.0D0, 0.0D0, 0.0D0)
      t528 = t158 * t337
      t531 = log(0.4D1 * t6 * t528)
      t532 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t524, t526,
     # 0.0D0, 0.0D0, 0.0D0)
      t537 = t58 * t532
      t539 = 0.180D3 * t141 * t537
      t543 = (-0.90D2 * t59 * (-t527 + t531 * t532) - t539) * t109 * t11
     #0 / 0.1440D4
      t546 = log(0.4D1 * t210 * t338)
      t548 = t546 ** 2
      t551 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t524, t526,
     # 0.0D0, 0.0D0, 0.0D0)
      t552 = t546 * t527 - t548 * t532 / 0.2D1 - t551
      t556 = -t527 + t546 * t532
      t560 = t152 * t537
      t566 = t59 * t532 * t113 / 0.8D1
      t569 = log(0.4D1 * t72 * t528)
      t577 = (-0.90D2 * t59 * (t527 - t569 * t532) + t539) * t110 * t112
     # / 0.720D3
      t578 = t543 - (0.90D2 * t59 * t552 - 0.180D3 * t141 * t58 * t556 +
     # t560) * t110 / 0.1440D4 + t566 - t577
      t579 = FJET(XB1, XB2, s, -t524, 0.0D0, t526, 0.0D0, 0.0D0, t578)
      t581 = FJET(XB1, XB2, s, 0.0D0, t364, 0.0D0, -t363, 0.0D0, t417)
      t583 = FJET(XB1, XB2, s, t421, t3, -t424, 0.0D0, -t56, t448)
      t585 = FJET(XB1, XB2, s, t320, 0.0D0, -t315, 0.0D0, 0.0D0, t360)
      t587 = FJET(XB1, XB2, s, t281, -t280, -t279, t276, 0.0D0, t305)
      t596 = (-0.90D2 * t59 * t292 + t297) * t109 * t112 / 0.720D3 - t30
     #4
      t597 = FJET(XB1, XB2, s, -t279, t276, t281, -t280, 0.0D0, t596)
      t599 = FJET(XB1, XB2, s, 0.0D0, t320, 0.0D0, -t315, 0.0D0, t360)
      t601 = -t117 * pi * t120 * t114 / 0.8D1 + t274 * t273 + t306 * t30
     #5 + t361 * t360 + t418 * t417 + t449 * t448 + t506 * t505 + t508 *
     # t273 + t521 * t520 + t579 * t578 + t581 * t417 + t583 * t448 + t5
     #85 * t360 + t587 * t305 + t597 * t596 + t599 * t360
      t612 = t543 - (0.90D2 * t59 * t552 - 0.180D3 * t141 * t58 * t556 +
     # t560) * t110 / 0.1440D4 + t566 - t577
      t613 = FJET(XB1, XB2, s, 0.0D0, t526, 0.0D0, -t524, 0.0D0, t612)
      t615 = FJET(XB1, XB2, s, 0.0D0, -t315, 0.0D0, t320, 0.0D0, t360)
      t617 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t273)
      t629 = t381 + (-0.90D2 * t59 * t390 + 0.180D3 * t141 * t58 * t394 
     #- t398) * t109 / 0.1440D4 + t413 + t416
      t630 = FJET(XB1, XB2, s, -t363, 0.0D0, t364, 0.0D0, 0.0D0, t629)
      t632 = FJET(XB1, XB2, s, -t41, t10, t50, t45, -t56, -t116)
      t637 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t12, 0.0D0, t520)
      t639 = FJET(XB1, XB2, s, -t280, t281, t276, -t279, 0.0D0, t596)
      t648 = -t428 - (-0.90D2 * t59 * t438 - t443) * t110 * t112 / 0.720
     #D3
      t649 = FJET(XB1, XB2, s, -t424, 0.0D0, t421, t3, -t56, t648)
      t651 = FJET(XB1, XB2, s, t364, 0.0D0, -t363, 0.0D0, 0.0D0, t629)
      t653 = FJET(XB1, XB2, s, 0.0D0, -t524, 0.0D0, t526, 0.0D0, t612)
      t655 = FJET(XB1, XB2, s, -t12, t3, 0.0D0, 0.0D0, 0.0D0, t505)
      t657 = FJET(XB1, XB2, s, t526, 0.0D0, -t524, 0.0D0, 0.0D0, t578)
      t659 = FJET(XB1, XB2, s, 0.0D0, -t424, t3, t421, -t56, t448)
      t661 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t273)
      t663 = FJET(XB1, XB2, s, t45, t50, t10, -t41, -t56, -t116)
      t668 = FJET(XB1, XB2, s, t50, t45, -t41, t10, -t56, -t116)
      t673 = t613 * t612 + t615 * t360 + t617 * t273 + t630 * t629 - t63
     #2 * pi * t120 * t114 / 0.8D1 + t637 * t520 + t639 * t596 + t649 * 
     #t648 + t651 * t629 + t653 * t612 + t655 * t505 + t657 * t578 + t65
     #9 * t448 + t661 * t273 - t663 * pi * t120 * t114 / 0.8D1 - t668 * 
     #pi * t120 * t114 / 0.8D1
      rrgg2qqbarht7s2e0 = t601 + t673

      end function



      doubleprecision function rrgg2qqbarht7s2em1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t9 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0
     #, 0.0D0, 0.0D0)
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t19 = log(-0.4D1 * t15 * x3 * t4)
      t20 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D
     #0, 0.0D0, 0.0D0)
      t22 = t9 - t19 * t20
      t25 = pi * lh
      t28 = 0.180D3 * t25 * t7 * t20
      t30 = 0.1D1 / x3
      t33 = t20 * t30
      t34 = 0.1D1 / x2
      t37 = t8 * t33 * t34 / 0.16D2
      t38 = 0.1D1 / x1
      t41 = t8 * t33 * t38 / 0.8D1
      t42 = (0.90D2 * t8 * t22 - t28) * t30 / 0.1440D4 + t37 + t41
      t43 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t42)
      t45 = -0.1D1 + x2
      t47 = t45 * t1 * s
      t49 = x2 * t1 * s
      t50 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t47, t49, 0.
     #0D0, 0.0D0, 0.0D0)
      t54 = t8 * t50 * t30 * t34 / 0.16D2
      t55 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t47, t49, 0.
     #0D0, 0.0D0, 0.0D0)
      t56 = x2 * t14
      t57 = t45 ** 2
      t61 = log(0.4D1 * t56 * t11 * t57)
      t63 = -t55 + t61 * t50
      t68 = 0.180D3 * t25 * t7 * t50
      t75 = t8 * t50 * t34 * t38 / 0.8D1
      t76 = t54 - (0.90D2 * t8 * t63 + t68) * t34 / 0.1440D4 + t75
      t77 = FJET(XB1, XB2, s, -t47, 0.0D0, t49, 0.0D0, 0.0D0, t76)
      t85 = t54 - (0.90D2 * t8 * t63 + t68) * t34 / 0.1440D4 + t75
      t86 = FJET(XB1, XB2, s, 0.0D0, t49, 0.0D0, -t47, 0.0D0, t85)
      t88 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.
     #0D0, 0.0D0, 0.0D0)
      t92 = log(0.4D1 * x3 * t11 * t14)
      t93 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.
     #0D0, 0.0D0, 0.0D0)
      t100 = 0.180D3 * t25 * t7 * t93
      t106 = log(0.4D1 * t15)
      t107 = t106 * pi
      t115 = t106 ** 2
      t118 = lh ** 2
      t120 = pi ** 2
      t128 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t131 = t93 * t30
      t137 = log(0.4D1 * t56 * t11)
      t149 = x1 ** 2
      t150 = t149 * t11
      t153 = log(0.4D1 * t150 * t14)
      t164 = (-0.90D2 * t8 * (t88 - t92 * t93) + t100) * t30 / 0.1440D4 
     #+ (0.180D3 * t25 + 0.90D2 * t107) * t7 * t88 / 0.1440D4 + (-0.180D
     #3 * t107 * lh - 0.45D2 * t115 * pi + pi * (-0.180D3 * t118 + 0.30D
     #2 * t120)) * t7 * t93 / 0.1440D4 - t8 * t128 / 0.16D2 - t8 * t131 
     #* t34 / 0.16D2 - (-0.90D2 * t8 * (-t88 + t137 * t93) - t100) * t34
     # / 0.1440D4 - t8 * t93 * t34 * t38 / 0.8D1 + (-0.90D2 * t8 * (t88 
     #- t153 * t93) + t100) * t38 / 0.720D3 - t8 * t131 * t38 / 0.8D1
      t165 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t164)
      t173 = (0.90D2 * t8 * t22 - t28) * t30 / 0.1440D4 + t37 + t41
      t174 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t173)
      t176 = FJET(XB1, XB2, s, 0.0D0, -t47, 0.0D0, t49, 0.0D0, t85)
      t178 = -0.1D1 + x1
      t179 = t2 * t178
      t180 = t2 * x1
      t181 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t179, 0.0D0
     #, t180, 0.0D0, 0.0D0)
      t185 = t8 * t181 * t34 * t38 / 0.8D1
      t186 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t179, 0.0D0
     #, t180, 0.0D0, 0.0D0)
      t189 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t191 = t178 ** 2
      t195 = log(0.4D1 * t150 * t14 * t189 * t191)
      t197 = t186 - t195 * t181
      t202 = 0.180D3 * t25 * t7 * t181
      t209 = t8 * t181 * t30 * t38 / 0.8D1
      t210 = t185 + (0.90D2 * t8 * t197 - t202) * t38 / 0.720D3 + t209
      t211 = FJET(XB1, XB2, s, -t179, t180, 0.0D0, 0.0D0, 0.0D0, t210)
      t213 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t173)
      t215 = x2 * x3
      t216 = cos(t12)
      t218 = Sqrt(-t215 * t4)
      t219 = t216 * t218
      t220 = 0.2D1 * t219
      t224 = 0.1D1 / (-0.1D1 + t215)
      t226 = t2 * t45 * (-t215 - 0.1D1 + x3 + t220) * t224
      t228 = x2 ** 2
      t229 = t228 * x3
      t231 = 0.2D1 * t219 * x2
      t234 = t2 * (-x2 - x3 + 0.3D1 * t215 - t229 - t220 + t231) * t224
      t235 = x2 * z
      t236 = 0.1D1 - x2 + t235
      t238 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t226, t234,
     # 0.0D0, 0.0D0, 0.0D0)
      t248 = t238 / (-t229 * z + t229 + x2 - 0.2D1 * t215 - t231 - t235 
     #+ t215 * z + 0.2D1 * t219 * t235 + t220 - 0.1D1) * t30 * t34
      t250 = t8 * t236 * t248 / 0.16D2
      t251 = FJET(XB1, XB2, s, -t226, 0.0D0, t234, 0.0D0, 0.0D0, t250)
      t253 = t7 * t236
      t263 = t185 + (0.90D2 * t8 * t197 - t202) * t38 / 0.720D3 + t209
      t264 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t179, t180, 0.0D0, t263)
      t266 = FJET(XB1, XB2, s, t180, -t179, 0.0D0, 0.0D0, 0.0D0, t210)
      t268 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t180, -t179, 0.0D0, t263)
      t270 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t42)
      t272 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t164)
      t274 = t43 * t42 + t77 * t76 + t86 * t85 + t165 * t164 + t174 * t1
     #73 + t176 * t85 + t211 * t210 + t213 * t173 + t251 * pi * t253 * t
     #248 / 0.16D2 + t264 * t263 + t266 * t210 + t268 * t263 + t270 * t4
     #2 + t272 * t164
      t275 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t164)
      t277 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t164)
      t279 = FJET(XB1, XB2, s, t234, 0.0D0, -t226, 0.0D0, 0.0D0, t250)
      t285 = t1 * t178
      t286 = t45 * s * t285
      t289 = t2 * t178 * x2 * t189
      t290 = t1 ** 2
      t295 = s * t290 * x2 * x1 * t178 * t189
      t296 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t286, -t289,
     # t180, 0.0D0, -t295)
      t298 = t296 * t34 * t38
      t300 = t8 * t298 / 0.8D1
      t301 = FJET(XB1, XB2, s, t180, t286, 0.0D0, -t289, -t295, -t300)
      t306 = t4 * s
      t308 = t306 * t1 * x1
      t309 = t306 * t285
      t311 = t2 * x1 * x3
      t313 = x3 * s * t285
      t314 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t309, -t313,
     # -t308, t311, 0.0D0)
      t316 = t314 * t30 * t38
      t318 = t8 * t316 / 0.8D1
      t319 = FJET(XB1, XB2, s, -t308, t309, t311, -t313, 0.0D0, -t318)
      t324 = FJET(XB1, XB2, s, 0.0D0, -t289, t180, t286, -t295, -t300)
      t329 = FJET(XB1, XB2, s, t309, -t308, -t313, t311, 0.0D0, -t318)
      t334 = FJET(XB1, XB2, s, t286, t180, -t289, 0.0D0, -t295, -t300)
      t339 = FJET(XB1, XB2, s, t311, -t313, -t308, t309, 0.0D0, -t318)
      t344 = FJET(XB1, XB2, s, -t313, t311, t309, -t308, 0.0D0, -t318)
      t349 = FJET(XB1, XB2, s, -t289, 0.0D0, t286, t180, -t295, -t300)
      t354 = FJET(XB1, XB2, s, 0.0D0, t234, 0.0D0, -t226, 0.0D0, t250)
      t359 = FJET(XB1, XB2, s, t49, 0.0D0, -t47, 0.0D0, 0.0D0, t76)
      t361 = FJET(XB1, XB2, s, 0.0D0, -t226, 0.0D0, t234, 0.0D0, t250)
      t366 = t275 * t164 + t277 * t164 + t279 * pi * t253 * t248 / 0.16D
     #2 - t301 * pi * t7 * t298 / 0.8D1 - t319 * pi * t7 * t316 / 0.8D1 
     #- t324 * pi * t7 * t298 / 0.8D1 - t329 * pi * t7 * t316 / 0.8D1 - 
     #t334 * pi * t7 * t298 / 0.8D1 - t339 * pi * t7 * t316 / 0.8D1 - t3
     #44 * pi * t7 * t316 / 0.8D1 - t349 * pi * t7 * t298 / 0.8D1 + t354
     # * pi * t253 * t248 / 0.16D2 + t359 * t76 + t361 * pi * t253 * t24
     #8 / 0.16D2
      rrgg2qqbarht7s2em1 = t274 + t366

      end function



      doubleprecision function rrgg2qqbarht7s2em2
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.
     #0D0, 0.0D0, 0.0D0)
      t24 = z ** 2
      t27 = Sin(x4 * pi)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 + (0.180D3 * pi * lh + 0.90D2 * t3
     #1 * pi) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t45 = t2 * (-0.1D1 + x1)
      t46 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t45, 0.0D0, 
     #t43, 0.0D0, 0.0D0)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t49)
      t53 = t4 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t62 = t2 * x3
      t64 = t2 * (-0.1D1 + x3)
      t65 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t64, t62, 0.
     #0D0, 0.0D0, 0.0D0)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t68)
      t72 = t4 * t65 * t7
      t76 = x2 * t1 * s
      t79 = (-0.1D1 + x2) * t1 * s
      t80 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t79, t76, 0.
     #0D0, 0.0D0, 0.0D0)
      t83 = t5 * t80 * t11 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t76, 0.0D0, -t79, 0.0D0, t83)
      t87 = t4 * t80 * t11
      t90 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t76, 0.0D0, t83)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t68)
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t100 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t68)
      t108 = FJET(XB1, XB2, s, t76, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t112 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t68)
      t116 = FJET(XB1, XB2, s, -t79, 0.0D0, t76, 0.0D0, 0.0D0, t83)
      t120 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2qqbarht7s2em2 = t39 * t38 + t41 * t38 + t50 * pi * t53 / 0.8D
     #1 + t56 * pi * t53 / 0.8D1 + t60 * t38 + t69 * pi * t72 / 0.16D2 +
     # t84 * pi * t87 / 0.16D2 + t90 * pi * t87 / 0.16D2 + t94 * pi * t7
     #2 / 0.16D2 + t98 * t38 + t100 * pi * t53 / 0.8D1 + t104 * pi * t72
     # / 0.16D2 + t108 * pi * t87 / 0.16D2 + t112 * pi * t72 / 0.16D2 + 
     #t116 * pi * t87 / 0.16D2 + t120 * pi * t53 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht7s2em3
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.
     #16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s2em4
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht7s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht7s3e1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t15 = -0.1D1 + x3
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t20 = log(-0.4D1 * t7 * t17)
      t21 = t20 * z
      t22 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t24 = t20 ** 2
      t26 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t29 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t30 = z * t29
      t32 = cos(t11)
      t33 = x3 * z
      t35 = Sqrt(-t33 * t15)
      t39 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t35)
      t43 = log(0.4D1 * t7 * t14)
      t45 = t43 ** 2
      t51 = pi * lh
      t52 = z * t22
      t61 = lh ** 2
      t63 = pi ** 2
      t65 = -0.180D3 * t61 + 0.30D2 * t63
      t66 = pi * t65
      t68 = z * t26 * t39
      t73 = 0.1D1 / x3
      t75 = 0.1D1 / x1
      t78 = t6 * t10
      t79 = t78 * t13
      t81 = log(0.4D1 * t79)
      t86 = t81 ** 2
      t89 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t102 = -0.60D2 * lh * t63 + 0.240D3 * zeta3 + 0.120D3 * t61 * lh
      t103 = pi * t102
      t104 = t4 * t26
      t105 = t103 * t104
      t116 = x2 ** 2
      t117 = x3 * t116
      t118 = t117 * t6
      t121 = log(-0.4D1 * t118 * t17)
      t126 = -0.1D1 + x2
      t127 = t14 * t126
      t130 = log(-0.4D1 * t118 * t127)
      t134 = log(0.4D1 * t117 * t79)
      t139 = t51 * t4
      t144 = 0.1D1 / x2
      t145 = t144 * t75
      t148 = t116 * t6
      t151 = log(0.4D1 * t148 * t14)
      t153 = t151 ** 2
      t158 = log(-0.4D1 * t148 * t127)
      t160 = t158 ** 2
      t176 = t4 * t22
      t182 = -0.180D3 * t51 * t176 - t66 * t104 + 0.90D2 * t5 * t29
      t183 = x3 * t10
      t186 = log(0.4D1 * t183 * t13)
      t190 = log(-0.4D1 * t183 * t13 * t16)
      t195 = t190 ** 2
      t199 = t186 ** 2
      t220 = 0.90D2 * t5 * t22 - 0.180D3 * t51 * t104
      t231 = log(-0.4D1 * t117 * t127)
      t233 = t231 ** 2
      t238 = log(0.4D1 * t117 * t14)
      t240 = t238 ** 2
      t245 = log(-0.4D1 * t117 * t17)
      t246 = t245 * z
      t248 = t245 ** 2
      t266 = t66 * t4
      t272 = t10 * t116
      t273 = t13 * t126
      t276 = log(-0.4D1 * t272 * t273)
      t277 = t276 ** 2
      t280 = log(0.4D1 * t272 * t13)
      t281 = t280 ** 2
      t298 = log(0.4D1 * t14)
      t299 = t298 ** 2
      t300 = t299 * pi
      t304 = t299 * t298 * pi
      t306 = t298 * pi
      t325 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t335 = t63 ** 2
      t336 = t61 ** 2
      t342 = t299 ** 2
      t349 = -(-0.90D2 * t5 * ((t21 * t22 - t24 * z * t26 / 0.2D1 - t30)
     # * t39 + t43 * t22 - t45 * t26 / 0.2D1 - t29) + 0.180D3 * t51 * t4
     # * ((-t52 + t21 * t26) * t39 - t22 + t43 * t26) + t66 * t4 * (-t26
     # - t68)) * t73 * t75 / 0.1440D4 + (t66 * t4 * (t22 - t81 * t26) - 
     #0.90D2 * t5 * (t86 * t22 / 0.2D1 + t89 - t86 * t81 * t26 / 0.6D1 -
     # t81 * t29) + t105 + 0.180D3 * t51 * t4 * (-t81 * t22 + t86 * t26 
     #/ 0.2D1 + t29)) * t75 / 0.1440D4 - (-0.90D2 * t5 * ((-t52 + t121 *
     # z * t26) * t39 - t130 * t26 + t134 * t26) - 0.180D3 * t139 * t68)
     # * t73 * t145 / 0.720D3 + (-0.90D2 * t5 * (-t151 * t22 + t153 * t2
     #6 / 0.2D1 + t158 * t22 - t160 * t26 / 0.2D1) + 0.180D3 * t51 * t4 
     #* (-t151 * t26 + t158 * t26)) * t144 * t75 / 0.720D3 - (t182 * (-t
     #186 - t190 * z * t39) + 0.90D2 * t5 * t26 * (-t195 * t190 * z * t3
     #9 / 0.6D1 - t199 * t186 / 0.6D1) + (-t66 * t176 + 0.90D2 * t5 * t8
     #9 - t105 - 0.180D3 * t51 * t4 * t29) * (0.1D1 + z * t39) + t220 * 
     #(t199 / 0.2D1 + t195 * z * t39 / 0.2D1)) * t73 / 0.2880D4 - (-0.90
     #D2 * t5 * (-t231 * t22 + t233 * t26 / 0.2D1 + t238 * t22 - t240 * 
     #t26 / 0.2D1 + (t246 * t22 - t248 * z * t26 / 0.2D1 - t30) * t39) +
     # 0.180D3 * t51 * t4 * (-t231 * t26 + t238 * t26 + (-t52 + t246 * t
     #26) * t39) - t266 * t68) * t73 * t144 / 0.1440D4 + (t220 * (t277 /
     # 0.2D1 - t281 / 0.2D1) + 0.90D2 * t5 * t26 * (t281 * t280 / 0.6D1 
     #- t277 * t276 / 0.6D1) + t182 * (-t276 + t280)) * t144 / 0.1440D4 
     #+ (0.90D2 * t300 * lh + t103 + 0.15D2 * t304 - t306 * t65) * t4 * 
     #t22 / 0.2880D4 + (-0.180D3 * t306 * lh - 0.45D2 * t300 + t66) * t4
     # * t29 / 0.2880D4 + (0.180D3 * t51 + 0.90D2 * t306) * t4 * t89 / 0
     #.2880D4 - t5 * t325 / 0.32D2 + (-0.30D2 * t304 * lh + t300 * t65 /
     # 0.2D1 - t306 * t102 + pi * (-0.480D3 * lh * zeta3 - t335 - 0.60D2
     # * t336 + 0.60D2 * t61 * t63) - 0.15D2 / 0.4D1 * t342 * pi) * t4 *
     # t26 / 0.2880D4
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t349)
      t352 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t349)
      t354 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t349)
      t356 = x2 * x3
      t357 = 0.1D1 - x3 + t356
      t358 = 0.1D1 / t357
      t359 = t356 * t358
      t360 = t2 * t359
      t362 = t2 * t15 * t358
      t364 = t357 ** 2
      t365 = 0.1D1 / t364
      t367 = t273 * t15 * t365
      t370 = log(0.4D1 * t117 * t10 * t367)
      t371 = t370 * z
      t372 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t362, t360, 0.0D0)
      t374 = t370 ** 2
      t376 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t362, t360, 0.0D0)
      t379 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t362, t360, 0.0D0)
      t382 = t126 * t15
      t384 = Sqrt(t33 * t382)
      t388 = 0.1D1 / (-z - x3 + t356 + 0.2D1 * t32 * t384)
      t392 = z * t372
      t400 = z * t376 * t388
      t409 = log(0.4D1 * t117 * t78 * t367)
      t422 = -(-0.90D2 * t5 * (-t371 * t372 + t374 * z * t376 / 0.2D1 + 
     #z * t379) * t388 + 0.180D3 * t51 * t4 * (t392 - t371 * t376) * t38
     #8 + t266 * t400) * t73 * t144 / 0.1440D4 - (-0.90D2 * t5 * (t392 -
     # t409 * z * t376) * t388 + 0.180D3 * t139 * t400) * t73 * t145 / 0
     #.720D3
      t423 = FJET(XB1, XB2, s, 0.0D0, t360, 0.0D0, -t362, 0.0D0, t422)
      t426 = t1 * x1
      t427 = x1 * z
      t428 = -z - x1 + t427
      t429 = 0.1D1 / t428
      t431 = t126 * s * t426 * t429
      t432 = -0.1D1 + x1
      t433 = t2 * t432
      t435 = x2 * s * t426
      t436 = t1 ** 2
      t437 = s * t436
      t440 = x1 * t432 * t429
      t441 = t437 * t126 * t440
      t442 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t435, t431, 
     #-t433, 0.0D0, -t441)
      t443 = 0.1D1 / t8
      t444 = t6 * t443
      t445 = t117 * t444
      t446 = t432 ** 2
      t447 = t13 * t446
      t449 = t447 * t429 * t126
      t452 = log(0.4D1 * t445 * t449)
      t453 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t435, t431, 
     #-t433, 0.0D0, -t441)
      t458 = t4 * t453
      t464 = t148 * t443
      t467 = log(0.4D1 * t464 * t449)
      t469 = t467 ** 2
      t472 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t435, t431, 
     #-t433, 0.0D0, -t441)
      t486 = -(-0.90D2 * t5 * (-t442 + t452 * t453) - 0.180D3 * t51 * t4
     #58) * t73 * t145 / 0.720D3 + (-0.90D2 * t5 * (-t467 * t442 + t469 
     #* t453 / 0.2D1 + t472) + 0.180D3 * t51 * t4 * (t442 - t467 * t453)
     # + t66 * t458) * t144 * t75 / 0.720D3
      t487 = FJET(XB1, XB2, s, 0.0D0, t431, -t433, t435, -t441, t486)
      t490 = t2 * x1 * t429
      t491 = t437 * t440
      t492 = t7 * t443
      t494 = t447 * t429 * t16
      t497 = log(0.4D1 * t492 * t494)
      t498 = t497 * z
      t499 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t490
     #, -t433, 0.0D0, t491)
      t502 = t497 ** 2
      t504 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t490
     #, -t433, 0.0D0, t491)
      t505 = t428 * t504
      t508 = z * t428
      t509 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t490
     #, -t433, 0.0D0, t491)
      t512 = x3 * x1
      t513 = t512 * z
      t514 = x1 * t8
      t515 = x3 * t8
      t516 = t515 * x1
      t518 = 0.2D1 * t7 * z
      t519 = t7 * t8
      t520 = x3 * t428
      t522 = Sqrt(t520 * t15)
      t527 = 0.1D1 / (-t427 - t513 + t514 + t516 - t7 - t33 + t518 - t51
     #9 - t8 + 0.2D1 * t32 * t522 * z)
      t529 = t447 * t429
      t532 = log(-0.4D1 * t492 * t529)
      t534 = t532 ** 2
      t540 = t508 * t499
      t552 = t4 * (-t508 * t504 * t527 + t504)
      t557 = (-0.90D2 * t5 * (-(-t498 * t428 * t499 + t502 * z * t505 / 
     #0.2D1 + t508 * t509) * t527 - t532 * t499 + t534 * t504 / 0.2D1 + 
     #t509) + 0.180D3 * t51 * t4 * (-(t540 - t498 * t505) * t527 + t499 
     #- t532 * t504) + t66 * t552) * t73 * t75 / 0.1440D4
      t560 = log(-0.4D1 * t444 * t529)
      t562 = -t499 + t560 * t504
      t565 = t560 ** 2
      t568 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t490
     #, -t433, 0.0D0, t491)
      t573 = -t565 * t499 / 0.2D1 - t568 + t565 * t560 * t504 / 0.6D1 + 
     #t560 * t509
      t576 = t4 * t504
      t577 = t103 * t576
      t581 = t560 * t499 - t565 * t504 / 0.2D1 - t509
      t589 = t446 * t429
      t593 = log(-0.4D1 * t118 * t443 * t13 * t589)
      t597 = log(0.4D1 * t445 * t494)
      t610 = (-0.90D2 * t5 * (t499 - t593 * t504 - (t540 - t597 * z * t5
     #05) * t527) + 0.180D3 * t51 * t552) * t73 * t145 / 0.720D3
      t613 = log(-0.4D1 * t464 * t529)
      t615 = t613 ** 2
      t618 = t613 * t499 - t615 * t504 / 0.2D1 - t509
      t622 = -t499 + t613 * t504
      t626 = t66 * t576
      t630 = (-0.90D2 * t5 * t618 + 0.180D3 * t51 * t4 * t622 - t626) * 
     #t144 * t75 / 0.720D3
      t631 = -t557 + (t66 * t4 * t562 - 0.90D2 * t5 * t573 - t577 + 0.18
     #0D3 * t51 * t4 * t581) * t75 / 0.1440D4 - t610 + t630
      t632 = FJET(XB1, XB2, s, 0.0D0, -t433, -t490, 0.0D0, t491, t631)
      t634 = FJET(XB1, XB2, s, 0.0D0, -t490, -t433, 0.0D0, t491, t631)
      t636 = FJET(XB1, XB2, s, 0.0D0, -t362, 0.0D0, t360, 0.0D0, t422)
      t638 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t349)
      t640 = FJET(XB1, XB2, s, t435, -t433, t431, 0.0D0, -t441, t486)
      t642 = FJET(XB1, XB2, s, t360, 0.0D0, -t362, 0.0D0, 0.0D0, t422)
      t644 = FJET(XB1, XB2, s, t431, 0.0D0, t435, -t433, -t441, t486)
      t649 = t15 * s * t1 * t432 * t358
      t650 = t2 * x1
      t652 = Sqrt(-t520 * t382)
      t653 = t32 * t652
      t659 = t650 * x2 * (-x3 + t356 - z + t33 - x1 + t512 + t427 - t513
     # + 0.2D1 * t653) * t429 * t358
      t660 = t433 * t359
      t661 = t117 * t427
      t664 = t117 * x1
      t669 = t650 * (-t661 - x2 + t356 + 0.2D1 * t653 * x2 + t664 + t117
     # * z + 0.1D1 - x3) * t429 * t358
      t670 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t659, -t669,
     # t649, -t660, -t441)
      t678 = log(-0.4D1 * t117 * t444 * t13 * t589 * t382 * t365)
      t680 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t659, -t669,
     # t649, -t660, -t441)
      t683 = x2 * x1
      t684 = t683 * z
      t685 = -z + t684 - t683
      t689 = t8 * x2
      t691 = t6 * x2
      t701 = t427 - t514 + t7 + t33 + 0.2D1 * t653 * t684 + t684 - t689 
     #* x1 - 0.2D1 * t691 * z + t6 * t8 * x2 - 0.2D1 * t653 * z - t7 * x
     #2 - t356 * z + t356 * x1
      t711 = t8 + t513 - t516 - t518 + t519 + t691 - t664 + t661 - 0.2D1
     # * t356 * t427 + 0.2D1 * t7 * x2 * z + t515 * t683 - t7 * t689 - 0
     #.2D1 * t653 * t683
      t713 = 0.1D1 / (t701 + t711)
      t722 = -0.90D2 * t5 * (t428 * t670 - t678 * t428 * t680) * t685 * 
     #t713 + 0.180D3 * t139 * t428 * t680 * t685 * t713
      t725 = t722 * t73 * t145 / 0.720D3
      t726 = FJET(XB1, XB2, s, t649, t659, -t660, -t669, -t441, -t725)
      t729 = t73 * t144 * t75
      t732 = FJET(XB1, XB2, s, t659, t649, -t669, -t660, -t441, -t725)
      t748 = (t66 * t4 * t562 - 0.90D2 * t5 * t573 - t577 + 0.180D3 * t5
     #1 * t4 * t581) * t75 / 0.1440D4
      t749 = -t557 + t748 - t610 + t630
      t750 = FJET(XB1, XB2, s, -t433, 0.0D0, 0.0D0, -t490, t491, t749)
      t752 = FJET(XB1, XB2, s, -t433, t435, 0.0D0, t431, -t441, t486)
      t765 = -t557 + t748 - t610 + (-0.90D2 * t5 * t618 + 0.180D3 * t51 
     #* t4 * t622 - t626) * t144 * t75 / 0.720D3
      t766 = FJET(XB1, XB2, s, -t490, 0.0D0, 0.0D0, -t433, t491, t765)
      t768 = FJET(XB1, XB2, s, -t362, 0.0D0, t360, 0.0D0, 0.0D0, t422)
      t770 = FJET(XB1, XB2, s, -t669, -t660, t659, t649, -t441, -t725)
      t774 = FJET(XB1, XB2, s, -t660, -t669, t649, t659, -t441, -t725)
      rrgg2qqbarht7s3e1 = t350 * t349 + t352 * t349 + t354 * t349 + t423
     # * t422 + t487 * t486 + t632 * t631 + t634 * t631 + t636 * t422 + 
     #t638 * t349 + t640 * t486 + t642 * t422 + t644 * t486 - t726 * t72
     #2 * t729 / 0.720D3 - t732 * t722 * t729 / 0.720D3 + t750 * t749 + 
     #t752 * t486 + t766 * t765 + t768 * t422 - t770 * t722 * t729 / 0.7
     #20D3 - t774 * t722 * t729 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht7s3e0
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t7 = z * t6
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t24 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t27 = cos(t13)
      t28 = x3 * z
      t30 = Sqrt(-t28 * t17)
      t34 = 0.1D1 / (-z - x3 + 0.2D1 * t27 * t30)
      t38 = log(0.4D1 * t9 * t16)
      t43 = pi * lh
      t44 = z * t24
      t45 = t44 * t34
      t51 = 0.1D1 / x3
      t53 = 0.1D1 / x1
      t58 = 0.1D1 / x2
      t59 = t58 * t53
      t63 = x2 ** 2
      t64 = t63 * t8
      t67 = log(0.4D1 * t64 * t16)
      t69 = -0.1D1 + x2
      t70 = t16 * t69
      t73 = log(-0.4D1 * t64 * t70)
      t83 = log(0.4D1 * t8 * t12 * t15)
      t85 = t83 ** 2
      t88 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t97 = lh ** 2
      t99 = pi ** 2
      t101 = -0.180D3 * t97 + 0.30D2 * t99
      t102 = pi * t101
      t103 = t4 * t24
      t104 = t102 * t103
      t108 = x3 * t12
      t111 = log(0.4D1 * t108 * t15)
      t112 = t111 ** 2
      t116 = log(-0.4D1 * t108 * t15 * t18)
      t117 = t116 ** 2
      t129 = 0.90D2 * t5 * t6 - 0.180D3 * t43 * t103
      t147 = log(0.4D1 * t16)
      t148 = t147 * pi
      t151 = t147 ** 2
      t152 = t151 * pi
      t158 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t184 = x3 * t63
      t187 = log(-0.4D1 * t184 * t70)
      t191 = log(0.4D1 * t184 * t16)
      t195 = log(-0.4D1 * t184 * t19)
      t203 = t43 * t4
      t210 = t12 * t63
      t211 = t15 * t69
      t214 = log(-0.4D1 * t210 * t211)
      t215 = t214 ** 2
      t218 = log(0.4D1 * t210 * t15)
      t219 = t218 ** 2
      t230 = -(-0.90D2 * t5 * ((-t7 + t22 * z * t24) * t34 - t6 + t38 * 
     #t24) + 0.180D3 * t43 * t4 * (-t24 - t45)) * t51 * t53 / 0.1440D4 -
     # t5 * t44 * t34 * t51 * t59 / 0.8D1 - t5 * (-t67 * t24 + t73 * t24
     #) * t58 * t53 / 0.8D1 + (-0.90D2 * t5 * (-t83 * t6 + t85 * t24 / 0
     #.2D1 + t88) + 0.180D3 * t43 * t4 * (t6 - t83 * t24) + t104) * t53 
     #/ 0.1440D4 - (0.90D2 * t5 * t24 * (t112 / 0.2D1 + t117 * z * t34 /
     # 0.2D1) + t129 * (-t111 - t116 * z * t34) + (-0.180D3 * t43 * t4 *
     # t6 - t104 + 0.90D2 * t5 * t88) * (0.1D1 + z * t34)) * t51 / 0.288
     #0D4 + (-0.180D3 * t148 * lh - 0.45D2 * t152 + t102) * t4 * t6 / 0.
     #2880D4 - t5 * t158 / 0.32D2 + (0.90D2 * t152 * lh + pi * (-0.60D2 
     #* lh * t99 + 0.240D3 * zeta3 + 0.120D3 * t97 * lh) + 0.15D2 * t151
     # * t147 * pi - t148 * t101) * t4 * t24 / 0.2880D4 + (0.180D3 * t43
     # + 0.90D2 * t148) * t4 * t88 / 0.2880D4 - (-0.90D2 * t5 * (-t187 *
     # t24 + t191 * t24 + (-t7 + t195 * z * t24) * t34) - 0.180D3 * t203
     # * t45) * t51 * t58 / 0.1440D4 + (0.90D2 * t5 * t24 * (t215 / 0.2D
     #1 - t219 / 0.2D1) + t129 * (-t214 + t218)) * t58 / 0.1440D4
      t231 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t230)
      t233 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t230)
      t235 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t230)
      t237 = x2 * x3
      t238 = 0.1D1 - x3 + t237
      t239 = 0.1D1 / t238
      t240 = t237 * t239
      t241 = t2 * t240
      t243 = t2 * t17 * t239
      t244 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t243, t241, 0.0D0)
      t245 = z * t244
      t247 = t69 * t17
      t249 = Sqrt(t28 * t247)
      t253 = 0.1D1 / (-z - x3 + t237 + 0.2D1 * t27 * t249)
      t258 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t243, t241, 0.0D0)
      t261 = t238 ** 2
      t267 = log(0.4D1 * t184 * t12 * t211 * t17 / t261)
      t281 = t5 * t245 * t253 * t51 * t59 / 0.8D1 - (-0.90D2 * t5 * (z *
     # t258 - t267 * z * t244) * t253 + 0.180D3 * t203 * t245 * t253) * 
     #t51 * t58 / 0.1440D4
      t282 = FJET(XB1, XB2, s, 0.0D0, t241, 0.0D0, -t243, 0.0D0, t281)
      t285 = t1 * x1
      t286 = x1 * z
      t287 = -z - x1 + t286
      t288 = 0.1D1 / t287
      t290 = t69 * s * t285 * t288
      t291 = -0.1D1 + x1
      t292 = t2 * t291
      t294 = x2 * s * t285
      t295 = t1 ** 2
      t296 = s * t295
      t299 = x1 * t291 * t288
      t300 = t296 * t69 * t299
      t301 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t294, t290, 
     #-t292, 0.0D0, -t300)
      t304 = t51 * t58 * t53
      t307 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t294, t290, 
     #-t292, 0.0D0, -t300)
      t308 = 0.1D1 / t10
      t309 = t64 * t308
      t311 = t291 ** 2
      t316 = log(0.4D1 * t309 * t15 * t288 * t311 * t69)
      t328 = -t5 * t301 * t304 / 0.8D1 + (-0.90D2 * t5 * (t307 - t316 * 
     #t301) + 0.180D3 * t43 * t4 * t301) * t58 * t53 / 0.720D3
      t329 = FJET(XB1, XB2, s, 0.0D0, t290, -t292, t294, -t300, t328)
      t332 = t2 * x1 * t288
      t333 = t296 * t299
      t334 = z * t287
      t335 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t332
     #, -t292, 0.0D0, t333)
      t337 = t9 * t308
      t338 = t15 * t311
      t343 = log(0.4D1 * t337 * t338 * t288 * t18)
      t345 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t332
     #, -t292, 0.0D0, t333)
      t349 = x3 * x1
      t350 = t349 * z
      t351 = x1 * t10
      t352 = x3 * t10
      t353 = t352 * x1
      t355 = 0.2D1 * t9 * z
      t356 = t9 * t10
      t357 = x3 * t287
      t359 = Sqrt(t357 * t17)
      t364 = 0.1D1 / (-t286 - t350 + t351 + t353 - t9 - t28 + t355 - t35
     #6 - t10 + 0.2D1 * t27 * t359 * z)
      t366 = t338 * t288
      t369 = log(-0.4D1 * t337 * t366)
      t376 = -t334 * t345 * t364 + t345
      t383 = (-0.90D2 * t5 * (-(t334 * t335 - t343 * z * t287 * t345) * 
     #t364 + t335 - t369 * t345) + 0.180D3 * t43 * t4 * t376) * t51 * t5
     #3 / 0.1440D4
      t386 = t5 * t376 * t304 / 0.8D1
      t389 = log(-0.4D1 * t309 * t366)
      t391 = -t335 + t389 * t345
      t394 = t4 * t345
      t396 = 0.180D3 * t43 * t394
      t400 = (-0.90D2 * t5 * t391 - t396) * t58 * t53 / 0.720D3
      t404 = log(-0.4D1 * t8 * t308 * t366)
      t406 = t404 ** 2
      t409 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t332
     #, -t292, 0.0D0, t333)
      t410 = t404 * t335 - t406 * t345 / 0.2D1 - t409
      t414 = -t335 + t404 * t345
      t418 = t102 * t394
      t422 = -t383 + t386 + t400 + (-0.90D2 * t5 * t410 + 0.180D3 * t43 
     #* t4 * t414 - t418) * t53 / 0.1440D4
      t423 = FJET(XB1, XB2, s, 0.0D0, -t292, -t332, 0.0D0, t333, t422)
      t425 = FJET(XB1, XB2, s, 0.0D0, -t332, -t292, 0.0D0, t333, t422)
      t427 = FJET(XB1, XB2, s, 0.0D0, -t243, 0.0D0, t241, 0.0D0, t281)
      t429 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t230)
      t431 = FJET(XB1, XB2, s, t294, -t292, t290, 0.0D0, -t300, t328)
      t433 = FJET(XB1, XB2, s, t241, 0.0D0, -t243, 0.0D0, 0.0D0, t281)
      t435 = FJET(XB1, XB2, s, t290, 0.0D0, t294, -t292, -t300, t328)
      t440 = t17 * s * t1 * t291 * t239
      t441 = t2 * x1
      t443 = Sqrt(-t357 * t247)
      t444 = t27 * t443
      t450 = t441 * x2 * (-x3 + t237 - z + t28 - x1 + t349 + t286 - t350
     # + 0.2D1 * t444) * t288 * t239
      t451 = t292 * t240
      t452 = t184 * t286
      t455 = t184 * x1
      t460 = t441 * (-t452 - x2 + t237 + 0.2D1 * t444 * x2 + t455 + t184
     # * z + 0.1D1 - x3) * t288 * t239
      t461 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t450, -t460,
     # t440, -t451, -t300)
      t464 = x2 * x1
      t465 = t464 * z
      t473 = t10 * x2
      t477 = t452 - 0.2D1 * t237 * t286 + 0.2D1 * t9 * x2 * z + t352 * t
     #464 - t9 * t473 - 0.2D1 * t444 * t464 + t350 - t353 - t355 + t356 
     #+ t286 - t351 + t9
      t478 = t8 * x2
      t491 = t28 + t10 + t478 + 0.2D1 * t444 * t465 - t455 + t465 - t473
     # * x1 - 0.2D1 * t478 * z + t8 * t10 * x2 - 0.2D1 * t444 * z - t9 *
     # x2 - t237 * z + t237 * x1
      t495 = (-z + t465 - t464) / (t477 + t491) * t304
      t497 = t5 * t287 * t461 * t495 / 0.8D1
      t498 = FJET(XB1, XB2, s, t440, t450, -t451, -t460, -t300, t497)
      t501 = t4 * t287 * t461
      t505 = FJET(XB1, XB2, s, t450, t440, -t460, -t451, -t300, t497)
      t519 = (-0.90D2 * t5 * t410 + 0.180D3 * t43 * t4 * t414 - t418) * 
     #t53 / 0.1440D4
      t520 = -t383 + t386 + t400 + t519
      t521 = FJET(XB1, XB2, s, -t292, 0.0D0, 0.0D0, -t332, t333, t520)
      t523 = FJET(XB1, XB2, s, -t292, t294, 0.0D0, t290, -t300, t328)
      t532 = -t383 + t386 + (-0.90D2 * t5 * t391 - t396) * t58 * t53 / 0
     #.720D3 + t519
      t533 = FJET(XB1, XB2, s, -t332, 0.0D0, 0.0D0, -t292, t333, t532)
      t535 = FJET(XB1, XB2, s, -t243, 0.0D0, t241, 0.0D0, 0.0D0, t281)
      t537 = FJET(XB1, XB2, s, -t460, -t451, t450, t440, -t300, t497)
      t542 = FJET(XB1, XB2, s, -t451, -t460, t440, t450, -t300, t497)
      rrgg2qqbarht7s3e0 = t231 * t230 + t233 * t230 + t235 * t230 + t282
     # * t281 + t329 * t328 + t423 * t422 + t425 * t422 + t427 * t281 + 
     #t429 * t230 + t431 * t328 + t433 * t281 + t435 * t328 + t498 * pi 
     #* t501 * t495 / 0.8D1 + t505 * pi * t501 * t495 / 0.8D1 + t521 * t
     #520 + t523 * t328 + t533 * t532 + t535 * t281 + t537 * pi * t501 *
     # t495 / 0.8D1 + t542 * pi * t501 * t495 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht7s3em1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
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
      t37 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t40 = pi * lh
      t43 = 0.180D3 * t40 * t4 * t6
      t49 = 0.1D1 / x3
      t52 = x1 ** 2
      t56 = log(0.4D1 * t52 * t9 * t13)
      t62 = 0.1D1 / x1
      t72 = t5 * z
      t74 = 0.1D1 / x2
      t75 = t49 * t74
      t79 = x2 ** 2
      t80 = t9 * t79
      t81 = -0.1D1 + x2
      t85 = log(-0.4D1 * t80 * t13 * t81)
      t88 = log(0.4D1 * t80 * t13)
      t97 = log(0.4D1 * t9 * t13)
      t98 = t97 * pi
      t106 = t97 ** 2
      t109 = lh ** 2
      t111 = pi ** 2
      t119 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t122 = -(0.90D2 * t5 * t6 * (-t16 - t22 * z * t31) + (0.90D2 * t5 
     #* t37 - t43) * (0.1D1 + z * t31)) * t49 / 0.2880D4 + (-0.90D2 * t5
     # * (t37 - t56 * t6) + t43) * t62 / 0.1440D4 + t5 * (-t6 - z * t6 *
     # t31) * t49 * t62 / 0.16D2 - t72 * t6 * t31 * t75 / 0.16D2 + t5 * 
     #t6 * (-t85 + t88) * t74 / 0.16D2 + (0.180D3 * t40 + 0.90D2 * t98) 
     #* t4 * t37 / 0.2880D4 + (-0.180D3 * t98 * lh - 0.45D2 * t106 * pi 
     #+ pi * (-0.180D3 * t109 + 0.30D2 * t111)) * t4 * t6 / 0.2880D4 - t
     #5 * t119 / 0.32D2
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t122)
      t125 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t122)
      t127 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t122)
      t129 = x2 * x3
      t131 = 0.1D1 / (0.1D1 - x3 + t129)
      t133 = t2 * t129 * t131
      t135 = t2 * t17 * t131
      t136 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t135, t133, 0.0D0)
      t139 = Sqrt(t25 * t81 * t17)
      t145 = t136 / (-z - x3 + t129 + 0.2D1 * t24 * t139) * t75
      t147 = t72 * t145 / 0.16D2
      t148 = FJET(XB1, XB2, s, 0.0D0, t133, 0.0D0, -t135, 0.0D0, t147)
      t150 = t4 * z
      t155 = t1 * x1
      t156 = x1 * z
      t157 = -z - x1 + t156
      t158 = 0.1D1 / t157
      t160 = t81 * s * t155 * t158
      t161 = -0.1D1 + x1
      t162 = t2 * t161
      t164 = x2 * s * t155
      t165 = t1 ** 2
      t166 = s * t165
      t169 = x1 * t161 * t158
      t170 = t166 * t81 * t169
      t171 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t164, t160, 
     #-t162, 0.0D0, -t170)
      t173 = t171 * t74 * t62
      t175 = t5 * t173 / 0.8D1
      t176 = FJET(XB1, XB2, s, 0.0D0, t160, -t162, t164, -t170, -t175)
      t182 = t2 * x1 * t158
      t183 = t166 * t169
      t184 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t182
     #, -t162, 0.0D0, t183)
      t188 = t5 * t184 * t74 * t62 / 0.8D1
      t189 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t182
     #, -t162, 0.0D0, t183)
      t193 = t161 ** 2
      t197 = log(-0.4D1 * t52 / t7 * t13 * t158 * t193)
      t199 = -t189 + t197 * t184
      t204 = 0.180D3 * t40 * t4 * t184
      t214 = x3 * t52
      t220 = Sqrt(x3 * t157 * t17)
      t232 = t5 * (-z * t157 * t184 / (-t156 - x3 * x1 * z + x1 * t7 + x
     #3 * t7 * x1 - t214 - t25 + 0.2D1 * t214 * z - t214 * t7 - t7 + 0.2
     #D1 * t24 * t220 * z) + t184) * t49 * t62 / 0.16D2
      t233 = t188 + (-0.90D2 * t5 * t199 - t204) * t62 / 0.1440D4 + t232
      t234 = FJET(XB1, XB2, s, 0.0D0, -t162, -t182, 0.0D0, t183, t233)
      t236 = FJET(XB1, XB2, s, 0.0D0, -t182, -t162, 0.0D0, t183, t233)
      t238 = FJET(XB1, XB2, s, 0.0D0, -t135, 0.0D0, t133, 0.0D0, t147)
      t243 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t122)
      t245 = FJET(XB1, XB2, s, t164, -t162, t160, 0.0D0, -t170, -t175)
      t250 = FJET(XB1, XB2, s, t133, 0.0D0, -t135, 0.0D0, 0.0D0, t147)
      t255 = FJET(XB1, XB2, s, t160, 0.0D0, t164, -t162, -t170, -t175)
      t266 = t188 + (-0.90D2 * t5 * t199 - t204) * t62 / 0.1440D4 + t232
      t267 = FJET(XB1, XB2, s, -t162, 0.0D0, 0.0D0, -t182, t183, t266)
      t269 = FJET(XB1, XB2, s, -t162, t164, 0.0D0, t160, -t170, -t175)
      t274 = FJET(XB1, XB2, s, -t182, 0.0D0, 0.0D0, -t162, t183, t266)
      t276 = FJET(XB1, XB2, s, -t135, 0.0D0, t133, 0.0D0, 0.0D0, t147)
      rrgg2qqbarht7s3em1 = t123 * t122 + t125 * t122 + t127 * t122 + t14
     #8 * pi * t150 * t145 / 0.16D2 - t176 * pi * t4 * t173 / 0.8D1 + t2
     #34 * t233 + t236 * t233 + t238 * pi * t150 * t145 / 0.16D2 + t243 
     #* t122 - t245 * pi * t4 * t173 / 0.8D1 + t250 * pi * t150 * t145 /
     # 0.16D2 - t255 * pi * t4 * t173 / 0.8D1 + t267 * t266 - t269 * pi 
     #* t4 * t173 / 0.8D1 + t274 * t266 + t276 * pi * t150 * t145 / 0.16
     #D2

      end function



      doubleprecision function rrgg2qqbarht7s3em2
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = x4 * pi
      t12 = cos(t11)
      t16 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t28 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t33 = z ** 2
      t36 = Sin(t11)
      t37 = t36 ** 2
      t40 = log(0.4D1 / t33 / z * t37)
      t47 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * (0.1D1 + z / (-z - x3 + 0
     #.2D1 * t12 * t16)) / x3 / 0.32D2 - t5 * t28 / 0.32D2 + (0.180D3 * 
     #pi * lh + 0.90D2 * t40 * pi) * t4 * t6 / 0.2880D4
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t47)
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t47)
      t52 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t47)
      t54 = -0.1D1 + x1
      t55 = t2 * t54
      t58 = 0.1D1 / (-z - x1 + x1 * z)
      t60 = t2 * x1 * t58
      t61 = t1 ** 2
      t65 = s * t61 * x1 * t54 * t58
      t66 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t60, 
     #-t55, 0.0D0, t65)
      t69 = t5 * t66 * t7 / 0.16D2
      t70 = FJET(XB1, XB2, s, 0.0D0, -t55, -t60, 0.0D0, t65, t69)
      t73 = t4 * t66 * t7
      t76 = FJET(XB1, XB2, s, 0.0D0, -t60, -t55, 0.0D0, t65, t69)
      t80 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t47)
      t82 = FJET(XB1, XB2, s, -t55, 0.0D0, 0.0D0, -t60, t65, t69)
      t86 = FJET(XB1, XB2, s, -t60, 0.0D0, 0.0D0, -t55, t65, t69)
      rrgg2qqbarht7s3em2 = t48 * t47 + t50 * t47 + t52 * t47 + t70 * pi 
     #* t73 / 0.16D2 + t76 * pi * t73 / 0.16D2 + t80 * t47 + t82 * pi * 
     #t73 / 0.16D2 + t86 * pi * t73 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s3em3
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.32D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s3em3 = -t9 * pi * t11 / 0.32D2 - t13 * pi * t11 / 0.
     #32D2 - t16 * pi * t11 / 0.32D2 - t19 * pi * t11 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarht7s3em4
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht7s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht7s4e1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t18 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t24 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t28 = pi * lh
      t34 = lh ** 2
      t36 = pi ** 2
      t38 = -0.180D3 * t34 + 0.30D2 * t36
      t39 = pi * t38
      t40 = t4 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t6 * t10
      t49 = t48 * t13
      t51 = log(0.4D1 * t49)
      t56 = t51 ** 2
      t59 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t72 = -0.60D2 * lh * t36 + 0.240D3 * zeta3 + 0.120D3 * t34 * lh
      t73 = pi * t72
      t74 = t73 * t40
      t85 = t7 * x2
      t86 = -0.1D1 + x2
      t87 = t86 ** 2
      t88 = t14 * t87
      t91 = log(0.4D1 * t85 * t88)
      t93 = x2 * x3
      t96 = log(0.4D1 * t93 * t49)
      t100 = 0.1D1 / x2
      t102 = t43 * t100 * t45
      t105 = t6 * x2
      t108 = log(0.4D1 * t105 * t14)
      t110 = t108 ** 2
      t115 = log(0.4D1 * t105 * t88)
      t117 = t115 ** 2
      t133 = x3 * t10
      t136 = log(0.4D1 * t133 * t13)
      t141 = t136 ** 2
      t163 = log(0.4D1 * t93 * t14)
      t165 = t163 ** 2
      t170 = log(0.4D1 * t93 * t88)
      t172 = t170 ** 2
      t193 = x2 * t10
      t196 = log(0.4D1 * t193 * t13)
      t197 = t196 ** 2
      t198 = t13 * t87
      t201 = log(0.4D1 * t193 * t198)
      t202 = t201 ** 2
      t225 = log(0.4D1 * t14)
      t226 = t225 ** 2
      t227 = t226 * pi
      t231 = t226 * t225 * pi
      t233 = t225 * pi
      t252 = rrgg2qqbarh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t262 = t36 ** 2
      t263 = t34 ** 2
      t269 = t226 ** 2
      t276 = -(-0.90D2 * t5 * (t17 * t18 - t20 * t21 / 0.2D1 - t24) + 0.
     #180D3 * t28 * t4 * (-t18 + t17 * t21) - t41) * t43 * t45 / 0.720D3
     # - (t39 * t4 * (-t18 + t51 * t21) - 0.90D2 * t5 * (-t56 * t18 / 0.
     #2D1 - t59 + t56 * t51 * t21 / 0.6D1 + t51 * t24) - t74 + 0.180D3 *
     # t28 * t4 * (t51 * t18 - t56 * t21 / 0.2D1 - t24)) * t45 / 0.720D3
     # - t5 * (t91 * t21 - t96 * t21) * t102 / 0.8D1 - (-0.90D2 * t5 * (
     #t108 * t18 - t110 * t21 / 0.2D1 - t115 * t18 + t117 * t21 / 0.2D1)
     # + 0.180D3 * t28 * t4 * (t108 * t21 - t115 * t21)) * t100 * t45 / 
     #0.720D3 - (t39 * t4 * (-t18 + t136 * t21) - 0.90D2 * t5 * (-t141 *
     # t18 / 0.2D1 - t59 + t141 * t136 * t21 / 0.6D1 + t136 * t24) - t74
     # + 0.180D3 * t28 * t4 * (t136 * t18 - t141 * t21 / 0.2D1 - t24)) *
     # t43 / 0.1440D4 - (-0.90D2 * t5 * (t163 * t18 - t165 * t21 / 0.2D1
     # - t170 * t18 + t172 * t21 / 0.2D1) + 0.180D3 * t28 * t4 * (t163 *
     # t21 - t170 * t21)) * t43 * t100 / 0.1440D4 - ((0.90D2 * t5 * t18 
     #- 0.180D3 * t28 * t40) * (t197 / 0.2D1 - t202 / 0.2D1) + 0.90D2 * 
     #t5 * t21 * (t202 * t201 / 0.6D1 - t197 * t196 / 0.6D1) + (-0.180D3
     # * t28 * t4 * t18 - t41 + 0.90D2 * t5 * t24) * (-t196 + t201)) * t
     #100 / 0.1440D4 + (0.90D2 * t227 * lh + t73 + 0.15D2 * t231 - t233 
     #* t38) * t4 * t18 / 0.1440D4 + (-0.180D3 * t233 * lh - 0.45D2 * t2
     #27 + t39) * t4 * t24 / 0.1440D4 + (0.180D3 * t28 + 0.90D2 * t233) 
     #* t4 * t59 / 0.1440D4 - t5 * t252 / 0.16D2 + (-0.30D2 * t231 * lh 
     #+ t227 * t38 / 0.2D1 - t233 * t72 + pi * (-0.480D3 * lh * zeta3 - 
     #t262 - 0.60D2 * t263 + 0.60D2 * t34 * t36) - 0.15D2 / 0.4D1 * t269
     # * pi) * t4 * t21 / 0.1440D4
      t277 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t276)
      t280 = -0.1D1 + t93
      t281 = 0.1D1 / t280
      t282 = x3 * t86 * t281
      t283 = t2 * t282
      t284 = -0.1D1 + x3
      t286 = t2 * t284 * t281
      t287 = t280 ** 2
      t288 = 0.1D1 / t287
      t290 = t93 * t284 * t288
      t293 = log(-0.4D1 * t88 * t290)
      t294 = t293 * z
      t295 = cos(t11)
      t296 = x3 * z
      t297 = x2 * t284
      t299 = Sqrt(-t296 * t297)
      t303 = 0.1D1 / (-z - t93 + 0.2D1 * t295 * t299)
      t304 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t286, t283, 0.0D0)
      t307 = t293 ** 2
      t309 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t286, t283, 0.0D0)
      t310 = t303 * t309
      t313 = z * t303
      t314 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t286, t283, 0.0D0)
      t319 = t313 * t304
      t326 = t313 * t309
      t335 = log(-0.4D1 * t48 * t198 * t290)
      t341 = t28 * t4
      t346 = t100 * t45
      t349 = -(-0.90D2 * t5 * (-t294 * t303 * t304 + t307 * z * t310 / 0
     #.2D1 + t313 * t314) + 0.180D3 * t28 * t4 * (t319 - t294 * t310) + 
     #t39 * t4 * t326) * t43 * t100 / 0.1440D4 + (-0.90D2 * t5 * (-t319 
     #+ t335 * z * t310) - 0.180D3 * t341 * t326) * t43 * t346 / 0.720D3
      t350 = FJET(XB1, XB2, s, 0.0D0, t283, 0.0D0, t286, 0.0D0, t349)
      t352 = t284 * s
      t353 = -0.1D1 + x1
      t354 = t1 * t353
      t356 = t352 * t354 * t281
      t357 = t2 * x1
      t358 = x3 * x1
      t359 = x1 * z
      t360 = t358 * z
      t361 = -z - x1 + t359
      t364 = Sqrt(x3 * t361 * t297)
      t365 = t295 * t364
      t366 = 0.2D1 * t365
      t369 = 0.1D1 / t361
      t372 = t357 * t86 * (-t93 - z + t296 - x1 + t358 + t359 - t360 + t
     #366) * t369 * t281
      t373 = t2 * t353
      t374 = t373 * t282
      t379 = x2 ** 2
      t380 = x3 * t379
      t381 = t380 * t359
      t382 = t380 * x1
      t384 = t93 * z
      t386 = t93 * x1
      t388 = 0.2D1 * t365 * x2 - 0.2D1 * t93 * t359 + t381 + t93 - t382 
     #- t380 * z + t360 + 0.2D1 * t384 + 0.2D1 * t386 - x2 - t366 - t296
     # - t358
      t391 = t357 * t388 * t369 * t281
      t392 = t1 ** 2
      t397 = s * t392 * x2 * x1 * t353 * t369
      t398 = x2 * x1
      t399 = t398 * z
      t400 = z + t399 - t359 + x1 - t398
      t401 = t361 * t400
      t402 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t372, -t391,
     # -t356, -t374, t397)
      t404 = t6 * t13
      t405 = 0.1D1 / t8
      t406 = t404 * t405
      t408 = t353 ** 2
      t409 = t408 * t369
      t415 = log(0.4D1 * t93 * t406 * t409 * t87 * t284 * t288)
      t417 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t372, -t391,
     # -t356, -t374, t397)
      t426 = t8 * x2
      t432 = t6 * t8
      t437 = t105 - t8 + 0.2D1 * t7 * x2 * z + x3 * t8 * t398 - t381 - t
     #7 * t426 + 0.2D1 * x1 * t8 + 0.2D1 * t6 * z - t432 - t426 * x1 - 0
     #.2D1 * t105 * z + t432 * x2
      t449 = t382 - t85 + t399 - t384 - t386 - 0.2D1 * t359 + 0.2D1 * t3
     #65 * z + 0.2D1 * t365 * x1 - t6 - 0.2D1 * t365 * t398 - 0.2D1 * t3
     #65 * t359 + 0.2D1 * t365 * t399
      t451 = 0.1D1 / (t437 + t449)
      t459 = 0.90D2 * t5 * (t401 * t402 - t415 * t361 * t400 * t417) * t
     #451 - 0.180D3 * t341 * t401 * t417 * t451
      t462 = t459 * t43 * t346 / 0.720D3
      t463 = FJET(XB1, XB2, s, -t356, t372, -t374, -t391, t397, t462)
      t468 = x3 * s * t354
      t469 = t2 * t358
      t470 = t352 * t354
      t471 = t1 * x1
      t472 = t352 * t471
      t473 = x3 * t284
      t477 = log(0.4D1 * t406 * t473 * t409)
      t478 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t472, t469,
     # t470, -t468, 0.0D0)
      t480 = t477 ** 2
      t481 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t472, t469,
     # t470, -t468, 0.0D0)
      t484 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t472, t469,
     # t470, -t468, 0.0D0)
      t493 = t4 * t481
      t499 = t13 * t405
      t505 = log(0.4D1 * t369 * t6 * t499 * t408 * x2 * t473)
      t516 = -(-0.90D2 * t5 * (t477 * t478 - t480 * t481 / 0.2D1 - t484)
     # + 0.180D3 * t28 * t4 * (-t478 + t477 * t481) - t39 * t493) * t43 
     #* t45 / 0.720D3 + (-0.90D2 * t5 * (t478 - t505 * t481) + 0.180D3 *
     # t28 * t493) * t43 * t346 / 0.720D3
      t517 = FJET(XB1, XB2, s, -t468, t469, t470, -t472, 0.0D0, t516)
      t519 = FJET(XB1, XB2, s, t286, 0.0D0, t283, 0.0D0, 0.0D0, t349)
      t522 = t86 * s * t471
      t524 = t2 * t398 * t369
      t525 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t522, -t524
     #, -t373, 0.0D0, t397)
      t527 = t405 * t408
      t529 = t527 * t369 * t87
      t532 = log(-0.4D1 * t93 * t404 * t529)
      t533 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t522, -t524
     #, -t373, 0.0D0, t397)
      t538 = t4 * t533
      t543 = (-0.90D2 * t5 * (t525 - t532 * t533) + 0.180D3 * t28 * t538
     #) * t43 * t346
      t544 = t105 * t13
      t547 = log(-0.4D1 * t544 * t529)
      t549 = t547 ** 2
      t552 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t522, -t524
     #, -t373, 0.0D0, t397)
      t553 = -t547 * t525 + t549 * t533 / 0.2D1 + t552
      t557 = t525 - t547 * t533
      t561 = t39 * t538
      t566 = t543 / 0.720D3 - (0.90D2 * t5 * t553 - 0.180D3 * t28 * t4 *
     # t557 - t561) * t100 * t45 / 0.720D3
      t567 = FJET(XB1, XB2, s, -t522, -t373, -t524, 0.0D0, t397, t566)
      t571 = t13 * t408 * t369
      t574 = log(-0.4D1 * t7 * t405 * t571)
      t575 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t357, 0.0D0,
     # -t373, 0.0D0, 0.0D0)
      t577 = t574 ** 2
      t578 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t357, 0.0D0,
     # -t373, 0.0D0, 0.0D0)
      t581 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t357, 0.0D0,
     # -t373, 0.0D0, 0.0D0)
      t590 = t4 * t578
      t591 = t39 * t590
      t598 = log(-0.4D1 * t6 * t405 * t571)
      t603 = t598 ** 2
      t606 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, t357, 0.0D0,
     # -t373, 0.0D0, 0.0D0)
      t627 = log(-0.4D1 * t85 * t499 * t409)
      t640 = log(-0.4D1 * t544 * t527 * t369)
      t642 = t640 ** 2
      t657 = -(-0.90D2 * t5 * (-t574 * t575 + t577 * t578 / 0.2D1 + t581
     #) + 0.180D3 * t28 * t4 * (t575 - t574 * t578) + t591) * t43 * t45 
     #/ 0.720D3 - (t39 * t4 * (t575 - t598 * t578) - 0.90D2 * t5 * (t603
     # * t575 / 0.2D1 + t606 - t603 * t598 * t578 / 0.6D1 - t598 * t581)
     # + t73 * t590 + 0.180D3 * t28 * t4 * (-t598 * t575 + t603 * t578 /
     # 0.2D1 + t581)) * t45 / 0.720D3 + (-0.90D2 * t5 * (-t575 + t627 * 
     #t578) - 0.180D3 * t28 * t590) * t43 * t346 / 0.720D3 - (-0.90D2 * 
     #t5 * (-t640 * t575 + t642 * t578 / 0.2D1 + t581) + 0.180D3 * t28 *
     # t4 * (t575 - t640 * t578) + t591) * t100 * t45 / 0.720D3
      t658 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t357, -t373, 0.0D0, t657)
      t660 = FJET(XB1, XB2, s, -t373, t357, 0.0D0, 0.0D0, 0.0D0, t657)
      t662 = FJET(XB1, XB2, s, -t472, t470, t469, -t468, 0.0D0, t516)
      t664 = FJET(XB1, XB2, s, t469, -t468, -t472, t470, 0.0D0, t516)
      t666 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t276)
      t679 = t543 / 0.720D3 - (0.90D2 * t5 * t553 - 0.180D3 * t28 * t4 *
     # t557 - t561) * t100 * t45 / 0.720D3
      t680 = FJET(XB1, XB2, s, 0.0D0, -t524, -t373, -t522, t397, t679)
      t682 = t2 * t284
      t683 = t2 * x3
      t687 = log(-0.4D1 * t7 * t14 * t284)
      t688 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t682, t683, 0.0D0)
      t690 = t687 ** 2
      t691 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t682, t683, 0.0D0)
      t694 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t682, t683, 0.0D0)
      t703 = t4 * t691
      t704 = t39 * t703
      t712 = log(-0.4D1 * t544 * t133 * t284)
      t726 = log(-0.4D1 * t133 * t13 * t284)
      t731 = t726 ** 2
      t734 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t682, t683, 0.0D0)
      t757 = log(-0.4D1 * t193 * t13 * x3 * t284)
      t759 = t757 ** 2
      t774 = -(-0.90D2 * t5 * (-t687 * t688 + t690 * t691 / 0.2D1 + t694
     #) + 0.180D3 * t28 * t4 * (t688 - t687 * t691) + t704) * t43 * t45 
     #/ 0.720D3 + (-0.90D2 * t5 * (-t688 + t712 * t691) - 0.180D3 * t28 
     #* t703) * t43 * t346 / 0.720D3 - (t39 * t4 * (t688 - t726 * t691) 
     #- 0.90D2 * t5 * (t731 * t688 / 0.2D1 + t734 - t731 * t726 * t691 /
     # 0.6D1 - t726 * t694) + t73 * t703 + 0.180D3 * t28 * t4 * (-t726 *
     # t688 + t731 * t691 / 0.2D1 + t694)) * t43 / 0.1440D4 - (-0.90D2 *
     # t5 * (-t757 * t688 + t759 * t691 / 0.2D1 + t694) + 0.180D3 * t28 
     #* t4 * (t688 - t757 * t691) + t704) * t43 * t100 / 0.1440D4
      t775 = FJET(XB1, XB2, s, -t682, 0.0D0, t683, 0.0D0, 0.0D0, t774)
      t777 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t373, t357, 0.0D0, t657)
      t779 = t277 * t276 + t350 * t349 + t463 * t459 * t102 / 0.720D3 + 
     #t517 * t516 + t519 * t349 + t567 * t566 + t658 * t657 + t660 * t65
     #7 + t662 * t516 + t664 * t516 + t666 * t276 + t680 * t679 + t775 *
     # t774 + t777 * t657
      t780 = FJET(XB1, XB2, s, t470, -t472, -t468, t469, 0.0D0, t516)
      t782 = FJET(XB1, XB2, s, t357, -t373, 0.0D0, 0.0D0, 0.0D0, t657)
      t784 = FJET(XB1, XB2, s, 0.0D0, -t682, 0.0D0, t683, 0.0D0, t774)
      t786 = FJET(XB1, XB2, s, -t391, -t374, t372, -t356, t397, t462)
      t790 = FJET(XB1, XB2, s, -t374, -t391, -t356, t372, t397, t462)
      t794 = FJET(XB1, XB2, s, t372, -t356, -t391, -t374, t397, t462)
      t798 = FJET(XB1, XB2, s, 0.0D0, t683, 0.0D0, -t682, 0.0D0, t774)
      t800 = FJET(XB1, XB2, s, -t524, 0.0D0, -t522, -t373, t397, t566)
      t802 = FJET(XB1, XB2, s, t683, 0.0D0, -t682, 0.0D0, 0.0D0, t774)
      t804 = FJET(XB1, XB2, s, 0.0D0, t286, 0.0D0, t283, 0.0D0, t349)
      t806 = FJET(XB1, XB2, s, -t373, -t522, 0.0D0, -t524, t397, t566)
      t808 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t276)
      t810 = FJET(XB1, XB2, s, t283, 0.0D0, t286, 0.0D0, 0.0D0, t349)
      t812 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t276)
      t814 = t780 * t516 + t782 * t657 + t784 * t774 + t786 * t459 * t10
     #2 / 0.720D3 + t790 * t459 * t102 / 0.720D3 + t794 * t459 * t102 / 
     #0.720D3 + t798 * t774 + t800 * t566 + t802 * t774 + t804 * t349 + 
     #t806 * t566 + t808 * t276 + t810 * t349 + t812 * t276
      rrgg2qqbarht7s4e1 = t779 + t814

      end function



      doubleprecision function rrgg2qqbarht7s4e0
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t3 = -0.1D1 + x2
      t5 = x2 * x3
      t6 = -0.1D1 + t5
      t7 = 0.1D1 / t6
      t8 = x3 * t3 * t7
      t9 = t2 * t8
      t10 = -0.1D1 + x3
      t12 = t2 * t10 * t7
      t13 = s ** 2
      t14 = 0.1D1 / t13
      t15 = pi * t14
      t16 = x4 * pi
      t17 = cos(t16)
      t18 = x3 * z
      t19 = x2 * t10
      t21 = Sqrt(-t18 * t19)
      t25 = 0.1D1 / (-z - t5 + 0.2D1 * t17 * t21)
      t26 = z * t25
      t27 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t12, t9, 0.0D0)
      t29 = z ** 2
      t31 = 0.1D1 / t29 / z
      t32 = t3 ** 2
      t34 = Sin(t16)
      t35 = t34 ** 2
      t36 = t31 * t32 * t35
      t37 = t6 ** 2
      t43 = log(-0.4D1 * t36 * t5 * t10 / t37)
      t45 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t12, t9, 0.0D0)
      t51 = pi * lh
      t57 = 0.1D1 / x3
      t59 = 0.1D1 / x2
      t64 = 0.1D1 / x1
      t69 = -(-0.90D2 * t15 * (t26 * t27 - t43 * z * t25 * t45) + 0.180D
     #3 * t51 * t14 * t26 * t45) * t57 * t59 / 0.1440D4 + t15 * t26 * t4
     #5 * t57 * t59 * t64 / 0.8D1
      t70 = FJET(XB1, XB2, s, t9, 0.0D0, t12, 0.0D0, 0.0D0, t69)
      t72 = t2 * t10
      t73 = t2 * x3
      t74 = x3 * t31
      t78 = log(-0.4D1 * t74 * t35 * t10)
      t79 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # -t72, t73, 0.0D0)
      t81 = t78 ** 2
      t82 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # -t72, t73, 0.0D0)
      t85 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # -t72, t73, 0.0D0)
      t94 = lh ** 2
      t96 = pi ** 2
      t98 = -0.180D3 * t94 + 0.30D2 * t96
      t99 = pi * t98
      t100 = t14 * t82
      t105 = x1 ** 2
      t106 = x3 * t105
      t107 = t35 * t31
      t111 = log(-0.4D1 * t106 * t107 * t10)
      t117 = 0.180D3 * t51 * t100
      t124 = t57 * t59 * t64
      t127 = x2 * t31
      t132 = log(-0.4D1 * t127 * t35 * x3 * t10)
      t141 = -(-0.90D2 * t15 * (-t78 * t79 + t81 * t82 / 0.2D1 + t85) + 
     #0.180D3 * t51 * t14 * (t79 - t78 * t82) + t99 * t100) * t57 / 0.14
     #40D4 - (-0.90D2 * t15 * (t79 - t111 * t82) + t117) * t57 * t64 / 0
     #.720D3 + t15 * t82 * t124 / 0.8D1 - (-0.90D2 * t15 * (t79 - t132 *
     # t82) + t117) * t57 * t59 / 0.1440D4
      t142 = FJET(XB1, XB2, s, -t72, 0.0D0, t73, 0.0D0, 0.0D0, t141)
      t144 = FJET(XB1, XB2, s, t73, 0.0D0, -t72, 0.0D0, 0.0D0, t141)
      t146 = FJET(XB1, XB2, s, 0.0D0, -t72, 0.0D0, t73, 0.0D0, t141)
      t148 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t151 = log(0.4D1 * t106 * t107)
      t152 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t157 = t14 * t152
      t159 = 0.180D3 * t51 * t157
      t164 = t105 * x2
      t167 = log(0.4D1 * t164 * t107)
      t171 = log(0.4D1 * t164 * t36)
      t181 = log(0.4D1 * t105 * t31 * t35)
      t183 = t181 ** 2
      t186 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t195 = t99 * t157
      t201 = log(0.4D1 * t74 * t35)
      t203 = t201 ** 2
      t218 = log(0.4D1 * t107)
      t219 = t218 * pi
      t222 = t218 ** 2
      t223 = t222 * pi
      t229 = rrgg2qqbarh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t257 = log(0.4D1 * t5 * t107)
      t261 = log(0.4D1 * t5 * t36)
      t270 = log(0.4D1 * t127 * t35)
      t271 = t270 ** 2
      t275 = log(0.4D1 * t127 * t35 * t32)
      t276 = t275 ** 2
      t290 = -(-0.90D2 * t15 * (-t148 + t151 * t152) - t159) * t57 * t64
     # / 0.720D3 + t15 * (t167 * t152 - t171 * t152) * t59 * t64 / 0.8D1
     # - (-0.90D2 * t15 * (t181 * t148 - t183 * t152 / 0.2D1 - t186) + 0
     #.180D3 * t51 * t14 * (-t148 + t181 * t152) - t195) * t64 / 0.720D3
     # - (-0.90D2 * t15 * (t201 * t148 - t203 * t152 / 0.2D1 - t186) + 0
     #.180D3 * t51 * t14 * (-t148 + t201 * t152) - t195) * t57 / 0.1440D
     #4 + (-0.180D3 * t219 * lh - 0.45D2 * t223 + t99) * t14 * t148 / 0.
     #1440D4 - t15 * t229 / 0.16D2 + (0.90D2 * t223 * lh + pi * (-0.60D2
     # * lh * t96 + 0.240D3 * zeta3 + 0.120D3 * t94 * lh) + 0.15D2 * t22
     #2 * t218 * pi - t219 * t98) * t14 * t152 / 0.1440D4 + (0.180D3 * t
     #51 + 0.90D2 * t219) * t14 * t186 / 0.1440D4 + t15 * (t257 * t152 -
     # t261 * t152) * t57 * t59 / 0.16D2 - (0.90D2 * t15 * t152 * (t271 
     #/ 0.2D1 - t276 / 0.2D1) + (0.90D2 * t15 * t148 - t159) * (-t270 + 
     #t275)) * t59 / 0.1440D4
      t291 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t290)
      t293 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t290)
      t295 = t2 * x1
      t296 = x3 * x1
      t297 = x1 * z
      t298 = t296 * z
      t299 = -z - x1 + t297
      t302 = Sqrt(x3 * t299 * t19)
      t303 = t17 * t302
      t304 = 0.2D1 * t303
      t307 = 0.1D1 / t299
      t310 = t295 * t3 * (-t5 - z + t18 - x1 + t296 + t297 - t298 + t304
     #) * t307 * t7
      t311 = t10 * s
      t312 = -0.1D1 + x1
      t313 = t1 * t312
      t315 = t311 * t313 * t7
      t320 = x2 ** 2
      t321 = x3 * t320
      t322 = t321 * t297
      t323 = t321 * x1
      t325 = t5 * z
      t327 = t5 * x1
      t329 = 0.2D1 * t303 * x2 - 0.2D1 * t5 * t297 + t322 + t5 - t323 - 
     #t321 * z + t298 + 0.2D1 * t325 + 0.2D1 * t327 - x2 - t304 - t18 - 
     #t296
      t332 = t295 * t329 * t307 * t7
      t333 = t2 * t312
      t334 = t333 * t8
      t335 = t1 ** 2
      t340 = s * t335 * x2 * x1 * t312 * t307
      t341 = x2 * x1
      t342 = t341 * z
      t343 = z + t342 - t297 + x1 - t341
      t346 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t310, -t332,
     # -t315, -t334, t340)
      t352 = t29 * x2
      t358 = t105 * t29
      t363 = t164 - t29 + 0.2D1 * t106 * x2 * z + x3 * t29 * t341 - t322
     # - t106 * t352 + 0.2D1 * x1 * t29 + 0.2D1 * t105 * z - t358 - t352
     # * x1 - 0.2D1 * t164 * z + t358 * x2
      t376 = t323 - t106 * x2 + t342 - t325 - t327 - 0.2D1 * t297 + 0.2D
     #1 * t303 * z + 0.2D1 * t303 * x1 - t105 - 0.2D1 * t303 * t341 - 0.
     #2D1 * t303 * t297 + 0.2D1 * t303 * t342
      t380 = t346 / (t363 + t376) * t124
      t382 = t15 * t299 * t343 * t380 / 0.8D1
      t383 = FJET(XB1, XB2, s, t310, -t315, -t332, -t334, t340, t382)
      t386 = t14 * t299 * t343
      t390 = t2 * t296
      t392 = x3 * s * t313
      t393 = t1 * x1
      t394 = t311 * t393
      t395 = t311 * t313
      t396 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t394, t390,
     # t395, -t392, 0.0D0)
      t398 = 0.1D1 / t29
      t401 = t312 ** 2
      t406 = log(0.4D1 * t105 * t35 * t398 * x3 * t10 * t307 * t401)
      t407 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t394, t390,
     # t395, -t392, 0.0D0)
      t422 = -(-0.90D2 * t15 * (-t396 + t406 * t407) - 0.180D3 * t51 * t
     #14 * t407) * t57 * t64 / 0.720D3 - t15 * t407 * t124 / 0.8D1
      t423 = FJET(XB1, XB2, s, t390, -t392, -t394, t395, 0.0D0, t422)
      t425 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t290)
      t427 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t295, 0.0D0,
     # -t333, 0.0D0, 0.0D0)
      t430 = t35 * t401 * t307
      t433 = log(-0.4D1 * t106 * t398 * t430)
      t434 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t295, 0.0D0,
     # -t333, 0.0D0, 0.0D0)
      t439 = t14 * t434
      t441 = 0.180D3 * t51 * t439
      t449 = t164 * t35
      t454 = log(-0.4D1 * t449 * t398 * t401 * t307)
      t466 = log(-0.4D1 * t105 * t398 * t430)
      t468 = t466 ** 2
      t471 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, t295, 0.0D0,
     # -t333, 0.0D0, 0.0D0)
      t484 = -(-0.90D2 * t15 * (t427 - t433 * t434) + t441) * t57 * t64 
     #/ 0.720D3 + t15 * t434 * t124 / 0.8D1 - (-0.90D2 * t15 * (t427 - t
     #454 * t434) + t441) * t59 * t64 / 0.720D3 - (-0.90D2 * t15 * (-t46
     #6 * t427 + t468 * t434 / 0.2D1 + t471) + 0.180D3 * t51 * t14 * (t4
     #27 - t466 * t434) + t99 * t439) * t64 / 0.720D3
      t485 = FJET(XB1, XB2, s, t295, -t333, 0.0D0, 0.0D0, 0.0D0, t484)
      t487 = FJET(XB1, XB2, s, 0.0D0, t73, 0.0D0, -t72, 0.0D0, t141)
      t489 = FJET(XB1, XB2, s, 0.0D0, t9, 0.0D0, t12, 0.0D0, t69)
      t492 = t3 * s * t393
      t494 = t2 * t341 * t307
      t495 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t492, -t494
     #, -t333, 0.0D0, t340)
      t498 = t15 * t495 * t124 / 0.8D1
      t499 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t492, -t494
     #, -t333, 0.0D0, t340)
      t505 = log(-0.4D1 * t449 * t398 * t307 * t401 * t32)
      t507 = t499 - t505 * t495
      t512 = 0.180D3 * t51 * t14 * t495
      t517 = -t498 - (0.90D2 * t15 * t507 - t512) * t59 * t64 / 0.720D3
      t518 = FJET(XB1, XB2, s, -t492, -t333, -t494, 0.0D0, t340, t517)
      t520 = FJET(XB1, XB2, s, -t394, t395, t390, -t392, 0.0D0, t422)
      t522 = t70 * t69 + t142 * t141 + t144 * t141 + t146 * t141 + t291 
     #* t290 + t293 * t290 + t383 * pi * t386 * t380 / 0.8D1 + t423 * t4
     #22 + t425 * t290 + t485 * t484 + t487 * t141 + t489 * t69 + t518 *
     # t517 + t520 * t422
      t523 = FJET(XB1, XB2, s, -t332, -t334, t310, -t315, t340, t382)
      t528 = FJET(XB1, XB2, s, t395, -t394, -t392, t390, 0.0D0, t422)
      t530 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t290)
      t532 = FJET(XB1, XB2, s, -t315, t310, -t334, -t332, t340, t382)
      t544 = -t498 - (0.90D2 * t15 * t507 - t512) * t59 * t64 / 0.720D3
      t545 = FJET(XB1, XB2, s, 0.0D0, -t494, -t333, -t492, t340, t544)
      t547 = FJET(XB1, XB2, s, -t333, t295, 0.0D0, 0.0D0, 0.0D0, t484)
      t549 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t333, t295, 0.0D0, t484)
      t551 = FJET(XB1, XB2, s, -t333, -t492, 0.0D0, -t494, t340, t517)
      t553 = FJET(XB1, XB2, s, t12, 0.0D0, t9, 0.0D0, 0.0D0, t69)
      t555 = FJET(XB1, XB2, s, -t334, -t332, -t315, t310, t340, t382)
      t560 = FJET(XB1, XB2, s, -t392, t390, t395, -t394, 0.0D0, t422)
      t562 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t295, -t333, 0.0D0, t484)
      t564 = FJET(XB1, XB2, s, -t494, 0.0D0, -t492, -t333, t340, t517)
      t566 = FJET(XB1, XB2, s, 0.0D0, t12, 0.0D0, t9, 0.0D0, t69)
      t568 = t523 * pi * t386 * t380 / 0.8D1 + t528 * t422 + t530 * t290
     # + t532 * pi * t386 * t380 / 0.8D1 + t545 * t544 + t547 * t484 + t
     #549 * t484 + t551 * t517 + t553 * t69 + t555 * pi * t386 * t380 / 
     #0.8D1 + t560 * t422 + t562 * t484 + t564 * t517 + t566 * t69
      rrgg2qqbarht7s4e0 = t522 + t568

      end function



      doubleprecision function rrgg2qqbarht7s4em1
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t34 = log(0.4D1 * t30 * t9 * t13)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t50 = log(0.4D1 * t47 * t13)
      t51 = -0.1D1 + x2
      t52 = t51 ** 2
      t56 = log(0.4D1 * t47 * t13 * t52)
      t59 = 0.1D1 / x2
      t66 = log(0.4D1 * t9 * t13)
      t67 = t66 * pi
      t75 = t66 ** 2
      t78 = lh ** 2
      t80 = pi ** 2
      t88 = rrgg2qqbarh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t91 = -(-0.90D2 * t5 * (-t6 + t16 * t17) - t25) * t27 / 0.1440D4 -
     # (-0.90D2 * t5 * (-t6 + t34 * t17) - t25) * t40 / 0.720D3 - t5 * t
     #17 * t27 * t40 / 0.8D1 - t5 * t17 * (-t50 + t56) * t59 / 0.16D2 + 
     #(0.180D3 * t22 + 0.90D2 * t67) * t4 * t6 / 0.1440D4 + (-0.180D3 * 
     #t67 * lh - 0.45D2 * t75 * pi + pi * (-0.180D3 * t78 + 0.30D2 * t80
     #)) * t4 * t17 / 0.1440D4 - t5 * t88 / 0.16D2
      t92 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t91)
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t91)
      t96 = t2 * x1
      t97 = -0.1D1 + x1
      t98 = t2 * t97
      t99 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t96, 0.0D0, -
     #t98, 0.0D0, 0.0D0)
      t104 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, t96, 0.0D0, 
     #-t98, 0.0D0, 0.0D0)
      t109 = 0.1D1 / (-z - x1 + x1 * z)
      t111 = t97 ** 2
      t115 = log(-0.4D1 * t30 / t7 * t13 * t109 * t111)
      t130 = t5 * t99 * t59 * t40 / 0.8D1 - (-0.90D2 * t5 * (t104 - t115
     # * t99) + 0.180D3 * t22 * t4 * t99) * t40 / 0.720D3 + t5 * t99 * t
     #27 * t40 / 0.8D1
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t96, -t98, 0.0D0, t130)
      t133 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t98, t96, 0.0D0, t130)
      t135 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t91)
      t137 = t2 * x3
      t138 = -0.1D1 + x3
      t139 = t2 * t138
      t140 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t139, t137, 0.0D0)
      t144 = log(-0.4D1 * t10 * t13 * t138)
      t145 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t139, t137, 0.0D0)
      t156 = t145 * t27
      t163 = -(-0.90D2 * t5 * (t140 - t144 * t145) + 0.180D3 * t22 * t4 
     #* t145) * t27 / 0.1440D4 + t5 * t156 * t40 / 0.8D1 + t5 * t156 * t
     #59 / 0.16D2
      t164 = FJET(XB1, XB2, s, 0.0D0, t137, 0.0D0, -t139, 0.0D0, t163)
      t166 = x2 * x3
      t168 = 0.1D1 / (-0.1D1 + t166)
      t170 = t2 * t138 * t168
      t173 = t2 * x3 * t51 * t168
      t175 = cos(t11)
      t179 = Sqrt(-x3 * z * x2 * t138)
      t184 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t170, t173, 0.0D0)
      t187 = 0.1D1 / (-z - t166 + 0.2D1 * t175 * t179) * t184 * t27 * t5
     #9
      t189 = t5 * z * t187 / 0.16D2
      t190 = FJET(XB1, XB2, s, 0.0D0, t170, 0.0D0, t173, 0.0D0, t189)
      t192 = t4 * z
      t196 = FJET(XB1, XB2, s, 0.0D0, t173, 0.0D0, t170, 0.0D0, t189)
      t201 = FJET(XB1, XB2, s, 0.0D0, -t139, 0.0D0, t137, 0.0D0, t163)
      t205 = t2 * x1 * x2 * t109
      t207 = t1 * x1
      t208 = t51 * s * t207
      t209 = t1 ** 2
      t214 = s * t209 * x2 * x1 * t97 * t109
      t215 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t208, -t205
     #, -t98, 0.0D0, t214)
      t217 = t215 * t59 * t40
      t219 = t5 * t217 / 0.8D1
      t220 = FJET(XB1, XB2, s, 0.0D0, -t205, -t98, -t208, t214, -t219)
      t225 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t91)
      t227 = FJET(XB1, XB2, s, t96, -t98, 0.0D0, 0.0D0, 0.0D0, t130)
      t229 = t92 * t91 + t94 * t91 + t131 * t130 + t133 * t130 + t135 * 
     #t91 + t164 * t163 + t190 * pi * t192 * t187 / 0.16D2 + t196 * pi *
     # t192 * t187 / 0.16D2 + t201 * t163 - t220 * pi * t4 * t217 / 0.8D
     #1 + t225 * t91 + t227 * t130
      t230 = FJET(XB1, XB2, s, t137, 0.0D0, -t139, 0.0D0, 0.0D0, t163)
      t233 = t2 * x1 * x3
      t235 = t1 * t97
      t236 = x3 * s * t235
      t237 = t138 * s
      t238 = t237 * t207
      t239 = t237 * t235
      t240 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t238, t233,
     # t239, -t236, 0.0D0)
      t242 = t240 * t27 * t40
      t244 = t5 * t242 / 0.8D1
      t245 = FJET(XB1, XB2, s, t233, -t236, -t238, t239, 0.0D0, -t244)
      t250 = FJET(XB1, XB2, s, t239, -t238, -t236, t233, 0.0D0, -t244)
      t255 = FJET(XB1, XB2, s, t170, 0.0D0, t173, 0.0D0, 0.0D0, t189)
      t260 = FJET(XB1, XB2, s, t173, 0.0D0, t170, 0.0D0, 0.0D0, t189)
      t265 = FJET(XB1, XB2, s, -t98, t96, 0.0D0, 0.0D0, 0.0D0, t130)
      t267 = FJET(XB1, XB2, s, -t98, -t208, 0.0D0, -t205, t214, -t219)
      t272 = FJET(XB1, XB2, s, -t139, 0.0D0, t137, 0.0D0, 0.0D0, t163)
      t274 = FJET(XB1, XB2, s, -t236, t233, t239, -t238, 0.0D0, -t244)
      t279 = FJET(XB1, XB2, s, -t208, -t98, -t205, 0.0D0, t214, -t219)
      t284 = FJET(XB1, XB2, s, -t238, t239, t233, -t236, 0.0D0, -t244)
      t289 = FJET(XB1, XB2, s, -t205, 0.0D0, -t208, -t98, t214, -t219)
      t294 = t230 * t163 - t245 * pi * t4 * t242 / 0.8D1 - t250 * pi * t
     #4 * t242 / 0.8D1 + t255 * pi * t192 * t187 / 0.16D2 + t260 * pi * 
     #t192 * t187 / 0.16D2 + t265 * t130 - t267 * pi * t4 * t217 / 0.8D1
     # + t272 * t163 - t274 * pi * t4 * t242 / 0.8D1 - t279 * pi * t4 * 
     #t217 / 0.8D1 - t284 * pi * t4 * t242 / 0.8D1 - t289 * pi * t4 * t2
     #17 / 0.8D1
      rrgg2qqbarht7s4em1 = t229 + t294

      end function



      doubleprecision function rrgg2qqbarht7s4em2
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t15 = rrgg2qqbarh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t20 = z ** 2
      t24 = Sin(x4 * pi)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t35 = -t5 * t6 * t7 / 0.8D1 - t5 * t6 * t11 / 0.16D2 - t5 * t15 / 
     #0.16D2 + (0.180D3 * pi * lh + 0.90D2 * t28 * pi) * t4 * t6 / 0.144
     #0D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t40 = t2 * x1
      t42 = t2 * (-0.1D1 + x1)
      t43 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, t40, 0.0D0, -
     #t42, 0.0D0, 0.0D0)
      t46 = t5 * t43 * t7 / 0.8D1
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t40, -t42, 0.0D0, t46)
      t50 = t4 * t43 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t40, 0.0D0, t46)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t59 = t2 * x3
      t61 = t2 * (-0.1D1 + x3)
      t62 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # -t61, t59, 0.0D0)
      t65 = t5 * t62 * t11 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t61, 0.0D0, t65)
      t69 = t4 * t62 * t11
      t72 = FJET(XB1, XB2, s, 0.0D0, -t61, 0.0D0, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t78 = FJET(XB1, XB2, s, t40, -t42, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t61, 0.0D0, 0.0D0, t65)
      t86 = FJET(XB1, XB2, s, -t42, t40, 0.0D0, 0.0D0, 0.0D0, t46)
      t90 = FJET(XB1, XB2, s, -t61, 0.0D0, t59, 0.0D0, 0.0D0, t65)
      rrgg2qqbarht7s4em2 = t36 * t35 + t38 * t35 + t47 * pi * t50 / 0.8D
     #1 + t53 * pi * t50 / 0.8D1 + t57 * t35 + t66 * pi * t69 / 0.16D2 +
     # t72 * pi * t69 / 0.16D2 + t76 * t35 + t78 * pi * t50 / 0.8D1 + t8
     #2 * pi * t69 / 0.16D2 + t86 * pi * t50 / 0.8D1 + t90 * pi * t69 / 
     #0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s4em3
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

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
      t6 = rrgg2qqbarh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht7s4em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.
     #16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht7s4em4
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
      doubleprecision rrgg2qqbarh71J1
      doubleprecision rrgg2qqbarh71J2
      doubleprecision rrgg2qqbarh71J3
      doubleprecision rrgg2qqbarh71J4
      doubleprecision rrgg2qqbarh71J5
      doubleprecision rrgg2qqbarh71J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht7s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh71J1
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
      t9 = S13 ** 2
      t17 = t5 * S24
      t18 = S14 ** 2
      t21 = S23 ** 2
      t26 = t9 * S13
      t40 = S12 ** 2
      rrgg2qqbarh71J1 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24) * 
     #nf * S12 + 0.3D1 / 0.8D1 * (0.2D1 * t5 - 0.6D1 * S13 * S14 + 0.2D1
     # * t9 - 0.6D1 * S23 * S24) * nf + 0.3D1 / 0.8D1 * (0.4D1 * S23 * t
     #5 - t17 - 0.7D1 * S13 * t18 - 0.7D1 * t21 * S24 + 0.4D1 * t9 * S14
     # - t26) * nf / S12 + 0.3D1 / 0.8D1 * (0.4D1 * S23 * t17 + 0.4D1 * 
     #t18 * S14 * S13 + 0.4D1 * S14 * t26 + 0.4D1 * t21 * S23 * S24) * n
     #f / t40) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J2
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
      t9 = S23 * S24
      t11 = S13 ** 2
      t13 = S13 * S14
      t15 = S24 ** 2
      t19 = t15 * S24
      t21 = t11 * S14
      t23 = S23 ** 2
      t26 = S23 * t15
      t28 = S14 ** 2
      t31 = t11 * S13
      t33 = t23 * S24
      t35 = S13 * t28
      t67 = 0.4D1 * S14 * t26 + 0.4D1 * t9 * t28 + 0.4D1 * t33 * S14 + 0
     #.4D1 * t21 * S23 + 0.4D1 * t13 * t23 + 0.4D1 * t35 * S23 + 0.4D1 *
     # S23 * t19 - 0.8D1 * t23 * t15 - 0.8D1 * t28 * t11 + 0.4D1 * S14 *
     # t31 + 0.4D1 * t23 * S23 * S24 + 0.4D1 * t28 * S14 * S13
      t69 = S12 ** 2
      rrgg2qqbarh71J2 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24) * 
     #nf * S12 + 0.3D1 / 0.8D1 * (-0.2D1 * S24 * S14 - 0.2D1 * S23 * S13
     # - 0.12D2 * t9 + 0.4D1 * t11 - 0.12D2 * t13 + 0.4D1 * t15) * nf + 
     #0.3D1 / 0.8D1 * (-0.2D1 * t19 + 0.8D1 * t21 - 0.2D1 * S13 * t23 + 
     #0.8D1 * t26 - 0.2D1 * S24 * t28 - 0.2D1 * t31 - 0.14D2 * t33 - 0.1
     #4D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J3
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
      t9 = S23 * S24
      t11 = S13 ** 2
      t13 = S13 * S14
      t15 = S24 ** 2
      t19 = t15 * S24
      t21 = t11 * S14
      t23 = S23 ** 2
      t26 = S23 * t15
      t28 = S14 ** 2
      t31 = t11 * S13
      t33 = t23 * S24
      t35 = S13 * t28
      t67 = 0.8D1 * S14 * t26 + 0.8D1 * t9 * t28 + 0.8D1 * t33 * S14 + 0
     #.8D1 * t21 * S23 + 0.8D1 * t13 * t23 + 0.8D1 * t35 * S23 + 0.4D1 *
     # S23 * t19 - 0.16D2 * t23 * t15 - 0.16D2 * t28 * t11 + 0.4D1 * S14
     # * t31 + 0.4D1 * t23 * S23 * S24 + 0.4D1 * t28 * S14 * S13
      t69 = S12 ** 2
      rrgg2qqbarh71J3 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24) * 
     #nf * S12 + 0.3D1 / 0.8D1 * (-0.4D1 * S24 * S14 - 0.4D1 * S23 * S13
     # - 0.18D2 * t9 + 0.6D1 * t11 - 0.18D2 * t13 + 0.6D1 * t15) * nf + 
     #0.3D1 / 0.8D1 * (-0.3D1 * t19 + 0.12D2 * t21 - 0.4D1 * S13 * t23 +
     # 0.12D2 * t26 - 0.4D1 * S24 * t28 - 0.3D1 * t31 - 0.21D2 * t33 - 0
     #.21D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi * w
     #d / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J4
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
      t9 = S23 * S24
      t11 = S13 ** 2
      t13 = S13 * S14
      t15 = S24 ** 2
      t19 = t15 * S24
      t21 = t11 * S14
      t23 = S23 ** 2
      t26 = S23 * t15
      t28 = S14 ** 2
      t31 = t11 * S13
      t33 = t23 * S24
      t35 = S13 * t28
      t67 = 0.12D2 * S14 * t26 + 0.12D2 * t9 * t28 + 0.12D2 * t33 * S14 
     #+ 0.12D2 * t21 * S23 + 0.12D2 * t13 * t23 + 0.12D2 * t35 * S23 + 0
     #.4D1 * S23 * t19 - 0.24D2 * t23 * t15 - 0.24D2 * t28 * t11 + 0.4D1
     # * S14 * t31 + 0.4D1 * t23 * S23 * S24 + 0.4D1 * t28 * S14 * S13
      t69 = S12 ** 2
      rrgg2qqbarh71J4 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24) * 
     #nf * S12 + 0.3D1 / 0.8D1 * (-0.6D1 * S24 * S14 - 0.6D1 * S23 * S13
     # - 0.24D2 * t9 + 0.8D1 * t11 - 0.24D2 * t13 + 0.8D1 * t15) * nf + 
     #0.3D1 / 0.8D1 * (-0.4D1 * t19 + 0.16D2 * t21 - 0.6D1 * S13 * t23 +
     # 0.16D2 * t26 - 0.6D1 * S24 * t28 - 0.4D1 * t31 - 0.28D2 * t33 - 0
     #.28D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi * w
     #d / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J5
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
      t9 = S23 * S24
      t11 = S13 ** 2
      t13 = S13 * S14
      t15 = S24 ** 2
      t19 = t15 * S24
      t21 = t11 * S14
      t23 = S23 ** 2
      t26 = S23 * t15
      t28 = S14 ** 2
      t31 = t11 * S13
      t33 = t23 * S24
      t35 = S13 * t28
      t67 = 0.16D2 * S14 * t26 + 0.16D2 * t9 * t28 + 0.16D2 * t33 * S14 
     #+ 0.16D2 * t21 * S23 + 0.16D2 * t13 * t23 + 0.16D2 * t35 * S23 + 0
     #.4D1 * S23 * t19 - 0.32D2 * t23 * t15 - 0.32D2 * t28 * t11 + 0.4D1
     # * S14 * t31 + 0.4D1 * t23 * S23 * S24 + 0.4D1 * t28 * S14 * S13
      t69 = S12 ** 2
      rrgg2qqbarh71J5 = (0.3D1 / 0.8D1 * (-0.2D1 * S13 - 0.2D1 * S24) * 
     #nf * S12 + 0.3D1 / 0.8D1 * (-0.8D1 * S24 * S14 - 0.8D1 * S23 * S13
     # - 0.30D2 * t9 + 0.10D2 * t11 - 0.30D2 * t13 + 0.10D2 * t15) * nf 
     #+ 0.3D1 / 0.8D1 * (-0.5D1 * t19 + 0.20D2 * t21 - 0.8D1 * S13 * t23
     # + 0.20D2 * t26 - 0.8D1 * S24 * t28 - 0.5D1 * t31 - 0.35D2 * t33 -
     # 0.35D2 * t35) * nf / S12 + 0.3D1 / 0.8D1 * t67 * nf / t69) / pi *
     # wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh71J6
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
      t10 = S14 ** 2
      t12 = S23 ** 2
      t19 = S24 ** 2
      t37 = S13 ** 2
      t55 = 0.4D1 * S23 * t19 * S14 + 0.4D1 * S23 * S24 * t10 + 0.4D1 * 
     #S14 * S13 * t12 - 0.8D1 * t12 * t19 + 0.4D1 * t12 * S24 * S14 - 0.
     #4D1 * S23 * t19 * S24 + 0.4D1 * S14 * t37 * S23 - 0.4D1 * t10 * S1
     #4 * S13 - 0.4D1 * S14 * t37 * S13 - 0.4D1 * t12 * S23 * S24 + 0.4D
     #1 * t10 * S13 * S23 - 0.8D1 * t10 * t37
      t57 = S12 ** 2
      rrgg2qqbarh71J6 = (0.15D2 / 0.8D1 * (0.2D1 * S13 + 0.2D1 * S24) * 
     #nf * S12 + 0.15D2 / 0.8D1 * (-0.2D1 * S24 * S14 - 0.2D1 * S23 * S1
     #3) * nf + 0.15D2 / 0.8D1 * (-0.2D1 * S24 * t10 - 0.2D1 * S13 * t12
     #) * nf / S12 + 0.15D2 / 0.8D1 * t55 * nf / t57) / pi * wd / z

      end function
  
 