      subroutine rrgg2gght14
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt14
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt14
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhardt14s1e1  
      doubleprecision rrgg2gghhardt14s1e0  
      doubleprecision rrgg2gghhardt14s1em1  
      doubleprecision rrgg2gghhardt14s1em2  
      doubleprecision rrgg2gghhardt14s1em3  
      doubleprecision rrgg2gghhardt14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt14s1e1
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = t17 ** 2
      t20 = x2 * z
      t22 = (0.1D1 - x2 + t20) ** 2
      t23 = t22 ** 2
      t24 = 0.1D1 / t23
      t25 = cos(t8)
      t26 = t25 ** 2
      t27 = t24 * t26
      t28 = t4 * x2
      t29 = Sqrt(-t28)
      t30 = t29 ** 2
      t31 = t1 ** 2
      t32 = t31 ** 2
      t34 = t27 * t30 * t32
      t38 = lh * z
      t41 = (0.180D3 * z - 0.180D3 * t38) * wd
      t42 = -0.180D3 * t6 + t41
      t46 = t26 * t30 * t32
      t53 = pi ** 2
      t55 = lh ** 2
      t57 = -0.30D2 * t53 + 0.180D3 * t55
      t61 = 0.90D2 * t6 - 0.2D1 * t41 + (0.270D3 * z - 0.360D3 * t38 + t
     #57 * z) * wd
      t66 = 0.1D1 / x3
      t70 = t27 * t30 * z
      t78 = log(-0.4D1 * x2 * t10 * t12 * t4)
      t79 = t78 * t24
      t84 = (-0.180D3 * t27 * lh - 0.90D2 * t79 * t26) * t30 * z
      t91 = t26 * lh
      t94 = t78 ** 2
      t95 = t94 * t24
      t101 = (0.180D3 * t79 * t91 + 0.45D2 * t95 * t26 + t27 * t57) * t3
     #0 * z
      t122 = t26 * t57
      t131 = x1 ** 2
      t132 = x3 * t131
      t133 = t132 * x2
      t136 = log(-0.4D1 * t133 * t14)
      t145 = 0.1D1 / x1
      t148 = t26 * z
      t150 = x2 * t131
      t153 = log(-0.4D1 * t150 * t14)
      t154 = t153 ** 2
      t156 = t24 * t30
      t161 = t148 * wd * t32
      t163 = t91 * z
      t167 = (0.180D3 * t148 - 0.180D3 * t163) * wd * t32
      t168 = -0.180D3 * t161 + t167
      t180 = 0.90D2 * t161 - 0.2D1 * t167 + (0.270D3 * t148 - 0.360D3 * 
     #t163 + t122 * z) * wd * t32
      t187 = -(-0.720D3 * t6 * t18 * t34 + 0.16D2 * t42 * t17 * t24 * t4
     #6 - 0.16D2 * t61 * t24 * t46) * t66 / 0.320D3 + (0.180D3 * t70 + t
     #84) * wd * t32 / 0.20D2 - (0.270D3 * t70 + 0.2D1 * t84 + t101) * w
     #d * t32 / 0.10D2 + (0.360D3 * t70 + 0.3D1 * t84 + 0.2D1 * t101 + (
     #-0.90D2 * t95 * t91 + t27 * (-0.240D3 * zeta3 - 0.120D3 * t55 * lh
     # + 0.60D2 * lh * t53) - 0.15D2 * t94 * t78 * t24 * t26 - t79 * t12
     #2) * t30 * z) * wd * t32 / 0.20D2 - (0.1440D4 * t6 * t136 * t34 - 
     #0.16D2 * t42 * t24 * t46) * t66 * t145 / 0.160D3 - (-0.720D3 * t14
     #8 * wd * t32 * t154 * t156 + 0.16D2 * t168 * t153 * t156 - 0.16D2 
     #* t180 * t24 * t30) * t145 / 0.160D3
      t188 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t187)
      t190 = 0.2D1 * t7
      t191 = sqrt(x3)
      t192 = t25 * t191
      t193 = -0.1D1 + t191
      t194 = t191 + 0.1D1
      t195 = t193 * t194
      t197 = Sqrt(t28 * t195)
      t199 = 0.2D1 * t192 * t197
      t201 = t2 * (0.1D1 - x2 - x3 + t190 + t199)
      t203 = t2 * (-x3 + t190 - x2 + t199)
      t204 = x2 * t12
      t206 = t10 * x3
      t210 = log(0.4D1 * t204 * t4 * t195 * t206)
      t211 = t210 ** 2
      t213 = 0.1D1 / t22
      t215 = 0.3D1 * t191
      t216 = t191 * x2
      t218 = 0.5D1 * t216 * z
      t219 = z * t25
      t222 = t25 * x3
      t225 = t191 * x3
      t226 = t225 * x2
      t228 = 0.2D1 * t226 * z
      t229 = 0.2D1 * t226
      t230 = 0.5D1 * t216
      t233 = t225 * z
      t235 = 0.2D1 * t191 * z
      t239 = -t215 - t218 - 0.4D1 * t219 * t197 - 0.2D1 * t222 * t197 + 
     #t228 - t229 + t230 + 0.4D1 * t25 * t197 + t225 - t233 + t235 + 0.2
     #D1 * t222 * t197 * z
      t240 = t239 ** 2
      t242 = 0.2D1 * t7 * z
      t243 = x3 * z
      t248 = (t242 - t199 - t243 - t190 - 0.1D1 - t20 + x3 + 0.2D1 * t21
     #9 * t191 * t197 + x2) ** 2
      t249 = 0.1D1 / t248
      t251 = t31 * t213 * t240 * t249
      t257 = t213 * t240 * t249
      t270 = log(0.4D1 * t204 * t4 * t193 * t194 * t10 * t132)
      t280 = -(0.45D2 * t6 * t211 * t251 - t42 * t210 * t31 * t257 + t61
     # * t31 * t257) * t66 / 0.320D3 - (-0.90D2 * t6 * t270 * t251 + t42
     # * t31 * t257) * t66 * t145 / 0.160D3
      t281 = FJET(XB1, XB2, s, 0.0D0, t201, 0.0D0, -t203, 0.0D0, t280)
      t283 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t187)
      t285 = FJET(XB1, XB2, s, 0.0D0, -t203, 0.0D0, t201, 0.0D0, t280)
      t287 = -0.1D1 + x1
      t289 = x1 * z
      t290 = 0.1D1 - x1 + t289
      t291 = 0.1D1 / t290
      t293 = t2 * t287 * x2 * t291
      t294 = t2 * x1
      t297 = t4 * s * t1 * t287
      t302 = s * t31 * x2 * t287 * x1 * t291
      t306 = t287 ** 2
      t308 = t12 * t291 * t306 * t4
      t311 = log(-0.4D1 * t7 * t131 * t10 * t308)
      t314 = t4 * t290
      t316 = Sqrt(-t314 * x2)
      t317 = t316 ** 2
      t318 = x1 * x2
      t319 = t318 * z
      t321 = (-0.1D1 + x1 - t289 + x2 - t318 - t20 + t319) ** 2
      t322 = t321 ** 2
      t323 = 0.1D1 / t322
      t325 = t306 * t287
      t333 = t323 * t325
      t344 = log(-0.4D1 * t150 * t10 * t308)
      t345 = t344 ** 2
      t347 = t333 * t290
      t361 = -(0.1440D4 * t6 * t311 * t26 * t317 * t323 * t325 * t290 * 
     #t32 - 0.16D2 * t42 * t26 * t317 * t333 * t290 * t32) * t66 * t145 
     #/ 0.160D3 - (-0.720D3 * t161 * t345 * t317 * t347 + 0.16D2 * t168 
     #* t344 * t317 * t347 - 0.16D2 * t180 * t317 * t347) * t145 / 0.160
     #D3
      t362 = FJET(XB1, XB2, s, 0.0D0, -t293, t294, t297, -t302, t361)
      t364 = FJET(XB1, XB2, s, t294, t297, 0.0D0, -t293, -t302, t361)
      t366 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t187)
      t368 = FJET(XB1, XB2, s, t201, 0.0D0, -t203, 0.0D0, 0.0D0, t280)
      t370 = x3 * x1
      t371 = t2 * t370
      t372 = t370 * z
      t373 = t370 * x2
      t374 = t370 * t20
      t378 = Sqrt(t314 * x2 * t193 * t194)
      t380 = 0.2D1 * t192 * t378
      t384 = t2 * t287 * (-x3 + t370 - t372 + t190 - t373 + t374 - x2 + 
     #t380) * t291
      t387 = t2 * x1 * t193 * t194
      t388 = 0.1D1 - x1 + t289 - x2 + t318 - t319 - x3 + t370 - t372 + t
     #190 - t373 + t374 + t380
      t391 = t2 * t287 * t388 * t291
      t400 = log(0.4D1 * x2 * t291 * t306 * t12 * t4 * t195 * t206 * t13
     #1)
      t401 = 0.1D1 / t321
      t404 = t378 * x1
      t409 = 0.1D1 + t380 - 0.2D1 * t192 * t404 - x1 - x2 - x3 - t319 - 
     #0.2D1 * t372 - t242 - 0.3D1 * t373 + t289 + t190
      t410 = t191 * t378
      t417 = x2 * t11
      t421 = t131 * t11
      t424 = -0.2D1 * t219 * t410 + 0.2D1 * t219 * t410 * x1 + 0.4D1 * t
     #374 - t370 * t417 - 0.2D1 * t132 * t20 + t421 * t7 + t370 * t11 + 
     #t133 + t20 + t318 + t243 + t370
      t426 = (t409 + t424) ** 2
      t427 = 0.1D1 / t426
      t428 = t25 * t378
      t430 = x1 * t225
      t431 = t191 * x1
      t433 = t191 * t131
      t434 = t378 * z
      t448 = -0.4D1 * t428 + t430 - 0.4D1 * t431 + t433 + t218 - t228 - 
     #t225 + t229 - t230 + t233 - t235 + 0.2D1 * t222 * t434 * x1 + t215
     # + 0.4D1 * t226 * t289 - 0.11D2 * t431 * t20 + 0.3D1 * t431 * t417
     # + 0.6D1 * t433 * t20 - 0.3D1 * t421 * t216
      t449 = t225 * t131
      t484 = t449 * t417 - 0.2D1 * t449 * t20 - t430 * t417 - 0.4D1 * t2
     #19 * t404 - 0.2D1 * t222 * t404 - 0.2D1 * t222 * t434 - 0.3D1 * t4
     #30 * x2 + 0.6D1 * t431 * z + 0.8D1 * t431 * x2 - 0.2D1 * t430 * z 
     #+ t430 * t11 + t449 * x2 - 0.2D1 * t433 * z - 0.3D1 * t433 * x2 + 
     #t421 * t191 - 0.2D1 * t191 * t11 * x1 + 0.4D1 * t219 * t378 + 0.2D
     #1 * t222 * t378 + 0.4D1 * t428 * x1
      t486 = (t448 + t484) ** 2
      t499 = -0.90D2 * t6 * t400 * t401 * t427 * t486 * t31 * t290 * t28
     #7 + t42 * t401 * t427 * t486 * t31 * t290 * t287
      t502 = t499 * t66 * t145 / 0.160D3
      t503 = FJET(XB1, XB2, s, t371, t384, -t387, -t391, -t302, -t502)
      t505 = t66 * t145
      t508 = FJET(XB1, XB2, s, t297, t294, -t293, 0.0D0, -t302, t361)
      t510 = FJET(XB1, XB2, s, t384, t371, -t391, -t387, -t302, -t502)
      t514 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t187)
      t516 = FJET(XB1, XB2, s, -t203, 0.0D0, t201, 0.0D0, 0.0D0, t280)
      t518 = FJET(XB1, XB2, s, -t387, -t391, t371, t384, -t302, -t502)
      t522 = FJET(XB1, XB2, s, -t293, 0.0D0, t297, t294, -t302, t361)
      t524 = FJET(XB1, XB2, s, -t391, -t387, t384, t371, -t302, -t502)
      rrgg2gghhardt14s1e1 = t188 * t187 + t281 * t280 + t283 * t187 + t2
     #85 * t280 + t362 * t361 + t364 * t361 + t366 * t187 + t368 * t280 
     #- t503 * t499 * t505 / 0.160D3 + t508 * t361 - t510 * t499 * t505 
     #/ 0.160D3 + t514 * t187 + t516 * t280 - t518 * t499 * t505 / 0.160
     #D3 + t522 * t361 - t524 * t499 * t505 / 0.160D3

      end function



      doubleprecision function rrgg2gghhardt14s1e0
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t19 = x2 * z
      t21 = (0.1D1 - x2 + t19) ** 2
      t22 = t21 ** 2
      t23 = 0.1D1 / t22
      t24 = cos(t8)
      t25 = t24 ** 2
      t26 = t23 * t25
      t27 = t4 * x2
      t28 = Sqrt(-t27)
      t29 = t28 ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t41 = -0.180D3 * t6 + (0.180D3 * z - 0.180D3 * lh * z) * wd
      t44 = t25 * t29 * t31
      t48 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t53 = t48 * t52
      t57 = z * t25
      t59 = x1 ** 2
      t60 = x2 * t59
      t63 = log(-0.4D1 * t60 * t14)
      t70 = t57 * wd * t31
      t72 = t25 * lh
      t78 = -0.180D3 * t70 + (0.180D3 * t57 - 0.180D3 * t72 * z) * wd * 
     #t31
      t89 = t26 * t29 * z
      t97 = log(-0.4D1 * x2 * t10 * t12 * t4)
      t98 = t97 * t23
      t103 = (-0.180D3 * t26 * lh - 0.90D2 * t98 * t25) * t29 * z
      t112 = t97 ** 2
      t116 = pi ** 2
      t118 = lh ** 2
      t129 = -(0.1440D4 * t6 * t17 * t26 * t32 - 0.16D2 * t41 * t23 * t4
     #4) * t48 / 0.320D3 + 0.9D1 * t6 * t26 * t32 * t53 - (0.1440D4 * t5
     #7 * wd * t31 * t63 * t23 * t29 - 0.16D2 * t78 * t23 * t29) * t52 /
     # 0.160D3 + 0.9D1 / 0.2D1 * t6 * t23 * t44 - (0.180D3 * t89 + t103)
     # * wd * t31 / 0.10D2 + (0.270D3 * t89 + 0.2D1 * t103 + (0.180D3 * 
     #t98 * t72 + 0.45D2 * t112 * t23 * t25 + t26 * (-0.30D2 * t116 + 0.
     #180D3 * t118)) * t29 * z) * wd * t31 / 0.20D2
      t130 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t129)
      t132 = 0.2D1 * t7
      t133 = sqrt(x3)
      t134 = t24 * t133
      t135 = -0.1D1 + t133
      t136 = t133 + 0.1D1
      t137 = t135 * t136
      t139 = Sqrt(t27 * t137)
      t141 = 0.2D1 * t134 * t139
      t143 = t2 * (0.1D1 - x2 - x3 + t132 + t141)
      t145 = t2 * (-x3 + t132 - x2 + t141)
      t146 = 0.1D1 / t21
      t147 = t30 * t146
      t149 = 0.3D1 * t133
      t150 = t133 * x2
      t152 = 0.5D1 * t150 * z
      t153 = z * t24
      t156 = t24 * x3
      t159 = t133 * x3
      t160 = t159 * x2
      t162 = 0.2D1 * t160 * z
      t163 = 0.2D1 * t160
      t164 = 0.5D1 * t150
      t167 = t159 * z
      t169 = 0.2D1 * t133 * z
      t173 = -t149 - t152 - 0.4D1 * t153 * t139 - 0.2D1 * t156 * t139 + 
     #t162 - t163 + t164 + 0.4D1 * t24 * t139 + t159 - t167 + t169 + 0.2
     #D1 * t156 * t139 * z
      t174 = t173 ** 2
      t176 = 0.2D1 * t7 * z
      t177 = x3 * z
      t182 = (t176 - t141 - t177 - t132 - 0.1D1 - t19 + x3 + 0.2D1 * t15
     #3 * t133 * t139 + x2) ** 2
      t183 = 0.1D1 / t182
      t184 = t174 * t183
      t194 = log(0.4D1 * x2 * t12 * t4 * t137 * t10 * x3)
      t206 = -0.9D1 / 0.16D2 * t6 * t147 * t184 * t53 - (-0.90D2 * t6 * 
     #t194 * t147 * t184 + t41 * t30 * t146 * t174 * t183) * t48 / 0.320
     #D3
      t207 = FJET(XB1, XB2, s, 0.0D0, t143, 0.0D0, -t145, 0.0D0, t206)
      t209 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t129)
      t211 = FJET(XB1, XB2, s, 0.0D0, -t145, 0.0D0, t143, 0.0D0, t206)
      t213 = -0.1D1 + x1
      t215 = x1 * z
      t216 = 0.1D1 - x1 + t215
      t217 = 0.1D1 / t216
      t219 = t2 * t213 * x2 * t217
      t220 = t2 * x1
      t223 = t4 * s * t1 * t213
      t228 = s * t30 * x2 * t213 * x1 * t217
      t229 = t4 * t216
      t231 = Sqrt(-t229 * x2)
      t232 = t231 ** 2
      t234 = x1 * x2
      t235 = t234 * z
      t237 = (-0.1D1 + x1 - t215 + x2 - t234 - t19 + t235) ** 2
      t238 = t237 ** 2
      t239 = 0.1D1 / t238
      t242 = t213 ** 2
      t243 = t242 * t213
      t256 = log(-0.4D1 * t60 * t10 * t12 * t217 * t242 * t4)
      t259 = t239 * t243 * t216
      t269 = 0.9D1 * t6 * t25 * t232 * t239 * t243 * t216 * t31 * t48 * 
     #t52 - (0.1440D4 * t70 * t256 * t232 * t259 - 0.16D2 * t78 * t232 *
     # t259) * t52 / 0.160D3
      t270 = FJET(XB1, XB2, s, 0.0D0, -t219, t220, t223, -t228, t269)
      t272 = FJET(XB1, XB2, s, t220, t223, 0.0D0, -t219, -t228, t269)
      t274 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t129)
      t276 = FJET(XB1, XB2, s, t143, 0.0D0, -t145, 0.0D0, 0.0D0, t206)
      t278 = x3 * x1
      t279 = t2 * t278
      t280 = t278 * z
      t281 = t278 * x2
      t282 = t278 * t19
      t286 = Sqrt(t229 * x2 * t135 * t136)
      t288 = 0.2D1 * t134 * t286
      t292 = t2 * t213 * (-x3 + t278 - t280 + t132 - t281 + t282 - x2 + 
     #t288) * t217
      t295 = t2 * x1 * t135 * t136
      t296 = 0.1D1 - x1 + t215 - x2 + t234 - t235 - x3 + t278 - t280 + t
     #132 - t281 + t282 + t288
      t299 = t2 * t213 * t296 * t217
      t300 = 0.1D1 / t237
      t301 = t286 * x1
      t306 = 0.1D1 + t288 - 0.2D1 * t134 * t301 - x1 - x2 - x3 - t235 - 
     #0.2D1 * t280 - t176 - 0.3D1 * t281 + t215 + t132
      t307 = t133 * t286
      t314 = x2 * t11
      t316 = x3 * t59
      t319 = t59 * t11
      t323 = -0.2D1 * t153 * t307 + 0.2D1 * t153 * t307 * x1 + 0.4D1 * t
     #282 - t278 * t314 - 0.2D1 * t316 * t19 + t319 * t7 + t278 * t11 + 
     #t316 * x2 + t19 + t234 + t177 + t278
      t325 = (t306 + t323) ** 2
      t326 = 0.1D1 / t325
      t328 = t286 * z
      t332 = x1 * t159
      t335 = t133 * x1
      t343 = t159 * t59
      t345 = t133 * t59
      t358 = t24 * t286
      t363 = 0.2D1 * t156 * t328 * x1 + t149 - 0.3D1 * t332 * x2 + 0.6D1
     # * t335 * z + 0.8D1 * t335 * x2 - 0.2D1 * t332 * z + t332 * t11 + 
     #t343 * x2 - 0.2D1 * t345 * z - 0.3D1 * t345 * x2 + t319 * t133 - 0
     #.2D1 * t133 * t11 * x1 + 0.4D1 * t153 * t286 + 0.2D1 * t156 * t286
     # + 0.4D1 * t358 * x1 + t152 - t162 + 0.4D1 * t160 * t215
      t384 = -0.11D2 * t335 * t19 + 0.3D1 * t335 * t314 + 0.6D1 * t345 *
     # t19 - 0.3D1 * t319 * t150 + t343 * t314 - 0.2D1 * t343 * t19 - t3
     #32 * t314 - 0.4D1 * t153 * t301 - 0.2D1 * t156 * t301 - 0.2D1 * t1
     #56 * t328 + t163 - t164 + t167 - t169 - 0.4D1 * t358 + t332 - 0.4D
     #1 * t335 + t345 - t159
      t386 = (t363 + t384) ** 2
      t391 = t213 * t48 * t52
      t394 = 0.9D1 / 0.16D2 * t6 * t300 * t326 * t386 * t30 * t216 * t39
     #1
      t395 = FJET(XB1, XB2, s, t279, t292, -t295, -t299, -t228, -t394)
      t398 = wd * t300 * t326
      t402 = t386 * t30 * t216 * t391
      t405 = FJET(XB1, XB2, s, t223, t220, -t219, 0.0D0, -t228, t269)
      t407 = FJET(XB1, XB2, s, t292, t279, -t299, -t295, -t228, -t394)
      t412 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t129)
      t414 = FJET(XB1, XB2, s, -t145, 0.0D0, t143, 0.0D0, 0.0D0, t206)
      t416 = FJET(XB1, XB2, s, -t295, -t299, t279, t292, -t228, -t394)
      t421 = FJET(XB1, XB2, s, -t219, 0.0D0, t223, t220, -t228, t269)
      t423 = FJET(XB1, XB2, s, -t299, -t295, t292, t279, -t228, -t394)
      rrgg2gghhardt14s1e0 = t130 * t129 + t207 * t206 + t209 * t129 + t2
     #11 * t206 + t270 * t269 + t272 * t269 + t274 * t129 + t276 * t206 
     #- 0.9D1 / 0.16D2 * t395 * z * t398 * t402 + t405 * t269 - 0.9D1 / 
     #0.16D2 * t407 * z * t398 * t402 + t412 * t129 + t414 * t206 - 0.9D
     #1 / 0.16D2 * t416 * z * t398 * t402 + t421 * t269 - 0.9D1 / 0.16D2
     # * t423 * z * t398 * t402

      end function



      doubleprecision function rrgg2gghhardt14s1em1
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * wd
      t7 = x2 * z
      t9 = (0.1D1 - x2 + t7) ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t12 = t6 * t11
      t13 = x4 * pi
      t14 = cos(t13)
      t15 = t14 ** 2
      t16 = t4 * x2
      t17 = Sqrt(-t16)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = 0.1D1 / x1
      t30 = t11 * t15
      t36 = Sin(t13)
      t37 = t36 ** 2
      t39 = z ** 2
      t44 = log(-0.4D1 * x2 * t37 / t39 * t4)
      t55 = 0.1D1 / x3
      t60 = 0.9D1 * t12 * t19 * t21 * t22 - 0.9D1 * t12 * t19 * t21 + (0
     #.180D3 * t30 * t18 * z + (-0.180D3 * t30 * lh - 0.90D2 * t44 * t11
     # * t15) * t18 * z) * wd * t21 / 0.20D2 + 0.9D1 / 0.2D1 * t12 * t19
     # * t21 * t55
      t61 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t60)
      t63 = x2 * x3
      t64 = 0.2D1 * t63
      t65 = sqrt(x3)
      t71 = Sqrt(t16 * (-0.1D1 + t65) * (t65 + 0.1D1))
      t73 = 0.2D1 * t14 * t65 * t71
      t75 = t2 * (0.1D1 - x2 - x3 + t64 + t73)
      t77 = t2 * (-x3 + t64 - x2 + t73)
      t81 = t65 * x2
      t84 = z * t14
      t87 = t14 * x3
      t90 = t65 * x3
      t91 = t90 * x2
      t104 = -0.3D1 * t65 - 0.5D1 * t81 * z - 0.4D1 * t84 * t71 - 0.2D1 
     #* t87 * t71 + 0.2D1 * t91 * z - 0.2D1 * t91 + 0.5D1 * t81 + 0.4D1 
     #* t14 * t71 + t90 - t90 * z + 0.2D1 * t65 * z + 0.2D1 * t87 * t71 
     #* z
      t105 = t104 ** 2
      t114 = (0.2D1 * t63 * z - t73 - x3 * z - t64 - 0.1D1 - t7 + x3 + 0
     #.2D1 * t84 * t65 * t71 + x2) ** 2
      t117 = 0.1D1 / t9 * t105 / t114 * t55
      t119 = 0.9D1 / 0.32D2 * t6 * t20 * t117
      t120 = FJET(XB1, XB2, s, 0.0D0, t75, 0.0D0, -t77, 0.0D0, -t119)
      t122 = wd * t20
      t126 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t60)
      t128 = FJET(XB1, XB2, s, 0.0D0, -t77, 0.0D0, t75, 0.0D0, -t119)
      t133 = -0.1D1 + x1
      t135 = x1 * z
      t136 = 0.1D1 - x1 + t135
      t137 = 0.1D1 / t136
      t139 = t2 * t133 * x2 * t137
      t140 = t2 * x1
      t143 = t4 * s * t1 * t133
      t148 = s * t20 * x2 * t133 * x1 * t137
      t151 = Sqrt(-t4 * t136 * x2)
      t152 = t151 ** 2
      t155 = x1 * x2
      t158 = (-0.1D1 + x1 - t135 + x2 - t155 - t7 + t155 * z) ** 2
      t159 = t158 ** 2
      t161 = t133 ** 2
      t166 = 0.1D1 / t159 * t161 * t133 * t136 * t21 * t22
      t168 = 0.9D1 * t6 * t15 * t152 * t166
      t169 = FJET(XB1, XB2, s, 0.0D0, -t139, t140, t143, -t148, t168)
      t172 = wd * t15 * t152
      t176 = FJET(XB1, XB2, s, t140, t143, 0.0D0, -t139, -t148, t168)
      t181 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t60)
      t183 = FJET(XB1, XB2, s, t75, 0.0D0, -t77, 0.0D0, 0.0D0, -t119)
      t188 = FJET(XB1, XB2, s, t143, t140, -t139, 0.0D0, -t148, t168)
      t193 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t60)
      t195 = FJET(XB1, XB2, s, -t139, 0.0D0, t143, t140, -t148, t168)
      t200 = FJET(XB1, XB2, s, -t77, 0.0D0, t75, 0.0D0, 0.0D0, -t119)
      rrgg2gghhardt14s1em1 = t61 * t60 - 0.9D1 / 0.32D2 * t120 * z * t12
     #2 * t117 + t126 * t60 - 0.9D1 / 0.32D2 * t128 * z * t122 * t117 + 
     #0.9D1 * t169 * z * t172 * t166 + 0.9D1 * t176 * z * t172 * t166 + 
     #t181 * t60 - 0.9D1 / 0.32D2 * t183 * z * t122 * t117 + 0.9D1 * t18
     #8 * z * t172 * t166 + t193 * t60 + 0.9D1 * t195 * z * t172 * t166 
     #- 0.9D1 / 0.32D2 * t200 * z * t122 * t117

      end function



      doubleprecision function rrgg2gghhardt14s1em2
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t9 = (0.1D1 - x2 + x2 * z) ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t14 = cos(x4 * pi)
      t15 = t14 ** 2
      t17 = Sqrt(-t4 * x2)
      t18 = t17 ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = 0.9D1 / 0.2D1 * z * wd * t11 * t15 * t18 * t21
      t25 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t24)
      t30 = t11 * t15 * t18 * t21
      t32 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t24)
      t36 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t24)
      t40 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t24)
      rrgg2gghhardt14s1em2 = 0.9D1 / 0.2D1 * t25 * z * wd * t30 + 0.9D1 
     #/ 0.2D1 * t32 * z * wd * t30 + 0.9D1 / 0.2D1 * t36 * z * wd * t30 
     #+ 0.9D1 / 0.2D1 * t40 * z * wd * t30

      end function



      doubleprecision function rrgg2gghhardt14s1em3
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
      rrgg2gghhardt14s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt14s1em4
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
      rrgg2gghhardt14s1em4 = 0.0D0

      end function
  
      subroutine rrgg2gghsoftt14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt14s1e1  
      doubleprecision rrgg2gghsoftt14s1e0  
      doubleprecision rrgg2gghsoftt14s1em1  
      doubleprecision rrgg2gghsoftt14s1em2  
      doubleprecision rrgg2gghsoftt14s1em3  
      doubleprecision rrgg2gghsoftt14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt14s1e1
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
      rrgg2gghsoftt14s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt14s1e0
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
      rrgg2gghsoftt14s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt14s1em1
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
      rrgg2gghsoftt14s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt14s1em2
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
      rrgg2gghsoftt14s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt14s1em3
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
      rrgg2gghsoftt14s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt14s1em4
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
      rrgg2gghsoftt14s1em4 = 0.0D0

      end function
