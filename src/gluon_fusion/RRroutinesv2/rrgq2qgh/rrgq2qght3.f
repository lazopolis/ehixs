  
      subroutine rrgq2qght3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh31J1  
      doubleprecision rrgq2qgh31J2  
      doubleprecision rrgq2qgh31J3  
      doubleprecision rrgq2qgh31J4  
      doubleprecision rrgq2qgh31J5  
      doubleprecision rrgq2qgh31J6  
      doubleprecision rrgq2qgh31J7  
      doubleprecision rrgq2qgh32J1  
      doubleprecision rrgq2qgh32J2  
      doubleprecision rrgq2qgh32J3  
      doubleprecision rrgq2qgh32J4  
      doubleprecision rrgq2qgh32J5  
      doubleprecision rrgq2qgh32J6  
      doubleprecision rrgq2qght3s1e1  
      doubleprecision rrgq2qght3s1e0  
      doubleprecision rrgq2qght3s1em1  
      doubleprecision rrgq2qght3s1em2  
      doubleprecision rrgq2qght3s1em3  
      doubleprecision rrgq2qght3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght3s1e1
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
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = pi * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t21 = t19 * t20
      t22 = x2 * pi
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = t11 ** 2
      t29 = t27 * t28
      t30 = x1 ** 2
      t31 = t6 ** 2
      t32 = t30 * t31
      t33 = t9 ** 2
      t35 = t32 * x4 * t33
      t38 = log(0.4D1 * t29 * t35)
      t39 = t38 * t9
      t40 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t41 = t18 * t40
      t43 = t38 ** 2
      t44 = t43 * t9
      t45 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t46 = t18 * t45
      t53 = pi * lh
      t54 = t3 * t16
      t55 = t19 * t40
      t61 = lh ** 2
      t63 = pi ** 2
      t65 = 0.180D3 * t61 - 0.30D2 * t63
      t66 = pi * t65
      t67 = t66 * t3
      t68 = t16 * t9
      t69 = t68 * t46
      t70 = t67 * t69
      t72 = 0.1D1 / x4
      t78 = log(0.4D1 * t29 * t32 * t33)
      t79 = t78 * pi
      t82 = t78 ** 2
      t83 = t82 * pi
      t87 = (t66 + 0.180D3 * t79 * lh + 0.45D2 * t83) * t3 * t16
      t90 = t15 * t16
      t91 = rrgq2qgh31J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t110 = (pi * (0.60D2 * lh * t63 - 0.240D3 * zeta3 - 0.120D3 * t61 
     #* lh) - t79 * t65 - 0.90D2 * t83 * lh - 0.15D2 * t82 * t78 * pi) *
     # t3 * t16
      t118 = (-0.180D3 * t53 - 0.90D2 * t79) * t3 * t16
      t121 = x3 * t24
      t122 = t26 * t28
      t123 = t121 * t122
      t126 = log(0.4D1 * t123 * t35)
      t127 = t126 * t9
      t133 = t53 * t3
      t137 = 0.1D1 / x3
      t142 = t28 * t30
      t147 = log(0.4D1 * t121 * t26 * t142 * t31 * t33)
      t148 = t147 ** 2
      t149 = t148 * t9
      t152 = t147 * t9
      t166 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41 - t44 * t46 / 0.2D
     #1) - 0.180D3 * t53 * t54 * (-t55 + t39 * t46) - t70) * t72 / 0.720
     #D3 + t87 * t55 / 0.720D3 + t90 * t19 * t91 / 0.8D1 + t110 * t19 * 
     #t45 / 0.720D3 + t118 * t21 / 0.720D3 - (0.90D2 * t15 * t16 * (-t55
     # + t127 * t46) + 0.180D3 * t133 * t69) * t137 * t72 / 0.720D3 + (0
     #.90D2 * t15 * t16 * (t21 + t149 * t46 / 0.2D1 - t152 * t41) - 0.18
     #0D3 * t53 * t54 * (-t152 * t46 + t55) + t70) * t137 / 0.720D3
      t167 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t166)
      t169 = 0.1D1 - x4
      t170 = KAPPA2(x1, x2, 0.0D0, t169, z)
      t171 = s * t170
      t172 = t171 * t4
      t173 = -t169
      t174 = t7 * t173
      t175 = t171 * t174
      t176 = t7 * x4
      t177 = t171 * t176
      t178 = t170 ** 2
      t181 = x1 * t6
      t183 = s * t178 * t11 * t181 * t173
      t185 = 0.1D1 / (-0.2D1 + t170)
      t186 = t178 * t185
      t187 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t175, t1
     #72, -t177, t183)
      t191 = t178 ** 2
      t196 = log(-0.4D1 * t27 * t142 * t31 * x4 * t173 * t191)
      t197 = t196 ** 2
      t198 = t197 * t178
      t199 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t175, t1
     #72, -t177, t183)
      t200 = t185 * t199
      t203 = t196 * t178
      t204 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t175, t1
     #72, -t177, t183)
      t212 = t186 * t204
      t217 = t16 * t178
      t218 = t217 * t200
      t222 = x4 * t173
      t227 = log(-0.4D1 * t123 * t32 * t222 * t191)
      t228 = t227 * t178
      t240 = -(0.90D2 * t15 * t16 * (t186 * t187 + t198 * t200 / 0.2D1 -
     # t203 * t185 * t204) - 0.180D3 * t53 * t54 * (-t203 * t200 + t212)
     # + t67 * t218) * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t228 * t2
     #00 + t212) - 0.180D3 * t133 * t218) * t137 * t72 / 0.720D3
      t241 = FJET(XB1, XB2, s, 0.0D0, t172, t175, -t177, t183, t240)
      t243 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t244 = t18 * t243
      t246 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t247 = t19 * t246
      t248 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t249 = t18 * t248
      t257 = t19 * t243
      t262 = t68 * t249
      t263 = t67 * t262
      t269 = rrgq2qgh32J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t304 = -(0.90D2 * t15 * t16 * (t39 * t244 - t247 - t44 * t249 / 0.
     #2D1) - 0.180D3 * t53 * t54 * (t39 * t249 - t257) - t263) * t72 / 0
     #.720D3 + t87 * t257 / 0.720D3 + t90 * t19 * t269 / 0.8D1 + t110 * 
     #t19 * t248 / 0.720D3 + t118 * t247 / 0.720D3 - (0.90D2 * t15 * t16
     # * (-t257 + t127 * t249) + 0.180D3 * t133 * t262) * t137 * t72 / 0
     #.720D3 + (0.90D2 * t15 * t16 * (t149 * t249 / 0.2D1 + t247 - t152 
     #* t244) - 0.180D3 * t53 * t54 * (t257 - t152 * t249) + t263) * t13
     #7 / 0.720D3
      t305 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t304)
      t307 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t175, t1
     #72, -t177, t183)
      t309 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t175, t1
     #72, -t177, t183)
      t312 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t175, t1
     #72, -t177, t183)
      t313 = t185 * t312
      t320 = t186 * t309
      t326 = t217 * t313
      t341 = -(0.90D2 * t15 * t16 * (t186 * t307 - t203 * t185 * t309 + 
     #t198 * t313 / 0.2D1) - 0.180D3 * t53 * t54 * (t320 - t203 * t313) 
     #+ t67 * t326) * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t228 * t31
     #3 + t320) - 0.180D3 * t133 * t326) * t137 * t72 / 0.720D3
      t342 = FJET(XB1, XB2, s, t172, 0.0D0, -t177, t175, t183, t341)
      t344 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t345 = s * t344
      t346 = t4 * x3
      t347 = t345 * t346
      t348 = -0.1D1 + x3
      t349 = t4 * t348
      t350 = t345 * t349
      t351 = t345 * t7
      t352 = t344 ** 2
      t356 = s * t352 * t11 * t181 * t348
      t358 = t352 ** 2
      t363 = log(-0.4D1 * t123 * t32 * t348 * x4 * t358)
      t364 = t363 * t352
      t366 = 0.1D1 / (-0.2D1 + t344)
      t367 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, t347, -t351, -t
     #350, 0.0D0, t356)
      t368 = t366 * t367
      t370 = t352 * t366
      t371 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, t347, -t351, -t
     #350, 0.0D0, t356)
      t372 = t370 * t371
      t377 = t16 * t352
      t378 = t377 * t368
      t384 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, s, t347, -t351, -t
     #350, 0.0D0, t356)
      t390 = log(-0.4D1 * t123 * t32 * t348 * t358)
      t391 = t390 ** 2
      t392 = t391 * t352
      t395 = t390 * t352
      t411 = -(0.90D2 * t15 * t16 * (-t364 * t368 + t372) - 0.180D3 * t1
     #33 * t378) * t137 * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (-t370 *
     # t384 - t392 * t368 / 0.2D1 + t395 * t366 * t371) - 0.180D3 * t53 
     #* t54 * (t395 * t368 - t372) - t67 * t378) * t137 / 0.720D3
      t412 = FJET(XB1, XB2, s, t347, -t350, -t351, 0.0D0, t356, t411)
      t414 = KAPPA2(x1, x2, x3, t169, z)
      t415 = s * t414
      t416 = t415 * t346
      t417 = t415 * t349
      t418 = t415 * t174
      t419 = t415 * t176
      t420 = t414 ** 2
      t425 = cos(t22)
      t428 = Sqrt(x3 * t348 * t222)
      t433 = s * t420 * t11 * t181 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t425 * t428)
      t437 = t420 ** 2
      t442 = log(0.4D1 * t121 * t122 * t30 * t31 * t348 * t222 * t437)
      t443 = t442 * t420
      t445 = 0.1D1 / (-0.2D1 + t414)
      t446 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, t416, t418, -t4
     #17, -t419, t433)
      t447 = t445 * t446
      t449 = t420 * t445
      t450 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, t416, t418, -t4
     #17, -t419, t433)
      t456 = t16 * t420
      t460 = 0.90D2 * t15 * t16 * (t443 * t447 - t449 * t450) + 0.180D3 
     #* t133 * t456 * t447
      t464 = FJET(XB1, XB2, s, t416, -t417, t418, -t419, t433, -t460 * t
     #137 * t72 / 0.720D3)
      t466 = t137 * t72
      t469 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, t347, -t351, -t
     #350, 0.0D0, t356)
      t470 = t370 * t469
      t471 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, t347, -t351, -t
     #350, 0.0D0, t356)
      t472 = t366 * t471
      t478 = t377 * t472
      t484 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, s, t347, -t351, -t
     #350, 0.0D0, t356)
      t503 = -(0.90D2 * t15 * t16 * (t470 - t364 * t472) - 0.180D3 * t13
     #3 * t478) * t137 * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (-t370 * 
     #t484 - t392 * t472 / 0.2D1 + t395 * t366 * t469) - 0.180D3 * t53 *
     # t54 * (t395 * t472 - t470) - t67 * t478) * t137 / 0.720D3
      t504 = FJET(XB1, XB2, s, -t350, t347, 0.0D0, -t351, t356, t503)
      t506 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, t416, t418, -t4
     #17, -t419, t433)
      t508 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, t416, t418, -t4
     #17, -t419, t433)
      t509 = t445 * t508
      t518 = 0.90D2 * t15 * t16 * (-t449 * t506 + t443 * t509) + 0.180D3
     # * t133 * t456 * t509
      t522 = FJET(XB1, XB2, s, -t417, t416, -t419, t418, t433, -t518 * t
     #137 * t72 / 0.720D3)
      rrgq2qght3s1e1 = t167 * t166 + t241 * t240 + t305 * t304 + t342 * 
     #t341 + t412 * t411 - t464 * t460 * t466 / 0.720D3 + t504 * t503 - 
     #t522 * t518 * t466 / 0.720D3

      end function



      doubleprecision function rrgq2qght3s1e0
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
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = pi * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t21 = t19 * t20
      t22 = x2 * pi
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = t11 ** 2
      t29 = t27 * t28
      t30 = x1 ** 2
      t31 = t6 ** 2
      t32 = t30 * t31
      t33 = t9 ** 2
      t38 = log(0.4D1 * t29 * t32 * x4 * t33)
      t39 = t38 * t9
      t40 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t41 = t18 * t40
      t47 = pi * lh
      t48 = t47 * t3
      t49 = t16 * t9
      t52 = 0.180D3 * t48 * t49 * t41
      t54 = 0.1D1 / x4
      t57 = t15 * t49
      t58 = 0.1D1 / x3
      t59 = t58 * t54
      t63 = x3 * t24
      t65 = t28 * t30
      t70 = log(0.4D1 * t63 * t26 * t65 * t31 * t33)
      t71 = t70 * t9
      t80 = t15 * t16
      t81 = rrgq2qgh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t89 = log(0.4D1 * t29 * t32 * t33)
      t90 = t89 * pi
      t94 = (-0.180D3 * t47 - 0.90D2 * t90) * t3 * t16
      t97 = lh ** 2
      t99 = pi ** 2
      t105 = t89 ** 2
      t110 = (pi * (0.180D3 * t97 - 0.30D2 * t99) + 0.180D3 * t90 * lh +
     # 0.45D2 * t105 * pi) * t3 * t16
      t114 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41) + t52) * t54 / 0.
     #720D3 + t57 * t41 * t59 / 0.8D1 + (0.90D2 * t15 * t16 * (-t71 * t4
     #1 + t21) - t52) * t58 / 0.720D3 + t80 * t19 * t81 / 0.8D1 + t94 * 
     #t21 / 0.720D3 + t110 * t19 * t40 / 0.720D3
      t115 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t114)
      t117 = 0.1D1 - x4
      t118 = KAPPA2(x1, x2, 0.0D0, t117, z)
      t119 = s * t118
      t120 = t119 * t4
      t121 = -t117
      t122 = t7 * t121
      t123 = t119 * t122
      t124 = t7 * x4
      t125 = t119 * t124
      t126 = t118 ** 2
      t129 = x1 * t6
      t131 = s * t126 * t11 * t129 * t121
      t132 = t16 * t126
      t133 = t15 * t132
      t135 = 0.1D1 / (-0.2D1 + t118)
      t136 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t137 = t135 * t136
      t143 = t126 ** 2
      t148 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t143)
      t149 = t148 * t126
      t151 = t126 * t135
      t152 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t164 = -t133 * t137 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t149 *
     # t137 + t151 * t152) - 0.180D3 * t48 * t132 * t137) * t54 / 0.720D
     #3
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t168 = t18 * t167
      t170 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t171 = t19 * t170
      t178 = 0.180D3 * t48 * t49 * t168
      t195 = rrgq2qgh32J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t202 = -(0.90D2 * t15 * t16 * (t39 * t168 - t171) + t178) * t54 / 
     #0.720D3 + t94 * t171 / 0.720D3 + t57 * t168 * t59 / 0.8D1 + (0.90D
     #2 * t15 * t16 * (t171 - t71 * t168) - t178) * t58 / 0.720D3 + t80 
     #* t19 * t195 / 0.8D1 + t110 * t19 * t167 / 0.720D3
      t203 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t202)
      t205 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t207 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t208 = t135 * t207
      t223 = -(0.90D2 * t15 * t16 * (t151 * t205 - t149 * t208) - 0.180D
     #3 * t48 * t132 * t208) * t54 / 0.720D3 - t133 * t208 * t59 / 0.8D1
      t224 = FJET(XB1, XB2, s, t120, 0.0D0, -t125, t123, t131, t223)
      t226 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t227 = s * t226
      t228 = t4 * x3
      t229 = t227 * t228
      t230 = -0.1D1 + x3
      t231 = t4 * t230
      t232 = t227 * t231
      t233 = t227 * t7
      t234 = t226 ** 2
      t238 = s * t234 * t11 * t129 * t230
      t239 = t16 * t234
      t240 = t15 * t239
      t242 = 0.1D1 / (-0.2D1 + t226)
      t243 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t244 = t242 * t243
      t250 = t234 ** 2
      t255 = log(-0.4D1 * t63 * t26 * t28 * t32 * t230 * t250)
      t256 = t255 * t234
      t258 = t234 * t242
      t259 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t271 = -t240 * t244 * t59 / 0.8D1 + (0.90D2 * t15 * t16 * (t256 * 
     #t244 - t258 * t259) + 0.180D3 * t48 * t239 * t244) * t58 / 0.720D3
      t272 = FJET(XB1, XB2, s, t229, -t232, -t233, 0.0D0, t238, t271)
      t274 = KAPPA2(x1, x2, x3, t117, z)
      t275 = s * t274
      t276 = t275 * t228
      t277 = t275 * t231
      t278 = t275 * t122
      t279 = t275 * t124
      t280 = t274 ** 2
      t285 = cos(t22)
      t289 = Sqrt(x3 * t230 * x4 * t121)
      t294 = s * t280 * t11 * t129 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t285 * t289)
      t296 = t15 * t16 * t280
      t298 = 0.1D1 / (-0.2D1 + t274)
      t299 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, t276, t278, -t2
     #77, -t279, t294)
      t304 = FJET(XB1, XB2, s, t276, -t277, t278, -t279, t294, t296 * t2
     #98 * t299 * t59 / 0.8D1)
      t306 = t3 * t16
      t308 = t280 * t298
      t314 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t315 = t242 * t314
      t320 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t332 = -t240 * t315 * t59 / 0.8D1 + (0.90D2 * t15 * t16 * (t256 * 
     #t315 - t258 * t320) + 0.180D3 * t48 * t239 * t315) * t58 / 0.720D3
      t333 = FJET(XB1, XB2, s, -t232, t229, 0.0D0, -t233, t238, t332)
      t335 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, t276, t278, -t2
     #77, -t279, t294)
      t340 = FJET(XB1, XB2, s, -t277, t276, -t279, t278, t294, t296 * t2
     #98 * t335 * t59 / 0.8D1)
      rrgq2qght3s1e0 = t115 * t114 + t165 * t164 + t203 * t202 + t224 * 
     #t223 + t272 * t271 + t304 * pi * t306 * t308 * t299 * t58 * t54 / 
     #0.8D1 + t333 * t332 + t340 * pi * t306 * t308 * t335 * t58 * t54 /
     # 0.8D1

      end function



      doubleprecision function rrgq2qght3s1em1
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
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t17 = pi * t3 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t22 = 0.1D1 / x3
      t27 = rrgq2qgh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t34 = sin(x2 * pi)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t44 = t9 ** 2
      t48 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t44)
      t53 = (-0.180D3 * pi * lh - 0.90D2 * t48 * pi) * t3 * t16
      t57 = 0.1D1 / x4
      t62 = t17 * t20 * t21 * t22 / 0.8D1 + t17 * t20 * t27 / 0.8D1 + t5
     #3 * t20 * t21 / 0.720D3 + t17 * t20 * t21 * t57 / 0.8D1
      t63 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t62)
      t65 = 0.1D1 - x4
      t66 = KAPPA2(x1, x2, 0.0D0, t65, z)
      t67 = s * t66
      t68 = t67 * t4
      t69 = -t65
      t71 = t67 * t7 * t69
      t73 = t67 * t7 * x4
      t74 = t66 ** 2
      t77 = x1 * t6
      t79 = s * t74 * t11 * t77 * t69
      t82 = t74 / (-0.2D1 + t66)
      t83 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t71, t68,
     # -t73, t79)
      t85 = t82 * t83 * t57
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t17 * t85 / 0
     #.8D1)
      t90 = t3 * t16
      t94 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t99 = rrgq2qgh32J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t110 = t17 * t20 * t94 * t22 / 0.8D1 + t17 * t20 * t99 / 0.8D1 + t
     #53 * t20 * t94 / 0.720D3 + t17 * t20 * t94 * t57 / 0.8D1
      t111 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t113 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t71, t68
     #, -t73, t79)
      t115 = t82 * t113 * t57
      t118 = FJET(XB1, XB2, s, t68, 0.0D0, -t73, t71, t79, -t17 * t115 /
     # 0.8D1)
      t123 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t124 = s * t123
      t126 = t124 * t4 * x3
      t127 = -0.1D1 + x3
      t129 = t124 * t4 * t127
      t130 = t124 * t7
      t131 = t123 ** 2
      t135 = s * t131 * t11 * t77 * t127
      t138 = t131 / (-0.2D1 + t123)
      t139 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t130, -t
     #129, 0.0D0, t135)
      t141 = t138 * t139 * t22
      t144 = FJET(XB1, XB2, s, t126, -t129, -t130, 0.0D0, t135, -t17 * t
     #141 / 0.8D1)
      t149 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t130, -t
     #129, 0.0D0, t135)
      t151 = t138 * t149 * t22
      t154 = FJET(XB1, XB2, s, -t129, t126, 0.0D0, -t130, t135, -t17 * t
     #151 / 0.8D1)
      rrgq2qght3s1em1 = t63 * t62 - t88 * pi * t90 * t85 / 0.8D1 + t111 
     #* t110 - t118 * pi * t90 * t115 / 0.8D1 - t144 * pi * t90 * t141 /
     # 0.8D1 - t154 * pi * t90 * t151 / 0.8D1

      end function



      doubleprecision function rrgq2qght3s1em2
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
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t17 = pi * t3 * t16
      t19 = 0.1D1 / (-0.2D1 + t1)
      t20 = t9 * t19
      t21 = rrgq2qgh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t17 * t20 * t
     #21 / 0.8D1)
      t28 = t16 * t9
      t32 = rrgq2qgh32J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t36 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t17 * t20 * t
     #32 / 0.8D1)
      rrgq2qght3s1em2 = t25 * pi * t3 * t28 * t19 * t21 / 0.8D1 + t36 * 
     #pi * t3 * t28 * t19 * t32 / 0.8D1

      end function



      doubleprecision function rrgq2qght3s1em3
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
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght3s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght3s1em4
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
      doubleprecision rrgq2qgh31J1
      doubleprecision rrgq2qgh31J2
      doubleprecision rrgq2qgh31J3
      doubleprecision rrgq2qgh31J4
      doubleprecision rrgq2qgh31J5
      doubleprecision rrgq2qgh31J6
      doubleprecision rrgq2qgh31J7
      doubleprecision rrgq2qgh32J1
      doubleprecision rrgq2qgh32J2
      doubleprecision rrgq2qgh32J3
      doubleprecision rrgq2qgh32J4
      doubleprecision rrgq2qgh32J5
      doubleprecision rrgq2qgh32J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh31J1
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
      rrgq2qgh31J1 = (0.32D2 / 0.9D1 * S34 - 0.32D2 / 0.9D1 * S14 * S23 
     #/ S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh31J2
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
      t3 = 0.1D1 / (S12 + S13 + S23)
      t4 = S34 ** 2
      t11 = S14 ** 2
      t18 = S14 * S23
      rrgq2qgh31J2 = (0.16D2 / 0.9D1 * S12 + 0.16D2 / 0.9D1 * t3 * t4 + 
     #(0.32D2 / 0.9D1 + 0.32D2 / 0.9D1 * S14 * t3) * S34 + 0.16D2 / 0.9D
     #1 * t11 * t3 + ((0.16D2 / 0.9D1 - 0.16D2 / 0.9D1 * S23 * t3) * t4 
     #- 0.32D2 / 0.9D1 * t18 * t3 * S34 - 0.32D2 / 0.9D1 * t18 - 0.32D2 
     #/ 0.9D1 * t11 - 0.16D2 / 0.9D1 * S23 * t11 * t3) / S12) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrgq2qgh31J3
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = s ** 2
      t7 = z ** 2
      t11 = S23 * t4
      t15 = S23 ** 2
      t22 = S12 ** 2
      t33 = S34 ** 2
      t42 = 0.104D3 / 0.9D1 * S23
      t43 = 0.8D1 / 0.9D1 * S14
      t44 = S14 * S23
      t45 = 0.16D2 / 0.3D1 * t44
      t46 = S14 ** 2
      rrgq2qgh31J3 = (0.32D2 / 0.3D1 * S23 * t1 * t4 * t5 * t7 + (-0.32D
     #2 / 0.3D1 * t11 * S34 - 0.64D2 / 0.3D1 * S23 - 0.32D2 / 0.3D1 * t1
     #5 * t4) * t1 * s * z + 0.4D1 / 0.9D1 * t4 * t22 + (0.8D1 / 0.9D1 *
     # t4 * S34 + 0.4D1 / 0.3D1 + (-0.4D1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 
     #* S14) * t4) * S12 + 0.16D2 / 0.9D1 * t4 * t33 + (0.16D2 / 0.3D1 +
     # (0.16D2 / 0.3D1 * S14 - 0.16D2 / 0.9D1 * S23) * t4) * S34 + t42 -
     # t43 + (-t45 + 0.16D2 / 0.3D1 * t46 + 0.4D1 / 0.3D1 * t15) * t4 + 
     #((0.4D1 / 0.3D1 + 0.16D2 / 0.9D1 * t11) * t33 + (t42 - t43 + (0.8D
     #1 * t15 - t45) * t4) * S34 - 0.64D2 / 0.9D1 * t46 + 0.56D2 / 0.3D1
     # * t15 + 0.16D2 / 0.9D1 * t44 + (0.28D2 / 0.9D1 * t15 * S23 - 0.16
     #D2 / 0.3D1 * S23 * t46 + 0.8D1 / 0.3D1 * t15 * S14) * t4) * t1) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh31J4
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = s ** 2
      t7 = z ** 2
      t11 = S23 * t4
      t15 = S23 ** 2
      t22 = S12 ** 2
      t33 = S34 ** 2
      t42 = 0.104D3 / 0.9D1 * S23
      t43 = 0.8D1 / 0.9D1 * S14
      t44 = S14 * S23
      t45 = 0.16D2 / 0.3D1 * t44
      t46 = S14 ** 2
      rrgq2qgh31J4 = (0.32D2 / 0.3D1 * S23 * t1 * t4 * t5 * t7 + (-0.32D
     #2 / 0.3D1 * t11 * S34 - 0.64D2 / 0.3D1 * S23 - 0.32D2 / 0.3D1 * t1
     #5 * t4) * t1 * s * z + 0.4D1 / 0.9D1 * t4 * t22 + (0.8D1 / 0.9D1 *
     # t4 * S34 + 0.4D1 / 0.3D1 + (-0.4D1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 
     #* S14) * t4) * S12 + 0.16D2 / 0.9D1 * t4 * t33 + (0.16D2 / 0.3D1 +
     # (0.16D2 / 0.3D1 * S14 - 0.16D2 / 0.9D1 * S23) * t4) * S34 + t42 -
     # t43 + (-t45 + 0.16D2 / 0.3D1 * t46 + 0.4D1 / 0.3D1 * t15) * t4 + 
     #((0.4D1 / 0.3D1 + 0.16D2 / 0.9D1 * t11) * t33 + (t42 - t43 + (0.8D
     #1 * t15 - t45) * t4) * S34 - 0.64D2 / 0.9D1 * t46 + 0.56D2 / 0.3D1
     # * t15 + 0.16D2 / 0.9D1 * t44 + (0.28D2 / 0.9D1 * t15 * S23 - 0.16
     #D2 / 0.3D1 * S23 * t46 + 0.8D1 / 0.3D1 * t15 * S14) * t4) * t1) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh31J5
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = s ** 2
      t7 = z ** 2
      t11 = S23 * t4
      t15 = S23 ** 2
      t22 = S12 ** 2
      t33 = S34 ** 2
      t42 = 0.104D3 / 0.9D1 * S23
      t43 = 0.8D1 / 0.9D1 * S14
      t44 = S14 * S23
      t45 = 0.16D2 / 0.3D1 * t44
      t46 = S14 ** 2
      rrgq2qgh31J5 = (0.32D2 / 0.3D1 * S23 * t1 * t4 * t5 * t7 + (-0.32D
     #2 / 0.3D1 * t11 * S34 - 0.64D2 / 0.3D1 * S23 - 0.32D2 / 0.3D1 * t1
     #5 * t4) * t1 * s * z + 0.4D1 / 0.9D1 * t4 * t22 + (0.8D1 / 0.9D1 *
     # t4 * S34 + 0.4D1 / 0.3D1 + (-0.4D1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 
     #* S14) * t4) * S12 + 0.16D2 / 0.9D1 * t4 * t33 + (0.16D2 / 0.3D1 +
     # (0.16D2 / 0.3D1 * S14 - 0.16D2 / 0.9D1 * S23) * t4) * S34 + t42 -
     # t43 + (-t45 + 0.16D2 / 0.3D1 * t46 + 0.4D1 / 0.3D1 * t15) * t4 + 
     #((0.4D1 / 0.3D1 + 0.16D2 / 0.9D1 * t11) * t33 + (t42 - t43 + (0.8D
     #1 * t15 - t45) * t4) * S34 - 0.64D2 / 0.9D1 * t46 + 0.56D2 / 0.3D1
     # * t15 + 0.16D2 / 0.9D1 * t44 + (0.28D2 / 0.9D1 * t15 * S23 - 0.16
     #D2 / 0.3D1 * S23 * t46 + 0.8D1 / 0.3D1 * t15 * S14) * t4) * t1) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh31J6
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = s ** 2
      t7 = z ** 2
      t11 = S23 * t4
      t15 = S23 ** 2
      t22 = S12 ** 2
      t33 = S34 ** 2
      t42 = 0.104D3 / 0.9D1 * S23
      t43 = 0.8D1 / 0.9D1 * S14
      t45 = 0.16D2 / 0.3D1 * S14 * S23
      t46 = S14 ** 2
      rrgq2qgh31J6 = (0.32D2 / 0.3D1 * S23 * t1 * t4 * t5 * t7 + (-0.32D
     #2 / 0.3D1 * t11 * S34 - 0.64D2 / 0.3D1 * S23 - 0.32D2 / 0.3D1 * t1
     #5 * t4) * t1 * s * z + 0.4D1 / 0.9D1 * t4 * t22 + (0.8D1 / 0.9D1 *
     # t4 * S34 + 0.4D1 / 0.3D1 + (-0.4D1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 
     #* S14) * t4) * S12 + 0.16D2 / 0.9D1 * t4 * t33 + (0.16D2 / 0.9D1 +
     # (0.16D2 / 0.3D1 * S14 - 0.16D2 / 0.9D1 * S23) * t4) * S34 + t42 -
     # t43 + (-t45 + 0.16D2 / 0.3D1 * t46 + 0.4D1 / 0.3D1 * t15) * t4 + 
     #((0.4D1 / 0.3D1 + 0.16D2 / 0.9D1 * t11) * t33 + (t42 - t43 + (0.8D
     #1 * t15 - t45) * t4) * S34 - 0.64D2 / 0.9D1 * t46 + 0.56D2 / 0.3D1
     # * t15 + t45 + (0.28D2 / 0.9D1 * t15 * S23 - 0.16D2 / 0.3D1 * S23 
     #* t46 + 0.8D1 / 0.3D1 * t15 * S14) * t4) * t1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh31J7
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = s ** 2
      t7 = z ** 2
      t11 = S23 * t4
      t15 = S23 ** 2
      t22 = S12 ** 2
      t38 = 0.104D3 / 0.9D1 * S23
      t39 = 0.8D1 / 0.9D1 * S14
      t40 = S14 * S23
      t41 = 0.16D2 / 0.3D1 * t40
      t42 = S14 ** 2
      t43 = 0.32D2 / 0.9D1 * t42
      t49 = S34 ** 2
      rrgq2qgh31J7 = (0.32D2 / 0.3D1 * S23 * t1 * t4 * t5 * t7 + (-0.32D
     #2 / 0.3D1 * t11 * S34 - 0.64D2 / 0.3D1 * S23 - 0.32D2 / 0.3D1 * t1
     #5 * t4) * t1 * s * z + 0.4D1 / 0.9D1 * t4 * t22 + (0.8D1 / 0.9D1 *
     # t4 * S34 - 0.4D1 / 0.9D1 + (-0.4D1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 
     #* S14) * t4) * S12 + (0.16D2 / 0.9D1 + (0.16D2 / 0.9D1 * S14 - 0.1
     #6D2 / 0.9D1 * S23) * t4) * S34 + t38 - t39 + (-t41 + t43 + 0.4D1 /
     # 0.3D1 * t15) * t4 + ((-0.4D1 / 0.9D1 + 0.32D2 / 0.9D1 * t11) * t4
     #9 + (t38 - t39 + (0.8D1 * t15 - 0.16D2 / 0.9D1 * t40) * t4) * S34 
     #+ 0.56D2 / 0.3D1 * t15 + t41 - t43 + (0.28D2 / 0.9D1 * t15 * S23 +
     # 0.8D1 / 0.3D1 * t15 * S14 - 0.32D2 / 0.9D1 * S23 * t42) * t4) * t
     #1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh32J1
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t10 = S34 ** 2
      t16 = S14 * S23
      t18 = S14 ** 2
      t20 = S23 ** 2
      t25 = 0.1D1 / S12
      t47 = S12 ** 2
      rrgq2qgh32J1 = ((-0.4D1 * t2 * S12 + 0.8D1 * t2 * S34 + (0.8D1 * S
     #23 - 0.8D1 * S14) * t2 + (-0.8D1 * t2 * t10 - 0.8D1 * S23 * t2 * S
     #34 + (0.8D1 * t16 - 0.8D1 * t18 - 0.4D1 * t20) * t2) * t25) * s * 
     #z - 0.12D2 * S12 - 0.40D2 * S34 - 0.24D2 * S23 - 0.24D2 * S14 + (-
     #0.12D2 * t10 + (-0.24D2 * S23 - 0.24D2 * S14) * S34 - 0.12D2 * t20
     # - 0.40D2 * t16 - 0.12D2 * t18) * t25 + (-0.4D1 * S14 * t10 - 0.4D
     #1 * t18 * S14) / t47) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh32J2
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t15 = S14 * S23
      t17 = S23 ** 2
      t22 = 0.1D1 / S12
      t31 = S34 ** 2
      t37 = S14 ** 2
      t51 = S12 ** 2
      rrgq2qgh32J2 = ((0.4D1 * t2 * S12 + 0.16D2 * t2 * S34 + (-0.8D1 * 
     #S23 - 0.16D2 * S14) * t2 + ((-0.16D2 * S23 + 0.16D2 * S14) * t2 * 
     #S34 + (0.16D2 * t15 + 0.4D1 * t17) * t2) * t22) * s * z + 0.16D2 *
     # S12 + 0.8D1 * S34 + 0.32D2 * S14 + 0.32D2 * S23 + (0.16D2 * t31 +
     # (0.32D2 * S14 + 0.32D2 * S23) * S34 + 0.8D1 * t15 + 0.16D2 * t37 
     #+ 0.16D2 * t17) * t22 + ((-0.4D1 * t15 + 0.8D1 * t37) * S34 - 0.4D
     #1 * t17 * S14 - 0.4D1 * S23 * t37) / t51) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh32J3
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t15 = S14 * S23
      t17 = S23 ** 2
      t22 = 0.1D1 / S12
      t31 = S34 ** 2
      t37 = S14 ** 2
      t51 = S12 ** 2
      rrgq2qgh32J3 = ((0.4D1 * t2 * S12 + 0.16D2 * t2 * S34 + (-0.8D1 * 
     #S23 - 0.16D2 * S14) * t2 + ((-0.16D2 * S23 + 0.16D2 * S14) * t2 * 
     #S34 + (0.16D2 * t15 + 0.4D1 * t17) * t2) * t22) * s * z + 0.16D2 *
     # S12 + 0.8D1 * S34 + 0.32D2 * S14 + 0.32D2 * S23 + (0.16D2 * t31 +
     # (0.32D2 * S14 + 0.32D2 * S23) * S34 + 0.8D1 * t15 + 0.16D2 * t37 
     #+ 0.16D2 * t17) * t22 + ((-0.4D1 * t15 + 0.8D1 * t37) * S34 - 0.4D
     #1 * t17 * S14 - 0.4D1 * S23 * t37) / t51) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh32J4
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t15 = S14 * S23
      t17 = S23 ** 2
      t22 = 0.1D1 / S12
      t31 = S34 ** 2
      t37 = S14 ** 2
      t51 = S12 ** 2
      rrgq2qgh32J4 = ((0.4D1 * t2 * S12 + 0.16D2 * t2 * S34 + (-0.8D1 * 
     #S23 - 0.16D2 * S14) * t2 + ((-0.16D2 * S23 + 0.16D2 * S14) * t2 * 
     #S34 + (0.16D2 * t15 + 0.4D1 * t17) * t2) * t22) * s * z + 0.16D2 *
     # S12 + 0.8D1 * S34 + 0.32D2 * S14 + 0.32D2 * S23 + (0.16D2 * t31 +
     # (0.32D2 * S14 + 0.32D2 * S23) * S34 + 0.8D1 * t15 + 0.16D2 * t37 
     #+ 0.16D2 * t17) * t22 + ((-0.4D1 * t15 + 0.8D1 * t37) * S34 - 0.4D
     #1 * t17 * S14 - 0.4D1 * S23 * t37) / t51) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh32J5
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t15 = S14 * S23
      t17 = S23 ** 2
      t22 = 0.1D1 / S12
      t31 = S34 ** 2
      t37 = S14 ** 2
      t51 = S12 ** 2
      rrgq2qgh32J5 = ((0.4D1 * t2 * S12 + 0.16D2 * t2 * S34 + (-0.8D1 * 
     #S23 - 0.16D2 * S14) * t2 + ((-0.16D2 * S23 + 0.16D2 * S14) * t2 * 
     #S34 + (0.16D2 * t15 + 0.4D1 * t17) * t2) * t22) * s * z + 0.16D2 *
     # S12 + 0.8D1 * S34 + 0.32D2 * S14 + 0.32D2 * S23 + (0.16D2 * t31 +
     # (0.32D2 * S14 + 0.32D2 * S23) * S34 + 0.8D1 * t15 + 0.16D2 * t37 
     #+ 0.16D2 * t17) * t22 + ((-0.4D1 * t15 + 0.8D1 * t37) * S34 - 0.4D
     #1 * t17 * S14 - 0.4D1 * S23 * t37) / t51) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh32J6
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t11 = S34 ** 2
      t19 = S23 ** 2
      t20 = S14 ** 2
      t21 = S14 * S23
      t26 = 0.1D1 / S12
      t57 = S12 ** 2
      rrgq2qgh32J6 = ((0.8D1 * t2 * S12 + 0.8D1 * t2 * S34 + (-0.16D2 * 
     #S23 - 0.8D1 * S14) * t2 + (0.8D1 * t2 * t11 + (-0.8D1 * S23 + 0.16
     #D2 * S14) * t2 * S34 + (0.8D1 * t19 + 0.8D1 * t20 + 0.8D1 * t21) *
     # t2) * t26) * s * z + 0.28D2 * S12 + 0.48D2 * S34 + 0.56D2 * S23 +
     # 0.56D2 * S14 + (0.28D2 * t11 + (0.56D2 * S23 + 0.56D2 * S14) * S3
     #4 + 0.28D2 * t19 + 0.48D2 * t21 + 0.28D2 * t20) * t26 + (0.4D1 * S
     #14 * t11 + (-0.4D1 * t21 + 0.8D1 * t20) * S34 + 0.4D1 * t20 * S14 
     #- 0.4D1 * t19 * S14 - 0.4D1 * S23 * t20) / t57) / pi * wd / z

      end function
  
 