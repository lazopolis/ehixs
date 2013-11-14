  
      subroutine rrqq2qqht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqq2qqh11J1  
      doubleprecision rrqq2qqh11J2  
      doubleprecision rrqq2qqh11J3  
      doubleprecision rrqq2qqh12J1  
      doubleprecision rrqq2qqh12J2  
      doubleprecision rrqq2qqh12J3  
      doubleprecision rrqq2qqht1s1e1  
      doubleprecision rrqq2qqht1s1e0  
      doubleprecision rrqq2qqht1s1em1  
      doubleprecision rrqq2qqht1s1em2  
      doubleprecision rrqq2qqht1s1em3  
      doubleprecision rrqq2qqht1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqq2qqht1s1e1
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
      doubleprecision rrqq2qqh11J1
      doubleprecision rrqq2qqh11J2
      doubleprecision rrqq2qqh11J3
      doubleprecision rrqq2qqh12J1
      doubleprecision rrqq2qqh12J2
      doubleprecision rrqq2qqh12J3

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
      t3 = z - 0.1D1
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
      t20 = rrqq2qqh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
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
      t40 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t41 = t18 * t40
      t43 = t38 ** 2
      t44 = t43 * t9
      t45 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t46 = t18 * t45
      t53 = pi * lh
      t54 = t3 * t16
      t55 = t19 * t40
      t61 = pi ** 2
      t63 = lh ** 2
      t65 = -0.30D2 * t61 + 0.180D3 * t63
      t66 = pi * t65
      t67 = t66 * t3
      t68 = t16 * t9
      t69 = t68 * t46
      t70 = t67 * t69
      t72 = 0.1D1 / x4
      t78 = log(0.4D1 * t29 * t32 * t33)
      t79 = t78 * pi
      t83 = (-0.180D3 * t53 - 0.90D2 * t79) * t3 * t16
      t87 = t78 ** 2
      t88 = t87 * pi
      t92 = (t66 + 0.180D3 * t79 * lh + 0.45D2 * t88) * t3 * t16
      t109 = (pi * (-0.240D3 * zeta3 - 0.120D3 * t63 * lh + 0.60D2 * lh 
     #* t61) - t79 * t65 - 0.90D2 * t88 * lh - 0.15D2 * t87 * t78 * pi) 
     #* t3 * t16
      t112 = x3 * t24
      t113 = t26 * t28
      t114 = t112 * t113
      t117 = log(0.4D1 * t114 * t35)
      t118 = t117 * t9
      t124 = t53 * t3
      t128 = 0.1D1 / x3
      t132 = t28 * t30
      t137 = log(0.4D1 * t112 * t26 * t132 * t31 * t33)
      t138 = t137 ** 2
      t139 = t138 * t9
      t142 = t137 * t9
      t156 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41 - t44 * t46 / 0.2D
     #1) - 0.180D3 * t53 * t54 * (-t55 + t39 * t46) - t70) * t72 / 0.720
     #D3 + t83 * t21 / 0.720D3 + t92 * t55 / 0.720D3 + t109 * t19 * t45 
     #/ 0.720D3 + (0.90D2 * t15 * t16 * (t55 - t118 * t46) - 0.180D3 * t
     #124 * t69) * t128 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t21 - 
     #t139 * t46 / 0.2D1 + t142 * t41) - 0.180D3 * t53 * t54 * (t142 * t
     #46 - t55) - t70) * t128 / 0.720D3
      t157 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t156)
      t159 = 0.1D1 - x4
      t160 = KAPPA2(x1, x2, 0.0D0, t159, z)
      t161 = s * t160
      t162 = t161 * t4
      t163 = -t159
      t164 = t7 * t163
      t165 = t161 * t164
      t166 = t7 * x4
      t167 = t161 * t166
      t168 = t160 ** 2
      t171 = x1 * t6
      t173 = s * t168 * t11 * t171 * t163
      t175 = 0.1D1 / (-0.2D1 + t160)
      t176 = t168 * t175
      t177 = rrqq2qqh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165, t1
     #62, -t167, t173)
      t181 = t168 ** 2
      t186 = log(-0.4D1 * t27 * t132 * t31 * x4 * t163 * t181)
      t187 = t186 ** 2
      t188 = t187 * t168
      t189 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165, t1
     #62, -t167, t173)
      t190 = t175 * t189
      t193 = t186 * t168
      t194 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165, t1
     #62, -t167, t173)
      t202 = t176 * t194
      t207 = t16 * t168
      t208 = t207 * t190
      t212 = x4 * t163
      t217 = log(-0.4D1 * t114 * t32 * t212 * t181)
      t218 = t217 * t168
      t230 = -(0.90D2 * t15 * t16 * (t176 * t177 + t188 * t190 / 0.2D1 -
     # t193 * t175 * t194) - 0.180D3 * t53 * t54 * (-t193 * t190 + t202)
     # + t67 * t208) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (t218 * t19
     #0 - t202) + 0.180D3 * t124 * t208) * t128 * t72 / 0.720D3
      t231 = FJET(XB1, XB2, s, 0.0D0, t162, t165, -t167, t173, t230)
      t233 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t234 = t18 * t233
      t236 = rrqq2qqh12J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t237 = t19 * t236
      t238 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t239 = t18 * t238
      t247 = t19 * t233
      t252 = t68 * t239
      t253 = t67 * t252
      t285 = -(0.90D2 * t15 * t16 * (t39 * t234 - t237 - t44 * t239 / 0.
     #2D1) - 0.180D3 * t53 * t54 * (t39 * t239 - t247) - t253) * t72 / 0
     #.720D3 + (0.90D2 * t15 * t16 * (t247 - t118 * t239) - 0.180D3 * t1
     #24 * t252) * t128 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (-t139 *
     # t239 / 0.2D1 - t237 + t142 * t234) - 0.180D3 * t53 * t54 * (-t247
     # + t142 * t239) - t253) * t128 / 0.720D3 + t83 * t237 / 0.720D3 + 
     #t92 * t247 / 0.720D3 + t109 * t19 * t238 / 0.720D3
      t286 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t285)
      t288 = rrqq2qqh12J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165, t1
     #62, -t167, t173)
      t290 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165, t1
     #62, -t167, t173)
      t293 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165, t1
     #62, -t167, t173)
      t294 = t175 * t293
      t301 = t176 * t290
      t307 = t207 * t294
      t322 = -(0.90D2 * t15 * t16 * (t176 * t288 - t193 * t175 * t290 + 
     #t188 * t294 / 0.2D1) - 0.180D3 * t53 * t54 * (t301 - t193 * t294) 
     #+ t67 * t307) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (t218 * t294
     # - t301) + 0.180D3 * t124 * t307) * t128 * t72 / 0.720D3
      t323 = FJET(XB1, XB2, s, t162, 0.0D0, -t167, t165, t173, t322)
      t325 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t326 = s * t325
      t327 = t4 * x3
      t328 = t326 * t327
      t329 = -0.1D1 + x3
      t330 = t4 * t329
      t331 = t326 * t330
      t332 = t326 * t7
      t333 = t325 ** 2
      t337 = s * t333 * t11 * t171 * t329
      t339 = t333 ** 2
      t344 = log(-0.4D1 * t114 * t32 * t329 * x4 * t339)
      t345 = t344 * t333
      t347 = 0.1D1 / (-0.2D1 + t325)
      t348 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t332, -t
     #331, 0.0D0, t337)
      t349 = t347 * t348
      t351 = t333 * t347
      t352 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t332, -t
     #331, 0.0D0, t337)
      t353 = t351 * t352
      t358 = t16 * t333
      t359 = t358 * t349
      t365 = rrqq2qqh11J3(s, XB1, XB2, z, lh, wd, nf, s, t328, -t332, -t
     #331, 0.0D0, t337)
      t371 = log(-0.4D1 * t114 * t32 * t329 * t339)
      t372 = t371 ** 2
      t373 = t372 * t333
      t376 = t371 * t333
      t392 = (0.90D2 * t15 * t16 * (t345 * t349 - t353) + 0.180D3 * t124
     # * t359) * t128 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t351 * t3
     #65 + t373 * t349 / 0.2D1 - t376 * t347 * t352) - 0.180D3 * t53 * t
     #54 * (-t376 * t349 + t353) + t67 * t359) * t128 / 0.720D3
      t393 = FJET(XB1, XB2, s, t328, -t331, -t332, 0.0D0, t337, t392)
      t395 = KAPPA2(x1, x2, x3, t159, z)
      t396 = s * t395
      t397 = t396 * t327
      t398 = t396 * t330
      t399 = t396 * t164
      t400 = t396 * t166
      t401 = t395 ** 2
      t406 = cos(t22)
      t409 = Sqrt(x3 * t329 * t212)
      t414 = s * t401 * t11 * t171 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t406 * t409)
      t418 = t401 ** 2
      t423 = log(0.4D1 * t112 * t113 * t30 * t31 * t329 * t212 * t418)
      t424 = t423 * t401
      t426 = 0.1D1 / (-0.2D1 + t395)
      t427 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, t397, t399, -t3
     #98, -t400, t414)
      t428 = t426 * t427
      t430 = t401 * t426
      t431 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, t397, t399, -t3
     #98, -t400, t414)
      t437 = t16 * t401
      t441 = 0.90D2 * t15 * t16 * (-t424 * t428 + t430 * t431) - 0.180D3
     # * t124 * t437 * t428
      t445 = FJET(XB1, XB2, s, t397, -t398, t399, -t400, t414, t441 * t1
     #28 * t72 / 0.720D3)
      t447 = t128 * t72
      t450 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t332, -t
     #331, 0.0D0, t337)
      t451 = t351 * t450
      t452 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t332, -t
     #331, 0.0D0, t337)
      t453 = t347 * t452
      t459 = t358 * t453
      t465 = rrqq2qqh12J3(s, XB1, XB2, z, lh, wd, nf, s, t328, -t332, -t
     #331, 0.0D0, t337)
      t484 = (0.90D2 * t15 * t16 * (-t451 + t345 * t453) + 0.180D3 * t12
     #4 * t459) * t128 * t72 / 0.720D3 - (0.90D2 * t15 * t16 * (t351 * t
     #465 + t373 * t453 / 0.2D1 - t376 * t347 * t450) - 0.180D3 * t53 * 
     #t54 * (-t376 * t453 + t451) + t67 * t459) * t128 / 0.720D3
      t485 = FJET(XB1, XB2, s, -t331, t328, 0.0D0, -t332, t337, t484)
      t487 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, t397, t399, -t3
     #98, -t400, t414)
      t489 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, t397, t399, -t3
     #98, -t400, t414)
      t490 = t426 * t489
      t499 = 0.90D2 * t15 * t16 * (t430 * t487 - t424 * t490) - 0.180D3 
     #* t124 * t437 * t490
      t503 = FJET(XB1, XB2, s, -t398, t397, -t400, t399, t414, t499 * t1
     #28 * t72 / 0.720D3)
      rrqq2qqht1s1e1 = t157 * t156 + t231 * t230 + t286 * t285 + t323 * 
     #t322 + t393 * t392 + t445 * t441 * t447 / 0.720D3 + t485 * t484 + 
     #t503 * t499 * t447 / 0.720D3

      end function



      doubleprecision function rrqq2qqht1s1e0
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
      doubleprecision rrqq2qqh11J1
      doubleprecision rrqq2qqh11J2
      doubleprecision rrqq2qqh11J3
      doubleprecision rrqq2qqh12J1
      doubleprecision rrqq2qqh12J2
      doubleprecision rrqq2qqh12J3

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
      t3 = z - 0.1D1
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
      t20 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
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
      t40 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
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
      t81 = rrqq2qqh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t89 = log(0.4D1 * t29 * t32 * t33)
      t90 = t89 * pi
      t94 = (-0.180D3 * t47 - 0.90D2 * t90) * t3 * t16
      t97 = pi ** 2
      t99 = lh ** 2
      t105 = t89 ** 2
      t110 = (pi * (-0.30D2 * t97 + 0.180D3 * t99) + 0.180D3 * t90 * lh 
     #+ 0.45D2 * t105 * pi) * t3 * t16
      t114 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41) + t52) * t54 / 0.
     #720D3 + t57 * t41 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (t71 * t41
     # - t21) + t52) * t58 / 0.720D3 + t80 * t19 * t81 / 0.8D1 + t94 * t
     #21 / 0.720D3 + t110 * t19 * t40 / 0.720D3
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
      t136 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t137 = t135 * t136
      t143 = t126 ** 2
      t148 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t143)
      t149 = t148 * t126
      t151 = t126 * t135
      t152 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t164 = -t133 * t137 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t149 *
     # t137 + t151 * t152) - 0.180D3 * t48 * t132 * t137) * t54 / 0.720D
     #3
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t168 = t18 * t167
      t170 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t171 = t19 * t170
      t178 = 0.180D3 * t48 * t49 * t168
      t195 = rrqq2qqh12J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5,
     # 0.0D0, -t14)
      t202 = -(0.90D2 * t15 * t16 * (t39 * t168 - t171) + t178) * t54 / 
     #0.720D3 + t94 * t171 / 0.720D3 + t57 * t168 * t59 / 0.8D1 - (0.90D
     #2 * t15 * t16 * (-t171 + t71 * t168) + t178) * t58 / 0.720D3 + t80
     # * t19 * t195 / 0.8D1 + t110 * t19 * t167 / 0.720D3
      t203 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t202)
      t205 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
     #20, -t125, t131)
      t207 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123, t1
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
      t243 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t244 = t242 * t243
      t250 = t234 ** 2
      t255 = log(-0.4D1 * t63 * t26 * t28 * t32 * t230 * t250)
      t256 = t255 * t234
      t258 = t234 * t242
      t259 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t271 = -t240 * t244 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t256 *
     # t244 + t258 * t259) - 0.180D3 * t48 * t239 * t244) * t58 / 0.720D
     #3
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
      t294 = s * t280 * t11 * t129 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t285 * t289)
      t296 = t15 * t16 * t280
      t298 = 0.1D1 / (-0.2D1 + t274)
      t299 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, t276, t278, -t2
     #77, -t279, t294)
      t304 = FJET(XB1, XB2, s, t276, -t277, t278, -t279, t294, t296 * t2
     #98 * t299 * t59 / 0.8D1)
      t306 = t3 * t16
      t308 = t280 * t298
      t314 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t315 = t242 * t314
      t320 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, t229, -t233, -t
     #232, 0.0D0, t238)
      t332 = -t240 * t315 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (-t256 *
     # t315 + t258 * t320) - 0.180D3 * t48 * t239 * t315) * t58 / 0.720D
     #3
      t333 = FJET(XB1, XB2, s, -t232, t229, 0.0D0, -t233, t238, t332)
      t335 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, t276, t278, -t2
     #77, -t279, t294)
      t340 = FJET(XB1, XB2, s, -t277, t276, -t279, t278, t294, t296 * t2
     #98 * t335 * t59 / 0.8D1)
      rrqq2qqht1s1e0 = t115 * t114 + t165 * t164 + t203 * t202 + t224 * 
     #t223 + t272 * t271 + t304 * pi * t306 * t308 * t299 * t58 * t54 / 
     #0.8D1 + t333 * t332 + t340 * pi * t306 * t308 * t335 * t58 * t54 /
     # 0.8D1

      end function



      doubleprecision function rrqq2qqht1s1em1
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
      doubleprecision rrqq2qqh11J1
      doubleprecision rrqq2qqh11J2
      doubleprecision rrqq2qqh11J3
      doubleprecision rrqq2qqh12J1
      doubleprecision rrqq2qqh12J2
      doubleprecision rrqq2qqh12J3

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
      t3 = z - 0.1D1
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
      t21 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t22 = 0.1D1 / x3
      t27 = rrqq2qqh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
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
      t83 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t71, t68,
     # -t73, t79)
      t85 = t82 * t83 * t57
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t17 * t85 / 0
     #.8D1)
      t90 = t3 * t16
      t94 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t99 = rrqq2qqh12J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t110 = t17 * t20 * t94 * t22 / 0.8D1 + t17 * t20 * t99 / 0.8D1 + t
     #53 * t20 * t94 / 0.720D3 + t17 * t20 * t94 * t57 / 0.8D1
      t111 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t113 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t71, t68
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
      t139 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t130, -t
     #129, 0.0D0, t135)
      t141 = t138 * t139 * t22
      t144 = FJET(XB1, XB2, s, t126, -t129, -t130, 0.0D0, t135, -t17 * t
     #141 / 0.8D1)
      t149 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t130, -t
     #129, 0.0D0, t135)
      t151 = t138 * t149 * t22
      t154 = FJET(XB1, XB2, s, -t129, t126, 0.0D0, -t130, t135, -t17 * t
     #151 / 0.8D1)
      rrqq2qqht1s1em1 = t63 * t62 - t88 * pi * t90 * t85 / 0.8D1 + t111 
     #* t110 - t118 * pi * t90 * t115 / 0.8D1 - t144 * pi * t90 * t141 /
     # 0.8D1 - t154 * pi * t90 * t151 / 0.8D1

      end function



      doubleprecision function rrqq2qqht1s1em2
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
      doubleprecision rrqq2qqh11J1
      doubleprecision rrqq2qqh11J2
      doubleprecision rrqq2qqh11J3
      doubleprecision rrqq2qqh12J1
      doubleprecision rrqq2qqh12J2
      doubleprecision rrqq2qqh12J3

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
      t3 = z - 0.1D1
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
      t21 = rrqq2qqh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t17 * t20 * t
     #21 / 0.8D1)
      t28 = t16 * t9
      t32 = rrqq2qqh12J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t5, 
     #0.0D0, -t14)
      t36 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t17 * t20 * t
     #32 / 0.8D1)
      rrqq2qqht1s1em2 = t25 * pi * t3 * t28 * t19 * t21 / 0.8D1 + t36 * 
     #pi * t3 * t28 * t19 * t32 / 0.8D1

      end function



      doubleprecision function rrqq2qqht1s1em3
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
      doubleprecision rrqq2qqh11J1
      doubleprecision rrqq2qqh11J2
      doubleprecision rrqq2qqh11J3
      doubleprecision rrqq2qqh12J1
      doubleprecision rrqq2qqh12J2
      doubleprecision rrqq2qqh12J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht1s1em3 = 0.0D0

      end function



      doubleprecision function rrqq2qqht1s1em4
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
      doubleprecision rrqq2qqh11J1
      doubleprecision rrqq2qqh11J2
      doubleprecision rrqq2qqh11J3
      doubleprecision rrqq2qqh12J1
      doubleprecision rrqq2qqh12J2
      doubleprecision rrqq2qqh12J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqq2qqh11J1
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
      t3 = S14 ** 2
      t7 = S34 ** 2
      t9 = S23 ** 2
      rrqq2qqh11J1 = (0.64D2 / 0.27D2 * S12 - 0.128D3 / 0.27D2 * S34 + (
     #0.16D2 / 0.9D1 * t3 - 0.64D2 / 0.27D2 * S14 * S23 + 0.64D2 / 0.27D
     #2 * t7 + 0.16D2 / 0.9D1 * t9 + 0.128D3 / 0.27D2 * S13 * S24) / S12
     #) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh11J2
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
      t5 = S34 ** 2
      t10 = S23 ** 2
      t16 = S14 ** 2
      rrqq2qqh11J2 = (-0.64D2 / 0.27D2 * S12 - 0.128D3 / 0.27D2 * S34 - 
     #0.32D2 / 0.9D1 * S14 - 0.32D2 / 0.9D1 * S23 + (-0.64D2 / 0.27D2 * 
     #t5 + (-0.32D2 / 0.9D1 * S14 - 0.32D2 / 0.9D1 * S23) * S34 - 0.16D2
     # / 0.9D1 * t10 - 0.32D2 / 0.9D1 * S14 * S23 + 0.16D2 / 0.27D2 * S2
     #3 * S24 - 0.16D2 / 0.9D1 * t16 + 0.16D2 / 0.27D2 * S14 * S24 + 0.1
     #6D2 / 0.27D2 * S23 * S13 + 0.16D2 / 0.27D2 * S14 * S13) / S12) / p
     #i * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh11J3
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
      t7 = S34 ** 2
      rrqq2qqh11J3 = (-0.16D2 / 0.27D2 * S12 - 0.16D2 / 0.27D2 * S23 - 0
     #.16D2 / 0.27D2 * S24 - 0.16D2 / 0.27D2 * S13 - 0.32D2 / 0.27D2 * S
     #34 - 0.16D2 / 0.27D2 * S14 + (-0.16D2 / 0.27D2 * t7 + (-0.16D2 / 0
     #.27D2 * S23 - 0.16D2 / 0.27D2 * S24 - 0.16D2 / 0.27D2 * S13 - 0.16
     #D2 / 0.27D2 * S14) * S34 - 0.16D2 / 0.27D2 * S23 * S13 - 0.16D2 / 
     #0.27D2 * S23 * S24 - 0.16D2 / 0.27D2 * S14 * S13 - 0.16D2 / 0.27D2
     # * S14 * S24) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh12J1
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
      t3 = S14 ** 2
      t5 = S34 ** 2
      t7 = S23 ** 2
      rrqq2qqh12J1 = (0.64D2 / 0.27D2 * S12 - 0.128D3 / 0.27D2 * S34 + (
     #0.16D2 / 0.9D1 * t3 + 0.64D2 / 0.27D2 * t5 + 0.16D2 / 0.9D1 * t7 -
     # 0.64D2 / 0.27D2 * S14 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh12J2
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
      t5 = S34 ** 2
      t10 = S23 ** 2
      t16 = S14 ** 2
      rrqq2qqh12J2 = (-0.64D2 / 0.27D2 * S12 - 0.128D3 / 0.27D2 * S34 - 
     #0.32D2 / 0.9D1 * S14 - 0.32D2 / 0.9D1 * S23 + (-0.64D2 / 0.27D2 * 
     #t5 + (-0.32D2 / 0.9D1 * S14 - 0.32D2 / 0.9D1 * S23) * S34 - 0.16D2
     # / 0.9D1 * t10 - 0.32D2 / 0.9D1 * S14 * S23 + 0.16D2 / 0.27D2 * S2
     #3 * S24 - 0.16D2 / 0.9D1 * t16 + 0.16D2 / 0.27D2 * S14 * S24 + 0.1
     #6D2 / 0.27D2 * S23 * S13 + 0.16D2 / 0.27D2 * S14 * S13) / S12) / p
     #i * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh12J3
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
      t7 = S34 ** 2
      rrqq2qqh12J3 = (-0.16D2 / 0.27D2 * S12 - 0.16D2 / 0.27D2 * S23 - 0
     #.16D2 / 0.27D2 * S24 - 0.16D2 / 0.27D2 * S13 - 0.32D2 / 0.27D2 * S
     #34 - 0.16D2 / 0.27D2 * S14 + (-0.16D2 / 0.27D2 * t7 + (-0.16D2 / 0
     #.27D2 * S23 - 0.16D2 / 0.27D2 * S24 - 0.16D2 / 0.27D2 * S13 - 0.16
     #D2 / 0.27D2 * S14) * S34 - 0.16D2 / 0.27D2 * S23 * S13 - 0.16D2 / 
     #0.27D2 * S23 * S24 - 0.16D2 / 0.27D2 * S14 * S13 - 0.16D2 / 0.27D2
     # * S14 * S24) / S12) / pi * wd / z

      end function
  
 