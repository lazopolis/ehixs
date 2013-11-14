  
      subroutine rrqg2qght2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh21J1  
      doubleprecision rrqg2qgh21J2  
      doubleprecision rrqg2qgh21J3  
      doubleprecision rrqg2qgh21J4  
      doubleprecision rrqg2qgh21J5  
      doubleprecision rrqg2qgh21J6  
      doubleprecision rrqg2qgh21J7  
      doubleprecision rrqg2qgh22J1  
      doubleprecision rrqg2qgh22J2  
      doubleprecision rrqg2qgh22J3  
      doubleprecision rrqg2qgh22J4  
      doubleprecision rrqg2qgh22J5  
      doubleprecision rrqg2qgh22J6  
      doubleprecision rrqg2qgh22J7  
      doubleprecision rrqg2qght2s1e1  
      doubleprecision rrqg2qght2s1e0  
      doubleprecision rrqg2qght2s1em1  
      doubleprecision rrqg2qght2s1em2  
      doubleprecision rrqg2qght2s1em3  
      doubleprecision rrqg2qght2s1em4  
      doubleprecision rrqg2qght2s2e1  
      doubleprecision rrqg2qght2s2e0  
      doubleprecision rrqg2qght2s2em1  
      doubleprecision rrqg2qght2s2em2  
      doubleprecision rrqg2qght2s2em3  
      doubleprecision rrqg2qght2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght2s1e1
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t9 = t7 * t8
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x3
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = t25 ** 2
      t27 = t19 * t4
      t30 = log(-0.4D1 * t16 * t27)
      t31 = t30 ** 2
      t33 = t26 / 0.2D1 - t31 / 0.2D1
      t37 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t38 = t7 * t37
      t41 = pi * lh
      t42 = t1 * t7
      t43 = t42 * t8
      t47 = -t25 + t30
      t50 = 0.1D1 / x3
      t53 = lh ** 2
      t55 = pi ** 2
      t57 = 0.180D3 * t53 - 0.30D2 * t55
      t58 = pi * t57
      t59 = t12 * t15
      t62 = log(-0.4D1 * t59 * t27)
      t63 = t62 * pi
      t66 = t62 ** 2
      t67 = t66 * pi
      t70 = (t58 + 0.180D3 * t63 * lh + 0.45D2 * t67) * t1
      t73 = rrqg2qgh21J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t91 = (pi * (0.60D2 * lh * t55 - 0.240D3 * zeta3 - 0.120D3 * t53 *
     # lh) - t63 * t57 - 0.90D2 * t67 * lh - 0.15D2 * t66 * t62 * pi) * 
     #t1
      t97 = (-0.180D3 * t41 - 0.90D2 * t63) * t1
      t98 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t102 = t6 * t7
      t103 = x1 ** 2
      t104 = x3 * t103
      t106 = t15 * t18
      t107 = x4 * t4
      t111 = log(-0.4D1 * t104 * t12 * t106 * t107)
      t113 = t104 * t59
      t116 = log(0.4D1 * t113 * t22)
      t120 = 0.1D1 / x1
      t124 = t103 * t12
      t128 = log(-0.4D1 * t124 * t15 * t27)
      t129 = t128 ** 2
      t146 = (0.90D2 * t6 * t9 * t33 + (0.90D2 * t6 * t38 - 0.180D3 * t4
     #1 * t43) * t47) * t50 / 0.1440D4 - t70 * t38 / 0.1440D4 - t6 * t7 
     #* t73 / 0.16D2 - t91 * t9 / 0.1440D4 - t97 * t7 * t98 / 0.1440D4 -
     # t102 * (-t111 * t8 + t116 * t8) * t50 * t120 / 0.8D1 + (0.90D2 * 
     #t6 * t7 * (-t129 * t8 / 0.2D1 + t128 * t37 - t98) - 0.180D3 * t41 
     #* t42 * (t128 * t8 - t37) - t58 * t43) * t120 / 0.720D3
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t146)
      t149 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t150 = t7 * t149
      t154 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t155 = t7 * t154
      t158 = t42 * t149
      t166 = rrqg2qgh22J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t172 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t201 = (0.90D2 * t6 * t150 * t33 + (0.90D2 * t6 * t155 - 0.180D3 *
     # t41 * t158) * t47) * t50 / 0.1440D4 - t6 * t7 * t166 / 0.16D2 - t
     #91 * t150 / 0.1440D4 - t97 * t7 * t172 / 0.1440D4 - t70 * t155 / 0
     #.1440D4 - t102 * (t116 * t149 - t111 * t149) * t50 * t120 / 0.8D1 
     #+ (0.90D2 * t6 * t7 * (t128 * t154 - t129 * t149 / 0.2D1 - t172) -
     # 0.180D3 * t41 * t42 * (-t154 + t128 * t149) - t58 * t158) * t120 
     #/ 0.720D3
      t202 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t201)
      t204 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t205 = s * t204
      t206 = t1 * x1
      t207 = t205 * t206
      t208 = -0.1D1 + x1
      t209 = t1 * t208
      t210 = t209 * x4
      t211 = t205 * t210
      t212 = t209 * t4
      t213 = t205 * t212
      t214 = t204 ** 2
      t217 = x1 * t208
      t219 = s * t214 * t17 * t217 * t4
      t220 = t208 ** 2
      t222 = t214 ** 2
      t227 = log(-0.4D1 * t113 * t19 * t4 * t220 * t222)
      t228 = t227 * t208
      t230 = 0.1D1 / (-0.2D1 + t204)
      t231 = t214 * t230
      t232 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, t207, -t211, 0.
     #0D0, t213, t219)
      t233 = t231 * t232
      t235 = t208 * t214
      t236 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, t207, -t211, 0.
     #0D0, t213, t219)
      t238 = t235 * t230 * t236
      t243 = t41 * t42
      t245 = t235 * t230 * t232
      t256 = log(-0.4D1 * t124 * t106 * t107 * t220 * t222)
      t257 = t256 ** 2
      t258 = t257 * t208
      t261 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, s, t207, -t211, 0.
     #0D0, t213, t219)
      t264 = t256 * t208
      t276 = t58 * t42
      t281 = -(0.90D2 * t6 * t7 * (t228 * t233 - t238) + 0.180D3 * t243 
     #* t245) * t50 * t120 / 0.720D3 + (0.90D2 * t6 * t7 * (t258 * t233 
     #/ 0.2D1 + t235 * t230 * t261 - t264 * t231 * t236) - 0.180D3 * t41
     # * t42 * (t238 - t264 * t233) + t276 * t245) * t120 / 0.720D3
      t282 = FJET(XB1, XB2, s, t207, 0.0D0, -t211, t213, t219, t281)
      t284 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, t207, -t211, 0.
     #0D0, t213, t219)
      t286 = t235 * t230 * t284
      t287 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, t207, -t211, 0.
     #0D0, t213, t219)
      t288 = t231 * t287
      t295 = t235 * t230 * t287
      t303 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, s, t207, -t211, 0.
     #0D0, t213, t219)
      t321 = -(0.90D2 * t6 * t7 * (-t286 + t228 * t288) + 0.180D3 * t243
     # * t295) * t50 * t120 / 0.720D3 + (0.90D2 * t6 * t7 * (t258 * t288
     # / 0.2D1 + t235 * t230 * t303 - t264 * t231 * t284) - 0.180D3 * t4
     #1 * t42 * (t286 - t264 * t288) + t276 * t295) * t120 / 0.720D3
      t322 = FJET(XB1, XB2, s, -t211, t213, t207, 0.0D0, t219, t321)
      t325 = KAPPA2(x1, x2, -t20, x4, z)
      t326 = s * t325
      t328 = t326 * t206 * t20
      t330 = t326 * t206 * x3
      t331 = t326 * t210
      t332 = t326 * t212
      t333 = t325 ** 2
      t338 = cos(t10)
      t341 = Sqrt(x3 * t20 * t107)
      t346 = s * t333 * t17 * t217 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t338 * t341)
      t347 = t208 * t333
      t349 = 0.1D1 / (-0.2D1 + t325)
      t350 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, -t328, -t331, t
     #330, t332, t346)
      t356 = t333 ** 2
      t361 = log(0.4D1 * t104 * t59 * t18 * t107 * t220 * t20 * t356)
      t362 = t361 * t208
      t363 = t333 * t349
      t364 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t328, -t331, t
     #330, t332, t346)
      t375 = 0.90D2 * t6 * t7 * (t347 * t349 * t350 - t362 * t363 * t364
     #) - 0.180D3 * t243 * t347 * t349 * t364
      t379 = FJET(XB1, XB2, s, -t328, t330, -t331, t332, t346, -t375 * t
     #50 * t120 / 0.720D3)
      t381 = t50 * t120
      t384 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, -t328, -t331, t
     #330, t332, t346)
      t387 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, -t328, -t331, t
     #330, t332, t346)
      t398 = 0.90D2 * t6 * t7 * (t347 * t349 * t384 - t362 * t363 * t387
     #) - 0.180D3 * t243 * t347 * t349 * t387
      t402 = FJET(XB1, XB2, s, -t331, t332, -t328, t330, t346, -t398 * t
     #50 * t120 / 0.720D3)
      rrqg2qght2s1e1 = t147 * t146 + t202 * t201 + t282 * t281 + t322 * 
     #t321 - t379 * t375 * t381 / 0.720D3 - t402 * t398 * t381 / 0.720D3

      end function



      doubleprecision function rrqg2qght2s1e0
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t12 = pi * lh
      t14 = x2 * pi
      t15 = sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t21 * x4
      t23 = t22 * t4
      t26 = log(-0.4D1 * t16 * t18 * t23)
      t27 = t26 * pi
      t30 = (-0.180D3 * t12 - 0.90D2 * t27) * t1
      t31 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t35 = lh ** 2
      t37 = pi ** 2
      t43 = t26 ** 2
      t47 = (pi * (0.180D3 * t35 - 0.30D2 * t37) + 0.180D3 * t27 * lh + 
     #0.45D2 * t43 * pi) * t1
      t48 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t52 = t6 * t7
      t54 = x3 * t16 * t18
      t55 = -0.1D1 + x3
      t60 = log(0.4D1 * t54 * t22 * t4 * t55)
      t63 = log(-0.4D1 * t54 * t23)
      t64 = -t60 + t63
      t66 = 0.1D1 / x3
      t70 = x1 ** 2
      t71 = t70 * t16
      t75 = log(-0.4D1 * t71 * t18 * t23)
      t81 = t1 * t7
      t86 = 0.1D1 / x1
      t89 = -t6 * t7 * t8 / 0.16D2 - t30 * t7 * t31 / 0.1440D4 - t47 * t
     #7 * t48 / 0.1440D4 + t52 * t48 * t64 * t66 / 0.16D2 + (0.90D2 * t6
     # * t7 * (t75 * t48 - t31) + 0.180D3 * t12 * t81 * t48) * t86 / 0.7
     #20D3
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t89)
      t92 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t96 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t104 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t119 = -t30 * t7 * t92 / 0.1440D4 - t47 * t7 * t96 / 0.1440D4 + t5
     #2 * t96 * t64 * t66 / 0.16D2 - t6 * t7 * t104 / 0.16D2 + (0.90D2 *
     # t6 * t7 * (-t92 + t75 * t96) + 0.180D3 * t12 * t81 * t96) * t86 /
     # 0.720D3
      t120 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t119)
      t122 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t123 = s * t122
      t124 = t1 * x1
      t125 = t123 * t124
      t126 = -0.1D1 + x1
      t127 = t1 * t126
      t128 = t127 * x4
      t129 = t123 * t128
      t130 = t127 * t4
      t131 = t123 * t130
      t132 = t122 ** 2
      t135 = x1 * t126
      t137 = s * t132 * t20 * t135 * t4
      t139 = t6 * t7 * t126
      t141 = 0.1D1 / (-0.2D1 + t122)
      t142 = t132 * t141
      t143 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t129, 0.
     #0D0, t131, t137)
      t149 = t126 * t132
      t150 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, t125, -t129, 0.
     #0D0, t131, t137)
      t155 = x4 * t4
      t156 = t126 ** 2
      t157 = t132 ** 2
      t162 = log(-0.4D1 * t71 * t18 * t21 * t155 * t156 * t157)
      t163 = t162 * t126
      t170 = t12 * t81
      t178 = t139 * t142 * t143 * t66 * t86 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t149 * t141 * t150 - t163 * t142 * t143) - 0.180D3 * t170 * t14
     #9 * t141 * t143) * t86 / 0.720D3
      t179 = FJET(XB1, XB2, s, t125, 0.0D0, -t129, t131, t137, t178)
      t181 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t129, 0.
     #0D0, t131, t137)
      t187 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, t125, -t129, 0.
     #0D0, t131, t137)
      t203 = t139 * t142 * t181 * t66 * t86 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t149 * t141 * t187 - t163 * t142 * t181) - 0.180D3 * t170 * t14
     #9 * t141 * t181) * t86 / 0.720D3
      t204 = FJET(XB1, XB2, s, -t129, t131, t125, 0.0D0, t137, t203)
      t207 = KAPPA2(x1, x2, -t55, x4, z)
      t208 = s * t207
      t210 = t208 * t124 * t55
      t212 = t208 * t124 * x3
      t213 = t208 * t128
      t214 = t208 * t130
      t215 = t207 ** 2
      t220 = cos(t14)
      t223 = Sqrt(x3 * t55 * t155)
      t228 = s * t215 * t20 * t135 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t220 * t223)
      t231 = t215 / (-0.2D1 + t207)
      t232 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t210, -t213, t
     #212, t214, t228)
      t235 = t231 * t232 * t66 * t86
      t238 = FJET(XB1, XB2, s, -t210, t212, -t213, t214, t228, -t139 * t
     #235 / 0.8D1)
      t240 = t81 * t126
      t244 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, -t210, -t213, t
     #212, t214, t228)
      t247 = t231 * t244 * t66 * t86
      t250 = FJET(XB1, XB2, s, -t213, t214, -t210, t212, t228, -t139 * t
     #247 / 0.8D1)
      rrqg2qght2s1e0 = t90 * t89 + t120 * t119 + t179 * t178 + t204 * t2
     #03 - t238 * pi * t240 * t235 / 0.8D1 - t250 * pi * t240 * t247 / 0
     #.8D1

      end function



      doubleprecision function rrqg2qght2s1em1
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t15 = sin(x2 * pi)
      t16 = t15 ** 2
      t17 = z ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t26 = log(-0.4D1 * t16 / t17 * t21 * x4 * t4)
      t30 = (-0.180D3 * pi * lh - 0.90D2 * t26 * pi) * t1
      t31 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t32 = t7 * t31
      t35 = 0.1D1 / x1
      t39 = -t6 * t7 * t8 / 0.16D2 - t30 * t32 / 0.1440D4 - t6 * t32 * t
     #35 / 0.8D1
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t39)
      t42 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t46 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t47 = t7 * t46
      t53 = -t6 * t7 * t42 / 0.16D2 - t30 * t47 / 0.1440D4 - t6 * t47 * 
     #t35 / 0.8D1
      t54 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t53)
      t56 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t57 = s * t56
      t59 = t57 * t1 * x1
      t60 = -0.1D1 + x1
      t61 = t1 * t60
      t63 = t57 * t61 * x4
      t65 = t57 * t61 * t4
      t66 = t56 ** 2
      t71 = s * t66 * t20 * x1 * t60 * t4
      t73 = t6 * t7 * t60
      t75 = 0.1D1 / (-0.2D1 + t56)
      t76 = t66 * t75
      t77 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, t59, -t63, 0.0D0
     #, t65, t71)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t63, t65, t71, t73 * t76 * t7
     #7 * t35 / 0.8D1)
      t84 = t1 * t7
      t86 = t60 * t66
      t92 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, t59, -t63, 0.0D0
     #, t65, t71)
      t97 = FJET(XB1, XB2, s, -t63, t65, t59, 0.0D0, t71, t73 * t76 * t9
     #2 * t35 / 0.8D1)
      rrqg2qght2s1em1 = t40 * t39 + t54 * t53 + t82 * pi * t84 * t86 * t
     #75 * t77 * t35 / 0.8D1 + t97 * pi * t84 * t86 * t75 * t92 * t35 / 
     #0.8D1

      end function



      doubleprecision function rrqg2qght2s1em2
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrqg2qght2s1em2 = -t12 * pi * t14 * t8 / 0.16D2 - t21 * pi * t14 *
     # t17 / 0.16D2

      end function



      doubleprecision function rrqg2qght2s1em3
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght2s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght2s1em4
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght2s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght2s2e1
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t9 = t7 * t8
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x3
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = t25 ** 2
      t27 = t19 * t4
      t30 = log(-0.4D1 * t16 * t27)
      t31 = t30 ** 2
      t33 = t26 / 0.2D1 - t31 / 0.2D1
      t37 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t38 = t7 * t37
      t41 = pi * lh
      t42 = t1 * t7
      t43 = t42 * t8
      t47 = -t25 + t30
      t50 = 0.1D1 / x3
      t53 = lh ** 2
      t55 = pi ** 2
      t57 = 0.180D3 * t53 - 0.30D2 * t55
      t58 = pi * t57
      t59 = t12 * t15
      t62 = log(-0.4D1 * t59 * t27)
      t63 = t62 * pi
      t66 = t62 ** 2
      t67 = t66 * pi
      t70 = (t58 + 0.180D3 * t63 * lh + 0.45D2 * t67) * t1
      t73 = rrqg2qgh21J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t91 = (pi * (0.60D2 * lh * t55 - 0.240D3 * zeta3 - 0.120D3 * t53 *
     # lh) - t63 * t57 - 0.90D2 * t67 * lh - 0.15D2 * t66 * t62 * pi) * 
     #t1
      t97 = (-0.180D3 * t41 - 0.90D2 * t63) * t1
      t98 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t102 = t6 * t7
      t103 = x1 ** 2
      t104 = x3 * t103
      t106 = t15 * t18
      t107 = x4 * t4
      t111 = log(-0.4D1 * t104 * t12 * t106 * t107)
      t113 = t104 * t59
      t116 = log(0.4D1 * t113 * t22)
      t120 = 0.1D1 / x1
      t124 = t103 * t12
      t128 = log(-0.4D1 * t124 * t15 * t27)
      t129 = t128 ** 2
      t146 = (0.90D2 * t6 * t9 * t33 + (0.90D2 * t6 * t38 - 0.180D3 * t4
     #1 * t43) * t47) * t50 / 0.1440D4 - t70 * t38 / 0.1440D4 - t6 * t7 
     #* t73 / 0.16D2 - t91 * t9 / 0.1440D4 - t97 * t7 * t98 / 0.1440D4 +
     # t102 * (t111 * t8 - t116 * t8) * t50 * t120 / 0.8D1 + (0.90D2 * t
     #6 * t7 * (-t129 * t8 / 0.2D1 + t128 * t37 - t98) - 0.180D3 * t41 *
     # t42 * (t128 * t8 - t37) - t58 * t43) * t120 / 0.720D3
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t146)
      t149 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t150 = s * t149
      t151 = t1 * x1
      t152 = t150 * t151
      t153 = -0.1D1 + x1
      t154 = t1 * t153
      t155 = t154 * x4
      t156 = t150 * t155
      t157 = t154 * t4
      t158 = t150 * t157
      t159 = t149 ** 2
      t162 = x1 * t153
      t164 = s * t159 * t17 * t162 * x4
      t165 = t153 ** 2
      t167 = t159 ** 2
      t172 = log(-0.4D1 * t113 * t19 * t4 * t165 * t167)
      t173 = t172 * t153
      t175 = 0.1D1 / (-0.2D1 + t149)
      t176 = t159 * t175
      t177 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t178 = t176 * t177
      t180 = t153 * t159
      t181 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t183 = t180 * t175 * t181
      t188 = t41 * t42
      t190 = t180 * t175 * t177
      t196 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t204 = log(-0.4D1 * t124 * t106 * t107 * t165 * t167)
      t205 = t204 ** 2
      t206 = t205 * t153
      t209 = t204 * t153
      t221 = t58 * t42
      t226 = (0.90D2 * t6 * t7 * (-t173 * t178 + t183) - 0.180D3 * t188 
     #* t190) * t50 * t120 / 0.720D3 + (0.90D2 * t6 * t7 * (t180 * t175 
     #* t196 + t206 * t178 / 0.2D1 - t209 * t176 * t181) - 0.180D3 * t41
     # * t42 * (-t209 * t178 + t183) + t221 * t190) * t120 / 0.720D3
      t227 = FJET(XB1, XB2, s, 0.0D0, t152, -t156, t158, -t164, t226)
      t229 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t230 = t7 * t229
      t234 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t235 = t7 * t234
      t238 = t42 * t229
      t246 = rrqg2qgh22J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t252 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t281 = (0.90D2 * t6 * t230 * t33 + (0.90D2 * t6 * t235 - 0.180D3 *
     # t41 * t238) * t47) * t50 / 0.1440D4 - t6 * t7 * t246 / 0.16D2 - t
     #91 * t230 / 0.1440D4 - t97 * t7 * t252 / 0.1440D4 - t70 * t235 / 0
     #.1440D4 + t102 * (t111 * t229 - t116 * t229) * t50 * t120 / 0.8D1 
     #+ (0.90D2 * t6 * t7 * (t128 * t234 - t129 * t229 / 0.2D1 - t252) -
     # 0.180D3 * t41 * t42 * (-t234 + t128 * t229) - t58 * t238) * t120 
     #/ 0.720D3
      t282 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t281)
      t284 = KAPPA2(x1, x2, x3, x4, z)
      t285 = s * t284
      t287 = t285 * t151 * x3
      t289 = t285 * t151 * t20
      t290 = t285 * t155
      t291 = t285 * t157
      t292 = t284 ** 2
      t297 = cos(t10)
      t300 = Sqrt(x3 * t20 * t107)
      t305 = s * t292 * t17 * t162 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t297 * t300)
      t306 = t153 * t292
      t308 = 0.1D1 / (-0.2D1 + t284)
      t309 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, t287, -t290, -t
     #289, t291, t305)
      t315 = t292 ** 2
      t320 = log(0.4D1 * t104 * t59 * t18 * t107 * t165 * t20 * t315)
      t321 = t320 * t153
      t322 = t292 * t308
      t323 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, t287, -t290, -t
     #289, t291, t305)
      t334 = 0.90D2 * t6 * t7 * (-t306 * t308 * t309 + t321 * t322 * t32
     #3) + 0.180D3 * t188 * t306 * t308 * t323
      t338 = FJET(XB1, XB2, s, t287, -t289, -t290, t291, t305, t334 * t5
     #0 * t120 / 0.720D3)
      t340 = t50 * t120
      t343 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t345 = t180 * t175 * t343
      t346 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t347 = t176 * t346
      t354 = t180 * t175 * t346
      t360 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t380 = (0.90D2 * t6 * t7 * (t345 - t173 * t347) - 0.180D3 * t188 *
     # t354) * t50 * t120 / 0.720D3 + (0.90D2 * t6 * t7 * (t180 * t175 *
     # t360 - t209 * t176 * t343 + t206 * t347 / 0.2D1) - 0.180D3 * t41 
     #* t42 * (t345 - t209 * t347) + t221 * t354) * t120 / 0.720D3
      t381 = FJET(XB1, XB2, s, -t156, t158, 0.0D0, t152, -t164, t380)
      t383 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, t287, -t290, -t
     #289, t291, t305)
      t386 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, t287, -t290, -t
     #289, t291, t305)
      t397 = 0.90D2 * t6 * t7 * (-t306 * t308 * t383 + t321 * t322 * t38
     #6) + 0.180D3 * t188 * t306 * t308 * t386
      t401 = FJET(XB1, XB2, s, -t290, t291, t287, -t289, t305, t397 * t5
     #0 * t120 / 0.720D3)
      rrqg2qght2s2e1 = t147 * t146 + t227 * t226 + t282 * t281 + t338 * 
     #t334 * t340 / 0.720D3 + t381 * t380 + t401 * t397 * t340 / 0.720D3

      end function



      doubleprecision function rrqg2qght2s2e0
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t12 = pi * lh
      t14 = x2 * pi
      t15 = sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t21 * x4
      t23 = t22 * t4
      t26 = log(-0.4D1 * t16 * t18 * t23)
      t27 = t26 * pi
      t30 = (-0.180D3 * t12 - 0.90D2 * t27) * t1
      t31 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t35 = lh ** 2
      t37 = pi ** 2
      t43 = t26 ** 2
      t47 = (pi * (0.180D3 * t35 - 0.30D2 * t37) + 0.180D3 * t27 * lh + 
     #0.45D2 * t43 * pi) * t1
      t48 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t52 = t6 * t7
      t54 = x3 * t16 * t18
      t55 = -0.1D1 + x3
      t60 = log(0.4D1 * t54 * t22 * t4 * t55)
      t63 = log(-0.4D1 * t54 * t23)
      t64 = -t60 + t63
      t66 = 0.1D1 / x3
      t70 = x1 ** 2
      t71 = t70 * t16
      t75 = log(-0.4D1 * t71 * t18 * t23)
      t81 = t1 * t7
      t86 = 0.1D1 / x1
      t89 = -t6 * t7 * t8 / 0.16D2 - t30 * t7 * t31 / 0.1440D4 - t47 * t
     #7 * t48 / 0.1440D4 + t52 * t48 * t64 * t66 / 0.16D2 + (0.90D2 * t6
     # * t7 * (t75 * t48 - t31) + 0.180D3 * t12 * t81 * t48) * t86 / 0.7
     #20D3
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t89)
      t92 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t93 = s * t92
      t94 = t1 * x1
      t95 = t93 * t94
      t96 = -0.1D1 + x1
      t97 = t1 * t96
      t98 = t97 * x4
      t99 = t93 * t98
      t100 = t97 * t4
      t101 = t93 * t100
      t102 = t92 ** 2
      t105 = x1 * t96
      t107 = s * t102 * t20 * t105 * x4
      t109 = t6 * t7 * t96
      t111 = 0.1D1 / (-0.2D1 + t92)
      t112 = t102 * t111
      t113 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t99, t9
     #5, t101, -t107)
      t121 = x4 * t4
      t122 = t96 ** 2
      t123 = t102 ** 2
      t128 = log(-0.4D1 * t71 * t18 * t21 * t121 * t122 * t123)
      t129 = t128 * t96
      t132 = t96 * t102
      t133 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t99, t9
     #5, t101, -t107)
      t140 = t12 * t81
      t148 = t109 * t112 * t113 * t66 * t86 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (-t129 * t112 * t113 + t132 * t111 * t133) - 0.180D3 * t140 * t1
     #32 * t111 * t113) * t86 / 0.720D3
      t149 = FJET(XB1, XB2, s, 0.0D0, t95, -t99, t101, -t107, t148)
      t151 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t155 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t163 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t178 = -t30 * t7 * t151 / 0.1440D4 - t47 * t7 * t155 / 0.1440D4 + 
     #t52 * t155 * t64 * t66 / 0.16D2 - t6 * t7 * t163 / 0.16D2 + (0.90D
     #2 * t6 * t7 * (-t151 + t75 * t155) + 0.180D3 * t12 * t81 * t155) *
     # t86 / 0.720D3
      t179 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t178)
      t181 = KAPPA2(x1, x2, x3, x4, z)
      t182 = s * t181
      t184 = t182 * t94 * x3
      t186 = t182 * t94 * t55
      t187 = t182 * t98
      t188 = t182 * t100
      t189 = t181 ** 2
      t194 = cos(t14)
      t197 = Sqrt(x3 * t55 * t121)
      t202 = s * t189 * t20 * t105 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t194 * t197)
      t205 = t189 / (-0.2D1 + t181)
      t206 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, t184, -t187, -t
     #186, t188, t202)
      t209 = t205 * t206 * t66 * t86
      t212 = FJET(XB1, XB2, s, t184, -t186, -t187, t188, t202, -t109 * t
     #209 / 0.8D1)
      t214 = t81 * t96
      t218 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t99, t9
     #5, t101, -t107)
      t224 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t99, t9
     #5, t101, -t107)
      t240 = t109 * t112 * t218 * t66 * t86 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t132 * t111 * t224 - t129 * t112 * t218) - 0.180D3 * t140 * t13
     #2 * t111 * t218) * t86 / 0.720D3
      t241 = FJET(XB1, XB2, s, -t99, t101, 0.0D0, t95, -t107, t240)
      t243 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, t184, -t187, -t
     #186, t188, t202)
      t246 = t205 * t243 * t66 * t86
      t249 = FJET(XB1, XB2, s, -t187, t188, t184, -t186, t202, -t109 * t
     #246 / 0.8D1)
      rrqg2qght2s2e0 = t90 * t89 + t149 * t148 + t179 * t178 - t212 * pi
     # * t214 * t209 / 0.8D1 + t241 * t240 - t249 * pi * t214 * t246 / 0
     #.8D1

      end function



      doubleprecision function rrqg2qght2s2em1
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t15 = sin(x2 * pi)
      t16 = t15 ** 2
      t17 = z ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t26 = log(-0.4D1 * t16 / t17 * t21 * x4 * t4)
      t30 = (-0.180D3 * pi * lh - 0.90D2 * t26 * pi) * t1
      t31 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t32 = t7 * t31
      t35 = 0.1D1 / x1
      t39 = -t6 * t7 * t8 / 0.16D2 - t30 * t32 / 0.1440D4 - t6 * t32 * t
     #35 / 0.8D1
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t39)
      t42 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t43 = s * t42
      t45 = t43 * t1 * x1
      t46 = -0.1D1 + x1
      t47 = t1 * t46
      t49 = t43 * t47 * x4
      t51 = t43 * t47 * t4
      t52 = t42 ** 2
      t57 = s * t52 * t20 * x1 * t46 * x4
      t59 = t6 * t7 * t46
      t61 = 0.1D1 / (-0.2D1 + t42)
      t62 = t52 * t61
      t63 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49, t45
     #, t51, -t57)
      t68 = FJET(XB1, XB2, s, 0.0D0, t45, -t49, t51, -t57, t59 * t62 * t
     #63 * t35 / 0.8D1)
      t70 = t1 * t7
      t72 = t46 * t52
      t78 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t82 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t83 = t7 * t82
      t89 = -t6 * t7 * t78 / 0.16D2 - t30 * t83 / 0.1440D4 - t6 * t83 * 
     #t35 / 0.8D1
      t90 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49, t45
     #, t51, -t57)
      t97 = FJET(XB1, XB2, s, -t49, t51, 0.0D0, t45, -t57, t59 * t62 * t
     #92 * t35 / 0.8D1)
      rrqg2qght2s2em1 = t40 * t39 + t68 * pi * t70 * t72 * t61 * t63 * t
     #35 / 0.8D1 + t90 * t89 + t97 * pi * t70 * t72 * t61 * t92 * t35 / 
     #0.8D1

      end function



      doubleprecision function rrqg2qght2s2em2
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

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
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrqg2qght2s2em2 = -t12 * pi * t14 * t8 / 0.16D2 - t21 * pi * t14 *
     # t17 / 0.16D2

      end function



      doubleprecision function rrqg2qght2s2em3
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght2s2em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght2s2em4
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
      doubleprecision rrqg2qgh21J1
      doubleprecision rrqg2qgh21J2
      doubleprecision rrqg2qgh21J3
      doubleprecision rrqg2qgh21J4
      doubleprecision rrqg2qgh21J5
      doubleprecision rrqg2qgh21J6
      doubleprecision rrqg2qgh21J7
      doubleprecision rrqg2qgh22J1
      doubleprecision rrqg2qgh22J2
      doubleprecision rrqg2qgh22J3
      doubleprecision rrqg2qgh22J4
      doubleprecision rrqg2qgh22J5
      doubleprecision rrqg2qgh22J6
      doubleprecision rrqg2qgh22J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh21J1
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
      t1 = S12 + S13 + S23
      t2 = t1 ** 2
      t3 = 0.1D1 / t2
      t6 = 0.1D1 / t1
      t7 = S23 * t6
      t9 = S13 ** 2
      t13 = 0.1D1 / S12
      t16 = s ** 2
      t18 = z ** 2
      t20 = S13 * t6
      t26 = S14 * S23
      t46 = 0.4D1 * t26
      t47 = S13 * S24
      t49 = S23 ** 2
      t52 = S13 * S14
      t58 = S34 ** 2
      t67 = S23 * S24
      t73 = S24 ** 2
      t75 = S14 ** 2
      t78 = S24 * S14
      t86 = t67 * S14
      t88 = S13 * t73
      t92 = t78 * S13
      t94 = t73 * S23
      t96 = t75 * S13
      t121 = S12 ** 2
      rrqg2qgh21J1 = ((-0.16D2 / 0.9D1 * S13 * t3 + (0.48D2 * t7 + 0.16D
     #2 / 0.9D1 * t9 * t3) * t13) * t16 * t18 + (0.32D2 / 0.9D1 * t20 + 
     #(-0.48D2 * t7 * S34 - 0.96D2 * S23 + (-0.32D2 / 0.9D1 * t9 - 0.48D
     #2 * t26) * t6) * t13) * s * z + (0.8D1 * S23 - 0.47D2 / 0.36D2 * S
     #13) * t6 * S12 + (-0.8D1 / 0.3D1 * S13 - 0.4D1 * S23) * t6 * S34 -
     # 0.17D2 / 0.36D2 * S13 + (t46 - 0.32D2 / 0.9D1 * t47 - 0.16D2 * t4
     #9 + 0.47D2 / 0.18D2 * t9 - 0.37D2 / 0.9D1 * t52) * t6 + ((0.7D1 - 
     #0.187D3 / 0.9D1 * t20) * t58 + (0.6D1 * S24 - 0.4D1 * S14 + 0.17D2
     # / 0.2D1 * S13 + (0.4D1 * t49 + 0.8D1 / 0.3D1 * t9 - 0.220D3 / 0.9
     #D1 * t47 - 0.4D1 / 0.9D1 * t52 - 0.40D2 * t67) * t6) * S34 + 0.2D1
     # * t73 + t75 - 0.64D2 * t67 + 0.275D3 / 0.36D2 * t9 - 0.2D1 * t78 
     #+ 0.61D2 / 0.18D2 * t52 + 0.44D2 / 0.3D1 * t47 + (-0.4D1 * t49 * S
     #14 - 0.47D2 / 0.36D2 * t9 * S13 - 0.40D2 * t86 - 0.26D2 / 0.3D1 * 
     #t88 + 0.8D1 * t49 * S23 + 0.20D2 / 0.3D1 * t92 - 0.40D2 * t94 + t9
     #6 / 0.3D1 + 0.32D2 / 0.9D1 * t9 * S24 + 0.37D2 / 0.9D1 * S14 * t9)
     # * t6) * t13 + ((0.8D1 / 0.9D1 * S13 + S23) * t58 + (-0.2D1 * t67 
     #- t46 + 0.8D1 / 0.9D1 * t47) * S34 + 0.7D1 * S23 * t75 + 0.8D1 / 0
     #.9D1 * t88 + 0.8D1 / 0.9D1 * t96 + 0.8D1 / 0.9D1 * t92 + 0.6D1 * t
     #86 + 0.2D1 * t94) / t121) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh21J2
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
      t1 = S12 + S13 + S23
      t2 = t1 ** 2
      t3 = 0.1D1 / t2
      t5 = S13 ** 2
      t7 = 0.1D1 / S12
      t11 = s ** 2
      t13 = z ** 2
      t15 = 0.1D1 / t1
      t23 = 0.8D1 * S23
      t36 = 0.78D2 * S23
      t38 = S14 * S23
      t40 = S13 * S14
      t42 = S13 * S24
      t44 = S23 ** 2
      t52 = S34 ** 2
      t62 = S23 * S24
      t69 = S14 ** 2
      t75 = S24 * S14
      t83 = S23 * t69
      t85 = t44 * S23
      t91 = t62 * S14
      t93 = S24 ** 2
      t94 = S13 * t93
      t96 = t75 * S13
      t100 = (0.7D1 + (t23 - 0.13D2 / 0.3D1 * S13) * t15) * t52 + (0.29D
     #2 / 0.9D1 * S13 + t36 + 0.6D1 * S24 - 0.4D1 * S14 + (-0.20D2 / 0.3
     #D1 * t42 + 0.16D2 * t38 + 0.2D1 * t44 + 0.148D3 / 0.9D1 * t40 - 0.
     #136D3 / 0.9D1 * t5 + 0.8D1 * t62) * t15) * S34 + 0.78D2 * t38 + t6
     #9 + 0.56D2 * t62 + 0.78D2 * t44 + 0.119D3 / 0.36D2 * t5 - t40 / 0.
     #3D1 - 0.56D2 / 0.9D1 * t42 - 0.2D1 * t75 + (-0.92D2 / 0.9D1 * t5 *
     # S24 - 0.2D1 * t44 * S14 + 0.7D1 / 0.9D1 * t69 * S13 + 0.8D1 * t83
     # + 0.8D1 * t85 + 0.7D1 / 0.36D2 * t5 * S13 - 0.17D2 / 0.9D1 * S14 
     #* t5 + 0.8D1 * t91 - 0.4D1 / 0.3D1 * t94 + 0.76D2 / 0.9D1 * t96) *
     # t15
      t117 = S12 ** 2
      rrqg2qgh21J2 = ((0.16D2 / 0.9D1 * S13 * t3 - 0.16D2 / 0.9D1 * t5 *
     # t3 * t7) * t11 * t13 + (-0.32D2 / 0.9D1 * S13 * t15 + 0.32D2 / 0.
     #9D1 * t5 * t15 * t7) * s * z + (0.2D1 + (t23 + 0.7D1 / 0.36D2 * S1
     #3) * t15) * S12 + (-0.2D1 * S23 + 0.136D3 / 0.9D1 * S13) * t15 * S
     #34 + 0.2D1 * S24 + 0.473D3 / 0.36D2 * S13 + t36 + (-0.7D1 / 0.18D2
     # * t5 + 0.2D1 * t38 + 0.17D2 / 0.9D1 * t40 + 0.92D2 / 0.9D1 * t42 
     #- 0.16D2 * t44) * t15 + t100 * t7 + (S23 * t52 + (-0.2D1 * t62 - 0
     #.16D2 / 0.9D1 * t40 - 0.4D1 * t38 - 0.8D1 / 0.9D1 * t42) * S34 + 0
     #.7D1 * t83 + 0.2D1 * S24 * t44 - 0.8D1 / 0.9D1 * t94 - 0.8D1 / 0.9
     #D1 * t96 + 0.2D1 * t85 + 0.6D1 * t91) / t117) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh21J3
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
      t1 = S13 ** 2
      t3 = 0.1D1 / (S12 + S13 + S23)
      t5 = 0.1D1 / S12
      t10 = 0.8D1 * S23
      t23 = 0.78D2 * S23
      t25 = S14 * S23
      t27 = S13 * S14
      t29 = S13 * S24
      t31 = S23 ** 2
      t39 = S34 ** 2
      t49 = S23 * S24
      t56 = S14 ** 2
      t62 = S24 * S14
      t69 = S23 * t56
      t71 = t31 * S23
      t77 = t49 * S14
      t79 = S24 ** 2
      t80 = S13 * t79
      t82 = t62 * S13
      t86 = (0.7D1 + (t10 - 0.55D2 / 0.9D1 * S13) * t3) * t39 + (0.7085D
     #4 / 0.9D1 * S13 + t23 + 0.6D1 * S24 - 0.4D1 * S14 + (-0.92D2 / 0.9
     #D1 * t29 + 0.16D2 * t25 + 0.2D1 * t31 + 0.116D3 / 0.9D1 * t27 - 0.
     #8D1 * t1 + 0.8D1 * t49) * t3) * S34 + 0.78D2 * t25 + t56 + 0.56D2 
     #* t49 + 0.78D2 * t31 - 0.28253D5 / 0.36D2 * t1 - 0.79D2 / 0.9D1 * 
     #t27 + 0.18518D5 / 0.9D1 * t29 - 0.2D1 * t62 + (-0.10D2 * t1 * S24 
     #- 0.2D1 * t31 * S14 - t56 * S13 + 0.8D1 * t69 + 0.8D1 * t71 + 0.25
     #9D3 / 0.36D2 * t1 * S13 + 0.5D1 / 0.3D1 * S14 * t1 + 0.8D1 * t77 -
     # 0.28D2 / 0.9D1 * t80 + 0.44D2 / 0.9D1 * t82) * t3
      t103 = S12 ** 2
      rrqg2qgh21J3 = (-0.32D2 / 0.3D1 * t1 * t3 * t5 * s * z + (0.2D1 + 
     #(t10 + S13 / 0.12D2) * t3) * S12 + (-0.2D1 * S23 + 0.136D3 / 0.9D1
     # * S13) * t3 * S34 + 0.2D1 * S24 + 0.25693D5 / 0.36D2 * S13 + t23 
     #+ (-t1 / 0.6D1 + 0.2D1 * t25 + 0.17D2 / 0.9D1 * t27 + 0.10D2 * t29
     # - 0.16D2 * t31) * t3 + t86 * t5 + (S23 * t39 + (-0.2D1 * t49 - 0.
     #16D2 / 0.9D1 * t27 - 0.4D1 * t25 - 0.8D1 / 0.9D1 * t29) * S34 + 0.
     #7D1 * t69 + 0.2D1 * S24 * t31 - 0.8D1 / 0.9D1 * t80 - 0.8D1 / 0.9D
     #1 * t82 + 0.2D1 * t71 + 0.6D1 * t77) / t103) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh21J4
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
      t1 = S13 ** 2
      t3 = 0.1D1 / (S12 + S13 + S23)
      t5 = 0.1D1 / S12
      t10 = 0.8D1 * S23
      t23 = 0.78D2 * S23
      t25 = S14 * S23
      t27 = S13 * S14
      t29 = S13 * S24
      t31 = S23 ** 2
      t39 = S34 ** 2
      t49 = S23 * S24
      t56 = S14 ** 2
      t62 = S24 * S14
      t69 = S23 * t56
      t71 = t31 * S23
      t77 = t49 * S14
      t79 = S24 ** 2
      t80 = S13 * t79
      t82 = t62 * S13
      t86 = (0.7D1 + (t10 - 0.55D2 / 0.9D1 * S13) * t3) * t39 + (0.7085D
     #4 / 0.9D1 * S13 + t23 + 0.6D1 * S24 - 0.4D1 * S14 + (-0.92D2 / 0.9
     #D1 * t29 + 0.16D2 * t25 + 0.2D1 * t31 + 0.116D3 / 0.9D1 * t27 - 0.
     #8D1 * t1 + 0.8D1 * t49) * t3) * S34 + 0.78D2 * t25 + t56 + 0.56D2 
     #* t49 + 0.78D2 * t31 - 0.28253D5 / 0.36D2 * t1 - 0.79D2 / 0.9D1 * 
     #t27 + 0.18518D5 / 0.9D1 * t29 - 0.2D1 * t62 + (-0.10D2 * t1 * S24 
     #- 0.2D1 * t31 * S14 - t56 * S13 + 0.8D1 * t69 + 0.8D1 * t71 + 0.25
     #9D3 / 0.36D2 * t1 * S13 + 0.5D1 / 0.3D1 * S14 * t1 + 0.8D1 * t77 -
     # 0.28D2 / 0.9D1 * t80 + 0.44D2 / 0.9D1 * t82) * t3
      t103 = S12 ** 2
      rrqg2qgh21J4 = (-0.32D2 / 0.3D1 * t1 * t3 * t5 * s * z + (0.2D1 + 
     #(t10 + S13 / 0.12D2) * t3) * S12 + (-0.2D1 * S23 + 0.136D3 / 0.9D1
     # * S13) * t3 * S34 + 0.2D1 * S24 + 0.25693D5 / 0.36D2 * S13 + t23 
     #+ (-t1 / 0.6D1 + 0.2D1 * t25 + 0.17D2 / 0.9D1 * t27 + 0.10D2 * t29
     # - 0.16D2 * t31) * t3 + t86 * t5 + (S23 * t39 + (-0.2D1 * t49 - 0.
     #16D2 / 0.9D1 * t27 - 0.4D1 * t25 - 0.8D1 / 0.9D1 * t29) * S34 + 0.
     #7D1 * t69 + 0.2D1 * S24 * t31 - 0.8D1 / 0.9D1 * t80 - 0.8D1 / 0.9D
     #1 * t82 + 0.2D1 * t71 + 0.6D1 * t77) / t103) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh21J5
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
      t1 = S13 ** 2
      t3 = 0.1D1 / (S12 + S13 + S23)
      t5 = 0.1D1 / S12
      t10 = 0.8D1 * S23
      t23 = 0.78D2 * S23
      t25 = S14 * S23
      t27 = S13 * S14
      t29 = S13 * S24
      t31 = S23 ** 2
      t39 = S34 ** 2
      t49 = S23 * S24
      t56 = S14 ** 2
      t62 = S24 * S14
      t69 = S23 * t56
      t71 = t31 * S23
      t77 = t49 * S14
      t79 = S24 ** 2
      t80 = S13 * t79
      t82 = t62 * S13
      t86 = (0.7D1 + (t10 - 0.55D2 / 0.9D1 * S13) * t3) * t39 + (0.7085D
     #4 / 0.9D1 * S13 + t23 + 0.6D1 * S24 - 0.4D1 * S14 + (-0.92D2 / 0.9
     #D1 * t29 + 0.16D2 * t25 + 0.2D1 * t31 + 0.116D3 / 0.9D1 * t27 - 0.
     #8D1 * t1 + 0.8D1 * t49) * t3) * S34 + 0.78D2 * t25 + t56 + 0.56D2 
     #* t49 + 0.78D2 * t31 - 0.28253D5 / 0.36D2 * t1 - 0.79D2 / 0.9D1 * 
     #t27 + 0.18518D5 / 0.9D1 * t29 - 0.2D1 * t62 + (-0.10D2 * t1 * S24 
     #- 0.2D1 * t31 * S14 - t56 * S13 + 0.8D1 * t69 + 0.8D1 * t71 + 0.25
     #9D3 / 0.36D2 * t1 * S13 + 0.5D1 / 0.3D1 * S14 * t1 + 0.8D1 * t77 -
     # 0.28D2 / 0.9D1 * t80 + 0.44D2 / 0.9D1 * t82) * t3
      t103 = S12 ** 2
      rrqg2qgh21J5 = (-0.32D2 / 0.3D1 * t1 * t3 * t5 * s * z + (0.2D1 + 
     #(t10 + S13 / 0.12D2) * t3) * S12 + (-0.2D1 * S23 + 0.136D3 / 0.9D1
     # * S13) * t3 * S34 + 0.2D1 * S24 + 0.25693D5 / 0.36D2 * S13 + t23 
     #+ (-t1 / 0.6D1 + 0.2D1 * t25 + 0.17D2 / 0.9D1 * t27 + 0.10D2 * t29
     # - 0.16D2 * t31) * t3 + t86 * t5 + (S23 * t39 + (-0.2D1 * t49 - 0.
     #16D2 / 0.9D1 * t27 - 0.4D1 * t25 - 0.8D1 / 0.9D1 * t29) * S34 + 0.
     #7D1 * t69 + 0.2D1 * S24 * t31 - 0.8D1 / 0.9D1 * t80 - 0.8D1 / 0.9D
     #1 * t82 + 0.2D1 * t71 + 0.6D1 * t77) / t103) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh21J6
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
      t1 = S12 + S13 + S23
      t2 = t1 ** 2
      t3 = 0.1D1 / t2
      t6 = 0.1D1 / t1
      t7 = S23 * t6
      t9 = S13 ** 2
      t13 = 0.1D1 / S12
      t16 = s ** 2
      t18 = z ** 2
      t20 = S13 * t6
      t25 = S14 * S23
      t44 = 0.78D2 * S23
      t47 = S13 * S14
      t50 = S13 * S24
      t58 = S34 ** 2
      t62 = S23 ** 2
      t64 = S23 * S24
      t73 = S24 ** 2
      t81 = S14 ** 2
      t82 = t81 * S13
      t86 = t73 * S23
      t96 = 0.16D2 / 0.9D1 * S24 * S14 * S13
      t101 = S13 * t73
      t120 = S12 ** 2
      rrqg2qgh21J6 = ((0.16D2 / 0.9D1 * S13 * t3 + (-0.48D2 * t7 - 0.16D
     #2 / 0.9D1 * t9 * t3) * t13) * t16 * t18 + (-0.32D2 / 0.9D1 * t20 +
     # (0.48D2 * t7 * S34 + 0.96D2 * S23 + (0.48D2 * t25 - 0.64D2 / 0.9D
     #1 * t9) * t6) * t13) * s * z + (0.2D1 + 0.25D2 / 0.18D2 * t20) * S
     #12 + (0.2D1 * S23 + 0.160D3 / 0.9D1 * S13) * t6 * S34 + 0.4285D4 /
     # 0.6D1 * S13 + t44 + 0.2D1 * S24 + (-0.2D1 * t25 + 0.6D1 * t47 - 0
     #.25D2 / 0.9D1 * t9 + 0.122D3 / 0.9D1 * t50) * t6 + ((0.8D1 * S23 +
     # 0.44D2 / 0.3D1 * S13) * t6 * t58 + (t44 + 0.14017D5 / 0.18D2 * S1
     #3 + (0.16D2 * t25 - 0.2D1 * t62 + 0.48D2 * t64 + 0.40D2 / 0.3D1 * 
     #t47 - 0.32D2 / 0.3D1 * t9 + 0.128D3 / 0.9D1 * t50) * t6) * S34 - 0
     #.2D1 * t73 + 0.78D2 * t25 + 0.120D3 * t64 - 0.73D2 / 0.6D1 * t47 +
     # 0.78D2 * t62 - 0.7132D4 / 0.9D1 * t9 + 0.18386D5 / 0.9D1 * t50 + 
     #(-0.4D1 / 0.3D1 * t82 - 0.122D3 / 0.9D1 * t9 * S24 + 0.40D2 * t86 
     #+ 0.17D2 / 0.2D1 * t9 * S13 - 0.22D2 / 0.9D1 * S14 * t9 + 0.8D1 * 
     #S23 * t81 - t96 + 0.48D2 * t64 * S14 + 0.2D1 * t62 * S14 + 0.50D2 
     #/ 0.9D1 * t101) * t6) * t13 + (-0.8D1 / 0.9D1 * S13 * t58 + (-0.16
     #D2 / 0.9D1 * t47 - 0.16D2 / 0.9D1 * t50) * S34 - 0.16D2 / 0.9D1 * 
     #t101 - 0.8D1 / 0.9D1 * t82 + 0.2D1 * t62 * S23 - t96 - 0.2D1 * t86
     # + 0.2D1 * S24 * t62) / t120) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh21J7
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
      t1 = S12 + S13 + S23
      t2 = t1 ** 2
      t3 = 0.1D1 / t2
      t5 = S13 ** 2
      t7 = 0.1D1 / S12
      t11 = s ** 2
      t13 = z ** 2
      t15 = 0.1D1 / t1
      t16 = S13 * t15
      t27 = S13 * S24
      t31 = S34 ** 2
      t36 = S13 * S14
      t49 = S24 ** 2
      t56 = S14 ** 2
      rrqg2qgh21J7 = ((-0.16D2 / 0.9D1 * S13 * t3 + 0.16D2 / 0.9D1 * t5 
     #* t3 * t7) * t11 * t13 + (0.32D2 / 0.9D1 * t16 - 0.128D3 / 0.9D1 *
     # t5 * t15 * t7) * s * z - t16 * S12 / 0.9D1 + 0.6305D4 / 0.9D1 * S
     #13 + (0.2D1 / 0.9D1 * t5 - 0.2D1 / 0.9D1 * t27) * t15 + (-0.16D2 /
     # 0.9D1 * t16 * t31 + (0.784D3 * S13 + (0.64D2 / 0.9D1 * t5 - 0.32D
     #2 / 0.9D1 * t36 - 0.32D2 / 0.9D1 * t27) * t15) * S34 - 0.76D2 / 0.
     #9D1 * t36 - 0.7093D4 / 0.9D1 * t5 + 0.18574D5 / 0.9D1 * t27 + (-0.
     #32D2 / 0.9D1 * S24 * S14 * S13 - 0.16D2 / 0.9D1 * S13 * t49 + 0.7D
     #1 * t5 * S13 + 0.2D1 / 0.9D1 * t5 * S24 - 0.16D2 / 0.9D1 * t56 * S
     #13 + 0.32D2 / 0.9D1 * S14 * t5) * t15) * t7) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J1
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
      t5 = 0.1D1 / (S12 + S14 + S24)
      t11 = 0.416D3 * S13
      t13 = S13 * S24
      t15 = S14 * S23
      t17 = S13 * S14
      t19 = S23 * S24
      t21 = S23 ** 2
      t23 = S13 ** 2
      t40 = t23 * S24
      t42 = S24 ** 2
      t45 = S14 ** 2
      t69 = S34 ** 2
      t76 = t23 ** 2
      t81 = S12 ** 2
      rrqg2qgh22J1 = ((-0.215D3 / 0.18D2 * S23 + 0.4D1 * S13) * t5 * S12
     # + 0.416D3 * S13 * t5 * S34 - t11 + 0.17D2 / 0.2D1 * S23 + (0.8D1 
     #* t13 - 0.265D3 / 0.18D2 * t15 + 0.4D1 * t17 - 0.82D2 / 0.9D1 * t1
     #9 - 0.146D3 / 0.9D1 * t21 + 0.412D3 * t23) * t5 + ((-0.17D2 / 0.36
     #D2 * S23 - t11 + (0.412D3 * t17 + 0.412D3 * t13 + 0.16D2 * t23) * 
     #t5) * S34 + 0.61D2 / 0.18D2 * t19 - 0.416D3 * t13 + 0.44D2 / 0.3D1
     # * t21 - 0.416D3 * t23 + 0.275D3 / 0.36D2 * t15 + (0.416D3 * t40 -
     # 0.5D1 / 0.18D2 * t42 * S23 - 0.16D2 / 0.3D1 * S23 * t45 + 0.412D3
     # * S14 * t23 - 0.82D2 / 0.9D1 * S24 * t21 - 0.91D2 / 0.18D2 * t19 
     #* S14 - 0.100D3 / 0.9D1 * t21 * S14 - 0.14D2 / 0.9D1 * t21 * S23 +
     # 0.4D1 * S24 * S14 * S13 + 0.4D1 * S13 * t42) * t5) / S12 + (-0.4D
     #1 * t23 * t5 * t69 - 0.8D1 * t40 * t5 * S34 + (-0.4D1 * t23 * t42 
     #- 0.4D1 * t76) * t5) / t81) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J2
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
      t5 = 0.1D1 / (S12 + S14 + S24)
      t14 = 0.10D2 * S13
      t15 = S13 * S24
      t17 = S13 * S14
      t19 = S23 * S24
      t21 = S13 ** 2
      t23 = S23 ** 2
      t25 = S14 * S23
      t33 = S34 ** 2
      t61 = S14 ** 2
      t64 = S24 ** 2
      t69 = S24 * t21
      t71 = t21 * S13
      t77 = S14 * t23
      t79 = -0.37D2 / 0.18D2 * t19 * S14 - 0.56D2 * S24 * S14 * S13 - 0.
     #2D1 * t23 * S24 - 0.20D2 / 0.9D1 * t23 * S23 - 0.9D1 / 0.2D1 * S23
     # * t61 + 0.13D2 / 0.9D1 * t64 * S23 + 0.30D2 * S13 * t64 - 0.16D2 
     #* t69 - 0.12D2 * t71 - 0.86D2 * t61 * S13 - 0.20D2 * t21 * S14 - 0
     #.28D2 / 0.9D1 * t77
      t94 = S12 ** 2
      rrqg2qgh22J2 = ((0.30D2 * S13 + 0.28D2 / 0.9D1 * S23) * t5 * S12 +
     # (-0.16D2 * S13 - 0.64D2 / 0.9D1 * S23) * t5 * S34 + 0.29D2 / 0.9D
     #1 * S23 - t14 + (0.60D2 * t15 - 0.56D2 * t17 + 0.23D2 / 0.3D1 * t1
     #9 - 0.24D2 * t21 - 0.10D2 / 0.9D1 * t23 - 0.43D2 / 0.18D2 * t25) *
     # t5 + ((-0.12D2 * S13 - 0.34D2 / 0.9D1 * S23) * t5 * t33 + (0.473D
     #3 / 0.36D2 * S23 - t14 + (-0.24D2 * t15 - 0.64D2 / 0.9D1 * t25 - 0
     #.20D2 * t17 - 0.64D2 / 0.9D1 * t19 - 0.8D1 * t21 - 0.6D1 * t23) * 
     #t5) * S34 - 0.10D2 * t21 + 0.119D3 / 0.36D2 * t25 + 0.172D3 * t17 
     #- 0.56D2 / 0.9D1 * t23 - t19 / 0.3D1 - 0.10D2 * t15 + t79 * t5) / 
     #S12 + ((0.8D1 * t71 + 0.4D1 * t69) * t5 * S34 - 0.4D1 / 0.9D1 * t7
     #7 + 0.12D2 * t71 * S24 * t5) / t94) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J3
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
      t5 = 0.1D1 / (S12 + S14 + S24)
      t14 = 0.10D2 * S13
      t15 = S13 * S14
      t17 = S13 * S24
      t19 = S23 ** 2
      t21 = S14 * S23
      t23 = S13 ** 2
      t25 = S23 * S24
      t33 = S34 ** 2
      t57 = S24 * t19
      t59 = t19 * S23
      t61 = S14 ** 2
      t64 = S24 ** 2
      t69 = t23 * S24
      t71 = t23 * S13
      t77 = t19 * S14
      t79 = 0.14527D5 / 0.18D2 * t25 * S14 - 0.56D2 * S24 * S14 * S13 - 
     #0.18598D5 / 0.9D1 * t57 - 0.28D2 / 0.9D1 * t59 + 0.14315D5 / 0.18D
     #2 * S23 * t61 + 0.89D2 / 0.9D1 * t64 * S23 + 0.30D2 * S13 * t64 - 
     #0.16D2 * t69 - 0.12D2 * t71 - 0.86D2 * t61 * S13 - 0.20D2 * S14 * 
     #t23 - 0.6200D4 / 0.3D1 * t77
      t103 = S12 ** 2
      rrqg2qgh22J3 = ((-0.7024D4 / 0.9D1 * S23 + 0.30D2 * S13) * t5 * S1
     #2 + (-0.6380D4 / 0.9D1 * S23 - 0.16D2 * S13) * t5 * S34 + 0.7085D4
     # / 0.9D1 * S23 - t14 + (-0.56D2 * t15 + 0.60D2 * t17 - 0.18562D5 /
     # 0.9D1 * t19 + 0.25D2 / 0.2D1 * t21 - 0.24D2 * t23 - 0.6911D4 / 0.
     #9D1 * t25) * t5 + ((-0.12D2 * S13 - 0.34D2 / 0.9D1 * S23) * t5 * t
     #33 + (0.25693D5 / 0.36D2 * S23 - t14 + (-0.24D2 * t17 - 0.6376D4 /
     # 0.9D1 * t21 - 0.20D2 * t15 - 0.6388D4 / 0.9D1 * t25 - 0.8D1 * t23
     # - 0.74D2 / 0.9D1 * t19) * t5) * S34 - 0.10D2 * t23 - 0.28253D5 / 
     #0.36D2 * t21 + 0.172D3 * t15 + 0.18518D5 / 0.9D1 * t19 - 0.79D2 / 
     #0.9D1 * t25 - 0.10D2 * t17 + t79 * t5) / S12 + ((0.4D1 / 0.9D1 * t
     #19 + (0.8D1 * t71 + 0.4D1 * t69) * t5) * S34 - 0.8D1 / 0.9D1 * t77
     # + 0.4D1 / 0.9D1 * t57 + 0.8D1 / 0.9D1 * t59 + (-0.4D1 / 0.9D1 * t
     #59 * S24 + 0.12D2 * t71 * S24 - 0.4D1 / 0.9D1 * t19 * t64) * t5) /
     # t103) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J4
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
      t5 = 0.1D1 / (S12 + S14 + S24)
      t14 = 0.10D2 * S13
      t15 = S13 * S14
      t17 = S13 * S24
      t19 = S23 ** 2
      t21 = S14 * S23
      t23 = S13 ** 2
      t25 = S23 * S24
      t33 = S34 ** 2
      t57 = S24 * t19
      t59 = t19 * S23
      t61 = S14 ** 2
      t64 = S24 ** 2
      t69 = t23 * S24
      t71 = t23 * S13
      t77 = t19 * S14
      t79 = 0.14527D5 / 0.18D2 * t25 * S14 - 0.56D2 * S24 * S14 * S13 - 
     #0.18598D5 / 0.9D1 * t57 - 0.28D2 / 0.9D1 * t59 + 0.14315D5 / 0.18D
     #2 * S23 * t61 + 0.89D2 / 0.9D1 * t64 * S23 + 0.30D2 * S13 * t64 - 
     #0.16D2 * t69 - 0.12D2 * t71 - 0.86D2 * t61 * S13 - 0.20D2 * S14 * 
     #t23 - 0.6200D4 / 0.3D1 * t77
      t103 = S12 ** 2
      rrqg2qgh22J4 = ((-0.7024D4 / 0.9D1 * S23 + 0.30D2 * S13) * t5 * S1
     #2 + (-0.6380D4 / 0.9D1 * S23 - 0.16D2 * S13) * t5 * S34 + 0.7085D4
     # / 0.9D1 * S23 - t14 + (-0.56D2 * t15 + 0.60D2 * t17 - 0.18562D5 /
     # 0.9D1 * t19 + 0.25D2 / 0.2D1 * t21 - 0.24D2 * t23 - 0.6911D4 / 0.
     #9D1 * t25) * t5 + ((-0.12D2 * S13 - 0.34D2 / 0.9D1 * S23) * t5 * t
     #33 + (0.25693D5 / 0.36D2 * S23 - t14 + (-0.24D2 * t17 - 0.6376D4 /
     # 0.9D1 * t21 - 0.20D2 * t15 - 0.6388D4 / 0.9D1 * t25 - 0.8D1 * t23
     # - 0.74D2 / 0.9D1 * t19) * t5) * S34 - 0.10D2 * t23 - 0.28253D5 / 
     #0.36D2 * t21 + 0.172D3 * t15 + 0.18518D5 / 0.9D1 * t19 - 0.79D2 / 
     #0.9D1 * t25 - 0.10D2 * t17 + t79 * t5) / S12 + ((0.4D1 / 0.9D1 * t
     #19 + (0.8D1 * t71 + 0.4D1 * t69) * t5) * S34 - 0.8D1 / 0.9D1 * t77
     # + 0.4D1 / 0.9D1 * t57 + 0.8D1 / 0.9D1 * t59 + (-0.4D1 / 0.9D1 * t
     #59 * S24 + 0.12D2 * t71 * S24 - 0.4D1 / 0.9D1 * t19 * t64) * t5) /
     # t103) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J5
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
      t5 = 0.1D1 / (S12 + S14 + S24)
      t14 = 0.10D2 * S13
      t15 = S13 * S14
      t17 = S13 * S24
      t19 = S23 ** 2
      t21 = S14 * S23
      t23 = S13 ** 2
      t25 = S23 * S24
      t33 = S34 ** 2
      t57 = S24 * t19
      t59 = t19 * S23
      t61 = S14 ** 2
      t64 = S24 ** 2
      t69 = t23 * S24
      t71 = t23 * S13
      t77 = t19 * S14
      t79 = 0.14527D5 / 0.18D2 * t25 * S14 - 0.56D2 * S24 * S14 * S13 - 
     #0.18598D5 / 0.9D1 * t57 - 0.28D2 / 0.9D1 * t59 + 0.14315D5 / 0.18D
     #2 * S23 * t61 + 0.89D2 / 0.9D1 * t64 * S23 + 0.30D2 * S13 * t64 - 
     #0.16D2 * t69 - 0.12D2 * t71 - 0.86D2 * t61 * S13 - 0.20D2 * S14 * 
     #t23 - 0.6200D4 / 0.3D1 * t77
      t103 = S12 ** 2
      rrqg2qgh22J5 = ((-0.7024D4 / 0.9D1 * S23 + 0.30D2 * S13) * t5 * S1
     #2 + (-0.6380D4 / 0.9D1 * S23 - 0.16D2 * S13) * t5 * S34 + 0.7085D4
     # / 0.9D1 * S23 - t14 + (-0.56D2 * t15 + 0.60D2 * t17 - 0.18562D5 /
     # 0.9D1 * t19 + 0.25D2 / 0.2D1 * t21 - 0.24D2 * t23 - 0.6911D4 / 0.
     #9D1 * t25) * t5 + ((-0.12D2 * S13 - 0.34D2 / 0.9D1 * S23) * t5 * t
     #33 + (0.25693D5 / 0.36D2 * S23 - t14 + (-0.24D2 * t17 - 0.6376D4 /
     # 0.9D1 * t21 - 0.20D2 * t15 - 0.6388D4 / 0.9D1 * t25 - 0.8D1 * t23
     # - 0.74D2 / 0.9D1 * t19) * t5) * S34 - 0.10D2 * t23 - 0.28253D5 / 
     #0.36D2 * t21 + 0.172D3 * t15 + 0.18518D5 / 0.9D1 * t19 - 0.79D2 / 
     #0.9D1 * t25 - 0.10D2 * t17 + t79 * t5) / S12 + ((0.4D1 / 0.9D1 * t
     #19 + (0.8D1 * t71 + 0.4D1 * t69) * t5) * S34 - 0.8D1 / 0.9D1 * t77
     # + 0.4D1 / 0.9D1 * t57 + 0.8D1 / 0.9D1 * t59 + (-0.4D1 / 0.9D1 * t
     #59 * S24 + 0.12D2 * t71 * S24 - 0.4D1 / 0.9D1 * t19 * t64) * t5) /
     # t103) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J6
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
      t5 = 0.1D1 / (S12 + S14 + S24)
      t13 = 0.406D3 * S13
      t15 = S23 * S24
      t17 = S23 ** 2
      t19 = S13 * S14
      t21 = S13 * S24
      t23 = S13 ** 2
      t25 = S14 * S23
      t33 = S34 ** 2
      t57 = S24 * t17
      t59 = t17 * S23
      t61 = S14 ** 2
      t64 = S24 ** 2
      t69 = t23 * S24
      t71 = t23 * S13
      t77 = t17 * S14
      t79 = 0.7309D4 / 0.9D1 * t15 * S14 - 0.60D2 * S24 * S14 * S13 - 0.
     #6172D4 / 0.3D1 * t57 - 0.14D2 / 0.9D1 * t59 + 0.14411D5 / 0.18D2 *
     # S23 * t61 + 0.61D2 / 0.6D1 * t64 * S23 + 0.26D2 * S13 * t64 - 0.4
     #32D3 * t69 - 0.12D2 * t71 - 0.86D2 * t61 * S13 - 0.432D3 * S14 * t
     #23 - 0.18500D5 / 0.9D1 * t77
      t103 = t23 ** 2
      t110 = S12 ** 2
      rrqg2qgh22J6 = ((-0.1537D4 / 0.2D1 * S23 + 0.26D2 * S13) * t5 * S1
     #2 + (-0.6380D4 / 0.9D1 * S23 - 0.432D3 * S13) * t5 * S34 + t13 + 0
     #.14017D5 / 0.18D2 * S23 + (-0.6829D4 / 0.9D1 * t15 - 0.18416D5 / 0
     #.9D1 * t17 - 0.60D2 * t19 + 0.52D2 * t21 - 0.436D3 * t23 + 0.245D3
     # / 0.9D1 * t25) * t5 + ((-0.12D2 * S13 - 0.34D2 / 0.9D1 * S23) * t
     #5 * t33 + (0.4285D4 / 0.6D1 * S23 + t13 + (-0.436D3 * t21 - 0.6376
     #D4 / 0.9D1 * t25 - 0.432D3 * t19 - 0.6388D4 / 0.9D1 * t15 - 0.24D2
     # * t23 - 0.74D2 / 0.9D1 * t17) * t5) * S34 + 0.406D3 * t23 - 0.713
     #2D4 / 0.9D1 * t25 + 0.172D3 * t19 + 0.18386D5 / 0.9D1 * t17 - 0.73
     #D2 / 0.6D1 * t15 + 0.406D3 * t21 + t79 * t5) / S12 + (0.4D1 * t23 
     #* t5 * t33 + (0.4D1 / 0.9D1 * t17 + (0.8D1 * t71 + 0.12D2 * t69) *
     # t5) * S34 - 0.8D1 / 0.9D1 * t77 + 0.4D1 / 0.9D1 * t57 + 0.8D1 / 0
     #.9D1 * t59 + (0.4D1 * t23 * t64 - 0.4D1 / 0.9D1 * t17 * t64 - 0.4D
     #1 / 0.9D1 * t59 * S24 + 0.4D1 * t103 + 0.12D2 * t71 * S24) * t5) /
     # t110) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh22J7
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
      t3 = S23 * t2
      t9 = S23 ** 2
      t11 = S14 * S23
      t13 = S23 * S24
      t28 = S24 ** 2
      t31 = t9 * S24
      t33 = S14 ** 2
      t38 = S14 * t9
      t40 = t9 * S23
      t41 = 0.8D1 / 0.9D1 * t40
      t57 = S12 ** 2
      rrqg2qgh22J7 = (-0.7052D4 / 0.9D1 * t3 * S12 - 0.6316D4 / 0.9D1 * 
     #t3 * S34 + 0.784D3 * S23 + (-0.6184D4 / 0.3D1 * t9 + 0.134D3 / 0.9
     #D1 * t11 - 0.6980D4 / 0.9D1 * t13) * t2 + ((0.6305D4 / 0.9D1 * S23
     # + (-0.20D2 / 0.9D1 * t9 - 0.2104D4 / 0.3D1 * t11 - 0.2108D4 / 0.3
     #D1 * t13) * t2) * S34 + 0.18574D5 / 0.9D1 * t9 - 0.7093D4 / 0.9D1 
     #* t11 - 0.76D2 / 0.9D1 * t13 + (0.76D2 / 0.9D1 * t28 * S23 - 0.185
     #80D5 / 0.9D1 * t31 + 0.7198D4 / 0.9D1 * S23 * t33 + 0.7282D4 / 0.9
     #D1 * t13 * S14 - 0.18572D5 / 0.9D1 * t38 - t41) * t2) / S12 + (0.4
     #D1 / 0.9D1 * t9 * S34 - 0.4D1 / 0.9D1 * t38 + t41 + 0.4D1 / 0.9D1 
     #* t31 + (-0.4D1 / 0.9D1 * t9 * t28 - 0.4D1 / 0.9D1 * t40 * S24) * 
     #t2) / t57) / pi * wd / z

      end function
  
 