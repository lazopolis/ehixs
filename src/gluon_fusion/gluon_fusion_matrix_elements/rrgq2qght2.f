  
      subroutine rrgq2qght2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh21J1  
      doubleprecision rrgq2qgh21J2  
      doubleprecision rrgq2qgh21J3  
      doubleprecision rrgq2qgh21J4  
      doubleprecision rrgq2qgh21J5  
      doubleprecision rrgq2qgh21J6  
      doubleprecision rrgq2qgh21J7  
      doubleprecision rrgq2qgh22J1  
      doubleprecision rrgq2qgh22J2  
      doubleprecision rrgq2qgh22J3  
      doubleprecision rrgq2qgh22J4  
      doubleprecision rrgq2qgh22J5  
      doubleprecision rrgq2qgh22J6  
      doubleprecision rrgq2qgh22J7  
      doubleprecision rrgq2qght2s1e1  
      doubleprecision rrgq2qght2s1e0  
      doubleprecision rrgq2qght2s1em1  
      doubleprecision rrgq2qght2s1em2  
      doubleprecision rrgq2qght2s1em3  
      doubleprecision rrgq2qght2s1em4  
      doubleprecision rrgq2qght2s2e1  
      doubleprecision rrgq2qght2s2e0  
      doubleprecision rrgq2qght2s2em1  
      doubleprecision rrgq2qght2s2em2  
      doubleprecision rrgq2qght2s2em3  
      doubleprecision rrgq2qght2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = x3 * t10 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t18 = -0.1D1 + x3
      t20 = t17 * t4 * t18
      t23 = log(0.4D1 * t14 * t20)
      t24 = t23 ** 2
      t25 = -t18
      t26 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4)
      t29 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4)
      t31 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4)
      t32 = t17 * t4
      t35 = log(-0.4D1 * t14 * t32)
      t36 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t38 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t39 = t35 ** 2
      t40 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t47 = 0.3141592653589793D1 * lh
      t48 = t1 * t7
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t60 = 0.3141592653589793D1 * t59
      t61 = t26 - t40
      t65 = 0.1D1 / x3
      t68 = t10 * t13
      t71 = log(-0.4D1 * t68 * t32)
      t72 = t71 * 0.3141592653589793D1
      t75 = t71 ** 2
      t76 = t75 * 0.3141592653589793D1
      t79 = (t60 + 0.180D3 * t72 * lh + 0.45D2 * t76) * t1
      t83 = rrgq2qgh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t100 = (0.3141592653589793D1 * (0.60D2 * lh * t57 - 0.288493656758
     #3026D3 - 0.120D3 * t55 * lh) - t72 * t59 - 0.90D2 * t76 * lh - 0.1
     #5D2 * t75 * t71 * 0.3141592653589793D1) * t1
      t107 = (-0.180D3 * t47 - 0.90D2 * t72) * t1
      t111 = x1 ** 2
      t112 = x3 * t111
      t113 = t112 * t68
      t116 = log(0.4D1 * t113 * t20)
      t119 = t13 * t16
      t120 = x4 * t4
      t124 = log(-0.4D1 * t112 * t10 * t119 * t120)
      t136 = 0.1D1 / x1
      t139 = t111 * t10
      t143 = log(-0.4D1 * t139 * t13 * t32)
      t144 = t143 ** 2
      t162 = (0.90D2 * t6 * t7 * (t24 * t26 / 0.2D1 - t23 * t29 + t31 + 
     #t35 * t36 - t38 - t39 * t40 / 0.2D1) - 0.180D3 * t47 * t48 * (t29 
     #- t23 * t26 - t36 + t35 * t40) + t60 * t48 * t61) * t65 / 0.1440D4
     # - t79 * t7 * t36 / 0.1440D4 - t6 * t7 * t83 / 0.16D2 - t100 * t7 
     #* t40 / 0.1440D4 - t107 * t7 * t38 / 0.1440D4 - (0.90D2 * t6 * t7 
     #* (t116 * t26 - t29 - t124 * t40 + t36) + 0.180D3 * t47 * t48 * t6
     #1) * t65 * t136 / 0.720D3 + (0.90D2 * t6 * t7 * (-t144 * t40 / 0.2
     #D1 + t143 * t36 - t38) - 0.180D3 * t47 * t48 * (t143 * t40 - t36) 
     #- t60 * t48 * t40) * t136 / 0.720D3
      t163 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t162)
      t165 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4
     #)
      t168 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4
     #)
      t169 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4
     #)
      t171 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t172 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t174 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t187 = t165 - t174
      t193 = rrgq2qgh22J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t237 = (0.90D2 * t6 * t7 * (t24 * t165 / 0.2D1 + t168 - t23 * t169
     # - t171 + t35 * t172 - t39 * t174 / 0.2D1) - 0.180D3 * t47 * t48 *
     # (t169 - t23 * t165 - t172 + t35 * t174) + t60 * t48 * t187) * t65
     # / 0.1440D4 - t6 * t7 * t193 / 0.16D2 - t100 * t7 * t174 / 0.1440D
     #4 - t107 * t7 * t171 / 0.1440D4 - t79 * t7 * t172 / 0.1440D4 - (0.
     #90D2 * t6 * t7 * (t116 * t165 + t172 - t124 * t174 - t169) + 0.180
     #D3 * t47 * t48 * t187) * t65 * t136 / 0.720D3 + (0.90D2 * t6 * t7 
     #* (t143 * t172 - t144 * t174 / 0.2D1 - t171) - 0.180D3 * t47 * t48
     # * (-t172 + t143 * t174) - t60 * t48 * t174) * t136 / 0.720D3
      t238 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t237)
      t240 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t241 = s * t240
      t242 = t1 * x1
      t243 = t241 * t242
      t244 = -0.1D1 + x1
      t245 = t1 * t244
      t246 = t245 * x4
      t247 = t241 * t246
      t248 = t245 * t4
      t249 = t241 * t248
      t250 = t240 ** 2
      t253 = x1 * t244
      t255 = s * t250 * t15 * t253 * t4
      t256 = t244 ** 2
      t258 = t250 ** 2
      t263 = log(-0.4D1 * t113 * t17 * t4 * t256 * t258)
      t264 = t263 * t244
      t266 = 0.1D1 / (-0.2D1 + t240)
      t267 = t250 * t266
      t268 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t269 = t267 * t268
      t271 = t244 * t250
      t272 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t274 = t271 * t266 * t272
      t279 = t47 * t48
      t281 = t271 * t266 * t268
      t292 = log(-0.4D1 * t139 * t119 * t120 * t256 * t258)
      t293 = t292 ** 2
      t294 = t293 * t244
      t297 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t300 = t292 * t244
      t312 = t60 * t48
      t317 = -(0.90D2 * t6 * t7 * (t264 * t269 - t274) + 0.180D3 * t279 
     #* t281) * t65 * t136 / 0.720D3 + (0.90D2 * t6 * t7 * (t294 * t269 
     #/ 0.2D1 + t271 * t266 * t297 - t300 * t267 * t272) - 0.180D3 * t47
     # * t48 * (t274 - t300 * t269) + t312 * t281) * t136 / 0.720D3
      t318 = FJET(XB1, XB2, s, t243, 0.0D0, -t247, t249, t255, t317)
      t320 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t322 = t271 * t266 * t320
      t323 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t324 = t267 * t323
      t331 = t271 * t266 * t323
      t339 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t357 = -(0.90D2 * t6 * t7 * (-t322 + t264 * t324) + 0.180D3 * t279
     # * t331) * t65 * t136 / 0.720D3 + (0.90D2 * t6 * t7 * (t294 * t324
     # / 0.2D1 + t271 * t266 * t339 - t300 * t267 * t320) - 0.180D3 * t4
     #7 * t48 * (t322 - t300 * t324) + t312 * t331) * t136 / 0.720D3
      t358 = FJET(XB1, XB2, s, -t247, t249, t243, 0.0D0, t255, t357)
      t360 = KAPPA2(x1, x2, t25, x4, z)
      t361 = s * t360
      t363 = t361 * t242 * t18
      t365 = t361 * t242 * x3
      t366 = t361 * t246
      t367 = t361 * t248
      t368 = t360 ** 2
      t373 = cos(t8)
      t376 = Sqrt(x3 * t18 * t120)
      t381 = s * t368 * t15 * t253 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t373 * t376)
      t385 = t368 ** 2
      t390 = log(0.4D1 * t112 * t68 * t16 * t120 * t256 * t18 * t385)
      t391 = t390 * t244
      t393 = 0.1D1 / (-0.2D1 + t360)
      t394 = t368 * t393
      t395 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t398 = t244 * t368
      t399 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t410 = 0.90D2 * t6 * t7 * (-t391 * t394 * t395 + t398 * t393 * t39
     #9) - 0.180D3 * t279 * t398 * t393 * t395
      t414 = FJET(XB1, XB2, s, -t363, t365, -t366, t367, t381, -t410 * t
     #65 * t136 / 0.720D3)
      t416 = t65 * t136
      t419 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t422 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t433 = 0.90D2 * t6 * t7 * (-t391 * t394 * t419 + t398 * t393 * t42
     #2) - 0.180D3 * t279 * t398 * t393 * t419
      t437 = FJET(XB1, XB2, s, -t366, t367, -t363, t365, t381, -t433 * t
     #65 * t136 / 0.720D3)
      rrgq2qght2s1e1 = t163 * t162 + t238 * t237 + t318 * t317 + t358 * 
     #t357 - t414 * t410 * t416 / 0.720D3 - t437 * t433 * t416 / 0.720D3

      end function



      doubleprecision function rrgq2qght2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = 0.1D1 - x3
      t9 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -t8
      t25 = log(0.4D1 * t16 * t19 * t4 * t20)
      t26 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t28 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t29 = t19 * t4
      t32 = log(-0.4D1 * t16 * t29)
      t33 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t39 = 0.3141592653589793D1 * lh
      t40 = t1 * t7
      t41 = t26 - t33
      t46 = 0.1D1 / x3
      t49 = t6 * t7
      t52 = 0.1D1 / x1
      t56 = x1 ** 2
      t57 = t56 * t12
      t61 = log(-0.4D1 * t57 * t15 * t29)
      t73 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t81 = log(-0.4D1 * t12 * t15 * t29)
      t82 = t81 * 0.3141592653589793D1
      t85 = (-0.180D3 * t39 - 0.90D2 * t82) * t1
      t89 = lh ** 2
      t91 = 0.3141592653589793D1 ** 2
      t97 = t81 ** 2
      t101 = (0.3141592653589793D1 * (0.180D3 * t89 - 0.30D2 * t91) + 0.
     #180D3 * t82 * lh + 0.45D2 * t97 * 0.3141592653589793D1) * t1
      t105 = (0.90D2 * t6 * t7 * (t9 - t25 * t26 - t28 + t32 * t33) - 0.
     #180D3 * t39 * t40 * t41) * t46 / 0.1440D4 + t49 * t41 * t46 * t52 
     #/ 0.8D1 + (0.90D2 * t6 * t7 * (t61 * t33 - t28) + 0.180D3 * t39 * 
     #t40 * t33) * t52 / 0.720D3 - t6 * t7 * t73 / 0.16D2 - t85 * t7 * t
     #28 / 0.1440D4 - t101 * t7 * t33 / 0.1440D4
      t106 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t105)
      t108 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t109 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t111 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t112 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t118 = t109 - t112
      t141 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t151 = (0.90D2 * t6 * t7 * (t108 - t25 * t109 - t111 + t32 * t112)
     # - 0.180D3 * t39 * t40 * t118) * t46 / 0.1440D4 + t49 * t118 * t46
     # * t52 / 0.8D1 + (0.90D2 * t6 * t7 * (-t111 + t61 * t112) + 0.180D
     #3 * t39 * t40 * t112) * t52 / 0.720D3 - t6 * t7 * t141 / 0.16D2 - 
     #t85 * t7 * t111 / 0.1440D4 - t101 * t7 * t112 / 0.1440D4
      t152 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t151)
      t154 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t155 = s * t154
      t156 = t1 * x1
      t157 = t155 * t156
      t158 = -0.1D1 + x1
      t159 = t1 * t158
      t160 = t159 * x4
      t161 = t155 * t160
      t162 = t159 * t4
      t163 = t155 * t162
      t164 = t154 ** 2
      t167 = x1 * t158
      t169 = s * t164 * t17 * t167 * t4
      t171 = t6 * t7 * t158
      t173 = 0.1D1 / (-0.2D1 + t154)
      t174 = t164 * t173
      t175 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t181 = t158 * t164
      t182 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t187 = x4 * t4
      t188 = t158 ** 2
      t189 = t164 ** 2
      t194 = log(-0.4D1 * t57 * t15 * t18 * t187 * t188 * t189)
      t195 = t194 * t158
      t202 = t39 * t40
      t210 = t171 * t174 * t175 * t46 * t52 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t181 * t173 * t182 - t195 * t174 * t175) - 0.180D3 * t202 * t18
     #1 * t173 * t175) * t52 / 0.720D3
      t211 = FJET(XB1, XB2, s, t157, 0.0D0, -t161, t163, t169, t210)
      t213 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t219 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t235 = t171 * t174 * t213 * t46 * t52 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t181 * t173 * t219 - t195 * t174 * t213) - 0.180D3 * t202 * t18
     #1 * t173 * t213) * t52 / 0.720D3
      t236 = FJET(XB1, XB2, s, -t161, t163, t157, 0.0D0, t169, t235)
      t238 = KAPPA2(x1, x2, t8, x4, z)
      t239 = s * t238
      t241 = t239 * t156 * t20
      t243 = t239 * t156 * x3
      t244 = t239 * t160
      t245 = t239 * t162
      t246 = t238 ** 2
      t251 = cos(t10)
      t254 = Sqrt(x3 * t20 * t187)
      t259 = s * t246 * t17 * t167 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t251 * t254)
      t262 = t246 / (-0.2D1 + t238)
      t263 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t266 = t262 * t263 * t46 * t52
      t269 = FJET(XB1, XB2, s, -t241, t243, -t244, t245, t259, -t171 * t
     #266 / 0.8D1)
      t271 = t40 * t158
      t275 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t278 = t262 * t275 * t46 * t52
      t281 = FJET(XB1, XB2, s, -t244, t245, -t241, t243, t259, -t171 * t
     #278 / 0.8D1)
      rrgq2qght2s1e0 = t106 * t105 + t152 * t151 + t211 * t210 + t236 * 
     #t235 - t269 * 0.3141592653589793D1 * t271 * t266 / 0.8D1 - t281 * 
     #0.3141592653589793D1 * t271 * t278 / 0.8D1

      end function



      doubleprecision function rrgq2qght2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, x
     #4)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t36 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.314
     #1592653589793D1) * t1
      t39 = 0.1D1 - x3
      t40 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t39, x4)
      t43 = 0.1D1 / x3
      t47 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.16D2 - t36 * t9 /
     # 0.1440D4 + t6 * t7 * (t40 - t8) * t43 / 0.16D2
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t47)
      t50 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t51 = t7 * t50
      t55 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t61 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t39, x4)
      t67 = -t6 * t51 * t10 / 0.8D1 - t6 * t7 * t55 / 0.16D2 - t36 * t51
     # / 0.1440D4 + t6 * t7 * (t61 - t50) * t43 / 0.16D2
      t68 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t67)
      t70 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t71 = s * t70
      t73 = t71 * t1 * x1
      t74 = -0.1D1 + x1
      t75 = t1 * t74
      t77 = t71 * t75 * x4
      t79 = t71 * t75 * t4
      t80 = t70 ** 2
      t85 = s * t80 * t26 * x1 * t74 * t4
      t87 = t6 * t7 * t74
      t89 = 0.1D1 / (-0.2D1 + t70)
      t90 = t80 * t89
      t91 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4)
      t96 = FJET(XB1, XB2, s, t73, 0.0D0, -t77, t79, t85, t87 * t90 * t9
     #1 * t10 / 0.8D1)
      t98 = t1 * t7
      t100 = t74 * t80
      t106 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t111 = FJET(XB1, XB2, s, -t77, t79, t73, 0.0D0, t85, t87 * t90 * t
     #106 * t10 / 0.8D1)
      rrgq2qght2s1em1 = t47 * t48 + t68 * t67 + t96 * 0.3141592653589793
     #D1 * t98 * t100 * t89 * t91 * t10 / 0.8D1 + t111 * 0.3141592653589
     #793D1 * t98 * t100 * t89 * t106 * t10 / 0.8D1

      end function



      doubleprecision function rrgq2qght2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, x
     #4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrgq2qght2s1em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0.16D2 
     #- t21 * 0.3141592653589793D1 * t14 * t17 / 0.16D2

      end function



      doubleprecision function rrgq2qght2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      rrgq2qght2s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      rrgq2qght2s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght2s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = x3 * t10 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t18 = -0.1D1 + x3
      t20 = t17 * t4 * t18
      t23 = log(0.4D1 * t14 * t20)
      t24 = t23 ** 2
      t25 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t28 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t30 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t31 = t17 * t4
      t34 = log(-0.4D1 * t14 * t31)
      t35 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t37 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t38 = t34 ** 2
      t39 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t46 = 0.3141592653589793D1 * lh
      t47 = t1 * t7
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t58 = 0.180D3 * t54 - 0.30D2 * t56
      t59 = 0.3141592653589793D1 * t58
      t61 = t47 * (t25 - t39)
      t64 = 0.1D1 / x3
      t67 = t10 * t13
      t70 = log(-0.4D1 * t67 * t31)
      t71 = t70 * 0.3141592653589793D1
      t74 = t70 ** 2
      t75 = t74 * 0.3141592653589793D1
      t78 = (t59 + 0.180D3 * t71 * lh + 0.45D2 * t75) * t1
      t82 = rrgq2qgh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t99 = (0.3141592653589793D1 * (0.60D2 * lh * t56 - 0.2884936567583
     #026D3 - 0.120D3 * t54 * lh) - t71 * t58 - 0.90D2 * t75 * lh - 0.15
     #D2 * t74 * t70 * 0.3141592653589793D1) * t1
      t106 = (-0.180D3 * t46 - 0.90D2 * t71) * t1
      t110 = x1 ** 2
      t111 = x3 * t110
      t113 = t13 * t16
      t114 = x4 * t4
      t118 = log(-0.4D1 * t111 * t10 * t113 * t114)
      t120 = t111 * t67
      t123 = log(0.4D1 * t120 * t20)
      t133 = 0.1D1 / x1
      t136 = t110 * t10
      t140 = log(-0.4D1 * t136 * t13 * t31)
      t142 = t140 ** 2
      t159 = (0.90D2 * t6 * t7 * (t24 * t25 / 0.2D1 - t23 * t28 + t30 + 
     #t34 * t35 - t37 - t38 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (t28 
     #- t23 * t25 - t35 + t34 * t39) + t59 * t61) * t64 / 0.1440D4 - t78
     # * t7 * t35 / 0.1440D4 - t6 * t7 * t82 / 0.16D2 - t99 * t7 * t39 /
     # 0.1440D4 - t106 * t7 * t37 / 0.1440D4 + (0.90D2 * t6 * t7 * (-t35
     # + t118 * t39 + t28 - t123 * t25) - 0.180D3 * t46 * t61) * t64 * t
     #133 / 0.720D3 + (0.90D2 * t6 * t7 * (t140 * t35 - t37 - t142 * t39
     # / 0.2D1) - 0.180D3 * t46 * t47 * (-t35 + t140 * t39) - t59 * t47 
     #* t39) * t133 / 0.720D3
      t160 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t159)
      t162 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t163 = s * t162
      t164 = t1 * x1
      t165 = t163 * t164
      t166 = -0.1D1 + x1
      t167 = t1 * t166
      t168 = t167 * x4
      t169 = t163 * t168
      t170 = t167 * t4
      t171 = t163 * t170
      t172 = t162 ** 2
      t175 = x1 * t166
      t177 = s * t172 * t15 * t175 * x4
      t178 = t166 * t172
      t180 = 0.1D1 / (-0.2D1 + t162)
      t181 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t183 = t178 * t180 * t181
      t184 = t166 ** 2
      t186 = t172 ** 2
      t191 = log(-0.4D1 * t120 * t17 * t4 * t184 * t186)
      t192 = t191 * t166
      t193 = t172 * t180
      t194 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t195 = t193 * t194
      t201 = t46 * t47
      t203 = t178 * t180 * t194
      t209 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t217 = log(-0.4D1 * t136 * t113 * t114 * t184 * t186)
      t218 = t217 ** 2
      t219 = t218 * t166
      t222 = t217 * t166
      t234 = t59 * t47
      t239 = (0.90D2 * t6 * t7 * (t183 - t192 * t195) - 0.180D3 * t201 *
     # t203) * t64 * t133 / 0.720D3 + (0.90D2 * t6 * t7 * (t178 * t180 *
     # t209 + t219 * t195 / 0.2D1 - t222 * t193 * t181) - 0.180D3 * t46 
     #* t47 * (-t222 * t195 + t183) + t234 * t203) * t133 / 0.720D3
      t240 = FJET(XB1, XB2, s, 0.0D0, t165, -t169, t171, -t177, t239)
      t242 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t245 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t246 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t248 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t249 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t251 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t265 = t47 * (t242 - t251)
      t270 = rrgq2qgh22J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t312 = (0.90D2 * t6 * t7 * (t24 * t242 / 0.2D1 + t245 - t23 * t246
     # - t248 + t34 * t249 - t38 * t251 / 0.2D1) - 0.180D3 * t46 * t47 *
     # (t246 - t23 * t242 - t249 + t34 * t251) + t59 * t265) * t64 / 0.1
     #440D4 - t6 * t7 * t270 / 0.16D2 - t99 * t7 * t251 / 0.1440D4 - t10
     #6 * t7 * t248 / 0.1440D4 - t78 * t7 * t249 / 0.1440D4 + (0.90D2 * 
     #t6 * t7 * (t246 - t249 + t118 * t251 - t123 * t242) - 0.180D3 * t4
     #6 * t265) * t64 * t133 / 0.720D3 + (0.90D2 * t6 * t7 * (-t248 - t1
     #42 * t251 / 0.2D1 + t140 * t249) - 0.180D3 * t46 * t47 * (t140 * t
     #251 - t249) - t59 * t47 * t251) * t133 / 0.720D3
      t313 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t312)
      t315 = KAPPA2(x1, x2, x3, x4, z)
      t316 = s * t315
      t318 = t316 * t164 * x3
      t320 = t316 * t164 * t18
      t321 = t316 * t168
      t322 = t316 * t170
      t323 = t315 ** 2
      t328 = cos(t8)
      t331 = Sqrt(x3 * t18 * t114)
      t336 = s * t323 * t15 * t175 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t328 * t331)
      t340 = t323 ** 2
      t345 = log(0.4D1 * t111 * t67 * t16 * t114 * t184 * t18 * t340)
      t346 = t345 * t166
      t348 = 0.1D1 / (-0.2D1 + t315)
      t349 = t323 * t348
      t350 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t353 = t166 * t323
      t354 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t365 = 0.90D2 * t6 * t7 * (t346 * t349 * t350 - t353 * t348 * t354
     #) + 0.180D3 * t201 * t353 * t348 * t350
      t369 = FJET(XB1, XB2, s, t318, -t320, -t321, t322, t336, t365 * t6
     #4 * t133 / 0.720D3)
      t371 = t64 * t133
      t374 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t375 = t193 * t374
      t377 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t379 = t178 * t180 * t377
      t385 = t178 * t180 * t374
      t391 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t411 = (0.90D2 * t6 * t7 * (-t192 * t375 + t379) - 0.180D3 * t201 
     #* t385) * t64 * t133 / 0.720D3 + (0.90D2 * t6 * t7 * (t178 * t180 
     #* t391 - t222 * t193 * t377 + t219 * t375 / 0.2D1) - 0.180D3 * t46
     # * t47 * (t379 - t222 * t375) + t234 * t385) * t133 / 0.720D3
      t412 = FJET(XB1, XB2, s, -t169, t171, 0.0D0, t165, -t177, t411)
      t414 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t417 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t428 = 0.90D2 * t6 * t7 * (-t353 * t348 * t414 + t346 * t349 * t41
     #7) + 0.180D3 * t201 * t353 * t348 * t417
      t432 = FJET(XB1, XB2, s, -t321, t322, t318, -t320, t336, t428 * t6
     #4 * t133 / 0.720D3)
      rrgq2qght2s2e1 = t160 * t159 + t240 * t239 + t313 * t312 + t369 * 
     #t365 * t371 / 0.720D3 + t412 * t411 + t432 * t428 * t371 / 0.720D3

      end function



      doubleprecision function rrgq2qght2s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = x3 * t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x3
      t24 = log(0.4D1 * t15 * t18 * t4 * t19)
      t25 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t27 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t28 = t18 * t4
      t31 = log(-0.4D1 * t15 * t28)
      t32 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t1 * t7
      t40 = t25 - t32
      t45 = 0.1D1 / x3
      t48 = t6 * t7
      t50 = 0.1D1 / x1
      t54 = x1 ** 2
      t55 = t54 * t11
      t59 = log(-0.4D1 * t55 * t14 * t28)
      t71 = rrgq2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t79 = log(-0.4D1 * t11 * t14 * t28)
      t80 = t79 * 0.3141592653589793D1
      t83 = (-0.180D3 * t38 - 0.90D2 * t80) * t1
      t87 = lh ** 2
      t89 = 0.3141592653589793D1 ** 2
      t95 = t79 ** 2
      t99 = (0.3141592653589793D1 * (0.180D3 * t87 - 0.30D2 * t89) + 0.1
     #80D3 * t80 * lh + 0.45D2 * t95 * 0.3141592653589793D1) * t1
      t103 = (0.90D2 * t6 * t7 * (t8 - t24 * t25 - t27 + t31 * t32) - 0.
     #180D3 * t38 * t39 * t40) * t45 / 0.1440D4 + t48 * t40 * t45 * t50 
     #/ 0.8D1 + (0.90D2 * t6 * t7 * (-t27 + t59 * t32) + 0.180D3 * t38 *
     # t39 * t32) * t50 / 0.720D3 - t6 * t7 * t71 / 0.16D2 - t83 * t7 * 
     #t27 / 0.1440D4 - t99 * t7 * t32 / 0.1440D4
      t104 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t103)
      t106 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t107 = s * t106
      t108 = t1 * x1
      t109 = t107 * t108
      t110 = -0.1D1 + x1
      t111 = t1 * t110
      t112 = t111 * x4
      t113 = t107 * t112
      t114 = t111 * t4
      t115 = t107 * t114
      t116 = t106 ** 2
      t119 = x1 * t110
      t121 = s * t116 * t16 * t119 * x4
      t123 = t6 * t7 * t110
      t125 = 0.1D1 / (-0.2D1 + t106)
      t126 = t116 * t125
      t127 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t135 = x4 * t4
      t136 = t110 ** 2
      t137 = t116 ** 2
      t142 = log(-0.4D1 * t55 * t14 * t17 * t135 * t136 * t137)
      t143 = t142 * t110
      t146 = t110 * t116
      t147 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t154 = t38 * t39
      t162 = t123 * t126 * t127 * t45 * t50 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (-t143 * t126 * t127 + t146 * t125 * t147) - 0.180D3 * t154 * t1
     #46 * t125 * t127) * t50 / 0.720D3
      t163 = FJET(XB1, XB2, s, 0.0D0, t109, -t113, t115, -t121, t162)
      t165 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t166 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t168 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t169 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t175 = t166 - t169
      t197 = rrgq2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t207 = (0.90D2 * t6 * t7 * (t165 - t24 * t166 - t168 + t31 * t169)
     # - 0.180D3 * t38 * t39 * t175) * t45 / 0.1440D4 + t48 * t175 * t45
     # * t50 / 0.8D1 + (0.90D2 * t6 * t7 * (t59 * t169 - t168) + 0.180D3
     # * t38 * t39 * t169) * t50 / 0.720D3 - t6 * t7 * t197 / 0.16D2 - t
     #83 * t7 * t168 / 0.1440D4 - t99 * t7 * t169 / 0.1440D4
      t208 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = KAPPA2(x1, x2, x3, x4, z)
      t211 = s * t210
      t213 = t211 * t108 * x3
      t215 = t211 * t108 * t19
      t216 = t211 * t112
      t217 = t211 * t114
      t218 = t210 ** 2
      t223 = cos(t9)
      t226 = Sqrt(x3 * t19 * t135)
      t231 = s * t218 * t16 * t119 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t223 * t226)
      t234 = t218 / (-0.2D1 + t210)
      t235 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t238 = t234 * t235 * t45 * t50
      t241 = FJET(XB1, XB2, s, t213, -t215, -t216, t217, t231, -t123 * t
     #238 / 0.8D1)
      t243 = t39 * t110
      t247 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t253 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t269 = t123 * t126 * t247 * t45 * t50 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t146 * t125 * t253 - t143 * t126 * t247) - 0.180D3 * t154 * t14
     #6 * t125 * t247) * t50 / 0.720D3
      t270 = FJET(XB1, XB2, s, -t113, t115, 0.0D0, t109, -t121, t269)
      t272 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t275 = t234 * t272 * t45 * t50
      t278 = FJET(XB1, XB2, s, -t216, t217, t213, -t215, t231, -t123 * t
     #275 / 0.8D1)
      rrgq2qght2s2e0 = t104 * t103 + t163 * t162 + t208 * t207 - t241 * 
     #0.3141592653589793D1 * t243 * t238 / 0.8D1 + t270 * t269 - t278 * 
     #0.3141592653589793D1 * t243 * t275 / 0.8D1

      end function



      doubleprecision function rrgq2qght2s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x4
     #)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrgq2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t36 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.314
     #1592653589793D1) * t1
      t39 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t42 = 0.1D1 / x3
      t46 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.16D2 - t36 * t9 /
     # 0.1440D4 + t6 * t7 * (t39 - t8) * t42 / 0.16D2
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t46)
      t49 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t50 = s * t49
      t52 = t50 * t1 * x1
      t53 = -0.1D1 + x1
      t54 = t1 * t53
      t56 = t50 * t54 * x4
      t58 = t50 * t54 * t4
      t59 = t49 ** 2
      t64 = s * t59 * t26 * x1 * t53 * x4
      t66 = t6 * t7 * t53
      t68 = 0.1D1 / (-0.2D1 + t49)
      t69 = t59 * t68
      t70 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t75 = FJET(XB1, XB2, s, 0.0D0, t52, -t56, t58, -t64, t66 * t69 * t
     #70 * t10 / 0.8D1)
      t77 = t1 * t7
      t79 = t53 * t59
      t85 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t86 = t7 * t85
      t90 = rrgq2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t96 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t102 = -t6 * t86 * t10 / 0.8D1 - t6 * t7 * t90 / 0.16D2 - t36 * t8
     #6 / 0.1440D4 + t6 * t7 * (t96 - t85) * t42 / 0.16D2
      t103 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t110 = FJET(XB1, XB2, s, -t56, t58, 0.0D0, t52, -t64, t66 * t69 * 
     #t105 * t10 / 0.8D1)
      rrgq2qght2s2em1 = t46 * t47 + t75 * 0.3141592653589793D1 * t77 * t
     #79 * t68 * t70 * t10 / 0.8D1 + t103 * t102 + t110 * 0.314159265358
     #9793D1 * t77 * t79 * t68 * t105 * t10 / 0.8D1

      end function



      doubleprecision function rrgq2qght2s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x4
     #)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrgq2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrgq2qght2s2em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0.16D2 
     #- t21 * 0.3141592653589793D1 * t14 * t17 / 0.16D2

      end function



      doubleprecision function rrgq2qght2s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      rrgq2qght2s2em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght2s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh21J1
      doubleprecision rrgq2qgh21J2
      doubleprecision rrgq2qgh21J3
      doubleprecision rrgq2qgh21J4
      doubleprecision rrgq2qgh21J5
      doubleprecision rrgq2qgh21J6
      doubleprecision rrgq2qgh21J7
      doubleprecision rrgq2qgh22J1
      doubleprecision rrgq2qgh22J2
      doubleprecision rrgq2qgh22J3
      doubleprecision rrgq2qgh22J4
      doubleprecision rrgq2qgh22J5
      doubleprecision rrgq2qgh22J6
      doubleprecision rrgq2qgh22J7
      rrgq2qght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh21J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t2 * t3
      t5 = 0.1D1 - z
      t6 = t5 * x1
      t8 = s * t3
      t11 = 0.1D1 - x3
      t14 = s - t8 * t6 * x3 - t8 * t6 * t11
      t15 = t14 ** 2
      t16 = x3 * t15
      t17 = 0.1D1 - x1
      t18 = t5 * t17
      t21 = 0.1D1 - x4
      t24 = s - t8 * t18 * x4 - t8 * t18 * t21
      t25 = t24 * z
      t29 = t1 ** 2
      t30 = t3 ** 2
      t31 = t29 * t30
      t32 = t5 ** 2
      t33 = t32 * x1
      t36 = t14 * t24
      t42 = x1 ** 2
      t43 = t32 * t42
      t45 = t11 ** 2
      t46 = t45 * t14
      t50 = t29 * t3
      t51 = t50 * t6
      t52 = t11 * t14
      t56 = t30 * t3
      t57 = t29 * t56
      t58 = t32 * t5
      t59 = t58 * t42
      t65 = cos(x2 * 0.3141592653589793D1)
      t67 = x4 * t21
      t69 = Sqrt(x3 * t11 * t67)
      t72 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t65 * t69
      t78 = t30 ** 2
      t79 = t78 * t3
      t81 = t32 ** 2
      t83 = t42 * x1
      t84 = t81 * t5 * t83
      t85 = t2 * t79 * t84
      t86 = t17 ** 2
      t87 = t24 * t86
      t88 = t72 ** 2
      t89 = t87 * t88
      t93 = t2 * t78
      t94 = t81 * t42
      t95 = t94 * x3
      t96 = t93 * t95
      t97 = t15 * t24
      t99 = t86 * x4 * t72
      t100 = t97 * t99
      t103 = t86 * t21
      t104 = t103 * t72
      t108 = t57 * t59
      t109 = x3 * t14
      t110 = t24 * t17
      t111 = t110 * t72
      t115 = t29 * t78
      t117 = t115 * t81 * t83
      t118 = x3 ** 2
      t119 = t118 * t14
      t123 = t2 * t56
      t124 = t58 * x1
      t125 = t123 * t124
      t126 = t11 * t15
      t127 = t21 ** 2
      t128 = t87 * t127
      t132 = x4 ** 2
      t133 = t87 * t132
      t137 = t2 * t15
      t147 = -0.3456D4 * t4 * t6 * t16 * t25 + 0.1728D4 * t31 * t33 * x3
     # * t36 * z * t17 * t21 + 0.128D3 * t31 * t43 * t46 * t25 + 0.128D3
     # * t51 * t52 * t25 - 0.1728D4 * t57 * t59 * x3 * t36 * t17 * t72 *
     # z + 0.36D2 * t85 * t16 * t89 + 0.72D2 * t96 * t100 + 0.144D3 * t9
     #6 * t97 * t104 - 0.144D3 * t108 * t109 * t111 - 0.144D3 * t117 * t
     #119 * t111 + 0.32D2 * t125 * t126 * t128 + 0.32D2 * t125 * t126 * 
     #t133 + 0.72D2 * t137 * t24 * t30 * t32 * t86 * t67 + 0.32D2 * t85 
     #* t126 * t89
      t148 = t94 * t11
      t153 = t137 * t24 * t56
      t154 = t58 * t86
      t171 = z ** 2
      t176 = t137 * t24
      t177 = t30 * t32
      t187 = t29 * s * t171
      t198 = t31 * t32
      t203 = t50 * t5
      t208 = t57 * t58
      t214 = x1 * t11
      t222 = -0.32D2 * t93 * t148 * t100 + 0.216D3 * t153 * t154 * x4 * 
     #x1 * t72 - 0.144D3 * t153 * t154 * t21 * x1 * t72 - 0.252D3 * t137
     # * t24 * t78 * t94 * t86 * t88 + 0.1728D4 * t51 * t109 * t24 * t17
     #1 - 0.72D2 * t176 * t177 * t86 * t132 - 0.36D2 * t176 * t177 * t86
     # * t127 - 0.64D2 * t187 * t3 * t6 * t11 * t24 - 0.64D2 * t187 * t3
     #0 * t43 * t45 * t24 + 0.576D3 * t198 * t42 * t118 * t36 + 0.288D3 
     #* t203 * x1 * x3 * t36 + 0.288D3 * t208 * t83 * t118 * x3 * t36 - 
     #0.47D2 * t203 * t214 * t36 - 0.17D2 * t4 * t5 * t214 * t97
      t224 = t2 * t30
      t226 = t42 * t45
      t238 = t224 * t33
      t239 = t110 * x4
      t243 = t124 * x3
      t245 = t103 * x4
      t246 = t36 * t245
      t258 = t57 * t124
      t262 = t110 * t21
      t266 = t31 * t33
      t271 = t36 * t99
      t274 = t124 * t11
      t281 = -0.275D3 * t224 * t32 * t226 * t97 - 0.94D2 * t198 * t226 *
     # t36 - 0.47D2 * t208 * t83 * t45 * t11 * t36 + 0.2304D4 * t238 * t
     #16 * t239 - 0.1440D4 * t57 * t243 * t246 + 0.252D3 * t125 * t16 * 
     #t128 + 0.72D2 * t125 * t16 * t133 - 0.528D3 * t238 * t126 * t239 +
     # 0.12D2 * t258 * t52 * t128 + 0.148D3 * t108 * t46 * t262 + 0.128D
     #3 * t266 * t52 * t239 + 0.1440D4 * t115 * t95 * t271 + 0.240D3 * t
     #57 * t274 * t246 - 0.1440D4 * t258 * t109 * t133
      t289 = t97 * t245
      t307 = t115 * t148
      t328 = -0.144D3 * t108 * t119 * t262 - 0.144D3 * t266 * t109 * t26
     #2 + 0.216D3 * t123 * t243 * t289 - 0.748D3 * t29 * t79 * t84 * t52
     # * t89 + 0.306D3 * t123 * t59 * t126 * t111 - 0.96D2 * t117 * t46 
     #* t111 - 0.96D2 * t108 * t52 * t111 + 0.880D3 * t307 * t271 + 0.16
     #D2 * t307 * t36 * t104 - 0.122D3 * t238 * t126 * t262 + 0.32D2 * t
     #123 * t274 * t289 + 0.148D3 * t266 * t52 * t262 - 0.312D3 * t258 *
     # t52 * t133 + 0.128D3 * t108 * t46 * t239
      rrgq2qgh21J1 = -wd * (t147 + t222 + t281 + t328) / t1 / t15 / t24 
     #/ z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh21J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t9 * t10
      t12 = t6 * t11
      t13 = x3 ** 2
      t14 = s * t3
      t15 = t7 * x1
      t18 = 0.1D1 - x3
      t21 = s - t14 * t15 * x3 - t14 * t15 * t18
      t22 = t13 * t21
      t23 = 0.1D1 - x1
      t24 = t7 * t23
      t27 = 0.1D1 - x4
      t30 = s - t14 * t24 * x4 - t14 * t24 * t27
      t31 = t30 * t23
      t32 = t31 * t27
      t34 = t12 * t22 * t32
      t36 = t2 * t4
      t37 = t8 * x1
      t38 = t36 * t37
      t39 = x3 * t21
      t41 = t38 * t39 * t32
      t43 = s * t1
      t44 = t43 * t5
      t45 = t9 * x1
      t46 = t45 * x3
      t48 = t21 ** 2
      t49 = t48 * t30
      t50 = t23 ** 2
      t51 = t50 * t27
      t52 = t51 * x4
      t53 = t49 * t52
      t56 = t43 * t4
      t57 = t56 * t37
      t58 = x3 * t48
      t59 = t31 * x4
      t61 = t57 * t58 * t59
      t64 = t21 * t30
      t65 = t64 * t52
      t66 = t6 * t46 * t65
      t68 = t44 * t45
      t69 = t30 * t50
      t70 = t27 ** 2
      t71 = t69 * t70
      t75 = x4 ** 2
      t76 = t69 * t75
      t79 = 0.72D2 * t68 * t58 * t76
      t80 = t18 * t48
      t82 = t57 * t80 * t32
      t84 = t45 * t18
      t86 = t44 * t84 * t53
      t88 = t18 * t21
      t90 = t38 * t88 * t32
      t92 = t6 * t45
      t94 = t92 * t88 * t76
      t96 = t18 ** 2
      t97 = t96 * t21
      t99 = t12 * t97 * t59
      t102 = t57 * t80 * t59
      t105 = t92 * t88 * t71
      t107 = -0.144D3 * t34 - 0.144D3 * t41 + 0.216D3 * t44 * t46 * t53 
     #+ 0.2304D4 * t61 - 0.1440D4 * t66 + 0.252D3 * t68 * t58 * t71 + t7
     #9 - 0.122D3 * t82 + 0.32D2 * t86 + 0.148D3 * t90 - 0.312D3 * t94 +
     # 0.128D3 * t99 - 0.528D3 * t102 + 0.12D2 * t105
      t109 = t12 * t97 * t32
      t112 = t38 * t88 * t59
      t116 = 0.32D2 * t68 * t80 * t71
      t118 = t68 * t80 * t76
      t120 = t43 * t48
      t124 = t27 * x4
      t128 = t4 ** 2
      t129 = t128 * t3
      t131 = t8 ** 2
      t133 = t10 * x1
      t134 = t131 * t7 * t133
      t135 = t43 * t129 * t134
      t139 = cos(x2 * 0.3141592653589793D1)
      t142 = Sqrt(x3 * t18 * t124)
      t145 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t139 * t142
      t146 = t145 ** 2
      t147 = t69 * t146
      t150 = 0.32D2 * t135 * t80 * t147
      t151 = t2 * t128
      t152 = t131 * t10
      t153 = t152 * t18
      t154 = t151 * t153
      t156 = t50 * x4 * t145
      t157 = t64 * t156
      t158 = t154 * t157
      t160 = t51 * t145
      t161 = t64 * t160
      t162 = t154 * t161
      t167 = t43 * t128
      t168 = t152 * x3
      t169 = t167 * t168
      t170 = t49 * t156
      t173 = t49 * t160
      t176 = t31 * t145
      t178 = t12 * t39 * t176
      t181 = t151 * t131 * t133
      t183 = t181 * t22 * t176
      t185 = t151 * t168
      t186 = t185 * t157
      t188 = 0.148D3 * t109 + 0.128D3 * t112 + t116 + 0.32D2 * t118 + 0.
     #72D2 * t120 * t30 * t4 * t8 * t50 * t124 + t150 + 0.880D3 * t158 +
     # 0.16D2 * t162 + 0.36D2 * t135 * t58 * t147 + 0.72D2 * t169 * t170
     # + 0.144D3 * t169 * t173 - 0.144D3 * t178 - 0.144D3 * t183 + 0.144
     #0D4 * t186
      t191 = t2 * t129 * t134
      t193 = t191 * t88 * t147
      t195 = t44 * t11
      t197 = t195 * t80 * t176
      t200 = t181 * t97 * t176
      t203 = t12 * t88 * t176
      t205 = t167 * t153
      t206 = t205 * t170
      t209 = t120 * t30 * t5
      t210 = t9 * t50
      t227 = t2 * t3
      t228 = t227 * t15
      t229 = z ** 2
      t233 = 0.1728D4 * t228 * t39 * t30 * t229
      t234 = t43 * t3
      t236 = t30 * z
      t239 = 0.3456D4 * t234 * t15 * t58 * t236
      t246 = 0.1728D4 * t36 * t37 * x3 * t64 * z * t23 * t27
      t247 = t8 * t10
      t250 = t36 * t247 * t97 * t236
      t253 = t228 * t88 * t236
      t261 = 0.1728D4 * t6 * t11 * x3 * t64 * t23 * t145 * z
      t262 = -0.748D3 * t193 + 0.306D3 * t197 - 0.96D2 * t200 - 0.96D2 *
     # t203 - 0.32D2 * t206 + 0.216D3 * t209 * t210 * x4 * x1 * t145 - 0
     #.144D3 * t209 * t210 * t27 * x1 * t145 - 0.252D3 * t120 * t30 * t1
     #28 * t152 * t50 * t146 + t233 - t239 + t246 + 0.128D3 * t250 + 0.1
     #28D3 * t253 - t261
      t264 = t6 * t84 * t65
      t268 = 0.1440D4 * t92 * t39 * t76
      t269 = t120 * t30
      t270 = t4 * t8
      t274 = 0.72D2 * t269 * t270 * t50 * t75
      t280 = t2 * s * t229
      t284 = t280 * t3 * t15 * t18 * t30
      t289 = t280 * t4 * t247 * t96 * t30
      t291 = t36 * t8
      t292 = t10 * t13
      t296 = t227 * t7
      t297 = x1 * x3
      t301 = t6 * t9
      t303 = t133 * t13 * x3
      t307 = x1 * t18
      t309 = t296 * t307 * t64
      t311 = t234 * t7
      t313 = t311 * t307 * t49
      t315 = t56 * t8
      t316 = t10 * t96
      t318 = t315 * t316 * t49
      t321 = t291 * t316 * t64
      t326 = t301 * t133 * t96 * t18 * t64
      t328 = 0.240D3 * t264 - t268 - t274 - 0.36D2 * t269 * t270 * t50 *
     # t70 - 0.64D2 * t284 - 0.64D2 * t289 + 0.576D3 * t291 * t292 * t64
     # + 0.288D3 * t296 * t297 * t64 + 0.288D3 * t301 * t303 * t64 - 0.4
     #7D2 * t309 - 0.17D2 * t313 - 0.275D3 * t318 - 0.94D2 * t321 - 0.47
     #D2 * t326
      t344 = 0.72D2 * t34 + 0.72D2 * t41 - 0.4320D4 * t61 + 0.1728D4 * t
     #66 - t79 + 0.134D3 * t82 - 0.64D2 * t86 - 0.216D3 * t90 + 0.264D3 
     #* t94 - 0.496D3 * t99 + 0.752D3 * t102 + 0.16D2 * t105 - 0.216D3 *
     # t109
      t357 = -0.496D3 * t112 - t116 - 0.64D2 * t118 - t150 - 0.640D3 * t
     #158 - 0.608D3 * t162 + 0.72D2 * t178 + 0.72D2 * t183 - 0.1728D4 * 
     #t186 + 0.592D3 * t193 - 0.190D3 * t197 + 0.640D3 * t200 + 0.640D3 
     #* t203 + 0.64D2 * t206
      t382 = -t233 + t239 - t246 - 0.256D3 * t250 - 0.256D3 * t253 + t26
     #1 - 0.2808D4 * t57 * t58 * t32 + 0.288D3 * t191 * t39 * t147 + 0.2
     #808D4 * t195 * t58 * t176 - 0.576D3 * t185 * t161 + 0.64D2 * t205 
     #* t173 + 0.288D3 * t92 * t39 * t71 + 0.72D2 * t195 * t13 * t48 * t
     #59 + 0.64D2 * t264
      t406 = t268 - 0.72D2 * t269 + t274 + 0.128D3 * t284 + 0.128D3 * t2
     #89 + 0.54D2 * t309 + 0.490D3 * t313 + 0.156D3 * t318 + 0.108D3 * t
     #321 + 0.54D2 * t326 + 0.72D2 * t44 * t9 * t303 * t49 + 0.72D2 * t2
     #69 * t3 * t7 * t23 * x4 - 0.2808D4 * t315 * t292 * t49 + 0.2808D4 
     #* t311 * t297 * t49
      rrgq2qgh21J2 = -(wd * (t107 + t188 + t262 + t328) + wd * (t344 + t
     #357 + t382 + t406)) / t1 / t48 / t30 / z / 0.3141592653589793D1 / 
     #0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh21J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t7 * x1
      t9 = t5 * t8
      t10 = 0.1D1 - x3
      t11 = s * t3
      t12 = t6 * x1
      t17 = s - t11 * t12 * x3 - t11 * t12 * t10
      t18 = t10 * t17
      t19 = 0.1D1 - x1
      t20 = t6 * t19
      t23 = 0.1D1 - x4
      t26 = s - t11 * t20 * x4 - t11 * t20 * t23
      t27 = t26 * t19
      t28 = t27 * x4
      t30 = t9 * t18 * t28
      t32 = s * t1
      t33 = t4 * t3
      t34 = t32 * t33
      t35 = t7 * t6
      t36 = t35 * x1
      t37 = t34 * t36
      t38 = t17 ** 2
      t39 = t10 * t38
      t40 = t19 ** 2
      t41 = t26 * t40
      t42 = t23 ** 2
      t43 = t41 * t42
      t46 = 0.32D2 * t37 * t39 * t43
      t47 = t2 * t33
      t48 = t47 * t36
      t49 = x4 ** 2
      t50 = t41 * t49
      t52 = t48 * t18 * t50
      t54 = x1 ** 2
      t55 = t35 * t54
      t56 = t47 * t55
      t57 = t10 ** 2
      t58 = t57 * t17
      t60 = t56 * t58 * t28
      t62 = t32 * t4
      t63 = t62 * t8
      t65 = t63 * t39 * t28
      t67 = t32 * t38
      t68 = t4 ** 2
      t71 = t7 ** 2
      t72 = t71 * t54
      t76 = cos(x2 * 0.3141592653589793D1)
      t78 = x4 * t23
      t80 = Sqrt(x3 * t10 * t78)
      t83 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t76 * t80
      t84 = t83 ** 2
      t89 = x3 ** 2
      t90 = t89 * t17
      t91 = t27 * t23
      t93 = t56 * t90 * t91
      t96 = t67 * t26 * t33
      t97 = t35 * t40
      t108 = t67 * t26
      t109 = t4 * t7
      t113 = 0.72D2 * t108 * t109 * t40 * t49
      t118 = t5 * t7
      t119 = t54 * t89
      t120 = t17 * t26
      t125 = z ** 2
      t126 = t2 * s * t125
      t128 = t7 * t54
      t131 = t126 * t4 * t128 * t57 * t26
      t132 = 0.64D2 * t131
      t136 = t126 * t3 * t12 * t10 * t26
      t137 = 0.64D2 * t136
      t138 = 0.128D3 * t30 + t46 - 0.312D3 * t52 + 0.128D3 * t60 - 0.528
     #D3 * t65 - 0.252D3 * t67 * t26 * t68 * t72 * t40 * t84 - 0.144D3 *
     # t93 - 0.144D3 * t96 * t97 * t23 * x1 * t83 + 0.216D3 * t96 * t97 
     #* x4 * x1 * t83 - t113 - 0.36D2 * t108 * t109 * t40 * t42 + 0.576D
     #3 * t118 * t119 * t120 - t132 - t137
      t139 = x3 * t38
      t141 = t63 * t139 * t28
      t143 = t68 * t3
      t146 = t54 * x1
      t147 = t71 * t6 * t146
      t148 = t2 * t143 * t147
      t149 = t41 * t84
      t151 = t148 * t18 * t149
      t153 = t34 * t55
      t154 = t27 * t83
      t156 = t153 * t39 * t154
      t158 = x3 * t17
      t160 = t56 * t158 * t154
      t162 = t2 * t68
      t164 = t162 * t71 * t146
      t166 = t164 * t90 * t154
      t168 = t72 * x3
      t169 = t162 * t168
      t171 = t40 * x4 * t83
      t172 = t120 * t171
      t173 = t169 * t172
      t175 = t2 * t3
      t176 = t175 * t6
      t177 = x1 * x3
      t181 = t47 * t35
      t183 = t146 * t89 * x3
      t187 = t10 * x1
      t189 = t176 * t187 * t120
      t194 = t181 * t146 * t57 * t10 * t120
      t196 = t54 * t57
      t198 = t118 * t196 * t120
      t200 = t62 * t7
      t201 = t38 * t26
      t203 = t200 * t196 * t201
      t205 = t32 * t3
      t206 = t205 * t6
      t208 = t206 * t187 * t201
      t211 = t37 * t39 * t50
      t213 = 0.2304D4 * t141 - 0.748D3 * t151 + 0.306D3 * t156 - 0.144D3
     # * t160 - 0.144D3 * t166 + 0.1440D4 * t173 + 0.288D3 * t176 * t177
     # * t120 + 0.288D3 * t181 * t183 * t120 - 0.47D2 * t189 - 0.47D2 * 
     #t194 - 0.94D2 * t198 - 0.275D3 * t203 - 0.17D2 * t208 + 0.32D2 * t
     #211
      t217 = 0.1440D4 * t48 * t158 * t50
      t219 = t48 * t18 * t43
      t222 = t56 * t58 * t91
      t225 = t164 * t58 * t154
      t228 = t56 * t18 * t154
      t236 = 0.1728D4 * t5 * t8 * x3 * t120 * z * t19 * t23
      t237 = t36 * x3
      t239 = t40 * t23
      t240 = t239 * x4
      t241 = t120 * t240
      t242 = t47 * t237 * t241
      t249 = 0.72D2 * t37 * t139 * t50
      t250 = t175 * t12
      t254 = 0.1728D4 * t250 * t158 * t26 * t125
      t256 = t26 * z
      t259 = 0.3456D4 * t205 * t12 * t139 * t256
      t260 = t36 * t10
      t262 = t47 * t260 * t241
      t264 = t72 * t10
      t265 = t162 * t264
      t266 = t239 * t83
      t267 = t120 * t266
      t268 = t265 * t267
      t270 = t32 * t68
      t271 = t270 * t264
      t272 = t201 * t171
      t273 = t271 * t272
      t275 = -t217 + 0.12D2 * t219 + 0.148D3 * t222 - 0.96D2 * t225 - 0.
     #96D2 * t228 + t236 - 0.1440D4 * t242 + 0.252D3 * t37 * t139 * t43 
     #+ t249 + t254 - t259 + 0.240D3 * t262 + 0.16D2 * t268 - 0.32D2 * t
     #273
      t283 = t32 * t143 * t147
      t286 = 0.32D2 * t283 * t39 * t149
      t287 = t265 * t172
      t291 = t5 * t128 * t58 * t256
      t294 = t250 * t18 * t256
      t295 = 0.128D3 * t294
      t302 = 0.1728D4 * t47 * t55 * x3 * t120 * t19 * t83 * z
      t304 = t63 * t39 * t91
      t307 = t201 * t240
      t308 = t34 * t260 * t307
      t313 = t270 * t168
      t316 = t201 * t266
      t323 = t9 * t158 * t91
      t326 = t9 * t18 * t91
      t328 = 0.72D2 * t67 * t26 * t4 * t7 * t40 * t78 + t286 + 0.880D3 *
     # t287 + 0.128D3 * t291 + t295 - t302 - 0.122D3 * t304 + 0.32D2 * t
     #308 + 0.36D2 * t283 * t139 * t149 + 0.72D2 * t313 * t272 + 0.144D3
     # * t313 * t316 + 0.216D3 * t34 * t237 * t307 - 0.144D3 * t323 + 0.
     #148D3 * t326
      t352 = -0.496D3 * t30 - t46 + 0.264D3 * t52 - 0.496D3 * t60 + 0.75
     #2D3 * t65 + 0.288D3 * t48 * t158 * t43 + 0.72D2 * t153 * t89 * t38
     # * t28 + 0.72D2 * t93 + 0.2808D4 * t153 * t139 * t154 - 0.576D3 * 
     #t169 * t267 + 0.64D2 * t271 * t316 - 0.72D2 * t108 + t113
      t367 = 0.128D3 * t131 + 0.128D3 * t136 - 0.4320D4 * t141 + 0.592D3
     # * t151 - 0.190D3 * t156 + 0.72D2 * t160 + 0.72D2 * t166 - 0.1728D
     #4 * t173 + 0.54D2 * t189 + 0.54D2 * t194 + 0.108D3 * t198 + 0.156D
     #3 * t203 + 0.490D3 * t208 - 0.64D2 * t211
      t395 = 0.72D2 * t34 * t35 * t183 * t201 + t217 + 0.72D2 * t108 * t
     #3 * t6 * t19 * x4 + 0.2808D4 * t206 * t177 * t201 - 0.2808D4 * t20
     #0 * t119 * t201 + 0.16D2 * t219 - 0.216D3 * t222 + 0.640D3 * t225 
     #+ 0.640D3 * t228 - t236 + 0.1728D4 * t242 - t249 - 0.2808D4 * t63 
     #* t139 * t91 + 0.288D3 * t148 * t158 * t149
      t406 = -t254 + t259 + 0.64D2 * t262 - 0.608D3 * t268 + 0.64D2 * t2
     #73 - t286 - 0.640D3 * t287 - 0.256D3 * t291 - 0.256D3 * t294 + t30
     #2 + 0.134D3 * t304 - 0.64D2 * t308 + 0.72D2 * t323 - 0.216D3 * t32
     #6
      t419 = t295 - 0.8D1 * t198 + 0.128D3 * t287 - 0.64D2 * t151 + 0.28
     #372D5 * t203 - t137 + 0.28224D5 * t156 + 0.252D3 * t194 + 0.304D3 
     #* t304 + 0.8D1 * t60 + 0.25220D5 * t208
      t430 = -0.64D2 * t219 - 0.256D3 * t225 + 0.128D3 * t222 - 0.64D2 *
     # t52 - 0.4D1 * t189 - t132 + 0.128D3 * t268 - 0.128D3 * t262 - 0.7
     #4296D5 * t65 + 0.512D3 * t291 + 0.8D1 * t30
      rrgq2qgh21J3 = -(wd * (t138 + t213 + t275 + t328) + wd * (t352 + t
     #367 + t395 + t406) + wd * (t419 + t430)) / t1 / t38 / t26 / z / 0.
     #3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh21J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t9 * t10
      t12 = t6 * t11
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t15 = s * t3
      t16 = t7 * x1
      t21 = s - t15 * t16 * x3 - t15 * t16 * t13
      t22 = t14 * t21
      t23 = 0.1D1 - x1
      t24 = t7 * t23
      t27 = 0.1D1 - x4
      t30 = s - t15 * t24 * x4 - t15 * t24 * t27
      t31 = t30 * t23
      t32 = t31 * t27
      t34 = t12 * t22 * t32
      t36 = s * t1
      t37 = t4 ** 2
      t38 = t37 * t3
      t40 = t8 ** 2
      t42 = t10 * x1
      t43 = t40 * t7 * t42
      t44 = t36 * t38 * t43
      t45 = t21 ** 2
      t46 = t45 * x3
      t47 = t23 ** 2
      t48 = t30 * t47
      t52 = cos(x2 * 0.3141592653589793D1)
      t54 = t27 * x4
      t56 = Sqrt(x3 * t13 * t54)
      t59 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t52 * t56
      t60 = t59 ** 2
      t61 = t48 * t60
      t65 = t13 * t21
      t66 = t31 * t59
      t68 = t12 * t65 * t66
      t70 = t2 * t3
      t71 = t70 * t16
      t72 = x3 * t21
      t73 = z ** 2
      t77 = 0.1728D4 * t71 * t72 * t30 * t73
      t78 = t36 * t3
      t80 = t30 * z
      t83 = 0.3456D4 * t78 * t16 * t46 * t80
      t84 = t36 * t45
      t91 = t2 * t37
      t92 = t40 * t10
      t93 = t92 * x3
      t94 = t91 * t93
      t95 = t21 * t30
      t97 = t47 * x4 * t59
      t98 = t95 * t97
      t99 = t94 * t98
      t101 = t13 * t45
      t104 = 0.32D2 * t44 * t101 * t61
      t105 = t92 * t13
      t106 = t91 * t105
      t107 = t106 * t98
      t109 = t47 * t27
      t110 = t109 * t59
      t111 = t95 * t110
      t112 = t106 * t111
      t114 = t36 * t5
      t115 = t9 * x1
      t116 = t115 * x3
      t118 = t45 * t30
      t119 = t109 * x4
      t120 = t118 * t119
      t123 = t36 * t4
      t124 = t8 * x1
      t125 = t123 * t124
      t126 = t31 * x4
      t128 = t125 * t46 * t126
      t131 = t95 * t119
      t132 = t6 * t116 * t131
      t134 = t114 * t115
      t135 = t27 ** 2
      t136 = t48 * t135
      t140 = 0.148D3 * t34 + 0.36D2 * t44 * t46 * t61 - 0.96D2 * t68 + t
     #77 - t83 + 0.72D2 * t84 * t30 * t4 * t8 * t47 * t54 + 0.1440D4 * t
     #99 + t104 + 0.880D3 * t107 + 0.16D2 * t112 + 0.216D3 * t114 * t116
     # * t120 + 0.2304D4 * t128 - 0.1440D4 * t132 + 0.252D3 * t134 * t46
     # * t136
      t141 = t115 * t13
      t143 = t6 * t141 * t131
      t145 = t6 * t115
      t146 = x4 ** 2
      t147 = t48 * t146
      t150 = 0.1440D4 * t145 * t72 * t147
      t151 = x3 ** 2
      t152 = t151 * t21
      t154 = t12 * t152 * t32
      t156 = t2 * t4
      t157 = t156 * t124
      t159 = t157 * t72 * t32
      t167 = 0.1728D4 * t156 * t124 * x3 * t95 * z * t23 * t27
      t168 = t8 * t10
      t171 = t156 * t168 * t22 * t80
      t174 = t71 * t65 * t80
      t175 = 0.128D3 * t174
      t177 = t125 * t101 * t32
      t180 = t114 * t141 * t120
      t183 = t157 * t65 * t32
      t186 = t145 * t65 * t147
      t189 = t84 * t30 * t5
      t190 = t9 * t47
      t207 = 0.240D3 * t143 - t150 - 0.144D3 * t154 - 0.144D3 * t159 + t
     #167 + 0.128D3 * t171 + t175 - 0.122D3 * t177 + 0.32D2 * t180 + 0.1
     #48D3 * t183 - 0.312D3 * t186 + 0.216D3 * t189 * t190 * x4 * x1 * t
     #59 - 0.144D3 * t189 * t190 * t27 * x1 * t59 - 0.252D3 * t84 * t30 
     #* t37 * t92 * t47 * t60
      t210 = t2 * t38 * t43
      t212 = t210 * t65 * t61
      t214 = t114 * t11
      t216 = t214 * t101 * t66
      t219 = t91 * t40 * t42
      t221 = t219 * t22 * t66
      t224 = t125 * t101 * t126
      t227 = t145 * t65 * t136
      t229 = t84 * t30
      t230 = t4 * t8
      t234 = 0.72D2 * t229 * t230 * t47 * t146
      t240 = t2 * s * t73
      t244 = t240 * t3 * t16 * t13 * t30
      t245 = 0.64D2 * t244
      t249 = t240 * t4 * t168 * t14 * t30
      t250 = 0.64D2 * t249
      t251 = t156 * t8
      t252 = t10 * t151
      t256 = t70 * t7
      t257 = x1 * x3
      t261 = t6 * t9
      t263 = t42 * t151 * x3
      t267 = x1 * t13
      t269 = t256 * t267 * t95
      t271 = t78 * t7
      t273 = t271 * t267 * t118
      t275 = -0.748D3 * t212 + 0.306D3 * t216 - 0.96D2 * t221 - 0.528D3 
     #* t224 + 0.12D2 * t227 - t234 - 0.36D2 * t229 * t230 * t47 * t135 
     #- t245 - t250 + 0.576D3 * t251 * t252 * t95 + 0.288D3 * t256 * t25
     #7 * t95 + 0.288D3 * t261 * t263 * t95 - 0.47D2 * t269 - 0.17D2 * t
     #273
      t276 = t123 * t8
      t277 = t10 * t14
      t279 = t276 * t277 * t118
      t282 = t251 * t277 * t95
      t287 = t261 * t42 * t14 * t13 * t95
      t289 = t36 * t37
      t290 = t289 * t105
      t291 = t118 * t97
      t292 = t290 * t291
      t294 = t289 * t93
      t297 = t118 * t110
      t301 = t12 * t72 * t66
      t304 = t219 * t152 * t66
      t312 = 0.1728D4 * t6 * t11 * x3 * t95 * t23 * t59 * z
      t314 = t157 * t65 * t126
      t318 = 0.32D2 * t134 * t101 * t136
      t320 = t134 * t101 * t147
      t323 = t12 * t22 * t126
      t327 = 0.72D2 * t134 * t46 * t147
      t328 = -0.275D3 * t279 - 0.94D2 * t282 - 0.47D2 * t287 - 0.32D2 * 
     #t292 + 0.72D2 * t294 * t291 + 0.144D3 * t294 * t297 - 0.144D3 * t3
     #01 - 0.144D3 * t304 - t312 + 0.128D3 * t314 + t318 + 0.32D2 * t320
     # + 0.128D3 * t323 + t327
      t355 = -0.216D3 * t34 + 0.64D2 * t290 * t297 + 0.640D3 * t68 + 0.2
     #88D3 * t145 * t72 * t136 + 0.72D2 * t214 * t151 * t45 * t126 - t77
     # + t83 - 0.2808D4 * t125 * t46 * t32 + 0.288D3 * t210 * t72 * t61 
     #+ 0.2808D4 * t214 * t46 * t66 - 0.576D3 * t94 * t111 - 0.1728D4 * 
     #t99 - t104
      t368 = -0.640D3 * t107 - 0.608D3 * t112 - 0.4320D4 * t128 + 0.1728
     #D4 * t132 + 0.64D2 * t143 + t150 + 0.72D2 * t154 + 0.72D2 * t159 -
     # t167 - 0.256D3 * t171 - 0.256D3 * t174 + 0.134D3 * t177 - 0.64D2 
     #* t180 - 0.216D3 * t183
      t383 = 0.264D3 * t186 + 0.592D3 * t212 - 0.190D3 * t216 + 0.640D3 
     #* t221 + 0.752D3 * t224 + 0.16D2 * t227 + t234 + 0.128D3 * t244 + 
     #0.128D3 * t249 + 0.54D2 * t269 + 0.490D3 * t273 + 0.156D3 * t279 +
     # 0.108D3 * t282 + 0.54D2 * t287
      t406 = 0.72D2 * t114 * t9 * t263 * t118 + 0.72D2 * t229 * t3 * t7 
     #* t23 * x4 - 0.2808D4 * t276 * t252 * t118 + 0.2808D4 * t271 * t25
     #7 * t118 + 0.64D2 * t292 + 0.72D2 * t301 + 0.72D2 * t304 + t312 - 
     #0.72D2 * t229 - 0.496D3 * t314 - t318 - 0.64D2 * t320 - 0.496D3 * 
     #t323 - t327
      t419 = t175 - 0.8D1 * t282 + 0.128D3 * t107 - 0.64D2 * t212 + 0.28
     #372D5 * t279 - t245 + 0.28224D5 * t216 + 0.252D3 * t287 + 0.304D3 
     #* t177 + 0.8D1 * t323 + 0.25220D5 * t273
      t430 = -0.64D2 * t227 - 0.256D3 * t221 + 0.128D3 * t34 - 0.64D2 * 
     #t186 - 0.4D1 * t269 - t250 + 0.128D3 * t112 - 0.128D3 * t143 - 0.7
     #4296D5 * t224 + 0.512D3 * t171 + 0.8D1 * t314
      rrgq2qgh21J4 = -(wd * (t140 + t207 + t275 + t328) + wd * (t355 + t
     #368 + t383 + t406) + wd * (t419 + t430)) / t1 / t45 / t30 / z / 0.
     #3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh21J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t7 * x1
      t9 = t5 * t8
      t10 = s * t3
      t11 = t6 * x1
      t14 = 0.1D1 - x3
      t17 = s - t10 * t11 * x3 - t10 * t11 * t14
      t18 = x3 * t17
      t19 = 0.1D1 - x1
      t20 = t6 * t19
      t23 = 0.1D1 - x4
      t26 = s - t10 * t20 * x4 - t10 * t20 * t23
      t27 = t26 * t19
      t28 = t27 * t23
      t30 = t9 * t18 * t28
      t32 = s * t1
      t33 = t4 * t3
      t34 = t32 * t33
      t35 = t7 * t6
      t36 = t35 * x1
      t37 = t36 * x3
      t39 = t17 ** 2
      t40 = t39 * t26
      t41 = t19 ** 2
      t42 = t41 * t23
      t43 = t42 * x4
      t44 = t40 * t43
      t47 = t32 * t4
      t48 = t47 * t8
      t49 = x3 * t39
      t50 = t27 * x4
      t52 = t48 * t49 * t50
      t54 = t2 * t33
      t56 = t17 * t26
      t57 = t56 * t43
      t58 = t54 * t37 * t57
      t60 = x1 ** 2
      t61 = t35 * t60
      t62 = t54 * t61
      t63 = t14 ** 2
      t64 = t63 * t17
      t66 = t62 * t64 * t28
      t68 = t14 * t17
      t70 = t9 * t68 * t50
      t72 = t4 ** 2
      t73 = t72 * t3
      t75 = t7 ** 2
      t77 = t60 * x1
      t78 = t75 * t6 * t77
      t79 = t32 * t73 * t78
      t80 = t14 * t39
      t81 = t26 * t41
      t85 = cos(x2 * 0.3141592653589793D1)
      t87 = x4 * t23
      t89 = Sqrt(x3 * t14 * t87)
      t92 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t85 * t89
      t93 = t92 ** 2
      t94 = t81 * t93
      t97 = 0.32D2 * t79 * t80 * t94
      t98 = t2 * t72
      t99 = t75 * t60
      t100 = t99 * t14
      t101 = t98 * t100
      t103 = t41 * x4 * t92
      t104 = t56 * t103
      t105 = t101 * t104
      t107 = t42 * t92
      t108 = t56 * t107
      t109 = t101 * t108
      t117 = 0.1728D4 * t54 * t61 * x3 * t56 * t19 * t92 * z
      t121 = t32 * t72
      t122 = t99 * x3
      t123 = t121 * t122
      t124 = t40 * t103
      t133 = 0.1728D4 * t5 * t8 * x3 * t56 * z * t19 * t23
      t134 = t7 * t60
      t136 = t26 * z
      t138 = t5 * t134 * t64 * t136
      t140 = -0.144D3 * t30 + 0.216D3 * t34 * t37 * t44 + 0.2304D4 * t52
     # - 0.1440D4 * t58 + 0.148D3 * t66 + 0.128D3 * t70 + t97 + 0.880D3 
     #* t105 + 0.16D2 * t109 - t117 + 0.36D2 * t79 * t49 * t94 + 0.72D2 
     #* t123 * t124 + t133 + 0.128D3 * t138
      t141 = t2 * t3
      t142 = t141 * t11
      t144 = t142 * t68 * t136
      t145 = 0.128D3 * t144
      t146 = t36 * t14
      t148 = t54 * t146 * t57
      t150 = t54 * t36
      t151 = x4 ** 2
      t152 = t81 * t151
      t155 = 0.1440D4 * t150 * t18 * t152
      t156 = x3 ** 2
      t157 = t156 * t17
      t159 = t62 * t157 * t28
      t162 = t48 * t80 * t28
      t165 = t34 * t146 * t44
      t168 = t9 * t68 * t28
      t171 = t150 * t68 * t152
      t173 = t34 * t61
      t174 = t27 * t92
      t176 = t173 * t80 * t174
      t179 = t98 * t75 * t77
      t181 = t179 * t64 * t174
      t184 = t62 * t68 * t174
      t186 = t32 * t39
      t187 = t186 * t26
      t188 = t4 * t7
      t192 = 0.72D2 * t187 * t188 * t41 * t151
      t193 = t23 ** 2
      t199 = z ** 2
      t200 = t2 * s * t199
      t204 = t200 * t3 * t11 * t14 * t26
      t205 = 0.64D2 * t204
      t206 = t145 + 0.240D3 * t148 - t155 - 0.144D3 * t159 - 0.122D3 * t
     #162 + 0.32D2 * t165 + 0.148D3 * t168 - 0.312D3 * t171 + 0.306D3 * 
     #t176 - 0.96D2 * t181 - 0.96D2 * t184 - t192 - 0.36D2 * t187 * t188
     # * t41 * t193 - t205
      t211 = t200 * t4 * t134 * t63 * t26
      t212 = 0.64D2 * t211
      t213 = t5 * t7
      t214 = t60 * t156
      t218 = t141 * t6
      t219 = x1 * x3
      t223 = t35 * t54
      t225 = t77 * t156 * x3
      t229 = x1 * t14
      t231 = t218 * t229 * t56
      t233 = t32 * t3
      t234 = t233 * t6
      t236 = t234 * t229 * t40
      t238 = t47 * t7
      t239 = t60 * t63
      t241 = t238 * t239 * t40
      t244 = t213 * t239 * t56
      t249 = t223 * t77 * t63 * t14 * t56
      t251 = t34 * t36
      t252 = t81 * t193
      t258 = 0.72D2 * t251 * t49 * t152
      t260 = t62 * t64 * t50
      t263 = t48 * t80 * t50
      t266 = t150 * t68 * t252
      t268 = -t212 + 0.576D3 * t213 * t214 * t56 + 0.288D3 * t218 * t219
     # * t56 + 0.288D3 * t223 * t225 * t56 - 0.47D2 * t231 - 0.17D2 * t2
     #36 - 0.275D3 * t241 - 0.94D2 * t244 - 0.47D2 * t249 + 0.252D3 * t2
     #51 * t49 * t252 + t258 + 0.128D3 * t260 - 0.528D3 * t263 + 0.12D2 
     #* t266
      t272 = 0.1728D4 * t142 * t18 * t26 * t199
      t276 = 0.3456D4 * t233 * t11 * t49 * t136
      t277 = t40 * t107
      t281 = t62 * t18 * t174
      t284 = t179 * t157 * t174
      t286 = t98 * t122
      t287 = t286 * t104
      t290 = t2 * t73 * t78
      t292 = t290 * t68 * t94
      t296 = 0.32D2 * t251 * t80 * t252
      t298 = t251 * t80 * t152
      t306 = t121 * t100
      t307 = t306 * t124
      t310 = t186 * t26 * t33
      t311 = t35 * t41
      t328 = t272 - t276 + 0.144D3 * t123 * t277 - 0.144D3 * t281 - 0.14
     #4D3 * t284 + 0.1440D4 * t287 - 0.748D3 * t292 + t296 + 0.32D2 * t2
     #98 + 0.72D2 * t186 * t26 * t4 * t7 * t41 * t87 - 0.32D2 * t307 + 0
     #.216D3 * t310 * t311 * x4 * x1 * t92 - 0.144D3 * t310 * t311 * t23
     # * x1 * t92 - 0.252D3 * t186 * t26 * t72 * t99 * t41 * t93
      t342 = 0.72D2 * t30 - 0.4320D4 * t52 + 0.1728D4 * t58 - 0.216D3 * 
     #t66 - 0.496D3 * t70 - t97 - 0.640D3 * t105 - 0.608D3 * t109 + t117
     # - t133 - 0.256D3 * t138 - 0.256D3 * t144 + 0.64D2 * t148
      t361 = t155 + 0.72D2 * t159 + 0.134D3 * t162 - 0.64D2 * t165 - 0.2
     #16D3 * t168 + 0.264D3 * t171 + 0.288D3 * t290 * t18 * t94 + 0.2808
     #D4 * t173 * t49 * t174 - 0.576D3 * t286 * t108 + 0.64D2 * t306 * t
     #277 - 0.190D3 * t176 + 0.640D3 * t181 + 0.640D3 * t184 + t192
      t383 = 0.128D3 * t204 + 0.128D3 * t211 + 0.54D2 * t231 + 0.490D3 *
     # t236 + 0.156D3 * t241 + 0.108D3 * t244 + 0.54D2 * t249 - t258 + 0
     #.288D3 * t150 * t18 * t252 + 0.72D2 * t173 * t156 * t39 * t50 - 0.
     #496D3 * t260 + 0.752D3 * t263 + 0.16D2 * t266 - 0.2808D4 * t48 * t
     #49 * t28
      t406 = -t272 + t276 - 0.72D2 * t187 + 0.72D2 * t281 + 0.72D2 * t28
     #4 - 0.1728D4 * t287 + 0.592D3 * t292 + 0.72D2 * t34 * t35 * t225 *
     # t40 + 0.72D2 * t187 * t3 * t6 * t19 * x4 - 0.2808D4 * t238 * t214
     # * t40 + 0.2808D4 * t234 * t219 * t40 - t296 - 0.64D2 * t298 + 0.6
     #4D2 * t307
      t419 = t145 - 0.8D1 * t244 + 0.128D3 * t105 - 0.64D2 * t292 + 0.28
     #372D5 * t241 - t205 + 0.28224D5 * t176 + 0.252D3 * t249 + 0.304D3 
     #* t162 + 0.8D1 * t260 + 0.25220D5 * t236
      t430 = -0.64D2 * t266 - 0.256D3 * t181 + 0.128D3 * t66 - 0.64D2 * 
     #t171 - 0.4D1 * t231 - t212 + 0.128D3 * t109 - 0.128D3 * t148 - 0.7
     #4296D5 * t263 + 0.512D3 * t138 + 0.8D1 * t70
      rrgq2qgh21J5 = -(wd * (t140 + t206 + t268 + t328) + wd * (t342 + t
     #361 + t383 + t406) + wd * (t419 + t430)) / t1 / t39 / t26 / z / 0.
     #3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh21J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = s * t3
      t5 = 0.1D1 - z
      t6 = t5 * x1
      t9 = 0.1D1 - x3
      t12 = s - t4 * t6 * x3 - t4 * t6 * t9
      t13 = t12 ** 2
      t15 = 0.1D1 - x1
      t16 = t5 * t15
      t19 = 0.1D1 - x4
      t22 = s - t4 * t16 * x4 - t4 * t16 * t19
      t23 = t2 * t13 * t22
      t25 = t3 ** 2
      t26 = t2 * t25
      t27 = t5 ** 2
      t28 = t27 * x1
      t29 = t26 * t28
      t30 = x3 * t13
      t31 = t22 * t15
      t32 = t31 * t19
      t36 = t1 ** 2
      t37 = t25 ** 2
      t38 = t37 * t3
      t40 = t27 ** 2
      t42 = x1 ** 2
      t43 = t42 * x1
      t44 = t40 * t5 * t43
      t45 = t36 * t38 * t44
      t46 = x3 * t12
      t47 = t15 ** 2
      t48 = t22 * t47
      t52 = cos(x2 * 0.3141592653589793D1)
      t56 = Sqrt(x3 * t9 * x4 * t19)
      t59 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t52 * t56
      t60 = t59 ** 2
      t61 = t48 * t60
      t65 = t25 * t3
      t66 = t2 * t65
      t67 = t27 * t5
      t68 = t67 * t42
      t69 = t66 * t68
      t70 = t31 * t59
      t74 = t36 * t37
      t75 = t40 * t42
      t77 = t74 * t75 * x3
      t78 = t12 * t22
      t79 = t47 * t19
      t80 = t79 * t59
      t81 = t78 * t80
      t85 = t75 * t9
      t86 = t2 * t37 * t85
      t87 = t13 * t22
      t91 = t36 * t65
      t92 = t67 * x1
      t93 = t91 * t92
      t94 = t19 ** 2
      t95 = t48 * t94
      t99 = x3 ** 2
      t101 = t31 * x4
      t105 = t92 * t9
      t107 = t79 * x4
      t108 = t78 * t107
      t109 = t91 * t105 * t108
      t111 = x4 ** 2
      t112 = t48 * t111
      t116 = t91 * t68
      t117 = t99 * t12
      t121 = t36 * t25
      t122 = t121 * t28
      t129 = -0.72D2 * t23 - 0.2808D4 * t29 * t30 * t32 + 0.288D3 * t45 
     #* t46 * t61 + 0.2808D4 * t69 * t30 * t70 - 0.576D3 * t77 * t81 + 0
     #.64D2 * t86 * t87 * t80 + 0.288D3 * t93 * t46 * t95 + 0.72D2 * t69
     # * t99 * t13 * t101 + 0.64D2 * t109 + 0.1440D4 * t93 * t46 * t112 
     #+ 0.72D2 * t116 * t117 * t32 + 0.72D2 * t122 * t46 * t32 - 0.4320D
     #4 * t29 * t30 * t101
      t134 = t66 * t92
      t138 = t9 * t13
      t140 = t29 * t138 * t32
      t146 = t9 * t12
      t151 = t93 * t146 * t112
      t153 = t9 ** 2
      t154 = t153 * t12
      t156 = t116 * t154 * t101
      t159 = t29 * t138 * t101
      t162 = t93 * t146 * t95
      t165 = t116 * t154 * t32
      t168 = t122 * t146 * t101
      t181 = 0.1728D4 * t91 * t92 * x3 * t108 - 0.72D2 * t134 * t30 * t1
     #12 + 0.134D3 * t140 - 0.64D2 * t66 * t105 * t87 * t107 - 0.216D3 *
     # t122 * t146 * t32 + 0.264D3 * t151 - 0.496D3 * t156 + 0.752D3 * t
     #159 + 0.16D2 * t162 - 0.216D3 * t165 - 0.496D3 * t168 - 0.32D2 * t
     #134 * t138 * t95 - 0.64D2 * t134 * t138 * t112 - 0.32D2 * t2 * t38
     # * t44 * t138 * t61
      t183 = t74 * t85
      t185 = t47 * x4 * t59
      t186 = t78 * t185
      t187 = t183 * t186
      t189 = t183 * t81
      t195 = t74 * t40 * t43
      t202 = t45 * t146 * t61
      t205 = t69 * t138 * t70
      t208 = t195 * t154 * t70
      t216 = t36 * t3
      t217 = t216 * t6
      t218 = z ** 2
      t223 = t2 * t3
      t225 = t22 * z
      t236 = t27 * t42
      t239 = t121 * t236 * t154 * t225
      t241 = -0.640D3 * t187 - 0.608D3 * t189 + 0.72D2 * t116 * t46 * t7
     #0 + 0.72D2 * t195 * t117 * t70 - 0.1728D4 * t77 * t186 + 0.592D3 *
     # t202 - 0.190D3 * t205 + 0.640D3 * t208 + 0.640D3 * t116 * t146 * 
     #t70 + 0.64D2 * t86 * t87 * t185 - 0.1728D4 * t217 * t46 * t22 * t2
     #18 + 0.3456D4 * t223 * t6 * t30 * t225 - 0.1728D4 * t121 * t28 * x
     #3 * t78 * z * t15 * t19 - 0.256D3 * t239
      t243 = t217 * t146 * t225
      t258 = t36 * s * t218
      t262 = t258 * t3 * t6 * t9 * t22
      t267 = t258 * t25 * t236 * t153 * t22
      t270 = x1 * t9
      t272 = t216 * t5 * t270 * t78
      t274 = t223 * t5
      t276 = t274 * t270 * t87
      t278 = t26 * t27
      t279 = t42 * t153
      t281 = t278 * t279 * t87
      t285 = t121 * t27 * t279 * t78
      t291 = t91 * t67 * t43 * t153 * t9 * t78
      t312 = -0.256D3 * t243 + 0.1728D4 * t91 * t68 * x3 * t78 * t15 * t
     #59 * z + 0.72D2 * t23 * t25 * t27 * t47 * t111 + 0.128D3 * t262 + 
     #0.128D3 * t267 + 0.54D2 * t272 + 0.490D3 * t276 + 0.156D3 * t281 +
     # 0.108D3 * t285 + 0.54D2 * t291 + 0.72D2 * t66 * t67 * t43 * t99 *
     # x3 * t87 + 0.72D2 * t23 * t3 * t5 * t15 * x4 - 0.2808D4 * t278 * 
     #t42 * t99 * t87 + 0.2808D4 * t274 * x1 * x3 * t87
      t327 = 0.128D3 * t243 - 0.8D1 * t285 + 0.128D3 * t187 - 0.64D2 * t
     #202 + 0.28372D5 * t281 - 0.64D2 * t262 + 0.28224D5 * t205 + 0.252D
     #3 * t291 + 0.304D3 * t140 + 0.8D1 * t156 + 0.25220D5 * t276
      t339 = -0.64D2 * t162 - 0.256D3 * t208 + 0.128D3 * t165 - 0.64D2 *
     # t151 - 0.4D1 * t272 - 0.64D2 * t267 + 0.128D3 * t189 - 0.128D3 * 
     #t109 - 0.74296D5 * t159 + 0.512D3 * t239 + 0.8D1 * t168
      rrgq2qgh21J6 = -(wd * (t129 + t181 + t241 + t312) + wd * (t327 + t
     #339)) / t1 / t13 / t22 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh21J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t2 * t3
      t5 = 0.1D1 - z
      t6 = t5 * x1
      t8 = 0.1D1 - x3
      t9 = s * t3
      t14 = s - t9 * t6 * x3 - t9 * t6 * t8
      t15 = t8 * t14
      t16 = 0.1D1 - x1
      t17 = t5 * t16
      t20 = 0.1D1 - x4
      t23 = s - t9 * t17 * x4 - t9 * t17 * t20
      t24 = t23 * z
      t28 = t3 ** 2
      t29 = t2 * t28
      t30 = t5 ** 2
      t32 = x1 ** 2
      t33 = t8 ** 2
      t34 = t32 * t33
      t35 = t14 * t23
      t39 = t28 ** 2
      t40 = t2 * t39
      t41 = t30 ** 2
      t44 = t40 * t41 * t32 * t8
      t45 = t16 ** 2
      t50 = cos(x2 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t8 * x4 * t20)
      t57 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t50 * t54
      t65 = t32 * x1
      t68 = t23 * t45
      t69 = t57 ** 2
      t74 = t1 * s
      t75 = t74 * t28
      t77 = t14 ** 2
      t78 = t77 * t23
      t83 = z ** 2
      t84 = t2 * s * t83
      t90 = t28 * t3
      t92 = t30 * t5
      t93 = t92 * t32
      t95 = t8 * t77
      t96 = t23 * t16
      t97 = t96 * t57
      t101 = t2 * t90
      t108 = t30 * x1
      t109 = t75 * t108
      t110 = t96 * t20
      t114 = t101 * t93
      t115 = t33 * t14
      t116 = t96 * x4
      t122 = x1 * t8
      t126 = 0.128D3 * t4 * t6 * t15 * t24 - 0.8D1 * t29 * t30 * t34 * t
     #35 + 0.128D3 * t44 * t35 * t45 * x4 * t57 - 0.64D2 * t2 * t39 * t3
     # * t41 * t5 * t65 * t15 * t68 * t69 + 0.28372D5 * t75 * t30 * t34 
     #* t78 - 0.64D2 * t84 * t3 * t6 * t8 * t23 + 0.28224D5 * t74 * t90 
     #* t93 * t95 * t97 + 0.252D3 * t101 * t92 * t65 * t33 * t8 * t35 + 
     #0.304D3 * t109 * t95 * t110 + 0.8D1 * t114 * t115 * t116 + 0.25220
     #D5 * t74 * t3 * t5 * t122 * t78
      t127 = t92 * x1
      t128 = t101 * t127
      t129 = t20 ** 2
      t142 = x4 ** 2
      t152 = t30 * t32
      t157 = t45 * t20
      t179 = -0.64D2 * t128 * t15 * t68 * t129 - 0.256D3 * t40 * t41 * t
     #65 * t115 * t97 + 0.128D3 * t114 * t115 * t110 - 0.64D2 * t128 * t
     #15 * t68 * t142 - 0.4D1 * t4 * t5 * t122 * t35 - 0.64D2 * t84 * t2
     #8 * t152 * t33 * t23 + 0.128D3 * t44 * t35 * t157 * t57 - 0.128D3 
     #* t101 * t127 * t8 * t35 * t157 * x4 - 0.74296D5 * t109 * t95 * t1
     #16 + 0.512D3 * t29 * t152 * t115 * t24 + 0.8D1 * t29 * t108 * t15 
     #* t116
      rrgq2qgh21J7 = -wd * (t126 + t179) / t1 / t77 / t23 / z / 0.314159
     #2653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t7 * x1
      t9 = t5 * t8
      t10 = s * t3
      t11 = t6 * x1
      t14 = 0.1D1 - x3
      t18 = (s - t10 * t11 * x3 - t10 * t11 * t14) ** 2
      t19 = x3 * t18
      t20 = 0.1D1 - x1
      t21 = t6 * t20
      t24 = 0.1D1 - x4
      t27 = s - t10 * t21 * x4 - t10 * t21 * t24
      t28 = t27 * t20
      t33 = t1 ** 2
      t35 = t33 * t3 * t6
      t36 = x1 * x3
      t40 = t4 * t3
      t41 = t33 * t40
      t42 = t7 * t6
      t44 = t41 * t42 * x1
      t45 = t20 ** 2
      t46 = t24 ** 2
      t51 = x1 ** 2
      t52 = t42 * t51
      t53 = t41 * t52
      t54 = t14 ** 2
      t55 = t54 * t18
      t56 = t20 * t24
      t60 = t33 * t4
      t61 = t60 * t8
      t62 = t14 * t18
      t66 = t20 * x4
      t70 = x3 ** 2
      t71 = t70 * t18
      t81 = cos(x2 * 0.3141592653589793D1)
      t85 = Sqrt(x3 * t14 * x4 * t24)
      t88 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t81 * t85
      t89 = t20 * t88
      t93 = t28 * x4
      t101 = t2 * t40 * t52
      t102 = t28 * t88
      t106 = t4 ** 2
      t107 = t33 * t106
      t108 = t7 ** 2
      t110 = t107 * t108 * t51
      t111 = x4 ** 2
      t112 = t45 * t111
      t123 = t2 * t3 * t6
      t124 = t18 * t27
      t128 = t5 * t7
      t129 = t51 * t70
      t133 = -0.275D3 * t9 * t19 * t28 * t24 - 0.430D3 * t35 * t36 * t18
     # - 0.192D3 * t44 * t19 * t45 * t46 + 0.14832D5 * t53 * t55 * t56 -
     # 0.144D3 * t61 * t62 * t56 + 0.14976D5 * t53 * t55 * t66 - 0.328D3
     # * t53 * t71 * t66 + 0.328D3 * t61 * t19 * t66 + 0.14976D5 * t53 *
     # t62 * t89 - 0.122D3 * t9 * t19 * t93 + 0.14976D5 * t9 * t62 * t93
     # - 0.14976D5 * t101 * t62 * t102 + 0.144D3 * t110 * t55 * t112 - 0
     #.400D3 * t53 * t71 * t56 + 0.530D3 * t61 * t19 * t56 + 0.306D3 * t
     #123 * t36 * t124 - 0.528D3 * t128 * t129 * t124
      t134 = t51 * t54
      t138 = x1 * t14
      t142 = t45 * t24
      t143 = t142 * x4
      t147 = t60 * t7
      t158 = t51 ** 2
      t159 = t54 ** 2
      t165 = t51 * x1
      t177 = t45 * x4 * t88
      t201 = t88 ** 2
      t218 = 0.14976D5 * t128 * t134 * t124 - 0.14976D5 * t123 * t138 * 
     #t124 + 0.144D3 * t44 * t62 * t143 - 0.14832D5 * t147 * t134 * t18 
     #+ 0.584D3 * t147 * t129 * t18 + 0.144D3 * t35 * t138 * t18 + 0.144
     #D3 * t107 * t108 * t158 * t159 * t18 - 0.56D2 * t41 * t42 * t165 *
     # t70 * x3 * t18 - 0.576D3 * t107 * t108 * t165 * t55 * t89 - 0.148
     #32D5 * t110 * t62 * t177 - 0.14832D5 * t110 * t62 * t142 * t88 - 0
     #.288D3 * t33 * t106 * t3 * t108 * t6 * t165 * t55 * t177 - 0.17D2 
     #* t101 * t19 * t102 + 0.144D3 * t33 * t106 * t4 * t108 * t7 * t158
     # * t55 * t45 * t201 - 0.10D2 * t44 * t19 * t112 - 0.288D3 * t61 * 
     #t62 * t66 + 0.144D3 * t44 * t62 * t112 - 0.182D3 * t44 * t19 * t14
     #3
      rrgq2qgh22J1 = -wd * (t133 + t218) / t1 / t18 / t27 / z / 0.314159
     #2653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t7 = t2 * t5 * t3
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t11 = t10 * t8
      t12 = x1 ** 2
      t13 = t12 * x1
      t15 = t7 * t11 * t13
      t16 = 0.1D1 - x3
      t17 = t16 ** 2
      t18 = s * t3
      t19 = x1 * t8
      t25 = (s - t18 * t19 * x3 - t18 * t19 * t16) ** 2
      t26 = t17 * t25
      t27 = 0.1D1 - x1
      t28 = t27 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t35 = 0.1D1 - x4
      t38 = Sqrt(x3 * t16 * x4 * t35)
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t38
      t42 = t28 * x4 * t41
      t44 = t15 * t26 * t42
      t46 = t4 * t3
      t47 = t2 * t46
      t48 = t9 * t8
      t49 = t48 * t12
      t50 = t47 * t49
      t51 = t16 * t25
      t52 = t27 * t41
      t54 = t50 * t51 * t52
      t56 = t2 * t5
      t58 = t56 * t10 * t13
      t60 = t58 * t26 * t52
      t63 = t56 * t10 * t12
      t65 = t63 * t51 * t42
      t67 = t28 * t35
      t68 = t67 * t41
      t70 = t63 * t51 * t68
      t73 = t47 * t48 * x1
      t74 = x3 * t25
      t75 = t67 * x4
      t77 = t73 * t74 * t75
      t80 = t73 * t51 * t75
      t83 = t2 * t3 * t8
      t84 = x1 * x3
      t86 = t83 * t84 * t25
      t88 = t2 * t4
      t89 = t88 * t9
      t90 = t12 * t17
      t92 = t89 * t90 * t25
      t94 = t1 * s
      t95 = t94 * t4
      t96 = x1 * t9
      t97 = t95 * t96
      t98 = t8 * t27
      t103 = s - t18 * t98 * x4 - t18 * t98 * t35
      t104 = t103 * t27
      t105 = t104 * x4
      t107 = t97 * t74 * t105
      t110 = t97 * t51 * t105
      t113 = t94 * t46 * t49
      t114 = t104 * t41
      t116 = t113 * t51 * t114
      t118 = t35 ** 2
      t119 = t28 * t118
      t121 = t73 * t74 * t119
      t123 = t27 * t35
      t125 = t50 * t26 * t123
      t127 = t88 * t96
      t129 = t127 * t51 * t123
      t131 = t27 * x4
      t133 = t50 * t26 * t131
      t135 = x3 ** 2
      t136 = t12 * t135
      t138 = t89 * t136 * t25
      t140 = -0.288D3 * t44 + 0.14976D5 * t54 - 0.576D3 * t60 - 0.14832D
     #5 * t65 - 0.14832D5 * t70 - 0.182D3 * t77 + 0.144D3 * t80 - 0.430D
     #3 * t86 - 0.14832D5 * t92 - 0.122D3 * t107 + 0.14976D5 * t110 - 0.
     #14976D5 * t116 - 0.192D3 * t121 + 0.14832D5 * t125 - 0.144D3 * t12
     #9 + 0.14976D5 * t133 + 0.584D3 * t138
      t141 = x1 * t16
      t143 = t83 * t141 * t25
      t146 = t12 ** 2
      t147 = t17 ** 2
      t151 = 0.144D3 * t56 * t10 * t146 * t147 * t25
      t152 = t47 * t48
      t156 = t152 * t13 * t135 * x3 * t25
      t158 = t135 * t25
      t160 = t50 * t158 * t131
      t163 = t127 * t74 * t131
      t165 = x4 ** 2
      t166 = t28 * t165
      t169 = 0.144D3 * t63 * t26 * t166
      t171 = t50 * t158 * t123
      t174 = t127 * t74 * t123
      t176 = t95 * t9
      t177 = t25 * t103
      t179 = t176 * t136 * t177
      t182 = t176 * t90 * t177
      t185 = t94 * t3 * t8
      t187 = t185 * t141 * t177
      t190 = t73 * t51 * t166
      t193 = t185 * t84 * t177
      t196 = t113 * t74 * t114
      t203 = t41 ** 2
      t204 = t28 * t203
      t207 = 0.144D3 * t2 * t5 * t4 * t10 * t9 * t146 * t26 * t204
      t209 = t73 * t74 * t166
      t212 = t127 * t51 * t131
      t214 = t104 * t35
      t216 = t97 * t74 * t214
      t218 = 0.144D3 * t143 + t151 - 0.56D2 * t156 - 0.328D3 * t160 + 0.
     #328D3 * t163 + t169 - 0.400D3 * t171 + 0.530D3 * t174 - 0.528D3 * 
     #t179 + 0.14976D5 * t182 - 0.14976D5 * t187 + 0.144D3 * t190 + 0.30
     #6D3 * t193 - 0.17D2 * t196 + t207 - 0.10D2 * t209 - 0.288D3 * t212
     # - 0.275D3 * t216
      t236 = 0.432D3 * t44 - 0.15552D5 * t54 + 0.864D3 * t60 + 0.15696D5
     # * t65 + 0.15552D5 * t70 + 0.108D3 * t77 - 0.2160D4 * t80 + 0.542D
     #3 * t86 + 0.15696D5 * t92 - 0.16D2 * t113 * t158 * t214 - 0.3096D4
     # * t73 * t51 * t119
      t260 = t17 * t16
      t261 = t260 * t25
      t267 = 0.256D3 * t63 * t74 * t68 - 0.136D3 * t15 * t74 * t204 + 0.
     #134D3 * t107 - 0.14616D5 * t110 + 0.14616D5 * t116 - 0.6192D4 * t9
     #7 * t51 * t214 - 0.256D3 * t50 * t74 * t52 + 0.216D3 * t58 * t158 
     #* t52 + 0.256D3 * t63 * t74 * t42 + 0.288D3 * t7 * t11 * t146 * t2
     #61 * t52 + 0.30D2 * t121 - 0.15552D5 * t125
      t283 = 0.2160D4 * t129 - 0.15552D5 * t133 - 0.544D3 * t138 + 0.936
     #D3 * t143 - t151 - 0.24D2 * t156 - 0.432D3 * t15 * t51 * t204 - 0.
     #432D3 * t58 * t261 * t131 + 0.256D3 * t160 - 0.604D3 * t163 - t169
     # + 0.288D3 * t171
      t298 = -0.444D3 * t174 + 0.752D3 * t179 - 0.14616D5 * t182 + 0.146
     #16D5 * t187 + 0.936D3 * t190 - 0.190D3 * t193 + 0.490D3 * t196 - t
     #207 + 0.62D2 * t209 - 0.1872D4 * t212 + 0.156D3 * t216 - 0.432D3 *
     # t152 * t13 * t260 * t25
      rrgq2qgh22J2 = -(wd * (t140 + t218) + wd * (t236 + t267 + t283 + t
     #298)) / t1 / t25 / t103 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t7 * x1
      t9 = t5 * t8
      t10 = 0.1D1 - x3
      t11 = s * t3
      t12 = t6 * x1
      t18 = (s - t11 * t12 * x3 - t11 * t12 * t10) ** 2
      t19 = t10 * t18
      t20 = 0.1D1 - x1
      t21 = t20 * x4
      t23 = t9 * t19 * t21
      t25 = t4 * t3
      t26 = t2 * t25
      t27 = t7 * t6
      t29 = t26 * t27 * x1
      t30 = t20 ** 2
      t31 = x4 ** 2
      t32 = t30 * t31
      t34 = t29 * t19 * t32
      t36 = x3 * t18
      t37 = 0.1D1 - x4
      t38 = t30 * t37
      t39 = t38 * x4
      t41 = t29 * t36 * t39
      t44 = t29 * t19 * t39
      t46 = t4 ** 2
      t47 = t2 * t46
      t48 = t7 ** 2
      t49 = x1 ** 2
      t51 = t47 * t48 * t49
      t55 = cos(x2 * 0.3141592653589793D1)
      t59 = Sqrt(x3 * t10 * x4 * t37)
      t62 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t55 * t59
      t63 = t38 * t62
      t65 = t51 * t19 * t63
      t68 = t2 * t46 * t3
      t69 = t48 * t6
      t70 = t49 * x1
      t72 = t68 * t69 * t70
      t73 = t10 ** 2
      t74 = t73 * t18
      t76 = t30 * x4 * t62
      t78 = t72 * t74 * t76
      t80 = t27 * t49
      t81 = t26 * t80
      t82 = t20 * t62
      t84 = t81 * t19 * t82
      t86 = t48 * t70
      t87 = t47 * t86
      t89 = t87 * t74 * t82
      t91 = t5 * t7
      t92 = x3 ** 2
      t93 = t49 * t92
      t95 = t91 * t93 * t18
      t98 = t2 * t3 * t6
      t99 = x1 * t10
      t101 = t98 * t99 * t18
      t104 = t49 ** 2
      t105 = t73 ** 2
      t109 = 0.144D3 * t47 * t48 * t104 * t105 * t18
      t110 = t26 * t27
      t111 = t92 * x3
      t112 = t70 * t111
      t114 = t110 * t112 * t18
      t116 = t1 * s
      t118 = t116 * t3 * t6
      t119 = t6 * t20
      t124 = s - t11 * t119 * x4 - t11 * t119 * t37
      t125 = t18 * t124
      t127 = t118 * t99 * t125
      t129 = t116 * t4
      t130 = t129 * t7
      t131 = t49 * t73
      t133 = t130 * t131 * t125
      t136 = t130 * t93 * t125
      t138 = x1 * x3
      t140 = t118 * t138 * t125
      t142 = t129 * t8
      t143 = t124 * t20
      t144 = t143 * x4
      t146 = t142 * t36 * t144
      t148 = -0.288D3 * t23 + 0.144D3 * t34 - 0.182D3 * t41 + 0.144D3 * 
     #t44 - 0.14832D5 * t65 - 0.288D3 * t78 + 0.14976D5 * t84 - 0.576D3 
     #* t89 + 0.584D3 * t95 + 0.144D3 * t101 + t109 - 0.56D2 * t114 - 0.
     #14976D5 * t127 + 0.14976D5 * t133 - 0.528D3 * t136 + 0.306D3 * t14
     #0 - 0.122D3 * t146
      t150 = t142 * t19 * t144
      t152 = t116 * t25
      t153 = t152 * t80
      t154 = t143 * t62
      t156 = t153 * t19 * t154
      t159 = t98 * t138 * t18
      t162 = t91 * t131 * t18
      t164 = t37 ** 2
      t165 = t30 * t164
      t167 = t29 * t36 * t165
      t169 = t20 * t37
      t171 = t81 * t74 * t169
      t174 = t9 * t19 * t169
      t177 = t81 * t74 * t21
      t179 = t92 * t18
      t181 = t81 * t179 * t21
      t184 = t9 * t36 * t21
      t188 = 0.144D3 * t51 * t74 * t32
      t190 = t81 * t179 * t169
      t193 = t9 * t36 * t169
      t196 = t153 * t36 * t154
      t203 = t62 ** 2
      t204 = t30 * t203
      t207 = 0.144D3 * t2 * t46 * t4 * t48 * t7 * t104 * t74 * t204
      t209 = t29 * t36 * t32
      t212 = t51 * t19 * t76
      t214 = t143 * t37
      t216 = t142 * t36 * t214
      t218 = 0.14976D5 * t150 - 0.14976D5 * t156 - 0.430D3 * t159 - 0.14
     #832D5 * t162 - 0.192D3 * t167 + 0.14832D5 * t171 - 0.144D3 * t174 
     #+ 0.14976D5 * t177 - 0.328D3 * t181 + 0.328D3 * t184 + t188 - 0.40
     #0D3 * t190 + 0.530D3 * t193 - 0.17D2 * t196 + t207 - 0.10D2 * t209
     # - 0.14832D5 * t212 - 0.275D3 * t216
      t229 = t81 * t36 * t82
      t232 = t87 * t179 * t82
      t235 = t51 * t36 * t76
      t240 = -0.1872D4 * t23 + 0.936D3 * t34 + 0.108D3 * t41 - 0.2160D4 
     #* t44 - 0.136D3 * t72 * t36 * t204 - 0.256D3 * t229 + 0.216D3 * t2
     #32 + 0.256D3 * t235 + 0.15552D5 * t65 + 0.432D3 * t78 - 0.15552D5 
     #* t84
      t245 = t73 * t10
      t258 = 0.864D3 * t89 - 0.544D3 * t95 + 0.936D3 * t101 - t109 - 0.2
     #4D2 * t114 - 0.432D3 * t110 * t70 * t245 * t18 + 0.14616D5 * t127 
     #- 0.14616D5 * t133 + 0.752D3 * t136 - 0.190D3 * t140 - 0.6192D4 * 
     #t142 * t19 * t214 + 0.134D3 * t146
      t266 = 0.16D2 * t153 * t179 * t214
      t271 = t51 * t36 * t63
      t278 = -0.14616D5 * t150 + 0.14616D5 * t156 + 0.542D3 * t159 + 0.1
     #5696D5 * t162 - t266 - 0.3096D4 * t29 * t19 * t165 + 0.256D3 * t27
     #1 + 0.30D2 * t167 - 0.15552D5 * t171 + 0.2160D4 * t174 - 0.15552D5
     # * t177 + 0.256D3 * t181
      t282 = t245 * t18
      t298 = -0.604D3 * t184 - t188 + 0.288D3 * t190 - 0.444D3 * t193 - 
     #0.432D3 * t87 * t282 * t21 + 0.490D3 * t196 - t207 + 0.62D2 * t209
     # + 0.288D3 * t68 * t69 * t104 * t282 * t82 - 0.432D3 * t72 * t19 *
     # t204 + 0.15696D5 * t212 + 0.156D3 * t216
      t325 = 0.16D2 * t87 * t111 * t20 * x4 * t18 + 0.29128D5 * t41 - 0.
     #25264D5 * t229 + 0.80D2 * t232 + 0.25296D5 * t235 + 0.16D2 * t51 *
     # t92 * t30 * t31 * t18 + 0.74208D5 * t95 - 0.32D2 * t114 - 0.74296
     #D5 * t136 + 0.28224D5 * t140 + 0.32D2 * t152 * t27 * t112 * t125 +
     # 0.304D3 * t146
      t344 = -0.28208D5 * t159 - t266 + 0.25248D5 * t271 + 0.28792D5 * t
     #167 - 0.74320D5 * t181 + 0.27920D5 * t184 - 0.74288D5 * t190 - 0.5
     #36D3 * t193 + 0.25220D5 * t196 + 0.304D3 * t209 - 0.16D2 * t116 * 
     #t46 * t86 * t179 * t154 + 0.16D2 * t153 * t179 * t144 + 0.28372D5 
     #* t216
      rrgq2qgh22J3 = -(wd * (t148 + t218) + wd * (t240 + t258 + t278 + t
     #298) + wd * (t325 + t344)) / t1 / t18 / t124 / z / 0.3141592653589
     #793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = x1 ** 2
      t13 = t12 ** 2
      t16 = 0.1D1 - x3
      t17 = t16 ** 2
      t18 = s * t3
      t19 = x1 * t8
      t25 = (s - t18 * t19 * x3 - t18 * t19 * t16) ** 2
      t26 = t17 * t25
      t27 = 0.1D1 - x1
      t28 = t27 ** 2
      t32 = cos(x2 * 0.3141592653589793D1)
      t34 = 0.1D1 - x4
      t37 = Sqrt(x3 * t16 * x4 * t34)
      t40 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t32 * t37
      t41 = t40 ** 2
      t42 = t28 * t41
      t45 = 0.144D3 * t2 * t5 * t4 * t10 * t9 * t13 * t26 * t42
      t46 = t4 * t3
      t47 = t2 * t46
      t48 = t9 * t8
      t50 = t47 * t48 * x1
      t51 = x3 * t25
      t52 = x4 ** 2
      t53 = t28 * t52
      t55 = t50 * t51 * t53
      t57 = t1 * s
      t58 = t57 * t46
      t59 = t48 * t12
      t60 = t58 * t59
      t61 = t16 * t25
      t62 = t8 * t27
      t67 = s - t18 * t62 * x4 - t18 * t62 * t34
      t68 = t67 * t27
      t69 = t68 * t40
      t71 = t60 * t61 * t69
      t73 = t57 * t4
      t74 = x1 * t9
      t75 = t73 * t74
      t76 = t68 * t34
      t78 = t75 * t51 * t76
      t81 = t60 * t51 * t69
      t83 = t47 * t59
      t84 = t27 * t34
      t86 = t83 * t26 * t84
      t88 = t2 * t4
      t89 = t88 * t74
      t91 = t89 * t61 * t84
      t93 = t27 * x4
      t95 = t83 * t26 * t93
      t97 = x3 ** 2
      t98 = t97 * t25
      t100 = t83 * t98 * t93
      t103 = t89 * t51 * t93
      t105 = t2 * t5
      t107 = t105 * t10 * t12
      t110 = 0.144D3 * t107 * t26 * t53
      t112 = t83 * t98 * t84
      t115 = t57 * t3 * t8
      t116 = x1 * t16
      t117 = t25 * t67
      t119 = t115 * t116 * t117
      t121 = t73 * t9
      t122 = t12 * t17
      t124 = t121 * t122 * t117
      t126 = t12 * t97
      t128 = t121 * t126 * t117
      t130 = x1 * x3
      t132 = t115 * t130 * t117
      t135 = t2 * t3 * t8
      t137 = t135 * t130 * t25
      t139 = t45 - 0.10D2 * t55 - 0.14976D5 * t71 - 0.275D3 * t78 - 0.17
     #D2 * t81 + 0.14832D5 * t86 - 0.144D3 * t91 + 0.14976D5 * t95 - 0.3
     #28D3 * t100 + 0.328D3 * t103 + t110 - 0.400D3 * t112 - 0.14976D5 *
     # t119 + 0.14976D5 * t124 - 0.528D3 * t128 + 0.306D3 * t132 - 0.430
     #D3 * t137
      t140 = t88 * t9
      t142 = t140 * t122 * t25
      t145 = t140 * t126 * t25
      t148 = t135 * t116 * t25
      t151 = t17 ** 2
      t155 = 0.144D3 * t105 * t10 * t13 * t151 * t25
      t156 = t47 * t48
      t157 = t12 * x1
      t158 = t97 * x3
      t159 = t157 * t158
      t161 = t156 * t159 * t25
      t163 = t28 * t34
      t164 = t163 * t40
      t166 = t107 * t61 * t164
      t169 = t2 * t5 * t3
      t170 = t10 * t8
      t172 = t169 * t170 * t157
      t174 = t28 * x4 * t40
      t176 = t172 * t26 * t174
      t178 = t27 * t40
      t180 = t83 * t61 * t178
      t182 = t10 * t157
      t183 = t105 * t182
      t185 = t183 * t26 * t178
      t187 = t68 * x4
      t189 = t75 * t51 * t187
      t192 = t89 * t61 * t93
      t195 = t50 * t61 * t53
      t197 = t163 * x4
      t199 = t50 * t51 * t197
      t202 = t50 * t61 * t197
      t204 = t34 ** 2
      t205 = t28 * t204
      t207 = t50 * t51 * t205
      t210 = t75 * t61 * t187
      t213 = t89 * t51 * t84
      t216 = t107 * t61 * t174
      t218 = -0.14832D5 * t142 + 0.584D3 * t145 + 0.144D3 * t148 + t155 
     #- 0.56D2 * t161 - 0.14832D5 * t166 - 0.288D3 * t176 + 0.14976D5 * 
     #t180 - 0.576D3 * t185 - 0.122D3 * t189 - 0.288D3 * t192 + 0.144D3 
     #* t195 - 0.182D3 * t199 + 0.144D3 * t202 - 0.192D3 * t207 + 0.1497
     #6D5 * t210 + 0.530D3 * t213 - 0.14832D5 * t216
      t228 = t17 * t16
      t229 = t228 * t25
      t237 = -t45 + 0.62D2 * t55 + 0.14616D5 * t71 + 0.156D3 * t78 + 0.4
     #90D3 * t81 - 0.432D3 * t172 * t61 * t42 - 0.432D3 * t183 * t229 * 
     #t93 - 0.15552D5 * t86 + 0.2160D4 * t91 - 0.15552D5 * t95 + 0.256D3
     # * t100
      t251 = -0.604D3 * t103 - t110 + 0.288D3 * t112 - 0.6192D4 * t75 * 
     #t61 * t76 + 0.14616D5 * t119 - 0.14616D5 * t124 + 0.752D3 * t128 -
     # 0.190D3 * t132 + 0.542D3 * t137 + 0.15696D5 * t142 - 0.544D3 * t1
     #45 + 0.936D3 * t148
      t267 = -t155 - 0.24D2 * t161 - 0.432D3 * t156 * t157 * t228 * t25 
     #+ 0.15552D5 * t166 + 0.432D3 * t176 - 0.15552D5 * t180 + 0.864D3 *
     # t185 + 0.134D3 * t189 - 0.1872D4 * t192 + 0.936D3 * t195 + 0.108D
     #3 * t199 - 0.2160D4 * t202
      t273 = t107 * t51 * t164
      t279 = t83 * t51 * t178
      t282 = t183 * t98 * t178
      t285 = t107 * t51 * t174
      t295 = 0.16D2 * t60 * t98 * t76
      t298 = 0.30D2 * t207 - 0.3096D4 * t50 * t61 * t205 + 0.256D3 * t27
     #3 - 0.136D3 * t172 * t51 * t42 - 0.256D3 * t279 + 0.216D3 * t282 +
     # 0.256D3 * t285 + 0.288D3 * t169 * t170 * t13 * t229 * t178 - 0.14
     #616D5 * t210 - t295 - 0.444D3 * t213 + 0.15696D5 * t216
      t321 = 0.16D2 * t183 * t158 * t27 * x4 * t25 + 0.304D3 * t55 + 0.2
     #8372D5 * t78 + 0.25220D5 * t81 - 0.74320D5 * t100 + 0.27920D5 * t1
     #03 - 0.74288D5 * t112 - 0.74296D5 * t128 + 0.28224D5 * t132 + 0.32
     #D2 * t58 * t48 * t159 * t117 - 0.28208D5 * t137 + 0.74208D5 * t145
      t344 = -0.32D2 * t161 + 0.16D2 * t107 * t97 * t28 * t52 * t25 - 0.
     #16D2 * t57 * t5 * t182 * t98 * t69 + 0.304D3 * t189 + 0.29128D5 * 
     #t199 + 0.28792D5 * t207 + 0.25248D5 * t273 - 0.25264D5 * t279 + 0.
     #80D2 * t282 + 0.25296D5 * t285 + 0.16D2 * t60 * t98 * t187 - t295 
     #- 0.536D3 * t213
      rrgq2qgh22J4 = -(wd * (t139 + t218) + wd * (t237 + t251 + t267 + t
     #298) + wd * (t321 + t344)) / t1 / t25 / t67 / z / 0.31415926535897
     #93D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = x1 ** 2
      t11 = t9 * t10
      t12 = t6 * t11
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t15 = s * t3
      t16 = t7 * x1
      t22 = (s - t15 * t16 * x3 - t15 * t16 * t13) ** 2
      t23 = t14 * t22
      t24 = 0.1D1 - x1
      t25 = t24 * x4
      t27 = t12 * t23 * t25
      t29 = x3 ** 2
      t30 = t29 * t22
      t32 = t12 * t30 * t25
      t34 = t2 * t4
      t35 = x1 * t8
      t36 = t34 * t35
      t37 = x3 * t22
      t39 = t36 * t37 * t25
      t41 = t4 ** 2
      t42 = t2 * t41
      t43 = t8 ** 2
      t45 = t42 * t43 * t10
      t46 = t24 ** 2
      t47 = x4 ** 2
      t48 = t46 * t47
      t51 = 0.144D3 * t45 * t23 * t48
      t52 = 0.1D1 - x4
      t53 = t24 * t52
      t55 = t12 * t30 * t53
      t58 = t36 * t37 * t53
      t60 = t13 * t22
      t65 = cos(x2 * 0.3141592653589793D1)
      t69 = Sqrt(x3 * t13 * x4 * t52)
      t72 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t65 * t69
      t73 = t46 * x4 * t72
      t75 = t45 * t60 * t73
      t77 = t46 * t52
      t78 = t77 * t72
      t80 = t45 * t60 * t78
      t83 = t2 * t41 * t3
      t84 = t43 * t7
      t85 = x1 * t10
      t87 = t83 * t84 * t85
      t89 = t87 * t23 * t73
      t91 = t24 * t72
      t93 = t12 * t60 * t91
      t96 = t2 * t3 * t7
      t97 = x1 * x3
      t99 = t96 * t97 * t22
      t101 = t34 * t8
      t102 = t10 * t14
      t104 = t101 * t102 * t22
      t106 = t10 * t29
      t108 = t101 * t106 * t22
      t110 = x1 * t13
      t112 = t96 * t110 * t22
      t115 = t10 ** 2
      t116 = t14 ** 2
      t120 = 0.144D3 * t42 * t43 * t115 * t116 * t22
      t121 = t6 * t9
      t122 = t29 * x3
      t123 = t85 * t122
      t125 = t121 * t123 * t22
      t127 = t1 * s
      t128 = t127 * t5
      t129 = t128 * t11
      t130 = t7 * t24
      t135 = s - t15 * t130 * x4 - t15 * t130 * t52
      t136 = t135 * t24
      t137 = t136 * t72
      t139 = t129 * t60 * t137
      t141 = 0.14976D5 * t27 - 0.328D3 * t32 + 0.328D3 * t39 + t51 - 0.4
     #00D3 * t55 + 0.530D3 * t58 - 0.14832D5 * t75 - 0.14832D5 * t80 - 0
     #.288D3 * t89 + 0.14976D5 * t93 - 0.430D3 * t99 - 0.14832D5 * t104 
     #+ 0.584D3 * t108 + 0.144D3 * t112 + t120 - 0.56D2 * t125 - 0.14976
     #D5 * t139
      t142 = t43 * t85
      t143 = t42 * t142
      t145 = t143 * t23 * t91
      t147 = t127 * t4
      t148 = t147 * t35
      t149 = t136 * x4
      t151 = t148 * t37 * t149
      t154 = t148 * t60 * t149
      t157 = t6 * t9 * x1
      t158 = t52 ** 2
      t159 = t46 * t158
      t161 = t157 * t37 * t159
      t164 = t12 * t23 * t53
      t167 = t36 * t60 * t53
      t169 = t136 * t52
      t171 = t148 * t37 * t169
      t174 = t129 * t37 * t137
      t181 = t72 ** 2
      t182 = t46 * t181
      t185 = 0.144D3 * t2 * t41 * t4 * t43 * t8 * t115 * t23 * t182
      t187 = t157 * t37 * t48
      t190 = t36 * t60 * t25
      t193 = t157 * t60 * t48
      t195 = t77 * x4
      t197 = t157 * t37 * t195
      t200 = t157 * t60 * t195
      t203 = t127 * t3 * t7
      t204 = t22 * t135
      t206 = t203 * t110 * t204
      t208 = t147 * t8
      t210 = t208 * t102 * t204
      t213 = t208 * t106 * t204
      t216 = t203 * t97 * t204
      t218 = -0.576D3 * t145 - 0.122D3 * t151 + 0.14976D5 * t154 - 0.192
     #D3 * t161 + 0.14832D5 * t164 - 0.144D3 * t167 - 0.275D3 * t171 - 0
     #.17D2 * t174 + t185 - 0.10D2 * t187 - 0.288D3 * t190 + 0.144D3 * t
     #193 - 0.182D3 * t197 + 0.144D3 * t200 - 0.14976D5 * t206 + 0.14976
     #D5 * t210 - 0.528D3 * t213 + 0.306D3 * t216
      t231 = -0.15552D5 * t27 + 0.256D3 * t32 - 0.604D3 * t39 - t51 + 0.
     #288D3 * t55 - 0.444D3 * t58 + 0.15696D5 * t75 + 0.15552D5 * t80 + 
     #0.432D3 * t89 - 0.15552D5 * t93 + 0.542D3 * t99
      t236 = t14 * t13
      t249 = 0.15696D5 * t104 - 0.544D3 * t108 + 0.936D3 * t112 - t120 -
     # 0.24D2 * t125 - 0.432D3 * t121 * t85 * t236 * t22 + 0.14616D5 * t
     #139 + 0.864D3 * t145 + 0.134D3 * t151 - 0.14616D5 * t154 - 0.6192D
     #4 * t148 * t60 * t169 + 0.30D2 * t161
      t255 = 0.16D2 * t129 * t30 * t169
      t260 = t45 * t37 * t78
      t266 = t12 * t37 * t91
      t269 = t143 * t30 * t91
      t272 = t45 * t37 * t73
      t276 = t236 * t22
      t286 = -0.15552D5 * t164 + 0.2160D4 * t167 - t255 - 0.3096D4 * t15
     #7 * t60 * t159 + 0.256D3 * t260 - 0.136D3 * t87 * t37 * t182 - 0.2
     #56D3 * t266 + 0.216D3 * t269 + 0.256D3 * t272 + 0.288D3 * t83 * t8
     #4 * t115 * t276 * t91 - 0.432D3 * t87 * t60 * t182 - 0.432D3 * t14
     #3 * t276 * t25
      t298 = 0.156D3 * t171 + 0.490D3 * t174 - t185 + 0.62D2 * t187 - 0.
     #1872D4 * t190 + 0.936D3 * t193 + 0.108D3 * t197 - 0.2160D4 * t200 
     #+ 0.14616D5 * t206 - 0.14616D5 * t210 + 0.752D3 * t213 - 0.190D3 *
     # t216
      t329 = -0.74320D5 * t32 + 0.27920D5 * t39 - 0.74288D5 * t55 - 0.53
     #6D3 * t58 - 0.28208D5 * t99 + 0.74208D5 * t108 - 0.32D2 * t125 + 0
     #.16D2 * t143 * t122 * t24 * x4 * t22 + 0.16D2 * t45 * t29 * t46 * 
     #t47 * t22 - 0.16D2 * t127 * t41 * t142 * t30 * t137 + 0.304D3 * t1
     #51 + 0.32D2 * t128 * t9 * t123 * t204
      t344 = 0.16D2 * t129 * t30 * t149 + 0.28792D5 * t161 - t255 + 0.25
     #248D5 * t260 - 0.25264D5 * t266 + 0.80D2 * t269 + 0.25296D5 * t272
     # + 0.28372D5 * t171 + 0.25220D5 * t174 + 0.304D3 * t187 + 0.29128D
     #5 * t197 - 0.74296D5 * t213 + 0.28224D5 * t216
      rrgq2qgh22J5 = -(wd * (t141 + t218) + wd * (t231 + t249 + t286 + t
     #298) + wd * (t329 + t344)) / t1 / t22 / t135 / z / 0.3141592653589
     #793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t5 = 0.1D1 - z
      t6 = t2 * t3 * t5
      t7 = x1 * x3
      t8 = s * t3
      t9 = t5 * x1
      t12 = 0.1D1 - x3
      t16 = (s - t8 * t9 * x3 - t8 * t9 * t12) ** 2
      t17 = 0.1D1 - x1
      t18 = t5 * t17
      t21 = 0.1D1 - x4
      t24 = s - t8 * t18 * x4 - t8 * t18 * t21
      t25 = t16 * t24
      t27 = t6 * t7 * t25
      t29 = t3 ** 2
      t30 = t2 * t29
      t31 = t5 ** 2
      t32 = t30 * t31
      t33 = x1 ** 2
      t34 = t12 ** 2
      t35 = t33 * t34
      t39 = x1 * t12
      t43 = t1 ** 2
      t45 = t43 * t3 * t5
      t49 = t43 * t29
      t50 = t49 * t31
      t51 = x3 ** 2
      t52 = t33 * t51
      t54 = t50 * t52 * t16
      t59 = t31 * x1
      t60 = t30 * t59
      t61 = x3 * t16
      t62 = t24 * t17
      t63 = t62 * t21
      t65 = t60 * t61 * t63
      t68 = t32 * t52 * t25
      t70 = t29 * t3
      t71 = t43 * t70
      t72 = t31 * t5
      t73 = t72 * t33
      t74 = t71 * t73
      t75 = t34 * t16
      t76 = t17 * x4
      t80 = t12 * t16
      t84 = t62 * x4
      t88 = -0.190D3 * t27 - 0.14616D5 * t32 * t35 * t25 + 0.14616D5 * t
     #6 * t39 * t25 + 0.936D3 * t45 * t39 * t16 - 0.544D3 * t54 + 0.1569
     #6D5 * t50 * t35 * t16 + 0.156D3 * t65 + 0.752D3 * t68 - 0.15552D5 
     #* t74 * t75 * t76 - 0.6192D4 * t60 * t80 * t63 - 0.14616D5 * t60 *
     # t80 * t84
      t89 = t2 * t70
      t90 = t89 * t73
      t94 = cos(x2 * 0.3141592653589793D1)
      t98 = Sqrt(x3 * t12 * x4 * t21)
      t101 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t94 * t98
      t102 = t62 * t101
      t107 = t45 * t7 * t16
      t109 = t29 ** 2
      t110 = t43 * t109
      t111 = t31 ** 2
      t112 = t33 * x1
      t113 = t111 * t112
      t114 = t110 * t113
      t115 = t17 * t101
      t120 = t43 * t109 * t3
      t121 = t111 * t5
      t123 = t120 * t121 * t112
      t124 = t17 ** 2
      t126 = t124 * x4 * t101
      t136 = t33 ** 2
      t139 = t101 ** 2
      t140 = t124 * t139
      t145 = t71 * t72 * x1
      t146 = x4 ** 2
      t147 = t124 * t146
      t149 = t145 * t61 * t147
      t152 = t90 * t61 * t102
      t154 = t49 * t59
      t159 = t110 * t111 * t33
      t160 = t124 * t21
      t161 = t160 * t101
      t163 = t159 * t61 * t161
      t165 = t51 * t16
      t168 = 0.16D2 * t90 * t165 * t63
      t169 = t21 ** 2
      t170 = t124 * t169
      t174 = 0.14616D5 * t90 * t80 * t102 + 0.542D3 * t107 + 0.864D3 * t
     #114 * t75 * t115 + 0.432D3 * t123 * t75 * t126 - 0.15552D5 * t74 *
     # t80 * t115 - 0.144D3 * t43 * t109 * t29 * t111 * t31 * t136 * t75
     # * t140 + 0.62D2 * t149 + 0.490D3 * t152 - 0.1872D4 * t154 * t80 *
     # t76 + 0.256D3 * t163 - t168 - 0.3096D4 * t145 * t80 * t170
      t177 = t74 * t165 * t76
      t181 = t34 * t12
      t182 = t181 * t16
      t187 = t60 * t61 * t84
      t190 = t154 * t61 * t76
      t198 = t160 * x4
      t200 = t145 * t61 * t198
      t202 = t17 * t21
      t213 = t145 * t61 * t170
      t218 = 0.256D3 * t177 + 0.288D3 * t120 * t121 * t136 * t182 * t115
     # + 0.134D3 * t187 - 0.604D3 * t190 - 0.144D3 * t159 * t75 * t147 +
     # 0.936D3 * t145 * t80 * t147 + 0.108D3 * t200 - 0.15552D5 * t74 * 
     #t75 * t202 + 0.2160D4 * t154 * t80 * t202 - 0.2160D4 * t145 * t80 
     #* t198 + 0.30D2 * t213 + 0.15552D5 * t159 * t80 * t161
      t220 = t74 * t165 * t202
      t223 = t154 * t61 * t202
      t238 = t74 * t61 * t115
      t241 = t114 * t165 * t115
      t244 = t159 * t61 * t126
      t246 = t71 * t72
      t251 = t51 * x3
      t252 = t112 * t251
      t254 = t246 * t252 * t16
      t257 = t34 ** 2
      t262 = 0.288D3 * t220 - 0.444D3 * t223 + 0.15696D5 * t159 * t80 * 
     #t126 - 0.432D3 * t123 * t80 * t140 - 0.432D3 * t114 * t182 * t76 -
     # 0.136D3 * t123 * t61 * t140 - 0.256D3 * t238 + 0.216D3 * t241 + 0
     #.256D3 * t244 - 0.432D3 * t246 * t112 * t181 * t16 - 0.24D2 * t254
     # - 0.144D3 * t110 * t111 * t136 * t257 * t16
      t295 = 0.32D2 * t89 * t72 * t252 * t25 + 0.28224D5 * t27 + 0.74208
     #D5 * t54 + 0.28372D5 * t65 - 0.74296D5 * t68 - 0.28208D5 * t107 + 
     #0.304D3 * t149 + 0.25220D5 * t152 + 0.16D2 * t90 * t165 * t84 + 0.
     #16D2 * t114 * t251 * t17 * x4 * t16 + 0.16D2 * t159 * t51 * t124 *
     # t146 * t16 - 0.16D2 * t2 * t109 * t113 * t165 * t102
      t308 = 0.25248D5 * t163 - t168 - 0.74320D5 * t177 + 0.304D3 * t187
     # + 0.27920D5 * t190 + 0.29128D5 * t200 + 0.28792D5 * t213 - 0.7428
     #8D5 * t220 - 0.536D3 * t223 - 0.25264D5 * t238 + 0.80D2 * t241 + 0
     #.25296D5 * t244 - 0.32D2 * t254
      rrgq2qgh22J6 = -(wd * (t88 + t174 + t218 + t262) + wd * (t295 + t3
     #08)) / t1 / t16 / t24 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrgq2qgh22J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t5 = 0.1D1 - z
      t7 = x1 * x3
      t8 = s * t3
      t9 = t5 * x1
      t12 = 0.1D1 - x3
      t16 = (s - t8 * t9 * x3 - t8 * t9 * t12) ** 2
      t17 = 0.1D1 - x1
      t18 = t5 * t17
      t21 = 0.1D1 - x4
      t24 = s - t8 * t18 * x4 - t8 * t18 * t21
      t25 = t16 * t24
      t29 = t3 ** 2
      t30 = t2 * t29
      t31 = t5 ** 2
      t33 = x1 ** 2
      t34 = x3 ** 2
      t35 = t33 * t34
      t39 = t29 * t3
      t40 = t2 * t39
      t41 = t31 * t5
      t43 = t33 * x1
      t44 = t34 * x3
      t45 = t43 * t44
      t49 = t1 ** 2
      t50 = t49 * t39
      t52 = t50 * t41 * x1
      t53 = x3 * t16
      t54 = t17 ** 2
      t55 = t21 ** 2
      t60 = t41 * t33
      t61 = t50 * t60
      t62 = t34 * t16
      t63 = t17 * x4
      t67 = t49 * t29
      t68 = t31 * x1
      t69 = t67 * t68
      t73 = t17 * t21
      t80 = x4 ** 2
      t85 = t54 * t21
      t90 = t29 ** 2
      t91 = t49 * t90
      t92 = t31 ** 2
      t93 = t92 * t43
      t94 = t91 * t93
      t101 = t91 * t92 * t33
      t107 = 0.28224D5 * t2 * t3 * t5 * t7 * t25 - 0.74296D5 * t30 * t31
     # * t35 * t25 + 0.32D2 * t40 * t41 * t45 * t25 + 0.28792D5 * t52 * 
     #t53 * t54 * t55 - 0.74320D5 * t61 * t62 * t63 + 0.27920D5 * t69 * 
     #t53 * t63 - 0.74288D5 * t61 * t62 * t73 - 0.536D3 * t69 * t53 * t7
     #3 + 0.304D3 * t52 * t53 * t54 * t80 + 0.29128D5 * t52 * t53 * t85 
     #* x4 + 0.16D2 * t94 * t44 * t17 * x4 * t16 + 0.16D2 * t101 * t34 *
     # t54 * t80 * t16
      t110 = t24 * t17
      t114 = cos(x2 * 0.3141592653589793D1)
      t118 = Sqrt(x3 * t12 * x4 * t21)
      t121 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t114 * t118
      t122 = t110 * t121
      t130 = t17 * t121
      t134 = t40 * t60
      t138 = t110 * x4
      t142 = t110 * t21
      t162 = t30 * t68
      t174 = -0.16D2 * t2 * t90 * t93 * t62 * t122 + 0.25248D5 * t101 * 
     #t53 * t85 * t121 - 0.25264D5 * t61 * t53 * t130 + 0.25220D5 * t134
     # * t53 * t122 + 0.16D2 * t134 * t62 * t138 - 0.16D2 * t134 * t62 *
     # t142 - 0.32D2 * t50 * t41 * t45 * t16 - 0.28208D5 * t49 * t3 * t5
     # * t7 * t16 + 0.74208D5 * t67 * t31 * t35 * t16 + 0.80D2 * t94 * t
     #62 * t130 + 0.304D3 * t162 * t53 * t138 + 0.28372D5 * t162 * t53 *
     # t142 + 0.25296D5 * t101 * t53 * t54 * x4 * t121
      rrgq2qgh22J7 = -wd * (t107 + t174) / t1 / t16 / t24 / z / 0.314159
     #2653589793D1 / 0.36D2

      end function
  
 