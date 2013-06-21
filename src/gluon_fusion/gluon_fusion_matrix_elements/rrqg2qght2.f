  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t26 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4)
      t29 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4)
      t31 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4)
      t32 = t17 * t4
      t35 = log(-0.4D1 * t14 * t32)
      t36 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t38 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t39 = t35 ** 2
      t40 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t83 = rrqg2qgh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t165 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4
     #)
      t168 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4
     #)
      t169 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t25, x4
     #)
      t171 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t172 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t174 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t187 = t165 - t174
      t193 = rrqg2qgh22J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
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
      t242 = x1 * t1
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
      t268 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t269 = t267 * t268
      t271 = t244 * t250
      t272 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t274 = t271 * t266 * t272
      t279 = t47 * t48
      t281 = t271 * t266 * t268
      t292 = log(-0.4D1 * t139 * t119 * t120 * t258 * t256)
      t293 = t292 ** 2
      t294 = t293 * t244
      t297 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t300 = t292 * t244
      t312 = t60 * t48
      t317 = -(0.90D2 * t6 * t7 * (t264 * t269 - t274) + 0.180D3 * t279 
     #* t281) * t65 * t136 / 0.720D3 + (0.90D2 * t6 * t7 * (t294 * t269 
     #/ 0.2D1 + t271 * t266 * t297 - t300 * t267 * t272) - 0.180D3 * t47
     # * t48 * (t274 - t300 * t269) + t312 * t281) * t136 / 0.720D3
      t318 = FJET(XB1, XB2, s, t243, 0.0D0, -t247, t249, t255, t317)
      t320 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t322 = t271 * t266 * t320
      t323 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t324 = t267 * t323
      t331 = t271 * t266 * t323
      t339 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
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
      t395 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t398 = t244 * t368
      t399 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t410 = 0.90D2 * t6 * t7 * (-t391 * t394 * t395 + t398 * t393 * t39
     #9) - 0.180D3 * t279 * t398 * t393 * t395
      t414 = FJET(XB1, XB2, s, -t363, t365, -t366, t367, t381, -t410 * t
     #65 * t136 / 0.720D3)
      t416 = t65 * t136
      t419 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t422 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25, x4)
      t433 = 0.90D2 * t6 * t7 * (-t391 * t394 * t419 + t398 * t393 * t42
     #2) - 0.180D3 * t279 * t398 * t393 * t419
      t437 = FJET(XB1, XB2, s, -t366, t367, -t363, t365, t381, -t433 * t
     #65 * t136 / 0.720D3)
      rrqg2qght2s1e1 = t163 * t162 + t238 * t237 + t318 * t317 + t358 * 
     #t357 - t414 * t410 * t416 / 0.720D3 - t437 * t433 * t416 / 0.720D3

      end function



      doubleprecision function rrqg2qght2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = 0.1D1 - x3
      t9 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
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
      t26 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t28 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t29 = t19 * t4
      t32 = log(-0.4D1 * t16 * t29)
      t33 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t73 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t108 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t109 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4)
      t111 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t112 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t118 = t109 - t112
      t141 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t151 = (0.90D2 * t6 * t7 * (t108 - t25 * t109 - t111 + t32 * t112)
     # - 0.180D3 * t39 * t40 * t118) * t46 / 0.1440D4 + t49 * t118 * t46
     # * t52 / 0.8D1 + (0.90D2 * t6 * t7 * (-t111 + t61 * t112) + 0.180D
     #3 * t39 * t40 * t112) * t52 / 0.720D3 - t6 * t7 * t141 / 0.16D2 - 
     #t85 * t7 * t111 / 0.1440D4 - t101 * t7 * t112 / 0.1440D4
      t152 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t151)
      t154 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t155 = s * t154
      t156 = x1 * t1
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
      t175 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t181 = t158 * t164
      t182 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t187 = x4 * t4
      t188 = t164 ** 2
      t189 = t158 ** 2
      t194 = log(-0.4D1 * t57 * t15 * t18 * t187 * t188 * t189)
      t195 = t194 * t158
      t202 = t39 * t40
      t210 = t171 * t174 * t175 * t46 * t52 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t181 * t173 * t182 - t195 * t174 * t175) - 0.180D3 * t202 * t18
     #1 * t173 * t175) * t52 / 0.720D3
      t211 = FJET(XB1, XB2, s, t157, 0.0D0, -t161, t163, t169, t210)
      t213 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t219 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
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
      t263 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t266 = t262 * t263 * t46 * t52
      t269 = FJET(XB1, XB2, s, -t241, t243, -t244, t245, t259, -t171 * t
     #266 / 0.8D1)
      t271 = t40 * t158
      t275 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t278 = t262 * t275 * t46 * t52
      t281 = FJET(XB1, XB2, s, -t244, t245, -t241, t243, t259, -t171 * t
     #278 / 0.8D1)
      rrqg2qght2s1e0 = t106 * t105 + t152 * t151 + t211 * t210 + t236 * 
     #t235 - t269 * 0.3141592653589793D1 * t271 * t266 / 0.8D1 - t281 * 
     #0.3141592653589793D1 * t271 * t278 / 0.8D1

      end function



      doubleprecision function rrqg2qght2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, x
     #4)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t40 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t39, x4)
      t43 = 0.1D1 / x3
      t47 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.16D2 - t36 * t9 /
     # 0.1440D4 + t6 * t7 * (t40 - t8) * t43 / 0.16D2
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t47)
      t50 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t51 = t7 * t50
      t55 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t61 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t39, x4)
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
      t91 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4)
      t96 = FJET(XB1, XB2, s, t73, 0.0D0, -t77, t79, t85, t87 * t90 * t9
     #1 * t10 / 0.8D1)
      t98 = t1 * t7
      t100 = t74 * t80
      t106 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t111 = FJET(XB1, XB2, s, -t77, t79, t73, 0.0D0, t85, t87 * t90 * t
     #106 * t10 / 0.8D1)
      rrqg2qght2s1em1 = t47 * t48 + t68 * t67 + t96 * 0.3141592653589793
     #D1 * t98 * t100 * t89 * t91 * t10 / 0.8D1 + t111 * 0.3141592653589
     #793D1 * t98 * t100 * t89 * t106 * t10 / 0.8D1

      end function



      doubleprecision function rrqg2qght2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, x
     #4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrqg2qght2s1em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0.16D2 
     #- t21 * 0.3141592653589793D1 * t14 * t17 / 0.16D2

      end function



      doubleprecision function rrqg2qght2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrqg2qght2s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrqg2qght2s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght2s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t25 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t28 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t30 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t31 = t17 * t4
      t34 = log(-0.4D1 * t14 * t31)
      t35 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t37 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t38 = t34 ** 2
      t39 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t46 = 0.3141592653589793D1 * lh
      t47 = t1 * t7
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t58 = 0.180D3 * t54 - 0.30D2 * t56
      t59 = 0.3141592653589793D1 * t58
      t60 = t25 - t39
      t64 = 0.1D1 / x3
      t67 = t10 * t13
      t70 = log(-0.4D1 * t67 * t31)
      t71 = t70 * 0.3141592653589793D1
      t74 = t70 ** 2
      t75 = t74 * 0.3141592653589793D1
      t78 = (t59 + 0.180D3 * t71 * lh + 0.45D2 * t75) * t1
      t82 = rrqg2qgh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
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
      t135 = 0.1D1 / x1
      t138 = t110 * t10
      t142 = log(-0.4D1 * t138 * t13 * t31)
      t144 = t142 ** 2
      t161 = (0.90D2 * t6 * t7 * (t24 * t25 / 0.2D1 - t23 * t28 + t30 + 
     #t34 * t35 - t37 - t38 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (t28 
     #- t23 * t25 - t35 + t34 * t39) + t59 * t47 * t60) * t64 / 0.1440D4
     # - t78 * t7 * t35 / 0.1440D4 - t6 * t7 * t82 / 0.16D2 - t99 * t7 *
     # t39 / 0.1440D4 - t106 * t7 * t37 / 0.1440D4 - (0.90D2 * t6 * t7 *
     # (t35 - t118 * t39 - t28 + t123 * t25) + 0.180D3 * t46 * t47 * t60
     #) * t64 * t135 / 0.720D3 + (0.90D2 * t6 * t7 * (t142 * t35 - t37 -
     # t144 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (-t35 + t142 * t39) -
     # t59 * t47 * t39) * t135 / 0.720D3
      t162 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t161)
      t164 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t165 = s * t164
      t166 = t1 * x1
      t167 = t165 * t166
      t168 = -0.1D1 + x1
      t169 = t1 * t168
      t170 = t169 * x4
      t171 = t165 * t170
      t172 = t169 * t4
      t173 = t165 * t172
      t174 = t164 ** 2
      t177 = x1 * t168
      t179 = s * t174 * t15 * t177 * x4
      t180 = t168 * t174
      t182 = 0.1D1 / (-0.2D1 + t164)
      t183 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t185 = t180 * t182 * t183
      t186 = t168 ** 2
      t188 = t174 ** 2
      t193 = log(-0.4D1 * t120 * t17 * t4 * t186 * t188)
      t194 = t193 * t168
      t195 = t174 * t182
      t196 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t197 = t195 * t196
      t203 = t46 * t47
      t205 = t180 * t182 * t196
      t211 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t219 = log(-0.4D1 * t138 * t113 * t114 * t188 * t186)
      t220 = t219 ** 2
      t221 = t220 * t168
      t224 = t219 * t168
      t236 = t59 * t47
      t241 = -(0.90D2 * t6 * t7 * (-t185 + t194 * t197) + 0.180D3 * t203
     # * t205) * t64 * t135 / 0.720D3 + (0.90D2 * t6 * t7 * (t180 * t182
     # * t211 + t221 * t197 / 0.2D1 - t224 * t195 * t183) - 0.180D3 * t4
     #6 * t47 * (-t224 * t197 + t185) + t236 * t205) * t135 / 0.720D3
      t242 = FJET(XB1, XB2, s, 0.0D0, t167, -t171, t173, -t179, t241)
      t244 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t247 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t248 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t250 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t251 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t253 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t266 = t244 - t253
      t272 = rrqg2qgh22J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t316 = (0.90D2 * t6 * t7 * (t24 * t244 / 0.2D1 + t247 - t23 * t248
     # - t250 + t34 * t251 - t38 * t253 / 0.2D1) - 0.180D3 * t46 * t47 *
     # (t248 - t23 * t244 - t251 + t34 * t253) + t59 * t47 * t266) * t64
     # / 0.1440D4 - t6 * t7 * t272 / 0.16D2 - t99 * t7 * t253 / 0.1440D4
     # - t106 * t7 * t250 / 0.1440D4 - t78 * t7 * t251 / 0.1440D4 - (0.9
     #0D2 * t6 * t7 * (-t248 + t251 - t118 * t253 + t123 * t244) + 0.180
     #D3 * t46 * t47 * t266) * t64 * t135 / 0.720D3 + (0.90D2 * t6 * t7 
     #* (-t250 - t144 * t253 / 0.2D1 + t142 * t251) - 0.180D3 * t46 * t4
     #7 * (t142 * t253 - t251) - t59 * t47 * t253) * t135 / 0.720D3
      t317 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t316)
      t319 = KAPPA2(x1, x2, x3, x4, z)
      t320 = s * t319
      t322 = t320 * t166 * x3
      t324 = t320 * t166 * t18
      t325 = t320 * t170
      t326 = t320 * t172
      t327 = t319 ** 2
      t332 = cos(t8)
      t335 = Sqrt(x3 * t18 * t114)
      t340 = s * t327 * t15 * t177 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t332 * t335)
      t344 = t327 ** 2
      t349 = log(0.4D1 * t111 * t67 * t16 * t114 * t186 * t18 * t344)
      t350 = t349 * t168
      t352 = 0.1D1 / (-0.2D1 + t319)
      t353 = t327 * t352
      t354 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t357 = t168 * t327
      t358 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t369 = 0.90D2 * t6 * t7 * (-t350 * t353 * t354 + t357 * t352 * t35
     #8) - 0.180D3 * t203 * t357 * t352 * t354
      t373 = FJET(XB1, XB2, s, t322, -t324, -t325, t326, t340, -t369 * t
     #64 * t135 / 0.720D3)
      t375 = t64 * t135
      t378 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t379 = t195 * t378
      t381 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t383 = t180 * t182 * t381
      t389 = t180 * t182 * t378
      t395 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t415 = -(0.90D2 * t6 * t7 * (t194 * t379 - t383) + 0.180D3 * t203 
     #* t389) * t64 * t135 / 0.720D3 + (0.90D2 * t6 * t7 * (t180 * t182 
     #* t395 - t224 * t195 * t381 + t221 * t379 / 0.2D1) - 0.180D3 * t46
     # * t47 * (t383 - t224 * t379) + t236 * t389) * t135 / 0.720D3
      t416 = FJET(XB1, XB2, s, -t171, t173, 0.0D0, t167, -t179, t415)
      t418 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t421 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t432 = 0.90D2 * t6 * t7 * (t357 * t352 * t418 - t350 * t353 * t421
     #) - 0.180D3 * t203 * t357 * t352 * t421
      t436 = FJET(XB1, XB2, s, -t325, t326, t322, -t324, t340, -t432 * t
     #64 * t135 / 0.720D3)
      rrqg2qght2s2e1 = t162 * t161 + t242 * t241 + t317 * t316 - t373 * 
     #t369 * t375 / 0.720D3 + t416 * t415 - t436 * t432 * t375 / 0.720D3

      end function



      doubleprecision function rrqg2qght2s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
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
      t25 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t27 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t28 = t18 * t4
      t31 = log(-0.4D1 * t15 * t28)
      t32 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t1 * t7
      t40 = t25 - t32
      t45 = 0.1D1 / x3
      t48 = t6 * t7
      t51 = 0.1D1 / x1
      t55 = x1 ** 2
      t56 = t55 * t11
      t60 = log(-0.4D1 * t56 * t14 * t28)
      t72 = rrqg2qgh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t80 = log(-0.4D1 * t11 * t14 * t28)
      t81 = t80 * 0.3141592653589793D1
      t84 = (-0.180D3 * t38 - 0.90D2 * t81) * t1
      t88 = lh ** 2
      t90 = 0.3141592653589793D1 ** 2
      t96 = t80 ** 2
      t100 = (0.3141592653589793D1 * (0.180D3 * t88 - 0.30D2 * t90) + 0.
     #180D3 * t81 * lh + 0.45D2 * t96 * 0.3141592653589793D1) * t1
      t104 = (0.90D2 * t6 * t7 * (t8 - t24 * t25 - t27 + t31 * t32) - 0.
     #180D3 * t38 * t39 * t40) * t45 / 0.1440D4 + t48 * t40 * t45 * t51 
     #/ 0.8D1 + (0.90D2 * t6 * t7 * (-t27 + t60 * t32) + 0.180D3 * t38 *
     # t39 * t32) * t51 / 0.720D3 - t6 * t7 * t72 / 0.16D2 - t84 * t7 * 
     #t27 / 0.1440D4 - t100 * t7 * t32 / 0.1440D4
      t105 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t104)
      t107 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t108 = s * t107
      t109 = t1 * x1
      t110 = t108 * t109
      t111 = -0.1D1 + x1
      t112 = t1 * t111
      t113 = t112 * x4
      t114 = t108 * t113
      t115 = t112 * t4
      t116 = t108 * t115
      t117 = t107 ** 2
      t120 = x1 * t111
      t122 = s * t117 * t16 * t120 * x4
      t124 = t6 * t7 * t111
      t126 = 0.1D1 / (-0.2D1 + t107)
      t127 = t117 * t126
      t128 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t136 = x4 * t4
      t137 = t117 ** 2
      t138 = t111 ** 2
      t143 = log(-0.4D1 * t56 * t14 * t17 * t136 * t137 * t138)
      t144 = t143 * t111
      t147 = t111 * t117
      t148 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t155 = t38 * t39
      t163 = t124 * t127 * t128 * t45 * t51 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (-t144 * t127 * t128 + t147 * t126 * t148) - 0.180D3 * t155 * t1
     #47 * t126 * t128) * t51 / 0.720D3
      t164 = FJET(XB1, XB2, s, 0.0D0, t110, -t114, t116, -t122, t163)
      t166 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t167 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t169 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t170 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t176 = t167 - t170
      t199 = rrqg2qgh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t209 = (0.90D2 * t6 * t7 * (t166 - t24 * t167 - t169 + t31 * t170)
     # - 0.180D3 * t38 * t39 * t176) * t45 / 0.1440D4 + t48 * t176 * t45
     # * t51 / 0.8D1 + (0.90D2 * t6 * t7 * (t60 * t170 - t169) + 0.180D3
     # * t38 * t39 * t170) * t51 / 0.720D3 - t6 * t7 * t199 / 0.16D2 - t
     #84 * t7 * t169 / 0.1440D4 - t100 * t7 * t170 / 0.1440D4
      t210 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t209)
      t212 = KAPPA2(x1, x2, x3, x4, z)
      t213 = s * t212
      t215 = t213 * t109 * x3
      t217 = t213 * t109 * t19
      t218 = t213 * t113
      t219 = t213 * t115
      t220 = t212 ** 2
      t225 = cos(t9)
      t228 = Sqrt(x3 * t19 * t136)
      t233 = s * t220 * t16 * t120 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t225 * t228)
      t236 = t220 / (-0.2D1 + t212)
      t237 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t240 = t236 * t237 * t45 * t51
      t243 = FJET(XB1, XB2, s, t215, -t217, -t218, t219, t233, -t124 * t
     #240 / 0.8D1)
      t245 = t39 * t111
      t249 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t255 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t271 = t124 * t127 * t249 * t45 * t51 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t147 * t126 * t255 - t144 * t127 * t249) - 0.180D3 * t155 * t14
     #7 * t126 * t249) * t51 / 0.720D3
      t272 = FJET(XB1, XB2, s, -t114, t116, 0.0D0, t110, -t122, t271)
      t274 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t277 = t236 * t274 * t45 * t51
      t280 = FJET(XB1, XB2, s, -t218, t219, t215, -t217, t233, -t124 * t
     #277 / 0.8D1)
      rrqg2qght2s2e0 = t105 * t104 + t164 * t163 + t210 * t209 - t243 * 
     #0.3141592653589793D1 * t245 * t240 / 0.8D1 + t272 * t271 - t280 * 
     #0.3141592653589793D1 * t245 * t277 / 0.8D1

      end function



      doubleprecision function rrqg2qght2s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x4
     #)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqg2qgh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t36 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.314
     #1592653589793D1) * t1
      t39 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
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
      t70 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t75 = FJET(XB1, XB2, s, 0.0D0, t52, -t56, t58, -t64, t66 * t69 * t
     #70 * t10 / 0.8D1)
      t77 = t1 * t7
      t79 = t53 * t59
      t85 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t86 = t7 * t85
      t90 = rrqg2qgh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t96 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t102 = -t6 * t86 * t10 / 0.8D1 - t6 * t7 * t90 / 0.16D2 - t36 * t8
     #6 / 0.1440D4 + t6 * t7 * (t96 - t85) * t42 / 0.16D2
      t103 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t110 = FJET(XB1, XB2, s, -t56, t58, 0.0D0, t52, -t64, t66 * t69 * 
     #t105 * t10 / 0.8D1)
      rrqg2qght2s2em1 = t46 * t47 + t75 * 0.3141592653589793D1 * t77 * t
     #79 * t68 * t70 * t10 / 0.8D1 + t103 * t102 + t110 * 0.314159265358
     #9793D1 * t77 * t79 * t68 * t105 * t10 / 0.8D1

      end function



      doubleprecision function rrqg2qght2s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x4
     #)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrqg2qgh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrqg2qght2s2em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0.16D2 
     #- t21 * 0.3141592653589793D1 * t14 * t17 / 0.16D2

      end function



      doubleprecision function rrqg2qght2s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrqg2qght2s2em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght2s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrqg2qght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh21J1
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
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x1 ** 2
      t11 = t9 * t10
      t12 = t11 * x3
      t14 = s * t3
      t15 = t7 * x1
      t18 = 0.1D1 - x3
      t21 = s - t14 * t15 * x3 - t14 * t15 * t18
      t22 = t21 ** 2
      t23 = 0.1D1 - x1
      t24 = t7 * t23
      t27 = 0.1D1 - x4
      t30 = s - t14 * t24 * x4 - t14 * t24 * t27
      t31 = t22 * t30
      t32 = t23 ** 2
      t33 = t32 * t27
      t37 = cos(x2 * 0.3141592653589793D1)
      t39 = x4 * t27
      t41 = Sqrt(x3 * t18 * t39)
      t44 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t37 * t41
      t45 = t33 * t44
      t46 = t31 * t45
      t49 = t2 * t22
      t52 = t44 ** 2
      t57 = t2 * t4
      t58 = t8 * x1
      t59 = t57 * t58
      t60 = t18 * t22
      t61 = t30 * t23
      t62 = t61 * t27
      t66 = t4 * t3
      t67 = t2 * t66
      t68 = t7 * t8
      t69 = t68 * x1
      t70 = t67 * t69
      t71 = t30 * t32
      t72 = t27 ** 2
      t73 = t71 * t72
      t77 = t2 * t3
      t79 = t30 * z
      t83 = t1 ** 2
      t84 = t5 * t3
      t87 = t10 * x1
      t88 = t9 * t7 * t87
      t90 = x3 * t21
      t91 = t71 * t52
      t95 = t83 * t4
      t96 = t95 * t58
      t97 = t18 * t21
      t98 = t61 * x4
      t102 = t83 * t66
      t103 = t68 * t10
      t104 = t103 * t102
      t105 = t18 ** 2
      t106 = t105 * t21
      t110 = t69 * t18
      t112 = t33 * x4
      t113 = t31 * t112
      t116 = t83 * t3
      t117 = t116 * t15
      t118 = z ** 2
      t123 = t102 * t69
      t131 = t21 * t30
      t132 = t131 * t112
      t135 = x3 ** 2
      t136 = t135 * t21
      t140 = 0.32D2 * t6 * t12 * t46 + 0.252D3 * t49 * t30 * t5 * t11 * 
     #t32 * t52 - 0.2304D4 * t59 * t60 * t62 - 0.72D2 * t70 * t60 * t73 
     #+ 0.3456D4 * t77 * t15 * t60 * t79 + 0.748D3 * t83 * t84 * t88 * t
     #90 * t91 + 0.144D3 * t96 * t97 * t98 + 0.144D3 * t104 * t106 * t98
     # - 0.216D3 * t67 * t110 * t113 - 0.1728D4 * t117 * t97 * t30 * t11
     #8 + 0.1440D4 * t123 * t97 * t73 + 0.312D3 * t123 * t90 * t73 + 0.1
     #440D4 * t102 * t110 * t132 - 0.128D3 * t104 * t136 * t62
      t142 = t2 * t84 * t88
      t146 = t61 * t44
      t150 = t11 * t18
      t151 = t6 * t150
      t153 = t32 * x4 * t44
      t167 = x4 ** 2
      t168 = t71 * t167
      t172 = x3 * t22
      t182 = t83 * t5
      t184 = t182 * t9 * t87
      t189 = t49 * t30 * t66
      t190 = t68 * t32
      t204 = t8 * t10
      t209 = -0.36D2 * t142 * t60 * t91 + 0.144D3 * t104 * t97 * t146 - 
     #0.144D3 * t151 * t31 * t153 - 0.1728D4 * t95 * t58 * t18 * t131 * 
     #z * t23 * x4 - 0.128D3 * t117 * t90 * t79 - 0.252D3 * t70 * t60 * 
     #t168 - 0.32D2 * t142 * t172 * t91 - 0.306D3 * t67 * t103 * t172 * 
     #t146 - 0.72D2 * t151 * t46 + 0.144D3 * t184 * t106 * t146 + 0.144D
     #3 * t189 * t190 * x4 * x1 * t44 - 0.216D3 * t189 * t190 * t27 * x1
     # * t44 - 0.32D2 * t70 * t172 * t73 - 0.128D3 * t95 * t204 * t136 *
     # t79
      t218 = t182 * t12
      t228 = t49 * t30
      t229 = t4 * t8
      t234 = t95 * t8
      t239 = t116 * t7
      t245 = x1 * x3
      t250 = t83 * s * t118
      t257 = t10 * t135
      t266 = t102 * t68
      t279 = 0.1728D4 * t102 * t103 * t18 * t131 * t23 * t44 * z - 0.16D
     #2 * t218 * t131 * t153 + 0.96D2 * t184 * t136 * t146 + 0.96D2 * t1
     #04 * t90 * t146 + 0.36D2 * t228 * t229 * t32 * t167 - 0.576D3 * t2
     #34 * t10 * t105 * t131 - 0.288D3 * t239 * x1 * t18 * t131 + 0.17D2
     # * t77 * t7 * t245 * t31 + 0.64D2 * t250 * t3 * t15 * x3 * t30 + 0
     #.275D3 * t57 * t8 * t257 * t31 + 0.64D2 * t250 * t4 * t204 * t135 
     #* t30 - 0.288D3 * t266 * t87 * t105 * t18 * t131 + 0.47D2 * t239 *
     # t245 * t131 + 0.72D2 * t228 * t229 * t32 * t72
      t288 = t131 * t45
      t294 = t69 * x3
      t328 = 0.94D2 * t234 * t257 * t131 + 0.47D2 * t266 * t87 * t135 * 
     #x3 * t131 - 0.880D3 * t218 * t288 + 0.122D3 * t59 * t172 * t98 - 0
     #.32D2 * t67 * t294 * t113 - 0.240D3 * t102 * t294 * t132 - 0.12D2 
     #* t123 * t90 * t168 - 0.32D2 * t70 * t172 * t168 - 0.148D3 * t104 
     #* t136 * t98 - 0.148D3 * t96 * t90 * t98 - 0.128D3 * t96 * t90 * t
     #62 + 0.528D3 * t59 * t172 * t62 - 0.1440D4 * t182 * t150 * t288 - 
     #0.72D2 * t49 * t30 * t4 * t8 * t32 * t39
      rrqg2qgh21J1 = wd * (t140 + t209 + t279 + t328) / t1 / t22 / t30 /
     # z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh21J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t4 = z ** 2
      t5 = t2 * s * t4
      t6 = kappa2(x1, x2, x3, x4, z)
      t8 = 0.1D1 - z
      t9 = t8 * x1
      t10 = s * t6
      t11 = 0.1D1 - x1
      t12 = t8 * t11
      t15 = 0.1D1 - x4
      t18 = s - t10 * t12 * x4 - t10 * t12 * t15
      t21 = t5 * t6 * t9 * x3 * t18
      t23 = s * t1
      t26 = 0.1D1 - x3
      t29 = s - t10 * t9 * x3 - t10 * t9 * t26
      t30 = t29 ** 2
      t31 = t30 * t23
      t32 = t31 * t18
      t33 = t6 ** 2
      t34 = t8 ** 2
      t35 = t33 * t34
      t36 = t11 ** 2
      t37 = t15 ** 2
      t41 = 0.72D2 * t32 * t35 * t36 * t37
      t42 = t2 * t6
      t43 = t42 * t8
      t44 = x1 * x3
      t45 = t29 * t18
      t47 = t43 * t44 * t45
      t49 = t2 * t33
      t50 = t49 * t34
      t51 = x1 ** 2
      t52 = x3 ** 2
      t53 = t51 * t52
      t55 = t50 * t53 * t45
      t57 = t33 * t6
      t58 = t2 * t57
      t59 = t34 * t8
      t60 = t58 * t59
      t61 = t51 * x1
      t65 = t60 * t61 * t52 * x3 * t45
      t67 = x4 ** 2
      t72 = t26 ** 2
      t73 = t51 * t72
      t77 = x1 * t26
      t81 = t23 * t6
      t82 = t81 * t8
      t83 = t30 * t18
      t85 = t82 * t44 * t83
      t88 = t61 * t72 * t26
      t93 = t34 * t51
      t96 = t5 * t33 * t93 * t52 * t18
      t98 = t23 * t33
      t99 = t98 * t34
      t101 = t99 * t53 * t83
      t103 = t23 * t57
      t104 = t59 * x1
      t105 = t104 * t26
      t107 = t36 * t15
      t108 = t107 * x4
      t109 = t83 * t108
      t112 = t58 * t104
      t113 = t26 * t29
      t114 = t18 * t36
      t115 = t114 * t37
      t118 = 0.1440D4 * t112 * t113 * t115
      t119 = 0.64D2 * t21 + t41 + 0.47D2 * t47 + 0.94D2 * t55 + 0.47D2 *
     # t65 + 0.36D2 * t32 * t35 * t36 * t67 - 0.576D3 * t50 * t73 * t45 
     #- 0.288D3 * t43 * t77 * t45 + 0.17D2 * t85 - 0.288D3 * t60 * t88 *
     # t45 + 0.64D2 * t96 + 0.275D3 * t101 - 0.216D3 * t103 * t105 * t10
     #9 + t118
      t120 = t59 * t51
      t121 = t103 * t120
      t122 = x3 * t30
      t123 = t11 * t18
      t127 = cos(x2 * 0.3141592653589793D1)
      t129 = x4 * t15
      t131 = Sqrt(x3 * t26 * t129)
      t134 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t127 * t131
      t135 = t123 * t134
      t137 = t121 * t122 * t135
      t139 = t33 ** 2
      t140 = t2 * t139
      t141 = t34 ** 2
      t142 = t141 * t51
      t143 = t142 * x3
      t144 = t140 * t143
      t145 = t107 * t134
      t146 = t45 * t145
      t147 = t144 * t146
      t150 = t31 * t18 * t57
      t151 = t59 * t36
      t159 = t134 ** 2
      t164 = t139 * t6
      t167 = t141 * t8 * t61
      t168 = t23 * t164 * t167
      t169 = t26 * t30
      t170 = t114 * t159
      t174 = t58 * t120
      t176 = t174 * t113 * t135
      t178 = x3 * t29
      t180 = t112 * t178 * t115
      t182 = t103 * t104
      t184 = t182 * t122 * t115
      t187 = t52 * t29
      t188 = t18 * z
      t190 = t49 * t93 * t187 * t188
      t198 = 0.1728D4 * t58 * t120 * t26 * t45 * t11 * t134 * z
      t199 = t34 * x1
      t200 = t49 * t199
      t201 = t123 * x4
      t203 = t200 * t113 * t201
      t205 = t72 * t29
      t207 = t174 * t205 * t201
      t209 = t98 * t199
      t210 = t123 * t15
      t212 = t209 * t122 * t210
      t215 = t209 * t122 * t201
      t217 = -0.306D3 * t137 - 0.880D3 * t147 - 0.216D3 * t150 * t151 * 
     #t15 * x1 * t134 + 0.252D3 * t31 * t18 * t139 * t142 * t36 * t159 -
     # 0.36D2 * t168 * t169 * t170 + 0.144D3 * t176 + 0.312D3 * t180 - 0
     #.32D2 * t184 - 0.128D3 * t190 + t198 + 0.144D3 * t203 + 0.144D3 * 
     #t207 + 0.528D3 * t212 + 0.122D3 * t215
      t220 = t36 * x4 * t134
      t221 = t45 * t220
      t222 = t144 * t221
      t225 = t140 * t141 * t61
      t227 = t225 * t187 * t135
      t230 = t174 * t178 * t135
      t233 = t2 * t164 * t167
      t235 = t233 * t178 * t170
      t243 = 0.1728D4 * t49 * t199 * t26 * t45 * z * t11 * x4
      t244 = t42 * t9
      t246 = t244 * t178 * t188
      t248 = t114 * t67
      t253 = t174 * t187 * t201
      t256 = t200 * t178 * t201
      t259 = t200 * t178 * t210
      t262 = t174 * t187 * t210
      t265 = t209 * t169 * t210
      t269 = 0.72D2 * t182 * t169 * t115
      t270 = t23 * t139
      t271 = t142 * t26
      t272 = t270 * t271
      t273 = t83 * t220
      t276 = -0.16D2 * t222 + 0.96D2 * t227 + 0.96D2 * t230 + 0.748D3 * 
     #t235 - t243 - 0.128D3 * t246 - 0.252D3 * t182 * t169 * t248 - 0.14
     #8D3 * t253 - 0.148D3 * t256 - 0.128D3 * t259 - 0.128D3 * t262 - 0.
     #2304D4 * t265 - t269 - 0.144D3 * t272 * t273
      t277 = t104 * x3
      t279 = t103 * t277 * t109
      t282 = t45 * t108
      t283 = t58 * t277 * t282
      t286 = t112 * t178 * t248
      t289 = t58 * t105 * t282
      t291 = t140 * t271
      t292 = t291 * t146
      t297 = 0.1728D4 * t244 * t113 * t18 * t4
      t301 = 0.3456D4 * t81 * t9 * t169 * t188
      t302 = t270 * t143
      t303 = t83 * t145
      t304 = t302 * t303
      t309 = t225 * t205 * t135
      t318 = 0.32D2 * t182 * t122 * t248
      t327 = 0.32D2 * t168 * t122 * t170
      t328 = -0.32D2 * t279 - 0.240D3 * t283 - 0.12D2 * t286 + 0.1440D4 
     #* t289 - 0.1440D4 * t292 - t297 + t301 + 0.32D2 * t304 - 0.72D2 * 
     #t272 * t303 + 0.144D3 * t309 + 0.144D3 * t150 * t151 * x4 * x1 * t
     #134 - t318 - 0.72D2 * t31 * t18 * t33 * t34 * t36 * t129 - t327
      t354 = -0.128D3 * t21 - t41 - 0.54D2 * t47 - 0.108D3 * t55 - 0.54D
     #2 * t65 - 0.72D2 * t103 * t59 * t88 * t83 + 0.2808D4 * t99 * t73 *
     # t83 - 0.2808D4 * t82 * t77 * t83 - 0.72D2 * t32 * t6 * t8 * t11 *
     # t15 - 0.490D3 * t85 - 0.128D3 * t96 - 0.156D3 * t101 - t118
      t372 = -0.288D3 * t233 * t113 * t170 - 0.2808D4 * t121 * t169 * t1
     #35 + 0.190D3 * t137 + 0.640D3 * t147 - 0.72D2 * t176 - 0.264D3 * t
     #180 + 0.64D2 * t184 + 0.256D3 * t190 - t198 - 0.72D2 * t203 - 0.72
     #D2 * t207 - 0.752D3 * t212 - 0.134D3 * t215 + 0.608D3 * t222
      t386 = -0.640D3 * t227 - 0.640D3 * t230 - 0.592D3 * t235 + t243 + 
     #0.256D3 * t246 + 0.216D3 * t253 + 0.216D3 * t256 + 0.496D3 * t259 
     #+ 0.496D3 * t262 + 0.4320D4 * t265 + t269 + 0.72D2 * t32 + 0.64D2 
     #* t279 - 0.64D2 * t283
      t406 = -0.16D2 * t286 - 0.1728D4 * t289 + 0.1728D4 * t292 + 0.576D
     #3 * t291 * t221 - 0.64D2 * t302 * t273 + t297 - t301 - 0.64D2 * t3
     #04 - 0.72D2 * t309 + t318 - 0.288D3 * t112 * t113 * t248 + 0.2808D
     #4 * t209 * t169 * t201 - 0.72D2 * t121 * t72 * t30 * t210 + t327
      rrqg2qgh21J2 = (wd * (t119 + t217 + t276 + t328) + wd * (t354 + t3
     #72 + t386 + t406)) / t1 / t30 / t18 / z / 0.3141592653589793D1 / 0
     #.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh21J3
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
      t18 = t17 ** 2
      t19 = x3 * t18
      t20 = 0.1D1 - x1
      t21 = t6 * t20
      t24 = 0.1D1 - x4
      t27 = s - t10 * t21 * x4 - t10 * t21 * t24
      t28 = t27 * t20
      t29 = t28 * x4
      t31 = t9 * t19 * t29
      t33 = t4 * t3
      t34 = t2 * t33
      t35 = t6 * t7
      t36 = t35 * x1
      t37 = t36 * x3
      t39 = t18 * t27
      t40 = t20 ** 2
      t41 = t40 * t24
      t42 = t41 * x4
      t43 = t39 * t42
      t44 = t34 * t37 * t43
      t46 = t1 ** 2
      t47 = t46 * t33
      t49 = t17 * t27
      t50 = t49 * t42
      t51 = t47 * t37 * t50
      t53 = t47 * t36
      t54 = x3 * t17
      t55 = t27 * t40
      t56 = x4 ** 2
      t57 = t55 * t56
      t59 = t53 * t54 * t57
      t61 = t36 * t14
      t63 = t47 * t61 * t50
      t65 = x1 ** 2
      t66 = t35 * t65
      t67 = t47 * t66
      t68 = x3 ** 2
      t69 = t68 * t17
      t70 = t28 * t24
      t72 = t67 * t69 * t70
      t74 = t14 * t18
      t76 = t9 * t74 * t70
      t78 = t34 * t36
      t79 = t24 ** 2
      t80 = t55 * t79
      t83 = 0.72D2 * t78 * t74 * t80
      t84 = t2 * t18
      t88 = t24 * x4
      t92 = t4 ** 2
      t93 = t92 * t3
      t95 = t7 ** 2
      t97 = t65 * x1
      t98 = t95 * t6 * t97
      t99 = t2 * t93 * t98
      t103 = cos(x2 * 0.3141592653589793D1)
      t106 = Sqrt(x3 * t14 * t88)
      t109 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t103 * t106
      t110 = t109 ** 2
      t111 = t55 * t110
      t114 = 0.32D2 * t99 * t19 * t111
      t115 = t34 * t66
      t116 = t28 * t109
      t118 = t115 * t19 * t116
      t120 = t46 * t92
      t121 = t95 * t65
      t122 = t121 * x3
      t123 = t120 * t122
      t124 = t41 * t109
      t125 = t49 * t124
      t126 = t123 * t125
      t129 = t40 * x4 * t109
      t130 = t49 * t129
      t131 = t123 * t130
      t134 = t120 * t95 * t97
      t136 = t134 * t69 * t116
      t138 = 0.122D3 * t31 - 0.32D2 * t44 - 0.240D3 * t51 - 0.12D2 * t59
     # + 0.1440D4 * t63 - 0.128D3 * t72 - 0.2304D4 * t76 - t83 - 0.72D2 
     #* t84 * t27 * t4 * t7 * t40 * t88 - t114 - 0.306D3 * t118 - 0.880D
     #3 * t126 - 0.16D2 * t131 + 0.96D2 * t136
      t140 = t67 * t54 * t116
      t142 = t121 * t14
      t143 = t120 * t142
      t144 = t143 * t125
      t146 = t2 * t92
      t147 = t146 * t122
      t148 = t39 * t124
      t149 = t147 * t148
      t151 = t146 * t142
      t154 = t14 ** 2
      t155 = t154 * t17
      t157 = t134 * t155 * t116
      t160 = t84 * t27 * t33
      t161 = t35 * t40
      t181 = t14 * t17
      t183 = t67 * t181 * t116
      t185 = t39 * t129
      t189 = t46 * t93 * t98
      t191 = t189 * t54 * t111
      t193 = t46 * t4
      t200 = 0.1728D4 * t193 * t8 * t14 * t49 * z * t20 * x4
      t201 = t46 * t3
      t202 = t201 * t11
      t203 = t27 * z
      t205 = t202 * t54 * t203
      t206 = 0.128D3 * t205
      t207 = 0.96D2 * t140 - 0.1440D4 * t144 + 0.32D2 * t149 - 0.72D2 * 
     #t151 * t148 + 0.144D3 * t157 + 0.144D3 * t160 * t161 * x4 * x1 * t
     #109 - 0.216D3 * t160 * t161 * t24 * x1 * t109 + 0.252D3 * t84 * t2
     #7 * t92 * t121 * t40 * t110 - 0.36D2 * t99 * t74 * t111 + 0.144D3 
     #* t183 - 0.144D3 * t151 * t185 + 0.748D3 * t191 - t200 - t206
      t212 = t193 * t8
      t214 = t212 * t181 * t29
      t217 = t67 * t155 * t29
      t224 = 0.1440D4 * t53 * t181 * t80
      t226 = t53 * t54 * t80
      t229 = t78 * t19 * t80
      t231 = t7 * t65
      t234 = t193 * t231 * t69 * t203
      t242 = 0.1728D4 * t47 * t66 * t14 * t49 * t20 * t109 * z
      t243 = z ** 2
      t247 = 0.1728D4 * t202 * t181 * t27 * t243
      t250 = 0.32D2 * t78 * t19 * t57
      t252 = t67 * t69 * t29
      t255 = t212 * t54 * t29
      t258 = t212 * t54 * t70
      t260 = -0.252D3 * t78 * t74 * t57 + 0.144D3 * t214 + 0.144D3 * t21
     #7 - 0.216D3 * t34 * t61 * t43 + t224 + 0.312D3 * t226 - 0.32D2 * t
     #229 - 0.128D3 * t234 + t242 - t247 - t250 - 0.148D3 * t252 - 0.148
     #D3 * t255 - 0.128D3 * t258
      t262 = t9 * t19 * t70
      t264 = t2 * t3
      t268 = 0.3456D4 * t264 * t11 * t74 * t203
      t269 = t84 * t27
      t270 = t4 * t7
      t275 = t193 * t7
      t276 = t65 * t154
      t280 = t201 * t6
      t281 = x1 * t14
      t285 = t264 * t6
      t286 = x1 * x3
      t288 = t285 * t286 * t39
      t290 = t47 * t35
      t292 = t97 * t154 * t14
      t297 = t46 * s * t243
      t301 = t297 * t4 * t231 * t68 * t27
      t302 = 0.64D2 * t301
      t303 = t5 * t7
      t304 = t65 * t68
      t306 = t303 * t304 * t39
      t311 = t297 * t3 * t11 * x3 * t27
      t312 = 0.64D2 * t311
      t316 = 0.72D2 * t269 * t270 * t40 * t79
      t318 = t280 * t286 * t49
      t321 = t275 * t304 * t49
      t326 = t290 * t97 * t68 * x3 * t49
      t328 = 0.528D3 * t262 + t268 + 0.36D2 * t269 * t270 * t40 * t56 - 
     #0.576D3 * t275 * t276 * t49 - 0.288D3 * t280 * t281 * t49 + 0.17D2
     # * t288 - 0.288D3 * t290 * t292 * t49 + t302 + 0.275D3 * t306 + t3
     #12 + t316 + 0.47D2 * t318 + 0.94D2 * t321 + 0.47D2 * t326
      t343 = -0.134D3 * t31 + 0.64D2 * t44 - 0.64D2 * t51 - 0.16D2 * t59
     # - 0.1728D4 * t63 + 0.496D3 * t72 + 0.4320D4 * t76 + t83 + t114 + 
     #0.190D3 * t118 + 0.640D3 * t126 + 0.608D3 * t131 - 0.640D3 * t136
      t356 = -0.640D3 * t140 + 0.1728D4 * t144 - 0.64D2 * t149 - 0.72D2 
     #* t157 - 0.72D2 * t183 - 0.592D3 * t191 + t200 + 0.256D3 * t205 - 
     #0.72D2 * t214 - 0.72D2 * t217 - t224 - 0.264D3 * t226 + 0.64D2 * t
     #229 + 0.256D3 * t234
      t379 = -t242 + t247 + t250 + 0.216D3 * t252 + 0.216D3 * t255 + 0.4
     #96D3 * t258 - 0.752D3 * t262 - t268 - 0.288D3 * t53 * t181 * t57 +
     # 0.2808D4 * t9 * t74 * t29 - 0.72D2 * t115 * t154 * t18 * t70 + 0.
     #576D3 * t143 * t130 - 0.64D2 * t147 * t185 - 0.288D3 * t189 * t181
     # * t111
      t406 = -0.2808D4 * t115 * t74 * t116 + 0.72D2 * t269 - 0.490D3 * t
     #288 - 0.128D3 * t301 - 0.156D3 * t306 - 0.128D3 * t311 - t316 - 0.
     #54D2 * t318 - 0.108D3 * t321 - 0.54D2 * t326 - 0.72D2 * t34 * t35 
     #* t292 * t39 + 0.2808D4 * t303 * t276 * t39 - 0.2808D4 * t285 * t2
     #81 * t39 - 0.72D2 * t269 * t3 * t6 * t20 * t24
      t420 = 0.64D2 * t191 - t206 - 0.512D3 * t234 - 0.28372D5 * t306 - 
     #0.128D3 * t252 - 0.304D3 * t31 + 0.64D2 * t59 - 0.8D1 * t258 + 0.4
     #D1 * t318 - 0.128D3 * t126 + 0.256D3 * t136
      t430 = t302 + t312 - 0.252D3 * t326 + 0.74296D5 * t262 + 0.8D1 * t
     #321 - 0.28224D5 * t118 - 0.25220D5 * t288 + 0.64D2 * t226 - 0.8D1 
     #* t72 - 0.128D3 * t131 + 0.128D3 * t51
      rrqg2qgh21J3 = (wd * (t138 + t207 + t260 + t328) + wd * (t343 + t3
     #56 + t379 + t406) + wd * (t420 + t430)) / t1 / t18 / t27 / z / 0.3
     #141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh21J4
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
      t14 = s * t3
      t15 = t7 * x1
      t20 = s - t14 * t15 * x3 - t14 * t15 * t13
      t21 = t13 * t20
      t22 = 0.1D1 - x1
      t23 = t7 * t22
      t26 = 0.1D1 - x4
      t29 = s - t14 * t23 * x4 - t14 * t23 * t26
      t30 = t29 * t22
      t34 = cos(x2 * 0.3141592653589793D1)
      t36 = x4 * t26
      t38 = Sqrt(x3 * t13 * t36)
      t41 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t38
      t42 = t30 * t41
      t44 = t12 * t21 * t42
      t46 = x3 ** 2
      t47 = t46 * t20
      t48 = t30 * x4
      t50 = t12 * t47 * t48
      t52 = s * t1
      t53 = t52 * t5
      t54 = t9 * x1
      t55 = t53 * t54
      t56 = t20 ** 2
      t57 = x3 * t56
      t58 = t22 ** 2
      t59 = t29 * t58
      t60 = x4 ** 2
      t61 = t59 * t60
      t64 = 0.32D2 * t55 * t57 * t61
      t65 = t26 ** 2
      t66 = t59 * t65
      t68 = t55 * t57 * t66
      t70 = t2 * t4
      t71 = t8 * t10
      t73 = t29 * z
      t75 = t70 * t71 * t47 * t73
      t79 = t20 * t29
      t84 = 0.1728D4 * t6 * t11 * t13 * t79 * t22 * t41 * z
      t85 = t2 * t3
      t86 = t85 * t15
      t87 = z ** 2
      t91 = 0.1728D4 * t86 * t21 * t29 * t87
      t92 = t52 * t3
      t94 = t13 * t56
      t97 = 0.3456D4 * t92 * t15 * t94 * t73
      t98 = t8 * x1
      t99 = t70 * t98
      t100 = x3 * t20
      t102 = t99 * t100 * t48
      t106 = 0.72D2 * t55 * t94 * t66
      t107 = t6 * t54
      t110 = 0.1440D4 * t107 * t21 * t66
      t112 = t107 * t100 * t66
      t114 = t52 * t56
      t115 = t114 * t29
      t116 = t4 * t8
      t121 = t70 * t8
      t122 = t13 ** 2
      t123 = t10 * t122
      t127 = 0.144D3 * t44 - 0.148D3 * t50 - t64 - 0.32D2 * t68 - 0.128D
     #3 * t75 + t84 - t91 + t97 - 0.148D3 * t102 - t106 + t110 + 0.312D3
     # * t112 + 0.36D2 * t115 * t116 * t58 * t60 - 0.576D3 * t121 * t123
     # * t79
      t128 = t85 * t7
      t129 = x1 * t13
      t133 = t6 * t9
      t134 = t10 * x1
      t136 = t134 * t122 * t13
      t140 = t92 * t7
      t141 = x1 * x3
      t142 = t56 * t29
      t144 = t140 * t141 * t142
      t147 = t2 * s * t87
      t151 = t147 * t4 * t71 * t46 * t29
      t152 = 0.64D2 * t151
      t153 = t30 * t26
      t155 = t12 * t47 * t153
      t157 = t52 * t4
      t158 = t157 * t98
      t160 = t158 * t94 * t153
      t162 = t157 * t8
      t163 = t10 * t46
      t165 = t162 * t163 * t142
      t170 = t147 * t3 * t15 * x3 * t29
      t171 = 0.64D2 * t170
      t175 = 0.72D2 * t115 * t116 * t58 * t65
      t177 = t128 * t141 * t79
      t179 = t53 * t11
      t181 = t179 * t57 * t42
      t184 = t121 * t163 * t79
      t189 = t133 * t134 * t46 * x3 * t79
      t191 = t4 ** 2
      t192 = t52 * t191
      t193 = t8 ** 2
      t194 = t193 * t10
      t195 = t194 * t13
      t196 = t192 * t195
      t198 = t58 * x4 * t41
      t199 = t142 * t198
      t202 = -0.288D3 * t128 * t129 * t79 - 0.288D3 * t133 * t136 * t79 
     #+ 0.17D2 * t144 + t152 - 0.128D3 * t155 - 0.2304D4 * t160 + 0.275D
     #3 * t165 + t171 + t175 + 0.47D2 * t177 - 0.306D3 * t181 + 0.94D2 *
     # t184 + 0.47D2 * t189 - 0.144D3 * t196 * t199
      t204 = t191 * t3
      t207 = t193 * t7 * t134
      t208 = t2 * t204 * t207
      t209 = t41 ** 2
      t210 = t59 * t209
      t212 = t208 * t100 * t210
      t220 = 0.1728D4 * t70 * t98 * t13 * t79 * z * t22 * x4
      t222 = t86 * t100 * t73
      t223 = 0.128D3 * t222
      t224 = t2 * t191
      t225 = t194 * x3
      t226 = t224 * t225
      t227 = t58 * t26
      t228 = t227 * t41
      t229 = t79 * t228
      t230 = t226 * t229
      t232 = t79 * t198
      t233 = t226 * t232
      t235 = t224 * t195
      t236 = t235 * t229
      t238 = t192 * t225
      t239 = t142 * t228
      t240 = t238 * t239
      t245 = t114 * t29 * t5
      t246 = t9 * t58
      t264 = t52 * t204 * t207
      t269 = t99 * t100 * t153
      t272 = t158 * t57 * t153
      t274 = 0.748D3 * t212 - t220 - t223 - 0.880D3 * t230 - 0.16D2 * t2
     #33 - 0.1440D4 * t236 + 0.32D2 * t240 - 0.72D2 * t196 * t239 + 0.14
     #4D3 * t245 * t246 * x4 * x1 * t41 - 0.216D3 * t245 * t246 * t26 * 
     #x1 * t41 + 0.252D3 * t114 * t29 * t191 * t194 * t58 * t209 - 0.36D
     #2 * t264 * t94 * t210 - 0.128D3 * t269 + 0.528D3 * t272
      t276 = t158 * t57 * t48
      t278 = t54 * x3
      t280 = t227 * x4
      t281 = t142 * t280
      t282 = t53 * t278 * t281
      t285 = t79 * t280
      t286 = t6 * t278 * t285
      t299 = 0.32D2 * t264 * t57 * t210
      t301 = t224 * t193 * t134
      t303 = t301 * t47 * t42
      t306 = t12 * t100 * t42
      t309 = t99 * t21 * t48
      t311 = t122 * t20
      t313 = t12 * t311 * t48
      t315 = t54 * t13
      t320 = t301 * t311 * t42
      t323 = t107 * t100 * t61
      t326 = t6 * t315 * t285
      t328 = 0.122D3 * t276 - 0.32D2 * t282 - 0.240D3 * t286 - 0.252D3 *
     # t55 * t94 * t61 - 0.72D2 * t114 * t29 * t4 * t8 * t58 * t36 - t29
     #9 + 0.96D2 * t303 + 0.96D2 * t306 + 0.144D3 * t309 + 0.144D3 * t31
     #3 - 0.216D3 * t53 * t315 * t281 + 0.144D3 * t320 - 0.12D2 * t323 +
     # 0.1440D4 * t326
      t347 = -0.72D2 * t44 + 0.216D3 * t50 + t64 + 0.64D2 * t68 + 0.256D
     #3 * t75 - t84 + t91 - t97 + 0.216D3 * t102 + t106 - 0.288D3 * t107
     # * t21 * t61 + 0.2808D4 * t158 * t94 * t48 - 0.72D2 * t179 * t122 
     #* t56 * t153
      t366 = -t110 - 0.264D3 * t112 - 0.490D3 * t144 - 0.128D3 * t151 + 
     #0.496D3 * t155 + 0.4320D4 * t160 - 0.156D3 * t165 - 0.128D3 * t170
     # + 0.576D3 * t235 * t232 - 0.64D2 * t238 * t199 - 0.288D3 * t208 *
     # t21 * t210 - 0.2808D4 * t179 * t94 * t42 - t175 - 0.54D2 * t177
      t392 = 0.190D3 * t181 - 0.108D3 * t184 - 0.2808D4 * t140 * t129 * 
     #t142 + 0.2808D4 * t162 * t123 * t142 - 0.72D2 * t53 * t9 * t136 * 
     #t142 - 0.54D2 * t189 - 0.72D2 * t115 * t3 * t7 * t22 * t26 - 0.592
     #D3 * t212 + t220 + 0.256D3 * t222 + 0.72D2 * t115 + 0.640D3 * t230
     # + 0.608D3 * t233 + 0.1728D4 * t236
      t406 = -0.64D2 * t240 + 0.496D3 * t269 - 0.752D3 * t272 - 0.134D3 
     #* t276 + 0.64D2 * t282 - 0.64D2 * t286 + t299 - 0.640D3 * t303 - 0
     #.640D3 * t306 - 0.72D2 * t309 - 0.72D2 * t313 - 0.72D2 * t320 - 0.
     #16D2 * t323 - 0.1728D4 * t326
      t420 = 0.64D2 * t212 - t223 - 0.512D3 * t75 - 0.28372D5 * t165 - 0
     #.128D3 * t50 - 0.304D3 * t276 + 0.64D2 * t323 - 0.8D1 * t269 + 0.4
     #D1 * t177 - 0.128D3 * t230 + 0.256D3 * t303
      t430 = t152 + t171 - 0.252D3 * t189 + 0.74296D5 * t272 + 0.8D1 * t
     #184 - 0.28224D5 * t181 - 0.25220D5 * t144 + 0.64D2 * t112 - 0.8D1 
     #* t155 - 0.128D3 * t233 + 0.128D3 * t286
      rrqg2qgh21J4 = (wd * (t127 + t202 + t274 + t328) + wd * (t347 + t3
     #66 + t392 + t406) + wd * (t420 + t430)) / t1 / t56 / t29 / z / 0.3
     #141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh21J5
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
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t5 * t3
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = x1 ** 2
      t13 = t12 * x1
      t14 = t10 * t8 * t13
      t15 = t2 * t6 * t14
      t16 = 0.1D1 - x3
      t17 = s * t3
      t18 = t8 * x1
      t23 = s - t17 * t18 * x3 - t17 * t18 * t16
      t24 = t23 ** 2
      t25 = t16 * t24
      t26 = 0.1D1 - x1
      t27 = t8 * t26
      t30 = 0.1D1 - x4
      t33 = s - t17 * t27 * x4 - t17 * t27 * t30
      t34 = t26 ** 2
      t35 = t33 * t34
      t39 = cos(x2 * 0.3141592653589793D1)
      t41 = t30 * x4
      t43 = Sqrt(x3 * t16 * t41)
      t46 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t39 * t43
      t47 = t46 ** 2
      t48 = t47 * t35
      t52 = t1 ** 2
      t53 = t4 * t3
      t54 = t52 * t53
      t55 = t9 * t8
      t56 = t55 * t12
      t57 = t54 * t56
      t58 = t16 * t23
      t59 = t33 * t26
      t60 = t59 * t46
      t62 = t57 * t58 * t60
      t64 = t2 * t5
      t65 = t10 * t12
      t66 = t65 * t16
      t67 = t64 * t66
      t68 = t24 * t33
      t70 = t34 * x4 * t46
      t71 = t68 * t70
      t74 = x3 ** 2
      t75 = t74 * t23
      t76 = t59 * x4
      t78 = t57 * t75 * t76
      t80 = t52 * t4
      t81 = t9 * x1
      t82 = t80 * t81
      t83 = x3 * t23
      t85 = t82 * t83 * t76
      t87 = t59 * t30
      t89 = t82 * t83 * t87
      t91 = t2 * t4
      t92 = t91 * t81
      t93 = x3 * t24
      t95 = t92 * t93 * t87
      t98 = t92 * t93 * t76
      t101 = t57 * t83 * t60
      t103 = t52 * t5
      t104 = t103 * t66
      t105 = t23 * t33
      t106 = t34 * t30
      t107 = t106 * t46
      t108 = t105 * t107
      t109 = t104 * t108
      t111 = t65 * x3
      t112 = t64 * t111
      t113 = t68 * t107
      t114 = t112 * t113
      t122 = 0.1728D4 * t80 * t81 * t16 * t105 * z * t26 * x4
      t123 = t55 * x1
      t124 = t54 * t123
      t125 = t30 ** 2
      t126 = t35 * t125
      t128 = t124 * t83 * t126
      t130 = t2 * t53
      t131 = t123 * t130
      t133 = t131 * t93 * t126
      t135 = -0.36D2 * t15 * t25 * t48 + 0.144D3 * t62 - 0.144D3 * t67 *
     # t71 - 0.148D3 * t78 - 0.148D3 * t85 - 0.128D3 * t89 + 0.528D3 * t
     #95 + 0.122D3 * t98 + 0.96D2 * t101 - 0.1440D4 * t109 + 0.32D2 * t1
     #14 - t122 + 0.312D3 * t128 - 0.32D2 * t133
      t136 = t52 * t3
      t137 = t136 * t18
      t138 = t33 * z
      t140 = t137 * t83 * t138
      t141 = 0.128D3 * t140
      t142 = x4 ** 2
      t143 = t35 * t142
      t147 = t123 * t16
      t149 = t106 * x4
      t150 = t68 * t149
      t155 = 0.1440D4 * t124 * t58 * t126
      t156 = t9 * t12
      t159 = t80 * t156 * t75 * t138
      t167 = 0.1728D4 * t54 * t56 * t16 * t105 * t26 * t46 * z
      t168 = z ** 2
      t172 = 0.1728D4 * t137 * t58 * t33 * t168
      t173 = t103 * t111
      t174 = t105 * t70
      t175 = t173 * t174
      t178 = t103 * t10 * t13
      t180 = t178 * t75 * t60
      t186 = 0.32D2 * t15 * t93 * t48
      t187 = t130 * t56
      t189 = t187 * t93 * t60
      t191 = t16 ** 2
      t192 = t191 * t23
      t194 = t178 * t192 * t60
      t196 = t2 * t24
      t198 = t196 * t33 * t53
      t199 = t55 * t34
      t205 = -t141 - 0.252D3 * t131 * t25 * t143 - 0.216D3 * t130 * t147
     # * t150 + t155 - 0.128D3 * t159 + t167 - t172 - 0.16D2 * t175 + 0.
     #96D2 * t180 - 0.72D2 * t67 * t113 - t186 - 0.306D3 * t189 + 0.144D
     #3 * t194 + 0.144D3 * t198 * t199 * x4 * x1 * t46
      t218 = t196 * t33
      t219 = t4 * t9
      t224 = t80 * t9
      t225 = t12 * t191
      t229 = t136 * t8
      t230 = x1 * t16
      t234 = t2 * t3
      t235 = t234 * t8
      t236 = x1 * x3
      t238 = t235 * t236 * t68
      t240 = t54 * t55
      t242 = t13 * t191 * t16
      t247 = t52 * s * t168
      t251 = t247 * t4 * t156 * t74 * t33
      t252 = 0.64D2 * t251
      t253 = t91 * t9
      t254 = t12 * t74
      t256 = t253 * t254 * t68
      t261 = t247 * t3 * t18 * x3 * t33
      t262 = 0.64D2 * t261
      t266 = 0.72D2 * t218 * t219 * t34 * t125
      t268 = t229 * t236 * t105
      t271 = t224 * t254 * t105
      t276 = t240 * t13 * t74 * x3 * t105
      t278 = -0.216D3 * t198 * t199 * t30 * x1 * t46 + 0.252D3 * t196 * 
     #t33 * t5 * t65 * t34 * t47 + 0.36D2 * t218 * t219 * t34 * t142 - 0
     #.576D3 * t224 * t225 * t105 - 0.288D3 * t229 * t230 * t105 + 0.17D
     #2 * t238 - 0.288D3 * t240 * t242 * t105 + t252 + 0.275D3 * t256 + 
     #t262 + t266 + 0.47D2 * t268 + 0.94D2 * t271 + 0.47D2 * t276
      t280 = t52 * t6 * t14
      t282 = t280 * t83 * t48
      t286 = 0.32D2 * t131 * t93 * t143
      t288 = t57 * t75 * t87
      t291 = t92 * t25 * t87
      t293 = t173 * t108
      t298 = 0.3456D4 * t234 * t18 * t25 * t138
      t300 = t82 * t58 * t76
      t303 = t57 * t192 * t76
      t305 = t123 * x3
      t307 = t130 * t305 * t150
      t310 = t105 * t149
      t311 = t54 * t305 * t310
      t314 = t124 * t83 * t143
      t317 = t54 * t147 * t310
      t321 = 0.72D2 * t131 * t25 * t126
      t328 = 0.748D3 * t282 - t286 - 0.128D3 * t288 - 0.2304D4 * t291 - 
     #0.880D3 * t293 + t298 + 0.144D3 * t300 + 0.144D3 * t303 - 0.32D2 *
     # t307 - 0.240D3 * t311 - 0.12D2 * t314 + 0.1440D4 * t317 - t321 - 
     #0.72D2 * t196 * t33 * t4 * t9 * t34 * t41
      t346 = -0.72D2 * t62 + 0.216D3 * t78 + 0.216D3 * t85 + 0.496D3 * t
     #89 - 0.752D3 * t95 - 0.134D3 * t98 - 0.640D3 * t101 + 0.1728D4 * t
     #109 - 0.64D2 * t114 + t122 + 0.2808D4 * t92 * t25 * t76 - 0.264D3 
     #* t128 + 0.64D2 * t133
      t362 = 0.256D3 * t140 - 0.72D2 * t187 * t191 * t24 * t87 + 0.576D3
     # * t104 * t174 - 0.64D2 * t112 * t71 - t155 + 0.256D3 * t159 - t16
     #7 + t172 + 0.608D3 * t175 - 0.640D3 * t180 + t186 + 0.190D3 * t189
     # + 0.72D2 * t218 - 0.72D2 * t194
      t390 = -0.490D3 * t238 - 0.128D3 * t251 - 0.156D3 * t256 - 0.128D3
     # * t261 - t266 - 0.54D2 * t268 - 0.108D3 * t271 - 0.54D2 * t276 - 
     #0.72D2 * t130 * t55 * t242 * t68 + 0.2808D4 * t253 * t225 * t68 - 
     #0.2808D4 * t235 * t230 * t68 - 0.72D2 * t218 * t3 * t8 * t26 * t30
     # - 0.288D3 * t124 * t58 * t143 - 0.592D3 * t282
      t406 = t286 + 0.496D3 * t288 + 0.4320D4 * t291 + 0.640D3 * t293 - 
     #t298 - 0.288D3 * t280 * t58 * t48 - 0.2808D4 * t187 * t25 * t60 - 
     #0.72D2 * t300 - 0.72D2 * t303 + 0.64D2 * t307 - 0.64D2 * t311 - 0.
     #16D2 * t314 - 0.1728D4 * t317 + t321
      t420 = 0.64D2 * t282 - t141 - 0.512D3 * t159 - 0.28372D5 * t256 - 
     #0.128D3 * t78 - 0.304D3 * t98 + 0.64D2 * t314 - 0.8D1 * t89 + 0.4D
     #1 * t268 - 0.128D3 * t293 + 0.256D3 * t180
      t430 = t252 + t262 - 0.252D3 * t276 + 0.74296D5 * t95 + 0.8D1 * t2
     #71 - 0.28224D5 * t189 - 0.25220D5 * t238 + 0.64D2 * t128 - 0.8D1 *
     # t288 - 0.128D3 * t175 + 0.128D3 * t311
      rrqg2qgh21J5 = (wd * (t135 + t205 + t278 + t328) + wd * (t346 + t3
     #62 + t390 + t406) + wd * (t420 + t430)) / t1 / t24 / t33 / z / 0.3
     #141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh21J6
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
      t4 = t3 ** 2
      t5 = t4 ** 2
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x1 ** 2
      t11 = t9 * t10
      t12 = t11 * x3
      t13 = t2 * t5 * t12
      t14 = s * t3
      t15 = t7 * x1
      t18 = 0.1D1 - x3
      t21 = s - t14 * t15 * x3 - t14 * t15 * t18
      t22 = t21 ** 2
      t23 = 0.1D1 - x1
      t24 = t7 * t23
      t27 = 0.1D1 - x4
      t30 = s - t14 * t24 * x4 - t14 * t24 * t27
      t31 = t22 * t30
      t32 = t23 ** 2
      t33 = t32 * t27
      t37 = cos(x2 * 0.3141592653589793D1)
      t41 = Sqrt(x3 * t18 * x4 * t27)
      t44 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t37 * t41
      t45 = t33 * t44
      t49 = t1 ** 2
      t50 = t49 * t5
      t51 = t10 * x1
      t53 = t50 * t9 * t51
      t54 = t18 ** 2
      t55 = t54 * t21
      t56 = t30 * t23
      t57 = t56 * t44
      t61 = t5 * t3
      t64 = t9 * t7 * t51
      t66 = x3 * t22
      t67 = t30 * t32
      t68 = t44 ** 2
      t69 = t67 * t68
      t73 = t4 * t3
      t74 = t2 * t73
      t75 = t8 * t7
      t76 = t75 * t10
      t77 = t74 * t76
      t79 = t77 * t66 * t57
      t81 = t50 * t12
      t82 = t21 * t30
      t83 = t82 * t45
      t84 = t81 * t83
      t86 = t49 * t4
      t87 = t8 * x1
      t88 = t86 * t87
      t89 = t18 * t21
      t90 = t56 * x4
      t94 = t49 * t73
      t95 = t94 * t76
      t99 = t75 * x1
      t100 = t94 * t99
      t101 = t27 ** 2
      t102 = t67 * t101
      t106 = x3 * t21
      t108 = t100 * t106 * t102
      t110 = t74 * t99
      t114 = x4 ** 2
      t115 = t67 * t114
      t125 = t2 * t4
      t126 = t125 * t8
      t131 = -0.64D2 * t13 * t31 * t45 - 0.72D2 * t53 * t55 * t57 + 0.32
     #D2 * t2 * t61 * t64 * t66 * t69 + 0.190D3 * t79 + 0.640D3 * t84 - 
     #0.72D2 * t88 * t89 * t90 - 0.72D2 * t95 * t55 * t90 - 0.1440D4 * t
     #100 * t89 * t102 - 0.264D3 * t108 + 0.64D2 * t110 * t66 * t102 - 0
     #.288D3 * t100 * t89 * t115 - 0.72D2 * t74 * t75 * t51 * t54 * t18 
     #* t31 + 0.2808D4 * t126 * t10 * t54 * t31
      t132 = t2 * t3
      t133 = t132 * t7
      t139 = t2 * t22 * t30
      t145 = x1 * x3
      t147 = t133 * t145 * t31
      t150 = z ** 2
      t151 = t49 * s * t150
      t153 = t8 * t10
      t154 = x3 ** 2
      t157 = t151 * t4 * t153 * t154 * t30
      t159 = t10 * t154
      t161 = t126 * t159 * t31
      t166 = t151 * t3 * t15 * x3 * t30
      t173 = t49 * t3
      t176 = t173 * t7 * t145 * t82
      t180 = t86 * t8 * t159 * t82
      t186 = t94 * t75 * t51 * t154 * x3 * t82
      t189 = t154 * t21
      t190 = t30 * z
      t192 = t86 * t153 * t189 * t190
      t194 = t125 * t87
      t195 = t56 * t27
      t197 = t194 * t66 * t195
      t200 = t194 * t66 * t90
      t202 = t99 * x3
      t204 = t33 * x4
      t208 = -0.2808D4 * t133 * x1 * t18 * t31 - 0.72D2 * t139 * t3 * t7
     # * t23 * t27 - 0.490D3 * t147 - 0.128D3 * t157 - 0.156D3 * t161 - 
     #0.128D3 * t166 - 0.72D2 * t139 * t4 * t8 * t32 * t101 - 0.54D2 * t
     #176 - 0.108D3 * t180 - 0.54D2 * t186 + 0.256D3 * t192 - 0.752D3 * 
     #t197 - 0.134D3 * t200 + 0.64D2 * t74 * t202 * t31 * t204
      t211 = t82 * t204
      t212 = t94 * t202 * t211
      t215 = t100 * t106 * t115
      t218 = t49 * t61 * t64
      t222 = t18 * t22
      t234 = t50 * t11 * t18
      t236 = t32 * x4 * t44
      t237 = t82 * t236
      t244 = t95 * t189 * t90
      t250 = t88 * t106 * t195
      t252 = t81 * t237
      t255 = t53 * t189 * t57
      t260 = -0.64D2 * t212 - 0.16D2 * t215 - 0.288D3 * t218 * t89 * t69
     # - 0.2808D4 * t77 * t222 * t57 + 0.2808D4 * t194 * t222 * t90 - 0.
     #72D2 * t77 * t54 * t22 * t195 + 0.576D3 * t234 * t237 - 0.64D2 * t
     #13 * t31 * t236 + 0.216D3 * t244 + 0.216D3 * t88 * t106 * t90 + 0.
     #496D3 * t250 + 0.608D3 * t252 - 0.640D3 * t255 - 0.640D3 * t95 * t
     #106 * t57
      t272 = t95 * t189 * t195
      t287 = t173 * t15
      t300 = t218 * t106 * t69
      t310 = t287 * t106 * t190
      t312 = 0.1728D4 * t234 * t83 + 0.32D2 * t110 * t66 * t115 + 0.72D2
     # * t139 - 0.1728D4 * t94 * t99 * t18 * t211 + 0.496D3 * t272 + 0.4
     #320D4 * t194 * t222 * t195 + 0.72D2 * t110 * t222 * t102 - 0.1728D
     #4 * t94 * t76 * t18 * t82 * t23 * t44 * z + 0.1728D4 * t287 * t89 
     #* t30 * t150 - 0.3456D4 * t132 * t15 * t222 * t190 - 0.72D2 * t95 
     #* t89 * t57 - 0.592D3 * t300 + 0.1728D4 * t86 * t87 * t18 * t82 * 
     #z * t23 * x4 + 0.256D3 * t310
      t327 = 0.64D2 * t300 - 0.128D3 * t310 - 0.512D3 * t192 - 0.28372D5
     # * t161 - 0.128D3 * t244 - 0.304D3 * t200 + 0.64D2 * t215 - 0.8D1 
     #* t250 + 0.4D1 * t176 - 0.128D3 * t84 + 0.256D3 * t255
      t339 = 0.64D2 * t157 + 0.64D2 * t166 - 0.252D3 * t186 + 0.74296D5 
     #* t197 + 0.8D1 * t180 - 0.28224D5 * t79 - 0.25220D5 * t147 + 0.64D
     #2 * t108 - 0.8D1 * t272 - 0.128D3 * t252 + 0.128D3 * t212
      rrqg2qgh21J6 = (wd * (t131 + t208 + t260 + t312) + wd * (t327 + t3
     #39)) / t1 / t22 / t30 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh21J7
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
      t13 = t12 * x1
      t16 = s * t3
      t17 = t8 * x1
      t20 = 0.1D1 - x3
      t23 = s - t16 * t17 * x3 - t16 * t17 * t20
      t24 = x3 * t23
      t25 = 0.1D1 - x1
      t26 = t8 * t25
      t29 = 0.1D1 - x4
      t32 = s - t16 * t26 * x4 - t16 * t26 * t29
      t33 = t25 ** 2
      t34 = t32 * t33
      t38 = cos(x2 * 0.3141592653589793D1)
      t42 = Sqrt(x3 * t20 * x4 * t29)
      t45 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t38 * t42
      t46 = t45 ** 2
      t51 = t2 * t3
      t53 = t32 * z
      t57 = t2 * t4
      t58 = t9 * t12
      t60 = x3 ** 2
      t61 = t60 * t23
      t65 = s * t1
      t66 = t65 * t4
      t68 = t12 * t60
      t69 = t23 ** 2
      t70 = t69 * t32
      t74 = t4 * t3
      t75 = t2 * t74
      t76 = t9 * t8
      t77 = t76 * t12
      t78 = t75 * t77
      t79 = t32 * t25
      t80 = t79 * x4
      t84 = t9 * x1
      t85 = t66 * t84
      t86 = x3 * t69
      t90 = t76 * x1
      t91 = t75 * t90
      t92 = x4 ** 2
      t98 = t79 * t29
      t103 = x1 * x3
      t104 = t23 * t32
      t108 = t2 * t5
      t111 = t108 * t10 * t12 * x3
      t112 = t33 * t29
      t119 = t79 * t45
      t123 = 0.64D2 * t2 * t5 * t3 * t10 * t8 * t13 * t24 * t34 * t46 - 
     #0.128D3 * t51 * t17 * t24 * t53 - 0.512D3 * t57 * t58 * t61 * t53 
     #- 0.28372D5 * t66 * t9 * t68 * t70 - 0.128D3 * t78 * t61 * t80 - 0
     #.304D3 * t85 * t86 * t80 + 0.64D2 * t91 * t24 * t34 * t92 - 0.8D1 
     #* t57 * t84 * t24 * t98 + 0.4D1 * t51 * t8 * t103 * t104 - 0.128D3
     # * t111 * t104 * t112 * t45 + 0.256D3 * t108 * t10 * t13 * t61 * t
     #119
      t125 = z ** 2
      t126 = t2 * s * t125
      t160 = t29 ** 2
      t179 = 0.64D2 * t126 * t4 * t58 * t60 * t32 + 0.64D2 * t126 * t3 *
     # t17 * x3 * t32 - 0.252D3 * t75 * t76 * t13 * t60 * x3 * t104 + 0.
     #74296D5 * t85 * t86 * t98 + 0.8D1 * t57 * t9 * t68 * t104 - 0.2822
     #4D5 * t65 * t74 * t77 * t86 * t119 - 0.25220D5 * t65 * t3 * t8 * t
     #103 * t70 + 0.64D2 * t91 * t24 * t34 * t160 - 0.8D1 * t78 * t61 * 
     #t98 - 0.128D3 * t111 * t104 * t33 * x4 * t45 + 0.128D3 * t75 * t90
     # * x3 * t104 * t112 * x4
      rrqg2qgh21J7 = wd * (t123 + t179) / t1 / t69 / t32 / z / 0.3141592
     #653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J1
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
      t29 = t28 * t24
      t33 = t14 * t18
      t37 = t4 * t3
      t39 = t6 * t7
      t40 = x1 ** 2
      t41 = t39 * t40
      t42 = t2 * t37 * t41
      t46 = cos(x2 * 0.3141592653589793D1)
      t50 = Sqrt(x3 * t14 * x4 * t24)
      t53 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t46 * t50
      t54 = t28 * t53
      t65 = t1 ** 2
      t66 = t65 * t37
      t67 = t66 * t41
      t68 = t14 ** 2
      t69 = t68 * t18
      t70 = t20 * x4
      t75 = t66 * t39 * x1
      t76 = t20 ** 2
      t77 = x4 ** 2
      t82 = t65 * t4
      t83 = t82 * t8
      t87 = x3 ** 2
      t88 = t87 * t18
      t95 = t4 ** 2
      t96 = t95 * t65
      t97 = t7 ** 2
      t99 = t96 * t97 * t40
      t100 = t24 ** 2
      t101 = t76 * t100
      t108 = t20 * t24
      t124 = -0.14976D5 * t9 * t19 * t29 + 0.122D3 * t9 * t33 * t29 + 0.
     #14976D5 * t42 * t19 * t54 + 0.275D3 * t9 * t33 * t28 * x4 + 0.17D2
     # * t42 * t33 * t54 + 0.400D3 * t67 * t69 * t70 + 0.192D3 * t75 * t
     #33 * t76 * t77 - 0.530D3 * t83 * t33 * t70 - 0.14832D5 * t67 * t88
     # * t70 + 0.144D3 * t83 * t19 * t70 - 0.144D3 * t99 * t88 * t101 + 
     #0.10D2 * t75 * t33 * t101 + 0.328D3 * t67 * t69 * t108 - 0.328D3 *
     # t83 * t33 * t108 - 0.14976D5 * t67 * t88 * t108 + 0.288D3 * t83 *
     # t19 * t108 - 0.144D3 * t75 * t19 * t101
      t125 = t76 * t24
      t126 = t125 * x4
      t136 = t40 ** 2
      t139 = t53 ** 2
      t144 = t20 * t53
      t148 = t40 * x1
      t164 = t125 * t53
      t171 = t5 * t7
      t172 = t40 * t68
      t173 = t18 * t27
      t178 = t2 * t3 * t6
      t179 = x1 * t14
      t183 = x1 * x3
      t187 = t40 * t87
      t192 = t65 * t3 * t6
      t205 = t82 * t7
      t213 = t87 ** 2
      t218 = 0.182D3 * t75 * t33 * t126 - 0.144D3 * t75 * t19 * t126 - 0
     #.144D3 * t65 * t95 * t4 * t97 * t7 * t136 * t88 * t76 * t139 - 0.1
     #4976D5 * t67 * t19 * t144 + 0.576D3 * t96 * t97 * t148 * t88 * t14
     #4 + 0.14832D5 * t99 * t19 * t76 * x4 * t53 + 0.288D3 * t65 * t95 *
     # t3 * t97 * t6 * t148 * t88 * t164 + 0.14832D5 * t99 * t19 * t164 
     #+ 0.528D3 * t171 * t172 * t173 - 0.306D3 * t178 * t179 * t173 + 0.
     #14976D5 * t178 * t183 * t173 - 0.14976D5 * t171 * t187 * t173 - 0.
     #144D3 * t192 * t183 * t18 + 0.56D2 * t66 * t39 * t148 * t68 * t14 
     #* t18 + 0.430D3 * t192 * t179 * t18 + 0.14832D5 * t205 * t187 * t1
     #8 - 0.584D3 * t205 * t172 * t18 - 0.144D3 * t96 * t97 * t136 * t21
     #3 * t18
      rrqg2qgh22J1 = wd * (t124 + t218) / t1 / t18 / t27 / z / 0.3141592
     #653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J2
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
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t5 * t7
      t9 = x1 ** 2
      t10 = x3 ** 2
      t11 = t9 * t10
      t12 = s * t3
      t13 = t6 * x1
      t16 = 0.1D1 - x3
      t20 = (s - t12 * t13 * x3 - t12 * t13 * t16) ** 2
      t21 = 0.1D1 - x1
      t22 = t6 * t21
      t25 = 0.1D1 - x4
      t28 = s - t12 * t22 * x4 - t12 * t22 * t25
      t29 = t20 * t28
      t31 = t8 * t11 * t29
      t34 = t2 * t3 * t6
      t35 = x1 * x3
      t37 = t34 * t35 * t29
      t39 = x1 * t16
      t41 = t34 * t39 * t29
      t43 = t16 ** 2
      t44 = t9 * t43
      t46 = t8 * t44 * t29
      t48 = t7 * x1
      t49 = t5 * t48
      t50 = t16 * t20
      t51 = t28 * t21
      t52 = t51 * x4
      t54 = t49 * t50 * t52
      t56 = t4 * t3
      t58 = t6 * t7
      t59 = t58 * t9
      t60 = t2 * t56 * t59
      t64 = cos(x2 * 0.3141592653589793D1)
      t68 = Sqrt(x3 * t16 * x4 * t25)
      t71 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t64 * t68
      t72 = t51 * t71
      t74 = t60 * t50 * t72
      t76 = t1 ** 2
      t77 = t76 * t56
      t78 = t77 * t59
      t79 = t43 * t20
      t80 = t21 * x4
      t82 = t78 * t79 * t80
      t84 = t4 ** 2
      t85 = t76 * t84
      t86 = t7 ** 2
      t88 = t9 ** 2
      t89 = t10 ** 2
      t93 = 0.144D3 * t85 * t86 * t88 * t89 * t20
      t95 = t77 * t58 * x1
      t96 = t21 ** 2
      t97 = t96 * t25
      t98 = t97 * x4
      t100 = t95 * t50 * t98
      t102 = x3 * t20
      t104 = t95 * t102 * t98
      t106 = x4 ** 2
      t107 = t96 * t106
      t109 = t95 * t50 * t107
      t111 = t76 * t4
      t112 = t111 * t48
      t114 = t112 * t50 * t80
      t117 = t76 * t84 * t3
      t118 = t86 * t6
      t119 = t9 * x1
      t121 = t117 * t118 * t119
      t122 = t10 * t20
      t123 = t97 * t71
      t125 = t121 * t122 * t123
      t128 = t85 * t86 * t9
      t130 = t128 * t102 * t123
      t137 = t71 ** 2
      t138 = t96 * t137
      t141 = 0.144D3 * t76 * t84 * t4 * t86 * t7 * t88 * t122 * t138
      t142 = t21 * t71
      t144 = t78 * t102 * t142
      t147 = t85 * t86 * t119
      t149 = t147 * t122 * t142
      t151 = -0.14976D5 * t31 + 0.14976D5 * t37 - 0.306D3 * t41 + 0.528D
     #3 * t46 + 0.275D3 * t54 + 0.17D2 * t74 + 0.400D3 * t82 - t93 + 0.1
     #82D3 * t100 - 0.144D3 * t104 + 0.192D3 * t109 - 0.530D3 * t114 + 0
     #.288D3 * t125 + 0.14832D5 * t130 - t141 - 0.14976D5 * t144 + 0.576
     #D3 * t149
      t152 = t21 * t25
      t154 = t78 * t122 * t152
      t157 = t112 * t102 * t152
      t159 = t25 ** 2
      t160 = t96 * t159
      t162 = t95 * t102 * t160
      t164 = t51 * t25
      t166 = t49 * t102 * t164
      t169 = t96 * x4 * t71
      t171 = t128 * t102 * t169
      t174 = t78 * t122 * t80
      t177 = t112 * t102 * t80
      t179 = t111 * t7
      t181 = t179 * t44 * t20
      t184 = t76 * t3 * t6
      t186 = t184 * t35 * t20
      t188 = t77 * t58
      t192 = t188 * t119 * t43 * t16 * t20
      t195 = t184 * t39 * t20
      t198 = t179 * t11 * t20
      t201 = t49 * t50 * t164
      t204 = t60 * t102 * t72
      t208 = 0.144D3 * t128 * t122 * t160
      t210 = t95 * t50 * t160
      t213 = t78 * t79 * t152
      t216 = t112 * t50 * t152
      t218 = -0.14976D5 * t154 + 0.288D3 * t157 - 0.144D3 * t162 - 0.149
     #76D5 * t166 + 0.14832D5 * t171 - 0.14832D5 * t174 + 0.144D3 * t177
     # - 0.584D3 * t181 - 0.144D3 * t186 + 0.56D2 * t192 + 0.430D3 * t19
     #5 + 0.14832D5 * t198 + 0.122D3 * t201 + 0.14976D5 * t204 - t208 + 
     #0.10D2 * t210 + 0.328D3 * t213 - 0.328D3 * t216
      t225 = t10 * x3
      t237 = 0.14616D5 * t31 - 0.14616D5 * t37 + 0.190D3 * t41 - 0.752D3
     # * t46 + 0.432D3 * t188 * t119 * t225 * t20 - 0.156D3 * t54 - 0.49
     #0D3 * t74 + 0.6192D4 * t49 * t102 * t52 - 0.288D3 * t82 + t93 - 0.
     #108D3 * t100
      t255 = 0.2160D4 * t104 - 0.30D2 * t109 + 0.444D3 * t114 - 0.216D3 
     #* t147 * t79 * t142 - 0.256D3 * t128 * t50 * t169 + 0.256D3 * t78 
     #* t50 * t142 - 0.432D3 * t125 - 0.15696D5 * t130 + t141 + 0.15552D
     #5 * t144 - 0.864D3 * t149 + 0.15552D5 * t154
      t262 = t225 * t20
      t282 = 0.1872D4 * t157 - 0.936D3 * t162 + 0.14616D5 * t166 - 0.288
     #D3 * t117 * t118 * t88 * t262 * t142 + 0.136D3 * t121 * t50 * t138
     # - 0.256D3 * t128 * t50 * t123 + 0.432D3 * t121 * t102 * t138 - 0.
     #15552D5 * t171 + 0.16D2 * t60 * t79 * t52 + 0.15552D5 * t174 - 0.2
     #160D4 * t177 + 0.544D3 * t181
      t298 = -0.936D3 * t186 + 0.24D2 * t192 - 0.542D3 * t195 - 0.15696D
     #5 * t198 + 0.432D3 * t147 * t262 * t152 + 0.3096D4 * t95 * t102 * 
     #t107 - 0.134D3 * t201 - 0.14616D5 * t204 + t208 - 0.62D2 * t210 - 
     #0.256D3 * t213 + 0.604D3 * t216
      rrqg2qgh22J2 = (wd * (t151 + t218) + wd * (t237 + t255 + t282 + t2
     #98)) / t1 / t20 / t28 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J3
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
      t21 = t6 * t20
      t24 = 0.1D1 - x4
      t27 = s - t11 * t21 * x4 - t11 * t21 * t24
      t28 = t27 * t20
      t29 = t28 * t24
      t31 = t9 * t19 * t29
      t33 = t4 * t3
      t34 = t2 * t33
      t35 = t6 * t7
      t36 = x1 ** 2
      t37 = t35 * t36
      t38 = t34 * t37
      t39 = x3 * t18
      t43 = cos(x2 * 0.3141592653589793D1)
      t47 = Sqrt(x3 * t10 * x4 * t24)
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t43 * t47
      t51 = t28 * t50
      t53 = t38 * t39 * t51
      t55 = t1 ** 2
      t57 = t55 * t3 * t6
      t58 = x1 * x3
      t60 = t57 * t58 * t18
      t62 = t55 * t33
      t63 = t62 * t35
      t64 = t36 * x1
      t65 = t10 ** 2
      t66 = t65 * t10
      t67 = t64 * t66
      t69 = t63 * t67 * t18
      t71 = x1 * t10
      t73 = t57 * t71 * t18
      t75 = t55 * t4
      t76 = t75 * t7
      t77 = x3 ** 2
      t78 = t36 * t77
      t80 = t76 * t78 * t18
      t82 = t75 * t8
      t83 = t20 * x4
      t85 = t82 * t39 * t83
      t87 = t4 ** 2
      t88 = t55 * t87
      t89 = t7 ** 2
      t91 = t88 * t89 * t36
      t92 = t77 * t18
      t93 = t20 ** 2
      t94 = t24 ** 2
      t95 = t93 * t94
      t98 = 0.144D3 * t91 * t92 * t95
      t100 = t62 * t35 * x1
      t102 = t100 * t19 * t95
      t104 = t62 * t37
      t105 = t65 * t18
      t106 = t20 * t24
      t108 = t104 * t105 * t106
      t111 = t82 * t19 * t106
      t114 = t104 * t92 * t106
      t117 = t104 * t92 * t83
      t120 = t82 * t39 * t106
      t123 = t100 * t39 * t95
      t126 = t104 * t105 * t83
      t128 = x4 ** 2
      t129 = t93 * t128
      t131 = t100 * t19 * t129
      t133 = 0.122D3 * t31 + 0.14976D5 * t53 - 0.144D3 * t60 + 0.56D2 * 
     #t69 + 0.430D3 * t73 + 0.14832D5 * t80 + 0.144D3 * t85 - t98 + 0.10
     #D2 * t102 + 0.328D3 * t108 - 0.328D3 * t111 - 0.14976D5 * t114 - 0
     #.14832D5 * t117 + 0.288D3 * t120 - 0.144D3 * t123 + 0.400D3 * t126
     # + 0.192D3 * t131
      t135 = t2 * t3 * t6
      t136 = t18 * t27
      t138 = t135 * t71 * t136
      t140 = t5 * t7
      t141 = t36 * t65
      t143 = t140 * t141 * t136
      t146 = t135 * t58 * t136
      t149 = t38 * t19 * t51
      t152 = t140 * t78 * t136
      t155 = t55 * t87 * t3
      t156 = t89 * t6
      t158 = t155 * t156 * t64
      t159 = t93 * t24
      t160 = t159 * t50
      t162 = t158 * t92 * t160
      t165 = t91 * t39 * t160
      t168 = t76 * t141 * t18
      t171 = t36 ** 2
      t172 = t77 ** 2
      t176 = 0.144D3 * t88 * t89 * t171 * t172 * t18
      t178 = t82 * t19 * t83
      t181 = t93 * x4 * t50
      t183 = t91 * t39 * t181
      t185 = t28 * x4
      t187 = t9 * t19 * t185
      t190 = t9 * t39 * t29
      t192 = t159 * x4
      t194 = t100 * t19 * t192
      t197 = t100 * t39 * t192
      t204 = t50 ** 2
      t205 = t93 * t204
      t208 = 0.144D3 * t55 * t87 * t4 * t89 * t7 * t171 * t92 * t205
      t209 = t20 * t50
      t211 = t104 * t39 * t209
      t213 = t89 * t64
      t214 = t88 * t213
      t216 = t214 * t92 * t209
      t218 = -0.306D3 * t138 + 0.528D3 * t143 + 0.14976D5 * t146 + 0.17D
     #2 * t149 - 0.14976D5 * t152 + 0.288D3 * t162 + 0.14832D5 * t165 - 
     #0.584D3 * t168 - t176 - 0.530D3 * t178 + 0.14832D5 * t183 + 0.275D
     #3 * t187 - 0.14976D5 * t190 + 0.182D3 * t194 - 0.144D3 * t197 - t2
     #08 - 0.14976D5 * t211 + 0.576D3 * t216
      t231 = -0.134D3 * t31 - 0.14616D5 * t53 - 0.936D3 * t60 + 0.24D2 *
     # t69 - 0.542D3 * t73 - 0.15696D5 * t80 - 0.2160D4 * t85 + t98 - 0.
     #62D2 * t102 - 0.256D3 * t108 + 0.604D3 * t111
      t245 = t214 * t105 * t209
      t248 = t91 * t19 * t181
      t251 = t104 * t19 * t209
      t253 = t77 * x3
      t254 = t253 * t18
      t258 = 0.15552D5 * t114 + 0.6192D4 * t9 * t39 * t185 + 0.15552D5 *
     # t117 + 0.1872D4 * t120 - 0.936D3 * t123 - 0.288D3 * t126 - 0.30D2
     # * t131 + 0.3096D4 * t100 * t39 * t129 - 0.216D3 * t245 - 0.256D3 
     #* t248 + 0.256D3 * t251 + 0.432D3 * t214 * t254 * t106
      t262 = 0.16D2 * t38 * t105 * t185
      t276 = t91 * t19 * t160
      t278 = t262 + 0.190D3 * t138 - 0.752D3 * t143 - 0.14616D5 * t146 -
     # 0.490D3 * t149 + 0.14616D5 * t152 - 0.432D3 * t162 - 0.15696D5 * 
     #t165 + 0.544D3 * t168 + t176 + 0.432D3 * t63 * t64 * t253 * t18 - 
     #0.256D3 * t276
      t298 = 0.432D3 * t158 * t39 * t205 + 0.444D3 * t178 - 0.288D3 * t1
     #55 * t156 * t171 * t254 * t209 + 0.136D3 * t158 * t19 * t205 - 0.1
     #5552D5 * t183 - 0.156D3 * t187 + 0.14616D5 * t190 - 0.108D3 * t194
     # + 0.2160D4 * t197 + t208 + 0.15552D5 * t211 - 0.864D3 * t216
      t313 = -0.304D3 * t31 + 0.32D2 * t69 + 0.28208D5 * t73 - 0.304D3 *
     # t102 + 0.74320D5 * t108 - 0.27920D5 * t111 + 0.74288D5 * t126 - 0
     #.28792D5 * t131 - 0.80D2 * t245 - 0.25248D5 * t248 + 0.25264D5 * t
     #251 + t262
      t344 = -0.28224D5 * t138 + 0.74296D5 * t143 - 0.25220D5 * t149 - 0
     #.32D2 * t34 * t35 * t67 * t136 - 0.74208D5 * t168 - 0.25296D5 * t2
     #76 + 0.536D3 * t178 - 0.28372D5 * t187 - 0.29128D5 * t194 - 0.16D2
     # * t91 * t65 * t93 * t94 * t18 - 0.16D2 * t38 * t105 * t29 + 0.16D
     #2 * t2 * t87 * t213 * t105 * t51 - 0.16D2 * t214 * t66 * t20 * t24
     # * t18
      rrqg2qgh22J3 = (wd * (t133 + t218) + wd * (t231 + t258 + t278 + t2
     #98) + wd * (t313 + t344)) / t1 / t18 / t27 / z / 0.314159265358979
     #3D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J4
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
      t11 = t6 * t9 * x1
      t12 = 0.1D1 - x3
      t13 = s * t3
      t14 = t7 * x1
      t20 = (s - t13 * t14 * x3 - t13 * t14 * t12) ** 2
      t21 = t12 * t20
      t22 = 0.1D1 - x1
      t23 = t22 ** 2
      t24 = 0.1D1 - x4
      t25 = t23 * t24
      t26 = t25 * x4
      t28 = t11 * t21 * t26
      t30 = x3 * t20
      t32 = t11 * t30 * t26
      t34 = t4 ** 2
      t37 = t8 ** 2
      t39 = x1 ** 2
      t40 = t39 ** 2
      t43 = x3 ** 2
      t44 = t43 * t20
      t48 = cos(x2 * 0.3141592653589793D1)
      t52 = Sqrt(x3 * t12 * x4 * t24)
      t55 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t48 * t52
      t56 = t55 ** 2
      t57 = t23 * t56
      t60 = 0.144D3 * t2 * t34 * t4 * t37 * t8 * t40 * t44 * t57
      t61 = t9 * t39
      t62 = t6 * t61
      t63 = t22 * t24
      t65 = t62 * t44 * t63
      t67 = t2 * t4
      t68 = t8 * x1
      t69 = t67 * t68
      t71 = t69 * t30 * t63
      t73 = t22 * t55
      t75 = t62 * t30 * t73
      t77 = t12 ** 2
      t78 = t77 * t20
      t79 = t22 * x4
      t81 = t62 * t78 * t79
      t83 = x4 ** 2
      t84 = t23 * t83
      t86 = t11 * t21 * t84
      t89 = t69 * t21 * t79
      t91 = t2 * t34
      t93 = t91 * t37 * t39
      t94 = t25 * t55
      t96 = t93 * t30 * t94
      t98 = s * t1
      t99 = t98 * t4
      t100 = t99 * t68
      t101 = t7 * t22
      t106 = s - t13 * t101 * x4 - t13 * t101 * t24
      t107 = t106 * t22
      t108 = t107 * t24
      t110 = t100 * t21 * t108
      t112 = t98 * t5
      t113 = t112 * t61
      t114 = t107 * t55
      t116 = t113 * t30 * t114
      t118 = t39 * x1
      t119 = t37 * t118
      t120 = t91 * t119
      t122 = t120 * t44 * t73
      t125 = t23 * x4 * t55
      t127 = t93 * t30 * t125
      t130 = t2 * t34 * t3
      t131 = t37 * t7
      t133 = t130 * t131 * t118
      t135 = t133 * t44 * t94
      t137 = t24 ** 2
      t138 = t23 * t137
      t140 = t11 * t30 * t138
      t143 = t100 * t30 * t108
      t145 = 0.182D3 * t28 - 0.144D3 * t32 - t60 - 0.14976D5 * t65 + 0.2
     #88D3 * t71 - 0.14976D5 * t75 + 0.400D3 * t81 + 0.192D3 * t86 - 0.5
     #30D3 * t89 + 0.14832D5 * t96 + 0.122D3 * t110 + 0.14976D5 * t116 +
     # 0.576D3 * t122 + 0.14832D5 * t127 + 0.288D3 * t135 - 0.144D3 * t1
     #40 - 0.14976D5 * t143
      t146 = t107 * x4
      t148 = t100 * t21 * t146
      t151 = t62 * t44 * t79
      t154 = t69 * t30 * t79
      t158 = 0.144D3 * t93 * t44 * t138
      t160 = t11 * t21 * t138
      t163 = t62 * t78 * t63
      t166 = t69 * t21 * t63
      t169 = t2 * t3 * t7
      t170 = x1 * x3
      t172 = t169 * t170 * t20
      t174 = t6 * t9
      t175 = t77 * t12
      t176 = t118 * t175
      t178 = t174 * t176 * t20
      t180 = x1 * t12
      t182 = t169 * t180 * t20
      t184 = t67 * t8
      t185 = t39 * t43
      t187 = t184 * t185 * t20
      t189 = t39 * t77
      t191 = t184 * t189 * t20
      t194 = t43 ** 2
      t198 = 0.144D3 * t91 * t37 * t40 * t194 * t20
      t200 = t113 * t21 * t114
      t202 = t99 * t8
      t203 = t20 * t106
      t205 = t202 * t189 * t203
      t208 = t98 * t3 * t7
      t210 = t208 * t180 * t203
      t213 = t208 * t170 * t203
      t216 = t202 * t185 * t203
      t218 = 0.275D3 * t148 - 0.14832D5 * t151 + 0.144D3 * t154 - t158 +
     # 0.10D2 * t160 + 0.328D3 * t163 - 0.328D3 * t166 - 0.144D3 * t172 
     #+ 0.56D2 * t178 + 0.430D3 * t182 + 0.14832D5 * t187 - 0.584D3 * t1
     #91 - t198 + 0.17D2 * t200 + 0.528D3 * t205 - 0.306D3 * t210 + 0.14
     #976D5 * t213 - 0.14976D5 * t216
      t235 = -0.108D3 * t28 + 0.2160D4 * t32 + t60 + 0.15552D5 * t65 + 0
     #.1872D4 * t71 + 0.15552D5 * t75 + 0.432D3 * t133 * t30 * t57 + 0.3
     #096D4 * t11 * t30 * t84 - 0.288D3 * t81 - 0.30D2 * t86 + 0.444D3 *
     # t89
      t247 = t43 * x3
      t248 = t247 * t20
      t256 = t93 * t21 * t94
      t263 = -0.15696D5 * t96 - 0.134D3 * t110 - 0.14616D5 * t116 + 0.61
     #92D4 * t100 * t30 * t146 - 0.864D3 * t122 - 0.15552D5 * t127 - 0.4
     #32D3 * t135 - 0.288D3 * t130 * t131 * t40 * t248 * t73 + 0.136D3 *
     # t133 * t21 * t57 - 0.256D3 * t256 + 0.432D3 * t174 * t118 * t247 
     #* t20 - 0.936D3 * t140
      t276 = 0.14616D5 * t143 - 0.156D3 * t148 + 0.15552D5 * t151 - 0.21
     #60D4 * t154 + t158 - 0.62D2 * t160 - 0.256D3 * t163 + 0.604D3 * t1
     #66 - 0.936D3 * t172 + 0.24D2 * t178 - 0.542D3 * t182 - 0.15696D5 *
     # t187
      t281 = 0.16D2 * t113 * t78 * t146
      t283 = t120 * t78 * t73
      t286 = t93 * t21 * t125
      t289 = t62 * t21 * t73
      t298 = 0.544D3 * t191 + t198 - 0.490D3 * t200 + t281 - 0.216D3 * t
     #283 - 0.256D3 * t286 + 0.256D3 * t289 + 0.432D3 * t120 * t248 * t6
     #3 - 0.752D3 * t205 + 0.190D3 * t210 - 0.14616D5 * t213 + 0.14616D5
     # * t216
      t322 = -0.29128D5 * t28 + 0.74288D5 * t81 - 0.28792D5 * t86 + 0.53
     #6D3 * t89 - 0.304D3 * t110 - 0.16D2 * t93 * t77 * t23 * t137 * t20
     # - 0.16D2 * t120 * t175 * t22 * t24 * t20 - 0.25296D5 * t256 - 0.2
     #8372D5 * t148 - 0.304D3 * t160 + 0.74320D5 * t163 - 0.27920D5 * t1
     #66
      t344 = 0.32D2 * t178 + 0.28208D5 * t182 - 0.74208D5 * t191 - 0.252
     #20D5 * t200 + t281 - 0.16D2 * t113 * t78 * t108 + 0.16D2 * t98 * t
     #34 * t119 * t78 * t114 - 0.80D2 * t283 - 0.25248D5 * t286 + 0.2526
     #4D5 * t289 + 0.74296D5 * t205 - 0.28224D5 * t210 - 0.32D2 * t112 *
     # t9 * t176 * t203
      rrqg2qgh22J4 = (wd * (t145 + t218) + wd * (t235 + t263 + t276 + t2
     #98) + wd * (t322 + t344)) / t1 / t20 / t106 / z / 0.31415926535897
     #93D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J5
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
      t11 = t6 * t9 * x1
      t12 = 0.1D1 - x3
      t13 = s * t3
      t14 = t7 * x1
      t20 = (s - t13 * t14 * x3 - t13 * t14 * t12) ** 2
      t21 = t12 * t20
      t22 = 0.1D1 - x1
      t23 = t22 ** 2
      t24 = 0.1D1 - x4
      t25 = t23 * t24
      t26 = t25 * x4
      t28 = t11 * t21 * t26
      t30 = x3 * t20
      t32 = t11 * t30 * t26
      t34 = t4 ** 2
      t37 = t8 ** 2
      t39 = x1 ** 2
      t40 = t39 ** 2
      t43 = x3 ** 2
      t44 = t43 * t20
      t48 = cos(x2 * 0.3141592653589793D1)
      t52 = Sqrt(x3 * t12 * x4 * t24)
      t55 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t48 * t52
      t56 = t55 ** 2
      t57 = t23 * t56
      t60 = 0.144D3 * t2 * t34 * t4 * t37 * t8 * t40 * t44 * t57
      t61 = t9 * t39
      t62 = t6 * t61
      t63 = t22 * t55
      t65 = t62 * t30 * t63
      t67 = s * t1
      t68 = t67 * t5
      t69 = t68 * t61
      t70 = t7 * t22
      t75 = s - t13 * t70 * x4 - t13 * t70 * t24
      t76 = t75 * t22
      t77 = t76 * t55
      t79 = t69 * t30 * t77
      t82 = t2 * t3 * t7
      t83 = x1 * x3
      t85 = t82 * t83 * t20
      t87 = t6 * t9
      t88 = t39 * x1
      t89 = t12 ** 2
      t90 = t89 * t12
      t91 = t88 * t90
      t93 = t87 * t91 * t20
      t95 = x1 * t12
      t97 = t82 * t95 * t20
      t99 = t2 * t4
      t100 = t99 * t8
      t101 = t39 * t43
      t103 = t100 * t101 * t20
      t105 = t67 * t4
      t106 = t8 * x1
      t107 = t105 * t106
      t108 = t76 * x4
      t110 = t107 * t21 * t108
      t113 = t69 * t21 * t77
      t115 = t2 * t34
      t116 = t37 * t88
      t117 = t115 * t116
      t119 = t117 * t44 * t63
      t122 = t115 * t37 * t39
      t124 = t23 * x4 * t55
      t126 = t122 * t30 * t124
      t129 = t2 * t34 * t3
      t130 = t37 * t7
      t132 = t129 * t130 * t88
      t133 = t25 * t55
      t135 = t132 * t44 * t133
      t138 = t122 * t30 * t133
      t140 = t24 ** 2
      t141 = t23 * t140
      t144 = 0.144D3 * t122 * t44 * t141
      t146 = t11 * t21 * t141
      t148 = 0.182D3 * t28 - 0.144D3 * t32 - t60 - 0.14976D5 * t65 + 0.1
     #4976D5 * t79 - 0.144D3 * t85 + 0.56D2 * t93 + 0.430D3 * t97 + 0.14
     #832D5 * t103 + 0.275D3 * t110 + 0.17D2 * t113 + 0.576D3 * t119 + 0
     #.14832D5 * t126 + 0.288D3 * t135 + 0.14832D5 * t138 - t144 + 0.10D
     #2 * t146
      t149 = t89 * t20
      t150 = t22 * t24
      t152 = t62 * t149 * t150
      t154 = t99 * t106
      t156 = t154 * t21 * t150
      t159 = t62 * t44 * t150
      t162 = t154 * t30 * t150
      t165 = t11 * t30 * t141
      t167 = t39 * t89
      t169 = t100 * t167 * t20
      t172 = t43 ** 2
      t176 = 0.144D3 * t115 * t37 * t40 * t172 * t20
      t177 = t22 * x4
      t179 = t62 * t149 * t177
      t181 = x4 ** 2
      t182 = t23 * t181
      t184 = t11 * t21 * t182
      t187 = t154 * t21 * t177
      t190 = t62 * t44 * t177
      t193 = t154 * t30 * t177
      t195 = t76 * t24
      t197 = t107 * t30 * t195
      t200 = t107 * t21 * t195
      t202 = t105 * t8
      t203 = t20 * t75
      t205 = t202 * t167 * t203
      t208 = t67 * t3 * t7
      t210 = t208 * t95 * t203
      t213 = t208 * t83 * t203
      t216 = t202 * t101 * t203
      t218 = 0.328D3 * t152 - 0.328D3 * t156 - 0.14976D5 * t159 + 0.288D
     #3 * t162 - 0.144D3 * t165 - 0.584D3 * t169 - t176 + 0.400D3 * t179
     # + 0.192D3 * t184 - 0.530D3 * t187 - 0.14832D5 * t190 + 0.144D3 * 
     #t193 - 0.14976D5 * t197 + 0.122D3 * t200 + 0.528D3 * t205 - 0.306D
     #3 * t210 + 0.14976D5 * t213 - 0.14976D5 * t216
      t233 = t117 * t149 * t63
      t235 = -0.108D3 * t28 + 0.2160D4 * t32 + t60 + 0.15552D5 * t65 - 0
     #.14616D5 * t79 - 0.936D3 * t85 + 0.24D2 * t93 - 0.542D3 * t97 - 0.
     #15696D5 * t103 + 0.3096D4 * t11 * t30 * t182 - 0.216D3 * t233
      t237 = t122 * t21 * t124
      t240 = t62 * t21 * t63
      t242 = t43 * x3
      t243 = t242 * t20
      t255 = -0.256D3 * t237 + 0.256D3 * t240 + 0.432D3 * t117 * t243 * 
     #t150 - 0.156D3 * t110 - 0.490D3 * t113 - 0.864D3 * t119 - 0.15552D
     #5 * t126 - 0.432D3 * t135 - 0.15696D5 * t138 + t144 - 0.62D2 * t14
     #6 - 0.256D3 * t152
      t273 = 0.16D2 * t69 * t149 * t108
      t275 = 0.604D3 * t156 + 0.15552D5 * t159 + 0.1872D4 * t162 - 0.936
     #D3 * t165 + 0.544D3 * t169 + t176 + 0.432D3 * t87 * t88 * t242 * t
     #20 - 0.288D3 * t179 - 0.30D2 * t184 + 0.6192D4 * t107 * t30 * t108
     # + t273 + 0.444D3 * t187
      t287 = t122 * t21 * t133
      t298 = 0.15552D5 * t190 - 0.2160D4 * t193 - 0.288D3 * t129 * t130 
     #* t40 * t243 * t63 + 0.136D3 * t132 * t21 * t57 - 0.256D3 * t287 +
     # 0.432D3 * t132 * t30 * t57 + 0.14616D5 * t197 - 0.134D3 * t200 - 
     #0.752D3 * t205 + 0.190D3 * t210 - 0.14616D5 * t213 + 0.14616D5 * t
     #216
      t314 = -0.29128D5 * t28 + 0.32D2 * t93 + 0.28208D5 * t97 - 0.80D2 
     #* t233 - 0.25248D5 * t237 + 0.25264D5 * t240 - 0.28372D5 * t110 - 
     #0.25220D5 * t113 - 0.304D3 * t146 + 0.74320D5 * t152 - 0.27920D5 *
     # t156 - 0.74208D5 * t169
      t344 = 0.74288D5 * t179 - 0.28792D5 * t184 - 0.32D2 * t68 * t9 * t
     #91 * t203 + t273 + 0.536D3 * t187 - 0.25296D5 * t287 - 0.304D3 * t
     #200 + 0.74296D5 * t205 - 0.28224D5 * t210 - 0.16D2 * t122 * t89 * 
     #t23 * t140 * t20 - 0.16D2 * t69 * t149 * t195 + 0.16D2 * t67 * t34
     # * t116 * t149 * t77 - 0.16D2 * t117 * t90 * t22 * t24 * t20
      rrqg2qgh22J5 = (wd * (t148 + t218) + wd * (t235 + t255 + t275 + t2
     #98) + wd * (t314 + t344)) / t1 / t20 / t75 / z / 0.314159265358979
     #3D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J6
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
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = x1 ** 2
      t11 = x1 * t10
      t12 = t9 * t11
      t13 = t6 * t12
      t14 = 0.1D1 - x3
      t15 = t14 ** 2
      t16 = s * t3
      t17 = t7 * x1
      t23 = (s - t16 * t17 * x3 - t16 * t17 * t14) ** 2
      t24 = t15 * t23
      t25 = 0.1D1 - x1
      t29 = cos(x2 * 0.3141592653589793D1)
      t31 = 0.1D1 - x4
      t34 = Sqrt(x3 * t14 * x4 * t31)
      t37 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t29 * t34
      t38 = t25 * t37
      t40 = t13 * t24 * t38
      t43 = t6 * t9 * t10
      t44 = t14 * t23
      t45 = t25 ** 2
      t47 = t45 * x4 * t37
      t49 = t43 * t44 * t47
      t51 = t4 * t3
      t52 = t2 * t51
      t53 = t8 * t7
      t54 = t53 * t10
      t55 = t52 * t54
      t57 = t55 * t44 * t38
      t59 = x3 ** 2
      t60 = t59 * x3
      t61 = t60 * t23
      t62 = t25 * t31
      t66 = s * t1
      t67 = t66 * t51
      t68 = t67 * t54
      t69 = t7 * t25
      t74 = s - t16 * t69 * x4 - t16 * t69 * t31
      t75 = t74 * t25
      t76 = t75 * x4
      t79 = 0.16D2 * t68 * t24 * t76
      t80 = t66 * t4
      t81 = t8 * x1
      t82 = t80 * t81
      t83 = x3 * t23
      t84 = t75 * t31
      t92 = t82 * t44 * t84
      t94 = t75 * t37
      t99 = t2 * t5 * t3
      t100 = t9 * t7
      t101 = t10 ** 2
      t108 = t99 * t100 * t11
      t109 = t37 ** 2
      t110 = t45 * t109
      t114 = -0.216D3 * t40 - 0.256D3 * t49 + 0.256D3 * t57 + 0.432D3 * 
     #t13 * t61 * t62 + t79 + 0.14616D5 * t82 * t83 * t84 + 0.6192D4 * t
     #82 * t83 * t76 - 0.134D3 * t92 - 0.14616D5 * t68 * t83 * t94 - 0.2
     #88D3 * t99 * t100 * t101 * t61 * t38 + 0.136D3 * t108 * t44 * t110
      t115 = t45 * t31
      t116 = t115 * t37
      t118 = t43 * t44 * t116
      t124 = t52 * t53 * x1
      t125 = x4 ** 2
      t126 = t45 * t125
      t130 = t2 * t4
      t131 = t130 * t81
      t132 = t25 * x4
      t136 = t59 * t23
      t137 = t31 ** 2
      t138 = t45 * t137
      t143 = t124 * t44 * t138
      t146 = t55 * t24 * t62
      t149 = t131 * t44 * t62
      t160 = t115 * x4
      t162 = t124 * t44 * t160
      t164 = -0.256D3 * t118 + 0.432D3 * t108 * t83 * t110 + 0.3096D4 * 
     #t124 * t83 * t126 - 0.2160D4 * t131 * t83 * t132 + 0.144D3 * t43 *
     # t136 * t138 - 0.62D2 * t143 - 0.256D3 * t146 + 0.604D3 * t149 + 0
     #.15552D5 * t55 * t136 * t62 + 0.1872D4 * t131 * t83 * t62 - 0.936D
     #3 * t124 * t83 * t138 - 0.108D3 * t162
      t172 = t80 * t8
      t173 = t10 * t15
      t174 = t23 * t74
      t176 = t172 * t173 * t174
      t179 = t66 * t3 * t7
      t180 = x1 * t14
      t182 = t179 * t180 * t174
      t184 = x1 * x3
      t188 = t10 * t59
      t192 = t52 * t53
      t198 = t2 * t3 * t7
      t202 = t15 * t14
      t203 = t11 * t202
      t205 = t192 * t203 * t23
      t208 = t198 * t180 * t23
      t221 = 0.2160D4 * t124 * t83 * t160 - 0.15696D5 * t43 * t83 * t116
     # - 0.752D3 * t176 + 0.190D3 * t182 - 0.14616D5 * t179 * t184 * t17
     #4 + 0.14616D5 * t172 * t188 * t174 + 0.432D3 * t192 * t11 * t60 * 
     #t23 - 0.936D3 * t198 * t184 * t23 + 0.24D2 * t205 - 0.542D3 * t208
     # + 0.144D3 * t2 * t5 * t4 * t9 * t8 * t101 * t136 * t110 + 0.15552
     #D5 * t55 * t83 * t38
      t231 = t130 * t8
      t236 = t231 * t173 * t23
      t239 = t59 ** 2
      t245 = t55 * t24 * t132
      t248 = t124 * t44 * t126
      t251 = t131 * t44 * t132
      t257 = t82 * t44 * t76
      t260 = t68 * t44 * t94
      t262 = -0.864D3 * t13 * t136 * t38 - 0.15552D5 * t43 * t83 * t47 -
     # 0.432D3 * t108 * t136 * t116 - 0.15696D5 * t231 * t188 * t23 + 0.
     #544D3 * t236 + 0.144D3 * t6 * t9 * t101 * t239 * t23 - 0.288D3 * t
     #245 - 0.30D2 * t248 + 0.444D3 * t251 + 0.15552D5 * t55 * t136 * t1
     #32 - 0.156D3 * t257 - 0.490D3 * t260
      t277 = -0.80D2 * t40 - 0.25248D5 * t49 + 0.25264D5 * t57 + t79 - 0
     #.304D3 * t92 - 0.25296D5 * t118 - 0.304D3 * t143 + 0.74320D5 * t14
     #6 - 0.27920D5 * t149 - 0.29128D5 * t162 + 0.74296D5 * t176 - 0.282
     #24D5 * t182
      t308 = -0.32D2 * t67 * t53 * t203 * t174 + 0.32D2 * t205 + 0.28208
     #D5 * t208 - 0.16D2 * t43 * t15 * t45 * t137 * t23 - 0.16D2 * t68 *
     # t24 * t84 + 0.16D2 * t66 * t5 * t12 * t24 * t94 - 0.16D2 * t13 * 
     #t202 * t25 * t31 * t23 - 0.74208D5 * t236 + 0.74288D5 * t245 - 0.2
     #8792D5 * t248 + 0.536D3 * t251 - 0.28372D5 * t257 - 0.25220D5 * t2
     #60
      rrqg2qgh22J6 = (wd * (t114 + t164 + t221 + t262) + wd * (t277 + t3
     #08)) / t1 / t23 / t74 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh22J7
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
      t11 = x1 ** 2
      t12 = t11 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t15 = t14 * t13
      t16 = t12 * t15
      t17 = s * t3
      t18 = t7 * x1
      t24 = (s - t17 * t18 * x3 - t17 * t18 * t13) ** 2
      t30 = x1 * t13
      t34 = t2 * t4
      t36 = t11 * t14
      t40 = s * t1
      t41 = t40 * t4
      t43 = 0.1D1 - x1
      t44 = t7 * t43
      t47 = 0.1D1 - x4
      t50 = s - t17 * t44 * x4 - t17 * t44 * t47
      t51 = t24 * t50
      t60 = t40 * t5
      t65 = t8 * x1
      t66 = t41 * t65
      t67 = t13 * t24
      t68 = t50 * t43
      t69 = t68 * t47
      t73 = t4 ** 2
      t74 = t2 * t73
      t75 = t8 ** 2
      t77 = t74 * t75 * t11
      t78 = t43 ** 2
      t80 = t47 ** 2
      t85 = t9 * t11
      t86 = t60 * t85
      t87 = t14 * t24
      t92 = t75 * t12
      t97 = cos(x2 * 0.3141592653589793D1)
      t101 = Sqrt(x3 * t13 * x4 * t47)
      t104 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t97 * t101
      t105 = t68 * t104
      t109 = t74 * t92
      t115 = t78 * t47
      t120 = 0.32D2 * t6 * t9 * t16 * t24 + 0.28208D5 * t2 * t3 * t7 * t
     #30 * t24 - 0.74208D5 * t34 * t8 * t36 * t24 + 0.74296D5 * t41 * t8
     # * t36 * t51 - 0.28224D5 * t40 * t3 * t7 * t30 * t51 - 0.32D2 * t6
     #0 * t9 * t16 * t51 - 0.304D3 * t66 * t67 * t69 - 0.16D2 * t77 * t1
     #4 * t78 * t80 * t24 - 0.16D2 * t86 * t87 * t69 + 0.16D2 * t40 * t7
     #3 * t92 * t87 * t105 - 0.16D2 * t109 * t15 * t43 * t47 * t24 - 0.2
     #5296D5 * t77 * t67 * t115 * t104
      t121 = t43 * t104
      t130 = t6 * t85
      t134 = t68 * x4
      t144 = t43 * x4
      t149 = t6 * t9 * x1
      t150 = x4 ** 2
      t155 = t34 * t65
      t163 = t43 * t47
      t174 = -0.80D2 * t109 * t87 * t121 - 0.25248D5 * t77 * t67 * t78 *
     # x4 * t104 + 0.25264D5 * t130 * t67 * t121 + 0.16D2 * t86 * t87 * 
     #t134 - 0.28372D5 * t66 * t67 * t134 - 0.25220D5 * t86 * t67 * t105
     # + 0.74288D5 * t130 * t87 * t144 - 0.28792D5 * t149 * t67 * t78 * 
     #t150 + 0.536D3 * t155 * t67 * t144 - 0.304D3 * t149 * t67 * t78 * 
     #t80 + 0.74320D5 * t130 * t87 * t163 - 0.27920D5 * t155 * t67 * t16
     #3 - 0.29128D5 * t149 * t67 * t115 * x4
      rrqg2qgh22J7 = wd * (t120 + t174) / t1 / t24 / t50 / z / 0.3141592
     #653589793D1 / 0.36D2

      end function
  
 