  
      subroutine rrqqbar2qqbarht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2qqbarh21J1  
      doubleprecision rrqqbar2qqbarh21J2  
      doubleprecision rrqqbar2qqbarh21J3  
      doubleprecision rrqqbar2qqbarh22J1  
      doubleprecision rrqqbar2qqbarh22J2  
      doubleprecision rrqqbar2qqbarh22J3  
      doubleprecision rrqqbar2qqbarht2s1e1  
      doubleprecision rrqqbar2qqbarht2s1e0  
      doubleprecision rrqqbar2qqbarht2s1em1  
      doubleprecision rrqqbar2qqbarht2s1em2  
      doubleprecision rrqqbar2qqbarht2s1em3  
      doubleprecision rrqqbar2qqbarht2s1em4  
      doubleprecision rrqqbar2qqbarht2s2e1  
      doubleprecision rrqqbar2qqbarht2s2e0  
      doubleprecision rrqqbar2qqbarht2s2em1  
      doubleprecision rrqqbar2qqbarht2s2em2  
      doubleprecision rrqqbar2qqbarht2s2em3  
      doubleprecision rrqqbar2qqbarht2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2qqbarht2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
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
      t26 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t2
     #5, x4)
      t29 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t2
     #5, x4)
      t31 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t2
     #5, x4)
      t32 = t17 * t4
      t35 = log(-0.4D1 * t14 * t32)
      t36 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t38 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t39 = t35 ** 2
      t40 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t47 = 0.3141592653589793D1 * lh
      t48 = t1 * t7
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t60 = 0.3141592653589793D1 * t59
      t61 = t26 - t40
      t65 = 0.1D1 / x3
      t69 = t10 * t13
      t72 = log(-0.4D1 * t69 * t32)
      t73 = t72 * 0.3141592653589793D1
      t76 = (-0.180D3 * t47 - 0.90D2 * t73) * t1
      t82 = t72 ** 2
      t83 = t82 * 0.3141592653589793D1
      t86 = (t60 + 0.180D3 * t73 * lh + 0.45D2 * t83) * t1
      t103 = (0.3141592653589793D1 * (0.60D2 * lh * t57 - 0.288493656758
     #3026D3 - 0.120D3 * t55 * lh) - t73 * t59 - 0.90D2 * t83 * lh - 0.1
     #5D2 * t82 * t72 * 0.3141592653589793D1) * t1
      t107 = x1 ** 2
      t108 = x3 * t107
      t109 = t108 * t69
      t112 = log(0.4D1 * t109 * t20)
      t115 = t13 * t16
      t116 = x4 * t4
      t120 = log(-0.4D1 * t108 * t10 * t115 * t116)
      t132 = 0.1D1 / x1
      t135 = t107 * t10
      t139 = log(-0.4D1 * t135 * t13 * t32)
      t140 = t139 ** 2
      t158 = (0.90D2 * t6 * t7 * (t24 * t26 / 0.2D1 - t23 * t29 + t31 + 
     #t35 * t36 - t38 - t39 * t40 / 0.2D1) - 0.180D3 * t47 * t48 * (t29 
     #- t23 * t26 - t36 + t35 * t40) + t60 * t48 * t61) * t65 / 0.1440D4
     # - t76 * t7 * t38 / 0.1440D4 - t86 * t7 * t36 / 0.1440D4 - t103 * 
     #t7 * t40 / 0.1440D4 - (0.90D2 * t6 * t7 * (t112 * t26 - t29 - t120
     # * t40 + t36) + 0.180D3 * t47 * t48 * t61) * t65 * t132 / 0.720D3 
     #+ (0.90D2 * t6 * t7 * (-t140 * t40 / 0.2D1 + t139 * t36 - t38) - 0
     #.180D3 * t47 * t48 * (t139 * t40 - t36) - t60 * t48 * t40) * t132 
     #/ 0.720D3
      t159 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t158)
      t161 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t165 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t169 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t173 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #25, x4)
      t176 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #25, x4)
      t177 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #25, x4)
      t192 = t173 - t169
      t229 = -t76 * t7 * t161 / 0.1440D4 - t86 * t7 * t165 / 0.1440D4 - 
     #t103 * t7 * t169 / 0.1440D4 + (0.90D2 * t6 * t7 * (t24 * t173 / 0.
     #2D1 + t176 - t23 * t177 - t161 + t35 * t165 - t39 * t169 / 0.2D1) 
     #- 0.180D3 * t47 * t48 * (t177 - t23 * t173 - t165 + t35 * t169) + 
     #t60 * t48 * t192) * t65 / 0.1440D4 - (0.90D2 * t6 * t7 * (t112 * t
     #173 + t165 - t120 * t169 - t177) + 0.180D3 * t47 * t48 * t192) * t
     #65 * t132 / 0.720D3 + (0.90D2 * t6 * t7 * (t139 * t165 - t140 * t1
     #69 / 0.2D1 - t161) - 0.180D3 * t47 * t48 * (-t165 + t139 * t169) -
     # t60 * t48 * t169) * t132 / 0.720D3
      t230 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t229)
      t232 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t233 = s * t232
      t234 = t1 * x1
      t235 = t233 * t234
      t236 = -0.1D1 + x1
      t237 = t1 * t236
      t238 = t237 * x4
      t239 = t233 * t238
      t240 = t237 * t4
      t241 = t233 * t240
      t242 = t232 ** 2
      t245 = x1 * t236
      t247 = s * t242 * t15 * t245 * t4
      t248 = t236 ** 2
      t250 = t242 ** 2
      t255 = log(-0.4D1 * t109 * t17 * t4 * t248 * t250)
      t256 = t255 * t236
      t258 = 0.1D1 / (-0.2D1 + t232)
      t259 = t242 * t258
      t260 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t261 = t259 * t260
      t263 = t236 * t242
      t264 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t266 = t263 * t258 * t264
      t271 = t47 * t48
      t273 = t263 * t258 * t260
      t284 = log(-0.4D1 * t135 * t115 * t116 * t248 * t250)
      t285 = t284 ** 2
      t286 = t285 * t236
      t289 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t292 = t284 * t236
      t304 = t60 * t48
      t309 = -(0.90D2 * t6 * t7 * (t256 * t261 - t266) + 0.180D3 * t271 
     #* t273) * t65 * t132 / 0.720D3 + (0.90D2 * t6 * t7 * (t286 * t261 
     #/ 0.2D1 + t263 * t258 * t289 - t292 * t259 * t264) - 0.180D3 * t47
     # * t48 * (t266 - t292 * t261) + t304 * t273) * t132 / 0.720D3
      t310 = FJET(XB1, XB2, s, t235, 0.0D0, -t239, t241, t247, t309)
      t312 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t314 = t263 * t258 * t312
      t315 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t316 = t259 * t315
      t323 = t263 * t258 * t315
      t331 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t349 = -(0.90D2 * t6 * t7 * (-t314 + t256 * t316) + 0.180D3 * t271
     # * t323) * t65 * t132 / 0.720D3 + (0.90D2 * t6 * t7 * (t286 * t316
     # / 0.2D1 + t263 * t258 * t331 - t292 * t259 * t312) - 0.180D3 * t4
     #7 * t48 * (t314 - t292 * t316) + t304 * t323) * t132 / 0.720D3
      t350 = FJET(XB1, XB2, s, -t239, t241, t235, 0.0D0, t247, t349)
      t352 = KAPPA2(x1, x2, t25, x4, z)
      t353 = s * t352
      t355 = t353 * t234 * t18
      t357 = t353 * t234 * x3
      t358 = t353 * t238
      t359 = t353 * t240
      t360 = t352 ** 2
      t365 = cos(t8)
      t368 = Sqrt(x3 * t18 * t116)
      t373 = s * t360 * t15 * t245 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t365 * t368)
      t377 = t360 ** 2
      t382 = log(0.4D1 * t108 * t69 * t16 * t116 * t248 * t18 * t377)
      t383 = t382 * t236
      t385 = 0.1D1 / (-0.2D1 + t352)
      t386 = t360 * t385
      t387 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25,
     # x4)
      t390 = t236 * t360
      t391 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25,
     # x4)
      t402 = 0.90D2 * t6 * t7 * (-t383 * t386 * t387 + t390 * t385 * t39
     #1) - 0.180D3 * t271 * t390 * t385 * t387
      t406 = FJET(XB1, XB2, s, -t355, t357, -t358, t359, t373, -t402 * t
     #65 * t132 / 0.720D3)
      t408 = t65 * t132
      t411 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25,
     # x4)
      t414 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t25,
     # x4)
      t425 = 0.90D2 * t6 * t7 * (-t383 * t386 * t411 + t390 * t385 * t41
     #4) - 0.180D3 * t271 * t390 * t385 * t411
      t429 = FJET(XB1, XB2, s, -t358, t359, -t355, t357, t373, -t425 * t
     #65 * t132 / 0.720D3)
      rrqqbar2qqbarht2s1e1 = t159 * t158 + t230 * t229 + t310 * t309 + t
     #350 * t349 - t406 * t402 * t408 / 0.720D3 - t429 * t425 * t408 / 0
     #.720D3

      end function



      doubleprecision function rrqqbar2qqbarht2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = 0.1D1 - x3
      t9 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8,
     # x4)
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
      t26 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8
     #, x4)
      t28 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t29 = t19 * t4
      t32 = log(-0.4D1 * t16 * t29)
      t33 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t39 = 0.3141592653589793D1 * lh
      t40 = t1 * t7
      t41 = t26 - t33
      t46 = 0.1D1 / x3
      t49 = t6 * t7
      t52 = 0.1D1 / x1
      t56 = x1 ** 2
      t57 = t56 * t12
      t61 = log(-0.4D1 * t57 * t15 * t29)
      t73 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
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
      t108 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #8, x4)
      t109 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #8, x4)
      t111 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t112 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t118 = t109 - t112
      t141 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
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
      t175 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t181 = t158 * t164
      t182 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
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
      t213 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t219 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
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
      t263 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, 
     #x4)
      t266 = t262 * t263 * t46 * t52
      t269 = FJET(XB1, XB2, s, -t241, t243, -t244, t245, t259, -t171 * t
     #266 / 0.8D1)
      t271 = t40 * t158
      t275 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, 
     #x4)
      t278 = t262 * t275 * t46 * t52
      t281 = FJET(XB1, XB2, s, -t244, t245, -t241, t243, t259, -t171 * t
     #278 / 0.8D1)
      rrqqbar2qqbarht2s1e0 = t106 * t105 + t152 * t151 + t211 * t210 + t
     #236 * t235 - t269 * 0.3141592653589793D1 * t271 * t266 / 0.8D1 - t
     #281 * 0.3141592653589793D1 * t271 * t278 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1
     #0D1, x4)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t36 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.314
     #1592653589793D1) * t1
      t39 = 0.1D1 - x3
      t40 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t3
     #9, x4)
      t43 = 0.1D1 / x3
      t47 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.16D2 - t36 * t9 /
     # 0.1440D4 + t6 * t7 * (t40 - t8) * t43 / 0.16D2
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t47)
      t50 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t51 = t7 * t50
      t55 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t61 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t3
     #9, x4)
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
      t91 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D
     #1, x4)
      t96 = FJET(XB1, XB2, s, t73, 0.0D0, -t77, t79, t85, t87 * t90 * t9
     #1 * t10 / 0.8D1)
      t98 = t1 * t7
      t100 = t74 * t80
      t106 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t111 = FJET(XB1, XB2, s, -t77, t79, t73, 0.0D0, t85, t87 * t90 * t
     #106 * t10 / 0.8D1)
      rrqqbar2qqbarht2s1em1 = t47 * t48 + t68 * t67 + t96 * 0.3141592653
     #589793D1 * t98 * t100 * t89 * t91 * t10 / 0.8D1 + t111 * 0.3141592
     #653589793D1 * t98 * t100 * t89 * t106 * t10 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1
     #0D1, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrqqbar2qqbarht2s1em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0
     #.16D2 - t21 * 0.3141592653589793D1 * t14 * t17 / 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      rrqqbar2qqbarht2s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      rrqqbar2qqbarht2s1em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2qqbarht2s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
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
      t25 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3
     #, x4)
      t28 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3
     #, x4)
      t30 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3
     #, x4)
      t31 = t17 * t4
      t34 = log(-0.4D1 * t14 * t31)
      t35 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t37 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t38 = t34 ** 2
      t39 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t46 = 0.3141592653589793D1 * lh
      t47 = t1 * t7
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t58 = 0.180D3 * t54 - 0.30D2 * t56
      t59 = 0.3141592653589793D1 * t58
      t61 = t47 * (t25 - t39)
      t64 = 0.1D1 / x3
      t68 = t10 * t13
      t71 = log(-0.4D1 * t68 * t31)
      t72 = t71 * 0.3141592653589793D1
      t75 = (-0.180D3 * t46 - 0.90D2 * t72) * t1
      t81 = t71 ** 2
      t82 = t81 * 0.3141592653589793D1
      t85 = (t59 + 0.180D3 * t72 * lh + 0.45D2 * t82) * t1
      t102 = (0.3141592653589793D1 * (0.60D2 * lh * t56 - 0.288493656758
     #3026D3 - 0.120D3 * t54 * lh) - t72 * t58 - 0.90D2 * t82 * lh - 0.1
     #5D2 * t81 * t71 * 0.3141592653589793D1) * t1
      t106 = x1 ** 2
      t107 = x3 * t106
      t109 = t13 * t16
      t110 = x4 * t4
      t114 = log(-0.4D1 * t107 * t10 * t109 * t110)
      t116 = t107 * t68
      t119 = log(0.4D1 * t116 * t20)
      t129 = 0.1D1 / x1
      t132 = t106 * t10
      t136 = log(-0.4D1 * t132 * t13 * t31)
      t138 = t136 ** 2
      t155 = (0.90D2 * t6 * t7 * (t24 * t25 / 0.2D1 - t23 * t28 + t30 + 
     #t34 * t35 - t37 - t38 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (t28 
     #- t23 * t25 - t35 + t34 * t39) + t59 * t61) * t64 / 0.1440D4 - t75
     # * t7 * t37 / 0.1440D4 - t85 * t7 * t35 / 0.1440D4 - t102 * t7 * t
     #39 / 0.1440D4 + (0.90D2 * t6 * t7 * (-t35 + t114 * t39 + t28 - t11
     #9 * t25) - 0.180D3 * t46 * t61) * t64 * t129 / 0.720D3 - (0.90D2 *
     # t6 * t7 * (-t136 * t35 + t37 + t138 * t39 / 0.2D1) - 0.180D3 * t4
     #6 * t47 * (t35 - t136 * t39) + t59 * t47 * t39) * t129 / 0.720D3
      t156 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t155)
      t158 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t159 = s * t158
      t160 = t1 * x1
      t161 = t159 * t160
      t162 = -0.1D1 + x1
      t163 = t1 * t162
      t164 = t163 * x4
      t165 = t159 * t164
      t166 = t163 * t4
      t167 = t159 * t166
      t168 = t158 ** 2
      t171 = x1 * t162
      t173 = s * t168 * t15 * t171 * x4
      t174 = t162 * t168
      t176 = 0.1D1 / (-0.2D1 + t158)
      t177 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t179 = t174 * t176 * t177
      t180 = t162 ** 2
      t182 = t168 ** 2
      t187 = log(-0.4D1 * t116 * t17 * t4 * t180 * t182)
      t188 = t187 * t162
      t189 = t168 * t176
      t190 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t191 = t189 * t190
      t197 = t46 * t47
      t199 = t174 * t176 * t190
      t205 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t213 = log(-0.4D1 * t132 * t109 * t110 * t182 * t180)
      t214 = t213 ** 2
      t215 = t214 * t162
      t218 = t213 * t162
      t230 = t59 * t47
      t235 = (0.90D2 * t6 * t7 * (t179 - t188 * t191) - 0.180D3 * t197 *
     # t199) * t64 * t129 / 0.720D3 - (0.90D2 * t6 * t7 * (-t174 * t176 
     #* t205 - t215 * t191 / 0.2D1 + t218 * t189 * t177) - 0.180D3 * t46
     # * t47 * (t218 * t191 - t179) - t230 * t199) * t129 / 0.720D3
      t236 = FJET(XB1, XB2, s, 0.0D0, t161, -t165, t167, -t173, t235)
      t238 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.0D0, x4)
      t242 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.0D0, x4)
      t246 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.0D0, x4)
      t250 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x
     #3, x4)
      t253 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x
     #3, x4)
      t254 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x
     #3, x4)
      t270 = t47 * (t250 - t246)
      t304 = -t75 * t7 * t238 / 0.1440D4 - t85 * t7 * t242 / 0.1440D4 - 
     #t102 * t7 * t246 / 0.1440D4 + (0.90D2 * t6 * t7 * (t24 * t250 / 0.
     #2D1 + t253 - t23 * t254 - t238 + t34 * t242 - t38 * t246 / 0.2D1) 
     #- 0.180D3 * t46 * t47 * (t254 - t23 * t250 - t242 + t34 * t246) + 
     #t59 * t270) * t64 / 0.1440D4 + (0.90D2 * t6 * t7 * (t254 - t242 + 
     #t114 * t246 - t119 * t250) - 0.180D3 * t46 * t270) * t64 * t129 / 
     #0.720D3 - (0.90D2 * t6 * t7 * (t238 + t138 * t246 / 0.2D1 - t136 *
     # t242) - 0.180D3 * t46 * t47 * (-t136 * t246 + t242) + t59 * t47 *
     # t246) * t129 / 0.720D3
      t305 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t304)
      t307 = KAPPA2(x1, x2, x3, x4, z)
      t308 = s * t307
      t310 = t308 * t160 * x3
      t312 = t308 * t160 * t18
      t313 = t308 * t164
      t314 = t308 * t166
      t315 = t307 ** 2
      t320 = cos(t8)
      t323 = Sqrt(x3 * t18 * t110)
      t328 = s * t315 * t15 * t171 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t320 * t323)
      t332 = t315 ** 2
      t337 = log(0.4D1 * t107 * t68 * t16 * t110 * t180 * t18 * t332)
      t338 = t337 * t162
      t340 = 0.1D1 / (-0.2D1 + t307)
      t341 = t315 * t340
      t342 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t345 = t162 * t315
      t346 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t357 = 0.90D2 * t6 * t7 * (t338 * t341 * t342 - t345 * t340 * t346
     #) + 0.180D3 * t197 * t345 * t340 * t342
      t361 = FJET(XB1, XB2, s, t310, -t312, -t313, t314, t328, t357 * t6
     #4 * t129 / 0.720D3)
      t363 = t64 * t129
      t366 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t367 = t189 * t366
      t369 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t371 = t174 * t176 * t369
      t377 = t174 * t176 * t366
      t383 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t403 = (0.90D2 * t6 * t7 * (-t188 * t367 + t371) - 0.180D3 * t197 
     #* t377) * t64 * t129 / 0.720D3 - (0.90D2 * t6 * t7 * (-t174 * t176
     # * t383 + t218 * t189 * t369 - t215 * t367 / 0.2D1) - 0.180D3 * t4
     #6 * t47 * (-t371 + t218 * t367) - t230 * t377) * t129 / 0.720D3
      t404 = FJET(XB1, XB2, s, -t165, t167, 0.0D0, t161, -t173, t403)
      t406 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t409 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t420 = 0.90D2 * t6 * t7 * (-t345 * t340 * t406 + t338 * t341 * t40
     #9) + 0.180D3 * t197 * t345 * t340 * t409
      t424 = FJET(XB1, XB2, s, -t313, t314, t310, -t312, t328, t420 * t6
     #4 * t129 / 0.720D3)
      rrqqbar2qqbarht2s2e1 = t155 * t156 + t236 * t235 + t305 * t304 + t
     #361 * t357 * t363 / 0.720D3 + t404 * t403 + t424 * t420 * t363 / 0
     #.720D3

      end function



      doubleprecision function rrqqbar2qqbarht2s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3,
     # x4)
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
      t25 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3
     #, x4)
      t27 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t28 = t18 * t4
      t31 = log(-0.4D1 * t15 * t28)
      t32 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t1 * t7
      t40 = t25 - t32
      t45 = 0.1D1 / x3
      t48 = t6 * t7
      t50 = 0.1D1 / x1
      t54 = x1 ** 2
      t55 = t54 * t11
      t59 = log(-0.4D1 * t55 * t14 * t28)
      t71 = rrqqbar2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
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
     #/ 0.8D1 - (0.90D2 * t6 * t7 * (t27 - t59 * t32) - 0.180D3 * t38 * 
     #t39 * t32) * t50 / 0.720D3 - t6 * t7 * t71 / 0.16D2 - t83 * t7 * t
     #27 / 0.1440D4 - t99 * t7 * t32 / 0.1440D4
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
      t127 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t135 = x4 * t4
      t136 = t116 ** 2
      t137 = t110 ** 2
      t142 = log(-0.4D1 * t55 * t14 * t17 * t135 * t136 * t137)
      t143 = t142 * t110
      t146 = t110 * t116
      t147 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t154 = t38 * t39
      t162 = t123 * t126 * t127 * t45 * t50 / 0.8D1 - (0.90D2 * t6 * t7 
     #* (t143 * t126 * t127 - t146 * t125 * t147) + 0.180D3 * t154 * t14
     #6 * t125 * t127) * t50 / 0.720D3
      t163 = FJET(XB1, XB2, s, 0.0D0, t109, -t113, t115, -t121, t162)
      t165 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x
     #3, x4)
      t166 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x
     #3, x4)
      t168 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.0D0, x4)
      t169 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.0D0, x4)
      t175 = t166 - t169
      t197 = rrqqbar2qqbarh22J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.0D0, x4)
      t207 = (0.90D2 * t6 * t7 * (t165 - t24 * t166 - t168 + t31 * t169)
     # - 0.180D3 * t38 * t39 * t175) * t45 / 0.1440D4 + t48 * t175 * t45
     # * t50 / 0.8D1 - (0.90D2 * t6 * t7 * (-t59 * t169 + t168) - 0.180D
     #3 * t38 * t39 * t169) * t50 / 0.720D3 - t6 * t7 * t197 / 0.16D2 - 
     #t83 * t7 * t168 / 0.1440D4 - t99 * t7 * t169 / 0.1440D4
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
      t235 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t238 = t234 * t235 * t45 * t50
      t241 = FJET(XB1, XB2, s, t213, -t215, -t216, t217, t231, -t123 * t
     #238 / 0.8D1)
      t243 = t39 * t110
      t247 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t253 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t269 = t123 * t126 * t247 * t45 * t50 / 0.8D1 - (0.90D2 * t6 * t7 
     #* (-t146 * t125 * t253 + t143 * t126 * t247) + 0.180D3 * t154 * t1
     #46 * t125 * t247) * t50 / 0.720D3
      t270 = FJET(XB1, XB2, s, -t113, t115, 0.0D0, t109, -t121, t269)
      t272 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t275 = t234 * t272 * t45 * t50
      t278 = FJET(XB1, XB2, s, -t216, t217, t213, -t215, t231, -t123 * t
     #275 / 0.8D1)
      rrqqbar2qqbarht2s2e0 = t104 * t103 + t163 * t162 + t208 * t207 - t
     #241 * 0.3141592653589793D1 * t243 * t238 / 0.8D1 + t270 * t269 - t
     #278 * 0.3141592653589793D1 * t243 * t275 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht2s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0
     #D0, x4)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqqbar2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t36 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.314
     #1592653589793D1) * t1
      t39 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3
     #, x4)
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
      t70 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0
     #, x4)
      t75 = FJET(XB1, XB2, s, 0.0D0, t52, -t56, t58, -t64, t66 * t69 * t
     #70 * t10 / 0.8D1)
      t77 = t1 * t7
      t79 = t53 * t59
      t85 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t86 = t7 * t85
      t90 = rrqqbar2qqbarh22J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t96 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3
     #, x4)
      t102 = -t6 * t86 * t10 / 0.8D1 - t6 * t7 * t90 / 0.16D2 - t36 * t8
     #6 / 0.1440D4 + t6 * t7 * (t96 - t85) * t42 / 0.16D2
      t103 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D
     #0, x4)
      t110 = FJET(XB1, XB2, s, -t56, t58, 0.0D0, t52, -t64, t66 * t69 * 
     #t105 * t10 / 0.8D1)
      rrqqbar2qqbarht2s2em1 = t46 * t47 + t75 * 0.3141592653589793D1 * t
     #77 * t79 * t68 * t70 * t10 / 0.8D1 + t103 * t102 + t110 * 0.314159
     #2653589793D1 * t77 * t79 * t68 * t105 * t10 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht2s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0
     #D0, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.16D2)
      t14 = t1 * t7
      t17 = rrqqbar2qqbarh22J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #0D0, x4)
      t21 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.16D2)
      rrqqbar2qqbarht2s2em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0
     #.16D2 - t21 * 0.3141592653589793D1 * t14 * t17 / 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht2s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      rrqqbar2qqbarht2s2em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht2s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh21J1
      doubleprecision rrqqbar2qqbarh21J2
      doubleprecision rrqqbar2qqbarh21J3
      doubleprecision rrqqbar2qqbarh22J1
      doubleprecision rrqqbar2qqbarh22J2
      doubleprecision rrqqbar2qqbarh22J3
      rrqqbar2qqbarht2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2qqbarh21J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarh21J1 = 0.0D0

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh21J2
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
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t10 = t2 * t4 * t3 * t8 * t7
      t11 = x1 ** 2
      t13 = 0.1D1 - x1
      t17 = cos(x2 * 0.3141592653589793D1)
      t20 = 0.1D1 - x4
      t23 = Sqrt(x3 * (0.1D1 - x3) * x4 * t20)
      t34 = x3 ** 2
      rrqqbar2qqbarh21J2 = -0.32D2 / 0.27D2 * wd * (t10 * t11 * x3 * t13
     # * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t17 * t23) + t2 * t3 * t7 
     #* x1 * x3 - t10 * t11 * t34 * t13 * t20) / t1 / z / 0.314159265358
     #9793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh21J3
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
      t9 = 0.1D1 - x1
      t10 = 0.1D1 - x4
      t11 = t9 * t10
      t19 = x1 ** 2
      t21 = x3 ** 2
      t27 = t2 * t4 * t3 * t7 * t6
      t32 = cos(x2 * 0.3141592653589793D1)
      t37 = Sqrt(x3 * (0.1D1 - x3) * x4 * t10)
      rrqqbar2qqbarh21J3 = -0.32D2 / 0.27D2 * wd * (t5 * t7 * t11 * x1 *
     # x3 - t2 * t3 * t6 * x1 * x3 + t5 * t7 * t19 * t21 - t27 * t19 * x
     #3 * t9 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t32 * t37) + t27 * t
     #19 * t21 * t11) / t1 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh22J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarh22J1 = 0.0D0

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh22J2
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
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t10 = t2 * t4 * t3 * t8 * t7
      t11 = x1 ** 2
      t12 = 0.1D1 - x1
      t17 = cos(x2 * 0.3141592653589793D1)
      t18 = 0.1D1 - x3
      t23 = Sqrt(x3 * t18 * x4 * (0.1D1 - x4))
      t34 = t18 ** 2
      rrqqbar2qqbarh22J2 = -0.32D2 / 0.27D2 * wd * (t10 * t11 * t12 * (x
     #3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t17 * t23) * t18 + t2 * t3 * t7
     # * x1 * t18 - t10 * t11 * t34 * t12 * x4) / t1 / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh22J3
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
      t9 = 0.1D1 - x1
      t10 = t9 * x4
      t11 = 0.1D1 - x3
      t19 = x1 ** 2
      t21 = t11 ** 2
      t27 = t2 * t4 * t3 * t7 * t6
      t32 = cos(x2 * 0.3141592653589793D1)
      t37 = Sqrt(x3 * t11 * x4 * (0.1D1 - x4))
      rrqqbar2qqbarh22J3 = -0.32D2 / 0.27D2 * wd * (t5 * t7 * t10 * x1 *
     # t11 - t2 * t3 * t6 * x1 * t11 + t5 * t7 * t19 * t21 - t27 * t19 *
     # t9 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t32 * t37) * t11 + t27 
     #* t19 * t21 * t10) / t1 / z / 0.3141592653589793D1

      end function
  
 