  
      subroutine bbarbbarH6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH6n1e1  
      doubleprecision bbarbbarH6n1e0  
      doubleprecision bbarbbarH6n1em1  
      doubleprecision bbarbbarH6n1em2  
      doubleprecision bbarbbarH6n1em3  
      doubleprecision bbarbbarH6n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH6n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t15 = log(-0.4D1 * x3 * t8 * t11 * t4)
      t16 = t15 * z
      t19 = lh ** 2
      t21 = 0.3141592653589793D1 ** 2
      t23 = 0.180D3 * t19 - 0.30D2 * t21
      t24 = z * t23
      t25 = t15 ** 2
      t26 = t25 * z
      t29 = (0.180D3 * t16 * lh + t24 + 0.45D2 * t26) * t1
      t30 = -t4
      t47 = (-t16 * t23 - 0.90D2 * t26 * lh + z * (-0.2884936567583026D3
     # - 0.120D3 * t19 * lh + 0.60D2 * lh * t21) - 0.15D2 * t25 * t15 * 
     #z) * t1
      t48 = 0.4D1 * t4
      t52 = z * lh
      t53 = t1 * wd
      t54 = x2 * x3
      t55 = t8 * t11
      t56 = t55 * t4
      t59 = log(-0.4D1 * t54 * t56)
      t65 = z * t1
      t66 = t59 ** 2
      t78 = 0.1D1 / x2
      t82 = 0.90D2 * t1 * wd
      t83 = z * t30
      t84 = x1 ** 2
      t85 = t84 * x3
      t86 = t85 * x2
      t89 = log(-0.4D1 * t86 * t56)
      t90 = t89 * z
      t94 = lh * t1
      t95 = wd * z
      t96 = t95 * t48
      t101 = 0.1D1 / x1
      t106 = log(-0.4D1 * t85 * t56)
      t107 = t106 * z
      t114 = t106 ** 2
      t115 = t114 * z
      t120 = t23 * t1
      t125 = t29 * wd * t30 / 0.540D3 + t47 * wd * t48 / 0.540D3 - (-0.1
     #80D3 * t52 * t53 * (x3 - 0.1D1 + t59 * t48) + 0.90D2 * t65 * wd * 
     #(-t66 * t48 / 0.2D1 + t59 * t30) - t24 * t53 * t48) * t78 / 0.540D
     #3 + (t82 * (t83 - t90 * t48) - 0.180D3 * t94 * t96) * t78 * t101 /
     # 0.270D3 - (-0.180D3 * t94 * wd * (t107 * t48 - t83) + t82 * (t107
     # * t30 - t115 * t48 / 0.2D1) - t120 * t96) * t101 / 0.270D3
      t126 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t125)
      t128 = 0.2D1 * t54
      t129 = cos(t6)
      t130 = -0.1D1 + x2
      t131 = x3 * t130
      t134 = Sqrt(t131 * x2 * t4)
      t136 = 0.2D1 * t129 * t134
      t138 = t2 * (-x3 + t128 - x2 + t136)
      t140 = t2 * (0.1D1 - x2 - x3 + t128 + t136)
      t146 = log(0.4D1 * t54 * t8 * t11 * t130 * t4)
      t147 = x2 * z
      t149 = (0.1D1 - x2 + t147) ** 2
      t150 = 0.1D1 / t149
      t151 = t146 * t150
      t152 = 0.3D1 * t147
      t153 = 0.3D1 * x2
      t155 = 0.4D1 + t152 - t153 - 0.4D1 * x3
      t157 = 0.4D1 * t147
      t158 = 0.4D1 * x2
      t159 = -t157 + x3 + t158 - 0.1D1
      t166 = t150 * (-x2 + t147)
      t168 = t146 ** 2
      t169 = t168 * t150
      t176 = t24 * t1
      t177 = wd * t150
      t183 = z * t150
      t189 = log(0.4D1 * t86 * t55 * t130 * t4)
      t190 = t189 * z
      t195 = t94 * wd
      t203 = -(-0.180D3 * t52 * t53 * (t151 * t155 - t150 * t159) + 0.90
     #D2 * t65 * wd * (-t166 + t151 * t159 - t169 * t155 / 0.2D1) - t176
     # * t177 * t155) * t78 / 0.540D3 + (t82 * (t183 * t159 - t190 * t15
     #0 * t155) - 0.180D3 * t195 * t183 * t155) * t78 * t101 / 0.270D3
      t204 = FJET(XB1, XB2, s, 0.0D0, -t138, 0.0D0, t140, 0.0D0, t203)
      t206 = 0.2D1 * x3
      t207 = -0.3D1 + t206
      t233 = x3 * z
      t236 = t95 * t207
      t257 = t47 * wd * t207 / 0.540D3 - (-0.180D3 * t52 * t53 * (t59 * 
     #t207 - x3) + 0.90D2 * t65 * wd * (-t66 * t207 / 0.2D1 + t59 * x3) 
     #- t24 * t53 * t207) * t78 / 0.540D3 + t29 * wd * x3 / 0.540D3 + (t
     #82 * (-t90 * t207 + t233) - 0.180D3 * t94 * t236) * t78 * t101 / 0
     #.270D3 - (-0.180D3 * t94 * wd * (-t233 + t107 * t207) + t82 * (t10
     #7 * x3 - t115 * t207 / 0.2D1) - t120 * t236) * t101 / 0.270D3
      t258 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t257)
      t260 = x1 * x3
      t261 = t2 * t260
      t262 = -0.1D1 + x1
      t263 = t260 * z
      t264 = t54 * x1
      t265 = x1 * z
      t266 = t54 * t265
      t267 = 0.1D1 - x1 + t265
      t271 = Sqrt(t131 * t267 * x2 * t4)
      t273 = 0.2D1 * t129 * t271
      t276 = 0.1D1 / t267
      t278 = t2 * t262 * (-x3 + t260 - t263 + t128 - t264 + t266 - x2 + 
     #t273) * t276
      t279 = t4 * s
      t281 = t279 * t1 * x1
      t282 = x2 * x1
      t283 = t282 * z
      t284 = 0.1D1 - x1 + t265 - x2 + t282 - t283 - x3 + t260 - t263 + t
     #128 - t264 + t266 + t273
      t287 = t2 * t262 * t284 * t276
      t288 = t1 ** 2
      t293 = s * t288 * x2 * t262 * x1 * t276
      t295 = (-0.1D1 + x1 - t265 + x2 - t282 - t147 + t283) ** 2
      t296 = 0.1D1 / t295
      t297 = 0.2D1 * t84
      t300 = 0.2D1 * t85
      t301 = x2 ** 2
      t303 = 0.2D1 * t301 * t84
      t304 = t301 * x1
      t305 = t84 * x1
      t306 = t301 * t305
      t307 = t10 * z
      t308 = t307 * t84
      t309 = x3 * t305
      t310 = x2 * t10
      t311 = 0.4D1 * t310
      t312 = x1 * t10
      t314 = t84 * z
      t316 = t84 * t10
      t318 = x2 * t84
      t320 = -t297 - t260 - 0.3D1 * t265 - 0.2D1 * t282 + t300 - t303 + 
     #t304 + t306 + t308 - t309 + t311 + 0.2D1 * t312 + 0.5D1 * t314 - 0
     #.4D1 * t316 + 0.4D1 * t318
      t321 = z * t305
      t322 = 0.3D1 * t321
      t324 = 0.3D1 * t10 * t305
      t325 = x2 * t305
      t327 = t307 * t305
      t328 = x3 * t10
      t329 = t328 * t282
      t331 = 0.5D1 * t85 * t147
      t333 = 0.4D1 * t85 * t310
      t334 = 0.2D1 * t266
      t335 = x3 * t307
      t336 = t335 * t318
      t338 = 0.3D1 * t328 * t325
      t340 = 0.3D1 * t54 * t321
      t341 = t335 * t325
      t342 = 0.3D1 * t263
      t343 = -t322 + t324 - 0.2D1 * t325 - t327 - t233 + t329 + t331 - t
     #333 - t334 + t336 + t338 - t340 - t341 + t342 + t264
      t346 = t307 * x2
      t347 = t346 * t305
      t349 = t310 * t305
      t351 = t147 * t305
      t353 = t301 * t307
      t354 = t353 * x1
      t356 = 0.2D1 * t353 * t84
      t357 = t353 * t305
      t358 = t301 * z
      t360 = 0.3D1 * t358 * t305
      t361 = t301 * t10
      t363 = 0.3D1 * t361 * t305
      t365 = 0.3D1 * t358 * x1
      t367 = 0.6D1 * t358 * t84
      t369 = 0.3D1 * t361 * x1
      t371 = 0.6D1 * t361 * t84
      t373 = 0.3D1 * t328 * t305
      t374 = t335 * t305
      t375 = 0.12D2 * t283 + 0.2D1 * t347 - 0.6D1 * t349 + 0.6D1 * t351 
     #- t354 + t356 - t357 - t360 + t363 - t365 + t367 + t369 - t371 - t
     #373 + t374
      t377 = 0.3D1 * t233 * t305
      t378 = t335 * t84
      t379 = t54 * t305
      t380 = t346 * t84
      t382 = t346 * x1
      t383 = 0.4D1 * t382
      t384 = t328 * x1
      t385 = 0.2D1 * t384
      t386 = t85 * z
      t387 = 0.5D1 * t386
      t388 = 0.2D1 * t86
      t389 = t85 * t10
      t390 = 0.4D1 * t389
      t391 = t282 * t10
      t393 = t318 * z
      t395 = t318 * t10
      t397 = t377 - t378 + t379 - 0.6D1 * t380 + t383 - t385 - t387 - t3
     #88 + t390 + t305 + z + x1 - t157 - 0.14D2 * t391 - 0.14D2 * t393 +
     # 0.16D2 * t395
      t402 = t54 * t84 * t8
      t403 = t11 * t276
      t404 = t262 ** 2
      t410 = log(0.4D1 * t402 * t403 * t404 * t130 * t4)
      t411 = t410 * t296
      t414 = 0.3D1 * t310
      t418 = 0.2D1 * t318
      t419 = t297 + t260 + 0.9D1 * t265 + t282 - t300 - 0.4D1 * t308 + t
     #309 - t414 - 0.8D1 * t312 - 0.8D1 * t314 + 0.10D2 * t316 - t418
      t420 = 0.4D1 * t233
      t421 = t322 - t324 + t325 + t327 + t420 - t329 - t331 + t333 + t33
     #4 - t336 - t338 + t340
      t425 = 0.3D1 * t349
      t426 = 0.3D1 * t351
      t428 = t341 - 0.9D1 * t263 - t264 - 0.8D1 * t283 - t347 + t425 - t
     #426 + t373 - t374 - t377 + 0.4D1 * t378 - t379
      t430 = 0.3D1 * t382
      t434 = 0.4D1 * z
      t438 = 0.4D1 * t380 - t430 + 0.8D1 * t384 + 0.8D1 * t386 + t388 - 
     #0.10D2 * t389 - t305 - t434 - x1 + t152 + 0.10D2 * t391 + 0.8D1 * 
     #t393 - 0.10D2 * t395
      t440 = t419 + t421 + t428 + t438
      t444 = wd * t296
      t448 = t82 * (t296 * (t320 + t343 + t375 + t397) - t411 * t440) - 
     #0.180D3 * t94 * t444 * t440
      t452 = FJET(XB1, XB2, s, t261, t278, -t281, -t287, -t293, t448 * t
     #78 * t101 / 0.270D3)
      t454 = t78 * t101
      t458 = t2 * t262 * x3
      t460 = t279 * t1 * t262
      t462 = t403 * t404 * t4
      t465 = log(-0.4D1 * t402 * t462)
      t466 = t263 - t420 + x1 - t260 - t265 + t434
      t479 = log(-0.4D1 * t85 * t8 * t462)
      t487 = t479 ** 2
      t498 = (t82 * (-t263 + t265 + t233 - z - x1 + t260 - t465 * t466) 
     #- 0.180D3 * t94 * wd * t466) * t78 * t101 / 0.270D3 - (-0.180D3 * 
     #t94 * wd * (t263 + x1 - t260 - t233 + z + t479 * t466 - t265) + t8
     #2 * (t479 * (-t263 - x1 + t260 + t265 + t233 - z) - t487 * t466 / 
     #0.2D1) - t120 * wd * t466) * t101 / 0.270D3
      t499 = FJET(XB1, XB2, s, t261, -t458, -t281, t460, 0.0D0, t498)
      t501 = t260 - t282 - t300 - t303 + t304 + t306 + t309 + t311 + t41
     #8 - t325 + t233 - t329
      t503 = -t331 + t333 + t334 - t336 - t338 + t340 + t341 - t342 - t2
     #64 + 0.10D2 * t283 + t347 - t425
      t505 = t426 - t354 + t356 - t357 - t360 + t363 - t365 + t367 + t36
     #9 - t371 + t373 - t374
      t510 = -t377 + t378 - t379 - 0.5D1 * t380 + t383 + t385 + t387 + t
     #388 - t390 - t157 - 0.13D2 * t391 - 0.9D1 * t393 + 0.12D2 * t395
      t519 = 0.2D1 * t233
      t520 = -t260 + 0.6D1 * t265 + t300 - 0.3D1 * t308 - t309 - t414 - 
     #0.6D1 * t312 - 0.3D1 * t314 + 0.6D1 * t316 + t519 + t329 + t331 - 
     #t333 - t334 + t336 + t338 - t340 - t341
      t527 = 0.3D1 * z
      t531 = -t342 + t264 - 0.6D1 * t283 - t373 + t374 + t377 + 0.2D1 * 
     #t378 + t379 + 0.3D1 * t380 - t430 + 0.4D1 * t384 - 0.2D1 * t386 - 
     #t388 - 0.2D1 * t389 - t527 + t152 + 0.9D1 * t391 + 0.3D1 * t393 - 
     #0.6D1 * t395
      t532 = t520 + t531
      t539 = t82 * (t296 * (t501 + t503 + t505 + t510) - t411 * t532) - 
     #0.180D3 * t94 * t444 * t532
      t543 = FJET(XB1, XB2, s, t278, t261, -t287, -t281, -t293, t539 * t
     #78 * t101 / 0.270D3)
      t547 = 0.3D1 + t152 - t153 - t206
      t550 = -t157 - x3 + t158
      t579 = (t82 * (-t190 * t150 * t547 + t183 * t550) - 0.180D3 * t195
     # * t183 * t547) * t78 * t101 / 0.270D3 - (-0.180D3 * t52 * t53 * (
     #-t150 * t550 + t151 * t547) + 0.90D2 * t65 * wd * (-t166 + t151 * 
     #t550 - t169 * t547 / 0.2D1) - t176 * t177 * t547) * t78 / 0.540D3
      t580 = FJET(XB1, XB2, s, -t138, 0.0D0, t140, 0.0D0, 0.0D0, t579)
      t582 = -t263 + t260 - t519 + t527
      t609 = (t82 * (-t465 * t582 + t263 - t260 - t233) - 0.180D3 * t94 
     #* wd * t582) * t78 * t101 / 0.270D3 - (-0.180D3 * t94 * wd * (t479
     # * t582 + t260 + t233 - t263) + t82 * (t479 * (t263 - t260 - t233)
     # - t487 * t582 / 0.2D1) - t120 * wd * t582) * t101 / 0.270D3
      t610 = FJET(XB1, XB2, s, -t458, t261, t460, -t281, 0.0D0, t609)
      bbarbbarH6n1e1 = t126 * t125 + t204 * t203 + t258 * t257 + t452 * 
     #t448 * t454 / 0.270D3 + t499 * t498 + t543 * t539 * t454 / 0.270D3
     # + t580 * t579 + t610 * t609

      end function



      doubleprecision function bbarbbarH6n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = z * t1
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = 0.4D1 * t4
      t24 = z * lh
      t25 = t1 * wd
      t31 = 0.1D1 / x2
      t34 = 0.90D2 * t1
      t35 = t34 * wd
      t36 = t35 * z
      t38 = 0.1D1 / x1
      t42 = x1 ** 2
      t43 = t42 * x3
      t46 = log(-0.4D1 * t43 * t14)
      t47 = t46 * z
      t49 = -t4
      t53 = lh * t1
      t54 = wd * z
      t66 = log(-0.4D1 * x3 * t10 * t12 * t4)
      t67 = t66 * z
      t70 = (-0.180D3 * t24 - 0.90D2 * t67) * t1
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t82 = t66 ** 2
      t86 = (0.180D3 * t67 * lh + z * (0.180D3 * t76 - 0.30D2 * t78) + 0
     #.45D2 * t82 * z) * t1
      t90 = -(0.90D2 * t6 * wd * (x3 - 0.1D1 + t17 * t18) + 0.180D3 * t2
     #4 * t25 * t18) * t31 / 0.540D3 + t36 * t18 * t31 * t38 / 0.270D3 -
     # (t35 * (t47 * t18 - z * t49) + 0.180D3 * t53 * t54 * t18) * t38 /
     # 0.270D3 + t70 * wd * t49 / 0.540D3 + t86 * wd * t18 / 0.540D3
      t91 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t90)
      t93 = 0.2D1 * t7
      t94 = cos(t8)
      t95 = -0.1D1 + x2
      t96 = x3 * t95
      t99 = Sqrt(t96 * x2 * t4)
      t101 = 0.2D1 * t94 * t99
      t103 = t2 * (-x3 + t93 - x2 + t101)
      t105 = t2 * (0.1D1 - x2 - x3 + t93 + t101)
      t106 = x2 * z
      t108 = (0.1D1 - x2 + t106) ** 2
      t109 = 0.1D1 / t108
      t110 = 0.3D1 * t106
      t111 = 0.3D1 * x2
      t113 = 0.4D1 + t110 - t111 - 0.4D1 * x3
      t115 = t31 * t38
      t124 = log(0.4D1 * t7 * t10 * t12 * t95 * t4)
      t125 = t124 * t109
      t127 = 0.4D1 * t106
      t128 = 0.4D1 * x2
      t135 = t53 * wd
      t136 = z * t109
      t143 = t36 * t109 * t113 * t115 / 0.270D3 - (0.90D2 * t6 * wd * (t
     #125 * t113 - t109 * (-t127 + x3 + t128 - 0.1D1)) + 0.180D3 * t135 
     #* t136 * t113) * t31 / 0.540D3
      t144 = FJET(XB1, XB2, s, 0.0D0, -t103, 0.0D0, t105, 0.0D0, t143)
      t146 = 0.2D1 * x3
      t147 = -0.3D1 + t146
      t167 = x3 * z
      t180 = -(0.90D2 * t6 * wd * (t17 * t147 - x3) + 0.180D3 * t24 * t2
     #5 * t147) * t31 / 0.540D3 + t86 * wd * t147 / 0.540D3 + t36 * t147
     # * t31 * t38 / 0.270D3 - (t35 * (-t167 + t47 * t147) + 0.180D3 * t
     #53 * t54 * t147) * t38 / 0.270D3 + t70 * wd * x3 / 0.540D3
      t181 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t180)
      t183 = x1 * x3
      t184 = t2 * t183
      t185 = -0.1D1 + x1
      t186 = t183 * z
      t187 = t7 * x1
      t188 = x1 * z
      t189 = t7 * t188
      t190 = 0.1D1 - x1 + t188
      t194 = Sqrt(t96 * t190 * x2 * t4)
      t196 = 0.2D1 * t94 * t194
      t199 = 0.1D1 / t190
      t201 = t2 * t185 * (-x3 + t183 - t186 + t93 - t187 + t189 - x2 + t
     #196) * t199
      t202 = t4 * s
      t204 = t202 * t1 * x1
      t205 = x2 * x1
      t206 = t205 * z
      t207 = 0.1D1 - x1 + t188 - x2 + t205 - t206 - x3 + t183 - t186 + t
     #93 - t187 + t189 + t196
      t210 = t2 * t185 * t207 * t199
      t211 = t1 ** 2
      t216 = s * t211 * x2 * t185 * x1 * t199
      t218 = (-0.1D1 + x1 - t188 + x2 - t205 - t106 + t206) ** 2
      t219 = 0.1D1 / t218
      t220 = t35 * t219
      t221 = 0.4D1 * z
      t222 = 0.2D1 * t189
      t223 = t11 * z
      t224 = x3 * t223
      t225 = x2 * t42
      t226 = t224 * t225
      t227 = x3 * t11
      t228 = t42 * x1
      t229 = x2 * t228
      t231 = 0.3D1 * t227 * t229
      t232 = z * t228
      t234 = 0.3D1 * t7 * t232
      t235 = t224 * t229
      t237 = t43 * z
      t240 = 0.2D1 * t43 * x2
      t241 = t43 * t11
      t243 = t205 * t11
      t245 = t225 * z
      t247 = -t221 + t222 - t226 - t231 + t234 + t235 + 0.2D1 * t42 + 0.
     #8D1 * t237 + t240 - 0.10D2 * t241 + 0.10D2 * t243 + 0.8D1 * t245
      t248 = t225 * t11
      t250 = 0.4D1 * t167
      t252 = 0.2D1 * t43
      t253 = t223 * t42
      t255 = x3 * t228
      t256 = x2 * t11
      t257 = 0.3D1 * t256
      t258 = x1 * t11
      t260 = t42 * z
      t262 = t42 * t11
      t264 = -0.10D2 * t248 + t250 + t183 + 0.9D1 * t188 + t205 - t252 -
     # 0.4D1 * t253 + t255 - t257 - 0.8D1 * t258 - 0.8D1 * t260 + 0.10D2
     # * t262
      t273 = t223 * x2
      t275 = -0.2D1 * t225 + 0.3D1 * t232 - 0.3D1 * t11 * t228 + t229 + 
     #t223 * t228 - t228 + t110 - x1 - 0.9D1 * t186 - t187 - 0.8D1 * t20
     #6 - t273 * t228
      t281 = 0.3D1 * t227 * t228
      t282 = t224 * t228
      t284 = 0.3D1 * t167 * t228
      t285 = t224 * t42
      t287 = t7 * t228
      t288 = t273 * t42
      t291 = 0.3D1 * t273 * x1
      t292 = t227 * x1
      t294 = t227 * t205
      t296 = 0.5D1 * t43 * t106
      t298 = 0.4D1 * t43 * t256
      t299 = 0.3D1 * t256 * t228 - 0.3D1 * t106 * t228 + t281 - t282 - t
     #284 + 0.4D1 * t285 - t287 + 0.4D1 * t288 - t291 + 0.8D1 * t292 - t
     #294 - t296 + t298
      t301 = t247 + t264 + t275 + t299
      t306 = FJET(XB1, XB2, s, t184, t201, -t204, -t210, -t216, t220 * t
     #301 * t31 * t38 / 0.270D3)
      t314 = t2 * t185 * x3
      t316 = t202 * t1 * t185
      t317 = t186 - t250 + x1 - t183 - t188 + t221
      t323 = t185 ** 2
      t328 = log(-0.4D1 * t43 * t10 * t12 * t199 * t323 * t4)
      t339 = t35 * t317 * t31 * t38 / 0.270D3 - (t35 * (t186 + x1 - t183
     # - t167 + z + t328 * t317 - t188) + 0.180D3 * t53 * wd * t317) * t
     #38 / 0.270D3
      t340 = FJET(XB1, XB2, s, t184, -t314, -t204, t316, 0.0D0, t339)
      t342 = 0.3D1 * z
      t348 = 0.2D1 * t167
      t351 = -t342 - t222 + t226 + t231 - t234 - t235 - 0.2D1 * t237 - t
     #240 - 0.2D1 * t241 + 0.9D1 * t243 + 0.3D1 * t245 - 0.6D1 * t248 + 
     #t348 - t183 + 0.6D1 * t188 + t252 - 0.3D1 * t253 - t255
      t360 = -t257 - 0.6D1 * t258 - 0.3D1 * t260 + 0.6D1 * t262 + t110 -
     # 0.3D1 * t186 + t187 - 0.6D1 * t206 - t281 + t282 + t284 + 0.2D1 *
     # t285 + t287 + 0.3D1 * t288 - t291 + 0.4D1 * t292 + t294 + t296 - 
     #t298
      t361 = t351 + t360
      t366 = FJET(XB1, XB2, s, t201, t184, -t210, -t204, -t216, t220 * t
     #361 * t31 * t38 / 0.270D3)
      t375 = 0.3D1 + t110 - t111 - t146
      t391 = -(0.90D2 * t6 * wd * (-t109 * (-t127 - x3 + t128) + t125 * 
     #t375) + 0.180D3 * t135 * t136 * t375) * t31 / 0.540D3 + t36 * t109
     # * t375 * t115 / 0.270D3
      t392 = FJET(XB1, XB2, s, -t103, 0.0D0, t105, 0.0D0, 0.0D0, t391)
      t394 = -t186 + t183 - t348 + t342
      t408 = t35 * t394 * t31 * t38 / 0.270D3 - (t35 * (t328 * t394 + t1
     #83 + t167 - t186) + 0.180D3 * t53 * wd * t394) * t38 / 0.270D3
      t409 = FJET(XB1, XB2, s, -t314, t184, t316, -t204, 0.0D0, t408)
      bbarbbarH6n1e0 = t91 * t90 + t144 * t143 + t181 * t180 + t306 * t3
     #4 * wd * t219 * t301 * t115 / 0.270D3 + t340 * t339 + t366 * t34 *
     # wd * t219 * t361 * t115 / 0.270D3 + t392 * t391 + t409 * t408

      end function



      doubleprecision function bbarbbarH6n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = z * t1
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = z ** 2
      t22 = log(-0.4D1 * x3 * t15 / t17 * t4)
      t26 = (-0.180D3 * z * lh - 0.90D2 * t22 * z) * t1
      t27 = 0.4D1 * t4
      t33 = 0.1D1 / x2
      t37 = 0.90D2 * t1
      t38 = t37 * wd
      t40 = 0.1D1 / x1
      t44 = -t6 * wd * t4 / 0.6D1 + t26 * wd * t27 / 0.540D3 + t6 * wd *
     # t27 * t33 / 0.6D1 + t38 * z * t27 * t40 / 0.270D3
      t45 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t44)
      t48 = 0.2D1 * x2 * x3
      t49 = cos(t13)
      t54 = Sqrt(x3 * (-0.1D1 + x2) * x2 * t4)
      t56 = 0.2D1 * t49 * t54
      t58 = t2 * (-x3 + t48 - x2 + t56)
      t60 = t2 * (0.1D1 - x2 - x3 + t48 + t56)
      t61 = t6 * wd
      t62 = x2 * z
      t64 = (0.1D1 - x2 + t62) ** 2
      t65 = 0.1D1 / t64
      t66 = 0.3D1 * t62
      t67 = 0.3D1 * x2
      t69 = 0.4D1 + t66 - t67 - 0.4D1 * x3
      t74 = FJET(XB1, XB2, s, 0.0D0, -t58, 0.0D0, t60, 0.0D0, t61 * t65 
     #* t69 * t33 / 0.6D1)
      t77 = wd * t65
      t82 = 0.2D1 * x3
      t83 = -0.3D1 + t82
      t99 = t26 * wd * t83 / 0.540D3 + t6 * wd * t83 * t33 / 0.6D1 + t6 
     #* wd * x3 / 0.6D1 + t38 * z * t83 * t40 / 0.270D3
      t100 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t99)
      t102 = x1 * x3
      t103 = t2 * t102
      t104 = -0.1D1 + x1
      t106 = t2 * t104 * x3
      t107 = t4 * s
      t109 = t107 * t1 * x1
      t111 = t107 * t1 * t104
      t112 = x3 * z
      t115 = t102 * z
      t117 = -x1 + 0.4D1 * t112 + t102 - 0.4D1 * z - t115 + x1 * z
      t121 = FJET(XB1, XB2, s, t103, -t106, -t109, t111, 0.0D0, -t38 * t
     #117 * t40 / 0.270D3)
      t127 = 0.3D1 + t66 - t67 - t82
      t132 = FJET(XB1, XB2, s, -t58, 0.0D0, t60, 0.0D0, 0.0D0, t61 * t65
     # * t127 * t33 / 0.6D1)
      t141 = 0.2D1 * t112 - t102 - 0.3D1 * z + t115
      t145 = FJET(XB1, XB2, s, -t106, t103, t111, -t109, 0.0D0, -t38 * t
     #141 * t40 / 0.270D3)
      bbarbbarH6n1em1 = t45 * t44 + t74 * z * t1 * t77 * t69 * t33 / 0.6
     #D1 + t100 * t99 - t121 * t37 * wd * t117 * t40 / 0.270D3 + t132 * 
     #z * t1 * t77 * t127 * t33 / 0.6D1 - t145 * t37 * wd * t141 * t40 /
     # 0.270D3

      end function



      doubleprecision function bbarbbarH6n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = z * t1
      t7 = 0.4D1 * t4
      t11 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t6 * wd * t7
     # / 0.6D1)
      t13 = t1 * wd
      t17 = -0.3D1 + 0.2D1 * x3
      t21 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t6 * wd * t1
     #7 / 0.6D1)
      bbarbbarH6n1em2 = t11 * z * t13 * t7 / 0.6D1 + t21 * z * t13 * t17
     # / 0.6D1

      end function



      doubleprecision function bbarbbarH6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH6n1em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH6n1em4 = 0.0D0

      end function
