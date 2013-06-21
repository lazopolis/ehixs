  
      subroutine rrgq2qght9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh91J1  
      doubleprecision rrgq2qgh91J2  
      doubleprecision rrgq2qgh91J3  
      doubleprecision rrgq2qgh91J4  
      doubleprecision rrgq2qgh91J5  
      doubleprecision rrgq2qgh91J6  
      doubleprecision rrgq2qgh91J7  
      doubleprecision rrgq2qgh92J1  
      doubleprecision rrgq2qgh92J2  
      doubleprecision rrgq2qgh92J3  
      doubleprecision rrgq2qgh92J4  
      doubleprecision rrgq2qgh92J5  
      doubleprecision rrgq2qgh92J6  
      doubleprecision rrgq2qght9s1e1  
      doubleprecision rrgq2qght9s1e0  
      doubleprecision rrgq2qght9s1em1  
      doubleprecision rrgq2qght9s1em2  
      doubleprecision rrgq2qght9s1em3  
      doubleprecision rrgq2qght9s1em4  
      doubleprecision rrgq2qght9s2e1  
      doubleprecision rrgq2qght9s2e0  
      doubleprecision rrgq2qght9s2em1  
      doubleprecision rrgq2qght9s2em2  
      doubleprecision rrgq2qght9s2em3  
      doubleprecision rrgq2qght9s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght9s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh91J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 ** 2
      t23 = t22 * 0.3141592653589793D1
      t26 = 0.3141592653589793D1 ** 2
      t29 = lh ** 2
      t32 = -0.60D2 * lh * t26 + 0.2884936567583026D3 + 0.120D3 * t29 * 
     #lh
      t33 = 0.3141592653589793D1 * t32
      t35 = t22 * t21 * 0.3141592653589793D1
      t37 = t21 * 0.3141592653589793D1
      t40 = -0.180D3 * t29 + 0.30D2 * t26
      t43 = (0.90D2 * t23 * lh + t33 + 0.15D2 * t35 - t37 * t40) * t3
      t44 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t51 = 0.3141592653589793D1 * t40
      t53 = (-0.180D3 * t37 * lh - 0.45D2 * t23 + t51) * t3
      t54 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t58 = 0.3141592653589793D1 * lh
      t62 = (0.180D3 * t58 + 0.90D2 * t37) * t3
      t63 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t73 = t26 ** 2
      t74 = t29 ** 2
      t80 = t22 ** 2
      t84 = (-0.30D2 * t35 * lh + t23 * t40 / 0.2D1 - t37 * t32 + 0.3141
     #592653589793D1 * (-0.5769873135166051D3 * lh - t73 - 0.60D2 * t74 
     #+ 0.60D2 * t29 * t26) - 0.15D2 / 0.4D1 * t80 * 0.3141592653589793D
     #1) * t3
      t85 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t89 = x1 ** 2
      t90 = x3 * t89
      t93 = log(0.4D1 * t90 * t19)
      t94 = t93 ** 2
      t108 = t6 * t51 * t85
      t110 = 0.1D1 / x3
      t112 = 0.1D1 / x1
      t115 = t89 * t15
      t116 = t12 * t18
      t119 = log(0.4D1 * t115 * t116)
      t124 = t119 ** 2
      t127 = t124 * t119
      t136 = t6 * t33 * t85
      t147 = x2 * x3
      t148 = t147 * t89
      t151 = log(0.4D1 * t148 * t19)
      t162 = 0.1D1 / x2
      t163 = t162 * t112
      t166 = x2 * t89
      t169 = log(0.4D1 * t166 * t19)
      t171 = t169 ** 2
      t190 = log(0.4D1 * x3 * t15 * t116)
      t195 = t190 ** 2
      t198 = t195 * t190
      t218 = log(0.4D1 * t147 * t19)
      t219 = t218 ** 2
      t239 = log(0.4D1 * x2 * t18 * t16)
      t244 = t239 ** 2
      t247 = t244 * t239
      t265 = t6 * 0.3141592653589793D1 * t7 / 0.16D2 - t43 * t5 * t44 / 
     #0.1440D4 - t53 * t5 * t54 / 0.1440D4 - t62 * t5 * t63 / 0.1440D4 -
     # t84 * t5 * t85 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 
     #* (-t94 * t85 / 0.2D1 + t93 * t44 - t54) + 0.180D3 * t6 * t58 * (t
     #93 * t85 - t44) - t108) * t110 * t112 / 0.720D3 - (t6 * t51 * (-t1
     #19 * t85 + t44) - 0.90D2 * t6 * 0.3141592653589793D1 * (t124 * t44
     # / 0.2D1 + t63 - t127 * t85 / 0.6D1 - t119 * t54) + t136 + 0.180D3
     # * t6 * t58 * (-t119 * t44 + t54 + t124 * t85 / 0.2D1)) * t112 / 0
     #.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t151 * t85 + t44
     #) + 0.180D3 * t6 * t58 * t85) * t110 * t163 / 0.720D3 - (-0.90D2 *
     # t6 * 0.3141592653589793D1 * (-t169 * t44 + t54 + t171 * t85 / 0.2
     #D1) + 0.180D3 * t6 * t58 * (-t169 * t85 + t44) + t108) * t162 * t1
     #12 / 0.720D3 - (t6 * t51 * (-t190 * t85 + t44) - 0.90D2 * t6 * 0.3
     #141592653589793D1 * (t195 * t44 / 0.2D1 + t63 - t198 * t85 / 0.6D1
     # - t190 * t54) + t136 + 0.180D3 * t6 * t58 * (-t190 * t44 + t54 + 
     #t195 * t85 / 0.2D1)) * t110 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t219 * t85 / 0.2D1 + t218 * t44 - t54) + 0.180D3 *
     # t6 * t58 * (t218 * t85 - t44) - t108) * t110 * t162 / 0.1440D4 + 
     #(t6 * t51 * (-t44 + t239 * t85) - 0.90D2 * t6 * 0.3141592653589793
     #D1 * (-t244 * t44 / 0.2D1 - t63 + t247 * t85 / 0.6D1 + t239 * t54)
     # - t136 + 0.180D3 * t6 * t58 * (t239 * t44 - t54 - t244 * t85 / 0.
     #2D1)) * t162 / 0.1440D4
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t265)
      t268 = t2 * x1
      t269 = -0.1D1 + x1
      t270 = t2 * t269
      t272 = x1 * z
      t273 = 0.1D1 - x1 + t272
      t274 = 0.1D1 / t273
      t275 = t269 ** 2
      t276 = t274 * t275
      t280 = log(0.4D1 * t90 * t15 * t116 * t276)
      t281 = t280 ** 2
      t282 = -t269
      t283 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D0
     #, x4)
      t286 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D0
     #, x4)
      t287 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D0
     #, x4)
      t299 = t6 * t51 * t283
      t303 = t115 * t12
      t304 = t18 * t274
      t308 = log(0.4D1 * t303 * t304 * t275)
      t313 = t308 ** 2
      t316 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D0
     #, x4)
      t317 = t313 * t308
      t338 = t16 * t276
      t341 = log(0.4D1 * t147 * t89 * t18 * t338)
      t356 = log(0.4D1 * t166 * t18 * t338)
      t357 = t356 ** 2
      t374 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t281 * t283 / 0.2D1
     # + t286 - t280 * t287) + 0.180D3 * t6 * t58 * (-t280 * t283 + t287
     #) + t299) * t110 * t112 / 0.720D3 - (t6 * t51 * (-t287 + t308 * t2
     #83) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t313 * t287 / 0.2D1 -
     # t316 + t317 * t283 / 0.6D1 + t308 * t286) - t6 * t33 * t283 + 0.1
     #80D3 * t6 * t58 * (t308 * t287 - t313 * t283 / 0.2D1 - t286)) * t1
     #12 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t341 * t283
     # - t287) - 0.180D3 * t6 * t58 * t283) * t110 * t163 / 0.720D3 - (-
     #0.90D2 * t6 * 0.3141592653589793D1 * (-t357 * t283 / 0.2D1 - t286 
     #+ t356 * t287) + 0.180D3 * t6 * t58 * (-t287 + t356 * t283) - t299
     #) * t162 * t112 / 0.720D3
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t268, -t270, 0.0D0, t374)
      t377 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t380 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t381 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t393 = t6 * t51 * t377
      t405 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t413 = t6 * t33 * t377
      t424 = rrgq2qgh92J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t530 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t219 * t377 / 0.2D
     #1 - t380 + t218 * t381) + 0.180D3 * t6 * t58 * (-t381 + t218 * t37
     #7) - t393) * t110 * t162 / 0.1440D4 + (t6 * t51 * (-t381 + t239 * 
     #t377) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t244 * t381 / 0.2D1
     # + t239 * t380 - t405 + t247 * t377 / 0.6D1) - t413 + 0.180D3 * t6
     # * t58 * (-t244 * t377 / 0.2D1 - t380 + t239 * t381)) * t162 / 0.1
     #440D4 + t6 * 0.3141592653589793D1 * t424 / 0.16D2 - (t6 * t51 * (t
     #381 - t190 * t377) - 0.90D2 * t6 * 0.3141592653589793D1 * (t195 * 
     #t381 / 0.2D1 + t405 - t198 * t377 / 0.6D1 - t190 * t380) + t413 + 
     #0.180D3 * t6 * t58 * (t195 * t377 / 0.2D1 + t380 - t190 * t381)) *
     # t110 / 0.1440D4 - t53 * t5 * t380 / 0.1440D4 - t62 * t5 * t405 / 
     #0.1440D4 - t84 * t5 * t377 / 0.1440D4 - t43 * t5 * t381 / 0.1440D4
     # + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t380 + t93 * t381 - t9
     #4 * t377 / 0.2D1) + 0.180D3 * t6 * t58 * (t93 * t377 - t381) - t39
     #3) * t110 * t112 / 0.720D3 - (t6 * t51 * (t381 - t119 * t377) - 0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t119 * t380 + t405 + t124 * t
     #381 / 0.2D1 - t127 * t377 / 0.6D1) + t413 + 0.180D3 * t6 * t58 * (
     #-t119 * t381 + t124 * t377 / 0.2D1 + t380)) * t112 / 0.720D3 - (-0
     #.90D2 * t6 * 0.3141592653589793D1 * (t381 - t151 * t377) + 0.180D3
     # * t6 * t58 * t377) * t110 * t163 / 0.720D3 - (-0.90D2 * t6 * 0.31
     #41592653589793D1 * (t380 - t169 * t381 + t171 * t377 / 0.2D1) + 0.
     #180D3 * t6 * t58 * (t381 - t169 * t377) + t393) * t162 * t112 / 0.
     #720D3
      t531 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t530)
      t533 = t2 * x3
      t534 = -0.1D1 + x3
      t535 = t2 * t534
      t536 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t538 = t16 * t534
      t541 = log(-0.4D1 * x3 * t18 * t538)
      t542 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t547 = t541 ** 2
      t550 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t551 = t547 * t541
      t554 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t575 = log(-0.4D1 * t90 * t18 * t538)
      t576 = t575 ** 2
      t590 = t6 * t51 * t542
      t595 = t166 * t15
      t596 = x3 * t534
      t600 = log(-0.4D1 * t595 * t116 * t596)
      t616 = log(-0.4D1 * t147 * t18 * t538)
      t617 = t616 ** 2
      t634 = -(t6 * t51 * (-t536 + t541 * t542) - 0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t547 * t536 / 0.2D1 - t550 + t551 * t542 / 0.6D1 
     #+ t541 * t554) - t6 * t33 * t542 + 0.180D3 * t6 * t58 * (-t547 * t
     #542 / 0.2D1 - t554 + t541 * t536)) * t110 / 0.1440D4 + (-0.90D2 * 
     #t6 * 0.3141592653589793D1 * (t576 * t542 / 0.2D1 + t554 - t575 * t
     #536) + 0.180D3 * t6 * t58 * (-t575 * t542 + t536) + t590) * t110 *
     # t112 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t536 + 
     #t600 * t542) - 0.180D3 * t6 * t58 * t542) * t110 * t163 / 0.720D3 
     #+ (-0.90D2 * t6 * 0.3141592653589793D1 * (t617 * t542 / 0.2D1 - t6
     #16 * t536 + t554) + 0.180D3 * t6 * t58 * (-t616 * t542 + t536) + t
     #590) * t110 * t162 / 0.1440D4
      t635 = FJET(XB1, XB2, s, 0.0D0, t533, 0.0D0, -t535, 0.0D0, t634)
      t637 = 0.2D1 * t147
      t638 = cos(t13)
      t639 = -0.1D1 + x2
      t640 = x3 * t639
      t643 = Sqrt(t640 * x2 * t534)
      t645 = 0.2D1 * t638 * t643
      t647 = t2 * (0.1D1 - x2 - x3 + t637 + t645)
      t649 = t2 * (-x3 + t637 - x2 + t645)
      t650 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t651 = t147 * t115
      t653 = t116 * t639 * t534
      t656 = log(0.4D1 * t651 * t653)
      t657 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t670 = t147 * t15
      t673 = log(0.4D1 * t670 * t653)
      t674 = t673 ** 2
      t678 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t694 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t650 - t656 * t657
     #) + 0.180D3 * t6 * t58 * t657) * t110 * t163 / 0.720D3 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t674 * t657 / 0.2D1 + t673 * t650 
     #- t678) + 0.180D3 * t6 * t58 * (-t650 + t673 * t657) - t6 * t51 * 
     #t657) * t110 * t162 / 0.1440D4
      t695 = FJET(XB1, XB2, s, 0.0D0, t647, 0.0D0, -t649, 0.0D0, t694)
      t698 = x2 * s * t1
      t699 = t639 * s
      t700 = t699 * t1
      t701 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t706 = log(-0.4D1 * t148 * t16 * t18 * t639)
      t707 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t720 = t116 * t639
      t723 = log(-0.4D1 * t595 * t720)
      t724 = t723 ** 2
      t728 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t739 = t6 * t51 * t707
      t746 = log(-0.4D1 * t670 * t720)
      t747 = t746 ** 2
      t767 = log(-0.4D1 * x2 * t15 * t720)
      t772 = t767 ** 2
      t776 = t772 * t767
      t779 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t796 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t701 + t706 * t70
     #7) - 0.180D3 * t6 * t58 * t707) * t110 * t163 / 0.720D3 - (-0.90D2
     # * t6 * 0.3141592653589793D1 * (-t724 * t707 / 0.2D1 + t723 * t701
     # - t728) + 0.180D3 * t6 * t58 * (t723 * t707 - t701) - t739) * t16
     #2 * t112 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t728 
     #+ t747 * t707 / 0.2D1 - t746 * t701) + 0.180D3 * t6 * t58 * (t701 
     #- t746 * t707) + t739) * t110 * t162 / 0.1440D4 + (t6 * t51 * (t70
     #1 - t767 * t707) - 0.90D2 * t6 * 0.3141592653589793D1 * (t772 * t7
     #01 / 0.2D1 - t767 * t728 - t776 * t707 / 0.6D1 + t779) + t6 * t33 
     #* t707 + 0.180D3 * t6 * t58 * (-t767 * t701 + t728 + t772 * t707 /
     # 0.2D1)) * t162 / 0.1440D4
      t797 = FJET(XB1, XB2, s, 0.0D0, t698, 0.0D0, -t700, 0.0D0, t796)
      t799 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t800 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t807 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t810 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t841 = t6 * t51 * t800
      t874 = -(t6 * t51 * (-t799 + t541 * t800) - 0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t547 * t799 / 0.2D1 - t807 + t551 * t800 / 0.6D1 
     #+ t541 * t810) - t6 * t33 * t800 + 0.180D3 * t6 * t58 * (t541 * t7
     #99 - t810 - t547 * t800 / 0.2D1)) * t110 / 0.1440D4 + (-0.90D2 * t
     #6 * 0.3141592653589793D1 * (-t575 * t799 + t576 * t800 / 0.2D1 + t
     #810) + 0.180D3 * t6 * t58 * (t799 - t575 * t800) + t841) * t110 * 
     #t112 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t799 + t
     #600 * t800) - 0.180D3 * t6 * t58 * t800) * t110 * t163 / 0.720D3 +
     # (-0.90D2 * t6 * 0.3141592653589793D1 * (t617 * t800 / 0.2D1 - t61
     #6 * t799 + t810) + 0.180D3 * t6 * t58 * (t799 - t616 * t800) + t84
     #1) * t110 * t162 / 0.1440D4
      t875 = FJET(XB1, XB2, s, 0.0D0, -t535, 0.0D0, t533, 0.0D0, t874)
      t877 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t879 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t892 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t910 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t656 * t877 + t87
     #9) + 0.180D3 * t6 * t58 * t877) * t110 * t163 / 0.720D3 + (-0.90D2
     # * t6 * 0.3141592653589793D1 * (t673 * t879 - t892 - t674 * t877 /
     # 0.2D1) + 0.180D3 * t6 * t58 * (-t879 + t673 * t877) - t6 * t51 * 
     #t877) * t110 * t162 / 0.1440D4
      t911 = FJET(XB1, XB2, s, 0.0D0, -t649, 0.0D0, t647, 0.0D0, t910)
      t913 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t915 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t927 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t941 = t6 * t51 * t913
      t969 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t988 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t706 * t913 - t915
     #) - 0.180D3 * t6 * t58 * t913) * t110 * t163 / 0.720D3 - (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t927 + t723 * t915 - t724 * t913 /
     # 0.2D1) + 0.180D3 * t6 * t58 * (-t915 + t723 * t913) - t941) * t16
     #2 * t112 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t747 
     #* t913 / 0.2D1 + t927 - t746 * t915) + 0.180D3 * t6 * t58 * (t915 
     #- t746 * t913) + t941) * t110 * t162 / 0.1440D4 + (t6 * t51 * (t91
     #5 - t767 * t913) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t767 * t
     #927 - t776 * t913 / 0.6D1 + t969 + t772 * t915 / 0.2D1) + t6 * t33
     # * t913 + 0.180D3 * t6 * t58 * (t772 * t913 / 0.2D1 + t927 - t767 
     #* t915)) * t162 / 0.1440D4
      t989 = FJET(XB1, XB2, s, 0.0D0, -t700, 0.0D0, t698, 0.0D0, t988)
      t993 = t2 * t269 * x2 * t274
      t994 = t1 * t269
      t995 = t699 * t994
      t1000 = s * t17 * x2 * x1 * t269 * t274
      t1001 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t282, x2, 0.0D0, 
     #x4)
      t1006 = log(-0.4D1 * t651 * t116 * t276 * t639)
      t1007 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t282, x2, 0.0D0, 
     #x4)
      t1020 = t275 * t639
      t1024 = log(-0.4D1 * t166 * t16 * t304 * t1020)
      t1026 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t282, x2, 0.0D0, 
     #x4)
      t1027 = t1024 ** 2
      t1045 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t1001 - t1006 * t
     #1007) + 0.180D3 * t6 * t58 * t1007) * t110 * t163 / 0.720D3 - (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t1024 * t1001 + t1026 + t1027
     # * t1007 / 0.2D1) + 0.180D3 * t6 * t58 * (t1001 - t1024 * t1007) +
     # t6 * t51 * t1007) * t162 * t112 / 0.720D3
      t1046 = FJET(XB1, XB2, s, 0.0D0, -t993, t268, t995, -t1000, t1045)
      t1048 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t282, x2, 0.0D0, 
     #x4)
      t1050 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t282, x2, 0.0D0, 
     #x4)
      t1063 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t282, x2, 0.0D0, 
     #x4)
      t1080 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1006 * t1048 + 
     #t1050) + 0.180D3 * t6 * t58 * t1048) * t110 * t163 / 0.720D3 - (-0
     #.90D2 * t6 * 0.3141592653589793D1 * (t1027 * t1048 / 0.2D1 + t1063
     # - t1024 * t1050) + 0.180D3 * t6 * t58 * (t1050 - t1024 * t1048) +
     # t6 * t51 * t1048) * t162 * t112 / 0.720D3
      t1081 = FJET(XB1, XB2, s, t268, t995, 0.0D0, -t993, -t1000, t1080)
      t1083 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D
     #0, x4)
      t1085 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D
     #0, x4)
      t1088 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D
     #0, x4)
      t1099 = t6 * t51 * t1085
      t1110 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, 0.0D
     #0, x4)
      t1155 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t280 * t1083 + t2
     #81 * t1085 / 0.2D1 + t1088) + 0.180D3 * t6 * t58 * (t1083 - t280 *
     # t1085) + t1099) * t110 * t112 / 0.720D3 - (t6 * t51 * (t308 * t10
     #85 - t1083) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t313 * t1083 
     #/ 0.2D1 + t308 * t1088 - t1110 + t317 * t1085 / 0.6D1) - t6 * t33 
     #* t1085 + 0.180D3 * t6 * t58 * (t308 * t1083 - t313 * t1085 / 0.2D
     #1 - t1088)) * t112 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t1083 + t341 * t1085) - 0.180D3 * t6 * t58 * t1085) * t110 *
     # t163 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t357 * 
     #t1085 / 0.2D1 - t1088 + t356 * t1083) + 0.180D3 * t6 * t58 * (-t10
     #83 + t356 * t1085) - t1099) * t162 * t112 / 0.720D3
      t1156 = FJET(XB1, XB2, s, t268, -t270, 0.0D0, 0.0D0, 0.0D0, t1155)
      t1158 = x3 * x1
      t1159 = t2 * t1158
      t1160 = t1158 * z
      t1161 = t147 * x1
      t1162 = t147 * t272
      t1166 = Sqrt(t640 * t273 * x2 * t534)
      t1168 = 0.2D1 * t638 * t1166
      t1172 = t2 * t269 * (-x3 + t1158 - t1160 + t637 - t1161 + t1162 - 
     #x2 + t1168) * t274
      t1173 = t534 * s
      t1175 = t1173 * t1 * x1
      t1176 = x2 * x1
      t1178 = 0.1D1 - x1 + t272 - x2 + t1176 - t1176 * z - x3 + t1158 - 
     #t1160 + t637 - t1161 + t1162 + t1168
      t1181 = t2 * t269 * t1178 * t274
      t1182 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t282, x2, x3, x4)
      t1188 = log(0.4D1 * t147 * t303 * t304 * t1020 * t534)
      t1189 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t282, x2, x3, x4)
      t1198 = -0.90D2 * t6 * 0.3141592653589793D1 * (-t1182 + t1188 * t1
     #189) - 0.180D3 * t6 * t58 * t1189
      t1202 = FJET(XB1, XB2, s, t1159, t1172, -t1175, -t1181, -t1000, -t
     #1198 * t110 * t163 / 0.720D3)
      t1205 = t110 * t162 * t112
      t1209 = x3 * s * t994
      t1210 = t1173 * t994
      t1218 = log(-0.4D1 * t90 * t18 * t15 * t12 * t274 * t275 * t534)
      t1219 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, x3, 
     #x4)
      t1221 = t1218 ** 2
      t1222 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, x3, 
     #x4)
      t1225 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, x3, 
     #x4)
      t1246 = log(-0.4D1 * t274 * t15 * t116 * t166 * t596 * t275)
      t1259 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t1218 * t1219 - t1
     #221 * t1222 / 0.2D1 - t1225) + 0.180D3 * t6 * t58 * (-t1219 + t121
     #8 * t1222) - t6 * t51 * t1222) * t110 * t112 / 0.720D3 - (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t1246 * t1222 + t1219) + 0.180D3 *
     # t6 * t58 * t1222) * t110 * t163 / 0.720D3
      t1260 = FJET(XB1, XB2, s, t1159, -t1209, -t1175, t1210, 0.0D0, t12
     #59)
      t1262 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, x3, 
     #x4)
      t1263 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, x3, 
     #x4)
      t1265 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t282, 0.0D0, x3, 
     #x4)
      t1294 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t1262 + t1218 * t
     #1263 - t1221 * t1265 / 0.2D1) + 0.180D3 * t6 * t58 * (-t1263 + t12
     #18 * t1265) - t6 * t51 * t1265) * t110 * t112 / 0.720D3 - (-0.90D2
     # * t6 * 0.3141592653589793D1 * (-t1246 * t1265 + t1263) + 0.180D3 
     #* t6 * t58 * t1265) * t110 * t163 / 0.720D3
      t1295 = FJET(XB1, XB2, s, -t1175, t1210, t1159, -t1209, 0.0D0, t12
     #94)
      t1297 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t282, x2, x3, x4)
      t1299 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t282, x2, x3, x4)
      t1307 = -0.90D2 * t6 * 0.3141592653589793D1 * (t1188 * t1297 - t12
     #99) - 0.180D3 * t6 * t58 * t1297
      t1311 = FJET(XB1, XB2, s, -t1175, -t1181, t1159, t1172, -t1000, -t
     #1307 * t110 * t163 / 0.720D3)
      rrgq2qght9s1e1 = t266 * t265 + t375 * t374 + t531 * t530 + t635 * 
     #t634 + t695 * t694 + t797 * t796 + t875 * t874 + t911 * t910 + t98
     #9 * t988 + t1046 * t1045 + t1081 * t1080 + t1156 * t1155 - t1202 *
     # t1198 * t1205 / 0.720D3 + t1260 * t1259 + t1295 * t1294 - t1311 *
     # t1307 * t1205 / 0.720D3

      end function



      doubleprecision function rrgq2qght9s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = t12 * t14
      t18 = log(0.4D1 * x3 * t9 * t15)
      t19 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t21 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t22 = t18 ** 2
      t23 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t30 = 0.3141592653589793D1 * lh
      t36 = lh ** 2
      t38 = 0.3141592653589793D1 ** 2
      t40 = -0.180D3 * t36 + 0.30D2 * t38
      t41 = 0.3141592653589793D1 * t40
      t43 = t6 * t41 * t23
      t45 = 0.1D1 / x3
      t48 = t12 * t9
      t49 = t48 * t14
      t51 = log(0.4D1 * t49)
      t52 = t51 * 0.3141592653589793D1
      t55 = t51 ** 2
      t56 = t55 * 0.3141592653589793D1
      t59 = (-0.180D3 * t52 * lh - 0.45D2 * t56 + t41) * t3
      t63 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t80 = (0.90D2 * t56 * lh + 0.3141592653589793D1 * (-0.60D2 * lh * 
     #t38 + 0.2884936567583026D3 + 0.120D3 * t36 * lh) + 0.15D2 * t55 * 
     #t51 * 0.3141592653589793D1 - t52 * t40) * t3
      t87 = (0.180D3 * t30 + 0.90D2 * t52) * t3
      t91 = x2 * x3
      t94 = log(0.4D1 * t91 * t49)
      t102 = 0.180D3 * t6 * t30 * t23
      t105 = 0.1D1 / x2
      t111 = log(0.4D1 * x2 * t14 * t48)
      t113 = t111 ** 2
      t128 = x1 ** 2
      t129 = x3 * t128
      t132 = log(0.4D1 * t129 * t49)
      t140 = 0.1D1 / x1
      t143 = t6 * 0.3141592653589793D1
      t145 = t105 * t140
      t149 = x2 * t128
      t152 = log(0.4D1 * t149 * t49)
      t162 = t128 * t9
      t165 = log(0.4D1 * t162 * t15)
      t167 = t165 ** 2
      t182 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t18 * t19 + t21 +
     # t22 * t23 / 0.2D1) + 0.180D3 * t6 * t30 * (-t18 * t23 + t19) + t4
     #3) * t45 / 0.1440D4 - t59 * t5 * t19 / 0.1440D4 + t6 * 0.314159265
     #3589793D1 * t63 / 0.16D2 - t80 * t5 * t23 / 0.1440D4 - t87 * t5 * 
     #t21 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t94 * t23
     # - t19) - t102) * t45 * t105 / 0.1440D4 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t111 * t19 - t21 - t113 * t23 / 0.2D1) + 0.180D3 *
     # t6 * t30 * (-t19 + t111 * t23) - t43) * t105 / 0.1440D4 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * (t132 * t23 - t19) - t102) * t45 *
     # t140 / 0.720D3 + t143 * t23 * t45 * t145 / 0.8D1 - (-0.90D2 * t6 
     #* 0.3141592653589793D1 * (-t152 * t23 + t19) + t102) * t105 * t140
     # / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t165 * t19 +
     # t21 + t167 * t23 / 0.2D1) + 0.180D3 * t6 * t30 * (-t165 * t23 + t
     #19) + t43) * t140 / 0.720D3
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t182)
      t185 = t2 * x1
      t186 = -0.1D1 + x1
      t187 = t2 * t186
      t189 = x1 * z
      t190 = 0.1D1 - x1 + t189
      t191 = 0.1D1 / t190
      t192 = t186 ** 2
      t193 = t191 * t192
      t197 = log(0.4D1 * t129 * t9 * t15 * t193)
      t198 = -t186
      t199 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t201 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t208 = 0.180D3 * t6 * t30 * t199
      t221 = log(0.4D1 * t149 * t14 * t48 * t193)
      t232 = t14 * t191
      t236 = log(0.4D1 * t162 * t12 * t232 * t192)
      t238 = t236 ** 2
      t241 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t256 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t197 * t199 + t201
     #) + t208) * t45 * t140 / 0.720D3 - t143 * t199 * t45 * t145 / 0.8D
     #1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t201 + t221 * t199) -
     # t208) * t105 * t140 / 0.720D3 - (-0.90D2 * t6 * 0.314159265358979
     #3D1 * (t236 * t201 - t238 * t199 / 0.2D1 - t241) + 0.180D3 * t6 * 
     #t30 * (-t201 + t236 * t199) - t6 * t41 * t199) * t140 / 0.720D3
      t257 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t185, -t187, 0.0D0, t256)
      t259 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t262 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t263 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t275 = t6 * t41 * t259
      t279 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t296 = 0.180D3 * t6 * t30 * t259
      t356 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t22 * t259 / 0.2D1
     # + t262 - t18 * t263) + 0.180D3 * t6 * t30 * (t263 - t18 * t259) +
     # t275) * t45 / 0.1440D4 + t6 * 0.3141592653589793D1 * t279 / 0.16D
     #2 - t87 * t5 * t262 / 0.1440D4 - t59 * t5 * t263 / 0.1440D4 + (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t263 + t94 * t259) - t296) * 
     #t45 * t105 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t
     #113 * t259 / 0.2D1 - t262 + t111 * t263) + 0.180D3 * t6 * t30 * (-
     #t263 + t111 * t259) - t275) * t105 / 0.1440D4 - t80 * t5 * t259 / 
     #0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t132 * t259 - t
     #263) - t296) * t45 * t140 / 0.720D3 + t143 * t259 * t45 * t145 / 0
     #.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t263 - t152 * t259)
     # + t296) * t105 * t140 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (-t165 * t263 + t167 * t259 / 0.2D1 + t262) + 0.180D3 * t6
     # * t30 * (t263 - t165 * t259) + t275) * t140 / 0.720D3
      t357 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t356)
      t359 = t2 * x3
      t360 = -0.1D1 + x3
      t361 = t2 * t360
      t363 = t48 * t360
      t366 = log(-0.4D1 * x3 * t14 * t363)
      t367 = t366 ** 2
      t368 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t371 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t372 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t391 = log(-0.4D1 * t91 * t14 * t363)
      t399 = 0.180D3 * t6 * t30 * t368
      t407 = log(-0.4D1 * t129 * t14 * t363)
      t421 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t367 * t368 / 0.2
     #D1 - t371 + t366 * t372) + 0.180D3 * t6 * t30 * (-t372 + t366 * t3
     #68) - t6 * t41 * t368) * t45 / 0.1440D4 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t391 * t368 + t372) + t399) * t45 * t105 / 0.1440
     #D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t407 * t368 + t372) 
     #+ t399) * t45 * t140 / 0.720D3 - t143 * t368 * t45 * t145 / 0.8D1
      t422 = FJET(XB1, XB2, s, 0.0D0, t359, 0.0D0, -t361, 0.0D0, t421)
      t424 = 0.2D1 * t91
      t425 = cos(t7)
      t426 = -0.1D1 + x2
      t427 = x3 * t426
      t430 = Sqrt(t427 * x2 * t360)
      t432 = 0.2D1 * t425 * t430
      t434 = t2 * (0.1D1 - x2 - x3 + t424 + t432)
      t436 = t2 * (-x3 + t424 - x2 + t432)
      t437 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t438 = t91 * t9
      t443 = log(0.4D1 * t438 * t15 * t426 * t360)
      t444 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t461 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t437 + t443 * t444
     #) - 0.180D3 * t6 * t30 * t444) * t45 * t105 / 0.1440D4 + t143 * t4
     #44 * t45 * t145 / 0.8D1
      t462 = FJET(XB1, XB2, s, 0.0D0, t434, 0.0D0, -t436, 0.0D0, t461)
      t465 = x2 * s * t1
      t466 = t426 * s
      t467 = t466 * t1
      t468 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t469 = t15 * t426
      t472 = log(-0.4D1 * t438 * t469)
      t473 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t481 = 0.180D3 * t6 * t30 * t473
      t489 = log(-0.4D1 * x2 * t9 * t469)
      t491 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t492 = t489 ** 2
      t516 = log(-0.4D1 * t149 * t9 * t469)
      t526 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t468 - t472 * t473)
     # + t481) * t45 * t105 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (-t489 * t468 + t491 + t492 * t473 / 0.2D1) + 0.180D3 * t6
     # * t30 * (t468 - t489 * t473) + t6 * t41 * t473) * t105 / 0.1440D4
     # - t143 * t473 * t45 * t145 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653
     #589793D1 * (t516 * t473 - t468) - t481) * t105 * t140 / 0.720D3
      t527 = FJET(XB1, XB2, s, 0.0D0, t465, 0.0D0, -t467, 0.0D0, t526)
      t529 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t531 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t532 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t556 = 0.180D3 * t6 * t30 * t532
      t574 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t366 * t529 - t531
     # - t367 * t532 / 0.2D1) + 0.180D3 * t6 * t30 * (-t529 + t366 * t53
     #2) - t6 * t41 * t532) * t45 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592
     #653589793D1 * (t529 - t391 * t532) + t556) * t45 * t105 / 0.1440D4
     # + (-0.90D2 * t6 * 0.3141592653589793D1 * (t529 - t407 * t532) + t
     #556) * t45 * t140 / 0.720D3 - t143 * t532 * t45 * t145 / 0.8D1
      t575 = FJET(XB1, XB2, s, 0.0D0, -t361, 0.0D0, t359, 0.0D0, t574)
      t577 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t578 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t595 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t577 + t443 * t578
     #) - 0.180D3 * t6 * t30 * t578) * t45 * t105 / 0.1440D4 + t143 * t5
     #78 * t45 * t145 / 0.8D1
      t596 = FJET(XB1, XB2, s, 0.0D0, -t436, 0.0D0, t434, 0.0D0, t595)
      t598 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t599 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t607 = 0.180D3 * t6 * t30 * t599
      t614 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t643 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t598 - t472 * t599)
     # + t607) * t45 * t105 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (t492 * t599 / 0.2D1 + t614 - t489 * t598) + 0.180D3 * t6 
     #* t30 * (t598 - t489 * t599) + t6 * t41 * t599) * t105 / 0.1440D4 
     #- t143 * t599 * t45 * t145 / 0.8D1 - (-0.90D2 * t6 * 0.31415926535
     #89793D1 * (-t598 + t516 * t599) - t607) * t105 * t140 / 0.720D3
      t644 = FJET(XB1, XB2, s, 0.0D0, -t467, 0.0D0, t465, 0.0D0, t643)
      t648 = t2 * t186 * x2 * t191
      t649 = t1 * t186
      t650 = t466 * t649
      t655 = s * t13 * x2 * x1 * t186 * t191
      t656 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t661 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t667 = log(-0.4D1 * t149 * t48 * t232 * t192 * t426)
      t680 = t143 * t656 * t45 * t145 / 0.8D1 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t661 - t667 * t656) + 0.180D3 * t6 * t30 * t656) *
     # t105 * t140 / 0.720D3
      t681 = FJET(XB1, XB2, s, 0.0D0, -t648, t185, t650, -t655, t680)
      t683 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t688 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t701 = t143 * t683 * t45 * t145 / 0.8D1 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t688 - t667 * t683) + 0.180D3 * t6 * t30 * t683) *
     # t105 * t140 / 0.720D3
      t702 = FJET(XB1, XB2, s, t185, t650, 0.0D0, -t648, -t655, t701)
      t704 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t705 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t713 = 0.180D3 * t6 * t30 * t705
      t734 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t749 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t704 - t197 * t705)
     # + t713) * t45 * t140 / 0.720D3 - t143 * t705 * t45 * t145 / 0.8D1
     # - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t704 + t221 * t705) - 
     #t713) * t105 * t140 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793
     #D1 * (t236 * t704 - t238 * t705 / 0.2D1 - t734) + 0.180D3 * t6 * t
     #30 * (t236 * t705 - t704) - t6 * t41 * t705) * t140 / 0.720D3
      t750 = FJET(XB1, XB2, s, t185, -t187, 0.0D0, 0.0D0, 0.0D0, t749)
      t752 = x3 * x1
      t753 = t2 * t752
      t754 = t752 * z
      t755 = t91 * x1
      t756 = t91 * t189
      t760 = Sqrt(t427 * t190 * x2 * t360)
      t762 = 0.2D1 * t425 * t760
      t766 = t2 * t186 * (-x3 + t752 - t754 + t424 - t755 + t756 - x2 + 
     #t762) * t191
      t767 = t360 * s
      t769 = t767 * t1 * x1
      t770 = x2 * x1
      t772 = 0.1D1 - x1 + t189 - x2 + t770 - t770 * z - x3 + t752 - t754
     # + t424 - t755 + t756 + t762
      t775 = t2 * t186 * t772 * t191
      t776 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, x3, x4)
      t778 = t776 * t45 * t145
      t781 = FJET(XB1, XB2, s, t753, t766, -t769, -t775, -t655, -t143 * 
     #t778 / 0.8D1)
      t783 = t5 * 0.3141592653589793D1
      t788 = x3 * s * t649
      t789 = t767 * t649
      t790 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t798 = log(-0.4D1 * t129 * t14 * t9 * t12 * t191 * t192 * t360)
      t799 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t816 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t790 + t798 * t799
     #) - 0.180D3 * t6 * t30 * t799) * t45 * t140 / 0.720D3 + t143 * t79
     #9 * t45 * t145 / 0.8D1
      t817 = FJET(XB1, XB2, s, t753, -t788, -t769, t789, 0.0D0, t816)
      t819 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t820 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t837 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t819 + t798 * t820
     #) - 0.180D3 * t6 * t30 * t820) * t45 * t140 / 0.720D3 + t143 * t82
     #0 * t45 * t145 / 0.8D1
      t838 = FJET(XB1, XB2, s, -t769, t789, t753, -t788, 0.0D0, t837)
      t840 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, x3, x4)
      t842 = t840 * t45 * t145
      t845 = FJET(XB1, XB2, s, -t769, -t775, t753, t766, -t655, -t143 * 
     #t842 / 0.8D1)
      rrgq2qght9s1e0 = t183 * t182 + t257 * t256 + t357 * t356 + t422 * 
     #t421 + t462 * t461 + t527 * t526 + t575 * t574 + t596 * t595 + t64
     #4 * t643 + t681 * t680 + t702 * t701 + t750 * t749 - t781 * t3 * t
     #783 * t778 / 0.8D1 + t817 * t816 + t838 * t837 - t845 * t3 * t783 
     #* t842 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = t12 * t14
      t18 = log(0.4D1 * x3 * t9 * t15)
      t19 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t21 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t26 = 0.3141592653589793D1 * lh
      t29 = 0.180D3 * t6 * t26 * t19
      t31 = 0.1D1 / x3
      t34 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t39 = t12 * t9
      t42 = log(0.4D1 * t39 * t14)
      t43 = t42 * 0.3141592653589793D1
      t46 = (0.180D3 * t26 + 0.90D2 * t43) * t3
      t52 = t42 ** 2
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t62 = (-0.180D3 * t43 * lh - 0.45D2 * t52 * 0.3141592653589793D1 +
     # 0.3141592653589793D1 * (-0.180D3 * t55 + 0.30D2 * t57)) * t3
      t66 = t6 * 0.3141592653589793D1
      t67 = t19 * t31
      t68 = 0.1D1 / x2
      t75 = log(0.4D1 * x2 * t14 * t39)
      t85 = 0.1D1 / x1
      t89 = x1 ** 2
      t90 = t89 * t9
      t93 = log(0.4D1 * t90 * t15)
      t105 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t18 * t19 + t21) 
     #+ t29) * t31 / 0.1440D4 + t6 * 0.3141592653589793D1 * t34 / 0.16D2
     # - t46 * t5 * t21 / 0.1440D4 - t62 * t5 * t19 / 0.1440D4 + t66 * t
     #67 * t68 / 0.16D2 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t21 +
     # t75 * t19) - t29) * t68 / 0.1440D4 + t66 * t19 * t68 * t85 / 0.8D
     #1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t93 * t19 + t21) + t2
     #9) * t85 / 0.720D3 + t66 * t67 * t85 / 0.8D1
      t106 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t105)
      t108 = t2 * x1
      t109 = -0.1D1 + x1
      t110 = t2 * t109
      t111 = -t109
      t112 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t111, 0.0D0, 0.0D0
     #, x4)
      t117 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t111, 0.0D0, 0.0D0
     #, x4)
      t121 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t123 = t109 ** 2
      t127 = log(0.4D1 * t90 * t12 * t14 * t121 * t123)
      t143 = -t66 * t112 * t68 * t85 / 0.8D1 - (-0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t117 + t127 * t112) - 0.180D3 * t6 * t26 * t112) *
     # t85 / 0.720D3 - t66 * t112 * t31 * t85 / 0.8D1
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t108, -t110, 0.0D0, t143)
      t146 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t150 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t154 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t162 = 0.180D3 * t6 * t26 * t150
      t166 = t150 * t31
      t196 = t6 * 0.3141592653589793D1 * t146 / 0.16D2 - t62 * t5 * t150
     # / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t154 - t18 *
     # t150) + t162) * t31 / 0.1440D4 + t66 * t166 * t68 / 0.16D2 + (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t154 + t75 * t150) - t162) * 
     #t68 / 0.1440D4 - t46 * t5 * t154 / 0.1440D4 + t66 * t150 * t68 * t
     #85 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t154 - t93 * 
     #t150) + t162) * t85 / 0.720D3 + t66 * t166 * t85 / 0.8D1
      t197 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t196)
      t199 = t2 * x3
      t200 = -0.1D1 + x3
      t201 = t2 * t200
      t202 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t207 = log(-0.4D1 * x3 * t14 * t39 * t200)
      t208 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t220 = t208 * t31
      t227 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t202 + t207 * t20
     #8) - 0.180D3 * t6 * t26 * t208) * t31 / 0.1440D4 - t66 * t220 * t6
     #8 / 0.16D2 - t66 * t220 * t85 / 0.8D1
      t228 = FJET(XB1, XB2, s, 0.0D0, t199, 0.0D0, -t201, 0.0D0, t227)
      t231 = 0.2D1 * x2 * x3
      t232 = cos(t7)
      t233 = -0.1D1 + x2
      t237 = Sqrt(x3 * t233 * x2 * t200)
      t239 = 0.2D1 * t232 * t237
      t241 = t2 * (0.1D1 - x2 - x3 + t231 + t239)
      t243 = t2 * (-x3 + t231 - x2 + t239)
      t244 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t249 = FJET(XB1, XB2, s, 0.0D0, t241, 0.0D0, -t243, 0.0D0, t66 * t
     #244 * t31 * t68 / 0.16D2)
      t253 = t31 * t68
      t258 = x2 * s * t1
      t259 = t233 * s
      t260 = t259 * t1
      t261 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t266 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t271 = log(-0.4D1 * x2 * t9 * t15 * t233)
      t287 = -t66 * t261 * t31 * t68 / 0.16D2 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t266 - t271 * t261) + 0.180D3 * t6 * t26 * t261) *
     # t68 / 0.1440D4 - t66 * t261 * t68 * t85 / 0.8D1
      t288 = FJET(XB1, XB2, s, 0.0D0, t258, 0.0D0, -t260, 0.0D0, t287)
      t290 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t291 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t303 = t291 * t31
      t310 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t290 + t207 * t29
     #1) - 0.180D3 * t6 * t26 * t291) * t31 / 0.1440D4 - t66 * t303 * t6
     #8 / 0.16D2 - t66 * t303 * t85 / 0.8D1
      t311 = FJET(XB1, XB2, s, 0.0D0, -t201, 0.0D0, t199, 0.0D0, t310)
      t313 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t318 = FJET(XB1, XB2, s, 0.0D0, -t243, 0.0D0, t241, 0.0D0, t66 * t
     #313 * t31 * t68 / 0.16D2)
      t325 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t330 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t346 = -t66 * t325 * t31 * t68 / 0.16D2 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t330 - t271 * t325) + 0.180D3 * t6 * t26 * t325) *
     # t68 / 0.1440D4 - t66 * t325 * t68 * t85 / 0.8D1
      t347 = FJET(XB1, XB2, s, 0.0D0, -t260, 0.0D0, t258, 0.0D0, t346)
      t351 = t2 * t109 * x2 * t121
      t352 = t1 * t109
      t353 = t259 * t352
      t358 = s * t13 * x2 * x1 * t109 * t121
      t359 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.0D0, x
     #4)
      t364 = FJET(XB1, XB2, s, 0.0D0, -t351, t108, t353, -t358, t66 * t3
     #59 * t68 * t85 / 0.8D1)
      t368 = t68 * t85
      t372 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.0D0, x
     #4)
      t377 = FJET(XB1, XB2, s, t108, t353, 0.0D0, -t351, -t358, t66 * t3
     #72 * t68 * t85 / 0.8D1)
      t384 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t111, 0.0D0, 0.0D0
     #, x4)
      t390 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t111, 0.0D0, 0.0D0
     #, x4)
      t405 = -t66 * t384 * t68 * t85 / 0.8D1 - (-0.90D2 * t6 * 0.3141592
     #653589793D1 * (t127 * t384 - t390) - 0.180D3 * t6 * t26 * t384) * 
     #t85 / 0.720D3 - t66 * t384 * t31 * t85 / 0.8D1
      t406 = FJET(XB1, XB2, s, t108, -t110, 0.0D0, 0.0D0, 0.0D0, t405)
      t409 = t2 * x1 * x3
      t411 = x3 * s * t352
      t412 = t200 * s
      t414 = t412 * t1 * x1
      t415 = t412 * t352
      t416 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t111, 0.0D0, x3, x
     #4)
      t421 = FJET(XB1, XB2, s, t409, -t411, -t414, t415, 0.0D0, t66 * t4
     #16 * t31 * t85 / 0.8D1)
      t425 = t31 * t85
      t429 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t111, 0.0D0, x3, x
     #4)
      t434 = FJET(XB1, XB2, s, -t414, t415, t409, -t411, 0.0D0, t66 * t4
     #29 * t31 * t85 / 0.8D1)
      rrgq2qght9s1em1 = t106 * t105 + t144 * t143 + t197 * t196 + t228 *
     # t227 + t249 * t3 * t5 * 0.3141592653589793D1 * t244 * t253 / 0.16
     #D2 + t288 * t287 + t311 * t310 + t318 * t3 * t5 * 0.31415926535897
     #93D1 * t313 * t253 / 0.16D2 + t347 * t346 + t364 * t3 * t5 * 0.314
     #1592653589793D1 * t359 * t368 / 0.8D1 + t377 * t3 * t5 * 0.3141592
     #653589793D1 * t372 * t368 / 0.8D1 + t406 * t405 + t421 * t3 * t5 *
     # 0.3141592653589793D1 * t416 * t425 / 0.8D1 + t434 * t3 * t5 * 0.3
     #141592653589793D1 * t429 * t425 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t27 = z ** 2
      t30 = Sin(x4 * 0.3141592653589793D1)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t41 = (0.180D3 * 0.3141592653589793D1 * lh + 0.90D2 * t37 * 0.3141
     #592653589793D1) * t3
      t45 = t6 * t8 * t9 / 0.16D2 + t6 * t8 * t13 / 0.16D2 + t6 * t8 * t
     #17 / 0.8D1 + t6 * 0.3141592653589793D1 * t21 / 0.16D2 - t41 * t5 *
     # t7 / 0.1440D4
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t45)
      t48 = t2 * x1
      t49 = -0.1D1 + x1
      t50 = t2 * t49
      t51 = -t49
      t52 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t51, 0.0D0, 0.0D0, 
     #x4)
      t54 = 0.3141592653589793D1 * t52 * t17
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t48, -t50, 0.0D0, -t6 * t54 
     #/ 0.8D1)
      t62 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t63 = 0.3141592653589793D1 * t62
      t73 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t80 = t6 * t63 * t9 / 0.16D2 + t6 * t63 * t13 / 0.16D2 + t6 * t63 
     #* t17 / 0.8D1 + t6 * 0.3141592653589793D1 * t73 / 0.16D2 - t41 * t
     #5 * t62 / 0.1440D4
      t81 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t80)
      t83 = t2 * x3
      t85 = t2 * (-0.1D1 + x3)
      t86 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t88 = 0.3141592653589793D1 * t86 * t9
      t91 = FJET(XB1, XB2, s, 0.0D0, t83, 0.0D0, -t85, 0.0D0, -t6 * t88 
     #/ 0.16D2)
      t97 = x2 * s * t1
      t100 = (-0.1D1 + x2) * s * t1
      t101 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t103 = 0.3141592653589793D1 * t101 * t13
      t106 = FJET(XB1, XB2, s, 0.0D0, t97, 0.0D0, -t100, 0.0D0, -t6 * t1
     #03 / 0.16D2)
      t111 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t113 = 0.3141592653589793D1 * t111 * t9
      t116 = FJET(XB1, XB2, s, 0.0D0, -t85, 0.0D0, t83, 0.0D0, -t6 * t11
     #3 / 0.16D2)
      t121 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t123 = 0.3141592653589793D1 * t121 * t13
      t126 = FJET(XB1, XB2, s, 0.0D0, -t100, 0.0D0, t97, 0.0D0, -t6 * t1
     #23 / 0.16D2)
      t131 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t51, 0.0D0, 0.0D0,
     # x4)
      t133 = 0.3141592653589793D1 * t131 * t17
      t136 = FJET(XB1, XB2, s, t48, -t50, 0.0D0, 0.0D0, 0.0D0, -t6 * t13
     #3 / 0.8D1)
      rrgq2qght9s1em2 = t46 * t45 - t57 * t3 * t5 * t54 / 0.8D1 + t81 * 
     #t80 - t91 * t3 * t5 * t88 / 0.16D2 - t106 * t3 * t5 * t103 / 0.16D
     #2 - t116 * t3 * t5 * t113 / 0.16D2 - t126 * t3 * t5 * t123 / 0.16D
     #2 - t136 * t3 * t5 * t133 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.16D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.16D2)
      rrgq2qght9s1em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2

      end function



      doubleprecision function rrgq2qght9s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      rrgq2qght9s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght9s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = lh ** 2
      t9 = 0.3141592653589793D1 ** 2
      t11 = -0.180D3 * t7 + 0.30D2 * t9
      t12 = 0.3141592653589793D1 * t11
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = x3 * t15
      t17 = x4 * 0.3141592653589793D1
      t18 = Sin(t17)
      t19 = t18 ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t19 * t21
      t25 = log(0.4D1 * t16 * t22)
      t26 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t28 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t32 = t25 ** 2
      t35 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t36 = t32 * t25
      t39 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t49 = -0.60D2 * lh * t9 + 0.2884936567583026D3 + 0.120D3 * t7 * lh
      t50 = 0.3141592653589793D1 * t49
      t52 = t6 * t50 * t26
      t53 = 0.3141592653589793D1 * lh
      t62 = 0.1D1 / x3
      t65 = t15 * t19
      t66 = t65 * t21
      t68 = log(0.4D1 * t66)
      t69 = t68 ** 2
      t72 = t69 * t68
      t92 = t9 ** 2
      t93 = t7 ** 2
      t97 = -0.5769873135166051D3 * lh - t92 - 0.60D2 * t93 + 0.60D2 * t
     #7 * t9
      t104 = rrgq2qgh91J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t105 = t69 ** 2
      t115 = x1 ** 2
      t116 = x3 * t115
      t119 = log(0.4D1 * t116 * t66)
      t120 = t119 ** 2
      t137 = 0.1D1 / x1
      t140 = t115 * t19
      t141 = t15 * t21
      t144 = log(0.4D1 * t140 * t141)
      t149 = t144 ** 2
      t152 = t149 * t144
      t170 = x2 * x3
      t171 = t170 * t115
      t174 = log(0.4D1 * t171 * t66)
      t176 = -0.1D1 + x2
      t181 = log(-0.4D1 * t171 * t65 * t21 * t176)
      t182 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t184 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t189 = -t182 + t26
      t195 = 0.1D1 / x2
      t196 = t195 * t137
      t199 = x2 * t115
      t204 = log(-0.4D1 * t199 * t19 * t141 * t176)
      t206 = t204 ** 2
      t211 = log(0.4D1 * t199 * t66)
      t213 = t211 ** 2
      t216 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t227 = -t189
      t229 = t6 * t12 * t227
      t234 = t170 * t15
      t235 = t22 * t176
      t238 = log(-0.4D1 * t234 * t235)
      t242 = log(0.4D1 * t170 * t66)
      t244 = t238 ** 2
      t247 = t242 ** 2
      t264 = x2 * t15
      t267 = log(0.4D1 * t264 * t22)
      t271 = log(-0.4D1 * t264 * t235)
      t276 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t277 = t271 ** 2
      t278 = t277 * t271
      t282 = t267 ** 2
      t285 = t282 * t267
      t310 = (t6 * t12 * (t25 * t26 - t28) - 0.90D2 * t6 * 0.31415926535
     #89793D1 * (-t32 * t28 / 0.2D1 - t35 + t36 * t26 / 0.6D1 + t25 * t3
     #9) - t52 + 0.180D3 * t6 * t53 * (t25 * t28 - t39 - t32 * t26 / 0.2
     #D1)) * t62 / 0.1440D4 - (0.180D3 * (t69 * t28 / 0.2D1 + t35 - t72 
     #* t26 / 0.6D1 - t68 * t39) * 0.3141592653589793D1 * lh + (-t68 * t
     #28 + t39 + t69 * t26 / 0.2D1) * 0.3141592653589793D1 * t11 + (t28 
     #- t68 * t26) * 0.3141592653589793D1 * t49 + t26 * 0.31415926535897
     #93D1 * t97 - 0.90D2 * (-t72 * t28 / 0.6D1 + t69 * t39 / 0.2D1 - t6
     #8 * t35 + t104 + t105 * t26 / 0.24D2) * 0.3141592653589793D1) * t3
     # * t5 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t120 *
     # t26 / 0.2D1 + t119 * t28 - t39) + 0.180D3 * t6 * t53 * (t119 * t2
     #6 - t28) - t6 * t12 * t26) * t62 * t137 / 0.720D3 + (t6 * t12 * (t
     #144 * t26 - t28) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t149 * t
     #28 / 0.2D1 - t35 + t152 * t26 / 0.6D1 + t144 * t39) - t52 + 0.180D
     #3 * t6 * t53 * (t144 * t28 - t39 - t149 * t26 / 0.2D1)) * t137 / 0
     #.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t174 * t26 + t18
     #1 * t182 - t184 + t28) + 0.180D3 * t6 * t53 * t189) * t62 * t196 /
     # 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t204 * t184 + 
     #t206 * t182 / 0.2D1 + t211 * t28 - t39 - t213 * t26 / 0.2D1 + t216
     #) + 0.180D3 * t6 * t53 * (t211 * t26 - t204 * t182 + t184 - t28) +
     # t229) * t195 * t137 / 0.720D3 + (-0.90D2 * t6 * 0.314159265358979
     #3D1 * (t216 - t39 - t238 * t184 + t242 * t28 + t244 * t182 / 0.2D1
     # - t247 * t26 / 0.2D1) + 0.180D3 * t6 * t53 * (-t238 * t182 + t242
     # * t26 - t28 + t184) + t229) * t62 * t195 / 0.1440D4 + (t6 * t12 *
     # (-t28 + t267 * t26 + t184 - t271 * t182) - 0.90D2 * t6 * 0.314159
     #2653589793D1 * (t276 - t278 * t182 / 0.6D1 - t271 * t216 - t282 * 
     #t28 / 0.2D1 - t35 + t285 * t26 / 0.6D1 + t277 * t184 / 0.2D1 + t26
     #7 * t39) + t6 * t50 * t227 + 0.180D3 * t6 * t53 * (-t282 * t26 / 0
     #.2D1 + t267 * t28 - t39 - t271 * t184 + t216 + t277 * t182 / 0.2D1
     #)) * t195 / 0.1440D4
      t311 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t310)
      t313 = -0.1D1 + x1
      t314 = t2 * t313
      t315 = t2 * x1
      t317 = 0.1D1 / t13
      t318 = t317 * t21
      t319 = x1 * z
      t320 = -z - x1 + t319
      t321 = 0.1D1 / t320
      t322 = t313 ** 2
      t323 = t321 * t322
      t327 = log(-0.4D1 * t116 * t19 * t318 * t323)
      t328 = t327 ** 2
      t329 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t332 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t333 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t345 = t6 * t12 * t329
      t349 = t140 * t317
      t350 = t21 * t321
      t354 = log(-0.4D1 * t349 * t350 * t322)
      t359 = t354 ** 2
      t362 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t363 = t359 * t354
      t383 = t170 * t115 * t21
      t384 = t19 * t317
      t385 = t384 * t323
      t388 = log(-0.4D1 * t383 * t385)
      t403 = log(-0.4D1 * t199 * t21 * t385)
      t404 = t403 ** 2
      t421 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t328 * t329 / 0.2D1
     # + t332 - t327 * t333) + 0.180D3 * t6 * t53 * (t333 - t327 * t329)
     # + t345) * t62 * t137 / 0.720D3 + (t6 * t12 * (t333 - t354 * t329)
     # - 0.90D2 * t6 * 0.3141592653589793D1 * (t359 * t333 / 0.2D1 + t36
     #2 - t363 * t329 / 0.6D1 - t354 * t332) + t6 * t50 * t329 + 0.180D3
     # * t6 * t53 * (-t354 * t333 + t359 * t329 / 0.2D1 + t332)) * t137 
     #/ 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t388 * t329 - 
     #t333) - 0.180D3 * t6 * t53 * t329) * t62 * t196 / 0.720D3 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * (t404 * t329 / 0.2D1 - t403 * t33
     #3 + t332) + 0.180D3 * t6 * t53 * (t333 - t403 * t329) + t345) * t1
     #95 * t137 / 0.720D3
      t422 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t314, t315, 0.0D0, t421)
      t424 = x2 * x1
      t426 = t2 * t424 * t321
      t428 = t1 * x1
      t429 = t176 * s * t428
      t434 = s * t20 * x2 * x1 * t313 * t321
      t435 = t170 * t140
      t440 = log(0.4D1 * t435 * t318 * t323 * t176)
      t441 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t443 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t454 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t456 = t322 * t176
      t460 = log(0.4D1 * t199 * t384 * t350 * t456)
      t462 = t460 ** 2
      t480 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t440 * t441 + t44
     #3) + 0.180D3 * t6 * t53 * t441) * t62 * t196 / 0.720D3 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t454 + t460 * t443 - t462 * t441 /
     # 0.2D1) + 0.180D3 * t6 * t53 * (-t443 + t460 * t441) - t6 * t12 * 
     #t441) * t195 * t137 / 0.720D3
      t481 = FJET(XB1, XB2, s, 0.0D0, -t426, -t314, -t429, t434, t480)
      t483 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t486 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t487 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t490 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t510 = rrgq2qgh92J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t555 = t6 * t50 * t487
      t567 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t569 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t574 = -t567 + t487
      t587 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t599 = -t574
      t601 = t6 * t12 * t599
      t657 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t684 = -(0.180D3 * (t69 * t483 / 0.2D1 + t486 - t72 * t487 / 0.6D1
     # - t68 * t490) * 0.3141592653589793D1 * lh + (t69 * t487 / 0.2D1 +
     # t490 - t68 * t483) * 0.3141592653589793D1 * t11 + (t483 - t68 * t
     #487) * 0.3141592653589793D1 * t49 + t487 * 0.3141592653589793D1 * 
     #t97 - 0.90D2 * (t69 * t490 / 0.2D1 + t510 - t72 * t483 / 0.6D1 + t
     #105 * t487 / 0.24D2 - t68 * t486) * 0.3141592653589793D1) * t3 * t
     #5 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t490 + t11
     #9 * t483 - t120 * t487 / 0.2D1) + 0.180D3 * t6 * t53 * (-t483 + t1
     #19 * t487) - t6 * t12 * t487) * t62 * t137 / 0.720D3 + (t6 * t12 *
     # (-t483 + t144 * t487) - 0.90D2 * t6 * 0.3141592653589793D1 * (t14
     #4 * t490 - t486 - t149 * t483 / 0.2D1 + t152 * t487 / 0.6D1) - t55
     #5 + 0.180D3 * t6 * t53 * (t144 * t483 - t149 * t487 / 0.2D1 - t490
     #)) * t137 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t17
     #4 * t487 + t181 * t567 - t569 + t483) + 0.180D3 * t6 * t53 * t574)
     # * t62 * t196 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 * (
     #-t213 * t487 / 0.2D1 - t490 + t211 * t483 + t206 * t567 / 0.2D1 + 
     #t587 - t204 * t569) + 0.180D3 * t6 * t53 * (-t483 + t569 - t204 * 
     #t567 + t211 * t487) + t601) * t195 * t137 / 0.720D3 + (t6 * t12 * 
     #(-t483 + t25 * t487) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t32 
     #* t483 / 0.2D1 - t486 + t36 * t487 / 0.6D1 + t25 * t490) - t555 + 
     #0.180D3 * t6 * t53 * (-t32 * t487 / 0.2D1 - t490 + t25 * t483)) * 
     #t62 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t238 * t
     #569 + t587 - t247 * t487 / 0.2D1 - t490 + t242 * t483 + t244 * t56
     #7 / 0.2D1) + 0.180D3 * t6 * t53 * (-t238 * t567 - t483 + t242 * t4
     #87 + t569) + t601) * t62 * t195 / 0.1440D4 + (t6 * t12 * (-t483 + 
     #t267 * t487 + t569 - t271 * t567) - 0.90D2 * t6 * 0.31415926535897
     #93D1 * (t277 * t569 / 0.2D1 + t267 * t490 + t657 - t278 * t567 / 0
     #.6D1 - t271 * t587 - t282 * t483 / 0.2D1 - t486 + t285 * t487 / 0.
     #6D1) + t6 * t50 * t599 + 0.180D3 * t6 * t53 * (-t282 * t487 / 0.2D
     #1 - t490 + t267 * t483 + t587 - t271 * t569 + t277 * t567 / 0.2D1)
     #) * t195 / 0.1440D4
      t685 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t684)
      t687 = t2 * x3
      t688 = -0.1D1 + x3
      t689 = t2 * t688
      t690 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t691 = t22 * t688
      t694 = log(-0.4D1 * t16 * t691)
      t695 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t700 = t694 ** 2
      t703 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t704 = t700 * t694
      t707 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t725 = t176 * t688
      t729 = log(0.4D1 * t234 * t22 * t725)
      t730 = t729 ** 2
      t731 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t734 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t735 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t739 = log(-0.4D1 * t234 * t691)
      t740 = t739 ** 2
      t754 = -t731 + t695
      t765 = log(-0.4D1 * t116 * t21 * t65 * t688)
      t766 = t765 ** 2
      t789 = log(-0.4D1 * t171 * t22 * t15 * t688)
      t794 = log(0.4D1 * t435 * t141 * t725)
      t808 = (t6 * t12 * (t690 - t694 * t695) - 0.90D2 * t6 * 0.31415926
     #53589793D1 * (t700 * t690 / 0.2D1 + t703 - t704 * t695 / 0.6D1 - t
     #694 * t707) + t6 * t50 * t695 + 0.180D3 * t6 * t53 * (t700 * t695 
     #/ 0.2D1 + t707 - t694 * t690)) * t62 / 0.1440D4 + (-0.90D2 * t6 * 
     #0.3141592653589793D1 * (-t730 * t731 / 0.2D1 - t734 + t729 * t735 
     #+ t707 + t740 * t695 / 0.2D1 - t739 * t690) + 0.180D3 * t6 * t53 *
     # (t729 * t731 - t739 * t695 - t735 + t690) + t6 * t12 * t754) * t6
     #2 * t195 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t766
     # * t695 / 0.2D1 - t765 * t690 + t707) + 0.180D3 * t6 * t53 * (-t76
     #5 * t695 + t690) + t6 * t12 * t695) * t62 * t137 / 0.720D3 - (-0.9
     #0D2 * t6 * 0.3141592653589793D1 * (t789 * t695 - t794 * t731 + t73
     #5 - t690) - 0.180D3 * t6 * t53 * t754) * t62 * t196 / 0.720D3
      t809 = FJET(XB1, XB2, s, t687, 0.0D0, -t689, 0.0D0, 0.0D0, t808)
      t811 = t688 * s
      t812 = t1 * t313
      t813 = t811 * t812
      t814 = t811 * t428
      t816 = x3 * s * t812
      t817 = x3 * x1
      t818 = t2 * t817
      t825 = log(0.4D1 * t116 * t22 * t317 * t321 * t322 * t688)
      t826 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t828 = t825 ** 2
      t829 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t832 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t851 = log(0.4D1 * t383 * t384 * t323 * t688)
      t864 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t825 * t826 - t828 
     #* t829 / 0.2D1 - t832) + 0.180D3 * t6 * t53 * (-t826 + t825 * t829
     #) - t6 * t12 * t829) * t62 * t137 / 0.720D3 - (-0.90D2 * t6 * 0.31
     #41592653589793D1 * (-t851 * t829 + t826) + 0.180D3 * t6 * t53 * t8
     #29) * t62 * t196 / 0.720D3
      t865 = FJET(XB1, XB2, s, t813, -t814, -t816, t818, 0.0D0, t864)
      t869 = x3 * z
      t870 = t817 * z
      t871 = t170 * z
      t872 = t170 * x1
      t873 = t170 * t319
      t874 = cos(t17)
      t879 = Sqrt(-x3 * t176 * t320 * x2 * t688)
      t881 = 0.2D1 * t874 * t879
      t882 = z + x1 - t319 - x2 * z - t424 + t424 * z - t869 - t817 + t8
     #70 + t871 + t872 - t873 + t170 + t881
      t885 = t2 * x1 * t882 * t321
      t889 = t2 * x1 * (-t869 - t817 + t870 + t871 + t872 - t873 - x2 + 
     #t170 + t881) * t321
      t890 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t896 = log(-0.4D1 * t170 * t349 * t350 * t456 * t688)
      t897 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t906 = -0.90D2 * t6 * 0.3141592653589793D1 * (-t890 + t896 * t897)
     # - 0.180D3 * t6 * t53 * t897
      t910 = FJET(XB1, XB2, s, t813, -t885, -t816, t889, t434, -t906 * t
     #62 * t196 / 0.720D3)
      t913 = t62 * t195 * t137
      t916 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t919 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t920 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t932 = t6 * t12 * t916
      t943 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t988 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t328 * t916 / 0.2D1
     # + t919 - t327 * t920) + 0.180D3 * t6 * t53 * (-t327 * t916 + t920
     #) + t932) * t62 * t137 / 0.720D3 + (t6 * t12 * (-t354 * t916 + t92
     #0) - 0.90D2 * t6 * 0.3141592653589793D1 * (t359 * t920 / 0.2D1 - t
     #354 * t919 + t943 - t363 * t916 / 0.6D1) + t6 * t50 * t916 + 0.180
     #D3 * t6 * t53 * (-t354 * t920 + t359 * t916 / 0.2D1 + t919)) * t13
     #7 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t920 + t388
     # * t916) - 0.180D3 * t6 * t53 * t916) * t62 * t196 / 0.720D3 + (-0
     #.90D2 * t6 * 0.3141592653589793D1 * (t404 * t916 / 0.2D1 + t919 - 
     #t403 * t920) + 0.180D3 * t6 * t53 * (-t403 * t916 + t920) + t932) 
     #* t195 * t137 / 0.720D3
      t989 = FJET(XB1, XB2, s, -t314, t315, 0.0D0, 0.0D0, 0.0D0, t988)
      t991 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t993 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t1004 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1023 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t440 * t991 + t9
     #93) + 0.180D3 * t6 * t53 * t991) * t62 * t196 / 0.720D3 + (-0.90D2
     # * t6 * 0.3141592653589793D1 * (-t1004 - t462 * t991 / 0.2D1 + t46
     #0 * t993) + 0.180D3 * t6 * t53 * (-t993 + t460 * t991) - t6 * t12 
     #* t991) * t195 * t137 / 0.720D3
      t1024 = FJET(XB1, XB2, s, -t314, -t429, 0.0D0, -t426, t434, t1023)
      t1026 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3,
     # x4)
      t1027 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3,
     # x4)
      t1034 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3,
     # x4)
      t1037 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3,
     # x4)
      t1055 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t1058 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t1060 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t1074 = t1027 - t1055
      t1113 = (t6 * t12 * (t1026 - t694 * t1027) - 0.90D2 * t6 * 0.31415
     #92653589793D1 * (t700 * t1026 / 0.2D1 + t1034 - t704 * t1027 / 0.6
     #D1 - t694 * t1037) + t6 * t50 * t1027 + 0.180D3 * t6 * t53 * (-t69
     #4 * t1026 + t1037 + t700 * t1027 / 0.2D1)) * t62 / 0.1440D4 + (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t730 * t1055 / 0.2D1 + t729 *
     # t1058 - t1060 + t740 * t1027 / 0.2D1 - t739 * t1026 + t1037) + 0.
     #180D3 * t6 * t53 * (t729 * t1055 + t1026 - t739 * t1027 - t1058) +
     # t6 * t12 * t1074) * t62 * t195 / 0.1440D4 + (-0.90D2 * t6 * 0.314
     #1592653589793D1 * (-t765 * t1026 + t766 * t1027 / 0.2D1 + t1037) +
     # 0.180D3 * t6 * t53 * (t1026 - t765 * t1027) + t6 * t12 * t1027) *
     # t62 * t137 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t
     #1026 - t794 * t1055 + t789 * t1027 + t1058) - 0.180D3 * t6 * t53 *
     # t1074) * t62 * t196 / 0.720D3
      t1114 = FJET(XB1, XB2, s, -t689, 0.0D0, t687, 0.0D0, 0.0D0, t1113)
      t1116 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4
     #)
      t1119 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4
     #)
      t1120 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4
     #)
      t1148 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t828 * t1116 / 0.
     #2D1 - t1119 + t825 * t1120) + 0.180D3 * t6 * t53 * (-t1120 + t825 
     #* t1116) - t6 * t12 * t1116) * t62 * t137 / 0.720D3 - (-0.90D2 * t
     #6 * 0.3141592653589793D1 * (t1120 - t851 * t1116) + 0.180D3 * t6 *
     # t53 * t1116) * t62 * t196 / 0.720D3
      t1149 = FJET(XB1, XB2, s, -t816, t818, t813, -t814, 0.0D0, t1148)
      t1151 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t1152 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t1161 = -0.90D2 * t6 * 0.3141592653589793D1 * (-t1151 + t896 * t11
     #52) - 0.180D3 * t6 * t53 * t1152
      t1165 = FJET(XB1, XB2, s, -t816, t889, t813, -t885, t434, -t1161 *
     # t62 * t196 / 0.720D3)
      rrgq2qght9s2e1 = t311 * t310 + t422 * t421 + t481 * t480 + t685 * 
     #t684 + t809 * t808 + t865 * t864 - t910 * t906 * t913 / 0.720D3 + 
     #t989 * t988 + t1024 * t1023 + t1114 * t1113 + t1149 * t1148 - t116
     #5 * t1161 * t913 / 0.720D3

      end function



      doubleprecision function rrgq2qght9s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t22 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t23 = t19 ** 2
      t24 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t31 = 0.3141592653589793D1 * lh
      t37 = lh ** 2
      t39 = 0.3141592653589793D1 ** 2
      t41 = -0.180D3 * t37 + 0.30D2 * t39
      t42 = 0.3141592653589793D1 * t41
      t44 = t6 * t42 * t24
      t46 = 0.1D1 / x3
      t49 = t9 * t13
      t50 = t49 * t15
      t52 = log(0.4D1 * t50)
      t54 = t52 ** 2
      t66 = -0.60D2 * lh * t39 + 0.2884936567583026D3 + 0.120D3 * t37 * 
     #lh
      t70 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t71 = t54 * t52
      t86 = x2 * x3
      t87 = t86 * t9
      t88 = -0.1D1 + x2
      t89 = t16 * t88
      t92 = log(-0.4D1 * t87 * t89)
      t93 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t97 = log(0.4D1 * t86 * t50)
      t99 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t104 = -t24 + t93
      t107 = 0.180D3 * t6 * t31 * t104
      t110 = 0.1D1 / x2
      t113 = x2 * t9
      t116 = log(0.4D1 * t113 * t16)
      t117 = t116 ** 2
      t123 = log(-0.4D1 * t113 * t89)
      t125 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t126 = t123 ** 2
      t144 = x1 ** 2
      t145 = x3 * t144
      t148 = log(0.4D1 * t145 * t50)
      t159 = 0.1D1 / x1
      t162 = t6 * 0.3141592653589793D1
      t165 = t110 * t159
      t169 = x2 * t144
      t172 = log(0.4D1 * t169 * t50)
      t175 = t9 * t15
      t179 = log(-0.4D1 * t169 * t13 * t175 * t88)
      t189 = t144 * t13
      t192 = log(0.4D1 * t189 * t175)
      t194 = t192 ** 2
      t209 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t19 * t20 - t22 - t
     #23 * t24 / 0.2D1) + 0.180D3 * t6 * t31 * (t19 * t24 - t20) - t44) 
     #* t46 / 0.1440D4 - (0.180D3 * (-t52 * t20 + t22 + t54 * t24 / 0.2D
     #1) * 0.3141592653589793D1 * lh + t24 * 0.3141592653589793D1 * t66 
     #- 0.90D2 * (t54 * t20 / 0.2D1 + t70 - t71 * t24 / 0.6D1 - t52 * t2
     #2) * 0.3141592653589793D1 + (t20 - t52 * t24) * 0.3141592653589793
     #D1 * t41) * t3 * t5 / 0.1440D4 + (-0.90D2 * t6 * 0.314159265358979
     #3D1 * (-t92 * t93 + t97 * t24 - t20 + t99) + t107) * t46 * t110 / 
     #0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t117 * t24 / 0
     #.2D1 + t116 * t20 - t22 - t123 * t99 + t125 + t126 * t93 / 0.2D1) 
     #+ 0.180D3 * t6 * t31 * (-t20 + t116 * t24 + t99 - t123 * t93) + t6
     # * t42 * t104) * t110 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (t148 * t24 - t20) - 0.180D3 * t6 * t31 * t24) * t46 * t15
     #9 / 0.720D3 - t162 * t104 * t46 * t165 / 0.8D1 + (-0.90D2 * t6 * 0
     #.3141592653589793D1 * (t172 * t24 - t179 * t93 + t99 - t20) + t107
     #) * t110 * t159 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 *
     # (t192 * t20 - t22 - t194 * t24 / 0.2D1) + 0.180D3 * t6 * t31 * (t
     #192 * t24 - t20) - t44) * t159 / 0.720D3
      t210 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t209)
      t212 = -0.1D1 + x1
      t213 = t2 * t212
      t214 = t2 * x1
      t215 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t217 = 0.1D1 / t7
      t219 = x1 * z
      t220 = -z - x1 + t219
      t221 = 0.1D1 / t220
      t222 = t212 ** 2
      t223 = t221 * t222
      t227 = log(-0.4D1 * t145 * t13 * t217 * t15 * t223)
      t228 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t236 = 0.180D3 * t6 * t31 * t228
      t246 = t13 * t217
      t250 = log(-0.4D1 * t169 * t15 * t246 * t223)
      t261 = t15 * t221
      t265 = log(-0.4D1 * t189 * t217 * t261 * t222)
      t267 = t265 ** 2
      t270 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t285 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t215 - t228 * t227)
     # + t236) * t46 * t159 / 0.720D3 - t162 * t228 * t46 * t165 / 0.8D1
     # + (-0.90D2 * t6 * 0.3141592653589793D1 * (t215 - t250 * t228) + t
     #236) * t110 * t159 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t265 * t215 + t267 * t228 / 0.2D1 + t270) + 0.180D3 * t6 * t
     #31 * (t215 - t265 * t228) + t6 * t42 * t228) * t159 / 0.720D3
      t286 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t213, t214, 0.0D0, t285)
      t288 = x2 * x1
      t290 = t2 * t288 * t221
      t292 = t1 * x1
      t293 = t88 * s * t292
      t298 = s * t14 * x2 * x1 * t212 * t221
      t299 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t304 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t310 = log(0.4D1 * t169 * t246 * t261 * t222 * t88)
      t323 = t162 * t299 * t46 * t165 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t304 + t310 * t299) - 0.180D3 * t6 * t31 * t299) 
     #* t110 * t159 / 0.720D3
      t324 = FJET(XB1, XB2, s, 0.0D0, -t290, -t213, -t293, t298, t323)
      t326 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t329 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t330 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t340 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t355 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t358 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t363 = t355 - t326
      t366 = 0.180D3 * t6 * t31 * t363
      t374 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t406 = t6 * t42 * t326
      t452 = -(0.180D3 * (t54 * t326 / 0.2D1 + t329 - t52 * t330) * 0.31
     #41592653589793D1 * lh + t326 * 0.3141592653589793D1 * t66 - 0.90D2
     # * (t54 * t330 / 0.2D1 + t340 - t71 * t326 / 0.6D1 - t52 * t329) *
     # 0.3141592653589793D1 + (t330 - t52 * t326) * 0.3141592653589793D1
     # * t41) * t3 * t5 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t92 * t355 - t330 + t97 * t326 + t358) + t366) * t46 * t110 
     #/ 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t117 * t326 
     #/ 0.2D1 - t329 + t116 * t330 + t374 - t123 * t358 + t126 * t355 / 
     #0.2D1) + 0.180D3 * t6 * t31 * (-t330 + t116 * t326 + t358 - t123 *
     # t355) + t6 * t42 * t363) * t110 / 0.1440D4 + (-0.90D2 * t6 * 0.31
     #41592653589793D1 * (-t23 * t326 / 0.2D1 - t329 + t19 * t330) + 0.1
     #80D3 * t6 * t31 * (-t330 + t19 * t326) - t406) * t46 / 0.1440D4 + 
     #(-0.90D2 * t6 * 0.3141592653589793D1 * (-t330 + t148 * t326) - 0.1
     #80D3 * t6 * t31 * t326) * t46 * t159 / 0.720D3 - t162 * t363 * t46
     # * t165 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t330 + 
     #t358 - t179 * t355 + t172 * t326) + t366) * t110 * t159 / 0.720D3 
     #+ (-0.90D2 * t6 * 0.3141592653589793D1 * (t192 * t330 - t194 * t32
     #6 / 0.2D1 - t329) + 0.180D3 * t6 * t31 * (-t330 + t192 * t326) - t
     #406) * t159 / 0.720D3
      t453 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t452)
      t455 = t2 * x3
      t456 = -0.1D1 + x3
      t457 = t2 * t456
      t458 = t16 * t456
      t461 = log(-0.4D1 * t10 * t458)
      t462 = t461 ** 2
      t463 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t466 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t467 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t487 = log(-0.4D1 * t145 * t15 * t49 * t456)
      t500 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t501 = -t463 + t500
      t510 = log(0.4D1 * t87 * t16 * t88 * t456)
      t514 = log(-0.4D1 * t87 * t458)
      t516 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t529 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t462 * t463 / 0.2D1
     # + t466 - t461 * t467) + 0.180D3 * t6 * t31 * (t467 - t461 * t463)
     # + t6 * t42 * t463) * t46 / 0.1440D4 + (-0.90D2 * t6 * 0.314159265
     #3589793D1 * (-t487 * t463 + t467) + 0.180D3 * t6 * t31 * t463) * t
     #46 * t159 / 0.720D3 + t162 * t501 * t46 * t165 / 0.8D1 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (t510 * t500 - t514 * t463 - t516 + 
     #t467) - 0.180D3 * t6 * t31 * t501) * t46 * t110 / 0.1440D4
      t530 = FJET(XB1, XB2, s, t455, 0.0D0, -t457, 0.0D0, 0.0D0, t529)
      t532 = t456 * s
      t533 = t1 * t212
      t534 = t532 * t533
      t535 = t532 * t292
      t537 = x3 * s * t533
      t538 = x3 * x1
      t539 = t2 * t538
      t540 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t547 = log(0.4D1 * t145 * t16 * t217 * t221 * t222 * t456)
      t548 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t565 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t540 + t547 * t548
     #) - 0.180D3 * t6 * t31 * t548) * t46 * t159 / 0.720D3 + t162 * t54
     #8 * t46 * t165 / 0.8D1
      t566 = FJET(XB1, XB2, s, t534, -t535, -t537, t539, 0.0D0, t565)
      t570 = x3 * z
      t571 = t538 * z
      t572 = t86 * z
      t573 = t86 * x1
      t574 = t86 * t219
      t575 = cos(t11)
      t580 = Sqrt(-x3 * t88 * t220 * x2 * t456)
      t582 = 0.2D1 * t575 * t580
      t583 = z + x1 - t219 - x2 * z - t288 + t288 * z - t570 - t538 + t5
     #71 + t572 + t573 - t574 + t86 + t582
      t586 = t2 * x1 * t583 * t221
      t590 = t2 * x1 * (-t570 - t538 + t571 + t572 + t573 - t574 - x2 + 
     #t86 + t582) * t221
      t591 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t593 = t591 * t46 * t165
      t596 = FJET(XB1, XB2, s, t534, -t586, -t537, t590, t298, -t162 * t
     #593 / 0.8D1)
      t598 = t5 * 0.3141592653589793D1
      t602 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t604 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t611 = 0.180D3 * t6 * t31 * t602
      t632 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t647 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t227 * t602 + t604
     #) + t611) * t46 * t159 / 0.720D3 - t162 * t602 * t46 * t165 / 0.8D
     #1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t250 * t602 + t604) +
     # t611) * t110 * t159 / 0.720D3 + (-0.90D2 * t6 * 0.314159265358979
     #3D1 * (-t265 * t604 + t267 * t602 / 0.2D1 + t632) + 0.180D3 * t6 *
     # t31 * (-t265 * t602 + t604) + t6 * t42 * t602) * t159 / 0.720D3
      t648 = FJET(XB1, XB2, s, -t213, t214, 0.0D0, 0.0D0, 0.0D0, t647)
      t650 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t655 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t668 = t162 * t650 * t46 * t165 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t655 + t310 * t650) - 0.180D3 * t6 * t31 * t650) 
     #* t110 * t159 / 0.720D3
      t669 = FJET(XB1, XB2, s, -t213, -t293, 0.0D0, -t290, t298, t668)
      t671 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t673 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t674 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t703 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t704 = t703 - t674
      t711 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t724 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t461 * t671 + t673
     # + t462 * t674 / 0.2D1) + 0.180D3 * t6 * t31 * (t671 - t461 * t674
     #) + t6 * t42 * t674) * t46 / 0.1440D4 + (-0.90D2 * t6 * 0.31415926
     #53589793D1 * (t671 - t487 * t674) + 0.180D3 * t6 * t31 * t674) * t
     #46 * t159 / 0.720D3 + t162 * t704 * t46 * t165 / 0.8D1 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (t510 * t703 + t671 - t514 * t674 - 
     #t711) - 0.180D3 * t6 * t31 * t704) * t46 * t110 / 0.1440D4
      t725 = FJET(XB1, XB2, s, -t457, 0.0D0, t455, 0.0D0, 0.0D0, t724)
      t727 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t728 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t745 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t727 + t547 * t728
     #) - 0.180D3 * t6 * t31 * t728) * t46 * t159 / 0.720D3 + t162 * t72
     #8 * t46 * t165 / 0.8D1
      t746 = FJET(XB1, XB2, s, -t537, t539, t534, -t535, 0.0D0, t745)
      t748 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t750 = t748 * t46 * t165
      t753 = FJET(XB1, XB2, s, -t537, t590, t534, -t586, t298, -t162 * t
     #750 / 0.8D1)
      rrgq2qght9s2e0 = t210 * t209 + t286 * t285 + t324 * t323 + t453 * 
     #t452 + t530 * t529 + t566 * t565 - t596 * t3 * t598 * t593 / 0.8D1
     # + t648 * t647 + t669 * t668 + t725 * t724 + t746 * t745 - t753 * 
     #t3 * t598 * t750 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * 0.3141592653589793D1)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t22 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t27 = 0.3141592653589793D1 * lh
      t30 = 0.180D3 * t6 * t27 * t20
      t32 = 0.1D1 / x3
      t38 = log(0.4D1 * t9 * t13 * t15)
      t45 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t46 = t38 ** 2
      t53 = lh ** 2
      t55 = 0.3141592653589793D1 ** 2
      t57 = -0.180D3 * t53 + 0.30D2 * t55
      t63 = t6 * 0.3141592653589793D1
      t64 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t65 = -t20 + t64
      t67 = 0.1D1 / x2
      t71 = x2 * t9
      t74 = log(0.4D1 * t71 * t16)
      t76 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t77 = -0.1D1 + x2
      t81 = log(-0.4D1 * t71 * t16 * t77)
      t94 = 0.1D1 / x1
      t98 = x1 ** 2
      t99 = t98 * t13
      t103 = log(0.4D1 * t99 * t9 * t15)
      t116 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t19 * t20 - t22) - 
     #t30) * t32 / 0.1440D4 - (0.180D3 * (t22 - t38 * t20) * 0.314159265
     #3589793D1 * lh - 0.90D2 * (-t38 * t22 + t45 + t46 * t20 / 0.2D1) *
     # 0.3141592653589793D1 + t20 * 0.3141592653589793D1 * t57) * t3 * t
     #5 / 0.1440D4 - t63 * t65 * t32 * t67 / 0.16D2 + (-0.90D2 * t6 * 0.
     #3141592653589793D1 * (-t22 + t74 * t20 + t76 - t81 * t64) + 0.180D
     #3 * t6 * t27 * t65) * t67 / 0.1440D4 - t63 * t65 * t67 * t94 / 0.8
     #D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t103 * t20 - t22) - t
     #30) * t94 / 0.720D3 + t63 * t20 * t32 * t94 / 0.8D1
      t117 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t116)
      t119 = -0.1D1 + x1
      t120 = t2 * t119
      t121 = t2 * x1
      t122 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t127 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t132 = 0.1D1 / (-z - x1 + x1 * z)
      t134 = t119 ** 2
      t138 = log(-0.4D1 * t99 / t7 * t15 * t132 * t134)
      t154 = -t63 * t122 * t67 * t94 / 0.8D1 + (-0.90D2 * t6 * 0.3141592
     #653589793D1 * (t127 - t138 * t122) + 0.180D3 * t6 * t27 * t122) * 
     #t94 / 0.720D3 - t63 * t122 * t32 * t94 / 0.8D1
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t120, t121, 0.0D0, t154)
      t159 = t2 * x1 * x2 * t132
      t161 = t1 * x1
      t162 = t77 * s * t161
      t167 = s * t14 * x2 * x1 * t119 * t132
      t168 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t173 = FJET(XB1, XB2, s, 0.0D0, -t159, -t120, -t162, t167, t63 * t
     #168 * t67 * t94 / 0.8D1)
      t177 = t67 * t94
      t181 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t182 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t190 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t201 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t202 = t201 - t182
      t208 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t227 = 0.180D3 * t6 * t27 * t182
      t247 = -(0.180D3 * (t181 - t38 * t182) * 0.3141592653589793D1 * lh
     # - 0.90D2 * (t46 * t182 / 0.2D1 + t190 - t38 * t181) * 0.314159265
     #3589793D1 + t182 * 0.3141592653589793D1 * t57) * t3 * t5 / 0.1440D
     #4 - t63 * t202 * t32 * t67 / 0.16D2 + (-0.90D2 * t6 * 0.3141592653
     #589793D1 * (-t181 + t74 * t182 + t208 - t81 * t201) + 0.180D3 * t6
     # * t27 * t202) * t67 / 0.1440D4 + (-0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t181 + t19 * t182) - t227) * t32 / 0.1440D4 - t63 * t202 
     #* t67 * t94 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t18
     #1 + t103 * t182) - t227) * t94 / 0.720D3 + t63 * t182 * t32 * t94 
     #/ 0.8D1
      t248 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t247)
      t250 = t2 * x3
      t251 = -0.1D1 + x3
      t252 = t2 * t251
      t253 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t257 = log(-0.4D1 * t10 * t16 * t251)
      t258 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t274 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t280 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t253 - t257 * t258)
     # + 0.180D3 * t6 * t27 * t258) * t32 / 0.1440D4 - t63 * t258 * t32 
     #* t94 / 0.8D1 - t63 * (-t274 + t258) * t32 * t67 / 0.16D2
      t281 = FJET(XB1, XB2, s, t250, 0.0D0, -t252, 0.0D0, 0.0D0, t280)
      t283 = t251 * s
      t284 = t1 * t119
      t285 = t283 * t284
      t286 = t283 * t161
      t288 = x3 * s * t284
      t290 = t2 * x1 * x3
      t291 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t296 = FJET(XB1, XB2, s, t285, -t286, -t288, t290, 0.0D0, t63 * t2
     #91 * t32 * t94 / 0.8D1)
      t300 = t32 * t94
      t304 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t310 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t325 = -t63 * t304 * t67 * t94 / 0.8D1 + (-0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t138 * t304 + t310) + 0.180D3 * t6 * t27 * t304) *
     # t94 / 0.720D3 - t63 * t304 * t32 * t94 / 0.8D1
      t326 = FJET(XB1, XB2, s, -t120, t121, 0.0D0, 0.0D0, 0.0D0, t325)
      t328 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t333 = FJET(XB1, XB2, s, -t120, -t162, 0.0D0, -t159, t167, t63 * t
     #328 * t67 * t94 / 0.8D1)
      t340 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t341 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t357 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t363 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t340 - t257 * t341)
     # + 0.180D3 * t6 * t27 * t341) * t32 / 0.1440D4 - t63 * t341 * t32 
     #* t94 / 0.8D1 - t63 * (t341 - t357) * t32 * t67 / 0.16D2
      t364 = FJET(XB1, XB2, s, -t252, 0.0D0, t250, 0.0D0, 0.0D0, t363)
      t366 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t371 = FJET(XB1, XB2, s, -t288, t290, t285, -t286, 0.0D0, t63 * t3
     #66 * t32 * t94 / 0.8D1)
      rrgq2qght9s2em1 = t117 * t116 + t155 * t154 + t173 * t3 * t5 * 0.3
     #141592653589793D1 * t168 * t177 / 0.8D1 + t248 * t247 + t281 * t28
     #0 + t296 * t3 * t5 * 0.3141592653589793D1 * t291 * t300 / 0.8D1 + 
     #t326 * t325 + t333 * t3 * t5 * 0.3141592653589793D1 * t328 * t177 
     #/ 0.8D1 + t364 * t363 + t371 * t3 * t5 * 0.3141592653589793D1 * t3
     #66 * t300 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t16 = 0.1D1 / x2
      t20 = 0.1D1 / x1
      t26 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t27 = z ** 2
      t31 = Sin(x4 * 0.3141592653589793D1)
      t32 = t31 ** 2
      t34 = t1 ** 2
      t35 = t34 ** 2
      t38 = log(0.4D1 / t27 / z * t32 * t35)
      t47 = t6 * t8 * t9 / 0.16D2 - t6 * 0.3141592653589793D1 * (-t7 + t
     #13) * t16 / 0.16D2 + t6 * t8 * t20 / 0.8D1 - (0.180D3 * t8 * lh - 
     #0.90D2 * (t26 - t38 * t7) * 0.3141592653589793D1) * t3 * t5 / 0.14
     #40D4
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t47)
      t51 = t2 * (-0.1D1 + x1)
      t52 = t2 * x1
      t53 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, x
     #4)
      t55 = 0.3141592653589793D1 * t53 * t20
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t51, t52, 0.0D0, -t6 * t55 
     #/ 0.8D1)
      t63 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t64 = 0.3141592653589793D1 * t63
      t68 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t79 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t88 = t6 * t64 * t9 / 0.16D2 - t6 * 0.3141592653589793D1 * (t68 - 
     #t63) * t16 / 0.16D2 + t6 * t64 * t20 / 0.8D1 - (0.180D3 * t64 * lh
     # - 0.90D2 * (t79 - t38 * t63) * 0.3141592653589793D1) * t3 * t5 / 
     #0.1440D4
      t89 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t88)
      t91 = t2 * x3
      t93 = t2 * (-0.1D1 + x3)
      t94 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t96 = 0.3141592653589793D1 * t94 * t9
      t99 = FJET(XB1, XB2, s, t91, 0.0D0, -t93, 0.0D0, 0.0D0, -t6 * t96 
     #/ 0.16D2)
      t104 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t106 = 0.3141592653589793D1 * t104 * t9
      t109 = FJET(XB1, XB2, s, -t93, 0.0D0, t91, 0.0D0, 0.0D0, -t6 * t10
     #6 / 0.16D2)
      t114 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t116 = 0.3141592653589793D1 * t114 * t20
      t119 = FJET(XB1, XB2, s, -t51, t52, 0.0D0, 0.0D0, 0.0D0, -t6 * t11
     #6 / 0.8D1)
      rrgq2qght9s2em2 = t48 * t47 - t58 * t3 * t5 * t55 / 0.8D1 + t89 * 
     #t88 - t99 * t3 * t5 * t96 / 0.16D2 - t109 * t3 * t5 * t106 / 0.16D
     #2 - t119 * t3 * t5 * t116 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.16D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.16D2)
      rrgq2qght9s2em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2

      end function



      doubleprecision function rrgq2qght9s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6
      rrgq2qght9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh91J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x3
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t17 = x3 * (0.1D1 - x2)
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t4)
      t28 = t17 * t14 + x2 * t4 - 0.2D1 * t21 * t25
      t30 = t2 ** 2
      t31 = t4 ** 2
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t46 = s * t1
      rrgq2qgh91J1 = 0.4D1 / 0.9D1 * t1 * t2 * t4 * wd * (-t11 * x1 * t1
     #5 * t28 * t30 * t31 - t11 * t35 * x1 / t37 / t14 * t41 * t28) / (s
     # - t46 * x1 * t15 * t28 - t46 * t2 * x3) / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrgq2qgh91J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t17 = x3 * (0.1D1 - x2)
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t28 = t17 * t14 + x2 * t5 - 0.2D1 * t21 * t25
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      rrgq2qgh91J2 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - 0.2D1 * t11 * t35 / t37 * t41 * t3 * t5 + t34)) / (s - t
     #2 * x1 * t15 * t28 - t2 * t3 * x3) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh91J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t27 = 0.2D1 * t21 * t25
      t28 = t17 * t14 + x2 * t5 - t27
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      t49 = 0.1D1 / t37
      t54 = 0.2D1 * t11 * t35 * t49 * t41 * t3 * t5
      t63 = s - t2 * x1 * t15 * t28 - t2 * t3 * x3
      t64 = t63 * t7
      t71 = t5 * t16 * t14 + x2 * x3 + t27
      t77 = t71 ** 2
      rrgq2qgh91J3 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - t54 + t34) - t4 * t6 * (-t54 + t44 + t34 + t64 * t9 * t3
     # * t5 * x1 * t15 * t71 + t64 * t9 * t35 * t49 * t77)) / t63 / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh91J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t27 = 0.2D1 * t21 * t25
      t28 = t17 * t14 + x2 * t5 - t27
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      t49 = 0.1D1 / t37
      t54 = 0.2D1 * t11 * t35 * t49 * t41 * t3 * t5
      t63 = s - t2 * x1 * t15 * t28 - t2 * t3 * x3
      t64 = t63 * t7
      t71 = t5 * t16 * t14 + x2 * x3 + t27
      t77 = t71 ** 2
      rrgq2qgh91J4 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - t54 + t34) - t4 * t6 * (-t54 + t44 + t34 + t64 * t9 * t3
     # * t5 * x1 * t15 * t71 + t64 * t9 * t35 * t49 * t77)) / t63 / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh91J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t27 = 0.2D1 * t21 * t25
      t28 = t17 * t14 + x2 * t5 - t27
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      t49 = 0.1D1 / t37
      t54 = 0.2D1 * t11 * t35 * t49 * t41 * t3 * t5
      t63 = s - t2 * x1 * t15 * t28 - t2 * t3 * x3
      t64 = t63 * t7
      t71 = t5 * t16 * t14 + x2 * x3 + t27
      t77 = t71 ** 2
      rrgq2qgh91J5 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - t54 + t34) - t4 * t6 * (-t54 + t44 + t34 + t64 * t9 * t3
     # * t5 * x1 * t15 * t71 + t64 * t9 * t35 * t49 * t77)) / t63 / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh91J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t12 = x1 ** 2
      t15 = z + t1 * x1
      t16 = t15 ** 2
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t21 * t15 * x2 * t5)
      t31 = 0.2D1 * t25 * t29
      t32 = t21 * t15 + x2 * t5 - t31
      t33 = t32 ** 2
      t36 = t11 * t12 * x1 / t16 / t15 * t33 * t32
      t38 = 0.1D1 / t16
      t43 = 0.2D1 * t11 * t12 * t38 * t33 * t3 * t5
      t45 = 0.1D1 / t15
      t47 = t3 ** 2
      t48 = t5 ** 2
      t51 = t11 * x1 * t45 * t32 * t47 * t48
      t60 = s - t2 * x1 * t45 * t32 - t2 * t3 * x3
      t61 = t60 * t7
      t68 = t5 * t20 * t15 + x2 * x3 + t31
      t74 = t68 ** 2
      rrgq2qgh91J6 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t36 - t43 + t51) - t4
     # * t6 * (-t43 + t36 + t51 + t61 * t9 * t3 * t5 * x1 * t45 * t68 + 
     #t61 * t9 * t12 * t38 * t74)) / t60 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh91J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x3
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t12 = x1 ** 2
      t15 = z + t1 * x1
      t16 = t15 ** 2
      t17 = 0.1D1 / t16
      t18 = 0.1D1 - x2
      t19 = x3 * t18
      t23 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(t19 * t15 * x2 * t4)
      t29 = 0.2D1 * t23 * t27
      t30 = t19 * t15 + x2 * t4 - t29
      t31 = t30 ** 2
      t45 = 0.1D1 / t15
      t47 = t2 ** 2
      t48 = t4 ** 2
      t52 = s * t1
      t58 = s - t52 * x1 * t45 * t30 - t52 * t2 * x3
      t59 = t58 * t7
      t66 = t4 * t18 * t15 + x2 * x3 + t29
      t72 = t66 ** 2
      rrgq2qgh91J7 = 0.4D1 / 0.9D1 * t1 * t2 * t4 * wd * (-0.2D1 * t11 *
     # t12 * t17 * t31 * t2 * t4 + t11 * t12 * x1 / t16 / t15 * t31 * t3
     #0 + t11 * x1 * t45 * t30 * t47 * t48 + t59 * t9 * t2 * t4 * x1 * t
     #45 * t66 + t59 * t9 * t12 * t17 * t72) / t58 / z / 0.3141592653589
     #793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh92J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x3
      t7 = s * t1
      t9 = z + t1 * x1
      t13 = x3 * (0.1D1 - x2)
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t13 * t9 * x2 * t4)
      t24 = t9 * t13 + x2 * t4 - 0.2D1 * t17 * t21
      t25 = x1 / t9 * t24
      t29 = s - t7 * t25 - t7 * t2 * x3
      t30 = s ** 2
      t31 = t29 * t30
      t32 = t1 ** 2
      t33 = t2 ** 2
      t35 = t4 ** 2
      t44 = x1 ** 2
      t45 = t9 ** 2
      t48 = t24 ** 2
      rrgq2qgh92J1 = 0.4D1 / 0.9D1 * t1 * t2 * t4 * wd * (0.9D1 * t31 * 
     #t32 * t33 * t35 - 0.18D2 * t31 * t1 * t25 + 0.9D1 * t31 + 0.9D1 * 
     #t31 * t32 * t44 / t45 * t48) / t29 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh92J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t32 = t3 ** 2
      t34 = t5 ** 2
      t37 = 0.9D1 * t30 * t31 * t32 * t34
      t39 = t30 * t1 * t24
      t41 = 0.9D1 * t30
      t43 = x1 ** 2
      t44 = t8 ** 2
      t47 = t23 ** 2
      t50 = 0.9D1 * t30 * t31 * t43 / t44 * t47
      rrgq2qgh92J2 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t37 - 0.18D2 * t39 + 
     #t41 + t50) - t4 * t6 * (-t41 - t37 + 0.27D2 * t39 - t50 + 0.18D2 *
     # t30 * t1 * t3 * t5 - 0.27D2 * t30 * t31 * t3 * t5 * x1 * t9 * t23
     #)) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh92J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t32 = t3 ** 2
      t34 = t5 ** 2
      t37 = 0.9D1 * t30 * t31 * t32 * t34
      t39 = t30 * t1 * t24
      t41 = 0.9D1 * t30
      t43 = x1 ** 2
      t44 = t8 ** 2
      t47 = t23 ** 2
      t50 = 0.9D1 * t30 * t31 * t43 / t44 * t47
      rrgq2qgh92J3 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t37 - 0.18D2 * t39 + 
     #t41 + t50) - t4 * t6 * (-t41 - t37 + 0.27D2 * t39 - t50 + 0.18D2 *
     # t30 * t1 * t3 * t5 - 0.27D2 * t30 * t31 * t3 * t5 * x1 * t9 * t23
     #)) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh92J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t32 = t3 ** 2
      t34 = t5 ** 2
      t37 = 0.9D1 * t30 * t31 * t32 * t34
      t39 = t30 * t1 * t24
      t41 = 0.9D1 * t30
      t43 = x1 ** 2
      t44 = t8 ** 2
      t47 = t23 ** 2
      t50 = 0.9D1 * t30 * t31 * t43 / t44 * t47
      rrgq2qgh92J4 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t37 - 0.18D2 * t39 + 
     #t41 + t50) - t4 * t6 * (-t41 - t37 + 0.27D2 * t39 - t50 + 0.18D2 *
     # t30 * t1 * t3 * t5 - 0.27D2 * t30 * t31 * t3 * t5 * x1 * t9 * t23
     #)) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh92J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t32 = t3 ** 2
      t34 = t5 ** 2
      t37 = 0.9D1 * t30 * t31 * t32 * t34
      t39 = t30 * t1 * t24
      t41 = 0.9D1 * t30
      t43 = x1 ** 2
      t44 = t8 ** 2
      t47 = t23 ** 2
      t50 = 0.9D1 * t30 * t31 * t43 / t44 * t47
      rrgq2qgh92J5 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t37 - 0.18D2 * t39 + 
     #t41 + t50) - t4 * t6 * (-t41 - t37 + 0.27D2 * t39 - t50 + 0.18D2 *
     # t30 * t1 * t3 * t5 - 0.27D2 * t30 * t31 * t3 * t5 * x1 * t9 * t23
     #)) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh92J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t3 = t1 * t2
      t4 = 0.1D1 - x3
      t7 = s * t1
      t9 = z + t1 * x1
      t10 = 0.1D1 / t9
      t13 = x3 * (0.1D1 - x2)
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t13 * t9 * x2 * t4)
      t24 = t9 * t13 + x2 * t4 - 0.2D1 * t17 * t21
      t25 = x1 * t10 * t24
      t29 = s - t7 * t25 - t7 * t2 * x3
      t30 = s ** 2
      t31 = t29 * t30
      t33 = t1 ** 2
      t34 = t2 ** 2
      t36 = t4 ** 2
      t44 = x1 ** 2
      t45 = t9 ** 2
      t48 = t24 ** 2
      rrgq2qgh92J6 = 0.4D1 / 0.9D1 * t3 * t4 * wd * (-0.9D1 * t31 - 0.9D
     #1 * t31 * t33 * t34 * t36 + 0.27D2 * t31 * t1 * t25 - 0.9D1 * t31 
     #* t33 * t44 / t45 * t48 + 0.18D2 * t31 * t3 * t4 - 0.27D2 * t31 * 
     #t33 * t2 * t4 * x1 * t10 * t24) / t29 / z / 0.3141592653589793D1

      end function
  
 