  
      subroutine ggbbH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision ggbbH51J1  
      doubleprecision ggbbH51J2  
      doubleprecision ggbbH51J3  
      doubleprecision ggbbH5n1e1  
      doubleprecision ggbbH5n1e0  
      doubleprecision ggbbH5n1em1  
      doubleprecision ggbbH5n1em2  
      doubleprecision ggbbH5n1em3  
      doubleprecision ggbbH5n1em4  
      doubleprecision ggbbH5n2e1  
      doubleprecision ggbbH5n2e0  
      doubleprecision ggbbH5n2em1  
      doubleprecision ggbbH5n2em2  
      doubleprecision ggbbH5n2em3  
      doubleprecision ggbbH5n2em4  
      doubleprecision ggbbH5n3e1  
      doubleprecision ggbbH5n3e0  
      doubleprecision ggbbH5n3em1  
      doubleprecision ggbbH5n3em2  
      doubleprecision ggbbH5n3em3  
      doubleprecision ggbbH5n3em4  
      doubleprecision ggbbH5n4e1  
      doubleprecision ggbbH5n4e0  
      doubleprecision ggbbH5n4em1  
      doubleprecision ggbbH5n4em2  
      doubleprecision ggbbH5n4em3  
      doubleprecision ggbbH5n4em4  
      doubleprecision ggbbH5n5e1  
      doubleprecision ggbbH5n5e0  
      doubleprecision ggbbH5n5em1  
      doubleprecision ggbbH5n5em2  
      doubleprecision ggbbH5n5em3  
      doubleprecision ggbbH5n5em4  
      doubleprecision ggbbH5n6e1  
      doubleprecision ggbbH5n6e0  
      doubleprecision ggbbH5n6em1  
      doubleprecision ggbbH5n6em2  
      doubleprecision ggbbH5n6em3  
      doubleprecision ggbbH5n6em4  
      doubleprecision ggbbH5n7e1  
      doubleprecision ggbbH5n7e0  
      doubleprecision ggbbH5n7em1  
      doubleprecision ggbbH5n7em2  
      doubleprecision ggbbH5n7em3  
      doubleprecision ggbbH5n7em4  
      doubleprecision ggbbH5n8e1  
      doubleprecision ggbbH5n8e0  
      doubleprecision ggbbH5n8em1  
      doubleprecision ggbbH5n8em2  
      doubleprecision ggbbH5n8em3  
      doubleprecision ggbbH5n8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=ggbbH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH5n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=ggbbH5n3e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=ggbbH5n4e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=ggbbH5n5e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=ggbbH5n6e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=ggbbH5n7e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=ggbbH5n8e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=ggbbH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH5n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=ggbbH5n3e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=ggbbH5n4e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=ggbbH5n5e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=ggbbH5n6e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=ggbbH5n7e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=ggbbH5n8e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=ggbbH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH5n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=ggbbH5n3em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=ggbbH5n4em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=ggbbH5n5em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=ggbbH5n6em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=ggbbH5n7em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=ggbbH5n8em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=ggbbH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH5n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=ggbbH5n3em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=ggbbH5n4em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=ggbbH5n5em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=ggbbH5n6em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=ggbbH5n7em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=ggbbH5n8em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=ggbbH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH5n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=ggbbH5n3em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=ggbbH5n4em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=ggbbH5n5em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=ggbbH5n6em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=ggbbH5n7em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=ggbbH5n8em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=ggbbH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH5n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=ggbbH5n3em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=ggbbH5n4em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=ggbbH5n5em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=ggbbH5n6em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=ggbbH5n7em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=ggbbH5n8em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function ggbbH5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t20 = t17 ** 2
      t21 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t28 - t30
      t32 = t31 * t5
      t46 = 0.120D3 * t27 * lh
      t48 = 0.60D2 * lh * t29
      t49 = -0.2884936567583026D3 - t46 + t48
      t50 = t49 * t5
      t51 = t50 * t21
      t53 = 0.1D1 / x1
      t56 = 0.1D1 - x4
      t57 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t56)
      t58 = t14 * x4
      t59 = -t56
      t60 = t58 * t59
      t63 = log(-0.4D1 * t12 * t60)
      t64 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t56)
      t68 = log(0.4D1 * t12 * t58)
      t73 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t56)
      t75 = t63 ** 2
      t79 = t68 ** 2
      t85 = -t64 + t21
      t86 = t32 * t85
      t89 = 0.1D1 / x4
      t92 = x3 * t8
      t93 = t11 * t14
      t94 = t93 * x4
      t97 = log(0.4D1 * t92 * t94)
      t99 = t92 * t11
      t102 = log(-0.4D1 * t99 * t60)
      t110 = 0.1D1 / x3
      t112 = t53 * t89
      t115 = t92 * t93
      t117 = log(0.4D1 * t115)
      t123 = t117 ** 2
      t135 = log(0.4D1 * t93)
      t138 = t135 ** 2
      t147 = t138 * t135
      t153 = t29 ** 2
      t154 = t27 ** 2
      t160 = t138 ** 2
      t170 = x4 * t59
      t173 = log(-0.4D1 * t93 * t170)
      t175 = t173 ** 2
      t179 = log(0.4D1 * t94)
      t181 = t179 ** 2
      t211 = x3 * t11
      t212 = t211 * t14
      t214 = log(0.4D1 * t212)
      t216 = t214 ** 2
      t239 = log(-0.4D1 * t211 * t60)
      t243 = log(0.4D1 * t211 * t58)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t263 = -(-0.180D3 * t6 * (t7 - t17 * t18 + t20 * t21 / 0.2D1) + t3
     #2 * (t18 - t17 * t21) + 0.90D2 * t5 * (-t17 * t7 + t20 * t18 / 0.2
     #D1 - t20 * t17 * t21 / 0.6D1) + t51) * t53 / 0.2880D4 - (-0.180D3 
     #* t6 * (-t57 + t63 * t64 + t18 - t68 * t21) + 0.90D2 * t5 * (-t73 
     #+ t63 * t57 - t75 * t64 / 0.2D1 + t7 - t68 * t18 + t79 * t21 / 0.2
     #D1) + t86) * t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (t18 - t97 * t2
     #1 - t57 + t102 * t64) - 0.180D3 * t6 * t85) * t110 * t112 / 0.2880
     #D4 + (-0.180D3 * t6 * (-t18 + t117 * t21) + 0.90D2 * t5 * (-t7 + t
     #117 * t18 - t123 * t21 / 0.2D1) - t32 * t21) * t110 * t53 / 0.2880
     #D4 - (t28 - t30 + 0.180D3 * t135 * lh + 0.45D2 * t138) * t5 * t7 /
     # 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t135 * t31 - 0.90
     #D2 * t138 * lh - 0.15D2 * t147) * t5 * t18 / 0.5760D4 - (t153 + 0.
     #60D2 * t154 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29 - t13
     #5 * t49 + 0.15D2 / 0.4D1 * t160 + t138 * t31 / 0.2D1 + 0.30D2 * t1
     #47 * lh) * t5 * t21 / 0.5760D4 + (-0.180D3 * t6 * (t73 - t173 * t5
     #7 + t175 * t64 / 0.2D1 - t7 + t179 * t18 - t181 * t21 / 0.2D1) + t
     #32 * (-t18 + t179 * t21 + t57 - t173 * t64) + 0.90D2 * t5 * (-t173
     # * t73 + t175 * t57 / 0.2D1 - t175 * t173 * t64 / 0.6D1 + t179 * t
     #7 - t181 * t18 / 0.2D1 + t181 * t179 * t21 / 0.6D1) - t50 * t85) *
     # t89 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t214 * t18 + t216 * t21 /
     # 0.2D1) + t32 * (t18 - t214 * t21) + 0.90D2 * t5 * (-t214 * t7 + t
     #216 * t18 / 0.2D1 - t216 * t214 * t21 / 0.6D1) + t51) * t110 / 0.5
     #760D4 - (-0.180D3 * t6 * (-t57 + t239 * t64 + t18 - t243 * t21) + 
     #0.90D2 * t5 * (-t73 + t239 * t57 - t249 * t64 / 0.2D1 + t7 - t243 
     #* t18 + t253 * t21 / 0.2D1) + t86) * t110 * t89 / 0.5760D4
      t264 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t263)
      t266 = 0.1D1 - x1
      t267 = 0.1D1 - x3
      t268 = KAPPA2(t266, x2, t267, 0.10D1, z)
      t269 = s * t268
      t270 = -t266
      t271 = t1 * t270
      t272 = -t267
      t273 = t271 * t272
      t275 = t271 * x3
      t277 = t1 * x1
      t279 = t268 ** 2
      t281 = t1 ** 2
      t283 = t270 * x1
      t287 = 0.1D1 / (-0.2D1 + t268)
      t288 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.10D1)
      t289 = t287 * t288
      t290 = t270 ** 2
      t291 = t290 * t272
      t292 = t279 ** 2
      t297 = log(-0.4D1 * t115 * t291 * x4 * t292)
      t299 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.10D1)
      t304 = t287 * t299
      t315 = log(-0.4D1 * t99 * t14 * t290 * t272 * t292)
      t316 = t315 * t287
      t321 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.10D1)
      t324 = t315 ** 2
      t336 = -(0.90D2 * t5 * (-t289 + t297 * t287 * t299) + 0.180D3 * t6
     # * t304) * t110 * t112 / 0.2880D4 + (-0.180D3 * t6 * (t289 - t316 
     #* t299) + 0.90D2 * t5 * (t287 * t321 - t316 * t288 + t324 * t287 *
     # t299 / 0.2D1) + t32 * t304) * t110 * t53 / 0.2880D4
      t337 = FJET(XB1, XB2, s, t269 * t273, -t269 * t275, t269 * t277, 0
     #.0D0, -s * t279 * t281 * t283 * x3, t336)
      t339 = KAPPA2(t266, x2, t267, t56, z)
      t340 = s * t339
      t343 = t277 * t59
      t345 = t277 * x4
      t347 = t339 ** 2
      t352 = cos(t9)
      t355 = sqrt(x3 * t272 * t170)
      t362 = 0.1D1 / (-0.2D1 + t339)
      t363 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, t267, t56)
      t365 = t347 ** 2
      t370 = log(0.4D1 * t115 * t291 * t170 * t365)
      t372 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, t267, t56)
      t380 = 0.90D2 * t5 * (t362 * t363 - t370 * t362 * t372) - 0.180D3 
     #* t6 * t362 * t372
      t384 = FJET(XB1, XB2, s, t340 * t273, -t340 * t275, -t340 * t343, 
     #t340 * t345, s * t347 * t281 * t283 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t352 * t355), -t380 * t110 * t112 / 0.2880D4)
      t392 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.10D1)
      t393 = t8 * t290
      t396 = log(0.4D1 * t93 * t393)
      t397 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.10D1)
      t399 = t396 ** 2
      t400 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.10D1)
      t421 = t393 * x4
      t424 = log(0.4D1 * t93 * t421)
      t430 = t424 ** 2
      t436 = t32 * t400
      t442 = log(0.4D1 * t212 * t421)
      t456 = log(0.4D1 * t211 * t14 * t8 * t290)
      t462 = t456 ** 2
      t472 = -(0.180D3 * t6 * (t392 - t396 * t397 + t399 * t400 / 0.2D1)
     # - t32 * (t397 - t396 * t400) - 0.90D2 * t5 * (-t396 * t392 + t399
     # * t397 / 0.2D1 - t399 * t396 * t400 / 0.6D1) - t50 * t400) * t53 
     #/ 0.2880D4 - (-0.180D3 * t6 * (-t397 + t424 * t400) + 0.90D2 * t5 
     #* (-t392 + t424 * t397 - t430 * t400 / 0.2D1) - t436) * t53 * t89 
     #/ 0.2880D4 - (0.90D2 * t5 * (-t397 + t442 * t400) + 0.180D3 * t6 *
     # t400) * t110 * t112 / 0.2880D4 + (-0.180D3 * t6 * (t397 - t456 * 
     #t400) + 0.90D2 * t5 * (t392 - t456 * t397 + t462 * t400 / 0.2D1) +
     # t436) * t110 * t53 / 0.2880D4
      t473 = FJET(XB1, XB2, s, -t2 * t270, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t472)
      t477 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.10D1)
      t478 = t14 * t272
      t479 = t478 * x4
      t482 = log(-0.4D1 * t99 * t479)
      t483 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.10D1)
      t485 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, t56)
      t490 = log(0.4D1 * t99 * t58 * t59 * t272)
      t491 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, t56)
      t496 = -t483 + t491
      t506 = log(-0.4D1 * t92 * t93 * t272)
      t511 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.10D1)
      t513 = t506 ** 2
      t526 = log(-0.4D1 * t211 * t478)
      t528 = t526 ** 2
      t553 = log(0.4D1 * t212 * t170 * t272)
      t557 = log(-0.4D1 * t211 * t479)
      t562 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, t56)
      t564 = t553 ** 2
      t568 = t557 ** 2
      t579 = -(0.90D2 * t5 * (-t477 + t482 * t483 + t485 - t490 * t491) 
     #- 0.180D3 * t6 * t496) * t110 * t112 / 0.2880D4 + (-0.180D3 * t6 *
     # (t477 - t506 * t483) + 0.90D2 * t5 * (t511 - t506 * t477 + t513 *
     # t483 / 0.2D1) + t32 * t483) * t110 * t53 / 0.2880D4 - (0.180D3 * 
     #t6 * (t511 - t526 * t477 + t528 * t483 / 0.2D1) - t32 * (t477 - t5
     #26 * t483) - 0.90D2 * t5 * (-t526 * t511 + t528 * t477 / 0.2D1 - t
     #528 * t526 * t483 / 0.6D1) - t50 * t483) * t110 / 0.5760D4 - (-0.1
     #80D3 * t6 * (t485 - t553 * t491 - t477 + t557 * t483) + 0.90D2 * t
     #5 * (t562 - t553 * t485 + t564 * t491 / 0.2D1 - t511 + t557 * t477
     # - t568 * t483 / 0.2D1) + t32 * t496) * t110 * t89 / 0.5760D4
      t580 = FJET(XB1, XB2, s, -t2 * t272, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t579)
      t582 = KAPPA2(t266, x2, 0.10D1, t56, z)
      t583 = s * t582
      t587 = t582 ** 2
      t593 = 0.1D1 / (-0.2D1 + t582)
      t594 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, t56)
      t595 = t593 * t594
      t597 = t587 ** 2
      t599 = t290 * x4 * t59 * t597
      t602 = log(-0.4D1 * t15 * t599)
      t603 = t602 * t593
      t604 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, t56)
      t609 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, t56)
      t612 = t602 ** 2
      t619 = t593 * t604
      t626 = log(-0.4D1 * t115 * t599)
      t638 = -(0.180D3 * t6 * (t595 - t603 * t604) - 0.90D2 * t5 * (t593
     # * t609 - t603 * t594 + t612 * t593 * t604 / 0.2D1) - t32 * t619) 
     #* t53 * t89 / 0.2880D4 - (-0.90D2 * t5 * (t595 - t626 * t593 * t60
     #4) + 0.180D3 * t6 * t619) * t110 * t112 / 0.2880D4
      t639 = FJET(XB1, XB2, s, -t583 * t271, 0.0D0, -t583 * t343, t583 *
     # t345, -s * t587 * t281 * t283 * x4, t638)
      ggbbH5n1e1 = t264 * t263 + t337 * t336 - t384 * t380 * t110 * t53 
     #* t89 / 0.2880D4 + t473 * t472 + t580 * t579 + t639 * t638

      end function



      doubleprecision function ggbbH5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 - x4
      t7 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t6)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * x4
      t16 = -t6
      t17 = t15 * t16
      t20 = log(-0.4D1 * t12 * t17)
      t21 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t6)
      t23 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t26 = log(0.4D1 * t12 * t15)
      t27 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t32 = lh * t5
      t33 = -t21 + t27
      t35 = 0.180D3 * t32 * t33
      t37 = 0.1D1 / x1
      t39 = 0.1D1 / x4
      t43 = 0.1D1 / x3
      t45 = t43 * t37 * t39
      t48 = x3 * t8
      t49 = t11 * t14
      t52 = log(0.4D1 * t48 * t49)
      t63 = t12 * t14
      t65 = log(0.4D1 * t63)
      t70 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t27
      t90 = log(0.4D1 * t49 * x4)
      t92 = x4 * t16
      t95 = log(-0.4D1 * t49 * t92)
      t100 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t6)
      t102 = t95 ** 2
      t106 = t90 ** 2
      t117 = x3 * t11
      t120 = log(-0.4D1 * t117 * t17)
      t124 = log(0.4D1 * t117 * t15)
      t133 = t117 * t14
      t135 = log(0.4D1 * t133)
      t141 = t135 ** 2
      t152 = log(0.4D1 * t49)
      t160 = t152 ** 2
      t179 = -(0.90D2 * t5 * (-t7 + t20 * t21 + t23 - t26 * t27) - t35) 
     #* t37 * t39 / 0.2880D4 - t5 * t33 * t45 / 0.32D2 + (0.90D2 * t5 * 
     #(-t23 + t52 * t27) + 0.180D3 * t32 * t27) * t43 * t37 / 0.2880D4 -
     # (-0.180D3 * t32 * (t23 - t65 * t27) + 0.90D2 * t5 * (t70 - t65 * 
     #t23 + t72 * t27 / 0.2D1) + t84) * t37 / 0.2880D4 + (-0.180D3 * t32
     # * (-t23 + t90 * t27 + t7 - t95 * t21) + 0.90D2 * t5 * (t100 - t95
     # * t7 + t102 * t21 / 0.2D1 - t70 + t90 * t23 - t106 * t27 / 0.2D1)
     # - t83 * t33) * t39 / 0.5760D4 - (0.90D2 * t5 * (-t7 + t120 * t21 
     #+ t23 - t124 * t27) - t35) * t43 * t39 / 0.5760D4 - (-0.180D3 * t3
     #2 * (t23 - t135 * t27) + 0.90D2 * t5 * (t70 - t135 * t23 + t141 * 
     #t27 / 0.2D1) + t84) * t43 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t
     #152) * t5 * t70 / 0.5760D4 - (t79 - t81 + 0.180D3 * t152 * lh + 0.
     #45D2 * t160) * t5 * t23 / 0.5760D4 - (-0.2884936567583026D3 - 0.12
     #0D3 * t78 * lh + 0.60D2 * lh * t80 - t152 * t82 - 0.90D2 * t160 * 
     #lh - 0.15D2 * t160 * t152) * t5 * t27 / 0.5760D4
      t180 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = 0.1D1 - x1
      t183 = 0.1D1 - x3
      t184 = KAPPA2(t182, x2, t183, 0.10D1, z)
      t185 = s * t184
      t186 = -t182
      t187 = t1 * t186
      t188 = -t183
      t189 = t187 * t188
      t191 = t187 * x3
      t193 = t1 * x1
      t195 = t184 ** 2
      t197 = t1 ** 2
      t199 = t186 * x1
      t203 = 0.1D1 / (-0.2D1 + t184)
      t205 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t182, x2, t183, 0.10D1)
      t209 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t182, x2, t183, 0.10D1)
      t212 = t186 ** 2
      t214 = t195 ** 2
      t219 = log(-0.4D1 * t48 * t11 * t14 * t212 * t188 * t214)
      t232 = t5 * t203 * t205 * t45 / 0.32D2 + (0.90D2 * t5 * (t203 * t2
     #09 - t219 * t203 * t205) - 0.180D3 * t32 * t203 * t205) * t43 * t3
     #7 / 0.2880D4
      t233 = FJET(XB1, XB2, s, t185 * t189, -t185 * t191, t185 * t193, 0
     #.0D0, -s * t195 * t197 * t199 * x3, t232)
      t235 = KAPPA2(t182, x2, t183, t6, z)
      t236 = s * t235
      t239 = t193 * t16
      t241 = t193 * x4
      t243 = t235 ** 2
      t248 = cos(t9)
      t251 = sqrt(x3 * t188 * t92)
      t258 = 0.1D1 / (-0.2D1 + t235)
      t260 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t182, x2, t183, t6)
      t264 = FJET(XB1, XB2, s, t236 * t189, -t236 * t191, -t236 * t239, 
     #t236 * t241, s * t243 * t197 * t199 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t248 * t251), -t5 * t258 * t260 * t45 / 0.32D2)
      t274 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t182, x2, 0.10D1, 0.10D1)
      t275 = t8 * t212
      t279 = log(0.4D1 * t49 * t275 * x4)
      t280 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t182, x2, 0.10D1, 0.10D1)
      t286 = 0.180D3 * t32 * t280
      t298 = log(0.4D1 * t117 * t14 * t8 * t212)
      t309 = log(0.4D1 * t49 * t275)
      t314 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t182, x2, 0.10D1, 0.10D1)
      t316 = t309 ** 2
      t326 = -(0.90D2 * t5 * (-t274 + t279 * t280) + t286) * t37 * t39 /
     # 0.2880D4 + t5 * t280 * t45 / 0.32D2 + (0.90D2 * t5 * (t274 - t298
     # * t280) - t286) * t43 * t37 / 0.2880D4 - (0.180D3 * t32 * (t274 -
     # t309 * t280) - 0.90D2 * t5 * (t314 - t309 * t274 + t316 * t280 / 
     #0.2D1) - t83 * t280) * t37 / 0.2880D4
      t327 = FJET(XB1, XB2, s, -t2 * t186, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t326)
      t331 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t183, 0.10D1)
      t332 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t183, t6)
      t333 = -t331 + t332
      t337 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t183, 0.10D1)
      t341 = log(-0.4D1 * t48 * t49 * t188)
      t352 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t183, t6)
      t356 = log(0.4D1 * t133 * t92 * t188)
      t358 = t14 * t188
      t362 = log(-0.4D1 * t117 * t358 * x4)
      t375 = log(-0.4D1 * t117 * t358)
      t380 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t183, 0.10D1)
      t382 = t375 ** 2
      t392 = -t5 * t333 * t45 / 0.32D2 + (0.90D2 * t5 * (t337 - t341 * t
     #331) - 0.180D3 * t32 * t331) * t43 * t37 / 0.2880D4 - (0.90D2 * t5
     # * (t352 - t356 * t332 - t337 + t362 * t331) - 0.180D3 * t32 * t33
     #3) * t43 * t39 / 0.5760D4 - (0.180D3 * t32 * (t337 - t375 * t331) 
     #- 0.90D2 * t5 * (t380 - t375 * t337 + t382 * t331 / 0.2D1) - t83 *
     # t331) * t43 / 0.5760D4
      t393 = FJET(XB1, XB2, s, -t2 * t188, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t392)
      t395 = KAPPA2(t182, x2, 0.10D1, t6, z)
      t396 = s * t395
      t400 = t395 ** 2
      t406 = 0.1D1 / (-0.2D1 + t395)
      t407 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t182, x2, 0.10D1, t6)
      t410 = t400 ** 2
      t415 = log(-0.4D1 * t63 * t212 * x4 * t16 * t410)
      t417 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t182, x2, 0.10D1, t6)
      t433 = -(-0.90D2 * t5 * (t406 * t407 - t415 * t406 * t417) + 0.180
     #D3 * t32 * t406 * t417) * t37 * t39 / 0.2880D4 + t5 * t406 * t417 
     #* t45 / 0.32D2
      t434 = FJET(XB1, XB2, s, -t396 * t187, 0.0D0, -t396 * t239, t396 *
     # t241, -s * t400 * t197 * t199 * x4, t433)
      ggbbH5n1e0 = t180 * t179 + t233 * t232 - t264 * t5 * t258 * t260 *
     # t43 * t37 * t39 / 0.32D2 + t327 * t326 + t393 * t392 + t434 * t43
     #3

      end function



      doubleprecision function ggbbH5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t7 * t10 * t13)
      t17 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = 0.1D1 - x4
      t30 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t29)
      t31 = -t30 + t17
      t32 = t31 * t5
      t33 = 0.1D1 / x4
      t34 = t26 * t33
      t38 = 0.1D1 / x3
      t39 = t38 * t26
      t42 = t38 * t33
      t45 = x3 * t10
      t48 = log(0.4D1 * t45 * t13)
      t56 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t60 = t10 * t13
      t62 = log(0.4D1 * t60)
      t68 = lh ** 2
      t70 = 0.3141592653589793D1 ** 2
      t74 = t62 ** 2
      t82 = log(0.4D1 * t60 * x4)
      t84 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t29)
      t85 = -t29
      t89 = log(-0.4D1 * t60 * x4 * t85)
      t100 = -(0.90D2 * t5 * (t6 - t16 * t17) - t24) * t26 / 0.2880D4 - 
     #t32 * t34 / 0.32D2 - t5 * t17 * t39 / 0.32D2 - t32 * t42 / 0.64D2 
     #- (0.90D2 * t5 * (t6 - t48 * t17) - t24) * t38 / 0.5760D4 - t5 * t
     #56 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t62) * t5 * t6 / 0.5760D4 
     #- (0.180D3 * t68 - 0.30D2 * t70 + 0.180D3 * t62 * lh + 0.45D2 * t7
     #4) * t5 * t17 / 0.5760D4 + (0.90D2 * t5 * (-t6 + t82 * t17 + t84 -
     # t89 * t30) + 0.180D3 * t22 * t31) * t33 / 0.5760D4
      t101 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t100)
      t103 = 0.1D1 - x1
      t104 = 0.1D1 - x3
      t105 = KAPPA2(t103, x2, t104, 0.10D1, z)
      t106 = s * t105
      t107 = -t103
      t108 = t1 * t107
      t109 = -t104
      t114 = t1 * x1
      t116 = t105 ** 2
      t118 = t1 ** 2
      t120 = t107 * x1
      t124 = 0.1D1 / (-0.2D1 + t105)
      t126 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t103, x2, t104, 0.10D1)
      t128 = t126 * t38 * t26
      t131 = FJET(XB1, XB2, s, t106 * t108 * t109, -t106 * t108 * x3, t1
     #06 * t114, 0.0D0, -s * t116 * t118 * t120 * x3, t5 * t124 * t128 /
     # 0.32D2)
      t138 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, 0.10D1)
      t139 = t107 ** 2
      t143 = log(0.4D1 * t60 * t7 * t139)
      t144 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, 0.10D1)
      t154 = t5 * t144
      t159 = -(-0.90D2 * t5 * (t138 - t143 * t144) + 0.180D3 * t22 * t14
     #4) * t26 / 0.2880D4 + t154 * t34 / 0.32D2 + t154 * t39 / 0.32D2
      t160 = FJET(XB1, XB2, s, -t2 * t107, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t159)
      t164 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, 0.10D1)
      t168 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, t29)
      t173 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, 0.10D1)
      t177 = log(-0.4D1 * t45 * t13 * t109)
      t187 = t5 * t164 * t39 / 0.32D2 - t5 * (-t164 + t168) * t42 / 0.64
     #D2 - (-0.90D2 * t5 * (t173 - t177 * t164) + 0.180D3 * t22 * t164) 
     #* t38 / 0.5760D4
      t188 = FJET(XB1, XB2, s, -t2 * t109, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t187)
      t190 = KAPPA2(t103, x2, 0.10D1, t29, z)
      t191 = s * t190
      t197 = t190 ** 2
      t203 = 0.1D1 / (-0.2D1 + t190)
      t205 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, t29)
      t207 = t205 * t26 * t33
      t210 = FJET(XB1, XB2, s, -t191 * t108, 0.0D0, -t191 * t114 * t85, 
     #t191 * t114 * x4, -s * t197 * t118 * t120 * x4, t5 * t203 * t207 /
     # 0.32D2)
      ggbbH5n1em1 = t101 * t100 + t131 * t5 * t124 * t128 / 0.32D2 + t16
     #0 * t159 + t187 * t188 + t210 * t5 * t203 * t207 / 0.32D2

      end function



      doubleprecision function ggbbH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t32 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.1D1 
     #- x4)
      t38 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 + t5 * (-t6 + 
     #t32) / x4 / 0.64D2
      t39 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t41 = -0.1D1 + x1
      t45 = ggbbH51J1(s, XB1, XB2, z, lh, wd, -t41, x2, 0.10D1, 0.10D1)
      t49 = FJET(XB1, XB2, s, -t2 * t41, 0.0D0, t2 * x1, 0.0D0, 0.0D0, t
     #5 * t45 * t8 / 0.32D2)
      t54 = -0.1D1 + x3
      t58 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, -t54, 0.10D1)
      t62 = FJET(XB1, XB2, s, -t2 * t54, t2 * x3, 0.0D0, 0.0D0, 0.0D0, t
     #5 * t58 * t11 / 0.64D2)
      ggbbH5n1em2 = t39 * t38 + t49 * t5 * t45 * t8 / 0.32D2 + t62 * t5 
     #* t58 * t11 / 0.64D2

      end function



      doubleprecision function ggbbH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t9 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n1em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n1em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t20 = t17 ** 2
      t21 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t28 - t30
      t32 = t31 * t5
      t46 = 0.120D3 * t27 * lh
      t48 = 0.60D2 * lh * t29
      t49 = -0.2884936567583026D3 - t46 + t48
      t50 = t49 * t5
      t51 = t50 * t21
      t53 = 0.1D1 / x1
      t56 = t14 * x4
      t59 = log(0.4D1 * t12 * t56)
      t61 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t62 = -0.1D1 + x4
      t63 = t56 * t62
      t66 = log(-0.4D1 * t12 * t63)
      t67 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t73 = t59 ** 2
      t76 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t78 = t66 ** 2
      t84 = -t21 + t67
      t88 = 0.1D1 / x4
      t91 = x3 * t8
      t92 = t11 * t14
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t91 * t11
      t101 = log(-0.4D1 * t98 * t63)
      t109 = 0.1D1 / x3
      t111 = t53 * t88
      t114 = t91 * t92
      t116 = log(0.4D1 * t114)
      t122 = t116 ** 2
      t134 = log(0.4D1 * t92)
      t137 = t134 ** 2
      t146 = t137 * t134
      t152 = t29 ** 2
      t153 = t27 ** 2
      t159 = t137 ** 2
      t169 = x4 * t62
      t172 = log(-0.4D1 * t92 * t169)
      t174 = t172 ** 2
      t178 = log(0.4D1 * t93)
      t180 = t178 ** 2
      t205 = -t84
      t210 = x3 * t11
      t211 = t210 * t14
      t213 = log(0.4D1 * t211)
      t215 = t213 ** 2
      t238 = log(-0.4D1 * t210 * t63)
      t242 = log(0.4D1 * t210 * t56)
      t248 = t238 ** 2
      t252 = t242 ** 2
      t263 = (-0.180D3 * t6 * (-t7 + t17 * t18 - t20 * t21 / 0.2D1) + t3
     #2 * (-t18 + t17 * t21) + 0.90D2 * t5 * (t17 * t7 - t20 * t18 / 0.2
     #D1 + t20 * t17 * t21 / 0.6D1) - t51) * t53 / 0.2880D4 + (-0.180D3 
     #* t6 * (-t18 + t59 * t21 + t61 - t66 * t67) + 0.90D2 * t5 * (-t7 +
     # t59 * t18 - t73 * t21 / 0.2D1 + t76 - t66 * t61 + t78 * t67 / 0.2
     #D1) + t32 * t84) * t53 * t88 / 0.2880D4 + (0.90D2 * t5 * (-t18 + t
     #96 * t21 + t61 - t101 * t67) - 0.180D3 * t6 * t84) * t109 * t111 /
     # 0.2880D4 + (-0.180D3 * t6 * (-t18 + t116 * t21) + 0.90D2 * t5 * (
     #-t7 + t116 * t18 - t122 * t21 / 0.2D1) - t32 * t21) * t109 * t53 /
     # 0.2880D4 - (t28 - t30 + 0.180D3 * t134 * lh + 0.45D2 * t137) * t5
     # * t7 / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t134 * t31
     # - 0.90D2 * t137 * lh - 0.15D2 * t146) * t5 * t18 / 0.5760D4 - (t1
     #52 + 0.60D2 * t153 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t2
     #9 - t134 * t49 + 0.15D2 / 0.4D1 * t159 + t137 * t31 / 0.2D1 + 0.30
     #D2 * t146 * lh) * t5 * t21 / 0.5760D4 - (-0.180D3 * t6 * (-t76 + t
     #172 * t61 - t174 * t67 / 0.2D1 + t7 - t178 * t18 + t180 * t21 / 0.
     #2D1) + t32 * (t18 - t178 * t21 - t61 + t172 * t67) + 0.90D2 * t5 *
     # (t172 * t76 - t174 * t61 / 0.2D1 + t174 * t172 * t67 / 0.6D1 - t1
     #78 * t7 + t180 * t18 / 0.2D1 - t180 * t178 * t21 / 0.6D1) + t50 * 
     #t205) * t88 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t213 * t18 + t215 
     #* t21 / 0.2D1) + t32 * (t18 - t213 * t21) + 0.90D2 * t5 * (-t213 *
     # t7 + t215 * t18 / 0.2D1 - t215 * t213 * t21 / 0.6D1) + t51) * t10
     #9 / 0.5760D4 - (-0.180D3 * t6 * (-t61 + t238 * t67 + t18 - t242 * 
     #t21) + 0.90D2 * t5 * (-t76 + t238 * t61 - t248 * t67 / 0.2D1 + t7 
     #- t242 * t18 + t252 * t21 / 0.2D1) + t32 * t205) * t109 * t88 / 0.
     #5760D4
      t264 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t263)
      t266 = 0.1D1 - x1
      t267 = 0.1D1 - x3
      t268 = KAPPA2(t266, x2, t267, 0.0D0, z)
      t269 = s * t268
      t270 = -t266
      t271 = t1 * t270
      t272 = -t267
      t273 = t271 * t272
      t275 = t271 * x3
      t277 = t1 * x1
      t279 = t268 ** 2
      t281 = t1 ** 2
      t283 = t270 * x1
      t287 = 0.1D1 / (-0.2D1 + t268)
      t288 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.0D0)
      t289 = t287 * t288
      t290 = t270 ** 2
      t291 = t290 * t272
      t292 = t279 ** 2
      t297 = log(-0.4D1 * t114 * t291 * x4 * t292)
      t299 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.0D0)
      t304 = t287 * t299
      t310 = t14 * t290
      t315 = log(-0.4D1 * t98 * t310 * t272 * t292)
      t316 = t315 * t287
      t321 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.0D0)
      t324 = t315 ** 2
      t336 = (0.90D2 * t5 * (t289 - t297 * t287 * t299) - 0.180D3 * t6 *
     # t304) * t109 * t111 / 0.2880D4 + (-0.180D3 * t6 * (t289 - t316 * 
     #t299) + 0.90D2 * t5 * (t287 * t321 - t316 * t288 + t324 * t287 * t
     #299 / 0.2D1) + t32 * t304) * t109 * t53 / 0.2880D4
      t337 = FJET(XB1, XB2, s, t269 * t273, -t269 * t275, 0.0D0, t269 * 
     #t277, s * t279 * t281 * t283 * t272, t336)
      t339 = KAPPA2(t266, x2, t267, x4, z)
      t340 = s * t339
      t343 = t277 * x4
      t345 = t277 * t62
      t347 = t339 ** 2
      t352 = cos(t9)
      t355 = sqrt(x3 * t272 * t169)
      t362 = 0.1D1 / (-0.2D1 + t339)
      t363 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, t267, x4)
      t365 = t347 ** 2
      t370 = log(0.4D1 * t114 * t291 * t169 * t365)
      t372 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, t267, x4)
      t380 = 0.90D2 * t5 * (-t362 * t363 + t370 * t362 * t372) + 0.180D3
     # * t6 * t362 * t372
      t384 = FJET(XB1, XB2, s, t340 * t273, -t340 * t275, t340 * t343, -
     #t340 * t345, s * t347 * t281 * t283 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t352 * t355), t380 * t109 * t111 / 0.2880D4)
      t392 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, x4)
      t397 = log(0.4D1 * t98 * t56 * t62 * t272)
      t398 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, x4)
      t400 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.0D0)
      t401 = t14 * t272
      t402 = t401 * x4
      t405 = log(-0.4D1 * t98 * t402)
      t406 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.0D0)
      t411 = t406 - t398
      t421 = log(-0.4D1 * t91 * t92 * t272)
      t426 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.0D0)
      t428 = t421 ** 2
      t441 = log(-0.4D1 * t210 * t401)
      t443 = t441 ** 2
      t468 = log(0.4D1 * t211 * t169 * t272)
      t472 = log(-0.4D1 * t210 * t402)
      t477 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, x4)
      t479 = t468 ** 2
      t483 = t472 ** 2
      t495 = (0.90D2 * t5 * (-t392 + t397 * t398 + t400 - t405 * t406) -
     # 0.180D3 * t6 * t411) * t109 * t111 / 0.2880D4 + (-0.180D3 * t6 * 
     #(t400 - t421 * t406) + 0.90D2 * t5 * (t426 - t421 * t400 + t428 * 
     #t406 / 0.2D1) + t32 * t406) * t109 * t53 / 0.2880D4 - (0.180D3 * t
     #6 * (t426 - t441 * t400 + t443 * t406 / 0.2D1) - t32 * (t400 - t44
     #1 * t406) - 0.90D2 * t5 * (-t441 * t426 + t443 * t400 / 0.2D1 - t4
     #43 * t441 * t406 / 0.6D1) - t50 * t406) * t109 / 0.5760D4 - (-0.18
     #0D3 * t6 * (t392 - t468 * t398 - t400 + t472 * t406) + 0.90D2 * t5
     # * (t477 - t468 * t392 + t479 * t398 / 0.2D1 - t426 + t472 * t400 
     #- t483 * t406 / 0.2D1) - t32 * t411) * t109 * t88 / 0.5760D4
      t496 = FJET(XB1, XB2, s, -t2 * t272, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t495)
      t498 = KAPPA2(t266, x2, 0.10D1, 0.0D0, z)
      t499 = s * t498
      t502 = t498 ** 2
      t508 = 0.1D1 / (-0.2D1 + t498)
      t509 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.0D0)
      t510 = t508 * t509
      t511 = t502 ** 2
      t512 = t310 * t511
      t515 = log(0.4D1 * t12 * t512)
      t516 = t515 * t508
      t517 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.0D0)
      t519 = t515 ** 2
      t520 = t519 * t508
      t521 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.0D0)
      t527 = t508 * t517
      t541 = t508 * t521
      t545 = t290 * x4
      t549 = log(0.4D1 * t15 * t545 * t511)
      t550 = t549 * t508
      t556 = t549 ** 2
      t563 = t32 * t541
      t571 = log(0.4D1 * t98 * t310 * x4 * t511)
      t584 = log(0.4D1 * t98 * t512)
      t585 = t584 * t508
      t591 = t584 ** 2
      t602 = (0.180D3 * t6 * (t510 - t516 * t517 + t520 * t521 / 0.2D1) 
     #- t32 * (t527 - t516 * t521) - 0.90D2 * t5 * (-t516 * t509 + t520 
     #* t517 / 0.2D1 - t519 * t515 * t508 * t521 / 0.6D1) - t50 * t541) 
     #* t53 / 0.2880D4 + (-0.180D3 * t6 * (-t527 + t550 * t521) + 0.90D2
     # * t5 * (-t510 + t550 * t517 - t556 * t508 * t521 / 0.2D1) - t563)
     # * t53 * t88 / 0.2880D4 + (0.90D2 * t5 * (-t527 + t571 * t508 * t5
     #21) + 0.180D3 * t6 * t541) * t109 * t111 / 0.2880D4 + (0.180D3 * t
     #6 * (t527 - t585 * t521) - 0.90D2 * t5 * (t510 - t585 * t517 + t59
     #1 * t508 * t521 / 0.2D1) - t563) * t109 * t53 / 0.2880D4
      t603 = FJET(XB1, XB2, s, -t499 * t271, 0.0D0, 0.0D0, t499 * t277, 
     #-s * t502 * t281 * t270 * x1, t602)
      t605 = KAPPA2(t266, x2, 0.10D1, x4, z)
      t606 = s * t605
      t610 = t605 ** 2
      t616 = 0.1D1 / (-0.2D1 + t605)
      t617 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, x4)
      t618 = t616 * t617
      t619 = t610 ** 2
      t621 = t545 * t62 * t619
      t624 = log(-0.4D1 * t15 * t621)
      t625 = t624 * t616
      t626 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, x4)
      t631 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, x4)
      t634 = t624 ** 2
      t641 = t616 * t626
      t648 = log(-0.4D1 * t114 * t621)
      t660 = (0.180D3 * t6 * (-t618 + t625 * t626) - 0.90D2 * t5 * (-t61
     #6 * t631 + t625 * t617 - t634 * t616 * t626 / 0.2D1) + t32 * t641)
     # * t53 * t88 / 0.2880D4 + (-0.90D2 * t5 * (-t618 + t648 * t616 * t
     #626) - 0.180D3 * t6 * t641) * t109 * t111 / 0.2880D4
      t661 = FJET(XB1, XB2, s, -t606 * t271, 0.0D0, t606 * t343, -t606 *
     # t345, s * t610 * t281 * t283 * t62, t660)
      ggbbH5n2e1 = t264 * t263 + t337 * t336 + t384 * t380 * t109 * t53 
     #* t88 / 0.2880D4 + t496 * t495 + t603 * t602 + t661 * t660

      end function



      doubleprecision function ggbbH5n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t20 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t21 = -0.1D1 + x4
      t22 = t14 * t21
      t25 = log(-0.4D1 * t11 * t22)
      t26 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t31 = lh * t5
      t32 = -t18 + t26
      t36 = 0.1D1 / x1
      t38 = 0.1D1 / x4
      t42 = 0.1D1 / x3
      t44 = t42 * t36 * t38
      t47 = x3 * t7
      t48 = t10 * t13
      t51 = log(0.4D1 * t47 * t48)
      t62 = t11 * t13
      t64 = log(0.4D1 * t62)
      t69 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t71 = t64 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t5
      t83 = t82 * t18
      t89 = log(0.4D1 * t48 * x4)
      t91 = x4 * t21
      t94 = log(-0.4D1 * t48 * t91)
      t99 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t101 = t94 ** 2
      t105 = t89 ** 2
      t111 = -t32
      t116 = x3 * t10
      t119 = log(-0.4D1 * t116 * t22)
      t123 = log(0.4D1 * t116 * t14)
      t134 = t116 * t13
      t136 = log(0.4D1 * t134)
      t142 = t136 ** 2
      t153 = log(0.4D1 * t48)
      t161 = t153 ** 2
      t180 = (0.90D2 * t5 * (-t6 + t17 * t18 + t20 - t25 * t26) - 0.180D
     #3 * t31 * t32) * t36 * t38 / 0.2880D4 + t5 * t32 * t44 / 0.32D2 + 
     #(0.90D2 * t5 * (-t6 + t51 * t18) + 0.180D3 * t31 * t18) * t42 * t3
     #6 / 0.2880D4 + (-0.180D3 * t31 * (-t6 + t64 * t18) + 0.90D2 * t5 *
     # (-t69 + t64 * t6 - t71 * t18 / 0.2D1) - t83) * t36 / 0.2880D4 - (
     #-0.180D3 * t31 * (t6 - t89 * t18 - t20 + t94 * t26) + 0.90D2 * t5 
     #* (-t99 + t94 * t20 - t101 * t26 / 0.2D1 + t69 - t89 * t6 + t105 *
     # t18 / 0.2D1) + t82 * t111) * t38 / 0.5760D4 - (0.90D2 * t5 * (-t2
     #0 + t119 * t26 + t6 - t123 * t18) - 0.180D3 * t31 * t111) * t42 * 
     #t38 / 0.5760D4 - (-0.180D3 * t31 * (t6 - t136 * t18) + 0.90D2 * t5
     # * (t69 - t136 * t6 + t142 * t18 / 0.2D1) + t83) * t42 / 0.5760D4 
     #- (-0.180D3 * lh - 0.90D2 * t153) * t5 * t69 / 0.5760D4 - (t78 - t
     #80 + 0.180D3 * t153 * lh + 0.45D2 * t161) * t5 * t6 / 0.5760D4 - (
     #-0.2884936567583026D3 - 0.120D3 * t77 * lh + 0.60D2 * lh * t79 - t
     #153 * t81 - 0.90D2 * t161 * lh - 0.15D2 * t161 * t153) * t5 * t18 
     #/ 0.5760D4
      t181 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t180)
      t183 = 0.1D1 - x1
      t184 = 0.1D1 - x3
      t185 = KAPPA2(t183, x2, t184, 0.0D0, z)
      t186 = s * t185
      t187 = -t183
      t188 = t1 * t187
      t189 = -t184
      t190 = t188 * t189
      t192 = t188 * x3
      t194 = t1 * x1
      t196 = t185 ** 2
      t198 = t1 ** 2
      t200 = t187 * x1
      t204 = 0.1D1 / (-0.2D1 + t185)
      t206 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t183, x2, t184, 0.0D0)
      t210 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t183, x2, t184, 0.0D0)
      t212 = t47 * t10
      t213 = t187 ** 2
      t214 = t13 * t213
      t215 = t196 ** 2
      t220 = log(-0.4D1 * t212 * t214 * t189 * t215)
      t233 = t5 * t204 * t206 * t44 / 0.32D2 + (0.90D2 * t5 * (t204 * t2
     #10 - t220 * t204 * t206) - 0.180D3 * t31 * t204 * t206) * t42 * t3
     #6 / 0.2880D4
      t234 = FJET(XB1, XB2, s, t186 * t190, -t186 * t192, 0.0D0, t186 * 
     #t194, s * t196 * t198 * t200 * t189, t233)
      t236 = KAPPA2(t183, x2, t184, x4, z)
      t237 = s * t236
      t240 = t194 * x4
      t242 = t194 * t21
      t244 = t236 ** 2
      t249 = cos(t8)
      t252 = sqrt(x3 * t189 * t91)
      t259 = 0.1D1 / (-0.2D1 + t236)
      t261 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t183, x2, t184, x4)
      t265 = FJET(XB1, XB2, s, t237 * t190, -t237 * t192, t237 * t240, -
     #t237 * t242, s * t244 * t198 * t200 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t249 * t252), -t5 * t259 * t261 * t44 / 0.32D2)
      t275 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, 0.0D0)
      t276 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, x4)
      t277 = t275 - t276
      t281 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, 0.0D0)
      t285 = log(-0.4D1 * t47 * t48 * t189)
      t296 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, x4)
      t300 = log(0.4D1 * t134 * t91 * t189)
      t302 = t13 * t189
      t306 = log(-0.4D1 * t116 * t302 * x4)
      t320 = log(-0.4D1 * t116 * t302)
      t325 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, 0.0D0)
      t327 = t320 ** 2
      t337 = t5 * t277 * t44 / 0.32D2 + (0.90D2 * t5 * (t281 - t285 * t2
     #75) - 0.180D3 * t31 * t275) * t42 * t36 / 0.2880D4 - (0.90D2 * t5 
     #* (t296 - t300 * t276 - t281 + t306 * t275) + 0.180D3 * t31 * t277
     #) * t42 * t38 / 0.5760D4 - (0.180D3 * t31 * (t281 - t320 * t275) -
     # 0.90D2 * t5 * (t325 - t320 * t281 + t327 * t275 / 0.2D1) - t82 * 
     #t275) * t42 / 0.5760D4
      t338 = FJET(XB1, XB2, s, -t2 * t189, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t337)
      t340 = KAPPA2(t183, x2, 0.10D1, 0.0D0, z)
      t341 = s * t340
      t344 = t340 ** 2
      t350 = 0.1D1 / (-0.2D1 + t340)
      t351 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, 0.0D0)
      t352 = t350 * t351
      t353 = t213 * x4
      t354 = t344 ** 2
      t358 = log(0.4D1 * t62 * t353 * t354)
      t360 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, 0.0D0)
      t365 = t350 * t360
      t367 = 0.180D3 * t31 * t365
      t376 = t214 * t354
      t379 = log(0.4D1 * t212 * t376)
      t391 = log(0.4D1 * t11 * t376)
      t392 = t391 * t350
      t397 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, 0.0D0)
      t400 = t391 ** 2
      t411 = (0.90D2 * t5 * (-t352 + t358 * t350 * t360) + t367) * t36 *
     # t38 / 0.2880D4 - t5 * t350 * t360 * t44 / 0.32D2 + (-0.90D2 * t5 
     #* (t352 - t379 * t350 * t360) + t367) * t42 * t36 / 0.2880D4 + (0.
     #180D3 * t31 * (t352 - t392 * t360) - 0.90D2 * t5 * (t350 * t397 - 
     #t392 * t351 + t400 * t350 * t360 / 0.2D1) - t82 * t365) * t36 / 0.
     #2880D4
      t412 = FJET(XB1, XB2, s, -t341 * t188, 0.0D0, 0.0D0, t341 * t194, 
     #-s * t344 * t198 * t187 * x1, t411)
      t414 = KAPPA2(t183, x2, 0.10D1, x4, z)
      t415 = s * t414
      t419 = t414 ** 2
      t425 = 0.1D1 / (-0.2D1 + t414)
      t426 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, x4)
      t428 = t419 ** 2
      t433 = log(-0.4D1 * t62 * t353 * t21 * t428)
      t435 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, x4)
      t451 = (-0.90D2 * t5 * (-t425 * t426 + t433 * t425 * t435) - 0.180
     #D3 * t31 * t425 * t435) * t36 * t38 / 0.2880D4 + t5 * t425 * t435 
     #* t44 / 0.32D2
      t452 = FJET(XB1, XB2, s, -t415 * t188, 0.0D0, t415 * t240, -t415 *
     # t242, s * t419 * t198 * t200 * t21, t451)
      ggbbH5n2e0 = t181 * t180 + t234 * t233 - t265 * t5 * t259 * t261 *
     # t42 * t36 * t38 / 0.32D2 + t338 * t337 + t412 * t411 + t452 * t45
     #1

      end function



      doubleprecision function ggbbH5n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t30 = -t17 + t29
      t32 = 0.1D1 / x4
      t37 = 0.1D1 / x3
      t38 = t37 * t26
      t41 = -t30
      t43 = t37 * t32
      t46 = x3 * t10
      t49 = log(0.4D1 * t46 * t13)
      t57 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t61 = t10 * t13
      t63 = log(0.4D1 * t61)
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t75 = t63 ** 2
      t83 = log(0.4D1 * t61 * x4)
      t85 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t86 = -0.1D1 + x4
      t90 = log(-0.4D1 * t61 * x4 * t86)
      t100 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.2880D4 + 
     #t5 * t30 * t26 * t32 / 0.32D2 - t5 * t17 * t38 / 0.32D2 - t5 * t41
     # * t43 / 0.64D2 - (0.90D2 * t5 * (t6 - t49 * t17) - t24) * t37 / 0
     #.5760D4 - t5 * t57 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t63) * t5 
     #* t6 / 0.5760D4 - (0.180D3 * t69 - 0.30D2 * t71 + 0.180D3 * t63 * 
     #lh + 0.45D2 * t75) * t5 * t17 / 0.5760D4 - (0.90D2 * t5 * (t6 - t8
     #3 * t17 - t85 + t90 * t29) - 0.180D3 * t22 * t41) * t32 / 0.5760D4
      t101 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t100)
      t103 = 0.1D1 - x1
      t104 = 0.1D1 - x3
      t105 = KAPPA2(t103, x2, t104, 0.0D0, z)
      t106 = s * t105
      t107 = -t103
      t108 = t1 * t107
      t109 = -t104
      t114 = t1 * x1
      t116 = t105 ** 2
      t118 = t1 ** 2
      t120 = t107 * x1
      t124 = 0.1D1 / (-0.2D1 + t105)
      t126 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t103, x2, t104, 0.0D0)
      t128 = t126 * t37 * t26
      t131 = FJET(XB1, XB2, s, t106 * t108 * t109, -t106 * t108 * x3, 0.
     #0D0, t106 * t114, s * t116 * t118 * t120 * t109, t5 * t124 * t128 
     #/ 0.32D2)
      t138 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, 0.0D0)
      t142 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, x4)
      t147 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, 0.0D0)
      t151 = log(-0.4D1 * t46 * t13 * t109)
      t161 = t5 * t138 * t38 / 0.32D2 - t5 * (t142 - t138) * t43 / 0.64D
     #2 - (-0.90D2 * t5 * (t147 - t151 * t138) + 0.180D3 * t22 * t138) *
     # t37 / 0.5760D4
      t162 = FJET(XB1, XB2, s, -t2 * t109, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t161)
      t164 = KAPPA2(t103, x2, 0.10D1, 0.0D0, z)
      t165 = s * t164
      t168 = t164 ** 2
      t174 = 0.1D1 / (-0.2D1 + t164)
      t175 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, 0.0D0)
      t177 = t107 ** 2
      t179 = t168 ** 2
      t183 = log(0.4D1 * t11 * t13 * t177 * t179)
      t185 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, 0.0D0)
      t196 = t5 * t174
      t205 = (-0.90D2 * t5 * (t174 * t175 - t183 * t174 * t185) + 0.180D
     #3 * t22 * t174 * t185) * t26 / 0.2880D4 - t196 * t185 * t26 * t32 
     #/ 0.32D2 - t196 * t185 * t37 * t26 / 0.32D2
      t206 = FJET(XB1, XB2, s, -t165 * t108, 0.0D0, 0.0D0, t165 * t114, 
     #-s * t168 * t118 * t107 * x1, t205)
      t208 = KAPPA2(t103, x2, 0.10D1, x4, z)
      t209 = s * t208
      t215 = t208 ** 2
      t221 = 0.1D1 / (-0.2D1 + t208)
      t223 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, x4)
      t225 = t223 * t26 * t32
      t228 = FJET(XB1, XB2, s, -t209 * t108, 0.0D0, t209 * t114 * x4, -t
     #209 * t114 * t86, s * t215 * t118 * t120 * t86, t5 * t221 * t225 /
     # 0.32D2)
      ggbbH5n2em1 = t101 * t100 + t131 * t5 * t124 * t128 / 0.32D2 + t16
     #2 * t161 + t206 * t205 + t228 * t5 * t221 * t225 / 0.32D2

      end function



      doubleprecision function ggbbH5n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t31 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t37 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 - t5 * (t6 - t
     #31) / x4 / 0.64D2
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t37)
      t40 = 0.1D1 - x1
      t41 = KAPPA2(t40, x2, 0.10D1, 0.0D0, z)
      t42 = s * t41
      t43 = -t40
      t48 = t41 ** 2
      t50 = t1 ** 2
      t55 = 0.1D1 / (-0.2D1 + t41)
      t57 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t40, x2, 0.10D1, 0.0D0)
      t61 = FJET(XB1, XB2, s, -t42 * t1 * t43, 0.0D0, 0.0D0, t42 * t1 * 
     #x1, -s * t48 * t50 * t43 * x1, -t5 * t55 * t57 * t8 / 0.32D2)
      t67 = -0.1D1 + x3
      t71 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, -t67, 0.0D0)
      t75 = FJET(XB1, XB2, s, -t2 * t67, t2 * x3, 0.0D0, 0.0D0, 0.0D0, t
     #5 * t71 * t11 / 0.64D2)
      ggbbH5n2em2 = t38 * t37 - t61 * t5 * t55 * t57 * t8 / 0.32D2 + t75
     # * t5 * t71 * t11 / 0.64D2

      end function



      doubleprecision function ggbbH5n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t9 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n2em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n2em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n3e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t20 = t17 ** 2
      t21 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t28 - t30
      t32 = t31 * t5
      t46 = 0.120D3 * t27 * lh
      t48 = 0.60D2 * lh * t29
      t49 = -0.2884936567583026D3 - t46 + t48
      t50 = t49 * t5
      t51 = t50 * t21
      t53 = 0.1D1 / x1
      t56 = t14 * x4
      t59 = log(0.4D1 * t12 * t56)
      t61 = 0.1D1 - x4
      t62 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t61)
      t63 = -t61
      t64 = t56 * t63
      t67 = log(-0.4D1 * t12 * t64)
      t68 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t61)
      t74 = t59 ** 2
      t77 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t61)
      t79 = t67 ** 2
      t85 = t68 - t21
      t89 = 0.1D1 / x4
      t92 = x3 * t8
      t93 = t92 * t11
      t96 = log(-0.4D1 * t93 * t64)
      t98 = t11 * t14
      t99 = t98 * x4
      t102 = log(0.4D1 * t92 * t99)
      t107 = -t85
      t111 = 0.1D1 / x3
      t113 = t53 * t89
      t116 = t92 * t98
      t118 = log(0.4D1 * t116)
      t124 = t118 ** 2
      t136 = log(0.4D1 * t98)
      t139 = t136 ** 2
      t148 = t139 * t136
      t154 = t29 ** 2
      t155 = t27 ** 2
      t161 = t139 ** 2
      t172 = log(0.4D1 * t99)
      t174 = t172 ** 2
      t177 = x4 * t63
      t180 = log(-0.4D1 * t98 * t177)
      t182 = t180 ** 2
      t211 = x3 * t11
      t212 = t211 * t14
      t214 = log(0.4D1 * t212)
      t216 = t214 ** 2
      t239 = log(-0.4D1 * t211 * t64)
      t243 = log(0.4D1 * t211 * t56)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t264 = (-0.180D3 * t6 * (-t7 + t17 * t18 - t20 * t21 / 0.2D1) + t3
     #2 * (-t18 + t17 * t21) + 0.90D2 * t5 * (t17 * t7 - t20 * t18 / 0.2
     #D1 + t20 * t17 * t21 / 0.6D1) - t51) * t53 / 0.2880D4 + (-0.180D3 
     #* t6 * (-t18 + t59 * t21 + t62 - t67 * t68) + 0.90D2 * t5 * (-t7 +
     # t59 * t18 - t74 * t21 / 0.2D1 + t77 - t67 * t62 + t79 * t68 / 0.2
     #D1) + t32 * t85) * t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (-t62 + t
     #96 * t68 + t18 - t102 * t21) - 0.180D3 * t6 * t107) * t111 * t113 
     #/ 0.2880D4 - (-0.180D3 * t6 * (t18 - t118 * t21) + 0.90D2 * t5 * (
     #t7 - t118 * t18 + t124 * t21 / 0.2D1) + t32 * t21) * t111 * t53 / 
     #0.2880D4 - (t28 - t30 + 0.180D3 * t136 * lh + 0.45D2 * t139) * t5 
     #* t7 / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t136 * t31 
     #- 0.90D2 * t139 * lh - 0.15D2 * t148) * t5 * t18 / 0.5760D4 - (t15
     #4 + 0.60D2 * t155 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29
     # - t136 * t49 + 0.15D2 / 0.4D1 * t161 + t139 * t31 / 0.2D1 + 0.30D
     #2 * t148 * lh) * t5 * t21 / 0.5760D4 + (-0.180D3 * t6 * (-t7 + t17
     #2 * t18 - t174 * t21 / 0.2D1 + t77 - t180 * t62 + t182 * t68 / 0.2
     #D1) + t32 * (t62 - t180 * t68 - t18 + t172 * t21) + 0.90D2 * t5 * 
     #(t172 * t7 - t174 * t18 / 0.2D1 + t174 * t172 * t21 / 0.6D1 - t180
     # * t77 + t182 * t62 / 0.2D1 - t182 * t180 * t68 / 0.6D1) + t50 * t
     #85) * t89 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t214 * t18 + t216 * 
     #t21 / 0.2D1) + t32 * (t18 - t214 * t21) + 0.90D2 * t5 * (-t214 * t
     #7 + t216 * t18 / 0.2D1 - t216 * t214 * t21 / 0.6D1) + t51) * t111 
     #/ 0.5760D4 - (-0.180D3 * t6 * (-t62 + t239 * t68 + t18 - t243 * t2
     #1) + 0.90D2 * t5 * (-t77 + t239 * t62 - t249 * t68 / 0.2D1 + t7 - 
     #t243 * t18 + t253 * t21 / 0.2D1) + t32 * t107) * t111 * t89 / 0.57
     #60D4
      t265 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t264)
      t267 = 0.1D1 - x1
      t268 = KAPPA2(t267, x2, 0.0D0, 0.10D1, z)
      t269 = s * t268
      t270 = -t267
      t271 = t1 * t270
      t273 = t1 * x1
      t275 = t268 ** 2
      t277 = t1 ** 2
      t282 = 0.1D1 / (-0.2D1 + t268)
      t283 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, 0.10D1)
      t284 = t282 * t283
      t285 = t270 ** 2
      t286 = t14 * t285
      t287 = t275 ** 2
      t288 = t286 * t287
      t291 = log(0.4D1 * t12 * t288)
      t292 = t291 * t282
      t293 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, 0.10D1)
      t295 = t291 ** 2
      t296 = t295 * t282
      t297 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, 0.10D1)
      t303 = t282 * t293
      t317 = t282 * t297
      t321 = t285 * x4
      t325 = log(0.4D1 * t15 * t321 * t287)
      t326 = t325 * t282
      t332 = t325 ** 2
      t339 = t32 * t317
      t347 = log(0.4D1 * t93 * t286 * x4 * t287)
      t360 = log(0.4D1 * t93 * t288)
      t361 = t360 * t282
      t367 = t360 ** 2
      t378 = (0.180D3 * t6 * (t284 - t292 * t293 + t296 * t297 / 0.2D1) 
     #- t32 * (t303 - t292 * t297) - 0.90D2 * t5 * (-t292 * t283 + t296 
     #* t293 / 0.2D1 - t295 * t291 * t282 * t297 / 0.6D1) - t50 * t317) 
     #* t53 / 0.2880D4 + (-0.180D3 * t6 * (-t303 + t326 * t297) + 0.90D2
     # * t5 * (-t284 + t326 * t293 - t332 * t282 * t297 / 0.2D1) - t339)
     # * t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (t303 - t347 * t282 * t29
     #7) - 0.180D3 * t6 * t317) * t111 * t113 / 0.2880D4 - (-0.180D3 * t
     #6 * (t303 - t361 * t297) + 0.90D2 * t5 * (t284 - t361 * t293 + t36
     #7 * t282 * t297 / 0.2D1) + t339) * t111 * t53 / 0.2880D4
      t379 = FJET(XB1, XB2, s, 0.0D0, -t269 * t271, t269 * t273, 0.0D0, 
     #-s * t275 * t277 * t270 * x1, t378)
      t381 = KAPPA2(t267, x2, 0.0D0, t61, z)
      t382 = s * t381
      t384 = t273 * t63
      t386 = t273 * x4
      t388 = t381 ** 2
      t391 = t270 * x1
      t395 = 0.1D1 / (-0.2D1 + t381)
      t396 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, t61)
      t397 = t395 * t396
      t398 = t388 ** 2
      t400 = t321 * t63 * t398
      t403 = log(-0.4D1 * t15 * t400)
      t404 = t403 * t395
      t405 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, t61)
      t410 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, t61)
      t413 = t403 ** 2
      t420 = t395 * t405
      t427 = log(-0.4D1 * t116 * t400)
      t439 = (-0.180D3 * t6 * (t397 - t404 * t405) + 0.90D2 * t5 * (t395
     # * t410 - t404 * t396 + t413 * t395 * t405 / 0.2D1) + t32 * t420) 
     #* t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (-t397 + t427 * t395 * t40
     #5) + 0.180D3 * t6 * t420) * t111 * t113 / 0.2880D4
      t440 = FJET(XB1, XB2, s, 0.0D0, -t382 * t271, -t382 * t384, t382 *
     # t386, s * t388 * t277 * t391 * t63, t439)
      t443 = -0.1D1 + x3
      t445 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t446 = t14 * t443
      t447 = t446 * x4
      t450 = log(-0.4D1 * t93 * t447)
      t451 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t453 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t61)
      t458 = log(0.4D1 * t93 * t56 * t63 * t443)
      t459 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t61)
      t464 = -t451 + t459
      t474 = log(-0.4D1 * t92 * t98 * t443)
      t479 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t481 = t474 ** 2
      t494 = log(-0.4D1 * t211 * t446)
      t496 = t494 ** 2
      t520 = log(-0.4D1 * t211 * t447)
      t525 = log(0.4D1 * t212 * t177 * t443)
      t531 = t520 ** 2
      t534 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t61)
      t536 = t525 ** 2
      t547 = -(0.90D2 * t5 * (-t445 + t450 * t451 + t453 - t458 * t459) 
     #- 0.180D3 * t6 * t464) * t111 * t113 / 0.2880D4 - (-0.180D3 * t6 *
     # (-t445 + t474 * t451) + 0.90D2 * t5 * (-t479 + t474 * t445 - t481
     # * t451 / 0.2D1) - t32 * t451) * t111 * t53 / 0.2880D4 - (0.180D3 
     #* t6 * (t479 - t494 * t445 + t496 * t451 / 0.2D1) - t32 * (t445 - 
     #t494 * t451) - 0.90D2 * t5 * (-t494 * t479 + t496 * t445 / 0.2D1 -
     # t496 * t494 * t451 / 0.6D1) - t50 * t451) * t111 / 0.5760D4 - (-0
     #.180D3 * t6 * (-t445 + t520 * t451 + t453 - t525 * t459) + 0.90D2 
     #* t5 * (-t479 + t520 * t445 - t531 * t451 / 0.2D1 + t534 - t525 * 
     #t453 + t536 * t459 / 0.2D1) + t32 * t464) * t111 * t89 / 0.5760D4
      t548 = FJET(XB1, XB2, s, t2 * x3, -t2 * t443, 0.0D0, 0.0D0, 0.0D0,
     # t547)
      t550 = KAPPA2(t267, x2, x3, 0.10D1, z)
      t551 = s * t550
      t552 = t271 * x3
      t554 = t271 * t443
      t557 = t550 ** 2
      t563 = 0.1D1 / (-0.2D1 + t550)
      t564 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t267, x2, x3, 0.10D1)
      t565 = t563 * t564
      t566 = t285 * t443
      t567 = t557 ** 2
      t572 = log(-0.4D1 * t116 * t566 * x4 * t567)
      t574 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t267, x2, x3, 0.10D1)
      t579 = t563 * t574
      t589 = log(-0.4D1 * t93 * t286 * t443 * t567)
      t590 = t589 * t563
      t595 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t267, x2, x3, 0.10D1)
      t598 = t589 ** 2
      t610 = -(0.90D2 * t5 * (-t565 + t572 * t563 * t574) + 0.180D3 * t6
     # * t579) * t111 * t113 / 0.2880D4 - (0.180D3 * t6 * (t565 - t590 *
     # t574) - 0.90D2 * t5 * (t563 * t595 - t590 * t564 + t598 * t563 * 
     #t574 / 0.2D1) - t32 * t579) * t111 * t53 / 0.2880D4
      t611 = FJET(XB1, XB2, s, -t551 * t552, t551 * t554, t551 * t273, 0
     #.0D0, s * t557 * t277 * t391 * t443, t610)
      t613 = KAPPA2(t267, x2, x3, t61, z)
      t614 = s * t613
      t619 = t613 ** 2
      t624 = cos(t9)
      t627 = sqrt(x3 * t443 * t177)
      t634 = 0.1D1 / (-0.2D1 + t613)
      t635 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t267, x2, x3, t61)
      t637 = t619 ** 2
      t642 = log(0.4D1 * t116 * t566 * t177 * t637)
      t644 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t267, x2, x3, t61)
      t652 = 0.90D2 * t5 * (t634 * t635 - t642 * t634 * t644) - 0.180D3 
     #* t6 * t634 * t644
      t656 = FJET(XB1, XB2, s, -t614 * t552, t614 * t554, -t614 * t384, 
     #t614 * t386, s * t619 * t277 * t391 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t624 * t627), -t652 * t111 * t113 / 0.2880D4)
      ggbbH5n3e1 = t265 * t264 + t379 * t378 + t440 * t439 + t548 * t547
     # + t611 * t610 - t656 * t652 * t111 * t53 * t89 / 0.2880D4

      end function



      doubleprecision function ggbbH5n3e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t20 = 0.1D1 - x4
      t21 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t20)
      t22 = -t20
      t23 = t14 * t22
      t26 = log(-0.4D1 * t11 * t23)
      t27 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t20)
      t32 = lh * t5
      t33 = t27 - t18
      t37 = 0.1D1 / x1
      t39 = 0.1D1 / x4
      t42 = -t33
      t44 = 0.1D1 / x3
      t46 = t44 * t37 * t39
      t49 = x3 * t7
      t50 = t10 * t13
      t53 = log(0.4D1 * t49 * t50)
      t64 = t11 * t13
      t66 = log(0.4D1 * t64)
      t71 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t73 = t66 ** 2
      t79 = lh ** 2
      t80 = 0.180D3 * t79
      t81 = 0.3141592653589793D1 ** 2
      t82 = 0.30D2 * t81
      t83 = t80 - t82
      t84 = t83 * t5
      t85 = t84 * t18
      t89 = x4 * t22
      t92 = log(-0.4D1 * t50 * t89)
      t96 = log(0.4D1 * t50 * x4)
      t102 = t96 ** 2
      t105 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t20)
      t107 = t92 ** 2
      t117 = x3 * t10
      t120 = log(-0.4D1 * t117 * t23)
      t124 = log(0.4D1 * t117 * t14)
      t135 = t117 * t13
      t137 = log(0.4D1 * t135)
      t143 = t137 ** 2
      t154 = log(0.4D1 * t50)
      t162 = t154 ** 2
      t181 = (0.90D2 * t5 * (-t6 + t17 * t18 + t21 - t26 * t27) - 0.180D
     #3 * t32 * t33) * t37 * t39 / 0.2880D4 - t5 * t42 * t46 / 0.32D2 - 
     #(0.90D2 * t5 * (t6 - t53 * t18) - 0.180D3 * t32 * t18) * t44 * t37
     # / 0.2880D4 + (-0.180D3 * t32 * (-t6 + t66 * t18) + 0.90D2 * t5 * 
     #(-t71 + t66 * t6 - t73 * t18 / 0.2D1) - t85) * t37 / 0.2880D4 + (-
     #0.180D3 * t32 * (t21 - t92 * t27 - t6 + t96 * t18) + 0.90D2 * t5 *
     # (-t71 + t96 * t6 - t102 * t18 / 0.2D1 + t105 - t92 * t21 + t107 *
     # t27 / 0.2D1) + t84 * t33) * t39 / 0.5760D4 - (0.90D2 * t5 * (-t21
     # + t120 * t27 + t6 - t124 * t18) - 0.180D3 * t32 * t42) * t44 * t3
     #9 / 0.5760D4 - (-0.180D3 * t32 * (t6 - t137 * t18) + 0.90D2 * t5 *
     # (t71 - t137 * t6 + t143 * t18 / 0.2D1) + t85) * t44 / 0.5760D4 - 
     #(-0.180D3 * lh - 0.90D2 * t154) * t5 * t71 / 0.5760D4 - (t80 - t82
     # + 0.180D3 * t154 * lh + 0.45D2 * t162) * t5 * t6 / 0.5760D4 - (-0
     #.2884936567583026D3 - 0.120D3 * t79 * lh + 0.60D2 * lh * t81 - t15
     #4 * t83 - 0.90D2 * t162 * lh - 0.15D2 * t162 * t154) * t5 * t18 / 
     #0.5760D4
      t182 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t181)
      t184 = 0.1D1 - x1
      t185 = KAPPA2(t184, x2, 0.0D0, 0.10D1, z)
      t186 = s * t185
      t187 = -t184
      t188 = t1 * t187
      t190 = t1 * x1
      t192 = t185 ** 2
      t194 = t1 ** 2
      t199 = 0.1D1 / (-0.2D1 + t185)
      t200 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, 0.10D1)
      t201 = t199 * t200
      t202 = t187 ** 2
      t203 = t202 * x4
      t204 = t192 ** 2
      t208 = log(0.4D1 * t64 * t203 * t204)
      t210 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, 0.10D1)
      t215 = t199 * t210
      t217 = 0.180D3 * t32 * t215
      t226 = t49 * t10
      t227 = t13 * t202
      t228 = t227 * t204
      t231 = log(0.4D1 * t226 * t228)
      t243 = log(0.4D1 * t11 * t228)
      t244 = t243 * t199
      t249 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, 0.10D1)
      t252 = t243 ** 2
      t263 = (0.90D2 * t5 * (-t201 + t208 * t199 * t210) + t217) * t37 *
     # t39 / 0.2880D4 - t5 * t199 * t210 * t46 / 0.32D2 - (0.90D2 * t5 *
     # (t201 - t231 * t199 * t210) - t217) * t44 * t37 / 0.2880D4 + (0.1
     #80D3 * t32 * (t201 - t244 * t210) - 0.90D2 * t5 * (t199 * t249 - t
     #244 * t200 + t252 * t199 * t210 / 0.2D1) - t84 * t215) * t37 / 0.2
     #880D4
      t264 = FJET(XB1, XB2, s, 0.0D0, -t186 * t188, t186 * t190, 0.0D0, 
     #-s * t192 * t194 * t187 * x1, t263)
      t266 = KAPPA2(t184, x2, 0.0D0, t20, z)
      t267 = s * t266
      t269 = t190 * t22
      t271 = t190 * x4
      t273 = t266 ** 2
      t276 = t187 * x1
      t280 = 0.1D1 / (-0.2D1 + t266)
      t281 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, t20)
      t283 = t273 ** 2
      t288 = log(-0.4D1 * t64 * t203 * t22 * t283)
      t290 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, t20)
      t306 = (0.90D2 * t5 * (t280 * t281 - t288 * t280 * t290) - 0.180D3
     # * t32 * t280 * t290) * t37 * t39 / 0.2880D4 + t5 * t280 * t290 * 
     #t46 / 0.32D2
      t307 = FJET(XB1, XB2, s, 0.0D0, -t267 * t188, -t267 * t269, t267 *
     # t271, s * t273 * t194 * t276 * t22, t306)
      t310 = -0.1D1 + x3
      t312 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t313 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t20)
      t314 = -t312 + t313
      t318 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t322 = log(-0.4D1 * t49 * t50 * t310)
      t333 = t13 * t310
      t337 = log(-0.4D1 * t117 * t333 * x4)
      t339 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t20)
      t343 = log(0.4D1 * t135 * t89 * t310)
      t356 = log(-0.4D1 * t117 * t333)
      t361 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t363 = t356 ** 2
      t373 = -t5 * t314 * t46 / 0.32D2 - (0.90D2 * t5 * (-t318 + t322 * 
     #t312) + 0.180D3 * t32 * t312) * t44 * t37 / 0.2880D4 - (0.90D2 * t
     #5 * (-t318 + t337 * t312 + t339 - t343 * t313) - 0.180D3 * t32 * t
     #314) * t44 * t39 / 0.5760D4 - (0.180D3 * t32 * (t318 - t356 * t312
     #) - 0.90D2 * t5 * (t361 - t356 * t318 + t363 * t312 / 0.2D1) - t84
     # * t312) * t44 / 0.5760D4
      t374 = FJET(XB1, XB2, s, t2 * x3, -t2 * t310, 0.0D0, 0.0D0, 0.0D0,
     # t373)
      t376 = KAPPA2(t184, x2, x3, 0.10D1, z)
      t377 = s * t376
      t378 = t188 * x3
      t380 = t188 * t310
      t383 = t376 ** 2
      t389 = 0.1D1 / (-0.2D1 + t376)
      t391 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t184, x2, x3, 0.10D1)
      t395 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t184, x2, x3, 0.10D1)
      t397 = t383 ** 2
      t402 = log(-0.4D1 * t226 * t227 * t310 * t397)
      t415 = t5 * t389 * t391 * t46 / 0.32D2 - (-0.90D2 * t5 * (t389 * t
     #395 - t402 * t389 * t391) + 0.180D3 * t32 * t389 * t391) * t44 * t
     #37 / 0.2880D4
      t416 = FJET(XB1, XB2, s, -t377 * t378, t377 * t380, t377 * t190, 0
     #.0D0, s * t383 * t194 * t276 * t310, t415)
      t418 = KAPPA2(t184, x2, x3, t20, z)
      t419 = s * t418
      t424 = t418 ** 2
      t429 = cos(t8)
      t432 = sqrt(x3 * t310 * t89)
      t439 = 0.1D1 / (-0.2D1 + t418)
      t441 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t184, x2, x3, t20)
      t445 = FJET(XB1, XB2, s, -t419 * t378, t419 * t380, -t419 * t269, 
     #t419 * t271, s * t424 * t194 * t276 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t429 * t432), -t5 * t439 * t441 * t46 / 0.32D2)
      ggbbH5n3e0 = t182 * t181 + t264 * t263 + t307 * t306 + t374 * t373
     # + t416 * t415 - t445 * t5 * t439 * t441 * t44 * t37 * t39 / 0.32D
     #2

      end function



      doubleprecision function ggbbH5n3em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = 0.1D1 - x4
      t30 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t29)
      t31 = t30 - t17
      t33 = 0.1D1 / x4
      t38 = 0.1D1 / x3
      t39 = t38 * t26
      t44 = t38 * t33
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t13)
      t58 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t62 = t10 * t13
      t64 = log(0.4D1 * t62)
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t76 = t64 ** 2
      t82 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t29)
      t83 = -t29
      t87 = log(-0.4D1 * t62 * x4 * t83)
      t91 = log(0.4D1 * t62 * x4)
      t101 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.2880D4 + 
     #t5 * t31 * t26 * t33 / 0.32D2 - t5 * t17 * t39 / 0.32D2 + t5 * t31
     # * t44 / 0.64D2 - (0.90D2 * t5 * (t6 - t50 * t17) - t24) * t38 / 0
     #.5760D4 - t5 * t58 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t64) * t5 
     #* t6 / 0.5760D4 - (0.180D3 * t70 - 0.30D2 * t72 + 0.180D3 * t64 * 
     #lh + 0.45D2 * t76) * t5 * t17 / 0.5760D4 + (0.90D2 * t5 * (t82 - t
     #87 * t30 - t6 + t91 * t17) - 0.180D3 * t22 * t31) * t33 / 0.5760D4
      t102 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t101)
      t104 = 0.1D1 - x1
      t105 = KAPPA2(t104, x2, 0.0D0, 0.10D1, z)
      t106 = s * t105
      t107 = -t104
      t108 = t1 * t107
      t110 = t1 * x1
      t112 = t105 ** 2
      t114 = t1 ** 2
      t119 = 0.1D1 / (-0.2D1 + t105)
      t120 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t104, x2, 0.0D0, 0.10D1)
      t122 = t107 ** 2
      t124 = t112 ** 2
      t128 = log(0.4D1 * t11 * t13 * t122 * t124)
      t130 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t104, x2, 0.0D0, 0.10D1)
      t141 = t5 * t119
      t150 = (-0.90D2 * t5 * (t119 * t120 - t128 * t119 * t130) + 0.180D
     #3 * t22 * t119 * t130) * t26 / 0.2880D4 - t141 * t130 * t26 * t33 
     #/ 0.32D2 - t141 * t130 * t38 * t26 / 0.32D2
      t151 = FJET(XB1, XB2, s, 0.0D0, -t106 * t108, t106 * t110, 0.0D0, 
     #-s * t112 * t114 * t107 * x1, t150)
      t153 = KAPPA2(t104, x2, 0.0D0, t29, z)
      t154 = s * t153
      t160 = t153 ** 2
      t163 = t107 * x1
      t167 = 0.1D1 / (-0.2D1 + t153)
      t169 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t104, x2, 0.0D0, t29)
      t171 = t169 * t26 * t33
      t174 = FJET(XB1, XB2, s, 0.0D0, -t154 * t108, -t154 * t110 * t83, 
     #t154 * t110 * x4, s * t160 * t114 * t163 * t83, t5 * t167 * t171 /
     # 0.32D2)
      t180 = -0.1D1 + x3
      t182 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t186 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t29)
      t191 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t195 = log(-0.4D1 * t47 * t13 * t180)
      t205 = t5 * t182 * t39 / 0.32D2 - t5 * (-t182 + t186) * t44 / 0.64
     #D2 - (-0.90D2 * t5 * (t191 - t195 * t182) + 0.180D3 * t22 * t182) 
     #* t38 / 0.5760D4
      t206 = FJET(XB1, XB2, s, t2 * x3, -t2 * t180, 0.0D0, 0.0D0, 0.0D0,
     # t205)
      t208 = KAPPA2(t104, x2, x3, 0.10D1, z)
      t209 = s * t208
      t215 = t208 ** 2
      t221 = 0.1D1 / (-0.2D1 + t208)
      t223 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t104, x2, x3, 0.10D1)
      t225 = t223 * t38 * t26
      t228 = FJET(XB1, XB2, s, -t209 * t108 * x3, t209 * t108 * t180, t2
     #09 * t110, 0.0D0, s * t215 * t114 * t163 * t180, t5 * t221 * t225 
     #/ 0.32D2)
      ggbbH5n3em1 = t102 * t101 + t151 * t150 + t174 * t5 * t167 * t171 
     #/ 0.32D2 + t206 * t205 + t228 * t5 * t221 * t225 / 0.32D2

      end function



      doubleprecision function ggbbH5n3em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t32 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.1D1 -
     # x4)
      t38 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 + t5 * (t32 - 
     #t6) / x4 / 0.64D2
      t39 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t41 = 0.1D1 - x1
      t42 = KAPPA2(t41, x2, 0.0D0, 0.10D1, z)
      t43 = s * t42
      t44 = -t41
      t49 = t42 ** 2
      t51 = t1 ** 2
      t56 = 0.1D1 / (-0.2D1 + t42)
      t58 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t41, x2, 0.0D0, 0.10D1)
      t62 = FJET(XB1, XB2, s, 0.0D0, -t43 * t1 * t44, t43 * t1 * x1, 0.0
     #D0, -s * t49 * t51 * t44 * x1, -t5 * t56 * t58 * t8 / 0.32D2)
      t71 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t75 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, t5 * t71 * t11 / 0.64D2)
      ggbbH5n3em2 = t39 * t38 - t62 * t5 * t56 * t58 * t8 / 0.32D2 + t75
     # * t5 * t71 * t11 / 0.64D2

      end function



      doubleprecision function ggbbH5n3em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t9 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n3em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n3em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n3em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n4e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t28 - t30
      t32 = t31 * t5
      t46 = 0.120D3 * t27 * lh
      t48 = 0.60D2 * lh * t29
      t49 = -0.2884936567583026D3 - t46 + t48
      t50 = t49 * t5
      t51 = t50 * t21
      t53 = 0.1D1 / x1
      t56 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t57 = t14 * x4
      t58 = -0.1D1 + x4
      t59 = t57 * t58
      t62 = log(-0.4D1 * t12 * t59)
      t63 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t67 = log(0.4D1 * t12 * t57)
      t73 = t67 ** 2
      t76 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t78 = t62 ** 2
      t84 = t21 - t63
      t88 = 0.1D1 / x4
      t91 = x3 * t8
      t92 = t91 * t11
      t95 = log(-0.4D1 * t92 * t59)
      t97 = t11 * t14
      t98 = t97 * x4
      t101 = log(0.4D1 * t91 * t98)
      t109 = 0.1D1 / x3
      t111 = t53 * t88
      t114 = t91 * t97
      t116 = log(0.4D1 * t114)
      t122 = t116 ** 2
      t134 = log(0.4D1 * t97)
      t137 = t134 ** 2
      t146 = t137 * t134
      t152 = t29 ** 2
      t153 = t27 ** 2
      t159 = t137 ** 2
      t169 = x4 * t58
      t172 = log(-0.4D1 * t97 * t169)
      t174 = t172 ** 2
      t178 = log(0.4D1 * t98)
      t180 = t178 ** 2
      t209 = x3 * t11
      t210 = t209 * t14
      t212 = log(0.4D1 * t210)
      t214 = t212 ** 2
      t237 = log(-0.4D1 * t209 * t59)
      t241 = log(0.4D1 * t209 * t57)
      t247 = t241 ** 2
      t251 = t237 ** 2
      t263 = (-0.180D3 * t6 * (-t7 + t17 * t18 - t20 * t21 / 0.2D1) + t3
     #2 * (-t18 + t17 * t21) + 0.90D2 * t5 * (t17 * t7 - t20 * t18 / 0.2
     #D1 + t20 * t17 * t21 / 0.6D1) - t51) * t53 / 0.2880D4 - (-0.180D3 
     #* t6 * (-t56 + t62 * t63 + t18 - t67 * t21) + 0.90D2 * t5 * (t7 - 
     #t67 * t18 + t73 * t21 / 0.2D1 - t76 + t62 * t56 - t78 * t63 / 0.2D
     #1) + t32 * t84) * t53 * t88 / 0.2880D4 - (0.90D2 * t5 * (-t56 + t9
     #5 * t63 + t18 - t101 * t21) - 0.180D3 * t6 * t84) * t109 * t111 / 
     #0.2880D4 - (-0.180D3 * t6 * (t18 - t116 * t21) + 0.90D2 * t5 * (t7
     # - t116 * t18 + t122 * t21 / 0.2D1) + t32 * t21) * t109 * t53 / 0.
     #2880D4 - (t28 - t30 + 0.180D3 * t134 * lh + 0.45D2 * t137) * t5 * 
     #t7 / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t134 * t31 - 
     #0.90D2 * t137 * lh - 0.15D2 * t146) * t5 * t18 / 0.5760D4 - (t152 
     #+ 0.60D2 * t153 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29 -
     # t134 * t49 + 0.15D2 / 0.4D1 * t159 + t137 * t31 / 0.2D1 + 0.30D2 
     #* t146 * lh) * t5 * t21 / 0.5760D4 - (-0.180D3 * t6 * (-t76 + t172
     # * t56 - t174 * t63 / 0.2D1 + t7 - t178 * t18 + t180 * t21 / 0.2D1
     #) + t32 * (t18 - t178 * t21 - t56 + t172 * t63) + 0.90D2 * t5 * (t
     #172 * t76 - t174 * t56 / 0.2D1 + t174 * t172 * t63 / 0.6D1 - t178 
     #* t7 + t180 * t18 / 0.2D1 - t180 * t178 * t21 / 0.6D1) + t50 * t84
     #) * t88 / 0.5760D4 + (-0.180D3 * t6 * (-t7 + t212 * t18 - t214 * t
     #21 / 0.2D1) + t32 * (-t18 + t212 * t21) + 0.90D2 * t5 * (t212 * t7
     # - t214 * t18 / 0.2D1 + t214 * t212 * t21 / 0.6D1) - t51) * t109 /
     # 0.5760D4 + (-0.180D3 * t6 * (t56 - t237 * t63 - t18 + t241 * t21)
     # + 0.90D2 * t5 * (-t7 + t241 * t18 - t247 * t21 / 0.2D1 + t76 - t2
     #37 * t56 + t251 * t63 / 0.2D1) - t32 * t84) * t109 * t88 / 0.5760D
     #4
      t264 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t263)
      t266 = -0.1D1 + x1
      t269 = -t266
      t270 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, 0.0D0)
      t271 = t266 ** 2
      t272 = t8 * t271
      t275 = log(0.4D1 * t97 * t272)
      t276 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, 0.0D0)
      t278 = t275 ** 2
      t279 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, 0.0D0)
      t300 = t272 * x4
      t303 = log(0.4D1 * t97 * t300)
      t309 = t303 ** 2
      t315 = t32 * t279
      t321 = log(0.4D1 * t210 * t300)
      t335 = log(0.4D1 * t209 * t14 * t8 * t271)
      t341 = t335 ** 2
      t351 = (-0.180D3 * t6 * (t270 - t275 * t276 + t278 * t279 / 0.2D1)
     # + t32 * (t276 - t275 * t279) + 0.90D2 * t5 * (-t275 * t270 + t278
     # * t276 / 0.2D1 - t278 * t275 * t279 / 0.6D1) + t50 * t279) * t53 
     #/ 0.2880D4 - (-0.180D3 * t6 * (-t276 + t303 * t279) + 0.90D2 * t5 
     #* (-t270 + t303 * t276 - t309 * t279 / 0.2D1) - t315) * t53 * t88 
     #/ 0.2880D4 - (0.90D2 * t5 * (-t276 + t321 * t279) + 0.180D3 * t6 *
     # t279) * t109 * t111 / 0.2880D4 - (-0.180D3 * t6 * (-t276 + t335 *
     # t279) + 0.90D2 * t5 * (-t270 + t335 * t276 - t341 * t279 / 0.2D1)
     # - t315) * t109 * t53 / 0.2880D4
      t352 = FJET(XB1, XB2, s, 0.0D0, -t2 * t266, 0.0D0, t2 * x1, 0.0D0,
     # t351)
      t354 = KAPPA2(t269, x2, 0.0D0, x4, z)
      t355 = s * t354
      t356 = t1 * t266
      t358 = t1 * x1
      t359 = t358 * x4
      t361 = t358 * t58
      t363 = t354 ** 2
      t365 = t1 ** 2
      t367 = t266 * x1
      t371 = 0.1D1 / (-0.2D1 + t354)
      t372 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, x4)
      t373 = t371 * t372
      t375 = t363 ** 2
      t377 = t271 * x4 * t58 * t375
      t380 = log(-0.4D1 * t15 * t377)
      t381 = t380 * t371
      t382 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, x4)
      t387 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, x4)
      t390 = t380 ** 2
      t397 = t371 * t382
      t404 = log(-0.4D1 * t114 * t377)
      t416 = -(0.180D3 * t6 * (t373 - t381 * t382) - 0.90D2 * t5 * (t371
     # * t387 - t381 * t372 + t390 * t371 * t382 / 0.2D1) - t32 * t397) 
     #* t53 * t88 / 0.2880D4 - (0.90D2 * t5 * (-t373 + t404 * t371 * t38
     #2) + 0.180D3 * t6 * t397) * t109 * t111 / 0.2880D4
      t417 = FJET(XB1, XB2, s, 0.0D0, -t355 * t356, t355 * t359, -t355 *
     # t361, -s * t363 * t365 * t367 * x4, t416)
      t420 = -0.1D1 + x3
      t422 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t427 = log(0.4D1 * t92 * t57 * t58 * t420)
      t428 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t430 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t431 = t14 * t420
      t432 = t431 * x4
      t435 = log(-0.4D1 * t92 * t432)
      t436 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t441 = t428 - t436
      t451 = log(-0.4D1 * t91 * t97 * t420)
      t456 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t458 = t451 ** 2
      t471 = log(-0.4D1 * t209 * t431)
      t473 = t471 ** 2
      t497 = log(-0.4D1 * t209 * t432)
      t502 = log(0.4D1 * t210 * t169 * t420)
      t507 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t509 = t502 ** 2
      t513 = t497 ** 2
      t525 = -(0.90D2 * t5 * (t422 - t427 * t428 - t430 + t435 * t436) -
     # 0.180D3 * t6 * t441) * t109 * t111 / 0.2880D4 - (-0.180D3 * t6 * 
     #(-t430 + t451 * t436) + 0.90D2 * t5 * (-t456 + t451 * t430 - t458 
     #* t436 / 0.2D1) - t32 * t436) * t109 * t53 / 0.2880D4 + (-0.180D3 
     #* t6 * (t456 - t471 * t430 + t473 * t436 / 0.2D1) + t32 * (t430 - 
     #t471 * t436) + 0.90D2 * t5 * (-t471 * t456 + t473 * t430 / 0.2D1 -
     # t473 * t471 * t436 / 0.6D1) + t50 * t436) * t109 / 0.5760D4 + (-0
     #.180D3 * t6 * (t430 - t497 * t436 - t422 + t502 * t428) + 0.90D2 *
     # t5 * (-t507 + t502 * t422 - t509 * t428 / 0.2D1 + t456 - t497 * t
     #430 + t513 * t436 / 0.2D1) - t32 * t441) * t109 * t88 / 0.5760D4
      t526 = FJET(XB1, XB2, s, t2 * x3, -t2 * t420, 0.0D0, 0.0D0, 0.0D0,
     # t525)
      t528 = KAPPA2(t269, x2, x3, 0.0D0, z)
      t529 = s * t528
      t530 = t356 * x3
      t532 = t356 * t420
      t535 = t528 ** 2
      t541 = 0.1D1 / (-0.2D1 + t528)
      t542 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t269, x2, x3, 0.0D0)
      t543 = t541 * t542
      t544 = t271 * t420
      t545 = t535 ** 2
      t550 = log(-0.4D1 * t114 * t544 * x4 * t545)
      t552 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t269, x2, x3, 0.0D0)
      t557 = t541 * t552
      t568 = log(-0.4D1 * t92 * t14 * t271 * t420 * t545)
      t569 = t568 * t541
      t574 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t269, x2, x3, 0.0D0)
      t577 = t568 ** 2
      t589 = -(0.90D2 * t5 * (-t543 + t550 * t541 * t552) + 0.180D3 * t6
     # * t557) * t109 * t111 / 0.2880D4 - (0.180D3 * t6 * (t543 - t569 *
     # t552) - 0.90D2 * t5 * (t541 * t574 - t569 * t542 + t577 * t541 * 
     #t552 / 0.2D1) - t32 * t557) * t109 * t53 / 0.2880D4
      t590 = FJET(XB1, XB2, s, -t529 * t530, t529 * t532, 0.0D0, t529 * 
     #t358, -s * t535 * t365 * t367 * x3, t589)
      t592 = KAPPA2(t269, x2, x3, x4, z)
      t593 = s * t592
      t598 = t592 ** 2
      t603 = cos(t9)
      t606 = sqrt(x3 * t420 * t169)
      t613 = 0.1D1 / (-0.2D1 + t592)
      t614 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t269, x2, x3, x4)
      t616 = t598 ** 2
      t621 = log(0.4D1 * t114 * t544 * t169 * t616)
      t623 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t269, x2, x3, x4)
      t631 = 0.90D2 * t5 * (t613 * t614 - t621 * t613 * t623) - 0.180D3 
     #* t6 * t613 * t623
      t635 = FJET(XB1, XB2, s, -t593 * t530, t593 * t532, t593 * t359, -
     #t593 * t361, s * t598 * t365 * t367 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t603 * t606), -t631 * t109 * t111 / 0.2880D4)
      ggbbH5n4e1 = t264 * t263 + t352 * t351 + t417 * t416 + t526 * t525
     # + t590 * t589 - t635 * t631 * t109 * t53 * t88 / 0.2880D4

      end function



      doubleprecision function ggbbH5n4e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t15 = -0.1D1 + x4
      t16 = t14 * t15
      t19 = log(-0.4D1 * t11 * t16)
      t20 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t22 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t25 = log(0.4D1 * t11 * t14)
      t26 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t31 = lh * t5
      t32 = t26 - t20
      t36 = 0.1D1 / x1
      t38 = 0.1D1 / x4
      t42 = 0.1D1 / x3
      t44 = t42 * t36 * t38
      t47 = x3 * t7
      t48 = t10 * t13
      t51 = log(0.4D1 * t47 * t48)
      t62 = t11 * t13
      t64 = log(0.4D1 * t62)
      t69 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t71 = t64 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t5
      t83 = t82 * t26
      t89 = log(0.4D1 * t48 * x4)
      t91 = x4 * t15
      t94 = log(-0.4D1 * t48 * t91)
      t99 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t101 = t94 ** 2
      t105 = t89 ** 2
      t115 = x3 * t10
      t118 = log(-0.4D1 * t115 * t16)
      t122 = log(0.4D1 * t115 * t14)
      t134 = t115 * t13
      t136 = log(0.4D1 * t134)
      t142 = t136 ** 2
      t153 = log(0.4D1 * t48)
      t161 = t153 ** 2
      t180 = -(0.90D2 * t5 * (-t6 + t19 * t20 + t22 - t25 * t26) - 0.180
     #D3 * t31 * t32) * t36 * t38 / 0.2880D4 - t5 * t32 * t44 / 0.32D2 -
     # (0.90D2 * t5 * (t22 - t51 * t26) - 0.180D3 * t31 * t26) * t42 * t
     #36 / 0.2880D4 + (-0.180D3 * t31 * (-t22 + t64 * t26) + 0.90D2 * t5
     # * (-t69 + t64 * t22 - t71 * t26 / 0.2D1) - t83) * t36 / 0.2880D4 
     #- (-0.180D3 * t31 * (t22 - t89 * t26 - t6 + t94 * t20) + 0.90D2 * 
     #t5 * (-t99 + t94 * t6 - t101 * t20 / 0.2D1 + t69 - t89 * t22 + t10
     #5 * t26 / 0.2D1) + t82 * t32) * t38 / 0.5760D4 + (0.90D2 * t5 * (t
     #6 - t118 * t20 - t22 + t122 * t26) + 0.180D3 * t31 * t32) * t42 * 
     #t38 / 0.5760D4 + (-0.180D3 * t31 * (-t22 + t136 * t26) + 0.90D2 * 
     #t5 * (-t69 + t136 * t22 - t142 * t26 / 0.2D1) - t83) * t42 / 0.576
     #0D4 - (-0.180D3 * lh - 0.90D2 * t153) * t5 * t69 / 0.5760D4 - (t78
     # - t80 + 0.180D3 * t153 * lh + 0.45D2 * t161) * t5 * t22 / 0.5760D
     #4 - (-0.2884936567583026D3 - 0.120D3 * t77 * lh + 0.60D2 * lh * t7
     #9 - t153 * t81 - 0.90D2 * t161 * lh - 0.15D2 * t161 * t153) * t5 *
     # t26 / 0.5760D4
      t181 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t180)
      t183 = -0.1D1 + x1
      t186 = -t183
      t187 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, 0.0D0)
      t188 = t183 ** 2
      t189 = t7 * t188
      t193 = log(0.4D1 * t48 * t189 * x4)
      t194 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, 0.0D0)
      t200 = 0.180D3 * t31 * t194
      t212 = log(0.4D1 * t115 * t13 * t7 * t188)
      t223 = log(0.4D1 * t48 * t189)
      t228 = ggbbH51J3(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, 0.0D0)
      t230 = t223 ** 2
      t240 = -(0.90D2 * t5 * (-t187 + t193 * t194) + t200) * t36 * t38 /
     # 0.2880D4 + t5 * t194 * t44 / 0.32D2 - (0.90D2 * t5 * (-t187 + t21
     #2 * t194) + t200) * t42 * t36 / 0.2880D4 + (-0.180D3 * t31 * (t187
     # - t223 * t194) + 0.90D2 * t5 * (t228 - t223 * t187 + t230 * t194 
     #/ 0.2D1) + t82 * t194) * t36 / 0.2880D4
      t241 = FJET(XB1, XB2, s, 0.0D0, -t2 * t183, 0.0D0, t2 * x1, 0.0D0,
     # t240)
      t243 = KAPPA2(t186, x2, 0.0D0, x4, z)
      t244 = s * t243
      t245 = t1 * t183
      t247 = t1 * x1
      t248 = t247 * x4
      t250 = t247 * t15
      t252 = t243 ** 2
      t254 = t1 ** 2
      t256 = t183 * x1
      t260 = 0.1D1 / (-0.2D1 + t243)
      t261 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, x4)
      t264 = t252 ** 2
      t269 = log(-0.4D1 * t62 * t188 * x4 * t15 * t264)
      t271 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, x4)
      t287 = -(-0.90D2 * t5 * (t260 * t261 - t269 * t260 * t271) + 0.180
     #D3 * t31 * t260 * t271) * t36 * t38 / 0.2880D4 + t5 * t260 * t271 
     #* t44 / 0.32D2
      t288 = FJET(XB1, XB2, s, 0.0D0, -t244 * t245, t244 * t248, -t244 *
     # t250, -s * t252 * t254 * t256 * x4, t287)
      t291 = -0.1D1 + x3
      t293 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t294 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t295 = t293 - t294
      t299 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t303 = log(-0.4D1 * t47 * t48 * t291)
      t314 = t13 * t291
      t318 = log(-0.4D1 * t115 * t314 * x4)
      t320 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t324 = log(0.4D1 * t134 * t91 * t291)
      t338 = log(-0.4D1 * t115 * t314)
      t343 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t345 = t338 ** 2
      t355 = -t5 * t295 * t44 / 0.32D2 - (0.90D2 * t5 * (-t299 + t303 * 
     #t294) + 0.180D3 * t31 * t294) * t42 * t36 / 0.2880D4 + (0.90D2 * t
     #5 * (t299 - t318 * t294 - t320 + t324 * t293) + 0.180D3 * t31 * t2
     #95) * t42 * t38 / 0.5760D4 + (-0.180D3 * t31 * (t299 - t338 * t294
     #) + 0.90D2 * t5 * (t343 - t338 * t299 + t345 * t294 / 0.2D1) + t82
     # * t294) * t42 / 0.5760D4
      t356 = FJET(XB1, XB2, s, t2 * x3, -t2 * t291, 0.0D0, 0.0D0, 0.0D0,
     # t355)
      t358 = KAPPA2(t186, x2, x3, 0.0D0, z)
      t359 = s * t358
      t360 = t245 * x3
      t362 = t245 * t291
      t365 = t358 ** 2
      t371 = 0.1D1 / (-0.2D1 + t358)
      t373 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t186, x2, x3, 0.0D0)
      t377 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t186, x2, x3, 0.0D0)
      t381 = t365 ** 2
      t386 = log(-0.4D1 * t47 * t10 * t13 * t188 * t291 * t381)
      t399 = t5 * t371 * t373 * t44 / 0.32D2 - (-0.90D2 * t5 * (t371 * t
     #377 - t386 * t371 * t373) + 0.180D3 * t31 * t371 * t373) * t42 * t
     #36 / 0.2880D4
      t400 = FJET(XB1, XB2, s, -t359 * t360, t359 * t362, 0.0D0, t359 * 
     #t247, -s * t365 * t254 * t256 * x3, t399)
      t402 = KAPPA2(t186, x2, x3, x4, z)
      t403 = s * t402
      t408 = t402 ** 2
      t413 = cos(t8)
      t416 = sqrt(x3 * t291 * t91)
      t423 = 0.1D1 / (-0.2D1 + t402)
      t425 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t186, x2, x3, x4)
      t429 = FJET(XB1, XB2, s, -t403 * t360, t403 * t362, t403 * t248, -
     #t403 * t250, s * t408 * t254 * t256 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t413 * t416), -t5 * t423 * t425 * t44 / 0.32D2)
      ggbbH5n4e0 = t181 * t180 + t241 * t240 + t288 * t287 + t355 * t356
     # + t400 * t399 - t429 * t5 * t423 * t425 * t42 * t36 * t38 / 0.32D
     #2

      end function



      doubleprecision function ggbbH5n4em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t7 * t10 * t13)
      t17 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t30 = t17 - t29
      t32 = 0.1D1 / x4
      t33 = t26 * t32
      t37 = 0.1D1 / x3
      t38 = t37 * t26
      t43 = t37 * t32
      t46 = x3 * t10
      t49 = log(0.4D1 * t46 * t13)
      t57 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t61 = t10 * t13
      t63 = log(0.4D1 * t61)
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t75 = t63 ** 2
      t83 = log(0.4D1 * t61 * x4)
      t85 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t86 = -0.1D1 + x4
      t90 = log(-0.4D1 * t61 * x4 * t86)
      t100 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.2880D4 - 
     #t5 * t30 * t33 / 0.32D2 - t5 * t17 * t38 / 0.32D2 - t5 * t30 * t43
     # / 0.64D2 + (0.90D2 * t5 * (-t6 + t49 * t17) + t24) * t37 / 0.5760
     #D4 - t5 * t57 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t63) * t5 * t6 
     #/ 0.5760D4 - (0.180D3 * t69 - 0.30D2 * t71 + 0.180D3 * t63 * lh + 
     #0.45D2 * t75) * t5 * t17 / 0.5760D4 - (0.90D2 * t5 * (t6 - t83 * t
     #17 - t85 + t90 * t29) - 0.180D3 * t22 * t30) * t32 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t100)
      t103 = -0.1D1 + x1
      t106 = -t103
      t107 = ggbbH51J2(s, XB1, XB2, z, lh, wd, t106, x2, 0.0D0, 0.0D0)
      t108 = t103 ** 2
      t112 = log(0.4D1 * t61 * t7 * t108)
      t113 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t106, x2, 0.0D0, 0.0D0)
      t123 = t5 * t113
      t128 = (0.90D2 * t5 * (t107 - t112 * t113) - 0.180D3 * t22 * t113)
     # * t26 / 0.2880D4 + t123 * t33 / 0.32D2 + t123 * t38 / 0.32D2
      t129 = FJET(XB1, XB2, s, 0.0D0, -t2 * t103, 0.0D0, t2 * x1, 0.0D0,
     # t128)
      t131 = KAPPA2(t106, x2, 0.0D0, x4, z)
      t132 = s * t131
      t133 = t1 * t103
      t135 = t1 * x1
      t140 = t131 ** 2
      t142 = t1 ** 2
      t144 = t103 * x1
      t148 = 0.1D1 / (-0.2D1 + t131)
      t150 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t106, x2, 0.0D0, x4)
      t152 = t150 * t26 * t32
      t155 = FJET(XB1, XB2, s, 0.0D0, -t132 * t133, t132 * t135 * x4, -t
     #132 * t135 * t86, -s * t140 * t142 * t144 * x4, t5 * t148 * t152 /
     # 0.32D2)
      t161 = -0.1D1 + x3
      t163 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t167 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t172 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t176 = log(-0.4D1 * t46 * t13 * t161)
      t186 = t5 * t163 * t38 / 0.32D2 + t5 * (t163 - t167) * t43 / 0.64D
     #2 + (0.90D2 * t5 * (t172 - t176 * t163) - 0.180D3 * t22 * t163) * 
     #t37 / 0.5760D4
      t187 = FJET(XB1, XB2, s, t2 * x3, -t2 * t161, 0.0D0, 0.0D0, 0.0D0,
     # t186)
      t189 = KAPPA2(t106, x2, x3, 0.0D0, z)
      t190 = s * t189
      t196 = t189 ** 2
      t202 = 0.1D1 / (-0.2D1 + t189)
      t204 = ggbbH51J1(s, XB1, XB2, z, lh, wd, t106, x2, x3, 0.0D0)
      t206 = t204 * t37 * t26
      t209 = FJET(XB1, XB2, s, -t190 * t133 * x3, t190 * t133 * t161, 0.
     #0D0, t190 * t135, -s * t196 * t142 * t144 * x3, t5 * t202 * t206 /
     # 0.32D2)
      ggbbH5n4em1 = t101 * t100 + t129 * t128 + t155 * t5 * t148 * t152 
     #/ 0.32D2 + t187 * t186 + t209 * t5 * t202 * t206 / 0.32D2

      end function



      doubleprecision function ggbbH5n4em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t31 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t37 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 - t5 * (t6 - t
     #31) / x4 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t37)
      t40 = -0.1D1 + x1
      t44 = ggbbH51J1(s, XB1, XB2, z, lh, wd, -t40, x2, 0.0D0, 0.0D0)
      t48 = FJET(XB1, XB2, s, 0.0D0, -t2 * t40, 0.0D0, t2 * x1, 0.0D0, t
     #5 * t44 * t8 / 0.32D2)
      t56 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t60 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, t5 * t56 * t11 / 0.64D2)
      ggbbH5n4em2 = t38 * t37 + t48 * t5 * t44 * t8 / 0.32D2 + t60 * t5 
     #* t56 * t11 / 0.64D2

      end function



      doubleprecision function ggbbH5n4em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t9 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n4em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n4em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n4em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n5e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t14 = log(0.4D1 * t12)
      t17 = t14 ** 2
      t20 = s ** 2
      t22 = 0.1D1 / t20 / s
      t24 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t61 = lh * t22
      t62 = t12 * x4
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t72 = t31 * t22
      t85 = t48 * t22
      t86 = t85 * t58
      t88 = 0.1D1 / x4
      t91 = x1 ** 2
      t92 = t91 * t9
      t93 = t92 * t11
      t95 = log(0.4D1 * t93)
      t97 = t95 ** 2
      t116 = 0.1D1 / x1
      t119 = t11 * x4
      t122 = log(0.4D1 * t92 * t119)
      t128 = t122 ** 2
      t139 = 0.1D1 - x3
      t140 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, 0.10D1)
      t141 = x3 * t91
      t142 = t141 * t9
      t143 = -t139
      t144 = t11 * t143
      t145 = t144 * x4
      t148 = log(-0.4D1 * t142 * t145)
      t149 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, 0.10D1)
      t153 = log(0.4D1 * t141 * t62)
      t158 = t58 - t149
      t162 = 0.1D1 / x3
      t164 = t116 * t88
      t167 = t141 * t12
      t169 = log(0.4D1 * t167)
      t174 = log(-0.4D1 * t141 * t12 * t143)
      t180 = t169 ** 2
      t183 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, 0.10D1)
      t185 = t174 ** 2
      t191 = t72 * t158
      t196 = x3 * t9
      t199 = log(-0.4D1 * t196 * t144)
      t201 = t199 ** 2
      t204 = t196 * t11
      t206 = log(0.4D1 * t204)
      t208 = t206 ** 2
      t239 = log(0.4D1 * t196 * t119)
      t243 = log(-0.4D1 * t196 * t145)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t263 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 - (-0.180D3 * t61 * (t24 - t64 * t39 + t66 
     #* t58 / 0.2D1) + t72 * (t39 - t64 * t58) + 0.90D2 * t22 * (-t64 * 
     #t24 + t66 * t39 / 0.2D1 - t66 * t64 * t58 / 0.6D1) + t86) * t88 / 
     #0.5760D4 + (-0.180D3 * t61 * (-t24 + t95 * t39 - t97 * t58 / 0.2D1
     #) + t72 * (-t39 + t95 * t58) + 0.90D2 * t22 * (t95 * t24 - t97 * t
     #39 / 0.2D1 + t97 * t95 * t58 / 0.6D1) - t86) * t116 / 0.2880D4 - (
     #-0.180D3 * t61 * (t39 - t122 * t58) + 0.90D2 * t22 * (t24 - t122 *
     # t39 + t128 * t58 / 0.2D1) + t72 * t58) * t116 * t88 / 0.2880D4 - 
     #(0.90D2 * t22 * (-t140 + t148 * t149 + t39 - t153 * t58) - 0.180D3
     # * t61 * t158) * t162 * t164 / 0.2880D4 - (-0.180D3 * t61 * (t39 -
     # t169 * t58 - t140 + t174 * t149) + 0.90D2 * t22 * (t24 - t169 * t
     #39 + t180 * t58 / 0.2D1 - t183 + t174 * t140 - t185 * t149 / 0.2D1
     #) + t191) * t162 * t116 / 0.2880D4 - (-0.180D3 * t61 * (-t183 + t1
     #99 * t140 - t201 * t149 / 0.2D1 + t24 - t206 * t39 + t208 * t58 / 
     #0.2D1) + t72 * (t39 - t206 * t58 - t140 + t199 * t149) + 0.90D2 * 
     #t22 * (t199 * t183 - t201 * t140 / 0.2D1 + t201 * t199 * t149 / 0.
     #6D1 - t206 * t24 + t208 * t39 / 0.2D1 - t208 * t206 * t58 / 0.6D1)
     # + t85 * t158) * t162 / 0.5760D4 - (-0.180D3 * t61 * (t39 - t239 *
     # t58 - t140 + t243 * t149) + 0.90D2 * t22 * (t24 - t239 * t39 + t2
     #49 * t58 / 0.2D1 - t183 + t243 * t140 - t253 * t149 / 0.2D1) + t19
     #1) * t162 * t88 / 0.5760D4
      t264 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t263)
      t266 = -0.1D1 + x4
      t269 = -t266
      t270 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t269)
      t271 = t119 * t266
      t274 = log(-0.4D1 * t92 * t271)
      t275 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t269)
      t280 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t269)
      t282 = t274 ** 2
      t293 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, t269)
      t298 = log(0.4D1 * t142 * t119 * t266 * t143)
      t299 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, t269)
      t303 = log(-0.4D1 * t142 * t271)
      t308 = -t275 + t299
      t315 = x4 * t266
      t319 = log(0.4D1 * t204 * t315 * t143)
      t323 = log(-0.4D1 * t196 * t271)
      t328 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, t269)
      t330 = t319 ** 2
      t334 = t323 ** 2
      t347 = log(-0.4D1 * t12 * t315)
      t349 = t347 ** 2
      t371 = -(-0.180D3 * t61 * (-t270 + t274 * t275) + 0.90D2 * t22 * (
     #-t280 + t274 * t270 - t282 * t275 / 0.2D1) - t72 * t275) * t116 * 
     #t88 / 0.2880D4 - (0.90D2 * t22 * (t293 - t298 * t299 - t270 + t303
     # * t275) - 0.180D3 * t61 * t308) * t162 * t164 / 0.2880D4 - (-0.18
     #0D3 * t61 * (t293 - t319 * t299 - t270 + t323 * t275) + 0.90D2 * t
     #22 * (t328 - t319 * t293 + t330 * t299 / 0.2D1 - t280 + t323 * t27
     #0 - t334 * t275 / 0.2D1) + t72 * t308) * t162 * t88 / 0.5760D4 - (
     #0.180D3 * t61 * (t280 - t347 * t270 + t349 * t275 / 0.2D1) - t72 *
     # (t270 - t347 * t275) - 0.90D2 * t22 * (-t347 * t280 + t349 * t270
     # / 0.2D1 - t349 * t347 * t275 / 0.6D1) - t85 * t275) * t88 / 0.576
     #0D4
      t372 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t266, t2 * x4, 0.0D0,
     # t371)
      t375 = -0.1D1 + x1
      t377 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t378 = t375 ** 2
      t379 = t91 * t378
      t382 = log(0.4D1 * t12 * t379)
      t383 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t385 = t382 ** 2
      t386 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t407 = t379 * x4
      t410 = log(0.4D1 * t12 * t407)
      t416 = t410 ** 2
      t422 = t72 * t386
      t428 = log(0.4D1 * t204 * t407)
      t442 = log(0.4D1 * t196 * t11 * t91 * t378)
      t448 = t442 ** 2
      t458 = (-0.180D3 * t61 * (t377 - t382 * t383 + t385 * t386 / 0.2D1
     #) + t72 * (t383 - t382 * t386) + 0.90D2 * t22 * (-t382 * t377 + t3
     #85 * t383 / 0.2D1 - t385 * t382 * t386 / 0.6D1) + t85 * t386) * t1
     #16 / 0.2880D4 - (-0.180D3 * t61 * (-t383 + t410 * t386) + 0.90D2 *
     # t22 * (-t377 + t410 * t383 - t416 * t386 / 0.2D1) - t422) * t116 
     #* t88 / 0.2880D4 - (0.90D2 * t22 * (-t383 + t428 * t386) + 0.180D3
     # * t61 * t386) * t162 * t164 / 0.2880D4 - (-0.180D3 * t61 * (-t383
     # + t442 * t386) + 0.90D2 * t22 * (-t377 + t442 * t383 - t448 * t38
     #6 / 0.2D1) - t422) * t162 * t116 / 0.2880D4
      t459 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t375, 0.0D0, 0.0D0,
     # t458)
      t461 = KAPPA2(x1, x2, 0.10D1, t269, z)
      t462 = s * t461
      t463 = t1 * x1
      t465 = t1 * t375
      t466 = t465 * t266
      t468 = t465 * x4
      t470 = t461 ** 2
      t472 = t1 ** 2
      t474 = x1 * t375
      t478 = 0.1D1 / (-0.2D1 + t461)
      t479 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t269)
      t480 = t478 * t479
      t482 = t470 ** 2
      t484 = t378 * x4 * t266 * t482
      t487 = log(-0.4D1 * t93 * t484)
      t488 = t487 * t478
      t489 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t269)
      t494 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t269)
      t497 = t487 ** 2
      t504 = t478 * t489
      t511 = log(-0.4D1 * t167 * t484)
      t523 = -(0.180D3 * t61 * (t480 - t488 * t489) - 0.90D2 * t22 * (t4
     #78 * t494 - t488 * t479 + t497 * t478 * t489 / 0.2D1) - t72 * t504
     #) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (-t480 + t511 * t478 *
     # t489) + 0.180D3 * t61 * t504) * t162 * t164 / 0.2880D4
      t524 = FJET(XB1, XB2, s, t462 * t463, 0.0D0, t462 * t466, -t462 * 
     #t468, -s * t470 * t472 * t474 * x4, t523)
      t526 = KAPPA2(x1, x2, t139, 0.10D1, z)
      t527 = s * t526
      t528 = t463 * t143
      t530 = t463 * x3
      t533 = t526 ** 2
      t539 = 0.1D1 / (-0.2D1 + t526)
      t540 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t139, 0.10D1)
      t541 = t539 * t540
      t542 = t378 * t143
      t543 = t533 ** 2
      t548 = log(-0.4D1 * t167 * t542 * x4 * t543)
      t550 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t139, 0.10D1)
      t555 = t539 * t550
      t566 = log(-0.4D1 * t142 * t11 * t378 * t143 * t543)
      t567 = t566 * t539
      t572 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, t139, 0.10D1)
      t575 = t566 ** 2
      t587 = -(0.90D2 * t22 * (-t541 + t548 * t539 * t550) + 0.180D3 * t
     #61 * t555) * t162 * t164 / 0.2880D4 - (0.180D3 * t61 * (t541 - t56
     #7 * t550) - 0.90D2 * t22 * (t539 * t572 - t567 * t540 + t575 * t53
     #9 * t550 / 0.2D1) - t72 * t555) * t162 * t116 / 0.2880D4
      t588 = FJET(XB1, XB2, s, -t527 * t528, t527 * t530, -t527 * t465, 
     #0.0D0, -s * t533 * t472 * t474 * x3, t587)
      t590 = KAPPA2(x1, x2, t139, t269, z)
      t591 = s * t590
      t596 = t590 ** 2
      t601 = cos(t7)
      t604 = sqrt(x3 * t143 * t315)
      t611 = 0.1D1 / (-0.2D1 + t590)
      t612 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t139, t269)
      t614 = t596 ** 2
      t619 = log(0.4D1 * t167 * t542 * t315 * t614)
      t621 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t139, t269)
      t629 = -0.90D2 * t22 * (-t611 * t612 + t619 * t611 * t621) - 0.180
     #D3 * t61 * t611 * t621
      t633 = FJET(XB1, XB2, s, -t591 * t528, t591 * t530, t591 * t466, -
     #t591 * t468, s * t596 * t472 * t474 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t601 * t604), -t629 * t162 * t164 / 0.2880D4)
      ggbbH5n5e1 = t264 * t263 + t371 * t372 + t459 * t458 + t524 * t523
     # + t588 * t587 - t633 * t629 * t162 * t116 * t88 / 0.2880D4

      end function



      doubleprecision function ggbbH5n5e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = 0.1D1 - x3
      t33 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.10D1)
      t34 = t18 - t33
      t36 = 0.1D1 / x3
      t38 = t36 * t27 * t29
      t41 = x3 * t7
      t42 = t10 * t13
      t45 = log(0.4D1 * t41 * t42)
      t47 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.10D1)
      t48 = -t32
      t52 = log(-0.4D1 * t41 * t42 * t48)
      t58 = 0.180D3 * t23 * t34
      t63 = t11 * t13
      t65 = log(0.4D1 * t63)
      t70 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t18
      t90 = log(0.4D1 * t42 * x4)
      t96 = t90 ** 2
      t105 = x3 * t10
      t108 = log(0.4D1 * t105 * t14)
      t110 = t13 * t48
      t114 = log(-0.4D1 * t105 * t110 * x4)
      t123 = t105 * t13
      t125 = log(0.4D1 * t123)
      t129 = log(-0.4D1 * t105 * t110)
      t134 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.10D1)
      t136 = t129 ** 2
      t140 = t125 ** 2
      t152 = log(0.4D1 * t42)
      t160 = t152 ** 2
      t179 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t5 * t34 * t38 / 0.32D2 - (0.90D2 * t5 * (t6
     # - t45 * t18 - t47 + t52 * t33) - t58) * t36 * t27 / 0.2880D4 + (-
     #0.180D3 * t23 * (-t6 + t65 * t18) + 0.90D2 * t5 * (-t70 + t65 * t6
     # - t72 * t18 / 0.2D1) - t84) * t27 / 0.2880D4 - (-0.180D3 * t23 * 
     #(t6 - t90 * t18) + 0.90D2 * t5 * (t70 - t90 * t6 + t96 * t18 / 0.2
     #D1) + t84) * t29 / 0.5760D4 - (0.90D2 * t5 * (t6 - t108 * t18 - t4
     #7 + t114 * t33) - t58) * t36 * t29 / 0.5760D4 - (-0.180D3 * t23 * 
     #(t6 - t125 * t18 - t47 + t129 * t33) + 0.90D2 * t5 * (-t134 + t129
     # * t47 - t136 * t33 / 0.2D1 + t70 - t125 * t6 + t140 * t18 / 0.2D1
     #) + t83 * t34) * t36 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t152) 
     #* t5 * t70 / 0.5760D4 - (t79 - t81 + 0.180D3 * t152 * lh + 0.45D2 
     #* t160) * t5 * t6 / 0.5760D4 - (-0.2884936567583026D3 - 0.120D3 * 
     #t78 * lh + 0.60D2 * lh * t80 - t152 * t82 - 0.90D2 * t160 * lh - 0
     #.15D2 * t160 * t152) * t5 * t18 / 0.5760D4
      t180 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t179)
      t182 = -0.1D1 + x4
      t185 = -t182
      t186 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t185)
      t187 = t14 * t182
      t190 = log(-0.4D1 * t11 * t187)
      t191 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t185)
      t202 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, t185)
      t203 = -t191 + t202
      t207 = x4 * t182
      t210 = log(-0.4D1 * t42 * t207)
      t215 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t185)
      t217 = t210 ** 2
      t227 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, t185)
      t231 = log(0.4D1 * t123 * t207 * t48)
      t235 = log(-0.4D1 * t105 * t187)
      t246 = -(0.90D2 * t5 * (-t186 + t190 * t191) + 0.180D3 * t23 * t19
     #1) * t27 * t29 / 0.2880D4 - t5 * t203 * t38 / 0.32D2 - (0.180D3 * 
     #t23 * (t186 - t210 * t191) - 0.90D2 * t5 * (t215 - t210 * t186 + t
     #217 * t191 / 0.2D1) - t83 * t191) * t29 / 0.5760D4 - (0.90D2 * t5 
     #* (t227 - t231 * t202 - t186 + t235 * t191) - 0.180D3 * t23 * t203
     #) * t36 * t29 / 0.5760D4
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t182, t2 * x4, 0.0D0,
     # t246)
      t250 = -0.1D1 + x1
      t252 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t253 = t250 ** 2
      t254 = t7 * t253
      t258 = log(0.4D1 * t42 * t254 * x4)
      t259 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t265 = 0.180D3 * t23 * t259
      t277 = log(0.4D1 * t105 * t13 * t7 * t253)
      t288 = log(0.4D1 * t42 * t254)
      t293 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t295 = t288 ** 2
      t305 = -(0.90D2 * t5 * (-t252 + t258 * t259) + t265) * t27 * t29 /
     # 0.2880D4 + t5 * t259 * t38 / 0.32D2 - (0.90D2 * t5 * (-t252 + t27
     #7 * t259) + t265) * t36 * t27 / 0.2880D4 + (-0.180D3 * t23 * (t252
     # - t288 * t259) + 0.90D2 * t5 * (t293 - t288 * t252 + t295 * t259 
     #/ 0.2D1) + t83 * t259) * t27 / 0.2880D4
      t306 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t250, 0.0D0, 0.0D0,
     # t305)
      t308 = KAPPA2(x1, x2, 0.10D1, t185, z)
      t309 = s * t308
      t310 = t1 * x1
      t312 = t1 * t250
      t313 = t312 * t182
      t315 = t312 * x4
      t317 = t308 ** 2
      t319 = t1 ** 2
      t321 = x1 * t250
      t325 = 0.1D1 / (-0.2D1 + t308)
      t326 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t185)
      t329 = t317 ** 2
      t334 = log(-0.4D1 * t63 * t253 * x4 * t182 * t329)
      t336 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t185)
      t352 = -(-0.90D2 * t5 * (t325 * t326 - t334 * t325 * t336) + 0.180
     #D3 * t23 * t325 * t336) * t27 * t29 / 0.2880D4 + t5 * t325 * t336 
     #* t38 / 0.32D2
      t353 = FJET(XB1, XB2, s, t309 * t310, 0.0D0, t309 * t313, -t309 * 
     #t315, -s * t317 * t319 * t321 * x4, t352)
      t355 = KAPPA2(x1, x2, t32, 0.10D1, z)
      t356 = s * t355
      t357 = t310 * t48
      t359 = t310 * x3
      t362 = t355 ** 2
      t368 = 0.1D1 / (-0.2D1 + t355)
      t370 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.10D1)
      t374 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.10D1)
      t378 = t362 ** 2
      t383 = log(-0.4D1 * t41 * t10 * t13 * t253 * t48 * t378)
      t396 = t5 * t368 * t370 * t38 / 0.32D2 - (-0.90D2 * t5 * (t368 * t
     #374 - t383 * t368 * t370) + 0.180D3 * t23 * t368 * t370) * t36 * t
     #27 / 0.2880D4
      t397 = FJET(XB1, XB2, s, -t356 * t357, t356 * t359, -t356 * t312, 
     #0.0D0, -s * t362 * t319 * t321 * x3, t396)
      t399 = KAPPA2(x1, x2, t32, t185, z)
      t400 = s * t399
      t405 = t399 ** 2
      t410 = cos(t8)
      t413 = sqrt(x3 * t48 * t207)
      t420 = 0.1D1 / (-0.2D1 + t399)
      t422 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, t185)
      t426 = FJET(XB1, XB2, s, -t400 * t357, t400 * t359, t400 * t313, -
     #t400 * t315, s * t405 * t319 * t321 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t410 * t413), -t5 * t420 * t422 * t38 / 0.32D2)
      ggbbH5n5e0 = t180 * t179 + t247 * t246 + t306 * t305 + t353 * t352
     # + t397 * t396 - t426 * t5 * t420 * t422 * t36 * t27 * t29 / 0.32D
     #2

      end function



      doubleprecision function ggbbH5n5em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t7 = 0.1D1 - x3
      t8 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, 0.10D1)
      t9 = t6 - t8
      t10 = t5 * t9
      t11 = 0.1D1 / x3
      t12 = 0.1D1 / x4
      t13 = t11 * t12
      t16 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t18 = sin(x2 * 0.3141592653589793D1)
      t19 = t18 ** 2
      t20 = x3 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t25 = log(0.4D1 * t20 * t22)
      t27 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, 0.10D1)
      t28 = -t7
      t32 = log(-0.4D1 * t20 * t22 * t28)
      t37 = lh * t5
      t43 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t47 = t19 * t22
      t49 = log(0.4D1 * t47)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t61 = t49 ** 2
      t67 = x1 ** 2
      t71 = log(0.4D1 * t67 * t19 * t22)
      t77 = 0.180D3 * t37 * t6
      t79 = 0.1D1 / x1
      t83 = t79 * t12
      t86 = t11 * t79
      t91 = log(0.4D1 * t47 * x4)
      t99 = -t10 * t13 / 0.64D2 - (0.90D2 * t5 * (t16 - t25 * t6 - t27 +
     # t32 * t8) - 0.180D3 * t37 * t9) * t11 / 0.5760D4 - t5 * t43 / 0.6
     #4D2 - (-0.180D3 * lh - 0.90D2 * t49) * t5 * t16 / 0.5760D4 - (0.18
     #0D3 * t55 - 0.30D2 * t57 + 0.180D3 * t49 * lh + 0.45D2 * t61) * t5
     # * t6 / 0.5760D4 + (0.90D2 * t5 * (-t16 + t71 * t6) + t77) * t79 /
     # 0.2880D4 - t5 * t6 * t83 / 0.32D2 - t10 * t86 / 0.32D2 - (0.90D2 
     #* t5 * (t16 - t91 * t6) - t77) * t12 / 0.5760D4
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t99)
      t102 = -0.1D1 + x4
      t105 = -t102
      t106 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t105)
      t110 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t105)
      t114 = log(-0.4D1 * t47 * x4 * t102)
      t124 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, t105)
      t129 = t5 * t106 * t83 / 0.32D2 - (-0.90D2 * t5 * (t110 - t114 * t
     #106) + 0.180D3 * t37 * t106) * t12 / 0.5760D4 - t5 * (-t106 + t124
     #) * t13 / 0.64D2
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t102, t2 * x4, 0.0D0,
     # t129)
      t133 = -0.1D1 + x1
      t135 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t136 = t133 ** 2
      t140 = log(0.4D1 * t47 * t67 * t136)
      t141 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t151 = t5 * t141
      t156 = (0.90D2 * t5 * (t135 - t140 * t141) - 0.180D3 * t37 * t141)
     # * t79 / 0.2880D4 + t151 * t83 / 0.32D2 + t151 * t86 / 0.32D2
      t157 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t133, 0.0D0, 0.0D0,
     # t156)
      t159 = KAPPA2(x1, x2, 0.10D1, t105, z)
      t160 = s * t159
      t161 = t1 * x1
      t163 = t1 * t133
      t168 = t159 ** 2
      t170 = t1 ** 2
      t172 = x1 * t133
      t176 = 0.1D1 / (-0.2D1 + t159)
      t178 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t105)
      t180 = t178 * t79 * t12
      t183 = FJET(XB1, XB2, s, t160 * t161, 0.0D0, t160 * t163 * t102, -
     #t160 * t163 * x4, -s * t168 * t170 * t172 * x4, t5 * t176 * t180 /
     # 0.32D2)
      t188 = KAPPA2(x1, x2, t7, 0.10D1, z)
      t189 = s * t188
      t195 = t188 ** 2
      t201 = 0.1D1 / (-0.2D1 + t188)
      t203 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t7, 0.10D1)
      t205 = t203 * t11 * t79
      t208 = FJET(XB1, XB2, s, -t189 * t161 * t28, t189 * t161 * x3, -t1
     #89 * t163, 0.0D0, -s * t195 * t170 * t172 * x3, t5 * t201 * t205 /
     # 0.32D2)
      ggbbH5n5em1 = t100 * t99 + t130 * t129 + t157 * t156 + t183 * t5 *
     # t176 * t180 / 0.32D2 + t208 * t5 * t201 * t205 / 0.32D2

      end function



      doubleprecision function ggbbH5n5em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t12 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, 0.1
     #0D1)
      t18 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t23 = sin(x2 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t25 = z ** 2
      t29 = log(0.4D1 * t24 / t25)
      t35 = 0.1D1 / x4
      t38 = -t7 * t8 / 0.32D2 - t5 * (t6 - t12) / x3 / 0.64D2 - t5 * t18
     # / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t29) * t5 * t6 / 0.5760D4 - 
     #t7 * t35 / 0.64D2
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t44 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t48 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, t5 * t44 * t8 / 0.32D2)
      t53 = -0.1D1 + x4
      t57 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, -t53)
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t53, t2 * x4, 0.0D0, t
     #5 * t57 * t35 / 0.64D2)
      ggbbH5n5em2 = t39 * t38 + t48 * t5 * t44 * t8 / 0.32D2 + t61 * t5 
     #* t57 * t35 / 0.64D2

      end function



      doubleprecision function ggbbH5n5em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n5em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n5em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n5em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n6e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t14 = log(0.4D1 * t12)
      t17 = t14 ** 2
      t20 = s ** 2
      t22 = 0.1D1 / t20 / s
      t24 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t61 = lh * t22
      t62 = t12 * x4
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t72 = t31 * t22
      t85 = t48 * t22
      t86 = t85 * t58
      t88 = 0.1D1 / x4
      t91 = x1 ** 2
      t92 = t91 * t9
      t93 = t92 * t11
      t95 = log(0.4D1 * t93)
      t97 = t95 ** 2
      t116 = 0.1D1 / x1
      t119 = t11 * x4
      t122 = log(0.4D1 * t92 * t119)
      t128 = t122 ** 2
      t139 = x3 * t91
      t142 = log(0.4D1 * t139 * t62)
      t144 = 0.1D1 - x3
      t145 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, 0.0D0)
      t146 = t139 * t9
      t147 = -t144
      t148 = t11 * t147
      t149 = t148 * x4
      t152 = log(-0.4D1 * t146 * t149)
      t153 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, 0.0D0)
      t158 = t58 - t153
      t162 = 0.1D1 / x3
      t164 = t116 * t88
      t167 = t139 * t12
      t169 = log(0.4D1 * t167)
      t174 = log(-0.4D1 * t139 * t12 * t147)
      t180 = t169 ** 2
      t183 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, 0.0D0)
      t185 = t174 ** 2
      t191 = t72 * t158
      t196 = x3 * t9
      t199 = log(-0.4D1 * t196 * t148)
      t201 = t199 ** 2
      t204 = t196 * t11
      t206 = log(0.4D1 * t204)
      t208 = t206 ** 2
      t239 = log(-0.4D1 * t196 * t149)
      t243 = log(0.4D1 * t196 * t119)
      t249 = t243 ** 2
      t253 = t239 ** 2
      t263 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 - (-0.180D3 * t61 * (t24 - t64 * t39 + t66 
     #* t58 / 0.2D1) + t72 * (t39 - t64 * t58) + 0.90D2 * t22 * (-t64 * 
     #t24 + t66 * t39 / 0.2D1 - t66 * t64 * t58 / 0.6D1) + t86) * t88 / 
     #0.5760D4 + (-0.180D3 * t61 * (-t24 + t95 * t39 - t97 * t58 / 0.2D1
     #) + t72 * (-t39 + t95 * t58) + 0.90D2 * t22 * (t95 * t24 - t97 * t
     #39 / 0.2D1 + t97 * t95 * t58 / 0.6D1) - t86) * t116 / 0.2880D4 - (
     #-0.180D3 * t61 * (t39 - t122 * t58) + 0.90D2 * t22 * (t24 - t122 *
     # t39 + t128 * t58 / 0.2D1) + t72 * t58) * t116 * t88 / 0.2880D4 - 
     #(0.90D2 * t22 * (t39 - t142 * t58 - t145 + t152 * t153) - 0.180D3 
     #* t61 * t158) * t162 * t164 / 0.2880D4 - (-0.180D3 * t61 * (t39 - 
     #t169 * t58 - t145 + t174 * t153) + 0.90D2 * t22 * (t24 - t169 * t3
     #9 + t180 * t58 / 0.2D1 - t183 + t174 * t145 - t185 * t153 / 0.2D1)
     # + t191) * t162 * t116 / 0.2880D4 - (-0.180D3 * t61 * (-t183 + t19
     #9 * t145 - t201 * t153 / 0.2D1 + t24 - t206 * t39 + t208 * t58 / 0
     #.2D1) + t72 * (t39 - t206 * t58 - t145 + t199 * t153) + 0.90D2 * t
     #22 * (t199 * t183 - t201 * t145 / 0.2D1 + t201 * t199 * t153 / 0.6
     #D1 - t206 * t24 + t208 * t39 / 0.2D1 - t208 * t206 * t58 / 0.6D1) 
     #+ t85 * t158) * t162 / 0.5760D4 - (-0.180D3 * t61 * (-t145 + t239 
     #* t153 + t39 - t243 * t58) + 0.90D2 * t22 * (t24 - t243 * t39 + t2
     #49 * t58 / 0.2D1 - t183 + t239 * t145 - t253 * t153 / 0.2D1) + t19
     #1) * t162 * t88 / 0.5760D4
      t264 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t263)
      t267 = -0.1D1 + x4
      t269 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t270 = t119 * t267
      t273 = log(-0.4D1 * t92 * t270)
      t274 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t279 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t281 = t273 ** 2
      t292 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, x4)
      t297 = log(0.4D1 * t146 * t119 * t267 * t147)
      t298 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, x4)
      t302 = log(-0.4D1 * t146 * t270)
      t307 = -t274 + t298
      t316 = log(-0.4D1 * t196 * t270)
      t318 = x4 * t267
      t322 = log(0.4D1 * t204 * t318 * t147)
      t327 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, x4)
      t329 = t322 ** 2
      t333 = t316 ** 2
      t346 = log(-0.4D1 * t12 * t318)
      t348 = t346 ** 2
      t370 = -(-0.180D3 * t61 * (-t269 + t273 * t274) + 0.90D2 * t22 * (
     #-t279 + t273 * t269 - t281 * t274 / 0.2D1) - t72 * t274) * t116 * 
     #t88 / 0.2880D4 - (0.90D2 * t22 * (t292 - t297 * t298 - t269 + t302
     # * t274) - 0.180D3 * t61 * t307) * t162 * t164 / 0.2880D4 - (-0.18
     #0D3 * t61 * (-t269 + t316 * t274 + t292 - t322 * t298) + 0.90D2 * 
     #t22 * (t327 - t322 * t292 + t329 * t298 / 0.2D1 - t279 + t316 * t2
     #69 - t333 * t274 / 0.2D1) + t72 * t307) * t162 * t88 / 0.5760D4 - 
     #(0.180D3 * t61 * (t279 - t346 * t269 + t348 * t274 / 0.2D1) - t72 
     #* (t269 - t346 * t274) - 0.90D2 * t22 * (-t346 * t279 + t348 * t26
     #9 / 0.2D1 - t348 * t346 * t274 / 0.6D1) - t85 * t274) * t88 / 0.57
     #60D4
      t371 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t267, 0.0D0,
     # t370)
      t373 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t374 = s * t373
      t375 = t1 * x1
      t377 = -0.1D1 + x1
      t378 = t1 * t377
      t380 = t373 ** 2
      t382 = t1 ** 2
      t387 = 0.1D1 / (-0.2D1 + t373)
      t388 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t389 = t387 * t388
      t390 = t377 ** 2
      t391 = t11 * t390
      t392 = t380 ** 2
      t393 = t391 * t392
      t396 = log(0.4D1 * t92 * t393)
      t397 = t396 * t387
      t398 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t400 = t396 ** 2
      t401 = t400 * t387
      t402 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t408 = t387 * t398
      t422 = t387 * t402
      t430 = log(0.4D1 * t93 * t390 * x4 * t392)
      t431 = t430 * t387
      t437 = t430 ** 2
      t444 = t72 * t422
      t452 = log(0.4D1 * t146 * t391 * x4 * t392)
      t465 = log(0.4D1 * t146 * t393)
      t466 = t465 * t387
      t472 = t465 ** 2
      t483 = (0.180D3 * t61 * (t389 - t397 * t398 + t401 * t402 / 0.2D1)
     # - t72 * (t408 - t397 * t402) - 0.90D2 * t22 * (-t397 * t388 + t40
     #1 * t398 / 0.2D1 - t400 * t396 * t387 * t402 / 0.6D1) - t85 * t422
     #) * t116 / 0.2880D4 - (-0.180D3 * t61 * (t408 - t431 * t402) + 0.9
     #0D2 * t22 * (t389 - t431 * t398 + t437 * t387 * t402 / 0.2D1) + t4
     #44) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (t408 - t452 * t387 
     #* t402) - 0.180D3 * t61 * t422) * t162 * t164 / 0.2880D4 - (-0.180
     #D3 * t61 * (t408 - t466 * t402) + 0.90D2 * t22 * (t389 - t466 * t3
     #98 + t472 * t387 * t402 / 0.2D1) + t444) * t162 * t116 / 0.2880D4
      t484 = FJET(XB1, XB2, s, t374 * t375, 0.0D0, 0.0D0, -t374 * t378, 
     #-s * t380 * t382 * x1 * t377, t483)
      t486 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t487 = s * t486
      t489 = t378 * x4
      t491 = t378 * t267
      t493 = t486 ** 2
      t496 = x1 * t377
      t500 = 0.1D1 / (-0.2D1 + t486)
      t501 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t502 = t500 * t501
      t503 = t493 ** 2
      t505 = t318 * t503 * t390
      t508 = log(-0.4D1 * t93 * t505)
      t509 = t508 * t500
      t510 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t515 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t518 = t508 ** 2
      t525 = t500 * t510
      t532 = log(-0.4D1 * t167 * t505)
      t544 = -(0.180D3 * t61 * (t502 - t509 * t510) - 0.90D2 * t22 * (t5
     #00 * t515 - t509 * t501 + t518 * t500 * t510 / 0.2D1) - t72 * t525
     #) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (-t502 + t532 * t500 *
     # t510) + 0.180D3 * t61 * t525) * t162 * t164 / 0.2880D4
      t545 = FJET(XB1, XB2, s, t487 * t375, 0.0D0, -t487 * t489, t487 * 
     #t491, s * t493 * t382 * t496 * t267, t544)
      t547 = KAPPA2(x1, x2, t144, 0.0D0, z)
      t548 = s * t547
      t549 = t375 * t147
      t551 = t375 * x3
      t554 = t547 ** 2
      t560 = 0.1D1 / (-0.2D1 + t547)
      t561 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t144, 0.0D0)
      t562 = t560 * t561
      t563 = t390 * t147
      t564 = t554 ** 2
      t569 = log(-0.4D1 * t167 * t563 * x4 * t564)
      t571 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t144, 0.0D0)
      t576 = t560 * t571
      t586 = log(-0.4D1 * t146 * t391 * t147 * t564)
      t587 = t586 * t560
      t592 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, t144, 0.0D0)
      t595 = t586 ** 2
      t607 = -(0.90D2 * t22 * (-t562 + t569 * t560 * t571) + 0.180D3 * t
     #61 * t576) * t162 * t164 / 0.2880D4 - (0.180D3 * t61 * (t562 - t58
     #7 * t571) - 0.90D2 * t22 * (t560 * t592 - t587 * t561 + t595 * t56
     #0 * t571 / 0.2D1) - t72 * t576) * t162 * t116 / 0.2880D4
      t608 = FJET(XB1, XB2, s, -t548 * t549, t548 * t551, 0.0D0, -t548 *
     # t378, s * t554 * t382 * t496 * t147, t607)
      t610 = KAPPA2(x1, x2, t144, x4, z)
      t611 = s * t610
      t616 = t610 ** 2
      t621 = cos(t7)
      t624 = sqrt(x3 * t147 * t318)
      t631 = 0.1D1 / (-0.2D1 + t610)
      t632 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t144, x4)
      t634 = t616 ** 2
      t639 = log(0.4D1 * t167 * t318 * t563 * t634)
      t641 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t144, x4)
      t649 = 0.90D2 * t22 * (t631 * t632 - t639 * t631 * t641) - 0.180D3
     # * t61 * t631 * t641
      t653 = FJET(XB1, XB2, s, -t611 * t549, t611 * t551, -t611 * t489, 
     #t611 * t491, s * t616 * t382 * t496 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t621 * t624), -t649 * t162 * t164 / 0.2880D4)
      ggbbH5n6e1 = t264 * t263 + t371 * t370 + t484 * t483 + t545 * t544
     # + t608 * t607 - t653 * t649 * t162 * t116 * t88 / 0.2880D4

      end function



      doubleprecision function ggbbH5n6e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = 0.1D1 - x3
      t33 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.0D0)
      t34 = t18 - t33
      t36 = 0.1D1 / x3
      t38 = t36 * t27 * t29
      t41 = x3 * t7
      t42 = t10 * t13
      t45 = log(0.4D1 * t41 * t42)
      t47 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.0D0)
      t48 = -t32
      t52 = log(-0.4D1 * t41 * t42 * t48)
      t58 = 0.180D3 * t23 * t34
      t63 = t11 * t13
      t65 = log(0.4D1 * t63)
      t70 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t18
      t90 = log(0.4D1 * t42 * x4)
      t96 = t90 ** 2
      t105 = x3 * t10
      t106 = t13 * t48
      t110 = log(-0.4D1 * t105 * t106 * x4)
      t114 = log(0.4D1 * t105 * t14)
      t123 = t105 * t13
      t125 = log(0.4D1 * t123)
      t129 = log(-0.4D1 * t105 * t106)
      t134 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.0D0)
      t136 = t129 ** 2
      t140 = t125 ** 2
      t152 = log(0.4D1 * t42)
      t160 = t152 ** 2
      t179 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t5 * t34 * t38 / 0.32D2 - (0.90D2 * t5 * (t6
     # - t45 * t18 - t47 + t52 * t33) - t58) * t36 * t27 / 0.2880D4 + (-
     #0.180D3 * t23 * (-t6 + t65 * t18) + 0.90D2 * t5 * (-t70 + t65 * t6
     # - t72 * t18 / 0.2D1) - t84) * t27 / 0.2880D4 - (-0.180D3 * t23 * 
     #(t6 - t90 * t18) + 0.90D2 * t5 * (t70 - t90 * t6 + t96 * t18 / 0.2
     #D1) + t84) * t29 / 0.5760D4 - (0.90D2 * t5 * (-t47 + t110 * t33 + 
     #t6 - t114 * t18) - t58) * t36 * t29 / 0.5760D4 - (-0.180D3 * t23 *
     # (t6 - t125 * t18 - t47 + t129 * t33) + 0.90D2 * t5 * (-t134 + t12
     #9 * t47 - t136 * t33 / 0.2D1 + t70 - t125 * t6 + t140 * t18 / 0.2D
     #1) + t83 * t34) * t36 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t152)
     # * t5 * t70 / 0.5760D4 - (t79 - t81 + 0.180D3 * t152 * lh + 0.45D2
     # * t160) * t5 * t6 / 0.5760D4 - (-0.2884936567583026D3 - 0.120D3 *
     # t78 * lh + 0.60D2 * lh * t80 - t152 * t82 - 0.90D2 * t160 * lh - 
     #0.15D2 * t160 * t152) * t5 * t18 / 0.5760D4
      t180 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t179)
      t183 = -0.1D1 + x4
      t185 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t186 = t14 * t183
      t189 = log(-0.4D1 * t11 * t186)
      t190 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t201 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, x4)
      t202 = -t190 + t201
      t206 = x4 * t183
      t209 = log(-0.4D1 * t42 * t206)
      t214 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t216 = t209 ** 2
      t228 = log(-0.4D1 * t105 * t186)
      t230 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, x4)
      t234 = log(0.4D1 * t123 * t206 * t48)
      t245 = -(0.90D2 * t5 * (-t185 + t189 * t190) + 0.180D3 * t23 * t19
     #0) * t27 * t29 / 0.2880D4 - t5 * t202 * t38 / 0.32D2 - (0.180D3 * 
     #t23 * (t185 - t209 * t190) - 0.90D2 * t5 * (t214 - t209 * t185 + t
     #216 * t190 / 0.2D1) - t83 * t190) * t29 / 0.5760D4 - (0.90D2 * t5 
     #* (-t185 + t228 * t190 + t230 - t234 * t201) - 0.180D3 * t23 * t20
     #2) * t36 * t29 / 0.5760D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t183, 0.0D0,
     # t245)
      t248 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t249 = s * t248
      t250 = t1 * x1
      t252 = -0.1D1 + x1
      t253 = t1 * t252
      t255 = t248 ** 2
      t257 = t1 ** 2
      t262 = 0.1D1 / (-0.2D1 + t248)
      t263 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t264 = t262 * t263
      t265 = t252 ** 2
      t267 = t255 ** 2
      t271 = log(0.4D1 * t63 * t265 * x4 * t267)
      t273 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t278 = t262 * t273
      t280 = 0.180D3 * t23 * t278
      t289 = t41 * t10
      t290 = t13 * t265
      t291 = t290 * t267
      t294 = log(0.4D1 * t289 * t291)
      t306 = log(0.4D1 * t11 * t291)
      t307 = t306 * t262
      t312 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t315 = t306 ** 2
      t326 = -(0.90D2 * t5 * (t264 - t271 * t262 * t273) - t280) * t27 *
     # t29 / 0.2880D4 - t5 * t262 * t273 * t38 / 0.32D2 - (0.90D2 * t5 *
     # (t264 - t294 * t262 * t273) - t280) * t36 * t27 / 0.2880D4 + (0.1
     #80D3 * t23 * (t264 - t307 * t273) - 0.90D2 * t5 * (t262 * t312 - t
     #307 * t263 + t315 * t262 * t273 / 0.2D1) - t83 * t278) * t27 / 0.2
     #880D4
      t327 = FJET(XB1, XB2, s, t249 * t250, 0.0D0, 0.0D0, -t249 * t253, 
     #-s * t255 * t257 * x1 * t252, t326)
      t329 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t330 = s * t329
      t332 = t253 * x4
      t334 = t253 * t183
      t336 = t329 ** 2
      t339 = x1 * t252
      t343 = 0.1D1 / (-0.2D1 + t329)
      t344 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t346 = t336 ** 2
      t351 = log(-0.4D1 * t63 * t206 * t346 * t265)
      t353 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t369 = -(-0.90D2 * t5 * (t343 * t344 - t351 * t343 * t353) + 0.180
     #D3 * t23 * t343 * t353) * t27 * t29 / 0.2880D4 + t5 * t343 * t353 
     #* t38 / 0.32D2
      t370 = FJET(XB1, XB2, s, t330 * t250, 0.0D0, -t330 * t332, t330 * 
     #t334, s * t336 * t257 * t339 * t183, t369)
      t372 = KAPPA2(x1, x2, t32, 0.0D0, z)
      t373 = s * t372
      t374 = t250 * t48
      t376 = t250 * x3
      t379 = t372 ** 2
      t385 = 0.1D1 / (-0.2D1 + t372)
      t387 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.0D0)
      t391 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.0D0)
      t393 = t379 ** 2
      t398 = log(-0.4D1 * t289 * t290 * t48 * t393)
      t411 = t5 * t385 * t387 * t38 / 0.32D2 - (-0.90D2 * t5 * (t385 * t
     #391 - t398 * t385 * t387) + 0.180D3 * t23 * t385 * t387) * t36 * t
     #27 / 0.2880D4
      t412 = FJET(XB1, XB2, s, -t373 * t374, t373 * t376, 0.0D0, -t373 *
     # t253, s * t379 * t257 * t339 * t48, t411)
      t414 = KAPPA2(x1, x2, t32, x4, z)
      t415 = s * t414
      t420 = t414 ** 2
      t425 = cos(t8)
      t428 = sqrt(x3 * t48 * t206)
      t435 = 0.1D1 / (-0.2D1 + t414)
      t437 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, x4)
      t441 = FJET(XB1, XB2, s, -t415 * t374, t415 * t376, -t415 * t332, 
     #t415 * t334, s * t420 * t257 * t339 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t425 * t428), -t5 * t435 * t437 * t38 / 0.32D2)
      ggbbH5n6e0 = t180 * t179 + t246 * t245 + t327 * t326 + t370 * t369
     # + t412 * t411 - t441 * t5 * t435 * t437 * t36 * t27 * t29 / 0.32D
     #2

      end function



      doubleprecision function ggbbH5n6em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t7 = 0.1D1 - x3
      t8 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, 0.0D0)
      t9 = t6 - t8
      t10 = t5 * t9
      t11 = 0.1D1 / x3
      t12 = 0.1D1 / x4
      t13 = t11 * t12
      t16 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t18 = sin(x2 * 0.3141592653589793D1)
      t19 = t18 ** 2
      t20 = x3 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t25 = log(0.4D1 * t20 * t22)
      t27 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, 0.0D0)
      t28 = -t7
      t32 = log(-0.4D1 * t20 * t22 * t28)
      t37 = lh * t5
      t43 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t47 = t19 * t22
      t49 = log(0.4D1 * t47)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t61 = t49 ** 2
      t67 = x1 ** 2
      t68 = t67 * t19
      t71 = log(0.4D1 * t68 * t22)
      t77 = 0.180D3 * t37 * t6
      t79 = 0.1D1 / x1
      t83 = t79 * t12
      t91 = log(0.4D1 * t47 * x4)
      t99 = -t10 * t13 / 0.64D2 - (0.90D2 * t5 * (t16 - t25 * t6 - t27 +
     # t32 * t8) - 0.180D3 * t37 * t9) * t11 / 0.5760D4 - t5 * t43 / 0.6
     #4D2 - (-0.180D3 * lh - 0.90D2 * t49) * t5 * t16 / 0.5760D4 - (0.18
     #0D3 * t55 - 0.30D2 * t57 + 0.180D3 * t49 * lh + 0.45D2 * t61) * t5
     # * t6 / 0.5760D4 + (0.90D2 * t5 * (-t16 + t71 * t6) + t77) * t79 /
     # 0.2880D4 - t5 * t6 * t83 / 0.32D2 - t10 * t11 * t79 / 0.32D2 - (0
     #.90D2 * t5 * (t16 - t91 * t6) - t77) * t12 / 0.5760D4
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t99)
      t103 = -0.1D1 + x4
      t105 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t109 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t113 = log(-0.4D1 * t47 * x4 * t103)
      t123 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t7, x4)
      t128 = t5 * t105 * t83 / 0.32D2 - (-0.90D2 * t5 * (t109 - t113 * t
     #105) + 0.180D3 * t37 * t105) * t12 / 0.5760D4 - t5 * (-t105 + t123
     #) * t13 / 0.64D2
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t103, 0.0D0,
     # t128)
      t131 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t132 = s * t131
      t133 = t1 * x1
      t135 = -0.1D1 + x1
      t136 = t1 * t135
      t138 = t131 ** 2
      t140 = t1 ** 2
      t145 = 0.1D1 / (-0.2D1 + t131)
      t146 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t148 = t135 ** 2
      t150 = t138 ** 2
      t154 = log(0.4D1 * t68 * t22 * t148 * t150)
      t156 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t167 = t5 * t145
      t176 = (-0.90D2 * t5 * (t145 * t146 - t154 * t145 * t156) + 0.180D
     #3 * t37 * t145 * t156) * t79 / 0.2880D4 - t167 * t156 * t79 * t12 
     #/ 0.32D2 - t167 * t156 * t11 * t79 / 0.32D2
      t177 = FJET(XB1, XB2, s, t132 * t133, 0.0D0, 0.0D0, -t132 * t136, 
     #-s * t138 * t140 * x1 * t135, t176)
      t179 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t180 = s * t179
      t186 = t179 ** 2
      t189 = x1 * t135
      t193 = 0.1D1 / (-0.2D1 + t179)
      t195 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t197 = t195 * t79 * t12
      t200 = FJET(XB1, XB2, s, t180 * t133, 0.0D0, -t180 * t136 * x4, t1
     #80 * t136 * t103, s * t186 * t140 * t189 * t103, t5 * t193 * t197 
     #/ 0.32D2)
      t205 = KAPPA2(x1, x2, t7, 0.0D0, z)
      t206 = s * t205
      t212 = t205 ** 2
      t218 = 0.1D1 / (-0.2D1 + t205)
      t220 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t7, 0.0D0)
      t222 = t220 * t11 * t79
      t225 = FJET(XB1, XB2, s, -t206 * t133 * t28, t206 * t133 * x3, 0.0
     #D0, -t206 * t136, s * t212 * t140 * t189 * t28, t5 * t218 * t222 /
     # 0.32D2)
      ggbbH5n6em1 = t100 * t99 + t129 * t128 + t177 * t176 + t200 * t5 *
     # t193 * t197 / 0.32D2 + t225 * t5 * t218 * t222 / 0.32D2

      end function



      doubleprecision function ggbbH5n6em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t12 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, 0.0
     #D0)
      t18 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t23 = sin(x2 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t25 = z ** 2
      t29 = log(0.4D1 * t24 / t25)
      t35 = 0.1D1 / x4
      t38 = -t7 * t8 / 0.32D2 - t5 * (t6 - t12) / x3 / 0.64D2 - t5 * t18
     # / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t29) * t5 * t6 / 0.5760D4 - 
     #t7 * t35 / 0.64D2
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t42 = s * t41
      t45 = -0.1D1 + x1
      t48 = t41 ** 2
      t50 = t1 ** 2
      t55 = 0.1D1 / (-0.2D1 + t41)
      t57 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t61 = FJET(XB1, XB2, s, t42 * t1 * x1, 0.0D0, 0.0D0, -t42 * t1 * t
     #45, -s * t48 * t50 * x1 * t45, -t5 * t55 * t57 * t8 / 0.32D2)
      t70 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t5 * t70 * t35 / 0.64D2)
      ggbbH5n6em2 = t39 * t38 - t61 * t5 * t55 * t57 * t8 / 0.32D2 + t74
     # * t5 * t70 * t35 / 0.64D2

      end function



      doubleprecision function ggbbH5n6em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n6em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n6em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n6em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n7e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t14 = log(0.4D1 * t12)
      t17 = t14 ** 2
      t20 = s ** 2
      t22 = 0.1D1 / t20 / s
      t24 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t61 = lh * t22
      t62 = t12 * x4
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t72 = t31 * t22
      t85 = t48 * t22
      t86 = t85 * t58
      t88 = 0.1D1 / x4
      t91 = x1 ** 2
      t92 = t91 * t9
      t93 = t92 * t11
      t95 = log(0.4D1 * t93)
      t97 = t95 ** 2
      t116 = 0.1D1 / x1
      t119 = t11 * x4
      t122 = log(0.4D1 * t92 * t119)
      t128 = t122 ** 2
      t139 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t140 = x3 * t91
      t141 = t140 * t9
      t142 = -0.1D1 + x3
      t143 = t11 * t142
      t144 = t143 * x4
      t147 = log(-0.4D1 * t141 * t144)
      t148 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t152 = log(0.4D1 * t140 * t62)
      t157 = -t148 + t58
      t161 = 0.1D1 / x3
      t163 = t116 * t88
      t169 = log(-0.4D1 * t140 * t12 * t142)
      t171 = t140 * t12
      t173 = log(0.4D1 * t171)
      t179 = t173 ** 2
      t182 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t184 = t169 ** 2
      t190 = t72 * t157
      t195 = x3 * t9
      t196 = t195 * t11
      t198 = log(0.4D1 * t196)
      t200 = t198 ** 2
      t205 = log(-0.4D1 * t195 * t143)
      t207 = t205 ** 2
      t238 = log(-0.4D1 * t195 * t144)
      t242 = log(0.4D1 * t195 * t119)
      t248 = t242 ** 2
      t252 = t238 ** 2
      t262 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 - (-0.180D3 * t61 * (t24 - t64 * t39 + t66 
     #* t58 / 0.2D1) + t72 * (t39 - t64 * t58) + 0.90D2 * t22 * (-t64 * 
     #t24 + t66 * t39 / 0.2D1 - t66 * t64 * t58 / 0.6D1) + t86) * t88 / 
     #0.5760D4 + (-0.180D3 * t61 * (-t24 + t95 * t39 - t97 * t58 / 0.2D1
     #) + t72 * (-t39 + t95 * t58) + 0.90D2 * t22 * (t95 * t24 - t97 * t
     #39 / 0.2D1 + t97 * t95 * t58 / 0.6D1) - t86) * t116 / 0.2880D4 - (
     #-0.180D3 * t61 * (t39 - t122 * t58) + 0.90D2 * t22 * (t24 - t122 *
     # t39 + t128 * t58 / 0.2D1) + t72 * t58) * t116 * t88 / 0.2880D4 - 
     #(0.90D2 * t22 * (-t139 + t147 * t148 + t39 - t152 * t58) - 0.180D3
     # * t61 * t157) * t161 * t163 / 0.2880D4 - (-0.180D3 * t61 * (-t139
     # + t169 * t148 + t39 - t173 * t58) + 0.90D2 * t22 * (t24 - t173 * 
     #t39 + t179 * t58 / 0.2D1 - t182 + t169 * t139 - t184 * t148 / 0.2D
     #1) + t190) * t161 * t116 / 0.2880D4 - (-0.180D3 * t61 * (t24 - t19
     #8 * t39 + t200 * t58 / 0.2D1 - t182 + t205 * t139 - t207 * t148 / 
     #0.2D1) + t72 * (-t139 + t205 * t148 + t39 - t198 * t58) + 0.90D2 *
     # t22 * (-t198 * t24 + t200 * t39 / 0.2D1 - t200 * t198 * t58 / 0.6
     #D1 + t205 * t182 - t207 * t139 / 0.2D1 + t207 * t205 * t148 / 0.6D
     #1) + t85 * t157) * t161 / 0.5760D4 - (-0.180D3 * t61 * (-t139 + t2
     #38 * t148 + t39 - t242 * t58) + 0.90D2 * t22 * (t24 - t242 * t39 +
     # t248 * t58 / 0.2D1 - t182 + t238 * t139 - t252 * t148 / 0.2D1) + 
     #t190) * t161 * t88 / 0.5760D4
      t263 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t262)
      t265 = -0.1D1 + x4
      t268 = -t265
      t269 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t268)
      t270 = t119 * t265
      t273 = log(-0.4D1 * t92 * t270)
      t274 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t268)
      t279 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t268)
      t281 = t273 ** 2
      t294 = log(-0.4D1 * t141 * t270)
      t296 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t268)
      t301 = log(0.4D1 * t141 * t119 * t265 * t142)
      t302 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t268)
      t307 = -t274 + t302
      t316 = log(-0.4D1 * t195 * t270)
      t318 = x4 * t265
      t322 = log(0.4D1 * t196 * t318 * t142)
      t327 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t268)
      t329 = t322 ** 2
      t333 = t316 ** 2
      t346 = log(-0.4D1 * t12 * t318)
      t348 = t346 ** 2
      t370 = -(-0.180D3 * t61 * (-t269 + t273 * t274) + 0.90D2 * t22 * (
     #-t279 + t273 * t269 - t281 * t274 / 0.2D1) - t72 * t274) * t116 * 
     #t88 / 0.2880D4 - (0.90D2 * t22 * (-t269 + t294 * t274 + t296 - t30
     #1 * t302) - 0.180D3 * t61 * t307) * t161 * t163 / 0.2880D4 - (-0.1
     #80D3 * t61 * (-t269 + t316 * t274 + t296 - t322 * t302) + 0.90D2 *
     # t22 * (t327 - t322 * t296 + t329 * t302 / 0.2D1 - t279 + t316 * t
     #269 - t333 * t274 / 0.2D1) + t72 * t307) * t161 * t88 / 0.5760D4 -
     # (0.180D3 * t61 * (t279 - t346 * t269 + t348 * t274 / 0.2D1) - t72
     # * (t269 - t346 * t274) - 0.90D2 * t22 * (-t346 * t279 + t348 * t2
     #69 / 0.2D1 - t348 * t346 * t274 / 0.6D1) - t85 * t274) * t88 / 0.5
     #760D4
      t371 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t265, t2 * x4, 0.0D0,
     # t370)
      t373 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t374 = s * t373
      t375 = t1 * x1
      t377 = -0.1D1 + x1
      t378 = t1 * t377
      t380 = t373 ** 2
      t382 = t1 ** 2
      t387 = 0.1D1 / (-0.2D1 + t373)
      t388 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t389 = t387 * t388
      t390 = t377 ** 2
      t391 = t91 * t390
      t392 = t380 ** 2
      t393 = t391 * t392
      t396 = log(0.4D1 * t12 * t393)
      t397 = t396 * t387
      t398 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t400 = t396 ** 2
      t401 = t400 * t387
      t402 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t408 = t387 * t398
      t422 = t387 * t402
      t426 = t390 * x4
      t430 = log(0.4D1 * t93 * t426 * t392)
      t431 = t430 * t387
      t437 = t430 ** 2
      t444 = t72 * t422
      t452 = log(0.4D1 * t196 * t391 * x4 * t392)
      t465 = log(0.4D1 * t196 * t393)
      t466 = t465 * t387
      t472 = t465 ** 2
      t483 = (0.180D3 * t61 * (t389 - t397 * t398 + t401 * t402 / 0.2D1)
     # - t72 * (t408 - t397 * t402) - 0.90D2 * t22 * (-t397 * t388 + t40
     #1 * t398 / 0.2D1 - t400 * t396 * t387 * t402 / 0.6D1) - t85 * t422
     #) * t116 / 0.2880D4 - (-0.180D3 * t61 * (t408 - t431 * t402) + 0.9
     #0D2 * t22 * (t389 - t431 * t398 + t437 * t387 * t402 / 0.2D1) + t4
     #44) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (t408 - t452 * t387 
     #* t402) - 0.180D3 * t61 * t422) * t161 * t163 / 0.2880D4 - (-0.180
     #D3 * t61 * (t408 - t466 * t402) + 0.90D2 * t22 * (t389 - t466 * t3
     #98 + t472 * t387 * t402 / 0.2D1) + t444) * t161 * t116 / 0.2880D4
      t484 = FJET(XB1, XB2, s, 0.0D0, t374 * t375, -t374 * t378, 0.0D0, 
     #-s * t380 * t382 * x1 * t377, t483)
      t486 = KAPPA2(x1, x2, 0.0D0, t268, z)
      t487 = s * t486
      t489 = t378 * t265
      t491 = t378 * x4
      t493 = t486 ** 2
      t496 = x1 * t377
      t500 = 0.1D1 / (-0.2D1 + t486)
      t501 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t268)
      t502 = t500 * t501
      t503 = t493 ** 2
      t505 = t426 * t265 * t503
      t508 = log(-0.4D1 * t93 * t505)
      t509 = t508 * t500
      t510 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t268)
      t515 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t268)
      t518 = t508 ** 2
      t525 = t500 * t510
      t532 = log(-0.4D1 * t171 * t505)
      t544 = -(0.180D3 * t61 * (t502 - t509 * t510) - 0.90D2 * t22 * (t5
     #00 * t515 - t509 * t501 + t518 * t500 * t510 / 0.2D1) - t72 * t525
     #) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (-t502 + t532 * t500 *
     # t510) + 0.180D3 * t61 * t525) * t161 * t163 / 0.2880D4
      t545 = FJET(XB1, XB2, s, 0.0D0, t487 * t375, t487 * t489, -t487 * 
     #t491, s * t493 * t382 * t496 * t265, t544)
      t547 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t548 = s * t547
      t549 = t375 * x3
      t551 = t375 * t142
      t554 = t547 ** 2
      t560 = 0.1D1 / (-0.2D1 + t547)
      t561 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t562 = t560 * t561
      t563 = t390 * t142
      t564 = t554 ** 2
      t569 = log(-0.4D1 * t171 * t563 * x4 * t564)
      t571 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t576 = t560 * t571
      t586 = log(-0.4D1 * t196 * t391 * t142 * t564)
      t587 = t586 * t560
      t592 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t595 = t586 ** 2
      t607 = -(0.90D2 * t22 * (-t562 + t569 * t560 * t571) + 0.180D3 * t
     #61 * t576) * t161 * t163 / 0.2880D4 - (0.180D3 * t61 * (t562 - t58
     #7 * t571) - 0.90D2 * t22 * (t560 * t592 - t587 * t561 + t595 * t56
     #0 * t571 / 0.2D1) - t72 * t576) * t161 * t116 / 0.2880D4
      t608 = FJET(XB1, XB2, s, t548 * t549, -t548 * t551, -t548 * t378, 
     #0.0D0, s * t554 * t382 * t496 * t142, t607)
      t610 = KAPPA2(x1, x2, x3, t268, z)
      t611 = s * t610
      t616 = t610 ** 2
      t621 = cos(t7)
      t624 = sqrt(x3 * t142 * t318)
      t631 = 0.1D1 / (-0.2D1 + t610)
      t632 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, t268)
      t634 = t616 ** 2
      t639 = log(0.4D1 * t171 * t563 * t318 * t634)
      t641 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t268)
      t649 = 0.90D2 * t22 * (t631 * t632 - t639 * t631 * t641) - 0.180D3
     # * t61 * t631 * t641
      t653 = FJET(XB1, XB2, s, t611 * t549, -t611 * t551, t611 * t489, -
     #t611 * t491, s * t616 * t382 * t496 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t621 * t624), -t649 * t161 * t163 / 0.2880D4)
      ggbbH5n7e1 = t262 * t263 + t371 * t370 + t484 * t483 + t545 * t544
     # + t608 * t607 - t653 * t649 * t161 * t116 * t88 / 0.2880D4

      end function



      doubleprecision function ggbbH5n7e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t33 = -t32 + t18
      t35 = 0.1D1 / x3
      t37 = t35 * t27 * t29
      t40 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t41 = x3 * t7
      t42 = t10 * t13
      t43 = -0.1D1 + x3
      t47 = log(-0.4D1 * t41 * t42 * t43)
      t51 = log(0.4D1 * t41 * t42)
      t57 = 0.180D3 * t23 * t33
      t62 = t11 * t13
      t64 = log(0.4D1 * t62)
      t69 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t71 = t64 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t5
      t83 = t82 * t18
      t89 = log(0.4D1 * t42 * x4)
      t95 = t89 ** 2
      t104 = x3 * t10
      t105 = t13 * t43
      t109 = log(-0.4D1 * t104 * t105 * x4)
      t113 = log(0.4D1 * t104 * t14)
      t124 = log(-0.4D1 * t104 * t105)
      t126 = t104 * t13
      t128 = log(0.4D1 * t126)
      t134 = t128 ** 2
      t137 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t139 = t124 ** 2
      t151 = log(0.4D1 * t42)
      t159 = t151 ** 2
      t178 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t5 * t33 * t37 / 0.32D2 - (0.90D2 * t5 * (-t
     #40 + t47 * t32 + t6 - t51 * t18) - t57) * t35 * t27 / 0.2880D4 + (
     #-0.180D3 * t23 * (-t6 + t64 * t18) + 0.90D2 * t5 * (-t69 + t64 * t
     #6 - t71 * t18 / 0.2D1) - t83) * t27 / 0.2880D4 - (-0.180D3 * t23 *
     # (t6 - t89 * t18) + 0.90D2 * t5 * (t69 - t89 * t6 + t95 * t18 / 0.
     #2D1) + t83) * t29 / 0.5760D4 - (0.90D2 * t5 * (-t40 + t109 * t32 +
     # t6 - t113 * t18) - t57) * t35 * t29 / 0.5760D4 - (-0.180D3 * t23 
     #* (-t40 + t124 * t32 + t6 - t128 * t18) + 0.90D2 * t5 * (t69 - t12
     #8 * t6 + t134 * t18 / 0.2D1 - t137 + t124 * t40 - t139 * t32 / 0.2
     #D1) + t82 * t33) * t35 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t151
     #) * t5 * t69 / 0.5760D4 - (t78 - t80 + 0.180D3 * t151 * lh + 0.45D
     #2 * t159) * t5 * t6 / 0.5760D4 - (-0.2884936567583026D3 - 0.120D3 
     #* t77 * lh + 0.60D2 * lh * t79 - t151 * t81 - 0.90D2 * t159 * lh -
     # 0.15D2 * t159 * t151) * t5 * t18 / 0.5760D4
      t179 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t178)
      t181 = -0.1D1 + x4
      t184 = -t181
      t185 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t184)
      t186 = t14 * t181
      t189 = log(-0.4D1 * t11 * t186)
      t190 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t184)
      t201 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t184)
      t202 = -t190 + t201
      t206 = x4 * t181
      t209 = log(-0.4D1 * t42 * t206)
      t214 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t184)
      t216 = t209 ** 2
      t228 = log(-0.4D1 * t104 * t186)
      t230 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t184)
      t234 = log(0.4D1 * t126 * t206 * t43)
      t245 = -(0.90D2 * t5 * (-t185 + t189 * t190) + 0.180D3 * t23 * t19
     #0) * t27 * t29 / 0.2880D4 - t5 * t202 * t37 / 0.32D2 - (0.180D3 * 
     #t23 * (t185 - t209 * t190) - 0.90D2 * t5 * (t214 - t209 * t185 + t
     #216 * t190 / 0.2D1) - t82 * t190) * t29 / 0.5760D4 - (0.90D2 * t5 
     #* (-t185 + t228 * t190 + t230 - t234 * t201) - 0.180D3 * t23 * t20
     #2) * t35 * t29 / 0.5760D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t181, t2 * x4, 0.0D0,
     # t245)
      t248 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t249 = s * t248
      t250 = t1 * x1
      t252 = -0.1D1 + x1
      t253 = t1 * t252
      t255 = t248 ** 2
      t257 = t1 ** 2
      t262 = 0.1D1 / (-0.2D1 + t248)
      t263 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t264 = t262 * t263
      t265 = t252 ** 2
      t266 = x4 * t265
      t267 = t255 ** 2
      t271 = log(0.4D1 * t62 * t266 * t267)
      t273 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t278 = t262 * t273
      t280 = 0.180D3 * t23 * t278
      t289 = t7 * t265
      t290 = t289 * t267
      t293 = log(0.4D1 * t126 * t290)
      t305 = log(0.4D1 * t42 * t290)
      t306 = t305 * t262
      t311 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t314 = t305 ** 2
      t325 = -(0.90D2 * t5 * (t264 - t271 * t262 * t273) - t280) * t27 *
     # t29 / 0.2880D4 - t5 * t262 * t273 * t37 / 0.32D2 - (0.90D2 * t5 *
     # (t264 - t293 * t262 * t273) - t280) * t35 * t27 / 0.2880D4 + (0.1
     #80D3 * t23 * (t264 - t306 * t273) - 0.90D2 * t5 * (t262 * t311 - t
     #306 * t263 + t314 * t262 * t273 / 0.2D1) - t82 * t278) * t27 / 0.2
     #880D4
      t326 = FJET(XB1, XB2, s, 0.0D0, t249 * t250, -t249 * t253, 0.0D0, 
     #-s * t255 * t257 * x1 * t252, t325)
      t328 = KAPPA2(x1, x2, 0.0D0, t184, z)
      t329 = s * t328
      t331 = t253 * t181
      t333 = t253 * x4
      t335 = t328 ** 2
      t338 = x1 * t252
      t342 = 0.1D1 / (-0.2D1 + t328)
      t343 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t184)
      t345 = t335 ** 2
      t350 = log(-0.4D1 * t62 * t266 * t181 * t345)
      t352 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t184)
      t368 = -(-0.90D2 * t5 * (t342 * t343 - t350 * t342 * t352) + 0.180
     #D3 * t23 * t342 * t352) * t27 * t29 / 0.2880D4 + t5 * t342 * t352 
     #* t37 / 0.32D2
      t369 = FJET(XB1, XB2, s, 0.0D0, t329 * t250, t329 * t331, -t329 * 
     #t333, s * t335 * t257 * t338 * t181, t368)
      t371 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t372 = s * t371
      t373 = t250 * x3
      t375 = t250 * t43
      t378 = t371 ** 2
      t384 = 0.1D1 / (-0.2D1 + t371)
      t386 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t390 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t392 = t378 ** 2
      t397 = log(-0.4D1 * t126 * t289 * t43 * t392)
      t410 = t5 * t384 * t386 * t37 / 0.32D2 - (-0.90D2 * t5 * (t384 * t
     #390 - t397 * t384 * t386) + 0.180D3 * t23 * t384 * t386) * t35 * t
     #27 / 0.2880D4
      t411 = FJET(XB1, XB2, s, t372 * t373, -t372 * t375, -t372 * t253, 
     #0.0D0, s * t378 * t257 * t338 * t43, t410)
      t413 = KAPPA2(x1, x2, x3, t184, z)
      t414 = s * t413
      t419 = t413 ** 2
      t424 = cos(t8)
      t427 = sqrt(x3 * t43 * t206)
      t434 = 0.1D1 / (-0.2D1 + t413)
      t436 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t184)
      t440 = FJET(XB1, XB2, s, t414 * t373, -t414 * t375, t414 * t331, -
     #t414 * t333, s * t419 * t257 * t338 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t424 * t427), -t5 * t434 * t436 * t37 / 0.32D2)
      ggbbH5n7e0 = t179 * t178 + t246 * t245 + t326 * t325 + t369 * t368
     # + t411 * t410 - t440 * t5 * t434 * t436 * t35 * t27 * t29 / 0.32D
     #2

      end function



      doubleprecision function ggbbH5n7em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t7 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t8 = -t6 + t7
      t9 = t5 * t8
      t10 = 0.1D1 / x3
      t11 = 0.1D1 / x4
      t12 = t10 * t11
      t15 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t17 = sin(x2 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t19 = x3 * t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t22 = -0.1D1 + x3
      t26 = log(-0.4D1 * t19 * t21 * t22)
      t28 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t31 = log(0.4D1 * t19 * t21)
      t36 = lh * t5
      t42 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t46 = t18 * t21
      t48 = log(0.4D1 * t46)
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t60 = t48 ** 2
      t66 = x1 ** 2
      t70 = log(0.4D1 * t66 * t18 * t21)
      t76 = 0.180D3 * t36 * t7
      t78 = 0.1D1 / x1
      t82 = t78 * t11
      t90 = log(0.4D1 * t46 * x4)
      t98 = -t9 * t12 / 0.64D2 - (0.90D2 * t5 * (-t15 + t26 * t6 + t28 -
     # t31 * t7) - 0.180D3 * t36 * t8) * t10 / 0.5760D4 - t5 * t42 / 0.6
     #4D2 - (-0.180D3 * lh - 0.90D2 * t48) * t5 * t28 / 0.5760D4 - (0.18
     #0D3 * t54 - 0.30D2 * t56 + 0.180D3 * t48 * lh + 0.45D2 * t60) * t5
     # * t7 / 0.5760D4 + (0.90D2 * t5 * (-t28 + t70 * t7) + t76) * t78 /
     # 0.2880D4 - t5 * t7 * t82 / 0.32D2 - t9 * t10 * t78 / 0.32D2 - (0.
     #90D2 * t5 * (t28 - t90 * t7) - t76) * t11 / 0.5760D4
      t99 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t98)
      t101 = -0.1D1 + x4
      t104 = -t101
      t105 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t104)
      t109 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t104)
      t113 = log(-0.4D1 * t46 * x4 * t101)
      t123 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t104)
      t128 = t5 * t105 * t82 / 0.32D2 - (-0.90D2 * t5 * (t109 - t113 * t
     #105) + 0.180D3 * t36 * t105) * t11 / 0.5760D4 - t5 * (-t105 + t123
     #) * t12 / 0.64D2
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t101, t2 * x4, 0.0D0,
     # t128)
      t131 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t132 = s * t131
      t133 = t1 * x1
      t135 = -0.1D1 + x1
      t136 = t1 * t135
      t138 = t131 ** 2
      t140 = t1 ** 2
      t145 = 0.1D1 / (-0.2D1 + t131)
      t146 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t148 = t135 ** 2
      t150 = t138 ** 2
      t154 = log(0.4D1 * t46 * t66 * t148 * t150)
      t156 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t167 = t5 * t145
      t176 = (-0.90D2 * t5 * (t145 * t146 - t154 * t145 * t156) + 0.180D
     #3 * t36 * t145 * t156) * t78 / 0.2880D4 - t167 * t156 * t78 * t11 
     #/ 0.32D2 - t167 * t156 * t10 * t78 / 0.32D2
      t177 = FJET(XB1, XB2, s, 0.0D0, t132 * t133, -t132 * t136, 0.0D0, 
     #-s * t138 * t140 * x1 * t135, t176)
      t179 = KAPPA2(x1, x2, 0.0D0, t104, z)
      t180 = s * t179
      t186 = t179 ** 2
      t189 = x1 * t135
      t193 = 0.1D1 / (-0.2D1 + t179)
      t195 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t104)
      t197 = t195 * t78 * t11
      t200 = FJET(XB1, XB2, s, 0.0D0, t180 * t133, t180 * t136 * t101, -
     #t180 * t136 * x4, s * t186 * t140 * t189 * t101, t5 * t193 * t197 
     #/ 0.32D2)
      t205 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t206 = s * t205
      t212 = t205 ** 2
      t218 = 0.1D1 / (-0.2D1 + t205)
      t220 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t222 = t220 * t10 * t78
      t225 = FJET(XB1, XB2, s, t206 * t133 * x3, -t206 * t133 * t22, -t2
     #06 * t136, 0.0D0, s * t212 * t140 * t189 * t22, t5 * t218 * t222 /
     # 0.32D2)
      ggbbH5n7em1 = t99 * t98 + t129 * t128 + t177 * t176 + t200 * t5 * 
     #t193 * t197 / 0.32D2 + t225 * t5 * t218 * t222 / 0.32D2

      end function



      doubleprecision function ggbbH5n7em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t17 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t34 = 0.1D1 / x4
      t37 = -t7 * t8 / 0.32D2 - t5 * (-t11 + t6) / x3 / 0.64D2 - t5 * t1
     #7 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t28) * t5 * t6 / 0.5760D4 -
     # t7 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t40 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t41 = s * t40
      t44 = -0.1D1 + x1
      t47 = t40 ** 2
      t49 = t1 ** 2
      t54 = 0.1D1 / (-0.2D1 + t40)
      t56 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t60 = FJET(XB1, XB2, s, 0.0D0, t41 * t1 * x1, -t41 * t1 * t44, 0.0
     #D0, -s * t47 * t49 * x1 * t44, -t5 * t54 * t56 * t8 / 0.32D2)
      t66 = -0.1D1 + x4
      t70 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, -t66)
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t66, t2 * x4, 0.0D0, t
     #5 * t70 * t34 / 0.64D2)
      ggbbH5n7em2 = t38 * t37 - t60 * t5 * t54 * t56 * t8 / 0.32D2 + t74
     # * t5 * t70 * t34 / 0.64D2

      end function



      doubleprecision function ggbbH5n7em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n7em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n7em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n7em4 = 0.0D0

      end function


      doubleprecision function ggbbH5n8e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t14 = log(0.4D1 * t12)
      t17 = t14 ** 2
      t20 = s ** 2
      t22 = 0.1D1 / t20 / s
      t24 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t61 = lh * t22
      t62 = t12 * x4
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t72 = t31 * t22
      t85 = t48 * t22
      t86 = t85 * t58
      t88 = 0.1D1 / x4
      t91 = x1 ** 2
      t92 = t91 * t9
      t93 = t92 * t11
      t95 = log(0.4D1 * t93)
      t97 = t95 ** 2
      t116 = 0.1D1 / x1
      t119 = t11 * x4
      t122 = log(0.4D1 * t92 * t119)
      t128 = t122 ** 2
      t139 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t140 = x3 * t91
      t141 = t140 * t9
      t142 = -0.1D1 + x3
      t143 = t11 * t142
      t144 = t143 * x4
      t147 = log(-0.4D1 * t141 * t144)
      t148 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t152 = log(0.4D1 * t140 * t62)
      t157 = -t58 + t148
      t161 = 0.1D1 / x3
      t163 = t116 * t88
      t169 = log(-0.4D1 * t140 * t12 * t142)
      t171 = t140 * t12
      t173 = log(0.4D1 * t171)
      t178 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t180 = t169 ** 2
      t184 = t173 ** 2
      t195 = x3 * t9
      t198 = log(-0.4D1 * t195 * t143)
      t200 = t198 ** 2
      t203 = t195 * t11
      t205 = log(0.4D1 * t203)
      t207 = t205 ** 2
      t232 = -t157
      t239 = log(0.4D1 * t195 * t119)
      t243 = log(-0.4D1 * t195 * t144)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t264 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 - (-0.180D3 * t61 * (t24 - t64 * t39 + t66 
     #* t58 / 0.2D1) + t72 * (t39 - t64 * t58) + 0.90D2 * t22 * (-t64 * 
     #t24 + t66 * t39 / 0.2D1 - t66 * t64 * t58 / 0.6D1) + t86) * t88 / 
     #0.5760D4 - (-0.180D3 * t61 * (t24 - t95 * t39 + t97 * t58 / 0.2D1)
     # + t72 * (t39 - t95 * t58) + 0.90D2 * t22 * (-t95 * t24 + t97 * t3
     #9 / 0.2D1 - t97 * t95 * t58 / 0.6D1) + t86) * t116 / 0.2880D4 + (-
     #0.180D3 * t61 * (-t39 + t122 * t58) + 0.90D2 * t22 * (-t24 + t122 
     #* t39 - t128 * t58 / 0.2D1) - t72 * t58) * t116 * t88 / 0.2880D4 +
     # (0.90D2 * t22 * (t139 - t147 * t148 - t39 + t152 * t58) - 0.180D3
     # * t61 * t157) * t161 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t139 
     #- t169 * t148 - t39 + t173 * t58) + 0.90D2 * t22 * (t178 - t169 * 
     #t139 + t180 * t148 / 0.2D1 - t24 + t173 * t39 - t184 * t58 / 0.2D1
     #) + t72 * t157) * t161 * t116 / 0.2880D4 - (-0.180D3 * t61 * (-t17
     #8 + t198 * t139 - t200 * t148 / 0.2D1 + t24 - t205 * t39 + t207 * 
     #t58 / 0.2D1) + t72 * (t39 - t205 * t58 - t139 + t198 * t148) + 0.9
     #0D2 * t22 * (t198 * t178 - t200 * t139 / 0.2D1 + t200 * t198 * t14
     #8 / 0.6D1 - t205 * t24 + t207 * t39 / 0.2D1 - t207 * t205 * t58 / 
     #0.6D1) + t85 * t232) * t161 / 0.5760D4 - (-0.180D3 * t61 * (t39 - 
     #t239 * t58 - t139 + t243 * t148) + 0.90D2 * t22 * (t24 - t239 * t3
     #9 + t249 * t58 / 0.2D1 - t178 + t243 * t139 - t253 * t148 / 0.2D1)
     # + t72 * t232) * t161 * t88 / 0.5760D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t264)
      t268 = -0.1D1 + x4
      t270 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t271 = t119 * t268
      t274 = log(-0.4D1 * t92 * t271)
      t275 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t280 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t282 = t274 ** 2
      t295 = log(-0.4D1 * t141 * t271)
      t297 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t302 = log(0.4D1 * t141 * t119 * t268 * t142)
      t303 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t308 = -t303 + t275
      t315 = x4 * t268
      t319 = log(0.4D1 * t203 * t315 * t142)
      t323 = log(-0.4D1 * t195 * t271)
      t328 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t330 = t319 ** 2
      t334 = t323 ** 2
      t348 = log(-0.4D1 * t12 * t315)
      t350 = t348 ** 2
      t372 = (-0.180D3 * t61 * (t270 - t274 * t275) + 0.90D2 * t22 * (t2
     #80 - t274 * t270 + t282 * t275 / 0.2D1) + t72 * t275) * t116 * t88
     # / 0.2880D4 + (0.90D2 * t22 * (t270 - t295 * t275 - t297 + t302 * 
     #t303) - 0.180D3 * t61 * t308) * t161 * t163 / 0.2880D4 - (-0.180D3
     # * t61 * (t297 - t319 * t303 - t270 + t323 * t275) + 0.90D2 * t22 
     #* (t328 - t319 * t297 + t330 * t303 / 0.2D1 - t280 + t323 * t270 -
     # t334 * t275 / 0.2D1) - t72 * t308) * t161 * t88 / 0.5760D4 - (0.1
     #80D3 * t61 * (t280 - t348 * t270 + t350 * t275 / 0.2D1) - t72 * (t
     #270 - t348 * t275) - 0.90D2 * t22 * (-t348 * t280 + t350 * t270 / 
     #0.2D1 - t350 * t348 * t275 / 0.6D1) - t85 * t275) * t88 / 0.5760D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t268, 0.0D0,
     # t372)
      t376 = -0.1D1 + x1
      t378 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t379 = t376 ** 2
      t380 = t91 * t379
      t383 = log(0.4D1 * t12 * t380)
      t384 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t386 = t383 ** 2
      t387 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t408 = t380 * x4
      t411 = log(0.4D1 * t12 * t408)
      t417 = t411 ** 2
      t423 = t72 * t387
      t429 = log(0.4D1 * t203 * t408)
      t443 = log(0.4D1 * t195 * t11 * t91 * t379)
      t449 = t443 ** 2
      t459 = -(0.180D3 * t61 * (t378 - t383 * t384 + t386 * t387 / 0.2D1
     #) - t72 * (t384 - t383 * t387) - 0.90D2 * t22 * (-t383 * t378 + t3
     #86 * t384 / 0.2D1 - t386 * t383 * t387 / 0.6D1) - t85 * t387) * t1
     #16 / 0.2880D4 + (-0.180D3 * t61 * (t384 - t411 * t387) + 0.90D2 * 
     #t22 * (t378 - t411 * t384 + t417 * t387 / 0.2D1) + t423) * t116 * 
     #t88 / 0.2880D4 + (0.90D2 * t22 * (t384 - t429 * t387) - 0.180D3 * 
     #t61 * t387) * t161 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t384 - t
     #443 * t387) + 0.90D2 * t22 * (t378 - t443 * t384 + t449 * t387 / 0
     #.2D1) + t423) * t161 * t116 / 0.2880D4
      t460 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t376, 0.0D0,
     # t459)
      t462 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t463 = s * t462
      t464 = t1 * x1
      t466 = t1 * t376
      t467 = t466 * x4
      t469 = t466 * t268
      t471 = t462 ** 2
      t473 = t1 ** 2
      t475 = x1 * t376
      t479 = 0.1D1 / (-0.2D1 + t462)
      t480 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t481 = t479 * t480
      t482 = t471 ** 2
      t484 = t315 * t482 * t379
      t487 = log(-0.4D1 * t93 * t484)
      t488 = t487 * t479
      t489 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t494 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t497 = t487 ** 2
      t504 = t479 * t489
      t511 = log(-0.4D1 * t171 * t484)
      t523 = (-0.180D3 * t61 * (t481 - t488 * t489) + 0.90D2 * t22 * (t4
     #79 * t494 - t488 * t480 + t497 * t479 * t489 / 0.2D1) + t72 * t504
     #) * t116 * t88 / 0.2880D4 + (0.90D2 * t22 * (t481 - t511 * t479 * 
     #t489) - 0.180D3 * t61 * t504) * t161 * t163 / 0.2880D4
      t524 = FJET(XB1, XB2, s, 0.0D0, t463 * t464, -t463 * t467, t463 * 
     #t469, -s * t471 * t473 * t475 * x4, t523)
      t526 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t527 = s * t526
      t528 = t464 * x3
      t530 = t464 * t142
      t533 = t526 ** 2
      t539 = 0.1D1 / (-0.2D1 + t526)
      t540 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t541 = t539 * t540
      t542 = t379 * t142
      t543 = t533 ** 2
      t548 = log(-0.4D1 * t171 * t542 * x4 * t543)
      t550 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t555 = t539 * t550
      t565 = log(-0.4D1 * t203 * t380 * t142 * t543)
      t566 = t565 * t539
      t571 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t574 = t565 ** 2
      t586 = (0.90D2 * t22 * (t541 - t548 * t539 * t550) - 0.180D3 * t61
     # * t555) * t161 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t541 - t566
     # * t550) + 0.90D2 * t22 * (t539 * t571 - t566 * t540 + t574 * t539
     # * t550 / 0.2D1) + t72 * t555) * t161 * t116 / 0.2880D4
      t587 = FJET(XB1, XB2, s, t527 * t528, -t527 * t530, 0.0D0, -t527 *
     # t466, -s * t533 * t473 * t475 * x3, t586)
      t589 = KAPPA2(x1, x2, x3, x4, z)
      t590 = s * t589
      t595 = t589 ** 2
      t600 = cos(t7)
      t603 = sqrt(x3 * t142 * t315)
      t610 = 0.1D1 / (-0.2D1 + t589)
      t611 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t613 = t595 ** 2
      t618 = log(0.4D1 * t171 * t315 * t542 * t613)
      t620 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t628 = -0.90D2 * t22 * (t610 * t611 - t618 * t610 * t620) + 0.180D
     #3 * t61 * t610 * t620
      t632 = FJET(XB1, XB2, s, t590 * t528, -t590 * t530, -t590 * t467, 
     #t590 * t469, s * t595 * t473 * t475 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t600 * t603), t628 * t161 * t163 / 0.2880D4)
      ggbbH5n8e1 = t265 * t264 + t372 * t373 + t460 * t459 + t524 * t523
     # + t587 * t586 + t632 * t628 * t161 * t116 * t88 / 0.2880D4

      end function



      doubleprecision function ggbbH5n8e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t33 = -t18 + t32
      t35 = 0.1D1 / x3
      t37 = t35 * t27 * t29
      t40 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t41 = x3 * t7
      t42 = t10 * t13
      t43 = -0.1D1 + x3
      t47 = log(-0.4D1 * t41 * t42 * t43)
      t51 = log(0.4D1 * t41 * t42)
      t62 = t11 * t13
      t64 = log(0.4D1 * t62)
      t69 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t71 = t64 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t5
      t83 = t82 * t18
      t89 = log(0.4D1 * t42 * x4)
      t95 = t89 ** 2
      t104 = x3 * t10
      t107 = log(0.4D1 * t104 * t14)
      t109 = t13 * t43
      t113 = log(-0.4D1 * t104 * t109 * x4)
      t118 = -t33
      t125 = t104 * t13
      t127 = log(0.4D1 * t125)
      t131 = log(-0.4D1 * t104 * t109)
      t136 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t138 = t131 ** 2
      t142 = t127 ** 2
      t154 = log(0.4D1 * t42)
      t162 = t154 ** 2
      t181 = (0.90D2 * t5 * (-t6 + t17 * t18) + 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 + t5 * t33 * t37 / 0.32D2 + (0.90D2 * t5 * (t4
     #0 - t47 * t32 - t6 + t51 * t18) - 0.180D3 * t23 * t33) * t35 * t27
     # / 0.2880D4 - (-0.180D3 * t23 * (t6 - t64 * t18) + 0.90D2 * t5 * (
     #t69 - t64 * t6 + t71 * t18 / 0.2D1) + t83) * t27 / 0.2880D4 - (-0.
     #180D3 * t23 * (t6 - t89 * t18) + 0.90D2 * t5 * (t69 - t89 * t6 + t
     #95 * t18 / 0.2D1) + t83) * t29 / 0.5760D4 - (0.90D2 * t5 * (t6 - t
     #107 * t18 - t40 + t113 * t32) - 0.180D3 * t23 * t118) * t35 * t29 
     #/ 0.5760D4 - (-0.180D3 * t23 * (t6 - t127 * t18 - t40 + t131 * t32
     #) + 0.90D2 * t5 * (-t136 + t131 * t40 - t138 * t32 / 0.2D1 + t69 -
     # t127 * t6 + t142 * t18 / 0.2D1) + t82 * t118) * t35 / 0.5760D4 - 
     #(-0.180D3 * lh - 0.90D2 * t154) * t5 * t69 / 0.5760D4 - (t78 - t80
     # + 0.180D3 * t154 * lh + 0.45D2 * t162) * t5 * t6 / 0.5760D4 - (-0
     #.2884936567583026D3 - 0.120D3 * t77 * lh + 0.60D2 * lh * t79 - t15
     #4 * t81 - 0.90D2 * t162 * lh - 0.15D2 * t162 * t154) * t5 * t18 / 
     #0.5760D4
      t182 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t181)
      t185 = -0.1D1 + x4
      t187 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t188 = t14 * t185
      t191 = log(-0.4D1 * t11 * t188)
      t192 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t203 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t204 = -t203 + t192
      t208 = x4 * t185
      t211 = log(-0.4D1 * t42 * t208)
      t216 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t218 = t211 ** 2
      t228 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t232 = log(0.4D1 * t125 * t208 * t43)
      t236 = log(-0.4D1 * t104 * t188)
      t248 = (0.90D2 * t5 * (t187 - t191 * t192) - 0.180D3 * t23 * t192)
     # * t27 * t29 / 0.2880D4 + t5 * t204 * t37 / 0.32D2 - (0.180D3 * t2
     #3 * (t187 - t211 * t192) - 0.90D2 * t5 * (t216 - t211 * t187 + t21
     #8 * t192 / 0.2D1) - t82 * t192) * t29 / 0.5760D4 - (0.90D2 * t5 * 
     #(t228 - t232 * t203 - t187 + t236 * t192) + 0.180D3 * t23 * t204) 
     #* t35 * t29 / 0.5760D4
      t249 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t185, 0.0D0,
     # t248)
      t252 = -0.1D1 + x1
      t254 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t255 = t252 ** 2
      t256 = t7 * t255
      t260 = log(0.4D1 * t42 * t256 * x4)
      t261 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t267 = 0.180D3 * t23 * t261
      t279 = log(0.4D1 * t104 * t13 * t7 * t255)
      t290 = log(0.4D1 * t42 * t256)
      t295 = ggbbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t297 = t290 ** 2
      t307 = (0.90D2 * t5 * (t254 - t260 * t261) - t267) * t27 * t29 / 0
     #.2880D4 + t5 * t261 * t37 / 0.32D2 + (0.90D2 * t5 * (t254 - t279 *
     # t261) - t267) * t35 * t27 / 0.2880D4 - (0.180D3 * t23 * (t254 - t
     #290 * t261) - 0.90D2 * t5 * (t295 - t290 * t254 + t297 * t261 / 0.
     #2D1) - t82 * t261) * t27 / 0.2880D4
      t308 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t252, 0.0D0,
     # t307)
      t310 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t311 = s * t310
      t312 = t1 * x1
      t314 = t1 * t252
      t315 = t314 * x4
      t317 = t314 * t185
      t319 = t310 ** 2
      t321 = t1 ** 2
      t323 = x1 * t252
      t327 = 0.1D1 / (-0.2D1 + t310)
      t328 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t330 = t319 ** 2
      t335 = log(-0.4D1 * t62 * t208 * t330 * t255)
      t337 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t353 = (0.90D2 * t5 * (t327 * t328 - t335 * t327 * t337) - 0.180D3
     # * t23 * t327 * t337) * t27 * t29 / 0.2880D4 + t5 * t327 * t337 * 
     #t37 / 0.32D2
      t354 = FJET(XB1, XB2, s, 0.0D0, t311 * t312, -t311 * t315, t311 * 
     #t317, -s * t319 * t321 * t323 * x4, t353)
      t356 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t357 = s * t356
      t358 = t312 * x3
      t360 = t312 * t43
      t363 = t356 ** 2
      t369 = 0.1D1 / (-0.2D1 + t356)
      t371 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t375 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t377 = t363 ** 2
      t382 = log(-0.4D1 * t125 * t256 * t43 * t377)
      t395 = t5 * t369 * t371 * t37 / 0.32D2 + (0.90D2 * t5 * (t369 * t3
     #75 - t382 * t369 * t371) - 0.180D3 * t23 * t369 * t371) * t35 * t2
     #7 / 0.2880D4
      t396 = FJET(XB1, XB2, s, t357 * t358, -t357 * t360, 0.0D0, -t357 *
     # t314, -s * t363 * t321 * t323 * x3, t395)
      t398 = KAPPA2(x1, x2, x3, x4, z)
      t399 = s * t398
      t404 = t398 ** 2
      t409 = cos(t8)
      t412 = sqrt(x3 * t43 * t208)
      t419 = 0.1D1 / (-0.2D1 + t398)
      t421 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t425 = FJET(XB1, XB2, s, t399 * t358, -t399 * t360, -t399 * t315, 
     #t399 * t317, s * t404 * t321 * t323 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t409 * t412), -t5 * t419 * t421 * t37 / 0.32D2)
      ggbbH5n8e0 = t182 * t181 + t249 * t248 + t308 * t307 + t354 * t353
     # + t396 * t395 - t425 * t5 * t419 * t421 * t35 * t27 * t29 / 0.32D
     #2

      end function



      doubleprecision function ggbbH5n8em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t7 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t8 = t6 - t7
      t10 = 0.1D1 / x3
      t11 = 0.1D1 / x4
      t12 = t10 * t11
      t15 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t17 = sin(x2 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t19 = x3 * t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t19 * t21)
      t26 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t27 = -0.1D1 + x3
      t31 = log(-0.4D1 * t19 * t21 * t27)
      t36 = lh * t5
      t42 = ggbbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t46 = t18 * t21
      t48 = log(0.4D1 * t46)
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t60 = t48 ** 2
      t66 = x1 ** 2
      t70 = log(0.4D1 * t66 * t18 * t21)
      t76 = 0.180D3 * t36 * t6
      t78 = 0.1D1 / x1
      t82 = t78 * t11
      t87 = t10 * t78
      t92 = log(0.4D1 * t46 * x4)
      t100 = -t5 * t8 * t12 / 0.64D2 - (0.90D2 * t5 * (t15 - t24 * t6 - 
     #t26 + t31 * t7) - 0.180D3 * t36 * t8) * t10 / 0.5760D4 - t5 * t42 
     #/ 0.64D2 - (-0.180D3 * lh - 0.90D2 * t48) * t5 * t15 / 0.5760D4 - 
     #(0.180D3 * t54 - 0.30D2 * t56 + 0.180D3 * t48 * lh + 0.45D2 * t60)
     # * t5 * t6 / 0.5760D4 - (0.90D2 * t5 * (t15 - t70 * t6) - t76) * t
     #78 / 0.2880D4 - t5 * t6 * t82 / 0.32D2 - t5 * t8 * t87 / 0.32D2 - 
     #(0.90D2 * t5 * (t15 - t92 * t6) - t76) * t11 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t100)
      t104 = -0.1D1 + x4
      t106 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t110 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t114 = log(-0.4D1 * t46 * x4 * t104)
      t124 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t129 = t5 * t106 * t82 / 0.32D2 - (-0.90D2 * t5 * (t110 - t114 * t
     #106) + 0.180D3 * t36 * t106) * t11 / 0.5760D4 - t5 * (t124 - t106)
     # * t12 / 0.64D2
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t104, 0.0D0,
     # t129)
      t133 = -0.1D1 + x1
      t135 = ggbbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t136 = t133 ** 2
      t140 = log(0.4D1 * t46 * t66 * t136)
      t141 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t151 = t5 * t141
      t156 = -(-0.90D2 * t5 * (t135 - t140 * t141) + 0.180D3 * t36 * t14
     #1) * t78 / 0.2880D4 + t151 * t82 / 0.32D2 + t151 * t87 / 0.32D2
      t157 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t133, 0.0D0,
     # t156)
      t159 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t160 = s * t159
      t161 = t1 * x1
      t163 = t1 * t133
      t168 = t159 ** 2
      t170 = t1 ** 2
      t172 = x1 * t133
      t176 = 0.1D1 / (-0.2D1 + t159)
      t178 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t180 = t178 * t78 * t11
      t183 = FJET(XB1, XB2, s, 0.0D0, t160 * t161, -t160 * t163 * x4, t1
     #60 * t163 * t104, -s * t168 * t170 * t172 * x4, t5 * t176 * t180 /
     # 0.32D2)
      t188 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t189 = s * t188
      t195 = t188 ** 2
      t201 = 0.1D1 / (-0.2D1 + t188)
      t203 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t205 = t203 * t10 * t78
      t208 = FJET(XB1, XB2, s, t189 * t161 * x3, -t189 * t161 * t27, 0.0
     #D0, -t189 * t163, -s * t195 * t170 * t172 * x3, t5 * t201 * t205 /
     # 0.32D2)
      ggbbH5n8em1 = t101 * t100 + t130 * t129 + t157 * t156 + t183 * t5 
     #* t176 * t180 / 0.32D2 + t208 * t5 * t201 * t205 / 0.32D2

      end function



      doubleprecision function ggbbH5n8em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t17 = ggbbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t34 = 0.1D1 / x4
      t37 = -t7 * t8 / 0.32D2 - t5 * (t6 - t11) / x3 / 0.64D2 - t5 * t17
     # / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t28) * t5 * t6 / 0.5760D4 - 
     #t7 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t43 = ggbbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t47 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, t5 * t43 * t8 / 0.32D2)
      t55 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t5 * t55 * t34 / 0.64D2)
      ggbbH5n8em2 = t38 * t37 + t47 * t5 * t43 * t8 / 0.32D2 + t59 * t5 
     #* t55 * t34 / 0.64D2

      end function



      doubleprecision function ggbbH5n8em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = ggbbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D
     #0, -t5 * t6 / 0.64D2)
      ggbbH5n8em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function ggbbH5n8em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH51J1
      doubleprecision ggbbH51J2
      doubleprecision ggbbH51J3
      ggbbH5n8em4 = 0.0D0

      end function
  
 

      doubleprecision function ggbbH51J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t2 * t3
      t5 = 0.1D1 - z
      t6 = t4 * t5
      t7 = 0.1D1 - x1
      t8 = t7 * x4
      t9 = z ** 2
      t13 = t2 * z
      t14 = t3 ** 2
      t15 = t13 * t14
      t16 = t5 ** 2
      t17 = x1 ** 2
      t18 = t16 * t17
      t19 = x3 ** 2
      t20 = t18 * t19
      t23 = t7 ** 2
      t24 = t16 * t23
      t25 = x4 ** 2
      t26 = t24 * t25
      t32 = x1 * x3
      t36 = 0.1D1 - x4
      t37 = t36 ** 2
      t38 = t24 * t37
      t41 = 0.1D1 - x3
      t42 = x1 * t41
      t52 = t7 * t36
      t59 = t41 ** 2
      t60 = t18 * t59
      t66 = 0.4D1 * t6 * t8 * t9 + 0.3D1 * t15 * t20 + 0.3D1 * t15 * t26
     # - 0.6D1 * t6 * t8 * z - 0.6D1 * t6 * t32 * z + 0.3D1 * t15 * t38 
     #+ 0.4D1 * t6 * t42 * t9 - 0.6D1 * t6 * t42 * z + 0.4D1 * t6 * t32 
     #* t9 - 0.6D1 * t6 * t52 * z + 0.4D1 * t6 * t52 * t9 + 0.3D1 * t15 
     #* t60 + 0.3D1 * t13 - 0.4D1 * t2 * t9
      t70 = t2 * t14
      t73 = t5 * t7
      t80 = t2 * t14 * t3
      t81 = t16 * t5
      t83 = t81 * t23 * t7
      t88 = t81 * t17 * x1
      t92 = t5 * x1
      t112 = 0.2D1 * t2 * t9 * z - t2 - 0.3D1 * t70 * t20 + 0.3D1 * t4 *
     # t73 * x4 - 0.3D1 * t70 * t60 + t80 * t83 * t37 * t36 + t80 * t88 
     #* t19 * x3 + 0.3D1 * t4 * t92 * x3 + t80 * t88 * t59 * t41 + t80 *
     # t83 * t25 * x4 + 0.3D1 * t4 * t73 * t36 + 0.3D1 * t4 * t92 * t41 
     #- 0.3D1 * t70 * t26 - 0.3D1 * t70 * t38
      ggbbH51J1 = -0.16D2 / 0.3D1 * wd * (t66 + t112)

      end function
  
   
 

      doubleprecision function ggbbH51J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = 0.1D1 - x1
      t9 = t8 ** 2
      t10 = t7 * t9
      t11 = 0.1D1 - x4
      t12 = t11 ** 2
      t13 = t10 * t12
      t16 = t2 * t3
      t17 = t16 * t6
      t22 = t6 * t8
      t26 = t2 * z
      t27 = t26 * t4
      t28 = x1 ** 2
      t29 = t7 * t28
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t32 = t29 * t31
      t40 = t2 * t4 * t3
      t41 = t7 * t6
      t43 = t41 * t9 * t8
      t44 = x4 ** 2
      t56 = t41 * t28 * x1
      t57 = x3 ** 2
      t61 = 0.3D1 * t5 * t13 + 0.2D1 * t17 * x1 * x3 * z - 0.3D1 * t16 *
     # t22 * t11 - t27 * t32 - t27 * t13 + 0.2D1 * t17 * x1 * t30 * z - 
     #t40 * t43 * t44 * x4 + 0.2D1 * t17 * t8 * t11 * z - 0.3D1 * t16 * 
     #t22 * x4 - t26 - t40 * t56 * t57 * x3
      t62 = t10 * t44
      t65 = t6 * x1
      t72 = t29 * t57
      t89 = 0.3D1 * t5 * t62 - 0.3D1 * t16 * t65 * x3 - t40 * t56 * t31 
     #* t30 - t27 * t72 - t40 * t43 * t12 * t11 + 0.2D1 * t17 * t8 * x4 
     #* z - 0.3D1 * t16 * t65 * t30 + t2 + 0.3D1 * t5 * t72 - t27 * t62 
     #+ 0.3D1 * t5 * t32
      ggbbH51J2 = -0.16D2 / 0.3D1 * wd * (t61 + t89)

      end function
  
   
 

      doubleprecision function ggbbH51J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t2 * t3
      t5 = 0.1D1 - z
      t6 = t4 * t5
      t7 = 0.1D1 - x1
      t12 = t5 * t7
      t16 = t3 ** 2
      t17 = t2 * t16
      t18 = t5 ** 2
      t19 = t7 ** 2
      t20 = t18 * t19
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t28 = 0.1D1 - x3
      t32 = x1 ** 2
      t33 = t18 * t32
      t34 = t28 ** 2
      t37 = x4 ** 2
      t40 = t5 * x1
      t50 = x3 ** 2
      t56 = t6 * t7 * x4 * z - t2 * z - 0.2D1 * t4 * t12 * x4 + t17 * t2
     #0 * t22 + t6 * t7 * t21 * z + t6 * x1 * t28 * z + t17 * t33 * t34 
     #+ t17 * t20 * t37 - 0.2D1 * t4 * t40 * t28 - 0.2D1 * t4 * t40 * x3
     # + t6 * x1 * x3 * z + t17 * t33 * t50 - 0.2D1 * t4 * t12 * t21 + t
     #2
      ggbbH51J3 = -0.16D2 / 0.3D1 * wd * t56

      end function
  
 