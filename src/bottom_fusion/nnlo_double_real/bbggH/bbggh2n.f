  
      subroutine bbggh2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh21J1  
      doubleprecision bbggh21J2  
      doubleprecision bbggh21J3  
      doubleprecision bbggh2n1e1  
      doubleprecision bbggh2n1e0  
      doubleprecision bbggh2n1em1  
      doubleprecision bbggh2n1em2  
      doubleprecision bbggh2n1em3  
      doubleprecision bbggh2n1em4  
      doubleprecision bbggh2n2e1  
      doubleprecision bbggh2n2e0  
      doubleprecision bbggh2n2em1  
      doubleprecision bbggh2n2em2  
      doubleprecision bbggh2n2em3  
      doubleprecision bbggh2n2em4  
      doubleprecision bbggh2n3e1  
      doubleprecision bbggh2n3e0  
      doubleprecision bbggh2n3em1  
      doubleprecision bbggh2n3em2  
      doubleprecision bbggh2n3em3  
      doubleprecision bbggh2n3em4  
      doubleprecision bbggh2n4e1  
      doubleprecision bbggh2n4e0  
      doubleprecision bbggh2n4em1  
      doubleprecision bbggh2n4em2  
      doubleprecision bbggh2n4em3  
      doubleprecision bbggh2n4em4  
      doubleprecision bbggh2n5e1  
      doubleprecision bbggh2n5e0  
      doubleprecision bbggh2n5em1  
      doubleprecision bbggh2n5em2  
      doubleprecision bbggh2n5em3  
      doubleprecision bbggh2n5em4  
      doubleprecision bbggh2n6e1  
      doubleprecision bbggh2n6e0  
      doubleprecision bbggh2n6em1  
      doubleprecision bbggh2n6em2  
      doubleprecision bbggh2n6em3  
      doubleprecision bbggh2n6em4  
      doubleprecision bbggh2n7e1  
      doubleprecision bbggh2n7e0  
      doubleprecision bbggh2n7em1  
      doubleprecision bbggh2n7em2  
      doubleprecision bbggh2n7em3  
      doubleprecision bbggh2n7em4  
      doubleprecision bbggh2n8e1  
      doubleprecision bbggh2n8e0  
      doubleprecision bbggh2n8em1  
      doubleprecision bbggh2n8em2  
      doubleprecision bbggh2n8em3  
      doubleprecision bbggh2n8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh2n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh2n3e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh2n4e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=bbggh2n5e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=bbggh2n6e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=bbggh2n7e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=bbggh2n8e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh2n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh2n3e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh2n4e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=bbggh2n5e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=bbggh2n6e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=bbggh2n7e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=bbggh2n8e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh2n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh2n3em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh2n4em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=bbggh2n5em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=bbggh2n6em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=bbggh2n7em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=bbggh2n8em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh2n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh2n3em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh2n4em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=bbggh2n5em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=bbggh2n6em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=bbggh2n7em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=bbggh2n8em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh2n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh2n3em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh2n4em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=bbggh2n5em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=bbggh2n6em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=bbggh2n7em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=bbggh2n8em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh2n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbggh2n3em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbggh2n4em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=bbggh2n5em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=bbggh2n6em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=bbggh2n7em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=bbggh2n8em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t20 = t17 ** 2
      t21 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
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
      t57 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t56)
      t58 = t14 * x4
      t59 = -t56
      t60 = t58 * t59
      t63 = log(-0.4D1 * t12 * t60)
      t64 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t56)
      t68 = log(0.4D1 * t12 * t58)
      t73 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t56)
      t75 = t63 ** 2
      t79 = t68 ** 2
      t85 = t21 - t64
      t89 = 0.1D1 / x4
      t92 = x3 * t8
      t93 = t92 * t11
      t96 = log(-0.4D1 * t93 * t60)
      t98 = t11 * t14
      t99 = t98 * x4
      t102 = log(0.4D1 * t92 * t99)
      t111 = 0.1D1 / x3
      t112 = t111 * t89
      t115 = t92 * t98
      t117 = log(0.4D1 * t115)
      t123 = t117 ** 2
      t135 = log(0.4D1 * t98)
      t138 = t135 ** 2
      t147 = t138 * t135
      t153 = t29 ** 2
      t154 = t27 ** 2
      t160 = t138 ** 2
      t171 = log(0.4D1 * t99)
      t173 = t171 ** 2
      t176 = x4 * t59
      t179 = log(-0.4D1 * t98 * t176)
      t181 = t179 ** 2
      t206 = -t85
      t211 = x3 * t11
      t212 = t211 * t14
      t214 = log(0.4D1 * t212)
      t216 = t214 ** 2
      t239 = log(-0.4D1 * t211 * t60)
      t243 = log(0.4D1 * t211 * t58)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t264 = -(-0.180D3 * t6 * (t7 - t17 * t18 + t20 * t21 / 0.2D1) + t3
     #2 * (t18 - t17 * t21) + 0.90D2 * t5 * (-t17 * t7 + t20 * t18 / 0.2
     #D1 - t20 * t17 * t21 / 0.6D1) + t51) * t53 / 0.2880D4 - (-0.180D3 
     #* t6 * (-t57 + t63 * t64 + t18 - t68 * t21) + 0.90D2 * t5 * (-t73 
     #+ t63 * t57 - t75 * t64 / 0.2D1 + t7 - t68 * t18 + t79 * t21 / 0.2
     #D1) + t32 * t85) * t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (-t57 + t
     #96 * t64 + t18 - t102 * t21) - 0.180D3 * t6 * t85) * t53 * t112 / 
     #0.2880D4 + (-0.180D3 * t6 * (-t18 + t117 * t21) + 0.90D2 * t5 * (-
     #t7 + t117 * t18 - t123 * t21 / 0.2D1) - t32 * t21) * t53 * t111 / 
     #0.2880D4 - (t28 - t30 + 0.180D3 * t135 * lh + 0.45D2 * t138) * t5 
     #* t7 / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t135 * t31 
     #- 0.90D2 * t138 * lh - 0.15D2 * t147) * t5 * t18 / 0.5760D4 - (t15
     #3 + 0.60D2 * t154 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29
     # - t135 * t49 + 0.15D2 / 0.4D1 * t160 + t138 * t31 / 0.2D1 + 0.30D
     #2 * t147 * lh) * t5 * t21 / 0.5760D4 + (-0.180D3 * t6 * (-t7 + t17
     #1 * t18 - t173 * t21 / 0.2D1 + t73 - t179 * t57 + t181 * t64 / 0.2
     #D1) + t32 * (t57 - t179 * t64 - t18 + t171 * t21) + 0.90D2 * t5 * 
     #(t171 * t7 - t173 * t18 / 0.2D1 + t173 * t171 * t21 / 0.6D1 - t179
     # * t73 + t181 * t57 / 0.2D1 - t181 * t179 * t64 / 0.6D1) + t50 * t
     #206) * t89 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t214 * t18 + t216 *
     # t21 / 0.2D1) + t32 * (t18 - t214 * t21) + 0.90D2 * t5 * (-t214 * 
     #t7 + t216 * t18 / 0.2D1 - t216 * t214 * t21 / 0.6D1) + t51) * t111
     # / 0.5760D4 + (-0.180D3 * t6 * (t57 - t239 * t64 - t18 + t243 * t2
     #1) + 0.90D2 * t5 * (t73 - t239 * t57 + t249 * t64 / 0.2D1 - t7 + t
     #243 * t18 - t253 * t21 / 0.2D1) + t32 * t206) * t111 * t89 / 0.576
     #0D4
      t265 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t264)
      t267 = 0.1D1 - x1
      t268 = 0.1D1 - x3
      t269 = KAPPA2(t267, x2, t268, 0.10D1, z)
      t270 = s * t269
      t271 = -t267
      t272 = t1 * t271
      t273 = -t268
      t274 = t272 * t273
      t276 = t272 * x3
      t278 = t1 * x1
      t280 = t269 ** 2
      t282 = t1 ** 2
      t284 = t271 * x1
      t288 = 0.1D1 / (-0.2D1 + t269)
      t289 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, t268, 0.10D1)
      t290 = t288 * t289
      t291 = t271 ** 2
      t292 = t291 * t273
      t293 = t280 ** 2
      t298 = log(-0.4D1 * t115 * t292 * x4 * t293)
      t300 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, t268, 0.10D1)
      t305 = t288 * t300
      t316 = log(-0.4D1 * t93 * t14 * t291 * t273 * t293)
      t317 = t316 * t288
      t322 = bbggh21J3(s, XB1, XB2, z, lh, wd, t267, x2, t268, 0.10D1)
      t325 = t316 ** 2
      t337 = -(0.90D2 * t5 * (-t290 + t298 * t288 * t300) + 0.180D3 * t6
     # * t305) * t53 * t112 / 0.2880D4 + (-0.180D3 * t6 * (t290 - t317 *
     # t300) + 0.90D2 * t5 * (t288 * t322 - t317 * t289 + t325 * t288 * 
     #t300 / 0.2D1) + t32 * t305) * t53 * t111 / 0.2880D4
      t338 = FJET(XB1, XB2, s, t270 * t274, -t270 * t276, t270 * t278, 0
     #.0D0, -s * t280 * t282 * t284 * x3, t337)
      t340 = KAPPA2(t267, x2, t268, t56, z)
      t341 = s * t340
      t344 = t278 * t59
      t346 = t278 * x4
      t348 = t340 ** 2
      t353 = cos(t9)
      t356 = sqrt(x3 * t273 * t176)
      t363 = 0.1D1 / (-0.2D1 + t340)
      t364 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, t268, t56)
      t366 = t348 ** 2
      t371 = log(0.4D1 * t115 * t292 * t176 * t366)
      t373 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, t268, t56)
      t381 = 0.90D2 * t5 * (t363 * t364 - t371 * t363 * t373) - 0.180D3 
     #* t6 * t363 * t373
      t385 = FJET(XB1, XB2, s, t341 * t274, -t341 * t276, -t341 * t344, 
     #t341 * t346, s * t348 * t282 * t284 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t353 * t356), -t381 * t53 * t112 / 0.2880D4)
      t393 = bbggh21J3(s, XB1, XB2, z, lh, wd, t267, x2, 0.10D1, 0.10D1)
      t394 = t8 * t291
      t397 = log(0.4D1 * t98 * t394)
      t398 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, 0.10D1, 0.10D1)
      t400 = t397 ** 2
      t401 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, 0.10D1, 0.10D1)
      t422 = t394 * x4
      t425 = log(0.4D1 * t98 * t422)
      t431 = t425 ** 2
      t437 = t32 * t401
      t443 = log(0.4D1 * t212 * t422)
      t457 = log(0.4D1 * t211 * t14 * t8 * t291)
      t463 = t457 ** 2
      t473 = -(0.180D3 * t6 * (t393 - t397 * t398 + t400 * t401 / 0.2D1)
     # - t32 * (t398 - t397 * t401) - 0.90D2 * t5 * (-t397 * t393 + t400
     # * t398 / 0.2D1 - t400 * t397 * t401 / 0.6D1) - t50 * t401) * t53 
     #/ 0.2880D4 - (-0.180D3 * t6 * (-t398 + t425 * t401) + 0.90D2 * t5 
     #* (-t393 + t425 * t398 - t431 * t401 / 0.2D1) - t437) * t53 * t89 
     #/ 0.2880D4 - (0.90D2 * t5 * (-t398 + t443 * t401) + 0.180D3 * t6 *
     # t401) * t53 * t112 / 0.2880D4 + (-0.180D3 * t6 * (t398 - t457 * t
     #401) + 0.90D2 * t5 * (t393 - t457 * t398 + t463 * t401 / 0.2D1) + 
     #t437) * t53 * t111 / 0.2880D4
      t474 = FJET(XB1, XB2, s, -t2 * t271, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t473)
      t478 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t268, t56)
      t483 = log(0.4D1 * t93 * t58 * t59 * t273)
      t484 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t268, t56)
      t486 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t268, 0.10D1)
      t487 = t14 * t273
      t488 = t487 * x4
      t491 = log(-0.4D1 * t93 * t488)
      t492 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t268, 0.10D1)
      t497 = -t492 + t484
      t507 = log(-0.4D1 * t92 * t98 * t273)
      t512 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t268, 0.10D1)
      t514 = t507 ** 2
      t527 = log(-0.4D1 * t211 * t487)
      t529 = t527 ** 2
      t553 = log(-0.4D1 * t211 * t488)
      t558 = log(0.4D1 * t212 * t176 * t273)
      t564 = t553 ** 2
      t567 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t268, t56)
      t569 = t558 ** 2
      t581 = -(0.90D2 * t5 * (t478 - t483 * t484 - t486 + t491 * t492) -
     # 0.180D3 * t6 * t497) * t53 * t112 / 0.2880D4 + (-0.180D3 * t6 * (
     #t486 - t507 * t492) + 0.90D2 * t5 * (t512 - t507 * t486 + t514 * t
     #492 / 0.2D1) + t32 * t492) * t53 * t111 / 0.2880D4 - (0.180D3 * t6
     # * (t512 - t527 * t486 + t529 * t492 / 0.2D1) - t32 * (t486 - t527
     # * t492) - 0.90D2 * t5 * (-t527 * t512 + t529 * t486 / 0.2D1 - t52
     #9 * t527 * t492 / 0.6D1) - t50 * t492) * t111 / 0.5760D4 + (-0.180
     #D3 * t6 * (t486 - t553 * t492 - t478 + t558 * t484) + 0.90D2 * t5 
     #* (t512 - t553 * t486 + t564 * t492 / 0.2D1 - t567 + t558 * t478 -
     # t569 * t484 / 0.2D1) - t32 * t497) * t111 * t89 / 0.5760D4
      t582 = FJET(XB1, XB2, s, -t2 * t273, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t581)
      t584 = KAPPA2(t267, x2, 0.10D1, t56, z)
      t585 = s * t584
      t589 = t584 ** 2
      t595 = 0.1D1 / (-0.2D1 + t584)
      t596 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, 0.10D1, t56)
      t597 = t595 * t596
      t599 = t589 ** 2
      t601 = t291 * x4 * t59 * t599
      t604 = log(-0.4D1 * t15 * t601)
      t605 = t604 * t595
      t606 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, 0.10D1, t56)
      t611 = bbggh21J3(s, XB1, XB2, z, lh, wd, t267, x2, 0.10D1, t56)
      t614 = t604 ** 2
      t621 = t595 * t606
      t628 = log(-0.4D1 * t115 * t601)
      t640 = -(0.180D3 * t6 * (t597 - t605 * t606) - 0.90D2 * t5 * (t595
     # * t611 - t605 * t596 + t614 * t595 * t606 / 0.2D1) - t32 * t621) 
     #* t53 * t89 / 0.2880D4 - (-0.90D2 * t5 * (t597 - t628 * t595 * t60
     #6) + 0.180D3 * t6 * t621) * t53 * t112 / 0.2880D4
      t641 = FJET(XB1, XB2, s, -t585 * t272, 0.0D0, -t585 * t344, t585 *
     # t346, -s * t589 * t282 * t284 * x4, t640)
      bbggh2n1e1 = t265 * t264 + t338 * t337 - t385 * t381 * t53 * t111 
     #* t89 / 0.2880D4 + t474 * t473 + t582 * t581 + t641 * t640

      end function



      doubleprecision function bbggh2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 - x4
      t7 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t6)
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
      t21 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t6)
      t23 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t26 = log(0.4D1 * t12 * t15)
      t27 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t32 = lh * t5
      t33 = t27 - t21
      t37 = 0.1D1 / x1
      t39 = 0.1D1 / x4
      t43 = 0.1D1 / x3
      t45 = t37 * t43 * t39
      t48 = x3 * t8
      t49 = t11 * t14
      t52 = log(0.4D1 * t48 * t49)
      t63 = t12 * t14
      t65 = log(0.4D1 * t63)
      t70 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t27
      t88 = x4 * t16
      t91 = log(-0.4D1 * t49 * t88)
      t95 = log(0.4D1 * t49 * x4)
      t101 = t95 ** 2
      t104 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t6)
      t106 = t91 ** 2
      t112 = -t33
      t117 = x3 * t11
      t120 = log(-0.4D1 * t117 * t17)
      t124 = log(0.4D1 * t117 * t15)
      t135 = t117 * t14
      t137 = log(0.4D1 * t135)
      t143 = t137 ** 2
      t154 = log(0.4D1 * t49)
      t162 = t154 ** 2
      t181 = -(0.90D2 * t5 * (-t7 + t20 * t21 + t23 - t26 * t27) - 0.180
     #D3 * t32 * t33) * t37 * t39 / 0.2880D4 - t5 * t33 * t45 / 0.32D2 +
     # (0.90D2 * t5 * (-t23 + t52 * t27) + 0.180D3 * t32 * t27) * t37 * 
     #t43 / 0.2880D4 - (-0.180D3 * t32 * (t23 - t65 * t27) + 0.90D2 * t5
     # * (t70 - t65 * t23 + t72 * t27 / 0.2D1) + t84) * t37 / 0.2880D4 +
     # (-0.180D3 * t32 * (t7 - t91 * t21 - t23 + t95 * t27) + 0.90D2 * t
     #5 * (-t70 + t95 * t23 - t101 * t27 / 0.2D1 + t104 - t91 * t7 + t10
     #6 * t21 / 0.2D1) + t83 * t112) * t39 / 0.5760D4 + (0.90D2 * t5 * (
     #t7 - t120 * t21 - t23 + t124 * t27) - 0.180D3 * t32 * t112) * t43 
     #* t39 / 0.5760D4 - (-0.180D3 * t32 * (t23 - t137 * t27) + 0.90D2 *
     # t5 * (t70 - t137 * t23 + t143 * t27 / 0.2D1) + t84) * t43 / 0.576
     #0D4 - (-0.180D3 * lh - 0.90D2 * t154) * t5 * t70 / 0.5760D4 - (t79
     # - t81 + 0.180D3 * t154 * lh + 0.45D2 * t162) * t5 * t23 / 0.5760D
     #4 - (-0.2884936567583026D3 - 0.120D3 * t78 * lh + 0.60D2 * lh * t8
     #0 - t154 * t82 - 0.90D2 * t162 * lh - 0.15D2 * t162 * t154) * t5 *
     # t27 / 0.5760D4
      t182 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t181)
      t184 = 0.1D1 - x1
      t185 = 0.1D1 - x3
      t186 = KAPPA2(t184, x2, t185, 0.10D1, z)
      t187 = s * t186
      t188 = -t184
      t189 = t1 * t188
      t190 = -t185
      t191 = t189 * t190
      t193 = t189 * x3
      t195 = t1 * x1
      t197 = t186 ** 2
      t199 = t1 ** 2
      t201 = t188 * x1
      t205 = 0.1D1 / (-0.2D1 + t186)
      t207 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, t185, 0.10D1)
      t211 = bbggh21J2(s, XB1, XB2, z, lh, wd, t184, x2, t185, 0.10D1)
      t214 = t188 ** 2
      t216 = t197 ** 2
      t221 = log(-0.4D1 * t48 * t11 * t14 * t214 * t190 * t216)
      t234 = t5 * t205 * t207 * t45 / 0.32D2 + (0.90D2 * t5 * (t205 * t2
     #11 - t221 * t205 * t207) - 0.180D3 * t32 * t205 * t207) * t37 * t4
     #3 / 0.2880D4
      t235 = FJET(XB1, XB2, s, t187 * t191, -t187 * t193, t187 * t195, 0
     #.0D0, -s * t197 * t199 * t201 * x3, t234)
      t237 = KAPPA2(t184, x2, t185, t6, z)
      t238 = s * t237
      t241 = t195 * t16
      t243 = t195 * x4
      t245 = t237 ** 2
      t250 = cos(t9)
      t253 = sqrt(x3 * t190 * t88)
      t260 = 0.1D1 / (-0.2D1 + t237)
      t262 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, t185, t6)
      t266 = FJET(XB1, XB2, s, t238 * t191, -t238 * t193, -t238 * t241, 
     #t238 * t243, s * t245 * t199 * t201 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t250 * t253), -t5 * t260 * t262 * t45 / 0.32D2)
      t276 = bbggh21J2(s, XB1, XB2, z, lh, wd, t184, x2, 0.10D1, 0.10D1)
      t277 = t8 * t214
      t281 = log(0.4D1 * t49 * t277 * x4)
      t282 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, 0.10D1, 0.10D1)
      t288 = 0.180D3 * t32 * t282
      t300 = log(0.4D1 * t117 * t14 * t8 * t214)
      t311 = log(0.4D1 * t49 * t277)
      t316 = bbggh21J3(s, XB1, XB2, z, lh, wd, t184, x2, 0.10D1, 0.10D1)
      t318 = t311 ** 2
      t328 = -(0.90D2 * t5 * (-t276 + t281 * t282) + t288) * t37 * t39 /
     # 0.2880D4 + t5 * t282 * t45 / 0.32D2 + (0.90D2 * t5 * (t276 - t300
     # * t282) - t288) * t37 * t43 / 0.2880D4 - (0.180D3 * t32 * (t276 -
     # t311 * t282) - 0.90D2 * t5 * (t316 - t311 * t276 + t318 * t282 / 
     #0.2D1) - t83 * t282) * t37 / 0.2880D4
      t329 = FJET(XB1, XB2, s, -t2 * t188, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t328)
      t333 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t185, 0.10D1)
      t334 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t185, t6)
      t335 = -t333 + t334
      t339 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t185, 0.10D1)
      t343 = log(-0.4D1 * t48 * t49 * t190)
      t354 = t14 * t190
      t358 = log(-0.4D1 * t117 * t354 * x4)
      t360 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t185, t6)
      t364 = log(0.4D1 * t135 * t88 * t190)
      t378 = log(-0.4D1 * t117 * t354)
      t383 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t185, 0.10D1)
      t385 = t378 ** 2
      t395 = -t5 * t335 * t45 / 0.32D2 + (0.90D2 * t5 * (t339 - t343 * t
     #333) - 0.180D3 * t32 * t333) * t37 * t43 / 0.2880D4 + (0.90D2 * t5
     # * (t339 - t358 * t333 - t360 + t364 * t334) + 0.180D3 * t32 * t33
     #5) * t43 * t39 / 0.5760D4 - (0.180D3 * t32 * (t339 - t378 * t333) 
     #- 0.90D2 * t5 * (t383 - t378 * t339 + t385 * t333 / 0.2D1) - t83 *
     # t333) * t43 / 0.5760D4
      t396 = FJET(XB1, XB2, s, -t2 * t190, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t395)
      t398 = KAPPA2(t184, x2, 0.10D1, t6, z)
      t399 = s * t398
      t403 = t398 ** 2
      t409 = 0.1D1 / (-0.2D1 + t398)
      t410 = bbggh21J2(s, XB1, XB2, z, lh, wd, t184, x2, 0.10D1, t6)
      t413 = t403 ** 2
      t418 = log(-0.4D1 * t63 * t214 * x4 * t16 * t413)
      t420 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, 0.10D1, t6)
      t436 = -(-0.90D2 * t5 * (t409 * t410 - t418 * t409 * t420) + 0.180
     #D3 * t32 * t409 * t420) * t37 * t39 / 0.2880D4 + t5 * t409 * t420 
     #* t45 / 0.32D2
      t437 = FJET(XB1, XB2, s, -t399 * t189, 0.0D0, -t399 * t241, t399 *
     # t243, -s * t403 * t199 * t201 * x4, t436)
      bbggh2n1e0 = t182 * t181 + t235 * t234 - t266 * t5 * t260 * t262 *
     # t37 * t43 * t39 / 0.32D2 + t329 * t328 + t396 * t395 + t437 * t43
     #6

      end function



      doubleprecision function bbggh2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t7 * t10 * t13)
      t17 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = 0.1D1 - x4
      t30 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t29)
      t31 = t17 - t30
      t33 = 0.1D1 / x4
      t34 = t26 * t33
      t38 = 0.1D1 / x3
      t39 = t26 * t38
      t42 = -t31
      t44 = t38 * t33
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t13)
      t58 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t62 = t10 * t13
      t64 = log(0.4D1 * t62)
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t76 = t64 ** 2
      t82 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, t29)
      t83 = -t29
      t87 = log(-0.4D1 * t62 * x4 * t83)
      t91 = log(0.4D1 * t62 * x4)
      t101 = -(0.90D2 * t5 * (t6 - t16 * t17) - t24) * t26 / 0.2880D4 - 
     #t5 * t31 * t34 / 0.32D2 - t5 * t17 * t39 / 0.32D2 + t5 * t42 * t44
     # / 0.64D2 - (0.90D2 * t5 * (t6 - t50 * t17) - t24) * t38 / 0.5760D
     #4 - t5 * t58 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t64) * t5 * t6 /
     # 0.5760D4 - (0.180D3 * t70 - 0.30D2 * t72 + 0.180D3 * t64 * lh + 0
     #.45D2 * t76) * t5 * t17 / 0.5760D4 + (0.90D2 * t5 * (t82 - t87 * t
     #30 - t6 + t91 * t17) - 0.180D3 * t22 * t42) * t33 / 0.5760D4
      t102 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t101)
      t104 = 0.1D1 - x1
      t105 = 0.1D1 - x3
      t106 = KAPPA2(t104, x2, t105, 0.10D1, z)
      t107 = s * t106
      t108 = -t104
      t109 = t1 * t108
      t110 = -t105
      t115 = t1 * x1
      t117 = t106 ** 2
      t119 = t1 ** 2
      t121 = t108 * x1
      t125 = 0.1D1 / (-0.2D1 + t106)
      t127 = bbggh21J1(s, XB1, XB2, z, lh, wd, t104, x2, t105, 0.10D1)
      t129 = t127 * t26 * t38
      t132 = FJET(XB1, XB2, s, t107 * t109 * t110, -t107 * t109 * x3, t1
     #07 * t115, 0.0D0, -s * t117 * t119 * t121 * x3, t5 * t125 * t129 /
     # 0.32D2)
      t139 = bbggh21J2(s, XB1, XB2, z, lh, wd, t104, x2, 0.10D1, 0.10D1)
      t140 = t108 ** 2
      t144 = log(0.4D1 * t62 * t7 * t140)
      t145 = bbggh21J1(s, XB1, XB2, z, lh, wd, t104, x2, 0.10D1, 0.10D1)
      t155 = t5 * t145
      t160 = -(-0.90D2 * t5 * (t139 - t144 * t145) + 0.180D3 * t22 * t14
     #5) * t26 / 0.2880D4 + t155 * t34 / 0.32D2 + t155 * t39 / 0.32D2
      t161 = FJET(XB1, XB2, s, -t2 * t108, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t160)
      t165 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t105, 0.10D1)
      t169 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t105, t29)
      t174 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t105, 0.10D1)
      t178 = log(-0.4D1 * t47 * t13 * t110)
      t188 = t5 * t165 * t39 / 0.32D2 + t5 * (t165 - t169) * t44 / 0.64D
     #2 - (-0.90D2 * t5 * (t174 - t178 * t165) + 0.180D3 * t22 * t165) *
     # t38 / 0.5760D4
      t189 = FJET(XB1, XB2, s, -t2 * t110, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t188)
      t191 = KAPPA2(t104, x2, 0.10D1, t29, z)
      t192 = s * t191
      t198 = t191 ** 2
      t204 = 0.1D1 / (-0.2D1 + t191)
      t206 = bbggh21J1(s, XB1, XB2, z, lh, wd, t104, x2, 0.10D1, t29)
      t208 = t206 * t26 * t33
      t211 = FJET(XB1, XB2, s, -t192 * t109, 0.0D0, -t192 * t115 * t83, 
     #t192 * t115 * x4, -s * t198 * t119 * t121 * x4, t5 * t204 * t208 /
     # 0.32D2)
      bbggh2n1em1 = t102 * t101 + t132 * t5 * t125 * t129 / 0.32D2 + t16
     #1 * t160 + t189 * t188 + t211 * t5 * t204 * t208 / 0.32D2

      end function



      doubleprecision function bbggh2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1
     #)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t32 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.1D1 
     #- x4)
      t38 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 + t5 * (t32 - 
     #t6) / x4 / 0.64D2
      t39 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t41 = -0.1D1 + x1
      t45 = bbggh21J1(s, XB1, XB2, z, lh, wd, -t41, x2, 0.10D1, 0.10D1)
      t49 = FJET(XB1, XB2, s, -t2 * t41, 0.0D0, t2 * x1, 0.0D0, 0.0D0, t
     #5 * t45 * t8 / 0.32D2)
      t54 = -0.1D1 + x3
      t58 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, -t54, 0.10D1)
      t62 = FJET(XB1, XB2, s, -t2 * t54, t2 * x3, 0.0D0, 0.0D0, 0.0D0, t
     #5 * t58 * t11 / 0.64D2)
      bbggh2n1em2 = t39 * t38 + t49 * t5 * t45 * t8 / 0.32D2 + t62 * t5 
     #* t58 * t11 / 0.64D2

      end function



      doubleprecision function bbggh2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.10D1)
      t9 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n1em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n1em4 = 0.0D0

      end function


      doubleprecision function bbggh2n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t20 = t17 ** 2
      t21 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
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
      t61 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t62 = -0.1D1 + x4
      t63 = t56 * t62
      t66 = log(-0.4D1 * t12 * t63)
      t67 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t73 = t59 ** 2
      t76 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t78 = t66 ** 2
      t84 = t21 - t67
      t88 = 0.1D1 / x4
      t91 = x3 * t8
      t92 = t91 * t11
      t95 = log(-0.4D1 * t92 * t63)
      t97 = t11 * t14
      t98 = t97 * x4
      t101 = log(0.4D1 * t91 * t98)
      t110 = 0.1D1 / x3
      t111 = t110 * t88
      t114 = t91 * t97
      t116 = log(0.4D1 * t114)
      t122 = t116 ** 2
      t134 = log(0.4D1 * t97)
      t137 = t134 ** 2
      t146 = t137 * t134
      t152 = t29 ** 2
      t153 = t27 ** 2
      t159 = t137 ** 2
      t169 = t62 * x4
      t172 = log(-0.4D1 * t97 * t169)
      t174 = t172 ** 2
      t178 = log(0.4D1 * t98)
      t180 = t178 ** 2
      t205 = -t84
      t210 = x3 * t11
      t211 = t210 * t14
      t213 = log(0.4D1 * t211)
      t215 = t213 ** 2
      t238 = log(-0.4D1 * t210 * t63)
      t242 = log(0.4D1 * t210 * t56)
      t248 = t242 ** 2
      t252 = t238 ** 2
      t263 = (-0.180D3 * t6 * (-t7 + t17 * t18 - t20 * t21 / 0.2D1) + t3
     #2 * (-t18 + t17 * t21) + 0.90D2 * t5 * (t17 * t7 - t20 * t18 / 0.2
     #D1 + t20 * t17 * t21 / 0.6D1) - t51) * t53 / 0.2880D4 - (-0.180D3 
     #* t6 * (t18 - t59 * t21 - t61 + t66 * t67) + 0.90D2 * t5 * (t7 - t
     #59 * t18 + t73 * t21 / 0.2D1 - t76 + t66 * t61 - t78 * t67 / 0.2D1
     #) + t32 * t84) * t53 * t88 / 0.2880D4 - (0.90D2 * t5 * (-t61 + t95
     # * t67 + t18 - t101 * t21) - 0.180D3 * t6 * t84) * t53 * t111 / 0.
     #2880D4 + (-0.180D3 * t6 * (-t18 + t116 * t21) + 0.90D2 * t5 * (-t7
     # + t116 * t18 - t122 * t21 / 0.2D1) - t32 * t21) * t53 * t110 / 0.
     #2880D4 - (t28 - t30 + 0.180D3 * t134 * lh + 0.45D2 * t137) * t5 * 
     #t7 / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t134 * t31 - 
     #0.90D2 * t137 * lh - 0.15D2 * t146) * t5 * t18 / 0.5760D4 - (t152 
     #+ 0.60D2 * t153 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29 -
     # t134 * t49 + 0.15D2 / 0.4D1 * t159 + t137 * t31 / 0.2D1 + 0.30D2 
     #* t146 * lh) * t5 * t21 / 0.5760D4 + (-0.180D3 * t6 * (t76 - t172 
     #* t61 + t174 * t67 / 0.2D1 - t7 + t178 * t18 - t180 * t21 / 0.2D1)
     # + t32 * (-t18 + t178 * t21 + t61 - t172 * t67) + 0.90D2 * t5 * (-
     #t172 * t76 + t174 * t61 / 0.2D1 - t174 * t172 * t67 / 0.6D1 + t178
     # * t7 - t180 * t18 / 0.2D1 + t180 * t178 * t21 / 0.6D1) + t50 * t2
     #05) * t88 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t213 * t18 + t215 * 
     #t21 / 0.2D1) + t32 * (t18 - t213 * t21) + 0.90D2 * t5 * (-t213 * t
     #7 + t215 * t18 / 0.2D1 - t215 * t213 * t21 / 0.6D1) + t51) * t110 
     #/ 0.5760D4 + (-0.180D3 * t6 * (t61 - t238 * t67 - t18 + t242 * t21
     #) + 0.90D2 * t5 * (-t7 + t242 * t18 - t248 * t21 / 0.2D1 + t76 - t
     #238 * t61 + t252 * t67 / 0.2D1) + t32 * t205) * t110 * t88 / 0.576
     #0D4
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
      t288 = bbggh21J2(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.0D0)
      t289 = t287 * t288
      t290 = t270 ** 2
      t291 = t290 * t272
      t292 = t279 ** 2
      t297 = log(-0.4D1 * t114 * t291 * x4 * t292)
      t299 = bbggh21J1(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.0D0)
      t304 = t287 * t299
      t310 = t14 * t290
      t315 = log(-0.4D1 * t92 * t310 * t272 * t292)
      t316 = t315 * t287
      t321 = bbggh21J3(s, XB1, XB2, z, lh, wd, t266, x2, t267, 0.0D0)
      t324 = t315 ** 2
      t336 = -(0.90D2 * t5 * (-t289 + t297 * t287 * t299) + 0.180D3 * t6
     # * t304) * t53 * t111 / 0.2880D4 + (-0.180D3 * t6 * (t289 - t316 *
     # t299) + 0.90D2 * t5 * (t287 * t321 - t316 * t288 + t324 * t287 * 
     #t299 / 0.2D1) + t32 * t304) * t53 * t110 / 0.2880D4
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
      t363 = bbggh21J2(s, XB1, XB2, z, lh, wd, t266, x2, t267, x4)
      t365 = t347 ** 2
      t370 = log(0.4D1 * t114 * t291 * t169 * t365)
      t372 = bbggh21J1(s, XB1, XB2, z, lh, wd, t266, x2, t267, x4)
      t380 = 0.90D2 * t5 * (t362 * t363 - t370 * t362 * t372) - 0.180D3 
     #* t6 * t362 * t372
      t384 = FJET(XB1, XB2, s, t340 * t273, -t340 * t275, t340 * t343, -
     #t340 * t345, s * t347 * t281 * t283 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t352 * t355), -t380 * t53 * t111 / 0.2880D4)
      t392 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, x4)
      t397 = log(0.4D1 * t92 * t56 * t62 * t272)
      t398 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, x4)
      t400 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.0D0)
      t401 = t14 * t272
      t402 = t401 * x4
      t405 = log(-0.4D1 * t92 * t402)
      t406 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.0D0)
      t411 = -t406 + t398
      t421 = log(-0.4D1 * t91 * t97 * t272)
      t426 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, 0.0D0)
      t428 = t421 ** 2
      t441 = log(-0.4D1 * t210 * t401)
      t443 = t441 ** 2
      t467 = log(-0.4D1 * t210 * t402)
      t472 = log(0.4D1 * t211 * t169 * t272)
      t477 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t267, x4)
      t479 = t472 ** 2
      t483 = t467 ** 2
      t495 = -(0.90D2 * t5 * (t392 - t397 * t398 - t400 + t405 * t406) -
     # 0.180D3 * t6 * t411) * t53 * t111 / 0.2880D4 + (-0.180D3 * t6 * (
     #t400 - t421 * t406) + 0.90D2 * t5 * (t426 - t421 * t400 + t428 * t
     #406 / 0.2D1) + t32 * t406) * t53 * t110 / 0.2880D4 - (0.180D3 * t6
     # * (t426 - t441 * t400 + t443 * t406 / 0.2D1) - t32 * (t400 - t441
     # * t406) - 0.90D2 * t5 * (-t441 * t426 + t443 * t400 / 0.2D1 - t44
     #3 * t441 * t406 / 0.6D1) - t50 * t406) * t110 / 0.5760D4 + (-0.180
     #D3 * t6 * (t400 - t467 * t406 - t392 + t472 * t398) + 0.90D2 * t5 
     #* (-t477 + t472 * t392 - t479 * t398 / 0.2D1 + t426 - t467 * t400 
     #+ t483 * t406 / 0.2D1) - t32 * t411) * t110 * t88 / 0.5760D4
      t496 = FJET(XB1, XB2, s, -t2 * t272, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t495)
      t498 = KAPPA2(t266, x2, 0.10D1, 0.0D0, z)
      t499 = s * t498
      t502 = t498 ** 2
      t508 = 0.1D1 / (-0.2D1 + t498)
      t509 = bbggh21J3(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.0D0)
      t510 = t508 * t509
      t511 = t502 ** 2
      t512 = t310 * t511
      t515 = log(0.4D1 * t12 * t512)
      t516 = t515 * t508
      t517 = bbggh21J2(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.0D0)
      t519 = t515 ** 2
      t520 = t519 * t508
      t521 = bbggh21J1(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, 0.0D0)
      t527 = t508 * t517
      t541 = t508 * t521
      t545 = t290 * x4
      t549 = log(0.4D1 * t15 * t545 * t511)
      t550 = t549 * t508
      t556 = t549 ** 2
      t563 = t32 * t541
      t571 = log(0.4D1 * t92 * t310 * x4 * t511)
      t584 = log(0.4D1 * t92 * t512)
      t585 = t584 * t508
      t591 = t584 ** 2
      t602 = (0.180D3 * t6 * (t510 - t516 * t517 + t520 * t521 / 0.2D1) 
     #- t32 * (t527 - t516 * t521) - 0.90D2 * t5 * (-t516 * t509 + t520 
     #* t517 / 0.2D1 - t519 * t515 * t508 * t521 / 0.6D1) - t50 * t541) 
     #* t53 / 0.2880D4 - (-0.180D3 * t6 * (t527 - t550 * t521) + 0.90D2 
     #* t5 * (t510 - t550 * t517 + t556 * t508 * t521 / 0.2D1) + t563) *
     # t53 * t88 / 0.2880D4 - (0.90D2 * t5 * (t527 - t571 * t508 * t521)
     # - 0.180D3 * t6 * t541) * t53 * t111 / 0.2880D4 + (0.180D3 * t6 * 
     #(t527 - t585 * t521) - 0.90D2 * t5 * (t510 - t585 * t517 + t591 * 
     #t508 * t521 / 0.2D1) - t563) * t53 * t110 / 0.2880D4
      t603 = FJET(XB1, XB2, s, -t499 * t271, 0.0D0, 0.0D0, t499 * t277, 
     #-s * t502 * t281 * t270 * x1, t602)
      t605 = KAPPA2(t266, x2, 0.10D1, x4, z)
      t606 = s * t605
      t610 = t605 ** 2
      t616 = 0.1D1 / (-0.2D1 + t605)
      t617 = bbggh21J2(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, x4)
      t618 = t616 * t617
      t619 = t610 ** 2
      t621 = t545 * t62 * t619
      t624 = log(-0.4D1 * t15 * t621)
      t625 = t624 * t616
      t626 = bbggh21J1(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, x4)
      t631 = bbggh21J3(s, XB1, XB2, z, lh, wd, t266, x2, 0.10D1, x4)
      t634 = t624 ** 2
      t641 = t616 * t626
      t648 = log(-0.4D1 * t114 * t621)
      t660 = -(0.180D3 * t6 * (t618 - t625 * t626) - 0.90D2 * t5 * (t616
     # * t631 - t625 * t617 + t634 * t616 * t626 / 0.2D1) - t32 * t641) 
     #* t53 * t88 / 0.2880D4 - (-0.90D2 * t5 * (t618 - t648 * t616 * t62
     #6) + 0.180D3 * t6 * t641) * t53 * t111 / 0.2880D4
      t661 = FJET(XB1, XB2, s, -t606 * t271, 0.0D0, t606 * t343, -t606 *
     # t345, s * t610 * t281 * t283 * t62, t660)
      bbggh2n2e1 = t264 * t263 + t337 * t336 - t384 * t380 * t53 * t110 
     #* t88 / 0.2880D4 + t496 * t495 + t603 * t602 + t661 * t660

      end function



      doubleprecision function bbggh2n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t20 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t21 = -0.1D1 + x4
      t22 = t14 * t21
      t25 = log(-0.4D1 * t11 * t22)
      t26 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t31 = lh * t5
      t32 = t18 - t26
      t36 = 0.1D1 / x1
      t38 = 0.1D1 / x4
      t42 = 0.1D1 / x3
      t44 = t36 * t42 * t38
      t47 = x3 * t7
      t48 = t10 * t13
      t51 = log(0.4D1 * t47 * t48)
      t62 = t11 * t13
      t64 = log(0.4D1 * t62)
      t69 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
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
      t99 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
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
      t180 = -(0.90D2 * t5 * (t6 - t17 * t18 - t20 + t25 * t26) - 0.180D
     #3 * t31 * t32) * t36 * t38 / 0.2880D4 - t5 * t32 * t44 / 0.32D2 + 
     #(0.90D2 * t5 * (-t6 + t51 * t18) + 0.180D3 * t31 * t18) * t36 * t4
     #2 / 0.2880D4 + (-0.180D3 * t31 * (-t6 + t64 * t18) + 0.90D2 * t5 *
     # (-t69 + t64 * t6 - t71 * t18 / 0.2D1) - t83) * t36 / 0.2880D4 + (
     #-0.180D3 * t31 * (-t6 + t89 * t18 + t20 - t94 * t26) + 0.90D2 * t5
     # * (t99 - t94 * t20 + t101 * t26 / 0.2D1 - t69 + t89 * t6 - t105 *
     # t18 / 0.2D1) + t82 * t111) * t38 / 0.5760D4 + (0.90D2 * t5 * (t20
     # - t119 * t26 - t6 + t123 * t18) - 0.180D3 * t31 * t111) * t42 * t
     #38 / 0.5760D4 - (-0.180D3 * t31 * (t6 - t136 * t18) + 0.90D2 * t5 
     #* (t69 - t136 * t6 + t142 * t18 / 0.2D1) + t83) * t42 / 0.5760D4 -
     # (-0.180D3 * lh - 0.90D2 * t153) * t5 * t69 / 0.5760D4 - (t78 - t8
     #0 + 0.180D3 * t153 * lh + 0.45D2 * t161) * t5 * t6 / 0.5760D4 - (-
     #0.2884936567583026D3 - 0.120D3 * t77 * lh + 0.60D2 * lh * t79 - t1
     #53 * t81 - 0.90D2 * t161 * lh - 0.15D2 * t161 * t153) * t5 * t18 /
     # 0.5760D4
      t181 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t180)
      t183 = 0.1D1 - x1
      t184 = 0.1D1 - x3
      t185 = KAPPA2(t183, x2, t184, 0.0D0, z)
      t186 = s * t185
      t187 = -t183
      t188 = t1 * t187
      t189 = -t184
      t190 = t189 * t188
      t192 = t188 * x3
      t194 = t1 * x1
      t196 = t185 ** 2
      t198 = t1 ** 2
      t200 = t187 * x1
      t204 = 0.1D1 / (-0.2D1 + t185)
      t206 = bbggh21J1(s, XB1, XB2, z, lh, wd, t183, x2, t184, 0.0D0)
      t210 = bbggh21J2(s, XB1, XB2, z, lh, wd, t183, x2, t184, 0.0D0)
      t212 = t47 * t10
      t213 = t187 ** 2
      t214 = t13 * t213
      t215 = t196 ** 2
      t220 = log(-0.4D1 * t212 * t214 * t189 * t215)
      t233 = t5 * t204 * t206 * t44 / 0.32D2 + (0.90D2 * t5 * (t204 * t2
     #10 - t220 * t204 * t206) - 0.180D3 * t31 * t204 * t206) * t36 * t4
     #2 / 0.2880D4
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
      t261 = bbggh21J1(s, XB1, XB2, z, lh, wd, t183, x2, t184, x4)
      t265 = FJET(XB1, XB2, s, t237 * t190, -t237 * t192, t237 * t240, -
     #t237 * t242, s * t244 * t198 * t200 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t249 * t252), -t5 * t259 * t261 * t44 / 0.32D2)
      t275 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, 0.0D0)
      t276 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, x4)
      t277 = -t275 + t276
      t281 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, 0.0D0)
      t285 = log(-0.4D1 * t47 * t48 * t189)
      t296 = t13 * t189
      t300 = log(-0.4D1 * t116 * t296 * x4)
      t302 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, x4)
      t306 = log(0.4D1 * t134 * t91 * t189)
      t320 = log(-0.4D1 * t116 * t296)
      t325 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t184, 0.0D0)
      t327 = t320 ** 2
      t337 = -t5 * t277 * t44 / 0.32D2 + (0.90D2 * t5 * (t281 - t285 * t
     #275) - 0.180D3 * t31 * t275) * t36 * t42 / 0.2880D4 + (0.90D2 * t5
     # * (t281 - t300 * t275 - t302 + t306 * t276) + 0.180D3 * t31 * t27
     #7) * t42 * t38 / 0.5760D4 - (0.180D3 * t31 * (t281 - t320 * t275) 
     #- 0.90D2 * t5 * (t325 - t320 * t281 + t327 * t275 / 0.2D1) - t82 *
     # t275) * t42 / 0.5760D4
      t338 = FJET(XB1, XB2, s, -t2 * t189, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t337)
      t340 = KAPPA2(t183, x2, 0.10D1, 0.0D0, z)
      t341 = s * t340
      t344 = t340 ** 2
      t350 = 0.1D1 / (-0.2D1 + t340)
      t351 = bbggh21J2(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, 0.0D0)
      t352 = t350 * t351
      t353 = t213 * x4
      t354 = t344 ** 2
      t358 = log(0.4D1 * t62 * t353 * t354)
      t360 = bbggh21J1(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, 0.0D0)
      t365 = t350 * t360
      t367 = 0.180D3 * t31 * t365
      t376 = t214 * t354
      t379 = log(0.4D1 * t212 * t376)
      t391 = log(0.4D1 * t11 * t376)
      t392 = t391 * t350
      t397 = bbggh21J3(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, 0.0D0)
      t400 = t391 ** 2
      t411 = -(0.90D2 * t5 * (t352 - t358 * t350 * t360) - t367) * t36 *
     # t38 / 0.2880D4 - t5 * t350 * t360 * t44 / 0.32D2 + (-0.90D2 * t5 
     #* (t352 - t379 * t350 * t360) + t367) * t36 * t42 / 0.2880D4 + (0.
     #180D3 * t31 * (t352 - t392 * t360) - 0.90D2 * t5 * (t350 * t397 - 
     #t392 * t351 + t400 * t350 * t360 / 0.2D1) - t82 * t365) * t36 / 0.
     #2880D4
      t412 = FJET(XB1, XB2, s, -t341 * t188, 0.0D0, 0.0D0, t341 * t194, 
     #-s * t344 * t198 * t187 * x1, t411)
      t414 = KAPPA2(t183, x2, 0.10D1, x4, z)
      t415 = s * t414
      t419 = t414 ** 2
      t425 = 0.1D1 / (-0.2D1 + t414)
      t426 = bbggh21J2(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, x4)
      t428 = t419 ** 2
      t433 = log(-0.4D1 * t62 * t353 * t21 * t428)
      t435 = bbggh21J1(s, XB1, XB2, z, lh, wd, t183, x2, 0.10D1, x4)
      t451 = -(-0.90D2 * t5 * (t425 * t426 - t433 * t425 * t435) + 0.180
     #D3 * t31 * t425 * t435) * t36 * t38 / 0.2880D4 + t5 * t425 * t435 
     #* t44 / 0.32D2
      t452 = FJET(XB1, XB2, s, -t415 * t188, 0.0D0, t415 * t240, -t415 *
     # t242, s * t419 * t198 * t200 * t21, t451)
      bbggh2n2e0 = t181 * t180 + t234 * t233 - t265 * t5 * t259 * t261 *
     # t36 * t42 * t38 / 0.32D2 + t338 * t337 + t412 * t411 + t452 * t45
     #1

      end function



      doubleprecision function bbggh2n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t30 = t17 - t29
      t32 = 0.1D1 / x4
      t37 = 0.1D1 / x3
      t38 = t26 * t37
      t41 = -t30
      t43 = t37 * t32
      t46 = x3 * t10
      t49 = log(0.4D1 * t46 * t13)
      t57 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t61 = t10 * t13
      t63 = log(0.4D1 * t61)
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t75 = t63 ** 2
      t83 = log(0.4D1 * t61 * x4)
      t85 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t86 = -0.1D1 + x4
      t90 = log(-0.4D1 * t61 * x4 * t86)
      t100 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.2880D4 - 
     #t5 * t30 * t26 * t32 / 0.32D2 - t5 * t17 * t38 / 0.32D2 + t5 * t41
     # * t43 / 0.64D2 - (0.90D2 * t5 * (t6 - t49 * t17) - t24) * t37 / 0
     #.5760D4 - t5 * t57 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t63) * t5 
     #* t6 / 0.5760D4 - (0.180D3 * t69 - 0.30D2 * t71 + 0.180D3 * t63 * 
     #lh + 0.45D2 * t75) * t5 * t17 / 0.5760D4 + (0.90D2 * t5 * (-t6 + t
     #83 * t17 + t85 - t90 * t29) - 0.180D3 * t22 * t41) * t32 / 0.5760D
     #4
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
      t126 = bbggh21J1(s, XB1, XB2, z, lh, wd, t103, x2, t104, 0.0D0)
      t128 = t126 * t26 * t37
      t131 = FJET(XB1, XB2, s, t106 * t108 * t109, -t106 * t108 * x3, 0.
     #0D0, t106 * t114, s * t116 * t118 * t120 * t109, t5 * t124 * t128 
     #/ 0.32D2)
      t138 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, 0.0D0)
      t142 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, x4)
      t147 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t104, 0.0D0)
      t151 = log(-0.4D1 * t46 * t13 * t109)
      t161 = t5 * t138 * t38 / 0.32D2 + t5 * (-t142 + t138) * t43 / 0.64
     #D2 - (-0.90D2 * t5 * (t147 - t151 * t138) + 0.180D3 * t22 * t138) 
     #* t37 / 0.5760D4
      t162 = FJET(XB1, XB2, s, -t2 * t109, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t161)
      t164 = KAPPA2(t103, x2, 0.10D1, 0.0D0, z)
      t165 = s * t164
      t168 = t164 ** 2
      t174 = 0.1D1 / (-0.2D1 + t164)
      t175 = bbggh21J2(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, 0.0D0)
      t177 = t107 ** 2
      t179 = t168 ** 2
      t183 = log(0.4D1 * t11 * t13 * t177 * t179)
      t185 = bbggh21J1(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, 0.0D0)
      t196 = t5 * t174
      t197 = t185 * t26
      t204 = (-0.90D2 * t5 * (t174 * t175 - t183 * t174 * t185) + 0.180D
     #3 * t22 * t174 * t185) * t26 / 0.2880D4 - t196 * t197 * t32 / 0.32
     #D2 - t196 * t197 * t37 / 0.32D2
      t205 = FJET(XB1, XB2, s, -t165 * t108, 0.0D0, 0.0D0, t165 * t114, 
     #-s * t168 * t118 * t107 * x1, t204)
      t207 = KAPPA2(t103, x2, 0.10D1, x4, z)
      t208 = s * t207
      t214 = t207 ** 2
      t220 = 0.1D1 / (-0.2D1 + t207)
      t222 = bbggh21J1(s, XB1, XB2, z, lh, wd, t103, x2, 0.10D1, x4)
      t224 = t222 * t26 * t32
      t227 = FJET(XB1, XB2, s, -t208 * t108, 0.0D0, t208 * t114 * x4, -t
     #208 * t114 * t86, s * t214 * t118 * t120 * t86, t5 * t220 * t224 /
     # 0.32D2)
      bbggh2n2em1 = t101 * t100 + t131 * t5 * t124 * t128 / 0.32D2 + t16
     #2 * t161 + t205 * t204 + t227 * t5 * t220 * t224 / 0.32D2

      end function



      doubleprecision function bbggh2n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t31 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t37 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 + t5 * (-t6 + 
     #t31) / x4 / 0.64D2
      t38 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t37)
      t40 = 0.1D1 - x1
      t41 = KAPPA2(t40, x2, 0.10D1, 0.0D0, z)
      t42 = s * t41
      t43 = -t40
      t48 = t41 ** 2
      t50 = t1 ** 2
      t55 = 0.1D1 / (-0.2D1 + t41)
      t57 = bbggh21J1(s, XB1, XB2, z, lh, wd, t40, x2, 0.10D1, 0.0D0)
      t61 = FJET(XB1, XB2, s, -t42 * t1 * t43, 0.0D0, 0.0D0, t42 * t1 * 
     #x1, -s * t48 * t50 * t43 * x1, -t5 * t55 * t57 * t8 / 0.32D2)
      t67 = -0.1D1 + x3
      t71 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, -t67, 0.0D0)
      t75 = FJET(XB1, XB2, s, -t2 * t67, t2 * x3, 0.0D0, 0.0D0, 0.0D0, t
     #5 * t71 * t11 / 0.64D2)
      bbggh2n2em2 = t38 * t37 - t61 * t5 * t55 * t57 * t8 / 0.32D2 + t75
     # * t5 * t71 * t11 / 0.64D2

      end function



      doubleprecision function bbggh2n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, 0.0D0)
      t9 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n2em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n2em4 = 0.0D0

      end function


      doubleprecision function bbggh2n3e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t20 = t17 ** 2
      t21 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
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
      t62 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t61)
      t63 = -t61
      t64 = t56 * t63
      t67 = log(-0.4D1 * t12 * t64)
      t68 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t61)
      t74 = t59 ** 2
      t77 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t61)
      t79 = t67 ** 2
      t85 = -t68 + t21
      t89 = 0.1D1 / x4
      t92 = x3 * t8
      t93 = t11 * t14
      t94 = t93 * x4
      t97 = log(0.4D1 * t92 * t94)
      t99 = t92 * t11
      t102 = log(-0.4D1 * t99 * t64)
      t111 = 0.1D1 / x3
      t112 = t111 * t89
      t115 = t92 * t93
      t117 = log(0.4D1 * t115)
      t123 = t117 ** 2
      t135 = log(0.4D1 * t93)
      t138 = t135 ** 2
      t147 = t138 * t135
      t153 = t29 ** 2
      t154 = t27 ** 2
      t160 = t138 ** 2
      t171 = log(0.4D1 * t94)
      t173 = t171 ** 2
      t176 = x4 * t63
      t179 = log(-0.4D1 * t93 * t176)
      t181 = t179 ** 2
      t206 = -t85
      t211 = x3 * t11
      t212 = t211 * t14
      t214 = log(0.4D1 * t212)
      t216 = t214 ** 2
      t239 = log(0.4D1 * t211 * t56)
      t243 = log(-0.4D1 * t211 * t64)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t264 = (-0.180D3 * t6 * (-t7 + t17 * t18 - t20 * t21 / 0.2D1) + t3
     #2 * (-t18 + t17 * t21) + 0.90D2 * t5 * (t17 * t7 - t20 * t18 / 0.2
     #D1 + t20 * t17 * t21 / 0.6D1) - t51) * t53 / 0.2880D4 - (-0.180D3 
     #* t6 * (t18 - t59 * t21 - t62 + t67 * t68) + 0.90D2 * t5 * (t7 - t
     #59 * t18 + t74 * t21 / 0.2D1 - t77 + t67 * t62 - t79 * t68 / 0.2D1
     #) + t32 * t85) * t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (t18 - t97 
     #* t21 - t62 + t102 * t68) - 0.180D3 * t6 * t85) * t53 * t112 / 0.2
     #880D4 + (-0.180D3 * t6 * (-t18 + t117 * t21) + 0.90D2 * t5 * (-t7 
     #+ t117 * t18 - t123 * t21 / 0.2D1) - t32 * t21) * t53 * t111 / 0.2
     #880D4 - (t28 - t30 + 0.180D3 * t135 * lh + 0.45D2 * t138) * t5 * t
     #7 / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t135 * t31 - 0
     #.90D2 * t138 * lh - 0.15D2 * t147) * t5 * t18 / 0.5760D4 - (t153 +
     # 0.60D2 * t154 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29 - 
     #t135 * t49 + 0.15D2 / 0.4D1 * t160 + t138 * t31 / 0.2D1 + 0.30D2 *
     # t147 * lh) * t5 * t21 / 0.5760D4 + (-0.180D3 * t6 * (-t7 + t171 *
     # t18 - t173 * t21 / 0.2D1 + t77 - t179 * t62 + t181 * t68 / 0.2D1)
     # + t32 * (t62 - t179 * t68 - t18 + t171 * t21) + 0.90D2 * t5 * (t1
     #71 * t7 - t173 * t18 / 0.2D1 + t173 * t171 * t21 / 0.6D1 - t179 * 
     #t77 + t181 * t62 / 0.2D1 - t181 * t179 * t68 / 0.6D1) + t50 * t206
     #) * t89 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t214 * t18 + t216 * t2
     #1 / 0.2D1) + t32 * (t18 - t214 * t21) + 0.90D2 * t5 * (-t214 * t7 
     #+ t216 * t18 / 0.2D1 - t216 * t214 * t21 / 0.6D1) + t51) * t111 / 
     #0.5760D4 + (-0.180D3 * t6 * (-t18 + t239 * t21 + t62 - t243 * t68)
     # + 0.90D2 * t5 * (-t7 + t239 * t18 - t249 * t21 / 0.2D1 + t77 - t2
     #43 * t62 + t253 * t68 / 0.2D1) + t32 * t206) * t111 * t89 / 0.5760
     #D4
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
      t283 = bbggh21J3(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, 0.10D1)
      t284 = t282 * t283
      t285 = t270 ** 2
      t286 = t14 * t285
      t287 = t275 ** 2
      t288 = t286 * t287
      t291 = log(0.4D1 * t12 * t288)
      t292 = t291 * t282
      t293 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, 0.10D1)
      t295 = t291 ** 2
      t296 = t295 * t282
      t297 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, 0.10D1)
      t303 = t282 * t293
      t317 = t282 * t297
      t321 = t285 * x4
      t325 = log(0.4D1 * t15 * t321 * t287)
      t326 = t325 * t282
      t332 = t325 ** 2
      t339 = t32 * t317
      t347 = log(0.4D1 * t99 * t286 * x4 * t287)
      t360 = log(0.4D1 * t99 * t288)
      t361 = t360 * t282
      t367 = t360 ** 2
      t378 = (0.180D3 * t6 * (t284 - t292 * t293 + t296 * t297 / 0.2D1) 
     #- t32 * (t303 - t292 * t297) - 0.90D2 * t5 * (-t292 * t283 + t296 
     #* t293 / 0.2D1 - t295 * t291 * t282 * t297 / 0.6D1) - t50 * t317) 
     #* t53 / 0.2880D4 - (-0.180D3 * t6 * (t303 - t326 * t297) + 0.90D2 
     #* t5 * (t284 - t326 * t293 + t332 * t282 * t297 / 0.2D1) + t339) *
     # t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (t303 - t347 * t282 * t297)
     # - 0.180D3 * t6 * t317) * t53 * t112 / 0.2880D4 + (-0.180D3 * t6 *
     # (-t303 + t361 * t297) + 0.90D2 * t5 * (-t284 + t361 * t293 - t367
     # * t282 * t297 / 0.2D1) - t339) * t53 * t111 / 0.2880D4
      t379 = FJET(XB1, XB2, s, 0.0D0, -t269 * t271, t269 * t273, 0.0D0, 
     #-s * t275 * t277 * t270 * x1, t378)
      t381 = KAPPA2(t267, x2, 0.0D0, t61, z)
      t382 = s * t381
      t384 = t273 * t63
      t386 = t273 * x4
      t388 = t381 ** 2
      t391 = t270 * x1
      t395 = 0.1D1 / (-0.2D1 + t381)
      t396 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, t61)
      t397 = t395 * t396
      t398 = t388 ** 2
      t400 = t321 * t63 * t398
      t403 = log(-0.4D1 * t15 * t400)
      t404 = t403 * t395
      t405 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, t61)
      t410 = bbggh21J3(s, XB1, XB2, z, lh, wd, t267, x2, 0.0D0, t61)
      t413 = t403 ** 2
      t420 = t395 * t405
      t427 = log(-0.4D1 * t115 * t400)
      t439 = -(0.180D3 * t6 * (t397 - t404 * t405) - 0.90D2 * t5 * (t395
     # * t410 - t404 * t396 + t413 * t395 * t405 / 0.2D1) - t32 * t420) 
     #* t53 * t89 / 0.2880D4 - (0.90D2 * t5 * (-t397 + t427 * t395 * t40
     #5) + 0.180D3 * t6 * t420) * t53 * t112 / 0.2880D4
      t440 = FJET(XB1, XB2, s, 0.0D0, -t382 * t271, -t382 * t384, t382 *
     # t386, s * t388 * t277 * t391 * t63, t439)
      t443 = -0.1D1 + x3
      t445 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t446 = t14 * t443
      t447 = t446 * x4
      t450 = log(-0.4D1 * t99 * t447)
      t451 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t453 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t61)
      t458 = log(0.4D1 * t99 * t56 * t63 * t443)
      t459 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t61)
      t464 = t459 - t451
      t474 = log(-0.4D1 * t92 * t93 * t443)
      t479 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t481 = t474 ** 2
      t494 = log(-0.4D1 * t211 * t446)
      t496 = t494 ** 2
      t520 = log(-0.4D1 * t211 * t447)
      t525 = log(0.4D1 * t212 * t176 * t443)
      t531 = t520 ** 2
      t534 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t61)
      t536 = t525 ** 2
      t548 = -(0.90D2 * t5 * (-t445 + t450 * t451 + t453 - t458 * t459) 
     #- 0.180D3 * t6 * t464) * t53 * t112 / 0.2880D4 + (-0.180D3 * t6 * 
     #(t445 - t474 * t451) + 0.90D2 * t5 * (t479 - t474 * t445 + t481 * 
     #t451 / 0.2D1) + t32 * t451) * t53 * t111 / 0.2880D4 - (0.180D3 * t
     #6 * (t479 - t494 * t445 + t496 * t451 / 0.2D1) - t32 * (t445 - t49
     #4 * t451) - 0.90D2 * t5 * (-t494 * t479 + t496 * t445 / 0.2D1 - t4
     #96 * t494 * t451 / 0.6D1) - t50 * t451) * t111 / 0.5760D4 + (-0.18
     #0D3 * t6 * (t445 - t520 * t451 - t453 + t525 * t459) + 0.90D2 * t5
     # * (t479 - t520 * t445 + t531 * t451 / 0.2D1 - t534 + t525 * t453 
     #- t536 * t459 / 0.2D1) - t32 * t464) * t111 * t89 / 0.5760D4
      t549 = FJET(XB1, XB2, s, t2 * x3, -t2 * t443, 0.0D0, 0.0D0, 0.0D0,
     # t548)
      t551 = KAPPA2(t267, x2, x3, 0.10D1, z)
      t552 = s * t551
      t553 = t271 * x3
      t555 = t271 * t443
      t558 = t551 ** 2
      t564 = 0.1D1 / (-0.2D1 + t551)
      t565 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, x3, 0.10D1)
      t566 = t564 * t565
      t567 = t285 * t443
      t568 = t558 ** 2
      t573 = log(-0.4D1 * t115 * t567 * x4 * t568)
      t575 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, x3, 0.10D1)
      t580 = t564 * t575
      t590 = log(-0.4D1 * t99 * t286 * t443 * t568)
      t591 = t590 * t564
      t596 = bbggh21J3(s, XB1, XB2, z, lh, wd, t267, x2, x3, 0.10D1)
      t599 = t590 ** 2
      t611 = -(0.90D2 * t5 * (-t566 + t573 * t564 * t575) + 0.180D3 * t6
     # * t580) * t53 * t112 / 0.2880D4 + (-0.180D3 * t6 * (t566 - t591 *
     # t575) + 0.90D2 * t5 * (t564 * t596 - t591 * t565 + t599 * t564 * 
     #t575 / 0.2D1) + t32 * t580) * t53 * t111 / 0.2880D4
      t612 = FJET(XB1, XB2, s, -t552 * t553, t552 * t555, t552 * t273, 0
     #.0D0, s * t558 * t277 * t391 * t443, t611)
      t614 = KAPPA2(t267, x2, x3, t61, z)
      t615 = s * t614
      t620 = t614 ** 2
      t625 = cos(t9)
      t628 = sqrt(x3 * t443 * t176)
      t635 = 0.1D1 / (-0.2D1 + t614)
      t636 = bbggh21J2(s, XB1, XB2, z, lh, wd, t267, x2, x3, t61)
      t638 = t620 ** 2
      t643 = log(0.4D1 * t115 * t567 * t176 * t638)
      t645 = bbggh21J1(s, XB1, XB2, z, lh, wd, t267, x2, x3, t61)
      t653 = 0.90D2 * t5 * (t635 * t636 - t643 * t635 * t645) - 0.180D3 
     #* t6 * t635 * t645
      t657 = FJET(XB1, XB2, s, -t615 * t553, t615 * t555, -t615 * t384, 
     #t615 * t386, s * t620 * t277 * t391 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t625 * t628), -t653 * t53 * t112 / 0.2880D4)
      bbggh2n3e1 = t265 * t264 + t379 * t378 + t440 * t439 + t549 * t548
     # + t612 * t611 - t657 * t653 * t53 * t111 * t89 / 0.2880D4

      end function



      doubleprecision function bbggh2n3e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t20 = 0.1D1 - x4
      t21 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t20)
      t22 = -t20
      t23 = t14 * t22
      t26 = log(-0.4D1 * t11 * t23)
      t27 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t20)
      t32 = lh * t5
      t33 = -t27 + t18
      t37 = 0.1D1 / x1
      t39 = 0.1D1 / x4
      t43 = 0.1D1 / x3
      t45 = t37 * t43 * t39
      t48 = x3 * t7
      t49 = t10 * t13
      t52 = log(0.4D1 * t48 * t49)
      t63 = t11 * t13
      t65 = log(0.4D1 * t63)
      t70 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t18
      t88 = x4 * t22
      t91 = log(-0.4D1 * t49 * t88)
      t95 = log(0.4D1 * t49 * x4)
      t101 = t95 ** 2
      t104 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t20)
      t106 = t91 ** 2
      t112 = -t33
      t117 = x3 * t10
      t120 = log(0.4D1 * t117 * t14)
      t124 = log(-0.4D1 * t117 * t23)
      t135 = t117 * t13
      t137 = log(0.4D1 * t135)
      t143 = t137 ** 2
      t154 = log(0.4D1 * t49)
      t162 = t154 ** 2
      t181 = -(0.90D2 * t5 * (t6 - t17 * t18 - t21 + t26 * t27) - 0.180D
     #3 * t32 * t33) * t37 * t39 / 0.2880D4 - t5 * t33 * t45 / 0.32D2 + 
     #(0.90D2 * t5 * (-t6 + t52 * t18) + 0.180D3 * t32 * t18) * t37 * t4
     #3 / 0.2880D4 + (-0.180D3 * t32 * (-t6 + t65 * t18) + 0.90D2 * t5 *
     # (-t70 + t65 * t6 - t72 * t18 / 0.2D1) - t84) * t37 / 0.2880D4 + (
     #-0.180D3 * t32 * (t21 - t91 * t27 - t6 + t95 * t18) + 0.90D2 * t5 
     #* (-t70 + t95 * t6 - t101 * t18 / 0.2D1 + t104 - t91 * t21 + t106 
     #* t27 / 0.2D1) + t83 * t112) * t39 / 0.5760D4 + (0.90D2 * t5 * (-t
     #6 + t120 * t18 + t21 - t124 * t27) - 0.180D3 * t32 * t112) * t43 *
     # t39 / 0.5760D4 - (-0.180D3 * t32 * (t6 - t137 * t18) + 0.90D2 * t
     #5 * (t70 - t137 * t6 + t143 * t18 / 0.2D1) + t84) * t43 / 0.5760D4
     # - (-0.180D3 * lh - 0.90D2 * t154) * t5 * t70 / 0.5760D4 - (t79 - 
     #t81 + 0.180D3 * t154 * lh + 0.45D2 * t162) * t5 * t6 / 0.5760D4 - 
     #(-0.2884936567583026D3 - 0.120D3 * t78 * lh + 0.60D2 * lh * t80 - 
     #t154 * t82 - 0.90D2 * t162 * lh - 0.15D2 * t162 * t154) * t5 * t18
     # / 0.5760D4
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
      t200 = bbggh21J2(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, 0.10D1)
      t201 = t199 * t200
      t202 = t187 ** 2
      t203 = t202 * x4
      t204 = t192 ** 2
      t208 = log(0.4D1 * t63 * t203 * t204)
      t210 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, 0.10D1)
      t215 = t199 * t210
      t217 = 0.180D3 * t32 * t215
      t226 = t48 * t10
      t227 = t13 * t202
      t228 = t227 * t204
      t231 = log(0.4D1 * t226 * t228)
      t243 = log(0.4D1 * t11 * t228)
      t244 = t243 * t199
      t249 = bbggh21J3(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, 0.10D1)
      t252 = t243 ** 2
      t263 = -(0.90D2 * t5 * (t201 - t208 * t199 * t210) - t217) * t37 *
     # t39 / 0.2880D4 - t5 * t199 * t210 * t45 / 0.32D2 + (0.90D2 * t5 *
     # (-t201 + t231 * t199 * t210) + t217) * t37 * t43 / 0.2880D4 + (0.
     #180D3 * t32 * (t201 - t244 * t210) - 0.90D2 * t5 * (t199 * t249 - 
     #t244 * t200 + t252 * t199 * t210 / 0.2D1) - t83 * t215) * t37 / 0.
     #2880D4
      t264 = FJET(XB1, XB2, s, 0.0D0, -t186 * t188, t186 * t190, 0.0D0, 
     #-s * t192 * t194 * t187 * x1, t263)
      t266 = KAPPA2(t184, x2, 0.0D0, t20, z)
      t267 = s * t266
      t269 = t190 * t22
      t271 = t190 * x4
      t273 = t266 ** 2
      t276 = t187 * x1
      t280 = 0.1D1 / (-0.2D1 + t266)
      t281 = bbggh21J2(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, t20)
      t283 = t273 ** 2
      t288 = log(-0.4D1 * t63 * t203 * t22 * t283)
      t290 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, 0.0D0, t20)
      t306 = -(-0.90D2 * t5 * (t280 * t281 - t288 * t280 * t290) + 0.180
     #D3 * t32 * t280 * t290) * t37 * t39 / 0.2880D4 + t5 * t280 * t290 
     #* t45 / 0.32D2
      t307 = FJET(XB1, XB2, s, 0.0D0, -t267 * t188, -t267 * t269, t267 *
     # t271, s * t273 * t194 * t276 * t22, t306)
      t310 = -0.1D1 + x3
      t312 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t20)
      t313 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t314 = t312 - t313
      t318 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t322 = log(-0.4D1 * t48 * t49 * t310)
      t333 = t13 * t310
      t337 = log(-0.4D1 * t117 * t333 * x4)
      t339 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t20)
      t343 = log(0.4D1 * t135 * t88 * t310)
      t357 = log(-0.4D1 * t117 * t333)
      t362 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t364 = t357 ** 2
      t374 = -t5 * t314 * t45 / 0.32D2 + (0.90D2 * t5 * (t318 - t322 * t
     #313) - 0.180D3 * t32 * t313) * t37 * t43 / 0.2880D4 + (0.90D2 * t5
     # * (t318 - t337 * t313 - t339 + t343 * t312) + 0.180D3 * t32 * t31
     #4) * t43 * t39 / 0.5760D4 - (0.180D3 * t32 * (t318 - t357 * t313) 
     #- 0.90D2 * t5 * (t362 - t357 * t318 + t364 * t313 / 0.2D1) - t83 *
     # t313) * t43 / 0.5760D4
      t375 = FJET(XB1, XB2, s, t2 * x3, -t2 * t310, 0.0D0, 0.0D0, 0.0D0,
     # t374)
      t377 = KAPPA2(t184, x2, x3, 0.10D1, z)
      t378 = s * t377
      t379 = t188 * x3
      t381 = t188 * t310
      t384 = t377 ** 2
      t390 = 0.1D1 / (-0.2D1 + t377)
      t392 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, x3, 0.10D1)
      t396 = bbggh21J2(s, XB1, XB2, z, lh, wd, t184, x2, x3, 0.10D1)
      t398 = t384 ** 2
      t403 = log(-0.4D1 * t226 * t227 * t310 * t398)
      t416 = t5 * t390 * t392 * t45 / 0.32D2 + (0.90D2 * t5 * (t390 * t3
     #96 - t403 * t390 * t392) - 0.180D3 * t32 * t390 * t392) * t37 * t4
     #3 / 0.2880D4
      t417 = FJET(XB1, XB2, s, -t379 * t378, t378 * t381, t378 * t190, 0
     #.0D0, s * t384 * t194 * t276 * t310, t416)
      t419 = KAPPA2(t184, x2, x3, t20, z)
      t420 = s * t419
      t425 = t419 ** 2
      t430 = cos(t8)
      t433 = sqrt(x3 * t310 * t88)
      t440 = 0.1D1 / (-0.2D1 + t419)
      t442 = bbggh21J1(s, XB1, XB2, z, lh, wd, t184, x2, x3, t20)
      t446 = FJET(XB1, XB2, s, -t420 * t379, t420 * t381, -t420 * t269, 
     #t420 * t271, s * t425 * t194 * t276 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t430 * t433), -t5 * t440 * t442 * t45 / 0.32D2)
      bbggh2n3e0 = t182 * t181 + t264 * t263 + t307 * t306 + t375 * t374
     # + t417 * t416 - t446 * t5 * t440 * t442 * t37 * t43 * t39 / 0.32D
     #2

      end function



      doubleprecision function bbggh2n3em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = 0.1D1 - x4
      t30 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t29)
      t31 = -t30 + t17
      t33 = 0.1D1 / x4
      t38 = 0.1D1 / x3
      t39 = t26 * t38
      t42 = -t31
      t44 = t38 * t33
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t13)
      t58 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t62 = t10 * t13
      t64 = log(0.4D1 * t62)
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t76 = t64 ** 2
      t82 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, t29)
      t83 = -t29
      t87 = log(-0.4D1 * t62 * x4 * t83)
      t91 = log(0.4D1 * t62 * x4)
      t101 = (0.90D2 * t5 * (-t6 + t16 * t17) + t24) * t26 / 0.2880D4 - 
     #t5 * t31 * t26 * t33 / 0.32D2 - t5 * t17 * t39 / 0.32D2 + t5 * t42
     # * t44 / 0.64D2 - (0.90D2 * t5 * (t6 - t50 * t17) - t24) * t38 / 0
     #.5760D4 - t5 * t58 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t64) * t5 
     #* t6 / 0.5760D4 - (0.180D3 * t70 - 0.30D2 * t72 + 0.180D3 * t64 * 
     #lh + 0.45D2 * t76) * t5 * t17 / 0.5760D4 + (0.90D2 * t5 * (t82 - t
     #87 * t30 - t6 + t91 * t17) - 0.180D3 * t22 * t42) * t33 / 0.5760D4
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
      t120 = bbggh21J2(s, XB1, XB2, z, lh, wd, t104, x2, 0.0D0, 0.10D1)
      t122 = t107 ** 2
      t124 = t112 ** 2
      t128 = log(0.4D1 * t11 * t13 * t122 * t124)
      t130 = bbggh21J1(s, XB1, XB2, z, lh, wd, t104, x2, 0.0D0, 0.10D1)
      t141 = t5 * t119
      t142 = t130 * t26
      t149 = (-0.90D2 * t5 * (t119 * t120 - t128 * t119 * t130) + 0.180D
     #3 * t22 * t119 * t130) * t26 / 0.2880D4 - t141 * t142 * t33 / 0.32
     #D2 - t141 * t142 * t38 / 0.32D2
      t150 = FJET(XB1, XB2, s, 0.0D0, -t106 * t108, t106 * t110, 0.0D0, 
     #-s * t112 * t114 * t107 * x1, t149)
      t152 = KAPPA2(t104, x2, 0.0D0, t29, z)
      t153 = s * t152
      t159 = t152 ** 2
      t162 = t107 * x1
      t166 = 0.1D1 / (-0.2D1 + t152)
      t168 = bbggh21J1(s, XB1, XB2, z, lh, wd, t104, x2, 0.0D0, t29)
      t170 = t168 * t26 * t33
      t173 = FJET(XB1, XB2, s, 0.0D0, -t153 * t108, -t153 * t110 * t83, 
     #t153 * t110 * x4, s * t159 * t114 * t162 * t83, t5 * t166 * t170 /
     # 0.32D2)
      t179 = -0.1D1 + x3
      t181 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t185 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, t29)
      t190 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t194 = log(-0.4D1 * t47 * t13 * t179)
      t204 = t5 * t181 * t39 / 0.32D2 + t5 * (t181 - t185) * t44 / 0.64D
     #2 - (-0.90D2 * t5 * (t190 - t194 * t181) + 0.180D3 * t22 * t181) *
     # t38 / 0.5760D4
      t205 = FJET(XB1, XB2, s, t2 * x3, -t2 * t179, 0.0D0, 0.0D0, 0.0D0,
     # t204)
      t207 = KAPPA2(t104, x2, x3, 0.10D1, z)
      t208 = s * t207
      t214 = t207 ** 2
      t220 = 0.1D1 / (-0.2D1 + t207)
      t222 = bbggh21J1(s, XB1, XB2, z, lh, wd, t104, x2, x3, 0.10D1)
      t224 = t222 * t26 * t38
      t227 = FJET(XB1, XB2, s, -t208 * t108 * x3, t208 * t108 * t179, t2
     #08 * t110, 0.0D0, s * t214 * t114 * t162 * t179, t5 * t220 * t224 
     #/ 0.32D2)
      bbggh2n3em1 = t102 * t101 + t150 * t149 + t173 * t5 * t166 * t170 
     #/ 0.32D2 + t205 * t204 + t227 * t5 * t220 * t224 / 0.32D2

      end function



      doubleprecision function bbggh2n3em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t32 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.1D1 -
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
      t58 = bbggh21J1(s, XB1, XB2, z, lh, wd, t41, x2, 0.0D0, 0.10D1)
      t62 = FJET(XB1, XB2, s, 0.0D0, -t43 * t1 * t44, t43 * t1 * x1, 0.0
     #D0, -s * t49 * t51 * t44 * x1, -t5 * t56 * t58 * t8 / 0.32D2)
      t71 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.10D1)
      t75 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, t5 * t71 * t11 / 0.64D2)
      bbggh2n3em2 = t39 * t38 - t62 * t5 * t56 * t58 * t8 / 0.32D2 + t75
     # * t5 * t71 * t11 / 0.64D2

      end function



      doubleprecision function bbggh2n3em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.10D1)
      t9 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n3em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n3em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n3em4 = 0.0D0

      end function


      doubleprecision function bbggh2n4e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = lh * t5
      t7 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t17 = log(0.4D1 * t15)
      t18 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
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
      t56 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t57 = t14 * x4
      t58 = -0.1D1 + x4
      t59 = t57 * t58
      t62 = log(-0.4D1 * t12 * t59)
      t63 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t67 = log(0.4D1 * t12 * t57)
      t72 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t74 = t62 ** 2
      t78 = t67 ** 2
      t84 = t63 - t21
      t88 = 0.1D1 / x4
      t91 = x3 * t8
      t92 = t11 * t14
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t91 * t11
      t101 = log(-0.4D1 * t98 * t59)
      t106 = -t84
      t111 = 0.1D1 / x3
      t112 = t111 * t88
      t115 = t91 * t92
      t117 = log(0.4D1 * t115)
      t123 = t117 ** 2
      t135 = log(0.4D1 * t92)
      t138 = t135 ** 2
      t147 = t138 * t135
      t153 = t29 ** 2
      t154 = t27 ** 2
      t160 = t138 ** 2
      t170 = x4 * t58
      t173 = log(-0.4D1 * t92 * t170)
      t175 = t173 ** 2
      t179 = log(0.4D1 * t93)
      t181 = t179 ** 2
      t210 = x3 * t11
      t211 = t210 * t14
      t213 = log(0.4D1 * t211)
      t215 = t213 ** 2
      t238 = log(-0.4D1 * t210 * t59)
      t242 = log(0.4D1 * t210 * t57)
      t248 = t242 ** 2
      t252 = t238 ** 2
      t263 = -(-0.180D3 * t6 * (t7 - t17 * t18 + t20 * t21 / 0.2D1) + t3
     #2 * (t18 - t17 * t21) + 0.90D2 * t5 * (-t17 * t7 + t20 * t18 / 0.2
     #D1 - t20 * t17 * t21 / 0.6D1) + t51) * t53 / 0.2880D4 + (-0.180D3 
     #* t6 * (t56 - t62 * t63 - t18 + t67 * t21) + 0.90D2 * t5 * (t72 - 
     #t62 * t56 + t74 * t63 / 0.2D1 - t7 + t67 * t18 - t78 * t21 / 0.2D1
     #) + t32 * t84) * t53 * t88 / 0.2880D4 - (0.90D2 * t5 * (t18 - t96 
     #* t21 - t56 + t101 * t63) - 0.180D3 * t6 * t106) * t53 * t112 / 0.
     #2880D4 - (-0.180D3 * t6 * (t18 - t117 * t21) + 0.90D2 * t5 * (t7 -
     # t117 * t18 + t123 * t21 / 0.2D1) + t32 * t21) * t53 * t111 / 0.28
     #80D4 - (t28 - t30 + 0.180D3 * t135 * lh + 0.45D2 * t138) * t5 * t7
     # / 0.5760D4 - (-0.2884936567583026D3 - t46 + t48 - t135 * t31 - 0.
     #90D2 * t138 * lh - 0.15D2 * t147) * t5 * t18 / 0.5760D4 - (t153 + 
     #0.60D2 * t154 + 0.5769873135166051D3 * lh - 0.60D2 * t27 * t29 - t
     #135 * t49 + 0.15D2 / 0.4D1 * t160 + t138 * t31 / 0.2D1 + 0.30D2 * 
     #t147 * lh) * t5 * t21 / 0.5760D4 - (-0.180D3 * t6 * (-t72 + t173 *
     # t56 - t175 * t63 / 0.2D1 + t7 - t179 * t18 + t181 * t21 / 0.2D1) 
     #+ t32 * (t18 - t179 * t21 - t56 + t173 * t63) + 0.90D2 * t5 * (t17
     #3 * t72 - t175 * t56 / 0.2D1 + t175 * t173 * t63 / 0.6D1 - t179 * 
     #t7 + t181 * t18 / 0.2D1 - t181 * t179 * t21 / 0.6D1) + t50 * t106)
     # * t88 / 0.5760D4 - (-0.180D3 * t6 * (t7 - t213 * t18 + t215 * t21
     # / 0.2D1) + t32 * (t18 - t213 * t21) + 0.90D2 * t5 * (-t213 * t7 +
     # t215 * t18 / 0.2D1 - t215 * t213 * t21 / 0.6D1) + t51) * t111 / 0
     #.5760D4 - (-0.180D3 * t6 * (-t56 + t238 * t63 + t18 - t242 * t21) 
     #+ 0.90D2 * t5 * (t7 - t242 * t18 + t248 * t21 / 0.2D1 - t72 + t238
     # * t56 - t252 * t63 / 0.2D1) + t32 * t106) * t111 * t88 / 0.5760D4
      t264 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t263)
      t266 = -0.1D1 + x1
      t269 = -t266
      t270 = bbggh21J3(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, 0.0D0)
      t271 = t266 ** 2
      t272 = t8 * t271
      t275 = log(0.4D1 * t92 * t272)
      t276 = bbggh21J2(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, 0.0D0)
      t278 = t275 ** 2
      t279 = bbggh21J1(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, 0.0D0)
      t300 = t272 * x4
      t303 = log(0.4D1 * t92 * t300)
      t309 = t303 ** 2
      t315 = t32 * t279
      t321 = log(0.4D1 * t211 * t300)
      t335 = log(0.4D1 * t210 * t14 * t8 * t271)
      t341 = t335 ** 2
      t351 = -(-0.180D3 * t6 * (-t270 + t275 * t276 - t278 * t279 / 0.2D
     #1) + t32 * (-t276 + t275 * t279) + 0.90D2 * t5 * (t275 * t270 - t2
     #78 * t276 / 0.2D1 + t278 * t275 * t279 / 0.6D1) - t50 * t279) * t5
     #3 / 0.2880D4 + (-0.180D3 * t6 * (t276 - t303 * t279) + 0.90D2 * t5
     # * (t270 - t303 * t276 + t309 * t279 / 0.2D1) + t315) * t53 * t88 
     #/ 0.2880D4 - (0.90D2 * t5 * (-t276 + t321 * t279) + 0.180D3 * t6 *
     # t279) * t53 * t112 / 0.2880D4 - (-0.180D3 * t6 * (-t276 + t335 * 
     #t279) + 0.90D2 * t5 * (-t270 + t335 * t276 - t341 * t279 / 0.2D1) 
     #- t315) * t53 * t111 / 0.2880D4
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
      t372 = bbggh21J2(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, x4)
      t373 = t371 * t372
      t375 = t363 ** 2
      t377 = t271 * x4 * t58 * t375
      t380 = log(-0.4D1 * t15 * t377)
      t381 = t380 * t371
      t382 = bbggh21J1(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, x4)
      t387 = bbggh21J3(s, XB1, XB2, z, lh, wd, t269, x2, 0.0D0, x4)
      t390 = t380 ** 2
      t397 = t371 * t382
      t404 = log(-0.4D1 * t115 * t377)
      t416 = (-0.180D3 * t6 * (t373 - t381 * t382) + 0.90D2 * t5 * (t371
     # * t387 - t381 * t372 + t390 * t371 * t382 / 0.2D1) + t32 * t397) 
     #* t53 * t88 / 0.2880D4 - (0.90D2 * t5 * (-t373 + t404 * t371 * t38
     #2) + 0.180D3 * t6 * t397) * t53 * t112 / 0.2880D4
      t417 = FJET(XB1, XB2, s, 0.0D0, -t355 * t356, t355 * t359, -t355 *
     # t361, -s * t363 * t365 * t367 * x4, t416)
      t420 = -0.1D1 + x3
      t422 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t423 = t14 * t420
      t424 = t423 * x4
      t427 = log(-0.4D1 * t98 * t424)
      t428 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t430 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t435 = log(0.4D1 * t98 * t57 * t58 * t420)
      t436 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t441 = -t428 + t436
      t451 = log(-0.4D1 * t91 * t92 * t420)
      t456 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t458 = t451 ** 2
      t471 = log(-0.4D1 * t210 * t423)
      t473 = t471 ** 2
      t497 = log(-0.4D1 * t210 * t424)
      t502 = log(0.4D1 * t211 * t170 * t420)
      t507 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t509 = t502 ** 2
      t513 = t497 ** 2
      t524 = -(0.90D2 * t5 * (-t422 + t427 * t428 + t430 - t435 * t436) 
     #- 0.180D3 * t6 * t441) * t53 * t112 / 0.2880D4 - (-0.180D3 * t6 * 
     #(-t422 + t451 * t428) + 0.90D2 * t5 * (-t456 + t451 * t422 - t458 
     #* t428 / 0.2D1) - t32 * t428) * t53 * t111 / 0.2880D4 - (0.180D3 *
     # t6 * (t456 - t471 * t422 + t473 * t428 / 0.2D1) - t32 * (t422 - t
     #471 * t428) - 0.90D2 * t5 * (-t471 * t456 + t473 * t422 / 0.2D1 - 
     #t473 * t471 * t428 / 0.6D1) - t50 * t428) * t111 / 0.5760D4 - (-0.
     #180D3 * t6 * (-t422 + t497 * t428 + t430 - t502 * t436) + 0.90D2 *
     # t5 * (t507 - t502 * t430 + t509 * t436 / 0.2D1 - t456 + t497 * t4
     #22 - t513 * t428 / 0.2D1) + t32 * t441) * t111 * t88 / 0.5760D4
      t525 = FJET(XB1, XB2, s, t2 * x3, -t2 * t420, 0.0D0, 0.0D0, 0.0D0,
     # t524)
      t527 = KAPPA2(t269, x2, x3, 0.0D0, z)
      t528 = s * t527
      t529 = t356 * x3
      t531 = t356 * t420
      t534 = t527 ** 2
      t540 = 0.1D1 / (-0.2D1 + t527)
      t541 = bbggh21J2(s, XB1, XB2, z, lh, wd, t269, x2, x3, 0.0D0)
      t542 = t540 * t541
      t543 = t420 * t271
      t544 = t534 ** 2
      t549 = log(-0.4D1 * t115 * t543 * x4 * t544)
      t551 = bbggh21J1(s, XB1, XB2, z, lh, wd, t269, x2, x3, 0.0D0)
      t556 = t540 * t551
      t567 = log(-0.4D1 * t98 * t14 * t271 * t420 * t544)
      t568 = t567 * t540
      t573 = bbggh21J3(s, XB1, XB2, z, lh, wd, t269, x2, x3, 0.0D0)
      t576 = t567 ** 2
      t588 = -(0.90D2 * t5 * (-t542 + t549 * t540 * t551) + 0.180D3 * t6
     # * t556) * t53 * t112 / 0.2880D4 - (0.180D3 * t6 * (t542 - t568 * 
     #t551) - 0.90D2 * t5 * (t540 * t573 - t568 * t541 + t576 * t540 * t
     #551 / 0.2D1) - t32 * t556) * t53 * t111 / 0.2880D4
      t589 = FJET(XB1, XB2, s, -t528 * t529, t528 * t531, 0.0D0, t528 * 
     #t358, -s * t534 * t365 * t367 * x3, t588)
      t591 = KAPPA2(t269, x2, x3, x4, z)
      t592 = s * t591
      t597 = t591 ** 2
      t602 = cos(t9)
      t605 = sqrt(x3 * t420 * t170)
      t612 = 0.1D1 / (-0.2D1 + t591)
      t613 = bbggh21J2(s, XB1, XB2, z, lh, wd, t269, x2, x3, x4)
      t615 = t597 ** 2
      t620 = log(0.4D1 * t115 * t543 * t170 * t615)
      t622 = bbggh21J1(s, XB1, XB2, z, lh, wd, t269, x2, x3, x4)
      t630 = 0.90D2 * t5 * (t612 * t613 - t620 * t612 * t622) - 0.180D3 
     #* t6 * t612 * t622
      t634 = FJET(XB1, XB2, s, -t592 * t529, t592 * t531, t592 * t359, -
     #t592 * t361, s * t597 * t365 * t367 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t602 * t605), -t630 * t53 * t112 / 0.2880D4)
      bbggh2n4e1 = t264 * t263 + t352 * t351 + t417 * t416 + t525 * t524
     # + t589 * t588 - t634 * t630 * t53 * t111 * t88 / 0.2880D4

      end function



      doubleprecision function bbggh2n4e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
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
      t20 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t22 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t25 = log(0.4D1 * t11 * t14)
      t26 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t31 = lh * t5
      t32 = t20 - t26
      t36 = 0.1D1 / x1
      t38 = 0.1D1 / x4
      t41 = -t32
      t43 = 0.1D1 / x3
      t45 = t36 * t43 * t38
      t48 = x3 * t7
      t49 = t10 * t13
      t52 = log(0.4D1 * t48 * t49)
      t63 = t11 * t13
      t65 = log(0.4D1 * t63)
      t70 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t26
      t90 = log(0.4D1 * t49 * x4)
      t92 = x4 * t15
      t95 = log(-0.4D1 * t49 * t92)
      t100 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t102 = t95 ** 2
      t106 = t90 ** 2
      t116 = x3 * t10
      t119 = log(-0.4D1 * t116 * t16)
      t123 = log(0.4D1 * t116 * t14)
      t134 = t116 * t13
      t136 = log(0.4D1 * t134)
      t142 = t136 ** 2
      t153 = log(0.4D1 * t49)
      t161 = t153 ** 2
      t180 = (0.90D2 * t5 * (t6 - t19 * t20 - t22 + t25 * t26) - 0.180D3
     # * t31 * t32) * t36 * t38 / 0.2880D4 - t5 * t41 * t45 / 0.32D2 - (
     #0.90D2 * t5 * (t22 - t52 * t26) - 0.180D3 * t31 * t26) * t36 * t43
     # / 0.2880D4 - (-0.180D3 * t31 * (t22 - t65 * t26) + 0.90D2 * t5 * 
     #(t70 - t65 * t22 + t72 * t26 / 0.2D1) + t84) * t36 / 0.2880D4 - (-
     #0.180D3 * t31 * (t22 - t90 * t26 - t6 + t95 * t20) + 0.90D2 * t5 *
     # (-t100 + t95 * t6 - t102 * t20 / 0.2D1 + t70 - t90 * t22 + t106 *
     # t26 / 0.2D1) + t83 * t41) * t38 / 0.5760D4 - (0.90D2 * t5 * (-t6 
     #+ t119 * t20 + t22 - t123 * t26) - 0.180D3 * t31 * t41) * t43 * t3
     #8 / 0.5760D4 - (-0.180D3 * t31 * (t22 - t136 * t26) + 0.90D2 * t5 
     #* (t70 - t136 * t22 + t142 * t26 / 0.2D1) + t84) * t43 / 0.5760D4 
     #- (-0.180D3 * lh - 0.90D2 * t153) * t5 * t70 / 0.5760D4 - (t79 - t
     #81 + 0.180D3 * t153 * lh + 0.45D2 * t161) * t5 * t22 / 0.5760D4 - 
     #(-0.2884936567583026D3 - 0.120D3 * t78 * lh + 0.60D2 * lh * t80 - 
     #t153 * t82 - 0.90D2 * t161 * lh - 0.15D2 * t161 * t153) * t5 * t26
     # / 0.5760D4
      t181 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t180)
      t183 = -0.1D1 + x1
      t186 = -t183
      t187 = bbggh21J2(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, 0.0D0)
      t188 = t183 ** 2
      t189 = t7 * t188
      t193 = log(0.4D1 * t49 * t189 * x4)
      t194 = bbggh21J1(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, 0.0D0)
      t200 = 0.180D3 * t31 * t194
      t212 = log(0.4D1 * t116 * t13 * t7 * t188)
      t223 = log(0.4D1 * t49 * t189)
      t228 = bbggh21J3(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, 0.0D0)
      t230 = t223 ** 2
      t240 = (0.90D2 * t5 * (t187 - t193 * t194) - t200) * t36 * t38 / 0
     #.2880D4 + t5 * t194 * t45 / 0.32D2 - (0.90D2 * t5 * (-t187 + t212 
     #* t194) + t200) * t36 * t43 / 0.2880D4 - (-0.180D3 * t31 * (-t187 
     #+ t223 * t194) + 0.90D2 * t5 * (-t228 + t223 * t187 - t230 * t194 
     #/ 0.2D1) - t83 * t194) * t36 / 0.2880D4
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
      t261 = bbggh21J2(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, x4)
      t264 = t252 ** 2
      t269 = log(-0.4D1 * t63 * t188 * x4 * t15 * t264)
      t271 = bbggh21J1(s, XB1, XB2, z, lh, wd, t186, x2, 0.0D0, x4)
      t287 = (0.90D2 * t5 * (t260 * t261 - t269 * t260 * t271) - 0.180D3
     # * t31 * t260 * t271) * t36 * t38 / 0.2880D4 + t5 * t260 * t271 * 
     #t45 / 0.32D2
      t288 = FJET(XB1, XB2, s, 0.0D0, -t244 * t245, t244 * t248, -t244 *
     # t250, -s * t252 * t254 * t256 * x4, t287)
      t291 = -0.1D1 + x3
      t293 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t294 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t295 = -t293 + t294
      t299 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t303 = log(-0.4D1 * t48 * t49 * t291)
      t314 = t13 * t291
      t318 = log(-0.4D1 * t116 * t314 * x4)
      t320 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t324 = log(0.4D1 * t134 * t92 * t291)
      t337 = log(-0.4D1 * t116 * t314)
      t342 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t344 = t337 ** 2
      t354 = -t5 * t295 * t45 / 0.32D2 - (0.90D2 * t5 * (-t299 + t303 * 
     #t293) + 0.180D3 * t31 * t293) * t36 * t43 / 0.2880D4 - (0.90D2 * t
     #5 * (-t299 + t318 * t293 + t320 - t324 * t294) - 0.180D3 * t31 * t
     #295) * t43 * t38 / 0.5760D4 - (0.180D3 * t31 * (t299 - t337 * t293
     #) - 0.90D2 * t5 * (t342 - t337 * t299 + t344 * t293 / 0.2D1) - t83
     # * t293) * t43 / 0.5760D4
      t355 = FJET(XB1, XB2, s, t2 * x3, -t2 * t291, 0.0D0, 0.0D0, 0.0D0,
     # t354)
      t357 = KAPPA2(t186, x2, x3, 0.0D0, z)
      t358 = s * t357
      t359 = t245 * x3
      t361 = t245 * t291
      t364 = t357 ** 2
      t370 = 0.1D1 / (-0.2D1 + t357)
      t372 = bbggh21J1(s, XB1, XB2, z, lh, wd, t186, x2, x3, 0.0D0)
      t376 = bbggh21J2(s, XB1, XB2, z, lh, wd, t186, x2, x3, 0.0D0)
      t380 = t364 ** 2
      t385 = log(-0.4D1 * t48 * t10 * t13 * t188 * t291 * t380)
      t398 = t5 * t370 * t372 * t45 / 0.32D2 - (-0.90D2 * t5 * (t370 * t
     #376 - t385 * t370 * t372) + 0.180D3 * t31 * t370 * t372) * t36 * t
     #43 / 0.2880D4
      t399 = FJET(XB1, XB2, s, -t358 * t359, t358 * t361, 0.0D0, t358 * 
     #t247, -s * t364 * t254 * t256 * x3, t398)
      t401 = KAPPA2(t186, x2, x3, x4, z)
      t402 = s * t401
      t407 = t401 ** 2
      t412 = cos(t8)
      t415 = sqrt(x3 * t291 * t92)
      t422 = 0.1D1 / (-0.2D1 + t401)
      t424 = bbggh21J1(s, XB1, XB2, z, lh, wd, t186, x2, x3, x4)
      t428 = FJET(XB1, XB2, s, -t402 * t359, t402 * t361, t402 * t248, -
     #t402 * t250, s * t407 * t254 * t256 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t412 * t415), -t5 * t422 * t424 * t45 / 0.32D2)
      bbggh2n4e0 = t181 * t180 + t241 * t240 + t288 * t287 + t355 * t354
     # + t399 * t398 - t428 * t5 * t422 * t424 * t36 * t43 * t38 / 0.32D
     #2

      end function



      doubleprecision function bbggh2n4em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t7 = x1 ** 2
      t9 = sin(x2 * 0.3141592653589793D1)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t7 * t10 * t13)
      t17 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t22 = lh * t5
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t29 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t30 = t29 - t17
      t32 = 0.1D1 / x4
      t33 = t26 * t32
      t37 = 0.1D1 / x3
      t38 = t26 * t37
      t41 = -t30
      t43 = t37 * t32
      t46 = x3 * t10
      t49 = log(0.4D1 * t46 * t13)
      t57 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t61 = t10 * t13
      t63 = log(0.4D1 * t61)
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t75 = t63 ** 2
      t83 = log(0.4D1 * t61 * x4)
      t85 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t86 = -0.1D1 + x4
      t90 = log(-0.4D1 * t61 * x4 * t86)
      t100 = -(0.90D2 * t5 * (t6 - t16 * t17) - t24) * t26 / 0.2880D4 + 
     #t5 * t30 * t33 / 0.32D2 - t5 * t17 * t38 / 0.32D2 - t5 * t41 * t43
     # / 0.64D2 - (0.90D2 * t5 * (t6 - t49 * t17) - t24) * t37 / 0.5760D
     #4 - t5 * t57 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t63) * t5 * t6 /
     # 0.5760D4 - (0.180D3 * t69 - 0.30D2 * t71 + 0.180D3 * t63 * lh + 0
     #.45D2 * t75) * t5 * t17 / 0.5760D4 - (0.90D2 * t5 * (t6 - t83 * t1
     #7 - t85 + t90 * t29) - 0.180D3 * t22 * t41) * t32 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t100)
      t103 = -0.1D1 + x1
      t106 = -t103
      t107 = bbggh21J2(s, XB1, XB2, z, lh, wd, t106, x2, 0.0D0, 0.0D0)
      t108 = t103 ** 2
      t112 = log(0.4D1 * t61 * t7 * t108)
      t113 = bbggh21J1(s, XB1, XB2, z, lh, wd, t106, x2, 0.0D0, 0.0D0)
      t123 = t5 * t113
      t128 = -(0.90D2 * t5 * (-t107 + t112 * t113) + 0.180D3 * t22 * t11
     #3) * t26 / 0.2880D4 + t123 * t33 / 0.32D2 + t123 * t38 / 0.32D2
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
      t150 = bbggh21J1(s, XB1, XB2, z, lh, wd, t106, x2, 0.0D0, x4)
      t152 = t150 * t26 * t32
      t155 = FJET(XB1, XB2, s, 0.0D0, -t132 * t133, t132 * t135 * x4, -t
     #132 * t135 * t86, -s * t140 * t142 * t144 * x4, t5 * t148 * t152 /
     # 0.32D2)
      t161 = -0.1D1 + x3
      t163 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t167 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t172 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t176 = log(-0.4D1 * t46 * t13 * t161)
      t186 = t5 * t163 * t38 / 0.32D2 - t5 * (-t163 + t167) * t43 / 0.64
     #D2 - (-0.90D2 * t5 * (t172 - t176 * t163) + 0.180D3 * t22 * t163) 
     #* t37 / 0.5760D4
      t187 = FJET(XB1, XB2, s, t2 * x3, -t2 * t161, 0.0D0, 0.0D0, 0.0D0,
     # t186)
      t189 = KAPPA2(t106, x2, x3, 0.0D0, z)
      t190 = s * t189
      t196 = t189 ** 2
      t202 = 0.1D1 / (-0.2D1 + t189)
      t204 = bbggh21J1(s, XB1, XB2, z, lh, wd, t106, x2, x3, 0.0D0)
      t206 = t204 * t26 * t37
      t209 = FJET(XB1, XB2, s, -t190 * t133 * x3, t190 * t133 * t161, 0.
     #0D0, t190 * t135, -s * t196 * t142 * t144 * x3, t5 * t202 * t206 /
     # 0.32D2)
      bbggh2n4em1 = t101 * t100 + t129 * t128 + t155 * t5 * t148 * t152 
     #/ 0.32D2 + t187 * t186 + t209 * t5 * t202 * t206 / 0.32D2

      end function



      doubleprecision function bbggh2n4em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t14 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t19 = sin(x2 * 0.3141592653589793D1)
      t20 = t19 ** 2
      t21 = z ** 2
      t25 = log(0.4D1 * t20 / t21)
      t31 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t37 = -t7 * t8 / 0.32D2 - t7 * t11 / 0.64D2 - t5 * t14 / 0.64D2 - 
     #(-0.180D3 * lh - 0.90D2 * t25) * t5 * t6 / 0.5760D4 - t5 * (t6 - t
     #31) / x4 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t37)
      t40 = -0.1D1 + x1
      t44 = bbggh21J1(s, XB1, XB2, z, lh, wd, -t40, x2, 0.0D0, 0.0D0)
      t48 = FJET(XB1, XB2, s, 0.0D0, -t2 * t40, 0.0D0, t2 * x1, 0.0D0, t
     #5 * t44 * t8 / 0.32D2)
      t56 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, 0.0D0)
      t60 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, t5 * t56 * t11 / 0.64D2)
      bbggh2n4em2 = t38 * t37 + t48 * t5 * t44 * t8 / 0.32D2 + t60 * t5 
     #* t56 * t11 / 0.64D2

      end function



      doubleprecision function bbggh2n4em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, 0.0D0)
      t9 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n4em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n4em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n4em4 = 0.0D0

      end function


      doubleprecision function bbggh2n5e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
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
      t24 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
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
      t145 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, 0.10D1)
      t146 = t139 * t9
      t147 = -t144
      t148 = t11 * t147
      t149 = t148 * x4
      t152 = log(-0.4D1 * t146 * t149)
      t153 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, 0.10D1)
      t158 = -t153 + t58
      t163 = 0.1D1 / x3
      t164 = t163 * t88
      t167 = t139 * t12
      t169 = log(0.4D1 * t167)
      t174 = log(-0.4D1 * t139 * t12 * t147)
      t180 = t169 ** 2
      t183 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, 0.10D1)
      t185 = t174 ** 2
      t191 = t72 * t158
      t196 = x3 * t9
      t199 = log(-0.4D1 * t196 * t148)
      t201 = t199 ** 2
      t204 = t196 * t11
      t206 = log(0.4D1 * t204)
      t208 = t206 ** 2
      t240 = log(0.4D1 * t196 * t119)
      t244 = log(-0.4D1 * t196 * t149)
      t250 = t244 ** 2
      t254 = t240 ** 2
      t264 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 + (-0.180D3 * t61 * (-t24 + t64 * t39 - t66
     # * t58 / 0.2D1) + t72 * (-t39 + t64 * t58) + 0.90D2 * t22 * (t64 *
     # t24 - t66 * t39 / 0.2D1 + t66 * t64 * t58 / 0.6D1) - t86) * t88 /
     # 0.5760D4 - (-0.180D3 * t61 * (t24 - t95 * t39 + t97 * t58 / 0.2D1
     #) + t72 * (t39 - t95 * t58) + 0.90D2 * t22 * (-t95 * t24 + t97 * t
     #39 / 0.2D1 - t97 * t95 * t58 / 0.6D1) + t86) * t116 / 0.2880D4 - (
     #-0.180D3 * t61 * (t39 - t122 * t58) + 0.90D2 * t22 * (t24 - t122 *
     # t39 + t128 * t58 / 0.2D1) + t72 * t58) * t116 * t88 / 0.2880D4 - 
     #(0.90D2 * t22 * (t39 - t142 * t58 - t145 + t152 * t153) - 0.180D3 
     #* t61 * t158) * t116 * t164 / 0.2880D4 - (-0.180D3 * t61 * (t39 - 
     #t169 * t58 - t145 + t174 * t153) + 0.90D2 * t22 * (t24 - t169 * t3
     #9 + t180 * t58 / 0.2D1 - t183 + t174 * t145 - t185 * t153 / 0.2D1)
     # + t191) * t116 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t183 - t199
     # * t145 + t201 * t153 / 0.2D1 - t24 + t206 * t39 - t208 * t58 / 0.
     #2D1) + t72 * (-t39 + t206 * t58 + t145 - t199 * t153) + 0.90D2 * t
     #22 * (-t199 * t183 + t201 * t145 / 0.2D1 - t201 * t199 * t153 / 0.
     #6D1 + t206 * t24 - t208 * t39 / 0.2D1 + t208 * t206 * t58 / 0.6D1)
     # - t85 * t158) * t163 / 0.5760D4 - (-0.180D3 * t61 * (t39 - t240 *
     # t58 - t145 + t244 * t153) + 0.90D2 * t22 * (-t183 + t244 * t145 -
     # t250 * t153 / 0.2D1 + t24 - t240 * t39 + t254 * t58 / 0.2D1) + t1
     #91) * t163 * t88 / 0.5760D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t264)
      t267 = -0.1D1 + x4
      t270 = -t267
      t271 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t270)
      t272 = t119 * t267
      t275 = log(-0.4D1 * t92 * t272)
      t276 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t270)
      t281 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t270)
      t283 = t275 ** 2
      t294 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, t270)
      t299 = log(0.4D1 * t146 * t119 * t267 * t147)
      t300 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, t270)
      t304 = log(-0.4D1 * t146 * t272)
      t309 = -t276 + t300
      t318 = log(-0.4D1 * t196 * t272)
      t320 = x4 * t267
      t324 = log(0.4D1 * t204 * t320 * t147)
      t329 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, t270)
      t331 = t324 ** 2
      t335 = t318 ** 2
      t348 = log(-0.4D1 * t12 * t320)
      t350 = t348 ** 2
      t372 = -(-0.180D3 * t61 * (-t271 + t275 * t276) + 0.90D2 * t22 * (
     #-t281 + t275 * t271 - t283 * t276 / 0.2D1) - t72 * t276) * t116 * 
     #t88 / 0.2880D4 - (0.90D2 * t22 * (t294 - t299 * t300 - t271 + t304
     # * t276) - 0.180D3 * t61 * t309) * t116 * t164 / 0.2880D4 - (-0.18
     #0D3 * t61 * (-t271 + t318 * t276 + t294 - t324 * t300) + 0.90D2 * 
     #t22 * (t329 - t324 * t294 + t331 * t300 / 0.2D1 - t281 + t318 * t2
     #71 - t335 * t276 / 0.2D1) + t72 * t309) * t163 * t88 / 0.5760D4 + 
     #(-0.180D3 * t61 * (t281 - t348 * t271 + t350 * t276 / 0.2D1) + t72
     # * (t271 - t348 * t276) + 0.90D2 * t22 * (-t348 * t281 + t350 * t2
     #71 / 0.2D1 - t350 * t348 * t276 / 0.6D1) + t85 * t276) * t88 / 0.5
     #760D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t267, t2 * x4, 0.0D0,
     # t372)
      t376 = -0.1D1 + x1
      t378 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t379 = t376 ** 2
      t380 = t91 * t379
      t383 = log(0.4D1 * t12 * t380)
      t384 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t386 = t383 ** 2
      t387 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t408 = t380 * x4
      t411 = log(0.4D1 * t12 * t408)
      t417 = t411 ** 2
      t423 = t72 * t387
      t429 = log(0.4D1 * t204 * t408)
      t443 = log(0.4D1 * t196 * t11 * t91 * t379)
      t449 = t443 ** 2
      t459 = -(0.180D3 * t61 * (t378 - t383 * t384 + t386 * t387 / 0.2D1
     #) - t72 * (t384 - t383 * t387) - 0.90D2 * t22 * (-t383 * t378 + t3
     #86 * t384 / 0.2D1 - t386 * t383 * t387 / 0.6D1) - t85 * t387) * t1
     #16 / 0.2880D4 - (-0.180D3 * t61 * (-t384 + t411 * t387) + 0.90D2 *
     # t22 * (-t378 + t411 * t384 - t417 * t387 / 0.2D1) - t423) * t116 
     #* t88 / 0.2880D4 - (0.90D2 * t22 * (-t384 + t429 * t387) + 0.180D3
     # * t61 * t387) * t116 * t164 / 0.2880D4 - (-0.180D3 * t61 * (-t384
     # + t443 * t387) + 0.90D2 * t22 * (-t378 + t443 * t384 - t449 * t38
     #7 / 0.2D1) - t423) * t116 * t163 / 0.2880D4
      t460 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t376, 0.0D0, 0.0D0,
     # t459)
      t462 = KAPPA2(x1, x2, 0.10D1, t270, z)
      t463 = s * t462
      t464 = t1 * x1
      t466 = t1 * t376
      t467 = t466 * t267
      t469 = t466 * x4
      t471 = t462 ** 2
      t473 = t1 ** 2
      t475 = x1 * t376
      t479 = 0.1D1 / (-0.2D1 + t462)
      t480 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t270)
      t481 = t479 * t480
      t483 = t471 ** 2
      t485 = t379 * x4 * t267 * t483
      t488 = log(-0.4D1 * t93 * t485)
      t489 = t488 * t479
      t490 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t270)
      t495 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t270)
      t498 = t488 ** 2
      t505 = t479 * t490
      t512 = log(-0.4D1 * t167 * t485)
      t524 = -(0.180D3 * t61 * (t481 - t489 * t490) - 0.90D2 * t22 * (t4
     #79 * t495 - t489 * t480 + t498 * t479 * t490 / 0.2D1) - t72 * t505
     #) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (-t481 + t512 * t479 *
     # t490) + 0.180D3 * t61 * t505) * t116 * t164 / 0.2880D4
      t525 = FJET(XB1, XB2, s, t463 * t464, 0.0D0, t463 * t467, -t463 * 
     #t469, -s * t471 * t473 * t475 * x4, t524)
      t527 = KAPPA2(x1, x2, t144, 0.10D1, z)
      t528 = s * t527
      t529 = t464 * t147
      t531 = t464 * x3
      t534 = t527 ** 2
      t540 = 0.1D1 / (-0.2D1 + t527)
      t541 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, t144, 0.10D1)
      t542 = t540 * t541
      t543 = t379 * t147
      t544 = t534 ** 2
      t549 = log(-0.4D1 * t167 * t543 * x4 * t544)
      t551 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t144, 0.10D1)
      t556 = t540 * t551
      t567 = log(-0.4D1 * t146 * t11 * t379 * t147 * t544)
      t568 = t567 * t540
      t573 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, t144, 0.10D1)
      t576 = t567 ** 2
      t588 = -(0.90D2 * t22 * (-t542 + t549 * t540 * t551) + 0.180D3 * t
     #61 * t556) * t116 * t164 / 0.2880D4 - (0.180D3 * t61 * (t542 - t56
     #8 * t551) - 0.90D2 * t22 * (t540 * t573 - t568 * t541 + t576 * t54
     #0 * t551 / 0.2D1) - t72 * t556) * t116 * t163 / 0.2880D4
      t589 = FJET(XB1, XB2, s, -t528 * t529, t528 * t531, -t528 * t466, 
     #0.0D0, -s * t534 * t473 * t475 * x3, t588)
      t591 = KAPPA2(x1, x2, t144, t270, z)
      t592 = s * t591
      t597 = t591 ** 2
      t602 = cos(t7)
      t605 = sqrt(x3 * t147 * t320)
      t612 = 0.1D1 / (-0.2D1 + t591)
      t613 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, t144, t270)
      t615 = t597 ** 2
      t620 = log(0.4D1 * t167 * t543 * t320 * t615)
      t622 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t144, t270)
      t630 = 0.90D2 * t22 * (t612 * t613 - t620 * t612 * t622) - 0.180D3
     # * t61 * t612 * t622
      t634 = FJET(XB1, XB2, s, -t592 * t529, t592 * t531, t592 * t467, -
     #t592 * t469, s * t597 * t473 * t475 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t602 * t605), -t630 * t116 * t164 / 0.2880D4)
      bbggh2n5e1 = t265 * t264 + t373 * t372 + t460 * t459 + t525 * t524
     # + t589 * t588 - t634 * t630 * t116 * t163 * t88 / 0.2880D4

      end function



      doubleprecision function bbggh2n5e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = 0.1D1 - x3
      t33 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.10D1)
      t34 = -t33 + t18
      t36 = 0.1D1 / x3
      t38 = t27 * t36 * t29
      t41 = x3 * t7
      t42 = t10 * t13
      t45 = log(0.4D1 * t41 * t42)
      t47 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.10D1)
      t48 = -t32
      t52 = log(-0.4D1 * t41 * t42 * t48)
      t58 = 0.180D3 * t23 * t34
      t63 = t11 * t13
      t65 = log(0.4D1 * t63)
      t70 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
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
      t134 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.10D1)
      t136 = t129 ** 2
      t140 = t125 ** 2
      t153 = log(0.4D1 * t42)
      t161 = t153 ** 2
      t180 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t5 * t34 * t38 / 0.32D2 - (0.90D2 * t5 * (t6
     # - t45 * t18 - t47 + t52 * t33) - t58) * t27 * t36 / 0.2880D4 - (-
     #0.180D3 * t23 * (t6 - t65 * t18) + 0.90D2 * t5 * (t70 - t65 * t6 +
     # t72 * t18 / 0.2D1) + t84) * t27 / 0.2880D4 + (-0.180D3 * t23 * (-
     #t6 + t90 * t18) + 0.90D2 * t5 * (-t70 + t90 * t6 - t96 * t18 / 0.2
     #D1) - t84) * t29 / 0.5760D4 - (0.90D2 * t5 * (t6 - t108 * t18 - t4
     #7 + t114 * t33) - t58) * t36 * t29 / 0.5760D4 + (-0.180D3 * t23 * 
     #(-t6 + t125 * t18 + t47 - t129 * t33) + 0.90D2 * t5 * (t134 - t129
     # * t47 + t136 * t33 / 0.2D1 - t70 + t125 * t6 - t140 * t18 / 0.2D1
     #) - t83 * t34) * t36 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t153) 
     #* t5 * t70 / 0.5760D4 - (t79 - t81 + 0.180D3 * t153 * lh + 0.45D2 
     #* t161) * t5 * t6 / 0.5760D4 - (-0.2884936567583026D3 - 0.120D3 * 
     #t78 * lh + 0.60D2 * lh * t80 - t153 * t82 - 0.90D2 * t161 * lh - 0
     #.15D2 * t161 * t153) * t5 * t18 / 0.5760D4
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t180)
      t183 = -0.1D1 + x4
      t186 = -t183
      t187 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t186)
      t188 = t14 * t183
      t191 = log(-0.4D1 * t11 * t188)
      t192 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t186)
      t203 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, t186)
      t204 = -t192 + t203
      t208 = x4 * t183
      t211 = log(-0.4D1 * t42 * t208)
      t216 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t186)
      t218 = t211 ** 2
      t230 = log(-0.4D1 * t105 * t188)
      t232 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, t186)
      t236 = log(0.4D1 * t123 * t208 * t48)
      t247 = -(0.90D2 * t5 * (-t187 + t191 * t192) + 0.180D3 * t23 * t19
     #2) * t27 * t29 / 0.2880D4 - t5 * t204 * t38 / 0.32D2 + (-0.180D3 *
     # t23 * (t187 - t211 * t192) + 0.90D2 * t5 * (t216 - t211 * t187 + 
     #t218 * t192 / 0.2D1) + t83 * t192) * t29 / 0.5760D4 - (0.90D2 * t5
     # * (-t187 + t230 * t192 + t232 - t236 * t203) - 0.180D3 * t23 * t2
     #04) * t36 * t29 / 0.5760D4
      t248 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t183, t2 * x4, 0.0D0,
     # t247)
      t251 = -0.1D1 + x1
      t253 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t254 = t251 ** 2
      t255 = t7 * t254
      t259 = log(0.4D1 * t42 * t255 * x4)
      t260 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t266 = 0.180D3 * t23 * t260
      t278 = log(0.4D1 * t105 * t13 * t7 * t254)
      t289 = log(0.4D1 * t42 * t255)
      t294 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t296 = t289 ** 2
      t306 = -(0.90D2 * t5 * (-t253 + t259 * t260) + t266) * t27 * t29 /
     # 0.2880D4 + t5 * t260 * t38 / 0.32D2 - (0.90D2 * t5 * (-t253 + t27
     #8 * t260) + t266) * t27 * t36 / 0.2880D4 - (0.180D3 * t23 * (t253 
     #- t289 * t260) - 0.90D2 * t5 * (t294 - t289 * t253 + t296 * t260 /
     # 0.2D1) - t83 * t260) * t27 / 0.2880D4
      t307 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t251, 0.0D0, 0.0D0,
     # t306)
      t309 = KAPPA2(x1, x2, 0.10D1, t186, z)
      t310 = s * t309
      t311 = t1 * x1
      t313 = t1 * t251
      t314 = t313 * t183
      t316 = t313 * x4
      t318 = t309 ** 2
      t320 = t1 ** 2
      t322 = x1 * t251
      t326 = 0.1D1 / (-0.2D1 + t309)
      t327 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t186)
      t330 = t318 ** 2
      t335 = log(-0.4D1 * t63 * t254 * x4 * t183 * t330)
      t337 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t186)
      t353 = -(-0.90D2 * t5 * (t326 * t327 - t335 * t326 * t337) + 0.180
     #D3 * t23 * t326 * t337) * t27 * t29 / 0.2880D4 + t5 * t326 * t337 
     #* t38 / 0.32D2
      t354 = FJET(XB1, XB2, s, t310 * t311, 0.0D0, t310 * t314, -t310 * 
     #t316, -s * t318 * t320 * t322 * x4, t353)
      t356 = KAPPA2(x1, x2, t32, 0.10D1, z)
      t357 = s * t356
      t358 = t311 * t48
      t360 = t311 * x3
      t363 = t356 ** 2
      t369 = 0.1D1 / (-0.2D1 + t356)
      t371 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.10D1)
      t375 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.10D1)
      t379 = t363 ** 2
      t384 = log(-0.4D1 * t41 * t10 * t13 * t254 * t48 * t379)
      t397 = t5 * t369 * t371 * t38 / 0.32D2 - (-0.90D2 * t5 * (t369 * t
     #375 - t384 * t369 * t371) + 0.180D3 * t23 * t369 * t371) * t27 * t
     #36 / 0.2880D4
      t398 = FJET(XB1, XB2, s, -t357 * t358, t357 * t360, -t357 * t313, 
     #0.0D0, -s * t363 * t320 * t322 * x3, t397)
      t400 = KAPPA2(x1, x2, t32, t186, z)
      t401 = s * t400
      t406 = t400 ** 2
      t411 = cos(t8)
      t414 = sqrt(x3 * t48 * t208)
      t421 = 0.1D1 / (-0.2D1 + t400)
      t423 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, t186)
      t427 = FJET(XB1, XB2, s, -t401 * t358, t401 * t360, t401 * t314, -
     #t401 * t316, s * t406 * t320 * t322 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t411 * t414), -t5 * t421 * t423 * t38 / 0.32D2)
      bbggh2n5e0 = t181 * t180 + t248 * t247 + t307 * t306 + t354 * t353
     # + t398 * t397 - t427 * t5 * t421 * t423 * t27 * t36 * t29 / 0.32D
     #2

      end function



      doubleprecision function bbggh2n5em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 - x3
      t7 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t6, 0.10D1)
      t8 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t9 = -t7 + t8
      t10 = t5 * t9
      t11 = 0.1D1 / x3
      t12 = 0.1D1 / x4
      t13 = t11 * t12
      t16 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t18 = sin(x2 * 0.3141592653589793D1)
      t19 = t18 ** 2
      t20 = x3 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t25 = log(0.4D1 * t20 * t22)
      t27 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t6, 0.10D1)
      t28 = -t6
      t32 = log(-0.4D1 * t20 * t22 * t28)
      t37 = lh * t5
      t44 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t48 = t19 * t22
      t50 = log(0.4D1 * t48)
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t62 = t50 ** 2
      t68 = x1 ** 2
      t72 = log(0.4D1 * t68 * t19 * t22)
      t78 = 0.180D3 * t37 * t8
      t80 = 0.1D1 / x1
      t84 = t80 * t12
      t87 = t80 * t11
      t92 = log(0.4D1 * t48 * x4)
      t100 = -t10 * t13 / 0.64D2 + (0.90D2 * t5 * (-t16 + t25 * t8 + t27
     # - t32 * t7) + 0.180D3 * t37 * t9) * t11 / 0.5760D4 - t5 * t44 / 0
     #.64D2 - (-0.180D3 * lh - 0.90D2 * t50) * t5 * t16 / 0.5760D4 - (0.
     #180D3 * t56 - 0.30D2 * t58 + 0.180D3 * t50 * lh + 0.45D2 * t62) * 
     #t5 * t8 / 0.5760D4 - (0.90D2 * t5 * (t16 - t72 * t8) - t78) * t80 
     #/ 0.2880D4 - t5 * t8 * t84 / 0.32D2 - t10 * t87 / 0.32D2 + (0.90D2
     # * t5 * (-t16 + t92 * t8) + t78) * t12 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t100)
      t103 = -0.1D1 + x4
      t106 = -t103
      t107 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t106)
      t111 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, t106)
      t115 = log(-0.4D1 * t48 * x4 * t103)
      t125 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t6, t106)
      t130 = t5 * t107 * t84 / 0.32D2 + (0.90D2 * t5 * (t111 - t115 * t1
     #07) - 0.180D3 * t37 * t107) * t12 / 0.5760D4 - t5 * (-t107 + t125)
     # * t13 / 0.64D2
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t103, t2 * x4, 0.0D0,
     # t130)
      t134 = -0.1D1 + x1
      t136 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t137 = t134 ** 2
      t141 = log(0.4D1 * t48 * t68 * t137)
      t142 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t152 = t5 * t142
      t157 = -(-0.90D2 * t5 * (t136 - t141 * t142) + 0.180D3 * t37 * t14
     #2) * t80 / 0.2880D4 + t152 * t84 / 0.32D2 + t152 * t87 / 0.32D2
      t158 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t134, 0.0D0, 0.0D0,
     # t157)
      t160 = KAPPA2(x1, x2, 0.10D1, t106, z)
      t161 = s * t160
      t162 = t1 * x1
      t164 = t1 * t134
      t169 = t160 ** 2
      t171 = t1 ** 2
      t173 = x1 * t134
      t177 = 0.1D1 / (-0.2D1 + t160)
      t179 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, t106)
      t181 = t179 * t80 * t12
      t184 = FJET(XB1, XB2, s, t161 * t162, 0.0D0, t161 * t164 * t103, -
     #t161 * t164 * x4, -s * t169 * t171 * t173 * x4, t5 * t177 * t181 /
     # 0.32D2)
      t189 = KAPPA2(x1, x2, t6, 0.10D1, z)
      t190 = s * t189
      t196 = t189 ** 2
      t202 = 0.1D1 / (-0.2D1 + t189)
      t204 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t6, 0.10D1)
      t206 = t204 * t80 * t11
      t209 = FJET(XB1, XB2, s, -t190 * t162 * t28, t190 * t162 * x3, -t1
     #90 * t164, 0.0D0, -s * t196 * t171 * t173 * x3, t5 * t202 * t206 /
     # 0.32D2)
      bbggh2n5em1 = t101 * t100 + t131 * t130 + t158 * t157 + t184 * t5 
     #* t177 * t181 / 0.32D2 + t209 * t5 * t202 * t206 / 0.32D2

      end function



      doubleprecision function bbggh2n5em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t12 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, 0.1
     #0D1)
      t18 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t23 = sin(x2 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t25 = z ** 2
      t29 = log(0.4D1 * t24 / t25)
      t35 = 0.1D1 / x4
      t38 = -t7 * t8 / 0.32D2 + t5 * (-t6 + t12) / x3 / 0.64D2 - t5 * t1
     #8 / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t29) * t5 * t6 / 0.5760D4 -
     # t7 * t35 / 0.64D2
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t44 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.10D1)
      t48 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, t5 * t44 * t8 / 0.32D2)
      t53 = -0.1D1 + x4
      t57 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, -t53)
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t53, t2 * x4, 0.0D0, t
     #5 * t57 * t35 / 0.64D2)
      bbggh2n5em2 = t39 * t38 + t48 * t5 * t44 * t8 / 0.32D2 + t61 * t5 
     #* t57 * t35 / 0.64D2

      end function



      doubleprecision function bbggh2n5em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.10D1)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n5em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n5em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n5em4 = 0.0D0

      end function


      doubleprecision function bbggh2n6e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
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
      t24 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
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
      t140 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, 0.0D0)
      t141 = x3 * t91
      t142 = t141 * t9
      t143 = -t139
      t144 = t11 * t143
      t145 = t144 * x4
      t148 = log(-0.4D1 * t142 * t145)
      t149 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, 0.0D0)
      t153 = log(0.4D1 * t141 * t62)
      t158 = t58 - t149
      t163 = 0.1D1 / x3
      t164 = t163 * t88
      t170 = log(-0.4D1 * t141 * t12 * t143)
      t172 = t141 * t12
      t174 = log(0.4D1 * t172)
      t180 = t174 ** 2
      t183 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, 0.0D0)
      t185 = t170 ** 2
      t192 = -t72 * t158
      t197 = x3 * t9
      t200 = log(-0.4D1 * t197 * t144)
      t202 = t200 ** 2
      t205 = t197 * t11
      t207 = log(0.4D1 * t205)
      t209 = t207 ** 2
      t240 = log(-0.4D1 * t197 * t145)
      t244 = log(0.4D1 * t197 * t119)
      t250 = t244 ** 2
      t254 = t240 ** 2
      t264 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
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
     # * t61 * t158) * t116 * t164 / 0.2880D4 + (-0.180D3 * t61 * (t140 
     #- t170 * t149 - t39 + t174 * t58) + 0.90D2 * t22 * (-t24 + t174 * 
     #t39 - t180 * t58 / 0.2D1 + t183 - t170 * t140 + t185 * t149 / 0.2D
     #1) + t192) * t116 * t163 / 0.2880D4 - (-0.180D3 * t61 * (-t183 + t
     #200 * t140 - t202 * t149 / 0.2D1 + t24 - t207 * t39 + t209 * t58 /
     # 0.2D1) + t72 * (t39 - t207 * t58 - t140 + t200 * t149) + 0.90D2 *
     # t22 * (t200 * t183 - t202 * t140 / 0.2D1 + t202 * t200 * t149 / 0
     #.6D1 - t207 * t24 + t209 * t39 / 0.2D1 - t209 * t207 * t58 / 0.6D1
     #) + t85 * t158) * t163 / 0.5760D4 + (-0.180D3 * t61 * (t140 - t240
     # * t149 - t39 + t244 * t58) + 0.90D2 * t22 * (-t24 + t244 * t39 - 
     #t250 * t58 / 0.2D1 + t183 - t240 * t140 + t254 * t149 / 0.2D1) + t
     #192) * t163 * t88 / 0.5760D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t264)
      t268 = -0.1D1 + x4
      t270 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t271 = t119 * t268
      t274 = log(-0.4D1 * t92 * t271)
      t275 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t280 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t282 = t274 ** 2
      t295 = log(-0.4D1 * t142 * t271)
      t297 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, x4)
      t302 = log(0.4D1 * t142 * t119 * t268 * t143)
      t303 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, x4)
      t308 = -t275 + t303
      t315 = x4 * t268
      t319 = log(0.4D1 * t205 * t315 * t143)
      t323 = log(-0.4D1 * t197 * t271)
      t329 = t323 ** 2
      t332 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t139, x4)
      t334 = t319 ** 2
      t348 = log(-0.4D1 * t12 * t315)
      t350 = t348 ** 2
      t372 = -(-0.180D3 * t61 * (-t270 + t274 * t275) + 0.90D2 * t22 * (
     #-t280 + t274 * t270 - t282 * t275 / 0.2D1) - t72 * t275) * t116 * 
     #t88 / 0.2880D4 - (0.90D2 * t22 * (-t270 + t295 * t275 + t297 - t30
     #2 * t303) - 0.180D3 * t61 * t308) * t116 * t164 / 0.2880D4 + (-0.1
     #80D3 * t61 * (-t297 + t319 * t303 + t270 - t323 * t275) + 0.90D2 *
     # t22 * (t280 - t323 * t270 + t329 * t275 / 0.2D1 - t332 + t319 * t
     #297 - t334 * t303 / 0.2D1) - t72 * t308) * t163 * t88 / 0.5760D4 -
     # (0.180D3 * t61 * (t280 - t348 * t270 + t350 * t275 / 0.2D1) - t72
     # * (t270 - t348 * t275) - 0.90D2 * t22 * (-t348 * t280 + t350 * t2
     #70 / 0.2D1 - t350 * t348 * t275 / 0.6D1) - t85 * t275) * t88 / 0.5
     #760D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t268, 0.0D0,
     # t372)
      t375 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t376 = s * t375
      t377 = t1 * x1
      t379 = -0.1D1 + x1
      t380 = t1 * t379
      t382 = t375 ** 2
      t384 = t1 ** 2
      t389 = 0.1D1 / (-0.2D1 + t375)
      t390 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t391 = t389 * t390
      t392 = t379 ** 2
      t393 = t11 * t392
      t394 = t382 ** 2
      t395 = t393 * t394
      t398 = log(0.4D1 * t92 * t395)
      t399 = t398 * t389
      t400 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t402 = t398 ** 2
      t403 = t402 * t389
      t404 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t410 = t389 * t400
      t424 = t389 * t404
      t432 = log(0.4D1 * t93 * t392 * x4 * t394)
      t433 = t432 * t389
      t439 = t432 ** 2
      t446 = t72 * t424
      t454 = log(0.4D1 * t142 * t393 * x4 * t394)
      t467 = log(0.4D1 * t142 * t395)
      t468 = t467 * t389
      t474 = t467 ** 2
      t485 = (0.180D3 * t61 * (t391 - t399 * t400 + t403 * t404 / 0.2D1)
     # - t72 * (t410 - t399 * t404) - 0.90D2 * t22 * (-t399 * t390 + t40
     #3 * t400 / 0.2D1 - t402 * t398 * t389 * t404 / 0.6D1) - t85 * t424
     #) * t116 / 0.2880D4 - (-0.180D3 * t61 * (t410 - t433 * t404) + 0.9
     #0D2 * t22 * (t391 - t433 * t400 + t439 * t389 * t404 / 0.2D1) + t4
     #46) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (t410 - t454 * t389 
     #* t404) - 0.180D3 * t61 * t424) * t116 * t164 / 0.2880D4 + (-0.180
     #D3 * t61 * (-t410 + t468 * t404) + 0.90D2 * t22 * (-t391 + t468 * 
     #t400 - t474 * t389 * t404 / 0.2D1) - t446) * t116 * t163 / 0.2880D
     #4
      t486 = FJET(XB1, XB2, s, t376 * t377, 0.0D0, 0.0D0, -t376 * t380, 
     #-s * t382 * t384 * x1 * t379, t485)
      t488 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t489 = s * t488
      t491 = t380 * x4
      t493 = t380 * t268
      t495 = t488 ** 2
      t498 = x1 * t379
      t502 = 0.1D1 / (-0.2D1 + t488)
      t503 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t504 = t502 * t503
      t505 = t495 ** 2
      t507 = t315 * t505 * t392
      t510 = log(-0.4D1 * t93 * t507)
      t511 = t510 * t502
      t512 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t517 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t520 = t510 ** 2
      t527 = t502 * t512
      t534 = log(-0.4D1 * t172 * t507)
      t546 = -(0.180D3 * t61 * (t504 - t511 * t512) - 0.90D2 * t22 * (t5
     #02 * t517 - t511 * t503 + t520 * t502 * t512 / 0.2D1) - t72 * t527
     #) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (-t504 + t534 * t502 *
     # t512) + 0.180D3 * t61 * t527) * t116 * t164 / 0.2880D4
      t547 = FJET(XB1, XB2, s, t489 * t377, 0.0D0, -t489 * t491, t489 * 
     #t493, s * t495 * t384 * t498 * t268, t546)
      t549 = KAPPA2(x1, x2, t139, 0.0D0, z)
      t550 = s * t549
      t551 = t377 * t143
      t553 = t377 * x3
      t556 = t549 ** 2
      t562 = 0.1D1 / (-0.2D1 + t549)
      t563 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, t139, 0.0D0)
      t564 = t562 * t563
      t565 = t392 * t143
      t566 = t556 ** 2
      t571 = log(-0.4D1 * t172 * t565 * x4 * t566)
      t573 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t139, 0.0D0)
      t578 = t562 * t573
      t588 = log(-0.4D1 * t142 * t393 * t143 * t566)
      t589 = t588 * t562
      t594 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, t139, 0.0D0)
      t597 = t588 ** 2
      t609 = -(0.90D2 * t22 * (-t564 + t571 * t562 * t573) + 0.180D3 * t
     #61 * t578) * t116 * t164 / 0.2880D4 + (-0.180D3 * t61 * (t564 - t5
     #89 * t573) + 0.90D2 * t22 * (t562 * t594 - t589 * t563 + t597 * t5
     #62 * t573 / 0.2D1) + t72 * t578) * t116 * t163 / 0.2880D4
      t610 = FJET(XB1, XB2, s, -t550 * t551, t550 * t553, 0.0D0, -t550 *
     # t380, s * t556 * t384 * t498 * t143, t609)
      t612 = KAPPA2(x1, x2, t139, x4, z)
      t613 = s * t612
      t618 = t612 ** 2
      t623 = cos(t7)
      t626 = sqrt(x3 * t143 * t315)
      t633 = 0.1D1 / (-0.2D1 + t612)
      t634 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, t139, x4)
      t636 = t618 ** 2
      t641 = log(0.4D1 * t172 * t315 * t565 * t636)
      t643 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t139, x4)
      t651 = 0.90D2 * t22 * (t633 * t634 - t641 * t633 * t643) - 0.180D3
     # * t61 * t633 * t643
      t655 = FJET(XB1, XB2, s, -t613 * t551, t613 * t553, -t613 * t491, 
     #t613 * t493, s * t618 * t384 * t498 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t623 * t626), -t651 * t116 * t164 / 0.2880D4)
      bbggh2n6e1 = t265 * t264 + t373 * t372 + t486 * t485 + t547 * t546
     # + t610 * t609 - t655 * t651 * t116 * t163 * t88 / 0.2880D4

      end function



      doubleprecision function bbggh2n6e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = 0.1D1 - x3
      t33 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.0D0)
      t34 = t18 - t33
      t36 = 0.1D1 / x3
      t38 = t27 * t36 * t29
      t41 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.0D0)
      t42 = x3 * t7
      t43 = t10 * t13
      t44 = -t32
      t48 = log(-0.4D1 * t42 * t43 * t44)
      t52 = log(0.4D1 * t42 * t43)
      t59 = -0.180D3 * t23 * t34
      t64 = t11 * t13
      t66 = log(0.4D1 * t64)
      t71 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t73 = t66 ** 2
      t79 = lh ** 2
      t80 = 0.180D3 * t79
      t81 = 0.3141592653589793D1 ** 2
      t82 = 0.30D2 * t81
      t83 = t80 - t82
      t84 = t83 * t5
      t85 = t84 * t18
      t91 = log(0.4D1 * t43 * x4)
      t97 = t91 ** 2
      t106 = x3 * t10
      t107 = t13 * t44
      t111 = log(-0.4D1 * t106 * t107 * x4)
      t115 = log(0.4D1 * t106 * t14)
      t124 = t106 * t13
      t126 = log(0.4D1 * t124)
      t130 = log(-0.4D1 * t106 * t107)
      t135 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, 0.0D0)
      t137 = t130 ** 2
      t141 = t126 ** 2
      t153 = log(0.4D1 * t43)
      t161 = t153 ** 2
      t180 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t5 * t34 * t38 / 0.32D2 + (0.90D2 * t5 * (t4
     #1 - t48 * t33 - t6 + t52 * t18) - t59) * t27 * t36 / 0.2880D4 + (-
     #0.180D3 * t23 * (-t6 + t66 * t18) + 0.90D2 * t5 * (-t71 + t66 * t6
     # - t73 * t18 / 0.2D1) - t85) * t27 / 0.2880D4 - (-0.180D3 * t23 * 
     #(t6 - t91 * t18) + 0.90D2 * t5 * (t71 - t91 * t6 + t97 * t18 / 0.2
     #D1) + t85) * t29 / 0.5760D4 + (0.90D2 * t5 * (t41 - t111 * t33 - t
     #6 + t115 * t18) - t59) * t36 * t29 / 0.5760D4 - (-0.180D3 * t23 * 
     #(t6 - t126 * t18 - t41 + t130 * t33) + 0.90D2 * t5 * (-t135 + t130
     # * t41 - t137 * t33 / 0.2D1 + t71 - t126 * t6 + t141 * t18 / 0.2D1
     #) + t84 * t34) * t36 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t153) 
     #* t5 * t71 / 0.5760D4 - (t80 - t82 + 0.180D3 * t153 * lh + 0.45D2 
     #* t161) * t5 * t6 / 0.5760D4 - (-0.2884936567583026D3 - 0.120D3 * 
     #t79 * lh + 0.60D2 * lh * t81 - t153 * t83 - 0.90D2 * t161 * lh - 0
     #.15D2 * t161 * t153) * t5 * t18 / 0.5760D4
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t180)
      t184 = -0.1D1 + x4
      t186 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t187 = t14 * t184
      t190 = log(-0.4D1 * t11 * t187)
      t191 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t202 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, x4)
      t203 = -t191 + t202
      t207 = x4 * t184
      t210 = log(-0.4D1 * t43 * t207)
      t215 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t217 = t210 ** 2
      t227 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t32, x4)
      t231 = log(0.4D1 * t124 * t207 * t44)
      t235 = log(-0.4D1 * t106 * t187)
      t247 = -(0.90D2 * t5 * (-t186 + t190 * t191) + 0.180D3 * t23 * t19
     #1) * t27 * t29 / 0.2880D4 - t5 * t203 * t38 / 0.32D2 - (0.180D3 * 
     #t23 * (t186 - t210 * t191) - 0.90D2 * t5 * (t215 - t210 * t186 + t
     #217 * t191 / 0.2D1) - t84 * t191) * t29 / 0.5760D4 + (0.90D2 * t5 
     #* (-t227 + t231 * t202 + t186 - t235 * t191) + 0.180D3 * t23 * t20
     #3) * t36 * t29 / 0.5760D4
      t248 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t184, 0.0D0,
     # t247)
      t250 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t251 = s * t250
      t252 = t1 * x1
      t254 = -0.1D1 + x1
      t255 = t1 * t254
      t257 = t250 ** 2
      t259 = t1 ** 2
      t264 = 0.1D1 / (-0.2D1 + t250)
      t265 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t266 = t265 * t264
      t267 = t254 ** 2
      t269 = t257 ** 2
      t273 = log(0.4D1 * t64 * t267 * x4 * t269)
      t275 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t280 = t264 * t275
      t282 = 0.180D3 * t23 * t280
      t291 = t42 * t10
      t292 = t13 * t267
      t293 = t292 * t269
      t296 = log(0.4D1 * t291 * t293)
      t308 = log(0.4D1 * t11 * t293)
      t309 = t308 * t264
      t314 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t317 = t308 ** 2
      t328 = -(0.90D2 * t5 * (t266 - t273 * t264 * t275) - t282) * t27 *
     # t29 / 0.2880D4 - t5 * t264 * t275 * t38 / 0.32D2 + (0.90D2 * t5 *
     # (-t266 + t296 * t264 * t275) + t282) * t27 * t36 / 0.2880D4 + (0.
     #180D3 * t23 * (t266 - t309 * t275) - 0.90D2 * t5 * (t264 * t314 - 
     #t309 * t265 + t317 * t264 * t275 / 0.2D1) - t84 * t280) * t27 / 0.
     #2880D4
      t329 = FJET(XB1, XB2, s, t251 * t252, 0.0D0, 0.0D0, -t251 * t255, 
     #-s * t257 * t259 * x1 * t254, t328)
      t331 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t332 = s * t331
      t334 = t255 * x4
      t336 = t255 * t184
      t338 = t331 ** 2
      t341 = x1 * t254
      t345 = 0.1D1 / (-0.2D1 + t331)
      t346 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t348 = t338 ** 2
      t353 = log(-0.4D1 * t64 * t207 * t348 * t267)
      t355 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t371 = -(-0.90D2 * t5 * (t345 * t346 - t353 * t345 * t355) + 0.180
     #D3 * t23 * t345 * t355) * t27 * t29 / 0.2880D4 + t5 * t345 * t355 
     #* t38 / 0.32D2
      t372 = FJET(XB1, XB2, s, t332 * t252, 0.0D0, -t332 * t334, t332 * 
     #t336, s * t338 * t259 * t341 * t184, t371)
      t374 = KAPPA2(x1, x2, t32, 0.0D0, z)
      t375 = s * t374
      t376 = t252 * t44
      t378 = t252 * x3
      t381 = t374 ** 2
      t387 = 0.1D1 / (-0.2D1 + t374)
      t389 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.0D0)
      t393 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, t32, 0.0D0)
      t395 = t381 ** 2
      t400 = log(-0.4D1 * t291 * t292 * t44 * t395)
      t413 = t5 * t387 * t389 * t38 / 0.32D2 + (0.90D2 * t5 * (t387 * t3
     #93 - t400 * t387 * t389) - 0.180D3 * t23 * t387 * t389) * t27 * t3
     #6 / 0.2880D4
      t414 = FJET(XB1, XB2, s, -t375 * t376, t375 * t378, 0.0D0, -t375 *
     # t255, s * t381 * t259 * t341 * t44, t413)
      t416 = KAPPA2(x1, x2, t32, x4, z)
      t417 = s * t416
      t422 = t416 ** 2
      t427 = cos(t8)
      t430 = sqrt(x3 * t44 * t207)
      t437 = 0.1D1 / (-0.2D1 + t416)
      t439 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t32, x4)
      t443 = FJET(XB1, XB2, s, -t417 * t376, t417 * t378, -t417 * t334, 
     #t417 * t336, s * t422 * t259 * t341 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t427 * t430), -t5 * t437 * t439 * t38 / 0.32D2)
      bbggh2n6e0 = t181 * t180 + t248 * t247 + t329 * t328 + t372 * t371
     # + t414 * t413 - t443 * t5 * t437 * t439 * t27 * t36 * t29 / 0.32D
     #2

      end function



      doubleprecision function bbggh2n6em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.1D1 - x3
      t7 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t6, 0.0D0)
      t8 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t9 = t7 - t8
      t10 = t5 * t9
      t11 = 0.1D1 / x3
      t12 = 0.1D1 / x4
      t13 = t11 * t12
      t16 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t18 = sin(x2 * 0.3141592653589793D1)
      t19 = t18 ** 2
      t20 = x3 * t19
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t25 = log(0.4D1 * t20 * t22)
      t27 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t6, 0.0D0)
      t28 = -t6
      t32 = log(-0.4D1 * t20 * t22 * t28)
      t37 = lh * t5
      t44 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t48 = t19 * t22
      t50 = log(0.4D1 * t48)
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t62 = t50 ** 2
      t68 = x1 ** 2
      t69 = t68 * t19
      t72 = log(0.4D1 * t69 * t22)
      t78 = 0.180D3 * t37 * t8
      t80 = 0.1D1 / x1
      t84 = t80 * t12
      t92 = log(0.4D1 * t48 * x4)
      t100 = t10 * t13 / 0.64D2 - (0.90D2 * t5 * (t16 - t25 * t8 - t27 +
     # t32 * t7) + 0.180D3 * t37 * t9) * t11 / 0.5760D4 - t5 * t44 / 0.6
     #4D2 - (-0.180D3 * lh - 0.90D2 * t50) * t5 * t16 / 0.5760D4 - (0.18
     #0D3 * t56 - 0.30D2 * t58 + 0.180D3 * t50 * lh + 0.45D2 * t62) * t5
     # * t8 / 0.5760D4 + (0.90D2 * t5 * (-t16 + t72 * t8) + t78) * t80 /
     # 0.2880D4 - t5 * t8 * t84 / 0.32D2 + t10 * t80 * t11 / 0.32D2 - (0
     #.90D2 * t5 * (t16 - t92 * t8) - t78) * t12 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t100)
      t104 = -0.1D1 + x4
      t106 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t110 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t114 = log(-0.4D1 * t48 * x4 * t104)
      t124 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t6, x4)
      t129 = t5 * t106 * t84 / 0.32D2 - (-0.90D2 * t5 * (t110 - t114 * t
     #106) + 0.180D3 * t37 * t106) * t12 / 0.5760D4 + t5 * (-t124 + t106
     #) * t13 / 0.64D2
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t104, 0.0D0,
     # t129)
      t132 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t133 = s * t132
      t134 = t1 * x1
      t136 = -0.1D1 + x1
      t137 = t1 * t136
      t139 = t132 ** 2
      t141 = t1 ** 2
      t146 = 0.1D1 / (-0.2D1 + t132)
      t147 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t149 = t136 ** 2
      t151 = t139 ** 2
      t155 = log(0.4D1 * t69 * t22 * t149 * t151)
      t157 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t168 = t5 * t146
      t169 = t157 * t80
      t176 = (-0.90D2 * t5 * (t146 * t147 - t155 * t146 * t157) + 0.180D
     #3 * t37 * t146 * t157) * t80 / 0.2880D4 - t168 * t169 * t12 / 0.32
     #D2 - t168 * t169 * t11 / 0.32D2
      t177 = FJET(XB1, XB2, s, t133 * t134, 0.0D0, 0.0D0, -t133 * t137, 
     #-s * t139 * t141 * x1 * t136, t176)
      t179 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t180 = s * t179
      t186 = t179 ** 2
      t189 = x1 * t136
      t193 = 0.1D1 / (-0.2D1 + t179)
      t195 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t197 = t195 * t80 * t12
      t200 = FJET(XB1, XB2, s, t180 * t134, 0.0D0, -t180 * t137 * x4, t1
     #80 * t137 * t104, s * t186 * t141 * t189 * t104, t5 * t193 * t197 
     #/ 0.32D2)
      t205 = KAPPA2(x1, x2, t6, 0.0D0, z)
      t206 = s * t205
      t212 = t205 ** 2
      t218 = 0.1D1 / (-0.2D1 + t205)
      t220 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, t6, 0.0D0)
      t222 = t220 * t80 * t11
      t225 = FJET(XB1, XB2, s, -t206 * t134 * t28, t206 * t134 * x3, 0.0
     #D0, -t206 * t137, s * t212 * t141 * t189 * t28, t5 * t218 * t222 /
     # 0.32D2)
      bbggh2n6em1 = t101 * t100 + t130 * t129 + t177 * t176 + t200 * t5 
     #* t193 * t197 / 0.32D2 + t225 * t5 * t218 * t222 / 0.32D2

      end function



      doubleprecision function bbggh2n6em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t12 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, 0.0
     #D0)
      t18 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
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
      t57 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, 0.0D0)
      t61 = FJET(XB1, XB2, s, t42 * t1 * x1, 0.0D0, 0.0D0, -t42 * t1 * t
     #45, -s * t48 * t50 * x1 * t45, -t5 * t55 * t57 * t8 / 0.32D2)
      t70 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t5 * t70 * t35 / 0.64D2)
      bbggh2n6em2 = t39 * t38 - t61 * t5 * t55 * t57 * t8 / 0.32D2 + t74
     # * t5 * t70 * t35 / 0.64D2

      end function



      doubleprecision function bbggh2n6em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, 0.0D0)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n6em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n6em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n6em4 = 0.0D0

      end function


      doubleprecision function bbggh2n7e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
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
      t24 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
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
      t144 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t145 = t139 * t9
      t146 = -0.1D1 + x3
      t147 = t11 * t146
      t148 = t147 * x4
      t151 = log(-0.4D1 * t145 * t148)
      t152 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t157 = t58 - t152
      t162 = 0.1D1 / x3
      t163 = t162 * t88
      t166 = t139 * t12
      t168 = log(0.4D1 * t166)
      t173 = log(-0.4D1 * t139 * t12 * t146)
      t178 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t180 = t173 ** 2
      t184 = t168 ** 2
      t196 = x3 * t9
      t199 = log(-0.4D1 * t196 * t147)
      t201 = t199 ** 2
      t204 = t196 * t11
      t206 = log(0.4D1 * t204)
      t208 = t206 ** 2
      t239 = log(-0.4D1 * t196 * t148)
      t243 = log(0.4D1 * t196 * t119)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t264 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 + (-0.180D3 * t61 * (-t24 + t64 * t39 - t66
     # * t58 / 0.2D1) + t72 * (-t39 + t64 * t58) + 0.90D2 * t22 * (t64 *
     # t24 - t66 * t39 / 0.2D1 + t66 * t64 * t58 / 0.6D1) - t86) * t88 /
     # 0.5760D4 + (-0.180D3 * t61 * (-t24 + t95 * t39 - t97 * t58 / 0.2D
     #1) + t72 * (-t39 + t95 * t58) + 0.90D2 * t22 * (t95 * t24 - t97 * 
     #t39 / 0.2D1 + t97 * t95 * t58 / 0.6D1) - t86) * t116 / 0.2880D4 - 
     #(-0.180D3 * t61 * (t39 - t122 * t58) + 0.90D2 * t22 * (t24 - t122 
     #* t39 + t128 * t58 / 0.2D1) + t72 * t58) * t116 * t88 / 0.2880D4 -
     # (0.90D2 * t22 * (t39 - t142 * t58 - t144 + t151 * t152) - 0.180D3
     # * t61 * t157) * t116 * t163 / 0.2880D4 + (-0.180D3 * t61 * (-t39 
     #+ t168 * t58 + t144 - t173 * t152) + 0.90D2 * t22 * (t178 - t173 *
     # t144 + t180 * t152 / 0.2D1 - t24 + t168 * t39 - t184 * t58 / 0.2D
     #1) - t72 * t157) * t116 * t162 / 0.2880D4 - (-0.180D3 * t61 * (-t1
     #78 + t199 * t144 - t201 * t152 / 0.2D1 + t24 - t206 * t39 + t208 *
     # t58 / 0.2D1) + t72 * (t39 - t206 * t58 - t144 + t199 * t152) + 0.
     #90D2 * t22 * (t199 * t178 - t201 * t144 / 0.2D1 + t201 * t199 * t1
     #52 / 0.6D1 - t206 * t24 + t208 * t39 / 0.2D1 - t208 * t206 * t58 /
     # 0.6D1) + t85 * t157) * t162 / 0.5760D4 - (-0.180D3 * t61 * (-t144
     # + t239 * t152 + t39 - t243 * t58) + 0.90D2 * t22 * (-t178 + t239 
     #* t144 - t249 * t152 / 0.2D1 + t24 - t243 * t39 + t253 * t58 / 0.2
     #D1) + t72 * t157) * t162 * t88 / 0.5760D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t264)
      t267 = -0.1D1 + x4
      t270 = -t267
      t271 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t270)
      t272 = t119 * t267
      t275 = log(-0.4D1 * t92 * t272)
      t276 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t270)
      t281 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t270)
      t283 = t275 ** 2
      t294 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t270)
      t299 = log(0.4D1 * t145 * t119 * t267 * t146)
      t300 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t270)
      t304 = log(-0.4D1 * t145 * t272)
      t309 = -t276 + t300
      t318 = log(-0.4D1 * t196 * t272)
      t320 = x4 * t267
      t324 = log(0.4D1 * t204 * t320 * t146)
      t330 = t318 ** 2
      t333 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t270)
      t335 = t324 ** 2
      t348 = log(-0.4D1 * t12 * t320)
      t350 = t348 ** 2
      t372 = -(-0.180D3 * t61 * (-t271 + t275 * t276) + 0.90D2 * t22 * (
     #-t281 + t275 * t271 - t283 * t276 / 0.2D1) - t72 * t276) * t116 * 
     #t88 / 0.2880D4 - (0.90D2 * t22 * (t294 - t299 * t300 - t271 + t304
     # * t276) - 0.180D3 * t61 * t309) * t116 * t163 / 0.2880D4 - (-0.18
     #0D3 * t61 * (-t271 + t318 * t276 + t294 - t324 * t300) + 0.90D2 * 
     #t22 * (-t281 + t318 * t271 - t330 * t276 / 0.2D1 + t333 - t324 * t
     #294 + t335 * t300 / 0.2D1) + t72 * t309) * t162 * t88 / 0.5760D4 +
     # (-0.180D3 * t61 * (t281 - t348 * t271 + t350 * t276 / 0.2D1) + t7
     #2 * (t271 - t348 * t276) + 0.90D2 * t22 * (-t348 * t281 + t350 * t
     #271 / 0.2D1 - t350 * t348 * t276 / 0.6D1) + t85 * t276) * t88 / 0.
     #5760D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t267, t2 * x4, 0.0D0,
     # t372)
      t375 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t376 = s * t375
      t377 = t1 * x1
      t379 = -0.1D1 + x1
      t380 = t1 * t379
      t382 = t375 ** 2
      t384 = t1 ** 2
      t389 = 0.1D1 / (-0.2D1 + t375)
      t390 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t391 = t389 * t390
      t392 = t379 ** 2
      t393 = t91 * t392
      t394 = t382 ** 2
      t395 = t393 * t394
      t398 = log(0.4D1 * t12 * t395)
      t399 = t398 * t389
      t400 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t402 = t398 ** 2
      t403 = t402 * t389
      t404 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t410 = t389 * t400
      t424 = t389 * t404
      t428 = t392 * x4
      t432 = log(0.4D1 * t93 * t428 * t394)
      t433 = t432 * t389
      t439 = t432 ** 2
      t446 = t72 * t424
      t454 = log(0.4D1 * t204 * t393 * x4 * t394)
      t467 = log(0.4D1 * t204 * t395)
      t468 = t467 * t389
      t474 = t467 ** 2
      t485 = (0.180D3 * t61 * (t391 - t399 * t400 + t403 * t404 / 0.2D1)
     # - t72 * (t410 - t399 * t404) - 0.90D2 * t22 * (-t399 * t390 + t40
     #3 * t400 / 0.2D1 - t402 * t398 * t389 * t404 / 0.6D1) - t85 * t424
     #) * t116 / 0.2880D4 - (-0.180D3 * t61 * (t410 - t433 * t404) + 0.9
     #0D2 * t22 * (t391 - t433 * t400 + t439 * t389 * t404 / 0.2D1) + t4
     #46) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (t410 - t454 * t389 
     #* t404) - 0.180D3 * t61 * t424) * t116 * t163 / 0.2880D4 + (-0.180
     #D3 * t61 * (-t410 + t468 * t404) + 0.90D2 * t22 * (-t391 + t468 * 
     #t400 - t474 * t389 * t404 / 0.2D1) - t446) * t116 * t162 / 0.2880D
     #4
      t486 = FJET(XB1, XB2, s, 0.0D0, t376 * t377, -t376 * t380, 0.0D0, 
     #-s * t382 * t384 * x1 * t379, t485)
      t488 = KAPPA2(x1, x2, 0.0D0, t270, z)
      t489 = s * t488
      t491 = t380 * t267
      t493 = t380 * x4
      t495 = t488 ** 2
      t498 = x1 * t379
      t502 = 0.1D1 / (-0.2D1 + t488)
      t503 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t270)
      t504 = t502 * t503
      t505 = t495 ** 2
      t507 = t428 * t267 * t505
      t510 = log(-0.4D1 * t93 * t507)
      t511 = t510 * t502
      t512 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t270)
      t517 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t270)
      t520 = t510 ** 2
      t527 = t502 * t512
      t534 = log(-0.4D1 * t166 * t507)
      t546 = -(0.180D3 * t61 * (t504 - t511 * t512) - 0.90D2 * t22 * (t5
     #02 * t517 - t511 * t503 + t520 * t502 * t512 / 0.2D1) - t72 * t527
     #) * t116 * t88 / 0.2880D4 - (0.90D2 * t22 * (-t504 + t534 * t502 *
     # t512) + 0.180D3 * t61 * t527) * t116 * t163 / 0.2880D4
      t547 = FJET(XB1, XB2, s, 0.0D0, t489 * t377, t489 * t491, -t489 * 
     #t493, s * t495 * t384 * t498 * t267, t546)
      t549 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t550 = s * t549
      t551 = t377 * x3
      t553 = t377 * t146
      t556 = t549 ** 2
      t562 = 0.1D1 / (-0.2D1 + t549)
      t563 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t564 = t562 * t563
      t565 = t392 * t146
      t566 = t556 ** 2
      t571 = log(-0.4D1 * t166 * t565 * x4 * t566)
      t573 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t578 = t562 * t573
      t588 = log(-0.4D1 * t204 * t393 * t146 * t566)
      t589 = t588 * t562
      t594 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t597 = t588 ** 2
      t609 = -(0.90D2 * t22 * (-t564 + t571 * t562 * t573) + 0.180D3 * t
     #61 * t578) * t116 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t564 - t5
     #89 * t573) + 0.90D2 * t22 * (t562 * t594 - t589 * t563 + t597 * t5
     #62 * t573 / 0.2D1) + t72 * t578) * t116 * t162 / 0.2880D4
      t610 = FJET(XB1, XB2, s, t550 * t551, -t550 * t553, -t550 * t380, 
     #0.0D0, s * t556 * t384 * t498 * t146, t609)
      t612 = KAPPA2(x1, x2, x3, t270, z)
      t613 = s * t612
      t618 = t612 ** 2
      t623 = cos(t7)
      t626 = sqrt(x3 * t146 * t320)
      t633 = 0.1D1 / (-0.2D1 + t612)
      t634 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, t270)
      t636 = t618 ** 2
      t641 = log(0.4D1 * t166 * t565 * t320 * t636)
      t643 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t270)
      t651 = 0.90D2 * t22 * (t633 * t634 - t641 * t633 * t643) - 0.180D3
     # * t61 * t633 * t643
      t655 = FJET(XB1, XB2, s, t613 * t551, -t613 * t553, t613 * t491, -
     #t613 * t493, s * t618 * t384 * t498 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t623 * t626), -t651 * t116 * t163 / 0.2880D4)
      bbggh2n7e1 = t265 * t264 + t373 * t372 + t486 * t485 + t547 * t546
     # + t610 * t609 - t655 * t651 * t116 * t162 * t88 / 0.2880D4

      end function



      doubleprecision function bbggh2n7e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t33 = t18 - t32
      t35 = 0.1D1 / x3
      t37 = t27 * t35 * t29
      t40 = x3 * t7
      t41 = t10 * t13
      t44 = log(0.4D1 * t40 * t41)
      t46 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t47 = -0.1D1 + x3
      t51 = log(-0.4D1 * t40 * t41 * t47)
      t63 = t11 * t13
      t65 = log(0.4D1 * t63)
      t70 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t72 = t65 ** 2
      t78 = lh ** 2
      t79 = 0.180D3 * t78
      t80 = 0.3141592653589793D1 ** 2
      t81 = 0.30D2 * t80
      t82 = t79 - t81
      t83 = t82 * t5
      t84 = t83 * t18
      t90 = log(0.4D1 * t41 * x4)
      t96 = t90 ** 2
      t105 = x3 * t10
      t106 = t13 * t47
      t110 = log(-0.4D1 * t105 * t106 * x4)
      t114 = log(0.4D1 * t105 * t14)
      t125 = t105 * t13
      t127 = log(0.4D1 * t125)
      t131 = log(-0.4D1 * t105 * t106)
      t136 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t138 = t131 ** 2
      t142 = t127 ** 2
      t154 = log(0.4D1 * t41)
      t162 = t154 ** 2
      t181 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 - t5 * t33 * t37 / 0.32D2 + (0.90D2 * t5 * (-t
     #6 + t44 * t18 + t46 - t51 * t32) + 0.180D3 * t23 * t33) * t27 * t3
     #5 / 0.2880D4 + (-0.180D3 * t23 * (-t6 + t65 * t18) + 0.90D2 * t5 *
     # (-t70 + t65 * t6 - t72 * t18 / 0.2D1) - t84) * t27 / 0.2880D4 + (
     #-0.180D3 * t23 * (-t6 + t90 * t18) + 0.90D2 * t5 * (-t70 + t90 * t
     #6 - t96 * t18 / 0.2D1) - t84) * t29 / 0.5760D4 - (0.90D2 * t5 * (-
     #t46 + t110 * t32 + t6 - t114 * t18) - 0.180D3 * t23 * t33) * t35 *
     # t29 / 0.5760D4 - (-0.180D3 * t23 * (t6 - t127 * t18 - t46 + t131 
     #* t32) + 0.90D2 * t5 * (-t136 + t131 * t46 - t138 * t32 / 0.2D1 + 
     #t70 - t127 * t6 + t142 * t18 / 0.2D1) + t83 * t33) * t35 / 0.5760D
     #4 - (-0.180D3 * lh - 0.90D2 * t154) * t5 * t70 / 0.5760D4 - (t79 -
     # t81 + 0.180D3 * t154 * lh + 0.45D2 * t162) * t5 * t6 / 0.5760D4 -
     # (-0.2884936567583026D3 - 0.120D3 * t78 * lh + 0.60D2 * lh * t80 -
     # t154 * t82 - 0.90D2 * t162 * lh - 0.15D2 * t162 * t154) * t5 * t1
     #8 / 0.5760D4
      t182 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t181)
      t184 = -0.1D1 + x4
      t187 = -t184
      t188 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t187)
      t189 = t14 * t184
      t192 = log(-0.4D1 * t11 * t189)
      t193 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t187)
      t204 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t187)
      t205 = -t193 + t204
      t209 = x4 * t184
      t212 = log(-0.4D1 * t41 * t209)
      t217 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t187)
      t219 = t212 ** 2
      t231 = log(-0.4D1 * t105 * t189)
      t233 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t187)
      t237 = log(0.4D1 * t125 * t209 * t47)
      t248 = -(0.90D2 * t5 * (-t188 + t192 * t193) + 0.180D3 * t23 * t19
     #3) * t27 * t29 / 0.2880D4 - t5 * t205 * t37 / 0.32D2 + (-0.180D3 *
     # t23 * (t188 - t212 * t193) + 0.90D2 * t5 * (t217 - t212 * t188 + 
     #t219 * t193 / 0.2D1) + t83 * t193) * t29 / 0.5760D4 - (0.90D2 * t5
     # * (-t188 + t231 * t193 + t233 - t237 * t204) - 0.180D3 * t23 * t2
     #05) * t35 * t29 / 0.5760D4
      t249 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t184, t2 * x4, 0.0D0,
     # t248)
      t251 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t252 = s * t251
      t253 = t1 * x1
      t255 = -0.1D1 + x1
      t256 = t1 * t255
      t258 = t251 ** 2
      t260 = t1 ** 2
      t265 = 0.1D1 / (-0.2D1 + t251)
      t266 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t267 = t265 * t266
      t268 = t255 ** 2
      t269 = t268 * x4
      t270 = t258 ** 2
      t274 = log(0.4D1 * t63 * t269 * t270)
      t276 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t281 = t265 * t276
      t283 = 0.180D3 * t23 * t281
      t292 = t7 * t268
      t293 = t292 * t270
      t296 = log(0.4D1 * t125 * t293)
      t308 = log(0.4D1 * t41 * t293)
      t309 = t308 * t265
      t314 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t317 = t308 ** 2
      t328 = -(0.90D2 * t5 * (t267 - t274 * t265 * t276) - t283) * t27 *
     # t29 / 0.2880D4 - t5 * t265 * t276 * t37 / 0.32D2 + (0.90D2 * t5 *
     # (-t267 + t296 * t265 * t276) + t283) * t27 * t35 / 0.2880D4 + (0.
     #180D3 * t23 * (t267 - t309 * t276) - 0.90D2 * t5 * (t265 * t314 - 
     #t309 * t266 + t317 * t265 * t276 / 0.2D1) - t83 * t281) * t27 / 0.
     #2880D4
      t329 = FJET(XB1, XB2, s, 0.0D0, t252 * t253, -t252 * t256, 0.0D0, 
     #-s * t258 * t260 * x1 * t255, t328)
      t331 = KAPPA2(x1, x2, 0.0D0, t187, z)
      t332 = s * t331
      t334 = t256 * t184
      t336 = t256 * x4
      t338 = t331 ** 2
      t341 = x1 * t255
      t345 = 0.1D1 / (-0.2D1 + t331)
      t346 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t187)
      t348 = t338 ** 2
      t353 = log(-0.4D1 * t63 * t269 * t184 * t348)
      t355 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t187)
      t371 = -(-0.90D2 * t5 * (t345 * t346 - t353 * t345 * t355) + 0.180
     #D3 * t23 * t345 * t355) * t27 * t29 / 0.2880D4 + t5 * t345 * t355 
     #* t37 / 0.32D2
      t372 = FJET(XB1, XB2, s, 0.0D0, t332 * t253, t332 * t334, -t332 * 
     #t336, s * t338 * t260 * t341 * t184, t371)
      t374 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t375 = s * t374
      t376 = t253 * x3
      t378 = t253 * t47
      t381 = t374 ** 2
      t387 = 0.1D1 / (-0.2D1 + t374)
      t389 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t393 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t395 = t381 ** 2
      t400 = log(-0.4D1 * t125 * t292 * t47 * t395)
      t413 = t5 * t387 * t389 * t37 / 0.32D2 + (0.90D2 * t5 * (t387 * t3
     #93 - t400 * t387 * t389) - 0.180D3 * t23 * t387 * t389) * t27 * t3
     #5 / 0.2880D4
      t414 = FJET(XB1, XB2, s, t375 * t376, -t375 * t378, -t375 * t256, 
     #0.0D0, s * t381 * t260 * t341 * t47, t413)
      t416 = KAPPA2(x1, x2, x3, t187, z)
      t417 = s * t416
      t422 = t416 ** 2
      t427 = cos(t8)
      t430 = sqrt(x3 * t47 * t209)
      t437 = 0.1D1 / (-0.2D1 + t416)
      t439 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t187)
      t443 = FJET(XB1, XB2, s, t417 * t376, -t417 * t378, t417 * t334, -
     #t417 * t336, s * t422 * t260 * t341 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t427 * t430), -t5 * t437 * t439 * t37 / 0.32D2)
      bbggh2n7e0 = t182 * t181 + t249 * t248 + t329 * t328 + t372 * t371
     # + t414 * t413 - t443 * t5 * t437 * t439 * t27 * t35 * t29 / 0.32D
     #2

      end function



      doubleprecision function bbggh2n7em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t7 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t8 = t6 - t7
      t10 = 0.1D1 / x3
      t11 = 0.1D1 / x4
      t12 = t10 * t11
      t15 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t17 = sin(x2 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t19 = x3 * t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t19 * t21)
      t26 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t27 = -0.1D1 + x3
      t31 = log(-0.4D1 * t19 * t21 * t27)
      t36 = lh * t5
      t42 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
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
      t92 = log(0.4D1 * t46 * x4)
      t100 = -t5 * t8 * t12 / 0.64D2 - (0.90D2 * t5 * (t15 - t24 * t6 - 
     #t26 + t31 * t7) - 0.180D3 * t36 * t8) * t10 / 0.5760D4 - t5 * t42 
     #/ 0.64D2 - (-0.180D3 * lh - 0.90D2 * t48) * t5 * t15 / 0.5760D4 - 
     #(0.180D3 * t54 - 0.30D2 * t56 + 0.180D3 * t48 * lh + 0.45D2 * t60)
     # * t5 * t6 / 0.5760D4 + (0.90D2 * t5 * (-t15 + t70 * t6) + t76) * 
     #t78 / 0.2880D4 - t5 * t6 * t82 / 0.32D2 - t5 * t8 * t78 * t10 / 0.
     #32D2 + (0.90D2 * t5 * (-t15 + t92 * t6) + t76) * t11 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t100)
      t103 = -0.1D1 + x4
      t106 = -t103
      t107 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t106)
      t111 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, t106)
      t115 = log(-0.4D1 * t46 * x4 * t103)
      t125 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, t106)
      t130 = t5 * t107 * t82 / 0.32D2 + (0.90D2 * t5 * (t111 - t115 * t1
     #07) - 0.180D3 * t36 * t107) * t11 / 0.5760D4 - t5 * (-t107 + t125)
     # * t12 / 0.64D2
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t103, t2 * x4, 0.0D0,
     # t130)
      t133 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t134 = s * t133
      t135 = t1 * x1
      t137 = -0.1D1 + x1
      t138 = t1 * t137
      t140 = t133 ** 2
      t142 = t1 ** 2
      t147 = 0.1D1 / (-0.2D1 + t133)
      t148 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t150 = t137 ** 2
      t152 = t140 ** 2
      t156 = log(0.4D1 * t46 * t66 * t150 * t152)
      t158 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t169 = t5 * t147
      t170 = t158 * t78
      t177 = (-0.90D2 * t5 * (t147 * t148 - t156 * t147 * t158) + 0.180D
     #3 * t36 * t147 * t158) * t78 / 0.2880D4 - t169 * t170 * t11 / 0.32
     #D2 - t169 * t170 * t10 / 0.32D2
      t178 = FJET(XB1, XB2, s, 0.0D0, t134 * t135, -t134 * t138, 0.0D0, 
     #-s * t140 * t142 * x1 * t137, t177)
      t180 = KAPPA2(x1, x2, 0.0D0, t106, z)
      t181 = s * t180
      t187 = t180 ** 2
      t190 = x1 * t137
      t194 = 0.1D1 / (-0.2D1 + t180)
      t196 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t106)
      t198 = t196 * t78 * t11
      t201 = FJET(XB1, XB2, s, 0.0D0, t181 * t135, t181 * t138 * t103, -
     #t181 * t138 * x4, s * t187 * t142 * t190 * t103, t5 * t194 * t198 
     #/ 0.32D2)
      t206 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t207 = s * t206
      t213 = t206 ** 2
      t219 = 0.1D1 / (-0.2D1 + t206)
      t221 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t223 = t221 * t78 * t10
      t226 = FJET(XB1, XB2, s, t207 * t135 * x3, -t207 * t135 * t27, -t2
     #07 * t138, 0.0D0, s * t213 * t142 * t190 * t27, t5 * t219 * t223 /
     # 0.32D2)
      bbggh2n7em1 = t101 * t100 + t131 * t130 + t178 * t177 + t201 * t5 
     #* t194 * t198 / 0.32D2 + t226 * t5 * t219 * t223 / 0.32D2

      end function



      doubleprecision function bbggh2n7em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.10D1)
      t17 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t34 = 0.1D1 / x4
      t37 = -t7 * t8 / 0.32D2 - t5 * (t6 - t11) / x3 / 0.64D2 - t5 * t17
     # / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t28) * t5 * t6 / 0.5760D4 - 
     #t7 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t40 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t41 = s * t40
      t44 = -0.1D1 + x1
      t47 = t40 ** 2
      t49 = t1 ** 2
      t54 = 0.1D1 / (-0.2D1 + t40)
      t56 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t60 = FJET(XB1, XB2, s, 0.0D0, t41 * t1 * x1, -t41 * t1 * t44, 0.0
     #D0, -s * t47 * t49 * x1 * t44, -t5 * t54 * t56 * t8 / 0.32D2)
      t66 = -0.1D1 + x4
      t70 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, -t66)
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t66, t2 * x4, 0.0D0, t
     #5 * t70 * t34 / 0.64D2)
      bbggh2n7em2 = t38 * t37 - t60 * t5 * t54 * t56 * t8 / 0.32D2 + t74
     # * t5 * t70 * t34 / 0.64D2

      end function



      doubleprecision function bbggh2n7em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.10D1)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n7em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n7em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n7em4 = 0.0D0

      end function


      doubleprecision function bbggh2n8e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
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
      t24 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t28 = 0.120D3 * t3 * lh
      t30 = 0.60D2 * lh * t5
      t31 = t4 - t6
      t35 = t17 * t14
      t39 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t42 = t5 ** 2
      t43 = t3 ** 2
      t48 = -0.2884936567583026D3 - t28 + t30
      t50 = t17 ** 2
      t58 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
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
      t139 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t140 = x3 * t91
      t141 = t140 * t9
      t142 = -0.1D1 + x3
      t143 = t11 * t142
      t144 = t143 * x4
      t147 = log(-0.4D1 * t141 * t144)
      t148 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t152 = log(0.4D1 * t140 * t62)
      t157 = -t58 + t148
      t162 = 0.1D1 / x3
      t163 = t162 * t88
      t166 = t140 * t12
      t168 = log(0.4D1 * t166)
      t173 = log(-0.4D1 * t140 * t12 * t142)
      t178 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t180 = t173 ** 2
      t184 = t168 ** 2
      t190 = t72 * t157
      t195 = x3 * t9
      t198 = log(-0.4D1 * t195 * t143)
      t200 = t198 ** 2
      t203 = t195 * t11
      t205 = log(0.4D1 * t203)
      t207 = t205 ** 2
      t239 = log(0.4D1 * t195 * t119)
      t243 = log(-0.4D1 * t195 * t144)
      t249 = t239 ** 2
      t253 = t243 ** 2
      t263 = -(t4 - t6 + 0.180D3 * t14 * lh + 0.45D2 * t17) * t22 * t24 
     #/ 0.5760D4 - (-0.2884936567583026D3 - t28 + t30 - t14 * t31 - 0.90
     #D2 * t17 * lh - 0.15D2 * t35) * t22 * t39 / 0.5760D4 - (t42 + 0.60
     #D2 * t43 + 0.5769873135166051D3 * lh - 0.60D2 * t3 * t5 - t14 * t4
     #8 + 0.15D2 / 0.4D1 * t50 + t17 * t31 / 0.2D1 + 0.30D2 * t35 * lh) 
     #* t22 * t58 / 0.5760D4 + (-0.180D3 * t61 * (-t24 + t64 * t39 - t66
     # * t58 / 0.2D1) + t72 * (-t39 + t64 * t58) + 0.90D2 * t22 * (t64 *
     # t24 - t66 * t39 / 0.2D1 + t66 * t64 * t58 / 0.6D1) - t86) * t88 /
     # 0.5760D4 - (-0.180D3 * t61 * (t24 - t95 * t39 + t97 * t58 / 0.2D1
     #) + t72 * (t39 - t95 * t58) + 0.90D2 * t22 * (-t95 * t24 + t97 * t
     #39 / 0.2D1 - t97 * t95 * t58 / 0.6D1) + t86) * t116 / 0.2880D4 - (
     #-0.180D3 * t61 * (t39 - t122 * t58) + 0.90D2 * t22 * (t24 - t122 *
     # t39 + t128 * t58 / 0.2D1) + t72 * t58) * t116 * t88 / 0.2880D4 + 
     #(0.90D2 * t22 * (t139 - t147 * t148 - t39 + t152 * t58) - 0.180D3 
     #* t61 * t157) * t116 * t163 / 0.2880D4 + (-0.180D3 * t61 * (-t39 +
     # t168 * t58 + t139 - t173 * t148) + 0.90D2 * t22 * (t178 - t173 * 
     #t139 + t180 * t148 / 0.2D1 - t24 + t168 * t39 - t184 * t58 / 0.2D1
     #) + t190) * t116 * t162 / 0.2880D4 - (-0.180D3 * t61 * (-t178 + t1
     #98 * t139 - t200 * t148 / 0.2D1 + t24 - t205 * t39 + t207 * t58 / 
     #0.2D1) + t72 * (t39 - t205 * t58 - t139 + t198 * t148) + 0.90D2 * 
     #t22 * (t198 * t178 - t200 * t139 / 0.2D1 + t200 * t198 * t148 / 0.
     #6D1 - t205 * t24 + t207 * t39 / 0.2D1 - t207 * t205 * t58 / 0.6D1)
     # - t85 * t157) * t162 / 0.5760D4 + (-0.180D3 * t61 * (-t39 + t239 
     #* t58 + t139 - t243 * t148) + 0.90D2 * t22 * (-t24 + t239 * t39 - 
     #t249 * t58 / 0.2D1 + t178 - t243 * t139 + t253 * t148 / 0.2D1) + t
     #190) * t162 * t88 / 0.5760D4
      t264 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t263)
      t267 = -0.1D1 + x4
      t269 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t270 = t119 * t267
      t273 = log(-0.4D1 * t92 * t270)
      t274 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t279 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t281 = t273 ** 2
      t294 = log(-0.4D1 * t141 * t270)
      t296 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t301 = log(0.4D1 * t141 * t119 * t267 * t142)
      t302 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t307 = t274 - t302
      t314 = x4 * t267
      t318 = log(0.4D1 * t203 * t314 * t142)
      t322 = log(-0.4D1 * t195 * t270)
      t327 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t329 = t318 ** 2
      t333 = t322 ** 2
      t346 = log(-0.4D1 * t12 * t314)
      t348 = t346 ** 2
      t370 = -(-0.180D3 * t61 * (-t269 + t273 * t274) + 0.90D2 * t22 * (
     #-t279 + t273 * t269 - t281 * t274 / 0.2D1) - t72 * t274) * t116 * 
     #t88 / 0.2880D4 + (0.90D2 * t22 * (t269 - t294 * t274 - t296 + t301
     # * t302) - 0.180D3 * t61 * t307) * t116 * t163 / 0.2880D4 + (-0.18
     #0D3 * t61 * (-t296 + t318 * t302 + t269 - t322 * t274) + 0.90D2 * 
     #t22 * (-t327 + t318 * t296 - t329 * t302 / 0.2D1 + t279 - t322 * t
     #269 + t333 * t274 / 0.2D1) + t72 * t307) * t162 * t88 / 0.5760D4 +
     # (-0.180D3 * t61 * (t279 - t346 * t269 + t348 * t274 / 0.2D1) + t7
     #2 * (t269 - t346 * t274) + 0.90D2 * t22 * (-t346 * t279 + t348 * t
     #269 / 0.2D1 - t348 * t346 * t274 / 0.6D1) + t85 * t274) * t88 / 0.
     #5760D4
      t371 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t267, 0.0D0,
     # t370)
      t374 = -0.1D1 + x1
      t376 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t377 = t374 ** 2
      t378 = t91 * t377
      t381 = log(0.4D1 * t12 * t378)
      t382 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t384 = t381 ** 2
      t385 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t406 = t378 * x4
      t409 = log(0.4D1 * t12 * t406)
      t415 = t409 ** 2
      t421 = t72 * t385
      t427 = log(0.4D1 * t203 * t406)
      t441 = log(0.4D1 * t195 * t11 * t91 * t377)
      t447 = t441 ** 2
      t457 = -(0.180D3 * t61 * (t376 - t381 * t382 + t384 * t385 / 0.2D1
     #) - t72 * (t382 - t381 * t385) - 0.90D2 * t22 * (-t381 * t376 + t3
     #84 * t382 / 0.2D1 - t384 * t381 * t385 / 0.6D1) - t85 * t385) * t1
     #16 / 0.2880D4 - (-0.180D3 * t61 * (-t382 + t409 * t385) + 0.90D2 *
     # t22 * (-t376 + t409 * t382 - t415 * t385 / 0.2D1) - t421) * t116 
     #* t88 / 0.2880D4 + (0.90D2 * t22 * (t382 - t427 * t385) - 0.180D3 
     #* t61 * t385) * t116 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t382 -
     # t441 * t385) + 0.90D2 * t22 * (t376 - t441 * t382 + t447 * t385 /
     # 0.2D1) + t421) * t116 * t162 / 0.2880D4
      t458 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t374, 0.0D0,
     # t457)
      t460 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t461 = s * t460
      t462 = t1 * x1
      t464 = t1 * t374
      t465 = t464 * x4
      t467 = t464 * t267
      t469 = t460 ** 2
      t471 = t1 ** 2
      t473 = x1 * t374
      t477 = 0.1D1 / (-0.2D1 + t460)
      t478 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t479 = t477 * t478
      t480 = t469 ** 2
      t482 = t314 * t480 * t377
      t485 = log(-0.4D1 * t93 * t482)
      t486 = t485 * t477
      t487 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t492 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t495 = t485 ** 2
      t502 = t477 * t487
      t509 = log(-0.4D1 * t166 * t482)
      t521 = -(0.180D3 * t61 * (t479 - t486 * t487) - 0.90D2 * t22 * (t4
     #77 * t492 - t486 * t478 + t495 * t477 * t487 / 0.2D1) - t72 * t502
     #) * t116 * t88 / 0.2880D4 + (0.90D2 * t22 * (t479 - t509 * t477 * 
     #t487) - 0.180D3 * t61 * t502) * t116 * t163 / 0.2880D4
      t522 = FJET(XB1, XB2, s, 0.0D0, t461 * t462, -t461 * t465, t461 * 
     #t467, -s * t469 * t471 * t473 * x4, t521)
      t524 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t525 = s * t524
      t526 = t462 * x3
      t528 = t462 * t142
      t531 = t524 ** 2
      t537 = 0.1D1 / (-0.2D1 + t524)
      t538 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t539 = t537 * t538
      t540 = t377 * t142
      t541 = t531 ** 2
      t546 = log(-0.4D1 * t166 * t540 * x4 * t541)
      t548 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t553 = t537 * t548
      t563 = log(-0.4D1 * t203 * t378 * t142 * t541)
      t564 = t563 * t537
      t569 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t572 = t563 ** 2
      t584 = (0.90D2 * t22 * (t539 - t546 * t537 * t548) - 0.180D3 * t61
     # * t553) * t116 * t163 / 0.2880D4 + (-0.180D3 * t61 * (t539 - t564
     # * t548) + 0.90D2 * t22 * (t537 * t569 - t564 * t538 + t572 * t537
     # * t548 / 0.2D1) + t72 * t553) * t116 * t162 / 0.2880D4
      t585 = FJET(XB1, XB2, s, t525 * t526, -t525 * t528, 0.0D0, -t525 *
     # t464, -s * t531 * t471 * t473 * x3, t584)
      t587 = KAPPA2(x1, x2, x3, x4, z)
      t588 = s * t587
      t593 = t587 ** 2
      t598 = cos(t7)
      t601 = sqrt(x3 * t142 * t314)
      t608 = 0.1D1 / (-0.2D1 + t587)
      t609 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t611 = t593 ** 2
      t616 = log(0.4D1 * t166 * t314 * t540 * t611)
      t618 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t626 = -0.90D2 * t22 * (t608 * t609 - t616 * t608 * t618) + 0.180D
     #3 * t61 * t608 * t618
      t630 = FJET(XB1, XB2, s, t588 * t526, -t588 * t528, -t588 * t465, 
     #t588 * t467, s * t593 * t471 * t473 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t598 * t601), t626 * t116 * t163 / 0.2880D4)
      bbggh2n8e1 = t264 * t263 + t371 * t370 + t458 * t457 + t522 * t521
     # + t585 * t584 + t630 * t626 * t116 * t162 * t88 / 0.2880D4

      end function



      doubleprecision function bbggh2n8e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t7 = x1 ** 2
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t23 = lh * t5
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t32 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t33 = -t18 + t32
      t35 = 0.1D1 / x3
      t37 = t27 * t35 * t29
      t40 = x3 * t7
      t41 = t10 * t13
      t44 = log(0.4D1 * t40 * t41)
      t46 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t47 = -0.1D1 + x3
      t51 = log(-0.4D1 * t40 * t41 * t47)
      t57 = 0.180D3 * t23 * t33
      t62 = t11 * t13
      t64 = log(0.4D1 * t62)
      t69 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t71 = t64 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t5
      t83 = t82 * t18
      t89 = log(0.4D1 * t41 * x4)
      t95 = t89 ** 2
      t104 = x3 * t10
      t107 = log(0.4D1 * t104 * t14)
      t109 = t13 * t47
      t113 = log(-0.4D1 * t104 * t109 * x4)
      t122 = t104 * t13
      t124 = log(0.4D1 * t122)
      t128 = log(-0.4D1 * t104 * t109)
      t133 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t135 = t128 ** 2
      t139 = t124 ** 2
      t152 = log(0.4D1 * t41)
      t160 = t152 ** 2
      t179 = -(0.90D2 * t5 * (t6 - t17 * t18) - 0.180D3 * t23 * t18) * t
     #27 * t29 / 0.2880D4 + t5 * t33 * t37 / 0.32D2 + (0.90D2 * t5 * (-t
     #6 + t44 * t18 + t46 - t51 * t32) - t57) * t27 * t35 / 0.2880D4 - (
     #-0.180D3 * t23 * (t6 - t64 * t18) + 0.90D2 * t5 * (t69 - t64 * t6 
     #+ t71 * t18 / 0.2D1) + t83) * t27 / 0.2880D4 + (-0.180D3 * t23 * (
     #-t6 + t89 * t18) + 0.90D2 * t5 * (-t69 + t89 * t6 - t95 * t18 / 0.
     #2D1) - t83) * t29 / 0.5760D4 + (0.90D2 * t5 * (-t6 + t107 * t18 + 
     #t46 - t113 * t32) - t57) * t35 * t29 / 0.5760D4 - (-0.180D3 * t23 
     #* (t6 - t124 * t18 - t46 + t128 * t32) + 0.90D2 * t5 * (-t133 + t1
     #28 * t46 - t135 * t32 / 0.2D1 + t69 - t124 * t6 + t139 * t18 / 0.2
     #D1) - t82 * t33) * t35 / 0.5760D4 - (-0.180D3 * lh - 0.90D2 * t152
     #) * t5 * t69 / 0.5760D4 - (t78 - t80 + 0.180D3 * t152 * lh + 0.45D
     #2 * t160) * t5 * t6 / 0.5760D4 - (-0.2884936567583026D3 - 0.120D3 
     #* t77 * lh + 0.60D2 * lh * t79 - t152 * t81 - 0.90D2 * t160 * lh -
     # 0.15D2 * t160 * t152) * t5 * t18 / 0.5760D4
      t180 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t179)
      t183 = -0.1D1 + x4
      t185 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t186 = t14 * t183
      t189 = log(-0.4D1 * t11 * t186)
      t190 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t201 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t202 = t190 - t201
      t206 = x4 * t183
      t209 = log(-0.4D1 * t41 * t206)
      t214 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t216 = t209 ** 2
      t226 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t230 = log(0.4D1 * t122 * t206 * t47)
      t234 = log(-0.4D1 * t104 * t186)
      t245 = -(0.90D2 * t5 * (-t185 + t189 * t190) + 0.180D3 * t23 * t19
     #0) * t27 * t29 / 0.2880D4 + t5 * t202 * t37 / 0.32D2 + (-0.180D3 *
     # t23 * (t185 - t209 * t190) + 0.90D2 * t5 * (t214 - t209 * t185 + 
     #t216 * t190 / 0.2D1) + t82 * t190) * t29 / 0.5760D4 + (0.90D2 * t5
     # * (-t226 + t230 * t201 + t185 - t234 * t190) - 0.180D3 * t23 * t2
     #02) * t35 * t29 / 0.5760D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t183, 0.0D0,
     # t245)
      t249 = -0.1D1 + x1
      t251 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t252 = t249 ** 2
      t253 = t7 * t252
      t257 = log(0.4D1 * t41 * t253 * x4)
      t258 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t264 = 0.180D3 * t23 * t258
      t276 = log(0.4D1 * t104 * t13 * t7 * t252)
      t287 = log(0.4D1 * t41 * t253)
      t292 = bbggh21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t294 = t287 ** 2
      t304 = -(0.90D2 * t5 * (-t251 + t257 * t258) + t264) * t27 * t29 /
     # 0.2880D4 + t5 * t258 * t37 / 0.32D2 + (0.90D2 * t5 * (t251 - t276
     # * t258) - t264) * t27 * t35 / 0.2880D4 - (0.180D3 * t23 * (t251 -
     # t287 * t258) - 0.90D2 * t5 * (t292 - t287 * t251 + t294 * t258 / 
     #0.2D1) - t82 * t258) * t27 / 0.2880D4
      t305 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t249, 0.0D0,
     # t304)
      t307 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t308 = s * t307
      t309 = t1 * x1
      t311 = t1 * t249
      t312 = t311 * x4
      t314 = t311 * t183
      t316 = t307 ** 2
      t318 = t1 ** 2
      t320 = x1 * t249
      t324 = 0.1D1 / (-0.2D1 + t307)
      t325 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t327 = t316 ** 2
      t332 = log(-0.4D1 * t62 * t206 * t327 * t252)
      t334 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t350 = -(-0.90D2 * t5 * (t324 * t325 - t332 * t324 * t334) + 0.180
     #D3 * t23 * t324 * t334) * t27 * t29 / 0.2880D4 + t5 * t324 * t334 
     #* t37 / 0.32D2
      t351 = FJET(XB1, XB2, s, 0.0D0, t308 * t309, -t308 * t312, t308 * 
     #t314, -s * t316 * t318 * t320 * x4, t350)
      t353 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t354 = s * t353
      t355 = t309 * x3
      t357 = t309 * t47
      t360 = t353 ** 2
      t366 = 0.1D1 / (-0.2D1 + t353)
      t368 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t372 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t374 = t360 ** 2
      t379 = log(-0.4D1 * t122 * t253 * t47 * t374)
      t392 = t5 * t366 * t368 * t37 / 0.32D2 + (0.90D2 * t5 * (t366 * t3
     #72 - t379 * t366 * t368) - 0.180D3 * t23 * t366 * t368) * t27 * t3
     #5 / 0.2880D4
      t393 = FJET(XB1, XB2, s, t354 * t355, -t354 * t357, 0.0D0, -t354 *
     # t311, -s * t360 * t318 * t320 * x3, t392)
      t395 = KAPPA2(x1, x2, x3, x4, z)
      t396 = s * t395
      t401 = t395 ** 2
      t406 = cos(t8)
      t409 = sqrt(x3 * t47 * t206)
      t416 = 0.1D1 / (-0.2D1 + t395)
      t418 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t422 = FJET(XB1, XB2, s, t396 * t355, -t396 * t357, -t396 * t312, 
     #t396 * t314, s * t401 * t318 * t320 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t406 * t409), -t5 * t416 * t418 * t37 / 0.32D2)
      bbggh2n8e0 = t180 * t179 + t246 * t245 + t305 * t304 + t351 * t350
     # + t393 * t392 - t422 * t5 * t416 * t418 * t27 * t35 * t29 / 0.32D
     #2

      end function



      doubleprecision function bbggh2n8em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t7 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t8 = -t6 + t7
      t9 = t5 * t8
      t10 = 0.1D1 / x3
      t11 = 0.1D1 / x4
      t12 = t10 * t11
      t15 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t17 = sin(x2 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t19 = x3 * t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t19 * t21)
      t26 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t27 = -0.1D1 + x3
      t31 = log(-0.4D1 * t19 * t21 * t27)
      t36 = lh * t5
      t43 = bbggh21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t47 = t18 * t21
      t49 = log(0.4D1 * t47)
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t61 = t49 ** 2
      t67 = x1 ** 2
      t71 = log(0.4D1 * t67 * t18 * t21)
      t77 = 0.180D3 * t36 * t6
      t79 = 0.1D1 / x1
      t83 = t79 * t11
      t86 = t79 * t10
      t91 = log(0.4D1 * t47 * x4)
      t99 = t9 * t12 / 0.64D2 - (0.90D2 * t5 * (t15 - t24 * t6 - t26 + t
     #31 * t7) + 0.180D3 * t36 * t8) * t10 / 0.5760D4 - t5 * t43 / 0.64D
     #2 - (-0.180D3 * lh - 0.90D2 * t49) * t5 * t15 / 0.5760D4 - (0.180D
     #3 * t55 - 0.30D2 * t57 + 0.180D3 * t49 * lh + 0.45D2 * t61) * t5 *
     # t6 / 0.5760D4 - (0.90D2 * t5 * (t15 - t71 * t6) - t77) * t79 / 0.
     #2880D4 - t5 * t6 * t83 / 0.32D2 + t9 * t86 / 0.32D2 + (0.90D2 * t5
     # * (-t15 + t91 * t6) + t77) * t11 / 0.5760D4
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t99)
      t103 = -0.1D1 + x4
      t105 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t109 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t113 = log(-0.4D1 * t47 * x4 * t103)
      t123 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t128 = t5 * t105 * t83 / 0.32D2 + (0.90D2 * t5 * (t109 - t113 * t1
     #05) - 0.180D3 * t36 * t105) * t11 / 0.5760D4 + t5 * (t105 - t123) 
     #* t12 / 0.64D2
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t103, 0.0D0,
     # t128)
      t132 = -0.1D1 + x1
      t134 = bbggh21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t135 = t132 ** 2
      t139 = log(0.4D1 * t47 * t67 * t135)
      t140 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t150 = t5 * t140
      t155 = -(-0.90D2 * t5 * (t134 - t139 * t140) + 0.180D3 * t36 * t14
     #0) * t79 / 0.2880D4 + t150 * t83 / 0.32D2 + t150 * t86 / 0.32D2
      t156 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t132, 0.0D0,
     # t155)
      t158 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t159 = s * t158
      t160 = t1 * x1
      t162 = t1 * t132
      t167 = t158 ** 2
      t169 = t1 ** 2
      t171 = x1 * t132
      t175 = 0.1D1 / (-0.2D1 + t158)
      t177 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t179 = t177 * t79 * t11
      t182 = FJET(XB1, XB2, s, 0.0D0, t159 * t160, -t159 * t162 * x4, t1
     #59 * t162 * t103, -s * t167 * t169 * t171 * x4, t5 * t175 * t179 /
     # 0.32D2)
      t187 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t188 = s * t187
      t194 = t187 ** 2
      t200 = 0.1D1 / (-0.2D1 + t187)
      t202 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t204 = t202 * t79 * t10
      t207 = FJET(XB1, XB2, s, t188 * t160 * x3, -t188 * t160 * t27, 0.0
     #D0, -t188 * t162, -s * t194 * t169 * t171 * x3, t5 * t200 * t204 /
     # 0.32D2)
      bbggh2n8em1 = t100 * t99 + t129 * t128 + t156 * t155 + t182 * t5 *
     # t175 * t179 / 0.32D2 + t207 * t5 * t200 * t204 / 0.32D2

      end function



      doubleprecision function bbggh2n8em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t7 = t5 * t6
      t8 = 0.1D1 / x1
      t11 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, 0.0D0)
      t17 = bbggh21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t34 = 0.1D1 / x4
      t37 = -t7 * t8 / 0.32D2 - t5 * (t6 - t11) / x3 / 0.64D2 - t5 * t17
     # / 0.64D2 - (-0.180D3 * lh - 0.90D2 * t28) * t5 * t6 / 0.5760D4 - 
     #t7 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t43 = bbggh21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t47 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, t5 * t43 * t8 / 0.32D2)
      t55 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t5 * t55 * t34 / 0.64D2)
      bbggh2n8em2 = t38 * t37 + t47 * t5 * t43 * t8 / 0.32D2 + t59 * t5 
     #* t55 * t34 / 0.64D2

      end function



      doubleprecision function bbggh2n8em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = bbggh21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, 0.0D0)
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D
     #0, -t5 * t6 / 0.64D2)
      bbggh2n8em3 = -t9 * t5 * t6 / 0.64D2

      end function



      doubleprecision function bbggh2n8em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh21J1
      doubleprecision bbggh21J2
      doubleprecision bbggh21J3
      bbggh2n8em4 = 0.0D0

      end function
  
 

      doubleprecision function bbggh21J1
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
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = t2 * t5 * t3 * t10 * t8
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t15 = t14 * t13
      t16 = 0.1D1 - x4
      t17 = t15 * t16
      t18 = x1 ** 2
      t19 = x3 * x4
      t22 = cos(x2 * 0.3141592653589793D1)
      t23 = 0.1D1 - x3
      t24 = x3 * t23
      t25 = x4 * t16
      t27 = sqrt(t24 * t25)
      t30 = x3 + x4 - 0.2D1 * t19 - 0.2D1 * t22 * t27
      t31 = t30 ** 2
      t32 = t18 * t31
      t36 = t2 * t5
      t37 = t36 * t10
      t38 = x4 ** 2
      t40 = x1 * t30
      t45 = t2 * t4 * t3
      t46 = t9 * t8
      t47 = t45 * t46
      t48 = t14 * t38
      t49 = x1 * x3
      t53 = t13 * t16
      t54 = x3 ** 2
      t55 = t18 * t54
      t59 = t18 * x1
      t60 = t59 * x3
      t61 = t14 * t31
      t65 = t13 * x4
      t69 = t2 * z
      t75 = x1 * t23
      t79 = t2 * t4
      t80 = t79 * t9
      t82 = x4 * z
      t90 = t23 ** 2
      t91 = t18 * t90
      t98 = t16 ** 2
      t99 = t14 * t98
      t104 = t23 * z
      t108 = 0.3D1 * t12 * t17 * t32 - 0.3D1 * t37 * t15 * t38 * t40 + 0
     #.3D1 * t47 * t48 * t49 + 0.21D2 * t47 * t53 * t55 + 0.3D1 * t12 * 
     #t60 * t61 + 0.3D1 * t47 * t65 * t55 + 0.3D1 * t69 * t5 * t10 * t18
     # * t61 + 0.21D2 * t47 * t48 * t75 + 0.12D2 * t80 * t14 * t16 * t82
     # + 0.3D1 * t12 * t15 * x4 * t32 + 0.3D1 * t47 * t53 * t91 + 0.21D2
     # * t47 * t65 * t91 + 0.3D1 * t47 * t99 * t75 + 0.12D2 * t80 * t18 
     #* x3 * t104
      t110 = t13 * t30
      t118 = z ** 2
      t127 = t59 * t54
      t131 = t15 * t98
      t136 = t36 * t10 * t14
      t137 = x4 * t18
      t138 = x3 * t30
      t142 = t23 * t30
      t153 = t16 * t18
      t158 = t79 * t9 * t13
      t159 = t16 * x1
      t164 = t45 * t46 * t18
      t171 = t45 * t46 * t14
      t178 = -0.3D1 * t37 * t59 * t90 * t110 + 0.21D2 * t47 * t99 * t49 
     #- 0.4D1 * t80 * x1 * t13 * t30 * t118 + 0.3D1 * t12 * t59 * t23 * 
     #t61 - 0.3D1 * t37 * t127 * t110 - 0.3D1 * t37 * t131 * t40 - 0.6D1
     # * t136 * t137 * t138 - 0.15D2 * t136 * t137 * t142 - 0.6D1 * t36 
     #* t10 * t15 * t25 * t40 + 0.14D2 * t45 * t46 * t13 * t153 * t24 + 
     #0.6D1 * t158 * t159 * t104 + 0.14D2 * t164 * x3 * t13 * x4 * t23 +
     # 0.24D2 * t171 * t25 * t75 + 0.24D2 * t171 * t159 * t19
      t185 = x4 * x1
      t186 = x3 * z
      t207 = t69 * t4
      t208 = t9 * t14
      t223 = t2 * t3 * t8
      t227 = t46 * t59
      t234 = t46 * t15
      t238 = -0.6D1 * t36 * t10 * t59 * t24 * t110 + 0.6D1 * t158 * t185
     # * t186 + 0.16D2 * t158 * t159 * t186 + 0.2D1 * t2 * t118 * z + 0.
     #6D1 * t47 * t127 * t23 - t2 * t5 * t4 * t10 * t9 * t59 * t15 * t31
     # * t30 + 0.3D1 * t207 * t208 * t98 - 0.15D2 * t136 * t153 * t138 -
     # 0.6D1 * t164 * t104 * t110 - 0.6D1 * t171 * t16 * z * t40 + 0.4D1
     # * t223 * t53 * t118 + t45 * t227 * t54 * x3 + t45 * t227 * t90 * 
     #t23 + t45 * t234 * t38 * x4
      t260 = t9 * t18
      t282 = t45 * t234 * t98 * t16 + 0.3D1 * t207 * t208 * t38 + 0.4D1 
     #* t223 * t49 * t118 + 0.4D1 * t223 * t75 * t118 + 0.6D1 * t47 * t1
     #31 * x4 + 0.6D1 * t47 * t60 * t90 + 0.4D1 * t223 * t65 * t118 + 0.
     #3D1 * t207 * t260 * t54 + 0.3D1 * t207 * t260 * t90 - 0.6D1 * t136
     # * t153 * t142 - 0.6D1 * t164 * t186 * t110 + 0.16D2 * t158 * t185
     # * t104 - 0.6D1 * t171 * t82 * t40 + 0.6D1 * t47 * t17 * t38
      bbggh21J1 = -0.16D2 / 0.3D1 * wd * (t108 + t178 + t238 + t282)

      end function
  
   
 

      doubleprecision function bbggh21J2
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
      t6 = t2 * t4 * t3
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t12 = t9 * t11
      t13 = x3 ** 2
      t17 = 0.1D1 - x1
      t18 = t17 ** 2
      t19 = t18 * t17
      t20 = t9 * t19
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t26 = x4 ** 2
      t30 = t4 ** 2
      t31 = t2 * t30
      t32 = t8 ** 2
      t33 = t31 * t32
      t35 = x3 * x4
      t38 = cos(x2 * 0.3141592653589793D1)
      t39 = 0.1D1 - x3
      t40 = x3 * t39
      t41 = x4 * t21
      t43 = sqrt(t40 * t41)
      t46 = x3 + x4 - 0.2D1 * t35 - 0.2D1 * t38 * t43
      t47 = x1 * t46
      t54 = t2 * t30 * t3 * t32 * t7
      t55 = t19 * t21
      t56 = t46 ** 2
      t57 = t10 * t56
      t61 = t11 * x3
      t62 = t18 * t56
      t66 = t6 * t9
      t67 = t18 * t26
      t68 = x1 * x3
      t72 = t17 * t21
      t73 = t10 * t13
      t77 = t19 * t22
      t81 = t2 * z
      t86 = x1 * t39
      t90 = t17 * x4
      t94 = -t6 * t12 * t13 * x3 - t6 * t20 * t22 * t21 - t6 * t20 * t26
     # * x4 + 0.3D1 * t33 * t19 * t26 * t47 - 0.3D1 * t54 * t55 * t57 - 
     #0.3D1 * t54 * t61 * t62 - 0.2D1 * t66 * t67 * t68 - 0.22D2 * t66 *
     # t72 * t73 + 0.3D1 * t33 * t77 * t47 - t81 * t30 * t32 * t10 * t62
     # - 0.22D2 * t66 * t67 * t86 - 0.2D1 * t66 * t90 * t73
      t99 = t2 * t4
      t100 = t99 * t8
      t102 = x4 * z
      t107 = t39 * z
      t111 = t39 ** 2
      t112 = t10 * t111
      t119 = t18 * t22
      t127 = t17 * t46
      t135 = t11 * t13
      t145 = t6 * t9 * t10
      t152 = t6 * t9 * t18
      t156 = -0.3D1 * t54 * t19 * x4 * t57 - 0.4D1 * t100 * t18 * t21 * 
     #t102 - 0.4D1 * t100 * t10 * x3 * t107 - 0.2D1 * t66 * t72 * t112 -
     # 0.22D2 * t66 * t90 * t112 - 0.2D1 * t66 * t119 * t86 - 0.22D2 * t
     #66 * t119 * t68 + 0.3D1 * t33 * t11 * t111 * t127 - 0.3D1 * t54 * 
     #t11 * t39 * t62 + 0.3D1 * t33 * t135 * t127 + 0.6D1 * t31 * t32 * 
     #t19 * t41 * t47 - 0.14D2 * t145 * x3 * t17 * x4 * t39 - 0.24D2 * t
     #152 * t41 * t86
      t158 = t21 * x1
      t163 = t31 * t32 * t18
      t164 = x4 * t10
      t165 = t39 * t46
      t178 = t99 * t8 * t17
      t179 = x4 * x1
      t180 = x3 * z
      t195 = t21 * t10
      t199 = x3 * t46
      t206 = -0.24D2 * t152 * t158 * t35 + 0.16D2 * t163 * t164 * t165 -
     # t6 * t12 * t111 * t39 + 0.6D1 * t31 * t32 * t11 * t40 * t127 - t1
     #78 * t179 * t180 - 0.11D2 * t178 * t158 * t180 + 0.2D1 * t152 * t1
     #02 * t47 + 0.2D1 * t145 * t180 * t127 - 0.11D2 * t178 * t179 * t10
     #7 + 0.5D1 * t163 * t195 * t165 + 0.16D2 * t163 * t195 * t199 + 0.2
     #D1 * t145 * t107 * t127
      t207 = t81 * t4
      t208 = t8 * t18
      t230 = t8 * t10
      t252 = -t207 * t208 * t22 - t207 * t208 * t26 + 0.2D1 * t152 * t21
     # * z * t47 + 0.5D1 * t163 * t164 * t199 - 0.14D2 * t6 * t9 * t17 *
     # t195 * t40 - t178 * t158 * t107 - 0.6D1 * t66 * t55 * t26 - t207 
     #* t230 * t111 - t207 * t230 * t13 - 0.6D1 * t66 * t61 * t111 - 0.6
     #D1 * t66 * t77 * x4 - 0.6D1 * t66 * t135 * t39 + t2 * t30 * t4 * t
     #32 * t8 * t11 * t19 * t56 * t46
      bbggh21J2 = -0.16D2 / 0.3D1 * wd * (t94 + t156 + t206 + t252)

      end function
  
   
 

      doubleprecision function bbggh21J3
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
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t13 = t6 * t9 * t11
      t14 = 0.1D1 - x4
      t15 = x1 ** 2
      t16 = t14 * t15
      t17 = x3 * x4
      t20 = cos(x2 * 0.3141592653589793D1)
      t21 = 0.1D1 - x3
      t23 = x4 * t14
      t25 = sqrt(x3 * t21 * t23)
      t28 = x3 + x4 - 0.2D1 * t17 - 0.2D1 * t20 * t25
      t29 = x3 * t28
      t34 = t2 * t4 * t3
      t35 = t7 * t8
      t37 = t34 * t35 * t15
      t38 = t21 * z
      t39 = t10 * t28
      t43 = t34 * t35 * t11
      t45 = x1 * t28
      t52 = t15 * x1
      t53 = t10 * t11
      t55 = t28 ** 2
      t59 = t21 * t28
      t63 = t6 * t9
      t64 = x4 ** 2
      t71 = t2 * t5 * t3 * t9 * t7
      t73 = t11 * t55
      t78 = t15 * t55
      t82 = t34 * t35
      t83 = t10 * t14
      t84 = x3 ** 2
      t85 = t15 * t84
      t89 = t14 ** 2
      t93 = x4 * t10
      t96 = t11 * t64
      t97 = x1 * x3
      t105 = t21 * x1
      t109 = t11 * t89
      t116 = 0.11D2 * t13 * t16 * t29 + t37 * t38 * t39 + t43 * t14 * z 
     #* t45 + t2 * t5 * t4 * t9 * t8 * t52 * t53 * t55 * t28 + 0.3D1 * t
     #13 * t16 * t59 + t63 * t53 * t64 * t45 - 0.2D1 * t71 * t52 * x3 * 
     #t73 - 0.2D1 * t71 * t53 * t14 * t78 - 0.9D1 * t82 * t83 * t85 + t6
     #3 * t53 * t89 * t45 - t82 * t93 * t85 - t82 * t96 * t97 - t2 * z *
     # t5 * t9 * t15 * t73 - 0.9D1 * t82 * t96 * t105 - t82 * t109 * t10
     #5 - 0.2D1 * t71 * t53 * x4 * t78
      t117 = t21 ** 2
      t118 = t15 * t117
      t137 = x4 * t15
      t143 = t2 * t4 * t8 * t10
      t144 = t14 * x1
      t156 = x4 * x1
      t157 = x3 * z
      t171 = -0.9D1 * t82 * t93 * t118 - t82 * t83 * t118 + t63 * t52 * 
     #t117 * t39 - 0.9D1 * t82 * t109 * t97 + t63 * t52 * t84 * t39 - 0.
     #2D1 * t71 * t52 * t21 * t73 + 0.3D1 * t13 * t137 * t29 - t143 * t1
     #44 * t38 + 0.11D2 * t13 * t137 * t59 - 0.9D1 * t43 * t23 * t105 - 
     #0.9D1 * t43 * t144 * t17 - t143 * t156 * t157 - 0.8D1 * t143 * t14
     #4 * t157 + t43 * x4 * z * t45 + t37 * t157 * t39 - 0.8D1 * t143 * 
     #t156 * t38
      bbggh21J3 = -0.16D2 / 0.3D1 * wd * (t116 + t171)

      end function
  
 