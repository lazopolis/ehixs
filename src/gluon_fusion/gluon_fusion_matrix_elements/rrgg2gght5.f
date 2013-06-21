  
      subroutine rrgg2gght5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh51J1  
      doubleprecision rrgg2ggh51J2  
      doubleprecision rrgg2ggh51J3  
      doubleprecision rrgg2ggh51J4  
      doubleprecision rrgg2ggh51J5  
      doubleprecision rrgg2ggh51J6  
      doubleprecision rrgg2gght5s1e1  
      doubleprecision rrgg2gght5s1e0  
      doubleprecision rrgg2gght5s1em1  
      doubleprecision rrgg2gght5s1em2  
      doubleprecision rrgg2gght5s1em3  
      doubleprecision rrgg2gght5s1em4  
      doubleprecision rrgg2gght5s2e1  
      doubleprecision rrgg2gght5s2e0  
      doubleprecision rrgg2gght5s2em1  
      doubleprecision rrgg2gght5s2em2  
      doubleprecision rrgg2gght5s2em3  
      doubleprecision rrgg2gght5s2em4  
      doubleprecision rrgg2gght5s3e1  
      doubleprecision rrgg2gght5s3e0  
      doubleprecision rrgg2gght5s3em1  
      doubleprecision rrgg2gght5s3em2  
      doubleprecision rrgg2gght5s3em3  
      doubleprecision rrgg2gght5s3em4  
      doubleprecision rrgg2gght5s4e1  
      doubleprecision rrgg2gght5s4e0  
      doubleprecision rrgg2gght5s4em1  
      doubleprecision rrgg2gght5s4em2  
      doubleprecision rrgg2gght5s4em3  
      doubleprecision rrgg2gght5s4em4  
      doubleprecision rrgg2gght5s5e1  
      doubleprecision rrgg2gght5s5e0  
      doubleprecision rrgg2gght5s5em1  
      doubleprecision rrgg2gght5s5em2  
      doubleprecision rrgg2gght5s5em3  
      doubleprecision rrgg2gght5s5em4  
      doubleprecision rrgg2gght5s6e1  
      doubleprecision rrgg2gght5s6e0  
      doubleprecision rrgg2gght5s6em1  
      doubleprecision rrgg2gght5s6em2  
      doubleprecision rrgg2gght5s6em3  
      doubleprecision rrgg2gght5s6em4  
      doubleprecision rrgg2gght5s7e1  
      doubleprecision rrgg2gght5s7e0  
      doubleprecision rrgg2gght5s7em1  
      doubleprecision rrgg2gght5s7em2  
      doubleprecision rrgg2gght5s7em3  
      doubleprecision rrgg2gght5s7em4  
      doubleprecision rrgg2gght5s8e1  
      doubleprecision rrgg2gght5s8e0  
      doubleprecision rrgg2gght5s8em1  
      doubleprecision rrgg2gght5s8em2  
      doubleprecision rrgg2gght5s8em3  
      doubleprecision rrgg2gght5s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght5s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = t18 * 0.3141592653589793D1
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t84 = -0.1D1 + x4
      t85 = t83 * t84
      t88 = log(-0.4D1 * t82 * t85)
      t89 = -t84
      t90 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t89)
      t92 = t88 ** 2
      t93 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t89)
      t96 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t89)
      t99 = log(0.4D1 * t82 * t83)
      t101 = t99 ** 2
      t113 = -t78 + t93
      t115 = t39 * t34 * t113
      t117 = 0.1D1 / x1
      t119 = 0.1D1 / x4
      t122 = t82 * t15
      t124 = log(0.4D1 * t122)
      t129 = t124 ** 2
      t139 = t34 * t78
      t140 = t10 * t139
      t151 = x3 * t81
      t152 = t151 * t13
      t155 = log(-0.4D1 * t152 * t85)
      t157 = t16 * x4
      t160 = log(0.4D1 * t151 * t157)
      t166 = -t34 * t113
      t170 = 0.1D1 / x3
      t172 = t117 * t119
      t175 = t151 * t16
      t177 = log(0.4D1 * t175)
      t179 = t177 ** 2
      t196 = log(0.4D1 * t157)
      t198 = x4 * t84
      t201 = log(-0.4D1 * t16 * t198)
      t206 = t201 ** 2
      t209 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t89)
      t214 = t196 ** 2
      t238 = x3 * t13
      t241 = log(-0.4D1 * t238 * t85)
      t243 = t241 ** 2
      t248 = log(0.4D1 * t238 * t83)
      t250 = t248 ** 2
      t266 = t238 * t15
      t268 = log(0.4D1 * t266)
      t273 = t268 ** 2
      t293 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t56 * 
     #(-t88 * t90 + t92 * t93 / 0.2D1 + t96 + t99 * t36 - t101 * t78 / 0
     #.2D1 - t45) - 0.180D3 * t48 * t34 * (t90 - t88 * t93 - t36 + t99 *
     # t78) + t115) * t117 * t119 / 0.720D3 - (t39 * t34 * (t36 - t124 *
     # t78) + 0.90D2 * t56 * (t129 * t36 / 0.2D1 + t53 - t129 * t124 * t
     #78 / 0.6D1 - t124 * t45) + t140 - 0.180D3 * t48 * t34 * (-t124 * t
     #36 + t129 * t78 / 0.2D1 + t45)) * t117 / 0.720D3 - (0.90D2 * t56 *
     # (-t90 + t155 * t93 + t36 - t160 * t78) - 0.180D3 * t48 * t166) * 
     #t170 * t172 / 0.720D3 - (0.90D2 * t56 * (-t177 * t36 + t179 * t78 
     #/ 0.2D1 + t45) - 0.180D3 * t48 * t34 * (t36 - t177 * t78) + t39 * 
     #t139) * t170 * t117 / 0.720D3 - (t39 * t34 * (t36 - t196 * t78 - t
     #90 + t201 * t93) + 0.90D2 * t56 * (-t206 * t90 / 0.2D1 - t209 + t2
     #06 * t201 * t93 / 0.6D1 + t201 * t96 + t214 * t36 / 0.2D1 + t53 - 
     #t214 * t196 * t78 / 0.6D1 - t196 * t45) + t10 * t166 - 0.180D3 * t
     #48 * t34 * (-t196 * t36 + t214 * t78 / 0.2D1 + t45 + t201 * t90 - 
     #t206 * t93 / 0.2D1 - t96)) * t119 / 0.1440D4 + (0.90D2 * t56 * (-t
     #241 * t90 + t243 * t93 / 0.2D1 + t96 + t248 * t36 - t250 * t78 / 0
     #.2D1 - t45) - 0.180D3 * t48 * t34 * (t90 - t241 * t93 - t36 + t248
     # * t78) + t115) * t170 * t119 / 0.1440D4 + (t39 * t34 * (-t36 + t2
     #68 * t78) + 0.90D2 * t56 * (-t273 * t36 / 0.2D1 - t53 + t273 * t26
     #8 * t78 / 0.6D1 + t268 * t45) - t140 - 0.180D3 * t48 * t34 * (t268
     # * t36 - t273 * t78 / 0.2D1 - t45)) * t170 / 0.1440D4
      t294 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t293)
      t296 = 0.1D1 - x1
      t297 = 0.1D1 - x3
      t298 = KAPPA2(t296, x2, t297, 0.10D1, z)
      t299 = s * t298
      t300 = -t296
      t301 = t1 * t300
      t302 = -t297
      t303 = t301 * t302
      t305 = t301 * x3
      t307 = t1 * x1
      t309 = t298 ** 2
      t311 = t1 ** 2
      t313 = t300 * x1
      t317 = 0.1D1 / (-0.2D1 + t298)
      t318 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, 0.
     #10D1)
      t319 = t317 * t318
      t320 = t300 ** 2
      t321 = t320 * t302
      t322 = t309 ** 2
      t327 = log(-0.4D1 * t175 * t321 * x4 * t322)
      t329 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, 0.
     #10D1)
      t335 = t34 * t317 * t329
      t346 = log(-0.4D1 * t152 * t15 * t320 * t302 * t322)
      t347 = t346 * t317
      t349 = t346 ** 2
      t353 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, 0.
     #10D1)
      t368 = -(0.90D2 * t56 * (-t319 + t327 * t317 * t329) + 0.180D3 * t
     #48 * t335) * t170 * t172 / 0.720D3 - (0.90D2 * t56 * (t347 * t318 
     #- t349 * t317 * t329 / 0.2D1 - t317 * t353) - 0.180D3 * t48 * t34 
     #* (-t319 + t347 * t329) - t39 * t335) * t170 * t117 / 0.720D3
      t369 = FJET(XB1, XB2, s, t299 * t303, -t299 * t305, t299 * t307, 0
     #.0D0, -s * t309 * t311 * t313 * x3, t368)
      t371 = KAPPA2(t296, x2, t297, t89, z)
      t372 = s * t371
      t375 = t307 * t84
      t377 = t307 * x4
      t379 = t371 ** 2
      t384 = cos(t11)
      t387 = Sqrt(x3 * t302 * t198)
      t394 = 0.1D1 / (-0.2D1 + t371)
      t395 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, t8
     #9)
      t397 = t379 ** 2
      t402 = log(0.4D1 * t175 * t321 * t198 * t397)
      t404 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, t8
     #9)
      t413 = 0.90D2 * t56 * (t394 * t395 - t402 * t394 * t404) - 0.180D3
     # * t48 * t34 * t394 * t404
      t417 = FJET(XB1, XB2, s, t372 * t303, -t372 * t305, -t372 * t375, 
     #t372 * t377, s * t379 * t311 * t313 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t384 * t387), -t413 * t170 * t172 / 0.720D3)
      t425 = t81 * t320
      t426 = t425 * x4
      t429 = log(0.4D1 * t16 * t426)
      t430 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.10D1)
      t432 = t429 ** 2
      t433 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.10D1)
      t436 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.10D1)
      t445 = t34 * t433
      t446 = t39 * t445
      t452 = log(0.4D1 * t16 * t425)
      t457 = t452 ** 2
      t460 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.10D1)
      t480 = log(0.4D1 * t266 * t426)
      t494 = log(0.4D1 * t238 * t15 * t81 * t320)
      t496 = t494 ** 2
      t511 = (0.90D2 * t56 * (-t429 * t430 + t432 * t433 / 0.2D1 + t436)
     # - 0.180D3 * t48 * t34 * (t430 - t429 * t433) + t446) * t117 * t11
     #9 / 0.720D3 - (-t39 * t34 * (t430 - t452 * t433) - 0.90D2 * t56 * 
     #(t457 * t430 / 0.2D1 + t460 - t457 * t452 * t433 / 0.6D1 - t452 * 
     #t436) - t10 * t445 + 0.180D3 * t48 * t34 * (-t452 * t430 + t457 * 
     #t433 / 0.2D1 + t436)) * t117 / 0.720D3 - (0.90D2 * t56 * (-t430 + 
     #t480 * t433) + 0.180D3 * t48 * t445) * t170 * t172 / 0.720D3 - (0.
     #90D2 * t56 * (t494 * t430 - t496 * t433 / 0.2D1 - t436) - 0.180D3 
     #* t48 * t34 * (-t430 + t494 * t433) - t446) * t170 * t117 / 0.720D
     #3
      t512 = FJET(XB1, XB2, s, -t2 * t300, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t511)
      t519 = log(0.4D1 * t266 * t198 * t302)
      t520 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #t89)
      t522 = t519 ** 2
      t523 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #t89)
      t526 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #t89)
      t527 = t15 * t302
      t528 = t527 * x4
      t531 = log(-0.4D1 * t238 * t528)
      t532 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.10D1)
      t534 = t531 ** 2
      t535 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.10D1)
      t538 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.10D1)
      t548 = -t523 + t535
      t557 = log(-0.4D1 * t238 * t527)
      t562 = t557 ** 2
      t565 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.10D1)
      t573 = t34 * t535
      t587 = log(-0.4D1 * t152 * t528)
      t593 = log(0.4D1 * t152 * t83 * t84 * t302)
      t609 = log(-0.4D1 * t151 * t16 * t302)
      t611 = t609 ** 2
      t627 = (0.90D2 * t56 * (t519 * t520 - t522 * t523 / 0.2D1 - t526 -
     # t531 * t532 + t534 * t535 / 0.2D1 + t538) - 0.180D3 * t48 * t34 *
     # (-t520 + t519 * t523 + t532 - t531 * t535) + t39 * t34 * t548) * 
     #t170 * t119 / 0.1440D4 + (t39 * t34 * (t532 - t557 * t535) + 0.90D
     #2 * t56 * (t562 * t532 / 0.2D1 + t565 - t562 * t557 * t535 / 0.6D1
     # - t557 * t538) + t10 * t573 - 0.180D3 * t48 * t34 * (-t557 * t532
     # + t562 * t535 / 0.2D1 + t538)) * t170 / 0.1440D4 - (0.90D2 * t56 
     #* (-t532 + t587 * t535 + t520 - t593 * t523) + 0.180D3 * t48 * t34
     # * t548) * t170 * t172 / 0.720D3 - (-0.90D2 * t56 * (-t609 * t532 
     #+ t611 * t535 / 0.2D1 + t538) + 0.180D3 * t48 * t34 * (t532 - t609
     # * t535) - t39 * t573) * t170 * t117 / 0.720D3
      t628 = FJET(XB1, XB2, s, -t2 * t302, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t627)
      t630 = KAPPA2(t296, x2, 0.10D1, t89, z)
      t631 = s * t630
      t635 = t630 ** 2
      t641 = t635 ** 2
      t643 = t320 * x4 * t84 * t641
      t646 = log(-0.4D1 * t122 * t643)
      t648 = 0.1D1 / (-0.2D1 + t630)
      t649 = t646 * t648
      t650 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #t89)
      t652 = t646 ** 2
      t654 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #t89)
      t657 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #t89)
      t662 = t648 * t650
      t669 = t34 * t648 * t654
      t676 = log(-0.4D1 * t175 * t643)
      t688 = (0.90D2 * t56 * (-t649 * t650 + t652 * t648 * t654 / 0.2D1 
     #+ t648 * t657) - 0.180D3 * t48 * t34 * (t662 - t649 * t654) + t39 
     #* t669) * t117 * t119 / 0.720D3 - (-0.90D2 * t56 * (t662 - t676 * 
     #t648 * t654) + 0.180D3 * t48 * t669) * t170 * t172 / 0.720D3
      t689 = FJET(XB1, XB2, s, -t631 * t301, 0.0D0, -t631 * t375, t631 *
     # t377, -s * t635 * t311 * t313 * x4, t688)
      rrgg2gght5s1e1 = t294 * t293 + t369 * t368 - t417 * t413 * t170 * 
     #t117 * t119 / 0.720D3 + t512 * t511 + t628 * t627 + t689 * t688

      end function



      doubleprecision function rrgg2gght5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t60 = 0.1D1 - x4
      t61 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t60)
      t62 = x3 * t11
      t63 = t13 * x4
      t64 = -t60
      t65 = t63 * t64
      t68 = log(-0.4D1 * t62 * t65)
      t69 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t60)
      t73 = log(0.4D1 * t62 * t63)
      t78 = -t49 + t69
      t81 = 0.180D3 * t52 * t26 * t78
      t83 = 0.1D1 / x3
      t85 = 0.1D1 / x4
      t88 = t62 * t13
      t90 = log(0.4D1 * t88)
      t92 = t90 ** 2
      t103 = t26 * t49
      t104 = t8 * t103
      t110 = log(0.4D1 * t14 * x4)
      t112 = t110 ** 2
      t115 = x4 * t64
      t118 = log(-0.4D1 * t14 * t115)
      t120 = t118 ** 2
      t123 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t60)
      t133 = -t78
      t139 = x1 ** 2
      t140 = t139 * t11
      t143 = log(-0.4D1 * t140 * t65)
      t147 = log(0.4D1 * t140 * t63)
      t153 = 0.1D1 / x1
      t159 = t83 * t153 * t85
      t162 = x3 * t139
      t165 = log(0.4D1 * t162 * t14)
      t176 = t140 * t13
      t178 = log(0.4D1 * t176)
      t180 = t178 ** 2
      t194 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 + (0.90D2 * t31 * (t61 - t68 * t69 - t28 + t73 * t49) - t81
     #) * t83 * t85 / 0.1440D4 + (0.90D2 * t31 * (t90 * t28 - t92 * t49 
     #/ 0.2D1 - t57) - 0.180D3 * t52 * t26 * (-t28 + t90 * t49) - t104) 
     #* t83 / 0.1440D4 - (0.90D2 * t31 * (-t110 * t28 + t112 * t49 / 0.2
     #D1 + t57 + t118 * t61 - t120 * t69 / 0.2D1 - t123) - 0.180D3 * t52
     # * t26 * (t28 - t110 * t49 - t61 + t118 * t69) + t8 * t26 * t133) 
     #* t85 / 0.1440D4 + (0.90D2 * t31 * (t61 - t143 * t69 - t28 + t147 
     #* t49) - t81) * t153 * t85 / 0.720D3 - t31 * t133 * t159 / 0.8D1 -
     # (0.90D2 * t31 * (t28 - t165 * t49) - 0.180D3 * t52 * t103) * t83 
     #* t153 / 0.720D3 - (0.90D2 * t31 * (-t178 * t28 + t180 * t49 / 0.2
     #D1 + t57) - 0.180D3 * t52 * t26 * (t28 - t178 * t49) + t104) * t15
     #3 / 0.720D3
      t195 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t194)
      t197 = 0.1D1 - x1
      t198 = 0.1D1 - x3
      t199 = KAPPA2(t197, x2, t198, 0.10D1, z)
      t200 = s * t199
      t201 = -t197
      t202 = t1 * t201
      t203 = -t198
      t204 = t202 * t203
      t206 = t202 * x3
      t208 = t1 * x1
      t210 = t199 ** 2
      t212 = t1 ** 2
      t214 = t201 * x1
      t218 = 0.1D1 / (-0.2D1 + t199)
      t220 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, t198, 0.
     #10D1)
      t222 = t153 * t85
      t226 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, t198, 0.
     #10D1)
      t229 = t201 ** 2
      t231 = t210 ** 2
      t236 = log(-0.4D1 * t162 * t11 * t13 * t229 * t203 * t231)
      t250 = t31 * t218 * t220 * t83 * t222 / 0.8D1 - (0.90D2 * t31 * (-
     #t218 * t226 + t236 * t218 * t220) + 0.180D3 * t52 * t26 * t218 * t
     #220) * t83 * t153 / 0.720D3
      t251 = FJET(XB1, XB2, s, t200 * t204, -t200 * t206, t200 * t208, 0
     #.0D0, -s * t210 * t212 * t214 * x3, t250)
      t253 = KAPPA2(t197, x2, t198, t60, z)
      t254 = s * t253
      t257 = t208 * t64
      t259 = t208 * x4
      t261 = t253 ** 2
      t266 = cos(t9)
      t269 = Sqrt(x3 * t203 * t115)
      t276 = 0.1D1 / (-0.2D1 + t253)
      t278 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, t198, t6
     #0)
      t280 = t278 * t83 * t222
      t283 = FJET(XB1, XB2, s, t254 * t204, -t254 * t206, -t254 * t257, 
     #t254 * t259, s * t261 * t212 * t214 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t266 * t269), -t31 * t276 * t280 / 0.8D1)
      t291 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #0.10D1)
      t292 = t139 * t229
      t296 = log(0.4D1 * t14 * t292 * x4)
      t297 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #0.10D1)
      t302 = t26 * t297
      t304 = 0.180D3 * t52 * t302
      t316 = log(0.4D1 * t62 * t13 * t139 * t229)
      t327 = log(0.4D1 * t14 * t292)
      t329 = t327 ** 2
      t332 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #0.10D1)
      t345 = (0.90D2 * t31 * (t291 - t296 * t297) - t304) * t153 * t85 /
     # 0.720D3 + t31 * t297 * t159 / 0.8D1 - (0.90D2 * t31 * (-t291 + t3
     #16 * t297) + t304) * t83 * t153 / 0.720D3 - (-0.90D2 * t31 * (-t32
     #7 * t291 + t329 * t297 / 0.2D1 + t332) + 0.180D3 * t52 * t26 * (t2
     #91 - t327 * t297) - t8 * t302) * t153 / 0.720D3
      t346 = FJET(XB1, XB2, s, -t2 * t201, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t345)
      t350 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t198, 
     #0.10D1)
      t351 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t198, 
     #t60)
      t352 = -t350 + t351
      t356 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t198, 
     #0.10D1)
      t360 = log(-0.4D1 * t162 * t14 * t203)
      t365 = t26 * t350
      t372 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t198, 
     #t60)
      t376 = log(0.4D1 * t88 * t115 * t203)
      t378 = t13 * t203
      t382 = log(-0.4D1 * t62 * t378 * x4)
      t397 = log(-0.4D1 * t62 * t378)
      t399 = t397 ** 2
      t402 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t198, 
     #0.10D1)
      t415 = -t31 * t352 * t159 / 0.8D1 - (-0.90D2 * t31 * (t356 - t360 
     #* t350) + 0.180D3 * t52 * t365) * t83 * t153 / 0.720D3 + (0.90D2 *
     # t31 * (-t372 + t376 * t351 + t356 - t382 * t350) + 0.180D3 * t52 
     #* t26 * t352) * t83 * t85 / 0.1440D4 + (0.90D2 * t31 * (-t397 * t3
     #56 + t399 * t350 / 0.2D1 + t402) - 0.180D3 * t52 * t26 * (t356 - t
     #397 * t350) + t8 * t365) * t83 / 0.1440D4
      t416 = FJET(XB1, XB2, s, -t2 * t203, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t415)
      t418 = KAPPA2(t197, x2, 0.10D1, t60, z)
      t419 = s * t418
      t423 = t418 ** 2
      t429 = 0.1D1 / (-0.2D1 + t418)
      t430 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #t60)
      t433 = t423 ** 2
      t438 = log(-0.4D1 * t176 * t229 * x4 * t64 * t433)
      t440 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #t60)
      t458 = (0.90D2 * t31 * (t429 * t430 - t438 * t429 * t440) - 0.180D
     #3 * t52 * t26 * t429 * t440) * t153 * t85 / 0.720D3 + t31 * t429 *
     # t440 * t83 * t222 / 0.8D1
      t459 = FJET(XB1, XB2, s, -t419 * t202, 0.0D0, -t419 * t257, t419 *
     # t259, -s * t423 * t212 * t214 * x4, t458)
      rrgg2gght5s1e0 = t195 * t194 + t251 * t250 - t283 * 0.314159265358
     #9793D1 * t26 * t276 * t280 / 0.8D1 + t346 * t345 + t416 * t415 + t
     #459 * t458

      end function



      doubleprecision function rrgg2gght5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t43 = 0.1D1 - x4
      t44 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t43)
      t45 = -t36 + t44
      t46 = 0.1D1 / x3
      t48 = 0.1D1 / x4
      t52 = x3 * t7
      t55 = log(0.4D1 * t52 * t9)
      t62 = 0.180D3 * t3 * t18 * t36
      t68 = log(0.4D1 * t10 * x4)
      t70 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t43)
      t71 = -t43
      t75 = log(-0.4D1 * t10 * x4 * t71)
      t88 = 0.1D1 / x1
      t92 = x1 ** 2
      t96 = log(0.4D1 * t92 * t7 * t9)
      t108 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 + t39 * t45 * t46 * t48 / 0.16D2 + (0.90D
     #2 * t39 * (-t20 + t55 * t36) + t62) * t46 / 0.1440D4 - (0.90D2 * t
     #39 * (t20 - t68 * t36 - t70 + t75 * t44) + 0.180D3 * t3 * t18 * t4
     #5) * t48 / 0.1440D4 - t39 * t36 * t46 * t88 / 0.8D1 - (0.90D2 * t3
     #9 * (t20 - t96 * t36) - t62) * t88 / 0.720D3 + t39 * t45 * t88 * t
     #48 / 0.8D1
      t109 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t108)
      t111 = 0.1D1 - x1
      t112 = 0.1D1 - x3
      t113 = KAPPA2(t111, x2, t112, 0.10D1, z)
      t114 = s * t113
      t115 = -t111
      t116 = t1 * t115
      t117 = -t112
      t122 = t1 * x1
      t124 = t113 ** 2
      t126 = t1 ** 2
      t128 = t115 * x1
      t132 = 0.1D1 / (-0.2D1 + t113)
      t134 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, t112, 0.
     #10D1)
      t139 = FJET(XB1, XB2, s, t114 * t116 * t117, -t114 * t116 * x3, t1
     #14 * t122, 0.0D0, -s * t124 * t126 * t128 * x3, t39 * t132 * t134 
     #* t46 * t88 / 0.8D1)
      t149 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.10D1, 
     #0.10D1)
      t154 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.10D1, 
     #0.10D1)
      t155 = t115 ** 2
      t159 = log(0.4D1 * t10 * t92 * t155)
      t174 = t39 * t149 * t46 * t88 / 0.8D1 - (-0.90D2 * t39 * (t154 - t
     #159 * t149) + 0.180D3 * t3 * t18 * t149) * t88 / 0.720D3 + t39 * t
     #149 * t88 * t48 / 0.8D1
      t175 = FJET(XB1, XB2, s, -t2 * t115, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t174)
      t179 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t112, 
     #0.10D1)
      t184 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t112, 
     #t43)
      t190 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t112, 
     #0.10D1)
      t194 = log(-0.4D1 * t52 * t9 * t117)
      t205 = t39 * t179 * t46 * t88 / 0.8D1 + t39 * (-t184 + t179) * t46
     # * t48 / 0.16D2 + (0.90D2 * t39 * (t190 - t194 * t179) - 0.180D3 *
     # t3 * t18 * t179) * t46 / 0.1440D4
      t206 = FJET(XB1, XB2, s, -t2 * t117, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t205)
      t208 = KAPPA2(t111, x2, 0.10D1, t43, z)
      t209 = s * t208
      t215 = t208 ** 2
      t221 = 0.1D1 / (-0.2D1 + t208)
      t223 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.10D1, 
     #t43)
      t228 = FJET(XB1, XB2, s, -t209 * t116, 0.0D0, -t209 * t122 * t71, 
     #t209 * t122 * x4, -s * t215 * t126 * t128 * x4, t39 * t221 * t223 
     #* t88 * t48 / 0.8D1)
      rrgg2gght5s1em1 = t109 * t108 + t139 * 0.3141592653589793D1 * t18 
     #* t132 * t134 * t46 * t88 / 0.8D1 + t175 * t174 + t206 * t205 + t2
     #28 * 0.3141592653589793D1 * t18 * t221 * t223 * t88 * t48 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t9 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.1D1 - x4)
      t15 = 0.1D1 / x3
      t19 = 0.1D1 / x1
      t23 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t29 = sin(x2 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t31 = z ** 2
      t35 = log(0.4D1 * t30 / t31)
      t42 = -t6 * (t7 - t9) / x4 / 0.16D2 - t6 * t7 * t15 / 0.16D2 - t6 
     #* t7 * t19 / 0.8D1 - t6 * t23 / 0.16D2 - (-0.180D3 * 0.31415926535
     #89793D1 * lh - 0.90D2 * t35 * 0.3141592653589793D1) * t5 * t7 / 0.
     #1440D4
      t43 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t42)
      t45 = -0.1D1 + x3
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t45, 0
     #.10D1)
      t53 = FJET(XB1, XB2, s, -t2 * t45, t2 * x3, 0.0D0, 0.0D0, 0.0D0, t
     #6 * t49 * t15 / 0.16D2)
      t59 = -0.1D1 + x1
      t63 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, -t59, x2, 0.10D1, 0
     #.10D1)
      t67 = FJET(XB1, XB2, s, -t2 * t59, 0.0D0, t2 * x1, 0.0D0, 0.0D0, t
     #6 * t63 * t19 / 0.8D1)
      rrgg2gght5s1em2 = t43 * t42 + t53 * 0.3141592653589793D1 * t5 * t4
     #9 * t15 / 0.16D2 + t67 * 0.3141592653589793D1 * t5 * t63 * t19 / 0
     #.8D1

      end function



      doubleprecision function rrgg2gght5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t10 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s1em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = 0.3141592653589793D1 * t18
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t84 = -0.1D1 + x4
      t85 = t83 * t84
      t88 = log(-0.4D1 * t82 * t85)
      t89 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t91 = t88 ** 2
      t92 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t95 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t98 = log(0.4D1 * t82 * t83)
      t100 = t98 ** 2
      t112 = -t78 + t92
      t113 = t34 * t112
      t116 = 0.1D1 / x1
      t118 = 0.1D1 / x4
      t121 = t82 * t15
      t123 = log(0.4D1 * t121)
      t128 = t123 ** 2
      t138 = t34 * t78
      t139 = t10 * t138
      t150 = x3 * t81
      t151 = t150 * t13
      t154 = log(-0.4D1 * t151 * t85)
      t156 = t16 * x4
      t159 = log(0.4D1 * t150 * t156)
      t167 = 0.1D1 / x3
      t169 = t116 * t118
      t172 = t150 * t16
      t174 = log(0.4D1 * t172)
      t176 = t174 ** 2
      t192 = x4 * t84
      t195 = log(-0.4D1 * t16 * t192)
      t198 = log(0.4D1 * t156)
      t203 = t198 ** 2
      t210 = t195 ** 2
      t213 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t222 = -t34 * t112
      t237 = x3 * t13
      t240 = log(-0.4D1 * t237 * t85)
      t242 = t240 ** 2
      t247 = log(0.4D1 * t237 * t83)
      t249 = t247 ** 2
      t266 = t237 * t15
      t268 = log(0.4D1 * t266)
      t273 = t268 ** 2
      t293 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t56 * 
     #(-t88 * t89 + t91 * t92 / 0.2D1 + t95 + t98 * t36 - t100 * t78 / 0
     #.2D1 - t45) - 0.180D3 * t48 * t34 * (t89 - t88 * t92 - t36 + t98 *
     # t78) + t39 * t113) * t116 * t118 / 0.720D3 + (t39 * t34 * (-t36 +
     # t123 * t78) + 0.90D2 * t56 * (-t128 * t36 / 0.2D1 - t53 + t128 * 
     #t123 * t78 / 0.6D1 + t123 * t45) - t139 - 0.180D3 * t48 * t34 * (t
     #123 * t36 - t128 * t78 / 0.2D1 - t45)) * t116 / 0.720D3 + (0.90D2 
     #* t56 * (t89 - t154 * t92 - t36 + t159 * t78) - 0.180D3 * t48 * t1
     #13) * t167 * t169 / 0.720D3 + (0.90D2 * t56 * (t174 * t36 - t176 *
     # t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (-t36 + t174 * t78) - 
     #t39 * t138) * t167 * t116 / 0.720D3 - (t39 * t34 * (-t89 + t195 * 
     #t92 + t36 - t198 * t78) + 0.90D2 * t56 * (t203 * t36 / 0.2D1 + t53
     # - t203 * t198 * t78 / 0.6D1 - t198 * t45 - t210 * t89 / 0.2D1 - t
     #213 + t210 * t195 * t92 / 0.6D1 + t195 * t95) + t10 * t222 - 0.180
     #D3 * t48 * t34 * (t195 * t89 - t210 * t92 / 0.2D1 - t95 - t198 * t
     #36 + t203 * t78 / 0.2D1 + t45)) * t118 / 0.1440D4 - (0.90D2 * t56 
     #* (t240 * t89 - t242 * t92 / 0.2D1 - t95 - t247 * t36 + t249 * t78
     # / 0.2D1 + t45) - 0.180D3 * t48 * t34 * (-t89 + t240 * t92 + t36 -
     # t247 * t78) + t39 * t222) * t167 * t118 / 0.1440D4 - (t39 * t34 *
     # (t36 - t268 * t78) + 0.90D2 * t56 * (t273 * t36 / 0.2D1 + t53 - t
     #273 * t268 * t78 / 0.6D1 - t268 * t45) + t139 - 0.180D3 * t48 * t3
     #4 * (-t268 * t36 + t273 * t78 / 0.2D1 + t45)) * t167 / 0.1440D4
      t294 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t293)
      t296 = 0.1D1 - x1
      t297 = 0.1D1 - x3
      t298 = KAPPA2(t296, x2, t297, 0.0D0, z)
      t299 = s * t298
      t300 = -t296
      t301 = t1 * t300
      t302 = -t297
      t303 = t301 * t302
      t305 = t301 * x3
      t307 = t1 * x1
      t309 = t298 ** 2
      t311 = t1 ** 2
      t313 = t300 * x1
      t317 = 0.1D1 / (-0.2D1 + t298)
      t318 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, 0.
     #0D0)
      t319 = t317 * t318
      t320 = t300 ** 2
      t321 = t320 * t302
      t322 = t309 ** 2
      t327 = log(-0.4D1 * t172 * t321 * x4 * t322)
      t329 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, 0.
     #0D0)
      t335 = t34 * t317 * t329
      t341 = t15 * t320
      t346 = log(-0.4D1 * t151 * t341 * t302 * t322)
      t347 = t346 * t317
      t349 = t346 ** 2
      t353 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, 0.
     #0D0)
      t368 = (0.90D2 * t56 * (t319 - t327 * t317 * t329) - 0.180D3 * t48
     # * t335) * t167 * t169 / 0.720D3 + (0.90D2 * t56 * (-t347 * t318 +
     # t349 * t317 * t329 / 0.2D1 + t317 * t353) - 0.180D3 * t48 * t34 *
     # (t319 - t347 * t329) + t39 * t335) * t167 * t116 / 0.720D3
      t369 = FJET(XB1, XB2, s, t299 * t303, -t299 * t305, 0.0D0, t299 * 
     #t307, s * t309 * t311 * t313 * t302, t368)
      t371 = KAPPA2(t296, x2, t297, x4, z)
      t372 = s * t371
      t375 = t307 * x4
      t377 = t307 * t84
      t379 = t371 ** 2
      t384 = cos(t11)
      t387 = Sqrt(x3 * t302 * t192)
      t394 = 0.1D1 / (-0.2D1 + t371)
      t395 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, x4
     #)
      t397 = t379 ** 2
      t402 = log(0.4D1 * t172 * t321 * t192 * t397)
      t404 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, t297, x4
     #)
      t413 = 0.90D2 * t56 * (-t394 * t395 + t402 * t394 * t404) + 0.180D
     #3 * t48 * t34 * t394 * t404
      t417 = FJET(XB1, XB2, s, t372 * t303, -t372 * t305, t372 * t375, -
     #t372 * t377, s * t379 * t311 * t313 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t384 * t387), t413 * t167 * t169 / 0.720D3)
      t428 = log(0.4D1 * t266 * t192 * t302)
      t429 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #x4)
      t431 = t428 ** 2
      t432 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #x4)
      t435 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #x4)
      t436 = t15 * t302
      t437 = t436 * x4
      t440 = log(-0.4D1 * t237 * t437)
      t441 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.0D0)
      t443 = t440 ** 2
      t444 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.0D0)
      t447 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.0D0)
      t457 = -t444 + t432
      t466 = log(-0.4D1 * t237 * t436)
      t471 = t466 ** 2
      t474 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t297, 
     #0.0D0)
      t482 = t34 * t444
      t496 = log(-0.4D1 * t151 * t437)
      t502 = log(0.4D1 * t151 * t83 * t84 * t302)
      t518 = log(-0.4D1 * t150 * t16 * t302)
      t520 = t518 ** 2
      t536 = -(0.90D2 * t56 * (-t428 * t429 + t431 * t432 / 0.2D1 + t435
     # + t440 * t441 - t443 * t444 / 0.2D1 - t447) - 0.180D3 * t48 * t34
     # * (t429 - t428 * t432 - t441 + t440 * t444) + t39 * t34 * t457) *
     # t167 * t118 / 0.1440D4 - (-t39 * t34 * (t441 - t466 * t444) - 0.9
     #0D2 * t56 * (t471 * t441 / 0.2D1 + t474 - t471 * t466 * t444 / 0.6
     #D1 - t466 * t447) - t10 * t482 + 0.180D3 * t48 * t34 * (-t466 * t4
     #41 + t471 * t444 / 0.2D1 + t447)) * t167 / 0.1440D4 + (0.90D2 * t5
     #6 * (t441 - t496 * t444 - t429 + t502 * t432) + 0.180D3 * t48 * t3
     #4 * t457) * t167 * t169 / 0.720D3 + (0.90D2 * t56 * (-t518 * t441 
     #+ t520 * t444 / 0.2D1 + t447) - 0.180D3 * t48 * t34 * (t441 - t518
     # * t444) + t39 * t482) * t167 * t116 / 0.720D3
      t537 = FJET(XB1, XB2, s, -t2 * t302, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t536)
      t539 = KAPPA2(t296, x2, 0.10D1, 0.0D0, z)
      t540 = s * t539
      t543 = t539 ** 2
      t548 = t320 * x4
      t549 = t543 ** 2
      t553 = log(0.4D1 * t121 * t548 * t549)
      t555 = 0.1D1 / (-0.2D1 + t539)
      t556 = t553 * t555
      t557 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.0D0)
      t559 = t553 ** 2
      t561 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.0D0)
      t564 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.0D0)
      t565 = t555 * t564
      t569 = t555 * t557
      t576 = t34 * t555 * t561
      t577 = t39 * t576
      t581 = t341 * t549
      t584 = log(0.4D1 * t82 * t581)
      t585 = t584 * t555
      t590 = t584 ** 2
      t591 = t590 * t555
      t594 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #0.0D0)
      t618 = log(0.4D1 * t151 * t341 * x4 * t549)
      t631 = log(0.4D1 * t151 * t581)
      t632 = t631 * t555
      t634 = t631 ** 2
      t650 = (0.90D2 * t56 * (t556 * t557 - t559 * t555 * t561 / 0.2D1 -
     # t565) - 0.180D3 * t48 * t34 * (-t569 + t556 * t561) - t577) * t11
     #6 * t118 / 0.720D3 + (t39 * t34 * (-t569 + t585 * t561) + 0.90D2 *
     # t56 * (-t591 * t557 / 0.2D1 - t555 * t594 + t590 * t584 * t555 * 
     #t561 / 0.6D1 + t585 * t564) - t10 * t576 - 0.180D3 * t48 * t34 * (
     #t585 * t557 - t591 * t561 / 0.2D1 - t565)) * t116 / 0.720D3 + (0.9
     #0D2 * t56 * (-t569 + t618 * t555 * t561) + 0.180D3 * t48 * t576) *
     # t167 * t169 / 0.720D3 + (0.90D2 * t56 * (t632 * t557 - t634 * t55
     #5 * t561 / 0.2D1 - t565) - 0.180D3 * t48 * t34 * (-t569 + t632 * t
     #561) - t577) * t167 * t116 / 0.720D3
      t651 = FJET(XB1, XB2, s, -t540 * t301, 0.0D0, 0.0D0, t540 * t307, 
     #-s * t543 * t311 * t300 * x1, t650)
      t653 = KAPPA2(t296, x2, 0.10D1, x4, z)
      t654 = s * t653
      t658 = t653 ** 2
      t663 = t658 ** 2
      t665 = t548 * t84 * t663
      t668 = log(-0.4D1 * t121 * t665)
      t670 = 0.1D1 / (-0.2D1 + t653)
      t671 = t668 * t670
      t672 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #x4)
      t674 = t668 ** 2
      t676 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #x4)
      t679 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t296, x2, 0.10D1, 
     #x4)
      t684 = t670 * t672
      t691 = t34 * t670 * t676
      t698 = log(-0.4D1 * t172 * t665)
      t710 = (-0.90D2 * t56 * (t671 * t672 - t674 * t670 * t676 / 0.2D1 
     #- t670 * t679) + 0.180D3 * t48 * t34 * (-t684 + t671 * t676) + t39
     # * t691) * t116 * t118 / 0.720D3 + (-0.90D2 * t56 * (-t684 + t698 
     #* t670 * t676) - 0.180D3 * t48 * t691) * t167 * t169 / 0.720D3
      t711 = FJET(XB1, XB2, s, -t654 * t301, 0.0D0, t654 * t375, -t654 *
     # t377, s * t658 * t311 * t313 * t84, t710)
      rrgg2gght5s2e1 = t294 * t293 + t369 * t368 + t417 * t413 * t167 * 
     #t116 * t118 / 0.720D3 + t537 * t536 + t651 * t650 + t711 * t710

      end function



      doubleprecision function rrgg2gght5s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t60 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t61 = x3 * t11
      t62 = t13 * x4
      t63 = -0.1D1 + x4
      t64 = t62 * t63
      t67 = log(-0.4D1 * t61 * t64)
      t68 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t72 = log(0.4D1 * t61 * t62)
      t77 = -t68 + t49
      t78 = t26 * t77
      t82 = 0.1D1 / x3
      t84 = 0.1D1 / x4
      t87 = t61 * t13
      t89 = log(0.4D1 * t87)
      t91 = t89 ** 2
      t102 = t26 * t49
      t103 = t8 * t102
      t107 = x4 * t63
      t110 = log(-0.4D1 * t14 * t107)
      t112 = t110 ** 2
      t115 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t118 = log(0.4D1 * t14 * x4)
      t120 = t118 ** 2
      t136 = x1 ** 2
      t137 = t136 * t11
      t140 = log(-0.4D1 * t137 * t64)
      t144 = log(0.4D1 * t137 * t62)
      t149 = -t77
      t154 = 0.1D1 / x1
      t160 = t82 * t154 * t84
      t163 = x3 * t136
      t166 = log(0.4D1 * t163 * t14)
      t177 = t137 * t13
      t179 = log(0.4D1 * t177)
      t181 = t179 ** 2
      t195 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 - (0.90D2 * t31 * (-t60 + t67 * t68 + t28 - t72 * t49) - 0.
     #180D3 * t52 * t78) * t82 * t84 / 0.1440D4 - (0.90D2 * t31 * (-t89 
     #* t28 + t91 * t49 / 0.2D1 + t57) - 0.180D3 * t52 * t26 * (t28 - t8
     #9 * t49) + t103) * t82 / 0.1440D4 - (0.90D2 * t31 * (t110 * t60 - 
     #t112 * t68 / 0.2D1 - t115 - t118 * t28 + t120 * t49 / 0.2D1 + t57)
     # - 0.180D3 * t52 * t26 * (-t60 + t110 * t68 + t28 - t118 * t49) + 
     #t8 * t78) * t84 / 0.1440D4 + (0.90D2 * t31 * (t60 - t140 * t68 - t
     #28 + t144 * t49) - 0.180D3 * t52 * t26 * t149) * t154 * t84 / 0.72
     #0D3 + t31 * t149 * t160 / 0.8D1 + (0.90D2 * t31 * (-t28 + t166 * t
     #49) + 0.180D3 * t52 * t102) * t82 * t154 / 0.720D3 + (0.90D2 * t31
     # * (t179 * t28 - t181 * t49 / 0.2D1 - t57) - 0.180D3 * t52 * t26 *
     # (-t28 + t179 * t49) - t103) * t154 / 0.720D3
      t196 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t195)
      t198 = 0.1D1 - x1
      t199 = 0.1D1 - x3
      t200 = KAPPA2(t198, x2, t199, 0.0D0, z)
      t201 = s * t200
      t202 = -t198
      t203 = t1 * t202
      t204 = -t199
      t205 = t203 * t204
      t207 = t203 * x3
      t209 = t1 * x1
      t211 = t200 ** 2
      t213 = t1 ** 2
      t215 = t202 * x1
      t219 = 0.1D1 / (-0.2D1 + t200)
      t221 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, t199, 0.
     #0D0)
      t223 = t154 * t84
      t227 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, t199, 0.
     #0D0)
      t229 = t163 * t11
      t230 = t202 ** 2
      t231 = t13 * t230
      t232 = t211 ** 2
      t237 = log(-0.4D1 * t229 * t231 * t204 * t232)
      t251 = t31 * t219 * t221 * t82 * t223 / 0.8D1 + (0.90D2 * t31 * (t
     #219 * t227 - t237 * t219 * t221) - 0.180D3 * t52 * t26 * t219 * t2
     #21) * t82 * t154 / 0.720D3
      t252 = FJET(XB1, XB2, s, t201 * t205, -t201 * t207, 0.0D0, t201 * 
     #t209, s * t211 * t213 * t215 * t204, t251)
      t254 = KAPPA2(t198, x2, t199, x4, z)
      t255 = s * t254
      t258 = t209 * x4
      t260 = t209 * t63
      t262 = t254 ** 2
      t267 = cos(t9)
      t270 = Sqrt(x3 * t204 * t107)
      t277 = 0.1D1 / (-0.2D1 + t254)
      t279 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, t199, x4
     #)
      t281 = t279 * t82 * t223
      t284 = FJET(XB1, XB2, s, t255 * t205, -t255 * t207, t255 * t258, -
     #t255 * t260, s * t262 * t213 * t215 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t267 * t270), -t31 * t277 * t281 / 0.8D1)
      t292 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t199, 
     #0.0D0)
      t293 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t199, 
     #x4)
      t294 = t292 - t293
      t298 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t199, 
     #0.0D0)
      t302 = log(-0.4D1 * t163 * t14 * t204)
      t307 = t26 * t292
      t314 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t199, 
     #x4)
      t318 = log(0.4D1 * t87 * t107 * t204)
      t320 = t13 * t204
      t324 = log(-0.4D1 * t61 * t320 * x4)
      t339 = log(-0.4D1 * t61 * t320)
      t341 = t339 ** 2
      t344 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t199, 
     #0.0D0)
      t357 = t31 * t294 * t160 / 0.8D1 + (0.90D2 * t31 * (t298 - t302 * 
     #t292) - 0.180D3 * t52 * t307) * t82 * t154 / 0.720D3 - (0.90D2 * t
     #31 * (t314 - t318 * t293 - t298 + t324 * t292) + 0.180D3 * t52 * t
     #26 * t294) * t82 * t84 / 0.1440D4 - (-0.90D2 * t31 * (-t339 * t298
     # + t341 * t292 / 0.2D1 + t344) + 0.180D3 * t52 * t26 * (t298 - t33
     #9 * t292) - t8 * t307) * t82 / 0.1440D4
      t358 = FJET(XB1, XB2, s, -t2 * t204, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t357)
      t360 = KAPPA2(t198, x2, 0.10D1, 0.0D0, z)
      t361 = s * t360
      t364 = t360 ** 2
      t370 = 0.1D1 / (-0.2D1 + t360)
      t371 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.10D1, 
     #0.0D0)
      t372 = t370 * t371
      t373 = t230 * x4
      t374 = t364 ** 2
      t378 = log(0.4D1 * t177 * t373 * t374)
      t380 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.10D1, 
     #0.0D0)
      t386 = t26 * t370 * t380
      t388 = 0.180D3 * t52 * t386
      t398 = t231 * t374
      t401 = log(0.4D1 * t229 * t398)
      t413 = log(0.4D1 * t137 * t398)
      t414 = t413 * t370
      t416 = t413 ** 2
      t420 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.10D1, 
     #0.0D0)
      t434 = (0.90D2 * t31 * (-t372 + t378 * t370 * t380) + t388) * t154
     # * t84 / 0.720D3 - t31 * t370 * t380 * t82 * t223 / 0.8D1 + (0.90D
     #2 * t31 * (-t372 + t401 * t370 * t380) + t388) * t82 * t154 / 0.72
     #0D3 + (0.90D2 * t31 * (t414 * t371 - t416 * t370 * t380 / 0.2D1 - 
     #t370 * t420) - 0.180D3 * t52 * t26 * (-t372 + t414 * t380) - t8 * 
     #t386) * t154 / 0.720D3
      t435 = FJET(XB1, XB2, s, -t361 * t203, 0.0D0, 0.0D0, t361 * t209, 
     #-s * t364 * t213 * t202 * x1, t434)
      t437 = KAPPA2(t198, x2, 0.10D1, x4, z)
      t438 = s * t437
      t442 = t437 ** 2
      t448 = 0.1D1 / (-0.2D1 + t437)
      t449 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.10D1, 
     #x4)
      t451 = t442 ** 2
      t456 = log(-0.4D1 * t177 * t373 * t63 * t451)
      t458 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.10D1, 
     #x4)
      t476 = (-0.90D2 * t31 * (-t448 * t449 + t456 * t448 * t458) - 0.18
     #0D3 * t52 * t26 * t448 * t458) * t154 * t84 / 0.720D3 + t31 * t448
     # * t458 * t82 * t223 / 0.8D1
      t477 = FJET(XB1, XB2, s, -t438 * t203, 0.0D0, t438 * t258, -t438 *
     # t260, s * t442 * t213 * t215 * t63, t476)
      rrgg2gght5s2e0 = t196 * t195 + t252 * t251 - t284 * 0.314159265358
     #9793D1 * t26 * t277 * t281 / 0.8D1 + t358 * t357 + t435 * t434 + t
     #477 * t476

      end function



      doubleprecision function rrgg2gght5s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t43 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t44 = -t43 + t36
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x4
      t51 = x3 * t7
      t54 = log(0.4D1 * t51 * t9)
      t61 = 0.180D3 * t3 * t18 * t36
      t65 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t66 = -0.1D1 + x4
      t70 = log(-0.4D1 * t10 * x4 * t66)
      t74 = log(0.4D1 * t10 * x4)
      t86 = 0.1D1 / x1
      t90 = x1 ** 2
      t91 = t90 * t7
      t94 = log(0.4D1 * t91 * t9)
      t107 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 - t39 * t44 * t45 * t47 / 0.16D2 - (0.90D
     #2 * t39 * (t20 - t54 * t36) - t61) * t45 / 0.1440D4 - (0.90D2 * t3
     #9 * (-t65 + t70 * t43 + t20 - t74 * t36) - 0.180D3 * t3 * t18 * t4
     #4) * t47 / 0.1440D4 - t39 * t36 * t45 * t86 / 0.8D1 + (0.90D2 * t3
     #9 * (-t20 + t94 * t36) + t61) * t86 / 0.720D3 - t39 * t44 * t86 * 
     #t47 / 0.8D1
      t108 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t107)
      t110 = 0.1D1 - x1
      t111 = 0.1D1 - x3
      t112 = KAPPA2(t110, x2, t111, 0.0D0, z)
      t113 = s * t112
      t114 = -t110
      t115 = t1 * t114
      t116 = -t111
      t121 = t1 * x1
      t123 = t112 ** 2
      t125 = t1 ** 2
      t127 = t114 * x1
      t131 = 0.1D1 / (-0.2D1 + t112)
      t133 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t110, x2, t111, 0.
     #0D0)
      t138 = FJET(XB1, XB2, s, t113 * t115 * t116, -t113 * t115 * x3, 0.
     #0D0, t113 * t121, s * t123 * t125 * t127 * t116, t39 * t131 * t133
     # * t45 * t86 / 0.8D1)
      t148 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t111, 
     #0.0D0)
      t153 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t111, 
     #x4)
      t159 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t111, 
     #0.0D0)
      t163 = log(-0.4D1 * t51 * t9 * t116)
      t174 = t39 * t148 * t45 * t86 / 0.8D1 - t39 * (-t148 + t153) * t45
     # * t47 / 0.16D2 - (-0.90D2 * t39 * (t159 - t163 * t148) + 0.180D3 
     #* t3 * t18 * t148) * t45 / 0.1440D4
      t175 = FJET(XB1, XB2, s, -t2 * t116, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t174)
      t177 = KAPPA2(t110, x2, 0.10D1, 0.0D0, z)
      t178 = s * t177
      t181 = t177 ** 2
      t187 = 0.1D1 / (-0.2D1 + t177)
      t188 = t39 * t187
      t189 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t110, x2, 0.10D1, 
     #0.0D0)
      t194 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t110, x2, 0.10D1, 
     #0.0D0)
      t196 = t114 ** 2
      t198 = t181 ** 2
      t202 = log(0.4D1 * t91 * t9 * t196 * t198)
      t219 = -t188 * t189 * t45 * t86 / 0.8D1 + (0.90D2 * t39 * (-t187 *
     # t194 + t202 * t187 * t189) + 0.180D3 * t3 * t18 * t187 * t189) * 
     #t86 / 0.720D3 - t188 * t189 * t86 * t47 / 0.8D1
      t220 = FJET(XB1, XB2, s, -t178 * t115, 0.0D0, 0.0D0, t178 * t121, 
     #-s * t181 * t125 * t114 * x1, t219)
      t222 = KAPPA2(t110, x2, 0.10D1, x4, z)
      t223 = s * t222
      t229 = t222 ** 2
      t235 = 0.1D1 / (-0.2D1 + t222)
      t237 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t110, x2, 0.10D1, 
     #x4)
      t242 = FJET(XB1, XB2, s, -t223 * t115, 0.0D0, t223 * t121 * x4, -t
     #223 * t121 * t66, s * t229 * t125 * t127 * t66, t39 * t235 * t237 
     #* t86 * t47 / 0.8D1)
      rrgg2gght5s2em1 = t108 * t107 + t138 * 0.3141592653589793D1 * t18 
     #* t131 * t133 * t45 * t86 / 0.8D1 + t175 * t174 + t220 * t219 + t2
     #42 * 0.3141592653589793D1 * t18 * t235 * t237 * t86 * t47 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #x4)
      t8 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t14 = 0.1D1 / x3
      t18 = 0.1D1 / x1
      t22 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t28 = sin(x2 * 0.3141592653589793D1)
      t29 = t28 ** 2
      t30 = z ** 2
      t34 = log(0.4D1 * t29 / t30)
      t41 = -t6 * (-t7 + t8) / x4 / 0.16D2 - t6 * t8 * t14 / 0.16D2 - t6
     # * t8 * t18 / 0.8D1 - t6 * t22 / 0.16D2 - (-0.180D3 * 0.3141592653
     #589793D1 * lh - 0.90D2 * 0.3141592653589793D1 * t34) * t5 * t8 / 0
     #.1440D4
      t42 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t44 = -0.1D1 + x3
      t48 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t44, 0
     #.0D0)
      t52 = FJET(XB1, XB2, s, -t2 * t44, t2 * x3, 0.0D0, 0.0D0, 0.0D0, t
     #6 * t48 * t14 / 0.16D2)
      t58 = 0.1D1 - x1
      t59 = KAPPA2(t58, x2, 0.10D1, 0.0D0, z)
      t60 = s * t59
      t61 = -t58
      t66 = t59 ** 2
      t68 = t1 ** 2
      t74 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t58, x2, 0.10D1, 0.
     #0D0)
      t76 = 0.1D1 / (-0.2D1 + t59) * t74 * t18
      t79 = FJET(XB1, XB2, s, -t60 * t1 * t61, 0.0D0, 0.0D0, t60 * t1 * 
     #x1, -s * t66 * t68 * t61 * x1, -t6 * t76 / 0.8D1)
      rrgg2gght5s2em2 = t42 * t41 + t52 * 0.3141592653589793D1 * t5 * t4
     #8 * t14 / 0.16D2 - t79 * 0.3141592653589793D1 * t5 * t76 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t10 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s2em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = 0.3141592653589793D1 * t18
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t84 = -0.1D1 + x4
      t85 = t83 * t84
      t88 = log(-0.4D1 * t82 * t85)
      t89 = -t84
      t90 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t89)
      t92 = t88 ** 2
      t93 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t89)
      t96 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t89)
      t99 = log(0.4D1 * t82 * t83)
      t101 = t99 ** 2
      t113 = -t93 + t78
      t114 = t34 * t113
      t117 = 0.1D1 / x1
      t119 = 0.1D1 / x4
      t122 = t82 * t15
      t124 = log(0.4D1 * t122)
      t129 = t124 ** 2
      t139 = t34 * t78
      t140 = t10 * t139
      t151 = x3 * t81
      t152 = t16 * x4
      t155 = log(0.4D1 * t151 * t152)
      t157 = t151 * t13
      t160 = log(-0.4D1 * t157 * t85)
      t168 = 0.1D1 / x3
      t170 = t117 * t119
      t173 = t151 * t16
      t175 = log(0.4D1 * t173)
      t177 = t175 ** 2
      t193 = x4 * t84
      t196 = log(-0.4D1 * t16 * t193)
      t199 = log(0.4D1 * t152)
      t204 = t199 ** 2
      t211 = t196 ** 2
      t214 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t89)
      t236 = x3 * t13
      t239 = log(0.4D1 * t236 * t83)
      t241 = t239 ** 2
      t246 = log(-0.4D1 * t236 * t85)
      t248 = t246 ** 2
      t267 = t236 * t15
      t269 = log(0.4D1 * t267)
      t274 = t269 ** 2
      t294 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 - (0.90D2 * t56 * 
     #(t88 * t90 - t92 * t93 / 0.2D1 - t96 - t99 * t36 + t101 * t78 / 0.
     #2D1 + t45) - 0.180D3 * t48 * t34 * (-t90 + t88 * t93 + t36 - t99 *
     # t78) + t39 * t114) * t117 * t119 / 0.720D3 + (t39 * t34 * (-t36 +
     # t124 * t78) + 0.90D2 * t56 * (-t129 * t36 / 0.2D1 - t53 + t129 * 
     #t124 * t78 / 0.6D1 + t124 * t45) - t140 - 0.180D3 * t48 * t34 * (t
     #124 * t36 - t129 * t78 / 0.2D1 - t45)) * t117 / 0.720D3 - (0.90D2 
     #* t56 * (t36 - t155 * t78 - t90 + t160 * t93) - 0.180D3 * t48 * t1
     #14) * t168 * t170 / 0.720D3 + (0.90D2 * t56 * (t175 * t36 - t177 *
     # t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (-t36 + t175 * t78) - 
     #t39 * t139) * t168 * t117 / 0.720D3 - (t39 * t34 * (-t90 + t196 * 
     #t93 + t36 - t199 * t78) + 0.90D2 * t56 * (t204 * t36 / 0.2D1 + t53
     # - t204 * t199 * t78 / 0.6D1 - t199 * t45 - t211 * t90 / 0.2D1 - t
     #214 + t211 * t196 * t93 / 0.6D1 + t196 * t96) + t10 * t114 - 0.180
     #D3 * t48 * t34 * (t196 * t90 - t211 * t93 / 0.2D1 - t96 - t199 * t
     #36 + t204 * t78 / 0.2D1 + t45)) * t119 / 0.1440D4 + (0.90D2 * t56 
     #* (t239 * t36 - t241 * t78 / 0.2D1 - t45 - t246 * t90 + t248 * t93
     # / 0.2D1 + t96) - 0.180D3 * t48 * t34 * (-t36 + t239 * t78 + t90 -
     # t246 * t93) - t39 * t34 * t113) * t168 * t119 / 0.1440D4 - (t39 *
     # t34 * (t36 - t269 * t78) + 0.90D2 * t56 * (t274 * t36 / 0.2D1 + t
     #53 - t274 * t269 * t78 / 0.6D1 - t269 * t45) + t140 - 0.180D3 * t4
     #8 * t34 * (-t269 * t36 + t274 * t78 / 0.2D1 + t45)) * t168 / 0.144
     #0D4
      t295 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t294)
      t297 = 0.1D1 - x1
      t298 = KAPPA2(t297, x2, 0.0D0, 0.10D1, z)
      t299 = s * t298
      t300 = -t297
      t301 = t1 * t300
      t303 = t1 * x1
      t305 = t298 ** 2
      t307 = t1 ** 2
      t311 = t300 ** 2
      t312 = t311 * x4
      t313 = t305 ** 2
      t317 = log(0.4D1 * t122 * t312 * t313)
      t319 = 0.1D1 / (-0.2D1 + t298)
      t320 = t317 * t319
      t321 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, 0
     #.10D1)
      t323 = t317 ** 2
      t325 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, 0
     #.10D1)
      t328 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, 0
     #.10D1)
      t329 = t319 * t328
      t333 = t319 * t321
      t340 = t34 * t319 * t325
      t341 = t39 * t340
      t345 = t15 * t311
      t346 = t345 * t313
      t349 = log(0.4D1 * t82 * t346)
      t350 = t349 * t319
      t355 = t349 ** 2
      t356 = t355 * t319
      t359 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, 0
     #.10D1)
      t383 = log(0.4D1 * t157 * t345 * x4 * t313)
      t396 = log(0.4D1 * t157 * t346)
      t397 = t396 * t319
      t399 = t396 ** 2
      t415 = -(0.90D2 * t56 * (-t320 * t321 + t323 * t319 * t325 / 0.2D1
     # + t329) - 0.180D3 * t48 * t34 * (t333 - t320 * t325) + t341) * t1
     #17 * t119 / 0.720D3 + (-t39 * t34 * (t333 - t350 * t325) - 0.90D2 
     #* t56 * (t356 * t321 / 0.2D1 + t319 * t359 - t355 * t349 * t319 * 
     #t325 / 0.6D1 - t350 * t328) - t10 * t340 + 0.180D3 * t48 * t34 * (
     #-t350 * t321 + t356 * t325 / 0.2D1 + t329)) * t117 / 0.720D3 - (0.
     #90D2 * t56 * (t333 - t383 * t319 * t325) - 0.180D3 * t48 * t340) *
     # t168 * t170 / 0.720D3 + (0.90D2 * t56 * (t397 * t321 - t399 * t31
     #9 * t325 / 0.2D1 - t329) - 0.180D3 * t48 * t34 * (-t333 + t397 * t
     #325) - t341) * t168 * t117 / 0.720D3
      t416 = FJET(XB1, XB2, s, 0.0D0, -t299 * t301, t299 * t303, 0.0D0, 
     #-s * t305 * t307 * t300 * x1, t415)
      t418 = KAPPA2(t297, x2, 0.0D0, t89, z)
      t419 = s * t418
      t421 = t303 * t84
      t423 = t303 * x4
      t425 = t418 ** 2
      t428 = t300 * x1
      t431 = t425 ** 2
      t433 = t312 * t84 * t431
      t436 = log(-0.4D1 * t122 * t433)
      t438 = 0.1D1 / (-0.2D1 + t418)
      t439 = t436 * t438
      t440 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, t
     #89)
      t442 = t436 ** 2
      t444 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, t
     #89)
      t447 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t297, x2, 0.0D0, t
     #89)
      t452 = t438 * t440
      t459 = t34 * t438 * t444
      t466 = log(-0.4D1 * t173 * t433)
      t478 = -(-0.90D2 * t56 * (-t439 * t440 + t442 * t438 * t444 / 0.2D
     #1 + t438 * t447) + 0.180D3 * t48 * t34 * (t452 - t439 * t444) - t3
     #9 * t459) * t117 * t119 / 0.720D3 - (0.90D2 * t56 * (-t452 + t466 
     #* t438 * t444) + 0.180D3 * t48 * t459) * t168 * t170 / 0.720D3
      t479 = FJET(XB1, XB2, s, 0.0D0, -t419 * t301, -t419 * t421, t419 *
     # t423, s * t425 * t307 * t428 * t84, t478)
      t482 = -0.1D1 + x3
      t487 = log(0.4D1 * t267 * t193 * t482)
      t488 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t8
     #9)
      t490 = t487 ** 2
      t491 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t8
     #9)
      t494 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t8
     #9)
      t495 = t15 * t482
      t496 = t495 * x4
      t499 = log(-0.4D1 * t236 * t496)
      t500 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t502 = t499 ** 2
      t503 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t506 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t516 = -t491 + t503
      t525 = log(-0.4D1 * t236 * t495)
      t530 = t525 ** 2
      t533 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t541 = t34 * t503
      t557 = log(0.4D1 * t157 * t83 * t84 * t482)
      t561 = log(-0.4D1 * t157 * t496)
      t577 = log(-0.4D1 * t151 * t16 * t482)
      t579 = t577 ** 2
      t595 = (0.90D2 * t56 * (t487 * t488 - t490 * t491 / 0.2D1 - t494 -
     # t499 * t500 + t502 * t503 / 0.2D1 + t506) - 0.180D3 * t48 * t34 *
     # (-t488 + t487 * t491 + t500 - t499 * t503) + t39 * t34 * t516) * 
     #t168 * t119 / 0.1440D4 - (-t39 * t34 * (t500 - t525 * t503) - 0.90
     #D2 * t56 * (t530 * t500 / 0.2D1 + t533 - t530 * t525 * t503 / 0.6D
     #1 - t525 * t506) - t10 * t541 + 0.180D3 * t48 * t34 * (-t525 * t50
     #0 + t530 * t503 / 0.2D1 + t506)) * t168 / 0.1440D4 - (0.90D2 * t56
     # * (t488 - t557 * t491 - t500 + t561 * t503) + 0.180D3 * t48 * t34
     # * t516) * t168 * t170 / 0.720D3 + (0.90D2 * t56 * (-t577 * t500 +
     # t579 * t503 / 0.2D1 + t506) - 0.180D3 * t48 * t34 * (t500 - t577 
     #* t503) + t39 * t541) * t168 * t117 / 0.720D3
      t596 = FJET(XB1, XB2, s, t2 * x3, -t2 * t482, 0.0D0, 0.0D0, 0.0D0,
     # t595)
      t598 = KAPPA2(t297, x2, x3, 0.10D1, z)
      t599 = s * t598
      t600 = t301 * x3
      t602 = t301 * t482
      t605 = t598 ** 2
      t611 = 0.1D1 / (-0.2D1 + t598)
      t612 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t297, x2, x3, 0.10
     #D1)
      t613 = t611 * t612
      t614 = t311 * t482
      t615 = t605 ** 2
      t620 = log(-0.4D1 * t173 * t614 * x4 * t615)
      t622 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t297, x2, x3, 0.10
     #D1)
      t628 = t34 * t611 * t622
      t638 = log(-0.4D1 * t157 * t345 * t482 * t615)
      t639 = t638 * t611
      t641 = t638 ** 2
      t645 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t297, x2, x3, 0.10
     #D1)
      t660 = -(0.90D2 * t56 * (-t613 + t620 * t611 * t622) + 0.180D3 * t
     #48 * t628) * t168 * t170 / 0.720D3 + (0.90D2 * t56 * (-t639 * t612
     # + t641 * t611 * t622 / 0.2D1 + t611 * t645) - 0.180D3 * t48 * t34
     # * (t613 - t639 * t622) + t39 * t628) * t168 * t117 / 0.720D3
      t661 = FJET(XB1, XB2, s, -t599 * t600, t599 * t602, t599 * t303, 0
     #.0D0, s * t605 * t307 * t428 * t482, t660)
      t663 = KAPPA2(t297, x2, x3, t89, z)
      t664 = s * t663
      t669 = t663 ** 2
      t674 = cos(t11)
      t677 = Sqrt(x3 * t482 * t193)
      t684 = 0.1D1 / (-0.2D1 + t663)
      t685 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t297, x2, x3, t89)
      t687 = t669 ** 2
      t692 = log(0.4D1 * t173 * t614 * t193 * t687)
      t694 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t297, x2, x3, t89)
      t703 = 0.90D2 * t56 * (t684 * t685 - t692 * t684 * t694) - 0.180D3
     # * t48 * t34 * t684 * t694
      t707 = FJET(XB1, XB2, s, -t664 * t600, t664 * t602, -t664 * t421, 
     #t664 * t423, s * t669 * t307 * t428 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t674 * t677), -t703 * t168 * t170 / 0.720D3)
      rrgg2gght5s3e1 = t295 * t294 + t416 * t415 + t479 * t478 + t596 * 
     #t595 + t661 * t660 - t707 * t703 * t168 * t117 * t119 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t60 = x3 * t11
      t61 = t13 * x4
      t64 = log(0.4D1 * t60 * t61)
      t66 = 0.1D1 - x4
      t67 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t66)
      t68 = -t66
      t69 = t61 * t68
      t72 = log(-0.4D1 * t60 * t69)
      t73 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t66)
      t78 = -t49 + t73
      t83 = 0.1D1 / x3
      t85 = 0.1D1 / x4
      t88 = t60 * t13
      t90 = log(0.4D1 * t88)
      t92 = t90 ** 2
      t103 = t26 * t49
      t104 = t8 * t103
      t108 = x4 * t68
      t111 = log(-0.4D1 * t14 * t108)
      t113 = t111 ** 2
      t116 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t66)
      t119 = log(0.4D1 * t14 * x4)
      t121 = t119 ** 2
      t133 = -t78
      t134 = t26 * t133
      t139 = x1 ** 2
      t140 = t139 * t11
      t143 = log(-0.4D1 * t140 * t69)
      t147 = log(0.4D1 * t140 * t61)
      t155 = 0.1D1 / x1
      t161 = t83 * t155 * t85
      t164 = x3 * t139
      t167 = log(0.4D1 * t164 * t14)
      t178 = t140 * t13
      t180 = log(0.4D1 * t178)
      t182 = t180 ** 2
      t196 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 + (0.90D2 * t31 * (-t28 + t64 * t49 + t67 - t72 * t73) - 0.
     #180D3 * t52 * t26 * t78) * t83 * t85 / 0.1440D4 - (0.90D2 * t31 * 
     #(-t90 * t28 + t92 * t49 / 0.2D1 + t57) - 0.180D3 * t52 * t26 * (t2
     #8 - t90 * t49) + t104) * t83 / 0.1440D4 - (0.90D2 * t31 * (t111 * 
     #t67 - t113 * t73 / 0.2D1 - t116 - t119 * t28 + t121 * t49 / 0.2D1 
     #+ t57) - 0.180D3 * t52 * t26 * (-t67 + t111 * t73 + t28 - t119 * t
     #49) + t8 * t134) * t85 / 0.1440D4 - (0.90D2 * t31 * (-t67 + t143 *
     # t73 + t28 - t147 * t49) - 0.180D3 * t52 * t134) * t155 * t85 / 0.
     #720D3 - t31 * t133 * t161 / 0.8D1 + (0.90D2 * t31 * (-t28 + t167 *
     # t49) + 0.180D3 * t52 * t103) * t83 * t155 / 0.720D3 + (0.90D2 * t
     #31 * (t180 * t28 - t182 * t49 / 0.2D1 - t57) - 0.180D3 * t52 * t26
     # * (-t28 + t180 * t49) - t104) * t155 / 0.720D3
      t197 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t196)
      t199 = 0.1D1 - x1
      t200 = KAPPA2(t199, x2, 0.0D0, 0.10D1, z)
      t201 = s * t200
      t202 = -t199
      t203 = t1 * t202
      t205 = t1 * x1
      t207 = t200 ** 2
      t209 = t1 ** 2
      t214 = 0.1D1 / (-0.2D1 + t200)
      t215 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, 0
     #.10D1)
      t216 = t214 * t215
      t217 = t202 ** 2
      t218 = t217 * x4
      t219 = t207 ** 2
      t223 = log(0.4D1 * t178 * t218 * t219)
      t225 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, 0
     #.10D1)
      t231 = t26 * t214 * t225
      t233 = 0.180D3 * t52 * t231
      t240 = t155 * t85
      t244 = t164 * t11
      t245 = t13 * t217
      t246 = t245 * t219
      t249 = log(0.4D1 * t244 * t246)
      t261 = log(0.4D1 * t140 * t246)
      t262 = t261 * t214
      t264 = t261 ** 2
      t268 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, 0
     #.10D1)
      t282 = -(0.90D2 * t31 * (t216 - t223 * t214 * t225) - t233) * t155
     # * t85 / 0.720D3 - t31 * t214 * t225 * t83 * t240 / 0.8D1 + (0.90D
     #2 * t31 * (-t216 + t249 * t214 * t225) + t233) * t83 * t155 / 0.72
     #0D3 + (-0.90D2 * t31 * (-t262 * t215 + t264 * t214 * t225 / 0.2D1 
     #+ t214 * t268) + 0.180D3 * t52 * t26 * (t216 - t262 * t225) - t8 *
     # t231) * t155 / 0.720D3
      t283 = FJET(XB1, XB2, s, 0.0D0, -t201 * t203, t201 * t205, 0.0D0, 
     #-s * t207 * t209 * t202 * x1, t282)
      t285 = KAPPA2(t199, x2, 0.0D0, t66, z)
      t286 = s * t285
      t288 = t205 * t68
      t290 = t205 * x4
      t292 = t285 ** 2
      t295 = t202 * x1
      t299 = 0.1D1 / (-0.2D1 + t285)
      t300 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, t
     #66)
      t302 = t292 ** 2
      t307 = log(-0.4D1 * t178 * t218 * t68 * t302)
      t309 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, t
     #66)
      t327 = -(-0.90D2 * t31 * (t299 * t300 - t307 * t299 * t309) + 0.18
     #0D3 * t52 * t26 * t299 * t309) * t155 * t85 / 0.720D3 + t31 * t299
     # * t309 * t83 * t240 / 0.8D1
      t328 = FJET(XB1, XB2, s, 0.0D0, -t286 * t203, -t286 * t288, t286 *
     # t290, s * t292 * t209 * t295 * t68, t327)
      t331 = -0.1D1 + x3
      t333 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t334 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t6
     #6)
      t335 = -t333 + t334
      t339 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t343 = log(-0.4D1 * t164 * t14 * t331)
      t348 = t26 * t333
      t355 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t6
     #6)
      t359 = log(0.4D1 * t88 * t108 * t331)
      t361 = t13 * t331
      t365 = log(-0.4D1 * t60 * t361 * x4)
      t380 = log(-0.4D1 * t60 * t361)
      t382 = t380 ** 2
      t385 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t398 = -t31 * t335 * t161 / 0.8D1 + (0.90D2 * t31 * (t339 - t343 *
     # t333) - 0.180D3 * t52 * t348) * t83 * t155 / 0.720D3 + (0.90D2 * 
     #t31 * (-t355 + t359 * t334 + t339 - t365 * t333) + 0.180D3 * t52 *
     # t26 * t335) * t83 * t85 / 0.1440D4 - (-0.90D2 * t31 * (-t380 * t3
     #39 + t382 * t333 / 0.2D1 + t385) + 0.180D3 * t52 * t26 * (t339 - t
     #380 * t333) - t8 * t348) * t83 / 0.1440D4
      t399 = FJET(XB1, XB2, s, t2 * x3, -t2 * t331, 0.0D0, 0.0D0, 0.0D0,
     # t398)
      t401 = KAPPA2(t199, x2, x3, 0.10D1, z)
      t402 = s * t401
      t403 = t203 * x3
      t405 = t203 * t331
      t408 = t401 ** 2
      t414 = 0.1D1 / (-0.2D1 + t401)
      t416 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, x3, 0.10
     #D1)
      t421 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t199, x2, x3, 0.10
     #D1)
      t423 = t408 ** 2
      t428 = log(-0.4D1 * t244 * t245 * t331 * t423)
      t442 = t31 * t414 * t416 * t83 * t240 / 0.8D1 + (0.90D2 * t31 * (t
     #414 * t421 - t428 * t414 * t416) - 0.180D3 * t52 * t26 * t414 * t4
     #16) * t83 * t155 / 0.720D3
      t443 = FJET(XB1, XB2, s, -t402 * t403, t402 * t405, t402 * t205, 0
     #.0D0, s * t408 * t209 * t295 * t331, t442)
      t445 = KAPPA2(t199, x2, x3, t66, z)
      t446 = s * t445
      t451 = t445 ** 2
      t456 = cos(t9)
      t459 = Sqrt(x3 * t331 * t108)
      t466 = 0.1D1 / (-0.2D1 + t445)
      t468 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, x3, t66)
      t470 = t468 * t83 * t240
      t473 = FJET(XB1, XB2, s, -t446 * t403, t446 * t405, -t446 * t288, 
     #t446 * t290, s * t451 * t209 * t295 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t456 * t459), -t31 * t466 * t470 / 0.8D1)
      rrgg2gght5s3e0 = t197 * t196 + t283 * t282 + t328 * t327 + t399 * 
     #t398 + t443 * t442 - t473 * 0.3141592653589793D1 * t26 * t466 * t4
     #70 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t43 = 0.1D1 - x4
      t44 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t43)
      t45 = -t36 + t44
      t46 = 0.1D1 / x3
      t48 = 0.1D1 / x4
      t52 = x3 * t7
      t55 = log(0.4D1 * t52 * t9)
      t62 = 0.180D3 * t3 * t18 * t36
      t66 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t43)
      t67 = -t43
      t71 = log(-0.4D1 * t10 * x4 * t67)
      t75 = log(0.4D1 * t10 * x4)
      t80 = -t45
      t88 = 0.1D1 / x1
      t92 = x1 ** 2
      t93 = t92 * t7
      t96 = log(0.4D1 * t93 * t9)
      t108 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 + t39 * t45 * t46 * t48 / 0.16D2 - (0.90D
     #2 * t39 * (t20 - t55 * t36) - t62) * t46 / 0.1440D4 - (0.90D2 * t3
     #9 * (-t66 + t71 * t44 + t20 - t75 * t36) - 0.180D3 * t3 * t18 * t8
     #0) * t48 / 0.1440D4 - t39 * t36 * t46 * t88 / 0.8D1 + (0.90D2 * t3
     #9 * (-t20 + t96 * t36) + t62) * t88 / 0.720D3 - t39 * t80 * t88 * 
     #t48 / 0.8D1
      t109 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t111 = 0.1D1 - x1
      t112 = KAPPA2(t111, x2, 0.0D0, 0.10D1, z)
      t113 = s * t112
      t114 = -t111
      t115 = t1 * t114
      t117 = t1 * x1
      t119 = t112 ** 2
      t121 = t1 ** 2
      t126 = 0.1D1 / (-0.2D1 + t112)
      t127 = t39 * t126
      t128 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.0D0, 0
     #.10D1)
      t133 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.0D0, 0
     #.10D1)
      t135 = t114 ** 2
      t137 = t119 ** 2
      t141 = log(0.4D1 * t93 * t9 * t135 * t137)
      t158 = -t127 * t128 * t46 * t88 / 0.8D1 + (-0.90D2 * t39 * (t126 *
     # t133 - t141 * t126 * t128) + 0.180D3 * t3 * t18 * t126 * t128) * 
     #t88 / 0.720D3 - t127 * t128 * t88 * t48 / 0.8D1
      t159 = FJET(XB1, XB2, s, 0.0D0, -t113 * t115, t113 * t117, 0.0D0, 
     #-s * t119 * t121 * t114 * x1, t158)
      t161 = KAPPA2(t111, x2, 0.0D0, t43, z)
      t162 = s * t161
      t168 = t161 ** 2
      t171 = t114 * x1
      t175 = 0.1D1 / (-0.2D1 + t161)
      t177 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, 0.0D0, t
     #43)
      t182 = FJET(XB1, XB2, s, 0.0D0, -t162 * t115, -t162 * t117 * t67, 
     #t162 * t117 * x4, s * t168 * t121 * t171 * t67, t39 * t175 * t177 
     #* t88 * t48 / 0.8D1)
      t191 = -0.1D1 + x3
      t193 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t198 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t4
     #3)
      t204 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t208 = log(-0.4D1 * t52 * t9 * t191)
      t219 = t39 * t193 * t46 * t88 / 0.8D1 + t39 * (-t198 + t193) * t46
     # * t48 / 0.16D2 - (-0.90D2 * t39 * (t204 - t208 * t193) + 0.180D3 
     #* t3 * t18 * t193) * t46 / 0.1440D4
      t220 = FJET(XB1, XB2, s, t2 * x3, -t2 * t191, 0.0D0, 0.0D0, 0.0D0,
     # t219)
      t222 = KAPPA2(t111, x2, x3, 0.10D1, z)
      t223 = s * t222
      t229 = t222 ** 2
      t235 = 0.1D1 / (-0.2D1 + t222)
      t237 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t111, x2, x3, 0.10
     #D1)
      t242 = FJET(XB1, XB2, s, -t223 * t115 * x3, t223 * t115 * t191, t2
     #23 * t117, 0.0D0, s * t229 * t121 * t171 * t191, t39 * t235 * t237
     # * t46 * t88 / 0.8D1)
      rrgg2gght5s3em1 = t109 * t108 + t159 * t158 + t182 * 0.31415926535
     #89793D1 * t18 * t175 * t177 * t88 * t48 / 0.8D1 + t220 * t219 + t2
     #42 * 0.3141592653589793D1 * t18 * t235 * t237 * t46 * t88 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t8 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.1D1 - x4)
      t9 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.10D1)
      t15 = 0.1D1 / x3
      t19 = 0.1D1 / x1
      t23 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t29 = sin(x2 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t31 = z ** 2
      t35 = log(0.4D1 * t30 / t31)
      t42 = -t6 * (-t8 + t9) / x4 / 0.16D2 - t6 * t9 * t15 / 0.16D2 - t6
     # * t9 * t19 / 0.8D1 - t6 * t23 / 0.16D2 - (-0.180D3 * 0.3141592653
     #589793D1 * lh - 0.90D2 * t35 * 0.3141592653589793D1) * t5 * t9 / 0
     #.1440D4
      t43 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t42)
      t48 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.1
     #0D1)
      t52 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, t6 * t48 * t15 / 0.16D2)
      t58 = 0.1D1 - x1
      t59 = KAPPA2(t58, x2, 0.0D0, 0.10D1, z)
      t60 = s * t59
      t61 = -t58
      t66 = t59 ** 2
      t68 = t1 ** 2
      t74 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t58, x2, 0.0D0, 0.1
     #0D1)
      t76 = 0.1D1 / (-0.2D1 + t59) * t74 * t19
      t79 = FJET(XB1, XB2, s, 0.0D0, -t60 * t1 * t61, t60 * t1 * x1, 0.0
     #D0, -s * t66 * t68 * t61 * x1, -t6 * t76 / 0.8D1)
      rrgg2gght5s3em2 = t43 * t42 + t52 * 0.3141592653589793D1 * t5 * t4
     #8 * t15 / 0.16D2 - t79 * 0.3141592653589793D1 * t5 * t76 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.10D1)
      t10 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s3em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = 0.3141592653589793D1 * t18
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t84 = -0.1D1 + x4
      t85 = t83 * t84
      t88 = log(-0.4D1 * t82 * t85)
      t89 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t91 = t88 ** 2
      t92 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t95 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t98 = log(0.4D1 * t82 * t83)
      t100 = t98 ** 2
      t112 = t92 - t78
      t114 = t39 * t34 * t112
      t116 = 0.1D1 / x1
      t118 = 0.1D1 / x4
      t121 = t82 * t15
      t123 = log(0.4D1 * t121)
      t128 = t123 ** 2
      t138 = t34 * t78
      t139 = t10 * t138
      t150 = x3 * t81
      t151 = t150 * t13
      t154 = log(-0.4D1 * t151 * t85)
      t156 = t16 * x4
      t159 = log(0.4D1 * t150 * t156)
      t165 = -t34 * t112
      t169 = 0.1D1 / x3
      t171 = t116 * t118
      t174 = t150 * t16
      t176 = log(0.4D1 * t174)
      t178 = t176 ** 2
      t195 = log(0.4D1 * t156)
      t197 = x4 * t84
      t200 = log(-0.4D1 * t16 * t197)
      t205 = t200 ** 2
      t208 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t213 = t195 ** 2
      t237 = x3 * t13
      t240 = log(0.4D1 * t237 * t83)
      t242 = t240 ** 2
      t247 = log(-0.4D1 * t237 * t85)
      t249 = t247 ** 2
      t265 = t237 * t15
      t267 = log(0.4D1 * t265)
      t272 = t267 ** 2
      t292 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t56 * 
     #(-t88 * t89 + t91 * t92 / 0.2D1 + t95 + t98 * t36 - t100 * t78 / 0
     #.2D1 - t45) - 0.180D3 * t48 * t34 * (t89 - t88 * t92 - t36 + t98 *
     # t78) + t114) * t116 * t118 / 0.720D3 - (t39 * t34 * (t36 - t123 *
     # t78) + 0.90D2 * t56 * (t128 * t36 / 0.2D1 + t53 - t128 * t123 * t
     #78 / 0.6D1 - t123 * t45) + t139 - 0.180D3 * t48 * t34 * (-t123 * t
     #36 + t128 * t78 / 0.2D1 + t45)) * t116 / 0.720D3 - (0.90D2 * t56 *
     # (-t89 + t154 * t92 + t36 - t159 * t78) - 0.180D3 * t48 * t165) * 
     #t169 * t171 / 0.720D3 - (0.90D2 * t56 * (-t176 * t36 + t178 * t78 
     #/ 0.2D1 + t45) - 0.180D3 * t48 * t34 * (t36 - t176 * t78) + t39 * 
     #t138) * t169 * t116 / 0.720D3 - (t39 * t34 * (t36 - t195 * t78 - t
     #89 + t200 * t92) + 0.90D2 * t56 * (-t205 * t89 / 0.2D1 - t208 + t2
     #05 * t200 * t92 / 0.6D1 + t200 * t95 + t213 * t36 / 0.2D1 + t53 - 
     #t213 * t195 * t78 / 0.6D1 - t195 * t45) + t10 * t165 - 0.180D3 * t
     #48 * t34 * (-t195 * t36 + t213 * t78 / 0.2D1 + t45 + t200 * t89 - 
     #t205 * t92 / 0.2D1 - t95)) * t118 / 0.1440D4 + (0.90D2 * t56 * (t2
     #40 * t36 - t242 * t78 / 0.2D1 - t45 - t247 * t89 + t249 * t92 / 0.
     #2D1 + t95) - 0.180D3 * t48 * t34 * (-t36 + t240 * t78 + t89 - t247
     # * t92) + t114) * t169 * t118 / 0.1440D4 + (t39 * t34 * (-t36 + t2
     #67 * t78) + 0.90D2 * t56 * (-t272 * t36 / 0.2D1 - t53 + t272 * t26
     #7 * t78 / 0.6D1 + t267 * t45) - t139 - 0.180D3 * t48 * t34 * (t267
     # * t36 - t272 * t78 / 0.2D1 - t45)) * t169 / 0.1440D4
      t293 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t292)
      t295 = -0.1D1 + x1
      t298 = t295 ** 2
      t299 = t81 * t298
      t300 = t299 * x4
      t303 = log(0.4D1 * t16 * t300)
      t304 = -t295
      t305 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, 0
     #.0D0)
      t307 = t303 ** 2
      t308 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, 0
     #.0D0)
      t311 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, 0
     #.0D0)
      t320 = t34 * t308
      t321 = t39 * t320
      t327 = log(0.4D1 * t16 * t299)
      t332 = t327 ** 2
      t335 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, 0
     #.0D0)
      t355 = log(0.4D1 * t265 * t300)
      t369 = log(0.4D1 * t237 * t15 * t81 * t298)
      t371 = t369 ** 2
      t386 = (0.90D2 * t56 * (-t303 * t305 + t307 * t308 / 0.2D1 + t311)
     # - 0.180D3 * t48 * t34 * (t305 - t303 * t308) + t321) * t116 * t11
     #8 / 0.720D3 - (-t39 * t34 * (t305 - t327 * t308) - 0.90D2 * t56 * 
     #(t332 * t305 / 0.2D1 + t335 - t332 * t327 * t308 / 0.6D1 - t327 * 
     #t311) - t10 * t320 + 0.180D3 * t48 * t34 * (-t327 * t305 + t332 * 
     #t308 / 0.2D1 + t311)) * t116 / 0.720D3 - (0.90D2 * t56 * (-t305 + 
     #t355 * t308) + 0.180D3 * t48 * t320) * t169 * t171 / 0.720D3 - (0.
     #90D2 * t56 * (t369 * t305 - t371 * t308 / 0.2D1 - t311) - 0.180D3 
     #* t48 * t34 * (-t305 + t369 * t308) - t321) * t169 * t116 / 0.720D
     #3
      t387 = FJET(XB1, XB2, s, 0.0D0, -t2 * t295, 0.0D0, t2 * x1, 0.0D0,
     # t386)
      t389 = KAPPA2(t304, x2, 0.0D0, x4, z)
      t390 = s * t389
      t391 = t1 * t295
      t393 = t1 * x1
      t394 = t393 * x4
      t396 = t393 * t84
      t398 = t389 ** 2
      t400 = t1 ** 2
      t402 = t295 * x1
      t406 = t398 ** 2
      t408 = t298 * x4 * t84 * t406
      t411 = log(-0.4D1 * t121 * t408)
      t413 = 0.1D1 / (-0.2D1 + t389)
      t414 = t411 * t413
      t415 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, x
     #4)
      t417 = t411 ** 2
      t419 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, x
     #4)
      t422 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.0D0, x
     #4)
      t427 = t413 * t415
      t434 = t34 * t413 * t419
      t441 = log(-0.4D1 * t174 * t408)
      t453 = (0.90D2 * t56 * (-t414 * t415 + t417 * t413 * t419 / 0.2D1 
     #+ t413 * t422) - 0.180D3 * t48 * t34 * (t427 - t414 * t419) + t39 
     #* t434) * t116 * t118 / 0.720D3 - (0.90D2 * t56 * (-t427 + t441 * 
     #t413 * t419) + 0.180D3 * t48 * t434) * t169 * t171 / 0.720D3
      t454 = FJET(XB1, XB2, s, 0.0D0, -t390 * t391, t390 * t394, -t390 *
     # t396, -s * t398 * t400 * t402 * x4, t453)
      t457 = -0.1D1 + x3
      t459 = t15 * t457
      t460 = t459 * x4
      t463 = log(-0.4D1 * t237 * t460)
      t464 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t466 = t463 ** 2
      t467 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t470 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t474 = log(0.4D1 * t265 * t197 * t457)
      t475 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t477 = t474 ** 2
      t478 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t481 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t491 = -t478 + t467
      t500 = log(-0.4D1 * t237 * t459)
      t505 = t500 ** 2
      t508 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t516 = t34 * t467
      t530 = log(-0.4D1 * t151 * t460)
      t536 = log(0.4D1 * t151 * t83 * t84 * t457)
      t552 = log(-0.4D1 * t150 * t16 * t457)
      t554 = t552 ** 2
      t570 = (0.90D2 * t56 * (-t463 * t464 + t466 * t467 / 0.2D1 + t470 
     #+ t474 * t475 - t477 * t478 / 0.2D1 - t481) - 0.180D3 * t48 * t34 
     #* (t464 - t463 * t467 - t475 + t474 * t478) + t39 * t34 * t491) * 
     #t169 * t118 / 0.1440D4 + (t39 * t34 * (t464 - t500 * t467) + 0.90D
     #2 * t56 * (t505 * t464 / 0.2D1 + t508 - t505 * t500 * t467 / 0.6D1
     # - t500 * t470) + t10 * t516 - 0.180D3 * t48 * t34 * (-t500 * t464
     # + t505 * t467 / 0.2D1 + t470)) * t169 / 0.1440D4 - (0.90D2 * t56 
     #* (-t464 + t530 * t467 + t475 - t536 * t478) + 0.180D3 * t48 * t34
     # * t491) * t169 * t171 / 0.720D3 - (0.90D2 * t56 * (t552 * t464 - 
     #t554 * t467 / 0.2D1 - t470) - 0.180D3 * t48 * t34 * (-t464 + t552 
     #* t467) - t39 * t516) * t169 * t116 / 0.720D3
      t571 = FJET(XB1, XB2, s, t2 * x3, -t2 * t457, 0.0D0, 0.0D0, 0.0D0,
     # t570)
      t573 = KAPPA2(t304, x2, x3, 0.0D0, z)
      t574 = s * t573
      t575 = t391 * x3
      t577 = t391 * t457
      t580 = t573 ** 2
      t586 = 0.1D1 / (-0.2D1 + t573)
      t587 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, x3, 0.0D
     #0)
      t588 = t586 * t587
      t589 = t298 * t457
      t590 = t580 ** 2
      t595 = log(-0.4D1 * t174 * t589 * x4 * t590)
      t597 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, x3, 0.0D
     #0)
      t603 = t34 * t586 * t597
      t614 = log(-0.4D1 * t151 * t15 * t298 * t457 * t590)
      t615 = t614 * t586
      t617 = t614 ** 2
      t621 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t304, x2, x3, 0.0D
     #0)
      t636 = -(0.90D2 * t56 * (-t588 + t595 * t586 * t597) + 0.180D3 * t
     #48 * t603) * t169 * t171 / 0.720D3 - (-0.90D2 * t56 * (-t615 * t58
     #7 + t617 * t586 * t597 / 0.2D1 + t586 * t621) + 0.180D3 * t48 * t3
     #4 * (t588 - t615 * t597) - t39 * t603) * t169 * t116 / 0.720D3
      t637 = FJET(XB1, XB2, s, -t574 * t575, t574 * t577, 0.0D0, t574 * 
     #t393, -s * t580 * t400 * t402 * x3, t636)
      t639 = KAPPA2(t304, x2, x3, x4, z)
      t640 = s * t639
      t645 = t639 ** 2
      t650 = cos(t11)
      t653 = Sqrt(x3 * t457 * t197)
      t660 = 0.1D1 / (-0.2D1 + t639)
      t661 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, x3, x4)
      t663 = t645 ** 2
      t668 = log(0.4D1 * t174 * t589 * t197 * t663)
      t670 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, x3, x4)
      t679 = 0.90D2 * t56 * (t661 * t660 - t668 * t660 * t670) - 0.180D3
     # * t48 * t34 * t660 * t670
      t683 = FJET(XB1, XB2, s, -t640 * t575, t640 * t577, t640 * t394, -
     #t640 * t396, s * t645 * t400 * t402 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t650 * t653), -t679 * t169 * t171 / 0.720D3)
      rrgg2gght5s4e1 = t293 * t292 + t387 * t386 + t454 * t453 + t571 * 
     #t570 + t637 * t636 - t683 * t679 * t169 * t116 * t118 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t60 = x3 * t11
      t61 = t13 * x4
      t64 = log(0.4D1 * t60 * t61)
      t66 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t67 = -0.1D1 + x4
      t68 = t61 * t67
      t71 = log(-0.4D1 * t60 * t68)
      t72 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t77 = t72 - t49
      t80 = 0.180D3 * t52 * t26 * t77
      t82 = 0.1D1 / x3
      t84 = 0.1D1 / x4
      t87 = t60 * t13
      t89 = log(0.4D1 * t87)
      t91 = t89 ** 2
      t102 = t26 * t49
      t103 = t8 * t102
      t109 = log(0.4D1 * t14 * x4)
      t111 = t109 ** 2
      t114 = x4 * t67
      t117 = log(-0.4D1 * t14 * t114)
      t119 = t117 ** 2
      t122 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t132 = -t77
      t138 = x1 ** 2
      t139 = t138 * t11
      t142 = log(-0.4D1 * t139 * t68)
      t146 = log(0.4D1 * t139 * t61)
      t152 = 0.1D1 / x1
      t158 = t82 * t152 * t84
      t161 = x3 * t138
      t164 = log(0.4D1 * t161 * t14)
      t175 = t139 * t13
      t177 = log(0.4D1 * t175)
      t179 = t177 ** 2
      t193 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 + (0.90D2 * t31 * (-t28 + t64 * t49 + t66 - t71 * t72) - t8
     #0) * t82 * t84 / 0.1440D4 + (0.90D2 * t31 * (t89 * t28 - t91 * t49
     # / 0.2D1 - t57) - 0.180D3 * t52 * t26 * (-t28 + t89 * t49) - t103)
     # * t82 / 0.1440D4 - (0.90D2 * t31 * (-t109 * t28 + t111 * t49 / 0.
     #2D1 + t57 + t117 * t66 - t119 * t72 / 0.2D1 - t122) - 0.180D3 * t5
     #2 * t26 * (t28 - t109 * t49 - t66 + t117 * t72) + t8 * t26 * t132)
     # * t84 / 0.1440D4 + (0.90D2 * t31 * (t66 - t142 * t72 - t28 + t146
     # * t49) - t80) * t152 * t84 / 0.720D3 - t31 * t132 * t158 / 0.8D1 
     #- (0.90D2 * t31 * (t28 - t164 * t49) - 0.180D3 * t52 * t102) * t82
     # * t152 / 0.720D3 - (0.90D2 * t31 * (-t177 * t28 + t179 * t49 / 0.
     #2D1 + t57) - 0.180D3 * t52 * t26 * (t28 - t177 * t49) + t103) * t1
     #52 / 0.720D3
      t194 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t193)
      t196 = -0.1D1 + x1
      t199 = -t196
      t200 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, 0
     #.0D0)
      t201 = t196 ** 2
      t202 = t138 * t201
      t206 = log(0.4D1 * t14 * t202 * x4)
      t207 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, 0
     #.0D0)
      t212 = t26 * t207
      t214 = 0.180D3 * t52 * t212
      t226 = log(0.4D1 * t60 * t13 * t138 * t201)
      t237 = log(0.4D1 * t14 * t202)
      t239 = t237 ** 2
      t242 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, 0
     #.0D0)
      t255 = (0.90D2 * t31 * (t200 - t206 * t207) - t214) * t152 * t84 /
     # 0.720D3 + t31 * t207 * t158 / 0.8D1 - (0.90D2 * t31 * (-t200 + t2
     #26 * t207) + t214) * t82 * t152 / 0.720D3 - (-0.90D2 * t31 * (-t23
     #7 * t200 + t239 * t207 / 0.2D1 + t242) + 0.180D3 * t52 * t26 * (t2
     #00 - t237 * t207) - t8 * t212) * t152 / 0.720D3
      t256 = FJET(XB1, XB2, s, 0.0D0, -t2 * t196, 0.0D0, t2 * x1, 0.0D0,
     # t255)
      t258 = KAPPA2(t199, x2, 0.0D0, x4, z)
      t259 = s * t258
      t260 = t1 * t196
      t262 = t1 * x1
      t263 = t262 * x4
      t265 = t262 * t67
      t267 = t258 ** 2
      t269 = t1 ** 2
      t271 = t196 * x1
      t275 = 0.1D1 / (-0.2D1 + t258)
      t276 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, x
     #4)
      t279 = t267 ** 2
      t284 = log(-0.4D1 * t175 * t201 * x4 * t67 * t279)
      t286 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, 0.0D0, x
     #4)
      t301 = t152 * t84
      t305 = (0.90D2 * t31 * (t275 * t276 - t284 * t275 * t286) - 0.180D
     #3 * t52 * t26 * t275 * t286) * t152 * t84 / 0.720D3 + t31 * t275 *
     # t286 * t82 * t301 / 0.8D1
      t306 = FJET(XB1, XB2, s, 0.0D0, -t259 * t260, t259 * t263, -t259 *
     # t265, -s * t267 * t269 * t271 * x4, t305)
      t309 = -0.1D1 + x3
      t311 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t312 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t313 = -t311 + t312
      t317 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t321 = log(-0.4D1 * t161 * t14 * t309)
      t326 = t26 * t311
      t333 = t13 * t309
      t337 = log(-0.4D1 * t60 * t333 * x4)
      t339 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t343 = log(0.4D1 * t87 * t114 * t309)
      t358 = log(-0.4D1 * t60 * t333)
      t360 = t358 ** 2
      t363 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t376 = -t31 * t313 * t158 / 0.8D1 - (0.90D2 * t31 * (-t317 + t321 
     #* t311) + 0.180D3 * t52 * t326) * t82 * t152 / 0.720D3 + (0.90D2 *
     # t31 * (t317 - t337 * t311 - t339 + t343 * t312) + 0.180D3 * t52 *
     # t26 * t313) * t82 * t84 / 0.1440D4 + (0.90D2 * t31 * (-t358 * t31
     #7 + t360 * t311 / 0.2D1 + t363) - 0.180D3 * t52 * t26 * (t317 - t3
     #58 * t311) + t8 * t326) * t82 / 0.1440D4
      t377 = FJET(XB1, XB2, s, t2 * x3, -t2 * t309, 0.0D0, 0.0D0, 0.0D0,
     # t376)
      t379 = KAPPA2(t199, x2, x3, 0.0D0, z)
      t380 = s * t379
      t381 = t260 * x3
      t383 = t260 * t309
      t386 = t379 ** 2
      t392 = 0.1D1 / (-0.2D1 + t379)
      t394 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, x3, 0.0D
     #0)
      t399 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t199, x2, x3, 0.0D
     #0)
      t403 = t386 ** 2
      t408 = log(-0.4D1 * t161 * t11 * t13 * t201 * t309 * t403)
      t422 = t31 * t392 * t394 * t82 * t301 / 0.8D1 - (-0.90D2 * t31 * (
     #t392 * t399 - t408 * t392 * t394) + 0.180D3 * t52 * t26 * t392 * t
     #394) * t82 * t152 / 0.720D3
      t423 = FJET(XB1, XB2, s, -t380 * t381, t380 * t383, 0.0D0, t380 * 
     #t262, -s * t386 * t269 * t271 * x3, t422)
      t425 = KAPPA2(t199, x2, x3, x4, z)
      t426 = s * t425
      t431 = t425 ** 2
      t436 = cos(t9)
      t439 = Sqrt(x3 * t309 * t114)
      t446 = 0.1D1 / (-0.2D1 + t425)
      t448 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t199, x2, x3, x4)
      t450 = t448 * t82 * t301
      t453 = FJET(XB1, XB2, s, -t426 * t381, t426 * t383, t426 * t263, -
     #t426 * t265, s * t431 * t269 * t271 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t436 * t439), -t31 * t446 * t450 / 0.8D1)
      rrgg2gght5s4e0 = t194 * t193 + t256 * t255 + t306 * t305 + t377 * 
     #t376 + t423 * t422 - t453 * 0.3141592653589793D1 * t26 * t446 * t4
     #50 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t43 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t44 = t43 - t36
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x4
      t51 = x3 * t7
      t54 = log(0.4D1 * t51 * t9)
      t61 = 0.180D3 * t3 * t18 * t36
      t67 = log(0.4D1 * t10 * x4)
      t69 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t70 = -0.1D1 + x4
      t74 = log(-0.4D1 * t10 * x4 * t70)
      t87 = 0.1D1 / x1
      t91 = x1 ** 2
      t95 = log(0.4D1 * t91 * t7 * t9)
      t107 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 + t39 * t44 * t45 * t47 / 0.16D2 + (0.90D
     #2 * t39 * (-t20 + t54 * t36) + t61) * t45 / 0.1440D4 - (0.90D2 * t
     #39 * (t20 - t67 * t36 - t69 + t74 * t43) + 0.180D3 * t3 * t18 * t4
     #4) * t47 / 0.1440D4 - t39 * t36 * t45 * t87 / 0.8D1 - (0.90D2 * t3
     #9 * (t20 - t95 * t36) - t61) * t87 / 0.720D3 + t39 * t44 * t87 * t
     #47 / 0.8D1
      t108 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t107)
      t110 = -0.1D1 + x1
      t113 = -t110
      t114 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t113, x2, 0.0D0, 0
     #.0D0)
      t119 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t113, x2, 0.0D0, 0
     #.0D0)
      t120 = t110 ** 2
      t124 = log(0.4D1 * t10 * t91 * t120)
      t139 = t39 * t114 * t45 * t87 / 0.8D1 - (-0.90D2 * t39 * (t119 - t
     #124 * t114) + 0.180D3 * t3 * t18 * t114) * t87 / 0.720D3 + t39 * t
     #114 * t87 * t47 / 0.8D1
      t140 = FJET(XB1, XB2, s, 0.0D0, -t2 * t110, 0.0D0, t2 * x1, 0.0D0,
     # t139)
      t142 = KAPPA2(t113, x2, 0.0D0, x4, z)
      t143 = s * t142
      t144 = t1 * t110
      t146 = t1 * x1
      t151 = t142 ** 2
      t153 = t1 ** 2
      t155 = t110 * x1
      t159 = 0.1D1 / (-0.2D1 + t142)
      t161 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t113, x2, 0.0D0, x
     #4)
      t166 = FJET(XB1, XB2, s, 0.0D0, -t143 * t144, t143 * t146 * x4, -t
     #143 * t146 * t70, -s * t151 * t153 * t155 * x4, t39 * t159 * t161 
     #* t87 * t47 / 0.8D1)
      t175 = -0.1D1 + x3
      t177 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t182 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t188 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t192 = log(-0.4D1 * t51 * t9 * t175)
      t203 = t39 * t177 * t45 * t87 / 0.8D1 + t39 * (-t182 + t177) * t45
     # * t47 / 0.16D2 + (0.90D2 * t39 * (t188 - t192 * t177) - 0.180D3 *
     # t3 * t18 * t177) * t45 / 0.1440D4
      t204 = FJET(XB1, XB2, s, t2 * x3, -t2 * t175, 0.0D0, 0.0D0, 0.0D0,
     # t203)
      t206 = KAPPA2(t113, x2, x3, 0.0D0, z)
      t207 = s * t206
      t213 = t206 ** 2
      t219 = 0.1D1 / (-0.2D1 + t206)
      t221 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t113, x2, x3, 0.0D
     #0)
      t226 = FJET(XB1, XB2, s, -t207 * t144 * x3, t207 * t144 * t175, 0.
     #0D0, t207 * t146, -s * t213 * t153 * t155 * x3, t39 * t219 * t221 
     #* t45 * t87 / 0.8D1)
      rrgg2gght5s4em1 = t108 * t107 + t140 * t139 + t166 * 0.31415926535
     #89793D1 * t18 * t159 * t161 * t87 * t47 / 0.8D1 + t204 * t203 + t2
     #26 * 0.3141592653589793D1 * t18 * t219 * t221 * t45 * t87 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t8 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, x
     #4)
      t14 = 0.1D1 / x3
      t18 = 0.1D1 / x1
      t22 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t28 = sin(x2 * 0.3141592653589793D1)
      t29 = t28 ** 2
      t30 = z ** 2
      t34 = log(0.4D1 * t29 / t30)
      t41 = -t6 * (t7 - t8) / x4 / 0.16D2 - t6 * t7 * t14 / 0.16D2 - t6 
     #* t7 * t18 / 0.8D1 - t6 * t22 / 0.16D2 - (-0.180D3 * 0.31415926535
     #89793D1 * lh - 0.90D2 * t34 * 0.3141592653589793D1) * t5 * t7 / 0.
     #1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t47 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.0
     #D0)
      t51 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, t6 * t47 * t14 / 0.16D2)
      t57 = -0.1D1 + x1
      t61 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, -t57, x2, 0.0D0, 0.
     #0D0)
      t65 = FJET(XB1, XB2, s, 0.0D0, -t2 * t57, 0.0D0, t2 * x1, 0.0D0, t
     #6 * t61 * t18 / 0.8D1)
      rrgg2gght5s4em2 = t42 * t41 + t51 * 0.3141592653589793D1 * t5 * t4
     #7 * t14 / 0.16D2 + t65 * 0.3141592653589793D1 * t5 * t61 * t18 / 0
     #.8D1

      end function



      doubleprecision function rrgg2gght5s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s4em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s4em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s5e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = 0.3141592653589793D1 * t18
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t56 = t34 * 0.3141592653589793D1
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t86 = log(0.4D1 * t82 * t83)
      t88 = t86 ** 2
      t99 = t34 * t78
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t82 * t15
      t109 = log(0.4D1 * t107)
      t114 = t109 ** 2
      t124 = t10 * t99
      t135 = x3 * t81
      t136 = t16 * x4
      t139 = log(0.4D1 * t135 * t136)
      t141 = 0.1D1 - x3
      t142 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.10D1)
      t143 = t135 * t13
      t144 = -t141
      t145 = t15 * t144
      t146 = t145 * x4
      t149 = log(-0.4D1 * t143 * t146)
      t150 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.10D1)
      t155 = t150 - t78
      t156 = t34 * t155
      t160 = 0.1D1 / x3
      t162 = t102 * t104
      t165 = t135 * t16
      t167 = log(0.4D1 * t165)
      t169 = t167 ** 2
      t175 = log(-0.4D1 * t135 * t16 * t144)
      t177 = t175 ** 2
      t180 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.10D1)
      t190 = t39 * t156
      t196 = log(0.4D1 * t136)
      t201 = t196 ** 2
      t221 = x3 * t13
      t224 = log(-0.4D1 * t221 * t146)
      t226 = t224 ** 2
      t231 = log(0.4D1 * t221 * t83)
      t233 = t231 ** 2
      t251 = log(-0.4D1 * t221 * t145)
      t253 = t221 * t15
      t255 = log(0.4D1 * t253)
      t260 = t255 ** 2
      t267 = t251 ** 2
      t270 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.10D1)
      t294 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 - (0.90D2 * t56 * 
     #(-t86 * t36 + t88 * t78 / 0.2D1 + t45) - 0.180D3 * t48 * t34 * (t3
     #6 - t86 * t78) + t39 * t99) * t102 * t104 / 0.720D3 - (t39 * t34 *
     # (t36 - t109 * t78) + 0.90D2 * t56 * (t114 * t36 / 0.2D1 + t53 - t
     #114 * t109 * t78 / 0.6D1 - t109 * t45) + t124 - 0.180D3 * t48 * t3
     #4 * (-t109 * t36 + t114 * t78 / 0.2D1 + t45)) * t102 / 0.720D3 + (
     #0.90D2 * t56 * (-t36 + t139 * t78 + t142 - t149 * t150) - 0.180D3 
     #* t48 * t156) * t160 * t162 / 0.720D3 + (0.90D2 * t56 * (t167 * t3
     #6 - t169 * t78 / 0.2D1 - t45 - t175 * t142 + t177 * t150 / 0.2D1 +
     # t180) - 0.180D3 * t48 * t34 * (-t36 + t167 * t78 + t142 - t175 * 
     #t150) + t190) * t160 * t102 / 0.720D3 + (t39 * t34 * (-t36 + t196 
     #* t78) + 0.90D2 * t56 * (-t201 * t36 / 0.2D1 - t53 + t201 * t196 *
     # t78 / 0.6D1 + t196 * t45) - t124 - 0.180D3 * t48 * t34 * (t196 * 
     #t36 - t201 * t78 / 0.2D1 - t45)) * t104 / 0.1440D4 + (0.90D2 * t56
     # * (-t224 * t142 + t226 * t150 / 0.2D1 + t180 + t231 * t36 - t233 
     #* t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (t142 - t224 * t150 -
     # t36 + t231 * t78) + t190) * t160 * t104 / 0.1440D4 - (t39 * t34 *
     # (-t142 + t251 * t150 + t36 - t255 * t78) + 0.90D2 * t56 * (t260 *
     # t36 / 0.2D1 + t53 - t260 * t255 * t78 / 0.6D1 - t255 * t45 - t267
     # * t142 / 0.2D1 - t270 + t267 * t251 * t150 / 0.6D1 + t251 * t180)
     # - t10 * t34 * t155 - 0.180D3 * t48 * t34 * (t251 * t142 - t267 * 
     #t150 / 0.2D1 - t180 - t255 * t36 + t260 * t78 / 0.2D1 + t45)) * t1
     #60 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t294)
      t297 = -0.1D1 + x4
      t300 = -t297
      t301 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t300)
      t302 = x4 * t297
      t305 = log(-0.4D1 * t16 * t302)
      t306 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t300)
      t311 = t305 ** 2
      t314 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t300)
      t318 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t300)
      t323 = t34 * t306
      t335 = t83 * t297
      t338 = log(-0.4D1 * t221 * t335)
      t340 = t338 ** 2
      t346 = log(0.4D1 * t253 * t302 * t144)
      t347 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, t
     #300)
      t349 = t346 ** 2
      t350 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, t
     #300)
      t353 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, t
     #300)
      t364 = t34 * (t306 - t350)
      t372 = log(-0.4D1 * t82 * t335)
      t374 = t372 ** 2
      t394 = log(0.4D1 * t143 * t83 * t297 * t144)
      t398 = log(-0.4D1 * t143 * t335)
      t409 = (t39 * t34 * (t301 - t305 * t306) + 0.90D2 * t56 * (t311 * 
     #t301 / 0.2D1 + t314 - t311 * t305 * t306 / 0.6D1 - t305 * t318) + 
     #t10 * t323 - 0.180D3 * t48 * t34 * (-t305 * t301 + t311 * t306 / 0
     #.2D1 + t318)) * t104 / 0.1440D4 + (0.90D2 * t56 * (-t338 * t301 + 
     #t340 * t306 / 0.2D1 + t318 + t346 * t347 - t349 * t350 / 0.2D1 - t
     #353) - 0.180D3 * t48 * t34 * (t301 - t338 * t306 - t347 + t346 * t
     #350) + t39 * t364) * t160 * t104 / 0.1440D4 - (0.90D2 * t56 * (t37
     #2 * t301 - t374 * t306 / 0.2D1 - t318) - 0.180D3 * t48 * t34 * (-t
     #301 + t372 * t306) - t39 * t323) * t102 * t104 / 0.720D3 + (0.90D2
     # * t56 * (-t347 + t394 * t350 + t301 - t398 * t306) - 0.180D3 * t4
     #8 * t364) * t160 * t162 / 0.720D3
      t410 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t297, t2 * x4, 0.0D0,
     # t409)
      t413 = -0.1D1 + x1
      t415 = t413 ** 2
      t416 = t81 * t415
      t417 = t416 * x4
      t420 = log(0.4D1 * t16 * t417)
      t421 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t423 = t420 ** 2
      t424 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t427 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t436 = t34 * t424
      t437 = t39 * t436
      t443 = log(0.4D1 * t16 * t416)
      t448 = t443 ** 2
      t451 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t471 = log(0.4D1 * t253 * t417)
      t485 = log(0.4D1 * t221 * t15 * t81 * t415)
      t487 = t485 ** 2
      t502 = -(0.90D2 * t56 * (t420 * t421 - t423 * t424 / 0.2D1 - t427)
     # - 0.180D3 * t48 * t34 * (-t421 + t420 * t424) - t437) * t102 * t1
     #04 / 0.720D3 - (-t39 * t34 * (t421 - t443 * t424) - 0.90D2 * t56 *
     # (t448 * t421 / 0.2D1 + t451 - t448 * t443 * t424 / 0.6D1 - t443 *
     # t427) - t10 * t436 + 0.180D3 * t48 * t34 * (-t443 * t421 + t448 *
     # t424 / 0.2D1 + t427)) * t102 / 0.720D3 + (0.90D2 * t56 * (t421 - 
     #t471 * t424) - 0.180D3 * t48 * t436) * t160 * t162 / 0.720D3 + (0.
     #90D2 * t56 * (-t485 * t421 + t487 * t424 / 0.2D1 + t427) - 0.180D3
     # * t48 * t34 * (t421 - t485 * t424) + t437) * t160 * t102 / 0.720D
     #3
      t503 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t413, 0.0D0, 0.0D0,
     # t502)
      t505 = KAPPA2(x1, x2, 0.10D1, t300, z)
      t506 = s * t505
      t507 = t1 * x1
      t509 = t1 * t413
      t510 = t509 * t297
      t512 = t509 * x4
      t514 = t505 ** 2
      t516 = t1 ** 2
      t518 = x1 * t413
      t522 = t514 ** 2
      t524 = t415 * x4 * t297 * t522
      t527 = log(-0.4D1 * t107 * t524)
      t529 = 0.1D1 / (-0.2D1 + t505)
      t530 = t527 * t529
      t531 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #00)
      t533 = t527 ** 2
      t535 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #00)
      t538 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #00)
      t543 = t529 * t531
      t550 = t34 * t529 * t535
      t557 = log(-0.4D1 * t165 * t524)
      t569 = -(0.90D2 * t56 * (t530 * t531 - t533 * t529 * t535 / 0.2D1 
     #- t529 * t538) - 0.180D3 * t48 * t34 * (-t543 + t530 * t535) - t39
     # * t550) * t102 * t104 / 0.720D3 + (0.90D2 * t56 * (t543 - t557 * 
     #t529 * t535) - 0.180D3 * t48 * t550) * t160 * t162 / 0.720D3
      t570 = FJET(XB1, XB2, s, t506 * t507, 0.0D0, t506 * t510, -t506 * 
     #t512, -s * t514 * t516 * t518 * x4, t569)
      t572 = KAPPA2(x1, x2, t141, 0.10D1, z)
      t573 = s * t572
      t574 = t507 * t144
      t576 = t507 * x3
      t579 = t572 ** 2
      t585 = 0.1D1 / (-0.2D1 + t572)
      t586 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, 0.10
     #D1)
      t587 = t585 * t586
      t588 = t415 * t144
      t589 = t579 ** 2
      t594 = log(-0.4D1 * t165 * t588 * x4 * t589)
      t596 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, 0.10
     #D1)
      t602 = t34 * t585 * t596
      t613 = log(-0.4D1 * t143 * t15 * t415 * t144 * t589)
      t614 = t613 * t585
      t616 = t613 ** 2
      t620 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, 0.10
     #D1)
      t635 = (0.90D2 * t56 * (t587 - t594 * t585 * t596) - 0.180D3 * t48
     # * t602) * t160 * t162 / 0.720D3 + (0.90D2 * t56 * (-t614 * t586 +
     # t616 * t585 * t596 / 0.2D1 + t585 * t620) - 0.180D3 * t48 * t34 *
     # (t587 - t614 * t596) + t39 * t602) * t160 * t102 / 0.720D3
      t636 = FJET(XB1, XB2, s, -t573 * t574, t573 * t576, -t573 * t509, 
     #0.0D0, -s * t579 * t516 * t518 * x3, t635)
      t638 = KAPPA2(x1, x2, t141, t300, z)
      t639 = s * t638
      t644 = t638 ** 2
      t649 = cos(t11)
      t652 = Sqrt(x3 * t144 * t302)
      t659 = 0.1D1 / (-0.2D1 + t638)
      t660 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, t300
     #)
      t662 = t644 ** 2
      t667 = log(0.4D1 * t165 * t588 * t302 * t662)
      t669 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, t300
     #)
      t678 = -0.90D2 * t56 * (t659 * t660 - t667 * t659 * t669) + 0.180D
     #3 * t48 * t34 * t659 * t669
      t682 = FJET(XB1, XB2, s, -t639 * t574, t639 * t576, t639 * t510, -
     #t639 * t512, s * t644 * t516 * t518 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t649 * t652), t678 * t160 * t162 / 0.720D3)
      rrgg2gght5s5e1 = t295 * t294 + t410 * t409 + t503 * t502 + t570 * 
     #t569 + t636 * t635 + t682 * t678 * t160 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s5e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t60 = 0.1D1 - x3
      t61 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t60, 0.1
     #0D1)
      t62 = x3 * t11
      t63 = -t60
      t64 = t13 * t63
      t68 = log(-0.4D1 * t62 * t64 * x4)
      t69 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t60, 0.1
     #0D1)
      t71 = t13 * x4
      t74 = log(0.4D1 * t62 * t71)
      t79 = t69 - t49
      t82 = 0.180D3 * t52 * t26 * t79
      t84 = 0.1D1 / x3
      t86 = 0.1D1 / x4
      t91 = log(-0.4D1 * t62 * t64)
      t93 = t91 ** 2
      t96 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t60, 0.1
     #0D1)
      t97 = t62 * t13
      t99 = log(0.4D1 * t97)
      t101 = t99 ** 2
      t121 = log(0.4D1 * t14 * x4)
      t123 = t121 ** 2
      t134 = t26 * t49
      t135 = t8 * t134
      t139 = x1 ** 2
      t140 = t139 * t11
      t143 = log(0.4D1 * t140 * t71)
      t151 = 0.1D1 / x1
      t157 = t84 * t151 * t86
      t160 = x3 * t139
      t163 = log(0.4D1 * t160 * t14)
      t168 = log(-0.4D1 * t160 * t14 * t63)
      t177 = t140 * t13
      t179 = log(0.4D1 * t177)
      t181 = t179 ** 2
      t195 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 + (0.90D2 * t31 * (t61 - t68 * t69 - t28 + t74 * t49) - t82
     #) * t84 * t86 / 0.1440D4 - (0.90D2 * t31 * (t91 * t61 - t93 * t69 
     #/ 0.2D1 - t96 - t99 * t28 + t101 * t49 / 0.2D1 + t57) - 0.180D3 * 
     #t52 * t26 * (-t61 + t91 * t69 + t28 - t99 * t49) - t8 * t26 * t79)
     # * t84 / 0.1440D4 + (0.90D2 * t31 * (t121 * t28 - t123 * t49 / 0.2
     #D1 - t57) - 0.180D3 * t52 * t26 * (-t28 + t121 * t49) - t135) * t8
     #6 / 0.1440D4 - (0.90D2 * t31 * (t28 - t143 * t49) - 0.180D3 * t52 
     #* t134) * t151 * t86 / 0.720D3 + t31 * t79 * t157 / 0.8D1 + (0.90D
     #2 * t31 * (-t28 + t163 * t49 + t61 - t168 * t69) - t82) * t84 * t1
     #51 / 0.720D3 - (0.90D2 * t31 * (-t179 * t28 + t181 * t49 / 0.2D1 +
     # t57) - 0.180D3 * t52 * t26 * (t28 - t179 * t49) + t135) * t151 / 
     #0.720D3
      t196 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t195)
      t198 = -0.1D1 + x4
      t201 = x4 * t198
      t204 = log(-0.4D1 * t14 * t201)
      t205 = -t198
      t206 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t205)
      t208 = t204 ** 2
      t209 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t205)
      t212 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t205)
      t221 = t26 * t209
      t226 = t71 * t198
      t229 = log(-0.4D1 * t140 * t226)
      t240 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t60, t2
     #05)
      t241 = t209 - t240
      t247 = log(-0.4D1 * t62 * t226)
      t249 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t60, t2
     #05)
      t253 = log(0.4D1 * t97 * t201 * t63)
      t265 = (0.90D2 * t31 * (-t204 * t206 + t208 * t209 / 0.2D1 + t212)
     # - 0.180D3 * t52 * t26 * (t206 - t204 * t209) + t8 * t221) * t86 /
     # 0.1440D4 - (0.90D2 * t31 * (-t206 + t229 * t209) + 0.180D3 * t52 
     #* t221) * t151 * t86 / 0.720D3 + t31 * t241 * t157 / 0.8D1 + (0.90
     #D2 * t31 * (t206 - t247 * t209 - t249 + t253 * t240) - 0.180D3 * t
     #52 * t26 * t241) * t84 * t86 / 0.1440D4
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t198, t2 * x4, 0.0D0,
     # t265)
      t269 = -0.1D1 + x1
      t271 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t272 = t269 ** 2
      t273 = t139 * t272
      t277 = log(0.4D1 * t14 * t273 * x4)
      t278 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t283 = t26 * t278
      t285 = 0.180D3 * t52 * t283
      t297 = log(0.4D1 * t62 * t13 * t139 * t272)
      t308 = log(0.4D1 * t14 * t273)
      t310 = t308 ** 2
      t313 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t326 = -(0.90D2 * t31 * (-t271 + t277 * t278) + t285) * t151 * t86
     # / 0.720D3 + t31 * t278 * t157 / 0.8D1 + (0.90D2 * t31 * (t271 - t
     #297 * t278) - t285) * t84 * t151 / 0.720D3 - (-0.90D2 * t31 * (-t3
     #08 * t271 + t310 * t278 / 0.2D1 + t313) + 0.180D3 * t52 * t26 * (t
     #271 - t308 * t278) - t8 * t283) * t151 / 0.720D3
      t327 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t269, 0.0D0, 0.0D0,
     # t326)
      t329 = KAPPA2(x1, x2, 0.10D1, t205, z)
      t330 = s * t329
      t331 = t1 * x1
      t333 = t1 * t269
      t334 = t333 * t198
      t336 = t333 * x4
      t338 = t329 ** 2
      t340 = t1 ** 2
      t342 = x1 * t269
      t346 = 0.1D1 / (-0.2D1 + t329)
      t347 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #05)
      t350 = t338 ** 2
      t355 = log(-0.4D1 * t177 * t272 * x4 * t198 * t350)
      t357 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #05)
      t372 = t151 * t86
      t376 = -(0.90D2 * t31 * (-t346 * t347 + t355 * t346 * t357) + 0.18
     #0D3 * t52 * t26 * t346 * t357) * t151 * t86 / 0.720D3 + t31 * t346
     # * t357 * t84 * t372 / 0.8D1
      t377 = FJET(XB1, XB2, s, t330 * t331, 0.0D0, t330 * t334, -t330 * 
     #t336, -s * t338 * t340 * t342 * x4, t376)
      t379 = KAPPA2(x1, x2, t60, 0.10D1, z)
      t380 = s * t379
      t381 = t331 * t63
      t383 = t331 * x3
      t386 = t379 ** 2
      t392 = 0.1D1 / (-0.2D1 + t379)
      t394 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t60, 0.10D
     #1)
      t399 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t60, 0.10D
     #1)
      t403 = t386 ** 2
      t408 = log(-0.4D1 * t160 * t11 * t13 * t272 * t63 * t403)
      t422 = t31 * t392 * t394 * t84 * t372 / 0.8D1 + (0.90D2 * t31 * (t
     #392 * t399 - t408 * t392 * t394) - 0.180D3 * t52 * t26 * t392 * t3
     #94) * t84 * t151 / 0.720D3
      t423 = FJET(XB1, XB2, s, -t380 * t381, t380 * t383, -t380 * t333, 
     #0.0D0, -s * t386 * t340 * t342 * x3, t422)
      t425 = KAPPA2(x1, x2, t60, t205, z)
      t426 = s * t425
      t431 = t425 ** 2
      t436 = cos(t9)
      t439 = Sqrt(x3 * t63 * t201)
      t446 = 0.1D1 / (-0.2D1 + t425)
      t448 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t60, t205)
      t450 = t448 * t84 * t372
      t453 = FJET(XB1, XB2, s, -t426 * t381, t426 * t383, t426 * t334, -
     #t426 * t336, s * t431 * t340 * t342 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t436 * t439), -t31 * t446 * t450 / 0.8D1)
      rrgg2gght5s5e0 = t196 * t195 + t266 * t265 + t327 * t326 + t377 * 
     #t376 + t423 * t422 - t453 * 0.3141592653589793D1 * t26 * t446 * t4
     #50 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s5em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t43 = 0.1D1 - x3
      t44 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t43, 0.1
     #0D1)
      t45 = t44 - t36
      t46 = 0.1D1 / x3
      t47 = t45 * t46
      t48 = 0.1D1 / x4
      t52 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t43, 0.1
     #0D1)
      t53 = x3 * t7
      t54 = -t43
      t58 = log(-0.4D1 * t53 * t9 * t54)
      t62 = log(0.4D1 * t53 * t9)
      t76 = log(0.4D1 * t10 * x4)
      t83 = 0.180D3 * t3 * t18 * t36
      t87 = 0.1D1 / x1
      t91 = x1 ** 2
      t95 = log(0.4D1 * t91 * t7 * t9)
      t107 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 + t39 * t47 * t48 / 0.16D2 - (0.90D2 * t3
     #9 * (-t52 + t58 * t44 + t20 - t62 * t36) + 0.180D3 * t3 * t18 * t4
     #5) * t46 / 0.1440D4 + (0.90D2 * t39 * (-t20 + t76 * t36) + t83) * 
     #t48 / 0.1440D4 + t39 * t47 * t87 / 0.8D1 - (0.90D2 * t39 * (t20 - 
     #t95 * t36) - t83) * t87 / 0.720D3 - t39 * t36 * t87 * t48 / 0.8D1
      t108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t107)
      t110 = -0.1D1 + x4
      t113 = -t110
      t114 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t113)
      t118 = log(-0.4D1 * t10 * x4 * t110)
      t119 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t113)
      t134 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t43, t1
     #13)
      t140 = (0.90D2 * t39 * (t114 - t118 * t119) - 0.180D3 * t3 * t18 *
     # t119) * t48 / 0.1440D4 + t39 * t119 * t87 * t48 / 0.8D1 + t39 * (
     #t119 - t134) * t46 * t48 / 0.16D2
      t141 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t110, t2 * x4, 0.0D0,
     # t140)
      t144 = -0.1D1 + x1
      t146 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t151 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t152 = t144 ** 2
      t156 = log(0.4D1 * t10 * t91 * t152)
      t171 = t39 * t146 * t46 * t87 / 0.8D1 - (-0.90D2 * t39 * (t151 - t
     #156 * t146) + 0.180D3 * t3 * t18 * t146) * t87 / 0.720D3 + t39 * t
     #146 * t87 * t48 / 0.8D1
      t172 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t144, 0.0D0, 0.0D0,
     # t171)
      t174 = KAPPA2(x1, x2, 0.10D1, t113, z)
      t175 = s * t174
      t176 = t1 * x1
      t178 = t1 * t144
      t183 = t174 ** 2
      t185 = t1 ** 2
      t187 = x1 * t144
      t191 = 0.1D1 / (-0.2D1 + t174)
      t193 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t1
     #13)
      t198 = FJET(XB1, XB2, s, t175 * t176, 0.0D0, t175 * t178 * t110, -
     #t175 * t178 * x4, -s * t183 * t185 * t187 * x4, t39 * t191 * t193 
     #* t87 * t48 / 0.8D1)
      t206 = KAPPA2(x1, x2, t43, 0.10D1, z)
      t207 = s * t206
      t213 = t206 ** 2
      t219 = 0.1D1 / (-0.2D1 + t206)
      t221 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t43, 0.10D
     #1)
      t226 = FJET(XB1, XB2, s, -t207 * t176 * t54, t207 * t176 * x3, -t2
     #07 * t178, 0.0D0, -s * t213 * t185 * t187 * x3, t39 * t219 * t221 
     #* t46 * t87 / 0.8D1)
      rrgg2gght5s5em1 = t108 * t107 + t141 * t140 + t172 * t171 + t198 *
     # 0.3141592653589793D1 * t18 * t191 * t193 * t87 * t48 / 0.8D1 + t2
     #26 * 0.3141592653589793D1 * t18 * t219 * t221 * t46 * t87 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s5em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t8 = 0.1D1 / x4
      t13 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.10D1)
      t19 = 0.1D1 / x1
      t23 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t29 = sin(x2 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t31 = z ** 2
      t35 = log(0.4D1 * t30 / t31)
      t42 = -t6 * t7 * t8 / 0.16D2 - t6 * (-t13 + t7) / x3 / 0.16D2 - t6
     # * t7 * t19 / 0.8D1 - t6 * t23 / 0.16D2 - (-0.180D3 * 0.3141592653
     #589793D1 * lh - 0.90D2 * t35 * 0.3141592653589793D1) * t5 * t7 / 0
     #.1440D4
      t43 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t42)
      t45 = -0.1D1 + x4
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #-t45)
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t45, t2 * x4, 0.0D0, t
     #6 * t49 * t8 / 0.16D2)
      t62 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.1
     #0D1)
      t66 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, t6 * t62 * t19 / 0.8D1)
      rrgg2gght5s5em2 = t43 * t42 + t53 * 0.3141592653589793D1 * t5 * t4
     #9 * t8 / 0.16D2 + t66 * 0.3141592653589793D1 * t5 * t62 * t19 / 0.
     #8D1

      end function



      doubleprecision function rrgg2gght5s5em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s5em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s5em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s5em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s6e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = 0.3141592653589793D1 * t18
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t56 = t34 * 0.3141592653589793D1
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t86 = log(0.4D1 * t82 * t83)
      t88 = t86 ** 2
      t99 = t34 * t78
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t82 * t15
      t109 = log(0.4D1 * t107)
      t114 = t109 ** 2
      t124 = t10 * t99
      t135 = x3 * t81
      t136 = t16 * x4
      t139 = log(0.4D1 * t135 * t136)
      t141 = 0.1D1 - x3
      t142 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.0D0)
      t143 = t135 * t13
      t144 = -t141
      t145 = t15 * t144
      t146 = t145 * x4
      t149 = log(-0.4D1 * t143 * t146)
      t150 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.0D0)
      t155 = t150 - t78
      t156 = t34 * t155
      t160 = 0.1D1 / x3
      t162 = t102 * t104
      t168 = log(-0.4D1 * t135 * t16 * t144)
      t170 = t168 ** 2
      t173 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.0D0)
      t174 = t135 * t16
      t176 = log(0.4D1 * t174)
      t178 = t176 ** 2
      t190 = t39 * t156
      t196 = log(0.4D1 * t136)
      t201 = t196 ** 2
      t221 = x3 * t13
      t224 = log(0.4D1 * t221 * t83)
      t226 = t224 ** 2
      t231 = log(-0.4D1 * t221 * t146)
      t233 = t231 ** 2
      t249 = t221 * t15
      t251 = log(0.4D1 * t249)
      t255 = log(-0.4D1 * t221 * t145)
      t260 = t255 ** 2
      t263 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, 0
     #.0D0)
      t268 = t251 ** 2
      t294 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t56 * 
     #(t86 * t36 - t88 * t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (-t3
     #6 + t86 * t78) - t39 * t99) * t102 * t104 / 0.720D3 + (t39 * t34 *
     # (-t36 + t109 * t78) + 0.90D2 * t56 * (-t114 * t36 / 0.2D1 - t53 +
     # t114 * t109 * t78 / 0.6D1 + t109 * t45) - t124 - 0.180D3 * t48 * 
     #t34 * (t109 * t36 - t114 * t78 / 0.2D1 - t45)) * t102 / 0.720D3 + 
     #(0.90D2 * t56 * (-t36 + t139 * t78 + t142 - t149 * t150) - 0.180D3
     # * t48 * t156) * t160 * t162 / 0.720D3 + (0.90D2 * t56 * (-t168 * 
     #t142 + t170 * t150 / 0.2D1 + t173 + t176 * t36 - t178 * t78 / 0.2D
     #1 - t45) - 0.180D3 * t48 * t34 * (t142 - t168 * t150 - t36 + t176 
     #* t78) + t190) * t160 * t102 / 0.720D3 + (t39 * t34 * (-t36 + t196
     # * t78) + 0.90D2 * t56 * (-t201 * t36 / 0.2D1 - t53 + t201 * t196 
     #* t78 / 0.6D1 + t196 * t45) - t124 - 0.180D3 * t48 * t34 * (t196 *
     # t36 - t201 * t78 / 0.2D1 - t45)) * t104 / 0.1440D4 + (0.90D2 * t5
     #6 * (t224 * t36 - t226 * t78 / 0.2D1 - t45 - t231 * t142 + t233 * 
     #t150 / 0.2D1 + t173) - 0.180D3 * t48 * t34 * (-t36 + t224 * t78 + 
     #t142 - t231 * t150) + t190) * t160 * t104 / 0.1440D4 - (t39 * t34 
     #* (t36 - t251 * t78 - t142 + t255 * t150) + 0.90D2 * t56 * (-t260 
     #* t142 / 0.2D1 - t263 + t260 * t255 * t150 / 0.6D1 + t255 * t173 +
     # t268 * t36 / 0.2D1 + t53 - t268 * t251 * t78 / 0.6D1 - t251 * t45
     #) - t10 * t34 * t155 - 0.180D3 * t48 * t34 * (-t251 * t36 + t268 *
     # t78 / 0.2D1 + t45 + t255 * t142 - t260 * t150 / 0.2D1 - t173)) * 
     #t160 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t294)
      t298 = -0.1D1 + x4
      t300 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t301 = x4 * t298
      t304 = log(-0.4D1 * t16 * t301)
      t305 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t310 = t304 ** 2
      t313 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t317 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t322 = t34 * t305
      t334 = t83 * t298
      t337 = log(-0.4D1 * t221 * t334)
      t339 = t337 ** 2
      t345 = log(0.4D1 * t249 * t301 * t144)
      t346 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, x
     #4)
      t348 = t345 ** 2
      t349 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, x
     #4)
      t352 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t141, x
     #4)
      t363 = t34 * (-t349 + t305)
      t371 = log(-0.4D1 * t82 * t334)
      t373 = t371 ** 2
      t393 = log(0.4D1 * t143 * t83 * t298 * t144)
      t397 = log(-0.4D1 * t143 * t334)
      t408 = (t39 * t34 * (t300 - t304 * t305) + 0.90D2 * t56 * (t310 * 
     #t300 / 0.2D1 + t313 - t310 * t304 * t305 / 0.6D1 - t304 * t317) + 
     #t10 * t322 - 0.180D3 * t48 * t34 * (-t304 * t300 + t310 * t305 / 0
     #.2D1 + t317)) * t104 / 0.1440D4 + (0.90D2 * t56 * (-t337 * t300 + 
     #t339 * t305 / 0.2D1 + t317 + t345 * t346 - t348 * t349 / 0.2D1 - t
     #352) - 0.180D3 * t48 * t34 * (t300 - t337 * t305 - t346 + t345 * t
     #349) + t39 * t363) * t160 * t104 / 0.1440D4 + (0.90D2 * t56 * (-t3
     #71 * t300 + t373 * t305 / 0.2D1 + t317) - 0.180D3 * t48 * t34 * (t
     #300 - t371 * t305) + t39 * t322) * t102 * t104 / 0.720D3 + (0.90D2
     # * t56 * (-t346 + t393 * t349 + t300 - t397 * t305) - 0.180D3 * t4
     #8 * t363) * t160 * t162 / 0.720D3
      t409 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t298, 0.0D0,
     # t408)
      t411 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t412 = s * t411
      t413 = t1 * x1
      t415 = -0.1D1 + x1
      t416 = t1 * t415
      t418 = t411 ** 2
      t420 = t1 ** 2
      t424 = t415 ** 2
      t426 = t418 ** 2
      t430 = log(0.4D1 * t107 * t424 * x4 * t426)
      t432 = 0.1D1 / (-0.2D1 + t411)
      t433 = t430 * t432
      t434 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t436 = t430 ** 2
      t438 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t441 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t442 = t432 * t441
      t446 = t432 * t434
      t453 = t34 * t432 * t438
      t454 = t39 * t453
      t458 = t15 * t424
      t459 = t458 * t426
      t462 = log(0.4D1 * t82 * t459)
      t463 = t462 * t432
      t468 = t462 ** 2
      t469 = t468 * t432
      t472 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t496 = log(0.4D1 * t143 * t458 * x4 * t426)
      t509 = log(0.4D1 * t143 * t459)
      t510 = t509 * t432
      t512 = t509 ** 2
      t528 = (0.90D2 * t56 * (t433 * t434 - t436 * t432 * t438 / 0.2D1 -
     # t442) - 0.180D3 * t48 * t34 * (-t446 + t433 * t438) - t454) * t10
     #2 * t104 / 0.720D3 + (-t39 * t34 * (t446 - t463 * t438) - 0.90D2 *
     # t56 * (t469 * t434 / 0.2D1 + t432 * t472 - t468 * t462 * t432 * t
     #438 / 0.6D1 - t463 * t441) - t10 * t453 + 0.180D3 * t48 * t34 * (-
     #t463 * t434 + t469 * t438 / 0.2D1 + t442)) * t102 / 0.720D3 + (0.9
     #0D2 * t56 * (-t446 + t496 * t432 * t438) + 0.180D3 * t48 * t453) *
     # t160 * t162 / 0.720D3 + (0.90D2 * t56 * (t510 * t434 - t512 * t43
     #2 * t438 / 0.2D1 - t442) - 0.180D3 * t48 * t34 * (-t446 + t510 * t
     #438) - t454) * t160 * t102 / 0.720D3
      t529 = FJET(XB1, XB2, s, t412 * t413, 0.0D0, 0.0D0, -t412 * t416, 
     #-s * t418 * t420 * x1 * t415, t528)
      t531 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t532 = s * t531
      t534 = t416 * x4
      t536 = t416 * t298
      t538 = t531 ** 2
      t541 = x1 * t415
      t544 = t538 ** 2
      t546 = t301 * t544 * t424
      t549 = log(-0.4D1 * t107 * t546)
      t551 = 0.1D1 / (-0.2D1 + t531)
      t552 = t549 * t551
      t553 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t555 = t549 ** 2
      t557 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t560 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t565 = t551 * t553
      t572 = t34 * t551 * t557
      t579 = log(-0.4D1 * t174 * t546)
      t591 = (0.90D2 * t56 * (-t552 * t553 + t555 * t551 * t557 / 0.2D1 
     #+ t551 * t560) - 0.180D3 * t48 * t34 * (t565 - t552 * t557) + t39 
     #* t572) * t102 * t104 / 0.720D3 + (0.90D2 * t56 * (t565 - t579 * t
     #551 * t557) - 0.180D3 * t48 * t572) * t160 * t162 / 0.720D3
      t592 = FJET(XB1, XB2, s, t532 * t413, 0.0D0, -t532 * t534, t532 * 
     #t536, s * t538 * t420 * t541 * t298, t591)
      t594 = KAPPA2(x1, x2, t141, 0.0D0, z)
      t595 = s * t594
      t596 = t413 * t144
      t598 = t413 * x3
      t601 = t594 ** 2
      t607 = 0.1D1 / (-0.2D1 + t594)
      t608 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, 0.0D
     #0)
      t609 = t607 * t608
      t610 = t424 * t144
      t611 = t601 ** 2
      t616 = log(-0.4D1 * t174 * t610 * x4 * t611)
      t618 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, 0.0D
     #0)
      t624 = t34 * t607 * t618
      t634 = log(-0.4D1 * t143 * t458 * t144 * t611)
      t635 = t634 * t607
      t637 = t634 ** 2
      t641 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, 0.0D
     #0)
      t656 = (0.90D2 * t56 * (t609 - t616 * t607 * t618) - 0.180D3 * t48
     # * t624) * t160 * t162 / 0.720D3 + (0.90D2 * t56 * (-t635 * t608 +
     # t637 * t607 * t618 / 0.2D1 + t607 * t641) - 0.180D3 * t48 * t34 *
     # (t609 - t635 * t618) + t39 * t624) * t160 * t102 / 0.720D3
      t657 = FJET(XB1, XB2, s, -t595 * t596, t595 * t598, 0.0D0, -t595 *
     # t416, s * t601 * t420 * t541 * t144, t656)
      t659 = KAPPA2(x1, x2, t141, x4, z)
      t660 = s * t659
      t665 = t659 ** 2
      t670 = cos(t11)
      t673 = Sqrt(x3 * t144 * t301)
      t680 = 0.1D1 / (-0.2D1 + t659)
      t681 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, x4)
      t683 = t665 ** 2
      t688 = log(0.4D1 * t174 * t301 * t610 * t683)
      t690 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t141, x4)
      t699 = -0.90D2 * t56 * (t680 * t681 - t688 * t680 * t690) + 0.180D
     #3 * t48 * t34 * t680 * t690
      t703 = FJET(XB1, XB2, s, -t660 * t596, t660 * t598, -t660 * t534, 
     #t660 * t536, s * t665 * t420 * t541 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t670 * t673), t699 * t160 * t162 / 0.720D3)
      rrgg2gght5s6e1 = t295 * t294 + t409 * t408 + t529 * t528 + t592 * 
     #t591 + t657 * t656 + t703 * t699 * t160 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s6e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t60 = x3 * t11
      t61 = t13 * x4
      t64 = log(0.4D1 * t60 * t61)
      t66 = 0.1D1 - x3
      t67 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t66, 0.0
     #D0)
      t68 = -t66
      t69 = t13 * t68
      t73 = log(-0.4D1 * t60 * t69 * x4)
      t74 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t66, 0.0
     #D0)
      t79 = t74 - t49
      t82 = 0.180D3 * t52 * t26 * t79
      t84 = 0.1D1 / x3
      t86 = 0.1D1 / x4
      t89 = t60 * t13
      t91 = log(0.4D1 * t89)
      t93 = t91 ** 2
      t98 = log(-0.4D1 * t60 * t69)
      t100 = t98 ** 2
      t103 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t66, 0.
     #0D0)
      t121 = log(0.4D1 * t14 * x4)
      t123 = t121 ** 2
      t134 = t26 * t49
      t135 = t8 * t134
      t139 = x1 ** 2
      t140 = t139 * t11
      t143 = log(0.4D1 * t140 * t61)
      t151 = 0.1D1 / x1
      t157 = t84 * t151 * t86
      t160 = x3 * t139
      t164 = log(-0.4D1 * t160 * t14 * t68)
      t168 = log(0.4D1 * t160 * t14)
      t177 = t140 * t13
      t179 = log(0.4D1 * t177)
      t181 = t179 ** 2
      t195 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * t3 * lh) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 + (0.90D2 * t31 * (-t28 + t64 * t49 + t67 - t73 * t74) - t8
     #2) * t84 * t86 / 0.1440D4 - (0.90D2 * t31 * (-t91 * t28 + t93 * t4
     #9 / 0.2D1 + t57 + t98 * t67 - t100 * t74 / 0.2D1 - t103) - 0.180D3
     # * t52 * t26 * (t28 - t91 * t49 - t67 + t98 * t74) - t8 * t26 * t7
     #9) * t84 / 0.1440D4 + (0.90D2 * t31 * (t121 * t28 - t123 * t49 / 0
     #.2D1 - t57) - 0.180D3 * t52 * t26 * (-t28 + t121 * t49) - t135) * 
     #t86 / 0.1440D4 + (0.90D2 * t31 * (-t28 + t143 * t49) + 0.180D3 * t
     #52 * t134) * t151 * t86 / 0.720D3 + t31 * t79 * t157 / 0.8D1 + (0.
     #90D2 * t31 * (t67 - t164 * t74 - t28 + t168 * t49) - t82) * t84 * 
     #t151 / 0.720D3 + (0.90D2 * t31 * (t179 * t28 - t181 * t49 / 0.2D1 
     #- t57) - 0.180D3 * t52 * t26 * (-t28 + t179 * t49) - t135) * t151 
     #/ 0.720D3
      t196 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t195)
      t199 = -0.1D1 + x4
      t201 = x4 * t199
      t204 = log(-0.4D1 * t14 * t201)
      t205 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t207 = t204 ** 2
      t208 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t211 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t220 = t26 * t208
      t225 = t61 * t199
      t228 = log(-0.4D1 * t140 * t225)
      t239 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t66, x4
     #)
      t240 = -t239 + t208
      t246 = log(-0.4D1 * t60 * t225)
      t248 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t66, x4
     #)
      t252 = log(0.4D1 * t89 * t201 * t68)
      t264 = (0.90D2 * t31 * (-t204 * t205 + t207 * t208 / 0.2D1 + t211)
     # - 0.180D3 * t52 * t26 * (t205 - t204 * t208) + t8 * t220) * t86 /
     # 0.1440D4 + (0.90D2 * t31 * (t205 - t228 * t208) - 0.180D3 * t52 *
     # t220) * t151 * t86 / 0.720D3 + t31 * t240 * t157 / 0.8D1 + (0.90D
     #2 * t31 * (t205 - t246 * t208 - t248 + t252 * t239) - 0.180D3 * t5
     #2 * t26 * t240) * t84 * t86 / 0.1440D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t199, 0.0D0,
     # t264)
      t267 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t268 = s * t267
      t269 = t1 * x1
      t271 = -0.1D1 + x1
      t272 = t1 * t271
      t274 = t267 ** 2
      t276 = t1 ** 2
      t281 = 0.1D1 / (-0.2D1 + t267)
      t282 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t283 = t281 * t282
      t284 = t271 ** 2
      t286 = t274 ** 2
      t290 = log(0.4D1 * t177 * t284 * x4 * t286)
      t292 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t298 = t26 * t281 * t292
      t300 = 0.180D3 * t52 * t298
      t307 = t151 * t86
      t311 = t160 * t11
      t312 = t13 * t284
      t313 = t312 * t286
      t316 = log(0.4D1 * t311 * t313)
      t328 = log(0.4D1 * t140 * t313)
      t329 = t328 * t281
      t331 = t328 ** 2
      t335 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t349 = (0.90D2 * t31 * (-t283 + t290 * t281 * t292) + t300) * t151
     # * t86 / 0.720D3 - t31 * t281 * t292 * t84 * t307 / 0.8D1 + (0.90D
     #2 * t31 * (-t283 + t316 * t281 * t292) + t300) * t84 * t151 / 0.72
     #0D3 + (-0.90D2 * t31 * (-t329 * t282 + t331 * t281 * t292 / 0.2D1 
     #+ t281 * t335) + 0.180D3 * t52 * t26 * (t283 - t329 * t292) - t8 *
     # t298) * t151 / 0.720D3
      t350 = FJET(XB1, XB2, s, t268 * t269, 0.0D0, 0.0D0, -t268 * t272, 
     #-s * t274 * t276 * x1 * t271, t349)
      t352 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t353 = s * t352
      t355 = t272 * x4
      t357 = t272 * t199
      t359 = t352 ** 2
      t362 = x1 * t271
      t366 = 0.1D1 / (-0.2D1 + t352)
      t367 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t369 = t359 ** 2
      t374 = log(-0.4D1 * t177 * t201 * t369 * t284)
      t376 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t394 = (0.90D2 * t31 * (t366 * t367 - t374 * t366 * t376) - 0.180D
     #3 * t52 * t26 * t366 * t376) * t151 * t86 / 0.720D3 + t31 * t366 *
     # t376 * t84 * t307 / 0.8D1
      t395 = FJET(XB1, XB2, s, t353 * t269, 0.0D0, -t353 * t355, t353 * 
     #t357, s * t359 * t276 * t362 * t199, t394)
      t397 = KAPPA2(x1, x2, t66, 0.0D0, z)
      t398 = s * t397
      t399 = t269 * t68
      t401 = t269 * x3
      t404 = t397 ** 2
      t410 = 0.1D1 / (-0.2D1 + t397)
      t412 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t66, 0.0D0
     #)
      t417 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t66, 0.0D0
     #)
      t419 = t404 ** 2
      t424 = log(-0.4D1 * t311 * t312 * t68 * t419)
      t438 = t31 * t410 * t412 * t84 * t307 / 0.8D1 + (0.90D2 * t31 * (t
     #410 * t417 - t424 * t410 * t412) - 0.180D3 * t52 * t26 * t410 * t4
     #12) * t84 * t151 / 0.720D3
      t439 = FJET(XB1, XB2, s, -t398 * t399, t398 * t401, 0.0D0, -t398 *
     # t272, s * t404 * t276 * t362 * t68, t438)
      t441 = KAPPA2(x1, x2, t66, x4, z)
      t442 = s * t441
      t447 = t441 ** 2
      t452 = cos(t9)
      t455 = Sqrt(x3 * t68 * t201)
      t462 = 0.1D1 / (-0.2D1 + t441)
      t464 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t66, x4)
      t466 = t464 * t84 * t307
      t469 = FJET(XB1, XB2, s, -t442 * t399, t442 * t401, -t442 * t355, 
     #t442 * t357, s * t447 * t276 * t362 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t452 * t455), -t31 * t462 * t466 / 0.8D1)
      rrgg2gght5s6e0 = t196 * t195 + t265 * t264 + t350 * t349 + t395 * 
     #t394 + t439 * t438 - t469 * 0.3141592653589793D1 * t26 * t462 * t4
     #66 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s6em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t43 = 0.1D1 - x3
      t44 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t43, 0.0
     #D0)
      t45 = t44 - t36
      t46 = 0.1D1 / x3
      t47 = t45 * t46
      t48 = 0.1D1 / x4
      t52 = x3 * t7
      t55 = log(0.4D1 * t52 * t9)
      t57 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t43, 0.0
     #D0)
      t58 = -t43
      t62 = log(-0.4D1 * t52 * t9 * t58)
      t76 = log(0.4D1 * t10 * x4)
      t83 = 0.180D3 * t3 * t18 * t36
      t87 = 0.1D1 / x1
      t91 = x1 ** 2
      t92 = t91 * t7
      t95 = log(0.4D1 * t92 * t9)
      t107 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 + t39 * t47 * t48 / 0.16D2 - (0.90D2 * t3
     #9 * (t20 - t55 * t36 - t57 + t62 * t44) + 0.180D3 * t3 * t18 * t45
     #) * t46 / 0.1440D4 + (0.90D2 * t39 * (-t20 + t76 * t36) + t83) * t
     #48 / 0.1440D4 + t39 * t47 * t87 / 0.8D1 + (0.90D2 * t39 * (-t20 + 
     #t95 * t36) + t83) * t87 / 0.720D3 - t39 * t36 * t87 * t48 / 0.8D1
      t108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t107)
      t111 = -0.1D1 + x4
      t113 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t117 = log(-0.4D1 * t10 * x4 * t111)
      t118 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t133 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t43, x4
     #)
      t139 = (0.90D2 * t39 * (t113 - t117 * t118) - 0.180D3 * t3 * t18 *
     # t118) * t48 / 0.1440D4 + t39 * t118 * t87 * t48 / 0.8D1 + t39 * (
     #-t133 + t118) * t46 * t48 / 0.16D2
      t140 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t111, 0.0D0,
     # t139)
      t142 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t143 = s * t142
      t144 = t1 * x1
      t146 = -0.1D1 + x1
      t147 = t1 * t146
      t149 = t142 ** 2
      t151 = t1 ** 2
      t156 = 0.1D1 / (-0.2D1 + t142)
      t157 = t39 * t156
      t158 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t163 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t165 = t146 ** 2
      t167 = t149 ** 2
      t171 = log(0.4D1 * t92 * t9 * t165 * t167)
      t188 = -t157 * t158 * t46 * t87 / 0.8D1 + (-0.90D2 * t39 * (t156 *
     # t163 - t171 * t156 * t158) + 0.180D3 * t3 * t18 * t156 * t158) * 
     #t87 / 0.720D3 - t157 * t158 * t87 * t48 / 0.8D1
      t189 = FJET(XB1, XB2, s, t143 * t144, 0.0D0, 0.0D0, -t143 * t147, 
     #-s * t149 * t151 * x1 * t146, t188)
      t191 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t192 = s * t191
      t198 = t191 ** 2
      t201 = x1 * t146
      t205 = 0.1D1 / (-0.2D1 + t191)
      t207 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t212 = FJET(XB1, XB2, s, t192 * t144, 0.0D0, -t192 * t147 * x4, t1
     #92 * t147 * t111, s * t198 * t151 * t201 * t111, t39 * t205 * t207
     # * t87 * t48 / 0.8D1)
      t220 = KAPPA2(x1, x2, t43, 0.0D0, z)
      t221 = s * t220
      t227 = t220 ** 2
      t233 = 0.1D1 / (-0.2D1 + t220)
      t235 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t43, 0.0D0
     #)
      t240 = FJET(XB1, XB2, s, -t221 * t144 * t58, t221 * t144 * x3, 0.0
     #D0, -t221 * t147, s * t227 * t151 * t201 * t58, t39 * t233 * t235 
     #* t46 * t87 / 0.8D1)
      rrgg2gght5s6em1 = t108 * t107 + t140 * t139 + t189 * t188 + t212 *
     # 0.3141592653589793D1 * t18 * t205 * t207 * t87 * t48 / 0.8D1 + t2
     #40 * 0.3141592653589793D1 * t18 * t233 * t235 * t46 * t87 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s6em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t8 = 0.1D1 / x4
      t13 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.0D0)
      t19 = 0.1D1 / x1
      t23 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t29 = sin(x2 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t31 = z ** 2
      t35 = log(0.4D1 * t30 / t31)
      t42 = -t6 * t7 * t8 / 0.16D2 - t6 * (t7 - t13) / x3 / 0.16D2 - t6 
     #* t7 * t19 / 0.8D1 - t6 * t23 / 0.16D2 - (-0.180D3 * 0.31415926535
     #89793D1 * lh - 0.90D2 * t35 * 0.3141592653589793D1) * t5 * t7 / 0.
     #1440D4
      t43 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t42)
      t48 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t6 * t48 * t8 / 0.16D2)
      t58 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t59 = s * t58
      t62 = -0.1D1 + x1
      t65 = t58 ** 2
      t67 = t1 ** 2
      t73 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.0
     #D0)
      t75 = 0.1D1 / (-0.2D1 + t58) * t73 * t19
      t78 = FJET(XB1, XB2, s, t59 * t1 * x1, 0.0D0, 0.0D0, -t59 * t1 * t
     #62, -s * t65 * t67 * x1 * t62, -t6 * t75 / 0.8D1)
      rrgg2gght5s6em2 = t43 * t42 + t52 * 0.3141592653589793D1 * t5 * t4
     #8 * t8 / 0.16D2 - t78 * 0.3141592653589793D1 * t5 * t75 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s6em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s6em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s6em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s6em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s7e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * t3 * lh - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = 0.3141592653589793D1 * t18
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t86 = log(0.4D1 * t82 * t83)
      t88 = t86 ** 2
      t99 = t34 * t78
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t82 * t15
      t109 = log(0.4D1 * t107)
      t114 = t109 ** 2
      t124 = t10 * t99
      t135 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t136 = x3 * t81
      t137 = t136 * t13
      t138 = -0.1D1 + x3
      t139 = t15 * t138
      t140 = t139 * x4
      t143 = log(-0.4D1 * t137 * t140)
      t144 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t146 = t16 * x4
      t149 = log(0.4D1 * t136 * t146)
      t154 = -t78 + t144
      t155 = t34 * t154
      t159 = 0.1D1 / x3
      t161 = t102 * t104
      t164 = t136 * t16
      t166 = log(0.4D1 * t164)
      t168 = t166 ** 2
      t174 = log(-0.4D1 * t136 * t16 * t138)
      t176 = t174 ** 2
      t179 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t195 = log(0.4D1 * t146)
      t200 = t195 ** 2
      t220 = x3 * t13
      t223 = log(0.4D1 * t220 * t83)
      t225 = t223 ** 2
      t230 = log(-0.4D1 * t220 * t140)
      t232 = t230 ** 2
      t245 = -t34 * t154
      t253 = log(-0.4D1 * t220 * t139)
      t255 = t220 * t15
      t257 = log(0.4D1 * t255)
      t262 = t257 ** 2
      t269 = t253 ** 2
      t272 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t294 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t56 * 
     #(t86 * t36 - t88 * t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (-t3
     #6 + t86 * t78) - t39 * t99) * t102 * t104 / 0.720D3 + (t39 * t34 *
     # (-t36 + t109 * t78) + 0.90D2 * t56 * (-t114 * t36 / 0.2D1 - t53 +
     # t114 * t109 * t78 / 0.6D1 + t109 * t45) - t124 - 0.180D3 * t48 * 
     #t34 * (t109 * t36 - t114 * t78 / 0.2D1 - t45)) * t102 / 0.720D3 + 
     #(0.90D2 * t56 * (t135 - t143 * t144 - t36 + t149 * t78) - 0.180D3 
     #* t48 * t155) * t159 * t161 / 0.720D3 + (0.90D2 * t56 * (t166 * t3
     #6 - t168 * t78 / 0.2D1 - t45 - t174 * t135 + t176 * t144 / 0.2D1 +
     # t179) - 0.180D3 * t48 * t34 * (-t36 + t166 * t78 + t135 - t174 * 
     #t144) + t39 * t155) * t159 * t102 / 0.720D3 - (t39 * t34 * (t36 - 
     #t195 * t78) + 0.90D2 * t56 * (t200 * t36 / 0.2D1 + t53 - t200 * t1
     #95 * t78 / 0.6D1 - t195 * t45) + t124 - 0.180D3 * t48 * t34 * (-t1
     #95 * t36 + t200 * t78 / 0.2D1 + t45)) * t104 / 0.1440D4 - (0.90D2 
     #* t56 * (-t223 * t36 + t225 * t78 / 0.2D1 + t45 + t230 * t135 - t2
     #32 * t144 / 0.2D1 - t179) - 0.180D3 * t48 * t34 * (t36 - t223 * t7
     #8 - t135 + t230 * t144) + t39 * t245) * t159 * t104 / 0.1440D4 - (
     #t39 * t34 * (-t135 + t253 * t144 + t36 - t257 * t78) + 0.90D2 * t5
     #6 * (t262 * t36 / 0.2D1 + t53 - t262 * t257 * t78 / 0.6D1 - t257 *
     # t45 - t269 * t135 / 0.2D1 - t272 + t269 * t253 * t144 / 0.6D1 + t
     #253 * t179) + t10 * t245 - 0.180D3 * t48 * t34 * (t253 * t135 - t2
     #69 * t144 / 0.2D1 - t179 - t257 * t36 + t262 * t78 / 0.2D1 + t45))
     # * t159 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t294)
      t297 = -0.1D1 + x4
      t300 = -t297
      t301 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t300)
      t302 = x4 * t297
      t305 = log(-0.4D1 * t16 * t302)
      t306 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t300)
      t311 = t305 ** 2
      t314 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t300)
      t318 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t300)
      t323 = t34 * t306
      t335 = t83 * t297
      t338 = log(-0.4D1 * t220 * t335)
      t340 = t338 ** 2
      t346 = log(0.4D1 * t255 * t302 * t138)
      t347 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t30
     #0)
      t349 = t346 ** 2
      t350 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t30
     #0)
      t353 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t30
     #0)
      t363 = -t306 + t350
      t372 = log(-0.4D1 * t82 * t335)
      t374 = t372 ** 2
      t394 = log(0.4D1 * t137 * t83 * t297 * t138)
      t398 = log(-0.4D1 * t137 * t335)
      t411 = -(-t39 * t34 * (t301 - t305 * t306) - 0.90D2 * t56 * (t311 
     #* t301 / 0.2D1 + t314 - t311 * t305 * t306 / 0.6D1 - t305 * t318) 
     #- t10 * t323 + 0.180D3 * t48 * t34 * (-t305 * t301 + t311 * t306 /
     # 0.2D1 + t318)) * t104 / 0.1440D4 - (0.90D2 * t56 * (t338 * t301 -
     # t340 * t306 / 0.2D1 - t318 - t346 * t347 + t350 * t349 / 0.2D1 + 
     #t353) - 0.180D3 * t48 * t34 * (-t301 + t338 * t306 + t347 - t346 *
     # t350) + t39 * t34 * t363) * t159 * t104 / 0.1440D4 + (0.90D2 * t5
     #6 * (-t372 * t301 + t374 * t306 / 0.2D1 + t318) - 0.180D3 * t48 * 
     #t34 * (t301 - t372 * t306) + t39 * t323) * t102 * t104 / 0.720D3 +
     # (0.90D2 * t56 * (-t347 + t394 * t350 + t301 - t398 * t306) + 0.18
     #0D3 * t48 * t34 * t363) * t159 * t161 / 0.720D3
      t412 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t297, t2 * x4, 0.0D0,
     # t411)
      t414 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t415 = s * t414
      t416 = t1 * x1
      t418 = -0.1D1 + x1
      t419 = t1 * t418
      t421 = t414 ** 2
      t423 = t1 ** 2
      t427 = t418 ** 2
      t428 = t427 * x4
      t429 = t421 ** 2
      t433 = log(0.4D1 * t107 * t428 * t429)
      t435 = 0.1D1 / (-0.2D1 + t414)
      t436 = t433 * t435
      t437 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t439 = t433 ** 2
      t441 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t444 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t445 = t435 * t444
      t449 = t435 * t437
      t456 = t34 * t435 * t441
      t457 = t39 * t456
      t461 = t81 * t427
      t462 = t461 * t429
      t465 = log(0.4D1 * t16 * t462)
      t466 = t465 * t435
      t471 = t465 ** 2
      t472 = t471 * t435
      t475 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t499 = log(0.4D1 * t255 * t461 * x4 * t429)
      t512 = log(0.4D1 * t255 * t462)
      t513 = t512 * t435
      t515 = t512 ** 2
      t531 = (0.90D2 * t56 * (t436 * t437 - t439 * t435 * t441 / 0.2D1 -
     # t445) - 0.180D3 * t48 * t34 * (-t449 + t436 * t441) - t457) * t10
     #2 * t104 / 0.720D3 + (-t39 * t34 * (t449 - t466 * t441) - 0.90D2 *
     # t56 * (t472 * t437 / 0.2D1 + t435 * t475 - t471 * t465 * t435 * t
     #441 / 0.6D1 - t466 * t444) - t10 * t456 + 0.180D3 * t48 * t34 * (-
     #t466 * t437 + t472 * t441 / 0.2D1 + t445)) * t102 / 0.720D3 + (0.9
     #0D2 * t56 * (-t449 + t499 * t435 * t441) + 0.180D3 * t48 * t456) *
     # t159 * t161 / 0.720D3 + (0.90D2 * t56 * (t513 * t437 - t515 * t43
     #5 * t441 / 0.2D1 - t445) - 0.180D3 * t48 * t34 * (-t449 + t513 * t
     #441) - t457) * t159 * t102 / 0.720D3
      t532 = FJET(XB1, XB2, s, 0.0D0, t415 * t416, -t415 * t419, 0.0D0, 
     #-s * t421 * t423 * x1 * t418, t531)
      t534 = KAPPA2(x1, x2, 0.0D0, t300, z)
      t535 = s * t534
      t537 = t419 * t297
      t539 = t419 * x4
      t541 = t534 ** 2
      t544 = x1 * t418
      t547 = t541 ** 2
      t549 = t428 * t297 * t547
      t552 = log(-0.4D1 * t107 * t549)
      t554 = 0.1D1 / (-0.2D1 + t534)
      t555 = t552 * t554
      t556 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t30
     #0)
      t558 = t552 ** 2
      t560 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t30
     #0)
      t563 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t30
     #0)
      t568 = t554 * t556
      t575 = t34 * t554 * t560
      t582 = log(-0.4D1 * t164 * t549)
      t594 = (0.90D2 * t56 * (-t555 * t556 + t558 * t554 * t560 / 0.2D1 
     #+ t554 * t563) - 0.180D3 * t48 * t34 * (t568 - t555 * t560) + t39 
     #* t575) * t102 * t104 / 0.720D3 + (0.90D2 * t56 * (t568 - t582 * t
     #554 * t560) - 0.180D3 * t48 * t575) * t159 * t161 / 0.720D3
      t595 = FJET(XB1, XB2, s, 0.0D0, t535 * t416, t535 * t537, -t535 * 
     #t539, s * t541 * t423 * t544 * t297, t594)
      t597 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t598 = s * t597
      t599 = t416 * x3
      t601 = t416 * t138
      t604 = t597 ** 2
      t610 = 0.1D1 / (-0.2D1 + t597)
      t611 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t612 = t610 * t611
      t613 = t427 * t138
      t614 = t604 ** 2
      t619 = log(-0.4D1 * t164 * t613 * x4 * t614)
      t621 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t627 = t34 * t610 * t621
      t637 = log(-0.4D1 * t255 * t461 * t138 * t614)
      t638 = t637 * t610
      t640 = t637 ** 2
      t644 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t659 = (0.90D2 * t56 * (t612 - t619 * t610 * t621) - 0.180D3 * t48
     # * t627) * t159 * t161 / 0.720D3 + (0.90D2 * t56 * (-t638 * t611 +
     # t640 * t610 * t621 / 0.2D1 + t610 * t644) - 0.180D3 * t48 * t34 *
     # (t612 - t638 * t621) + t39 * t627) * t159 * t102 / 0.720D3
      t660 = FJET(XB1, XB2, s, t598 * t599, -t598 * t601, -t598 * t419, 
     #0.0D0, s * t604 * t423 * t544 * t138, t659)
      t662 = KAPPA2(x1, x2, x3, t300, z)
      t663 = s * t662
      t668 = t662 ** 2
      t673 = cos(t11)
      t676 = Sqrt(x3 * t138 * t302)
      t683 = 0.1D1 / (-0.2D1 + t662)
      t684 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t300)
      t686 = t668 ** 2
      t691 = log(0.4D1 * t164 * t613 * t302 * t686)
      t693 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t300)
      t702 = -0.90D2 * t56 * (t683 * t684 - t691 * t683 * t693) + 0.180D
     #3 * t48 * t34 * t683 * t693
      t706 = FJET(XB1, XB2, s, t663 * t599, -t663 * t601, t663 * t537, -
     #t663 * t539, s * t668 * t423 * t544 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t673 * t676), t702 * t159 * t161 / 0.720D3)
      rrgg2gght5s7e1 = t295 * t294 + t412 * t411 + t532 * t531 + t595 * 
     #t594 + t660 * t659 + t706 * t702 * t159 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s7e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t60 = x3 * t11
      t61 = t13 * x4
      t64 = log(0.4D1 * t60 * t61)
      t66 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t67 = -0.1D1 + x3
      t68 = t13 * t67
      t72 = log(-0.4D1 * t60 * t68 * x4)
      t73 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t78 = -t73 + t49
      t79 = t26 * t78
      t83 = 0.1D1 / x3
      t85 = 0.1D1 / x4
      t90 = log(-0.4D1 * t60 * t68)
      t92 = t90 ** 2
      t95 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t96 = t60 * t13
      t98 = log(0.4D1 * t96)
      t100 = t98 ** 2
      t118 = log(0.4D1 * t14 * x4)
      t120 = t118 ** 2
      t131 = t26 * t49
      t132 = t8 * t131
      t136 = x1 ** 2
      t137 = t136 * t11
      t140 = log(0.4D1 * t137 * t61)
      t148 = 0.1D1 / x1
      t152 = -t78
      t155 = t83 * t148 * t85
      t158 = x3 * t136
      t161 = log(0.4D1 * t158 * t14)
      t166 = log(-0.4D1 * t158 * t14 * t67)
      t178 = t137 * t13
      t180 = log(0.4D1 * t178)
      t182 = t180 ** 2
      t196 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * t3 * lh) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 - (0.90D2 * t31 * (t28 - t64 * t49 - t66 + t72 * t73) - 0.1
     #80D3 * t52 * t79) * t83 * t85 / 0.1440D4 - (0.90D2 * t31 * (t90 * 
     #t66 - t92 * t73 / 0.2D1 - t95 - t98 * t28 + t100 * t49 / 0.2D1 + t
     #57) - 0.180D3 * t52 * t26 * (-t66 + t90 * t73 + t28 - t98 * t49) +
     # t8 * t79) * t83 / 0.1440D4 - (0.90D2 * t31 * (-t118 * t28 + t120 
     #* t49 / 0.2D1 + t57) - 0.180D3 * t52 * t26 * (t28 - t118 * t49) + 
     #t132) * t85 / 0.1440D4 + (0.90D2 * t31 * (-t28 + t140 * t49) + 0.1
     #80D3 * t52 * t131) * t148 * t85 / 0.720D3 + t31 * t152 * t155 / 0.
     #8D1 + (0.90D2 * t31 * (-t28 + t161 * t49 + t66 - t166 * t73) - 0.1
     #80D3 * t52 * t26 * t152) * t83 * t148 / 0.720D3 + (0.90D2 * t31 * 
     #(t180 * t28 - t182 * t49 / 0.2D1 - t57) - 0.180D3 * t52 * t26 * (-
     #t28 + t180 * t49) - t132) * t148 / 0.720D3
      t197 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t196)
      t199 = -0.1D1 + x4
      t202 = x4 * t199
      t205 = log(-0.4D1 * t14 * t202)
      t206 = -t199
      t207 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t206)
      t209 = t205 ** 2
      t210 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t206)
      t213 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t206)
      t222 = t26 * t210
      t227 = t61 * t199
      t230 = log(-0.4D1 * t137 * t227)
      t241 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t20
     #6)
      t242 = t210 - t241
      t248 = log(-0.4D1 * t60 * t227)
      t250 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t20
     #6)
      t254 = log(0.4D1 * t96 * t202 * t67)
      t267 = -(-0.90D2 * t31 * (-t205 * t207 + t209 * t210 / 0.2D1 + t21
     #3) + 0.180D3 * t52 * t26 * (t207 - t205 * t210) - t8 * t222) * t85
     # / 0.1440D4 + (0.90D2 * t31 * (t207 - t230 * t210) - 0.180D3 * t52
     # * t222) * t148 * t85 / 0.720D3 + t31 * t242 * t155 / 0.8D1 - (0.9
     #0D2 * t31 * (-t207 + t248 * t210 + t250 - t254 * t241) + 0.180D3 *
     # t52 * t26 * t242) * t83 * t85 / 0.1440D4
      t268 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t199, t2 * x4, 0.0D0,
     # t267)
      t270 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t271 = s * t270
      t272 = t1 * x1
      t274 = -0.1D1 + x1
      t275 = t1 * t274
      t277 = t270 ** 2
      t279 = t1 ** 2
      t284 = 0.1D1 / (-0.2D1 + t270)
      t285 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t286 = t284 * t285
      t287 = t274 ** 2
      t288 = t287 * x4
      t289 = t277 ** 2
      t293 = log(0.4D1 * t178 * t288 * t289)
      t295 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t301 = t26 * t284 * t295
      t303 = 0.180D3 * t52 * t301
      t310 = t148 * t85
      t314 = t136 * t287
      t315 = t314 * t289
      t318 = log(0.4D1 * t96 * t315)
      t330 = log(0.4D1 * t14 * t315)
      t331 = t330 * t284
      t333 = t330 ** 2
      t337 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t351 = (0.90D2 * t31 * (-t286 + t293 * t284 * t295) + t303) * t148
     # * t85 / 0.720D3 - t31 * t284 * t295 * t83 * t310 / 0.8D1 + (0.90D
     #2 * t31 * (-t286 + t318 * t284 * t295) + t303) * t83 * t148 / 0.72
     #0D3 + (-0.90D2 * t31 * (-t331 * t285 + t333 * t284 * t295 / 0.2D1 
     #+ t284 * t337) + 0.180D3 * t52 * t26 * (t286 - t331 * t295) - t8 *
     # t301) * t148 / 0.720D3
      t352 = FJET(XB1, XB2, s, 0.0D0, t271 * t272, -t271 * t275, 0.0D0, 
     #-s * t277 * t279 * x1 * t274, t351)
      t354 = KAPPA2(x1, x2, 0.0D0, t206, z)
      t355 = s * t354
      t357 = t275 * t199
      t359 = t275 * x4
      t361 = t354 ** 2
      t364 = x1 * t274
      t368 = 0.1D1 / (-0.2D1 + t354)
      t369 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t20
     #6)
      t371 = t361 ** 2
      t376 = log(-0.4D1 * t178 * t288 * t199 * t371)
      t378 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t20
     #6)
      t396 = (0.90D2 * t31 * (t368 * t369 - t376 * t368 * t378) - 0.180D
     #3 * t52 * t26 * t368 * t378) * t148 * t85 / 0.720D3 + t31 * t368 *
     # t378 * t83 * t310 / 0.8D1
      t397 = FJET(XB1, XB2, s, 0.0D0, t355 * t272, t355 * t357, -t355 * 
     #t359, s * t361 * t279 * t364 * t199, t396)
      t399 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t400 = s * t399
      t401 = t272 * x3
      t403 = t272 * t67
      t406 = t399 ** 2
      t412 = 0.1D1 / (-0.2D1 + t399)
      t414 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t419 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t421 = t406 ** 2
      t426 = log(-0.4D1 * t96 * t314 * t67 * t421)
      t440 = t31 * t412 * t414 * t83 * t310 / 0.8D1 + (0.90D2 * t31 * (t
     #412 * t419 - t426 * t412 * t414) - 0.180D3 * t52 * t26 * t412 * t4
     #14) * t83 * t148 / 0.720D3
      t441 = FJET(XB1, XB2, s, t400 * t401, -t400 * t403, -t400 * t275, 
     #0.0D0, s * t406 * t279 * t364 * t67, t440)
      t443 = KAPPA2(x1, x2, x3, t206, z)
      t444 = s * t443
      t449 = t443 ** 2
      t454 = cos(t9)
      t457 = Sqrt(x3 * t67 * t202)
      t464 = 0.1D1 / (-0.2D1 + t443)
      t466 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t206)
      t468 = t466 * t83 * t310
      t471 = FJET(XB1, XB2, s, t444 * t401, -t444 * t403, t444 * t357, -
     #t444 * t359, s * t449 * t279 * t364 * (-0.1D1 + x3 + x4 - 0.2D1 * 
     #x3 * x4 + 0.2D1 * t454 * t457), -t31 * t464 * t468 / 0.8D1)
      rrgg2gght5s7e0 = t197 * t196 + t268 * t267 + t352 * t351 + t397 * 
     #t396 + t441 * t440 - t471 * 0.3141592653589793D1 * t26 * t464 * t4
     #68 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s7em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t39 = 0.3141592653589793D1 * t18
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t43 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t44 = -t43 + t36
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x4
      t51 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t52 = x3 * t7
      t53 = -0.1D1 + x3
      t57 = log(-0.4D1 * t52 * t9 * t53)
      t61 = log(0.4D1 * t52 * t9)
      t74 = log(0.4D1 * t10 * x4)
      t81 = 0.180D3 * t3 * t18 * t36
      t87 = 0.1D1 / x1
      t91 = x1 ** 2
      t95 = log(0.4D1 * t91 * t7 * t9)
      t107 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 - t39 * t44 * t45 * t47 / 0.16D2 - (0.90D
     #2 * t39 * (-t51 + t57 * t43 + t20 - t61 * t36) - 0.180D3 * t3 * t1
     #8 * t44) * t45 / 0.1440D4 - (0.90D2 * t39 * (t20 - t74 * t36) - t8
     #1) * t47 / 0.1440D4 - t39 * t44 * t45 * t87 / 0.8D1 + (0.90D2 * t3
     #9 * (-t20 + t95 * t36) + t81) * t87 / 0.720D3 - t39 * t36 * t87 * 
     #t47 / 0.8D1
      t108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t107)
      t110 = -0.1D1 + x4
      t113 = -t110
      t114 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t113)
      t118 = log(-0.4D1 * t10 * x4 * t110)
      t119 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t113)
      t134 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t11
     #3)
      t140 = -(-0.90D2 * t39 * (t114 - t118 * t119) + 0.180D3 * t3 * t18
     # * t119) * t47 / 0.1440D4 + t39 * t119 * t87 * t47 / 0.8D1 - t39 *
     # (-t119 + t134) * t45 * t47 / 0.16D2
      t141 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t110, t2 * x4, 0.0D0,
     # t140)
      t143 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t144 = s * t143
      t145 = t1 * x1
      t147 = -0.1D1 + x1
      t148 = t1 * t147
      t150 = t143 ** 2
      t152 = t1 ** 2
      t157 = 0.1D1 / (-0.2D1 + t143)
      t158 = t39 * t157
      t159 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t164 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t166 = t147 ** 2
      t168 = t150 ** 2
      t172 = log(0.4D1 * t10 * t91 * t166 * t168)
      t189 = -t158 * t159 * t45 * t87 / 0.8D1 + (-0.90D2 * t39 * (t157 *
     # t164 - t172 * t157 * t159) + 0.180D3 * t3 * t18 * t157 * t159) * 
     #t87 / 0.720D3 - t158 * t159 * t87 * t47 / 0.8D1
      t190 = FJET(XB1, XB2, s, 0.0D0, t144 * t145, -t144 * t148, 0.0D0, 
     #-s * t150 * t152 * x1 * t147, t189)
      t192 = KAPPA2(x1, x2, 0.0D0, t113, z)
      t193 = s * t192
      t199 = t192 ** 2
      t202 = x1 * t147
      t206 = 0.1D1 / (-0.2D1 + t192)
      t208 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t11
     #3)
      t213 = FJET(XB1, XB2, s, 0.0D0, t193 * t145, t193 * t148 * t110, -
     #t193 * t148 * x4, s * t199 * t152 * t202 * t110, t39 * t206 * t208
     # * t87 * t47 / 0.8D1)
      t221 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t222 = s * t221
      t228 = t221 ** 2
      t234 = 0.1D1 / (-0.2D1 + t221)
      t236 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t241 = FJET(XB1, XB2, s, t222 * t145 * x3, -t222 * t145 * t53, -t2
     #22 * t148, 0.0D0, s * t228 * t152 * t202 * t53, t39 * t234 * t236 
     #* t45 * t87 / 0.8D1)
      rrgg2gght5s7em1 = t108 * t107 + t141 * t140 + t190 * t189 + t213 *
     # 0.3141592653589793D1 * t18 * t206 * t208 * t87 * t47 / 0.8D1 + t2
     #41 * 0.3141592653589793D1 * t18 * t234 * t236 * t45 * t87 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s7em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t8 = 0.1D1 / x4
      t12 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t18 = 0.1D1 / x1
      t22 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t28 = sin(x2 * 0.3141592653589793D1)
      t29 = t28 ** 2
      t30 = z ** 2
      t34 = log(0.4D1 * t29 / t30)
      t41 = -t6 * t7 * t8 / 0.16D2 - t6 * (-t12 + t7) / x3 / 0.16D2 - t6
     # * t7 * t18 / 0.8D1 - t6 * t22 / 0.16D2 - (-0.180D3 * 0.3141592653
     #589793D1 * lh - 0.90D2 * 0.3141592653589793D1 * t34) * t5 * t7 / 0
     #.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t44 = -0.1D1 + x4
      t48 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, -
     #t44)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t44, t2 * x4, 0.0D0, t
     #6 * t48 * t8 / 0.16D2)
      t58 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t59 = s * t58
      t62 = -0.1D1 + x1
      t65 = t58 ** 2
      t67 = t1 ** 2
      t73 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t75 = 0.1D1 / (-0.2D1 + t58) * t73 * t18
      t78 = FJET(XB1, XB2, s, 0.0D0, t59 * t1 * x1, -t59 * t1 * t62, 0.0
     #D0, -s * t65 * t67 * x1 * t62, -t6 * t75 / 0.8D1)
      rrgg2gght5s7em2 = t42 * t41 + t52 * 0.3141592653589793D1 * t5 * t4
     #8 * t8 / 0.16D2 - t78 * 0.3141592653589793D1 * t5 * t75 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s7em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s7em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s7em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s7em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s8e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t6 = lh ** 2
      t9 = 0.60D2 * lh * t3 - 0.2884936567583026D3 - 0.120D3 * t6 * lh
      t10 = 0.3141592653589793D1 * t9
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t18 = log(0.4D1 * t16)
      t19 = t18 * 0.3141592653589793D1
      t22 = 0.180D3 * t6 - 0.30D2 * t3
      t24 = t18 ** 2
      t25 = t24 * 0.3141592653589793D1
      t29 = t24 * t18 * 0.3141592653589793D1
      t32 = s ** 2
      t34 = 0.1D1 / t32 / s
      t36 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t39 = 0.3141592653589793D1 * t22
      t45 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t60 = t3 ** 2
      t61 = t6 ** 2
      t73 = t24 ** 2
      t78 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t81 = x1 ** 2
      t82 = t81 * t13
      t83 = t15 * x4
      t86 = log(0.4D1 * t82 * t83)
      t88 = t86 ** 2
      t99 = t34 * t78
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t82 * t15
      t109 = log(0.4D1 * t107)
      t114 = t109 ** 2
      t124 = t10 * t99
      t135 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t136 = x3 * t81
      t137 = t136 * t13
      t138 = -0.1D1 + x3
      t139 = t15 * t138
      t140 = t139 * x4
      t143 = log(-0.4D1 * t137 * t140)
      t144 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t146 = t16 * x4
      t149 = log(0.4D1 * t136 * t146)
      t154 = -t78 + t144
      t155 = t34 * t154
      t159 = 0.1D1 / x3
      t161 = t102 * t104
      t167 = log(-0.4D1 * t136 * t16 * t138)
      t169 = t167 ** 2
      t172 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t173 = t136 * t16
      t175 = log(0.4D1 * t173)
      t177 = t175 ** 2
      t195 = log(0.4D1 * t146)
      t200 = t195 ** 2
      t220 = x3 * t13
      t223 = log(0.4D1 * t220 * t83)
      t225 = t223 ** 2
      t230 = log(-0.4D1 * t220 * t140)
      t232 = t230 ** 2
      t245 = -t34 * t154
      t253 = log(-0.4D1 * t220 * t139)
      t255 = t220 * t15
      t257 = log(0.4D1 * t255)
      t262 = t257 ** 2
      t269 = t253 ** 2
      t272 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t294 = -(t10 - t19 * t22 - 0.90D2 * t25 * lh - 0.15D2 * t29) * t34
     # * t36 / 0.1440D4 - (t39 + 0.180D3 * t19 * lh + 0.45D2 * t25) * t3
     #4 * t45 / 0.1440D4 - (-0.180D3 * t48 - 0.90D2 * t19) * t34 * t53 /
     # 0.1440D4 - t56 * t57 / 0.16D2 - (0.3141592653589793D1 * (t60 + 0.
     #60D2 * t61 + 0.5769873135166051D3 * lh - 0.60D2 * t6 * t3) - t19 *
     # t9 + t25 * t22 / 0.2D1 + 0.30D2 * t29 * lh + 0.15D2 / 0.4D1 * t73
     # * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 + (0.90D2 * t56 * 
     #(t86 * t36 - t88 * t78 / 0.2D1 - t45) - 0.180D3 * t48 * t34 * (-t3
     #6 + t86 * t78) - t39 * t99) * t102 * t104 / 0.720D3 - (t39 * t34 *
     # (t36 - t109 * t78) + 0.90D2 * t56 * (t114 * t36 / 0.2D1 + t53 - t
     #114 * t109 * t78 / 0.6D1 - t109 * t45) + t124 - 0.180D3 * t48 * t3
     #4 * (-t109 * t36 + t114 * t78 / 0.2D1 + t45)) * t102 / 0.720D3 + (
     #0.90D2 * t56 * (t135 - t143 * t144 - t36 + t149 * t78) - 0.180D3 *
     # t48 * t155) * t159 * t161 / 0.720D3 + (0.90D2 * t56 * (-t167 * t1
     #35 + t169 * t144 / 0.2D1 + t172 + t175 * t36 - t177 * t78 / 0.2D1 
     #- t45) - 0.180D3 * t48 * t34 * (t135 - t167 * t144 - t36 + t175 * 
     #t78) + t39 * t155) * t159 * t102 / 0.720D3 - (t39 * t34 * (t36 - t
     #195 * t78) + 0.90D2 * t56 * (t200 * t36 / 0.2D1 + t53 - t200 * t19
     #5 * t78 / 0.6D1 - t195 * t45) + t124 - 0.180D3 * t48 * t34 * (-t19
     #5 * t36 + t200 * t78 / 0.2D1 + t45)) * t104 / 0.1440D4 - (0.90D2 *
     # t56 * (-t223 * t36 + t225 * t78 / 0.2D1 + t45 + t230 * t135 - t23
     #2 * t144 / 0.2D1 - t172) - 0.180D3 * t48 * t34 * (t36 - t223 * t78
     # - t135 + t230 * t144) + t39 * t245) * t159 * t104 / 0.1440D4 - (t
     #39 * t34 * (-t135 + t253 * t144 + t36 - t257 * t78) + 0.90D2 * t56
     # * (t262 * t36 / 0.2D1 + t53 - t262 * t257 * t78 / 0.6D1 - t257 * 
     #t45 - t269 * t135 / 0.2D1 - t272 + t269 * t253 * t144 / 0.6D1 + t2
     #53 * t172) + t10 * t245 - 0.180D3 * t48 * t34 * (t253 * t135 - t26
     #9 * t144 / 0.2D1 - t172 - t257 * t36 + t262 * t78 / 0.2D1 + t45)) 
     #* t159 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t294)
      t298 = -0.1D1 + x4
      t300 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t301 = x4 * t298
      t304 = log(-0.4D1 * t16 * t301)
      t305 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t310 = t304 ** 2
      t313 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t317 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t322 = t34 * t305
      t334 = t83 * t298
      t337 = log(-0.4D1 * t220 * t334)
      t339 = t337 ** 2
      t345 = log(0.4D1 * t255 * t301 * t138)
      t346 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t348 = t345 ** 2
      t349 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t352 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t362 = t349 - t305
      t371 = log(-0.4D1 * t82 * t334)
      t373 = t371 ** 2
      t391 = log(-0.4D1 * t137 * t334)
      t397 = log(0.4D1 * t137 * t83 * t298 * t138)
      t410 = -(-t39 * t34 * (t300 - t304 * t305) - 0.90D2 * t56 * (t310 
     #* t300 / 0.2D1 + t313 - t310 * t304 * t305 / 0.6D1 - t304 * t317) 
     #- t10 * t322 + 0.180D3 * t48 * t34 * (-t304 * t300 + t310 * t305 /
     # 0.2D1 + t317)) * t104 / 0.1440D4 - (0.90D2 * t56 * (t337 * t300 -
     # t339 * t305 / 0.2D1 - t317 - t345 * t346 + t348 * t349 / 0.2D1 + 
     #t352) - 0.180D3 * t48 * t34 * (-t300 + t337 * t305 + t346 - t345 *
     # t349) + t39 * t34 * t362) * t159 * t104 / 0.1440D4 + (0.90D2 * t5
     #6 * (-t371 * t300 + t373 * t305 / 0.2D1 + t317) - 0.180D3 * t48 * 
     #t34 * (t300 - t371 * t305) + t39 * t322) * t102 * t104 / 0.720D3 +
     # (0.90D2 * t56 * (t300 - t391 * t305 - t346 + t397 * t349) + 0.180
     #D3 * t48 * t34 * t362) * t159 * t161 / 0.720D3
      t411 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t298, 0.0D0,
     # t410)
      t414 = -0.1D1 + x1
      t416 = t414 ** 2
      t417 = t81 * t416
      t418 = t417 * x4
      t421 = log(0.4D1 * t16 * t418)
      t422 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t424 = t421 ** 2
      t425 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t428 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t437 = t34 * t425
      t438 = t39 * t437
      t444 = log(0.4D1 * t16 * t417)
      t449 = t444 ** 2
      t452 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t472 = log(0.4D1 * t255 * t418)
      t486 = log(0.4D1 * t220 * t15 * t81 * t416)
      t488 = t486 ** 2
      t503 = (0.90D2 * t56 * (-t421 * t422 + t424 * t425 / 0.2D1 + t428)
     # - 0.180D3 * t48 * t34 * (t422 - t421 * t425) + t438) * t102 * t10
     #4 / 0.720D3 - (-t39 * t34 * (t422 - t444 * t425) - 0.90D2 * t56 * 
     #(t449 * t422 / 0.2D1 + t452 - t449 * t444 * t425 / 0.6D1 - t444 * 
     #t428) - t10 * t437 + 0.180D3 * t48 * t34 * (-t444 * t422 + t449 * 
     #t425 / 0.2D1 + t428)) * t102 / 0.720D3 + (0.90D2 * t56 * (t422 - t
     #472 * t425) - 0.180D3 * t48 * t437) * t159 * t161 / 0.720D3 + (0.9
     #0D2 * t56 * (-t486 * t422 + t488 * t425 / 0.2D1 + t428) - 0.180D3 
     #* t48 * t34 * (t422 - t486 * t425) + t438) * t159 * t102 / 0.720D3
      t504 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t414, 0.0D0,
     # t503)
      t506 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t507 = s * t506
      t508 = t1 * x1
      t510 = t1 * t414
      t511 = t510 * x4
      t513 = t510 * t298
      t515 = t506 ** 2
      t517 = t1 ** 2
      t519 = x1 * t414
      t522 = t515 ** 2
      t524 = t301 * t522 * t416
      t527 = log(-0.4D1 * t107 * t524)
      t529 = 0.1D1 / (-0.2D1 + t506)
      t530 = t527 * t529
      t531 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t533 = t527 ** 2
      t535 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t538 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t543 = t529 * t531
      t550 = t34 * t529 * t535
      t557 = log(-0.4D1 * t173 * t524)
      t569 = (0.90D2 * t56 * (-t530 * t531 + t533 * t529 * t535 / 0.2D1 
     #+ t529 * t538) - 0.180D3 * t48 * t34 * (t543 - t530 * t535) + t39 
     #* t550) * t102 * t104 / 0.720D3 + (0.90D2 * t56 * (t543 - t557 * t
     #529 * t535) - 0.180D3 * t48 * t550) * t159 * t161 / 0.720D3
      t570 = FJET(XB1, XB2, s, 0.0D0, t507 * t508, -t507 * t511, t507 * 
     #t513, -s * t515 * t517 * t519 * x4, t569)
      t572 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t573 = s * t572
      t574 = t508 * x3
      t576 = t508 * t138
      t579 = t572 ** 2
      t585 = 0.1D1 / (-0.2D1 + t572)
      t586 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t587 = t585 * t586
      t588 = t416 * t138
      t589 = t579 ** 2
      t594 = log(-0.4D1 * t173 * t588 * x4 * t589)
      t596 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t602 = t34 * t585 * t596
      t612 = log(-0.4D1 * t255 * t417 * t138 * t589)
      t613 = t612 * t585
      t615 = t612 ** 2
      t619 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t634 = (0.90D2 * t56 * (t587 - t594 * t585 * t596) - 0.180D3 * t48
     # * t602) * t159 * t161 / 0.720D3 + (0.90D2 * t56 * (-t613 * t586 +
     # t615 * t585 * t596 / 0.2D1 + t585 * t619) - 0.180D3 * t48 * t34 *
     # (t587 - t613 * t596) + t39 * t602) * t159 * t102 / 0.720D3
      t635 = FJET(XB1, XB2, s, t573 * t574, -t573 * t576, 0.0D0, -t573 *
     # t510, -s * t579 * t517 * t519 * x3, t634)
      t637 = KAPPA2(x1, x2, x3, x4, z)
      t638 = s * t637
      t643 = t637 ** 2
      t648 = cos(t11)
      t651 = Sqrt(x3 * t138 * t301)
      t658 = 0.1D1 / (-0.2D1 + t637)
      t659 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t661 = t643 ** 2
      t666 = log(0.4D1 * t173 * t301 * t588 * t661)
      t668 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t677 = -0.90D2 * t56 * (t658 * t659 - t666 * t658 * t668) + 0.180D
     #3 * t48 * t34 * t658 * t668
      t681 = FJET(XB1, XB2, s, t638 * t574, -t638 * t576, -t638 * t511, 
     #t638 * t513, s * t643 * t517 * t519 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t648 * t651), t677 * t159 * t161 / 0.720D3)
      rrgg2gght5s8e1 = t295 * t294 + t411 * t410 + t504 * t503 + t570 * 
     #t569 + t635 * t634 + t681 * t677 * t159 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s8e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t5 = 0.3141592653589793D1 ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = 0.3141592653589793D1 * t7
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 * 0.3141592653589793D1
      t20 = t16 ** 2
      t21 = t20 * 0.3141592653589793D1
      t24 = s ** 2
      t26 = 0.1D1 / t24 / s
      t28 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t31 = 0.3141592653589793D1 * t26
      t32 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t49 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t52 = 0.3141592653589793D1 * lh
      t57 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t60 = x3 * t11
      t61 = t13 * x4
      t64 = log(0.4D1 * t60 * t61)
      t66 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t67 = -0.1D1 + x3
      t68 = t13 * t67
      t72 = log(-0.4D1 * t60 * t68 * x4)
      t73 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t78 = -t73 + t49
      t79 = t26 * t78
      t83 = 0.1D1 / x3
      t85 = 0.1D1 / x4
      t90 = log(-0.4D1 * t60 * t68)
      t92 = t90 ** 2
      t95 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t96 = t60 * t13
      t98 = log(0.4D1 * t96)
      t100 = t98 ** 2
      t118 = log(0.4D1 * t14 * x4)
      t120 = t118 ** 2
      t131 = t26 * t49
      t132 = t8 * t131
      t136 = x1 ** 2
      t137 = t136 * t11
      t140 = log(0.4D1 * t137 * t61)
      t148 = 0.1D1 / x1
      t152 = -t78
      t155 = t83 * t148 * t85
      t158 = x3 * t136
      t162 = log(-0.4D1 * t158 * t14 * t67)
      t166 = log(0.4D1 * t158 * t14)
      t178 = t137 * t13
      t180 = log(0.4D1 * t178)
      t182 = t180 ** 2
      t196 = -(t8 + 0.180D3 * t17 * lh + 0.45D2 * t21) * t26 * t28 / 0.1
     #440D4 - t31 * t32 / 0.16D2 - (0.3141592653589793D1 * (0.60D2 * lh 
     #* t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t17 * t7 - 0.90
     #D2 * t21 * lh - 0.15D2 * t20 * t16 * 0.3141592653589793D1) * t26 *
     # t49 / 0.1440D4 - (-0.180D3 * t52 - 0.90D2 * t17) * t26 * t57 / 0.
     #1440D4 - (0.90D2 * t31 * (t28 - t64 * t49 - t66 + t72 * t73) - 0.1
     #80D3 * t52 * t79) * t83 * t85 / 0.1440D4 - (0.90D2 * t31 * (t90 * 
     #t66 - t92 * t73 / 0.2D1 - t95 - t98 * t28 + t100 * t49 / 0.2D1 + t
     #57) - 0.180D3 * t52 * t26 * (-t66 + t90 * t73 + t28 - t98 * t49) +
     # t8 * t79) * t83 / 0.1440D4 - (0.90D2 * t31 * (-t118 * t28 + t120 
     #* t49 / 0.2D1 + t57) - 0.180D3 * t52 * t26 * (t28 - t118 * t49) + 
     #t132) * t85 / 0.1440D4 + (0.90D2 * t31 * (-t28 + t140 * t49) + 0.1
     #80D3 * t52 * t131) * t148 * t85 / 0.720D3 + t31 * t152 * t155 / 0.
     #8D1 + (0.90D2 * t31 * (t66 - t162 * t73 - t28 + t166 * t49) - 0.18
     #0D3 * t52 * t26 * t152) * t83 * t148 / 0.720D3 - (0.90D2 * t31 * (
     #-t180 * t28 + t182 * t49 / 0.2D1 + t57) - 0.180D3 * t52 * t26 * (t
     #28 - t180 * t49) + t132) * t148 / 0.720D3
      t197 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t196)
      t200 = -0.1D1 + x4
      t202 = x4 * t200
      t205 = log(-0.4D1 * t14 * t202)
      t206 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t208 = t205 ** 2
      t209 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t212 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t221 = t26 * t209
      t226 = t61 * t200
      t229 = log(-0.4D1 * t137 * t226)
      t240 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t241 = t209 - t240
      t247 = log(-0.4D1 * t60 * t226)
      t249 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t253 = log(0.4D1 * t96 * t202 * t67)
      t266 = -(-0.90D2 * t31 * (-t205 * t206 + t208 * t209 / 0.2D1 + t21
     #2) + 0.180D3 * t52 * t26 * (t206 - t205 * t209) - t8 * t221) * t85
     # / 0.1440D4 + (0.90D2 * t31 * (t206 - t229 * t209) - 0.180D3 * t52
     # * t221) * t148 * t85 / 0.720D3 + t31 * t241 * t155 / 0.8D1 - (0.9
     #0D2 * t31 * (-t206 + t247 * t209 + t249 - t253 * t240) + 0.180D3 *
     # t52 * t26 * t241) * t83 * t85 / 0.1440D4
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t200, 0.0D0,
     # t266)
      t270 = -0.1D1 + x1
      t272 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t273 = t270 ** 2
      t274 = t136 * t273
      t278 = log(0.4D1 * t14 * t274 * x4)
      t279 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t284 = t26 * t279
      t286 = 0.180D3 * t52 * t284
      t298 = log(0.4D1 * t60 * t13 * t136 * t273)
      t309 = log(0.4D1 * t14 * t274)
      t311 = t309 ** 2
      t314 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t327 = (0.90D2 * t31 * (t272 - t278 * t279) - t286) * t148 * t85 /
     # 0.720D3 + t31 * t279 * t155 / 0.8D1 + (0.90D2 * t31 * (t272 - t29
     #8 * t279) - t286) * t83 * t148 / 0.720D3 - (-0.90D2 * t31 * (-t309
     # * t272 + t311 * t279 / 0.2D1 + t314) + 0.180D3 * t52 * t26 * (t27
     #2 - t309 * t279) - t8 * t284) * t148 / 0.720D3
      t328 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t270, 0.0D0,
     # t327)
      t330 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t331 = s * t330
      t332 = t1 * x1
      t334 = t1 * t270
      t335 = t334 * x4
      t337 = t334 * t200
      t339 = t330 ** 2
      t341 = t1 ** 2
      t343 = x1 * t270
      t347 = 0.1D1 / (-0.2D1 + t330)
      t348 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t350 = t339 ** 2
      t355 = log(-0.4D1 * t178 * t202 * t350 * t273)
      t357 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t372 = t148 * t85
      t376 = (0.90D2 * t31 * (t347 * t348 - t355 * t347 * t357) - 0.180D
     #3 * t52 * t26 * t347 * t357) * t148 * t85 / 0.720D3 + t31 * t347 *
     # t357 * t83 * t372 / 0.8D1
      t377 = FJET(XB1, XB2, s, 0.0D0, t331 * t332, -t331 * t335, t331 * 
     #t337, -s * t339 * t341 * t343 * x4, t376)
      t379 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t380 = s * t379
      t381 = t332 * x3
      t383 = t332 * t67
      t386 = t379 ** 2
      t392 = 0.1D1 / (-0.2D1 + t379)
      t394 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t399 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t401 = t386 ** 2
      t406 = log(-0.4D1 * t96 * t274 * t67 * t401)
      t420 = t31 * t392 * t394 * t83 * t372 / 0.8D1 + (0.90D2 * t31 * (t
     #392 * t399 - t406 * t392 * t394) - 0.180D3 * t52 * t26 * t392 * t3
     #94) * t83 * t148 / 0.720D3
      t421 = FJET(XB1, XB2, s, t380 * t381, -t380 * t383, 0.0D0, -t380 *
     # t334, -s * t386 * t341 * t343 * x3, t420)
      t423 = KAPPA2(x1, x2, x3, x4, z)
      t424 = s * t423
      t429 = t423 ** 2
      t434 = cos(t9)
      t437 = Sqrt(x3 * t67 * t202)
      t444 = 0.1D1 / (-0.2D1 + t423)
      t446 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t448 = t446 * t83 * t372
      t451 = FJET(XB1, XB2, s, t424 * t381, -t424 * t383, -t424 * t335, 
     #t424 * t337, s * t429 * t341 * t343 * (-x3 - x4 + 0.2D1 * x3 * x4 
     #+ 0.2D1 * t434 * t437), -t31 * t444 * t448 / 0.8D1)
      rrgg2gght5s8e0 = t197 * t196 + t267 * t266 + t328 * t327 + t377 * 
     #t376 + t421 * t420 - t451 * 0.3141592653589793D1 * t26 * t444 * t4
     #48 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s8em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t12 = log(0.4D1 * t10)
      t13 = t12 * 0.3141592653589793D1
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t23 = lh ** 2
      t25 = 0.3141592653589793D1 ** 2
      t31 = t12 ** 2
      t36 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t39 = t18 * 0.3141592653589793D1
      t40 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t43 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t44 = -t43 + t36
      t45 = 0.1D1 / x3
      t47 = 0.1D1 / x4
      t51 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t52 = x3 * t7
      t53 = -0.1D1 + x3
      t57 = log(-0.4D1 * t52 * t9 * t53)
      t61 = log(0.4D1 * t52 * t9)
      t74 = log(0.4D1 * t10 * x4)
      t81 = 0.180D3 * t3 * t18 * t36
      t87 = 0.1D1 / x1
      t91 = x1 ** 2
      t95 = log(0.4D1 * t91 * t7 * t9)
      t107 = -(-0.180D3 * t3 - 0.90D2 * t13) * t18 * t20 / 0.1440D4 - (0
     #.3141592653589793D1 * (0.180D3 * t23 - 0.30D2 * t25) + 0.180D3 * t
     #13 * lh + 0.45D2 * t31 * 0.3141592653589793D1) * t18 * t36 / 0.144
     #0D4 - t39 * t40 / 0.16D2 - t39 * t44 * t45 * t47 / 0.16D2 - (0.90D
     #2 * t39 * (-t51 + t57 * t43 + t20 - t61 * t36) - 0.180D3 * t3 * t1
     #8 * t44) * t45 / 0.1440D4 - (0.90D2 * t39 * (t20 - t74 * t36) - t8
     #1) * t47 / 0.1440D4 - t39 * t44 * t45 * t87 / 0.8D1 - (0.90D2 * t3
     #9 * (t20 - t95 * t36) - t81) * t87 / 0.720D3 - t39 * t36 * t87 * t
     #47 / 0.8D1
      t108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t107)
      t111 = -0.1D1 + x4
      t113 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t117 = log(-0.4D1 * t10 * x4 * t111)
      t118 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t133 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t139 = -(-0.90D2 * t39 * (t113 - t117 * t118) + 0.180D3 * t3 * t18
     # * t118) * t47 / 0.1440D4 + t39 * t118 * t87 * t47 / 0.8D1 - t39 *
     # (t133 - t118) * t45 * t47 / 0.16D2
      t140 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t111, 0.0D0,
     # t139)
      t143 = -0.1D1 + x1
      t145 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t150 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t151 = t143 ** 2
      t155 = log(0.4D1 * t10 * t91 * t151)
      t170 = t39 * t145 * t45 * t87 / 0.8D1 - (-0.90D2 * t39 * (t150 - t
     #155 * t145) + 0.180D3 * t3 * t18 * t145) * t87 / 0.720D3 + t39 * t
     #145 * t87 * t47 / 0.8D1
      t171 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t143, 0.0D0,
     # t170)
      t173 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t174 = s * t173
      t175 = t1 * x1
      t177 = t1 * t143
      t182 = t173 ** 2
      t184 = t1 ** 2
      t186 = x1 * t143
      t190 = 0.1D1 / (-0.2D1 + t173)
      t192 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t197 = FJET(XB1, XB2, s, 0.0D0, t174 * t175, -t174 * t177 * x4, t1
     #74 * t177 * t111, -s * t182 * t184 * t186 * x4, t39 * t190 * t192 
     #* t87 * t47 / 0.8D1)
      t205 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t206 = s * t205
      t212 = t205 ** 2
      t218 = 0.1D1 / (-0.2D1 + t205)
      t220 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t225 = FJET(XB1, XB2, s, t206 * t175 * x3, -t206 * t175 * t53, 0.0
     #D0, -t206 * t177, -s * t212 * t184 * t186 * x3, t39 * t218 * t220 
     #* t45 * t87 / 0.8D1)
      rrgg2gght5s8em1 = t108 * t107 + t140 * t139 + t171 * t170 + t197 *
     # 0.3141592653589793D1 * t18 * t190 * t192 * t87 * t47 / 0.8D1 + t2
     #25 * 0.3141592653589793D1 * t18 * t218 * t220 * t45 * t87 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s8em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = 0.3141592653589793D1 * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t8 = 0.1D1 / x4
      t12 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t18 = 0.1D1 / x1
      t22 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t28 = sin(x2 * 0.3141592653589793D1)
      t29 = t28 ** 2
      t30 = z ** 2
      t34 = log(0.4D1 * t29 / t30)
      t41 = -t6 * t7 * t8 / 0.16D2 - t6 * (-t12 + t7) / x3 / 0.16D2 - t6
     # * t7 * t18 / 0.8D1 - t6 * t22 / 0.16D2 - (-0.180D3 * 0.3141592653
     #589793D1 * lh - 0.90D2 * 0.3141592653589793D1 * t34) * t5 * t7 / 0
     #.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t47 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, t6 * t47 * t8 / 0.16D2)
      t60 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t64 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, t6 * t60 * t18 / 0.8D1)
      rrgg2gght5s8em2 = t42 * t41 + t51 * 0.3141592653589793D1 * t5 * t4
     #7 * t8 / 0.16D2 + t64 * 0.3141592653589793D1 * t5 * t60 * t18 / 0.
     #8D1

      end function



      doubleprecision function rrgg2gght5s8em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * (-0.1D1 + z), 0.0
     #D0, -0.3141592653589793D1 * t5 * t7 / 0.16D2)
      rrgg2gght5s8em3 = -t10 * 0.3141592653589793D1 * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s8em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6
      rrgg2gght5s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh51J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t11 = t3 * t6 * t9
      t12 = 0.1D1 - x1
      t13 = t12 ** 2
      t18 = cos(x2 * 0.3141592653589793D1)
      t19 = 0.1D1 - x3
      t21 = 0.1D1 - x4
      t24 = Sqrt(x3 * t19 * x4 * t21)
      t27 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t18 * t24
      t28 = x1 * t27
      t36 = x1 ** 2
      t38 = t12 * t27
      t42 = t5 ** 2
      t43 = t42 * t4
      t44 = t8 ** 2
      t45 = t44 * t7
      t47 = t3 * t43 * t45
      t48 = t36 * x1
      t50 = t27 ** 2
      t51 = t13 * t50
      t55 = t2 * t42
      t56 = t13 * t12
      t59 = x4 ** 2
      t63 = t6 * t3
      t64 = t9 * t48
      t65 = t19 ** 2
      t69 = x3 ** 2
      t74 = t21 ** 2
      t84 = t3 * t4
      t85 = t7 * t12
      t109 = -t2 + 0.4D1 * t11 * t13 * x4 * t28 + 0.4D1 * t11 * t13 * t2
     #1 * t28 + 0.4D1 * t11 * t36 * x3 * t38 + 0.2D1 * t47 * t48 * x3 * 
     #t51 - t55 * t44 * t56 * t59 * z * t28 + t63 * t64 * t65 * t19 + t6
     #3 * t64 * t69 * x3 + t63 * t9 * t56 * t74 * t21 + t2 * t6 * t9 * t
     #56 * t59 * x4 * z + 0.2D1 * t84 * t85 * t21 - 0.3D1 * t55 * t44 * 
     #t36 * t13 * t50 - 0.2D1 * t2 * t5 * t8 * x1 * t12 * t27 - 0.2D1 * 
     #t2 * t42 * t5 * t44 * t8 * t48 * t56 * t50 * t27
      t113 = t3 * t5
      t114 = t8 * t36
      t117 = t8 * t13
      t124 = t7 * x1
      t131 = t42 ** 2
      t133 = t44 ** 2
      t135 = t36 ** 2
      t136 = t13 ** 2
      t138 = t50 ** 2
      t142 = t3 * t42 * t44
      t153 = t36 * t50
      t172 = 0.2D1 * t84 * t85 * x4 - t113 * t114 * t69 - t113 * t117 * 
     #t74 - t113 * t117 * t59 - t113 * t114 * t65 + 0.2D1 * t84 * t124 *
     # x3 + 0.2D1 * t84 * t124 * t19 - t2 * t131 * t133 * t135 * t136 * 
     #t138 - t142 * t56 * t74 * t28 - t142 * t48 * t65 * t38 - t142 * t4
     #8 * t69 * t38 + 0.2D1 * t47 * t56 * t21 * t153 + 0.2D1 * t2 * t43 
     #* t45 * t56 * x4 * z * t153 + 0.2D1 * t47 * t48 * t19 * t51 + 0.4D
     #1 * t11 * t36 * t19 * t38
      rrgg2ggh51J1 = -0.9D1 * wd * (t109 + t172) / s / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh51J2
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
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t11 * t10
      t15 = x4 ** 2
      t20 = cos(x2 * 0.3141592653589793D1)
      t21 = 0.1D1 - x3
      t23 = 0.1D1 - x4
      t26 = Sqrt(x3 * t21 * x4 * t23)
      t29 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t20 * t26
      t30 = x1 * t29
      t32 = t6 * t9 * t12 * t15 * z * t30
      t33 = t2 * z
      t35 = t33 * t5 * t9
      t36 = t23 ** 2
      t39 = t35 * t12 * t36 * t30
      t40 = x1 ** 2
      t41 = t40 * x1
      t42 = t21 ** 2
      t44 = t10 * t29
      t46 = t35 * t41 * t42 * t44
      t47 = x3 ** 2
      t50 = t35 * t41 * t47 * t44
      t51 = t3 * t5
      t52 = t7 * t9
      t54 = t33 * t51 * t52
      t56 = t29 ** 2
      t57 = t40 * t56
      t60 = 0.2D1 * t54 * t12 * t23 * t57
      t67 = 0.2D1 * t2 * t51 * t52 * t12 * x4 * z * t57
      t69 = t11 * t56
      t72 = 0.2D1 * t54 * t41 * t21 * t69
      t73 = t3 * t4
      t74 = t8 * t7
      t76 = t33 * t73 * t74
      t79 = t76 * t40 * t21 * t44
      t83 = t76 * t11 * x4 * t30
      t87 = t76 * t11 * t23 * t30
      t91 = t76 * t40 * x3 * t44
      t96 = 0.2D1 * t54 * t41 * x3 * t69
      t97 = t33 * t73
      t98 = t74 * t41
      t101 = t97 * t98 * t42 * t21
      t102 = -t2 - t32 - t39 - t46 - t50 + t60 + t67 + t72 + 0.4D1 * t79
     # + 0.4D1 * t83 + 0.4D1 * t87 + 0.4D1 * t91 + t96 + t101
      t116 = t33 * t3
      t117 = t7 * t10
      t144 = t33 * t4
      t145 = t8 * t40
      t148 = t8 * t11
      t155 = t7 * x1
      t162 = t5 ** 2
      t164 = t9 ** 2
      t166 = t40 ** 2
      t167 = t11 ** 2
      t169 = t56 ** 2
      t172 = t97 * t98 * t47 * x3 + t97 * t74 * t12 * t36 * t23 + t2 * t
     #73 * t74 * t12 * t15 * x4 * z + 0.2D1 * t116 * t117 * t23 - 0.3D1 
     #* t6 * t9 * t40 * t11 * t56 - 0.2D1 * t2 * t4 * t8 * x1 * t10 * t2
     #9 - 0.2D1 * t2 * t5 * t4 * t9 * t8 * t41 * t12 * t56 * t29 + 0.2D1
     # * t116 * t117 * x4 - t144 * t145 * t47 - t144 * t148 * t36 - t144
     # * t148 * t15 - t144 * t145 * t42 + 0.2D1 * t116 * t155 * x3 + 0.2
     #D1 * t116 * t155 * t21 - t2 * t162 * t164 * t166 * t167 * t169
      t180 = t2 + t32 + t39 + t46 + t50 - t60 - t67 - t72 - 0.7D1 * t79 
     #- 0.7D1 * t83 - 0.7D1 * t87 - 0.7D1 * t91 - t96 - t101
      rrgg2ggh51J2 = -0.9D1 * (0.2D1 * wd * (t102 + t172) + wd * (t180 -
     # t172)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh51J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t11 = t3 * t6 * t9
      t12 = x1 ** 2
      t13 = 0.1D1 - x3
      t15 = 0.1D1 - x1
      t19 = cos(x2 * 0.3141592653589793D1)
      t21 = 0.1D1 - x4
      t24 = Sqrt(x3 * t13 * x4 * t21)
      t27 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t19 * t24
      t28 = t15 * t27
      t30 = t11 * t12 * t13 * t28
      t32 = t15 ** 2
      t34 = x1 * t27
      t36 = t11 * t32 * x4 * t34
      t40 = t11 * t32 * t21 * t34
      t42 = t5 ** 2
      t43 = t2 * t42
      t44 = t8 ** 2
      t45 = t32 * t15
      t48 = x4 ** 2
      t51 = t43 * t44 * t45 * t48 * z * t34
      t53 = t3 * t42 * t44
      t54 = t21 ** 2
      t57 = t53 * t45 * t54 * t34
      t58 = t12 * x1
      t59 = t13 ** 2
      t62 = t53 * t58 * t59 * t28
      t63 = x3 ** 2
      t66 = t53 * t58 * t63 * t28
      t67 = t42 * t4
      t68 = t44 * t7
      t70 = t3 * t67 * t68
      t72 = t27 ** 2
      t73 = t12 * t72
      t76 = 0.2D1 * t70 * t45 * t21 * t73
      t83 = 0.2D1 * t2 * t67 * t68 * t45 * x4 * z * t73
      t85 = t32 * t72
      t88 = 0.2D1 * t70 * t58 * t13 * t85
      t89 = t6 * t3
      t90 = t9 * t58
      t93 = t89 * t90 * t59 * t13
      t96 = t89 * t90 * t63 * x3
      t100 = t89 * t9 * t45 * t54 * t21
      t101 = 0.4D1 * t30 + 0.4D1 * t36 - t2 + 0.4D1 * t40 - t51 - t57 - 
     #t62 - t66 + t76 + t83 + t88 + t93 + t96 + t100
      t107 = t2 * t6 * t9 * t45 * t48 * x4 * z
      t108 = t3 * t4
      t109 = t7 * t15
      t112 = 0.2D1 * t108 * t109 * t21
      t117 = 0.3D1 * t43 * t44 * t12 * t32 * t72
      t123 = 0.2D1 * t2 * t5 * t8 * x1 * t15 * t27
      t132 = 0.2D1 * t2 * t42 * t5 * t44 * t8 * t58 * t45 * t72 * t27
      t135 = 0.2D1 * t108 * t109 * x4
      t136 = t3 * t5
      t137 = t8 * t12
      t139 = t136 * t137 * t63
      t140 = t8 * t32
      t142 = t136 * t140 * t54
      t144 = t136 * t140 * t48
      t146 = t136 * t137 * t59
      t147 = t7 * x1
      t150 = 0.2D1 * t108 * t147 * x3
      t153 = 0.2D1 * t108 * t147 * t13
      t154 = t42 ** 2
      t156 = t44 ** 2
      t158 = t12 ** 2
      t159 = t32 ** 2
      t161 = t72 ** 2
      t163 = t2 * t154 * t156 * t158 * t159 * t161
      t166 = t11 * t12 * x3 * t28
      t171 = 0.2D1 * t70 * t58 * x3 * t85
      t172 = t107 + t112 - t117 - t123 - t132 + t135 - t139 - t142 - t14
     #4 - t146 + t150 + t153 - t163 + 0.4D1 * t166 + t171
      t179 = -0.7D1 * t30 - 0.7D1 * t36 + t2 - 0.7D1 * t40 + t51 + t57 +
     # t62 + t66 - t76 - t83 - t88 - t93 - t96 - t100
      t181 = -t107 - t112 + t117 + t123 + t132 - t135 + t139 + t142 + t1
     #44 + t146 - t150 - t153 + t163 - 0.7D1 * t166 - t171
      rrgg2ggh51J3 = -0.9D1 * (0.3D1 * wd * (t101 + t172) + 0.2D1 * wd *
     # (t179 + t181)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh51J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t6 * t3
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 * t8
      t11 = x1 ** 2
      t12 = t11 * x1
      t13 = t10 * t12
      t14 = 0.1D1 - x3
      t15 = t14 ** 2
      t19 = x3 ** 2
      t23 = 0.1D1 - x1
      t24 = t23 ** 2
      t25 = t24 * t23
      t27 = 0.1D1 - x4
      t28 = t27 ** 2
      t34 = x4 ** 2
      t39 = t3 * t4
      t40 = t8 * t23
      t44 = t5 ** 2
      t45 = t2 * t44
      t46 = t9 ** 2
      t52 = cos(x2 * 0.3141592653589793D1)
      t56 = Sqrt(x3 * t14 * x4 * t27)
      t59 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t52 * t56
      t60 = t59 ** 2
      t82 = t3 * t5
      t83 = t9 * t11
      t86 = t9 * t24
      t93 = t8 * x1
      t97 = t7 * t13 * t15 * t14 + t7 * t13 * t19 * x3 + t7 * t10 * t25 
     #* t28 * t27 + t2 * t6 * t10 * t25 * t34 * x4 * z + 0.2D1 * t39 * t
     #40 * t27 - 0.3D1 * t45 * t46 * t11 * t24 * t60 - 0.2D1 * t2 * t5 *
     # t9 * x1 * t23 * t59 - 0.2D1 * t2 * t44 * t5 * t46 * t9 * t12 * t2
     #5 * t60 * t59 + 0.2D1 * t39 * t40 * x4 - t82 * t83 * t19 - t82 * t
     #86 * t28 - t82 * t86 * t34 - t82 * t83 * t15 + 0.2D1 * t39 * t93 *
     # x3
      t100 = 0.2D1 * t39 * t93 * t14
      t101 = t44 ** 2
      t103 = t46 ** 2
      t105 = t11 ** 2
      t106 = t24 ** 2
      t108 = t60 ** 2
      t110 = t2 * t101 * t103 * t105 * t106 * t108
      t114 = x1 * t59
      t116 = t45 * t46 * t25 * t34 * z * t114
      t118 = t3 * t6 * t10
      t120 = t23 * t59
      t122 = t118 * t11 * t14 * t120
      t126 = t118 * t24 * x4 * t114
      t130 = t118 * t24 * t27 * t114
      t134 = t118 * t11 * x3 * t120
      t136 = t44 * t4
      t137 = t46 * t8
      t139 = t3 * t136 * t137
      t141 = t24 * t60
      t144 = 0.2D1 * t139 * t12 * x3 * t141
      t146 = t3 * t44 * t46
      t149 = t146 * t25 * t28 * t114
      t152 = t146 * t12 * t15 * t120
      t155 = t146 * t12 * t19 * t120
      t157 = t11 * t60
      t160 = 0.2D1 * t139 * t25 * t27 * t157
      t167 = 0.2D1 * t2 * t136 * t137 * t25 * x4 * z * t157
      t171 = 0.2D1 * t139 * t12 * t14 * t141
      t172 = t100 - t110 - t116 + 0.4D1 * t122 + 0.4D1 * t126 + 0.4D1 * 
     #t130 + 0.4D1 * t134 + t144 - t149 - t152 - t155 + t160 + t167 + t1
     #71 - t2
      t181 = -t100 + t110 + t116 - 0.7D1 * t122 - 0.7D1 * t126 - 0.7D1 *
     # t130 - 0.7D1 * t134 - t144 + t149 + t152 + t155 - t160 - t167 - t
     #171 + t2
      rrgg2ggh51J4 = -0.9D1 * (0.4D1 * wd * (t97 + t172) + 0.3D1 * wd * 
     #(-t97 + t181)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh51J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t6 * t3
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 * t8
      t11 = x1 ** 2
      t12 = t11 * x1
      t13 = t10 * t12
      t14 = 0.1D1 - x3
      t15 = t14 ** 2
      t19 = x3 ** 2
      t23 = 0.1D1 - x1
      t24 = t23 ** 2
      t25 = t24 * t23
      t27 = 0.1D1 - x4
      t28 = t27 ** 2
      t34 = x4 ** 2
      t39 = t3 * t4
      t40 = t8 * t23
      t44 = t5 ** 2
      t45 = t2 * t44
      t46 = t9 ** 2
      t52 = cos(x2 * 0.3141592653589793D1)
      t56 = Sqrt(x3 * t14 * x4 * t27)
      t59 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t52 * t56
      t60 = t59 ** 2
      t82 = t3 * t5
      t83 = t9 * t11
      t86 = t9 * t24
      t93 = t8 * x1
      t97 = t7 * t13 * t15 * t14 + t7 * t13 * t19 * x3 + t7 * t10 * t25 
     #* t28 * t27 + t2 * t6 * t10 * t25 * t34 * x4 * z + 0.2D1 * t39 * t
     #40 * t27 - 0.3D1 * t45 * t46 * t11 * t24 * t60 - 0.2D1 * t2 * t5 *
     # t9 * x1 * t23 * t59 - 0.2D1 * t2 * t44 * t5 * t46 * t9 * t12 * t2
     #5 * t60 * t59 + 0.2D1 * t39 * t40 * x4 - t82 * t83 * t19 - t82 * t
     #86 * t28 - t82 * t86 * t34 - t82 * t83 * t15 + 0.2D1 * t39 * t93 *
     # x3
      t100 = 0.2D1 * t39 * t93 * t14
      t101 = t44 ** 2
      t103 = t46 ** 2
      t105 = t11 ** 2
      t106 = t24 ** 2
      t108 = t60 ** 2
      t110 = t2 * t101 * t103 * t105 * t106 * t108
      t111 = t44 * t4
      t112 = t46 * t8
      t114 = t3 * t111 * t112
      t116 = t24 * t60
      t119 = 0.2D1 * t114 * t12 * t14 * t116
      t121 = t3 * t6 * t10
      t123 = t23 * t59
      t125 = t121 * t11 * t14 * t123
      t128 = x1 * t59
      t130 = t121 * t24 * x4 * t128
      t134 = t121 * t24 * t27 * t128
      t138 = t121 * t11 * x3 * t123
      t143 = 0.2D1 * t114 * t12 * x3 * t116
      t148 = t45 * t46 * t25 * t34 * z * t128
      t150 = t3 * t44 * t46
      t153 = t150 * t25 * t28 * t128
      t156 = t150 * t12 * t15 * t123
      t159 = t150 * t12 * t19 * t123
      t161 = t11 * t60
      t164 = 0.2D1 * t114 * t25 * t27 * t161
      t171 = 0.2D1 * t2 * t111 * t112 * t25 * x4 * z * t161
      t172 = t100 - t110 + t119 + 0.4D1 * t125 + 0.4D1 * t130 + 0.4D1 * 
     #t134 + 0.4D1 * t138 + t143 - t2 - t148 - t153 - t156 - t159 + t164
     # + t171
      t181 = -t100 + t110 - t119 - 0.7D1 * t125 - 0.7D1 * t130 - 0.7D1 *
     # t134 - 0.7D1 * t138 - t143 + t2 + t148 + t153 + t156 + t159 - t16
     #4 - t171
      rrgg2ggh51J5 = -0.9D1 * (0.5D1 * wd * (t97 + t172) + 0.4D1 * wd * 
     #(-t97 + t181)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh51J6
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
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t12 = t11 * t10
      t15 = x4 ** 2
      t20 = cos(x2 * 0.3141592653589793D1)
      t21 = 0.1D1 - x3
      t23 = 0.1D1 - x4
      t26 = Sqrt(x3 * t21 * x4 * t23)
      t29 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t20 * t26
      t30 = x1 * t29
      t33 = t3 * t5
      t35 = t7 * t9
      t39 = x1 ** 2
      t40 = t29 ** 2
      t41 = t39 * t40
      t45 = t2 * z
      t47 = t45 * t33 * t35
      t48 = t39 * x1
      t50 = t11 * t40
      t54 = t3 * t4
      t55 = t8 * t7
      t57 = t45 * t54 * t55
      t59 = t10 * t29
      t72 = t45 * t5 * t9
      t73 = t23 ** 2
      t77 = t21 ** 2
      t81 = x3 ** 2
      t97 = t45 * t54
      t98 = t55 * t48
      t102 = t6 * t9 * t12 * t15 * z * t30 - 0.2D1 * t2 * t33 * t35 * t1
     #2 * x4 * z * t41 - 0.2D1 * t47 * t48 * t21 * t50 - 0.7D1 * t57 * t
     #39 * t21 * t59 - 0.7D1 * t57 * t11 * x4 * t30 - 0.7D1 * t57 * t11 
     #* t23 * t30 + t72 * t12 * t73 * t30 + t72 * t48 * t77 * t59 + t72 
     #* t48 * t81 * t59 - 0.2D1 * t47 * t12 * t23 * t41 - 0.7D1 * t57 * 
     #t39 * x3 * t59 - 0.2D1 * t47 * t48 * x3 * t50 + t2 - t97 * t98 * t
     #77 * t21
      t116 = t45 * t3
      t117 = t7 * t10
      t144 = t45 * t4
      t145 = t8 * t39
      t148 = t8 * t11
      t155 = t7 * x1
      t162 = t5 ** 2
      t164 = t9 ** 2
      t166 = t39 ** 2
      t167 = t11 ** 2
      t169 = t40 ** 2
      t172 = -t97 * t98 * t81 * x3 - t97 * t55 * t12 * t73 * t23 - t2 * 
     #t54 * t55 * t12 * t15 * x4 * z - 0.2D1 * t116 * t117 * t23 + 0.3D1
     # * t6 * t9 * t39 * t11 * t40 + 0.2D1 * t2 * t4 * t8 * x1 * t10 * t
     #29 + 0.2D1 * t2 * t5 * t4 * t9 * t8 * t48 * t12 * t40 * t29 - 0.2D
     #1 * t116 * t117 * x4 + t144 * t145 * t81 + t144 * t148 * t73 + t14
     #4 * t148 * t15 + t144 * t145 * t77 - 0.2D1 * t116 * t155 * x3 - 0.
     #2D1 * t116 * t155 * t21 + t2 * t162 * t164 * t166 * t167 * t169
      rrgg2ggh51J6 = -0.45D2 * wd * (t102 + t172) / s / z / 0.3141592653
     #589793D1

      end function
  
 