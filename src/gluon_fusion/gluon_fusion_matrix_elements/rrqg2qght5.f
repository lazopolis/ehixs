  
      subroutine rrqg2qght5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh51J1  
      doubleprecision rrqg2qgh51J2  
      doubleprecision rrqg2qgh51J3  
      doubleprecision rrqg2qgh51J4  
      doubleprecision rrqg2qgh51J5  
      doubleprecision rrqg2qgh51J6  
      doubleprecision rrqg2qght5s1e1  
      doubleprecision rrqg2qght5s1e0  
      doubleprecision rrqg2qght5s1em1  
      doubleprecision rrqg2qght5s1em2  
      doubleprecision rrqg2qght5s1em3  
      doubleprecision rrqg2qght5s1em4  
      doubleprecision rrqg2qght5s2e1  
      doubleprecision rrqg2qght5s2e0  
      doubleprecision rrqg2qght5s2em1  
      doubleprecision rrqg2qght5s2em2  
      doubleprecision rrqg2qght5s2em3  
      doubleprecision rrqg2qght5s2em4  
      doubleprecision rrqg2qght5s3e1  
      doubleprecision rrqg2qght5s3e0  
      doubleprecision rrqg2qght5s3em1  
      doubleprecision rrqg2qght5s3em2  
      doubleprecision rrqg2qght5s3em3  
      doubleprecision rrqg2qght5s3em4  
      doubleprecision rrqg2qght5s4e1  
      doubleprecision rrqg2qght5s4e0  
      doubleprecision rrqg2qght5s4em1  
      doubleprecision rrqg2qght5s4em2  
      doubleprecision rrqg2qght5s4em3  
      doubleprecision rrqg2qght5s4em4  
      doubleprecision rrqg2qght5s5e1  
      doubleprecision rrqg2qght5s5e0  
      doubleprecision rrqg2qght5s5em1  
      doubleprecision rrqg2qght5s5em2  
      doubleprecision rrqg2qght5s5em3  
      doubleprecision rrqg2qght5s5em4  
      doubleprecision rrqg2qght5s6e1  
      doubleprecision rrqg2qght5s6e0  
      doubleprecision rrqg2qght5s6em1  
      doubleprecision rrqg2qght5s6em2  
      doubleprecision rrqg2qght5s6em3  
      doubleprecision rrqg2qght5s6em4  
      doubleprecision rrqg2qght5s7e1  
      doubleprecision rrqg2qght5s7e0  
      doubleprecision rrqg2qght5s7em1  
      doubleprecision rrqg2qght5s7em2  
      doubleprecision rrqg2qght5s7em3  
      doubleprecision rrqg2qght5s7em4  
      doubleprecision rrqg2qght5s8e1  
      doubleprecision rrqg2qght5s8e0  
      doubleprecision rrqg2qght5s8em1  
      doubleprecision rrqg2qght5s8em2  
      doubleprecision rrqg2qght5s8em3  
      doubleprecision rrqg2qght5s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght5s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght5s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqg2qght5s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqg2qght5s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqg2qght5s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqg2qght5s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght5s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght5s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqg2qght5s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqg2qght5s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqg2qght5s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqg2qght5s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght5s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght5s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqg2qght5s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqg2qght5s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqg2qght5s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqg2qght5s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght5s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght5s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqg2qght5s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqg2qght5s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqg2qght5s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqg2qght5s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght5s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght5s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqg2qght5s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqg2qght5s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqg2qght5s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqg2qght5s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght5s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght5s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqg2qght5s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqg2qght5s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqg2qght5s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqg2qght5s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght5s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t102 = t18 * x4
      t103 = -0.1D1 + x4
      t104 = t102 * t103
      t107 = log(-0.4D1 * t91 * t15 * t104)
      t108 = -t103
      t109 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t111 = t107 ** 2
      t112 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t115 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t120 = t35 * t39
      t127 = -t112 + t86
      t128 = t120 * t127
      t129 = t44 * t128
      t131 = 0.1D1 / x1
      t133 = 0.1D1 / x4
      t136 = t91 * t92
      t138 = log(0.4D1 * t136)
      t143 = t138 ** 2
      t154 = t120 * t86
      t155 = t10 * t154
      t166 = x3 * t90
      t167 = t166 * t13
      t170 = log(0.4D1 * t167 * t93)
      t172 = x4 * t103
      t176 = log(-0.4D1 * t167 * t92 * t172)
      t187 = 0.1D1 / x3
      t189 = t131 * t133
      t192 = t166 * t19
      t194 = log(0.4D1 * t192)
      t196 = t194 ** 2
      t215 = log(-0.4D1 * t16 * t104)
      t219 = log(0.4D1 * t16 * t102)
      t224 = t219 ** 2
      t231 = t215 ** 2
      t234 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t257 = x3 * t13
      t260 = log(0.4D1 * t257 * t93)
      t262 = t260 ** 2
      t265 = t257 * t15
      t268 = log(-0.4D1 * t265 * t104)
      t270 = t268 ** 2
      t289 = log(0.4D1 * t257 * t92)
      t294 = t289 ** 2
      t315 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (-t96 * t40 + t98 * t86 / 0.2D1
     # + t50 + t107 * t109 - t111 * t112 / 0.2D1 - t115) - 0.180D3 * t54
     # * t120 * (t40 - t96 * t86 - t109 + t107 * t112) + t129) * t131 * 
     #t133 / 0.720D3 - (t44 * t120 * (-t40 + t138 * t86) + 0.90D2 * t63 
     #* t39 * (-t143 * t40 / 0.2D1 - t59 + t143 * t138 * t86 / 0.6D1 + t
     #138 * t50) - t155 - 0.180D3 * t54 * t120 * (t138 * t40 - t143 * t8
     #6 / 0.2D1 - t50)) * t131 / 0.720D3 - (0.90D2 * t63 * t39 * (-t40 +
     # t170 * t86 + t109 - t176 * t112) + 0.180D3 * t54 * t120 * t127) *
     # t187 * t189 / 0.720D3 + (0.90D2 * t63 * t39 * (-t194 * t40 + t196
     # * t86 / 0.2D1 + t50) - 0.180D3 * t54 * t120 * (t40 - t194 * t86) 
     #+ t44 * t154) * t187 * t131 / 0.720D3 + (t44 * t120 * (-t109 + t21
     #5 * t112 + t40 - t219 * t86) + 0.90D2 * t63 * t39 * (t224 * t40 / 
     #0.2D1 + t59 - t224 * t219 * t86 / 0.6D1 - t219 * t50 - t231 * t109
     # / 0.2D1 - t234 + t231 * t215 * t112 / 0.6D1 + t215 * t115) + t10 
     #* t128 - 0.180D3 * t54 * t120 * (t215 * t109 - t231 * t112 / 0.2D1
     # - t115 - t219 * t40 + t224 * t86 / 0.2D1 + t50)) * t133 / 0.1440D
     #4 + (0.90D2 * t63 * t39 * (-t260 * t40 + t262 * t86 / 0.2D1 + t50 
     #+ t268 * t109 - t270 * t112 / 0.2D1 - t115) - 0.180D3 * t54 * t120
     # * (t40 - t260 * t86 - t109 + t268 * t112) + t129) * t187 * t133 /
     # 0.1440D4 + (t44 * t120 * (t40 - t289 * t86) + 0.90D2 * t63 * t39 
     #* (t294 * t40 / 0.2D1 + t59 - t294 * t289 * t86 / 0.6D1 - t289 * t
     #50) + t155 - 0.180D3 * t54 * t120 * (-t289 * t40 + t294 * t86 / 0.
     #2D1 + t50)) * t187 / 0.1440D4
      t316 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t315)
      t318 = 0.1D1 - x1
      t319 = 0.1D1 - x3
      t320 = KAPPA2(t318, x2, t319, 0.10D1, z)
      t321 = s * t320
      t322 = -t318
      t323 = t1 * t322
      t324 = -t319
      t325 = t323 * t324
      t327 = t323 * x3
      t329 = t1 * x1
      t331 = t320 ** 2
      t334 = t322 * x1
      t338 = 0.1D1 / (-0.2D1 + t320)
      t339 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #10D1)
      t340 = t338 * t339
      t341 = t166 * t16
      t342 = t322 ** 2
      t343 = t18 * t342
      t344 = t324 * x4
      t345 = t331 ** 2
      t350 = log(-0.4D1 * t341 * t343 * t344 * t345)
      t352 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #10D1)
      t358 = t54 * t35
      t360 = t39 * t338 * t352
      t370 = log(-0.4D1 * t341 * t343 * t324 * t345)
      t371 = t370 * t338
      t373 = t370 ** 2
      t377 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #10D1)
      t388 = t44 * t35
      t394 = -(0.90D2 * t63 * t39 * (t340 - t350 * t338 * t352) - 0.180D
     #3 * t358 * t360) * t187 * t189 / 0.720D3 + (0.90D2 * t63 * t39 * (
     #t371 * t339 - t373 * t338 * t352 / 0.2D1 - t338 * t377) - 0.180D3 
     #* t54 * t120 * (-t340 + t371 * t352) - t388 * t360) * t187 * t131 
     #/ 0.720D3
      t395 = FJET(XB1, XB2, s, t321 * t325, -t321 * t327, t321 * t329, 0
     #.0D0, -s * t331 * t17 * t334 * x3, t394)
      t397 = KAPPA2(t318, x2, t319, t108, z)
      t398 = s * t397
      t401 = t329 * t103
      t403 = t329 * x4
      t405 = t397 ** 2
      t410 = cos(t11)
      t413 = Sqrt(x3 * t324 * t172)
      t420 = 0.1D1 / (-0.2D1 + t397)
      t421 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, t1
     #08)
      t424 = t405 ** 2
      t429 = log(0.4D1 * t192 * t342 * t324 * t172 * t424)
      t431 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, t1
     #08)
      t441 = 0.90D2 * t63 * t39 * (-t420 * t421 + t429 * t420 * t431) + 
     #0.180D3 * t358 * t39 * t420 * t431
      t445 = FJET(XB1, XB2, s, t398 * t325, -t398 * t327, -t398 * t401, 
     #t398 * t403, s * t405 * t17 * t334 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t410 * t413), -t441 * t187 * t189 / 0.720D3)
      t457 = log(0.4D1 * t19 * t90 * t342 * x4)
      t458 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.10D1)
      t460 = t457 ** 2
      t461 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.10D1)
      t464 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.10D1)
      t474 = t120 * t461
      t475 = t44 * t474
      t479 = t18 * t90
      t480 = t479 * t342
      t483 = log(0.4D1 * t16 * t480)
      t488 = t483 ** 2
      t491 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.10D1)
      t510 = t342 * x4
      t514 = log(0.4D1 * t265 * t479 * t510)
      t527 = log(0.4D1 * t265 * t480)
      t529 = t527 ** 2
      t545 = (0.90D2 * t63 * t39 * (t457 * t458 - t460 * t461 / 0.2D1 - 
     #t464) - 0.180D3 * t54 * t120 * (-t458 + t457 * t461) - t475) * t13
     #1 * t133 / 0.720D3 - (t44 * t120 * (t458 - t483 * t461) + 0.90D2 *
     # t63 * t39 * (t488 * t458 / 0.2D1 + t491 - t488 * t483 * t461 / 0.
     #6D1 - t483 * t464) + t10 * t474 - 0.180D3 * t54 * t120 * (-t483 * 
     #t458 + t488 * t461 / 0.2D1 + t464)) * t131 / 0.720D3 - (0.90D2 * t
     #63 * t39 * (t458 - t514 * t461) - 0.180D3 * t54 * t474) * t187 * t
     #189 / 0.720D3 + (0.90D2 * t63 * t39 * (t527 * t458 - t529 * t461 /
     # 0.2D1 - t464) - 0.180D3 * t54 * t120 * (-t458 + t527 * t461) - t4
     #75) * t187 * t131 / 0.720D3
      t546 = FJET(XB1, XB2, s, -t2 * t322, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t545)
      t551 = t102 * t103 * t324
      t554 = log(0.4D1 * t265 * t551)
      t555 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #t108)
      t557 = t554 ** 2
      t558 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #t108)
      t561 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #t108)
      t566 = log(-0.4D1 * t265 * t18 * t324 * x4)
      t567 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.10D1)
      t569 = t566 ** 2
      t570 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.10D1)
      t573 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.10D1)
      t584 = -t570 + t558
      t591 = t92 * t324
      t594 = log(-0.4D1 * t257 * t591)
      t599 = t594 ** 2
      t602 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.10D1)
      t611 = t120 * t570
      t625 = log(0.4D1 * t341 * t551)
      t630 = log(-0.4D1 * t167 * t92 * t344)
      t646 = log(-0.4D1 * t167 * t591)
      t648 = t646 ** 2
      t665 = (0.90D2 * t63 * t39 * (-t554 * t555 + t557 * t558 / 0.2D1 +
     # t561 + t566 * t567 - t569 * t570 / 0.2D1 - t573) - 0.180D3 * t54 
     #* t120 * (t555 - t554 * t558 - t567 + t566 * t570) + t44 * t120 * 
     #t584) * t187 * t133 / 0.1440D4 + (-t44 * t120 * (t567 - t594 * t57
     #0) - 0.90D2 * t63 * t39 * (t599 * t567 / 0.2D1 + t602 - t599 * t59
     #4 * t570 / 0.6D1 - t594 * t573) - t10 * t611 + 0.180D3 * t54 * t12
     #0 * (-t594 * t567 + t599 * t570 / 0.2D1 + t573)) * t187 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t555 + t625 * t558 + t567 - t630 * t570
     #) + 0.180D3 * t54 * t120 * t584) * t187 * t189 / 0.720D3 + (-0.90D
     #2 * t63 * t39 * (-t646 * t567 + t648 * t570 / 0.2D1 + t573) + 0.18
     #0D3 * t54 * t120 * (t567 - t646 * t570) - t44 * t611) * t187 * t13
     #1 / 0.720D3
      t666 = FJET(XB1, XB2, s, -t2 * t324, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t665)
      t668 = KAPPA2(t318, x2, 0.10D1, t108, z)
      t669 = s * t668
      t673 = t668 ** 2
      t678 = t673 ** 2
      t683 = log(-0.4D1 * t136 * t510 * t103 * t678)
      t685 = 0.1D1 / (-0.2D1 + t668)
      t686 = t683 * t685
      t687 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #t108)
      t689 = t683 ** 2
      t691 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #t108)
      t694 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #t108)
      t700 = t685 * t687
      t707 = t39 * t685 * t691
      t716 = log(-0.4D1 * t341 * t343 * t172 * t678)
      t729 = (-0.90D2 * t63 * t39 * (-t686 * t687 + t689 * t685 * t691 /
     # 0.2D1 + t685 * t694) + 0.180D3 * t54 * t120 * (t700 - t686 * t691
     #) - t388 * t707) * t131 * t133 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t700 - t716 * t685 * t691) - 0.180D3 * t358 * t707) * t187 * t189 
     #/ 0.720D3
      t730 = FJET(XB1, XB2, s, -t669 * t323, 0.0D0, -t669 * t401, t669 *
     # t403, -s * t673 * t17 * t334 * x4, t729)
      rrqg2qght5s1e1 = t316 * t315 + t395 * t394 - t445 * t441 * t187 * 
     #t131 * t133 / 0.720D3 + t546 * t545 + t666 * t665 + t730 * t729

      end function



      doubleprecision function rrqg2qght5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = 0.1D1 - x4
      t76 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t75)
      t77 = t68 * t13
      t78 = t16 * x4
      t79 = -t75
      t80 = t78 * t79
      t83 = log(-0.4D1 * t77 * t80)
      t84 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t75)
      t90 = t27 * t31
      t91 = -t84 + t55
      t92 = t90 * t91
      t94 = 0.180D3 * t59 * t92
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t103 = log(0.4D1 * t68 * t69)
      t105 = t103 ** 2
      t117 = t90 * t55
      t118 = t8 * t117
      t124 = log(-0.4D1 * t14 * t80)
      t126 = t124 ** 2
      t129 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t75)
      t132 = log(0.4D1 * t14 * t78)
      t134 = t132 ** 2
      t151 = x1 ** 2
      t152 = t151 * t11
      t155 = log(0.4D1 * t152 * t70)
      t160 = log(-0.4D1 * t152 * t13 * t80)
      t167 = 0.1D1 / x1
      t171 = t36 * t31
      t174 = t167 * t98
      t178 = x3 * t151
      t181 = log(0.4D1 * t178 * t17)
      t193 = t152 * t69
      t195 = log(0.4D1 * t193)
      t197 = t195 ** 2
      t212 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (t32 - t73 *
     # t55 - t76 + t83 * t84) - t94) * t96 * t98 / 0.1440D4 + (0.90D2 * 
     #t36 * t31 * (-t103 * t32 + t105 * t55 / 0.2D1 + t64) - 0.180D3 * t
     #59 * t90 * (t32 - t103 * t55) + t118) * t96 / 0.1440D4 + (0.90D2 *
     # t36 * t31 * (t124 * t76 - t126 * t84 / 0.2D1 - t129 - t132 * t32 
     #+ t134 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t90 * (-t76 + t124 *
     # t84 + t32 - t132 * t55) + t8 * t92) * t98 / 0.1440D4 + (0.90D2 * 
     #t36 * t31 * (t32 - t155 * t55 - t76 + t160 * t84) - t94) * t167 * 
     #t98 / 0.720D3 + t171 * t91 * t96 * t174 / 0.8D1 + (0.90D2 * t36 * 
     #t31 * (t32 - t181 * t55) - 0.180D3 * t59 * t117) * t96 * t167 / 0.
     #720D3 - (0.90D2 * t36 * t31 * (t195 * t32 - t197 * t55 / 0.2D1 - t
     #64) - 0.180D3 * t59 * t90 * (-t32 + t195 * t55) - t118) * t167 / 0
     #.720D3
      t213 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t212)
      t215 = 0.1D1 - x1
      t216 = 0.1D1 - x3
      t217 = KAPPA2(t215, x2, t216, 0.10D1, z)
      t218 = s * t217
      t219 = -t215
      t220 = t1 * t219
      t221 = -t216
      t222 = t220 * t221
      t224 = t220 * x3
      t226 = t1 * x1
      t228 = t217 ** 2
      t231 = t219 * x1
      t235 = 0.1D1 / (-0.2D1 + t217)
      t236 = t31 * t235
      t238 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t215, x2, t216, 0.
     #10D1)
      t243 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t215, x2, t216, 0.
     #10D1)
      t246 = t219 ** 2
      t248 = t228 ** 2
      t253 = log(-0.4D1 * t178 * t14 * t16 * t246 * t221 * t248)
      t260 = t59 * t27
      t268 = -t36 * t236 * t238 * t96 * t174 / 0.8D1 + (0.90D2 * t36 * t
     #31 * (-t235 * t243 + t253 * t235 * t238) + 0.180D3 * t260 * t236 *
     # t238) * t96 * t167 / 0.720D3
      t269 = FJET(XB1, XB2, s, t218 * t222, -t218 * t224, t218 * t226, 0
     #.0D0, -s * t228 * t15 * t231 * x3, t268)
      t271 = KAPPA2(t215, x2, t216, t75, z)
      t272 = s * t271
      t275 = t226 * t79
      t277 = t226 * x4
      t279 = t271 ** 2
      t284 = cos(t9)
      t288 = Sqrt(x3 * t221 * x4 * t79)
      t295 = 0.1D1 / (-0.2D1 + t271)
      t298 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t215, x2, t216, t7
     #5)
      t303 = FJET(XB1, XB2, s, t272 * t222, -t272 * t224, -t272 * t275, 
     #t272 * t277, s * t279 * t15 * t231 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t284 * t288), t36 * t31 * t295 * t298 * t96 * t174 / 0.8D
     #1)
      t314 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t215, x2, 0.10D1, 
     #0.10D1)
      t319 = log(0.4D1 * t17 * t151 * t246 * x4)
      t320 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t215, x2, 0.10D1, 
     #0.10D1)
      t326 = t90 * t320
      t328 = 0.180D3 * t59 * t326
      t338 = t16 * t151 * t246
      t341 = log(0.4D1 * t77 * t338)
      t353 = log(0.4D1 * t14 * t338)
      t355 = t353 ** 2
      t358 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t215, x2, 0.10D1, 
     #0.10D1)
      t372 = (0.90D2 * t36 * t31 * (-t314 + t319 * t320) + t328) * t167 
     #* t98 / 0.720D3 - t171 * t320 * t96 * t174 / 0.8D1 + (0.90D2 * t36
     # * t31 * (-t314 + t341 * t320) + t328) * t96 * t167 / 0.720D3 - (0
     #.90D2 * t36 * t31 * (-t353 * t314 + t355 * t320 / 0.2D1 + t358) - 
     #0.180D3 * t59 * t90 * (t314 - t353 * t320) + t8 * t326) * t167 / 0
     #.720D3
      t373 = FJET(XB1, XB2, s, -t2 * t219, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t372)
      t377 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t216, 
     #0.10D1)
      t378 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t216, 
     #t75)
      t379 = t377 - t378
      t384 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t216, 
     #0.10D1)
      t386 = t69 * t221
      t389 = log(-0.4D1 * t178 * t11 * t386)
      t395 = t90 * t377
      t402 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t216, 
     #t75)
      t407 = log(0.4D1 * t77 * t78 * t79 * t221)
      t413 = log(-0.4D1 * t77 * t16 * t221 * x4)
      t429 = log(-0.4D1 * t68 * t386)
      t431 = t429 ** 2
      t434 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t216, 
     #0.10D1)
      t448 = -t171 * t379 * t96 * t174 / 0.8D1 + (-0.90D2 * t36 * t31 * 
     #(t384 - t389 * t377) + 0.180D3 * t59 * t395) * t96 * t167 / 0.720D
     #3 + (0.90D2 * t36 * t31 * (t402 - t407 * t378 - t384 + t413 * t377
     #) + 0.180D3 * t59 * t90 * t379) * t96 * t98 / 0.1440D4 + (-0.90D2 
     #* t36 * t31 * (-t429 * t384 + t431 * t377 / 0.2D1 + t434) + 0.180D
     #3 * t59 * t90 * (t384 - t429 * t377) - t8 * t395) * t96 / 0.1440D4
      t449 = FJET(XB1, XB2, s, -t2 * t221, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t448)
      t451 = KAPPA2(t215, x2, 0.10D1, t75, z)
      t452 = s * t451
      t456 = t451 ** 2
      t462 = 0.1D1 / (-0.2D1 + t451)
      t463 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t215, x2, 0.10D1, 
     #t75)
      t466 = t456 ** 2
      t471 = log(-0.4D1 * t193 * t246 * x4 * t79 * t466)
      t473 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t215, x2, 0.10D1, 
     #t75)
      t479 = t31 * t462
      t492 = (-0.90D2 * t36 * t31 * (t462 * t463 - t471 * t462 * t473) +
     # 0.180D3 * t260 * t479 * t473) * t167 * t98 / 0.720D3 - t36 * t479
     # * t473 * t96 * t174 / 0.8D1
      t493 = FJET(XB1, XB2, s, -t452 * t220, 0.0D0, -t452 * t275, t452 *
     # t277, -s * t456 * t15 * t231 * x4, t492)
      rrqg2qght5s1e0 = t213 * t212 + t269 * t268 + t303 * 0.314159265358
     #9793D1 * t90 * t295 * t298 * t96 * t167 * t98 / 0.8D1 + t373 * t37
     #2 + t449 * t448 + t493 * t492

      end function



      doubleprecision function rrqg2qght5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t50 = t45 * t23
      t51 = 0.1D1 - x4
      t52 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t51)
      t53 = -t52 + t41
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t7
      t61 = t9 * t12
      t64 = log(0.4D1 * t60 * t61)
      t70 = t19 * t23
      t73 = 0.180D3 * t3 * t70 * t41
      t77 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t51)
      t78 = t12 * x4
      t79 = -t51
      t83 = log(-0.4D1 * t10 * t78 * t79)
      t87 = log(0.4D1 * t10 * t78)
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t108 = log(0.4D1 * t104 * t7 * t61)
      t121 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t53 * t54 * t56 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t64 * t41) - t73) * t54 / 
     #0.1440D4 + (0.90D2 * t45 * t23 * (-t77 + t83 * t52 + t24 - t87 * t
     #41) - 0.180D3 * t3 * t70 * t53) * t56 / 0.1440D4 + t50 * t41 * t54
     # * t100 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t108 * t41) + t73)
     # * t100 / 0.720D3 + t50 * t53 * t100 * t56 / 0.8D1
      t122 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t124 = 0.1D1 - x1
      t125 = 0.1D1 - x3
      t126 = KAPPA2(t124, x2, t125, 0.10D1, z)
      t127 = s * t126
      t128 = -t124
      t129 = t1 * t128
      t130 = -t125
      t135 = t1 * x1
      t137 = t126 ** 2
      t140 = t128 * x1
      t145 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, t125, 0.
     #10D1)
      t148 = 0.1D1 / (-0.2D1 + t126) * t145 * t54 * t100
      t151 = FJET(XB1, XB2, s, t127 * t129 * t130, -t127 * t129 * x3, t1
     #27 * t135, 0.0D0, -s * t137 * t11 * t140 * x3, -t50 * t148 / 0.8D1
     #)
      t158 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.10D1)
      t163 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.10D1)
      t165 = t128 ** 2
      t169 = log(0.4D1 * t10 * t12 * t104 * t165)
      t185 = -t50 * t158 * t54 * t100 / 0.8D1 - (0.90D2 * t45 * t23 * (t
     #163 - t169 * t158) - 0.180D3 * t3 * t70 * t158) * t100 / 0.720D3 -
     # t50 * t158 * t100 * t56 / 0.8D1
      t186 = FJET(XB1, XB2, s, -t2 * t128, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t185)
      t190 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.10D1)
      t195 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #t51)
      t201 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.10D1)
      t205 = log(-0.4D1 * t60 * t61 * t130)
      t217 = -t50 * t190 * t54 * t100 / 0.8D1 + t50 * (-t190 + t195) * t
     #54 * t56 / 0.16D2 + (-0.90D2 * t45 * t23 * (t201 - t205 * t190) + 
     #0.180D3 * t3 * t70 * t190) * t54 / 0.1440D4
      t218 = FJET(XB1, XB2, s, -t2 * t130, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t217)
      t220 = KAPPA2(t124, x2, 0.10D1, t51, z)
      t221 = s * t220
      t227 = t220 ** 2
      t234 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #t51)
      t237 = 0.1D1 / (-0.2D1 + t220) * t234 * t100 * t56
      t240 = FJET(XB1, XB2, s, -t221 * t129, 0.0D0, -t221 * t135 * t79, 
     #t221 * t135 * x4, -s * t227 * t11 * t140 * x4, -t50 * t237 / 0.8D1
     #)
      rrqg2qght5s1em1 = t122 * t121 - t151 * 0.3141592653589793D1 * t70 
     #* t148 / 0.8D1 + t186 * t185 + t218 * t217 - t240 * 0.314159265358
     #9793D1 * t70 * t237 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t9 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.1D1 - x4)
      t10 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t17 = t7 * t10
      t18 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t26 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t7 * (-t9 + t10) / x4 / 0.16D2 + t4 * t17 * t18 / 0.16D
     #2 + t4 * t17 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 
     #0.3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) *
     # t3 * t17 / 0.1440D4
      t50 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t52 = -0.1D1 + x3
      t56 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t52, 0
     #.10D1)
      t58 = t7 * t56 * t18
      t61 = FJET(XB1, XB2, s, -t2 * t52, t2 * x3, 0.0D0, 0.0D0, 0.0D0, -
     #t4 * t58 / 0.16D2)
      t66 = -0.1D1 + x1
      t70 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, -t66, x2, 0.10D1, 0
     #.10D1)
      t72 = t7 * t70 * t22
      t75 = FJET(XB1, XB2, s, -t2 * t66, 0.0D0, t2 * x1, 0.0D0, 0.0D0, -
     #t4 * t72 / 0.8D1)
      rrqg2qght5s1em2 = t50 * t49 - t61 * 0.3141592653589793D1 * t3 * t5
     #8 / 0.16D2 - t75 * 0.3141592653589793D1 * t3 * t72 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t12 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s1em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t101 = t91 * t15
      t102 = t18 * x4
      t103 = -0.1D1 + x4
      t104 = t102 * t103
      t107 = log(-0.4D1 * t101 * t104)
      t108 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t110 = t107 ** 2
      t111 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t114 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t119 = t35 * t39
      t126 = -t86 + t111
      t130 = 0.1D1 / x1
      t132 = 0.1D1 / x4
      t135 = t91 * t92
      t137 = log(0.4D1 * t135)
      t142 = t137 ** 2
      t153 = t119 * t86
      t154 = t10 * t153
      t165 = x3 * t90
      t166 = t165 * t13
      t167 = x4 * t103
      t171 = log(-0.4D1 * t166 * t92 * t167)
      t175 = log(0.4D1 * t166 * t93)
      t182 = -t119 * t126
      t186 = 0.1D1 / x3
      t188 = t130 * t132
      t191 = t165 * t19
      t193 = log(0.4D1 * t191)
      t195 = t193 ** 2
      t214 = log(0.4D1 * t16 * t102)
      t218 = log(-0.4D1 * t16 * t104)
      t223 = t218 ** 2
      t226 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t231 = t214 ** 2
      t256 = x3 * t13
      t259 = log(0.4D1 * t256 * t93)
      t261 = t259 ** 2
      t264 = t256 * t15
      t267 = log(-0.4D1 * t264 * t104)
      t269 = t267 ** 2
      t289 = log(0.4D1 * t256 * t92)
      t294 = t289 ** 2
      t315 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 - (0.90D2 * t63 * t39 * (t96 * t40 - t98 * t86 / 0.2D1 
     #- t50 - t107 * t108 + t110 * t111 / 0.2D1 + t114) - 0.180D3 * t54 
     #* t119 * (-t40 + t96 * t86 + t108 - t107 * t111) + t44 * t119 * t1
     #26) * t130 * t132 / 0.720D3 - (t44 * t119 * (-t40 + t137 * t86) + 
     #0.90D2 * t63 * t39 * (-t142 * t40 / 0.2D1 - t59 + t142 * t137 * t8
     #6 / 0.6D1 + t137 * t50) - t154 - 0.180D3 * t54 * t119 * (t137 * t4
     #0 - t142 * t86 / 0.2D1 - t50)) * t130 / 0.720D3 + (0.90D2 * t63 * 
     #t39 * (-t108 + t171 * t111 + t40 - t175 * t86) - 0.180D3 * t54 * t
     #182) * t186 * t188 / 0.720D3 - (0.90D2 * t63 * t39 * (t193 * t40 -
     # t195 * t86 / 0.2D1 - t50) - 0.180D3 * t54 * t119 * (-t40 + t193 *
     # t86) - t44 * t153) * t186 * t130 / 0.720D3 + (t44 * t119 * (t40 -
     # t214 * t86 - t108 + t218 * t111) + 0.90D2 * t63 * t39 * (-t223 * 
     #t108 / 0.2D1 - t226 + t223 * t218 * t111 / 0.6D1 + t218 * t114 + t
     #231 * t40 / 0.2D1 + t59 - t231 * t214 * t86 / 0.6D1 - t214 * t50) 
     #+ t10 * t182 - 0.180D3 * t54 * t119 * (-t214 * t40 + t231 * t86 / 
     #0.2D1 + t50 + t218 * t108 - t223 * t111 / 0.2D1 - t114)) * t132 / 
     #0.1440D4 + (0.90D2 * t63 * t39 * (-t259 * t40 + t261 * t86 / 0.2D1
     # + t50 + t267 * t108 - t269 * t111 / 0.2D1 - t114) - 0.180D3 * t54
     # * t119 * (t40 - t259 * t86 - t108 + t267 * t111) + t44 * t182) * 
     #t186 * t132 / 0.1440D4 + (t44 * t119 * (t40 - t289 * t86) + 0.90D2
     # * t63 * t39 * (t294 * t40 / 0.2D1 + t59 - t294 * t289 * t86 / 0.6
     #D1 - t289 * t50) + t154 - 0.180D3 * t54 * t119 * (-t289 * t40 + t2
     #94 * t86 / 0.2D1 + t50)) * t186 / 0.1440D4
      t316 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t315)
      t318 = 0.1D1 - x1
      t319 = 0.1D1 - x3
      t320 = KAPPA2(t318, x2, t319, 0.0D0, z)
      t321 = s * t320
      t322 = -t318
      t323 = t1 * t322
      t324 = -t319
      t325 = t323 * t324
      t327 = t323 * x3
      t329 = t1 * x1
      t331 = t320 ** 2
      t334 = t322 * x1
      t338 = 0.1D1 / (-0.2D1 + t320)
      t339 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #0D0)
      t340 = t338 * t339
      t341 = t165 * t16
      t342 = t322 ** 2
      t343 = t18 * t342
      t344 = t324 * x4
      t345 = t331 ** 2
      t350 = log(-0.4D1 * t341 * t343 * t344 * t345)
      t352 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #0D0)
      t358 = t54 * t35
      t360 = t39 * t338 * t352
      t370 = log(-0.4D1 * t341 * t343 * t324 * t345)
      t371 = t370 * t338
      t373 = t370 ** 2
      t377 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #0D0)
      t388 = t44 * t35
      t394 = (0.90D2 * t63 * t39 * (-t340 + t350 * t338 * t352) + 0.180D
     #3 * t358 * t360) * t186 * t188 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t371 * t339 + t373 * t338 * t352 / 0.2D1 + t338 * t377) - 0.180D3
     # * t54 * t119 * (t340 - t371 * t352) + t388 * t360) * t186 * t130 
     #/ 0.720D3
      t395 = FJET(XB1, XB2, s, t321 * t325, -t321 * t327, 0.0D0, t321 * 
     #t329, s * t331 * t17 * t334 * t324, t394)
      t397 = KAPPA2(t318, x2, t319, x4, z)
      t398 = s * t397
      t401 = t329 * x4
      t403 = t329 * t103
      t405 = t397 ** 2
      t410 = cos(t11)
      t413 = Sqrt(x3 * t324 * t167)
      t420 = 0.1D1 / (-0.2D1 + t397)
      t421 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, x4
     #)
      t424 = t405 ** 2
      t429 = log(0.4D1 * t191 * t342 * t324 * t167 * t424)
      t431 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, x4
     #)
      t441 = 0.90D2 * t63 * t39 * (t420 * t421 - t429 * t420 * t431) - 0
     #.180D3 * t358 * t39 * t420 * t431
      t445 = FJET(XB1, XB2, s, t398 * t325, -t398 * t327, t398 * t401, -
     #t398 * t403, s * t405 * t17 * t334 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t410 * t413), t441 * t186 * t188 / 0.720D3)
      t457 = log(-0.4D1 * t264 * t18 * t324 * x4)
      t458 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t460 = t457 ** 2
      t461 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t464 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t466 = t102 * t103 * t324
      t469 = log(0.4D1 * t264 * t466)
      t470 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #x4)
      t472 = t469 ** 2
      t473 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #x4)
      t476 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #x4)
      t488 = t119 * (-t461 + t473)
      t494 = t92 * t324
      t497 = log(-0.4D1 * t256 * t494)
      t502 = t497 ** 2
      t505 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t514 = t119 * t461
      t529 = log(-0.4D1 * t166 * t92 * t344)
      t533 = log(0.4D1 * t341 * t466)
      t547 = log(-0.4D1 * t166 * t494)
      t549 = t547 ** 2
      t566 = (0.90D2 * t63 * t39 * (t457 * t458 - t460 * t461 / 0.2D1 - 
     #t464 - t469 * t470 + t472 * t473 / 0.2D1 + t476) - 0.180D3 * t54 *
     # t119 * (-t458 + t457 * t461 + t470 - t469 * t473) + t44 * t488) *
     # t186 * t132 / 0.1440D4 + (-t44 * t119 * (t458 - t497 * t461) - 0.
     #90D2 * t63 * t39 * (t502 * t458 / 0.2D1 + t505 - t502 * t497 * t46
     #1 / 0.6D1 - t497 * t464) - t10 * t514 + 0.180D3 * t54 * t119 * (-t
     #497 * t458 + t502 * t461 / 0.2D1 + t464)) * t186 / 0.1440D4 + (0.9
     #0D2 * t63 * t39 * (-t458 + t529 * t461 + t470 - t533 * t473) - 0.1
     #80D3 * t54 * t488) * t186 * t188 / 0.720D3 - (0.90D2 * t63 * t39 *
     # (-t547 * t458 + t549 * t461 / 0.2D1 + t464) - 0.180D3 * t54 * t11
     #9 * (t458 - t547 * t461) + t44 * t514) * t186 * t130 / 0.720D3
      t567 = FJET(XB1, XB2, s, -t2 * t324, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t566)
      t569 = KAPPA2(t318, x2, 0.10D1, 0.0D0, z)
      t570 = s * t569
      t573 = t569 ** 2
      t578 = t573 ** 2
      t580 = t343 * x4 * t578
      t583 = log(0.4D1 * t101 * t580)
      t585 = 0.1D1 / (-0.2D1 + t569)
      t586 = t583 * t585
      t587 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t589 = t583 ** 2
      t591 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t594 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t595 = t585 * t594
      t600 = t585 * t587
      t607 = t39 * t585 * t591
      t608 = t388 * t607
      t615 = log(0.4D1 * t101 * t343 * t578)
      t616 = t615 * t585
      t621 = t615 ** 2
      t622 = t621 * t585
      t625 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t649 = log(0.4D1 * t341 * t580)
      t665 = log(0.4D1 * t166 * t92 * t342 * t578)
      t666 = t665 * t585
      t668 = t665 ** 2
      t685 = -(0.90D2 * t63 * t39 * (t586 * t587 - t589 * t585 * t591 / 
     #0.2D1 - t595) - 0.180D3 * t54 * t119 * (-t600 + t586 * t591) - t60
     #8) * t130 * t132 / 0.720D3 - (-t44 * t119 * (t600 - t616 * t591) -
     # 0.90D2 * t63 * t39 * (t622 * t587 / 0.2D1 + t585 * t625 - t621 * 
     #t615 * t585 * t591 / 0.6D1 - t616 * t594) - t10 * t35 * t607 + 0.1
     #80D3 * t54 * t119 * (-t616 * t587 + t622 * t591 / 0.2D1 + t595)) *
     # t130 / 0.720D3 + (0.90D2 * t63 * t39 * (t600 - t649 * t585 * t591
     #) - 0.180D3 * t358 * t607) * t186 * t188 / 0.720D3 - (-0.90D2 * t6
     #3 * t39 * (-t666 * t587 + t668 * t585 * t591 / 0.2D1 + t595) + 0.1
     #80D3 * t54 * t119 * (t600 - t666 * t591) - t608) * t186 * t130 / 0
     #.720D3
      t686 = FJET(XB1, XB2, s, -t570 * t323, 0.0D0, 0.0D0, t570 * t329, 
     #-s * t573 * t17 * t322 * x1, t685)
      t688 = KAPPA2(t318, x2, 0.10D1, x4, z)
      t689 = s * t688
      t693 = t688 ** 2
      t699 = t693 ** 2
      t704 = log(-0.4D1 * t135 * t342 * x4 * t103 * t699)
      t706 = 0.1D1 / (-0.2D1 + t688)
      t707 = t704 * t706
      t708 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #x4)
      t710 = t704 ** 2
      t712 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #x4)
      t715 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #x4)
      t721 = t706 * t708
      t728 = t39 * t706 * t712
      t737 = log(-0.4D1 * t341 * t343 * t167 * t699)
      t750 = -(0.90D2 * t63 * t39 * (-t707 * t708 + t710 * t706 * t712 /
     # 0.2D1 + t706 * t715) - 0.180D3 * t54 * t119 * (t721 - t707 * t712
     #) + t388 * t728) * t130 * t132 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(t721 - t737 * t706 * t712) + 0.180D3 * t358 * t728) * t186 * t188
     # / 0.720D3
      t751 = FJET(XB1, XB2, s, -t689 * t323, 0.0D0, t689 * t401, -t689 *
     # t403, s * t693 * t17 * t334 * t103, t750)
      rrqg2qght5s2e1 = t316 * t315 + t395 * t394 + t445 * t441 * t186 * 
     #t130 * t132 / 0.720D3 + t566 * t567 + t686 * t685 + t751 * t750

      end function



      doubleprecision function rrqg2qght5s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t76 = t68 * t13
      t77 = t16 * x4
      t78 = -0.1D1 + x4
      t79 = t77 * t78
      t82 = log(-0.4D1 * t76 * t79)
      t83 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t89 = t27 * t31
      t90 = t55 - t83
      t91 = t89 * t90
      t95 = 0.1D1 / x3
      t97 = 0.1D1 / x4
      t102 = log(0.4D1 * t68 * t69)
      t104 = t102 ** 2
      t116 = t89 * t55
      t117 = t8 * t116
      t123 = log(0.4D1 * t14 * t77)
      t125 = t123 ** 2
      t130 = log(-0.4D1 * t14 * t79)
      t132 = t130 ** 2
      t135 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t150 = x1 ** 2
      t151 = t150 * t11
      t154 = log(0.4D1 * t151 * t70)
      t156 = t151 * t13
      t159 = log(-0.4D1 * t156 * t79)
      t170 = 0.1D1 / x1
      t174 = t36 * t31
      t176 = t170 * t97
      t180 = x3 * t150
      t183 = log(0.4D1 * t180 * t17)
      t195 = t151 * t69
      t197 = log(0.4D1 * t195)
      t199 = t197 ** 2
      t214 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (t32 - t73 *
     # t55 - t75 + t82 * t83) - 0.180D3 * t59 * t91) * t95 * t97 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (-t102 * t32 + t104 * t55 / 0.2D1 + t6
     #4) - 0.180D3 * t59 * t89 * (t32 - t102 * t55) + t117) * t95 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (-t123 * t32 + t125 * t55 / 0.2D1 + t
     #64 + t130 * t75 - t132 * t83 / 0.2D1 - t135) - 0.180D3 * t59 * t89
     # * (t32 - t123 * t55 - t75 + t130 * t83) + t8 * t91) * t97 / 0.144
     #0D4 - (0.90D2 * t36 * t31 * (-t32 + t154 * t55 + t75 - t159 * t83)
     # + 0.180D3 * t59 * t89 * t90) * t170 * t97 / 0.720D3 + t174 * t90 
     #* t95 * t176 / 0.8D1 - (0.90D2 * t36 * t31 * (-t32 + t183 * t55) +
     # 0.180D3 * t59 * t116) * t95 * t170 / 0.720D3 - (0.90D2 * t36 * t3
     #1 * (t197 * t32 - t199 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t89 
     #* (-t32 + t197 * t55) - t117) * t170 / 0.720D3
      t215 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t214)
      t217 = 0.1D1 - x1
      t218 = 0.1D1 - x3
      t219 = KAPPA2(t217, x2, t218, 0.0D0, z)
      t220 = s * t219
      t221 = -t217
      t222 = t1 * t221
      t223 = -t218
      t224 = t222 * t223
      t226 = t222 * x3
      t228 = t1 * x1
      t230 = t219 ** 2
      t233 = t221 * x1
      t237 = 0.1D1 / (-0.2D1 + t219)
      t238 = t31 * t237
      t240 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, t218, 0.
     #0D0)
      t245 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t217, x2, t218, 0.
     #0D0)
      t248 = t221 ** 2
      t249 = t16 * t248
      t250 = t230 ** 2
      t255 = log(-0.4D1 * t180 * t14 * t249 * t223 * t250)
      t262 = t59 * t27
      t270 = -t36 * t238 * t240 * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t237 * t245 - t255 * t237 * t240) - 0.180D3 * t262 * t238 * 
     #t240) * t95 * t170 / 0.720D3
      t271 = FJET(XB1, XB2, s, t220 * t224, -t220 * t226, 0.0D0, t220 * 
     #t228, s * t230 * t15 * t233 * t223, t270)
      t273 = KAPPA2(t217, x2, t218, x4, z)
      t274 = s * t273
      t277 = t228 * x4
      t279 = t228 * t78
      t281 = t273 ** 2
      t286 = cos(t9)
      t290 = Sqrt(x3 * t223 * x4 * t78)
      t297 = 0.1D1 / (-0.2D1 + t273)
      t300 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, t218, x4
     #)
      t305 = FJET(XB1, XB2, s, t274 * t224, -t274 * t226, t274 * t277, -
     #t274 * t279, s * t281 * t15 * t233 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t286 * t290), t36 * t31 * t297 * t300 * t95 * t17
     #6 / 0.8D1)
      t316 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #0.0D0)
      t317 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #x4)
      t318 = -t316 + t317
      t323 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #0.0D0)
      t324 = t180 * t11
      t325 = t69 * t223
      t328 = log(-0.4D1 * t324 * t325)
      t334 = t89 * t316
      t345 = log(-0.4D1 * t76 * t16 * t223 * x4)
      t347 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #x4)
      t352 = log(0.4D1 * t76 * t77 * t78 * t223)
      t367 = log(-0.4D1 * t68 * t325)
      t369 = t367 ** 2
      t372 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #0.0D0)
      t386 = t174 * t318 * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t31 * (t
     #323 - t328 * t316) - 0.180D3 * t59 * t334) * t95 * t170 / 0.720D3 
     #+ (0.90D2 * t36 * t31 * (-t323 + t345 * t316 + t347 - t352 * t317)
     # - 0.180D3 * t59 * t89 * t318) * t95 * t97 / 0.1440D4 + (-0.90D2 *
     # t36 * t31 * (-t367 * t323 + t369 * t316 / 0.2D1 + t372) + 0.180D3
     # * t59 * t89 * (t323 - t367 * t316) - t8 * t334) * t95 / 0.1440D4
      t387 = FJET(XB1, XB2, s, -t2 * t223, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t386)
      t389 = KAPPA2(t217, x2, 0.10D1, 0.0D0, z)
      t390 = s * t389
      t393 = t389 ** 2
      t399 = 0.1D1 / (-0.2D1 + t389)
      t400 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #0.0D0)
      t401 = t399 * t400
      t402 = t393 ** 2
      t407 = log(0.4D1 * t156 * t249 * x4 * t402)
      t409 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #0.0D0)
      t415 = t31 * t399
      t416 = t415 * t409
      t418 = 0.180D3 * t262 * t416
      t432 = log(0.4D1 * t324 * t69 * t248 * t402)
      t446 = log(0.4D1 * t156 * t249 * t402)
      t447 = t446 * t399
      t449 = t446 ** 2
      t453 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #0.0D0)
      t469 = -(0.90D2 * t36 * t31 * (-t401 + t407 * t399 * t409) + t418)
     # * t170 * t97 / 0.720D3 + t36 * t415 * t409 * t95 * t176 / 0.8D1 -
     # (-0.90D2 * t36 * t31 * (t401 - t432 * t399 * t409) + t418) * t95 
     #* t170 / 0.720D3 - (-0.90D2 * t36 * t31 * (-t447 * t400 + t449 * t
     #399 * t409 / 0.2D1 + t399 * t453) + 0.180D3 * t59 * t89 * (t401 - 
     #t447 * t409) - t8 * t27 * t416) * t170 / 0.720D3
      t470 = FJET(XB1, XB2, s, -t390 * t222, 0.0D0, 0.0D0, t390 * t228, 
     #-s * t393 * t15 * t221 * x1, t469)
      t472 = KAPPA2(t217, x2, 0.10D1, x4, z)
      t473 = s * t472
      t477 = t472 ** 2
      t483 = 0.1D1 / (-0.2D1 + t472)
      t484 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #x4)
      t487 = t477 ** 2
      t492 = log(-0.4D1 * t195 * t248 * x4 * t78 * t487)
      t494 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #x4)
      t500 = t31 * t483
      t513 = -(0.90D2 * t36 * t31 * (t483 * t484 - t492 * t483 * t494) -
     # 0.180D3 * t262 * t500 * t494) * t170 * t97 / 0.720D3 - t36 * t500
     # * t494 * t95 * t176 / 0.8D1
      t514 = FJET(XB1, XB2, s, -t473 * t222, 0.0D0, t473 * t277, -t473 *
     # t279, s * t477 * t15 * t233 * t78, t513)
      rrqg2qght5s2e0 = t215 * t214 + t271 * t270 + t305 * 0.314159265358
     #9793D1 * t89 * t297 * t300 * t95 * t170 * t97 / 0.8D1 + t387 * t38
     #6 + t469 * t470 + t514 * t513

      end function



      doubleprecision function rrqg2qght5s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t50 = t45 * t23
      t51 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t52 = t41 - t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t7
      t60 = t9 * t12
      t63 = log(0.4D1 * t59 * t60)
      t69 = t19 * t23
      t72 = 0.180D3 * t3 * t69 * t41
      t76 = t12 * x4
      t79 = log(0.4D1 * t10 * t76)
      t81 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t82 = -0.1D1 + x4
      t86 = log(-0.4D1 * t10 * t76 * t82)
      t99 = 0.1D1 / x1
      t103 = x1 ** 2
      t104 = t103 * t7
      t107 = log(0.4D1 * t104 * t60)
      t121 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t52 * t53 * t55 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t63 * t41) - t72) * t53 / 
     #0.1440D4 + (0.90D2 * t45 * t23 * (t24 - t79 * t41 - t81 + t86 * t5
     #1) - 0.180D3 * t3 * t69 * t52) * t55 / 0.1440D4 + t50 * t41 * t53 
     #* t99 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t107 * t41) + t72) *
     # t99 / 0.720D3 + t50 * t52 * t99 * t55 / 0.8D1
      t122 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t124 = 0.1D1 - x1
      t125 = 0.1D1 - x3
      t126 = KAPPA2(t124, x2, t125, 0.0D0, z)
      t127 = s * t126
      t128 = -t124
      t129 = t1 * t128
      t130 = -t125
      t135 = t1 * x1
      t137 = t126 ** 2
      t140 = t128 * x1
      t145 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, t125, 0.
     #0D0)
      t147 = t53 * t99
      t148 = 0.1D1 / (-0.2D1 + t126) * t145 * t147
      t151 = FJET(XB1, XB2, s, t127 * t129 * t130, -t127 * t129 * x3, 0.
     #0D0, t127 * t135, s * t137 * t11 * t140 * t130, -t50 * t148 / 0.8D
     #1)
      t158 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.0D0)
      t163 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #x4)
      t169 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.0D0)
      t173 = log(-0.4D1 * t59 * t60 * t130)
      t185 = -t50 * t158 * t53 * t99 / 0.8D1 + t50 * (-t158 + t163) * t5
     #3 * t55 / 0.16D2 + (-0.90D2 * t45 * t23 * (t169 - t173 * t158) + 0
     #.180D3 * t3 * t69 * t158) * t53 / 0.1440D4
      t186 = FJET(XB1, XB2, s, -t2 * t130, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t185)
      t188 = KAPPA2(t124, x2, 0.10D1, 0.0D0, z)
      t189 = s * t188
      t192 = t188 ** 2
      t198 = 0.1D1 / (-0.2D1 + t188)
      t199 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.0D0)
      t200 = t198 * t199
      t204 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.0D0)
      t207 = t128 ** 2
      t209 = t192 ** 2
      t213 = log(0.4D1 * t104 * t9 * t12 * t207 * t209)
      t228 = t99 * t55
      t232 = t50 * t200 * t147 / 0.8D1 - (-0.90D2 * t45 * t23 * (t198 * 
     #t204 - t213 * t198 * t199) + 0.180D3 * t3 * t19 * t23 * t198 * t19
     #9) * t99 / 0.720D3 + t50 * t200 * t228 / 0.8D1
      t233 = FJET(XB1, XB2, s, -t189 * t129, 0.0D0, 0.0D0, t189 * t135, 
     #-s * t192 * t11 * t128 * x1, t232)
      t235 = KAPPA2(t124, x2, 0.10D1, x4, z)
      t236 = s * t235
      t242 = t235 ** 2
      t249 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #x4)
      t251 = 0.1D1 / (-0.2D1 + t235) * t249 * t228
      t254 = FJET(XB1, XB2, s, -t236 * t129, 0.0D0, t236 * t135 * x4, -t
     #236 * t135 * t82, s * t242 * t11 * t140 * t82, -t50 * t251 / 0.8D1
     #)
      rrqg2qght5s2em1 = t122 * t121 - t151 * 0.3141592653589793D1 * t69 
     #* t148 / 0.8D1 + t186 * t185 + t233 * t232 - t254 * 0.314159265358
     #9793D1 * t69 * t251 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t9 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #x4)
      t16 = t7 * t8
      t17 = 0.1D1 / x3
      t21 = 0.1D1 / x1
      t25 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = t4 * t7 * (t8 - t9) / x4 / 0.16D2 + t4 * t16 * t17 / 0.16D2 
     #+ t4 * t16 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) * t
     #3 * t16 / 0.1440D4
      t49 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t51 = -0.1D1 + x3
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t51, 0
     #.0D0)
      t57 = t7 * t55 * t17
      t60 = FJET(XB1, XB2, s, -t2 * t51, t2 * x3, 0.0D0, 0.0D0, 0.0D0, -
     #t4 * t57 / 0.16D2)
      t65 = 0.1D1 - x1
      t66 = KAPPA2(t65, x2, 0.10D1, 0.0D0, z)
      t67 = s * t66
      t68 = -t65
      t73 = t66 ** 2
      t80 = 0.1D1 / (-0.2D1 + t66)
      t81 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t65, x2, 0.10D1, 0.
     #0D0)
      t86 = FJET(XB1, XB2, s, -t67 * t1 * t68, 0.0D0, 0.0D0, t67 * t1 * 
     #x1, -s * t73 * t37 * t68 * x1, t4 * t7 * t80 * t81 * t21 / 0.8D1)
      rrqg2qght5s2em2 = t49 * t48 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t86 * 0.3141592653589793D1 * t3 * t7 * t80 * t81 * t2
     #1 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t12 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s2em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s2em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t91 * t15
      t93 = t18 * x4
      t94 = -0.1D1 + x4
      t95 = t93 * t94
      t98 = log(-0.4D1 * t92 * t95)
      t99 = -t94
      t100 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t102 = t98 ** 2
      t103 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t106 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t107 = t15 * t18
      t108 = t107 * x4
      t111 = log(0.4D1 * t91 * t108)
      t113 = t111 ** 2
      t120 = t35 * t39
      t127 = t86 - t103
      t128 = t120 * t127
      t131 = 0.1D1 / x1
      t133 = 0.1D1 / x4
      t136 = t91 * t107
      t138 = log(0.4D1 * t136)
      t143 = t138 ** 2
      t154 = t120 * t86
      t155 = t10 * t154
      t166 = x3 * t90
      t167 = t166 * t13
      t168 = x4 * t94
      t172 = log(-0.4D1 * t167 * t107 * t168)
      t176 = log(0.4D1 * t167 * t108)
      t185 = 0.1D1 / x3
      t187 = t131 * t133
      t190 = t166 * t19
      t192 = log(0.4D1 * t190)
      t194 = t192 ** 2
      t213 = log(-0.4D1 * t16 * t95)
      t217 = log(0.4D1 * t16 * t93)
      t222 = t217 ** 2
      t229 = t213 ** 2
      t232 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t242 = -t120 * t127
      t257 = x3 * t13
      t260 = log(0.4D1 * t257 * t108)
      t262 = t260 ** 2
      t265 = t257 * t15
      t268 = log(-0.4D1 * t265 * t95)
      t270 = t268 ** 2
      t290 = log(0.4D1 * t257 * t107)
      t295 = t290 ** 2
      t316 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (t98 * t100 - t102 * t103 / 0.2
     #D1 - t106 - t111 * t40 + t113 * t86 / 0.2D1 + t50) - 0.180D3 * t54
     # * t120 * (-t100 + t98 * t103 + t40 - t111 * t86) + t44 * t128) * 
     #t131 * t133 / 0.720D3 - (t44 * t120 * (-t40 + t138 * t86) + 0.90D2
     # * t63 * t39 * (-t143 * t40 / 0.2D1 - t59 + t143 * t138 * t86 / 0.
     #6D1 + t138 * t50) - t155 - 0.180D3 * t54 * t120 * (t138 * t40 - t1
     #43 * t86 / 0.2D1 - t50)) * t131 / 0.720D3 + (0.90D2 * t63 * t39 * 
     #(-t100 + t172 * t103 + t40 - t176 * t86) - 0.180D3 * t54 * t128) *
     # t185 * t187 / 0.720D3 - (0.90D2 * t63 * t39 * (t192 * t40 - t194 
     #* t86 / 0.2D1 - t50) - 0.180D3 * t54 * t120 * (-t40 + t192 * t86) 
     #- t44 * t154) * t185 * t131 / 0.720D3 - (t44 * t120 * (t100 - t213
     # * t103 - t40 + t217 * t86) + 0.90D2 * t63 * t39 * (-t222 * t40 / 
     #0.2D1 - t59 + t222 * t217 * t86 / 0.6D1 + t217 * t50 + t229 * t100
     # / 0.2D1 + t232 - t229 * t213 * t103 / 0.6D1 - t213 * t106) + t10 
     #* t242 - 0.180D3 * t54 * t120 * (-t213 * t100 + t229 * t103 / 0.2D
     #1 + t106 + t217 * t40 - t222 * t86 / 0.2D1 - t50)) * t133 / 0.1440
     #D4 - (0.90D2 * t63 * t39 * (t260 * t40 - t262 * t86 / 0.2D1 - t50 
     #- t268 * t100 + t270 * t103 / 0.2D1 + t106) - 0.180D3 * t54 * t120
     # * (-t40 + t260 * t86 + t100 - t268 * t103) + t44 * t242) * t185 *
     # t133 / 0.1440D4 + (t44 * t120 * (t40 - t290 * t86) + 0.90D2 * t63
     # * t39 * (t295 * t40 / 0.2D1 + t59 - t295 * t290 * t86 / 0.6D1 - t
     #290 * t50) + t155 - 0.180D3 * t54 * t120 * (-t290 * t40 + t295 * t
     #86 / 0.2D1 + t50)) * t185 / 0.1440D4
      t317 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t316)
      t319 = 0.1D1 - x1
      t320 = KAPPA2(t319, x2, 0.0D0, 0.10D1, z)
      t321 = s * t320
      t322 = -t319
      t323 = t1 * t322
      t325 = t1 * x1
      t327 = t320 ** 2
      t332 = t322 ** 2
      t333 = t18 * t332
      t334 = t327 ** 2
      t336 = t333 * x4 * t334
      t339 = log(0.4D1 * t92 * t336)
      t341 = 0.1D1 / (-0.2D1 + t320)
      t342 = t339 * t341
      t343 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t345 = t339 ** 2
      t347 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t350 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t351 = t341 * t350
      t356 = t341 * t343
      t362 = t44 * t35
      t364 = t39 * t341 * t347
      t365 = t362 * t364
      t372 = log(0.4D1 * t92 * t333 * t334)
      t373 = t372 * t341
      t378 = t372 ** 2
      t379 = t378 * t341
      t382 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t404 = t166 * t16
      t407 = log(0.4D1 * t404 * t336)
      t414 = t54 * t35
      t424 = log(0.4D1 * t167 * t107 * t332 * t334)
      t425 = t424 * t341
      t427 = t424 ** 2
      t444 = (0.90D2 * t63 * t39 * (-t342 * t343 + t345 * t341 * t347 / 
     #0.2D1 + t351) - 0.180D3 * t54 * t120 * (t356 - t342 * t347) + t365
     #) * t131 * t133 / 0.720D3 - (-t44 * t120 * (t356 - t373 * t347) - 
     #0.90D2 * t63 * t39 * (t379 * t343 / 0.2D1 + t341 * t382 - t378 * t
     #372 * t341 * t347 / 0.6D1 - t373 * t350) - t10 * t35 * t364 + 0.18
     #0D3 * t54 * t120 * (-t373 * t343 + t379 * t347 / 0.2D1 + t351)) * 
     #t131 / 0.720D3 + (0.90D2 * t63 * t39 * (t356 - t407 * t341 * t347)
     # - 0.180D3 * t414 * t364) * t185 * t187 / 0.720D3 - (0.90D2 * t63 
     #* t39 * (t425 * t343 - t427 * t341 * t347 / 0.2D1 - t351) - 0.180D
     #3 * t54 * t120 * (-t356 + t425 * t347) - t365) * t185 * t131 / 0.7
     #20D3
      t445 = FJET(XB1, XB2, s, 0.0D0, -t321 * t323, t321 * t325, 0.0D0, 
     #-s * t327 * t17 * t322 * x1, t444)
      t447 = KAPPA2(t319, x2, 0.0D0, t99, z)
      t448 = s * t447
      t450 = t325 * t94
      t452 = t325 * x4
      t454 = t447 ** 2
      t457 = t322 * x1
      t461 = t454 ** 2
      t466 = log(-0.4D1 * t136 * t332 * x4 * t94 * t461)
      t468 = 0.1D1 / (-0.2D1 + t447)
      t469 = t466 * t468
      t470 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, t
     #99)
      t472 = t466 ** 2
      t474 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, t
     #99)
      t477 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, t
     #99)
      t483 = t468 * t470
      t490 = t39 * t468 * t474
      t499 = log(-0.4D1 * t404 * t333 * t168 * t461)
      t512 = (-0.90D2 * t63 * t39 * (-t469 * t470 + t472 * t468 * t474 /
     # 0.2D1 + t468 * t477) + 0.180D3 * t54 * t120 * (t483 - t469 * t474
     #) - t362 * t490) * t131 * t133 / 0.720D3 + (0.90D2 * t63 * t39 * (
     #-t483 + t499 * t468 * t474) + 0.180D3 * t414 * t490) * t185 * t187
     # / 0.720D3
      t513 = FJET(XB1, XB2, s, 0.0D0, -t448 * t323, -t448 * t450, t448 *
     # t452, s * t454 * t17 * t457 * t94, t512)
      t516 = -0.1D1 + x3
      t522 = log(-0.4D1 * t265 * t18 * t516 * x4)
      t523 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t525 = t522 ** 2
      t526 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t529 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t531 = t93 * t94 * t516
      t534 = log(0.4D1 * t265 * t531)
      t535 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t9
     #9)
      t537 = t534 ** 2
      t538 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t9
     #9)
      t541 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t9
     #9)
      t552 = t526 - t538
      t559 = t107 * t516
      t562 = log(-0.4D1 * t257 * t559)
      t567 = t562 ** 2
      t570 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t579 = t120 * t526
      t591 = t516 * x4
      t595 = log(-0.4D1 * t167 * t107 * t591)
      t599 = log(0.4D1 * t404 * t531)
      t615 = log(-0.4D1 * t167 * t559)
      t617 = t615 ** 2
      t634 = -(0.90D2 * t63 * t39 * (-t522 * t523 + t525 * t526 / 0.2D1 
     #+ t529 + t534 * t535 - t537 * t538 / 0.2D1 - t541) - 0.180D3 * t54
     # * t120 * (t523 - t522 * t526 - t535 + t534 * t538) + t44 * t120 *
     # t552) * t185 * t133 / 0.1440D4 + (-t44 * t120 * (t523 - t562 * t5
     #26) - 0.90D2 * t63 * t39 * (t567 * t523 / 0.2D1 + t570 - t567 * t5
     #62 * t526 / 0.6D1 - t562 * t529) - t10 * t579 + 0.180D3 * t54 * t1
     #20 * (-t562 * t523 + t567 * t526 / 0.2D1 + t529)) * t185 / 0.1440D
     #4 + (0.90D2 * t63 * t39 * (-t523 + t595 * t526 + t535 - t599 * t53
     #8) + 0.180D3 * t54 * t120 * t552) * t185 * t187 / 0.720D3 - (0.90D
     #2 * t63 * t39 * (-t615 * t523 + t617 * t526 / 0.2D1 + t529) - 0.18
     #0D3 * t54 * t120 * (t523 - t615 * t526) + t44 * t579) * t185 * t13
     #1 / 0.720D3
      t635 = FJET(XB1, XB2, s, t2 * x3, -t2 * t516, 0.0D0, 0.0D0, 0.0D0,
     # t634)
      t637 = KAPPA2(t319, x2, x3, 0.10D1, z)
      t638 = s * t637
      t639 = t323 * x3
      t641 = t323 * t516
      t644 = t637 ** 2
      t650 = 0.1D1 / (-0.2D1 + t637)
      t651 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, 0.10
     #D1)
      t652 = t650 * t651
      t653 = t644 ** 2
      t658 = log(-0.4D1 * t404 * t333 * t591 * t653)
      t660 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, 0.10
     #D1)
      t667 = t39 * t650 * t660
      t677 = log(-0.4D1 * t404 * t333 * t516 * t653)
      t678 = t677 * t650
      t680 = t677 ** 2
      t684 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, 0.10
     #D1)
      t700 = (0.90D2 * t63 * t39 * (-t652 + t658 * t650 * t660) + 0.180D
     #3 * t414 * t667) * t185 * t187 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t678 * t651 + t680 * t650 * t660 / 0.2D1 + t650 * t684) - 0.180D3
     # * t54 * t120 * (t652 - t678 * t660) + t362 * t667) * t185 * t131 
     #/ 0.720D3
      t701 = FJET(XB1, XB2, s, -t638 * t639, t638 * t641, t638 * t325, 0
     #.0D0, s * t644 * t17 * t457 * t516, t700)
      t703 = KAPPA2(t319, x2, x3, t99, z)
      t704 = s * t703
      t709 = t703 ** 2
      t714 = cos(t11)
      t717 = Sqrt(x3 * t516 * t168)
      t724 = 0.1D1 / (-0.2D1 + t703)
      t725 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, t99)
      t728 = t709 ** 2
      t733 = log(0.4D1 * t190 * t332 * t516 * t168 * t728)
      t735 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, t99)
      t745 = 0.90D2 * t63 * t39 * (t724 * t725 - t733 * t724 * t735) - 0
     #.180D3 * t414 * t39 * t724 * t735
      t749 = FJET(XB1, XB2, s, -t704 * t639, t704 * t641, -t704 * t450, 
     #t704 * t452, s * t709 * t17 * t457 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t714 * t717), t745 * t185 * t187 / 0.720D3)
      rrqg2qght5s3e1 = t317 * t316 + t445 * t444 + t513 * t512 + t635 * 
     #t634 + t701 * t700 + t749 * t745 * t185 * t131 * t133 / 0.720D3

      end function



      doubleprecision function rrqg2qght5s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = 0.1D1 - x4
      t76 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t75)
      t77 = t68 * t13
      t78 = t16 * x4
      t79 = -t75
      t80 = t78 * t79
      t83 = log(-0.4D1 * t77 * t80)
      t84 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t75)
      t90 = t27 * t31
      t91 = t84 - t55
      t92 = t90 * t91
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t103 = log(0.4D1 * t68 * t69)
      t105 = t103 ** 2
      t117 = t90 * t55
      t118 = t8 * t117
      t124 = log(-0.4D1 * t14 * t80)
      t126 = t124 ** 2
      t129 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t75)
      t132 = log(0.4D1 * t14 * t78)
      t134 = t132 ** 2
      t151 = x1 ** 2
      t152 = t151 * t11
      t153 = t152 * t13
      t156 = log(-0.4D1 * t153 * t80)
      t160 = log(0.4D1 * t152 * t70)
      t166 = -t91
      t171 = 0.1D1 / x1
      t175 = t36 * t31
      t177 = t171 * t98
      t181 = x3 * t151
      t184 = log(0.4D1 * t181 * t17)
      t196 = t152 * t69
      t198 = log(0.4D1 * t196)
      t200 = t198 ** 2
      t215 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 - (0.90D2 * t36 * t31 * (-t32 + t73 
     #* t55 + t76 - t83 * t84) - 0.180D3 * t59 * t92) * t96 * t98 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (-t103 * t32 + t105 * t55 / 0.2D1 + t
     #64) - 0.180D3 * t59 * t90 * (t32 - t103 * t55) + t118) * t96 / 0.1
     #440D4 - (0.90D2 * t36 * t31 * (-t124 * t76 + t126 * t84 / 0.2D1 + 
     #t129 + t132 * t32 - t134 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t9
     #0 * (t76 - t124 * t84 - t32 + t132 * t55) + t8 * t92) * t98 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (-t76 + t156 * t84 + t32 - t160 * t55
     #) - 0.180D3 * t59 * t90 * t166) * t171 * t98 / 0.720D3 + t175 * t1
     #66 * t96 * t177 / 0.8D1 - (0.90D2 * t36 * t31 * (-t32 + t184 * t55
     #) + 0.180D3 * t59 * t117) * t96 * t171 / 0.720D3 - (0.90D2 * t36 *
     # t31 * (t198 * t32 - t200 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t
     #90 * (-t32 + t198 * t55) - t118) * t171 / 0.720D3
      t216 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t215)
      t218 = 0.1D1 - x1
      t219 = KAPPA2(t218, x2, 0.0D0, 0.10D1, z)
      t220 = s * t219
      t221 = -t218
      t222 = t1 * t221
      t224 = t1 * x1
      t226 = t219 ** 2
      t232 = 0.1D1 / (-0.2D1 + t219)
      t233 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, 0
     #.10D1)
      t234 = t233 * t232
      t235 = t221 ** 2
      t236 = t16 * t235
      t237 = t226 ** 2
      t242 = log(0.4D1 * t153 * t236 * x4 * t237)
      t244 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, 0
     #.10D1)
      t250 = t59 * t27
      t251 = t31 * t232
      t252 = t251 * t244
      t254 = 0.180D3 * t250 * t252
      t264 = t181 * t11
      t269 = log(0.4D1 * t264 * t69 * t235 * t237)
      t283 = log(0.4D1 * t153 * t236 * t237)
      t284 = t283 * t232
      t286 = t283 ** 2
      t290 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, 0
     #.10D1)
      t306 = (0.90D2 * t36 * t31 * (t234 - t242 * t232 * t244) - t254) *
     # t171 * t98 / 0.720D3 + t36 * t251 * t244 * t96 * t177 / 0.8D1 - (
     #0.90D2 * t36 * t31 * (-t234 + t269 * t232 * t244) + t254) * t96 * 
     #t171 / 0.720D3 - (-0.90D2 * t36 * t31 * (-t284 * t233 + t286 * t23
     #2 * t244 / 0.2D1 + t232 * t290) + 0.180D3 * t59 * t90 * (t234 - t2
     #84 * t244) - t8 * t27 * t252) * t171 / 0.720D3
      t307 = FJET(XB1, XB2, s, 0.0D0, -t220 * t222, t220 * t224, 0.0D0, 
     #-s * t226 * t15 * t221 * x1, t306)
      t309 = KAPPA2(t218, x2, 0.0D0, t75, z)
      t310 = s * t309
      t312 = t224 * t79
      t314 = t224 * x4
      t316 = t309 ** 2
      t319 = t221 * x1
      t323 = 0.1D1 / (-0.2D1 + t309)
      t324 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, t
     #75)
      t327 = t316 ** 2
      t332 = log(-0.4D1 * t196 * t235 * x4 * t79 * t327)
      t334 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, t
     #75)
      t340 = t31 * t323
      t353 = (-0.90D2 * t36 * t31 * (t323 * t324 - t332 * t323 * t334) +
     # 0.180D3 * t250 * t340 * t334) * t171 * t98 / 0.720D3 - t36 * t340
     # * t334 * t96 * t177 / 0.8D1
      t354 = FJET(XB1, XB2, s, 0.0D0, -t310 * t222, -t310 * t312, t310 *
     # t314, s * t316 * t15 * t319 * t79, t353)
      t357 = -0.1D1 + x3
      t359 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t7
     #5)
      t360 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t361 = t359 - t360
      t366 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t367 = t69 * t357
      t370 = log(-0.4D1 * t264 * t367)
      t376 = t90 * t360
      t387 = log(-0.4D1 * t77 * t16 * t357 * x4)
      t389 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t7
     #5)
      t394 = log(0.4D1 * t77 * t78 * t79 * t357)
      t410 = log(-0.4D1 * t68 * t367)
      t412 = t410 ** 2
      t415 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t429 = t175 * t361 * t96 * t177 / 0.8D1 - (0.90D2 * t36 * t31 * (t
     #366 - t370 * t360) - 0.180D3 * t59 * t376) * t96 * t171 / 0.720D3 
     #- (0.90D2 * t36 * t31 * (t366 - t387 * t360 - t389 + t394 * t359) 
     #+ 0.180D3 * t59 * t90 * t361) * t96 * t98 / 0.1440D4 + (-0.90D2 * 
     #t36 * t31 * (-t410 * t366 + t412 * t360 / 0.2D1 + t415) + 0.180D3 
     #* t59 * t90 * (t366 - t410 * t360) - t8 * t376) * t96 / 0.1440D4
      t430 = FJET(XB1, XB2, s, t2 * x3, -t2 * t357, 0.0D0, 0.0D0, 0.0D0,
     # t429)
      t432 = KAPPA2(t218, x2, x3, 0.10D1, z)
      t433 = s * t432
      t434 = t222 * x3
      t436 = t222 * t357
      t439 = t432 ** 2
      t445 = 0.1D1 / (-0.2D1 + t432)
      t446 = t31 * t445
      t448 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, x3, 0.10
     #D1)
      t453 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, x3, 0.10
     #D1)
      t456 = t439 ** 2
      t461 = log(-0.4D1 * t181 * t14 * t236 * t357 * t456)
      t475 = -t36 * t446 * t448 * t96 * t177 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t445 * t453 - t461 * t445 * t448) - 0.180D3 * t250 * t446 * 
     #t448) * t96 * t171 / 0.720D3
      t476 = FJET(XB1, XB2, s, -t433 * t434, t433 * t436, t433 * t224, 0
     #.0D0, s * t439 * t15 * t319 * t357, t475)
      t478 = KAPPA2(t218, x2, x3, t75, z)
      t479 = s * t478
      t484 = t478 ** 2
      t489 = cos(t9)
      t493 = Sqrt(x3 * t357 * x4 * t79)
      t500 = 0.1D1 / (-0.2D1 + t478)
      t503 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, x3, t75)
      t508 = FJET(XB1, XB2, s, -t479 * t434, t479 * t436, -t479 * t312, 
     #t479 * t314, s * t484 * t15 * t319 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t489 * t493), t36 * t31 * t500 * t503 * t96 * t17
     #7 / 0.8D1)
      rrqg2qght5s3e0 = t216 * t215 + t307 * t306 + t354 * t353 + t430 * 
     #t429 + t476 * t475 + t508 * 0.3141592653589793D1 * t90 * t500 * t5
     #03 * t96 * t171 * t98 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t50 = t45 * t23
      t51 = 0.1D1 - x4
      t52 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t51)
      t53 = t52 - t41
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t7
      t61 = t9 * t12
      t64 = log(0.4D1 * t60 * t61)
      t70 = t19 * t23
      t73 = 0.180D3 * t3 * t70 * t41
      t77 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t51)
      t78 = t12 * x4
      t79 = -t51
      t83 = log(-0.4D1 * t10 * t78 * t79)
      t87 = log(0.4D1 * t10 * t78)
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t105 = t104 * t7
      t108 = log(0.4D1 * t105 * t61)
      t122 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 - t50 * t53 * t54 * t56 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t64 * t41) - t73) * t54 / 
     #0.1440D4 - (0.90D2 * t45 * t23 * (t77 - t83 * t52 - t24 + t87 * t4
     #1) - 0.180D3 * t3 * t70 * t53) * t56 / 0.1440D4 + t50 * t41 * t54 
     #* t100 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t108 * t41) + t73) 
     #* t100 / 0.720D3 - t50 * t53 * t100 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t122)
      t125 = 0.1D1 - x1
      t126 = KAPPA2(t125, x2, 0.0D0, 0.10D1, z)
      t127 = s * t126
      t128 = -t125
      t129 = t1 * t128
      t131 = t1 * x1
      t133 = t126 ** 2
      t139 = 0.1D1 / (-0.2D1 + t126)
      t140 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, 0
     #.10D1)
      t141 = t139 * t140
      t142 = t54 * t100
      t146 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, 0
     #.10D1)
      t149 = t128 ** 2
      t151 = t133 ** 2
      t155 = log(0.4D1 * t105 * t9 * t12 * t149 * t151)
      t170 = t100 * t56
      t174 = t50 * t141 * t142 / 0.8D1 - (-0.90D2 * t45 * t23 * (t139 * 
     #t146 - t155 * t139 * t140) + 0.180D3 * t3 * t19 * t23 * t139 * t14
     #0) * t100 / 0.720D3 + t50 * t141 * t170 / 0.8D1
      t175 = FJET(XB1, XB2, s, 0.0D0, -t127 * t129, t127 * t131, 0.0D0, 
     #-s * t133 * t11 * t128 * x1, t174)
      t177 = KAPPA2(t125, x2, 0.0D0, t51, z)
      t178 = s * t177
      t184 = t177 ** 2
      t187 = t128 * x1
      t192 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, t
     #51)
      t194 = 0.1D1 / (-0.2D1 + t177) * t192 * t170
      t197 = FJET(XB1, XB2, s, 0.0D0, -t178 * t129, -t178 * t131 * t79, 
     #t178 * t131 * x4, s * t184 * t11 * t187 * t79, -t50 * t194 / 0.8D1
     #)
      t203 = -0.1D1 + x3
      t205 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t210 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t5
     #1)
      t216 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t220 = log(-0.4D1 * t60 * t61 * t203)
      t232 = -t50 * t205 * t54 * t100 / 0.8D1 - t50 * (t205 - t210) * t5
     #4 * t56 / 0.16D2 + (-0.90D2 * t45 * t23 * (t216 - t220 * t205) + 0
     #.180D3 * t3 * t70 * t205) * t54 / 0.1440D4
      t233 = FJET(XB1, XB2, s, t2 * x3, -t2 * t203, 0.0D0, 0.0D0, 0.0D0,
     # t232)
      t235 = KAPPA2(t125, x2, x3, 0.10D1, z)
      t236 = s * t235
      t242 = t235 ** 2
      t249 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, x3, 0.10
     #D1)
      t251 = 0.1D1 / (-0.2D1 + t235) * t249 * t142
      t254 = FJET(XB1, XB2, s, -t236 * t129 * x3, t236 * t129 * t203, t2
     #36 * t131, 0.0D0, s * t242 * t11 * t187 * t203, -t50 * t251 / 0.8D
     #1)
      rrqg2qght5s3em1 = t123 * t122 + t175 * t174 - t197 * 0.31415926535
     #89793D1 * t70 * t194 / 0.8D1 + t233 * t232 - t254 * 0.314159265358
     #9793D1 * t70 * t251 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t9 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.1D1 - x4)
      t10 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t17 = t7 * t10
      t18 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t26 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = -t4 * t7 * (t9 - t10) / x4 / 0.16D2 + t4 * t17 * t18 / 0.16D
     #2 + t4 * t17 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 
     #0.3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) *
     # t3 * t17 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.1
     #0D1)
      t57 = t7 * t55 * t18
      t60 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, -t4 * t57 / 0.16D2)
      t65 = 0.1D1 - x1
      t66 = KAPPA2(t65, x2, 0.0D0, 0.10D1, z)
      t67 = s * t66
      t68 = -t65
      t73 = t66 ** 2
      t80 = 0.1D1 / (-0.2D1 + t66)
      t81 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t65, x2, 0.0D0, 0.1
     #0D1)
      t86 = FJET(XB1, XB2, s, 0.0D0, -t67 * t1 * t68, t67 * t1 * x1, 0.0
     #D0, -s * t73 * t38 * t68 * x1, t4 * t7 * t80 * t81 * t22 / 0.8D1)
      rrqg2qght5s3em2 = t50 * t49 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t86 * 0.3141592653589793D1 * t3 * t7 * t80 * t81 * t2
     #2 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s3em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s3em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t102 = t18 * x4
      t103 = -0.1D1 + x4
      t104 = t102 * t103
      t107 = log(-0.4D1 * t91 * t15 * t104)
      t108 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t110 = t107 ** 2
      t111 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t114 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t119 = t35 * t39
      t126 = -t111 + t86
      t127 = t119 * t126
      t130 = 0.1D1 / x1
      t132 = 0.1D1 / x4
      t135 = t91 * t92
      t137 = log(0.4D1 * t135)
      t142 = t137 ** 2
      t153 = t119 * t86
      t154 = t10 * t153
      t165 = x3 * t90
      t166 = t165 * t13
      t169 = log(0.4D1 * t166 * t93)
      t171 = x4 * t103
      t175 = log(-0.4D1 * t166 * t92 * t171)
      t184 = 0.1D1 / x3
      t186 = t130 * t132
      t189 = t165 * t19
      t191 = log(0.4D1 * t189)
      t193 = t191 ** 2
      t212 = log(0.4D1 * t16 * t102)
      t216 = log(-0.4D1 * t16 * t104)
      t221 = t216 ** 2
      t224 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t229 = t212 ** 2
      t241 = -t119 * t126
      t256 = x3 * t13
      t259 = log(0.4D1 * t256 * t93)
      t261 = t259 ** 2
      t264 = t256 * t15
      t267 = log(-0.4D1 * t264 * t104)
      t269 = t267 ** 2
      t289 = log(0.4D1 * t256 * t92)
      t294 = t289 ** 2
      t315 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (-t96 * t40 + t98 * t86 / 0.2D1
     # + t50 + t107 * t108 - t110 * t111 / 0.2D1 - t114) - 0.180D3 * t54
     # * t119 * (t40 - t96 * t86 - t108 + t107 * t111) + t44 * t127) * t
     #130 * t132 / 0.720D3 + (t44 * t119 * (t40 - t137 * t86) + 0.90D2 *
     # t63 * t39 * (t142 * t40 / 0.2D1 + t59 - t142 * t137 * t86 / 0.6D1
     # - t137 * t50) + t154 - 0.180D3 * t54 * t119 * (-t137 * t40 + t142
     # * t86 / 0.2D1 + t50)) * t130 / 0.720D3 + (0.90D2 * t63 * t39 * (t
     #40 - t169 * t86 - t108 + t175 * t111) - 0.180D3 * t54 * t127) * t1
     #84 * t186 / 0.720D3 + (0.90D2 * t63 * t39 * (-t191 * t40 + t193 * 
     #t86 / 0.2D1 + t50) - 0.180D3 * t54 * t119 * (t40 - t191 * t86) + t
     #44 * t153) * t184 * t130 / 0.720D3 - (t44 * t119 * (-t40 + t212 * 
     #t86 + t108 - t216 * t111) + 0.90D2 * t63 * t39 * (t221 * t108 / 0.
     #2D1 + t224 - t221 * t216 * t111 / 0.6D1 - t216 * t114 - t229 * t40
     # / 0.2D1 - t59 + t229 * t212 * t86 / 0.6D1 + t212 * t50) + t10 * t
     #241 - 0.180D3 * t54 * t119 * (t212 * t40 - t229 * t86 / 0.2D1 - t5
     #0 - t216 * t108 + t221 * t111 / 0.2D1 + t114)) * t132 / 0.1440D4 -
     # (0.90D2 * t63 * t39 * (t259 * t40 - t261 * t86 / 0.2D1 - t50 - t2
     #67 * t108 + t269 * t111 / 0.2D1 + t114) - 0.180D3 * t54 * t119 * (
     #-t40 + t259 * t86 + t108 - t267 * t111) + t44 * t241) * t184 * t13
     #2 / 0.1440D4 + (t44 * t119 * (t40 - t289 * t86) + 0.90D2 * t63 * t
     #39 * (t294 * t40 / 0.2D1 + t59 - t294 * t289 * t86 / 0.6D1 - t289 
     #* t50) + t154 - 0.180D3 * t54 * t119 * (-t289 * t40 + t294 * t86 /
     # 0.2D1 + t50)) * t184 / 0.1440D4
      t316 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t315)
      t318 = -0.1D1 + x1
      t321 = t318 ** 2
      t326 = log(0.4D1 * t19 * t90 * t321 * x4)
      t327 = -t318
      t328 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t330 = t326 ** 2
      t331 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t334 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t344 = t119 * t331
      t345 = t44 * t344
      t349 = t18 * t90
      t350 = t349 * t321
      t353 = log(0.4D1 * t16 * t350)
      t358 = t353 ** 2
      t361 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t380 = t321 * x4
      t384 = log(0.4D1 * t264 * t349 * t380)
      t397 = log(0.4D1 * t264 * t350)
      t399 = t397 ** 2
      t415 = (0.90D2 * t63 * t39 * (t326 * t328 - t330 * t331 / 0.2D1 - 
     #t334) - 0.180D3 * t54 * t119 * (-t328 + t326 * t331) - t345) * t13
     #0 * t132 / 0.720D3 + (-t44 * t119 * (t328 - t353 * t331) - 0.90D2 
     #* t63 * t39 * (t358 * t328 / 0.2D1 + t361 - t358 * t353 * t331 / 0
     #.6D1 - t353 * t334) - t10 * t344 + 0.180D3 * t54 * t119 * (-t353 *
     # t328 + t358 * t331 / 0.2D1 + t334)) * t130 / 0.720D3 + (0.90D2 * 
     #t63 * t39 * (-t328 + t384 * t331) + 0.180D3 * t54 * t344) * t184 *
     # t186 / 0.720D3 + (0.90D2 * t63 * t39 * (t397 * t328 - t399 * t331
     # / 0.2D1 - t334) - 0.180D3 * t54 * t119 * (-t328 + t397 * t331) - 
     #t345) * t184 * t130 / 0.720D3
      t416 = FJET(XB1, XB2, s, 0.0D0, -t2 * t318, 0.0D0, t2 * x1, 0.0D0,
     # t415)
      t418 = KAPPA2(t327, x2, 0.0D0, x4, z)
      t419 = s * t418
      t420 = t1 * t318
      t422 = t1 * x1
      t423 = t422 * x4
      t425 = t422 * t103
      t427 = t418 ** 2
      t430 = t318 * x1
      t433 = t427 ** 2
      t438 = log(-0.4D1 * t135 * t380 * t103 * t433)
      t440 = 0.1D1 / (-0.2D1 + t418)
      t441 = t438 * t440
      t442 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, x
     #4)
      t444 = t438 ** 2
      t446 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, x
     #4)
      t449 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, x
     #4)
      t455 = t440 * t442
      t461 = t44 * t35
      t463 = t39 * t440 * t446
      t468 = t165 * t16
      t469 = t18 * t321
      t474 = log(-0.4D1 * t468 * t469 * t171 * t433)
      t481 = t54 * t35
      t488 = (-0.90D2 * t63 * t39 * (-t441 * t442 + t444 * t440 * t446 /
     # 0.2D1 + t440 * t449) + 0.180D3 * t54 * t119 * (t455 - t441 * t446
     #) - t461 * t463) * t130 * t132 / 0.720D3 + (0.90D2 * t63 * t39 * (
     #-t455 + t474 * t440 * t446) + 0.180D3 * t481 * t463) * t184 * t186
     # / 0.720D3
      t489 = FJET(XB1, XB2, s, 0.0D0, -t419 * t420, t419 * t423, -t419 *
     # t425, -s * t427 * t17 * t430 * x4, t488)
      t492 = -0.1D1 + x3
      t495 = t102 * t103 * t492
      t498 = log(0.4D1 * t264 * t495)
      t499 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t501 = t498 ** 2
      t502 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t505 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t510 = log(-0.4D1 * t264 * t18 * t492 * x4)
      t511 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t513 = t510 ** 2
      t514 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t517 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t528 = t514 - t502
      t535 = t92 * t492
      t538 = log(-0.4D1 * t256 * t535)
      t543 = t538 ** 2
      t546 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t555 = t119 * t514
      t567 = t492 * x4
      t571 = log(-0.4D1 * t166 * t92 * t567)
      t575 = log(0.4D1 * t468 * t495)
      t591 = log(-0.4D1 * t166 * t535)
      t593 = t591 ** 2
      t610 = -(0.90D2 * t63 * t39 * (t498 * t499 - t501 * t502 / 0.2D1 -
     # t505 - t510 * t511 + t514 * t513 / 0.2D1 + t517) - 0.180D3 * t54 
     #* t119 * (-t499 + t498 * t502 + t511 - t510 * t514) + t44 * t119 *
     # t528) * t184 * t132 / 0.1440D4 + (-t44 * t119 * (t511 - t538 * t5
     #14) - 0.90D2 * t63 * t39 * (t543 * t511 / 0.2D1 + t546 - t543 * t5
     #38 * t514 / 0.6D1 - t538 * t517) - t10 * t555 + 0.180D3 * t54 * t1
     #19 * (-t538 * t511 + t543 * t514 / 0.2D1 + t517)) * t184 / 0.1440D
     #4 + (0.90D2 * t63 * t39 * (-t511 + t571 * t514 + t499 - t575 * t50
     #2) + 0.180D3 * t54 * t119 * t528) * t184 * t186 / 0.720D3 + (0.90D
     #2 * t63 * t39 * (t591 * t511 - t593 * t514 / 0.2D1 - t517) - 0.180
     #D3 * t54 * t119 * (-t511 + t591 * t514) - t44 * t555) * t184 * t13
     #0 / 0.720D3
      t611 = FJET(XB1, XB2, s, t2 * x3, -t2 * t492, 0.0D0, 0.0D0, 0.0D0,
     # t610)
      t613 = KAPPA2(t327, x2, x3, 0.0D0, z)
      t614 = s * t613
      t615 = t420 * x3
      t617 = t420 * t492
      t620 = t613 ** 2
      t626 = 0.1D1 / (-0.2D1 + t613)
      t627 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, 0.0D
     #0)
      t628 = t626 * t627
      t629 = t620 ** 2
      t634 = log(-0.4D1 * t468 * t469 * t567 * t629)
      t636 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, 0.0D
     #0)
      t643 = t39 * t626 * t636
      t653 = log(-0.4D1 * t468 * t469 * t492 * t629)
      t654 = t653 * t626
      t656 = t653 ** 2
      t660 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, 0.0D
     #0)
      t676 = (0.90D2 * t63 * t39 * (-t628 + t634 * t626 * t636) + 0.180D
     #3 * t481 * t643) * t184 * t186 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(-t654 * t627 + t656 * t626 * t636 / 0.2D1 + t626 * t660) + 0.180D
     #3 * t54 * t119 * (t628 - t654 * t636) - t461 * t643) * t184 * t130
     # / 0.720D3
      t677 = FJET(XB1, XB2, s, -t614 * t615, t614 * t617, 0.0D0, t614 * 
     #t422, -s * t620 * t17 * t430 * x3, t676)
      t679 = KAPPA2(t327, x2, x3, x4, z)
      t680 = s * t679
      t685 = t679 ** 2
      t690 = cos(t11)
      t693 = Sqrt(x3 * t492 * t171)
      t700 = 0.1D1 / (-0.2D1 + t679)
      t701 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, x4)
      t704 = t685 ** 2
      t709 = log(0.4D1 * t189 * t321 * t492 * t171 * t704)
      t711 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, x4)
      t721 = 0.90D2 * t63 * t39 * (t701 * t700 - t709 * t700 * t711) - 0
     #.180D3 * t481 * t39 * t700 * t711
      t725 = FJET(XB1, XB2, s, -t680 * t615, t680 * t617, t680 * t423, -
     #t680 * t425, s * t685 * t17 * t430 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t690 * t693), t721 * t184 * t186 / 0.720D3)
      rrqg2qght5s4e1 = t316 * t315 + t416 * t415 + t489 * t488 + t611 * 
     #t610 + t677 * t676 + t725 * t721 * t184 * t130 * t132 / 0.720D3

      end function



      doubleprecision function rrqg2qght5s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t76 = t68 * t13
      t77 = t16 * x4
      t78 = -0.1D1 + x4
      t79 = t77 * t78
      t82 = log(-0.4D1 * t76 * t79)
      t83 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t89 = t27 * t31
      t90 = -t55 + t83
      t91 = t89 * t90
      t95 = 0.1D1 / x3
      t97 = 0.1D1 / x4
      t102 = log(0.4D1 * t68 * t69)
      t104 = t102 ** 2
      t116 = t89 * t55
      t117 = t8 * t116
      t123 = log(0.4D1 * t14 * t77)
      t125 = t123 ** 2
      t130 = log(-0.4D1 * t14 * t79)
      t132 = t130 ** 2
      t135 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t150 = x1 ** 2
      t151 = t150 * t11
      t154 = log(0.4D1 * t151 * t70)
      t159 = log(-0.4D1 * t151 * t13 * t79)
      t165 = -t90
      t170 = 0.1D1 / x1
      t174 = t36 * t31
      t176 = t170 * t97
      t180 = x3 * t150
      t183 = log(0.4D1 * t180 * t17)
      t195 = t151 * t69
      t197 = log(0.4D1 * t195)
      t199 = t197 ** 2
      t214 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 - (0.90D2 * t36 * t31 * (-t32 + t73 
     #* t55 + t75 - t82 * t83) - 0.180D3 * t59 * t91) * t95 * t97 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (-t102 * t32 + t104 * t55 / 0.2D1 + t
     #64) - 0.180D3 * t59 * t89 * (t32 - t102 * t55) + t117) * t95 / 0.1
     #440D4 - (0.90D2 * t36 * t31 * (t123 * t32 - t125 * t55 / 0.2D1 - t
     #64 - t130 * t75 + t132 * t83 / 0.2D1 + t135) - 0.180D3 * t59 * t89
     # * (-t32 + t123 * t55 + t75 - t130 * t83) + t8 * t91) * t97 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (t32 - t154 * t55 - t75 + t159 * t83)
     # - 0.180D3 * t59 * t89 * t165) * t170 * t97 / 0.720D3 + t174 * t16
     #5 * t95 * t176 / 0.8D1 + (0.90D2 * t36 * t31 * (t32 - t183 * t55) 
     #- 0.180D3 * t59 * t116) * t95 * t170 / 0.720D3 + (0.90D2 * t36 * t
     #31 * (-t197 * t32 + t199 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t8
     #9 * (t32 - t197 * t55) + t117) * t170 / 0.720D3
      t215 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t214)
      t217 = -0.1D1 + x1
      t220 = -t217
      t221 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, 0
     #.0D0)
      t222 = t217 ** 2
      t227 = log(0.4D1 * t17 * t150 * t222 * x4)
      t228 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, 0
     #.0D0)
      t234 = t89 * t228
      t236 = 0.180D3 * t59 * t234
      t246 = t16 * t150 * t222
      t249 = log(0.4D1 * t76 * t246)
      t261 = log(0.4D1 * t14 * t246)
      t263 = t261 ** 2
      t266 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, 0
     #.0D0)
      t280 = (0.90D2 * t36 * t31 * (-t221 + t227 * t228) + t236) * t170 
     #* t97 / 0.720D3 - t174 * t228 * t95 * t176 / 0.8D1 + (0.90D2 * t36
     # * t31 * (-t221 + t249 * t228) + t236) * t95 * t170 / 0.720D3 + (-
     #0.90D2 * t36 * t31 * (-t261 * t221 + t263 * t228 / 0.2D1 + t266) +
     # 0.180D3 * t59 * t89 * (t221 - t261 * t228) - t8 * t234) * t170 / 
     #0.720D3
      t281 = FJET(XB1, XB2, s, 0.0D0, -t2 * t217, 0.0D0, t2 * x1, 0.0D0,
     # t280)
      t283 = KAPPA2(t220, x2, 0.0D0, x4, z)
      t284 = s * t283
      t285 = t1 * t217
      t287 = t1 * x1
      t288 = t287 * x4
      t290 = t287 * t78
      t292 = t283 ** 2
      t295 = t217 * x1
      t299 = 0.1D1 / (-0.2D1 + t283)
      t300 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, x
     #4)
      t303 = t292 ** 2
      t308 = log(-0.4D1 * t195 * t222 * x4 * t78 * t303)
      t310 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, x
     #4)
      t316 = t59 * t27
      t317 = t31 * t299
      t330 = (-0.90D2 * t36 * t31 * (t299 * t300 - t308 * t299 * t310) +
     # 0.180D3 * t316 * t317 * t310) * t170 * t97 / 0.720D3 - t36 * t317
     # * t310 * t95 * t176 / 0.8D1
      t331 = FJET(XB1, XB2, s, 0.0D0, -t284 * t285, t284 * t288, -t284 *
     # t290, -s * t292 * t15 * t295 * x4, t330)
      t334 = -0.1D1 + x3
      t336 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t337 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t338 = t336 - t337
      t343 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t345 = t69 * t334
      t348 = log(-0.4D1 * t180 * t11 * t345)
      t354 = t89 * t337
      t361 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t366 = log(0.4D1 * t76 * t77 * t78 * t334)
      t372 = log(-0.4D1 * t76 * t16 * t334 * x4)
      t388 = log(-0.4D1 * t68 * t345)
      t390 = t388 ** 2
      t393 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t407 = t174 * t338 * t95 * t176 / 0.8D1 + (0.90D2 * t36 * t31 * (-
     #t343 + t348 * t337) + 0.180D3 * t59 * t354) * t95 * t170 / 0.720D3
     # - (0.90D2 * t36 * t31 * (-t361 + t366 * t336 + t343 - t372 * t337
     #) + 0.180D3 * t59 * t89 * t338) * t95 * t97 / 0.1440D4 + (-0.90D2 
     #* t36 * t31 * (-t388 * t343 + t390 * t337 / 0.2D1 + t393) + 0.180D
     #3 * t59 * t89 * (t343 - t388 * t337) - t8 * t354) * t95 / 0.1440D4
      t408 = FJET(XB1, XB2, s, t2 * x3, -t2 * t334, 0.0D0, 0.0D0, 0.0D0,
     # t407)
      t410 = KAPPA2(t220, x2, x3, 0.0D0, z)
      t411 = s * t410
      t412 = t285 * x3
      t414 = t285 * t334
      t417 = t410 ** 2
      t423 = 0.1D1 / (-0.2D1 + t410)
      t424 = t31 * t423
      t426 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, x3, 0.0D
     #0)
      t431 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t220, x2, x3, 0.0D
     #0)
      t435 = t417 ** 2
      t440 = log(-0.4D1 * t180 * t14 * t16 * t222 * t334 * t435)
      t454 = -t36 * t424 * t426 * t95 * t176 / 0.8D1 + (-0.90D2 * t36 * 
     #t31 * (t423 * t431 - t440 * t423 * t426) + 0.180D3 * t316 * t424 *
     # t426) * t95 * t170 / 0.720D3
      t455 = FJET(XB1, XB2, s, -t411 * t412, t411 * t414, 0.0D0, t411 * 
     #t287, -s * t417 * t15 * t295 * x3, t454)
      t457 = KAPPA2(t220, x2, x3, x4, z)
      t458 = s * t457
      t463 = t457 ** 2
      t468 = cos(t9)
      t472 = Sqrt(x3 * t334 * x4 * t78)
      t479 = 0.1D1 / (-0.2D1 + t457)
      t482 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, x3, x4)
      t487 = FJET(XB1, XB2, s, -t458 * t412, t458 * t414, t458 * t288, -
     #t458 * t290, s * t463 * t15 * t295 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t468 * t472), t36 * t31 * t479 * t482 * t95 * t176 / 0.8D
     #1)
      rrqg2qght5s4e0 = t215 * t214 + t281 * t280 + t330 * t331 + t408 * 
     #t407 + t455 * t454 + t487 * 0.3141592653589793D1 * t89 * t479 * t4
     #82 * t95 * t170 * t97 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t50 = t45 * t23
      t51 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t52 = -t41 + t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t7
      t60 = t9 * t12
      t63 = log(0.4D1 * t59 * t60)
      t69 = t19 * t23
      t72 = 0.180D3 * t3 * t69 * t41
      t76 = t12 * x4
      t79 = log(0.4D1 * t10 * t76)
      t81 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t82 = -0.1D1 + x4
      t86 = log(-0.4D1 * t10 * t76 * t82)
      t99 = 0.1D1 / x1
      t103 = x1 ** 2
      t107 = log(0.4D1 * t103 * t7 * t60)
      t121 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 - t50 * t52 * t53 * t55 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t63 * t41) - t72) * t53 / 
     #0.1440D4 - (0.90D2 * t45 * t23 * (-t24 + t79 * t41 + t81 - t86 * t
     #51) - 0.180D3 * t3 * t69 * t52) * t55 / 0.1440D4 + t50 * t41 * t53
     # * t99 / 0.8D1 + (0.90D2 * t45 * t23 * (t24 - t107 * t41) - t72) *
     # t99 / 0.720D3 - t50 * t52 * t99 * t55 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t124 = -0.1D1 + x1
      t127 = -t124
      t128 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, 0.0D0, 0
     #.0D0)
      t133 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t127, x2, 0.0D0, 0
     #.0D0)
      t135 = t124 ** 2
      t139 = log(0.4D1 * t10 * t12 * t103 * t135)
      t155 = -t50 * t128 * t53 * t99 / 0.8D1 + (-0.90D2 * t45 * t23 * (t
     #133 - t139 * t128) + 0.180D3 * t3 * t69 * t128) * t99 / 0.720D3 - 
     #t50 * t128 * t99 * t55 / 0.8D1
      t156 = FJET(XB1, XB2, s, 0.0D0, -t2 * t124, 0.0D0, t2 * x1, 0.0D0,
     # t155)
      t158 = KAPPA2(t127, x2, 0.0D0, x4, z)
      t159 = s * t158
      t160 = t1 * t124
      t162 = t1 * x1
      t167 = t158 ** 2
      t170 = t124 * x1
      t175 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, 0.0D0, x
     #4)
      t178 = 0.1D1 / (-0.2D1 + t158) * t175 * t99 * t55
      t181 = FJET(XB1, XB2, s, 0.0D0, -t159 * t160, t159 * t162 * x4, -t
     #159 * t162 * t82, -s * t167 * t11 * t170 * x4, -t50 * t178 / 0.8D1
     #)
      t187 = -0.1D1 + x3
      t189 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t194 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t200 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t204 = log(-0.4D1 * t59 * t60 * t187)
      t216 = -t50 * t189 * t53 * t99 / 0.8D1 - t50 * (t189 - t194) * t53
     # * t55 / 0.16D2 + (-0.90D2 * t45 * t23 * (t200 - t204 * t189) + 0.
     #180D3 * t3 * t69 * t189) * t53 / 0.1440D4
      t217 = FJET(XB1, XB2, s, t2 * x3, -t2 * t187, 0.0D0, 0.0D0, 0.0D0,
     # t216)
      t219 = KAPPA2(t127, x2, x3, 0.0D0, z)
      t220 = s * t219
      t226 = t219 ** 2
      t233 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, x3, 0.0D
     #0)
      t236 = 0.1D1 / (-0.2D1 + t219) * t233 * t53 * t99
      t239 = FJET(XB1, XB2, s, -t220 * t160 * x3, t220 * t160 * t187, 0.
     #0D0, t220 * t162, -s * t226 * t11 * t170 * x3, -t50 * t236 / 0.8D1
     #)
      rrqg2qght5s4em1 = t122 * t121 + t156 * t155 - t181 * 0.31415926535
     #89793D1 * t69 * t178 / 0.8D1 + t217 * t216 - t239 * 0.314159265358
     #9793D1 * t69 * t236 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t9 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, x
     #4)
      t16 = t7 * t8
      t17 = 0.1D1 / x3
      t21 = 0.1D1 / x1
      t25 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = -t4 * t7 * (-t8 + t9) / x4 / 0.16D2 + t4 * t16 * t17 / 0.16D
     #2 + t4 * t16 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 
     #0.3141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) *
     # t3 * t16 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t54 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.0
     #D0)
      t56 = t7 * t54 * t17
      t59 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, -t4 * t56 / 0.16D2)
      t64 = -0.1D1 + x1
      t68 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, -t64, x2, 0.0D0, 0.
     #0D0)
      t70 = t7 * t68 * t21
      t73 = FJET(XB1, XB2, s, 0.0D0, -t2 * t64, 0.0D0, t2 * x1, 0.0D0, -
     #t4 * t70 / 0.8D1)
      rrqg2qght5s4em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.16D2 - t73 * 0.3141592653589793D1 * t3 * t70 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s4em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s4em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s5e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t105 = t35 * t39
      t111 = t105 * t86
      t114 = 0.1D1 / x1
      t116 = 0.1D1 / x4
      t119 = t91 * t92
      t121 = log(0.4D1 * t119)
      t126 = t121 ** 2
      t137 = t10 * t111
      t148 = 0.1D1 - x3
      t149 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t150 = x3 * t90
      t151 = t150 * t13
      t152 = -t148
      t153 = t152 * x4
      t157 = log(-0.4D1 * t151 * t92 * t153)
      t158 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t162 = log(0.4D1 * t151 * t93)
      t169 = t105 * (-t158 + t86)
      t173 = 0.1D1 / x3
      t175 = t114 * t116
      t178 = t150 * t19
      t180 = log(0.4D1 * t178)
      t182 = t180 ** 2
      t185 = t92 * t152
      t188 = log(-0.4D1 * t151 * t185)
      t190 = t188 ** 2
      t193 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t204 = t44 * t169
      t209 = t18 * x4
      t212 = log(0.4D1 * t16 * t209)
      t217 = t212 ** 2
      t238 = x3 * t13
      t241 = log(0.4D1 * t238 * t93)
      t243 = t241 ** 2
      t246 = t238 * t15
      t251 = log(-0.4D1 * t246 * t18 * t152 * x4)
      t253 = t251 ** 2
      t272 = log(-0.4D1 * t238 * t185)
      t276 = log(0.4D1 * t238 * t92)
      t281 = t276 ** 2
      t288 = t272 ** 2
      t291 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t314 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (-t96 * t40 + t98 * t86 / 0.2D1
     # + t50) - 0.180D3 * t54 * t105 * (t40 - t96 * t86) + t44 * t111) *
     # t114 * t116 / 0.720D3 - (t44 * t105 * (-t40 + t121 * t86) + 0.90D
     #2 * t63 * t39 * (-t126 * t40 / 0.2D1 - t59 + t126 * t121 * t86 / 0
     #.6D1 + t121 * t50) - t137 - 0.180D3 * t54 * t105 * (t121 * t40 - t
     #126 * t86 / 0.2D1 - t50)) * t114 / 0.720D3 + (0.90D2 * t63 * t39 *
     # (-t149 + t157 * t158 + t40 - t162 * t86) - 0.180D3 * t54 * t169) 
     #* t173 * t175 / 0.720D3 + (0.90D2 * t63 * t39 * (-t180 * t40 + t18
     #2 * t86 / 0.2D1 + t50 + t188 * t149 - t190 * t158 / 0.2D1 - t193) 
     #- 0.180D3 * t54 * t105 * (t40 - t180 * t86 - t149 + t188 * t158) +
     # t204) * t173 * t114 / 0.720D3 + (t44 * t105 * (t40 - t212 * t86) 
     #+ 0.90D2 * t63 * t39 * (t217 * t40 / 0.2D1 + t59 - t217 * t212 * t
     #86 / 0.6D1 - t212 * t50) + t137 - 0.180D3 * t54 * t105 * (-t212 * 
     #t40 + t217 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 + (0.90D2 * t63
     # * t39 * (-t241 * t40 + t243 * t86 / 0.2D1 + t50 + t251 * t149 - t
     #253 * t158 / 0.2D1 - t193) - 0.180D3 * t54 * t105 * (t40 - t241 * 
     #t86 - t149 + t251 * t158) + t204) * t173 * t116 / 0.1440D4 + (t44 
     #* t105 * (-t149 + t272 * t158 + t40 - t276 * t86) + 0.90D2 * t63 *
     # t39 * (t281 * t40 / 0.2D1 + t59 - t281 * t276 * t86 / 0.6D1 - t27
     #6 * t50 - t288 * t149 / 0.2D1 - t291 + t288 * t272 * t158 / 0.6D1 
     #+ t272 * t193) + t10 * t169 - 0.180D3 * t54 * t105 * (t272 * t149 
     #- t288 * t158 / 0.2D1 - t193 - t276 * t40 + t281 * t86 / 0.2D1 + t
     #50)) * t173 / 0.1440D4
      t315 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t314)
      t317 = -0.1D1 + x4
      t320 = -t317
      t321 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t320)
      t322 = t209 * t317
      t325 = log(-0.4D1 * t16 * t322)
      t326 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t320)
      t331 = t325 ** 2
      t334 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t320)
      t338 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t320)
      t344 = t105 * t326
      t358 = log(-0.4D1 * t246 * t322)
      t360 = t358 ** 2
      t364 = t209 * t317 * t152
      t367 = log(0.4D1 * t246 * t364)
      t368 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, t
     #320)
      t370 = t367 ** 2
      t371 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, t
     #320)
      t374 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, t
     #320)
      t386 = t105 * (-t326 + t371)
      t395 = log(-0.4D1 * t91 * t15 * t322)
      t397 = t395 ** 2
      t414 = t150 * t16
      t417 = log(0.4D1 * t414 * t364)
      t419 = x4 * t317
      t423 = log(-0.4D1 * t151 * t92 * t419)
      t435 = (-t44 * t105 * (t321 - t325 * t326) - 0.90D2 * t63 * t39 * 
     #(t331 * t321 / 0.2D1 + t334 - t331 * t325 * t326 / 0.6D1 - t325 * 
     #t338) - t10 * t344 + 0.180D3 * t54 * t105 * (-t325 * t321 + t326 *
     # t331 / 0.2D1 + t338)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #t358 * t321 - t360 * t326 / 0.2D1 - t338 - t367 * t368 + t370 * t3
     #71 / 0.2D1 + t374) - 0.180D3 * t54 * t105 * (-t321 + t358 * t326 +
     # t368 - t367 * t371) + t44 * t386) * t173 * t116 / 0.1440D4 + (0.9
     #0D2 * t63 * t39 * (t395 * t321 - t397 * t326 / 0.2D1 - t338) - 0.1
     #80D3 * t54 * t105 * (-t321 + t395 * t326) - t44 * t344) * t114 * t
     #116 / 0.720D3 + (0.90D2 * t63 * t39 * (t368 - t417 * t371 - t321 +
     # t423 * t326) - 0.180D3 * t54 * t386) * t173 * t175 / 0.720D3
      t436 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t317, t2 * x4, 0.0D0,
     # t435)
      t439 = -0.1D1 + x1
      t441 = t439 ** 2
      t446 = log(0.4D1 * t19 * t90 * t441 * x4)
      t447 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t449 = t446 ** 2
      t450 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t453 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t463 = t105 * t450
      t464 = t44 * t463
      t468 = t18 * t90
      t469 = t468 * t441
      t472 = log(0.4D1 * t16 * t469)
      t477 = t472 ** 2
      t480 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t499 = t441 * x4
      t503 = log(0.4D1 * t246 * t468 * t499)
      t516 = log(0.4D1 * t246 * t469)
      t518 = t516 ** 2
      t534 = (0.90D2 * t63 * t39 * (t446 * t447 - t449 * t450 / 0.2D1 - 
     #t453) - 0.180D3 * t54 * t105 * (-t447 + t446 * t450) - t464) * t11
     #4 * t116 / 0.720D3 - (t44 * t105 * (t447 - t472 * t450) + 0.90D2 *
     # t63 * t39 * (t477 * t447 / 0.2D1 + t480 - t477 * t472 * t450 / 0.
     #6D1 - t472 * t453) + t10 * t463 - 0.180D3 * t54 * t105 * (-t472 * 
     #t447 + t477 * t450 / 0.2D1 + t453)) * t114 / 0.720D3 + (0.90D2 * t
     #63 * t39 * (-t447 + t503 * t450) + 0.180D3 * t54 * t463) * t173 * 
     #t175 / 0.720D3 + (0.90D2 * t63 * t39 * (t516 * t447 - t518 * t450 
     #/ 0.2D1 - t453) - 0.180D3 * t54 * t105 * (-t447 + t516 * t450) - t
     #464) * t173 * t114 / 0.720D3
      t535 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t439, 0.0D0, 0.0D0,
     # t534)
      t537 = KAPPA2(x1, x2, 0.10D1, t320, z)
      t538 = s * t537
      t539 = t1 * x1
      t541 = t1 * t439
      t542 = t541 * t317
      t544 = t541 * x4
      t546 = t537 ** 2
      t549 = x1 * t439
      t552 = t546 ** 2
      t557 = log(-0.4D1 * t119 * t499 * t317 * t552)
      t559 = 0.1D1 / (-0.2D1 + t537)
      t560 = t557 * t559
      t561 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #20)
      t563 = t557 ** 2
      t565 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #20)
      t568 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #20)
      t574 = t559 * t561
      t580 = t44 * t35
      t582 = t39 * t559 * t565
      t587 = t18 * t441
      t592 = log(-0.4D1 * t414 * t587 * t419 * t552)
      t599 = t54 * t35
      t606 = (-0.90D2 * t63 * t39 * (-t560 * t561 + t563 * t559 * t565 /
     # 0.2D1 + t559 * t568) + 0.180D3 * t54 * t105 * (t574 - t560 * t565
     #) - t580 * t582) * t114 * t116 / 0.720D3 + (0.90D2 * t63 * t39 * (
     #-t574 + t592 * t559 * t565) + 0.180D3 * t599 * t582) * t173 * t175
     # / 0.720D3
      t607 = FJET(XB1, XB2, s, t538 * t539, 0.0D0, t538 * t542, -t538 * 
     #t544, -s * t546 * t17 * t549 * x4, t606)
      t609 = KAPPA2(x1, x2, t148, 0.10D1, z)
      t610 = s * t609
      t611 = t539 * t152
      t613 = t539 * x3
      t616 = t609 ** 2
      t622 = 0.1D1 / (-0.2D1 + t609)
      t623 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, 0.10
     #D1)
      t624 = t622 * t623
      t625 = t616 ** 2
      t630 = log(-0.4D1 * t414 * t587 * t153 * t625)
      t632 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, 0.10
     #D1)
      t639 = t39 * t622 * t632
      t649 = log(-0.4D1 * t414 * t587 * t152 * t625)
      t650 = t649 * t622
      t652 = t649 ** 2
      t656 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, 0.10
     #D1)
      t672 = (0.90D2 * t63 * t39 * (-t624 + t630 * t622 * t632) + 0.180D
     #3 * t599 * t639) * t173 * t175 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(-t650 * t623 + t652 * t622 * t632 / 0.2D1 + t622 * t656) + 0.180D
     #3 * t54 * t105 * (t624 - t650 * t632) - t580 * t639) * t173 * t114
     # / 0.720D3
      t673 = FJET(XB1, XB2, s, -t611 * t610, t610 * t613, -t610 * t541, 
     #0.0D0, -s * t616 * t17 * t549 * x3, t672)
      t675 = KAPPA2(x1, x2, t148, t320, z)
      t676 = s * t675
      t681 = t675 ** 2
      t686 = cos(t11)
      t689 = Sqrt(x3 * t152 * t419)
      t696 = 0.1D1 / (-0.2D1 + t675)
      t697 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, t320
     #)
      t700 = t681 ** 2
      t705 = log(0.4D1 * t178 * t441 * t152 * t419 * t700)
      t707 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, t320
     #)
      t717 = 0.90D2 * t63 * t39 * (t696 * t697 - t705 * t696 * t707) - 0
     #.180D3 * t599 * t39 * t696 * t707
      t721 = FJET(XB1, XB2, s, -t676 * t611, t676 * t613, t676 * t542, -
     #t676 * t544, s * t681 * t17 * t549 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t686 * t689), t717 * t173 * t175 / 0.720D3)
      rrqg2qght5s5e1 = t315 * t314 + t436 * t435 + t535 * t534 + t607 * 
     #t606 + t673 * t672 + t721 * t717 * t173 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrqg2qght5s5e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = 0.1D1 - x3
      t76 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.1
     #0D1)
      t77 = t68 * t13
      t78 = -t75
      t83 = log(-0.4D1 * t77 * t16 * t78 * x4)
      t84 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.1
     #0D1)
      t90 = t27 * t31
      t91 = -t84 + t55
      t92 = t90 * t91
      t94 = 0.180D3 * t59 * t92
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t101 = t69 * t78
      t104 = log(-0.4D1 * t68 * t101)
      t106 = t104 ** 2
      t109 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.
     #10D1)
      t112 = log(0.4D1 * t68 * t69)
      t114 = t112 ** 2
      t131 = t16 * x4
      t134 = log(0.4D1 * t14 * t131)
      t136 = t134 ** 2
      t148 = t90 * t55
      t149 = t8 * t148
      t153 = x1 ** 2
      t154 = t153 * t11
      t157 = log(0.4D1 * t154 * t70)
      t166 = 0.1D1 / x1
      t170 = t36 * t31
      t172 = t166 * t98
      t176 = x3 * t153
      t179 = log(0.4D1 * t176 * t17)
      t184 = log(-0.4D1 * t176 * t11 * t101)
      t194 = t154 * t69
      t196 = log(0.4D1 * t194)
      t198 = t196 ** 2
      t213 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (t32 - t73 *
     # t55 - t76 + t83 * t84) - t94) * t96 * t98 / 0.1440D4 + (0.90D2 * 
     #t36 * t31 * (t104 * t76 - t106 * t84 / 0.2D1 - t109 - t112 * t32 +
     # t114 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t90 * (-t76 + t104 * 
     #t84 + t32 - t112 * t55) + t8 * t92) * t96 / 0.1440D4 + (0.90D2 * t
     #36 * t31 * (-t134 * t32 + t136 * t55 / 0.2D1 + t64) - 0.180D3 * t5
     #9 * t90 * (t32 - t134 * t55) + t149) * t98 / 0.1440D4 + (0.90D2 * 
     #t36 * t31 * (t32 - t157 * t55) - 0.180D3 * t59 * t148) * t166 * t9
     #8 / 0.720D3 + t170 * t91 * t96 * t172 / 0.8D1 + (0.90D2 * t36 * t3
     #1 * (t32 - t179 * t55 - t76 + t184 * t84) - t94) * t96 * t166 / 0.
     #720D3 - (0.90D2 * t36 * t31 * (t196 * t32 - t198 * t55 / 0.2D1 - t
     #64) - 0.180D3 * t59 * t90 * (-t32 + t196 * t55) - t149) * t166 / 0
     #.720D3
      t214 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t213)
      t216 = -0.1D1 + x4
      t219 = t131 * t216
      t222 = log(-0.4D1 * t14 * t219)
      t223 = -t216
      t224 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t223)
      t226 = t222 ** 2
      t227 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t223)
      t230 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t223)
      t240 = t90 * t227
      t248 = log(-0.4D1 * t154 * t13 * t219)
      t260 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, t2
     #23)
      t261 = -t227 + t260
      t268 = log(-0.4D1 * t77 * t219)
      t270 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, t2
     #23)
      t275 = log(0.4D1 * t77 * t131 * t216 * t78)
      t288 = (-0.90D2 * t36 * t31 * (-t222 * t224 + t226 * t227 / 0.2D1 
     #+ t230) + 0.180D3 * t59 * t90 * (t224 - t222 * t227) - t8 * t240) 
     #* t98 / 0.1440D4 + (0.90D2 * t36 * t31 * (-t224 + t248 * t227) + 0
     #.180D3 * t59 * t240) * t166 * t98 / 0.720D3 + t170 * t261 * t96 * 
     #t172 / 0.8D1 + (0.90D2 * t36 * t31 * (-t224 + t268 * t227 + t270 -
     # t275 * t260) - 0.180D3 * t59 * t90 * t261) * t96 * t98 / 0.1440D4
      t289 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t216, t2 * x4, 0.0D0,
     # t288)
      t292 = -0.1D1 + x1
      t294 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t295 = t292 ** 2
      t300 = log(0.4D1 * t17 * t153 * t295 * x4)
      t301 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t307 = t90 * t301
      t309 = 0.180D3 * t59 * t307
      t319 = t16 * t153 * t295
      t322 = log(0.4D1 * t77 * t319)
      t334 = log(0.4D1 * t14 * t319)
      t336 = t334 ** 2
      t339 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t353 = (0.90D2 * t36 * t31 * (-t294 + t300 * t301) + t309) * t166 
     #* t98 / 0.720D3 - t170 * t301 * t96 * t172 / 0.8D1 + (0.90D2 * t36
     # * t31 * (-t294 + t322 * t301) + t309) * t96 * t166 / 0.720D3 - (0
     #.90D2 * t36 * t31 * (-t334 * t294 + t336 * t301 / 0.2D1 + t339) - 
     #0.180D3 * t59 * t90 * (t294 - t334 * t301) + t8 * t307) * t166 / 0
     #.720D3
      t354 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t292, 0.0D0, 0.0D0,
     # t353)
      t356 = KAPPA2(x1, x2, 0.10D1, t223, z)
      t357 = s * t356
      t358 = t1 * x1
      t360 = t1 * t292
      t361 = t360 * t216
      t363 = t360 * x4
      t365 = t356 ** 2
      t368 = x1 * t292
      t372 = 0.1D1 / (-0.2D1 + t356)
      t373 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #23)
      t376 = t365 ** 2
      t381 = log(-0.4D1 * t194 * t295 * x4 * t216 * t376)
      t383 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #23)
      t389 = t59 * t27
      t390 = t31 * t372
      t403 = (-0.90D2 * t36 * t31 * (t372 * t373 - t381 * t372 * t383) +
     # 0.180D3 * t389 * t390 * t383) * t166 * t98 / 0.720D3 - t36 * t390
     # * t383 * t96 * t172 / 0.8D1
      t404 = FJET(XB1, XB2, s, t357 * t358, 0.0D0, t357 * t361, -t357 * 
     #t363, -s * t365 * t15 * t368 * x4, t403)
      t406 = KAPPA2(x1, x2, t75, 0.10D1, z)
      t407 = s * t406
      t408 = t358 * t78
      t410 = t358 * x3
      t413 = t406 ** 2
      t419 = 0.1D1 / (-0.2D1 + t406)
      t420 = t31 * t419
      t422 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, 0.10D
     #1)
      t427 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, 0.10D
     #1)
      t431 = t413 ** 2
      t436 = log(-0.4D1 * t176 * t14 * t16 * t295 * t78 * t431)
      t450 = -t36 * t420 * t422 * t96 * t172 / 0.8D1 + (-0.90D2 * t36 * 
     #t31 * (t419 * t427 - t436 * t419 * t422) + 0.180D3 * t389 * t420 *
     # t422) * t96 * t166 / 0.720D3
      t451 = FJET(XB1, XB2, s, -t408 * t407, t407 * t410, -t407 * t360, 
     #0.0D0, -s * t413 * t15 * t368 * x3, t450)
      t453 = KAPPA2(x1, x2, t75, t223, z)
      t454 = s * t453
      t459 = t453 ** 2
      t464 = cos(t9)
      t468 = Sqrt(x3 * t78 * x4 * t216)
      t475 = 0.1D1 / (-0.2D1 + t453)
      t478 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, t223)
      t483 = FJET(XB1, XB2, s, -t454 * t408, t454 * t410, t454 * t361, -
     #t454 * t363, s * t459 * t15 * t368 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t464 * t468), t36 * t31 * t475 * t478 * t96 * t172 / 0.8D
     #1)
      rrqg2qght5s5e0 = t214 * t213 + t289 * t288 + t354 * t353 + t404 * 
     #t403 + t451 * t450 + t483 * 0.3141592653589793D1 * t90 * t475 * t4
     #78 * t96 * t166 * t98 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s5em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t50 = t45 * t23
      t51 = 0.1D1 - x3
      t52 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.1
     #0D1)
      t53 = -t52 + t41
      t54 = 0.1D1 / x3
      t55 = t53 * t54
      t56 = 0.1D1 / x4
      t60 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.1
     #0D1)
      t61 = x3 * t7
      t62 = t9 * t12
      t63 = -t51
      t67 = log(-0.4D1 * t61 * t62 * t63)
      t71 = log(0.4D1 * t61 * t62)
      t77 = t19 * t23
      t84 = t12 * x4
      t87 = log(0.4D1 * t10 * t84)
      t95 = 0.180D3 * t3 * t77 * t41
      t99 = 0.1D1 / x1
      t103 = x1 ** 2
      t107 = log(0.4D1 * t103 * t7 * t62)
      t120 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t55 * t56 / 0.16
     #D2 + (0.90D2 * t45 * t23 * (-t60 + t67 * t52 + t24 - t71 * t41) - 
     #0.180D3 * t3 * t77 * t53) * t54 / 0.1440D4 + (0.90D2 * t45 * t23 *
     # (t24 - t87 * t41) - t95) * t56 / 0.1440D4 + t50 * t55 * t99 / 0.8
     #D1 - (0.90D2 * t45 * t23 * (-t24 + t107 * t41) + t95) * t99 / 0.72
     #0D3 + t50 * t41 * t99 * t56 / 0.8D1
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t120)
      t123 = -0.1D1 + x4
      t126 = -t123
      t127 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t126)
      t131 = log(-0.4D1 * t10 * t84 * t123)
      t132 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t126)
      t148 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, t1
     #26)
      t154 = (-0.90D2 * t45 * t23 * (t127 - t131 * t132) + 0.180D3 * t3 
     #* t77 * t132) * t56 / 0.1440D4 - t50 * t132 * t99 * t56 / 0.8D1 + 
     #t50 * (-t132 + t148) * t54 * t56 / 0.16D2
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t123, t2 * x4, 0.0D0,
     # t154)
      t158 = -0.1D1 + x1
      t160 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t165 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t167 = t158 ** 2
      t171 = log(0.4D1 * t10 * t12 * t103 * t167)
      t187 = -t50 * t160 * t54 * t99 / 0.8D1 - (0.90D2 * t45 * t23 * (t1
     #65 - t171 * t160) - 0.180D3 * t3 * t77 * t160) * t99 / 0.720D3 - t
     #50 * t160 * t99 * t56 / 0.8D1
      t188 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t158, 0.0D0, 0.0D0,
     # t187)
      t190 = KAPPA2(x1, x2, 0.10D1, t126, z)
      t191 = s * t190
      t192 = t1 * x1
      t194 = t1 * t158
      t199 = t190 ** 2
      t202 = x1 * t158
      t207 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t1
     #26)
      t210 = 0.1D1 / (-0.2D1 + t190) * t207 * t99 * t56
      t213 = FJET(XB1, XB2, s, t191 * t192, 0.0D0, t191 * t194 * t123, -
     #t191 * t194 * x4, -s * t199 * t11 * t202 * x4, -t50 * t210 / 0.8D1
     #)
      t218 = KAPPA2(x1, x2, t51, 0.10D1, z)
      t219 = s * t218
      t225 = t218 ** 2
      t232 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t51, 0.10D
     #1)
      t235 = 0.1D1 / (-0.2D1 + t218) * t232 * t54 * t99
      t238 = FJET(XB1, XB2, s, -t219 * t192 * t63, t219 * t192 * x3, -t2
     #19 * t194, 0.0D0, -s * t225 * t11 * t202 * x3, -t50 * t235 / 0.8D1
     #)
      rrqg2qght5s5em1 = t121 * t120 + t155 * t154 + t188 * t187 - t213 *
     # 0.3141592653589793D1 * t77 * t210 / 0.8D1 - t238 * 0.314159265358
     #9793D1 * t77 * t235 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s5em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t15 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.10D1)
      t22 = 0.1D1 / x1
      t26 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (-t15 + t8) / x3 / 0.16D2
     # + t4 * t9 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * t
     #3 * t9 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t52 = -0.1D1 + x4
      t56 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #-t52)
      t58 = t7 * t56 * t10
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t52, t2 * x4, 0.0D0, -
     #t4 * t58 / 0.16D2)
      t69 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.1
     #0D1)
      t71 = t7 * t69 * t22
      t74 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, -t4 * t71 / 0.8D1)
      rrqg2qght5s5em2 = t50 * t49 - t61 * 0.3141592653589793D1 * t3 * t5
     #8 / 0.16D2 - t74 * 0.3141592653589793D1 * t3 * t71 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s5em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s5em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s5em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s5em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s6e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t105 = t35 * t39
      t111 = t105 * t86
      t114 = 0.1D1 / x1
      t116 = 0.1D1 / x4
      t119 = t91 * t92
      t121 = log(0.4D1 * t119)
      t126 = t121 ** 2
      t137 = t10 * t111
      t148 = x3 * t90
      t149 = t148 * t13
      t152 = log(0.4D1 * t149 * t93)
      t154 = 0.1D1 - x3
      t155 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t156 = -t154
      t157 = t156 * x4
      t161 = log(-0.4D1 * t149 * t92 * t157)
      t162 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t168 = t162 - t86
      t169 = t105 * t168
      t173 = 0.1D1 / x3
      t175 = t114 * t116
      t178 = t148 * t19
      t180 = log(0.4D1 * t178)
      t182 = t180 ** 2
      t185 = t92 * t156
      t188 = log(-0.4D1 * t149 * t185)
      t190 = t188 ** 2
      t193 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t209 = t18 * x4
      t212 = log(0.4D1 * t16 * t209)
      t217 = t212 ** 2
      t238 = x3 * t13
      t239 = t238 * t15
      t244 = log(-0.4D1 * t239 * t18 * t156 * x4)
      t246 = t244 ** 2
      t251 = log(0.4D1 * t238 * t93)
      t253 = t251 ** 2
      t267 = -t105 * t168
      t275 = log(-0.4D1 * t238 * t185)
      t279 = log(0.4D1 * t238 * t92)
      t284 = t279 ** 2
      t291 = t275 ** 2
      t294 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t317 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 - (0.90D2 * t63 * t39 * (t96 * t40 - t98 * t86 / 0.2D1 
     #- t50) - 0.180D3 * t54 * t105 * (-t40 + t96 * t86) - t44 * t111) *
     # t114 * t116 / 0.720D3 - (t44 * t105 * (-t40 + t121 * t86) + 0.90D
     #2 * t63 * t39 * (-t126 * t40 / 0.2D1 - t59 + t126 * t121 * t86 / 0
     #.6D1 + t121 * t50) - t137 - 0.180D3 * t54 * t105 * (t121 * t40 - t
     #126 * t86 / 0.2D1 - t50)) * t114 / 0.720D3 - (0.90D2 * t63 * t39 *
     # (-t40 + t152 * t86 + t155 - t161 * t162) - 0.180D3 * t54 * t169) 
     #* t173 * t175 / 0.720D3 - (0.90D2 * t63 * t39 * (t180 * t40 - t182
     # * t86 / 0.2D1 - t50 - t188 * t155 + t190 * t162 / 0.2D1 + t193) -
     # 0.180D3 * t54 * t105 * (-t40 + t180 * t86 + t155 - t188 * t162) +
     # t44 * t169) * t173 * t114 / 0.720D3 + (t44 * t105 * (t40 - t212 *
     # t86) + 0.90D2 * t63 * t39 * (t217 * t40 / 0.2D1 + t59 - t217 * t2
     #12 * t86 / 0.6D1 - t212 * t50) + t137 - 0.180D3 * t54 * t105 * (-t
     #212 * t40 + t217 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 + (0.90D2
     # * t63 * t39 * (t244 * t155 - t246 * t162 / 0.2D1 - t193 - t251 * 
     #t40 + t253 * t86 / 0.2D1 + t50) - 0.180D3 * t54 * t105 * (-t155 + 
     #t244 * t162 + t40 - t251 * t86) + t44 * t267) * t173 * t116 / 0.14
     #40D4 + (t44 * t105 * (-t155 + t275 * t162 + t40 - t279 * t86) + 0.
     #90D2 * t63 * t39 * (t284 * t40 / 0.2D1 + t59 - t284 * t279 * t86 /
     # 0.6D1 - t279 * t50 - t291 * t155 / 0.2D1 - t294 + t291 * t275 * t
     #162 / 0.6D1 + t275 * t193) + t10 * t267 - 0.180D3 * t54 * t105 * (
     #t275 * t155 - t291 * t162 / 0.2D1 - t193 - t279 * t40 + t284 * t86
     # / 0.2D1 + t50)) * t173 / 0.1440D4
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t317)
      t321 = -0.1D1 + x4
      t323 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t324 = t209 * t321
      t327 = log(-0.4D1 * t16 * t324)
      t328 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t333 = t327 ** 2
      t336 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t340 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t346 = t105 * t328
      t359 = t209 * t321 * t156
      t362 = log(0.4D1 * t239 * t359)
      t363 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, x
     #4)
      t365 = t362 ** 2
      t366 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, x
     #4)
      t369 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, x
     #4)
      t372 = log(-0.4D1 * t239 * t324)
      t374 = t372 ** 2
      t387 = -t328 + t366
      t394 = t91 * t15
      t397 = log(-0.4D1 * t394 * t324)
      t399 = t397 ** 2
      t416 = t148 * t16
      t419 = log(0.4D1 * t416 * t359)
      t421 = x4 * t321
      t425 = log(-0.4D1 * t149 * t92 * t421)
      t439 = (t44 * t105 * (-t323 + t327 * t328) + 0.90D2 * t63 * t39 * 
     #(-t333 * t323 / 0.2D1 - t336 + t333 * t327 * t328 / 0.6D1 + t327 *
     # t340) - t10 * t346 - 0.180D3 * t54 * t105 * (t327 * t323 - t333 *
     # t328 / 0.2D1 - t340)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #-t362 * t363 + t365 * t366 / 0.2D1 + t369 + t372 * t323 - t374 * t
     #328 / 0.2D1 - t340) - 0.180D3 * t54 * t105 * (t363 - t362 * t366 -
     # t323 + t372 * t328) + t44 * t105 * t387) * t173 * t116 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t397 * t323 + t399 * t328 / 0.2D1 + t34
     #0) - 0.180D3 * t54 * t105 * (t323 - t397 * t328) + t44 * t346) * t
     #114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (-t363 + t419 * t366 
     #+ t323 - t425 * t328) + 0.180D3 * t54 * t105 * t387) * t173 * t175
     # / 0.720D3
      t440 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t321, 0.0D0,
     # t439)
      t442 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t443 = s * t442
      t444 = t1 * x1
      t446 = -0.1D1 + x1
      t447 = t1 * t446
      t449 = t442 ** 2
      t454 = t446 ** 2
      t455 = t18 * t454
      t456 = t449 ** 2
      t458 = t455 * x4 * t456
      t461 = log(0.4D1 * t394 * t458)
      t463 = 0.1D1 / (-0.2D1 + t442)
      t464 = t461 * t463
      t465 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t467 = t461 ** 2
      t469 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t472 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t473 = t463 * t472
      t478 = t463 * t465
      t484 = t44 * t35
      t486 = t39 * t463 * t469
      t487 = t484 * t486
      t494 = log(0.4D1 * t394 * t455 * t456)
      t495 = t494 * t463
      t500 = t494 ** 2
      t501 = t500 * t463
      t504 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t528 = log(0.4D1 * t416 * t458)
      t535 = t54 * t35
      t545 = log(0.4D1 * t149 * t92 * t454 * t456)
      t546 = t545 * t463
      t548 = t545 ** 2
      t565 = -(0.90D2 * t63 * t39 * (t464 * t465 - t467 * t463 * t469 / 
     #0.2D1 - t473) - 0.180D3 * t54 * t105 * (-t478 + t464 * t469) - t48
     #7) * t114 * t116 / 0.720D3 - (-t44 * t105 * (t478 - t495 * t469) -
     # 0.90D2 * t63 * t39 * (t501 * t465 / 0.2D1 + t463 * t504 - t500 * 
     #t494 * t463 * t469 / 0.6D1 - t495 * t472) - t10 * t35 * t486 + 0.1
     #80D3 * t54 * t105 * (-t495 * t465 + t501 * t469 / 0.2D1 + t473)) *
     # t114 / 0.720D3 - (0.90D2 * t63 * t39 * (-t478 + t528 * t463 * t46
     #9) + 0.180D3 * t535 * t486) * t173 * t175 / 0.720D3 - (0.90D2 * t6
     #3 * t39 * (t546 * t465 - t548 * t463 * t469 / 0.2D1 - t473) - 0.18
     #0D3 * t54 * t105 * (-t478 + t546 * t469) - t487) * t173 * t114 / 0
     #.720D3
      t566 = FJET(XB1, XB2, s, t443 * t444, 0.0D0, 0.0D0, -t443 * t447, 
     #-s * t449 * t17 * x1 * t446, t565)
      t568 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t569 = s * t568
      t571 = t447 * x4
      t573 = t447 * t321
      t575 = t568 ** 2
      t578 = x1 * t446
      t581 = t575 ** 2
      t586 = log(-0.4D1 * t119 * t421 * t581 * t454)
      t588 = 0.1D1 / (-0.2D1 + t568)
      t589 = t586 * t588
      t590 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t592 = t586 ** 2
      t594 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t597 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t603 = t588 * t590
      t610 = t39 * t588 * t594
      t620 = log(-0.4D1 * t416 * t209 * t321 * t454 * t581)
      t633 = -(0.90D2 * t63 * t39 * (-t589 * t590 + t592 * t588 * t594 /
     # 0.2D1 + t588 * t597) - 0.180D3 * t54 * t105 * (t603 - t589 * t594
     #) + t484 * t610) * t114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t603 - t620 * t588 * t594) - 0.180D3 * t535 * t610) * t173 * t175 
     #/ 0.720D3
      t634 = FJET(XB1, XB2, s, t569 * t444, 0.0D0, -t569 * t571, t569 * 
     #t573, s * t575 * t17 * t578 * t321, t633)
      t636 = KAPPA2(x1, x2, t154, 0.0D0, z)
      t637 = s * t636
      t638 = t444 * t156
      t640 = t444 * x3
      t643 = t636 ** 2
      t649 = 0.1D1 / (-0.2D1 + t636)
      t650 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, 0.0D
     #0)
      t651 = t649 * t650
      t652 = t643 ** 2
      t657 = log(-0.4D1 * t416 * t455 * t157 * t652)
      t659 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, 0.0D
     #0)
      t666 = t39 * t649 * t659
      t676 = log(-0.4D1 * t416 * t455 * t156 * t652)
      t677 = t676 * t649
      t679 = t676 ** 2
      t683 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, 0.0D
     #0)
      t699 = -(0.90D2 * t63 * t39 * (t651 - t657 * t649 * t659) - 0.180D
     #3 * t535 * t666) * t173 * t175 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t677 * t650 + t679 * t649 * t659 / 0.2D1 + t649 * t683) - 0.180D3
     # * t54 * t105 * (t651 - t677 * t659) + t484 * t666) * t173 * t114 
     #/ 0.720D3
      t700 = FJET(XB1, XB2, s, -t637 * t638, t637 * t640, 0.0D0, -t637 *
     # t447, s * t643 * t17 * t578 * t156, t699)
      t702 = KAPPA2(x1, x2, t154, x4, z)
      t703 = s * t702
      t708 = t702 ** 2
      t713 = cos(t11)
      t716 = Sqrt(x3 * t156 * t421)
      t723 = 0.1D1 / (-0.2D1 + t702)
      t724 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, x4)
      t727 = t708 ** 2
      t732 = log(0.4D1 * t178 * t421 * t454 * t156 * t727)
      t734 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, x4)
      t744 = -0.90D2 * t63 * t39 * (t723 * t724 - t732 * t723 * t734) + 
     #0.180D3 * t535 * t39 * t723 * t734
      t748 = FJET(XB1, XB2, s, -t703 * t638, t703 * t640, -t703 * t571, 
     #t703 * t573, s * t708 * t17 * t578 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t713 * t716), -t744 * t173 * t175 / 0.720D3)
      rrqg2qght5s6e1 = t318 * t317 + t440 * t439 + t566 * t565 + t634 * 
     #t633 + t700 * t699 - t748 * t744 * t173 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrqg2qght5s6e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t68 = 0.1D1 - x3
      t69 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t68, 0.0
     #D0)
      t70 = x3 * t11
      t71 = t70 * t13
      t72 = -t68
      t77 = log(-0.4D1 * t71 * t16 * t72 * x4)
      t78 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t68, 0.0
     #D0)
      t80 = t13 * t16
      t81 = t80 * x4
      t84 = log(0.4D1 * t70 * t81)
      t90 = t27 * t31
      t91 = -t78 + t55
      t92 = t90 * t91
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t101 = t80 * t72
      t104 = log(-0.4D1 * t70 * t101)
      t106 = t104 ** 2
      t109 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t68, 0.
     #0D0)
      t112 = log(0.4D1 * t70 * t80)
      t114 = t112 ** 2
      t131 = t16 * x4
      t134 = log(0.4D1 * t14 * t131)
      t136 = t134 ** 2
      t148 = t90 * t55
      t149 = t8 * t148
      t153 = x1 ** 2
      t154 = t153 * t11
      t157 = log(0.4D1 * t154 * t81)
      t166 = 0.1D1 / x1
      t170 = t36 * t31
      t171 = -t91
      t173 = t166 * t98
      t177 = x3 * t153
      t180 = log(0.4D1 * t177 * t17)
      t182 = t177 * t11
      t185 = log(-0.4D1 * t182 * t101)
      t198 = t154 * t80
      t200 = log(0.4D1 * t198)
      t202 = t200 ** 2
      t217 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (-t69 + t77 
     #* t78 + t32 - t84 * t55) - 0.180D3 * t59 * t92) * t96 * t98 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (t104 * t69 - t106 * t78 / 0.2D1 - t1
     #09 - t112 * t32 + t114 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t90 
     #* (-t69 + t104 * t78 + t32 - t112 * t55) + t8 * t92) * t96 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (-t134 * t32 + t136 * t55 / 0.2D1 + t6
     #4) - 0.180D3 * t59 * t90 * (t32 - t134 * t55) + t149) * t98 / 0.14
     #40D4 - (0.90D2 * t36 * t31 * (-t32 + t157 * t55) + 0.180D3 * t59 *
     # t148) * t166 * t98 / 0.720D3 - t170 * t171 * t96 * t173 / 0.8D1 -
     # (0.90D2 * t36 * t31 * (-t32 + t180 * t55 + t69 - t185 * t78) - 0.
     #180D3 * t59 * t90 * t171) * t96 * t166 / 0.720D3 - (0.90D2 * t36 *
     # t31 * (t200 * t32 - t202 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t
     #90 * (-t32 + t200 * t55) - t149) * t166 / 0.720D3
      t218 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t217)
      t221 = -0.1D1 + x4
      t223 = t131 * t221
      t226 = log(-0.4D1 * t14 * t223)
      t227 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t229 = t226 ** 2
      t230 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t233 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t243 = t90 * t230
      t248 = t154 * t13
      t251 = log(-0.4D1 * t248 * t223)
      t263 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t68, x4
     #)
      t264 = t230 - t263
      t269 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t68, x4
     #)
      t274 = log(0.4D1 * t71 * t131 * t221 * t72)
      t278 = log(-0.4D1 * t71 * t223)
      t292 = (0.90D2 * t36 * t31 * (t226 * t227 - t229 * t230 / 0.2D1 - 
     #t233) - 0.180D3 * t59 * t90 * (-t227 + t226 * t230) - t8 * t243) *
     # t98 / 0.1440D4 - (0.90D2 * t36 * t31 * (t227 - t251 * t230) - 0.1
     #80D3 * t59 * t243) * t166 * t98 / 0.720D3 - t170 * t264 * t96 * t1
     #73 / 0.8D1 + (0.90D2 * t36 * t31 * (t269 - t274 * t263 - t227 + t2
     #78 * t230) + 0.180D3 * t59 * t90 * t264) * t96 * t98 / 0.1440D4
      t293 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t221, 0.0D0,
     # t292)
      t295 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t296 = s * t295
      t297 = t1 * x1
      t299 = -0.1D1 + x1
      t300 = t1 * t299
      t302 = t295 ** 2
      t308 = 0.1D1 / (-0.2D1 + t295)
      t309 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t310 = t308 * t309
      t311 = t299 ** 2
      t312 = t16 * t311
      t313 = t302 ** 2
      t318 = log(0.4D1 * t248 * t312 * x4 * t313)
      t320 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t326 = t59 * t27
      t327 = t31 * t308
      t328 = t327 * t320
      t330 = 0.180D3 * t326 * t328
      t344 = log(0.4D1 * t182 * t80 * t311 * t313)
      t358 = log(0.4D1 * t248 * t312 * t313)
      t359 = t358 * t308
      t361 = t358 ** 2
      t365 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t381 = -(0.90D2 * t36 * t31 * (-t310 + t318 * t308 * t320) + t330)
     # * t166 * t98 / 0.720D3 + t36 * t327 * t320 * t96 * t173 / 0.8D1 -
     # (0.90D2 * t36 * t31 * (-t310 + t344 * t308 * t320) + t330) * t96 
     #* t166 / 0.720D3 - (-0.90D2 * t36 * t31 * (-t359 * t309 + t361 * t
     #308 * t320 / 0.2D1 + t308 * t365) + 0.180D3 * t59 * t90 * (t310 - 
     #t359 * t320) - t8 * t27 * t328) * t166 / 0.720D3
      t382 = FJET(XB1, XB2, s, t296 * t297, 0.0D0, 0.0D0, -t296 * t300, 
     #-s * t302 * t15 * x1 * t299, t381)
      t384 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t385 = s * t384
      t387 = t300 * x4
      t389 = t300 * t221
      t391 = t384 ** 2
      t394 = x1 * t299
      t398 = 0.1D1 / (-0.2D1 + t384)
      t399 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t401 = x4 * t221
      t402 = t391 ** 2
      t407 = log(-0.4D1 * t198 * t401 * t402 * t311)
      t409 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t415 = t31 * t398
      t428 = -(0.90D2 * t36 * t31 * (t398 * t399 - t407 * t398 * t409) -
     # 0.180D3 * t326 * t415 * t409) * t166 * t98 / 0.720D3 - t36 * t415
     # * t409 * t96 * t173 / 0.8D1
      t429 = FJET(XB1, XB2, s, t385 * t297, 0.0D0, -t385 * t387, t385 * 
     #t389, s * t391 * t15 * t394 * t221, t428)
      t431 = KAPPA2(x1, x2, t68, 0.0D0, z)
      t432 = s * t431
      t433 = t297 * t72
      t435 = t297 * x3
      t438 = t431 ** 2
      t444 = 0.1D1 / (-0.2D1 + t431)
      t445 = t31 * t444
      t447 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t68, 0.0D0
     #)
      t452 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t68, 0.0D0
     #)
      t455 = t438 ** 2
      t460 = log(-0.4D1 * t177 * t14 * t312 * t72 * t455)
      t474 = -t36 * t445 * t447 * t96 * t173 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t444 * t452 - t460 * t444 * t447) - 0.180D3 * t326 * t445 * 
     #t447) * t96 * t166 / 0.720D3
      t475 = FJET(XB1, XB2, s, -t432 * t433, t432 * t435, 0.0D0, -t432 *
     # t300, s * t438 * t15 * t394 * t72, t474)
      t477 = KAPPA2(x1, x2, t68, x4, z)
      t478 = s * t477
      t483 = t477 ** 2
      t488 = cos(t9)
      t491 = Sqrt(x3 * t72 * t401)
      t498 = 0.1D1 / (-0.2D1 + t477)
      t501 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t68, x4)
      t506 = FJET(XB1, XB2, s, -t478 * t433, t478 * t435, -t478 * t387, 
     #t478 * t389, s * t483 * t15 * t394 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t488 * t491), t36 * t31 * t498 * t501 * t96 * t17
     #3 / 0.8D1)
      rrqg2qght5s6e0 = t218 * t217 + t293 * t292 + t382 * t381 + t429 * 
     #t428 + t475 * t474 + t506 * 0.3141592653589793D1 * t90 * t498 * t5
     #01 * t96 * t166 * t98 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s6em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t50 = t45 * t23
      t51 = 0.1D1 - x3
      t52 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.0
     #D0)
      t53 = -t52 + t41
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.0
     #D0)
      t61 = x3 * t7
      t62 = t9 * t12
      t63 = -t51
      t67 = log(-0.4D1 * t61 * t62 * t63)
      t71 = log(0.4D1 * t61 * t62)
      t77 = t19 * t23
      t84 = t12 * x4
      t87 = log(0.4D1 * t10 * t84)
      t95 = 0.180D3 * t3 * t77 * t41
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t7
      t109 = log(0.4D1 * t106 * t62)
      t122 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t53 * t54 * t56 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (-t60 + t67 * t52 + t24 - t71 * t
     #41) - 0.180D3 * t3 * t77 * t53) * t54 / 0.1440D4 + (0.90D2 * t45 *
     # t23 * (t24 - t87 * t41) - t95) * t56 / 0.1440D4 + t50 * t53 * t54
     # * t101 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t109 * t41) + t95)
     # * t101 / 0.720D3 + t50 * t41 * t101 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t122)
      t126 = -0.1D1 + x4
      t128 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t132 = log(-0.4D1 * t10 * t84 * t126)
      t133 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t149 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, x4
     #)
      t155 = (0.90D2 * t45 * t23 * (-t128 + t132 * t133) + 0.180D3 * t3 
     #* t77 * t133) * t56 / 0.1440D4 - t50 * t133 * t101 * t56 / 0.8D1 +
     # t50 * (-t133 + t149) * t54 * t56 / 0.16D2
      t156 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t126, 0.0D0,
     # t155)
      t158 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t159 = s * t158
      t160 = t1 * x1
      t162 = -0.1D1 + x1
      t163 = t1 * t162
      t165 = t158 ** 2
      t171 = 0.1D1 / (-0.2D1 + t158)
      t172 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t173 = t171 * t172
      t174 = t54 * t101
      t178 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t181 = t162 ** 2
      t183 = t165 ** 2
      t187 = log(0.4D1 * t106 * t9 * t12 * t181 * t183)
      t202 = t101 * t56
      t206 = t50 * t173 * t174 / 0.8D1 - (-0.90D2 * t45 * t23 * (t171 * 
     #t178 - t187 * t171 * t172) + 0.180D3 * t3 * t19 * t23 * t171 * t17
     #2) * t101 / 0.720D3 + t50 * t173 * t202 / 0.8D1
      t207 = FJET(XB1, XB2, s, t159 * t160, 0.0D0, 0.0D0, -t159 * t163, 
     #-s * t165 * t11 * x1 * t162, t206)
      t209 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t210 = s * t209
      t216 = t209 ** 2
      t219 = x1 * t162
      t224 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t226 = 0.1D1 / (-0.2D1 + t209) * t224 * t202
      t229 = FJET(XB1, XB2, s, t210 * t160, 0.0D0, -t210 * t163 * x4, t2
     #10 * t163 * t126, s * t216 * t11 * t219 * t126, -t50 * t226 / 0.8D
     #1)
      t234 = KAPPA2(x1, x2, t51, 0.0D0, z)
      t235 = s * t234
      t241 = t234 ** 2
      t248 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t51, 0.0D0
     #)
      t250 = 0.1D1 / (-0.2D1 + t234) * t248 * t174
      t253 = FJET(XB1, XB2, s, -t235 * t160 * t63, t235 * t160 * x3, 0.0
     #D0, -t235 * t163, s * t241 * t11 * t219 * t63, -t50 * t250 / 0.8D1
     #)
      rrqg2qght5s6em1 = t123 * t122 + t156 * t155 + t207 * t206 - t229 *
     # 0.3141592653589793D1 * t77 * t226 / 0.8D1 - t253 * 0.314159265358
     #9793D1 * t77 * t250 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s6em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t15 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.0D0)
      t22 = 0.1D1 / x1
      t26 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (-t15 + t8) / x3 / 0.16D2
     # + t4 * t9 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * t
     #3 * t9 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t57 = t7 * t55 * t10
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, -t4 * t57 / 0.16D2)
      t65 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t66 = s * t65
      t69 = -0.1D1 + x1
      t72 = t65 ** 2
      t79 = 0.1D1 / (-0.2D1 + t65)
      t80 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.0
     #D0)
      t85 = FJET(XB1, XB2, s, t66 * t1 * x1, 0.0D0, 0.0D0, -t66 * t1 * t
     #69, -s * t72 * t38 * x1 * t69, t4 * t7 * t79 * t80 * t22 / 0.8D1)
      rrqg2qght5s6em2 = t50 * t49 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t85 * 0.3141592653589793D1 * t3 * t7 * t79 * t80 * t2
     #2 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s6em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s6em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s6em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s6em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s7e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t105 = t35 * t39
      t111 = t105 * t86
      t114 = 0.1D1 / x1
      t116 = 0.1D1 / x4
      t119 = t91 * t92
      t121 = log(0.4D1 * t119)
      t126 = t121 ** 2
      t137 = t10 * t111
      t148 = x3 * t90
      t149 = t148 * t13
      t152 = log(0.4D1 * t149 * t93)
      t154 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t155 = -0.1D1 + x3
      t156 = t155 * x4
      t160 = log(-0.4D1 * t149 * t92 * t156)
      t161 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t167 = t161 - t86
      t168 = t105 * t167
      t172 = 0.1D1 / x3
      t174 = t114 * t116
      t177 = t148 * t19
      t179 = log(0.4D1 * t177)
      t181 = t179 ** 2
      t184 = t92 * t155
      t187 = log(-0.4D1 * t149 * t184)
      t189 = t187 ** 2
      t192 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t208 = t18 * x4
      t211 = log(0.4D1 * t16 * t208)
      t216 = t211 ** 2
      t237 = x3 * t13
      t240 = log(0.4D1 * t237 * t93)
      t242 = t240 ** 2
      t245 = t237 * t15
      t250 = log(-0.4D1 * t245 * t18 * t155 * x4)
      t252 = t250 ** 2
      t266 = -t105 * t167
      t272 = t237 * t92
      t274 = log(0.4D1 * t272)
      t278 = log(-0.4D1 * t237 * t184)
      t283 = t278 ** 2
      t286 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t291 = t274 ** 2
      t316 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 - (0.90D2 * t63 * t39 * (t96 * t40 - t98 * t86 / 0.2D1 
     #- t50) - 0.180D3 * t54 * t105 * (-t40 + t96 * t86) - t44 * t111) *
     # t114 * t116 / 0.720D3 - (t44 * t105 * (-t40 + t121 * t86) + 0.90D
     #2 * t63 * t39 * (-t126 * t40 / 0.2D1 - t59 + t126 * t121 * t86 / 0
     #.6D1 + t121 * t50) - t137 - 0.180D3 * t54 * t105 * (t121 * t40 - t
     #126 * t86 / 0.2D1 - t50)) * t114 / 0.720D3 - (0.90D2 * t63 * t39 *
     # (-t40 + t152 * t86 + t154 - t160 * t161) - 0.180D3 * t54 * t168) 
     #* t172 * t174 / 0.720D3 - (0.90D2 * t63 * t39 * (t179 * t40 - t181
     # * t86 / 0.2D1 - t50 - t187 * t154 + t189 * t161 / 0.2D1 + t192) -
     # 0.180D3 * t54 * t105 * (-t40 + t179 * t86 + t154 - t187 * t161) +
     # t44 * t168) * t172 * t114 / 0.720D3 + (t44 * t105 * (t40 - t211 *
     # t86) + 0.90D2 * t63 * t39 * (t216 * t40 / 0.2D1 + t59 - t216 * t2
     #11 * t86 / 0.6D1 - t211 * t50) + t137 - 0.180D3 * t54 * t105 * (-t
     #211 * t40 + t216 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 + (0.90D2
     # * t63 * t39 * (-t240 * t40 + t242 * t86 / 0.2D1 + t50 + t250 * t1
     #54 - t252 * t161 / 0.2D1 - t192) - 0.180D3 * t54 * t105 * (t40 - t
     #240 * t86 - t154 + t250 * t161) + t44 * t266) * t172 * t116 / 0.14
     #40D4 + (t44 * t105 * (t40 - t274 * t86 - t154 + t278 * t161) + 0.9
     #0D2 * t63 * t39 * (-t283 * t154 / 0.2D1 - t286 + t283 * t278 * t16
     #1 / 0.6D1 + t278 * t192 + t291 * t40 / 0.2D1 + t59 - t291 * t274 *
     # t86 / 0.6D1 - t274 * t50) + t10 * t266 - 0.180D3 * t54 * t105 * (
     #-t274 * t40 + t291 * t86 / 0.2D1 + t50 + t278 * t154 - t283 * t161
     # / 0.2D1 - t192)) * t172 / 0.1440D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t316)
      t319 = -0.1D1 + x4
      t322 = -t319
      t323 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t324 = t208 * t319
      t327 = log(-0.4D1 * t16 * t324)
      t328 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t333 = t327 ** 2
      t336 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t340 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t346 = t105 * t328
      t359 = t208 * t319 * t155
      t362 = log(0.4D1 * t245 * t359)
      t363 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t32
     #2)
      t365 = t362 ** 2
      t366 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t32
     #2)
      t369 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t32
     #2)
      t372 = log(-0.4D1 * t245 * t324)
      t374 = t372 ** 2
      t387 = -t328 + t366
      t397 = log(-0.4D1 * t91 * t15 * t324)
      t399 = t397 ** 2
      t419 = log(0.4D1 * t148 * t16 * t359)
      t421 = x4 * t319
      t425 = log(-0.4D1 * t149 * t92 * t421)
      t439 = (-t44 * t105 * (t323 - t327 * t328) - 0.90D2 * t63 * t39 * 
     #(t333 * t323 / 0.2D1 + t336 - t333 * t327 * t328 / 0.6D1 - t327 * 
     #t340) - t10 * t346 + 0.180D3 * t54 * t105 * (-t327 * t323 + t333 *
     # t328 / 0.2D1 + t340)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #-t362 * t363 + t365 * t366 / 0.2D1 + t369 + t372 * t323 - t374 * t
     #328 / 0.2D1 - t340) - 0.180D3 * t54 * t105 * (t363 - t362 * t366 -
     # t323 + t372 * t328) + t44 * t105 * t387) * t172 * t116 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t397 * t323 + t399 * t328 / 0.2D1 + t34
     #0) - 0.180D3 * t54 * t105 * (t323 - t397 * t328) + t44 * t346) * t
     #114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (-t363 + t419 * t366 
     #+ t323 - t425 * t328) + 0.180D3 * t54 * t105 * t387) * t172 * t174
     # / 0.720D3
      t440 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t319, t2 * x4, 0.0D0,
     # t439)
      t442 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t443 = s * t442
      t444 = t1 * x1
      t446 = -0.1D1 + x1
      t447 = t1 * t446
      t449 = t442 ** 2
      t454 = t446 ** 2
      t455 = t90 * t454
      t456 = t449 ** 2
      t458 = t455 * x4 * t456
      t461 = log(0.4D1 * t19 * t458)
      t463 = 0.1D1 / (-0.2D1 + t442)
      t464 = t461 * t463
      t465 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t467 = t461 ** 2
      t469 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t472 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t473 = t463 * t472
      t478 = t463 * t465
      t484 = t44 * t35
      t486 = t39 * t463 * t469
      t487 = t484 * t486
      t494 = log(0.4D1 * t19 * t455 * t456)
      t495 = t494 * t463
      t500 = t494 ** 2
      t501 = t500 * t463
      t504 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t528 = log(0.4D1 * t272 * t458)
      t535 = t54 * t35
      t546 = log(0.4D1 * t245 * t18 * t90 * t454 * t456)
      t547 = t546 * t463
      t549 = t546 ** 2
      t566 = -(0.90D2 * t63 * t39 * (t464 * t465 - t467 * t463 * t469 / 
     #0.2D1 - t473) - 0.180D3 * t54 * t105 * (-t478 + t464 * t469) - t48
     #7) * t114 * t116 / 0.720D3 - (-t44 * t105 * (t478 - t495 * t469) -
     # 0.90D2 * t63 * t39 * (t501 * t465 / 0.2D1 + t463 * t504 - t500 * 
     #t494 * t463 * t469 / 0.6D1 - t495 * t472) - t10 * t35 * t486 + 0.1
     #80D3 * t54 * t105 * (-t495 * t465 + t501 * t469 / 0.2D1 + t473)) *
     # t114 / 0.720D3 - (0.90D2 * t63 * t39 * (-t478 + t528 * t463 * t46
     #9) + 0.180D3 * t535 * t486) * t172 * t174 / 0.720D3 - (0.90D2 * t6
     #3 * t39 * (t547 * t465 - t549 * t463 * t469 / 0.2D1 - t473) - 0.18
     #0D3 * t54 * t105 * (-t478 + t547 * t469) - t487) * t172 * t114 / 0
     #.720D3
      t567 = FJET(XB1, XB2, s, 0.0D0, t443 * t444, -t443 * t447, 0.0D0, 
     #-s * t449 * t17 * x1 * t446, t566)
      t569 = KAPPA2(x1, x2, 0.0D0, t322, z)
      t570 = s * t569
      t572 = t447 * t319
      t574 = t447 * x4
      t576 = t569 ** 2
      t579 = x1 * t446
      t583 = t576 ** 2
      t588 = log(-0.4D1 * t119 * t454 * x4 * t319 * t583)
      t590 = 0.1D1 / (-0.2D1 + t569)
      t591 = t588 * t590
      t592 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t32
     #2)
      t594 = t588 ** 2
      t596 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t32
     #2)
      t599 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t32
     #2)
      t605 = t590 * t592
      t612 = t39 * t590 * t596
      t621 = log(-0.4D1 * t272 * t455 * t421 * t583)
      t634 = -(0.90D2 * t63 * t39 * (-t591 * t592 + t594 * t590 * t596 /
     # 0.2D1 + t590 * t599) - 0.180D3 * t54 * t105 * (t605 - t591 * t596
     #) + t484 * t612) * t114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t605 - t621 * t590 * t596) - 0.180D3 * t535 * t612) * t172 * t174 
     #/ 0.720D3
      t635 = FJET(XB1, XB2, s, 0.0D0, t570 * t444, t570 * t572, -t570 * 
     #t574, s * t576 * t17 * t579 * t319, t634)
      t637 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t638 = s * t637
      t639 = t444 * x3
      t641 = t444 * t155
      t644 = t637 ** 2
      t650 = 0.1D1 / (-0.2D1 + t637)
      t651 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t652 = t650 * t651
      t653 = t644 ** 2
      t658 = log(-0.4D1 * t272 * t455 * t156 * t653)
      t660 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t667 = t39 * t650 * t660
      t677 = log(-0.4D1 * t272 * t455 * t155 * t653)
      t678 = t677 * t650
      t680 = t677 ** 2
      t684 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t700 = -(0.90D2 * t63 * t39 * (t652 - t658 * t650 * t660) - 0.180D
     #3 * t535 * t667) * t172 * t174 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t678 * t651 + t680 * t650 * t660 / 0.2D1 + t650 * t684) - 0.180D3
     # * t54 * t105 * (t652 - t678 * t660) + t484 * t667) * t172 * t114 
     #/ 0.720D3
      t701 = FJET(XB1, XB2, s, t638 * t639, -t638 * t641, -t638 * t447, 
     #0.0D0, s * t644 * t17 * t579 * t155, t700)
      t703 = KAPPA2(x1, x2, x3, t322, z)
      t704 = s * t703
      t709 = t703 ** 2
      t714 = cos(t11)
      t717 = Sqrt(x3 * t155 * t421)
      t724 = 0.1D1 / (-0.2D1 + t703)
      t725 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t322)
      t728 = t709 ** 2
      t733 = log(0.4D1 * t177 * t454 * t155 * t421 * t728)
      t735 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t322)
      t745 = -0.90D2 * t63 * t39 * (t724 * t725 - t733 * t724 * t735) + 
     #0.180D3 * t535 * t39 * t724 * t735
      t749 = FJET(XB1, XB2, s, t704 * t639, -t704 * t641, t704 * t572, -
     #t704 * t574, s * t709 * t17 * t579 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t714 * t717), -t745 * t172 * t174 / 0.720D3)
      rrqg2qght5s7e1 = t317 * t316 + t440 * t439 + t567 * t566 + t635 * 
     #t634 + t701 * t700 - t749 * t745 * t172 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrqg2qght5s7e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t76 = t68 * t13
      t77 = -0.1D1 + x3
      t82 = log(-0.4D1 * t76 * t16 * t77 * x4)
      t83 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t89 = t27 * t31
      t90 = t55 - t83
      t91 = t89 * t90
      t95 = 0.1D1 / x3
      t97 = 0.1D1 / x4
      t100 = t68 * t69
      t102 = log(0.4D1 * t100)
      t104 = t102 ** 2
      t107 = t69 * t77
      t110 = log(-0.4D1 * t68 * t107)
      t112 = t110 ** 2
      t115 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t130 = t16 * x4
      t133 = log(0.4D1 * t14 * t130)
      t135 = t133 ** 2
      t147 = t89 * t55
      t148 = t8 * t147
      t152 = x1 ** 2
      t153 = t152 * t11
      t156 = log(0.4D1 * t153 * t70)
      t165 = 0.1D1 / x1
      t169 = t36 * t31
      t170 = -t90
      t172 = t165 * t97
      t176 = x3 * t152
      t179 = log(0.4D1 * t176 * t17)
      t184 = log(-0.4D1 * t176 * t11 * t107)
      t197 = t153 * t69
      t199 = log(0.4D1 * t197)
      t201 = t199 ** 2
      t216 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (t32 - t73 *
     # t55 - t75 + t82 * t83) - 0.180D3 * t59 * t91) * t95 * t97 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (-t102 * t32 + t104 * t55 / 0.2D1 + t6
     #4 + t110 * t75 - t112 * t83 / 0.2D1 - t115) - 0.180D3 * t59 * t89 
     #* (t32 - t102 * t55 - t75 + t110 * t83) + t8 * t91) * t95 / 0.1440
     #D4 + (0.90D2 * t36 * t31 * (-t133 * t32 + t135 * t55 / 0.2D1 + t64
     #) - 0.180D3 * t59 * t89 * (t32 - t133 * t55) + t148) * t97 / 0.144
     #0D4 - (0.90D2 * t36 * t31 * (-t32 + t156 * t55) + 0.180D3 * t59 * 
     #t147) * t165 * t97 / 0.720D3 - t169 * t170 * t95 * t172 / 0.8D1 - 
     #(0.90D2 * t36 * t31 * (-t32 + t179 * t55 + t75 - t184 * t83) - 0.1
     #80D3 * t59 * t89 * t170) * t95 * t165 / 0.720D3 - (0.90D2 * t36 * 
     #t31 * (t199 * t32 - t201 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t8
     #9 * (-t32 + t199 * t55) - t148) * t165 / 0.720D3
      t217 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t216)
      t219 = -0.1D1 + x4
      t222 = t130 * t219
      t225 = log(-0.4D1 * t14 * t222)
      t226 = -t219
      t227 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t226)
      t229 = t225 ** 2
      t230 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t226)
      t233 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t226)
      t243 = t89 * t230
      t251 = log(-0.4D1 * t153 * t13 * t222)
      t263 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t22
     #6)
      t264 = -t263 + t230
      t269 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t22
     #6)
      t274 = log(0.4D1 * t76 * t130 * t219 * t77)
      t278 = log(-0.4D1 * t76 * t222)
      t292 = (-0.90D2 * t36 * t31 * (-t225 * t227 + t229 * t230 / 0.2D1 
     #+ t233) + 0.180D3 * t59 * t89 * (t227 - t225 * t230) - t8 * t243) 
     #* t97 / 0.1440D4 - (0.90D2 * t36 * t31 * (t227 - t251 * t230) - 0.
     #180D3 * t59 * t243) * t165 * t97 / 0.720D3 - t169 * t264 * t95 * t
     #172 / 0.8D1 + (0.90D2 * t36 * t31 * (t269 - t274 * t263 - t227 + t
     #278 * t230) + 0.180D3 * t59 * t89 * t264) * t95 * t97 / 0.1440D4
      t293 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t219, t2 * x4, 0.0D0,
     # t292)
      t295 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t296 = s * t295
      t297 = t1 * x1
      t299 = -0.1D1 + x1
      t300 = t1 * t299
      t302 = t295 ** 2
      t308 = 0.1D1 / (-0.2D1 + t295)
      t309 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t310 = t308 * t309
      t311 = t299 ** 2
      t312 = t152 * t311
      t313 = t302 ** 2
      t318 = log(0.4D1 * t17 * t312 * x4 * t313)
      t320 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t326 = t59 * t27
      t327 = t31 * t308
      t328 = t327 * t320
      t330 = 0.180D3 * t326 * t328
      t345 = log(0.4D1 * t76 * t16 * t152 * t311 * t313)
      t359 = log(0.4D1 * t17 * t312 * t313)
      t360 = t359 * t308
      t362 = t359 ** 2
      t366 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t382 = -(0.90D2 * t36 * t31 * (-t310 + t318 * t308 * t320) + t330)
     # * t165 * t97 / 0.720D3 + t36 * t327 * t320 * t95 * t172 / 0.8D1 -
     # (0.90D2 * t36 * t31 * (-t310 + t345 * t308 * t320) + t330) * t95 
     #* t165 / 0.720D3 - (-0.90D2 * t36 * t31 * (-t360 * t309 + t362 * t
     #308 * t320 / 0.2D1 + t308 * t366) + 0.180D3 * t59 * t89 * (t310 - 
     #t360 * t320) - t8 * t27 * t328) * t165 / 0.720D3
      t383 = FJET(XB1, XB2, s, 0.0D0, t296 * t297, -t296 * t300, 0.0D0, 
     #-s * t302 * t15 * x1 * t299, t382)
      t385 = KAPPA2(x1, x2, 0.0D0, t226, z)
      t386 = s * t385
      t388 = t300 * t219
      t390 = t300 * x4
      t392 = t385 ** 2
      t395 = x1 * t299
      t399 = 0.1D1 / (-0.2D1 + t385)
      t400 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t22
     #6)
      t403 = t392 ** 2
      t408 = log(-0.4D1 * t197 * t311 * x4 * t219 * t403)
      t410 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t22
     #6)
      t416 = t31 * t399
      t429 = -(0.90D2 * t36 * t31 * (t399 * t400 - t408 * t399 * t410) -
     # 0.180D3 * t326 * t416 * t410) * t165 * t97 / 0.720D3 - t36 * t416
     # * t410 * t95 * t172 / 0.8D1
      t430 = FJET(XB1, XB2, s, 0.0D0, t386 * t297, t386 * t388, -t386 * 
     #t390, s * t392 * t15 * t395 * t219, t429)
      t432 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t433 = s * t432
      t434 = t297 * x3
      t436 = t297 * t77
      t439 = t432 ** 2
      t445 = 0.1D1 / (-0.2D1 + t432)
      t446 = t31 * t445
      t448 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t453 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t455 = t439 ** 2
      t460 = log(-0.4D1 * t100 * t312 * t77 * t455)
      t474 = -t36 * t446 * t448 * t95 * t172 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t445 * t453 - t460 * t445 * t448) - 0.180D3 * t326 * t446 * 
     #t448) * t95 * t165 / 0.720D3
      t475 = FJET(XB1, XB2, s, t433 * t434, -t433 * t436, -t433 * t300, 
     #0.0D0, s * t439 * t15 * t395 * t77, t474)
      t477 = KAPPA2(x1, x2, x3, t226, z)
      t478 = s * t477
      t483 = t477 ** 2
      t488 = cos(t9)
      t492 = Sqrt(x3 * t77 * x4 * t219)
      t499 = 0.1D1 / (-0.2D1 + t477)
      t502 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t226)
      t507 = FJET(XB1, XB2, s, t478 * t434, -t478 * t436, t478 * t388, -
     #t478 * t390, s * t483 * t15 * t395 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t488 * t492), t36 * t31 * t499 * t502 * t95 * t17
     #2 / 0.8D1)
      rrqg2qght5s7e0 = t217 * t216 + t293 * t292 + t383 * t382 + t430 * 
     #t429 + t475 * t474 + t507 * 0.3141592653589793D1 * t89 * t499 * t5
     #02 * t95 * t165 * t97 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s7em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t13 = t10 * t12
      t15 = log(0.4D1 * t13)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t50 = t45 * t23
      t51 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t52 = t41 - t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t7
      t60 = t9 * t12
      t63 = log(0.4D1 * t59 * t60)
      t65 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t66 = -0.1D1 + x3
      t70 = log(-0.4D1 * t59 * t60 * t66)
      t76 = t19 * t23
      t83 = t12 * x4
      t86 = log(0.4D1 * t10 * t83)
      t94 = 0.180D3 * t3 * t76 * t41
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t108 = log(0.4D1 * t104 * t7 * t60)
      t121 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t52 * t53 * t55 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t63 * t41 - t65 + t70 * t5
     #1) - 0.180D3 * t3 * t76 * t52) * t53 / 0.1440D4 + (0.90D2 * t45 * 
     #t23 * (t24 - t86 * t41) - t94) * t55 / 0.1440D4 + t50 * t52 * t53 
     #* t100 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t108 * t41) + t94) 
     #* t100 / 0.720D3 + t50 * t41 * t100 * t55 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t124 = -0.1D1 + x4
      t127 = -t124
      t128 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t127)
      t132 = log(-0.4D1 * t10 * t83 * t124)
      t133 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t127)
      t149 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t12
     #7)
      t155 = (-0.90D2 * t45 * t23 * (t128 - t132 * t133) + 0.180D3 * t3 
     #* t76 * t133) * t55 / 0.1440D4 - t50 * t133 * t100 * t55 / 0.8D1 +
     # t50 * (-t133 + t149) * t53 * t55 / 0.16D2
      t156 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t124, t2 * x4, 0.0D0,
     # t155)
      t158 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t159 = s * t158
      t160 = t1 * x1
      t162 = -0.1D1 + x1
      t163 = t1 * t162
      t165 = t158 ** 2
      t171 = 0.1D1 / (-0.2D1 + t158)
      t172 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t173 = t171 * t172
      t174 = t53 * t100
      t178 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t180 = t162 ** 2
      t182 = t165 ** 2
      t186 = log(0.4D1 * t13 * t104 * t180 * t182)
      t201 = t100 * t55
      t205 = t50 * t173 * t174 / 0.8D1 - (-0.90D2 * t45 * t23 * (t171 * 
     #t178 - t186 * t171 * t172) + 0.180D3 * t3 * t19 * t23 * t171 * t17
     #2) * t100 / 0.720D3 + t50 * t173 * t201 / 0.8D1
      t206 = FJET(XB1, XB2, s, 0.0D0, t159 * t160, -t159 * t163, 0.0D0, 
     #-s * t165 * t11 * x1 * t162, t205)
      t208 = KAPPA2(x1, x2, 0.0D0, t127, z)
      t209 = s * t208
      t215 = t208 ** 2
      t218 = x1 * t162
      t223 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t12
     #7)
      t225 = 0.1D1 / (-0.2D1 + t208) * t223 * t201
      t228 = FJET(XB1, XB2, s, 0.0D0, t209 * t160, t209 * t163 * t124, -
     #t209 * t163 * x4, s * t215 * t11 * t218 * t124, -t50 * t225 / 0.8D
     #1)
      t233 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t234 = s * t233
      t240 = t233 ** 2
      t247 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t249 = 0.1D1 / (-0.2D1 + t233) * t247 * t174
      t252 = FJET(XB1, XB2, s, t234 * t160 * x3, -t234 * t160 * t66, -t2
     #34 * t163, 0.0D0, s * t240 * t11 * t218 * t66, -t50 * t249 / 0.8D1
     #)
      rrqg2qght5s7em1 = t122 * t121 + t156 * t155 + t206 * t205 - t228 *
     # 0.3141592653589793D1 * t76 * t225 / 0.8D1 - t252 * 0.314159265358
     #9793D1 * t76 * t249 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s7em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t14 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t21 = 0.1D1 / x1
      t25 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (t8 - t14) / x3 / 0.16D2 
     #+ t4 * t9 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 0.3
     #141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) * t3
     # * t9 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t51 = -0.1D1 + x4
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, -
     #t51)
      t57 = t7 * t55 * t10
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t51, t2 * x4, 0.0D0, -
     #t4 * t57 / 0.16D2)
      t65 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t66 = s * t65
      t69 = -0.1D1 + x1
      t72 = t65 ** 2
      t79 = 0.1D1 / (-0.2D1 + t65)
      t80 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t85 = FJET(XB1, XB2, s, 0.0D0, t66 * t1 * x1, -t66 * t1 * t69, 0.0
     #D0, -s * t72 * t37 * x1 * t69, t4 * t7 * t79 * t80 * t21 / 0.8D1)
      rrqg2qght5s7em2 = t49 * t48 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t85 * 0.3141592653589793D1 * t3 * t7 * t79 * t80 * t2
     #1 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s7em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s7em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s7em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s7em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght5s8e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = t27 * 0.3141592653589793D1
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrqg2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t15 * t18
      t93 = t92 * x4
      t96 = log(0.4D1 * t91 * t93)
      t98 = t96 ** 2
      t105 = t35 * t39
      t111 = t105 * t86
      t114 = 0.1D1 / x1
      t116 = 0.1D1 / x4
      t119 = t91 * t92
      t121 = log(0.4D1 * t119)
      t126 = t121 ** 2
      t137 = t10 * t111
      t148 = x3 * t90
      t149 = t148 * t13
      t152 = log(0.4D1 * t149 * t93)
      t154 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t155 = -0.1D1 + x3
      t156 = t155 * x4
      t160 = log(-0.4D1 * t149 * t92 * t156)
      t161 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t167 = t161 - t86
      t168 = t105 * t167
      t172 = 0.1D1 / x3
      t174 = t114 * t116
      t177 = t92 * t155
      t180 = log(-0.4D1 * t149 * t177)
      t182 = t180 ** 2
      t185 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t186 = t148 * t19
      t188 = log(0.4D1 * t186)
      t190 = t188 ** 2
      t208 = t18 * x4
      t211 = log(0.4D1 * t16 * t208)
      t216 = t211 ** 2
      t237 = x3 * t13
      t238 = t237 * t15
      t243 = log(-0.4D1 * t238 * t18 * t155 * x4)
      t245 = t243 ** 2
      t250 = log(0.4D1 * t237 * t93)
      t252 = t250 ** 2
      t266 = -t105 * t167
      t272 = t237 * t92
      t274 = log(0.4D1 * t272)
      t278 = log(-0.4D1 * t237 * t177)
      t283 = t278 ** 2
      t286 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t291 = t274 ** 2
      t316 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 - (0.90D2 * t63 * t39 * (t96 * t40 - t98 * t86 / 0.2D1 
     #- t50) - 0.180D3 * t54 * t105 * (-t40 + t96 * t86) - t44 * t111) *
     # t114 * t116 / 0.720D3 + (t44 * t105 * (t40 - t121 * t86) + 0.90D2
     # * t63 * t39 * (t126 * t40 / 0.2D1 + t59 - t126 * t121 * t86 / 0.6
     #D1 - t121 * t50) + t137 - 0.180D3 * t54 * t105 * (-t121 * t40 + t1
     #26 * t86 / 0.2D1 + t50)) * t114 / 0.720D3 - (0.90D2 * t63 * t39 * 
     #(-t40 + t152 * t86 + t154 - t160 * t161) - 0.180D3 * t54 * t168) *
     # t172 * t174 / 0.720D3 - (0.90D2 * t63 * t39 * (-t180 * t154 + t18
     #2 * t161 / 0.2D1 + t185 + t188 * t40 - t190 * t86 / 0.2D1 - t50) -
     # 0.180D3 * t54 * t105 * (t154 - t180 * t161 - t40 + t188 * t86) + 
     #t44 * t168) * t172 * t114 / 0.720D3 + (t44 * t105 * (t40 - t211 * 
     #t86) + 0.90D2 * t63 * t39 * (t216 * t40 / 0.2D1 + t59 - t216 * t21
     #1 * t86 / 0.6D1 - t211 * t50) + t137 - 0.180D3 * t54 * t105 * (-t2
     #11 * t40 + t216 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 + (0.90D2 
     #* t63 * t39 * (t243 * t154 - t245 * t161 / 0.2D1 - t185 - t250 * t
     #40 + t252 * t86 / 0.2D1 + t50) - 0.180D3 * t54 * t105 * (-t154 + t
     #243 * t161 + t40 - t250 * t86) + t44 * t266) * t172 * t116 / 0.144
     #0D4 + (t44 * t105 * (t40 - t274 * t86 - t154 + t278 * t161) + 0.90
     #D2 * t63 * t39 * (-t283 * t154 / 0.2D1 - t286 + t283 * t278 * t161
     # / 0.6D1 + t278 * t185 + t291 * t40 / 0.2D1 + t59 - t291 * t274 * 
     #t86 / 0.6D1 - t274 * t50) + t10 * t266 - 0.180D3 * t54 * t105 * (-
     #t274 * t40 + t291 * t86 / 0.2D1 + t50 + t278 * t154 - t283 * t161 
     #/ 0.2D1 - t185)) * t172 / 0.1440D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t316)
      t320 = -0.1D1 + x4
      t322 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t323 = t208 * t320
      t326 = log(-0.4D1 * t16 * t323)
      t327 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t332 = t326 ** 2
      t335 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t339 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t345 = t105 * t327
      t358 = t208 * t320 * t155
      t361 = log(0.4D1 * t238 * t358)
      t362 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t364 = t361 ** 2
      t365 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t368 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t371 = log(-0.4D1 * t238 * t323)
      t373 = t371 ** 2
      t386 = t365 - t327
      t396 = log(-0.4D1 * t91 * t15 * t323)
      t398 = t396 ** 2
      t415 = x4 * t320
      t419 = log(-0.4D1 * t149 * t92 * t415)
      t421 = t148 * t16
      t424 = log(0.4D1 * t421 * t358)
      t438 = (-t44 * t105 * (t322 - t326 * t327) - 0.90D2 * t63 * t39 * 
     #(t332 * t322 / 0.2D1 + t335 - t332 * t326 * t327 / 0.6D1 - t326 * 
     #t339) - t10 * t345 + 0.180D3 * t54 * t105 * (-t326 * t322 + t332 *
     # t327 / 0.2D1 + t339)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #-t361 * t362 + t364 * t365 / 0.2D1 + t368 + t371 * t322 - t373 * t
     #327 / 0.2D1 - t339) - 0.180D3 * t54 * t105 * (t362 - t361 * t365 -
     # t322 + t371 * t327) + t44 * t105 * t386) * t172 * t116 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t396 * t322 + t398 * t327 / 0.2D1 + t33
     #9) - 0.180D3 * t54 * t105 * (t322 - t396 * t327) + t44 * t345) * t
     #114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (t322 - t419 * t327 -
     # t362 + t424 * t365) + 0.180D3 * t54 * t105 * t386) * t172 * t174 
     #/ 0.720D3
      t439 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t320, 0.0D0,
     # t438)
      t442 = -0.1D1 + x1
      t444 = t442 ** 2
      t445 = t90 * t444
      t449 = log(0.4D1 * t19 * t445 * x4)
      t450 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t452 = t449 ** 2
      t453 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t456 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t466 = t105 * t453
      t467 = t44 * t466
      t471 = t18 * t90
      t472 = t471 * t444
      t475 = log(0.4D1 * t16 * t472)
      t480 = t475 ** 2
      t483 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t506 = log(0.4D1 * t238 * t471 * t444 * x4)
      t519 = log(0.4D1 * t238 * t472)
      t521 = t519 ** 2
      t537 = -(0.90D2 * t63 * t39 * (-t449 * t450 + t452 * t453 / 0.2D1 
     #+ t456) - 0.180D3 * t54 * t105 * (t450 - t449 * t453) + t467) * t1
     #14 * t116 / 0.720D3 + (-t44 * t105 * (t450 - t475 * t453) - 0.90D2
     # * t63 * t39 * (t480 * t450 / 0.2D1 + t483 - t480 * t475 * t453 / 
     #0.6D1 - t475 * t456) - t10 * t466 + 0.180D3 * t54 * t105 * (-t475 
     #* t450 + t480 * t453 / 0.2D1 + t456)) * t114 / 0.720D3 - (0.90D2 *
     # t63 * t39 * (t450 - t506 * t453) - 0.180D3 * t54 * t466) * t172 *
     # t174 / 0.720D3 - (0.90D2 * t63 * t39 * (-t519 * t450 + t521 * t45
     #3 / 0.2D1 + t456) - 0.180D3 * t54 * t105 * (t450 - t519 * t453) + 
     #t467) * t172 * t114 / 0.720D3
      t538 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t442, 0.0D0,
     # t537)
      t540 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t541 = s * t540
      t542 = t1 * x1
      t544 = t1 * t442
      t545 = t544 * x4
      t547 = t544 * t320
      t549 = t540 ** 2
      t552 = x1 * t442
      t555 = t549 ** 2
      t560 = log(-0.4D1 * t119 * t415 * t555 * t444)
      t562 = 0.1D1 / (-0.2D1 + t540)
      t563 = t560 * t562
      t564 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t566 = t560 ** 2
      t568 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t571 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t577 = t562 * t564
      t583 = t44 * t35
      t585 = t39 * t562 * t568
      t595 = log(-0.4D1 * t421 * t208 * t320 * t444 * t555)
      t602 = t54 * t35
      t609 = -(0.90D2 * t63 * t39 * (-t563 * t564 + t566 * t562 * t568 /
     # 0.2D1 + t562 * t571) - 0.180D3 * t54 * t105 * (t577 - t563 * t568
     #) + t583 * t585) * t114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t577 - t595 * t562 * t568) - 0.180D3 * t602 * t585) * t172 * t174 
     #/ 0.720D3
      t610 = FJET(XB1, XB2, s, 0.0D0, t541 * t542, -t541 * t545, t541 * 
     #t547, -s * t549 * t17 * t552 * x4, t609)
      t612 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t613 = s * t612
      t614 = t542 * x3
      t616 = t542 * t155
      t619 = t612 ** 2
      t625 = 0.1D1 / (-0.2D1 + t612)
      t626 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t627 = t625 * t626
      t628 = t619 ** 2
      t633 = log(-0.4D1 * t272 * t445 * t156 * t628)
      t635 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t642 = t39 * t625 * t635
      t652 = log(-0.4D1 * t272 * t445 * t155 * t628)
      t653 = t652 * t625
      t655 = t652 ** 2
      t659 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t675 = -(0.90D2 * t63 * t39 * (t627 - t633 * t625 * t635) - 0.180D
     #3 * t602 * t642) * t172 * t174 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t653 * t626 + t655 * t625 * t635 / 0.2D1 + t625 * t659) - 0.180D3
     # * t54 * t105 * (t627 - t653 * t635) + t583 * t642) * t172 * t114 
     #/ 0.720D3
      t676 = FJET(XB1, XB2, s, t613 * t614, -t613 * t616, 0.0D0, -t613 *
     # t544, -s * t619 * t17 * t552 * x3, t675)
      t678 = KAPPA2(x1, x2, x3, x4, z)
      t679 = s * t678
      t684 = t678 ** 2
      t689 = cos(t11)
      t692 = Sqrt(x3 * t155 * t415)
      t699 = 0.1D1 / (-0.2D1 + t678)
      t700 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t703 = t684 ** 2
      t708 = log(0.4D1 * t186 * t415 * t444 * t155 * t703)
      t710 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t720 = 0.90D2 * t63 * t39 * (-t699 * t700 + t708 * t699 * t710) + 
     #0.180D3 * t602 * t39 * t699 * t710
      t724 = FJET(XB1, XB2, s, t679 * t614, -t679 * t616, -t679 * t545, 
     #t679 * t547, s * t684 * t17 * t552 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t689 * t692), -t720 * t172 * t174 / 0.720D3)
      rrqg2qght5s8e1 = t317 * t316 + t439 * t438 + t538 * t537 + t610 * 
     #t609 + t676 * t675 - t724 * t720 * t172 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrqg2qght5s8e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
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
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t19 = log(0.4D1 * t17)
      t20 = t19 * 0.3141592653589793D1
      t23 = t19 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.1D1 / t1
      t29 = s ** 2
      t31 = 0.1D1 / t29 / s
      t32 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrqg2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t55 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t68 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t69 = x3 * t11
      t70 = t69 * t13
      t71 = -0.1D1 + x3
      t76 = log(-0.4D1 * t70 * t16 * t71 * x4)
      t77 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t69 * t80)
      t89 = t27 * t31
      t90 = t55 - t77
      t91 = t89 * t90
      t95 = 0.1D1 / x3
      t97 = 0.1D1 / x4
      t100 = t69 * t79
      t102 = log(0.4D1 * t100)
      t104 = t102 ** 2
      t107 = t79 * t71
      t110 = log(-0.4D1 * t69 * t107)
      t112 = t110 ** 2
      t115 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t130 = t16 * x4
      t133 = log(0.4D1 * t14 * t130)
      t135 = t133 ** 2
      t147 = t89 * t55
      t148 = t8 * t147
      t152 = x1 ** 2
      t153 = t152 * t11
      t156 = log(0.4D1 * t153 * t80)
      t165 = 0.1D1 / x1
      t169 = t36 * t31
      t170 = -t90
      t172 = t165 * t97
      t176 = x3 * t152
      t180 = log(-0.4D1 * t176 * t11 * t107)
      t184 = log(0.4D1 * t176 * t17)
      t197 = t153 * t79
      t199 = log(0.4D1 * t197)
      t201 = t199 ** 2
      t216 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (-t68 + t76 
     #* t77 + t32 - t83 * t55) - 0.180D3 * t59 * t91) * t95 * t97 / 0.14
     #40D4 + (0.90D2 * t36 * t31 * (-t102 * t32 + t104 * t55 / 0.2D1 + t
     #64 + t110 * t68 - t112 * t77 / 0.2D1 - t115) - 0.180D3 * t59 * t89
     # * (t32 - t102 * t55 - t68 + t110 * t77) + t8 * t91) * t95 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (-t133 * t32 + t135 * t55 / 0.2D1 + t6
     #4) - 0.180D3 * t59 * t89 * (t32 - t133 * t55) + t148) * t97 / 0.14
     #40D4 - (0.90D2 * t36 * t31 * (-t32 + t156 * t55) + 0.180D3 * t59 *
     # t147) * t165 * t97 / 0.720D3 - t169 * t170 * t95 * t172 / 0.8D1 -
     # (0.90D2 * t36 * t31 * (t68 - t180 * t77 - t32 + t184 * t55) - 0.1
     #80D3 * t59 * t89 * t170) * t95 * t165 / 0.720D3 + (0.90D2 * t36 * 
     #t31 * (-t199 * t32 + t201 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t
     #89 * (t32 - t199 * t55) + t148) * t165 / 0.720D3
      t217 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t216)
      t220 = -0.1D1 + x4
      t222 = t130 * t220
      t225 = log(-0.4D1 * t14 * t222)
      t226 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t228 = t225 ** 2
      t229 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t232 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t242 = t89 * t229
      t250 = log(-0.4D1 * t153 * t13 * t222)
      t262 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t263 = t229 - t262
      t268 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t273 = log(0.4D1 * t70 * t130 * t220 * t71)
      t277 = log(-0.4D1 * t70 * t222)
      t291 = (-0.90D2 * t36 * t31 * (-t225 * t226 + t228 * t229 / 0.2D1 
     #+ t232) + 0.180D3 * t59 * t89 * (t226 - t225 * t229) - t8 * t242) 
     #* t97 / 0.1440D4 - (0.90D2 * t36 * t31 * (t226 - t250 * t229) - 0.
     #180D3 * t59 * t242) * t165 * t97 / 0.720D3 - t169 * t263 * t95 * t
     #172 / 0.8D1 + (0.90D2 * t36 * t31 * (t268 - t273 * t262 - t226 + t
     #277 * t229) + 0.180D3 * t59 * t89 * t263) * t95 * t97 / 0.1440D4
      t292 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t220, 0.0D0,
     # t291)
      t295 = -0.1D1 + x1
      t297 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t298 = t295 ** 2
      t299 = t152 * t298
      t303 = log(0.4D1 * t17 * t299 * x4)
      t304 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t310 = t89 * t304
      t312 = 0.180D3 * t59 * t310
      t322 = t16 * t152 * t298
      t325 = log(0.4D1 * t70 * t322)
      t337 = log(0.4D1 * t14 * t322)
      t339 = t337 ** 2
      t342 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t356 = -(0.90D2 * t36 * t31 * (t297 - t303 * t304) - t312) * t165 
     #* t97 / 0.720D3 - t169 * t304 * t95 * t172 / 0.8D1 - (0.90D2 * t36
     # * t31 * (t297 - t325 * t304) - t312) * t95 * t165 / 0.720D3 + (-0
     #.90D2 * t36 * t31 * (-t337 * t297 + t339 * t304 / 0.2D1 + t342) + 
     #0.180D3 * t59 * t89 * (t297 - t337 * t304) - t8 * t310) * t165 / 0
     #.720D3
      t357 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t295, 0.0D0,
     # t356)
      t359 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t360 = s * t359
      t361 = t1 * x1
      t363 = t1 * t295
      t364 = t363 * x4
      t366 = t363 * t220
      t368 = t359 ** 2
      t371 = x1 * t295
      t375 = 0.1D1 / (-0.2D1 + t359)
      t376 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t378 = x4 * t220
      t379 = t368 ** 2
      t384 = log(-0.4D1 * t197 * t378 * t379 * t298)
      t386 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t392 = t59 * t27
      t393 = t31 * t375
      t406 = -(0.90D2 * t36 * t31 * (t375 * t376 - t384 * t375 * t386) -
     # 0.180D3 * t392 * t393 * t386) * t165 * t97 / 0.720D3 - t36 * t393
     # * t386 * t95 * t172 / 0.8D1
      t407 = FJET(XB1, XB2, s, 0.0D0, t360 * t361, -t360 * t364, t360 * 
     #t366, -s * t368 * t15 * t371 * x4, t406)
      t409 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t410 = s * t409
      t411 = t361 * x3
      t413 = t361 * t71
      t416 = t409 ** 2
      t422 = 0.1D1 / (-0.2D1 + t409)
      t423 = t31 * t422
      t425 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t430 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t432 = t416 ** 2
      t437 = log(-0.4D1 * t100 * t299 * t71 * t432)
      t451 = -t36 * t423 * t425 * t95 * t172 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t422 * t430 - t437 * t422 * t425) - 0.180D3 * t392 * t423 * 
     #t425) * t95 * t165 / 0.720D3
      t452 = FJET(XB1, XB2, s, t410 * t411, -t410 * t413, 0.0D0, -t410 *
     # t363, -s * t416 * t15 * t371 * x3, t451)
      t454 = KAPPA2(x1, x2, x3, x4, z)
      t455 = s * t454
      t460 = t454 ** 2
      t465 = cos(t9)
      t468 = Sqrt(x3 * t71 * t378)
      t475 = 0.1D1 / (-0.2D1 + t454)
      t478 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t483 = FJET(XB1, XB2, s, t455 * t411, -t455 * t413, -t455 * t364, 
     #t455 * t366, s * t460 * t15 * t371 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t465 * t468), t36 * t31 * t475 * t478 * t95 * t172 / 0.8D
     #1)
      rrqg2qght5s8e0 = t217 * t216 + t292 * t291 + t357 * t356 + t407 * 
     #t406 + t452 * t451 + t483 * 0.3141592653589793D1 * t89 * t475 * t4
     #78 * t95 * t165 * t97 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s8em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t6 = sin(x2 * 0.3141592653589793D1)
      t7 = t6 ** 2
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t15 = log(0.4D1 * t10 * t12)
      t16 = t15 * 0.3141592653589793D1
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrqg2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t50 = t45 * t23
      t51 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t52 = t41 - t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t7
      t60 = t9 * t12
      t63 = log(0.4D1 * t59 * t60)
      t65 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t66 = -0.1D1 + x3
      t70 = log(-0.4D1 * t59 * t60 * t66)
      t76 = t19 * t23
      t83 = t12 * x4
      t86 = log(0.4D1 * t10 * t83)
      t94 = 0.180D3 * t3 * t76 * t41
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t108 = log(0.4D1 * t104 * t7 * t60)
      t121 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t52 * t53 * t55 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t63 * t41 - t65 + t70 * t5
     #1) - 0.180D3 * t3 * t76 * t52) * t53 / 0.1440D4 + (0.90D2 * t45 * 
     #t23 * (t24 - t86 * t41) - t94) * t55 / 0.1440D4 + t50 * t52 * t53 
     #* t100 / 0.8D1 + (0.90D2 * t45 * t23 * (t24 - t108 * t41) - t94) *
     # t100 / 0.720D3 + t50 * t41 * t100 * t55 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t125 = -0.1D1 + x4
      t127 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t131 = log(-0.4D1 * t10 * t83 * t125)
      t132 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t148 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t154 = (-0.90D2 * t45 * t23 * (t127 - t131 * t132) + 0.180D3 * t3 
     #* t76 * t132) * t55 / 0.1440D4 - t50 * t132 * t100 * t55 / 0.8D1 +
     # t50 * (t148 - t132) * t53 * t55 / 0.16D2
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t125, 0.0D0,
     # t154)
      t158 = -0.1D1 + x1
      t160 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t165 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t167 = t158 ** 2
      t171 = log(0.4D1 * t10 * t12 * t104 * t167)
      t187 = -t50 * t160 * t53 * t100 / 0.8D1 + (-0.90D2 * t45 * t23 * (
     #t165 - t171 * t160) + 0.180D3 * t3 * t76 * t160) * t100 / 0.720D3 
     #- t50 * t160 * t100 * t55 / 0.8D1
      t188 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t158, 0.0D0,
     # t187)
      t190 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t191 = s * t190
      t192 = t1 * x1
      t194 = t1 * t158
      t199 = t190 ** 2
      t202 = x1 * t158
      t207 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t210 = 0.1D1 / (-0.2D1 + t190) * t207 * t100 * t55
      t213 = FJET(XB1, XB2, s, 0.0D0, t191 * t192, -t191 * t194 * x4, t1
     #91 * t194 * t125, -s * t199 * t11 * t202 * x4, -t50 * t210 / 0.8D1
     #)
      t218 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t219 = s * t218
      t225 = t218 ** 2
      t232 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t235 = 0.1D1 / (-0.2D1 + t218) * t232 * t53 * t100
      t238 = FJET(XB1, XB2, s, t219 * t192 * x3, -t219 * t192 * t66, 0.0
     #D0, -t219 * t194, -s * t225 * t11 * t202 * x3, -t50 * t235 / 0.8D1
     #)
      rrqg2qght5s8em1 = t122 * t121 + t155 * t154 + t188 * t187 - t213 *
     # 0.3141592653589793D1 * t76 * t210 / 0.8D1 - t238 * 0.314159265358
     #9793D1 * t76 * t235 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s8em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t14 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t21 = 0.1D1 / x1
      t25 = rrqg2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (t8 - t14) / x3 / 0.16D2 
     #+ t4 * t9 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 0.3
     #141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) * t3
     # * t9 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t54 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t56 = t7 * t54 * t10
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, -t4 * t56 / 0.16D2)
      t67 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t69 = t7 * t67 * t21
      t72 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, -t4 * t69 / 0.8D1)
      rrqg2qght5s8em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.16D2 - t72 * 0.3141592653589793D1 * t3 * t69 / 0.8D1

      end function



      doubleprecision function rrqg2qght5s8em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqg2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqg2qght5s8em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqg2qght5s8em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh51J1
      doubleprecision rrqg2qgh51J2
      doubleprecision rrqg2qgh51J3
      doubleprecision rrqg2qgh51J4
      doubleprecision rrqg2qgh51J5
      doubleprecision rrqg2qgh51J6
      rrqg2qght5s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh51J1
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
      t5 = 0.1D1 - z
      t7 = 0.1D1 - x3
      t10 = t3 ** 2
      t11 = t10 * t3
      t13 = t5 ** 2
      t14 = t13 * t5
      t15 = x1 ** 2
      t16 = t15 * x1
      t18 = t7 ** 2
      t22 = t10 ** 2
      t25 = t13 ** 2
      t29 = (0.1D1 - x1) ** 2
      t30 = t29 ** 2
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t44 = t43 ** 2
      t54 = x4 ** 2
      rrqg2qgh51J1 = 0.4D1 * wd * (t2 * t3 * t5 * x1 * t7 + t2 * t11 * t
     #14 * t16 * t18 * t7 + t2 * t22 * t11 * t25 * t14 * t16 * t30 * t44
     # * t43 * x4 + t2 * t22 * t3 * t25 * t5 * x1 * t30 * t43 * t54 * x4
     #) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh51J2
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
      t5 = 0.1D1 - z
      t7 = 0.1D1 - x3
      t9 = t2 * t3 * t5 * x1 * t7
      t10 = t3 ** 2
      t11 = t10 * t3
      t12 = t2 * t11
      t13 = t5 ** 2
      t14 = t13 * t5
      t15 = x1 ** 2
      t16 = t15 * x1
      t18 = t7 ** 2
      t21 = t12 * t14 * t16 * t18 * t7
      t22 = t10 ** 2
      t25 = t13 ** 2
      t28 = 0.1D1 - x1
      t29 = t28 ** 2
      t30 = t29 ** 2
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t44 = t43 ** 2
      t48 = t2 * t22 * t11 * t25 * t14 * t16 * t30 * t44 * t43 * x4
      t52 = t2 * t22 * t3 * t25 * t5
      t54 = x4 ** 2
      t58 = t52 * x1 * t30 * t43 * t54 * x4
      t62 = t2 * t22 * t25
      t72 = t12 * t14
      t81 = t29 * t28
      t103 = -t62 * t16 * t28 * t43 * t18 + 0.2D1 * t2 * t10 * t13 * t15
     # * t18 + t72 * t15 * t28 * t43 * t7 + t52 * t16 * t29 * t44 * t7 -
     # t21 - t9 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t22 * t10 * 
     #t25 * t13 * t30 * t54 * t15 * t44 + t52 * t15 * t81 * t44 * x4 + t
     #72 * x1 * t29 * t43 * x4 - t58 - t48
      rrqg2qgh51J2 = 0.4D1 * (wd * (t9 + t21 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh51J3
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
      t5 = 0.1D1 - z
      t7 = 0.1D1 - x3
      t9 = t2 * t3 * t5 * x1 * t7
      t10 = t3 ** 2
      t11 = t10 * t3
      t12 = t2 * t11
      t13 = t5 ** 2
      t14 = t13 * t5
      t15 = x1 ** 2
      t16 = t15 * x1
      t18 = t7 ** 2
      t21 = t12 * t14 * t16 * t18 * t7
      t22 = t10 ** 2
      t25 = t13 ** 2
      t28 = 0.1D1 - x1
      t29 = t28 ** 2
      t30 = t29 ** 2
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t44 = t43 ** 2
      t48 = t2 * t22 * t11 * t25 * t14 * t16 * t30 * t44 * t43 * x4
      t52 = t2 * t22 * t3 * t25 * t5
      t54 = x4 ** 2
      t58 = t52 * x1 * t30 * t43 * t54 * x4
      t62 = t2 * t22 * t25
      t72 = t12 * t14
      t81 = t29 * t28
      t103 = -t62 * t16 * t28 * t43 * t18 + 0.2D1 * t2 * t10 * t13 * t15
     # * t18 + t72 * t15 * t28 * t43 * t7 + t52 * t16 * t29 * t44 * t7 -
     # t21 - t9 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t22 * t10 * 
     #t25 * t13 * t30 * t54 * t15 * t44 + t52 * t15 * t81 * t44 * x4 + t
     #72 * x1 * t29 * t43 * x4 - t58 - t48
      rrqg2qgh51J3 = 0.4D1 * (wd * (t9 + t21 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh51J4
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
      t5 = 0.1D1 - z
      t7 = 0.1D1 - x3
      t9 = t2 * t3 * t5 * x1 * t7
      t10 = t3 ** 2
      t11 = t10 * t3
      t12 = t2 * t11
      t13 = t5 ** 2
      t14 = t13 * t5
      t15 = x1 ** 2
      t16 = t15 * x1
      t18 = t7 ** 2
      t21 = t12 * t14 * t16 * t18 * t7
      t22 = t10 ** 2
      t25 = t13 ** 2
      t28 = 0.1D1 - x1
      t29 = t28 ** 2
      t30 = t29 ** 2
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t44 = t43 ** 2
      t48 = t2 * t22 * t11 * t25 * t14 * t16 * t30 * t44 * t43 * x4
      t52 = t2 * t22 * t3 * t25 * t5
      t54 = x4 ** 2
      t58 = t52 * x1 * t30 * t43 * t54 * x4
      t62 = t2 * t22 * t25
      t72 = t12 * t14
      t81 = t29 * t28
      t103 = -t62 * t16 * t28 * t43 * t18 + 0.2D1 * t2 * t10 * t13 * t15
     # * t18 + t72 * t15 * t28 * t43 * t7 + t52 * t16 * t29 * t44 * t7 -
     # t21 - t9 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t22 * t10 * 
     #t25 * t13 * t30 * t54 * t15 * t44 + t52 * t15 * t81 * t44 * x4 + t
     #72 * x1 * t29 * t43 * x4 - t58 - t48
      rrqg2qgh51J4 = 0.4D1 * (wd * (t9 + t21 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh51J5
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
      t5 = 0.1D1 - z
      t7 = 0.1D1 - x3
      t9 = t2 * t3 * t5 * x1 * t7
      t10 = t3 ** 2
      t11 = t10 * t3
      t12 = t2 * t11
      t13 = t5 ** 2
      t14 = t13 * t5
      t15 = x1 ** 2
      t16 = t15 * x1
      t18 = t7 ** 2
      t21 = t12 * t14 * t16 * t18 * t7
      t22 = t10 ** 2
      t25 = t13 ** 2
      t28 = 0.1D1 - x1
      t29 = t28 ** 2
      t30 = t29 ** 2
      t35 = cos(x2 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t35 * t40
      t44 = t43 ** 2
      t48 = t2 * t22 * t11 * t25 * t14 * t16 * t30 * t44 * t43 * x4
      t52 = t2 * t22 * t3 * t25 * t5
      t54 = x4 ** 2
      t58 = t52 * x1 * t30 * t43 * t54 * x4
      t62 = t2 * t22 * t25
      t72 = t12 * t14
      t81 = t29 * t28
      t103 = -t62 * t16 * t28 * t43 * t18 + 0.2D1 * t2 * t10 * t13 * t15
     # * t18 + t72 * t15 * t28 * t43 * t7 + t52 * t16 * t29 * t44 * t7 -
     # t21 - t9 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t22 * t10 * 
     #t25 * t13 * t30 * t54 * t15 * t44 + t52 * t15 * t81 * t44 * x4 + t
     #72 * x1 * t29 * t43 * x4 - t58 - t48
      rrqg2qgh51J5 = 0.4D1 * (wd * (t9 + t21 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh51J6
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
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 ** 2
      t10 = t2 * t5 * t9
      t11 = x1 ** 2
      t12 = t11 * x1
      t13 = 0.1D1 - x1
      t18 = cos(x2 * 0.3141592653589793D1)
      t19 = 0.1D1 - x3
      t24 = Sqrt(x3 * t19 * x4 * (0.1D1 - x4))
      t27 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t18 * t24
      t28 = t19 ** 2
      t37 = t4 * t3
      t38 = t2 * t37
      t39 = t7 * t8
      t40 = t38 * t39
      t48 = t2 * t5 * t3 * t9 * t7
      t49 = t13 ** 2
      t51 = t27 ** 2
      t63 = t49 * t13
      t65 = x4 ** 2
      t73 = t49 ** 2
      t101 = -t10 * t12 * t13 * t27 * t28 + 0.2D1 * t2 * t4 * t8 * t11 *
     # t28 + t40 * t11 * t13 * t27 * t19 + t48 * t12 * t49 * t51 * t19 -
     # t38 * t39 * t12 * t28 * t19 - t2 * t3 * t7 * x1 * t19 - t10 * x1 
     #* t63 * t27 * t65 + 0.2D1 * t2 * t5 * t4 * t9 * t8 * t73 * t65 * t
     #11 * t51 + t48 * t11 * t63 * t51 * x4 + t40 * x1 * t49 * t27 * x4 
     #- t48 * x1 * t73 * t27 * t65 * x4 - t2 * t5 * t37 * t9 * t39 * t12
     # * t73 * t51 * t27 * x4
      rrqg2qgh51J6 = 0.4D1 * wd * t101 / s / z / 0.3141592653589793D1

      end function
  
 