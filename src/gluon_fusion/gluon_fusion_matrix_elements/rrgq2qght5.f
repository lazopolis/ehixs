  
      subroutine rrgq2qght5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh51J1  
      doubleprecision rrgq2qgh51J2  
      doubleprecision rrgq2qgh51J3  
      doubleprecision rrgq2qgh51J4  
      doubleprecision rrgq2qgh51J5  
      doubleprecision rrgq2qgh51J6  
      doubleprecision rrgq2qght5s1e1  
      doubleprecision rrgq2qght5s1e0  
      doubleprecision rrgq2qght5s1em1  
      doubleprecision rrgq2qght5s1em2  
      doubleprecision rrgq2qght5s1em3  
      doubleprecision rrgq2qght5s1em4  
      doubleprecision rrgq2qght5s2e1  
      doubleprecision rrgq2qght5s2e0  
      doubleprecision rrgq2qght5s2em1  
      doubleprecision rrgq2qght5s2em2  
      doubleprecision rrgq2qght5s2em3  
      doubleprecision rrgq2qght5s2em4  
      doubleprecision rrgq2qght5s3e1  
      doubleprecision rrgq2qght5s3e0  
      doubleprecision rrgq2qght5s3em1  
      doubleprecision rrgq2qght5s3em2  
      doubleprecision rrgq2qght5s3em3  
      doubleprecision rrgq2qght5s3em4  
      doubleprecision rrgq2qght5s4e1  
      doubleprecision rrgq2qght5s4e0  
      doubleprecision rrgq2qght5s4em1  
      doubleprecision rrgq2qght5s4em2  
      doubleprecision rrgq2qght5s4em3  
      doubleprecision rrgq2qght5s4em4  
      doubleprecision rrgq2qght5s5e1  
      doubleprecision rrgq2qght5s5e0  
      doubleprecision rrgq2qght5s5em1  
      doubleprecision rrgq2qght5s5em2  
      doubleprecision rrgq2qght5s5em3  
      doubleprecision rrgq2qght5s5em4  
      doubleprecision rrgq2qght5s6e1  
      doubleprecision rrgq2qght5s6e0  
      doubleprecision rrgq2qght5s6em1  
      doubleprecision rrgq2qght5s6em2  
      doubleprecision rrgq2qght5s6em3  
      doubleprecision rrgq2qght5s6em4  
      doubleprecision rrgq2qght5s7e1  
      doubleprecision rrgq2qght5s7e0  
      doubleprecision rrgq2qght5s7em1  
      doubleprecision rrgq2qght5s7em2  
      doubleprecision rrgq2qght5s7em3  
      doubleprecision rrgq2qght5s7em4  
      doubleprecision rrgq2qght5s8e1  
      doubleprecision rrgq2qght5s8e0  
      doubleprecision rrgq2qght5s8em1  
      doubleprecision rrgq2qght5s8em2  
      doubleprecision rrgq2qght5s8em3  
      doubleprecision rrgq2qght5s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght5s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
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
      t109 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t111 = t107 ** 2
      t112 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t115 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t120 = t35 * t39
      t127 = -t112 + t86
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
      t183 = -t120 * t127
      t187 = 0.1D1 / x3
      t189 = t131 * t133
      t192 = t166 * t19
      t194 = log(0.4D1 * t192)
      t196 = t194 ** 2
      t215 = log(0.4D1 * t16 * t102)
      t219 = log(-0.4D1 * t16 * t104)
      t224 = t219 ** 2
      t227 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t108)
      t232 = t215 ** 2
      t257 = x3 * t13
      t258 = t257 * t15
      t261 = log(-0.4D1 * t258 * t104)
      t263 = t261 ** 2
      t268 = log(0.4D1 * t257 * t93)
      t270 = t268 ** 2
      t290 = log(0.4D1 * t257 * t92)
      t295 = t290 ** 2
      t316 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (-t96 * t40 + t98 * t86 / 0.2D1
     # + t50 + t107 * t109 - t111 * t112 / 0.2D1 - t115) - 0.180D3 * t54
     # * t120 * (t40 - t96 * t86 - t109 + t107 * t112) + t44 * t120 * t1
     #27) * t131 * t133 / 0.720D3 - (t44 * t120 * (-t40 + t138 * t86) + 
     #0.90D2 * t63 * t39 * (-t143 * t40 / 0.2D1 - t59 + t143 * t138 * t8
     #6 / 0.6D1 + t138 * t50) - t155 - 0.180D3 * t54 * t120 * (t138 * t4
     #0 - t143 * t86 / 0.2D1 - t50)) * t131 / 0.720D3 - (0.90D2 * t63 * 
     #t39 * (-t40 + t170 * t86 + t109 - t176 * t112) - 0.180D3 * t54 * t
     #183) * t187 * t189 / 0.720D3 - (0.90D2 * t63 * t39 * (t194 * t40 -
     # t196 * t86 / 0.2D1 - t50) - 0.180D3 * t54 * t120 * (-t40 + t194 *
     # t86) - t44 * t154) * t187 * t131 / 0.720D3 - (t44 * t120 * (-t40 
     #+ t215 * t86 + t109 - t219 * t112) + 0.90D2 * t63 * t39 * (t224 * 
     #t109 / 0.2D1 + t227 - t224 * t219 * t112 / 0.6D1 - t219 * t115 - t
     #232 * t40 / 0.2D1 - t59 + t232 * t215 * t86 / 0.6D1 + t215 * t50) 
     #+ t10 * t183 - 0.180D3 * t54 * t120 * (t215 * t40 - t232 * t86 / 0
     #.2D1 - t50 - t219 * t109 + t224 * t112 / 0.2D1 + t115)) * t133 / 0
     #.1440D4 - (0.90D2 * t63 * t39 * (-t261 * t109 + t263 * t112 / 0.2D
     #1 + t115 + t268 * t40 - t270 * t86 / 0.2D1 - t50) - 0.180D3 * t54 
     #* t120 * (t109 - t261 * t112 - t40 + t268 * t86) + t44 * t183) * t
     #187 * t133 / 0.1440D4 - (t44 * t120 * (-t40 + t290 * t86) + 0.90D2
     # * t63 * t39 * (-t295 * t40 / 0.2D1 - t59 + t295 * t290 * t86 / 0.
     #6D1 + t290 * t50) - t155 - 0.180D3 * t54 * t120 * (t290 * t40 - t2
     #95 * t86 / 0.2D1 - t50)) * t187 / 0.1440D4
      t317 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t316)
      t319 = 0.1D1 - x1
      t320 = 0.1D1 - x3
      t321 = KAPPA2(t319, x2, t320, 0.10D1, z)
      t322 = s * t321
      t323 = -t319
      t324 = t1 * t323
      t325 = -t320
      t326 = t324 * t325
      t328 = t324 * x3
      t330 = t1 * x1
      t332 = t321 ** 2
      t335 = t323 * x1
      t339 = 0.1D1 / (-0.2D1 + t321)
      t340 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, t320, 0.
     #10D1)
      t341 = t339 * t340
      t342 = t166 * t16
      t343 = t323 ** 2
      t344 = t18 * t343
      t345 = t325 * x4
      t346 = t332 ** 2
      t351 = log(-0.4D1 * t342 * t344 * t345 * t346)
      t353 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, t320, 0.
     #10D1)
      t359 = t54 * t35
      t361 = t39 * t339 * t353
      t371 = log(-0.4D1 * t342 * t344 * t325 * t346)
      t372 = t371 * t339
      t374 = t371 ** 2
      t378 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, t320, 0.
     #10D1)
      t389 = t44 * t35
      t395 = -(0.90D2 * t63 * t39 * (t341 - t351 * t339 * t353) - 0.180D
     #3 * t359 * t361) * t187 * t189 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t372 * t340 + t374 * t339 * t353 / 0.2D1 + t339 * t378) - 0.180D3
     # * t54 * t120 * (t341 - t372 * t353) + t389 * t361) * t187 * t131 
     #/ 0.720D3
      t396 = FJET(XB1, XB2, s, t322 * t326, -t322 * t328, t322 * t330, 0
     #.0D0, -s * t332 * t17 * t335 * x3, t395)
      t398 = KAPPA2(t319, x2, t320, t108, z)
      t399 = s * t398
      t402 = t330 * t103
      t404 = t330 * x4
      t406 = t398 ** 2
      t411 = cos(t11)
      t414 = Sqrt(x3 * t325 * t172)
      t421 = 0.1D1 / (-0.2D1 + t398)
      t422 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, t320, t1
     #08)
      t425 = t406 ** 2
      t430 = log(0.4D1 * t192 * t343 * t325 * t172 * t425)
      t432 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, t320, t1
     #08)
      t442 = 0.90D2 * t63 * t39 * (-t421 * t422 + t430 * t421 * t432) + 
     #0.180D3 * t359 * t39 * t421 * t432
      t446 = FJET(XB1, XB2, s, t399 * t326, -t399 * t328, -t399 * t402, 
     #t399 * t404, s * t406 * t17 * t335 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t411 * t414), -t442 * t187 * t189 / 0.720D3)
      t458 = log(0.4D1 * t19 * t90 * t343 * x4)
      t459 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #0.10D1)
      t461 = t458 ** 2
      t462 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #0.10D1)
      t465 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #0.10D1)
      t475 = t120 * t462
      t476 = t44 * t475
      t480 = t18 * t90
      t481 = t480 * t343
      t484 = log(0.4D1 * t16 * t481)
      t489 = t484 ** 2
      t492 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #0.10D1)
      t511 = t343 * x4
      t515 = log(0.4D1 * t258 * t480 * t511)
      t528 = log(0.4D1 * t258 * t481)
      t530 = t528 ** 2
      t546 = (0.90D2 * t63 * t39 * (t458 * t459 - t461 * t462 / 0.2D1 - 
     #t465) - 0.180D3 * t54 * t120 * (-t459 + t458 * t462) - t476) * t13
     #1 * t133 / 0.720D3 - (t44 * t120 * (t459 - t484 * t462) + 0.90D2 *
     # t63 * t39 * (t489 * t459 / 0.2D1 + t492 - t489 * t484 * t462 / 0.
     #6D1 - t484 * t465) + t10 * t475 - 0.180D3 * t54 * t120 * (-t484 * 
     #t459 + t489 * t462 / 0.2D1 + t465)) * t131 / 0.720D3 - (0.90D2 * t
     #63 * t39 * (t459 - t515 * t462) - 0.180D3 * t54 * t475) * t187 * t
     #189 / 0.720D3 - (0.90D2 * t63 * t39 * (-t528 * t459 + t530 * t462 
     #/ 0.2D1 + t465) - 0.180D3 * t54 * t120 * (t459 - t528 * t462) + t4
     #76) * t187 * t131 / 0.720D3
      t547 = FJET(XB1, XB2, s, -t2 * t323, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t546)
      t555 = log(-0.4D1 * t258 * t18 * t325 * x4)
      t556 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #0.10D1)
      t558 = t555 ** 2
      t559 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #0.10D1)
      t562 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #0.10D1)
      t564 = t102 * t103 * t325
      t567 = log(0.4D1 * t258 * t564)
      t568 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #t108)
      t570 = t567 ** 2
      t571 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #t108)
      t574 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #t108)
      t586 = t120 * (-t571 + t559)
      t592 = t92 * t325
      t595 = log(-0.4D1 * t257 * t592)
      t600 = t595 ** 2
      t603 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t320, 
     #0.10D1)
      t612 = t120 * t559
      t627 = log(-0.4D1 * t167 * t92 * t345)
      t631 = log(0.4D1 * t342 * t564)
      t645 = log(-0.4D1 * t167 * t592)
      t647 = t645 ** 2
      t664 = -(0.90D2 * t63 * t39 * (-t555 * t556 + t558 * t559 / 0.2D1 
     #+ t562 + t567 * t568 - t570 * t571 / 0.2D1 - t574) - 0.180D3 * t54
     # * t120 * (t556 - t555 * t559 - t568 + t567 * t571) + t44 * t586) 
     #* t187 * t133 / 0.1440D4 - (t44 * t120 * (t556 - t595 * t559) + 0.
     #90D2 * t63 * t39 * (t600 * t556 / 0.2D1 + t603 - t600 * t595 * t55
     #9 / 0.6D1 - t595 * t562) + t10 * t612 - 0.180D3 * t54 * t120 * (-t
     #595 * t556 + t600 * t559 / 0.2D1 + t562)) * t187 / 0.1440D4 - (0.9
     #0D2 * t63 * t39 * (t556 - t627 * t559 - t568 + t631 * t571) - 0.18
     #0D3 * t54 * t586) * t187 * t189 / 0.720D3 - (0.90D2 * t63 * t39 * 
     #(-t645 * t556 + t647 * t559 / 0.2D1 + t562) - 0.180D3 * t54 * t120
     # * (t556 - t645 * t559) + t44 * t612) * t187 * t131 / 0.720D3
      t665 = FJET(XB1, XB2, s, -t2 * t325, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t664)
      t667 = KAPPA2(t319, x2, 0.10D1, t108, z)
      t668 = s * t667
      t672 = t667 ** 2
      t677 = t672 ** 2
      t682 = log(-0.4D1 * t136 * t511 * t103 * t677)
      t684 = 0.1D1 / (-0.2D1 + t667)
      t685 = t682 * t684
      t686 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #t108)
      t688 = t682 ** 2
      t690 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #t108)
      t693 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.10D1, 
     #t108)
      t699 = t684 * t686
      t706 = t39 * t684 * t690
      t715 = log(-0.4D1 * t342 * t344 * t172 * t677)
      t728 = (-0.90D2 * t63 * t39 * (-t685 * t686 + t688 * t684 * t690 /
     # 0.2D1 + t684 * t693) + 0.180D3 * t54 * t120 * (t699 - t685 * t690
     #) - t389 * t706) * t131 * t133 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t699 - t715 * t684 * t690) - 0.180D3 * t359 * t706) * t187 * t189 
     #/ 0.720D3
      t729 = FJET(XB1, XB2, s, -t668 * t324, 0.0D0, -t668 * t402, t668 *
     # t404, -s * t672 * t17 * t335 * x4, t728)
      rrgq2qght5s1e1 = t317 * t316 + t396 * t395 - t446 * t442 * t187 * 
     #t131 * t133 / 0.720D3 + t547 * t546 + t665 * t664 + t729 * t728

      end function



      doubleprecision function rrgq2qght5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t68 = 0.1D1 - x4
      t69 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t68)
      t70 = x3 * t11
      t71 = t70 * t13
      t72 = t16 * x4
      t73 = -t68
      t74 = t72 * t73
      t77 = log(-0.4D1 * t71 * t74)
      t78 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t68)
      t80 = t13 * t16
      t81 = t80 * x4
      t84 = log(0.4D1 * t70 * t81)
      t90 = t27 * t31
      t91 = -t55 + t78
      t92 = t90 * t91
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t103 = log(0.4D1 * t70 * t80)
      t105 = t103 ** 2
      t117 = t90 * t55
      t118 = t8 * t117
      t124 = log(0.4D1 * t14 * t72)
      t126 = t124 ** 2
      t131 = log(-0.4D1 * t14 * t74)
      t133 = t131 ** 2
      t136 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t68)
      t151 = x1 ** 2
      t152 = t151 * t11
      t155 = log(0.4D1 * t152 * t81)
      t160 = log(-0.4D1 * t152 * t13 * t74)
      t171 = 0.1D1 / x1
      t175 = t36 * t31
      t177 = t171 * t98
      t181 = x3 * t151
      t184 = log(0.4D1 * t181 * t17)
      t196 = t152 * t80
      t198 = log(0.4D1 * t196)
      t200 = t198 ** 2
      t215 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 - (0.90D2 * t36 * t31 * (t69 - t77 *
     # t78 - t32 + t84 * t55) - 0.180D3 * t59 * t92) * t96 * t98 / 0.144
     #0D4 - (0.90D2 * t36 * t31 * (t103 * t32 - t105 * t55 / 0.2D1 - t64
     #) - 0.180D3 * t59 * t90 * (-t32 + t103 * t55) - t118) * t96 / 0.14
     #40D4 - (0.90D2 * t36 * t31 * (t124 * t32 - t126 * t55 / 0.2D1 - t6
     #4 - t131 * t69 + t133 * t78 / 0.2D1 + t136) - 0.180D3 * t59 * t90 
     #* (-t32 + t124 * t55 + t69 - t131 * t78) + t8 * t92) * t98 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (t32 - t155 * t55 - t69 + t160 * t78) 
     #+ 0.180D3 * t59 * t90 * t91) * t171 * t98 / 0.720D3 - t175 * t91 *
     # t96 * t177 / 0.8D1 - (0.90D2 * t36 * t31 * (-t32 + t184 * t55) + 
     #0.180D3 * t59 * t117) * t96 * t171 / 0.720D3 - (0.90D2 * t36 * t31
     # * (t198 * t32 - t200 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t90 *
     # (-t32 + t198 * t55) - t118) * t171 / 0.720D3
      t216 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t215)
      t218 = 0.1D1 - x1
      t219 = 0.1D1 - x3
      t220 = KAPPA2(t218, x2, t219, 0.10D1, z)
      t221 = s * t220
      t222 = -t218
      t223 = t1 * t222
      t224 = -t219
      t225 = t223 * t224
      t227 = t223 * x3
      t229 = t1 * x1
      t231 = t220 ** 2
      t234 = t222 * x1
      t238 = 0.1D1 / (-0.2D1 + t220)
      t239 = t31 * t238
      t241 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, t219, 0.
     #10D1)
      t246 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, t219, 0.
     #10D1)
      t249 = t222 ** 2
      t251 = t231 ** 2
      t256 = log(-0.4D1 * t181 * t14 * t16 * t249 * t224 * t251)
      t263 = t59 * t27
      t271 = -t36 * t239 * t241 * t96 * t177 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t238 * t246 - t256 * t238 * t241) - 0.180D3 * t263 * t239 * 
     #t241) * t96 * t171 / 0.720D3
      t272 = FJET(XB1, XB2, s, t221 * t225, -t221 * t227, t221 * t229, 0
     #.0D0, -s * t231 * t15 * t234 * x3, t271)
      t274 = KAPPA2(t218, x2, t219, t68, z)
      t275 = s * t274
      t278 = t229 * t73
      t280 = t229 * x4
      t282 = t274 ** 2
      t287 = cos(t9)
      t291 = Sqrt(x3 * t224 * x4 * t73)
      t298 = 0.1D1 / (-0.2D1 + t274)
      t301 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, t219, t6
     #8)
      t306 = FJET(XB1, XB2, s, t275 * t225, -t275 * t227, -t275 * t278, 
     #t275 * t280, s * t282 * t15 * t234 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t287 * t291), t36 * t31 * t298 * t301 * t96 * t177 / 0.8D
     #1)
      t317 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.10D1, 
     #0.10D1)
      t322 = log(0.4D1 * t17 * t151 * t249 * x4)
      t323 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.10D1, 
     #0.10D1)
      t329 = t90 * t323
      t331 = 0.180D3 * t59 * t329
      t341 = t16 * t151 * t249
      t344 = log(0.4D1 * t71 * t341)
      t356 = log(0.4D1 * t14 * t341)
      t358 = t356 ** 2
      t361 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.10D1, 
     #0.10D1)
      t375 = (0.90D2 * t36 * t31 * (-t317 + t322 * t323) + t331) * t171 
     #* t98 / 0.720D3 - t175 * t323 * t96 * t177 / 0.8D1 - (0.90D2 * t36
     # * t31 * (t317 - t344 * t323) - t331) * t96 * t171 / 0.720D3 - (0.
     #90D2 * t36 * t31 * (-t356 * t317 + t358 * t323 / 0.2D1 + t361) - 0
     #.180D3 * t59 * t90 * (t317 - t356 * t323) + t8 * t329) * t171 / 0.
     #720D3
      t376 = FJET(XB1, XB2, s, -t2 * t222, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t375)
      t380 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t219, 
     #t68)
      t381 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t219, 
     #0.10D1)
      t382 = -t380 + t381
      t387 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t219, 
     #0.10D1)
      t389 = t80 * t224
      t392 = log(-0.4D1 * t181 * t11 * t389)
      t398 = t90 * t381
      t409 = log(-0.4D1 * t71 * t16 * t224 * x4)
      t411 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t219, 
     #t68)
      t416 = log(0.4D1 * t71 * t72 * t73 * t224)
      t431 = log(-0.4D1 * t70 * t389)
      t433 = t431 ** 2
      t436 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t219, 
     #0.10D1)
      t450 = -t175 * t382 * t96 * t177 / 0.8D1 - (0.90D2 * t36 * t31 * (
     #t387 - t392 * t381) - 0.180D3 * t59 * t398) * t96 * t171 / 0.720D3
     # - (0.90D2 * t36 * t31 * (t387 - t409 * t381 - t411 + t416 * t380)
     # - 0.180D3 * t59 * t90 * t382) * t96 * t98 / 0.1440D4 - (0.90D2 * 
     #t36 * t31 * (-t431 * t387 + t433 * t381 / 0.2D1 + t436) - 0.180D3 
     #* t59 * t90 * (t387 - t431 * t381) + t8 * t398) * t96 / 0.1440D4
      t451 = FJET(XB1, XB2, s, -t2 * t224, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t450)
      t453 = KAPPA2(t218, x2, 0.10D1, t68, z)
      t454 = s * t453
      t458 = t453 ** 2
      t464 = 0.1D1 / (-0.2D1 + t453)
      t465 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.10D1, 
     #t68)
      t468 = t458 ** 2
      t473 = log(-0.4D1 * t196 * t249 * x4 * t73 * t468)
      t475 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.10D1, 
     #t68)
      t481 = t31 * t464
      t494 = (-0.90D2 * t36 * t31 * (t464 * t465 - t473 * t464 * t475) +
     # 0.180D3 * t263 * t481 * t475) * t171 * t98 / 0.720D3 - t36 * t481
     # * t475 * t96 * t177 / 0.8D1
      t495 = FJET(XB1, XB2, s, -t454 * t223, 0.0D0, -t454 * t278, t454 *
     # t280, -s * t458 * t15 * t234 * x4, t494)
      rrgq2qght5s1e0 = t216 * t215 + t272 * t271 + t306 * 0.314159265358
     #9793D1 * t90 * t298 * t301 * t96 * t171 * t98 / 0.8D1 + t376 * t37
     #5 + t451 * t450 + t495 * t494

      end function



      doubleprecision function rrgq2qght5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t50 = t45 * t23
      t51 = 0.1D1 - x4
      t52 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t51)
      t53 = -t41 + t52
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t7
      t61 = t9 * t12
      t64 = log(0.4D1 * t60 * t61)
      t70 = t19 * t23
      t73 = 0.180D3 * t3 * t70 * t41
      t77 = t12 * x4
      t80 = log(0.4D1 * t10 * t77)
      t82 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t51)
      t83 = -t51
      t87 = log(-0.4D1 * t10 * t77 * t83)
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t108 = log(0.4D1 * t104 * t7 * t61)
      t122 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 - t50 * t53 * t54 * t56 
     #/ 0.16D2 - (0.90D2 * t45 * t23 * (-t24 + t64 * t41) + t73) * t54 /
     # 0.1440D4 - (0.90D2 * t45 * t23 * (-t24 + t80 * t41 + t82 - t87 * 
     #t52) - 0.180D3 * t3 * t70 * t53) * t56 / 0.1440D4 + t50 * t41 * t5
     #4 * t100 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t108 * t41) + t73
     #) * t100 / 0.720D3 - t50 * t53 * t100 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t122)
      t125 = 0.1D1 - x1
      t126 = 0.1D1 - x3
      t127 = KAPPA2(t125, x2, t126, 0.10D1, z)
      t128 = s * t127
      t129 = -t125
      t130 = t1 * t129
      t131 = -t126
      t136 = t1 * x1
      t138 = t127 ** 2
      t141 = t129 * x1
      t146 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, t126, 0.
     #10D1)
      t149 = 0.1D1 / (-0.2D1 + t127) * t146 * t54 * t100
      t152 = FJET(XB1, XB2, s, t128 * t130 * t131, -t128 * t130 * x3, t1
     #28 * t136, 0.0D0, -s * t138 * t11 * t141 * x3, -t50 * t149 / 0.8D1
     #)
      t159 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.10D1, 
     #0.10D1)
      t164 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.10D1, 
     #0.10D1)
      t166 = t129 ** 2
      t170 = log(0.4D1 * t10 * t12 * t104 * t166)
      t186 = -t50 * t159 * t54 * t100 / 0.8D1 - (0.90D2 * t45 * t23 * (t
     #164 - t170 * t159) - 0.180D3 * t3 * t70 * t159) * t100 / 0.720D3 -
     # t50 * t159 * t100 * t56 / 0.8D1
      t187 = FJET(XB1, XB2, s, -t2 * t129, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t186)
      t191 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t126, 
     #0.10D1)
      t196 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t126, 
     #t51)
      t202 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t126, 
     #0.10D1)
      t206 = log(-0.4D1 * t60 * t61 * t131)
      t218 = -t50 * t191 * t54 * t100 / 0.8D1 - t50 * (-t196 + t191) * t
     #54 * t56 / 0.16D2 - (0.90D2 * t45 * t23 * (t202 - t206 * t191) - 0
     #.180D3 * t3 * t70 * t191) * t54 / 0.1440D4
      t219 = FJET(XB1, XB2, s, -t2 * t131, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t218)
      t221 = KAPPA2(t125, x2, 0.10D1, t51, z)
      t222 = s * t221
      t228 = t221 ** 2
      t235 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.10D1, 
     #t51)
      t238 = 0.1D1 / (-0.2D1 + t221) * t235 * t100 * t56
      t241 = FJET(XB1, XB2, s, -t222 * t130, 0.0D0, -t222 * t136 * t83, 
     #t222 * t136 * x4, -s * t228 * t11 * t141 * x4, -t50 * t238 / 0.8D1
     #)
      rrgq2qght5s1em1 = t123 * t122 - t152 * 0.3141592653589793D1 * t70 
     #* t149 / 0.8D1 + t187 * t186 + t219 * t218 - t241 * 0.314159265358
     #9793D1 * t70 * t238 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t10 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.1D1 - x4)
      t17 = t7 * t8
      t18 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t26 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = -t4 * t7 * (-t8 + t10) / x4 / 0.16D2 + t4 * t17 * t18 / 0.16
     #D2 + t4 * t17 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 *
     # 0.3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) 
     #* t3 * t17 / 0.1440D4
      t50 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t52 = -0.1D1 + x3
      t56 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t52, 0
     #.10D1)
      t58 = t7 * t56 * t18
      t61 = FJET(XB1, XB2, s, -t2 * t52, t2 * x3, 0.0D0, 0.0D0, 0.0D0, -
     #t4 * t58 / 0.16D2)
      t66 = -0.1D1 + x1
      t70 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, -t66, x2, 0.10D1, 0
     #.10D1)
      t72 = t7 * t70 * t22
      t75 = FJET(XB1, XB2, s, -t2 * t66, 0.0D0, t2 * x1, 0.0D0, 0.0D0, -
     #t4 * t72 / 0.8D1)
      rrgq2qght5s1em2 = t50 * t49 - t61 * 0.3141592653589793D1 * t3 * t5
     #8 / 0.16D2 - t75 * 0.3141592653589793D1 * t3 * t72 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t12 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s1em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t91 * t15
      t93 = t18 * x4
      t94 = -0.1D1 + x4
      t95 = t93 * t94
      t98 = log(-0.4D1 * t92 * t95)
      t99 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t101 = t98 ** 2
      t102 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t105 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t106 = t15 * t18
      t107 = t106 * x4
      t110 = log(0.4D1 * t91 * t107)
      t112 = t110 ** 2
      t119 = t35 * t39
      t126 = -t102 + t86
      t127 = t119 * t126
      t130 = 0.1D1 / x1
      t132 = 0.1D1 / x4
      t135 = t91 * t106
      t137 = log(0.4D1 * t135)
      t142 = t137 ** 2
      t153 = t119 * t86
      t154 = t10 * t153
      t165 = x3 * t90
      t166 = t165 * t13
      t169 = log(0.4D1 * t166 * t107)
      t171 = x4 * t94
      t175 = log(-0.4D1 * t166 * t106 * t171)
      t184 = 0.1D1 / x3
      t186 = t130 * t132
      t189 = t165 * t19
      t191 = log(0.4D1 * t189)
      t193 = t191 ** 2
      t212 = log(0.4D1 * t16 * t93)
      t216 = log(-0.4D1 * t16 * t95)
      t221 = t216 ** 2
      t224 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t229 = t212 ** 2
      t241 = -t119 * t126
      t256 = x3 * t13
      t259 = log(0.4D1 * t256 * t107)
      t261 = t259 ** 2
      t264 = t256 * t15
      t267 = log(-0.4D1 * t264 * t95)
      t269 = t267 ** 2
      t289 = log(0.4D1 * t256 * t106)
      t294 = t289 ** 2
      t315 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (t98 * t99 - t101 * t102 / 0.2D
     #1 - t105 - t110 * t40 + t112 * t86 / 0.2D1 + t50) - 0.180D3 * t54 
     #* t119 * (-t99 + t98 * t102 + t40 - t110 * t86) + t44 * t127) * t1
     #30 * t132 / 0.720D3 - (t44 * t119 * (-t40 + t137 * t86) + 0.90D2 *
     # t63 * t39 * (-t142 * t40 / 0.2D1 - t59 + t142 * t137 * t86 / 0.6D
     #1 + t137 * t50) - t154 - 0.180D3 * t54 * t119 * (t137 * t40 - t142
     # * t86 / 0.2D1 - t50)) * t130 / 0.720D3 + (0.90D2 * t63 * t39 * (t
     #40 - t169 * t86 - t99 + t175 * t102) - 0.180D3 * t54 * t127) * t18
     #4 * t186 / 0.720D3 - (0.90D2 * t63 * t39 * (t191 * t40 - t193 * t8
     #6 / 0.2D1 - t50) - 0.180D3 * t54 * t119 * (-t40 + t191 * t86) - t4
     #4 * t153) * t184 * t130 / 0.720D3 - (t44 * t119 * (-t40 + t212 * t
     #86 + t99 - t216 * t102) + 0.90D2 * t63 * t39 * (t221 * t99 / 0.2D1
     # + t224 - t221 * t216 * t102 / 0.6D1 - t216 * t105 - t229 * t40 / 
     #0.2D1 - t59 + t229 * t212 * t86 / 0.6D1 + t212 * t50) + t10 * t241
     # - 0.180D3 * t54 * t119 * (t212 * t40 - t229 * t86 / 0.2D1 - t50 -
     # t216 * t99 + t221 * t102 / 0.2D1 + t105)) * t132 / 0.1440D4 - (0.
     #90D2 * t63 * t39 * (t259 * t40 - t261 * t86 / 0.2D1 - t50 - t267 *
     # t99 + t269 * t102 / 0.2D1 + t105) - 0.180D3 * t54 * t119 * (-t40 
     #+ t259 * t86 + t99 - t267 * t102) + t44 * t241) * t184 * t132 / 0.
     #1440D4 + (t44 * t119 * (t40 - t289 * t86) + 0.90D2 * t63 * t39 * (
     #t294 * t40 / 0.2D1 + t59 - t294 * t289 * t86 / 0.6D1 - t289 * t50)
     # + t154 - 0.180D3 * t54 * t119 * (-t289 * t40 + t294 * t86 / 0.2D1
     # + t50)) * t184 / 0.1440D4
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
      t339 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #0D0)
      t340 = t338 * t339
      t341 = t165 * t16
      t342 = t322 ** 2
      t343 = t18 * t342
      t344 = t324 * x4
      t345 = t331 ** 2
      t350 = log(-0.4D1 * t341 * t343 * t344 * t345)
      t352 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #0D0)
      t358 = t54 * t35
      t360 = t39 * t338 * t352
      t370 = log(-0.4D1 * t341 * t343 * t324 * t345)
      t371 = t370 * t338
      t373 = t370 ** 2
      t377 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, 0.
     #0D0)
      t388 = t44 * t35
      t394 = (0.90D2 * t63 * t39 * (-t340 + t350 * t338 * t352) + 0.180D
     #3 * t358 * t360) * t184 * t186 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t371 * t339 + t373 * t338 * t352 / 0.2D1 + t338 * t377) - 0.180D3
     # * t54 * t119 * (t340 - t371 * t352) + t388 * t360) * t184 * t130 
     #/ 0.720D3
      t395 = FJET(XB1, XB2, s, t321 * t325, -t321 * t327, 0.0D0, t321 * 
     #t329, s * t331 * t17 * t334 * t324, t394)
      t397 = KAPPA2(t318, x2, t319, x4, z)
      t398 = s * t397
      t401 = t329 * x4
      t403 = t329 * t94
      t405 = t397 ** 2
      t410 = cos(t11)
      t413 = Sqrt(x3 * t324 * t171)
      t420 = 0.1D1 / (-0.2D1 + t397)
      t421 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, x4
     #)
      t424 = t405 ** 2
      t429 = log(0.4D1 * t189 * t342 * t324 * t171 * t424)
      t431 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, t319, x4
     #)
      t441 = 0.90D2 * t63 * t39 * (t420 * t421 - t429 * t420 * t431) - 0
     #.180D3 * t358 * t39 * t420 * t431
      t445 = FJET(XB1, XB2, s, t398 * t325, -t398 * t327, t398 * t401, -
     #t398 * t403, s * t405 * t17 * t334 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t410 * t413), t441 * t184 * t186 / 0.720D3)
      t454 = t93 * t94 * t324
      t457 = log(0.4D1 * t264 * t454)
      t458 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #x4)
      t460 = t457 ** 2
      t461 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #x4)
      t464 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #x4)
      t469 = log(-0.4D1 * t264 * t18 * t324 * x4)
      t470 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t472 = t469 ** 2
      t473 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t476 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t487 = t473 - t461
      t494 = t106 * t324
      t497 = log(-0.4D1 * t256 * t494)
      t502 = t497 ** 2
      t505 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t319, 
     #0.0D0)
      t514 = t119 * t473
      t529 = log(-0.4D1 * t166 * t106 * t344)
      t533 = log(0.4D1 * t341 * t454)
      t549 = log(-0.4D1 * t166 * t494)
      t551 = t549 ** 2
      t568 = -(0.90D2 * t63 * t39 * (t457 * t458 - t460 * t461 / 0.2D1 -
     # t464 - t469 * t470 + t472 * t473 / 0.2D1 + t476) - 0.180D3 * t54 
     #* t119 * (-t458 + t457 * t461 + t470 - t469 * t473) + t44 * t119 *
     # t487) * t184 * t132 / 0.1440D4 + (-t44 * t119 * (t470 - t497 * t4
     #73) - 0.90D2 * t63 * t39 * (t502 * t470 / 0.2D1 + t505 - t502 * t4
     #97 * t473 / 0.6D1 - t497 * t476) - t10 * t514 + 0.180D3 * t54 * t1
     #19 * (-t497 * t470 + t502 * t473 / 0.2D1 + t476)) * t184 / 0.1440D
     #4 + (0.90D2 * t63 * t39 * (-t470 + t529 * t473 + t458 - t533 * t46
     #1) + 0.180D3 * t54 * t119 * t487) * t184 * t186 / 0.720D3 - (0.90D
     #2 * t63 * t39 * (-t549 * t470 + t551 * t473 / 0.2D1 + t476) - 0.18
     #0D3 * t54 * t119 * (t470 - t549 * t473) + t44 * t514) * t184 * t13
     #0 / 0.720D3
      t569 = FJET(XB1, XB2, s, -t2 * t324, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t568)
      t571 = KAPPA2(t318, x2, 0.10D1, 0.0D0, z)
      t572 = s * t571
      t575 = t571 ** 2
      t580 = t575 ** 2
      t582 = t343 * x4 * t580
      t585 = log(0.4D1 * t92 * t582)
      t587 = 0.1D1 / (-0.2D1 + t571)
      t588 = t585 * t587
      t589 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t591 = t585 ** 2
      t593 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t596 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t597 = t587 * t596
      t602 = t587 * t589
      t609 = t39 * t587 * t593
      t610 = t388 * t609
      t617 = log(0.4D1 * t92 * t343 * t580)
      t618 = t617 * t587
      t623 = t617 ** 2
      t624 = t623 * t587
      t627 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #0.0D0)
      t651 = log(0.4D1 * t341 * t582)
      t667 = log(0.4D1 * t166 * t106 * t342 * t580)
      t668 = t667 * t587
      t670 = t667 ** 2
      t687 = (0.90D2 * t63 * t39 * (-t588 * t589 + t591 * t587 * t593 / 
     #0.2D1 + t597) - 0.180D3 * t54 * t119 * (t602 - t588 * t593) + t610
     #) * t130 * t132 / 0.720D3 - (-t44 * t119 * (t602 - t618 * t593) - 
     #0.90D2 * t63 * t39 * (t624 * t589 / 0.2D1 + t587 * t627 - t623 * t
     #617 * t587 * t593 / 0.6D1 - t618 * t596) - t10 * t35 * t609 + 0.18
     #0D3 * t54 * t119 * (-t618 * t589 + t624 * t593 / 0.2D1 + t597)) * 
     #t130 / 0.720D3 + (0.90D2 * t63 * t39 * (t602 - t651 * t587 * t593)
     # - 0.180D3 * t358 * t609) * t184 * t186 / 0.720D3 - (-0.90D2 * t63
     # * t39 * (-t668 * t589 + t670 * t587 * t593 / 0.2D1 + t597) + 0.18
     #0D3 * t54 * t119 * (t602 - t668 * t593) - t610) * t184 * t130 / 0.
     #720D3
      t688 = FJET(XB1, XB2, s, -t572 * t323, 0.0D0, 0.0D0, t572 * t329, 
     #-s * t575 * t17 * t322 * x1, t687)
      t690 = KAPPA2(t318, x2, 0.10D1, x4, z)
      t691 = s * t690
      t695 = t690 ** 2
      t701 = t695 ** 2
      t706 = log(-0.4D1 * t135 * t342 * x4 * t94 * t701)
      t708 = 0.1D1 / (-0.2D1 + t690)
      t709 = t706 * t708
      t710 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #x4)
      t712 = t706 ** 2
      t714 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #x4)
      t717 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t318, x2, 0.10D1, 
     #x4)
      t723 = t708 * t710
      t730 = t39 * t708 * t714
      t739 = log(-0.4D1 * t341 * t343 * t171 * t701)
      t752 = (-0.90D2 * t63 * t39 * (-t709 * t710 + t712 * t708 * t714 /
     # 0.2D1 + t708 * t717) + 0.180D3 * t54 * t119 * (t723 - t709 * t714
     #) - t388 * t730) * t130 * t132 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(t723 - t739 * t708 * t714) + 0.180D3 * t358 * t730) * t184 * t186
     # / 0.720D3
      t753 = FJET(XB1, XB2, s, -t691 * t323, 0.0D0, t691 * t401, -t691 *
     # t403, s * t695 * t17 * t334 * t94, t752)
      rrgq2qght5s2e1 = t316 * t315 + t395 * t394 + t445 * t441 * t184 * 
     #t130 * t132 / 0.720D3 + t569 * t568 + t688 * t687 + t753 * t752

      end function



      doubleprecision function rrgq2qght5s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t76 = t68 * t13
      t77 = t16 * x4
      t78 = -0.1D1 + x4
      t79 = t77 * t78
      t82 = log(-0.4D1 * t76 * t79)
      t83 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
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
      t135 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t150 = x1 ** 2
      t151 = t150 * t11
      t152 = t151 * t13
      t155 = log(-0.4D1 * t152 * t79)
      t159 = log(0.4D1 * t151 * t70)
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
     #40D4 + (0.90D2 * t36 * t31 * (-t75 + t155 * t83 + t32 - t159 * t55
     #) - 0.180D3 * t59 * t89 * t165) * t170 * t97 / 0.720D3 + t174 * t1
     #65 * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t31 * (-t32 + t183 * t55
     #) + 0.180D3 * t59 * t116) * t95 * t170 / 0.720D3 - (0.90D2 * t36 *
     # t31 * (t197 * t32 - t199 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t
     #89 * (-t32 + t197 * t55) - t117) * t170 / 0.720D3
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
      t240 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, t218, 0.
     #0D0)
      t245 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t217, x2, t218, 0.
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
      t300 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, t218, x4
     #)
      t305 = FJET(XB1, XB2, s, t274 * t224, -t274 * t226, t274 * t277, -
     #t274 * t279, s * t281 * t15 * t233 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t286 * t290), t36 * t31 * t297 * t300 * t95 * t17
     #6 / 0.8D1)
      t316 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #0.0D0)
      t317 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #x4)
      t318 = -t316 + t317
      t323 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #0.0D0)
      t324 = t180 * t11
      t325 = t69 * t223
      t328 = log(-0.4D1 * t324 * t325)
      t334 = t89 * t316
      t341 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #x4)
      t346 = log(0.4D1 * t76 * t77 * t78 * t223)
      t352 = log(-0.4D1 * t76 * t16 * t223 * x4)
      t368 = log(-0.4D1 * t68 * t325)
      t370 = t368 ** 2
      t373 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t218, 
     #0.0D0)
      t387 = t174 * t318 * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t31 * (t
     #323 - t328 * t316) - 0.180D3 * t59 * t334) * t95 * t170 / 0.720D3 
     #- (0.90D2 * t36 * t31 * (-t341 + t346 * t317 + t323 - t352 * t316)
     # + 0.180D3 * t59 * t89 * t318) * t95 * t97 / 0.1440D4 + (-0.90D2 *
     # t36 * t31 * (-t368 * t323 + t370 * t316 / 0.2D1 + t373) + 0.180D3
     # * t59 * t89 * (t323 - t368 * t316) - t8 * t334) * t95 / 0.1440D4
      t388 = FJET(XB1, XB2, s, -t2 * t223, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t387)
      t390 = KAPPA2(t217, x2, 0.10D1, 0.0D0, z)
      t391 = s * t390
      t394 = t390 ** 2
      t400 = 0.1D1 / (-0.2D1 + t390)
      t401 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #0.0D0)
      t402 = t400 * t401
      t403 = t394 ** 2
      t408 = log(0.4D1 * t152 * t249 * x4 * t403)
      t410 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #0.0D0)
      t416 = t31 * t400
      t417 = t416 * t410
      t419 = 0.180D3 * t262 * t417
      t433 = log(0.4D1 * t324 * t69 * t248 * t403)
      t447 = log(0.4D1 * t152 * t249 * t403)
      t448 = t447 * t400
      t450 = t447 ** 2
      t454 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #0.0D0)
      t470 = (0.90D2 * t36 * t31 * (t402 - t408 * t400 * t410) - t419) *
     # t170 * t97 / 0.720D3 + t36 * t416 * t410 * t95 * t176 / 0.8D1 - (
     #-0.90D2 * t36 * t31 * (t402 - t433 * t400 * t410) + t419) * t95 * 
     #t170 / 0.720D3 - (-0.90D2 * t36 * t31 * (-t448 * t401 + t450 * t40
     #0 * t410 / 0.2D1 + t400 * t454) + 0.180D3 * t59 * t89 * (t402 - t4
     #48 * t410) - t8 * t27 * t417) * t170 / 0.720D3
      t471 = FJET(XB1, XB2, s, -t391 * t222, 0.0D0, 0.0D0, t391 * t228, 
     #-s * t394 * t15 * t221 * x1, t470)
      t473 = KAPPA2(t217, x2, 0.10D1, x4, z)
      t474 = s * t473
      t478 = t473 ** 2
      t484 = 0.1D1 / (-0.2D1 + t473)
      t485 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #x4)
      t488 = t478 ** 2
      t493 = log(-0.4D1 * t195 * t248 * x4 * t78 * t488)
      t495 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t217, x2, 0.10D1, 
     #x4)
      t501 = t31 * t484
      t514 = (-0.90D2 * t36 * t31 * (t484 * t485 - t493 * t484 * t495) +
     # 0.180D3 * t262 * t501 * t495) * t170 * t97 / 0.720D3 - t36 * t501
     # * t495 * t95 * t176 / 0.8D1
      t515 = FJET(XB1, XB2, s, -t474 * t222, 0.0D0, t474 * t277, -t474 *
     # t279, s * t478 * t15 * t233 * t78, t514)
      rrgq2qght5s2e0 = t215 * t214 + t271 * t270 + t305 * 0.314159265358
     #9793D1 * t89 * t297 * t300 * t95 * t170 * t97 / 0.8D1 + t388 * t38
     #7 + t471 * t470 + t515 * t514

      end function



      doubleprecision function rrgq2qght5s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t50 = t45 * t23
      t51 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
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
      t81 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
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
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 - t50 * t52 * t53 * t55 
     #/ 0.16D2 + (0.90D2 * t45 * t23 * (t24 - t63 * t41) - t72) * t53 / 
     #0.1440D4 - (0.90D2 * t45 * t23 * (-t24 + t79 * t41 + t81 - t86 * t
     #51) - 0.180D3 * t3 * t69 * t52) * t55 / 0.1440D4 + t50 * t41 * t53
     # * t99 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t107 * t41) + t72) 
     #* t99 / 0.720D3 - t50 * t52 * t99 * t55 / 0.8D1
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
      t145 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, t125, 0.
     #0D0)
      t147 = t53 * t99
      t148 = 0.1D1 / (-0.2D1 + t126) * t145 * t147
      t151 = FJET(XB1, XB2, s, t127 * t129 * t130, -t127 * t129 * x3, 0.
     #0D0, t127 * t135, s * t137 * t11 * t140 * t130, -t50 * t148 / 0.8D
     #1)
      t158 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.0D0)
      t163 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #x4)
      t169 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.0D0)
      t173 = log(-0.4D1 * t59 * t60 * t130)
      t185 = -t50 * t158 * t53 * t99 / 0.8D1 - t50 * (t158 - t163) * t53
     # * t55 / 0.16D2 + (-0.90D2 * t45 * t23 * (t169 - t173 * t158) + 0.
     #180D3 * t3 * t69 * t158) * t53 / 0.1440D4
      t186 = FJET(XB1, XB2, s, -t2 * t130, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t185)
      t188 = KAPPA2(t124, x2, 0.10D1, 0.0D0, z)
      t189 = s * t188
      t192 = t188 ** 2
      t198 = 0.1D1 / (-0.2D1 + t188)
      t199 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.0D0)
      t200 = t198 * t199
      t204 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
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
      t249 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #x4)
      t251 = 0.1D1 / (-0.2D1 + t235) * t249 * t228
      t254 = FJET(XB1, XB2, s, -t236 * t129, 0.0D0, t236 * t135 * x4, -t
     #236 * t135 * t82, s * t242 * t11 * t140 * t82, -t50 * t251 / 0.8D1
     #)
      rrgq2qght5s2em1 = t122 * t121 - t151 * 0.3141592653589793D1 * t69 
     #* t148 / 0.8D1 + t186 * t185 + t233 * t232 - t254 * 0.314159265358
     #9793D1 * t69 * t251 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #x4)
      t16 = t7 * t8
      t17 = 0.1D1 / x3
      t21 = 0.1D1 / x1
      t25 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
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
      t49 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t51 = -0.1D1 + x3
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t51, 0
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
      t81 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t65, x2, 0.10D1, 0.
     #0D0)
      t86 = FJET(XB1, XB2, s, -t67 * t1 * t68, 0.0D0, 0.0D0, t67 * t1 * 
     #x1, -s * t73 * t37 * t68 * x1, t4 * t7 * t80 * t81 * t21 / 0.8D1)
      rrgq2qght5s2em2 = t49 * t48 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t86 * 0.3141592653589793D1 * t3 * t7 * t80 * t81 * t2
     #1 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t12 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s2em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s2em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t90 = x1 ** 2
      t91 = t90 * t13
      t92 = t91 * t15
      t93 = t18 * x4
      t94 = -0.1D1 + x4
      t95 = t93 * t94
      t98 = log(-0.4D1 * t92 * t95)
      t99 = -t94
      t100 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t102 = t98 ** 2
      t103 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t106 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t107 = t15 * t18
      t108 = t107 * x4
      t111 = log(0.4D1 * t91 * t108)
      t113 = t111 ** 2
      t120 = t35 * t39
      t127 = t103 - t86
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
      t170 = log(0.4D1 * t167 * t108)
      t172 = x4 * t94
      t176 = log(-0.4D1 * t167 * t107 * t172)
      t185 = 0.1D1 / x3
      t187 = t131 * t133
      t190 = t166 * t19
      t192 = log(0.4D1 * t190)
      t194 = t192 ** 2
      t213 = log(-0.4D1 * t16 * t95)
      t217 = log(0.4D1 * t16 * t93)
      t222 = t217 ** 2
      t229 = t213 ** 2
      t232 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t99)
      t255 = x3 * t13
      t256 = t255 * t15
      t259 = log(-0.4D1 * t256 * t95)
      t261 = t259 ** 2
      t266 = log(0.4D1 * t255 * t108)
      t268 = t266 ** 2
      t290 = log(0.4D1 * t255 * t107)
      t295 = t290 ** 2
      t316 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 - (0.90D2 * t63 * t39 * (-t98 * t100 + t102 * t103 / 0.
     #2D1 + t106 + t111 * t40 - t113 * t86 / 0.2D1 - t50) - 0.180D3 * t5
     #4 * t120 * (t100 - t98 * t103 - t40 + t111 * t86) + t44 * t128) * 
     #t131 * t133 / 0.720D3 + (t44 * t120 * (t40 - t138 * t86) + 0.90D2 
     #* t63 * t39 * (t143 * t40 / 0.2D1 + t59 - t143 * t138 * t86 / 0.6D
     #1 - t138 * t50) + t155 - 0.180D3 * t54 * t120 * (-t138 * t40 + t14
     #3 * t86 / 0.2D1 + t50)) * t131 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t40 + t170 * t86 + t100 - t176 * t103) - 0.180D3 * t54 * t128) * 
     #t185 * t187 / 0.720D3 + (0.90D2 * t63 * t39 * (-t192 * t40 + t194 
     #* t86 / 0.2D1 + t50) - 0.180D3 * t54 * t120 * (t40 - t192 * t86) +
     # t44 * t154) * t185 * t131 / 0.720D3 - (t44 * t120 * (t100 - t213 
     #* t103 - t40 + t217 * t86) + 0.90D2 * t63 * t39 * (-t222 * t40 / 0
     #.2D1 - t59 + t222 * t217 * t86 / 0.6D1 + t217 * t50 + t229 * t100 
     #/ 0.2D1 + t232 - t229 * t213 * t103 / 0.6D1 - t213 * t106) + t10 *
     # t128 - 0.180D3 * t54 * t120 * (-t213 * t100 + t229 * t103 / 0.2D1
     # + t106 + t217 * t40 - t222 * t86 / 0.2D1 - t50)) * t133 / 0.1440D
     #4 + (0.90D2 * t63 * t39 * (t259 * t100 - t261 * t103 / 0.2D1 - t10
     #6 - t266 * t40 + t268 * t86 / 0.2D1 + t50) - 0.180D3 * t54 * t120 
     #* (-t100 + t259 * t103 + t40 - t266 * t86) - t44 * t120 * t127) * 
     #t185 * t133 / 0.1440D4 - (t44 * t120 * (-t40 + t290 * t86) + 0.90D
     #2 * t63 * t39 * (-t295 * t40 / 0.2D1 - t59 + t295 * t290 * t86 / 0
     #.6D1 + t290 * t50) - t155 - 0.180D3 * t54 * t120 * (t290 * t40 - t
     #295 * t86 / 0.2D1 - t50)) * t185 / 0.1440D4
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
      t343 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t345 = t339 ** 2
      t347 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t350 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
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
      t382 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, 0
     #.10D1)
      t404 = t166 * t16
      t407 = log(0.4D1 * t404 * t336)
      t414 = t54 * t35
      t424 = log(0.4D1 * t167 * t107 * t332 * t334)
      t425 = t424 * t341
      t427 = t424 ** 2
      t444 = -(0.90D2 * t63 * t39 * (t342 * t343 - t345 * t341 * t347 / 
     #0.2D1 - t351) - 0.180D3 * t54 * t120 * (-t356 + t342 * t347) - t36
     #5) * t131 * t133 / 0.720D3 + (t44 * t120 * (t356 - t373 * t347) + 
     #0.90D2 * t63 * t39 * (t379 * t343 / 0.2D1 + t341 * t382 - t378 * t
     #372 * t341 * t347 / 0.6D1 - t373 * t350) + t10 * t35 * t364 - 0.18
     #0D3 * t54 * t120 * (-t373 * t343 + t379 * t347 / 0.2D1 + t351)) * 
     #t131 / 0.720D3 - (0.90D2 * t63 * t39 * (-t356 + t407 * t341 * t347
     #) + 0.180D3 * t414 * t364) * t185 * t187 / 0.720D3 + (0.90D2 * t63
     # * t39 * (-t425 * t343 + t427 * t341 * t347 / 0.2D1 + t351) - 0.18
     #0D3 * t54 * t120 * (t356 - t425 * t347) + t365) * t185 * t131 / 0.
     #720D3
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
      t470 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, t
     #99)
      t472 = t466 ** 2
      t474 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, t
     #99)
      t477 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, 0.0D0, t
     #99)
      t483 = t468 * t470
      t490 = t39 * t468 * t474
      t499 = log(-0.4D1 * t404 * t333 * t172 * t461)
      t512 = -(0.90D2 * t63 * t39 * (-t469 * t470 + t472 * t468 * t474 /
     # 0.2D1 + t468 * t477) - 0.180D3 * t54 * t120 * (t483 - t469 * t474
     #) + t362 * t490) * t131 * t133 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t483 - t499 * t468 * t474) - 0.180D3 * t414 * t490) * t185 * t187 
     #/ 0.720D3
      t513 = FJET(XB1, XB2, s, 0.0D0, -t448 * t323, -t448 * t450, t448 *
     # t452, s * t454 * t17 * t457 * t94, t512)
      t516 = -0.1D1 + x3
      t519 = t93 * t94 * t516
      t522 = log(0.4D1 * t256 * t519)
      t523 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t9
     #9)
      t525 = t522 ** 2
      t526 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t9
     #9)
      t529 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t9
     #9)
      t534 = log(-0.4D1 * t256 * t18 * t516 * x4)
      t535 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t537 = t534 ** 2
      t538 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t541 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t552 = -t538 + t526
      t559 = t107 * t516
      t562 = log(-0.4D1 * t255 * t559)
      t567 = t562 ** 2
      t570 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t579 = t120 * t538
      t591 = t516 * x4
      t595 = log(-0.4D1 * t167 * t107 * t591)
      t599 = log(0.4D1 * t404 * t519)
      t615 = log(-0.4D1 * t167 * t559)
      t617 = t615 ** 2
      t634 = (0.90D2 * t63 * t39 * (-t522 * t523 + t525 * t526 / 0.2D1 +
     # t529 + t534 * t535 - t537 * t538 / 0.2D1 - t541) - 0.180D3 * t54 
     #* t120 * (t523 - t522 * t526 - t535 + t534 * t538) + t44 * t120 * 
     #t552) * t185 * t133 / 0.1440D4 - (t44 * t120 * (t535 - t562 * t538
     #) + 0.90D2 * t63 * t39 * (t567 * t535 / 0.2D1 + t570 - t567 * t562
     # * t538 / 0.6D1 - t562 * t541) + t10 * t579 - 0.180D3 * t54 * t120
     # * (-t562 * t535 + t567 * t538 / 0.2D1 + t541)) * t185 / 0.1440D4 
     #- (0.90D2 * t63 * t39 * (t535 - t595 * t538 - t523 + t599 * t526) 
     #+ 0.180D3 * t54 * t120 * t552) * t185 * t187 / 0.720D3 + (0.90D2 *
     # t63 * t39 * (t615 * t535 - t617 * t538 / 0.2D1 - t541) - 0.180D3 
     #* t54 * t120 * (-t535 + t615 * t538) - t44 * t579) * t185 * t131 /
     # 0.720D3
      t635 = FJET(XB1, XB2, s, t2 * x3, -t2 * t516, 0.0D0, 0.0D0, 0.0D0,
     # t634)
      t637 = KAPPA2(t319, x2, x3, 0.10D1, z)
      t638 = s * t637
      t639 = t323 * x3
      t641 = t323 * t516
      t644 = t637 ** 2
      t650 = 0.1D1 / (-0.2D1 + t637)
      t651 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, 0.10
     #D1)
      t652 = t650 * t651
      t653 = t644 ** 2
      t658 = log(-0.4D1 * t404 * t333 * t591 * t653)
      t660 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, 0.10
     #D1)
      t667 = t39 * t650 * t660
      t677 = log(-0.4D1 * t404 * t333 * t516 * t653)
      t678 = t677 * t650
      t680 = t677 ** 2
      t684 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, 0.10
     #D1)
      t700 = -(0.90D2 * t63 * t39 * (t652 - t658 * t650 * t660) - 0.180D
     #3 * t414 * t667) * t185 * t187 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(-t678 * t651 + t680 * t650 * t660 / 0.2D1 + t650 * t684) + 0.180D
     #3 * t54 * t120 * (t652 - t678 * t660) - t362 * t667) * t185 * t131
     # / 0.720D3
      t701 = FJET(XB1, XB2, s, -t638 * t639, t638 * t641, t638 * t325, 0
     #.0D0, s * t644 * t17 * t457 * t516, t700)
      t703 = KAPPA2(t319, x2, x3, t99, z)
      t704 = s * t703
      t709 = t703 ** 2
      t714 = cos(t11)
      t717 = Sqrt(x3 * t516 * t172)
      t724 = 0.1D1 / (-0.2D1 + t703)
      t725 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, t99)
      t728 = t709 ** 2
      t733 = log(0.4D1 * t190 * t332 * t516 * t172 * t728)
      t735 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t319, x2, x3, t99)
      t745 = -0.90D2 * t63 * t39 * (t724 * t725 - t733 * t724 * t735) + 
     #0.180D3 * t414 * t39 * t724 * t735
      t749 = FJET(XB1, XB2, s, -t704 * t639, t704 * t641, -t704 * t450, 
     #t704 * t452, s * t709 * t17 * t457 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t714 * t717), -t745 * t185 * t187 / 0.720D3)
      rrgq2qght5s3e1 = t317 * t316 + t445 * t444 + t513 * t512 + t635 * 
     #t634 + t701 * t700 - t749 * t745 * t185 * t131 * t133 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t68 = 0.1D1 - x4
      t69 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t68)
      t70 = x3 * t11
      t71 = t70 * t13
      t72 = t16 * x4
      t73 = -t68
      t74 = t72 * t73
      t77 = log(-0.4D1 * t71 * t74)
      t78 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t68)
      t80 = t13 * t16
      t81 = t80 * x4
      t84 = log(0.4D1 * t70 * t81)
      t90 = t27 * t31
      t91 = t55 - t78
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t103 = log(0.4D1 * t70 * t80)
      t105 = t103 ** 2
      t117 = t90 * t55
      t118 = t8 * t117
      t124 = log(-0.4D1 * t14 * t74)
      t126 = t124 ** 2
      t129 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t68)
      t132 = log(0.4D1 * t14 * t72)
      t134 = t132 ** 2
      t147 = -t91
      t148 = t90 * t147
      t153 = x1 ** 2
      t154 = t153 * t11
      t155 = t154 * t13
      t158 = log(-0.4D1 * t155 * t74)
      t162 = log(0.4D1 * t154 * t81)
      t171 = 0.1D1 / x1
      t175 = t36 * t31
      t177 = t171 * t98
      t181 = x3 * t153
      t184 = log(0.4D1 * t181 * t17)
      t196 = t154 * t80
      t198 = log(0.4D1 * t196)
      t200 = t198 ** 2
      t215 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (-t69 + t77 
     #* t78 + t32 - t84 * t55) - 0.180D3 * t59 * t90 * t91) * t96 * t98 
     #/ 0.1440D4 - (0.90D2 * t36 * t31 * (t103 * t32 - t105 * t55 / 0.2D
     #1 - t64) - 0.180D3 * t59 * t90 * (-t32 + t103 * t55) - t118) * t96
     # / 0.1440D4 - (0.90D2 * t36 * t31 * (-t124 * t69 + t126 * t78 / 0.
     #2D1 + t129 + t132 * t32 - t134 * t55 / 0.2D1 - t64) - 0.180D3 * t5
     #9 * t90 * (t69 - t124 * t78 - t32 + t132 * t55) + t8 * t148) * t98
     # / 0.1440D4 - (0.90D2 * t36 * t31 * (t69 - t158 * t78 - t32 + t162
     # * t55) - 0.180D3 * t59 * t148) * t171 * t98 / 0.720D3 - t175 * t1
     #47 * t96 * t177 / 0.8D1 + (0.90D2 * t36 * t31 * (t32 - t184 * t55)
     # - 0.180D3 * t59 * t117) * t96 * t171 / 0.720D3 + (0.90D2 * t36 * 
     #t31 * (-t198 * t32 + t200 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t
     #90 * (t32 - t198 * t55) + t118) * t171 / 0.720D3
      t216 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t215)
      t218 = 0.1D1 - x1
      t219 = KAPPA2(t218, x2, 0.0D0, 0.10D1, z)
      t220 = s * t219
      t221 = -t218
      t222 = t1 * t221
      t224 = t1 * x1
      t226 = t219 ** 2
      t232 = 0.1D1 / (-0.2D1 + t219)
      t233 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, 0
     #.10D1)
      t234 = t233 * t232
      t235 = t221 ** 2
      t236 = t16 * t235
      t237 = t226 ** 2
      t242 = log(0.4D1 * t155 * t236 * x4 * t237)
      t244 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, 0
     #.10D1)
      t250 = t59 * t27
      t251 = t31 * t232
      t252 = t251 * t244
      t254 = 0.180D3 * t250 * t252
      t264 = t181 * t11
      t269 = log(0.4D1 * t264 * t80 * t235 * t237)
      t283 = log(0.4D1 * t155 * t236 * t237)
      t284 = t283 * t232
      t286 = t283 ** 2
      t290 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, 0
     #.10D1)
      t306 = -(0.90D2 * t36 * t31 * (-t234 + t242 * t232 * t244) + t254)
     # * t171 * t98 / 0.720D3 + t36 * t251 * t244 * t96 * t177 / 0.8D1 +
     # (0.90D2 * t36 * t31 * (t234 - t269 * t232 * t244) - t254) * t96 *
     # t171 / 0.720D3 + (0.90D2 * t36 * t31 * (-t284 * t233 + t286 * t23
     #2 * t244 / 0.2D1 + t232 * t290) - 0.180D3 * t59 * t90 * (t234 - t2
     #84 * t244) + t8 * t27 * t252) * t171 / 0.720D3
      t307 = FJET(XB1, XB2, s, 0.0D0, -t220 * t222, t220 * t224, 0.0D0, 
     #-s * t226 * t15 * t221 * x1, t306)
      t309 = KAPPA2(t218, x2, 0.0D0, t68, z)
      t310 = s * t309
      t312 = t224 * t73
      t314 = t224 * x4
      t316 = t309 ** 2
      t319 = t221 * x1
      t323 = 0.1D1 / (-0.2D1 + t309)
      t324 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, t
     #68)
      t327 = t316 ** 2
      t332 = log(-0.4D1 * t196 * t235 * x4 * t73 * t327)
      t334 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, 0.0D0, t
     #68)
      t340 = t31 * t323
      t353 = -(0.90D2 * t36 * t31 * (t323 * t324 - t332 * t323 * t334) -
     # 0.180D3 * t250 * t340 * t334) * t171 * t98 / 0.720D3 - t36 * t340
     # * t334 * t96 * t177 / 0.8D1
      t354 = FJET(XB1, XB2, s, 0.0D0, -t310 * t222, -t310 * t312, t310 *
     # t314, s * t316 * t15 * t319 * t73, t353)
      t357 = -0.1D1 + x3
      t359 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t360 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t6
     #8)
      t361 = t359 - t360
      t366 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t367 = t80 * t357
      t370 = log(-0.4D1 * t264 * t367)
      t376 = t90 * t359
      t383 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t6
     #8)
      t388 = log(0.4D1 * t71 * t72 * t73 * t357)
      t394 = log(-0.4D1 * t71 * t16 * t357 * x4)
      t410 = log(-0.4D1 * t70 * t367)
      t412 = t410 ** 2
      t415 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t429 = -t175 * t361 * t96 * t177 / 0.8D1 + (0.90D2 * t36 * t31 * (
     #-t366 + t370 * t359) + 0.180D3 * t59 * t376) * t96 * t171 / 0.720D
     #3 + (0.90D2 * t36 * t31 * (t383 - t388 * t360 - t366 + t394 * t359
     #) + 0.180D3 * t59 * t90 * t361) * t96 * t98 / 0.1440D4 - (0.90D2 *
     # t36 * t31 * (-t410 * t366 + t412 * t359 / 0.2D1 + t415) - 0.180D3
     # * t59 * t90 * (t366 - t410 * t359) + t8 * t376) * t96 / 0.1440D4
      t430 = FJET(XB1, XB2, s, t2 * x3, -t2 * t357, 0.0D0, 0.0D0, 0.0D0,
     # t429)
      t432 = KAPPA2(t218, x2, x3, 0.10D1, z)
      t433 = s * t432
      t434 = t222 * x3
      t436 = t222 * t357
      t439 = t432 ** 2
      t445 = 0.1D1 / (-0.2D1 + t432)
      t446 = t31 * t445
      t448 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, x3, 0.10
     #D1)
      t453 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t218, x2, x3, 0.10
     #D1)
      t456 = t439 ** 2
      t461 = log(-0.4D1 * t181 * t14 * t236 * t357 * t456)
      t475 = -t36 * t446 * t448 * t96 * t177 / 0.8D1 + (-0.90D2 * t36 * 
     #t31 * (t445 * t453 - t461 * t445 * t448) + 0.180D3 * t250 * t446 *
     # t448) * t96 * t171 / 0.720D3
      t476 = FJET(XB1, XB2, s, -t433 * t434, t433 * t436, t433 * t224, 0
     #.0D0, s * t439 * t15 * t319 * t357, t475)
      t478 = KAPPA2(t218, x2, x3, t68, z)
      t479 = s * t478
      t484 = t478 ** 2
      t489 = cos(t9)
      t493 = Sqrt(x3 * t357 * x4 * t73)
      t500 = 0.1D1 / (-0.2D1 + t478)
      t503 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t218, x2, x3, t68)
      t508 = FJET(XB1, XB2, s, -t479 * t434, t479 * t436, -t479 * t312, 
     #t479 * t314, s * t484 * t15 * t319 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t489 * t493), t36 * t31 * t500 * t503 * t96 * t17
     #7 / 0.8D1)
      rrgq2qght5s3e0 = t216 * t215 + t307 * t306 + t354 * t353 + t430 * 
     #t429 + t476 * t475 + t508 * 0.3141592653589793D1 * t90 * t500 * t5
     #03 * t96 * t171 * t98 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t50 = t45 * t23
      t51 = 0.1D1 - x4
      t52 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t51)
      t53 = t41 - t52
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t7
      t61 = t9 * t12
      t64 = log(0.4D1 * t60 * t61)
      t70 = t19 * t23
      t73 = 0.180D3 * t3 * t70 * t41
      t77 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t51)
      t78 = t12 * x4
      t79 = -t51
      t83 = log(-0.4D1 * t10 * t78 * t79)
      t87 = log(0.4D1 * t10 * t78)
      t93 = -t53
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t7
      t109 = log(0.4D1 * t106 * t61)
      t122 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t53 * t54 * t56 
     #/ 0.16D2 - (0.90D2 * t45 * t23 * (-t24 + t64 * t41) + t73) * t54 /
     # 0.1440D4 - (0.90D2 * t45 * t23 * (t77 - t83 * t52 - t24 + t87 * t
     #41) - 0.180D3 * t3 * t70 * t93) * t56 / 0.1440D4 + t50 * t41 * t54
     # * t101 / 0.8D1 + (0.90D2 * t45 * t23 * (t24 - t109 * t41) - t73) 
     #* t101 / 0.720D3 - t50 * t93 * t101 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t122)
      t125 = 0.1D1 - x1
      t126 = KAPPA2(t125, x2, 0.0D0, 0.10D1, z)
      t127 = s * t126
      t128 = -t125
      t129 = t1 * t128
      t131 = t1 * x1
      t133 = t126 ** 2
      t139 = 0.1D1 / (-0.2D1 + t126)
      t140 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, 0
     #.10D1)
      t141 = t139 * t140
      t142 = t54 * t101
      t146 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, 0
     #.10D1)
      t149 = t128 ** 2
      t151 = t133 ** 2
      t155 = log(0.4D1 * t106 * t9 * t12 * t149 * t151)
      t170 = t101 * t56
      t174 = t50 * t141 * t142 / 0.8D1 + (0.90D2 * t45 * t23 * (t139 * t
     #146 - t155 * t139 * t140) - 0.180D3 * t3 * t19 * t23 * t139 * t140
     #) * t101 / 0.720D3 + t50 * t141 * t170 / 0.8D1
      t175 = FJET(XB1, XB2, s, 0.0D0, -t127 * t129, t127 * t131, 0.0D0, 
     #-s * t133 * t11 * t128 * x1, t174)
      t177 = KAPPA2(t125, x2, 0.0D0, t51, z)
      t178 = s * t177
      t184 = t177 ** 2
      t187 = t128 * x1
      t192 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, t
     #51)
      t194 = 0.1D1 / (-0.2D1 + t177) * t192 * t170
      t197 = FJET(XB1, XB2, s, 0.0D0, -t178 * t129, -t178 * t131 * t79, 
     #t178 * t131 * x4, s * t184 * t11 * t187 * t79, -t50 * t194 / 0.8D1
     #)
      t203 = -0.1D1 + x3
      t205 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t210 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t5
     #1)
      t216 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t220 = log(-0.4D1 * t60 * t61 * t203)
      t232 = -t50 * t205 * t54 * t101 / 0.8D1 + t50 * (-t205 + t210) * t
     #54 * t56 / 0.16D2 - (0.90D2 * t45 * t23 * (t216 - t220 * t205) - 0
     #.180D3 * t3 * t70 * t205) * t54 / 0.1440D4
      t233 = FJET(XB1, XB2, s, t2 * x3, -t2 * t203, 0.0D0, 0.0D0, 0.0D0,
     # t232)
      t235 = KAPPA2(t125, x2, x3, 0.10D1, z)
      t236 = s * t235
      t242 = t235 ** 2
      t249 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, x3, 0.10
     #D1)
      t251 = 0.1D1 / (-0.2D1 + t235) * t249 * t142
      t254 = FJET(XB1, XB2, s, -t236 * t129 * x3, t236 * t129 * t203, t2
     #36 * t131, 0.0D0, s * t242 * t11 * t187 * t203, -t50 * t251 / 0.8D
     #1)
      rrgq2qght5s3em1 = t123 * t122 + t175 * t174 - t197 * 0.31415926535
     #89793D1 * t70 * t194 / 0.8D1 + t233 * t232 - t254 * 0.314159265358
     #9793D1 * t70 * t251 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.1D1 - x4)
      t10 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t17 = t7 * t10
      t18 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t26 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
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
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.1
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
      t81 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t65, x2, 0.0D0, 0.1
     #0D1)
      t86 = FJET(XB1, XB2, s, 0.0D0, -t67 * t1 * t68, t67 * t1 * x1, 0.0
     #D0, -s * t73 * t38 * t68 * x1, t4 * t7 * t80 * t81 * t22 / 0.8D1)
      rrgq2qght5s3em2 = t50 * t49 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t86 * 0.3141592653589793D1 * t3 * t7 * t80 * t81 * t2
     #2 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s3em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s3em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
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
      t108 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t110 = t107 ** 2
      t111 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t114 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t119 = t35 * t39
      t126 = t86 - t111
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
      t224 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t229 = t212 ** 2
      t241 = -t119 * t126
      t256 = x3 * t13
      t257 = t256 * t15
      t260 = log(-0.4D1 * t257 * t104)
      t262 = t260 ** 2
      t267 = log(0.4D1 * t256 * t93)
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
     #84 * t186 / 0.720D3 - (0.90D2 * t63 * t39 * (t191 * t40 - t193 * t
     #86 / 0.2D1 - t50) - 0.180D3 * t54 * t119 * (-t40 + t191 * t86) - t
     #44 * t153) * t184 * t130 / 0.720D3 - (t44 * t119 * (-t40 + t212 * 
     #t86 + t108 - t216 * t111) + 0.90D2 * t63 * t39 * (t221 * t108 / 0.
     #2D1 + t224 - t221 * t216 * t111 / 0.6D1 - t216 * t114 - t229 * t40
     # / 0.2D1 - t59 + t229 * t212 * t86 / 0.6D1 + t212 * t50) + t10 * t
     #241 - 0.180D3 * t54 * t119 * (t212 * t40 - t229 * t86 / 0.2D1 - t5
     #0 - t216 * t108 + t221 * t111 / 0.2D1 + t114)) * t132 / 0.1440D4 -
     # (0.90D2 * t63 * t39 * (-t260 * t108 + t262 * t111 / 0.2D1 + t114 
     #+ t267 * t40 - t269 * t86 / 0.2D1 - t50) - 0.180D3 * t54 * t119 * 
     #(t108 - t260 * t111 - t40 + t267 * t86) + t44 * t241) * t184 * t13
     #2 / 0.1440D4 + (t44 * t119 * (t40 - t289 * t86) + 0.90D2 * t63 * t
     #39 * (t294 * t40 / 0.2D1 + t59 - t294 * t289 * t86 / 0.6D1 - t289 
     #* t50) + t154 - 0.180D3 * t54 * t119 * (-t289 * t40 + t294 * t86 /
     # 0.2D1 + t50)) * t184 / 0.1440D4
      t316 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t315)
      t318 = -0.1D1 + x1
      t321 = t318 ** 2
      t326 = log(0.4D1 * t19 * t90 * t321 * x4)
      t327 = -t318
      t328 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t330 = t326 ** 2
      t331 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t334 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t344 = t119 * t331
      t345 = t44 * t344
      t349 = t18 * t90
      t350 = t349 * t321
      t353 = log(0.4D1 * t16 * t350)
      t358 = t353 ** 2
      t361 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, 0
     #.0D0)
      t380 = t321 * x4
      t384 = log(0.4D1 * t257 * t349 * t380)
      t397 = log(0.4D1 * t257 * t350)
      t399 = t397 ** 2
      t415 = (0.90D2 * t63 * t39 * (t326 * t328 - t330 * t331 / 0.2D1 - 
     #t334) - 0.180D3 * t54 * t119 * (-t328 + t326 * t331) - t345) * t13
     #0 * t132 / 0.720D3 + (-t44 * t119 * (t328 - t353 * t331) - 0.90D2 
     #* t63 * t39 * (t358 * t328 / 0.2D1 + t361 - t358 * t353 * t331 / 0
     #.6D1 - t353 * t334) - t10 * t344 + 0.180D3 * t54 * t119 * (-t353 *
     # t328 + t358 * t331 / 0.2D1 + t334)) * t130 / 0.720D3 + (0.90D2 * 
     #t63 * t39 * (-t328 + t384 * t331) + 0.180D3 * t54 * t344) * t184 *
     # t186 / 0.720D3 - (0.90D2 * t63 * t39 * (-t397 * t328 + t399 * t33
     #1 / 0.2D1 + t334) - 0.180D3 * t54 * t119 * (t328 - t397 * t331) + 
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
      t442 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, x
     #4)
      t444 = t438 ** 2
      t446 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, x
     #4)
      t449 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t327, x2, 0.0D0, x
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
      t498 = log(-0.4D1 * t257 * t18 * t492 * x4)
      t499 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t501 = t498 ** 2
      t502 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t505 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t507 = t102 * t103 * t492
      t510 = log(0.4D1 * t257 * t507)
      t511 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t513 = t510 ** 2
      t514 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t517 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t528 = -t514 + t502
      t535 = t92 * t492
      t538 = log(-0.4D1 * t256 * t535)
      t543 = t538 ** 2
      t546 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t555 = t119 * t502
      t567 = t492 * x4
      t571 = log(-0.4D1 * t166 * t92 * t567)
      t575 = log(0.4D1 * t468 * t507)
      t591 = log(-0.4D1 * t166 * t535)
      t593 = t591 ** 2
      t610 = -(0.90D2 * t63 * t39 * (-t498 * t499 + t501 * t502 / 0.2D1 
     #+ t505 + t510 * t511 - t513 * t514 / 0.2D1 - t517) - 0.180D3 * t54
     # * t119 * (t499 - t498 * t502 - t511 + t510 * t514) + t44 * t119 *
     # t528) * t184 * t132 / 0.1440D4 + (-t44 * t119 * (t499 - t538 * t5
     #02) - 0.90D2 * t63 * t39 * (t543 * t499 / 0.2D1 + t546 - t543 * t5
     #38 * t502 / 0.6D1 - t538 * t505) - t10 * t555 + 0.180D3 * t54 * t1
     #19 * (-t538 * t499 + t543 * t502 / 0.2D1 + t505)) * t184 / 0.1440D
     #4 + (0.90D2 * t63 * t39 * (-t499 + t571 * t502 + t511 - t575 * t51
     #4) + 0.180D3 * t54 * t119 * t528) * t184 * t186 / 0.720D3 - (0.90D
     #2 * t63 * t39 * (-t591 * t499 + t593 * t502 / 0.2D1 + t505) - 0.18
     #0D3 * t54 * t119 * (t499 - t591 * t502) + t44 * t555) * t184 * t13
     #0 / 0.720D3
      t611 = FJET(XB1, XB2, s, t2 * x3, -t2 * t492, 0.0D0, 0.0D0, 0.0D0,
     # t610)
      t613 = KAPPA2(t327, x2, x3, 0.0D0, z)
      t614 = s * t613
      t615 = t420 * x3
      t617 = t420 * t492
      t620 = t613 ** 2
      t626 = 0.1D1 / (-0.2D1 + t613)
      t627 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, 0.0D
     #0)
      t628 = t626 * t627
      t629 = t620 ** 2
      t634 = log(-0.4D1 * t468 * t469 * t567 * t629)
      t636 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, 0.0D
     #0)
      t643 = t39 * t626 * t636
      t653 = log(-0.4D1 * t468 * t469 * t492 * t629)
      t654 = t653 * t626
      t656 = t653 ** 2
      t660 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, 0.0D
     #0)
      t676 = (0.90D2 * t63 * t39 * (-t628 + t634 * t626 * t636) + 0.180D
     #3 * t481 * t643) * t184 * t186 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #-t654 * t627 + t656 * t626 * t636 / 0.2D1 + t626 * t660) - 0.180D3
     # * t54 * t119 * (t628 - t654 * t636) + t461 * t643) * t184 * t130 
     #/ 0.720D3
      t677 = FJET(XB1, XB2, s, -t614 * t615, t614 * t617, 0.0D0, t614 * 
     #t422, -s * t620 * t17 * t430 * x3, t676)
      t679 = KAPPA2(t327, x2, x3, x4, z)
      t680 = s * t679
      t685 = t679 ** 2
      t690 = cos(t11)
      t693 = Sqrt(x3 * t492 * t171)
      t700 = 0.1D1 / (-0.2D1 + t679)
      t701 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, x4)
      t704 = t685 ** 2
      t709 = log(0.4D1 * t189 * t321 * t492 * t171 * t704)
      t711 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t327, x2, x3, x4)
      t721 = 0.90D2 * t63 * t39 * (t700 * t701 - t709 * t700 * t711) - 0
     #.180D3 * t481 * t39 * t700 * t711
      t725 = FJET(XB1, XB2, s, -t680 * t615, t680 * t617, t680 * t423, -
     #t680 * t425, s * t685 * t17 * t430 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t690 * t693), t721 * t184 * t186 / 0.720D3)
      rrgq2qght5s4e1 = t316 * t315 + t416 * t415 + t489 * t488 + t611 * 
     #t610 + t677 * t676 + t725 * t721 * t184 * t130 * t132 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t68 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t69 = x3 * t11
      t70 = t69 * t13
      t71 = t16 * x4
      t72 = -0.1D1 + x4
      t73 = t71 * t72
      t76 = log(-0.4D1 * t70 * t73)
      t77 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t69 * t80)
      t89 = t27 * t31
      t90 = -t55 + t77
      t91 = t89 * t90
      t95 = 0.1D1 / x3
      t97 = 0.1D1 / x4
      t102 = log(0.4D1 * t69 * t79)
      t104 = t102 ** 2
      t116 = t89 * t55
      t117 = t8 * t116
      t123 = log(0.4D1 * t14 * t71)
      t125 = t123 ** 2
      t130 = log(-0.4D1 * t14 * t73)
      t132 = t130 ** 2
      t135 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t150 = x1 ** 2
      t151 = t150 * t11
      t154 = log(0.4D1 * t151 * t80)
      t159 = log(-0.4D1 * t151 * t13 * t73)
      t165 = -t90
      t170 = 0.1D1 / x1
      t174 = t36 * t31
      t176 = t170 * t97
      t180 = x3 * t150
      t183 = log(0.4D1 * t180 * t17)
      t195 = t151 * t79
      t197 = log(0.4D1 * t195)
      t199 = t197 ** 2
      t214 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * lh * t3) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 - (0.90D2 * t36 * t31 * (t68 - t76 *
     # t77 - t32 + t83 * t55) - 0.180D3 * t59 * t91) * t95 * t97 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (-t102 * t32 + t104 * t55 / 0.2D1 + t6
     #4) - 0.180D3 * t59 * t89 * (t32 - t102 * t55) + t117) * t95 / 0.14
     #40D4 - (0.90D2 * t36 * t31 * (t123 * t32 - t125 * t55 / 0.2D1 - t6
     #4 - t130 * t68 + t132 * t77 / 0.2D1 + t135) - 0.180D3 * t59 * t89 
     #* (-t32 + t123 * t55 + t68 - t130 * t77) + t8 * t91) * t97 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (t32 - t154 * t55 - t68 + t159 * t77) 
     #- 0.180D3 * t59 * t89 * t165) * t170 * t97 / 0.720D3 + t174 * t165
     # * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t31 * (-t32 + t183 * t55) 
     #+ 0.180D3 * t59 * t116) * t95 * t170 / 0.720D3 + (0.90D2 * t36 * t
     #31 * (-t197 * t32 + t199 * t55 / 0.2D1 + t64) - 0.180D3 * t59 * t8
     #9 * (t32 - t197 * t55) + t117) * t170 / 0.720D3
      t215 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t214)
      t217 = -0.1D1 + x1
      t220 = -t217
      t221 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, 0
     #.0D0)
      t222 = t217 ** 2
      t227 = log(0.4D1 * t17 * t150 * t222 * x4)
      t228 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, 0
     #.0D0)
      t234 = t89 * t228
      t236 = 0.180D3 * t59 * t234
      t246 = t16 * t150 * t222
      t249 = log(0.4D1 * t70 * t246)
      t261 = log(0.4D1 * t14 * t246)
      t263 = t261 ** 2
      t266 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, 0
     #.0D0)
      t280 = (0.90D2 * t36 * t31 * (-t221 + t227 * t228) + t236) * t170 
     #* t97 / 0.720D3 - t174 * t228 * t95 * t176 / 0.8D1 - (0.90D2 * t36
     # * t31 * (t221 - t249 * t228) - t236) * t95 * t170 / 0.720D3 + (-0
     #.90D2 * t36 * t31 * (-t261 * t221 + t263 * t228 / 0.2D1 + t266) + 
     #0.180D3 * t59 * t89 * (t221 - t261 * t228) - t8 * t234) * t170 / 0
     #.720D3
      t281 = FJET(XB1, XB2, s, 0.0D0, -t2 * t217, 0.0D0, t2 * x1, 0.0D0,
     # t280)
      t283 = KAPPA2(t220, x2, 0.0D0, x4, z)
      t284 = s * t283
      t285 = t1 * t217
      t287 = t1 * x1
      t288 = t287 * x4
      t290 = t287 * t72
      t292 = t283 ** 2
      t295 = t217 * x1
      t299 = 0.1D1 / (-0.2D1 + t283)
      t300 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, x
     #4)
      t303 = t292 ** 2
      t308 = log(-0.4D1 * t195 * t222 * x4 * t72 * t303)
      t310 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, 0.0D0, x
     #4)
      t316 = t59 * t27
      t317 = t31 * t299
      t330 = (-0.90D2 * t36 * t31 * (t299 * t300 - t308 * t299 * t310) +
     # 0.180D3 * t316 * t317 * t310) * t170 * t97 / 0.720D3 - t36 * t317
     # * t310 * t95 * t176 / 0.8D1
      t331 = FJET(XB1, XB2, s, 0.0D0, -t284 * t285, t284 * t288, -t284 *
     # t290, -s * t292 * t15 * t295 * x4, t330)
      t334 = -0.1D1 + x3
      t336 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t337 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t338 = t336 - t337
      t343 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t345 = t79 * t334
      t348 = log(-0.4D1 * t180 * t11 * t345)
      t354 = t89 * t337
      t365 = log(-0.4D1 * t70 * t16 * t334 * x4)
      t367 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t372 = log(0.4D1 * t70 * t71 * t72 * t334)
      t388 = log(-0.4D1 * t69 * t345)
      t390 = t388 ** 2
      t393 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t407 = t174 * t338 * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t31 * (t
     #343 - t348 * t337) - 0.180D3 * t59 * t354) * t95 * t170 / 0.720D3 
     #- (0.90D2 * t36 * t31 * (t343 - t365 * t337 - t367 + t372 * t336) 
     #+ 0.180D3 * t59 * t89 * t338) * t95 * t97 / 0.1440D4 + (-0.90D2 * 
     #t36 * t31 * (-t388 * t343 + t390 * t337 / 0.2D1 + t393) + 0.180D3 
     #* t59 * t89 * (t343 - t388 * t337) - t8 * t354) * t95 / 0.1440D4
      t408 = FJET(XB1, XB2, s, t2 * x3, -t2 * t334, 0.0D0, 0.0D0, 0.0D0,
     # t407)
      t410 = KAPPA2(t220, x2, x3, 0.0D0, z)
      t411 = s * t410
      t412 = t285 * x3
      t414 = t285 * t334
      t417 = t410 ** 2
      t423 = 0.1D1 / (-0.2D1 + t410)
      t424 = t31 * t423
      t426 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, x3, 0.0D
     #0)
      t431 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t220, x2, x3, 0.0D
     #0)
      t435 = t417 ** 2
      t440 = log(-0.4D1 * t180 * t14 * t16 * t222 * t334 * t435)
      t454 = -t36 * t424 * t426 * t95 * t176 / 0.8D1 - (0.90D2 * t36 * t
     #31 * (t423 * t431 - t440 * t423 * t426) - 0.180D3 * t316 * t424 * 
     #t426) * t95 * t170 / 0.720D3
      t455 = FJET(XB1, XB2, s, -t411 * t412, t411 * t414, 0.0D0, t411 * 
     #t287, -s * t417 * t15 * t295 * x3, t454)
      t457 = KAPPA2(t220, x2, x3, x4, z)
      t458 = s * t457
      t463 = t457 ** 2
      t468 = cos(t9)
      t472 = Sqrt(x3 * t334 * x4 * t72)
      t479 = 0.1D1 / (-0.2D1 + t457)
      t482 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t220, x2, x3, x4)
      t487 = FJET(XB1, XB2, s, -t458 * t412, t458 * t414, t458 * t288, -
     #t458 * t290, s * t463 * t15 * t295 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t468 * t472), t36 * t31 * t479 * t482 * t95 * t176 / 0.8D
     #1)
      rrgq2qght5s4e0 = t215 * t214 + t281 * t280 + t330 * t331 + t408 * 
     #t407 + t455 * t454 + t487 * 0.3141592653589793D1 * t89 * t479 * t4
     #82 * t95 * t170 * t97 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t50 = t45 * t23
      t51 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
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
      t81 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
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
      t128 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, 0.0D0, 0
     #.0D0)
      t133 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, t127, x2, 0.0D0, 0
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
      t175 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, 0.0D0, x
     #4)
      t178 = 0.1D1 / (-0.2D1 + t158) * t175 * t99 * t55
      t181 = FJET(XB1, XB2, s, 0.0D0, -t159 * t160, t159 * t162 * x4, -t
     #159 * t162 * t82, -s * t167 * t11 * t170 * x4, -t50 * t178 / 0.8D1
     #)
      t187 = -0.1D1 + x3
      t189 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t194 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t200 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t204 = log(-0.4D1 * t59 * t60 * t187)
      t216 = -t50 * t189 * t53 * t99 / 0.8D1 - t50 * (-t194 + t189) * t5
     #3 * t55 / 0.16D2 + (-0.90D2 * t45 * t23 * (t200 - t204 * t189) + 0
     #.180D3 * t3 * t69 * t189) * t53 / 0.1440D4
      t217 = FJET(XB1, XB2, s, t2 * x3, -t2 * t187, 0.0D0, 0.0D0, 0.0D0,
     # t216)
      t219 = KAPPA2(t127, x2, x3, 0.0D0, z)
      t220 = s * t219
      t226 = t219 ** 2
      t233 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, t127, x2, x3, 0.0D
     #0)
      t236 = 0.1D1 / (-0.2D1 + t219) * t233 * t53 * t99
      t239 = FJET(XB1, XB2, s, -t220 * t160 * x3, t220 * t160 * t187, 0.
     #0D0, t220 * t162, -s * t226 * t11 * t170 * x3, -t50 * t236 / 0.8D1
     #)
      rrgq2qght5s4em1 = t122 * t121 + t156 * t155 - t181 * 0.31415926535
     #89793D1 * t69 * t178 / 0.8D1 + t217 * t216 - t239 * 0.314159265358
     #9793D1 * t69 * t236 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, x
     #4)
      t16 = t7 * t8
      t17 = 0.1D1 / x3
      t21 = 0.1D1 / x1
      t25 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
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
      t54 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.0
     #D0)
      t56 = t7 * t54 * t17
      t59 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, -t4 * t56 / 0.16D2)
      t64 = -0.1D1 + x1
      t68 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, -t64, x2, 0.0D0, 0.
     #0D0)
      t70 = t7 * t68 * t21
      t73 = FJET(XB1, XB2, s, 0.0D0, -t2 * t64, 0.0D0, t2 * x1, 0.0D0, -
     #t4 * t70 / 0.8D1)
      rrgq2qght5s4em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.16D2 - t73 * 0.3141592653589793D1 * t3 * t70 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s4em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s4em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s5e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t149 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t150 = x3 * t90
      t151 = t150 * t13
      t152 = -t148
      t153 = t152 * x4
      t157 = log(-0.4D1 * t151 * t92 * t153)
      t158 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t162 = log(0.4D1 * t151 * t93)
      t168 = t158 - t86
      t169 = t105 * t168
      t173 = 0.1D1 / x3
      t175 = t114 * t116
      t178 = t92 * t152
      t181 = log(-0.4D1 * t151 * t178)
      t183 = t181 ** 2
      t186 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t187 = t150 * t19
      t189 = log(0.4D1 * t187)
      t191 = t189 ** 2
      t206 = -t44 * t105 * t168
      t211 = t18 * x4
      t214 = log(0.4D1 * t16 * t211)
      t219 = t214 ** 2
      t240 = x3 * t13
      t243 = log(0.4D1 * t240 * t93)
      t245 = t243 ** 2
      t248 = t240 * t15
      t253 = log(-0.4D1 * t248 * t18 * t152 * x4)
      t255 = t253 ** 2
      t274 = log(-0.4D1 * t240 * t178)
      t278 = log(0.4D1 * t240 * t92)
      t283 = t278 ** 2
      t290 = t274 ** 2
      t293 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, 0
     #.10D1)
      t316 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
     #* t39 * t40 / 0.1440D4 + (t44 + 0.180D3 * t22 * lh + 0.45D2 * t28)
     # * t35 * t39 * t50 / 0.1440D4 + (-0.180D3 * t54 - 0.90D2 * t22) * 
     #t35 * t39 * t59 / 0.1440D4 + t63 * t39 * t64 / 0.16D2 + (0.3141592
     #653589793D1 * (t68 + 0.60D2 * t69 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t6 * t3) - t22 * t9 + t28 * t25 / 0.2D1 + 0.30D2 * t32 * lh
     # + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1) * t35 * t39 * t86 
     #/ 0.1440D4 + (0.90D2 * t63 * t39 * (-t96 * t40 + t98 * t86 / 0.2D1
     # + t50) - 0.180D3 * t54 * t105 * (t40 - t96 * t86) + t44 * t111) *
     # t114 * t116 / 0.720D3 + (t44 * t105 * (t40 - t121 * t86) + 0.90D2
     # * t63 * t39 * (t126 * t40 / 0.2D1 + t59 - t126 * t121 * t86 / 0.6
     #D1 - t121 * t50) + t137 - 0.180D3 * t54 * t105 * (-t121 * t40 + t1
     #26 * t86 / 0.2D1 + t50)) * t114 / 0.720D3 - (0.90D2 * t63 * t39 * 
     #(t149 - t157 * t158 - t40 + t162 * t86) - 0.180D3 * t54 * t169) * 
     #t173 * t175 / 0.720D3 + (0.90D2 * t63 * t39 * (t181 * t149 - t183 
     #* t158 / 0.2D1 - t186 - t189 * t40 + t191 * t86 / 0.2D1 + t50) - 0
     #.180D3 * t54 * t105 * (-t149 + t181 * t158 + t40 - t189 * t86) + t
     #206) * t173 * t114 / 0.720D3 - (t44 * t105 * (-t40 + t214 * t86) +
     # 0.90D2 * t63 * t39 * (-t219 * t40 / 0.2D1 - t59 + t219 * t214 * t
     #86 / 0.6D1 + t214 * t50) - t137 - 0.180D3 * t54 * t105 * (t214 * t
     #40 - t219 * t86 / 0.2D1 - t50)) * t116 / 0.1440D4 + (0.90D2 * t63 
     #* t39 * (-t243 * t40 + t245 * t86 / 0.2D1 + t50 + t253 * t149 - t2
     #55 * t158 / 0.2D1 - t186) - 0.180D3 * t54 * t105 * (t40 - t243 * t
     #86 - t149 + t253 * t158) + t206) * t173 * t116 / 0.1440D4 - (t44 *
     # t105 * (t149 - t274 * t158 - t40 + t278 * t86) + 0.90D2 * t63 * t
     #39 * (-t283 * t40 / 0.2D1 - t59 + t283 * t278 * t86 / 0.6D1 + t278
     # * t50 + t290 * t149 / 0.2D1 + t293 - t290 * t274 * t158 / 0.6D1 -
     # t274 * t186) + t10 * t169 - 0.180D3 * t54 * t105 * (-t274 * t149 
     #+ t290 * t158 / 0.2D1 + t186 + t278 * t40 - t283 * t86 / 0.2D1 - t
     #50)) * t173 / 0.1440D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t316)
      t319 = -0.1D1 + x4
      t322 = -t319
      t323 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t322)
      t324 = t211 * t319
      t327 = log(-0.4D1 * t16 * t324)
      t328 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t322)
      t333 = t327 ** 2
      t336 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t322)
      t340 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t322)
      t346 = t105 * t328
      t359 = t211 * t319 * t152
      t362 = log(0.4D1 * t248 * t359)
      t363 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, t
     #322)
      t365 = t362 ** 2
      t366 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, t
     #322)
      t369 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t148, t
     #322)
      t372 = log(-0.4D1 * t248 * t324)
      t374 = t372 ** 2
      t387 = t366 - t328
      t397 = log(-0.4D1 * t91 * t15 * t324)
      t399 = t397 ** 2
      t416 = x4 * t319
      t420 = log(-0.4D1 * t151 * t92 * t416)
      t422 = t150 * t16
      t425 = log(0.4D1 * t422 * t359)
      t439 = -(t44 * t105 * (t323 - t327 * t328) + 0.90D2 * t63 * t39 * 
     #(t333 * t323 / 0.2D1 + t336 - t333 * t327 * t328 / 0.6D1 - t327 * 
     #t340) + t10 * t346 - 0.180D3 * t54 * t105 * (-t327 * t323 + t333 *
     # t328 / 0.2D1 + t340)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #-t362 * t363 + t365 * t366 / 0.2D1 + t369 + t372 * t323 - t374 * t
     #328 / 0.2D1 - t340) - 0.180D3 * t54 * t105 * (t363 - t362 * t366 -
     # t323 + t372 * t328) + t44 * t105 * t387) * t173 * t116 / 0.1440D4
     # + (0.90D2 * t63 * t39 * (t397 * t323 - t399 * t328 / 0.2D1 - t340
     #) - 0.180D3 * t54 * t105 * (-t323 + t397 * t328) - t44 * t346) * t
     #114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (t323 - t420 * t328 -
     # t363 + t425 * t366) + 0.180D3 * t54 * t105 * t387) * t173 * t175 
     #/ 0.720D3
      t440 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t319, t2 * x4, 0.0D0,
     # t439)
      t443 = -0.1D1 + x1
      t445 = t443 ** 2
      t450 = log(0.4D1 * t19 * t90 * t445 * x4)
      t451 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t453 = t450 ** 2
      t454 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t457 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t467 = t105 * t454
      t468 = t44 * t467
      t472 = t18 * t90
      t473 = t472 * t445
      t476 = log(0.4D1 * t16 * t473)
      t481 = t476 ** 2
      t484 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t503 = t445 * x4
      t507 = log(0.4D1 * t248 * t472 * t503)
      t520 = log(0.4D1 * t248 * t473)
      t522 = t520 ** 2
      t538 = (0.90D2 * t63 * t39 * (t450 * t451 - t453 * t454 / 0.2D1 - 
     #t457) - 0.180D3 * t54 * t105 * (-t451 + t450 * t454) - t468) * t11
     #4 * t116 / 0.720D3 + (-t44 * t105 * (t451 - t476 * t454) - 0.90D2 
     #* t63 * t39 * (t481 * t451 / 0.2D1 + t484 - t481 * t476 * t454 / 0
     #.6D1 - t476 * t457) - t10 * t467 + 0.180D3 * t54 * t105 * (-t476 *
     # t451 + t481 * t454 / 0.2D1 + t457)) * t114 / 0.720D3 - (0.90D2 * 
     #t63 * t39 * (t451 - t507 * t454) - 0.180D3 * t54 * t467) * t173 * 
     #t175 / 0.720D3 + (0.90D2 * t63 * t39 * (t520 * t451 - t522 * t454 
     #/ 0.2D1 - t457) - 0.180D3 * t54 * t105 * (-t451 + t520 * t454) - t
     #468) * t173 * t114 / 0.720D3
      t539 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t443, 0.0D0, 0.0D0,
     # t538)
      t541 = KAPPA2(x1, x2, 0.10D1, t322, z)
      t542 = s * t541
      t543 = t1 * x1
      t545 = t1 * t443
      t546 = t545 * t319
      t548 = t545 * x4
      t550 = t541 ** 2
      t553 = x1 * t443
      t556 = t550 ** 2
      t561 = log(-0.4D1 * t119 * t503 * t319 * t556)
      t563 = 0.1D1 / (-0.2D1 + t541)
      t564 = t561 * t563
      t565 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #22)
      t567 = t561 ** 2
      t569 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #22)
      t572 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #22)
      t578 = t563 * t565
      t584 = t44 * t35
      t586 = t39 * t563 * t569
      t591 = t18 * t445
      t596 = log(-0.4D1 * t422 * t591 * t416 * t556)
      t603 = t54 * t35
      t610 = (-0.90D2 * t63 * t39 * (-t564 * t565 + t567 * t563 * t569 /
     # 0.2D1 + t563 * t572) + 0.180D3 * t54 * t105 * (t578 - t564 * t569
     #) - t584 * t586) * t114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t578 - t596 * t563 * t569) - 0.180D3 * t603 * t586) * t173 * t175 
     #/ 0.720D3
      t611 = FJET(XB1, XB2, s, t542 * t543, 0.0D0, t542 * t546, -t542 * 
     #t548, -s * t550 * t17 * t553 * x4, t610)
      t613 = KAPPA2(x1, x2, t148, 0.10D1, z)
      t614 = s * t613
      t615 = t543 * t152
      t617 = t543 * x3
      t620 = t613 ** 2
      t626 = 0.1D1 / (-0.2D1 + t613)
      t627 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, 0.10
     #D1)
      t628 = t626 * t627
      t629 = t620 ** 2
      t634 = log(-0.4D1 * t422 * t591 * t153 * t629)
      t636 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, 0.10
     #D1)
      t643 = t39 * t626 * t636
      t653 = log(-0.4D1 * t422 * t591 * t152 * t629)
      t654 = t653 * t626
      t656 = t653 ** 2
      t660 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, 0.10
     #D1)
      t676 = -(0.90D2 * t63 * t39 * (t628 - t634 * t626 * t636) - 0.180D
     #3 * t603 * t643) * t173 * t175 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(-t654 * t627 + t656 * t626 * t636 / 0.2D1 + t626 * t660) + 0.180D
     #3 * t54 * t105 * (t628 - t654 * t636) - t584 * t643) * t173 * t114
     # / 0.720D3
      t677 = FJET(XB1, XB2, s, -t614 * t615, t614 * t617, -t614 * t545, 
     #0.0D0, -s * t620 * t17 * t553 * x3, t676)
      t679 = KAPPA2(x1, x2, t148, t322, z)
      t680 = s * t679
      t685 = t679 ** 2
      t690 = cos(t11)
      t693 = Sqrt(x3 * t152 * t416)
      t700 = 0.1D1 / (-0.2D1 + t679)
      t701 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, t322
     #)
      t704 = t685 ** 2
      t709 = log(0.4D1 * t187 * t445 * t152 * t416 * t704)
      t711 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t148, t322
     #)
      t721 = -0.90D2 * t63 * t39 * (t700 * t701 - t709 * t700 * t711) + 
     #0.180D3 * t603 * t39 * t700 * t711
      t725 = FJET(XB1, XB2, s, -t680 * t615, t680 * t617, t680 * t546, -
     #t680 * t548, s * t685 * t17 * t553 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t690 * t693), -t721 * t173 * t175 / 0.720D3)
      rrgq2qght5s5e1 = t317 * t316 + t440 * t439 + t539 * t538 + t611 * 
     #t610 + t677 * t676 - t725 * t721 * t173 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s5e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t36 = 0.3141592653589793D1 * t27
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = 0.1D1 - x3
      t76 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.1
     #0D1)
      t77 = t68 * t13
      t78 = -t75
      t83 = log(-0.4D1 * t77 * t16 * t78 * x4)
      t84 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.1
     #0D1)
      t90 = t27 * t31
      t91 = t55 - t84
      t94 = 0.180D3 * t59 * t90 * t91
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t101 = t69 * t78
      t104 = log(-0.4D1 * t68 * t101)
      t106 = t104 ** 2
      t109 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.
     #10D1)
      t112 = log(0.4D1 * t68 * t69)
      t114 = t112 ** 2
      t127 = -t91
      t133 = t16 * x4
      t136 = log(0.4D1 * t14 * t133)
      t138 = t136 ** 2
      t150 = t90 * t55
      t151 = t8 * t150
      t155 = x1 ** 2
      t156 = t155 * t11
      t159 = log(0.4D1 * t156 * t70)
      t168 = 0.1D1 / x1
      t172 = t36 * t31
      t174 = t168 * t98
      t178 = x3 * t155
      t182 = log(-0.4D1 * t178 * t11 * t101)
      t186 = log(0.4D1 * t178 * t17)
      t196 = t156 * t69
      t198 = log(0.4D1 * t196)
      t200 = t198 ** 2
      t215 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * t3 * lh) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (t32 - t73 *
     # t55 - t76 + t83 * t84) - t94) * t96 * t98 / 0.1440D4 - (0.90D2 * 
     #t36 * t31 * (-t104 * t76 + t106 * t84 / 0.2D1 + t109 + t112 * t32 
     #- t114 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t90 * (t76 - t104 * 
     #t84 - t32 + t112 * t55) + t8 * t90 * t127) * t96 / 0.1440D4 - (0.9
     #0D2 * t36 * t31 * (t136 * t32 - t138 * t55 / 0.2D1 - t64) - 0.180D
     #3 * t59 * t90 * (-t32 + t136 * t55) - t151) * t98 / 0.1440D4 + (0.
     #90D2 * t36 * t31 * (t32 - t159 * t55) - 0.180D3 * t59 * t150) * t1
     #68 * t98 / 0.720D3 - t172 * t127 * t96 * t174 / 0.8D1 + (0.90D2 * 
     #t36 * t31 * (-t76 + t182 * t84 + t32 - t186 * t55) - t94) * t96 * 
     #t168 / 0.720D3 + (0.90D2 * t36 * t31 * (-t198 * t32 + t200 * t55 /
     # 0.2D1 + t64) - 0.180D3 * t59 * t90 * (t32 - t198 * t55) + t151) *
     # t168 / 0.720D3
      t216 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t215)
      t218 = -0.1D1 + x4
      t221 = t133 * t218
      t224 = log(-0.4D1 * t14 * t221)
      t225 = -t218
      t226 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t225)
      t228 = t224 ** 2
      t229 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t225)
      t232 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t225)
      t242 = t90 * t229
      t250 = log(-0.4D1 * t156 * t13 * t221)
      t262 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, t2
     #25)
      t263 = t229 - t262
      t268 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, t2
     #25)
      t273 = log(0.4D1 * t77 * t133 * t218 * t78)
      t277 = log(-0.4D1 * t77 * t221)
      t291 = -(0.90D2 * t36 * t31 * (-t224 * t226 + t228 * t229 / 0.2D1 
     #+ t232) - 0.180D3 * t59 * t90 * (t226 - t224 * t229) + t8 * t242) 
     #* t98 / 0.1440D4 + (0.90D2 * t36 * t31 * (-t226 + t250 * t229) + 0
     #.180D3 * t59 * t242) * t168 * t98 / 0.720D3 - t172 * t263 * t96 * 
     #t174 / 0.8D1 + (0.90D2 * t36 * t31 * (t268 - t273 * t262 - t226 + 
     #t277 * t229) + 0.180D3 * t59 * t90 * t263) * t96 * t98 / 0.1440D4
      t292 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t218, t2 * x4, 0.0D0,
     # t291)
      t295 = -0.1D1 + x1
      t297 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t298 = t295 ** 2
      t303 = log(0.4D1 * t17 * t155 * t298 * x4)
      t304 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t310 = t90 * t304
      t312 = 0.180D3 * t59 * t310
      t322 = t16 * t155 * t298
      t325 = log(0.4D1 * t77 * t322)
      t337 = log(0.4D1 * t14 * t322)
      t339 = t337 ** 2
      t342 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t356 = (0.90D2 * t36 * t31 * (-t297 + t303 * t304) + t312) * t168 
     #* t98 / 0.720D3 - t172 * t304 * t96 * t174 / 0.8D1 + (0.90D2 * t36
     # * t31 * (-t297 + t325 * t304) + t312) * t96 * t168 / 0.720D3 + (-
     #0.90D2 * t36 * t31 * (-t337 * t297 + t339 * t304 / 0.2D1 + t342) +
     # 0.180D3 * t59 * t90 * (t297 - t337 * t304) - t8 * t310) * t168 / 
     #0.720D3
      t357 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t295, 0.0D0, 0.0D0,
     # t356)
      t359 = KAPPA2(x1, x2, 0.10D1, t225, z)
      t360 = s * t359
      t361 = t1 * x1
      t363 = t1 * t295
      t364 = t363 * t218
      t366 = t363 * x4
      t368 = t359 ** 2
      t371 = x1 * t295
      t375 = 0.1D1 / (-0.2D1 + t359)
      t376 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #25)
      t379 = t368 ** 2
      t384 = log(-0.4D1 * t196 * t298 * x4 * t218 * t379)
      t386 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #25)
      t392 = t59 * t27
      t393 = t31 * t375
      t406 = (-0.90D2 * t36 * t31 * (t375 * t376 - t384 * t375 * t386) +
     # 0.180D3 * t392 * t393 * t386) * t168 * t98 / 0.720D3 - t36 * t393
     # * t386 * t96 * t174 / 0.8D1
      t407 = FJET(XB1, XB2, s, t360 * t361, 0.0D0, t360 * t364, -t360 * 
     #t366, -s * t368 * t15 * t371 * x4, t406)
      t409 = KAPPA2(x1, x2, t75, 0.10D1, z)
      t410 = s * t409
      t411 = t361 * t78
      t413 = t361 * x3
      t416 = t409 ** 2
      t422 = 0.1D1 / (-0.2D1 + t409)
      t423 = t31 * t422
      t425 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, 0.10D
     #1)
      t430 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, 0.10D
     #1)
      t434 = t416 ** 2
      t439 = log(-0.4D1 * t178 * t14 * t16 * t298 * t78 * t434)
      t453 = -t36 * t423 * t425 * t96 * t174 / 0.8D1 + (-0.90D2 * t36 * 
     #t31 * (t422 * t430 - t439 * t422 * t425) + 0.180D3 * t392 * t423 *
     # t425) * t96 * t168 / 0.720D3
      t454 = FJET(XB1, XB2, s, -t410 * t411, t410 * t413, -t410 * t363, 
     #0.0D0, -s * t416 * t15 * t371 * x3, t453)
      t456 = KAPPA2(x1, x2, t75, t225, z)
      t457 = s * t456
      t462 = t456 ** 2
      t467 = cos(t9)
      t471 = Sqrt(x3 * t78 * x4 * t218)
      t478 = 0.1D1 / (-0.2D1 + t456)
      t481 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, t225)
      t486 = FJET(XB1, XB2, s, -t457 * t411, t457 * t413, t457 * t364, -
     #t457 * t366, s * t462 * t15 * t371 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t467 * t471), t36 * t31 * t478 * t481 * t96 * t174 / 0.8D
     #1)
      rrgq2qght5s5e0 = t216 * t215 + t292 * t291 + t357 * t356 + t407 * 
     #t406 + t454 * t453 + t486 * 0.3141592653589793D1 * t90 * t478 * t4
     #81 * t96 * t168 * t98 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s5em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t50 = t45 * t23
      t51 = 0.1D1 - x3
      t52 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.1
     #0D1)
      t53 = t41 - t52
      t54 = 0.1D1 / x3
      t55 = t53 * t54
      t56 = 0.1D1 / x4
      t60 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.1
     #0D1)
      t61 = x3 * t7
      t62 = t9 * t12
      t63 = -t51
      t67 = log(-0.4D1 * t61 * t62 * t63)
      t71 = log(0.4D1 * t61 * t62)
      t77 = t19 * t23
      t85 = t12 * x4
      t88 = log(0.4D1 * t10 * t85)
      t96 = 0.180D3 * t3 * t77 * t41
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t108 = log(0.4D1 * t104 * t7 * t62)
      t121 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t55 * t56 / 0.16
     #D2 - (0.90D2 * t45 * t23 * (t60 - t67 * t52 - t24 + t71 * t41) + 0
     #.180D3 * t3 * t77 * t53) * t54 / 0.1440D4 - (0.90D2 * t45 * t23 * 
     #(-t24 + t88 * t41) + t96) * t56 / 0.1440D4 + t50 * t55 * t100 / 0.
     #8D1 + (0.90D2 * t45 * t23 * (t24 - t108 * t41) - t96) * t100 / 0.7
     #20D3 + t50 * t41 * t100 * t56 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t124 = -0.1D1 + x4
      t127 = -t124
      t128 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t127)
      t132 = log(-0.4D1 * t10 * t85 * t124)
      t133 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t127)
      t149 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, t1
     #27)
      t155 = -(0.90D2 * t45 * t23 * (t128 - t132 * t133) - 0.180D3 * t3 
     #* t77 * t133) * t56 / 0.1440D4 - t50 * t133 * t100 * t56 / 0.8D1 +
     # t50 * (t149 - t133) * t54 * t56 / 0.16D2
      t156 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t124, t2 * x4, 0.0D0,
     # t155)
      t159 = -0.1D1 + x1
      t161 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t166 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t168 = t159 ** 2
      t172 = log(0.4D1 * t10 * t12 * t104 * t168)
      t188 = -t50 * t161 * t54 * t100 / 0.8D1 + (-0.90D2 * t45 * t23 * (
     #t166 - t172 * t161) + 0.180D3 * t3 * t77 * t161) * t100 / 0.720D3 
     #- t50 * t161 * t100 * t56 / 0.8D1
      t189 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t159, 0.0D0, 0.0D0,
     # t188)
      t191 = KAPPA2(x1, x2, 0.10D1, t127, z)
      t192 = s * t191
      t193 = t1 * x1
      t195 = t1 * t159
      t200 = t191 ** 2
      t203 = x1 * t159
      t208 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t1
     #27)
      t211 = 0.1D1 / (-0.2D1 + t191) * t208 * t100 * t56
      t214 = FJET(XB1, XB2, s, t192 * t193, 0.0D0, t192 * t195 * t124, -
     #t192 * t195 * x4, -s * t200 * t11 * t203 * x4, -t50 * t211 / 0.8D1
     #)
      t219 = KAPPA2(x1, x2, t51, 0.10D1, z)
      t220 = s * t219
      t226 = t219 ** 2
      t233 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t51, 0.10D
     #1)
      t236 = 0.1D1 / (-0.2D1 + t219) * t233 * t54 * t100
      t239 = FJET(XB1, XB2, s, -t220 * t193 * t63, t220 * t193 * x3, -t2
     #20 * t195, 0.0D0, -s * t226 * t11 * t203 * x3, -t50 * t236 / 0.8D1
     #)
      rrgq2qght5s5em1 = t122 * t121 + t156 * t155 + t189 * t188 - t214 *
     # 0.3141592653589793D1 * t77 * t211 / 0.8D1 - t239 * 0.314159265358
     #9793D1 * t77 * t236 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s5em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t15 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.10D1)
      t22 = 0.1D1 / x1
      t26 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t9 * t10 / 0.16D2 - t4 * t7 * (t15 - t8) / x3 / 0.16D2 
     #+ t4 * t9 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0.3
     #141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * t3
     # * t9 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t52 = -0.1D1 + x4
      t56 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #-t52)
      t58 = t7 * t56 * t10
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t52, t2 * x4, 0.0D0, -
     #t4 * t58 / 0.16D2)
      t69 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.1
     #0D1)
      t71 = t7 * t69 * t22
      t74 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, -t4 * t71 / 0.8D1)
      rrgq2qght5s5em2 = t50 * t49 - t61 * 0.3141592653589793D1 * t3 * t5
     #8 / 0.16D2 - t74 * 0.3141592653589793D1 * t3 * t71 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s5em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s5em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s5em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s5em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s6e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 * 0.3141592653589793D1
      t25 = 0.180D3 * t6 - 0.30D2 * t3
      t27 = t21 ** 2
      t28 = 0.3141592653589793D1 * t27
      t32 = t27 * t21 * 0.3141592653589793D1
      t35 = 0.1D1 / t1
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
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
      t155 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t156 = -t154
      t157 = t156 * x4
      t161 = log(-0.4D1 * t149 * t92 * t157)
      t162 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t168 = -t162 + t86
      t169 = t105 * t168
      t173 = 0.1D1 / x3
      t175 = t114 * t116
      t178 = t148 * t19
      t180 = log(0.4D1 * t178)
      t182 = t180 ** 2
      t185 = t92 * t156
      t188 = log(-0.4D1 * t149 * t185)
      t190 = t188 ** 2
      t193 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
     #.0D0)
      t209 = t18 * x4
      t212 = log(0.4D1 * t16 * t209)
      t217 = t212 ** 2
      t238 = x3 * t13
      t241 = log(0.4D1 * t238 * t93)
      t243 = t241 ** 2
      t246 = t238 * t15
      t251 = log(-0.4D1 * t246 * t18 * t156 * x4)
      t253 = t251 ** 2
      t267 = -t105 * t168
      t275 = log(-0.4D1 * t238 * t185)
      t279 = log(0.4D1 * t238 * t92)
      t284 = t279 ** 2
      t291 = t275 ** 2
      t294 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, 0
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
     #126 * t86 / 0.2D1 - t50)) * t114 / 0.720D3 + (0.90D2 * t63 * t39 *
     # (t40 - t152 * t86 - t155 + t161 * t162) - 0.180D3 * t54 * t169) *
     # t173 * t175 / 0.720D3 + (0.90D2 * t63 * t39 * (-t180 * t40 + t182
     # * t86 / 0.2D1 + t50 + t188 * t155 - t190 * t162 / 0.2D1 - t193) -
     # 0.180D3 * t54 * t105 * (t40 - t180 * t86 - t155 + t188 * t162) + 
     #t44 * t169) * t173 * t114 / 0.720D3 + (t44 * t105 * (t40 - t212 * 
     #t86) + 0.90D2 * t63 * t39 * (t217 * t40 / 0.2D1 + t59 - t217 * t21
     #2 * t86 / 0.6D1 - t212 * t50) + t137 - 0.180D3 * t54 * t105 * (-t2
     #12 * t40 + t217 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 - (0.90D2 
     #* t63 * t39 * (t241 * t40 - t243 * t86 / 0.2D1 - t50 - t251 * t155
     # + t253 * t162 / 0.2D1 + t193) - 0.180D3 * t54 * t105 * (-t40 + t2
     #41 * t86 + t155 - t251 * t162) + t44 * t267) * t173 * t116 / 0.144
     #0D4 - (t44 * t105 * (t155 - t275 * t162 - t40 + t279 * t86) + 0.90
     #D2 * t63 * t39 * (-t284 * t40 / 0.2D1 - t59 + t284 * t279 * t86 / 
     #0.6D1 + t279 * t50 + t291 * t155 / 0.2D1 + t294 - t291 * t275 * t1
     #62 / 0.6D1 - t275 * t193) + t10 * t267 - 0.180D3 * t54 * t105 * (-
     #t275 * t155 + t291 * t162 / 0.2D1 + t193 + t279 * t40 - t284 * t86
     # / 0.2D1 - t50)) * t173 / 0.1440D4
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t317)
      t321 = -0.1D1 + x4
      t323 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t324 = t209 * t321
      t327 = log(-0.4D1 * t16 * t324)
      t328 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t333 = t327 ** 2
      t336 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t340 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t346 = t105 * t328
      t360 = log(-0.4D1 * t246 * t324)
      t362 = t360 ** 2
      t366 = t209 * t321 * t156
      t369 = log(0.4D1 * t246 * t366)
      t370 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, x
     #4)
      t372 = t369 ** 2
      t373 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, x
     #4)
      t376 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t154, x
     #4)
      t387 = t328 - t373
      t394 = t91 * t15
      t397 = log(-0.4D1 * t394 * t324)
      t399 = t397 ** 2
      t416 = x4 * t321
      t420 = log(-0.4D1 * t149 * t92 * t416)
      t422 = t148 * t16
      t425 = log(0.4D1 * t422 * t366)
      t439 = (t44 * t105 * (-t323 + t327 * t328) + 0.90D2 * t63 * t39 * 
     #(-t333 * t323 / 0.2D1 - t336 + t333 * t327 * t328 / 0.6D1 + t327 *
     # t340) - t10 * t346 - 0.180D3 * t54 * t105 * (t327 * t323 - t333 *
     # t328 / 0.2D1 - t340)) * t116 / 0.1440D4 - (0.90D2 * t63 * t39 * (
     #-t360 * t323 + t362 * t328 / 0.2D1 + t340 + t369 * t370 - t372 * t
     #373 / 0.2D1 - t376) - 0.180D3 * t54 * t105 * (t323 - t360 * t328 -
     # t370 + t369 * t373) + t44 * t105 * t387) * t173 * t116 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t397 * t323 + t399 * t328 / 0.2D1 + t34
     #0) - 0.180D3 * t54 * t105 * (t323 - t397 * t328) + t44 * t346) * t
     #114 * t116 / 0.720D3 + (0.90D2 * t63 * t39 * (-t323 + t420 * t328 
     #+ t370 - t425 * t373) + 0.180D3 * t54 * t105 * t387) * t173 * t175
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
      t465 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t467 = t461 ** 2
      t469 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t472 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
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
      t504 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t528 = log(0.4D1 * t422 * t458)
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
     # t114 / 0.720D3 + (0.90D2 * t63 * t39 * (t478 - t528 * t463 * t469
     #) - 0.180D3 * t535 * t486) * t173 * t175 / 0.720D3 + (0.90D2 * t63
     # * t39 * (-t546 * t465 + t548 * t463 * t469 / 0.2D1 + t473) - 0.18
     #0D3 * t54 * t105 * (t478 - t546 * t469) + t487) * t173 * t114 / 0.
     #720D3
      t566 = FJET(XB1, XB2, s, t443 * t444, 0.0D0, 0.0D0, -t443 * t447, 
     #-s * t449 * t17 * x1 * t446, t565)
      t568 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t569 = s * t568
      t571 = t447 * x4
      t573 = t447 * t321
      t575 = t568 ** 2
      t578 = x1 * t446
      t581 = t575 ** 2
      t586 = log(-0.4D1 * t119 * t416 * t454 * t581)
      t588 = 0.1D1 / (-0.2D1 + t568)
      t589 = t586 * t588
      t590 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t592 = t586 ** 2
      t594 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t597 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t603 = t588 * t590
      t610 = t39 * t588 * t594
      t620 = log(-0.4D1 * t422 * t209 * t321 * t454 * t581)
      t633 = -(0.90D2 * t63 * t39 * (-t589 * t590 + t592 * t588 * t594 /
     # 0.2D1 + t588 * t597) - 0.180D3 * t54 * t105 * (t603 - t589 * t594
     #) + t484 * t610) * t114 * t116 / 0.720D3 + (0.90D2 * t63 * t39 * (
     #-t603 + t620 * t588 * t594) + 0.180D3 * t535 * t610) * t173 * t175
     # / 0.720D3
      t634 = FJET(XB1, XB2, s, t569 * t444, 0.0D0, -t569 * t571, t569 * 
     #t573, s * t575 * t17 * t578 * t321, t633)
      t636 = KAPPA2(x1, x2, t154, 0.0D0, z)
      t637 = s * t636
      t638 = t444 * t156
      t640 = t444 * x3
      t643 = t636 ** 2
      t649 = 0.1D1 / (-0.2D1 + t636)
      t650 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, 0.0D
     #0)
      t651 = t649 * t650
      t652 = t643 ** 2
      t657 = log(-0.4D1 * t422 * t455 * t157 * t652)
      t659 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, 0.0D
     #0)
      t666 = t39 * t649 * t659
      t676 = log(-0.4D1 * t422 * t455 * t156 * t652)
      t677 = t676 * t649
      t679 = t676 ** 2
      t683 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, 0.0D
     #0)
      t699 = (0.90D2 * t63 * t39 * (-t651 + t657 * t649 * t659) + 0.180D
     #3 * t535 * t666) * t173 * t175 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(-t677 * t650 + t679 * t649 * t659 / 0.2D1 + t649 * t683) + 0.180D
     #3 * t54 * t105 * (t651 - t677 * t659) - t484 * t666) * t173 * t114
     # / 0.720D3
      t700 = FJET(XB1, XB2, s, -t637 * t638, t637 * t640, 0.0D0, -t637 *
     # t447, s * t643 * t17 * t578 * t156, t699)
      t702 = KAPPA2(x1, x2, t154, x4, z)
      t703 = s * t702
      t708 = t702 ** 2
      t713 = cos(t11)
      t716 = Sqrt(x3 * t156 * t416)
      t723 = 0.1D1 / (-0.2D1 + t702)
      t724 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, x4)
      t727 = t708 ** 2
      t732 = log(0.4D1 * t178 * t416 * t454 * t156 * t727)
      t734 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t154, x4)
      t744 = 0.90D2 * t63 * t39 * (t723 * t724 - t732 * t723 * t734) - 0
     #.180D3 * t535 * t39 * t723 * t734
      t748 = FJET(XB1, XB2, s, -t703 * t638, t703 * t640, -t703 * t571, 
     #t703 * t573, s * t708 * t17 * t578 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t713 * t716), t744 * t173 * t175 / 0.720D3)
      rrgq2qght5s6e1 = t318 * t317 + t440 * t439 + t566 * t565 + t634 * 
     #t633 + t700 * t699 + t748 * t744 * t173 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s6e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t36 = 0.3141592653589793D1 * t27
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t68 = x3 * t11
      t69 = t13 * t16
      t70 = t69 * x4
      t73 = log(0.4D1 * t68 * t70)
      t75 = 0.1D1 - x3
      t76 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.0
     #D0)
      t77 = t68 * t13
      t78 = -t75
      t83 = log(-0.4D1 * t77 * t16 * t78 * x4)
      t84 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.0
     #D0)
      t90 = t27 * t31
      t91 = t84 - t55
      t92 = t90 * t91
      t96 = 0.1D1 / x3
      t98 = 0.1D1 / x4
      t101 = t69 * t78
      t104 = log(-0.4D1 * t68 * t101)
      t106 = t104 ** 2
      t109 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, 0.
     #0D0)
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
      t171 = -t91
      t173 = t166 * t98
      t177 = x3 * t153
      t180 = log(0.4D1 * t177 * t17)
      t182 = t177 * t11
      t185 = log(-0.4D1 * t182 * t101)
      t198 = t154 * t69
      t200 = log(0.4D1 * t198)
      t202 = t200 ** 2
      t217 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * t3 * lh) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 - (0.90D2 * t36 * t31 * (-t32 + t73 
     #* t55 + t76 - t83 * t84) - 0.180D3 * t59 * t92) * t96 * t98 / 0.14
     #40D4 - (0.90D2 * t36 * t31 * (-t104 * t76 + t106 * t84 / 0.2D1 + t
     #109 + t112 * t32 - t114 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t90
     # * (t76 - t104 * t84 - t32 + t112 * t55) + t8 * t92) * t96 / 0.144
     #0D4 + (0.90D2 * t36 * t31 * (-t134 * t32 + t136 * t55 / 0.2D1 + t6
     #4) - 0.180D3 * t59 * t90 * (t32 - t134 * t55) + t149) * t98 / 0.14
     #40D4 - (0.90D2 * t36 * t31 * (-t32 + t157 * t55) + 0.180D3 * t59 *
     # t148) * t166 * t98 / 0.720D3 + t170 * t171 * t96 * t173 / 0.8D1 +
     # (0.90D2 * t36 * t31 * (t32 - t180 * t55 - t76 + t185 * t84) - 0.1
     #80D3 * t59 * t90 * t171) * t96 * t166 / 0.720D3 - (0.90D2 * t36 * 
     #t31 * (t200 * t32 - t202 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t9
     #0 * (-t32 + t200 * t55) - t149) * t166 / 0.720D3
      t218 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t217)
      t221 = -0.1D1 + x4
      t223 = t131 * t221
      t226 = log(-0.4D1 * t14 * t223)
      t227 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t229 = t226 ** 2
      t230 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t233 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t243 = t90 * t230
      t248 = t154 * t13
      t251 = log(-0.4D1 * t248 * t223)
      t263 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, x4
     #)
      t264 = t263 - t230
      t271 = log(-0.4D1 * t77 * t223)
      t273 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t75, x4
     #)
      t278 = log(0.4D1 * t77 * t131 * t221 * t78)
      t292 = (0.90D2 * t36 * t31 * (t226 * t227 - t229 * t230 / 0.2D1 - 
     #t233) - 0.180D3 * t59 * t90 * (-t227 + t226 * t230) - t8 * t243) *
     # t98 / 0.1440D4 - (0.90D2 * t36 * t31 * (t227 - t251 * t230) - 0.1
     #80D3 * t59 * t243) * t166 * t98 / 0.720D3 + t170 * t264 * t96 * t1
     #73 / 0.8D1 - (0.90D2 * t36 * t31 * (t227 - t271 * t230 - t273 + t2
     #78 * t263) + 0.180D3 * t59 * t90 * t264) * t96 * t98 / 0.1440D4
      t293 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t221, 0.0D0,
     # t292)
      t295 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t296 = s * t295
      t297 = t1 * x1
      t299 = -0.1D1 + x1
      t300 = t1 * t299
      t302 = t295 ** 2
      t308 = 0.1D1 / (-0.2D1 + t295)
      t309 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t310 = t308 * t309
      t311 = t299 ** 2
      t312 = t16 * t311
      t313 = t302 ** 2
      t318 = log(0.4D1 * t248 * t312 * x4 * t313)
      t320 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t326 = t59 * t27
      t327 = t31 * t308
      t328 = t327 * t320
      t330 = 0.180D3 * t326 * t328
      t344 = log(0.4D1 * t182 * t69 * t311 * t313)
      t358 = log(0.4D1 * t248 * t312 * t313)
      t359 = t358 * t308
      t361 = t358 ** 2
      t365 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t381 = -(0.90D2 * t36 * t31 * (-t310 + t318 * t308 * t320) + t330)
     # * t166 * t98 / 0.720D3 + t36 * t327 * t320 * t96 * t173 / 0.8D1 +
     # (0.90D2 * t36 * t31 * (t310 - t344 * t308 * t320) - t330) * t96 *
     # t166 / 0.720D3 - (-0.90D2 * t36 * t31 * (-t359 * t309 + t361 * t3
     #08 * t320 / 0.2D1 + t308 * t365) + 0.180D3 * t59 * t90 * (t310 - t
     #359 * t320) - t8 * t27 * t328) * t166 / 0.720D3
      t382 = FJET(XB1, XB2, s, t296 * t297, 0.0D0, 0.0D0, -t296 * t300, 
     #-s * t302 * t15 * x1 * t299, t381)
      t384 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t385 = s * t384
      t387 = t300 * x4
      t389 = t300 * t221
      t391 = t384 ** 2
      t394 = x1 * t299
      t398 = 0.1D1 / (-0.2D1 + t384)
      t399 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t401 = x4 * t221
      t402 = t391 ** 2
      t407 = log(-0.4D1 * t198 * t401 * t311 * t402)
      t409 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t415 = t31 * t398
      t428 = -(0.90D2 * t36 * t31 * (t398 * t399 - t407 * t398 * t409) -
     # 0.180D3 * t326 * t415 * t409) * t166 * t98 / 0.720D3 - t36 * t415
     # * t409 * t96 * t173 / 0.8D1
      t429 = FJET(XB1, XB2, s, t385 * t297, 0.0D0, -t385 * t387, t385 * 
     #t389, s * t391 * t15 * t394 * t221, t428)
      t431 = KAPPA2(x1, x2, t75, 0.0D0, z)
      t432 = s * t431
      t433 = t297 * t78
      t435 = t297 * x3
      t438 = t431 ** 2
      t444 = 0.1D1 / (-0.2D1 + t431)
      t445 = t31 * t444
      t447 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, 0.0D0
     #)
      t452 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, 0.0D0
     #)
      t455 = t438 ** 2
      t460 = log(-0.4D1 * t177 * t14 * t312 * t78 * t455)
      t474 = -t36 * t445 * t447 * t96 * t173 / 0.8D1 + (-0.90D2 * t36 * 
     #t31 * (t444 * t452 - t460 * t444 * t447) + 0.180D3 * t326 * t445 *
     # t447) * t96 * t166 / 0.720D3
      t475 = FJET(XB1, XB2, s, -t432 * t433, t432 * t435, 0.0D0, -t432 *
     # t300, s * t438 * t15 * t394 * t78, t474)
      t477 = KAPPA2(x1, x2, t75, x4, z)
      t478 = s * t477
      t483 = t477 ** 2
      t488 = cos(t9)
      t491 = Sqrt(x3 * t78 * t401)
      t498 = 0.1D1 / (-0.2D1 + t477)
      t501 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t75, x4)
      t506 = FJET(XB1, XB2, s, -t478 * t433, t478 * t435, -t478 * t387, 
     #t478 * t389, s * t483 * t15 * t394 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t488 * t491), t36 * t31 * t498 * t501 * t96 * t17
     #3 / 0.8D1)
      rrgq2qght5s6e0 = t218 * t217 + t293 * t292 + t382 * t381 + t429 * 
     #t428 + t475 * t474 + t506 * 0.3141592653589793D1 * t90 * t498 * t5
     #01 * t96 * t166 * t98 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s6em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t50 = t45 * t23
      t51 = 0.1D1 - x3
      t52 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.0
     #D0)
      t53 = t52 - t41
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.0
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
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 - t50 * t53 * t54 * t56 
     #/ 0.16D2 - (0.90D2 * t45 * t23 * (t60 - t67 * t52 - t24 + t71 * t4
     #1) - 0.180D3 * t3 * t77 * t53) * t54 / 0.1440D4 + (0.90D2 * t45 * 
     #t23 * (t24 - t87 * t41) - t95) * t56 / 0.1440D4 - t50 * t53 * t54 
     #* t101 / 0.8D1 - (0.90D2 * t45 * t23 * (-t24 + t109 * t41) + t95) 
     #* t101 / 0.720D3 + t50 * t41 * t101 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t122)
      t126 = -0.1D1 + x4
      t128 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t132 = log(-0.4D1 * t10 * t84 * t126)
      t133 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t149 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, x4
     #)
      t155 = (0.90D2 * t45 * t23 * (-t128 + t132 * t133) + 0.180D3 * t3 
     #* t77 * t133) * t56 / 0.1440D4 - t50 * t133 * t101 * t56 / 0.8D1 -
     # t50 * (t133 - t149) * t54 * t56 / 0.16D2
      t156 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t126, 0.0D0,
     # t155)
      t158 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t159 = s * t158
      t160 = t1 * x1
      t162 = -0.1D1 + x1
      t163 = t1 * t162
      t165 = t158 ** 2
      t171 = 0.1D1 / (-0.2D1 + t158)
      t172 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t173 = t171 * t172
      t174 = t54 * t101
      t178 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
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
      t224 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t226 = 0.1D1 / (-0.2D1 + t209) * t224 * t202
      t229 = FJET(XB1, XB2, s, t210 * t160, 0.0D0, -t210 * t163 * x4, t2
     #10 * t163 * t126, s * t216 * t11 * t219 * t126, -t50 * t226 / 0.8D
     #1)
      t234 = KAPPA2(x1, x2, t51, 0.0D0, z)
      t235 = s * t234
      t241 = t234 ** 2
      t248 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t51, 0.0D0
     #)
      t250 = 0.1D1 / (-0.2D1 + t234) * t248 * t174
      t253 = FJET(XB1, XB2, s, -t235 * t160 * t63, t235 * t160 * x3, 0.0
     #D0, -t235 * t163, s * t241 * t11 * t219 * t63, -t50 * t250 / 0.8D1
     #)
      rrgq2qght5s6em1 = t123 * t122 + t156 * t155 + t207 * t206 - t229 *
     # 0.3141592653589793D1 * t77 * t226 / 0.8D1 - t253 * 0.314159265358
     #9793D1 * t77 * t250 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s6em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t15 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.0D0)
      t22 = 0.1D1 / x1
      t26 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t9 * t10 / 0.16D2 - t4 * t7 * (t15 - t8) / x3 / 0.16D2 
     #+ t4 * t9 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0.3
     #141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * t3
     # * t9 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t57 = t7 * t55 * t10
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, -t4 * t57 / 0.16D2)
      t65 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t66 = s * t65
      t69 = -0.1D1 + x1
      t72 = t65 ** 2
      t79 = 0.1D1 / (-0.2D1 + t65)
      t80 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.0
     #D0)
      t85 = FJET(XB1, XB2, s, t66 * t1 * x1, 0.0D0, 0.0D0, -t66 * t1 * t
     #69, -s * t72 * t38 * x1 * t69, t4 * t7 * t79 * t80 * t22 / 0.8D1)
      rrgq2qght5s6em2 = t50 * t49 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t85 * 0.3141592653589793D1 * t3 * t7 * t79 * t80 * t2
     #2 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s6em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s6em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s6em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s6em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s7e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
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
      t148 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t149 = x3 * t90
      t150 = t149 * t13
      t151 = -0.1D1 + x3
      t152 = t151 * x4
      t156 = log(-0.4D1 * t150 * t92 * t152)
      t157 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t161 = log(0.4D1 * t150 * t93)
      t167 = -t86 + t157
      t168 = t105 * t167
      t172 = 0.1D1 / x3
      t174 = t114 * t116
      t177 = t92 * t151
      t180 = log(-0.4D1 * t150 * t177)
      t182 = t180 ** 2
      t185 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t186 = t149 * t19
      t188 = log(0.4D1 * t186)
      t190 = t188 ** 2
      t208 = t18 * x4
      t211 = log(0.4D1 * t16 * t208)
      t216 = t211 ** 2
      t237 = x3 * t13
      t238 = t237 * t15
      t243 = log(-0.4D1 * t238 * t18 * t151 * x4)
      t245 = t243 ** 2
      t250 = log(0.4D1 * t237 * t93)
      t252 = t250 ** 2
      t266 = -t105 * t167
      t272 = t237 * t92
      t274 = log(0.4D1 * t272)
      t278 = log(-0.4D1 * t237 * t177)
      t283 = t278 ** 2
      t286 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
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
     # (t148 - t156 * t157 - t40 + t161 * t86) - 0.180D3 * t54 * t168) *
     # t172 * t174 / 0.720D3 - (0.90D2 * t63 * t39 * (-t180 * t148 + t18
     #2 * t157 / 0.2D1 + t185 + t188 * t40 - t190 * t86 / 0.2D1 - t50) -
     # 0.180D3 * t54 * t105 * (t148 - t180 * t157 - t40 + t188 * t86) + 
     #t44 * t168) * t172 * t114 / 0.720D3 + (t44 * t105 * (t40 - t211 * 
     #t86) + 0.90D2 * t63 * t39 * (t216 * t40 / 0.2D1 + t59 - t216 * t21
     #1 * t86 / 0.6D1 - t211 * t50) + t137 - 0.180D3 * t54 * t105 * (-t2
     #11 * t40 + t216 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 + (0.90D2 
     #* t63 * t39 * (t243 * t148 - t245 * t157 / 0.2D1 - t185 - t250 * t
     #40 + t252 * t86 / 0.2D1 + t50) - 0.180D3 * t54 * t105 * (-t148 + t
     #243 * t157 + t40 - t250 * t86) + t44 * t266) * t172 * t116 / 0.144
     #0D4 + (t44 * t105 * (t40 - t274 * t86 - t148 + t278 * t157) + 0.90
     #D2 * t63 * t39 * (-t283 * t148 / 0.2D1 - t286 + t283 * t278 * t157
     # / 0.6D1 + t278 * t185 + t291 * t40 / 0.2D1 + t59 - t291 * t274 * 
     #t86 / 0.6D1 - t274 * t50) + t10 * t266 - 0.180D3 * t54 * t105 * (-
     #t274 * t40 + t291 * t86 / 0.2D1 + t50 + t278 * t148 - t283 * t157 
     #/ 0.2D1 - t185)) * t172 / 0.1440D4
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t316)
      t319 = -0.1D1 + x4
      t322 = -t319
      t323 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t324 = t208 * t319
      t327 = log(-0.4D1 * t16 * t324)
      t328 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t333 = t327 ** 2
      t336 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t340 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t322)
      t346 = t105 * t328
      t360 = log(-0.4D1 * t238 * t324)
      t362 = t360 ** 2
      t366 = t208 * t319 * t151
      t369 = log(0.4D1 * t238 * t366)
      t370 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t32
     #2)
      t372 = t369 ** 2
      t373 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t32
     #2)
      t376 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t32
     #2)
      t387 = -t328 + t373
      t397 = log(-0.4D1 * t91 * t15 * t324)
      t399 = t397 ** 2
      t419 = log(0.4D1 * t149 * t16 * t366)
      t421 = x4 * t319
      t425 = log(-0.4D1 * t150 * t92 * t421)
      t439 = (-t44 * t105 * (t323 - t327 * t328) - 0.90D2 * t63 * t39 * 
     #(t333 * t323 / 0.2D1 + t336 - t333 * t327 * t328 / 0.6D1 - t327 * 
     #t340) - t10 * t346 + 0.180D3 * t54 * t105 * (-t327 * t323 + t333 *
     # t328 / 0.2D1 + t340)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #t360 * t323 - t362 * t328 / 0.2D1 - t340 - t369 * t370 + t372 * t3
     #73 / 0.2D1 + t376) - 0.180D3 * t54 * t105 * (-t323 + t360 * t328 +
     # t370 - t369 * t373) + t44 * t105 * t387) * t172 * t116 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t397 * t323 + t399 * t328 / 0.2D1 + t34
     #0) - 0.180D3 * t54 * t105 * (t323 - t397 * t328) + t44 * t346) * t
     #114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (-t370 + t419 * t373 
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
      t465 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t467 = t461 ** 2
      t469 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t472 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
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
      t504 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t528 = log(0.4D1 * t272 * t458)
      t535 = t54 * t35
      t546 = log(0.4D1 * t238 * t18 * t90 * t454 * t456)
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
      t592 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t32
     #2)
      t594 = t588 ** 2
      t596 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t32
     #2)
      t599 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t32
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
      t641 = t444 * t151
      t644 = t637 ** 2
      t650 = 0.1D1 / (-0.2D1 + t637)
      t651 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t652 = t650 * t651
      t653 = t644 ** 2
      t658 = log(-0.4D1 * t272 * t455 * t152 * t653)
      t660 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t667 = t39 * t650 * t660
      t677 = log(-0.4D1 * t272 * t455 * t151 * t653)
      t678 = t677 * t650
      t680 = t677 ** 2
      t684 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t700 = -(0.90D2 * t63 * t39 * (t652 - t658 * t650 * t660) - 0.180D
     #3 * t535 * t667) * t172 * t174 / 0.720D3 - (-0.90D2 * t63 * t39 * 
     #(t678 * t651 - t680 * t650 * t660 / 0.2D1 - t650 * t684) + 0.180D3
     # * t54 * t105 * (-t652 + t678 * t660) + t484 * t667) * t172 * t114
     # / 0.720D3
      t701 = FJET(XB1, XB2, s, t638 * t639, -t638 * t641, -t638 * t447, 
     #0.0D0, s * t644 * t17 * t579 * t151, t700)
      t703 = KAPPA2(x1, x2, x3, t322, z)
      t704 = s * t703
      t709 = t703 ** 2
      t714 = cos(t11)
      t717 = Sqrt(x3 * t151 * t421)
      t724 = 0.1D1 / (-0.2D1 + t703)
      t725 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t322)
      t728 = t709 ** 2
      t733 = log(0.4D1 * t186 * t454 * t151 * t421 * t728)
      t735 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t322)
      t745 = -0.90D2 * t63 * t39 * (t724 * t725 - t733 * t724 * t735) + 
     #0.180D3 * t535 * t39 * t724 * t735
      t749 = FJET(XB1, XB2, s, t704 * t639, -t704 * t641, t704 * t572, -
     #t704 * t574, s * t709 * t17 * t579 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t714 * t717), -t745 * t172 * t174 / 0.720D3)
      rrgq2qght5s7e1 = t317 * t316 + t440 * t439 + t567 * t566 + t635 * 
     #t634 + t701 * t700 - t749 * t745 * t172 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s7e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t36 = t27 * 0.3141592653589793D1
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t68 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t69 = x3 * t11
      t70 = t69 * t13
      t71 = -0.1D1 + x3
      t76 = log(-0.4D1 * t70 * t16 * t71 * x4)
      t77 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
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
      t115 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
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
     #80D3 * t59 * t89 * t170) * t95 * t165 / 0.720D3 - (0.90D2 * t36 * 
     #t31 * (t199 * t32 - t201 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t8
     #9 * (-t32 + t199 * t55) - t148) * t165 / 0.720D3
      t217 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t216)
      t219 = -0.1D1 + x4
      t222 = t130 * t219
      t225 = log(-0.4D1 * t14 * t222)
      t226 = -t219
      t227 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t226)
      t229 = t225 ** 2
      t230 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t226)
      t233 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t226)
      t243 = t89 * t230
      t251 = log(-0.4D1 * t153 * t13 * t222)
      t263 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t22
     #6)
      t264 = t230 - t263
      t271 = log(-0.4D1 * t70 * t222)
      t273 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t22
     #6)
      t278 = log(0.4D1 * t70 * t130 * t219 * t71)
      t292 = (-0.90D2 * t36 * t31 * (-t225 * t227 + t229 * t230 / 0.2D1 
     #+ t233) + 0.180D3 * t59 * t89 * (t227 - t225 * t230) - t8 * t243) 
     #* t97 / 0.1440D4 - (0.90D2 * t36 * t31 * (t227 - t251 * t230) - 0.
     #180D3 * t59 * t243) * t165 * t97 / 0.720D3 - t169 * t264 * t95 * t
     #172 / 0.8D1 + (0.90D2 * t36 * t31 * (-t227 + t271 * t230 + t273 - 
     #t278 * t263) + 0.180D3 * t59 * t89 * t264) * t95 * t97 / 0.1440D4
      t293 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t219, t2 * x4, 0.0D0,
     # t292)
      t295 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t296 = s * t295
      t297 = t1 * x1
      t299 = -0.1D1 + x1
      t300 = t1 * t299
      t302 = t295 ** 2
      t308 = 0.1D1 / (-0.2D1 + t295)
      t309 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t310 = t308 * t309
      t311 = t299 ** 2
      t312 = t152 * t311
      t313 = t302 ** 2
      t318 = log(0.4D1 * t17 * t312 * x4 * t313)
      t320 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t326 = t59 * t27
      t327 = t31 * t308
      t328 = t327 * t320
      t330 = 0.180D3 * t326 * t328
      t345 = log(0.4D1 * t70 * t16 * t152 * t311 * t313)
      t359 = log(0.4D1 * t17 * t312 * t313)
      t360 = t359 * t308
      t362 = t359 ** 2
      t366 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
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
      t400 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t22
     #6)
      t403 = t392 ** 2
      t408 = log(-0.4D1 * t197 * t311 * x4 * t219 * t403)
      t410 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t22
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
      t436 = t297 * t71
      t439 = t432 ** 2
      t445 = 0.1D1 / (-0.2D1 + t432)
      t446 = t31 * t445
      t448 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t453 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t455 = t439 ** 2
      t460 = log(-0.4D1 * t100 * t312 * t71 * t455)
      t474 = -t36 * t446 * t448 * t95 * t172 / 0.8D1 - (-0.90D2 * t36 * 
     #t31 * (-t445 * t453 + t460 * t445 * t448) - 0.180D3 * t326 * t446 
     #* t448) * t95 * t165 / 0.720D3
      t475 = FJET(XB1, XB2, s, t433 * t434, -t433 * t436, -t433 * t300, 
     #0.0D0, s * t439 * t15 * t395 * t71, t474)
      t477 = KAPPA2(x1, x2, x3, t226, z)
      t478 = s * t477
      t483 = t477 ** 2
      t488 = cos(t9)
      t492 = Sqrt(x3 * t71 * x4 * t219)
      t499 = 0.1D1 / (-0.2D1 + t477)
      t502 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t226)
      t507 = FJET(XB1, XB2, s, t478 * t434, -t478 * t436, t478 * t388, -
     #t478 * t390, s * t483 * t15 * t395 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t488 * t492), t36 * t31 * t499 * t502 * t95 * t17
     #2 / 0.8D1)
      rrgq2qght5s7e0 = t217 * t216 + t293 * t292 + t383 * t382 + t430 * 
     #t429 + t475 * t474 + t507 * 0.3141592653589793D1 * t89 * t499 * t5
     #02 * t95 * t165 * t97 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s7em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t50 = t45 * t23
      t51 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t52 = t41 - t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t7
      t60 = t9 * t12
      t63 = log(0.4D1 * t59 * t60)
      t65 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
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
      t128 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t127)
      t132 = log(-0.4D1 * t10 * t83 * t124)
      t133 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t127)
      t149 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t12
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
      t172 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t173 = t171 * t172
      t174 = t53 * t100
      t178 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
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
      t223 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t12
     #7)
      t225 = 0.1D1 / (-0.2D1 + t208) * t223 * t201
      t228 = FJET(XB1, XB2, s, 0.0D0, t209 * t160, t209 * t163 * t124, -
     #t209 * t163 * x4, s * t215 * t11 * t218 * t124, -t50 * t225 / 0.8D
     #1)
      t233 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t234 = s * t233
      t240 = t233 ** 2
      t247 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t249 = 0.1D1 / (-0.2D1 + t233) * t247 * t174
      t252 = FJET(XB1, XB2, s, t234 * t160 * x3, -t234 * t160 * t66, -t2
     #34 * t163, 0.0D0, s * t240 * t11 * t218 * t66, -t50 * t249 / 0.8D1
     #)
      rrgq2qght5s7em1 = t122 * t121 + t156 * t155 + t206 * t205 - t228 *
     # 0.3141592653589793D1 * t76 * t225 / 0.8D1 - t252 * 0.314159265358
     #9793D1 * t76 * t249 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s7em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t14 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t21 = 0.1D1 / x1
      t25 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
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
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, -
     #t51)
      t57 = t7 * t55 * t10
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t51, t2 * x4, 0.0D0, -
     #t4 * t57 / 0.16D2)
      t65 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t66 = s * t65
      t69 = -0.1D1 + x1
      t72 = t65 ** 2
      t79 = 0.1D1 / (-0.2D1 + t65)
      t80 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t85 = FJET(XB1, XB2, s, 0.0D0, t66 * t1 * x1, -t66 * t1 * t69, 0.0
     #D0, -s * t72 * t37 * x1 * t69, t4 * t7 * t79 * t80 * t21 / 0.8D1)
      rrgq2qght5s7em2 = t49 * t48 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t85 * 0.3141592653589793D1 * t3 * t7 * t79 * t80 * t2
     #1 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s7em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s7em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s7em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s7em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s8e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t44 = 0.3141592653589793D1 * t25
      t50 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t54 = 0.3141592653589793D1 * lh
      t59 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t68 = t3 ** 2
      t69 = t6 ** 2
      t81 = t27 ** 2
      t86 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
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
      t154 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t155 = -0.1D1 + x3
      t156 = t155 * x4
      t160 = log(-0.4D1 * t149 * t92 * t156)
      t161 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t167 = t161 - t86
      t168 = t105 * t167
      t172 = 0.1D1 / x3
      t174 = t114 * t116
      t177 = t92 * t155
      t180 = log(-0.4D1 * t149 * t177)
      t182 = t180 ** 2
      t185 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t186 = t148 * t19
      t188 = log(0.4D1 * t186)
      t190 = t188 ** 2
      t205 = -t44 * t105 * t167
      t210 = t18 * x4
      t213 = log(0.4D1 * t16 * t210)
      t218 = t213 ** 2
      t239 = x3 * t13
      t240 = t239 * t15
      t245 = log(-0.4D1 * t240 * t18 * t155 * x4)
      t247 = t245 ** 2
      t252 = log(0.4D1 * t239 * t93)
      t254 = t252 ** 2
      t273 = log(-0.4D1 * t239 * t177)
      t275 = t239 * t92
      t277 = log(0.4D1 * t275)
      t282 = t277 ** 2
      t289 = t273 ** 2
      t292 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t315 = (t10 - t22 * t25 - 0.90D2 * t28 * lh - 0.15D2 * t32) * t35 
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
     #* t172 * t174 / 0.720D3 + (0.90D2 * t63 * t39 * (t180 * t154 - t18
     #2 * t161 / 0.2D1 - t185 - t188 * t40 + t190 * t86 / 0.2D1 + t50) -
     # 0.180D3 * t54 * t105 * (-t154 + t180 * t161 + t40 - t188 * t86) +
     # t205) * t172 * t114 / 0.720D3 + (t44 * t105 * (t40 - t213 * t86) 
     #+ 0.90D2 * t63 * t39 * (t218 * t40 / 0.2D1 + t59 - t218 * t213 * t
     #86 / 0.6D1 - t213 * t50) + t137 - 0.180D3 * t54 * t105 * (-t213 * 
     #t40 + t218 * t86 / 0.2D1 + t50)) * t116 / 0.1440D4 + (0.90D2 * t63
     # * t39 * (t245 * t154 - t247 * t161 / 0.2D1 - t185 - t252 * t40 + 
     #t254 * t86 / 0.2D1 + t50) - 0.180D3 * t54 * t105 * (-t154 + t245 *
     # t161 + t40 - t252 * t86) + t205) * t172 * t116 / 0.1440D4 - (t44 
     #* t105 * (t154 - t273 * t161 - t40 + t277 * t86) + 0.90D2 * t63 * 
     #t39 * (-t282 * t40 / 0.2D1 - t59 + t282 * t277 * t86 / 0.6D1 + t27
     #7 * t50 + t289 * t154 / 0.2D1 + t292 - t289 * t273 * t161 / 0.6D1 
     #- t273 * t185) + t10 * t168 - 0.180D3 * t54 * t105 * (-t273 * t154
     # + t289 * t161 / 0.2D1 + t185 + t277 * t40 - t282 * t86 / 0.2D1 - 
     #t50)) * t172 / 0.1440D4
      t316 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t315)
      t319 = -0.1D1 + x4
      t321 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t322 = t210 * t319
      t325 = log(-0.4D1 * t16 * t322)
      t326 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t331 = t325 ** 2
      t334 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t338 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t344 = t105 * t326
      t358 = log(-0.4D1 * t240 * t322)
      t360 = t358 ** 2
      t364 = t210 * t319 * t155
      t367 = log(0.4D1 * t240 * t364)
      t368 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t370 = t367 ** 2
      t371 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t374 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t385 = -t326 + t371
      t395 = log(-0.4D1 * t91 * t15 * t322)
      t397 = t395 ** 2
      t414 = x4 * t319
      t418 = log(-0.4D1 * t149 * t92 * t414)
      t420 = t148 * t16
      t423 = log(0.4D1 * t420 * t364)
      t437 = (-t44 * t105 * (t321 - t325 * t326) - 0.90D2 * t63 * t39 * 
     #(t331 * t321 / 0.2D1 + t334 - t331 * t325 * t326 / 0.6D1 - t325 * 
     #t338) - t10 * t344 + 0.180D3 * t54 * t105 * (-t325 * t321 + t331 *
     # t326 / 0.2D1 + t338)) * t116 / 0.1440D4 + (0.90D2 * t63 * t39 * (
     #t358 * t321 - t360 * t326 / 0.2D1 - t338 - t367 * t368 + t370 * t3
     #71 / 0.2D1 + t374) - 0.180D3 * t54 * t105 * (-t321 + t358 * t326 +
     # t368 - t367 * t371) + t44 * t105 * t385) * t172 * t116 / 0.1440D4
     # - (0.90D2 * t63 * t39 * (-t395 * t321 + t397 * t326 / 0.2D1 + t33
     #8) - 0.180D3 * t54 * t105 * (t321 - t395 * t326) + t44 * t344) * t
     #114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (t321 - t418 * t326 -
     # t368 + t423 * t371) + 0.180D3 * t54 * t105 * t385) * t172 * t174 
     #/ 0.720D3
      t438 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t319, 0.0D0,
     # t437)
      t441 = -0.1D1 + x1
      t443 = t441 ** 2
      t444 = t90 * t443
      t448 = log(0.4D1 * t19 * t444 * x4)
      t449 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t451 = t448 ** 2
      t452 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t455 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t465 = t105 * t452
      t466 = t44 * t465
      t470 = t18 * t90
      t471 = t470 * t443
      t474 = log(0.4D1 * t16 * t471)
      t479 = t474 ** 2
      t482 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t505 = log(0.4D1 * t240 * t470 * t443 * x4)
      t518 = log(0.4D1 * t240 * t471)
      t520 = t518 ** 2
      t536 = -(0.90D2 * t63 * t39 * (-t448 * t449 + t451 * t452 / 0.2D1 
     #+ t455) - 0.180D3 * t54 * t105 * (t449 - t448 * t452) + t466) * t1
     #14 * t116 / 0.720D3 - (t44 * t105 * (t449 - t474 * t452) + 0.90D2 
     #* t63 * t39 * (t479 * t449 / 0.2D1 + t482 - t479 * t474 * t452 / 0
     #.6D1 - t474 * t455) + t10 * t465 - 0.180D3 * t54 * t105 * (-t474 *
     # t449 + t479 * t452 / 0.2D1 + t455)) * t114 / 0.720D3 - (0.90D2 * 
     #t63 * t39 * (t449 - t505 * t452) - 0.180D3 * t54 * t465) * t172 * 
     #t174 / 0.720D3 + (0.90D2 * t63 * t39 * (t518 * t449 - t520 * t452 
     #/ 0.2D1 - t455) - 0.180D3 * t54 * t105 * (-t449 + t518 * t452) - t
     #466) * t172 * t114 / 0.720D3
      t537 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t441, 0.0D0,
     # t536)
      t539 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t540 = s * t539
      t541 = t1 * x1
      t543 = t1 * t441
      t544 = t543 * x4
      t546 = t543 * t319
      t548 = t539 ** 2
      t551 = x1 * t441
      t554 = t548 ** 2
      t559 = log(-0.4D1 * t119 * t414 * t443 * t554)
      t561 = 0.1D1 / (-0.2D1 + t539)
      t562 = t559 * t561
      t563 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t565 = t559 ** 2
      t567 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t570 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t576 = t561 * t563
      t582 = t44 * t35
      t584 = t39 * t561 * t567
      t594 = log(-0.4D1 * t420 * t210 * t319 * t443 * t554)
      t601 = t54 * t35
      t608 = -(0.90D2 * t63 * t39 * (-t562 * t563 + t565 * t561 * t567 /
     # 0.2D1 + t561 * t570) - 0.180D3 * t54 * t105 * (t576 - t562 * t567
     #) + t582 * t584) * t114 * t116 / 0.720D3 - (0.90D2 * t63 * t39 * (
     #t576 - t594 * t561 * t567) - 0.180D3 * t601 * t584) * t172 * t174 
     #/ 0.720D3
      t609 = FJET(XB1, XB2, s, 0.0D0, t540 * t541, -t540 * t544, t540 * 
     #t546, -s * t548 * t17 * t551 * x4, t608)
      t611 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t612 = s * t611
      t613 = t541 * x3
      t615 = t541 * t155
      t618 = t611 ** 2
      t624 = 0.1D1 / (-0.2D1 + t611)
      t625 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t626 = t624 * t625
      t627 = t618 ** 2
      t632 = log(-0.4D1 * t275 * t444 * t156 * t627)
      t634 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t641 = t39 * t624 * t634
      t651 = log(-0.4D1 * t275 * t444 * t155 * t627)
      t652 = t651 * t624
      t654 = t651 ** 2
      t658 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t674 = -(0.90D2 * t63 * t39 * (t626 - t632 * t624 * t634) - 0.180D
     #3 * t601 * t641) * t172 * t174 / 0.720D3 + (-0.90D2 * t63 * t39 * 
     #(-t652 * t625 + t654 * t624 * t634 / 0.2D1 + t624 * t658) + 0.180D
     #3 * t54 * t105 * (t626 - t652 * t634) - t582 * t641) * t172 * t114
     # / 0.720D3
      t675 = FJET(XB1, XB2, s, t612 * t613, -t612 * t615, 0.0D0, -t612 *
     # t543, -s * t618 * t17 * t551 * x3, t674)
      t677 = KAPPA2(x1, x2, x3, x4, z)
      t678 = s * t677
      t683 = t677 ** 2
      t688 = cos(t11)
      t691 = Sqrt(x3 * t155 * t414)
      t698 = 0.1D1 / (-0.2D1 + t677)
      t699 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t702 = t683 ** 2
      t707 = log(0.4D1 * t186 * t414 * t443 * t155 * t702)
      t709 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t719 = -0.90D2 * t63 * t39 * (t698 * t699 - t707 * t698 * t709) + 
     #0.180D3 * t601 * t39 * t698 * t709
      t723 = FJET(XB1, XB2, s, t678 * t613, -t678 * t615, -t678 * t544, 
     #t678 * t546, s * t683 * t17 * t551 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t688 * t691), -t719 * t172 * t174 / 0.720D3)
      rrgq2qght5s8e1 = t316 * t315 + t438 * t437 + t537 * t536 + t609 * 
     #t608 + t675 * t674 - t723 * t719 * t172 * t114 * t116 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s8e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t32 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t36 = 0.3141592653589793D1 * t27
      t37 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t55 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t59 = 0.3141592653589793D1 * lh
      t64 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t68 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t69 = x3 * t11
      t70 = t69 * t13
      t71 = -0.1D1 + x3
      t76 = log(-0.4D1 * t70 * t16 * t71 * x4)
      t77 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t69 * t80)
      t89 = t27 * t31
      t90 = t55 - t77
      t93 = 0.180D3 * t59 * t89 * t90
      t95 = 0.1D1 / x3
      t97 = 0.1D1 / x4
      t100 = t79 * t71
      t103 = log(-0.4D1 * t69 * t100)
      t105 = t103 ** 2
      t108 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t109 = t69 * t79
      t111 = log(0.4D1 * t109)
      t113 = t111 ** 2
      t126 = -t90
      t132 = t16 * x4
      t135 = log(0.4D1 * t14 * t132)
      t137 = t135 ** 2
      t149 = t89 * t55
      t150 = t8 * t149
      t154 = x1 ** 2
      t155 = t154 * t11
      t158 = log(0.4D1 * t155 * t80)
      t167 = 0.1D1 / x1
      t171 = t36 * t31
      t173 = t167 * t97
      t177 = x3 * t154
      t181 = log(-0.4D1 * t177 * t11 * t100)
      t185 = log(0.4D1 * t177 * t17)
      t195 = t155 * t79
      t197 = log(0.4D1 * t195)
      t199 = t197 ** 2
      t214 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + t36 * t31 * t37 / 0.16D2 + (0.3141592653589793D1 * (0
     #.60D2 * lh * t5 - 0.2884936567583026D3 - 0.120D3 * t3 * lh) - t20 
     #* t7 - 0.90D2 * t24 * lh - 0.15D2 * t23 * t19 * 0.3141592653589793
     #D1) * t27 * t31 * t55 / 0.1440D4 + (-0.180D3 * t59 - 0.90D2 * t20)
     # * t27 * t31 * t64 / 0.1440D4 + (0.90D2 * t36 * t31 * (-t68 + t76 
     #* t77 + t32 - t83 * t55) - t93) * t95 * t97 / 0.1440D4 - (0.90D2 *
     # t36 * t31 * (-t103 * t68 + t105 * t77 / 0.2D1 + t108 + t111 * t32
     # - t113 * t55 / 0.2D1 - t64) - 0.180D3 * t59 * t89 * (t68 - t103 *
     # t77 - t32 + t111 * t55) + t8 * t89 * t126) * t95 / 0.1440D4 + (0.
     #90D2 * t36 * t31 * (-t135 * t32 + t137 * t55 / 0.2D1 + t64) - 0.18
     #0D3 * t59 * t89 * (t32 - t135 * t55) + t150) * t97 / 0.1440D4 - (0
     #.90D2 * t36 * t31 * (-t32 + t158 * t55) + 0.180D3 * t59 * t149) * 
     #t167 * t97 / 0.720D3 - t171 * t126 * t95 * t173 / 0.8D1 + (0.90D2 
     #* t36 * t31 * (-t68 + t181 * t77 + t32 - t185 * t55) - t93) * t95 
     #* t167 / 0.720D3 - (0.90D2 * t36 * t31 * (t197 * t32 - t199 * t55 
     #/ 0.2D1 - t64) - 0.180D3 * t59 * t89 * (-t32 + t197 * t55) - t150)
     # * t167 / 0.720D3
      t215 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t214)
      t218 = -0.1D1 + x4
      t220 = t132 * t218
      t223 = log(-0.4D1 * t14 * t220)
      t224 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t226 = t223 ** 2
      t227 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t230 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t240 = t89 * t227
      t248 = log(-0.4D1 * t155 * t13 * t220)
      t260 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t261 = t227 - t260
      t268 = log(-0.4D1 * t70 * t220)
      t270 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t275 = log(0.4D1 * t70 * t132 * t218 * t71)
      t289 = (-0.90D2 * t36 * t31 * (-t223 * t224 + t226 * t227 / 0.2D1 
     #+ t230) + 0.180D3 * t59 * t89 * (t224 - t223 * t227) - t8 * t240) 
     #* t97 / 0.1440D4 - (0.90D2 * t36 * t31 * (t224 - t248 * t227) - 0.
     #180D3 * t59 * t240) * t167 * t97 / 0.720D3 - t171 * t261 * t95 * t
     #173 / 0.8D1 + (0.90D2 * t36 * t31 * (-t224 + t268 * t227 + t270 - 
     #t275 * t260) + 0.180D3 * t59 * t89 * t261) * t95 * t97 / 0.1440D4
      t290 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t218, 0.0D0,
     # t289)
      t293 = -0.1D1 + x1
      t295 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t296 = t293 ** 2
      t297 = t154 * t296
      t301 = log(0.4D1 * t17 * t297 * x4)
      t302 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t308 = t89 * t302
      t310 = 0.180D3 * t59 * t308
      t320 = t16 * t154 * t296
      t323 = log(0.4D1 * t70 * t320)
      t335 = log(0.4D1 * t14 * t320)
      t337 = t335 ** 2
      t340 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t354 = -(0.90D2 * t36 * t31 * (t295 - t301 * t302) - t310) * t167 
     #* t97 / 0.720D3 - t171 * t302 * t95 * t173 / 0.8D1 + (0.90D2 * t36
     # * t31 * (-t295 + t323 * t302) + t310) * t95 * t167 / 0.720D3 - (0
     #.90D2 * t36 * t31 * (-t335 * t295 + t337 * t302 / 0.2D1 + t340) - 
     #0.180D3 * t59 * t89 * (t295 - t335 * t302) + t8 * t308) * t167 / 0
     #.720D3
      t355 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t293, 0.0D0,
     # t354)
      t357 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t358 = s * t357
      t359 = t1 * x1
      t361 = t1 * t293
      t362 = t361 * x4
      t364 = t361 * t218
      t366 = t357 ** 2
      t369 = x1 * t293
      t373 = 0.1D1 / (-0.2D1 + t357)
      t374 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t376 = x4 * t218
      t377 = t366 ** 2
      t382 = log(-0.4D1 * t195 * t376 * t296 * t377)
      t384 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t390 = t59 * t27
      t391 = t31 * t373
      t404 = -(0.90D2 * t36 * t31 * (t373 * t374 - t382 * t373 * t384) -
     # 0.180D3 * t390 * t391 * t384) * t167 * t97 / 0.720D3 - t36 * t391
     # * t384 * t95 * t173 / 0.8D1
      t405 = FJET(XB1, XB2, s, 0.0D0, t358 * t359, -t358 * t362, t358 * 
     #t364, -s * t366 * t15 * t369 * x4, t404)
      t407 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t408 = s * t407
      t409 = t359 * x3
      t411 = t359 * t71
      t414 = t407 ** 2
      t420 = 0.1D1 / (-0.2D1 + t407)
      t421 = t31 * t420
      t423 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t428 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t430 = t414 ** 2
      t435 = log(-0.4D1 * t109 * t297 * t71 * t430)
      t449 = -t36 * t421 * t423 * t95 * t173 / 0.8D1 + (-0.90D2 * t36 * 
     #t31 * (t420 * t428 - t435 * t420 * t423) + 0.180D3 * t390 * t421 *
     # t423) * t95 * t167 / 0.720D3
      t450 = FJET(XB1, XB2, s, t408 * t409, -t408 * t411, 0.0D0, -t408 *
     # t361, -s * t414 * t15 * t369 * x3, t449)
      t452 = KAPPA2(x1, x2, x3, x4, z)
      t453 = s * t452
      t458 = t452 ** 2
      t463 = cos(t9)
      t466 = Sqrt(x3 * t71 * t376)
      t473 = 0.1D1 / (-0.2D1 + t452)
      t476 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t481 = FJET(XB1, XB2, s, t453 * t409, -t453 * t411, -t453 * t362, 
     #t453 * t364, s * t458 * t15 * t369 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t463 * t466), t36 * t31 * t473 * t476 * t95 * t173 / 0.8D
     #1)
      rrgq2qght5s8e0 = t215 * t214 + t290 * t289 + t355 * t354 + t405 * 
     #t404 + t450 * t449 + t481 * 0.3141592653589793D1 * t89 * t473 * t4
     #76 * t95 * t167 * t97 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s8em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
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
      t24 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t28 = lh ** 2
      t30 = 0.3141592653589793D1 ** 2
      t36 = t15 ** 2
      t41 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t45 = t19 * 0.3141592653589793D1
      t46 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t50 = t45 * t23
      t51 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t52 = t41 - t51
      t53 = 0.1D1 / x3
      t54 = t52 * t53
      t55 = 0.1D1 / x4
      t59 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t60 = x3 * t7
      t61 = t9 * t12
      t62 = -0.1D1 + x3
      t66 = log(-0.4D1 * t60 * t61 * t62)
      t70 = log(0.4D1 * t60 * t61)
      t76 = t19 * t23
      t84 = t12 * x4
      t87 = log(0.4D1 * t10 * t84)
      t95 = 0.180D3 * t3 * t76 * t41
      t99 = 0.1D1 / x1
      t103 = x1 ** 2
      t107 = log(0.4D1 * t103 * t7 * t61)
      t120 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (0.3141592653589793D1 * (0.180D3 * t28 - 0.30D2 * t30) + 0.180D
     #3 * t16 * lh + 0.45D2 * t36 * 0.3141592653589793D1) * t19 * t23 * 
     #t41 / 0.1440D4 + t45 * t23 * t46 / 0.16D2 + t50 * t54 * t55 / 0.16
     #D2 - (0.90D2 * t45 * t23 * (t59 - t66 * t51 - t24 + t70 * t41) + 0
     #.180D3 * t3 * t76 * t52) * t53 / 0.1440D4 + (0.90D2 * t45 * t23 * 
     #(t24 - t87 * t41) - t95) * t55 / 0.1440D4 + t50 * t54 * t99 / 0.8D
     #1 - (0.90D2 * t45 * t23 * (-t24 + t107 * t41) + t95) * t99 / 0.720
     #D3 + t50 * t41 * t99 * t55 / 0.8D1
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t120)
      t124 = -0.1D1 + x4
      t126 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t130 = log(-0.4D1 * t10 * t84 * t124)
      t131 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t147 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t153 = (-0.90D2 * t45 * t23 * (t126 - t130 * t131) + 0.180D3 * t3 
     #* t76 * t131) * t55 / 0.1440D4 - t50 * t131 * t99 * t55 / 0.8D1 + 
     #t50 * (-t131 + t147) * t53 * t55 / 0.16D2
      t154 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t124, 0.0D0,
     # t153)
      t157 = -0.1D1 + x1
      t159 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t164 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t166 = t157 ** 2
      t170 = log(0.4D1 * t10 * t12 * t103 * t166)
      t186 = -t50 * t159 * t53 * t99 / 0.8D1 - (0.90D2 * t45 * t23 * (t1
     #64 - t170 * t159) - 0.180D3 * t3 * t76 * t159) * t99 / 0.720D3 - t
     #50 * t159 * t99 * t55 / 0.8D1
      t187 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t157, 0.0D0,
     # t186)
      t189 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t190 = s * t189
      t191 = t1 * x1
      t193 = t1 * t157
      t198 = t189 ** 2
      t201 = x1 * t157
      t206 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t209 = 0.1D1 / (-0.2D1 + t189) * t206 * t99 * t55
      t212 = FJET(XB1, XB2, s, 0.0D0, t190 * t191, -t190 * t193 * x4, t1
     #90 * t193 * t124, -s * t198 * t11 * t201 * x4, -t50 * t209 / 0.8D1
     #)
      t217 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t218 = s * t217
      t224 = t217 ** 2
      t231 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t234 = 0.1D1 / (-0.2D1 + t217) * t231 * t53 * t99
      t237 = FJET(XB1, XB2, s, t218 * t191 * x3, -t218 * t191 * t62, 0.0
     #D0, -t218 * t193, -s * t224 * t11 * t201 * x3, -t50 * t234 / 0.8D1
     #)
      rrgq2qght5s8em1 = t121 * t120 + t154 * t153 + t187 * t186 - t212 *
     # 0.3141592653589793D1 * t76 * t209 / 0.8D1 - t237 * 0.314159265358
     #9793D1 * t76 * t234 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s8em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t14 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t21 = 0.1D1 / x1
      t25 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = t4 * t9 * t10 / 0.16D2 - t4 * t7 * (t14 - t8) / x3 / 0.16D2 
     #+ t4 * t9 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 0.3
     #141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) * t3
     # * t9 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t54 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t56 = t7 * t54 * t10
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, -t4 * t56 / 0.16D2)
      t67 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t69 = t7 * t67 * t21
      t72 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, -t4 * t69 / 0.8D1)
      rrgq2qght5s8em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.16D2 - t72 * 0.3141592653589793D1 * t3 * t69 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s8em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrgq2qght5s8em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrgq2qght5s8em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6
      rrgq2qght5s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh51J1
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
      t9 = t3 ** 2
      t10 = t9 * t3
      t12 = t5 ** 2
      t13 = t12 * t5
      t14 = x1 ** 2
      t15 = t14 * x1
      t17 = x3 ** 2
      t21 = t9 ** 2
      t24 = t12 ** 2
      t28 = (0.1D1 - x1) ** 2
      t29 = t28 ** 2
      t34 = cos(x2 * 0.3141592653589793D1)
      t37 = 0.1D1 - x4
      t40 = Sqrt(x3 * (0.1D1 - x3) * x4 * t37)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t40
      t44 = t43 ** 2
      t54 = t37 ** 2
      rrgq2qgh51J1 = 0.4D1 * wd * (t2 * t3 * t5 * x1 * x3 + t2 * t10 * t
     #13 * t15 * t17 * x3 + t2 * t21 * t10 * t24 * t13 * t15 * t29 * t44
     # * t43 * t37 + t2 * t21 * t3 * t24 * t5 * x1 * t29 * t43 * t54 * t
     #37) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh51J2
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
      t8 = t2 * t3 * t5 * x1 * x3
      t9 = t3 ** 2
      t10 = t9 * t3
      t11 = t2 * t10
      t12 = t5 ** 2
      t13 = t12 * t5
      t14 = x1 ** 2
      t15 = t14 * x1
      t17 = x3 ** 2
      t20 = t11 * t13 * t15 * t17 * x3
      t21 = t9 ** 2
      t24 = t12 ** 2
      t27 = 0.1D1 - x1
      t28 = t27 ** 2
      t29 = t28 ** 2
      t34 = cos(x2 * 0.3141592653589793D1)
      t37 = 0.1D1 - x4
      t40 = Sqrt(x3 * (0.1D1 - x3) * x4 * t37)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t40
      t44 = t43 ** 2
      t48 = t2 * t21 * t10 * t24 * t13 * t15 * t29 * t44 * t43 * t37
      t52 = t2 * t21 * t3 * t24 * t5
      t54 = t37 ** 2
      t58 = t52 * x1 * t29 * t43 * t54 * t37
      t62 = t2 * t21 * t24
      t72 = t11 * t13
      t81 = t28 * t27
      t103 = -t62 * t15 * t27 * t43 * t17 + 0.2D1 * t2 * t9 * t12 * t14 
     #* t17 + t72 * t14 * t27 * t43 * x3 + t52 * t15 * t28 * t44 * x3 - 
     #t20 - t8 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t21 * t9 * t2
     #4 * t12 * t29 * t54 * t14 * t44 + t52 * t14 * t81 * t44 * t37 + t7
     #2 * x1 * t28 * t43 * t37 - t58 - t48
      rrgq2qgh51J2 = 0.4D1 * (wd * (t8 + t20 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh51J3
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
      t8 = t2 * t3 * t5 * x1 * x3
      t9 = t3 ** 2
      t10 = t9 * t3
      t11 = t2 * t10
      t12 = t5 ** 2
      t13 = t12 * t5
      t14 = x1 ** 2
      t15 = t14 * x1
      t17 = x3 ** 2
      t20 = t11 * t13 * t15 * t17 * x3
      t21 = t9 ** 2
      t24 = t12 ** 2
      t27 = 0.1D1 - x1
      t28 = t27 ** 2
      t29 = t28 ** 2
      t34 = cos(x2 * 0.3141592653589793D1)
      t37 = 0.1D1 - x4
      t40 = Sqrt(x3 * (0.1D1 - x3) * x4 * t37)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t40
      t44 = t43 ** 2
      t48 = t2 * t21 * t10 * t24 * t13 * t15 * t29 * t44 * t43 * t37
      t52 = t2 * t21 * t3 * t24 * t5
      t54 = t37 ** 2
      t58 = t52 * x1 * t29 * t43 * t54 * t37
      t62 = t2 * t21 * t24
      t72 = t11 * t13
      t81 = t28 * t27
      t103 = -t62 * t15 * t27 * t43 * t17 + 0.2D1 * t2 * t9 * t12 * t14 
     #* t17 + t72 * t14 * t27 * t43 * x3 + t52 * t15 * t28 * t44 * x3 - 
     #t20 - t8 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t21 * t9 * t2
     #4 * t12 * t29 * t54 * t14 * t44 + t52 * t14 * t81 * t44 * t37 + t7
     #2 * x1 * t28 * t43 * t37 - t58 - t48
      rrgq2qgh51J3 = 0.4D1 * (wd * (t8 + t20 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh51J4
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
      t8 = t2 * t3 * t5 * x1 * x3
      t9 = t3 ** 2
      t10 = t9 * t3
      t11 = t2 * t10
      t12 = t5 ** 2
      t13 = t12 * t5
      t14 = x1 ** 2
      t15 = t14 * x1
      t17 = x3 ** 2
      t20 = t11 * t13 * t15 * t17 * x3
      t21 = t9 ** 2
      t24 = t12 ** 2
      t27 = 0.1D1 - x1
      t28 = t27 ** 2
      t29 = t28 ** 2
      t34 = cos(x2 * 0.3141592653589793D1)
      t37 = 0.1D1 - x4
      t40 = Sqrt(x3 * (0.1D1 - x3) * x4 * t37)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t40
      t44 = t43 ** 2
      t48 = t2 * t21 * t10 * t24 * t13 * t15 * t29 * t44 * t43 * t37
      t52 = t2 * t21 * t3 * t24 * t5
      t54 = t37 ** 2
      t58 = t52 * x1 * t29 * t43 * t54 * t37
      t62 = t2 * t21 * t24
      t72 = t11 * t13
      t81 = t28 * t27
      t103 = -t62 * t15 * t27 * t43 * t17 + 0.2D1 * t2 * t9 * t12 * t14 
     #* t17 + t72 * t14 * t27 * t43 * x3 + t52 * t15 * t28 * t44 * x3 - 
     #t20 - t8 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t21 * t9 * t2
     #4 * t12 * t29 * t54 * t14 * t44 + t52 * t14 * t81 * t44 * t37 + t7
     #2 * x1 * t28 * t43 * t37 - t58 - t48
      rrgq2qgh51J4 = 0.4D1 * (wd * (t8 + t20 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh51J5
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
      t8 = t2 * t3 * t5 * x1 * x3
      t9 = t3 ** 2
      t10 = t9 * t3
      t11 = t2 * t10
      t12 = t5 ** 2
      t13 = t12 * t5
      t14 = x1 ** 2
      t15 = t14 * x1
      t17 = x3 ** 2
      t20 = t11 * t13 * t15 * t17 * x3
      t21 = t9 ** 2
      t24 = t12 ** 2
      t27 = 0.1D1 - x1
      t28 = t27 ** 2
      t29 = t28 ** 2
      t34 = cos(x2 * 0.3141592653589793D1)
      t37 = 0.1D1 - x4
      t40 = Sqrt(x3 * (0.1D1 - x3) * x4 * t37)
      t43 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t34 * t40
      t44 = t43 ** 2
      t48 = t2 * t21 * t10 * t24 * t13 * t15 * t29 * t44 * t43 * t37
      t52 = t2 * t21 * t3 * t24 * t5
      t54 = t37 ** 2
      t58 = t52 * x1 * t29 * t43 * t54 * t37
      t62 = t2 * t21 * t24
      t72 = t11 * t13
      t81 = t28 * t27
      t103 = -t62 * t15 * t27 * t43 * t17 + 0.2D1 * t2 * t9 * t12 * t14 
     #* t17 + t72 * t14 * t27 * t43 * x3 + t52 * t15 * t28 * t44 * x3 - 
     #t20 - t8 - t62 * x1 * t81 * t43 * t54 + 0.2D1 * t2 * t21 * t9 * t2
     #4 * t12 * t29 * t54 * t14 * t44 + t52 * t14 * t81 * t44 * t37 + t7
     #2 * x1 * t28 * t43 * t37 - t58 - t48
      rrgq2qgh51J5 = 0.4D1 * (wd * (t8 + t20 + t48 + t58) + wd * t103) /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh51J6
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
      t21 = 0.1D1 - x4
      t24 = Sqrt(x3 * (0.1D1 - x3) * x4 * t21)
      t27 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t18 * t24
      t28 = x3 ** 2
      t37 = t4 * t3
      t38 = t2 * t37
      t39 = t7 * t8
      t40 = t38 * t39
      t48 = t2 * t5 * t3 * t9 * t7
      t49 = t13 ** 2
      t51 = t27 ** 2
      t63 = t49 * t13
      t65 = t21 ** 2
      t73 = t49 ** 2
      t101 = -t10 * t12 * t13 * t27 * t28 + 0.2D1 * t2 * t4 * t8 * t11 *
     # t28 + t40 * t11 * t13 * t27 * x3 + t48 * t12 * t49 * t51 * x3 - t
     #38 * t39 * t12 * t28 * x3 - t2 * t3 * t7 * x1 * x3 - t10 * x1 * t6
     #3 * t27 * t65 + 0.2D1 * t2 * t5 * t4 * t9 * t8 * t73 * t65 * t11 *
     # t51 + t48 * t11 * t63 * t51 * t21 + t40 * x1 * t49 * t27 * t21 - 
     #t48 * x1 * t73 * t27 * t65 * t21 - t2 * t5 * t37 * t9 * t39 * t12 
     #* t73 * t51 * t27 * t21
      rrgq2qgh51J6 = 0.4D1 * wd * t101 / s / z / 0.3141592653589793D1

      end function
  
 